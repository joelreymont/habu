\ aio.f - asynchronous I/O, package AIO: a task arms a wait for a descriptor's
\ readiness or for a deadline, gets a ticket and AWAITs it, or AWAIT-ANYs a
\ group of tickets. Every wait is its task's own: POLL arms the calling task's
\ TASK:QUEUE through the host layer (src/lib/aio/linux.f), and a record ends
\ only inside its owner's wait, which also serves a TASK:HALT posted to that
\ queue.
\ RECORD STATES. FREE, ARMED, DONE. FREE->ARMED: POLL or TIMEOUT by the owner,
\ or POLL-INTO by another task, the worker: its record is pushed, owner -2
\ minus the worker's id, and goes back FREE if the group has no holder; else
\ it goes onto the group's inbox, is armed in the holder's queue and, once
\ that arm held, confirmed (OWNER-CONFIRMED). The holder adopts it in its wait,
\ its owner the holder's id, and arms it again if it was still pushed; a room
\ or arm failure there ends it rejected. ARMED->DONE: in the owner's wait, on
\ its event (ready, with the revents), its passed deadline (timed-out), or the
\ owner's CANCEL (cancelled); at the arm, for a failed arm (rejected, with the
\ errno); for every record on the chain when its re-arm fails (rejected). A
\ halt that unwinds a wait cancels that wait's record or group. DONE->FREE:
\ the owner's AWAIT or AWAIT-ANY. Any->FREE at the owner's exit, which
\ disarms what is still armed, unadopted records of its groups included.
\ ONE REGISTRATION PER DESCRIPTOR. The queue keeps one registration per
\ descriptor, so a task's armed records on one descriptor share it: they form
\ a chain, its first in the task's descriptor table and each next in
\ REC.FD-NEXT, and the registration is armed one-shot for the union of the
\ chain's events under event id descriptor plus 1. An arm joins the chain and
\ re-arms the union; a regular file is ready at once and any other failure
\ rejects the record, which stays off the chain. A disarm leaves the chain and
\ re-arms the rest's union, or drops the registration when none is left.
\ THE OWNER'S WAIT. It hands out a DONE record if its target is one: the
\ record, or for AWAIT-ANY the group's done list. Else it waits in the queue
\ until the earliest deadline the task has armed, a min-heap per task, rounded
\ up to a millisecond; a task that holds a group also sleeps no later than
\ FLOOR, the deadline of the record whose event last ended a wait, while that
\ is still ahead. An event ends DONE ready, with the revents, each record on
\ its descriptor's chain whose events it meets, or every one on an error or
\ hang-up, moving each to its group's done list, and re-arms the rest's union.
\ Then each record whose deadline has passed goes DONE timed-out, disarmed;
\ and the wait goes round. The kernel wait's timeout is the only timer.
\ GROUPS. A group's entry holds its ticket count, the task that holds it, that
\ task's queue and sleep end, its inbox and its done list. The entries live in a
\ chain of chunks of GROUPS-FIRST each, the first static; a chunk is appended
\ when a group past the chain is first used, without the facility, so the chunks
\ are one size and an append that loses a race is only spare room. A group is
\ held while its count is above zero: the task whose GROUP+ finds it at zero
\ takes G.OWNER and links it on its HELD list, and holds it until the count is
\ back to zero; until then only it may GROUP+, GROUP- or AWAIT-ANY on it.
\ Another task may arm into a held group through POLL-INTO, which raises the
\ count only while it is above zero; the holder adopts the group's inbox in its
\ own wait, before and after the kernel wait, and in CANCEL, AWAIT and GROUP- of
\ a record not yet adopted. Before each kernel wait the holder publishes its
\ sleep end in G.END, fences, then reads its inboxes again; a POLL-INTO pushes,
\ arms, fences and reads G.END, so one of the two sees the other, and it wakes
\ the holder only when its arm failed or its deadline is earlier than G.END.
\ STORAGE CLASS. PROCESS-WIDE: the record and group chunks, a record claimed
\ under AIO-LOCK; G.INBOX, written by any task, lock-free; G.END, written by
\ its holder only; SINK, which every waker's task writes and none reads.
\ TASK-LOCAL: AIO-REGISTERED, AIO-PARKED, HELD, FLOOR, the deadline heap and
\ the descriptor table (TASK:+USER rows), and the host layer's event buffers.

require src/lib/error.f
require src/lib/memory.f
require src/lib/allocator.f
require src/lib/heap.f
require src/lib/ffi-abi.f
require src/lib/num-types.f
require src/lib/task.f
require src/lib/fd.f
HB-TARGET-LINUX? [if] require src/lib/aio/linux.f [then]

package AIO
public

\ A submission's handle: its record's address, in memory mapped while the
\ program runs. Only the submitting task may AWAIT it, once.
NEWTYPE ticket ^n

\ A set of tickets one AWAIT-ANY waits on, as the index a GROUP definition
\ was given.
NEWTYPE group

\ What an operation ended as. `ready` carries the poll revents the kernel
\ reported; `rejected` carries the errno as a positive number.
SUMTYPE outcome
   VARIANT ready n ;VARIANT
   VARIANT timed-out ;VARIANT
   VARIANT cancelled ;VARIANT
   VARIANT rejected n ;VARIANT
;SUMTYPE

private

CAST: >TICKET ( ^n -- ticket )
CAST: TICKET>REC ( ticket -- ^n )
CAST: >GROUP ( n -- group )
CAST: GROUP>N ( group -- n )

\ A record, a chunk, a heap row, a table or a task's TCB, which this module
\ names by address as n: 0 is none, or for a TCB the main thread.
CAST: AIO-N>PTR ( n -- ^n )

1 constant EV-READABLE              \ POLLIN
4 constant EV-WRITABLE              \ POLLOUT
$7D constant ERR-CANCELED

\ REC.OWNER of a POLL-INTO record whose worker's arm held, until adopted.
-1 constant OWNER-CONFIRMED

\ The end of a wait with no deadline.
$7FFFFFFFFFFFFFFF constant END-NEVER

1000000 constant NS-PER-MS

\ ---- records -----------------------------------------------------------------
0 constant STATE-FREE
1 constant STATE-ARMED
2 constant STATE-DONE

\ REC.OUT: which outcome a DONE record answers, with REC.RES.
0 constant OUT-READY
1 constant OUT-TIMED-OUT
2 constant OUT-CANCELLED
3 constant OUT-REJECTED

BEGIN-STRUCTURE REC-BYTES
   CELL +FIELD REC.STATE
   CELL +FIELD REC.IDX              \ set when its chunk is made
   CELL +FIELD REC.OWNER            \ the arming task's TASK:SELF-N; below 0
                                    \ for a POLL-INTO record until adopted
   CELL +FIELD REC.FD               \ -1 for none
   CELL +FIELD REC.EVENTS
   CELL +FIELD REC.FD-NEXT          \ its chain's next record, index plus 1, or 0
   CELL +FIELD REC.DEADLINE         \ mono-ns, while REC.HEAP-POS is a row; a
                                    \ POLL-INTO record's from its claim, or -1
   CELL +FIELD REC.HEAP-POS         \ its row in the owner's heap, or -1
   CELL +FIELD REC.GROUP            \ its group plus 1, 0 for none
   CELL +FIELD REC.TAG
   CELL +FIELD REC.NEXT             \ the next on its group's done list or
                                    \ inbox, or 0
   CELL +FIELD REC.OUT
   CELL +FIELD REC.RES
END-STRUCTURE

\ A chain of mapped chunks, a link cell then the records, each holding as many
\ as all before it plus RECS-FIRST. None moves or goes back in the process.
$40 constant RECS-FIRST
CELL constant CHUNK-HEAD

\ A group's entry; only its holder writes it, but for a POLL-INTO's raise of
\ G.COUNT and its push onto G.INBOX.
$10 constant GROUPS-FIRST

BEGIN-STRUCTURE G-BYTES
   CELL +FIELD G.COUNT              \ the tickets the group holds
   CELL +FIELD G.OWNER              \ the holder's TASK:SELF-N while G.COUNT > 0
   CELL +FIELD G.QUEUE              \ the holder's TASK:QUEUE while G.COUNT > 0
   CELL +FIELD G.HELD               \ the holder's next held group, plus 1, or 0
   CELL +FIELD G.INBOX              \ POLL-INTO records not yet adopted, or 0
   CELL +FIELD G.END                \ the holder's sleep end, mono-ns, or
                                    \ END-NEVER
   CELL +FIELD G.HEAD               \ its DONE records, oldest first, or 0
   CELL +FIELD G.TAIL
END-STRUCTURE

\ SCRATCH-BUFFER: rounds the address to a cell, as the atomic words need for a
\ link cell and an entry's cells. AIO-RECS is the chain's link cell, 0 with no
\ chunk.
CHUNK-HEAD SCRATCH-BUFFER: AIO-RECS
CHUNK-HEAD GROUPS-FIRST G-BYTES * + SCRATCH-BUFFER: AIO-GROUPS

\ The records the chunks hold: grown under the facility, after the chunk's link.
SCRATCH-VARIABLE REC-GROWN

: REC-CAP ( -- n ) REC-GROWN atomic@ ;

variable GROUP-N                    \ the groups GROUP defined, which a build keeps

TASK:FACILITY AIO-LOCK

\ Set at a task's first submission, when its cleanup registers, so a program
\ that never submits registers nothing; the cell keeps a submitting task from
\ walking its TASK:AT-EXIT chain again at later submissions.
TASK:+USER AIO-REGISTERED n

\ A wait's record or group, set around its kernel wait, else -1. A throw out of
\ the kernel wait leaves it set for the finally to cancel.
TASK:+USER AIO-PARKED n

\ The task's armed deadlines: a min-heap of records by REC.DEADLINE, HEAP-LEN
\ rows of the HEAP-CAP at HEAP-BASE.
TASK:+USER HEAP-BASE n
TASK:+USER HEAP-LEN n
TASK:+USER HEAP-CAP n

\ The task's chains: FD-CAP cells at FD-BASE, the cell a descriptor indexes
\ holding its chain's first record, index plus 1, or 0 for none.
TASK:+USER FD-BASE n
TASK:+USER FD-CAP n

\ The first group the task holds, plus 1, or 0; each next in G.HELD.
TASK:+USER HELD n

\ The deadline of the record whose event last ended a wait, mono-ns: a later
\ POLL-INTO's deadline is past it, so a holder that sleeps to it is not woken.
TASK:+USER FLOOR n

: CHAIN-HEAD ( -- n )
   AIO-RECS CELL-VIEW FFI:>CELL ;

: CHUNK-NEXT ( n -- n )
   AIO-N>PTR atomic@ ;

: CHUNK-FIRST ( -- n )
   CHAIN-HEAD CHUNK-NEXT ;

: CHUNK-REC ( n n -- ^n ) {: c:n k:n :}
   c CHUNK-HEAD + k REC-BYTES * + AIO-N>PTR ;

: CHUNK-LEN ( n -- NUM:alloc-byte-len )
   REC-BYTES * CHUNK-HEAD + MEM:BYTES-ALLOC-LEN ;

: CHUNK-LAST ( -- n )
   CHAIN-HEAD begin dup CHUNK-NEXT dup 0<> while nip repeat drop ;

: REC ( n -- ^n ) {: idx:n :}
   CHUNK-FIRST idx RECS-FIRST
   begin 2dup >= while
      swap over - swap 2 *
      rot CHUNK-NEXT -rot
   repeat
   drop CHUNK-REC ;

: ARMED? ( ^n -- bool )
   REC.STATE atomic@ STATE-ARMED = ;

: DONE? ( ^n -- bool )
   REC.STATE atomic@ STATE-DONE = ;

\ ---- group entries -----------------------------------------------------------
: GROUPS-HEAD ( -- n )
   AIO-GROUPS CELL-VIEW FFI:>CELL ;

: G-AT ( n n -- ^n ) {: c:n k:n :}
   c CHUNK-HEAD + k G-BYTES * + AIO-N>PTR ;

: G-CHUNK-LEN ( -- NUM:alloc-byte-len )
   GROUPS-FIRST G-BYTES * CHUNK-HEAD + MEM:BYTES-ALLOC-LEN ;

\ Links a chunk after the last one. Two tasks appending at the same moment each
\ link their own; the one that loses the race walks on to the new last.
: G-CHUNK-APPEND ( n -- ) {: c:n :}
   GROUPS-HEAD
   begin
      dup 0 c rot AIO-N>PTR atomic-cas
      dup 0<>
   while nip repeat
   2drop ;

\ The chunk after that one, appending a fresh one when there is none: a fresh
\ mapping is zero, every entry empty.
: G-NEXT ( n -- n ) {: c:n :}
   c CHUNK-NEXT dup 0<> if exit then drop
   G-CHUNK-LEN MEM:ALLOC-BYTES drop FFI:>CELL G-CHUNK-APPEND
   c CHUNK-NEXT ;

: G-ENTRY ( n -- ^n ) {: g:n :}
   g 0 < g GROUP-N @ >= or if E-AIO-GROUP throw then
   GROUPS-HEAD g
   begin dup GROUPS-FIRST >= while
      GROUPS-FIRST - swap G-NEXT swap
   repeat
   G-AT ;

\ A record's group entry; the record is in a group.
: REC-G ( ^n -- ^n )
   REC.GROUP @ 1 - G-ENTRY ;

: DONE-PUSH ( ^n ^n -- ) {: e:^ r:^ :}
   r FFI:>CELL {: rn:n :}
   0 r REC.NEXT !
   e G.TAIL @ {: t:n :}
   t 0= if rn e G.HEAD ! else rn t AIO-N>PTR REC.NEXT ! then
   rn e G.TAIL ! ;

\ The record before r on e's done list as n, 0 when r is first, or -1 when r
\ is not on it.
: DONE-PREV ( ^n ^n -- n ) {: e:^ r:^ :}
   r FFI:>CELL {: rn:n :}
   e G.HEAD @ rn = if 0 exit then
   e G.HEAD @
   begin dup 0<> while
      dup AIO-N>PTR REC.NEXT @ rn = if exit then
      AIO-N>PTR REC.NEXT @
   repeat
   drop -1 ;

: DONE-UNLINK ( ^n ^n -- ) {: e:^ r:^ :}
   e r DONE-PREV {: p:n :}
   p -1 = if exit then
   r REC.NEXT @ {: nx:n :}
   p 0= if nx e G.HEAD ! else nx p AIO-N>PTR REC.NEXT ! then
   e G.TAIL @ r FFI:>CELL = if p e G.TAIL ! then ;

: HELD-LINK ( n ^n -- ) {: g:n e:^ :}
   HELD @ e G.HELD !
   g 1 + HELD ! ;

\ Takes g off the list, nx the group after it, read while g was held: once
\ its count is zero another task may take it and rewrite G.HELD.
: HELD-UNLINK ( n n -- ) {: g:n nx:n :}
   HELD @ g 1 + = if nx HELD ! exit then
   HELD @
   begin dup 0<> while
      1 - G-ENTRY
      dup G.HELD @ g 1 + = if nx swap G.HELD ! exit then
      G.HELD @
   repeat
   drop ;

\ The record leaves its group, and its done list if it is on it.
: G-LEAVE ( ^n -- ) {: r:^ :}
   r REC-G {: e:^ :}
   r REC.GROUP @ 1 - {: g:n :}
   e r DONE-UNLINK
   0 r REC.GROUP !
   e G.HELD @ {: nx:n :}
   -1 e G.COUNT atomic-add 1 = if g nx HELD-UNLINK then ;

\ Owner, queue and the HELD link are stored before the count makes the group
\ held, so a POLL-INTO that raises the count reads them.
: G-ADD ( n ^n n -- ) {: g:n r:^ tag:n :}
   g G-ENTRY {: e:^ :}
   e G.COUNT atomic@ 0= if
      TASK:SELF-N e G.OWNER atomic!
      TASK:QUEUE e G.QUEUE atomic!
      g e HELD-LINK
   then
   g 1 + r REC.GROUP !
   tag r REC.TAG !
   1 e G.COUNT atomic-add drop
   r DONE? if e r DONE-PUSH then ;

\ Raises the count only while it is above zero, answering whether it did: a
\ group with no holder stays empty.
: G-PIN ( ^n -- bool ) {: e:^ :}
   e G.COUNT atomic@
   begin
      dup 0= if drop false exit then
      dup dup 1 + e G.COUNT atomic-cas
      2dup <> while nip
   repeat
   2drop true ;

\ A retry follows a push or a take that changed the head since it was read.
: INBOX-PUSH ( ^n ^n -- ) {: e:^ r:^ :}
   r FFI:>CELL {: rn:n :}
   e G.INBOX atomic@
   begin
      dup r REC.NEXT !
      dup rn e G.INBOX atomic-cas
      2dup <> while nip
   repeat
   2drop ;

\ The whole inbox, its head as n or 0; a CAS loop, as no word exchanges a cell.
: INBOX-TAKE ( ^n -- n ) {: e:^ :}
   e G.INBOX atomic@
   begin
      dup 0= if exit then
      dup 0 e G.INBOX atomic-cas
      2dup <> while nip
   repeat
   drop ;

: G-REGISTER ( -- n )
   GROUP-N @ dup 1 + GROUP-N ! ;

\ ---- claim and give back ------------------------------------------------------
\ A fresh mapping is zero, every record free. The chunk is linked before
\ REC-GROWN counts it.
: REC-GROW ( -- )
   REC-CAP {: base:n :}
   base RECS-FIRST + {: len:n :}
   len CHUNK-LEN MEM:ALLOC-BYTES drop FFI:>CELL {: c:n :}
   len 0 ?do base i + c i CHUNK-REC REC.IDX ! loop
   c CHUNK-LAST AIO-N>PTR atomic!
   base len + REC-GROWN atomic! ;

\ FREE->ARMED. The owner is written before the store that makes the record
\ ARMED, so a reader that sees ARMED sees its owner.
: REC-OPEN ( ^n -- ) {: r:^ :}
   TASK:SELF-N r REC.OWNER !
   STATE-ARMED r REC.STATE atomic! ;

\ The first free record of the chunk at c, whose records start at index base,
\ opened, or -1. The caller holds AIO-LOCK.
: CHUNK-CLAIM ( n n n -- n ) {: c:n base:n len:n :}
   len 0 ?do
      c i CHUNK-REC REC.STATE atomic@ STATE-FREE = if
         c i CHUNK-REC REC-OPEN
         base i + unloop exit
      then
   loop
   -1 ;

: REC-FIND ( -- n )
   CHUNK-FIRST dup 0= if drop -1 exit then
   0 RECS-FIRST
   begin
      >r 2dup r@ CHUNK-CLAIM r> swap
      dup 0 >= if nip nip nip exit then drop
      >r r@ + r> 2 *
      rot CHUNK-NEXT dup 0= if drop 2drop -1 exit then -rot
   again ;

\ A record opened, grown into when all are taken: 0 and its index, or a code
\ and -1. The caller holds AIO-LOCK, which serializes REC-GROW.
: REC-TAKE ( -- n n )
   REC-FIND dup 0 >= if 0 swap exit then drop
   [: REC-GROW ;] catch dup 0<> if -1 exit then drop
   0 REC-FIND ;

\ The rest of an opened record's fields; only its owner reads them.
: REC-INIT ( ^n -- ) {: r:^ :}
   -1 r REC.FD !  0 r REC.EVENTS !  0 r REC.FD-NEXT !  -1 r REC.HEAP-POS !
   0 r REC.GROUP !  0 r REC.TAG !  0 r REC.NEXT !
   OUT-READY r REC.OUT !  0 r REC.RES ! ;

: REC-CLAIM ( -- ^n )
   AIO-LOCK TASK:GET
   REC-TAKE {: rc:n idx:n :}
   AIO-LOCK TASK:RELEASE
   rc 0<> if rc throw then
   idx REC dup REC-INIT ;

\ ---- the deadline heap --------------------------------------------------------
: H-SLOT ( n -- ^n )
   cells HEAP-BASE @ + AIO-N>PTR ;

: H-REC ( n -- ^n )
   H-SLOT @ AIO-N>PTR ;

: H-KEY ( n -- n )
   H-REC REC.DEADLINE @ ;

: H-PUT ( ^n n -- ) {: r:^ i:n :}
   r FFI:>CELL i H-SLOT !
   i r REC.HEAP-POS ! ;

: H-LESS? ( n n -- bool ) {: i:n j:n :}
   i H-KEY j H-KEY < ;

: H-SWAP ( n n -- ) {: i:n j:n :}
   i H-REC j H-REC {: a:^ b:^ :}
   b i H-PUT  a j H-PUT ;

: H-PARENT ( n -- n )
   1 - 2 / ;

\ Whether row i rose a level: its deadline is before its parent's.
: H-RISE? ( n -- bool ) {: i:n :}
   i 0= if false exit then
   i i H-PARENT H-LESS? 0= if false exit then
   i i H-PARENT H-SWAP
   true ;

: H-UP ( n -- )
   begin dup H-RISE? while H-PARENT repeat drop ;

\ Row i, or the child of i whose deadline is earliest when it is before i's.
: H-LEAST ( n -- n ) {: i:n :}
   i 2 * 1 + {: l:n :}
   i
   l HEAP-LEN @ < if l over H-LESS? if drop l then then
   l 1 + HEAP-LEN @ < if l 1 + over H-LESS? if drop l 1 + then then ;

: H-DOWN ( n -- )
   begin dup H-LEAST 2dup <> while 2dup H-SWAP nip repeat 2drop ;

\ Room for one more row, made before a record is claimed, so an arm never
\ fails half way.
: H-ROOM ( -- )
   HEAP-LEN @ HEAP-CAP @ < if exit then
   HEAP-CAP @ 2 * 16 max {: cap:n :}
   HEAP:ALLOCATOR  HEAP-BASE @ AIO-N>PTR BYTE-VIEW
   HEAP-CAP @ cells  cap cells
   ALLOC:RESIZE drop FFI:>CELL HEAP-BASE !
   cap HEAP-CAP ! ;

: H-PUSH ( ^n -- ) {: r:^ :}
   r HEAP-LEN @ H-PUT
   HEAP-LEN @ 1 + HEAP-LEN !
   HEAP-LEN @ 1 - H-UP ;

: H-REMOVE ( ^n -- ) {: r:^ :}
   r REC.HEAP-POS @ {: i:n :}
   -1 r REC.HEAP-POS !
   HEAP-LEN @ 1 - {: last:n :}
   last HEAP-LEN !
   i last = if exit then
   last H-REC i H-PUT
   i H-DOWN
   i H-UP ;

: HEAP-RELEASE ( -- )
   HEAP:ALLOCATOR HEAP-BASE @ AIO-N>PTR BYTE-VIEW ALLOC:FREE
   0 HEAP-BASE !  0 HEAP-LEN !  0 HEAP-CAP ! ;

\ ---- the descriptor table ----------------------------------------------------
512 constant FD-FIRST               \ one page of cells

: FD-SLOT ( n -- ^n )
   cells FD-BASE @ + AIO-N>PTR ;

\ The descriptor's first armed record, index plus 1, or 0.
: FD-HEAD ( n -- n ) {: fd:n :}
   fd 0 < fd FD-CAP @ >= or if 0 exit then
   fd FD-SLOT @ ;

: FD-BYTES ( n -- NUM:alloc-byte-len )
   cells MEM:BYTES-ALLOC-LEN ;

: FD-RELEASE ( -- )
   FD-CAP @ 0= if exit then
   FD-BASE @ AIO-N>PTR BYTE-VIEW FD-CAP @ FD-BYTES MEM:RELEASE-BYTES
   0 FD-BASE !  0 FD-CAP ! ;

\ The descriptor's row, made before a record is claimed: 0, or the errno of a
\ number past the table that names no open descriptor. A fresh mapping is zero.
: FD-ROOM ( n -- n ) {: fd:n :}
   fd FD-CAP @ < if 0 exit then
   fd FD-ERRNO dup 0<> if exit then drop
   FD-CAP @ FD-FIRST max begin dup fd <= while 2 * repeat {: cap:n :}
   cap FD-BYTES MEM:ALLOC-BYTES drop FFI:>CELL {: base:n :}
   FD-CAP @ 0 ?do
      i FD-SLOT @ dup 0<> if base i cells + AIO-N>PTR ! else drop then
   loop
   FD-RELEASE
   base FD-BASE !
   cap FD-CAP !
   0 ;

\ ---- the record's transitions ------------------------------------------------
: REC-OF ( n -- ^n )
   1 - REC ;

: REC-LINK ( ^n -- n )
   REC.IDX @ 1 + ;

: CHAIN-EVENTS ( n -- n )
   0 swap
   begin dup 0<> while
      REC-OF dup REC.EVENTS @ rot or swap REC.FD-NEXT @
   repeat
   drop ;

\ ARMED->DONE, onto its group's done list.
: FINISH ( ^n n n -- ) {: r:^ out:n res:n :}
   out r REC.OUT !
   res r REC.RES !
   STATE-DONE r REC.STATE atomic!
   r REC.GROUP @ 0<> if r REC-G r DONE-PUSH then ;

\ A failed arm leaves the heap and ends the record at once.
: ARM-FAILED ( ^n n -- ) {: r:^ rc:n :}
   r REC.HEAP-POS @ 0 >= if r H-REMOVE then
   rc Q-READY-NOW = if r OUT-READY r REC.EVENTS @ FINISH exit then
   r OUT-REJECTED rc FINISH ;

\ A re-arm that failed ends the whole chain as a failed arm.
: CHAIN-FAIL ( n n -- ) {: fd:n rc:n :}
   fd FD-HEAD
   0 fd FD-SLOT !
   begin dup 0<> while
      REC-OF dup REC.FD-NEXT @ swap
      0 over REC.FD-NEXT !
      rc ARM-FAILED
   repeat
   drop ;

\ Arms the descriptor one-shot for its chain's union and the events in n.
: FD-ARM ( n n -- n ) {: fd:n events:n :}
   TASK:QUEUE fd fd FD-HEAD CHAIN-EVENTS events or fd 1 + Q-ARM ;

: CHAIN-REARM ( n -- ) {: fd:n :}
   fd 0 FD-ARM {: rc:n :}
   rc 0<> if fd rc CHAIN-FAIL then ;

: CHAIN-UNLINK ( ^n -- ) {: r:^ :}
   r REC.FD @ {: fd:n :}
   r REC-LINK {: me:n :}
   r REC.FD-NEXT @ {: nx:n :}
   0 r REC.FD-NEXT !
   fd FD-HEAD me = if nx fd FD-SLOT ! exit then
   fd FD-HEAD
   begin dup 0<> while
      REC-OF dup REC.FD-NEXT @ me = if REC.FD-NEXT nx swap ! exit then
      REC.FD-NEXT @
   repeat
   drop ;

\ An armed record with a descriptor is on its chain.
: DISARM ( ^n -- ) {: r:^ :}
   r REC.FD @ {: fd:n :}
   r REC.HEAP-POS @ 0 >= if r H-REMOVE then
   fd 0 < if exit then
   r CHAIN-UNLINK
   fd FD-HEAD 0= if fd Q-DISARM exit then
   fd CHAIN-REARM ;

: CANCEL-REC ( ^n -- ) {: r:^ :}
   r DISARM
   r OUT-CANCELLED 0 FINISH ;

: DEADLINE! ( ^n n -- ) {: r:^ ms:n :}
   ms 0 < if exit then
   mono-ns ms NS-PER-MS * + r REC.DEADLINE !
   r H-PUSH ;

: CHAIN-JOIN ( ^n -- ) {: r:^ :}
   r REC.FD @ {: fd:n :}
   fd FD-HEAD r REC.FD-NEXT !
   r REC-LINK fd FD-SLOT ! ;

: ARM-FD ( ^n -- ) {: r:^ :}
   r REC.FD @ r REC.EVENTS @ FD-ARM {: rc:n :}
   rc 0<> if r rc ARM-FAILED exit then
   r CHAIN-JOIN ;

\ ---- adoption ----------------------------------------------------------------
\ The record becomes the holder's, answering whether the holder must arm it:
\ it was still pushed, so the worker's arm had not held when the CAS won. The
\ worker's arm may hold after all, so both arm: waiting for its verdict would
\ put the worker's system call in the holder's path.
: ADOPT-OWNER ( ^n -- bool ) {: r:^ :}
   TASK:SELF-N {: me:n :}
   r REC.OWNER atomic@ {: o:n :}
   o OWNER-CONFIRMED <> if
      o me r REC.OWNER atomic-cas o = if true exit then
   then
   me r REC.OWNER atomic!
   false ;

\ Only the holder can grow its own tables, so the room is made here: 0, or
\ FD-ROOM's errno for a number that names no open descriptor.
: ADOPT-ROOM ( ^n -- n ) {: r:^ :}
   H-ROOM
   r REC.FD @ FD-ROOM ;

\ One record of a taken inbox, answering the next: REC.NEXT is read first, as
\ FINISH rewrites it. A failed room ends it rejected, out of the queue. One
\ ended here wakes the task's queue: its wait looked for a done record before.
: ADOPT-ONE ( n -- n ) {: rn:n :}
   rn AIO-N>PTR {: r:^ :}
   r REC.NEXT @ {: nx:n :}
   r ADOPT-OWNER {: arm:bool :}
   [: r ADOPT-ROOM ;] catch {: code:n :}
   code 0<> if drop ERR-NOMEM then {: rc:n :}
   rc 0<> if
      r REC.FD @ Q-DISARM
      r OUT-REJECTED rc FINISH
   else
      r REC.DEADLINE @ 0 >= if r H-PUSH then
      arm if r ARM-FD else r CHAIN-JOIN then
   then
   r DONE? if TASK:SELF TASK:QUEUE-WAKE then
   nx ;

: G-ADOPT ( ^n -- )
   INBOX-TAKE begin dup 0<> while ADOPT-ONE repeat drop ;

\ Every group the task holds; before EVENTS-TAKE, which drops an event of a
\ descriptor with no chain.
: ADOPT-HELD ( -- )
   HELD @
   begin dup 0<> while
      1 - G-ENTRY dup G-ADOPT G.HELD @
   repeat
   drop ;

: ADOPT-OF ( ^n -- ) {: r:^ :}
   r REC.OWNER atomic@ 0 < if r REC-G G-ADOPT then ;

: OUTCOME-OF ( ^n -- outcome ) {: r:^ :}
   r REC.RES @ {: res:n :}
   r REC.OUT @ {: k:n :}
   k OUT-READY = if res AIO-OUTCOME:READY exit then
   k OUT-TIMED-OUT = if AIO-OUTCOME:TIMED-OUT exit then
   k OUT-CANCELLED = if AIO-OUTCOME:CANCELLED exit then
   res AIO-OUTCOME:REJECTED ;

\ DONE->FREE.
: TAKE ( ^n -- outcome ) {: r:^ :}
   r REC.GROUP @ 0<> if r G-LEAVE then
   r OUTCOME-OF
   STATE-FREE r REC.STATE atomic! ;

\ ---- the owner's wait ---------------------------------------------------------
\ The end of the next kernel wait, END-NEVER with no deadline armed.
: WAIT-END ( -- n )
   HEAP-LEN @ 0= if END-NEVER else 0 H-KEY then {: end:n :}
   HELD @ 0= if end exit then
   FLOOR @ {: floor:n :}
   floor mono-ns <= if end exit then
   end floor min ;

\ The end the holder sleeps to, published in each held group's G.END.
: SLEEP-END ( -- n )
   begin
      ADOPT-HELD
      WAIT-END
      HELD @ 0= if exit then
      HELD @
      begin dup 0<> while
         1 - G-ENTRY 2dup G.END atomic! G.HELD @
      repeat
      drop
      fence
      false HELD @
      begin dup 0<> while
         1 - G-ENTRY dup G.INBOX atomic@ 0<> rot or swap G.HELD @
      repeat
      drop
   while
      drop
   repeat ;

\ The ms the kernel wait may take until the end n: -1 for END-NEVER, 0 once
\ it has passed, else rounded up, at most $7FFFFFFF.
: WAIT-MS ( n -- n ) {: end:n :}
   end END-NEVER = if -1 exit then
   end mono-ns - {: left:n :}
   left 0 <= if 0 exit then
   left NS-PER-MS 1 - + NS-PER-MS / $7FFFFFFF min ;

\ POLLERR and POLLHUP. epoll polls the open file, never the number, so it
\ answers no POLLNVAL.
$18 constant EV-FAULT

: EV-MEETS? ( ^n n -- bool ) {: r:^ revents:n :}
   revents EV-FAULT and 0<> if true exit then
   r REC.EVENTS @ revents and 0<> ;

\ A record of the chain the event took: it ends ready or goes back on the
\ chain. Answers the next.
: EV-ONE ( n n n -- n ) {: fd:n revents:n link:n :}
   link REC-OF {: r:^ :}
   r REC.FD-NEXT @ {: nx:n :}
   r revents EV-MEETS? if
      0 r REC.FD-NEXT !
      r REC.HEAP-POS @ 0 >= if r REC.DEADLINE @ FLOOR ! r H-REMOVE then
      r OUT-READY revents FINISH
   else
      fd FD-HEAD r REC.FD-NEXT !
      link fd FD-SLOT !
   then
   nx ;

\ The one-shot fired, so the records left are armed again. An id with no chain
\ is dropped.
: EV-TAKE ( n n -- ) {: id:n revents:n :}
   id 1 - {: fd:n :}
   fd FD-HEAD {: first:n :}
   first 0= if exit then
   0 fd FD-SLOT !
   first begin dup 0<> while fd revents rot EV-ONE repeat drop
   fd FD-HEAD 0<> if fd CHAIN-REARM then ;

\ Answers whether the queue's wake was among the n events.
: EVENTS-TAKE ( n -- bool )
   false swap 0 ?do
      i EV-ID dup 0= if drop drop true else i EV-REVENTS EV-TAKE then
   loop ;

\ Ends the earliest deadline if it has passed, answering whether it did.
: EXPIRE-TOP ( n -- bool ) {: now:n :}
   HEAP-LEN @ 0= if false exit then
   0 H-REC {: r:^ :}
   r REC.DEADLINE @ now > if false exit then
   r DISARM
   r OUT-TIMED-OUT 0 FINISH
   true ;

: EXPIRE ( -- )
   mono-ns begin dup EXPIRE-TOP 0= until drop ;

\ One kernel wait and what it ended. The queue is made and fenced before the
\ halt check, so a TASK:HALT after the check posts it and ends the wait.
: WAIT-PASS ( -- )
   TASK:QUEUE drop fence
   0 TASK:HALT-UNWIND
   SLEEP-END WAIT-MS Q-WAIT
   ADOPT-HELD
   EVENTS-TAKE
   EXPIRE
   if 0 TASK:HALT-UNWIND then ;

: REC-WAIT ( n -- ) {: idx:n :}
   idx REC {: r:^ :}
   begin r DONE? 0= while
      idx AIO-PARKED !
      WAIT-PASS
      -1 AIO-PARKED !
   repeat ;

: WAIT-GIVE-BACK ( -- )
   AIO-PARKED @ dup 0 < if drop exit then
   REC dup ARMED? if CANCEL-REC else drop then ;

\ A halt that ends the task in the wait unwinds through this finally, which
\ cancels the wait's record before an outer cleanup can close its descriptor.
: WAIT-DONE ( n -- ) {: idx:n :}
   -1 AIO-PARKED !
   idx [: REC-WAIT ;] [: WAIT-GIVE-BACK ;] finally ;

: AWAIT-ONE ( n -- outcome ) {: idx:n :}
   idx WAIT-DONE idx REC TAKE ;

: GROUP-WAIT ( n -- ) {: g:n :}
   g G-ENTRY {: e:^ :}
   begin e G.HEAD @ 0= while
      g AIO-PARKED !
      WAIT-PASS
      -1 AIO-PARKED !
   repeat ;

: GIVE-BACK-IF ( n n n -- ) {: idx:n gid:n me:n :}
   idx REC {: r:^ :}
   r ARMED? 0= if exit then
   r REC.OWNER @ me <> if exit then
   r REC.GROUP @ gid <> if exit then
   r CANCEL-REC ;

\ Cancels the group's armed records; only the owner writes them, so no lock.
: GROUP-GIVE-BACK ( -- )
   AIO-PARKED @ {: g:n :}
   g 0 < if exit then
   TASK:SELF-N {: me:n :}
   REC-CAP 0 ?do i g 1 + me GIVE-BACK-IF loop ;

: AWAIT-GROUP ( n -- n outcome ) {: g:n :}
   g G-ENTRY G.COUNT atomic@ 0= if E-AIO-GROUP throw then
   -1 AIO-PARKED !
   g [: GROUP-WAIT ;] [: GROUP-GIVE-BACK ;] finally
   g G-ENTRY G.HEAD @ AIO-N>PTR {: r:^ :}
   r REC.TAG @ r TAKE ;

\ ---- the cleanup a submitting task registers ---------------------------------
\ The group's unadopted records become the ending task's, for SCRUB-ONE to
\ disarm and free.
: INBOX-DRAIN ( ^n -- )
   TASK:SELF-N {: me:n :}
   INBOX-TAKE
   begin dup 0<> while
      AIO-N>PTR me over REC.OWNER atomic! REC.NEXT @
   repeat
   drop ;

\ The ending task's held groups go back empty and off its list, so no group
\ stays held by a task that is gone. The count goes to zero last: from then
\ another task may take the group and rewrite its cells.
: GROUPS-RELEASE ( -- )
   HELD @
   begin dup 0<> while
      1 - G-ENTRY
      dup G.HELD @ swap
      dup INBOX-DRAIN
      0 over G.HEAD !  0 over G.TAIL !
      0 swap G.COUNT atomic!
   repeat
   drop
   0 HELD ! ;

: SCRUB-ONE ( n n -- ) {: idx:n me:n :}
   idx REC {: r:^ :}
   r REC.STATE atomic@ STATE-FREE = if exit then
   r REC.OWNER @ me <> if exit then
   r ARMED? r REC.FD @ 0 >= and if r REC.FD @ Q-DISARM then
   STATE-FREE r REC.STATE atomic! ;

\ The ending task's records go free, its armed ones disarmed first.
: SCRUB ( -- )
   GROUPS-RELEASE
   HEAP-RELEASE
   FD-RELEASE
   TASK:SELF-N {: me:n :}
   REC-CAP 0 ?do i me SCRUB-ONE loop ;

: ENSURE-SCRUB ( -- )
   TASK:SELF-N 0= if exit then
   AIO-REGISTERED @ 0 <> if exit then
   1 AIO-REGISTERED !
   ['] SCRUB TASK:SELF TASK:AT-EXIT ;

\ ---- submission --------------------------------------------------------------
\ The fd as the kernel's int reads it: its low 32 bits, sign-extended.
: FD-INT ( fd -- n )
   FD>N FFI:>I32 ;

\ ---- arming into another task's group ----------------------------------------
\ FREE->ARMED, unpublished. The owner is pushed, -2 minus the worker's id, so
\ its CAS cannot match the record once freed and claimed again.
: INTO-CLAIM ( n n n n n -- ^n )
   {: fd:n events:n ms:n gn:n tag:n :}
   REC-CLAIM {: r:^ :}
   fd r REC.FD !  events r REC.EVENTS !
   gn 1 + r REC.GROUP !  tag r REC.TAG !
   ms 0 < if -1 else mono-ns ms NS-PER-MS * + then r REC.DEADLINE !
   -2 TASK:SELF-N - r REC.OWNER !
   r ;

\ Pushed before armed, as the holder drops an event of a descriptor with no
\ chain; all it needs of the record is read first, as the holder may free it.
: INTO-PUBLISH ( ^n ^n n n -- ) {: e:^ r:^ fd:n events:n :}
   e G.QUEUE atomic@ {: q:n :}
   e G.OWNER atomic@ AIO-N>PTR {: h:^ :}
   r REC.OWNER @ {: pushed:n :}
   r REC.DEADLINE @ {: due:n :}
   e r INBOX-PUSH
   q fd events fd 1 + Q-ARM {: rc:n :}
   rc 0= if pushed OWNER-CONFIRMED r REC.OWNER atomic-cas drop then
   fence
   rc 0<>  due 0 >= due e G.END atomic@ < and  or if h TASK:QUEUE-WAKE then ;

public

\ The two event masks a POLL takes; they are POLLIN and POLLOUT, so they
\ combine with `or` and come back in the `ready` arm as the kernel's revents.
: READABLE ( -- n )
   EV-READABLE ;

: WRITABLE ( -- n )
   EV-WRITABLE ;

\ Waits for a descriptor to carry one of the events in the mask. ms is a
\ deadline in milliseconds, or -1 for no deadline. A descriptor the host
\ cannot wait on ends the ticket rejected with its errno; a regular file is
\ ready at once, as poll(2) answers it. A `ready` can be spurious once, after a
\ descriptor closed while armed whose file a duplicate kept open, so a reader
\ reads non-blocking. Each task that waits holds two descriptors, its queue and
\ its wake, from its first POLL or TIMEOUT for its TCB's life; a first one made
\ without them throws E-TASK-THREAD with nothing claimed.
: POLL ( fd n ms -- ticket ) {: f:fd events:n timeout:ms :}
   ENSURE-SCRUB
   H-ROOM
   TASK:QUEUE drop
   f FD-INT FD-ROOM {: rc:n :}
   REC-CLAIM {: r:^ :}
   f FD-INT r REC.FD !
   events r REC.EVENTS !
   r timeout MS>N DEADLINE!
   rc 0<> if r rc ARM-FAILED else r ARM-FD then
   r >TICKET ;

\ Fires once, after ms milliseconds. The outcome is timed-out, or cancelled when
\ a CANCEL reached it first.
: TIMEOUT ( ms -- ticket ) {: timeout:ms :}
   ENSURE-SCRUB
   H-ROOM
   TASK:QUEUE drop
   REC-CLAIM {: r:^ :}
   r timeout MS>N DEADLINE!
   r >TICKET ;

\ Ends that operation as cancelled unless it has ended already; the ticket
\ still has to be AWAITed. Only the task that submitted it may cancel it.
: CANCEL ( ticket -- )
   TICKET>REC dup ADOPT-OF dup ARMED? if CANCEL-REC else drop then ;

\ Blocks until that operation has ended and answers its outcome, releasing the
\ record. Only the task that submitted the ticket may await it, and only once.
\ The main thread may await too.
: AWAIT ( ticket -- outcome ) {: t:ticket :}
   t TICKET>REC dup ADOPT-OF REC.IDX @ AWAIT-ONE ;

\ What a wait for readiness ended as: the revents the kernel reported, the
\ deadline, or the errno of a failed poll.
ENUM readiness
   VARIANT ready FIELD revents n ;VARIANT
   VARIANT timed-out ;VARIANT
   VARIANT rejected FIELD errno n ;VARIANT
;ENUM

\ POLL and AWAIT in one call, under the same rules. Nothing outside this
\ word holds the ticket, so nothing cancels it; the cancelled arm answers
\ ECANCELED.
: WAIT-READY ( fd n ms -- readiness )
   POLL AWAIT
   MATCH outcome
      ready OF AIO-READINESS:READY ENDOF
      timed-out OF AIO-READINESS:TIMED-OUT ENDOF
      cancelled OF ERR-CANCELED AIO-READINESS:REJECTED ENDOF
      rejected OF AIO-READINESS:REJECTED ENDOF
   ;MATCH ;

\ ---- the waker ---------------------------------------------------------------
\ A self-pipe one parked task is woken through: other tasks change shared state
\ and WAKE it, and the task, parked in AWAIT-ANY on WAKER-POLL's ticket among
\ its others, calls WOKE and then scans that state. At most one byte is owed,
\ so the pipe never fills and a WAKE from the parked task itself never blocks.
\ The storage is the caller's; owed is 1 from the WAKE that writes the byte
\ until WOKE has read it.
STRUCTURE waker OPAQUE DERIVE addr
   FIELD read-end fd
   FIELD write-end fd
   FIELD owed n
;STRUCTURE

private

create WAKE-BYTE $01 c,
\ What WOKE reads lands here unread, so every parked task shares it.
1 SCRATCH-BUFFER: SINK

public

\ Opens the waker's pipe, with no byte owed. FD:PIPE's code is thrown when the
\ pipe fails to open, and nothing is left open then.
: WAKER-OPEN ( ^waker -- ) {: w:^ :}
   FD:PIPE w WAKER-WRITE-END ! w WAKER-READ-END !
   0 w WAKER-OWED atomic! ;

\ Closes both ends. No WAKE may run after it or while it runs: the caller orders
\ that, as by clearing its own open flag under the lock its wakers check it in,
\ since a closed descriptor may name another file by then.
: WAKER-CLOSE ( ^waker -- ) {: w:^ :}
   w WAKER-READ-END @ FD>N close-rc drop
   w WAKER-WRITE-END @ FD>N close-rc drop ;

\ From any task, with no lock: fences, then writes the byte unless one is owed;
\ of WAKEs racing for the flag one writes. Never during or after WAKER-CLOSE.
: WAKE ( ^waker -- ) {: w:^ :}
   fence
   0 1 w WAKER-OWED atomic-cas 0<> if exit then
   w WAKER-WRITE-END @ FD>N WAKE-BYTE 1 >str write drop ;

\ The read end polled for readable, with no deadline, for the parked task to
\ add to its group: ready once a WAKE has written.
: WAKER-POLL ( ^waker -- ticket ) {: w:^ :}
   w WAKER-READ-END @ READABLE -1 >MS POLL ;

\ On the parked task, once WAKER-POLL's ticket is ready: reads the one byte
\ owed, then clears the flag and fences, so a WAKE after it writes again and the
\ scan the caller makes next sees each change made before a WAKE.
: WOKE ( ^waker -- ) {: w:^ :}
   w WAKER-READ-END @ FD>N SINK 1 SPAN:MAKE read drop
   0 w WAKER-OWED atomic! fence ;

: GROUP ( -- )
   create G-REGISTER ,
   does> ( -- AIO:group ) @ >GROUP ;

\ A group belongs to the task that adds to it while empty until it is empty
\ again; no other task may GROUP+, GROUP- or AWAIT-ANY on it meanwhile. When
\ the holder ends, AIO's exit cleanup empties every group it holds, so a cleanup
\ of the caller's that runs after AIO's finds them empty.
\ The number is the caller's, any value; AWAIT-ANY hands it back for this wait.
: GROUP+ ( ticket n group -- ) {: t:ticket tag:n g:group :}
   g GROUP>N t TICKET>REC tag G-ADD ;

\ POLL from any task into a group another task holds, with POLL's outcome; the
\ ticket joins the group with the tag as GROUP+ would, and only the holder
\ AWAIT-ANYs it. A group no task holds is E-AIO-GROUP. The caller keeps that
\ (a) the call returns before the holder ends, else the record outlives it,
\ and (b) until the call returns, and while the ticket is armed, the holder
\ arms nothing else on that descriptor, else the one registration's events
\ are replaced.
: POLL-INTO ( fd n ms n group -- ticket )
   {: f:fd events:n timeout:ms tag:n g:group :}
   g GROUP>N {: gn:n :}
   gn G-ENTRY {: e:^ :}
   f FD-INT {: fd:n :}
   fd events timeout MS>N gn tag INTO-CLAIM {: r:^ :}
   e G-PIN 0= if STATE-FREE r REC.STATE atomic! E-AIO-GROUP throw then
   e r fd events INTO-PUBLISH
   r >TICKET ;

\ A ticket the group does not hold is E-AIO-GROUP. A group no task holds is
\ empty: E-AIO-GROUP.
: GROUP- ( ticket group -- ) {: t:ticket g:group :}
   t TICKET>REC {: r:^ :}
   r ADOPT-OF
   r REC.GROUP @ g GROUP>N 1 + <> if E-AIO-GROUP throw then
   r G-LEAVE ;

\ The number GROUP+ was given with the first wait of the group whose operation
\ has ended, with its outcome. That ticket leaves the group and its record is
\ released, exactly as AWAIT does; the others stay. An empty group has nothing to
\ wait for and is E-AIO-GROUP.
: AWAIT-ANY ( group -- n outcome ) {: g:group :}
   g GROUP>N AWAIT-GROUP ;

;package
