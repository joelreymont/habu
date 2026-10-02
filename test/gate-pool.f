\ gate-pool.f - checked bounded process pool for native tests.

require lib/fmt.f
require lib/process-fork.f
require lib/process-env.f
require lib/process-tree.f               \ a slot ends with everything descended from its child
require lib/signal.f                     \ a signalled run is answered at a pool step
require lib/test/runner.f
require lib/test/suite.f                 \ TEST:ITEM-MAX sizes the red table
require tools/why-threw.f

using WHY-THREW                          \ the fork-throw self-identifying report

16 constant GT-POOL-MAX
6 constant GT-POOL-LINUX-DEFAULT
8 constant GT-POOL-MACOS-DEFAULT
2 constant GT-POOL-FDS
8 constant GT-PFD-SZ
$64 constant GT-POOL-POLL-MS
$1000 constant GT-POOL-CHUNK-CAP
64 constant GT-POOL-NAME-CAP
32 constant GT-POOL-NUM-CAP
\ Rows an adapter starts beside the registered suites: test/gate-images.f starts
\ one build row per keyed image family, and refuses more families than this.
8 constant GT-POOL-SIDE-MAX
10000 constant GT-POOL-GRACE-MS         \ a root that catches SIGTERM ends itself inside this (GT-POOL-ASK-END)
\ Red rows: one per registered suite (lib/test/suite.f ITEM-MAX) plus the side
\ rows, so a complete run reports every red with its exit code and capture
\ paths, and a row the table has no record of passed.
TEST:ITEM-MAX GT-POOL-SIDE-MAX + constant GT-POOL-RED-MAX
GT-POOL-MAX GT-OUT-CAP * constant GT-POOL-OUT-BYTES
GT-POOL-MAX GT-ERR-CAP * constant GT-POOL-ERR-BYTES

create GT-POOL-PIDS GT-POOL-MAX cells allot
create GT-POOL-REAPER-PIDS GT-POOL-MAX cells allot
create GT-POOL-OUT-RS GT-POOL-MAX cells allot
create GT-POOL-OUT-WS GT-POOL-MAX cells allot
create GT-POOL-ERR-RS GT-POOL-MAX cells allot
create GT-POOL-ERR-WS GT-POOL-MAX cells allot
create GT-POOL-OUT-US GT-POOL-MAX cells allot
create GT-POOL-ERR-US GT-POOL-MAX cells allot
create GT-POOL-EXITEDS GT-POOL-MAX cells allot
create GT-POOL-TIMED-OUTS GT-POOL-MAX cells allot
create GT-POOL-CODES GT-POOL-MAX cells allot
create GT-POOL-DONES GT-POOL-MAX cells allot
create GT-POOL-STARTS GT-POOL-MAX cells allot
create GT-POOL-LASTS GT-POOL-MAX cells allot
create GT-POOL-TIMEOUTS GT-POOL-MAX cells allot
create GT-POOL-ASKEDS GT-POOL-MAX cells allot
create GT-POOL-LABELS GT-POOL-MAX GT-FAIL-NAME-CAP * allot
create GT-POOL-LABEL-US GT-POOL-MAX cells allot
create GT-POOL-PFDS GT-POOL-MAX GT-POOL-FDS * GT-PFD-SZ * allot
create GT-POOL-CHUNK GT-POOL-CHUNK-CAP allot
create GT-POOL-NUM-BUF GT-POOL-NUM-CAP allot
create GT-POOL-NAME-BUF GT-POOL-NAME-CAP allot
create GT-POOL-FALLBACK-BUF FS-PATH-CAP allot
create GT-POOL-OUT-PATHS GT-POOL-MAX FS-PATH-CAP * allot
create GT-POOL-ERR-PATHS GT-POOL-MAX FS-PATH-CAP * allot
create GT-POOL-OUT-PATH-US GT-POOL-MAX cells allot
create GT-POOL-ERR-PATH-US GT-POOL-MAX cells allot
create GT-POOL-TMP-PATHS GT-POOL-MAX FS-PATH-CAP * allot
create GT-POOL-TMP-PATH-US GT-POOL-MAX cells allot
create GT-POOL-SOCK-PATHS GT-POOL-MAX FS-PATH-CAP * allot
create GT-POOL-SOCK-PATH-US GT-POOL-MAX cells allot
create GT-POOL-SOCK-ROOT-BUF FS-PATH-CAP allot
create GT-POOL-OUT-FDS GT-POOL-MAX cells allot
create GT-POOL-ERR-FDS GT-POOL-MAX cells allot
create GT-POOL-OUT-TOTALS GT-POOL-MAX cells allot
create GT-POOL-ERR-TOTALS GT-POOL-MAX cells allot
create GT-POOL-SEQS GT-POOL-MAX cells allot
create GT-POOL-WAITS GT-POOL-MAX cells allot
create GT-POOL-SAT-LIVES GT-POOL-MAX cells allot
create GT-POOL-RED-LABELS GT-POOL-RED-MAX GT-FAIL-NAME-CAP * allot
create GT-POOL-RED-LABEL-US GT-POOL-RED-MAX cells allot
create GT-POOL-RED-EXITEDS GT-POOL-RED-MAX cells allot
create GT-POOL-RED-TIMED-OUTS GT-POOL-RED-MAX cells allot
create GT-POOL-RED-CODES GT-POOL-RED-MAX cells allot
create GT-POOL-RED-SEQS GT-POOL-RED-MAX cells allot
create GT-POOL-RED-PATH-BUF FS-PATH-CAP allot
create GT-POOL-RED-WAITS GT-POOL-RED-MAX cells allot
create GT-POOL-RED-SAT-LIVES GT-POOL-RED-MAX cells allot
create GT-POOL-RED-SAT-LIMITS GT-POOL-RED-MAX cells allot
create GT-POOL-RED-SAT-MSS GT-POOL-RED-MAX cells allot

TYPED-VARIABLE GT-POOL-OUT-BUFS-A ptr u8
TYPED-VARIABLE GT-POOL-ERR-BUFS-A ptr u8
variable GT-POOL-LIVE
variable GT-POOL-RD
variable GT-POOL-LIMIT
variable GT-POOL-REQ
variable GT-POOL-SEQ
variable GT-POOL-NUM-U
variable GT-POOL-NAME-U
variable GT-POOL-WR
variable GT-POOL-WR-OFF
variable GT-POOL-RED-N
variable GT-POOL-RED-PATH-U
variable GT-POOL-FALLBACK-U
variable GT-POOL-DEATH-RD
variable GT-POOL-DEATH-WR
variable GT-POOL-DEATH-MADE

: GT-POOL-RED# ( -- n )
   GT-POOL-RED-N @ ;

: GT-POOL-RED-RESET ( -- )
   0 GT-POOL-RED-N ! ;

: GT-POOL-OUT-BUFS@ ( -- ptr u8 )
   GT-POOL-OUT-BUFS-A @ ;

: GT-POOL-OUT-BUFS! ( ptr u8 -- )
   GT-POOL-OUT-BUFS-A ! ;

: GT-POOL-OUT-BUFS ( -- ptr u8 )
   GT-POOL-OUT-BUFS@ ;

: GT-POOL-ERR-BUFS@ ( -- ptr u8 )
   GT-POOL-ERR-BUFS-A @ ;

: GT-POOL-ERR-BUFS! ( ptr u8 -- )
   GT-POOL-ERR-BUFS-A ! ;

: GT-POOL-ERR-BUFS ( -- ptr u8 )
   GT-POOL-ERR-BUFS@ ;

: GT-POOL-ALLOC-BYTES ( n -- ptr u8 )
   MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop ;

: GT-POOL-ALLOC-BUFFERS ( -- )
   GT-POOL-OUT-BUFS@ 0= if
      GT-POOL-OUT-BYTES GT-POOL-ALLOC-BYTES GT-POOL-OUT-BUFS!
   then
   GT-POOL-ERR-BUFS@ 0= if
      GT-POOL-ERR-BYTES GT-POOL-ALLOC-BYTES GT-POOL-ERR-BUFS!
   then ;

\ Name-builder overflows are infra failures that can happen while children
\ are live (capture-start runs inside the pool). Route them through this
\ defer so they kill the pool like any other infra throw; it is installed to
\ GT-POOL-THROW once that word exists (below), staying a bare throw only for
\ the pre-pool build-time self-tests.
defer GT-POOL-ABORT ( n -- )
: GT-POOL-ABORT-BARE ( n -- ) throw ;
: GT-POOL-ABORT-BARE! ( -- ) [: GT-POOL-ABORT-BARE ;] is GT-POOL-ABORT ;
GT-POOL-ABORT-BARE!

: GT-POOL-NUM-C+ ( n -- ) {: c:n :}
   GT-POOL-NUM-U @ GT-POOL-NUM-CAP >= if E-STR-CAPACITY GT-POOL-ABORT then
   c GT-POOL-NUM-BUF GT-POOL-NUM-U @ + c!
   GT-POOL-NUM-U @ 1+ GT-POOL-NUM-U ! ;

: GT-POOL-NUM+ ( n -- ) {: v:n :}
   v 10 >= if v 10 / RECURSE then
   v 10 mod STR-ZERO + GT-POOL-NUM-C+ ;

: GT-POOL-NUM$ ( n -- ptr u8 n ) {: v:n :}
   v 0 < if E-STR-BOUNDS GT-POOL-ABORT then
   0 GT-POOL-NUM-U !
   v GT-POOL-NUM+
   GT-POOL-NUM-BUF GT-POOL-NUM-U @ ;

: GT-POOL-NAME-RESET ( -- )
   0 GT-POOL-NAME-U ! ;

: GT-POOL-NAME+ ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0 < if E-STR-BOUNDS GT-POOL-ABORT then
   GT-POOL-NAME-U @ u + GT-POOL-NAME-CAP > if E-STR-CAPACITY GT-POOL-ABORT then
   a GT-POOL-NAME-BUF GT-POOL-NAME-U @ + u BYTE-COPY
   GT-POOL-NAME-U @ u + GT-POOL-NAME-U ! ;

: GT-POOL-NAME$ ( -- ptr u8 n )
   GT-POOL-NAME-BUF GT-POOL-NAME-U @ ;

: GT-POOL-CAPTURE-NAME ( n ptr u8 n -- ptr u8 n ) {: seq:n suf:ptr sufu:n :}
   GT-POOL-NAME-RESET
   s" pool-" GT-POOL-NAME+
   getpid GT-POOL-NUM$ GT-POOL-NAME+
   s" -" GT-POOL-NAME+
   seq GT-POOL-NUM$ GT-POOL-NAME+
   suf sufu GT-POOL-NAME+
   GT-POOL-NAME$ ;

: GT-POOL-PID-PTR ( idx -- ptr pid )
   IDX>N cells GT-POOL-PIDS + ;

: GT-POOL-REAPER-PID-PTR ( idx -- ptr pid )
   IDX>N cells GT-POOL-REAPER-PIDS + ;

: GT-POOL-OUT-R-PTR ( idx -- ptr fd )
   IDX>N cells GT-POOL-OUT-RS + ;

: GT-POOL-OUT-W-PTR ( idx -- ptr fd )
   IDX>N cells GT-POOL-OUT-WS + ;

: GT-POOL-ERR-R-PTR ( idx -- ptr fd )
   IDX>N cells GT-POOL-ERR-RS + ;

: GT-POOL-ERR-W-PTR ( idx -- ptr fd )
   IDX>N cells GT-POOL-ERR-WS + ;

: GT-POOL-OUT-U-PTR ( idx -- ptr n )
   IDX>N cells GT-POOL-OUT-US + ;

: GT-POOL-ERR-U-PTR ( idx -- ptr n )
   IDX>N cells GT-POOL-ERR-US + ;

: GT-POOL-EXITED-PTR ( idx -- ptr bool )
   IDX>N cells GT-POOL-EXITEDS + ;

: GT-POOL-TIMED-OUT-PTR ( idx -- ptr bool )
   IDX>N cells GT-POOL-TIMED-OUTS + ;

: GT-POOL-CODE-PTR ( idx -- ptr n )
   IDX>N cells GT-POOL-CODES + ;

\ Decompose a worker outcome into the slot's exited/timed-out/code cells
\ (lossless: exit codes >= 128 stay distinct from signal deaths).
: GT-POOL-OUTCOME! ( outcome idx -- ) {: idx:idx :}
   MATCH outcome
     exited OF idx GT-POOL-CODE-PTR ! 0 0= idx GT-POOL-EXITED-PTR ! 0 0= 0= idx GT-POOL-TIMED-OUT-PTR ! ENDOF
     signaled OF idx GT-POOL-CODE-PTR ! 0 0= 0= idx GT-POOL-EXITED-PTR ! 0 0= 0= idx GT-POOL-TIMED-OUT-PTR ! ENDOF
     timeout OF 0 idx GT-POOL-CODE-PTR ! 0 0= 0= idx GT-POOL-EXITED-PTR ! 0 0= idx GT-POOL-TIMED-OUT-PTR ! ENDOF
   ;MATCH ;

\ Distinct verdict token for a slot the pool's OWN timeout/reaper killed:
\ GT-POOL-TIMEOUT set the timed-out flag (via GT-POOL-OUTCOME! timeout variant)
\ before sending SIGKILL, so this is attributable pool saturation, not a real
\ exit/signal death. Still a red/failing test, but not to be misread as a
\ genuine failure on a contended host.
: GT-POOL-TIMEOUT-KIND$ ( -- ptr u8 n )
   s" TIMEOUT-UNDER-LOAD" ;

: GT-POOL-KIND-NAME. ( bool bool -- )   \ exited timed-out -> printed kind name
   if drop GT-POOL-TIMEOUT-KIND$ type exit then
   if s" exit" type exit then
   s" signal" type ;

: GT-POOL-DONE-PTR ( idx -- ptr n )
   IDX>N cells GT-POOL-DONES + ;

: GT-POOL-START-PTR ( idx -- ptr n )
   IDX>N cells GT-POOL-STARTS + ;

: GT-POOL-LAST-PTR ( idx -- ptr n )
   IDX>N cells GT-POOL-LASTS + ;

: GT-POOL-TIMEOUT-PTR ( idx -- ptr n )
   IDX>N cells GT-POOL-TIMEOUTS + ;

\ The slot's child was sent SIGTERM and is being given its grace.
: GT-POOL-ASKED-PTR ( idx -- ptr bool )
   IDX>N cells GT-POOL-ASKEDS + ;

: GT-POOL-LABEL-U-PTR ( idx -- ptr n )
   IDX>N cells GT-POOL-LABEL-US + ;

: GT-POOL-OUT-FD-PTR ( idx -- ptr fd )
   IDX>N cells GT-POOL-OUT-FDS + ;

: GT-POOL-ERR-FD-PTR ( idx -- ptr fd )
   IDX>N cells GT-POOL-ERR-FDS + ;

: GT-POOL-OUT-TOTAL-PTR ( idx -- ptr n )
   IDX>N cells GT-POOL-OUT-TOTALS + ;

: GT-POOL-ERR-TOTAL-PTR ( idx -- ptr n )
   IDX>N cells GT-POOL-ERR-TOTALS + ;

: GT-POOL-SEQ-PTR ( idx -- ptr n )
   IDX>N cells GT-POOL-SEQS + ;

\ Per-slot WAIT-heartbeat count and the pool-live saturation depth snapshotted
\ at timeout, so a timed-out RED line can report the load context.
: GT-POOL-WAITS-PTR ( idx -- ptr n )
   IDX>N cells GT-POOL-WAITS + ;

: GT-POOL-SAT-LIVE-PTR ( idx -- ptr n )
   IDX>N cells GT-POOL-SAT-LIVES + ;

: GT-POOL-OUT-PATH-BUF ( idx -- ptr u8 )
   IDX>N FS-PATH-CAP * GT-POOL-OUT-PATHS + ;

: GT-POOL-ERR-PATH-BUF ( idx -- ptr u8 )
   IDX>N FS-PATH-CAP * GT-POOL-ERR-PATHS + ;

: GT-POOL-OUT-PATH-U-PTR ( idx -- ptr n )
   IDX>N cells GT-POOL-OUT-PATH-US + ;

: GT-POOL-ERR-PATH-U-PTR ( idx -- ptr n )
   IDX>N cells GT-POOL-ERR-PATH-US + ;

: GT-POOL-TMP-PATH-BUF ( idx -- ptr u8 )
   IDX>N FS-PATH-CAP * GT-POOL-TMP-PATHS + ;

: GT-POOL-TMP-PATH-U-PTR ( idx -- ptr n )
   IDX>N cells GT-POOL-TMP-PATH-US + ;

\ The scratch directory this pool made for a SPAWNED slot's child, empty for a
\ forked slot, which shares the parent's image and gets no directory of its own.
: GT-POOL-TMP$ ( idx -- ptr u8 n ) {: idx:idx :}
   idx GT-POOL-TMP-PATH-BUF idx GT-POOL-TMP-PATH-U-PTR @ ;

: GT-POOL-SOCK-PATH-BUF ( idx -- ptr u8 )
   IDX>N FS-PATH-CAP * GT-POOL-SOCK-PATHS + ;

: GT-POOL-SOCK-PATH-U-PTR ( idx -- ptr n )
   IDX>N cells GT-POOL-SOCK-PATH-US + ;

\ The short directory this pool made for a SPAWNED slot's child's sockets,
\ empty for a forked slot (GT-POOL-CHILD-SOCK!).
: GT-POOL-SOCK$ ( idx -- ptr u8 n ) {: idx:idx :}
   idx GT-POOL-SOCK-PATH-BUF idx GT-POOL-SOCK-PATH-U-PTR @ ;

: GT-POOL-OUT-FILE$ ( idx -- ptr u8 n ) {: idx:idx :}
   idx GT-POOL-OUT-PATH-BUF idx GT-POOL-OUT-PATH-U-PTR @ ;

: GT-POOL-ERR-FILE$ ( idx -- ptr u8 n ) {: idx:idx :}
   idx GT-POOL-ERR-PATH-BUF idx GT-POOL-ERR-PATH-U-PTR @ ;

: GT-POOL-PID@ ( idx -- pid )
   GT-POOL-PID-PTR @ ;

: GT-POOL-REAPER-PID@ ( idx -- pid )
   GT-POOL-REAPER-PID-PTR @ ;

: GT-POOL-OUT-R@ ( idx -- fd )
   GT-POOL-OUT-R-PTR @ ;

: GT-POOL-ERR-R@ ( idx -- fd )
   GT-POOL-ERR-R-PTR @ ;

: GT-POOL-DONE@ ( idx -- n )
   GT-POOL-DONE-PTR @ ;

: GT-POOL-OUT-BUF ( idx -- ptr u8 )
   IDX>N GT-OUT-CAP * GT-POOL-OUT-BUFS + ;

: GT-POOL-ERR-BUF ( idx -- ptr u8 )
   IDX>N GT-ERR-CAP * GT-POOL-ERR-BUFS + ;

: GT-POOL-LABEL-BUF ( idx -- ptr u8 )
   IDX>N GT-FAIL-NAME-CAP * GT-POOL-LABELS + ;

: GT-POOL-LABEL$ ( idx -- ptr u8 n ) {: idx :}
   idx GT-POOL-LABEL-BUF
   idx GT-POOL-LABEL-U-PTR @ ;

: GT-POOL-LABEL! ( ptr u8 n idx -- ) {: a:ptr u idx :}
   u 0 < if E-TBL-FIELD throw then
   u GT-FAIL-NAME-CAP > if E-TBL-FIELD throw then
   a idx GT-POOL-LABEL-BUF u BYTE-COPY
   u idx GT-POOL-LABEL-U-PTR ! ;

: GT-POOL-CLOSE-FD ( ptr fd -- ) {: p:ptr :}
   p @ dup FD>N 0 >= if
      FD>N close
      -1 >FD p !
   else
      drop
   then ;

: GT-POOL-CLOSE-WRITES ( idx -- ) {: idx :}
   idx GT-POOL-OUT-W-PTR GT-POOL-CLOSE-FD
   idx GT-POOL-ERR-W-PTR GT-POOL-CLOSE-FD ;

: GT-POOL-CLOSE-READS ( idx -- ) {: idx :}
   idx GT-POOL-OUT-R-PTR GT-POOL-CLOSE-FD
   idx GT-POOL-ERR-R-PTR GT-POOL-CLOSE-FD ;

: GT-POOL-CLOSE-CAPTURE ( idx -- ) {: idx:idx :}
   idx GT-POOL-OUT-FD-PTR GT-POOL-CLOSE-FD
   idx GT-POOL-ERR-FD-PTR GT-POOL-CLOSE-FD ;

\ Kill+wait the co-located reaper armed for a spawned slot. A spawned child leads
\ its own group and the reaper joined it, so a slot group-kill already SIGKILLed
\ the reaper - this still reaps its zombie, and in the normal-completion path
\ (child exited on its own, reaper still blocking) it SIGKILLs the reaper first.
\ Forked slots never arm a reaper (pid stays -1), so the guard makes this a no-op.
: GT-POOL-KILL-REAPER ( idx -- ) {: idx:idx :}
   idx GT-POOL-REAPER-PID@ PID>N 0 >= if
      idx GT-POOL-REAPER-PID@ SIGKILL PROC-KILL-RAW drop
      idx GT-POOL-REAPER-PID@ PROC-WAIT-STATUS drop
      -1 >PID idx GT-POOL-REAPER-PID-PTR !
   then ;

\ A SLOT ENDS WITH EVERYTHING DESCENDED FROM ITS CHILD. The group kill below
\ reaches the child and what it forked. What it spawned leads groups of its own
\ (docs/process-pty.md) - for a gate row, the engines it builds and runs - and
\ used to outlive the row, reparented to init. lib/process-tree.f stops and
\ lists those first, so they end here with it. A walk that throws is named and
\ the slot is ended the old way all the same: the kill has to go on to the
\ wait and the descriptor closes whatever the process table answered.
: GT-POOL-KILL-TREE ( idx -- ) {: idx:idx :}
   idx GT-POOL-PID@ [: dup PROC-TREE:KILL-TREE ;] catch {: code:n :}
   drop
   code 0= if exit then
   s" test pool: process tree of " type idx GT-POOL-LABEL$ type
   s"  not walked, throw " type code FMT:.INT cr ;

: GT-POOL-KILL-SLOT ( idx -- ) {: idx :}
   idx GT-POOL-PID@ PID>N 0 >= if
      idx GT-POOL-KILL-TREE
      idx GT-POOL-PID@ SIGKILL PROC-FORK:KILL-GROUP drop
      idx GT-POOL-PID@ SIGKILL PROC-KILL-RAW drop
      idx GT-POOL-PID@ PROC-WAIT-STATUS drop
      -1 >PID idx GT-POOL-PID-PTR !
   then
   idx GT-POOL-KILL-REAPER
   idx GT-POOL-CLOSE-WRITES
   idx GT-POOL-CLOSE-READS
   idx GT-POOL-CLOSE-CAPTURE ;

\ Absence is tolerated: a directory is gone already when a child removed it
\ itself, and a forked slot never had one.
: GT-POOL-TREE-REMOVE ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0 <= if exit then
   a u EXISTS? 0= if exit then
   a u REMOVE-TREE ;

\ The child's scratch and its socket directory (GT-POOL-CHILD-TMP!), once
\ nothing is left to write into them.
: GT-POOL-CHILD-TMP-REMOVE ( idx -- ) {: idx:idx :}
   idx GT-POOL-TMP$ GT-POOL-TREE-REMOVE
   idx GT-POOL-SOCK$ GT-POOL-TREE-REMOVE ;

\ THE SOCKET ROOT. Every spawned child's socket directory (GT-POOL-CHILD-SOCK!)
\ is made inside one directory under TMPDIR per pool session, which goes in
\ the cleanup registry once (lib/fs-mutate.f CLEANUP-TREE+). One entry a row
\ would not fit: the registry holds 64 (FS-MUT-CLEANUP-MAX), keeps each entry
\ until a CLEANUP-RUN, and a gate starts hundreds of rows. So a process that
\ ends without retiring or killing its slots - an uncaught throw, a die -
\ still removes every slot's directory with the root, and GT-CLEANUP removes it
\ with the runner's root. A session's root is made at its first spawn, and
\ made again if a GT-CLEANUP removed it. GT-POOL-RESET removes the one before:
\ no live slot's directory is in it by then, and a GT-START since may have
\ reset its entry away. A SIGKILL of this process leaves it (docs/gate.md).
variable GT-POOL-SOCK-ROOT-U
0 GT-POOL-SOCK-ROOT-U !

: GT-POOL-SOCK-ROOT$ ( -- ptr u8 n )
   GT-POOL-SOCK-ROOT-BUF GT-POOL-SOCK-ROOT-U @ ;

: GT-POOL-SOCK-ROOT-REMOVE ( -- )
   GT-POOL-SOCK-ROOT$ GT-POOL-TREE-REMOVE
   0 GT-POOL-SOCK-ROOT-U ! ;

: GT-POOL-SOCK-ROOT-MAKE ( -- )
   GT-POOL-SOCK-ROOT-U @ 0 > if GT-POOL-SOCK-ROOT$ EXISTS? if exit then then
   s" hb-sock" TMPDIR-MKDIR {: a:ptr u:n :}
   a GT-POOL-SOCK-ROOT-BUF u BYTE-COPY
   u GT-POOL-SOCK-ROOT-U !
   GT-POOL-SOCK-ROOT$ CLEANUP-TREE+ ;

\ A ROW'S ROOT THAT CATCHES SIGTERM IS ASKED TO END ITSELF FIRST. The kill below
\ freezes a tree and SIGKILLs every member, and a process SIGKILLed runs none of
\ its own exit path: a server it was running loses what only that path
\ releases. PostgreSQL removes its SysV shared-memory segment there, and
\ nothing else ever does: the key is the data directory's inode, which APFS
\ does not hand out again, and kern.sysv.shmmni caps the segments host-wide
\ (32 here), so every killed pg row took one for good. So a root that catches
\ SIGTERM (PROC-TREE:CATCHES?) is sent it, and the kill waits until that root
\ has exited or GT-POOL-GRACE-MS has passed. In that time the root owes the
\ pool its whole tree: a process it spawned leads a group of its own and goes
\ to init when the root exits, out of the walk's reach. test/db/pg-cluster.f
\ has initdb end its step, or stops its server and ends its case engine's
\ tree, then dies of the signal.
\
\ A root that does not catch SIGTERM is not sent it: the default action would
\ end it at once and leave what it spawned to init before the walk could list
\ it. Those rows are killed as before, and not held for the others' grace.

\ A read of the process table that throws is named, and the root is not asked:
\ the kill has to go on whatever the table answered.
TYPED-VARIABLE GT-POOL-CAUGHT bool
false GT-POOL-CAUGHT !

: GT-POOL-CATCHES-TERM? ( idx -- bool ) {: idx:idx :}
   false GT-POOL-CAUGHT !
   idx GT-POOL-PID@
   [: dup SIGNAL:SIGTERM PROC-TREE:CATCHES? GT-POOL-CAUGHT ! ;] catch {: code:n :}
   drop
   code 0= if GT-POOL-CAUGHT @ exit then
   s" test pool: signals of " type idx GT-POOL-LABEL$ type
   s"  not read, throw " type code FMT:.INT cr
   false ;

: GT-POOL-ASK-END ( idx -- ) {: idx:idx :}
   false idx GT-POOL-ASKED-PTR !
   idx GT-POOL-PID@ PID>N 0 < if exit then
   idx GT-POOL-CATCHES-TERM? 0= if exit then
   idx GT-POOL-PID@ SIGNAL:SIGTERM PROC-KILL-RAW drop
   true idx GT-POOL-ASKED-PTR ! ;

\ Until the asked root has exited or the deadline passes. macOS cannot watch a
\ process that has already exited (test/proc-watch-smoke.f): proc-watch-open
\ answers -1 for one, and there is nothing left to wait for.
: GT-POOL-AWAIT-END ( idx n -- ) {: idx:idx deadline:n :}
   idx GT-POOL-ASKED-PTR @ 0= if exit then
   idx GT-POOL-PID@ PID>N proc-watch-open {: w:n :}
   w 0 < if exit then
   w >FD POLLIN PROC-PFD!
   1 deadline PROC-LEFT-MS MS>N deadline PROC-POLL-RESTART drop
   w close ;

\ A slot killed here is never reaped, so it never retires: its directories go
\ here, after the kill. HB_TMP is under the root a signalled run removes
\ anyway; the socket directory is not. Every root that catches SIGTERM is asked
\ before any slot is killed, so their graces run together and beside the
\ other slots' kills.
: GT-POOL-KILL-ALL ( -- )
   GT-POOL-MAX 0 ?do
      i >IDX GT-POOL-DONE@ 0= if i >IDX GT-POOL-ASK-END then
   loop
   GT-POOL-MAX 0 ?do
      i >IDX GT-POOL-DONE@ 0= i >IDX GT-POOL-ASKED-PTR @ 0= and if
         i >IDX GT-POOL-KILL-SLOT
         i >IDX GT-POOL-CHILD-TMP-REMOVE
      then
   loop
   GT-POOL-GRACE-MS >MS PROC-DEADLINE-AT {: deadline:n :}
   GT-POOL-MAX 0 ?do
      i >IDX GT-POOL-DONE@ 0= i >IDX GT-POOL-ASKED-PTR @ and if
         i >IDX deadline GT-POOL-AWAIT-END
         i >IDX GT-POOL-KILL-SLOT
         i >IDX GT-POOL-CHILD-TMP-REMOVE
      then
   loop ;

: GT-POOL-THROW ( n -- ) {: code :}
   GT-POOL-KILL-ALL
   code throw ;

\ Upgrade the name-builder abort: once GT-POOL-THROW exists, a build-name
\ overflow with live children kills the pool instead of leaking orphans.
: GT-POOL-ABORT-KILL! ( -- ) [: GT-POOL-THROW ;] is GT-POOL-ABORT ;
GT-POOL-ABORT-KILL!

\ ---- a signal that stops the run ---------------------------------------------
\
\ A RUN THAT IS SIGNALLED ENDS ITS OWN CHILDREN AND REMOVES ITS OWN TREE. Under
\ the default action SIGTERM, SIGINT and SIGHUP end the pool where it stands.
\ Its reapers then kill each row's group, the rows' own children live on
\ (lib/process-tree.f), and nothing removes the root: the engine calls the exit
\ hook only on its own way out (src/habu/layout.f EXIT-HOOK-CELL).
\
\ A program that is a pool from its first row to its exit - the gate - calls
\ GT-POOL-CATCH-SIGNALS before it makes its root. No Forth word runs in a
\ handler (lib/signal.f), so a signal is answered at the next GT-POOL-STEP,
\ which is never far: the signal itself interrupts a step's poll, and between
\ two steps the pool only starts a row. The answer is GT-POOL-SIGNAL-DIE,
\ below GT-POOL-FALLBACK-REMOVE.
\
\ IT IS ASKED FOR, NOT INSTALLED BY GT-POOL-RESET. The catch lasts as long as
\ the process and only a step answers it, so a program that ran a pool and
\ went on to other work would never answer SIGTERM again.
\
\ The catch is lib/signal.f CATCH-STOPS, which leaves a signal the process was
\ started with ignored as it was.
TYPED-VARIABLE GT-POOL-CATCHING bool
false GT-POOL-CATCHING !

: GT-POOL-CATCH-SIGNALS ( -- )
   GT-POOL-CATCHING @ if exit then
   SIGNAL:CATCH-STOPS
   true GT-POOL-CATCHING ! ;

\ A forked worker is a new process holding its parent's pipe: left as it is, a
\ signal sent to the worker would be written there and answered by the parent
\ as its own.
: GT-POOL-UNCATCH-SIGNALS ( -- )
   GT-POOL-CATCHING @ 0= if exit then
   SIGNAL:RELEASE
   false GT-POOL-CATCHING ! ;

: GT-POOL-CHECK-LIMIT ( n -- n ) {: n :}
   n 1 < if E-TBL-BOUNDS throw then
   n GT-POOL-MAX > if E-TBL-BOUNDS throw then
   n ;

: GT-POOL-DEFAULT ( -- n )
   HB-TARGET-MACOS? if GT-POOL-MACOS-DEFAULT exit then
   GT-POOL-LINUX-DEFAULT ;

: GT-POOL-SLOTS! ( n -- )
   GT-POOL-CHECK-LIMIT GT-POOL-REQ ! ;

: GT-POOL-LIMIT-SELECT ( -- n )
   GT-POOL-REQ @ dup 0 > if GT-POOL-CHECK-LIMIT exit then
   drop GT-POOL-DEFAULT ;

: GT-POOL-OPEN-PIPE ( ptr fd ptr fd -- ) {: rp:ptr wp:ptr :}
   PIPE-PAIR {: r w :}
   r rp !
   w wp ! ;

: GT-POOL-CLOEXEC@ ( ptr fd -- ) {: p:ptr :}
   p @ FD-CLOEXEC! ;

: GT-POOL-PIPES ( idx -- ) {: idx :}
   idx GT-POOL-OUT-R-PTR idx GT-POOL-OUT-W-PTR GT-POOL-OPEN-PIPE
   idx GT-POOL-ERR-R-PTR idx GT-POOL-ERR-W-PTR GT-POOL-OPEN-PIPE
   idx GT-POOL-OUT-R-PTR GT-POOL-CLOEXEC@
   idx GT-POOL-OUT-W-PTR GT-POOL-CLOEXEC@
   idx GT-POOL-ERR-R-PTR GT-POOL-CLOEXEC@
   idx GT-POOL-ERR-W-PTR GT-POOL-CLOEXEC@ ;

: GT-POOL-RESET-SLOT ( idx -- ) {: idx :}
   -1 >PID idx GT-POOL-PID-PTR !
   -1 >PID idx GT-POOL-REAPER-PID-PTR !
   -1 >FD idx GT-POOL-OUT-R-PTR !
   -1 >FD idx GT-POOL-OUT-W-PTR !
   -1 >FD idx GT-POOL-ERR-R-PTR !
   -1 >FD idx GT-POOL-ERR-W-PTR !
   -1 >FD idx GT-POOL-OUT-FD-PTR !
   -1 >FD idx GT-POOL-ERR-FD-PTR !
   0 idx GT-POOL-OUT-U-PTR !
   0 idx GT-POOL-ERR-U-PTR !
   0 idx GT-POOL-OUT-TOTAL-PTR !
   0 idx GT-POOL-ERR-TOTAL-PTR !
   0 idx GT-POOL-OUT-PATH-U-PTR !
   0 idx GT-POOL-ERR-PATH-U-PTR !
   0 idx GT-POOL-TMP-PATH-U-PTR !
   0 idx GT-POOL-SOCK-PATH-U-PTR !
   false idx GT-POOL-ASKED-PTR !
   0 idx GT-POOL-SEQ-PTR !
   0 idx GT-POOL-WAITS-PTR !
   0 idx GT-POOL-SAT-LIVE-PTR !
   0 0= idx GT-POOL-EXITED-PTR !
   0 0= 0= idx GT-POOL-TIMED-OUT-PTR !
   0 idx GT-POOL-CODE-PTR !
   -1 idx GT-POOL-DONE-PTR ! ;

: GT-POOL-DEATH-RD@ ( -- fd )
   GT-POOL-DEATH-RD @ >FD ;

: GT-POOL-DEATH-WR@ ( -- fd )
   GT-POOL-DEATH-WR @ >FD ;

\ Create the pool-parent death pipe once per pool. The parent keeps WR; every
\ forked worker's reaper keeps RD.
: GT-POOL-DEATH-MAKE ( -- )
   GT-POOL-DEATH-MADE @ 0 <> if exit then
   PROC-FORK:DEATH-PIPE {: rd:fd wr:fd :}
   rd FD>N GT-POOL-DEATH-RD !
   wr FD>N GT-POOL-DEATH-WR !
   -1 GT-POOL-DEATH-MADE ! ;

: GT-POOL-RESET ( -- )
   GT-POOL-SOCK-ROOT-REMOVE
   GT-POOL-ALLOC-BUFFERS
   GT-POOL-LIMIT-SELECT GT-POOL-LIMIT !
   0 GT-POOL-LIVE !
   GT-POOL-DEATH-MAKE
   0 begin dup GT-POOL-MAX < while
      dup >IDX GT-POOL-RESET-SLOT
      1+
   repeat drop ;

: GT-POOL-FREE? ( idx -- bool )
   GT-POOL-DONE@ 0= 0= ;

: GT-POOL-FIND-FREE ( -- idx )
   0 begin dup GT-POOL-LIMIT @ < while
      dup >IDX GT-POOL-FREE? if >IDX exit then
      1+
   repeat drop
   E-TBL-BOUNDS throw ;

\ A retired slot is reused by the next start, so the capture sequence number,
\ unique per start, is the one handle a caller can keep on a row it started.
: GT-POOL-SEQ-LIVE? ( n -- bool ) {: seq:n :}
   0 begin dup GT-POOL-LIMIT @ < while
      dup >IDX GT-POOL-DONE@ 0=
      over >IDX GT-POOL-SEQ-PTR @ seq = and if drop true exit then
      1+
   repeat drop
   false ;

: GT-POOL-ELAPSED-MS ( idx -- n ) {: idx :}
   mono-ns idx GT-POOL-START-PTR @ - PROC-NS-PER-MS / ;

: GT-POOL-PASS-LINE ( idx -- ) {: idx :}
   s" PASS: " type
   idx GT-POOL-LABEL$ type
   s"  (" type
   idx GT-POOL-ELAPSED-MS GT-U-TYPE
   s" ms)" type cr ;

: GT-POOL-WAIT-DUE? ( idx -- bool ) {: idx :}
   mono-ns idx GT-POOL-LAST-PTR @ - PROC-NS-PER-MS / GT-HEARTBEAT-MS >= ;

: GT-POOL-WAIT-LINE ( idx -- ) {: idx :}
   idx GT-POOL-WAIT-DUE? if
      mono-ns idx GT-POOL-LAST-PTR !
      idx GT-POOL-WAITS-PTR @ 1+ idx GT-POOL-WAITS-PTR !
      s" WAIT: " type
      idx GT-POOL-LABEL$ type
      s"  (" type
      idx GT-POOL-ELAPSED-MS GT-U-TYPE
      s" ms)" type cr
   then ;

: GT-POOL-LINE$ ( ptr u8 n ptr u8 n -- ) {: name:ptr nameu:n val:ptr valu:n :}
   name nameu type s" : " type val valu type cr ;

: GT-POOL-N-TYPE ( n -- ) {: val:n :}
   val 0 < if $2D emit val negate GT-U-TYPE exit then
   val GT-U-TYPE ;

: GT-POOL-LINE-N ( ptr u8 n n -- ) {: name:ptr nameu:n val:n :}
   name nameu type s" : " type val GT-POOL-N-TYPE cr ;

: GT-POOL-LINE-FD ( ptr u8 n fd -- ) {: name:ptr nameu:n val:fd :}
   name nameu type s" : " type val FD>N GT-POOL-N-TYPE cr ;

: GT-POOL-SPAWN-ERRNO ( pid -- n )
   PID>N negate ;

: GT-POOL-SPAWN-FAIL. ( idx ptr u8 n pid -- ) {: idx:idx path:ptr pathu:n pid:pid :}
   s" FAIL: test pool spawn" type cr
   s" test" idx GT-POOL-LABEL$ GT-POOL-LINE$
   s" path" path pathu GT-POOL-LINE$
   s" raw" pid PID>N GT-POOL-LINE-N
   s" errno" pid GT-POOL-SPAWN-ERRNO GT-POOL-LINE-N
   s" argv-count" PROC-ARGV-N @ COUNT>N GT-POOL-LINE-N
   s" env-count" PROC-ENV-N @ COUNT>N GT-POOL-LINE-N
   s" pool-live" GT-POOL-LIVE @ GT-POOL-LINE-N
   s" pool-limit" GT-POOL-LIMIT @ GT-POOL-LINE-N
   s" stdout-fd" idx GT-POOL-OUT-W-PTR @ GT-POOL-LINE-FD
   s" stderr-fd" idx GT-POOL-ERR-W-PTR @ GT-POOL-LINE-FD ;

\ Arm a co-located reaper for the just-spawned child. The spawn primitive makes
\ every child its own process-group leader before exec, so timeout and
\ parent-death cleanup can signal the whole subtree without a parent-side
\ setpgid race.
\
\ Fork a reaper that joins the child's group and watches the pool-death read end,
\ then track its pid so GT-POOL-REAP/KILL-SLOT reap it. Stack-preserving under
\ catch: the slot index is the quotation's window.
: GT-POOL-SPAWN-REAPER! ( idx -- idx ) {: idx:idx :}
   GT-POOL-DEATH-RD@ idx GT-POOL-PID@ PROC-FORK:SPAWN-REAPER
   idx GT-POOL-REAPER-PID-PTR !
   idx ;

\ Reaper fork failure (PROC-FORK:SPAWN-REAPER throws) is also fail-closed: the
\ pool kills every slot, this child included; otherwise a spawned subtree could
\ survive a killed pool.
: GT-POOL-ARM-SPAWN-REAPER ( idx -- ) {: idx:idx :}
   idx [: GT-POOL-SPAWN-REAPER! ;] catch {: code:n :}
   drop
   code 0<> if code GT-POOL-THROW then ;

: GT-POOL-SPAWN-FD ( idx ptr u8 n fd -- ) {: idx:idx path:ptr pathu:n stdin:fd :}
   path pathu >LEN PROC-ARGV-CHECK-PATH
   path pathu >LEN PROC-ARGV-PREPARE {: pathz:ptr argv:ptr :}
   PROC-ENV-PREPARE {: envp:ptr :}
   pathz argv envp stdin idx GT-POOL-OUT-W-PTR @ idx GT-POOL-ERR-W-PTR @
   PROC-SPAWN-ARGV-ENV-RAW {: pid:pid :}
   pid PID>N 0 < if
      idx path pathu pid GT-POOL-SPAWN-FAIL.
      PROC-ARGV-ENV-RESET
      E-PROC-SPAWN GT-POOL-THROW
   then
   PROC-ARGV-ENV-RESET
   pid idx GT-POOL-PID-PTR !
   idx GT-POOL-CLOSE-WRITES
   idx GT-POOL-ARM-SPAWN-REAPER ;

: GT-POOL-SPAWN ( idx ptr u8 n -- )
   -1 >FD GT-POOL-SPAWN-FD ;

: GT-POOL-FORK-EXIT ( n -- )
   s" " rot die ;

\ Exit with a fixed nonzero code: the kernel keeps only the low 8 exit bits,
\ so a raw throw code that is a multiple of 256 would read as success.
: GT-POOL-FORK-THROW ( n -- ) {: rc:n :}
   rc WHY-THREW-DUMP                             \ self-identify an opaque capacity throw before dying
   GT-POOL-NAME-RESET
   s" fork worker throw rc " GT-POOL-NAME+
   rc 0 < if
      s" -" GT-POOL-NAME+
      rc negate GT-POOL-NUM$ GT-POOL-NAME+
   else
      rc GT-POOL-NUM$ GT-POOL-NAME+
   then
   GT-POOL-NAME$ 1 die ;

: GT-POOL-FORK-SETUP-FAIL ( -- )
   127 GT-POOL-FORK-EXIT ;

: GT-POOL-DUP2! ( fd n -- ) {: fd:fd dst:n :}
   fd FD>N dst dup2 dup 0 < if drop GT-POOL-FORK-SETUP-FAIL then drop ;

\ Become our own process-group leader right after fork so a timeout kill
\ (GT-POOL-KILL-SLOT signalling -pid) reaches this worker's whole subtree --
\ nested pools and spawned hb -- instead of orphaning grandchildren.
: GT-POOL-SETPGID-SELF ( -- )
   0 >PID 0 >PID PROC-FORK:SET-PGID RC>N 0 < if GT-POOL-FORK-SETUP-FAIL then ;

\ Worker side: open a worker-alive pipe, arm the parent-death reaper watching
\ both the pool parent's death pipe (RD) and this worker-alive pipe, then drop
\ every copy the worker no longer needs: the inherited parent death-pipe RD/WR.
\ The worker keeps the worker-alive WR open (an untracked fd, close-on-exec) so
\ it closes exactly at this worker's exit and the reaper self-exits with no
\ orphan -- and it keeps the worker-alive RD published as PROC-FORK:REAP-WATCH-FD,
\ so every capture child this worker spawns (PROC-RUN-CAPTURE family) gets its
\ own co-located reaper watching this worker's life. Extra RD copies never
\ delay the EOF (only write ends count) and both ends are close-on-exec. The
\ made-flag is cleared so a nested pool this worker later drives builds its own
\ death pipe. The reaper is a reparented grandchild (see PROC-FORK:FORK-REAPER),
\ never a child of this worker, so the worker body's wait(-1) still sees no
\ children. A refused reaper fork ends the worker through GT-POOL-FORK-THROW.
: GT-POOL-ARM-REAPER ( -- )
   PROC-FORK:DEATH-PIPE {: wa-rd:fd wa-wr:fd :}
   GT-POOL-DEATH-RD@ wa-rd PROC-FORK:FORK-REAPER
   GT-POOL-DEATH-RD@ FD>N close
   GT-POOL-DEATH-WR@ FD>N close
   wa-rd FD>N PROC-FORK:REAP-WATCH-FD !
   0 GT-POOL-DEATH-MADE ! ;

: GT-POOL-FORK-CHILD ( idx [ -- ] -- ) {: idx:idx q :}
   GT-POOL-UNCATCH-SIGNALS
   idx GT-POOL-CLOSE-READS
   idx GT-POOL-OUT-W-PTR @ 1 GT-POOL-DUP2!
   idx GT-POOL-ERR-W-PTR @ 2 GT-POOL-DUP2!
   idx GT-POOL-CLOSE-WRITES
   idx GT-POOL-CLOSE-CAPTURE
   GT-POOL-SETPGID-SELF
   GT-POOL-RED-RESET
   [: GT-POOL-ARM-REAPER ;] catch {: arm:n :}
   arm 0<> if arm GT-POOL-FORK-THROW then
   q catch {: rc:n :}
   rc 0= if 0 GT-POOL-FORK-EXIT then
   rc GT-POOL-FORK-THROW ;

: GT-POOL-FORK ( idx [ -- ] -- ) {: idx:idx q :}
   PROC-FORK:RAW {: pid:pid :}
   pid PID>N 0 < if E-PROC-SPAWN GT-POOL-THROW then
   pid PID>N 0= if idx q GT-POOL-FORK-CHILD then
   pid idx GT-POOL-PID-PTR !
   idx GT-POOL-CLOSE-WRITES ;

: GT-POOL-STREAM-FILE-FD-PTR ( idx n -- ptr fd ) {: idx:idx stream:n :}
   stream 0= if idx GT-POOL-OUT-FD-PTR exit then
   idx GT-POOL-ERR-FD-PTR ;

: GT-POOL-STREAM-TOTAL-PTR ( idx n -- ptr n ) {: idx:idx stream:n :}
   stream 0= if idx GT-POOL-OUT-TOTAL-PTR exit then
   idx GT-POOL-ERR-TOTAL-PTR ;

: GT-POOL-STREAM-FILE$ ( idx n -- ptr u8 n ) {: idx:idx stream:n :}
   stream 0= if idx GT-POOL-OUT-FILE$ exit then
   idx GT-POOL-ERR-FILE$ ;

: GT-POOL-STREAM-SUFFIX$ ( n -- ptr u8 n ) {: stream:n :}
   stream 0= if s" -out.log" exit then
   s" -err.log" ;

: GT-POOL-STREAM-PATH-BUF ( idx n -- ptr u8 ) {: idx:idx stream:n :}
   stream 0= if idx GT-POOL-OUT-PATH-BUF exit then
   idx GT-POOL-ERR-PATH-BUF ;

: GT-POOL-STREAM-PATH-U-PTR ( idx n -- ptr n ) {: idx:idx stream:n :}
   stream 0= if idx GT-POOL-OUT-PATH-U-PTR exit then
   idx GT-POOL-ERR-PATH-U-PTR ;

: GT-POOL-FALLBACK-BASE$ ( -- ptr u8 n )
   s" HB_TMP" GETENV dup 0 > if exit then
   2drop
   s" TMPDIR" GETENV dup 0 > if exit then
   2drop s" /tmp" ;

: GT-POOL-FALLBACK-NAME$ ( -- ptr u8 n )
   GT-POOL-NAME-RESET
   s" hb-pool-" GT-POOL-NAME+
   mono-ns GT-POOL-NUM$ GT-POOL-NAME+
   GT-POOL-NAME$ ;

\ Suite-less pool users get one process-unique capture directory so parallel
\ runs sharing HB_TMP/TMPDIR cannot clobber each other's capture files.
: GT-POOL-FALLBACK-ROOT$ ( -- ptr u8 n )
   GT-POOL-FALLBACK-U @ 0 > if
      GT-POOL-FALLBACK-BUF GT-POOL-FALLBACK-U @ exit
   then
   GT-POOL-FALLBACK-BASE$ GT-POOL-FALLBACK-NAME$
   GT-POOL-FALLBACK-BUF JOIN-PATH GT-POOL-FALLBACK-U !
   GT-POOL-FALLBACK-BUF GT-POOL-FALLBACK-U @ MAKE-DIRS
   GT-POOL-FALLBACK-BUF GT-POOL-FALLBACK-U @ ;

: GT-POOL-CAPTURE-ROOT$ ( -- ptr u8 n )
   GT-ROOT-U @ 0 > if GT-ROOT exit then
   GT-POOL-FALLBACK-ROOT$ ;

\ The fallback root belongs to the process, not to one pool: the memo above
\ builds it once and GT-POOL-RESET keeps it, so every pool a suite-less user
\ runs captures into the same directory until a drain removes it.
\ A suite's root is the runner's (GT-START registers it with CLEANUP-TREE+),
\ so this exits when there is one. Absence is tolerated the way
\ GT-POOL-CHILD-TMP-REMOVE tolerates it. Clearing the memo is what lets a
\ later pool in the same process capture again: it rebuilds a fresh root with
\ MAKE-DIRS, where a stale memo would open the captures under a removed
\ directory (E-FS-OPEN).
: GT-POOL-FALLBACK-REMOVE ( -- )
   GT-ROOT-U @ 0 > if exit then
   GT-POOL-FALLBACK-U @ 0 <= if exit then
   GT-POOL-FALLBACK-BUF GT-POOL-FALLBACK-U @ EXISTS? if
      GT-POOL-FALLBACK-BUF GT-POOL-FALLBACK-U @ REMOVE-TREE
   then
   0 GT-POOL-FALLBACK-U ! ;

: GT-POOL-CAPTURE-PATH! ( idx n -- ) {: idx:idx stream:n :}
   GT-POOL-CAPTURE-ROOT$
   idx GT-POOL-SEQ-PTR @ stream GT-POOL-STREAM-SUFFIX$ GT-POOL-CAPTURE-NAME
   idx stream GT-POOL-STREAM-PATH-BUF JOIN-PATH
   idx stream GT-POOL-STREAM-PATH-U-PTR ! ;

: GT-POOL-CAPTURE-OPEN-FD ( ptr u8 n -- fd ) {: pa:ptr pu:n :}
   pa pu FS-PATHZ FS-O-WRONLY FS-O-CREAT or FS-O-TRUNC or FS-MODE-0644 open {: raw:n :}
   raw 0 < if E-FS-OPEN GT-POOL-THROW then
   raw >FD ;

: GT-POOL-CAPTURE-FD! ( idx n -- ) {: idx:idx stream:n :}
   idx stream GT-POOL-STREAM-FILE$ GT-POOL-CAPTURE-OPEN-FD {: fd:fd :}
   fd FD-CLOEXEC!
   fd idx stream GT-POOL-STREAM-FILE-FD-PTR ! ;

: GT-POOL-CAPTURE-START ( idx -- ) {: idx:idx :}
   GT-POOL-SEQ @ 1+ GT-POOL-SEQ !
   GT-POOL-SEQ @ idx GT-POOL-SEQ-PTR !
   idx 0 GT-POOL-CAPTURE-PATH!
   idx 1 GT-POOL-CAPTURE-PATH!
   idx 0 GT-POOL-CAPTURE-FD!
   idx 1 GT-POOL-CAPTURE-FD! ;

\ THE POOL OWNS ITS SPAWNED CHILDREN'S SCRATCH. Each gets a directory of its
\ own beside its capture files, named HB_TMP in its environment, and the PARENT
\ removes it when the slot retires. A child the pool killed - a timeout, a
\ group kill - never reaches its own CLEANUP-RUN, so its temp trees survived it
\ and nothing ever removed them; under HB_TMP they now go with the directory,
\ and so does anything its own children left. PROC-ENV-SET, not PROC-ENV+,
\ because the caller has already inherited its environment into the table and
\ an inherited HB_TMP has to be replaced rather than shadowed.
\
\ Forked slots never call this: a forked worker shares the parent's image, gets
\ no environment of its own, and must not be handed a directory whose removal
\ would touch the parent's tree.
\
\ An HB_TMP row already in the caller's table is one of two things: the copy
\ PROC-ENV-INHERIT-MISSING took from this process's own environment, which is
\ the pool's to replace, or a value the caller chose for its child - a scratch
\ root the caller owns and reaps (test/nf-path-test.f hands each build a root
\ of a chosen length and byte content) - which stays, and then the slot gets no
\ directory at all.
: GT-POOL-ENV-HB-TMP-OWN? ( -- bool )
   s" HB_TMP=" {: k:ptr ku:n :}
   s" HB_TMP" >LEN PROC-ENV-NAME-IDX {: i:n :}
   i 0 < if false exit then
   i >IDX PROC-ENV-SLOT @ {: z:ptr :}
   z ku BYTE+ z ZLEN ku -
   s" HB_TMP" GETENV STR= 0= ;

\ EACH SPAWNED CHILD ALSO GETS A SHORT DIRECTORY FOR ITS SOCKETS. A
\ Unix-domain socket's path has to fit sun_path - 104 bytes on macOS, 108 on
\ Linux - and a gate row's HB_TMP is about 100 bytes long already, longer
\ again for each pool that runs under a pool. So the pool also makes each
\ spawned child a directory named by its capture sequence number in the
\ session's socket root under TMPDIR, which no pool changes and which is as
\ short at every level, and names it HB_SOCK_TMP in the child's environment
\ (test/db/pg-cluster.f puts its server's socket there). It goes with the
\ scratch. Every spawned slot gets one, whoever owns its HB_TMP: the value
\ inherited in its place would name this process's own, which a sibling may
\ be using. Under macOS's default TMPDIR a PostgreSQL socket path there is
\ about 90 bytes.
: GT-POOL-CHILD-SOCK! ( idx -- ) {: idx:idx :}
   GT-POOL-SOCK-ROOT-MAKE
   GT-POOL-SOCK-ROOT$ idx GT-POOL-SEQ-PTR @ GT-POOL-NUM$
   idx GT-POOL-SOCK-PATH-BUF JOIN-PATH idx GT-POOL-SOCK-PATH-U-PTR !
   idx GT-POOL-SOCK$ MAKE-DIRS
   s" HB_SOCK_TMP" >LEN idx GT-POOL-SOCK$ >LEN PROC-ENV-SET ;

: GT-POOL-CHILD-TMP! ( idx -- ) {: idx:idx :}
   idx GT-POOL-CHILD-SOCK!
   GT-POOL-ENV-HB-TMP-OWN? if 0 idx GT-POOL-TMP-PATH-U-PTR ! exit then
   GT-POOL-CAPTURE-ROOT$
   idx GT-POOL-SEQ-PTR @ s" -tmp" GT-POOL-CAPTURE-NAME
   idx GT-POOL-TMP-PATH-BUF JOIN-PATH idx GT-POOL-TMP-PATH-U-PTR !
   idx GT-POOL-TMP$ MAKE-DIRS
   s" HB_TMP" >LEN idx GT-POOL-TMP$ >LEN PROC-ENV-SET ;

\ The one place a slot leaves the live set, whatever its outcome was: a reap
\ (exited or signaled) or a timeout the pool killed. The child's scratch goes
\ here, after the kill, so nothing is still writing into it.
: GT-POOL-RETIRE-SLOT ( idx -- ) {: idx:idx :}
   idx GT-POOL-CHILD-TMP-REMOVE
   1 idx GT-POOL-DONE-PTR !
   GT-POOL-LIVE @ 1- GT-POOL-LIVE ! ;

: GT-POOL-RED-CHECK ( n -- n ) {: i:n :}
   i 0 < if E-TBL-BOUNDS throw then
   i GT-POOL-RED-MAX >= if E-TBL-BOUNDS throw then
   i ;

: GT-POOL-RED-LABEL-BUF ( n -- ptr u8 )
   GT-POOL-RED-CHECK GT-FAIL-NAME-CAP * GT-POOL-RED-LABELS + ;

: GT-POOL-RED-LABEL-U-PTR ( n -- ptr n )
   GT-POOL-RED-CHECK cells GT-POOL-RED-LABEL-US + ;

: GT-POOL-RED-EXITED-PTR ( n -- ptr bool )
   GT-POOL-RED-CHECK cells GT-POOL-RED-EXITEDS + ;

: GT-POOL-RED-TIMED-OUT-PTR ( n -- ptr bool )
   GT-POOL-RED-CHECK cells GT-POOL-RED-TIMED-OUTS + ;

: GT-POOL-RED-CODE-PTR ( n -- ptr n )
   GT-POOL-RED-CHECK cells GT-POOL-RED-CODES + ;

: GT-POOL-RED-SEQ-PTR ( n -- ptr n )
   GT-POOL-RED-CHECK cells GT-POOL-RED-SEQS + ;





: GT-POOL-RED-WAITS-PTR ( n -- ptr n )
   GT-POOL-RED-CHECK cells GT-POOL-RED-WAITS + ;

: GT-POOL-RED-SAT-LIVE-PTR ( n -- ptr n )
   GT-POOL-RED-CHECK cells GT-POOL-RED-SAT-LIVES + ;

: GT-POOL-RED-SAT-LIMIT-PTR ( n -- ptr n )
   GT-POOL-RED-CHECK cells GT-POOL-RED-SAT-LIMITS + ;

: GT-POOL-RED-SAT-MS-PTR ( n -- ptr n )
   GT-POOL-RED-CHECK cells GT-POOL-RED-SAT-MSS + ;

: GT-POOL-RED-LABEL$ ( n -- ptr u8 n ) {: i:n :}
   i GT-POOL-RED-LABEL-BUF i GT-POOL-RED-LABEL-U-PTR @ ;

\ A red row keeps its capture sequence number; the report rebuilds the two
\ capture paths from it the way GT-POOL-CAPTURE-PATH! built them, which holds
\ while the capture root (GT-START) stays the same between the capture and the
\ report, as it does for every pool session in the tree.
: GT-POOL-RED-STREAM$ ( n n -- ptr u8 n ) {: i:n stream:n :}
   GT-POOL-CAPTURE-ROOT$
   i GT-POOL-RED-SEQ-PTR @ stream GT-POOL-STREAM-SUFFIX$ GT-POOL-CAPTURE-NAME
   GT-POOL-RED-PATH-BUF JOIN-PATH GT-POOL-RED-PATH-U !
   GT-POOL-RED-PATH-BUF GT-POOL-RED-PATH-U @ ;

: GT-POOL-RED-OUT$ ( n -- ptr u8 n )
   0 GT-POOL-RED-STREAM$ ;

: GT-POOL-RED-ERR$ ( n -- ptr u8 n )
   1 GT-POOL-RED-STREAM$ ;



: GT-POOL-RED-COPY$ ( ptr u8 n ptr u8 ptr n n -- ) {: a:ptr u:n dst:ptr up:ptr cap:n :}
   u 0 < if E-TBL-FIELD throw then
   u cap > if E-TBL-FIELD throw then
   a dst u BYTE-COPY
   u up ! ;

: GT-POOL-RED-STORE ( n idx -- ) {: i:n idx:idx :}
   idx GT-POOL-LABEL$ i GT-POOL-RED-LABEL-BUF i GT-POOL-RED-LABEL-U-PTR GT-FAIL-NAME-CAP GT-POOL-RED-COPY$
   idx GT-POOL-EXITED-PTR @ i GT-POOL-RED-EXITED-PTR !
   idx GT-POOL-TIMED-OUT-PTR @ i GT-POOL-RED-TIMED-OUT-PTR !
   idx GT-POOL-CODE-PTR @ i GT-POOL-RED-CODE-PTR !
   idx GT-POOL-WAITS-PTR @ i GT-POOL-RED-WAITS-PTR !
   idx GT-POOL-SAT-LIVE-PTR @ i GT-POOL-RED-SAT-LIVE-PTR !
   GT-POOL-LIMIT @ i GT-POOL-RED-SAT-LIMIT-PTR !
   idx GT-POOL-ELAPSED-MS i GT-POOL-RED-SAT-MS-PTR !
   idx GT-POOL-SEQ-PTR @ i GT-POOL-RED-SEQ-PTR ! ;

: GT-POOL-RED+ ( idx -- ) {: idx:idx :}
   GT-POOL-RED-N @ GT-POOL-RED-MAX < if
      GT-POOL-RED-N @ idx GT-POOL-RED-STORE
   then
   GT-POOL-RED-N @ 1+ GT-POOL-RED-N ! ;

: GT-POOL-RED-DETAILED ( -- n )
   GT-POOL-RED# dup GT-POOL-RED-MAX > if drop GT-POOL-RED-MAX then ;

\ The red record of the row whose capture seq is n, or -1 when that row is not
\ among the detailed reds (see GT-POOL-SEQ-LIVE? for why the seq is the handle).
: GT-POOL-RED-FIND-SEQ ( n -- n ) {: seq:n :}
   0 begin dup GT-POOL-RED-DETAILED < while
      dup GT-POOL-RED-SEQ-PTR @ seq = if exit then
      1+
   repeat drop
   -1 ;

\ Saturation suffix, appended only for a pool-timeout (TIMEOUT-UNDER-LOAD) red:
\ the live/limit depth and WAIT-heartbeat count that witness the contention, plus
\ how long the killed slot ran. Non-timeout reds are byte-identical to before.
: GT-POOL-RED-SAT-LINE ( n -- ) {: i:n :}
   i GT-POOL-RED-TIMED-OUT-PTR @ 0= if exit then
   s"  sat=" type i GT-POOL-RED-SAT-LIVE-PTR @ GT-POOL-N-TYPE
   $2F emit i GT-POOL-RED-SAT-LIMIT-PTR @ GT-POOL-N-TYPE
   s"  waits=" type i GT-POOL-RED-WAITS-PTR @ GT-POOL-N-TYPE
   s"  ran=" type i GT-POOL-RED-SAT-MS-PTR @ GT-POOL-N-TYPE
   s" ms" type ;

: GT-POOL-RED-LINE ( n -- ) {: i:n :}
   s" RED: " type i GT-POOL-RED-LABEL$ type
   s"  kind=" type i GT-POOL-RED-EXITED-PTR @ i GT-POOL-RED-TIMED-OUT-PTR @ GT-POOL-KIND-NAME.
   s"  code=" type i GT-POOL-RED-CODE-PTR @ GT-POOL-N-TYPE
   s"  out=" type i GT-POOL-RED-OUT$ type
   s"  err=" type i GT-POOL-RED-ERR$ type
   i GT-POOL-RED-SAT-LINE cr ;

: GT-POOL-RED-OVERFLOW-LINE ( -- )
   GT-POOL-RED# GT-POOL-RED-MAX > if
      s" RED: +" type GT-POOL-RED# GT-POOL-RED-MAX - GT-POOL-N-TYPE
      s"  more failed tests (details not recorded)" type cr
   then ;

: GT-POOL-RED-REPORT ( -- )
   GT-POOL-RED# 0= if exit then
   s" red tests: " type GT-POOL-RED# GT-POOL-N-TYPE cr
   0 begin dup GT-POOL-RED-DETAILED < while
      dup GT-POOL-RED-LINE
      1+
   repeat drop
   GT-POOL-RED-OVERFLOW-LINE ;

\ The slot bookkeeping every start shares: pipes, capture files, label and
\ clocks. The caller then spawns or forks into the slot and counts it live.
: GT-POOL-OPEN-SLOT ( ptr u8 n n idx -- ) {: label:ptr labelu timeout idx :}
   idx GT-POOL-DONE@ 0= if
      s" test pool: fixed slot already active" type cr
      E-TBL-FIELD GT-POOL-THROW
   then
   idx GT-POOL-RESET-SLOT
   0 idx GT-POOL-DONE-PTR !
   idx GT-POOL-PIPES
   idx GT-POOL-CAPTURE-START
   label labelu idx GT-POOL-LABEL!
   mono-ns idx GT-POOL-START-PTR !
   idx GT-POOL-START-PTR @ idx GT-POOL-LAST-PTR !
   timeout idx GT-POOL-TIMEOUT-PTR ! ;

: GT-POOL-START-SLOT ( ptr u8 n ptr u8 n n idx -- ) {: path:ptr pathu label:ptr labelu timeout idx :}
   label labelu timeout idx GT-POOL-OPEN-SLOT
   idx GT-POOL-CHILD-TMP!
   idx path pathu GT-POOL-SPAWN
   GT-POOL-LIVE @ 1+ GT-POOL-LIVE ! ;

\ A stdin-fed slot: the child reads its source from a pipe. The bytes go down
\ in one write before the pool polls, bounded by PIPE_BUF so the write is
\ atomic and cannot block on an empty pipe; the write end closes at once so
\ the child sees end of input. A child that exited before reading makes the
\ write fail with EPIPE (SIGPIPE is disarmed on the descriptor first); its
\ exit code, reaped like any slot's, makes the row red.
4096 constant GT-POOL-STDIN-CAP

: GT-POOL-FEED-STDIN ( fd ptr u8 n -- ) {: w:fd bytes:ptr byten:n :}
   byten GT-POOL-STDIN-CAP > if E-TBL-BOUNDS GT-POOL-THROW then
   byten 0 > if
      w FD>N bytes byten write {: wrote:n :}
      wrote 0 < 0= wrote byten <> and if E-PROC-OUTPUT GT-POOL-THROW then
   then
   w FD>N close ;

: GT-POOL-START-STDIN-SLOT ( ptr u8 n ptr u8 n ptr u8 n n idx -- )
   {: path:ptr pathu label:ptr labelu bytes:ptr byten timeout idx :}
   label labelu timeout idx GT-POOL-OPEN-SLOT
   idx GT-POOL-CHILD-TMP!
   PIPE-PAIR {: r w :}
   r FD-CLOEXEC!
   w FD-CLOEXEC!
   w FD-NOSIGPIPE!
   idx path pathu r GT-POOL-SPAWN-FD
   r FD>N close
   w bytes byten GT-POOL-FEED-STDIN
   GT-POOL-LIVE @ 1+ GT-POOL-LIVE ! ;

: GT-POOL-START-FORK-SLOT ( ptr u8 n n idx [ -- ] -- ) {: label:ptr labelu:n timeout:n idx:idx q :}
   label labelu timeout idx GT-POOL-OPEN-SLOT
   idx q GT-POOL-FORK
   GT-POOL-LIVE @ 1+ GT-POOL-LIVE ! ;

: GT-POOL-PFD-SLOT ( idx -- ptr n ) {: idx :}
   idx IDX>N 0 < if E-TBL-BOUNDS throw then
   idx IDX>N GT-POOL-MAX GT-POOL-FDS * >= if E-TBL-BOUNDS throw then
   idx IDX>N GT-PFD-SZ * GT-POOL-PFDS + ;

: GT-POOL-PFD! ( fd n idx -- ) {: fd events idx :}
   events 32 lshift fd FD>N $FFFFFFFF and or idx GT-POOL-PFD-SLOT ! ;

: GT-POOL-PFD-REVENTS ( idx -- n )
   GT-POOL-PFD-SLOT @ 48 rshift $FFFF and ;

: GT-POOL-OUT-SLOT ( idx -- idx )
   IDX>N GT-POOL-FDS * >IDX ;

: GT-POOL-ERR-SLOT ( idx -- idx )
   IDX>N GT-POOL-FDS * 1+ >IDX ;

: GT-POOL-POLL-SLOT ( idx -- ) {: idx :}
   idx GT-POOL-DONE@ 0= 0= if
      -1 >FD 0 idx GT-POOL-OUT-SLOT GT-POOL-PFD!
      -1 >FD 0 idx GT-POOL-ERR-SLOT GT-POOL-PFD!
      exit
   then
   idx GT-POOL-OUT-R@ POLLIN idx GT-POOL-OUT-SLOT GT-POOL-PFD!
   idx GT-POOL-ERR-R@ POLLIN idx GT-POOL-ERR-SLOT GT-POOL-PFD! ;

: GT-POOL-POLL-BUILD ( -- )
   0 begin dup GT-POOL-MAX < while
      dup >IDX GT-POOL-POLL-SLOT
      1+
   repeat drop ;

\ poll(2) is the one wait SA_RESTART never restarts, so a caught signal ends
\ it with -EINTR: nothing failed and no descriptor is ready. The build above
\ rewrote every slot, so no stale readiness is read either.
: GT-POOL-POLL ( -- n )
   GT-POOL-POLL-BUILD
   GT-POOL-PFDS GT-POOL-MAX GT-POOL-FDS * GT-POOL-POLL-MS poll {: rc :}
   rc EINTR# negate = if 0 exit then
   rc 0 < if E-PROC-OUTPUT GT-POOL-THROW then
   rc ;

: GT-POOL-STREAM-FD-PTR ( idx n -- ptr fd ) {: idx stream :}
   stream 0= if idx GT-POOL-OUT-R-PTR exit then
   idx GT-POOL-ERR-R-PTR ;

: GT-POOL-STREAM-U-PTR ( idx n -- ptr n ) {: idx stream :}
   stream 0= if idx GT-POOL-OUT-U-PTR exit then
   idx GT-POOL-ERR-U-PTR ;

: GT-POOL-STREAM-BUF ( idx n -- ptr u8 ) {: idx stream :}
   stream 0= if idx GT-POOL-OUT-BUF exit then
   idx GT-POOL-ERR-BUF ;

: GT-POOL-STREAM-CAP ( n -- n ) {: stream :}
   stream 0= if GT-OUT-CAP exit then
   GT-ERR-CAP ;

: GT-POOL-FILE-WRITE ( fd ptr u8 n -- ) {: fd:fd a:ptr u:n :}
   0 GT-POOL-WR-OFF !
   begin GT-POOL-WR-OFF @ u < while
      fd FD>N a GT-POOL-WR-OFF @ + u GT-POOL-WR-OFF @ - write GT-POOL-WR !
      GT-POOL-WR @ 0 <= if E-FS-IO GT-POOL-THROW then
      GT-POOL-WR @ u GT-POOL-WR-OFF @ - > if E-FS-IO GT-POOL-THROW then
      GT-POOL-WR-OFF @ GT-POOL-WR @ + GT-POOL-WR-OFF !
   repeat ;

: GT-POOL-TAIL+ ( idx n ptr u8 n -- ) {: idx:idx stream:n a:ptr u:n :}
   idx stream GT-POOL-STREAM-U-PTR {: up:ptr :}
   idx stream GT-POOL-STREAM-BUF {: buf:ptr :}
   stream GT-POOL-STREAM-CAP {: cap:n :}
   u cap >= if
      a u cap - + buf cap BYTE-COPY
      cap up !
      exit
   then
   up @ u + cap > if
      up @ u + cap - {: shift:n :}
      buf shift + buf up @ shift - BYTE-COPY
      up @ shift - up !
   then
   a buf up @ + u BYTE-COPY
   up @ u + up ! ;

: GT-POOL-READ-STREAM ( idx n -- ) {: idx:idx stream:n :}
   idx stream GT-POOL-STREAM-FD-PTR {: fdp:ptr :}
   fdp @ FD>N GT-POOL-CHUNK GT-POOL-CHUNK-CAP read GT-POOL-RD !
   GT-POOL-RD @ 0 < if E-PROC-OUTPUT GT-POOL-THROW then
   GT-POOL-RD @ GT-POOL-CHUNK-CAP > if E-PROC-OUTPUT GT-POOL-THROW then
   GT-POOL-RD @ 0= if fdp GT-POOL-CLOSE-FD exit then
   idx stream GT-POOL-STREAM-FILE-FD-PTR @ GT-POOL-CHUNK GT-POOL-RD @ GT-POOL-FILE-WRITE
   idx stream GT-POOL-CHUNK GT-POOL-RD @ GT-POOL-TAIL+
   idx stream GT-POOL-STREAM-TOTAL-PTR {: tp:ptr :}
   tp @ GT-POOL-RD @ + tp ! ;

: GT-POOL-DRAIN-SLOT ( idx -- ) {: idx :}
   idx GT-POOL-OUT-SLOT GT-POOL-PFD-REVENTS 0 <> if idx 0 GT-POOL-READ-STREAM then
   idx GT-POOL-ERR-SLOT GT-POOL-PFD-REVENTS 0 <> if idx 1 GT-POOL-READ-STREAM then ;

: GT-POOL-CAPTURE-DONE? ( idx -- bool ) {: idx :}
   idx GT-POOL-OUT-R@ FD>N 0 < idx GT-POOL-ERR-R@ FD>N 0 < and ;

: GT-POOL-OK? ( idx -- bool ) {: idx :}
   idx GT-POOL-EXITED-PTR @
   idx GT-POOL-CODE-PTR @ 0= and ;

\ ---- a deadline the child itself owned ---------------------------------------
\
\ lib/process throws E-PROC-TIMEOUT when a child a suite spawned outlives the
\ deadline that suite gave it. Uncaught, it leaves the suite through the
\ engine's top-level reporter (src/habu/habu2.f, labels LUNCAUGHT and LUNCMSG):
\ for a throw code outside [1,255] that reporter writes "hb: uncaught throw
\ code ", the signed decimal code and a newline to fd 2 as the process's last
\ output, then exits the deterministic UNCAUGHT-RC (src/habu/layout.f). Such a
\ row used to read kind=exit code=67 with nothing naming the cause, so a red
\ this pool caused by starving that child could not be told apart from a
\ genuine failure and every gate run under load had to be repeated by hand.
19 constant GT-POOL-UNC-DIGITS-MAX      \ a signed 64-bit code has no more
variable GT-POOL-UNC-I                  \ cursor into the captured stderr
variable GT-POOL-UNC-V                  \ the code being read back
variable GT-POOL-UNC-SCALE              \ place value of the digit under the cursor

: GT-POOL-UNCAUGHT-MSG$ ( -- ptr u8 n )
   s" hb: uncaught throw code " ;

: GT-POOL-DIGIT? ( n -- bool ) {: c:n :}
   c $30 >= c $39 <= and ;

: GT-POOL-DIGIT-BEFORE? ( ptr u8 n n -- bool ) {: a:ptr u:n i:n :}
   i 0 <= if 0 0= 0= exit then
   i u > if 0 0= 0= exit then
   a i 1 - BYTE+ c@ GT-POOL-DIGIT? ;

\ The v bytes of b sit immediately before index i.
: GT-POOL-ENDS-AT? ( ptr u8 n n ptr u8 n -- bool ) {: a:ptr u:n i:n b:ptr v:n :}
   i v - 0 < if 0 0= 0= exit then
   i u > if 0 0= 0= exit then
   a i v - BYTE+ v b v STR= ;

\ Read the code back out of that report. The reporter's write is the process's
\ last, so the report ends the capture, which keeps the stream's tail: the
\ prefix, an optional minus, at least one digit, the closing newline and
\ nothing after it. A capture shaped any other way reports no code, so output
\ that merely carries the text is not mistaken for the engine's own report.
: GT-POOL-UNCAUGHT-CODE? ( ptr u8 n -- bool n )   \ reported? code
   {: a:ptr u:n :}
   0 GT-POOL-UNC-V !  1 GT-POOL-UNC-SCALE !
   u 0 <= if 0 0= 0= 0 exit then
   a u 1 - BYTE+ c@ $0A <> if 0 0= 0= 0 exit then
   u 1 - GT-POOL-UNC-I !
   begin a u GT-POOL-UNC-I @ GT-POOL-DIGIT-BEFORE? while
      GT-POOL-UNC-I @ 1 - GT-POOL-UNC-I !
      a GT-POOL-UNC-I @ BYTE+ c@ $30 - GT-POOL-UNC-SCALE @ * GT-POOL-UNC-V @ + GT-POOL-UNC-V !
      GT-POOL-UNC-SCALE @ 10 * GT-POOL-UNC-SCALE !
   repeat
   u 1 - GT-POOL-UNC-I @ <= if 0 0= 0= 0 exit then
   u 1 - GT-POOL-UNC-I @ - GT-POOL-UNC-DIGITS-MAX > if 0 0= 0= 0 exit then
   a u GT-POOL-UNC-I @ s" -" GT-POOL-ENDS-AT? if
      GT-POOL-UNC-V @ negate GT-POOL-UNC-V !
      GT-POOL-UNC-I @ 1 - GT-POOL-UNC-I !
   then
   a u GT-POOL-UNC-I @ GT-POOL-UNCAUGHT-MSG$ GT-POOL-ENDS-AT? 0= if 0 0= 0= 0 exit then
   0 0= GT-POOL-UNC-V @ ;

: GT-POOL-UNCAUGHT-TIMEOUT? ( ptr u8 n -- bool )
   GT-POOL-UNCAUGHT-CODE? E-PROC-TIMEOUT = and ;

\ A slot whose child died on its own inner deadline: it exited UNCAUGHT-RC and
\ its last stderr bytes are the engine's report for E-PROC-TIMEOUT.
: GT-POOL-INNER-TIMEOUT? ( idx -- bool ) {: idx :}
   idx GT-POOL-EXITED-PTR @ 0= if 0 0= 0= exit then
   idx GT-POOL-CODE-PTR @ UNCAUGHT-RC <> if 0 0= 0= exit then
   idx GT-POOL-ERR-BUF idx GT-POOL-ERR-U-PTR @ GT-POOL-UNCAUGHT-TIMEOUT? ;

\ Report it the way the pool reports a slot its own reaper killed: a deadline
\ expired, and the saturation suffix says how contended the pool was when it
\ did. The row stays red - the timeout variant clears the exited flag, which
\ is what GT-POOL-OK? answers on - and the depth is snapshotted before the
\ reap drops this slot from the live count, exactly as GT-POOL-TIMEOUT does.
: GT-POOL-RECLASSIFY-INNER-TIMEOUT ( idx -- ) {: idx :}
   idx GT-POOL-INNER-TIMEOUT? 0= if exit then
   OUTCOME:TIMEOUT idx GT-POOL-OUTCOME!
   GT-POOL-LIVE @ idx GT-POOL-SAT-LIVE-PTR ! ;

: GT-POOL-TRUNC-LINE ( idx n -- ) {: idx:idx stream:n :}
   idx stream GT-POOL-STREAM-TOTAL-PTR @ idx stream GT-POOL-STREAM-U-PTR @ - {: cut:n :}
   cut 0 <= if exit then
   s" [tail truncated " type cut GT-U-TYPE
   s"  bytes; full capture: " type
   idx stream GT-POOL-STREAM-FILE$ type
   s" ]" type cr ;

: GT-POOL-OUTPUT ( idx -- ) {: idx :}
   idx 0 GT-POOL-TRUNC-LINE
   idx GT-POOL-OUT-BUF idx GT-POOL-OUT-U-PTR @ type
   idx 1 GT-POOL-TRUNC-LINE
   idx GT-POOL-ERR-BUF idx GT-POOL-ERR-U-PTR @ type ;

: GT-POOL-OUTCOME-LINE ( idx -- ) {: idx :}
   s" outcome: " type idx GT-POOL-EXITED-PTR @ idx GT-POOL-TIMED-OUT-PTR @ GT-POOL-KIND-NAME.
   s"  code: " type idx GT-POOL-CODE-PTR @ FMT:.INT cr ;

: GT-POOL-CAPTURE-LINES ( idx -- ) {: idx:idx :}
   s" stdout-file" idx 0 GT-POOL-STREAM-FILE$ GT-POOL-LINE$
   s" stderr-file" idx 1 GT-POOL-STREAM-FILE$ GT-POOL-LINE$ ;

: GT-POOL-FAIL ( idx -- ) {: idx :}
   idx GT-POOL-OUTPUT
   idx GT-POOL-OUTCOME-LINE
   idx GT-POOL-CAPTURE-LINES
   s" FAIL: " type idx GT-POOL-LABEL$ type cr
   idx GT-POOL-RED+ ;

: GT-POOL-REAP ( idx -- ) {: idx :}
   idx GT-POOL-PID@ PROC-WAIT-OUTCOME idx GT-POOL-OUTCOME!
   idx GT-POOL-RECLASSIFY-INNER-TIMEOUT
   -1 >PID idx GT-POOL-PID-PTR !
   idx GT-POOL-KILL-REAPER
   idx GT-POOL-CLOSE-CAPTURE
   idx GT-POOL-RETIRE-SLOT
   idx GT-POOL-OK? if idx GT-POOL-PASS-LINE exit then
   idx GT-POOL-FAIL ;

: GT-POOL-REAP-DONE ( idx -- ) {: idx :}
   idx GT-POOL-DONE@ 0= 0= if exit then
   idx GT-POOL-CAPTURE-DONE? if idx GT-POOL-REAP then ;

: GT-POOL-DRAIN-READY ( -- )
   0 begin dup GT-POOL-MAX < while
      dup >IDX GT-POOL-DRAIN-SLOT
      dup >IDX GT-POOL-REAP-DONE
      1+
   repeat drop ;

: GT-POOL-TIMEOUT? ( idx -- bool ) {: idx :}
   idx GT-POOL-DONE@ 0= 0= if 0 0= 0= exit then
   mono-ns idx GT-POOL-START-PTR @ - PROC-NS-PER-MS /
   idx GT-POOL-TIMEOUT-PTR @ >= ;

: GT-POOL-TIMEOUT ( idx -- ) {: idx :}
   OUTCOME:TIMEOUT idx GT-POOL-OUTCOME!
   \ Snapshot the saturation depth (live slots, including this one) for RED.
   GT-POOL-LIVE @ idx GT-POOL-SAT-LIVE-PTR !
   \ A root that catches SIGTERM is asked to end first (GT-POOL-ASK-END), so
   \ what it writes while it ends is in the pipe for the drain below.
   idx GT-POOL-ASK-END
   idx GT-POOL-GRACE-MS >MS PROC-DEADLINE-AT GT-POOL-AWAIT-END
   \ Final bounded poll+drain so the last bytes the worker wrote (often the
   \ hang clue) reach the tail and capture file before the fds are closed.
   GT-POOL-POLL-BUILD  GT-POOL-POLL drop  idx GT-POOL-DRAIN-SLOT
   idx GT-POOL-KILL-SLOT
   idx GT-POOL-RETIRE-SLOT
   idx GT-POOL-FAIL ;

: GT-POOL-CHECK-TIMEOUTS ( -- )
   0 begin dup GT-POOL-MAX < while
      dup >IDX GT-POOL-TIMEOUT? if dup >IDX GT-POOL-TIMEOUT then
      1+
   repeat drop ;

: GT-POOL-WAIT-LINES ( -- )
   0 begin dup GT-POOL-MAX < while
      dup >IDX GT-POOL-DONE@ 0= if dup >IDX GT-POOL-WAIT-LINE then
      1+
   repeat drop ;

\ THE ANSWER TO A CAUGHT SIGNAL (see GT-POOL-CATCH-SIGNALS). Every live slot
\ is killed with its whole tree, the cleanup registry removes what this
\ process registered - the runner's root, and with it every slot's scratch -
\ and the process then dies OF THE SIGNAL (lib/signal.f DIE-OF). A removal
\ that throws is named and the process still ends as signalled.
: GT-POOL-SIGNAL@ ( -- n )
   GT-POOL-CATCHING @ 0= if 0 exit then
   SIGNAL:TAKE MATCH SIGNAL:signal-result
      signal OF ENDOF
      timeout OF 0 ENDOF
   ;MATCH ;

: GT-POOL-SIGNAL-DIE ( n -- ) {: sig:n :}
   s" test pool: signal " type sig GT-POOL-N-TYPE
   s" , ending " type GT-POOL-LIVE @ GT-POOL-N-TYPE s"  live rows" type cr
   GT-POOL-KILL-ALL
   [: GT-CLEANUP GT-POOL-FALLBACK-REMOVE ;] catch {: code:n :}
   code 0<> if s" test pool: cleanup threw " type code GT-POOL-N-TYPE cr then
   s" test pool: signal" sig SIGNAL:DIE-OF ;

\ A step asks after every poll. A program that caught signals asks once more
\ when its own cleanup is done, for the one that arrived after its last step.
: GT-POOL-SIGNAL-CHECK ( -- )
   GT-POOL-SIGNAL@ dup 0= if drop exit then
   GT-POOL-SIGNAL-DIE ;

: GT-POOL-STEP ( -- )
   GT-POOL-POLL drop
   GT-POOL-SIGNAL-CHECK
   GT-POOL-DRAIN-READY
   GT-POOL-CHECK-TIMEOUTS
   GT-POOL-WAIT-LINES ;

: GT-POOL-WAIT-FREE ( -- )
   begin GT-POOL-LIVE @ GT-POOL-LIMIT @ >= while
      GT-POOL-STEP
   repeat ;

: GT-POOL-START ( ptr u8 n ptr u8 n n -- ) {: path:ptr pathu label:ptr labelu timeout :}
   GT-POOL-WAIT-FREE
   GT-POOL-FIND-FREE {: idx :}
   path pathu label labelu timeout idx GT-POOL-START-SLOT ;

: GT-POOL-START-STDIN ( ptr u8 n ptr u8 n ptr u8 n n -- )
   {: path:ptr pathu label:ptr labelu bytes:ptr byten timeout :}
   GT-POOL-WAIT-FREE
   GT-POOL-FIND-FREE {: idx :}
   path pathu label labelu bytes byten timeout idx GT-POOL-START-STDIN-SLOT ;

: GT-POOL-START-FORK ( ptr u8 n n [ -- ] -- ) {: label:ptr labelu:n timeout:n q :}
   GT-POOL-WAIT-FREE
   GT-POOL-FIND-FREE {: idx:idx :}
   label labelu timeout idx q GT-POOL-START-FORK-SLOT ;

: GT-POOL-DRAIN-SOFT ( -- )
   begin GT-POOL-LIVE @ 0 > while
      GT-POOL-STEP
   repeat ;

\ The report is on stdout before the tree goes; the die ends the process, so
\ the cleanup has to run in front of it or the pool root outlives every red
\ run (that is where the leaked gate-pool-test-battery trees came from). The
\ fallback root goes last for the same reason the report goes first: the red
\ lines rebuild their capture paths under it (GT-POOL-RED-STREAM$).
: GT-POOL-RED-DIE ( -- )
   GT-POOL-RED-REPORT
   GT-CLEANUP
   GT-POOL-FALLBACK-REMOVE
   s" test pool failed" 1 die ;

\ Green path: every slot has retired and nothing reads the captures after this,
\ so the suite-less user's root goes here.
: GT-POOL-DRAIN ( -- )
   GT-POOL-DRAIN-SOFT
   GT-POOL-RED# 0 > if GT-POOL-RED-DIE then
   GT-POOL-FALLBACK-REMOVE ;

;using
