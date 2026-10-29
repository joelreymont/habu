\ gate-pool.f - checked bounded process pool for native tests.
\
\ A caller starts rows - an executable spawned with the argv and environment it
\ prepared (GT-POOL-START, GT-POOL-START-STDIN) or a forked quotation
\ (GT-POOL-START-FORK) - and GT-POOL-DRAIN waits for every one: a red row's
\ report, then exit 1. At most GT-POOL-SLOTS! rows run at once, GT-POOL-MAX at
\ most, six by default on Linux and eight on macOS. A row's stdout and stderr go
\ to capture files; its red line keeps the tail of each.
\
\ A SPAWNED ROW gets its own scratch directory as TMPDIR, which the pool
\ removes when the slot retires, whatever the row left in it; a scratch that
\ will not go turns the row red. A row whose caller set TMPDIR itself owns that
\ directory. It also gets a short directory for Unix-domain sockets as
\ HB_SOCK_TMP, under one socket root per pool session.
\
\ A ROW ENDS WHEN ITS PROCESS EXITS, and its kind is exit or signal. The pool
\ keeps no clock on it: every GT-HEARTBEAT-MS a live row prints a WAIT line
\ with its wall time and its tree's CPU time. Output still open after the exit
\ is held by something the row left behind, and the pool kills the row's tree
\ (src/lib/process-tree.f). When the run ends early every row ends with its
\ tree; a row that catches SIGTERM is sent it first and given GT-POOL-GRACE-MS.
\
\ A program that calls GT-POOL-CATCH-SIGNALS answers SIGTERM, SIGINT and SIGHUP
\ at its next GT-POOL-STEP: it prints each live row's WAIT line and output tail,
\ ends every live row, removes its roots and dies of the same signal. A SIGKILL
\ of the pool leaves what its rows spawned, its temporary root and its socket
\ root; one that lands during a tree walk leaves what the walk had stopped
\ stopped. Only an orphaned process group is resumed: POSIX has the kernel send
\ each member SIGHUP and then SIGCONT when a death orphans a group with a
\ stopped member, which ends a member that does not ignore SIGHUP.
\ test/gate-signal-test.f checks these.

require src/lib/string.f
require src/lib/fmt.f
require src/lib/fs-mutate.f
require src/lib/process-fork.f
require src/lib/process-env.f
require src/lib/process-tree.f               \ a slot ends with everything descended from its child
require src/lib/signal.f                     \ a signalled run is answered at a pool step
require src/lib/test/runner.f
require src/lib/test/suite.f                 \ TEST:ITEM-MAX sizes the red table
require tools/why-threw.f

using WHY-THREW                          \ the fork-throw self-identifying report

16 constant GT-POOL-MAX
6 constant GT-POOL-LINUX-DEFAULT
8 constant GT-POOL-MACOS-DEFAULT
3 constant GT-POOL-FDS                    \ stdout, stderr and the exit watch
8 constant GT-PFD-SZ
$64 constant GT-POOL-POLL-MS
$1000 constant GT-POOL-CHUNK-CAP
64 constant GT-POOL-NAME-CAP
32 constant GT-POOL-NUM-CAP
\ Rows an adapter starts beside the registered suites: test/gate-images.f starts
\ one build row per keyed image family, and refuses more families than this.
10 constant GT-POOL-SIDE-MAX
10000 constant GT-POOL-GRACE-MS         \ a root that catches SIGTERM ends itself inside this (GT-POOL-ASK-END)
\ Red rows: one per registered suite (src/lib/test/suite.f ITEM-MAX) plus the side
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
create GT-POOL-WATCHES GT-POOL-MAX cells allot
create GT-POOL-ENDEDS GT-POOL-MAX cells allot
create GT-POOL-CODES GT-POOL-MAX cells allot
create GT-POOL-DONES GT-POOL-MAX cells allot
create GT-POOL-STARTS GT-POOL-MAX cells allot
create GT-POOL-LASTS GT-POOL-MAX cells allot
create GT-POOL-ASKEDS GT-POOL-MAX cells allot
create GT-POOL-LABELS GT-POOL-MAX GT-FAIL-NAME-CAP * allot
create GT-POOL-LABEL-US GT-POOL-MAX cells allot
create GT-POOL-PFDS GT-POOL-MAX GT-POOL-FDS * GT-PFD-SZ * allot
create GT-POOL-CHUNK GT-POOL-CHUNK-CAP allot
create GT-POOL-NUM-BUF GT-POOL-NUM-CAP allot
create GT-POOL-NAME-BUF GT-POOL-NAME-CAP allot
create GT-POOL-FALLBACK-BUF PATH-CAP allot
create GT-POOL-OUT-PATHS GT-POOL-MAX PATH-CAP * allot
create GT-POOL-ERR-PATHS GT-POOL-MAX PATH-CAP * allot
create GT-POOL-OUT-PATH-US GT-POOL-MAX cells allot
create GT-POOL-ERR-PATH-US GT-POOL-MAX cells allot
create GT-POOL-TMP-PATHS GT-POOL-MAX PATH-CAP * allot
create GT-POOL-TMP-PATH-US GT-POOL-MAX cells allot
create GT-POOL-SOCK-PATHS GT-POOL-MAX PATH-CAP * allot
create GT-POOL-SOCK-PATH-US GT-POOL-MAX cells allot
create GT-POOL-SCRATCH-CODES GT-POOL-MAX cells allot
create GT-POOL-SOCK-ROOT-BUF PATH-CAP allot
create GT-POOL-OUT-FDS GT-POOL-MAX cells allot
create GT-POOL-ERR-FDS GT-POOL-MAX cells allot
create GT-POOL-OUT-TOTALS GT-POOL-MAX cells allot
create GT-POOL-ERR-TOTALS GT-POOL-MAX cells allot
create GT-POOL-SEQS GT-POOL-MAX cells allot
create GT-POOL-CPU-USEDS GT-POOL-MAX cells allot
create GT-POOL-RED-LABELS GT-POOL-RED-MAX GT-FAIL-NAME-CAP * allot
create GT-POOL-RED-LABEL-US GT-POOL-RED-MAX cells allot
create GT-POOL-RED-EXITEDS GT-POOL-RED-MAX cells allot
create GT-POOL-RED-CODES GT-POOL-RED-MAX cells allot
create GT-POOL-RED-SEQS GT-POOL-RED-MAX cells allot
create GT-POOL-RED-PATH-BUF PATH-CAP allot
create GT-POOL-RED-SCRATCH-CODES GT-POOL-RED-MAX cells allot

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

: GT-POOL-WATCH-PTR ( idx -- ptr fd )
   IDX>N cells GT-POOL-WATCHES + ;

\ The slot's process has exited: its watch fired, or it was gone at the open.
: GT-POOL-ENDED-PTR ( idx -- ptr bool )
   IDX>N cells GT-POOL-ENDEDS + ;

: GT-POOL-CODE-PTR ( idx -- ptr n )
   IDX>N cells GT-POOL-CODES + ;

\ Decompose a worker outcome into the slot's exited/code cells (lossless: exit
\ codes >= 128 stay distinct from signal deaths). A wait for the pid never
\ answers a timeout.
: GT-POOL-OUTCOME! ( outcome idx -- ) {: idx:idx :}
   MATCH outcome
     exited OF idx GT-POOL-CODE-PTR ! true idx GT-POOL-EXITED-PTR ! ENDOF
     signaled OF idx GT-POOL-CODE-PTR ! false idx GT-POOL-EXITED-PTR ! ENDOF
     timeout OF E-PROC-TIMEOUT throw ENDOF
   ;MATCH ;

: GT-POOL-KIND-NAME. ( bool -- ) {: exited:bool :}
   exited if s" exit" type exit then
   s" signal" type ;

: GT-POOL-DONE-PTR ( idx -- ptr n )
   IDX>N cells GT-POOL-DONES + ;

: GT-POOL-START-PTR ( idx -- ptr n )
   IDX>N cells GT-POOL-STARTS + ;

: GT-POOL-LAST-PTR ( idx -- ptr n )
   IDX>N cells GT-POOL-LASTS + ;

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

\ The CPU time in ms the slot's process tree had run at its last reading.
: GT-POOL-CPU-USED-PTR ( idx -- ptr n )
   IDX>N cells GT-POOL-CPU-USEDS + ;

: GT-POOL-OUT-PATH-BUF ( idx -- ptr u8 )
   IDX>N PATH-CAP * GT-POOL-OUT-PATHS + ;

: GT-POOL-ERR-PATH-BUF ( idx -- ptr u8 )
   IDX>N PATH-CAP * GT-POOL-ERR-PATHS + ;

: GT-POOL-OUT-PATH-U-PTR ( idx -- ptr n )
   IDX>N cells GT-POOL-OUT-PATH-US + ;

: GT-POOL-ERR-PATH-U-PTR ( idx -- ptr n )
   IDX>N cells GT-POOL-ERR-PATH-US + ;

: GT-POOL-TMP-PATH-BUF ( idx -- ptr u8 )
   IDX>N PATH-CAP * GT-POOL-TMP-PATHS + ;

: GT-POOL-TMP-PATH-U-PTR ( idx -- ptr n )
   IDX>N cells GT-POOL-TMP-PATH-US + ;

\ The scratch directory this pool made for a SPAWNED slot's child, empty for a
\ forked slot, which shares the parent's image and gets no directory of its own.
: GT-POOL-TMP$ ( idx -- ptr u8 n ) {: idx:idx :}
   idx GT-POOL-TMP-PATH-BUF idx GT-POOL-TMP-PATH-U-PTR @ ;

: GT-POOL-SOCK-PATH-BUF ( idx -- ptr u8 )
   IDX>N PATH-CAP * GT-POOL-SOCK-PATHS + ;

: GT-POOL-SOCK-PATH-U-PTR ( idx -- ptr n )
   IDX>N cells GT-POOL-SOCK-PATH-US + ;

\ The code the last refused removal of the slot's directories threw, 0 while
\ they all went (GT-POOL-CHILD-TMP-REMOVE).
: GT-POOL-SCRATCH-CODE-PTR ( idx -- ptr n )
   IDX>N cells GT-POOL-SCRATCH-CODES + ;

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

: GT-POOL-CLOSE-WATCH ( idx -- ) {: idx:idx :}
   idx GT-POOL-WATCH-PTR GT-POOL-CLOSE-FD ;

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
\ - for a gate row, the engines it builds and runs - and
\ used to outlive the row, reparented to init. src/lib/process-tree.f stops and
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
   idx GT-POOL-CLOSE-WATCH
   idx GT-POOL-CLOSE-CAPTURE ;

\ Absence is tolerated: a directory is gone already when a child removed it
\ itself, and a forked slot never had one.
: GT-POOL-TREE-REMOVE ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0 <= if exit then
   a u FS:EXISTS? 0= if exit then
   a u FS:REMOVE-TREE ;

\ One of a slot's directories. A removal that throws - a tree its child left
\ with a directory it locked, which FS:REMOVE-TREE refuses with E-FS-IO - is
\ named with the row, the path and the code, and the code is the slot's
\ (GT-POOL-OK?): the row fails and the pool goes on.
: GT-POOL-SCRATCH-REMOVE ( ptr u8 n idx -- ) {: a:ptr u:n idx:idx :}
   a u [: 2dup GT-POOL-TREE-REMOVE ;] catch {: code:n :}
   2drop
   code 0= if exit then
   s" test pool: scratch " type a u type
   s"  of " type idx GT-POOL-LABEL$ type
   s"  not removed, throw " type code FMT:.INT cr
   code idx GT-POOL-SCRATCH-CODE-PTR ! ;

\ The child's scratch and its socket directory (GT-POOL-CHILD-TMP!), once
\ nothing is left to write into them.
: GT-POOL-CHILD-TMP-REMOVE ( idx -- ) {: idx:idx :}
   idx GT-POOL-TMP$ idx GT-POOL-SCRATCH-REMOVE
   idx GT-POOL-SOCK$ idx GT-POOL-SCRATCH-REMOVE ;

\ THE SOCKET ROOT. Every spawned child's socket directory (GT-POOL-CHILD-SOCK!)
\ is made inside one directory per pool session, which goes in
\ the cleanup registry once (src/lib/fs-mutate.f FS:CLEANUP-TREE+). One entry a row
\ would keep hundreds in the registry, which holds each entry until a
\ FS:CLEANUP-RUN, since a gate starts hundreds of rows. So a process that
\ ends without retiring or killing its slots - an uncaught throw, a die -
\ still removes every slot's directory with the root, and GT-CLEANUP removes it
\ with the runner's root. A session's root is made at its first spawn, and
\ made again if a GT-CLEANUP removed it. GT-POOL-RESET removes the one before:
\ no live slot's directory is in it by then, and a GT-START since may have
\ reset its entry away. A SIGKILL of this process leaves it.
\
\ A pool that a pool spawned has a row directory of about 100 bytes as its
\ TMPDIR, too long to hold a socket, and the short HB_SOCK_TMP its own pool
\ gave it: its root goes under that. A pool nobody spawned has no HB_SOCK_TMP,
\ and its root goes under TMPDIR.
variable GT-POOL-SOCK-ROOT-U
0 GT-POOL-SOCK-ROOT-U !

: GT-POOL-SOCK-ROOT$ ( -- ptr u8 n )
   GT-POOL-SOCK-ROOT-BUF GT-POOL-SOCK-ROOT-U @ ;

: GT-POOL-SOCK-ROOT-REMOVE ( -- )
   GT-POOL-SOCK-ROOT$ GT-POOL-TREE-REMOVE
   0 GT-POOL-SOCK-ROOT-U ! ;

: GT-POOL-SOCK-ROOT-NEW ( -- ptr u8 n )
   s" HB_SOCK_TMP" GETENV {: base:ptr baseu:n :}
   baseu 0= if s" hb-sock" FS:MKTEMP-DIR exit then
   base baseu s" hb-sock" FS:MKTEMP-DIR-IN ;

: GT-POOL-SOCK-ROOT-MAKE ( -- )
   GT-POOL-SOCK-ROOT-U @ 0 > if GT-POOL-SOCK-ROOT$ FS:EXISTS? if exit then then
   GT-POOL-SOCK-ROOT-NEW {: a:ptr u:n :}
   a GT-POOL-SOCK-ROOT-BUF u BYTE-COPY
   u GT-POOL-SOCK-ROOT-U !
   GT-POOL-SOCK-ROOT$ FS:CLEANUP-TREE+ ;

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
\ process that has already exited: proc-watch-open
\ answers -1 for one, and there is nothing left to wait for.
: GT-POOL-AWAIT-END ( idx n -- ) {: idx:idx deadline:n :}
   idx GT-POOL-ASKED-PTR @ 0= if exit then
   idx GT-POOL-PID@ PID>N proc-watch-open {: w:n :}
   w 0 < if exit then
   w >FD POLLIN PROC-PFD!
   1 deadline PROC-LEFT-MS MS>N deadline PROC-POLL-RESTART drop
   w close ;

\ A slot killed here is never reaped, so it never retires: its directories go
\ here, after the kill. Its TMPDIR is under the root a signalled run
\ removes anyway; the socket directory is not. Every root that catches SIGTERM
\ is asked before any slot is killed, so their graces run together and beside
\ the other slots' kills.
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
\ (src/lib/process-tree.f), and nothing removes the root: the engine calls the exit
\ hook only on its own way out (src/habu/layout.f EXIT-HOOK-CELL).
\
\ A program that is a pool from its first row to its exit - the gate - calls
\ GT-POOL-CATCH-SIGNALS before it makes its root. No Forth word runs in a
\ handler (src/lib/signal.f), so a signal is answered at the next GT-POOL-STEP,
\ which is never far: the signal itself interrupts a step's poll, and between
\ two steps the pool only starts a row. The answer is GT-POOL-SIGNAL-DIE,
\ below GT-POOL-FALLBACK-REMOVE.
\
\ IT IS ASKED FOR, NOT INSTALLED BY GT-POOL-RESET. The catch lasts as long as
\ the process and only a step answers it, so a program that ran a pool and
\ went on to other work would never answer SIGTERM again.
\
\ The catch is src/lib/signal.f CATCH-STOPS, which leaves a signal the process was
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
   0 idx GT-POOL-SCRATCH-CODE-PTR !
   false idx GT-POOL-ASKED-PTR !
   0 idx GT-POOL-SEQ-PTR !
   0 idx GT-POOL-CPU-USED-PTR !
   -1 >FD idx GT-POOL-WATCH-PTR !
   false idx GT-POOL-ENDED-PTR !
   true idx GT-POOL-EXITED-PTR !
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
\ The live slot whose capture seq is n, or -1.
: GT-POOL-SEQ-SLOT ( n -- n ) {: seq:n :}
   0 begin dup GT-POOL-LIMIT @ < while
      dup >IDX GT-POOL-DONE@ 0=
      over >IDX GT-POOL-SEQ-PTR @ seq = and if exit then
      1+
   repeat drop
   -1 ;

: GT-POOL-SEQ-LIVE? ( n -- bool )
   GT-POOL-SEQ-SLOT 0 >= ;

: GT-POOL-ELAPSED-MS ( idx -- n ) {: idx :}
   mono-ns idx GT-POOL-START-PTR @ - PROC-NS-PER-MS / ;

: GT-POOL-PASS-LINE ( idx -- )
   {: idx :}
   s" PASS: " type
   idx GT-POOL-LABEL$ type
   s"  (" type
   idx GT-POOL-ELAPSED-MS GT-U-TYPE
   s" ms)" type cr
   idx GT-POOL-CPU-USED-PTR @ {: cpu:n :}
   cpu 0 <= if exit then
   s"   sampled CPU: " type cpu GT-U-TYPE s" ms" type cr ;

: GT-POOL-WAIT-DUE? ( idx -- bool ) {: idx :}
   mono-ns idx GT-POOL-LAST-PTR @ - PROC-NS-PER-MS / GT-HEARTBEAT-MS >= ;

\ A reading that throws ends the run, as a refused reaper fork does.
variable GT-POOL-CPU-NS                  \ the reading, out of its catch

: GT-POOL-CPU-READ ( idx -- ) {: idx:idx :}
   idx GT-POOL-PID@ [: dup PROC-TREE:CPU-NS GT-POOL-CPU-NS ! ;] catch {: code:n :}
   drop
   code 0= if GT-POOL-CPU-NS @ PROC-NS-PER-MS / idx GT-POOL-CPU-USED-PTR ! exit then
   s" test pool: CPU time of " type idx GT-POOL-LABEL$ type
   s"  not read, throw " type code FMT:.INT cr
   code GT-POOL-THROW ;

\ A live row's wall time, its tree's CPU time, user and system with what its
\ members reaped, and its change since the last reading: a delta that stays at
\ zero is a blocked row, one that never stops growing a spinning row, and a
\ negative one a process that left the tree.
: GT-POOL-WAIT. ( idx -- ) {: idx:idx :}
   idx GT-POOL-CPU-USED-PTR @ {: was:n :}
   idx GT-POOL-CPU-READ
   idx GT-POOL-CPU-USED-PTR @ {: cur:n :}
   s" WAIT: " type
   idx GT-POOL-LABEL$ type
   s"  (" type idx GT-POOL-ELAPSED-MS GT-U-TYPE
   s"  ms, cpu " type cur GT-U-TYPE
   s"  ms, " type cur was - {: d:n :}
   d 0 >= if $2B emit then d FMT:.INT
   s"  ms)" type cr ;

: GT-POOL-WAIT-LINE ( idx -- ) {: idx:idx :}
   idx GT-POOL-WAIT-DUE? if
      mono-ns idx GT-POOL-LAST-PTR !
      idx GT-POOL-WAIT.
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

: GT-POOL-SPAWN-FAIL. ( idx ptr u8 n -- ) {: idx:idx path:ptr pathu:n :}
   s" FAIL: test pool spawn" type cr
   s" test" idx GT-POOL-LABEL$ GT-POOL-LINE$
   s" path" path pathu GT-POOL-LINE$
   s" errno" PROCESS:ERRNO GT-POOL-LINE-N
   s" argv-count" PROC-ARGV-N @ COUNT>N GT-POOL-LINE-N
   s" env-count" PROC-ENV-N @ COUNT>N GT-POOL-LINE-N
   s" pool-live" GT-POOL-LIVE @ GT-POOL-LINE-N
   s" pool-limit" GT-POOL-LIMIT @ GT-POOL-LINE-N
   s" stdout-fd" idx GT-POOL-OUT-W-PTR @ GT-POOL-LINE-FD
   s" stderr-fd" idx GT-POOL-ERR-W-PTR @ GT-POOL-LINE-FD ;

\ Arm a co-located reaper for the just-spawned child. The spawn primitive makes
\ every child its own process-group leader before exec, so a kill and
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

\ The row's exit watch, opened as soon as its pid is known. macOS refuses a
\ watch on a process that has already exited (ESRCH): that row has ended.
: GT-POOL-WATCH-OPEN ( idx -- ) {: idx:idx :}
   idx GT-POOL-PID@ PID>N proc-watch-open {: w:n :}
   w 0 >= if w >FD idx GT-POOL-WATCH-PTR ! exit then
   w ESRCH# negate = if true idx GT-POOL-ENDED-PTR ! exit then
   E-PROC-OUTPUT GT-POOL-THROW ;

\ Spawns path with the staged argv and only the staged env, which its caller
\ has already inherited into, as slot idx's child, its stdin on
\ stdin (a negative one inherits this process's) and its stdout and stderr on
\ the slot's pipes, and records its pid. Stack-preserving under catch, so a
\ refusal is reported with the staging it refused.
: GT-POOL-LAUNCH ( idx ptr u8 n fd -- idx ptr u8 n fd ) {: idx:idx path:ptr pathu:n stdin:fd :}
   path pathu >LEN PROC-ARGV-PREPARE {: pathz:ptr argv:ptr :}
   PROC-ENV-PREPARE {: envp:ptr :}
   pathz dup ZLEN PROCESS:NEW PROCESS:HERMETIC
   argv PROCESS:ARGV-ROWS
   envp PROCESS:ENV-ROWS
   stdin FD>N 0 >= if 0 >FD stdin PROCESS:FD then
   1 >FD idx GT-POOL-OUT-W-PTR @ PROCESS:FD
   2 >FD idx GT-POOL-ERR-W-PTR @ PROCESS:FD
   PROCESS:SPAWN idx GT-POOL-PID-PTR !
   idx path pathu stdin ;

: GT-POOL-SPAWN-FD ( idx ptr u8 n fd -- ) {: idx:idx path:ptr pathu:n stdin:fd :}
   path pathu >LEN PROC-ARGV-CHECK-PATH
   idx path pathu stdin [: GT-POOL-LAUNCH ;] catch {: code:n :}
   drop drop drop drop
   code 0<> if
      idx path pathu GT-POOL-SPAWN-FAIL.
      PROC-ARGV-ENV-RESET
      code GT-POOL-THROW
   then
   PROC-ARGV-ENV-RESET
   idx GT-POOL-WATCH-OPEN
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

\ Become our own process-group leader right after fork so a kill
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
   idx GT-POOL-WATCH-OPEN
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

\ Suite-less pool users get one FS:MKTEMP-DIR capture directory so parallel
\ runs sharing TMPDIR cannot clobber each other's capture files. Its path is
\ copied out because the task's next temp reuses the span FS:MKTEMP-DIR answers.
: GT-POOL-FALLBACK-ROOT$ ( -- ptr u8 n )
   GT-POOL-FALLBACK-U @ 0 > if
      GT-POOL-FALLBACK-BUF GT-POOL-FALLBACK-U @ exit
   then
   s" hb-pool" FS:MKTEMP-DIR {: a:ptr u:n :}
   a GT-POOL-FALLBACK-BUF u BYTE-COPY u GT-POOL-FALLBACK-U !
   GT-POOL-FALLBACK-BUF GT-POOL-FALLBACK-U @ ;

: GT-POOL-CAPTURE-ROOT$ ( -- ptr u8 n )
   GT-ROOT-U @ 0 > if GT-ROOT exit then
   GT-POOL-FALLBACK-ROOT$ ;

\ The fallback root belongs to the process, not to one pool: the memo above
\ builds it once and GT-POOL-RESET keeps it, so every pool a suite-less user
\ runs captures into the same directory until a drain removes it.
\ A suite's root is the runner's (GT-START registers it with FS:CLEANUP-TREE+),
\ so this exits when there is one. Absence is tolerated the way
\ GT-POOL-CHILD-TMP-REMOVE tolerates it. Clearing the memo is what lets a
\ later pool in the same process capture again: it makes a fresh root with
\ FS:MKTEMP-DIR, where a stale memo would open the captures under a removed
\ directory (E-FS-OPEN).
: GT-POOL-FALLBACK-REMOVE ( -- )
   GT-ROOT-U @ 0 > if exit then
   GT-POOL-FALLBACK-U @ 0 <= if exit then
   GT-POOL-FALLBACK-BUF GT-POOL-FALLBACK-U @ FS:EXISTS? if
      GT-POOL-FALLBACK-BUF GT-POOL-FALLBACK-U @ FS:REMOVE-TREE
   then
   0 GT-POOL-FALLBACK-U ! ;

: GT-POOL-CAPTURE-PATH! ( idx n -- ) {: idx:idx stream:n :}
   GT-POOL-CAPTURE-ROOT$
   idx GT-POOL-SEQ-PTR @ stream GT-POOL-STREAM-SUFFIX$ GT-POOL-CAPTURE-NAME
   idx stream GT-POOL-STREAM-PATH-BUF FS:JOIN-PATH
   idx stream GT-POOL-STREAM-PATH-U-PTR ! ;

: GT-POOL-CAPTURE-OPEN-FD ( ptr u8 n -- fd ) {: pa:ptr pu:n :}
   pa pu FS:>PATHZ FS:O-WRONLY FS:O-CREAT or FS:O-TRUNC or FS:MODE-0644 open {: raw:n :}
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
\ own beside its capture files, named TMPDIR in its environment, and the
\ PARENT removes it when the slot retires. A child the pool killed - a tree kill,
\ a group kill - never reaches its own FS:CLEANUP-RUN, so its temp trees survived
\ it and nothing ever removed them; under TMPDIR they now go with the
\ directory, and so does anything its own children or the tools it ran left.
\ PROC-ENV-SET, not PROC-ENV+, because the caller has already inherited its
\ environment into the table and an inherited TMPDIR has to be replaced rather
\ than shadowed.
\
\ Forked slots never call this: a forked worker shares the parent's image, gets
\ no environment of its own, and must not be handed a directory whose removal
\ would touch the parent's tree.
\
\ A TMPDIR row already in the caller's table is one of two things: the copy
\ PROC-ENV-INHERIT-MISSING took from this process's own environment, which is
\ the pool's to replace, or a value the caller chose for its child - a scratch
\ root the caller owns and reaps (test/nf-path-test.f hands each build a root
\ of a chosen length and byte content) - which stays, and then the slot gets no
\ directory at all. A set TMPDIR is no private root by itself: macOS hands every
\ process the same per-user one, which this process's own copy names.
: GT-POOL-ENV-TMPDIR-OWN? ( -- bool )
   s" TMPDIR=" {: k:ptr ku:n :}
   s" TMPDIR" >LEN PROC-ENV-NAME-IDX {: i:n :}
   i 0 < if false exit then
   i >IDX PROC-ENV-SLOT @ {: z:ptr :}
   z ku BYTE+ z ZLEN ku -
   s" TMPDIR" GETENV STR= 0= ;

\ EACH SPAWNED CHILD ALSO GETS A SHORT DIRECTORY FOR ITS SOCKETS. A
\ Unix-domain socket's path has to fit sun_path - 104 bytes on macOS, 108 on
\ Linux - and a gate row's TMPDIR is about 100 bytes long already, longer again
\ for each pool that runs under a pool. So the pool also makes each spawned
\ child a directory named by its capture sequence number in the session's
\ socket root (GT-POOL-SOCK-ROOT-MAKE), which is short at every level, and
\ names it HB_SOCK_TMP in the child's environment
\ (test/db/pg-cluster.f puts its server's socket there). It goes with the
\ scratch. Every spawned slot gets one, whoever owns its TMPDIR: the value
\ inherited in its place would name this process's own, which a sibling may
\ be using. Under macOS's default TMPDIR a PostgreSQL socket path there is
\ about 90 bytes.
: GT-POOL-CHILD-SOCK! ( idx -- ) {: idx:idx :}
   GT-POOL-SOCK-ROOT-MAKE
   GT-POOL-SOCK-ROOT$ idx GT-POOL-SEQ-PTR @ GT-POOL-NUM$
   idx GT-POOL-SOCK-PATH-BUF FS:JOIN-PATH idx GT-POOL-SOCK-PATH-U-PTR !
   idx GT-POOL-SOCK$ FS:MAKE-DIRS
   s" HB_SOCK_TMP" >LEN idx GT-POOL-SOCK$ >LEN PROC-ENV-SET ;

: GT-POOL-CHILD-TMP! ( idx -- ) {: idx:idx :}
   idx GT-POOL-CHILD-SOCK!
   GT-POOL-ENV-TMPDIR-OWN? if 0 idx GT-POOL-TMP-PATH-U-PTR ! exit then
   GT-POOL-CAPTURE-ROOT$
   idx GT-POOL-SEQ-PTR @ s" -tmp" GT-POOL-CAPTURE-NAME
   idx GT-POOL-TMP-PATH-BUF FS:JOIN-PATH idx GT-POOL-TMP-PATH-U-PTR !
   idx GT-POOL-TMP$ FS:MAKE-DIRS
   s" TMPDIR" >LEN idx GT-POOL-TMP$ >LEN PROC-ENV-SET ;

\ The one place a slot leaves the live set, at its reap, whether it exited or
\ was signaled. The child's scratch goes here, after the kill, so nothing is
\ still writing into it.
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

: GT-POOL-RED-CODE-PTR ( n -- ptr n )
   GT-POOL-RED-CHECK cells GT-POOL-RED-CODES + ;

: GT-POOL-RED-SEQ-PTR ( n -- ptr n )
   GT-POOL-RED-CHECK cells GT-POOL-RED-SEQS + ;

: GT-POOL-RED-SCRATCH-CODE-PTR ( n -- ptr n )
   GT-POOL-RED-CHECK cells GT-POOL-RED-SCRATCH-CODES + ;

: GT-POOL-RED-LABEL$ ( n -- ptr u8 n ) {: i:n :}
   i GT-POOL-RED-LABEL-BUF i GT-POOL-RED-LABEL-U-PTR @ ;

\ A red row keeps its capture sequence number; the report rebuilds the two
\ capture paths from it the way GT-POOL-CAPTURE-PATH! built them, which holds
\ while the capture root (GT-START) stays the same between the capture and the
\ report, as it does for every pool session in the tree.
: GT-POOL-RED-STREAM$ ( n n -- ptr u8 n ) {: i:n stream:n :}
   GT-POOL-CAPTURE-ROOT$
   i GT-POOL-RED-SEQ-PTR @ stream GT-POOL-STREAM-SUFFIX$ GT-POOL-CAPTURE-NAME
   GT-POOL-RED-PATH-BUF FS:JOIN-PATH GT-POOL-RED-PATH-U !
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
   idx GT-POOL-CODE-PTR @ i GT-POOL-RED-CODE-PTR !
   idx GT-POOL-SCRATCH-CODE-PTR @ i GT-POOL-RED-SCRATCH-CODE-PTR !
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

: GT-POOL-RED-LINE ( n -- ) {: i:n :}
   s" RED: " type i GT-POOL-RED-LABEL$ type
   s"  kind=" type
   i GT-POOL-RED-EXITED-PTR @ GT-POOL-KIND-NAME.
   s"  code=" type i GT-POOL-RED-CODE-PTR @ GT-POOL-N-TYPE
   s"  out=" type i GT-POOL-RED-OUT$ type
   s"  err=" type i GT-POOL-RED-ERR$ type
   i GT-POOL-RED-SCRATCH-CODE-PTR @ 0<> if
      s"  scratch=" type i GT-POOL-RED-SCRATCH-CODE-PTR @ GT-POOL-N-TYPE
   then
   cr ;

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
\ start time. The caller then spawns or forks into the slot and counts it live.
: GT-POOL-OPEN-SLOT ( ptr u8 n idx -- ) {: label:ptr labelu idx :}
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
   idx GT-POOL-START-PTR @ idx GT-POOL-LAST-PTR ! ;

: GT-POOL-START-SLOT ( ptr u8 n ptr u8 n idx -- ) {: path:ptr pathu label:ptr labelu idx :}
   label labelu idx GT-POOL-OPEN-SLOT
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

: GT-POOL-START-STDIN-SLOT ( ptr u8 n ptr u8 n ptr u8 n idx -- )
   {: path:ptr pathu label:ptr labelu bytes:ptr byten idx :}
   label labelu idx GT-POOL-OPEN-SLOT
   idx GT-POOL-CHILD-TMP!
   PIPE-PAIR {: r w :}
   r FD-CLOEXEC!
   w FD-CLOEXEC!
   w FD-NOSIGPIPE!
   idx path pathu r GT-POOL-SPAWN-FD
   r FD>N close
   w bytes byten GT-POOL-FEED-STDIN
   GT-POOL-LIVE @ 1+ GT-POOL-LIVE ! ;

: GT-POOL-START-FORK-SLOT ( ptr u8 n idx [ -- ] -- ) {: label:ptr labelu:n idx:idx q :}
   label labelu idx GT-POOL-OPEN-SLOT
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

: GT-POOL-WATCH-SLOT ( idx -- idx )
   IDX>N GT-POOL-FDS * 2 + >IDX ;

: GT-POOL-POLL-SLOT ( idx -- ) {: idx :}
   idx GT-POOL-DONE@ 0= 0= if
      -1 >FD 0 idx GT-POOL-OUT-SLOT GT-POOL-PFD!
      -1 >FD 0 idx GT-POOL-ERR-SLOT GT-POOL-PFD!
      -1 >FD 0 idx GT-POOL-WATCH-SLOT GT-POOL-PFD!
      exit
   then
   idx GT-POOL-OUT-R@ POLLIN idx GT-POOL-OUT-SLOT GT-POOL-PFD!
   idx GT-POOL-ERR-R@ POLLIN idx GT-POOL-ERR-SLOT GT-POOL-PFD!
   idx GT-POOL-WATCH-PTR @ POLLIN idx GT-POOL-WATCH-SLOT GT-POOL-PFD! ;

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
   idx GT-POOL-ERR-SLOT GT-POOL-PFD-REVENTS 0 <> if idx 1 GT-POOL-READ-STREAM then
   idx GT-POOL-WATCH-SLOT GT-POOL-PFD-REVENTS 0 <> if true idx GT-POOL-ENDED-PTR ! then ;

\ A zero-wait poll of one stream's read end: TRUE when a read will not block.
: GT-POOL-READY-NOW? ( idx n -- bool ) {: idx:idx stream:n :}
   idx stream GT-POOL-STREAM-FD-PTR @ {: fd:fd :}
   fd FD>N 0 < if false exit then
   idx IDX>N GT-POOL-FDS * stream + >IDX {: ps:idx :}
   fd POLLIN ps GT-POOL-PFD!
   begin ps GT-POOL-PFD-SLOT 1 0 poll dup EINTR# negate = while drop repeat
   {: rc:n :}
   rc 0 < if E-PROC-OUTPUT GT-POOL-THROW then
   rc 0 > ;

: GT-POOL-READ-NOW ( idx n -- ) {: idx:idx stream:n :}
   idx stream GT-POOL-READY-NOW? if idx stream GT-POOL-READ-STREAM then ;

\ Read one stream until nothing is ready or it reaches EOF.
: GT-POOL-DRAIN-NOW ( idx n -- ) {: idx:idx stream:n :}
   begin idx stream GT-POOL-READY-NOW? while
      idx stream GT-POOL-READ-STREAM
   repeat ;

: GT-POOL-CAPTURE-DONE? ( idx -- bool ) {: idx :}
   idx GT-POOL-OUT-R@ FD>N 0 < idx GT-POOL-ERR-R@ FD>N 0 < and ;

: GT-POOL-OK? ( idx -- bool ) {: idx :}
   idx GT-POOL-EXITED-PTR @
   idx GT-POOL-CODE-PTR @ 0= and
   idx GT-POOL-SCRATCH-CODE-PTR @ 0= and ;

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
   s" outcome: " type
   idx GT-POOL-EXITED-PTR @ GT-POOL-KIND-NAME.
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
   -1 >PID idx GT-POOL-PID-PTR !
   idx GT-POOL-KILL-REAPER
   idx GT-POOL-CLOSE-CAPTURE
   idx GT-POOL-RETIRE-SLOT
   idx GT-POOL-OK? if idx GT-POOL-PASS-LINE exit then
   idx GT-POOL-FAIL ;

\ The row has exited, so the wait returns at once. A pipe not at EOF after one
\ read may be held by something the row left behind, and a drain then could
\ read a live writer forever: the tree is killed first, a zombie leader's group
\ included, and only then is each pipe drained.
: GT-POOL-FINISH ( idx -- ) {: idx:idx :}
   idx 0 GT-POOL-READ-NOW
   idx 1 GT-POOL-READ-NOW
   idx GT-POOL-CAPTURE-DONE? 0= if idx GT-POOL-KILL-TREE then
   idx 0 GT-POOL-DRAIN-NOW
   idx 1 GT-POOL-DRAIN-NOW
   idx GT-POOL-CLOSE-READS
   idx GT-POOL-CLOSE-WATCH
   idx GT-POOL-REAP ;

: GT-POOL-REAP-DONE ( idx -- ) {: idx :}
   idx GT-POOL-DONE@ 0= 0= if exit then
   idx GT-POOL-ENDED-PTR @ if idx GT-POOL-FINISH then ;

: GT-POOL-DRAIN-READY ( -- )
   0 begin dup GT-POOL-MAX < while
      dup >IDX GT-POOL-DRAIN-SLOT
      dup >IDX GT-POOL-REAP-DONE
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
\ and the process then dies OF THE SIGNAL (src/lib/signal.f DIE-OF). A removal
\ that throws is named and the process still ends as signalled.
: GT-POOL-SIGNAL@ ( -- n )
   GT-POOL-CATCHING @ 0= if 0 exit then
   SIGNAL:TAKE MATCH SIGNAL:signal-result
      signal OF ENDOF
      timeout OF 0 ENDOF
   ;MATCH ;

\ The end of a run removes the runner's root and the fallback root. A removal
\ that throws, as one does on a tree with a locked directory that a slot left
\ under the root, is named here, and the run still ends the way it was ending.
\ The registry goes with the report: the exit hook's retry would throw at the
\ same entry and name it a second time.
: GT-POOL-CLEANUP ( -- )
   [: GT-CLEANUP GT-POOL-FALLBACK-REMOVE ;] catch {: code:n :}
   code 0= if exit then
   s" test pool: cleanup threw " type code GT-POOL-N-TYPE cr
   FS:CLEANUP-RESET ;

\ Each live row's WAIT facts and output tail, with what its pipes hold now, so a
\ hang is named on the terminal before the cleanup removes its capture.
: GT-POOL-LIVE-REPORT ( -- )
   GT-POOL-MAX 0 ?do
      i >IDX GT-POOL-DONE@ 0= if
         i >IDX 0 GT-POOL-READ-NOW
         i >IDX 1 GT-POOL-READ-NOW
         i >IDX GT-POOL-WAIT.
         i >IDX GT-POOL-OUTPUT
      then
   loop ;

: GT-POOL-SIGNAL-DIE ( n -- ) {: sig:n :}
   s" test pool: signal " type sig GT-POOL-N-TYPE
   s" , ending " type GT-POOL-LIVE @ GT-POOL-N-TYPE s"  live rows" type cr
   GT-POOL-LIVE-REPORT
   GT-POOL-KILL-ALL
   GT-POOL-CLEANUP
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
   GT-POOL-WAIT-LINES ;

: GT-POOL-WAIT-FREE ( -- )
   begin GT-POOL-LIVE @ GT-POOL-LIMIT @ >= while
      GT-POOL-STEP
   repeat ;

: GT-POOL-START ( ptr u8 n ptr u8 n -- ) {: path:ptr pathu label:ptr labelu :}
   GT-POOL-WAIT-FREE
   GT-POOL-FIND-FREE {: idx :}
   path pathu label labelu idx GT-POOL-START-SLOT ;

: GT-POOL-START-STDIN ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: path:ptr pathu label:ptr labelu bytes:ptr byten :}
   GT-POOL-WAIT-FREE
   GT-POOL-FIND-FREE {: idx :}
   path pathu label labelu bytes byten idx GT-POOL-START-STDIN-SLOT ;

: GT-POOL-START-FORK ( ptr u8 n [ -- ] -- ) {: label:ptr labelu:n q :}
   GT-POOL-WAIT-FREE
   GT-POOL-FIND-FREE {: idx:idx :}
   label labelu idx q GT-POOL-START-FORK-SLOT ;

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
   GT-POOL-CLEANUP
   s" test pool failed" 1 die ;

\ Green path: every slot has retired and nothing reads the captures after this,
\ so the suite-less user's root goes here.
: GT-POOL-DRAIN ( -- )
   GT-POOL-DRAIN-SOFT
   GT-POOL-RED# 0 > if GT-POOL-RED-DIE then
   GT-POOL-FALLBACK-REMOVE ;

;using
