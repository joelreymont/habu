\ gate-pool.f - checked bounded process pool for native tests.
\
\ Load after lib/process-env.f and lib/test/runner.f.

require lib/process-fork.f

16 constant GT-POOL-MAX
6 constant GT-POOL-LINUX-DEFAULT
8 constant GT-POOL-MACOS-DEFAULT
2 constant GT-POOL-FDS
8 constant GT-PFD-SZ
$64 constant GT-POOL-POLL-MS
GT-POOL-MAX GT-OUT-CAP * constant GT-POOL-OUT-BYTES
GT-POOL-MAX GT-ERR-CAP * constant GT-POOL-ERR-BYTES

create GT-POOL-PIDS GT-POOL-MAX cells allot
create GT-POOL-OUT-RS GT-POOL-MAX cells allot
create GT-POOL-OUT-WS GT-POOL-MAX cells allot
create GT-POOL-ERR-RS GT-POOL-MAX cells allot
create GT-POOL-ERR-WS GT-POOL-MAX cells allot
create GT-POOL-OUT-US GT-POOL-MAX cells allot
create GT-POOL-ERR-US GT-POOL-MAX cells allot
create GT-POOL-KINDS GT-POOL-MAX cells allot
create GT-POOL-CODES GT-POOL-MAX cells allot
create GT-POOL-DONES GT-POOL-MAX cells allot
create GT-POOL-STARTS GT-POOL-MAX cells allot
create GT-POOL-LASTS GT-POOL-MAX cells allot
create GT-POOL-TIMEOUTS GT-POOL-MAX cells allot
create GT-POOL-LABELS GT-POOL-MAX GT-FAIL-NAME-CAP * allot
create GT-POOL-LABEL-US GT-POOL-MAX cells allot
create GT-POOL-PFDS GT-POOL-MAX GT-POOL-FDS * GT-PFD-SZ * allot
create GT-POOL-PROBE 1 allot

variable GT-POOL-OUT-BUFS-A
variable GT-POOL-ERR-BUFS-A
variable GT-POOL-LIVE
variable GT-POOL-RD
variable GT-POOL-LIMIT
variable GT-POOL-REQ

: GT-POOL-NO-PASS-HOOK ( ptr u8 n n -- )
   drop 2drop ;

defer GT-POOL-PASS-HOOK ( ptr u8 n n -- )

: GT-POOL-PASS-HOOK-DEFAULT! ( -- )
   [: GT-POOL-NO-PASS-HOOK ;] is GT-POOL-PASS-HOOK ;

GT-POOL-PASS-HOOK-DEFAULT!

: GT-POOL-OUT-BUFS-FIELD ( -- ptr ptr u8 )
   GT-POOL-OUT-BUFS-A 0 ptr-field ;

: GT-POOL-OUT-BUFS@ ( -- ptr u8 )
   GT-POOL-OUT-BUFS-FIELD @ ;

: GT-POOL-OUT-BUFS! ( ptr u8 -- )
   GT-POOL-OUT-BUFS-FIELD ! ;

: GT-POOL-OUT-BUFS ( -- ptr u8 )
   GT-POOL-OUT-BUFS@ ;

: GT-POOL-ERR-BUFS-FIELD ( -- ptr ptr u8 )
   GT-POOL-ERR-BUFS-A 0 ptr-field ;

: GT-POOL-ERR-BUFS@ ( -- ptr u8 )
   GT-POOL-ERR-BUFS-FIELD @ ;

: GT-POOL-ERR-BUFS! ( ptr u8 -- )
   GT-POOL-ERR-BUFS-FIELD ! ;

: GT-POOL-ERR-BUFS ( -- ptr u8 )
   GT-POOL-ERR-BUFS@ ;

: GT-POOL-ALLOC-BYTES ( n -- ptr u8 )
   MEM-ALLOC-BYTES drop ;

: GT-POOL-ALLOC-BUFFERS ( -- )
   GT-POOL-OUT-BUFS@ 0= if
      GT-POOL-OUT-BYTES GT-POOL-ALLOC-BYTES GT-POOL-OUT-BUFS!
   then
   GT-POOL-ERR-BUFS@ 0= if
      GT-POOL-ERR-BYTES GT-POOL-ALLOC-BYTES GT-POOL-ERR-BUFS!
   then ;

: GT-POOL-PID-PTR ( idx -- ptr pid )
   IDX>N cells GT-POOL-PIDS + ;

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

: GT-POOL-KIND-PTR ( idx -- ptr n )
   IDX>N cells GT-POOL-KINDS + ;

: GT-POOL-CODE-PTR ( idx -- ptr n )
   IDX>N cells GT-POOL-CODES + ;

: GT-POOL-DONE-PTR ( idx -- ptr n )
   IDX>N cells GT-POOL-DONES + ;

: GT-POOL-START-PTR ( idx -- ptr n )
   IDX>N cells GT-POOL-STARTS + ;

: GT-POOL-LAST-PTR ( idx -- ptr n )
   IDX>N cells GT-POOL-LASTS + ;

: GT-POOL-TIMEOUT-PTR ( idx -- ptr n )
   IDX>N cells GT-POOL-TIMEOUTS + ;

: GT-POOL-LABEL-U-PTR ( idx -- ptr n )
   IDX>N cells GT-POOL-LABEL-US + ;

: GT-POOL-PID@ ( idx -- pid )
   GT-POOL-PID-PTR @ ;

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

: GT-POOL-KILL-SLOT ( idx -- ) {: idx :}
   idx GT-POOL-PID@ PID>N 0 >= if
      idx GT-POOL-PID@ SIGKILL PROC-KILL-RAW drop
      idx GT-POOL-PID@ PROC-WAIT-STATUS drop
      -1 >PID idx GT-POOL-PID-PTR !
   then
   idx GT-POOL-CLOSE-WRITES
   idx GT-POOL-CLOSE-READS ;

: GT-POOL-KILL-ALL ( -- )
   0 begin dup GT-POOL-MAX < while
      dup >IDX GT-POOL-DONE@ 0= if dup >IDX GT-POOL-KILL-SLOT then
      1+
   repeat drop ;

: GT-POOL-THROW ( n -- ) {: code :}
   GT-POOL-KILL-ALL
   code throw ;

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
   -1 >FD idx GT-POOL-OUT-R-PTR !
   -1 >FD idx GT-POOL-OUT-W-PTR !
   -1 >FD idx GT-POOL-ERR-R-PTR !
   -1 >FD idx GT-POOL-ERR-W-PTR !
   0 idx GT-POOL-OUT-U-PTR !
   0 idx GT-POOL-ERR-U-PTR !
   PROC-OUTCOME-EXIT idx GT-POOL-KIND-PTR !
   0 idx GT-POOL-CODE-PTR !
   -1 idx GT-POOL-DONE-PTR ! ;

: GT-POOL-RESET ( -- )
   GT-POOL-ALLOC-BUFFERS
   GT-POOL-LIMIT-SELECT GT-POOL-LIMIT !
   0 GT-POOL-LIVE !
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

: GT-POOL-RUN-LINE ( idx -- ) {: idx :}
   ;

: GT-POOL-ELAPSED-MS ( idx -- n ) {: idx :}
   mono-ns idx GT-POOL-START-PTR @ - PROC-NS-PER-MS / ;

: GT-POOL-PASS-LINE ( idx -- ) {: idx :}
   s" PASS: " type
   idx GT-POOL-LABEL$ type
   s"  (" type
   idx GT-POOL-ELAPSED-MS GT-U-TYPE
   s" ms)" type cr
   idx GT-POOL-LABEL$ idx GT-POOL-ELAPSED-MS GT-POOL-PASS-HOOK ;

: GT-POOL-WAIT-DUE? ( idx -- bool ) {: idx :}
   mono-ns idx GT-POOL-LAST-PTR @ - PROC-NS-PER-MS / GT-HEARTBEAT-MS >= ;

: GT-POOL-WAIT-LINE ( idx -- ) {: idx :}
   idx GT-POOL-WAIT-DUE? if
      mono-ns idx GT-POOL-LAST-PTR !
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

: GT-POOL-SPAWN ( idx ptr u8 n -- ) {: idx:idx path:ptr pathu:n :}
   path pathu >LEN PROC-ARGV-CHECK-PATH
   path pathu >LEN PROC-ARGV-PREPARE {: pathz:ptr argv:ptr :}
   PROC-ENV-PREPARE {: envp:ptr :}
   pathz argv envp -1 >FD idx GT-POOL-OUT-W-PTR @ idx GT-POOL-ERR-W-PTR @
   PROC-SPAWN-ARGV-ENV-RAW {: pid:pid :}
   pid PID>N 0 < if
      idx path pathu pid GT-POOL-SPAWN-FAIL.
      PROC-ARGV-ENV-RESET
      E-PROC-SPAWN GT-POOL-THROW
   then
   PROC-ARGV-ENV-RESET
   pid idx GT-POOL-PID-PTR !
   idx GT-POOL-CLOSE-WRITES ;

: GT-POOL-FORK-EXIT ( n -- )
   s" " rot die ;

: GT-POOL-FORK-THROW ( n -- )
   s" fork worker throw" rot die ;

: GT-POOL-FORK-SETUP-FAIL ( -- )
   127 GT-POOL-FORK-EXIT ;

: GT-POOL-DUP2! ( fd n -- ) {: fd:fd dst:n :}
   fd FD>N dst dup2 dup 0 < if drop GT-POOL-FORK-SETUP-FAIL then drop ;

\ typed-local-lint: allow-bare-local - q keeps the forked worker quotation effect.
: GT-POOL-FORK-CHILD ( idx [ -- ] -- ) {: idx:idx q :}
   idx GT-POOL-CLOSE-READS
   idx GT-POOL-OUT-W-PTR @ 1 GT-POOL-DUP2!
   idx GT-POOL-ERR-W-PTR @ 2 GT-POOL-DUP2!
   idx GT-POOL-CLOSE-WRITES
   q catch {: rc:n :}
   rc 0= if 0 GT-POOL-FORK-EXIT then
   rc GT-POOL-FORK-THROW ;

\ typed-local-lint: allow-bare-local - q keeps the forked worker quotation effect.
: GT-POOL-FORK ( idx [ -- ] -- ) {: idx:idx q :}
   PROC-FORK-RAW {: pid:pid :}
   pid PID>N 0 < if E-PROC-SPAWN GT-POOL-THROW then
   pid PID>N 0= if idx q GT-POOL-FORK-CHILD then
   pid idx GT-POOL-PID-PTR !
   idx GT-POOL-CLOSE-WRITES ;

: GT-POOL-START-SLOT ( ptr u8 n ptr u8 n n idx -- ) {: path:ptr pathu label:ptr labelu timeout idx :}
   idx GT-POOL-DONE@ 0= if
      s" test pool: fixed slot already active" type cr
      E-TBL-FIELD GT-POOL-THROW
   then
   idx GT-POOL-RESET-SLOT
   0 idx GT-POOL-DONE-PTR !
   idx GT-POOL-PIPES
   label labelu idx GT-POOL-LABEL!
   mono-ns idx GT-POOL-START-PTR !
   idx GT-POOL-START-PTR @ idx GT-POOL-LAST-PTR !
   timeout idx GT-POOL-TIMEOUT-PTR !
   idx GT-POOL-RUN-LINE
   idx path pathu GT-POOL-SPAWN
   GT-POOL-LIVE @ 1+ GT-POOL-LIVE ! ;

\ typed-local-lint: allow-bare-local - q keeps the forked worker quotation effect.
: GT-POOL-START-FORK-SLOT ( ptr u8 n n idx [ -- ] -- ) {: label:ptr labelu:n timeout:n idx:idx q :}
   idx GT-POOL-DONE@ 0= if
      s" test pool: fixed slot already active" type cr
      E-TBL-FIELD GT-POOL-THROW
   then
   idx GT-POOL-RESET-SLOT
   0 idx GT-POOL-DONE-PTR !
   idx GT-POOL-PIPES
   label labelu idx GT-POOL-LABEL!
   mono-ns idx GT-POOL-START-PTR !
   idx GT-POOL-START-PTR @ idx GT-POOL-LAST-PTR !
   timeout idx GT-POOL-TIMEOUT-PTR !
   idx GT-POOL-RUN-LINE
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

: GT-POOL-POLL ( -- n )
   GT-POOL-POLL-BUILD
   GT-POOL-PFDS GT-POOL-MAX GT-POOL-FDS * GT-POOL-POLL-MS poll {: rc :}
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

: GT-POOL-PROBE-FULL ( ptr fd -- ) {: fdp:ptr :}
   fdp @ FD>N GT-POOL-PROBE 1 read GT-POOL-RD !
   GT-POOL-RD @ 0 < if E-PROC-OUTPUT GT-POOL-THROW then
   GT-POOL-RD @ 1 > if E-PROC-OUTPUT GT-POOL-THROW then
   GT-POOL-RD @ 0= if fdp GT-POOL-CLOSE-FD exit then
   E-PROC-TRUNCATED GT-POOL-THROW ;

: GT-POOL-READ-STREAM ( idx n -- ) {: idx stream :}
   idx stream GT-POOL-STREAM-FD-PTR {: fdp:ptr :}
   idx stream GT-POOL-STREAM-U-PTR {: up:ptr :}
   idx stream GT-POOL-STREAM-BUF {: buf:ptr :}
   stream GT-POOL-STREAM-CAP {: cap :}
   up @ 0 < if E-PROC-TRUNCATED GT-POOL-THROW then
   up @ cap > if E-PROC-TRUNCATED GT-POOL-THROW then
   cap up @ - 0 <= if fdp GT-POOL-PROBE-FULL exit then
   fdp @ FD>N buf up @ + cap up @ - read GT-POOL-RD !
   GT-POOL-RD @ 0 < if E-PROC-OUTPUT GT-POOL-THROW then
   GT-POOL-RD @ cap up @ - > if E-PROC-OUTPUT GT-POOL-THROW then
   GT-POOL-RD @ 0= if
      fdp GT-POOL-CLOSE-FD
   else
      up @ GT-POOL-RD @ + up !
   then ;

: GT-POOL-DRAIN-SLOT ( idx -- ) {: idx :}
   idx GT-POOL-OUT-SLOT GT-POOL-PFD-REVENTS 0 <> if idx 0 GT-POOL-READ-STREAM then
   idx GT-POOL-ERR-SLOT GT-POOL-PFD-REVENTS 0 <> if idx 1 GT-POOL-READ-STREAM then ;

: GT-POOL-CAPTURE-DONE? ( idx -- bool ) {: idx :}
   idx GT-POOL-OUT-R@ FD>N 0 < idx GT-POOL-ERR-R@ FD>N 0 < and ;

: GT-POOL-OK? ( idx -- bool ) {: idx :}
   idx GT-POOL-KIND-PTR @ PROC-OUTCOME-EXIT =
   idx GT-POOL-CODE-PTR @ 0= and ;

: GT-POOL-OUTPUT ( idx -- ) {: idx :}
   idx GT-POOL-OUT-BUF idx GT-POOL-OUT-U-PTR @ type
   idx GT-POOL-ERR-BUF idx GT-POOL-ERR-U-PTR @ type ;

: GT-POOL-OUTCOME-LINE ( idx -- ) {: idx :}
   s" outcome kind: " type idx GT-POOL-KIND-PTR @ .
   s" code: " type idx GT-POOL-CODE-PTR @ . cr ;

: GT-POOL-FAIL ( idx -- ) {: idx :}
   idx GT-POOL-OUTPUT
   idx GT-POOL-OUTCOME-LINE
   GT-POOL-KILL-ALL
   s" FAIL: " type idx GT-POOL-LABEL$ type cr
   s" test pool phase failed" 1 die ;

: GT-POOL-REAP ( idx -- ) {: idx :}
   idx GT-POOL-PID@ PROC-WAIT-OUTCOME idx GT-POOL-CODE-PTR ! idx GT-POOL-KIND-PTR !
   -1 >PID idx GT-POOL-PID-PTR !
   1 idx GT-POOL-DONE-PTR !
   GT-POOL-LIVE @ 1- GT-POOL-LIVE !
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
   PROC-OUTCOME-TIMEOUT idx GT-POOL-KIND-PTR !
   SIGKILL idx GT-POOL-CODE-PTR !
   idx GT-POOL-OUTPUT
   idx GT-POOL-OUTCOME-LINE
   GT-POOL-KILL-ALL
   s" FAIL: " type idx GT-POOL-LABEL$ type cr
   s" test pool phase timed out" 1 die ;

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

: GT-POOL-STEP ( -- )
   GT-POOL-POLL drop
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

\ typed-local-lint: allow-bare-local - q keeps the forked worker quotation effect.
: GT-POOL-START-FORK ( ptr u8 n n [ -- ] -- ) {: label:ptr labelu:n timeout:n q :}
   GT-POOL-WAIT-FREE
   GT-POOL-FIND-FREE {: idx:idx :}
   label labelu timeout idx q GT-POOL-START-FORK-SLOT ;

: GT-POOL-DRAIN ( -- )
   begin GT-POOL-LIVE @ 0 > while
      GT-POOL-STEP
   repeat ;
