\ gate-pool.f - checked bounded process pool for native tests.
\
\ Load after lib/process-env.f and lib/test/runner.f.

require lib/process-fork.f
require test/gate-stats.f
require tools/why-threw.f

16 constant GT-POOL-MAX
6 constant GT-POOL-LINUX-DEFAULT
8 constant GT-POOL-MACOS-DEFAULT
2 constant GT-POOL-FDS
8 constant GT-PFD-SZ
$64 constant GT-POOL-POLL-MS
$1000 constant GT-POOL-CHUNK-CAP
64 constant GT-POOL-NAME-CAP
32 constant GT-POOL-NUM-CAP
16 constant GT-POOL-RED-MAX
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
create GT-POOL-KINDS GT-POOL-MAX cells allot
create GT-POOL-CODES GT-POOL-MAX cells allot
create GT-POOL-DONES GT-POOL-MAX cells allot
create GT-POOL-STARTS GT-POOL-MAX cells allot
create GT-POOL-LASTS GT-POOL-MAX cells allot
create GT-POOL-TIMEOUTS GT-POOL-MAX cells allot
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
create GT-POOL-OUT-FDS GT-POOL-MAX cells allot
create GT-POOL-ERR-FDS GT-POOL-MAX cells allot
create GT-POOL-OUT-TOTALS GT-POOL-MAX cells allot
create GT-POOL-ERR-TOTALS GT-POOL-MAX cells allot
create GT-POOL-SEQS GT-POOL-MAX cells allot
create GT-POOL-RED-LABELS GT-POOL-RED-MAX GT-FAIL-NAME-CAP * allot
create GT-POOL-RED-LABEL-US GT-POOL-RED-MAX cells allot
create GT-POOL-RED-KINDS GT-POOL-RED-MAX cells allot
create GT-POOL-RED-CODES GT-POOL-RED-MAX cells allot
create GT-POOL-RED-OUT-PATHS GT-POOL-RED-MAX FS-PATH-CAP * allot
create GT-POOL-RED-OUT-US GT-POOL-RED-MAX cells allot
create GT-POOL-RED-ERR-PATHS GT-POOL-RED-MAX FS-PATH-CAP * allot
create GT-POOL-RED-ERR-US GT-POOL-RED-MAX cells allot

variable GT-POOL-OUT-BUFS-A
variable GT-POOL-ERR-BUFS-A
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
variable GT-POOL-FALLBACK-U
variable GT-POOL-DEATH-RD
variable GT-POOL-DEATH-WR
variable GT-POOL-DEATH-MADE

: GT-POOL-RED# ( -- n )
   GT-POOL-RED-N @ ;

: GT-POOL-RED-RESET ( -- )
   0 GT-POOL-RED-N ! ;

: GT-POOL-NO-PASS-HOOK ( ptr u8 n n -- )
   drop 2drop ;

\ Receives ( label labelu ms ) for every passing slot. Hooks that record the
\ span must emit through GS-SPAN-AUTH: the pool owns the slot, so its span is
\ authoritative and must bypass the fork child's self-suppression.
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
   GS-GEN$ GT-POOL-NAME+
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

: GT-POOL-OUT-PATH-BUF ( idx -- ptr u8 )
   IDX>N FS-PATH-CAP * GT-POOL-OUT-PATHS + ;

: GT-POOL-ERR-PATH-BUF ( idx -- ptr u8 )
   IDX>N FS-PATH-CAP * GT-POOL-ERR-PATHS + ;

: GT-POOL-OUT-PATH-U-PTR ( idx -- ptr n )
   IDX>N cells GT-POOL-OUT-PATH-US + ;

: GT-POOL-ERR-PATH-U-PTR ( idx -- ptr n )
   IDX>N cells GT-POOL-ERR-PATH-US + ;

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

: GT-POOL-KILL-SLOT ( idx -- ) {: idx :}
   idx GT-POOL-PID@ PID>N 0 >= if
      idx GT-POOL-PID@ SIGKILL PROC-KILL-GROUP drop
      idx GT-POOL-PID@ SIGKILL PROC-KILL-RAW drop
      idx GT-POOL-PID@ PROC-WAIT-STATUS drop
      -1 >PID idx GT-POOL-PID-PTR !
   then
   idx GT-POOL-KILL-REAPER
   idx GT-POOL-CLOSE-WRITES
   idx GT-POOL-CLOSE-READS
   idx GT-POOL-CLOSE-CAPTURE ;

: GT-POOL-KILL-ALL ( -- )
   0 begin dup GT-POOL-MAX < while
      dup >IDX GT-POOL-DONE@ 0= if dup >IDX GT-POOL-KILL-SLOT then
      1+
   repeat drop ;

: GT-POOL-THROW ( n -- ) {: code :}
   GT-POOL-KILL-ALL
   code throw ;

\ Upgrade the name-builder abort: once GT-POOL-THROW exists, a build-name
\ overflow with live children kills the pool instead of leaking orphans.
: GT-POOL-ABORT-KILL! ( -- ) [: GT-POOL-THROW ;] is GT-POOL-ABORT ;
GT-POOL-ABORT-KILL!

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
   0 idx GT-POOL-SEQ-PTR !
   PROC-OUTCOME-EXIT idx GT-POOL-KIND-PTR !
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
   PROC-DEATH-PIPE {: rd:fd wr:fd :}
   rd FD>N GT-POOL-DEATH-RD !
   wr FD>N GT-POOL-DEATH-WR !
   -1 GT-POOL-DEATH-MADE ! ;

: GT-POOL-RESET ( -- )
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

\ Arm a co-located reaper for the just-spawned child. The spawn primitive makes
\ every child its own process-group leader before exec, so timeout and
\ parent-death cleanup can signal the whole subtree without a parent-side
\ setpgid race.
\
\ Fork a reaper that joins the child's group and watches the pool-death read end,
\ then track its pid so GT-POOL-REAP/KILL-SLOT reap it. Reaper fork failure is
\ also fail-closed; otherwise a spawned subtree could survive a killed pool.
: GT-POOL-ARM-SPAWN-REAPER ( idx -- ) {: idx:idx :}
   GT-POOL-DEATH-RD@ idx GT-POOL-PID@ PROC-SPAWN-REAPER {: rpid:pid :}
   rpid PID>N 0 < if E-PROC-SPAWN GT-POOL-THROW then
   rpid idx GT-POOL-REAPER-PID-PTR ! ;

\ Slot generation: the inherited process generation extended with this slot's
\ seq, built in the shared name buffer.
: GT-POOL-SLOT-GEN$ ( idx -- ptr u8 n ) {: idx:idx :}
   GT-POOL-NAME-RESET
   GS-GEN$ GT-POOL-NAME+
   s" -" GT-POOL-NAME+
   idx GT-POOL-SEQ-PTR @ GT-POOL-NUM$ GT-POOL-NAME+
   GT-POOL-NAME$ ;

: GT-POOL-SPAWN ( idx ptr u8 n -- ) {: idx:idx path:ptr pathu:n :}
   \ A spawned child cannot inherit this process's in-memory generation, so
   \ export the slot generation; GS-GEN-INIT adopts it at load. PROC-ENV-SET
   \ replaces any row PROC-ENV-INHERIT-MISSING copied from this process's own
   \ environment, so exactly one HABU_GATE_GEN reaches the child.
   s" HABU_GATE_GEN" >LEN idx GT-POOL-SLOT-GEN$ >LEN PROC-ENV-SET
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
   idx GT-POOL-CLOSE-WRITES
   idx GT-POOL-ARM-SPAWN-REAPER ;

\ Fork-child generation: adopt this slot's generation in place, so every label
\ this child stores or emits is qualified with its own identity path.
: GT-POOL-GEN-CHILD! ( idx -- )
   GT-POOL-SLOT-GEN$ GS-GEN! ;

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
   0 >PID 0 >PID PROC-SETPGID RC>N 0 < if GT-POOL-FORK-SETUP-FAIL then ;

\ Worker side: open a worker-alive pipe, arm the parent-death reaper watching
\ both the pool parent's death pipe (RD) and this worker-alive pipe, then drop
\ every copy the worker no longer needs: the inherited parent death-pipe RD/WR.
\ The worker keeps the worker-alive WR open (an untracked fd, close-on-exec) so
\ it closes exactly at this worker's exit and the reaper self-exits with no
\ orphan -- and it keeps the worker-alive RD published as PROC-REAP-WATCH-FD,
\ so every capture child this worker spawns (PROC-RUN-CAPTURE family) gets its
\ own co-located reaper watching this worker's life. Extra RD copies never
\ delay the EOF (only write ends count) and both ends are close-on-exec. The
\ made-flag is cleared so a nested pool this worker later drives builds its own
\ death pipe. The reaper is a reparented grandchild (see PROC-FORK-REAPER),
\ never a child of this worker, so the worker body's wait(-1) still sees no
\ children.
: GT-POOL-ARM-REAPER ( -- )
   PROC-DEATH-PIPE {: wa-rd:fd wa-wr:fd :}
   GT-POOL-DEATH-RD@ wa-rd PROC-FORK-REAPER
   GT-POOL-DEATH-RD@ FD>N close
   GT-POOL-DEATH-WR@ FD>N close
   wa-rd FD>N PROC-REAP-WATCH-FD !
   0 GT-POOL-DEATH-MADE ! ;

\ typed-local-lint: allow-bare-local - q keeps the forked worker quotation effect.
: GT-POOL-FORK-CHILD ( idx [ -- ] -- ) {: idx:idx q :}
   idx GT-POOL-CLOSE-READS
   idx GT-POOL-OUT-W-PTR @ 1 GT-POOL-DUP2!
   idx GT-POOL-ERR-W-PTR @ 2 GT-POOL-DUP2!
   idx GT-POOL-CLOSE-WRITES
   idx GT-POOL-CLOSE-CAPTURE
   GT-POOL-SETPGID-SELF
   GT-POOL-ARM-REAPER
   idx GT-POOL-GEN-CHILD!
   GT-POOL-RED-RESET
   idx GT-POOL-LABEL$ GS-CHILD-LABEL!
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

: GT-POOL-RED-CHECK ( n -- n ) {: i:n :}
   i 0 < if E-TBL-BOUNDS throw then
   i GT-POOL-RED-MAX >= if E-TBL-BOUNDS throw then
   i ;

: GT-POOL-RED-LABEL-BUF ( n -- ptr u8 )
   GT-POOL-RED-CHECK GT-FAIL-NAME-CAP * GT-POOL-RED-LABELS + ;

: GT-POOL-RED-LABEL-U-PTR ( n -- ptr n )
   GT-POOL-RED-CHECK cells GT-POOL-RED-LABEL-US + ;

: GT-POOL-RED-KIND-PTR ( n -- ptr n )
   GT-POOL-RED-CHECK cells GT-POOL-RED-KINDS + ;

: GT-POOL-RED-CODE-PTR ( n -- ptr n )
   GT-POOL-RED-CHECK cells GT-POOL-RED-CODES + ;

: GT-POOL-RED-OUT-BUF ( n -- ptr u8 )
   GT-POOL-RED-CHECK FS-PATH-CAP * GT-POOL-RED-OUT-PATHS + ;

: GT-POOL-RED-OUT-U-PTR ( n -- ptr n )
   GT-POOL-RED-CHECK cells GT-POOL-RED-OUT-US + ;

: GT-POOL-RED-ERR-BUF ( n -- ptr u8 )
   GT-POOL-RED-CHECK FS-PATH-CAP * GT-POOL-RED-ERR-PATHS + ;

: GT-POOL-RED-ERR-U-PTR ( n -- ptr n )
   GT-POOL-RED-CHECK cells GT-POOL-RED-ERR-US + ;

: GT-POOL-RED-LABEL$ ( n -- ptr u8 n ) {: i:n :}
   i GT-POOL-RED-LABEL-BUF i GT-POOL-RED-LABEL-U-PTR @ ;

: GT-POOL-RED-OUT$ ( n -- ptr u8 n ) {: i:n :}
   i GT-POOL-RED-OUT-BUF i GT-POOL-RED-OUT-U-PTR @ ;

: GT-POOL-RED-ERR$ ( n -- ptr u8 n ) {: i:n :}
   i GT-POOL-RED-ERR-BUF i GT-POOL-RED-ERR-U-PTR @ ;

: GT-POOL-RED-COPY$ ( ptr u8 n ptr u8 ptr n n -- ) {: a:ptr u:n dst:ptr up:ptr cap:n :}
   u 0 < if E-TBL-FIELD throw then
   u cap > if E-TBL-FIELD throw then
   a dst u BYTE-COPY
   u up ! ;

: GT-POOL-RED-STORE ( n idx -- ) {: i:n idx:idx :}
   idx GT-POOL-LABEL$ i GT-POOL-RED-LABEL-BUF i GT-POOL-RED-LABEL-U-PTR GT-FAIL-NAME-CAP GT-POOL-RED-COPY$
   idx GT-POOL-KIND-PTR @ i GT-POOL-RED-KIND-PTR !
   idx GT-POOL-CODE-PTR @ i GT-POOL-RED-CODE-PTR !
   idx 0 GT-POOL-STREAM-FILE$ i GT-POOL-RED-OUT-BUF i GT-POOL-RED-OUT-U-PTR FS-PATH-CAP GT-POOL-RED-COPY$
   idx 1 GT-POOL-STREAM-FILE$ i GT-POOL-RED-ERR-BUF i GT-POOL-RED-ERR-U-PTR FS-PATH-CAP GT-POOL-RED-COPY$ ;

: GT-POOL-RED+ ( idx -- ) {: idx:idx :}
   GT-POOL-RED-N @ GT-POOL-RED-MAX < if
      GT-POOL-RED-N @ idx GT-POOL-RED-STORE
   then
   GT-POOL-RED-N @ 1+ GT-POOL-RED-N ! ;

: GT-POOL-RED-DETAILED ( -- n )
   GT-POOL-RED# dup GT-POOL-RED-MAX > if drop GT-POOL-RED-MAX then ;

: GT-POOL-RED-LINE ( n -- ) {: i:n :}
   s" RED: " type i GT-POOL-RED-LABEL$ type
   s"  kind=" type i GT-POOL-RED-KIND-PTR @ GT-POOL-N-TYPE
   s"  code=" type i GT-POOL-RED-CODE-PTR @ GT-POOL-N-TYPE
   s"  out=" type i GT-POOL-RED-OUT$ type
   s"  err=" type i GT-POOL-RED-ERR$ type cr ;

: GT-POOL-RED-OVERFLOW-LINE ( -- )
   GT-POOL-RED# GT-POOL-RED-MAX > if
      s" RED: +" type GT-POOL-RED# GT-POOL-RED-MAX - GT-POOL-N-TYPE
      s"  more failed phases (details not recorded)" type cr
   then ;

: GT-POOL-RED-REPORT ( -- )
   GT-POOL-RED# 0= if exit then
   s" red phases: " type GT-POOL-RED# GT-POOL-N-TYPE cr
   0 begin dup GT-POOL-RED-DETAILED < while
      dup GT-POOL-RED-LINE
      1+
   repeat drop
   GT-POOL-RED-OVERFLOW-LINE ;

: GT-POOL-START-SLOT ( ptr u8 n ptr u8 n n idx -- ) {: path:ptr pathu label:ptr labelu timeout idx :}
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
   idx GT-POOL-CAPTURE-START
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
   idx GT-POOL-KIND-PTR @ PROC-OUTCOME-EXIT =
   idx GT-POOL-CODE-PTR @ 0= and ;

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
   s" outcome kind: " type idx GT-POOL-KIND-PTR @ .
   s" code: " type idx GT-POOL-CODE-PTR @ . cr ;

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
   idx GT-POOL-PID@ PROC-WAIT-OUTCOME PROC-OUTCOME-PAIR idx GT-POOL-CODE-PTR ! idx GT-POOL-KIND-PTR !
   -1 >PID idx GT-POOL-PID-PTR !
   idx GT-POOL-KILL-REAPER
   idx GT-POOL-CLOSE-CAPTURE
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
   \ Final bounded poll+drain so the last bytes the worker wrote (often the
   \ hang clue) reach the tail and capture file before the fds are closed.
   GT-POOL-POLL-BUILD  GT-POOL-POLL drop  idx GT-POOL-DRAIN-SLOT
   idx GT-POOL-KILL-SLOT
   1 idx GT-POOL-DONE-PTR !
   GT-POOL-LIVE @ 1- GT-POOL-LIVE !
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

: GT-POOL-DRAIN-SOFT ( -- )
   begin GT-POOL-LIVE @ 0 > while
      GT-POOL-STEP
   repeat ;

: GT-POOL-RED-DIE ( -- )
   GT-POOL-RED-REPORT
   s" test pool phases failed" 1 die ;

: GT-POOL-DRAIN ( -- )
   GT-POOL-DRAIN-SOFT
   GT-POOL-RED# 0 > if GT-POOL-RED-DIE then ;
