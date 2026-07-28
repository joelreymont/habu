\ gate-pool-test.f - focused coverage for fork-backed test pool workers.
\ Run: bin/hb --load test/gate-pool-test.f

require lib/errors.f
require lib/string.f
require lib/adt/option.f                 \ option<CAD-NUM:index> STR:INDEX-OF consumer (switchover wave A)
require lib/test.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/process-fork.f
require lib/test/runner.f
require test/gate-pool.f
require test/run-lib.f

\ White-box CAD-NUM role reader (precedent: lib/string-test.f STR-T-IX>RAW):
\ reopen the unsealed CAD-NUM package to project the typed STR:FIND-SUB /
\ STR:INDEX-OF index back to its raw cell, keeping GPT-KR-PARSE-ROOT
\ byte-identical. A plain checked word over the audited private INDEX>N
\ projection - not a new boundary.
package CAD-NUM
public
: GPT-IX>RAW ( CAD-NUM:index -- n ) INDEX>N ;
;package

package GATE-POOL-TEST
private

$20000 constant GPT-CAP
$2710 constant GPT-TIMEOUT-MS
250 constant GPT-HANG-TIMEOUT-MS
$100 constant GPT-BIG-CHUNK
400 constant GPT-BIG-N
GPT-BIG-CHUNK GPT-BIG-N * constant GPT-BIG-BYTES
160 constant GPT-SPEW-N
12 constant GPT-OVERFLOW-N

create GPT-OUT GPT-CAP allot
create GPT-ERR GPT-CAP allot
create GPT-BIG-BUF GPT-BIG-CHUNK allot
create GPT-GS-SAVE FS-PATH-CAP allot
create GPT-ROOT-SAVE FS-PATH-CAP allot
create GPT-KR-ROOT FS-PATH-CAP allot
create GPT-KR-LOG FS-PATH-CAP allot

variable GPT-COW
variable GPT-BIG-A
variable GPT-GS-SAVE-U
variable GPT-COUNT-N
variable GPT-SPAN-RC
variable GPT-ROOT-SAVE-U
variable GPT-KR-ROOT-U
variable GPT-KR-LOG-U

: GPT-BIG-A-FIELD ( -- ptr ptr u8 )
   GPT-BIG-A 0 ptr-field ;

: GPT-HB$ ( -- ptr u8 n )
   s" HABU_UNDER_TEST" GETENV dup 0= if
      2drop s" bin/hb" exit
   then ;

: GPT-WORKER ( -- )
   7 GPT-COW !
   s" gate-pool fork worker" type cr ;

: GPT-BIG-FILL ( -- )
   GPT-BIG-CHUNK 0 ?do
      $41 GPT-BIG-BUF i + c!
   loop ;

: GPT-BIG-WORKER ( -- )
   GPT-BIG-FILL
   GPT-BIG-N 0 ?do
      GPT-BIG-BUF GPT-BIG-CHUNK type
   loop ;

: GPT-FAIL-SPEW ( -- )
   GPT-BIG-FILL
   GPT-SPEW-N 0 ?do
      GPT-BIG-BUF GPT-BIG-CHUNK type
   loop ;

: GPT-FAIL-WORKER ( -- )
   GPT-FAIL-SPEW
   s" gate-pool failing worker" type cr
   77 throw ;

: GPT-SOFT-ONE ( -- )
   s" gate-pool soft one" type cr
   77 throw ;

: GPT-SOFT-TWO ( -- )
   s" gate-pool soft two" type cr
   78 throw ;

: GPT-WRAP-WORKER ( -- )
   s" gate-pool wrap worker" type cr
   256 throw ;

\ Sentinel the hang worker writes as its final bytes just before spinning, so
\ the timeout battery can prove GT-POOL-TIMEOUT's final poll+drain captured the
\ last output the worker produced before it was killed.
: GPT-HANG-SENTINEL$ ( -- ptr u8 n )
   s" hang-sentinel-xyzzy" ;

: GPT-HANG-WORKER ( -- )
   s" gate-pool hang worker" type cr
   GPT-HANG-SENTINEL$ type
   begin 0 0= 0= until ;

: GPT-BATTERY-TRUNC ( -- )
   s" fork failing worker" GPT-TIMEOUT-MS [: GPT-FAIL-WORKER ;] GT-POOL-START-FORK
   GT-POOL-DRAIN-SOFT ;

: GPT-BATTERY-SOFT ( -- )
   s" soft fail one" GPT-TIMEOUT-MS [: GPT-SOFT-ONE ;] GT-POOL-START-FORK
   s" soft fail two" GPT-TIMEOUT-MS [: GPT-SOFT-TWO ;] GT-POOL-START-FORK
   s" soft pass" GPT-TIMEOUT-MS [: GPT-WORKER ;] GT-POOL-START-FORK
   GT-POOL-DRAIN-SOFT ;

: GPT-BATTERY-WRAP ( -- )
   s" wrap throw worker" GPT-TIMEOUT-MS [: GPT-WRAP-WORKER ;] GT-POOL-START-FORK
   GT-POOL-DRAIN-SOFT ;

: GPT-BATTERY-TIMEOUT ( -- )
   s" hang worker" GPT-HANG-TIMEOUT-MS [: GPT-HANG-WORKER ;] GT-POOL-START-FORK
   s" quick worker" GPT-TIMEOUT-MS [: GPT-WORKER ;] GT-POOL-START-FORK
   GT-POOL-DRAIN-SOFT ;

: GPT-BATTERY-OVERFLOW ( -- )
   GPT-OVERFLOW-N 0 ?do
      s" soft overflow" GPT-TIMEOUT-MS [: GPT-SOFT-ONE ;] GT-POOL-START-FORK
   loop ;

\ One child process runs every failure class; reds accumulate to 17 so the
\ final fail-closed drain also proves red-list overflow reporting.
: GPT-BATTERY-CASE ( -- )
   s" gate-pool-test-battery" GT-START
   2 GT-POOL-SLOTS!
   GT-POOL-RESET
   GPT-BATTERY-TRUNC
   GPT-BATTERY-SOFT
   GPT-BATTERY-WRAP
   GPT-BATTERY-TIMEOUT
   GPT-BATTERY-OVERFLOW
   GT-POOL-DRAIN ;

: GPT-MODE? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   SCRIPT-ARGC 0 > if 0 SCRIPT-ARGV$ a u STR= exit then
   0 0= 0= ;

: GPT-CAPTURE>N ( result<pcap:captured,pcap:failed> -- n n n )   \ outn errn code (0 on clean exit)
   MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE {: o:len e:len :} o LEN>N e LEN>N 0 ENDOF
     err OF PCAP-FAILED:UNMAKE  {: o:len e:len c:rc :} o LEN>N e LEN>N c RC>N ENDOF
   ;MATCH ;

: GPT-MODE-CAPTURE ( ptr u8 n -- n n n ) {: mode:ptr modeu:n :}
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   s" test/gate-pool-test.f" >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   mode modeu >LEN PROC-ARGV+
   GPT-HB$ >LEN GPT-OUT GPT-CAP >LEN GPT-ERR GPT-CAP >LEN
   GPT-TIMEOUT-MS >MS RUN-ARGV-CAPTURE
   GPT-CAPTURE>N ;

: GPT-BATTERY-CAPTURE ( -- n n n )
   s" fail-battery-case" GPT-MODE-CAPTURE ;

: GPT-COUNT$ ( ptr u8 n ptr u8 n -- n ) {: a:ptr u:n needle:ptr nu:n :}
   0 GPT-COUNT-N !
   nu 0 <= if 0 exit then
   0 begin dup nu + u <= while
      a over BYTE+ nu needle nu STR= if
         GPT-COUNT-N @ 1+ GPT-COUNT-N !
      then
      1+
   repeat drop
   GPT-COUNT-N @ ;

;package

package GATE-POOL-TEST

variable ROW-I
variable ROW-START

: SPAN-ROW? ( ptr u8 n ptr u8 n -- bool )
   {: row:ptr rowu:n label:ptr labelu:n :}
   rowu 5 < if GS-FALSE exit then
   row 4 s" span" STR= 0= if GS-FALSE exit then
   row 4 BYTE+ c@ GS-TAB <> if GS-FALSE exit then
   row rowu label labelu CONTAINS? ;

public

: SPAN-COUNT$ ( ptr u8 n ptr u8 n -- n )
   {: a:ptr u:n label:ptr labelu:n :}
   0 GPT-COUNT-N !
   0 ROW-START !
   0 ROW-I !
   begin ROW-I @ u <= while
      ROW-I @ u = if
         a ROW-START @ BYTE+ ROW-I @ ROW-START @ -
         label labelu SPAN-ROW? if GPT-COUNT-N @ 1+ GPT-COUNT-N ! then
      else
         a ROW-I @ BYTE+ c@ GS-LF = if
            a ROW-START @ BYTE+ ROW-I @ ROW-START @ -
            label labelu SPAN-ROW? if GPT-COUNT-N @ 1+ GPT-COUNT-N ! then
            ROW-I @ 1+ ROW-START !
         then
      then
      ROW-I @ 1+ ROW-I !
   repeat
   GPT-COUNT-N @ ;

;package

package GATE-POOL-TEST
private

: GPT-EXPECT-TRUNC-OUT ( n -- ) {: outu:n :}
   GPT-OUT outu s" gate-pool failing worker" CONTAINS? TTRUE
   GPT-OUT outu s" [tail truncated " CONTAINS? TTRUE
   GPT-OUT outu s" outcome: exit" CONTAINS? TTRUE
   GPT-OUT outu s" fork worker throw rc 77" CONTAINS? TTRUE
   GPT-OUT outu s" code: 1" CONTAINS? TTRUE
   GPT-OUT outu s" stdout-file: " CONTAINS? TTRUE
   GPT-OUT outu s" stderr-file: " CONTAINS? TTRUE
   GPT-OUT outu s" -out.log" CONTAINS? TTRUE
   GPT-OUT outu s" RED: fork failing worker" CONTAINS? TTRUE
   GPT-OUT outu s" FAIL: fork failing worker" CONTAINS? TTRUE ;

: GPT-EXPECT-SOFT-OUT ( n -- ) {: outu:n :}
   GPT-OUT outu s" gate-pool soft one" CONTAINS? TTRUE
   GPT-OUT outu s" gate-pool soft two" CONTAINS? TTRUE
   GPT-OUT outu s" FAIL: soft fail one" CONTAINS? TTRUE
   GPT-OUT outu s" FAIL: soft fail two" CONTAINS? TTRUE
   GPT-OUT outu s" PASS: soft pass" CONTAINS? TTRUE
   GPT-OUT outu s" fork worker throw rc 78" CONTAINS? TTRUE
   GPT-OUT outu s" RED: soft fail one" CONTAINS? TTRUE
   GPT-OUT outu s" RED: soft fail two" CONTAINS? TTRUE
   GPT-OUT outu s" RED: soft pass" CONTAINS? TFALSE ;

: GPT-EXPECT-WRAP-OUT ( n -- ) {: outu:n :}
   GPT-OUT outu s" gate-pool wrap worker" CONTAINS? TTRUE
   GPT-OUT outu s" fork worker throw rc 256" CONTAINS? TTRUE
   GPT-OUT outu s" RED: wrap throw worker" CONTAINS? TTRUE ;

: GPT-EXPECT-TIMEOUT-OUT ( n -- ) {: outu:n :}
   GPT-OUT outu s" gate-pool hang worker" CONTAINS? TTRUE
   GPT-OUT outu GPT-HANG-SENTINEL$ CONTAINS? TTRUE
   GPT-OUT outu s" outcome: TIMEOUT-UNDER-LOAD code: 0" CONTAINS? TTRUE
   GPT-OUT outu s" RED: hang worker kind=TIMEOUT-UNDER-LOAD code=0" CONTAINS? TTRUE
   GPT-OUT outu s" sat=" CONTAINS? TTRUE
   GPT-OUT outu s" waits=" CONTAINS? TTRUE
   GPT-OUT outu s" ran=" CONTAINS? TTRUE
   GPT-OUT outu s" PASS: quick worker" CONTAINS? TTRUE ;

: GPT-EXPECT-OVERFLOW-OUT ( n -- ) {: outu:n :}
   GPT-OUT outu s" red phases: 17" CONTAINS? TTRUE
   GPT-OUT outu s" RED: +1 more failed phases" CONTAINS? TTRUE
   GPT-OUT outu s" RED: soft overflow" GPT-COUNT$ 11 T= ;

: GPT-EXPECT-BATTERY-ERR ( n -- ) {: erru:n :}
   GPT-ERR erru s" test pool phases failed" CONTAINS? TTRUE ;

: GPT-BATTERY-REPORT ( -- )
   GPT-BATTERY-CAPTURE 1 T=
   {: outu:n erru:n :}
   outu GPT-EXPECT-TRUNC-OUT
   outu GPT-EXPECT-SOFT-OUT
   outu GPT-EXPECT-WRAP-OUT
   outu GPT-EXPECT-TIMEOUT-OUT
   outu GPT-EXPECT-OVERFLOW-OUT
   erru GPT-EXPECT-BATTERY-ERR ;

: GPT-BIG-ALLOC ( -- )
   GPT-BIG-A-FIELD @ 0= if
      GPT-BIG-BYTES $400 + GT-POOL-ALLOC-BYTES GPT-BIG-A-FIELD !
   then ;

: GPT-BIG-FILE-LEN ( -- n )
   0 >IDX GT-POOL-OUT-FILE$ GPT-BIG-A-FIELD @ GPT-BIG-BYTES $400 + READ-ALL ;

: GPT-BIG-EXPECT ( -- )
   0 >IDX GT-POOL-OUT-U-PTR @ GT-OUT-CAP T=
   0 >IDX GT-POOL-OUT-TOTAL-PTR @ GPT-BIG-BYTES T=
   0 >IDX GT-POOL-OUT-FILE$ FILE? TTRUE
   GPT-BIG-FILE-LEN GPT-BIG-BYTES T=
   GPT-BIG-A-FIELD @ c@ $41 T=
   GPT-BIG-A-FIELD @ GPT-BIG-BYTES 1- BYTE+ c@ $41 T=
   0 >IDX GT-POOL-OUT-BUF c@ $41 T= ;

: GPT-BIG-CASE ( -- )
   GPT-BIG-ALLOC
   1 GT-POOL-SLOTS!
   GT-POOL-RESET
   s" fork big output" GPT-TIMEOUT-MS [: GPT-BIG-WORKER ;] GT-POOL-START-FORK
   GT-POOL-DRAIN
   GPT-BIG-EXPECT ;

: GPT-GS-SAVE! ( -- )
   GS-ENSURE
   GS-PATH$ {: a:ptr u:n :}
   u FS-PATH-CAP > if E-FS-PATH throw then
   a GPT-GS-SAVE u BYTE-COPY
   u GPT-GS-SAVE-U ! ;

: GPT-GS-RESTORE ( -- )
   GPT-GS-SAVE GPT-GS-SAVE-U @ GS-COPY-PATH!
   -1 GS-INITED ! ;

: GPT-SPAN-HOOK ( ptr u8 n n -- ) {: label:ptr labelu:n ms:n :}
   label labelu ms GS-SPAN-AUTH ;

: GPT-SPAN-CHILD ( -- )
   s" fork span child" 7 GS-SPAN
   s" fork span inner" 9 GS-SPAN ;

: GPT-SPAN-CASE-BODY ( -- )
   GT-ROOT GS-ROOT!
   [: GPT-SPAN-HOOK ;] is GT-POOL-PASS-HOOK
   1 GT-POOL-SLOTS!
   GT-POOL-RESET
   s" fork span child" GPT-TIMEOUT-MS [: GPT-SPAN-CHILD ;] GT-POOL-START-FORK
   GT-POOL-DRAIN
   GS-PATH$ GPT-OUT GPT-CAP READ-ALL {: n:n :}
   GPT-OUT n s" fork span child" GATE-POOL-TEST:SPAN-COUNT$ 1 T=
   GPT-OUT n s" process-fork" s" fork span child" GATE-PROCESS:ROW-COUNT$ 3 T=
   GPT-OUT n s" fork span inner" GPT-COUNT$ 1 T= ;

: GPT-SPAN-CASE ( -- )
   GPT-GS-SAVE!
   [: GPT-SPAN-CASE-BODY ;] catch GPT-SPAN-RC !
   GPT-GS-RESTORE
   GT-POOL-PASS-HOOK-DEFAULT!
   GPT-SPAN-RC @ 0 <> if GPT-SPAN-RC @ throw then ;

\ Pin the other real-fork shape: a worker that emits SEVERAL sibling completion
\ spans, none of which is its own slot label. This is the GSI-LINT-TOOLS-STATUS
\ pattern (four GSI-RUN/GSI-INCLUDE spans - repl-lint, trust-lint,
\ stale-status-lint, gate-stats-test.f - none equal to the "lint-tools/status"
\ pool label). The parent pass-hook is the ONLY emitter of the slot label; the
\ child never emits it, so GS-CHILD-OWNED? suppresses nothing and nothing is
\ double-counted. Because these sub-spans sit at the same fork depth as the
\ single self-completion span in GPT-SPAN-CASE, no label-free structural rule
\ can suppress the self-completion there yet keep every sub-span here - the
\ evidence that this dot's design (b) is unsound.
: GPT-SPAN-MULTI-CHILD ( -- )
   s" fork sub one" 7 GS-SPAN
   s" fork sub two" 9 GS-SPAN
   s" fork sub three" 11 GS-SPAN ;

: GPT-SPAN-MULTI-CASE-BODY ( -- )
   GT-ROOT GS-ROOT!
   [: GPT-SPAN-HOOK ;] is GT-POOL-PASS-HOOK
   1 GT-POOL-SLOTS!
   GT-POOL-RESET
   s" fork multi parent" GPT-TIMEOUT-MS [: GPT-SPAN-MULTI-CHILD ;] GT-POOL-START-FORK
   GT-POOL-DRAIN
   GS-PATH$ GPT-OUT GPT-CAP READ-ALL {: n:n :}
   GPT-OUT n s" fork multi parent" GATE-POOL-TEST:SPAN-COUNT$ 1 T=
   GPT-OUT n s" process-fork" s" fork multi parent" GATE-PROCESS:ROW-COUNT$ 3 T=
   GPT-OUT n s" fork sub one" GPT-COUNT$ 1 T=
   GPT-OUT n s" fork sub two" GPT-COUNT$ 1 T=
   GPT-OUT n s" fork sub three" GPT-COUNT$ 1 T= ;

: GPT-SPAN-MULTI-CASE ( -- )
   GPT-GS-SAVE!
   [: GPT-SPAN-MULTI-CASE-BODY ;] catch GPT-SPAN-RC !
   GPT-GS-RESTORE
   GT-POOL-PASS-HOOK-DEFAULT!
   GPT-SPAN-RC @ 0 <> if GPT-SPAN-RC @ throw then ;

create GPT-GEN-SAVE GS-GEN-CAP allot
variable GPT-GEN-SAVE-U

: GPT-GEN-SAVE! ( -- )
   GS-GEN$ {: a:ptr u:n :}
   a GPT-GEN-SAVE u BYTE-COPY
   u GPT-GEN-SAVE-U ! ;

: GPT-GEN-RESTORE ( -- )
   GPT-GEN-SAVE GPT-GEN-SAVE-U @ GS-GEN! ;

\ The genuine label-collision mis-fire this dot prevents: a fork child that is
\ ITSELF a nested pool parent (the TEST:TRWE-POST-CANDIDATE shape) emits its nested
\ slot's authoritative pass-hook span through the same GS-SPAN entry its own
\ self-suppression watches. When the nested slot's label bytes equal the
\ child's own slot label, byte-matching suppression swallows an authoritative
\ span the child-as-pool-parent owns: exactly two "fork nest label" spans must
\ reach the tsv (outer pool's + nested pool's), not one.
: GPT-NEST-INNER ( -- )
   s" gate-pool nested inner" type cr ;

: GPT-NEST-CHILD ( -- )
   1 GT-POOL-SLOTS!
   GT-POOL-RESET
   s" fork nest label" GPT-TIMEOUT-MS [: GPT-NEST-INNER ;] GT-POOL-START-FORK
   GT-POOL-DRAIN ;

: GPT-NEST-CASE-BODY ( -- )
   GT-ROOT GS-ROOT!
   s" 7" GS-GEN!
   [: GPT-SPAN-HOOK ;] is GT-POOL-PASS-HOOK
   1 GT-POOL-SLOTS!
   GT-POOL-RESET
   s" fork nest label" GPT-TIMEOUT-MS [: GPT-NEST-CHILD ;] GT-POOL-START-FORK
   GT-POOL-DRAIN
   GS-PATH$ GPT-OUT GPT-CAP READ-ALL {: n:n :}
   GPT-OUT n s" fork nest label" GATE-POOL-TEST:SPAN-COUNT$ 2 T=
   GPT-OUT n s" process-fork" s" fork nest label" GATE-PROCESS:ROW-COUNT$ 6 T=
   GS-READ
   GS-SCAN
   GATE-PROCESS:FORK# 6 T=
   GATE-PROCESS:REAPER# 4 T=
   GS-BUF GS-U @ S\" process-fork\t7\tfork nest label" CONTAINS? TTRUE
   GS-BUF GS-U @ S\" process-fork\t7-" CONTAINS? TTRUE ;

: GPT-NEST-CASE ( -- )
   GPT-GS-SAVE!
   GPT-GEN-SAVE!
   [: GPT-NEST-CASE-BODY ;] catch GPT-SPAN-RC !
   GPT-GS-RESTORE
   GPT-GEN-RESTORE
   GT-POOL-PASS-HOOK-DEFAULT!
   GPT-SPAN-RC @ 0 <> if GPT-SPAN-RC @ throw then ;

\ Spawn-slot generation inheritance: a pool-SPAWNED child cannot inherit the
\ parent's in-memory generation the way a fork child does, so the pool passes
\ the slot generation (parent gen + "-" + seq) through HABU_GATE_GEN and
\ GS-GEN-INIT adopts it at load. With the parent at gen "5", the child's span
\ must be qualified "g5-<seq>:", never rebooted to root "g0:".
create GPT-SG-PATH FS-PATH-CAP allot
variable GPT-SG-PATH-U

: GPT-SG-PATH$ ( -- ptr u8 n )
   GPT-SG-PATH GPT-SG-PATH-U @ ;

: GPT-SG-FIXTURE! ( -- )
   GT-ROOT s" spawn-gen.f" GPT-SG-PATH JOIN-PATH GPT-SG-PATH-U !
   GPT-SG-PATH$ S\" s\" spawn gen label\" 3 GS-SPAN" ATOMIC-WRITE-FILE ;

: GPT-SG-ARGV ( -- )
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   s" lib/errors.f" >LEN PROC-ARGV+
   s" lib/string.f" >LEN PROC-ARGV+
   s" lib/memory.f" >LEN PROC-ARGV+
   s" lib/fs.f" >LEN PROC-ARGV+
   s" lib/fs-mutate.f" >LEN PROC-ARGV+
   s" lib/process.f" >LEN PROC-ARGV+
   s" lib/process-argv.f" >LEN PROC-ARGV+
   s" lib/process-env.f" >LEN PROC-ARGV+
   s" test/gate-stats.f" >LEN PROC-ARGV+
   GPT-SG-PATH$ >LEN PROC-ARGV+ ;

: GPT-SG-CASE-BODY ( -- )
   GT-ROOT GS-ROOT!
   GT-POOL-PASS-HOOK-DEFAULT!
   GPT-SG-FIXTURE!
   s" 5" GS-GEN!
   GPT-SG-ARGV
   PROC-ENV-RESET
   GS-ENV+
   PROC-ENV-INHERIT-MISSING
   1 GT-POOL-SLOTS!
   GT-POOL-RESET
   s" bin/hb" s" spawn gen case" GPT-TIMEOUT-MS GT-POOL-START
   GT-POOL-DRAIN
   GS-PATH$ GPT-OUT GPT-CAP READ-ALL {: n:n :}
   GPT-OUT n s" spawn gen label" GPT-COUNT$ 1 T=
   GPT-OUT n s" process-exec" s" spawn gen case" GATE-PROCESS:ROW-COUNT$ 1 T=
   GPT-OUT n s" process-fork" s" spawn gen case" GATE-PROCESS:ROW-COUNT$ 1 T=
   GPT-OUT n s" g5-" GPT-COUNT$ 1 T=
   GPT-OUT n s" g0:spawn gen label" GPT-COUNT$ 0 T= ;

: GPT-SG-CASE ( -- )
   GPT-GS-SAVE!
   GPT-GEN-SAVE!
   [: GPT-SG-CASE-BODY ;] catch GPT-SPAN-RC !
   GPT-GS-RESTORE
   GPT-GEN-RESTORE
   GT-POOL-PASS-HOOK-DEFAULT!
   GPT-SPAN-RC @ 0 <> if GPT-SPAN-RC @ throw then ;

: GPT-ROOT-SAVE! ( -- )
   GT-ROOT {: a:ptr u:n :}
   u FS-PATH-CAP > if E-FS-PATH throw then
   a GPT-ROOT-SAVE u BYTE-COPY
   u GPT-ROOT-SAVE-U ! ;

: GPT-ROOT-RESTORE ( -- )
   GPT-ROOT-SAVE GPT-ROOT-SAVE-U @ GT-COPY-ROOT! ;

: GPT-INFRA-START ( -- )
   s" infra fork" GPT-TIMEOUT-MS [: GPT-WORKER ;] GT-POOL-START-FORK ;

: GPT-INFRA-CASE ( -- )
   GPT-ROOT-SAVE!
   s" /nonexistent-habu-pool-root" GT-COPY-ROOT!
   1 GT-POOL-SLOTS!
   GT-POOL-RESET
   GT-POOL-RED-RESET
   [: GPT-INFRA-START ;] E-FS-OPEN TTHROWSQ
   GT-POOL-RED# 0 T=
   GT-POOL-LIVE @ 0 T=
   GPT-ROOT-RESTORE ;

\ Child driver for the kept-root e2e: reproduce the real red-gate completion so
\ TEST:COMPLETE routes a red phase to TEST:RED-COMPLETE, which prints the red list
\ plus TEST:KEPT-ROOT-LINE and dies without GT-CLEANUP, leaving the capture root
\ (and its pool-*-out.log files) on disk for the parent to inspect.
: GPT-KEPT-ROOT-CHILD ( -- )
   s" gpt-kept-root" GT-START
   GT-ROOT GS-ROOT!
   1 GT-POOL-SLOTS!
   GT-POOL-RESET
   GT-POOL-RED-RESET
   s" kept-root worker" GPT-TIMEOUT-MS [: GPT-SOFT-ONE ;] GT-POOL-START-FORK
   GT-POOL-DRAIN-SOFT
   TEST:COMPLETE ;

: GPT-KR-MARK$ ( -- ptr u8 n )
   s" capture root kept: " ;

: GPT-KR-PARSE-ROOT ( n -- ) {: outu:n :}
   GPT-OUT outu STR:LENGTH GPT-KR-MARK$ STR:LENGTH STR:FIND-SUB MATCH option
     none OF E-STR-BOUNDS throw ENDOF
     some OF CAD-NUM:GPT-IX>RAW ENDOF
   ;MATCH {: pos:n :}
   pos GPT-KR-MARK$ nip + {: start:n :}
   GPT-OUT start BYTE+ outu start - STR:LENGTH $A STR:INDEX-OF MATCH option
     none OF E-STR-BOUNDS throw ENDOF
     some OF CAD-NUM:GPT-IX>RAW ENDOF
   ;MATCH {: rel:n :}
   rel FS-PATH-CAP > if E-FS-PATH throw then
   GPT-OUT start BYTE+ GPT-KR-ROOT rel BYTE-COPY
   rel GPT-KR-ROOT-U ! ;

: GPT-KR-ROOT$ ( -- ptr u8 n )
   GPT-KR-ROOT GPT-KR-ROOT-U @ ;

: GPT-KR-LOG$ ( -- ptr u8 n )
   GPT-KR-LOG GPT-KR-LOG-U @ ;

: GPT-KR-LOG! ( -- )
   GPT-KR-ROOT$ s" pool-0-1-out.log" GPT-KR-LOG JOIN-PATH GPT-KR-LOG-U ! ;

: GPT-KR-EXPECT-CAPTURE ( -- )
   GPT-KR-ROOT$ EXISTS? TTRUE
   GPT-KR-LOG!
   GPT-KR-LOG$ FILE? TTRUE
   GPT-KR-LOG$ GPT-OUT GPT-CAP READ-ALL {: logn:n :}
   GPT-OUT logn s" gate-pool soft one" CONTAINS? TTRUE ;

: GPT-KR-EXPECT-OUT ( n -- ) {: outu:n :}
   GPT-OUT outu s" red phases: " CONTAINS? TTRUE
   GPT-OUT outu GPT-KR-MARK$ CONTAINS? TTRUE ;

: GPT-KEPT-ROOT-CASE ( -- )
   s" kept-root-case" GPT-MODE-CAPTURE {: outu:n erru:n rc:n :}
   rc 0 <> TTRUE
   outu GPT-KR-EXPECT-OUT
   outu GPT-KR-PARSE-ROOT
   GPT-KR-EXPECT-CAPTURE
   GPT-KR-ROOT$ REMOVE-TREE ;

\ Group-kill regression: a fork worker forks a grandchild that would write a
\ sentinel after a delay, then hangs. The pool times the worker out and
\ GT-POOL-KILL-SLOT signals the whole process group, so the grandchild dies
\ before its delay elapses and the sentinel never appears.
250 constant GPT-GK-TIMEOUT-MS
1500 constant GPT-GK-DELAY-MS
1500 constant GPT-GK-WAIT-MS

create GPT-GK-SENTINEL FS-PATH-CAP allot
create GPT-GK-SLEEP-PFD 64 allot
variable GPT-GK-SENTINEL-U

: GPT-GK-SENTINEL$ ( -- ptr u8 n )
   GPT-GK-SENTINEL GPT-GK-SENTINEL-U @ ;

\ Wall-clock sleep by polling no fds until the deadline; poll rc (timeout or
\ EINTR) is intentionally ignored because both just re-loop toward the deadline.
: GPT-SLEEP-MS ( n -- ) {: ms:n :}
   mono-ns ms PROC-NS-PER-MS * + {: deadline:n :}
   begin mono-ns deadline < while
      GPT-GK-SLEEP-PFD 0 50 poll drop
   repeat ;

: GPT-GK-GRANDCHILD ( -- )
   GPT-GK-DELAY-MS GPT-SLEEP-MS
   GPT-GK-SENTINEL$ s" grandchild-was-here" WRITE-ALL
   s" " 0 die ;

: GPT-GK-CHILD ( -- )
   PROC-FORK:CHECKED {: pid:pid :}
   pid PID>N 0= if GPT-GK-GRANDCHILD then
   s" gate-pool group-kill child" type cr
   begin 0 0= 0= until ;

: GPT-GK-SENTINEL! ( -- )
   GT-ROOT s" group-kill-sentinel" GPT-GK-SENTINEL JOIN-PATH GPT-GK-SENTINEL-U ! ;

: GPT-GROUP-KILL-CASE ( -- )
   s" gate-pool-group-kill" GT-START
   GPT-GK-SENTINEL!
   1 GT-POOL-SLOTS!
   GT-POOL-RESET
   GT-POOL-RED-RESET
   s" group-kill worker" GPT-GK-TIMEOUT-MS [: GPT-GK-CHILD ;] GT-POOL-START-FORK
   GT-POOL-DRAIN-SOFT
   GPT-GK-WAIT-MS GPT-SLEEP-MS
   s" group-kill: grandchild sentinel absent" T-LABEL
   GPT-GK-SENTINEL$ EXISTS? TFALSE
   GT-CLEANUP ;

\ Regression for the parent-death reaper's wait(-1) conflict: the reaper armed
\ by GT-POOL-FORK-CHILD must be reparented away from the worker (double-fork),
\ so a worker body that calls wait(-1) expecting no children of its own still
\ gets ECHILD instead of blocking forever on the reaper. This is exactly the
\ case that stalled stdlib/tail-process (lib/process-test.f TEST-WAIT-BAD) when
\ the reaper was a direct child. If the reaper regresses to a worker child, the
\ worker blocks, the pool times it out, and GT-POOL-DRAIN dies here.
: GPT-WAIT-ANY ( -- )
   -1 >PID PROC-WAIT-RC MATCH result ok OF drop ENDOF err OF drop ENDOF ;MATCH ;

: GPT-WAIT-NEG-WORKER ( -- )
   [: GPT-WAIT-ANY ;] E-PROC-WAIT TTHROWSQ ;

: GPT-WAIT-NEG-CASE ( -- )
   s" gate-pool-wait-neg" GT-START
   1 GT-POOL-SLOTS!
   GT-POOL-RESET
   GT-POOL-RED-RESET
   s" wait-neg worker" GPT-TIMEOUT-MS [: GPT-WAIT-NEG-WORKER ;] GT-POOL-START-FORK
   GT-POOL-DRAIN
   GT-CLEANUP ;

\ A child that dies by a SIGKILL the pool did NOT send: it signals its own
\ process group (an OOM/AMFI kill is the real-world analog). It reaps as
\ signaled(9) - rc 137 when flattened - but the pool's timed-out flag stays
\ false, so it is a plain FAIL (kind=signal), never TIMEOUT-UNDER-LOAD. waitpid's
\ WIFSIGNALED vs WIFEXITED (PROC-STATUS>OUTCOME) also keeps a self-exit(137)
\ distinct: that would read exited(137)/kind=exit, likewise not a timeout.
: GPT-EXTERNAL-KILL-WORKER ( -- )
   s" gate-pool external-kill worker" type cr
   0 >PID SIGKILL PROC-KILL-RAW drop ;

: GPT-EXTERNAL-KILL-CASE ( -- )
   s" gate-pool-external-kill" GT-START
   1 GT-POOL-SLOTS!
   GT-POOL-RESET
   GT-POOL-RED-RESET
   s" external-kill worker" GPT-TIMEOUT-MS [: GPT-EXTERNAL-KILL-WORKER ;] GT-POOL-START-FORK
   GT-POOL-DRAIN-SOFT
   s" external-kill: pool-unsent SIGKILL stays plain FAIL, not a timeout" T-LABEL
   GT-POOL-RED# 1 T=
   0 GT-POOL-RED-TIMED-OUT-PTR @ TFALSE
   0 GT-POOL-RED-EXITED-PTR @ TFALSE
   0 GT-POOL-RED-CODE-PTR @ SIGKILL T=
   GT-CLEANUP ;

\ gate-stats attribution: with a timeout hook installed (as run-lib does), a
\ pool-timed-out slot writes a distinct `pool-timeout` gate-stats.tsv row so the
\ contended-host kill is attributable in machine RCA, not only on the RED: line.
: GPT-TIMEOUT-STAT-HOOK ( ptr u8 n n n n n -- ) {: label:ptr labelu:n ms:n live:n limit:n waits:n :}
   label labelu ms live limit waits GS-POOL-TIMEOUT ;

: GPT-TIMEOUT-STAT-CASE-BODY ( -- )
   GT-ROOT GS-ROOT!
   [: GPT-TIMEOUT-STAT-HOOK ;] is GT-POOL-TIMEOUT-HOOK
   1 GT-POOL-SLOTS!
   GT-POOL-RESET
   GT-POOL-RED-RESET
   s" stat hang worker" GPT-HANG-TIMEOUT-MS [: GPT-HANG-WORKER ;] GT-POOL-START-FORK
   GT-POOL-DRAIN-SOFT
   GS-PATH$ GPT-OUT GPT-CAP READ-ALL {: n:n :}
   GPT-OUT n s" pool-timeout" GPT-COUNT$ 1 T=
   GPT-OUT n s" process-fork" s" stat hang worker" GATE-PROCESS:ROW-COUNT$ 3 T= ;

: GPT-TIMEOUT-STAT-CASE ( -- )
   GPT-GS-SAVE!
   [: GPT-TIMEOUT-STAT-CASE-BODY ;] catch GPT-SPAN-RC !
   GPT-GS-RESTORE
   GT-POOL-TIMEOUT-HOOK-DEFAULT!
   GT-POOL-PASS-HOOK-DEFAULT!
   GPT-SPAN-RC @ 0 <> if GPT-SPAN-RC @ throw then ;

: GATE-POOL-TEST-MAIN ( -- )
   s" fail-battery-case" GPT-MODE? if GPT-BATTERY-CASE exit then
   s" kept-root-case" GPT-MODE? if GPT-KEPT-ROOT-CHILD exit then
   T-RESET
   0 GPT-COW !
   s" gate-pool-test" GT-START
   2 GT-POOL-SLOTS!
   GT-POOL-RESET
   s" fork worker" 1000 [: GPT-WORKER ;] GT-POOL-START-FORK
   GT-POOL-DRAIN
   GPT-COW @ 0 T=
   GPT-BIG-CASE
   GPT-SPAN-CASE
   GPT-SPAN-MULTI-CASE
   GPT-NEST-CASE
   GPT-SG-CASE
   GPT-TIMEOUT-STAT-CASE
   GPT-INFRA-CASE
   GT-CLEANUP
   GPT-GROUP-KILL-CASE
   GPT-WAIT-NEG-CASE
   GPT-EXTERNAL-KILL-CASE
   GPT-KEPT-ROOT-CASE
   GPT-BATTERY-REPORT
   T-REPORT
   s" gate-pool-test: ok" type cr ;

\ The battery runs at load time, so it is dispatched from top-level scope:
\ a package is still open above this line.
' GATE-POOL-TEST-MAIN
;package
execute
