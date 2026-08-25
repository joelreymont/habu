\ top-row-warn-test.f - top-row tracker tier-1 warning + tier-2 reject regressions
\ (dots habu-typed-top-checker-82cf8b84 sub-dot 3, habu-typed-top-tier-589c550f
\ sub-dot 7 - the tier-2 switch + tree burn-down).
\
\ The tracker (src/core/top-row.f) is installed by default in the cold prefix, so
\ every child `bin/hb --load <program>` carries it. The child mode is staged by the
\ HABU_TOP_TIER env var: unset -> tier-1 (warn, rc unchanged), 2 -> tier-2 (reject
\ pre-execution, rc 70 named diagnostic, no crash). Each probe program runs as a
\ child; the fixture counts `hb: top-row: ` lines on the child's stderr.
\
\ Tier-1 positives (docs §1): p1 `' FOO2 execute` (xt underflow) and p3
\ `s" abc" + .` (byte pointer into a scalar consumer) each emit EXACTLY ONE named
\ warning with rc 0. Tier-2 (docs §4 Tier 2): p1, p2 `0 0 catch` (non-xt to catch,
\ which crashes rc 134 at tier-1), and p3 each REJECT pre-execution - one named
\ diagnostic, clean exit rc 70, no crash. p2 proves the crash is eliminated.
\ Negatives (the four zero-warning guards, quiet at BOTH tiers - the no-false-reject
\ proof): a literal -> certified-(ptr u8 n)-consumer line, the CHECK! probe idiom,
\ a mid-stream TRUSTED: shim call, and a `0 set-check` window each emit ZERO and
\ rc 0. The row persists across `require` (tier-2 still rejects p1 after two
\ requires).
\
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/memory.f lib/fs.f
\   lib/fs-mutate.f lib/process.f lib/process-argv.f lib/process-env.f
\   test/top-row-warn-test.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f

package TOP-ROW-WARN-TEST

4096 constant TW-CAP
10000 constant TW-TIMEOUT-MS
1 constant TW-TIER1                      \ child default: warn only
2 constant TW-TIER2                      \ child HABU_TOP_TIER=2: reject pre-execution
70 constant TW-REJECT-RC                 \ clean rc-70 reject (RC-REJECT)

variable TW-ROOT-U
variable TW-CHILD-U
variable TW-OUT-U
variable TW-ERR-U
variable TW-EXITED
variable TW-RC

create TW-ROOT-BUF FS-PATH-CAP allot
create TW-CHILD-BUF FS-PATH-CAP allot
create TW-OUT TW-CAP allot
create TW-ERR TW-CAP allot
create TW-EMPTY 1 allot

: TW-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: src:ptr u:n dst:ptr lenp:ptr :}
   u FS-PATH-CAP > if E-FS-CAPACITY throw then
   src dst u BYTE-COPY
   u lenp ! ;

: TW-ROOT ( -- ptr u8 n ) TW-ROOT-BUF TW-ROOT-U @ ;
: TW-CHILD ( -- ptr u8 n ) TW-CHILD-BUF TW-CHILD-U @ ;
: TW-ERR$ ( -- ptr u8 n ) TW-ERR TW-ERR-U @ ;

\ Resolve the child engine: gate default env HABU_UNDER_TEST, else bin/hb.
: TW-HB$ ( -- ptr u8 n )
   s" HABU_UNDER_TEST" >LEN PROC-ENV-DEFAULT$? if LEN>N exit then
   2drop
   s" HABU_UNDER_TEST" GETENV dup 0= if 2drop s" bin/hb" exit then ;

: TW-STORE! ( len len outcome -- )
   MATCH outcome
     exited OF TW-RC ! 0 0= TW-EXITED ! ENDOF
     signaled OF TW-RC ! 0 0= 0= TW-EXITED ! ENDOF
     timeout OF 0 TW-RC ! 0 0= 0= TW-EXITED ! ENDOF
   ;MATCH
   LEN>N TW-ERR-U !  LEN>N TW-OUT-U ! ;

\ Stage the child tier through its env: HABU_TOP_TIER=2 -> tier-2 reject, any other
\ value -> tier-1 warn. The tier is pinned EXPLICITLY (not merely omitted) so a
\ tier-2 gate leg that exports HABU_TOP_TIER=2 in the parent cannot leak into the
\ tier-1 children. PROC-ENV-INHERIT-MISSING copies the rest of the parent env so
\ the child keeps HB_TMP etc. (matching the inherited-environ non-env spawn).
: TW-SET-TIER ( n -- ) {: tier:n :}
   PROC-ENV-RESET
   tier TW-TIER2 =
      if s" HABU_TOP_TIER" >LEN s" 2" >LEN PROC-ENV+
      else s" HABU_TOP_TIER" >LEN s" 1" >LEN PROC-ENV+ then
   PROC-ENV-INHERIT-MISSING ;

\ Write the assembled program (SB$) to the child file and run it via --load, with
\ an explicit env table (RUN-ARGV-ENV-* consults PROC-ENV; the plain RUN-ARGV-*
\ inherits the parent environ and ignores it).
: TW-RUN-TIER ( ptr u8 n n -- ) {: tier:n :}
   TW-CHILD 2swap WRITE-ALL
   PROC-ARGV-RESET
   tier TW-SET-TIER
   s" --load" >LEN PROC-ARGV+
   TW-CHILD >LEN PROC-ARGV+
   TW-HB$ >LEN  TW-EMPTY 0 >LEN  TW-OUT TW-CAP >LEN
   TW-ERR TW-CAP >LEN  TW-TIMEOUT-MS >MS  RUN-ARGV-ENV-STDIN-CAPTURE-OUTCOME
   TW-STORE! ;

: TW-RUN ( ptr u8 n -- )  TW-TIER1 TW-RUN-TIER ;   \ default: tier-1 warn
: TW-RUN2 ( ptr u8 n -- ) TW-TIER2 TW-RUN-TIER ;   \ staged: tier-2 reject

\ Non-overlapping count of a needle across the captured stderr buffer.
variable TW-CNT
: TW-COUNT ( ptr u8 n ptr u8 n -- n ) {: a:ptr u:n b:ptr v:n :}
   0 TW-CNT !
   v 0= if 0 exit then
   0 begin dup u v - <= while
      a over + v  b v  STR= if
         TW-CNT @ 1 + TW-CNT !  v +
      else 1 + then
   repeat drop
   TW-CNT @ ;

: TW-WARN-COUNT ( -- n ) TW-ERR$ s" hb: top-row: " TW-COUNT ;

: TW-LF ( -- ) 10 SB-APPEND-C ;
: TW-LINE ( ptr u8 n -- ) SB-APPEND TW-LF ;

\ ---- probe programs ----------------------------------------------------------
: TW-P1$ ( -- ptr u8 n )                 \ ' FOO2 execute on an empty stack
   SB-RESET
   s" : FOO2 ( n -- n n ) dup ;" TW-LINE
   s" ' FOO2 execute" TW-LINE
   SB$ ;

: TW-P2$ ( -- ptr u8 n )                 \ 0 0 catch: catch handed a non-xt
   SB-RESET
   s" 0 0 catch" TW-LINE
   SB$ ;

: TW-P3$ ( -- ptr u8 n )                 \ s" abc" + . : byte pointer into a scalar consumer
   SB-RESET
   S\" s\" abc\" + . cr" TW-LINE
   SB$ ;

: TW-EVAL$ ( -- ptr u8 n )               \ literal -> certified (ptr u8 n) consumer -> bool
   SB-RESET
   s" TRUSTED: TW-PASSES? ( ptr u8 n -- bool ) 2drop 0 0= ;" TW-LINE
   S\" s\" some source\" TW-PASSES? drop" TW-LINE
   SB$ ;

: TW-CHECK$ ( -- ptr u8 n )              \ the CHECK! probing idiom
   SB-RESET
   S\" s\" : TW-G ( n -- n ) 1 + ;\" CHECK! . cr" TW-LINE
   SB$ ;

: TW-SHIM$ ( -- ptr u8 n )               \ mid-stream TRUSTED: shim call (no reset, no warn)
   SB-RESET
   s" TRUSTED: TW-SHIM ( n n -- n ) + ;" TW-LINE
   s" 1 2 TW-SHIM . cr" TW-LINE
   SB$ ;

: TW-SETCHECK$ ( -- ptr u8 n )           \ 0 set-check window: enforcement suspended
   SB-RESET
   s" 0 set-check" TW-LINE
   S\" s\" abc\" + . cr" TW-LINE
   SB$ ;

: TW-REQUIRE$ ( -- ptr u8 n )            \ row persists across require: p1 still caught after two requires
   SB-RESET
   s" require lib/errors.f" TW-LINE
   s" require lib/string.f" TW-LINE
   s" : FOO2 ( n -- n n ) dup ;" TW-LINE
   s" ' FOO2 execute" TW-LINE
   SB$ ;

\ ---- assertions --------------------------------------------------------------
: TW-ASSERT-WARNS ( ptr u8 n n -- ) {: n:n :}   \ tier-1: run program, assert warning count == n
   TW-RUN  TW-WARN-COUNT n T= ;

: TW-ASSERT-REJECTS ( ptr u8 n -- )     \ tier-2: clean exit rc 70, exactly one diagnostic, no crash
   TW-RUN2
   TW-EXITED @ TTRUE                     \ exited (not signaled): the tier-1 crash is gone
   TW-RC @ TW-REJECT-RC T=               \ clean rc-70 reject
   TW-WARN-COUNT 1 T= ;                  \ exactly one named diagnostic

: TW-ASSERT-QUIET2 ( ptr u8 n -- )      \ tier-2: no reject (rc 0), zero diagnostics (no false positive)
   TW-RUN2
   TW-EXITED @ TTRUE  TW-RC @ 0 T=  TW-WARN-COUNT 0 T= ;

: TW-POSITIVES ( -- )                    \ tier-1: warn, rc unchanged (observe, never block)
   s" tier-1 p1 ' FOO2 execute emits exactly one xt-underflow warning" T-LABEL
   TW-P1$ 1 TW-ASSERT-WARNS
   s" tier-1 p1 leaves rc 0 (tier-1 observes, never blocks)" T-LABEL
   TW-EXITED @ TTRUE  TW-RC @ 0 T=
   s" tier-1 p3 byte pointer into a scalar consumer emits exactly one warning" T-LABEL
   TW-P3$ 1 TW-ASSERT-WARNS
   s" tier-1 p3 leaves rc 0 (execution unchanged)" T-LABEL
   TW-EXITED @ TTRUE  TW-RC @ 0 T= ;

: TW-TIER2-REJECTS ( -- )                \ tier-2: reject pre-execution, rc 70, no crash
   s" tier-2 p1 ' FOO2 execute rejects rc 70 pre-execution" T-LABEL
   TW-P1$ TW-ASSERT-REJECTS
   s" tier-2 p2 0 0 catch rejects rc 70 (no BLR-into-xt-0 crash)" T-LABEL
   TW-P2$ TW-ASSERT-REJECTS
   s" tier-2 p3 byte pointer into a scalar consumer rejects rc 70" T-LABEL
   TW-P3$ TW-ASSERT-REJECTS ;

: TW-NEGATIVES ( -- )                    \ tier-1: the four guards emit zero warnings
   s" tier-1 eval-fixture idiom (literal -> certified consumer) is quiet" T-LABEL
   TW-EVAL$ 0 TW-ASSERT-WARNS
   s" tier-1 CHECK! probe idiom is quiet" T-LABEL
   TW-CHECK$ 0 TW-ASSERT-WARNS
   s" tier-1 mid-stream TRUSTED: shim call is quiet (no reset)" T-LABEL
   TW-SHIM$ 0 TW-ASSERT-WARNS
   s" tier-1 0 set-check window suspends enforcement (quiet)" T-LABEL
   TW-SETCHECK$ 0 TW-ASSERT-WARNS ;

: TW-NEGATIVES2 ( -- )                   \ tier-2: the four guards stay quiet - no false reject
   s" tier-2 eval-fixture idiom stays quiet (no false reject)" T-LABEL
   TW-EVAL$ TW-ASSERT-QUIET2
   s" tier-2 CHECK! probe idiom stays quiet (no false reject)" T-LABEL
   TW-CHECK$ TW-ASSERT-QUIET2
   s" tier-2 mid-stream TRUSTED: shim call stays quiet (certified path)" T-LABEL
   TW-SHIM$ TW-ASSERT-QUIET2
   s" tier-2 0 set-check window still suspends enforcement (escape hatch)" T-LABEL
   TW-SETCHECK$ TW-ASSERT-QUIET2 ;

: TW-PERSIST ( -- )
   s" tier-2 row persists across require (p1 still rejected after two requires)" T-LABEL
   TW-REQUIRE$ TW-ASSERT-REJECTS ;

: TW-PREPARE ( -- )
   CLEANUP-RESET
   s" habu-tw" TMPDIR-MKDIR {: a:ptr u:n :}
   a u TW-ROOT-BUF TW-ROOT-U TW-COPY!
   TW-ROOT CLEANUP-TREE+
   TW-ROOT s" probe.f" TW-CHILD-BUF JOIN-PATH TW-CHILD-U ! ;

: TW-CLEANUP ( -- )
   CLEANUP-RUN
   TW-ROOT EXISTS? TFALSE ;

: TW-MAIN ( -- )
   T-RESET
   TW-PREPARE
   TW-POSITIVES
   TW-TIER2-REJECTS
   TW-NEGATIVES
   TW-NEGATIVES2
   TW-PERSIST
   TW-CLEANUP
   T-REPORT
   s" top-row-warn: ok" type cr ;

TW-MAIN

;package
