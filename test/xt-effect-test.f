\ xt-effect-test.f - xt<effect> value regressions (dot habu-typed-top-xt-096a8f1b,
\ docs/typed-top-level.md §5 sub-dot 4).
\
\ `['] W` retypes from a plain n to xt<effect(W)>: a T-QUOT of W's certified
\ effect that execute/catch/is fit-check by the existing RSEXEC/RSCATCH/IS-APPLY
\ quotation unification (src/core/checker.f BTICK-TOK). The effect is materialised
\ only where W's operand token is in the checked text (the CANDIDATE path -
\ CHECK-CANDIDATE!) AND the very next token is an xt consumer, so store-consumer
\ sites (`['] B+ FPRIM-L`) keep the raw xt cell. Positives run each candidate in a
\ spawned child and read its printed verdict:
\   fit    `['] A execute`  (A's ( n -- n n ) fits the row)         -> CERT (-1)
\   misfit `['] A execute`  on ( -- n ) (A needs an input, none)    -> REJ  (0)
\   catch/is fit + misfit, and an unsafe-definer tick (`['] deftype`) -> REJ.
\ Tier-1 interop (the top-row tracker, src/core/top-row.f, unchanged here): the
\ top-level `' FOO2 execute` xt-underflow warning stays (rc 0, exactly one line),
\ and `0 0 catch` still emits its one non-xt warning - the pre-armed tier-2 reject
\ fixture, whose CURRENT verdict is that warning followed by the BLR-into-xt-0
\ crash (rc 134); tier-2 (sub-dot 7) will flip it to a clean pre-execution rc-70
\ reject, at which point this fixture's assertion moves from "warns" to "rejects".
\
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/memory.f lib/fs.f
\   lib/fs-mutate.f lib/process.f lib/process-argv.f lib/process-env.f
\   test/xt-effect-test.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f

package XT-EFFECT-TEST

4096 constant XE-CAP
10000 constant XE-TIMEOUT-MS

variable XE-ROOT-U
variable XE-CHILD-U
variable XE-OUT-U
variable XE-ERR-U
variable XE-EXITED
variable XE-RC

create XE-ROOT-BUF FS-PATH-CAP allot
create XE-CHILD-BUF FS-PATH-CAP allot
create XE-OUT XE-CAP allot
create XE-ERR XE-CAP allot
create XE-EMPTY 1 allot

: XE-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: src:ptr u:n dst:ptr lenp:ptr :}
   u FS-PATH-CAP > if E-FS-CAPACITY throw then
   src dst u BYTE-COPY
   u lenp ! ;

: XE-ROOT ( -- ptr u8 n ) XE-ROOT-BUF XE-ROOT-U @ ;
: XE-CHILD ( -- ptr u8 n ) XE-CHILD-BUF XE-CHILD-U @ ;
: XE-OUT$ ( -- ptr u8 n ) XE-OUT XE-OUT-U @ ;
: XE-ERR$ ( -- ptr u8 n ) XE-ERR XE-ERR-U @ ;

\ Resolve the child engine: gate default env HABU_UNDER_TEST, else bin/hb.
: XE-HB$ ( -- ptr u8 n )
   s" HABU_UNDER_TEST" >LEN PROC-ENV-DEFAULT$? if LEN>N exit then
   2drop
   s" HABU_UNDER_TEST" GETENV dup 0= if 2drop s" bin/hb" exit then ;

: XE-STORE! ( len len outcome -- )
   MATCH outcome
     exited OF XE-RC ! 0 0= XE-EXITED ! ENDOF
     signaled OF XE-RC ! 0 0= 0= XE-EXITED ! ENDOF
     timeout OF 0 XE-RC ! 0 0= 0= XE-EXITED ! ENDOF
   ;MATCH
   LEN>N XE-ERR-U !  LEN>N XE-OUT-U ! ;

\ Write the assembled program (SB$) to the child file and run it via --load.
: XE-RUN ( ptr u8 n -- )
   XE-CHILD 2swap WRITE-ALL
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   XE-CHILD >LEN PROC-ARGV+
   XE-HB$ >LEN  XE-EMPTY 0 >LEN  XE-OUT XE-CAP >LEN
   XE-ERR XE-CAP >LEN  XE-TIMEOUT-MS >MS  RUN-ARGV-STDIN-CAPTURE-OUTCOME
   XE-STORE! ;

\ Non-overlapping count of a needle across the captured stderr buffer.
variable XE-CNT
: XE-COUNT ( ptr u8 n ptr u8 n -- n ) {: a:ptr u:n b:ptr v:n :}
   0 XE-CNT !
   v 0= if 0 exit then
   0 begin dup u v - <= while
      a over + v  b v  STR= if
         XE-CNT @ 1 + XE-CNT !  v +
      else 1 + then
   repeat drop
   XE-CNT @ ;

: XE-WARN-COUNT ( -- n ) XE-ERR$ s" hb: top-row: " XE-COUNT ;

: XE-LF ( -- ) 10 SB-APPEND-C ;
: XE-LINE ( ptr u8 n -- ) SB-APPEND XE-LF ;

\ ---- candidate program prelude + emitter -------------------------------------
\ A: a net-+1 word; SP: a stack-preserving word; ACT: a deferred ( n -- n ). The
\ child prints one verdict marker per CHECK-CANDIDATE! so the parent reads the
\ certified/rejected outcome without depending on checker-internal diagnostics.
: XE-PRELUDE ( -- )
   SB-RESET
   s" require lib/errors.f" XE-LINE
   s" require lib/string.f" XE-LINE
   s" : A ( n -- n n ) dup ;" XE-LINE
   s" : SP ( n -- n ) 1 + ;" XE-LINE
   s" defer ACT ( n -- n )" XE-LINE
   s" : XE-SHOW ( n -- )" XE-LINE
   S\" dup -1 = if drop .\" <<CERT>>\" cr exit then" XE-LINE
   S\" dup 0=  if drop .\" <<REJ>>\"  cr exit then" XE-LINE
   S\" drop .\" <<UNCK>>\" cr ;" XE-LINE ;

\ Emit one labelled candidate check into the child program:
\   ." <tag>"  s" <cand>" CHECK-CANDIDATE! XE-SHOW
\ so the child prints "<tag><<CERT>>" / "<tag><<REJ>>". cand must hold no '"'.
: XE-CASE-LINE ( ptr u8 n ptr u8 n -- ) {: ta:ptr tu:n ca:ptr cu:n :}
   S\" .\" " SB-APPEND  ta tu SB-APPEND  S\" \"  " SB-APPEND
   S\" s\" " SB-APPEND  ca cu SB-APPEND  S\" \" CHECK-CANDIDATE! XE-SHOW" XE-LINE ;

\ All value cases run in ONE child (one spawn, not one per case). Each is a
\ (tag, candidate) pair; the parent reads "<tag><verdict>" back from stdout.
: XE-VALUES-PROG$ ( -- ptr u8 n )
   XE-PRELUDE
   s" v1:" s" B ( n -- n n ) ['] A execute"     XE-CASE-LINE
   s" v2:" s" C ( -- n ) ['] A execute"         XE-CASE-LINE
   s" v3:" s" M ( n n -- n n n ) ['] A execute" XE-CASE-LINE
   s" v4:" s" H ( n -- n n ) ['] SP catch"      XE-CASE-LINE
   s" v5:" s" I ( n -- n n ) ['] A catch"       XE-CASE-LINE
   s" v6:" s" J ( -- ) ['] SP is ACT"           XE-CASE-LINE
   s" v7:" s" K ( -- ) ['] A is ACT"            XE-CASE-LINE
   s" v8:" s" F ( -- ) ['] deftype execute"     XE-CASE-LINE
   s" v9:" s" E ( n -- n ) ['] A +"             XE-CASE-LINE
   SB$ ;

\ ---- assertions --------------------------------------------------------------
: XE-HAS ( ptr u8 n -- ) XE-OUT$ 2swap CONTAINS? TTRUE ;   \ stdout carries the needle

\ v9 (`['] A +`) is the store-consumer compatibility case: the retype fires only
\ when the next token is an xt consumer, so a scalar sink gets a plain n exactly
\ as today (a T-QUOT would not unify with +'s input) - the byte-identical model
\ the engine's `['] B+ FPRIM-L` sites rely on.
: XE-VALUES ( -- )
   XE-VALUES-PROG$ XE-RUN
   XE-EXITED @ TTRUE
   XE-RC @ 0 T=
   s" ['] A execute fits ( n -- n n ) and certifies" T-LABEL
   s" v1:<<CERT>>" XE-HAS
   s" ['] A execute on ( -- n ) misfits (A needs 1 input) and rejects" T-LABEL
   s" v2:<<REJ>>" XE-HAS
   s" ['] A execute keeps surplus depth (row-poly) and certifies" T-LABEL
   s" v3:<<CERT>>" XE-HAS
   s" ['] SP catch (stack-preserving) certifies" T-LABEL
   s" v4:<<CERT>>" XE-HAS
   s" ['] A catch (not stack-preserving) rejects" T-LABEL
   s" v5:<<REJ>>" XE-HAS
   s" ['] SP is ACT (effect fits the defer) certifies" T-LABEL
   s" v6:<<CERT>>" XE-HAS
   s" ['] A is ACT (effect misfits the defer) rejects" T-LABEL
   s" v7:<<REJ>>" XE-HAS
   s" ['] deftype execute (unsafe definer tick) rejects" T-LABEL
   s" v8:<<REJ>>" XE-HAS
   s" ['] A + (non-xt-consumer sink) stays a plain n and certifies" T-LABEL
   s" v9:<<CERT>>" XE-HAS ;

\ ---- tier-1 interop + tier-2 pre-arm (top-row tracker, unchanged) ------------
: XE-EXEC$ ( -- ptr u8 n )               \ ' FOO2 execute on an empty stack
   SB-RESET
   s" : FOO2 ( n -- n n ) dup ;" XE-LINE
   s" ' FOO2 execute" XE-LINE
   SB$ ;

: XE-CATCH$ ( -- ptr u8 n )              \ 0 0 catch: the pre-armed tier-2 reject fixture
   SB-RESET
   s" 0 0 catch" XE-LINE
   SB$ ;

: XE-TIER1 ( -- )
   s" tier-1: ' FOO2 execute still warns exactly once at underdepth" T-LABEL
   XE-EXEC$ XE-RUN  XE-WARN-COUNT 1 T=
   s" tier-1: ' FOO2 execute leaves rc 0 (tracker observes, never blocks)" T-LABEL
   XE-EXITED @ TTRUE  XE-RC @ 0 T=
   s" tier-2 pre-arm: 0 0 catch warns once now; tier-2 will reject rc 70" T-LABEL
   XE-CATCH$ XE-RUN  XE-WARN-COUNT 1 T= ;

: XE-PREPARE ( -- )
   CLEANUP-RESET
   s" habu-xe" TMPDIR-MKDIR {: a:ptr u:n :}
   a u XE-ROOT-BUF XE-ROOT-U XE-COPY!
   XE-ROOT CLEANUP-TREE+
   XE-ROOT s" cand.f" XE-CHILD-BUF JOIN-PATH XE-CHILD-U ! ;

: XE-CLEANUP ( -- )
   CLEANUP-RUN
   XE-ROOT EXISTS? TFALSE ;

: XE-MAIN ( -- )
   T-RESET
   XE-PREPARE
   XE-VALUES
   XE-TIER1
   XE-CLEANUP
   T-REPORT
   s" xt-effect: ok" type cr ;

XE-MAIN

;package
