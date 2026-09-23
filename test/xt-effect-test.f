\ xt-effect-test.f - xt<effect> value regressions (dot habu-typed-top-xt-096a8f1b,
\ docs/typed-top-level.md §5 sub-dot 4).
\
\ `['] W` retypes from a plain n to xt<effect(W)>: a T-QUOT of W's certified
\ effect that execute/catch/is fit-check by the existing RSEXEC/RSCATCH/IS-APPLY
\ quotation unification (src/core/checker.f BTICK-TOK). The effect is materialised
\ wherever W's operand token is in the checked text (the CANDIDATE path -
\ CHECK-CANDIDATE!), with no lookahead at the consumer: every tick of a word the
\ checker knows is a quotation, so a consumer declares a quotation parameter
\ (`FPRIM-L ( ptr u8 n [ -- ] -- )`, src/habu/habu1.f) and a consumer that really
\ wants the code address as a number takes it through a TRUSTED: row of its own.
\ Positives run each candidate in a spawned child and read its printed verdict:
\   fit    `['] A execute`  (A's ( n -- n n ) fits the row)         -> CERT (-1)
\   misfit `['] A execute`  on ( -- n ) (A needs an input, none)    -> REJ  (0)
\   catch/is fit + misfit, and an unsafe-definer tick (`['] deflinear`) -> REJ.
\ Tier interop with the top-row tracker (src/core/top-row.f): the child tier is
\ staged by HABU_TOP_TIER (XE-RUN = tier-1 warn, XE-RUN2 = tier-2 reject). XE-TIER1
\ pins the tier-1 warn contract: `' FOO2 execute` warns once and still runs, and
\ FOO2's `dup` then reads below the base INSIDE COMPILED CODE, which carries no
\ per-transfer bounds check under guard pages -- the read faults the data
\ stack's guard page before the interpreter's own depth floor can see it, so
\ the run ends in the crash handler's named "(data)" exit (rc 102), not the
\ interpreter's E-UNDERFLOW (that stays rc 70, but only for a top-level word
\ interpreted directly). XE-TIER2 pins
\ the sub-dot-7 flip: `' FOO2 execute` and `0 0 catch` REJECT pre-execution with a
\ clean rc-70 diagnostic and no crash (`0 0 catch` no longer reaches the
\ BLR-into-xt-0 rc-134 crash it hit at tier-1).
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
1 constant XE-WARN-TIER                  \ child default: tier-1 warn
2 constant XE-REJECT-TIER                \ child HABU_TOP_TIER=2: tier-2 reject

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

\ Stage the child tier through HABU_TOP_TIER (pinned explicitly so a tier-2 gate
\ leg cannot leak into a tier-1 child); inherit the rest of the parent env.
: XE-SET-TIER ( n -- ) {: tier:n :}
   PROC-ENV-RESET
   tier XE-REJECT-TIER =
      if s" HABU_TOP_TIER" >LEN s" 2" >LEN PROC-ENV+
      else s" HABU_TOP_TIER" >LEN s" 1" >LEN PROC-ENV+ then
   PROC-ENV-INHERIT-MISSING ;

\ Write the assembled program (SB$) to the child file and run it via --load with an
\ explicit env table (RUN-ARGV-ENV-* consults PROC-ENV).
: XE-RUN-TIER ( ptr u8 n n -- ) {: tier:n :}
   XE-CHILD 2swap WRITE-ALL
   PROC-ARGV-RESET
   tier XE-SET-TIER
   s" --load" >LEN PROC-ARGV+
   XE-CHILD >LEN PROC-ARGV+
   XE-HB$ >LEN  XE-EMPTY 0 >LEN  XE-OUT XE-CAP >LEN
   XE-ERR XE-CAP >LEN  XE-TIMEOUT-MS >MS  RUN-ARGV-ENV-STDIN-CAPTURE-OUTCOME
   XE-STORE! ;

: XE-RUN ( ptr u8 n -- )  XE-WARN-TIER XE-RUN-TIER ;     \ tier-1 (warn / compile-time)
: XE-RUN2 ( ptr u8 n -- ) XE-REJECT-TIER XE-RUN-TIER ;   \ tier-2 (reject)

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
   s" v8:" s" F ( -- ) ['] deflinear execute"     XE-CASE-LINE
   s" v9:" s" E ( n -- n ) ['] A +"             XE-CASE-LINE
   SB$ ;

\ ---- assertions --------------------------------------------------------------
: XE-HAS ( ptr u8 n -- ) XE-OUT$ 2swap CONTAINS? TTRUE ;   \ stdout carries the needle

\ v9 (`['] A +`) is the scalar-sink case: the retype has no consumer lookahead, so
\ the tick is a T-QUOT that cannot unify with +'s `n` input and the body REJECTS.
\ Adding a code address as a number is a raw-cell operation and needs a TRUSTED:
\ row that says so; the engine's own `['] B+ FPRIM-L` sites declare `[ -- ]`.
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
   s" ['] deflinear execute (unsafe definer tick) rejects" T-LABEL
   s" v8:<<REJ>>" XE-HAS
   s" ['] A + (scalar sink) rejects: the tick is a quotation, not a number" T-LABEL
   s" v9:<<REJ>>" XE-HAS ;

\ ---- `[']` of a using-imported bare name (src/habu/habu2.f C-BTICK) ----------
\ C-BTICK resolves its token the way the compile path resolves a call: LFIND,
\ then the used packages' publics (LFINDUSED), then the undefined diagnostic.
\ Its miss branch used to compile NOTHING - no literal, no diagnostic - so a bare
\ name only a `using` import resolved left the body one cell short and the later
\ `execute`/`catch` ran whatever was on top; the engine before this change dies
\ rc 134 on the first row of XE-USING-PROG$. Both programs run in a child, so a
\ regression is a red row here and not a crashed suite.
94 constant XE-AMB-RC       \ ENGINE-ERROR:USING-AMBIGUOUS, the compile path's own exit
70 constant XE-UNDEF-RC

: XE-USING-PRELUDE ( -- )
   SB-RESET
   s" require lib/errors.f" XE-LINE
   s" require lib/string.f" XE-LINE
   s" package XEU-P" XE-LINE
   s" public" XE-LINE
   s" : XEU-W ( n -- n ) 1 + ;" XE-LINE
   s" ;package" XE-LINE
   s" using XEU-P" XE-LINE ;

\ The bare tick and the qualified tick of one word, through execute, catch, a
\ local binding, and the plain drop. Each row prints "<tag>:<value>".
: XE-USING-PROG$ ( -- ptr u8 n )
   XE-USING-PRELUDE
   s" package XEU-T" XE-LINE
   s" : XEU-BARE ( n -- n ) ['] XEU-W execute ;" XE-LINE
   s" : XEU-QUAL ( n -- n ) ['] XEU-P:XEU-W execute ;" XE-LINE
   s" : XEU-CBARE ( n -- n n ) ['] XEU-W catch ;" XE-LINE
   s" : XEU-CQUAL ( n -- n n ) ['] XEU-P:XEU-W catch ;" XE-LINE
   s" : XEU-LOCAL ( n -- n ) ['] XEU-W {: v q :} v q execute ;" XE-LINE
   s" : XEU-DROP ( -- n ) 7 ['] XEU-W drop ;" XE-LINE
   s" : XEU-DROPQ ( -- n ) 7 ['] XEU-P:XEU-W drop ;" XE-LINE
   S\" .\" u1:\" 6 XEU-BARE ." XE-LINE
   S\" .\" u2:\" 6 XEU-QUAL ." XE-LINE
   S\" 6 XEU-CBARE .\" u3rc:\" . .\" u3:\" ." XE-LINE
   S\" 6 XEU-CQUAL .\" u4rc:\" . .\" u4:\" ." XE-LINE
   S\" .\" u5:\" 6 XEU-LOCAL ." XE-LINE
   S\" .\" u6:\" XEU-DROP ." XE-LINE
   S\" .\" u7:\" XEU-DROPQ ." XE-LINE
   s" ;package" XE-LINE
   SB$ ;

\ Two used packages exporting one tail, and one body line to compile against
\ them: the refusals a tick shares with a call.
: XE-AMB-PROG$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   SB-RESET
   s" require lib/errors.f" XE-LINE
   s" package XEU-A  public  : XEU-W ( n -- n ) 1 + ;  ;package" XE-LINE
   s" package XEU-B  public  : XEU-W ( n -- n ) 2 + ;  ;package" XE-LINE
   s" using XEU-A" XE-LINE
   s" using XEU-B" XE-LINE
   s" package XEU-AMB" XE-LINE
   a u XE-LINE
   s" ;package" XE-LINE
   SB$ ;

: XE-AMB-SAID ( -- )                     \ the compile path's ambiguity diagnostic, on stderr
   XE-ERR$ s" hb: ambiguous bare word resolves in multiple used packages: XEU-W"
   CONTAINS? TTRUE ;

: XE-USING ( -- )
   XE-USING-PROG$ XE-RUN
   s" a body ticking a using-imported bare name runs to the end (rc 0)" T-LABEL
   XE-EXITED @ TTRUE  XE-RC @ 0 T=
   s" ['] XEU-W execute and ['] XEU-P:XEU-W execute give the same 7" T-LABEL
   s" u1:7" XE-HAS  s" u2:7" XE-HAS
   s" both ticks catch to 0 with the same 7" T-LABEL
   s" u3rc:0" XE-HAS  s" u3:7" XE-HAS  s" u4rc:0" XE-HAS  s" u4:7" XE-HAS
   s" a bare-name tick bound to a local executes" T-LABEL
   s" u5:7" XE-HAS
   s" the probe's shape: ['] XEU-W drop and ['] XEU-P:XEU-W drop both leave 7" T-LABEL
   s" u6:7" XE-HAS  s" u7:7" XE-HAS ;

: XE-USING-REFUSED ( -- )
   s" ['] of a bare name two used packages export dies rc 94" T-LABEL
   s" : XEU-TICK ( -- n ) 7 ['] XEU-W drop ;" XE-AMB-PROG$ XE-RUN
   XE-EXITED @ TTRUE  XE-RC @ XE-AMB-RC T=  XE-AMB-SAID
   s" a CALL of that bare name dies the same way (same message, same rc)" T-LABEL
   s" : XEU-CALL ( -- n ) 7 XEU-W ;" XE-AMB-PROG$ XE-RUN
   XE-EXITED @ TTRUE  XE-RC @ XE-AMB-RC T=  XE-AMB-SAID
   s" ['] of a name nothing resolves is undefined, never a silent no-op" T-LABEL
   s" : XEU-NONE ( -- n ) 7 ['] XEU-NOSUCH drop ;" XE-AMB-PROG$ XE-RUN
   XE-EXITED @ TTRUE  XE-RC @ XE-UNDEF-RC T=
   XE-ERR$ s" E-UNDEFINED: XEU-NOSUCH" CONTAINS? TTRUE ;

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

\ FOO2's `dup` runs inside COMPILED code (called through execute, not
\ interpreted directly), and compiled code carries no per-transfer bounds
\ check under guard pages: the read below the base faults the data stack's
\ guard page before the interpreter's own depth floor ever sees it, so this
\ ends in the crash handler's named "(data)" exit (102), not E-UNDERFLOW/70 --
\ that diagnostic is reserved for a top-level word interpreted directly (see
\ test/runtime-regression-test.f, unchanged).
: XE-TIER1 ( -- )                        \ tier-1: the tracker observes, the guard page names the fault
   s" tier-1: ' FOO2 execute still warns exactly once at underdepth" T-LABEL
   XE-EXEC$ XE-RUN  XE-WARN-COUNT 1 T=
   s" tier-1: ' FOO2 execute's dup faults the data-stack guard page (rc 102)" T-LABEL
   XE-EXITED @ TTRUE  XE-RC @ 102 T=
   XE-ERR$ s" hb: stack bounds exceeded (data)" CONTAINS? TTRUE ;

: XE-TIER2 ( -- )                        \ tier-2 (sub-dot 7): the pre-armed pins now REJECT
   s" tier-2: ' FOO2 execute rejects rc 70 pre-execution (xt underflow)" T-LABEL
   XE-EXEC$ XE-RUN2  XE-EXITED @ TTRUE  XE-RC @ 70 T=  XE-WARN-COUNT 1 T=
   s" tier-2: 0 0 catch rejects rc 70 pre-execution (no BLR-into-xt-0 crash)" T-LABEL
   XE-CATCH$ XE-RUN2  XE-EXITED @ TTRUE  XE-RC @ 70 T=  XE-WARN-COUNT 1 T= ;

: XE-PREPARE ( -- )
   CLEANUP-RESET
   s" habu-xe" HB-TMP-MKDIR {: a:ptr u:n :}
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
   XE-USING
   XE-USING-REFUSED
   XE-TIER1
   XE-TIER2
   XE-CLEANUP
   T-REPORT
   s" xt-effect: ok" type cr ;

XE-MAIN

;package
