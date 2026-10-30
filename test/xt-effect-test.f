\ xt-effect-test.f - xt<effect> value regressions (docs/design.md "Typed top
\ level"). `['] W` is xt<effect(W)>, a T-QUOT of W's certified effect;
\ execute, catch and is fit-check it by quotation unification
\ (src/core/checker.f BTICK-TOK).
\ Run: bin/hb --load src/lib/errors.f src/lib/string.f src/lib/test.f src/lib/memory.f src/lib/fs.f
\   src/lib/fs-mutate.f src/lib/process.f src/lib/process-argv.f test/xt-effect-test.f

require src/lib/errors.f
require src/lib/string.f
require src/lib/test.f
require src/lib/memory.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/test/outcome.f
require src/lib/engine-candidate.f

package XT-EFFECT-TEST

4096 constant XE-CAP
10000 constant XE-TIMEOUT-MS

variable XE-ROOT-U
variable XE-CHILD-U
variable XE-OUT-U
variable XE-ERR-U
variable XE-EXITED
variable XE-RC

create XE-ROOT-BUF PATH-CAP allot
create XE-CHILD-BUF PATH-CAP allot
create XE-OUT XE-CAP allot
create XE-ERR XE-CAP allot
create XE-EMPTY 1 allot

: XE-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: src:ptr u:n dst:ptr lenp:ptr :}
   u PATH-CAP > if E-FS-CAPACITY throw then
   src dst u BYTE-COPY
   u lenp ! ;

: XE-ROOT ( -- ptr u8 n ) XE-ROOT-BUF XE-ROOT-U @ ;
: XE-CHILD ( -- ptr u8 n ) XE-CHILD-BUF XE-CHILD-U @ ;
: XE-OUT$ ( -- ptr u8 n ) XE-OUT XE-OUT-U @ ;
: XE-ERR$ ( -- ptr u8 n ) XE-ERR XE-ERR-U @ ;

: XE-STORE! ( len len outcome ptr u8 n -- )
   {: outu:len erru:len oc src:ptr srcu:n :}
   outu LEN>N XE-OUT-U !  erru LEN>N XE-ERR-U !
   oc MATCH outcome
     exited OF XE-RC ! 0 0= XE-EXITED ! ENDOF
     signaled OF XE-RC ! 0 0= 0= XE-EXITED ! ENDOF
     timeout OF src srcu XE-OUT$ XE-ERR$ T-TIMED-OUT ENDOF
   ;MATCH ;

: XE-RUN ( ptr u8 n -- ) {: src:ptr srcu:n :}
   XE-CHILD src srcu FS:WRITE-ALL
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   XE-CHILD >LEN PROC-ARGV+
   ENGINE-CANDIDATE:PATH$ >LEN  XE-EMPTY 0 >LEN  XE-OUT XE-CAP >LEN
   XE-ERR XE-CAP >LEN  XE-TIMEOUT-MS >MS  RUN-ARGV-STDIN-CAPTURE-OUTCOME
   src srcu XE-STORE! ;

: XE-LF ( -- ) 10 SB-APPEND-C ;
: XE-LINE ( ptr u8 n -- ) SB-APPEND XE-LF ;

\ ---- candidate program prelude + emitter -------------------------------------
\ A: a net-+1 word; SP: a stack-preserving word; ACT: a deferred ( n -- n ). The
\ child prints one verdict marker per CHECK-CANDIDATE!, so the parent reads the
\ outcome without depending on checker-internal diagnostics.
: XE-PRELUDE ( -- )
   SB-RESET
   s" require src/lib/errors.f" XE-LINE
   s" require src/lib/string.f" XE-LINE
   s" : A ( n -- n n ) dup ;" XE-LINE
   s" : SP ( n -- n ) 1 + ;" XE-LINE
   s" defer ACT ( n -- n )" XE-LINE
   s" : XE-SHOW ( n -- )" XE-LINE
   S\" dup -1 = if drop .\" <<CERT>>\" cr exit then" XE-LINE
   S\" dup 0=  if drop .\" <<REJ>>\"  cr exit then" XE-LINE
   S\" drop .\" <<UNCK>>\" cr ;" XE-LINE ;

\ The child prints "<tag><<CERT>>" or "<tag><<REJ>>"; cand must hold no '"'.
: XE-CASE-LINE ( ptr u8 n ptr u8 n -- ) {: ta:ptr tu:n ca:ptr cu:n :}
   S\" .\" " SB-APPEND  ta tu SB-APPEND  S\" \"  " SB-APPEND
   S\" s\" " SB-APPEND  ca cu SB-APPEND  S\" \" CHECK-CANDIDATE! XE-SHOW" XE-LINE ;

\ All value cases run in one child; the parent reads "<tag><verdict>" back.
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
: XE-HAS ( ptr u8 n -- ) XE-OUT$ 2swap CONTAINS? TTRUE ;

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

\ ---- `[']` of a using-imported bare name ------------------------------------
\ A tick resolves its token as a call does. Each program runs in a child, so a
\ regression is a red row, not a crashed suite.
70 constant XE-UNDEF-RC

: XE-USING-PRELUDE ( -- )
   SB-RESET
   s" require src/lib/errors.f" XE-LINE
   s" require src/lib/string.f" XE-LINE
   s" package XEU-P" XE-LINE
   s" public" XE-LINE
   s" : XEU-W ( n -- n ) 1 + ;" XE-LINE
   s" ;package" XE-LINE
   s" using XEU-P" XE-LINE ;

\ Each row prints "<tag>:<value>".
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
   s" require src/lib/errors.f" XE-LINE
   s" package XEU-A  public  : XEU-W ( n -- n ) 1 + ;  ;package" XE-LINE
   s" package XEU-B  public  : XEU-W ( n -- n ) 2 + ;  ;package" XE-LINE
   s" using XEU-A" XE-LINE
   s" using XEU-B" XE-LINE
   s" package XEU-AMB" XE-LINE
   a u XE-LINE
   s" ;package" XE-LINE
   SB$ ;

\ The checker's ambiguity refusal, the same at either native tier.
: XE-AMB-DIED ( -- )
   XE-EXITED @ TTRUE
   XE-RC @ 70 T=
   XE-ERR$ s" E-USING-AMBIGUOUS habu: bare 'XEU-W'" CONTAINS? TTRUE ;

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
   s" ['] of a bare name two used packages export dies" T-LABEL
   s" : XEU-TICK ( -- n ) 7 ['] XEU-W drop ;" XE-AMB-PROG$ XE-RUN
   XE-AMB-DIED
   s" a CALL of that bare name dies the same way (same message, same rc)" T-LABEL
   s" : XEU-CALL ( -- n ) 7 XEU-W ;" XE-AMB-PROG$ XE-RUN
   XE-AMB-DIED
   s" ['] of a name nothing resolves is undefined, never a silent no-op" T-LABEL
   s" : XEU-NONE ( -- n ) 7 ['] XEU-NOSUCH drop ;" XE-AMB-PROG$ XE-RUN
   XE-EXITED @ TTRUE  XE-RC @ XE-UNDEF-RC T=
   XE-ERR$ s" E-UNDEFINED" CONTAINS? TTRUE
   XE-ERR$ s" XEU-NOSUCH" CONTAINS? TTRUE ;

: XE-PREPARE ( -- )
   FS:CLEANUP-RESET
   s" habu-xe" FS:MKTEMP-DIR {: a:ptr u:n :}
   a u XE-ROOT-BUF XE-ROOT-U XE-COPY!
   XE-ROOT FS:CLEANUP-TREE+
   XE-ROOT s" cand.f" XE-CHILD-BUF FS:JOIN-PATH XE-CHILD-U ! ;

: XE-CLEANUP ( -- )
   FS:CLEANUP-RUN
   XE-ROOT FS:EXISTS? TFALSE ;

: XE-MAIN ( -- )
   T-RESET
   XE-PREPARE
   XE-VALUES
   XE-USING
   XE-USING-REFUSED
   XE-CLEANUP
   T-REPORT
   s" xt-effect: ok" type cr ;

XE-MAIN

;package
