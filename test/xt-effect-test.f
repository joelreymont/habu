split-str
\ xt-effect-test.f - xt<effect> value regressions (docs/design.md "Typed top
\ level"). `['] W` is xt<effect(W)>, a T-QUOT of W's certified effect;
\ execute, catch and is fit-check it by quotation unification
\ (src/core/checker.f BTICK-TOK).
\ Run: bin/hb --load src/lib/error.f src/lib/string.f src/lib/test.f src/lib/memory.f src/lib/fs.f
\   src/lib/fs-mutate.f src/lib/process.f src/lib/process-argv.f test/xt-effect-test.f

require src/lib/error.f
require src/lib/string.f
require src/lib/test.f
require src/lib/memory.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/test/ending.f
require src/lib/engine-candidate.f

package XT-EFFECT-TEST

4096 constant XE-CAP

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

: XE-COPY! ( str ^u8 ^n -- ) {: src:^ u:n dst:^ lenp:^ :}
   u PATH-CAP > if E-FS-CAPACITY throw then
   src dst u BYTE-COPY
   u lenp ! ;

: XE-ROOT ( -- str ) XE-ROOT-BUF XE-ROOT-U @ ;
: XE-CHILD ( -- str ) XE-CHILD-BUF XE-CHILD-U @ ;
: XE-OUT$ ( -- str ) XE-OUT XE-OUT-U @ ;
: XE-ERR$ ( -- str ) XE-ERR XE-ERR-U @ ;

: XE-STORE! ( len len PROCESS:ending str -- )
   {: outu:len erru:len oc src:^ srcu:n :}
   outu LEN>N XE-OUT-U !  erru LEN>N XE-ERR-U !
   oc MATCH PROCESS:ending
     exited OF XE-RC ! 0 0= XE-EXITED ! ENDOF
     signaled OF XE-RC ! 0 0= 0= XE-EXITED ! ENDOF
     timeout OF src srcu XE-OUT$ XE-ERR$ T-TIMED-OUT ENDOF
   ;MATCH ;

: XE-RUN ( str -- ) {: src:^ srcu:n :}
   XE-CHILD src srcu FS:WRITE-ALL
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   XE-CHILD >LEN PROC-ARGV+
   ENGINE-CANDIDATE:PATH$ >LEN  XE-EMPTY 0 >LEN  XE-OUT XE-CAP >LEN
   XE-ERR XE-CAP >LEN  PROCESS:WAIT-FOREVER  RUN-ARGV-STDIN-CAPTURE-OUTCOME
   src srcu XE-STORE! ;

: XE-LF ( -- ) 10 SB-APPEND-C ;
: XE-LINE ( str -- ) SB-APPEND XE-LF ;

\ ---- candidate program prelude + emitter -------------------------------------
\ A: a net-+1 word; SP: a stack-preserving word; ACT: a deferred ( n -- n ). The
\ child prints one verdict marker per CHECK-CANDIDATE!, so the parent reads the
\ outcome without depending on checker-internal diagnostics.
: XE-PRELUDE ( -- )
   SB-RESET
   s" require src/lib/error.f" XE-LINE
   s" require src/lib/string.f" XE-LINE
   s" : A ( n -- n n ) dup ;" XE-LINE
   s" : SP ( n -- n ) 1 + ;" XE-LINE
   s" defer ACT ( n -- n )" XE-LINE
   s" : XE-SHOW ( n -- )" XE-LINE
   S\" dup -1 = if drop .\" <<CERT>>\" cr exit then" XE-LINE
   S\" dup 0=  if drop .\" <<REJ>>\"  cr exit then" XE-LINE
   S\" drop .\" <<UNCK>>\" cr ;" XE-LINE ;

\ The child prints "<tag><<CERT>>" or "<tag><<REJ>>"; cand must hold no '"'.
: XE-CASE-LINE ( str str -- ) {: ta:^ tu:n ca:^ cu:n :}
   S\" .\" " SB-APPEND  ta tu SB-APPEND  S\" \"  " SB-APPEND
   S\" s\" " SB-APPEND  ca cu SB-APPEND  S\" \" CHECK-CANDIDATE! XE-SHOW" XE-LINE ;

\ All value cases run in one child; the parent reads "<tag><verdict>" back.
: XE-VALUES-PROG$ ( -- str )
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
: XE-HAS ( str -- ) XE-OUT$ 2swap CONTAINS? TTRUE ;

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

: XE-USING-PRELUDE ( -- )
   SB-RESET
   s" require src/lib/error.f" XE-LINE
   s" require src/lib/string.f" XE-LINE
   s" package XEU-P" XE-LINE
   s" public" XE-LINE
   s" : XEU-W ( n -- n ) 1 + ;" XE-LINE
   s" ;package" XE-LINE
   s" using XEU-P" XE-LINE ;

\ Each row prints "<tag>:<value>".
: XE-USING-PROG$ ( -- str )
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
: XE-AMB-PROG$ ( str -- str ) {: a:^ u:n :}
   SB-RESET
   s" require src/lib/error.f" XE-LINE
   s" package XEU-A  public  : XEU-W ( n -- n ) 1 + ;  ;package" XE-LINE
   s" package XEU-B  public  : XEU-W ( n -- n ) 2 + ;  ;package" XE-LINE
   s" using XEU-A" XE-LINE
   s" using XEU-B" XE-LINE
   s" package XEU-AMB" XE-LINE
   a u XE-LINE
   s" ;package" XE-LINE
   SB$ ;

: XE-AMB-DIED ( -- )
   XE-EXITED @ TTRUE
   XE-RC @ 70 T=
   XE-ERR$ s" error[E-USING-AMBIGUOUS]: `XEU-W`" CONTAINS? TTRUE ;

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
   XE-EXITED @ TTRUE  XE-RC @ 70 T=
   XE-ERR$ s" E-UNDEFINED" CONTAINS? TTRUE
   XE-ERR$ s" XEU-NOSUCH" CONTAINS? TTRUE ;

\ ---- fact words in a quotation type -------------------------------------------
\ Each program runs in its own child: a rejection ends its load.
: XE-FACT-REJECTED ( str str -- ) {: src:^ srcu:n want:^ wantu:n :}
   src srcu XE-RUN
   XE-EXITED @ TTRUE  XE-RC @ 70 T=
   XE-ERR$ want wantu CONTAINS? TTRUE ;

: XE-FACT-WORDS ( -- )
   s" a fact word is stated once" T-LABEL
   SB-RESET  s" TYPED-VARIABLE XB [ -- renders renders ]" XE-LINE
   SB$ s" does not parse" XE-FACT-REJECTED
   s" no type follows a fact word" T-LABEL
   SB-RESET  s" TYPED-VARIABLE XD [ -- renders n ]" XE-LINE
   SB$ s" does not parse" XE-FACT-REJECTED
   s" the facts follow the last outputs, never a return clause" T-LABEL
   SB-RESET  s" TYPED-VARIABLE XE [ -- renders | -- ]" XE-LINE
   SB$ s" does not parse" XE-FACT-REJECTED
   s" a typed cell's quotation shows its stated facts" T-LABEL
   SB-RESET  s" TYPED-VARIABLE XV [ -- renders ]" XE-LINE  s" : XG ( -- n ) XV @ ;" XE-LINE
   SB$ s" found: [ -- renders ]" XE-FACT-REJECTED
   s" a field's quotation type keeps its stated facts" T-LABEL
   SB-RESET  s" STRUCTURE xfr DERIVE addr FIELD run [ -- renders ] ;STRUCTURE" XE-LINE
   s" : XF ( ^xfr -- n ) XFR:RUN @ ;" XE-LINE
   SB$ s" found: [ -- renders ]" XE-FACT-REJECTED
   s" a recorded output keeps its quotation's facts" T-LABEL
   SB-RESET
   S\" : EVC ( -- ) s\" 1 drop\" evaluate-closed ;" XE-LINE
   s" : MKR ['] EVC ;" XE-LINE
   s" : XH ( -- n ) MKR ;" XE-LINE
   SB$ s" found: [ -- renders ]" XE-FACT-REJECTED
   s" a literal's term carries its body's facts" T-LABEL
   SB-RESET
   S\" : EVC ( -- ) s\" 1 drop\" evaluate-closed ;" XE-LINE
   s" : MKL [: EVC ;] ;" XE-LINE
   s" : XH ( -- n ) MKL ;" XE-LINE
   SB$ s" found: [ -- renders ]" XE-FACT-REJECTED
   s" a literal that is never run charges nothing" T-LABEL
   SB-RESET
   S\" : EVC ( -- ) s\" 1 drop\" evaluate-closed ;" XE-LINE
   s" : DR [: EVC ;] drop ;" XE-LINE
   s" : XH ( -- n ) ['] DR ;" XE-LINE
   SB$ s" found: [ -- ]" XE-FACT-REJECTED ;

: XE-DEFER-FACTS ( -- )
   s" a defer's effect states facts after its outputs" T-LABEL
   SB-RESET  s" defer DV ( -- renders )" XE-LINE
   s" : MKR ['] DV ;" XE-LINE  s" : XH ( -- n ) MKR ;" XE-LINE
   SB$ s" found: [ -- renders ]" XE-FACT-REJECTED
   s" a call of a defer may throw and keeps no input intact" T-LABEL
   SB-RESET  s" defer DT ( n -- n )" XE-LINE
   s" : R ( n -- n ) ['] DT catch drop 1 + ;" XE-LINE
   SB$ s" E-STALE-READ" XE-FACT-REJECTED
   s" a colon word's effect states no facts" T-LABEL
   SB-RESET  s" : CF ( -- renders ) ;" XE-LINE
   SB$ s" bad signature at `renders`" XE-FACT-REJECTED
   s" a TRUSTED: effect states no facts" T-LABEL
   SB-RESET  s" TRUSTED: TX ( -- renders ) ;" XE-LINE
   SB$ s" the signature does not parse here" XE-FACT-REJECTED
   s" a defer's inputs state no facts" T-LABEL
   SB-RESET  s" defer DI ( renders -- )" XE-LINE
   SB$ s" does not parse" XE-FACT-REJECTED
   s" a defer's facts follow the last outputs, never a return clause" T-LABEL
   SB-RESET  s" defer DE ( -- renders | )" XE-LINE
   SB$ s" does not parse" XE-FACT-REJECTED ;

: XE-FACT-ACCEPTED ( str -- )
   XE-RUN  XE-EXITED @ TTRUE  XE-RC @ 0 T= ;

\ A quotation kept in a slot has only the facts the slot's type states; one a
\ callee only runs charges the caller.
: XE-KEPT-FACTS ( -- )
   s" a typed cell keeps only a quotation its type states the facts of" T-LABEL
   SB-RESET
   S\" : EVC ( -- ) s\" 1 drop\" evaluate-closed ;" XE-LINE
   s" TYPED-VARIABLE XV [ -- ]" XE-LINE  s" : S ( -- ) ['] EVC XV ! ;" XE-LINE
   SB$ s" does more than the type that keeps it states" XE-FACT-REJECTED
   XE-ERR$ s" state `renders`" CONTAINS? TTRUE
   s" a declared output keeps only a quotation its type states the facts of" T-LABEL
   SB-RESET
   S\" : EVC ( -- ) s\" 1 drop\" evaluate-closed ;" XE-LINE
   s" : MK ( -- [ -- ] ) [: EVC ;] ;" XE-LINE
   SB$ s" does more than the type that keeps it states" XE-FACT-REJECTED
   s" a defer keeps only a quotation its effect states the facts of" T-LABEL
   SB-RESET
   S\" : EVC ( -- ) s\" 1 drop\" evaluate-closed ;" XE-LINE
   s" defer DV ( -- )" XE-LINE  s" : IN ( -- ) ['] EVC is DV ;" XE-LINE
   SB$ s" does more than the type that keeps it states" XE-FACT-REJECTED
   s" a parameter the callee keeps takes only a quotation its type states the facts of" T-LABEL
   SB-RESET
   S\" : EVC ( -- ) s\" 1 drop\" evaluate-closed ;" XE-LINE
   s" TYPED-VARIABLE XV [ -- ]" XE-LINE  s" : XKEEP ( [ -- ] -- ) XV ! ;" XE-LINE
   s" : K ( -- ) ['] EVC XKEEP ;" XE-LINE
   SB$ s" does more than the type that keeps it states" XE-FACT-REJECTED
   s" a parameter the callee only runs charges the caller with its facts" T-LABEL
   SB-RESET
   S\" : EVC ( -- ) s\" 1 drop\" evaluate-closed ;" XE-LINE
   s" TYPED-VARIABLE XV [ -- ]" XE-LINE  s" : A ( [ -- ] -- ) execute ;" XE-LINE
   s" : R ( -- ) ['] EVC A ;" XE-LINE  s" : S3 ( -- ) ['] R XV ! ;" XE-LINE
   SB$ s" does more than the type that keeps it states" XE-FACT-REJECTED
   s" a pointer to a quotation cell keeps the cell's facts" T-LABEL
   SB-RESET
   S\" : EVC ( -- ) s\" 1 drop\" evaluate-closed ;" XE-LINE
   s" TYPED-VARIABLE XV [ -- ]" XE-LINE
   s" : W ( ^[ -- renders ] -- ) ['] EVC swap ! ;" XE-LINE  s" : C ( -- ) XV W ;" XE-LINE
   SB$ s" E-MISMATCH" XE-FACT-REJECTED
   s" a pointer to a cell of quotations that take quotations keeps their inputs' facts" T-LABEL
   SB-RESET
   s" TYPED-VARIABLE D [ -- ]" XE-LINE
   s" TYPED-VARIABLE C [ [ -- renders ] -- ]" XE-LINE
   s" : KEEPD ( [ -- ] -- ) D ! ;" XE-LINE
   s" : W ( ^[ [ -- ] -- ] -- ) ['] KEEPD swap ! ;" XE-LINE
   s" : P ( -- ) C W ;" XE-LINE
   SB$ s" E-MISMATCH" XE-FACT-REJECTED
   XE-ERR$ s" : P ( -- ) C W ;" CONTAINS? TTRUE
   s" a join keeps its branches' quotation inputs apart" T-LABEL
   SB-RESET
   S\" : EVC ( -- ) s\" 1 drop\" evaluate-closed ;" XE-LINE
   s" TYPED-VARIABLE XV [ -- ]" XE-LINE  s" : K1 ( [ -- ] -- ) XV ! ;" XE-LINE
   s" : K2 ( [ -- renders ] -- ) execute ;" XE-LINE
   s" : JN ( bool -- ) ['] EVC swap if ['] K2 else ['] K1 then execute ;" XE-LINE
   SB$ s" E-MISMATCH" XE-FACT-REJECTED
   XE-ERR$ s" does more than the type that keeps it states" CONTAINS? TFALSE
   s" a join keeps its branches' quotation inputs apart in either branch order" T-LABEL
   SB-RESET
   S\" : EVC ( -- ) s\" 1 drop\" evaluate-closed ;" XE-LINE
   s" TYPED-VARIABLE XV [ -- ]" XE-LINE  s" : K1 ( [ -- ] -- ) XV ! ;" XE-LINE
   s" : K2 ( [ -- renders ] -- ) execute ;" XE-LINE
   s" : JN ( bool -- ) ['] EVC swap if ['] K1 else ['] K2 then execute ;" XE-LINE
   SB$ s" E-MISMATCH" XE-FACT-REJECTED
   XE-ERR$ s" does more than the type that keeps it states" CONTAINS? TFALSE
   s" a word compile, keeps states no facts" T-LABEL
   SB-RESET
   s" : V ( -- ) parse-name 2drop ;" XE-LINE  s" : C ( -- ) ['] V compile, ;" XE-LINE
   SB$ s" or keep a word without `parses`" XE-FACT-REJECTED
   s" a typed cell whose type states the facts keeps the quotation" T-LABEL
   SB-RESET
   S\" : EVC ( -- ) s\" 1 drop\" evaluate-closed ;" XE-LINE
   s" TYPED-VARIABLE XR [ -- renders ]" XE-LINE  s" : S2 ( -- ) ['] EVC XR ! ;" XE-LINE
   SB$ XE-RUN  XE-EXITED @ TTRUE  XE-RC @ 0 T=
   s" a defer whose effect states the facts keeps the quotation" T-LABEL
   SB-RESET
   S\" : EVC ( -- ) s\" 1 drop\" evaluate-closed ;" XE-LINE
   s" defer DR ( -- renders )" XE-LINE  s" : IR ( -- ) ['] EVC is DR ;" XE-LINE
   SB$ XE-RUN  XE-EXITED @ TTRUE  XE-RC @ 0 T=
   s" a parameter the callee only runs takes any quotation" T-LABEL
   SB-RESET
   S\" : EVC ( -- ) s\" 1 drop\" evaluate-closed ;" XE-LINE
   s" : A ( [ -- ] -- ) execute ;" XE-LINE  s" : R ( -- ) ['] EVC A ;" XE-LINE
   SB$ XE-RUN  XE-EXITED @ TTRUE  XE-RC @ 0 T=
   s" an unsigned word that returns a throwing quotation loads" T-LABEL
   SB-RESET
   s" : T ( n -- n ) drop 7 throw ;" XE-LINE  s" : MK [: T ;] ;" XE-LINE
   SB$ XE-FACT-ACCEPTED ;

: XE-RUN-LINE ( -- ) s" : RUN ( [ -- ] -- ) execute ;" XE-LINE ;

\ A kept quotation does what each quotation its slot hands it and it only runs
\ does, so the slot's type states those facts too; installing it charges nobody.
: XE-RUN-FACTS ( -- )
   s" a defer keeps a runner only if its effect states what the runner runs" T-LABEL
   SB-RESET  XE-RUN-LINE  s" defer DX ( [ -- renders ] -- )" XE-LINE
   s" : INST ( -- ) [: RUN ;] is DX ;" XE-LINE
   SB$ s" state `renders`" XE-FACT-REJECTED
   s" a defer keeps a ticked runner only if its effect states what it runs" T-LABEL
   SB-RESET  XE-RUN-LINE  s" defer DX ( [ -- renders ] -- )" XE-LINE
   s" : INST ( -- ) ['] RUN is DX ;" XE-LINE
   SB$ s" state `renders`" XE-FACT-REJECTED
   s" a defer whose effect states what the runner runs keeps it" T-LABEL
   SB-RESET  XE-RUN-LINE  s" defer DX ( [ -- renders ] -- renders )" XE-LINE
   s" : INST ( -- ) [: RUN ;] is DX ;" XE-LINE
   SB$ XE-FACT-ACCEPTED
   s" a defer whose effect states what the ticked runner runs keeps it" T-LABEL
   SB-RESET  XE-RUN-LINE  s" defer DX ( [ -- renders ] -- renders )" XE-LINE
   s" : INST ( -- ) ['] RUN is DX ;" XE-LINE
   SB$ XE-FACT-ACCEPTED
   s" a typed cell keeps a runner only if its type states what the runner runs" T-LABEL
   SB-RESET  XE-RUN-LINE  s" TYPED-VARIABLE XC [ [ -- renders ] -- ]" XE-LINE
   s" : S ( -- ) [: RUN ;] XC ! ;" XE-LINE
   SB$ s" state `renders`" XE-FACT-REJECTED
   s" a typed cell keeps a ticked runner only if its type states what it runs" T-LABEL
   SB-RESET  XE-RUN-LINE  s" TYPED-VARIABLE XC [ [ -- renders ] -- ]" XE-LINE
   s" : S ( -- ) ['] RUN XC ! ;" XE-LINE
   SB$ s" state `renders`" XE-FACT-REJECTED
   s" a typed cell whose type states what the runner runs keeps it" T-LABEL
   SB-RESET  XE-RUN-LINE  s" TYPED-VARIABLE XC [ [ -- renders ] -- renders ]" XE-LINE
   s" : S ( -- ) [: RUN ;] XC ! ;" XE-LINE
   SB$ XE-FACT-ACCEPTED
   s" a typed cell whose type states what the ticked runner runs keeps it" T-LABEL
   SB-RESET  XE-RUN-LINE  s" TYPED-VARIABLE XC [ [ -- renders ] -- renders ]" XE-LINE
   s" : S ( -- ) ['] RUN XC ! ;" XE-LINE
   SB$ XE-FACT-ACCEPTED
   s" installing a runner charges the installer nothing" T-LABEL
   SB-RESET  XE-RUN-LINE  s" defer DX ( [ -- renders ] -- renders )" XE-LINE
   s" : INST ( -- ) [: RUN ;] is DX ;" XE-LINE
   s" TYPED-VARIABLE XV [ -- ]" XE-LINE  s" : S ( -- ) ['] INST XV ! ;" XE-LINE
   SB$ XE-FACT-ACCEPTED
   s" a declared output whose type states what the runner runs keeps it" T-LABEL
   SB-RESET  XE-RUN-LINE
   s" : MK2 ( -- [ [ -- renders ] -- renders ] ) [: RUN ;] ;" XE-LINE
   SB$ XE-FACT-ACCEPTED
   s" a declared output whose type states what the ticked runner runs keeps it" T-LABEL
   SB-RESET  XE-RUN-LINE
   s" : MK2 ( -- [ [ -- renders ] -- renders ] ) ['] RUN ;" XE-LINE
   SB$ XE-FACT-ACCEPTED
   s" a declared output keeps a runner only if its type states what it runs" T-LABEL
   SB-RESET  XE-RUN-LINE
   s" : MK2 ( -- [ [ -- renders ] -- ] ) [: RUN ;] ;" XE-LINE
   SB$ s" state `renders`" XE-FACT-REJECTED
   s" a quotation type inside a quotation type keeps what it is handed, in either order" T-LABEL
   SB-RESET
   S\" : EVC ( -- ) s\" 1 drop\" evaluate-closed ;" XE-LINE
   s" TYPED-VARIABLE KV [ -- ]" XE-LINE
   s" : W2R ( [ [ -- ] -- ] [ -- ] -- ) swap execute ;" XE-LINE
   s" : C7 ( -- ) [: KV ! ;] [: EVC ;] W2R ;" XE-LINE
   SB$ s" E-MISMATCH" XE-FACT-REJECTED
   XE-ERR$ s" : C7 ( -- ) [: KV ! ;] [: EVC ;] W2R ;" CONTAINS? TTRUE
   SB-RESET
   S\" : EVC ( -- ) s\" 1 drop\" evaluate-closed ;" XE-LINE
   s" TYPED-VARIABLE KV [ -- ]" XE-LINE
   s" : MKK ( -- [ [ -- ] -- ] ) [: KV ! ;] ;" XE-LINE
   s" : W2R ( [ [ -- ] -- ] [ -- ] -- ) swap execute ;" XE-LINE
   s" : C10 ( -- ) MKK [: EVC ;] W2R ;" XE-LINE
   SB$ s" E-MISMATCH" XE-FACT-REJECTED
   XE-ERR$ s" : C10 ( -- ) MKK [: EVC ;] W2R ;" CONTAINS? TTRUE
   s" a quotation kept twice is charged at each keeping" T-LABEL
   SB-RESET  XE-RUN-LINE
   s" TYPED-VARIABLE SV [ [ -- renders ] -- renders ]" XE-LINE
   s" TYPED-VARIABLE XN [ [ -- renders ] -- ]" XE-LINE
   s" : S ( -- ) [: RUN ;] dup SV ! XN ! ;" XE-LINE
   SB$ s" state `renders`" XE-FACT-REJECTED
   XE-ERR$ s" : S ( -- ) [: RUN ;] dup SV ! XN ! ;" CONTAINS? TTRUE
   s" a bodyless defer's output keeps what it is handed" T-LABEL
   SB-RESET
   s" TYPED-VARIABLE KV [ -- ]" XE-LINE
   s" TYPED-VARIABLE SV [ [ -- renders ] -- renders ]" XE-LINE
   s" defer DQ ( -- [ [ -- ] -- ] )" XE-LINE
   s" : MKK3 ( -- [ [ -- ] -- ] ) [: KV ! ;] ;" XE-LINE
   s" : INST ( -- ) ['] MKK3 is DQ ;" XE-LINE
   s" : S1 ( -- ) DQ SV ! ;" XE-LINE
   SB$ s" state `renders`" XE-FACT-REJECTED
   XE-ERR$ s" : S1 ( -- ) DQ SV ! ;" CONTAINS? TTRUE ;

: XE-PREPARE ( -- )
   FS:CLEANUP-RESET
   s" habu-xe" FS:MKTEMP-DIR {: a:^ u:n :}
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
   XE-FACT-WORDS
   XE-DEFER-FACTS
   XE-KEPT-FACTS
   XE-RUN-FACTS
   XE-CLEANUP
   T-REPORT
   s" xt-effect: ok" type cr ;

XE-MAIN

;package
