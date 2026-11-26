\ xt-effect-test.f - xt<effect> value regressions (docs/design.md "Typed top
\ level"). `['] W` is xt<effect(W)>, a T-QUOT of W's certified effect;
\ execute, catch and is fit-check it by quotation unification
\ (src/core/checker.f BTICK-TOK).
\ Run: bin/hb --load src/lib/error.f src/lib/string.f src/lib/memory.f src/lib/fs.f
\   src/lib/fs-mutate.f src/lib/process.f src/lib/process-argv.f test/xt-effect-test.f

require src/lib/error.f
require src/lib/string.f
require src/lib/test/assert.f
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

: XE-COPY! ( str ^u8 ^n -- ) {: src:str dst:^ lenp:^ :}
   src STR:LEN PATH-CAP > if E-FS-CAPACITY throw then
   src STR:DATA dst src STR:LEN BYTE-COPY
   src STR:LEN lenp ! ;

: XE-ROOT ( -- str ) XE-ROOT-BUF XE-ROOT-U @ >str ;
: XE-CHILD ( -- str ) XE-CHILD-BUF XE-CHILD-U @ >str ;
: XE-OUT$ ( -- str ) XE-OUT XE-OUT-U @ >str ;
: XE-ERR$ ( -- str ) XE-ERR XE-ERR-U @ >str ;

: XE-STORE! ( len len PROCESS:ending str -- )
   {: outu:len erru:len oc src:str :}
   outu LEN>N XE-OUT-U !  erru LEN>N XE-ERR-U !
   oc MATCH PROCESS:ending
     exited OF XE-RC ! 0 0= XE-EXITED ! ENDOF
     signaled OF XE-RC ! 0 0= 0= XE-EXITED ! ENDOF
     timeout OF src XE-OUT$ XE-ERR$ T-TIMED-OUT ENDOF
   ;MATCH ;

: XE-RUN ( str -- ) {: src:str :}
   XE-CHILD src FS:WRITE-ALL
   PROC-ARGV-RESET
   s" --load" PROC-ARGV+
   XE-CHILD PROC-ARGV+
   ENGINE-CANDIDATE:PATH$  XE-EMPTY 0 >str  XE-OUT XE-CAP >LEN
   XE-ERR XE-CAP >LEN  PROCESS:WAIT-FOREVER  RUN-ARGV-STDIN-CAPTURE-OUTCOME
   src XE-STORE! ;

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
: XE-CASE-LINE ( str str -- ) {: ts:str cs:str :}
   S\" .\" " SB-APPEND  ts SB-APPEND  S\" \"  " SB-APPEND
   S\" s\" " SB-APPEND  cs SB-APPEND  S\" \" CHECK-CANDIDATE! XE-SHOW" XE-LINE ;

\ All value cases run in one child; the parent reads "<tag><verdict>" back.
: XE-VALUES-PROG$ ( -- str )
   XE-PRELUDE
   s" v1:" s" B ( n -- n n ) ['] A execute"     XE-CASE-LINE
   s" v2:" s" C ( -- n ) ['] A execute"         XE-CASE-LINE
   s" v3:" s" M ( n n -- n n n ) ['] A execute" XE-CASE-LINE
   s" v4:" s" H ( n -- n n ) ['] SP catch"      XE-CASE-LINE
   s" v5:" s" XI ( n -- n n ) ['] A catch"      XE-CASE-LINE
   s" v6:" s" XJ ( -- ) ['] SP is ACT"          XE-CASE-LINE
   s" v7:" s" K ( -- ) ['] A is ACT"            XE-CASE-LINE
   s" v8:" s" F ( -- ) ['] deflinear execute"     XE-CASE-LINE
   s" v9:" s" E ( n -- n ) ['] A +"             XE-CASE-LINE
   SB$ ;

\ ---- assertions --------------------------------------------------------------
: XE-HAS ( str -- ) XE-OUT$ swap STR:CONTAINS? TTRUE ;

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
   s" ['] A catch leaves n n and the code, not ( n -- n n ), and rejects" T-LABEL
   s" v5:<<REJ>>" XE-HAS
   s" ['] SP is ACT (effect fits the defer) certifies" T-LABEL
   s" v6:<<CERT>>" XE-HAS
   s" ['] A is ACT (effect misfits the defer) rejects" T-LABEL
   s" v7:<<REJ>>" XE-HAS
   s" ['] deflinear execute (unsafe definer tick) rejects" T-LABEL
   s" v8:<<REJ>>" XE-HAS
   s" ['] A + (scalar sink) rejects: the tick is a quotation, not a number" T-LABEL
   s" v9:<<REJ>>" XE-HAS ;

\ ---- fact words in a quotation type -------------------------------------------
\ Each program runs in its own child: a rejection ends its load.
: XE-FACT-REJECTED ( str str -- ) {: src:str want:str :}
   src XE-RUN
   XE-EXITED @ TTRUE  XE-RC @ 70 T=
   XE-ERR$ want STR:CONTAINS? TTRUE ;

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
   XE-ERR$ s" state `renders`" STR:CONTAINS? TTRUE
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
   XE-ERR$ s" : P ( -- ) C W ;" STR:CONTAINS? TTRUE
   s" a join keeps its branches' quotation inputs apart" T-LABEL
   SB-RESET
   S\" : EVC ( -- ) s\" 1 drop\" evaluate-closed ;" XE-LINE
   s" TYPED-VARIABLE XV [ -- ]" XE-LINE  s" : K1 ( [ -- ] -- ) XV ! ;" XE-LINE
   s" : K2 ( [ -- renders ] -- ) execute ;" XE-LINE
   s" : JN ( bool -- ) ['] EVC swap if ['] K2 else ['] K1 then execute ;" XE-LINE
   SB$ s" E-MISMATCH" XE-FACT-REJECTED
   XE-ERR$ s" does more than the type that keeps it states" STR:CONTAINS? TFALSE
   s" a join keeps its branches' quotation inputs apart in either branch order" T-LABEL
   SB-RESET
   S\" : EVC ( -- ) s\" 1 drop\" evaluate-closed ;" XE-LINE
   s" TYPED-VARIABLE XV [ -- ]" XE-LINE  s" : K1 ( [ -- ] -- ) XV ! ;" XE-LINE
   s" : K2 ( [ -- renders ] -- ) execute ;" XE-LINE
   s" : JN ( bool -- ) ['] EVC swap if ['] K1 else ['] K2 then execute ;" XE-LINE
   SB$ s" E-MISMATCH" XE-FACT-REJECTED
   XE-ERR$ s" does more than the type that keeps it states" STR:CONTAINS? TFALSE
   s" a word compile, keeps states no facts" T-LABEL
   SB-RESET
   s" : V ( -- ) parse-name drop ;" XE-LINE  s" : C ( -- ) ['] V compile, ;" XE-LINE
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
   XE-ERR$ s" = note: it may run the `[ -- renders ]` the type that keeps it hands it" STR:CONTAINS? TTRUE
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
   s" a quotation type inside a quotation type keeps what it is handed" T-LABEL
   SB-RESET
   S\" : EVC ( -- ) s\" 1 drop\" evaluate-closed ;" XE-LINE
   s" TYPED-VARIABLE KV [ -- ]" XE-LINE
   s" : W2R ( [ [ -- ] -- ] [ -- ] -- ) swap execute ;" XE-LINE
   s" : C7 ( -- ) [: KV ! ;] [: EVC ;] W2R ;" XE-LINE
   SB$ s" E-MISMATCH" XE-FACT-REJECTED
   XE-ERR$ s" : C7 ( -- ) [: KV ! ;] [: EVC ;] W2R ;" STR:CONTAINS? TTRUE
   s" a quotation kept twice is charged at each keeping" T-LABEL
   SB-RESET  XE-RUN-LINE
   s" TYPED-VARIABLE SV [ [ -- renders ] -- renders ]" XE-LINE
   s" TYPED-VARIABLE XN [ [ -- renders ] -- ]" XE-LINE
   s" : S ( -- ) [: RUN ;] dup SV ! XN ! ;" XE-LINE
   SB$ s" state `renders`" XE-FACT-REJECTED
   XE-ERR$ s" : S ( -- ) [: RUN ;] dup SV ! XN ! ;" STR:CONTAINS? TTRUE
   s" a keeping that hands its quotation more than its input type allows names that input" T-LABEL
   SB-RESET
   s" TYPED-VARIABLE KV [ -- ]" XE-LINE
   s" TYPED-VARIABLE SV [ [ -- renders ] -- renders ]" XE-LINE
   s" : MKK2 ( -- [ [ -- ] -- ] ) [: KV ! ;] ;" XE-LINE
   s" : ST2 ( -- ) MKK2 SV ! ;" XE-LINE
   SB$ s" hands it a quotation its input type does not allow" XE-FACT-REJECTED
   XE-ERR$ s" : ST2 ( -- ) MKK2 SV ! ;" STR:CONTAINS? TTRUE
   XE-ERR$ s" state `renders` in the input `[ -- ]` of the quotation's type" STR:CONTAINS? TTRUE
   s" a parameter kept with more than its input type allows is rejected in its own word" T-LABEL
   SB-RESET
   s" TYPED-VARIABLE KV [ -- ]" XE-LINE
   s" TYPED-VARIABLE SV [ [ -- renders ] -- renders ]" XE-LINE
   s" : ST3 ( [ [ -- ] -- ] -- ) SV ! ;" XE-LINE
   SB$ s" hands it a quotation its input type does not allow" XE-FACT-REJECTED
   XE-ERR$ s" : ST3 ( [ [ -- ] -- ] -- ) SV ! ;" STR:CONTAINS? TTRUE
   s" a signed word is saved with the facts its signature states" T-LABEL
   SB-RESET  XE-RUN-LINE
   S\" : EVC ( -- ) s\" 1 drop\" evaluate-closed ;" XE-LINE
   s" TYPED-VARIABLE KV [ -- ]" XE-LINE
   s" : K2 ( bool [ -- ] -- ) dup KV ! swap if drop [: EVC ;] then RUN ;" XE-LINE
   s" : C ( -- ) false ['] EVC K2 ;" XE-LINE
   SB$ s" does more than the type that keeps it states" XE-FACT-REJECTED
   XE-ERR$ s" : C ( -- ) false ['] EVC K2 ;" STR:CONTAINS? TTRUE
   s" a body's join still reaches its output" T-LABEL
   SB-RESET
   S\" : EVC ( -- ) s\" 1 drop\" evaluate-closed ;" XE-LINE
   s" : KJ ( bool [ -- ] -- [ -- ] ) swap if drop [: EVC ;] then ;" XE-LINE
   SB$ s" E-MISMATCH" XE-FACT-REJECTED
   s" a bodyless defer's output keeps what it is handed" T-LABEL
   SB-RESET
   s" TYPED-VARIABLE KV [ -- ]" XE-LINE
   s" TYPED-VARIABLE SV [ [ -- renders ] -- renders ]" XE-LINE
   s" defer DQ ( -- [ [ -- ] -- ] )" XE-LINE
   s" : MKK3 ( -- [ [ -- ] -- ] ) [: KV ! ;] ;" XE-LINE
   s" : INST ( -- ) ['] MKK3 is DQ ;" XE-LINE
   s" : S1 ( -- ) DQ SV ! ;" XE-LINE
   SB$ s" hands it a quotation its input type does not allow" XE-FACT-REJECTED
   XE-ERR$ s" : S1 ( -- ) DQ SV ! ;" STR:CONTAINS? TTRUE
   s" a quotation run where it is made hands on the facts of what it passes on" T-LABEL
   SB-RESET  XE-RUN-LINE
   S\" : EVC ( -- ) s\" 1 drop\" evaluate-closed ;" XE-LINE
   s" TYPED-VARIABLE KV [ -- ]" XE-LINE
   s" : GO ( -- ) ['] EVC [: dup RUN ;] execute KV ! ;" XE-LINE
   SB$ s" E-MISMATCH" XE-FACT-REJECTED
   XE-ERR$ s" : GO ( -- ) ['] EVC [: dup RUN ;] execute KV ! ;" STR:CONTAINS? TTRUE
   s" the note names only a handed quotation with a fact the keeping type lacks" T-LABEL
   SB-RESET
   s" : RUN2 ( [ -- ] [ -- ] -- ) execute execute ;" XE-LINE
   s" TYPED-VARIABLE SB [ [ -- parses ] [ -- renders ] -- parses ]" XE-LINE
   s" : B ( -- ) [: RUN2 ;] SB ! ;" XE-LINE
   SB$ s" it may run the `[ -- renders ]`" XE-FACT-REJECTED
   XE-ERR$ s" it may run the `[ -- parses ]`" STR:CONTAINS? TFALSE
   s" a quotation passed one its input type does not state, kept by no type, names the passing" T-LABEL
   SB-RESET
   S\" : EVC ( -- ) s\" 1 drop\" evaluate-closed ;" XE-LINE
   s" TYPED-VARIABLE KV [ -- ]" XE-LINE
   s" : W ( [ [ -- renders ] -- ] -- ) ['] EVC swap execute ;" XE-LINE
   s" : C ( -- ) [: KV ! ;] W ;" XE-LINE
   SB$ s" the type this quotation is passed as hands it" XE-FACT-REJECTED
   XE-ERR$ s" pass it as a type whose input states no `renders`" STR:CONTAINS? TTRUE ;

: XE-PREPARE ( -- )
   FS:CLEANUP-RESET
   s" habu-xe" FS:MKTEMP-DIR XE-ROOT-BUF XE-ROOT-U XE-COPY!

   XE-ROOT FS:CLEANUP-TREE+
   XE-ROOT s" cand.f" XE-CHILD-BUF FS:JOIN-PATH XE-CHILD-U ! ;

: XE-CLEANUP ( -- )
   FS:CLEANUP-RUN
   XE-ROOT FS:EXISTS? TFALSE ;

: XE-MAIN ( -- )
   T-RESET
   XE-PREPARE
   XE-VALUES
   XE-FACT-WORDS
   XE-DEFER-FACTS
   XE-KEPT-FACTS
   XE-RUN-FACTS
   XE-CLEANUP
   T-REPORT
   s" xt-effect: ok" type cr ;

XE-MAIN

;package
