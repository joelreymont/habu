\ signature-nesting.f - a signature nested past the parser's limits refuses by
\ name through the real load path: a 33rd nested binder, and pointees or
\ quotations nested past EFFECT-DEPTH-MAX. A body nested past CF-FRAMES-MAX
\ control frames, a RECURSE with no declared effect and a RECURSE in a does>
\ clause are uncheckable for a reason they name too, and each says so in its
\ JSON packet with help of its own; the packet of a RECURSE with no declared
\ effect has no declared note and is in the record form. A token that fails
\ before the frames overflow keeps its mismatch in prose, packet and
\ CHECK-CANDIDATE!'s answer; a failure pinned before a name a rendering
\ statement may define keeps its packet and answer, and that name alone defers
\ its definition. An undefined word is refused in packet and answer. A mismatch
\ over cells below prints its expected row over them too, so it is a type fault
\ and a surplus a depth fault; one over an open input (a quotation's or an
\ unsigned definition's) prints its found row over the cells it would take, so
\ it is a type fault too, while one over a row the signature declares is a
\ missing value. The tokens after a refusal
\ check in CPU time linear in their count.
\ A refusal ends the load, so each refused text runs in a child; the deepest
\ are stored rows (`trust`). A packet is read in this process. An effect split
\ by line breaks and tabs loads, and an unknown type ending a line refuses by name.
\ A pointer is spelled ^T: `ptr` in a signature, a stored signature or a local's
\ annotation is an unknown type, and a `^` a blank parts from its element type
\ is a bare `^`. A local's annotation verdict names and places its type.
require src/lib/test.f
require src/lib/string.f
require src/lib/adt/result.f
require src/lib/test/subject.f
require src/lib/test/ending.f
require tools/gate-json-assert-core.f
require src/lib/time-cpu.f

package SIGNATURE-NESTING

$10000 constant CAP            \ bytes: the deepest text
10000 constant CHILD-MS
create SRC-BUF CAP allot
create OUT CAP allot
create ERR CAP allot
TYPED-VARIABLE SRC-U len

: SRC-RESET ( -- )
   SRC-U BUF-RESET ;
: S+ ( ^u8 n -- )
   SRC-BUF CAP SRC-U BUF-APPEND ;
: SRC$ ( -- ^u8 n )
   SRC-BUF SRC-U @ LEN>N ;


: S+N ( ^u8 n n -- )
   {: a:^ u:n k:n :}
   k 0 ?do a u S+ loop ;


\ A colon definition NAME whose input is k binders nested around a quotation.
: BINDERS ( ^u8 n n -- )
   {: name:^ nameu:n k:n :}
   SRC-RESET
   s" : " S+  name nameu S+  s"  ( " S+
   s" forall<a," k S+N  s" [ -- ]" S+  s" >" k S+N
   s"  -- ) drop ;" S+ ;


: TRUST-OPEN ( -- )
   SRC-RESET
   S\" : SN-DEEP ( -- ) ; s\" SN-DEEP\" s\" " S+ ;
: TRUST-CLOSE ( -- )
   S\"  --\" trust" S+ ;

\ A stored row whose input is k nested pointees.
: POINTEES ( n -- )
   {: k:n :}
   TRUST-OPEN  s" ^" k S+N  s" n" S+  TRUST-CLOSE ;

\ A stored row whose input is k nested quotations, each the input of the next.
: QUOTATIONS ( n -- )
   {: k:n :}
   TRUST-OPEN  s" [ " k S+N  s" -- ]" S+  s"  -- ]" k 1 - S+N  TRUST-CLOSE ;

\ A colon definition whose signature opens k quotations it never closes.
: DEF-QUOTATIONS ( n -- )
   {: k:n :}
   SRC-RESET
   s" : SN-DEF-DEEP ( " S+  s" [" k S+N  s"  -- ) ;" S+ ;

\ A colon definition whose local's type opens k quotations it never closes.
: LOCAL-QUOTATIONS ( n -- )
   {: k:n :}
   SRC-RESET
   s" : SN-LOCAL-DEEP ( n -- ) {: x:" S+  s" [" k S+N  s"  :} ;" S+ ;

\ A definition NAME whose body nests k `IF`s, from its name to its last token.
: IFS-TEXT ( ^u8 n n -- )
   {: name:^ nameu:n k:n :}
   name nameu S+  s"  ( bool -- ) " S+  s" dup IF " k S+N  s" THEN " k S+N  s" drop" S+ ;

: NESTED-IFS ( ^u8 n n -- )
   {: name:^ nameu:n k:n :}
   SRC-RESET  s" : " S+  name nameu k IFS-TEXT  s"  ;" S+ ;

\ The same nest whose innermost `IF` tests a number, from its name to its last
\ token: the token fails its condition before it overflows the frames.
: BADCOND-TEXT ( n -- )
   {: k:n :}
   s" SN-BADCOND ( bool -- ) " S+  s" dup IF " k 1 - S+N  s" 1 IF " S+
   s" THEN " k S+N  s" drop" S+ ;

: NESTED-BADCOND ( n -- )
   {: k:n :}
   SRC-RESET  s" : " S+  k BADCOND-TEXT  s"  ;" S+ ;

\ A loop over 70 cells, more than a diagnostic prints of a row, whose body
\ leaves one more cell, from its name to its last token.
: BIGLOOP-TEXT ( -- )
   SRC-RESET  s" SN-BIGLOOP ( -- ) " S+  s" 1 " 70 S+N  s" 10 0 DO 1 LOOP" S+
   s"  drop" 70 S+N ;


\ A definition refused at a `THEN` whose arm changes the depth over an empty
\ stack, followed by k `1 drop` pairs the walk still steps through.
: STRAY-THEN ( n -- )
   {: k:n :}
   SRC-RESET  s" SN-STRAY ( bool -- ) IF 1 THEN" S+  s"  1 drop" k S+N ;


\ This thread's CPU nanoseconds for one quiet check of k pairs, which refuses.
: STRAY-NS ( n -- n )
   STRAY-THEN
   TIME:THREAD-CPU-NS  SRC$ CHECK-QUIET-CANDIDATE! VERDICT-REFUSED T=
   TIME:THREAD-CPU-NS swap - ;

\ A definition with no signature that recurses, from its name to its last token.
: RECURSE-TEXT ( -- )   s" SN-DOWN dup 0 > IF 1 - RECURSE THEN" S+ ;

: RECURSE-NOSIG ( -- )
   SRC-RESET  s" : " S+  RECURSE-TEXT  s"  ;" S+ ;

\ A definer whose does> clause recurses.
: RECURSE-DOES ( -- )
   SRC-RESET  s" : SN-MK ( n -- ) create , does> ( -- n ) @ dup 0 > IF RECURSE THEN ;" S+ ;


\ Effects whose blanks are line breaks and tabs: `S\"` writes bytes 10 and 9.
: ADD-BLANKS ( -- )
   SRC-RESET  S\" : SN-ADD (\nn\tn\n-- n\n) + ;" S+ ;
: RESULT-LINE ( -- )
   SRC-RESET  S\" : SN-RESULT-LINE ( result<n,\nn> -- result<n,n> ) ;" S+ ;
: UNKNOWN-LINE ( -- )
   SRC-RESET  S\" : SN-UNKNOWN-LINE ( n sn-nosuch\n-- ) drop drop ;" S+ ;


variable ERR-U

\ The text in SRC-BUF, run as the subject, exits 70 and names why on stderr.
: REFUSES ( ^u8 n -- )
   {: why:^ whyu:n :}
   SRC$ OUT CAP >LEN ERR CAP >LEN CHILD-MS >MS SUBJECT:RUN
   {: outu:len erru:len oc :}
   erru LEN>N ERR-U !
   SRC$ OUT outu LEN>N ERR erru LEN>N oc 70 T-ENDING-EXITED=
   ERR erru LEN>N why whyu CONTAINS? TTRUE ;

\ The last REFUSES's stderr also holds this text.
: SAID ( ^u8 n -- )
   {: a:^ u:n :}
   ERR ERR-U @ a u CONTAINS? TTRUE ;


: PACKET-OPEN ( -- )   REJECTION:CAPTURE  0 0= REJECTION:JSON ! ;

\ REFUSES reads prose from a fork that inherits the format, and a run under
\ `tools/check.f --json-errors` starts in JSON; a packet sets prose back.
variable JSON-WAS   REJECTION:JSON @ JSON-WAS !   false REJECTION:JSON !


\ The packet the text in SRC-BUF writes, checked in this process with JSON
\ diagnostics captured; it stays in REJECTION's capture until the next one.
: PACKET$ ( -- ^u8 n )
   PACKET-OPEN  SRC$ CHECK-CANDIDATE! drop
   0 0= 0= REJECTION:JSON ! REJECTION:BUFFER-OFF  REJECTION:BUFFER$ ;


: PACKET-PASSES ( -- )
   PACKET$ GJA-PARSE GJA-RECORD-FORM ;


\ A caller passes CODE, TOKEN and WHY as packet fields in text; SRC-BUF's packet
\ holds all three: checked as a candidate, or compiled to walk a does> clause.
: PACKET-HOLDS ( ^u8 n ^u8 n ^u8 n -- )
   {: code:^ codeu:n token:^ tokenu:n why:^ whyu:n :}
   REJECTION:BUFFER$ code codeu CONTAINS? TTRUE
   REJECTION:BUFFER$ token tokenu CONTAINS? TTRUE
   REJECTION:BUFFER$ why whyu CONTAINS? TTRUE
   0 0= 0= REJECTION:JSON ! REJECTION:BUFFER-OFF ;


\ CHECK-CANDIDATE! also answers ANSWER: VERDICT-REFUSED or VERDICT-UNCHECKABLE.
: PACKET-HAS ( ^u8 n ^u8 n ^u8 n n -- )
   {: answer:n :}
   PACKET-OPEN  SRC$ CHECK-CANDIDATE! answer T=  PACKET-HOLDS ;


: CLAUSE-PACKET-HAS ( ^u8 n ^u8 n ^u8 n -- )
   PACKET-OPEN  SRC$ TEST-EVAL:RC drop  PACKET-HOLDS ;

public

: RUN ( -- )
   T-RESET
   s" 33 nested binders refuse by the binder limit" T-LABEL
   s" SN-BINDERS-33" 33 BINDERS
   s" error[E-BAD-SIGNATURE]: more than 32 nested binders at `a`" REFUSES
   s" 32 nested binders check" T-LABEL
   s" SN-BINDERS-32" 32 BINDERS
   SRC$ TEST-EVAL:RC 0 T=
   s" 9000 nested pointees refuse by the depth limit" T-LABEL
   9000 POINTEES
   s" effect too deep to record (depth 4097, at most 4096)" REFUSES
   s" 4500 nested quotations refuse by the depth limit" T-LABEL
   4500 QUOTATIONS
   s" effect too deep to record (depth 4097, at most 4096)" REFUSES
   s" a definition's signature nested past the depth limit refuses by it" T-LABEL
   2049 DEF-QUOTATIONS
   s" effect too deep to record (depth 4097, at most 4096)" REFUSES
   s" a local's type nested past the depth limit refuses by it" T-LABEL
   2049 LOCAL-QUOTATIONS
   s" error[E-BAD-SIGNATURE]: effect too deep to record (depth 4097, at most 4096) at `" REFUSES
   s" 33 nested IFs are uncheckable by the control-frame limit" T-LABEL
   s" SN-IFS-33" 33 NESTED-IFS
   s" error[E-UNCHECKABLE]: control flow nested too deep to check (depth 33, at most 32)" REFUSES
   s" = evaluated: ... dup IF dup IF dup IF dup IF dup IF dup IF THEN THEN" SAID  s" one structure too deep" SAID
   s" 32 nested IFs check" T-LABEL
   s" SN-IFS-32" 32 NESTED-IFS
   SRC$ TEST-EVAL:RC 0 T=
   s" 33 nested IFs write an uncheckable packet that names the limit" T-LABEL
   SRC-RESET s" SN-IFS-33" 33 IFS-TEXT
   S\" \"code\":\"E-UNCHECKABLE\"" S\" \"token\":\"IF\""
   S\" \"message\":\"control flow nested too deep to check (depth 33, at most 32)\""
   VERDICT-UNCHECKABLE PACKET-HAS
   s" 33 nested IFs suggest a named word for the inner structures" T-LABEL
   SRC-RESET s" SN-IFS-33" 33 IFS-TEXT
   S\" \"code\":\"E-UNCHECKABLE\"" S\" \"token\":\"IF\""
   S\" \"help\":\"move the inner control structures into a named word"
   VERDICT-UNCHECKABLE PACKET-HAS
   s" a wrong condition at the 33rd nested IF reports the mismatch" T-LABEL
   33 NESTED-BADCOND
   s" error[E-MISMATCH]: `IF` takes bool bool; the stack holds bool n" REFUSES
   s" a wrong condition at the 33rd nested IF writes a rejected packet" T-LABEL
   SRC-RESET 33 BADCOND-TEXT
   S\" \"code\":\"E-MISMATCH\"" S\" \"token\":\"IF\""
   S\" \"expected\":\"bool bool\",\"found\":\"bool n\"" VERDICT-REFUSED PACKET-HAS
   s" an undefined word is refused and writes a rejected packet" T-LABEL
   SRC-RESET  s" SN-UNDEF ( -- ) SN-NO-SUCH-WORD" S+
   S\" \"code\":\"E-UNDEFINED\"" S\" \"token\":\"SN-NO-SUCH-WORD\""
   S\" \"message\":\"cannot find word `SN-NO-SUCH-WORD`\"" VERDICT-REFUSED PACKET-HAS
   s" a word the engine holds that no row answers is refused and writes a rejected packet" T-LABEL
   SRC-RESET  s" SN-PRIVPRIM ( -- ) 0 0 0 xref-retarget" S+
   S\" \"code\":\"E-UNDEFINED\"" S\" \"token\":\"xref-retarget\""
   S\" \"message\":\"cannot find word `xref-retarget`\"" VERDICT-REFUSED PACKET-HAS
   s" a scalar ?dup is refused as an undefined word and writes a rejected packet" T-LABEL
   SRC-RESET  s" SN-QDUP ( n -- ) ?dup drop" S+
   S\" \"code\":\"E-UNDEFINED\"" S\" \"token\":\"?dup\""
   S\" \"message\":\"cannot find word `?dup`\"" VERDICT-REFUSED PACKET-HAS
   s" a call's wrong input over cells below is a type fault over the same cells" T-LABEL
   SRC-RESET  s" : SN-CF ( n bool -- ) drop drop ;" S+  SRC$ TEST-EVAL:RC 0 T=
   SRC-RESET  s" SN-CALLB ( n -- ) 1 2 SN-CF drop" S+
   S\" \"code\":\"E-MISMATCH\"" S\" \"token\":\"SN-CF\""
   S\" \"expected\":\"n n bool\",\"found\":\"n n n\"" VERDICT-REFUSED PACKET-HAS
   s" a RECURSE's wrong input over cells below is a type fault over the same cells" T-LABEL
   SRC-RESET  s" SN-RC1 ( n bool -- ) IF dup 1 RECURSE THEN drop" S+
   S\" \"code\":\"E-MISMATCH\"" S\" \"token\":\"RECURSE\""
   S\" \"expected\":\"n n bool\",\"found\":\"n n n\"" VERDICT-REFUSED PACKET-HAS
   s" a quotation's wrong condition over cells below is a type fault over the same cells" T-LABEL
   SRC-RESET  s" SN-Q1 ( -- ) [: 0 0= 1 IF THEN drop ;] drop" S+
   S\" \"code\":\"E-MISMATCH\"" S\" \"token\":\"IF\""
   S\" \"expected\":\"bool bool\",\"found\":\"bool n\"" VERDICT-REFUSED PACKET-HAS
   s" a does> clause's wrong condition over cells below is a type fault over the same cells" T-LABEL
   SRC-RESET  s" : SN-DO1 ( n -- ) create , does> ( -- ) drop 0 0= 1 IF THEN drop ;" S+
   S\" \"code\":\"E-MISMATCH\"" S\" \"token\":\"IF\""
   S\" \"expected\":\"bool bool\",\"found\":\"bool n\"" CLAUSE-PACKET-HAS
   s" a loop body that leaves one more cell over 70 cells is a surplus" T-LABEL
   BIGLOOP-TEXT
   S\" \"code\":\"E-MISMATCH\"" S\" \"token\":\"LOOP\""
   S\" \"help\":\"drop the extra value" VERDICT-REFUSED PACKET-HAS
   s" a surplus value over cells below stays a surplus" T-LABEL
   SRC-RESET  s" SN-SU1 ( n -- n ) 1" S+
   S\" \"code\":\"E-MISMATCH\"" S\" \"token\":\"1\""
   S\" \"expected\":\"n\",\"found\":\"n n\"" VERDICT-REFUSED PACKET-HAS
   s" a generic bundle input over a caller's group is a type fault over the same cells" T-LABEL
   SRC-RESET  s" STRUCTURE snz<a,b> FIELD fst a FIELD snd b ;STRUCTURE" S+  SRC$ TEST-EVAL:RC 0 T=
   SRC-RESET  s" : SN-SNZ ( n n -- snz<n,n> ) SNZ:MAKE ;" S+  SRC$ TEST-EVAL:RC 0 T=
   SRC-RESET  s" : SN-LHT ( snz<a,b> bool -- ) drop SNZ:UNMAKE drop drop ;" S+  SRC$ TEST-EVAL:RC 0 T=
   SRC-RESET  s" SN-LH1 ( n -- ) 1 2 SN-SNZ 3 SN-LHT drop" S+
   S\" \"code\":\"E-MISMATCH\"" S\" \"token\":\"SN-LHT\""
   S\" \"expected\":\"n snz<a,b> bool\",\"found\":\"n snz<n,n> n\"" VERDICT-REFUSED PACKET-HAS
   s" a group input over a generic bundle is a type fault over the same cells" T-LABEL
   SRC-RESET  s" : SN-LHN ( snz<n,n> bool -- ) drop SNZ:UNMAKE drop drop ;" S+  SRC$ TEST-EVAL:RC 0 T=
   SRC-RESET  s" SN-LH2 ( n snz<a,b> -- ) 3 SN-LHN drop" S+
   S\" \"code\":\"E-MISMATCH\"" S\" \"token\":\"SN-LHN\""
   S\" \"expected\":\"n snz<n,n> bool\",\"found\":\"n snz<a,b> n\"" VERDICT-REFUSED PACKET-HAS
   s" a scalar input over a generic bundle is a type fault" T-LABEL
   SRC-RESET  s" SN-GL0 ( snz<a,b> -- ) 1+ drop" S+
   S\" \"code\":\"E-MISMATCH\"" S\" \"token\":\"1+\""
   S\" \"expected\":\"n\",\"found\":\"snz<a,b>\"" VERDICT-REFUSED PACKET-HAS
   s" a generic bundle input over scalars is a type fault" T-LABEL
   SRC-RESET  s" : SN-TGL ( snz<a,b> -- ) SNZ:UNMAKE drop drop ;" S+  SRC$ TEST-EVAL:RC 0 T=
   SRC-RESET  s" SN-GM2 ( n -- ) 7 8 9 SN-TGL drop" S+
   S\" \"code\":\"E-MISMATCH\"" S\" \"token\":\"SN-TGL\""
   S\" \"expected\":\"n n n snz<a,b>\",\"found\":\"n n n n\"" VERDICT-REFUSED PACKET-HAS
   s" an if arm that turns a cell into a group is a depth fault counted in cells" T-LABEL
   SRC-RESET  s" SN-IFZ ( n bool -- n ) IF dup SN-SNZ THEN" S+
   S\" \"code\":\"E-MISMATCH\"" S\" \"token\":\"THEN\""
   S\" \"message\":\"the if arm changes the stack's depth\"" VERDICT-REFUSED PACKET-HAS
   s" the tokens after a refusal check in time linear in their count" T-LABEL
   \ Eight times the tokens takes about 8 times the CPU time when each token's
   \ cost is bounded, and up to 64 when it grows with the tokens before it.
   1000 STRAY-NS {: small:n :}   8000 STRAY-NS {: big:n :}
   big  small 24 *  < TTRUE
   s" a quotation's wrong input over its open input is a type fault over the same values" T-LABEL
   SRC-RESET  s" SN-QS2 ( n -- ) [: 1 SN-CF ;] execute" S+
   S\" \"code\":\"E-MISMATCH\"" S\" \"token\":\"SN-CF\""
   S\" \"expected\":\"n bool\",\"found\":\"n n\"" VERDICT-REFUSED PACKET-HAS
   s" a quotation's wrong input over a group it takes from below is a type fault" T-LABEL
   SRC-RESET  s" SN-QG ( snz<n,n> -- ) [: 1 SN-LHN ;] execute" S+
   S\" \"code\":\"E-MISMATCH\"" S\" \"token\":\"SN-LHN\""
   S\" \"expected\":\"snz<n,n> bool\",\"found\":\"snz<n,n> n\"" VERDICT-REFUSED PACKET-HAS
   s" an unsigned definition's wrong input over its inferred input is a type fault" T-LABEL
   SRC-RESET  s" SN-QI 1 SN-CF" S+
   S\" \"code\":\"E-MISMATCH\"" S\" \"token\":\"SN-CF\""
   S\" \"expected\":\"n bool\",\"found\":\"n n\"" VERDICT-REFUSED PACKET-HAS
   s" a call short against a declared empty input is a missing producer" T-LABEL
   SRC-RESET  s" SN-S ( -- ) 1 SN-CF" S+
   S\" \"code\":\"E-MISMATCH\"" S\" \"token\":\"SN-CF\""
   S\" \"expected\":\"n bool\",\"found\":\"n\"" VERDICT-REFUSED PACKET-HAS
   s" a call short against a named row is a missing producer" T-LABEL
   SRC-RESET  s" SN-SX ( R [ R -- S ] -- S ) execute 1 SN-CF" S+
   S\" \"code\":\"E-MISMATCH\"" S\" \"token\":\"SN-CF\""
   S\" \"expected\":\"n bool\",\"found\":\"n\"" VERDICT-REFUSED PACKET-HAS
   s" a RECURSE with no declared effect is uncheckable for want of one, and says to declare one" T-LABEL
   RECURSE-NOSIG
   s" error[E-UNCHECKABLE]: recursion needs a declared stack effect" REFUSES
   s" = evaluated: : SN-DOWN dup 0 > IF 1 - RECURSE THEN ;" SAID  s" no effect to call" SAID
   s" help: declare the definition's stack effect after its name" SAID
   s" a RECURSE with no declared effect writes an uncheckable packet that says why" T-LABEL
   SRC-RESET RECURSE-TEXT
   S\" \"code\":\"E-UNCHECKABLE\"" S\" \"token\":\"RECURSE\""
   S\" \"message\":\"recursion needs a declared stack effect\"" VERDICT-UNCHECKABLE PACKET-HAS
   s" a RECURSE with no declared effect suggests declaring one" T-LABEL
   S\" \"code\":\"E-UNCHECKABLE\"" S\" \"token\":\"RECURSE\""
   S\" \"help\":\"declare the definition's stack effect after its name" VERDICT-UNCHECKABLE PACKET-HAS
   s" a RECURSE with no declared effect writes no declared note" T-LABEL
   PACKET-OPEN  SRC$ CHECK-CANDIDATE! VERDICT-UNCHECKABLE T=
   REJECTION:BUFFER$ S\" \"code\":\"E-UNCHECKABLE\"" CONTAINS? TTRUE
   REJECTION:BUFFER$ S\" \"declared\"" CONTAINS? TFALSE
   0 0= 0= REJECTION:JSON ! REJECTION:BUFFER-OFF
   PACKET-PASSES
   s" a RECURSE in a does> clause is uncheckable for calling the defining word, and says where to recurse" T-LABEL
   RECURSE-DOES
   s" error[E-UNCHECKABLE]: recurse in a does> clause would call the defining word" REFUSES
   s" = evaluated: : SN-MK ( n -- ) create , does>" SAID  s" inside a `does>` clause" SAID
   s" help: move the recursion into a named word" SAID
   s" a RECURSE in a does> clause writes an uncheckable packet that says why" T-LABEL
   RECURSE-DOES
   S\" \"code\":\"E-UNCHECKABLE\"" S\" \"token\":\"RECURSE\""
   S\" \"message\":\"recurse in a does> clause would call the defining word\"" CLAUSE-PACKET-HAS
   s" a RECURSE in a does> clause suggests a named word to recurse in" T-LABEL
   RECURSE-DOES
   S\" \"code\":\"E-UNCHECKABLE\"" S\" \"token\":\"RECURSE\""
   S\" \"help\":\"move the recursion into a named word" CLAUSE-PACKET-HAS
   s" an effect with line breaks after (, before -- and before ) and a tab between its inputs loads" T-LABEL
   ADD-BLANKS  SRC$ TEST-EVAL:RC 0 T=   s" 1 2 SN-ADD" TEST-EVAL:N 3 T=
   s" a line break after a comma inside a family loads" T-LABEL
   RESULT-LINE  SRC$ TEST-EVAL:RC 0 T=
   s" an unknown type that ends a line refuses by its name alone" T-LABEL
   UNKNOWN-LINE
   s" error[E-UNKNOWN-SIGNATURE-TYPE]: unknown type `sn-nosuch`" REFUSES
   s" a type mismatch prints its expected and found rows" T-LABEL
   SRC-RESET  s" : SN-MIS ( -- bool ) 1 ;" S+
   s" error[E-MISMATCH]: SN-MIS ends with n on the stack; its signature promises bool" REFUSES
   s" = expected: bool" SAID  s" = found: n" SAID
   s" a bare ^ in a signature is rejected for its missing element type" T-LABEL
   SRC-RESET  s" : SN-BP ( ^ -- ) drop ;" S+
   s" error[E-BARE-PTR-SIGNATURE]: `^` needs an element type" REFUSES
   s" a ^ a blank parts from its element type is a bare ^" T-LABEL
   SRC-RESET  s" : SN-BPU ( ^ u8 -- ) drop ;" S+
   s" error[E-BARE-PTR-SIGNATURE]: `^` needs an element type" REFUSES
   SRC-RESET  s" SN-BPU ( ^ u8 -- ) drop" S+
   S\" \"code\":\"E-BARE-PTR-SIGNATURE\"" S\" \"token\":\"^\""
   S\" \"message\":\"`^` needs an element type\"" VERDICT-REFUSED PACKET-HAS
   s" ptr in a signature is an unknown type: a pointer is ^T" T-LABEL
   SRC-RESET  s" : SN-PTR ( ptr u8 -- ) drop ;" S+
   s" error[E-UNKNOWN-SIGNATURE-TYPE]: unknown type `ptr`" REFUSES
   S\" = help: spell a pointer `^T`, `^` joined to the type it points at, as in `^u8`\n" SAID
   SRC-RESET  s" SN-PTR ( ptr u8 -- ) drop" S+
   S\" \"code\":\"E-UNKNOWN-SIGNATURE-TYPE\"" S\" \"token\":\"ptr\""
   S\" \"message\":\"unknown type `ptr` in signature\"" VERDICT-REFUSED PACKET-HAS
   s" ptr in a local's annotation is an unknown type" T-LABEL
   SRC-RESET  s" : SN-PTRL ( ^u8 -- ) {: p:ptr :} ;" S+
   s" error[E-UNKNOWN-SIGNATURE-TYPE]: unknown type `ptr` in the annotation of local `p`" REFUSES
   S\" = help: spell a pointer `^T`, `^` joined to the type it points at, as in `p:^u8`\n" SAID
   SRC-RESET  s" SN-PTRL ( ^u8 -- ) {: p:ptr :}" S+
   S\" \"code\":\"E-UNKNOWN-SIGNATURE-TYPE\"" S\" \"token\":\"ptr\""
   S\" \"message\":\"unknown type `ptr` in the annotation of local `p`\"" VERDICT-REFUSED PACKET-HAS
   s" a local's annotation of the wrong arity names its type, its local and the type's place" T-LABEL
   SRC-RESET  s" : SN-AR ( n -- ) {: p:option<n,n> :} ;" S+
   s" error[E-WRONG-ARITY]: type family `option<n,n>` takes 1 type argument, not 2 in the annotation of local `p`" REFUSES
   S\" ^ wrong number of type arguments\n" SAID
   S\" = evaluated: : SN-AR ( n -- ) {: p:option<n,n> :} ;\n" SAID
   S\"  ^^^^^^^^^^^\n" SAID
   S\" = help: give `option<n,n>` 1 type argument\n" SAID
   s" ptr in a stored signature is an unknown type: a pointer is ^T" T-LABEL
   SRC-RESET  s" defer SN-PTRD ( ptr -- )" S+
   s" error[E-BAD-STORED-SIGNATURE]: the signature of `SN-PTRD` cannot be recorded" REFUSES
   S\" = help: spell a pointer `^T`, `^` joined to the type it points at, as in `^u8`\n" SAID
   s" a trust-boundary primitive in a checked body is rejected by name" T-LABEL
   SRC-RESET  s" : SN-CAP ( -- ) reloc-maps-clear ;" S+
   s" error[E-CAP-TRUSTED]: `reloc-maps-clear` is a trust-boundary primitive" REFUSES ;

\ Run while a package holding a rendering statement's mark is open, so MADE is
\ a name the run may define to these texts and to no other.
: UNSEEN-CASES ( -- )
   s" an is target the run may define defers its definition to the run" T-LABEL
   SRC-RESET  s" SN-IS-UNSEEN ( -- ) ['] T-RESET is MADE-D" S+
   SRC$ CHECK-CANDIDATE! VERDICT-DEFERRED T=
   s" a covered name after a pinned reason keeps the reason" T-LABEL
   SRC-RESET  s" SN-LOOPLESS ( -- ) i drop MADE" S+
   S\" \"code\":\"E-NO-LOOP\"" S\" \"token\":\"i\""
   S\" \"message\":\"no loop is open here\"" VERDICT-REFUSED PACKET-HAS
   s" a covered name after a pinned mismatch keeps the mismatch" T-LABEL
   SRC-RESET  s" SN-MISMATCH ( -- ) 1 IF THEN MADE" S+
   S\" \"code\":\"E-MISMATCH\"" S\" \"token\":\"IF\""
   S\" \"expected\":\"bool\",\"found\":\"n\"" VERDICT-REFUSED PACKET-HAS ;

: REPORT ( -- )
   JSON-WAS @ REJECTION:JSON !
   T-REPORT
   s" signature-nesting: ok" type cr ;
;package

SIGNATURE-NESTING:RUN

\ The mark covers this package alone.
package UNSEEN-NAMES
CHECKER-UNSEEN-MARK
SIGNATURE-NESTING:UNSEEN-CASES
;package

SIGNATURE-NESTING:REPORT
