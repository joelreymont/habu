\ signature-nesting.f - a signature nested past the parser's limits refuses by
\ name through the real load path: a 33rd nested binder (checker.f
\ BINDER-DEPTH-MAX), and pointees or quotations nested past EFFECT-DEPTH-MAX.
\ A body nested past CF-FRAMES-MAX control frames, a RECURSE with no declared
\ effect and a RECURSE in a does> clause are uncheckable for a reason they name
\ too, and each says so in its JSON packet with a suggestion of its own; the
\ packet of a RECURSE with no declared effect has a null declared_effect, which
\ the diagnostics gate accepts. A token that fails before the frames overflow
\ keeps its mismatch in prose, packet and CHECK-CANDIDATE!'s answer; a failure
\ pinned before a name a rendering statement may define, or a compile-time
\ refusal after it in an unfit definition, keeps its packet and answer, and that
\ name alone defers its definition, or makes an unfit one uncheckable for its
\ width. An undefined word is refused in packet and answer, alone or after a
\ covered name in an unfit definition. A refusal's prose line ends with its
\ packet's suggestion, after the rows when it prints them. A mismatch over cells
\ below prints its expected row over them too, so it is classed by type and a
\ surplus by depth. The tokens after a refusal check in CPU time linear in
\ their count.
\ A refusal ends the load, so each refused text runs in a child; the deepest
\ are stored rows (`trust`). A packet is read in this process. An effect split
\ by line breaks and tabs loads, and an unknown type ending a line refuses by name.
require src/lib/test.f
require src/lib/string.f
require src/lib/adt/result.f
require src/lib/test/subject.f
require src/lib/test/ending.f
require tools/gate-json-assert-core.f
require src/lib/time-cpu.f

package SIGNATURE-NESTING

$10000 constant CAP            \ bytes: the deepest text and its echo on stderr
10000 constant CHILD-MS
create SRC-BUF CAP allot
create OUT CAP allot
create ERR CAP allot
TYPED-VARIABLE SRC-U len

: SRC-RESET ( -- )
   SRC-U BUF-RESET ;
: S+ ( ptr u8 n -- )
   SRC-BUF CAP SRC-U BUF-APPEND ;
: SRC$ ( -- ptr u8 n )
   SRC-BUF SRC-U @ LEN>N ;


: S+N ( ptr u8 n n -- )
   {: a:ptr u:n k:n :}
   k 0 ?do a u S+ loop ;


\ A colon definition NAME whose input is k binders nested around a quotation.
: BINDERS ( ptr u8 n n -- )
   {: name:ptr nameu:n k:n :}
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
   TRUST-OPEN  s" ptr " k S+N  s" n" S+  TRUST-CLOSE ;

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
: IFS-TEXT ( ptr u8 n n -- )
   {: name:ptr nameu:n k:n :}
   name nameu S+  s"  ( bool -- ) " S+  s" dup IF " k S+N  s" THEN " k S+N  s" drop" S+ ;

: NESTED-IFS ( ptr u8 n n -- )
   {: name:ptr nameu:n k:n :}
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

\ A loop over 70 cells, more than a diagnostic prints of a row (RBUF-CAP), whose
\ body leaves one more cell, from its name to its last token.
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

\ A definition NAME taking 256 cells, one more than a record holds, whose body
\ is BODY, then drops them all.
: WIDE-TEXT ( ptr u8 n ptr u8 n -- )
   {: name:ptr nameu:n body:ptr bodyu:n :}
   SRC-RESET  name nameu S+  s"  ( " S+  s" n " 256 S+N  s" -- ) " S+
   body bodyu S+  s"  drop" 256 S+N ;

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


\ The text in SRC-BUF, run as the subject, exits 70 and names why on stderr.
: REFUSES ( ptr u8 n -- )
   {: why:ptr whyu:n :}
   SRC$ OUT CAP >LEN ERR CAP >LEN CHILD-MS >MS SUBJECT:RUN
   {: outu:len erru:len oc :}
   SRC$ OUT outu LEN>N ERR erru LEN>N oc 70 T-ENDING-EXITED=
   ERR erru LEN>N why whyu CONTAINS? TTRUE ;


create PKT-BUF 8192 allot
: PACKET-OPEN ( -- )   PKT-BUF 8192 DIAG-BUFFER!  0 0= DIAG-JSON! ;


\ The packet the text in SRC-BUF writes, checked in this process with JSON
\ diagnostics captured; it stays in PKT-BUF until the next capture.
: PACKET$ ( -- ptr u8 n )
   PACKET-OPEN  SRC$ CHECK-CANDIDATE! drop
   DIAG-BUFFER$ nip  0 0= 0= DIAG-JSON! DIAG-BUFFER-OFF  PKT-BUF swap ;


\ The packet passes the diagnostics gate, which adds to PACKET-HAS's substrings
\ each field's presence and kind, the declared pair null together, and the
\ verdict and repair class its code names.
: PACKET-PASSES ( -- )
   PACKET$ GJA-PARSE dup GJA-OBJ GJA-DIAG-CONTRACT-ROW ;


\ A caller passes CODE, VERDICT and WHY as packet fields in text; SRC-BUF's packet
\ holds all three: checked as a candidate, or compiled to walk a does> clause.
: PACKET-HOLDS ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: code:ptr codeu:n verdict:ptr verdictu:n why:ptr whyu:n :}
   DIAG-BUFFER$ code codeu CONTAINS? TTRUE
   DIAG-BUFFER$ verdict verdictu CONTAINS? TTRUE
   DIAG-BUFFER$ why whyu CONTAINS? TTRUE
   0 0= 0= DIAG-JSON! DIAG-BUFFER-OFF ;


\ CHECK-CANDIDATE! also answers ANSWER: VERDICT-REFUSED or VERDICT-UNCHECKABLE.
: PACKET-HAS ( ptr u8 n ptr u8 n ptr u8 n n -- )
   {: answer:n :}
   PACKET-OPEN  SRC$ CHECK-CANDIDATE! answer T=  PACKET-HOLDS ;


: CLAUSE-PACKET-HAS ( ptr u8 n ptr u8 n ptr u8 n -- )
   PACKET-OPEN  SRC$ TEST-EVAL:RC drop  PACKET-HOLDS ;

public

: RUN ( -- )
   T-RESET
   s" 33 nested binders refuse by the binder limit" T-LABEL
   s" SN-BINDERS-33" 33 BINDERS
   s" more than 32 nested binders at 'a'" REFUSES
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
   s" effect too deep to record (depth 4097, at most 4096) at '" REFUSES
   s" 33 nested IFs are uncheckable by the control-frame limit" T-LABEL
   s" SN-IFS-33" 33 NESTED-IFS
   s" at 'IF' control flow nested too deep to check (depth 33, at most 32)" REFUSES
   s" 32 nested IFs check" T-LABEL
   s" SN-IFS-32" 32 NESTED-IFS
   SRC$ TEST-EVAL:RC 0 T=
   s" 33 nested IFs write an uncheckable packet that names the limit" T-LABEL
   SRC-RESET s" SN-IFS-33" 33 IFS-TEXT
   S\" \"code\":\"E-UNCHECKABLE\"" S\" \"verdict\":\"uncheckable\""
   S\" \"reason\":\"control flow nested too deep to check (depth 33, at most 32)\""
   VERDICT-UNCHECKABLE PACKET-HAS
   s" 33 nested IFs suggest a named word for the inner structures" T-LABEL
   SRC-RESET s" SN-IFS-33" 33 IFS-TEXT
   S\" \"code\":\"E-UNCHECKABLE\"" S\" \"verdict\":\"uncheckable\""
   S\" \"suggestion\":\"Move the inner control structures into a named word and call it there; each definition counts its own control frames.\""
   VERDICT-UNCHECKABLE PACKET-HAS
   s" a wrong condition at the 33rd nested IF reports the mismatch" T-LABEL
   33 NESTED-BADCOND
   s" at 'IF' expected: bool bool actual: bool n ; suggestion: Change the body so produced types match the signature." REFUSES
   s" a wrong condition at the 33rd nested IF writes a rejected packet" T-LABEL
   SRC-RESET 33 BADCOND-TEXT
   S\" \"code\":\"E-MISMATCH\",\"repair_class\":\"fix_type\"" S\" \"verdict\":\"rejected\""
   S\" \"expected\":\"bool bool \",\"actual\":\"bool n \"" VERDICT-REFUSED PACKET-HAS
   s" an undefined word is refused and writes a rejected packet" T-LABEL
   SRC-RESET  s" SN-UNDEF ( -- ) SN-NO-SUCH-WORD" S+
   S\" \"code\":\"E-UNDEFINED\"" S\" \"verdict\":\"rejected\""
   S\" \"token\":\"SN-NO-SUCH-WORD\"" VERDICT-REFUSED PACKET-HAS
   s" a word the engine holds that no row answers is refused and writes a rejected packet" T-LABEL
   SRC-RESET  s" SN-PRIVPRIM ( -- ) 0 0 0 xref-retarget" S+
   S\" \"code\":\"E-UNDEFINED\"" S\" \"verdict\":\"rejected\""
   S\" \"token\":\"xref-retarget\"" VERDICT-REFUSED PACKET-HAS
   s" a scalar ?dup is refused as an undefined word and writes a rejected packet" T-LABEL
   SRC-RESET  s" SN-QDUP ( n -- ) ?dup drop" S+
   S\" \"code\":\"E-UNDEFINED\"" S\" \"verdict\":\"rejected\""
   S\" \"token\":\"?dup\"" VERDICT-REFUSED PACKET-HAS
   s" a call's wrong input over cells below is a type fault over the same cells" T-LABEL
   SRC-RESET  s" : SN-CF ( n bool -- ) drop drop ;" S+  SRC$ TEST-EVAL:RC 0 T=
   SRC-RESET  s" SN-CALLB ( n -- ) 1 2 SN-CF drop" S+
   S\" \"code\":\"E-MISMATCH\",\"repair_class\":\"fix_type\"" S\" \"verdict\":\"rejected\""
   S\" \"expected\":\"n n bool \",\"actual\":\"n n n \"" VERDICT-REFUSED PACKET-HAS
   s" a RECURSE's wrong input over cells below is a type fault over the same cells" T-LABEL
   SRC-RESET  s" SN-RC1 ( n bool -- ) IF dup 1 RECURSE THEN drop" S+
   S\" \"code\":\"E-MISMATCH\",\"repair_class\":\"fix_type\"" S\" \"verdict\":\"rejected\""
   S\" \"expected\":\"n n bool \",\"actual\":\"n n n \"" VERDICT-REFUSED PACKET-HAS
   s" a quotation's wrong condition over cells below is a type fault over the same cells" T-LABEL
   SRC-RESET  s" SN-Q1 ( -- ) [: 0 0= 1 IF THEN drop ;] drop" S+
   S\" \"code\":\"E-MISMATCH\",\"repair_class\":\"fix_type\"" S\" \"verdict\":\"rejected\""
   S\" \"expected\":\"bool bool \",\"actual\":\"bool n \"" VERDICT-REFUSED PACKET-HAS
   s" a does> clause's wrong condition over cells below is a type fault over the same cells" T-LABEL
   SRC-RESET  s" : SN-DO1 ( n -- ) create , does> ( -- ) drop 0 0= 1 IF THEN drop ;" S+
   S\" \"code\":\"E-MISMATCH\",\"repair_class\":\"fix_type\"" S\" \"verdict\":\"rejected\""
   S\" \"expected\":\"bool bool \",\"actual\":\"bool n \"" CLAUSE-PACKET-HAS
   s" a loop body that leaves one more cell over 70 cells is a surplus" T-LABEL
   BIGLOOP-TEXT
   S\" \"code\":\"E-MISMATCH\",\"repair_class\":\"remove_producer\"" S\" \"verdict\":\"rejected\""
   S\" \"suggestion\":\"Remove an extra producer or drop the surplus value.\"" VERDICT-REFUSED PACKET-HAS
   s" a surplus value over cells below stays a surplus" T-LABEL
   SRC-RESET  s" SN-SU1 ( n -- n ) 1" S+
   S\" \"code\":\"E-MISMATCH\",\"repair_class\":\"remove_producer\"" S\" \"verdict\":\"rejected\""
   S\" \"expected\":\"n \",\"actual\":\"n n \"" VERDICT-REFUSED PACKET-HAS
   s" a generic bundle input over a caller's group is a type fault over the same cells" T-LABEL
   SRC-RESET  s" PRODUCT snz<a,b> FIELD fst a FIELD snd b ;PRODUCT" S+  SRC$ TEST-EVAL:RC 0 T=
   SRC-RESET  s" : SN-SNZ ( n n -- snz<n,n> ) SNZ:MAKE ;" S+  SRC$ TEST-EVAL:RC 0 T=
   SRC-RESET  s" : SN-LHT ( snz<a,b> bool -- ) drop SNZ:UNMAKE drop drop ;" S+  SRC$ TEST-EVAL:RC 0 T=
   SRC-RESET  s" SN-LH1 ( n -- ) 1 2 SN-SNZ 3 SN-LHT drop" S+
   S\" \"code\":\"E-MISMATCH\",\"repair_class\":\"fix_type\"" S\" \"verdict\":\"rejected\""
   S\" \"expected\":\"n snz<a,b> bool \",\"actual\":\"n snz<n,n> n \"" VERDICT-REFUSED PACKET-HAS
   s" a group input over a generic bundle is a type fault over the same cells" T-LABEL
   SRC-RESET  s" : SN-LHN ( snz<n,n> bool -- ) drop SNZ:UNMAKE drop drop ;" S+  SRC$ TEST-EVAL:RC 0 T=
   SRC-RESET  s" SN-LH2 ( n snz<a,b> -- ) 3 SN-LHN drop" S+
   S\" \"code\":\"E-MISMATCH\",\"repair_class\":\"fix_type\"" S\" \"verdict\":\"rejected\""
   S\" \"expected\":\"n snz<n,n> bool \",\"actual\":\"n snz<a,b> n \"" VERDICT-REFUSED PACKET-HAS
   s" a scalar input over a generic bundle is a type fault" T-LABEL
   SRC-RESET  s" SN-GL0 ( snz<a,b> -- ) 1+ drop" S+
   S\" \"code\":\"E-MISMATCH\",\"repair_class\":\"fix_type\"" S\" \"verdict\":\"rejected\""
   S\" \"suggestion\":\"Change the body so produced types match the signature.\"" VERDICT-REFUSED PACKET-HAS
   s" a generic bundle input over scalars is a type fault" T-LABEL
   SRC-RESET  s" : SN-TGL ( snz<a,b> -- ) SNZ:UNMAKE drop drop ;" S+  SRC$ TEST-EVAL:RC 0 T=
   SRC-RESET  s" SN-GM2 ( n -- ) 7 8 9 SN-TGL drop" S+
   S\" \"code\":\"E-MISMATCH\",\"repair_class\":\"fix_type\"" S\" \"verdict\":\"rejected\""
   S\" \"suggestion\":\"Change the body so produced types match the signature.\"" VERDICT-REFUSED PACKET-HAS
   s" an if arm that turns a cell into a group is a depth fault counted in cells" T-LABEL
   SRC-RESET  s" SN-IFZ ( n bool -- n ) IF dup SN-SNZ THEN" S+
   S\" \"code\":\"E-MISMATCH\",\"repair_class\":\"remove_producer\"" S\" \"verdict\":\"rejected\""
   S\" \"reason\":\"the if arm changes the stack's depth\"" VERDICT-REFUSED PACKET-HAS
   s" the tokens after a refusal check in time linear in their count" T-LABEL
   \ Eight times the tokens takes about 8 times the CPU time when each token's
   \ cost is bounded, and up to 64 when it grows with the tokens before it.
   1000 STRAY-NS {: small:n :}   8000 STRAY-NS {: big:n :}
   big  small 24 *  < TTRUE
   s" a RECURSE with no declared effect is uncheckable for want of one, and says to declare one" T-LABEL
   RECURSE-NOSIG
   S\" habu: in sn-down: at 'RECURSE' recursion needs a declared stack effect; suggestion: Declare the definition's stack effect after its name.\n" REFUSES
   s" a RECURSE with no declared effect writes an uncheckable packet that says why" T-LABEL
   SRC-RESET RECURSE-TEXT
   S\" \"code\":\"E-UNCHECKABLE\"" S\" \"verdict\":\"uncheckable\""
   S\" \"reason\":\"recursion needs a declared stack effect\"" VERDICT-UNCHECKABLE PACKET-HAS
   s" a RECURSE with no declared effect suggests declaring one" T-LABEL
   S\" \"code\":\"E-UNCHECKABLE\"" S\" \"verdict\":\"uncheckable\""
   S\" \"suggestion\":\"Declare the definition's stack effect after its name.\"" VERDICT-UNCHECKABLE PACKET-HAS
   s" a RECURSE with no declared effect writes null for its declared effect" T-LABEL
   S\" \"code\":\"E-UNCHECKABLE\"" S\" \"verdict\":\"uncheckable\""
   S\" \"declared_effect\":null,\"declared_effect_source\":null,"
   VERDICT-UNCHECKABLE PACKET-HAS
   PACKET-PASSES
   s" a RECURSE in a does> clause is uncheckable for calling the defining word, and says where to recurse" T-LABEL
   RECURSE-DOES
   S\" habu: in sn-mk;does: at 'RECURSE' recurse in a does> clause would call the defining word; suggestion: Move the recursion into a named word with a declared effect and call it from the clause.\n" REFUSES
   s" a RECURSE in a does> clause writes an uncheckable packet that says why" T-LABEL
   RECURSE-DOES
   S\" \"code\":\"E-UNCHECKABLE\"" S\" \"verdict\":\"uncheckable\""
   S\" \"reason\":\"recurse in a does> clause would call the defining word\"" CLAUSE-PACKET-HAS
   s" a RECURSE in a does> clause suggests a named word to recurse in" T-LABEL
   RECURSE-DOES
   S\" \"code\":\"E-UNCHECKABLE\"" S\" \"verdict\":\"uncheckable\""
   S\" \"suggestion\":\"Move the recursion into a named word with a declared effect and call it from the clause.\"" CLAUSE-PACKET-HAS
   s" an effect with line breaks after (, before -- and before ) and a tab between its inputs loads" T-LABEL
   ADD-BLANKS  SRC$ TEST-EVAL:RC 0 T=   s" 1 2 SN-ADD" TEST-EVAL:N 3 T=
   s" a line break after a comma inside a family loads" T-LABEL
   RESULT-LINE  SRC$ TEST-EVAL:RC 0 T=
   s" an unknown type that ends a line refuses by its name alone" T-LABEL
   UNKNOWN-LINE
   s" at 'sn-nosuch'" REFUSES
   s" a type mismatch prints its suggestion after its actual row" T-LABEL
   SRC-RESET  s" : SN-MIS ( -- bool ) 1 ;" S+
   S\" habu: in sn-mis: at '1' expected: bool actual: n ; suggestion: Change the body so produced types match the signature.\n" REFUSES
   s" a bare ptr in a signature names its missing element type once, in its suggestion" T-LABEL
   SRC-RESET  s" : SN-BP ( ptr -- ) drop ;" S+
   S\" habu: in sn-bp: 'ptr' needs an element type; suggestion: Give 'ptr' an element type, e.g. 'ptr u8' or 'ptr a'.\n" REFUSES
   s" a trust-boundary primitive in a checked body names its boundary once, in its suggestion" T-LABEL
   SRC-RESET  s" : SN-CAP ( -- ) reloc-maps-clear ;" S+
   S\" E-CAP-TRUSTED habu: in sn-cap: 'reloc-maps-clear' is a trust-boundary primitive; suggestion: Move this compiler or runtime boundary behind audited TRUST.\n" REFUSES ;

\ Run while a package holding a rendering statement's mark is open, so MADE is
\ a name the run may define (UNSEEN-COVERS?) to these texts and to no other.
: UNSEEN-CASES ( -- )
   s" an is target the run may define defers its definition to the run" T-LABEL
   SRC-RESET  s" SN-IS-UNSEEN ( -- ) ['] T-RESET is MADE-D" S+
   SRC$ CHECK-CANDIDATE! VERDICT-DEFERRED T=
   s" a covered name alone in an unfit definition is uncheckable for its width" T-LABEL
   s" SN-WIDE-UNSEEN" s" MADE" WIDE-TEXT
   S\" \"code\":\"E-UNCHECKABLE\"" S\" \"verdict\":\"uncheckable\""
   S\" \"reason\":\"input row too wide to record (256 cells, at most 255)\"" VERDICT-UNCHECKABLE PACKET-HAS
   s" a covered name after a pinned reason in an unfit definition keeps the reason" T-LABEL
   s" SN-LOOPLESS" s" i drop MADE" WIDE-TEXT
   S\" \"code\":\"E-REJECTED\"" S\" \"verdict\":\"rejected\""
   S\" \"reason\":\"no loop is open here\"" VERDICT-REFUSED PACKET-HAS
   s" a structure left open after a covered name in an unfit definition keeps its reason" T-LABEL
   s" SN-OPEN-UNSEEN" s" MADE" WIDE-TEXT  s"  0 0= IF" S+
   S\" \"code\":\"E-REJECTED\"" S\" \"verdict\":\"rejected\""
   S\" \"reason\":\"a control structure is still open at the end of the definition\"" VERDICT-REFUSED PACKET-HAS
   s" a covered name after a pinned mismatch keeps the mismatch" T-LABEL
   SRC-RESET  s" SN-MISMATCH ( -- ) 1 IF THEN MADE" S+
   S\" \"code\":\"E-MISMATCH\"" S\" \"verdict\":\"rejected\""
   S\" \"expected\":\"bool \",\"actual\":\"n \"" VERDICT-REFUSED PACKET-HAS ;

\ Run from a package no mark covers, so a bare name nothing defines is
\ undefined while SN-MARKED:MADE is a name the run may define (UNSEEN-COVERS?).
: COVERED-CASES ( -- )
   s" an undefined word after a covered name in an unfit definition is refused" T-LABEL
   s" SN-WIDE-UNDEF" s" SN-MARKED:MADE SN-NO-SUCH-WORD" WIDE-TEXT
   S\" \"code\":\"E-UNDEFINED\"" S\" \"verdict\":\"rejected\""
   S\" \"token\":\"SN-NO-SUCH-WORD\"" VERDICT-REFUSED PACKET-HAS ;

: REPORT ( -- )
   T-REPORT
   s" signature-nesting: ok" type cr ;
;package

SIGNATURE-NESTING:RUN

\ The mark covers this package alone (UNSEEN-COVERS?).
package UNSEEN-NAMES
CHECKER-UNSEEN-MARK
SIGNATURE-NESTING:UNSEEN-CASES
;package

\ A mark in a package's public words covers that package's qualified names.
package SN-MARKED
public
CHECKER-UNSEEN-MARK
;package

package SN-UNMARKED
SIGNATURE-NESTING:COVERED-CASES
;package

SIGNATURE-NESTING:REPORT
