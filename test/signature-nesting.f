\ signature-nesting.f - a signature nested past the parser's limits refuses by
\ name through the real load path: a 33rd nested binder (checker.f
\ BINDER-DEPTH-MAX), and pointees or quotations nested past EFFECT-DEPTH-MAX.
\ A body nested past CF-FRAMES-MAX control frames, a RECURSE with no declared
\ effect and a RECURSE in a does> clause are uncheckable for a reason they name
\ too, and each says so in its JSON packet with a suggestion of its own; the
\ packet of a RECURSE with no declared effect has a null declared_effect, which
\ the diagnostics gate accepts.
\ A refusal ends the load, so each refused text runs in a child; the deepest
\ are stored rows (`trust`). A packet is read in this process. An effect split
\ by line breaks and tabs loads, and an unknown type ending a line refuses by name.
require src/lib/test.f
require src/lib/string.f
require src/lib/adt/result.f
require src/lib/test/subject.f
require src/lib/test/ending.f
require tools/gate-json-assert-core.f

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


: PACKET-HAS ( ptr u8 n ptr u8 n ptr u8 n -- )
   PACKET-OPEN  SRC$ CHECK-CANDIDATE! drop  PACKET-HOLDS ;


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
   S\" \"reason\":\"control flow nested too deep to check (depth 33, at most 32)\"" PACKET-HAS
   s" 33 nested IFs suggest a named word for the inner structures" T-LABEL
   SRC-RESET s" SN-IFS-33" 33 IFS-TEXT
   S\" \"code\":\"E-UNCHECKABLE\"" S\" \"verdict\":\"uncheckable\""
   S\" \"suggestion\":\"Move the inner control structures into a named word and call it there; each definition counts its own control frames.\"" PACKET-HAS
   s" a RECURSE with no declared effect is uncheckable for want of one" T-LABEL
   RECURSE-NOSIG
   s" at 'RECURSE' recursion needs a declared stack effect" REFUSES
   s" a RECURSE with no declared effect writes an uncheckable packet that says why" T-LABEL
   SRC-RESET RECURSE-TEXT
   S\" \"code\":\"E-UNCHECKABLE\"" S\" \"verdict\":\"uncheckable\""
   S\" \"reason\":\"recursion needs a declared stack effect\"" PACKET-HAS
   s" a RECURSE with no declared effect suggests declaring one" T-LABEL
   S\" \"code\":\"E-UNCHECKABLE\"" S\" \"verdict\":\"uncheckable\""
   S\" \"suggestion\":\"Declare the definition's stack effect after its name.\"" PACKET-HAS
   s" a RECURSE with no declared effect writes null for its declared effect" T-LABEL
   S\" \"code\":\"E-UNCHECKABLE\"" S\" \"verdict\":\"uncheckable\""
   S\" \"declared_effect\":null,\"declared_effect_source\":null," PACKET-HAS
   PACKET-PASSES
   s" a RECURSE in a does> clause is uncheckable for calling the defining word" T-LABEL
   RECURSE-DOES
   S\" at 'RECURSE' recurse in a does> clause would call the defining word\n" REFUSES
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
   T-REPORT
   s" signature-nesting: ok" type cr ;
;package

SIGNATURE-NESTING:RUN
