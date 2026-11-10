\ signature-nesting.f - a signature nested past the parser's limits refuses by
\ name through the real load path: a 33rd nested binder (checker.f
\ BINDER-DEPTH-MAX), and pointees or quotations nested past EFFECT-DEPTH-MAX.
\ A body nested past CF-FRAMES-MAX control frames, and a RECURSE with no
\ declared effect, are uncheckable for a reason they name too, and each says
\ so in its JSON packet.
\ A refusal ends the load, so each refused text runs in a child; the deepest
\ are stored rows (`trust`). A packet is read in this process.
require src/lib/test.f
require src/lib/string.f
require src/lib/test/subject.f
require src/lib/test/ending.f

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


\ The text in SRC-BUF, run as the subject, exits 70 and names why on stderr.
: REFUSES ( ptr u8 n -- )
   {: why:ptr whyu:n :}
   SRC$ OUT CAP >LEN ERR CAP >LEN CHILD-MS >MS SUBJECT:RUN
   {: outu:len erru:len oc :}
   SRC$ OUT outu LEN>N ERR erru LEN>N oc 70 T-ENDING-EXITED=
   ERR erru LEN>N why whyu CONTAINS? TTRUE ;


\ The text in SRC-BUF, checked in this process with JSON diagnostics captured,
\ writes an E-UNCHECKABLE packet, verdict uncheckable, whose reason is WHY.
create PKT-BUF 8192 allot
: PACKET-HAS ( ptr u8 n -- )
   {: why:ptr whyu:n :}
   PKT-BUF 8192 DIAG-BUFFER!  0 0= DIAG-JSON!
   SRC$ CHECK-CANDIDATE! drop
   DIAG-BUFFER$ S\" \"code\":\"E-UNCHECKABLE\"" CONTAINS? TTRUE
   DIAG-BUFFER$ S\" \"verdict\":\"uncheckable\"" CONTAINS? TTRUE
   DIAG-BUFFER$ why whyu CONTAINS? TTRUE
   0 0= 0= DIAG-JSON! DIAG-BUFFER-OFF ;

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
   S\" \"reason\":\"control flow nested too deep to check (depth 33, at most 32)\"" PACKET-HAS
   s" a RECURSE with no declared effect is uncheckable for want of one" T-LABEL
   RECURSE-NOSIG
   s" at 'RECURSE' recursion needs a declared stack effect" REFUSES
   s" a RECURSE with no declared effect writes an uncheckable packet that says why" T-LABEL
   SRC-RESET RECURSE-TEXT
   S\" \"reason\":\"recursion needs a declared stack effect\"" PACKET-HAS
   T-REPORT
   s" signature-nesting: ok" type cr ;
;package

SIGNATURE-NESTING:RUN
