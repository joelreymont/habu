\ proof-manifest.f - the committed manifest a Rocq parity gate is checked against.
\
\ The module lives in `package PROOF-MANIFEST`. Two gates use it - the compiler
\ identity gate `test/compiler/ir-id-proof.f` and the interning gate
\ `test/compiler/ir-intern-proof.f` - and they must read one grammar, because a
\ second copy of it would be a second thing to keep true.
\
\ The manifest is a text file. A row is one of:
\
\   - `theorem <fully qualified name>`, opening a statement;
\   - `type <the statement that theorem makes>`, which must be the very next
\     non-comment row. The gate hands this text to Rocq as the ascribed type of a
\     definition whose body is the theorem, so Rocq's own type checker decides
\     whether the proof file still states this. Nothing ever reads a statement
\     out of a proof file, so editing a proof file cannot move its own target;
\   - an assumption row: `closed` is Rocq reporting "Closed under the global
\     context", and any other row is one external assumption written exactly as
\     Rocq prints it;
\   - a `#` comment or a blank line.
\
\ The shape rules are refusals, not conventions. A `type` row with no theorem
\ waiting above it, a second `type` row for one theorem, a `type` row written
\ after the theorem's assumption rows, a second theorem row before the first is
\ pinned, and a theorem left unpinned at the end of the file each fail when the
\ manifest is READ, before anything is asked of Rocq.
\
\ Assumption sets come in two strengths, and which one a gate wants is part of
\ its contract rather than an accident of what its proofs happen to need:
\
\   - allowed. The manifest may carry assumption rows other than `closed`, and
\     the gate's job is to hold the observed set equal to the committed one. The
\     identity proofs rest on the host compare-and-swap boundary, so that gate
\     runs this way and names exactly two assumptions.
\   - refused. Every statement must report "Closed under the global context", and
\     an assumption row cannot even be WRITTEN into the manifest: the read fails.
\     The interning proofs rest on nothing, and that gate runs this way, so the
\     empty assumption set is enforced twice - once when the manifest is read and
\     once when Rocq's answer is compared against it.
\
\ The module also normalizes what Rocq answers. `RENDER` turns the output of a
\ run of `Print Assumptions` commands into the same row vocabulary the manifest
\ is written in, so the two can be compared as whole texts rather than as a
\ subset, and it keeps the distinct assumption names it saw so a gate can report
\ them.

require lib/prelude.f
require lib/errors.f
require lib/string.f
require lib/fs.f

package PROOF-MANIFEST
private

$8000 constant AX-CAP
$4000 constant TY-CAP
128 constant TH-MAX
8 constant AXIOM-MAX
$0A constant LF
$23 constant HASH

create AX-RAW AX-CAP allot
create AX-WANT AX-CAP allot
create AX-GOT AX-CAP allot
create TY-RAW TY-CAP allot
create TH-OFF TH-MAX cells allot
create TH-LEN TH-MAX cells allot
create TYPE-OFF TH-MAX cells allot
create TYPE-LEN TH-MAX cells allot
create AXIOM-OFF AXIOM-MAX cells allot
create AXIOM-LEN AXIOM-MAX cells allot

variable AX-RAW-U
variable AX-WANT-U
variable AX-GOT-U
variable TY-U
variable TH-N
variable TYPE-N
variable PEND
variable AXIOM-N
variable CLOSED-N
variable BEARING-N
variable CUR
variable ALLOW-AX

\ ---- byte buffers ------------------------------------------------------------

: SINK+ ( ptr u8 n ptr u8 n ptr a -- )
   {: src:ptr srcu:n dst:ptr cap:n lenv:ptr :}
   lenv @ srcu + cap > if E-STR-CAPACITY throw then
   src dst lenv @ + srcu BYTE-COPY
   lenv @ srcu + lenv ! ;

: SINK-C+ ( n ptr u8 n ptr a -- ) {: c:n dst:ptr cap:n lenv:ptr :}
   lenv @ 1+ cap > if E-STR-CAPACITY throw then
   c dst lenv @ + c!
   lenv @ 1+ lenv ! ;

: WANT+ ( ptr u8 n -- )
   AX-WANT AX-CAP AX-WANT-U SINK+ ;

: WANT-NL ( -- )
   LF AX-WANT AX-CAP AX-WANT-U SINK-C+ ;

: GOT+ ( ptr u8 n -- )
   AX-GOT AX-CAP AX-GOT-U SINK+ ;

: GOT-NL ( -- )
   LF AX-GOT AX-CAP AX-GOT-U SINK-C+ ;

: TY+ ( ptr u8 n -- )
   TY-RAW TY-CAP TY-U SINK+ ;

\ ---- lines -------------------------------------------------------------------

: LINE-END ( ptr u8 n n -- n ) {: a:ptr u:n off:n :}
   off
   begin dup u < while
      dup a + c@ LF = if exit then
      1+
   repeat ;

: TRIM-LEAD ( ptr u8 n -- n ) {: a:ptr u:n :}
   0
   begin dup u < while
      dup a + c@ $20 <> if exit then
      1+
   repeat ;

: TRIM-TAIL ( ptr u8 n n -- n ) {: a:ptr u:n start:n :}
   u
   begin dup start > while
      dup 1- a + c@ $20 <> if exit then
      1-
   repeat ;

: TRIMMED$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   a u TRIM-LEAD {: start:n :}
   a start +
   a u start TRIM-TAIL start - ;

\ ---- the manifest grammar ----------------------------------------------------

: COMMENT-LINE? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   u 0= if true exit then
   a c@ HASH = ;

\ The two row tags, and the widths that skip them, from one text each, so a row
\ cannot be recognised by one spelling and stripped by another.
: THEOREM-TAG$ ( -- ptr u8 n )
   s" theorem " ;

: TYPE-TAG$ ( -- ptr u8 n )
   s" type " ;

: CLOSED-ROW$ ( -- ptr u8 n )
   s" closed" ;

: THEOREM-TAG ( -- n )
   THEOREM-TAG$ nip ;

: TYPE-TAG ( -- n )
   TYPE-TAG$ nip ;

: THEOREM-LINE? ( ptr u8 n -- bool )
   THEOREM-TAG$ STARTS-WITH? ;

: TYPE-LINE? ( ptr u8 n -- bool )
   TYPE-TAG$ STARTS-WITH? ;

: THEOREM+ ( n n -- ) {: off:n u:n :}
   TH-N @ TH-MAX >= if E-CID-AXIOM throw then
   off TH-N @ cells TH-OFF + !
   u TH-N @ cells TH-LEN + !
   TH-N @ 1+ TH-N ! ;

\ The statement the manifest says the pending theorem makes. `PEND` carries the
\ index of the theorem still waiting for its statement, and clearing it is what
\ makes a statement belong to the theorem row directly above it: a `type` row
\ with no theorem waiting, which covers both an orphan row and a second row for
\ one theorem, is refused here.
: TYPE+ ( ptr u8 n -- ) {: a:ptr u:n :}
   PEND @ 0 < if E-CID-AXIOM throw then
   TY-U @ PEND @ cells TYPE-OFF + !
   u PEND @ cells TYPE-LEN + !
   a u TY+
   TYPE-N @ 1+ TYPE-N !
   -1 PEND ! ;

\ A manifest row that names a theorem also records where its name sits in the
\ stripped body, so the generated Rocq file asks for exactly these names in
\ exactly this order, and opens the requirement that the next row pin what that
\ theorem states.
: MANIFEST-THEOREM ( ptr u8 n -- ) {: a:ptr u:n :}
   PEND @ 0 >= if E-CID-AXIOM throw then
   a u WANT+ WANT-NL
   AX-WANT-U @ u 1+ - THEOREM-TAG + u THEOREM-TAG - THEOREM+
   TH-N @ 1- PEND ! ;

\ Every other row is an assumption row, compared byte for byte against what Rocq
\ reported. One may not stand between a theorem row and its type row, so a
\ theorem whose statement is left unpinned fails the read rather than the
\ comparison. When the gate refuses assumptions the only row that may appear is
\ `closed`, so an assumption cannot be introduced by editing this file either.
: MANIFEST-ASSUMPTION ( ptr u8 n -- ) {: a:ptr u:n :}
   PEND @ 0 >= if E-CID-AXIOM throw then
   ALLOW-AX @ 0= if
      a u CLOSED-ROW$ STR= 0= if E-CID-AXIOM throw then
   then
   a u WANT+ WANT-NL ;

: MANIFEST-LINE ( ptr u8 n -- ) {: a:ptr u:n :}
   a u COMMENT-LINE? if exit then
   a u TYPE-LINE? if a TYPE-TAG + u TYPE-TAG - TYPE+ exit then
   a u THEOREM-LINE? if a u MANIFEST-THEOREM exit then
   a u MANIFEST-ASSUMPTION ;

: MANIFEST-RESET ( -- )
   0 AX-WANT-U !
   0 TY-U !
   0 TH-N !
   0 TYPE-N !
   -1 PEND ! ;

public

\ Whether an assumption row other than `closed` may appear at all. A gate states
\ this before it reads, so the strength of its assumption claim is written in the
\ gate rather than inferred from the manifest it happens to have.
: AXIOMS-ALLOWED! ( bool -- ) {: allowed:bool :}
   allowed if 1 else 0 then ALLOW-AX ! ;

\ The manifest grammar over bytes rather than over the committed file, so the
\ shape rules can be shown to refuse a malformed manifest.
: TEXT ( ptr u8 n -- ) {: a:ptr u:n :}
   MANIFEST-RESET
   0 CUR !
   begin CUR @ u < while
      a u CUR @ LINE-END {: stop:n :}
      a CUR @ + stop CUR @ - MANIFEST-LINE
      stop 1+ CUR !
   repeat
   PEND @ 0 >= if E-CID-AXIOM throw then ;

: READ ( ptr u8 n -- )
   2dup FILE-SIZE {: a:ptr u:n size:n :}
   size AX-CAP > if E-STR-CAPACITY throw then
   a u AX-RAW AX-CAP READ-ALL AX-RAW-U !
   AX-RAW AX-RAW-U @ TEXT ;

: THEOREMS ( -- n )
   TH-N @ ;

: TYPES ( -- n )
   TYPE-N @ ;

: THEOREM$ ( n -- ptr u8 n ) {: k:n :}
   k 0 < k TH-N @ >= or if E-CID-AXIOM throw then
   AX-WANT k cells TH-OFF + @ + k cells TH-LEN + @ ;

: TYPE$ ( n -- ptr u8 n ) {: k:n :}
   k 0 < k TYPE-N @ >= or if E-CID-AXIOM throw then
   TY-RAW k cells TYPE-OFF + @ + k cells TYPE-LEN + @ ;

\ The whole committed expectation, as one text.
: WANT$ ( -- ptr u8 n )
   AX-WANT AX-WANT-U @ ;

private

\ ---- rendering what Rocq answered --------------------------------------------

: AXIOM-DISTINCT? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   AXIOM-N @ 0 ?do
      AX-GOT i cells AXIOM-OFF + @ + i cells AXIOM-LEN + @ a u STR= if
         false unloop exit
      then
   loop
   true ;

: AXIOM-KEEP ( n n -- ) {: off:n u:n :}
   AX-GOT off + u AXIOM-DISTINCT? 0= if exit then
   AXIOM-N @ AXIOM-MAX >= if E-CID-AXIOM throw then
   off AXIOM-N @ cells AXIOM-OFF + !
   u AXIOM-N @ cells AXIOM-LEN + !
   AXIOM-N @ 1+ AXIOM-N ! ;

: RENDER-MARKER ( ptr u8 n -- ) {: a:ptr u:n :}
   s" theorem " GOT+
   a 11 + u 11 - GOT+
   GOT-NL ;

: RENDER-AXIOM ( ptr u8 n -- ) {: a:ptr u:n :}
   s" axiom " GOT+
   AX-GOT AX-GOT-U @ + {: at:ptr :}
   a u GOT+
   GOT-NL
   at AX-GOT - u AXIOM-KEEP ;

: RENDER-LINE ( ptr u8 n -- ) {: raw:ptr rawu:n :}
   raw rawu TRIMMED$ {: a:ptr u:n :}
   u 0= if exit then
   a u s" == theorem " STARTS-WITH? if a u RENDER-MARKER exit then
   a u s" Closed under the global context" STR= if
      CLOSED-ROW$ GOT+ GOT-NL
      CLOSED-N @ 1+ CLOSED-N !
      exit
   then
   a u s" Axioms:" STR= if
      BEARING-N @ 1+ BEARING-N !
      exit
   then
   a u RENDER-AXIOM ;

public

\ Turn one run of `Print Assumptions` output into manifest rows.
: RENDER ( ptr u8 n -- ) {: a:ptr u:n :}
   0 AX-GOT-U !
   0 AXIOM-N !
   0 CLOSED-N !
   0 BEARING-N !
   0 CUR !
   begin CUR @ u < while
      a u CUR @ LINE-END {: stop:n :}
      a CUR @ + stop CUR @ - RENDER-LINE
      stop 1+ CUR !
   repeat ;

: GOT$ ( -- ptr u8 n )
   AX-GOT AX-GOT-U @ ;

: CLOSED ( -- n )
   CLOSED-N @ ;

: BEARING ( -- n )
   BEARING-N @ ;

: AXIOM-COUNT ( -- n )
   AXIOM-N @ ;

: AXIOM$ ( n -- ptr u8 n ) {: k:n :}
   k 0 < k AXIOM-N @ >= or if E-CID-AXIOM throw then
   AX-GOT k cells AXIOM-OFF + @ + k cells AXIOM-LEN + @ ;

;package
