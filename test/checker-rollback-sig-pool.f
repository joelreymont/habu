\ checker-rollback-sig-pool.f - payload membership across checker rollback.
\
\ Arming records membership by symbol id. Freeze serializes each live member's
\ verified effect and replaces that marker with a row offset. A candidate scope
\ returns its symbol ids to the interner, so its membership must be retired too.
\ Otherwise a different word can inherit coverage it never acquired, defeating
\ the capture audit. The cases use the generated-declaration preflight's actual
\ scope entry points and intern different names after the pop to force id reuse.
\
\ Test membership and missing coverage while live; only frozen payload lookups
\ may be interpreted as serialized offsets. Counts, names and effect graphs
\ must describe the surviving words, with no rows for rolled-back candidates.

require lib/errors.f
require lib/string.f
require lib/test.f

package RBSIG-TEST
private

\ The capture reads these frozen spans. Rows contain four little-endian u32
\ fields: name, graph, package, visibility. Names are `[len u16][bytes]`;
\ effects are versioned graphs with blob-relative references.

$0 constant R.NAME
$4 constant R.GRAPH
16 constant ROW-BYTES

PTR-VARIABLE ROWS
variable ROW-U
PTR-VARIABLE STRINGS
variable STR-U

\ Whitebox payload-owner operations; the test deliberately controls the live
\ capture window and reads its private serialized spans.
TRUSTED: ARM ( -- ) CHECKER-PAYLOAD-ARM ;

TRUSTED: FREEZE ( -- )
   CHECKER-PAYLOAD-FREEZE
   CHECKER-PAYLOAD-SPANS STR-U ! STRINGS ! ROW-U ! ROWS ! ;

: ROW-COUNT ( -- n )
   ROW-U @ ROW-BYTES mod 0 T=
   ROW-U @ ROW-BYTES / ;

: ROW-BYTE@ ( n -- n ) {: at:n :}
   at 0 < at ROW-U @ >= or if 76 throw then
   ROWS @ at + c@ ;

: STR-BYTE@ ( n -- n ) {: at:n :}
   at 0 < at STR-U @ >= or if 76 throw then
   STRINGS @ at + c@ ;

: ROW-U32@ ( n -- n ) {: at:n :}
   at ROW-BYTE@
   at 1 + ROW-BYTE@ 8 lshift or
   at 2 + ROW-BYTE@ 16 lshift or
   at 3 + ROW-BYTE@ 24 lshift or ;

: STR-LEN ( n -- n ) {: at:n :}
   at STR-BYTE@  at 1 + STR-BYTE@ 8 lshift or ;

: STR-AT= ( n ptr u8 n -- bool ) {: at:n a:ptr u:n :}
   at STR-LEN u <> IF false EXIT THEN
   0 BEGIN dup u < WHILE
      dup at 2 + + STR-BYTE@  over a + c@ <> IF drop false EXIT THEN
      1 +
   REPEAT drop true ;

\ A known checked word with row zero is exactly the capture audit's refusal.
TRUSTED: LOOKUP ( ptr u8 n -- n bool ) {: a:ptr u:n :}
   s" " false a u CHECKER-PAYLOAD-LOOKUP ;

: ROW-NAMES? ( ptr u8 n ptr u8 n -- bool ) {: qa:ptr qu:n wa:ptr wu:n :}
   qa qu LOOKUP 0= if drop false exit then {: p:n :}
   p 0= IF false EXIT THEN
   p 1 - R.NAME + ROW-U32@  wa wu STR-AT= ;

: MISSING? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   s" " false a u CHECKER-ASIG-MISSING? ;

: MEMBER? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   s" " false a u CHECKER-ASIG-ROW-FOR 0 <> ;

\ Inspect the actual serialized graph, using its owner's field definitions.
\ This fixture's rows contain only n values and an implicit untouched tail.
TRUSTED: N-ROW? ( ptr u8 n n -- bool ) {: graph:ptr off:n count:n :}
   graph off + {: node:ptr :}
   count 0= if node EN.TAG @ EN-ROW = exit then
   node EN.TAG @ EN-PUSH <> node EN.C @ 2 <> or if false exit then
   graph node EN.A @ + {: kind:ptr :}
   kind EN.TAG @ EN-CON <> if false exit then
   graph kind EN.A @ + kind EN.B @ s" n" CORE-STR= 0= if false exit then
   graph node EN.B @ count 1- RECURSE ;

TRUSTED: GRAPH-EFFECT? ( ptr u8 n n -- bool ) {: graph:ptr in:n out:n :}
   graph EW.ACTIVE @ ASIG-GRAPH-MAGIC <> if false exit then
   graph EW.MINI @ in <> if false exit then
   graph EW.HASR @ 0<> if false exit then
   graph graph EW.DIN @ in N-ROW?
   graph graph EW.DOUT @ out N-ROW? and ;

: ROW-EFFECT? ( ptr u8 n n n -- bool ) {: a:ptr u:n in:n out:n :}
   a u LOOKUP 0= if drop false exit then {: row:n :}
   row 0= if false exit then
   row 1- R.GRAPH + ROW-U32@ STRINGS @ + in out GRAPH-EFFECT? ;

\ Certify one definition through the same entry point the generated-declaration
\ preflight uses, and refuse to build a case on a body the checker rejected.
: CERT ( ptr u8 n -- ) {: a:ptr u:n :}
   a u T-LABEL
   a u CHECK! -1 T= ;

\ ---- case one: the popped scope's rows may not answer for anybody ------------

: STALE-ROW-CASE ( -- )
   ARM
   CHECKER-CANDIDATE-SCOPE-START
   s" RBSIG-GHOST-A ( -- )" CERT
   s" RBSIG-GHOST-B ( -- )" CERT
   CHECKER-CANDIDATE-SCOPE-DONE

   s" rolled-back names have neither membership nor a known missing effect" T-LABEL
   s" RBSIG-GHOST-A" MEMBER? TFALSE
   s" RBSIG-GHOST-B" MEMBER? TFALSE
   s" RBSIG-GHOST-A" MISSING? TFALSE
   s" RBSIG-GHOST-B" MISSING? TFALSE

   \ Different names take the ids returned by the popped scope.
   s" RBSIG-SHIM ( -- )" CERT
   s" RBSIG-REAL-A ( -- )" CERT

   s" the new live definitions have their own capture membership" T-LABEL
   s" RBSIG-SHIM" MEMBER? TTRUE
   s" RBSIG-REAL-A" MEMBER? TTRUE
   s" RBSIG-SHIM" MISSING? TFALSE
   s" RBSIG-REAL-A" MISSING? TFALSE

   FREEZE
   s" freeze serializes exactly the two live members" T-LABEL
   ROW-COUNT 2 T=
   s" RBSIG-GHOST-A" LOOKUP TFALSE 0 T=
   s" RBSIG-GHOST-B" LOOKUP TFALSE 0 T=
   s" the row a capture would take for RBSIG-SHIM names RBSIG-SHIM" T-LABEL
   s" RBSIG-SHIM"   s" rbsig-shim"   ROW-NAMES? TTRUE
   s" ... and RBSIG-REAL-A's names RBSIG-REAL-A" T-LABEL
   s" RBSIG-REAL-A" s" rbsig-real-a" ROW-NAMES? TTRUE
   s" RBSIG-SHIM does not answer ghost A's row, whose id it took" T-LABEL
   s" RBSIG-SHIM"   s" rbsig-ghost-a" ROW-NAMES? TFALSE
   s" ... and RBSIG-REAL-A does not answer ghost B's" T-LABEL
   s" RBSIG-REAL-A" s" rbsig-ghost-b" ROW-NAMES? TFALSE
   s" both rows carry their verified empty effects" T-LABEL
   s" RBSIG-SHIM" 0 0 ROW-EFFECT? TTRUE
   s" RBSIG-REAL-A" 0 0 ROW-EFFECT? TTRUE ;

\ ---- case two: the audit that has to fail loudly still can -------------------
\ A word the checker knows and the pool has no row for is the ONE condition a
\ capture must refuse. Outside the arming window every definition is that word,
\ so a stale entry there does not hand over a wrong row - it hands over a false
\ "this one is covered", which is the silent half of the same defect.

: AUDIT-CASE ( -- )
   CHECKER-ASIG-DISARM
   s" RBSIG-BEFORE ( -- )" CERT
   ARM
   s" a known word outside the live window lacks membership" T-LABEL
   s" RBSIG-BEFORE" MEMBER? TFALSE
   s" RBSIG-BEFORE" MISSING? TTRUE
   CHECKER-CANDIDATE-SCOPE-START
   s" RBSIG-GHOST-C ( -- )" CERT
   s" RBSIG-GHOST-D ( -- )" CERT
   CHECKER-CANDIDATE-SCOPE-DONE
   FREEZE

   s" RBSIG-OUT-SHIM ( -- )" CERT
   s" RBSIG-OUT-A ( -- )" CERT

   s" freeze discarded the candidates and collected no later definitions" T-LABEL
   ROW-COUNT 0 T=
   s" the audit calls RBSIG-OUT-SHIM missing, which it is" T-LABEL
   s" RBSIG-OUT-SHIM" MISSING? TTRUE
   s" ... and RBSIG-OUT-A too" T-LABEL
   s" RBSIG-OUT-A" MISSING? TTRUE
   s" the capture lookup reports known words with missing rows" T-LABEL
   s" RBSIG-BEFORE" LOOKUP TTRUE 0 T=
   s" RBSIG-OUT-SHIM" LOOKUP TTRUE 0 T=
   s" RBSIG-OUT-A" LOOKUP TTRUE 0 T= ;

\ ---- case three: the retire may not reach below the frame --------------------
\ The mark is the frame's restored SYM-N. Membership recorded before the scope
\ belongs to a surviving symbol; retiring from zero would lose that coverage.

: KEEP-CASE ( -- )
   ARM
   s" RBSIG-KEEP ( -- n ) 0" CERT
   s" the live word is covered before any scope opens" T-LABEL
   s" RBSIG-KEEP" MEMBER? TTRUE
   s" RBSIG-KEEP" MISSING? TFALSE

   CHECKER-CANDIDATE-SCOPE-START
   s" RBSIG-GHOST-E ( -- )" CERT
   CHECKER-CANDIDATE-SCOPE-DONE

   s" the pop preserves membership below its restored mark" T-LABEL
   s" RBSIG-KEEP" MEMBER? TTRUE
   s" RBSIG-KEEP" MISSING? TFALSE
   s" RBSIG-GHOST-E" MEMBER? TFALSE
   FREEZE
   s" only the retained word's name and output effect are serialized" T-LABEL
   ROW-COUNT 1 T=
   s" RBSIG-KEEP" s" rbsig-keep" ROW-NAMES? TTRUE
   s" RBSIG-KEEP" 0 1 ROW-EFFECT? TTRUE
   s" RBSIG-KEEP" 0 0 ROW-EFFECT? TFALSE
   s" RBSIG-GHOST-E" LOOKUP TFALSE 0 T= ;

public

: RUN ( -- )
   STALE-ROW-CASE
   AUDIT-CASE
   KEEP-CASE
   CHECKER-ASIG-DISARM          \ leave the process as this file found it
   T-REPORT
   s" checker-rollback-sig-pool: ok" type cr ;

;package

RBSIG-TEST:RUN
