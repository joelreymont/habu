\ TWO QUOTATION BODIES OF ONE RECORD ARE ONE CLOSURE MEMBER. Under tier 1 a
\ quotation reference is an ADR and nothing marks a function boundary, so an
\ anonymous body runs to its record's end (src/habu/aot-closure.f BODY-END-SCAN)
\ and the LOWER of two bodies contains the higher one's code. Both are roots -
\ each is named by a declared xt cell - so the walk met both, and the closure
\ used to carry the same bytes twice: two members whose copies held the same
\ instructions, with the inner cell's row pointing into whichever copy the
\ discovery order put first. aot-closure.f DROP-NESTED-CLO keeps the
\ container only, and this file measures that on the closure itself.
\
\ THE FIXTURE IS test/compiler/aot-xt-cells-subject.f's SHAPE: one word binding
\ two quotations to two defers. That subject is linked end to end by the
\ aot-xt-cells suite, which says the image runs; this says which bytes the image
\ carries and what the cell's row resolves to, which an image that runs cannot.
require lib/test.f
require src/habu/app-image.f
require src/habu/aot-decl.f
require src/habu/aot-closure.f
require src/habu/aot-lib.f

\ Tier 1 is the shape the rule is about: at tier 2 a quotation reference IS an
\ address chain, so BODY-END-SCAN stops at the next body and the two are
\ disjoint already.
1 set-tier

package AOT-NESTED-BODY-FIXTURE
public
: BUMP ( n -- n ) 1+ ;
: TWICE ( n -- n ) 2 * ;
;package

\ THE CAPTURE WINDOW, LATCHED AROUND THE TWO CELLS AND NOTHING ELSE.
\ COLLECT-XT-CELLS collects the declared address-cell rows that fall inside
\ [BLOB-SRC, BLOB-END], the window tools/aot-build-core.f opens with
\ AOT-DATA-START and closes with AOT-DATA-SPAN around the application it links.
\ The two bounds are written directly here: the latch words also open a string
\ pool and reserve the carried-constant run, neither of which has anything to do
\ with the bodies, and DATA grows by the two cells alone between them.
package AOT-LINK
HERE-N BLOB-SRC !
;package

package AOT-NESTED-BODY-FIXTURE
public
defer QOUTER ( n -- n )
defer QINNER ( n -- n )
\ ONE EMISSION, TWO QUOTATIONS: BIND's own function, then the body QOUTER takes,
\ then the body QINNER takes (src/compiler/native/emit.f FUNCTION-OFFSET@), all
\ under one dictionary record. BIND is never a member itself - its `is` stores
\ declare an address cell, which a stripped image has no registrar for - so the
\ bodies arrive in the closure without the word that bound them.
: BIND ( -- ) [: BUMP BUMP ;] is QOUTER  [: TWICE ;] is QINNER ;
\ The entry reaches nothing, so the closure is exactly MAIN, the member the two
\ roots produce, and the two words that member calls.
: ROOT ( -- ) ;
;package
AOT-NESTED-BODY-FIXTURE:BIND

package AOT-LINK
HERE-N BLOB-END !

variable NEST-I  variable NEST-J  variable NEST-OV
: NEST-OVERLAP? ( n n -- bool ) {: a:n b:n :}
   a CLO-AT  b CLO-AT b CLO-BYTES +  <
   b CLO-AT  a CLO-AT a CLO-BYTES +  <  and ;
: NEST-OVERLAPS ( -- n )                  \ member pairs that share a byte
   0 NEST-OV !
   0 NEST-I ! BEGIN NEST-I @ NCLO @ < WHILE
      NEST-I @ 1+ NEST-J ! BEGIN NEST-J @ NCLO @ < WHILE
         NEST-I @ NEST-J @ NEST-OVERLAP? IF NEST-OV @ 1+ NEST-OV ! THEN
         NEST-J @ 1+ NEST-J ! REPEAT
      NEST-I @ 1+ NEST-I ! REPEAT
   NEST-OV @ ;

: NEST-RUN ( -- )
   T-RESET
   s" the fixture's two defer cells are the window's declared xt cells" T-LABEL
   COLLECT-XT-CELLS
   XTC-N @ 2 T=
   0 XTC-VAL@ {: v1:n :}
   1 XTC-VAL@ {: v2:n :}
   \ The rows are ascending by DATA offset (XTC-SORT), which is declaration
   \ order, and QOUTER's body is emitted first: the root the walk meets first is
   \ the OUTER body's, which is the copy the old lowest-index lookup answered
   \ with for an address in either.
   v1 v2 < TTRUE
   v1 ADDRESS-OWNER {: rec:ptr :}
   rec XREF-FOUND? TTRUE
   v2 ADDRESS-OWNER rec = TTRUE
   s" AOT-NESTED-BODY-FIXTURE:ROOT" ENTRY-NAME! CLOSURE
   \ PLAN-BLOBS is where a link plans the members AND builds the entry order the
   \ member lookups read (aot-lib.f MEMBER-ORDER), so every question about a
   \ member comes after it: MEMBER-AT asked before it throws E-LAYOUT-BOUNDS off
   \ the unfilled order rather than answering.
   ASM-INIT PLAN-BLOBS
   s" the contained body is not a member of its own" T-LABEL
   v1 CODE-PTR MEMBER-AT {: m:n :}
   m -1 T<>
   v2 CODE-PTR MEMBER-AT -1 T=
   NCLO @ 4 T=                            \ ROOT, the body, and the BUMP and TWICE it calls
   s" the member runs to its record's end and holds both bodies" T-LABEL
   m CLO-REC@ rec = TTRUE
   m CLO-AT-N v1 T=
   m CLO-BYTES  rec REC-END-N v1 -  T=
   v2 v1 m CLO-BYTES + < TTRUE
   s" no two members share a byte" T-LABEL
   NEST-OVERLAPS 0 T=
   s" the contained body's cell resolves through the container" T-LABEL
   0 XT-CELL-TARGET  m MEMBER-NEWOFF  T=
   1 XT-CELL-TARGET  m MEMBER-NEWOFF v2 v1 - +  T=
   T-REPORT ;

' NEST-RUN
;package
execute
