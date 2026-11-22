\ type-decl-suite.f — behavior suite for the NEWTYPE/SUMTYPE declaration grammar
\ (src/core/sumtype.f).

require src/core/exit-status.f
require src/lib/string.f
require src/lib/test.f
require test/decl-diag-capture.f
require src/lib/engine-candidate.f
require test/e2e-tree.f

using TYPE-DECL

T-RESET

\ Evaluate a declaration string through evaluate-closed, returning its throw
\ code (0 = accepted). The catch is compiled: a top-level catch around evaluate
\ loses the code.
TYPED-VARIABLE TDTE-A ^u8   variable TDTE-U
: TDT-EVAL-GO ( -- )
   TDTE-A @ TDTE-U @ evaluate-closed ;
: TDT-EVAL-CATCH ( ^u8 n -- n )
   TDTE-U ! TDTE-A !
   [: TDT-EVAL-GO ;] catch ;

\ Checked aliases of the checker's multi-error bracket.
: TWX-MULTI-ERR-BEGIN ( -- ) CHECK:MULTI-ERR-BEGIN ;
: TWX-MULTI-ERR-END ( -- n ) CHECK:MULTI-ERR-END ;

: TDT-NEG ( ^u8 n n -- ) {: a:^ u:n code:n :}
   a u TDT-EVAL-CATCH code T= ;

\ silence expected declaration diagnostics (asserted explicitly further down).
REJECTION:CAPTURE
\ The probes read prose, and a run under `tools/check.f --json-errors` starts in
\ JSON; each JSON stretch ends in prose, and the format found returns before T-REPORT.
variable TD-JSON-WAS   REJECTION:JSON @ TD-JSON-WAS !   false REJECTION:JSON !
\ TDT-REJ: SRC stops its load with status 70, and its record holds the HEAD
\ and LABEL lines. TDT-AT: SRC stops with status 70, its record holds the
\ HEAD line, and its evaluated note marks TOKEN under LABEL. TDT-HAS: the
\ record holds a line.
: TDT-REJ ( ^u8 n str str -- )
   {: sa:^ su:n h l :}
   REJECTION:CAPTURE
   sa su TDT-EVAL-CATCH 70 T=
   REJECTION:BUFFER$ h CONTAINS? TTRUE
   REJECTION:BUFFER$ l CONTAINS? TTRUE ;
: TDT-AT ( ^u8 n str str str -- )
   {: sa:^ su:n h t l :}
   REJECTION:CAPTURE
   sa su TDT-EVAL-CATCH 70 T=
   REJECTION:BUFFER$ h CONTAINS? TTRUE
   t l DECL-DIAG:CAUSE? TTRUE ;
: TDT-HAS ( str -- ) {: s :}
   REJECTION:BUFFER$ s CONTAINS? TTRUE ;

\ NEWTYPE: registers a TK-CELL family in the global scope, usable in sigs.
NEWTYPE tdfoo<a,b>
s" TDOK-USE ( tdfoo<n,n> -- tdfoo<n,n> )" >str CHECK-QUIET-CANDIDATE! -1 T=
s" TDBAD-ARITY ( tdfoo<n> -- ) drop" >str CHECK-QUIET-CANDIDATE! 0 T=
s" TDBAD-UPPER ( Tdfoo<n,n> -- ) drop" >str CHECK-QUIET-CANDIDATE! 0 T=

\ zero-arity family: the bare token resolves through TFAM, not nominal lookup.
NEWTYPE tdzero
s" TDOK-ZERO ( tdzero -- tdzero )" >str CHECK-QUIET-CANDIDATE! -1 T=
s" TDBAD-ZERO-N ( tdzero -- n )" >str CHECK-QUIET-CANDIDATE! 0 T=

\ A family takes no `SCRATCH-ONLY` clause: the word after its base is undefined.
s" NEWTYPE tdscrx ^n SCRATCH-ONLY" s\" error[E-UNDEFINED]: cannot find word `SCRATCH-ONLY`\n" >str s" not found" >str TDT-REJ

\ TYPEFAMILY is not a word: in a line comment, paren comment or string literal
\ it throws nothing (TDT-NEG).
s" \ TYPEFAMILY tdtombline 0" 0 TDT-NEG

s" ( TYPEFAMILY tdtombparen 0 )" 0 TDT-NEG

s\" : TDT-TOMB-TEXT ( -- ^u8 n ) s\" TYPEFAMILY tdtombstr 0\" ;" 0 TDT-NEG

\ The live spelling needs no separate control here: `tdfoo` and `tdzero` above
\ already pin what a NEWTYPE declaration registers.

\ SUMTYPE: registers a TK-SUM family + SUMV variants + payload schemas, and
\ closes the declaration by wiring the variant range and max payload slots.
SUMTYPE tdres<a,b>
  VARIANT ok  a ;VARIANT
  VARIANT err b ;VARIANT
;SUMTYPE
\ the sum family name is usable in signatures as a logical type expression.
s" TDOK-RES ( tdres<n,n> -- tdres<n,n> )" >str CHECK-QUIET-CANDIDATE! -1 T=
s" TDBAD-RES1 ( tdres<n> -- ) drop" >str CHECK-QUIET-CANDIDATE! 0 T=

\ zero-payload variant: padding-only, paycells 0 (the option `none` shape).
SUMTYPE tdopt<a>
  VARIANT none   ;VARIANT
  VARIANT some a ;VARIANT
;SUMTYPE

\ zero-arity sum (payload-free variants only): the enum-shaped sum.
SUMTYPE tdlight
  VARIANT red   ;VARIANT
  VARIANT green ;VARIANT
  VARIANT blue  ;VARIANT
;SUMTYPE

\ arity above four parses through growable schema storage.
SUMTYPE tdwide<a,b,c,d,e,g,h,i>
  VARIANT lo a ;VARIANT
  VARIANT hi i ;VARIANT
;SUMTYPE
s" TDOK-WIDE ( tdwide<n,n,n,n,n,n,n,n> -- tdwide<n,n,n,n,n,n,n,n> )" >str CHECK-QUIET-CANDIDATE! -1 T=

\ The arity-six failure boundary exercises the first spelling after reserved f.
STRUCTURE tdga06<a,b,c,d,e,g>
  FIELD p00 a FIELD p01 b FIELD p02 c
  FIELD p03 d FIELD p04 e FIELD p05 g
;STRUCTURE

\ The six-parameter MEM-shaped reproducer uses a,b,c,d,e,g.  Its last field is
\ schema parameter index 5, every generated input remains polymorphic, and both
\ generated directions preserve exact positions.
s" TDGA06-RT ( n bool char u8 i64 r -- n bool char u8 i64 r ) TDGA06:MAKE TDGA06:UNMAKE" >str CHECK-QUIET-CANDIDATE! -1 T=
s" TDGA06-BAD-MAKE ( bool n char u8 i64 r -- tdga06<n,bool,char,u8,i64,r> ) TDGA06:MAKE" >str CHECK-QUIET-CANDIDATE! 0 T=
s" TDGA06-BAD-UNMAKE ( tdga06<n,bool,char,u8,i64,r> -- bool n char u8 i64 r ) TDGA06:UNMAKE" >str CHECK-QUIET-CANDIDATE! 0 T=

\ The maximum declaration arity keeps 23 independent schema positions.  Its
\ generated MAKE/UNMAKE pair also executes as a physical round trip.
STRUCTURE tdga23<a,b,c,d,e,g,h,i,j,k,l,m,o,p,q,s,t,u,v,w,x,y,z>
  FIELD p00 a FIELD p01 b FIELD p02 c FIELD p03 d FIELD p04 e
  FIELD p05 g FIELD p06 h FIELD p07 i FIELD p08 j FIELD p09 k
  FIELD p10 l FIELD p11 m FIELD p12 o FIELD p13 p FIELD p14 q
  FIELD p15 s FIELD p16 t FIELD p17 u FIELD p18 v FIELD p19 w
  FIELD p20 x FIELD p21 y FIELD p22 z
  FIELD flag f FIELD integer n FIELD real r
;STRUCTURE
: TDGA23-RT ( n n n n n n n n n n n n n n n n n n n n n n n bool n r -- n n n n n n n n n n n n n n n n n n n n n n n bool n r )
   TDGA23:MAKE TDGA23:UNMAKE ;
1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 false 24 25.0 TDGA23-RT
25.0 f= TTRUE 24 T= TFALSE
23 T= 22 T= 21 T= 20 T= 19 T= 18 T= 17 T= 16 T= 15 T= 14 T= 13 T= 12 T=
11 T= 10 T= 9 T= 8 T= 7 T= 6 T= 5 T= 4 T= 3 T= 2 T= 1 T=
s" TDGA23-MAKE ( n n n n n n n n n n n n n n n n n n n n n n n bool n r -- tdga23<n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n> ) TDGA23:MAKE" >str CHECK-QUIET-CANDIDATE! -1 T=
s" TDGA23-UNMAKE ( tdga23<n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n> -- n n n n n n n n n n n n n n n n n n n n n n n bool n r ) TDGA23:UNMAKE" >str CHECK-QUIET-CANDIDATE! -1 T=
s" TDGA23-BADF ( n n n n n n n n n n n n n n n n n n n n n n n n n r -- tdga23<n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n> ) TDGA23:MAKE" >str CHECK-QUIET-CANDIDATE! 0 T=
s" TDGA23-BADN ( n n n n n n n n n n n n n n n n n n n n n n n bool bool r -- tdga23<n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n> ) TDGA23:MAKE" >str CHECK-QUIET-CANDIDATE! 0 T=
s" TDGA23-BADR ( n n n n n n n n n n n n n n n n n n n n n n n bool n n -- tdga23<n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n> ) TDGA23:MAKE" >str CHECK-QUIET-CANDIDATE! 0 T=

\ mixed payload widths: slots = max across variants.
SUMTYPE tdmix<a,b>
  VARIANT small a ;VARIANT
  VARIANT big a b n ;VARIANT
;SUMTYPE
\ ENUM: `ENUM name v0 v1 .. ;ENUM` registers a TK-ENUM
\ family — a zero-payload sum (arity 0, slots 0) — one bare variant name per
\ token becoming a payload-free SUMV row in declaration-tag order. It shares the
\ SUMTYPE close/rollback/constructor path, so the family MATCHes and gets one
\ generated constructor per variant just like an arity-0 zero-payload sum.
ENUM tdcolor
  red
  green
  blue
;ENUM
\ the bare enum tail resolves as a logical type in a signature (arity-0 family).
s" TDE-ID ( tdcolor -- tdcolor )" >str CHECK-QUIET-CANDIDATE! -1 T=
\ generated constructors: TDCOLOR:GREEN ( -- tdcolor ). A raw n is NOT the enum
\ (docs/design.md "Families and layout policies"): a payload-free enum ctor
\ takes no input and yields the enum type; a bare 0 cannot certify as tdcolor.
s" TDE-MK ( -- tdcolor ) TDCOLOR:GREEN" >str CHECK-QUIET-CANDIDATE! -1 T=
s" TDE-BAD ( -- tdcolor ) 0" >str CHECK-QUIET-CANDIDATE! 0 T=
\ a payload-free ctor rejects a spurious input just like the arity-0 sum ctors.
s" TDE-MK2 ( n -- tdcolor ) TDCOLOR:RED" >str CHECK-QUIET-CANDIDATE! 0 T=

\ STRUCTURE: `STRUCTURE name[<params>] FIELD f t .. ;STRUCTURE`
STRUCTURE tdpair<a,b>
  FIELD fst a
  FIELD snd b
;STRUCTURE
\ a concrete-arg product expands to hidden fields in a signature and transports
\ as ONE whole bundle (dup/drop are width-aware); identity flows.
s" TDP-ID ( tdpair<n,n> -- tdpair<n,n> )" >str CHECK-QUIET-CANDIDATE! -1 T=
s" TDP-DUP ( tdpair<n,n> -- tdpair<n,n> tdpair<n,n> ) dup" >str CHECK-QUIET-CANDIDATE! -1 T=
s" TDP-DROP ( tdpair<n,n> -- ) drop" >str CHECK-QUIET-CANDIDATE! -1 T=
\ hidden fields never masquerade as bare cells: the bundle cannot split into n n.
s" TDP-SPLIT ( tdpair<n,n> -- n n )" >str CHECK-QUIET-CANDIDATE! 0 T=
\ hidden physical field names never resolve in a public signature.
s" TDP-HID ( @tdpair.slot0<n,n> -- ) drop" >str CHECK-QUIET-CANDIDATE! 0 T=

\ concrete-field product (arity 0, all fields concrete n): width = field count.
STRUCTURE tdpoint
  FIELD x n
  FIELD y n
;STRUCTURE
s" TDPT-ID ( tdpoint -- tdpoint )" >str CHECK-QUIET-CANDIDATE! -1 T=

\ DERIVE init needs no package: a top-level record's init accessors check.
STRUCTURE tdtop DERIVE init FIELD x n ;STRUCTURE
s" TDTOP-GET ( mut-view<a,b,c,init<d,tdtop>> -- mut-view<a,b,c,init<d,tdtop>> n ) TDTOP:X@" >str CHECK-QUIET-CANDIDATE! -1 T=
s" TDTOP-SET ( mut-view<a,b,c,init<d,tdtop>> n -- mut-view<a,b,c,init<d,tdtop>> ) TDTOP:X!" >str CHECK-QUIET-CANDIDATE! -1 T=

\ A sum, enum or product value moves as one unit through the stack words,
\ >r/r>/r@/2>r/2r>/2r@ and a local capture; any other touch is rejected.

\ a layout value flows through untouched (identity is fine).
s" TD7-OPT-ID ( tdopt<n> -- tdopt<n> )" >str CHECK-QUIET-CANDIDATE! -1 T=
s" TD7-ENUM-ID ( tdlight -- tdlight )" >str CHECK-QUIET-CANDIDATE! -1 T=
\ whole-bundle transport ops move a sum layout value as one logical unit.
s" TD7-DROP ( tdres<n,n> -- ) drop" >str CHECK-QUIET-CANDIDATE! -1 T=
s" TD7-DUP ( tdres<n,n> -- tdres<n,n> tdres<n,n> ) dup" >str CHECK-QUIET-CANDIDATE! -1 T=
s" TD7-SWAP ( tdres<n,n> n -- n tdres<n,n> ) swap" >str CHECK-QUIET-CANDIDATE! -1 T=
s" TD7-OVER ( tdres<n,n> n -- tdres<n,n> n tdres<n,n> ) over" >str CHECK-QUIET-CANDIDATE! -1 T=
s" TD7-NIP ( tdres<n,n> n -- n ) nip" >str CHECK-QUIET-CANDIDATE! -1 T=
s" TD7-TOR ( tdres<n,n> -- tdres<n,n> ) >r r>" >str CHECK-QUIET-CANDIDATE! -1 T=
\ an enum (zero-payload) layout value is one logical cell and transports too.
s" TD7-ENUM-DROP ( tdlight -- ) drop" >str CHECK-QUIET-CANDIDATE! -1 T=
\ a layout value captures into a local as one whole bundle.
s" TD7-LOCAL ( tdres<n,n> -- n ) {: x :} x drop 0" >str CHECK-QUIET-CANDIDATE! -1 T=
\ cell families are unaffected: a one-cell tdfoo value is dropped/duped normally.
s" TD7-CELL-DROP ( tdfoo<n,n> -- n ) drop 0" >str CHECK-QUIET-CANDIDATE! -1 T=
s" TD7-CELL-DUP ( tdfoo<n,n> -- tdfoo<n,n> tdfoo<n,n> ) dup" >str CHECK-QUIET-CANDIDATE! -1 T=
\ hidden physical field names never resolve in a public signature.
s" TD7-HID-SLOT ( @tdres.slot0<n,n> -- ) drop" >str CHECK-QUIET-CANDIDATE! 0 T=
s" TD7-HID-TAG ( @tdopt.tag<n> -- ) drop" >str CHECK-QUIET-CANDIDATE! 0 T=

\ --- the full generic stack-op surface transports a bundle ---
\ every transport prim on a width-1 sum, a wider (slots=3) sum, and an enum,
\ with a cell above/below to prove only the layout bundle moves as a unit.
s" TD12-ROT ( tdres<n,n> n n -- n n tdres<n,n> ) rot" >str CHECK-QUIET-CANDIDATE! -1 T=
s" TD12-MROT ( n n tdres<n,n> -- tdres<n,n> n n ) -rot" >str CHECK-QUIET-CANDIDATE! -1 T=
s" TD12-TUCK ( n tdres<n,n> -- tdres<n,n> n tdres<n,n> ) tuck" >str CHECK-QUIET-CANDIDATE! -1 T=
s" TD12-2DUP ( tdres<n,n> n -- tdres<n,n> n tdres<n,n> n ) 2dup" >str CHECK-QUIET-CANDIDATE! -1 T=
s" TD12-2DROP ( tdres<n,n> n -- ) 2drop" >str CHECK-QUIET-CANDIDATE! -1 T=
s" TD12-2SWAP ( tdres<n,n> n n n -- n n tdres<n,n> n ) 2swap" >str CHECK-QUIET-CANDIDATE! -1 T=
s" TD12-2OVER ( tdres<n,n> n n n -- tdres<n,n> n n n tdres<n,n> n ) 2over" >str CHECK-QUIET-CANDIDATE! -1 T=
s" TD12-RAT ( tdres<n,n> -- tdres<n,n> tdres<n,n> ) >r r@ r>" >str CHECK-QUIET-CANDIDATE! -1 T=
s" TD12-2TOR ( tdres<n,n> n -- tdres<n,n> n ) 2>r 2r>" >str CHECK-QUIET-CANDIDATE! -1 T=
s" TD12-2RAT ( tdres<n,n> n -- tdres<n,n> n tdres<n,n> n ) 2>r 2r@ 2r>" >str CHECK-QUIET-CANDIDATE! -1 T=
\ a wider sum (tdmix: slots=3) still moves as ONE logical cell.
s" TD12-WIDE-DUP ( tdmix<n,n> -- tdmix<n,n> tdmix<n,n> ) dup" >str CHECK-QUIET-CANDIDATE! -1 T=
s" TD12-WIDE-SWAP ( tdmix<n,n> n -- n tdmix<n,n> ) swap" >str CHECK-QUIET-CANDIDATE! -1 T=
\ transport never lets a layout value satisfy a cell slot: swap keeps the bundle
\ whole, so a signature that splits it (claims a bare n out) still rejects.
s" TD12-SWAP-SPLIT ( tdres<n,n> n -- tdres<n,n> tdres<n,n> ) swap" >str CHECK-QUIET-CANDIDATE! 0 T=

\ --- branch-scoped locals lower ----------
\ Branch-scoped locals pop from #LOC at their join, so a local's width is read
\ by bind sequence, not by the live LOCW index.
s" TD12-BRLOC-IF ( tdres<n,n> n -- tdres<n,n> ) 0 > if {: a :} a else then" >str CHECK-QUIET-CANDIDATE! -1 T=
s" TD12-BRLOC-CASE ( tdres<n,n> n -- tdres<n,n> ) case 0 of {: a :} a endof endcase" >str CHECK-QUIET-CANDIDATE! -1 T=
\ a scalar AND a wide local bound in the same branch: certifies.
s" TD12-BRLOC-MIX ( tdres<n,n> n -- tdres<n,n> n ) 0 > if 5 {: s:n :} {: a :} a s else 0 then" >str CHECK-QUIET-CANDIDATE! -1 T=
\ a purely SCALAR branch local in a width-aware definition (the top-level wide
\ local a triggers pass-2, which replays s's bind by sequence): certifies.
s" TD12-BRLOC-SCALAR ( n tdres<n,n> -- tdres<n,n> n ) {: a :} 0 > if 5 {: s:n :} s else 9 then a swap" >str CHECK-QUIET-CANDIDATE! -1 T=
\ also certified: a wide local bound at TOP LEVEL, a scalar branch local in a
\ NON-width-aware definition, and a top-level wide local REFERENCED inside both
\ branches.
s" TD12-BROK-TOPWIDE ( n tdres<n,n> -- tdres<n,n> n ) {: a :} a swap" >str CHECK-QUIET-CANDIDATE! -1 T=
s" TD12-BROK-SCALARBR ( n -- n ) 0 > if 5 {: s:n :} s else 9 then" >str CHECK-QUIET-CANDIDATE! -1 T=
s" TD12-BROK-REF ( n tdres<n,n> -- tdres<n,n> ) {: a :} 0 > if a else a then" >str CHECK-QUIET-CANDIDATE! -1 T=

package LOCAL-BIND-CAP-TEST

$4000 constant BUF-CAP
create BUF BUF-CAP allot
variable BUF-N

: RESET ( -- ) 0 BUF-N ! ;
: APPEND ( ^u8 n -- ) {: a:^ u:n :}
   BUF-N @ u + BUF-CAP > IF s" local-bind test source overflow" EXIT-STATUS:FAILURE die THEN
   0 BEGIN dup u < WHILE
      a over + c@ BUF BUF-N @ + c!
      BUF-N @ 1 + BUF-N !
      1 +
   REPEAT drop ;
: BIND ( -- )
   s" dup 0 > if 5 {: x:n :} x drop then " APPEND ;
: SOURCE ( -- ^u8 n )
   RESET
   s" MANY-BINDS ( n -- n ) " APPEND
   257 0 ?do BIND loop
   BUF BUF-N @ ;

public

: RUN ( -- )
   SOURCE >str CHECK-QUIET-CANDIDATE! -1 T= ;

;package

LOCAL-BIND-CAP-TEST:RUN

\ --- negatives: non-transport touches still fail closed --------
\ control predicates read the top cell: a layout value is not a flag.
s" TD12-IF ( tdres<n,n> -- ) if then" >str CHECK-QUIET-CANDIDATE! 0 T=
\ arithmetic/compare/unary inspect the cell: reject a layout value.
s" TD12-ZEQ ( tdres<n,n> -- bool ) 0=" >str CHECK-QUIET-CANDIDATE! 0 T=
s" TD12-ADD ( tdres<n,n> n -- n ) +" >str CHECK-QUIET-CANDIDATE! 0 T=
\ higher-order apply (execute) must not consume a layout value as an xt/cell.
s" TD12-EXEC ( tdres<n,n> -- ) execute" >str CHECK-QUIET-CANDIDATE! 0 T=
\ memory store/fetch is a field coercion outside constructors/MATCH: reject.
s" TD12-STORE ( tdres<n,n> ^a -- ) !" >str CHECK-QUIET-CANDIDATE! 0 T=

\ --- possibly-linear layout transports reject -----------------------
\ A layout family whose args carry a linear con — or an arg still unresolved,
\ which may later bind linear — must not transport: the linear discipline
\ counts concrete linear cons, so a layout bundle would let a copy duplicate
\ (or a drop lose) the hidden payload resource. Identity flow stays legal, and
\ the same family with non-linear args transports freely.
DEFLINEAR tdown
SUMTYPE tdlin<a>
  VARIANT hold a ;VARIANT
;SUMTYPE
STRUCTURE tdlinp
  FIELD owned tdown
  FIELD value n
;STRUCTURE
SUMTYPE tdlins
  VARIANT owned tdown ;VARIANT
;SUMTYPE
s" TDLIN-DUP ( tdlin<tdown> -- tdlin<tdown> tdlin<tdown> ) dup" >str CHECK-QUIET-CANDIDATE! 0 T=
s" TDLIN-DROP ( tdlin<tdown> -- ) drop" >str CHECK-QUIET-CANDIDATE! 0 T=
s" TDLIN-NIP ( tdlin<tdown> n -- n ) nip" >str CHECK-QUIET-CANDIDATE! 0 T=
s" TDLIN-2DROP ( tdlin<tdown> n -- ) 2drop" >str CHECK-QUIET-CANDIDATE! 0 T=
s" TDLIN-LOCAL ( tdlin<tdown> -- ) {: x :} x drop" >str CHECK-QUIET-CANDIDATE! 0 T=
\ Move-class relaxation: a count-preserving move (>r r>) of a resolved linear
\ bundle conserves the count (LIN-CHECK before=after) and certifies; copy/drop
\ above reject. An OPEN-arg (may-later-bind-linear) layout stays fail-closed for
\ every transport, moves included (TDLIN-VAR-DUP).
s" TDLIN-TOR ( tdlin<tdown> -- tdlin<tdown> ) >r r>" >str CHECK-QUIET-CANDIDATE! -1 T=
\ open-arg move stays rejected.
s" TDLIN-VAR-TOR ( tdlin<a> -- tdlin<a> ) >r r>" >str CHECK-QUIET-CANDIDATE! 0 T=
\ identity needs no transport bind: the linear-carrying layout still flows.
s" TDLIN-ID ( tdlin<tdown> -- tdlin<tdown> )" >str CHECK-QUIET-CANDIDATE! -1 T=
\ the same family with a non-linear arg keeps full transport.
s" TDLIN-N-DUP ( tdlin<n> -- tdlin<n> tdlin<n> ) dup" >str CHECK-QUIET-CANDIDATE! -1 T=
s" TDLIN-N-DROP ( tdlin<n> -- ) drop" >str CHECK-QUIET-CANDIDATE! -1 T=
\ an unresolved arg may later bind linear: fail closed.
s" TDLIN-VAR-DUP ( tdlin<a> -- tdlin<a> tdlin<a> ) dup" >str CHECK-QUIET-CANDIDATE! 0 T=
\ the bare linear itself still rejects a raw drop (baseline discipline).
s" TDLIN-BARE ( tdown -- ) drop" >str CHECK-QUIET-CANDIDATE! 0 T=
\ Concrete schema nodes carry the same ownership obligation as family
\ arguments. Product fields and sum payloads therefore reject copy, drop, and
\ typed memory even though these arity-0 families have no arguments to scan.
s" TDLINC-P-DUP ( tdlinp -- tdlinp tdlinp ) dup" >str CHECK-QUIET-CANDIDATE! 0 T=
s" TDLINC-P-DROP ( tdlinp -- ) drop" >str CHECK-QUIET-CANDIDATE! 0 T=
s" TDLINC-P-STORE ( tdlinp ^tdlinp -- ) !" >str CHECK-QUIET-CANDIDATE! 0 T=
s" TDLINC-P-FETCH ( ^tdlinp -- tdlinp ) @" >str CHECK-QUIET-CANDIDATE! 0 T=
s" TDLINC-P-ID ( tdlinp -- tdlinp )" >str CHECK-QUIET-CANDIDATE! -1 T=
s" TDLINC-S-DUP ( tdlins -- tdlins tdlins ) dup" >str CHECK-QUIET-CANDIDATE! 0 T=
s" TDLINC-S-DROP ( tdlins -- ) drop" >str CHECK-QUIET-CANDIDATE! 0 T=
s" TDLINC-S-STORE ( tdlins ^tdlins -- ) !" >str CHECK-QUIET-CANDIDATE! 0 T=
s" TDLINC-S-FETCH ( ^tdlins -- tdlins ) @" >str CHECK-QUIET-CANDIDATE! 0 T=
s" TDLINC-S-ID ( tdlins -- tdlins )" >str CHECK-QUIET-CANDIDATE! -1 T=

\ --- a checked word that only moves its var takes a one-cell layout value --
\ A var the callee's body only moves (a stack word, a local, a call that only
\ moves it) takes tdlight or tdcolor at the call, as does an unsigned
\ definition's or a quotation literal's input that stack words only move; one
\ the body hands to a defer, a two-cell value and a one-cell value holding a
\ linear field, which a `dup` body would copy, are rejected, the last even by
\ an identity body and naming the linear value.
STRUCTURE tdmvlin
  FIELD owned tdown
;STRUCTURE
: TDMV-SW ( a b -- b a ) swap ;
: TDMV-ID ( a -- a ) ;
: TDMV-DUP ( a -- a a ) dup ;
: TDMV-LOC ( a -- a ) {: x :} x ;
: TDMV-SW2 ( a b -- b a ) TDMV-SW ;
defer TDMV-DF ( a -- a )
: TDMV-RD ( a -- a ) dup TDMV-DF drop ;
: TDMV-USE ( tdlight -- ) drop ;
: TDMV-UN dup drop ;
: TDMV-UR dup 0= drop ;
s" TDMV-1 ( n tdlight -- tdlight n ) TDMV-SW" >str CHECK-QUIET-CANDIDATE! -1 T=
s" TDMV-2 ( tdcolor tdcolor -- tdcolor tdcolor ) TDMV-SW" >str CHECK-QUIET-CANDIDATE! -1 T=
s" TDMV-3 ( tdlight -- tdlight ) TDMV-ID" >str CHECK-QUIET-CANDIDATE! -1 T=
s" TDMV-4 ( tdlight -- tdlight tdlight ) TDMV-DUP" >str CHECK-QUIET-CANDIDATE! -1 T=
s" TDMV-5 ( tdlight -- tdlight ) TDMV-LOC" >str CHECK-QUIET-CANDIDATE! -1 T=
s" TDMV-6 ( n tdlight -- tdlight n ) TDMV-SW2" >str CHECK-QUIET-CANDIDATE! -1 T=
s" TDMV-7 ( tdlight -- tdlight ) TDMV-RD" >str CHECK-QUIET-CANDIDATE! 0 T=
s" TDMV-8 ( n tdres<n,n> -- tdres<n,n> n ) TDMV-SW" >str CHECK-QUIET-CANDIDATE! 0 T=
s" TDMV-9 ( tdmvlin -- tdmvlin ) TDMV-ID" >str CHECK-QUIET-CANDIDATE! 0 T=
\ TDMV-LIN: SRC's rejection names its linear value.
: TDMV-LIN ( str -- ) {: s:str :}
   REJECTION:CAPTURE  s CHECK-CANDIDATE! drop
   s" `tdmvlin` may hold a linear value" >str TDT-HAS ;
s" TDMV-9 ( tdmvlin -- tdmvlin ) TDMV-ID" >str TDMV-LIN
s" TDMV-15 ( tdmvlin -- tdmvlin ) TDMV-UN" >str TDMV-LIN
s" TDMV-10 ( tdlight -- tdlight n ) [: dup TDMV-USE ;] catch" >str CHECK-QUIET-CANDIDATE! -1 T=
s" TDMV-11 ( tdlight -- tdlight ) [: dup drop ;] execute" >str CHECK-QUIET-CANDIDATE! -1 T=
s" TDMV-12 ( tdlight -- tdlight bool ) [: dup 0= ;] execute" >str CHECK-QUIET-CANDIDATE! 0 T=
s" TDMV-13 ( tdlight -- tdlight ) TDMV-UN" >str CHECK-QUIET-CANDIDATE! -1 T=
s" TDMV-14 ( tdlight -- tdlight ) TDMV-UR" >str CHECK-QUIET-CANDIDATE! 0 T=

\ --- interpret-mode + frame-metadata boundaries (docs/design.md
\ "Families and layout policies"). A layout value is a whole logical bundle, so
\ introspection and frame-crossing words either see it whole or fail closed.
\ depth/.s report raw physical cells, so they fail closed over any row holding
\ hidden fields.
s" TD12-DEPTH ( tdres<n,n> -- tdres<n,n> n ) depth" >str CHECK-QUIET-CANDIDATE! 0 T=
s" TD12-DOTS ( tdres<n,n> -- tdres<n,n> ) .s" >str CHECK-QUIET-CANDIDATE! 0 T=
\ catch/frame words: a layout value FLOWS through a stack-preserving quotation
\ (whole-row absorption) but cannot bind a quotation's polymorphic operand.
s" TD12-CATCH ( tdres<n,n> -- tdres<n,n> n ) [: ;] catch" >str CHECK-QUIET-CANDIDATE! -1 T=
s" TD12-CATCH-TOUCH ( tdres<n,n> -- n ) [: drop ;] catch" >str CHECK-QUIET-CANDIDATE! 0 T=
s" TD12-EXEC-Q ( tdres<n,n> [ a -- a ] -- a ) execute" >str CHECK-QUIET-CANDIDATE! 0 T=
\ nested evaluate is rejected in checked bodies outright (unsafe boundary).
s" TD12-EVAL ( tdres<n,n> ^u8 n -- tdres<n,n> ) evaluate" >str CHECK-QUIET-CANDIDATE! 0 T=
\ throw takes an n code: a layout value is not a throw code.
s" TD12-THROW ( tdres<n,n> -- ) throw" >str CHECK-QUIET-CANDIDATE! 0 T=

\ --- storable layouts ---
\ A layout value crosses `!`/`@` through a `^family` address; the address
\ type carries the family identity, and a var may bind a width-1 non-linear
\ layout pointee under a ptr spine. Only LAYOUT-BUFFER may introduce a
\ family-typed pointer. The one-cell ops are the exact W=1 lowering; pass 2
\ lowers W>1 from the token's width fact. Linear, open-arg, and untyped or
\ mismatched addresses fail closed.
SUMTYPE tdmemu<a>
  VARIANT uno ;VARIANT
  VARIANT dos ;VARIANT
;SUMTYPE
variable TDS1-MEM
s" TDS1-VP ( -- ^tdcolor ) TDS1-MEM" >str CHECK-QUIET-CANDIDATE! 0 T=
s" TDS1-STORE ( tdcolor ^tdcolor -- ) !" >str CHECK-QUIET-CANDIDATE! -1 T=
s" TDS1-FETCH ( ^tdcolor -- tdcolor ) @" >str CHECK-QUIET-CANDIDATE! -1 T=
s" TDS1-RT ( tdcolor ^tdcolor -- tdcolor ) tuck ! @" >str CHECK-QUIET-CANDIDATE! -1 T=
\ a zero-arity width-1 SUM gets the same memory tier as the TK-ENUM.
s" TDS1-SUM0 ( tdlight ^tdlight -- ) !" >str CHECK-QUIET-CANDIDATE! -1 T=
\ a parametric width-1 sum qualifies once its args are closed non-linear.
s" TDS1-PAR ( tdmemu<n> ^tdmemu<n> -- ) !" >str CHECK-QUIET-CANDIDATE! -1 T=
\ family identity: untyped and mismatched addresses stay rejected.
s" TDS1-BARE ( tdcolor ^a -- ) !" >str CHECK-QUIET-CANDIDATE! 0 T=
s" TDS1-BAREF ( ^a -- tdcolor ) @" >str CHECK-QUIET-CANDIDATE! 0 T=
s" TDS1-MIX ( tdlight ^tdcolor -- ) !" >str CHECK-QUIET-CANDIDATE! 0 T=
\ no n<->enum laundering in either direction.
s" TDS1-NIN ( n ^tdcolor -- ) !" >str CHECK-QUIET-CANDIDATE! 0 T=
s" TDS1-NOUT ( ^tdcolor -- n ) @" >str CHECK-QUIET-CANDIDATE! 0 T=
\ W > 1 store/fetch certifies before the executed round-trips below.
s" TDS2-WIDE ( tdres<n,n> ^tdres<n,n> -- ) !" >str CHECK-QUIET-CANDIDATE! -1 T=
s" TDS2-WIDEF ( ^tdres<n,n> -- tdres<n,n> ) @" >str CHECK-QUIET-CANDIDATE! -1 T=
\ wide family mismatch and scalar laundering stay rejected.
s" TDS2-WMIX ( tdres<n,n> ^tdmix<n,n> -- ) !" >str CHECK-QUIET-CANDIDATE! 0 T=
s" TDS2-WNIN ( n ^tdres<n,n> -- ) !" >str CHECK-QUIET-CANDIDATE! 0 T=
s" TDS2-WNOUT ( ^tdres<n,n> -- n ) @" >str CHECK-QUIET-CANDIDATE! 0 T=
\ linear / open args stay fail-closed even at width 1.
s" TDS1-LIN ( tdmemu<tdown> ^tdmemu<tdown> -- ) !" >str CHECK-QUIET-CANDIDATE! 0 T=
s" TDS1-OPEN ( tdmemu<a> ^tdmemu<a> -- ) !" >str CHECK-QUIET-CANDIDATE! 0 T=
\ executed round-trip: store an enum, fetch it, MATCH the fetched value.
1 LAYOUT-BUFFER TDS1-BUF tdcolor
: TDS1-P ( -- ^tdcolor ) 0 TDS1-BUF ;
: TDS1-PUT ( tdcolor -- ) TDS1-P ! ;
: TDS1-GET ( -- tdcolor ) TDS1-P @ ;
: TDS1-CODE ( -- n )
   TDS1-GET MATCH tdcolor
     red OF 0 ENDOF
     green OF 1 ENDOF
     blue OF 2 ENDOF
   ;MATCH ;
TDCOLOR:GREEN TDS1-PUT TDS1-CODE 1 T=
TDCOLOR:BLUE TDS1-PUT TDS1-CODE 2 T=

\ Executed W=2 round-trip through sealed typed storage.
1 LAYOUT-BUFFER TDS2-RES-BUF tdres<n,n>
: TDS2-RES-P ( -- ^tdres<n,n> ) 0 TDS2-RES-BUF ;
: TDS2-RES-PUT ( tdres<n,n> -- ) TDS2-RES-P ! ;
: TDS2-RES-GET ( -- tdres<n,n> ) TDS2-RES-P @ ;
: TDS2-RES-SEED ( -- tdres<n,n> ) 37 TDRES:ERR ;
: TDS2-RES-WRITE ( -- ) TDS2-RES-SEED TDS2-RES-PUT ;
: TDS2-RES-VAL ( -- n )
   TDS2-RES-GET MATCH tdres
     ok OF ENDOF
     err OF ENDOF
   ;MATCH ;
TDS2-RES-WRITE TDS2-RES-VAL 37 T=

\ Arbitrary W=4 family: both the full payload and zero-filled padding survive.
1 LAYOUT-BUFFER TDS2-MIX-BUF tdmix<n,n>
: TDS2-MIX-P ( -- ^tdmix<n,n> ) 0 TDS2-MIX-BUF ;
: TDS2-MIX-PUT ( tdmix<n,n> -- ) TDS2-MIX-P ! ;
: TDS2-MIX-GET ( -- tdmix<n,n> ) TDS2-MIX-P @ ;
: TDS2-MIX-BIG ( -- tdmix<n,n> ) 91 92 93 TDMIX:BIG ;
: TDS2-MIX-SMALL ( -- tdmix<n,n> ) 41 TDMIX:SMALL ;
: TDS2-MIX-BIG! ( -- ) TDS2-MIX-BIG TDS2-MIX-PUT ;
: TDS2-MIX-SMALL! ( -- ) TDS2-MIX-SMALL TDS2-MIX-PUT ;
: TDS2-MIX-SUM ( -- n )
   TDS2-MIX-GET MATCH tdmix
     small OF ENDOF
     big OF + + ENDOF
   ;MATCH ;
TDS2-MIX-BIG! TDS2-MIX-SUM 276 T=
TDS2-MIX-SMALL!
TDS2-MIX-SUM 41 T=

\ --- closed layout-family payloads -------------------------------------------
\ A closed, non-linear, arity-0 layout family may type a STRUCTURE field or SUM
\ payload: its SCH-APP schema carries the resolved family id, physical slot
\ widths recurse through the referenced family, PF.SLOT is the cumulative cell
\ offset, and MAKE/UNMAKE render the field as its family, so a swapped
\ enum-field argument order is a checker reject.
STRUCTURE tdprec
  FIELD col tdcolor
  FIELD lum tdlight
  FIELD cnt n
;STRUCTURE
\ generated MAKE/UNMAKE consume/produce the fields as their families.
s" TDP1 ( tdcolor tdlight n -- tdprec ) TDPREC:MAKE" >str CHECK-QUIET-CANDIDATE! -1 T=
s" TDP2 ( tdprec -- tdcolor tdlight n ) TDPREC:UNMAKE" >str CHECK-QUIET-CANDIDATE! -1 T=
\ the born-typed guarantee: swapped same-width enum fields are a checker reject.
s" TDP3 ( tdlight tdcolor n -- tdprec ) TDPREC:MAKE" >str CHECK-QUIET-CANDIDATE! 0 T=
\ typed field accessor: a checked composition over UNMAKE.
s" TDP4 ( tdprec -- tdcolor ) TDPREC:UNMAKE drop drop" >str CHECK-QUIET-CANDIDATE! -1 T=
\ executed round-trip: MAKE, destructure, MATCH the enum field.
: TDP-MK ( -- tdprec ) TDCOLOR:BLUE TDLIGHT:RED 7 TDPREC:MAKE ;
: TDP-COL ( tdprec -- tdcolor ) TDPREC:UNMAKE drop drop ;
: TDP-CODE ( -- n )
   TDP-MK TDP-COL MATCH tdcolor
     red OF 0 ENDOF
     green OF 1 ENDOF
     blue OF 2 ENDOF
   ;MATCH ;
TDP-CODE 2 T=
\ Wide STRUCTURE memory transports its full W=3 bundle. A different family or
\ scalar result cannot cross the boundary even when the physical representation
\ is cell-based.
s" TDP-MEM-S ( tdprec ^tdprec -- ) !" >str CHECK-QUIET-CANDIDATE! -1 T=
s" TDP-MEM-F ( ^tdprec -- tdprec ) @" >str CHECK-QUIET-CANDIDATE! -1 T=
s" TDP-MEM-MIX ( tdprec ^tdmix<n,n> -- ) !" >str CHECK-QUIET-CANDIDATE! 0 T=
s" TDP-MEM-N ( ^tdprec -- n ) @" >str CHECK-QUIET-CANDIDATE! 0 T=
1 LAYOUT-BUFFER TDP-BUF tdprec
: TDP-P ( -- ^tdprec ) 0 TDP-BUF ;
: TDP-PUT ( tdprec -- ) TDP-P ! ;
: TDP-GET ( -- tdprec ) TDP-P @ ;
: TDP-WRITE ( -- ) TDP-MK TDP-PUT ;
: TDP-MEM-COL ( -- n )
   TDP-GET TDPREC:UNMAKE drop drop MATCH tdcolor
     red OF 0 ENDOF
     green OF 1 ENDOF
     blue OF 2 ENDOF
   ;MATCH ;
: TDP-MEM-LIGHT ( -- n )
   TDP-GET TDPREC:UNMAKE drop nip MATCH tdlight
     red OF 0 ENDOF
     green OF 1 ENDOF
     blue OF 2 ENDOF
   ;MATCH ;
: TDP-MEM-N@ ( -- n ) TDP-GET TDPREC:UNMAKE nip nip ;
TDP-WRITE
TDP-MEM-COL 2 T=  TDP-MEM-LIGHT 0 T=  TDP-MEM-N@ 7 T=
\ Closed wider payloads preserve physical width through products and sums.
SUMTYPE tdpw
  VARIANT one n ;VARIANT
;SUMTYPE
s" STRUCTURE tdpwide FIELD w tdpw ;STRUCTURE" TDT-EVAL-CATCH 0 T=
s" TDP-WIDE-MAKE ( tdpw -- tdpwide ) TDPWIDE:MAKE" >str CHECK-QUIET-CANDIDATE! -1 T=
s" SUMTYPE tdpnest VARIANT value tdpw ;VARIANT ;SUMTYPE" TDT-EVAL-CATCH 0 T=
s" TDP-NEST-MAKE ( n -- tdpnest ) construct tdpw one construct tdpnest value" >str CHECK-QUIET-CANDIDATE! -1 T=

\ A closed wide STRUCTURE is one logical value backed by one hidden parameter per
\ physical cell.
STRUCTURE tdw12
  FIELD f0 n
  FIELD f1 n
  FIELD f2 n
  FIELD f3 n
  FIELD f4 n
  FIELD f5 n
  FIELD f6 n
  FIELD f7 n
  FIELD f8 n
  FIELD f9 n
  FIELD f10 n
  FIELD f11 n
;STRUCTURE
s" TDW12-PAIR ( tdw12 tdw12 -- tdw12 tdw12 )" >str CHECK-QUIET-CANDIDATE! -1 T=
STRUCTURE tdw34
  FIELD f0 n
  FIELD f1 n
  FIELD f2 n
  FIELD f3 n
  FIELD f4 n
  FIELD f5 n
  FIELD f6 n
  FIELD f7 n
  FIELD f8 n
  FIELD f9 n
  FIELD f10 n
  FIELD f11 n
  FIELD f12 n
  FIELD f13 n
  FIELD f14 n
  FIELD f15 n
  FIELD f16 n
  FIELD f17 n
  FIELD f18 n
  FIELD f19 n
  FIELD f20 n
  FIELD f21 n
  FIELD f22 n
  FIELD f23 n
  FIELD f24 n
  FIELD f25 n
  FIELD f26 n
  FIELD f27 n
  FIELD f28 n
  FIELD f29 n
  FIELD f30 n
  FIELD f31 n
  FIELD f32 n
  FIELD f33 n
;STRUCTURE
s" TDW34-MAKE ( n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n -- tdw34 ) TDW34:MAKE" >str CHECK-QUIET-CANDIDATE! -1 T=
s" TDW34-UNMAKE ( tdw34 -- n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n ) TDW34:UNMAKE" >str CHECK-QUIET-CANDIDATE! -1 T=
s" TDW34-PAIR ( tdw34 tdw34 -- tdw34 tdw34 )" >str CHECK-QUIET-CANDIDATE! -1 T=
s" TDW34-QUOTE ( tdw34 -- tdw34 n ) [: ;] catch" >str CHECK-QUIET-CANDIDATE! -1 T=
\ A published wide effect replays in later checks.
: TDW34-STORED ( tdw34 -- tdw34 ) ;
s" TDW34-REPLAY ( tdw34 -- tdw34 ) TDW34-STORED" >str CHECK-QUIET-CANDIDATE! -1 T=
\ Rejection rolls the candidate back without damaging the stored wide effect.
s" TDW34-BAD ( tdw34 -- n )" >str CHECK-QUIET-CANDIDATE! 0 T=
REJECTION:CAPTURE
s" TDW34-BAD-USE ( tdw34 -- n ) TDW34-BAD" >str CHECK-CANDIDATE! 0 T=
REJECTION:BUFFER$ s\" error[E-UNDEFINED]: cannot find word `TDW34-BAD`\n" >str CONTAINS? TTRUE
REJECTION:BUFFER$ s\"  --> <input>:1:30\n" >str CONTAINS? TTRUE
s" TDW34-REPLAY2 ( tdw34 -- tdw34 ) TDW34-STORED" >str CHECK-QUIET-CANDIDATE! -1 T=
\ A product containing W34 is one W34 wide, not W34^2.
STRUCTURE tdw34nest
  FIELD inner tdw34
;STRUCTURE
s" TDW34-NEST ( tdw34nest -- tdw34nest )" >str CHECK-QUIET-CANDIDATE! -1 T=

\ A STRUCTURE of 256 one-cell fields: MAKE takes 256 cells and UNMAKE leaves the
\ last field on top.
STRUCTURE tdw256
  FIELD f0 n
  FIELD f1 n
  FIELD f2 n
  FIELD f3 n
  FIELD f4 n
  FIELD f5 n
  FIELD f6 n
  FIELD f7 n
  FIELD f8 n
  FIELD f9 n
  FIELD f10 n
  FIELD f11 n
  FIELD f12 n
  FIELD f13 n
  FIELD f14 n
  FIELD f15 n
  FIELD f16 n
  FIELD f17 n
  FIELD f18 n
  FIELD f19 n
  FIELD f20 n
  FIELD f21 n
  FIELD f22 n
  FIELD f23 n
  FIELD f24 n
  FIELD f25 n
  FIELD f26 n
  FIELD f27 n
  FIELD f28 n
  FIELD f29 n
  FIELD f30 n
  FIELD f31 n
  FIELD f32 n
  FIELD f33 n
  FIELD f34 n
  FIELD f35 n
  FIELD f36 n
  FIELD f37 n
  FIELD f38 n
  FIELD f39 n
  FIELD f40 n
  FIELD f41 n
  FIELD f42 n
  FIELD f43 n
  FIELD f44 n
  FIELD f45 n
  FIELD f46 n
  FIELD f47 n
  FIELD f48 n
  FIELD f49 n
  FIELD f50 n
  FIELD f51 n
  FIELD f52 n
  FIELD f53 n
  FIELD f54 n
  FIELD f55 n
  FIELD f56 n
  FIELD f57 n
  FIELD f58 n
  FIELD f59 n
  FIELD f60 n
  FIELD f61 n
  FIELD f62 n
  FIELD f63 n
  FIELD f64 n
  FIELD f65 n
  FIELD f66 n
  FIELD f67 n
  FIELD f68 n
  FIELD f69 n
  FIELD f70 n
  FIELD f71 n
  FIELD f72 n
  FIELD f73 n
  FIELD f74 n
  FIELD f75 n
  FIELD f76 n
  FIELD f77 n
  FIELD f78 n
  FIELD f79 n
  FIELD f80 n
  FIELD f81 n
  FIELD f82 n
  FIELD f83 n
  FIELD f84 n
  FIELD f85 n
  FIELD f86 n
  FIELD f87 n
  FIELD f88 n
  FIELD f89 n
  FIELD f90 n
  FIELD f91 n
  FIELD f92 n
  FIELD f93 n
  FIELD f94 n
  FIELD f95 n
  FIELD f96 n
  FIELD f97 n
  FIELD f98 n
  FIELD f99 n
  FIELD f100 n
  FIELD f101 n
  FIELD f102 n
  FIELD f103 n
  FIELD f104 n
  FIELD f105 n
  FIELD f106 n
  FIELD f107 n
  FIELD f108 n
  FIELD f109 n
  FIELD f110 n
  FIELD f111 n
  FIELD f112 n
  FIELD f113 n
  FIELD f114 n
  FIELD f115 n
  FIELD f116 n
  FIELD f117 n
  FIELD f118 n
  FIELD f119 n
  FIELD f120 n
  FIELD f121 n
  FIELD f122 n
  FIELD f123 n
  FIELD f124 n
  FIELD f125 n
  FIELD f126 n
  FIELD f127 n
  FIELD f128 n
  FIELD f129 n
  FIELD f130 n
  FIELD f131 n
  FIELD f132 n
  FIELD f133 n
  FIELD f134 n
  FIELD f135 n
  FIELD f136 n
  FIELD f137 n
  FIELD f138 n
  FIELD f139 n
  FIELD f140 n
  FIELD f141 n
  FIELD f142 n
  FIELD f143 n
  FIELD f144 n
  FIELD f145 n
  FIELD f146 n
  FIELD f147 n
  FIELD f148 n
  FIELD f149 n
  FIELD f150 n
  FIELD f151 n
  FIELD f152 n
  FIELD f153 n
  FIELD f154 n
  FIELD f155 n
  FIELD f156 n
  FIELD f157 n
  FIELD f158 n
  FIELD f159 n
  FIELD f160 n
  FIELD f161 n
  FIELD f162 n
  FIELD f163 n
  FIELD f164 n
  FIELD f165 n
  FIELD f166 n
  FIELD f167 n
  FIELD f168 n
  FIELD f169 n
  FIELD f170 n
  FIELD f171 n
  FIELD f172 n
  FIELD f173 n
  FIELD f174 n
  FIELD f175 n
  FIELD f176 n
  FIELD f177 n
  FIELD f178 n
  FIELD f179 n
  FIELD f180 n
  FIELD f181 n
  FIELD f182 n
  FIELD f183 n
  FIELD f184 n
  FIELD f185 n
  FIELD f186 n
  FIELD f187 n
  FIELD f188 n
  FIELD f189 n
  FIELD f190 n
  FIELD f191 n
  FIELD f192 n
  FIELD f193 n
  FIELD f194 n
  FIELD f195 n
  FIELD f196 n
  FIELD f197 n
  FIELD f198 n
  FIELD f199 n
  FIELD f200 n
  FIELD f201 n
  FIELD f202 n
  FIELD f203 n
  FIELD f204 n
  FIELD f205 n
  FIELD f206 n
  FIELD f207 n
  FIELD f208 n
  FIELD f209 n
  FIELD f210 n
  FIELD f211 n
  FIELD f212 n
  FIELD f213 n
  FIELD f214 n
  FIELD f215 n
  FIELD f216 n
  FIELD f217 n
  FIELD f218 n
  FIELD f219 n
  FIELD f220 n
  FIELD f221 n
  FIELD f222 n
  FIELD f223 n
  FIELD f224 n
  FIELD f225 n
  FIELD f226 n
  FIELD f227 n
  FIELD f228 n
  FIELD f229 n
  FIELD f230 n
  FIELD f231 n
  FIELD f232 n
  FIELD f233 n
  FIELD f234 n
  FIELD f235 n
  FIELD f236 n
  FIELD f237 n
  FIELD f238 n
  FIELD f239 n
  FIELD f240 n
  FIELD f241 n
  FIELD f242 n
  FIELD f243 n
  FIELD f244 n
  FIELD f245 n
  FIELD f246 n
  FIELD f247 n
  FIELD f248 n
  FIELD f249 n
  FIELD f250 n
  FIELD f251 n
  FIELD f252 n
  FIELD f253 n
  FIELD f254 n
  FIELD f255 n
;STRUCTURE
: TDW256-MK ( -- tdw256 )
   0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15
   16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31
   32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47
   48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63
   64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79
   80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95
   96 97 98 99 100 101 102 103 104 105 106 107 108 109 110 111
   112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127
   128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143
   144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159
   160 161 162 163 164 165 166 167 168 169 170 171 172 173 174 175
   176 177 178 179 180 181 182 183 184 185 186 187 188 189 190 191
   192 193 194 195 196 197 198 199 200 201 202 203 204 205 206 207
   208 209 210 211 212 213 214 215 216 217 218 219 220 221 222 223
   224 225 226 227 228 229 230 231 232 233 234 235 236 237 238 239
   240 241 242 243 244 245 246 247 248 249 250 251 252 253 254 255
   TDW256:MAKE ;
: TDW-DROP16 ( n n n n n n n n n n n n n n n n -- )
   2drop 2drop 2drop 2drop 2drop 2drop 2drop 2drop ;
: TDW256-LAST ( tdw256 -- n )
   TDW256:UNMAKE >r
   TDW-DROP16 TDW-DROP16 TDW-DROP16 TDW-DROP16 TDW-DROP16
   TDW-DROP16 TDW-DROP16 TDW-DROP16 TDW-DROP16 TDW-DROP16
   TDW-DROP16 TDW-DROP16 TDW-DROP16 TDW-DROP16 TDW-DROP16
   2drop 2drop 2drop 2drop 2drop 2drop 2drop drop
   r> ;
: TDW256-READ ( -- n ) TDW256-MK TDW256-LAST ;
TDW256-READ 255 T=

s" WF6 ( tdres<n,n> -- ) ?dup drop drop" >str CHECK-QUIET-CANDIDATE! 0 T=
s" WF7 ( n -- n )" >str CHECK-QUIET-CANDIDATE! -1 T=
\ TFAM, TYPE and MATCH are reserved, so each accepting declaration also proves
\ user source registers families without opening a reserved package.
\ package-scoped declarations: family rows carry the active package and the
\ active visibility mode. Plain user packages — no reserved package is opened.
package tdpa
public
SUMTYPE tres<a>
  VARIANT yes a ;VARIANT
;SUMTYPE
private
NEWTYPE tpriv<a>
;package
\ same tail in a second package registers without aliasing (docs/design.md
\ "Families and layout policies").
package tdpb
public
NEWTYPE tres<a>
;package

\ package-scoped SIG resolution: unqualified family tokens resolve through the
\ ACTIVE package, then the globals and `using` imports; qualified PKG:tail folds
\ the qualifier, requires a lowercase tail, and resolves public rows only (plus
\ the active package's own private rows).
package tdpa
s" TPOK-LOCAL ( tres<n> -- tres<n> )" >str CHECK-QUIET-CANDIDATE! -1 T=
s" TPOK-PRIV ( tpriv<n> -- tpriv<n> )" >str CHECK-QUIET-CANDIDATE! -1 T=
s" TPOK-QSELF ( tdpa:tpriv<n> -- tdpa:tpriv<n> )" >str CHECK-QUIET-CANDIDATE! -1 T=
;package
\ cross-package: a private family never resolves, qualified or not...
s" TXBAD-PRIV ( tpriv<n> -- ) drop" >str CHECK-QUIET-CANDIDATE! 0 T=
s" TXBAD-QPRIV ( tdpa:tpriv<n> -- ) drop" >str CHECK-QUIET-CANDIDATE! 0 T=
\ ...a public family outside the open package does not resolve bare...
s" TXBAD-AMBIG ( tres<n> -- ) drop" >str CHECK-QUIET-CANDIDATE! 0 T=
\ ...and qualified references resolve each package distinctly.
s" TQOK-A ( tdpa:tres<n> -- tdpa:tres<n> )" >str CHECK-QUIET-CANDIDATE! -1 T=
s" TQOK-B ( tdpb:tres<n> -- tdpb:tres<n> )" >str CHECK-QUIET-CANDIDATE! -1 T=
s" TQOK-FOLD ( TDPA:tres<n> -- tdpa:tres<n> )" >str CHECK-QUIET-CANDIDATE! -1 T=
s" TQBAD-CASE ( TDPA:Tres<n> -- ) drop" >str CHECK-QUIET-CANDIDATE! 0 T=
\ same tail, different package: no unification at the SIG-parse level.
s" TQBAD-XUNIFY ( tdpa:tres<n> -- tdpb:tres<n> )" >str CHECK-QUIET-CANDIDATE! 0 T=
s" TQBAD-XUNIFY2 ( tdpb:tres<n> -- tdpa:tres<n> )" >str CHECK-QUIET-CANDIDATE! 0 T=
\ inside tdpb, unqualified tres is tdpb's own — never tdpa's same tail.
package tdpb
s" TPOK-OWN ( tres<n> -- tdpb:tres<n> )" >str CHECK-QUIET-CANDIDATE! -1 T=
s" TPBAD-OTHER ( tres<n> -- tdpa:tres<n> ) " >str CHECK-QUIET-CANDIDATE! 0 T=
;package
\ unknown qualifiers and malformed multi-colon tokens reject as unknown types.
s" TQBAD-NOPKG ( nopkg:tres<n> -- ) drop" >str CHECK-QUIET-CANDIDATE! 0 T=
s" TQBAD-COLONS ( tdpa:tres:x<n> -- ) drop" >str CHECK-QUIET-CANDIDATE! 0 T=
\ hidden physical names never resolve in public signatures.
s" TQBAD-HIDDEN ( @tdres.tag<n,n> -- ) drop" >str CHECK-QUIET-CANDIDATE! 0 T=

\ Inside tdpt and its parts a qualifier names tdpt's part before a top-level
\ package of its name; outside tdpt it never reaches the part. The top-level
\ tdpt-s:thing takes an argument and the part's none, so arity shows the row.
package tdpt-s
public
NEWTYPE thing<a>
;package
package tdpt
package tdpt-b
public
NEWTYPE thing
: KEEP ( thing -- thing ) ;
private
NEWTYPE hid
s" TDPT-OWN-HID ( tdpt-b:hid -- tdpt-b:hid )" >str CHECK-QUIET-CANDIDATE! -1 T=
;package
package tdpt-s
public
NEWTYPE thing
;package
s" TDPT-PARENT ( tdpt-b:thing -- tdpt-b:thing )" >str CHECK-QUIET-CANDIDATE! -1 T=
s" TDPT-HID ( tdpt-b:hid -- ) drop" >str CHECK-QUIET-CANDIDATE! 0 T=
s" TDPT-SHADOW ( tdpt-s:thing -- tdpt-s:thing )" >str CHECK-QUIET-CANDIDATE! -1 T=
package tdpt-c
s" TDPT-SIBLING ( tdpt-b:thing -- tdpt-b:thing )" >str CHECK-QUIET-CANDIDATE! -1 T=
s" TDPT-SIBLING-HID ( tdpt-b:hid -- ) drop" >str CHECK-QUIET-CANDIDATE! 0 T=
: TDPT-TAKE ( tdpt-b:thing -- tdpt-b:thing ) tdpt-b:KEEP ;
;package
;package
s" TDPT-OUTSIDE ( tdpt-b:thing -- ) drop" >str CHECK-QUIET-CANDIDATE! 0 T=
s" TDPT-TOP ( tdpt-s:thing<n> -- tdpt-s:thing<n> )" >str CHECK-QUIET-CANDIDATE! -1 T=

\ A private STRUCTURE declared in a part names its family B:tail in its
\ generated words, which the part resolves. Each step evaluates under the catch
\ so a rejected declaration fails its test and the load goes on.
package tdpr
package tdpr-p
s" STRUCTURE tdpr-pt FIELD x n FIELD y n ;STRUCTURE" TDT-EVAL-CATCH 0 T=
s" STRUCTURE tdpr-rec DERIVE addr FIELD id n FIELD w n ;STRUCTURE"
TDT-EVAL-CATCH 0 T=
s" STRUCTURE tdpr-box OPAQUE DERIVE addr FIELD x n ;STRUCTURE"
TDT-EVAL-CATCH 0 T=
s" : TDPR-Y ( -- n ) 3 4 TDPR-PT-MAKE TDPR-PT-UNMAKE nip ; TDPR-Y 4 T="
TDT-EVAL-CATCH 0 T=
s" 1 TYPED-BUFFER TDPR-RECS tdpr-rec" TDT-EVAL-CATCH 0 T=
s" : TDPR-W ( -- n ) 7 0 TDPR-RECS TDPR-REC-W ! 0 TDPR-RECS TDPR-REC-W @ ;"
TDT-EVAL-CATCH 0 T=
s" TDPR-W 7 T=" TDT-EVAL-CATCH 0 T=
;package
;package

\ A package-owned family may share a tail with a global family. The exact
\ identities keep separate arities: the owner gets its own row for a bare tail,
\ while top level and unrelated packages keep the global row for that bare tail.
package TDPM
public
NEWTYPE span<a,b,c,d,e,g>
private
NEWTYPE tier<a>

s" TPC-OWNER ( span<n,n,n,n,n,n> -- TDPM:span<n,n,n,n,n,n> )" >str CHECK-QUIET-CANDIDATE! -1 T=
s" TPC-OWN-PRIVATE ( tier<n> -- TDPM:tier<n> )" >str CHECK-QUIET-CANDIDATE! -1 T=
s" NEWTYPE span<a,b,c,d,e,g>"
s\" error[E-BAD-DECLARATION]: bad newtype declaration `span`\n" >str
s" span" >str s" duplicate family" >str TDT-AT
s\"   = help: rename the family: this package already declares a family with this name\n" >str TDT-HAS
\ This failure creates a family and one variant before the duplicate variant
\ rejects; the declaration transaction must retire every provisional row.
s" SUMTYPE rolled-span<a,b,c,d,e,g> VARIANT ok a ;VARIANT VARIANT ok b ;VARIANT ;SUMTYPE"
s\" error[E-BAD-DECLARATION]: bad sumtype declaration `rolled-span`\n" >str
s" ok" >str s" duplicate variant" >str TDT-AT
s\"   = help: give each variant of this declaration its own name\n" >str TDT-HAS
\ The name is free again: a stranded index row would make the bucket walk reach
\ a row the store no longer holds, and this declaration would fail.
SUMTYPE rolled-span<a,b,c,d,e,g> VARIANT ok a ;VARIANT VARIANT other b ;VARIANT ;SUMTYPE
;package

package OTHER
public
NEWTYPE tier<a,b>
;package

\ The existing global span remains the bare top-level identity.
s" TPC-GLOBAL ( span<n,n,n> -- span<n,n,n> )" >str CHECK-QUIET-CANDIDATE! -1 T=
s" TPC-PKG-QUAL ( TDPM:span<n,n,n,n,n,n> -- TDPM:span<n,n,n,n,n,n> )" >str CHECK-QUIET-CANDIDATE! -1 T=
s" TPC-GLOBAL-ARITY ( span<n,n,n,n,n,n> -- ) drop" >str CHECK-QUIET-CANDIDATE! 0 T=
s" TPC-PKG-ARITY ( TDPM:span<n,n,n> -- ) drop" >str CHECK-QUIET-CANDIDATE! 0 T=
s" TPC-CROSS-A ( span<n,n,n> -- TDPM:span<n,n,n,n,n,n> )" >str CHECK-QUIET-CANDIDATE! 0 T=
s" TPC-CROSS-B ( TDPM:span<n,n,n,n,n,n> -- span<n,n,n> )" >str CHECK-QUIET-CANDIDATE! 0 T=
\ A foreign private row is invisible, and a foreign public family needs its
\ package named or imported: the one public `tier` does not resolve bare.
s" TPC-PRIVATE-HIDDEN ( TDPM:tier<n> -- ) drop" >str CHECK-QUIET-CANDIDATE! 0 T=
s" TPC-PUBLIC-BARE ( tier<n,n> -- OTHER:tier<n,n> )" >str CHECK-QUIET-CANDIDATE! 0 T=
s" TPC-PUBLIC-QUAL ( OTHER:tier<n,n> -- OTHER:tier<n,n> )" >str CHECK-QUIET-CANDIDATE! -1 T=

\ Stored effects retain exact qualified identity and arity when replayed.
: TPC-PKG-STORED ( TDPM:span<n,n,n,n,n,n> -- TDPM:span<n,n,n,n,n,n> ) ;
s" TPC-PKG-REPLAY ( TDPM:span<n,n,n,n,n,n> -- TDPM:span<n,n,n,n,n,n> ) TPC-PKG-STORED" >str CHECK-QUIET-CANDIDATE! -1 T=
s" TPC-PKG-REPLAY-BAD ( span<n,n,n> -- span<n,n,n> ) TPC-PKG-STORED" >str CHECK-QUIET-CANDIDATE! 0 T=

\ An inferred effect in TDPM writes the global span bare and TDPM's own span
\ qualified; the hook certifies both only if each reads back to the same type.
: TPC-GLOBAL-KEEP ( span<n,n,n> -- span<n,n,n> ) ;
package TDPM
public
: TPC-FACTS-GLOBAL TPC-GLOBAL-KEEP ;
: TPC-FACTS-OWN TPC-PKG-STORED ;
;package
s" TPC-FACTS-GLOBAL-REPLAY ( span<n,n,n> -- span<n,n,n> ) TDPM:TPC-FACTS-GLOBAL" >str CHECK-QUIET-CANDIDATE! -1 T=
s" TPC-FACTS-OWN-REPLAY ( TDPM:span<n,n,n,n,n,n> -- TDPM:span<n,n,n,n,n,n> ) TDPM:TPC-FACTS-OWN" >str CHECK-QUIET-CANDIDATE! -1 T=

\ These load only if their facts text reads back: 24 sibling binders, 250 levels.
: TFX-BINDERS ( forall<p,[ n -- n ]> forall<p,[ n -- n ]> forall<p,[ n -- n ]> forall<p,[ n -- n ]> forall<p,[ n -- n ]> forall<p,[ n -- n ]> forall<p,[ n -- n ]> forall<p,[ n -- n ]> forall<p,[ n -- n ]> forall<p,[ n -- n ]> forall<p,[ n -- n ]> forall<p,[ n -- n ]> forall<p,[ n -- n ]> forall<p,[ n -- n ]> forall<p,[ n -- n ]> forall<p,[ n -- n ]> forall<p,[ n -- n ]> forall<p,[ n -- n ]> forall<p,[ n -- n ]> forall<p,[ n -- n ]> forall<p,[ n -- n ]> forall<p,[ n -- n ]> forall<p,[ n -- n ]> forall<p,[ n -- n ]> -- ) drop drop drop drop drop drop drop drop drop drop drop drop drop drop drop drop drop drop drop drop drop drop drop drop ;
STRUCTURE tfx-nest<a> FIELD a a FIELD b n ;STRUCTURE
: TFX-DEEP ( tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<tfx-nest<n>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> -- ) drop ;

\ The record's `found` note, in prose and JSON, keeps the package qualifier and
\ renders it case-folded: the declaring package was written `package TDPM`, but
\ a rendered family name is spelled canonically, the way word names and the
\ used-package row already are. `token` and the arity message echo what the
\ author typed, so they keep `TDPM:span`. The folded row still names the family
\ back, because a signature's qualifier folds before lookup.
REJECTION:CAPTURE
s" TPC-RENDER ( TDPM:span<n,n,n,n,n,n> -- n )" >str CHECK-CANDIDATE! 0 T=
REJECTION:BUFFER$ s\"   = found: tdpm:span<n,n,n,n,n,n>\n" >str CONTAINS? TTRUE
REJECTION:BUFFER$ s" found: TDPM:span<n,n,n,n,n,n>" >str CONTAINS? TFALSE
REJECTION:CAPTURE  true REJECTION:JSON !
s" TPC-JSON-RENDER ( TDPM:span<n,n,n,n,n,n> -- n )" >str CHECK-CANDIDATE! 0 T=
REJECTION:BUFFER$ s\" \"found\":\"tdpm:span<n,n,n,n,n,n>\"" >str CONTAINS? TTRUE
\ The body has no `;`: the record places an empty token at the text's end.
REJECTION:BUFFER$ s\" \"column\":48,\"byte_start\":47,\"byte_end\":47,\"token\":\"\"" >str CONTAINS? TTRUE
REJECTION:CAPTURE
s" TPC-JSON-ARITY ( TDPM:span<n,n,n> -- ) drop" >str CHECK-CANDIDATE! 0 T=
REJECTION:BUFFER$ s\" \"token\":\"TDPM:span\"" >str CONTAINS? TTRUE
REJECTION:BUFFER$ s" takes 6 type arguments, not 3" >str CONTAINS? TTRUE
false REJECTION:JSON !
REJECTION:CAPTURE

\ negative declarations: each record names its reason.

\ a missing name is placed at the keyword, and its help names that keyword.
s" NEWTYPE"
s\" error[E-BAD-DECLARATION]: bad newtype declaration\n" >str
s" NEWTYPE" >str s" missing name" >str TDT-AT
s\"   = help: write a name after `NEWTYPE`, as in `NEWTYPE meters`\n" >str TDT-HAS
s" SUMTYPE"
s\" error[E-BAD-DECLARATION]: bad sumtype declaration\n" >str
s" SUMTYPE" >str s" missing name" >str TDT-AT
s\"   = help: write a name after `SUMTYPE`, as in `SUMTYPE shape`\n" >str TDT-HAS
s" SUMTYPE tdshape VARIANT ;SUMTYPE"
s\" error[E-BAD-DECLARATION]: bad sumtype declaration `tdshape`\n" >str
s" VARIANT" >str s" missing name" >str TDT-AT
s\"   = help: write a name after `VARIANT`, as in `VARIANT none ;VARIANT`\n" >str TDT-HAS

\ tools/check.f reads argv and stops at its first rejection, so each record runs
\ in its own child. TDT-CHECKED: NAME holding SRC exits 70; stderr is HEAD,
\ NAME's canonical path, then TAIL.
s" type-decl-nameless" >str E2E-TREE:TREE
E2E-TREE:ROOT$ FS:CLEANUP-TREE+
: TDT-CHECKED ( ^u8 n str ^u8 n ^u8 n -- )
   {: na:^ nu:n s ha:^ hu:n ta:^ tu:n :}
   na nu >str E2E-TREE:AT-A s FS:WRITE-ALL
   s" tools/check.f" >str E2E-TREE:ARGS
   s" --" >LEN PROC-ARGV+
   na nu >LEN PROC-ARGV+
   ENGINE-CANDIDATE:PATH$ E2E-TREE:ROOT$ E2E-TREE:RUN-ON
   E2E-TREE:RC@ 70 T=
   E2E-TREE:OUT$ str> nip 0 T=
   na nu >str E2E-TREE:AT-A SOURCE-ROOT:CANONICAL drop
   str> {: pa:^ pu:n :}
   SB-RESET ha hu SB-APPEND pa pu SB-APPEND ta tu SB-APPEND
   E2E-TREE:ERR$ SB$ >str T$= ;
s" test/tdt-nameless-newtype.f" s\" NEWTYPE\n" >str
s\" error[E-BAD-DECLARATION]: bad newtype declaration\n --> "
s\" :1:1\n  |\n1 | NEWTYPE\n  | ^^^^^^^ missing name\n  = help: write a name after `NEWTYPE`, as in `NEWTYPE meters`\n\n"
TDT-CHECKED
s" test/tdt-nameless-sumtype.f" s\" SUMTYPE\n" >str
s\" error[E-BAD-DECLARATION]: bad sumtype declaration\n --> "
s\" :1:1\n  |\n1 | SUMTYPE\n  | ^^^^^^^ missing name\n  = help: write a name after `SUMTYPE`, as in `SUMTYPE shape`\n\n"
TDT-CHECKED
s" test/tdt-nameless-variant.f" s\" SUMTYPE tdshape VARIANT ;SUMTYPE\n" >str
s\" error[E-BAD-DECLARATION]: bad sumtype declaration `tdshape`\n --> "
s\" :1:17\n  |\n1 | SUMTYPE tdshape VARIANT ;SUMTYPE\n  |                 ^^^^^^^ missing name\n  = help: write a name after `VARIANT`, as in `VARIANT none ;VARIANT`\n\n"
TDT-CHECKED

\ uppercase/mixed-case family names reject before storage.
s" NEWTYPE Bad<a>"
s\" error[E-BAD-DECLARATION]: bad newtype declaration `Bad`\n" >str
s" Bad" >str s" name must be a lowercase family tail" >str TDT-AT
s\"   = help: write the name as a lowercase tail with no package qualifier, as in `point`\n" >str TDT-HAS
s" NEWTYPE tdBAD<a>"
s\" error[E-BAD-DECLARATION]: bad newtype declaration `tdBAD`\n" >str
s" tdBAD" >str s" name must be a lowercase family tail" >str TDT-AT
s" SUMTYPE Res<a> VARIANT ok a ;VARIANT ;SUMTYPE"
s\" error[E-BAD-DECLARATION]: bad sumtype declaration `Res`\n" >str
s" Res" >str s" name must be a lowercase family tail" >str TDT-AT
\ qualified names are illegal in declaration position.
s" NEWTYPE pkg:tail 1"
s\" error[E-BAD-DECLARATION]: bad newtype declaration `pkg:tail`\n" >str
s" pkg:tail" >str s" name must be a lowercase family tail" >str TDT-AT
\ hyphen edges / doubled hyphens are not canonical tails.
s" NEWTYPE td--x<a>"
s\" error[E-BAD-DECLARATION]: bad newtype declaration `td--x`\n" >str
s" td--x" >str s" name must be a lowercase family tail" >str TDT-AT
s" NEWTYPE -tdx 1"
s\" error[E-BAD-DECLARATION]: bad newtype declaration `-tdx`\n" >str
s" -tdx" >str s" name must be a lowercase family tail" >str TDT-AT
\ injection-shaped text is not a canonical name token.
s\" NEWTYPE td\" 1"
s\" error[E-BAD-DECLARATION]: bad newtype declaration `td\"`\n" >str
s\" td\"" >str s" name must be a lowercase family tail" >str TDT-AT
\ reserved signature/type tokens as family names.
s" NEWTYPE a"
s\" error[E-BAD-DECLARATION]: bad newtype declaration `a`\n" >str
s" a" >str s" reserved name" >str TDT-AT
s\"   = help: pick another name: a built-in type such as `n` cannot name a family or variant\n" >str TDT-HAS
s" SUMTYPE n VARIANT x ;VARIANT ;SUMTYPE"
s\" error[E-BAD-DECLARATION]: bad sumtype declaration `n`\n" >str
s" n" >str s" reserved name" >str TDT-AT
\ A variant defines a word, so a keyword cannot name one.
s" SUMTYPE tdkwv VARIANT export ;VARIANT ;SUMTYPE"
s\" error[E-RESERVED-DEFINITION]: `export` is a keyword and cannot name a definition\n" >str
s" export" >str s" reserved keyword" >str TDT-AT
s\"   = help: give the definition a name that is not a keyword\n" >str TDT-HAS
s" NEWTYPE field<a>"
s\" error[E-BAD-DECLARATION]: bad newtype declaration `field`\n" >str
s" field" >str s" reserved name" >str TDT-AT
s" NEWTYPE space-x<a>"
s\" error[E-BAD-DECLARATION]: bad newtype declaration `space-x`\n" >str
s" space-x" >str s" reserved name" >str TDT-AT
s" NEWTYPE fresh-mask-x<a>"
s\" error[E-BAD-DECLARATION]: bad newtype declaration `fresh-mask-x`\n" >str
s" fresh-mask-x" >str s" reserved name" >str TDT-AT
\ An effect reads the token `ptr` ahead of any family, so the tail is reserved
\ ahead of the global `ptr` family's duplicate check.
s" NEWTYPE ptr"
s\" error[E-BAD-DECLARATION]: bad newtype declaration `ptr`\n" >str
s" ptr" >str s" reserved name" >str TDT-AT
\ `scratch-only` is the storage clause, so no family takes the name.
: TDT-SCRATCH-NAME ( -- )
   REJECTION:CAPTURE
   s" NEWTYPE scratch-only" TDT-EVAL-CATCH {: rc:n :}
   s\" error[E-BAD-DECLARATION]: bad newtype declaration `scratch-only`\n" >str TDT-HAS
   s" scratch-only" >str s" reserved name" >str DECL-DIAG:CAUSE? TTRUE
   rc 70 T= ;
TDT-SCRATCH-NAME
\ Nor can a keyword of the match protocol.
s" SUMTYPE tdcn<a> VARIANT construct a ;VARIANT ;SUMTYPE"
s\" error[E-RESERVED-DEFINITION]: `construct` is a keyword and cannot name a definition\n" >str
s" construct" >str s" reserved keyword" >str TDT-AT
s" SUMTYPE tdcn2<a> VARIANT match a ;VARIANT ;SUMTYPE"
s\" error[E-RESERVED-DEFINITION]: `match` is a keyword and cannot name a definition\n" >str
s" match" >str s" reserved keyword" >str TDT-AT
s" SUMTYPE tdcn3<a> VARIANT ;match a ;VARIANT ;SUMTYPE"
s\" error[E-RESERVED-DEFINITION]: `;match` is a keyword and cannot name a definition\n" >str
s" ;match" >str s" reserved keyword" >str TDT-AT
\ redeclaring a global family at top level is a same-scope duplicate (the
\ top-level declaring scope IS the global scope, so the collision is a real
\ duplicate family, not a reserved-name shadow; both classes reject)...
s" NEWTYPE span<a,b,c>"
s\" error[E-BAD-DECLARATION]: bad newtype declaration `span`\n" >str
s" span" >str s" duplicate family" >str TDT-AT
s" NEWTYPE str<a>"
s\" error[E-BAD-DECLARATION]: bad newtype declaration `str`\n" >str
s" str" >str s" duplicate family" >str TDT-AT
\ The global/package same-tail case above is legal; only an exact same-package
\ duplicate remains a duplicate family.
\ variant names may not collide with any family the declaring scope resolves:
\ builtin tails and prior user families reject in every scope, the package's
\ own tails inside that package.
s" SUMTYPE tdvres<a> VARIANT span a ;VARIANT ;SUMTYPE"
s\" error[E-BAD-DECLARATION]: bad sumtype declaration `tdvres`\n" >str
s" span" >str s" collides with a type family" >str TDT-AT
s\"   = help: rename the variant: a type family in this scope already has this name\n" >str TDT-HAS
NEWTYPE tduserfam<a>
s" SUMTYPE tdvres2<a> VARIANT tduserfam a ;VARIANT ;SUMTYPE"
s\" error[E-BAD-DECLARATION]: bad sumtype declaration `tdvres2`\n" >str
s" tduserfam" >str s" collides with a type family" >str TDT-AT
package tvshad
s" SUMTYPE tdvres3<a> VARIANT span a ;VARIANT ;SUMTYPE"
s\" error[E-BAD-DECLARATION]: bad sumtype declaration `tdvres3`\n" >str
s" span" >str s" collides with a type family" >str TDT-AT
NEWTYPE tvloc<a>
s" SUMTYPE tdvres4<a> VARIANT tvloc a ;VARIANT ;SUMTYPE"
s\" error[E-BAD-DECLARATION]: bad sumtype declaration `tdvres4`\n" >str
s" tvloc" >str s" collides with a type family" >str TDT-AT
;package
\ family declarations must reject a prior in-scope variant tail too; otherwise
\ the collision verdict depends on declaration order.
ENUM tdvowner tdfuture ;ENUM
s" NEWTYPE tdfuture"
s\" error[E-BAD-DECLARATION]: bad newtype declaration `tdfuture`\n" >str
s" tdfuture" >str s" collides with a sum variant" >str TDT-AT
s\"   = help: rename the family: a variant declared in this scope already has this name\n" >str TDT-HAS
package tvorder
ENUM tvowner tvfuture ;ENUM
s" NEWTYPE tvfuture"
s\" error[E-BAD-DECLARATION]: bad newtype declaration `tvfuture`\n" >str
s" tvfuture" >str s" collides with a sum variant" >str TDT-AT
;package
\ A generated word that collides is placed at the family that generated it.
package tdgaddr
private
create TDGADDR-AT
s" STRUCTURE tdgaddr DERIVE addr FIELD id n ;STRUCTURE"
s\" error[E-BAD-DECLARATION]: bad structure declaration `tdgaddr`\n" >str
s" tdgaddr" >str s" generated declaration already defined" >str TDT-AT
s\" = word: TDGADDR-AT\n" >str TDT-HAS
;package
\ DECL-EVENT is a part of TYPE-DECL: no name outside TYPE-DECL reaches it.
s" DECL-EVENT:POLICY"
s\" error[E-UNDEFINED]: cannot find word `DECL-EVENT:POLICY`\n" >str s" not found" >str TDT-REJ
s" TYPE-DECL:DECL-EVENT:POLICY"
s\" error[E-UNDEFINED]: cannot find word `TYPE-DECL:DECL-EVENT:POLICY`\n" >str s" not found" >str TDT-REJ
\ Package-local variant tails do not reserve unrelated package namespaces.
package tvordera
ENUM tvowner tvlocal ;ENUM
;package
package tvorderb
NEWTYPE tvlocal
;package
\ ...and an unreserved variant name in the same shape still accepts.
SUMTYPE tdvok<a>
  VARIANT fine a ;VARIANT
;SUMTYPE
\ a declaration's grammar word is not a family name.
s" SUMTYPE variant<a> VARIANT ok a ;VARIANT ;SUMTYPE"
s\" error[E-BAD-DECLARATION]: bad sumtype declaration `variant`\n" >str
s" variant" >str s" reserved name" >str TDT-AT
\ value-record names collide.
VALUE-RECORD tdvrec x n END-VALUE-RECORD
s" NEWTYPE tdvrec<a>"
s\" error[E-BAD-DECLARATION]: bad newtype declaration `tdvrec`\n" >str
s" tdvrec" >str s" reserved name" >str TDT-AT
\ duplicate family (same package scope).
s" NEWTYPE tdfoo<a,b>"
s\" error[E-BAD-DECLARATION]: bad newtype declaration `tdfoo`\n" >str
s" tdfoo" >str s" duplicate family" >str TDT-AT
\ malformed parameter lists in family names.
s" NEWTYPE tdnoar<>"
s\" error[E-BAD-DECLARATION]: bad newtype declaration `tdnoar<>`\n" >str
s" tdnoar<>" >str s" invalid type parameter header" >str TDT-AT
s\"   = help: name each type parameter once, by a letter other than `f`, `n` or `r`, as in `box<a>` or `result<a,b>`\n" >str TDT-HAS
s" NEWTYPE tdarx<x,x>"
s\" error[E-BAD-DECLARATION]: bad newtype declaration `tdarx<x,x>`\n" >str
s" tdarx<x,x>" >str s" invalid type parameter header" >str TDT-AT
s" NEWTYPE tdarneg<-1>"
s\" error[E-BAD-DECLARATION]: bad newtype declaration `tdarneg<-1>`\n" >str
s" tdarneg<-1>" >str s" invalid type parameter header" >str TDT-AT
s" NEWTYPE tdarbig<a,>"
s\" error[E-BAD-DECLARATION]: bad newtype declaration `tdarbig<a,>`\n" >str
s" tdarbig<a,>" >str s" invalid type parameter header" >str TDT-AT
s" NEWTYPE tdarunc<a"
s\" error[E-BAD-DECLARATION]: bad newtype declaration `tdarunc<a`\n" >str
s" tdarunc<a" >str s" invalid type parameter header" >str TDT-AT
s" NEWTYPE tdartrail<a>b"
s\" error[E-BAD-DECLARATION]: bad newtype declaration `tdartrail<a>b`\n" >str
s" tdartrail<a>b" >str s" invalid type parameter header" >str TDT-AT
s" NEWTYPE tdarres<n>"
s\" error[E-BAD-DECLARATION]: bad newtype declaration `tdarres<n>`\n" >str
s" tdarres<n>" >str s" invalid type parameter header" >str TDT-AT
s" NEWTYPE tdarlong<item>"
s\" error[E-BAD-DECLARATION]: bad newtype declaration `tdarlong<item>`\n" >str
s" tdarlong<item>" >str s" invalid type parameter header" >str TDT-AT
s" SUMTYPE tdarv<> VARIANT ok a ;VARIANT ;SUMTYPE"
s\" error[E-BAD-DECLARATION]: bad sumtype declaration `tdarv<>`\n" >str
s" tdarv<>" >str s" invalid type parameter header" >str TDT-AT
\ an unterminated body (no ;SUMTYPE) reports the declaration record.
s" SUMTYPE tdnoend<a> VARIANT ok a ;VARIANT"
s\" error[E-BAD-DECLARATION]: bad sumtype declaration `tdnoend<a>`\n" >str
s" tdnoend<a>" >str s" missing ;SUMTYPE" >str TDT-AT
s\"   = help: end the declaration with `;SUMTYPE` after its last variant\n" >str TDT-HAS
\ malformed sum bodies.
s" SUMTYPE tdempty<a> ;SUMTYPE"
s\" error[E-BAD-DECLARATION]: bad sumtype declaration `tdempty`\n" >str
s" tdempty" >str s" empty sum" >str TDT-AT
s\"   = help: declare at least one variant, as in `VARIANT none ;VARIANT`\n" >str TDT-HAS
s" SUMTYPE tdnoterm<a> VARIANT ok a ;VARIANT"
s\" error[E-BAD-DECLARATION]: bad sumtype declaration `tdnoterm<a>`\n" >str
s" tdnoterm<a>" >str s" missing ;SUMTYPE" >str TDT-AT
s" SUMTYPE tdstray<a> stray VARIANT ok a ;VARIANT ;SUMTYPE"
s\" error[E-BAD-DECLARATION]: bad sumtype declaration `tdstray`\n" >str
s" stray" >str s" unexpected token in sum declaration" >str TDT-AT
s\"   = help: write each variant as `VARIANT name type ... ;VARIANT`, after any `POLICY` and `DERIVE` clause\n" >str TDT-HAS
s" SUMTYPE tdnovn<a> VARIANT ;VARIANT ;SUMTYPE"
s\" error[E-BAD-DECLARATION]: bad sumtype declaration `tdnovn`\n" >str
s" ;VARIANT" >str s" name must be a lowercase family tail" >str TDT-AT
s" SUMTYPE tdkw<a> VARIANT variant a ;VARIANT ;SUMTYPE"
s\" error[E-BAD-DECLARATION]: bad sumtype declaration `tdkw`\n" >str
s" variant" >str s" reserved name" >str TDT-AT
s" SUMTYPE tdvcase<a> VARIANT Ok a ;VARIANT ;SUMTYPE"
s\" error[E-BAD-DECLARATION]: bad sumtype declaration `tdvcase`\n" >str
s" Ok" >str s" name must be a lowercase family tail" >str TDT-AT
s" SUMTYPE tdvdup<a> VARIANT ok a ;VARIANT VARIANT ok a ;VARIANT ;SUMTYPE"
s\" error[E-BAD-DECLARATION]: bad sumtype declaration `tdvdup`\n" >str
s" ok" >str s" duplicate variant" >str TDT-AT
s" SUMTYPE tdbadptr<a> VARIANT ok ^ ;VARIANT ;SUMTYPE"
s\" error[E-BAD-DECLARATION]: bad sumtype declaration `tdbadptr`\n" >str
s" ^" >str s" pointer without a pointee" >str TDT-AT
s\"   = help: spell a pointer `^T`, `^` joined to the type it points at, as in `^u8`\n" >str TDT-HAS
\ unknown payload types: an out-of-arity letter and an unknown tail reject;
\ parametric family applications and quotations resolve as payloads, so the
\ other rejects are a wrong-arity application and an unbound argument letter.
\ tdres has arity 2.
s" SUMTYPE tdpay1<a> VARIANT ok q ;VARIANT ;SUMTYPE"
s\" error[E-BAD-DECLARATION]: bad sumtype declaration `tdpay1`\n" >str
s" q" >str s" unknown payload type" >str TDT-AT
s\"   = help: list the parameter in the family header, as in `box<a>`, before a payload names it\n" >str TDT-HAS
s" SUMTYPE tdpay2<a> VARIANT ok whatnot ;VARIANT ;SUMTYPE"
s\" error[E-BAD-DECLARATION]: bad sumtype declaration `tdpay2`\n" >str
s" whatnot" >str s" unknown payload type" >str TDT-AT
s" SUMTYPE tdpay3<a> VARIANT ok tdres<a> ;VARIANT ;SUMTYPE"
s\" error[E-BAD-DECLARATION]: bad sumtype declaration `tdpay3`\n" >str
s" tdres" >str s" type family applied to wrong number of arguments" >str TDT-AT
s\"   = help: give the family as many type arguments as its header declares\n" >str TDT-HAS
s" SUMTYPE tdpay4<a> VARIANT ok tdres<a,z> ;VARIANT ;SUMTYPE"
s\" error[E-BAD-DECLARATION]: bad sumtype declaration `tdpay4`\n" >str
s" z" >str s" unknown payload type" >str TDT-AT
s\" SUMTYPE tdpay6<a> VARIANT ok tdres<a,\n a, a> ;VARIANT ;SUMTYPE"
s\" error[E-BAD-DECLARATION]: bad sumtype declaration `tdpay6`\n" >str
s" tdres" >str s" type family applied to wrong number of arguments" >str TDT-AT
s\"   = help: give the family as many type arguments as its header declares\n" >str TDT-HAS
\ A pointer payload is spelled ^T: `ptr` is an unknown payload type.
s" SUMTYPE tdpayptr VARIANT ok ptr u8 ;VARIANT ;SUMTYPE"
s\" error[E-BAD-DECLARATION]: bad sumtype declaration `tdpayptr`\n" >str
s" ptr" >str s" unknown payload type" >str TDT-AT
s\"   = help: spell a pointer `^T`, `^` joined to the type it points at, as in `^u8`\n" >str TDT-HAS
s" SUMTYPE tdpayview<a,b> VARIANT ok mut-view<a,a,b,n> ;VARIANT ;SUMTYPE"
s\" error[E-BAD-DECLARATION]: bad sumtype declaration `tdpayview`\n" >str
s" mut-view" >str s" takes a scope or region, which a `SUMTYPE` payload cannot hold" >str TDT-AT
s\"   = help: hold it in a field of an `ENUM` whose header lists its scope and region parameters, as in `ENUM box<p,q>` with `FIELD v read-view<p,q,u8>`\n" >str TDT-HAS
\ quotation effect sides carry full rows: an empty input side, a multi-type
\ input side, an empty output side, and a multi-type input with a ptr element
\ all declare and resolve.
SUMTYPE tdpq0 VARIANT run [ -- n ] ;VARIANT VARIANT nop ;VARIANT ;SUMTYPE
SUMTYPE tdpq2 VARIANT run [ n n -- n ] ;VARIANT VARIANT nop ;VARIANT ;SUMTYPE
SUMTYPE tdpqd VARIANT run [ n -- ] ;VARIANT VARIANT nop ;VARIANT ;SUMTYPE
SUMTYPE tdpqm VARIANT run [ n ^u8 -- n ] ;VARIANT VARIANT nop ;VARIANT ;SUMTYPE
\ A malformed quotation is rejected at the token it stopped at, or at its `[`
\ when its variant ends first.
s" SUMTYPE tdpqx VARIANT run [ n n ] ;VARIANT ;SUMTYPE"
s\" error[E-BAD-DECLARATION]: bad sumtype declaration `tdpqx`\n" >str
s" ]" >str s" the quotation type needs `--` here" >str TDT-AT
s\"   = help: write the quotation type as `[ in -- out ]`\n" >str TDT-HAS
s" SUMTYPE tdpqy VARIANT run [ n -- n ;VARIANT ;SUMTYPE"
s\" error[E-BAD-DECLARATION]: bad sumtype declaration `tdpqy`\n" >str
s" [" >str s" the quotation type is not closed" >str TDT-AT
s\"   = help: close the quotation type with `]`\n" >str TDT-HAS
\ Fact words follow the last outputs, each stated once.
SUMTYPE tdpqf VARIANT run [ -- renders ] ;VARIANT VARIANT nop ;VARIANT ;SUMTYPE
s" SUMTYPE tdpqf1 VARIANT run [ renders -- ] ;VARIANT ;SUMTYPE"
s\" error[E-BAD-DECLARATION]: bad sumtype declaration `tdpqf1`\n" >str
s" renders" >str s" a fact follows only the last outputs" >str TDT-AT
s\"   = help: write the facts after the last outputs, as `[ n -- n renders ]`\n" >str TDT-HAS
s" SUMTYPE tdpqf2 VARIANT run [ -- renders renders ] ;VARIANT ;SUMTYPE"
s\" error[E-BAD-DECLARATION]: bad sumtype declaration `tdpqf2`\n" >str
s" renders" >str s" this fact is already stated" >str TDT-AT
s\"   = help: state each fact once\n" >str TDT-HAS
s" SUMTYPE tdpqf3 VARIANT run [ -- renders n ] ;VARIANT ;SUMTYPE"
s\" error[E-BAD-DECLARATION]: bad sumtype declaration `tdpqf3`\n" >str
s" n" >str s" a type follows the facts" >str TDT-AT
s\"   = help: write the facts after the last outputs, as `[ n -- n renders ]`\n" >str TDT-HAS
s" SUMTYPE tdpqf4 VARIANT run [ -- renders | -- ] ;VARIANT ;SUMTYPE"
s\" error[E-BAD-DECLARATION]: bad sumtype declaration `tdpqf4`\n" >str
s" |" >str s" a fact follows only the last outputs" >str TDT-AT
s\"   = help: write the facts after the last outputs, as `[ n -- n renders ]`\n" >str TDT-HAS

\ A quotation side of 300 elements declares, and the constructor takes a
\ quotation of 300 cells, not of 299; a variant of 300 payload elements declares.
package LONG-DECL-TEST
300 constant LONG-N
: Q-HEAD$ ( -- ^u8 n ) s" SUMTYPE tdpq300 VARIANT run [ " ;
: Q-TAIL$ ( -- ^u8 n ) s" -- ] ;VARIANT VARIANT nop ;VARIANT ;SUMTYPE" ;
: C-TAIL$ ( -- ^u8 n ) s" -- ] -- tdpq300 ) construct tdpq300 run ;" ;
: P-HEAD$ ( -- ^u8 n ) s" SUMTYPE tdpay300 VARIANT wide " ;
: P-TAIL$ ( -- ^u8 n ) s" ;VARIANT ;SUMTYPE" ;
Q-HEAD$ nip Q-TAIL$ nip +
s" : TDPQ300-NO ( [ " nip C-TAIL$ nip + max
P-HEAD$ nip P-TAIL$ nip + max
LONG-N 2 * + constant CAP
CAP BUFFER: BUF
variable BUF-N
: APPEND ( ^u8 n -- ) {: a:^ u:n :}
   a BUF BUF-N @ + u BYTE-COPY  BUF-N @ u + BUF-N ! ;
: NS ( n -- ) 0 ?do s" n " APPEND loop ;
: DECL$ ( -- ^u8 n )
   0 BUF-N !  Q-HEAD$ APPEND  LONG-N NS  Q-TAIL$ APPEND  BUF BUF-N @ ;
\ : NAME ( [ n n … -- ] -- tdpq300 ) … ; of k cells
: CTOR$ ( ^u8 n n -- ^u8 n ) {: a:^ u:n k:n :}
   0 BUF-N !  s" : " APPEND  a u APPEND  s"  ( [ " APPEND
   k NS  C-TAIL$ APPEND  BUF BUF-N @ ;
: PAY$ ( -- ^u8 n )
   0 BUF-N !  P-HEAD$ APPEND  LONG-N NS  P-TAIL$ APPEND  BUF BUF-N @ ;
: SHORT-CTOR ( -- )
   REJECTION:CAPTURE  true REJECTION:JSON !
   s" TDPQ300-NO" LONG-N 1 - CTOR$ TDT-EVAL-CATCH 70 T=
   REJECTION:BUFFER$ s\" \"code\":\"E-MISMATCH\"" >str CONTAINS? TTRUE
   false REJECTION:JSON !  REJECTION:CAPTURE ;
public
: RUN ( -- )
   DECL$ TDT-EVAL-CATCH 0 T=
   s" TDPQ300-OK" LONG-N CTOR$ TDT-EVAL-CATCH 0 T=
   SHORT-CTOR
   PAY$ TDT-EVAL-CATCH 0 T= ;
;package
LONG-DECL-TEST:RUN

\ declaration-time parameter-arity gate for SUM variant payloads. A payload
\ parameter letter beyond the family's arity rejects at declaration, not at a
\ later construct/MATCH site, in a plain element, an application argument, and
\ every quotation effect side (input, output, return-input, return-output). The
\ parse front line (TDECL-LETTER) and the whole-tree walk (TFAM-SCH-ARITY) both
\ reject it as an unknown payload type; the plain and application cases are
\ tdpay1/tdpay4 above.
s" SUMTYPE tdpaq1<a> VARIANT ok [ z -- ] ;VARIANT ;SUMTYPE"
s\" error[E-BAD-DECLARATION]: bad sumtype declaration `tdpaq1`\n" >str
s" z" >str s" unknown payload type" >str TDT-AT
s" SUMTYPE tdpaq2<a> VARIANT ok [ -- z ] ;VARIANT ;SUMTYPE"
s\" error[E-BAD-DECLARATION]: bad sumtype declaration `tdpaq2`\n" >str
s" z" >str s" unknown payload type" >str TDT-AT
s" SUMTYPE tdpaq3<a> VARIANT ok [ n -- | z -- ] ;VARIANT ;SUMTYPE"
s\" error[E-BAD-DECLARATION]: bad sumtype declaration `tdpaq3`\n" >str
s" z" >str s" unknown payload type" >str TDT-AT
s" SUMTYPE tdpaq4<a> VARIANT ok [ n -- | -- z ] ;VARIANT ;SUMTYPE"
s\" error[E-BAD-DECLARATION]: bad sumtype declaration `tdpaq4`\n" >str
s" z" >str s" unknown payload type" >str TDT-AT
\ an out-of-arity parameter nested inside an application argument that itself sits
\ on a quotation side still rejects at declaration (whole-subtree descent).
s" SUMTYPE tdpaq5<a> VARIANT ok [ tdres<a,z> -- ] ;VARIANT ;SUMTYPE"
s\" error[E-BAD-DECLARATION]: bad sumtype declaration `tdpaq5`\n" >str
s" z" >str s" unknown payload type" >str TDT-AT

\ no over-tightening: in-range parameter letters in a plain element, a parametric
\ application argument, and both quotation sides all declare; a nested-layout
\ argument (a layout family in a cell-parameter slot, sound via LOGHID width
\ coercion) declares too -- the gate resolves parameter indices only.
SUMTYPE tdpga<a,b>
  VARIANT plain   a ;VARIANT
  VARIANT applied tdres<a,b> ;VARIANT
  VARIANT quoted  [ a -- b ] ;VARIANT
;SUMTYPE
\ and the in-range variants still construct through the untouched construct path.
s" TDPGA-MK1 ( n -- tdpga<n,n> ) TDPGA:PLAIN"           >str CHECK-QUIET-CANDIDATE! -1 T=
s" TDPGA-MK2 ( tdres<n,n> -- tdpga<n,n> ) TDPGA:APPLIED" >str CHECK-QUIET-CANDIDATE! -1 T=
SUMTYPE tdpgn
  VARIANT nested tdopt<tdres<n,f>> ;VARIANT
  VARIANT nop ;VARIANT
;SUMTYPE

\ malformed enum declarations: each is rejected with its reason.
\ empty / unterminated bodies.
s" ENUM tdeempty ;ENUM"
s\" error[E-BAD-DECLARATION]: bad enum declaration `tdeempty`\n" >str
s" tdeempty" >str s" empty enum" >str TDT-AT
s" ENUM tdenoterm red green"
s\" error[E-BAD-DECLARATION]: bad enum declaration `tdenoterm`\n" >str
s" tdenoterm" >str s" missing ;ENUM" >str TDT-AT
\ duplicate variant within one enum.
s" ENUM tdedup red red ;ENUM"
s\" error[E-BAD-DECLARATION]: bad enum declaration `tdedup`\n" >str
s" red" >str s" duplicate variant" >str TDT-AT
\ bad variant names: uppercase, reserved single-letter, grammar keyword, and a
\ name that collides with an existing type family.
s" ENUM tdecase Red ;ENUM"
s\" error[E-BAD-DECLARATION]: bad enum declaration `tdecase`\n" >str
s" Red" >str s" name must be a lowercase tail" >str TDT-AT
s" ENUM tdesl a ;ENUM"
s\" error[E-BAD-DECLARATION]: bad enum declaration `tdesl`\n" >str
s" a" >str s" name is reserved or already taken" >str TDT-AT
s" ENUM tdekw enum ;ENUM"
s\" error[E-BAD-DECLARATION]: bad enum declaration `tdekw`\n" >str
s" enum" >str s" name is reserved or already taken" >str TDT-AT
\ `variant` in a compact body is malformed, not a reserved name: the ENUM
\ front end has both modes, and a block keyword where a compact variant belongs
\ confuses the two forms, reported at the token ("block keyword in a compact
\ enum"). test/enum-decl-suite.f pins the same reading.
s" ENUM tdekw2 red variant ;ENUM"
s\" error[E-BAD-DECLARATION]: bad enum declaration `tdekw2`\n" >str
s" variant" >str s" block keyword in a compact enum" >str TDT-AT
s" ENUM tdevf tdfoo ;ENUM"
s\" error[E-BAD-DECLARATION]: bad enum declaration `tdevf`\n" >str
s" tdfoo" >str s" name is reserved or already taken" >str TDT-AT
\ bad family names: uppercase, reserved single-letter, grammar keyword.
s" ENUM Bad red ;ENUM"
s\" error[E-BAD-DECLARATION]: bad enum declaration `Bad`\n" >str
s" Bad" >str s" name must be a lowercase family tail" >str TDT-AT
s" ENUM a red ;ENUM"
s\" error[E-BAD-DECLARATION]: bad enum declaration `a`\n" >str
s" a" >str s" reserved name" >str TDT-AT
s" ENUM sumtype red ;ENUM"
s\" error[E-BAD-DECLARATION]: bad enum declaration `sumtype`\n" >str
s" sumtype" >str s" reserved name" >str TDT-AT
\ redeclaring an existing family tail (enum over enum, and enum over a sum).
s" ENUM tdcolor red ;ENUM"
s\" error[E-BAD-DECLARATION]: bad enum declaration `tdcolor`\n" >str
s" tdcolor" >str s" duplicate family" >str TDT-AT
s" ENUM tdres red ;ENUM"
s\" error[E-BAD-DECLARATION]: bad enum declaration `tdres`\n" >str
s" tdres" >str s" duplicate family" >str TDT-AT

\ malformed STRUCTURE declarations: each is rejected with its reason.
\ a dangling ptr.
s" STRUCTURE tdpptr<a> FIELD x ^ ;STRUCTURE"
s\" error[E-BAD-DECLARATION]: bad structure declaration `tdpptr`\n" >str
s" ^" >str s" pointer without a pointee" >str TDT-AT
s\"   = help: spell a pointer `^T`, `^` joined to the type it points at, as in `FIELD next ^node`\n" >str TDT-HAS
\ a bad field name: uppercase.
s" STRUCTURE tdpfc<a> FIELD X a ;STRUCTURE"
s\" error[E-BAD-DECLARATION]: bad structure declaration `tdpfc`\n" >str
s" X" >str s" name must be a lowercase tail" >str TDT-AT
s\"   = help: write the name as a lowercase tail with no package qualifier, as in `point`\n" >str TDT-HAS
\ a control word cannot name a variant.
s" SUMTYPE tdscw VARIANT loop ;VARIANT ;SUMTYPE"
s\" error[E-RESERVED-DEFINITION]: `loop` is a keyword and cannot name a definition\n" >str
s" loop" >str s" reserved keyword" >str TDT-AT
\ an empty parameter list.
s" STRUCTURE tdpna<> FIELD x a ;STRUCTURE"
s\" error[E-BAD-DECLARATION]: bad structure declaration `tdpna<>`\n" >str
s" tdpna<>" >str s" invalid type parameter header" >str TDT-AT
\ a bad family name: another declaration's grammar keyword.
s" STRUCTURE sumtype FIELD x n ;STRUCTURE"
s\" error[E-BAD-DECLARATION]: bad structure declaration `sumtype`\n" >str
s" sumtype" >str s" reserved name" >str TDT-AT
\ field is reserved as a variant name too (case-folded dictionary).
s" SUMTYPE tdpv2<a> VARIANT field a ;VARIANT ;SUMTYPE"
s\" error[E-BAD-DECLARATION]: bad sumtype declaration `tdpv2`\n" >str
s" field" >str s" reserved name" >str TDT-AT

\ `POLICY <name>` header clause (docs/design.md "Families and layout policies").

\ explicit stack-cell-tag accepts on sum, enum and structure headers.
package tpol
SUMTYPE tdpol<a> POLICY stack-cell-tag
  VARIANT none   ;VARIANT
  VARIANT some a ;VARIANT
;SUMTYPE
ENUM tdpolen POLICY stack-cell-tag red green blue ;ENUM
STRUCTURE tdpolpr POLICY stack-cell-tag FIELD x n FIELD y n ;STRUCTURE
\ packed-tag accepts on every header.
SUMTYPE tdpolpk<a> POLICY packed-tag
  VARIANT none   ;VARIANT
  VARIANT some a ;VARIANT
;SUMTYPE
ENUM tdpolpke POLICY packed-tag red green ;ENUM
STRUCTURE tdpolpkr POLICY packed-tag FIELD x n ;STRUCTURE
;package
\ Re-arm the diagnostic capture: it only silences stderr, so each section
\ starts an empty one.
REJECTION:CAPTURE
\ boxed rejects on every header until it is lowered; niche-null rejects every
\ shape but its one, a structure included.
s" SUMTYPE tdpolns2<a> POLICY niche-null VARIANT some a ;VARIANT ;SUMTYPE"
s\" error[E-BAD-DECLARATION]: bad sumtype declaration `tdpolns2`\n" >str
s" tdpolns2" >str s" niche-null needs an empty variant, then one pointer field" >str TDT-AT
s\"   = help: drop `POLICY niche-null`, or declare an empty variant, then one holding a single `^` payload\n" >str TDT-HAS
s" SUMTYPE tdpolns3<a> POLICY boxed VARIANT some a ;VARIANT ;SUMTYPE"
s\" error[E-BAD-DECLARATION]: bad sumtype declaration `tdpolns3`\n" >str
s" boxed" >str s" layout policy not yet supported" >str TDT-AT
s\"   = help: replace `boxed` with `stack-cell-tag`, `packed-tag` or `niche-null`, or drop the `POLICY` clause\n" >str TDT-HAS
s" ENUM tdpolns4 POLICY boxed red green ;ENUM"
s\" error[E-BAD-DECLARATION]: bad enum declaration `tdpolns4`\n" >str
s" boxed" >str s" unknown layout policy" >str TDT-AT
s" STRUCTURE tdpolns5 POLICY niche-null FIELD x n ;STRUCTURE"
s\" error[E-BAD-DECLARATION]: bad structure declaration `tdpolns5`\n" >str
s" niche-null" >str s" niche-null needs an empty variant, then one pointer field" >str TDT-AT
\ niche-null accepts its one shape, an empty variant then one `^` field; an
\ ENUM of two empty variants is rejected.
s" SUMTYPE tdpolnq1<a> POLICY niche-null VARIANT none ;VARIANT VARIANT some ^u8 ;VARIANT ;SUMTYPE" 0 TDT-NEG
s" ENUM tdpolnq2 POLICY niche-null red green ;ENUM"
s\" error[E-BAD-DECLARATION]: bad enum declaration `tdpolnq2`\n" >str
s" tdpolnq2" >str s" niche-null needs an empty variant, then one pointer field" >str TDT-AT
\ unknown policy names reject (incl. the v1 non-goal `custom`).
s" SUMTYPE tdpolun1<a> POLICY bogus VARIANT some a ;VARIANT ;SUMTYPE"
s\" error[E-BAD-DECLARATION]: bad sumtype declaration `tdpolun1`\n" >str
s" bogus" >str s" unknown layout policy" >str TDT-AT
s\"   = help: name `stack-cell-tag`, `packed-tag` or `niche-null` after `POLICY`\n" >str TDT-HAS
s" SUMTYPE tdpolun2<a> POLICY custom VARIANT some a ;VARIANT ;SUMTYPE"
s\" error[E-BAD-DECLARATION]: bad sumtype declaration `tdpolun2`\n" >str
s" custom" >str s" unknown layout policy" >str TDT-AT
\ POLICY keyword with no following name.
s" SUMTYPE tdpolmiss<a> POLICY ;SUMTYPE"
s\" error[E-BAD-DECLARATION]: bad sumtype declaration `tdpolmiss`\n" >str
s" tdpolmiss" >str s" missing layout policy name" >str TDT-AT
s\"   = help: name a policy after `POLICY`: `stack-cell-tag`, `packed-tag` or `niche-null`\n" >str TDT-HAS
\ POLICY is reserved: it may not name a family or a variant.
s" SUMTYPE policy<a> VARIANT some a ;VARIANT ;SUMTYPE"
s\" error[E-BAD-DECLARATION]: bad sumtype declaration `policy`\n" >str
s" policy" >str s" reserved name" >str TDT-AT
s" SUMTYPE tdpolrv<a> VARIANT policy a ;VARIANT ;SUMTYPE"
s\" error[E-BAD-DECLARATION]: bad sumtype declaration `tdpolrv`\n" >str
s" policy" >str s" reserved name" >str TDT-AT
s" NEWTYPE policy<a>"
s\" error[E-BAD-DECLARATION]: bad newtype declaration `policy`\n" >str
s" policy" >str s" reserved name" >str TDT-AT

\ A self-family payload makes the family recursive and rejects with
\ the recursive reason, not an unknown payload, in ptr, inline and bare forms.
s" SUMTYPE tdrec1<a> VARIANT leaf a ;VARIANT VARIANT node ^tdrec1<a> ;VARIANT ;SUMTYPE"
s\" error[E-BAD-DECLARATION]: bad sumtype declaration `tdrec1`\n" >str
s" tdrec1" >str s" invalid layout policy for recursive sum" >str TDT-AT
s" SUMTYPE tdrec2<a> VARIANT node tdrec2<a> ;VARIANT ;SUMTYPE"
s\" error[E-BAD-DECLARATION]: bad sumtype declaration `tdrec2`\n" >str
s" tdrec2" >str s" invalid layout policy for recursive sum" >str TDT-AT
s" SUMTYPE tdrec3 VARIANT self tdrec3 ;VARIANT ;SUMTYPE"
s\" error[E-BAD-DECLARATION]: bad sumtype declaration `tdrec3`\n" >str
s" tdrec3" >str s" invalid layout policy for recursive sum" >str TDT-AT
\ the recursive reject is INDEPENDENT of the stated policy: a direct self-ref
\ rejects the same under an explicit stack-cell-tag AND under packed-tag (packed
\ keeps the cell width, so the width cycle is just as unrepresentable), on sum
\ variants only: a STRUCTURE field naming its own family is a field type that
\ cannot recursively name its owner. boxed is the only policy that COULD lay out
\ recursion, and it is still reject-until-supported, so a recursive boxed sum
\ rejects earlier — at the POLICY clause, before the self-ref.
s" SUMTYPE tdrecpk1 POLICY packed-tag VARIANT self tdrecpk1 ;VARIANT ;SUMTYPE"
s\" error[E-BAD-DECLARATION]: bad sumtype declaration `tdrecpk1`\n" >str
s" tdrecpk1" >str s" invalid layout policy for recursive sum" >str TDT-AT
s" SUMTYPE tdrecpk2<a> POLICY packed-tag VARIANT node ^tdrecpk2<a> ;VARIANT ;SUMTYPE"
s\" error[E-BAD-DECLARATION]: bad sumtype declaration `tdrecpk2`\n" >str
s" tdrecpk2" >str s" invalid layout policy for recursive sum" >str TDT-AT
s" SUMTYPE tdrecsc1 POLICY stack-cell-tag VARIANT self tdrecsc1 ;VARIANT ;SUMTYPE"
s\" error[E-BAD-DECLARATION]: bad sumtype declaration `tdrecsc1`\n" >str
s" tdrecsc1" >str s" invalid layout policy for recursive sum" >str TDT-AT
s" SUMTYPE tdrecbx1<a> POLICY boxed VARIANT node tdrecbx1<a> ;VARIANT ;SUMTYPE"
s\" error[E-BAD-DECLARATION]: bad sumtype declaration `tdrecbx1`\n" >str
s" boxed" >str s" layout policy not yet supported" >str TDT-AT
\ mutual recursion is fail-closed: a forward reference to a not-yet-declared
\ family is an unknown payload (no cycle can be expressed, no row leaks).
s" SUMTYPE tdmuta VARIANT mk tdmutb ;VARIANT ;SUMTYPE"
s\" error[E-BAD-DECLARATION]: bad sumtype declaration `tdmuta`\n" >str
s" tdmutb" >str s" unknown payload type" >str TDT-AT
\ a NON-self family payload is unchanged: still the generic unknown-payload reject.
s" SUMTYPE tdrec5<a> VARIANT node tdother<a> ;VARIANT ;SUMTYPE"
s\" error[E-BAD-DECLARATION]: bad sumtype declaration `tdrec5`\n" >str
s" tdother" >str s" unknown payload type" >str TDT-AT
\ non-recursive payloads are unaffected: a ptr-to-concrete payload still accepts.
package tdrp
SUMTYPE tdrec6<a>
  VARIANT node ^u8 ;VARIANT
;SUMTYPE
;package

\ payload family references resolve in signature scope: qualified PKG:tail
\ payloads resolve like a signature reference (public cross-package, private
\ in-package), and an arity-0 cell family (NEWTYPE x) is as admissible as a
\ layout family. A private family in another package stays unreachable; a
\ qualified self-reference rejects recursive by resolved family identity.
package tdqp
public
NEWTYPE qslot
SUMTYPE qsum
  VARIANT qa n ;VARIANT
;SUMTYPE
private
NEWTYPE qpriv
;package

\ cross-package qualified CELL-family payload.
SUMTYPE tdqw
  VARIANT qsome tdqp:qslot ;VARIANT
  VARIANT qnone ;VARIANT
;SUMTYPE
\ constructor + MATCH round-trip. Checked: the constructor input IS the
\ qualified nominal scalar (an n is not), and the qsome arm refines back to it.
s" TDQ-ID ( tdqp:qslot -- tdqw ) TDQW:QSOME" >str CHECK-QUIET-CANDIDATE! -1 T=
s" TDQ-BADN ( n -- tdqw ) TDQW:QSOME" >str CHECK-QUIET-CANDIDATE! 0 T=
\ runtime: construct with the generated ctor, eliminate with MATCH, value back.
\ An n enters tdqp:qslot only through a cast the declaring package owns.
package tdqp
public
CAST: TQX>QS ( n -- qslot )
;package
CAST: TQX<QS ( tdqp:qslot -- n )
: TDQ-MK ( n -- tdqw ) tdqp:TQX>QS TDQW:QSOME ;
: TDQ-GET ( tdqw -- n )
   MATCH tdqw
      qsome OF TQX<QS ENDOF
      qnone OF 0 ENDOF
   ;MATCH ;
: TDQ-RT ( n -- n ) TDQ-MK TDQ-GET ;
42 TDQ-RT 42 T=
\ cross-package qualified LAYOUT-family payload (uppercase qualifier folds):
\ paycells = the referenced family's full width (tag included).
SUMTYPE tdqv
  VARIANT qwrap TDQP:qsum ;VARIANT
;SUMTYPE
\ in-package: a package's OWN private cell family is a payload by bare tail,
\ and a STRUCTURE FIELD takes the qualified public cell family.
package tdqc
NEWTYPE cslot
SUMTYPE cwrap
  VARIANT cv cslot ;VARIANT
;SUMTYPE
STRUCTURE cprod
  FIELD child tdqp:qslot
;STRUCTURE
;package
\ negatives: each record names its reason.
\ a bogus qualifier never resolves.
s" SUMTYPE tdqb1 VARIANT bv nopkg:qslot ;VARIANT ;SUMTYPE"
s\" error[E-BAD-DECLARATION]: bad sumtype declaration `tdqb1`\n" >str
s" nopkg:qslot" >str s" unknown payload type" >str TDT-AT
\ a PRIVATE family in another package stays unreachable, qualified or bare.
s" SUMTYPE tdqb2 VARIANT bv tdqp:qpriv ;VARIANT ;SUMTYPE"
s\" error[E-BAD-DECLARATION]: bad sumtype declaration `tdqb2`\n" >str
s" tdqp:qpriv" >str s" unknown payload type" >str TDT-AT
s" SUMTYPE tdqb3 VARIANT bv qpriv ;VARIANT ;SUMTYPE"
s\" error[E-BAD-DECLARATION]: bad sumtype declaration `tdqb3`\n" >str
s" qpriv" >str s" unknown payload type" >str TDT-AT
\ malformed multi-colon tokens and non-canonical tails never split/resolve.
s" SUMTYPE tdqb4 VARIANT bv tdqp:qslot:x ;VARIANT ;SUMTYPE"
s\" error[E-BAD-DECLARATION]: bad sumtype declaration `tdqb4`\n" >str
s" tdqp:qslot:x" >str s" unknown payload type" >str TDT-AT
s" SUMTYPE tdqb5 VARIANT bv tdqp:QSLOT ;VARIANT ;SUMTYPE"
s\" error[E-BAD-DECLARATION]: bad sumtype declaration `tdqb5`\n" >str
s" tdqp:QSLOT" >str s" unknown payload type" >str TDT-AT
\ a QUALIFIED self-reference is the same recursion as the bare spelling:
\ private, public, ptr-wrapped and STRUCTURE field forms all reject it.
package tdqs
s" SUMTYPE qself VARIANT wv tdqs:qself ;VARIANT ;SUMTYPE"
s\" error[E-BAD-DECLARATION]: bad sumtype declaration `qself`\n" >str
s" tdqs:qself" >str s" invalid layout policy for recursive sum" >str TDT-AT
s" SUMTYPE qselfr VARIANT wv ^tdqs:qselfr ;VARIANT ;SUMTYPE"
s\" error[E-BAD-DECLARATION]: bad sumtype declaration `qselfr`\n" >str
s" tdqs:qselfr" >str s" invalid layout policy for recursive sum" >str TDT-AT
s" SUMTYPE qselfa<a> VARIANT wv tdqs:qselfa<a> ;VARIANT ;SUMTYPE"
s\" error[E-BAD-DECLARATION]: bad sumtype declaration `qselfa`\n" >str
s" tdqs:qselfa" >str s" invalid layout policy for recursive sum" >str TDT-AT
s" STRUCTURE qsprod FIELD child tdqs:qsprod ;STRUCTURE"
s\" error[E-BAD-DECLARATION]: bad structure declaration `qsprod`\n" >str
s" tdqs:qsprod" >str s" type cannot recursively name its owner" >str TDT-AT
public
s" SUMTYPE qselfp VARIANT wv tdqs:qselfp ;VARIANT ;SUMTYPE"
s\" error[E-BAD-DECLARATION]: bad sumtype declaration `qselfp`\n" >str
s" tdqs:qselfp" >str s" invalid layout policy for recursive sum" >str TDT-AT
;package

\ declaration rejection record: declaration-shaped, through the standard
\ machinery — no fake declared stack effect, definition fields, or word row.
REJECTION:BUFFER-OFF
REJECTION:CAPTURE  true REJECTION:JSON !
s" NEWTYPE tdfoo<a,b>" 70 TDT-NEG
REJECTION:BUFFER$ s\" \"code\":\"E-BAD-DECLARATION\"" >str CONTAINS? TTRUE
REJECTION:BUFFER$ s\" \"message\":\"bad newtype declaration `tdfoo`\"" >str CONTAINS? TTRUE
REJECTION:BUFFER$ s\" \"token\":\"tdfoo\"" >str CONTAINS? TTRUE
REJECTION:BUFFER$ s\" \"label\":\"duplicate family\"" >str CONTAINS? TTRUE
REJECTION:BUFFER$ s\" \"help\":\"rename the family: this package already declares a family with this name\"" >str CONTAINS? TTRUE
REJECTION:BUFFER$ s\" \"declared_effect\"" >str CONTAINS? TFALSE
REJECTION:BUFFER$ s\" \"definition_source\"" >str CONTAINS? TFALSE
REJECTION:BUFFER$ s\" \"return_stack\"" >str CONTAINS? TFALSE
REJECTION:BUFFER-OFF  false REJECTION:JSON !
\ prose form names the declaration kind and reason.
s" SUMTYPE tdvd2<a> VARIANT ok a ;VARIANT VARIANT ok a ;VARIANT ;SUMTYPE"
s\" error[E-BAD-DECLARATION]: bad sumtype declaration `tdvd2`\n" >str
s" ok" >str s" duplicate variant" >str TDT-AT
\ a rejection names the source token, not storage the declaration's rollback
\ scrubs.
s" ENUM tdcol2 DERIVE eq red tag blue ;ENUM"
s\" error[E-BAD-DECLARATION]: bad enum declaration `tdcol2`\n" >str
s" tag" >str s" variant name collides with a derived word" >str TDT-AT
\ the enum kind token flows into the same declaration-shaped prose record.
s" ENUM tdediag red red ;ENUM"
s\" error[E-BAD-DECLARATION]: bad enum declaration `tdediag`\n" >str
s" red" >str s" duplicate variant" >str TDT-AT
\ A policy rejection flows into the same declaration-shaped prose record.
s" SUMTYPE tdpoldg<a> POLICY boxed VARIANT some a ;VARIANT ;SUMTYPE"
s\" error[E-BAD-DECLARATION]: bad sumtype declaration `tdpoldg`\n" >str
s" boxed" >str s" layout policy not yet supported" >str TDT-AT
\ A private SUMTYPE's generated word whose name is taken is placed at the
\ family and named as its word.
package tdpq private
: TDHUE-RED ( -- n ) 1 ;
s" SUMTYPE tdhue VARIANT red ;VARIANT VARIANT blue ;VARIANT ;SUMTYPE"
s\" error[E-BAD-DECLARATION]: bad sumtype declaration `tdhue`\n" >str
s" tdhue" >str s" generated declaration already defined" >str TDT-AT
s\" = word: TDHUE-RED\n" >str TDT-HAS
;package

\ No byte count caps a declaration's body: the fixture's SUMTYPE loads, and its
\ constructor and derived EQ and HASH run.
require test/fixtures/big-decl.f
: BIGSUM-HASH ( -- bool )
   5 BIG-BIGSUM:V7 BIG-BIGSUM:HASH 5 BIG-BIGSUM:V7 BIG-BIGSUM:HASH = ;
: BIGSUM-EQ ( -- bool ) 5 BIG-BIGSUM:V7 5 BIG-BIGSUM:V7 BIG-BIGSUM:EQ ;
: BIGSUM-NEQ ( -- bool ) 5 BIG-BIGSUM:V7 5 BIG-BIGSUM:V199 BIG-BIGSUM:EQ ;
BIGSUM-HASH TTRUE
BIGSUM-EQ TTRUE
BIGSUM-NEQ TFALSE

\ multi-error load mode: a bad top-level declaration is reported + counted +
\ rolled back, without a fake declared signature, and the load continues.
TWX-MULTI-ERR-BEGIN
s" SUMTYPE tdme<a,b> VARIANT ok a ;VARIANT VARIANT ok b ;VARIANT ;SUMTYPE NEWTYPE tdcont<a> : TDMEW ( n -- n ) ;" evaluate
TWX-MULTI-ERR-END 1 T=
s" TDME-GONE ( tdme<n,n> -- tdme<n,n> )" >str CHECK-QUIET-CANDIDATE! 0 T=
s" TDCONT-OK ( tdcont<n> -- tdcont<n> )" >str CHECK-QUIET-CANDIDATE! -1 T=
s" TDMEW-OK ( n -- n ) TDMEW" >str CHECK-QUIET-CANDIDATE! -1 T=
\ missing terminator in multi-error mode: reported, counted, load continues.
TWX-MULTI-ERR-BEGIN
s" SUMTYPE tdnoe<a> VARIANT ok a ;VARIANT" evaluate
TWX-MULTI-ERR-END 1 T=
s" TDNOE-GONE ( tdnoe<n> -- tdnoe<n> )" >str CHECK-QUIET-CANDIDATE! 0 T=
\ The same contract for the two unified front ends, which read the input stream
\ as they parse: the tokens between the fault and the terminator belong to the
\ rejected declaration, so the front end consumes them before the interpreter
\ sees them. Each case rejects mid-body; a reject at or after the terminator
\ has nothing to skip.
TWX-MULTI-ERR-BEGIN
s" ENUM tdmee red red blue ;ENUM NEWTYPE tdmeecont<a> : TDMEEW ( n -- n ) ;" evaluate
TWX-MULTI-ERR-END 1 T=
s" TDMEE-GONE ( tdmee -- tdmee )" >str CHECK-QUIET-CANDIDATE! 0 T=
s" TDMEECONT-OK ( tdmeecont<n> -- tdmeecont<n> )" >str CHECK-QUIET-CANDIDATE! -1 T=
s" TDMEEW-OK ( n -- n ) TDMEEW" >str CHECK-QUIET-CANDIDATE! -1 T=
TWX-MULTI-ERR-BEGIN
s" STRUCTURE tdmes FIELD x n FIELD x n ;STRUCTURE NEWTYPE tdmescont<a> : TDMESW ( n -- n ) ;" evaluate
TWX-MULTI-ERR-END 1 T=
s" TDMES-GONE ( tdmes -- tdmes )" >str CHECK-QUIET-CANDIDATE! 0 T=
s" TDMESCONT-OK ( tdmescont<n> -- tdmescont<n> )" >str CHECK-QUIET-CANDIDATE! -1 T=
s" TDMESW-OK ( n -- n ) TDMESW" >str CHECK-QUIET-CANDIDATE! -1 T=
\ A reject raised AFTER the terminator was consumed must not skip anything: the
\ next tokens are the following declaration, not this one's tail. `tdmea` is an
\ enum with no variants, which ED-CLOSE refuses once `;ENUM` is already read.
TWX-MULTI-ERR-BEGIN
s" ENUM tdmea ;ENUM NEWTYPE tdmeacont<a>" evaluate
TWX-MULTI-ERR-END 1 T=
s" TDMEACONT-OK ( tdmeacont<n> -- tdmeacont<n> )" >str CHECK-QUIET-CANDIDATE! -1 T=
\ Nor must one raised at the terminator itself, read where a field's type
\ stands: both definitions after it are rejected too.
TWX-MULTI-ERR-BEGIN
s" STRUCTURE tdmez FIELD x ;STRUCTURE : TDMEZW ( -- n ) ; : TDMEZW2 ( -- n ) ;" evaluate
TWX-MULTI-ERR-END 3 T=
\ A declaration whose name is its own terminator is rejected at the name: the
\ load skips to the terminator that ends it and rejects both definitions after.
TWX-MULTI-ERR-BEGIN
s" STRUCTURE ;STRUCTURE FIELD x n ;STRUCTURE : TDMETW ( -- n ) ; : TDMETW2 ( -- n ) ;" evaluate
TWX-MULTI-ERR-END 3 T=
TWX-MULTI-ERR-BEGIN
s" ENUM ;ENUM red ;ENUM : TDMETE ( -- n ) ; : TDMETE2 ( -- n ) ;" evaluate
TWX-MULTI-ERR-END 3 T=
\ A unified declaration whose input simply ends: nothing to skip, still counted.
TWX-MULTI-ERR-BEGIN
s" ENUM tdmenoe red green" evaluate
TWX-MULTI-ERR-END 1 T=
s" TDMENOE-GONE ( tdmenoe -- tdmenoe )" >str CHECK-QUIET-CANDIDATE! 0 T=
\ two bad declarations count separately.
TWX-MULTI-ERR-BEGIN
s" NEWTYPE Bad1<a> NEWTYPE tdok9<a> SUMTYPE tdes<a> ;SUMTYPE" evaluate
TWX-MULTI-ERR-END 2 T=
\ a bad declaration does not poison later checks after the mode ends.
s" TDOK-AFTER ( tdfoo<n,n> -- tdfoo<n,n> )" >str CHECK-QUIET-CANDIDATE! -1 T=
\ unknown-family and wrong-arity SIGNATURES in multi-error mode: reported,
\ counted, and the load continues — but the invalid declared signature must
\ NOT be stored as a cert row (later checks stay sound).
TWX-MULTI-ERR-BEGIN
s" : TDSME1 ( nope<n> -- nope<n> ) ; : TDSME2 ( tdfoo<n> -- tdfoo<n> ) ; : TDSME3 ( n -- n ) ;" evaluate
TWX-MULTI-ERR-END 2 T=
\ A stored row resolves the call (-1, or 0 on a mismatch); with none it is undefined (0).
s" TDSME1-USE ( n -- n ) TDSME1" >str CHECK-QUIET-CANDIDATE! 0 T=
s" TDSME2-USE ( n -- n ) TDSME2" >str CHECK-QUIET-CANDIDATE! 0 T=
s" TDSME3-USE ( n -- n ) TDSME3" >str CHECK-QUIET-CANDIDATE! -1 T=
REJECTION:BUFFER-OFF

\ derive: `DERIVE eq` on a PUBLIC ENUM generates PKG:TAG (discriminant) and
\ PKG:EQ (tag equality) as ordinary checked words in the ctor package: no
\ window, no engine lowering.
REJECTION:CAPTURE   \ silence this section's expected declaration rejects
ENUM tdrv DERIVE eq red green blue ;ENUM
: TDRV-EQ-SAME ( -- bool ) TDRV:RED TDRV:RED TDRV:EQ ;
: TDRV-EQ-DIFF ( -- bool ) TDRV:RED TDRV:GREEN TDRV:EQ ;
: TDRV-EQ-LAST ( -- bool ) TDRV:BLUE TDRV:BLUE TDRV:EQ ;
: TDRV-TAG0 ( -- n ) TDRV:RED TDRV:TAG ;
: TDRV-TAG2 ( -- n ) TDRV:BLUE TDRV:TAG ;
TDRV-EQ-SAME TTRUE
TDRV-EQ-DIFF TFALSE
TDRV-EQ-LAST TTRUE
TDRV-TAG0 0 T=
TDRV-TAG2 2 T=

\ the scalar =/0= wall on layout values is UNTOUCHED by derive (TD12-ZEQ kin).
s" TDRV-RAWEQ ( tdrv tdrv -- bool ) =" >str CHECK-QUIET-CANDIDATE! 0 T=
s" TDRV-RAWZ ( tdrv -- bool ) 0=" >str CHECK-QUIET-CANDIDATE! 0 T=

\ the derived word is family-exact: the wrong family rejects.
ENUM tdrw one two ;ENUM
s" TDRW-XEQ ( tdrw tdrw -- bool ) TDRV:EQ" >str CHECK-QUIET-CANDIDATE! 0 T=
\ a non-derived enum grows no eq surface (undefined word: refused).
s" TDRW-NOEQ ( tdrw tdrw -- bool ) TDRW:EQ" >str CHECK-QUIET-CANDIDATE! 0 T=
\ the derived words are undefine-protected like constructors; these pins mirror
\ the type-ctor-suite seal pins (undefine + reopen).
s" undefine TDRV:EQ"
   s" error[E-CTOR-PROTECTED]: `TDRV:EQ`" >str s" generated constructor" >str TDT-REJ
s" undefine TDRV:TAG"
   s" error[E-CTOR-PROTECTED]: `TDRV:TAG`" >str s" generated constructor" >str TDT-REJ
s" package tdrv"
   s" error[E-CTOR-PROTECTED]: `tdrv` bears the name of the constructor package of the family `tdrv`," >str
   s" constructor package" >str TDT-REJ

\ grammar negatives: deferred/unknown features, kind gates, name collision,
\ missing feature token (DERIVE eats exactly one feature token).
\ `DERIVE hash` is accepted, pinned positively below.
s" ENUM tdrb2 DERIVE order aa bb ;ENUM"
s\" error[E-BAD-DECLARATION]: bad enum declaration `tdrb2`\n" >str
s" order" >str s" derive feature not yet supported" >str TDT-AT
s" ENUM tdrb3 DERIVE frobnicate aa bb ;ENUM"
s\" error[E-BAD-DECLARATION]: bad enum declaration `tdrb3`\n" >str
s" frobnicate" >str s" unknown derive feature" >str TDT-AT
s" ENUM tdrb4 DERIVE ;ENUM"
s\" error[E-BAD-DECLARATION]: bad enum declaration `tdrb4`\n" >str
s" ;ENUM" >str s" unknown derive feature" >str TDT-AT
\ a feature token right after the clause reads as a feature (a repeated one
\ rejects); a variant spelled like a derived word follows another variant and
\ collides.
s" ENUM tdrb7 DERIVE eq other eq ;ENUM"
s\" error[E-BAD-DECLARATION]: bad enum declaration `tdrb7`\n" >str
s" eq" >str s" variant name collides with a derived word" >str TDT-AT

\ rollback: a REJECTED derive declaration leaves no residue — the same family
\ name registers cleanly afterwards and its derived words work.
s" ENUM tdrb9 DERIVE eq bad eq ;ENUM"
s\" error[E-BAD-DECLARATION]: bad enum declaration `tdrb9`\n" >str
s" eq" >str s" variant name collides with a derived word" >str TDT-AT
ENUM tdrb9 DERIVE eq aa bb ;ENUM
: TDRB9-EQ ( -- bool ) TDRB9:AA TDRB9:BB TDRB9:EQ ;
TDRB9-EQ TFALSE

\ a variant named eq/tag stays legal WITHOUT derive (no reservation creep).
ENUM tdrx eq neq ;ENUM
s" TDRX-USE ( -- tdrx ) TDRX:EQ" >str CHECK-QUIET-CANDIDATE! -1 T=

\ derive: payload sums compare diagonally (outer MATCH binds one value's
\ payloads, inner MATCH the other's; same-variant arms compare scalars with
\ `=`, cross arms are false); products UNMAKE both values and compare
\ field-wise, family-typed fields through their family's EQ.
SUMTYPE tdsv DERIVE eq
  VARIANT nil ;VARIANT
  VARIANT sing n ;VARIANT
  VARIANT pair n i64 ;VARIANT
;SUMTYPE
: TDSV-EQ-NIL ( -- bool ) TDSV:NIL TDSV:NIL TDSV:EQ ;
: TDSV-EQ-S ( -- bool ) 5 TDSV:SING 5 TDSV:SING TDSV:EQ ;
: TDSV-NEQ-S ( -- bool ) 5 TDSV:SING 6 TDSV:SING TDSV:EQ ;
: TDSV-EQ-P ( -- bool ) 1 2 TDSV:PAIR 1 2 TDSV:PAIR TDSV:EQ ;
: TDSV-NEQ-P1 ( -- bool ) 1 2 TDSV:PAIR 1 3 TDSV:PAIR TDSV:EQ ;
: TDSV-NEQ-P0 ( -- bool ) 1 2 TDSV:PAIR 9 2 TDSV:PAIR TDSV:EQ ;
: TDSV-NEQ-X ( -- bool ) TDSV:NIL 5 TDSV:SING TDSV:EQ ;
: TDSV-NEQ-Y ( -- bool ) 1 2 TDSV:PAIR TDSV:NIL TDSV:EQ ;
: TDSV-TAG-P ( -- n ) 1 2 TDSV:PAIR TDSV:TAG ;
TDSV-EQ-NIL TTRUE
TDSV-EQ-S TTRUE
TDSV-NEQ-S TFALSE
TDSV-EQ-P TTRUE
TDSV-NEQ-P1 TFALSE
TDSV-NEQ-P0 TFALSE
TDSV-NEQ-X TFALSE
TDSV-NEQ-Y TFALSE
TDSV-TAG-P 2 T=
\ the scalar = wall stays closed on a derived W>1 sum.
s" TDSV-RAW ( tdsv tdsv -- bool ) =" >str CHECK-QUIET-CANDIDATE! 0 T=

\ product field-wise eq; the col field compares through TDRV:EQ.
STRUCTURE tdpv DERIVE eq
  FIELD col tdrv
  FIELD amt n
;STRUCTURE
: TDPV-EQ ( -- bool ) TDRV:RED 7 TDPV:MAKE TDRV:RED 7 TDPV:MAKE TDPV:EQ ;
: TDPV-NEQ-C ( -- bool ) TDRV:RED 7 TDPV:MAKE TDRV:GREEN 7 TDPV:MAKE TDPV:EQ ;
: TDPV-NEQ-N ( -- bool ) TDRV:RED 7 TDPV:MAKE TDRV:RED 8 TDPV:MAKE TDPV:EQ ;
TDPV-EQ TTRUE
TDPV-NEQ-C TFALSE
TDPV-NEQ-N TFALSE
\ products derive EQ only: no TAG surface (undefined -> refused), and the
\ derived EQ is undefine-protected like a constructor.
s" TDPV-TAG ( tdpv -- n ) TDPV:TAG" >str CHECK-QUIET-CANDIDATE! 0 T=
s" undefine TDPV:EQ"
   s" error[E-CTOR-PROTECTED]: `TDPV:EQ`" >str s" generated constructor" >str TDT-REJ

\ payload-role gates: pointer, parametric, linear, non-derived enum field,
\ and the sum-variant name collision.
s" SUMTYPE tdsb1 DERIVE eq VARIANT hold ^u8 ;VARIANT ;SUMTYPE"
s\" error[E-BAD-DECLARATION]: bad sumtype declaration `tdsb1`\n" >str
s" ^u8" >str s" pointer payloads have no derived equality" >str TDT-AT
s\"   = help: drop `eq` and `hash` from `DERIVE`, or give the payload a type that is not a pointer\n" >str TDT-HAS
s" SUMTYPE tdsb2<a> DERIVE eq VARIANT hold a ;VARIANT ;SUMTYPE"
s\" error[E-BAD-DECLARATION]: bad sumtype declaration `tdsb2`\n" >str
s" eq" >str s" derive requires a concrete (arity 0) family" >str TDT-AT
s\"   = help: drop `eq` and `hash` from `DERIVE`, or declare the family with no type parameters\n" >str TDT-HAS
s" SUMTYPE tdsb3 DERIVE eq VARIANT hold tdown ;VARIANT ;SUMTYPE"
s\" error[E-BAD-DECLARATION]: bad sumtype declaration `tdsb3`\n" >str
s" tdown" >str s" payload type has no derived equality" >str TDT-AT
s\"   = help: give the payload an integer type, or drop `eq` and `hash` from `DERIVE`\n" >str TDT-HAS
s" STRUCTURE tdsb4 DERIVE eq FIELD cc tdrw ;STRUCTURE"
s\" error[E-BAD-DECLARATION]: bad structure declaration `tdsb4`\n" >str
s" tdrw" >str s" field family must also derive eq" >str TDT-AT
s\"   = help: add `DERIVE eq` to the field's family, or drop `eq` from this `DERIVE`\n" >str TDT-HAS
\ an OPAQUE family's derived words are private to its package.
package tdsbo
public
STRUCTURE inner OPAQUE DERIVE eq hash FIELD u n ;STRUCTURE
;package
s" STRUCTURE tdsb7 DERIVE eq FIELD w tdsbo:inner ;STRUCTURE"
s\" error[E-BAD-DECLARATION]: bad structure declaration `tdsb7`\n" >str
s" tdsbo:inner" >str s" field family's derived words are private to its package" >str TDT-AT
s\"   = help: declare the field's family in a public section of its package, or drop `eq` and `hash` from `DERIVE`\n" >str TDT-HAS
s" SUMTYPE tdsb5 DERIVE eq VARIANT eq n ;VARIANT ;SUMTYPE"
s\" error[E-BAD-DECLARATION]: bad sumtype declaration `tdsb5`\n" >str
s" eq" >str s" variant name collides with a derived word" >str TDT-AT
s\"   = help: rename the variant: under `DERIVE` the names `eq`, `hash` and `tag` are taken\n" >str TDT-HAS

\ rollback: a rejected derive leaves no residue; the name redeclares clean.
s" SUMTYPE tdsb6 DERIVE eq VARIANT hold ^u8 ;VARIANT ;SUMTYPE"
s\" error[E-BAD-DECLARATION]: bad sumtype declaration `tdsb6`\n" >str
s" ^u8" >str s" pointer payloads have no derived equality" >str TDT-AT
SUMTYPE tdsb6 DERIVE eq VARIANT hold n ;VARIANT ;SUMTYPE
: TDSB6-EQ ( -- bool ) 4 TDSB6:HOLD 4 TDSB6:HOLD TDSB6:EQ ;
TDSB6-EQ TTRUE

\ derive hash: the checked SEMANTIC generator (FNV-1a folded over the
\ variant tag + bound payload scalars per branch; products fold fields,
\ family-typed fields through their family's HASH). INVARIANT: equal values hash equal
\ BY CONSTRUCTION (hash folds exactly what eq compares); the differs cases
\ are non-degeneracy smoke only. `DERIVE eq hash` and `DERIVE hash eq` are
\ an order-free feature list; hash-alone derives HASH (+TAG) without EQ.
SUMTYPE tdhs DERIVE eq hash
  VARIANT hnil ;VARIANT
  VARIANT hone n ;VARIANT
  VARIANT hpair n i64 ;VARIANT
;SUMTYPE
\ eq true implies hash equal, across every eq-suite case shape.
: TDHS-C1 ( -- bool ) TDHS:HNIL TDHS:HNIL TDHS:EQ ;
: TDHS-H1 ( -- bool ) TDHS:HNIL TDHS:HASH TDHS:HNIL TDHS:HASH = ;
: TDHS-C2 ( -- bool ) 5 TDHS:HONE 5 TDHS:HONE TDHS:EQ ;
: TDHS-H2 ( -- bool ) 5 TDHS:HONE TDHS:HASH 5 TDHS:HONE TDHS:HASH = ;
: TDHS-C3 ( -- bool ) 1 2 TDHS:HPAIR 1 2 TDHS:HPAIR TDHS:EQ ;
: TDHS-H3 ( -- bool ) 1 2 TDHS:HPAIR TDHS:HASH 1 2 TDHS:HPAIR TDHS:HASH = ;
TDHS-C1 TTRUE
TDHS-H1 TTRUE
TDHS-C2 TTRUE
TDHS-H2 TTRUE
TDHS-C3 TTRUE
TDHS-H3 TTRUE
\ non-degeneracy smoke: tag difference, payload difference, payload ORDER.
: TDHS-D1 ( -- bool ) TDHS:HNIL TDHS:HASH 5 TDHS:HONE TDHS:HASH = ;
: TDHS-D2 ( -- bool ) 5 TDHS:HONE TDHS:HASH 6 TDHS:HONE TDHS:HASH = ;
: TDHS-D3 ( -- bool ) 1 2 TDHS:HPAIR TDHS:HASH 2 1 TDHS:HPAIR TDHS:HASH = ;
TDHS-D1 TFALSE
TDHS-D2 TFALSE
TDHS-D3 TFALSE

\ product hash: order-free `DERIVE hash eq`; the col field folds TDHE:HASH.
ENUM tdhe DERIVE eq hash red green ;ENUM
STRUCTURE tdhp DERIVE hash eq
  FIELD col tdhe
  FIELD amt n
;STRUCTURE
: TDHP-C ( -- bool ) TDHE:RED 7 TDHP:MAKE TDHE:RED 7 TDHP:MAKE TDHP:EQ ;
: TDHP-H ( -- bool ) TDHE:RED 7 TDHP:MAKE TDHP:HASH TDHE:RED 7 TDHP:MAKE TDHP:HASH = ;
: TDHP-D1 ( -- bool ) TDHE:RED 7 TDHP:MAKE TDHP:HASH TDHE:GREEN 7 TDHP:MAKE TDHP:HASH = ;
: TDHP-D2 ( -- bool ) TDHE:RED 7 TDHP:MAKE TDHP:HASH TDHE:RED 8 TDHP:MAKE TDHP:HASH = ;
TDHP-C TTRUE
TDHP-H TTRUE
TDHP-D1 TFALSE
TDHP-D2 TFALSE

\ A STRUCTURE of 256 one-cell fields derives eq and hash; its EQ binds a local
\ per field of each value.
package WIDE-DERIVE-TEST

$2000 constant BUF-CAP
create BUF BUF-CAP allot
variable BUF-N

: C+ ( n -- )
   BUF-N @ BUF-CAP >= IF s" wide derive test source overflow" EXIT-STATUS:FAILURE die THEN
   BUF BUF-N @ + c!  BUF-N @ 1 + BUF-N ! ;
: APPEND ( ^u8 n -- ) {: a:^ u:n :}
   u 0 ?do  a i + c@ C+  loop ;
: NUM+ ( n -- ) {: v:n :}   \ three decimal digits
   v 100 / 48 + C+  v 10 / 10 mod 48 + C+  v 10 mod 48 + C+ ;
: DECL$ ( -- ^u8 n )
   0 BUF-N !  s" STRUCTURE tdeq256 DERIVE eq hash" APPEND
   256 0 ?do  s"  FIELD f" APPEND  i NUM+  s"  n" APPEND  loop
   s"  ;STRUCTURE" APPEND  BUF BUF-N @ ;
: VALUE+ ( n -- ) {: odd:n :}   \ field i holds i, field odd holds 999
   256 0 ?do
      i odd = IF 999 ELSE i THEN NUM+  32 C+
   loop  s" TDEQ256:MAKE " APPEND ;
\ A word comparing the value with every field i to one whose field odd is 999.
: EQ$ ( ^u8 n n -- str ) {: a:^ u:n odd:n :}
   0 BUF-N !  s" : " APPEND  a u APPEND  s"  ( -- bool ) " APPEND
   -1 VALUE+  odd VALUE+  s" TDEQ256:EQ ; " APPEND  a u APPEND
   BUF BUF-N @ >str ;

public

: RUN ( -- )
   DECL$ TDT-EVAL-CATCH 0 T=
   s" TDEQ256-SAME" -1 EQ$ TEST-EVAL:FLAG TTRUE
   s" TDEQ256-DIFF" 200 EQ$ TEST-EVAL:FLAG TFALSE ;

;package

WIDE-DERIVE-TEST:RUN

\ hash-alone: HASH + TAG generated, EQ absent (undefined -> refused);
\ the derived HASH is undefine-protected like a constructor.
ENUM tdho DERIVE hash oa ob ;ENUM
: TDHO-H ( -- bool ) TDHO:OA TDHO:HASH TDHO:OA TDHO:HASH = ;
: TDHO-T ( -- n ) TDHO:OB TDHO:TAG ;
TDHO-H TTRUE
TDHO-T 1 T=
s" TDHO-E ( tdho tdho -- bool ) TDHO:EQ" >str CHECK-QUIET-CANDIDATE! 0 T=
s" undefine TDHO:HASH"
   s" error[E-CTOR-PROTECTED]: `TDHO:HASH`" >str s" generated constructor" >str TDT-REJ
\ eq-only families grow no hash surface.
s" TDSV-NOH ( tdsv -- n ) TDSV:HASH" >str CHECK-QUIET-CANDIDATE! 0 T=

\ A derived word's engine words are the ones sumtype.f found: a declaring
\ package's own `=`, `and`, `0=`, `drop`, `swap`, `xor` and `*` capture none.
package tdbw
public
variable HITS
private
: = ( n n -- bool ) <> ;
: 0= ( n -- bool ) 0<> ;
: and ( bool bool -- bool ) or ;
: xor ( n n -- n ) + ;
: * ( n n -- n ) + ;
: swap ( a b -- b a ) {: x y :} 1 HITS +! y x ;
: drop ( a -- ) {: x :} 1 HITS +! ;
public
ENUM tdbe DERIVE eq hash red green ;ENUM
SUMTYPE tdbs DERIVE eq hash
  VARIANT bnil ;VARIANT
  VARIANT bpair n n ;VARIANT
;SUMTYPE
STRUCTURE tdbp DERIVE eq hash
  FIELD col tdbe
  FIELD amt n
;STRUCTURE
;package
package tdbc
public
ENUM tdbe DERIVE eq hash red green ;ENUM
SUMTYPE tdbs DERIVE eq hash
  VARIANT bnil ;VARIANT
  VARIANT bpair n n ;VARIANT
;SUMTYPE
STRUCTURE tdbp DERIVE eq hash
  FIELD col tdbe
  FIELD amt n
;STRUCTURE
;package
: TDBW-E1 ( -- bool ) TDBW-TDBE:RED TDBW-TDBE:RED TDBW-TDBE:EQ ;
: TDBW-E2 ( -- bool ) TDBW-TDBE:RED TDBW-TDBE:GREEN TDBW-TDBE:EQ ;
: TDBW-ET ( -- n ) TDBW-TDBE:GREEN TDBW-TDBE:TAG ;
: TDBW-EH ( -- bool )
   TDBW-TDBE:GREEN TDBW-TDBE:HASH TDBC-TDBE:GREEN TDBC-TDBE:HASH = ;
TDBW-E1 TTRUE
TDBW-E2 TFALSE
TDBW-ET 1 T=
TDBW-EH TTRUE
: TDBW-S1 ( -- bool ) 1 2 TDBW-TDBS:BPAIR 1 2 TDBW-TDBS:BPAIR TDBW-TDBS:EQ ;
: TDBW-S2 ( -- bool ) 1 2 TDBW-TDBS:BPAIR 1 3 TDBW-TDBS:BPAIR TDBW-TDBS:EQ ;
: TDBW-S3 ( -- bool ) TDBW-TDBS:BNIL 1 2 TDBW-TDBS:BPAIR TDBW-TDBS:EQ ;
: TDBW-S0 ( -- bool ) TDBW-TDBS:BNIL TDBW-TDBS:BNIL TDBW-TDBS:EQ ;
: TDBW-ST ( -- n ) 1 2 TDBW-TDBS:BPAIR TDBW-TDBS:TAG ;
: TDBW-SH ( -- bool )
   1 2 TDBW-TDBS:BPAIR TDBW-TDBS:HASH 1 2 TDBC-TDBS:BPAIR TDBC-TDBS:HASH = ;
TDBW-S0 TTRUE
TDBW-S1 TTRUE
TDBW-S2 TFALSE
TDBW-S3 TFALSE
TDBW-ST 1 T=
TDBW-SH TTRUE
: TDBW-P1 ( -- bool )
   TDBW-TDBE:RED 7 TDBW-TDBP:MAKE TDBW-TDBE:RED 7 TDBW-TDBP:MAKE TDBW-TDBP:EQ ;
: TDBW-P2 ( -- bool )
   TDBW-TDBE:RED 7 TDBW-TDBP:MAKE TDBW-TDBE:GREEN 7 TDBW-TDBP:MAKE TDBW-TDBP:EQ ;
: TDBW-P3 ( -- bool )
   TDBW-TDBE:RED 7 TDBW-TDBP:MAKE TDBW-TDBE:RED 8 TDBW-TDBP:MAKE TDBW-TDBP:EQ ;
: TDBW-PH ( -- bool )
   TDBW-TDBE:GREEN 7 TDBW-TDBP:MAKE TDBW-TDBP:HASH
   TDBC-TDBE:GREEN 7 TDBC-TDBP:MAKE TDBC-TDBP:HASH = ;
TDBW-P1 TTRUE
TDBW-P2 TFALSE
TDBW-P3 TFALSE
TDBW-PH TTRUE
TDBW:HITS @ 0 T=

\ A field family's derived words are called by their exact public names: a
\ part named as the field's constructor package captures neither EQ nor HASH.
package tdbx
public
ENUM tdxe DERIVE eq hash red green ;ENUM
;package
package tdby
private
package TDBX-TDXE
public
: EQ ( tdbx:tdxe tdbx:tdxe -- bool ) drop drop true ;
: HASH ( tdbx:tdxe -- n ) drop 7 ;
;package
public
STRUCTURE tdyp DERIVE eq hash
  FIELD col tdbx:tdxe
  FIELD amt n
;STRUCTURE
SUMTYPE tdys DERIVE eq hash
  VARIANT ywrap tdbx:tdxe ;VARIANT
;SUMTYPE
;package
: TDBY-P ( tdbx:tdxe -- tdby:tdyp ) 7 TDBY-TDYP:MAKE ;
: TDBY-PE ( -- bool )
   TDBX-TDXE:RED TDBY-P TDBX-TDXE:GREEN TDBY-P TDBY-TDYP:EQ ;
: TDBY-PH ( -- bool )
   TDBX-TDXE:RED TDBY-P TDBY-TDYP:HASH
   TDBX-TDXE:GREEN TDBY-P TDBY-TDYP:HASH = ;
: TDBY-SE ( -- bool )
   TDBX-TDXE:RED TDBY-TDYS:YWRAP TDBX-TDXE:GREEN TDBY-TDYS:YWRAP
   TDBY-TDYS:EQ ;
: TDBY-SH ( -- bool )
   TDBX-TDXE:RED TDBY-TDYS:YWRAP TDBY-TDYS:HASH
   TDBX-TDXE:GREEN TDBY-TDYS:YWRAP TDBY-TDYS:HASH = ;
TDBY-PE TFALSE
TDBY-PH TFALSE
TDBY-SE TFALSE
TDBY-SH TFALSE

\ grammar: order stays deferred inside the list; unknown first token rejects;
\ a sum variant named hash collides; hash-only ptr/field gates still fire.
s" ENUM tdhb1 DERIVE eq order aa bb ;ENUM"
s\" error[E-BAD-DECLARATION]: bad enum declaration `tdhb1`\n" >str
s" order" >str s" derive feature not yet supported" >str TDT-AT
s" SUMTYPE tdhb2 DERIVE hash VARIANT hash n ;VARIANT ;SUMTYPE"
s\" error[E-BAD-DECLARATION]: bad sumtype declaration `tdhb2`\n" >str
s" hash" >str s" variant name collides with a derived word" >str TDT-AT
s" SUMTYPE tdhb3 DERIVE hash VARIANT hold ^u8 ;VARIANT ;SUMTYPE"
s\" error[E-BAD-DECLARATION]: bad sumtype declaration `tdhb3`\n" >str
s" ^u8" >str s" pointer payloads have no derived equality" >str TDT-AT
s" STRUCTURE tdhb4 DERIVE hash FIELD cc tdrw ;STRUCTURE"
s\" error[E-BAD-DECLARATION]: bad structure declaration `tdhb4`\n" >str
s" tdrw" >str s" field family must also derive hash" >str TDT-AT
s\"   = help: add `DERIVE hash` to the field's family, or drop `hash` from this `DERIVE`\n" >str TDT-HAS

\ A family-typed field or payload compares through its family's EQ and hashes
\ through its HASH: a product of payload sums tells 1 2 from 1 3.
SUMTYPE tdfs DERIVE eq hash
  VARIANT fnil ;VARIANT
  VARIANT fpair n n ;VARIANT
;SUMTYPE
STRUCTURE tdfp DERIVE eq hash FIELD s tdfs ;STRUCTURE
: TDFP-12 ( -- tdfp ) 1 2 TDFS:FPAIR TDFP:MAKE ;
: TDFP-EQ ( -- bool ) TDFP-12 TDFP-12 TDFP:EQ ;
: TDFP-NEQ ( -- bool ) TDFP-12 1 3 TDFS:FPAIR TDFP:MAKE TDFP:EQ ;
: TDFP-H ( -- bool ) TDFP-12 TDFP:HASH TDFP-12 TDFP:HASH = ;
TDFP-EQ TTRUE
TDFP-NEQ TFALSE
TDFP-H TTRUE
\ an enum payload of a sum, and a product field of a product.
ENUM tdfe DERIVE eq red green ;ENUM
SUMTYPE tdfw DERIVE eq VARIANT fhold tdfe ;VARIANT VARIANT fnone ;VARIANT ;SUMTYPE
: TDFW-EQ ( -- bool ) TDFE:RED TDFW:FHOLD TDFE:RED TDFW:FHOLD TDFW:EQ ;
: TDFW-NEQ ( -- bool ) TDFE:RED TDFW:FHOLD TDFE:GREEN TDFW:FHOLD TDFW:EQ ;
TDFW-EQ TTRUE
TDFW-NEQ TFALSE
STRUCTURE tdfi DERIVE eq FIELD u n ;STRUCTURE
STRUCTURE tdfo DERIVE eq FIELD w tdfi ;STRUCTURE
: TDFO-EQ ( -- bool ) 4 TDFI:MAKE TDFO:MAKE 4 TDFI:MAKE TDFO:MAKE TDFO:EQ ;
: TDFO-NEQ ( -- bool ) 4 TDFI:MAKE TDFO:MAKE 5 TDFI:MAKE TDFO:MAKE TDFO:EQ ;
TDFO-EQ TTRUE
TDFO-NEQ TFALSE
\ the field's family derives what the outer does, or the declaration is refused.
REJECTION:CAPTURE
s" STRUCTURE tdfb DERIVE hash FIELD c tdrv ;STRUCTURE"
s\" error[E-BAD-DECLARATION]: bad structure declaration `tdfb`\n" >str
s" tdrv" >str s" field family must also derive hash" >str TDT-AT
REJECTION:BUFFER$ s" field family must also derive hash" >str CONTAINS? TTRUE

\ A full-mode ENUM builds its derived words at commit, once its FIELD rows are
\ committed, so the declaration below is the whole setup.
ENUM tdnu DERIVE eq hash
  VARIANT empty ;VARIANT
  VARIANT pair FIELD first n FIELD second n ;VARIANT
;ENUM
: TDNU-EQ-EMPTY ( -- bool ) construct tdnu empty construct tdnu empty TDNU:EQ ;
: TDNU-EQ-PAIR ( -- bool ) 1 2 construct tdnu pair 1 2 construct tdnu pair TDNU:EQ ;
: TDNU-NEQ-PAY ( -- bool ) 1 2 construct tdnu pair 1 3 construct tdnu pair TDNU:EQ ;
: TDNU-NEQ-ORDER ( -- bool ) 1 2 construct tdnu pair 2 1 construct tdnu pair TDNU:EQ ;
: TDNU-NEQ-CROSS ( -- bool ) construct tdnu empty 1 2 construct tdnu pair TDNU:EQ ;
: TDNU-TAG-PAIR ( -- n ) 1 2 construct tdnu pair TDNU:TAG ;
: TDNU-HASH-EQ ( -- bool )
   1 2 construct tdnu pair TDNU:HASH
   1 2 construct tdnu pair TDNU:HASH = ;
: TDNU-HASH-ORDER ( -- bool )
   1 2 construct tdnu pair TDNU:HASH
   2 1 construct tdnu pair TDNU:HASH = ;
TDNU-EQ-EMPTY TTRUE
TDNU-EQ-PAIR TTRUE
TDNU-NEQ-PAY TFALSE
TDNU-NEQ-ORDER TFALSE
TDNU-NEQ-CROSS TFALSE
TDNU-TAG-PAIR 1 T=
TDNU-HASH-EQ TTRUE
TDNU-HASH-ORDER TFALSE

\ A pointer field rejects at its type, as a SUMTYPE pointer payload does.
s" ENUM tdnu-bad DERIVE eq VARIANT hold FIELD raw ^u8 ;VARIANT ;ENUM"
s\" error[E-BAD-DECLARATION]: bad enum declaration `tdnu-bad`\n" >str
s" ^u8" >str s" pointer payloads have no derived equality" >str TDT-AT

\ typed locals for family types: a bare arity-0 family tail is a
\ legal {: x:fam :} annotation. Enum-tier layouts (W=1 sum/enum) assert the
\ one-cell hidden term — the :} bind unifies the captured bundle's tag term
\ against it, reads restore the exact bound term (family id intact, MATCH
\ works), and wrong families reject with the standard mismatch record.
\ Arity-0 CELL families assert their nominal scalar like a signature.
\ Parametric and wider non-linear locals are covered below; owning linear
\ values remain excluded from named locals.
ENUM tdlv DERIVE eq lva lvb ;ENUM
: TDLV-ID ( tdlv -- tdlv ) {: x:tdlv :} x ;
: TDLV-RT ( -- n ) TDLV:LVB TDLV-ID TDLV:TAG ;
TDLV-RT 1 T=
: TDLV-M ( tdlv -- n ) {: x:tdlv :} x match tdlv lva of 10 endof lvb of 20 endof ;match ;
: TDLV-MA ( -- n ) TDLV:LVA TDLV-M ;
: TDLV-MB ( -- n ) TDLV:LVB TDLV-M ;
TDLV-MA 10 T=
TDLV-MB 20 T=
\ two annotated locals + derived eq through the reads.
: TDLV-EQ2 ( -- bool ) TDLV:LVA TDLV:LVA {: x:tdlv y:tdlv :} x y TDLV:EQ ;
TDLV-EQ2 TTRUE
\ annotated local referenced in both branch arms.
: TDLV-BR ( n tdlv -- n ) {: v:tdlv :} 0 > if v TDLV:TAG else v TDLV:TAG 10 + then ;
: TDLV-BRT ( -- n ) 1 TDLV:LVB TDLV-BR ;
: TDLV-BRF ( -- n ) 0 TDLV:LVB TDLV-BR ;
TDLV-BRT 1 T=
TDLV-BRF 11 T=
\ single-field product (W=1 layout tier) rides the same path.
STRUCTURE tdlp FIELD amt n ;STRUCTURE
: TDLP-ID ( tdlp -- tdlp ) {: x:tdlp :} x ;
: TDLP-RT ( -- n ) 7 TDLP:MAKE TDLP-ID TDLP:UNMAKE ;
TDLP-RT 7 T=
\ negatives: wrong family, scalar-vs-family both ways, arity>0 cell tail — all
\ reject fail-closed.
s" TDLB1 ( tdrw -- tdrw ) {: x:tdrv :} x" >str CHECK-QUIET-CANDIDATE! 0 T=
s" TDLB2 ( n -- n ) {: x:tdrv :} 5" >str CHECK-QUIET-CANDIDATE! 0 T=
s" TDLB3 ( tdrv -- tdrv ) {: x:n :} x" >str CHECK-QUIET-CANDIDATE! 0 T=
\ an annotation is read by the SIGNATURE grammar, so a parametric spelling is
\ an annotation: tdres<n,n> is the W=2 instance the declared row carries.
s" TDLB4 ( tdres<n,n> -- n ) {: x:tdres<n,n> :} 5" >str CHECK-QUIET-CANDIDATE! -1 T=
s" TDLB4B ( tdres<n,n> -- tdres<n,n> ) {: x:tdres<n,n> :} x" >str CHECK-QUIET-CANDIDATE! -1 T=
\ the same grammar means the same arity check: a bare arity>0 tail and a wrong
\ argument count are both refused, in an annotation as in a signature.
s" TDLB6 ( n -- n ) {: x:tdfoo :} x" >str CHECK-QUIET-CANDIDATE! 0 T=
s" TDLB6B ( tdres<n,n> -- n ) {: x:tdres :} 5" >str CHECK-QUIET-CANDIDATE! 0 T=
s" TDLB6C ( tdres<n,n> -- n ) {: x:tdres<n> :} 5" >str CHECK-QUIET-CANDIDATE! 0 T=
s" TDLB6D ( tdres<n,n> -- n ) {: x:tdres<n,n,n> :} 5" >str CHECK-QUIET-CANDIDATE! 0 T=
\ the definition's own type variables are in scope in an annotation, and the
\ declared quantifier survives the local (the parametricity seal, NP-CHECK).
\ tdfoo is one cell; a local over tdres<a,b>, whose width is open, is refused
\ (test/checker-soundness-suite.f OPEN-WIDTH-LOCALS).
s" TDLB6E ( tdfoo<a,b> -- tdfoo<a,b> ) {: x:tdfoo<a,b> :} x" >str CHECK-QUIET-CANDIDATE! -1 T=
\ a nested family argument parses exactly as it does in a signature.
s" TDLB6F ( tdres<tdfoo<n,n>,n> -- tdres<tdfoo<n,n>,n> ) {: x:tdres<tdfoo<n,n>,n> :} x" >str CHECK-QUIET-CANDIDATE! -1 T=
\ and a wrong argument is still a wrong type: tdres<n,n> is not tdres<tdfoo<n,n>,n>.
s" TDLB6G ( tdres<n,n> -- n ) {: x:tdres<tdfoo<n,n>,n> :} 5" >str CHECK-QUIET-CANDIDATE! 0 T=
\ a parametric CELL family is one cell and stays its nominal scalar, so the
\ annotation unifies with the local's var exactly as the arity-0 nominal does.
s" TDLB6H ( tdfoo<n,n> -- tdfoo<n,n> ) {: x:tdfoo<n,n> :} x" >str CHECK-QUIET-CANDIDATE! -1 T=
s" TDLB6I ( n -- tdfoo<n,n> ) {: x:tdfoo<n,n> :} x" >str CHECK-QUIET-CANDIDATE! 0 T=
\ an arity-0 layout of ANY width is nameable: tdsv is
\ W=3, its annotation is the bundle's top hidden term and the bind records the
\ full width, so the local holds the whole value.
s" TDLB5 ( tdsv -- tdsv ) {: x:tdsv :} x" >str CHECK-QUIET-CANDIDATE! -1 T=
\ bare W>1 bundle locals keep working.
s" TDLB7 ( tdsv -- tdsv ) {: x :} x" >str CHECK-QUIET-CANDIDATE! -1 T=
\ arity-0 cell family annotation = the signature nominal.
NEWTYPE tdlnom
s" TDLB8 ( tdlnom -- tdlnom ) {: x:tdlnom :} x" >str CHECK-QUIET-CANDIDATE! -1 T=
s" TDLB9 ( n -- tdlnom ) {: x:tdlnom :} x" >str CHECK-QUIET-CANDIDATE! 0 T=

\ E-MISMATCH family-name render: a locals-annotation family term's stored name
\ span points into the shared TKF token-fold buffer, which later body tokens
\ overwrite before DIAG-PRINT runs; the renderer reads the interned registry
\ name from the term's family id, so a locals-sourced term pins the same row as
\ a signature-declared one, and a foreign-package family renders the qualified
\ pkg:tail spelling.

\ A failure INSIDE an annotation is pinned to the annotation token, not to the
\ `:}` that would fail next: the annotation parser forces the verdict where it
\ reads, and the signature's own arity diagnostic is what comes out.
REJECTION:CAPTURE
s" TDLRA1 ( tdres<n,n> -- n ) {: x:tdres<n> :} 5" >str CHECK-CANDIDATE! 0 T=
REJECTION:BUFFER$ s\"   = help: give `tdres` 2 type arguments\n" >str CONTAINS? TTRUE
REJECTION:CAPTURE
s" TDLR1 ( tdlnom -- n ) {: q:tdlnom :} q dup drop" >str CHECK-CANDIDATE! 0 T=
REJECTION:BUFFER$ s\" error[E-MISMATCH]: TDLR1 ends with tdlnom on the stack; its signature promises n\n" >str CONTAINS? TTRUE
REJECTION:BUFFER$ s\"   = expected: n\n  = found: tdlnom\n" >str CONTAINS? TTRUE
REJECTION:CAPTURE
s" TDLR2 ( tdlnom -- n ) dup drop" >str CHECK-CANDIDATE! 0 T=
REJECTION:BUFFER$ s\" error[E-MISMATCH]: TDLR2 ends with tdlnom on the stack; its signature promises n\n" >str CONTAINS? TTRUE
REJECTION:BUFFER$ s\"   = expected: n\n  = found: tdlnom\n" >str CONTAINS? TTRUE
\ enum-tier annotation term on the EXPECTED side of the :} bind check.
REJECTION:CAPTURE
s" TDLR3 ( tdlp -- n ) {: q:tdlv :} q dup drop" >str CHECK-CANDIDATE! 0 T=
REJECTION:BUFFER$ s\" error[E-MISMATCH]: `:}` takes tdlv; the stack holds tdlp\n" >str CONTAINS? TTRUE
REJECTION:BUFFER$ s\"   = expected: tdlv\n  = found: tdlp\n" >str CONTAINS? TTRUE
\ foreign-package family: the interned name renders qualified pkg:tail. Use the
\ real package path; checker mirror mutation is not namespace authority.
package tdlrpk
public
NEWTYPE tdlrfam
;package
REJECTION:CAPTURE
s" TDLR4 ( tdlrpk:tdlrfam -- n ) {: q:tdlrpk:tdlrfam :} q dup drop" >str CHECK-CANDIDATE! 0 T=
REJECTION:BUFFER$ s\" error[E-MISMATCH]: TDLR4 ends with tdlrpk:tdlrfam on the stack; its signature promises n\n" >str CONTAINS? TTRUE
REJECTION:BUFFER$ s\"   = expected: n\n  = found: tdlrpk:tdlrfam\n" >str CONTAINS? TTRUE

\ Same row, but the package was declared in UPPER case (TDLR4's `tdlrpk` is
\ already lowercase). A family stores the package name as the source `package`
\ line typed it, while packages compare case-insensitively, so the rendered
\ qualifier is folded and the verbatim spelling must NOT appear. A
\ locals-sourced term (TDLR5), a signature-sourced one (TDLR6) and the JSON
\ family hint (TDLR7) carry the same folded spelling.
package TDLRPKU
public
NEWTYPE tdlrfamu
;package
REJECTION:CAPTURE
s" TDLR5 ( TDLRPKU:tdlrfamu -- n ) {: q:TDLRPKU:tdlrfamu :} q dup drop" >str CHECK-CANDIDATE! 0 T=
REJECTION:BUFFER$ s\" error[E-MISMATCH]: TDLR5 ends with tdlrpku:tdlrfamu on the stack; its signature promises n\n" >str CONTAINS? TTRUE
REJECTION:BUFFER$ s\"   = expected: n\n  = found: tdlrpku:tdlrfamu\n" >str CONTAINS? TTRUE
REJECTION:BUFFER$ s" found: TDLRPKU:tdlrfamu" >str CONTAINS? TFALSE
REJECTION:CAPTURE
s" TDLR6 ( TDLRPKU:tdlrfamu -- n ) dup drop" >str CHECK-CANDIDATE! 0 T=
REJECTION:BUFFER$ s\" error[E-MISMATCH]: TDLR6 ends with tdlrpku:tdlrfamu on the stack; its signature promises n\n" >str CONTAINS? TTRUE
REJECTION:BUFFER$ s\"   = expected: n\n  = found: tdlrpku:tdlrfamu\n" >str CONTAINS? TTRUE
REJECTION:BUFFER$ s" found: TDLRPKU:tdlrfamu" >str CONTAINS? TFALSE
\ the folded qualifier still names the family back: a signature may spell it either way.
s" TDLR6R ( tdlrpku:tdlrfamu -- TDLRPKU:tdlrfamu )" >str CHECK-QUIET-CANDIDATE! -1 T=
REJECTION:CAPTURE  true REJECTION:JSON !
s" TDLR7 ( n -- TDLRPKU:tdlrfamu )" >str CHECK-CANDIDATE! 0 T=
REJECTION:BUFFER$ s\" \"expected\":\"tdlrpku:tdlrfamu\"" >str CONTAINS? TTRUE
REJECTION:BUFFER$ s\" \"expected\":\"TDLRPKU:tdlrfamu\"" >str CONTAINS? TFALSE
false REJECTION:JSON !
\ a package reopened under different capitalisation is the SAME package: the
\ engine reports the first-registered spelling, so the family renders bare
\ inside it rather than picking up a self-qualifier.
package tdlrpku
REJECTION:CAPTURE
s" TDLR8 ( tdlrfamu -- n ) dup drop" >str CHECK-CANDIDATE! 0 T=
REJECTION:BUFFER$ s\" error[E-MISMATCH]: TDLR8 ends with tdlrfamu on the stack; its signature promises n\n" >str CONTAINS? TTRUE
REJECTION:BUFFER$ s\"   = expected: n\n  = found: tdlrfamu\n" >str CONTAINS? TTRUE
REJECTION:BUFFER$ s" found: tdlrpku:tdlrfamu" >str CONTAINS? TFALSE
;package
REJECTION:CAPTURE

\ E-MISMATCH's `family` note carries the same interned qualified spelling as its
\ `expected` and `found` notes: bare tail for a global-package sum family,
\ pkg:tail for a foreign-package one.
\ Only a layout family gets the note, so both fixtures are SUMTYPEs.
SUMTYPE tdlrjg VARIANT keep n ;VARIANT ;SUMTYPE
package tdlrjp
public
SUMTYPE tdlrjs VARIANT keep n ;VARIANT ;SUMTYPE
;package
REJECTION:CAPTURE  true REJECTION:JSON !
s" TDLRJ1 ( n -- tdlrjg )" >str CHECK-CANDIDATE! 0 T=
REJECTION:BUFFER$ s\" \"family\":\"tdlrjg\"" >str CONTAINS? TTRUE
REJECTION:CAPTURE
s" TDLRJ2 ( n -- tdlrjp:tdlrjs )" >str CHECK-CANDIDATE! 0 T=
REJECTION:BUFFER$ s\" \"family\":\"tdlrjp:tdlrjs\"" >str CONTAINS? TTRUE
REJECTION:BUFFER$ s\" \"family\":\"tdlrjs\"" >str CONTAINS? TFALSE
false REJECTION:JSON !
REJECTION:BUFFER-OFF

\ a single multi-cell family parameter (option<off len>, option<^u8 n>,
\ result<n n,n>) rejects at the signature's one-type-per-slot separator check;
\ only comma-separated CELL params (tdres<n,n> above) are the accepted
\ flat-tuple form.
SUMTYPE tdpbopt<a> VARIANT none ;VARIANT VARIANT some a ;VARIANT ;SUMTYPE
SUMTYPE tdpbres<a,b> VARIANT ok a ;VARIANT VARIANT err b ;VARIANT ;SUMTYPE
s" TDPB1 ( -- tdpbopt<off len> )"  >str CHECK-QUIET-CANDIDATE! 0 T=
s" TDPB2 ( -- tdpbopt<^u8 n> )" >str CHECK-QUIET-CANDIDATE! 0 T=
s" TDPB3 ( -- tdpbres<n n,n> )"    >str CHECK-QUIET-CANDIDATE! 0 T=

\ constructor and MATCH effects for a NAMED multi-cell layout-family arg.
\ TDPN1-3 identity-check; TDPN4-7 construct and certify: the payload input and
\ the layout-bundle output expand to the arg-aware width (TFC-PAY-ROW), and the
\ family's type args come from the declared output (TFC-ARGS!), so
\ option<tdpbw2>'s tag sits at slot 2 (declared family width would put it at
\ slot 1). TDPA1-3 reject: wrong-width payload two ways, cross-family bundle.
\ The raw multi-token runs (TDPB1-3) stay rejected.
STRUCTURE tdpbw2 FIELD x n FIELD y n ;STRUCTURE
s" TDPN1 ( tdpbopt<tdpbw2> -- tdpbopt<tdpbw2> )" >str CHECK-QUIET-CANDIDATE! -1 T=
s" TDPN2 ( tdpbres<tdpbw2,n> -- tdpbres<tdpbw2,n> )" >str CHECK-QUIET-CANDIDATE! -1 T=
s" TDPN3 ( tdpbres<tdpbw2,tdpbw2> -- tdpbres<tdpbw2,tdpbw2> )" >str CHECK-QUIET-CANDIDATE! -1 T=
\ SOME / NONE / OK / ERR on named W>1 args certify.
s" TDPN4 ( tdpbw2 -- tdpbopt<tdpbw2> ) TDPBOPT:SOME" >str CHECK-QUIET-CANDIDATE! -1 T=
s" TDPN5 ( -- tdpbopt<tdpbw2> ) TDPBOPT:NONE" >str CHECK-QUIET-CANDIDATE! -1 T=
s" TDPN6 ( tdpbw2 -- tdpbres<tdpbw2,n> ) TDPBRES:OK" >str CHECK-QUIET-CANDIDATE! -1 T=
s" TDPN7 ( n -- tdpbres<tdpbw2,n> ) TDPBRES:ERR" >str CHECK-QUIET-CANDIDATE! -1 T=
\ TDPA1: the output run is read at the width it is laid at, so it prints as one
\ tdpbopt<tdpbw2>; test/type-ctor-suite.f CLFC-RT-SOME proves that width.
REJECTION:CAPTURE  true REJECTION:JSON !
s" TDPA1 ( n -- tdpbopt<tdpbw2> ) TDPBOPT:SOME" >str CHECK-CANDIDATE! 0 T=
REJECTION:BUFFER$ s\" \"code\":\"E-MISMATCH\"" >str CONTAINS? TTRUE
REJECTION:BUFFER$ s\" \"inferred\":\"n -- tdpbopt<tdpbw2>\"" >str CONTAINS? TTRUE
false REJECTION:JSON !
REJECTION:BUFFER-OFF
s" TDPA2 ( tdpbw2 -- tdpbopt<n> ) TDPBOPT:SOME" >str CHECK-QUIET-CANDIDATE! 0 T=
s" TDPA3 ( -- tdpbopt<tdpbw2> ) TDPBRES:OK" >str CHECK-QUIET-CANDIDATE! 0 T=
\ TDPA4: constructing a linear-payload multi-cell sum is sound
\ (test/type-linear-suite.f A1/A6/A7); duplicating the constructed bundle is
\ not. With the linear con `tdown` (declared above), SOME certifies and dup
\ rejects.
s" TDPA4 ( tdown -- tdpbopt<tdown> ) TDPBOPT:SOME" >str CHECK-QUIET-CANDIDATE! -1 T=
s" TDPA4B ( tdown -- tdpbopt<tdown> tdpbopt<tdown> ) TDPBOPT:SOME dup" >str CHECK-QUIET-CANDIDATE! 0 T=
\ real compiles of wide constructs are pinned in test/type-ctor-suite.f (CN-OPEN
\ for the rc 70 refusal); this suite stays stderr-clean.

\ A named multi-cell family instance as another's payload: every width site
\ recurses through the arguments (TFAM-INST-WIDTH@, TFC-VAR-PAYCELLS, T-WIDTH).
STRUCTURE tdnw2 FIELD x n FIELD y n ;STRUCTURE
SUMTYPE tdnopt<a> VARIANT none ;VARIANT VARIANT some a ;VARIANT ;SUMTYPE
SUMTYPE tdnres<a,b> VARIANT ok a ;VARIANT VARIANT err b ;VARIANT ;SUMTYPE
s" TDNEST1 ( tdnopt<tdnres<n,n>> -- tdnopt<tdnres<n,n>> )" >str CHECK-QUIET-CANDIDATE! -1 T=
s" TDNEST2 ( tdnres<n,n> -- tdnopt<tdnres<n,n>> ) TDNOPT:SOME" >str CHECK-QUIET-CANDIDATE! -1 T=
s" TDNEST3 ( -- tdnopt<tdnres<n,n>> ) TDNOPT:NONE" >str CHECK-QUIET-CANDIDATE! -1 T=
s" TDNEST4 ( tdnres<tdnw2,n> -- tdnopt<tdnres<tdnw2,n>> ) TDNOPT:SOME" >str CHECK-QUIET-CANDIDATE! -1 T=
\ An open inner var leaves the layout's width unknown, so the candidate refuses;
\ test/type-ctor-suite.f CN-OPEN pins the real load.
REJECTION:CAPTURE  true REJECTION:JSON !
s" TDNOPEN ( tdnres<n,a> -- tdnopt<tdnres<n,a>> ) TDNOPT:SOME" >str CHECK-CANDIDATE! 0 T=
REJECTION:BUFFER$ s\" \"code\":\"E-MISMATCH\"" >str CONTAINS? TTRUE
REJECTION:BUFFER-OFF
\ TDNA1's expected renders tdnres<tdnw2,n> whole: its width-3 instance has its
\ tag at slot 2, so the full run compacts to the sum type.
REJECTION:CAPTURE  true REJECTION:JSON !
s" TDNA1 ( tdnres<n,n> -- tdnopt<tdnres<tdnw2,n>> ) TDNOPT:SOME" >str CHECK-CANDIDATE! 0 T=
REJECTION:BUFFER$ s\" \"code\":\"E-MISMATCH\"" >str CONTAINS? TTRUE
REJECTION:BUFFER$ s\" \"expected\":\"tdnres<tdnw2,n>\"" >str CONTAINS? TTRUE
REJECTION:CAPTURE
s" TDNA2 ( tdnopt<n> -- tdnopt<tdnres<n,n>> ) TDNOPT:SOME" >str CHECK-CANDIDATE! 0 T=
REJECTION:BUFFER$ s\" \"code\":\"E-MISMATCH\"" >str CONTAINS? TTRUE
REJECTION:BUFFER$ s\" \"family\":\"tdnres\"" >str CONTAINS? TTRUE
false REJECTION:JSON !
REJECTION:BUFFER-OFF
s" TDNA3 ( n -- tdnopt<tdnres<n,n>> ) TDNOPT:SOME" >str CHECK-QUIET-CANDIDATE! 0 T=

\ These load only when their facts text reads back rows of 65 and 300 values.
: TFR-W65 ( n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n -- n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n ) ;
: TFR-Q300 ( [ n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n -- ] -- ) drop ;

\ NEWTYPE over a base spelled on the name's line: the family is as wide as the
\ base and a type apart from it, and no word is made, so casts cross it. A str
\ holds a pointer, so the casts mint one and sit in a private section.
REJECTION:CAPTURE
NEWTYPE tdwrap str
package TDWRAP-CAST
private
s" cast: >TDWRAP ( str -- tdwrap )" 0 TDT-NEG
s" cast: TDWRAP> ( tdwrap -- str )" 0 TDT-NEG
: TDWR-TRIP ( -- n ) s" abc" >str >TDWRAP TDWRAP> str> nip ;
TDWR-TRIP 3 T=
REJECTION:CAPTURE
s" cast: TDWR-NARROW ( n -- tdwrap )" 70 TDT-NEG
s\" error[E-CAST-ARITY]: in `cast: TDWR-NARROW`, the input and output differ in width\n" >str TDT-HAS
s\"   = help: give both sides the same width in cells: `n` is 1 cell, `tdwrap` is 2\n" >str TDT-HAS
;package
s" TDWR-NOT ( str -- tdwrap )" >str CHECK-QUIET-CANDIDATE! 0 T=
\ A comment on the name's line is no base: the family stays one cell.
NEWTYPE tdnote   \ a comment
s" cast: TDN-CELL ( n -- tdnote )" 0 TDT-NEG
\ A base never declared is rejected as an unknown type is.
s" NEWTYPE tdbad tdmissing-type"
s\" error[E-BAD-DECLARATION]: bad newtype declaration `tdbad`\n" >str
s" tdmissing-type" >str s" unknown type" >str TDT-AT
s\"   = help: name a type declared before this declaration: `n`, `^u8`, a `STRUCTURE` or an `ENUM`\n" >str TDT-HAS
\ A base's letters are not the previous declaration's header parameters.
s\" STRUCTURE tdwhead<a,q> FIELD x a ;STRUCTURE\nNEWTYPE tdwnohead read-view<q,q,a>"
s\" error[E-BAD-DECLARATION]: bad newtype declaration `tdwnohead`\n" >str
s" q" >str s" unknown type" >str TDT-AT
REJECTION:BUFFER$ s" error[" >str COUNT-SUB 1 T=
s\"   = help: name a type declared before this declaration: `n`, `^u8`, a `STRUCTURE` or an `ENUM`\n" >str TDT-HAS
\ The verifier replays a base on the name's line as the load registers it.
: TDWR-VERIFY ( -- )
   s\" NEWTYPE tdvwrap str\n: TDWR-VUSE ( tdvwrap -- tdvwrap ) ;" >str VERIFY:SOURCE-BUF ;
' TDWR-VERIFY catch 0 T=
REJECTION:BUFFER-OFF
TD-JSON-WAS @ REJECTION:JSON !

\ A failure prints F<index> + detail; T-REPORT exits 1.
T-REPORT

;using
