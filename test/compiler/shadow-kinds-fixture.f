\ shadow-kinds-fixture.f - one definition of each kind, read by
\ test/compiler/shadow-kinds.f through the typed interpret loop at tier 0 while
\ an arm64 shadow is open. Each `\ ==` line starts a section the driver reads
\ alone, so a kind that refuses leaves the others to be read: a section names
\ only words the driver or its own section made.
require src/lib/ffi-abi.f
require src/lib/ffi-callback.f

\ == variable
package KINDS-FIXTURE
variable KD-CELL
;package

\ == constant
package KINDS-FIXTURE
7 constant KD-SEVEN
;package

\ == create with allot
package KINDS-FIXTURE
create KD-ROW 4 cells allot
;package

\ == BUFFER:
package KINDS-FIXTURE
16 BUFFER: KD-BYTES
;package

\ == TYPED-VARIABLE
package KINDS-FIXTURE
TYPED-VARIABLE KD-TYPED n
;package

\ == DYNAMIC-BUFFER
package KINDS-FIXTURE
DYNAMIC-BUFFER KD-GROW n
;package

\ == STRUCTURE and the words it generates
package KINDS-FIXTURE
STRUCTURE kd-pair FIELD lo n FIELD hi n ;STRUCTURE
;package

\ == colon leaf, a colon word calling it (a bl), one ending in a tail call
package KINDS-FIXTURE
: KD-LEAF ( n n -- n ) + ;
: KD-CALLER ( n -- n ) dup KD-LEAF 1 + ;
: KD-TAIL ( n -- n ) 1 KD-LEAF ;
;package

\ == colon word reading a variable (a DATA address)
package KINDS-FIXTURE
: KD-READ ( -- n ) SHADOW-KINDS:PROBE @ ;
;package

\ == colon word with an s" literal
package KINDS-FIXTURE
: KD-TEXT ( -- ptr u8 n ) s" kinds" ;
;package

\ == colon word with a quotation
package KINDS-FIXTURE
: KD-QUOT ( -- [ n -- n ] ) [: 2 * ;] ;
;package

\ == defer, set with is
package KINDS-FIXTURE
defer KD-HOOK ( n -- n )
: KD-SET ( -- ) [: negate ;] is KD-HOOK ;
;package

\ == does> definer and one child it makes
package KINDS-FIXTURE
: KD-CONST ( n -- ) create , does> ( -- n ) @ ;
5 KD-CONST KD-FIVE
;package

\ == cast:
package KINDS-FIXTURE
cast: KD-AS ( n -- n )
;package

\ == linear:, minting a token its package owns
package KINDS-LINEAR
public
DEFLINEAR KINDS-LINEAR:tok
private
linear: KD-MINT ( n -- KINDS-LINEAR:tok )
;package

\ == immediate
package KINDS-FIXTURE
: KD-NOW ( -- ) ; immediate
;package

\ == word inside a package, with an EXPORT alias of it
package KINDS-FIXTURE
: KD-INNER ( -- n ) 3 ;
public
EXPORT KD-INNER
;package

\ == CALLBACK:
package KINDS-FIXTURE
CALLBACK: KD-CMP ( ptr u8 ptr u8 -- i32 ) 0 FALLBACK ;CALLBACK
;package
