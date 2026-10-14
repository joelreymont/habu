require src/compiler/native/compiler.f
1 set-tier
package FILE-STATE-A
private
variable COUNT-CELL
PERSISTED-PTR-VARIABLE LINK-CELL
TYPED-VARIABLE ACTION-CELL [ -- n ]
TYPED-VARIABLE QUOTE-CELL [ n -- n ]
create LETTERS 16 allot
public
: COUNT@ ( -- n ) COUNT-CELL @ ;
: COUNT! ( n -- ) COUNT-CELL ! ;
: LINK@ ( -- ptr u8 ) LINK-CELL @ ;
: LINK! ( ptr u8 -- ) LINK-CELL ! ;
: ACTION@ ( -- [ -- n ] ) ACTION-CELL @ ;
: ACTION! ( [ -- n ] -- ) ACTION-CELL ! ;
: QUOTE@ ( -- [ n -- n ] ) QUOTE-CELL @ ;
: QUOTE! ( [ n -- n ] -- ) QUOTE-CELL ! ;
: INIT-QUOTE ( -- ) [: 7 + ;] QUOTE! ;
: LETTERS$ ( -- ptr u8 n ) LETTERS 16 ;
: PREFIX-MAKE ( n -- ) create , does> ( -- n ) @ ;
INIT-QUOTE
;package
