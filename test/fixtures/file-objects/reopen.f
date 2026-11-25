\ reopen.f - wordlists minted out of record order: count.f's FILE-OBJECTS-LATE
\ and then this file's FILE-OBJECTS-SOLO gain their private wordlists after
\ their records, so SOLO's is not its public id plus one; an unused `does>`
\ definer; a word in no package, whose xt it stores into count.f's LATER; a data
\ cell row on count.f's MARKED, which holds 0, with no store; DOWN, private in
\ count.f's package, whose private wordlist count.f made. Loaded after count.f.

: FILE-OBJECTS-SOLO:ONE ( -- n ) 1 ;

package FILE-OBJECTS-LATE
private
: TWO ( -- n ) 2 ;
\ Keeps the xt, so the program reaches TWO.
variable KEEP-TWO  ' TWO KEEP-TWO xt!
;package

package FILE-OBJECTS-SOLO
private
: THREE ( -- n ) 3 ;
public
: MAKER ( n -- ) create , does> ( -- n ) @ ;
private
\ Keeps the xts, so the program reaches ONE and THREE.
variable KEEP-ONE  ' ONE KEEP-ONE xt!
variable KEEP-THREE  ' THREE KEEP-THREE xt!
;package

package FILE-OBJECTS-COUNT
private
: DOWN ( n -- n ) 1 - ;
\ Keeps the xt, so the program reaches DOWN.
variable KEEP-DOWN  ' DOWN KEEP-DOWN xt!
;package

: TOP ( -- n ) 4 ;
' TOP FILE-OBJECTS-COUNT:LATER xt!
FILE-OBJECTS-COUNT:MARKED ptr-cell-mark
