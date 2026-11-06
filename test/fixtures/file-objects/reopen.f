\ reopen.f - wordlists minted out of record order: count.f's FILE-OBJECTS-LATE
\ and then this file's FILE-OBJECTS-SOLO gain their private wordlists after
\ their records, so SOLO's is not its public id plus one; an unused `does>`
\ definer; a word in no package. Loaded after count.f.

: FILE-OBJECTS-SOLO:ONE ( -- n ) 1 ;

package FILE-OBJECTS-LATE
private
: TWO ( -- n ) 2 ;
;package

package FILE-OBJECTS-SOLO
private
: THREE ( -- n ) 3 ;
public
: MAKER ( n -- ) create , does> ( -- n ) @ ;
;package

: TOP ( -- n ) 4 ;
