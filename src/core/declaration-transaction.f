\ declaration-transaction.f - the one declaration transaction's open state.
\ Declarations never nest, so the state is one cell.

package TYPE-DECL
private
package DECLARATION-TRANSACTION

public

7166 constant E-ACTIVE   \ a declaration is already open

private

variable ACTIVE

: OPEN ( -- ) ACTIVE @ IF E-ACTIVE throw THEN  -1 ACTIVE ! ;
: CLOSE ( -- ) 0 ACTIVE ! ;

public

: ACTIVE? ( -- bool ) ACTIVE @ 0 <> ;

;package
;package
