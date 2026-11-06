\ declaration-transaction.f - the one declaration transaction's open state.
\ Its runner is in generated-declaration-protection.f; declarations never nest
\ (OPEN throws E-ACTIVE), so the state is one cell.

package DECLARATION-TRANSACTION

public

7166 constant E-ACTIVE   \ a declaration is already open
7171 constant E-FRAME    \ the body left the checker frame stack not matching the declaration's frame

private

variable ACTIVE

: OPEN ( -- ) ACTIVE @ IF E-ACTIVE throw THEN  -1 ACTIVE ! ;
: CLOSE ( -- ) 0 ACTIVE ! ;

public

: ACTIVE? ( -- bool ) ACTIVE @ 0 <> ;

;package
