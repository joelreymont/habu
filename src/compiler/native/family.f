\ family.f - what the checker's type-family registry says about a family name and
\ a variant name. One concern: turning the two operand tokens of a tag-dispatch
\ form into the numbers the chain has to compile with.

require lib/prelude.f
require lib/errors.f

package NFAM

public

\ ---- the family a form's first operand names ---------------------------------
\ Signature scope: eliminating a value is allowed wherever naming its type is.
: MATCH-FAM ( ptr u8 n -- n bool )
   TFL-MATCH-FAM? ;

\ Owner scope: minting a value belongs to the package that declared the family.
: CON-FAM ( ptr u8 n -- n bool )
   TFL-CON-FAM? ;

\ ---- the variant a form's second operand names -------------------------------
: VARIANT ( ptr u8 n n -- n bool )
   TFL-VAR? ;

\ ---- what a value of the family is on the data stack -------------------------
\ The DECLARED width, not the instantiated one: the arg-aware width is a function
\ of a resolved type term, and the chain holds only a family id and a token.
: WIDTH ( n -- n )
   TFAM-SLOTS@ 1+ ;

\ The count decides where the mismatch edge of the LAST arm goes.
: VARIANTS ( n -- n )
   TFAM-VAR-COUNT@ ;

: NAME$ ( n -- ptr u8 n )
   TFAM-NAME$ ;

\ ---- what one variant is -----------------------------------------------------
: TAG ( n -- n )
   SUMV-TAG@ ;

\ The pads sit between the payload and the tag, so dropping the top 1 + pads cells
\ of a matched bundle leaves exactly the payload (habu2.f EM-ADT-MATCH-OF).
: PADS ( n n -- n )
   TFL-VPADS ;

\ Equal counts mean every field is one cell; a difference means at least one is a
\ layout family, which a rename inside the arm must not take apart.
: PAY-CELLS ( n -- n )
   SUMV-PAYCELLS@ ;

: PAY-TERMS ( n -- n )
   SUMV-PAY-N ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;package
