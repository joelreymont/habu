\ family.f - what the checker's type-family registry says about the family and
\ variant operands of a tag-dispatch form.

require src/lib/prelude.f
require src/lib/error.f
require src/core/type-family.f

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
   TFAM:TFL-VAR? ;

\ ---- what a value of the family is on the data stack -------------------------
\ The DECLARED width, not the instantiated one: the arg-aware width is a function
\ of a resolved type term, and the chain holds only a family id and a token.
: WIDTH ( n -- n )
   TFAM:TFAM-SLOTS@ 1+ ;

\ The count decides where the mismatch edge of the LAST arm goes.
: VARIANTS ( n -- n )
   TFAM:TFAM-VAR-COUNT@ ;

\ Whether MATCH makes the value's tag and construct omits it.
: NICHE? ( n -- bool )
   TFAM:TFAM-NICHE? ;

: NAME$ ( n -- ptr u8 n )
   TFAM-NAME$ ;

\ ---- what one variant is -----------------------------------------------------
: TAG ( n -- n )
   TFAM:SUMV-TAG@ ;

\ The pads sit between the payload and the tag, so dropping the top 1 + pads cells
\ of a matched bundle leaves exactly the payload.
: PADS ( n n -- n )
   TFAM:TFL-VPADS ;

\ Equal counts mean every field is one cell; a difference means at least one is a
\ layout family, which a rename inside the arm must not take apart.
: PAY-CELLS ( n -- n )
   TFAM:SUMV-PAYCELLS@ ;

seal-package

;package
