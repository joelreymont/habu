\ family.f - what the checker's type-family registry says about a family name and
\ a variant name. One concern: turning the two operand tokens of a tag-dispatch
\ form into the numbers the chain has to compile with.
\
\ WHY THE CHAIN HAS TO ASK AT ALL. `MATCH family variant OF … ENDOF … ;MATCH`,
\ `case … endcase` and `construct family variant` are the source forms whose
\ operands are not body words: `option` and `some` denote nothing in the
\ dictionary, so the walk that compiles a body cannot ask what it asks about
\ every other token. What they denote is a row of the type-family registry, and
\ four numbers come off that row - the width of the bundle a value of the family
\ occupies, a variant's tag, how many of the bundle's cells that variant leaves
\ as zero pads, and how many cells its payload is - which is exactly what a
\ dispatch has to compare, drop and keep.
\
\ IT IS THE SAME REGISTRY AND THE SAME WORDS THE ENGINE'S OWN EMITTER USES.
\ src/habu/habu2.f reaches `TFL-MATCH-FAM?` at its `match` family token,
\ `TFL-CVAR?`'s pieces at each variant token and `TFL-CON-FAM?` at `construct`,
\ through a bridge that resolves each by name in the running dictionary. So an
\ interpreted MATCH and a compiled one are not two readings of a declaration:
\ they are one reading, asked twice. A second table of families here would be the
\ second authority this chain refuses to keep anywhere else.
\
\ AND THE SCOPE RULE IS THE REGISTRY'S TOO, WHICH IS WHY THERE ARE TWO FAMILY
\ RESOLVERS. A `MATCH` may name any family a stack signature could name, because
\ a value of that family can only have reached the body through such a signature;
\ `construct` may only name a family the ACTIVE package owns, because
\ constructing one is minting it. Those are two different questions with two
\ different answers for the same token, and the registry answers both
\ (src/core/type-family.f, docs/type-families.md §12 and §14). Asking the wrong
\ one here would let a body construct a value of somebody else's family.
\
\ WHY EVERY ANSWER ARRIVES PAST A TRUSTED BOUNDARY, AND WHY THE BOUNDARY IS ONE
\ LINE WIDE. The registry lives in the boot prefix, where its readers are
\ signature-less colon words the seal strips, so the checker has no symbol for
\ them and checked code cannot name one - measured: `: P ( ptr u8 n -- n bool )
\ TFL-MATCH-FAM? ;` is E-UNDEFINED and the definition is refused. That is the
\ same wall src/compiler/native/dict.f meets at the checker's effect store and it
\ is answered the same way: one declared word per question, whose whole body is
\ the call, and every decision above it ordinary checked Habu. Each boundary
\ asserts the effect the registry's own definition declares in its stack comment,
\ and the fixture beside this file calls each one on a real declaration.
\
\ WHAT IT REFUSES, AND WHERE THE REFUSING IS DONE. Nothing here refuses: a
\ declining resolver answers false and the caller decides. The caller is
\ src/compiler/native/elaborate.f, whose pre-pass turns a declined answer into
\ E-NELAB-MATCH at the token that was declined - and reaching that means the
\ chain and the checker read one body differently, because the checker refuses
\ every one of those forms before the definition is compiled at all.

require lib/prelude.f
require lib/errors.f

package NFAM
private

\ ---- the boundary ------------------------------------------------------------
\ One word per registry question. The effects are the ones src/core/type-family.f
\ declares for each of them; nothing here computes, so a body longer than the
\ call would be this file deciding something.
TRUSTED: R-MATCH-FAM ( ptr u8 n -- n bool )   TFL-MATCH-FAM? ;
TRUSTED: R-CON-FAM ( ptr u8 n -- n bool )     TFL-CON-FAM? ;
TRUSTED: R-VAR ( ptr u8 n n -- n bool )       TFL-VAR? ;
TRUSTED: R-SLOTS ( n -- n )                   TFAM-SLOTS@ ;
TRUSTED: R-VCOUNT ( n -- n )                  TFAM-VAR-COUNT@ ;
TRUSTED: R-FAM-NAME ( n -- ptr u8 n )         TFAM-NAME$ ;
TRUSTED: R-TAG ( n -- n )                     SUMV-TAG@ ;
TRUSTED: R-PADS ( n n -- n )                  TFL-VPADS ;
TRUSTED: R-PAY-CELLS ( n -- n )               SUMV-PAYCELLS@ ;
TRUSTED: R-PAY-N ( n -- n )                   SUMV-PAY-N ;

public

\ ---- the family a form's first operand names ---------------------------------
\ Signature scope, for a `MATCH`: eliminating a value is allowed wherever naming
\ its type is. A family that resolves to nothing, and one that is neither a sum
\ nor an enum, both answer false - the registry makes both refusals itself, so
\ this file states neither.
: MATCH-FAM ( ptr u8 n -- n bool )
   R-MATCH-FAM ;

\ Owner scope, for a `construct`: minting a value of a family belongs to the
\ package that declared it.
: CON-FAM ( ptr u8 n -- n bool )
   R-CON-FAM ;

\ ---- the variant a form's second operand names -------------------------------
: VARIANT ( ptr u8 n n -- n bool )
   R-VAR ;

\ ---- what a value of the family is on the data stack -------------------------
\ How many cells one value occupies: the tag, plus the payload slots the family
\ reserves for its widest variant. Both resolvers above admit only a sum or an
\ enum, and every value of one carries a tag, so the cell is unconditional.
\
\ IT IS THE DECLARED WIDTH AND NOT THE INSTANTIATED ONE, which is a real
\ narrowing and is stated here rather than left to be discovered. The registry
\ also computes an arg-aware width (`TFAM-INST-WIDTH@`), which is wider when a
\ parametric family is instantiated with a type argument that is itself several
\ cells - but it is a function of a RESOLVED TYPE TERM, and a type term is the
\ checker's value: the chain holds a family id and a source token and has no term
\ to hand it. So this answers the declared width, and the caller holds it against
\ the bundle the compile-time value vector really has, which is the only place
\ the two can be compared. A value wider than this is refused there by name
\ rather than compiled against the wrong number.
: WIDTH ( n -- n )
   R-SLOTS 1+ ;

\ How many variants the family has, which is how many arms an exhaustive dispatch
\ over it must have. The chain does not enforce exhaustiveness - the checker
\ already refuses a MATCH that is not, and its refusal names the variants the
\ body left out - but the count decides where the mismatch edge of the LAST arm
\ goes, so the number is read rather than counted from the tokens.
: VARIANTS ( n -- n )
   R-VCOUNT ;

\ The family's own name, which is the only name a trap over it could usefully
\ print, and the key src/compiler/native/trap.f gives an ordinal to.
: NAME$ ( n -- ptr u8 n )
   R-FAM-NAME ;

\ ---- what one variant is -----------------------------------------------------
\ Its tag, which is its position in the declaration and the value the dispatch
\ compares against.
: TAG ( n -- n )
   R-TAG ;

\ How many cells of the bundle this variant leaves as zero pads: the family
\ reserves room for its widest variant, so a narrower one is padded up to it. The
\ pads sit between the payload and the tag, which is what makes dropping the top
\ `1 + pads` cells of a matched bundle leave exactly the payload - measured on
\ generated constructors, and the same count the engine's own arm subtracts from
\ the data-stack pointer (src/habu/habu2.f EM-ADT-MATCH-OF).
: PADS ( n n -- n )
   R-PADS ;

\ How many cells its payload occupies, and how many VALUES those cells are.
\
\ THE TWO COUNTS ARE BOTH PUBLISHED BECAUSE THEIR DIFFERENCE IS THE ANSWER TO A
\ QUESTION THE CALLER HAS TO ASK. A payload of two `n` fields is two cells and
\ two values, and a rename inside the arm may move either; a payload of one field
\ whose type is itself a layout family is several cells and ONE value, and a
\ rename that moved one of them would take the value apart. The registry has no
\ exported per-field width to say WHICH of several fields is the wide one, so
\ equal counts mean every field is one cell and a difference means at least one
\ is not - which is exactly the distinction src/compiler/native/dict.f already
\ draws over a declared row's terms and cells, and it answers it the same way.
: PAY-CELLS ( n -- n )
   R-PAY-CELLS ;

: PAY-TERMS ( n -- n )
   R-PAY-N ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;package
