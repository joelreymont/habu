\ native-rename-rows.f - a compile-time stack rename may not take a multi-cell
\ value apart (dot habu-rename-over-rows-982167af).
\
\ WHAT WENT WRONG, AND WHY NOTHING CAUGHT IT. The elaborator's compile-time value
\ vector holds one entry per stack CELL. A value of a layout family occupies
\ several cells, so it occupies several entries, and a rename is nothing but a
\ permutation of that vector. Renaming across such a value therefore reorders its
\ cells - and because the rename's inputs and picks are counted in cells too, the
\ arity still balances and every later stage sees a well-formed program. Four
\ working definitions were compiled into wrong ones with no diagnostic anywhere.
\
\ SO THIS SUITE IS A DIFFERENTIAL AND NOT A TABLE OF EXPECTED NUMBERS. Each case
\ states one body TWICE: once for the engine to compile as an ordinary definition,
\ and once as source handed to NMIGRATE:DEFINE, which publishes the native chain's
\ code under a second name. Both are then executed on the same inputs and their
\ raw cells compared. An expected-value table would have been written from
\ whichever of the two the author trusted; comparing the two makes the engine the
\ authority and needs no author to be right about anything.
\
\ THE STACK IS READ AS CELLS, DELIBERATELY. A multi-cell value cannot be compared
\ by the checked language - there is no `=` for it, and taking it apart is the very
\ thing under test. So the comparisons are a named unchecked boundary and read the
\ cells the two words left, which is the level the bug lives at: the miscompile
\ produced the right NUMBER of cells with two of them exchanged.
\
\ THE TWO HALVES ARE BOTH ASSERTED, and the negative half is the one that keeps
\ this suite honest. A refusal that fired on every definition mentioning a layout
\ family would pass the first half and is caught by the second: bodies that hold
\ such a value without renaming across it must still compile, and a body whose
\ signature is two INDEPENDENT one-cell variables - which reports the same term
\ count, the same cell count and the same per-term family as the bundled one, and
\ differs only in the slots the checker records - must still compile and agree.
\ Both halves were falsified by mutation before being believed: deleting the
\ refusal fails the first four cases, and making the glue reader answer "bundled"
\ for every term fails exactly those last two.
\
\ EVERYTHING IS IN ONE PACKAGE, INCLUDING THE MIGRATED TWINS. The chain publishes
\ each twin by evaluating its source text, so the twin lands in whatever wordlist
\ is current when that happens; running the migrations inside the package block is
\ what puts the two columns of every comparison in one scope without a single
\ global name.
\
\ Run: bin/hb --load test/compiler/native-rename-rows.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/adt/option.f
require src/compiler/native/migrate.f

package NRR
private

8 constant REGS

\ ---- the bodies the engine compiles ------------------------------------------
: MK ( n -- option<n> )                        OPTION:SOME ;

\ The four that took a value apart. Each names its value a different way: from
\ the signature, through a deeper rename, from a generated constructor, and from
\ an ordinary user word whose declared output is the value.
: E-SWAP ( option<n> n -- n option<n> )        swap ;
: E-ROT  ( option<n> n n -- n n option<n> )    rot ;
: E-CTOR ( n n -- option<n> n )                swap OPTION:SOME swap ;
: E-USER ( n n -- option<n> n )                swap MK swap ;

\ The ones that never were wrong: a value held but not renamed across, one handed
\ straight back, one made and left alone, one carried across a loop edge, and the
\ two-variable twin of the first bundled signature.
: E-HOLD ( n n option<n> -- n n option<n> ) ;
: E-PASS ( option<n> -- option<n> ) ;
: E-MADE ( n -- option<n> )                    OPTION:SOME ;
: E-LOOP ( option<n> n n -- option<n> )        ?do loop ;
: E-VARS ( a b n -- n a b )                    swap ;
: E-CELL ( n n -- n n )                        swap ;

\ A CLOSED ENUM IS A LAYOUT FAMILY WHOSE VALUES ARE ONE CELL, and it is the case
\ that decides whether this refusal is worth having. The checker records such a
\ value cell by cell with its position exactly as it records a wider one, so a
\ reader that asked only "does this term carry a position" would mark every enum
\ in the tree and refuse renames over values that cannot be taken apart - it cost
\ sixteen definitions the first time this was measured. A value of one cell is
\ moved whole by moving one cell, so it is not marked and this must still compile.
public
ENUM lamp DERIVE eq
   dim
   bright
;ENUM
private

: E-ENUM ( lamp n -- n lamp )                  swap ;

\ ---- the source the chain is given -------------------------------------------
\ Character for character the body above it, so a difference between the two
\ columns can only come from the two compilers and never from two programs.
: SWAP$ ( -- ptr u8 n ) s" : C-SWAP ( option<n> n -- n option<n> ) swap ;" ;
: ROT$  ( -- ptr u8 n ) s" : C-ROT ( option<n> n n -- n n option<n> ) rot ;" ;
: CTOR$ ( -- ptr u8 n ) s" : C-CTOR ( n n -- option<n> n ) swap OPTION:SOME swap ;" ;
: USER$ ( -- ptr u8 n ) s" : C-USER ( n n -- option<n> n ) swap MK swap ;" ;

: HOLD$ ( -- ptr u8 n ) s" : C-HOLD ( n n option<n> -- n n option<n> ) ;" ;
: PASS$ ( -- ptr u8 n ) s" : C-PASS ( option<n> -- option<n> ) ;" ;
: MADE$ ( -- ptr u8 n ) s" : C-MADE ( n -- option<n> ) OPTION:SOME ;" ;
: LOOP$ ( -- ptr u8 n ) s" : C-LOOP ( option<n> n n -- option<n> ) ?do loop ;" ;
: VARS$ ( -- ptr u8 n ) s" : C-VARS ( a b n -- n a b ) swap ;" ;
: CELL$ ( -- ptr u8 n ) s" : C-CELL ( n n -- n n ) swap ;" ;
: ENUM$ ( -- ptr u8 n ) s" : C-ENUM ( lamp n -- n lamp ) swap ;" ;

\ ---- driving one migration where its refusal can be read ---------------------
\ A checked `catch` takes a stack-neutral quotation and a quotation cannot read
\ the enclosing word's locals, so what the migration needs is parked first - the
\ same shape src/compiler/native/migrate.f uses for the same reason.
variable M-A   variable M-U   variable M-IN   variable M-OUT

: MIGRATE-RC ( -- n )
   [: M-A @ M-U @ M-IN @ M-OUT @ REGS NMIGRATE:DEFINE ;] catch ;

: TRY ( ptr u8 n n n -- n ) {: a:ptr u:n in:n out:n :}
   a M-A !  u M-U !  in M-IN !  out M-OUT !
   MIGRATE-RC ;

\ ---- the migrations that must succeed ----------------------------------------
\ THESE RUN BEFORE THE COMPARISONS ARE COMPILED, because each publishes the name
\ one half of a comparison calls and a word cannot be compiled against a name
\ nothing has defined yet. The outcomes are parked and asserted with the rest.
variable RC-HOLD  variable RC-PASS  variable RC-MADE
variable RC-LOOP  variable RC-VARS  variable RC-CELL
variable RC-ENUM
variable RC-SWAP  variable RC-ROT   variable RC-CTOR  variable RC-USER

\ Every migration runs HERE, inside the package block, and for two reasons.
\ The chain publishes by evaluating the source text, so a twin migrated after
\ `;package` would land outside the package and a body naming a package word -
\ `MK` - would not resolve at all, which is a refusal about scope rather than
\ about renames. And the twins that succeed have to exist before the comparisons
\ below are compiled against their names.
: RUN-THE-MIGRATIONS ( -- )
   HOLD$ 4 4 TRY RC-HOLD !
   PASS$ 2 2 TRY RC-PASS !
   MADE$ 1 2 TRY RC-MADE !
   LOOP$ 4 2 TRY RC-LOOP !
   VARS$ 3 3 TRY RC-VARS !
   CELL$ 2 2 TRY RC-CELL !
   ENUM$ 2 2 TRY RC-ENUM !
   SWAP$ 3 3 TRY RC-SWAP !
   ROT$  4 4 TRY RC-ROT !
   CTOR$ 2 3 TRY RC-CTOR !
   USER$ 2 3 TRY RC-USER ! ;

RUN-THE-MIGRATIONS

\ ---- executing both publications ---------------------------------------------
\ Reading the cells two words left is not something the checked language can do -
\ a multi-cell value has no `=` and taking it apart is the bug under test - so
\ this is the suite's one named unchecked boundary, and it is exactly as wide as
\ the comparison needs. It retires with dot habu-rename-rows-row-143c0331, which
\ gives renames whole values to move and these comparisons something to say.
variable D1  variable D2  variable D3  variable D4

TRUSTED: GRAB4 ( n n n n -- )   D4 !  D3 !  D2 !  D1 ! ;
TRUSTED: GRAB3 ( n n n -- )     D3 !  D2 !  D1 !  0 D4 ! ;
TRUSTED: GRAB2 ( n n -- )       D2 !  D1 !  0 D3 !  0 D4 ! ;
TRUSTED: SAVE ( -- n n n n )    D1 @ D2 @ D3 @ D4 @ ;

TRUSTED: SAME ( n n n n n n n n -- )
   {: a1:n a2:n a3:n a4:n b1:n b2:n b3:n b4:n :}
   a1 b1 T=  a2 b2 T=  a3 b3 T=  a4 b4 T= ;

TRUSTED: AGREE-HOLD ( -- )
   s" a held value comes back cell for cell as the engine leaves it" T-LABEL
   5 6 42 OPTION:SOME E-HOLD GRAB4 SAVE
   5 6 42 OPTION:SOME C-HOLD GRAB4 SAVE
   SAME ;

TRUSTED: AGREE-PASS ( -- )
   s" and so does one handed straight back" T-LABEL
   42 OPTION:SOME E-PASS GRAB2 SAVE
   42 OPTION:SOME C-PASS GRAB2 SAVE
   SAME ;

TRUSTED: AGREE-MADE ( -- )
   s" and one the body made" T-LABEL
   7 E-MADE GRAB2 SAVE
   7 C-MADE GRAB2 SAVE
   SAME ;

TRUSTED: AGREE-LOOP ( -- )
   s" and one carried across a loop edge" T-LABEL
   42 OPTION:SOME 3 0 E-LOOP GRAB2 SAVE
   42 OPTION:SOME 3 0 C-LOOP GRAB2 SAVE
   SAME ;

TRUSTED: AGREE-VARS ( -- )
   s" the two-variable twin is renamed, and renamed the same way" T-LABEL
   11 22 33 E-VARS GRAB3 SAVE
   11 22 33 C-VARS GRAB3 SAVE
   SAME ;

TRUSTED: AGREE-CELL ( -- )
   s" and an ordinary two-cell swap is unchanged" T-LABEL
   1 2 E-CELL GRAB2 SAVE
   1 2 C-CELL GRAB2 SAVE
   SAME ;

TRUSTED: AGREE-ENUM ( -- )
   s" and a one-cell enum is renamed the way the engine renames it" T-LABEL
   NRR-LAMP:BRIGHT 9 E-ENUM GRAB2 SAVE
   NRR-LAMP:BRIGHT 9 C-ENUM GRAB2 SAVE
   SAME ;

\ The CODE is asserted, not merely that something failed: a body refused for
\ running out of registers, or for naming a word the dialect cannot compile,
\ would satisfy "did not compile" while proving nothing about renames.
: RENAMES-OVER-A-VALUE-REFUSE ( -- )
   s" swap across a value named by the signature is refused by name" T-LABEL
   RC-SWAP @ E-NELAB-BUNDLE T=
   s" and so is a deeper rename that reaches one of its cells" T-LABEL
   RC-ROT @ E-NELAB-BUNDLE T=
   s" a value a generated constructor left is the same value" T-LABEL
   RC-CTOR @ E-NELAB-BUNDLE T=
   s" and so is one an ordinary word declared it leaves" T-LABEL
   RC-USER @ E-NELAB-BUNDLE T= ;

: BODIES-THAT-NEVER-BROKE-STILL-COMPILE ( -- )
   s" a value held across a body that renames nothing still compiles" T-LABEL
   RC-HOLD @ 0 T=
   s" a value handed straight back still compiles" T-LABEL
   RC-PASS @ 0 T=
   s" a value made and left alone still compiles" T-LABEL
   RC-MADE @ 0 T=
   s" a value carried across a loop edge still compiles" T-LABEL
   RC-LOOP @ 0 T=
   s" two INDEPENDENT variables read as one value's two cells would still compile" T-LABEL
   RC-VARS @ 0 T=
   s" and an ordinary two-cell rename is untouched" T-LABEL
   RC-CELL @ 0 T=
   s" a closed enum's value is ONE cell, so renaming across it is not refused" T-LABEL
   RC-ENUM @ 0 T= ;

public

: MAIN ( -- )
   T-RESET
   RENAMES-OVER-A-VALUE-REFUSE
   BODIES-THAT-NEVER-BROKE-STILL-COMPILE
   AGREE-HOLD
   AGREE-PASS
   AGREE-MADE
   AGREE-LOOP
   AGREE-VARS
   AGREE-CELL
   AGREE-ENUM
   T-REPORT
   s" native-rename-rows: ok" type cr ;

;package

NRR:MAIN
