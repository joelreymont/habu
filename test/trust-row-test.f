\ trust-row-test.f - what a bare `s" NAME" s" SIG" trust` row may assert
\ (dot habu-make-trust-refuse-cc8e19de).
\
\ A row is a CLAIM that a word exists, and it used to be believed without being
\ asked. A stale one - a word since renamed, deleted, or moved into a package -
\ recorded its effect against a bare GLOBAL checker symbol nothing could ever
\ call, and the damage surfaced two layers away at some later file's `using`, as
\ E-USING-SHADOW-GLOBAL against a package public that legitimately owned the
\ tail. src/core/checker.f TRUST-RESOLVES? now answers the claim from the
\ ENGINE's wordlist the row's record lands in (the open section's, or the
\ global one outside a package, read through `search-wl`) and refuses the row
\ where it is written.
\
\ WHY EVERY CASE RUNS THROUGH INCLUDE-EVALUATE. The row has to be REJECTED AT
\ THE ROW, which is a statement about WHEN the throw happens and not only that
\ one happens. Evaluating one source string at a time makes that observable: the
\ code comes back from the string that carried the row, and the ordered cases
\ below prove a following statement never ran.
\
\ WHAT THE FIXTURES ARE BUILT TO FOOL. The check reads the dictionary, so the
\ fixtures put the name everywhere text-matching would find it and the
\ dictionary would not: inside a comment, inside a string literal, as a
\ definition made and then `undefine`d, and as a bare tail owned by a package
\ that is closed at the row. Each of those must still be refused. The mirror
\ cases - a word defined one statement earlier, a package's own private and
\ public words while that package is open - must be accepted, or the refusal
\ would be rejecting live rows and the whole boot prefix with them.

require test/checker-assert.f
require lib/fmt.f                        \ FMT:.INT - one-line number text
require lib/type/deftype.f               \ DEFTYPE - a nominal a raw type variable refuses

\ The harness owns a package because this suite shares an in-process slice with
\ others that carry the same test vocabulary; a global `T=` here is a duplicate
\ definition (throw 78) before the first case runs. The CASES stay at top level:
\ several of them open a package inside the evaluated source, and that has to
\ happen in top-level scope to mean what it says.
package TRUST-ROW
private

variable #FAIL
variable #CASE

: T-FAIL ( -- )
   [char] F emit #CASE @ . ;

\ Evaluate one source string, returning its throw code (0 = accepted). The row
\ under test is the LAST thing in the string, so a non-zero code is the row's.
TYPED-VARIABLE TCE-A ptr u8   variable TCE-U
: TCE-GO ( -- )  TCE-A @ TCE-U @ INCLUDE-EVALUATE ;

public

: T= ( n n -- ) {: got:n want:n :}
   #CASE @ 1 + #CASE !
   got want <> if
      T-FAIL s" trust-row-test: expected " type want FMT:.INT s"  got " type got FMT:.INT cr
      #FAIL @ 1 + #FAIL !
   then ;

: TCE-CATCH ( ptr u8 n -- n )  TCE-U ! TCE-A !  [: TCE-GO ;] catch ;

\ The codes the cases below expect. They live in the package because a
\ global `E-*` constant is lib/errors.f's surface alone.
70 constant E-REJECT            \ E-UNDEFINED / checker rejection
E-TRUST-UNRESOLVED constant E-STALE   \ the row names no word here
E-BAD-STORED-SIGNATURE constant E-BAD-SIG   \ the row's signature does not parse

: REPORT ( -- )
   #FAIL @ 0 = if s" ok" type cr exit then
   #FAIL @ . s" trust-row-test: failures" 1 die ;

;package

\ --- the two halves of the rule ---------------------------------------------
\ A row for a word the engine resolves is accepted; a row for a name it does not
\ is refused, and refused with its own code rather than as a later symptom.
s" : TRW-LIVE ( -- n ) 5 ;" TRUST-ROW:TCE-CATCH 0 TRUST-ROW:T=
s\" s\q TRW-LIVE\q s\q -- n\q trust" TRUST-ROW:TCE-CATCH 0 TRUST-ROW:T=
s\" s\q TRW-NO-SUCH-WORD\q s\q -- n\q trust" TRUST-ROW:TCE-CATCH TRUST-ROW:E-STALE TRUST-ROW:T=

\ A word defined one statement earlier resolves. This is not a formality: the
\ engine publishes a record at a point the definition's OWN publish tail cannot
\ see, which is why the definer-facing registrar is a different word, and a
\ resolution rule that could not see a just-defined word would refuse most of
\ src/os/env-base.f - every row there sits directly under its definition.
s\" : TRW-JUST ( -- n ) 6 ; s\q TRW-JUST\q s\q -- n\q trust" TRUST-ROW:TCE-CATCH 0 TRUST-ROW:T=

\ --- a name that is no span -------------------------------------------------
\ A negative length, or one that would run the name past the top of the address
\ range, spells nothing, so the row names no word even where its bytes begin a
\ live one. It is refused before the name is read: the maximum cell ran the
\ colon scan off the end of the string until the process was killed.
\ `trust-raw` and `trust-decl` refuse it the same way: before, -1 recorded a row
\ under that length, and the maximum cell died folding the name. Source cannot
\ run those internal engine words, so a TRUSTED: body calls them, as the
\ engine's own callers do.
s\" s\q TRW-LIVE\q drop -1 s\q -- n\q trust" TRUST-ROW:TCE-CATCH TRUST-ROW:E-STALE TRUST-ROW:T=
s\" s\q TRW-LIVE\q drop -1 1 rshift s\q -- n\q trust" TRUST-ROW:TCE-CATCH TRUST-ROW:E-STALE TRUST-ROW:T=
s\" TRUSTED: TRW-NR1 ( -- ) s\q TRW-LIVE\q drop -1 s\q -- n\q trust-raw ; TRW-NR1"
   TRUST-ROW:TCE-CATCH TRUST-ROW:E-STALE TRUST-ROW:T=
s\" TRUSTED: TRW-ND1 ( -- ) s\q TRW-LIVE\q drop -1 s\q -- n\q trust-decl ; TRW-ND1"
   TRUST-ROW:TCE-CATCH TRUST-ROW:E-STALE TRUST-ROW:T=
s\" TRUSTED: TRW-NR2 ( -- ) s\q TRW-LIVE\q drop -1 1 rshift s\q -- n\q trust-raw ; TRW-NR2"
   TRUST-ROW:TCE-CATCH TRUST-ROW:E-STALE TRUST-ROW:T=
s\" TRUSTED: TRW-ND2 ( -- ) s\q TRW-LIVE\q drop -1 1 rshift s\q -- n\q trust-decl ; TRW-ND2"
   TRUST-ROW:TCE-CATCH TRUST-ROW:E-STALE TRUST-ROW:T=

\ --- a signature that is no span --------------------------------------------
\ The signature is text the parser reads, so a length that describes no memory
\ gets the refusal of a signature that does not parse, before a byte of it is
\ read or printed. Given the maximum cell, the parser read on past the text until
\ the process was killed, its memory past 2 GB. test/gate-diagnostics-lib.f
\ SIGNATURE-NOSPAN-RECORD renders the refusal of -1. `trust-raw`, called from a
\ TRUSTED: body, shares the registration, and with it the refusal.
s\" s\q TRW-LIVE\q s\q -- n\q drop -1 1 rshift trust" TRUST-ROW:TCE-CATCH TRUST-ROW:E-BAD-SIG TRUST-ROW:T=
s\" TRUSTED: TRW-NS1 ( -- ) s\q TRW-LIVE\q s\q -- n\q drop -1 1 rshift trust-raw ; TRW-NS1"
   TRUST-ROW:TCE-CATCH TRUST-ROW:E-BAD-SIG TRUST-ROW:T=

\ `trust-raw` parses in raw-definer mode, and the refusal above left it by a
\ throw. Raw mode ends with the registration however the registration ends: left
\ on, it would seal the type variables of every later signature, and the
\ identity below would refuse a nominal value.
s" DEFTYPE TRWSER : TRW-PASS ( a -- a ) ; : TRW-PASS-SER ( trwser -- trwser ) TRW-PASS ;"
   TRUST-ROW:TCE-CATCH 0 TRUST-ROW:T=

\ --- fixtures built to fool a text match ------------------------------------
\ The name in a COMMENT. Nothing defines it, so the row is refused; a check that
\ scanned source text for the spelling would accept it.
s\" \\ TRW-COMMENT-ONLY is named here and nowhere else\n s\q TRW-COMMENT-ONLY\q s\q -- n\q trust" TRUST-ROW:TCE-CATCH TRUST-ROW:E-STALE TRUST-ROW:T=

\ The name inside a STRING LITERAL that is not the row's name string. Same
\ verdict, same reason.
s\" : TRW-HOLDER ( -- ptr u8 n ) s\q TRW-IN-A-STRING\q ;\n s\q TRW-IN-A-STRING\q s\q -- n\q trust" TRUST-ROW:TCE-CATCH TRUST-ROW:E-STALE TRUST-ROW:T=

\ The name in the WRONG ROLE: it is the row's SIGNATURE string, not its name.
\ The signature is text the parser reads, never a word, so the row is judged on
\ the name it actually carries - which does not exist either.
s\" s\q TRW-WRONG-ROLE\q s\q TRW-LIVE -- n\q trust" TRUST-ROW:TCE-CATCH TRUST-ROW:E-STALE TRUST-ROW:T=

\ DEFINED AND THEN RETIRED, which is the stale row this dot is named for: the
\ word existed when the row was written and does not now.
s" : TRW-GONE ( -- n ) 7 ;" TRUST-ROW:TCE-CATCH 0 TRUST-ROW:T=
s\" s\q TRW-GONE\q s\q -- n\q trust" TRUST-ROW:TCE-CATCH 0 TRUST-ROW:T=
s" undefine TRW-GONE" TRUST-ROW:TCE-CATCH 0 TRUST-ROW:T=
s\" s\q TRW-GONE\q s\q -- n\q trust" TRUST-ROW:TCE-CATCH TRUST-ROW:E-STALE TRUST-ROW:T=

\ DUPLICATED: the same row twice. The second is judged exactly like the first,
\ so a row is never accepted because an earlier one was.
s\" s\q TRW-DUP-NONE\q s\q -- n\q trust" TRUST-ROW:TCE-CATCH TRUST-ROW:E-STALE TRUST-ROW:T=
s\" s\q TRW-DUP-NONE\q s\q -- n\q trust" TRUST-ROW:TCE-CATCH TRUST-ROW:E-STALE TRUST-ROW:T=

\ --- the open section's own words --------------------------------------------
\ A package's own private and public words, each named from its own section
\ while that package is OPEN, or a row inside a package body would be refused.
s\" package TRWA : TRWA-PRIV ( -- n ) 8 ; s\q TRWA-PRIV\q s\q -- n\q trust ;package"
   TRUST-ROW:TCE-CATCH 0 TRUST-ROW:T=
s\" package TRWB public : TRWB-PUB ( -- n ) 9 ; s\q TRWB-PUB\q s\q -- n\q trust ;package"
   TRUST-ROW:TCE-CATCH 0 TRUST-ROW:T=

\ The same public word named as a BARE TAIL from outside, with its package
\ CLOSED. The engine does not reach a closed package's publics by a bare tail,
\ so the row is refused - which is exactly the shape that used to mint the bare
\ global symbol that collided with a later `using`.
s\" s\q TRWB-PUB\q s\q -- n\q trust" TRUST-ROW:TCE-CATCH TRUST-ROW:E-STALE TRUST-ROW:T=

\ A QUALIFIED spelling is accepted, and this case exists to pin that as a STATED
\ GAP rather than a silent one. `search-wl` answers per wordlist on the raw
\ spelling and a closed package's publics live in none it can reach, so PKG:TAIL
\ has no resolver in the boot prefix yet (dot habu-resolve-qualified-trust-2826761a).
\ When that lands, this case changes verdict and says so.
s\" s\q TRWB:TRWB-PUB\q s\q -- n\q trust" TRUST-ROW:TCE-CATCH 0 TRUST-ROW:T=
\ ... including for a package that does not exist at all, which is the whole of
\ what the gap costs today.
s\" s\q TRW-NO-SUCH-PKG:TAIL\q s\q -- n\q trust" TRUST-ROW:TCE-CATCH 0 TRUST-ROW:T=

\ --- at the row, not downstream ---------------------------------------------
\ The verdict has to be the ROW's. Here the bad row is followed, in the same
\ evaluated source, by a definition that would itself be refused for an
\ unrelated reason (E-UNDEFINED on a word nothing defines). If the row were
\ believed and the failure only surfaced later, the code coming back would be
\ that later one; it is E-STALE, so the row stopped it.
s\" s\q TRW-ORDER-CHECK\q s\q -- n\q trust\n : TRW-AFTER ( -- n ) TRW-NEVER-DEFINED ;" TRUST-ROW:TCE-CATCH TRUST-ROW:E-STALE TRUST-ROW:T=

\ And the control for that case: with the row removed, the SAME following
\ definition really does produce the other code, so the case above distinguishes
\ two outcomes that both exist rather than one that cannot happen.
s" : TRW-AFTER2 ( -- n ) TRW-NEVER-DEFINED ;" TRUST-ROW:TCE-CATCH TRUST-ROW:E-REJECT TRUST-ROW:T=

\ --- the wordlist the record lands in ---------------------------------------
\ A bare row records its effect in the open section's wordlist, or the global
\ one outside a package, so that wordlist is the one that must hold the name.
\ TRW-G7 is a global: inside TRWC the engine still resolves it, but a row there
\ would record onto a TRWC-private symbol the engine never defined, and checked
\ callers in TRWC would be certified against the row while the engine runs the
\ global. Each source form of the row is refused: `trust` asks the wordlist and
\ names the stale row, and the engine's registrars TRUST-RAW and TRUST-DECL,
\ which ask nothing, are internal engine words source cannot run (rc 70).
s" : TRW-G7 ( -- n ) 7 ;" TRUST-ROW:TCE-CATCH 0 TRUST-ROW:T=
s\" package TRWC\ns\q TRW-G7\q s\q n n -- n\q trust\n;package"
   TRUST-ROW:TCE-CATCH TRUST-ROW:E-STALE TRUST-ROW:T=
s\" package TRWC\ns\q TRW-G7\q s\q -- ptr n\q TRUST-RAW\n;package"
   TRUST-ROW:TCE-CATCH TRUST-ROW:E-REJECT TRUST-ROW:T=
s\" package TRWC\ns\q TRW-G7\q s\q -- ptr n\q TRUST-DECL\n;package"
   TRUST-ROW:TCE-CATCH TRUST-ROW:E-REJECT TRUST-ROW:T=
\ None of them left a row behind: the bare name in TRWC is still the global.
s\" package TRWC\npublic\n: TRWC-USE ( -- n ) TRW-G7 ;\n;package" TRUST-ROW:TCE-CATCH 0 TRUST-ROW:T=
s\" : TRWC-CK ( -- ) TRWC:TRWC-USE 7 <> if s\q a refused row bound the name\q 1 die then ;\nTRWC-CK"
   TRUST-ROW:TCE-CATCH 0 TRUST-ROW:T=

\ ---------------------------------------------------------------------------
TRUST-ROW:REPORT
