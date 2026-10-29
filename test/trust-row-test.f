\ trust-row-test.f - a `s" NAME" s" SIG" trust` row is refused at the row unless
\ the engine wordlist its record lands in holds NAME (src/core/checker.f
\ TRUST-RESOLVES?: the open section's, the global one outside a package, or
\ PKG's public one for PKG:TAIL, through `search-wl`). Each case evaluates one
\ source string, so the code returned is that string's and the ordered cases
\ show a following statement never ran.

require src/lib/type/deftype.f               \ DEFTYPE - a nominal a raw type variable refuses
require src/lib/test.f

\ The cases stay at top level: several open a package inside the evaluated
\ source, which must happen in top-level scope.
package TRUST-ROW
private

T-RESET

\ TCE-CATCH answers the string's throw code, 0 when accepted. The row under test
\ is last in the string, so a nonzero code is the row's.
TYPED-VARIABLE TCE-A ptr u8   variable TCE-U
: TCE-GO ( -- )  TCE-A @ TCE-U @ INCLUDE-EVALUATE ;

public

: TCE-CATCH ( ptr u8 n -- n )  TCE-U ! TCE-A !  [: TCE-GO ;] catch ;

\ The codes live in the package because a global `E-*` constant is
\ src/lib/errors.f's surface alone.
70 constant E-REJECT            \ E-UNDEFINED / checker rejection
E-TRUST-UNRESOLVED constant E-STALE   \ the row names no word here
E-BAD-STORED-SIGNATURE constant E-BAD-SIG   \ the row's signature does not parse

;package

\ A row for a word the engine resolves is accepted; a row for a name it does not
\ is refused with its own code, not as a later symptom.
s" : TRW-LIVE ( -- n ) 5 ;" TRUST-ROW:TCE-CATCH 0 T=
s\" s\q TRW-LIVE\q s\q -- n\q trust" TRUST-ROW:TCE-CATCH 0 T=
s\" s\q TRW-NO-SUCH-WORD\q s\q -- n\q trust" TRUST-ROW:TCE-CATCH TRUST-ROW:E-STALE T=

\ A word defined one statement earlier resolves: the engine publishes a record
\ at a point the definition's own publish tail cannot see, which is why the
\ definer-facing registrar is a different word, and every row in
\ src/os/env-base.f sits directly under its definition.
s\" : TRW-JUST ( -- n ) 6 ; s\q TRW-JUST\q s\q -- n\q trust" TRUST-ROW:TCE-CATCH 0 T=

\ A negative length, or one that runs the name past the top of the address
\ range, names no word even where its bytes begin a live one. It is refused
\ before the name is read: the maximum cell would run the colon scan off the end
\ of the string. `trust-raw` and `trust-decl` refuse it the same way; source
\ cannot run those internal engine words, so a TRUSTED: body calls them, as the
\ engine's own callers do.
s\" s\q TRW-LIVE\q drop -1 s\q -- n\q trust" TRUST-ROW:TCE-CATCH TRUST-ROW:E-STALE T=
s\" s\q TRW-LIVE\q drop -1 1 rshift s\q -- n\q trust" TRUST-ROW:TCE-CATCH TRUST-ROW:E-STALE T=
s\" TRUSTED: TRW-NR1 ( -- ) s\q TRW-LIVE\q drop -1 s\q -- n\q trust-raw ; TRW-NR1"
   TRUST-ROW:TCE-CATCH TRUST-ROW:E-STALE T=
s\" TRUSTED: TRW-ND1 ( -- ) s\q TRW-LIVE\q drop -1 s\q -- n\q trust-decl ; TRW-ND1"
   TRUST-ROW:TCE-CATCH TRUST-ROW:E-STALE T=
s\" TRUSTED: TRW-NR2 ( -- ) s\q TRW-LIVE\q drop -1 1 rshift s\q -- n\q trust-raw ; TRW-NR2"
   TRUST-ROW:TCE-CATCH TRUST-ROW:E-STALE T=
s\" TRUSTED: TRW-ND2 ( -- ) s\q TRW-LIVE\q drop -1 1 rshift s\q -- n\q trust-decl ; TRW-ND2"
   TRUST-ROW:TCE-CATCH TRUST-ROW:E-STALE T=

\ A signature length that describes no memory gets the refusal of a signature
\ that does not parse, before a byte of it is read or printed: the maximum cell
\ would read on past the text. test/gate-diagnostics-lib.f
\ SIGNATURE-NOSPAN-RECORD renders the refusal of -1. `trust-raw`, called from a
\ TRUSTED: body, shares the registration and its refusal.
s\" s\q TRW-LIVE\q s\q -- n\q drop -1 1 rshift trust" TRUST-ROW:TCE-CATCH TRUST-ROW:E-BAD-SIG T=
s\" TRUSTED: TRW-NS1 ( -- ) s\q TRW-LIVE\q s\q -- n\q drop -1 1 rshift trust-raw ; TRW-NS1"
   TRUST-ROW:TCE-CATCH TRUST-ROW:E-BAD-SIG T=

\ `trust-raw` parses in raw-definer mode, and the refusal above left it by a
\ throw. Raw mode ends with the registration however the registration ends: left
\ on, it would seal the type variables of every later signature, and the
\ identity below would refuse a nominal value.
s" DEFTYPE TRWSER : TRW-PASS ( a -- a ) ; : TRW-PASS-SER ( trwser -- trwser ) TRW-PASS ;"
   TRUST-ROW:TCE-CATCH 0 T=

\ Fixtures put the name where a text match would find it and the dictionary
\ would not; the mirror cases must be accepted.
\ The name in a comment: nothing defines it, so the row is refused; a check that
\ scanned source text for the spelling would accept it.
s\" \\ TRW-COMMENT-ONLY is named here and nowhere else\n s\q TRW-COMMENT-ONLY\q s\q -- n\q trust" TRUST-ROW:TCE-CATCH TRUST-ROW:E-STALE T=

\ The name inside a string literal that is not the row's name string: refused
\ the same way.
s\" : TRW-HOLDER ( -- ptr u8 n ) s\q TRW-IN-A-STRING\q ;\n s\q TRW-IN-A-STRING\q s\q -- n\q trust" TRUST-ROW:TCE-CATCH TRUST-ROW:E-STALE T=

\ The name as the row's signature string: the signature is text the parser
\ reads, never a word, so the row is judged on the name it carries, which does
\ not exist either.
s\" s\q TRW-WRONG-ROLE\q s\q TRW-LIVE -- n\q trust" TRUST-ROW:TCE-CATCH TRUST-ROW:E-STALE T=

\ Defined and then retired: the word existed when the row was written and does
\ not now.
s" : TRW-GONE ( -- n ) 7 ;" TRUST-ROW:TCE-CATCH 0 T=
s\" s\q TRW-GONE\q s\q -- n\q trust" TRUST-ROW:TCE-CATCH 0 T=
s" undefine TRW-GONE" TRUST-ROW:TCE-CATCH 0 T=
s\" s\q TRW-GONE\q s\q -- n\q trust" TRUST-ROW:TCE-CATCH TRUST-ROW:E-STALE T=

\ The same row twice. The second is judged exactly like the first,
\ so a row is never accepted because an earlier one was.
s\" s\q TRW-DUP-NONE\q s\q -- n\q trust" TRUST-ROW:TCE-CATCH TRUST-ROW:E-STALE T=
s\" s\q TRW-DUP-NONE\q s\q -- n\q trust" TRUST-ROW:TCE-CATCH TRUST-ROW:E-STALE T=

\ A package's own private and public words, each named from its own section
\ while the package is open, are accepted, or a row inside a package body would
\ be refused.
s\" package TRWA : TRWA-PRIV ( -- n ) 8 ; s\q TRWA-PRIV\q s\q -- n\q trust ;package"
   TRUST-ROW:TCE-CATCH 0 T=
s\" package TRWB public : TRWB-PUB ( -- n ) 9 ; s\q TRWB-PUB\q s\q -- n\q trust ;package"
   TRUST-ROW:TCE-CATCH 0 T=

\ The same public word as a bare tail from outside, its package closed: the
\ engine does not reach a closed package's publics by a bare tail, so the row is
\ refused. Believed, it would record a bare global symbol that a later `using`
\ reports as E-USING-SHADOW-GLOBAL.
s\" s\q TRWB-PUB\q s\q -- n\q trust" TRUST-ROW:TCE-CATCH TRUST-ROW:E-STALE T=

\ PKG:TAIL records its effect on PKG's public symbol, so PKG's public wordlist
\ must hold TAIL, asked through the package's namespace row as the engine's FIND
\ resolves the spelling. The closed package's public is accepted.
s\" s\q TRWB:TRWB-PUB\q s\q -- n\q trust" TRUST-ROW:TCE-CATCH 0 T=
\ A package that does not exist, before a tail only the global wordlist holds:
\ no package is no wordlist, never the global one.
s\" s\q TRW-NO-SUCH-PKG:TRW-LIVE\q s\q -- n\q trust" TRUST-ROW:TCE-CATCH TRUST-ROW:E-STALE T=
\ A real package that defines no such tail.
s\" s\q TRWB:TRWB-NONE\q s\q -- n\q trust" TRUST-ROW:TCE-CATCH TRUST-ROW:E-STALE T=
\ A real package's private word: a qualified name never reaches a private
\ wordlist, so the row names no word, as a missing tail does.
s\" s\q TRWA:TRWA-PRIV\q s\q -- n\q trust" TRUST-ROW:TCE-CATCH TRUST-ROW:E-STALE T=
\ A real package qualifying a global's name: the global wordlist holds TRW-LIVE,
\ TRWB's public one does not, and only the latter is asked.
s\" s\q TRWB:TRW-LIVE\q s\q -- n\q trust" TRUST-ROW:TCE-CATCH TRUST-ROW:E-STALE T=

\ The bad row is followed in the same source by a definition refused for another
\ reason (E-UNDEFINED on a word nothing defines). A row believed and failing
\ later would answer that code; E-STALE shows the row stopped it.
s\" s\q TRW-ORDER-CHECK\q s\q -- n\q trust\n : TRW-AFTER ( -- n ) TRW-NEVER-DEFINED ;" TRUST-ROW:TCE-CATCH TRUST-ROW:E-STALE T=

\ The control: without the row the same definition answers the other code, so
\ the case above tells apart two outcomes that both exist.
s" : TRW-AFTER2 ( -- n ) TRW-NEVER-DEFINED ;" TRUST-ROW:TCE-CATCH TRUST-ROW:E-REJECT T=

\ A bare row records its effect in the open section's wordlist, or the global
\ one outside a package, so that wordlist must hold the name. TRW-G7 is a
\ global: inside TRWC the engine still resolves it, but a row there would record
\ onto a TRWC-private symbol the engine never defined, and checked callers in
\ TRWC would be certified against the row while the engine runs the global.
\ `trust` asks the wordlist and names the stale row; the registrars TRUST-RAW
\ and TRUST-DECL ask nothing and are internal engine words source cannot run (rc
\ 70).
s" : TRW-G7 ( -- n ) 7 ;" TRUST-ROW:TCE-CATCH 0 T=
s\" package TRWC\ns\q TRW-G7\q s\q n n -- n\q trust\n;package"
   TRUST-ROW:TCE-CATCH TRUST-ROW:E-STALE T=
s\" package TRWC\ns\q TRW-G7\q s\q -- ptr n\q TRUST-RAW\n;package"
   TRUST-ROW:TCE-CATCH TRUST-ROW:E-REJECT T=
s\" package TRWC\ns\q TRW-G7\q s\q -- ptr n\q TRUST-DECL\n;package"
   TRUST-ROW:TCE-CATCH TRUST-ROW:E-REJECT T=
\ None of them left a row behind: the bare name in TRWC is still the global.
s\" package TRWC\npublic\n: TRWC-USE ( -- n ) TRW-G7 ;\n;package" TRUST-ROW:TCE-CATCH 0 T=
s\" : TRWC-CK ( -- ) TRWC:TRWC-USE 7 <> if s\q a refused row bound the name\q 1 die then ;\nTRWC-CK"
   TRUST-ROW:TCE-CATCH 0 T=

\ A word a called word renders is in the wordlist when the row after the call
\ runs, so the row is accepted and the same row misspelt is refused: the
\ judgment `tools/check.f --verify-only` leaves to the load for a row under a
\ rendering statement's mark (src/core/checker.f CHECKER-VERIFY-TRUST).
s\" : TRW-REN ( -- ) s\q : TRW-MADE ( -- n ) 1 ;\q evaluate-closed ; TRW-REN" TRUST-ROW:TCE-CATCH 0 T=
s\" s\q TRW-MADE\q s\q -- n\q trust" TRUST-ROW:TCE-CATCH 0 T=
s\" s\q TRW-MAED\q s\q -- n\q trust" TRUST-ROW:TCE-CATCH TRUST-ROW:E-STALE T=

T-REPORT
