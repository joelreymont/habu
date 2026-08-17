\ sig-scope-intake.f - a seeded signature is parsed in the ROW'S package.
\
\ WHAT SIGSCOPE IS. A signature's bare family tails resolve in the package the
\ signature was WRITTEN in. Normally that is the package of the definition being
\ checked, and src/core/checker.f SIG-SCOPE$ answers exactly that. The one other
\ case is the AOT intake: CK-AOT-TAKE re-parses a signature the capture stored,
\ and that text was written and certified in the ROW's package - so the intake
\ sets SIGSCOPE to the row's package for the length of the parse. Without it the
\ text is read in whatever package the CALLER happens to have open, and a bare
\ tail resolves to the caller's family or to nothing.
\
\ WHY IT NEEDED A FIXTURE. `SIGSCOPE` occurs in one file and nothing in the tree
\ redded its deletion. The reason is the generators: src/core/sumtype.f
\ TDGEN-FAM-REF renders a packaged family as `PKG:tail`, so no generated row can
\ exercise the rule. Measured over the whole of src/ (2026-08-17), exactly ONE
\ hand-written declared signature names its own package's family bare, and it is
\ the subject below: src/compiler/native/elaborate.f
\
\   : SPLICE-STAGING ( HIR:meaning -- staging )
\
\ in package NELAB, exported public, and inside the captured compiler chain - so
\ the shipped engine's baked pool carries that text, bare tail and all. The
\ population is one row, and this file is what holds it there.
\
\ THE CASE IS A FORGE, NOT A CALL. A caller that merely certifies proves the
\ intake happened, not the scope it parsed in. So the caller lives in a package
\ that declares its OWN public family named `staging` - the same tail, a
\ different family - and the two probes below are two-sided:
\
\   declaring the result NELAB:staging  must CERTIFY   (the row parsed in NELAB)
\   declaring the result bare `staging` must be REFUSED (it is NOT the forge's)
\
\ Parse the stored text in the intake's scope instead of the row's and both flip:
\ the bare tail becomes the forge's own family, the second probe certifies and
\ the first is refused "expected: nelab:staging<> actual: staging<>". The refusal
\ this file prints as it loads is the second probe's own evidence, named in full:
\ `expected: staging<> actual: nelab:staging<>`.
\
\ AND THE SUBJECT IS READ WHERE IT IS DECLARED, so this file cannot go quietly
\ vacuous. Qualify that one signature and the forge stops discriminating while
\ every probe still passes - the rule would be untested again with nothing to
\ say so. The source read below is structural, through the same lexer
\ package-diff-lint reads Habu with: a definition is `:` then the name then the
\ signature, in those roles, and the fixtures at the foot carry the same
\ spelling in a comment, in a string, in the wrong role and under a longer name
\ so that none of them can pass for one.
\
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f test/sig-scope-intake.f

require lib/errors.f
require lib/string.f
require lib/test.f
require tools/lint/text.f
require tools/lint/token.f
require tools/lint/lib.f
require tools/lint/source-lex.f

package SIGSCOPE-TEST
private

: SUBJECT-FILE$ ( -- ptr u8 n ) s" src/compiler/native/elaborate.f" ;
: SUBJECT-PKG$ ( -- ptr u8 n ) s" NELAB" ;
: SUBJECT-NAME$ ( -- ptr u8 n ) s" SPLICE-STAGING" ;
: BARE-TAIL$ ( -- ptr u8 n ) s" staging" ;
: QUAL-TAIL$ ( -- ptr u8 n ) s" NELAB:staging" ;

\ Does the checker hold an effect for the seeded word? False until an intake
\ takes its row: a chain word is in the shipped engine's DICTIONARY from the
\ seed, and in the checker's tables only when a body names it.
: SUBJECT-KNOWN? ( -- bool )
   SUBJECT-PKG$ true SUBJECT-NAME$ CHECKER-ASIG-KNOWN? ;

: FLAG>N ( bool -- n ) IF -1 ELSE 0 THEN ;

variable KNOWN-BEFORE
variable GOOD-V                    \ the qualified-result probe's verdict
variable DECOY-V                   \ the bare-result probe's verdict

\ ---- reading the declaration where it is written -----------------------------
\ The lexer keeps `( ... )` as a token of its own with the body in CONTENT, so a
\ declaration is three adjacent tokens in three roles: the word `:`, the name,
\ and the signature. Anything spelled the same in a comment, a string or another
\ order is not one, which is what the fixtures at the foot check.

variable DECLS                     \ how many declarations of the name the walk saw
variable BARE-HITS                 \ ... and how many bare tails the first one named
variable QUAL-HITS                 \ ... and how many qualified ones
variable SI  variable SS  variable SU
variable SIG-A

: SIG-A-FIELD ( -- ptr ptr u8 ) SIG-A 0 ptr-field ;
: SIG-A@ ( -- ptr u8 ) SIG-A-FIELD @ ;
: SIG-A! ( ptr u8 -- ) SIG-A-FIELD ! ;

: WORD? ( n -- bool ) LINT-LEX:KIND@ LINT-LEX:WORD = ;
: COMMENT? ( n -- bool ) LINT-LEX:KIND@ LINT-LEX:COMMENT = ;

: TOK= ( n ptr u8 n -- bool ) {: k:n a:ptr u:n :}
   k WORD? 0= IF LINT-FALSE EXIT THEN
   k LINT-LEX:TOKEN a u STR= ;

\ One signature body, split on spaces, counting the two spellings of the tail.
: COUNT-TAILS ( ptr u8 n -- ) {: a:ptr u:n :}
   a SIG-A!  u SU !
   0 SI !
   BEGIN SI @ SU @ < WHILE
      BEGIN SI @ SU @ < SIG-A@ SI @ + c@ 32 = and WHILE SI @ 1 + SI ! REPEAT
      SI @ SS !
      BEGIN SI @ SU @ < SIG-A@ SI @ + c@ 32 <> and WHILE SI @ 1 + SI ! REPEAT
      SI @ SS @ > IF
         SIG-A@ SS @ + SI @ SS @ - {: ta:ptr tu:n :}
         ta tu BARE-TAIL$ STR= IF BARE-HITS @ 1 + BARE-HITS ! THEN
         ta tu QUAL-TAIL$ STR= IF QUAL-HITS @ 1 + QUAL-HITS ! THEN
      THEN
   REPEAT ;

\ Answer the FIRST declaration and count them all: two declarations of one name
\ are two authorities, and taking either would hide the other.
\ The bound is guarded because a fixture may lex to nothing: `?do` with a limit
\ below its start does not skip, it wraps, and a source whose every token is
\ inside a comment is exactly such a fixture.
: SCAN ( -- )
   0 DECLS !  0 BARE-HITS !  0 QUAL-HITS !
   LINT-LEX:COUNT 3 < IF EXIT THEN
   LINT-LEX:COUNT 2 - 0 ?do
      i s" :" TOK= if
         i 1 + SUBJECT-NAME$ TOK= if
            i 2 + COMMENT? if
               DECLS @ 0= if i 2 + LINT-LEX:CONTENT COUNT-TAILS then
               DECLS @ 1 + DECLS !
            then
         then
      then
   loop ;

: LEX-FIXTURE ( ptr u8 n -- ) {: a:ptr u:n :}
   a u LINT-LEX:SOURCE
   s" the fixture lexes without a diagnostic" T-LABEL
   LINT-LEX:ERROR? 0= TTRUE
   SCAN ;

\ ---- case one: the seeded row is parsed in NELAB, not in the caller ----------

: INTAKE-CASE ( -- )
   s" the checker did not already hold the seeded word's effect" T-LABEL
   KNOWN-BEFORE @ 0 T=
   s" ... and holds it once a body named it: the intake ran" T-LABEL
   SUBJECT-KNOWN? TTRUE

   s" a caller declaring NELAB:staging certifies" T-LABEL
   GOOD-V @ -1 T=
   s" ... and one declaring the forge's own bare `staging` is refused" T-LABEL
   DECOY-V @ 0 T= ;

\ ---- case two: the subject still carries the bare tail -----------------------

: SUBJECT-CASE ( -- )
   SUBJECT-FILE$ LINT-SOURCE:LOAD
   LINT-SOURCE:TEXT LINT-LEX:SOURCE
   s" the subject source lexes without a diagnostic" T-LABEL
   LINT-LEX:ERROR? 0= TTRUE
   SCAN
   s" elaborate.f declares SPLICE-STAGING exactly once" T-LABEL
   DECLS @ 1 T=
   s" ... and its signature names the family tail BARE" T-LABEL
   BARE-HITS @ 1 T=
   s" ... and names no qualified spelling of it" T-LABEL
   QUAL-HITS @ 0 T= ;

\ ---- case three: the reader cannot be fooled ---------------------------------

: DECOY-CASE ( -- )
   s\" \\ : SPLICE-STAGING ( HIR:meaning -- staging )\n" LEX-FIXTURE
   s" a declaration inside a line comment is not a declaration" T-LABEL
   DECLS @ 0 T=

   s\" : Y ( -- ptr u8 n ) s\" : SPLICE-STAGING ( HIR:meaning -- staging )\" ;\n"
   LEX-FIXTURE
   s" ... and neither is one inside a string literal" T-LABEL
   DECLS @ 0 T=

   s\" ( HIR:meaning -- staging ) : SPLICE-STAGING\n" LEX-FIXTURE
   s" the three tokens in the wrong order do not declare it" T-LABEL
   DECLS @ 0 T=

   s\" : SPLICE-STAGING-TWIN ( HIR:meaning -- staging )\n" LEX-FIXTURE
   s" a longer name that starts with the wanted one is a different name" T-LABEL
   DECLS @ 0 T=

   s\" : SPLICE-STAGING HIR:meaning\n" LEX-FIXTURE
   s" a name with no signature after it is not a declaration" T-LABEL
   DECLS @ 0 T=

   s\" : SPLICE-STAGING ( HIR:meaning -- staging )\n" LEX-FIXTURE
   s" a real declaration IS read, and its bare tail counted" T-LABEL
   DECLS @ 1 T=
   BARE-HITS @ 1 T=

   s\" : SPLICE-STAGING ( HIR:meaning -- NELAB:staging )\n" LEX-FIXTURE
   s" a qualified tail is not a bare one" T-LABEL
   DECLS @ 1 T=
   BARE-HITS @ 0 T=
   QUAL-HITS @ 1 T=

   s\" : SPLICE-STAGING ( -- staging )\n: SPLICE-STAGING ( -- staging )\n"
   LEX-FIXTURE
   s" two declarations of one name are two authorities and are counted" T-LABEL
   DECLS @ 2 T= ;

public

\ Latched before anything in this process can have named the seeded word.
: BEFORE! ( -- ) SUBJECT-KNOWN? FLAG>N KNOWN-BEFORE ! ;

: GOOD! ( n -- ) GOOD-V ! ;
: DECOY! ( n -- ) DECOY-V ! ;

: RUN ( -- )
   INTAKE-CASE
   SUBJECT-CASE
   DECOY-CASE
   T-REPORT
   s" sig-scope-intake: ok" type cr ;

;package

SIGSCOPE-TEST:BEFORE!

\ The forge. Its `staging` is a real public family of this package with the same
\ tail the seeded row names bare, so the two probes below answer which package
\ that text was read in - and nothing else can answer it.
package SIGSCOPE-FORGE
public

ENUM staging DERIVE eq
   decoy-one
   decoy-two
;ENUM

s" SS-QUALIFIED ( HIR:meaning -- NELAB:staging ) NELAB:SPLICE-STAGING"
   CHECK! SIGSCOPE-TEST:GOOD!
s" SS-BARE ( HIR:meaning -- staging ) NELAB:SPLICE-STAGING"
   CHECK! SIGSCOPE-TEST:DECOY!

;package

SIGSCOPE-TEST:RUN
