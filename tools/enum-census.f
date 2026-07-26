\ enum-census.f - CLI for the plain-ENUM declaration census.
\
\   bin/hb --load tools/enum-census.f -- verify        (default: compare the
\                                                       tree against the
\                                                       committed baseline)
\   bin/hb --load tools/enum-census.f -- record PATH   (write a fresh baseline)
\
\ `record` exists so the baseline can be taken on a chosen commit - it was taken
\ on the commit before the global ENUM keyword moved to the unified front end,
\ which is what makes `verify` a before/after parity check. Re-recording is a
\ deliberate act: it discards the comparison, so it takes an explicit path and
\ never overwrites the committed baseline by default.
\
\ See tools/enum-census-core.f for what a site is and how each one is replayed.

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/vector.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require tools/lint/text.f
require tools/lint/token.f
require tools/lint/lib.f
require tools/lint/source-lex.f
require tools/enum-census-core.f
require lib/argv.f

package ENUM-CENSUS-CLI
private

\ Verification is two claims, and both have to hold. First: the keyword really is
\ the unified front end and nothing else answers for it. Second: every plain
\ declaration in the tree registers exactly what it registered before the keyword
\ moved. Either one alone is satisfiable by a tree nobody wants. The whole
\ sequence lives in the core (ENUM-CENSUS:VERIFY-COMMITTED) so the native gate
\ can run it without this file's argv parse.
: DO-VERIFY ( -- )
   ENUM-CENSUS:VERIFY-COMMITTED ;

\ Recording disarms the second-parser tripwire: a baseline is taken on the tree
\ BEFORE the cutover, where the legacy parser words are still present and are
\ supposed to be. See ENUM-CENSUS:TRIPWIRE-OFF!.
: DO-RECORD ( ptr u8 n -- ) {: path:ptr pathu:n :}
   ENUM-CENSUS:RESET
   ENUM-CENSUS:TRIPWIRE-OFF!
   ENUM-CENSUS:WALK
   ENUM-CENSUS:SUMMARY
   ENUM-CENSUS:REQUIRE-CLEAN
   path pathu ENUM-CENSUS:REPORT$ WRITE-ALL
   s" enum-census: baseline written to " type path pathu type cr ;

: VERB$ ( -- ptr u8 n ) 0 ARGV:POS$ ;

: EXEC ( -- )
   s" tools/enum-census.f [verify | record PATH]" ARGV:USAGE!
   ARGV:PARSE
   ARGV:POS# 0= IF DO-VERIFY EXIT THEN
   VERB$ s" verify" STR= IF
      1 1 ARGV:EXPECT-POS DO-VERIFY EXIT THEN
   VERB$ s" record" STR= IF
      2 2 ARGV:EXPECT-POS 1 ARGV:POS$ DO-RECORD EXIT THEN
   s" expected: verify, or record PATH" ARGV:FAIL ;

public

: MAIN ( -- ) EXEC ;

;package

ENUM-CENSUS-CLI:MAIN
