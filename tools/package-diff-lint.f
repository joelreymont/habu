\ package-diff-lint.f - CLI for exact-diff package ownership enforcement.
\ Run: bin/hb --load tools/package-diff-lint.f -- diff.patch ...

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/vector.f
require lib/fs.f
require tools/lint/text.f
require tools/lint/token.f
require tools/lint/lib.f
require tools/lint/source-lex.f
require tools/package-diff-lint-core.f
require lib/argv.f

package PACKAGE-DIFF-CLI
private

: RUN-CHECK ( -- )
   PACKAGE-DIFF:RESET
   0 begin dup ARGV:POS# < while
      dup ARGV:POS$ PACKAGE-DIFF:FILE
      1+
   repeat drop
   PACKAGE-DIFF:FINISH ;

: RUN ( -- )
   s" tools/package-diff-lint.f diff.patch ..." ARGV:USAGE!
   ARGV:PARSE
   1 -1 ARGV:EXPECT-POS
   [: RUN-CHECK ;] catch {: code:n :}
   s" package-diff-lint" code LINT-MAIN ;

RUN

;package
