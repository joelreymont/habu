\ typed-local-diff-lint.f - CLI wrapper for added bare-local lint.
\ Load after tools/typed-local-diff-lint-core.f and tools/argv.f.

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/vector.f
require lib/fs.f
require tools/lint/text.f
require tools/lint/token.f
require tools/lint/lib.f
require tools/lint/source-lex.f
require tools/typed-local-diff-lint-core.f
require tools/argv.f

: TYPED-LOCAL-DIFF-LINT ( -- )
   s" tools/typed-local-diff-lint.f diff.patch ..." ARGV-USAGE!
   ARGV-PARSE
   1 -1 ARGV-EXPECT-POS
   TYPED-LOCAL-DIFF-LINT-RESET
   0 begin dup ARGV-POS# < while
      dup ARGV-POS$ TYPED-LOCAL-DIFF-LINT-FILE
      1+
   repeat drop
   TYPED-LOCAL-DIFF-LINT-FINISH ;

TYPED-LOCAL-DIFF-LINT
