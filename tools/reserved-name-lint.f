\ reserved-name-lint.f - CLI wrapper for reserved definition-name lint.
\ Standalone: it loads its own dependencies (test/lint-cli-standalone-load.f).
\ tools/check.f runs the same lint (check-core.f CHK-RUN-RESERVED-NAMES) before
\ spawning the checker child, so a reserved name (a prefix-stripping rename makes
\ one: `CC-I` to `IX`) is E-RESERVED-DEFINITION with file, line and token
\ instead of a silent rc 70.

require src/lib/errors.f
require src/lib/string.f
require src/lib/memory.f
require src/lib/vector.f
require src/lib/fs.f
require tools/lint/text.f
require tools/lint/token.f
require tools/lint/lib.f
require src/lib/source-lex.f
require tools/reserved-name-lint-core.f
require src/lib/argv.f

package RESERVED-NAME-LINT-CLI
private

: ARGV-FILE ( n -- ) {: idx:n :}
   ARGV:LABEL? if
      idx ARGV:POS$ ARGV:LABEL$ RESERVED-NAME-LINT:FILE-AS
   else
      idx ARGV:POS$ RESERVED-NAME-LINT:FILE
   then ;

: RUN ( -- )
   s" tools/reserved-name-lint.f [--json] [--label name] file ..." ARGV:USAGE!
   ARGV:PARSE
   1 -1 ARGV:EXPECT-POS
   RESERVED-NAME-LINT:RESET
   ARGV:JSON? RESERVED-NAME-LINT:JSON!
   0 begin dup ARGV:POS# < while
      dup ARGV-FILE
      1+
   repeat drop
   RESERVED-NAME-LINT:FINISH ;

RUN

;package
