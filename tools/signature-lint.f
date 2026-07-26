\ signature-lint.f - CLI wrapper for strict typed-signature lint.
\ This is a standalone entry point: it loads its own dependencies, so
\ `bin/hb --load tools/signature-lint.f` works on its own. It used to only list
\ them in a comment - and that list still named tools/argv.f, which no longer
\ exists; the argv module lives in lib/argv.f.

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/vector.f
require lib/fs.f
require tools/lint/text.f
require tools/lint/token.f
require tools/lint/lib.f
require tools/lint/json-writer.f
require tools/lint/source-lex.f
require tools/signature-lint-core.f
require lib/argv.f

package SIGNATURE-LINT-CLI
private

: ARGV-FILE ( n -- ) {: idx:n :}
   ARGV:LABEL? IF
      idx ARGV:POS$ ARGV:LABEL$ SIGNATURE-LINT:FILE-AS
   ELSE
      idx ARGV:POS$ SIGNATURE-LINT:FILE
   THEN ;

: RUN ( -- )
   s" tools/signature-lint.f [--json] [--label name] file ..." ARGV:USAGE!
   ARGV:PARSE
   1 -1 ARGV:EXPECT-POS
   SIGNATURE-LINT:RESET
   ARGV:JSON? SIGNATURE-LINT:JSON!
   0 begin dup ARGV:POS# < while
      dup ARGV-FILE
      1+
   repeat drop
   SIGNATURE-LINT:FINISH ;

RUN

;package
