\ duplicate-definition-lint.f - CLI wrapper for flat duplicate-definition lint.
\ This is a standalone entry point: it loads its own dependencies, so
\ `bin/hb --load tools/duplicate-definition-lint.f` works on its own. It used to
\ only list them in a comment and rely on the caller having loaded them first,
\ which held inside test/run.f but left the standalone command dead.

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/fs.f
require tools/lint/text.f
require tools/lint/token.f
require tools/lint/lib.f
require tools/lint/json-writer.f
require tools/duplicate-definition-lint-core.f
require lib/argv.f

: DUPLICATE-DEFINITION-LINT-ARGV-FILE ( n -- ) {: idx :}
   ARGV:LABEL? if
      idx ARGV:POS$ ARGV:LABEL$ DUPLICATE-DEFINITION-LINT-FILE-AS
   else
      idx ARGV:POS$ DUPLICATE-DEFINITION-LINT-FILE
   then ;

: DUPLICATE-DEFINITION-LINT ( -- )
   s" tools/duplicate-definition-lint.f [--json] [--label name] file ..." ARGV:USAGE!
   ARGV:PARSE
   1 -1 ARGV:EXPECT-POS
   DUPLICATE-DEFINITION-LINT-RESET
   ARGV:JSON? DDL-JSON!
   0 begin dup ARGV:POS# < while
      dup DUPLICATE-DEFINITION-LINT-ARGV-FILE
      1+
   repeat drop
   DUPLICATE-DEFINITION-LINT-FINISH ;

DUPLICATE-DEFINITION-LINT
