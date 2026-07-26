\ aot-lint.f - CLI wrapper for stripped-AOT source lint.
\ This is a standalone entry point: it loads its own dependencies, so
\ `bin/hb --load tools/aot-lint.f` works on its own. It used to only list them in
\ a comment and rely on the caller having loaded them first, which held inside
\ test/run.f but left the standalone command dead.

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
require tools/aot-lint-core.f
require lib/argv.f

package AOT-LINT-CLI
private

: ARGV-FILE ( n -- ) {: idx:n :}
   ARGV:LABEL? IF
      idx ARGV:POS$ ARGV:LABEL$ AOT-LINT:FILE-AS
   ELSE
      idx ARGV:POS$ AOT-LINT:FILE
   THEN ;

: RUN ( -- )
   s" tools/aot-lint.f [--json] [--label name] file ..." ARGV:USAGE!
   ARGV:PARSE
   1 -1 ARGV:EXPECT-POS
   AOT-LINT:RESET
   ARGV:JSON? AOT-LINT:JSON!
   0 begin dup ARGV:POS# < while
      dup ARGV-FILE
      1+
   repeat drop
   AOT-LINT:FINISH ;

RUN

;package
