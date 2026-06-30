\ checked-boundary-lint.f - CLI wrapper for unchecked-boundary lint.
\ Load after lib/errors.f, lib/string.f, lib/memory.f, lib/fs.f,
\ tools/lint/json-writer.f, tools/checked-boundary-lint-core.f, and
\ tools/argv.f.

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/fs.f
require tools/lint/text.f
require tools/lint/json-writer.f
require tools/checked-boundary-lint-core.f
require tools/argv.f

: CHECKED-BOUNDARY-LINT-ARGV-FILE ( n -- )
   ARGV-POS$ CHECKED-BOUNDARY-LINT-FILE ;

: CHECKED-BOUNDARY-LINT ( -- )
   s" tools/checked-boundary-lint.f file ..." ARGV-USAGE!
   ARGV-PARSE
   1 -1 ARGV-EXPECT-POS
   CHECKED-BOUNDARY-LINT-RESET
   ARGV-JSON? UB-JSON!
   ARGV-STRICT-BOUNDARY? UB-STRICT-BOUNDARY!
   0 begin dup ARGV-POS# < while
      dup CHECKED-BOUNDARY-LINT-ARGV-FILE
      1+
   repeat drop
   CHECKED-BOUNDARY-LINT-FINISH ;

CHECKED-BOUNDARY-LINT
