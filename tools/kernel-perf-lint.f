\ kernel-perf-lint.f - CLI wrapper for the kernel profile-row diff lint.
\ Run: bin/hb tools/kernel-perf-lint.f diff.patch ...
\ Load after tools/kernel-perf-lint-core.f and tools/argv.f.

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/fs.f
require lib/adt/option.f
require tools/lint/text.f
require tools/lint/token.f
require tools/lint/lib.f
require tools/ptx/perf-registry.f
require tools/kernel-perf-lint-core.f
require tools/argv.f

package KERNEL-PERF-LINT-CLI
private

: RUN ( -- )
   s" tools/kernel-perf-lint.f diff.patch ..." ARGV-USAGE!
   ARGV-PARSE
   1 -1 ARGV-EXPECT-POS
   KERNEL-PERF-LINT:RESET
   0 begin dup ARGV-POS# < while
      dup ARGV-POS$ KERNEL-PERF-LINT:FILE
      1+
   repeat drop
   KERNEL-PERF-LINT:FINISH ;

RUN

;package
