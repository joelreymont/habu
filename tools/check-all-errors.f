\ check-all-errors.f - CLI wrapper for all-errors checker diagnostics.

require lib/argv.f
require tools/check-all-errors-core.f

package CHECK-ALL-ERRORS-CLI
private

\ The report goes to standard error as the core makes it, so it holds every
\ record the source makes. The scratch, where the core renders a pass's
\ diagnostics before it reports them, is the size tools/check.f gives it.
INCLUDE-BUF-CAP constant ERR-CAP

create ERR-BUF ERR-CAP allot

: RUN ( -- )
   s" tools/check-all-errors.f [--json-errors] --label name source" ARGV:USAGE!
   ARGV:PARSE
   ARGV:REQUIRE-LABEL
   1 ARGV:EXPECT-POS-EXACT
   2 >FD ERR-BUF ERR-CAP CHECK-ALL-ERRORS:STREAM!
   ARGV:JSON? CHECK-ALL-ERRORS:JSON!
   ARGV:LABEL$ 0 ARGV:POS$ CHECK-ALL-ERRORS:FILE ;

RUN

;package
