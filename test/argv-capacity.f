\ Run with 65 positional arguments, both with and without an initial --.
\ Mock input itself is bounded at 64, so only real script argv reaches this case.
require lib/test.f
require lib/argv.f

package ARGV-CAPACITY

: RUN ( -- )
   T-RESET
   -1 ARGV:QUIET!
   ARGV:MOCK-CLEAR
   s" --json" ARGV:MOCK+
   s" --label" ARGV:MOCK+ s" kept-label" ARGV:MOCK+
   s" -o" ARGV:MOCK+ s" kept-output" ARGV:MOCK+
   s" --all-errors" ARGV:MOCK+
   s" --strict-boundary" ARGV:MOCK+
   s" kept.f" ARGV:MOCK+
   ARGV:PARSE
   ARGV:USE-SCRIPT
   s" positional overflow preserves the published parse" T-LABEL
   [: ARGV:PARSE ;] catch ARGV:E-USAGE T=
   ARGV:POS# 1 T=
   0 ARGV:POS$ s" kept.f" T$=
   ARGV:JSON? TTRUE
   ARGV:ALL-ERRORS? TTRUE
   ARGV:STRICT-BOUNDARY? TTRUE
   ARGV:LABEL? TTRUE ARGV:OUT? TTRUE
   ARGV:LABEL$ s" kept-label" T$=
   ARGV:OUT$ s" kept-output" T$=
   T-REPORT ;

RUN
;package
