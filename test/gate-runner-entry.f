\ gate-runner-entry.f - dispatch for the baked native gate runner.
\
\ Loaded by the warm gate image after gate support libraries have already been
\ baked. Keep this file side-effect-only: it selects one phase from SCRIPT-ARGV.

64 constant GR-USAGE-RC

: GR-USAGE ( -- )
   s" usage: hb-gate-warm --load test/gate-runner-entry.f -- PHASE" GR-USAGE-RC die ;

: GR-ARG0= ( ptr u8 n -- bool )
   0 SCRIPT-ARGV$ STR= ;

: GR-STDLIB? ( -- bool )
   s" tool" GR-ARG0= if 0 0= exit then
   s" check-cli" GR-ARG0= if 0 0= exit then
   s" tail" GR-ARG0= if 0 0= exit then
   s" lint-tools" GR-ARG0= if 0 0= exit then
   s" lint-manifest" GR-ARG0= if 0 0= exit then
   s" lint-artifacts" GR-ARG0= if 0 0= exit then
   s" lint-libs" GR-ARG0= if 0 0= exit then
   0 0= 0= ;

: GR-STDLIB ( -- )
   s" test/gate-stdlib-cases.f" included ;

: GR-TOOL ( -- )
   SUITE-SKIP-TOOL-LINTS!
   GR-STDLIB ;

: GR-DISPATCH ( -- )
   s" tool" GR-ARG0= if GR-TOOL exit then
   GR-STDLIB? if GR-STDLIB exit then
   s" repair" GR-ARG0= if GE-MAIN exit then
   s" fixtures" GR-ARG0= if GE-MAIN exit then
   s" runtime" GR-ARG0= if GE-MAIN exit then
   s" diag-repair" GR-ARG0= if GDX-DISPATCH exit then
   s" diag-undef-primary" GR-ARG0= if GDX-DISPATCH exit then
   s" diag-all-strict" GR-ARG0= if GDX-DISPATCH exit then
   s" diag-file-unsafe" GR-ARG0= if GDX-DISPATCH exit then
   s" dictionary" GR-ARG0= if GD-MAIN exit then
   s" debug" GR-ARG0= if GDB-RUN exit then
   GR-USAGE ;

: GR-MAIN ( -- )
   SCRIPT-ARGC 1 <> if GR-USAGE then
   GR-DISPATCH ;

GR-MAIN
