\ definer-good.f - definer-bad.f and user-bad.f with the definers' files
\ required.
require test/fixtures/require-check/definer-sibling.f
require test/fixtures/require-check/definer.f
require src/lib/task.f
5 RQC-DEFINER:MAKE RQC-GOOD-FIVE
TASK:+USER RQC-GOOD-SLOT n
