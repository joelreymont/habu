\ stretch-bad.f - definer-bad.f's call after a name only evaluated text defines.
require test/fixtures/require-check/definer-sibling.f
s" : RQC-TEXT ( -- n ) 5 ;" evaluate
RQC-TEXT RQC-DEFINER:MAKE RQC-STRETCH-FIVE
