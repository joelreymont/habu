\ prov-main.f - provides prov-part.f, then includes it; sibling.f loads
\ word.f, which prov-main.f does not require.
require test/fixtures/require-check/sibling.f
s" test/fixtures/require-check/prov-part.f" provided
include test/fixtures/require-check/prov-part.f
