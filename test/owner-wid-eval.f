\ owner-wid-eval.f - warm snapshot nested-evaluate mode proof.

require test/owner-wid-guard.f
OWNER-WID-GUARD:REQUIRE-FORGED

require lib/test.f

T-RESET

s" 271828 . cr" evaluate

T-REPORT
s" owner-wid-eval-test: ok" type cr
