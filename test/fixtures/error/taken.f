\ taken.f - for src/lib/error-test.f: a code declared under a second name is rejected with a
\ record, rc 70; the second declaration reads the code through the first, one lint claim.

require src/lib/error.f

-9673 s" the first holder" ERROR:CODE E-TAKEN-OLD
E-TAKEN-OLD s" the second holder" ERROR:CODE E-TAKEN-NEW
