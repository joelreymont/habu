\ taken.f - for src/lib/error-test.f: a code outside 0..255 declared under a second name ends
\ the process; the second declaration reads the code through the first, one lint claim.

require src/lib/error.f

-9673 s" the first holder" ERROR:CODE E-TAKEN-OLD
E-TAKEN-OLD s" the second holder" ERROR:CODE E-TAKEN-NEW
