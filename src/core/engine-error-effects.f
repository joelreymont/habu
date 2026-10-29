\ engine-error-effects.f - checker rows for the early engine failure ABI.

package ENGINE-ERROR
public
\ AOT-SEED .. CODE-CERT are early-engine constants loaded before checker publication.
s" AOT-SEED" s" -- n" TRUST
s" SEAL-VIOLATION" s" -- n" TRUST
s" SEAL-PACKAGE" s" -- n" TRUST
s" BAD-TAG" s" -- n" TRUST
s" CALLABLE-ABI" s" -- n" TRUST
s" CATCH-STACK" s" -- n" TRUST
s" CODE-CERT" s" -- n" TRUST
PPRIM: ENGINE-ERROR STACK-BOUNDS PE-N PE-OUT PPRIM;
PPRIM: ENGINE-ERROR CALLBACK PE-N PE-OUT PPRIM;
PPRIM: ENGINE-ERROR POLICY PE-N PE-OUT PPRIM;
PPRIM: ENGINE-ERROR OVERLAY-OPEN PE-N PE-OUT PPRIM;
PPRIM: ENGINE-ERROR THROW-ZERO PE-N PE-OUT PPRIM;
PPRIM: DEF-OCC E-STALE PE-N PE-OUT PPRIM;
\ The Habu loop's export throws it before the checker hears of an export whose
\ clause would find no occurrence left (src/habu/packages.f PKG-OCC-ROOM).
PPRIM: DEF-OCC E-EXHAUSTED PE-N PE-OUT PPRIM;
\ The `using` failure codes, which the Habu loop's checked bodies throw
\ (src/habu/packages.f, src/habu/outer.f): without these rows
\ src/habu/interpret.f does not load on a from-source prefix boot
\ (test/cold-naming-test.f).
PPRIM: ENGINE-ERROR USING-NO-NAME PE-N PE-OUT PPRIM;
PPRIM: ENGINE-ERROR USING-BAD-NAME PE-N PE-OUT PPRIM;
PPRIM: ENGINE-ERROR USING-UNKNOWN PE-N PE-OUT PPRIM;
PPRIM: ENGINE-ERROR USING-OVERFLOW PE-N PE-OUT PPRIM;
PPRIM: ENGINE-ERROR USING-UNBALANCED PE-N PE-OUT PPRIM;
PPRIM: ENGINE-ERROR USING-AMBIGUOUS PE-N PE-OUT PPRIM;
PPRIM: ENGINE-ERROR USING-OUTER PE-N PE-OUT PPRIM;
PPRIM: ENGINE-ERROR USING-SHADOW-GLOBAL PE-N PE-OUT PPRIM;
;package
