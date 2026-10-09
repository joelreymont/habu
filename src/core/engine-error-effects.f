\ engine-error-effects.f - checker rows for the early engine failure ABI.

package ENGINE-ERROR
public
s" AOT-SEED" s" -- n" TRUST
s" SEAL-VIOLATION" s" -- n" TRUST
s" SEAL-PACKAGE" s" -- n" TRUST
\ The four surviving values are immutable early-engine constants loaded before
\ checker publication. Retirement: habu-campaign-c2-mem-c3d7662b.
s" BAD-TAG" s" -- n" TRUST
s" CALLABLE-ABI" s" -- n" TRUST
s" CATCH-STACK" s" -- n" TRUST
s" CODE-CERT" s" -- n" TRUST
s" IMAGE-CODE-ORIGIN" s" -- n" TRUST
s" CODE-ORIGIN-FULL" s" -- n" TRUST
\ These immutable values are part of the engine failure ABI.
PPRIM: ENGINE-ERROR STACK-BOUNDS PE-N PE-OUT PPRIM;
PPRIM: ENGINE-ERROR CALLBACK PE-N PE-OUT PPRIM;
PPRIM: ENGINE-ERROR POLICY PE-N PE-OUT PPRIM;
PPRIM: ENGINE-ERROR OVERLAY-OPEN PE-N PE-OUT PPRIM;
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
