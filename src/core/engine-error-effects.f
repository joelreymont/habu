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
;package
