\ engine-error-effects.f - checker rows for the early engine failure ABI.

package ENGINE-ERROR
public
\ DISPATCH-UNSET .. CODE-CERT are early-engine constants loaded before checker publication.
s" DISPATCH-UNSET" s" -- n" TRUST
s" SEAL-VIOLATION" s" -- n" TRUST
s" SEAL-PACKAGE" s" -- n" TRUST
s" BAD-TAG" s" -- n" TRUST
s" CODE-CERT" s" -- n" TRUST
PPRIM: ENGINE-ERROR STACK-BOUNDS PE-N PE-OUT PPRIM;
PPRIM: ENGINE-ERROR CALLBACK PE-N PE-OUT PPRIM;
PPRIM: ENGINE-ERROR POLICY PE-N PE-OUT PPRIM;
PPRIM: ENGINE-ERROR MEMORY-FAULT PE-N PE-OUT PPRIM;
PPRIM: DEF-OCC E-STALE PE-N PE-OUT PPRIM;
\ The Habu loop's export throws it before the checker hears of an export whose
\ clause would find no occurrence left (src/habu/interpreter.f PKG-OCC-ROOM).
PPRIM: DEF-OCC E-EXHAUSTED PE-N PE-OUT PPRIM;
;package
