\ deftype-dup-bad.f - a duplicate DEFTYPE in the SAME package is refused
\ fail-closed. DEFTYPE mints an arity-0 type family; redeclaring the same tail in
\ one package routes through CHECKER-DEFFAMILY's duplicate check and throws
\ E-TFAM-DUP ("duplicate family", uncaught exit 67). Registered as a negative gate
\ case in test/candidate-validation.f; the ARMED marker proves the first (valid)
\ declaration executed before the duplicate refusal fired. (The SAME name in a
\ DIFFERENT package is NOT a duplicate - that distinctness is the positive suite.)

require lib/type/deftype.f

package CAMERA
DEFTYPE SERIAL
s" DEFTYPE-DUP-ARMED" type cr
DEFTYPE SERIAL
;package
