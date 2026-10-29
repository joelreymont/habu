\ deftype-dup-bad.f - a duplicate DEFTYPE in one package is refused:
\ CHECKER-DEFFAMILY throws E-TFAM-DUP, which exits 70 uncaught because the
\ checker rendered it. The ARMED marker proves the first declaration ran before
\ the refusal. The same name in another package is not a duplicate.

require src/lib/type/deftype.f

package CAMERA
DEFTYPE SERIAL
s" DEFTYPE-DUP-ARMED" type cr
DEFTYPE SERIAL
;package
