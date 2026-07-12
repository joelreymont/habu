\ layout-valid-active-bad.f — an active nested invalid tag rejects.

require test/layout-valid-guard-base.f

package LAYOUT-VALID-GUARD

: READ-ACTIVE ( -- )
   0 BUF @ drop ;

2 0 0 SET
s" LAYOUT-VALID-ACTIVE-ARMED" type cr
READ-ACTIVE

;package
