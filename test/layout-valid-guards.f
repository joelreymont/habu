\ layout-valid-guards.f — active and inactive nested-tag positive cases.

require test/layout-valid-guard-base.f

package LAYOUT-VALID-GUARD

: READ ( n -- )
   BUF @ drop ;

0 0 0 SET
99 1 1 SET
0 READ
1 READ
s" ok" type cr

;package
