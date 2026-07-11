\ layout-valid-root-bad.f — an invalid root tag rejects before payload checks.

require test/layout-valid-guard-base.f

package LAYOUT-VALID-GUARD

: READ-ROOT ( -- )
   0 BUF @ drop ;

0 2 0 SET
s" LAYOUT-VALID-ROOT-ARMED" type cr
READ-ROOT

end-package
