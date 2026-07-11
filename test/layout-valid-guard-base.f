\ layout-valid-guard-base.f — low-level nested SUM schema for guard tests.

package LAYOUT-VALID-GUARD

public

ENUM lvg-inner zero one ;ENUM

private

TDECL-FAM-REG @ constant INNER

TFAM-ACTIVE-PKG$ CHECKER-PACKAGE-PUBLIC s" lvg-outer" 0 TK-SUM
   TFAM-DECL constant OUTER

SCHEMA-ROOT-N@ constant LEFT-SCHEMA
INNER 0 0 SCHEMA-APP SCHEMA-ROOT+ drop

SCHEMA-ROOT-N@ constant RIGHT-SCHEMA
CC-N SCHEMA-CON SCHEMA-ROOT+ drop

SUMV-N@ constant VARIANTS
OUTER s" left" 0 LEFT-SCHEMA 1 1 SUMV-ADD drop
OUTER s" right" 1 RIGHT-SCHEMA 1 1 SUMV-ADD drop
OUTER 1 TFAM-SLOTS!
OUTER VARIANTS 2 TFAM-VAR-RANGE!

2 LAYOUT-BUFFER BUF lvg-outer

TRUSTED: RAW ( ptr lvg-outer -- ptr n ) ;

TRUSTED: SET ( n n n -- )
   {: payload:n tag:n idx:n :}
   idx BUF RAW {: addr:ptr :}
   payload addr !
   tag addr cell+ ! ;

end-package
