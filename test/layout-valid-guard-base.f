\ layout-valid-guard-base.f — low-level nested SUM schema for guard tests.

package LAYOUT-VALID-GUARD

public

ENUM lvg-inner zero one ;ENUM

private

TDECL-FAM-REG @ constant INNER

\ Whitebox registry boundaries: the raw type-family/schema builders are
\ engine-internal (DNAME-INT after the seal-time marking pass), so the manual
\ family construction below reaches them through TRUSTED: wrappers, matching
\ the type-decl suite convention (dot habu-hb-crash-bare-c5be6634).
TRUSTED: LVG-TFAM-ACTIVE-PKG$ ( -- ptr u8 n ) TFAM-ACTIVE-PKG$ ;
TRUSTED: LVG-TFAM-DECL ( ptr u8 n n ptr u8 n n n -- n ) TFAM-DECL ;
TRUSTED: LVG-SCHEMA-APP ( n n n -- n ) SCHEMA-APP ;
TRUSTED: LVG-SCHEMA-CON ( n -- n ) SCHEMA-CON ;
TRUSTED: LVG-SCHEMA-ROOT+ ( n -- n ) SCHEMA-ROOT+ ;
TRUSTED: LVG-SUMV-ADD ( n ptr u8 n n n n n -- n ) SUMV-ADD ;
TRUSTED: LVG-TFAM-SLOTS! ( n n -- ) TFAM-SLOTS! ;
TRUSTED: LVG-TFAM-VAR-RANGE! ( n n n -- ) TFAM-VAR-RANGE! ;

LVG-TFAM-ACTIVE-PKG$ CHECKER-PACKAGE-PUBLIC s" lvg-outer" 0 TK-SUM
   LVG-TFAM-DECL constant OUTER

SCHEMA-ROOT-N@ constant LEFT-SCHEMA
INNER 0 0 LVG-SCHEMA-APP LVG-SCHEMA-ROOT+ drop

SCHEMA-ROOT-N@ constant RIGHT-SCHEMA
CC-N LVG-SCHEMA-CON LVG-SCHEMA-ROOT+ drop

SUMV-N@ constant VARIANTS
OUTER s" left" 0 LEFT-SCHEMA 1 1 LVG-SUMV-ADD drop
OUTER s" right" 1 RIGHT-SCHEMA 1 1 LVG-SUMV-ADD drop
OUTER 1 LVG-TFAM-SLOTS!
OUTER VARIANTS 2 LVG-TFAM-VAR-RANGE!

2 LAYOUT-BUFFER BUF lvg-outer

TRUSTED: RAW ( ptr lvg-outer -- ptr n ) ;

TRUSTED: SET ( n n n -- )
   {: payload:n tag:n idx:n :}
   idx BUF RAW {: addr:ptr :}
   payload addr !
   tag addr cell+ ! ;

;package
