\ layout-valid-guard-base.f — low-level nested SUM schema for guard tests.

using SCHEMA-REG
using TFAM

package LAYOUT-VALID-GUARD

public

ENUM lvg-inner zero one ;ENUM

private

\ Whitebox registry boundaries: the raw type-family/schema builders are
\ engine-internal (DNAME-INT after the seal-time marking pass), so every loader
\ of this fixture runs on the whitebox engine, where the manual family
\ construction below names them directly and a checked body binds their rows.

\ The family just declared, resolved by name in the declaring scope: the ENUM
\ front end publishes constructors inside the declaration transaction and never
\ writes src/core/sumtype.f TDECL-FAM-REG. TFAM-SIG-RESOLVE's found flag is
\ checked: a name this file cannot resolve would otherwise build on family 0.
: LVG-FAMID ( ptr u8 n -- n )
   TFAM-ACTIVE-PKG$ 2swap TFAM-SIG-RESOLVE
   0= IF drop s" layout-valid-guard-base: family does not resolve" 1 die THEN ;

s" lvg-inner" LVG-FAMID constant INNER

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

\ The forged rows need a raw cell view of the checked buffer.
CAST: RAW ( ptr lvg-outer -- ptr n )

: SET ( n n n -- )
   {: payload:n tag:n idx:n :}
   idx BUF RAW {: addr:ptr :}
   payload addr !
   tag addr cell+ ! ;

;package

;using
;using
