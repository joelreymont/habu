\ layout-valid-growth.f — grow descriptor, environment, and guard arenas.

package LAYOUT-VALID-GROWTH

ENUM lvgd-inner zero one ;ENUM
variable PREV

\ Seed the nesting chain with the family just declared, resolved BY NAME in the
\ declaring scope. This used to read TDECL-FAM-REG, the ambient "last family the
\ legacy definer registered" variable in sumtype.f. That variable belongs to the
\ legacy definers and their constructor adapter; the global ENUM keyword is the
\ unified front end now, which never writes it. Resolving the name is also the
\ honest question — this file wants `lvgd-inner`, not whichever family happened
\ to be registered last.
\ Fails closed on an unresolvable name for the same reason as
\ test/layout-valid-guard-base.f's LVG-FAMID: dropping the found flag would seed
\ the nesting chain with family 0.
TRUSTED: LVGD-FAMID ( ptr u8 n -- n )
   TFAM-ACTIVE-PKG$ 2swap TFAM-SIG-RESOLVE
   0= IF drop s" layout-valid-growth: family does not resolve" 1 die THEN ;

s" lvgd-inner" LVGD-FAMID PREV !

create NAME 11 allot

: DIGIT ( n -- n )
   dup 10 < if 48 + exit then
   10 - 97 + ;

TRUSTED: NAME$ ( n -- ptr u8 n )
   {: idx:n :}
   s" lvg-deep-" drop NAME 9 BYTE-COPY
   idx 36 / DIGIT NAME 9 + c!
   idx 36 mod DIGIT NAME 10 + c!
   NAME 11 ;

TRUSTED: BUILD ( -- )
   0 begin dup 40 < while
      dup NAME$ {: name:ptr nameu:n :}
      TFAM-ACTIVE-PKG$ CHECKER-PACKAGE-PUBLIC name nameu 0 TK-SUM
         TFAM-DECL {: fam:n :}
      SCHEMA-ROOT-N@ {: schema:n :}
      PREV @ 0 0 SCHEMA-APP SCHEMA-ROOT+ drop
      SUMV-N@ {: variants:n :}
      PREV @ TFAM-WIDTH@ {: width:n :}
      fam s" left" 0 schema 1 width SUMV-ADD drop
      fam s" right" 1 SCHEMA-ROOT-N@ 0 0 SUMV-ADD drop
      fam width TFAM-SLOTS!
      fam variants 2 TFAM-VAR-RANGE!
      fam PREV !
      1 +
   repeat drop ;

BUILD
1 LAYOUT-BUFFER BUF lvg-deep-13

: READ ( -- )
   0 BUF @ drop ;

READ
s" ok" type cr

;package
