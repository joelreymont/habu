\ layout-valid-growth.f — grow descriptor, environment, and guard arenas.

package LAYOUT-VALID-GROWTH

ENUM lvgd-inner zero one ;ENUM
variable PREV
TDECL-FAM-REG @ PREV !

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
