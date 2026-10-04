\ The private publication owner can replace a live record while its alias keeps
\ the old implementation. Run on the unsealed whitebox engine.
require lib/test.f
require src/habu/xref.f

package NDICT-RETARGET-TEST

TRUSTED: RECORD ( ptr u8 n n -- ptr n ) xref-search-wl ;
TRUSTED: ALIAS ( ptr u8 n n n -- ) alias-record ;
TRUSTED: RETARGET ( n n n -- ) xref-retarget ;
CAST: REF-WORD ( n -- [ -- n ] )
: REF-VALUE ( n n -- n ) DEF-OCC:CALLABLE REF-WORD execute ;
: EV ( ptr u8 n -- ) INCLUDE-EVALUATE ;

variable OLD-SLOT
variable OLD-OCC

1 set-tier

public
: RUN ( -- )
   T-RESET
   s" : OCC-OLD ( -- n ) 11 ; : OCC-ALT ( -- n ) 22 ;" EV
   s" OCC-OLD" 0 RECORD DEF-OCC:SELECT {: old-slot:n old-occ:n :}
   s" OCC-OLD" XREF-FIND-INDEX {: idx:n :}
   s" OCC-ALIAS" idx 0 ALIAS
   s" OCC-ALIAS" 0 RECORD DEF-OCC:SELECT {: alias-slot:n alias-occ:n :}
   s" OCC-ALT" 0 RECORD XREF-START
   s" OCC-ALT" 0 RECORD XREF-RAW-LEN  idx RETARGET
   old-slot OLD-SLOT !  old-occ OLD-OCC !
   s" replacing a live record retires only its occurrence" T-LABEL
   [: OLD-SLOT @ OLD-OCC @ DEF-OCC:RESOLVE drop ;]
      DEF-OCC:E-STALE TTHROWSQ
   alias-slot alias-occ REF-VALUE 11 T=
   s" OCC-OLD" 0 RECORD DEF-OCC:SELECT REF-VALUE 22 T=
   s" OCC-ALIAS" 0 RECORD DEF-OCC:SELECT REF-VALUE 11 T=
   T-REPORT ;

;package

NDICT-RETARGET-TEST:RUN
