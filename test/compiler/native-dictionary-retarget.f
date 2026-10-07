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
variable FRAME-SLOT
variable FRAME-OCC
variable FRAME-CODE

public
: FRAME-RETARGET ( -- )
   s" OCC-FRAME" XREF-FIND-INDEX {: idx:n :}
   s" OCC-FRAME-TEMP" 0 RECORD {: target:ptr :}
   target XREF-START FRAME-CODE !
   target XREF-START target XREF-RAW-LEN idx RETARGET
   s" OCC-FRAME" 0 RECORD DEF-OCC:SELECT
   FRAME-OCC !  FRAME-SLOT ! ;
private

: FAILED-FRAME ( -- )
   s" : OCC-FRAME-TEMP ( -- n ) 33 ; NDICT-RETARGET-TEST:FRAME-RETARGET 73 throw" EV ;

: RECOVER-RETARGET ( -- )
   s" : OCC-FRAME ( -- n ) 7 ;" EV
   ndict@ {: count:n :}
   cp@ {: code:n :}
   ['] FAILED-FRAME 73 TTHROWS
   ndict@ count T=
   cp@ code T=
   FRAME-SLOT @ FRAME-OCC @ DEF-OCC:RESOLVE drop ;

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
   s" a surviving retarget cannot retain thrown code" T-LABEL
   ['] RECOVER-RETARGET DEF-OCC:E-STALE TTHROWS
   s" : OCC-FRAME-NEW ( -- n ) 44 ;" EV
   s" OCC-FRAME-NEW" 0 RECORD XREF-START FRAME-CODE @ T=
   s" OCC-FRAME-NEW" 0 RECORD DEF-OCC:SELECT REF-VALUE 44 T=
   T-REPORT ;

;package

NDICT-RETARGET-TEST:RUN
