\ Real-load header cases: phantom parameters, declaration order in nested
\ payloads and quotations, and the token immediately after a NEWTYPE.
require src/lib/test.f
package DH
NEWTYPE ticket
7 drop
NEWTYPE phantom<i,a>
CAST: >PHANTOM ( n -- DH:phantom<n,r> )
CAST: PHANTOM> ( DH:phantom<n,r> -- n )
public
SUMTYPE choice<i,a>
   VARIANT left i ;VARIANT
   VARIANT right a ;VARIANT
;SUMTYPE
SUMTYPE nested<i,a>
   VARIANT both DH:choice<a,i> [ i -- a ] ;VARIANT
;SUMTYPE
STRUCTURE pair<i,a> FIELD first i FIELD second a ;STRUCTURE
PRODUCT packed<i,a> FIELD first i FIELD second a ;PRODUCT
ENUM color red green ;ENUM
ENUM shade opaque light ;ENUM
ENUM sole opaque ;ENUM
ENUM marked POLICY stack-cell-tag DERIVE eq
   VARIANT item FIELD value n ;VARIANT
;ENUM
ENUM response<i,a>
   VARIANT ok FIELD value i ;VARIANT
   VARIANT err FIELD value a ;VARIANT
;ENUM
1 set-tier
: LEFT ( n -- DH:choice<n,r> ) DH-CHOICE:LEFT ;
: RIGHT ( r -- DH:choice<n,r> ) DH-CHOICE:RIGHT ;
: PAIR ( n r -- n r ) DH-PAIR:MAKE DH-PAIR:UNMAKE ;
: PACKED ( n r -- n r ) DH-PACKED:MAKE DH-PACKED:UNMAKE ;
: NESTED ( n -- DH:nested<n,r> )
   DH-CHOICE:RIGHT [: drop 2.5 ;] DH-NESTED:BOTH ;
: OK ( n -- DH:response<n,r> ) DH-RESPONSE:OK ;
: PHANTOM ( n -- n ) >PHANTOM PHANTOM> ;
: RUN ( -- )
   T-RESET
   42 PHANTOM 42 T=
   19 2.5 PAIR 2.5 f= TTRUE 19 T=
   29 3.5 PACKED 3.5 f= TTRUE 29 T=
   DH-SHADE:OPAQUE MATCH DH:shade
      opaque OF ENDOF
      light OF false TTRUE ENDOF
   ;MATCH
   DH-SOLE:OPAQUE MATCH DH:sole opaque OF ENDOF ;MATCH
   41 NESTED MATCH DH:nested
      both OF
         {: item fn :}
         3 fn execute 2.5 f= TTRUE
         item MATCH DH:choice
            left OF fdrop false TTRUE ENDOF
            right OF 41 T= ENDOF
         ;MATCH
      ENDOF
   ;MATCH
   23 LEFT MATCH DH:choice
      left OF 23 T= ENDOF
      right OF fdrop false TTRUE ENDOF
   ;MATCH
   31 OK MATCH DH:response
      ok OF 31 T= ENDOF
      err OF fdrop false TTRUE ENDOF
   ;MATCH
   T-REPORT
   s" declaration headers: ok" type cr ;
;package
DH:RUN
