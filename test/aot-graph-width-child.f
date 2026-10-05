\ A saved graph built from real checked declarations, without an ARM code window.
package CHECKER-REG
CHECKER-SCOPE-START
CHECKER-PAYLOAD-ARM
;package

: GW-SCALAR ( n -- n ) ;
: GW-WIDE ( read-view<p,q,u8> -- read-view<p,q,u8> ) ;
: GW-LOGICAL ( init<p,a> -- init<p,a> ) ;

package CHECKER-REG

$20000 constant GW-CAP
create GW-POOL GW-CAP allot
create GW-REG GW-CAP allot
variable GW-LEN
variable GW-REG-U

TRUSTED: GW-COPY ( -- )
   CHECKER-PAYLOAD-FREEZE
   GW-REG GW-CAP CHECKER-REG-AOT-SAVE GW-REG-U !
   ASIG-ROW-U @ {: rows:n :}
   ASIG-STR-U @ {: strings:n :}
   56 rows + strings + {: regoff:n :}
   GW-REG-U @ GW-CAP regoff - > IF 79 throw THEN
   3 GW-POOL !
   56 GW-POOL 8 + ! rows GW-POOL 16 + !
   56 rows + GW-POOL 24 + ! strings GW-POOL 32 + !
   regoff GW-POOL 40 + ! GW-REG-U @ GW-POOL 48 + !
   ASIG-ROW-P @ GW-POOL 56 + rows USIGS-COPY
   ASIG-STR-P @ GW-POOL 56 rows + + strings USIGS-COPY
   GW-REG GW-POOL regoff + GW-REG-U @ USIGS-COPY
   regoff GW-REG-U @ + GW-LEN ! ;

TRUSTED: GW-INSTALL-POOL ( -- )
   GW-POOL CK-AOT-SIG-POOL-FIELD !
   GW-LEN @ data-base CK-AOT-SIG-LEN-OFF + !
   0 CK-AOT-STATE ! ;

TRUSTED: GW-AT ( n -- ptr u8 )
   4 CK-AOT-FIELD GW-POOL CK-AOT-S-STR CK-AOT-OFF + + ;

variable GW-FOUND
TRUSTED: GW-GRAPH ( ptr u8 n -- ptr u8 ) {: name:ptr u:n :}
   -1 GW-FOUND !
   CK-AOT-ROWS 0 ?do
      i 0 CK-AOT-FIELD CK-AOT-STR$ name u CORE-STR=CI IF
         i GW-FOUND !
      THEN
   loop
   GW-FOUND @ -1 = IF 79 throw THEN
   GW-FOUND @ GW-AT ;

TRUSTED: GW-MODE? ( ptr u8 n -- bool )
   s" HABU_GRAPH_WIDTH_MODE" GETENV CORE-STR= ;

TRUSTED: GW-CORRUPT ( -- )
   s" scalar-zero" GW-MODE? IF
      s" GW-SCALAR" GW-GRAPH {: graph:ptr :}
      graph graph EW.DIN @ + dup EN.C @ 2 <> IF 79 throw THEN
      1 swap EN.C ! 0 graph EW.MINI !
   THEN
   s" wide-width" GW-MODE? IF
      s" GW-WIDE" GW-GRAPH {: graph:ptr :}
      graph graph EW.DIN @ + {: row:ptr :}
      row EN.C @ 2 <> IF 79 throw THEN
      graph row EN.A @ + {: node:ptr :}
      node EN.TAG @ EN-PARAM <> node EN.E @ 2 <> or IF 79 throw THEN
      3 row EN.C ! 3 graph EW.MINI !
   THEN
   s" logical-width" GW-MODE? IF
      s" GW-LOGICAL" GW-GRAPH {: graph:ptr :}
      graph graph EW.DIN @ + {: row:ptr :}
      row EN.C @ 2 <> IF 79 throw THEN
      graph row EN.A @ + {: node:ptr :}
      node EN.TAG @ EN-PARAM <> node EN.E @ 0 <> or IF 79 throw THEN
      1 row EN.C ! 0 graph EW.MINI !
   THEN ;

TRUSTED: GW-WIDTH? ( -- bool )
   s" scalar-zero" GW-MODE?
   s" wide-width" GW-MODE? or
   s" logical-width" GW-MODE? or ;

TRUSTED: GW-REFUSAL ( -- )
   TFAM:PREPARE-GRAPH-STATE
   RES-FALSE GRAPH-PUBLICATION:START
   GRAPH-PUBLICATION:CORE TFAM:GRAPH-STATE GRAPH-PUBLICATION:FINISH
   [: CK-AOT-CONTENTS? ;] catch 76 <> IF 79 throw THEN
   CK-GRAPH-WIDTH-BAD @ -1 <> IF 79 throw THEN
   CK-GRAPH-RELEASE
   CK-AOT-STATE @ 0 <> IF 79 throw THEN
   RES-TRUE GRAPH-PUBLICATION:START
   GRAPH-PUBLICATION:CORE TFAM:GRAPH-STATE GRAPH-PUBLICATION:FINISH
   s" graph width refusal preserved publication state" type cr
   CK-AOT-REG-INSTALL ;

TRUSTED: GW-VALID ( -- )
   CK-AOT-CONTENTS?
   CK-AOT-REG-INSTALL
   CK-AOT-ROWS 0 ?do i CK-AOT-TAKE loop
   s" graph width valid: " type CK-AOT-ROWS . cr ;

TRUSTED: GW-RUN ( -- )
   GW-COPY
   CHECKER-SCOPE-DONE
   GW-INSTALL-POOL
   GW-CORRUPT
   GW-WIDTH? IF
      s" graph corruption applied: " type
      s" HABU_GRAPH_WIDTH_MODE" GETENV type cr
      GW-REFUSAL
   ELSE GW-VALID THEN ;

GW-RUN

;package
