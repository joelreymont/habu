\ A saved graph built from real checked declarations, without an ARM code window.
package CHECKER-REG
using STRUCTURE-DECL
using ENUM-DECL
variable GW-SOURCE-MARK
variable GW-SAVED-CHECK
variable GW-SAVED-TIER
CHECKER-SCOPE-START
CHECKER-PAYLOAD-ARM
UEND @ GW-SOURCE-MARK !
public

TRUSTED: GW-DECLARE-WIDE ( -- )
   s" payload-wide" s" 0 FIELD left n FIELD right r ;STRUCTURE" SD-REPLAY
   s" payload-empty" s" 0 ;STRUCTURE" SD-REPLAY
   s" payload-option" s" 1 VARIANT full FIELD value a ;VARIANT VARIANT empty ;VARIANT ;ENUM" ED-REPLAY ;

TRUSTED: GW-ABI-BEGIN ( -- )
   check@ GW-SAVED-CHECK ! tier@ GW-SAVED-TIER ! 0 set-check 1 TIER:SELECT ;
TRUSTED: GW-ABI-END ( -- )
   GW-SAVED-CHECK @ set-check GW-SAVED-TIER @ TIER:SELECT ;
;using
;using
;package

: ROW-ADD ( R -- R ) 1+ ;
: PAYLOAD-FIXED ( n -- n ) 1+ ;
: PAYLOAD-QUANT ( R [ R -- S ] -- S ) execute ;
NEWTYPE payload-tag 0
: PAYLOAD-TAG ( payload-tag -- payload-tag ) ;
: ANON-PROVIDER [: 1+ ;] ;
: RETURN-PROVIDER ( n | -- | n ) >r ;
CHECKER-REG:GW-DECLARE-WIDE
: PAYLOAD-WIDE-USE ( payload-wide -- payload-wide ) ;
: PAYLOAD-NESTED-USE ( payload-option<payload-wide> -- payload-option<payload-wide> ) ;
: PAYLOAD-POLY-USE ( payload-option<a> -- payload-option<a> ) ;
: PAYLOAD-ZERO-ARG-USE ( payload-option<payload-empty> -- payload-option<payload-empty> ) ;
CHECKER-REG:GW-ABI-BEGIN
: PAYLOAD-ABI ( n -- n ) ;
CHECKER-REG:GW-ABI-END

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

TRUSTED: GW-CORRUPT-SHAPE ( -- )
   s" authority-bits" GW-MODE? IF
      s" ROW-ADD" GW-GRAPH EW.SYM dup @ CTL-GRAPH-FLAGS invert
      dup negate and or swap !
   THEN
   s" length" GW-MODE? IF
      $7FFFFFFFFFFFFFFF s" ROW-ADD" GW-GRAPH EW.NEXT !
   THEN
   s" cycle" GW-MODE? IF
      s" ROW-ADD" GW-GRAPH {: graph:ptr :}
      graph EW.DIN @ {: off:n :}
      off graph off + EN.B !
   THEN
   s" tag" GW-MODE? IF
      s" ROW-ADD" GW-GRAPH {: graph:ptr :}
      99 graph graph EW.DIN @ + EN.TAG !
   THEN ;

TRUSTED: GW-CORRUPT-VARIABLES ( -- )
   s" variables" GW-MODE? IF
      s" PAYLOAD-QUANT" GW-GRAPH {: graph:ptr :}
      graph EW.TVN @ graph EW.RVN @ + 0 <= IF 79 throw THEN
      0 graph EW.TVN ! 0 graph EW.RVN !
   THEN ;

TRUSTED: GW-CORRUPT-FAMILY ( -- )
   s" family" GW-MODE? IF
      s" PAYLOAD-NESTED-USE" GW-GRAPH {: graph:ptr :}
      graph EW.DOUT @ graph + EN.A @ graph + {: node:ptr :}
      node EN.TAG @ EN-PARAM <> IF 79 throw THEN
      0 node EN.H !
   THEN ;

TRUSTED: GW-CORRUPT-EXCEPTION ( -- )
   s" exception" GW-MODE? IF
      s" ANON-PROVIDER" GW-GRAPH {: graph:ptr :}
      graph EW.DOUT @ graph + EN.A @ graph + {: node:ptr :}
      node EN.TAG @ EN-QUOT <> IF 79 throw THEN
      -1 node EN.E !
   THEN ;

TRUSTED: GW-CORRUPT-TYPE ( -- )
   GW-CORRUPT-VARIABLES GW-CORRUPT-FAMILY GW-CORRUPT-EXCEPTION ;

TRUSTED: GW-CORRUPT-WIDTH ( -- )
   s" scalar-zero" GW-MODE? IF
      s" PAYLOAD-FIXED" GW-GRAPH {: graph:ptr :}
      graph graph EW.DIN @ + dup EN.C @ 2 <> IF 79 throw THEN
      1 swap EN.C ! 0 graph EW.MINI !
   THEN
   s" wide-width" GW-MODE? IF
      s" PAYLOAD-WIDE-USE" GW-GRAPH {: graph:ptr :}
      graph graph EW.DIN @ + {: row:ptr :}
      row EN.C @ 2 <> IF 79 throw THEN
      graph row EN.A @ + {: node:ptr :}
      node EN.TAG @ EN-PARAM <> node EN.E @ 2 <> or IF 79 throw THEN
      3 row EN.C ! 3 graph EW.MINI !
   THEN
   s" logical-width" GW-MODE? IF
      s" PAYLOAD-POLY-USE" GW-GRAPH {: graph:ptr :}
      graph graph EW.DIN @ + {: row:ptr :}
      row EN.C @ 3 <> IF 79 throw THEN
      graph row EN.A @ + {: node:ptr :}
      node EN.TAG @ EN-PARAM <> node EN.E @ 0 <> or IF 79 throw THEN
      2 row EN.C ! 1 graph EW.MINI !
   THEN ;

TRUSTED: GW-CORRUPT ( -- )
   GW-CORRUPT-SHAPE GW-CORRUPT-TYPE GW-CORRUPT-WIDTH ;

TRUSTED: GW-WIDTH? ( -- bool )
   s" scalar-zero" GW-MODE?
   s" wide-width" GW-MODE? or
   s" logical-width" GW-MODE? or ;

TRUSTED: GW-SOURCE-CORRUPT ( -- )
   s" producer-scalar-zero" GW-MODE? IF
      s" PAYLOAD-FIXED" FIND-SIG -1 <> IF 79 throw THEN
      FEP @ {: rec:ptr :}
      rec E-DIN@ E-PTR EN.C @ 2 <> IF 79 throw THEN
      1 rec E-DIN@ E-PTR EN.C ! 0 rec E-CONTENT EC.MINI !
      s" graph corruption applied: producer-scalar-zero" type cr
   THEN
   s" recovery" GW-MODE? IF
      MULTI-ERR-BEGIN
      s" : PAYLOAD-RECOVERY ( n -- n ) drop ;" evaluate
      MULTI-ERR-END 1 <> IF 79 throw THEN
      s" graph corruption applied: recovery" type cr
   THEN ;

TRUSTED: GW-RETIRE-SOURCE ( -- )
   UEND @ {: end:n :}
   CHECKER-SCOPE-DONE
   UEND @ GW-SOURCE-MARK @ <> IF 79 throw THEN
   end GW-SOURCE-MARK @ ?do 0 USIGS i + c! loop ;

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
   GW-SOURCE-CORRUPT
   GW-COPY
   GW-RETIRE-SOURCE
   GW-INSTALL-POOL
   GW-CORRUPT
   s" HABU_GRAPH_WIDTH_MODE" GETENV nip IF
      s" graph corruption applied: " type
      s" HABU_GRAPH_WIDTH_MODE" GETENV type cr
   THEN
   GW-WIDTH? IF
      GW-REFUSAL
   ELSE GW-VALID THEN ;

GW-RUN

TRUSTED: GW-ACCEPT ( -- )
   s" PAYLOAD-FIXED" EFFECT-EXTERNAL-MIN-IN 1 <> IF 79 throw THEN
   s" PAYLOAD-ABI" EFFECT-QUERY -1 <> IF 79 throw THEN
   EFFECT-DIN-CELLS 1 <> EFFECT-DOUT-CELLS 1 <> or IF 79 throw THEN
   s" PAYLOAD-ABI" EFFECT-EXTERNAL-MIN-IN -1 <> IF 79 throw THEN
   s" PAYLOAD-ABI" CHECKER-RESOLVES? 0 <> IF 79 throw THEN
   s" ROUND-ABI ( n -- n ) PAYLOAD-ABI" CHECK! -1 <> IF 79 throw THEN
   s" PAYLOAD-WIDE-USE" EFFECT-QUERY -1 <> IF 79 throw THEN
   EFFECT-DIN-N 2 <> EFFECT-DIN-CELLS 2 <> or IF 79 throw THEN
   s" PAYLOAD-NESTED-USE" EFFECT-QUERY -1 <> IF 79 throw THEN
   EFFECT-DIN-N 3 <> EFFECT-DIN-CELLS 3 <> or IF 79 throw THEN
   s" PAYLOAD-POLY-USE" EFFECT-QUERY -1 <> IF 79 throw THEN
   EFFECT-DIN-N 1 <> EFFECT-DIN-CELLS 2 <> or IF 79 throw THEN
   s" PAYLOAD-ZERO-ARG-USE" EFFECT-QUERY -1 <> IF 79 throw THEN
   EFFECT-DIN-N 1 <> EFFECT-DIN-CELLS 1 <> or IF 79 throw THEN
   s" ROUND-GOOD ( n -- n ) ROW-ADD" CHECK! -1 <> IF 79 throw THEN
   s" ROUND-BAD ( ptr u8 -- ptr u8 ) ROW-ADD" CHECK! 0 <> IF 79 throw THEN
   s" ROUND-QUANT ( n -- n ) [: 1+ ;] PAYLOAD-QUANT" CHECK! -1 <> IF 79 throw THEN
   s" ROUND-QUANT-PTR ( ptr u8 -- ptr u8 ) [: ;] PAYLOAD-QUANT" CHECK! -1 <> IF 79 throw THEN
   s" ROUND-TAG ( payload-tag -- payload-tag ) PAYLOAD-TAG" CHECK! -1 <> IF 79 throw THEN
   s" ROUND-TAG-BAD ( n -- n ) PAYLOAD-TAG" CHECK! 0 <> IF 79 throw THEN
   s" ROUND-ANON-BAD ( ptr u8 -- ptr u8 ) ANON-PROVIDER execute" CHECK! 0 <> IF 79 throw THEN
   s" ROUND-RETURN ( n | -- | n ) RETURN-PROVIDER" CHECK! -1 <> IF 79 throw THEN
   s" graph metadata load: ok" type cr ;

GW-ACCEPT

;package
