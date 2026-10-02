\ Reserved initialized storage keeps its scope and element layout through the
\ real checker load path. Run with a candidate built from this source tree.

require lib/test.f
require lib/test/subject.f
require lib/adt/option.f

package C2-INIT-SCHEMA

public
PRODUCT pair 0 FIELD left n FIELD right n ;PRODUCT
ENUM tiny POLICY packed-tag red green ;ENUM
STRUCTURE scg 1 FIELD u a FIELD z n ;STRUCTURE
private

: KEEP ( init<p,pair> -- init<p,pair> ) ;
public
: KEEP-WIDE ( init<p,pair> -- init<p,pair> ) KEEP ;
private

TRUSTED: FAMILY ( ptr u8 n -- n ) {: name:ptr size:n :}
   s" c2-init-schema" name size TFAM:TFAM-RESOLVE drop ;

TRUSTED: TERM ( ptr u8 n -- n )
   FAMILY {: fam:n :}
   PARAM-SCR-N @ fam TFAM-NAME$ fam MK-PARAM ;

TRUSTED: WRAP ( n -- n ) {: elem:n :}
   s" " s" init" TFAM:TFAM-RESOLVE drop {: fam:n :}
   PARAM-SCR-N @ {: base:n :}
   0 MK-SCOPE PARAM-SCR+
   elem PARAM-SCR+
   base s" init" fam MK-PARAM ;

TRUSTED: WIDTH ( n -- n ) T-WIDTH ;
TRUSTED: PHYSICAL ( n -- n n bool ) TFAM:INIT-LAYOUT? ;

TRUSTED: APPLY ( n ptr u8 n -- n ) {: arg:n name:ptr size:n :}
   name size FAMILY {: fam:n :}
   PARAM-SCR-N @ {: base:n :}
   arg PARAM-SCR+
   base fam TFAM-NAME$ fam MK-PARAM ;

TRUSTED: OPEN ( -- n ) NEW FRESH MK-VAR ;

TRUSTED: OPTION-OF ( n -- n ) {: arg:n :}
   s" " s" option" TFAM:TFAM-RESOLVE drop {: fam:n :}
   PARAM-SCR-N @ {: base:n :}
   arg PARAM-SCR+
   base s" option" fam MK-PARAM ;

TRUSTED: READ-OPEN ( -- n )
   NEW
   s" " s" read-view" TFAM:TFAM-RESOLVE drop {: fam:n :}
   PARAM-SCR-N @ {: base:n :}
   0 MK-SCOPE PARAM-SCR+
   1 MK-SCOPE PARAM-SCR+
   FRESH MK-VAR PARAM-SCR+
   base s" read-view" fam MK-PARAM ;

$4000 constant CAP
create OUT CAP allot
create ERR CAP allot

: STATUS? ( ptr u8 n n -- bool ) {: expected:n :}
   OUT CAP >LEN ERR CAP >LEN 10000 >MS SUBJECT:RUN
   MATCH outcome
      exited OF expected = ENDOF
      signaled OF drop false ENDOF
      timeout OF false ENDOF
   ;MATCH
   >r 2drop r> ;

public

: CHECK ( -- )
   s" initialized storage follows a two-cell record" T-LABEL
   s" pair" TERM WRAP WIDTH 2 T=
   s" its physical size and alignment follow the record" T-LABEL
   s" pair" TERM WRAP PHYSICAL TTRUE CELL T= 2 CELL * T=
   s" a packed tag-only element retains byte alignment" T-LABEL
   s" tiny" TERM WRAP PHYSICAL TTRUE 1 T= 1 T=
   s" an open width-bearing field has no fixed storage layout" T-LABEL
   OPEN s" scg" APPLY WRAP PHYSICAL >r 2drop r> TFALSE
   s" nested initialized storage keeps an open width unknown" T-LABEL
   OPEN s" scg" APPLY WRAP WRAP PHYSICAL >r 2drop r> TFALSE
   s" a nested sum with an open width-bearing field stays unknown" T-LABEL
   OPEN s" scg" APPLY OPTION-OF WRAP PHYSICAL >r 2drop r> TFALSE
   s" a concrete wide field determines the complete layout" T-LABEL
   s" pair" TERM s" scg" APPLY WRAP PHYSICAL TTRUE CELL T= 3 CELL * T=
   s" an open element unused by the view layout remains placeable" T-LABEL
   READ-OPEN WRAP PHYSICAL TTRUE CELL T= 2 CELL * T=
   s" an initialized wide record retains its nominal type" T-LABEL
   s" : C2-INIT-WIDE ( init<p,C2-INIT-SCHEMA:pair> -- init<p,C2-INIT-SCHEMA:pair> ) C2-INIT-SCHEMA:KEEP-WIDE ;" 0 STATUS? TTRUE
   s" a different lifetime cannot be substituted" T-LABEL
   s" : C2-INIT-SCOPE-ERASE ( init<p,C2-INIT-SCHEMA:pair> -- init<q,C2-INIT-SCHEMA:pair> ) ;" 70 STATUS? TTRUE
   s" the element's own scope dependency survives the wrapper" T-LABEL
   s" : C2-INIT-NESTED ( init<p,read-view<q,q,u8>> -- init<p,read-view<q,q,u8>> ) ;" 0 STATUS? TTRUE
   s" the element's scope dependency cannot be substituted" T-LABEL
   s" : C2-INIT-NESTED-ERASE ( init<p,read-view<q,q,u8>> -- init<p,read-view<r,r,u8>> ) ;" 70 STATUS? TTRUE
   s" the element cannot be exposed by an identity effect" T-LABEL
   s" : C2-INIT-ERASE ( init<p,C2-INIT-SCHEMA:pair> -- C2-INIT-SCHEMA:pair ) ;" 70 STATUS? TTRUE
   s" raw cells cannot construct initialized storage" T-LABEL
   s" : C2-INIT-FORGE ( -- init<p,C2-INIT-SCHEMA:pair> ) 1 2 ;" 70 STATUS? TTRUE
   s" a cast cannot erase the initialized lifetime" T-LABEL
   s" CAST: C2-INIT-CAST ( init<p,n> -- n )" 67 STATUS? TTRUE
   s" initialized storage cannot enter a global slot" T-LABEL
   s" TYPED-VARIABLE C2-INIT-GLOBAL init<p,n>" 67 STATUS? TTRUE ;

;package

T-RESET
C2-INIT-SCHEMA:CHECK
T-REPORT
s" c2-init-schema: ok" type cr
