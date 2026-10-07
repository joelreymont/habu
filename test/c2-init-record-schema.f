\ Initialized record queries use committed field metadata from real declarations.
\ Run on the unsealed candidate through the WHITEBOX-SUITE load path.

require lib/test.f

package C2-INIT-RECORD-SCHEMA

public
PRODUCT pair 0 FIELD left n FIELD right n ;PRODUCT
PRODUCT other 0 FIELD item n ;PRODUCT
STRUCTURE nested 0 FIELD marker n FIELD body pair ;STRUCTURE
STRUCTURE scoped 2 FIELD source read-view<a,b,u8> FIELD mark n ;STRUCTURE
STRUCTURE phantom 3 FIELD source read-view<a,b,c> FIELD mark n ;STRUCTURE
STRUCTURE flexible 1 FIELD first a FIELD mark n ;STRUCTURE
STRUCTURE inner-flex 1 FIELD value a ;STRUCTURE
STRUCTURE outer-flex 1 FIELD nested inner-flex<a> ;STRUCTURE
STRUCTURE exchanged 2 FIELD first a FIELD second b ;STRUCTURE
STRUCTURE packed 0 POLICY packed-tag FIELD item n ;STRUCTURE
STRUCTURE empty 0 ;STRUCTURE
ENUM choice red blue ;ENUM

private

: FAMILY ( ptr u8 n -- n ) {: name:ptr size:n :}
   s" c2-init-record-schema" name size TFAM:TFAM-RESOLVE drop ;

: TERM ( ptr u8 n -- n )
   FAMILY {: fam:n :}
   PARAM-SCR-N @ fam TFAM-NAME$ fam MK-PARAM ;

: APPLY ( n ptr u8 n -- n ) {: arg:n name:ptr size:n :}
   name size FAMILY {: fam:n :}
   PARAM-SCR-N @ {: base:n :}
   arg PARAM-SCR+
   base fam TFAM-NAME$ fam MK-PARAM ;

: SCOPES ( ptr u8 n n n -- n )
   {: name:ptr size:n owner:n ceiling:n :}
   name size FAMILY {: fam:n :}
   PARAM-SCR-N @ {: base:n :}
   owner PARAM-SCR+ ceiling PARAM-SCR+
   base fam TFAM-NAME$ fam MK-PARAM ;

: SCOPES3 ( ptr u8 n n n n -- n )
   {: name:ptr size:n owner:n ceiling:n elem:n :}
   name size FAMILY {: fam:n :}
   PARAM-SCR-N @ {: base:n :}
   owner PARAM-SCR+ ceiling PARAM-SCR+ elem PARAM-SCR+
   base fam TFAM-NAME$ fam MK-PARAM ;

: SCOPE ( -- n ) 0 MK-SCOPE ;
: OPEN ( -- n ) NEW FRESH MK-VAR ;
: U8 ( -- n ) CC-U8 MK-CON ;
: WRAP ( n -- n ) {: elem:n :}
   s" " s" init" TFAM:TFAM-RESOLVE drop {: fam:n :}
   PARAM-SCR-N @ {: base:n :}
   0 MK-SCOPE PARAM-SCR+ elem PARAM-SCR+
   base s" init" fam MK-PARAM ;
: READ ( -- n )
   s" " s" read-view" TFAM:TFAM-RESOLVE drop {: fam:n :}
   PARAM-SCR-N @ {: base:n :}
   0 MK-SCOPE PARAM-SCR+ 0 MK-SCOPE PARAM-SCR+ U8 PARAM-SCR+
   base s" read-view" fam MK-PARAM ;
: MUT ( -- n )
   s" " s" mut-view" TFAM:TFAM-RESOLVE drop {: fam:n :}
   PARAM-SCR-N @ {: base:n :}
   0 MK-SCOPE PARAM-SCR+ 0 MK-SCOPE PARAM-SCR+
   FRESH MK-VAR PARAM-SCR+ U8 PARAM-SCR+
   base s" mut-view" fam MK-PARAM ;
: PAIR? ( n -- bool ) T-RES PARAM>FAM s" pair" FAMILY = ;
: FIELD-ID ( ptr u8 n ptr u8 n -- n )
   {: family:ptr family-len:n field:ptr field-len:n :}
   family family-len FAMILY TYPE-FIELD:NO-VARIANT field field-len TYPE-FIELD:FIND
   IF EXIT THEN s" missing committed test field" 76 die ;
: VIEW-SCOPES? ( n n n -- bool ) {: field:n owner:n ceiling:n :}
   field T-RES PARAM>FAM C2-READ-FAM @ <> IF RES-FALSE EXIT THEN
   field 0 PARAM>ARG T-RES owner =
   field 1 PARAM>ARG T-RES ceiling = and ;

public

: CHECK ( -- )
   s" a committed two-cell product has a fixed schema" T-LABEL
   s" pair" TERM TFAM:INIT-RECORD? TTRUE CELL T= 2 CELL * T= 2 T=
   s" a nested fixed product keeps its complete width" T-LABEL
   s" nested" TERM TFAM:INIT-RECORD? TTRUE CELL T= 3 CELL * T= 3 T=
   s" its nested field retains the committed physical position" T-LABEL
   s" nested" s" body" FIELD-ID s" nested" TERM TFAM:INIT-FIELD?
      TTRUE 2 CELL * T= 2 T= CELL T= PAIR? TTRUE
   s" a copied read field keeps both source scopes" T-LABEL
   SCOPE SCOPE {: p:n q:n :}
   s" scoped" p q SCOPES TFAM:INIT-RECORD? TTRUE CELL T= 3 CELL * T= 3 T=
   s" scoped" s" source" FIELD-ID s" scoped" p q SCOPES TFAM:INIT-FIELD?
      TTRUE 2 CELL * T= 2 T= 0 T= p q VIEW-SCOPES? TTRUE
   s" a view's open phantom element does not hide its width" T-LABEL
   SCOPE {: r:n :}
   OPEN {: x:n :}
   s" phantom" r r x SCOPES3 TFAM:INIT-RECORD? TTRUE CELL T= 3 CELL * T= 3 T=
   s" phantom" s" source" FIELD-ID s" phantom" r r x SCOPES3 TFAM:INIT-FIELD?
      TTRUE 2 CELL * T= 2 T= 0 T= r r VIEW-SCOPES? TTRUE
   s" ordinary fields return their instantiated type" T-LABEL
   s" pair" s" right" FIELD-ID s" pair" TERM TFAM:INIT-FIELD?
      TTRUE CELL T= 1 T= CELL T= drop
   s" an open width-bearing field is not a fixed schema" T-LABEL
   OPEN s" flexible" APPLY TFAM:INIT-RECORD? TFALSE 0 T= 0 T= 0 T=
   OPEN s" outer-flex" APPLY TFAM:INIT-RECORD? TFALSE 0 T= 0 T= 0 T=
   s" a widened field cannot use its committed placement" T-LABEL
   s" pair" TERM s" flexible" APPLY TFAM:INIT-RECORD? TFALSE 0 T= 0 T= 0 T=
   s" opposite field-width changes cannot cancel to a valid layout" T-LABEL
   s" exchanged" s" pair" TERM s" empty" TERM SCOPES TFAM:INIT-RECORD?
      TFALSE 0 T= 0 T= 0 T=
   s" pair" TERM s" flexible" APPLY
      s" flexible" s" mark" FIELD-ID swap TFAM:INIT-FIELD?
      TFALSE 0 T= 0 T= 0 T= drop
   s" field ids from another record cannot be projected" T-LABEL
   s" other" s" item" FIELD-ID s" pair" TERM TFAM:INIT-FIELD?
      TFALSE 0 T= 0 T= 0 T= drop
   s" an uncommitted field id is unknown" T-LABEL
   TYPE-FIELD:COUNT s" pair" TERM TFAM:INIT-FIELD? TFALSE 0 T= 0 T= 0 T= drop
   s" reserved read, mutable, and initialized wrappers are not records" T-LABEL
   READ TFAM:INIT-RECORD? TFALSE 0 T= 0 T= 0 T=
   MUT TFAM:INIT-RECORD? TFALSE 0 T= 0 T= 0 T=
   s" pair" TERM WRAP TFAM:INIT-RECORD? TFALSE 0 T= 0 T= 0 T=
   s" a fieldless product has no initialized schema" T-LABEL
   s" empty" TERM TFAM:INIT-RECORD? TFALSE 0 T= 0 T= 0 T=
   s" packed storage is outside the cell record surface" T-LABEL
   s" packed" TERM TFAM:INIT-RECORD? TFALSE 0 T= 0 T= 0 T=
   s" a tagged alternative is not a record" T-LABEL
   s" choice" TERM TFAM:INIT-RECORD? TFALSE 0 T= 0 T= 0 T= ;

;package

T-RESET
C2-INIT-RECORD-SCHEMA:CHECK
T-REPORT
s" c2-init-record-schema: ok" type cr
