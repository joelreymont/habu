split-str
\ check-walk.f - the checker's walk over a definition, which calls the type registry
\ and the diagnostics that load before it.

\ The longest effect a row states. The source pre-verifier refuses a longer one
\ at the same bound.
$100 constant GENR-SIG-CAP

package CHECK
EXPORT TFAM:EFFECT-ROWS-ON
EXPORT TFAM:EFFECT-TYPE
EXPORT TFAM:TFAM-GENERATOR
EXPORT TFAM:INIT-FIELD-NAME?
EXPORT TFAM:INIT-RECORD?
EXPORT TFAM:SUMV-FROM-CTOR-SYM
EXPORT TFAM:SUMV-TAG@
EXPORT TFAM:TFAM-CONSTRUCT-FAM
EXPORT TFAM:TFAM-CONSTRUCT-NODE
EXPORT TFAM:TFAM-CONSTRUCT-STEP
EXPORT TFAM:TFAM-CTOR-STEP?
EXPORT TFAM:TFAM-ENUM?
EXPORT TFAM:TFAM-MATCH-FAM
EXPORT TFAM:TFAM-MATCH-NODE
EXPORT TFAM:TFAM-MATCH-PAY
EXPORT TFAM:TFAM-MATCH-VARIANT
EXPORT TFAM:TFAM-MEMBER-STEP?
EXPORT TFAM:TFAM-NICHE?
EXPORT TFAM:TFAM-PKG-MATCH?
EXPORT TFAM:TFAM-SCRATCH?
EXPORT TFAM:TFAM-SUM?
EXPORT TFAM:TFAM-VAR-COUNT@
EXPORT TFAM:TFL-VPADS

: EV-VARIANT ( n n n -- )
   {: kind:n fam:n vid:n :}
   kind  vid SUMV-TAG@  fam vid TFL-VPADS  EVENT! ;

\ WITH-INIT is the only public entry that changes a byte view's element.
\ Its dictionary code entry authenticates this transition.
: SCOPE-INIT-STEP ( -- )
   DCUR @ R-RES dup TAG S-PUSH <> IF drop 4 C2-UNDERFLOW EXIT THEN
   {: callback-node:n :}
   callback-node P>TYPE T-RES {: supplied:n :}
   callback-node P>REST R-RES dup TAG S-PUSH <> IF drop 4 C2-UNDERFLOW EXIT THEN
   {: value-node:n :}
   value-node P>TYPE T-RES {: value-cell:n :}
   value-cell HIDDEN-PARAM? IF value-cell MK-LOGICAL ELSE value-cell THEN {: value:n :}
   value INIT-VALUE-OK? 0= IF value MD-C2-INIT-RECORD C2-REJECT EXIT THEN
   value INIT-RECORD? 0= IF
      drop drop drop value MD-C2-INIT-RECORD C2-REJECT EXIT THEN
   {: width:n bytes:n align:n :}
   value-node P>REST width 1- SCOPE-INIT-DROP R-RES dup TAG S-PUSH <> IF
      drop width 3 + C2-UNDERFLOW EXIT THEN
   {: parent-node:n :}
   parent-node P>TYPE T-RES {: parent-cell:n :}
   parent-node P>REST {: rest:n :}
   parent-cell HIDDEN-PARAM? IF parent-cell MK-LOGICAL ELSE parent-cell THEN {: parent:n :}
   parent TAG T-STALE = IF parent parent STALE-WHY C2-REJECT EXIT THEN
   parent MUT-VIEW? 0= IF
      PE-U8 C2-WANT-MUT parent C2-PARENT-MISMATCH EXIT THEN
   \ The whole view pairs, so its element slot pairs strictly: `n` is not `u8`.
   PE-U8 C2-WANT-MUT {: want:n :}
   parent want UNIFY-EXACT 0= IF want parent C2-PARENT-MISMATCH EXIT THEN
   parent 1 PARAM>ARG T-RES {: ceiling:n :}
   supplied TAG T-FORALL = IF
      supplied F>DOMAIN BIND-CHILD <>
      supplied F>PARENT 0= or supplied F>SECOND 0= or IF
         supplied MD-C2-BINDER C2-REJECT EXIT THEN
      supplied F>PARENT ceiling UNIFY-EXACT 0=
      supplied F>SECOND value UNIFY-EXACT 0= or IF
         supplied MD-C2-BINDER C2-REJECT EXIT THEN
   THEN
   ceiling value SCOPE-DOUBLE-FRESH {: scope:n :}
   supplied TAG T-FORALL = IF
      supplied F>BODY scope SCOPE-INST
      DCUR @ P>REST MK-PUSH DCUR !
   THEN
   FRESH MK-ROW {: r:n :}
   FRESH MK-ROW {: s:n :}
   FRESH MK-ROW {: u:n :}
   parent scope value SCOPE-INIT-VIEW {: view:n :}
   view r PUSH-LOGICAL view s PUSH-LOGICAL u u MK-QUOT {: callback:n :}
   callback value parent r PUSH-LOGICAL PUSH-LOGICAL MK-PUSH
   parent s PUSH-LOGICAL {: output:n :}
   output SCOPE-RECORDED-STEP
   OK @ 0= IF EXIT THEN
   scope SCOPE-EXTERIOR-XT IF
      SCOPE-WITNESS @ MD-C2-ESCAPE C2-REJECT EXIT THEN
   1 0 0 width WF-ADD-FULL
   REC-ON @ IF
      REC-IX @ width bytes CW-INIT-WIDTH CWIN-ADD
      REC-IX @ bytes align CW-INIT-EXTENT CWIN-ADD
   THEN ;

: SCOPE-RECORDS-STEP ( -- )
   DCUR @ R-RES dup TAG S-PUSH <> IF drop 5 C2-UNDERFLOW EXIT THEN
   {: callback-node:n :}
   callback-node P>TYPE T-RES {: supplied:n :}
   callback-node P>REST R-RES dup TAG S-PUSH <> IF drop 5 C2-UNDERFLOW EXIT THEN
   {: value-node:n :}
   value-node P>TYPE T-RES {: value-cell:n :}
   value-cell HIDDEN-PARAM? IF value-cell MK-LOGICAL ELSE value-cell THEN {: value:n :}
   value INIT-VALUE-OK? 0= IF value MD-C2-INIT-RECORD C2-REJECT EXIT THEN
   value INIT-RECORD? 0= IF
      drop drop drop value MD-C2-INIT-RECORD C2-REJECT EXIT THEN
   {: width:n bytes:n align:n :}
   value-node P>REST width 1- SCOPE-INIT-DROP R-RES dup TAG S-PUSH <> IF
      drop width 4 + C2-UNDERFLOW EXIT THEN
   {: count-node:n :}
   count-node P>REST R-RES dup TAG S-PUSH <> IF drop width 4 + C2-UNDERFLOW EXIT THEN
   {: parent-node:n :}
   parent-node P>TYPE T-RES {: parent-cell:n :}
   parent-node P>REST {: rest:n :}
   parent-cell HIDDEN-PARAM? IF parent-cell MK-LOGICAL ELSE parent-cell THEN {: parent:n :}
   parent TAG T-STALE = IF parent parent STALE-WHY C2-REJECT EXIT THEN
   parent MUT-VIEW? 0= IF
      PE-U8 C2-WANT-MUT parent C2-PARENT-MISMATCH EXIT THEN
   \ The whole view pairs, so its element slot pairs strictly: `n` is not `u8`.
   PE-U8 C2-WANT-MUT {: want:n :}
   parent want UNIFY-EXACT 0= IF want parent C2-PARENT-MISMATCH EXIT THEN
   parent 1 PARAM>ARG T-RES {: ceiling:n :}
   supplied TAG T-FORALL = IF
      supplied F>DOMAIN BIND-CHILD <>
      supplied F>PARENT 0= or supplied F>SECOND 0= or IF
         supplied MD-C2-RECORDS-BINDER C2-REJECT EXIT THEN
      supplied F>PARENT ceiling UNIFY-EXACT 0=
      supplied F>SECOND value UNIFY-EXACT 0= or IF
         supplied MD-C2-RECORDS-BINDER C2-REJECT EXIT THEN
   THEN
   ceiling value SCOPE-DOUBLE-FRESH {: scope:n :}
   supplied TAG T-FORALL = IF
      supplied F>BODY scope SCOPE-INST
      DCUR @ P>REST MK-PUSH DCUR !
   THEN
   FRESH MK-ROW {: r:n :}
   FRESH MK-ROW {: s:n :}
   FRESH MK-ROW {: u:n :}
   parent scope value SCOPE-RECORDS-TYPE {: records:n :}
   records r PUSH-LOGICAL records s PUSH-LOGICAL u u MK-QUOT {: callback:n :}
   parent r PUSH-LOGICAL PE-N swap MK-PUSH
   value swap PUSH-LOGICAL callback swap MK-PUSH
   parent s PUSH-LOGICAL RECORDED-STEP
   OK @ 0= IF EXIT THEN
   scope SCOPE-EXTERIOR-XT IF
      SCOPE-WITNESS @ MD-C2-ESCAPE C2-REJECT EXIT THEN
   1 0 0 width WF-ADD-FULL
   REC-ON @ IF
      REC-IX @ width bytes CW-INIT-WIDTH CWIN-ADD
      REC-IX @ bytes align CW-INIT-EXTENT CWIN-ADD
   THEN ;

: SCOPE-RECORD-STEP ( -- )
   DCUR @ R-RES dup TAG S-PUSH <> IF drop 4 C2-UNDERFLOW EXIT THEN
   dup P>TYPE T-RES {: supplied:n :}
   P>REST R-RES dup TAG S-PUSH <> IF drop 4 C2-UNDERFLOW EXIT THEN
   P>REST R-RES dup TAG S-PUSH <> IF drop 4 C2-UNDERFLOW EXIT THEN
   dup P>TYPE T-RES {: parent-cell:n :}
   P>REST {: rest:n :}
   parent-cell HIDDEN-PARAM? IF parent-cell MK-LOGICAL ELSE parent-cell THEN {: parent:n :}
   parent TAG T-STALE = IF parent parent STALE-WHY C2-REJECT EXIT THEN
   parent RECORDS? 0= IF
      C2-WANT-RECORDS parent C2-PARENT-MISMATCH EXIT THEN
   parent 3 PARAM>ARG {: value:n :}
   value INIT-VALUE-OK? 0= IF value MD-C2-INIT-RECORD C2-REJECT EXIT THEN
   value INIT-RECORD? 0= IF
      drop drop drop value MD-C2-INIT-RECORD C2-REJECT EXIT THEN
   {: width:n bytes:n align:n :}
   parent 1 PARAM>ARG T-RES {: ceiling:n :}
   supplied TAG T-FORALL = IF
      supplied F>DOMAIN BIND-CHILD <> supplied F>PARENT 0= or IF
         supplied MD-C2-CHILD-BINDER C2-REJECT EXIT THEN
      supplied F>PARENT ceiling UNIFY-EXACT 0= IF
         supplied MD-C2-CHILD-BINDER C2-REJECT EXIT THEN
   THEN
   ceiling SCOPE-CHILD-FRESH {: scope:n :}
   supplied TAG T-FORALL = IF
      supplied F>BODY scope SCOPE-INST
      DCUR @ P>REST MK-PUSH DCUR !
   THEN
   FRESH MK-ROW {: r:n :}
   FRESH MK-ROW {: s:n :}
   FRESH MK-ROW {: u:n :}
   parent scope SCOPE-RECORD-VIEW {: view:n :}
   view r PUSH-LOGICAL view s PUSH-LOGICAL u u MK-QUOT {: callback:n :}
   parent r PUSH-LOGICAL PE-N swap MK-PUSH callback swap MK-PUSH
   parent s PUSH-LOGICAL RECORDED-STEP
   OK @ 0= IF EXIT THEN
   scope SCOPE-EXTERIOR-XT IF
      SCOPE-WITNESS @ MD-C2-ESCAPE C2-REJECT EXIT THEN
   1 0 0 width WF-ADD-FULL
   REC-ON @ IF
      REC-IX @ width bytes CW-INIT-WIDTH CWIN-ADD
      REC-IX @ bytes align CW-INIT-EXTENT CWIN-ADD
   THEN ;

\ WITH-FIELD lends from a view of an initialized record.
: FIELD-PARENT-MISMATCH ( n -- ) {: parent:n :}
   FRESH MK-VAR FRESH MK-VAR SCOPE-INIT-ELEM C2-WANT-MUT
   parent C2-PARENT-MISMATCH ;

\ The selector is a token, never a runtime field id. The callback may change
\ the field, but it cannot return the child view or the projected region.
: SCOPE-FIELD-STEP ( str -- ) {: name:^ size:n :}
   DCUR @ R-RES dup TAG S-PUSH <> IF drop 3 C2-UNDERFLOW EXIT THEN
   dup P>TYPE T-RES {: supplied:n :}
   supplied SCOPE-FIELD-MAY-THROW? {: may-throw:bool :}
   supplied TAG T-QUOT = IF supplied INH-CHARGE THEN
   P>REST R-RES dup TAG S-PUSH <> IF drop 3 C2-UNDERFLOW EXIT THEN
   dup P>TYPE T-RES {: parent-cell:n :}
   P>REST {: rest:n :}
   parent-cell HIDDEN-PARAM? IF parent-cell MK-LOGICAL ELSE parent-cell THEN {: parent:n :}
   parent TAG T-STALE = IF parent parent STALE-WHY C2-REJECT EXIT THEN
   parent MUT-VIEW? 0= IF parent FIELD-PARENT-MISMATCH EXIT THEN
   parent 3 PARAM>ARG T-RES {: initialized:n :}
   initialized TAG T-PARAM <> IF parent FIELD-PARENT-MISMATCH EXIT THEN
   initialized PARAM>FAM C2-INIT-FAM @ <> IF
      parent FIELD-PARENT-MISMATCH EXIT THEN
   initialized 0 PARAM>ARG T-RES {: init-scope:n :}
   initialized 1 PARAM>ARG T-RES {: record:n :}
   record INIT-VALUE-OK? 0= IF record MD-C2-INIT-RECORD C2-REJECT EXIT THEN
   record INIT-RECORD? nip nip nip 0= IF
      record MD-C2-INIT-RECORD C2-REJECT EXIT THEN
   record name size INIT-FIELD-NAME? 0= IF
      2drop drop record MD-C2-FIELD-UNKNOWN C2-REJECT EXIT THEN
   {: descriptor:n off:n bytes:n :}
   descriptor FIELD-INNER {: field:n :}
   parent 1 PARAM>ARG T-RES {: ceiling:n :}
   supplied ceiling SCOPE-FIELD-SCHEME? 0= IF
      supplied MD-C2-FIELD-BINDER C2-REJECT EXIT THEN
   ceiling SCOPE-CHILD-FRESH {: child:n :}
   parent descriptor SCOPE-FIELD-REGION {: projected:n :}
   supplied ceiling child projected SCOPE-FIELD-OPEN
   OK @ 0= IF EXIT THEN
   DCUR @ XE-PD !  RCUR @ XE-PR !
   parent child projected init-scope field SCOPE-FIELD-CALLBACK
   OK @ 0= IF EXIT THEN
   child SCOPE-EXTERIOR-XT projected SCOPE-EXTERIOR-XT or IF
      SCOPE-WITNESS @ MD-C2-ESCAPE C2-REJECT EXIT THEN
   THROW-CUR? may-throw or IF CALL-THROW-EDGE THEN
   off bytes SCOPE-FIELD-FACTS ;

: SCOPE-FIELD-TOK ( str -- )
   EV-OPERAND                       \ the loaned field's name; the loan step compiles it
   0 FIELD-LOAN-PENDING !
   SCOPE-FIELD-STEP ;

: DO-TOK-BODY ( str -- ) {: a:^ u:n :}
   0 CURSYM !
   a u LITERAL-TOK? IF EXIT THEN
   a u TOK-INTRINSIC {: id:n :}
   a u BIND-SOURCE-CALL
   TOP-STEP @ 0= IF TOK-SYM @ DECLARATION-SYM? IF REJECT-UNSAFE EXIT THEN THEN
   \ Read before an identity arm takes the token (`create` keeps its axiom). Only
   \ a call that runs when the word runs gives facts: an admitted immediate does not.
   TSTART @ u POSTPONED-REC NULL-PTR <>
   data-base BODY-ITEMS:EFFECT-CELL + @ 0 <>  and {: built:bool :}
   a u TOK-SYM @ IMM-READS 0 <  POST-ON @ 0 <>  or  built or IF
      TOK-SYM @ CTL-FLAGS-SYM CTL-INHERITED and INHSET @ or INHSET !
   THEN
   id DEFINER-TOK IF EXIT THEN
   id INTRINSIC-CELL-FETCH = IF CELL-FETCH-TOK EXIT THEN
   id INTRINSIC-CELL-STORE = IF RES-TRUE CELL-STORE-TOK EXIT THEN
   id INTRINSIC-ONE-CELL-STORE = IF RES-FALSE CELL-STORE-TOK EXIT THEN
   id INTRINSIC-ATOMIC-CAS = IF ATOMIC-CAS-TOK EXIT THEN
   id INTRINSIC-RAW-FIELD = IF RAW-FIELD-TOK? IF EXIT THEN THEN
   TOK-SYM @ CURSYM !
   CURSYM @ SCOPE-KIND-SYM dup 2 = IF SCOPE-CHILD-STEP EXIT THEN
   dup 3 = IF drop SCOPE-MUT-ROOT-STEP EXIT THEN
   dup 4 = IF SCOPE-CHILD-STEP EXIT THEN
   dup 5 = IF drop SCOPE-INIT-STEP EXIT THEN
   dup 6 = IF drop SCOPE-RECORDS-STEP EXIT THEN
   dup 7 = IF drop SCOPE-RECORD-STEP EXIT THEN
   dup 8 = IF drop REC-IX @ FIELD-LOAN-ORD ! TSTART @ FIELD-LOAN-OFF !
      -1 FIELD-LOAN-PENDING ! EXIT THEN drop
   \ a call to a definer, counted for the straight-line-wrapper rule above. A
   \ top-level step's is not: it walks no body, and the next record published is
   \ the one the definer makes when it runs, which is no wrapper.
   TOP-STEP @ 0= IF
      CURSYM @ NORET-CREATES@ dup 0 <> IF
         WRAPC !  WRAPN @ 1 + WRAPN !  WRAP-MADE!
      ELSE drop THEN
   THEN
   FEP-CLEAR
   CURSYM @ CHECKER-FIND-USIG-SYM drop
   CURSYM @ TFAM-CTOR-STEP? IF EXIT THEN
   CURSYM @ TFAM-MEMBER-STEP? IF EXIT THEN
   FEP-HIT? IF
      CURSYM @ CALL-AUTHORITY
      dup CALL-REFUSED = IF drop -1 CAPREQ !  0 OK !  -1 FAILSET !  EXIT THEN
      dup CALL-RECOVERS = IF RES-TRUE RECOVERY-USED! THEN
      CALL-PRIMS <> IF FEP @ EFF-APPLY EXIT THEN
   THEN
   CURSYM @ TRY-PRIMS IF EXIT THEN
   TSEEN @ 0 <> IF TCLOSE @ E-PTR EFF-APPLY EXIT THEN
   \ The load compiles a word its lookup found, though no checker symbol or row
   \ types it, so only the check fails the call.
   TOK-LIVE-REC @ IF a u CALL-UNDEFINED drop EXIT THEN
   a u REFUSE-UNDEFINED ;

\ After a step of the engine's `throw` from stack row pre: when the code it
\ popped is one a catch answered, the token's event takes that catch's ordinal
\ as a1.
: THROW-ON-EVENT ( n -- ) {: pre:n :}
   TOK-CTL @ CTL-CORE-OP and 0= IF EXIT THEN
   pre R-RES dup TAG S-PUSH <> IF drop EXIT THEN
   P>PROOF dup 0= IF drop EXIT THEN
   dup CP-KIND CP-CODE <> IF drop EXIT THEN
   CURSYM @ s" throw" PE-SYM-OF <> IF drop EXIT THEN
   CP-TYPE EV-A1 ! ;

: DO-TOK ( str -- ) {: a:^ u:n :}
   DCUR @ {: pre:n :}  RCUR @ {: prer:n :}
   a u DO-TOK-BODY
   CURSYM @ 0 <> IF  pre NZ-CALL-CELLS  pre THROW-ON-EVENT  THEN
   pre prer ZERO-USE-TEST ;

\ Open the frames of a match on family FAM: its branches start from MTCH-ROW,
\ their payloads come from MTCH-TAGT (0: none), and LOST says whether a
\ scrutinee cell was lost.
: MATCH-OPEN ( n n -- )
   {: fam:n lost:n :}
   MF-ENSURE
   MF-DEPTH @ 1 + MF-DEPTH !
   MF-CUR
   {: r:^ :}
   fam r MF.FAM !
   MTCH-TAGT @ r MF.TERM !
   MTCH-ROW @ r MF.BASE !
   RCUR @ r MF.RBASE !
   0 r MF.OUT !  0 r MF.ROUT !  0 r MF.HAS !
   fam TFAM-VAR-COUNT@ dup r MF.VCNT !
   MSEEN-ALLOC r MF.SEEN !
   TOKIX @ r MF.TIX !
   MTCH-ROW @ DCUR !                         \ bundle popped; branches re-derive their rows
   9 DCUR @ 0 RCUR @ 0 CF-PUSH
   lost CF-TOP CF.FT !
   2 CHECKER-MATCH-MODE ! ;

\ A scrutinee that does not fit refuses the match at its family token, but the
\ family still gives the match's tokens their roles: the walk reads its
\ variants and compiles its branch words as the load does, each branch from the
\ row the scrutinee left and binding no payload.
: MATCH-SCRUT-FAIL ( n -- )
   {: fam:n :}
   fam MATCH-SCRUT-DIAG
   0 OK !  -1 FAILSET !  -1 MREJ !
   DCUR @ MTCH-ROW !  0 MTCH-TAGT !
   fam 0 MATCH-OPEN ;

EXPORT CHECKER-TAPE:K-MATCH

: MATCH-FAM-STEP ( n -- )
   {: fam:n :}
   K-MATCH fam 0 EVENT!
   fam MATCH-SCRUT? 0= IF fam MATCH-SCRUT-FAIL EXIT THEN
   DCUR @ MTCH-W @ ROW-TOP-LOST {: lost:n :}
   MTCH-W @ MWIN-CELLS!                      \ the bundle a cell-accurate consumer has to pop
   fam lost MATCH-OPEN ;

: MATCH-FAM-FOUND ( n bool -- )
   0= IF drop K-MATCH -1 0 EVENT!  MATCH-ABANDON EXIT THEN
   MATCH-FAM-STEP ;

: MATCH-FAM-TOK ( str -- )
   {: a:^ u:n :}
   a u TFAM-MATCH-FAM MATCH-FAM-FOUND ;

EXPORT CHECKER-TAPE:K-MATCH-END

: MATCH-END ( -- )
   K-MATCH-END MF-CUR MF.FAM @ 0 EVENT!
   MATCH-SEMI ;

EXPORT CHECKER-TAPE:K-MATCH-ARM

\ An arm whose variant the matched family does not declare.
: MATCH-VARIANT-NONE ( -- )
   K-MATCH-ARM -1 0 EVENT!  MD-VAR-UNKNOWN MDIAG-COMPILE!
   0 OK !  -1 MREJ !  -1 MPEND !  3 CHECKER-MATCH-MODE ! ;

: MATCH-VARIANT ( str -- )
   {: a:^ u:n :}
   a u MF-CUR MF.FAM @ TFAM-MATCH-VARIANT 0= IF
      drop MATCH-VARIANT-NONE EXIT THEN
   {: vid:n :}
   K-MATCH-ARM  MF-CUR MF.FAM @  vid EV-VARIANT
   vid SUMV-TAG@ {: tag:n :}
   MF-CUR MF.SEEN @ tag MSEEN-GET IF MD-VAR-DUP MDIAG! 0 OK !  -1 MREJ ! THEN
   MF-CUR MF.SEEN @ tag MSEEN-SET
   vid MPEND !
   3 CHECKER-MATCH-MODE ! ;

: MATCH-VARIANT-TOK ( str -- )
   {: a:^ u:n :}
   a u s" ;match" CORE-STR= IF MATCH-END EXIT THEN
   a u MATCH-VARIANT ;

EXPORT CHECKER-TAPE:K-MATCH-OF

\ The arm's `of` when true, else the token that stands in its place.
: MATCH-OF ( bool -- )
   IF K-MATCH-OF 0 0 EVENT!
   ELSE MD-MISSING-OF MDIAG-COMPILE! 0 OK !  -1 MREJ ! THEN
   MF-CUR {: r:^ :}
   r MF.BASE @ DCUR !
   r MF.RBASE @ RCUR !
   CF-ENTRY-FACTS
   MPEND @ 0 < 0=  r MF.TERM @ 0 <>  and IF
      MPEND @  r MF.TERM @  DCUR @  TFAM-MATCH-PAY
      SPLIT-MARKED DCUR !
      CF-TOP CF.FT @ 0 <> IF
         DCUR @ ROW-TERMS r MF.BASE @ ROW-TERMS - 0 max {: count:n :}
         DCUR @ count -1 ROW-TOP-OUTPUT-LOSS DCUR !
      THEN
      DCUR @ r MF.BASE @ MATCH-PAYLOAD-RECORD
   THEN
   10  r MF.BASE @  0  r MF.RBASE @  0  CF-PUSH
   0 DEADP !
   0 CHECKER-MATCH-MODE ! ;

: MATCH-OF-TOK ( str -- ) s" of" CORE-STR= MATCH-OF ;

: MATCH-TOK ( str -- ) {: a:^ u:n :}
   CHECKER-MATCH-MODE @ 1 = IF a u MATCH-FAM-TOK EXIT THEN
   CHECKER-MATCH-MODE @ 2 = IF a u MATCH-VARIANT-TOK EXIT THEN
   CHECKER-MATCH-MODE @ 4 = IF a u MATCH-SKIP-TOK EXIT THEN
   a u MATCH-OF-TOK ;

\ A failed family resolve still consumes the variant token (CONFAM -1): the
\ three-token form always captures whole, so the trailing operand never falls
\ through to locals or word lookup.
: CONSTRUCT-FAM-FOUND ( n bool -- )
   IF CONFAM ! ELSE drop 0 OK !  -1 CONFAM !  CONSTRUCT-NONE THEN
   2 CONM ! ;

: CONSTRUCT-TOK ( str -- )
   {: a:^ u:n :}
   EV-OPERAND                          \ the family compiles nothing, nor does a variant of no family
   CONM @ 1 = IF a u TFAM-CONSTRUCT-FAM CONSTRUCT-FAM-FOUND EXIT THEN
   CONFAM @ 0 < 0= IF
      a u CONFAM @ TFAM-MATCH-VARIANT IF                           \ latch variant SUMV id for the mismatch capture
         dup CVLIVE !  K-CONSTRUCT CONFAM @ rot EV-VARIANT
      ELSE drop CONSTRUCT-NONE THEN
      a u CONFAM @ TFAM-CONSTRUCT-STEP 0= IF 0 OK ! THEN
      -1 CVLIVE !                                                      \ variant leaves scope with the step
   THEN
   0 CONM ! ;

\ The call, judged as a call the body spelled.
: POSTPONE-CALL ( ^n n -- ) {: run:^ sym:n :}
   -1 POST-ON !
   run sym CALL-PRESET
   DCUR @ XE-PD !  RCUR @ XE-PR !
   run CK-REC-NAME$ DO-TOK
   OK @ IF THROW-CUR? IF CALL-THROW-EDGE THEN THEN ;

: POSTPONE-IMM ( ^n -- ) {: rec:^ :}
   rec FACTS-READ-XT
   rec REC-HOME-SYM {: sym:n :}
   sym 0= IF POSTPONE-UNDEFINED EXIT THEN
   K-OPERAND POST-KIND !  0 POST-A0 !
   rec sym POSTPONE-CALL ;

\ compile,'s input is a quotation of any effect: W's effect is judged where
\ the made word is checked.
: POSTPONE-TOK ( -- )
   IS-TARGET-TOK? 0= IF IS-FAIL EXIT THEN
   -1 CRTGT !                              \ a refusal names the operand, as a tick's does
   POSTPONE-TARGET {: rec:^ sym:n :}
   rec NULL-PTR = IF POSTPONE-UNDEFINED EXIT THEN
   rec POSTPONE-IMM?  sym SOURCE-IMM? or IF rec POSTPONE-IMM EXIT THEN
   sym DECLARATION-SYM? IF TICK-PIN REJECT-UNSAFE EXIT THEN
   sym TICK-REFUSED? IF EXIT THEN
   K-TICK POST-KIND !
   rec REC>INDEX POST-A0 !
   FREE-QUOT BTICK-PUSH
   COMPILE-CALL-REC POSTPONE-CALL ;

EXPORT CHECKER-PREFLIGHT:BODY-TOK?

\ Whether the call is of an immediate the body rejects, by REC, or by the spelling
\ for a text token (REC NULL-PTR); never for an item of a `define-word` body.
: CALL-IMM? ( ^n -- bool ) {: rec:^ :}
   rec NULL-PTR = IF TKF TKFU @ BODY-TOK? EXIT THEN
   data-base BODY-ITEMS:EFFECT-CELL + @ 0 <> IF RES-FALSE EXIT THEN
   rec POSTPONE-IMM? 0= IF RES-FALSE EXIT THEN
   rec CK-REC-NAME$ rec REC-HOME-SYM IMM-READS 0 < ;

\ An admitted immediate reading N operands runs while the body compiles, unless
\ `define-word` opened the body: none of its steps are the body's.
: IMM-STEP ( n -- ) {: n:n :}
   0 CURSYM !
   OPERANDS-SPENT @ 0 =  n 0 >  and IF n OPERANDS-SKIP THEN ;

\ The steps of a token no keyword, mode or literal form took, a call's or a
\ number's; a call's binding is made on the first ask.
: CALL-STEP ( str ^n -- ) {: a:^ u:n rec:^ :}
   TKF TKFU @ SPELLED-STEP? IF EXIT THEN
   rec CALL-IMM? IF REJECT-IMMEDIATE THEN
   a u TOK-SYM @ IMM-READS  dup 0 < 0=  data-base BODY-ITEMS:EFFECT-CELL + @ 0=  and IF
      IMM-STEP EXIT
   THEN drop
   DCUR @ XE-PD !  RCUR @ XE-PR !          \ the pre-op rows ROW-COMMON needs below
   0 CALL-DEAD !
   TKF TKFU @ DO-TOK
   OK @ IF THROW-CUR? IF CALL-THROW-EDGE THEN THEN
   OK @ IF DEAD-CUR?  CALL-DEAD @ 0 <> or IF a u DEAD-OWNER! -1 DEADP ! THEN THEN
   OK @ #CFC @ 0 > and IF BARRIER-CUR? IF ALL-CF-UNIFORM? 0= IF REJECT-DIVBAR THEN THEN THEN
   LITERAL-PAYLOAD-STEP ;

\ The step a dead token, a locals group's token or a field loan's name takes
\ before any other; true when it took one. A is its unfolded spelling.
: MODE-TOK? ( ^u8 -- bool ) {: a:^ :}
   TKF TKFU @ LIVE-TOKEN? 0= IF
      -1 DEADERR !  0 OK !  TKF TKFU @ DEAD-TOK  RES-TRUE EXIT
   THEN
   LMODE @ IF a TKF TKFU @ LOC-TOK  RES-TRUE EXIT THEN
   FIELD-LOAN-PENDING @ IF TKF TKFU @ SCOPE-FIELD-TOK  RES-TRUE EXIT THEN
   RES-FALSE ;

: DO-TOK1 {: a u :}
   a u DO-TOK-HEAD
   {: unj ok-in unck-in pin-in :}
   TOK0 @ IF NAME-TOK ELSE
   a MODE-TOK? IF ELSE
   CONM @ 0 <> IF TKF TKFU @ CONSTRUCT-TOK ELSE
   CHECKER-MATCH-MODE @ 0 <> IF
      CHECKER-MATCH-MODE @ 4 = IF LITERAL-PAYLOAD-STEP THEN
      TKF TKFU @ MATCH-TOK
   ELSE
   a u BODY-LOC-REF? IF ELSE
   TKF TKFU @ PARSE-LIT? IF TKF TKFU @ CHAR-LIT-TOK ELSE
   TKF TKFU @ PKG-LIT? IF TKF TKFU @ PKG-LIT-TOK ELSE
   TKF TKFU @ s" [type]" CORE-STR= IF TYPE-LIT-TOK ELSE
   TKF TKFU @ s" construct" CORE-STR= IF CONSTRUCT-BEGIN ELSE
   TKF TKFU @ s" match" CORE-STR= IF MATCH-BEGIN ELSE
   TKF TKFU @ s" {:" CORE-STR= IF LOC-BEGIN ELSE
   a u RETIRED-GLOBAL? IF REJECT-RETIRED ELSE
   TKF TKFU @ s" is" CORE-STR= IF IS-TOK ELSE
   TKF TKFU @ BTICK-CAND? IF BTICK-TOK ELSE
   TKF TKFU @ s" postpone" CORE-STR= IF POSTPONE-TOK ELSE
   TKF TKFU @ CF-CODE {: cf:n :}
   OK @ IF cf 21 = IF a u DEAD-OWNER! THEN THEN   \ exit
   OK @ IF cf 22 = IF a u DEAD-OWNER! THEN THEN   \ leave
   OK @ IF cf 12 = IF a u DEAD-OWNER! THEN THEN   \ again
   cf 0 >= IF cf CF-STEP THEN
   cf 0 < IF a u NULL-PTR CALL-STEP THEN
   THEN THEN THEN THEN THEN THEN THEN THEN THEN THEN THEN THEN THEN THEN THEN
   a u unj ok-in unck-in pin-in DO-TOK-TAIL ;

\ The rows ( din dout rin rout ) of a schema effect, unchecked.
: EFFECT-ROWS ( n -- n n n n ) {: node:n :}
   SIG-UNRES-CLEAR
   PKRESET NMAP-RESET ROWMAP-RESET FAM-RESET  0 SQUOT-N !  0 BIND-DEPTH !
   0 SIG-LEVEL !  0 SGHASR !  0 RR-SHARED !
   FRESH MK-ROW dup PD-BASE ! {: dr :}
   ATOMN @ PD-ATOM0 !
   node dr EFFECT-ROWS-ON
   PD-OUT !  PD-IN !  0 PR-IN !  0 PR-OUT !
   PD-IN @ PD-OUT @ 0 0 ;

\ EFFECT-ROWS checked by a written signature's rules, or a generated
\ constructor's when CTOR; a rejection sets SGBAD.
: EFFECT-SIG ( n bool -- n n n n ) {: node:n ctor:bool :}
   node EFFECT-ROWS 2drop 2drop
   SGBAD-CLEAR
   NULL-PTR 0 PD-IN @ PD-OUT @ 0 0 RES-FALSE ctor SIG-ROWS-CHECK
   PD-IN @ PD-OUT @ 0 0 ;

\ Whether A U names a generated variant constructor.
: CTOR-NAME? ( str -- bool )
   CHECKER-RECORD-SYM? SUMV-FROM-CTOR-SYM nip ;

\ Whether NODE's rows pass the rules word A U's signature must pass.
: EFFECT-RULES? ( str n -- bool ) {: a:^ u:n node:n :}
   node a u CTOR-NAME? EFFECT-SIG 2drop 2drop SGBAD @ 0= ;

\ Whether the text walked is the pending definition's body text.
: PEND-TEXT? ( -- bool )
   data-base PEND-CELL + @ 0= IF RES-FALSE EXIT THEN
   BODY-TEXT? ;

\ The effect `define-word` gave the pending definition when the text walked is
\ that definition's, else 0.
: BUILT-EFFECT ( -- n )
   PEND-TEXT? 0= IF 0 EXIT THEN
   data-base BODY-ITEMS:EFFECT-CELL + @ ;

\ The declared rows of the definition CHECK is on: from its effect when
\ `define-word` built it, else from the signature text at SGA.
: PENDING-SIG ( -- n n n n )
   BUILT-EFFECT {: eff:n :}
   eff 0 <> IF eff NMA @ NMU @ CTOR-NAME? EFFECT-SIG EXIT THEN
   SGA @ SGU @ PARSE-SIG-RAW ;

\ Records the pending definition's declared effect under name NA, AT the name
\ as read: its rows from PENDING-SIG, its text the signature at SGA.
: CHECKER-USIG-CERT-ADD-PENDING ( str str bool -- )
   {: na:^ nu:n at:^ atu:n external:bool :}
   na nu at atu USIG-CERT-ADMIT
   NEW
   SGBAD-CLEAR
   PENDING-SIG {: din:n dout:n rin:n rout:n :}
   SGA @ SGU @  at atu
   din dout rin rout external USIG-ADD-ROWS drop
   DL-TAKE ;

\ The name of the program's entry CHECK:ENTRY! states, none when empty.
PTR-VARIABLE CK-ENTRY-A   variable CK-ENTRY-U

public
\ States the name of the program's entry, which an app build calls; an empty
\ name states none. The name must outlive the load: it is kept, not copied.
: ENTRY! ( str -- ) {: a:^ u:n :}
   a CK-ENTRY-A !  u CK-ENTRY-U ! ;
private

\ Rejects, at its signature in TSTART..TI, a global definition of the stated
\ entry whose declared effect is not ( -- ).
: ENTRY-SIG-CK ( -- )
   CK-ENTRY-U @ 0= IF EXIT THEN
   NMA @ NMU @ CK-ENTRY-A @ CK-ENTRY-U @ CORE-STR=CI 0= IF EXIT THEN
   SGA @ SGU @ SIG-TRIM s" --" CORE-STR= IF EXIT THEN
   AUTHORITY {: pa:^ pu:n mode:n ok:bool :}
   ok 0=  mode CHECKER-PACKAGE-NONE <>  or IF EXIT THEN
   NMOFF @ TADDR NMU @ {: na:^ nu:n :}
   TSTART @ 1 - {: from:n :}
   TI @ 1 + TBLEN @ min from - {: eu:n :}
   from TADDR {: ea:^ :}
   ea eu RF-REJECTED PIN-REFUSED!
   s" E-MISMATCH" REJECTION:OPEN
   s" the entry `" REJECTION:EMIT-STR  na nu REJECTION:EMIT-STR
   s" ` must be ( -- ); it declares " REJECTION:EMIT-STR  ea eu REJECTION:EMIT-STR
   ea eu REJECTION:AT  s" the entry's declared effect" REJECTION:EMIT-STR
   REJECTION:HELP
   s" declare `: " REJECTION:EMIT-STR  na nu REJECTION:EMIT-STR
   s"  ( -- )` and read inputs inside it" REJECTION:EMIT-STR
   EXIT-STATUS:REJECTED REJECTION:STOP ;

\ Installs the '( ... )' in TSTART..TI when it is the first after the name, the
\ signature (a built definition's from its effect); true when that ends the walk.
: PAREN-SIG? ( -- bool )
   VSIG @  SGSEEN @ 0= and  TOKIX @ 2 < and  IF
      TSTART @ TADDR SGA !  TI @ TSTART @ - SGU !
      ENTRY-SIG-CK
      PENDING-SIG SIG-INSTALL EXIT
   THEN
   RES-FALSE ;

\ What a walk does around the step of the token at TSTART..TI: the tape's
\ fields and the cursor before it, answering TOK0, then the summary and the
\ report after, answering whether the walk ends. A walk with no tape still
\ rejects an over-long counted literal, so the opener's span and the payload
\ mark are kept on every walk.
: SCAN-TOK-HEAD ( -- n )
   TI @ TSTART @ - SCAN-U !  0 SPAY-ON !
   ARMED @ IF
      TOK0 @ SCAN-TOK0 !  0 CPAY-ON !  0 PPAY-ON !  0 POST-ON !  0 TPAY-ON !
      0 IS-PEND !  0 EV-HIT !
   THEN
   CUR-TOKEN
   SPAY-S @ SPAY-ARMED !  0 SPAY-S !
   TOK0 @ ;

: SCAN-TOK-TAIL ( n -- bool ) {: was-name:n :}
   TORDER-SCAN-UNKNOWN @ IF RES-TRUE EXIT THEN
   was-name 0= IF TSTART @ TADDR TI @ TSTART @ - ZSUMMARY-TOKEN THEN
   ARMED @ IF SCAN-REPORT ELSE
      SPAY-ON @ IF SPAY-BYTES SPAY-COUNTED-CK 2drop THEN
   THEN
   RES-FALSE ;

EXPORT CHECKER-ITEMS:ITEM-CELL@

: ITEM-SPAN ( n -- ) {: i:n :}
   i BODY-ITEMS:OFF ITEM-CELL@ TSTART !
   TSTART @  i BODY-ITEMS:LEN ITEM-CELL@ +  TI ! ;

\ A signature's span holds its parentheses, and the walk's paren branch reads
\ inside them.
: ITEM-SIG ( n -- bool ) {: i:n :}
   i BODY-ITEMS:OFF ITEM-CELL@ TI !
   CUR-PAREN-AT
   TI @ 1 + TSTART !
   TI @  i BODY-ITEMS:LEN ITEM-CELL@ +  1 - TI !
   PAREN-SIG? IF RES-TRUE EXIT THEN
   TI @ 1 + TI !
   CUR-PAREN
   RES-FALSE ;

\ A construct's variant that names none of its family's, or follows a family
\ that resolved nothing.
: CONSTRUCT-VARIANT-NONE ( -- )
   EV-OPERAND
   CONFAM @ 0 < 0= IF
      CONSTRUCT-NONE  MD-CON-VAR MDIAG-COMPILE!  0 OK !
   THEN
   0 CONM ! ;

\ The step an item of kind KIND takes at the family or the variant an open
\ construct awaits; V is a TYPE's schema node.
: ITEM-CONSTRUCT ( n n -- ) {: v:n kind:n :}
   CONM @ 2 = IF
      kind BODY-ITEM-KIND:OPERAND = IF TKF TKFU @ CONSTRUCT-TOK EXIT THEN
      CONSTRUCT-VARIANT-NONE EXIT
   THEN
   EV-OPERAND
   kind BODY-ITEM-KIND:TYPE = IF v TFAM-CONSTRUCT-NODE
   ELSE MD-CON-FAM MDIAG-COMPILE!  0 RES-FALSE THEN
   CONSTRUCT-FAM-FOUND ;

\ An item's step where an open match awaits its family, variant or `of`, or
\ inside an abandoned one, which only `match` or `;match` nests or ends.
: ITEM-MATCH ( n n -- ) {: v:n kind:n :}
   CHECKER-MATCH-MODE @ {: m:n :}
   kind BODY-ITEM-KIND:KEYWORD = {: kw:bool :}
   m 1 = IF
      kind BODY-ITEM-KIND:TYPE = IF v TFAM-MATCH-NODE
      ELSE MD-FAM-UNKNOWN MDIAG-COMPILE!  0 RES-FALSE THEN
      MATCH-FAM-FOUND EXIT
   THEN
   m 2 = IF
      kind BODY-ITEM-KIND:OPERAND = IF TKF TKFU @ MATCH-VARIANT EXIT THEN
      kw TKF TKFU @ s" ;match" CORE-STR= and IF MATCH-END EXIT THEN
      MATCH-VARIANT-NONE EXIT
   THEN
   m 3 = IF kw TKF TKFU @ s" of" CORE-STR= and MATCH-OF EXIT THEN
   kw IF TKF TKFU @ MATCH-SKIP-TOK ELSE EV-OPERAND THEN ;

\ The step an item takes while a construct or a match is open, decided by
\ its kind, never by its spelling's lookup; true when one is open.
: ITEM-OPEN? ( n n -- bool ) {: v:n kind:n :}
   CONM @ 0 <> IF v kind ITEM-CONSTRUCT RES-TRUE EXIT THEN
   CHECKER-MATCH-MODE @ 0 = IF RES-FALSE EXIT THEN
   v kind ITEM-MATCH RES-TRUE ;

: ITEM-CALL ( str n -- ) {: a:^ u:n ix:n :}
   ix REC-AT {: rec:^ :}
   rec TKF TKFU @ BIND-RECORD
   a u rec CALL-STEP ;

: ITEM-INT ( n -- ) {: v:n :}
   BIND-CLEAR
   0 CURSYM !
   v INT-STEP ;

\ A spelling no step of its item's kind takes names nothing.
: ITEM-UNDEFINED ( -- )
   TKF TKFU @ REFUSE-UNDEFINED  IS-FAIL ;

\ A TYPE or OPERAND item outside the form that takes it.
: ITEM-STRAY? ( n -- bool ) {: kind:n :}
   kind BODY-ITEM-KIND:TYPE =  kind BODY-ITEM-KIND:OPERAND = or ;

: ITEM-KEYWORD ( str -- ) {: a:^ u:n :}
   TKF TKFU @ s" construct" CORE-STR= IF CONSTRUCT-BEGIN EXIT THEN
   TKF TKFU @ s" match" CORE-STR= IF MATCH-BEGIN EXIT THEN
   TKF TKFU @ s" {:" CORE-STR= IF LOC-BEGIN EXIT THEN
   TKF TKFU @ CF-CODE {: cf:n :}
   cf 0 < IF ITEM-UNDEFINED EXIT THEN
   OK @ IF cf 21 = cf 22 = or cf 12 = or IF a u DEAD-OWNER! THEN THEN   \ exit, leave, again
   cf CF-STEP ;

\ An item of kind KIND inside a `{:` group: a LOCAL item opens a local of
\ type node V's term under its listing name A U, and `:}` binds the group.
: ITEM-IN-GROUP ( n n str -- ) {: v:n kind:n a:^ u:n :}
   NZ-MOVED
   kind BODY-ITEM-KIND:LOCAL = IF
      a u LOC-OPEN
      a u  v EFFECT-TYPE LOC-TYPED
      #LOC @ 1 + #LOC ! EXIT
   THEN
   kind BODY-ITEM-KIND:KEYWORD =  TKF TKFU @ s" :}" CORE-STR= and IF LOC-CLOSE EXIT THEN
   ITEM-UNDEFINED ;

\ The slot of the local in scope whose bind sequence is SEQ, else -1.
: SEQ-SLOT ( n -- n ) {: seq:n :}
   #LOC @ BEGIN dup 0 > WHILE
      1 -  dup cells LOCSEQIX + @ seq = IF EXIT THEN
   REPEAT drop -1 ;

\ A read of built local IX whose block ended; the load cannot compile it
\ either, so the reason is a compile-time one.
: LOCAL-ENDED ( n -- ) {: ix:n :}
   RCOMPILE @  CREFUSE @ 0 <>  or 0= IF ix MDIAG-LOCAL ! THEN
   MD-LOCAL-ENDED MDIAG-COMPILE!  IS-FAIL ;

\ A read of the local LOCAL answered index IX for: its bind sequence, which
\ names an ended local when an earlier bind took it and an unbound one when
\ no bind has, a negative index included.
: ITEM-LOCAL-REF ( n -- ) {: ix:n :}
   ix SEQ-SLOT {: li:n :}
   li 0 >= IF li LOC-REF-AT EXIT THEN
   ix 0 >=  ix LOCSEQ @ <  and IF ix LOCAL-ENDED EXIT THEN
   RCOMPILE @  CREFUSE @ 0 <>  or 0= IF ix MDIAG-LOCAL ! THEN
   MD-LOCAL-UNBOUND MDIAG-COMPILE!  IS-FAIL ;

\ A built local appended outside a `{:` group; the load cannot compile it
\ either, so the reason is a compile-time one.
: LOCAL-STRAY ( -- )
   MD-LOCAL-STRAY MDIAG-COMPILE!  IS-FAIL ;

EXPORT CHECKER-ITEMS:ITEM-COUNT

\ The type node of item IX's local: a LOCAL's own, a LOCAL-REF's from the LOCAL
\ item whose B is its A; false when no LOCAL item has that index.
: ITEM-LOCAL-NODE ( n -- n bool ) {: ix:n :}
   ix BODY-ITEMS:A ITEM-CELL@ {: v:n :}
   ix BODY-ITEMS:KIND ITEM-CELL@ BODY-ITEM-KIND:LOCAL = IF
      v RES-TRUE EXIT THEN
   ITEM-COUNT 0 ?DO
      i BODY-ITEMS:KIND ITEM-CELL@ BODY-ITEM-KIND:LOCAL =
      i BODY-ITEMS:B ITEM-CELL@ v = and IF
         i BODY-ITEMS:A ITEM-CELL@ RES-TRUE UNLOOP EXIT
      THEN
   LOOP
   0 RES-FALSE ;

\ A built local's rejection pinned on its item names the local by the type the
\ definer gave it; a read of an index no LOCAL item declared names none.
: ITEM-FAIL-SPELL ( n -- )
   ITEM-LOCAL-NODE 0= IF drop 0 FAILTU ! EXIT THEN
   EFFECT-TYPE TYPE-TEXT {: a:^ u:n :}
   u TOKBUF-ENSURE  a FAILTK u CCOPY  u FAILTU ! ;

: ITEM-STEP ( n -- ) {: i:n :}
   TSTART @ TADDR  TI @ TSTART @ -  {: a:^ u:n :}
   a u DO-TOK-HEAD
   {: unj ok-in unck-in pin-in :}
   i BODY-ITEMS:KIND ITEM-CELL@ {: kind:n :}
   i BODY-ITEMS:A ITEM-CELL@ {: v:n :}
   kind BODY-ITEM-KIND:NAME = IF NAME-TOK ELSE
   LMODE @ IF v kind a u ITEM-IN-GROUP ELSE
   a MODE-TOK? IF ELSE
   v kind ITEM-OPEN? IF ELSE
   kind BODY-ITEM-KIND:CALL = IF a u v ITEM-CALL ELSE
   kind BODY-ITEM-KIND:INT = IF v ITEM-INT ELSE
   kind BODY-ITEM-KIND:KEYWORD = IF a u ITEM-KEYWORD ELSE
   kind BODY-ITEM-KIND:LOCAL-REF = IF v ITEM-LOCAL-REF ELSE
   kind BODY-ITEM-KIND:LOCAL = IF LOCAL-STRAY ELSE
   kind ITEM-STRAY? IF ITEM-UNDEFINED ELSE
      s" checker: an item of no kind the walk knows" EXIT-STATUS:INTERNAL die
   THEN THEN THEN THEN THEN THEN THEN THEN THEN THEN
   a u unj ok-in unck-in pin-in DO-TOK-TAIL
   kind BODY-ITEM-KIND:LOCAL =  kind BODY-ITEM-KIND:LOCAL-REF = or
   FAILIX @  TOKIX @ 1 - =  and
   MDIAG @ MD-LOCAL-ENDED =  MDIAG @ MD-LOCAL-UNBOUND = or
   MDIAG @ MD-LOCAL-STRAY = or  and IF i ITEM-FAIL-SPELL THEN ;

variable WALK-ITEM    \ the first item the walk has not passed

\ The items of the text walked: the pending definition's body text's, else 0.
: TEXT-ITEMS ( -- n )
   PEND-TEXT? 0= IF 0 EXIT THEN
   ITEM-COUNT ;

: ITEM-OFF ( n -- n ) BODY-ITEMS:OFF ITEM-CELL@ ;

: ITEM-BEHIND? ( n -- bool ) {: i:n :}
   i WALK-ITEMS @ < 0= IF RES-FALSE EXIT THEN
   i ITEM-OFF TI @ < ;

\ Whether an item starts at TI; the cursor first passes every item a byte step
\ read as an operand, as a `[type]` span can.
: ITEM-AT? ( -- bool )
   BEGIN WALK-ITEM @ ITEM-BEHIND? WHILE WALK-ITEM @ 1 + WALK-ITEM ! REPEAT
   WALK-ITEM @ WALK-ITEMS @ < 0= IF RES-FALSE EXIT THEN
   WALK-ITEM @ ITEM-OFF TI @ = ;

\ Steps the cursor's item and moves the cursor on; true when that ends the
\ walk.
: ITEM-TOK ( -- bool )
   WALK-ITEM @ {: i:n :}
   i 1 + WALK-ITEM !
   i BODY-ITEMS:KIND ITEM-CELL@ BODY-ITEM-KIND:SIG = IF
      i ITEM-SIG EXIT THEN
   i ITEM-SPAN
   SCAN-TOK-HEAD {: was-name:n :}
   i ITEM-STEP
   was-name SCAN-TOK-TAIL ;

EXPORT CHECKER-TAPE:SCAN-TOKEN

\ ---- the body walk ----------------------------------------------------------
\ A body is walked token by token: an item `define-word` appended is stepped
\ where it starts, and bytes elsewhere are read as source. An item's span in
\ the listing is its token, and its kind picks the step that parsed source
\ reaches by spelling. A CALL binds the record the definer found, so nothing
\ the building package defines captures it.
: CHECK-SCAN-WALK ( -- )
   0 SPAY-S !
   EV-START
   TAPE-FEED? IF TBASE@ TBLEN @ SCAN-TOKEN THEN
   TEXT-ITEMS WALK-ITEMS !  0 WALK-ITEM !
   BEGIN TI @ TBLEN @ < WHILE
     TI @ CUR-GAP !
     BEGIN TI @ TBLEN @ <  TI @ TBLANK?  and WHILE TI @ 1 + TI ! REPEAT
     TI @ TBLEN @ < IF
       ITEM-AT? IF
         ITEM-TOK IF EXIT THEN
       ELSE
         TI @ TBYTE@ 40 =  TI @ 1 + TBLANK?  and IF   \ '(' then a blank (not '(CMP)') -> sig or comment
           CUR-PAREN-AT
           TI @ 1 + TI !  TI @ TSTART !
           BEGIN TI @ TBLEN @ <  TI @ 41 TBYTE=? 0=  and WHILE TI @ 1 + TI ! REPEAT
           TI @ TBLEN @ >= IF 0 PARSE-COMPLETE ! THEN
           PAREN-SIG? IF EXIT THEN
           TI @ TBLEN @ < IF TI @ 1 + TI ! THEN
           CUR-PAREN
         ELSE TI @ TBYTE@ 92 =  TI @ 1 + TBLANK?  and IF   \ '\' then a blank -> comment to the line's end
           CUR-PAREN-AT
           BEGIN TI @ TBLEN @ <  TI @ 10 TBYTE=? 0=  and WHILE TI @ 1 + TI ! REPEAT
           CUR-PAREN
         ELSE
           TI @ TSTART !
           BEGIN TI @ TBLANK? 0= WHILE TI @ 1 + TI ! REPEAT
           SCAN-TOK-HEAD {: was-name:n :}
           TSTART @ TADDR  TI @ TSTART @ -  DO-TOK1
           was-name SCAN-TOK-TAIL IF EXIT THEN
         THEN THEN
       THEN
     THEN
   REPEAT
   CUR-END ;

\ Walks a body, holding its using refusals for the end, where a structure open
\ at closer CA CU is refused first; a prefix passes none.
: CHECK-SCAN ( str -- )
   {: ca:^ cu:n :}
   BODY-WALK @ {: walk0:n :}
   -1 BODY-WALK !
   [: CHECK-SCAN-WALK ;] catch
   0 USING-HOLD !  walk0 BODY-WALK !
   {: rc:n :}
   rc 0 <> IF rc throw THEN
   TORDER-SCAN-UNKNOWN @ IF EXIT THEN
   ca cu CLOSER-REFUSE
   USING-RAISE ;

: CHECK-FOLD-EXITS ( -- )
   XSET @ IF
     DEADP @ IF XROW @ DCUR !  XRROW @ RCUR !  XFACT @ FACTS !
     ELSE XROW @ DCUR @ XRROW @ RCUR @ XFACT @ FACTS @ CF-JOIN-LIVE THEN
   THEN ;

: SGHASR? ( -- bool )
   SGHASR @ 0 <> ;

: CHECK-SIG? ( -- bool )
   VSIG-ON? SGSEEN? and ;

: CHECK-RET-SIG? ( -- bool )
   CHECK-SIG? SGHASR? and CHECK-RETURNS? and ;

\ A body with no return clause leaves the return row as it found it; one that
\ does not, as the definition's first failure, is the return row's (RSBAD 1).
: CHECK-RBALANCE ( -- )
   SGHASR @ 0 <> IF EXIT THEN
   CHECK-RETURNS? 0= IF EXIT THEN
   RCUR @ R-RES  RBROW @ R-RES  = IF EXIT THEN
   FAILSET @ 0=  OK @ and IF 1 RSBAD !  -1 FAILSET ! THEN
   0 OK ! ;

\ An effect too deep to record is uncheckable for it; a deferral would
\ record it, so a covered name yields; a pinned refusal stands.
: UNFIT-UNCHECKABLE ( -- )
   UNFIT @ 0= IF EXIT THEN
   UNSEEN @ 0 <> IF
      0 UNSEEN !  0 UNDEFERR !
      RCOMPILE @ 0 <> IF EXIT THEN
      MD-EFFECT-DEPTH MDIAG !  UNCK-RAISE  EXIT
   THEN
   OK @ 0=  UNCK @ 0 <>  or IF EXIT THEN
   MD-EFFECT-DEPTH MDIAG!  UNCK-RAISE ;

\ Every record of a checked declaration holds the declared rows, which
\ SIG-EFF-CACHE! measured; an inferred effect is measured here.
: CHECK-DEPTH-FITS ( -- )
   SGBAD @ 0 <> IF EXIT THEN
   CHECK-SIG? 0= IF
      OK @ 0=  UNCK @ 0 <>  or IF EXIT THEN
      BROW @ DCUR @ RBROW @ RCUR @ ROWS-FIT? IF EXIT THEN
   THEN
   UNFIT-UNCHECKABLE ;

\ VERDICT-DEFERRED defers the body to the run: it names a word only a rendering
\ statement in scope can have defined. A malformed declaration is still refused,
\ since the declaration is what gets recorded, and one nested too deep to record
\ is uncheckable; past a token of unknown effect nothing downstream is a
\ judgment, so the rest is the run's. A call to a malformed qualified name is no
\ such token: no word can ever answer it, so its body is refused like any other,
\ not left uncheckable. A recorded compile-time refusal, or an undefined word
\ the check finds itself, is a refusal whatever follows it.
: CHECK-VERDICT ( -- n )
   SGBAD @ 0 <> IF REFUSAL-UNCHECKABLE? IF VERDICT-UNCHECKABLE ELSE VERDICT-REFUSED THEN EXIT THEN
   UNSEEN @ 0 <> IF VERDICT-DEFERRED EXIT THEN
   UNSAFE @ RETIRED @ or  IMMERR @ or  LOCALBAD @ or  LINLOCBAD @ or  QUALBAD @ or  CAPREQ @ or  MREJ @ or  NPBAD @ or  RCOMPILE @ or 0 <>  REFUSAL@ RF-UNDEF =  or IF VERDICT-REFUSED ELSE
   UNCK @ 0 <> IF VERDICT-UNCHECKABLE ELSE OK @ THEN THEN ;

\ `cast: NAME ( in -- out )` is a declaration: the engine's reader keyword
\ publishes an identity word and asks CHECKER-DEFCAST to prove the declared
\ retype legal. The refusals are properties of the two declared terms only.

: CAST-ROW-1? ( n -- bool )
   VALUE-REST 0= IF drop RES-FALSE EXIT THEN
   R-RES TAG S-ROW = ;
\ The row under the value on top: a hidden group whole, or one other cell.
: CAST-ROW-UNDER ( n -- n ) VALUE-REST drop R-RES ;
\ The width of the value on top, in cells.
: CAST-ROW-WIDTH ( n -- n ) R-RES VALUE-CELLS ;
\ The term of the value on top: a hidden group's logical term.
: CAST-ROW-TERM ( n -- n )
   R-RES dup VALUE-CELLS 1 > IF P>TYPE MK-LOGICAL EXIT THEN
   P>TYPE ;
\ A cast retypes one value of any width, the same width on both sides (ROWS). A
\ pointer chain must end at storage: an atom is a phantom index. The pointee
\ carries no width rule, since a layout of any width is one pointer cell away.
: CAST-TERM? ( n -- bool ) {: t0:n :}
   t0 T-RES {: t:n :}
   t TAG T-CON = IF RES-TRUE EXIT THEN
   t TAG T-VAR = IF RES-TRUE EXIT THEN
   t TAG T-PARAM = IF RES-TRUE EXIT THEN
   t TAG T-QUOT = IF RES-TRUE EXIT THEN
   t TAG T-PTR = IF
      t BEGIN dup TAG T-PTR = WHILE PTR>INNER T-RES REPEAT
      TAG T-ATOM <> EXIT
   THEN
   RES-FALSE ;
package CAST-ERROR
\ Zeros outside a declaration, so a saved image holds none of the build
\ process's addresses.
: CLEAR ( -- )
   NULL-PTR WORD-A !  0 WORD-U !  NULL-PTR KEY-A !  0 KEY-U !
   NULL-PTR SIG-A !  0 SIG-U !  NULL-PTR TOK-A !  0 TOK-U ! ;
\ A signature whose length describes no memory gets no note.
: SIG-NOTE ( -- )
   SIG$ BYTE-SPAN? 0= IF EXIT THEN
   s" signature" REJECTION:NOTE  SIG$ SIG-TRIM REJECTION:EMIT-CUT ;
\ Opens the declaration's rejection under code, why wa wu saying what is wrong,
\ placed at its token; the caller writes the label.
: PLACE ( ^u8 n n -- ) {: wa:^ wu:n code:n :}
   code CODE$ REJECTION:OPEN  wa wu TITLE  SPOT$ REJECTION:AT ;
\ The term atop row and its width in cells, as "`str` is 2".
: TERM-WIDTH ( n -- ) {: row:n :}
   96 REJECTION:EMIT1  row CAST-ROW-TERM REND-TYPE
   s" ` is " REJECTION:EMIT-STR  row CAST-ROW-WIDTH REJECTION:EMIT-N ;
\ The terms atop rows in and out and their widths, as "`str` is 2 cells, `n` is 1".
: WIDTHS ( n n -- ) {: in:n out:n :}
   SEEN-RESET RATOM-RESET 0 NLET !
   in TERM-WIDTH  s"  cell" REJECTION:EMIT-STR
   in CAST-ROW-WIDTH 1 <> IF 115 REJECTION:EMIT1 THEN
   s" , " REJECTION:EMIT-STR  out TERM-WIDTH ;
public
\ Runs xt on the declaration of name na nu with signature sa su under keyword
\ ka ku, the record live until xt returns or throws.
: DECLARE ( ^u8 n ^u8 n ^u8 n [ ^u8 n ^u8 n -- ] -- )
   >r {: na:^ nu:n sa:^ su:n ka:^ ku:n :}
   na WORD-A !  nu WORD-U !  ka KEY-A !  ku KEY-U !  sa SIG-A !  su SIG-U !
   na nu sa su r> [: CLEAR ;] finally ;
: TOKEN! ( ^u8 n -- ) TOK-U !  TOK-A ! ;
\ Opens the declaration's rejection under code, why wa wu saying what is wrong;
\ the caller writes its help and stops.
: OPEN ( ^u8 n n -- ) {: wa:^ wu:n code:n :}
   wa wu code PLACE  code LABEL$ REJECTION:EMIT-STR  SIG-NOTE ;
\ Rejects the declaration under code: why wa wu says what is wrong, label la lu
\ the issue at its token, help ha hu what to write instead.
: RAISE-LABEL ( str str str n -- )
   {: wa:^ wu:n la:^ lu:n ha:^ hu:n code:n :}
   wa wu code PLACE  la lu REJECTION:EMIT-STR  SIG-NOTE
   REJECTION:HELP  ha hu REJECTION:EMIT-STR
   EXIT-STATUS:REJECTED REJECTION:STOP ;
\ RAISE-LABEL under code's own label.
: RAISE ( ^u8 n ^u8 n n -- ) {: wa:^ wu:n ha:^ hu:n code:n :}
   wa wu code LABEL$ ha hu code RAISE-LABEL ;
\ Rejects a cast whose input row in and output row out differ in width.
: WIDTH-RAISE ( n n -- ) {: in:n out:n :}
   s" the input and output differ in width" E-CAST-ARITY PLACE
   in out WIDTHS  SIG-NOTE
   REJECTION:HELP  s" give both sides the same width in cells: " REJECTION:EMIT-STR
   in out WIDTHS
   EXIT-STATUS:REJECTED REJECTION:STOP ;
\ Rejects signature sa su of name na nu that does not parse.
: REJECT-SIGNATURE ( str str -- ) {: sa:^ su:n na:^ nu:n :}
   sa su na nu SGBAD-REJECTION
   sa su na nu SGBAD-PIN
   EXIT-STATUS:REJECTED REJECTION:STOP ;
;package

\ The family TAGGED-HIT? last found, or -1 for a type variable.
variable TAGGED-FAM
variable SCRATCH-FAM
\ An enum or sum value carries a tag only its constructors and TAG> check, and a
\ caller may bind one to a type variable that takes no linear value (NLINEAR,
\ stored or `nolinear`). A pointee is storage, which the mint rule leaves to the
\ writer, and a niche-null sum has no invalid tag.
: TAGGED-HIT? ( n n -- bool )
   {: t:n sign:n :}
   sign CAST-ARG and 0 <> sign CAST-BOTH = or IF RES-FALSE EXIT THEN
   t T-UNSTALE {: u:n :}
   u ISVAR IF
      u PAY NLINEAR@ dup IF -1 TAGGED-FAM ! THEN EXIT
   THEN
   u LAYOUT-PARAM? 0= IF RES-FALSE EXIT THEN
   u PARAM>FAM {: fam:n :}
   fam TFAM-ENUM? fam TFAM-SUM? or
   fam TFAM-NICHE? 0= and
   dup IF fam TAGGED-FAM ! THEN ;

\ Rejects the enum or sum TAGGED-HIT? found on the output when true, else the
\ input.
: TAGGED-RAISE ( bool -- )
   {: out:bool :}
   TAGGED-FAM @ -1 = IF
      out IF
         s" the output hands out a value of a type variable, which a caller may bind to an enum or sum"
      ELSE
         s" the input is handed a value of a type variable, which a caller may bind to an enum or sum"
      THEN
      s" hold the value behind a pointer"
   ELSE
      TAGGED-FAM @ TFAM-NAME$ CAST-ERROR:TOKEN!
      out IF
         s" the output makes, from a cell, a value whose tag nothing checks, of"
      ELSE
         s" the input is handed, from a cell, a value whose tag nothing checks, of"
      THEN
      s" make an enum from a number with its TAG> (DERIVE tag), a sum with a variant's constructor, or view storage you wrote as ( ^u8 -- ^T ) in a private section"
   THEN
   E-CAST-CLASS CAST-ERROR:RAISE ;

\ A quotation the output hands out by value: itself or a by-value member.
: QUOT-HIT? ( n n -- bool ) {: t:n sign:n :}
   sign CAST-ARG and 0 <> sign CAST-BOTH = or IF RES-FALSE EXIT THEN
   t TAG T-QUOT = ;

\ A value of a SCRATCH-ONLY family another package declared, outside a layout
\ argument; a hit keeps the family for SCRATCH-RAISE.
: SCRATCH-HIT? ( n n -- bool ) {: t:n sign:n :}
   sign CAST-ARG and 0 <> IF RES-FALSE EXIT THEN
   t T-RES dup TAG T-PARAM <> IF drop RES-FALSE EXIT THEN
   PARAM>FAM {: fam:n :}
   fam 0 < IF RES-FALSE EXIT THEN
   fam TFAM-SCRATCH? 0= IF RES-FALSE EXIT THEN
   CHECKER-AUTH-PACKAGE$ fam TFAM-PKG-MATCH? IF RES-FALSE EXIT THEN
   fam SCRATCH-FAM !
   RES-TRUE ;

: SCRATCH-RAISE ( -- )
   SCRATCH-FAM @ TFAM-PKG$ {: pa:^ pu:n :}
   pu 0= IF
      s" the cast names a top-level SCRATCH-ONLY family"
      s" declare the cast at top level, where the family is declared"
   ELSE
      pa pu CAST-ERROR:TOKEN!
      s" the cast names a SCRATCH-ONLY family of package"
      s" declare the cast in the package that declares the family, which decides what its values may become"
   THEN
   E-CAST-SCRATCH CAST-ERROR:RAISE ;

\ The cast gate: true when the output mints a pointer or quotation, else a
\ representation cast.
: CAST-WALKS ( -- bool )
   SGIN @ CAST-ROW-TERM dup CAST-TERM? swap OPAQUE-XT? or 0= IF
      s" the input is not one machine cell a cast may retype"
      s" take one cell as the input: a con such as `n`, a width-1 family, a pointer chain ending at storage, or a quotation"
      E-CAST-CLASS CAST-ERROR:RAISE
   THEN
   SGOUT @ CAST-ROW-TERM CAST-TERM? 0= IF
      s" the output is not one machine cell a cast may retype"
      s" give one cell as the output: a con such as `n`, a width-1 family, a pointer chain ending at storage, or a quotation"
      E-CAST-CLASS CAST-ERROR:RAISE
   THEN
   SGOUT @ CAST-ROW-TERM [: drop ATOM-HIT? ;] CAST-INTRODUCES? IF
      s" the output hands out an atom, a phantom index"
      s" write a cell type such as `n` where the output, its pointees or its quotations' outputs name an atom"
      E-CAST-CLASS CAST-ERROR:RAISE
   THEN
   SGOUT @ CAST-ROW-TERM [: drop WIDTH-HIT? ;] CAST-INTRODUCES? IF
      s" the output is a width, which no cast yields"
      s" write `n` where the output names a `width<t>`, and read a width with `width>n`"
      E-CAST-CLASS CAST-ERROR:RAISE
   THEN
   SGOUT @ CAST-ROW-TERM [: TAGGED-HIT? ;] CAST-INTRODUCES? IF
      RES-TRUE TAGGED-RAISE
   THEN
   \ The source's consumed rows and pointees introduce values too: what the
   \ result is fed or has written through it reaches the source's code as the
   \ source's type.
   SGIN @ CAST-ROW-TERM CAST-NEG [: drop ATOM-HIT? ;] CAST-INTRO? IF
      s" the input is handed an atom, a phantom index"
      s" write a cell type such as `n` where the input's pointees or its quotations' inputs name an atom"
      E-CAST-CLASS CAST-ERROR:RAISE
   THEN
   SGIN @ CAST-ROW-TERM CAST-NEG [: drop WIDTH-HIT? ;] CAST-INTRO? IF
      s" the input is handed a width, which no cast yields"
      s" write `n` where the input's pointees or its quotations' inputs name a `width<t>`"
      E-CAST-CLASS CAST-ERROR:RAISE
   THEN
   SGIN @ CAST-ROW-TERM CAST-BOTH [: SCRATCH-HIT? ;] CAST-INTRO?
   SGOUT @ CAST-ROW-TERM CAST-BOTH [: SCRATCH-HIT? ;] CAST-INTRO? or IF SCRATCH-RAISE THEN
   \ A row variable stands for cells the signature does not show, which each use
   \ of the cast picks; an output holding no pointer or quotation passes.
   SGOUT @ CAST-ROW-TERM [: drop MINT-HIT? ;] CAST-INTRODUCES? {: mint:bool :}
   \ A caller feeds the source's consumed rows only by calling a quotation the
   \ output hands out by value.
   SGOUT @ CAST-ROW-TERM [: QUOT-HIT? ;] CAST-INTRODUCES? IF
      SGIN @ CAST-ROW-TERM CAST-NEG [: TAGGED-HIT? ;] CAST-INTRO? IF
         RES-FALSE TAGGED-RAISE
      THEN
   THEN
   CAST-WALK-RESET
   mint IF SGIN @ CAST-ROW-TERM SGOUT @ CAST-ROW-TERM CAST-ROWS-KEPT? 0= IF
      s" a row variable changes place between the input and the output"
      s" end each output quotation row in the input's row variable at the same place, with as many cells, as `( [ R n -- R ] -- [ R ^n -- R ] )`"
      E-CAST-CLASS CAST-ERROR:RAISE
   THEN THEN
   \ The input is only read, so a quotation whose tails differ is any quotation.
   SGIN @ CAST-ROW-TERM RES-FALSE CAST-MAY-LINEAR? IF
      s" the input may hold a linear value"
      s" write a closed non-linear type in place of the input's linear type or type variable; a token is erased only by its owner's `linear:` row"
      E-CAST-LINEAR CAST-ERROR:RAISE
   THEN
   SGOUT @ CAST-ROW-TERM RES-TRUE CAST-MAY-LINEAR? IF
      s" the output may hold a linear value"
      s" write a closed non-linear type in place of the output's linear type, type variable or `[ R -- S ]` quotation; a token is minted only by its owner's `linear:` row"
      E-CAST-LINEAR CAST-ERROR:RAISE
   THEN
   \ The mint rule comes last: a mint that also carries ownership is named by
   \ that stronger reject.
   mint IF
      CHECKER-AUTH-PACKAGE-MODE@ CHECKER-PACKAGE-PRIVATE <> IF
         s" the output mints a pointer or a quotation outside a private section"
         s" declare the cast after `private` in its package"
         E-CAST-MINT CAST-ERROR:RAISE
      THEN
   THEN
   mint ;

: CAST-SCOPED? ( -- bool )
   SGIN @ SCOPED-TYPE? SGOUT @ SCOPED-TYPE? or
   SGHASR @ IF SGRIN @ SCOPED-TYPE? or SGROUT @ SCOPED-TYPE? or THEN ;
\ A view in a row is two pushes, the hidden cells of one instance
\ (PUSH-LOGICAL): the base cell under the length.
: ROW-UNDER ( n -- n ) R-RES P>REST R-RES ;
: ROW-2? ( n -- bool )
   R-RES dup TAG S-PUSH <> IF drop RES-FALSE EXIT THEN
   P>REST CAST-ROW-1? ;
: VIEW-PAIR? ( n -- bool ) {: row:n :}
   row R-RES P>TYPE T-RES {: hi:n :}
   row ROW-UNDER CAST-ROW-TERM T-RES {: lo:n :}
   hi READ-VIEW? hi MUT-VIEW? or 0= IF RES-FALSE EXIT THEN
   hi HIDDEN-PARAM? lo HIDDEN-PARAM? and 0= IF RES-FALSE EXIT THEN
   hi HIDDEN-SLOT@ 1 =  lo HIDDEN-SLOT@ 0 =  and 0= IF RES-FALSE EXIT THEN
   hi lo CAST-SAME-INSTANCE? ;
: VIEW-REP? ( n -- bool ) {: row:n :}
   row CAST-ROW-TERM T-RES {: hi:n :}
   hi TAG T-CON <> IF RES-FALSE EXIT THEN
   hi PAY CC-N <> IF RES-FALSE EXIT THEN
   row ROW-UNDER CAST-ROW-TERM BYTE-PTR? ;
\ A quotation that consumes one two-cell value over its implicit tail and
\ leaves that tail: `[ V -- ]` or `[ ^u8 n -- ]`. A named tail could bind
\ the callback to cells under the view that the retyped callback's own tail
\ never supplies.
: PAIR-CALLBACK? ( n -- bool ) {: q:n :}
   q TAG T-QUOT <> IF RES-FALSE EXIT THEN
   q Q>DIN ROW-2? 0= IF RES-FALSE EXIT THEN
   q Q>DOUT R-RES  q Q>DIN ROW-UNDER ROW-UNDER <> IF RES-FALSE EXIT THEN
   q Q>DOUT R-RES PAY RVK-QUOT? ;
\ A callback pack retypes `[ V -- ]` as `[ ^u8 n -- ]` over the same tail:
\ calling the result packs the two cells into the view the callback takes.
: CALLBACK-PACK? ( -- bool )
   SGIN @ CAST-ROW-1? SGOUT @ CAST-ROW-1? and 0= IF RES-FALSE EXIT THEN
   SGIN @ ROW-UNDER  SGOUT @ ROW-UNDER <> IF RES-FALSE EXIT THEN
   SGIN @ CAST-ROW-TERM T-RES {: from:n :}
   SGOUT @ CAST-ROW-TERM T-RES {: to:n :}
   from PAIR-CALLBACK? to PAIR-CALLBACK? and 0= IF RES-FALSE EXIT THEN
   from Q>DIN VIEW-PAIR? to Q>DIN VIEW-REP? and ;
\ Answers whether the cast is an unpack, and the row whose top is the view: an
\ unpack's input, a pack's output, or a callback pack's callback input.
: VIEW-CAST-SIDE ( -- bool n )
   CALLBACK-PACK? IF RES-FALSE SGIN @ CAST-ROW-TERM T-RES Q>DIN EXIT THEN
   SGIN @ ROW-2? SGOUT @ ROW-2? and 0= IF
      s" a scoped cast is not two cells on each side"
      s" retype one view as its two cells, as `( read-view<p,q,u8> -- ^u8 n )`, or take the scope out of the cast"
      E-CAST-SCOPE CAST-ERROR:RAISE
   THEN
   SGIN @ ROW-UNDER ROW-UNDER  SGOUT @ ROW-UNDER ROW-UNDER <> IF
      s" the input and output rows end in different tails"
      s" end both sides of the view cast in the same tail, as `( R read-view<p,q,u8> -- R ^u8 n )`"
      E-CAST-SCOPE CAST-ERROR:RAISE
   THEN
   SGIN @ VIEW-PAIR? SGOUT @ VIEW-REP? and IF RES-TRUE SGIN @ EXIT THEN
   SGIN @ VIEW-REP? SGOUT @ VIEW-PAIR? and 0= IF
      s" a scoped cast is neither a view pack nor a view unpack"
      s" write a view unpack, as `( read-view<p,q,u8> -- ^u8 n )`; only package C2-MEM declares the other view casts"
      E-CAST-SCOPE CAST-ERROR:RAISE
   THEN
   RES-FALSE SGOUT @ ;
\ The gate for a cast with a scope dependency.
: VIEW-CAST-CERTIFY ( -- )
   SGHASR @ IF
      s" a scoped cast declares a return-stack effect"
      s" write the signature with no `|` clause"
      E-CAST-SCOPE CAST-ERROR:RAISE
   THEN
   VIEW-CAST-SIDE {: unpack:bool side:n :}
   CHECKER-AUTH-PACKAGE-MODE@ CHECKER-PACKAGE-PRIVATE <> IF
      s" a view cast is declared outside a private section"
      s" declare the view cast after `private` in its package"
      E-CAST-SCOPE CAST-ERROR:RAISE
   THEN
   side CAST-ROW-TERM T-RES {: view:n :}
   view view PARAM>ARGC 1 - PARAM>ARG RES-TRUE CAST-MAY-LINEAR? IF
      s" the view's element may hold a linear value"
      s" write a closed non-linear element type, such as `u8`, in place of the view element's type variable or linear type"
      E-CAST-LINEAR CAST-ERROR:RAISE
   THEN
   view view PARAM>ARGC 1 - PARAM>ARG CAST-BOTH [: SCRATCH-HIT? ;] CAST-INTRO? IF SCRATCH-RAISE THEN
   unpack IF view READ-VIEW? IF EXIT THEN THEN
   \ The literal is only the owner's name, which no registry holds: the view
   \ families are declared with package "".
   CHECKER-AUTH-PACKAGE$ s" C2-MEM" CORE-STR=CI 0= IF
      s" only package C2-MEM packs a view or unpacks a mutable view"
      s" unpack a read view, as `( read-view<p,q,u8> -- ^u8 n )`, or make the view with C2-MEM's words"
      E-CAST-SCOPE CAST-ERROR:RAISE
   THEN ;

\ What a `linear:` row carries beside its token: a non-linear con or a pointer
\ chain ending at one.
: LINEAR-PAYLOAD? ( n -- bool )
   T-RES BEGIN dup TAG T-PTR = WHILE PTR>INNER T-RES REPEAT
   dup TAG T-CON <> IF drop RES-FALSE EXIT THEN
   PAY CT-LINEAR? 0= ;

: LINEAR-CERTIFY ( -- )
   SGIN @ CAST-ROW-TERM {: in:n :}
   SGOUT @ CAST-ROW-TERM {: out:n :}
   in LIN-CON? IF in out ELSE out in THEN {: tok:n pay:n :}
   tok LIN-CON? 0= pay LINEAR-PAYLOAD? 0= or IF
      s" the row is not one linear type and one non-linear payload"
      s" put the linear type on one side and its payload, a con or a pointer chain ending at one, on the other, as `( ^n -- PKG:tok )`"
      E-LINEAR-PAYLOAD CAST-ERROR:RAISE
   THEN
   tok T-RES PAY CT-PKG$ {: pa:^ pu:n :}
   pu 0= IF
      s" the linear type is declared at top level, so no package mints it"
      s" declare the linear type with `DEFLINEAR` inside a package, and this row in that package's private section"
      E-LINEAR-OWNER CAST-ERROR:RAISE
   THEN
   pa pu CHECKER-AUTH-PACKAGE$ CORE-STR=CI 0= IF
      pa pu CAST-ERROR:TOKEN!
      s" the linear type belongs to package"
      s" declare this row in the private section of the package that declared the linear type"
      E-LINEAR-OWNER CAST-ERROR:RAISE
   THEN
   CHECKER-AUTH-PACKAGE-MODE@ CHECKER-PACKAGE-PRIVATE <> IF
      s" the row is outside its owner's private section"
      s" declare the row after `private` in its owner package"
      E-LINEAR-SCOPE CAST-ERROR:RAISE
   THEN ;

EXPORT CHECKER-QUOT-FRESH:FITS?

\ A body-supplied name left unbound takes a new identity, and only a region name may
\ be bound: to one this body minted that no other name claims and FITS? admits.
: FB-OWNED? ( n -- bool ) {: idx:n :}
   idx FB-NAME @ {: name:n :}
   idx FB-VAR @ T-RES {: r:n :}
   r ISVAR IF name FB-MINT r PAY TV! THEN
   name ATOM>A name ATOM>U FRESH-REGION-NAME? 0= IF r ISVAR EXIT THEN
   idx FB-VAR @ T-RES {: id:n :}
   id TAG T-ATOM <> IF RES-FALSE EXIT THEN
   id ATOM>K RGN-MARK @ < IF RES-FALSE EXIT THEN
   id ATOM>A id ATOM>U FRESH-REGION-NAME? 0= IF RES-FALSE EXIT THEN
   r ISVAR 0= IF
      id SGIN @ SGOUT @ SGRIN @ SGROUT @ FITS? 0= IF RES-FALSE EXIT THEN
   THEN
   idx 1+ FO-ENSURE
   id ATOM>K idx FO-MATCH? ;

: FB-CLAIM ( n -- ) {: idx:n :}
   idx FB-NAME @ {: name:n :}
   name 0= IF EXIT THEN
   idx FB-OWNED? IF EXIT THEN
   idx FB-VAR @ T-RES  name FB-MINT  RIGID-HELD-REFUSE ;

: FRESH-CLAIM ( -- )
   OK @ 0= IF EXIT THEN
   FO-RESET
   0 BEGIN dup FB-N @ < WHILE  dup FB-CLAIM  1 + REPEAT drop ;

\ An inferred input holding a rigid identity no call gave it holds one its body
\ minted, which no caller has (C-INFOWN): refused as a declared input's is (C-FOWN).
: INFER-RIGID-TYPE ( n -- ) {: t:n :}
   t T-RES {: r:n :}
   r TAG T-ATOM = IF
      r ATOM>K 0 > IF r CALL-MINT? 0= IF r  r FB-MINT  RIGID-HELD-REFUSE THEN THEN
      EXIT
   THEN
   r TAG T-PTR = IF r PTR>INNER RECURSE EXIT THEN
   r TAG T-PARAM = IF
      0 BEGIN dup r PARAM>ARGC < WHILE
         r over PARAM>ARG RECURSE
         1 +
      REPEAT drop
   THEN ;

: INFER-FRESH-CLAIM ( -- )
   OK @ 0= IF EXIT THEN
   BROW @ R-RES
   BEGIN dup TAG S-PUSH = WHILE
      dup P>TYPE INFER-RIGID-TYPE
      P>REST R-RES
   REPEAT drop ;

: CHECK-FRESH-OUTPUT ( -- )
   SGOUT @ SUNI-COERCE
   FRESH-CLAIM ;

\ A recorded name's refusals (closed constructor package, duplicate tail) come
\ before the record step's later-wins first write. a u is the copy NMA NMU of
\ the name at NMOFF, where a rejection is placed.
: CHECK-REC-ADMIT ( str -- )
   {: a:^ u:n :}
   a u CTOR-EXTEND?-XT IF NMOFF @ TADDR NMU @ CTOR-EXTEND-REJECT THEN
   a u CHECKER-CERT-DUP? IF NMOFF @ TADDR NMU @ CHECKER-DUP-DEFINITION THEN ;

\ Every refusal a certified definition can meet at publication, asked before its
\ first write to the store: a write left behind is read by a later verdict.
: PUBLISH-REFUSALS ( str -- )
   {: a:^ u:n :}
   a u CHECK-REC-ADMIT
   CHECK-SIG? IF SGWIN @ SGWOUT @ a u SHADOW-ARITY-CK THEN ;

\ The declared row reported this definition's bad signature.
: DECLARED-ROW-REPORTS? ( -- bool )
   BWIN-UNJUDGED @ 0= 0= SGDECL-BAD @ 0= 0= and ;
\ A definition rejected in a multi-error load keeps its declared signature as
\ source authority, so a later caller checks against it, unless that signature
\ is bad or too big to record.
: MULTI-REJECTED ( -- )
   DECLARED-ROW-REPORTS? IF EXIT THEN
   CHECK-SIG? SGBAD @ 0= and UNFIT @ 0= and IF
      NMA @ NMU @ CHECK-REC-ADMIT
      NMA @ NMU @ 0 NORET-ADD
      NMA @ NMU @  NMOFF @ TADDR NMU @  RES-FALSE CHECKER-USIG-CERT-ADD-PENDING
      RECOVERY-RECORD
      NMA @ NMU @ CHECKER-RECORD-SYM CK-CLOSE!
   THEN ;
: CHECK-UNRECORDED-DIN ( -- n ) UNREC-DIN @ ;
: CHECK-UNRECORDED-DOUT ( -- n ) UNREC-DOUT @ ;

: NAME-PINNED? ( -- bool )
   NMU @ 0 >  FAILB @ NMOFF @ =  and ;

: CHECK   \ ( a u -- -1=certified | 0=rejected | 1=uncheckable | 2=deferred )
   {: a u :}
   CELLS-NONE UNREC-DIN !  CELLS-NONE UNREC-DOUT !
   WRITE-WINDOW-CK
   -1 PARSE-COMPLETE !
   a u CHECK-RESET
   DOES-SPLIT @ 0 <> IF s" does>" ELSE s" ;" THEN CHECK-SCAN
   DOES-SPLIT @ 0 <>  TORDER-SCAN-UNKNOWN @ 0=  and IF s" does>" DOES-UNMADE-REJECT THEN
   SIG-UNRES @ SGBAD @ 0= and IF
      0 LAYOUT-XPORT !  0 BIND-HORIZON !  CK-CLOSE-CLEAR
      CHECKER-UNSEEN-MARK
      2 EXIT
   THEN
   \ A parent-order pass checks a definer's head before its does> clause; the
   \ parent's closing pass checks it again once the clause is checked and
   \ neither refused nor left to the run.
   TORDER-ACTIVE @ TORDER-PARENT-GATE >=
   RCOMPILE @ 0= REFUSAL @ USING-KIND? 0= and and IF
      0 LAYOUT-XPORT !  0 BIND-HORIZON !  CK-CLOSE-CLEAR
      2 EXIT
   THEN
   \ Unknown order cannot certify or even publish a declared recovery row.
   \ Its selected trusted tick is already pinned for the verifier's warning.
   TORDER-SCAN-UNKNOWN @ IF
      0 LAYOUT-XPORT !  0 BIND-HORIZON !  CK-CLOSE-CLEAR
      2 EXIT
   THEN
   \ A body that walked no token past its name is pinned at its closer.
   FAILSET @ 0=  FAILTU @ 0=  NAME-PINNED? or  and IF
      DOES-SPLIT @ 0 <> IF s" does>" ELSE s" ;" THEN CLOSER-PIN!
   THEN
   0 LAYOUT-XPORT !                  \ boundary unification is never in transport mode
   CHECK-FOLD-EXITS
   CONM @ 0 <> IF MD-CON-TRUNC MDIAG! THEN     \ latch truncation BEFORE the boundary
   FIELD-LOAN-PENDING @ IF MD-FIELD-NAME MDIAG! SCOPE-FIELD-REJECT THEN
   CHECKER-MATCH-MODE @ 0 <>  MF-DEPTH @ 0 <>  or IF MD-TRUNC MDIAG! THEN   \ unify pins its own mismatch
   CHECK-NO-BORROW
   CHECK-SIG? CHECK-RETURNS? and IF
      CHECK-FRESH-OUTPUT
      OK @ IF SGIN @ BROW !  SGOUT @ DCUR ! THEN
   THEN
   CHECK-SIG? CHECK-RETURNS? 0= and IF FRESH-CLAIM THEN   \ a body that never returns resolves its fresh names too
   \ An inferred effect records its data rows only (CHECKER-USIG-CERT-CURRENT);
   \ they hold a binder only where a declared one may (PSIG).
   CHECK-SIG? 0= IF
      INFER-FRESH-CLAIM
      BROW @ RES-TRUE FORALL-PLACE?  DCUR @ RES-FALSE FORALL-PLACE? and
      0= IF MD-BINDER-PLACE CF-FAIL THEN
   THEN
   DEF-LSC-CK
   CF-OPEN? IF MD-CF-OPEN CF-FAIL THEN
   CHECK-RBALANCE
   CHECK-RET-SIG? IF
      SGROUT @ RSUNI-COERCE
      OK @ IF SGRIN @ RBROW !  SGROUT @ RCUR ! THEN
   THEN
   CHECK-SIG? OK @ and IF NP-CHECK THEN               \ declared quantifiers must stay parametric
   CHECK-DEPTH-FITS
   CHECK-VERDICT
   dup DVERD !
   dup 0= IF RIGID-DIAG-CLASSIFY THEN
   dup VERDICT-REFUSED =  over VERDICT-UNCHECKABLE =  or
   REJECTION:QUIET @ 0= and BWIN-UNJUDGED @ 0= and IF DIAG-PRINT THEN
   \ The declared effect's record outlives the body until the diagnostic above,
   \ which renders it.
   SIG-EFF-DROP
   0 BIND-HORIZON !                                   \ the record step asks the store, not the walk
   CK-CLOSE-CLEAR
   \ An unjudged scan's call rows are the width authority whatever its verdict
   \ (UNJ-SPLICE), so a rejected TRUSTED: body still states its call rows.
   dup -1 = NMU @ 0 > and  BWIN-UNJUDGED @ 0 <> or IF CALL-FINALIZE THEN
   dup -1 = NMU @ 0 > and IF
      NMA @ NMU @ PUBLISH-REFUSALS
      \ Every fact below is about the symbol the definition is recorded under,
      \ never about what its name binds: the lookup cannot see a definition the
      \ engine has not published, and answers the word it shadows.
      NMA @ NMU @ CHECKER-RECORD-SYM {: sym:n :}
      \ The identity the symbol already carries (CTL-IDENTITY: the intrinsic
      \ tag and the engine's-own-word mark) stays: a replay of the engine's
      \ own source re-records the word and never runs the line that tagged
      \ it, while `undefine` zeroed it, so a redefinition carries none.
      \ CTL-RENDERS stays for the same reason: the evaluate boundary's is an
      \ axiom, and no body derives it.
      sym CTL-FLAGS-SYM CTL-IDENTITY CTL-RENDERS or CTL-NOMINAL or and CTLNEW !
      DEADP @ XSET @ 0= and IF CTLNEW @ CTL-DEAD or CTLNEW ! THEN
      THSET @ IF CTLNEW @ CTL-THROW or CTLNEW ! THEN
      CTLNEW @ INHSET @ or CTLNEW !
      ZSUMMARY-EFFECT? IF
         ZSHAPE @ 1 = IF CTLNEW @ CTL-ZERO-TRUE or CTLNEW ! THEN
         ZSHAPE @ 3 = IF CTLNEW @ CTL-ZERO-FALSE or CTLNEW ! THEN
      THEN
      \ The body's folded evidence, which is about the DECLARED input rows the
      \ edges were measured against (XBASE-D): with no declared signature there
      \ is no such row and nothing is claimed.
      THSET @ CHECK-SIG? and IF
         THDMASK @ XBASE-D ROW-TERMS XMASK-LOW
         THRMASK @ XBASE-R ROW-TERMS XMASK-LOW
      ELSE 0 0 THEN
      CTLRNEW !  CTLDNEW !
      \ The inputs the body divides by or throws before anything takes them, about
      \ the same rows.
      CHECK-SIG? IF NZ-FACT ELSE 0 THEN CTLZNEW !
      \ The control word and the NONZERO cell together say whether this
      \ definition adds anything the store does not already hold about its
      \ symbol.
      sym CTL-WORD-SYM
      CTLNEW @ CTLDNEW @ CTLRNEW @ XFER-PACK <>
      sym NORET-NONZERO@ CTLZNEW @ <> or IF
         sym CTLNEW @ CTLDNEW @ CTLRNEW @
         sym NORET-CREATES@ sym NORET-SOURCE@ CTLZNEW @ NORET-APPEND
      THEN
      CHECK-SIG? IF
         NMA @ NMU @  CHECKER-USIG-CERT-PARSED
      ELSE
         NMA @ NMU @ REC-SIG
      THEN
      sym CK-CLOSE!
   THEN
   \ Deferred: recorded so a caller binds and a duplicate is refused, with only
   \ the may-claims the walk saw (INHSET) and CTL-OPAQUE for those it did not.
   dup 2 =  NMU @ 0 >  and  CHECK-SIG? and
   TORDER-ACTIVE @ TORDER-PARENT-GATE < and IF
      NMA @ NMU @ CHECK-REC-ADMIT
      NMA @ NMU @ INHSET @ CTL-OPAQUE or NORET-ADD
      NMA @ NMU @  NMOFF @ TADDR NMU @  RES-TRUE CHECKER-USIG-CERT-ADD-PENDING
   THEN
   dup 0 =  over 1 = or  MULTI-ERR? and  NMU @ 0 > and
   REJECTION:QUIET @ 0= and BWIN-UNJUDGED @ 0= and IF
      MULTI-REJECTED
   THEN ;

\ The source pre-pass learns definers in rows keyed by this checker's symbol ids
\ and appends each new row at this count. A frame rewinds the count with the
\ ids, so a scope that retires a symbol takes the row naming it.
variable VERIFY-DEFINER-N   0 VERIFY-DEFINER-N !
\ Its `parses:` rows are keyed by the same ids and rewound alike; their
\ terminators' bytes lie past the rows that hold them, so the count bounds
\ those bytes too.
variable VERIFY-PARSES-N   0 VERIFY-PARSES-N !

$0 constant RBF.UEND-OFF
$8 constant RBF.NEND-OFF
$10 constant RBF.SYMN-OFF
$18 constant RBF.SYMU-OFF
$20 constant RBF.CTN-OFF
$28 constant RBF.CTU-OFF
$30 constant RBF.LIN-OFF
$38 constant RBF.VRECN-OFF
$40 constant RBF.VRECF-OFF
$48 constant RBF.VRECND-OFF
$50 constant RBF.VNARGN-OFF
$58 constant RBF.VRECU-OFF
$60 constant RBF.CAND-OFF
$68 constant RBF.VSIG-OFF
$70 constant RBF.PKGMODE-OFF
$78 constant RBF.PKGU-OFF
$80 constant RBF.DFEREND-OFF
$88 constant RBF.COORD-OFF
$90 constant RBF.OVNDICT-OFF               \ the checker overlay's NDICT, log count,
$98 constant RBF.OVLOG-OFF                 \ CP and WIDN (CHECKER-OVERLAY:MARKS)
$A0 constant RBF.OVCP-OFF
$A8 constant RBF.OVWIDN-OFF
$B0 constant RBF.FLOOR-OFF
$B8 constant RBF.VDEFN-OFF
$C0 constant RBF.VPRSN-OFF
$C8 constant RBF.PROWE-OFF                 \ the kept parses rows' end
$D0 constant RBF.NDICT-OFF                 \ the dictionary's record count
$D8 constant RBF.SIMMN-OFF                 \ the source-immediate table's row count
$E0 constant RBF.PKGA-OFF                  \ the open package's name offset
$E8 constant RBF.USE-OFF                   \ the import rows (CK-USE-SAVE)
RBF.USE-OFF CK-USE-SNAP-BYTES + constant RBF-REC
\ RBF.COORD distinguishes an ordinary checker scope (-1) from a declaration
\ frame's START tag. The declaration runner uses zero: declarations do not nest.
-1 constant RBF-NO-COORDINATOR

: RBF.UEND ( ^a -- ^a ) RBF.UEND-OFF + ;
: RBF.NEND ( ^a -- ^a ) RBF.NEND-OFF + ;
: RBF.SYMN ( ^a -- ^a ) RBF.SYMN-OFF + ;
: RBF.SYMU ( ^a -- ^a ) RBF.SYMU-OFF + ;
: RBF.CTN ( ^a -- ^a ) RBF.CTN-OFF + ;
: RBF.CTU ( ^a -- ^a ) RBF.CTU-OFF + ;
: RBF.LIN ( ^a -- ^a ) RBF.LIN-OFF + ;
: RBF.VRECN ( ^a -- ^a ) RBF.VRECN-OFF + ;
: RBF.VRECF ( ^a -- ^a ) RBF.VRECF-OFF + ;
: RBF.VRECND ( ^a -- ^a ) RBF.VRECND-OFF + ;
: RBF.VNARGN ( ^a -- ^a ) RBF.VNARGN-OFF + ;
: RBF.VRECU ( ^a -- ^a ) RBF.VRECU-OFF + ;
: RBF.CAND ( ^a -- ^a ) RBF.CAND-OFF + ;
: RBF.VSIG ( ^a -- ^a ) RBF.VSIG-OFF + ;
: RBF.PKGMODE ( ^a -- ^a ) RBF.PKGMODE-OFF + ;
: RBF.PKGU ( ^a -- ^a ) RBF.PKGU-OFF + ;
: RBF.PKGA ( ^a -- ^a ) RBF.PKGA-OFF + ;
: RBF.USE ( ^a -- ^a ) RBF.USE-OFF + ;
: RBF.DFEREND ( ^a -- ^a ) RBF.DFEREND-OFF + ;
: RBF.COORD ( ^a -- ^a ) RBF.COORD-OFF + ;
: RBF.OVNDICT ( ^a -- ^a ) RBF.OVNDICT-OFF + ;
: RBF.OVLOG ( ^a -- ^a ) RBF.OVLOG-OFF + ;
: RBF.OVCP ( ^a -- ^a ) RBF.OVCP-OFF + ;
: RBF.OVWIDN ( ^a -- ^a ) RBF.OVWIDN-OFF + ;
: RBF.FLOOR ( ^a -- ^a ) RBF.FLOOR-OFF + ;
: RBF.VDEFN ( ^a -- ^a ) RBF.VDEFN-OFF + ;
: RBF.VPRSN ( ^a -- ^a ) RBF.VPRSN-OFF + ;
: RBF.PROWE ( ^a -- ^a ) RBF.PROWE-OFF + ;
: RBF.NDICT ( ^a -- ^a ) RBF.NDICT-OFF + ;
: RBF.SIMMN ( ^a -- ^a ) RBF.SIMMN-OFF + ;

16 constant RBF-CAP-INIT
variable RBF-CAP-V   RBF-CAP-INIT RBF-CAP-V !
create RBF-A-BOOT      RBF-CAP-INIT RBF-REC * allot
PERSISTED-PTR-VARIABLE RBF-A-P       RBF-A-BOOT RBF-A-P !
: RBF-BASE ( -- ^n ) RBF-A-P @ ;
variable RBF-DEPTH   0 RBF-DEPTH !

: RBF-GROW ( -- )
   RBF-CAP-V @ 2 * {: nc:n :}
   RBF-A-P RBF-CAP-V @ RBF-REC *  nc RBF-REC *  REG-GROW1
   nc RBF-CAP-V ! ;
: RBF-ENSURE ( -- )
   RBF-DEPTH @ RBF-CAP-V @ < IF exit THEN
   RBF-GROW ;
: RBF-CUR ( -- ^n )       RBF-DEPTH @ RBF-REC * RBF-BASE + ;

\ An import savepoint: the row count, then the reachable rows. Nested neutral
\ scopes may overwrite any prefix, not just append imports. The names the rows
\ point at lie below the scope's SYM-STR mark, which the rollback keeps.
: CK-USE-SAVE ( ^n n -- ) {: dst:^ u:n :}
   u dst !
   CK-USE-NAMES dst CELL + u 2 * ARENA-CELLS-COPY ;

: CK-USE-RESTORE ( ^n -- ) {: src:^ :}
   src CELL + CK-USE-NAMES src @ 2 * ARENA-CELLS-COPY ;

EXPORT CHECKER-OVERLAY:MARKS

: RBF-SAVE-INTO ( ^n -- ) {: r:^ :}
   UEND @ r RBF.UEND !
   NORET-END @ r RBF.NEND !
   SYM-N @ r RBF.SYMN !
   SYM-STR-U @ r RBF.SYMU !
   CTN @ r RBF.CTN !
   CT-STR-U @ r RBF.CTU !
   LIN-NDECL @ r RBF.LIN !
   VREC-N @ r RBF.VRECN !
   VREC-FIELD-N @ r RBF.VRECF !
   VREC-NODE-N @ r RBF.VRECND !
   VNARG-N @ r RBF.VNARGN !
   VREC-STR-U @ r RBF.VRECU !
   CHK-CAND @ r RBF.CAND !
   VSIG @ r RBF.VSIG !
   CHECKER-PACKAGE-MODE @ r RBF.PKGMODE !
   CHECKER-PACKAGE-U @ r RBF.PKGU !
   CHECKER-PACKAGE-NAME @ r RBF.PKGA !
   r RBF.USE CK-USE-SCAN-N CK-USE-SAVE
   MARKS
   r RBF.OVWIDN !  r RBF.OVCP !  r RBF.OVLOG !  r RBF.OVNDICT !
   DFER-END @ r RBF.DFEREND !
   SIMM-N @ r RBF.SIMMN !
   PASS-FLOOR @ r RBF.FLOOR !
   VERIFY-DEFINER-N @ r RBF.VDEFN !
   VERIFY-PARSES-N @ r RBF.VPRSN !
   PROW-END @ r RBF.PROWE !
   ndict@ r RBF.NDICT !
   RBF-NO-COORDINATOR r RBF.COORD ! ;

\ A record made inside a scope keeps no facts text past it: the checker binds a
\ name it does not know through its record's text (CHECK:READ-BOUND), so the
\ text would bring back a name the scope retired.
: RBF-FACTS-FORGET ( n -- )
   ndict@ swap ?do 0 i record-facts! loop ;

EXPORT CHECKER-OVERLAY:ROLLBACK

: RBF-RESTORE-FROM ( ^n -- ) {: r:^ :}
   r RBF.PKGA @ CHECKER-PACKAGE-NAME !
   r RBF.USE CK-USE-RESTORE
   r RBF.OVNDICT @  r RBF.OVLOG @  r RBF.OVCP @  r RBF.OVWIDN @
   ROLLBACK
   r RBF.UEND @ USIGS-RESTORE-END
   r RBF.NEND @ NORET-RESTORE-END
   r RBF.SYMN @ r RBF.SYMU @ SYM-REWIND
   r RBF.CTN @ r RBF.CTU @ CT-REWIND
   r RBF.LIN @ LIN-NDECL !
   r RBF.VRECN @ r RBF.VRECF @ r RBF.VRECND @ r RBF.VNARGN @ r RBF.VRECU @ VREC-REWIND
   r RBF.CAND @ CHK-CAND !
   r RBF.VSIG @ VSIG !
   r RBF.PKGMODE @ CHECKER-PACKAGE-MODE !
   r RBF.PKGU @ CHECKER-PACKAGE-U !
   r RBF.FLOOR @ PASS-FLOOR !
   r RBF.VDEFN @ VERIFY-DEFINER-N !
   r RBF.VPRSN @ VERIFY-PARSES-N !
   r RBF.PROWE @ PROW-REWIND
   r RBF.DFEREND @ DFER-REWIND
   r RBF.SIMMN @ SIMM-REWIND ;

EXPORT CHECKER-DECL-FRAME:EXT-FINALIZE
EXPORT CHECKER-DECL-FRAME:EXT-RESTORE
EXPORT CHECKER-DECL-FRAME:EXT-SAVE

: RBF-PUSH ( -- )
   RBF-ENSURE
   RBF-CUR RBF-SAVE-INTO
   RBF-DEPTH @ 1 + RBF-DEPTH !
   EXT-SAVE ;

EXPORT CHECKER-OVERLAY:CLOSE-SCOPE

: RBF-POP-WITH ( [ -- ] -- )
   0 UWIN-VALID !
   execute
   RBF-DEPTH @ CLOSE-SCOPE
   RBF-DEPTH @ 1 - RBF-DEPTH !
   RBF-CUR RBF-RESTORE-FROM
   RBF-CUR RBF.NDICT @ RBF-FACTS-FORGET ;

: RBF-POP ( -- )
   [: EXT-RESTORE ;] RBF-POP-WITH ;

: RBF-FINALIZE ( -- )     \ keep every published row and release the live savepoint
   EXT-FINALIZE
   RBF-DEPTH @ CLOSE-SCOPE
   RBF-DEPTH @ 1 - RBF-DEPTH ! ;

;package
package CHECKER-DECL-FRAME
EXPORT CHECK:RBF-REC
EXPORT CHECK:RBF-BASE
EXPORT CHECK:RBF-DEPTH
EXPORT CHECK:RBF.COORD
EXPORT CHECK:RBF-NO-COORDINATOR
EXPORT CHECK:RBF-POP-WITH
EXPORT CHECK:RBF-PUSH

: FRAME ( n -- ^n )
   RBF-REC * RBF-BASE + ;

: TOP ( -- ^n )
   RBF-DEPTH @ 1 - FRAME ;

: ORDINARY? ( n -- bool )
   FRAME RBF.COORD @ RBF-NO-COORDINATOR = ;

: DECL-AT? ( n n -- bool ) {: idx:n depth:n :}
   idx FRAME RBF.COORD @ depth = ;

: TOP-MATCH? ( n -- bool ) {: depth:n :}
   RBF-DEPTH @ 0= IF 0 0= 0= EXIT THEN
   RBF-DEPTH @ 1 - depth DECL-AT? ;

: RESTORE-TOP ( -- )         \ declaration-owned pop: no product-field rejection
   [: TYPES-RESTORE ;] RBF-POP-WITH ;

\ Is there one more leaked ordinary scope below the ones already counted?
: ORDINARY-BELOW? ( n -- n bool )
   dup RBF-DEPTH @ >= IF 0 0= 0= EXIT THEN
   dup RBF-DEPTH @ 1 - swap - ORDINARY? ;

: ORDINARY-RUN ( -- n )      \ ordinary scopes above the first declaration frame
   0
   BEGIN ORDINARY-BELOW? WHILE
      1 +
   REPEAT ;

: POP-ORDINARY ( n -- )
   BEGIN dup 0 > WHILE
      RESTORE-TOP
      1 -
   REPEAT
   drop ;

: START ( n -- ) {: depth:n :}
   RBF-PUSH
   TOP {: r:^ :}
   depth r RBF.COORD ! ;

: PREPARE ( n -- bool ) {: depth:n :}
   depth TOP-MATCH? 0= IF 0 0= 0= EXIT THEN
   RBF-DEPTH @ TYPES-READY ;

: FRAME-MISMATCH ( -- )
   s" checker: declaration rollback frame mismatch" EXIT-STATUS:INTERNAL die ;

\ Find the declaration's frame underneath leaked ordinary scopes before
\ consuming them. If it is not there the frame stack is beyond
\ repair, so fail closed while the legitimate outer frames are still intact.
: ROLLBACK ( n -- ) {: depth:n :}
   ORDINARY-RUN {: leaked:n :}
   RBF-DEPTH @ leaked - 1 - {: idx:n :}
   idx 0 < IF FRAME-MISMATCH THEN
   idx depth DECL-AT? 0= IF FRAME-MISMATCH THEN
   leaked POP-ORDINARY
   RESTORE-TOP ;

\ The declaration runner releases after its participant commits succeed.
: RELEASE ( -- )
   TYPES-RELEASE
   RBF-DEPTH @ 1 - RBF-DEPTH ! ;

;package
package CHECK

EXPORT CHECKER-OVERLAY:OPEN-SCOPE

\ The scope opener for replaying STANDALONE source. A separate file's package
\ context is whatever it declares, so this scope starts at top level: no open
\ package and no imports.
: CHECKER-SCOPE-START-NEUTRAL ( -- )
   RBF-PUSH
   [: RES-TRUE RBF-DEPTH @ OPEN-SCOPE ;] catch {: rc:n :}
   rc 0= IF EXIT THEN
   RBF-POP
   rc throw ;

: CHECKER-SCOPE-DONE ( -- )
   RBF-POP ;

: CHECKER-SCOPE-DEPTH ( -- n )
   RBF-DEPTH @ ;

public
\ Runs q in a checker scope at neutral top level, no open package and no
\ imports, and closes the scope however q ends.
: SCOPED ( [ -- ] -- ) {: q :}
   CHECKER-SCOPE-START-NEUTRAL
   q catch {: rc:n :}
   CHECKER-SCOPE-DONE
   rc 0 <> IF rc throw THEN ;
private

EXPORT CHECKER-OVERLAY:OPEN?

\ --- the verifier window --------------------------------------------------
\ A window that opens the checker overlay takes back at its done the rows its
\ source recorded; one opened while the overlay is open leaves them to it.
: CHECKER-VERIFY-PKG-START ( -- )
   CHECKER-VERIFY-WINDOW @ VERIFY-WINDOW-NONE <> IF NESTED-REJECT THEN
   AUTHORITY 0= IF CHECKER-PKG-CONTEXT-REJECT THEN 2drop drop
   OPEN? IF VERIFY-WINDOW-SHARED ELSE VERIFY-WINDOW-OWNED THEN {: win:n :}
   win VERIFY-WINDOW-OWNED = IF RBF-PUSH THEN
   [: RES-FALSE -1 OPEN-SCOPE ;] catch {: rc:n :}
   rc 0 <> IF
      win VERIFY-WINDOW-OWNED = IF RBF-POP THEN
      rc throw
   THEN
   win CHECKER-VERIFY-WINDOW !
   PASS-FLOOR @ VERIFY-FLOOR0 !  -1 PASS-FLOOR !
   CK-USE-ENGINE-DEPTH SOURCE-FLOOR ! ;

: CHECKER-VERIFY-PKG-DONE ( -- )
   CHECKER-VERIFY-WINDOW @ {: win:n :}
   -1 CLOSE-SCOPE
   0 BIND-HORIZON !
   VERIFY-FLOOR0 @ PASS-FLOOR !
   win VERIFY-WINDOW-OWNED = IF RBF-POP THEN
   VERIFY-WINDOW-NONE CHECKER-VERIFY-WINDOW ! ;

;package
package CHECKER-REG
EXPORT CHECK:CHECKER-VERIFY-PKG-START
EXPORT CHECK:CHECKER-VERIFY-PKG-DONE
' CHECKER-VERIFY-PKG-START DECLARATIONS CHECKER-OWNER-ABI:VERIFY-START-OFF + xt!
' CHECKER-VERIFY-PKG-DONE DECLARATIONS CHECKER-OWNER-ABI:VERIFY-DONE-OFF + xt!
;package
package CHECK

: CHECK-CANDIDATE-START ( -- )
   WRITE-WINDOW-CK
   CHK-CAND @ 0= IF UEND @ CK-PEND-FLOOR !  ndict@ CK-PEND-IX ! THEN   \ the outermost scope's floor
   RBF-PUSH
   -1 CHK-CAND !
   -1 VSIG !
   0 OPERANDS-SPENT ! ;   \ candidate text is RAW source: parsing-immediate payloads present

: CHECK-CANDIDATE-DONE ( n -- n )
   -1 OPERANDS-SPENT !    \ back to the engine-stream default (hook loads)
   0 BIND-HORIZON !
   WRAP-CLEAR          \ a candidate body armed like any other; the next real record is not its heir
   RBF-POP ;

: CHECKER-CANDIDATE-SCOPE-START ( -- )
   CHECK-CANDIDATE-START ;

: CHECKER-CANDIDATE-SCOPE-DONE ( -- )
   0 CHECK-CANDIDATE-DONE drop ;

: CHECK-WINDOWED ( str -- n )
   {: a:^ u:n :}
   WRITE-WINDOW-CK
   a u CHECK ;

\ Arms the text as the rejection source, so a rejection is placed in it,
\ `<input>:L:C`, only when no source is armed and PLACE finds no source for
\ the text's first byte; answers whether it armed.
: ARM-TEXT ( str -- bool ) {: a:^ u:n :}
   REJECTION:SOURCE$ nip 0=  a REJECTION:PLACES? 0= and {: arm:bool :}
   arm IF a u 1 1 0 REJECTION:SOURCE! THEN
   arm ;

: DISARM-TEXT ( bool -- )
   IF REJECTION:SOURCE-OFF THEN ;

global
: CHECK-CANDIDATE! ( ^u8 n -- n ) {: a:^ u:n :}
   WRITE-WINDOW-CK
   a u ARM-TEXT {: arm:bool :}
   RECOVERY-USED? {: recovery0:bool :}
   BIND-HORIZON @ {: horizon0:n :}
   CHK-PROBE @ {: probe0:n :}
   PU-N @ {: pu0:n :}
   CUR-DONE @ {: done0:n :}
   CUR-GAP @ {: gap0:n :}
   CHECK-CANDIDATE-START
   -1 CHK-PROBE !
   -1 CUR-DONE !                       \ a candidate's text is not the cursor's
   0 [: drop a u CHECK-WINDOWED ;] catch {: v rc:n :}
   arm DISARM-TEXT
   done0 CUR-DONE !
   gap0 CUR-GAP !
   probe0 CHK-PROBE !
   0 CHECK-CANDIDATE-DONE drop
   pu0 PU-N !                          \ a candidate's uses are never published
   recovery0 RECOVERY-USED!
   horizon0 BIND-HORIZON !
   rc 0= IF v EXIT THEN
   rc throw ;

;package
package CHECKER-PREFIX
EXPORT CHECK:CHECK-SCAN
EXPORT CHECK:CHECK-CANDIDATE-START
EXPORT CHECK:CHECK-CANDIDATE-DONE

EXPORT CHECK:BIND-HORIZON
EXPORT CHECK:CHECK-RESET
EXPORT CHECK:CHECKER-PREFIX-ACTIVE
EXPORT CHECK:CHECKER-PREFIX-CUT
EXPORT CHECK:CHECKER-PREFIX-MISS
EXPORT CHECK:CHECKER-PREFIX-ROLLBACK
EXPORT CHECK:CHK-PROBE
EXPORT CHECK:CUR-DONE
EXPORT CHECK:CUR-GAP
EXPORT CHECK:OPERANDS-SPENT
EXPORT CHECK:PU-N
EXPORT CHECK:WRITE-WINDOW-CK
: SCAN ( ^u8 n -- bool )
   0 CHECKER-PREFIX-MISS !
   CHECKER-PREFIX-CUT @ CHECKER-PREFIX-ROLLBACK !
   CHECK-RESET
   s" " CHECK-SCAN                     \ a prefix: no closer
   CHECKER-PREFIX-MISS @ 0= ;

EXPORT CHECKER-EFFECT-AUTHORITY:RECOVERY-USED?
EXPORT CHECKER-EFFECT-AUTHORITY:RECOVERY-USED!

\ Ask only whether the newly captured part binds. Open control, quotation and
\ local scopes remain prefixes; the declared result and publication belong to
\ the caller's eventual `;`. No immediate word runs during this read.
: BIND? ( str n -- n bool ) {: a:^ u:n cut:n :}
   WRITE-WINDOW-CK
   cut CHECKER-PREFIX-CUT !
   RECOVERY-USED? {: recovery0:bool :}
   BIND-HORIZON @ {: horizon0:n :}
   CHK-PROBE @ {: probe0:n :}
   PU-N @ {: pu0:n :}
   CUR-DONE @ {: done0:n :}
   CUR-GAP @ {: gap0:n :}
   CHECK-CANDIDATE-START
   -1 CHK-PROBE !
   -1 CUR-DONE !                    \ a prefix's text is not the cursor's
   -1 OPERANDS-SPENT !              \ the engine's body capture has spent parser operands
   -1 CHECKER-PREFIX-ACTIVE !
   RES-FALSE [: drop a u SCAN ;] catch {: answer rc:n :}
   0 CHECKER-PREFIX-ACTIVE !
   done0 CUR-DONE !
   gap0 CUR-GAP !
   probe0 CHK-PROBE !
   0 CHECK-CANDIDATE-DONE drop
   pu0 PU-N !                       \ a prefix's uses are never published
   recovery0 RECOVERY-USED!
   horizon0 BIND-HORIZON !
   rc 0= IF CHECKER-PREFIX-ROLLBACK @ answer EXIT THEN
   rc throw ;

;package
package CHECK

global
\ The quiet probe: a caller that wants a verdict without a rejection diagnostic
\ asks here, so the render counter stays inside this file.
: CHECK-QUIET-CANDIDATE! ( ^u8 n -- n ) {: a:^ u:n :}
   WRITE-WINDOW-CK
   1 REJECTION:QUIET +!
   0 [: drop a u CHECK-CANDIDATE! ;] catch {: v rc:n :}
   -1 REJECTION:QUIET +!
   rc 0= IF v EXIT THEN
   rc throw ;
private

: DEF-RESUMES? ( n -- bool )
   EXIT-STATUS:REJECTED =  MULTI-ERR? and ;

: DEF-RESUMED ( -- )
   0 LAYOUT-XPORT !  0 BIND-HORIZON ! ;

\ Resumes past a rejected body and records it when the walk threw; a throw
\ after the verdict came from the verdict's record step, which judged the name.
: CHECK-DEF-THREW ( -- )
   DEF-RESUMED  DVERD @ VERDICT-NONE = IF MULTI-REJECTED THEN ;

EXPORT CHECKER-TAPE:DONE

global
\ Like CHECK, for the text `NAME ( in -- out ) body` with no `:` or `;`: verifies
\ the body against the declared effect and rejects a mismatch.
: CHECK! ( ^u8 n -- n ) {: a:^ u:n :}
   WRITE-WINDOW-CK
   0 UWIN-VALID !
   -1 VSIG !
   TORDER-ACTIVE @ TORDER-SCAN-UNKNOWN @ {: oldactive:n oldunknown:n :}
   1 TORDER-DEPTH +!
   TORDER-CHECKING TORDER-ACTIVE !  0 TORDER-SCAN-UNKNOWN !
   PU-N @ {: pu0:n :}
   CUR-TAKE {: took:bool :}
   a u ARM-TEXT {: arm:bool :}
   0 [: drop a u CHECK-WINDOWED ;] catch {: v rc:n :}
   arm DISARM-TEXT
   -1 TORDER-DEPTH +!
   TORDER-SCAN-UNKNOWN @ {: unknown:n :}
   oldactive TORDER-ACTIVE !  oldunknown TORDER-SCAN-UNKNOWN !
   \ The record step reads what the walk left, so it runs before the exit's
   \ handlers, which may start a check; the exit runs whatever either throws.
   rc 0 <> IF 0 PARSE-COMPLETE ! THEN
   rc DEF-RESUMES? IF [: CHECK-DEF-THREW ;] ELSE [: ;] THEN catch {: trc:n :}
   [: pu0 took PUBLISH-EXIT ;] catch {: prc:n :}
   rc 0 <> rc DEF-RESUMES? 0= and IF rc throw THEN
   trc 0 <> IF trc throw THEN
   prc 0 <> IF prc throw THEN
   rc 0= IF v ELSE 0 THEN {: verdict:n :}
   0 VSIG !
   ARMED @ 0 <> {: recording:bool :}
   unknown 0= recording and IF a u verdict DONE THEN
   verdict ;
private

EXPORT CHECKER-EFFECT-AUTHORITY:SCAN

\ A TRUSTED: definition's declared effect is the assertion: the body is scanned
\ only to fill the source tape the optimizing elaborator reads, and its verdict
\ is never acted on, so the verdict's record stays silent while BWIN-UNJUDGED
\ is set; a rejection that stops the definition still prints. A scan that
\ returns leaves the clause latch as it found it: its record grants no
\ authority. A scan that throws abandons the definition, so it clears the
\ latch: a restored arm would be stepped by the next body check into the
\ record of whatever is defined next.
: CHECK-UNJUDGED! ( str -- n ) {: a:^ u:n :}
   WRITE-WINDOW-CK
   DOESEFF @ DOESPEND @ {: eff:n pend:n :}
   -1 BWIN-UNJUDGED !
   0 [: drop a u CHECK! ;] SCAN {: v rc:n :}
   0 BWIN-UNJUDGED !
   rc 0 <> IF DOES-EFF-CLEAR rc throw THEN
   eff DOESEFF !  pend DOESPEND !
   ARMED @ 0= 0=  LMODE @ 0= and  #CFC @ 0= and
   CONM @ 0= and  CHECKER-MATCH-MODE @ 0= and  MF-DEPTH @ 0= and
   FIELD-LOAN-PENDING @ 0= and PARSE-COMPLETE @ 0= 0= and UWIN-VALID !
   v ;

;package
package CHECKER-PREFLIGHT

: RUN ( -- )
   BODY-A@ BODY-U @ CHECK! VERDICT ! ;

: RESET ( -- )
   0 ACTIVE !
   NULL-PTR BODY-A!  0 BODY-U !
   NULL-PTR TARGET-A!  0 TARGET-U ! ;

EXPORT CHECK:WRITE-WINDOW-CK

\ Keep this target-forcing entry's name distinct from CHECK!, which RUN calls
\ above: the name CHECK takes by EXPORT must not shadow that call on replay.
: CHECK-TARGET! ( str str -- n )
   {: ba:^ bu:n ta:^ tu:n :}
   WRITE-WINDOW-CK
   ba BODY-A!  bu BODY-U !  ta TARGET-A!  tu TARGET-U !
   -1 ACTIVE !
   [: RUN ;] catch {: rc:n :}
   RESET
   rc 0 <> IF rc throw THEN
   VERDICT @ ;

;package
package CHECK

: DOES-DIN ( n -- n )
   FRESH MK-VAR MK-PTR swap MK-PUSH ;

\ Capture both rows' physical widths before checking binds either row tail;
\ they are read after the scan, so they are taken by value.
SCRATCH{
variable CD-DIN-CELLS
variable CD-DOUT-CELLS
}SCRATCH

: CHECK-DOES-EFFECT! ( n n -- ) {: din:n dout:n :}
   din ROW-CELLS CD-DIN-CELLS !
   dout ROW-CELLS CD-DOUT-CELLS ! ;

: CHECK-DOES-DIN-CELLS ( -- n ) CD-DIN-CELLS @ ;
: CHECK-DOES-DOUT-CELLS ( -- n ) CD-DOUT-CELLS @ ;

: RAW-SIG! ( n n n n -- )
   PD-BASE @ SGDBASE !
   SGROUT !  SGRIN !  SGOUT !  SGIN !   \ no `|` anywhere: both return rows are 0
   SG-WRITTEN!
   SGHASR @ IF SGRIN @ ELSE RBROW @ THEN SGRBASE !   \ no `|`: the implicit base is the sealed tail
   ;

\ Certifies `cast:` and `linear:` for both front ends, the engine's reader
\ keyword and verify-source's pre-pass.
package CHECKER-IDENTITY
\ Rejects the type token the signature parse named under E-CAST-FAM: why wa wu
\ says what is wrong.
: FAM-RAISE ( str -- ) {: wa:^ wu:n :}
   SGBAD-A @ SGBAD-U @ {: ta:^ tu:n :}
   ta tu CAST-ERROR:TOKEN!
   wa wu E-CAST-FAM CAST-ERROR:OPEN
   REJECTION:HELP  ta tu ta 0 SGBAD-FIX
   EXIT-STATUS:REJECTED REJECTION:STOP ;
\ A row that did not parse is rejected before a gate walks it; family tails read
\ the engine's open namespace.
: PARSE ( ^u8 n ^u8 n -- ) {: na:^ nu:n sa:^ su:n :}
   sa su BYTE-SPAN? 0= IF
      s" the signature's length describes no memory"
      s" length describes no memory"
      s" pass the signature text's address and its length in bytes"
      E-CAST-ARITY CAST-ERROR:RAISE-LABEL
   THEN
   NEW
   SGBAD-CLEAR
   sa su PARSE-SIG-RAW RAW-SIG!
   SGBAD-UNKNOWN? IF s" the signature names an unknown type" FAM-RAISE THEN
   SGBAD-ARITY? IF
      s" the signature gives the wrong number of arguments to the type family"
      FAM-RAISE
   THEN
   SGBAD @ SGBAD-UNSUPPLIED? 0= and IF
      sa su na nu CAST-ERROR:REJECT-SIGNATURE
   THEN ;

\ An unsupplied output variable, a view pack's shape CAST sends elsewhere, is a
\ bad row here.
: ROWS ( ^u8 n ^u8 n -- ) {: na:^ nu:n sa:^ su:n :}
   SGBAD-UNSUPPLIED? IF sa su na nu CAST-ERROR:REJECT-SIGNATURE THEN
   SGHASR @ 0 <> IF
      s" the signature declares a return-stack effect"
      s" a return-stack effect"
      s" write the signature with no `|` clause"
      E-CAST-ARITY CAST-ERROR:RAISE-LABEL
   THEN
   SGIN @ CAST-ROW-1? 0= IF
      s" the input is not one term over the stack below"
      s" input not one term"
      s" take exactly one term as the input, as `( n -- u8 )`, with one declaration per term"
      E-CAST-ARITY CAST-ERROR:RAISE-LABEL
   THEN
   SGOUT @ CAST-ROW-1? 0= IF
      s" the output is not one term over the stack below"
      s" output not one term"
      s" give exactly one term as the output, as `( n -- u8 )`, with one declaration per term"
      E-CAST-ARITY CAST-ERROR:RAISE-LABEL
   THEN
   SGIN @ CAST-ROW-UNDER  SGOUT @ CAST-ROW-UNDER <> IF
      s" the input and output rows end in different tails"
      s" different tails"
      s" end both sides in the same tail, as `( R n -- R u8 )`, or name no tail"
      E-CAST-ARITY CAST-ERROR:RAISE-LABEL
   THEN
   SGIN @ CAST-ROW-WIDTH  SGOUT @ CAST-ROW-WIDTH <> IF
      SGIN @ SGOUT @ CAST-ERROR:WIDTH-RAISE
   THEN
   SIG-UNRES @ IF E-CHECKER-IDENTITY-UNRESOLVED throw THEN ;

\ A variable STORED-DERIVE left stored binds no view and no linear value, so a
\ cast through it hands out neither: TV! checks every binding.
: STORED-RAISE ( -- )
   0 BEGIN dup 26 < WHILE
      dup NMAP-SLOT @
      dup 0 >= IF
         dup TVK@ TVK-STORED = IF dup NSCOPE-RAISE dup NLINEAR-RAISE THEN
      THEN drop
      1 +
   REPEAT drop ;

\ An unscoped cast that mints nothing is a representation cast: its pending
\ record takes DNAME-REP, and the compiler lowers it as identity.
: MARK-REP ( n -- ) rep-mark ;
public
\ Refuse first, register second: a refusal throws before the engine counts the
\ record, so it leaves no name.
: CAST ( ^u8 n ^u8 n -- ) {: na:^ nu:n sa:^ su:n :}
   na nu sa su PARSE
   CAST-SCOPED? IF
      VIEW-CAST-CERTIFY
      na nu CHECKER-QUALIFIED? IF
         s" a qualified name publishes past the private section"
         s" write the cast's name with no `PKG:` qualifier"
         E-CAST-SCOPE CAST-ERROR:RAISE
      THEN
      SIG-UNRES @ IF E-CHECKER-IDENTITY-UNRESOLVED throw THEN
      sa su na nu CHECKER-USIG-CERT-ROWS
   ELSE
      na nu sa su ROWS
      STORED-DERIVE STORED-RAISE
      CAST-WALKS {: mint:bool :}
      mint na nu CHECKER-QUALIFIED? and IF
         s" a qualified name publishes past the private section"
         s" write the cast's name with no `PKG:` qualifier"
         E-CAST-MINT CAST-ERROR:RAISE
      THEN
      sa su na nu CHECKER-USIG-CERT-ROWS
      mint 0= CK-LIVE-PEND? and IF ndict@ MARK-REP THEN
   THEN
   CHECKER-REC-SYM @ CK-CLOSE! ;

\ A `linear:` record takes no DNAME-REP: minting or erasing a token changes
\ its owner, never a scalar's representation.
: LINEAR ( ^u8 n ^u8 n -- ) {: na:^ nu:n sa:^ su:n :}
   na nu sa su PARSE
   na nu sa su ROWS
   LINEAR-CERTIFY
   na nu CHECKER-QUALIFIED? IF
      s" a qualified name publishes past the private section"
      s" write the row's name with no `PKG:` qualifier"
      E-LINEAR-SCOPE CAST-ERROR:RAISE
   THEN
   sa su na nu CHECKER-USIG-CERT-ADD
   CHECKER-REC-SYM @ CK-CLOSE! ;
;package
: CHECKER-DEFCAST ( str str -- )
   s" cast:" [: CHECKER-IDENTITY:CAST ;] CAST-ERROR:DECLARE ;
: CHECKER-LINEAR ( str str -- )
   s" linear:" [: CHECKER-IDENTITY:LINEAR ;] CAST-ERROR:DECLARE ;
;package
package CHECKER-REG
EXPORT CHECK:CHECKER-DEFCAST
EXPORT CHECK:CHECKER-LINEAR
' CHECKER-DEFCAST DECLARATIONS CAST-OFF + xt!
' CHECKER-LINEAR DECLARATIONS LINEAR-OFF + xt!
;package
package CHECK

\ A candidate scope latches nothing: its records are truncated when it ends, so
\ a stored offset would name a record that no longer exists.
: DOES-EFF-LATCH! ( str -- ) {: sa:^ su:n :}
   DOES-EFF-CLEAR
   DVERD @ VERDICT-CHECKED <> IF EXIT THEN
   CHK-CAND @ 0 <> IF EXIT THEN
   sa su CREATED-RECORD-BUILD
   {: rec:n :}
   rec 0= IF EXIT THEN                       \ unresolvable family: no row, no latch
   rec DOES-EFF-ARM ;

\ A created word declared `( in -- out )` needs a DOES> body typing as
\ `( in ^a -- out )`, since the CREATE stub pushes the data-field address
\ before the body.
: CHECK-DOES-RUN {: ba bu sa su :}
   ba bu CHECK-RESET
   -1 CLAUSE-WALK !
   0 TOK0 !
   sa SGA !  su SGU !
   sa su PARSE-SIG-RAW
   SIG-INPUT-FRESH
   SGBAD-FAIL!
   SIG-UNRES @ SGBAD @ 0= and IF
      2drop 2drop
      SIG-UNRES-A @ TBASE@ - FAILB !
      FAILB @ SIG-UNRES-U @ + FAILE !
      CHECKER-UNSEEN-MARK
      2 EXIT
   THEN
   RAW-SIG!  SG-SPLIT
   SG-ROWS-COLLECT
   NP-COLLECT
   SGIN @ DOES-DIN dup BROW !
   SGOUT @ CHECK-DOES-EFFECT!
   \ The record is the signature's as its body reads it, whatever the runtime
   \ row below measures; a refused or uncheckable clause keeps it for its packet.
   SIG-EFF-CACHE!
   \ The runtime row holds the signature's and the data-field address, so its
   \ counts replace any the signature set and name the unfit clause.
   SGBAD @ 0= IF BROW @ SGOUT @ SGRIN @ SGROUT @ ROWS-FIT? drop THEN
   BROW @ DCUR !
   SGHASR @ IF SGRIN @ dup RBROW ! RCUR ! THEN
   SIG-BODY-FRESH
   s" ;" CHECK-SCAN
   TORDER-SCAN-UNKNOWN @ IF WRAP-CLEAR SIG-EFF-DROP 2 EXIT THEN
   \ The end-of-body checks below pin no token of their own: their packet names
   \ the token walked last. A clause that walked none names its `;` instead.
   FAILSET @ 0= FAILTU @ 0= and IF s" ;" CLOSER-PIN! THEN
   0 LAYOUT-XPORT !                  \ boundary unification is never in transport mode
   CHECK-FOLD-EXITS
   CONM @ 0 <> IF MD-CON-TRUNC MDIAG! THEN     \ latch truncation BEFORE the boundary
   FIELD-LOAN-PENDING @ IF MD-FIELD-NAME MDIAG! SCOPE-FIELD-REJECT THEN
   CHECKER-MATCH-MODE @ 0 <>  MF-DEPTH @ 0 <>  or IF MD-TRUNC MDIAG! THEN
   CHECK-NO-BORROW
   CHECK-RETURNS? IF
      CHECK-FRESH-OUTPUT
      OK @ IF SGOUT @ DCUR ! THEN
   ELSE FRESH-CLAIM THEN
   DEF-LSC-CK
   CF-OPEN? IF MD-CF-OPEN CF-FAIL THEN
   CHECK-RBALANCE
   SGHASR? CHECK-RETURNS? and IF SGROUT @ RSUNI-COERCE THEN
   OK @ IF NP-CHECK THEN
   UNFIT-UNCHECKABLE
   CHECK-VERDICT dup DVERD !
   dup VERDICT-CHECKED =  over VERDICT-DEFERRED =  or IF SIG-EFF-DROP THEN
   dup 0= IF RIGID-DIAG-CLASSIFY THEN
   dup -1 = IF CALL-FINALIZE THEN
   \ A clause body is walked like any other but is nobody's definition body, and
   \ the next record after it is the CREATED word's, so what this walk armed
   \ dies here, before the tape's observer, which may refuse the clause.
   WRAP-CLEAR
   ARMED @ IF ba bu DVERD @ DONE THEN ;

\ The retained engine reaches CHECK-DOES! at the `;` of a definition it is
\ compiling, before that definition's own body is checked and recorded, so the
\ certified effect belongs to the record that follows. The source pre-verifier
\ reaches the clause after the definer's body, so its next record is the next
\ definition's, which would take a latch left armed.
: CHECK-DOES! {: ba bu sa su :}
   ba bu sa su CHECK-DOES-RUN
   sa su DOES-EFF-LATCH! ;            \ last: that parse reopens the term arena

\ A CLAUSE IS CHECKED IN ITS PARENT'S REGIME: unjudged bindings and no call
\ authority, as tier 0 checks it. With authority, a clause would be refused a
\ call its parent's body may make (E-CAP-TRUSTED on a colon word defined with
\ checking off).
: CHECK-DOES-UNJUDGED! ( str str -- n ) {: ba:^ bu:n sa:^ su:n :}
   -1 BWIN-UNJUDGED !
   0 [: drop ba bu sa su CHECK-DOES-RUN ;]
   SCAN {: v rc:n :}
   0 BWIN-UNJUDGED !
   rc 0 <> IF rc throw THEN
   sa su DOES-EFF-LATCH!
   v ;

\ The pre-verifier's entry also reports what it refuses, since CHECK-DOES-RUN
\ renders nothing. An uncheckable clause is rendered here too: the pre-pass
\ renders nothing of its own for a clause.
: DOES-NAME! ( str -- ) {: na:^ nu:n :}
   na DOES-NA !  nu DOES-NU !
   na nu TOKFOLD drop
   TKF nu CLAUSE-NAME NMU !  NMA ! ;

: DOES-REPORT ( n str -- n ) {: v:n na:^ nu:n :}
   v -1 = v 2 = or IF v EXIT THEN
   na nu DOES-NAME!
   v DVERD !
   REJECTION:QUIET @ 0= IF DIAG-PRINT THEN
   SIG-EFF-DROP
   v ;

\ A clause is inside its definer's definition, so a rejection that leaves the
\ clause's walk by a throw is, in a multi-error load, a rejected clause like one
\ its verdict rejects: its record, written where the rejection was found, is
\ its diagnostic.
PTR-VARIABLE SRC-DOES-BA   variable SRC-DOES-BU
PTR-VARIABLE SRC-DOES-SA   variable SRC-DOES-SU   variable SRC-DOES-VERDICT

: SRC-DOES-BODY ( -- )
   SRC-DOES-BA @ SRC-DOES-BU @ SRC-DOES-SA @ SRC-DOES-SU @ CHECK-DOES-RUN
   SRC-DOES-VERDICT ! ;

\ Drops a thrown clause's effect and, when the load resumes past its rejection,
\ clears what the walk left; its catcher throws any other code on.
: SRC-DOES-THREW ( n -- ) {: rc:n :}
   SIG-EFF-DROP
   rc DEF-RESUMES? IF DEF-RESUMED WRAP-CLEAR THEN ;

: CHECKER-SOURCE-DOES! ( str str str -- n )
   {: ba:^ bu:n sa:^ su:n na:^ nu:n :}
   WRITE-WINDOW-CK
   ba SRC-DOES-BA !  bu SRC-DOES-BU !  sa SRC-DOES-SA !  su SRC-DOES-SU !
   TORDER-ACTIVE @ TORDER-SCAN-UNKNOWN @ {: oldactive:n oldunknown:n :}
   1 TORDER-DEPTH +!
   TORDER-CHECKING TORDER-ACTIVE !  0 TORDER-SCAN-UNKNOWN !
   PU-N @ {: pu0:n :}
   CUR-TAKE {: took:bool :}
   [: SRC-DOES-BODY ;] catch {: rc:n :}
   -1 TORDER-DEPTH +!
   oldactive TORDER-ACTIVE !  oldunknown TORDER-SCAN-UNKNOWN !
   rc 0 <> IF rc SRC-DOES-THREW THEN
   [: pu0 took PUBLISH-EXIT ;] catch {: prc:n :}
   DOES-EFF-CLEAR
   rc 0 <> rc DEF-RESUMES? 0= and IF rc throw THEN
   prc 0 <> IF prc throw THEN
   rc 0 <> IF 0 EXIT THEN
   SRC-DOES-VERDICT @ {: v:n :}
   v VERDICT-CHECKED =  v VERDICT-DEFERRED =  or IF na nu CHECKER-SOURCE-CLAUSE THEN
   v na nu DOES-REPORT ;

: TOP-ROW? ( n str -- bool )
   {: sym:n a:^ u:n :}
   sym CHECKER-FIND-USIG-SYM IF RES-TRUE EXIT THEN
   sym PRIM-FIRST-IDX 0 <> IF RES-TRUE EXIT THEN
   sym a u TOK-IDENTITY 0 <> ;

\ Ends a top-level token's record, which a caller collecting many keeps.
: TOP-END ( -- n )
   MULTI-ERR? IF REJECTION:KEEP 0 EXIT THEN
   EXIT-STATUS:REJECTED REJECTION:STOP ;

\ A top-level token no scope defines, run when runs is set, else ticked; or one
\ with a malformed qualifier when bad is set.
: TOP-REJECT ( str bool bool -- n )
   {: a:^ u:n runs:bool bad:bool :}
   bad IF
      s" E-BAD-QUALIFIED-TOP-LEVEL" REJECTION:OPEN
      s" the qualified name `" REJECTION:EMIT-STR  a u REJECTION:EMIT-STR
      s" ` is malformed" REJECTION:EMIT-STR
      a u REJECTION:AT  s" malformed qualified name" REJECTION:EMIT-STR
      REJECTION:HELP
      s" write `" REJECTION:EMIT-STR  a u REJECTION:EMIT-STR
      s" ` with one `:` between the package and the word, as `PKG:WORD`" REJECTION:EMIT-STR
   ELSE
      a u BODY-KEYWORD? IF
         runs IF a u BODY-ONLY-RECORD ELSE a u KEYWORD-RECORD THEN
      ELSE a u NULL-PTR 0 UNDEFINED-RECORD THEN
   THEN
   TOP-END ;

\ What the load does with a top-level token it runs, or ticks when RUNS is
\ false. The answer is -1 when it resolves, or 2 when it resolves to a word that
\ may read the source after it; 3 when it resolves nowhere but a rendering
\ statement in scope may define it, the run's to judge; and 0 when the load
\ refuses it. A tick needs no row, but one of a primitive whose rows are other
\ packages' private ones is refused as its call is.
: TOP-ANSWER ( str bool -- n )
   {: a:^ u:n runs:bool :}
   runs  a u LITERAL-RANGE?  and IF a u REJECTION:LITERAL-RANGE  TOP-END EXIT THEN
   a u WALK {: sym:n why:n :}
   sym HIDDEN-REC? {: hidden:bool :}
   why 0 <> IF a u why RAISE THEN
   CHECKER-QBAD-TOK @ 0= IF
      a u sym TOP-BOUND-SYM IF
         dup TOP-CONTROL {: flags:n deferred:bool :}
         runs 0=  hidden 0=  and  over a u TOP-ROW?  or IF
            dup a u NAV-USE-NOW drop
            flags deferred runs TOP-RUNS EXIT
         THEN
      THEN drop
      a u UNSEEN-COVERS? IF 3 EXIT THEN
   THEN
   a u runs  CHECKER-QBAD-TOK @ 0 <>  TOP-REJECT ;

: CHECKER-VERIFY-TOP ( str bool -- n )
   {: a:^ u:n runs:bool :}
   WRITE-WINDOW-CK
   0 [: drop a u runs TOP-ANSWER ;] catch {: v rc:n :}
   rc 0= IF v EXIT THEN
   rc DEF-RESUMES? 0= IF rc throw THEN  DEF-RESUMED  0 ;

\ One rollback frame spans the head's scan, the clause's and publication; while
\ open, RBF-DEPTH stays nonzero so compaction relocates none of its cursors.
: CHECKER-NATIVE-DOES-BEGIN ( -- )
   RBF-PUSH  1 DOES-SPLIT ! ;

: CHECKER-NATIVE-DOES-PUBLISH ( str -- ) {: a:^ u:n :}
   DOESPEND @ 1 <> IF DOES-EFF-CLEAR EXIT THEN
   DOESEFF @ {: creates:n :}
   DOES-EFF-CLEAR
   creates 0= IF EXIT THEN
   a u CHECKER-RECORD-SYM? {: sym:n :}
   sym CHECKER-FIND-USIG-SYM 0= IF EXIT THEN
   sym EFFECT-EXTERNAL-SYM? 0= IF EXIT THEN
   creates sym NORET-CREATES@ = IF EXIT THEN
   sym sym CTL-FLAGS-SYM sym CTL-MASKS-SYM creates sym NORET-NONZERO@
   NORET-ADD-CREATES
   sym VIS-SYM-REC sym RECORD-CONTROL-AT! ;

: CHECKER-NATIVE-DOES-FINISH ( str bool -- )
   {: a:^ u:n committed:bool :}
   0 DOES-SPLIT !
   committed 0= IF
      DOES-EFF-CLEAR WRAP-CLEAR RBF-POP EXIT
   THEN
   a u CHECKER-NATIVE-DOES-PUBLISH ;

: CHECKER-NATIVE-DOES-COMMIT ( -- )
   RBF-FINALIZE ;
2 constant GENR-BADSIG                    \ the effect does not parse or no record
                                          \ holds it (SGBAD-UNFIT!); SGBAD says why

\ Ends a row's rejection: counted under --all-errors, else thrown.
: ROW-END ( -- )
   MULTI-ERR? IF REJECTION:KEEP EXIT THEN
   EXIT-STATUS:REJECTED REJECTION:STOP ;

: GENR-MESSAGE ( str n -- ) {: na:^ nu:n kind:n :}
   s" the `generates:` row names " REJECTION:EMIT-STR  na nu CODE-SPAN
   kind GENR-UNRESOLVED = IF
      s" , which is not defined here" REJECTION:EMIT-STR EXIT
   THEN
   kind GENR-CREATES = IF
      s" , which already states what it makes" REJECTION:EMIT-STR EXIT
   THEN
   kind GENR-READS-NONE = IF
      s" , which is deferred or reads no name with `parse-name`" REJECTION:EMIT-STR EXIT
   THEN
   s"  with an effect that cannot be recorded" REJECTION:EMIT-STR ;

: GENR-LABEL$ ( n -- str ) {: kind:n :}
   kind GENR-UNRESOLVED = IF s" not defined here" EXIT THEN
   kind GENR-CREATES = IF s" already states what it makes" EXIT THEN
   s" deferred, or reads no name" ;

: GENR-HELP ( str n -- ) {: na:^ nu:n kind:n :}
   kind GENR-UNRESOLVED = IF
      s" define " REJECTION:EMIT-STR  na nu CODE-SPAN
      s"  before this row, or spell it as its definition does" REJECTION:EMIT-STR EXIT
   THEN
   kind GENR-CREATES = IF
      s" delete this row: the `does>` clause of " REJECTION:EMIT-STR  na nu CODE-SPAN
      s" , an earlier `generates:` row or the definer it wraps states what it makes"
      REJECTION:EMIT-STR EXIT
   THEN
   s" make " REJECTION:EMIT-STR  na nu CODE-SPAN
   s"  read its name with `parse-name`, called directly or through words it" REJECTION:EMIT-STR
   s"  calls by name, not through a `defer` or an executed xt; or delete this row"
   REJECTION:EMIT-STR ;

\ A bad effect is placed and labelled as a bad stored signature is.
: GENR-SIG-PARTS ( str str -- ) {: na:^ nu:n sa:^ su:n :}
   sa su na nu SGBAD-TOKEN REJECTION:AT
   s" the signature does not parse here" SGBAD-LABEL$ REJECTION:EMIT-STR
   s" signature" REJECTION:NOTE  sa su SIG-TRIM REJECTION:EMIT-CUT
   SGBAD-SIZE? IF s" reason" REJECTION:NOTE  BADSIG-REASON THEN
   sa su na nu SGBAD-HELP ;

\ D's name NA NU and the effect SA SU the row states.
: GENERATES-REFUSE ( str str n -- )
   {: na:^ nu:n sa:^ su:n kind:n :}
   kind GENR-BADSIG =  SGBAD-AMBIG?  and
   IF s" E-USING-AMBIGUOUS" ELSE s" E-GENERATES-ROW" THEN
   REJECTION:OPEN  na nu kind GENR-MESSAGE
   kind GENR-BADSIG = IF
      na nu sa su GENR-SIG-PARTS
   ELSE
      na nu REJECTION:AT  kind GENR-LABEL$ REJECTION:EMIT-STR
      REJECTION:HELP  na nu kind GENR-HELP
   THEN
   ROW-END ;

\ `already`: what the caller knows of D beyond the checker. D's symbol and the created
\ record's offset+1, 0 for a refused row; an opaque D's row stands unjudged.
: CHECKER-GENERATES ( str str bool -- n n )
   {: na:^ nu:n sa:^ su:n already:bool :}
   na nu CHECKER-FIND-ACTIVE-SYM
   {: sym:n :}
   sym 0= IF na nu sa su GENR-UNRESOLVED GENERATES-REFUSE sym 0 EXIT THEN
   sym na nu NAV-USE-NOW
   already sym NORET-CREATES@ 0 <> or IF na nu sa su GENR-CREATES GENERATES-REFUSE sym 0 EXIT THEN
   sym TOP-CONTROL {: flags:n deferred:bool :}
   flags CTL-OPAQUE and 0= IF
      flags CTL-PARSES and 0= deferred or IF na nu sa su GENR-READS-NONE GENERATES-REFUSE sym 0 EXIT THEN
   THEN
   sa su CREATED-RECORD-BUILD
   {: rec:n :}
   rec 0= IF na nu sa su GENR-BADSIG GENERATES-REFUSE THEN
   sym rec ;

\ A signature is read as a definition head reads one. PARSE-SIG-RAW splits the
\ effect at spaces only, so a line feed inside it fuses the tokens around it.
: CHECKER-SIG-SPAN ( str -- n n )
   {: a:^ u:n :}
   0 BEGIN dup u < IF a over + c@ 33 < ELSE RES-FALSE THEN WHILE 1 + REPEAT
   {: open:n :}
   open u = IF u 0 EXIT THEN
   a open + c@ 40 <> IF u 0 EXIT THEN
   open BEGIN dup u < IF a over + c@ 41 <> ELSE RES-FALSE THEN WHILE 1 + REPEAT
   {: close:n :}
   close u = IF open 0 EXIT THEN
   open close 1 + ;

\ The first token on the line A U starts, empty when none is left on it.
: GENR-NEXT-TOKEN ( str -- str ) {: a:^ u:n :}
   0 BEGIN
      dup u < IF a over + c@ dup 33 <  swap 10 <>  and ELSE RES-FALSE THEN
   WHILE 1 + REPEAT
   {: start:n :}
   start BEGIN dup u < IF a over + c@ 32 > ELSE RES-FALSE THEN WHILE 1 + REPEAT
   {: end:n :}
   a start +  end start - ;

: GENR-SHAPE-OPEN ( str -- ) {: na:^ nu:n :}
   s" E-GENERATES-ROW" REJECTION:OPEN
   s" the `generates:` row for " REJECTION:EMIT-STR  na nu CODE-SPAN ;

\ Ends a malformed row's help with an example row for D's name NA NU, and
\ throws.
: GENR-SHAPE-STOP ( str -- ) {: na:^ nu:n :}
   s" , as `generates: " REJECTION:EMIT-STR  na nu REJECTION:EMIT-STR
   s"  ( n -- )`" REJECTION:EMIT-STR
   EXIT-STATUS:REJECTED REJECTION:STOP ;

\ D's name NA NU, followed by the input A U, which opens with no `(`.
: GENR-NO-EFFECT ( str str -- ) {: na:^ nu:n a:^ u:n :}
   na nu GENR-SHAPE-OPEN  s"  states no effect" REJECTION:EMIT-STR
   a u GENR-NEXT-TOKEN {: ta:^ tu:n :}
   tu 0= IF
      na nu REJECTION:AT  s" no effect follows" REJECTION:EMIT-STR
   ELSE
      ta tu REJECTION:AT  s" an effect opens with `(`" REJECTION:EMIT-STR
   THEN
   REJECTION:HELP
   s" write the effect of the words " REJECTION:EMIT-STR  na nu CODE-SPAN
   s"  creates after its name" REJECTION:EMIT-STR  na nu GENR-SHAPE-STOP ;

\ D's name NA NU, and the `(` at OPEN that no `)` closes.
: GENR-UNCLOSED ( str ^u8 -- ) {: na:^ nu:n open:^ :}
   na nu GENR-SHAPE-OPEN  s"  has no closing `)`" REJECTION:EMIT-STR
   open 1 REJECTION:AT  s" not closed" REJECTION:EMIT-STR
   REJECTION:HELP  s" end the effect with `)`" REJECTION:EMIT-STR
   na nu GENR-SHAPE-STOP ;

\ The keyword KA KU, with no name after it.
: GENR-NO-NAME ( str -- ) {: ka:^ ku:n :}
   s" E-GENERATES-ROW" REJECTION:OPEN
   ka ku CODE-SPAN  s"  has no definer name after it" REJECTION:EMIT-STR
   ka ku REJECTION:AT  s" no name follows" REJECTION:EMIT-STR
   REJECTION:HELP
   s" write the definer's name and the effect of the words it creates after "
   REJECTION:EMIT-STR  ka ku CODE-SPAN  s" D" GENR-SHAPE-STOP ;

\ The effect after D's name NA NU, read as a definition head reads a signature.
: GENR-SIG-READ ( str -- str )
   {: na:^ nu:n :}
   na nu + {: at:^ :}
   REJECTION:INPUT$ + at -
   {: left:n :}
   at left CHECKER-SIG-SPAN
   {: open:n close:n :}
   open left = IF na nu at left GENR-NO-EFFECT THEN
   close 0= IF na nu at open + GENR-UNCLOSED THEN
   at close + NULL-PTR - data-base INP-CELL + !
   at open + 1 +  close open - 2 - ;

global
\ generates: D ( effect ) at top level, after D's definition: a declared
\ boundary, not a checked body: its declaration id bars it from checked bodies.
\ The keyword is the last token the engine read when the word starts (TKA-CELL).
TRUSTED: generates: ( -- )
   data-base TKA-CELL + @  data-base TKL-CELL + @
   {: ka:^ ku:n :}
   parse-name
   {: na:^ nu:n :}
   nu 0= IF ka ku GENR-NO-NAME THEN
   na nu GENR-SIG-READ
   {: sa:^ su:n :}
   na nu sa su RES-FALSE CHECKER-GENERATES
   {: sym:n rec:n :}
   rec 0= IF EXIT THEN
   BIND-REC @ {: selected:^ :}
   sym sym CTL-FLAGS-SYM CTL-GENERATES or sym CTL-MASKS-SYM rec
   sym NORET-NONZERO@ NORET-ADD-CREATES
   selected sym RECORD-CONTROL-AT! ;
INTRINSIC-DECLARATION INTRINSIC
private
7 constant PARSES-LIST                    \ no `(`, no terminator, or no `)`

\ What is wrong with a `parses:`, `parses-through:` or `names:` row's target.
: ROW-WHY$ ( n -- str ) {: kind:n :}
   kind PARSES-UNDEFINED = IF s" , which is not defined here" EXIT THEN
   kind PARSES-QUALIFIED = IF s" , a malformed qualified name" EXIT THEN
   kind PARSES-SHADOW = IF s" , which a used package's public word shadows" EXIT THEN
   kind PARSES-AMBIGUOUS = IF s" , which more than one used package makes public" EXIT THEN
   kind PARSES-READS-NONE = IF s" , which reads no source after it" EXIT THEN
   kind NAMES-NO-STRING = IF s" , which takes no string" EXIT THEN
   kind PARSES-COUNT = IF s"  with no count of 0 or more" EXIT THEN
   s"  with a malformed list of terminators" ;

: ROW-LABEL$ ( n -- str ) {: kind:n :}
   kind PARSES-NO-TARGET = IF s" no word follows" EXIT THEN
   kind PARSES-UNDEFINED = IF s" not defined here" EXIT THEN
   kind PARSES-QUALIFIED = IF s" malformed qualified name" EXIT THEN
   kind PARSES-SHADOW = IF s" shadowed by a used public word" EXIT THEN
   kind PARSES-AMBIGUOUS = IF s" ambiguous here" EXIT THEN
   kind PARSES-READS-NONE = IF s" reads no source" EXIT THEN
   kind NAMES-NO-STRING = IF s" takes no string" EXIT THEN
   kind PARSES-COUNT = IF s" no count of 0 or more follows" EXIT THEN
   s" its list of terminators is malformed" ;

\ The message, place and label of a rejected row, keyword KA KU as the source
\ spells it and target TA TU, empty when none follows; the help follows.
: ROW-PARTS ( str str n -- ) {: ka:^ ku:n ta:^ tu:n kind:n :}
   s" the " REJECTION:EMIT-STR  ka ku CODE-SPAN
   kind PARSES-NO-TARGET = IF
      s"  row names no word" REJECTION:EMIT-STR  ka ku REJECTION:AT
   ELSE
      s"  row names " REJECTION:EMIT-STR  ta tu CODE-SPAN
      kind ROW-WHY$ REJECTION:EMIT-STR  ta tu REJECTION:AT
   THEN
   kind ROW-LABEL$ REJECTION:EMIT-STR
   REJECTION:HELP ;

\ The help for a target TA TU no word here answers.
: ROW-HELP ( str n -- ) {: ta:^ tu:n kind:n :}
   kind PARSES-QUALIFIED = IF
      s" write one colon between a package and its public word, as `PKG:WORD`"
      REJECTION:EMIT-STR EXIT
   THEN
   kind PARSES-SHADOW = IF
      s" rename the used package's public " REJECTION:EMIT-STR  ta tu CODE-SPAN
      s" , which hides the global word of that name" REJECTION:EMIT-STR EXIT
   THEN
   kind PARSES-AMBIGUOUS = IF
      s" write the package's name before the word, as `PKG:" REJECTION:EMIT-STR
      ta tu REJECTION:EMIT-STR  96 REJECTION:EMIT1 EXIT
   THEN
   s" write the row after the definition of " REJECTION:EMIT-STR  ta tu CODE-SPAN
   s" , spelled as the definition spells it" REJECTION:EMIT-STR ;

: PARSES-FORMS ( -- )
   s" `parses: W n` or `parses-through: W n ( E1 E2 )`" REJECTION:EMIT-STR ;

: PARSES-HELP ( str n -- ) {: ta:^ tu:n kind:n :}
   kind PARSES-NO-TARGET = IF
      s" name the word the row is for and the count of tokens it reads: "
      REJECTION:EMIT-STR  PARSES-FORMS EXIT
   THEN
   kind PARSES-COUNT = IF
      s" after " REJECTION:EMIT-STR  ta tu CODE-SPAN
      s"  write the count of tokens it reads, 0 or more: " REJECTION:EMIT-STR
      PARSES-FORMS EXIT
   THEN
   kind PARSES-LIST = IF
      s" after the count list at least one terminator between a standalone `(`"
      REJECTION:EMIT-STR
      s"  and `)`: `parses-through: W n ( E1 E2 )`" REJECTION:EMIT-STR EXIT
   THEN
   kind PARSES-READS-NONE = IF
      s" delete this row, or name a word that reads the source after it with"
      REJECTION:EMIT-STR  s"  `parse-name`" REJECTION:EMIT-STR EXIT
   THEN
   ta tu kind ROW-HELP ;

\ The row's keyword KA KU and target TA TU.
: PARSES-REFUSE ( str str n -- )
   {: ka:^ ku:n ta:^ tu:n kind:n :}
   s" E-PARSES-ROW" REJECTION:OPEN  ka ku ta tu kind ROW-PARTS
   ta tu kind PARSES-HELP
   ROW-END ;

: ROW-UNBOUND ( str -- n )
   {: a:^ u:n :}
   a u WALK nip {: why:n :}
   why E-USING-SHADOW-GLOBAL = IF PARSES-SHADOW EXIT THEN
   why E-USING-AMBIGUOUS = IF PARSES-AMBIGUOUS EXIT THEN
   why 0 <> IF a u why RAISE -1 EXIT THEN
   CHECKER-QBAD-TOK @ IF PARSES-QUALIFIED EXIT THEN
   a u UNSEEN-COVERS? IF -1 EXIT THEN
   PARSES-UNDEFINED ;

: PARSES-UNBOUND ( str str -- )
   {: ka:^ ku:n a:^ u:n :}
   a u ROW-UNBOUND {: kind:n :}
   kind 0 >= IF ka ku a u kind PARSES-REFUSE THEN ;

\ A row on a deferred or opaque word stands unjudged: what the word runs is unseen. With no
\ checker symbol to key it, a row that stands bounds nothing (NULL-PTR).
: PARSES-TARGET ( str str -- n n ^n )
   {: ka:^ ku:n a:^ u:n :}
   a u TOP-BINDING-REC {: sym:n eff1:n ctl:n rec:^ :}
   sym 0= IF
      ctl CHECKER-OWNER-ABI:BINDING-DEFER and 0 <> IF 0 0 NULL-PTR EXIT THEN
      ctl CTL-OPAQUE and 0 <> IF 0 0 NULL-PTR EXIT THEN
      ctl CTL-PARSES and 0 <> IF 0 0 NULL-PTR EXIT THEN
      ctl 0 <> IF ka ku a u PARSES-READS-NONE PARSES-REFUSE 0 0 NULL-PTR EXIT THEN
      ka ku a u PARSES-UNBOUND 0 0 NULL-PTR EXIT
   THEN
   sym a u NAV-USE-NOW
   ctl CHECKER-OWNER-ABI:BINDING-DEFER CTL-OPAQUE or and 0=
   ctl CTL-PARSES and 0= and IF ka ku a u PARSES-READS-NONE PARSES-REFUSE 0 0 NULL-PTR EXIT THEN
   eff1 0= IF 0 0 NULL-PTR EXIT THEN
   sym eff1 rec ;

\ The checks every row passes, for the engine words and the source pre-verifier,
\ each of which has read the whole row first, so a row refused under
\ --all-errors leaves none of its tokens to what follows. The symbol, the
\ visible record's offset + 1, the count and the selected record.
: PARSES-ROW-CHECK ( str str str bool -- n n n ^n )
   {: ka:^ ku:n ta:^ tu:n ca:^ cu:n bad:bool :}
   tu 0= IF ka ku ta tu PARSES-NO-TARGET PARSES-REFUSE 0 0 0 NULL-PTR EXIT THEN
   ca cu num-parse {: v:n flt:bool ok:bool :}
   ok 0=  flt or  v 0 <  or IF ka ku ta tu PARSES-COUNT PARSES-REFUSE 0 0 0 NULL-PTR EXIT THEN
   bad IF ka ku ta tu PARSES-LIST PARSES-REFUSE 0 0 0 NULL-PTR EXIT THEN
   ka ku ta tu PARSES-TARGET {: sym:n eff1:n rec:^ :}
   sym eff1 v rec ;

: CHECKER-PARSES-ROW ( str str str bool -- n n n )
   PARSES-ROW-CHECK drop ;

\ What a list token is to its reader: 0 a terminator, 1 the closing `)`, 2 the
\ end of the input, where no `)` came.
: PARSES-ITEM ( str -- n )
   {: a:^ u:n :}
   u 0= IF 2 EXIT THEN
   a u s" )" CORE-STR= IF 1 EXIT THEN
   0 ;

: PARSES-LIST-REST ( ^u8 -- str )
   {: last:^ :}
   parse-name {: a:^ u:n :}
   a u PARSES-ITEM {: kind:n :}
   kind 0 <> IF last kind EXIT THEN
   a u + RECURSE ;

\ The terminators as the input holds them, from the first one's start to the
\ last one's end, and true when the list is malformed.
: PARSES-LIST-LOAD ( -- str bool )
   parse-name s" (" CORE-STR= 0= IF NULL-PTR 0 RES-TRUE EXIT THEN
   parse-name {: a:^ u:n :}
   a u PARSES-ITEM 0 <> IF NULL-PTR 0 RES-TRUE EXIT THEN
   a u + PARSES-LIST-REST {: end:^ kind:n :}
   a  end a -  kind 2 = ;

\ Checks a row as CHECKER-PARSES-ROW does and keeps it by its word's symbol,
\ with the terminators L LU. The word's facts text, written before, lacks
\ the row: its record is held as CHECKER-NAMES-ROW's is.
: PARSES-KEEP ( str str str str bool -- )
   {: ka:^ ku:n ta:^ tu:n ca:^ cu:n la:^ lu:n bad:bool :}
   ka ku ta tu ca cu bad PARSES-ROW-CHECK {: sym:n eff1:n cnt:n rec:^ :}
   sym 0= IF EXIT THEN
   sym cnt la lu PROW-ADD-TEXT
   rec REC>INDEX sym HOLD-AMENDED ;

: NAMES-HELP ( str n -- ) {: ta:^ tu:n kind:n :}
   kind PARSES-NO-TARGET = IF
      s" name a word that looks its string operand up as a name: `names: W`,"
      REJECTION:EMIT-STR  s"  after the definition of `W`" REJECTION:EMIT-STR EXIT
   THEN
   kind NAMES-NO-STRING = IF
      s" delete this row, or name a word whose input row ends in the string"
      REJECTION:EMIT-STR  s"  `^u8 n` it looks up" REJECTION:EMIT-STR EXIT
   THEN
   ta tu kind ROW-HELP ;

\ The row's keyword KA KU and target TA TU.
: NAMES-REFUSE ( str str n -- )
   {: ka:^ ku:n ta:^ tu:n kind:n :}
   s" E-NAMES-ROW" REJECTION:OPEN  ka ku ta tu kind ROW-PARTS
   ta tu kind NAMES-HELP
   ROW-END ;

: STRING-IN? ( ^u8 -- bool )
   {: rec:^ :}
   rec E-DIN@ {: din:n :}
   0 din EFF-ROW-CON CC-N <> IF RES-FALSE EXIT THEN
   1 din EFF-ROW-TERM {: t:n :}
   t 0= IF RES-FALSE EXIT THEN
   t EFF-TAG@ EN-PTR <> IF RES-FALSE EXIT THEN
   t EFF-A@ {: p:n :}
   p EFF-TAG@ EN-CON =  p EFF-A@ CC-U8 =  and ;

: NAMES-TAKES-STRING? ( n n -- bool )
   {: sym:n eff1:n :}
   eff1 0 <> IF eff1 1 - E-PTR STRING-IN? EXIT THEN
   sym PRIM-FIRST-SYM {: off:n :}
   off 0= IF RES-FALSE EXIT THEN
   off E-PTR STRING-IN? ;

\ A target the top-level find binds is a use. Its symbol takes the mark when its
\ declared input row ends in the string pair ( ^u8 n ); any other word, one
\ with no inputs among them, refuses the row, so a row on the wrong word cannot
\ make every string before it a use. A cold selected record with no checker
\ symbol to key the mark leaves the row standing and binding nothing.
: CHECKER-NAMES-ROW ( str str -- )
   {: ka:^ ku:n ta:^ tu:n :}
   tu 0= IF ka ku ta tu PARSES-NO-TARGET NAMES-REFUSE EXIT THEN
   ta tu TOP-BINDING-REC {: sym:n eff1:n ctl:n selected:^ :}
   sym 0= IF
      ctl 0 <> IF EXIT THEN
      ta tu ROW-UNBOUND {: kind:n :}
      kind 0 >= IF ka ku ta tu kind NAMES-REFUSE THEN
      EXIT
   THEN
   sym ta tu NAV-USE-NOW
   sym eff1 NAMES-TAKES-STRING? 0= IF ka ku ta tu NAMES-NO-STRING NAMES-REFUSE EXIT THEN
   sym  sym CTL-FLAGS-SYM CTL-NAMES or  sym CTL-MASKS-SYM  NORET-ADD-SYM
   selected sym RECORD-CONTROL-AT! ;

\ PKG:TAIL is PKG's public TAIL, a bare name a global and never a package's own
\ word, and a name with a second colon none, though the checker's split reads
\ one whose first colon is at an edge as bare.
: NAMED-SYM ( str -- n )
   {: a:^ u:n :}
   a u CHECKER-QUALIFIED? IF CHECKER-QPKG$ CHECKER-QTAIL$ CHECKER-PUBLIC-SYM? EXIT THEN
   CHECKER-QBAD-TOK @ IF 0 EXIT THEN
   a u CHECKER-COLON-SCAN drop 1 > IF 0 EXIT THEN
   a u CHECKER-GLOBAL-SYM? ;

\ The call BIND-TOK bound, right after an `s"` literal: the literal's body as
\ written names the word. One with an escape binds nothing, since a rename could
\ not edit it as written, and every escape is shorter than its spelling, so the
\ body is as written when the bytes it makes are as long.
: NAMED-OPERAND-USE ( -- )
   SPAY-ARMED @ 0= IF EXIT THEN
   SPAY-BYTES nip SPAY-U @ <> IF EXIT THEN
   SPAY-B @ TADDR SPAY-U @ {: a:^ u:n :}
   a u NAMED-SYM a u NAV-USE ;
: NAMED-OPERAND-INSTALL ( -- ) [: NAMED-OPERAND-USE ;] is NAMED-OPERAND-XT ;
NAMED-OPERAND-INSTALL

\ A top-level literal's body the source pre-verifier found before a call of a
\ marked word is a use of the word it names, published at once.
: CHECKER-NAMED-USE ( str -- )
   {: a:^ u:n :}
   a u NAMED-SYM a u NAV-USE-NOW ;

\ ---- the interpret row: saved between top-level events -----------------------
0 constant TS.BASE   1 constant TS.RP    2 constant TS.RC   3 constant TS.WP
4 constant TS.WC     5 constant TS.OFF   6 constant TS.TV   7 constant TS.RV
8 constant TS.ENTRY  9 constant TS.LEVEL  10 constant TS.END  11 constant TS.CELLS
12 constant TS.ROFF
13 constant TS.LEFT-A  14 constant TS.LEFT-U
15 constant TS-ROW
PTR-VARIABLE TOP-SEG-P   variable TOP-SEG-CAP   variable TOP-SEG-N
variable TOP-BASE   variable TOP-ENTRY   variable TOP-EVAL-LEVEL   variable TOP-LIVE
NULL-PTR TOP-SEG-P !  0 TOP-SEG-CAP !  0 TOP-SEG-N !  0 TOP-BASE !  0 TOP-LIVE !
0 TOP-ENTRY !  0 TOP-EVAL-LEVEL !
\ The token of the segment's last event on an empty stack, 0 0 for none: a
\ stack left non-empty holds that event's cells at its bottom.
PTR-VARIABLE TOP-LEFT-A   variable TOP-LEFT-U
NULL-PTR TOP-LEFT-A !  0 TOP-LEFT-U !

: TS-AT ( n n -- ^n ) {: ix:n fld:n :} TOP-SEG-P ix TS-ROW NAV-CELL fld cells + ;
;package
package CHECKER-REG                       \ REG-PTR-CELLS is CHECKER-REG's private cast
EXPORT CHECK:TS-AT
: TS-PTR ( n n -- ^^u8 ) TS-AT BYTE-VIEW REG-PTR-CELLS ;
;package
package CHECK
EXPORT CHECKER-REG:TS-PTR

: TOP-SEG-UNMAP ( n -- ) {: ix:n :}
   ix TS.RP TS-PTR @ CELL-VIEW ix TS.RC TS-AT @ cells ARENA-UNMAP
   ix TS.WP TS-PTR @ CELL-VIEW ix TS.WC TS-AT @ cells ARENA-UNMAP ;

: TOP-LEFT-CLEAR ( -- )   NULL-PTR TOP-LEFT-A !  0 TOP-LEFT-U ! ;

: TOP-SEG-RELEASE ( -- )
   TOP-READ-P TOP-READ-CAP 1 ARENA-ROWS-RELEASE
   TOP-WRITE-P TOP-WRITE-CAP 1 ARENA-ROWS-RELEASE
   0 TOP-ROW-OFF !  0 TOP-ROW-CELLS !  0 TOP-RROW-OFF !  TOP-LEFT-CLEAR ;

: TOP-SEG-PARK ( -- )
   TOP-SEG-P TOP-SEG-CAP TOP-SEG-N @ 1 + TS-ROW ARENA-ROWS-ENSURE
   TOP-SEG-N @ {: ix:n :}
   TOP-BASE @ ix TS.BASE TS-AT !
   TOP-ENTRY @ ix TS.ENTRY TS-AT !  TOP-EVAL-LEVEL @ ix TS.LEVEL TS-AT !
   TOP-READ-P @ BYTE-VIEW ix TS.RP TS-PTR !   TOP-READ-CAP @ ix TS.RC TS-AT !
   TOP-WRITE-P @ BYTE-VIEW ix TS.WP TS-PTR !  TOP-WRITE-CAP @ ix TS.WC TS-AT !
   TOP-ROW-OFF @ ix TS.OFF TS-AT !  TOP-RROW-OFF @ ix TS.ROFF TS-AT !
   TOP-ROW-TV @ ix TS.TV TS-AT !  TOP-ROW-RV @ ix TS.RV TS-AT !
   TOP-READ-END @ ix TS.END TS-AT !  TOP-ROW-CELLS @ ix TS.CELLS TS-AT !
   TOP-LEFT-A @ ix TS.LEFT-A TS-PTR !  TOP-LEFT-U @ ix TS.LEFT-U TS-AT !
   ix 1 + TOP-SEG-N !
   NULL-PTR TOP-READ-P !  0 TOP-READ-CAP !
   NULL-PTR TOP-WRITE-P !  0 TOP-WRITE-CAP !  0 TOP-ROW-OFF !  0 TOP-ROW-CELLS !
   0 TOP-RROW-OFF !  TOP-LEFT-CLEAR ;

: TOP-SEG-UNPARK ( n -- ) {: at:n :}
   TOP-SEG-RELEASE
   TOP-SEG-N @ at 1 + ?DO i TOP-SEG-UNMAP LOOP
   at TS.BASE TS-AT @ TOP-BASE !
   at TS.ENTRY TS-AT @ TOP-ENTRY !  at TS.LEVEL TS-AT @ TOP-EVAL-LEVEL !
   at TS.RP TS-PTR @ TOP-READ-P !   at TS.RC TS-AT @ TOP-READ-CAP !
   at TS.WP TS-PTR @ TOP-WRITE-P !  at TS.WC TS-AT @ TOP-WRITE-CAP !
   at TS.OFF TS-AT @ TOP-ROW-OFF !  at TS.ROFF TS-AT @ TOP-RROW-OFF !
   at TS.TV TS-AT @ TOP-ROW-TV !  at TS.RV TS-AT @ TOP-ROW-RV !
   at TS.END TS-AT @ TOP-READ-END !  at TS.CELLS TS-AT @ TOP-ROW-CELLS !
   at TS.LEFT-A TS-PTR @ TOP-LEFT-A !  at TS.LEFT-U TS-AT @ TOP-LEFT-U !
   at TOP-SEG-N ! ;

: TOP-SEG-SPANS? ( n n n -- bool ) {: ix:n base:n lvl:n :}
   ix TS.BASE TS-AT @ base =  ix TS.ENTRY TS-AT @ lvl <= and  lvl ix TS.LEVEL TS-AT @ <= and ;

: TOP-SEG-FIND ( n n -- n ) {: base:n lvl:n :}
   TOP-SEG-N @ BEGIN dup 0 > WHILE
      1 - dup base lvl TOP-SEG-SPANS? IF EXIT THEN
   REPEAT drop -1 ;

: TOP-LEFT-MARK ( str n -- ) {: a:^ u:n live:n :}
   live 0= IF a TOP-LEFT-A !  u TOP-LEFT-U ! THEN ;

\ cnt unknown cells over row: each the row's one unknown term (TOP-UNKNOWN),
\ lost, so a typed consumer refuses them.
: TOP-ROW-PAD ( n n -- n ) {: row:n cnt:n :}
   row cnt 0 ?DO TOP-UNKNOWN @ swap MK-PUSH dup -1 swap P-LOST! LOOP ;

\ The top cnt entries of row, in order, over base. CHECKER-TOP-LOAD's row is
\ fresh from E-INST-FROM, which mints every entry and links it only from the
\ entry above, so the cnt-th entry is relinked to base in place.
: TOP-ROW-REBASE ( n n n -- n ) {: row:n cnt:n base:n :}
   cnt 0= IF base EXIT THEN
   base  row cnt 1 - ROW-DROP-N R-RES  P-REST!
   row ;

: TOP-PUSH ( n -- ) DCUR @ MK-PUSH DCUR ! ;

\ A top step reads its token as the whole scanned text: a refusal pins it at
\ offset 0, and a parsing word finds nothing after it, since the load parses
\ that text itself. The per-token setup is DO-TOK1's, on the token the event
\ stored in TOK-SPELL-A and TOK-SPELL-U; it resolves the token, so it runs inside
\ the step (TOP-RUN). The row's bottom is the live stack's floor: it is the
\ step's declared base, so a step that reaches under the live cells is refused
\ as a body that reaches under its declared inputs is. A step clears the check
\ state (CHECK-CLEAR) as a body check does: every result a check leaves is
\ consumed inside the token that ran it, and no later token reads one.
: TOP-BEGIN ( -- )
   TOK-SPELL-A @ TOK-SPELL-U @ {: a:^ u:n :}
   CHECK-CLEAR
   BROW @ SGDBASE !
   u TOKBUF-ENSURE  a TBASE !  u TBLEN !  0 TOK0 !  u TI !  0 TSTART !
   0 CALL-HIT !  0 TOK-DONE !  0 BWIN-HIT !  0 STALE-HIT !
   NULL-PTR TOK-ABI-EFF !  0 TOK-ABI-DONE !
   a TOK-SPELL-A !  u TOK-SPELL-U !  -1 USING-HOLD !
   a u TOKFOLD drop
   a u CAP-FAIL
   0 EXEC-OPAQUE !  0 CATCH-OPAQUE !
   TKF TKFU @ s" xt!" CORE-STR= XT-DECL !
   TKF TKFU @ LAYOUT-XPORT-TOK? IF
      TKF TKFU @ TOK-CORE-OP?
   ELSE RES-FALSE THEN LAYOUT-XPORT ! ;

\ The spelling is tested first, so no other token is bound for it.
: TOP-EVALUATE? ( -- bool )
   TKF TKFU @ s" evaluate" CORE-STR= 0= IF RES-FALSE EXIT THEN
   TKF TKFU @ TOK-CORE-OP? ;

\ A type whose cell count is not known here; a stale value keeps its type's.
: TOP-OPEN-TYPE? ( n -- bool )
   T-UNSTALE T-RES {: t:n :}
   t LAYOUT-PARAM? 0= IF RES-FALSE EXIT THEN
   t HIDDEN-PARAM? 0=  t LAYOUT-WIDTH-OPEN? and ;

\ A type the interpret row holds as one item over more than one cell, or over a
\ count not known here; a stale value keeps the cells of its type.
: TOP-WIDE-TYPE? ( n -- bool )
   {: ty:n :}
   ty TOP-OPEN-TYPE? IF RES-TRUE EXIT THEN
   ty T-UNSTALE T-RES T-WIDTH 1 > ;

\ The k items atop row hold a multi-cell value.
: TOP-WIDE-IN-K? ( n n -- bool ) {: row:n k:n :}
   row 0 BEGIN dup k < WHILE
      over R-RES TAG S-PUSH <> IF 2drop RES-FALSE EXIT THEN
      over R-RES P>TYPE TOP-WIDE-TYPE? IF 2drop RES-TRUE EXIT THEN
      swap R-RES P>REST swap
      1 +
   REPEAT 2drop RES-FALSE ;

\ The items atop the row the guard judges: a transport's; 0 for `drop`, which
\ CHECKER-TOP-MOVE judges, and for any other word.
: TOP-XPORT-K ( -- n )
   TKF TKFU @ {: a:^ u:n :}
   a u s" drop" CORE-STR= IF 0 EXIT THEN
   LAYOUT-XPORT @ IF a u WF-XPORT-K EXIT THEN
   0 ;

\ The engine word spelled a u, run at top level, would take part of a
\ multi-cell value: rejected before it runs, at the word.
: TOP-LAYOUT-REJECT ( str -- ) {: a:^ u:n :}
   s" E-LAYOUT-VALUE" REJECTION:OPEN
   s" `" REJECTION:EMIT-STR  a u REJECTION:EMIT-STR
   s" ` at top level would take part of a value held in more than one cell"
   REJECTION:EMIT-STR
   a u REJECTION:AT  s" takes part of a multi-cell value" REJECTION:EMIT-STR
   REJECTION:HELP
   s" pass the value to a word whose effect names its type, or move `"
   REJECTION:EMIT-STR  a u REJECTION:EMIT-STR
   s" ` inside a definition" REJECTION:EMIT-STR
   EXIT-STATUS:REJECTED REJECTION:STOP ;

\ At top level the engine's one-cell primitive runs: over a multi-cell value it
\ would move part of it.
: TOP-XPORT-GUARD ( -- )
   TOP-XPORT-K {: k:n :}
   k 0= IF EXIT THEN
   TKF TKFU @ WF-XPORT-RROW? IF RCUR ELSE DCUR THEN @  k
   TOP-WIDE-IN-K? 0= IF EXIT THEN
   TOK-SPELL-A @ TOK-SPELL-U @ TOP-LAYOUT-REJECT ;

\ `evaluate` has no row: the text it reads steps itself, one level deeper on
\ this segment, so the step takes its string and leaves the rest of the row as
\ it was.
: TOP-WORD-STEP ( -- )
   TOP-BEGIN
   TOP-XPORT-GUARD
   TOP-EVALUATE? IF
      FRESH MK-VAR FRESH MK-VAR FRESH MK-ROW MK-PUSH MK-PUSH
      STEP-BORROWS? IF STEP-UNDERFLOW! EXIT THEN
      1 TOP-EVAL-LEVEL +!
      DCUR @ ROW-DROP-1 ROW-DROP-1 DCUR !  EXIT
   THEN
   TKF TKFU @ SPELLED-STEP? 0= IF
      DCUR @ XE-PD !  RCUR @ XE-PR !
      TKF TKFU @ DO-TOK
   THEN ;

\ `' W` pushes W's quotation when W has a row, else an opaque xt, and rejects
\ a primitive whose rows are other packages' private ones, as `['] W` does in a
\ body. W is the word the load's own find selects. A declaration's tick is
\ opaque, so no cell can carry it into a body.
: TOP-TICK-STEP ( -- )
   TOP-BEGIN
   TKF TKFU @ CHECKER-TOP-SYM {: sym:n hit:bool :}
   hit IF sym HIDDEN-REC? IF TKF TKFU @ CALL-UNDEFINED drop EXIT THEN THEN
   hit IF sym DECLARATION-SYM? IF OPAQUE-XT TOP-PUSH EXIT THEN THEN
   FEP-CLEAR
   hit IF sym CHECKER-FIND-USIG-SYM drop THEN
   FEP-HIT? IF FEP @ TICK-QUOT FEP @ USIG-SYM@ BTICK-EDGE ELSE OPAQUE-XT THEN
   TOP-PUSH ;

\ FILE-POS's last answer: a byte offset into the owned bytes, its line, its column.
variable FPOS-OFF  variable FPOS-LN  variable FPOS-CL

\ Forgets FILE-POS's last answer, which counts over the owned bytes: every
\ publication of the owned bytes runs it.
: FILE-POS-CLEAR ( -- )
   0 FPOS-OFF !  1 FPOS-LN !  1 FPOS-CL ! ;
FILE-POS-CLEAR

\ The file line, column and byte of t, which lies in the bytes the loader owns,
\ counted as REJECTION:POSITION counts from their first byte. It counts on from
\ its last answer when t is at or past it, so a file costs one pass.
: FILE-POS ( ^u8 -- n n n )
   {: t:^ :}
   REJECTION:OWNED$ drop {: b:^ :}
   t b - {: at:n :}
   at FPOS-OFF @ < IF FILE-POS-CLEAR THEN
   BEGIN FPOS-OFF @ at < WHILE
      b FPOS-OFF @ + c@ 10 = IF
         FPOS-LN @ 1 + FPOS-LN !  1 FPOS-CL !
      ELSE
         FPOS-CL @ 1 + FPOS-CL !
      THEN
      FPOS-OFF @ 1 + FPOS-OFF !
   REPEAT
   FPOS-LN @ FPOS-CL @ at ;

\ The wall's rejection of a duplicate, for one refused off the wall.
EXPORT POINTER-STORAGE:RESERVED-PTR-U8-CELL
INP-CELL RESERVED-PTR-U8-CELL SRC-CURSOR
PENDTKA-CELL RESERVED-PTR-U8-CELL SRC-NAME

\ The name a u as the source holds it, where the wall places a duplicate. At a
\ colon definition's closing `;` the definition is pending (PEND-CELL) and its
\ opener kept where its name starts (PENDTKA-CELL); create, variable and
\ constant run the hook with nothing pending, PENDTKA-CELL
\ still at the last colon definition's name and the cursor just past their own.
\ For DEFINITION's first token, a pending colon definition's name is taken
\ from the source when it lies between the file's first owned byte and the
\ cursor; any other name is a u as DEFINITION read it.
: SOURCE-NAME$ ( str -- str )
   {: a:^ u:n :}
   REJECTION:OWNED$ drop SRC-NAME @ SRC-CURSOR @ {: o:^ nm:^ p:^ :}
   data-base PEND-CELL + @ 0 <>  nm o - 0 >= and  p nm - 0 >= and
   if nm u else a u then ;

\ The record of a duplicate a u placed at t tu, written through its help; the
\ caller stops or keeps it.
: DUPLICATE-RECORD ( str str -- )
   {: a:^ u:n t:^ tu:n :}
   a u TFAM-GENERATOR {: f:^ fu:n gen:bool :}
   gen IF  a u t tu f fu REJECTION:DUPLICATE-GENERATED  EXIT  THEN
   a u t tu REJECTION:DUPLICATE ;

: REJECT-DUPLICATE ( str str -- )
   DUPLICATE-RECORD  EXIT-STATUS:REJECTED REJECTION:STOP ;

\ The token's verdict as DO-TOK1 and CHECK take it: a checked step runs, as
\ does one deferred to the run; a refused or uncheckable one renders the
\ diagnostic the same token gets in a body and throws the checker's rc, so the
\ word never runs.
: TOP-JUDGE ( -- )
   EXEC-OPAQUE @ IF MD-EXEC-OPAQUE MDIAG! THEN
   CATCH-OPAQUE @ IF MD-CATCH-OPAQUE MDIAG! THEN
   FENCE-OK RAW-FENCE !  0 ONE-CELL-HIT !  0 STEP-HIT !  0 FACT-HIT !  0 BASE-PTR-HIT !  0 STALE-HIT !  0 SCOPE-HIT !  0 XT-DECL !
   0 LAYOUT-XPORT !
   CHECK-NO-BORROW
   OK @ 0= FAILSET @ 0= and IF -1 FAILSET ! THEN
   0 USING-HOLD !
   TOK-SPELL-A @ TOK-SPELL-U @ REFUSAL-STEP
   CHECK-VERDICT dup DVERD ! {: v:n :}
   v VERDICT-CHECKED =  v VERDICT-DEFERRED =  or  RCOMPILE @ 0=  and IF EXIT THEN
   REJECTION:QUIET @ 0= IF DIAG-PRINT THEN
   EXIT-STATUS:REJECTED throw ;

\ A step is a guest in the term pools: an event fires between the tokens of a
\ declaration whose terms sit in the pools, on the stack and in its variables,
\ so the step allocates above the pools' marks, where NEW would reset them, and
\ releases back to the marks.
SCRATCH{
variable TG-FV     variable TG-SPN     variable TG-QEN     variable TG-PTRN
variable TG-STLN   variable TG-ATOMN   variable TG-PARAMN  variable TG-PSCR
variable TG-PARG   variable TG-RIGID   variable TG-RGN     variable TG-EXT
variable TG-GEN    variable TG-TERM-NAME
}SCRATCH

: TOP-GUEST ( -- )
   FV @ TG-FV !  SPN @ TG-SPN !  QEN @ TG-QEN !  PTRN @ TG-PTRN !  STLN @ TG-STLN !
   ATOMN @ TG-ATOMN !  TERM-NAME-N @ TG-TERM-NAME !  PARAMN @ TG-PARAMN !  PARAM-SCR-N @ TG-PSCR !  PARG-N @ TG-PARG !
   RIGID-N @ TG-RIGID !  RGN-N @ TG-RGN !  EXT-N @ TG-EXT !  GEN-N @ TG-GEN !
   NEW-WALK ;

: TOP-RELEASE ( -- )
   TG-FV @ TV-CLEAR-FROM
   TG-FV @ FV !  TG-SPN @ SPN !  TG-QEN @ QEN !  TG-PTRN @ PTRN !  TG-STLN @ STLN !
   TG-ATOMN @ ATOMN !  TG-TERM-NAME @ TERM-NAME-N !  TG-PARAMN @ PARAMN !  TG-PSCR @ PARAM-SCR-N !  TG-PARG @ PARG-N !
   TG-RIGID @ RIGID-N !  TG-RGN @ RGN-N !  TG-EXT @ EXT-N !  TG-GEN @ GEN-N ! ;

\ The flag is on while the step resolves and steps its token and its verdict is
\ taken, so the judge resolves the token as the step did; a throw from either (a
\ scope refusal raises) turns it off too, and releases the step's terms.
: TOP-RUN ( [ -- ] -- )
   -1 TOP-STEP !  catch {: rc:n :}
   rc 0 <> IF 0 TOP-STEP !  TOP-RELEASE  rc throw THEN
   ['] TOP-JUDGE catch {: jrc:n :}  0 TOP-STEP !
   jrc 0 <> IF TOP-RELEASE  jrc throw THEN ;

: TOP-SEG-PRUNE ( n -- ) {: lvl:n :}
   BEGIN TOP-ENTRY @ lvl >  TOP-LIVE @ 0 <> and WHILE
      TOP-SEG-N @ 0 > IF TOP-SEG-N @ 1 - TOP-SEG-UNPARK ELSE TOP-SEG-RELEASE  0 TOP-LIVE ! THEN
   REPEAT ;

: CHECKER-TOP-SEGMENT ( n -- ) {: base:n :}
   data-base EVALD-CELL + @ {: lvl:n :}
   lvl TOP-SEG-PRUNE
   TOP-LIVE @ 0= IF base TOP-BASE !  lvl TOP-ENTRY !  lvl TOP-EVAL-LEVEL !  -1 TOP-LIVE !  EXIT THEN
   TOP-BASE @ base =  TOP-ENTRY @ lvl <= and  lvl TOP-EVAL-LEVEL @ <= and IF lvl TOP-EVAL-LEVEL !  EXIT THEN
   base lvl TOP-SEG-FIND dup 0 >= IF TOP-SEG-UNPARK  lvl TOP-EVAL-LEVEL !  EXIT THEN drop
   TOP-SEG-PARK  base TOP-BASE !  lvl TOP-ENTRY !  lvl TOP-EVAL-LEVEL ! ;

\ Start a step on a live stack of n cells: DCUR is the saved row with the cells
\ it knows at the top. A longer row loses entries from its top until it fits; a
\ shorter one is padded beneath with unknown cells. RCUR is the saved return row
\ over RBROW, the step's return base, so a step that reads below the cells
\ earlier steps pushed is refused as a body that reads below its own is.
: CHECKER-TOP-LOAD ( n -- ) {: live:n :}
   TOP-GUEST  PE-N MK-STALE TOP-UNKNOWN !
   TOP-ROW-OFF @ 0=  TOP-RROW-OFF @ 0=  and IF BROW @ live TOP-ROW-PAD DCUR ! EXIT THEN
   TOP-ROW-TV @ TOP-ROW-RV @ E-INST-COUNTS
   TOP-ROW-OFF @ 0= IF BROW @ live TOP-ROW-PAD DCUR ! ELSE
      TOP-ROW-OFF @ TOP-READ-P @ BYTE-VIEW E-INST-FROM
      BEGIN dup ROW-CELLS live > WHILE ROW-DROP-1 REPEAT {: row:n :}
      row row ROW-TERMS  BROW @ live row ROW-CELLS - TOP-ROW-PAD  TOP-ROW-REBASE DCUR !
   THEN
   TOP-RROW-OFF @ 0= IF EXIT THEN
   TOP-RROW-OFF @ TOP-READ-P @ BYTE-VIEW E-INST-FROM {: rrow:n :}
   rrow rrow ROW-TERMS RBROW @ TOP-ROW-REBASE RCUR ! ;

: TOP-READ-AT ( n -- ^u8 ) TOP-READ-P @ BYTE-VIEW + ;

\ The cells a stored entry fills when a load reads it back: its recorded width,
\ except a cell lost to a live scope, which comes back as one unknown cell
\ (E-COPY*'s S-PUSH arm).
: TOP-PUSH-CELLS ( n -- n ) {: off:n :}
   off TOP-READ-AT EN.A @ TOP-READ-AT {: ty:^ :}
   ty EN.TAG @ EN-STALE =  ty EN.A @ 0=  and IF 1 EXIT THEN
   off TOP-READ-AT EN.C @ 1 - ;

: TOP-ROW-COUNT ( -- n )
   0 TOP-ROW-OFF @ BEGIN dup TOP-READ-AT EN.TAG @ EN-PUSH = WHILE
      dup TOP-PUSH-CELLS rot + swap  TOP-READ-AT EN.B @
   REPEAT drop ;

\ Keep DCUR and RCUR after a step: copy them into the write buffer under one set
\ of maps, so a type variable both rows hold stays one, and the write buffer
\ becomes the read buffer the next load instantiates; release the step's terms.
: CHECKER-TOP-SAVE ( -- )
   DCUR @ ROW-TERMS 0=  RCUR @ ROW-TERMS 0=  and IF
      0 TOP-ROW-OFF !  0 TOP-ROW-CELLS !  0 TOP-RROW-OFF !  TOP-RELEASE EXIT
   THEN
   -1 TOP-COPYING !
   CELL E-WEND!
   E-COPY-MAPS-RESET
   DCUR @ ROW-TERMS 0 <> IF DCUR @ ELSE 0 THEN {: d:n :}
   RCUR @ ROW-TERMS 0 <> IF RCUR @ ELSE 0 THEN {: r:n :}
   d 0 r 0 E-LET-PLAN
   d 0 <> IF d E-COPY ELSE 0 THEN {: off:n :}
   r 0 <> IF r E-COPY ELSE 0 THEN TOP-RROW-OFF !
   0 TOP-COPYING !
   TOP-READ-P @ TOP-WRITE-P @ {: old:^ next:^ :}
   TOP-READ-CAP @ TOP-WRITE-CAP @ {: oldcap:n nextcap:n :}
   next TOP-READ-P !  nextcap TOP-READ-CAP !
   old TOP-WRITE-P !  oldcap TOP-WRITE-CAP !
   off TOP-ROW-OFF !  EC-TVN @ TOP-ROW-TV !  EC-RVN @ TOP-ROW-RV !
   TOP-WRITE-END @ TOP-READ-END !
   off 0 <> IF TOP-ROW-COUNT ELSE 0 THEN TOP-ROW-CELLS !
   TOP-RELEASE ;

\ A record no source places is placed at the step's token.
: TOP-STEP-LOAD ( [ -- ] n -- )
   {: q live:n :}
   live CHECKER-TOP-LOAD
   TOK-SPELL-A @ TOK-SPELL-U @ [: q TOP-RUN ;] REJECTION:READING ;

: CHECKER-TOP-WORD ( str n -- )
   {: a:^ u:n live:n :}
   a u live TOP-LEFT-MARK
   a TOK-SPELL-A !  u TOK-SPELL-U !  ['] TOP-WORD-STEP live TOP-STEP-LOAD ;

: CHECKER-TOP-TICK ( str n -- )
   {: a:^ u:n live:n :}
   a u live TOP-LEFT-MARK
   a TOK-SPELL-A !  u TOK-SPELL-U !  ['] TOP-TICK-STEP live TOP-STEP-LOAD ;

\ `constant`, spelled a u, would take one cell of the multi-cell value atop the
\ row, the value it would name w v. A hidden cell's type names its whole value.
: TOP-CONSTANT-REJECT ( str str -- ) {: a:^ u:n w:^ v:n :}
   DCUR @ R-RES P>TYPE  dup HIDDEN-PARAM? IF MK-LOGICAL THEN {: ty:n :}
   SEEN-RESET RATOM-RESET 0 NLET !
   s" E-LAYOUT-VALUE" REJECTION:OPEN
   s" `constant` would take one cell of `" REJECTION:EMIT-STR  ty REND-TYPE
   s" `, a value held in more than one cell" REJECTION:EMIT-STR
   a u REJECTION:AT  s" takes one cell" REJECTION:EMIT-STR
   REJECTION:HELP
   s" define a word that leaves the whole value, `: " REJECTION:EMIT-STR
   w v REJECTION:EMIT-STR  s"  ( -- " REJECTION:EMIT-STR  ty REND-TYPE
   s"  ) ... ;`, in place of the constant" REJECTION:EMIT-STR
   EXIT-STATUS:REJECTED REJECTION:STOP ;

\ `constant`, spelled a u, would take the cell it names w v from an empty stack.
: TOP-EMPTY-TAKE-REJECT ( str str -- ) {: a:^ u:n w:^ v:n :}
   s" E-INPUT-UNDERFLOW" REJECTION:OPEN
   s" `constant` takes the value `" REJECTION:EMIT-STR  w v REJECTION:EMIT-STR
   s" ` names, and the stack is empty" REJECTION:EMIT-STR
   a u REJECTION:AT  s" takes one cell" REJECTION:EMIT-STR
   REJECTION:HELP
   s" push the value before `constant`, as `0 constant " REJECTION:EMIT-STR
   w v REJECTION:EMIT-STR  s" `" REJECTION:EMIT-STR
   EXIT-STATUS:REJECTED REJECTION:STOP ;

\ Then the keyword's own row judges the cell it takes.
: TOP-TAKE-STEP ( str str -- ) {: a:^ u:n w:^ v:n :}
   DCUR @ ROW-CELLS 0= IF a u w v TOP-EMPTY-TAKE-REJECT THEN
   DCUR @ 1 TOP-WIDE-IN-K? IF a u w v TOP-CONSTANT-REJECT THEN
   TOP-WORD-STEP ;

\ A keyword takes the top cell with no word event, so the row is not saved:
\ the next event fits it to the live depth.
: CHECKER-TOP-TAKE ( str str n -- )
   {: a:^ u:n w:^ v:n live:n :}
   a TOK-SPELL-A !  u TOK-SPELL-U !
   [: a u w v TOP-TAKE-STEP ;] live TOP-STEP-LOAD
   TOP-RELEASE ;

variable TOP-MOVE-N

: TOP-OPEN-TOP? ( -- bool )
   DCUR @ R-RES dup TAG S-PUSH <> IF drop RES-FALSE EXIT THEN
   P>TYPE TOP-OPEN-TYPE? ;

\ Type t as written, but each open argument its width reads is `n`, one cell.
: REND-CELL-ARGS ( n -- ) {: t:n :}
   t T-UNSTALE {: p:n :}
   p PARAM-HEAD
   p PARAM>ARGC 0= IF EXIT THEN
   60 REJECTION:EMIT1
   0 BEGIN dup p PARAM>ARGC < WHILE
      dup 0 > IF 44 REJECTION:EMIT1 THEN
      p over PARAM>ARG T-UNSTALE {: arg:n :}
      p PARAM>FAM over TFAM-WIDTH-SLOT-XT 0= IF arg REND-TYPE ELSE
      arg ISVAR IF s" n" REJECTION:EMIT-STR ELSE
      arg LAYOUT-PARAM? IF arg RECURSE ELSE
      arg REND-TYPE THEN THEN THEN
      1 +
   REPEAT drop
   62 REJECTION:EMIT1 ;

\ `drop`, spelled a u, over an item whose cell count is not known here.
: TOP-OPEN-DROP-REJECT ( str -- ) {: a:^ u:n :}
   DCUR @ R-RES P>TYPE {: ty:n :}
   SEEN-RESET RATOM-RESET 0 NLET !
   s" E-LAYOUT-VALUE" REJECTION:OPEN
   s" `drop` cannot drop `" REJECTION:EMIT-STR  ty REND-TYPE
   s" `, a value whose cell count is not known here" REJECTION:EMIT-STR
   a u REJECTION:AT  s" cell count not known" REJECTION:EMIT-STR
   REJECTION:HELP
   s" leave the value from a word whose effect gives `" REJECTION:EMIT-STR
   ty REND-TYPE  s" ` concrete type arguments, as `( -- " REJECTION:EMIT-STR
   ty REND-CELL-ARGS  s"  )`" REJECTION:EMIT-STR
   EXIT-STATUS:REJECTED REJECTION:STOP ;

\ The program's spelling of the step's token is a u, in any case.
: TOP-SPELLED? ( str -- bool )
   TOK-SPELL-A @ TOK-SPELL-U @ CORE-STR=CI ;

\ The cells of the value the engine `drop`, `@` or `!` moves, from the row's
\ cells before and after its step: `@` leaves the value for its address, `!`
\ takes the value and its address, `drop` takes the value.
: TOP-MOVED ( n n -- n ) {: pre:n post:n :}
   s" @" TOP-SPELLED? IF post pre - 1 + EXIT THEN
   s" !" TOP-SPELLED? IF pre post - 1 - EXIT THEN
   pre post - ;

\ An item of open width has no cell count to drop, so it is rejected at
\ `drop`; otherwise a body's step judges the word. A rejection is placed at the
\ source token the engine read last, the word as the program spells it.
: TOP-MOVE-STEP ( -- )
   s" drop" TOP-SPELLED? TOP-OPEN-TOP? and IF
      TOK-SPELL-A @ TOK-SPELL-U @ TOP-OPEN-DROP-REJECT
   THEN
   DCUR @ ROW-CELLS {: pre:n :}
   TOP-WORD-STEP
   pre DCUR @ ROW-CELLS TOP-MOVED TOP-MOVE-N ! ;

: CHECKER-TOP-MOVE ( n -- n )
   {: live:n :}
   SRCLOC:TOKEN$ TOK-SPELL-U !  TOK-SPELL-A !
   ['] TOP-MOVE-STEP live TOP-STEP-LOAD
   CHECKER-TOP-SAVE
   TOP-MOVE-N @ ;

\ ---- a literal event: pushed onto the saved row ------------------------------
\ A literal's cells have constant types, so over a saved row that fits the live
\ depth its event needs no step: it writes the literal's entries over the row's
\ top in the read buffer, where the next load reads them as it reads saved ones.
\ The cost is the change, not the row's depth. Constant types carry no
\ variables: TOP-ROW-TV and TOP-ROW-RV stand.

\ A zeroed node of tag at the read buffer's used end, by offset: the buffer
\ grows when full, and an offset survives the move.
: TOP-READ-NODE ( n -- n ) {: tag:n :}
   TOP-READ-P TOP-READ-CAP  TOP-READ-END @ EFF-NODE + CELL + 1 - CELL /  1 ARENA-ROWS-ENSURE
   TOP-READ-END @ {: off:n :}
   tag off TOP-READ-AT E-NODE-INIT
   off EFF-NODE + TOP-READ-END !
   off ;

: TOP-READ-CON ( n -- n ) {: con:n :}
   EN-CON TOP-READ-NODE {: off:n :}
   con off TOP-READ-AT EN.A !  off ;

: TOP-READ-PTR-U8 ( -- n )
   CC-U8 TOP-READ-CON {: inner:n :}
   EN-PTR TOP-READ-NODE {: off:n :}
   inner off TOP-READ-AT EN.A !  off ;

\ One cell of the stored type at offset ty over the row's top; its width is held
\ as width+1, as E-COPY* holds it.
: TOP-READ-PUSH ( n -- ) {: ty:n :}
   EN-PUSH TOP-READ-NODE {: off:n :}
   ty off TOP-READ-AT EN.A !
   TOP-ROW-OFF @ off TOP-READ-AT EN.B !
   2 off TOP-READ-AT EN.C !
   off TOP-ROW-OFF !
   TOP-ROW-CELLS @ 1 + TOP-ROW-CELLS ! ;

\ Entries leave from the top while the row has more cells than live, as
\ CHECKER-TOP-LOAD drops them.
: TOP-READ-FIT ( n -- ) {: live:n :}
   BEGIN TOP-ROW-CELLS @ live > WHILE
      TOP-ROW-CELLS @ TOP-ROW-OFF @ TOP-PUSH-CELLS - TOP-ROW-CELLS !
      TOP-ROW-OFF @ TOP-READ-AT EN.B @ TOP-ROW-OFF !
   REPEAT ;

\ A literal of class cls: STR pushes ^u8 and n, CSTR ^u8, NUM n or r,
\ CHAR n; an integer NUM's entry keeps its value.
: TOP-READ-LIT ( str n -- ) {: a:^ u:n cls:n :}
   cls TOP-EV-NUM = IF
      a u FLOAT-LITERAL-ADMITTED? nip IF CC-R ELSE CC-N THEN TOP-READ-CON TOP-READ-PUSH
      a u INTEGER-LITERAL-ADMITTED? IF
         TOP-ROW-OFF @ TOP-READ-AT EN.F !
         -1 TOP-ROW-OFF @ TOP-READ-AT EN.E !
      ELSE drop THEN EXIT
   THEN
   cls TOP-EV-STR = cls TOP-EV-CSTR = or IF TOP-READ-PTR-U8 TOP-READ-PUSH THEN
   cls TOP-EV-CSTR <> IF CC-N TOP-READ-CON TOP-READ-PUSH THEN ;

: TOP-LIT-STEP ( str n -- ) {: a:^ u:n cls:n :}
   cls TOP-EV-NUM = IF
      a u FLOAT-LITERAL-ADMITTED? nip IF PE-R ELSE PE-N THEN TOP-PUSH
      a u INTEGER-LITERAL-ADMITTED? IF
         CP-LIT swap 0 0 CP-NEW DCUR @ P-PROOF!
      ELSE drop THEN EXIT
   THEN
   cls TOP-EV-STR = cls TOP-EV-CSTR = or IF PE-PTR-U8 TOP-PUSH THEN
   cls TOP-EV-CSTR <> IF PE-N TOP-PUSH THEN ;

\ A literal over a live stack of n cells. Only a saved row whose fit holds
\ exactly the live cells takes the literal in place: with no saved row, or one
\ the fit leaves short (it was short, or a wide entry left), the cells below it
\ are unknown, and only a load, step and save stores them as padding.
: CHECKER-TOP-PUSH ( str n n -- ) {: a:^ u:n cls:n live:n :}
   a u live TOP-LEFT-MARK
   live TOP-READ-FIT
   TOP-ROW-OFF @ 0 <>  TOP-ROW-CELLS @ live =  and IF a u cls TOP-READ-LIT EXIT THEN
   live CHECKER-TOP-LOAD  a u cls TOP-LIT-STEP  CHECKER-TOP-SAVE ;

\ ---- the cells a file leaves: its segment's row, read once it ends -----------
\ The segment of the file on base: the oldest there, since text a word of the
\ file evaluated on its base opened newer ones, dead once it returned. It goes
\ live, dropping the dead ones above it.
: TOP-SEG-OF ( n -- bool ) {: base:n :}
   0 BEGIN dup TOP-SEG-N @ < WHILE
      dup TS.BASE TS-AT @ base = IF TOP-SEG-UNPARK RES-TRUE EXIT THEN
      1 +
   REPEAT drop
   TOP-LIVE @ 0 <>  TOP-BASE @ base =  and ;

: FILE-SEG? ( -- bool )   data-base STACK-ABI:BASE-CELL + @ TOP-SEG-OF ;

\ The token whose cells lie at the bottom of what the running file left, 0 0
\ when its row saw none.
: RESIDUE-AT ( -- str )
   FILE-SEG? 0= IF NULL-PTR 0 EXIT THEN
   TOP-LEFT-A @ TOP-LEFT-U @ ;

\ Note `left`, the row's types bottom first, when the row holds the n cells
\ the file left; a row that lost cells cannot name them, so nothing.
: RESIDUE-NOTE ( n -- ) {: n:n :}
   FILE-SEG? 0= IF EXIT THEN
   TOP-ROW-CELLS @ n <> IF EXIT THEN
   s" left" REJECTION:NOTE
   n CHECKER-TOP-LOAD
   0 RQM !  SEEN-RESET  0 NLET !  0 RROW-N !
   DCUR @ ROW-NOTE  TOP-RELEASE ;

;package
package CHECKER-REG
EXPORT CHECK:CHECKER-CREATES-SYM?
EXPORT CHECK:CHECKER-CTOR-DECL-AT!
EXPORT CHECK:CHECKER-CURSOR!
EXPORT CHECK:CHECKER-DECL-AT!
EXPORT CHECK:CHECKER-DECL-AT-OFF
EXPORT CHECK:CHECKER-FIND-QUIET-SYM
EXPORT CHECK:CHECKER-RECORD-CREATED
EXPORT CHECK:CHECKER-RECORD-SYM?
EXPORT CHECK:CHECKER-RETRACT-ROWS
EXPORT CHECK:CHECKER-ROWS-END
EXPORT CHECK:CHECKER-SOURCE-CLAUSE
EXPORT CHECK:CHECKER-SYM-IDENTITY
EXPORT CHECK:CHECKER-SYM-SOURCE
EXPORT CHECK:CHECKER-VERIFY-DEFERRED
EXPORT CHECK:CHECKER-VERIFY-DEFERRED-BODY
EXPORT CHECK:CHECKER-VERIFY-REACH
EXPORT CHECK:CHECKER-VERIFY-RENDERS
EXPORT CHECK:CHECKER-VERIFY-TOP-BINDING
EXPORT CHECK:CHECKER-WITH-USES
EXPORT CHECK:DP-CUT
EXPORT CHECK:HELD-FACTS
EXPORT CHECK:TRUST-DECL?
EXPORT CHECK:WITH-TICK-ORDER
' DP-CUT DECLARATIONS CHECKER-OWNER-ABI:DP-CUT-OFF + xt!
;package

\ Emitted code calls REC-WIDE-PUBLISH through its owner-record field.
package CHECKER-REG
EXPORT CHECK:REC-WIDE-PUBLISH

' REC-WIDE-PUBLISH            DECLARATIONS REC-WIDE-PUBLISH-OFF + xt!
;package

package CHECKER-REG
EXPORT CHECK:CHECKER-SOURCE-DOES!
EXPORT CHECK:CHECKER-VERIFY-TOP
EXPORT CHECKER-RESOLVE:EACH-VISIBLE
' CHECKER-RECORD-SYM? DECLARATIONS CHECKER-OWNER-ABI:VERIFY-RECORD-SYM-OFF + xt!
' CHECKER-CTOR-DECL-AT! DECLARATIONS CHECKER-OWNER-ABI:VERIFY-CTOR-DECL-OFF + xt!
' CHECKER-FIND-QUIET-SYM DECLARATIONS CHECKER-OWNER-ABI:VERIFY-FIND-SYM-OFF + xt!
' CHECKER-CREATES-SYM? DECLARATIONS CHECKER-OWNER-ABI:VERIFY-CREATES-SYM-OFF + xt!
' CHECKER-RECORD-CREATED DECLARATIONS CHECKER-OWNER-ABI:VERIFY-RECORD-CREATED-OFF + xt!
' CHECKER-SOURCE-DOES! DECLARATIONS CHECKER-OWNER-ABI:VERIFY-SOURCE-DOES-OFF + xt!
' CHECKER-VERIFY-RENDERS DECLARATIONS CHECKER-OWNER-ABI:VERIFY-RENDERS-OFF + xt!
' CHECKER-VERIFY-TOP DECLARATIONS CHECKER-OWNER-ABI:VERIFY-TOP-OFF + xt!
' CHECKER-VERIFY-DEFERRED DECLARATIONS CHECKER-OWNER-ABI:VERIFY-DEFERRED-OFF + xt!
' CHECKER-VERIFY-REACH DECLARATIONS CHECKER-OWNER-ABI:VERIFY-REACH-OFF + xt!
' CHECKER-SYM-IDENTITY DECLARATIONS CHECKER-OWNER-ABI:VERIFY-SYM-IDENTITY-OFF + xt!
' CHECKER-SYM-SOURCE DECLARATIONS CHECKER-OWNER-ABI:VERIFY-SYM-SOURCE-OFF + xt!
' CHECKER-VERIFY-DEFERRED-BODY DECLARATIONS CHECKER-OWNER-ABI:VERIFY-DEFERRED-BODY-OFF + xt!
' CHECKER-DECL-AT! DECLARATIONS CHECKER-OWNER-ABI:VERIFY-DECL-ARM-OFF + xt!
' CHECKER-DECL-AT-OFF DECLARATIONS CHECKER-OWNER-ABI:VERIFY-DECL-DISARM-OFF + xt!
' CHECKER-WITH-USES DECLARATIONS CHECKER-OWNER-ABI:VERIFY-USES-OFF + xt!
' CHECKER-CURSOR! DECLARATIONS CHECKER-OWNER-ABI:VERIFY-CURSOR-OFF + xt!
' EACH-VISIBLE DECLARATIONS CHECKER-OWNER-ABI:VERIFY-EACH-VISIBLE-OFF + xt!
' WITH-TICK-ORDER DECLARATIONS CHECKER-OWNER-ABI:WITH-TICK-ORDER-OFF + xt!
' TRUST-DECL? DECLARATIONS CHECKER-OWNER-ABI:VERIFY-DECL-OFF + xt!
' CHECKER-RETRACT-ROWS DECLARATIONS CHECKER-OWNER-ABI:RETRACT-ROWS-OFF + xt!
' CHECKER-ROWS-END DECLARATIONS CHECKER-OWNER-ABI:ROWS-END-OFF + xt!
' HELD-FACTS DECLARATIONS CHECKER-OWNER-ABI:HELD-END-OFF + xt!
' CHECKER-SOURCE-CLAUSE DECLARATIONS CHECKER-OWNER-ABI:VERIFY-SOURCE-CLAUSE-OFF + xt!
' CHECKER-VERIFY-TOP-BINDING DECLARATIONS CHECKER-OWNER-ABI:VERIFY-TOP-BINDING-OFF + xt!

\ A checker claims the source only where nothing owns it, after every callback
\ is installed.
: CLAIM-COLD-SOURCE ( -- )
   SOURCE-OWNER-A @ 0= if
      DECLARATIONS SOURCE-OWNER-A !
   then ;
CLAIM-COLD-SOURCE
;package
package CHECK

\ ---- the facts reader ---------------------------------------------------------
\ A record's facts text (CHECK:FACTS@) is read into the stores the first time a
\ lookup binds the record (LIVE-BIND), and an export's when the linker links it.

\ Per wordlist id, one plus the index of the newest namespace record whose public
\ or private wordlist it is, 0 for none, over the records below NS-SEEN. A lookup
\ first indexes the records made since. When the count went down it drops the rows
\ naming a record at or above it, which can empty a row an older record holds; it
\ indexes from 0 again when the answer is missing or no longer holds, as after a
\ package gains its private wordlist. The cells rest in a scratch span.
SCRATCH{
PTR-VARIABLE NS-ROWS-P   variable NS-CAP   variable NS-SEEN
}SCRATCH

: NS-ENSURE ( n -- ) {: need:n :}
   need NS-CAP @ <= IF EXIT THEN
   need NS-CAP @ 2 * max 64 max {: nc:n :}
   NS-ROWS-P @ NS-CAP @ cells nc cells SR-GROW NS-ROWS-P !
   nc NS-CAP ! ;

: NS-PUT ( n n -- ) {: wid:n i:n :}
   wid 0 <= IF EXIT THEN
   wid 1 + NS-ENSURE
   i 1 +  NS-ROWS-P @ wid cells + ! ;

: NS-NOTE ( n -- ) {: i:n :}
   i REC-AT {: ns:^ :}
   ns CK-REC-WID BOOT:NAMESPACE-WL <> IF EXIT THEN
   ns @ i NS-PUT  ns CELL + @ i NS-PUT ;

: NS-CLEAR ( -- ) NS-ROWS-P @ 0 NS-CAP @ ARENA-CELLS-ZERO  0 NS-SEEN ! ;

: NS-UNNAME ( ^n n -- )
   {: row:^ top:n :}
   row @ top > IF 0 row ! THEN ;

: NS-DROP ( n -- )
   {: top:n :}
   NS-CAP @ 0 ?DO NS-ROWS-P @ i cells + top NS-UNNAME LOOP
   top NS-SEEN ! ;

: NS-INDEX ( -- )
   ndict@ NS-SEEN @ < IF ndict@ NS-DROP THEN
   ndict@ NS-SEEN @ ?DO i NS-NOTE LOOP
   ndict@ NS-SEEN ! ;

: NS-ROW@ ( n -- n ) {: wid:n :}
   wid 0 >  wid NS-CAP @ <  and IF NS-ROWS-P @ wid cells + @ ELSE 0 THEN ;

: NS-HOLDS? ( n n -- bool ) {: wid:n row:n :}
   row 0= IF RES-FALSE EXIT THEN
   row 1 - REC-AT {: ns:^ :}
   ns CK-REC-WID BOOT:NAMESPACE-WL =  wid ns @ =  wid ns CELL + @ =  or  and ;

\ The global wordlists and any wid at or below 0 are no package's: a miss on
\ them would index every record again. The namespace marker is negative, and so
\ is the WL:RETIRED of a retired record the linker's NAME-PKG asks about.
: NS-FIND ( n -- n ) {: wid:n :}
   wid 0 <=  wid WL:GLOBAL?  or IF 0 EXIT THEN
   NS-INDEX
   wid  wid NS-ROW@  NS-HOLDS? 0= IF NS-CLEAR NS-INDEX THEN
   wid NS-ROW@ {: row:n :}
   wid row NS-HOLDS? IF row ELSE 0 THEN ;

: REC-SYM ( ^n -- n ) {: rec:^ :}
   rec CK-REC-NAME$ {: name:^ nameu:n :}
   rec CK-REC-WID {: wid:n :}
   nameu 0= wid -2 = or IF 0 EXIT THEN
   wid WL:GLOBAL? IF name nameu CHECKER-GLOBAL-SYM EXIT THEN
   wid NS-FIND {: row:n :}
   row 0= IF 0 EXIT THEN
   row 1 - REC-AT {: ns:^ :}
   ns CK-REC-NAME$
   wid ns @ = IF SYM-PUBLIC ELSE SYM-PRIVATE THEN
   name nameu CHECKER-PKG-SYM ;

\ A read runs inside a caller's check, between its tokens, so it is a guest
\ there, as a top step is (TOP-GUEST): it parses above the term pools' marks,
\ and the parser's and recorder's cells, which the caller may hold, are saved
\ and given back however the read ends. One read is open at a time.
\ GUEST-POOLS saves 16 cells, GUEST-PARSER 41 and its two tables,
\ GUEST-LATCHES 8; GUEST-POINTERS saves 10 pointers.
16 41 + 8 +  BINDER-DEPTH-MAX +  26 +  constant GUEST-CAP
10 constant GUEST-PTRS-CAP
SCRATCH{
create GUEST-SAVED GUEST-CAP cells allot
GUEST-PTRS-CAP PTR-U8-TABLE GUEST-SAVED-PTRS
variable GUEST-OPEN
PTR-U8-TABLE-VARIABLE GUEST-FAM-A-P
PTR-VARIABLE GUEST-FAM-U-P   variable GUEST-FAM-CAP
PTR-VARIABLE GUEST-NMAP-P   variable GUEST-NMAP-CAP
}SCRATCH

: GUEST-CELLS ( n ^n n bool -- n )
   {: k:n p:^ cnt:n save:bool :}
   k cnt + GUEST-CAP > IF s" checker: guest cells full" EXIT-STATUS:INTERNAL die THEN
   cnt 0 ?DO
      save IF i cells p + @  k i + cells GUEST-SAVED + !
      ELSE k i + cells GUEST-SAVED + @  i cells p + ! THEN
   LOOP
   k cnt + ;

: GUEST-CELL ( n ^n bool -- n ) {: p:^ save:bool :} p 1 save GUEST-CELLS ;

: GUEST-PTRS ( n ^^u8 n bool -- n )
   {: k:n p:^ cnt:n save:bool :}
   k cnt + GUEST-PTRS-CAP > IF s" checker: guest pointers full" EXIT-STATUS:INTERNAL die THEN
   cnt 0 ?DO
      save IF i cells p + 0 ptr-field @  k i + cells GUEST-SAVED-PTRS + 0 ptr-field !
      ELSE k i + cells GUEST-SAVED-PTRS + 0 ptr-field @  i cells p + 0 ptr-field ! THEN
   LOOP
   k cnt + ;

\ The term pools' counters, FV first: READ-LEAVE clears the variables above it.
: GUEST-POOLS ( n bool -- n ) {: save:bool :}
   FV save GUEST-CELL  SPN save GUEST-CELL  QEN save GUEST-CELL  PTRN save GUEST-CELL
   STLN save GUEST-CELL  ATOMN save GUEST-CELL  TERM-NAME-N save GUEST-CELL
   PARAMN save GUEST-CELL  PARAM-SCR-N save GUEST-CELL  PARG-N save GUEST-CELL
   RIGID-N save GUEST-CELL  RGN-N save GUEST-CELL  EXT-N save GUEST-CELL
   GEN-N save GUEST-CELL  RGN-MARK save GUEST-CELL  FB-N save GUEST-CELL ;

: GUEST-PARSER ( n bool -- n ) {: save:bool :}
   PD-IN save GUEST-CELL  PR-IN save GUEST-CELL  PD-OUT save GUEST-CELL  PR-OUT save GUEST-CELL
   PD-BASE save GUEST-CELL  PD-ATOM0 save GUEST-CELL  SGHASR save GUEST-CELL
   RR-SHARED save GUEST-CELL  SL save GUEST-CELL  SI save GUEST-CELL  PKU save GUEST-CELL
   PKHAVE save GUEST-CELL  BIND-DEPTH save GUEST-CELL
   BIND-DOMAINS BINDER-DEPTH-MAX save GUEST-CELLS
   ROWMAP 26 save GUEST-CELLS
   FAM-N save GUEST-CELL  FAM-I save GUEST-CELL  FAM-K save GUEST-CELL
   NRES save GUEST-CELL  NDI save GUEST-CELL  NDH save GUEST-CELL
   SGBAD save GUEST-CELL  SGBAD-U save GUEST-CELL  SGBAD-KIND save GUEST-CELL
   SGBAD-AR-DECL save GUEST-CELL  SGBAD-AR-GOT save GUEST-CELL
   SGBAD-EXP-U save GUEST-CELL  SGBAD-USED save GUEST-CELL  SGBAD-LOCAL save GUEST-CELL
   SIG-UNRES save GUEST-CELL  SIG-UNRES-U save GUEST-CELL
   SIG-RAW-MODE save GUEST-CELL  SIG-FACTS-MODE save GUEST-CELL  SIG-CTL save GUEST-CELL
   SIG-NONZERO save GUEST-CELL  SIG-TAGS-ONLY save GUEST-CELL
   SIG-PE save GUEST-CELL  SIG-CREATES-U save GUEST-CELL
   SIG-PARSES save GUEST-CELL  SIG-PARSES-U save GUEST-CELL
   SIG-OVERLOAD-U save GUEST-CELL  SIGSCOPE-ON save GUEST-CELL  SIGSCOPE-U save GUEST-CELL ;

\ The latches E-ADD-EFFECT sets for the record the engine publishes next.
: GUEST-LATCHES ( n bool -- n ) {: save:bool :}
   DL-NEW save GUEST-CELL  RECW save GUEST-CELL
   CHECKER-REC-SYM save GUEST-CELL
   DOESEFF save GUEST-CELL  DOESPEND save GUEST-CELL
   WRAPN save GUEST-CELL  WRAPC save GUEST-CELL  WRAPBENT save GUEST-CELL ;

: GUEST-POINTERS ( bool -- ) {: save:bool :}
   0  SB 1 save GUEST-PTRS  SS 1 save GUEST-PTRS  PKA 1 save GUEST-PTRS
   SGBAD-A 1 save GUEST-PTRS  SGBAD-EXP-A 1 save GUEST-PTRS  SIG-UNRES-A 1 save GUEST-PTRS
   SIG-CREATES-A 1 save GUEST-PTRS  SIG-OVERLOAD-A 1 save GUEST-PTRS
   SIG-PARSES-A 1 save GUEST-PTRS
   SIGSCOPE-P 1 save GUEST-PTRS  drop ;

\ A read parses its fresh names into a table of its own, which this swaps
\ with the caller's on entry and back on leaving.
: GUEST-FAMS ( -- )
   FAM-A-P @  GUEST-FAM-A-P @ FAM-A-P !  GUEST-FAM-A-P !
   FAM-U-P @  GUEST-FAM-U-P @ FAM-U-P !  GUEST-FAM-U-P !
   FAM-CAP @  GUEST-FAM-CAP @ FAM-CAP !  GUEST-FAM-CAP ! ;

\ The same for its type-variable names.
: GUEST-NMAP ( -- )
   NMAP-P @  GUEST-NMAP-P @ NMAP-P !  GUEST-NMAP-P !
   NMAP-CAP @  GUEST-NMAP-CAP @ NMAP-CAP !  GUEST-NMAP-CAP ! ;

: GUEST-KEEP ( bool -- ) {: save:bool :}
   0 save GUEST-POOLS save GUEST-PARSER save GUEST-LATCHES drop
   save GUEST-POINTERS  GUEST-FAMS  GUEST-NMAP ;

: READ-ENTER ( -- )
   GUEST-OPEN @ IF s" checker: a facts read inside another" EXIT-STATUS:INTERNAL die THEN
   RES-TRUE GUEST-KEEP  -1 GUEST-OPEN !
   DOES-EFF-CLEAR  WRAP-CLEAR
   0 SIG-RAW-MODE ! ;

\ A read's records are what an earlier file's compile taught, not the host
\ check's scratch: the drop of the host's signature cache (SIG-EFF-DROP) stops
\ above them, and the cache record stays, unnamed.
: READ-LEAVE ( -- )
   GUEST-SAVED @ TV-CLEAR-FROM
   RES-FALSE GUEST-KEEP  0 GUEST-OPEN !
   RECEFF-ON? IF UEND @ RECEFF-UEND ! THEN ;

\ Facts text a u parsed; false when the parse refuses it, names a type no
\ package declares here, or no record holds it. Tags alone answer no rows
\ (SIG-TAGS-ONLY).
: READ-ROWS ( str -- n n n n bool )
   {: a:^ u:n :}
   SGBAD-CLEAR
   a u PARSE-SIG-RAW {: din:n dout:n rin:n rout:n :}
   SGBAD @ 0 <>  SIG-UNRES @ 0 <> or IF
      din dout rin rout RES-FALSE EXIT
   THEN
   SIG-TAGS-ONLY @ IF din dout rin rout RES-TRUE EXIT THEN
   a u din dout rin rout SGBAD-UNFIT!
   din dout rin rout SGBAD @ 0= ;

: READ-REFUSE ( str str -- )
   {: a:^ u:n na:^ nu:n :}
   SGBAD @ 0= IF a u SGBAD-SYNTAX! THEN
   a u na nu USIG-ADD-BAD ;

\ The created words' effect, offset+1, under symbol 0; 0 for none.
: READ-MADE ( str -- n bool )
   {: ca:^ cu:n :}
   cu 0= IF 0 RES-TRUE EXIT THEN
   ca cu READ-ROWS {: din:n dout:n rin:n rout:n ok:bool :}
   ok 0=  SIG-TAGS-ONLY @ or IF 0 RES-FALSE EXIT THEN
   0 CHECKER-REC-SYM !
   din dout rin rout SGHASR @ E-BUILD-EFFECT 1 + RES-TRUE ;

\ A primitive row of sym as the table states one, its record under symbol 0, so a
\ call tries every row (TRY-PRIMS).
: READ-ROW ( n n n n bool n n -- )
   {: din:n dout:n rin:n rout:n hasr:bool sym:n pe:n :}
   0 CHECKER-REC-SYM !
   sym  din dout rin rout hasr E-BUILD-EFFECT  pe PE-ACTIVE or  PRIM-ADD
   HIDX-VALID @ IF  HIDX-EPOCH @ 1- sym HT-PRM-E HIDX-CELL !  THEN ;

\ The first row of sym's text: a primitive row when the text has primitive tags
\ or rows after it, else the word's external declaration. A prefix symbol's
\ rows are the build's prefix's. The caller's PE-LAST stays: a read is its
\ guest.
: READ-FIRST ( n n n n bool n n bool -- )
   {: din:n dout:n rin:n rout:n hasr:bool sym:n pe:n more:bool :}
   pe 0=  more 0=  and  sym PREFIX-SYM? or IF
      sym CHECKER-REC-SYM !
      din dout rin rout hasr RES-TRUE E-ADD-EFFECT EXIT
   THEN
   PE-LAST @ {: last:n :}
   din dout rin rout hasr sym pe READ-ROW
   last PE-LAST ! ;

\ One more primitive row of sym from the text after `overload`: its effect and
\ PE tags. Answers the text of the row after it; false when the text is
\ refused or carries a tag only a first row may.
: READ-OVERLOAD ( str n -- str bool )
   {: a:^ u:n sym:n :}
   a u READ-ROWS {: din:n dout:n rin:n rout:n ok:bool :}
   SIG-PE @  SGHASR @  SIG-OVERLOAD-A @  SIG-OVERLOAD-U @ {: pe:n hasr:bool ra:^ ru:n :}
   ok 0=  SIG-TAGS-ONLY @ or  SIG-CTL @ 0 <> or
   SIG-PARSES @ 0 >= or  SIG-CREATES-U @ 0 <> or IF
      NULL-PTR 0 RES-FALSE EXIT
   THEN
   din dout rin rout hasr sym pe READ-ROW
   ra ru RES-TRUE ;

: READ-OVERLOADS ( str n -- bool )
   {: a:^ u:n sym:n :}
   sym PREFIX-SYM? IF RES-TRUE EXIT THEN
   PE-LAST @ {: last:n :}
   a u BEGIN dup 0 <> WHILE
      sym READ-OVERLOAD 0= IF 2drop  last PE-LAST !  RES-FALSE EXIT THEN
   REPEAT 2drop
   last PE-LAST !  RES-TRUE ;

\ A text's control bits beside the ones storing an effect derives from its rows.
: READ-FLAGS ( n n -- n )
   {: sym:n ctl:n :}
   sym CTL-FLAGS-SYM  CTL-BARRIER EFFECT-EXTERNAL or  and
   ctl XFER-FLAGS CHECKER-OWNER-ABI:BINDING-DEFER invert and  or ;

\ Links record ix, sym's, with sym's newest effect and control entry. With no
\ effect, as for a text of tags alone or of primitive rows, the link names
\ none: LINKED? answers it read, RECORD-LINK-CURRENT? does not, and the row
\ keeps mark, the effect store's end when the read began, for a rewind to clear
\ it by (RLINK.READ).
: READ-LINK ( n n n -- ) {: ix:n sym:n mark:n :}
   sym USIG-NEWEST 0 <> IF
      ix sym USIG-NEWEST sym NORET-NEWEST RECORD-LINK-SET
      EXIT
   THEN
   ix RECORD-LINK-OCC {: slot:n occurrence:n :}
   slot ix <> IF EXIT THEN
   ix 0 sym NORET-NEWEST occurrence RECORD-LINK-WRITE
   ix mark 1 + RECORD-LINK-READ! ;

: READ-TAGS ( n n n n n -- ) {: ix:n sym:n ctl:n nz:n mark:n :}
   sym sym ctl READ-FLAGS ctl XFER-DMASK ctl XFER-RMASK 0 nz NORET-ADD-CREATES
   ix sym mark READ-LINK ;

: READ-BODY ( str n -- )
   {: a:^ u:n ix:n :}
   UEND @ {: mark:n :}
   ix REC-AT {: rec:^ :}
   rec CK-REC-NAME$ {: name:^ nameu:n :}
   rec REC-SYM {: sym:n :}
   sym 0= IF a u name nameu READ-REFUSE EXIT THEN
   sym SYM-PKG$ SIGSCOPE!
   RES-TRUE SIG-FACTS!
   a u READ-ROWS {: din:n dout:n rin:n rout:n ok:bool :}
   ok 0= IF a u name nameu READ-REFUSE EXIT THEN
   SIG-CTL @  SIG-PE @  SIG-CREATES-A @  SIG-CREATES-U @  SGHASR @
      {: ctl:n pe:n ca:^ cu:n hasr:bool :}
   SIG-OVERLOAD-A @  SIG-OVERLOAD-U @  SIG-NONZERO @ {: oa:^ ou:n nz:n :}
   SIG-PARSES @  SIG-PARSES-A @  SIG-PARSES-U @ {: pn:n pa:^ pu:n :}
   SIG-TAGS-ONLY @ IF
      pe 0 <>  pn 0 >= or  cu 0 <> or  ou 0 <> or
      ctl XFER-FLAGS CHECKER-OWNER-ABI:BINDING-DEFER and 0 <> or IF
         a u name nameu READ-REFUSE EXIT
      THEN
      ix sym ctl nz mark READ-TAGS EXIT
   THEN
   din dout rin rout hasr sym pe ou 0 <> READ-FIRST
   oa ou sym READ-OVERLOADS 0= IF a u name nameu READ-REFUSE EXIT THEN
   ca cu READ-MADE {: made:n good:bool :}
   good 0= IF a u name nameu READ-REFUSE EXIT THEN
   sym sym ctl READ-FLAGS ctl XFER-DMASK ctl XFER-RMASK made nz
   NORET-ADD-CREATES
   ctl XFER-FLAGS CHECKER-OWNER-ABI:BINDING-DEFER and 0 <> IF sym RES-TRUE DFER-ADD-SYM THEN
   pn 0 >= IF sym pn pa pu PROW-ADD-TEXT THEN
   ix sym mark READ-LINK ;

\ Whether record ix has a link row: its compile, or a read, taught the checker.
: LINKED? ( n -- bool ) {: ix:n :}
   ix RECORD-LINK-CAP @ < 0= IF RES-FALSE EXIT THEN
   ix RECORD-LINK-CELL RLINK.OCC cells + @ 0 <> ;

\ What a compile of the file that made record ix taught the checker, read back
\ from the record's facts text, so a later file's compile can call it.
: READ-IX ( n -- ) {: ix:n :}
   ix FACTS@ ix  READ-ENTER  [: READ-BODY ;] [: READ-LEAVE ;] finally ;

\ EXPORT-META-COPY for symbols, which a link has where a compile has a spelling.
: EXPORT-META-SYM ( n n -- ) {: sym:n src:n :}
   src DFER-FIND-SYM IF sym RES-TRUE DFER-ADD-SYM THEN
   src CTL-FLAGS-SYM ALIAS-CTL {: ctl:n :}
   src CTL-MASKS-SYM {: dm:n rm:n :}
   src NORET-CREATES@ {: creates:n :}
   sym ctl dm rm creates src CHECKER-SYM-SOURCE src NORET-NONZERO@ NORET-APPEND ;

\ An export's facts are its source's, as CHECKER-EXPORT-ROW records them.
: ALIAS-BODY ( n n -- )
   {: ix:n src:n :}
   ix REC-AT REC-SYM {: sym:n :}
   src REC-AT REC-SYM {: from:n :}
   sym 0=  from 0= or IF
      s" checker: an export or its source has no checked symbol" EXIT-STATUS:INTERNAL die
   THEN
   from USIG-NEWEST {: eff:n :}
   eff 0= IF s" checker: an export's source has no checked effect" EXIT-STATUS:INTERNAL die THEN
   eff 1 - E-PTR EXPORT-EFF-INST {: din:n dout:n rin:n rout:n hasr:bool :}
   sym CHECKER-REC-SYM !
   din dout rin rout hasr from EFFECT-EXTERNAL-SYM? E-ADD-EFFECT
   sym from EXPORT-META-SYM
   ix sym USIG-NEWEST sym NORET-NEWEST RECORD-LINK-SET ;

\ A record `undefine` retired has no symbol.
: HAS-SYM? ( n -- bool ) REC-AT REC-SYM 0 <> ;

\ Export record ix of record src, once src's own facts are read. An export
\ keeps the facts of a source retired after it, so it reads src's text as its own.
: ALIAS-READ ( n n -- ) {: ix:n src:n :}
   src HAS-SYM? 0= IF
      src FACTS@ ix  READ-ENTER  [: READ-BODY ;] [: READ-LEAVE ;] finally  EXIT
   THEN
   ix src  READ-ENTER  [: ALIAS-BODY ;] [: READ-LEAVE ;] finally ;

\ Reads rec's facts text, or an image's export's source's, when there is one and no link,
\ except hb's own record of a primitive or of a prefix symbol in a build: the
\ build's kernel object states the primitives the target has, and the prefix
\ holds its rows.
: READ-BOUND ( ^n -- ) {: rec:^ :}
   rec CK-REC-FACTS-SLOT cells + @ {: off:n :}
   off 0= IF EXIT THEN
   rec NULL-PTR - dbase@ - DREC / {: ix:n :}
   BUILDING? ix BUILD-FIRST @ < and IF
      rec CK-REC-WID WL:PRIMITIVES = IF EXIT THEN
      rec REC-SYM PREFIX-SYM? IF EXIT THEN
   THEN
   ix LINKED? IF EXIT THEN
   off 0 > IF ix READ-IX EXIT THEN
   -1 off - {: src:n :}
   src 0 <  src ix >=  or IF
      s" checker: an export's source is not an earlier record" EXIT-STATUS:INTERNAL die
   THEN
   src HAS-SYM? IF src REC-AT recurse THEN
   ix src ALIAS-READ ;

: FACTS-READ-INSTALL ( -- ) [: READ-BOUND ;] is FACTS-READ-XT ;
FACTS-READ-INSTALL

: WORDLIST-PACKAGE ( n -- n bool )
   NS-FIND {: row:n :}
   row 0= IF 0 RES-FALSE EXIT THEN
   row 1 - RES-TRUE ;

\ Export record ix of record src, as a compile of its file recorded it: an
\ export the file retired has no facts, since its `undefine` removed them.
: READ-ALIAS ( n n -- ) {: ix:n src:n :}
   ix HAS-SYM? 0= IF EXIT THEN
   src HAS-SYM? IF src REC-AT READ-BOUND THEN
   ix src ALIAS-READ ;

\ Reads text a u as record ix's facts now, over what a compile or a read taught:
\ an earlier file's record whose text a later file's amender rewrote.
: READ-FACTS ( str n -- )
   {: a:^ u:n ix:n :}
   u 0= IF EXIT THEN
   a u ix  READ-ENTER  [: READ-BODY ;] [: READ-LEAVE ;] finally ;

\ The kernel object's rows, stated before a build's first file as READ-ROW
\ states one: never a user record, which answers alone.

\ A row with no effect: its symbol alone.
: PRIM-NAME ( str -- ) PE-SYM-OF drop ;

\ Opens a row with an effect; PRIM-ATOM runs each of its atoms.
: PRIM-OPEN ( ^u8 n -- ) PE-OPEN  0 PE-SPEC-TOS ! ;

: PRIM-ATOM ( n -- ) PE-SPEC-ATOM ;

\ Closes the open row on pkg's private symbol, or the global one for an empty
\ pkg, with its flags; a symbol the prefix holds, a grammar word's, states
\ nothing.
: PRIM-CLOSE ( ^u8 n bool bool -- )
   {: pkg:^ pkgu:n trusted:bool fixed:bool :}
   PE-SPEC-TOS @ 0 <> IF s" checker: primitive-spec operand left over" EXIT-STATUS:INTERNAL die THEN
   pkgu 0= IF  PE-NA@ PE-NU @ PE-SYM-OF
   ELSE  pkg pkgu SYM-PRIVATE PE-NA@ PE-NU @ SYM-INTERN  THEN {: sym:n :}
   sym BUILD-SYM-END @ < IF  -1 PE-LAST !  EXIT  THEN
   PE-DIN @ PE-DOUT @ PE-RIN @ PE-ROUT @ PE-HASR @ sym 0 READ-ROW
   trusted IF PRIM-TRUSTED-ONLY! THEN
   fixed IF PRIM-FIXED-ABI! THEN ;

\ The stated symbols join the prefix; the primitives' axioms follow
\ (NORET-AXIOM names prefix symbols only).
: PRIMS-END ( -- )
   SYM-N @ BUILD-SYM-END !
   NORET-AXIOMS ;
;package
