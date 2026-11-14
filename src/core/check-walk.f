\ check-walk.f - the checker's walk over a definition, which calls the type registry
\ and the diagnostics that load before it.

: EV-VARIANT ( n n n -- )
   {: kind:n fam:n vid:n :}
   kind  vid TFAM:SUMV-TAG@  fam vid TFAM:TFL-VPADS  EVENT! ;

\ WITH-INIT is the only public entry that changes a byte view's element.
\ Its dictionary code entry authenticates this transition.
: SCOPE-INIT-STEP ( -- )
   DCUR @ R-RES dup TAG S-PUSH <> IF drop 0 OK ! -1 FAILSET ! EXIT THEN
   {: callback-node:n :}
   callback-node P>TYPE T-RES {: supplied:n :}
   callback-node P>REST R-RES dup TAG S-PUSH <> IF drop 0 OK ! -1 FAILSET ! EXIT THEN
   {: value-node:n :}
   value-node P>TYPE T-RES {: value-cell:n :}
   value-cell HIDDEN-PARAM? IF value-cell MK-LOGICAL ELSE value-cell THEN {: value:n :}
   value INIT-VALUE-OK? 0= IF 0 OK ! -1 FAILSET ! EXIT THEN
   value TFAM:INIT-RECORD? 0= IF drop drop drop 0 OK ! -1 FAILSET ! EXIT THEN
   {: width:n bytes:n align:n :}
   value-node P>REST width 1- SCOPE-INIT-DROP R-RES dup TAG S-PUSH <> IF
      drop 0 OK ! -1 FAILSET ! EXIT THEN
   {: parent-node:n :}
   parent-node P>TYPE T-RES {: parent-cell:n :}
   parent-node P>REST {: rest:n :}
   parent-cell HIDDEN-PARAM? IF parent-cell MK-LOGICAL ELSE parent-cell THEN {: parent:n :}
   parent TAG T-STALE = IF parent parent STALE-WHY C2-REJECT EXIT THEN
   parent MUT-VIEW? 0= IF 0 OK ! -1 FAILSET ! EXIT THEN
   parent 3 PARAM>ARG PE-U8 UNIFY-EXACT 0= IF 0 OK ! -1 FAILSET ! EXIT THEN
   parent 1 PARAM>ARG T-RES {: ceiling:n :}
   supplied TAG T-FORALL = IF
      supplied F>DOMAIN BIND-CHILD <>
      supplied F>PARENT 0= or supplied F>SECOND 0= or IF
         0 OK ! -1 FAILSET ! EXIT THEN
      supplied F>PARENT ceiling UNIFY-EXACT 0=
      supplied F>SECOND value UNIFY-EXACT 0= or IF
         0 OK ! -1 FAILSET ! EXIT THEN
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
   DCUR @ R-RES dup TAG S-PUSH <> IF drop 0 OK ! -1 FAILSET ! EXIT THEN
   {: callback-node:n :}
   callback-node P>TYPE T-RES {: supplied:n :}
   callback-node P>REST R-RES dup TAG S-PUSH <> IF drop 0 OK ! -1 FAILSET ! EXIT THEN
   {: value-node:n :}
   value-node P>TYPE T-RES {: value-cell:n :}
   value-cell HIDDEN-PARAM? IF value-cell MK-LOGICAL ELSE value-cell THEN {: value:n :}
   value INIT-VALUE-OK? 0= IF 0 OK ! -1 FAILSET ! EXIT THEN
   value TFAM:INIT-RECORD? 0= IF drop drop drop 0 OK ! -1 FAILSET ! EXIT THEN
   {: width:n bytes:n align:n :}
   value-node P>REST width 1- SCOPE-INIT-DROP R-RES dup TAG S-PUSH <> IF
      drop 0 OK ! -1 FAILSET ! EXIT THEN
   {: count-node:n :}
   count-node P>REST R-RES dup TAG S-PUSH <> IF drop 0 OK ! -1 FAILSET ! EXIT THEN
   {: parent-node:n :}
   parent-node P>TYPE T-RES {: parent-cell:n :}
   parent-node P>REST {: rest:n :}
   parent-cell HIDDEN-PARAM? IF parent-cell MK-LOGICAL ELSE parent-cell THEN {: parent:n :}
   parent TAG T-STALE = IF parent parent STALE-WHY C2-REJECT EXIT THEN
   parent MUT-VIEW? 0= IF 0 OK ! -1 FAILSET ! EXIT THEN
   parent 3 PARAM>ARG PE-U8 UNIFY-EXACT 0= IF 0 OK ! -1 FAILSET ! EXIT THEN
   parent 1 PARAM>ARG T-RES {: ceiling:n :}
   supplied TAG T-FORALL = IF
      supplied F>DOMAIN BIND-CHILD <>
      supplied F>PARENT 0= or supplied F>SECOND 0= or IF
         0 OK ! -1 FAILSET ! EXIT THEN
      supplied F>PARENT ceiling UNIFY-EXACT 0=
      supplied F>SECOND value UNIFY-EXACT 0= or IF
         0 OK ! -1 FAILSET ! EXIT THEN
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
   DCUR @ R-RES dup TAG S-PUSH <> IF drop 0 OK ! -1 FAILSET ! EXIT THEN
   dup P>TYPE T-RES {: supplied:n :}
   P>REST R-RES dup TAG S-PUSH <> IF drop 0 OK ! -1 FAILSET ! EXIT THEN
   P>REST R-RES dup TAG S-PUSH <> IF drop 0 OK ! -1 FAILSET ! EXIT THEN
   dup P>TYPE T-RES {: parent-cell:n :}
   P>REST {: rest:n :}
   parent-cell HIDDEN-PARAM? IF parent-cell MK-LOGICAL ELSE parent-cell THEN {: parent:n :}
   parent TAG T-STALE = IF parent parent STALE-WHY C2-REJECT EXIT THEN
   parent RECORDS? 0= IF 0 OK ! -1 FAILSET ! EXIT THEN
   parent 3 PARAM>ARG {: value:n :}
   value INIT-VALUE-OK? 0= IF 0 OK ! -1 FAILSET ! EXIT THEN
   value TFAM:INIT-RECORD? 0= IF drop drop drop 0 OK ! -1 FAILSET ! EXIT THEN
   {: width:n bytes:n align:n :}
   parent 1 PARAM>ARG T-RES {: ceiling:n :}
   supplied TAG T-FORALL = IF
      supplied F>DOMAIN BIND-CHILD <> supplied F>PARENT 0= or IF
         0 OK ! -1 FAILSET ! EXIT THEN
      supplied F>PARENT ceiling UNIFY-EXACT 0= IF
         0 OK ! -1 FAILSET ! EXIT THEN
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

\ The selector is a token, never a runtime field id. The callback may change
\ the field, but it cannot return the child view or the projected region.
: SCOPE-FIELD-STEP ( ptr u8 n -- ) {: name:ptr size:n :}
   DCUR @ R-RES dup TAG S-PUSH <> IF drop SCOPE-FIELD-REJECT EXIT THEN
   dup P>TYPE T-RES {: supplied:n :}
   supplied SCOPE-FIELD-MAY-THROW? {: may-throw:bool :}
   P>REST R-RES dup TAG S-PUSH <> IF drop SCOPE-FIELD-REJECT EXIT THEN
   dup P>TYPE T-RES {: parent-cell:n :}
   P>REST {: rest:n :}
   parent-cell HIDDEN-PARAM? IF parent-cell MK-LOGICAL ELSE parent-cell THEN {: parent:n :}
   parent TAG T-STALE = IF SCOPE-FIELD-REJECT EXIT THEN
   parent MUT-VIEW? 0= IF SCOPE-FIELD-REJECT EXIT THEN
   parent 3 PARAM>ARG T-RES {: initialized:n :}
   initialized TAG T-PARAM <> IF SCOPE-FIELD-REJECT EXIT THEN
   initialized PARAM>FAM C2-INIT-FAM @ <> IF SCOPE-FIELD-REJECT EXIT THEN
   initialized 0 PARAM>ARG T-RES {: init-scope:n :}
   initialized 1 PARAM>ARG T-RES {: record:n :}
   record INIT-VALUE-OK? 0= IF SCOPE-FIELD-REJECT EXIT THEN
   record name size TFAM:INIT-FIELD-NAME? 0= IF 2drop drop SCOPE-FIELD-REJECT EXIT THEN
   {: descriptor:n off:n bytes:n :}
   descriptor FIELD-PARAM? 0= IF SCOPE-FIELD-REJECT EXIT THEN
   descriptor FIELD-INNER {: field:n :}
   parent 1 PARAM>ARG T-RES {: ceiling:n :}
   supplied ceiling SCOPE-FIELD-SCHEME? 0= IF SCOPE-FIELD-REJECT EXIT THEN
   ceiling SCOPE-CHILD-FRESH {: child:n :}
   parent descriptor SCOPE-FIELD-REGION {: projected:n :}
   supplied ceiling child projected SCOPE-FIELD-OPEN
   OK @ 0= IF EXIT THEN
   DCUR @ XE-PD !  RCUR @ XE-PR !
   parent child projected init-scope field SCOPE-FIELD-CALLBACK
   OK @ 0= IF EXIT THEN
   child SCOPE-EXTERIOR-XT projected SCOPE-EXTERIOR-XT or IF
      SCOPE-FIELD-REJECT EXIT THEN
   THROW-CUR? may-throw or IF CALL-THROW-EDGE THEN
   off bytes SCOPE-FIELD-FACTS ;

: SCOPE-FIELD-TOK ( ptr u8 n -- )
   EV-OPERAND                       \ the loaned field's name; the loan step compiles it
   0 FIELD-LOAN-PENDING !
   SCOPE-FIELD-STEP ;

: DO-TOK-BODY ( ptr u8 n -- ) {: a:ptr u:n :}
   0 CURSYM !
   a u LITERAL-TOK? IF EXIT THEN
   a u TOK-INTRINSIC {: id:n :}
   a u BIND-SOURCE-CALL
   CUSING @ 0= TOK-SYM @ SYM-EFFECT-UNRESOLVED? and IF
      TOK-SYM @ PRIM-TRUSTED-SYM? IF
         TOP-STEP @ 0= IF -1 CAPREQ !  0 OK !  -1 FAILSET !  EXIT THEN
      ELSE UNSEEN-RAISE  EXIT THEN
   THEN
   \ What the call may do at run time is its symbol's, read before an identity
   \ arm takes the token: `create` leaves through DEFINER-TOK and still carries
   \ its axiom.
   TOK-SYM @ CTL-FLAGS-SYM CTL-INHERITED and INHSET @ or INHSET !
   id DEFINER-TOK IF EXIT THEN
   id INTRINSIC-CELL-FETCH = IF CELL-FETCH-TOK EXIT THEN
   id INTRINSIC-CELL-STORE = IF RES-TRUE CELL-STORE-TOK EXIT THEN
   id INTRINSIC-ATOMIC-STORE = id INTRINSIC-XT-STORE = or IF RES-FALSE CELL-STORE-TOK EXIT THEN
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
      CURSYM @ NORET-CREATES@ dup 0 <> IF WRAPC !  WRAPN @ 1 + WRAPN ! ELSE drop THEN
   THEN
   FEP-CLEAR
   CURSYM @ CHECKER-FIND-USIG-SYM drop
   FEP-HIT? IF FEP @ TOK-EFF ! THEN
   CURSYM @ TFAM:TFAM-CTOR-STEP? IF EXIT THEN
   FEP-HIT? IF
      CURSYM @ CALL-AUTHORITY
      dup CALL-REFUSED = IF drop -1 CAPREQ !  0 OK !  -1 FAILSET !  EXIT THEN
      dup CALL-RECOVERS = IF RES-TRUE CHECKER-EFFECT-AUTHORITY:RECOVERY-USED! THEN
      CALL-PRIMS <> IF FEP @ EFF-APPLY EXIT THEN
   THEN
   CURSYM @ TRY-PRIMS IF EXIT THEN
   TSEEN @ 0 <> IF TFA @ E-PTR EFF-APPLY EXIT THEN
   TOK-LIVE-REC @ IF a u CALL-UNDEFINED drop EXIT THEN
   a u REFUSE-UNDEFINED ;

: DO-TOK ( ptr u8 n -- ) {: a:ptr u:n :}
   DCUR @ {: pre:n :}  RCUR @ {: prer:n :}
   a u DO-TOK-BODY
   pre prer ZERO-USE-TEST ;

\ Open the frames of a match on family FAM: its branches start from MTCH-ROW,
\ their payloads come from MTCH-TAGT (0: none), and LOST says whether a
\ scrutinee cell was lost.
: MATCH-OPEN ( n n -- )
   {: fam:n lost:n :}
   MF-ENSURE
   MF-DEPTH @ 1 + MF-DEPTH !
   MF-CUR
   {: r:ptr :}
   fam r MF.FAM !
   MTCH-TAGT @ r MF.TERM !
   MTCH-ROW @ r MF.BASE !
   RCUR @ r MF.RBASE !
   0 r MF.OUT !  0 r MF.ROUT !  0 r MF.HAS !
   fam TFAM:TFAM-VAR-COUNT@ dup r MF.VCNT !
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

: MATCH-FAM-STEP ( n -- )
   {: fam:n :}
   CHECKER-TAPE:K-MATCH fam 0 EVENT!
   #CFC @ CF-FRAMES-MAX 2 - > IF MD-DEPTH MDIAG! MATCH-ABANDON EXIT THEN   \ two CFS frames per match: reject, never UNCK
   fam MATCH-SCRUT? 0= IF fam MATCH-SCRUT-FAIL EXIT THEN
   DCUR @ MTCH-W @ ROW-TOP-LOST {: lost:n :}
   MTCH-W @ MWIN-CELLS!                      \ the bundle a cell-accurate consumer has to pop
   fam lost MATCH-OPEN ;

: MATCH-FAM-TOK ( ptr u8 n -- )
   {: a:ptr u:n :}
   a u TFAM:TFAM-MATCH-FAM 0= IF drop CHECKER-TAPE:K-MATCH -1 0 EVENT!  MATCH-ABANDON EXIT THEN
   MATCH-FAM-STEP ;

: MATCH-VARIANT-TOK ( ptr u8 n -- )
   {: a:ptr u:n :}
   a u s" ;match" CORE-STR= IF
      CHECKER-TAPE:K-MATCH-END MF-CUR MF.FAM @ 0 EVENT!
      MATCH-SEMI EXIT THEN
   a u MF-CUR MF.FAM @ TFAM:TFAM-MATCH-VARIANT 0= IF
      drop CHECKER-TAPE:K-MATCH-ARM -1 0 EVENT!  MD-VAR-UNKNOWN MDIAG-COMPILE! 0 OK !  -1 MREJ !  -1 MPEND !  3 CHECKER-MATCH-MODE !  EXIT THEN
   {: vid:n :}
   CHECKER-TAPE:K-MATCH-ARM  MF-CUR MF.FAM @  vid EV-VARIANT
   vid TFAM:SUMV-TAG@ {: tag:n :}
   MF-CUR MF.SEEN @ tag MSEEN-GET IF MD-VAR-DUP MDIAG! 0 OK !  -1 MREJ ! THEN
   MF-CUR MF.SEEN @ tag MSEEN-SET
   vid MPEND !
   3 CHECKER-MATCH-MODE ! ;

: MATCH-OF-TOK ( ptr u8 n -- )
   {: a:ptr u:n :}
   a u s" of" CORE-STR= IF CHECKER-TAPE:K-MATCH-OF 0 0 EVENT!
   ELSE MD-MISSING-OF MDIAG-COMPILE! 0 OK !  -1 MREJ ! THEN
   MF-CUR {: r:ptr :}
   r MF.BASE @ DCUR !
   r MF.RBASE @ RCUR !
   CF-ENTRY-FACTS
   MPEND @ 0 < 0=  r MF.TERM @ 0 <>  and IF
      MPEND @  r MF.TERM @  DCUR @  TFAM:TFAM-MATCH-PAY DCUR !
      CF-TOP CF.FT @ 0 <> IF
         DCUR @ ROW-TERMS r MF.BASE @ ROW-TERMS - 0 max {: count:n :}
         DCUR @ count -1 ROW-TOP-OUTPUT-LOSS DCUR !
      THEN
      DCUR @ r MF.BASE @ MATCH-PAYLOAD-RECORD
   THEN
   10  r MF.BASE @  0  r MF.RBASE @  0  CF-PUSH
   0 DEADP !
   0 CHECKER-MATCH-MODE ! ;

: MATCH-TOK ( ptr u8 n -- ) {: a:ptr u:n :}
   CHECKER-MATCH-MODE @ 1 = IF a u MATCH-FAM-TOK EXIT THEN
   CHECKER-MATCH-MODE @ 2 = IF a u MATCH-VARIANT-TOK EXIT THEN
   CHECKER-MATCH-MODE @ 4 = IF a u MATCH-SKIP-TOK EXIT THEN
   a u MATCH-OF-TOK ;

\ A failed family resolve still consumes the variant token (CONFAM -1): the
\ three-token form always captures whole, so the trailing operand never falls
\ through to locals or word lookup.
: CONSTRUCT-TOK ( ptr u8 n -- )
   {: a:ptr u:n :}
   EV-OPERAND                          \ the family compiles nothing, nor does a variant of no family
   CONM @ 1 = IF
      a u TFAM:TFAM-CONSTRUCT-FAM IF CONFAM ! ELSE drop 0 OK !  -1 CONFAM !  CONSTRUCT-NONE THEN
      2 CONM !
      EXIT THEN
   CONFAM @ 0 < 0= IF
      a u CONFAM @ TFAM:TFAM-MATCH-VARIANT IF                           \ latch variant SUMV id for the mismatch capture
         dup CVLIVE !  CHECKER-TAPE:K-CONSTRUCT CONFAM @ rot EV-VARIANT
      ELSE drop CONSTRUCT-NONE THEN
      a u CONFAM @ TFAM:TFAM-CONSTRUCT-STEP 0= IF 0 OK ! THEN
      -1 CVLIVE !                                                      \ variant leaves scope with the step
   THEN
   0 CONM ! ;

\ The handle, pushed as a string literal is, then the call, judged as a call
\ the body spelled.
: POSTPONE-CALL ( ptr n n -- ) {: run:ptr sym:n :}
   -1 PPAY-ON !  -1 POST-ON !
   s" [package]" PE-SYM-OF PRIM-FIRST-SYM E-PTR EFF-APPLY
   run sym CALL-PRESET
   DCUR @ XE-PD !  RCUR @ XE-PR !
   run CHECKER-RESOLVE:CK-REC-NAME$ DO-TOK
   OK @ IF THROW-CUR? IF CALL-THROW-EDGE THEN THEN ;

: POSTPONE-TOK ( -- )
   IS-TARGET-TOK? 0= IF IS-FAIL EXIT THEN
   -1 CRTGT !                              \ a refusal names the operand, as a tick's does
   POSTPONE-RUN-REC {: run:ptr sym:n :}
   IS-TA@ IS-TU @ s" ;" CORE-STR= IF
      0 POST-ENSURE  0 POST-U !  s" end" POST+
      run sym POSTPONE-CALL EXIT
   THEN
   POSTPONE-TARGET {: rec:ptr :}
   rec NULL-PTR = IF POSTPONE-UNDEFINED EXIT THEN
   rec REC-HOME {: pa:ptr pu:n vis:n homed:bool :}
   homed 0= IF POSTPONE-UNDEFINED EXIT THEN
   pa pu vis rec CHECKER-RESOLVE:CK-REC-NAME$ POST-HANDLE!
   run sym POSTPONE-CALL ;

\ The steps of a token no keyword, mode or literal form took, a call's or a
\ number's; a call's binding is made on the first ask.
: CALL-STEP ( ptr u8 n -- ) {: a:ptr u:n :}
   TKF TKFU @ SPELLED-STEP? IF EXIT THEN
   TKF TKFU @ CHECKER-PREFLIGHT:BODY-TOK? IF REJECT-IMMEDIATE THEN
   DCUR @ XE-PD !  RCUR @ XE-PR !          \ the pre-op rows ROW-COMMON needs below
   TKF TKFU @ RELEASE-CALL? LAYOUT-RELEASE !
   TKF TKFU @ DO-TOK
   0 LAYOUT-RELEASE !
   OK @ IF THROW-CUR? IF CALL-THROW-EDGE THEN THEN
   OK @ IF DEAD-CUR? IF a u DEAD-OWNER! -1 DEADP ! THEN THEN
   OK @ #CFC @ 0 > and IF BARRIER-CUR? IF ALL-CF-UNIFORM? 0= IF REJECT-DIVBAR THEN THEN THEN
   LITERAL-PAYLOAD-STEP
   PIMM-STREAM @ 0 = IF
      CURSYM @ PIMM-IX dup 0 < 0= IF PIMM-CNT@ PIMM-SKIP ELSE drop THEN
   THEN ;

: DO-TOK1 {: a u :}
   a u DO-TOK-HEAD
   {: unj unsafe ok-in unck-in pin-in :}
   TOK0 @ IF NAME-TOK ELSE
   TKF TKFU @ LIVE-TOKEN? 0= IF -1 DEADERR ! 0 OK !  TKF TKFU @ DEAD-TOK ELSE
   LMODE @ IF a TKF TKFU @ LOC-TOK ELSE
   FIELD-LOAN-PENDING @ IF TKF TKFU @ SCOPE-FIELD-TOK ELSE
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
   TKF TKFU @ UNSAFE-TOK? IF
      BWIN-UNJUDGED @ IF TKF TKFU @ BIND-SOURCE-CALL THEN
      REJECT-UNSAFE ELSE
   a u RETIRED-GLOBAL? IF REJECT-RETIRED ELSE
   TKF TKFU @ s" is" CORE-STR= IF IS-TOK ELSE
   TKF TKFU @ BTICK-CAND? IF BTICK-TOK ELSE
   TKF TKFU @ s" postpone" CORE-STR= IF POSTPONE-TOK ELSE
   TKF TKFU @ CF-CODE {: cf:n :}
   OK @ IF cf 21 = IF a u DEAD-OWNER! THEN THEN   \ exit
   OK @ IF cf 22 = IF a u DEAD-OWNER! THEN THEN   \ leave
   OK @ IF cf 12 = IF a u DEAD-OWNER! THEN THEN   \ again
   cf 0 >= IF cf CF-STEP THEN
   cf 0 < IF a u CALL-STEP THEN
   THEN THEN THEN THEN THEN THEN THEN THEN THEN THEN THEN THEN THEN THEN THEN THEN
   THEN THEN
   a u unj unsafe ok-in unck-in pin-in DO-TOK-TAIL ;

\ The rows ( din dout rin rout ) of a schema effect, checked by the rules a
\ written signature's rows pass; a refusal sets SGBAD.
: EFFECT-SIG ( n -- n n n n ) {: node:n :}
   SIG-UNRES-CLEAR
   PKRESET NMAP-RESET ROWMAP-RESET FAM-RESET  0 SQUOT-N !  0 BIND-DEPTH !
   0 SIG-LEVEL !  0 SGHASR !  0 RR-SHARED !
   FRESH MK-ROW dup PD-BASE ! {: dr :}
   ATOMN @ PD-ATOM0 !
   node dr TFAM:EFFECT-ROWS
   PD-OUT !  PD-IN !  0 PR-IN !  0 PR-OUT !
   SGBAD-CLEAR
   NULL-PTR 0 PD-IN @ PD-OUT @ 0 0 RES-FALSE SIG-ROWS-CHECK
   PD-IN @ PD-OUT @ 0 0 ;

package CHECK
public

\ Whether effect node NODE's rows pass a written signature's rules.
: EFFECT-RULES? ( n -- bool ) {: node:n :}
   node EFFECT-SIG 2drop 2drop SGBAD @ 0= ;
;package

\ The effect `define-word` gave the pending definition when the text walked is
\ that definition's, else 0.
: BUILT-EFFECT ( -- n )
   data-base PEND-CELL + @ 0= IF 0 EXIT THEN
   TBASE@ NULL-PTR -  data-base BODY-TEXT:BASE-CELL + @ <> IF 0 EXIT THEN
   data-base BODY-ITEMS:EFFECT-CELL + @ ;

\ The declared rows of the definition CHECK is on: from its effect when
\ `define-word` built it, else from the signature text at SGA.
: PENDING-SIG ( -- n n n n )
   BUILT-EFFECT {: eff:n :}
   eff 0 <> IF eff EFFECT-SIG EXIT THEN
   SGA @ SGU @ PARSE-SIG-RAW ;

\ Records the pending definition's declared effect under name NA, AT the name
\ as read: its rows from PENDING-SIG, its text the signature at SGA.
: CHECKER-USIG-CERT-ADD-PENDING ( ptr u8 n ptr u8 n bool -- )
   {: na:ptr nu:n at:ptr atu:n external:bool :}
   na nu at atu USIG-CERT-ADMIT
   NEW
   SGBAD-CLEAR
   PENDING-SIG {: din:n dout:n rin:n rout:n :}
   SGA @ SGU @  CHECKER-REC-A@ CHECKER-REC-U@  at atu
   din dout rin rout external USIG-ADD-ROWS drop
   DL-TAKE ;

\ Installs the '( ... )' in TSTART..TI when it is the first after the name, the
\ signature (a built definition's from its effect); true when that ends the walk.
: PAREN-SIG? ( -- bool )
   VSIG @  SGSEEN @ 0= and  TOKIX @ 2 < and  IF
      TSTART @ TADDR SGA !  TI @ TSTART @ - SGU !
      PENDING-SIG SIG-INSTALL EXIT
   THEN
   RES-FALSE ;

\ What a walk does around the step of the token at TSTART..TI: the tape's
\ fields and the cursor before it, answering TOK0, then the summary and the
\ report after, answering whether the walk ends.
: SCAN-TOK-HEAD ( -- n )
   CHECKER-TAPE:ARMED @ IF
      TI @ TSTART @ - SCAN-U !  TOK0 @ SCAN-TOK0 !  0 SPAY-ON !  0 CPAY-ON !  0 PPAY-ON !  0 POST-ON !  0 TPAY-ON !
      0 IS-PEND !  0 EV-HIT !
   THEN
   CUR-TOKEN
   SPAY-S @ SPAY-ARMED !  0 SPAY-S !
   TOK0 @ ;

: SCAN-TOK-TAIL ( n -- bool ) {: was-name:n :}
   TORDER-SCAN-UNKNOWN @ IF RES-TRUE EXIT THEN
   was-name 0= IF TSTART @ TADDR TI @ TSTART @ - ZSUMMARY-TOKEN THEN
   CHECKER-TAPE:ARMED @ IF SCAN-REPORT THEN
   RES-FALSE ;

: CHECK-SCAN-WALK ( -- )
   0 SPAY-S !
   EV-START
   TAPE-FEED? IF TBASE@ TBLEN @ CHECKER-TAPE:SCAN THEN
   BEGIN TI @ TBLEN @ < WHILE
     TI @ CUR-GAP !
     BEGIN TI @ 32 TBYTE=? WHILE TI @ 1 + TI ! REPEAT
     TI @ TBLEN @ < IF
       TI @ TBYTE@ 40 =  TI @ 1 + TBLANK?  and IF   \ '(' then a blank (not '(CMP)') -> sig or comment
         CUR-PAREN-AT
         TI @ 1 + TI !  TI @ TSTART !
         BEGIN TI @ TBLEN @ <  TI @ 41 TBYTE=? 0=  and WHILE TI @ 1 + TI ! REPEAT
         TI @ TBLEN @ >= IF 0 PARSE-COMPLETE ! THEN
         PAREN-SIG? IF EXIT THEN
         TI @ TBLEN @ < IF TI @ 1 + TI ! THEN
         CUR-PAREN
       ELSE
         TI @ TSTART !
         BEGIN TI @ TBREAK? 0= WHILE TI @ 1 + TI ! REPEAT
         SCAN-TOK-HEAD {: was-name:n :}
         TSTART @ TADDR  TI @ TSTART @ -  DO-TOK1
         was-name SCAN-TOK-TAIL IF EXIT THEN
       THEN
     THEN
   REPEAT
   CUR-END ;

\ ---- the walk of a built definition's items ---------------------------------
\ A definition `define-word` opened is walked by its items, not its listing's
\ bytes: each row's span in the listing is its token, and its kind picks the
\ step that parsed source reaches by spelling. A CALL binds the record the
\ definer found, so nothing the building package defines captures it.
BODY-ITEMS:BASE-CELL RESERVED-PTR-U8-CELL ITEM-BLOCK

: ITEM-CELL@ ( n n -- n ) {: i:n f:n :}
   ITEM-BLOCK @  i BODY-ITEMS:ROW * f + cells +  cell-view @ ;

: ITEM-COUNT ( -- n ) data-base BODY-ITEMS:COUNT-CELL + @ ;

\ The items stand for the text walked: it is the pending definition's body
\ text, no text an item does not span joined it, and the last item ends it.
: ITEM-WALK? ( -- bool )
   data-base PEND-CELL + @ 0= IF RES-FALSE EXIT THEN
   TBASE@ NULL-PTR -  data-base BODY-TEXT:BASE-CELL + @ <> IF RES-FALSE EXIT THEN
   data-base BODY-ITEMS:RAW-CELL + @ 0 <> IF RES-FALSE EXIT THEN
   ITEM-COUNT {: n:n :}
   n 0= IF RES-FALSE EXIT THEN
   n 1 - BODY-ITEMS:OFF ITEM-CELL@  n 1 - BODY-ITEMS:LEN ITEM-CELL@ +  1 +
   TBLEN @ = ;

: ITEM-SPAN ( n -- ) {: i:n :}
   TI @ CUR-GAP !
   i BODY-ITEMS:OFF ITEM-CELL@ TSTART !
   TSTART @  i BODY-ITEMS:LEN ITEM-CELL@ +  TI ! ;

\ A signature's span holds its parentheses, and the byte walk's paren branch
\ reads inside them.
: ITEM-SIG ( n -- bool ) {: i:n :}
   TI @ CUR-GAP !
   i BODY-ITEMS:OFF ITEM-CELL@ TI !
   CUR-PAREN-AT
   TI @ 1 + TSTART !
   TI @  i BODY-ITEMS:LEN ITEM-CELL@ +  1 - TI !
   PAREN-SIG? IF RES-TRUE EXIT THEN
   TI @ 1 + TI !
   CUR-PAREN
   RES-FALSE ;

\ A token after a call that cannot return is dead, as in the byte walk.
: ITEM-LIVE? ( -- bool )
   TKF TKFU @ LIVE-TOKEN? IF RES-TRUE EXIT THEN
   -1 DEADERR !  0 OK !  TKF TKFU @ DEAD-TOK
   RES-FALSE ;

: ITEM-CALL ( ptr u8 n n -- ) {: a:ptr u:n ix:n :}
   ix CHECKER-RESOLVE:REC-AT  TKF TKFU @ BIND-RECORD
   ITEM-LIVE? 0= IF EXIT THEN
   TKF TKFU @ UNSAFE-TOK? IF
      BWIN-UNJUDGED @ IF TKF TKFU @ BIND-SOURCE-CALL THEN
      REJECT-UNSAFE EXIT
   THEN
   a u CALL-STEP ;

: ITEM-INT ( n -- ) {: v:n :}
   BIND-CLEAR
   ITEM-LIVE? 0= IF EXIT THEN
   0 CURSYM !
   v INT-STEP ;

: ITEM-STEP ( n -- ) {: i:n :}
   TSTART @ TADDR  TI @ TSTART @ -  {: a:ptr u:n :}
   a u DO-TOK-HEAD
   {: unj unsafe ok-in unck-in pin-in :}
   i BODY-ITEMS:KIND ITEM-CELL@ {: kind:n :}
   kind BODY-ITEM-KIND:NAME = IF NAME-TOK ELSE
   kind BODY-ITEM-KIND:CALL = IF a u  i BODY-ITEMS:A ITEM-CELL@  ITEM-CALL ELSE
   kind BODY-ITEM-KIND:INT = IF i BODY-ITEMS:A ITEM-CELL@ ITEM-INT ELSE
      s" checker: an item of no kind the walk knows" 76 die
   THEN THEN THEN
   a u unj unsafe ok-in unck-in pin-in DO-TOK-TAIL ;

: ITEM-WALK ( -- )
   0 SPAY-S !
   EV-START
   TAPE-FEED? IF TBASE@ TBLEN @ CHECKER-TAPE:SCAN THEN
   ITEM-COUNT 0 ?DO
      i BODY-ITEMS:KIND ITEM-CELL@ BODY-ITEM-KIND:SIG = IF
         i ITEM-SIG IF UNLOOP EXIT THEN
      ELSE
         i ITEM-SPAN
         SCAN-TOK-HEAD {: was-name:n :}
         i ITEM-STEP
         was-name SCAN-TOK-TAIL IF UNLOOP EXIT THEN
      THEN
   LOOP
   TI @ CUR-GAP !  TBLEN @ TI !
   CUR-END ;

\ Walks a body, holding its using refusals for the end, where a structure open
\ at closer CA CU is refused first; a prefix passes none.
: CHECK-SCAN ( ptr u8 n -- )
   {: ca:ptr cu:n :}
   BODY-WALK @ {: walk0:n :}
   -1 BODY-WALK !
   [: ITEM-WALK? IF ITEM-WALK ELSE CHECK-SCAN-WALK THEN ;] catch
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

\ An effect too deep or wide to record is uncheckable for it; a deferral would
\ record it, so a covered name yields; a pinned refusal stands.
: UNFIT-UNCHECKABLE ( -- )
   UNFIT @ 0= IF EXIT THEN
   UNSEEN @ 0 <> IF
      0 UNSEEN !  0 UNDEFERR !
      RCOMPILE @ 0 <> IF EXIT THEN
      UNFIT-WHY MDIAG !  UNCK-RAISE  EXIT
   THEN
   OK @ 0=  UNCK @ 0 <>  or IF EXIT THEN
   UNFIT-WHY MDIAG!  UNCK-RAISE ;

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
   UNSAFE @ RETIRED @ or  IMMERR @ or  LOCALBAD @ or  LINLOCBAD @ or  QDUPBAD @ or  QUALBAD @ or  CAPREQ @ or  MREJ @ or  NPBAD @ or  RCOMPILE @ or 0 <>  REFUSAL@ RF-UNDEF =  or IF VERDICT-REFUSED ELSE
   UNCK @ 0 <> IF VERDICT-UNCHECKABLE ELSE OK @ THEN THEN ;

\ A product bundle is its field cells in slot order, so generated MAKE and UNMAKE
\ have empty bodies; their sealed plan, never user source, earns the product effect.
4 constant CTOR-PEND-CAP-INIT
1 cells constant CTOR-PEND-REC
$7FFFFFFFFFFFFFFF constant CTOR-PEND-BYTE-MAX
CTOR-PEND-BYTE-MAX CTOR-PEND-REC / constant CTOR-PEND-ROW-MAX
create CTOR-PEND-BOOT CTOR-PEND-CAP-INIT CTOR-PEND-REC * allot
PERSISTED-PTR-VARIABLE CTOR-PEND-P   CTOR-PEND-BOOT CTOR-PEND-P !
variable CTOR-PEND-CAP     CTOR-PEND-CAP-INIT CTOR-PEND-CAP !
variable CTOR-PEND-COUNT
variable CTOR-PEND-POS

: CTOR-PEND-SYM ( n -- ptr n )
   dup 0 < over CTOR-PEND-CAP @ >= or IF
      s" checker: generated constructor plan capacity overflow" 76 die
   THEN
   CTOR-PEND-REC * CTOR-PEND-P @ + ;

: CTOR-PEND-ROWS>BYTES ( n -- n )
   dup 0 < over CTOR-PEND-ROW-MAX > or IF
      s" checker: generated constructor plan capacity overflow" 76 die
   THEN
   CTOR-PEND-REC * ;

: CTOR-PEND-NEXT-CAP ( -- n )
   CTOR-PEND-COUNT @ CTOR-PEND-ROW-MAX >= IF
      s" checker: generated constructor plan capacity overflow" 76 die
   THEN
   CTOR-PEND-COUNT @ 1 + {: need:n :}
   CTOR-PEND-CAP @ 0 <= CTOR-PEND-CAP @ CTOR-PEND-ROW-MAX > or IF
      s" checker: generated constructor plan capacity overflow" 76 die
   THEN
   CTOR-PEND-CAP @ CTOR-PEND-ROW-MAX 2 / <= IF
      CTOR-PEND-CAP @ 2 * need max EXIT
   THEN
   need ;

: CTOR-PEND-ENSURE ( -- )
   CTOR-PEND-COUNT @ CTOR-PEND-CAP @ < IF EXIT THEN
   CTOR-PEND-NEXT-CAP {: cap:n :}
   CTOR-PEND-P CTOR-PEND-CAP @ CTOR-PEND-ROWS>BYTES
      cap CTOR-PEND-ROWS>BYTES REG-GROW1
   cap CTOR-PEND-CAP ! ;

: CTOR-PEND-CLEAR ( -- )
   0 CTOR-PEND-COUNT !
   0 CTOR-PEND-POS ! ;

: CTOR-PEND-REWIND ( -- )
   0 CTOR-PEND-POS ! ;

: CTOR-PEND-SYM+ ( n -- ) {: sym:n :}
   CTOR-PEND-ENSURE
   0
   BEGIN dup CTOR-PEND-COUNT @ < WHILE
      dup CTOR-PEND-SYM @ sym = IF
         drop s" checker: duplicate generated constructor plan" 76 die
      THEN
      1 +
   REPEAT
   drop
   sym CTOR-PEND-COUNT @ CTOR-PEND-SYM !
   CTOR-PEND-COUNT @ 1 + CTOR-PEND-COUNT ! ;

: CTOR-PEND-TAKE? ( n -- bool ) {: sym:n :}
   CTOR-PEND-POS @ CTOR-PEND-COUNT @ < 0= IF RES-FALSE EXIT THEN
   CTOR-PEND-POS @ CTOR-PEND-SYM @ sym <> IF RES-FALSE EXIT THEN
   CTOR-PEND-POS @ 1 + CTOR-PEND-POS !
   RES-TRUE ;

: CTOR-PEND-MATCH? ( -- bool )
   CTOR-PEND-POS @ CTOR-PEND-COUNT @ <  NMU @ 0 > and 0= IF RES-FALSE EXIT THEN
   NMA @ NMU @ CHECKER-RECORD-SYM CTOR-PEND-TAKE? ;

: CTOR-PEND-REQUIRE-DONE ( -- )
   CTOR-PEND-POS @ CTOR-PEND-COUNT @ <> IF
      s" checker: incomplete generated constructor plan" 76 die
   THEN ;

\ `cast: NAME ( in -- out )` is a declaration: the engine's reader keyword
\ publishes an identity word and asks CHECKER-DEFCAST to prove the declared
\ retype legal. The refusals are properties of the two declared terms only.

: CAST-ROW-1? ( n -- bool )
   R-RES dup TAG S-PUSH <> IF drop RES-FALSE EXIT THEN
   P>REST R-RES TAG S-ROW = ;
: CAST-ROW-TERM ( n -- n ) R-RES P>TYPE ;
\ A cast may retype one machine cell. A pointer chain must end at storage: an
\ atom is a phantom index. The pointee carries no width rule, since a layout of
\ any width is one pointer cell away.
: CAST-TERM? ( n -- bool ) {: t0:n :}
   t0 T-RES {: t:n :}
   t TAG T-CON = IF RES-TRUE EXIT THEN
   t TAG T-VAR = IF RES-TRUE EXIT THEN
   t TAG T-PARAM = IF t T-WIDTH 1 = EXIT THEN
   t TAG T-QUOT = IF RES-TRUE EXIT THEN
   t TAG T-PTR = IF
      t BEGIN dup TAG T-PTR = WHILE PTR>INNER T-RES REPEAT
      TAG T-ATOM <> EXIT
   THEN
   RES-FALSE ;
\ True for an error the checker reports itself before throwing it.
: REPORTED-ERROR? ( n -- bool ) {: rc:n :}
   rc E-USING-SHADOW-GLOBAL =
   rc E-USING-AMBIGUOUS = or  rc E-TRUST-UNRESOLVED = or
   rc E-SHADOWED-ARITY = or  rc E-GENERATES-ROW = or  rc E-PARSES-ROW = or
   rc E-NAMES-ROW = or  rc E-BAD-STORED-SIGNATURE = or
   rc E-CAST-ARITY = or  rc E-CAST-CLASS = or  rc E-CAST-FAM = or
   rc E-CAST-LINEAR = or  rc E-CAST-MINT = or
   rc E-CAST-SCOPE = or  rc E-LINEAR-PAYLOAD = or  rc E-LINEAR-OWNER = or
   rc E-LINEAR-SCOPE = or ;
package CAST-ERROR
\ Zeros outside a declaration, so a saved image holds none of the build
\ process's addresses.
: CLEAR ( -- )
   NULL-PTR WORD-A !  0 WORD-U !  NULL-PTR KEY-A !  0 KEY-U !  NULL-PTR SIG-A !
   NULL-PTR WHY-A !  0 WHY-U !  NULL-PTR TOK-A !  0 TOK-U !  0 CODE ! ;
public
\ Runs xt on the declaration of name na nu with signature sa su under keyword
\ ka ku, the record live until xt returns or throws.
: DECLARE ( ptr u8 n ptr u8 n ptr u8 n [ ptr u8 n ptr u8 n -- ] -- )
   >r {: na:ptr nu:n sa:ptr su:n ka:ptr ku:n :}
   na WORD-A !  nu WORD-U !  ka KEY-A !  ku KEY-U !  sa SIG-A !
   na nu sa su r> [: CLEAR ;] finally ;
: TOKEN! ( ptr u8 n -- ) TOK-U !  TOK-A ! ;
: RAISE ( ptr u8 n n -- ) {: wa:ptr wu:n code:n :}
   wa WHY-A !  wu WHY-U !  code CODE !
   RENDER
   code CHECKER-REFUSE ;
\ Refuses signature sa su of name na nu that does not parse.
: REFUSE-SIGNATURE ( ptr u8 n ptr u8 n -- ) {: sa:ptr su:n na:ptr nu:n :}
   sa su na nu na nu BADSIG-XT
   sa su na nu SGBAD-PIN
   E-BAD-STORED-SIGNATURE CHECKER-REFUSE ;
: SKIP ( n -- ) {: rc:n :}
   rc REPORTED-ERROR? IF 1 MULTI-ERR-N +! EXIT THEN
   rc throw ;
;package

\ The cast gate: true when the output mints a pointer or quotation, else a
\ representation cast.
: CAST-CERTIFY ( -- bool )
   SGIN @ CAST-ROW-TERM dup CAST-TERM? swap OPAQUE-XT? or 0= IF
      s" the input is not one machine cell a cast may retype" E-CAST-CLASS CAST-ERROR:RAISE
   THEN
   SGOUT @ CAST-ROW-TERM CAST-TERM? 0= IF
      s" the output is not one machine cell a cast may retype" E-CAST-CLASS CAST-ERROR:RAISE
   THEN
   SGOUT @ CAST-ROW-TERM INTRO-ATOM CAST-INTRODUCES? IF
      s" the output hands out an atom, a phantom index" E-CAST-CLASS CAST-ERROR:RAISE
   THEN
   SGOUT @ CAST-ROW-TERM INTRO-WIDTH CAST-INTRODUCES? IF
      s" the output is a width, which no cast yields" E-CAST-CLASS CAST-ERROR:RAISE
   THEN
   \ The source's consumed rows and pointees introduce values too: what the
   \ result is fed or has written through it reaches the source's code as the
   \ source's type.
   CAST-WALK-RESET
   SGIN @ CAST-ROW-TERM CAST-NEG INTRO-ATOM CAST-INTRO? IF
      s" the input is handed an atom, a phantom index" E-CAST-CLASS CAST-ERROR:RAISE
   THEN
   CAST-WALK-RESET
   SGIN @ CAST-ROW-TERM CAST-NEG INTRO-WIDTH CAST-INTRO? IF
      s" the input is handed a width, which no cast yields" E-CAST-CLASS CAST-ERROR:RAISE
   THEN
   \ A row variable stands for cells the signature does not show, which each use
   \ of the cast picks; an output holding no pointer or quotation passes.
   SGOUT @ CAST-ROW-TERM INTRO-MINT CAST-INTRODUCES? {: mint:bool :}
   CAST-WALK-RESET
   mint IF SGIN @ CAST-ROW-TERM SGOUT @ CAST-ROW-TERM CAST-ROWS-KEPT? 0= IF
      s" a row variable changes place between the input and the output"
      E-CAST-CLASS CAST-ERROR:RAISE
   THEN THEN
   CAST-WALK-RESET
   \ The input is only read, so a quotation whose tails differ is any quotation.
   SGIN @ CAST-ROW-TERM RES-FALSE CAST-MAY-LINEAR? IF
      s" the input may hold a linear value" E-CAST-LINEAR CAST-ERROR:RAISE
   THEN
   CAST-WALK-RESET
   SGOUT @ CAST-ROW-TERM RES-TRUE CAST-MAY-LINEAR? IF
      s" the output may hold a linear value" E-CAST-LINEAR CAST-ERROR:RAISE
   THEN
   \ The mint rule comes last: a mint that also carries ownership is named by
   \ that stronger reject.
   mint IF
      CHECKER-AUTH-PACKAGE-MODE@ CHECKER-PACKAGE-PRIVATE <> IF
         s" the output mints a pointer or a quotation outside a private section"
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
   row CAST-ROW-TERM T-RES {: hi:n :}
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
\ leaves that tail: `[ V -- ]` or `[ ptr u8 n -- ]`. A named tail could bind
\ the callback to cells under the view that the retyped callback's own tail
\ never supplies.
: PAIR-CALLBACK? ( n -- bool ) {: q:n :}
   q TAG T-QUOT <> IF RES-FALSE EXIT THEN
   q Q>DIN ROW-2? 0= IF RES-FALSE EXIT THEN
   q Q>DOUT R-RES  q Q>DIN ROW-UNDER ROW-UNDER <> IF RES-FALSE EXIT THEN
   q Q>DOUT R-RES PAY RVK-QUOT? ;
\ A callback pack retypes `[ V -- ]` as `[ ptr u8 n -- ]` over the same tail:
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
      s" a scoped cast is not two cells on each side" E-CAST-SCOPE CAST-ERROR:RAISE
   THEN
   SGIN @ ROW-UNDER ROW-UNDER  SGOUT @ ROW-UNDER ROW-UNDER <> IF
      s" the input and output rows end in different tails" E-CAST-SCOPE CAST-ERROR:RAISE
   THEN
   SGIN @ VIEW-PAIR? SGOUT @ VIEW-REP? and IF RES-TRUE SGIN @ EXIT THEN
   SGIN @ VIEW-REP? SGOUT @ VIEW-PAIR? and 0= IF
      s" a scoped cast is neither a view pack nor a view unpack" E-CAST-SCOPE CAST-ERROR:RAISE
   THEN
   RES-FALSE SGOUT @ ;
\ The gate for a cast with a scope dependency.
: VIEW-CAST-CERTIFY ( -- )
   SGHASR @ IF
      s" a scoped cast declares a return-stack effect" E-CAST-SCOPE CAST-ERROR:RAISE
   THEN
   VIEW-CAST-SIDE {: unpack:bool side:n :}
   CHECKER-AUTH-PACKAGE-MODE@ CHECKER-PACKAGE-PRIVATE <> IF
      s" a view cast is declared outside a private section" E-CAST-SCOPE CAST-ERROR:RAISE
   THEN
   side CAST-ROW-TERM T-RES {: view:n :}
   CAST-WALK-RESET
   view view PARAM>ARGC 1 - PARAM>ARG RES-TRUE CAST-MAY-LINEAR? IF
      s" the view's element may hold a linear value" E-CAST-LINEAR CAST-ERROR:RAISE
   THEN
   unpack IF view READ-VIEW? IF EXIT THEN THEN
   \ The literal is only the owner's name, which no registry holds: the view
   \ families are declared with package "".
   CHECKER-AUTH-PACKAGE$ s" C2-MEM" CORE-STR=CI 0= IF
      s" only package C2-MEM packs a view or unpacks a mutable view"
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
      E-LINEAR-PAYLOAD CAST-ERROR:RAISE
   THEN
   tok T-RES PAY CT-PKG$ {: pa:ptr pu:n :}
   pu 0= IF
      s" the linear type is declared at top level, so no package mints it"
      E-LINEAR-OWNER CAST-ERROR:RAISE
   THEN
   pa pu CHECKER-AUTH-PACKAGE$ CORE-STR=CI 0= IF
      pa pu CAST-ERROR:TOKEN!
      s" the linear type belongs to package" E-LINEAR-OWNER CAST-ERROR:RAISE
   THEN
   CHECKER-AUTH-PACKAGE-MODE@ CHECKER-PACKAGE-PRIVATE <> IF
      s" the row is outside its owner's private section" E-LINEAR-SCOPE CAST-ERROR:RAISE
   THEN ;

\ Generative layout-buffer authorization. Every arming-state dictionary name is
\ erased once the allocator and CHECK are compiled, leaving only their baked
\ direct references, so user source cannot arm or mutate the one-shot boundary.
: LBUF-PEND! ( ptr u8 n -- ) {: a:ptr u:n :}
   0 LBUF-EVAL-OFF !
   a LBUF-PEND-A !  u LBUF-PEND-U ! ;
: LBUF-PEND-CLEAR ( -- )
   NULL-PTR LBUF-PEND-A !  0 LBUF-PEND-U !  0 LAYOUT-INTRO !  0 LAYOUT-RELEASE ! ;

\ A body-supplied name left unbound takes a new identity, and only a region name may
\ be bound: to one this body minted that no other name claims and FITS? admits.
: FB-OWNED? ( n -- bool ) {: idx:n :}
   idx cells FB-NAME + @ {: name:n :}
   idx cells FB-VAR + @ T-RES {: r:n :}
   r ISVAR IF name FB-MINT r PAY TV! THEN
   name ATOM>A name ATOM>U FRESH-REGION-NAME? 0= IF r ISVAR EXIT THEN
   idx cells FB-VAR + @ T-RES {: id:n :}
   id TAG T-ATOM <> IF RES-FALSE EXIT THEN
   id ATOM>K RGN-MARK @ < IF RES-FALSE EXIT THEN
   id ATOM>A id ATOM>U FRESH-REGION-NAME? 0= IF RES-FALSE EXIT THEN
   r ISVAR 0= IF
      id SGIN @ SGOUT @ SGRIN @ SGROUT @ CHECKER-QUOT-FRESH:FITS? 0= IF RES-FALSE EXIT THEN
   THEN
   idx 1+ FO-ENSURE
   id ATOM>K idx FO-MATCH? ;

: FB-CLAIM ( n -- ) {: idx:n :}
   idx cells FB-NAME + @ {: name:n :}
   name 0= IF EXIT THEN
   idx FB-OWNED? IF EXIT THEN
   idx cells FB-VAR + @ T-RES  name FB-MINT  RIGID-HELD-REFUSE ;

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
\ before the record step's later-wins first write.
: CHECK-REC-ADMIT ( ptr u8 n -- )
   {: a:ptr u:n :}
   a u CTOR-EXTEND?-XT IF E-CTOR-PROTECTED throw THEN
   a u CHECKER-CERT-DUP? IF CHECKER-DUP-DEFINITION THEN ;

\ Every refusal a certified definition can meet at publication, asked before its
\ first write to the store: a write left behind is read by a later verdict.
: PUBLISH-REFUSALS ( ptr u8 n -- )
   {: a:ptr u:n :}
   a u CHECK-REC-ADMIT
   CHECK-SIG? IF SGIN @ SGOUT @ a u SHADOW-ARITY-CK THEN ;

\ A definition refused in a multi-error load: counted, so the run exits
\ nonzero, and its declared signature kept as source authority, so a later
\ caller checks against it, unless that signature is bad or too big to record.
: MULTI-REFUSED ( -- )
   BWIN-UNJUDGED @ 0= 0= SGDECL-BAD @ 0= 0= and IF EXIT THEN  \ declared row reports this signature
   1 MULTI-ERR-N +!
   CHECK-SIG? SGBAD @ 0= and UNFIT @ 0= and IF
      NMA @ NMU @ CHECK-REC-ADMIT
      NMA @ NMU @ 0 NORET-ADD
      NMA @ NMU @  NMOFF @ TADDR NMU @  RES-FALSE CHECKER-USIG-CERT-ADD-PENDING
      RECOVERY-RECORD
      NMA @ NMU @ CHECKER-RECORD-SYM CK-CLOSE!
   THEN ;
: CHECK-UNRECORDED-DIN ( -- n ) UNREC-DIN @ ;
: CHECK-UNRECORDED-DOUT ( -- n ) UNREC-DOUT @ ;

: CHECK   \ ( a u -- -1=certified | 0=rejected | 1=uncheckable | 2=deferred )
   {: a u :}
   CELLS-NONE UNREC-DIN !  CELLS-NONE UNREC-DOUT !
   WRITE-WINDOW-CK
   -1 PARSE-COMPLETE !
   a u CHECK-RESET
   DOES-SPLIT @ 0 <> IF s" does>" ELSE s" ;" THEN CHECK-SCAN
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
   0 LAYOUT-XPORT !                  \ boundary unification is never in transport mode
   CHECK-FOLD-EXITS
   CONM @ 0 <> IF MD-CON-TRUNC MDIAG! THEN     \ latch truncation BEFORE the boundary
   FIELD-LOAN-PENDING @ IF SCOPE-FIELD-REJECT THEN
   CHECKER-MATCH-MODE @ 0 <>  MF-DEPTH @ 0 <>  or IF MD-TRUNC MDIAG! THEN   \ unify pins its own mismatch
   CHECK-NO-BORROW
   CHECK-SIG? CHECK-RETURNS? and IF
      CTOR-PEND-MATCH? IF
         \ One row of the sealed constructor plan: the body leaves SGIN, the
         \ field cells, whose quotations are stored.
         SGIN @ SUNI-COERCE
         SGIN @ KEEP-QUOTS
      ELSE
         LBUF-PEND-MATCH? IF
            -1 LAYOUT-INTRO !
            CHECK-FRESH-OUTPUT
            0 LAYOUT-INTRO !
         ELSE
            CHECK-FRESH-OUTPUT
         THEN
      THEN
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
   \ An uncheckable verdict is rendered here as JSON and in a multi-error load,
   \ whose record step below may refuse the definition by a throw (a duplicate,
   \ a name that keys no record) that would otherwise come before it.
   dup VERDICT-REFUSED =  over VERDICT-UNCHECKABLE = JSON-DIAGS @ MULTI-ERR? or and  or
   DIAG-QUIET @ 0= and IF DIAG-PRINT THEN
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
      \ The identity the symbol already carries (INTRINSIC) stays: a replay
      \ of the engine's own source re-records the word and never runs the
      \ line that tagged it, while `undefine` zeroed it, so a redefinition
      \ carries none. CTL-RENDERS stays for the same reason: the evaluate
      \ boundary's is an axiom, and no body derives it.
      sym CTL-FLAGS-SYM CTL-INTRINSIC-MASK CTL-RENDERS or CTL-NOMINAL or and CTLNEW !
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
      \ One word holds both, so one comparison decides whether this definition
      \ says anything the store does not already hold about its symbol.
      sym CTL-WORD-SYM
      CTLNEW @ CTLDNEW @ CTLRNEW @ XFER-PACK <> IF
         sym CTLNEW @ CTLDNEW @ CTLRNEW @ NORET-ADD-SYM
      THEN
      CHECK-SIG? IF
         NMA @ NMU @  CHECKER-USIG-CERT-PARSED
      ELSE
         NMA @ NMU @ REC-SIG
      THEN
      sym CK-CLOSE!
   THEN
   \ Deferred: the declaration is recorded with authority, so a later caller
   \ still binds to it and a duplicate is still refused, but no control claim
   \ is made for a body the run measures; only CTL-INHERITED, a may-claim the walk
   \ saw directly.
   dup 2 =  NMU @ 0 >  and  CHECK-SIG? and
   TORDER-ACTIVE @ TORDER-PARENT-GATE < and IF
      NMA @ NMU @ CHECK-REC-ADMIT
      NMA @ NMU @ INHSET @ NORET-ADD
      NMA @ NMU @  NMOFF @ TADDR NMU @  RES-TRUE CHECKER-USIG-CERT-ADD-PENDING
   THEN
   \ The count is the number of refusals rendered, so a quiet scan's is not
   \ counted.
   dup 0 =  over 1 = or  MULTI-ERR? and  NMU @ 0 > and
   DIAG-QUIET @ 0= and IF
      MULTI-REFUSED
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
$88 constant RBF.PIMMN-OFF                 \ the parse-imm table's row count
$90 constant RBF.COORD-OFF
$98 constant RBF.OVNDICT-OFF               \ the checker overlay's NDICT, log count,
$A0 constant RBF.OVLOG-OFF                 \ CP and WIDN (CHECKER-OVERLAY:MARKS)
$A8 constant RBF.OVCP-OFF
$B0 constant RBF.OVWIDN-OFF
$B8 constant RBF.FLOOR-OFF
$C0 constant RBF.VDEFN-OFF
$C8 constant RBF.VPRSN-OFF
$D0 constant RBF.NDICT-OFF                 \ the dictionary's record count
$D8 constant RBF-REC
\ RBF.COORD distinguishes an ordinary checker scope (-1) from a declaration
\ frame's START tag. The declaration runner uses zero: declarations do not nest.
-1 constant RBF-NO-COORDINATOR

: RBF.UEND ( ptr a -- ptr a ) RBF.UEND-OFF + ;
: RBF.NEND ( ptr a -- ptr a ) RBF.NEND-OFF + ;
: RBF.SYMN ( ptr a -- ptr a ) RBF.SYMN-OFF + ;
: RBF.SYMU ( ptr a -- ptr a ) RBF.SYMU-OFF + ;
: RBF.CTN ( ptr a -- ptr a ) RBF.CTN-OFF + ;
: RBF.CTU ( ptr a -- ptr a ) RBF.CTU-OFF + ;
: RBF.LIN ( ptr a -- ptr a ) RBF.LIN-OFF + ;
: RBF.VRECN ( ptr a -- ptr a ) RBF.VRECN-OFF + ;
: RBF.VRECF ( ptr a -- ptr a ) RBF.VRECF-OFF + ;
: RBF.VRECND ( ptr a -- ptr a ) RBF.VRECND-OFF + ;
: RBF.VNARGN ( ptr a -- ptr a ) RBF.VNARGN-OFF + ;
: RBF.VRECU ( ptr a -- ptr a ) RBF.VRECU-OFF + ;
: RBF.CAND ( ptr a -- ptr a ) RBF.CAND-OFF + ;
: RBF.VSIG ( ptr a -- ptr a ) RBF.VSIG-OFF + ;
: RBF.PKGMODE ( ptr a -- ptr a ) RBF.PKGMODE-OFF + ;
: RBF.PKGU ( ptr a -- ptr a ) RBF.PKGU-OFF + ;
: RBF.DFEREND ( ptr a -- ptr a ) RBF.DFEREND-OFF + ;
: RBF.PIMMN ( ptr a -- ptr a ) RBF.PIMMN-OFF + ;
: RBF.COORD ( ptr a -- ptr a ) RBF.COORD-OFF + ;
: RBF.OVNDICT ( ptr a -- ptr a ) RBF.OVNDICT-OFF + ;
: RBF.OVLOG ( ptr a -- ptr a ) RBF.OVLOG-OFF + ;
: RBF.OVCP ( ptr a -- ptr a ) RBF.OVCP-OFF + ;
: RBF.OVWIDN ( ptr a -- ptr a ) RBF.OVWIDN-OFF + ;
: RBF.FLOOR ( ptr a -- ptr a ) RBF.FLOOR-OFF + ;
: RBF.VDEFN ( ptr a -- ptr a ) RBF.VDEFN-OFF + ;
: RBF.VPRSN ( ptr a -- ptr a ) RBF.VPRSN-OFF + ;
: RBF.NDICT ( ptr a -- ptr a ) RBF.NDICT-OFF + ;

16 constant RBF-CAP-INIT
variable RBF-CAP-V   RBF-CAP-INIT RBF-CAP-V !
create RBF-A-BOOT      RBF-CAP-INIT RBF-REC * allot
PERSISTED-PTR-VARIABLE RBF-A-P       RBF-A-BOOT RBF-A-P !
: RBF-BASE ( -- ptr n ) RBF-A-P @ ;
PACKAGE-PATH:CAP CK-USE-SNAP-BYTES + constant RBF-NAME-REC
create RBF-NAME-BOOT   RBF-CAP-INIT RBF-NAME-REC * allot
PERSISTED-PTR-VARIABLE RBF-NAME-P    RBF-NAME-BOOT RBF-NAME-P !
: RBF-NAME-BASE ( -- ptr u8 ) RBF-NAME-P @ ;
variable RBF-DEPTH   0 RBF-DEPTH !

: RBF-GROW ( -- )
   RBF-CAP-V @ 2 * {: nc:n :}
   RBF-A-P    RBF-CAP-V @ RBF-REC *              nc RBF-REC *              REG-GROW1
   RBF-NAME-P RBF-CAP-V @ RBF-NAME-REC *  nc RBF-NAME-REC *  REG-GROW1
   nc RBF-CAP-V ! ;
: RBF-ENSURE ( -- )
   RBF-DEPTH @ RBF-CAP-V @ < IF exit THEN
   RBF-GROW ;
: RBF-CUR ( -- ptr n )       RBF-DEPTH @ RBF-REC * RBF-BASE + ;
: RBF-NAME-CUR ( -- ptr u8 )  RBF-DEPTH @ RBF-NAME-REC * RBF-NAME-BASE + ;

\ An import savepoint: the row count, then each row's length and name bytes.
: CK-USE-SAVE ( ptr u8 n -- ) {: dst:ptr u:n :}
   u dst cell-view !
   u 0 ?DO
      dst CK-USE-SNAP-HEADER + i CK-USE-SNAP-ROW * + {: row:ptr :}
      CK-USE-LENS i cells + @ {: len:n :}
      len row cell-view !
      CK-USE-NAMES i PACKAGE-PATH:CAP * + row CELL + len ARENA-COPY
   LOOP ;

: CK-USE-RESTORE ( ptr u8 -- ) {: src:ptr :}
   src cell-view @ 0 ?DO
      src CK-USE-SNAP-HEADER + i CK-USE-SNAP-ROW * + {: row:ptr :}
      row cell-view @ {: len:n :}
      len CK-USE-LENS i cells + !
      row CELL + CK-USE-NAMES i PACKAGE-PATH:CAP * + len ARENA-COPY
   LOOP ;

\ The byte half of each scope owns the package name and reachable import rows.
\ Nested neutral scopes may overwrite any prefix, not just append imports.
: RBF-NAME-SAVE ( ptr u8 -- ) {: dst:ptr :}
   CHECKER-PACKAGE-NAME dst CHECKER-PACKAGE-U @ ARENA-COPY
   dst PACKAGE-PATH:CAP + CK-USE-SCAN-N CK-USE-SAVE ;

: RBF-NAME-RESTORE ( ptr u8 n -- ) {: src:ptr u:n :}
   src CHECKER-PACKAGE-NAME u ARENA-COPY
   src PACKAGE-PATH:CAP + CK-USE-RESTORE ;

: RBF-SAVE-INTO ( ptr n -- ) {: r:ptr :}
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
   CHECKER-OVERLAY:MARKS
   r RBF.OVWIDN !  r RBF.OVCP !  r RBF.OVLOG !  r RBF.OVNDICT !
   DFER-END @ r RBF.DFEREND !
   PIMM-N @ r RBF.PIMMN !
   PASS-FLOOR @ r RBF.FLOOR !
   VERIFY-DEFINER-N @ r RBF.VDEFN !
   VERIFY-PARSES-N @ r RBF.VPRSN !
   ndict@ r RBF.NDICT !
   RBF-NO-COORDINATOR r RBF.COORD ! ;

\ A record made inside a scope keeps no facts text past it: the checker binds a
\ name it does not know through its record's text (CHECK:READ-BOUND), so the
\ text would bring back a name the scope retired.
: RBF-FACTS-FORGET ( n -- )
   ndict@ swap ?do 0 i record-facts! loop ;

: RBF-RESTORE-FROM ( ptr n -- ) {: r:ptr :}
   r RBF.OVNDICT @  r RBF.OVLOG @  r RBF.OVCP @  r RBF.OVWIDN @
   CHECKER-OVERLAY:ROLLBACK
   r RBF.UEND @ USIGS-RESTORE-END
   r RBF.NEND @ NORET-RESTORE-END
   r RBF.SYMN @ r RBF.SYMU @ SYM-REWIND
   r RBF.CTN @ r RBF.CTU @ CT-REWIND
   r RBF.LIN @ LIN-NDECL !
   r RBF.VRECN @ r RBF.VRECF @ r RBF.VRECND @ r RBF.VNARGN @ r RBF.VRECU @ VREC-REWIND
   r RBF.CAND @ CHK-CAND !
   r RBF.VSIG @ VSIG !
   r RBF.PKGMODE @ CHECKER-PACKAGE-MODE !
   r RBF.PKGU @ CHECKER-PACKAGE-U!
   r RBF.FLOOR @ PASS-FLOOR !
   r RBF.VDEFN @ VERIFY-DEFINER-N !
   r RBF.VPRSN @ VERIFY-PARSES-N !
   r RBF.DFEREND @ DFER-REWIND
   r RBF.PIMMN @ PIMM-REWIND ;

: RBF-PUSH ( -- )
   RBF-ENSURE
   RBF-CUR RBF-SAVE-INTO
   RBF-NAME-CUR RBF-NAME-SAVE
   RBF-DEPTH @ 1 + RBF-DEPTH !
   CHECKER-DECL-FRAME:EXT-SAVE ;

: RBF-POP-WITH ( [ -- ] -- )
   0 BWIN-VALID !  0 UWIN-VALID !
   execute
   RBF-DEPTH @ CHECKER-OVERLAY:CLOSE-SCOPE
   RBF-DEPTH @ 1 - RBF-DEPTH !
   RBF-NAME-CUR RBF-CUR RBF.PKGU @ RBF-NAME-RESTORE
   RBF-CUR RBF-RESTORE-FROM
   RBF-CUR RBF.NDICT @ RBF-FACTS-FORGET ;

: RBF-POP ( -- )
   [: CHECKER-DECL-FRAME:EXT-RESTORE ;] RBF-POP-WITH ;

: RBF-FINALIZE ( -- )     \ keep every published row and release the live savepoint
   CHECKER-DECL-FRAME:EXT-FINALIZE
   RBF-DEPTH @ CHECKER-OVERLAY:CLOSE-SCOPE
   RBF-DEPTH @ 1 - RBF-DEPTH ! ;

package CHECKER-DECL-FRAME

: FRAME ( n -- ptr n )
   RBF-REC * RBF-BASE + ;

: TOP ( -- ptr n )
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
   TOP {: r:ptr :}
   depth r RBF.COORD ! ;

: PREPARE ( n -- bool ) {: depth:n :}
   depth TOP-MATCH? 0= IF 0 0= 0= EXIT THEN
   RBF-DEPTH @ TYPES-READY ;

: FRAME-MISMATCH ( -- )
   s" checker: declaration rollback frame mismatch" 76 die ;

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

: CHECKER-SCOPE-START ( -- )
   RBF-PUSH ;

\ The scope opener for replaying STANDALONE source. CHECKER-SCOPE-START keeps
\ the caller's package mirror live, right for checking the caller's own code; a
\ separate file's package context is whatever it declares, so this scope starts
\ at top level: no open package and no imports.
: CHECKER-SCOPE-START-NEUTRAL ( -- )
   RBF-PUSH
   [: RES-TRUE RBF-DEPTH @ CHECKER-OVERLAY:OPEN-SCOPE ;] catch {: rc:n :}
   rc 0= IF EXIT THEN
   RBF-POP
   rc throw ;

: CHECKER-SCOPE-FINALIZE ( -- )
   RBF-FINALIZE ;

: CHECKER-SCOPE-DONE ( -- )
   RBF-POP ;

: CHECKER-SCOPE-DEPTH ( -- n )
   RBF-DEPTH @ ;

: CHECK-CANDIDATE-START ( -- )
   WRITE-WINDOW-CK
   CHK-CAND @ 0= IF UEND @ CK-PEND-FLOOR !  ndict@ CK-PEND-IX ! THEN   \ the outermost scope's floor
   RBF-PUSH
   -1 CHK-CAND !
   -1 VSIG !
   0 PIMM-STREAM ! ;   \ candidate text is RAW source: parsing-immediate payloads present

: CHECK-CANDIDATE-DONE ( n -- n )
   -1 PIMM-STREAM !    \ back to the engine-stream default (hook loads)
   0 BIND-HORIZON !
   WRAP-CLEAR          \ a candidate body armed like any other; the next real record is not its heir
   RBF-POP ;

CHECKER-REG:SCRATCH{
PTR-VARIABLE CAND-A   variable CAND-U
CHECKER-REG:}SCRATCH
variable CAND-VERDICT

: CHECKER-CANDIDATE-SCOPE-START ( -- )
   CHECK-CANDIDATE-START ;

: CHECKER-CANDIDATE-SCOPE-DONE ( -- )
   0 CHECK-CANDIDATE-DONE drop ;

: CHECK-WINDOWED ( ptr u8 n -- n )
   {: a:ptr u:n :}
   WRITE-WINDOW-CK
   a u CHECK ;

: CHECK-CANDIDATE-BODY ( -- )
   CAND-A @ CAND-U @ CHECK-WINDOWED CAND-VERDICT ! ;

: CHECK-CANDIDATE! ( ptr u8 n -- n ) {: a:ptr u:n :}
   WRITE-WINDOW-CK
   a CAND-A !  u CAND-U !
   CHECKER-EFFECT-AUTHORITY:RECOVERY-USED? {: recovery0:bool :}
   BIND-HORIZON @ {: horizon0:n :}
   CHK-PROBE @ {: probe0:n :}
   PU-N @ {: pu0:n :}
   CUR-DONE @ {: done0:n :}
   CUR-GAP @ {: gap0:n :}
   CHECK-CANDIDATE-START
   -1 CHK-PROBE !
   -1 CUR-DONE !                       \ a candidate's text is not the cursor's
   [: CHECK-CANDIDATE-BODY ;] catch {: rc:n :}
   done0 CUR-DONE !
   gap0 CUR-GAP !
   probe0 CHK-PROBE !
   0 CHECK-CANDIDATE-DONE drop
   pu0 PU-N !                          \ a candidate's uses are never published
   recovery0 CHECKER-EFFECT-AUTHORITY:RECOVERY-USED!
   horizon0 BIND-HORIZON !
   rc 0 <> IF rc throw THEN
   CAND-VERDICT @ ;

package CHECKER-PREFIX

PTR-VARIABLE SOURCE-A
variable SOURCE-U
variable ANSWER

: SCAN ( -- )
   0 CHECKER-PREFIX-MISS !
   CHECKER-PREFIX-CUT @ CHECKER-PREFIX-ROLLBACK !
   SOURCE-A @ SOURCE-U @ CHECK-RESET
   s" " CHECK-SCAN                     \ a prefix: no closer
   CHECKER-PREFIX-MISS @ 0= ANSWER ! ;

public

\ Ask only whether the newly captured part binds. Open control, quotation and
\ local scopes remain prefixes; the declared result and publication belong to
\ the caller's eventual `;`. No immediate word runs during this read.
: BIND? ( ptr u8 n n -- n bool ) {: a:ptr u:n cut:n :}
   WRITE-WINDOW-CK
   a SOURCE-A !  u SOURCE-U !  cut CHECKER-PREFIX-CUT !
   CHECKER-EFFECT-AUTHORITY:RECOVERY-USED? {: recovery0:bool :}
   BIND-HORIZON @ {: horizon0:n :}
   CHK-PROBE @ {: probe0:n :}
   PU-N @ {: pu0:n :}
   CUR-DONE @ {: done0:n :}
   CUR-GAP @ {: gap0:n :}
   CHECK-CANDIDATE-START
   -1 CHK-PROBE !
   -1 CUR-DONE !                    \ a prefix's text is not the cursor's
   -1 PIMM-STREAM !                 \ the engine's body capture has spent parser operands
   -1 CHECKER-PREFIX-ACTIVE !
   [: SCAN ;] catch {: rc:n :}
   0 CHECKER-PREFIX-ACTIVE !
   done0 CUR-DONE !
   gap0 CUR-GAP !
   probe0 CHK-PROBE !
   0 CHECK-CANDIDATE-DONE drop
   pu0 PU-N !                       \ a prefix's uses are never published
   recovery0 CHECKER-EFFECT-AUTHORITY:RECOVERY-USED!
   horizon0 BIND-HORIZON !
   rc 0 <> IF rc throw THEN
   CHECKER-PREFIX-ROLLBACK @ ANSWER @ ;

;package

package CHECKER-REG
' SIG-COUNTED? DECLARATIONS CHECKER-OWNER-ABI:SIG-COUNTED-OFF + xt!
;package

\ The quiet probe: a caller that wants a verdict without a rejection diagnostic
\ asks here, so the render counter stays inside this file.
CHECKER-REG:SCRATCH{
PTR-VARIABLE QCAND-A   variable QCAND-U   variable QCAND-VERDICT
CHECKER-REG:}SCRATCH

: QCAND-BODY ( -- )
   QCAND-A @ QCAND-U @ CHECK-CANDIDATE! QCAND-VERDICT ! ;

: CHECK-QUIET-CANDIDATE! ( ptr u8 n -- n ) {: a:ptr u:n :}
   WRITE-WINDOW-CK
   a QCAND-A !  u QCAND-U !
   1 DIAG-QUIET +!
   [: QCAND-BODY ;] catch {: rc:n :}
   -1 DIAG-QUIET +!
   rc 0 <> IF rc throw THEN
   QCAND-VERDICT @ ;

PTR-VARIABLE CK-DEF-A   variable CK-DEF-U   variable CK-DEF-VERDICT

: CHECK-DEF-BODY ( -- )
   CK-DEF-A @ CK-DEF-U @ CHECK-WINDOWED CK-DEF-VERDICT ! ;

\ The code of the last refusal that rendered its packet and stopped a load by its
\ throw, 0 before one.
variable DEF-STOPPED

: CHECKER-DEF-STOPPED? ( n -- bool ) DEF-STOPPED @ = ;

: DEF-REFUSED ( n -- )
   {: rc:n :}
   rc DEF-REFUSAL @ <> IF rc throw THEN
   MULTI-ERR? 0= IF rc DEF-STOPPED ! rc throw THEN
   0 LAYOUT-XPORT !  0 BIND-HORIZON ! ;

: CHECK-DEF-THREW ( n -- )
   0 PARSE-COMPLETE !
   DEF-REFUSED
   MULTI-REFUSED
   0 CK-DEF-VERDICT ! ;

\ Like CHECK, but verifies the body against a leading ( in -- out ) declared
\ sig and rejects on mismatch.
: CHECK! ( ptr u8 n -- n ) {: a:ptr u:n :}
   WRITE-WINDOW-CK
   0 BWIN-VALID !  0 UWIN-VALID !
   CHECKER-REPORT-RESET
   -1 VSIG !
   a CK-DEF-A !  u CK-DEF-U !  0 DEF-REFUSAL !
   TORDER-ACTIVE @ TORDER-SCAN-UNKNOWN @ {: oldactive:n oldunknown:n :}
   1 TORDER-DEPTH +!
   TORDER-CHECKING TORDER-ACTIVE !  0 TORDER-SCAN-UNKNOWN !
   PU-N @ {: pu0:n :}
   CUR-TAKE {: took:bool :}
   [: CHECK-DEF-BODY ;] catch {: rc:n :}
   NULL-PTR CK-DEF-A !  0 CK-DEF-U !
   -1 TORDER-DEPTH +!
   TORDER-SCAN-UNKNOWN @ {: unknown:n :}
   oldactive TORDER-ACTIVE !  oldunknown TORDER-SCAN-UNKNOWN !
   pu0 took PUBLISH-EXIT {: prc:n :}   \ the body's uses, its resolved prefix if it
   rc 0 <> IF rc CHECK-DEF-THREW THEN  \ stopped, and its candidates; the arm goes either way
   prc 0 <> IF prc throw THEN
   CK-DEF-VERDICT @ {: verdict:n :}
   0 VSIG !
   CHECKER-TAPE:ARMED @ 0 <> {: recording:bool :}
   unknown 0= recording and IF a u verdict CHECKER-TAPE:DONE THEN
   verdict -1 = recording and BWIN-UNJUDGED @ 0= and BWIN-VALID !
   verdict ;

\ An uncheckable verdict is rendered here unless CHECK rendered it: as JSON, in
\ a multi-error load, or not at all in a quiet probe.
: REPORT-UNCHECKABLE ( n -- n )
   dup 1 = JSON-DIAGS @ 0= and MULTI-ERR? 0= and DIAG-QUIET @ 0= and
   if dup DVERD !  DIAG-PRINT then ;

\ A TRUSTED: definition's declared effect is the assertion: the body is scanned
\ only to fill the source tape the optimizing elaborator reads, and its verdict
\ is never acted on. A scan that returns leaves the clause latch as it found it:
\ its record grants no authority. A scan that throws abandons the definition,
\ so it clears the latch: a restored arm would be stepped by the next body
\ check into the record of whatever is defined next.
CHECKER-REG:SCRATCH{
PTR-VARIABLE UNJ-A   variable UNJ-U
variable UNJ-VERDICT
CHECKER-REG:}SCRATCH

: CHECK-UNJUDGED-BODY ( -- )
   UNJ-A @ UNJ-U @ CHECK! UNJ-VERDICT ! ;

: CHECK-UNJUDGED! ( ptr u8 n -- n ) {: a:ptr u:n :}
   WRITE-WINDOW-CK
   a UNJ-A !  u UNJ-U !
   DOESEFF @ DOESPEND @ {: eff:n pend:n :}
   -1 BWIN-UNJUDGED !
   1 DIAG-QUIET +!
   [: CHECK-UNJUDGED-BODY ;] CHECKER-EFFECT-AUTHORITY:SCAN {: rc:n :}
   -1 DIAG-QUIET +!
   0 BWIN-UNJUDGED !
   rc 0 <> IF DOES-EFF-CLEAR rc throw THEN
   eff DOESEFF !  pend DOESPEND !
   CHECKER-TAPE:ARMED @ 0= 0=  LMODE @ 0= and  #CFC @ 0= and
   CONM @ 0= and  CHECKER-MATCH-MODE @ 0= and  MF-DEPTH @ 0= and
   FIELD-LOAN-PENDING @ 0= and PARSE-COMPLETE @ 0= 0= and UWIN-VALID !
   UNJ-VERDICT @ ;

package CHECKER-PREFLIGHT

: RUN ( -- )
   BODY-A@ BODY-U @ CHECK! VERDICT ! ;

: RESET ( -- )
   0 ACTIVE !
   NULL-PTR BODY-A!  0 BODY-U !
   NULL-PTR TARGET-A!  0 TARGET-U ! ;

public

\ Keep this target-forcing entry distinct from the source checker CHECK! that
\ RUN calls above. Its published name must not shadow that call on replay.
: CHECK-TARGET! ( ptr u8 n ptr u8 n -- n )
   {: ba:ptr bu:n ta:ptr tu:n :}
   WRITE-WINDOW-CK
   ba BODY-A!  bu BODY-U !  ta TARGET-A!  tu TARGET-U !
   -1 ACTIVE !
   [: RUN ;] catch {: rc:n :}
   RESET
   rc 0 <> IF rc throw THEN
   VERDICT @ ;

;package

: DOES-DIN ( n -- n )
   FRESH MK-VAR MK-PTR swap MK-PUSH ;

\ Capture both rows' physical widths before checking binds either row tail;
\ they are read after the scan, so they are taken by value.
CHECKER-REG:SCRATCH{
variable CD-DIN-CELLS
variable CD-DOUT-CELLS
CHECKER-REG:}SCRATCH

: CHECK-DOES-EFFECT! ( n n -- ) {: din:n dout:n :}
   din ROW-CELLS CD-DIN-CELLS !
   dout ROW-CELLS CD-DOUT-CELLS ! ;

: CHECK-DOES-DIN-CELLS ( -- n ) CD-DIN-CELLS @ ;
: CHECK-DOES-DOUT-CELLS ( -- n ) CD-DOUT-CELLS @ ;

: RAW-SIG! ( n n n n -- )
   PD-BASE @ SGDBASE !
   SGROUT !  SGRIN !  SGOUT !  SGIN !   \ no `|` anywhere: both return rows are 0
   SGHASR @ IF SGRIN @ ELSE RBROW @ THEN SGRBASE !   \ no `|`: the implicit base is the sealed tail
   ;

\ Certifies `cast:` and `linear:` for both front ends, the engine's reader
\ keyword and verify-source's pre-pass.
package CHECKER-IDENTITY
\ A row that did not parse is refused before a gate walks it; family tails read
\ the engine's open namespace.
: PARSE ( ptr u8 n ptr u8 n -- ) {: na:ptr nu:n sa:ptr su:n :}
   sa su BYTE-SPAN? 0= IF
      s" the signature's length describes no memory" E-CAST-ARITY CAST-ERROR:RAISE
   THEN
   NEW
   SGBAD-CLEAR
   sa su PARSE-SIG-RAW RAW-SIG!
   SGBAD-UNKNOWN? IF
      SGBAD-A @ SGBAD-U @ CAST-ERROR:TOKEN!  s" unknown type" E-CAST-FAM CAST-ERROR:RAISE
   THEN
   SGBAD-ARITY? IF
      SGBAD-A @ SGBAD-U @ CAST-ERROR:TOKEN!
      s" wrong arity for type family" E-CAST-FAM CAST-ERROR:RAISE
   THEN
   SGBAD @ SGBAD-UNSUPPLIED? 0= and IF
      sa su na nu CAST-ERROR:REFUSE-SIGNATURE
   THEN ;

\ An unsupplied output variable, a view pack's shape CAST sends elsewhere, is a
\ bad row here.
: ROWS ( ptr u8 n ptr u8 n -- ) {: na:ptr nu:n sa:ptr su:n :}
   SGBAD-UNSUPPLIED? IF sa su na nu CAST-ERROR:REFUSE-SIGNATURE THEN
   SGHASR @ 0 <> IF
      s" the signature declares a return-stack effect" E-CAST-ARITY CAST-ERROR:RAISE
   THEN
   SGIN @ CAST-ROW-1? 0= IF
      s" the input is not one term over the stack below" E-CAST-ARITY CAST-ERROR:RAISE
   THEN
   SGOUT @ CAST-ROW-1? 0= IF
      s" the output is not one term over the stack below" E-CAST-ARITY CAST-ERROR:RAISE
   THEN
   SGIN @ R-RES P>REST R-RES
   SGOUT @ R-RES P>REST R-RES <> IF
      s" the input and output rows end in different tails" E-CAST-ARITY CAST-ERROR:RAISE
   THEN
   SIG-UNRES @ IF E-CHECKER-IDENTITY-UNRESOLVED throw THEN ;

\ A variable STORED-DERIVE left stored binds no view and no linear value, so a
\ cast through it hands out neither: TV! checks every binding.
: STORED-RAISE ( -- )
   0 BEGIN dup 26 < WHILE
      dup cells NMAP + @
      dup 0 >= IF
         dup TVK@ TVK-STORED = IF dup NSCOPE-RAISE dup NLINEAR-RAISE THEN
      THEN drop
      1 +
   REPEAT drop ;

\ An unscoped cast that mints nothing is a representation cast: its pending
\ record takes DNAME-REP, and the compiler lowers it as identity.
TRUSTED: MARK-REP ( n -- ) rep-mark ;
public
\ Refuse first, register second: a refusal throws before the engine counts the
\ record, so it leaves no name.
: CAST ( ptr u8 n ptr u8 n -- ) {: na:ptr nu:n sa:ptr su:n :}
   na nu sa su PARSE
   CAST-SCOPED? IF
      VIEW-CAST-CERTIFY
      na nu CHECKER-QUALIFIED? IF
         s" a qualified name publishes past the private section"
         E-CAST-SCOPE CAST-ERROR:RAISE
      THEN
      SIG-UNRES @ IF E-CHECKER-IDENTITY-UNRESOLVED throw THEN
      sa su na nu CHECKER-USIG-CERT-ROWS
   ELSE
      na nu sa su ROWS
      STORED-DERIVE STORED-RAISE
      CAST-CERTIFY {: mint:bool :}
      mint na nu CHECKER-QUALIFIED? and IF
         s" a qualified name publishes past the private section"
         E-CAST-MINT CAST-ERROR:RAISE
      THEN
      sa su na nu CHECKER-USIG-CERT-ROWS
      mint 0= CK-LIVE-PEND? and IF ndict@ MARK-REP THEN
   THEN
   CHECKER-REC-SYM @ CK-CLOSE! ;

\ A `linear:` record takes no DNAME-REP: minting or erasing a token changes
\ its owner, never a scalar's representation.
: LINEAR ( ptr u8 n ptr u8 n -- ) {: na:ptr nu:n sa:ptr su:n :}
   na nu sa su PARSE
   na nu sa su ROWS
   LINEAR-CERTIFY
   na nu CHECKER-QUALIFIED? IF
      s" a qualified name publishes past the private section"
      E-LINEAR-SCOPE CAST-ERROR:RAISE
   THEN
   sa su na nu CHECKER-USIG-CERT-ADD
   CHECKER-REC-SYM @ CK-CLOSE! ;
;package
: CHECKER-DEFCAST ( ptr u8 n ptr u8 n -- )
   s" cast:" [: CHECKER-IDENTITY:CAST ;] CAST-ERROR:DECLARE ;
: CHECKER-LINEAR ( ptr u8 n ptr u8 n -- )
   s" linear:" [: CHECKER-IDENTITY:LINEAR ;] CAST-ERROR:DECLARE ;
package CHECKER-REG
' CHECKER-DEFCAST DECLARATIONS CAST-OFF + xt!
' CHECKER-LINEAR DECLARATIONS LINEAR-OFF + xt!
;package

\ A candidate scope latches nothing: its records are truncated when it ends, so
\ a stored offset would name a record that no longer exists.
: DOES-EFF-LATCH! ( ptr u8 n -- ) {: sa:ptr su:n :}
   DOES-EFF-CLEAR
   DVERD @ VERDICT-CHECKED <> IF EXIT THEN
   CHK-CAND @ 0 <> IF EXIT THEN
   sa su CREATED-RECORD-BUILD
   {: rec:n :}
   rec 0= IF EXIT THEN                       \ unresolvable family: no row, no latch
   rec DOES-EFF-ARM ;

\ A created word declared `( in -- out )` needs a DOES> body typing as
\ `( in ptr a -- out )`, since the CREATE stub pushes the data-field address
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
   RAW-SIG!
   SG-ROWS-COLLECT
   NP-COLLECT
   SGIN @ DOES-DIN dup BROW !
   SGOUT @ CHECK-DOES-EFFECT!
   \ The record is the signature's as written, whatever the runtime row below
   \ measures; a refused or uncheckable clause keeps it for its packet.
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
   FIELD-LOAN-PENDING @ IF SCOPE-FIELD-REJECT THEN
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
   CHECKER-TAPE:ARMED @ IF ba bu DVERD @ CHECKER-TAPE:DONE THEN ;

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
PTR-VARIABLE UNJ-DOES-BA   variable UNJ-DOES-BU
PTR-VARIABLE UNJ-DOES-SA   variable UNJ-DOES-SU   variable UNJ-DOES-VERDICT

: UNJ-DOES-BODY ( -- )
   UNJ-DOES-BA @ UNJ-DOES-BU @ UNJ-DOES-SA @ UNJ-DOES-SU @ CHECK-DOES-RUN
   UNJ-DOES-VERDICT ! ;

: CHECK-DOES-UNJUDGED! ( ptr u8 n ptr u8 n -- n ) {: ba:ptr bu:n sa:ptr su:n :}
   ba UNJ-DOES-BA !  bu UNJ-DOES-BU !  sa UNJ-DOES-SA !  su UNJ-DOES-SU !
   -1 BWIN-UNJUDGED !
   [: UNJ-DOES-BODY ;] CHECKER-EFFECT-AUTHORITY:SCAN {: rc:n :}
   0 BWIN-UNJUDGED !
   rc 0 <> IF rc throw THEN
   sa su DOES-EFF-LATCH!
   UNJ-DOES-VERDICT @ ;

\ The pre-verifier's entry also reports what it refuses, since CHECK-DOES-RUN
\ renders nothing. An uncheckable clause is rendered here too: the pre-pass
\ renders nothing of its own for a clause.
: DOES-NAME! ( ptr u8 n -- ) {: na:ptr nu:n :}
   na nu TOKFOLD drop
   TKF nu CLAUSE-NAME NMU !  NMA ! ;

: DOES-REPORT ( n ptr u8 n -- n ) {: v:n na:ptr nu:n :}
   v -1 = v 2 = or IF v EXIT THEN
   na nu DOES-NAME!
   v DVERD !
   DIAG-QUIET @ 0= IF DIAG-PRINT THEN
   SIG-EFF-DROP
   MULTI-ERR? DIAG-QUIET @ 0= and IF 1 MULTI-ERR-N +! THEN
   v ;

\ Both compilers report a clause their check refused the same way, but they name
\ the definer only after the scan, so the token buffers are sized for the clause
\ name here. A grown buffer starts empty and the old one stays mapped, so the
\ refused token is carried over from it.
: CHECKER-NATIVE-DOES-REPORT ( n ptr u8 n -- n ) {: v:n na:ptr nu:n :}
   FAILTK {: tk:ptr :}
   nu DOES-CLAUSE:SUFFIX$ nip + TOKBUF-ENSURE
   FAILTK tk <> IF tk FAILTK FAILTU @ CCOPY THEN
   v na nu DOES-REPORT ;

\ A clause is inside its definer's definition, so a refusal of the definition
\ that leaves the clause's walk by a throw is, in a multi-error load, a refused
\ clause like one its verdict refuses. Its packet, rendered where the refusal
\ was found, is its diagnostic.
PTR-VARIABLE SRC-DOES-BA   variable SRC-DOES-BU
PTR-VARIABLE SRC-DOES-SA   variable SRC-DOES-SU   variable SRC-DOES-VERDICT

: SRC-DOES-BODY ( -- )
   SRC-DOES-BA @ SRC-DOES-BU @ SRC-DOES-SA @ SRC-DOES-SU @ CHECK-DOES-RUN
   SRC-DOES-VERDICT ! ;

: SRC-DOES-THREW ( n -- n )
   SIG-EFF-DROP
   DEF-REFUSED
   WRAP-CLEAR
   1 MULTI-ERR-N +!
   0 ;

\ The clause's name is written after the scan, so the token buffers are sized
\ for it first: growing them afterwards would drop the refused token being
\ reported.
: CHECKER-SOURCE-DOES! ( ptr u8 n ptr u8 n ptr u8 n -- n )
   {: ba:ptr bu:n sa:ptr su:n na:ptr nu:n :}
   WRITE-WINDOW-CK
   nu DOES-CLAUSE:SUFFIX$ nip + TOKBUF-ENSURE
   ba SRC-DOES-BA !  bu SRC-DOES-BU !  sa SRC-DOES-SA !  su SRC-DOES-SU !
   0 DEF-REFUSAL !
   TORDER-ACTIVE @ TORDER-SCAN-UNKNOWN @ {: oldactive:n oldunknown:n :}
   1 TORDER-DEPTH +!
   TORDER-CHECKING TORDER-ACTIVE !  0 TORDER-SCAN-UNKNOWN !
   PU-N @ {: pu0:n :}
   CUR-TAKE {: took:bool :}
   [: SRC-DOES-BODY ;] catch {: rc:n :}
   -1 TORDER-DEPTH +!
   oldactive TORDER-ACTIVE !  oldunknown TORDER-SCAN-UNKNOWN !
   pu0 took PUBLISH-EXIT {: prc:n :}
   DOES-EFF-CLEAR
   rc 0 <> IF rc SRC-DOES-THREW  prc 0 <> IF prc throw THEN  EXIT THEN
   prc 0 <> IF prc throw THEN
   SRC-DOES-VERDICT @ {: v:n :}
   v VERDICT-CHECKED =  v VERDICT-DEFERRED =  or IF na nu CHECKER-SOURCE-CLAUSE THEN
   v na nu DOES-REPORT ;

: TOP-ROW? ( n ptr u8 n -- bool )
   {: sym:n a:ptr u:n :}
   sym CHECKER-FIND-USIG-SYM IF RES-TRUE EXIT THEN
   sym PRIM-FIRST-IDX 0 <> IF RES-TRUE EXIT THEN
   sym a u TOK-IDENTITY 0 <> ;

: TOP-REFUSE ( ptr u8 n n -- n )
   {: a:ptr u:n sel:n :}
   a USH-TOK-A !  u USH-TOK-U !
   sel SHADOW-DIAG-XT
   MULTI-ERR? IF 1 MULTI-ERR-N +! 0 EXIT THEN
   CHECKER-REJECT-RC throw ;

\ What the load does with a top-level token it runs, or ticks when RUNS is
\ false. The answer is -1 when it resolves, or 2 when it resolves to a word that
\ may read the source after it; 3 when it resolves to a word it runs whose
\ effect names a type that may arrive, or resolves nowhere but a rendering
\ statement in scope may define it, the run's to judge; and 0 when the load
\ refuses it.
: TOP-ANSWER ( ptr u8 n bool -- n )
   {: a:ptr u:n runs:bool :}
   a u CHECKER-RESOLVE:WALK {: sym:n why:n :}
   why 0 <> IF a u why CHECKER-RESOLVE:RAISE THEN
   CHECKER-QBAD-TOK @ 0= IF
      a u sym TOP-BOUND-SYM IF
         dup ARENA-CALL-DENIED? IF drop a u 6 TOP-REFUSE EXIT THEN
         dup TOP-CONTROL {: flags:n deferred:bool :}
         dup SYM-EFFECT-UNRESOLVED? IF
            drop flags deferred runs TOP-RUNS
            dup -1 = IF drop 3 THEN EXIT
         THEN
         runs 0=  over a u TOP-ROW?  or IF
            dup a u NAV-USE-NOW drop
            flags deferred runs TOP-RUNS EXIT
         THEN
      THEN drop
      a u UNSEEN-COVERS? IF 3 EXIT THEN
   THEN
   a u  CHECKER-QBAD-TOK @ IF 4 ELSE 3 THEN  TOP-REFUSE ;

PTR-VARIABLE TOP-ASK-A   variable TOP-ASK-U   variable TOP-ASK-RUNS
variable TOP-ASK-ANSWER

: TOP-ASK-BODY ( -- )
   TOP-ASK-A @ TOP-ASK-U @ TOP-ASK-RUNS @ TOP-ANSWER TOP-ASK-ANSWER ! ;

: CHECKER-VERIFY-TOP ( ptr u8 n bool -- n )
   {: a:ptr u:n runs:bool :}
   WRITE-WINDOW-CK
   a TOP-ASK-A !  u TOP-ASK-U !  runs TOP-ASK-RUNS !
   0 DEF-REFUSAL !
   [: TOP-ASK-BODY ;] catch {: rc:n :}
   rc 0 <> IF rc DEF-REFUSED  1 MULTI-ERR-N +!  0 EXIT THEN
   TOP-ASK-ANSWER @ ;

\ One rollback frame spans the head's scan, the clause's and publication; while
\ open, RBF-DEPTH stays nonzero so compaction relocates none of its cursors.
: CHECKER-NATIVE-DOES-BEGIN ( -- )
   RBF-PUSH  1 DOES-SPLIT ! ;

: CHECKER-NATIVE-DOES-PUBLISH ( ptr u8 n -- ) {: a:ptr u:n :}
   DOESPEND @ 1 <> IF DOES-EFF-CLEAR EXIT THEN
   DOESEFF @ {: creates:n :}
   DOES-EFF-CLEAR
   creates 0= IF EXIT THEN
   a u CHECKER-RECORD-SYM? {: sym:n :}
   sym CHECKER-FIND-USIG-SYM 0= IF EXIT THEN
   sym EFFECT-EXTERNAL-SYM? 0= IF EXIT THEN
   creates sym NORET-CREATES@ = IF EXIT THEN
   sym sym CTL-FLAGS-SYM sym CTL-MASKS-SYM creates NORET-ADD-CREATES
   sym VIS-SYM-REC sym RECORD-CONTROL-AT! ;

: CHECKER-NATIVE-DOES-FINISH ( ptr u8 n bool -- )
   {: a:ptr u:n committed:bool :}
   0 DOES-SPLIT !
   committed 0= IF
      DOES-EFF-CLEAR WRAP-CLEAR RBF-POP EXIT
   THEN
   a u CHECKER-NATIVE-DOES-PUBLISH ;

: CHECKER-NATIVE-DOES-COMMIT ( -- )
   RBF-FINALIZE ;
2 constant GENR-BADSIG                    \ the effect does not parse or no record
                                          \ holds it (SGBAD-UNFIT!); SGBAD says why

: GENERATES-REFUSE ( ptr u8 n n -- )
   GENR-DIAG
   MULTI-ERR? IF 1 MULTI-ERR-N +! EXIT THEN
   E-GENERATES-ROW throw ;

\ `already` is what the caller knows of D beyond the checker. Answers D's symbol
\ and the created word's record offset+1, 0 for a refused row.
: CHECKER-GENERATES ( ptr u8 n ptr u8 n bool -- n n )
   {: na:ptr nu:n sa:ptr su:n already:bool :}
   na nu CHECKER-FIND-ACTIVE-SYM
   {: sym:n :}
   sym 0= IF na nu GENR-UNRESOLVED GENERATES-REFUSE sym 0 EXIT THEN
   sym na nu NAV-USE-NOW
   already sym NORET-CREATES@ 0 <> or IF na nu GENR-CREATES GENERATES-REFUSE sym 0 EXIT THEN
   sym TOP-CONTROL {: flags:n deferred:bool :}
   flags CTL-PARSES and 0= deferred or IF na nu GENR-READS-NONE GENERATES-REFUSE sym 0 EXIT THEN
   sa su CREATED-RECORD-BUILD
   {: rec:n :}
   rec 0= IF na nu GENR-BADSIG GENERATES-REFUSE THEN
   sym rec ;

\ A signature is read as a definition head reads one. PARSE-SIG-RAW splits the
\ effect at spaces only, so a line feed inside it fuses the tokens around it.
: CHECKER-SIG-SPAN ( ptr u8 n -- n n )
   {: a:ptr u:n :}
   0 BEGIN dup u < IF a over + c@ 33 < ELSE RES-FALSE THEN WHILE 1 + REPEAT
   {: open:n :}
   open u = IF u 0 EXIT THEN
   a open + c@ 40 <> IF u 0 EXIT THEN
   open BEGIN dup u < IF a over + c@ 41 <> ELSE RES-FALSE THEN WHILE 1 + REPEAT
   {: close:n :}
   close u = IF open 0 EXIT THEN
   open close 1 + ;

\ The longest effect a row states. The source pre-verifier refuses a longer one
\ at the same bound.
$100 constant GENR-SIG-CAP

: GENR-SIG-READ ( ptr u8 -- ptr u8 n )
   {: at:ptr :}
   data-base INE-CELL + @  at NULL-PTR - -
   {: left:n :}
   at left CHECKER-SIG-SPAN
   {: open:n close:n :}
   open left = IF s" checker: generates: effect must open with (" 76 die THEN
   close 0= IF s" checker: generates: unterminated effect" 76 die THEN
   close open - 2 -
   {: su:n :}
   su GENR-SIG-CAP > IF s" checker: generates: effect too long" 76 die THEN
   at close + NULL-PTR - data-base INP-CELL + !
   at open + 1 +  su ;

\ generates: D ( effect ) at top level, after D's definition: a declared
\ boundary, not a checked body. UNSAFE-TOK? bars both `checker-generates` and
\ `generates:` from checked bodies.
TRUSTED: generates: ( -- )
   parse-name
   {: na:ptr nu:n :}
   nu 0= IF s" checker: generates: needs a definer name" 76 die THEN
   na nu + GENR-SIG-READ
   {: sa:ptr su:n :}
   na nu sa su RES-FALSE CHECKER-GENERATES
   {: sym:n rec:n :}
   rec 0= IF EXIT THEN
   BIND-REC @ {: selected:ptr :}
   sym sym CTL-FLAGS-SYM CTL-GENERATES or sym CTL-MASKS-SYM rec NORET-ADD-CREATES
   selected sym RECORD-CONTROL-AT! ;
7 constant PARSES-LIST                    \ no `(`, no terminator, or no `)`

: PARSES-REFUSE ( ptr u8 n n -- )
   PRS-DIAG
   MULTI-ERR? IF 1 MULTI-ERR-N +! EXIT THEN
   E-PARSES-ROW throw ;

: ROW-UNBOUND ( ptr u8 n -- n )
   {: a:ptr u:n :}
   a u CHECKER-RESOLVE:WALK nip {: why:n :}
   why E-USING-SHADOW-GLOBAL = IF PARSES-SHADOW EXIT THEN
   why E-USING-AMBIGUOUS = IF PARSES-AMBIGUOUS EXIT THEN
   why 0 <> IF a u why CHECKER-RESOLVE:RAISE -1 EXIT THEN
   CHECKER-QBAD-TOK @ IF PARSES-QUALIFIED EXIT THEN
   a u UNSEEN-COVERS? IF -1 EXIT THEN
   PARSES-UNDEFINED ;

: PARSES-UNBOUND ( ptr u8 n -- )
   {: a:ptr u:n :}
   a u ROW-UNBOUND {: kind:n :}
   kind 0 >= IF a u kind PARSES-REFUSE THEN ;

\ A deferred word stays opaque, whatever word is set in it, and so does a word
\ with no record (a primitive or an axiom), whose replacement no record tells
\ apart. A cold selected dictionary record can likewise have control but no
\ checker symbol to key a row: the row stands and bounds nothing until a checked
\ use materializes that binding.
: PARSES-TARGET ( ptr u8 n -- n n )
   {: a:ptr u:n :}
   a u TOP-BINDING {: sym:n eff1:n ctl:n :}
   sym 0= IF
      ctl CHECKER-OWNER-ABI:BINDING-DEFER and 0 <> IF 0 0 EXIT THEN
      ctl CTL-PARSES and 0 <> IF 0 0 EXIT THEN
      ctl 0 <> IF a u PARSES-READS-NONE PARSES-REFUSE 0 0 EXIT THEN
      a u PARSES-UNBOUND 0 0 EXIT
   THEN
   sym a u NAV-USE-NOW
   ctl CHECKER-OWNER-ABI:BINDING-DEFER and 0 <> IF 0 0 EXIT THEN
   ctl CTL-PARSES and 0= IF a u PARSES-READS-NONE PARSES-REFUSE 0 0 EXIT THEN
   eff1 0= IF 0 0 EXIT THEN
   sym eff1 ;

\ The checks every row passes, for the engine words and the source pre-verifier,
\ each of which has read the whole row first, so a row refused under
\ --all-errors leaves none of its tokens to what follows.
: CHECKER-PARSES-ROW ( ptr u8 n ptr u8 n ptr u8 n bool -- n n n )
   {: ka:ptr ku:n ta:ptr tu:n ca:ptr cu:n bad:bool :}
   tu 0= IF ka ku PARSES-NO-TARGET PARSES-REFUSE 0 0 0 EXIT THEN
   ca cu num-parse {: v:n flt:bool ok:bool :}
   ok 0=  flt or  v 0 <  or IF ta tu PARSES-COUNT PARSES-REFUSE 0 0 0 EXIT THEN
   bad IF ta tu PARSES-LIST PARSES-REFUSE 0 0 0 EXIT THEN
   ta tu PARSES-TARGET {: sym:n eff1:n :}
   sym eff1 v ;

\ What a list token is to its reader: 0 a terminator, 1 the closing `)`, 2 the
\ end of the input, where no `)` came.
: PARSES-ITEM ( ptr u8 n -- n )
   {: a:ptr u:n :}
   u 0= IF 2 EXIT THEN
   a u s" )" CORE-STR= IF 1 EXIT THEN
   0 ;

: PARSES-LIST-LOAD ( -- bool )
   parse-name s" (" CORE-STR= 0= IF RES-TRUE EXIT THEN
   0 BEGIN parse-name PARSES-ITEM dup 0= WHILE drop 1 + REPEAT
   {: terms:n end:n :}
   end 2 =  terms 0=  or ;

: NAMES-REFUSE ( ptr u8 n n -- )
   NMS-DIAG
   MULTI-ERR? IF 1 MULTI-ERR-N +! EXIT THEN
   E-NAMES-ROW throw ;

\ A record quiet TRUST left unresolved holds no row.
: STRING-IN? ( ptr u8 -- bool )
   {: rec:ptr :}
   rec ER.ACTIVE @ EFF-UNRESOLVED = IF RES-FALSE EXIT THEN
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
\ declared input row ends in the string pair ( ptr u8 n ); any other word, one
\ with no inputs among them, refuses the row, so a row on the wrong word cannot
\ make every string before it a use. A cold selected record with no checker
\ symbol to key the mark leaves the row standing and binding nothing.
: CHECKER-NAMES-ROW ( ptr u8 n ptr u8 n -- )
   {: ka:ptr ku:n ta:ptr tu:n :}
   tu 0= IF ka ku PARSES-NO-TARGET NAMES-REFUSE EXIT THEN
   ta tu TOP-BINDING-REC {: sym:n eff1:n ctl:n selected:ptr :}
   sym 0= IF
      ctl 0 <> IF EXIT THEN
      ta tu ROW-UNBOUND {: kind:n :}
      kind 0 >= IF ta tu kind NAMES-REFUSE THEN
      EXIT
   THEN
   sym ta tu NAV-USE-NOW
   sym eff1 NAMES-TAKES-STRING? 0= IF ta tu NAMES-NO-STRING NAMES-REFUSE EXIT THEN
   sym  sym CTL-FLAGS-SYM CTL-NAMES or  sym CTL-MASKS-SYM  NORET-ADD-SYM
   selected sym RECORD-CONTROL-AT! ;

\ PKG:TAIL is PKG's public TAIL, a bare name a global and never a package's own
\ word, and a name with a second colon none, though the checker's split reads
\ one whose first colon is at an edge as bare.
: NAMED-SYM ( ptr u8 n -- n )
   {: a:ptr u:n :}
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
   SPAY-B @ TADDR SPAY-U @ {: a:ptr u:n :}
   a u NAMED-SYM a u NAV-USE ;
: NAMED-OPERAND-INSTALL ( -- ) [: NAMED-OPERAND-USE ;] is NAMED-OPERAND-XT ;
NAMED-OPERAND-INSTALL

\ A top-level literal's body the source pre-verifier found before a call of a
\ marked word is a use of the word it names, published at once.
: CHECKER-NAMED-USE ( ptr u8 n -- )
   {: a:ptr u:n :}
   a u NAMED-SYM a u NAV-USE-NOW ;

\ ---- the interpret row: saved between top-level events -----------------------
0 constant TS.BASE   1 constant TS.RP    2 constant TS.RC   3 constant TS.WP
4 constant TS.WC     5 constant TS.OFF   6 constant TS.TV   7 constant TS.RV
8 constant TS.ENTRY  9 constant TS.LEVEL  10 constant TS.END  11 constant TS.CELLS
12 constant TS.ROFF
13 constant TS-ROW
PTR-VARIABLE TOP-SEG-P   variable TOP-SEG-CAP   variable TOP-SEG-N
variable TOP-BASE   variable TOP-ENTRY   variable TOP-LEVEL   variable TOP-LIVE
NULL-PTR TOP-SEG-P !  0 TOP-SEG-CAP !  0 TOP-SEG-N !  0 TOP-BASE !  0 TOP-LIVE !
0 TOP-ENTRY !  0 TOP-LEVEL !

: TS-AT ( n n -- ptr n ) {: ix:n fld:n :} TOP-SEG-P ix TS-ROW NAV-CELL fld cells + ;
package CHECKER-REG                       \ REG-PTR-CELLS is CHECKER-REG's private cast
public
: TS-PTR ( n n -- ptr ptr u8 ) TS-AT BYTE-VIEW REG-PTR-CELLS ;
;package

: TOP-SEG-UNMAP ( n -- ) {: ix:n :}
   ix TS.RP CHECKER-REG:TS-PTR @ CELL-VIEW ix TS.RC TS-AT @ cells ARENA-UNMAP
   ix TS.WP CHECKER-REG:TS-PTR @ CELL-VIEW ix TS.WC TS-AT @ cells ARENA-UNMAP ;

: TOP-SEG-RELEASE ( -- )
   TOP-READ-P TOP-READ-CAP 1 ARENA-ROWS-RELEASE
   TOP-WRITE-P TOP-WRITE-CAP 1 ARENA-ROWS-RELEASE
   0 TOP-ROW-OFF !  0 TOP-ROW-CELLS !  0 TOP-RROW-OFF ! ;

: TOP-SEG-PARK ( -- )
   TOP-SEG-P TOP-SEG-CAP TOP-SEG-N @ 1 + TS-ROW ARENA-ROWS-ENSURE
   TOP-SEG-N @ {: ix:n :}
   TOP-BASE @ ix TS.BASE TS-AT !
   TOP-ENTRY @ ix TS.ENTRY TS-AT !  TOP-LEVEL @ ix TS.LEVEL TS-AT !
   TOP-READ-P @ BYTE-VIEW ix TS.RP CHECKER-REG:TS-PTR !   TOP-READ-CAP @ ix TS.RC TS-AT !
   TOP-WRITE-P @ BYTE-VIEW ix TS.WP CHECKER-REG:TS-PTR !  TOP-WRITE-CAP @ ix TS.WC TS-AT !
   TOP-ROW-OFF @ ix TS.OFF TS-AT !  TOP-RROW-OFF @ ix TS.ROFF TS-AT !
   TOP-ROW-TV @ ix TS.TV TS-AT !  TOP-ROW-RV @ ix TS.RV TS-AT !
   TOP-READ-END @ ix TS.END TS-AT !  TOP-ROW-CELLS @ ix TS.CELLS TS-AT !
   ix 1 + TOP-SEG-N !
   NULL-PTR TOP-READ-P !  0 TOP-READ-CAP !
   NULL-PTR TOP-WRITE-P !  0 TOP-WRITE-CAP !  0 TOP-ROW-OFF !  0 TOP-ROW-CELLS !
   0 TOP-RROW-OFF ! ;

: TOP-SEG-UNPARK ( n -- ) {: at:n :}
   TOP-SEG-RELEASE
   TOP-SEG-N @ at 1 + ?DO i TOP-SEG-UNMAP LOOP
   at TS.BASE TS-AT @ TOP-BASE !
   at TS.ENTRY TS-AT @ TOP-ENTRY !  at TS.LEVEL TS-AT @ TOP-LEVEL !
   at TS.RP CHECKER-REG:TS-PTR @ TOP-READ-P !   at TS.RC TS-AT @ TOP-READ-CAP !
   at TS.WP CHECKER-REG:TS-PTR @ TOP-WRITE-P !  at TS.WC TS-AT @ TOP-WRITE-CAP !
   at TS.OFF TS-AT @ TOP-ROW-OFF !  at TS.ROFF TS-AT @ TOP-RROW-OFF !
   at TS.TV TS-AT @ TOP-ROW-TV !  at TS.RV TS-AT @ TOP-ROW-RV !
   at TS.END TS-AT @ TOP-READ-END !  at TS.CELLS TS-AT @ TOP-ROW-CELLS !
   at TOP-SEG-N ! ;

: TOP-SEG-SPANS? ( n n n -- bool ) {: ix:n base:n lvl:n :}
   ix TS.BASE TS-AT @ base =  ix TS.ENTRY TS-AT @ lvl <= and  lvl ix TS.LEVEL TS-AT @ <= and ;

: TOP-SEG-FIND ( n n -- n ) {: base:n lvl:n :}
   TOP-SEG-N @ BEGIN dup 0 > WHILE
      1 - dup base lvl TOP-SEG-SPANS? IF EXIT THEN
   REPEAT drop -1 ;

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
   TOK-SPELL-A @ TOK-SPELL-U @ {: a:ptr u:n :}
   CHECK-CLEAR
   BROW @ SGDBASE !
   u TOKBUF-ENSURE  a TBASE !  u TBLEN !  0 TOK0 !  u TI !  0 TSTART !
   0 CALL-HIT !  0 TOK-DONE !  0 BWIN-HIT !  0 STALE-HIT !
   NULL-PTR TOK-EFF !  NULL-PTR TOK-ABI-EFF !  0 TOK-ABI-DONE !
   a TOK-SPELL-A !  u TOK-SPELL-U !  -1 USING-HOLD !
   a u TOKFOLD drop
   a u CAP-FAIL
   0 EXEC-OPAQUE !  0 CATCH-OPAQUE !
   TKF TKFU @ s" xt!" CORE-STR= XT-DECL !
   TKF TKFU @ LAYOUT-XPORT-TOK? IF
      TKF TKFU @ TOK-CORE-OP?
   ELSE RES-FALSE THEN LAYOUT-XPORT ! ;

\ The spelling is tested first, so no other token is bound for it; a checked
\ step needs the test here, and the window tests it before it steps at all.
: TOP-WIDEN? ( -- bool )
   TKF TKFU @ s" evaluate" CORE-STR= 0= IF RES-FALSE EXIT THEN
   TKF TKFU @ TOK-CORE-OP? dup IF 1 TOP-LEVEL +! THEN ;

\ `evaluate` has no row: the text it reads steps itself, one level deeper on
\ this segment, so the step takes its string and leaves the rest of the row as
\ it was.
: TOP-WORD-STEP ( -- )
   TOP-BEGIN
   TOP-WIDEN? IF DCUR @ ROW-DROP-1 ROW-DROP-1 DCUR !  EXIT THEN
   TKF TKFU @ SPELLED-STEP? 0= IF
      DCUR @ XE-PD !  RCUR @ XE-PR !
      TKF TKFU @ DO-TOK
   THEN ;

\ `' W` pushes W's quotation when W has a row, else an opaque xt, as `['] W`
\ does in a body. W is the word the load's own find selects.
: TOP-TICK-STEP ( -- )
   TOP-BEGIN
   TKF TKFU @ CHECKER-TOP-SYM {: sym:n hit:bool :}
   hit IF sym ARENA-CALL-REFUSE? IF EXIT THEN THEN
   FEP-CLEAR
   hit IF sym CHECKER-FIND-USIG-SYM drop THEN
   FEP-HIT? IF FEP @ TICK-QUOT FEP @ USIG-SYM@ BTICK-EDGE ELSE OPAQUE-XT THEN
   TOP-PUSH ;

package SRCLOC
private
OWNED-CELL RESERVED-PTR-U8-CELL FILE-START
public

\ The cursor's line from the file's first owned byte; 0 outside file text.
: LINE ( -- n )
   FILE-BUFFER? 0= IF 0 EXIT THEN
   FILE-START @ CURSOR @ ERROR-TEXT:LINE ;
;package

package CHECK

private
\ FILE-POS's last answer: a byte offset into the owned bytes, its line, its column.
variable FPOS-OFF  variable FPOS-LN  variable FPOS-CL
public

\ Forgets FILE-POS's last answer, which counts over the owned bytes: every
\ publication of the owned bytes runs it.
: FILE-POS-CLEAR ( -- )
   0 FPOS-OFF !  1 FPOS-LN !  1 FPOS-CL ! ;
FILE-POS-CLEAR

\ The file line, column and byte of t, which lies in the bytes the loader owns,
\ counted as DIAG-POS counts from their first byte. It counts on from its last
\ answer when t is at or past it, so a file costs one pass.
: FILE-POS ( ptr u8 -- n n n )
   {: t:ptr :}
   SRC-OWNED @ {: b:ptr :}
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
;package

\ The wall's line and code for a duplicate refused off the wall.
package CHECK
private
\ The source tools classify $4E themselves.
$4E constant DUP-RC
INP-CELL RESERVED-PTR-U8-CELL SRC-CURSOR
PENDTKA-CELL RESERVED-PTR-U8-CELL SRC-NAME

\ The line of the definition's name, where the wall places a duplicate. At a
\ colon definition's closing `;` the definition is pending (PEND-CELL) and its
\ opener kept where its name starts (PENDTKA-CELL); create, variable and
\ constant run the hook with nothing pending, PENDTKA-CELL
\ still at the last colon definition's name and the cursor just past their own.
\ A name not between the file's first owned byte and the cursor is not counted
\ to, since the count would read outside the file: it is placed at the cursor.
: NAME-LINE ( -- n )
   SRC-OWNED @ SRC-NAME @ SRC-CURSOR @ {: o:ptr nm:ptr p:ptr :}
   data-base PEND-CELL + @ 0 <>  nm o - 0 >= and  p nm - 0 >= and
   if o nm ERROR-TEXT:LINE else SRCLOC:LINE then ;

public
: REFUSE-DUPLICATE ( ptr u8 n -- )
   {: a:ptr u:n :}
   s" E-DUPLICATE-DEFINITION" TOP-SITE-PATH$
   SRCLOC:FILE-BUFFER? if NAME-LINE else 0 then
   ERROR-TEXT:PREFIX
   s" duplicate definition: " ERROR-TEXT:TYPE
   a u ERROR-TEXT:TYPE  S\" \n" ERROR-TEXT:TYPE
   DUP-RC throw ;
;package

\ The token's verdict as DO-TOK1 and CHECK take it: a checked step runs, as
\ does one deferred to the run; a refused or uncheckable one renders the
\ diagnostic the same token gets in a body and throws the checker's rc, so the
\ word never runs.
: TOP-JUDGE ( -- )
   EXEC-OPAQUE @ IF MD-EXEC-OPAQUE MDIAG! THEN
   CATCH-OPAQUE @ IF MD-CATCH-OPAQUE MDIAG! THEN
   0 RAW-PTR-HIT !  0 BASE-PTR-HIT !  0 RAW-EXEC-HIT !  0 STALE-HIT !  0 SCOPE-HIT !  0 XT-DECL !
   0 LAYOUT-XPORT !
   CHECK-NO-BORROW
   OK @ 0= FAILSET @ 0= and IF -1 FAILSET ! THEN
   0 USING-HOLD !
   TOK-SPELL-A @ TOK-SPELL-U @ REFUSAL-STEP
   CHECK-VERDICT dup DVERD ! {: v:n :}
   v VERDICT-CHECKED =  v VERDICT-DEFERRED =  or  RCOMPILE @ 0=  and IF EXIT THEN
   DIAG-QUIET @ 0= IF DIAG-PRINT THEN
   CHECKER-REJECT-RC throw ;

\ A step is a guest in the term pools: an event fires between the tokens of a
\ declaration whose terms sit in the pools, on the stack and in its variables
\ (PRIM: ... PRIM;), so the step allocates above the pools' marks, where NEW
\ would reset them, and releases back to the marks.
CHECKER-REG:SCRATCH{
variable TG-FV     variable TG-SPN     variable TG-QEN     variable TG-PTRN
variable TG-STLN   variable TG-ATOMN   variable TG-PARAMN  variable TG-PSCR
variable TG-PARG   variable TG-RIGID   variable TG-RGN     variable TG-EXT
variable TG-GEN    variable TG-TERM-NAME
CHECKER-REG:}SCRATCH

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
   -1 TOP-STEP !  catch  dup 0= IF drop ['] TOP-JUDGE catch THEN  0 TOP-STEP !
   {: rc:n :}
   rc 0 <> IF TOP-RELEASE rc throw THEN ;

: TOP-SEG-PRUNE ( n -- ) {: lvl:n :}
   BEGIN TOP-ENTRY @ lvl >  TOP-LIVE @ 0 <> and WHILE
      TOP-SEG-N @ 0 > IF TOP-SEG-N @ 1 - TOP-SEG-UNPARK ELSE TOP-SEG-RELEASE  0 TOP-LIVE ! THEN
   REPEAT ;

: CHECKER-TOP-SEGMENT ( n -- ) {: base:n :}
   data-base EVALD-CELL + @ {: lvl:n :}
   lvl TOP-SEG-PRUNE
   TOP-LIVE @ 0= IF base TOP-BASE !  lvl TOP-ENTRY !  lvl TOP-LEVEL !  -1 TOP-LIVE !  EXIT THEN
   TOP-BASE @ base =  TOP-ENTRY @ lvl <= and  lvl TOP-LEVEL @ <= and IF lvl TOP-LEVEL !  EXIT THEN
   base lvl TOP-SEG-FIND dup 0 >= IF TOP-SEG-UNPARK  lvl TOP-LEVEL !  EXIT THEN drop
   TOP-SEG-PARK  base TOP-BASE !  lvl TOP-ENTRY !  lvl TOP-LEVEL ! ;

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

: TOP-READ-AT ( n -- ptr u8 ) TOP-READ-P @ BYTE-VIEW + ;

\ The cells a stored entry fills when a load reads it back: its recorded width,
\ except a cell lost to a live scope, which comes back as one unknown cell
\ (E-COPY*'s S-PUSH arm).
: TOP-PUSH-CELLS ( n -- n ) {: off:n :}
   off TOP-READ-AT EN.A @ TOP-READ-AT {: ty:ptr :}
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
   DCUR @ ROW-TERMS 0 <> IF DCUR @ E-COPY ELSE 0 THEN {: off:n :}
   RCUR @ ROW-TERMS 0 <> IF RCUR @ E-COPY ELSE 0 THEN TOP-RROW-OFF !
   0 TOP-COPYING !
   TOP-READ-P @ TOP-WRITE-P @ {: old:ptr next:ptr :}
   TOP-READ-CAP @ TOP-WRITE-CAP @ {: oldcap:n nextcap:n :}
   next TOP-READ-P !  nextcap TOP-READ-CAP !
   old TOP-WRITE-P !  oldcap TOP-WRITE-CAP !
   off TOP-ROW-OFF !  EC-TVN @ TOP-ROW-TV !  EC-RVN @ TOP-ROW-RV !
   TOP-WRITE-END @ TOP-READ-END !
   off 0 <> IF TOP-ROW-COUNT ELSE 0 THEN TOP-ROW-CELLS !
   TOP-RELEASE ;

: TOP-STEP-LOAD ( [ -- ] n -- )
   {: live:n :}
   live CHECKER-TOP-LOAD  TOP-RUN ;

: CHECKER-TOP-WORD ( ptr u8 n n -- )
   {: a:ptr u:n live:n :}
   a TOK-SPELL-A !  u TOK-SPELL-U !  ['] TOP-WORD-STEP live TOP-STEP-LOAD ;

: CHECKER-TOP-TICK ( ptr u8 n n -- )
   {: a:ptr u:n live:n :}
   a TOK-SPELL-A !  u TOK-SPELL-U !  ['] TOP-TICK-STEP live TOP-STEP-LOAD ;

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

\ A literal of class cls: STR pushes ptr u8 and n, CSTR ptr u8, NUM n or r,
\ CHAR n; an integer NUM's entry keeps its value.
: TOP-READ-LIT ( ptr u8 n n -- ) {: a:ptr u:n cls:n :}
   cls TOP-EV-NUM = IF
      a u FLOAT-LITERAL-ADMITTED? nip IF CC-R ELSE CC-N THEN TOP-READ-CON TOP-READ-PUSH
      a u INTEGER-LITERAL-ADMITTED? IF
         TOP-ROW-OFF @ TOP-READ-AT EN.F !
         -1 TOP-ROW-OFF @ TOP-READ-AT EN.E !
      ELSE drop THEN EXIT
   THEN
   cls TOP-EV-STR = cls TOP-EV-CSTR = or IF TOP-READ-PTR-U8 TOP-READ-PUSH THEN
   cls TOP-EV-CSTR <> IF CC-N TOP-READ-CON TOP-READ-PUSH THEN ;

: TOP-LIT-STEP ( ptr u8 n n -- ) {: a:ptr u:n cls:n :}
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
: CHECKER-TOP-PUSH ( ptr u8 n n n -- ) {: a:ptr u:n cls:n live:n :}
   live TOP-READ-FIT
   TOP-ROW-OFF @ 0 <>  TOP-ROW-CELLS @ live =  and IF a u cls TOP-READ-LIT EXIT THEN
   live CHECKER-TOP-LOAD  a u cls TOP-LIT-STEP  CHECKER-TOP-SAVE ;

package CHECKER-REG
' RECORD-GRID-BEFORE DECLARATIONS CHECKER-OWNER-ABI:GRID-BEFORE-OFF + xt!
' RECORD-GRID-AFTER DECLARATIONS CHECKER-OWNER-ABI:GRID-AFTER-OFF + xt!
' DP-CUT DECLARATIONS CHECKER-OWNER-ABI:GRID-REVOKE-OFF + xt!
' RECORD-GRID-RETIRE DECLARATIONS CHECKER-OWNER-ABI:GRID-RETIRE-OFF + xt!
;package

\ ---- the optimizing front end's half of the owner record ---------------------
\ Installed at this file's own load: every checker instance wires its own
\ seams, so the instance that owns the source is complete whoever loaded it.
\ The compiler reads these through NCOMP-DISPATCH:DECL-*-OFF, never by name; a
\ field this file forgets is a named refusal there, not a silent answer from
\ the retired instance.
package CHECKER-REG
' CHECK-DOES!                       DECLARATIONS DOES-CHECK-OFF + xt!
' CWIN-CELLS                        DECLARATIONS CALL-CELLS-OFF + xt!
' REC-CALL-BINDING                  DECLARATIONS CHECKER-OWNER-ABI:CALL-BINDING-OFF + xt!
' REC-UNJUDGED-BINDING              DECLARATIONS CHECKER-OWNER-ABI:UNJUDGED-BINDING-OFF + xt!
' UNJUDGED-CLOSED?                  DECLARATIONS CHECKER-OWNER-ABI:UNJUDGED-CLOSED-OFF + xt!
' PIN-SPAN                          DECLARATIONS CHECKER-OWNER-ABI:PIN-SPAN-OFF + xt!
' PIN-TOKEN                         DECLARATIONS CHECKER-OWNER-ABI:PIN-TOKEN-OFF + xt!
' REC-BINDING-WINDOW                DECLARATIONS CHECKER-OWNER-ABI:BINDING-WINDOW-OFF + xt!
' CWIN-MATCH-PAYLOAD                DECLARATIONS CALL-MATCH-OFF + xt!
' EFFECT-INIT-LAYOUT                DECLARATIONS CHECKER-OWNER-ABI:INIT-LAYOUT-OFF + xt!
' EFFECT-FIELD-SPAN                 DECLARATIONS FIELD-SPAN-OFF + xt!
' REC-MIN-IN@                       DECLARATIONS REC-MIN-IN-OFF + xt!
' REC-WIDE-PUBLISH                  DECLARATIONS REC-WIDE-PUBLISH-OFF + xt!
' CHECK-UNJUDGED!                   DECLARATIONS CHECK-UNJUDGED-OFF + xt!
;package

package CHECKER-REG
' CHECKER-RECORD-SYM? DECLARATIONS CHECKER-OWNER-ABI:VERIFY-RECORD-SYM-OFF + xt!
' CHECKER-CTOR-DECL-AT! DECLARATIONS CHECKER-OWNER-ABI:VERIFY-CTOR-DECL-OFF + xt!
' CHECKER-FIND-QUIET-SYM DECLARATIONS CHECKER-OWNER-ABI:VERIFY-FIND-SYM-OFF + xt!
' CHECKER-CREATES-SYM? DECLARATIONS CHECKER-OWNER-ABI:VERIFY-CREATES-SYM-OFF + xt!
' CHECKER-RECORD-CREATED DECLARATIONS CHECKER-OWNER-ABI:VERIFY-RECORD-CREATED-OFF + xt!
' CHECKER-SOURCE-DOES! DECLARATIONS CHECKER-OWNER-ABI:VERIFY-SOURCE-DOES-OFF + xt!
' CHECKER-VERIFY-RENDERS DECLARATIONS CHECKER-OWNER-ABI:VERIFY-RENDERS-OFF + xt!
' CHECKER-NATIVE-DOES-FINISH DECLARATIONS CHECKER-OWNER-ABI:NATIVE-DOES-FINISH-OFF + xt!
' CHECKER-NATIVE-DOES-BEGIN DECLARATIONS CHECKER-OWNER-ABI:NATIVE-DOES-BEGIN-OFF + xt!
' CHECKER-NATIVE-DOES-COMMIT DECLARATIONS CHECKER-OWNER-ABI:NATIVE-DOES-COMMIT-OFF + xt!
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
' CHECKER-RESOLVE:EACH-VISIBLE DECLARATIONS CHECKER-OWNER-ABI:VERIFY-EACH-VISIBLE-OFF + xt!
' WITH-TICK-ORDER DECLARATIONS CHECKER-OWNER-ABI:WITH-TICK-ORDER-OFF + xt!
' TRUST-DECL? DECLARATIONS CHECKER-OWNER-ABI:VERIFY-DECL-OFF + xt!
' CHECKER-RETRACT-ROWS DECLARATIONS CHECKER-OWNER-ABI:RETRACT-ROWS-OFF + xt!
' CHECKER-ROWS-END DECLARATIONS CHECKER-OWNER-ABI:ROWS-END-OFF + xt!
' CHECKER-WRITE-WINDOW! DECLARATIONS CHECKER-OWNER-ABI:WRITE-WINDOW-OFF + xt!
' CHECKER-SOURCE-CLAUSE DECLARATIONS CHECKER-OWNER-ABI:VERIFY-SOURCE-CLAUSE-OFF + xt!
' CHECKER-VERIFY-TOP-BINDING DECLARATIONS CHECKER-OWNER-ABI:VERIFY-TOP-BINDING-OFF + xt!
' CHECKER-VERIFY-TRUST DECLARATIONS CHECKER-OWNER-ABI:VERIFY-TRUST-OFF + xt!
' CHECKER-REPORT-RESET DECLARATIONS CHECKER-OWNER-ABI:RESET-REPORT-OFF + xt!
' CHECKER-JSON-REPORTED? DECLARATIONS CHECKER-OWNER-ABI:JSON-REPORTED-OFF + xt!
' CHECK-DOES-UNJUDGED! DECLARATIONS CHECKER-OWNER-ABI:DOES-UNJUDGED-OFF + xt!
' CHECKER-NATIVE-DOES-REPORT DECLARATIONS CHECKER-OWNER-ABI:NATIVE-DOES-REPORT-OFF + xt!

\ A checker claims the source only where nothing owns it, after every callback
\ is installed.
: CLAIM-COLD-SOURCE ( -- )
   SOURCE-OWNER-A @ 0= if
      DECLARATIONS SOURCE-OWNER-A !
   then ;
CLAIM-COLD-SOURCE
;package

\ ---- the facts reader ---------------------------------------------------------
\ A record's facts text (CHECK:FACTS@) is read into the stores the first time a
\ lookup binds the record (LIVE-BIND), and an export's when the linker links it.
package CHECK

\ Per wordlist id, one plus the index of the newest namespace record whose public
\ or private wordlist it is, 0 for none, over the records below NS-SEEN. A lookup
\ first indexes the records made since. When the count went down it drops the rows
\ naming a record at or above it, which can empty a row an older record holds; it
\ indexes from 0 again when the answer is missing or no longer holds, as after a
\ package gains its private wordlist. The cells rest in a scratch span.
CHECKER-REG:SCRATCH{
PTR-VARIABLE NS-ROWS-P   variable NS-CAP   variable NS-SEEN
CHECKER-REG:}SCRATCH

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
   i CHECKER-RESOLVE:REC-AT {: ns:ptr :}
   ns CK-REC-WID BOOT:NAMESPACE-WL <> IF EXIT THEN
   ns @ i NS-PUT  ns CELL + @ i NS-PUT ;

: NS-CLEAR ( -- ) NS-ROWS-P @ 0 NS-CAP @ ARENA-CELLS-ZERO  0 NS-SEEN ! ;

: NS-UNNAME ( ptr n n -- )
   {: row:ptr top:n :}
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
   row 1 - CHECKER-RESOLVE:REC-AT {: ns:ptr :}
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

: REC-SYM ( ptr n -- n ) {: rec:ptr :}
   rec CHECKER-RESOLVE:CK-REC-NAME$ {: name:ptr nameu:n :}
   rec CK-REC-WID {: wid:n :}
   nameu 0= wid -2 = or IF 0 EXIT THEN
   wid WL:GLOBAL? IF name nameu CHECKER-GLOBAL-SYM EXIT THEN
   wid NS-FIND {: row:n :}
   row 0= IF 0 EXIT THEN
   row 1 - CHECKER-RESOLVE:REC-AT {: ns:ptr :}
   ns CHECKER-RESOLVE:CK-REC-NAME$
   wid ns @ = IF SYM-PUBLIC ELSE SYM-PRIVATE THEN
   name nameu CHECKER-PKG-SYM ;

\ A read runs inside a caller's check, between its tokens, so it is a guest
\ there, as a top step is (TOP-GUEST): it parses above the term pools' marks,
\ and the parser's and recorder's cells, which the caller may hold, are saved
\ and given back however the read ends. One read is open at a time.
512 constant GUEST-CAP
80 constant GUEST-PTRS-CAP
CHECKER-REG:SCRATCH{
create GUEST-SAVED GUEST-CAP cells allot
GUEST-PTRS-CAP PTR-U8-TABLE GUEST-SAVED-PTRS
variable GUEST-OPEN
CHECKER-REG:}SCRATCH

: GUEST-CELLS ( n ptr n n bool -- n )
   {: k:n p:ptr cnt:n save:bool :}
   k cnt + GUEST-CAP > IF s" checker: guest cells full" 76 die THEN
   cnt 0 ?DO
      save IF i cells p + @  k i + cells GUEST-SAVED + !
      ELSE k i + cells GUEST-SAVED + @  i cells p + ! THEN
   LOOP
   k cnt + ;

: GUEST-CELL ( n ptr n bool -- n ) {: p:ptr save:bool :} p 1 save GUEST-CELLS ;

: GUEST-PTRS ( n ptr ptr u8 n bool -- n )
   {: k:n p:ptr cnt:n save:bool :}
   k cnt + GUEST-PTRS-CAP > IF s" checker: guest pointers full" 76 die THEN
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
   NMAP TYPE-VAR-MAX 3 + save GUEST-CELLS  ROWMAP 26 save GUEST-CELLS
   FAM-N save GUEST-CELL  FAM-I save GUEST-CELL  FAM-K save GUEST-CELL
   FAM-U FAM-CAP save GUEST-CELLS
   NRES save GUEST-CELL  NDI save GUEST-CELL  NDH save GUEST-CELL
   SGBAD save GUEST-CELL  SGBAD-U save GUEST-CELL  SGBAD-KIND save GUEST-CELL
   SGBAD-AR-DECL save GUEST-CELL  SGBAD-AR-GOT save GUEST-CELL  SGBAD-SIZE save GUEST-CELL
   SIG-UNRES save GUEST-CELL  SIG-UNRES-U save GUEST-CELL
   SIG-RAW-MODE save GUEST-CELL  SIG-FACTS-MODE save GUEST-CELL  SIG-CTL save GUEST-CELL
   SIG-TAGS-ONLY save GUEST-CELL
   SIG-PE save GUEST-CELL  SIG-PIMM save GUEST-CELL  SIG-CREATES-U save GUEST-CELL
   SIG-OVERLOAD-U save GUEST-CELL  SIGSCOPE-ON save GUEST-CELL  SIGSCOPE-U save GUEST-CELL ;

\ The latches E-ADD-EFFECT sets for the record the engine publishes next.
: GUEST-LATCHES ( n bool -- n ) {: save:bool :}
   DL-NEW save GUEST-CELL  RECW save GUEST-CELL  RECMI save GUEST-CELL
   LBUF-EVAL-OFF save GUEST-CELL  CHECKER-REC-SYM save GUEST-CELL ;

: GUEST-POINTERS ( bool -- ) {: save:bool :}
   0  SB 1 save GUEST-PTRS  SS 1 save GUEST-PTRS  PKA 1 save GUEST-PTRS
   SGBAD-A 1 save GUEST-PTRS  SIG-UNRES-A 1 save GUEST-PTRS
   SIG-CREATES-A 1 save GUEST-PTRS  SIG-OVERLOAD-A 1 save GUEST-PTRS
   SIGSCOPE-P 1 save GUEST-PTRS
   FAM-A FAM-CAP save GUEST-PTRS  drop ;

: GUEST-KEEP ( bool -- ) {: save:bool :}
   0 save GUEST-POOLS save GUEST-PARSER save GUEST-LATCHES drop
   save GUEST-POINTERS ;

: READ-ENTER ( -- )
   GUEST-OPEN @ IF s" checker: a facts read inside another" 76 die THEN
   RES-TRUE GUEST-KEEP  -1 GUEST-OPEN !
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
: READ-ROWS ( ptr u8 n -- n n n n bool )
   {: a:ptr u:n :}
   SGBAD-CLEAR
   a u PARSE-SIG-RAW {: din:n dout:n rin:n rout:n :}
   SGBAD @ 0 <>  SIG-UNRES @ 0 <> or IF
      din dout rin rout RES-FALSE EXIT
   THEN
   SIG-TAGS-ONLY @ IF din dout rin rout RES-TRUE EXIT THEN
   a u din dout rin rout SGBAD-UNFIT!
   din dout rin rout SGBAD @ 0= ;

: READ-REFUSE ( ptr u8 n ptr u8 n -- )
   {: a:ptr u:n na:ptr nu:n :}
   SGBAD @ 0= IF a u SGBAD-SYNTAX! THEN
   a u na nu na nu USIG-ADD-BAD ;

\ The created words' effect, offset+1, under symbol 0; 0 for none.
: READ-MADE ( ptr u8 n -- n bool )
   {: ca:ptr cu:n :}
   cu 0= IF 0 RES-TRUE EXIT THEN
   ca cu READ-ROWS {: din:n dout:n rin:n rout:n ok:bool :}
   ok 0=  SIG-TAGS-ONLY @ or IF 0 RES-FALSE EXIT THEN
   0 CHECKER-REC-SYM !
   din dout rin rout SGHASR @ E-BUILD-EFFECT 1 + RES-TRUE ;

\ A primitive row of sym as the table states one, its quotation inputs kept,
\ its record under symbol 0, so a call tries every row (TRY-PRIMS).
: READ-ROW ( n n n n bool n n -- )
   {: din:n dout:n rin:n rout:n hasr:bool sym:n pe:n :}
   0 CHECKER-REC-SYM !
   din KEEP-QUOTS
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
: READ-OVERLOAD ( ptr u8 n n -- ptr u8 n bool )
   {: a:ptr u:n sym:n :}
   a u READ-ROWS {: din:n dout:n rin:n rout:n ok:bool :}
   SIG-PE @  SGHASR @  SIG-OVERLOAD-A @  SIG-OVERLOAD-U @ {: pe:n hasr:bool ra:ptr ru:n :}
   ok 0=  SIG-TAGS-ONLY @ or  SIG-CTL @ 0 <> or  SIG-PIMM @ 0 >= or  SIG-CREATES-U @ 0 <> or IF
      NULL-PTR 0 RES-FALSE EXIT
   THEN
   din dout rin rout hasr sym pe READ-ROW
   ra ru RES-TRUE ;

: READ-OVERLOADS ( ptr u8 n n -- bool )
   {: a:ptr u:n sym:n :}
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

: READ-TAGS ( n n n n -- ) {: ix:n sym:n ctl:n mark:n :}
   sym sym ctl READ-FLAGS ctl XFER-DMASK ctl XFER-RMASK 0 NORET-ADD-CREATES
   ix sym mark READ-LINK ;

: READ-BODY ( ptr u8 n n -- )
   {: a:ptr u:n ix:n :}
   UEND @ {: mark:n :}
   ix CHECKER-RESOLVE:REC-AT {: rec:ptr :}
   rec CHECKER-RESOLVE:CK-REC-NAME$ {: name:ptr nameu:n :}
   rec REC-SYM {: sym:n :}
   sym 0= IF a u name nameu READ-REFUSE EXIT THEN
   sym SYM-PKG$ SIGSCOPE!
   RES-TRUE SIG-FACTS!
   a u READ-ROWS {: din:n dout:n rin:n rout:n ok:bool :}
   ok 0= IF a u name nameu READ-REFUSE EXIT THEN
   SIG-CTL @  SIG-PIMM @  SIG-PE @  SIG-CREATES-A @  SIG-CREATES-U @  SGHASR @
      {: ctl:n pimm:n pe:n ca:ptr cu:n hasr:bool :}
   SIG-OVERLOAD-A @  SIG-OVERLOAD-U @ {: oa:ptr ou:n :}
   SIG-TAGS-ONLY @ IF
      pe 0 <>  pimm 0 >= or  cu 0 <> or  ou 0 <> or
      ctl XFER-FLAGS CHECKER-OWNER-ABI:BINDING-DEFER and 0 <> or IF
         a u name nameu READ-REFUSE EXIT
      THEN
      ix sym ctl mark READ-TAGS EXIT
   THEN
   din dout rin rout hasr sym pe ou 0 <> READ-FIRST
   oa ou sym READ-OVERLOADS 0= IF a u name nameu READ-REFUSE EXIT THEN
   ca cu READ-MADE {: made:n good:bool :}
   good 0= IF a u name nameu READ-REFUSE EXIT THEN
   sym sym ctl READ-FLAGS ctl XFER-DMASK ctl XFER-RMASK made NORET-ADD-CREATES
   ctl XFER-FLAGS CHECKER-OWNER-ABI:BINDING-DEFER and 0 <> IF sym RES-TRUE DFER-ADD-SYM THEN
   pimm 0 >=  sym PIMM-IX 0 <  and IF sym pimm PIMM-ADD THEN
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
   src CTL-FLAGS-SYM CTL-IDENTITY invert and {: ctl:n :}
   src CTL-MASKS-SYM {: dm:n rm:n :}
   src NORET-CREATES@ {: creates:n :}
   sym ctl dm rm creates src CHECKER-SYM-SOURCE NORET-APPEND ;

\ An export's facts are its source's, as CHECKER-EXPORT-ROW records them.
: ALIAS-BODY ( n n -- )
   {: ix:n src:n :}
   ix CHECKER-RESOLVE:REC-AT REC-SYM {: sym:n :}
   src CHECKER-RESOLVE:REC-AT REC-SYM {: from:n :}
   sym 0=  from 0= or IF E-EXPORT-UNDEFINED throw THEN
   from USIG-NEWEST {: eff:n :}
   eff 0= IF E-EXPORT-UNDEFINED throw THEN
   eff 1 - E-PTR EXPORT-EFF-INST {: din:n dout:n rin:n rout:n hasr:bool :}
   sym CHECKER-REC-SYM !
   din dout rin rout hasr from EFFECT-EXTERNAL-SYM? E-ADD-EFFECT
   sym from EXPORT-META-SYM
   ix sym USIG-NEWEST sym NORET-NEWEST RECORD-LINK-SET ;

\ A record `undefine` retired has no symbol.
: HAS-SYM? ( n -- bool ) CHECKER-RESOLVE:REC-AT REC-SYM 0 <> ;

\ Export record ix of record src, once src's own facts are read. An export
\ keeps the facts of a source retired after it, so it reads src's text as its own.
: ALIAS-READ ( n n -- ) {: ix:n src:n :}
   src HAS-SYM? 0= IF
      src FACTS@ ix  READ-ENTER  [: READ-BODY ;] [: READ-LEAVE ;] finally  EXIT
   THEN
   ix src  READ-ENTER  [: ALIAS-BODY ;] [: READ-LEAVE ;] finally ;

\ Reads rec's facts text, or an image's export's source's, when there is one and no link,
\ except hb's own record of a prefix symbol in a build: the prefix holds its rows.
: READ-BOUND ( ptr n -- ) {: rec:ptr :}
   rec CK-REC-FACTS-SLOT cells + @ {: off:n :}
   off 0= IF EXIT THEN
   rec NULL-PTR - dbase@ - DREC / {: ix:n :}
   BUILDING? ix BUILD-FIRST @ < and IF
      rec REC-SYM PREFIX-SYM? IF EXIT THEN
   THEN
   ix LINKED? IF EXIT THEN
   off 0 > IF ix READ-IX EXIT THEN
   -1 off - {: src:n :}
   src 0 <  src ix >=  or IF s" checker: an export's source is not an earlier record" 76 die THEN
   src HAS-SYM? IF src CHECKER-RESOLVE:REC-AT recurse THEN
   ix src ALIAS-READ ;

: FACTS-READ-INSTALL ( -- ) [: READ-BOUND ;] is FACTS-READ-XT ;
FACTS-READ-INSTALL

public
: WORDLIST-PACKAGE ( n -- n bool )
   NS-FIND {: row:n :}
   row 0= IF 0 RES-FALSE EXIT THEN
   row 1 - RES-TRUE ;

\ Export record ix of record src, as a compile of its file recorded it: an
\ export the file retired has no facts, since its `undefine` removed them.
: READ-ALIAS ( n n -- ) {: ix:n src:n :}
   ix HAS-SYM? 0= IF EXIT THEN
   src HAS-SYM? IF src CHECKER-RESOLVE:REC-AT READ-BOUND THEN
   ix src ALIAS-READ ;

\ Reads text a u as record ix's facts now, over what a compile or a read taught:
\ an earlier file's record whose text a later file's `TRUST` row rewrote.
: READ-FACTS ( ptr u8 n n -- )
   {: a:ptr u:n ix:n :}
   u 0= IF EXIT THEN
   a u ix  READ-ENTER  [: READ-BODY ;] [: READ-LEAVE ;] finally ;

\ The kernel object's rows, stated before a build's first file as READ-ROW
\ states one: never a user record, which answers alone.

\ A row with no effect: its symbol alone.
: PRIM-NAME ( ptr u8 n -- ) PE-SYM-OF drop ;

\ Opens a row with an effect; PRIM-ATOM runs each of its atoms.
: PRIM-OPEN ( ptr u8 n -- ) PE-OPEN  0 PE-SPEC-TOS ! ;

: PRIM-ATOM ( n -- ) PE-SPEC-ATOM ;

\ Closes the open row on pkg's private symbol, or the global one for an empty
\ pkg, with its flags; a symbol the prefix holds, a grammar word's, states
\ nothing.
: PRIM-CLOSE ( ptr u8 n bool bool -- )
   {: pkg:ptr pkgu:n trusted:bool fixed:bool :}
   PE-SPEC-TOS @ 0 <> IF s" checker: primitive-spec operand left over" 76 die THEN
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
