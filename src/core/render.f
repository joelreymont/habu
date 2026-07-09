\ render.fs — render the checker's inferred residual stack (DCUR) back to readable
\ type names. Type variables get canonical letters a,b,c… (assigned bottom-to-top),
\ generic int=n, old flag=f, float=r; concrete types render by name. The "render"
\ half of the native sigparse/checker. Needs
\ checker.fs. Standalone has no emit/+!; chars go through a 1-byte buffer + type.
\ Also the checker's sig RECORDER: certified words render "in -- out" to a buffer
\ and append it to USIGS (installed via RECXT), so callers of certified words
\ certify too.
create ECH 1 allot
variable RDST   0 RDST !                 \ 0 = stdout, 1 = RSBUF (sig recording)
16384 constant RSBUF-CAP
create RSBUF RSBUF-CAP allot   variable RSN
variable RQM                             \ a '?' rendered = unknown tag, don't record
variable RDIAG-ON
variable RDIAG-A
variable RDIAG-CAP
variable RDIAG-U
variable RDIAG-I

: EMIT1 {: c :}
   c 63 = IF 1 RQM ! THEN
   RDST @ IF
     RSN @ RSBUF-CAP 2 - > IF s" render: sig buffer full" 76 die THEN
     c RSBUF RSN @ + c!  RSN @ 1 + RSN !
   ELSE c ECH c! ECH 1 type THEN ;

: DIAG-BUFFER! ( ptr u8 n -- )
   {: a:ptr cap:n :}
   a RDIAG-A !
   cap RDIAG-CAP !
   0 RDIAG-U !
   -1 RDIAG-ON ! ;

: DIAG-BUFFER-OFF ( -- )
   0 RDIAG-ON !
   0 RDIAG-U ! ;

\ Typed slot for the diagnostic buffer pointer: a plain `variable @` reads back
\ an untyped n, so the byte store below would not certify; `0 ptr-field` gives
\ the checked ptr u8 view (the lib-wide *-BUF-A idiom).
: RDIAG-A-FIELD ( -- ptr ptr u8 )
   RDIAG-A 0 ptr-field ;

: DIAG-BUFFER$ ( -- ptr u8 n )
   RDIAG-A-FIELD @ RDIAG-U @ ;

: RDIAG-COPY ( ptr u8 n -- )
   {: a:ptr u:n :}
   0 RDIAG-I !
   BEGIN RDIAG-I @ u < WHILE
      a RDIAG-I @ + c@
      RDIAG-A-FIELD @ RDIAG-U @ + RDIAG-I @ + c!
      RDIAG-I @ 1 + RDIAG-I !
   REPEAT ;

: RDIAG-APPEND ( ptr u8 n -- )
   {: a:ptr u:n :}
   RDIAG-ON @ 0= IF 2 a u write drop EXIT THEN
   RDIAG-U @ u + RDIAG-CAP @ > IF s" render: diagnostic buffer full" 76 die THEN
   a u RDIAG-COPY
   RDIAG-U @ u + RDIAG-U ! ;
create SEEN MAXTV cells allot   variable NLET           \ indexed by typevar (PAY)
64 constant RATOM-CAP
create RATOM-KEY RATOM-CAP cells allot
variable RATOM-N
variable RATOM-I

: SEEN-RESET 0 BEGIN dup cells SEEN + UNBOUND swap ! 1 + dup MAXTV 1 - > UNTIL drop ;
: RATOM-RESET ( -- )
   0 RATOM-N ! ;

: LET-OF {: vp :}
   vp cells SEEN + @ UNBOUND = IF NLET @ vp cells SEEN + ! NLET @ 1 + NLET ! THEN
   vp cells SEEN + @ 97 + ;
: RATOM-CHAR ( n -- n ) {: idx:n :}
   idx 26 < IF idx 97 + ELSE 1 RQM ! 63 THEN ;
: RATOM-FIND ( n -- n bool ) {: key:n :}
   0 RATOM-I !
   BEGIN RATOM-I @ RATOM-N @ < WHILE
      RATOM-I @ cells RATOM-KEY + @ key = IF RATOM-I @ 0 0= EXIT THEN
      RATOM-I @ 1 + RATOM-I !
   REPEAT
   0 1 0= ;
: RATOM-ADD ( n -- n ) {: key:n :}
   RATOM-N @ RATOM-CAP >= IF 1 RQM ! 0 EXIT THEN
   key RATOM-N @ cells RATOM-KEY + !
   RATOM-N @
   RATOM-N @ 1 + RATOM-N ! ;
: RATOM-ORD ( n -- n ) {: key:n :}
   key RATOM-FIND IF EXIT THEN drop
   key RATOM-ADD ;

: RSTR ( ptr u8 n -- ) {: a:ptr u:n :}
   0 BEGIN dup u < WHILE dup a + c@ EMIT1 1 + REPEAT drop ;

: CON-OUT ( n -- ) {: p:n :}
   p 2 = IF 102 EMIT1 ELSE
   p 0 > p CTN @ < and IF                 \ any registered type (built-in OR user deftype)
      p CT-NAME$ dup 0 <> IF RSTR ELSE 2drop 63 EMIT1 THEN
   ELSE 63 EMIT1 THEN THEN ;

: ATOM-REND {: t :}
   t ATOM>K 0 = IF t ATOM>A t ATOM>U RSTR EXIT THEN
   s" fresh-" RSTR
   t ATOM>A t ATOM>U RSTR
   45 EMIT1
   t ATOM>K RATOM-ORD RATOM-CHAR EMIT1 ;

: RNUM ( n -- )                 \ small non-negative number (hidden slot index)
   dup 10 >= IF dup 10 / RECURSE THEN
   10 mod 48 + EMIT1 ;

\ a hidden physical field renders as the diagnostic-only '@family.slotN<args>' /
\ '@family.tag<args>' form (docs §20) and sets RQM so REC-SIG never records a
\ sig containing a lone hidden cell. Full runs never reach here: row rendering
\ (REND-COLLECT / QREND's row mode) compacts them to the logical family type.
: PARAM-START {: t:n :}
   t HIDDEN-PARAM? IF
      1 RQM !
      64 EMIT1
      t PARAM>NAME-A t PARAM>NAME-U RSTR
      46 EMIT1
      t HIDDEN-SLOT@  t PARAM>FAM TFAM-WIDTH@* 1 -  = IF
         s" tag" RSTR
      ELSE
         s" slot" RSTR  t HIDDEN-SLOT@ RNUM
      THEN
   ELSE
      t PARAM>NAME-A t PARAM>NAME-U RSTR
   THEN
   60 EMIT1 ;

\ HID-RUN-REST ( n -- n bool ) : from a resolved S-PUSH node whose type is a
\ hidden field, walk the whole run (tag W-1 on top down to slot0, one family).
\ true: row below the full W-cell run (compact to the logical type). false:
\ lone/malformed run — row below the single cell (render the '@' form).
variable HRC  variable HRI  variable HRF
: HID-RUN-CELL? ( n n n -- bool ) {: node:n fam:n slot:n :}
   node TAG S-PUSH <> IF RES-FALSE EXIT THEN
   node P>TYPE T-RES {: t:n :}
   t HIDDEN-PARAM? 0= IF RES-FALSE EXIT THEN
   t PARAM>FAM fam <> IF RES-FALSE EXIT THEN
   t HIDDEN-SLOT@ slot = ;
: HID-RUN-REST ( n -- n bool ) {: node:n :}
   node P>TYPE T-RES {: t:n :}
   t PARAM>FAM {: fam:n :}
   fam TFAM-WIDTH@* {: w:n :}
   t HIDDEN-SLOT@ w 1 - <> IF node P>REST RES-FALSE EXIT THEN
   node HRC !  -1 HRF !
   w 1 - HRI !
   BEGIN HRI @ 0 >  HRF @ 0 <>  and WHILE
      HRC @ P>REST R-RES  fam  HRI @ 1 -  HID-RUN-CELL? IF
         HRC @ P>REST R-RES HRC !
      ELSE
         0 HRF !
      THEN
      HRI @ 1 - HRI !
   REPEAT
   HRF @ 0 <> IF HRC @ P>REST RES-TRUE ELSE node P>REST RES-FALSE THEN ;

\ a quot type renders [ in -- out ] or [ in -- out | rin -- rout ] when the
\ quotation has a non-neutral return-stack effect. Rendering is fully recursive
\ to a bounded nesting depth (QDEPTH-MAX levels) with a cycle guard, so a deeply
\ nested quot (combinator over combinator, typed loop/tile combinators) renders
\ in full instead of capping the 3rd level at '?'.
\ Gap2/3: quot-bearing sigs now RECORD as scheme-strings and round-trip, so
\ combinator call sites (dip, keep) are checked against them. Only a genuine '?'
\ (an unmodeled tag, via RQM) still blocks recording — see REC-SIG below.
6 constant QDEPTH-MAX                        \ quotation nesting render budget
create QPATH QDEPTH-MAX 1 + cells allot      \ quot node on the current render path, by depth

: QRET? ( q -- f ) {: q :}  q Q>RIN R-RES  q Q>ROUT R-RES  <> ;

\ is quot node r already being rendered above depth d (a type-graph cycle)?
: QANCESTOR? {: r:n d:n :}
   0 BEGIN dup d < WHILE
      dup cells QPATH + @ r = IF drop -1 EXIT THEN
      1 +
   REPEAT drop 0 ;

\ QREND ( x d mode -- ) : one recursive renderer. mode>0 renders a row
\ bottom-to-top (space-separated); mode=0 renders a type. RECURSE re-enters with
\ the mode flag, so nested quots reuse it at depth d+1 up to QDEPTH-MAX.
: QREND ( n n n -- ) {: x:n d:n mode:n :}
   mode 0 > IF
      x R-RES dup TAG S-PUSH = IF                 \ ( node )
         dup P>TYPE T-RES HIDDEN-PARAM? IF        \ hidden run: compact or '@' form (docs §20)
            dup HID-RUN-REST IF                   \ ( node rest ) full run -> logical type
               dup R-RES TAG S-PUSH = IF dup d 1 RECURSE 32 EMIT1 THEN
               drop
               P>TYPE T-RES MK-LOGICAL d 0 RECURSE
            ELSE                                  \ ( node rest ) lone/malformed -> '@' cell
               drop
               dup P>REST dup R-RES TAG S-PUSH = IF d 1 RECURSE 32 EMIT1 ELSE drop THEN
               P>TYPE d 0 RECURSE
            THEN
         ELSE
            dup P>REST dup R-RES TAG S-PUSH = IF d 1 RECURSE 32 EMIT1 ELSE drop THEN
            P>TYPE d 0 RECURSE
         THEN
      ELSE drop THEN
      EXIT
   THEN
   x T-RES {: r:n :}
   r TAG case
      T-VAR of r PAY LET-OF EMIT1 endof
      T-CON of r PAY CON-OUT endof
      T-PTR of s" ptr " RSTR  r PTR>INNER d 0 RECURSE endof
      T-QUOT of
        d QDEPTH-MAX <  r d QANCESTOR? 0=  and IF
           r d cells QPATH + !
           91 EMIT1 32 EMIT1  r Q>DIN d 1+ 1 RECURSE
           45 EMIT1 45 EMIT1 32 EMIT1  r Q>DOUT d 1+ 1 RECURSE
           r QRET? IF
              32 EMIT1 124 EMIT1 32 EMIT1
              r Q>RIN d 1+ 1 RECURSE 45 EMIT1 45 EMIT1 32 EMIT1
              r Q>ROUT d 1+ 1 RECURSE
           THEN
           93 EMIT1
        ELSE 63 EMIT1 THEN
      endof
      T-ATOM of r ATOM-REND endof
      T-PARAM of
        d QDEPTH-MAX <  r d QANCESTOR? 0=  and IF
           r d cells QPATH + !
           r PARAM-START
           0 BEGIN dup r PARAM>ARGC < WHILE
             dup 0 > IF 44 EMIT1 THEN
             r over PARAM>ARG d 1+ 0 RECURSE
             1 +
           REPEAT drop 62 EMIT1
        ELSE 63 EMIT1 THEN
      endof
      63 EMIT1
   endcase ;

: REND-TYPE {: t:n :}  t 0 0 QREND ;
create RBUF 64 cells allot   variable RBN
variable RSHOW-DST

: RBUF+ ( n -- )
   RBN @ cells RBUF + !  RBN @ 1 + RBN ! ;

\ REND-COLLECT compacts each full hidden-field run to ONE logical family term
\ (docs §20); a lone/malformed hidden cell stays and renders as its '@' form.
: REND-COLLECT {: s:n :}  0 RBN !  s
   BEGIN R-RES dup TAG S-PUSH = WHILE          \ no locals inside the loop
     dup P>TYPE T-RES HIDDEN-PARAM? IF
        dup HID-RUN-REST IF
           swap P>TYPE T-RES MK-LOGICAL RBUF+
        ELSE
           swap P>TYPE RBUF+
        THEN
     ELSE
        dup P>TYPE RBUF+  P>REST
     THEN
   REPEAT drop ;

\ RENDER ( -- ) : print DCUR's residual stack bottom-to-top, space-separated.
: RENDER  SEEN-RESET RATOM-RESET 0 NLET !  DCUR @ REND-COLLECT
   RBN @ BEGIN dup 0 > WHILE 1 - dup cells RBUF + @ REND-TYPE 32 EMIT1 REPEAT drop ;

: SHOW-LOCAL-TYPE ( ptr u8 n n -- ) {: name:ptr nameu:n t:n :}
   RDST @ RSHOW-DST !
   0 RDST !
   s" inferred " RSTR
   name nameu RSTR
   s" : " RSTR
   SEEN-RESET RATOM-RESET 0 NLET !
   t REND-TYPE
   10 EMIT1
   RSHOW-DST @ RDST ! ;
' SHOW-LOCAL-TYPE LOCSHOWXT !

\ REND-SIG ( -- a u ) : render the just-checked word's effect "in -- out" —
\ inputs from the base row's instantiation (BROW), outputs from DCUR.
: REND-SIG
   1 RDST !  0 RSN !  0 RQM !  SEEN-RESET RATOM-RESET 0 NLET !
   BROW @ REND-COLLECT
   RBN @ BEGIN dup 0 > WHILE 1 - dup cells RBUF + @ REND-TYPE 32 EMIT1 REPEAT drop
   45 EMIT1  45 EMIT1
   DCUR @ REND-COLLECT
   RBN @ BEGIN dup 0 > WHILE 1 - 32 EMIT1 dup cells RBUF + @ REND-TYPE REPEAT drop
   0 RDST !  RSBUF RSN @ ;

\ DIAG-PRINT ( -- ) : reject diagnostic, one line to stderr —
\   habu: in NAME: at 'TOK' expected: <row> actual: <row>
\ Rows render bottom-to-top with the shared var-letter naming; expected/actual
\ only appear when the failing unify was captured (STEP/SUNI).
: DTXT ( ptr u8 n -- ) {: a:ptr u:n :}
   0 BEGIN dup u < WHILE dup a + c@ EMIT1 1 + REPEAT drop ;

: DROW {: s :}  s REND-COLLECT
   RBN @ BEGIN dup 0 > WHILE 1 - dup cells RBUF + @ REND-TYPE 32 EMIT1 REPEAT drop ;

\ structured diagnostics: `JSON-DIAGS ON` emits one JSON object per reject or
\ uncheckable verdict for LLM repair.
create JNBUF 20 allot  variable JNV  variable JNN
variable DSUGE  variable DSUGA
: JNUM
   JNV !  0 JNN !
   JNV @ 0= IF 48 EMIT1 EXIT THEN
   BEGIN JNV @ 0 > WHILE
      JNV @ 10 mod 48 +  JNBUF JNN @ + c!
      JNN @ 1 + JNN !
      JNV @ 10 / JNV !
   REPEAT
   JNN @ BEGIN dup 0 > WHILE
      1 - dup JNBUF + c@ EMIT1
   REPEAT drop ;
: JCHAR {: c :}
   c case
      10 of 92 EMIT1 110 EMIT1 endof
      13 of 92 EMIT1 114 EMIT1 endof
      9 of 92 EMIT1 116 EMIT1 endof
      34 of 92 EMIT1 c EMIT1 endof
      92 of 92 EMIT1 c EMIT1 endof
      c EMIT1
   endcase ;
: JSTR ( ptr u8 n -- ) {: a:ptr u:n :}
   34 EMIT1  0 BEGIN dup u < WHILE dup a + c@ JCHAR 1 + REPEAT drop 34 EMIT1 ;
: JKEY ( ptr u8 n -- ) {: a:ptr u:n :}
   a u JSTR  58 EMIT1 ;
: JROW {: s :}  34 EMIT1  s DROW  34 EMIT1 ;
: SIG-WS? {: c :}  c 32 =  c 9 = or  c 10 = or  c 13 = or ;
: SIG-LTRIM ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   0 BEGIN dup u < WHILE
      dup a + c@ SIG-WS? 0= IF dup a + u rot - EXIT THEN
      1 +
   REPEAT drop a 0 ;
: SIG-RTRIM ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   u BEGIN dup 0 > WHILE
      a over 1 - + c@ SIG-WS? IF 1 - ELSE a swap EXIT THEN
   REPEAT drop a 0 ;
: SIG-TRIM ( a u -- a u )  SIG-LTRIM SIG-RTRIM ;
: JEFFECT {: din dout rin rout hasr :}
   34 EMIT1
   din DROW  s" -- " DTXT  dout DROW
   hasr IF s" | " DTXT  rin DROW  s" -- " DTXT  rout DROW THEN
   34 EMIT1 ;
: DCODE
   UNSAFE @ IF s" E-UNSAFE" ELSE
   LOCALBAD @ IF s" E-BAD-LOCAL-SHAPE" ELSE
   LINLOCBAD @ IF s" E-LINEAR-LOCAL" ELSE
   DEADERR @ IF s" E-DEAD-CODE" ELSE
   QUALBAD @ IF s" E-BAD-QUALIFIED" ELSE
   UNDEFERR @ IF s" E-UNDEFINED" ELSE
   DVERD @ 1 = IF s" E-UNCHECKABLE" ELSE
   SGBAD @ IF SGBAD-UNKNOWN? IF s" E-UNKNOWN-SIGNATURE-TYPE" ELSE SGBAD-BAREPTR? IF s" E-BARE-PTR-SIGNATURE" ELSE SGBAD-ARITY? IF s" E-WRONG-ARITY" ELSE s" E-BAD-SIGNATURE" THEN THEN THEN ELSE
   DEXP @ 0 <> IF s" E-MISMATCH" ELSE s" E-REJECTED" THEN THEN THEN THEN THEN THEN THEN THEN THEN ;
: DVERDICT ( -- ptr u8 n )
   UNDEFERR @ IF
      s" rejected"
   ELSE
      DVERD @ 1 = IF s" uncheckable" ELSE s" rejected" THEN
   THEN ;
: RETURN-MISMATCH? ( -- f )
   SGHASR @ IF
      RCUR @ R-RES  SGROUT @ R-RES  <>
   ELSE
      RCUR @ R-RES  RBROW @ R-RES  <>
   THEN ;
: REPAIR-CLASS ( -- a u )
   UNSAFE @ IF s" trusted_boundary_required" EXIT THEN
   LOCALBAD @ IF s" factor_local_shape" EXIT THEN
   LINLOCBAD @ IF s" factor_linear_local" EXIT THEN
   DEADERR @ IF s" remove_dead_code" EXIT THEN
   QUALBAD @ IF s" fix_qualified_name" EXIT THEN
   UNDEFERR @ IF s" unknown_rejection" EXIT THEN
   DVERD @ 1 = IF s" rewrite_uncheckable" EXIT THEN
   SGBAD @ IF
      SGBAD-UNKNOWN? IF s" fix_signature_type" ELSE SGBAD-BAREPTR? IF s" fix_bare_ptr_element" ELSE SGBAD-ARITY? IF s" fix_signature_arity" ELSE s" fix_signature_syntax" THEN THEN THEN
      EXIT
   THEN
   RETURN-MISMATCH? IF s" fix_return_stack" EXIT THEN
   DEXP @ 0= IF
      s" unknown_rejection" EXIT
   THEN
   DEXP @ REND-COLLECT RBN @ DSUGE !
   DACT @ REND-COLLECT RBN @ DSUGA !
   DSUGA @ DSUGE @ > IF s" remove_producer" ELSE
   DSUGA @ DSUGE @ < IF s" add_producer" ELSE
   s" fix_type" THEN THEN ;
\ Short repair hint derived from the stable class. Raw stack rows stay in their
\ own JSON fields; this text is only for LLM action selection.
: SUGGEST-TEXT ( -- a u )
   UNSAFE @ IF s" Move this compiler or runtime boundary behind audited TRUST." EXIT THEN
   LOCALBAD @ IF s" Move locals to a live top-level path or factor a helper." EXIT THEN
   LINLOCBAD @ IF s" Keep the linear value on the stack; do not bind it to a local." EXIT THEN
   DEADERR @ IF s" Remove tokens after the terminating control word, or move the work before it." EXIT THEN
   QUALBAD @ IF s" Use one ':' qualifier, e.g. PKG:WORD." EXIT THEN
   UNDEFERR @ IF s" Inspect the token, signature, and raw stack evidence." EXIT THEN
   DVERD @ 1 = IF s" Rewrite with modeled words or isolate an audited primitive." EXIT THEN
   SGBAD @ IF
      SGBAD-UNKNOWN? IF
         s" Use a known stack-signature type or a single-letter type variable."
      ELSE SGBAD-BAREPTR? IF
         s" Give 'ptr' an element type, e.g. 'ptr u8' or 'ptr a'."
      ELSE SGBAD-ARITY? IF
         s" Give the type family its exact declared number of arguments."
      ELSE
         s" Repair the stack-effect comment syntax, including --."
      THEN THEN THEN
      EXIT
   THEN
   RETURN-MISMATCH? IF s" Balance return-stack transfers before the definition exits." EXIT THEN
   DEXP @ 0= IF
      s" Inspect the token, signature, and raw stack evidence." EXIT
   THEN
   DEXP @ REND-COLLECT RBN @ DSUGE !
   DACT @ REND-COLLECT RBN @ DSUGA !
   DSUGA @ DSUGE @ > IF  s" Remove an extra producer or drop the surplus value."
   ELSE DSUGA @ DSUGE @ < IF  s" Add the missing producer or stop consuming a required value."
   ELSE  s" Change the body so produced types match the signature."
   THEN THEN ;
variable JPOS  variable JLINE  variable JCOL
: JLOC-CALC ( -- )
   1 JLINE !  1 JCOL !  0 JPOS !
   BEGIN JPOS @ FAILB @ <  JPOS @ TBLEN @ <  and WHILE
      TBASE 0 ptr-field @ JPOS @ + c@ 10 = IF
         JLINE @ 1 + JLINE !  1 JCOL !
      ELSE
         JCOL @ 1 + JCOL !
      THEN
      JPOS @ 1 + JPOS !
   REPEAT ;
: JABS-LINE ( -- n )  DIAGL0 @ JLINE @ + 1 - ;
: JABS-COL ( -- n )
   JLINE @ 1 = IF DIAGC0 @ JCOL @ + 1 - ELSE JCOL @ THEN ;
: JABS-BSTART ( -- n )  DIAGB0 @ FAILB @ + ;
: JABS-BEND ( -- n )  DIAGB0 @ FAILE @ + ;
: DIAG-PROSE
   SGBAD-UNKNOWN? IF
     s" habu: in " DTXT  NMA @ NMU @ DTXT  s" : unknown type '" DTXT
     FAILTK FAILTU @ DTXT  s" ' in signature" DTXT EXIT
   THEN
   SGBAD-BAREPTR? IF
     s" habu: in " DTXT  NMA @ NMU @ DTXT
     s" : 'ptr' needs an element type, e.g. 'ptr u8' or 'ptr a'" DTXT EXIT
   THEN
   SGBAD-ARITY? IF
     s" habu: in " DTXT  NMA @ NMU @ DTXT  s" : wrong arity for type family '" DTXT
     FAILTK FAILTU @ DTXT  s" '" DTXT EXIT
   THEN
   QUALBAD @ IF
     s" E-BAD-QUALIFIED habu: in " DTXT  NMA @ NMU @ DTXT
     s" : malformed qualified name '" DTXT  FAILTK FAILTU @ DTXT
     s" ' (more than one ':')" DTXT EXIT
   THEN
   UNDEFERR @ IF
     s" E-UNDEFINED habu: in " DTXT  NMA @ NMU @ DTXT
     s" : undefined word '" DTXT  FAILTK FAILTU @ DTXT  s" '" DTXT EXIT
   THEN
   LINLOCBAD @ IF
     s" E-LINEAR-LOCAL habu: in " DTXT  NMA @ NMU @ DTXT
     s" : linear value cannot be bound to a local; keep it on the stack" DTXT EXIT
   THEN
   s" habu: in " DTXT  NMA @ NMU @ DTXT  s" : at '" DTXT  FAILTK FAILTU @ DTXT
   s" '" DTXT
   DEADERR @ IF s"  after '" DTXT DEADTA @ DEADTU @ DTXT s" '" DTXT THEN
   DEXP @ 0 <> IF
     s"  expected: " DTXT  DEXP @ DROW
     s" actual: " DTXT  DACT @ DROW THEN ;
: DIAG-JSON
   JLOC-CALC
   123 EMIT1                                              \ {
   s" schema_version" JKEY 1 JNUM 44 EMIT1
   s" code" JKEY   DCODE JSTR  44 EMIT1
   s" repair_class" JKEY REPAIR-CLASS JSTR  44 EMIT1
   s" verdict" JKEY DVERDICT JSTR  44 EMIT1
   s" word" JKEY   NMA @ NMU @ JSTR   44 EMIT1
   s" token" JKEY  FAILTK FAILTU @ JSTR  44 EMIT1
   DEADERR @ IF s" dead_owner" JKEY DEADTA @ DEADTU @ JSTR 44 EMIT1 THEN
   s" token_index" JKEY  FAILIX @ JNUM  44 EMIT1
   s" file" JKEY  DIAGFB DIAGFU @ JSTR  44 EMIT1
   s" line" JKEY  JABS-LINE JNUM  44 EMIT1
   s" column" JKEY  JABS-COL JNUM  44 EMIT1
   s" byte_start" JKEY  JABS-BSTART JNUM  44 EMIT1
   s" byte_end" JKEY  JABS-BEND JNUM  44 EMIT1
   s" definition_source" JKEY  TBASE @ TBLEN @ JSTR  44 EMIT1
   SGSEEN @ IF
     s" declared_effect" JKEY
     SGIN @ SGOUT @ SGRIN @ SGROUT @ SGHASR @ JEFFECT  44 EMIT1
     s" declared_effect_source" JKEY
     SGA @ SGU @ SIG-TRIM JSTR  44 EMIT1
   THEN
   s" inferred_effect" JKEY
   SGSEEN @ IF SGIN @ ELSE BROW @ THEN
   DCUR @
   SGHASR @ IF SGRIN @ ELSE RBROW @ THEN
   RCUR @
   SGHASR @ JEFFECT  44 EMIT1
   s" return_stack" JKEY
   123 EMIT1
   s" expected" JKEY  SGHASR @ IF SGROUT @ ELSE RBROW @ THEN JROW  44 EMIT1
   s" actual" JKEY    RCUR @ JROW
   125 EMIT1
   DEXP @ 0 <> IF
     44 EMIT1 s" expected" JKEY DEXP @ JROW
     44 EMIT1 s" actual"   JKEY DACT @ JROW THEN
   44 EMIT1 s" suggestion" JKEY SUGGEST-TEXT JSTR
   125 EMIT1 ;                                            \ }
: DIAG-PRINT
   1 RDST !  0 RSN !  0 RQM !  SEEN-RESET 0 NLET !
   JSON-DIAGS @ IF DIAG-JSON ELSE DIAG-PROSE THEN
   10 EMIT1
   RSBUF RSN @ RDIAG-APPEND
   0 RDST !  0 RSN ! ;
' DIAG-PRINT DIAGXT !

\ --- bad stored-signature diagnostics (multi-error TRUST rows; USIG-ADD-BAD).
\ SGBAD state from the failed parse is still live, so class + suggestion mirror
\ REPAIR-CLASS's signature arm (same stable strings).
: BADSIG-CLASS ( -- ptr u8 n )
   SGBAD-UNKNOWN? IF s" fix_signature_type" EXIT THEN
   SGBAD-BAREPTR? IF s" fix_bare_ptr_element" EXIT THEN
   SGBAD-ARITY? IF s" fix_signature_arity" EXIT THEN
   s" fix_signature_syntax" ;
: BADSIG-SUGGEST ( -- ptr u8 n )
   SGBAD-UNKNOWN? IF s" Use a known stack-signature type or a single-letter type variable." EXIT THEN
   SGBAD-BAREPTR? IF s" Give 'ptr' an element type, e.g. 'ptr u8' or 'ptr a'." EXIT THEN
   SGBAD-ARITY? IF s" Give the type family its exact declared number of arguments." EXIT THEN
   s" Repair the stack-effect comment syntax, including --." ;
: BADSIG-JSON ( ptr u8 n ptr u8 n -- ) {: sa:ptr su:n na:ptr nu:n :}
   123 EMIT1                                              \ {
   s" schema_version" JKEY 1 JNUM 44 EMIT1
   s" code" JKEY s" E-BAD-STORED-SIGNATURE" JSTR 44 EMIT1
   s" repair_class" JKEY BADSIG-CLASS JSTR 44 EMIT1
   s" verdict" JKEY s" rejected" JSTR 44 EMIT1
   s" word" JKEY na nu JSTR 44 EMIT1
   s" declared_effect_source" JKEY sa su SIG-TRIM JSTR 44 EMIT1
   s" file" JKEY DIAGFB DIAGFU @ JSTR 44 EMIT1
   s" suggestion" JKEY BADSIG-SUGGEST JSTR
   125 EMIT1 ;                                            \ }
: BADSIG-PROSE ( ptr u8 n ptr u8 n -- ) {: sa:ptr su:n na:ptr nu:n :}
   s" habu: in " DTXT  na nu DTXT  s" : bad stored signature '" DTXT
   sa su SIG-TRIM DTXT  s" '" DTXT ;
: BADSIG-DIAG ( ptr u8 n ptr u8 n -- ) {: sa:ptr su:n na:ptr nu:n :}
   1 RDST !  0 RSN !
   sa su na nu JSON-DIAGS @ IF BADSIG-JSON ELSE BADSIG-PROSE THEN
   10 EMIT1
   RSBUF RSN @ RDIAG-APPEND
   0 RDST !  0 RSN ! ;
' BADSIG-DIAG BADSIG-XT !

\ --- top-level type-family declaration diagnostics (PLAN item 6). A bad
\ TYPEFAMILY/SUMTYPE reports a declaration-shaped packet: decl kind, family,
\ offending token, and reason — with NO invented definition fields (no
\ declared_effect, definition_source, or return_stack; docs/type-families.md
\ §24). Source-span fields wait on the declaration origin plumbing (item 13).
: TDECL-SUGGEST$ ( -- ptr u8 n )
   s" Repair the family declaration: unique lowercase names, exact arity, closed VARIANT blocks." ;
: TDECL-DIAG-JSON ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- )
   {: ka:ptr ku:n fa:ptr fu:n ta:ptr tu:n wa:ptr wu:n :}
   123 EMIT1                                              \ {
   s" schema_version" JKEY 1 JNUM 44 EMIT1
   s" code" JKEY s" E-BAD-DECLARATION" JSTR 44 EMIT1
   s" repair_class" JKEY s" fix_family_declaration" JSTR 44 EMIT1
   s" verdict" JKEY s" rejected" JSTR 44 EMIT1
   s" decl" JKEY ka ku JSTR 44 EMIT1
   s" family" JKEY fa fu JSTR 44 EMIT1
   s" token" JKEY ta tu JSTR 44 EMIT1
   s" reason" JKEY wa wu JSTR 44 EMIT1
   s" file" JKEY DIAGFB DIAGFU @ JSTR 44 EMIT1
   s" suggestion" JKEY TDECL-SUGGEST$ JSTR
   125 EMIT1 ;                                            \ }
: TDECL-DIAG-PROSE ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- )
   {: ka:ptr ku:n fa:ptr fu:n ta:ptr tu:n wa:ptr wu:n :}
   s" habu: bad " DTXT  ka ku DTXT  s"  declaration '" DTXT  fa fu DTXT
   s" ': " DTXT  wa wu DTXT
   tu 0 > IF s"  at '" DTXT  ta tu DTXT  s" '" DTXT THEN ;
: TDECL-DIAG ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- )
   {: ka:ptr ku:n fa:ptr fu:n ta:ptr tu:n wa:ptr wu:n :}
   1 RDST !  0 RSN !
   ka ku fa fu ta tu wa wu
   JSON-DIAGS @ IF TDECL-DIAG-JSON ELSE TDECL-DIAG-PROSE THEN
   10 EMIT1
   RSBUF RSN @ RDIAG-APPEND
   0 RDST !  0 RSN ! ;

\ REC-SIG ( ptr u8 n -- ) : record a certified sig-less word. Refuses
\ (conservatively, the word stays unrecorded) on unknown tags or absurd var
\ counts — and reports which word and why, since callers otherwise fail later
\ as undefined with no hint that the producer was the problem.
: REC-REFUSE-WHY ( -- ptr u8 n )
   RQM @ IF s" unmodeled type tag in inferred effect"
   ELSE s" more than 26 type variables in inferred effect" THEN ;

: REC-REFUSE-PROSE ( ptr u8 n ptr u8 n -- ) {: na:ptr nu:n wa:ptr wu:n :}
   s" habu: in " DTXT  na nu DTXT
   s" : effect not recorded: " DTXT  wa wu DTXT ;

: REC-REFUSE-JSON ( ptr u8 n ptr u8 n -- ) {: na:ptr nu:n wa:ptr wu:n :}
   123 EMIT1
   s" schema_version" JKEY 1 JNUM 44 EMIT1
   s" code" JKEY s" W-EFFECT-NOT-RECORDED" JSTR 44 EMIT1
   s" word" JKEY na nu JSTR 44 EMIT1
   s" reason" JKEY wa wu JSTR
   125 EMIT1 ;

: REC-REFUSE-EMIT ( ptr u8 n ptr u8 n -- ) {: na:ptr nu:n wa:ptr wu:n :}
   1 RDST !  0 RSN !
   na nu wa wu JSON-DIAGS @ IF REC-REFUSE-JSON ELSE REC-REFUSE-PROSE THEN
   10 EMIT1
   RSBUF RSN @ RDIAG-APPEND
   0 RDST !  0 RSN ! ;

: REC-REFUSE-DIAG ( ptr u8 n -- )
   REC-REFUSE-WHY REC-REFUSE-EMIT ;

: REC-SIG ( ptr u8 n -- ) {: na:ptr nu:n :}
   REND-SIG 2drop        \ rendered only to detect unmodeled tags / var count
   RQM @ 0 =  NLET @ 27 <  and IF na nu CHECKER-USIG-CERT-CURRENT EXIT THEN
   na nu REC-REFUSE-DIAG ;
' REC-SIG RECXT !
