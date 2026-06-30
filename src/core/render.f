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

: DIAG-BUFFER$ ( -- ptr u8 n )
   RDIAG-A @ RDIAG-U @ ;

: RDIAG-COPY ( ptr u8 n -- )
   {: a:ptr u:n :}
   0 RDIAG-I !
   BEGIN RDIAG-I @ u < WHILE
      a RDIAG-I @ + c@
      RDIAG-A @ RDIAG-U @ + RDIAG-I @ + c!
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
      RATOM-I @ cells RATOM-KEY + @ key = IF RATOM-I @ -1 EXIT THEN
      RATOM-I @ 1 + RATOM-I !
   REPEAT
   0 0 ;
: RATOM-ADD ( n -- n ) {: key:n :}
   RATOM-N @ RATOM-CAP >= IF 1 RQM ! 0 EXIT THEN
   key RATOM-N @ cells RATOM-KEY + !
   RATOM-N @
   RATOM-N @ 1 + RATOM-N ! ;
: RATOM-ORD ( n -- n ) {: key:n :}
   key RATOM-FIND IF EXIT THEN drop
   key RATOM-ADD ;

: RSTR {: a u :}  0 BEGIN dup u < WHILE dup a + c@ EMIT1 1 + REPEAT drop ;

: CON-OUT {: p :}
   p 2 = IF 102 EMIT1 ELSE
   p 0 > p CC-MAX < and IF
      p CT-NAME$ dup IF RSTR ELSE 2drop 63 EMIT1 THEN
   ELSE 63 EMIT1 THEN THEN ;

: ATOM-REND {: t :}
   t ATOM>K 0 = IF t ATOM>A t ATOM>U RSTR EXIT THEN
   s" fresh-" RSTR
   t ATOM>A t ATOM>U RSTR
   45 EMIT1
   t ATOM>K RATOM-ORD RATOM-CHAR EMIT1 ;

: PARAM-START {: t :}
   t PARAM>NAME-A t PARAM>NAME-U RSTR  60 EMIT1 ;

\ a quot type renders [ in -- out ] or [ in -- out | rin -- rout ] when the
\ quotation has a non-neutral return-stack effect (two nesting levels; deeper
\ caps at '?').
\ Gap2/3: quot-bearing sigs now RECORD as scheme-strings and round-trip, so
\ combinator call sites (dip, keep) are checked against them. Only a genuine '?'
\ (an unmodeled tag, via RQM) still blocks recording — see REC-SIG below.
create QRBUF 32 cells allot   variable QRBN     \ level-1 nested quot
create Q2BUF 16 cells allot   variable Q2BN     \ level-2 nested quot

\ A quot renders as [ in -- out ] to TWO nesting levels (one buffer per level;
\ a 3rd-level quot caps at '?'). That covers every combinator sig in practice.

: Q2REND-1 {: t :} t T-RES {: r :}              \ level-2 leaf: con | var | ptr | '?'
   r TAG case
      T-VAR of r PAY LET-OF EMIT1 endof
      T-CON of r PAY CON-OUT endof
      T-PTR of s" ptr " RSTR  r PTR>INNER RECURSE endof
      T-ATOM of r ATOM-REND endof
      T-PARAM of
        r PARAM-START
        0 BEGIN dup r PARAM>ARGC < WHILE
          dup 0 > IF 44 EMIT1 THEN
          r over PARAM>ARG RECURSE
          1 +
        REPEAT drop 62 EMIT1
      endof
      63 EMIT1
   endcase ;

: Q2REND-ROW {: row :}  0 Q2BN !  row
   BEGIN R-RES dup TAG S-PUSH = WHILE
     dup P>TYPE Q2BN @ cells Q2BUF + !  Q2BN @ 1 + Q2BN !
     P>REST
   REPEAT drop
   Q2BN @ BEGIN dup 0 > WHILE 1 - dup cells Q2BUF + @ Q2REND-1
     dup 0 > IF 32 EMIT1 THEN REPEAT drop ;

: QRET? ( q -- f ) {: q :}  q Q>RIN R-RES  q Q>ROUT R-RES  <> ;

: Q2RET ( q -- ) {: q :}
   q QRET? IF
      32 EMIT1 124 EMIT1 32 EMIT1
      q Q>RIN Q2REND-ROW 45 EMIT1 45 EMIT1 32 EMIT1
      q Q>ROUT Q2REND-ROW
   THEN ;

: QREND-1 {: t :} t T-RES {: r :}               \ level-1 leaf: con | var | nested quot
   r TAG case
      T-VAR of r PAY LET-OF EMIT1 endof
      T-CON of r PAY CON-OUT endof
      T-PTR of s" ptr " RSTR  r PTR>INNER RECURSE endof
      T-QUOT of                            \ a nested quot -> [ in -- out ... ]
        91 EMIT1 32 EMIT1  r Q>DIN Q2REND-ROW
        45 EMIT1 45 EMIT1 32 EMIT1  r Q>DOUT Q2REND-ROW
        r Q2RET  93 EMIT1
      endof
      T-ATOM of r ATOM-REND endof
      T-PARAM of
        r PARAM-START
        0 BEGIN dup r PARAM>ARGC < WHILE
          dup 0 > IF 44 EMIT1 THEN
          r over PARAM>ARG RECURSE
          1 +
        REPEAT drop 62 EMIT1
      endof
      63 EMIT1
   endcase ;

: QREND-ROW {: row :}  0 QRBN !  row
   BEGIN R-RES dup TAG S-PUSH = WHILE
     dup P>TYPE QRBN @ cells QRBUF + !  QRBN @ 1 + QRBN !
     P>REST
   REPEAT drop
   QRBN @ BEGIN dup 0 > WHILE 1 - dup cells QRBUF + @ QREND-1
     dup 0 > IF 32 EMIT1 THEN REPEAT drop ;

: QRET ( q -- ) {: q :}
   q QRET? IF
      32 EMIT1 124 EMIT1 32 EMIT1
      q Q>RIN QREND-ROW 45 EMIT1 45 EMIT1 32 EMIT1
      q Q>ROUT QREND-ROW
   THEN ;

: REND-TYPE {: t :} t T-RES {: r :}
   r TAG case
      T-VAR of r PAY LET-OF EMIT1 endof
      T-CON of r PAY CON-OUT endof
      T-PTR of s" ptr " RSTR  r PTR>INNER RECURSE endof
      T-QUOT of                                     \ quot<effect> -> [ in -- out ... ]
        91 EMIT1 32 EMIT1  r Q>DIN QREND-ROW
        45 EMIT1 45 EMIT1 32 EMIT1  r Q>DOUT QREND-ROW
        r QRET  93 EMIT1
      endof
      T-ATOM of r ATOM-REND endof
      T-PARAM of
        r PARAM-START
        0 BEGIN dup r PARAM>ARGC < WHILE
          dup 0 > IF 44 EMIT1 THEN
          r over PARAM>ARG RECURSE
          1 +
        REPEAT drop 62 EMIT1
      endof
      63 EMIT1
   endcase ;
create RBUF 64 cells allot   variable RBN
variable RSHOW-DST

: REND-COLLECT {: s :}  0 RBN !  s
   BEGIN R-RES dup TAG S-PUSH = WHILE          \ no locals inside the loop
     dup P>TYPE RBN @ cells RBUF + !  RBN @ 1 + RBN !
     P>REST
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

\ REC-SIG ( na nu -- ) : record a certified word. Refuses (conservatively, the
\ word just stays unrecorded) on unknown tags or absurd var counts.
: REC-SIG {: na nu :}
   REND-SIG
   RQM @ 0 =  NLET @ 27 <  and  IF drop drop na nu CHECKER-USIG-CERT-CURRENT ELSE drop drop THEN ;
' REC-SIG RECXT !

\ DIAG-PRINT ( -- ) : reject diagnostic, one line to stderr —
\   habu: in NAME: at 'TOK' expected: <row> actual: <row>
\ Rows render bottom-to-top with the shared var-letter naming; expected/actual
\ only appear when the failing unify was captured (STEP/SUNI).
: DTXT {: a u :}  0 BEGIN dup u < WHILE dup a + c@ EMIT1 1 + REPEAT drop ;

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
: JSTR {: a u :}  34 EMIT1  0 BEGIN dup u < WHILE dup a + c@ JCHAR 1 + REPEAT drop 34 EMIT1 ;
: JKEY {: a u :}  a u JSTR  58 EMIT1 ;
: JROW {: s :}  34 EMIT1  s DROW  34 EMIT1 ;
: SIG-WS? {: c :}  c 32 =  c 9 = or  c 10 = or  c 13 = or ;
: SIG-LTRIM {: a u :}
   0 BEGIN dup u < WHILE
      dup a + c@ SIG-WS? 0= IF dup a + u rot - EXIT THEN
      1 +
   REPEAT drop a 0 ;
: SIG-RTRIM {: a u :}
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
   DEADERR @ IF s" E-DEAD-CODE" ELSE
   DVERD @ 1 = IF s" E-UNCHECKABLE" ELSE
   SGBAD @ IF SGBAD-UNKNOWN? IF s" E-UNKNOWN-SIGNATURE-TYPE" ELSE SGBAD-BAREPTR? IF s" E-BARE-PTR-SIGNATURE" ELSE s" E-BAD-SIGNATURE" THEN THEN ELSE
   DEXP @ 0 <> IF s" E-MISMATCH" ELSE s" E-REJECTED" THEN THEN THEN THEN THEN THEN ;
: DVERDICT  DVERD @ 1 = IF s" uncheckable" ELSE s" rejected" THEN ;
: RETURN-MISMATCH? ( -- f )
   SGHASR @ IF
      RCUR @ R-RES  SGROUT @ R-RES  <>
   ELSE
      RCUR @ R-RES  RBROW @ R-RES  <>
   THEN ;
: REPAIR-CLASS ( -- a u )
   UNSAFE @ IF s" trusted_boundary_required" EXIT THEN
   LOCALBAD @ IF s" factor_local_shape" EXIT THEN
   DEADERR @ IF s" remove_dead_code" EXIT THEN
   DVERD @ 1 = IF s" rewrite_uncheckable" EXIT THEN
   SGBAD @ IF
      SGBAD-UNKNOWN? IF s" fix_signature_type" ELSE SGBAD-BAREPTR? IF s" fix_bare_ptr_element" ELSE s" fix_signature_syntax" THEN THEN
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
   DEADERR @ IF s" Remove tokens after the terminating control word, or move the work before it." EXIT THEN
   DVERD @ 1 = IF s" Rewrite with modeled words or isolate an audited primitive." EXIT THEN
   SGBAD @ IF
      SGBAD-UNKNOWN? IF
         s" Use a known stack-signature type or a single-letter type variable."
      ELSE SGBAD-BAREPTR? IF
         s" Give 'ptr' an element type, e.g. 'ptr u8' or 'ptr a'."
      ELSE
         s" Repair the stack-effect comment syntax, including --."
      THEN THEN
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
: JLOC-CALC
   1 JLINE !  1 JCOL !  0 JPOS !
   BEGIN JPOS @ FAILB @ <  JPOS @ TBLEN @ <  and WHILE
      TBASE @ JPOS @ + c@ 10 = IF
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
