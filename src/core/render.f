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

: EMIT1 {: c :}
   c 63 = IF 1 RQM ! THEN
   RDST @ IF
     RSN @ RSBUF-CAP 2 - > IF s" render: sig buffer full" 76 die THEN
     c RSBUF RSN @ + c!  RSN @ 1 + RSN !
   ELSE c ECH c! ECH 1 type THEN ;
create SEEN MAXTV cells allot   variable NLET           \ indexed by typevar (PAY)

: SEEN-RESET 0 BEGIN dup cells SEEN + UNBOUND swap ! 1 + dup MAXTV 1 - > UNTIL drop ;

: LET-OF {: vp :}
   vp cells SEEN + @ UNBOUND = IF NLET @ vp cells SEEN + ! NLET @ 1 + NLET ! THEN
   vp cells SEEN + @ 97 + ;

: RSTR {: a u :}  0 BEGIN dup u < WHILE dup a + c@ EMIT1 1 + REPEAT drop ;

: CON-OUT {: p :}
   p 1 = IF 110 EMIT1 ELSE
   p 2 = IF 102 EMIT1 ELSE
   p 3 = IF 114 EMIT1 ELSE
   p CC-I64  = IF s" i64"  RSTR ELSE
   p CC-U8   = IF s" u8"   RSTR ELSE
   p CC-U32  = IF s" u32"  RSTR ELSE
   p CC-CELL = IF s" cell" RSTR ELSE
   p CC-CHAR = IF s" char" RSTR ELSE
   p CC-STR  = IF s" str"  RSTR ELSE
   p CC-ADDR = IF s" addr" RSTR ELSE
   p CC-BOOL = IF s" bool" RSTR ELSE
   63 EMIT1 THEN THEN THEN THEN THEN THEN THEN THEN THEN THEN THEN ;

\ a quot type renders [ in -- out ] (two nesting levels; deeper caps at '?').
\ Gap2/3: quot-bearing sigs now RECORD as scheme-strings and round-trip, so
\ combinator call sites (dip, keep) are checked against them. Only a genuine '?'
\ (an unmodeled tag, via RQM) still blocks recording — see REC-SIG below.
create QRBUF 32 cells allot   variable QRBN     \ level-1 nested quot
create Q2BUF 16 cells allot   variable Q2BN     \ level-2 nested quot

\ A quot renders as [ in -- out ] to TWO nesting levels (one buffer per level;
\ a 3rd-level quot caps at '?'). That covers every combinator sig in practice.

: Q2REND-1 {: t :} t T-RES {: r :}              \ level-2 leaf: con | var | ptr | '?'
   r TAG T-VAR = IF r PAY LET-OF EMIT1 ELSE
   r TAG T-CON = IF r PAY CON-OUT ELSE
   r TAG T-PTR = IF s" ptr " RSTR  r PTR>INNER RECURSE ELSE 63 EMIT1 THEN THEN THEN ;

: Q2REND-ROW {: row :}  0 Q2BN !  row
   BEGIN R-RES dup TAG S-PUSH = WHILE
     dup P>TYPE Q2BN @ cells Q2BUF + !  Q2BN @ 1 + Q2BN !
     P>REST
   REPEAT drop
   Q2BN @ BEGIN dup 0 > WHILE 1 - dup cells Q2BUF + @ Q2REND-1
     dup 0 > IF 32 EMIT1 THEN REPEAT drop ;

: QREND-1 {: t :} t T-RES {: r :}               \ level-1 leaf: con | var | nested quot
   r TAG T-VAR = IF r PAY LET-OF EMIT1 ELSE
   r TAG T-CON = IF r PAY CON-OUT ELSE
   r TAG T-PTR = IF s" ptr " RSTR  r PTR>INNER RECURSE ELSE
   r TAG T-QUOT = IF                            \ a nested quot -> [ in -- out ]
     91 EMIT1 32 EMIT1  r Q>DIN Q2REND-ROW
     45 EMIT1 45 EMIT1 32 EMIT1  r Q>DOUT Q2REND-ROW  93 EMIT1
   ELSE 63 EMIT1 THEN THEN THEN THEN ;

: QREND-ROW {: row :}  0 QRBN !  row
   BEGIN R-RES dup TAG S-PUSH = WHILE
     dup P>TYPE QRBN @ cells QRBUF + !  QRBN @ 1 + QRBN !
     P>REST
   REPEAT drop
   QRBN @ BEGIN dup 0 > WHILE 1 - dup cells QRBUF + @ QREND-1
     dup 0 > IF 32 EMIT1 THEN REPEAT drop ;

: REND-TYPE {: t :} t T-RES {: r :}
   r TAG T-VAR = IF r PAY LET-OF EMIT1 ELSE
   r TAG T-CON = IF r PAY CON-OUT ELSE
   r TAG T-PTR = IF s" ptr " RSTR  r PTR>INNER RECURSE ELSE
   r TAG T-QUOT = IF                                     \ quot<effect> -> [ in -- out ]
     91 EMIT1 32 EMIT1  r Q>DIN QREND-ROW
     45 EMIT1 45 EMIT1 32 EMIT1  r Q>DOUT QREND-ROW  93 EMIT1
   ELSE 63 EMIT1 THEN THEN THEN THEN ;
create RBUF 64 cells allot   variable RBN

: REND-COLLECT {: s :}  0 RBN !  s
   BEGIN R-RES dup TAG S-PUSH = WHILE          \ no locals inside the loop
     dup P>TYPE RBN @ cells RBUF + !  RBN @ 1 + RBN !
     P>REST
   REPEAT drop ;

\ RENDER ( -- ) : print DCUR's residual stack bottom-to-top, space-separated.
: RENDER  SEEN-RESET 0 NLET !  DCUR @ REND-COLLECT
   RBN @ BEGIN dup 0 > WHILE 1 - dup cells RBUF + @ REND-TYPE 32 EMIT1 REPEAT drop ;

\ REND-SIG ( -- a u ) : render the just-checked word's effect "in -- out" —
\ inputs from the base row's instantiation (BROW), outputs from DCUR.
: REND-SIG
   1 RDST !  0 RSN !  0 RQM !  SEEN-RESET 0 NLET !
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
   RQM @ 0 =  NLET @ 27 <  and  IF na nu USIG-ADD ELSE drop drop THEN ;
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
   c 10 = IF 92 EMIT1 110 EMIT1 EXIT THEN
   c 13 = IF 92 EMIT1 114 EMIT1 EXIT THEN
   c 9 = IF 92 EMIT1 116 EMIT1 EXIT THEN
   c 34 =  c 92 = or IF 92 EMIT1 THEN  c EMIT1 ;
: JSTR {: a u :}  34 EMIT1  0 BEGIN dup u < WHILE dup a + c@ JCHAR 1 + REPEAT drop 34 EMIT1 ;
: JKEY {: a u :}  a u JSTR  58 EMIT1 ;
: JROW {: s :}  34 EMIT1  s DROW  34 EMIT1 ;
: JEFFECT {: din dout rin rout hasr :}
   34 EMIT1
   din DROW  s" -- " DTXT  dout DROW
   hasr IF s" | " DTXT  rin DROW  s" -- " DTXT  rout DROW THEN
   34 EMIT1 ;
: DCODE
   UNSAFE @ IF s" E-UNSAFE" ELSE
   DVERD @ 1 = IF s" E-UNCHECKABLE" ELSE
   SGBAD @ IF s" E-BAD-SIGNATURE" ELSE
   DEXP @ 0 <> IF s" E-MISMATCH" ELSE s" E-REJECTED" THEN THEN THEN THEN ;
: DVERDICT  DVERD @ 1 = IF s" uncheckable" ELSE s" rejected" THEN ;
: RETURN-MISMATCH? ( -- f )
   SGHASR @ IF
      RCUR @ R-RES  SGROUT @ R-RES  <>
   ELSE
      RCUR @ R-RES  RBROW @ R-RES  <>
   THEN ;
: REPAIR-CLASS ( -- a u )
   UNSAFE @ IF s" trusted_boundary_required" EXIT THEN
   DVERD @ 1 = IF s" rewrite_uncheckable" EXIT THEN
   SGBAD @ IF s" fix_signature_syntax" EXIT THEN
   DEXP @ 0= IF
      RETURN-MISMATCH? IF s" fix_return_stack" ELSE s" unknown_rejection" THEN
      EXIT
   THEN
   DEXP @ REND-COLLECT RBN @ DSUGE !
   DACT @ REND-COLLECT RBN @ DSUGA !
   DSUGA @ DSUGE @ > IF s" remove_producer" ELSE
   DSUGA @ DSUGE @ < IF s" add_producer" ELSE
   s" fix_type" THEN THEN ;
\ a length-based repair hint: more values out than declared (remove a producer),
\ fewer (consumes too much), or equal (a type mismatch — fix the body not the sig).
: SUGGEST-TEXT ( -- a u )
   UNSAFE @ IF s" compiler-manipulating words need an audited trusted boundary; remove this token from checked code" EXIT THEN
   DVERD @ 1 = IF s" checker could not infer this word; rewrite with modeled words or add TRUST only for audited primitives" EXIT THEN
   DEXP @ 0= IF s" rejected without a captured stack mismatch; inspect the token and declared signature" EXIT THEN
   DEXP @ REND-COLLECT RBN @ DSUGE !
   DACT @ REND-COLLECT RBN @ DSUGA !
   DSUGA @ DSUGE @ > IF  s" the body leaves more values than declared; remove a producer or drop the extra value"
   ELSE DSUGA @ DSUGE @ < IF  s" the body leaves fewer values than declared; add or restore a producer, or stop consuming a required value"
   ELSE  s" type mismatch at this token — fix the body to match the signature, do not weaken the signature"
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
   s" habu: in " DTXT  NMA @ NMU @ DTXT  s" : at '" DTXT  FAILTK FAILTU @ DTXT
   s" '" DTXT
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
   2 RSBUF RSN @ write drop
   0 RDST !  0 RSN ! ;
' DIAG-PRINT DIAGXT !
