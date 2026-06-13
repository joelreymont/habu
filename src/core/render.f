\ render.fs — render the checker's inferred residual stack (DCUR) back to readable
\ type names. Type variables get canonical letters a,b,c… (assigned bottom-to-top),
\ int=n, flag=f, float=r. The "render" half of the native sigparse/checker. Needs
\ checker.fs. Standalone has no emit/+!; chars go through a 1-byte buffer + type.
\ Also the checker's sig RECORDER: certified words render "in -- out" to a buffer
\ and append it to USIGS (installed via RECXT), so callers of certified words
\ certify too.
create ECH 1 allot
variable RDST   0 RDST !                 \ 0 = stdout, 1 = RSBUF (sig recording)
create RSBUF 512 allot   variable RSN
variable RQM                             \ a '?' rendered = unknown tag, don't record

: EMIT1 {: c :}
   c 63 = IF 1 RQM ! THEN
   RDST @ IF
     RSN @ 510 > IF s" render: sig buffer full" 76 die THEN
     c RSBUF RSN @ + c!  RSN @ 1 + RSN !
   ELSE c ECH c! ECH 1 type THEN ;
create SEEN MAXTV cells allot   variable NLET           \ indexed by typevar (PAY)

: SEEN-RESET 0 BEGIN dup cells SEEN + UNBOUND swap ! 1 + dup MAXTV 1 - > UNTIL drop ;

: LET-OF {: vp :}
   vp cells SEEN + @ UNBOUND = IF NLET @ vp cells SEEN + ! NLET @ 1 + NLET ! THEN
   vp cells SEEN + @ 97 + ;

: CON-CH {: p :} p 1 = IF 110 ELSE p 2 = IF 102 ELSE p 3 = IF 114 ELSE 99 THEN THEN THEN ;   \ n / f / r

\ a quot type renders '?' — quot-bearing sigs are never RECORDED (the native
\ sig grammar can't express them yet); inside one word they check fully.
create QRBUF 32 cells allot   variable QRBN     \ level-1 nested quot
create Q2BUF 16 cells allot   variable Q2BN     \ level-2 nested quot

\ A quot renders as [ in -- out ] to TWO nesting levels (one buffer per level;
\ a 3rd-level quot caps at '?'). That covers every combinator sig in practice.

: Q2REND-1 {: t :} t T-RES {: r :}              \ level-2 leaf: con | var | '?'
   r TAG T-VAR = IF r PAY LET-OF EMIT1 ELSE
   r TAG T-CON = IF r PAY CON-CH EMIT1 ELSE 63 EMIT1 THEN THEN ;

: Q2REND-ROW {: row :}  0 Q2BN !  row
   BEGIN R-RES dup TAG S-PUSH = WHILE
     dup P>TYPE Q2BN @ cells Q2BUF + !  Q2BN @ 1 + Q2BN !
     P>REST
   REPEAT drop
   Q2BN @ BEGIN dup 0 > WHILE 1 - dup cells Q2BUF + @ Q2REND-1
     dup 0 > IF 32 EMIT1 THEN REPEAT drop ;

: QREND-1 {: t :} t T-RES {: r :}               \ level-1 leaf: con | var | nested quot
   r TAG T-VAR = IF r PAY LET-OF EMIT1 ELSE
   r TAG T-CON = IF r PAY CON-CH EMIT1 ELSE
   r TAG T-QUOT = IF                            \ a nested quot -> [ in -- out ]
     91 EMIT1 32 EMIT1  r Q>DIN Q2REND-ROW
     45 EMIT1 45 EMIT1 32 EMIT1  r Q>DOUT Q2REND-ROW  93 EMIT1
   ELSE 63 EMIT1 THEN THEN THEN ;

: QREND-ROW {: row :}  0 QRBN !  row
   BEGIN R-RES dup TAG S-PUSH = WHILE
     dup P>TYPE QRBN @ cells QRBUF + !  QRBN @ 1 + QRBN !
     P>REST
   REPEAT drop
   QRBN @ BEGIN dup 0 > WHILE 1 - dup cells QRBUF + @ QREND-1
     dup 0 > IF 32 EMIT1 THEN REPEAT drop ;

: REND-TYPE {: t :} t T-RES {: r :}
   r TAG T-VAR = IF r PAY LET-OF EMIT1 ELSE
   r TAG T-CON = IF r PAY CON-CH EMIT1 ELSE
   r TAG T-QUOT = IF                                     \ quot<effect> -> [ in -- out ]
     91 EMIT1 32 EMIT1  r Q>DIN QREND-ROW
     45 EMIT1 45 EMIT1 32 EMIT1  r Q>DOUT QREND-ROW  93 EMIT1
   ELSE 63 EMIT1 THEN THEN THEN ;
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

: DIAG-PRINT
   1 RDST !  0 RSN !  0 RQM !  SEEN-RESET 0 NLET !
   s" habu: in " DTXT  NMA @ NMU @ DTXT  s" : at '" DTXT  FAILTK FAILTU @ DTXT
   s" '" DTXT
   DEXP @ 0 <> IF
     s"  expected: " DTXT  DEXP @ DROW
     s" actual: " DTXT  DACT @ DROW THEN
   10 EMIT1
   2 RSBUF RSN @ write drop
   0 RDST !  0 RSN ! ;
' DIAG-PRINT DIAGXT !
