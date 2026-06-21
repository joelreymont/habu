\ source-lex.f — source lexer records for self-hosted tooling.
\ Load after lib/memory.f and tools/lint/lib.f.

1 constant L-WORD
2 constant L-COMMENT
1024 constant LEX-MIN-CAP
2 constant LEX-GROWTH
variable LEX-CAP
variable LKIND-P  variable LADDR-P  variable LLEN-P  variable LBYTE-P
variable LLINE-P  variable LCOL-P   variable LCADDR-P variable LCLEN-P
variable LEX-OLD-P
variable LEX-NEW-P
variable L#   variable LEX-A   variable LEX-U   variable LX
variable LNO  variable LCO  variable LS  variable LSL  variable LSC

TRUSTED: LKIND ( -- ptr n ) LKIND-P @ ;
TRUSTED: LADDR ( -- ptr n ) LADDR-P @ ;
TRUSTED: LLEN  ( -- ptr n ) LLEN-P @ ;
TRUSTED: LBYTE ( -- ptr n ) LBYTE-P @ ;
TRUSTED: LLINE ( -- ptr n ) LLINE-P @ ;
TRUSTED: LCOL  ( -- ptr n ) LCOL-P @ ;
TRUSTED: LCADDR ( -- ptr n ) LCADDR-P @ ;
TRUSTED: LCLEN ( -- ptr n ) LCLEN-P @ ;

TRUSTED: LEX-ALLOC-CELLS ( n -- ptr n )
   cells MEM-ALLOC-BYTES drop ;

TRUSTED: LEX-COPY-CELLS ( ptr n ptr n n -- )
   cells BMOVE ;

: LEX-GROW-ONE ( n ptr n -- ) {: cap ptrp:ptr :}
   ptrp @ LEX-OLD-P !
   cap LEX-ALLOC-CELLS LEX-NEW-P !
   LEX-CAP @ 0 > IF LEX-OLD-P @ LEX-NEW-P @ LEX-CAP @ LEX-COPY-CELLS THEN
   LEX-NEW-P @ ptrp ! ;

: LEX-GROW ( n -- ) {: cap :}
   cap 0 <= IF E-MEM-SIZE throw THEN
   cap LKIND-P LEX-GROW-ONE
   cap LADDR-P LEX-GROW-ONE
   cap LLEN-P LEX-GROW-ONE
   cap LBYTE-P LEX-GROW-ONE
   cap LLINE-P LEX-GROW-ONE
   cap LCOL-P LEX-GROW-ONE
   cap LCADDR-P LEX-GROW-ONE
   cap LCLEN-P LEX-GROW-ONE
   cap LEX-CAP ! ;

: LEX-NEXT-CAP ( -- n )
   LEX-CAP @ 0= IF LEX-MIN-CAP exit THEN
   LEX-CAP @ LEX-GROWTH * ;

: LEX-ENSURE-ROOM ( -- )
   L# @ LEX-CAP @ < IF exit THEN
   LEX-NEXT-CAP LEX-GROW ;

: LEX-ADD  {: kind a u byte line col ca cu :}  ( -- )
   LEX-ENSURE-ROOM
   kind LKIND L# @ cells + !  a LADDR L# @ cells + !  u LLEN L# @ cells + !
   byte LBYTE L# @ cells + !  line LLINE L# @ cells + !  col LCOL L# @ cells + !
   ca LCADDR L# @ cells + !  cu LCLEN L# @ cells + !  L# @ 1+ L# ! ;
: LTOK  ( k -- a u )  dup cells LADDR + @  swap cells LLEN + @ ;
: LCONTENT  ( k -- a u )  dup cells LCADDR + @  swap cells LCLEN + @ ;
: LK@  ( k -- kind )  cells LKIND + @ ;
: LB@  ( k -- byte )  cells LBYTE + @ ;
: LL@  ( k -- line )  cells LLINE + @ ;
: LC@  ( k -- col )  cells LCOL + @ ;
: LEX-END?  ( -- f )  LX @ LEX-U @ >= ;
: LEX-C@  ( -- c )  LEX-A @ LX @ + c@ ;
: LEX-ADV  ( -- c )
   LEX-C@  LX @ 1+ LX !
   dup 10 = IF LNO @ 1+ LNO !  1 LCO ! ELSE LCO @ 1+ LCO ! THEN ;
: LEX-SKIP-QUOTE  ( -- )
   begin LEX-END? 0= while  LEX-ADV DQUOTE = IF exit THEN  repeat ;
: STRING-OPENER?  {: a u :}  ( -- f )
   u 2 <> IF 0 exit THEN
   a 1+ c@ DQUOTE <> IF 0 exit THEN
   a c@ FOLD 115 = IF -1 exit THEN
   a c@ DOT = IF -1 exit THEN
   a c@ FOLD 99 = ;
: LEX-LINE-COMMENT  ( -- )
   begin LEX-END? 0= LEX-C@ 10 <> and while LEX-ADV drop repeat ;
: LEX-PAREN-COMMENT  ( -- )
   LEX-ADV drop  LEX-A @ LX @ + P1A !  LX @ PSTART !
   begin LEX-END? 0= LEX-C@ 41 <> and while LEX-ADV drop repeat
   LEX-A @ LX @ + P1A @ - P1U !
   LEX-END? 0= IF LEX-ADV drop THEN
   L-COMMENT  LEX-A @ LS @ +  LX @ LS @ -  LS @  LSL @ LSC @  P1A @ P1U @  LEX-ADD ;
: LEX-WORD  ( -- )
   begin LEX-END? 0= LEX-C@ WS? 0= and while LEX-ADV drop repeat
   L-WORD  LEX-A @ LS @ +  LX @ LS @ -  LS @  LSL @ LSC @  0 0  LEX-ADD
   L# @ 1- LTOK STRING-OPENER? IF LEX-SKIP-QUOTE THEN ;
: LEX-SOURCE  {: a u :}  ( -- )
   a LEX-A !  u LEX-U !  0 LX !  1 LNO !  1 LCO !  0 L# !
   begin LEX-END? 0= while
      LEX-C@ WS? IF LEX-ADV drop
      ELSE
         LX @ LS !  LNO @ LSL !  LCO @ LSC !
         LEX-C@ 92 = IF LEX-LINE-COMMENT
         ELSE LEX-C@ 40 = IF LEX-PAREN-COMMENT
         ELSE LEX-WORD THEN THEN
      THEN
   repeat ;
