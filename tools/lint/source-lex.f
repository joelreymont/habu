\ source-lex.f — source lexer records for self-hosted tooling.
\ Load after tools/lint/lib.f.

1 constant L-WORD
2 constant L-COMMENT
7936 constant LEX-MAX
create LKIND LEX-MAX cells allot   create LADDR LEX-MAX cells allot
create LLEN  LEX-MAX cells allot   create LBYTE LEX-MAX cells allot
create LLINE LEX-MAX cells allot   create LCOL  LEX-MAX cells allot
create LCADDR LEX-MAX cells allot  create LCLEN LEX-MAX cells allot
variable L#   variable LEX-A   variable LEX-U   variable LX
variable LNO  variable LCO  variable LS  variable LSL  variable LSC

: LEX-ADD  {: kind a u byte line col ca cu :}  ( -- )
   L# @ LEX-MAX >= IF s" lint: source token overflow" type cr 1 die THEN
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
