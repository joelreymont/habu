\ source-lex.f — source lexer records for self-hosted tooling.
\ Load after lib/memory.f, lib/vector.f, tools/lint/text.f, tools/lint/token.f, and tools/lint/lib.f.

1 constant L-WORD
2 constant L-COMMENT
1024 constant LEX-MIN-CAP
2 constant LEX-GROWTH
variable LEX-CAP
variable L#   variable LEX-A   variable LEX-U   variable LX
variable LEX-LINE-N  variable LEX-COL-N  variable LEX-START  variable LEX-START-LINE  variable LEX-START-COL
variable LEX-COMMENT-LEN
variable LEX-UNTERM-QUOTE
variable LEX-UNTERM-BYTE
variable LEX-UNTERM-LINE
variable LEX-UNTERM-COL

create LEX-KIND-V VEC-HEADER-CELLS cells allot
create LEX-ADDR-V VEC-HEADER-CELLS cells allot
create LEX-LEN-V VEC-HEADER-CELLS cells allot
create LEX-BYTE-V VEC-HEADER-CELLS cells allot
create LEX-LINE-V VEC-HEADER-CELLS cells allot
create LEX-COL-V VEC-HEADER-CELLS cells allot
create LEX-CADDR-V VEC-HEADER-CELLS cells allot
create LEX-CLEN-V VEC-HEADER-CELLS cells allot

: LEX-A-FIELD ( -- ptr ptr u8 )
   LEX-A 0 ptr-field ;

: LEX-A@ ( -- ptr u8 )
   LEX-A-FIELD @ ;

: LEX-A! ( ptr u8 -- )
   LEX-A-FIELD ! ;

: LEX-INIT-ONE ( ptr a -- )
   LEX-MIN-CAP >COUNT VEC-INIT ;

: LEX-CLEAR-ONE ( ptr a -- )
   VEC-CLEAR ;

: LEX-INIT-VECTORS ( -- )
   LEX-KIND-V LEX-INIT-ONE
   LEX-ADDR-V LEX-INIT-ONE
   LEX-LEN-V LEX-INIT-ONE
   LEX-BYTE-V LEX-INIT-ONE
   LEX-LINE-V LEX-INIT-ONE
   LEX-COL-V LEX-INIT-ONE
   LEX-CADDR-V LEX-INIT-ONE
   LEX-CLEN-V LEX-INIT-ONE
   LEX-MIN-CAP LEX-CAP ! ;

: LEX-CLEAR-VECTORS ( -- )
   LEX-KIND-V LEX-CLEAR-ONE
   LEX-ADDR-V LEX-CLEAR-ONE
   LEX-LEN-V LEX-CLEAR-ONE
   LEX-BYTE-V LEX-CLEAR-ONE
   LEX-LINE-V LEX-CLEAR-ONE
   LEX-COL-V LEX-CLEAR-ONE
   LEX-CADDR-V LEX-CLEAR-ONE
   LEX-CLEN-V LEX-CLEAR-ONE ;

: LEX-RESET-TABLES ( -- )
   LEX-CAP @ 0= if LEX-INIT-VECTORS else LEX-CLEAR-VECTORS then
   0 L# ! ;

: LEX-SYNC-COUNT ( -- )
   LEX-KIND-V VEC-LEN@ LEN>N L# ! ;

: LEX-ADD ( n ptr u8 n n n n ptr u8 n -- ) {: kind a:ptr u byte line col ca:ptr cu :}
   kind LEX-KIND-V VEC-PUSH-N drop
   a LEX-ADDR-V VEC-PUSH-A drop
   u LEX-LEN-V VEC-PUSH-N drop
   byte LEX-BYTE-V VEC-PUSH-N drop
   line LEX-LINE-V VEC-PUSH-N drop
   col LEX-COL-V VEC-PUSH-N drop
   ca LEX-CADDR-V VEC-PUSH-A drop
   cu LEX-CLEN-V VEC-PUSH-N drop
   LEX-SYNC-COUNT ;

: LEX-TOK ( n -- ptr u8 n ) {: k :}
   LEX-ADDR-V k >IDX VEC-A@
   LEX-LEN-V k >IDX VEC-N@ ;

: LCONTENT ( n -- ptr u8 n ) {: k :}
   LEX-CADDR-V k >IDX VEC-A@
   LEX-CLEN-V k >IDX VEC-N@ ;

: LK@ ( n -- n ) {: k :}
   LEX-KIND-V k >IDX VEC-N@ ;

: LB@ ( n -- n ) {: k :}
   LEX-BYTE-V k >IDX VEC-N@ ;

: LL@ ( n -- n ) {: k :}
   LEX-LINE-V k >IDX VEC-N@ ;

: LC@ ( n -- n ) {: k :}
   LEX-COL-V k >IDX VEC-N@ ;

: LEX-END? ( -- bool )
   LX @ LEX-U @ >= ;

: LEX-C@ ( -- n )
   LEX-A@ LX @ + c@ ;

: LEX-ADV ( -- n )
   LEX-C@
   LX @ 1+ LX !
   dup 10 = if LEX-LINE-N @ 1+ LEX-LINE-N ! 1 LEX-COL-N ! else LEX-COL-N @ 1+ LEX-COL-N ! then ;

: LEX-SKIP-QUOTE ( -- bool )
   begin LEX-END? 0= while LEX-ADV DQUOTE = if LINT-TRUE exit then repeat
   LINT-FALSE ;

: LEX-SKIP-ESC-QUOTE ( -- bool )
   begin LEX-END? 0= while
      LEX-ADV dup 92 = if
         drop LEX-END? 0= if LEX-ADV drop then
      else
         DQUOTE = if LINT-TRUE exit then
      then
   repeat
   LINT-FALSE ;

: LEX-MARK-UNTERM-QUOTE ( n -- ) {: k :}
   -1 LEX-UNTERM-QUOTE !
   k LB@ LEX-UNTERM-BYTE !
   k LL@ LEX-UNTERM-LINE !
   k LC@ LEX-UNTERM-COL ! ;

: LEX-UNTERM-QUOTE? ( -- bool )
   LEX-UNTERM-QUOTE @ ;

: LEX-LINE-COMMENT ( -- )
   begin LEX-END? 0= LEX-C@ 10 <> and while LEX-ADV drop repeat ;

: LEX-COMMENT-A ( -- ptr u8 )
   LEX-A@ PSTART @ + ;

: LEX-COMMENT-U ( -- n )
   LEX-COMMENT-LEN @ ;

: LEX-PAREN-COMMENT ( -- )
   LEX-ADV drop
   LX @ PSTART !
   begin LEX-END? 0= LEX-C@ 41 <> and while LEX-ADV drop repeat
   LX @ PSTART @ - LEX-COMMENT-LEN !
   LEX-END? 0= if LEX-ADV drop then
   L-COMMENT LEX-A@ LEX-START @ + LX @ LEX-START @ - LEX-START @ LEX-START-LINE @ LEX-START-COL @
   LEX-COMMENT-A LEX-COMMENT-U LEX-ADD ;

: LEX-WORD ( -- )
   begin LEX-END? 0= LEX-C@ LINT-WS? 0= and while LEX-ADV drop repeat
   L-WORD LEX-A@ LEX-START @ + LX @ LEX-START @ - LEX-START @ LEX-START-LINE @ LEX-START-COL @ LEX-A@ 0 LEX-ADD
   L# @ 1- dup LEX-TOK LINT-ESC-STRING-OPENER? if
      LEX-SKIP-ESC-QUOTE 0= if dup LEX-MARK-UNTERM-QUOTE then
   else
      dup LEX-TOK LINT-NORMAL-STRING-OPENER? if
         LEX-SKIP-QUOTE 0= if dup LEX-MARK-UNTERM-QUOTE then
      then
   then drop ;

: LEX-SOURCE ( ptr u8 n -- ) {: a:ptr u :}
   a LEX-A! u LEX-U ! 0 LX ! 1 LEX-LINE-N ! 1 LEX-COL-N !
   0 LEX-UNTERM-QUOTE !
   LEX-RESET-TABLES
   begin LEX-END? 0= while
      LEX-C@ LINT-WS? if LEX-ADV drop
      else
         LX @ LEX-START !  LEX-LINE-N @ LEX-START-LINE !  LEX-COL-N @ LEX-START-COL !
         LEX-C@ 92 = if LEX-LINE-COMMENT
         else LEX-C@ 40 = if LEX-PAREN-COMMENT
         else LEX-WORD then then
      then
   repeat ;
