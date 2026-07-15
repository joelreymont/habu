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

\ ---- raw table cell -> CAD-NUM role bridges for the typed VEC surface ---------
\ The lexer's parallel record columns store raw cells (token / content addresses,
\ lengths, kinds, byte/line/col positions). The typed VEC surface (package VEC)
\ reads a validated CAD-NUM role - a capacity is a `CAD-NUM:item-count`, a record
\ position is a `CAD-NUM:index` - so a count/index role swap at a VEC call is a
\ checker reject. These lift a nonnegative cell to its role through the PUBLIC
\ CAD-NUM validators (no laundering back to n, no reopened package); the refusal
\ arms are unreachable invariants (LEX-MIN-CAP and a live record index are
\ nonnegative), an impossible negative surfaces the vector's own capacity / bounds
\ code. This is the maki/sched-key.f SK>ITEM / SK>INDEX idiom, kept lexer-local.
: LEX>ITEM ( n -- CAD-NUM:item-count )
   CAD-NUM:ITEM-COUNT
   MATCH CAD-NUM:numeric-result
      ok OF ENDOF                             negative OF E-VEC-CAPACITY throw ENDOF
      zero OF E-VEC-CAPACITY throw ENDOF        overflow OF E-VEC-CAPACITY throw ENDOF
      underflow OF E-VEC-CAPACITY throw ENDOF   bad-alignment OF E-VEC-CAPACITY throw ENDOF
      misaligned OF E-VEC-CAPACITY throw ENDOF
   ;MATCH ;
: LEX>INDEX ( n -- CAD-NUM:index )
   CAD-NUM:INDEX
   MATCH CAD-NUM:numeric-result
      ok OF ENDOF                             negative OF E-VEC-BOUNDS throw ENDOF
      zero OF E-VEC-BOUNDS throw ENDOF          overflow OF E-VEC-BOUNDS throw ENDOF
      underflow OF E-VEC-BOUNDS throw ENDOF     bad-alignment OF E-VEC-BOUNDS throw ENDOF
      misaligned OF E-VEC-BOUNDS throw ENDOF
   ;MATCH ;

: LEX-A-FIELD ( -- ptr ptr u8 )
   LEX-A 0 ptr-field ;

: LEX-A@ ( -- ptr u8 )
   LEX-A-FIELD @ ;

: LEX-A! ( ptr u8 -- )
   LEX-A-FIELD ! ;

: LEX-INIT-ONE ( ptr a -- )
   LEX-MIN-CAP LEX>ITEM VEC:INIT ;

: LEX-CLEAR-ONE ( ptr a -- )
   VEC:CLEAR ;

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

\ RAW residual (maki/sched-key.f SK-N precedent): VEC:LEN@ yields a
\ CAD-NUM:item-count and the checker correctly refuses to launder it back to n, but
\ L# is a raw n cache that drives the lexer's raw token arithmetic (L# @ 1- ...),
\ so the count is read through the raw VEC-LEN@ accessor for this word alone.
: LEX-SYNC-COUNT ( -- )
   LEX-KIND-V VEC-LEN@ LEN>N L# ! ;

: LEX-ADD ( n ptr u8 n n n n ptr u8 n -- ) {: kind a:ptr u byte line col ca:ptr cu :}
   kind LEX-KIND-V VEC:PUSH drop
   a LEX-ADDR-V VEC:PUSH drop
   u LEX-LEN-V VEC:PUSH drop
   byte LEX-BYTE-V VEC:PUSH drop
   line LEX-LINE-V VEC:PUSH drop
   col LEX-COL-V VEC:PUSH drop
   ca LEX-CADDR-V VEC:PUSH drop
   cu LEX-CLEN-V VEC:PUSH drop
   LEX-SYNC-COUNT ;

: LEX-TOK ( n -- ptr u8 n ) {: k :}
   LEX-ADDR-V k LEX>INDEX VEC:@
   LEX-LEN-V k LEX>INDEX VEC:@ ;

: LCONTENT ( n -- ptr u8 n ) {: k :}
   LEX-CADDR-V k LEX>INDEX VEC:@
   LEX-CLEN-V k LEX>INDEX VEC:@ ;

: LK@ ( n -- n ) {: k :}
   LEX-KIND-V k LEX>INDEX VEC:@ ;

: LB@ ( n -- n ) {: k :}
   LEX-BYTE-V k LEX>INDEX VEC:@ ;

: LL@ ( n -- n ) {: k :}
   LEX-LINE-V k LEX>INDEX VEC:@ ;

: LC@ ( n -- n ) {: k :}
   LEX-COL-V k LEX>INDEX VEC:@ ;

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
