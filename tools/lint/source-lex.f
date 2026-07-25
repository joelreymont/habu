\ source-lex.f — package LINT-LEX: the one shared source lexer for self-hosted
\ lint, checker, and codegen tooling.
\ Load after lib/memory.f, lib/vector.f, tools/lint/text.f, tools/lint/token.f, and tools/lint/lib.f.
\
\ Public surface (all reads; the package owns every cell):
\   WORD COMMENT                       token kinds returned by KIND@
\   UNTERMINATED-QUOTE                 diagnostic kind returned by ERROR-KIND@
\   SOURCE ( ptr u8 n -- )             scan a buffer; clears all prior state first
\   COUNT ( -- n )                     tokens produced by the last SOURCE
\   TOKEN CONTENT ( n -- ptr u8 n )    token span / paren-comment body span
\   KIND@ BYTE@ LINE@ COL@ ( n -- n )  kind, 0-based byte, 1-based line, 1-based column
\   ERROR? ( -- bool )                 the last scan hit malformed input
\   ERROR-KIND@ ERROR-BYTE@ ERROR-LINE@ ERROR-COL@ ( -- n )
\
\ The diagnostic is one generic record, not a quote-specific flag. A scan writes
\ it at most once: the only writer runs at the malformed site, after which the
\ scanner is already at end of input and the main loop exits. Consumers only read
\ it back, so no caller can mutate lexer state. A consumer that requires valid
\ source must reject when ERROR? is true.

package LINT-LEX
private

1024 constant MIN-CAP
0 constant NO-ERROR

variable CAP
variable TOK-N   variable SRC-A   variable SRC-U   variable POS
variable LINE-N  variable COL-N  variable START  variable START-LINE  variable START-COL
variable CSTART
variable CLEN
variable ERR-KIND
variable ERR-BYTE
variable ERR-LINE
variable ERR-COL

create KIND-V VEC-HEADER-CELLS cells allot
create ADDR-V VEC-HEADER-CELLS cells allot
create LEN-V VEC-HEADER-CELLS cells allot
create BYTE-V VEC-HEADER-CELLS cells allot
create LINE-V VEC-HEADER-CELLS cells allot
create COL-V VEC-HEADER-CELLS cells allot
create CADDR-V VEC-HEADER-CELLS cells allot
create CLEN-V VEC-HEADER-CELLS cells allot

\ ---- raw table cell -> CAD-NUM role bridges for the typed VEC surface ---------
\ The lexer's parallel record columns store raw cells (token / content addresses,
\ lengths, kinds, byte/line/col positions). The typed VEC surface (package VEC)
\ reads a validated CAD-NUM role - a capacity is a `CAD-NUM:item-count`, a record
\ position is a `CAD-NUM:index` - so a count/index role swap at a VEC call is a
\ checker reject. These lift a nonnegative cell to its role through the PUBLIC
\ CAD-NUM validators (no laundering back to n, no reopened package); the refusal
\ arms are unreachable invariants (MIN-CAP and a live record index are
\ nonnegative), an impossible negative surfaces the vector's own capacity / bounds
\ code. This is the maki/sched-key.f SK>ITEM / SK>INDEX idiom, kept lexer-local.
: N>ITEM ( n -- CAD-NUM:item-count )
   CAD-NUM:ITEM-COUNT
   MATCH CAD-NUM:numeric-result
      ok OF ENDOF                             negative OF E-VEC-CAPACITY throw ENDOF
      zero OF E-VEC-CAPACITY throw ENDOF        overflow OF E-VEC-CAPACITY throw ENDOF
      underflow OF E-VEC-CAPACITY throw ENDOF   bad-alignment OF E-VEC-CAPACITY throw ENDOF
      misaligned OF E-VEC-CAPACITY throw ENDOF
   ;MATCH ;
: N>INDEX ( n -- CAD-NUM:index )
   CAD-NUM:INDEX
   MATCH CAD-NUM:numeric-result
      ok OF ENDOF                             negative OF E-VEC-BOUNDS throw ENDOF
      zero OF E-VEC-BOUNDS throw ENDOF          overflow OF E-VEC-BOUNDS throw ENDOF
      underflow OF E-VEC-BOUNDS throw ENDOF     bad-alignment OF E-VEC-BOUNDS throw ENDOF
      misaligned OF E-VEC-BOUNDS throw ENDOF
   ;MATCH ;

: SRC-FIELD ( -- ptr ptr u8 )
   SRC-A 0 ptr-field ;

: SRC@ ( -- ptr u8 )
   SRC-FIELD @ ;

: SRC! ( ptr u8 -- )
   SRC-FIELD ! ;

: INIT-ONE ( ptr a -- )
   MIN-CAP N>ITEM VEC:INIT ;

: CLEAR-ONE ( ptr a -- )
   VEC:CLEAR ;

: INIT-VECTORS ( -- )
   KIND-V INIT-ONE
   ADDR-V INIT-ONE
   LEN-V INIT-ONE
   BYTE-V INIT-ONE
   LINE-V INIT-ONE
   COL-V INIT-ONE
   CADDR-V INIT-ONE
   CLEN-V INIT-ONE
   MIN-CAP CAP ! ;

: CLEAR-VECTORS ( -- )
   KIND-V CLEAR-ONE
   ADDR-V CLEAR-ONE
   LEN-V CLEAR-ONE
   BYTE-V CLEAR-ONE
   LINE-V CLEAR-ONE
   COL-V CLEAR-ONE
   CADDR-V CLEAR-ONE
   CLEN-V CLEAR-ONE ;

: RESET-TABLES ( -- )
   CAP @ 0= if INIT-VECTORS else CLEAR-VECTORS then
   0 TOK-N ! ;

\ RAW residual (maki/sched-key.f SK-N precedent): VEC:LEN@ yields a
\ CAD-NUM:item-count and the checker correctly refuses to launder it back to n, but
\ TOK-N is a raw n cache that drives the lexer's raw token arithmetic
\ (COUNT 1- ...), so the count is read through the raw VEC-LEN@ accessor for this
\ word alone.
: SYNC-COUNT ( -- )
   KIND-V VEC-LEN@ LEN>N TOK-N ! ;

: ADD ( n ptr u8 n n n n ptr u8 n -- ) {: kind:n a:ptr u:n byte:n line:n col:n ca:ptr cu:n :}
   kind KIND-V VEC:PUSH drop
   a ADDR-V VEC:PUSH drop
   u LEN-V VEC:PUSH drop
   byte BYTE-V VEC:PUSH drop
   line LINE-V VEC:PUSH drop
   col COL-V VEC:PUSH drop
   ca CADDR-V VEC:PUSH drop
   cu CLEN-V VEC:PUSH drop
   SYNC-COUNT ;

public

1 constant WORD                 \ KIND@: whitespace-delimited word token
2 constant COMMENT              \ KIND@: `( ... )` comment token, body read via CONTENT

1 constant UNTERMINATED-QUOTE   \ ERROR-KIND@: a string literal ran past end of input

: COUNT ( -- n )
   TOK-N @ ;

: TOKEN ( n -- ptr u8 n ) {: k:n :}
   ADDR-V k N>INDEX VEC:@
   LEN-V k N>INDEX VEC:@ ;

: CONTENT ( n -- ptr u8 n ) {: k:n :}
   CADDR-V k N>INDEX VEC:@
   CLEN-V k N>INDEX VEC:@ ;

: KIND@ ( n -- n ) {: k:n :}
   KIND-V k N>INDEX VEC:@ ;

: BYTE@ ( n -- n ) {: k:n :}
   BYTE-V k N>INDEX VEC:@ ;

: LINE@ ( n -- n ) {: k:n :}
   LINE-V k N>INDEX VEC:@ ;

: COL@ ( n -- n ) {: k:n :}
   COL-V k N>INDEX VEC:@ ;

: ERROR? ( -- bool )
   ERR-KIND @ NO-ERROR = if LINT-FALSE else LINT-TRUE then ;

: ERROR-KIND@ ( -- n )
   ERR-KIND @ ;

\ The three position readers below describe the site named by ERROR-KIND@ and
\ are meaningful only while ERROR? is true; a clean scan leaves them zeroed.
: ERROR-BYTE@ ( -- n )
   ERR-BYTE @ ;

: ERROR-LINE@ ( -- n )
   ERR-LINE @ ;

: ERROR-COL@ ( -- n )
   ERR-COL @ ;

private

: END? ( -- bool )
   POS @ SRC-U @ >= ;

: CUR ( -- n )
   SRC@ POS @ + c@ ;

: ADV ( -- n )
   CUR
   POS @ 1+ POS !
   dup 10 = if LINE-N @ 1+ LINE-N ! 1 COL-N ! else COL-N @ 1+ COL-N ! then ;

: SKIP-QUOTE ( -- bool )
   begin END? 0= while ADV DQUOTE = if LINT-TRUE exit then repeat
   LINT-FALSE ;

: SKIP-ESC-QUOTE ( -- bool )
   begin END? 0= while
      ADV dup 92 = if
         drop END? 0= if ADV drop then
      else
         DQUOTE = if LINT-TRUE exit then
      then
   repeat
   LINT-FALSE ;

: CLEAR-ERROR ( -- )
   NO-ERROR ERR-KIND !
   0 ERR-BYTE !
   0 ERR-LINE !
   0 ERR-COL ! ;

: MARK-UNTERM ( n -- ) {: k:n :}
   UNTERMINATED-QUOTE ERR-KIND !
   k BYTE@ ERR-BYTE !
   k LINE@ ERR-LINE !
   k COL@ ERR-COL ! ;

: LINE-COMMENT ( -- )
   begin END? 0= CUR 10 <> and while ADV drop repeat ;

: BODY-A ( -- ptr u8 )
   SRC@ CSTART @ + ;

: BODY-U ( -- n )
   CLEN @ ;

: PAREN-COMMENT ( -- )
   ADV drop
   POS @ CSTART !
   begin END? 0= CUR 41 <> and while ADV drop repeat
   POS @ CSTART @ - CLEN !
   END? 0= if ADV drop then
   COMMENT SRC@ START @ + POS @ START @ - START @ START-LINE @ START-COL @
   BODY-A BODY-U ADD ;

: SCAN-WORD ( -- )
   begin END? 0= CUR LINT-WS? 0= and while ADV drop repeat
   WORD SRC@ START @ + POS @ START @ - START @ START-LINE @ START-COL @ SRC@ 0 ADD
   COUNT 1- dup TOKEN LINT-ESC-STRING-OPENER? if
      SKIP-ESC-QUOTE 0= if dup MARK-UNTERM then
   else
      dup TOKEN LINT-NORMAL-STRING-OPENER? if
         SKIP-QUOTE 0= if dup MARK-UNTERM then
      then
   then drop ;

public

: SOURCE ( ptr u8 n -- ) {: a:ptr u:n :}
   a SRC! u SRC-U ! 0 POS ! 1 LINE-N ! 1 COL-N !
   CLEAR-ERROR
   RESET-TABLES
   begin END? 0= while
      CUR LINT-WS? if ADV drop
      else
         POS @ START !  LINE-N @ START-LINE !  COL-N @ START-COL !
         CUR 92 = if LINE-COMMENT
         else CUR 40 = if PAREN-COMMENT
         else SCAN-WORD then then
      then
   repeat ;

;package
