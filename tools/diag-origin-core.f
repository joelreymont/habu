\ diag-origin-core.f - inject checker diagnostic origin markers before definitions.
\ Load after lib/errors.f, lib/string.f, lib/memory.f, tools/lint/text.f,
\ tools/lint/token.f, and tools/lint/lib.f.

$40000 constant DO-FILE-CAP
32 constant DO-NUM-CAP

1 constant DO-WORD
2 constant DO-COMMENT

10 constant DO-LF
32 constant DO-SP
34 constant DO-DQ
40 constant DO-LPAREN
41 constant DO-RPAREN
58 constant DO-COLON-C
92 constant DO-BSLASH

create DO-NUM-BUF DO-NUM-CAP allot
create DO-ONE 1 allot

variable DO-FILE-A
variable DO-SRC-A
variable DO-SRC-U
variable DO-X
variable DO-LINE
variable DO-COL
variable DO-OUT-X
variable DO-NUM-I

variable DO-TOK-K
variable DO-TOK-A
variable DO-TOK-U
variable DO-TOK-BYTE
variable DO-TOK-LINE
variable DO-TOK-COL

variable DO-SAVE-X
variable DO-SAVE-LINE
variable DO-SAVE-COL
variable DO-ORIG-LINE
variable DO-ORIG-COL
variable DO-ORIG-BYTE
variable DO-ORIG-POS
variable DO-OUT-A
variable DO-OUT-U
variable DO-OUT-CAP
variable DO-OUT-BUF?

: DO-TRUE ( -- bool )
   0 0= ;

: DO-FALSE ( -- bool )
   DO-TRUE 0= ;

: DO-FILE-A-FIELD ( -- ptr ptr u8 )
   DO-FILE-A 0 ptr-field ;

: DO-FILE-A@ ( -- ptr u8 )
   DO-FILE-A-FIELD @ ;

: DO-FILE-A! ( ptr u8 -- )
   DO-FILE-A-FIELD ! ;

: DO-ALLOC-FILE-BUF ( -- )
   DO-FILE-A @ 0= if
      DO-FILE-CAP MEM-ALLOC-BYTES drop DO-FILE-A!
   then ;

: DO-FILE-BUF ( -- ptr u8 )
   DO-ALLOC-FILE-BUF
   DO-FILE-A@ ;

: DO-SRC-A-FIELD ( -- ptr ptr u8 )
   DO-SRC-A 0 ptr-field ;

: DO-SRC-A@ ( -- ptr u8 )
   DO-SRC-A-FIELD @ ;

: DO-SRC-A! ( ptr u8 -- )
   DO-SRC-A-FIELD ! ;

: DO-TOK-A-FIELD ( -- ptr ptr u8 )
   DO-TOK-A 0 ptr-field ;

: DO-TOK-A@ ( -- ptr u8 )
   DO-TOK-A-FIELD @ ;

: DO-TOK-A! ( ptr u8 -- )
   DO-TOK-A-FIELD ! ;

: DO-OUT-A-FIELD ( -- ptr ptr u8 )
   DO-OUT-A 0 ptr-field ;

: DO-OUT-A@ ( -- ptr u8 )
   DO-OUT-A-FIELD @ ;

: DO-OUT-A! ( ptr u8 -- )
   DO-OUT-A-FIELD ! ;

: DO-FAIL ( ptr u8 n n -- )
   die ;

: DO-WRITE ( n ptr u8 n -- ) {: fd a:ptr u :}
   u 0= if exit then
   fd a u write u <> if s" diag-origin: write failed" 74 DO-FAIL then ;

: DO-BUF-ROOM ( n -- ) {: u :}
   u 0 < if s" diag-origin: negative output" 74 DO-FAIL then
   DO-OUT-U @ u + DO-OUT-CAP @ > if s" diag-origin: output too large" 74 DO-FAIL then ;

: DO-BUF-OUT ( ptr u8 n -- ) {: a:ptr u :}
   u DO-BUF-ROOM
   a DO-OUT-A@ DO-OUT-U @ + u BYTE-COPY
   DO-OUT-U @ u + DO-OUT-U ! ;

: DO-FD-OUT ( ptr u8 n -- )
   1 -rot DO-WRITE ;

: DO-OUT ( ptr u8 n -- )
   DO-OUT-BUF? @ if DO-BUF-OUT else DO-FD-OUT then ;

: DO-ERR ( ptr u8 n -- )
   2 -rot DO-WRITE ;

: DO-C! ( n -- )
   DO-ONE c! ;

: DO-C ( n -- )
   DO-C!
   DO-ONE 1 DO-OUT ;

: DO-ERR-C ( n -- )
   DO-C!
   2 DO-ONE 1 DO-WRITE ;

: DO-USAGE ( -- )
   s" usage: tools/diag-origin.f file" 64 DO-FAIL ;

: DO-U$ ( n -- ptr u8 n ) {: u :}
   DO-NUM-CAP DO-NUM-I !
   u 0= if
      DO-NUM-I @ 1- DO-NUM-I !
      48 DO-NUM-BUF DO-NUM-I @ + c!
      DO-NUM-BUF DO-NUM-I @ + 1
      exit
   then
   u begin dup 0 > while
      dup 10 mod 48 +
      DO-NUM-I @ 1- DO-NUM-I !
      DO-NUM-BUF DO-NUM-I @ + c!
      10 /
   repeat drop
   DO-NUM-BUF DO-NUM-I @ + DO-NUM-CAP DO-NUM-I @ - ;

: DO-END? ( -- bool )
   DO-X @ DO-SRC-U @ >= ;

: DO-C@ ( -- n )
   DO-SRC-A@ DO-X @ + c@ ;

: DO-ADV ( -- n )
   DO-C@
   DO-X @ 1+ DO-X !
   dup DO-LF = if
      DO-LINE @ 1+ DO-LINE !
      1 DO-COL !
   else
      DO-COL @ 1+ DO-COL !
   then ;

: DO-SKIP-LINE ( -- )
   begin DO-END? 0= while
      DO-C@ DO-LF = if exit then
      DO-ADV drop
   repeat ;

: DO-SKIP-QUOTE ( -- )
   begin DO-END? 0= while
      DO-ADV DO-DQ = if exit then
   repeat ;

: DO-STRING-OPENER? ( ptr u8 n -- bool ) {: a:ptr u :}
   u 2 <> if DO-FALSE exit then
   a 1+ c@ DO-DQ <> if DO-FALSE exit then
   a c@ LINT-FOLD 115 = if DO-TRUE exit then
   a c@ LINT-FOLD 99 = if DO-TRUE exit then
   a c@ 46 = ;

: DO-SAVE-TOKEN ( n n n n n -- ) {: k start end line col :}
   k DO-TOK-K !
   DO-SRC-A@ start + DO-TOK-A!
   end start - DO-TOK-U !
   start DO-TOK-BYTE !
   line DO-TOK-LINE !
   col DO-TOK-COL ! ;

: DO-SKIP-IGNORED ( -- )
   begin DO-END? 0= while
      DO-C@ WS? if
         DO-ADV drop
      else DO-C@ DO-BSLASH = if
         DO-SKIP-LINE
      else
         exit
      then then
   repeat ;

: DO-PAREN-TOKEN ( n n n -- ) {: start line col :}
   DO-ADV drop
   begin DO-END? 0= while
      DO-C@ DO-RPAREN = if
         DO-ADV drop
         DO-COMMENT start DO-X @ line col DO-SAVE-TOKEN
         exit
      then
      DO-ADV drop
   repeat
   DO-COMMENT start DO-X @ line col DO-SAVE-TOKEN ;

: DO-WORD-TOKEN ( n n n -- ) {: start line col :}
   begin DO-END? 0= DO-C@ WS? 0= and while
      DO-ADV drop
   repeat
   DO-WORD start DO-X @ line col DO-SAVE-TOKEN
   DO-TOK-A@ DO-TOK-U @ DO-STRING-OPENER? if DO-SKIP-QUOTE then ;

: DO-NEXT-TOKEN ( -- bool )
   DO-SKIP-IGNORED
   DO-END? if DO-FALSE exit then
   DO-X @ DO-SAVE-X !
   DO-LINE @ DO-SAVE-LINE !
   DO-COL @ DO-SAVE-COL !
   DO-C@ DO-LPAREN = if
      DO-SAVE-X @ DO-SAVE-LINE @ DO-SAVE-COL @ DO-PAREN-TOKEN
   else
      DO-SAVE-X @ DO-SAVE-LINE @ DO-SAVE-COL @ DO-WORD-TOKEN
   then
   DO-TRUE ;

: DO-SAVE-SCAN ( -- )
   DO-X @ DO-SAVE-X !
   DO-LINE @ DO-SAVE-LINE !
   DO-COL @ DO-SAVE-COL ! ;

: DO-RESTORE-SCAN ( -- )
   DO-SAVE-X @ DO-X !
   DO-SAVE-LINE @ DO-LINE !
   DO-SAVE-COL @ DO-COL ! ;

: DO-COLON? ( -- bool )
   DO-TOK-K @ DO-WORD <> if DO-FALSE exit then
   DO-TOK-U @ 1 <> if DO-FALSE exit then
   DO-TOK-A@ c@ DO-COLON-C = ;

: DO-ORIGIN-WORD? ( -- bool )
   DO-TOK-K @ DO-WORD = ;

: DO-EMIT-RANGE ( n n -- ) {: start end :}
   end start <= if exit then
   DO-SRC-A@ start + end start - DO-OUT ;

: DO-EMIT-UNTIL ( n -- ) {: pos :}
   DO-OUT-X @ pos DO-EMIT-RANGE
   pos DO-OUT-X ! ;

: DO-EMIT-NUM ( u -- )
   DO-U$ DO-OUT ;

: DO-EMIT-MARKER ( n n n n -- ) {: line col byte pos :}
   pos DO-EMIT-UNTIL
   DO-LF DO-C
   line DO-EMIT-NUM DO-SP DO-C
   col DO-EMIT-NUM DO-SP DO-C
   byte DO-EMIT-NUM
   s"  DIAG-ORIGIN!" DO-OUT
   DO-LF DO-C ;

: DO-MARK-COLON ( -- )
   DO-TOK-LINE @ DO-ORIG-LINE !
   DO-TOK-COL @ DO-ORIG-COL !
   DO-TOK-BYTE @ DO-ORIG-BYTE !
   DO-TOK-BYTE @ DO-ORIG-POS !
   DO-SAVE-SCAN
   DO-NEXT-TOKEN if
      DO-ORIGIN-WORD? if
         DO-TOK-LINE @ DO-ORIG-LINE !
         DO-TOK-COL @ DO-ORIG-COL !
         DO-TOK-BYTE @ DO-ORIG-BYTE !
      then
   then
   DO-RESTORE-SCAN
   DO-ORIG-LINE @ DO-ORIG-COL @ DO-ORIG-BYTE @ DO-ORIG-POS @ DO-EMIT-MARKER ;

: DO-OUT-FD! ( -- )
   DO-FALSE DO-OUT-BUF? !
   0 DO-OUT-U ! ;

: DO-OUT-BUF! ( ptr u8 len -- ) {: out:ptr cap :}
   out DO-OUT-A!
   cap LEN>N DO-OUT-CAP !
   0 DO-OUT-U !
   DO-TRUE DO-OUT-BUF? ! ;

: DIAG-ORIGIN-RUN ( ptr u8 n -- )
   DO-FILE-BUF DO-FILE-CAP READ-FILE
   DO-SRC-U ! DO-SRC-A!
   0 DO-X !
   0 DO-OUT-X !
   1 DO-LINE !
   1 DO-COL !
   begin DO-NEXT-TOKEN while
      DO-COLON? if DO-MARK-COLON then
   repeat
   DO-OUT-X @ DO-SRC-U @ DO-EMIT-RANGE ;

: DIAG-ORIGIN ( ptr u8 n -- )
   DO-OUT-FD!
   DIAG-ORIGIN-RUN ;

: DIAG-ORIGIN>BUF ( ptr u8 n ptr u8 len -- len ) {: path:ptr pathu out:ptr cap :}
   out cap DO-OUT-BUF!
   path pathu DIAG-ORIGIN-RUN
   DO-OUT-U @ >LEN ;
