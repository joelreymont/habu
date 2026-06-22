\ diag-origin.f - inject checker diagnostic origin markers before definitions.
\ Load after tools/lint/text.f, tools/lint/token.f, and tools/lint/lib.f. Run with bin/hb.

0 set-check

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

create DO-FILE-BUF DO-FILE-CAP allot
create DO-NUM-BUF DO-NUM-CAP allot
create DO-ONE 1 allot

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

: DO-WRITE {: fd a u :} ( fd a u -- )
   u 0= IF exit THEN
   fd a u write u <> IF s" diag-origin: write failed" 74 die THEN ;

: DO-OUT ( a u -- )
   1 -rot DO-WRITE ;

: DO-ERR ( a u -- )
   2 -rot DO-WRITE ;

: DO-C! ( c -- )
   DO-ONE c! ;

: DO-C ( c -- )
   DO-C!
   DO-ONE 1 DO-OUT ;

: DO-ERR-C ( c -- )
   DO-C!
   2 DO-ONE 1 DO-WRITE ;

: DO-USAGE ( -- )
   s" usage: tools/diag-origin.f file" DO-ERR
   DO-LF DO-ERR-C
   64 die ;

: DO-U$ {: u :} ( u -- a u )
   DO-NUM-CAP DO-NUM-I !
   u 0= IF
      DO-NUM-I @ 1- DO-NUM-I !
      48 DO-NUM-BUF DO-NUM-I @ + c!
      DO-NUM-BUF DO-NUM-I @ + 1
      exit
   THEN
   u begin dup 0 > while
      dup 10 mod 48 +
      DO-NUM-I @ 1- DO-NUM-I !
      DO-NUM-BUF DO-NUM-I @ + c!
      10 /
   repeat drop
   DO-NUM-BUF DO-NUM-I @ + DO-NUM-CAP DO-NUM-I @ - ;

: DO-END? ( -- f )
   DO-X @ DO-SRC-U @ >= ;

: DO-C@ ( -- c )
   DO-SRC-A @ DO-X @ + c@ ;

: DO-ADV ( -- c )
   DO-C@
   DO-X @ 1+ DO-X !
   dup DO-LF = IF
      DO-LINE @ 1+ DO-LINE !
      1 DO-COL !
   ELSE
      DO-COL @ 1+ DO-COL !
   THEN ;

: DO-SKIP-LINE ( -- )
   begin DO-END? 0= while
      DO-C@ DO-LF = IF exit THEN
      DO-ADV drop
   repeat ;

: DO-SKIP-QUOTE ( -- )
   begin DO-END? 0= while
      DO-ADV DO-DQ = IF exit THEN
   repeat ;

: DO-STRING-OPENER? {: a u :} ( a u -- f )
   u 2 <> IF 0 exit THEN
   a 1+ c@ DO-DQ <> IF 0 exit THEN
   a c@ FOLD 115 = IF -1 exit THEN
   a c@ FOLD 99 = IF -1 exit THEN
   a c@ 46 = ;

: DO-SAVE-TOKEN {: k start end line col :} ( k start end line col -- )
   k DO-TOK-K !
   DO-SRC-A @ start + DO-TOK-A !
   end start - DO-TOK-U !
   start DO-TOK-BYTE !
   line DO-TOK-LINE !
   col DO-TOK-COL ! ;

: DO-SKIP-IGNORED ( -- )
   begin DO-END? 0= while
      DO-C@ WS? IF
         DO-ADV drop
      ELSE DO-C@ DO-BSLASH = IF
         DO-SKIP-LINE
      ELSE
         exit
      THEN THEN
   repeat ;

: DO-PAREN-TOKEN {: start line col :} ( start line col -- )
   DO-ADV drop
   begin DO-END? 0= while
      DO-C@ DO-RPAREN = IF
         DO-ADV drop
         DO-COMMENT start DO-X @ line col DO-SAVE-TOKEN
         exit
      THEN
      DO-ADV drop
   repeat
   DO-COMMENT start DO-X @ line col DO-SAVE-TOKEN ;

: DO-WORD-TOKEN {: start line col :} ( start line col -- )
   begin DO-END? 0= DO-C@ WS? 0= and while
      DO-ADV drop
   repeat
   DO-WORD start DO-X @ line col DO-SAVE-TOKEN
   DO-TOK-A @ DO-TOK-U @ DO-STRING-OPENER? IF DO-SKIP-QUOTE THEN ;

: DO-NEXT-TOKEN ( -- f )
   DO-SKIP-IGNORED
   DO-END? IF 0 exit THEN
   DO-X @ DO-SAVE-X !
   DO-LINE @ DO-SAVE-LINE !
   DO-COL @ DO-SAVE-COL !
   DO-C@ DO-LPAREN = IF
      DO-SAVE-X @ DO-SAVE-LINE @ DO-SAVE-COL @ DO-PAREN-TOKEN
   ELSE
      DO-SAVE-X @ DO-SAVE-LINE @ DO-SAVE-COL @ DO-WORD-TOKEN
   THEN
   -1 ;

: DO-SAVE-SCAN ( -- )
   DO-X @ DO-SAVE-X !
   DO-LINE @ DO-SAVE-LINE !
   DO-COL @ DO-SAVE-COL ! ;

: DO-RESTORE-SCAN ( -- )
   DO-SAVE-X @ DO-X !
   DO-SAVE-LINE @ DO-LINE !
   DO-SAVE-COL @ DO-COL ! ;

: DO-COLON? ( -- f )
   DO-TOK-K @ DO-WORD <> IF 0 exit THEN
   DO-TOK-U @ 1 <> IF 0 exit THEN
   DO-TOK-A @ c@ DO-COLON-C = ;

: DO-ORIGIN-WORD? ( -- f )
   DO-TOK-K @ DO-WORD = ;

: DO-EMIT-RANGE {: start end :} ( start end -- )
   end start <= IF exit THEN
   DO-SRC-A @ start + end start - DO-OUT ;

: DO-EMIT-UNTIL {: pos :} ( pos -- )
   DO-OUT-X @ pos DO-EMIT-RANGE
   pos DO-OUT-X ! ;

: DO-EMIT-NUM ( u -- )
   DO-U$ DO-OUT ;

: DO-EMIT-MARKER {: line col byte pos :} ( line col byte pos -- )
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
   DO-NEXT-TOKEN IF
      DO-ORIGIN-WORD? IF
         DO-TOK-LINE @ DO-ORIG-LINE !
         DO-TOK-COL @ DO-ORIG-COL !
         DO-TOK-BYTE @ DO-ORIG-BYTE !
      THEN
   THEN
   DO-RESTORE-SCAN
   DO-ORIG-LINE @ DO-ORIG-COL @ DO-ORIG-BYTE @ DO-ORIG-POS @ DO-EMIT-MARKER ;

: DIAG-ORIGIN ( a u -- )
   DO-FILE-BUF DO-FILE-CAP READ-FILE
   DO-SRC-U ! DO-SRC-A !
   0 DO-X !
   0 DO-OUT-X !
   1 DO-LINE !
   1 DO-COL !
   begin DO-NEXT-TOKEN while
      DO-COLON? IF DO-MARK-COLON THEN
   repeat
   DO-OUT-X @ DO-SRC-U @ DO-EMIT-RANGE ;

: DIAG-ORIGIN-MAIN ( -- )
   SCRIPT-ARGC 1 <> IF DO-USAGE THEN
   0 SCRIPT-ARGV$ DIAG-ORIGIN ;

DIAG-ORIGIN-MAIN
