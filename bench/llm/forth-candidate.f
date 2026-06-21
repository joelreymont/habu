\ forth-candidate.f - checked Forth candidate source scanner.
\
\ Load after lib/errors.f, lib/string.f, and bench/llm/manifest.f.

10 constant FC-LF
34 constant FC-DQ
40 constant FC-LPAREN
41 constant FC-RPAREN
46 constant FC-DOT
58 constant FC-COLON
59 constant FC-SEMI
92 constant FC-BACKSLASH
99 constant FC-C
115 constant FC-S

-3260 constant E-FC-CAPACITY

variable FC-LINE-NEXT
variable FC-STARTED
variable FC-DONE
variable FC-OUT-U
variable FC-IDX
variable FC-END
variable FC-SIG-OPEN
variable FC-SIG-CLOSE
variable FC-SCAN-IDX
variable FC-SCAN-END

: FC-TRUE ( -- bool )
   0 0= ;

: FC-FALSE ( -- bool )
   FC-TRUE 0= ;

: FC-BUF-ROOM ( n n n -- ) {: add cap used :}
   add 0 < if E-FC-CAPACITY throw then
   add cap used - > if E-FC-CAPACITY throw then ;

: FC-BUF+ ( ptr u8 n ptr u8 n -- ) {: a:ptr u dst:ptr cap :}
   u cap FC-OUT-U @ FC-BUF-ROOM
   a dst FC-OUT-U @ + u BYTE-COPY
   FC-OUT-U @ u + FC-OUT-U ! ;

: FC-BUF-C ( n ptr u8 n -- ) {: c dst:ptr cap :}
   1 cap FC-OUT-U @ FC-BUF-ROOM
   c dst FC-OUT-U @ + c!
   FC-OUT-U @ 1+ FC-OUT-U ! ;

: FC-BUF-LN ( ptr u8 n ptr u8 n -- ) {: a:ptr u dst:ptr cap :}
   a u dst cap FC-BUF+
   FC-LF dst cap FC-BUF-C ;

: FC-SPACE? ( n -- bool )
   dup STR-SPACE = over STR-TAB = or over STR-LF = or swap STR-CR = or ;

: FC-SKIP-SPACE ( ptr u8 n n -- n ) {: a:ptr u idx :}
   idx begin dup u < while
      dup a + c@ FC-SPACE? if 1+ else exit then
   repeat ;

: FC-SCAN-WORD-END ( ptr u8 n n -- n ) {: a:ptr u idx :}
   idx begin dup u < while
      dup a + c@ FC-SPACE? if exit then
      1+
   repeat ;

: FC-FENCE-LINE? ( ptr u8 n -- bool )
   TRIM s" ```" STARTS-WITH? ;

: FC-CODE-LINE? ( ptr u8 n -- bool )
   TRIM dup 0= if 2drop FC-FALSE exit then
   drop c@ FC-COLON = ;

: FC-LINE-SEMI? ( ptr u8 n -- bool )
   FC-SEMI INDEX-OF 0 >= ;

: FC-EXTRACT-RESET ( -- )
   0 FC-LINE-NEXT !
   0 FC-STARTED !
   0 FC-DONE !
   0 FC-OUT-U ! ;

: FC-EXTRACT-LINE ( ptr u8 n ptr u8 n -- ) {: line:ptr lineu dst:ptr cap :}
   FC-DONE @ if exit then
   line lineu FC-FENCE-LINE? if exit then
   FC-STARTED @ 0= if
      line lineu FC-CODE-LINE? 0= if exit then
      -1 FC-STARTED !
   then
   line lineu dst cap FC-BUF-LN
   line lineu FC-LINE-SEMI? if -1 FC-DONE ! then ;

: FC-EXTRACT-CANDIDATE ( ptr u8 n ptr u8 n -- n bool )
   {: src:ptr srcu dst:ptr cap :}
   FC-EXTRACT-RESET
   begin src srcu FC-LINE-NEXT @ BM-LINE-NEXT while
      FC-LINE-NEXT !
      dst cap FC-EXTRACT-LINE
   repeat drop 2drop
   FC-OUT-U @
   FC-STARTED @ if FC-TRUE else FC-FALSE then ;

: FC-FIRST-DEF-LINE$ ( ptr u8 n -- ptr u8 n bool ) {: src:ptr srcu :}
   0 FC-LINE-NEXT !
   begin src srcu FC-LINE-NEXT @ BM-LINE-NEXT while
      FC-LINE-NEXT !
      2dup FC-FENCE-LINE? if
         2drop
      else
         2dup FC-CODE-LINE? if TRIM FC-TRUE exit then
         2drop
      then
   repeat drop 2drop
   s" " FC-FALSE ;

: FC-TRIMMED-LINE-NAME$ ( ptr u8 n -- ptr u8 n bool ) {: a:ptr u :}
   u 2 < if a 0 FC-FALSE exit then
   a c@ FC-COLON <> if a 0 FC-FALSE exit then
   a u 1 FC-SKIP-SPACE FC-IDX !
   FC-IDX @ u >= if a 0 FC-FALSE exit then
   a u FC-IDX @ FC-SCAN-WORD-END FC-END !
   a FC-IDX @ + FC-END @ FC-IDX @ - FC-TRUE ;

: FC-LINE-NAME$ ( ptr u8 n -- ptr u8 n bool )
   TRIM FC-TRIMMED-LINE-NAME$ ;

: FC-FIRST-NAME$ ( ptr u8 n -- ptr u8 n bool )
   FC-FIRST-DEF-LINE$ if FC-LINE-NAME$ else 2drop s" " FC-FALSE then ;

: FC-TRIMMED-LINE-SIG$ ( ptr u8 n -- ptr u8 n bool ) {: a:ptr u :}
   a u FC-LPAREN INDEX-OF FC-SIG-OPEN !
   FC-SIG-OPEN @ 0 < if a 0 FC-FALSE exit then
   a FC-SIG-OPEN @ 1+ + u FC-SIG-OPEN @ 1+ - FC-RPAREN INDEX-OF FC-SIG-CLOSE !
   FC-SIG-CLOSE @ 0 < if a 0 FC-FALSE exit then
   a FC-SIG-OPEN @ 1+ + FC-SIG-CLOSE @ TRIM FC-TRUE ;

: FC-LINE-SIG$ ( ptr u8 n -- ptr u8 n bool )
   TRIM FC-TRIMMED-LINE-SIG$ ;

: FC-FIRST-SIG$ ( ptr u8 n -- ptr u8 n bool )
   FC-FIRST-DEF-LINE$ if FC-LINE-SIG$ else 2drop s" " FC-FALSE then ;

: FC-COMPLETE? ( ptr u8 n -- bool )
   FC-LINE-SEMI? ;

: FC-TOKEN-CI= ( ptr u8 n ptr u8 n -- bool )
   STR=CI ;

: FC-LINE-COMMENT-TOKEN? ( ptr u8 n -- bool ) {: a:ptr u :}
   u 1 <> if FC-FALSE exit then
   a c@ FC-BACKSLASH = ;

: FC-PAREN-COMMENT-TOKEN? ( ptr u8 n -- bool ) {: a:ptr u :}
   u 1 <> if FC-FALSE exit then
   a c@ FC-LPAREN = ;

: FC-STRING-TOKEN? ( ptr u8 n -- bool ) {: a:ptr u :}
   u 2 <> if FC-FALSE exit then
   a 1+ c@ FC-DQ <> if FC-FALSE exit then
   a c@ ASCII-LOWER
   dup FC-S = over FC-C = or swap FC-DOT = or ;

: FC-FORBIDDEN-TOKEN? ( ptr u8 n -- bool )
   2dup s" trust" FC-TOKEN-CI= if 2drop FC-TRUE exit then
   2dup s" trusted:" FC-TOKEN-CI= if 2drop FC-TRUE exit then
   s" set-check" FC-TOKEN-CI= ;

: FC-SKIP-UNTIL ( ptr u8 n n n -- n ) {: a:ptr u idx c :}
   idx begin dup u < while
      dup a + c@ c = if 1+ exit then
      1+
   repeat ;

: FC-SKIP-LINE ( ptr u8 n n -- n )
   FC-LF FC-SKIP-UNTIL ;

: FC-SKIP-STRING ( ptr u8 n n -- n )
   FC-DQ FC-SKIP-UNTIL ;

: FC-SKIP-PAREN-COMMENT ( ptr u8 n n -- n )
   FC-RPAREN FC-SKIP-UNTIL ;

: FC-FORBIDDEN? ( ptr u8 n -- bool ) {: a:ptr u :}
   0 FC-SCAN-IDX !
   begin
      a u FC-SCAN-IDX @ FC-SKIP-SPACE FC-SCAN-IDX !
      FC-SCAN-IDX @ u <
   while
      a u FC-SCAN-IDX @ FC-SCAN-WORD-END FC-SCAN-END !
      a FC-SCAN-IDX @ + FC-SCAN-END @ FC-SCAN-IDX @ -
      2dup FC-LINE-COMMENT-TOKEN? if
         2drop a u FC-SCAN-END @ FC-SKIP-LINE FC-SCAN-IDX !
      else
         2dup FC-PAREN-COMMENT-TOKEN? if
            2drop a u FC-SCAN-END @ FC-SKIP-PAREN-COMMENT FC-SCAN-IDX !
         else
            2dup FC-STRING-TOKEN? if
               2drop a u FC-SCAN-END @ FC-SKIP-STRING FC-SCAN-IDX !
            else
               2dup FC-FORBIDDEN-TOKEN? if 2drop FC-TRUE exit then
               2drop FC-SCAN-END @ FC-SCAN-IDX !
            then
         then
      then
   repeat
   FC-FALSE ;
