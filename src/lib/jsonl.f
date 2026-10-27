\ jsonl.f - JSONL framing: a caller-owned file cursor and a memory splitter.
\
\ Between OPEN and the end of input or CLOSE, a cursor owns its descriptor, a
\ growable BUF holding the current line, an 8192-byte input block mapping, and
\ the JR:cursor record its readers parse in, with any nesting stack a deep line
\ grew there. NEXT frames each nonempty physical line into the BUF; READER
\ lends it as a fresh linear JR reader until the next NEXT or CLOSE, and the
\ caller closes that reader first. At the end of input and after any throw the
\ cursor releases everything and is closed. SPLIT-LINE frames memory input
\ statelessly and owns nothing.
\
\ The line limit is inclusive and counts CR but not LF. NEXT skips empty lines,
\ keeps CR, returns a final unterminated line and invents none after a final
\ LF; at the end of input it releases everything and answers false once, after
\ which NEXT is E-STATE. A line over the limit is E-LIMIT and a failed read
\ E-FS-IO; open, path and allocation failures keep their owners' codes. A failed
\ descriptor close kills the process ("jsonl: close failed", exit 71): macOS and
\ Linux release the descriptor before close reports, so a retry could close a
\ reused one. CLOSE is idempotent. SPLIT-LINE answers the line at pos (CR kept),
\ the position after its LF and true, or an empty span, pos and false at the
\ end; a bad span or a negative pos is E-SOURCE. Neither form trims or parses;
\ each caller decides whether blank, malformed or non-object rows are errors.
\
\ STORAGE CLASS. CALLER-OWNED: the cursor, and what it owns between OPEN and the
\ end of input or CLOSE; SPLIT-LINE owns nothing.

require src/lib/byte-buffer.f
require src/lib/json-read.f
require src/lib/fs.f
require src/lib/memory.f
require src/lib/string.f
require src/lib/span.f

package JSONL
public

-9470 constant E-FIRST
-9479 constant E-LAST
-9470 constant E-STATE       \ OPEN of an open cursor; NEXT or READER closed or without a line
-9471 constant E-SOURCE      \ negative length, null nonempty span or negative position
-9472 constant E-LIMIT       \ a physical line exceeds its inclusive byte limit

STRUCTURE cursor OPAQUE DERIVE addr
   FIELD buf-base ptr u8
   FIELD buf-len n
   FIELD buf-cap n
   FIELD storage ptr u8
   FIELD pos n
   FIELD end n
   FIELD fd fd
   FIELD max NUM:byte-len
   FIELD live bool
   FIELD reader JR:cursor
;STRUCTURE

private

8192 constant BLOCK-BYTES
256 constant FIRST-CAP
10 constant LF
$47 constant CLOSE-EXIT      \ close releases the descriptor even when it fails

\ BUF's public header is the first three cells of cursor. Its API accepts a
\ pointer to that header, so project the record base at this one boundary.
CAST: BUF-HDR ( ptr cursor -- ptr n )

: LIVE ( ptr cursor -- )
   CURSOR-LIVE @ 0= if E-STATE throw then ;

: LINE-LEN ( ptr cursor -- n )
   BUF-HDR BUF:LEN@ BUF:BLEN>N ;

public

: CLOSE ( ptr cursor -- )
   {: cur:ptr :}
   cur CURSOR-LIVE @ 0= if exit then
   false cur CURSOR-LIVE !
   cur CURSOR-FD @ FD>N close-rc 0<> if s" jsonl: close failed" CLOSE-EXIT die then
   cur BUF-HDR BUF:DISPOSE
   cur CURSOR-READER JR:DISPOSE
   cur CURSOR-STORAGE @ 0= if exit then       \ OPEN's allocation threw first
   cur CURSOR-STORAGE @ BLOCK-BYTES SPAN:MAKE MEM:FREE-SPAN
   NULL-PTR cur CURSOR-STORAGE ! ;

private

: PREPARE ( ptr cursor -- ptr cursor )
   {: cur:ptr :}
   BLOCK-BYTES MEM:BYTES-ALLOC-LEN MEM:ALLOC-SPAN SPAN:$ drop cur CURSOR-STORAGE !
   cur BUF-HDR FIRST-CAP BUF:N>BLEN BUF:INIT
   cur ;

public

: OPEN ( ptr cursor ptr u8 n NUM:byte-len -- )
   {: cur:ptr path:ptr u:n max:NUM:byte-len :}
   cur CURSOR-LIVE @ if E-STATE throw then
   path u FS-PATHZ open-rd {: fd:n :}
   fd 0 < if E-FS-OPEN throw then
   fd >FD cur CURSOR-FD !
   max cur CURSOR-MAX !
   0 cur CURSOR-POS !
   0 cur CURSOR-END !
   true cur CURSOR-LIVE !                     \ from here CLOSE releases the descriptor
   cur ['] PREPARE catch {: code:n :} drop
   code 0<> if cur CLOSE code throw then ;

private

: APPEND ( ptr cursor ptr u8 n -- )
   {: cur:ptr seg:ptr u:n :}
   cur LINE-LEN u + cur CURSOR-MAX @ BUF:BLEN>N > if E-LIMIT throw then
   seg u BUF:N>BLEN cur BUF-HDR BUF:APPEND-SPAN ;

\ Read the next block; false at the end of input. Only a zero read is the end.
: FILL ( ptr cursor -- bool )
   {: cur:ptr :}
   cur CURSOR-FD @ FD>N cur CURSOR-STORAGE @ BLOCK-BYTES read {: n:n :}
   n 0 < if E-FS-IO throw then
   0 cur CURSOR-POS !
   n cur CURSOR-END !
   n 0 > ;

\ Leave the next nonempty line in the BUF, or an empty BUF at the end of input.
\ Each LF-free segment of a block is one append; SPLIT-NEXT answers end+1 as
\ the next position of a segment the block ends before its LF.
: FRAME ( ptr cursor -- ptr cursor )
   {: cur:ptr :}
   begin
      cur CURSOR-POS @ cur CURSOR-END @ >= if
         cur FILL 0= if cur exit then
      then
      cur CURSOR-STORAGE @ cur CURSOR-END @ LF cur CURSOR-POS @ SPLIT-NEXT drop
      {: seg:ptr segu:n next:n :}
      cur seg segu APPEND
      next cur CURSOR-END @ > if
         cur CURSOR-END @ cur CURSOR-POS !
      else
         next cur CURSOR-POS !
         cur LINE-LEN 0 > if cur exit then  \ an empty line frames nothing
      then
   again ;

public

: NEXT ( ptr cursor -- bool )
   {: cur:ptr :}
   cur LIVE
   cur BUF-HDR BUF:CLEAR
   cur ['] FRAME catch {: code:n :} drop
   code 0<> if cur CLOSE code throw then
   cur LINE-LEN 0 > if true exit then
   cur CLOSE
   false ;

: READER ( ptr cursor -- JR:reader )
   {: cur:ptr :}
   cur LIVE
   cur LINE-LEN 0= if E-STATE throw then
   cur CURSOR-READER
   cur BUF-HDR BUF:SPAN$ BUF:BLEN>N
   JR:INIT ;

\ The physical line at pos, the position after its LF (u when unterminated)
\ and true; at or past the end an empty span at pos, pos and false.
: SPLIT-LINE ( ptr u8 n n -- ptr u8 n n bool )
   {: a:ptr u:n pos:n :}
   u 0 < if E-SOURCE throw then
   u 0 > a 0= and if E-SOURCE throw then
   pos 0 < if E-SOURCE throw then
   pos u >= if a pos + 0 pos false exit then
   a u LF pos SPLIT-NEXT {: la:ptr lu:n next:n found:bool :}
   la lu next u min found ;

;package
