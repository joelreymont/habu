split-str
\ jsonl.f - JSONL framing: a caller-owned file cursor and a memory splitter.
\ Between OPEN and the end of input or CLOSE a cursor owns its descriptor, line
\ vector, 8192-byte block mapping and JSON:cursor record; at the end of input and
\ after any throw it releases all of them and is closed. Neither form trims or
\ parses: each caller decides whether blank or malformed rows are errors.
\ STORAGE CLASS. CALLER-OWNED: the cursor and what it owns; SPLIT-LINE owns none.

require src/lib/json.f
require src/lib/fs.f
require src/lib/memory.f
require src/lib/allocator.f
require src/lib/heap.f
require src/lib/vector.f
require src/lib/string.f
require src/lib/span.f
require src/core/compile-reads.f
require src/core/exit-status.f

package JSONL
public

-9470 constant E-FIRST
-9479 constant E-LAST
-9470 s" OPEN of an open cursor, or NEXT or READER on a closed cursor or one without a line"
   ERROR:FAULT E-STATE
-9471 s" a negative length, a null nonempty span or a negative position" ERROR:CODE E-SOURCE
-9472 s" a physical line exceeds its inclusive byte limit" ERROR:CODE E-LIMIT

STRUCTURE cursor OPAQUE DERIVE addr
   FIELD line ^VEC:vector<u8>
   FIELD storage ^u8
   FIELD pos n
   FIELD end n
   FIELD fd fd
   FIELD max n
   FIELD live bool
   FIELD reader JSON:cursor
;STRUCTURE

private

8192 constant BLOCK-BYTES
10 constant LF

: LIVE ( ^cursor -- )
   CURSOR-LIVE @ 0= if E-STATE throw then ;

: LINE-LEN ( ^cursor -- n )
   CURSOR-LINE @ VEC:COUNT ;

public

: CLOSE ( ^cursor -- )
   {: cur:^ :}
   cur CURSOR-LIVE @ 0= if exit then
   false cur CURSOR-LIVE !
   \ A failed close ends the process: macOS and Linux release the descriptor
   \ before close reports, so a retry could close a reused one.
   cur CURSOR-FD @ FD>N close-rc 0<> if s" jsonl: close failed" EXIT-STATUS:SYSTEM die then
   cur CURSOR-LINE @ VEC:DELETE  NULL-PTR cur CURSOR-LINE !
   cur CURSOR-READER JSON:READ-DISPOSE
   cur CURSOR-STORAGE @ 0= if exit then       \ OPEN's allocation threw first
   cur CURSOR-STORAGE @ BLOCK-BYTES SPAN:MAKE MEM:FREE-SPAN
   NULL-PTR cur CURSOR-STORAGE ! ;

private

: PREPARE ( ^cursor -- ^cursor )
   {: cur:^ :}
   BLOCK-BYTES MEM:BYTES-ALLOC-LEN MEM:ALLOC-SPAN SPAN:$ drop cur CURSOR-STORAGE !
   HEAP:ALLOCATOR 0 [type] u8 VEC:NEW cur CURSOR-LINE !
   cur ;

public

: OPEN ( ^cursor str n -- )
   {: cur:^ path:^ u:n max:n :}
   cur CURSOR-LIVE @ if E-STATE throw then
   path u FS:>PATHZ {: z:^ :}
   path u z COMPILE-READS:FILE+
   z open-rd {: fd:n :}
   fd 0 < if E-FS-OPEN throw then
   fd >FD cur CURSOR-FD !
   max cur CURSOR-MAX !
   0 cur CURSOR-POS !
   0 cur CURSOR-END !
   true cur CURSOR-LIVE !                     \ from here CLOSE releases the descriptor
   cur ['] PREPARE catch {: code:n :} drop
   code 0<> if cur CLOSE code throw then ;

private

: APPEND ( ^cursor ^u8 n -- )
   {: cur:^ seg:^ u:n :}
   cur LINE-LEN u + cur CURSOR-MAX @ > if E-LIMIT throw then
   seg u cur CURSOR-LINE @ VEC:APPEND ;

\ Only a zero read is the end.
: FILL ( ^cursor -- bool )
   {: cur:^ :}
   cur CURSOR-FD @ FD>N cur CURSOR-STORAGE @ BLOCK-BYTES read {: n:n :}
   n 0 < if E-FS-IO throw then
   0 cur CURSOR-POS !
   n cur CURSOR-END !
   n 0 > ;

\ SPLIT-NEXT answers end+1 as the next position of a segment the block ends
\ before its LF.
: FRAME ( ^cursor -- ^cursor )
   {: cur:^ :}
   begin
      cur CURSOR-POS @ cur CURSOR-END @ >= if
         cur FILL 0= if cur exit then
      then
      cur CURSOR-STORAGE @ cur CURSOR-END @ LF cur CURSOR-POS @ SPLIT-NEXT drop
      {: seg:^ segu:n next:n :}
      cur seg segu APPEND
      next cur CURSOR-END @ > if
         cur CURSOR-END @ cur CURSOR-POS !
      else
         next cur CURSOR-POS !
         cur LINE-LEN 0 > if cur exit then  \ an empty line frames nothing
      then
   again ;

public

\ The line limit is inclusive and counts CR but not LF. NEXT skips empty lines,
\ keeps CR, returns a final unterminated line and invents none after a final
\ LF; at the end of input it answers false once, after which NEXT is E-STATE.
\ READER lends the line as a fresh linear JSON reader until the next NEXT or
\ CLOSE, and the caller closes that reader first.
: NEXT ( ^cursor -- bool )
   {: cur:^ :}
   cur LIVE
   cur CURSOR-LINE @ 0 VEC:RESIZE
   cur ['] FRAME catch {: code:n :} drop
   code 0<> if cur CLOSE code throw then
   cur LINE-LEN 0 > if true exit then
   cur CLOSE
   false ;

: READER ( ^cursor -- JSON:reader )
   {: cur:^ :}
   cur LIVE
   cur LINE-LEN 0= if E-STATE throw then
   cur CURSOR-READER
   cur CURSOR-LINE @ VEC:SPAN SPAN:$
   JSON:READ-INIT ;

\ The physical line at pos, the position after its LF (u when unterminated)
\ and true; at or past the end an empty span at pos, pos and false.
: SPLIT-LINE ( str n -- str n bool )
   {: a:^ u:n pos:n :}
   u 0 < if E-SOURCE throw then
   u 0 > a 0= and if E-SOURCE throw then
   pos 0 < if E-SOURCE throw then
   pos u >= if a pos + 0 pos false exit then
   a u LF pos SPLIT-NEXT {: la:^ lu:n next:n found:bool :}
   la lu next u min found ;

;package
