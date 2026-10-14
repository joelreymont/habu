\ jsonl.f - caller-owned file and memory JSONL framing.
\
\ A cursor owns its growable BUF mapping and one fixed mapping containing an
\ 8192-byte file input block followed by JR backing cells. Returned lines are
\ borrowed until the next advance or CLOSE. Memory input must remain live.
\ READER mints a fresh linear JR reader over the current line; callers close
\ that token before advancing, closing, or reusing its cursor.

require src/lib/byte-buffer.f
require src/lib/json-read.f
require src/lib/fs.f
require src/lib/memory.f
require src/lib/adt/option.f

package JSONL
public

-9470 constant E-FIRST
-9479 constant E-LAST
-9470 constant E-STATE       \ closed cursor, second start, or reader without a line
-9471 constant E-SOURCE      \ negative memory source or null nonempty source
-9472 constant E-LIMIT       \ a physical line exceeds its inclusive byte limit
-9473 constant E-CLOSE       \ closing the owned file descriptor failed

STRUCTURE cursor OPAQUE DERIVE addr
   FIELD buf-base ptr u8
   FIELD buf-len n
   FIELD buf-cap n
   FIELD storage ptr u8
   FIELD source ptr u8
   FIELD pos n
   FIELD end n
   FIELD fd fd
   FIELD mode n
   FIELD number n
   FIELD max NUM:byte-len
   FIELD line-base ptr u8
   FIELD line-len NUM:byte-len
   FIELD has-line n
;STRUCTURE

STRUCTURE line
   FIELD ptr ptr u8
   FIELD len NUM:byte-len
   FIELD number n
;STRUCTURE

private

8192 constant BLOCK-BYTES
BLOCK-BYTES JR:STORAGE-BYTES + constant STORAGE-BYTES
256 constant FIRST-CAP
10 constant LF
0 constant CLOSED
1 constant MEMORY
2 constant FILE
3 constant FINISHED

\ BUF's public header is the first three cells of cursor. Its API accepts a
\ pointer to that header, so project the record base at this one boundary.
CAST: BUF-HDR ( ptr cursor -- ptr n )

: MODE@ ( ptr cursor -- n ) CURSOR-MODE @ ;
: MODE! ( n ptr cursor -- ) CURSOR-MODE ! ;
: POS@ ( ptr cursor -- n ) CURSOR-POS @ ;
: POS! ( n ptr cursor -- ) CURSOR-POS ! ;
: END@ ( ptr cursor -- n ) CURSOR-END @ ;
: END! ( n ptr cursor -- ) CURSOR-END ! ;
: MAX@ ( ptr cursor -- n ) CURSOR-MAX @ BUF:BLEN>N ;
: HAS? ( ptr cursor -- bool ) CURSOR-HAS-LINE @ 0<> ;
: LIVE ( ptr cursor -- ) MODE@ CLOSED = if E-STATE throw then ;

: CHECK-SOURCE ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0 < if E-SOURCE throw then
   u 0 > a 0= and if E-SOURCE throw then ;

: STORAGE-SPAN ( ptr cursor -- SPAN:span<u8> )
   CURSOR-STORAGE @ STORAGE-BYTES SPAN:MAKE ;

: INPUT ( ptr cursor -- ptr u8 ) CURSOR-STORAGE @ ;
: JR-STORAGE ( ptr cursor -- ptr n ) INPUT BLOCK-BYTES + CELL-VIEW ;

: RESET-LINE ( ptr cursor -- ) {: cur:ptr :}
   0 cur CURSOR-HAS-LINE !
   NULL-PTR cur CURSOR-LINE-BASE !
   0 BUF:N>BLEN cur CURSOR-LINE-LEN ! ;

: DISPOSE ( ptr cursor -- n ) {: cur:ptr :}
   cur MODE@ CLOSED = if 0 exit then
   cur MODE@ FILE = cur MODE@ FINISHED = or
   if cur CURSOR-FD @ FD>N close-rc else 0 then
   {: rc:n :}
   CLOSED cur MODE!
   cur CURSOR-BUF-CAP @ 0<> if cur BUF-HDR BUF:DISPOSE then
   cur STORAGE-SPAN {: owned :}
   NULL-PTR cur CURSOR-STORAGE !
   NULL-PTR cur CURSOR-SOURCE !
   cur RESET-LINE
   owned MEM:FREE-SPAN
   rc ;

public

: CLOSE ( ptr cursor -- )
   DISPOSE 0<> if E-CLOSE throw then ;

private

: PREPARE ( ptr cursor -- ptr cursor ) {: cur:ptr :}
   STORAGE-BYTES MEM:BYTES-ALLOC-LEN MEM:ALLOC-SPAN SPAN:$ drop
   cur CURSOR-STORAGE !
   MEMORY cur MODE!
   cur BUF-HDR FIRST-CAP BUF:N>BLEN BUF:INIT
   cur ;

: ALLOCATE ( ptr cursor -- ) {: cur:ptr :}
   cur ['] PREPARE catch {: code:n :} drop
   code 0<> if cur DISPOSE drop code throw then ;

public

: START ( ptr cursor ptr u8 n NUM:byte-len -- )
   {: cur:ptr a:ptr u:n max:NUM:byte-len :}
   cur MODE@ CLOSED <> if E-STATE throw then
   a u CHECK-SOURCE
   cur ALLOCATE
   a cur CURSOR-SOURCE !
   0 cur POS!
   u cur END!
   0 cur CURSOR-NUMBER !
   max cur CURSOR-MAX !
   cur RESET-LINE ;

private

: OPEN-RUN ( ptr cursor ptr u8 n -- ptr cursor ptr u8 n )
   {: cur:ptr path:ptr u:n :}
   path u FS-PATHZ open-rd {: fd:n :}
   fd 0 < if E-FS-OPEN throw then
   fd >FD cur CURSOR-FD !
   FILE cur MODE!
   cur path u ;

public

: OPEN ( ptr cursor ptr u8 n NUM:byte-len -- )
   {: cur:ptr path:ptr u:n max:NUM:byte-len :}
   cur NULL-PTR 0 max START
   cur path u ['] OPEN-RUN catch {: code:n :} 2drop drop
   code 0<> if cur DISPOSE drop code throw then ;

private

: LINE! ( ptr cursor ptr u8 n -- )
   {: cur:ptr a:ptr u:n :}
   cur CURSOR-NUMBER @ 1+ cur CURSOR-NUMBER !
   a cur CURSOR-LINE-BASE !
   u BUF:N>BLEN cur CURSOR-LINE-LEN !
   1 cur CURSOR-HAS-LINE ! ;

: CHECK-ADD ( ptr cursor n -- ) {: cur:ptr u:n :}
   u cur MAX@ > if E-LIMIT throw then ;

: MEMORY-STEP ( ptr cursor -- ) {: cur:ptr :}
   cur POS@ {: start:n :}
   begin cur POS@ cur END@ < while
      cur CURSOR-SOURCE @ cur POS@ + c@ {: byte:n :}
      cur POS@ 1+ cur POS!
      byte LF = if
         cur cur CURSOR-SOURCE @ start + cur POS@ start - 1- LINE!
         exit
      then
      cur cur POS@ start - CHECK-ADD
   repeat
   cur POS@ start > if
      cur cur CURSOR-SOURCE @ start + cur POS@ start - LINE!
   then ;

: READ-BLOCK ( ptr cursor -- bool ) {: cur:ptr :}
   cur CURSOR-FD @ FD>N cur INPUT BLOCK-BYTES read {: n:n :}
   n 0 < if E-FS-IO throw then
   n 0= if
      FINISHED cur MODE!
      false exit
   then
   0 cur POS!
   n cur END!
   true ;

: FILE-BYTE ( ptr cursor -- option<u8> ) {: cur:ptr :}
   cur POS@ cur END@ >= if
      cur READ-BLOCK 0= if OPTION:NONE exit then
   then
   cur INPUT cur POS@ + c@ {: byte:n :}
   cur POS@ 1+ cur POS!
   byte OPTION:SOME ;

: FILE-LINE ( ptr cursor -- ) {: cur:ptr :}
   cur cur BUF-HDR BUF:SPAN$ BUF:BLEN>N LINE! ;

: FILE-STEP ( ptr cursor -- ) {: cur:ptr :}
   cur BUF-HDR BUF:CLEAR
   begin
      cur FILE-BYTE MATCH option
         none OF
            cur BUF-HDR BUF:LEN@ BUF:BLEN>N 0 > if cur FILE-LINE then
            exit
         ENDOF
         some OF {: byte:n :}
            byte LF = if cur FILE-LINE exit then
            cur cur BUF-HDR BUF:LEN@ BUF:BLEN>N 1+ CHECK-ADD
            byte cur BUF-HDR BUF:APPEND-BYTE
         ENDOF
      ;MATCH
   again ;

: STEP ( ptr cursor -- ptr cursor ) {: cur:ptr :}
   cur RESET-LINE
   cur MODE@ MEMORY = if cur MEMORY-STEP cur exit then
   cur MODE@ FILE = if cur FILE-STEP cur exit then
   cur MODE@ FINISHED = if cur exit then
   E-STATE throw ;

public

: NEXT-LINE ( ptr cursor -- option<line> ) {: cur:ptr :}
   cur LIVE
   cur ['] STEP catch {: code:n :} drop
   code 0<> if cur DISPOSE drop code throw then
   cur HAS? if
      cur CURSOR-LINE-BASE @
      cur CURSOR-LINE-LEN @
      cur CURSOR-NUMBER @
      JSONL-LINE:MAKE OPTION:SOME
   else
      OPTION:NONE
   then ;

: NEXT ( ptr cursor -- option<line> ) {: cur:ptr :}
   begin
      cur NEXT-LINE MATCH option
         none OF OPTION:NONE exit ENDOF
         some OF
            JSONL-LINE:UNMAKE {: a:ptr u:NUM:byte-len number:n :}
            u BUF:BLEN>N 0 > if a u number JSONL-LINE:MAKE OPTION:SOME exit then
         ENDOF
      ;MATCH
   again ;

: READER ( ptr cursor -- JR:reader ) {: cur:ptr :}
   cur LIVE
   cur HAS? 0= if E-STATE throw then
   cur JR-STORAGE JR:STORAGE-BYTES
   cur CURSOR-LINE-BASE @ cur CURSOR-LINE-LEN @ BUF:BLEN>N
   JR:INIT ;

private

: CALL ( R ptr cursor [ R ptr cursor -- R ptr cursor ] -- R ptr cursor [ R ptr cursor -- R ptr cursor ] )
   {: cb :}
   cb execute cb ;

public

: WITH-FILE ( R ptr cursor ptr u8 n NUM:byte-len [ R ptr cursor -- R ptr cursor ] -- R )
   {: cur:ptr path:ptr u:n max:NUM:byte-len cb :}
   cur path u max OPEN
   cur cb ['] CALL catch {: code:n :}
   drop drop
   cur DISPOSE {: close-code:n :}
   code 0<> if code throw then
   close-code 0<> if E-CLOSE throw then ;

;package
