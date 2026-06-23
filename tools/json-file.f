\ json-file.f - dynamic JSONL file cursor.
\
\ Load after lib/errors.f, lib/memory.f, lib/fs.f, and tools/json.f.

$1000 constant JSONLF-READ-CAP
$1000 constant JSONLF-LINE-BOOT-CAP
10 constant JSONLF-LF

create JSONLF-READ-BUF JSONLF-READ-CAP allot
create JSONLF-LINE-BOOT JSONLF-LINE-BOOT-CAP allot

variable JSONLF-FD
variable JSONLF-RD
variable JSONLF-BUF-U
variable JSONLF-BUF-I
variable JSONLF-LINE-U
variable JSONLF-LINE-CAP-U
variable JSONLF-LINE-P
variable JSONLF-LINE-N
variable JSONLF-DONE

: JSONLF-TRUE ( -- bool )
   0 0= ;

: JSONLF-FALSE ( -- bool )
   JSONLF-TRUE 0= ;

: JSONLF-LINE-PTR ( -- ptr u8 )
   JSONLF-LINE-P JSON-PTR-U8@ ;

: JSONLF-LINE-P! ( ptr u8 -- )
   JSONLF-LINE-P JSON-PTR-U8! ;

JSONLF-LINE-BOOT JSONLF-LINE-P!
JSONLF-LINE-BOOT-CAP JSONLF-LINE-CAP-U !
-1 JSONLF-FD !

: JSONLF-LINE-CAP ( -- n )
   JSONLF-LINE-CAP-U @ ;

: JSONLF-LINE# ( -- n )
   JSONLF-LINE-N @ ;

: JSONLF-RESET ( -- )
   0 JSONLF-RD !
   0 JSONLF-BUF-U !
   0 JSONLF-BUF-I !
   0 JSONLF-LINE-U !
   0 JSONLF-LINE-N !
   0 JSONLF-DONE ! ;

: JSONLF-CLOSE ( -- )
   JSONLF-FD @ dup 0 >= if close else drop then
   -1 JSONLF-FD ! ;

: JSONLF-THROW ( n -- )
   JSONLF-CLOSE
   throw ;

: JSONLF-OPEN ( ptr u8 n -- )
   JSONLF-CLOSE
   JSONLF-RESET
   FS-PATHZ open-rd JSONLF-FD !
   JSONLF-FD @ 0 < if E-FS-OPEN JSONLF-THROW then ;

: JSONLF-GROW-LINE ( n -- ) {: need :}
   need MEM-ALLOC-64K-SPAN {: dst:ptr cap :}
   JSONLF-LINE-PTR JSONLF-LINE-U @ dst JSON-COPY
   dst JSONLF-LINE-P!
   cap JSONLF-LINE-CAP-U ! ;

: JSONLF-ENSURE-LINE ( n -- ) {: need :}
   need JSONLF-LINE-CAP <= if exit then
   need JSONLF-GROW-LINE ;

: JSONLF-APPEND-C ( n -- ) {: c :}
   JSONLF-LINE-U @ 1+ JSONLF-ENSURE-LINE
   c JSONLF-LINE-PTR JSONLF-LINE-U @ + c!
   JSONLF-LINE-U @ 1+ JSONLF-LINE-U ! ;

: JSONLF-READ-MORE ( -- bool )
   JSONLF-FD @ JSONLF-READ-BUF JSONLF-READ-CAP read JSONLF-RD !
   JSONLF-RD @ 0 < if E-FS-IO JSONLF-THROW then
   0 JSONLF-BUF-I !
   JSONLF-RD @ JSONLF-BUF-U !
   JSONLF-RD @ 0 > ;

: JSONLF-NEXT-BYTE ( -- n bool )
   JSONLF-BUF-I @ JSONLF-BUF-U @ >= if
      JSONLF-READ-MORE 0= if 0 JSONLF-FALSE exit then
   then
   JSONLF-READ-BUF JSONLF-BUF-I @ + c@
   JSONLF-BUF-I @ 1+ JSONLF-BUF-I !
   JSONLF-TRUE ;

: JSONLF-LINE-DONE ( -- ptr u8 n bool )
   JSONLF-LINE-N @ 1+ JSONLF-LINE-N !
   JSONLF-LINE-PTR JSONLF-LINE-U @ JSONLF-TRUE
   0 JSONLF-LINE-U ! ;

: JSONLF-EOF ( -- ptr u8 n bool )
   JSONLF-CLOSE
   -1 JSONLF-DONE !
   JSONLF-LINE-U @ 0 > if JSONLF-LINE-DONE exit then
   JSONLF-LINE-PTR 0 JSONLF-FALSE ;

: JSONLF-NEXT-LINE ( -- ptr u8 n bool )
   JSONLF-DONE @ 0 <> if JSONLF-LINE-PTR 0 JSONLF-FALSE exit then
   begin JSONLF-NEXT-BYTE while
      dup JSONLF-LF = if drop JSONLF-LINE-DONE exit then
      JSONLF-APPEND-C
   repeat drop
   JSONLF-EOF ;

: JSONLF-SET-JSONL-LINE ( ptr u8 n -- ptr u8 n ) {: a:ptr u :}
   a JSONL-LA!
   u JSONL-LU !
   a u ;

: JSONLF-PARSE-LINE ( ptr u8 n -- n n n bool )
   JSONLF-SET-JSONL-LINE
   dup 0= if
      2drop -1 JSONL-ROW-BLANK 0 JSONLF-TRUE exit
   then
   JSONL-START-STRICT
   JSONL-NEXT-ROW ;

: JSONLF-NEXT-ROW ( -- n n n bool )
   JSONLF-NEXT-LINE if
      JSONLF-PARSE-LINE
      exit
   then
   2drop -1 JSONL-ROW-EOF 0 JSONLF-FALSE ;
