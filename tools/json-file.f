\ json-file.f - dynamic JSONL file cursor, package JSONF.
\
\ Load after lib/errors.f, lib/memory.f, lib/fs.f, and tools/json.f.
\
\ Switchover wave B (dot habu-switchover-wave-b-08482d5b, batch 3): the line
\ cursor and row reader return option<JSONF:line> / option<JSONF:row> and
\ option<u8> instead of value+flag sentinels, so every caller MATCHes presence
\ at the checker boundary. The inner JSONL-* machinery in tools/json.f is the
\ trusted wrapped layer and is unchanged; JSONF is the checked wrapper.

require lib/adt/option.f
require tools/json.f

using JSON

package JSONF

public
$1000 constant LINE-BOOT-CAP

private
$1000 constant READ-CAP
10 constant LF

create READ-BUF READ-CAP allot
create LINE-BOOT LINE-BOOT-CAP allot

variable FD
variable RD
variable BUF-U
variable BUF-I
variable LINE-U
variable LINE-CAP-U
variable LINE-P
variable LINE-N
variable DONE

: LINE-PTR ( -- ptr u8 )
   LINE-P JSON-PTR-U8@ ;

: LINE-P! ( ptr u8 -- )
   LINE-P JSON-PTR-U8! ;

LINE-BOOT LINE-P!
LINE-BOOT-CAP LINE-CAP-U !
-1 FD !

public
: LINE-CAP ( -- n )
   LINE-CAP-U @ ;

: LINE# ( -- n )
   LINE-N @ ;

\ A read line is the next LF-delimited (or trailing-EOF) slice of the file. The
\ line cursor returns option<JSONF:line>: SOME(slice) while a line remains, NONE
\ at end of stream. A pending partial line at EOF (no trailing LF) still yields
\ SOME. Public so the unified declaration generates JSONF-LINE:MAKE/UNMAKE;
\ the cursor words stay package-private.
STRUCTURE line 0
  FIELD ptr ptr u8
  FIELD len n
;STRUCTURE

\ A parsed row mirrors JSONL-NEXT-ROW's three cells: node = the JSON parse root
\ (-1 for non-JSON rows), kind = the JSONL-ROW-* code (JSON / BLANK / ERROR),
\ code = the caught throw code for ERROR rows (0 otherwise). The row reader
\ returns option<JSONF:row>: SOME(row) for data AND blank rows (kind
\ distinguishes), NONE only at end of stream (the former JSONL-ROW-EOF sentinel).
STRUCTURE row 0
  FIELD node n
  FIELD kind n
  FIELD code n
;STRUCTURE

private
: RESET ( -- )
   0 RD !
   0 BUF-U !
   0 BUF-I !
   0 LINE-U !
   0 LINE-N !
   0 DONE ! ;

: CLOSE-FD ( -- )
   FD @ dup 0 >= if close else drop then
   -1 FD ! ;

: FAIL ( n -- )
   CLOSE-FD throw ;

public
: OPEN ( ptr u8 n -- )
   CLOSE-FD
   RESET
   FS-PATHZ open-rd FD !
   FD @ 0 < if E-FS-OPEN FAIL then ;

private
: GROW-LINE ( n -- ) {: need:n :}
   need MEM-ALLOC-64K-SPAN {: dst:ptr cap:n :}
   LINE-PTR LINE-U @ dst JSON-COPY
   dst LINE-P!
   cap LINE-CAP-U ! ;

: ENSURE-LINE ( n -- ) {: need:n :}
   need LINE-CAP <= if exit then
   need GROW-LINE ;

: APPEND-C ( n -- ) {: c:n :}
   LINE-U @ 1+ ENSURE-LINE
   c LINE-PTR LINE-U @ + c!
   LINE-U @ 1+ LINE-U ! ;

: READ-MORE ( -- bool )
   FD @ READ-BUF READ-CAP read RD !
   RD @ 0 < if E-FS-IO FAIL then
   0 BUF-I !
   RD @ BUF-U !
   RD @ 0 > ;

: NEXT-BYTE ( -- option<u8> )               \ SOME next input byte, NONE at EOF
   BUF-I @ BUF-U @ >= if
      READ-MORE 0= if OPTION:NONE exit then
   then
   READ-BUF BUF-I @ + c@
   BUF-I @ 1+ BUF-I !
   OPTION:SOME ;

: LINE-DONE ( -- option<line> )             \ finalize buffered line as SOME(slice), reset buffer
   LINE-N @ 1+ LINE-N !
   LINE-PTR LINE-U @
   0 LINE-U !
   JSONF-LINE:MAKE OPTION:SOME ;

: EOF-LINE ( -- option<line> )              \ close + mark done; SOME pending partial line, else NONE
   CLOSE-FD
   -1 DONE !
   LINE-U @ 0 > if LINE-DONE exit then
   OPTION:NONE ;

: NEXT-LINE ( -- option<line> )             \ SOME next LF/EOF line slice, NONE once the stream is done
   DONE @ 0 <> if OPTION:NONE exit then
   begin
      NEXT-BYTE MATCH option
        none OF EOF-LINE exit ENDOF
        some OF {: c:n :} c LF = if LINE-DONE exit then c APPEND-C ENDOF
      ;MATCH
   again ;

: SET-JSONL-LINE ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   a JSONL-LA!
   u JSONL-LU !
   a u ;

: ROW>OPTION ( n n n bool -- option<row> )   \ wrap JSONL-NEXT-ROW's ( node kind code present )
   if JSONF-ROW:MAKE OPTION:SOME else drop 2drop OPTION:NONE then ;

: PARSE-LINE ( ptr u8 n -- option<row> )
   SET-JSONL-LINE
   dup 0= if
      2drop -1 JSONL-ROW-BLANK 0 JSONF-ROW:MAKE OPTION:SOME exit
   then
   JSONL-START-STRICT
   JSONL-NEXT-ROW ROW>OPTION ;

public
: NEXT-ROW ( -- option<row> )               \ SOME next row (json/blank/error), NONE at end of stream
   NEXT-LINE MATCH option
     none OF OPTION:NONE ENDOF
     some OF JSONF-LINE:UNMAKE PARSE-LINE ENDOF
   ;MATCH ;

private
;package

;using
