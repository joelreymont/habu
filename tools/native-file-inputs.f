\ Files read while a selected source runs, plus the ordinary loader's path
\ queries. The query rows preserve search order: a newly present candidate can
\ change which child a require selects even when the old child is unchanged.

require src/lib/errors.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/le.f
require src/lib/memory.f

package NFILE-INPUTS
private

1 constant FILE-ROW
2 constant QUERY-ROW
64 constant ROW-BYTES
DYNAMIC-BUFFER INPUT-BUF u8
variable INPUT-U
variable INPUT-FIRST
variable CHECK-OFF

: BAD ( -- ) E-NUNIT-PROFILE throw ;
: MALFORMED ( -- ) E-NUNIT-FILE throw ;

: ROOM ( n -- n ) {: need:n :}
   need 0 < need MEM-MAX-N INPUT-U @ - > or if BAD then
   INPUT-U @ need + INPUT-BUF-RESERVE
   INPUT-U @ dup need + INPUT-U ! ;

: ROW ( n ptr u8 n ptr u8 n bool -- ptr u8 )
   {: kind:n path:ptr pathu:n answer:ptr answeru:n exists:bool :}
   pathu 0 <= answeru 0 < or if BAD then
   pathu MEM-MAX-N ROW-BYTES - > if BAD then
   answeru MEM-MAX-N ROW-BYTES - pathu - > if BAD then
   ROW-BYTES pathu + answeru + ROOM {: off:n :}
   off INPUT-BUF {: dst:ptr :}
   kind dst LE:U64!
   pathu dst 8 + LE:U64!
   answeru dst 16 + LE:U64!
   exists if 1 else 0 then dst 24 + LE:U64!
   32 0 ?do 0 dst 32 + i + c! loop
   path dst ROW-BYTES + pathu BYTE-COPY
   answer dst ROW-BYTES pathu + + answeru BYTE-COPY
   dst ;

: CHECK-FILE ( ptr u8 ptr u8 n -- )
   {: row:ptr path:ptr pathu:n :}
   row 24 + LE:U64@ 1 <> if MALFORMED then
   path pathu FILE? 0= if BAD then
   path pathu FILE-META {: size:n sec:n ns:n csec:n cns:n :}
   csec drop cns drop
   sec row 32 + LE:U64@ <>
   ns row 40 + LE:U64@ <> or
   size row 48 + LE:U64@ <> or if BAD then ;

: STAMP-FILE ( ptr u8 ptr u8 n -- )
   {: row:ptr path:ptr pathu:n :}
   path pathu FILE? 0= if 0 row 24 + LE:U64! exit then
   path pathu FILE-META {: size:n sec:n ns:n csec:n cns:n :}
   csec drop cns drop
   sec row 32 + LE:U64!
   ns row 40 + LE:U64!
   size row 48 + LE:U64! ;

: CHECK-QUERY ( ptr u8 ptr u8 n -- )
   {: row:ptr path:ptr pathu:n :}
   path pathu SOURCE-ROOT:CANON-OS {: answer:ptr answeru:n exists:bool :}
   exists if 1 else 0 then row 24 + LE:U64@ <> if BAD then
   answeru row 16 + LE:U64@ <> if BAD then
   answer answeru row ROW-BYTES pathu + + answeru STR= 0= if BAD then ;

public

: RESET ( -- )
   0 INPUT-U ! 0 INPUT-FIRST ! 0 CHECK-OFF ! ;

: ADD ( ptr u8 n ptr u8 n -- )
   {: path:ptr pathu:n source:ptr sourceu:n :}
   source drop sourceu drop
   FILE-ROW path pathu s" " true ROW {: row:ptr :}
   row path pathu STAMP-FILE ;

: ADD-ROOT ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: path:ptr pathu:n root:ptr rootu:n source:ptr sourceu:n :}
   source drop sourceu drop
   FILE-ROW path pathu root rootu true ROW {: row:ptr :}
   row path pathu STAMP-FILE ;

: ADD-MTIME ( ptr u8 n ptr u8 n -- )
   {: path:ptr pathu:n root:ptr rootu:n :}
   FILE-ROW path pathu root rootu true ROW {: row:ptr :}
   row path pathu STAMP-FILE ;

: ADD-STAMP ( ptr u8 n ptr u8 n n n n -- )
   {: path:ptr pathu:n root:ptr rootu:n size:n sec:n ns:n :}
   FILE-ROW path pathu root rootu true ROW {: row:ptr :}
   sec row 32 + LE:U64!
   ns row 40 + LE:U64!
   size row 48 + LE:U64! ;

: ADD-CANON ( ptr u8 n ptr u8 n bool -- )
   {: path:ptr pathu:n answer:ptr answeru:n exists:bool :}
   QUERY-ROW path pathu answer answeru exists ROW drop ;

: BYTES$ ( -- ptr u8 n )
   INPUT-U @ INPUT-FIRST @ = if NULL-PTR 0 exit then
   INPUT-FIRST @ INPUT-BUF INPUT-U @ INPUT-FIRST @ - ;

: MARK ( -- n ) INPUT-U @ ;

: FROM ( n -- ) {: off:n :}
   off 0 < off INPUT-U @ > or if BAD then
   off INPUT-FIRST ! ;

: BYTES-FROM ( n -- ptr u8 n ) {: off:n :}
   INPUT-U @ {: end:n :}
   off 0 < off end > or if BAD then
   off end = if NULL-PTR 0 exit then
   off INPUT-BUF end off - ;

: SPAN ( n n -- ptr u8 n ) {: first:n last:n :}
   first 0 < last first < or last INPUT-U @ > or if BAD then
   first last = if NULL-PTR 0 exit then
   first INPUT-BUF last first - ;

: APPEND ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0= if exit then
   u ROOM {: off:n :}
   a off INPUT-BUF u BYTE-COPY ;

: TRUNCATE ( n -- ) {: off:n :}
   off 0 < off INPUT-U @ > or if BAD then
   INPUT-FIRST @ off > if off INPUT-FIRST ! then
   off INPUT-U ! ;

: VERIFY ( ptr u8 n bool -- ) {: bytes:ptr size:n fresh:bool :}
   0 CHECK-OFF !
   begin CHECK-OFF @ size < while
      size CHECK-OFF @ - ROW-BYTES < if MALFORMED then
      bytes CHECK-OFF @ + {: row:ptr :}
      row 8 + LE:U64@ {: pathu:n :}
      row 16 + LE:U64@ {: answeru:n :}
      pathu 0 <= answeru 0 < or if MALFORMED then
      pathu size CHECK-OFF @ - ROW-BYTES - > if MALFORMED then
      answeru size CHECK-OFF @ - ROW-BYTES - pathu - > if MALFORMED then
      row ROW-BYTES + {: path:ptr :}
      row LE:U64@ {: kind:n :}
      kind FILE-ROW = if
         row 24 + LE:U64@ 1 <> if MALFORMED then
         fresh if row path pathu CHECK-FILE then
      else kind QUERY-ROW = if
         row 24 + LE:U64@ 1 > if MALFORMED then
         fresh if row path pathu CHECK-QUERY then
      else MALFORMED then then
      ROW-BYTES pathu + answeru + CHECK-OFF +!
   repeat
   CHECK-OFF @ size <> if MALFORMED then ;

: CHECK ( ptr u8 n -- ) true VERIFY ;
: FORMAT ( ptr u8 n -- ) false VERIFY ;

: CLOSE ( -- )
   INPUT-BUF-RELEASE
   0 INPUT-U ! 0 INPUT-FIRST ! 0 CHECK-OFF ! ;

;package
