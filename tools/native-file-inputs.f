\ Files read while a selected source runs, plus the ordinary loader's path
\ queries and observed environment values. The query rows preserve search order:
\ a newly present candidate can change which child a require selects even when
\ the old child is unchanged.

require src/lib/errors.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/le.f
require src/lib/memory.f
require src/os/env-base.f
require src/habu/xref.f

package NFILE-INPUTS
private

1 constant FILE-ROW
2 constant QUERY-ROW
3 constant CONTENT-ROW
4 constant ENV-ROW
\ A file's own source: the stat at capture (sec, ns, size at row+32) and, as
\ the answer, the SHA-256 of the bytes that ran.
5 constant SOURCE-ROW
64 constant ROW-BYTES
create CONTENT-SHA SHA256-FILE-CTX-BYTES allot
create CONTENT-DIGEST 32 allot
DYNAMIC-BUFFER INPUT-BUF u8
variable INPUT-U
variable INPUT-FIRST
variable CHECK-OFF
variable QUERY-OFF
here data-base - negate 7 and allot
variable MUTEX

: LOCK ( -- ) begin 0 1 MUTEX atomic-cas 0= until ;
: UNLOCK ( -- ) 0 MUTEX atomic! ;
CAST: ENV-CURRENT-XT ( n -- [ ptr u8 n -- ptr u8 n ] )

: BAD ( -- ) E-NUNIT-PROFILE throw ;
: MALFORMED ( -- ) E-NUNIT-FILE throw ;

: ROOM ( n -- n ) {: need:n :}
   need 0 < need MEM-MAX-N INPUT-U @ - > or if BAD then
   INPUT-U @ need + INPUT-BUF-RESERVE
   INPUT-U @ dup need + INPUT-U ! ;

: ROW ( n ptr u8 n ptr u8 n bool -- ptr u8 )
   {: kind:n path:ptr pathu:n answer:ptr answeru:n exists:bool :}
   pathu 0 < answeru 0 < or if BAD then
   pathu 0= kind ENV-ROW <> and if BAD then
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

\ Whether the file's stat equals the one row recorded.
: STAT= ( ptr u8 ptr u8 n -- bool )
   {: row:ptr path:ptr pathu:n :}
   path pathu FILE-META {: size:n sec:n ns:n csec:n cns:n :}
   csec drop cns drop
   sec row 32 + LE:U64@ =
   ns row 40 + LE:U64@ = and
   size row 48 + LE:U64@ = and ;

: CHECK-FILE ( ptr u8 ptr u8 n -- )
   {: row:ptr path:ptr pathu:n :}
   row 24 + LE:U64@ 1 <> if MALFORMED then
   path pathu FILE? 0= if BAD then
   row path pathu STAT= 0= if BAD then ;

: STAMP-FILE ( ptr u8 ptr u8 n -- )
   {: row:ptr path:ptr pathu:n :}
   path pathu FILE? 0= if 0 row 24 + LE:U64! exit then
   path pathu FILE-META {: size:n sec:n ns:n csec:n cns:n :}
   csec drop cns drop
   sec row 32 + LE:U64!
   ns row 40 + LE:U64!
   size row 48 + LE:U64! ;

: CONTENT-FILE ( ptr u8 ptr u8 n -- )
   {: row:ptr path:ptr pathu:n :}
   CONTENT-SHA path pathu row 32 + SHA256-FILE-IN
      dup 0<> if throw then drop ;

: CHECK-CONTENT ( ptr u8 ptr u8 n -- )
   {: row:ptr path:ptr pathu:n :}
   row 24 + LE:U64@ 1 <> if MALFORMED then
   path pathu FILE? 0= if BAD then
   CONTENT-SHA path pathu CONTENT-DIGEST SHA256-FILE-IN
      dup 0<> if BAD then drop
   row 32 + 32 CONTENT-DIGEST 32 STR= 0= if BAD then ;

\ An unchanged stat stands for unchanged bytes without reading them; a changed
\ one is fresh only when the file still hashes to the bytes that ran.
: CHECK-SOURCE ( ptr u8 ptr u8 n -- )
   {: row:ptr path:ptr pathu:n :}
   path pathu FILE? 0= if BAD then
   row 24 + LE:U64@ 1 = row path pathu STAT= and if exit then
   CONTENT-SHA path pathu CONTENT-DIGEST SHA256-FILE-IN
      dup 0<> if BAD then drop
   row ROW-BYTES + row 8 + LE:U64@ + 32 CONTENT-DIGEST 32 STR= 0= if BAD then ;

: CHECK-QUERY ( ptr u8 ptr u8 n -- )
   {: row:ptr path:ptr pathu:n :}
   path pathu SOURCE-ROOT:CANON-OS {: answer:ptr answeru:n exists:bool :}
   exists if 1 else 0 then row 24 + LE:U64@ <> if BAD then
   answeru row 16 + LE:U64@ <> if BAD then
   answer answeru row ROW-BYTES pathu + + answeru STR= 0= if BAD then ;

: CURRENT$ ( ptr u8 n -- ptr u8 n )
   s" ENV-INPUT:CURRENT$" XREF-FIND
   dup XREF-FOUND? 0= if drop BAD then
   XREF-START ENV-CURRENT-XT execute ;

: CHECK-ENV ( ptr u8 ptr u8 n [ ptr u8 n -- ptr u8 n ] -- )
   {: row:ptr query:ptr queryu:n current :}
   query queryu current execute {: answer:ptr answeru:n :}
   answer NULL-PTR <> if 1 else 0 then row 24 + LE:U64@ <> if BAD then
   answeru row 16 + LE:U64@ <> if BAD then
   answer answeru row ROW-BYTES queryu + + answeru STR= 0= if BAD then ;

public

: RESET ( -- )
   0 INPUT-U ! 0 INPUT-FIRST ! 0 CHECK-OFF ! ;

private

: STORE-SOURCE ( ptr u8 n ptr u8 n -- )
   {: path:ptr pathu:n source:ptr sourceu:n :}
   CONTENT-ROW path pathu s" " true ROW {: row:ptr :}
   CONTENT-SHA source sourceu row 32 + SHA256-IN ;

: STORE-OWN ( ptr u8 n ptr u8 n -- )
   {: path:ptr pathu:n source:ptr sourceu:n :}
   CONTENT-SHA source sourceu CONTENT-DIGEST SHA256-IN
   SOURCE-ROW path pathu CONTENT-DIGEST 32 true ROW
   path pathu STAMP-FILE ;

: STORE-ROOT ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: path:ptr pathu:n root:ptr rootu:n source:ptr sourceu:n :}
   CONTENT-ROW path pathu root rootu true ROW {: row:ptr :}
   CONTENT-SHA source sourceu row 32 + SHA256-IN ;

: STORE-HASH ( ptr u8 n ptr u8 n ptr u8 -- )
   {: path:ptr pathu:n root:ptr rootu:n digest:ptr :}
   CONTENT-ROW path pathu root rootu true ROW {: row:ptr :}
   digest row 32 + 32 BYTE-COPY ;

: STORE-MTIME ( ptr u8 n ptr u8 n -- )
   {: path:ptr pathu:n root:ptr rootu:n :}
   FILE-ROW path pathu root rootu true ROW {: row:ptr :}
   row path pathu STAMP-FILE ;

: STORE-CONTENT ( ptr u8 n ptr u8 n -- )
   {: path:ptr pathu:n root:ptr rootu:n :}
   CONTENT-ROW path pathu root rootu true ROW {: row:ptr :}
   row path pathu CONTENT-FILE ;

: STORE-STAMP ( ptr u8 n ptr u8 n n n n -- )
   {: path:ptr pathu:n root:ptr rootu:n size:n sec:n ns:n :}
   FILE-ROW path pathu root rootu true ROW {: row:ptr :}
   sec row 32 + LE:U64!
   ns row 40 + LE:U64!
   size row 48 + LE:U64! ;

: STORE-CANON ( ptr u8 n ptr u8 n bool -- )
   {: path:ptr pathu:n answer:ptr answeru:n exists:bool :}
   QUERY-ROW path pathu answer answeru exists ROW drop ;

\ One selection can revisit the same candidate during discovery's harvest
\ passes. Its answer needs one validation row, regardless of visit count.
: STORE-CANON-ONCE ( ptr u8 n ptr u8 n bool -- )
   {: path:ptr pathu:n answer:ptr answeru:n exists:bool :}
   INPUT-FIRST @ QUERY-OFF !
   begin QUERY-OFF @ INPUT-U @ < while
      QUERY-OFF @ INPUT-BUF {: row:ptr :}
      row LE:U64@ QUERY-ROW =
      row 8 + LE:U64@ pathu = and
      row 16 + LE:U64@ answeru = and
      row 24 + LE:U64@ exists if 1 else 0 then = and if
         row ROW-BYTES + pathu path pathu STR= if
            row ROW-BYTES pathu + + answeru answer answeru STR= if exit then
         then
      then
      ROW-BYTES row 8 + LE:U64@ + row 16 + LE:U64@ + QUERY-OFF +!
   repeat
   path pathu answer answeru exists STORE-CANON ;

: STORE-ENV ( ptr u8 n ptr u8 n -- )
   {: query:ptr queryu:n answer:ptr answeru:n :}
   ENV-ROW query queryu answer answeru answer NULL-PTR <> ROW drop ;

public

\ Workers may observe GETENV while the loading task records source rows.
\ Hold one lock through reservation, growth and complete row finalization.
\ Readers, reset and cleanup run after the source's workers have joined.
: ADD ( ptr u8 n ptr u8 n -- )
   LOCK ['] STORE-SOURCE ['] UNLOCK finally ;

\ A loaded file's own source row: its path stamped now, its bytes hashed.
: ADD-SOURCE ( ptr u8 n ptr u8 n -- )
   LOCK ['] STORE-OWN ['] UNLOCK finally ;

: ADD-ROOT ( ptr u8 n ptr u8 n ptr u8 n -- )
   LOCK ['] STORE-ROOT ['] UNLOCK finally ;

: ADD-HASH ( ptr u8 n ptr u8 n ptr u8 -- )
   LOCK ['] STORE-HASH ['] UNLOCK finally ;

: ADD-MTIME ( ptr u8 n ptr u8 n -- )
   LOCK ['] STORE-MTIME ['] UNLOCK finally ;

: ADD-CONTENT ( ptr u8 n ptr u8 n -- )
   LOCK ['] STORE-CONTENT ['] UNLOCK finally ;

: ADD-STAMP ( ptr u8 n ptr u8 n n n n -- )
   LOCK ['] STORE-STAMP ['] UNLOCK finally ;

: ADD-CANON ( ptr u8 n ptr u8 n bool -- )
   LOCK ['] STORE-CANON ['] UNLOCK finally ;

: ADD-CANON-ONCE ( ptr u8 n ptr u8 n bool -- )
   LOCK ['] STORE-CANON-ONCE ['] UNLOCK finally ;

: ADD-ENV ( ptr u8 n ptr u8 n -- )
   LOCK ['] STORE-ENV ['] UNLOCK finally ;

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

private

: APPEND-ROWS ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0= if exit then
   u ROOM {: off:n :}
   a off INPUT-BUF u BYTE-COPY ;

public

: APPEND ( ptr u8 n -- )
   LOCK ['] APPEND-ROWS ['] UNLOCK finally ;

: TRUNCATE ( n -- ) {: off:n :}
   off 0 < off INPUT-U @ > or if BAD then
   INPUT-FIRST @ off > if off INPUT-FIRST ! then
   off INPUT-U ! ;

: VERIFY ( ptr u8 n bool [ ptr u8 n -- ptr u8 n ] -- n )
   {: bytes:ptr size:n fresh:bool current :}
   0 CHECK-OFF !
   0 begin CHECK-OFF @ size < while
      size CHECK-OFF @ - ROW-BYTES < if MALFORMED then
      bytes CHECK-OFF @ + {: row:ptr :}
      row 8 + LE:U64@ {: pathu:n :}
      row 16 + LE:U64@ {: answeru:n :}
      pathu 0 < answeru 0 < or if MALFORMED then
      pathu size CHECK-OFF @ - ROW-BYTES - > if MALFORMED then
      answeru size CHECK-OFF @ - ROW-BYTES - pathu - > if MALFORMED then
      row ROW-BYTES + {: path:ptr :}
      row LE:U64@ {: kind:n :}
      pathu 0= kind ENV-ROW <> and if MALFORMED then
      kind FILE-ROW = if
         row 24 + LE:U64@ 1 <> if MALFORMED then
         fresh if row path pathu CHECK-FILE then
      else kind QUERY-ROW = if
         row 24 + LE:U64@ 1 > if MALFORMED then
         fresh if row path pathu CHECK-QUERY then
      else kind CONTENT-ROW = if
         row 24 + LE:U64@ 1 <> if MALFORMED then
         fresh if row path pathu CHECK-CONTENT then
      else kind ENV-ROW = if
         row 24 + LE:U64@ 1 > if MALFORMED then
         row 24 + LE:U64@ 0= answeru 0<> and if MALFORMED then
         fresh if row path pathu current CHECK-ENV then
      else kind SOURCE-ROW = if
         row 24 + LE:U64@ 1 > answeru 32 <> or if MALFORMED then
         fresh if row path pathu CHECK-SOURCE then
      else MALFORMED then then then then then
      ROW-BYTES pathu + answeru + CHECK-OFF +!
      1+
   repeat
   CHECK-OFF @ size <> if MALFORMED then ;

: CHECK-WITH ( ptr u8 n [ ptr u8 n -- ptr u8 n ] -- ) true swap VERIFY drop ;
: CHECK ( ptr u8 n -- ) ['] CURRENT$ CHECK-WITH ;

private
PTR-VARIABLE FRESH-BYTES
variable FRESH-U
: CHECK-HELD ( -- ) FRESH-BYTES @ FRESH-U @ CHECK ;
public

\ Whether every row still holds: a stale row is false, a malformed one throws.
: FRESH? ( ptr u8 n -- bool )
   FRESH-U ! FRESH-BYTES !
   ['] CHECK-HELD catch {: code:n :}
   code 0= if true exit then
   code E-NUNIT-PROFILE <> if code throw then
   false ;

\ The row count of well-formed rows, none checked against today's sources.
: ROW-COUNT ( ptr u8 n -- n ) false ['] CURRENT$ VERIFY ;
: FORMAT ( ptr u8 n -- ) ROW-COUNT drop ;

: CLOSE ( -- )
   INPUT-BUF-RELEASE
   0 INPUT-U ! 0 INPUT-FIRST ! 0 CHECK-OFF ! ;

;package
