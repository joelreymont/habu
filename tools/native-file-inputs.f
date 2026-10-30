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
\ A file's own source: the stat at capture (change time sec, ns and size at
\ row+32) and, as the answer, the SHA-256 of the bytes that ran. Change time,
\ not modification time: every write moves it and no user tool sets it back.
5 constant SOURCE-ROW
64 constant ROW-BYTES
create CONTENT-SHA SHA256-FILE-CTX-BYTES allot
create CONTENT-DIGEST 32 allot
DYNAMIC-BUFFER INPUT-BUF u8
variable INPUT-U
variable INPUT-FIRST
variable CHECK-OFF
variable QUERY-OFF
\ Rows rewritten against a tree (TREE-OUT, TREE-IN): the result of the last
\ call, valid until the next one or CLOSE.
DYNAMIC-BUFFER TREE-BUF u8
variable TREE-U
variable TREE-OFF
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

: STAT= ( ptr u8 ptr u8 n -- bool )
   {: row:ptr path:ptr pathu:n :}
   path pathu FS:FILE-META {: csec:n cns:n :} 2drop {: size:n :}
   csec row 32 + LE:U64@ =
   cns row 40 + LE:U64@ = and
   size row 48 + LE:U64@ = and ;

: CHECK-FILE ( ptr u8 ptr u8 n -- )
   {: row:ptr path:ptr pathu:n :}
   row 24 + LE:U64@ 1 <> if MALFORMED then
   path pathu FS:FILE? 0= if BAD then
   row path pathu STAT= 0= if BAD then ;

: STAMP-FILE ( ptr u8 ptr u8 n -- )
   {: row:ptr path:ptr pathu:n :}
   path pathu FS:FILE? 0= if 0 row 24 + LE:U64! exit then
   path pathu FS:FILE-META {: csec:n cns:n :} 2drop {: size:n :}
   csec row 32 + LE:U64!
   cns row 40 + LE:U64!
   size row 48 + LE:U64! ;

: CONTENT-FILE ( ptr u8 ptr u8 n -- )
   {: row:ptr path:ptr pathu:n :}
   CONTENT-SHA path pathu row 32 + SHA256-FILE-IN
      dup 0<> if throw then drop ;

: CHECK-CONTENT ( ptr u8 ptr u8 n -- )
   {: row:ptr path:ptr pathu:n :}
   row 24 + LE:U64@ 1 <> if MALFORMED then
   path pathu FS:FILE? 0= if BAD then
   CONTENT-SHA path pathu CONTENT-DIGEST SHA256-FILE-IN
      dup 0<> if BAD then drop
   row 32 + 32 CONTENT-DIGEST 32 STR= 0= if BAD then ;

\ An unchanged stat stands for unchanged bytes without reading them; a changed
\ one is fresh only when the file still hashes to the bytes that ran.
: CHECK-SOURCE ( ptr u8 ptr u8 n -- )
   {: row:ptr path:ptr pathu:n :}
   path pathu FS:FILE? 0= if BAD then
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

: STORE-CTIME ( ptr u8 n ptr u8 n -- )
   {: path:ptr pathu:n root:ptr rootu:n :}
   FILE-ROW path pathu root rootu true ROW {: row:ptr :}
   row path pathu STAMP-FILE ;

: STORE-CONTENT ( ptr u8 n ptr u8 n -- )
   {: path:ptr pathu:n root:ptr rootu:n :}
   CONTENT-ROW path pathu root rootu true ROW {: row:ptr :}
   row path pathu CONTENT-FILE ;

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

: ADD-SOURCE ( ptr u8 n ptr u8 n -- )
   LOCK ['] STORE-OWN ['] UNLOCK finally ;

: ADD-ROOT ( ptr u8 n ptr u8 n ptr u8 n -- )
   LOCK ['] STORE-ROOT ['] UNLOCK finally ;

: ADD-HASH ( ptr u8 n ptr u8 n ptr u8 -- )
   LOCK ['] STORE-HASH ['] UNLOCK finally ;

: ADD-CTIME ( ptr u8 n ptr u8 n -- )
   LOCK ['] STORE-CTIME ['] UNLOCK finally ;

: ADD-CONTENT ( ptr u8 n ptr u8 n -- )
   LOCK ['] STORE-CONTENT ['] UNLOCK finally ;

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

private

: TREE-PUT ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0= if exit then
   u MEM-MAX-N TREE-U @ - > if BAD then
   TREE-U @ u + TREE-BUF-RESERVE
   a TREE-U @ TREE-BUF u BYTE-COPY
   u TREE-U +! ;


\ A path under root as `./` and its path relative to root, else one under the
\ engine's source root as `@/` and its path relative to that, else as it is.
\ An empty root names no tree.
: OUT-PATH ( ptr u8 n ptr u8 n -- ) {: a:ptr u:n root:ptr rootu:n :}
   rootu 0<> if
      a u root rootu SOURCE-ROOT:BELOW? if
         s" ./" TREE-PUT a u root rootu SOURCE-ROOT:RELATIVE TREE-PUT exit
      then
   then
   a u SOURCE-ROOT:ENGINE$ SOURCE-ROOT:BELOW? if
      s" @/" TREE-PUT a u SOURCE-ROOT:ENGINE$ SOURCE-ROOT:RELATIVE TREE-PUT exit
   then
   a u TREE-PUT ;


\ Root and x joined by one separator; x `.` is root itself.
: JOINED ( ptr u8 n ptr u8 n -- ) {: x:ptr xu:n root:ptr rootu:n :}
   root rootu TREE-PUT
   x xu s" ." STR= if exit then
   rootu 0<> if root rootu + 1- c@ 47 <> if s" /" TREE-PUT then then
   x xu TREE-PUT ;


\ Whether a is the byte c, a slash and at least one more byte.
: MARKED? ( ptr u8 n n -- bool ) {: a:ptr u:n c:n :}
   u 3 < if false exit then
   a c@ c = a 1+ c@ 47 = and ;


\ The reverse of OUT-PATH.
: IN-PATH ( ptr u8 n ptr u8 n -- ) {: a:ptr u:n root:ptr rootu:n :}
   a u 46 MARKED? if a 2 + u 2 - root rootu JOINED exit then
   a u 64 MARKED? if a 2 + u 2 - SOURCE-ROOT:ENGINE$ JOINED exit then
   a u TREE-PUT ;


\ One row, the bytes left from it, root and the path map: the row rewritten
\ onto TREE-BUF, and the bytes it held. FILE, CONTENT and QUERY rows map their
\ path and a nonempty answer, a SOURCE row its path (its answer is a digest),
\ an ENV row nothing.
: TREE-ROW ( ptr u8 n ptr u8 n [ ptr u8 n ptr u8 n -- ] -- n )
   {: row:ptr left:n root:ptr rootu:n map :}
   left ROW-BYTES < if MALFORMED then
   row 8 + LE:U64@ {: pathu:n :}
   row 16 + LE:U64@ {: answeru:n :}
   pathu 0 < answeru 0 < or if MALFORMED then
   pathu left ROW-BYTES - > if MALFORMED then
   answeru left ROW-BYTES - pathu - > if MALFORMED then
   row LE:U64@ {: kind:n :}
   kind FILE-ROW < kind SOURCE-ROW > or if MALFORMED then
   row ROW-BYTES + {: path:ptr :}
   path pathu + {: answer:ptr :}
   TREE-U @ {: head:n :}
   row ROW-BYTES TREE-PUT
   kind ENV-ROW = if path pathu TREE-PUT
   else path pathu root rootu map execute then
   TREE-U @ head - ROW-BYTES - {: newpathu:n :}
   kind ENV-ROW = kind SOURCE-ROW = or answeru 0= or
      if answer answeru TREE-PUT
      else answer answeru root rootu map execute then
   TREE-U @ head - ROW-BYTES - newpathu - {: newansweru:n :}
   head TREE-BUF {: dst:ptr :}
   newpathu dst 8 + LE:U64!
   newansweru dst 16 + LE:U64!
   ROW-BYTES pathu + answeru + ;


: TREE-MAP ( ptr u8 n ptr u8 n [ ptr u8 n ptr u8 n -- ] -- ptr u8 n )
   {: bytes:ptr size:n root:ptr rootu:n map :}
   0 TREE-U ! 0 TREE-OFF !
   size 0= if NULL-PTR 0 exit then
   begin TREE-OFF @ size < while
      bytes TREE-OFF @ + size TREE-OFF @ - root rootu map TREE-ROW
         TREE-OFF +!
   repeat
   0 TREE-BUF TREE-U @ ;

public

\ An object stores its rows relative to its file's root and the engine's
\ source root, so an identical tree at another path checks its own files.
\ Rows and root: the rows as stored, valid until the next TREE-OUT, TREE-IN
\ or CLOSE.
: TREE-OUT ( ptr u8 n ptr u8 n -- ptr u8 n ) ['] OUT-PATH TREE-MAP ;

\ Stored rows and root: the rows against this tree, as TREE-OUT's are valid.
: TREE-IN ( ptr u8 n ptr u8 n -- ptr u8 n ) ['] IN-PATH TREE-MAP ;

private
: ZERO ( ptr u8 n -- ) {: bytes:ptr size:n :}
   size 0 ?do 0 bytes i + c! loop ;
public

\ A file digest leaves the path and the file's bytes in CONTENT-SHA, a
\ freshness check leaves the object's address in FRESH-BYTES and a tree
\ rewrite its rows in TREE-BUF: close clears them, so an image saved after a
\ warm build holds what a cold one does.
: CLOSE ( -- )
   INPUT-BUF-RELEASE TREE-BUF-RELEASE
   0 INPUT-U ! 0 INPUT-FIRST ! 0 CHECK-OFF ! 0 QUERY-OFF !
   0 TREE-U ! 0 TREE-OFF !
   CONTENT-SHA SHA256-FILE-CTX-BYTES ZERO
   CONTENT-DIGEST 32 ZERO
   NULL-PTR FRESH-BYTES ! 0 FRESH-U ! ;

;package
