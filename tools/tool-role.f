\ The build tool's role: a SHA-256 over the compiler role and the files loaded
\ between a builder's ENTRY and DONE, whose rows are the support image's inputs.
require src/lib/prelude.f
require src/lib/string.f
require src/lib/image-lifecycle.f
require src/core/sha256.f
require src/core/include.f
require src/core/source-producer.f

package TOOL-ROLE
private

1 constant QUERY-ROW  \ a path, its canonical answer and whether it exists
2 constant READ-ROW   \ a path, its root and the SHA-256 of the bytes that ran
3 constant COLD-READ  \ a cold load's read, as READ-ROW
\ A row: kind, path length, answer or root length, exists, digest, path, answer.
4 cells 32 + constant HEAD

DYNAMIC-BUFFER ROWS u8
variable ROWS-U
variable COLDS
variable DEPTH
variable LIVE
variable KNOWN
32 BUFFER: ROLE
SHA256-CTX-BYTES BUFFER: SHA
1 cells BUFFER: LEN-BYTES

: KIND ( ptr u8 -- n ) CELL-VIEW @ ;
: PATH-U ( ptr u8 -- n ) 1 cells + CELL-VIEW @ ;
: ANSWER-U ( ptr u8 -- n ) 2 cells + CELL-VIEW @ ;
: EXISTS? ( ptr u8 -- bool ) 3 cells + CELL-VIEW @ 0<> ;
: DIGEST ( ptr u8 -- ptr u8 ) 4 cells + ;
: PATH$ ( ptr u8 -- ptr u8 n ) {: row:ptr :} row HEAD + row PATH-U ;
: ANSWER$ ( ptr u8 -- ptr u8 n ) {: row:ptr :} row HEAD + row PATH-U + row ANSWER-U ;
: PAD ( n -- n ) 1 cells 1- + 1 cells negate and ;
: SIZE ( ptr u8 -- n ) {: row:ptr :} HEAD row PATH-U + row ANSWER-U + PAD ;

: RELEASE ( -- ) ROWS-RELEASE 0 ROWS-U ! 0 COLDS ! ;
: CLEAR ( -- ) RELEASE 0 LIVE ! ;


\ The rows live in a mapping, released before an image save.
: REGISTER-CLEANUP ( -- )
   LIVE @ 0= if
      [: CLEAR ;] IMAGE-LIFECYCLE:REGISTER
      -1 LIVE !
   then ;


\ A new row's digest field.
: ROW ( n ptr u8 n ptr u8 n bool -- ptr u8 )
   {: kind:n path:ptr pathu:n answer:ptr answeru:n exists:bool :}
   REGISTER-CLEANUP
   HEAD pathu + answeru + PAD {: size:n :}
   ROWS-U @ size + ROWS-RESERVE
   ROWS-U @ ROWS {: row:ptr :}
   size ROWS-U +!
   kind row CELL-VIEW !
   pathu row 1 cells + CELL-VIEW !
   answeru row 2 cells + CELL-VIEW !
   exists bool>n row 3 cells + CELL-VIEW !
   path row HEAD + pathu BYTE-COPY
   answer row HEAD + pathu + answeru BYTE-COPY
   row DIGEST ;


: OWN ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: path:ptr pathu:n root:ptr rootu:n src:ptr srcu:n :}
   READ-ROW path pathu root rootu true ROW {: digest:ptr :}
   SHA src srcu digest SHA256-IN ;


: CANON ( ptr u8 n -- ptr u8 n bool )
   {: path:ptr pathu:n :}
   path pathu SOURCE-ROOT:CANON-OS {: answer:ptr answeru:n exists:bool :}
   QUERY-ROW path pathu answer answeru exists ROW drop
   answer answeru exists ;


: READ ( ptr u8 n ptr u8 n -- ptr u8 n )
   {: path:ptr pathu:n root:ptr rootu:n :}
   path pathu root rootu SOURCE-ROOT:READ-OS {: src:ptr srcu:n :}
   path pathu root rootu src srcu OWN
   src srcu ;


: FEED ( ptr u8 n -- ) {: a:ptr u:n :} SHA a u SHA256-FEED ;


\ A name as its length and bytes, so no two rows feed the same bytes.
: NAME+ ( ptr u8 n ptr u8 n -- )
   {: mark:ptr marku:n rel:ptr relu:n :}
   marku relu + LEN-BYTES CELL-VIEW !
   LEN-BYTES 1 cells FEED
   mark marku FEED
   rel relu FEED ;


\ Relative to the engine root, else the row's root, so a copy keeps its role.
: PATH+ ( ptr u8 n ptr u8 n -- )
   {: a:ptr u:n root:ptr rootu:n :}
   a u SOURCE-ROOT:ENGINE$ SOURCE-ROOT:BELOW? if
      s" @/" a u SOURCE-ROOT:ENGINE$ SOURCE-ROOT:RELATIVE NAME+ exit
   then
   rootu 0<> if
      a u root rootu SOURCE-ROOT:BELOW? if
         s" ./" a u root rootu SOURCE-ROOT:RELATIVE NAME+ exit
      then
   then
   s" " a u NAME+ ;


\ The cold row that read this path, or false.
: COLD ( ptr u8 n -- ptr u8 bool )
   {: a:ptr u:n :}
   0 begin dup ROWS-U @ < while
      {: off:n :}
      off ROWS {: row:ptr :}
      row KIND COLD-READ = if
         row PATH$ a u STR= if row true exit then
      then
      off row SIZE +
   repeat drop
   NULL-PTR BYTE-VIEW false ;


\ A found built-in or cold-read file; LOADS-COVERED? makes any other a read.
: QUERY+ ( ptr u8 n -- )
   {: a:ptr u:n :}
   a u SOURCE-ROOT:ENGINE$ SOURCE-ROOT:BELOW? if
      a u SOURCE-ROOT:ENGINE$ SOURCE-ROOT:RELATIVE {: rel:ptr relu:n :}
      rel relu SOURCE-PRODUCER:FILE-HASH {: hash:ptr hashu:n built:bool :}
      built if
         s" @/" rel relu NAME+
         hash hashu FEED exit
      then
   then
   COLDS @ 0= if exit then
   a u COLD {: row:ptr found:bool :}
   found 0= if exit then
   row PATH$ row ANSWER$ PATH+
   row DIGEST 32 FEED ;


: NAMED? ( ptr u8 n n -- bool )
   {: a:ptr u:n end:n :}
   0 begin dup end < while
      {: off:n :}
      off ROWS {: row:ptr :}
      row KIND QUERY-ROW = if
         row EXISTS? if row ANSWER$ a u STR= if true exit then then
      then
      row KIND READ-ROW = if
         row PATH$ a u STR= if true exit then
      then
      off row SIZE +
   repeat drop false ;


\ A file an earlier row named feeds once.
: ROW+ ( ptr u8 n -- )
   {: row:ptr off:n :}
   row KIND READ-ROW = if
      row PATH$ row ANSWER$ PATH+
      row DIGEST 32 FEED exit
   then
   row KIND QUERY-ROW <> if exit then
   row EXISTS? 0= if exit then
   row ANSWER$ off NAMED? if exit then
   row ANSWER$ QUERY+ ;


: COVERED? ( ptr u8 n -- bool )
   {: a:ptr u:n :}
   0 begin dup ROWS-U @ < while
      {: off:n :}
      off ROWS {: row:ptr :}
      row KIND QUERY-ROW <> if
         row PATH$ a u STR= if true exit then
      then
      off row SIZE +
   repeat drop
   a u SOURCE-ROOT:ENGINE$ SOURCE-ROOT:BELOW? 0= if false exit then
   a u SOURCE-ROOT:ENGINE$ SOURCE-ROOT:RELATIVE
   SOURCE-PRODUCER:FILE-HASH nip nip ;


\ Each require-registry row past the boot rows is a file loaded from source;
\ one loaded outside ENTRY and DONE leaves the role unknown.
: LOADS-COVERED? ( -- bool )
   REQUIRE-BOOT-LIMIT begin dup REQUIRE-REG:COUNT < while
      {: idx:n :}
      idx REQUIRE-REG:SLOT idx REQUIRE-REG:LEN COVERED? 0= if false exit then
      idx 1+
   repeat drop true ;


: FIX ( -- )
   SOURCE-PRODUCER:COMPILER SOURCE-PRODUCER:ROLE$
      {: compiler:ptr compileru:n known:bool :}
   known 0= if 0 KNOWN ! exit then
   LOADS-COVERED? 0= if 0 KNOWN ! exit then
   SHA SHA256-BEGIN
   s" habu-tool-role" FEED
   compiler compileru FEED
   0 begin dup ROWS-U @ < while
      {: off:n :}
      off ROWS {: row:ptr :}
      row off ROW+
      off row SIZE +
   repeat drop
   SHA ROLE SHA256-END
   -1 KNOWN ! ;

public

\ At an entry file's top level. Only the outermost ENTRY records.
: ENTRY ( -- )
   1 DEPTH +!
   DEPTH @ 1 > if exit then
   SOURCE-ROOT:ACTIVE-SOURCE OWN
   ['] CANON ['] READ SOURCE-INPUT:USE ;

\ The outermost DONE fixes the role and stops recording; the rows stay.
: DONE ( -- )
   -1 DEPTH +!
   DEPTH @ 0 > if exit then
   SOURCE-INPUT:RESET
   FIX ;

: ROLE$ ( -- ptr u8 n bool ) ROLE 32 KNOWN @ 0<> ;

\ A cold load's read: path, root and the SHA-256 of its bytes.
: COLD-ROW ( ptr u8 n ptr u8 n ptr u8 -- )
   {: path:ptr pathu:n root:ptr rootu:n digest:ptr :}
   COLD-READ path pathu root rootu true ROW {: dst:ptr :}
   digest dst 32 BYTE-COPY
   1 COLDS +! ;

\ Hand each row on in order, a query to CANON as path, answer and whether it
\ exists, a read to HASH as path, root and digest, then free them.
: DRAIN ( [ ptr u8 n ptr u8 n bool -- ] [ ptr u8 n ptr u8 n ptr u8 -- ] -- )
   {: canon hash :}
   0 begin dup ROWS-U @ < while
      {: off:n :}
      off ROWS {: row:ptr :}
      row KIND QUERY-ROW = if
         row PATH$ row ANSWER$ row EXISTS? canon execute
      else
         row PATH$ row ANSWER$ row DIGEST hash execute
      then
      off row SIZE +
   repeat drop
   RELEASE ;

;package

package TOOL-ROLE
private
SOURCE-ROOT:ACTIVE-SOURCE OWN
;package
