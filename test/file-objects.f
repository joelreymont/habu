\ file-objects.f - three builds of the files in test/fixtures/file-objects/,
\ each by build.f in a fresh hb with HABU_OUT its own temp directory: a fresh
\ process, since objects must be a function of the files, not of the process.
\ Builds A and B compile count.f, shape.f, main.f, reopen.f and again.f twice
\ and load host.f after they close. Build A writes six objects; build B loads
\ pad.f first, so its later files' records, emissions and data sit elsewhere,
\ and writes seven. Each of A's objects must be byte-identical to B's of the
\ same name: an address in an object shows as different bytes. Build C, in
\ build.f's `stop` mode, runs seven failed builds and writes one object,
\ count.f's, byte-identical to A's: a refused file and DP moved back write none.
\ Each of A's PIECES reads entry by entry, then its CODE emission by emission,
\ then its DATA, STRINGS and STORES, to its final REQUEST. Every CODE's sizes,
\ functions, sites, zeroed sites and chains and references are checked, every
\ DATA's size, zeroed cells, cell rows and length cells, and every STORES row's
\ cell, kind, bytes and value; reopen.f's entries field by field, count.f's
\ entry count and package entries, and the fixtures' calls, addresses, cells,
\ strings and stores by name. The first build's program prints `Hi` and `ok` and
\ exits 0, run in its own process: the boundary under test is the written file.

require src/lib/error.f
require src/lib/string.f
require src/lib/test.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/fs-list.f
require src/lib/process-command.f
require src/lib/engine-candidate.f
require src/lib/le.f
require src/core/sha256.f

package FILE-OBJECTS-TEST

6 constant FILES                         \ build A's objects
7 constant FILES-B                       \ build B's: A's and pad.f's
5 constant FIXTURES                      \ the files with objects in both builds
64 constant HEX-BYTES
32 constant KEY-BYTES
HEX-BYTES 4 + constant NAME-BYTES        \ <hex key>.hfo
\ The names, newline-separated.
: LISTING-BYTES ( n -- n ) NAME-BYTES 1 + * 1 - ;
\ The header: magic, key, then an offset and a length for each section.
40 constant INPUT-FIELD
56 constant FILES-FIELD
72 constant REQUESTS-FIELD               \ REQUESTS, then NAMES: 0 0 each
32 constant EMPTY-BYTES
104 constant PIECES-FIELD
120 constant INPUT-AT                    \ the header's end
40 constant INPUT-BYTES                  \ a size, then a SHA-256
INPUT-AT INPUT-BYTES + constant FILES-AT
KEY-BYTES 2 * constant ROW-BYTES         \ a FILES row: a key, then a SHA-256
4096 constant CAP

PATH-CAP BUFFER: OUT-A   variable OUT-A-U
PATH-CAP BUFFER: OUT-B   variable OUT-B-U
PATH-CAP BUFFER: OUT-C   variable OUT-C-U
PATH-CAP BUFFER: OBJ-DIR
PATH-CAP BUFFER: JOINED
CAP BUFFER: NAMES-A      variable NAMES-A-U
CAP BUFFER: NAMES-B      variable NAMES-B-U
CAP BUFFER: NAMES-C      variable NAMES-C-U
CAP BUFFER: BYTES-A
CAP BUFFER: BYTES-B
CAP BUFFER: SOURCE-BYTES
SHA256-CTX-BYTES BUFFER: CTX
HEX-BYTES BUFFER: HEX
INPUT-BYTES BUFFER: EXPECT
EMPTY-BYTES BUFFER: ZEROS
ROW-BYTES BUFFER: COUNT-ROW              \ count.f's object key and SHA-256
ROW-BYTES BUFFER: SHAPE-ROW              \ shape.f's
8 BUFFER: NONE-BYTES                     \ REQUEST none: -1
128 BUFFER: LABEL-BUF
\ A byte per fixture file: the objects whose INPUT matched it.
variable MATCHED
variable SHAPE-I                         \ shape.f's object's name index, or -1
variable REOPEN-I                        \ reopen.f's, or -1
variable COUNT-I                         \ count.f's, or -1
variable MAIN-I                          \ main.f's, or -1
FILES TYPED-BUFFER OBJ-FIX n             \ each object's fixture, or -1
variable FIX                             \ the checked object's
variable LATE-REC                        \ count.f's LATE record, or -1
\ Record indices in their file, or -1: count.f's BUMP and HI, shape.f's DOUBLE
\ and TWICE.
variable BUMP-REC  variable HI-REC
variable DOUBLE-REC  variable TWICE-REC
variable TALLY-OFF                       \ the DATA offset count.f's TALLY body names
variable LATER-OFF                       \ and its LATER body's
variable MARKED-OFF                      \ and its MARKED body's
\ An entry walk of BYTES-A: the next entry's offset, and the final REQUEST's,
\ which no entry reaches; an overrun leaves WALK-AT past WALK-END.
variable WALK-AT
variable WALK-END
variable WALK-RECS                       \ records read; kind 4 is none
\ The last entry read; -1 where it has no such field.
variable ENT-KIND
variable ENT-NAME-AT  variable ENT-NAME-U
variable ENT-ROLE
variable ENT-FILE  variable ENT-REC      \ its wordlist's or kind 4's reference
\ Kind 1: 1 when the package's record minted its private wordlist.
variable ENT-MINTED
variable ENT-EM                          \ kinds 0 and 3: the emission
variable ENT-ENTRY                       \ kind 3: its entry's offset in the emission
variable MAX-EM                          \ the walk's highest ENT-EM, or -1
variable ENTRY-NO                        \ reopen.f's entry checked, from 1


: A$ ( -- ptr u8 n ) OUT-A OUT-A-U @ ;
: B$ ( -- ptr u8 n ) OUT-B OUT-B-U @ ;
: C$ ( -- ptr u8 n ) OUT-C OUT-C-U @ ;

: OBJ$ ( ptr u8 n ptr u8 n -- ptr u8 n ) {: out:ptr outu:n name:ptr nameu:n :}
   out outu s" obj" OBJ-DIR FS:JOIN-PATH {: du:n :}
   OBJ-DIR du name nameu JOINED FS:JOIN-PATH {: u:n :}
   JOINED u ;


\ A failed run shows its status and what it printed on both streams.
: SHOW ( CMD:command n -- CMD:command ) {: rc:n :}
   s" file-objects: exited " type rc .
   s" stdout: " type CMD:OUT$ type cr  s" stderr: " type CMD:ERR$ type cr ;

: DONE$ ( ptr u8 n -- ptr u8 n )
   {: mode:ptr modeu:n :}
   mode modeu s" second" STR= if s\" compiled 7\n" exit then
   mode modeu s" stop" STR= if s\" stopped\n" exit then
   s\" compiled 6\n" ;

\ A build by build.f with the mode as its script argument, none when empty.
: BUILD ( CMD:command ptr u8 n ptr u8 n -- CMD:command )
   {: out:ptr outu:n mode:ptr modeu:n :}
   CMD:RESET
   s" --load" CMD:ARG+  s" test/fixtures/file-objects/build.f" CMD:ARG+
   modeu 0<> if s" --" CMD:ARG+  mode modeu CMD:ARG+ then
   s" HABU_OUT" out outu CMD:ENV+
   ENGINE-CANDIDATE:PATH$ 120000 >MS CMD:RUN-OUTCOME
   PROCESS:ENDING>RC RC>N {: rc:n :}
   CMD:OUT$ mode modeu DONE$ ENDS-WITH? {: done:bool :}
   done  rc 0 =  and 0= if rc SHOW then
   mode modeu s" stop" STR= if
      s" the build stopped after each failed build" T-LABEL
   else
      s" the build compiled its loads and host.f none" T-LABEL
   then
   done TTRUE
   s" the build exits 0" T-LABEL  rc 0 T= ;

: RUN-PROG ( CMD:command ptr u8 n -- CMD:command ) {: out:ptr outu:n :}
   CMD:RESET
   out outu s" prog" JOINED FS:JOIN-PATH {: u:n :}
   JOINED u 60000 >MS CMD:RUN-OUTCOME PROCESS:ENDING>RC RC>N {: rc:n :}
   CMD:OUT$ s\" Hi\nok\n" STR=  rc 0 =  and 0= if rc SHOW then
   s" the program prints Hi and ok" T-LABEL  CMD:OUT$ s\" Hi\nok\n" T$=
   s" the program exits 0" T-LABEL  rc 0 T= ;


: LIST ( ptr u8 n ptr u8 -- n ) {: out:ptr outu:n dst:ptr :}
   out outu s" obj" OBJ-DIR FS:JOIN-PATH {: du:n :}
   OBJ-DIR du dst CAP FS-LIST:NAMES ;

: LIST-A ( -- ) A$ NAMES-A LIST NAMES-A-U ! ;
: LIST-B ( -- ) B$ NAMES-B LIST NAMES-B-U ! ;
: LIST-C ( -- ) C$ NAMES-C LIST NAMES-C-U ! ;

\ A build that made no obj directory lists no names.
: LIST-OR-NONE ( [ -- ] -- ) catch {: rc:n :}
   rc 0 =  rc E-FS-LIST-OPEN =  or 0= if rc throw then ;

: NAME ( n -- ptr u8 n )
   NAME-BYTES 1 + * {: off:n :}  NAMES-A off + NAME-BYTES ;

: FIXTURE$ ( n -- ptr u8 n )
   {: j:n :}
   j 0 = if s" test/fixtures/file-objects/count.f" exit then
   j 1 = if s" test/fixtures/file-objects/shape.f" exit then
   j 2 = if s" test/fixtures/file-objects/main.f" exit then
   j 3 = if s" test/fixtures/file-objects/again.f" exit then
   s" test/fixtures/file-objects/reopen.f" ;

\ EXPECT: the INPUT section of the file: its size, low byte first, and SHA-256.
: EXPECT! ( ptr u8 n -- ) {: path:ptr pathu:n :}
   path pathu SOURCE-BYTES CAP FS:READ-ALL {: u:n :}
   8 0 ?do u i 8 * rshift $FF and EXPECT i + c! loop
   CTX SOURCE-BYTES u EXPECT 8 + SHA256-IN ;

\ The fixture whose INPUT section this is, or -1.
: FIXTURE-OF ( ptr u8 -- n )
   {: input:ptr :}
   -1  FIXTURES 0 ?do
      i FIXTURE$ EXPECT!
      input INPUT-BYTES EXPECT INPUT-BYTES STR= if drop i then
   loop ;

: READ-A ( n -- n ) NAME {: name:ptr nameu:n :}
   A$ name nameu OBJ$ BYTES-A CAP FS:READ-ALL ;

\ Into dst, the FILES row that names the object in BYTES-A: its key and SHA-256.
: KEEP-ROW ( n ptr u8 -- )
   {: au:n dst:ptr :}
   BYTES-A 8 + dst KEY-BYTES BYTE-COPY
   CTX BYTES-A au dst KEY-BYTES + SHA256-IN ;

: FILES-N ( -- n ) BYTES-A FILES-FIELD 8 + + LE:U64@ ROW-BYTES / ;

\ The FILES row of BYTES-A's object, from 1, that holds the row's bytes, or -1.
: ROW-OF ( ptr u8 -- n )
   {: row:ptr :}
   BYTES-A FILES-FIELD + LE:U64@ {: fat:n :}
   fat 0 <  fat FILES-N ROW-BYTES * + CAP >  or if -1 exit then
   FILES-N 0 ?do
      BYTES-A fat + i ROW-BYTES * + ROW-BYTES  row ROW-BYTES  STR= if
         i 1 + unloop exit
      then
   loop
   -1 ;

\ The fixture's file name.
: SHORT$ ( n -- ptr u8 n )
   {: j:n :}
   j 0 < if s" an object" exit then
   s" test/fixtures/file-objects/" {: d:ptr du:n :}
   j FIXTURE$ {: p:ptr pu:n :}
   p du + pu du - ;

\ The checked object's fixture, ": " and the text, as the next label.
: LABEL ( ptr u8 n -- )
   {: a:ptr u:n :}
   FIX @ SHORT$ {: f:ptr fu:n :}
   f LABEL-BUF fu BYTE-COPY
   s" : " {: c:ptr cu:n :}
   c LABEL-BUF fu + cu BYTE-COPY
   a LABEL-BUF fu + cu + u BYTE-COPY
   LABEL-BUF fu cu + u + T-LABEL ;

\ True when PIECES lies whole in the object's last bytes, at least 16 of them.
: PIECES-OK? ( n -- bool )
   {: au:n :}
   BYTES-A PIECES-FIELD + LE:U64@ {: pat:n :}
   BYTES-A PIECES-FIELD 8 + + LE:U64@ {: pu:n :}
   pat pu + au =  pu 16 >=  and  pu au <=  and ;

: OVERRUN ( -- ) WALK-END @ 1 + WALK-AT ! ;

\ The next number of the walk, or -1 past its end.
: NUM ( -- n )
   WALK-AT @ 8 + WALK-END @ > if OVERRUN -1 exit then
   BYTES-A WALK-AT @ + LE:U64@  8 WALK-AT +! ;

: NAME@ ( -- )
   NUM {: u:n :}
   u 0 <  WALK-AT @ u + WALK-END @ >  or if OVERRUN exit then
   WALK-AT @ ENT-NAME-AT !  u ENT-NAME-U !  u WALK-AT +! ;

: ENT-NAME$ ( -- ptr u8 n ) BYTES-A ENT-NAME-AT @ + ENT-NAME-U @ ;

: REF@ ( -- ) NUM ENT-FILE !  NUM ENT-REC ! ;

: EM-SEEN ( -- ) ENT-EM @ MAX-EM @ > if ENT-EM @ MAX-EM ! then ;

\ The next entry, by the format src/habu/linker.f's PIECES! writes: kinds 0-3
\ a dictionary record, kind 4 a package gaining its private wordlist.
: READ-ENTRY ( -- )
   -1 ENT-ROLE !  -1 ENT-FILE !  -1 ENT-REC !  -1 ENT-MINTED !  0 ENT-NAME-U !
   -1 ENT-EM !  -1 ENT-ENTRY !
   NUM {: kind:n :}  kind ENT-KIND !
   kind 4 = if REF@ exit then
   kind 0 <  kind 3 >  or if OVERRUN exit then
   1 WALK-RECS +!
   NAME@  NUM drop                       \ the flags
   NUM {: role:n :}  role ENT-ROLE !
   role 3 =  role 4 =  or if REF@ then
   kind 0 = if NUM ENT-EM !  EM-SEEN exit then
   kind 3 = if NUM ENT-EM !  NUM ENT-ENTRY !  EM-SEEN exit then
   kind 2 = if NUM drop  NUM drop exit then  \ the source's reference
   NUM ENT-MINTED ! ;

\ Starts a walk of BYTES-A's PIECES; the entry count.
: WALK-START ( n -- n )
   {: au:n :}
   au 8 - WALK-END !  0 WALK-RECS !  -1 MAX-EM !
   BYTES-A PIECES-FIELD + LE:U64@ 8 + WALK-AT !
   NUM ;

\ The entries read, at most n, before the walk reached its end.
: WALK ( n -- n )
   {: n:n :}
   n 0 ?do
      WALK-AT @ WALK-END @ >= if i unloop exit then
      READ-ENTRY
   loop
   n ;

\ The record index of the walk's entry of the kind the string names, with that
\ entry in the ENT- cells; -1 when there is none.
: FIND-ENTRY ( ptr u8 n n n -- n )
   {: name:ptr nameu:n kind:n au:n :}
   au PIECES-OK? 0= if -1 exit then
   au WALK-START 0 ?do
      WALK-AT @ WALK-END @ >= if -1 unloop exit then
      READ-ENTRY
      ENT-KIND @ kind =  ENT-NAME$ name nameu STR=  and if
         WALK-RECS @ 1 - unloop exit
      then
   loop
   -1 ;

: FIND-PKG ( ptr u8 n n -- n ) >r 1 r> FIND-ENTRY ;

\ The emission of the definition the string names, or -1.
: EM-OF ( ptr u8 n n -- n )
   {: name:ptr nameu:n au:n :}
   name nameu 0 au FIND-ENTRY 0 >= if ENT-EM @ exit then
   -1 ;


\ ---- CODE -----------------------------------------------------------------------
\ The CODE of the object in BYTES-A, read into tables: each emission by its
\ number, its functions' offsets, and a row for each call, address and does>
\ patch target with its reference's tag and fields (-1 where it has none).
64 constant EMS-CAP
256 constant FUNS-CAP
512 constant ROWS-CAP
EMS-CAP TYPED-BUFFER EM-S n              \ size
EMS-CAP TYPED-BUFFER EM-R n              \ return bytes
EMS-CAP TYPED-BUFFER EM-AT n             \ its first byte's offset in BYTES-A
EMS-CAP TYPED-BUFFER EM-F n              \ function count
EMS-CAP TYPED-BUFFER EM-FUN0 n           \ its first FUN-OFF row
EMS-CAP TYPED-BUFFER EM-P n              \ the does> patch site, or -1
FUNS-CAP TYPED-BUFFER FUN-OFF n
ROWS-CAP TYPED-BUFFER ROW-EM n
ROWS-CAP TYPED-BUFFER ROW-WHAT n         \ CALL-ROW, ADDR-ROW, PATCH-ROW or CELL-ROW
ROWS-CAP TYPED-BUFFER ROW-SITE n         \ a CELL-ROW's: its DATA offset
ROWS-CAP TYPED-BUFFER ROW-KIND n
ROWS-CAP TYPED-BUFFER ROW-TAG n
ROWS-CAP TYPED-BUFFER ROW-A n
ROWS-CAP TYPED-BUFFER ROW-B n
ROWS-CAP TYPED-BUFFER ROW-C n
0 constant CALL-ROW
1 constant ADDR-ROW
2 constant PATCH-ROW
3 constant CELL-ROW                      \ a DATA address cell, of emission -1
variable CODE-E                          \ E as read; -1 past the walk's end
variable N-EMS  variable N-FUNS  variable N-ROWS
variable FULL                            \ 1 once a table's cap stopped the walk
variable OBJ-RECS                        \ the object's records, from its entries

: PAST? ( -- bool ) WALK-AT @ WALK-END @ > ;

\ True, with the walk stopped, when a table holding used rows of cap is full.
: FULL? ( n n -- bool )
   {: used:n cap:n :}
   used cap < if false exit then
   1 FULL !  OVERRUN  true ;

\ The last reference read: its tag and the tag's fields, -1 where it has none.
variable REF-TAG  variable REF-A  variable REF-B  variable REF-C

\ Read the next reference. A tag above 5 stops the walk.
: NEXT-REF ( -- )
   NUM {: tag:n :}
   tag REF-TAG !  -1 REF-A !  -1 REF-B !  -1 REF-C !
   tag 0 =  tag 3 =  or if NUM REF-A !  NUM REF-B !  exit then
   tag 1 =  tag 4 =  or if NUM REF-A !  exit then
   tag 2 = if NUM REF-A !  NUM REF-B !  NUM REF-C !  exit then
   tag 5 <> if OVERRUN then ;

\ Read the next reference into row k.
: REF-ROW ( n -- )
   {: k:n :}
   NEXT-REF
   REF-TAG @ k ROW-TAG !  REF-A @ k ROW-A !  REF-B @ k ROW-B !  REF-C @ k ROW-C ! ;

\ A row of emission e: what it is, its site and kind, then the next reference.
: ROW+ ( n n n n -- )
   {: e:n what:n site:n kind:n :}
   N-ROWS @ {: k:n :}
   k ROWS-CAP FULL? if exit then
   e k ROW-EM !  what k ROW-WHAT !  site k ROW-SITE !  kind k ROW-KIND !
   k REF-ROW  1 N-ROWS +! ;

\ Emission e's call or address rows: the count, then site, kind and reference each.
: ROWS@ ( n n -- )
   {: e:n what:n :}
   NUM 0 ?do
      PAST? if unloop exit then
      e what NUM NUM ROW+
   loop ;

: FUNS@ ( n -- )
   {: e:n :}
   NUM {: f:n :}
   f e EM-F !
   N-FUNS @ f + FUNS-CAP > if 1 FULL !  OVERRUN exit then
   f 0 ?do NUM N-FUNS @ FUN-OFF !  1 N-FUNS +! loop ;

\ Emission e: S, R, the S bytes, its functions, calls, addresses and P.
: EM@ ( n -- )
   {: e:n :}
   NUM {: s:n :}
   NUM {: r:n :}
   s e EM-S !  r e EM-R !  WALK-AT @ e EM-AT !
   0 e EM-F !  N-FUNS @ e EM-FUN0 !  -1 e EM-P !
   s 0 <  WALK-AT @ s + WALK-END @ >  or if OVERRUN exit then
   s WALK-AT +!
   e FUNS@
   e CALL-ROW ROWS@
   e ADDR-ROW ROWS@
   NUM {: p:n :}
   p e EM-P !
   p 0 >= if e PATCH-ROW p -1 ROW+ then ;

\ CODE from WALK-AT: E, then each emission while the walk lasts.
: READ-CODE ( -- )
   0 N-EMS !  0 N-FUNS !  0 N-ROWS !  0 FULL !
   NUM CODE-E !
   CODE-E @ 0 ?do
      PAST? if unloop exit then
      N-EMS @ EMS-CAP FULL? if unloop exit then
      N-EMS @ EM@  1 N-EMS +!
   loop ;


\ ---- DATA and STRINGS ------------------------------------------------------------
\ After CODE: D, the D bytes, the cell rows into the row tables as CELL-ROWs,
\ then each string body's offset and length.
64 constant STRS-CAP
STRS-CAP TYPED-BUFFER STR-OFF n
STRS-CAP TYPED-BUFFER STR-U n
variable DATA-D                          \ D as read; -1 past the walk's end
variable DATA-AT                         \ the first DATA byte's offset in BYTES-A, or -1
variable N-STRS

: CELLS@ ( -- )
   NUM 0 ?do
      PAST? if unloop exit then
      -1 CELL-ROW NUM NUM ROW+
   loop ;

: STRS@ ( -- )
   NUM 0 ?do
      PAST? if unloop exit then
      N-STRS @ STRS-CAP FULL? if unloop exit then
      NUM N-STRS @ STR-OFF !  NUM N-STRS @ STR-U !  1 N-STRS +!
   loop ;

: READ-DATA ( -- )
   0 N-STRS !  -1 DATA-AT !
   NUM {: d:n :}
   d DATA-D !
   d 0 <  WALK-AT @ d + WALK-END @ >  or if OVERRUN exit then
   WALK-AT @ DATA-AT !  d WALK-AT +!
   CELLS@  STRS@ ;


\ ---- STORES ----------------------------------------------------------------------
\ After STRINGS: S, then each store row: the cell's reference, kind, bytes and the
\ value's reference, into tables of their own.
16 constant STORES-CAP
40 constant STORE-MIN                    \ a row's least bytes: tag 4, kind, bytes, tag 5
STORES-CAP TYPED-BUFFER ST-TAG n         \ the cell's reference: tag and fields
STORES-CAP TYPED-BUFFER ST-A n
STORES-CAP TYPED-BUFFER ST-B n
STORES-CAP TYPED-BUFFER ST-KIND n
STORES-CAP TYPED-BUFFER ST-BYTES n
STORES-CAP TYPED-BUFFER VAL-TAG n        \ the value's reference: tag and fields
STORES-CAP TYPED-BUFFER VAL-A n
STORES-CAP TYPED-BUFFER VAL-B n
STORES-CAP TYPED-BUFFER VAL-C n
variable STORES-S                        \ S as read; -1 past the walk's end
variable N-STORES

: STORE@ ( n -- )
   {: j:n :}
   NEXT-REF  REF-TAG @ j ST-TAG !  REF-A @ j ST-A !  REF-B @ j ST-B !
   NUM j ST-KIND !  NUM j ST-BYTES !
   NEXT-REF  REF-TAG @ j VAL-TAG !  REF-A @ j VAL-A !  REF-B @ j VAL-B !  REF-C @ j VAL-C ! ;

\ An S below 0, or of more rows than the walk's bytes left can hold, stops the walk.
: READ-STORES ( -- )
   0 N-STORES !
   NUM {: s:n :}
   s STORES-S !
   s 0 <  s  WALK-END @ WALK-AT @ - STORE-MIN /  >  or if OVERRUN exit then
   s 0 ?do
      PAST? if unloop exit then
      N-STORES @ STORES-CAP FULL? if unloop exit then
      N-STORES @ STORE@  1 N-STORES +!
   loop ;

\ The entries, then CODE, of the object in BYTES-A.
: WALK-CODE ( n -- )
   WALK-START WALK drop  WALK-RECS @ OBJ-RECS !  READ-CODE ;

: EM-OK? ( n -- bool ) dup 0 >=  swap N-EMS @ <  and ;

\ True when emission e's functions are all in FUN-OFF.
: FUNS-READ? ( n -- bool )
   {: e:n :}
   e EM-OK? 0= if false exit then
   e EM-FUN0 @ e EM-F @ +  N-FUNS @ <= ;

\ True when one of emission e's functions starts at off.
: FUN-AT? ( n n -- bool )
   {: e:n off:n :}
   e FUNS-READ? 0= if false exit then
   false  e EM-F @ 0 ?do  e EM-FUN0 @ i + FUN-OFF @ off = or  loop ;

\ True when row k's site lies in function f of its emission: at or after f's
\ offset and before the next function's, or the emission's end.
: IN-FUN? ( n n -- bool )
   {: k:n f:n :}
   k 0 < if false exit then
   k ROW-EM @ {: e:n :}
   e FUNS-READ? 0= if false exit then
   f 0 <  f e EM-F @ >=  or if false exit then
   e EM-FUN0 @ f + {: at:n :}
   f 1 + e EM-F @ < if at 1 + FUN-OFF @ else e EM-S @ then {: hi:n :}
   k ROW-SITE @ at FUN-OFF @ >=  k ROW-SITE @ hi <  and ;

-2 constant ANY                          \ a row field every value matches

\ True when got is want, or want is ANY; a want of -1, a failed lookup, never matches.
: FIELD= ( n n -- bool )
   {: got:n want:n :}
   want ANY = if true exit then
   want 0 >=  got want =  and ;

\ True when row k is a what row of emission e, of that kind, and its reference
\ has that tag and fields a and b. ANY matches any kind, tag or field.
: ROW? ( n n n n n n n -- bool )
   {: k:n e:n what:n kind:n tag:n a:n b:n :}
   k ROW-EM @ e =  k ROW-WHAT @ what =  and
   k ROW-KIND @ kind FIELD= and
   k ROW-TAG @ tag FIELD= and
   k ROW-A @ a FIELD= and
   k ROW-B @ b FIELD= and ;

\ The first such row, or -1.
: FIRST-ROW ( n n n n n n -- n )
   {: e:n what:n kind:n tag:n a:n b:n :}
   N-ROWS @ 0 ?do
      i e what kind tag a b ROW? if i unloop exit then
   loop
   -1 ;

: N-OF ( n n n n n n -- n )
   {: e:n what:n kind:n tag:n a:n b:n :}
   0  N-ROWS @ 0 ?do  i e what kind tag a b ROW? if 1 + then  loop ;

: HAS? ( n n n n n n -- bool ) FIRST-ROW 0 >= ;

\ The offset in this object's DATA that emission e's first data row names, or -1.
: OWN-OFF ( n -- n )
   {: e:n :}
   e ADDR-ROW 1 3 0 ANY FIRST-ROW {: k:n :}
   k 0 < if -1 exit then
   k ROW-B @ ;

\ True when emission e has data rows and each names this object's DATA at off.
: ALL-DATA? ( n n -- bool )
   {: e:n off:n :}
   e ADDR-ROW ANY ANY ANY ANY N-OF {: all:n :}
   all 0 >  e ADDR-ROW 1 3 0 off N-OF all =  and ;


\ count.f's FILES row and records, and the DATA offsets its TALLY, LATER and MARKED
\ bodies name.
: COUNT-FOUND ( n -- )
   {: au:n :}
   au COUNT-ROW KEEP-ROW
   s" FILE-OBJECTS-LATE" au FIND-PKG LATE-REC !
   s" BUMP" 0 au FIND-ENTRY BUMP-REC !
   s" HI" 0 au FIND-ENTRY HI-REC !
   s" TALLY" au EM-OF {: tally:n :}
   s" LATER" au EM-OF {: later:n :}
   s" MARKED" au EM-OF {: marked:n :}
   au WALK-CODE  tally OWN-OFF TALLY-OFF !  later OWN-OFF LATER-OFF !
   marked OWN-OFF MARKED-OFF ! ;

: SHAPE-FOUND ( n -- )
   {: au:n :}
   au SHAPE-ROW KEEP-ROW
   s" DOUBLE" 0 au FIND-ENTRY DOUBLE-REC !
   s" TWICE" 0 au FIND-ENTRY TWICE-REC ! ;

\ MATCHED, OBJ-FIX, the fixtures' object indices, FILES rows and records from
\ A's objects' INPUT sections.
: FIND-FIXTURES ( -- )
   0 MATCHED !  -1 SHAPE-I !  -1 REOPEN-I !  -1 COUNT-I !  -1 MAIN-I !
   -1 LATE-REC !  -1 BUMP-REC !  -1 HI-REC !  -1 DOUBLE-REC !  -1 TWICE-REC !
   -1 TALLY-OFF !  -1 LATER-OFF !  -1 MARKED-OFF !
   FILES 0 ?do
      i READ-A {: au:n :}
      BYTES-A INPUT-AT + FIXTURE-OF {: j:n :}
      j i OBJ-FIX !
      j 0 >= if 1 j 8 * lshift MATCHED +! then
      j 0 = if au COUNT-FOUND  i COUNT-I ! then
      j 1 = if au SHAPE-FOUND  i SHAPE-I ! then
      j 2 = if i MAIN-I ! then
      j 4 = if i REOPEN-I ! then
   loop ;

\ main.f's FILES: two rows, count.f's and shape.f's, found by content.
: MAIN-FILES ( -- )
   s" FILES starts where INPUT ends" LABEL
   BYTES-A FILES-FIELD + LE:U64@ FILES-AT T=
   s" FILES is two rows" LABEL
   BYTES-A FILES-FIELD 8 + + LE:U64@ ROW-BYTES 2 * T=
   s" FILES names count.f's object by key and SHA-256" LABEL  COUNT-ROW ROW-OF 0 > TTRUE
   s" FILES names shape.f's object by key and SHA-256" LABEL  SHAPE-ROW ROW-OF 0 > TTRUE ;

\ FILES: one row naming count.f's object for shape.f, whose EXPORT of BUMP is
\ an alias of count.f's record, and for reopen.f, which gives count.f's
\ FILE-OBJECTS-LATE its private wordlist; main.f's two (MAIN-FILES); 0 0 for
\ every other object.
: CHECK-FILES ( n -- )
   {: idx:n :}
   idx MAIN-I @ = if MAIN-FILES exit then
   idx SHAPE-I @ =  idx REOPEN-I @ =  or if
      s" FILES starts where INPUT ends" T-LABEL
      BYTES-A FILES-FIELD + LE:U64@ FILES-AT T=
      s" FILES is one row" T-LABEL  BYTES-A FILES-FIELD 8 + + LE:U64@ ROW-BYTES T=
      s" the row names count.f's object by key and SHA-256" T-LABEL
      BYTES-A FILES-AT + ROW-BYTES COUNT-ROW ROW-BYTES T$=
      exit
   then
   s" FILES is empty" T-LABEL
   BYTES-A FILES-FIELD + 16 ZEROS 16 T$= ;

\ PIECES follows FILES, or INPUT when FILES is empty, and ends the object.
: CHECK-PIECES ( n -- )
   {: au:n :}
   BYTES-A FILES-FIELD + LE:U64@ {: fat:n :}
   BYTES-A FILES-FIELD 8 + + LE:U64@ {: fu:n :}
   fu 0 = if FILES-AT else fat fu + then {: want:n :}
   BYTES-A PIECES-FIELD + LE:U64@ {: pat:n :}
   BYTES-A PIECES-FIELD 8 + + LE:U64@ {: pu:n :}
   s" PIECES starts where FILES ends" T-LABEL  pat want T=
   s" PIECES ends the object" T-LABEL  pat pu + au T=
   au PIECES-OK? if
      s" PIECES starts with the piece count 1" T-LABEL
      BYTES-A pat + LE:U64@ 1 T=
      s" the piece ends with REQUEST none" T-LABEL
      BYTES-A au + 8 - 8 NONE-BYTES 8 T$=
   then ;

: CHECK-WALK ( n -- )
   {: au:n :}
   au WALK-START {: n:n :}
   s" PIECES holds its entry count's entries" T-LABEL  n WALK n T=
   WALK-RECS @ OBJ-RECS !
   s" the entries end where CODE starts" LABEL  WALK-AT @ 8 + WALK-END @ <= TTRUE
   READ-CODE
   s" CODE fits the test's tables" LABEL  FULL @ 0 T=
   s" CODE ends where DATA starts" LABEL  WALK-AT @ 8 + WALK-END @ <= TTRUE
   READ-DATA
   s" DATA and STRINGS fit the test's tables" LABEL  FULL @ 0 T=
   s" STRINGS end where STORES start" LABEL  WALK-AT @ 8 + WALK-END @ <= TTRUE
   READ-STORES
   s" STORES fit the test's tables" LABEL  FULL @ 0 T=
   s" STORES end at the final REQUEST" LABEL  WALK-AT @ WALK-END @ T= ;


\ Emission e's functions: the first at 0, ascending, each below its size.
: FUNS-OK? ( n -- bool )
   {: e:n :}
   e FUNS-READ? 0= if false exit then
   e EM-F @ {: f:n :}
   e EM-FUN0 @ {: f0:n :}
   f 1 < if false exit then
   f0 FUN-OFF @ 0 =
   f 0 ?do
      f0 i + FUN-OFF @ e EM-S @ < and
      i 0 > if  f0 i + FUN-OFF @  f0 i + 1 - FUN-OFF @ >  and  then
   loop ;

: CHECK-EMS ( -- )
   s" CODE holds its emission count" LABEL  CODE-E @ 0 >= TTRUE
   s" each entry's emission is one of CODE's" LABEL  MAX-EM @ CODE-E @ < TTRUE
   N-EMS @ 0 ?do
      s" an emission's return bytes fit its size" LABEL
      i EM-R @ 0 >=  i EM-R @ i EM-S @ <=  and TTRUE
      s" an emission's functions start at 0 and ascend below its size" LABEL
      i FUNS-OK? TTRUE
   loop ;

\ Row k's offset in BYTES-A.
: SITE-AT ( n -- n ) dup ROW-EM @ EM-AT @  swap ROW-SITE @ + ;

\ True when row k's site is 4-aligned with u bytes after it in its emission.
: SITE-OK? ( n n -- bool )
   {: k:n u:n :}
   k ROW-SITE @ {: s:n :}
   s 0 >=  s 3 and 0 =  and  s u +  k ROW-EM @ EM-S @  <=  and ;

\ True when the 16-bit immediates of the n instruction words at at are 0.
: IMM0? ( n n -- bool )
   {: at:n words:n :}
   true
   words 0 ?do
      BYTES-A at + i 4 * + LE:U32@ 5 rshift $FFFF and 0 = and
   loop ;

: CODE-TAG? ( n -- bool ) dup 0 >=  swap 3 <  and ;

: CHECK-CALL ( n -- )
   {: k:n :}
   s" a call site is 4-aligned in its emission" LABEL  k 4 SITE-OK? TTRUE
   s" a call is a call or a tail" LABEL
   k ROW-KIND @ 0 =  k ROW-KIND @ 1 =  or TTRUE
   s" a call names a record, hb's code or a quotation" LABEL
   k ROW-TAG @ CODE-TAG? TTRUE
   k 4 SITE-OK? if s" a call site's bytes are 0" LABEL  BYTES-A k SITE-AT + LE:U32@ 0 T= then ;

: CHECK-ADDR ( n -- )
   {: k:n :}
   k ROW-KIND @ {: kind:n :}
   k ROW-TAG @ {: tag:n :}
   s" an address row is code or data" LABEL  kind 0 =  kind 1 =  or TTRUE
   kind 0 = if
      s" a code address chain is 4-aligned in its emission" LABEL  k 16 SITE-OK? TTRUE
      s" a code address names a record, hb's code or a quotation" LABEL  tag CODE-TAG? TTRUE
      k 16 SITE-OK? if
         s" a code address chain's immediates are 0" LABEL  k SITE-AT 4 IMM0? TTRUE
      then
   then
   kind 1 = if
      s" a data address chain is 4-aligned in its emission" LABEL  k 12 SITE-OK? TTRUE
      s" a data address names a file's DATA or the header data" LABEL
      tag 3 =  tag 4 =  or TTRUE
      k 12 SITE-OK? if
         s" a data address chain's immediates are 0" LABEL  k SITE-AT 3 IMM0? TTRUE
      then
   then ;

\ A created body's does> slot, there before any clause patches it; no fixture
\ gives a created word a clause, so every slot's target is none.
: CHECK-PATCH ( n -- )
   {: k:n :}
   s" a does> slot is 4-aligned in its emission" LABEL  k 4 SITE-OK? TTRUE
   k 4 SITE-OK? if s" a does> slot's bytes are 0" LABEL  BYTES-A k SITE-AT + LE:U32@ 0 T= then
   s" a does> slot's target is none" LABEL  k ROW-TAG @ 5 T= ;

\ True when row k's file-0 quotation is a function of one of CODE's emissions.
: QUOT-OK? ( n -- bool )
   {: k:n :}
   k ROW-B @ {: e:n :}
   k ROW-C @ {: f:n :}
   e EM-OK? 0= if false exit then
   f 0 >=  f e EM-F @ <  and ;

\ Row k's reference: tag 0-4, or 5 for a patch target or a cell row; a file 0 or
\ a FILES row; a file-0 record or quotation that this object holds.
: CHECK-REF ( n -- )
   {: k:n :}
   k ROW-TAG @ {: tag:n :}
   k ROW-A @ {: file:n :}
   k ROW-WHAT @ PATCH-ROW =  k ROW-WHAT @ CELL-ROW =  or if 6 else 5 then {: tags:n :}
   s" a reference's tag is known" LABEL  tag 0 >=  tag tags <  and TTRUE
   tag 0 =  tag 2 =  or  tag 3 =  or if
      s" a reference's file is 0 or a FILES row" LABEL
      file 0 >=  file FILES-N <=  and TTRUE
      file 0 =  tag 0 =  and if
         s" a file-0 record is one of the object's" LABEL
         k ROW-B @ 0 >=  k ROW-B @ OBJ-RECS @ <  and TTRUE
      then
      file 0 =  tag 2 =  and if
         s" a file-0 quotation is a function of CODE's" LABEL  k QUOT-OK? TTRUE
      then
   then ;

\ The number in DATA's 8 bytes at off.
: DATA@ ( n -- n ) BYTES-A DATA-AT @ + swap + LE:U64@ ;

\ True when DATA holds a whole cell at off.
: CELL-IN? ( n -- bool )
   {: off:n :}
   DATA-AT @ 0 >=  off 0 >=  and  off 7 and 0 =  and  off 8 +  DATA-D @ <=  and ;

\ An address cell: written as 0, an xt naming code, a data cell naming data, or
\ either holding 0 and every scratch cell naming none.
: CHECK-CELL ( n -- )
   {: k:n :}
   k ROW-SITE @ {: off:n :}
   k ROW-KIND @ {: kind:n :}
   k ROW-TAG @ {: tag:n :}
   s" a cell row is a cell of DATA" LABEL  off CELL-IN? TTRUE
   off CELL-IN? if s" a cell row's DATA bytes are 0" LABEL  off DATA@ 0 T= then
   s" a cell row is an xt, data or scratch cell" LABEL  kind 0 >=  kind 3 <  and TTRUE
   kind 0 = if
      s" an xt cell names a record, hb's code, a quotation or none" LABEL
      tag CODE-TAG?  tag 5 =  or TTRUE
   then
   kind 1 = if
      s" a data cell names a file's DATA, the header data or none" LABEL
      tag 3 =  tag 4 =  or  tag 5 =  or TTRUE
   then
   kind 2 = if s" a scratch cell names none" LABEL  tag 5 T= then ;

: CHECK-ROWS ( -- )
   N-ROWS @ 0 ?do
      i ROW-WHAT @ CALL-ROW = if i CHECK-CALL then
      i ROW-WHAT @ ADDR-ROW = if i CHECK-ADDR then
      i ROW-WHAT @ PATCH-ROW = if i CHECK-PATCH then
      i ROW-WHAT @ CELL-ROW = if i CHECK-CELL then
      i CHECK-REF
   loop ;

\ True when string j's body and the length cell before it lie in DATA.
: STR-IN? ( n -- bool )
   {: j:n :}
   j STR-OFF @ {: off:n :}
   j STR-U @ {: u:n :}
   DATA-AT @ 0 >=  off 8 >=  and  off 7 and 0 =  and  u 0 >=  and  off u +  DATA-D @ <=  and ;

: STR$ ( n -- ptr u8 n ) {: j:n :}  BYTES-A DATA-AT @ + j STR-OFF @ +  j STR-U @ ;

: CHECK-STR ( n -- )
   {: j:n :}
   s" a string's body and length cell lie in DATA" LABEL  j STR-IN? TTRUE
   j 0 > if s" string offsets ascend" LABEL  j STR-OFF @  j 1 - STR-OFF @  > TTRUE then
   j STR-IN? if
      s" a string's length cell holds its length" LABEL
      j STR-OFF @ 8 - DATA@  j STR-U @ T=
   then ;

: CHECK-DATA ( -- )
   s" DATA's D bytes fit the object" LABEL  DATA-AT @ 0 >= TTRUE
   N-STRS @ 0 ?do i CHECK-STR loop ;

\ Store row j's cell: its file, a FILES row or 0 for the header data, and its offset there.
: ST-FILE ( n -- n ) {: j:n :}  j ST-TAG @ 3 = if j ST-A @ else 0 then ;
: ST-OFF ( n -- n ) {: j:n :}  j ST-TAG @ 3 = if j ST-B @ else j ST-A @ then ;

\ True when a value's tag fits the store's kind: kind 0 code or none, kind 1 data or
\ none, kinds 2 and 3 none.
: VALUE-FITS? ( n n -- bool )
   {: kind:n tag:n :}
   kind 0 = if tag CODE-TAG?  tag 5 =  or exit then
   kind 1 = if tag 3 =  tag 4 =  or  tag 5 =  or exit then
   tag 5 = ;

\ True when every earlier row naming row j's file names a lower offset in it.
: STORE-ASCENDS? ( n -- bool )
   {: j:n :}
   true  j 0 ?do
      i ST-FILE j ST-FILE = if  i ST-OFF j ST-OFF <  and  then
   loop ;

: CHECK-STORE ( n -- )
   {: j:n :}
   j ST-TAG @ {: tag:n :}
   j ST-KIND @ {: kind:n :}
   s" a store's cell is a file's DATA or the header data" LABEL  tag 3 =  tag 4 =  or TTRUE
   tag 3 = if
      s" a store's file is a FILES row" LABEL  j ST-A @ 1 >=  j ST-A @ FILES-N <=  and TTRUE
   then
   s" a store is an xt, data, scratch or number cell" LABEL  kind 0 >=  kind 4 <  and TTRUE
   kind 0 >=  kind 3 <  and if
      s" an address cell's store's bytes are 0" LABEL  j ST-BYTES @ 0 T=
   then
   s" a store's value fits its kind" LABEL  kind j VAL-TAG @ VALUE-FITS? TTRUE
   s" stores into one file's data ascend" LABEL  j STORE-ASCENDS? TTRUE ;

: CHECK-STORES ( -- ) N-STORES @ 0 ?do i CHECK-STORE loop ;

\ The first cell row at DATA offset off, or -1.
: CELL-ROW-AT ( n -- n )
   {: off:n :}
   N-ROWS @ 0 ?do
      i ROW-WHAT @ CELL-ROW =  i ROW-SITE @ off =  and if i unloop exit then
   loop
   -1 ;

: N-CELLS ( -- n ) -1 CELL-ROW ANY ANY ANY ANY N-OF ;

\ True when the cell row at DATA offset off has that kind, tag and fields a and b.
: CELL-ROW? ( n n n n n -- bool )
   {: off:n kind:n tag:n a:n b:n :}
   off CELL-ROW-AT {: k:n :}
   k 0 < if false exit then
   k -1 CELL-ROW kind tag a b ROW? ;

\ Checks that string j's bytes are s; they read as empty unless DATA holds string j.
: STR-IS ( n ptr u8 n -- )
   {: j:n s:ptr u:n :}
   j N-STRS @ <  if j STR-IN? if j STR$ s u T$= exit then then
   BYTES-A 0 s u T$= ;


\ ---- the fixtures' calls and addresses, by name ----------------------------------
: COUNT-NAMES ( n -- )
   {: au:n :}
   s" HI" au EM-OF {: hi:n :}
   s" BUMP" au EM-OF {: bump:n :}
   s" HI calls hb's code" LABEL  hi CALL-ROW ANY 1 ANY ANY HAS? TTRUE
   s" TALLY's body names its own DATA" LABEL  TALLY-OFF @ 0 >= TTRUE
   s" BUMP's data rows name TALLY's cell" LABEL  bump TALLY-OFF @ ALL-DATA? TTRUE ;

: SHAPE-NAMES ( n -- )
   {: au:n :}
   s" DOUBLE" au EM-OF {: double:n :}
   s" EDGE" au EM-OF {: edge:n :}
   s" DOUBLE tail-calls TWICE" LABEL
   double CALL-ROW 1 0 0 TWICE-REC @ HAS? TTRUE
   s" EDGE's empty body names its own DATA at 0" LABEL
   edge ADDR-ROW 1 3 0 0 HAS? TTRUE ;

\ RUN reaches count.f's BUMP through shape.f's alias, so it names count.f's
\ record.
: RUN-NAMES ( n -- )
   {: au:n :}
   COUNT-ROW ROW-OF {: cf:n :}
   SHAPE-ROW ROW-OF {: sf:n :}
   s" RUN" au EM-OF {: run:n :}
   s" Q" au EM-OF OWN-OFF {: q:n :}
   s" XT" 0 au FIND-ENTRY {: xt:n :}
   s" RUN calls count.f's BUMP" LABEL  run CALL-ROW ANY 0 cf BUMP-REC @ HAS? TTRUE
   s" RUN calls count.f's HI" LABEL  run CALL-ROW ANY 0 cf HI-REC @ HAS? TTRUE
   s" RUN calls shape.f's DOUBLE" LABEL  run CALL-ROW ANY 0 sf DOUBLE-REC @ HAS? TTRUE
   s" RUN calls XT" LABEL  run CALL-ROW ANY 0 0 xt HAS? TTRUE
   s" RUN's TALLY names count.f's cell" LABEL  run ADDR-ROW 1 3 cf TALLY-OFF @ HAS? TTRUE
   s" Q's body names its own DATA" LABEL  q 0 >= TTRUE
   s" RUN's Q names Q's cell" LABEL  run ADDR-ROW 1 3 0 q HAS? TTRUE
   s" AT-EDGE" au EM-OF {: edge:n :}
   s" AT-EDGE names shape.f's end, not count.f's" LABEL  edge ADDR-ROW 1 3 sf 0 HAS? TTRUE ;

\ SET-Q's quotation is its second function, reached by a PC-relative adr and
\ no row.
: SET-Q-NAMES ( n -- )
   {: au:n :}
   COUNT-ROW ROW-OF {: cf:n :}
   s" SET-Q" au EM-OF {: set:n :}
   s" Q" au EM-OF OWN-OFF {: q:n :}
   s" SET-Q has two functions" LABEL  set EM-OK? if set EM-F @ else -1 then 2 T=
   s" SET-Q's quotation tail-calls count.f's BUMP" LABEL
   set CALL-ROW 1 0 cf BUMP-REC @ FIRST-ROW 1 IN-FUN? TTRUE
   s" SET-Q's Q names Q's cell" LABEL  set ADDR-ROW 1 3 0 q HAS? TTRUE ;

: MAKER-NAMES ( n -- )
   {: au:n :}
   s" MAKER" au EM-OF {: maker:n :}
   s" MAKER;does" 3 au FIND-ENTRY 0 >= if ENT-EM @ ENT-ENTRY @ else -1 -1 then
   {: does:n entry:n :}
   s" MAKER calls hb's code" LABEL  maker CALL-ROW ANY 1 ANY ANY HAS? TTRUE
   s" MAKER;does shares MAKER's emission" LABEL  maker 0 >=  does maker =  and TTRUE
   s" MAKER;does enters at a function of it above 0" LABEL
   maker entry FUN-AT?  entry 0 >  and TTRUE ;

\ count.f's object: eight entries, so no kind 4 writes FILE-OBJECTS-COUNT's private
\ wordlist again, and that package's entry says the wordlist is its own.
: CHECK-COUNT ( n -- )
   {: au:n :}
   s" count.f's object holds eight entries" T-LABEL  au WALK-START 8 T=
   s" FILE-OBJECTS-COUNT" au FIND-PKG 0 >= if ENT-MINTED @ else -1 then {: own:n :}
   s" FILE-OBJECTS-COUNT's private wordlist is its own" T-LABEL  own 1 T=
   s" count.f's object holds FILE-OBJECTS-LATE's package entry" T-LABEL
   LATE-REC @ 0 >= TTRUE ;

\ "reopen.f entry <n> " and the field's name; n is one digit.
: ENTRY-LABEL ( ptr u8 n -- )
   {: a:ptr u:n :}
   s" reopen.f entry " {: p:ptr pu:n :}
   p LABEL-BUF pu BYTE-COPY
   ENTRY-NO @ [char] 0 + LABEL-BUF pu + c!
   32 LABEL-BUF pu 1 + + c!
   a LABEL-BUF pu 2 + + u BYTE-COPY
   LABEL-BUF pu 2 + u + T-LABEL ;

\ The next entry of reopen.f's object: a record's kind, name and role.
: NEXT-RECORD ( n ptr u8 n n -- )
   {: kind:n name:ptr nameu:n role:n :}
   1 ENTRY-NO +!  READ-ENTRY
   s" kind" ENTRY-LABEL  ENT-KIND @ kind T=
   s" name" ENTRY-LABEL  ENT-NAME$ name nameu T$=
   s" wordlist role" ENTRY-LABEL  ENT-ROLE @ role T= ;

\ The last entry's reference: a file, 0 this object's, then a record in it.
: REF= ( n n -- )
   {: file:n rec:n :}
   s" reference's file" ENTRY-LABEL  ENT-FILE @ file T=
   s" reference's record" ENTRY-LABEL  ENT-REC @ rec T= ;

\ The next entry: kind 4, the referenced package gaining its private wordlist.
: NEXT-GAIN ( n n -- )
   {: file:n rec:n :}
   1 ENTRY-NO +!  READ-ENTRY
   s" kind" ENTRY-LABEL  ENT-KIND @ 4 T=
   file rec REF= ;

\ reopen.f's entries in order; each kind 4 sits where its wordlist is minted.
: CHECK-REOPEN ( n -- )
   {: au:n :}
   0 ENTRY-NO !
   au WALK-START {: n:n :}
   s" reopen.f's object holds nine entries" T-LABEL  n 9 T=
   1 s" FILE-OBJECTS-SOLO" 2 NEXT-RECORD
   s" record minted its private wordlist" ENTRY-LABEL  ENT-MINTED @ 0 T=
   0 s" ONE" 3 NEXT-RECORD  0 0 REF=
   1 LATE-REC @ NEXT-GAIN
   0 s" TWO" 4 NEXT-RECORD  1 LATE-REC @ REF=
   0 0 NEXT-GAIN
   0 s" THREE" 4 NEXT-RECORD  0 0 REF=
   0 s" MAKER" 3 NEXT-RECORD  0 0 REF=
   3 s" MAKER;does" 3 NEXT-RECORD  0 0 REF=
   0 s" TOP" 0 NEXT-RECORD ;


\ ---- the fixtures' cells and strings, by name ------------------------------------
\ count.f's TALLY, LATER and MARKED hold 0 as the file's compile leaves them, so
\ none has a cell row (LATER's and MARKED's are reopen.f's); the one string is the
\ `Hi` HI prints.
: COUNT-DATA ( -- )
   TALLY-OFF @ {: off:n :}
   LATER-OFF @ {: later:n :}
   MARKED-OFF @ {: marked:n :}
   s" TALLY's cell lies in DATA" LABEL  off CELL-IN? TTRUE
   off CELL-IN? if s" TALLY's cell is 0" LABEL  off DATA@ 0 T= then
   s" TALLY's cell has no cell row" LABEL  off CELL-ROW-AT -1 T=
   s" LATER's cell lies in DATA" LABEL  later CELL-IN? TTRUE
   later CELL-IN? if s" LATER's cell is 0" LABEL  later DATA@ 0 T= then
   s" LATER's cell has no cell row" LABEL  later CELL-ROW-AT -1 T=
   s" MARKED's cell lies in DATA" LABEL  marked CELL-IN? TTRUE
   marked CELL-IN? if s" MARKED's cell is 0" LABEL  marked DATA@ 0 T= then
   s" MARKED's cell has no cell row" LABEL  marked CELL-ROW-AT -1 T=
   s" DATA holds one string" LABEL  N-STRS @ 1 T=
   s" the string is Hi" LABEL  0 s" Hi" STR-IS ;

\ main.f's Q holds SET-Q's second function, stored with xt!; TO-Q and TO-TALLY name data,
\ SCRATCH is scratch, NOWHERE holds 0, and the store into count.f's TALLY adds no row.
: MAIN-DATA ( n -- )
   {: au:n :}
   COUNT-ROW ROW-OF {: cf:n :}
   s" SET-Q" au EM-OF {: set:n :}
   s" Q" au EM-OF OWN-OFF {: q:n :}
   q CELL-ROW-AT {: k:n :}
   s" Q's cell has a cell row" LABEL  k 0 >= TTRUE
   k 0 >= if
      s" Q's cell row is an xt naming SET-Q's quotation" LABEL
      k ROW-KIND @ 0 =  k ROW-TAG @ 2 =  and  k ROW-A @ 0 =  and
      k ROW-B @ set =  and  k ROW-C @ 1 =  and TTRUE
   then
   s" main.f's DATA has five cell rows" LABEL  N-CELLS 5 T=
   s" TO-Q's cell row is data naming Q" LABEL
   s" TO-Q" au EM-OF OWN-OFF  1 3 0 q CELL-ROW? TTRUE
   s" TO-TALLY's cell row is data naming count.f's TALLY" LABEL
   s" TO-TALLY" au EM-OF OWN-OFF  1 3 cf TALLY-OFF @ CELL-ROW? TTRUE
   s" SCRATCH's cell row is scratch naming none" LABEL
   s" SCRATCH" au EM-OF OWN-OFF  2 5 ANY ANY CELL-ROW? TTRUE
   s" NOWHERE's cell row is data naming none" LABEL
   s" NOWHERE" au EM-OF OWN-OFF  1 5 ANY ANY CELL-ROW? TTRUE
   s" DATA holds four strings" LABEL  N-STRS @ 4 T=
   s" the first string is red" LABEL  0 s" red" STR-IS
   s" the second string is green" LABEL  1 s" green" STR-IS
   s" the third string is blue" LABEL  2 s" blue" STR-IS
   s" the fourth string is ok" LABEL  3 s" ok" STR-IS ;

\ True when STORES' row r exists and is a store into file's DATA at off of that kind
\ and bytes, its value's reference of tag vtag.
: STORE-ROW? ( n n n n n n -- bool )
   {: r:n file:n off:n kind:n bytes:n vtag:n :}
   N-STORES @ r 1 + < if false exit then
   r ST-TAG @ 3 =  r ST-A @ file FIELD= and  r ST-B @ off FIELD= and
   r ST-KIND @ kind = and  r ST-BYTES @ bytes = and  r VAL-TAG @ vtag = and ;

\ main.f's top level stores RUN's xt into the header's entry cell and 5 into
\ count.f's TALLY, a cell with no row.
: MAIN-STORES ( n -- )
   {: au:n :}
   COUNT-ROW ROW-OF {: cf:n :}
   s" RUN" 0 au FIND-ENTRY {: run:n :}
   s" STORES is two rows" LABEL  STORES-S @ 2 T=
   s" the first store is an xt cell, the header's entry cell" LABEL
   N-STORES @ 1 >= if
      0 ST-TAG @ 4 =  0 ST-A @ APP-ENTRY:XT-CELL =  and
      0 ST-KIND @ 0 =  and  0 ST-BYTES @ 0 =  and
   else false then TTRUE
   s" the entry store's value is RUN's record" LABEL
   N-STORES @ 1 >= if
      0 VAL-TAG @ 0 =  0 VAL-A @ 0 =  and  0 VAL-B @ run FIELD=  and
   else false then TTRUE
   s" the second store is 5 into count.f's TALLY, with no row" LABEL
   1 cf TALLY-OFF @ 3 5 5 STORE-ROW? TTRUE ;

: SIZES= ( n n n -- )
   {: d:n k:n s:n :}
   s" DATA's size" LABEL  DATA-D @ d T=
   s" DATA's cell rows" LABEL  N-CELLS k T=
   s" DATA's strings" LABEL  N-STRS @ s T= ;

\ reopen.f's DATA is one string: MAKER's does> effect ` -- n `, 6 bytes after
\ its length cell.
: REOPEN-DATA ( -- ) 14 0 1 SIZES= ;

\ reopen.f's top level stores TOP's xt into count.f's LATER, an xt cell, and marks
\ count.f's MARKED a data cell holding 0, with no store.
: REOPEN-STORES ( n -- )
   {: au:n :}
   COUNT-ROW ROW-OF {: cf:n :}
   s" TOP" 0 au FIND-ENTRY {: top:n :}
   s" STORES is two rows" LABEL  STORES-S @ 2 T=
   s" the store is an xt cell, count.f's LATER" LABEL
   0 cf LATER-OFF @ 0 0 0 STORE-ROW? TTRUE
   s" the store's value is TOP's record" LABEL
   N-STORES @ 1 >= if  0 VAL-A @ 0 =  0 VAL-B @ top FIELD=  and  else false then TTRUE
   s" the second row is a data cell, count.f's MARKED, holding 0" LABEL
   1 cf MARKED-OFF @ 1 0 5 STORE-ROW? TTRUE ;

: CHECK-OBJECT ( n -- )
   {: idx:n :}
   idx NAME {: name:ptr nameu:n :}
   idx OBJ-FIX @ FIX !
   s" build B wrote the object too" T-LABEL
   NAMES-B NAMES-B-U @ name nameu CONTAINS? TTRUE
   NAMES-B NAMES-B-U @ name nameu CONTAINS? 0= if exit then
   idx READ-A {: au:n :}
   B$ name nameu OBJ$ BYTES-B CAP FS:READ-ALL {: bu:n :}
   s" both builds wrote the object's bytes" T-LABEL  BYTES-A au BYTES-B bu T$=
   s" the object holds its header, INPUT and PIECES" T-LABEL
   au FILES-AT 16 + >= TTRUE
   au FILES-AT 16 + < if exit then
   s" the object starts with its magic" T-LABEL  BYTES-A 8 s" HABUFOBJ" T$=
   BYTES-A 8 + HEX SHA256>HEX
   s" the object holds the key its name spells" T-LABEL
   HEX HEX-BYTES name HEX-BYTES T$=
   s" INPUT starts where the header ends" T-LABEL
   BYTES-A INPUT-FIELD + LE:U64@ INPUT-AT T=
   s" INPUT is a size and a SHA-256" T-LABEL
   BYTES-A INPUT-FIELD 8 + + LE:U64@ INPUT-BYTES T=
   s" REQUESTS and NAMES are empty" T-LABEL
   BYTES-A REQUESTS-FIELD + EMPTY-BYTES ZEROS EMPTY-BYTES T$=
   idx CHECK-FILES
   au CHECK-PIECES
   au PIECES-OK? 0= if exit then
   au CHECK-WALK
   CHECK-EMS  CHECK-ROWS  CHECK-DATA  CHECK-STORES
   FIX @ 3 = if s" CODE holds no emission" LABEL  CODE-E @ 0 T=  0 0 0 SIZES= then
   idx COUNT-I @ = if au CHECK-COUNT  au COUNT-NAMES  COUNT-DATA then
   idx SHAPE-I @ = if au SHAPE-NAMES  0 0 0 SIZES= then
   idx MAIN-I @ = if au RUN-NAMES  au SET-Q-NAMES  au MAIN-DATA  au MAIN-STORES then
   idx REOPEN-I @ = if au CHECK-REOPEN  au MAKER-NAMES  REOPEN-DATA  au REOPEN-STORES then
   idx MAIN-I @ =  idx REOPEN-I @ =  or 0= if
      s" STORES is empty" LABEL  STORES-S @ 0 T=
   then ;

\ Build C's one object: count.f's, the same name and bytes as A's.
: CHECK-STOP ( -- )
   s" build C wrote one object" T-LABEL
   NAMES-C NAMES-C-U @ s" .hfo" COUNT-SUB 1 T=
   s" C's names are one key" T-LABEL  NAMES-C-U @ 1 LISTING-BYTES T=
   s" A wrote count.f's object" T-LABEL  COUNT-I @ 0 >= TTRUE
   NAMES-C-U @ 1 LISTING-BYTES <>  COUNT-I @ 0 <  or if exit then
   COUNT-I @ NAME {: name:ptr nameu:n :}
   s" C's object is count.f's" T-LABEL  NAMES-C NAME-BYTES name nameu T$=
   NAMES-C NAME-BYTES name nameu STR= 0= if exit then
   COUNT-I @ READ-A {: au:n :}
   C$ name nameu OBJ$ BYTES-B CAP FS:READ-ALL {: cu:n :}
   s" C's count.f object has A's bytes" T-LABEL  BYTES-A au BYTES-B cu T$= ;

: CHECKS ( -- )
   [:
      A$ s" " BUILD  A$ RUN-PROG  B$ s" second" BUILD  C$ s" stop" BUILD
   ;] CMD:WITH-COMMAND
   0 NAMES-A-U !  ['] LIST-A LIST-OR-NONE
   0 NAMES-B-U !  ['] LIST-B LIST-OR-NONE
   0 NAMES-C-U !  ['] LIST-C LIST-OR-NONE
   s" build A wrote six objects" T-LABEL
   NAMES-A NAMES-A-U @ s" .hfo" COUNT-SUB FILES T=
   s" A's names are six keys" T-LABEL  NAMES-A-U @ FILES LISTING-BYTES T=
   s" build B wrote seven objects" T-LABEL
   NAMES-B NAMES-B-U @ s" .hfo" COUNT-SUB FILES-B T=
   s" B's names are seven keys" T-LABEL  NAMES-B-U @ FILES-B LISTING-BYTES T=
   NAMES-A-U @ FILES LISTING-BYTES = if
      FIND-FIXTURES
      s" the INPUT sections are the files' sizes and SHA-256s" T-LABEL
      MATCHED @ $0102010101 T=
      s" one object is shape.f's" T-LABEL  SHAPE-I @ 0 >= TTRUE
      s" one object is main.f's" T-LABEL  MAIN-I @ 0 >= TTRUE
      s" one object is reopen.f's" T-LABEL  REOPEN-I @ 0 >= TTRUE
      FILES 0 ?do i CHECK-OBJECT loop
      CHECK-STOP
   then ;


: REMOVE-A ( -- ) A$ FS:REMOVE-TREE ;
: REMOVE-B ( -- ) B$ FS:REMOVE-TREE ;
: REMOVE-C ( -- ) C$ FS:REMOVE-TREE ;

: WITH-C ( -- )
   s" habu-file-objects-c" FS:MKTEMP-DIR {: c:ptr u:n :}
   c OUT-C u BYTE-COPY  u OUT-C-U !
   ['] CHECKS ['] REMOVE-C finally ;

: WITH-B ( -- )
   s" habu-file-objects-b" FS:MKTEMP-DIR {: b:ptr u:n :}
   b OUT-B u BYTE-COPY  u OUT-B-U !
   ['] WITH-C ['] REMOVE-B finally ;

public

: RUN ( -- )
   T-RESET
   8 0 ?do $FF NONE-BYTES i + c! loop
   s" habu-file-objects-a" FS:MKTEMP-DIR {: a:ptr u:n :}
   a OUT-A u BYTE-COPY  u OUT-A-U !
   ['] WITH-B ['] REMOVE-A finally
   T-REPORT ;

;package

FILE-OBJECTS-TEST:RUN
