\ file-objects.f - two builds of the files in test/fixtures/file-objects/, each
\ by build.f in a fresh hb with HABU_OUT its own temp directory: a fresh
\ process, since objects must be a function of the files, not of the process.
\ Each build compiles count.f, shape.f, main.f, reopen.f and again.f twice and
\ loads host.f after it closes. Build A refuses residue.f and writes six
\ objects; build B loads pad.f first and skips residue.f, so its later files'
\ records, emissions and data sit elsewhere, and writes seven. Each of A's
\ objects must be byte-identical to B's of the same name: an address or a
\ refused load's trace in an object shows as different bytes. Each of A's
\ PIECES reads entry by entry to its final REQUEST; reopen.f's entries are
\ checked field by field, and count.f's entry count and package entries. The
\ first build's program prints `Hi` and `ok` and exits 0, run in its own
\ process: the boundary under test is the written file.

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
PATH-CAP BUFFER: OBJ-DIR
PATH-CAP BUFFER: JOINED
CAP BUFFER: NAMES-A      variable NAMES-A-U
CAP BUFFER: NAMES-B      variable NAMES-B-U
CAP BUFFER: BYTES-A
CAP BUFFER: BYTES-B
CAP BUFFER: SOURCE-BYTES
SHA256-CTX-BYTES BUFFER: CTX
HEX-BYTES BUFFER: HEX
INPUT-BYTES BUFFER: EXPECT
EMPTY-BYTES BUFFER: ZEROS
ROW-BYTES BUFFER: COUNT-ROW              \ count.f's object key and SHA-256
8 BUFFER: NONE-BYTES                     \ REQUEST none: -1
128 BUFFER: LABEL-BUF
\ A byte per fixture file: the objects whose INPUT matched it.
variable MATCHED
variable SHAPE-I                         \ shape.f's object's name index, or -1
variable REOPEN-I                        \ reopen.f's, or -1
variable COUNT-I                         \ count.f's, or -1
variable LATE-REC                        \ count.f's LATE record, or -1
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
variable ENTRY-NO                        \ reopen.f's entry checked, from 1


: A$ ( -- ptr u8 n ) OUT-A OUT-A-U @ ;
: B$ ( -- ptr u8 n ) OUT-B OUT-B-U @ ;

: OBJ$ ( ptr u8 n ptr u8 n -- ptr u8 n ) {: out:ptr outu:n name:ptr nameu:n :}
   out outu s" obj" OBJ-DIR FS:JOIN-PATH {: du:n :}
   OBJ-DIR du name nameu JOINED FS:JOIN-PATH {: u:n :}
   JOINED u ;


\ A failed run shows its status and what it printed on both streams.
: SHOW ( CMD:command n -- CMD:command ) {: rc:n :}
   s" file-objects: exited " type rc .
   s" stdout: " type CMD:OUT$ type cr  s" stderr: " type CMD:ERR$ type cr ;

: COMPILED$ ( bool -- ptr u8 n )
   {: second:bool :}
   second if s\" compiled 7\n" exit then
   s\" compiled 6\n" ;

: BUILD ( CMD:command ptr u8 n bool -- CMD:command )
   {: out:ptr outu:n second:bool :}
   CMD:RESET
   s" --load" CMD:ARG+  s" test/fixtures/file-objects/build.f" CMD:ARG+
   second if s" --" CMD:ARG+  s" second" CMD:ARG+ then
   s" HABU_OUT" out outu CMD:ENV+
   ENGINE-CANDIDATE:PATH$ 120000 >MS CMD:RUN-OUTCOME
   PROCESS:ENDING>RC RC>N {: rc:n :}
   CMD:OUT$ second COMPILED$ ENDS-WITH? {: done:bool :}
   done  rc 0 =  and 0= if rc SHOW then
   s" the build compiled its loads, residue.f and host.f none" T-LABEL
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

\ COUNT-ROW: the FILES row that names count.f's object.
: COUNT-ROW! ( n -- )
   {: au:n :}
   BYTES-A 8 + COUNT-ROW KEY-BYTES BYTE-COPY
   CTX BYTES-A au COUNT-ROW KEY-BYTES + SHA256-IN ;

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

\ The next entry, by the format src/habu/linker.f's PIECES! writes: kinds 0-3
\ a dictionary record, kind 4 a package gaining its private wordlist.
: READ-ENTRY ( -- )
   -1 ENT-ROLE !  -1 ENT-FILE !  -1 ENT-REC !  -1 ENT-MINTED !  0 ENT-NAME-U !
   NUM {: kind:n :}  kind ENT-KIND !
   kind 4 = if REF@ exit then
   kind 0 <  kind 3 >  or if OVERRUN exit then
   1 WALK-RECS +!
   NAME@  NUM drop                       \ the flags
   NUM {: role:n :}  role ENT-ROLE !
   role 3 =  role 4 =  or if REF@ then
   kind 0 = if NUM drop exit then        \ the emission
   kind 3 = if NUM drop  NUM drop exit then
   kind 2 = if NUM drop  NUM drop exit then  \ the source's reference
   NUM ENT-MINTED ! ;

\ Starts a walk of BYTES-A's PIECES; the entry count.
: WALK-START ( n -- n )
   {: au:n :}
   au 8 - WALK-END !  0 WALK-RECS !
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

\ The record index of the walk's package entry the string names, with that entry
\ in the ENT- cells; -1 when there is none.
: FIND-PKG ( ptr u8 n n -- n )
   {: name:ptr nameu:n au:n :}
   au PIECES-OK? 0= if -1 exit then
   au WALK-START 0 ?do
      WALK-AT @ WALK-END @ >= if -1 unloop exit then
      READ-ENTRY
      ENT-KIND @ 1 =  ENT-NAME$ name nameu STR=  and if
         WALK-RECS @ 1 - unloop exit
      then
   loop
   -1 ;

\ MATCHED, SHAPE-I, REOPEN-I, COUNT-I, COUNT-ROW and LATE-REC from A's objects'
\ INPUT sections.
: FIND-FIXTURES ( -- )
   0 MATCHED !  -1 SHAPE-I !  -1 REOPEN-I !  -1 COUNT-I !  -1 LATE-REC !
   FILES 0 ?do
      i READ-A {: au:n :}
      BYTES-A INPUT-AT + FIXTURE-OF {: j:n :}
      j 0 >= if 1 j 8 * lshift MATCHED +! then
      j 0 = if
         au COUNT-ROW!  s" FILE-OBJECTS-LATE" au FIND-PKG LATE-REC !  i COUNT-I !
      then
      j 1 = if i SHAPE-I ! then
      j 4 = if i REOPEN-I ! then
   loop ;

\ FILES: one row naming count.f's object for shape.f, whose EXPORT of BUMP is
\ an alias of count.f's record, and for reopen.f, which gives count.f's
\ FILE-OBJECTS-LATE its private wordlist; 0 0 for every other object.
: CHECK-FILES ( n -- )
   {: idx:n :}
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
   s" the entries end at the final REQUEST" T-LABEL  WALK-AT @ WALK-END @ T= ;

\ count.f's object: six entries, so no kind 4 writes FILE-OBJECTS-COUNT's private
\ wordlist again, and that package's entry says the wordlist is its own.
: CHECK-COUNT ( n -- )
   {: au:n :}
   s" count.f's object holds six entries" T-LABEL  au WALK-START 6 T=
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

: CHECK-OBJECT ( n -- )
   {: idx:n :}
   idx NAME {: name:ptr nameu:n :}
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
   idx COUNT-I @ = if au CHECK-COUNT then
   idx REOPEN-I @ = if au CHECK-REOPEN then ;

: CHECKS ( -- )
   [: A$ false BUILD  A$ RUN-PROG  B$ true BUILD ;] CMD:WITH-COMMAND
   0 NAMES-A-U !  ['] LIST-A LIST-OR-NONE
   0 NAMES-B-U !  ['] LIST-B LIST-OR-NONE
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
      s" one object is reopen.f's" T-LABEL  REOPEN-I @ 0 >= TTRUE
      FILES 0 ?do i CHECK-OBJECT loop
   then ;


: REMOVE-A ( -- ) A$ FS:REMOVE-TREE ;
: REMOVE-B ( -- ) B$ FS:REMOVE-TREE ;

: WITH-B ( -- )
   s" habu-file-objects-b" FS:MKTEMP-DIR {: b:ptr u:n :}
   b OUT-B u BYTE-COPY  u OUT-B-U !
   ['] CHECKS ['] REMOVE-B finally ;

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
