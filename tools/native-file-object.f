\ One FILE object contains its own ordered native ranges and original loader
\ actions. Child code stays in the child's object. The outer input transcript
\ is checked before any range is installed.
require src/lib/errors.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/memory.f
require src/lib/le.f
require tools/native-unit-file.f
require tools/native-file-inputs.f

package NFILE-OBJECT
private

128 constant HEADER-BYTES
32 constant EVENT-BYTES
1 constant RANGE
2 constant LOAD
15 constant VERSION
32 constant SIGNATURE-BYTES
22 cells constant SCOPE-BYTES
18 cells constant HISTORY-BYTES
8 SIGNATURE-BYTES + SCOPE-BYTES + HISTORY-BYTES + constant RANGE-OVERHEAD
112 constant LOAD-HEAD
14 constant HANDLE-CELLS
0 constant H-PATH
1 constant H-PATH-U
2 constant H-ROOT
3 constant H-ROOT-U
4 constant H-KEY
5 constant H-SOURCE-U
6 constant H-COUNT
7 constant H-FILE-U
8 constant H-EVENT-START
9 constant H-EVENT-END
10 constant H-SCAN
11 constant H-LEFT
12 constant H-INPUT
13 constant H-INPUT-U
6 constant ROW-CELLS
ROW-CELLS cells constant ROW-BYTES
0 constant E-KIND
1 constant E-END
2 constant E-MODE
3 constant E-OFF
4 constant E-U
5 constant E-PREP

DYNAMIC-BUFFER HANDLES n
DYNAMIC-BUFFER EVENT-P ptr u8
DYNAMIC-BUFFER EVENT-CAP-N n
DYNAMIC-BUFFER TEXT u8
DYNAMIC-BUFFER PAYLOAD u8
DYNAMIC-BUFFER FILE-P ptr u8
DYNAMIC-BUFFER FILE-CAP-N n
DYNAMIC-BUFFER OUTPUT u8
DYNAMIC-BUFFER RANGE-BUF u8
variable HANDLE-N
variable TEXT-N
variable PAYLOAD-N
variable OUTPUT-N
variable WALK-PRIOR
variable WALK-OFF
variable HEADER-H
PTR-VARIABLE CHECK-P
variable CHECK-U

: BAD ( -- ) E-NUNIT-FILE throw ;
: STALE ( -- ) E-NUNIT-PROFILE throw ;
: H@ ( n n -- n ) {: h:n field:n :} h HANDLE-CELLS * field + HANDLES @ ;
: H! ( n n n -- ) {: x:n h:n field:n :}
   x h HANDLE-CELLS * field + HANDLES ! ;
: E@ ( n n n -- n ) {: h:n row:n field:n :}
   h EVENT-P @ row ROW-BYTES * field cells + + CELL-VIEW @ ;
: E! ( n n n n -- ) {: x:n h:n row:n field:n :}
   x h EVENT-P @ row ROW-BYTES * field cells + + CELL-VIEW ! ;
: H-CHECK ( n -- n ) {: h:n :}
   h 0 < h HANDLE-N @ >= or if BAD then h ;
: EVENT-ENSURE ( n n -- ) {: h:n need:n :}
   MEM-MAX-N ROW-BYTES / {: limit:n :}
   need 0 < need limit > or if BAD then
   h EVENT-CAP-N @ {: oldcap:n :}
   need oldcap <= if exit then
   oldcap 0= if 4 else
      oldcap limit 2 / > if limit else oldcap 2 * then
   then
      need max {: cap:n :}
   cap ROW-BYTES * map-anon 0<> if drop E-MEM-MAP throw then
   {: fresh:ptr :}
   oldcap 0<> if
      h EVENT-P @ fresh h H-COUNT H@ ROW-BYTES * BYTE-COPY
      h EVENT-P @ oldcap ROW-BYTES * munmap 0<> if E-MEM-UNMAP throw then
   then
   fresh h EVENT-P ! cap h EVENT-CAP-N ! ;
: TEXT+ ( ptr u8 n -- n ) {: a:ptr u:n :}
   u 0 < u MEM-MAX-N TEXT-N @ - > or if BAD then
   TEXT-N @ {: off:n :}
   u 0 > if
      off u + TEXT-RESERVE
      a off TEXT u BYTE-COPY
   then
   u TEXT-N +! off ;
: PAYLOAD+ ( ptr u8 n -- n ) {: a:ptr u:n :}
   u 0 < u MEM-MAX-N PAYLOAD-N @ - > or if BAD then
   PAYLOAD-N @ {: off:n :}
   u 0 > if
      off u + PAYLOAD-RESERVE
      a off PAYLOAD u BYTE-COPY
   then
   u PAYLOAD-N +! off ;
: OUTPUT+ ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0 < u MEM-MAX-N OUTPUT-N @ - > or if BAD then
   u 0 > if
      OUTPUT-N @ u + OUTPUT-RESERVE
      a OUTPUT-N @ OUTPUT u BYTE-COPY
   then
   u OUTPUT-N +! ;
: FIELD! ( n n -- ) OUTPUT LE:U64! ;
: FIELD@ ( ptr u8 -- n ) LE:U64@ ;
: RANGE-PARTS ( ptr u8 n -- ptr u8 n ptr u8 ptr u8 n n n n n n n n n n n n n n n n n n )
   {: bytes:ptr size:n :}
   size RANGE-OVERHEAD < if BAD then
   bytes LE:U64@ {: unitu:n :}
   unitu 0 <= unitu size RANGE-OVERHEAD - <> or if BAD then
   bytes 8 + {: unit:ptr :}
   unit unitu + {: signature:ptr :}
   signature SIGNATURE-BYTES + {: scope:ptr :}
   scope SCOPE-BYTES + {: history:ptr :}
   history LE:U64@ {: session:n :}
   history 8 + LE:U64@ {: module:n :}
   history 16 + LE:U64@ {: gen:n :}
   history 24 + LE:U64@ {: epoch:n :}
   history 32 + LE:U64@ {: before:n :}
   history 40 + LE:U64@ {: lease:n :}
   history 48 + LE:U64@ {: openlease:n :}
   history 56 + LE:U64@ {: openepoch:n :}
   history 64 + LE:U64@ {: beforemodule:n :}
   history 72 + LE:U64@ {: openmodulebefore:n :}
   history 80 + LE:U64@ {: openmoduleafter:n :}
   history 88 + LE:U64@ {: beforegen:n :}
   history 96 + LE:U64@ {: beforeepoch:n :}
   history 104 + LE:U64@ {: beforelease:n :}
   history 112 + LE:U64@ {: build:n :}
   history 120 + LE:U64@ {: beforebuild:n :}
   history 128 + LE:U64@ {: openbuildbefore:n :}
   history 136 + LE:U64@ {: openbuildafter:n :}
   session 0 < module 0 < or gen 0 < or epoch 0 < or
   before 0 < or lease 0 < or openlease 0 < or openepoch 0 < or
   beforemodule 0 < or openmodulebefore 0 < or openmoduleafter 0 < or
   beforegen 0 < or beforeepoch 0 < or beforelease 0 < or
   build 0 < or beforebuild 0 < or openbuildbefore 0 < or openbuildafter 0 < or
      if BAD then
   unit unitu signature scope session module gen epoch before lease openlease
      openepoch beforemodule openmodulebefore openmoduleafter
      beforegen beforeepoch beforelease build beforebuild openbuildbefore openbuildafter ;

: LOAD-CHECK ( ptr u8 n -- ) {: a:ptr u:n :}
   u LOAD-HEAD < if BAD then
   a LE:U64@ {: pathu:n :}
   a 8 + LE:U64@ {: rootu:n :}
   pathu 0 <= rootu 0 < or if BAD then
   pathu u LOAD-HEAD - > if BAD then
   rootu u LOAD-HEAD pathu + - <> if BAD then
   a 16 + LE:U64@ 0 < if BAD then ;

: NEW-HANDLE ( ptr u8 n ptr u8 n ptr u8 n n -- n )
   {: path:ptr pathu:n root:ptr rootu:n key:ptr keyu:n sourceu:n :}
   pathu 0 <= rootu 0 < or keyu 64 <> or sourceu 0 < or if BAD then
   HANDLE-N @ {: h:n :}
   h 1+ HANDLE-CELLS * HANDLES-RESERVE
   h 1+ FILE-P-RESERVE h 1+ FILE-CAP-N-RESERVE
   h 1+ EVENT-P-RESERVE h 1+ EVENT-CAP-N-RESERVE
   path pathu TEXT+ h H-PATH H!
   pathu h H-PATH-U H!
   root rootu TEXT+ h H-ROOT H!
   rootu h H-ROOT-U H!
   key keyu TEXT+ h H-KEY H!
   sourceu h H-SOURCE-U H!
   0 h H-COUNT H! 0 h H-FILE-U H!
   0 h H-EVENT-START H! 0 h H-EVENT-END H!
   0 h H-SCAN H! 0 h H-LEFT H!
   0 h H-INPUT H! 0 h H-INPUT-U H!
   NULL-PTR h FILE-P ! 0 h FILE-CAP-N !
   NULL-PTR h EVENT-P ! 0 h EVENT-CAP-N !
   1 HANDLE-N +! h ;

: APPEND ( n n n n ptr u8 n -- )
   {: h:n kind:n end:n mode:n bytes:ptr u:n :}
   h H-CHECK drop
   u 0 < end 0 < end h H-SOURCE-U H@ > or or if BAD then
   h H-COUNT H@ {: row:n :}
   row MEM-MAX-N ROW-BYTES / >= if BAD then
   h row 1+ EVENT-ENSURE
   kind h row E-KIND E! end h row E-END E! mode h row E-MODE E!
   bytes u PAYLOAD+ h row E-OFF E!
   u h row E-U E!
   -1 h row E-PREP E!
   row 1+ h H-COUNT H! ;

: CHECK-BUILD ( n -- ) {: h:n :}
   h H-COUNT H@ {: count:n :}
   count 1 < count 1 and 0= or if BAD then
   0 WALK-PRIOR !
   count 0 ?do
      i 1 and 0= if
         h i E-KIND E@ RANGE <>
         h i E-END E@ WALK-PRIOR @ < or
         h i E-MODE E@ 0 < or h i E-MODE E@ 3 > or if BAD then
         h i E-END E@ WALK-PRIOR !
      else
         h i E-KIND E@ LOAD <>
         h i E-END E@ WALK-PRIOR @ <> or
         h i E-MODE E@ 0 < or h i E-MODE E@ 1 > or
         h i E-U E@ 0 <= or if BAD then
      then
   loop
   WALK-PRIOR @ h H-SOURCE-U H@ <> if BAD then ;

: WRITE-EVENTS ( n -- ) {: h:n :}
   h H-COUNT H@ 0 ?do
      OUTPUT-N @ MEM-MAX-N EVENT-BYTES - > if BAD then
      OUTPUT-N @ EVENT-BYTES + OUTPUT-RESERVE
      h i E-KIND E@ OUTPUT-N @ OUTPUT LE:U64!
      h i E-END E@ OUTPUT-N @ 8 + OUTPUT LE:U64!
      h i E-MODE E@ OUTPUT-N @ 16 + OUTPUT LE:U64!
      h i E-U E@ OUTPUT-N @ 24 + OUTPUT LE:U64!
      EVENT-BYTES OUTPUT-N +!
      h i E-OFF E@ PAYLOAD h i E-U E@ OUTPUT+
   loop ;

: PACK ( n ptr u8 n -- ptr u8 n )
   {: h:n inputs:ptr inputu:n :}
   h H-CHECK CHECK-BUILD
   inputu 0 <= if BAD then
   0 OUTPUT-N !
   HEADER-BYTES OUTPUT-RESERVE
   HEADER-BYTES 0 ?do 0 i OUTPUT c! loop
   HEADER-BYTES OUTPUT-N !
   h H-PATH H@ TEXT h H-PATH-U H@ OUTPUT+
   h H-ROOT H@ TEXT h H-ROOT-U H@ OUTPUT+
   OUTPUT-N @ {: events0:n :}
   h WRITE-EVENTS
   OUTPUT-N @ events0 - {: eventsu:n :}
   inputs inputu OUTPUT+
   s" HBNFILE9" drop 0 OUTPUT 8 BYTE-COPY
   VERSION 8 FIELD!
   h H-PATH-U H@ 16 FIELD!
   h H-ROOT-U H@ 24 FIELD!
   h H-KEY H@ TEXT 32 OUTPUT 64 BYTE-COPY
   h H-SOURCE-U H@ 96 FIELD!
   h H-COUNT H@ 104 FIELD!
   eventsu 112 FIELD!
   inputu 120 FIELD!
   0 OUTPUT OUTPUT-N @ ;

: CHECK-INPUTS ( -- ) CHECK-P @ CHECK-U @ NFILE-INPUTS:CHECK ;
: INPUTS-FRESH? ( ptr u8 n -- bool )
   CHECK-U ! CHECK-P !
   ['] CHECK-INPUTS catch {: rc:n :}
   rc 0= if true exit then
   rc E-NUNIT-PROFILE = if false exit then
   rc throw ;

: FILE$ ( n -- ptr u8 n ) {: h:n :}
   h H-CHECK drop h FILE-P @ h H-FILE-U H@ ;
: KEY$ ( n -- ptr u8 n ) {: h:n :}
   h H-CHECK drop h H-KEY H@ TEXT 64 ;

: HEADER-CHECK ( n -- ) {: h:n :}
   h FILE$ {: a:ptr u:n :}
   u HEADER-BYTES < if BAD then
   a 8 s" HBNFILE9" STR= 0= if BAD then
   a 8 + FIELD@ VERSION <> if BAD then
   a 16 + FIELD@ h H-PATH-U H@ <> if BAD then
   a 24 + FIELD@ h H-ROOT-U H@ <> if BAD then
   a 32 + 64 h KEY$ STR= 0= if BAD then
   a 96 + FIELD@ h H-SOURCE-U H@ <> if STALE then
   a 104 + FIELD@ {: count:n :}
   count 1 < count 1 and 0= or if BAD then
   a 112 + FIELD@ {: eventsu:n :}
   a 120 + FIELD@ {: inputu:n :}
   eventsu 0 < inputu 0 < or if BAD then
   count eventsu EVENT-BYTES / > if BAD then
   h H-PATH-U H@ {: pathu:n :}
   h H-ROOT-U H@ {: rootu:n :}
   pathu u HEADER-BYTES - > if BAD then
   rootu u HEADER-BYTES pathu + - > if BAD then
   eventsu u HEADER-BYTES pathu + rootu + - > if BAD then
   inputu u HEADER-BYTES pathu + rootu + eventsu + - <> if BAD then
   a HEADER-BYTES + pathu h H-PATH H@ TEXT pathu STR= 0= if BAD then
   a HEADER-BYTES pathu + + rootu h H-ROOT H@ TEXT rootu STR= 0= if BAD then
   HEADER-BYTES pathu + rootu + {: events0:n :}
   events0 h H-EVENT-START H!
   events0 eventsu + h H-EVENT-END H!
   events0 eventsu + h H-INPUT H!
   inputu h H-INPUT-U H!
   count h H-COUNT H! ;

: HEADER-RUN ( -- ) HEADER-H @ HEADER-CHECK ;
: HEADER-FRESH? ( n -- bool ) HEADER-H !
   ['] HEADER-RUN catch {: rc:n :}
   rc 0= if true exit then
   rc E-NUNIT-PROFILE = if false exit then
   rc throw ;

: EVENTS-CHECK ( n -- ) {: h:n :}
   h FILE$ drop {: a:ptr :}
   h H-EVENT-START H@ WALK-OFF !
   0 WALK-PRIOR !
   h H-COUNT H@ 0 ?do
      EVENT-BYTES h H-EVENT-END H@ WALK-OFF @ - > if BAD then
      a WALK-OFF @ + {: row:ptr :}
      row FIELD@ {: kind:n :}
      row 8 + FIELD@ {: end:n :}
      row 16 + FIELD@ {: mode:n :}
      row 24 + FIELD@ {: size:n :}
      size 0 < if BAD then
      size h H-EVENT-END H@ WALK-OFF @ EVENT-BYTES + - > if BAD then
      i 1 and 0= if
         kind RANGE <> end WALK-PRIOR @ < or
         end h H-SOURCE-U H@ > or
         mode 0 < or mode 3 > or if BAD then
         size 0 > if
            row EVENT-BYTES + size RANGE-PARTS
                {: unit:ptr unitu:n signature:ptr scope:ptr
                   session:n module:n gen:n epoch:n before:n lease:n openlease:n
                   openepoch:n beforemodule:n openmodulebefore:n openmoduleafter:n
                   beforegen:n beforeepoch:n beforelease:n
                   build:n beforebuild:n openbuildbefore:n openbuildafter:n :}
             signature drop scope drop session drop module drop gen drop epoch drop
             before drop lease drop openlease drop openepoch drop beforemodule drop
             openmodulebefore drop openmoduleafter drop
             beforegen drop beforeepoch drop beforelease drop
             build drop beforebuild drop openbuildbefore drop openbuildafter drop
            unit unitu NUNIT-FILE:ARCH-AARCH64
               s" FILE" h KEY$ NUNIT-FILE:READ-BYTES
            NUNIT-FILE:CLOSE
         then
         end WALK-PRIOR !
      else
         kind LOAD <> end WALK-PRIOR @ <> or
         mode 0 < or mode 1 > or size 0 <= or if BAD then
         row EVENT-BYTES + size LOAD-CHECK
      then
      EVENT-BYTES size + WALK-OFF +!
   loop
   WALK-OFF @ h H-EVENT-END H@ <>
   WALK-PRIOR @ h H-SOURCE-U H@ <> or if BAD then ;

public

: START ( ptr u8 n ptr u8 n ptr u8 n n -- n ) NEW-HANDLE ;
: ADD-RANGE ( n n n n ptr u8 n ptr u8 ptr u8 n n n n n n n n n n n n n n n n n n -- )
   {: h:n end:n contributed:n compiled:n unit:ptr unitu:n
      signature:ptr scope:ptr session:n module:n gen:n epoch:n before:n lease:n
      openlease:n openepoch:n beforemodule:n openmodulebefore:n openmoduleafter:n
      beforegen:n beforeepoch:n beforelease:n
      build:n beforebuild:n openbuildbefore:n openbuildafter:n :}
   unitu 0 <= unitu MEM-MAX-N RANGE-OVERHEAD - > or if BAD then
   session 0 < module 0 < or gen 0 < or epoch 0 < or
   before 0 < or lease 0 < or openlease 0 < or openepoch 0 < or
   beforemodule 0 < or openmodulebefore 0 < or openmoduleafter 0 < or
   beforegen 0 < or beforeepoch 0 < or beforelease 0 < or
   build 0 < or beforebuild 0 < or openbuildbefore 0 < or openbuildafter 0 < or
      if BAD then
   unitu RANGE-OVERHEAD + {: size:n :}
   size RANGE-BUF-RESERVE
   unitu 0 RANGE-BUF LE:U64!
   unit 8 RANGE-BUF unitu BYTE-COPY
   signature 8 unitu + RANGE-BUF SIGNATURE-BYTES BYTE-COPY
   scope 8 unitu + SIGNATURE-BYTES + RANGE-BUF
      SCOPE-BYTES BYTE-COPY
   8 unitu + SIGNATURE-BYTES + SCOPE-BYTES + {: history:n :}
   session history RANGE-BUF LE:U64!
   module history 8 + RANGE-BUF LE:U64!
   gen history 16 + RANGE-BUF LE:U64!
   epoch history 24 + RANGE-BUF LE:U64!
   before history 32 + RANGE-BUF LE:U64!
   lease history 40 + RANGE-BUF LE:U64!
   openlease history 48 + RANGE-BUF LE:U64!
   openepoch history 56 + RANGE-BUF LE:U64!
   beforemodule history 64 + RANGE-BUF LE:U64!
   openmodulebefore history 72 + RANGE-BUF LE:U64!
   openmoduleafter history 80 + RANGE-BUF LE:U64!
   beforegen history 88 + RANGE-BUF LE:U64!
   beforeepoch history 96 + RANGE-BUF LE:U64!
   beforelease history 104 + RANGE-BUF LE:U64!
   build history 112 + RANGE-BUF LE:U64!
   beforebuild history 120 + RANGE-BUF LE:U64!
   openbuildbefore history 128 + RANGE-BUF LE:U64!
   openbuildafter history 136 + RANGE-BUF LE:U64!
   contributed 0 < contributed 1 > or
   compiled 0 < compiled 1 > or or if BAD then
   h RANGE end contributed compiled 2 * + 0 RANGE-BUF size APPEND ;
: SPLIT-RANGE ( ptr u8 n -- ptr u8 n ptr u8 ptr u8 n n n n n n n n n n n n n n n n n n ) RANGE-PARTS ;
: ADD-LOAD ( n n n n n n n -- )
   {: h:n end:n mode:n child:n size:n sec:n ns:n :}
   child H-CHECK drop
   child H-PATH-U H@ {: pathu:n :}
   child H-ROOT-U H@ {: rootu:n :}
   pathu MEM-MAX-N LOAD-HEAD - > if BAD then
   rootu MEM-MAX-N LOAD-HEAD pathu + - > if BAD then
   LOAD-HEAD pathu + rootu + {: u:n :}
   u RANGE-BUF-RESERVE
   pathu 0 RANGE-BUF LE:U64!
   rootu 8 RANGE-BUF LE:U64!
   child H-SOURCE-U H@ 16 RANGE-BUF LE:U64!
   size 24 RANGE-BUF LE:U64!
   sec 32 RANGE-BUF LE:U64!
   ns 40 RANGE-BUF LE:U64!
   child H-KEY H@ TEXT 48 RANGE-BUF 64 BYTE-COPY
   child H-PATH H@ TEXT 112 RANGE-BUF pathu BYTE-COPY
   child H-ROOT H@ TEXT 112 pathu + RANGE-BUF rootu BYTE-COPY
   h LOAD end mode 0 RANGE-BUF u APPEND ;
: LOAD-DETAIL ( ptr u8 n -- ptr u8 n ptr u8 n ptr u8 n n n n n )
   {: a:ptr u:n :}
   a u LOAD-CHECK
   a LE:U64@ {: pathu:n :}
   a 8 + LE:U64@ {: rootu:n :}
   a 112 + pathu
   a 112 + pathu + rootu
   a 48 + 64
   a 16 + LE:U64@
   a 24 + LE:U64@
   a 32 + LE:U64@
   a 40 + LE:U64@ ;
: POSITION ( n -- n ) {: h:n :}
   h H-CHECK drop
   h H-COUNT H@ h H-LEFT H@ - 1- dup 0 < if BAD then ;
: SELECT-CHILD ( n n -- ) {: h:n child:n :}
   h H-CHECK drop child H-CHECK drop
   h POSITION {: row:n :}
   h h H-COUNT H@ EVENT-ENSURE
   child 1+ h row E-OFF E! ;
: SELECT-INSTANCE ( n n -- ) {: h:n instance:n :}
   h H-CHECK drop
   h POSITION {: row:n :}
   h h H-COUNT H@ EVENT-ENSURE
   instance h row E-PREP E! ;
: SELECT-PREPARED ( n n -- ) {: h:n prepared:n :}
   h H-CHECK drop
   h POSITION {: row:n :}
   h h H-COUNT H@ EVENT-ENSURE
   prepared h row E-PREP E! ;
: SELECTED-PREPARED ( n -- n ) {: h:n :}
   h H-CHECK drop
   h POSITION {: row:n :}
   h row E-PREP E@ dup 0 < if BAD then ;
: SELECTED-CHILD ( n -- n ) {: h:n :}
   h H-CHECK drop
   h POSITION {: row:n :}
   h row E-OFF E@ 1- dup 0 < if BAD then ;
: SELECTED-INSTANCE ( n -- n ) {: h:n :}
   h H-CHECK drop
   h POSITION {: row:n :}
   h row E-PREP E@ dup 0 < if BAD then ;
: REWIND ( n -- ) {: h:n :}
   h H-CHECK drop
   h H-EVENT-START H@ h H-SCAN H!
   h H-COUNT H@ h H-LEFT H! ;
: WRITE ( n ptr u8 n ptr u8 n -- )
   {: h:n path:ptr pathu:n inputs:ptr inputu:n :}
   h inputs inputu PACK {: bytes:ptr u:n :}
   path pathu bytes u ATOMIC-WRITE-FILE ;

: TRY-OPEN ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n n -- n bool )
   {: artifact:ptr artifactu:n path:ptr pathu:n root:ptr rootu:n
      key:ptr keyu:n sourceu:n :}
   artifact artifactu FILE? 0= if -1 false exit then
   path pathu root rootu key keyu sourceu NEW-HANDLE {: h:n :}
   artifact artifactu FILE-SIZE {: size:n :}
   size HEADER-BYTES < if BAD then
   size MEM-ALLOC-BYTES {: a:ptr cap:n :}
   a h FILE-P ! cap h FILE-CAP-N !
   artifact artifactu a size READ-ALL size <> if BAD then
   size h H-FILE-U H!
   h HEADER-FRESH? 0= if -1 false exit then
   h EVENTS-CHECK
   h FILE-P @ h H-INPUT H@ + h H-INPUT-U H@ INPUTS-FRESH?
      0= if -1 false exit then
   h H-EVENT-START H@ h H-SCAN H!
   h H-COUNT H@ h H-LEFT H!
   h true ;

: NEXT ( n -- n n n ptr u8 n bool ) {: h:n :}
   h H-CHECK drop
   h H-LEFT H@ 0= if 0 0 0 NULL-PTR 0 false exit then
   h FILE-P @ h H-SCAN H@ + {: row:ptr :}
   row FIELD@ row 8 + FIELD@ row 16 + FIELD@
   row EVENT-BYTES + row 24 + FIELD@ true
   h H-SCAN H@ EVENT-BYTES row 24 + FIELD@ + + h H-SCAN H!
   h H-LEFT H@ 1- h H-LEFT H! ;

: INPUTS$ ( n -- ptr u8 n ) {: h:n :}
   h H-CHECK drop h FILE-P @ h H-INPUT H@ + h H-INPUT-U H@ ;
: PATH$ ( n -- ptr u8 n ) {: h:n :}
   h H-CHECK drop h H-PATH H@ TEXT h H-PATH-U H@ ;
: KEY-OF$ ( n -- ptr u8 n ) KEY$ ;
: ROOT$ ( n -- ptr u8 n ) {: h:n :}
   h H-CHECK drop h H-ROOT H@ TEXT h H-ROOT-U H@ ;
: RANGE-KIND ( -- n ) RANGE ;
: LOAD-KIND ( -- n ) LOAD ;

: CLOSE ( -- )
   HANDLE-N @ 0 ?do
      i FILE-P @ dup NULL-PTR <> if
         i FILE-CAP-N @ munmap 0 <> if E-MEM-UNMAP throw then
      else drop then
      i EVENT-P @ dup NULL-PTR <> if
         i EVENT-CAP-N @ ROW-BYTES * munmap 0 <> if E-MEM-UNMAP throw then
      else drop then
   loop
   HANDLES-RELEASE EVENT-P-RELEASE EVENT-CAP-N-RELEASE
   TEXT-RELEASE PAYLOAD-RELEASE
   FILE-P-RELEASE FILE-CAP-N-RELEASE OUTPUT-RELEASE RANGE-BUF-RELEASE
   0 HANDLE-N ! 0 TEXT-N ! 0 PAYLOAD-N ! 0 OUTPUT-N !
   0 WALK-PRIOR ! 0 WALK-OFF ! 0 HEADER-H !
   NULL-PTR CHECK-P ! 0 CHECK-U ! ;

;package
