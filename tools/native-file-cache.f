\ Ordered, make-style FILE objects at the ordinary source loader boundary.
\ One object holds this file's own native ranges and its original load actions.
\ A child always selects its own object at the actual child load.

require src/lib/errors.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/build-input.f
require src/lib/le.f
require src/lib/engine-id.f
require src/core/sha256.f
require src/core/source-producer.f
require src/habu/aot-arm.f
require src/habu/source-exec.f
require src/compiler/ir/id.f
require src/compiler/ir/context.f
require src/compiler/session/lease.f
require src/compiler/native/compiler.f
require src/compiler/workspace.f
require tools/native-file-scope.f
require tools/native-unit-object.f
require tools/native-file-object.f

package NFILE-CACHE
private

0 constant F-INPUT
8 constant F-RANGE-INPUT
16 constant F-LOADER-INPUT
24 constant F-UNIT
32 constant F-SOURCE-U
40 constant F-PENDING
48 constant F-STOPPED
56 constant F-DIRTY
64 constant F-STAMPED
72 constant F-RANGES
80 constant F-READ
88 constant F-ARTIFACT-U
96 constant F-META-SIZE
104 constant F-META-SEC
112 constant F-META-NS
120 constant F-PENDING-REQUIRES
128 constant F-CURSOR
136 constant F-CONT
144 constant F-DYNAMIC-END
152 constant F-SESSION-BEFORE
160 constant F-MODULE-BEFORE
168 constant F-CACHEABLE
176 constant F-GEN-BEFORE
184 constant F-EPOCH-BEFORE
192 constant F-LEASE-BEFORE
200 constant F-BUILD-BEFORE
208 constant F-LAYOUT-BEFORE
216 constant KEY-OFF
280 constant ARTIFACT-OFF
DYNAMIC-BUFFER FRAMES u8
variable FRAME-BYTES
variable DEPTH
variable HIT-N
variable COMPILE-N
variable READ-SIZE
variable READ-SEC
variable READ-NS
variable SELECT-SERIAL
variable SELECT-OPEN-LEASE
variable SELECT-GEN
variable SELECT-EPOCH
variable SELECT-LEASE
variable SELECT-MODULE
variable SELECT-OPEN-EPOCH
variable SELECT-OPEN-MODULE-BEFORE
variable SELECT-OPEN-MODULE-AFTER
variable SELECT-BUILD
variable SELECT-OPEN-BUILD-BEFORE
variable SELECT-OPEN-BUILD-AFTER
variable CHECKER-PLANNING
variable EX-SESSION-BEFORE
variable EX-MODULE-BEFORE
variable EX-GEN-BEFORE
variable EX-EPOCH-BEFORE
variable EX-LEASE-BEFORE
variable EX-BUILD-BEFORE
variable EX-LAYOUT-BEFORE
PTR-VARIABLE APP-PATH
variable APP-PATH-U
PTR-VARIABLE INPUTS-PATH
variable INPUTS-PATH-U
create HASH-CTX SHA256-CTX-BYTES allot
create KEY-CTX SHA256-CTX-BYTES allot
create SELECT-START SHA256-CTX-BYTES allot
create KEY-RAW 32 allot
create KEY-HEX 64 allot
create HASH-CELL 1 cells allot
\ A saved maker has already read its own producer closure when capture clears
\ the live loader history. Preserve that prefix separately; each application
\ build still folds the new process's live reads below.
create SAVED-PRODUCER 32 allot
variable SAVED-PRODUCER?
create NAME 76 allot
create SELECT-PATH FS-PATH-CAP allot
create SCOPE-BUF OUTER:SCOPE-BYTES allot

CAST: ACTION-XT ( n -- [ -- ] )
CAST: EXPORT-XT ( n -- [ -- ptr u8 n ] )
CAST: CHECKER-PREPARE-XT ( n -- [ ptr u8 n n n -- n ] )
CAST: CHECKER-INSTALL-XT ( n -- [ n -- ] )
CAST: UNIT-IMPORT-XT ( n -- [ ptr u8 n ptr u8 n ptr n n ptr n n -- ] )
CAST: SOURCE-XT ( n -- [ n n -- ] )
CAST: CONT-N ( [ n n -- ] -- n )

: BAD ( -- ) E-NUNIT-PROFILE throw ;
: OWNER ( -- ptr u8 )
   data-base NCOMP-DISPATCH:TARGET-DECL-CELL + 0 ptr-field @ ;
: MARK-CHECKER ( -- )
   OWNER CHECKER-OWNER-ABI:UNIT-MARK-OFF + CELL-VIEW @ ACTION-XT execute ;
: CHECKER$ ( -- ptr u8 n )
   OWNER CHECKER-OWNER-ABI:UNIT-EXPORT-OFF + CELL-VIEW @ EXPORT-XT execute ;
: CHECKER-BEGIN ( -- )
   CHECKER-PLANNING @ if BAD then
   OWNER CHECKER-OWNER-ABI:UNIT-PREPARE-BEGIN-OFF + CELL-VIEW @
      ACTION-XT execute
   1 CHECKER-PLANNING ! ;
: CHECKER-END ( -- )
   CHECKER-PLANNING @ 0= if exit then
   OWNER CHECKER-OWNER-ABI:UNIT-PREPARE-END-OFF + CELL-VIEW @
      ACTION-XT execute
   0 CHECKER-PLANNING ! ;
: CHECKER-PREPARE ( ptr u8 n n n -- n )
   OWNER CHECKER-OWNER-ABI:UNIT-PREPARE-OFF + CELL-VIEW @
      CHECKER-PREPARE-XT execute ;
: CHECKER-INSTALL ( n -- )
   OWNER CHECKER-OWNER-ABI:UNIT-INSTALL-OFF + CELL-VIEW @
      CHECKER-INSTALL-XT execute ;
: IMPORTER ( -- [ ptr u8 n ptr u8 n ptr n n ptr n n -- ] )
   ['] NPUB:IMPORT-FILE-UNIT ;
: SCOPE-SAVE ( -- ) SCOPE-BUF OUTER:SCOPE-SAVE ;
: SCOPE-INSTALL ( ptr u8 -- ) OUTER:SCOPE-INSTALL ;
: SCOPE-PREPARE ( ptr u8 n n -- ) OUTER:SCOPE-PREPARE ;

: FRAME ( n -- ptr u8 ) FRAME-BYTES @ * FRAMES ;
: TOP ( -- ptr u8 ) DEPTH @ 1- FRAME ;
: F@ ( n -- n ) TOP + CELL-VIEW @ ;
: F! ( n n -- ) TOP + CELL-VIEW ! ;
: KEY$ ( -- ptr u8 n ) TOP KEY-OFF + 64 ;
: ARTIFACT$ ( -- ptr u8 n ) TOP ARTIFACT-OFF + F-ARTIFACT-U F@ ;
: UNIT ( -- n ) F-UNIT F@ ;
: PATH$ ( -- ptr u8 n ) UNIT NFILE-OBJECT:PATH$ ;
: ROOT$ ( -- ptr u8 n ) UNIT NFILE-OBJECT:ROOT$ ;

: PUSH ( -- )
   DEPTH @ 1+ FRAME-BYTES @ * FRAMES-RESERVE
   1 DEPTH +!
   NFILE-INPUTS:MARK F-INPUT F!
   0 F-PENDING F! 0 F-STOPPED F! 0 F-RANGES F!
   1 F-CACHEABLE F! ;
: POP ( -- ) -1 DEPTH +! ;

: HASH-N ( ptr u8 n -- ) {: ctx:ptr x:n :}
   x HASH-CELL ! ctx HASH-CELL CELL SHA256-FEED ;
: HASH-S ( ptr u8 ptr u8 n -- ) {: ctx:ptr a:ptr u:n :}
   ctx u HASH-N
   u 0 > if ctx a u SHA256-FEED then ;
: CHAIN-BEGIN ( ptr u8 n -- ) {: mode:ptr modeu:n :}
   HASH-CTX SHA256-BEGIN
   HASH-CTX s" native-file-ranges-v15" HASH-S
   HASH-CTX ENGINE-ID:KEY$ HASH-S
   HASH-CTX mode modeu HASH-S
   SAVED-PRODUCER? @ if
      HASH-CTX s" saved-maker-producer-v1" HASH-S
      HASH-CTX SAVED-PRODUCER 32 HASH-S
   then
   HASH-CTX SOURCE-PRODUCER:HASH$ HASH-S ;
: CHAIN-SIGNATURE$ ( -- ptr u8 n )
   HASH-CTX KEY-CTX SHA256-CTX-BYTES BYTE-COPY
   KEY-CTX KEY-RAW SHA256-END
   KEY-RAW 32 ;
: FILE-KEY ( ptr u8 n ptr u8 n -- )
   {: path:ptr pathu:n root:ptr rootu:n :}
   HASH-CTX KEY-CTX SHA256-CTX-BYTES BYTE-COPY
   KEY-CTX path pathu HASH-S
   KEY-CTX root rootu HASH-S
   KEY-CTX KEY-RAW SHA256-END
   KEY-RAW KEY$ drop SHA256>HEX ;
: FILE-ROOT$ ( -- ptr u8 n )
   s" HABU_BUILD_CACHE" GETENV dup 0 > if exit then
   2drop s" .out" ;
: ARTIFACT-PATH ( -- )
   s" hb-file-" drop NAME 8 BYTE-COPY
   KEY$ drop NAME 8 + 64 BYTE-COPY
   s" .hfo" drop NAME 72 + 4 BYTE-COPY
   FILE-ROOT$ NAME 76 TOP ARTIFACT-OFF +
      JOIN-PATH F-ARTIFACT-U F! ;

: CHAIN-RANGE-BYTES ( n n n n ptr u8 n -- )
   {: h:n ordinal:n end:n dirty:n inputs:ptr inputu:n :}
   HASH-CTX s" R" HASH-S
   HASH-CTX ordinal HASH-N
   HASH-CTX dirty HASH-N
   dirty 0<> if HASH-CTX end HASH-N then
   HASH-CTX h NFILE-OBJECT:PATH$ HASH-S
   HASH-CTX h NFILE-OBJECT:ROOT$ HASH-S
   inputu 0 > if HASH-CTX inputs inputu SHA256-FEED then ;
: CHAIN-RANGE ( n n n n n -- )
   {: ordinal:n end:n dirty:n input0:n inputend:n :}
   input0 inputend NFILE-INPUTS:SPAN {: inputs:ptr inputu:n :}
   UNIT ordinal end dirty inputs inputu CHAIN-RANGE-BYTES ;
: CHAIN-LOAD-AT ( n ptr u8 n ptr u8 n -- )
   {: ordinal:n child:ptr childu:n root:ptr rootu:n :}
   HASH-CTX s" L" HASH-S
   HASH-CTX ordinal HASH-N
   HASH-CTX child childu HASH-S
   HASH-CTX root rootu HASH-S ;
: CHAIN-LOAD ( ptr u8 n ptr u8 n -- )
   {: child:ptr childu:n root:ptr rootu:n :}
   F-RANGES F@ child childu root rootu CHAIN-LOAD-AT ;

: ARTIFACT-FOR ( ptr u8 n -- ptr u8 n ) {: key:ptr keyu:n :}
   keyu 64 <> if BAD then
   s" hb-file-" drop NAME 8 BYTE-COPY
   key NAME 8 + 64 BYTE-COPY
   s" .hfo" drop NAME 72 + 4 BYTE-COPY
   FILE-ROOT$ NAME 76 SELECT-PATH BYTE-VIEW JOIN-PATH
      SELECT-PATH swap ;
: KEY-MATCH? ( ptr u8 n ptr u8 n ptr u8 n -- bool )
   {: path:ptr pathu:n root:ptr rootu:n key:ptr keyu:n :}
   keyu 64 <> if BAD then
   HASH-CTX KEY-CTX SHA256-CTX-BYTES BYTE-COPY
   KEY-CTX path pathu HASH-S
   KEY-CTX root rootu HASH-S
   KEY-CTX KEY-RAW SHA256-END
   KEY-RAW KEY-HEX SHA256>HEX
   KEY-HEX 64 key keyu STR= ;

\ Selection walks the whole dependency tree before importing anything. Model
\ its session and identity history so a range that cannot recreate the saved
\ live session falls back to source without partially installing the tree.
: SELECT-HISTORY ( n n n n n n n n n n n n n n n n n n -- bool )
   {: session:n module:n gen:n epoch:n before:n lease:n openlease:n
      openepoch:n beforemodule:n openmodulebefore:n openmoduleafter:n
      beforegen:n beforeepoch:n beforelease:n
      build:n beforebuild:n openbuildbefore:n openbuildafter:n :}
   gen epoch IR-CTX:CHECK-HISTORY
   lease NLEASE:CHECK-HISTORY
   openlease NLEASE:CHECK-HISTORY
   build IR-BUILD:CHECK-HISTORY
   beforebuild IR-BUILD:CHECK-HISTORY
   openbuildbefore IR-BUILD:CHECK-HISTORY
   openbuildafter IR-BUILD:CHECK-HISTORY
   module IR-ID:SERIAL-CHECK drop
   beforemodule IR-ID:SERIAL-CHECK drop
   openmodulebefore IR-ID:SERIAL-CHECK drop
   openmoduleafter IR-ID:SERIAL-CHECK drop
   before SELECT-SERIAL @ <> if false exit then
   beforemodule SELECT-MODULE @ <> if false exit then
   beforegen SELECT-GEN @ <> if false exit then
   beforeepoch SELECT-EPOCH @ <> if false exit then
   beforelease SELECT-LEASE @ <> if false exit then
   beforebuild SELECT-BUILD @ <> if false exit then
   build beforebuild < if false exit then
   module beforemodule < if false exit then
   gen beforegen < epoch beforeepoch < or lease beforelease < or
      if false exit then
   session 0= if
      openlease 0<> openepoch 0<> or openmodulebefore 0<> or
      openmoduleafter 0<> or openbuildbefore 0<> or
      openbuildafter 0<> or if false exit then
   else
      session IR-CTX:SESSION-GEN-OF gen > if false exit then
      openlease 0= openlease lease > or if false exit then
      openepoch 0 < openepoch epoch > or if false exit then
      openmoduleafter openmodulebefore <=
      openmoduleafter module > or if false exit then
      openbuildafter openbuildbefore <=
      openbuildafter build > or if false exit then
   then
   session before = if
      session 0<> if
         openlease SELECT-OPEN-LEASE @ <> if false exit then
         openepoch SELECT-OPEN-EPOCH @ <> if false exit then
         openmodulebefore SELECT-OPEN-MODULE-BEFORE @ <> if false exit then
         openmoduleafter SELECT-OPEN-MODULE-AFTER @ <> if false exit then
         openbuildbefore SELECT-OPEN-BUILD-BEFORE @ <> if false exit then
         openbuildafter SELECT-OPEN-BUILD-AFTER @ <> if false exit then
      then
   else
      before 0<> if
         lease SELECT-LEASE @ <= if false exit then
         epoch SELECT-EPOCH @ <= if false exit then
         SELECT-LEASE @ 1+ SELECT-LEASE !
         SELECT-EPOCH @ 1+ SELECT-EPOCH !
      then
      session 0<> if
         session IR-CTX:SESSION-GEN-OF SELECT-GEN @ <= if false exit then
         openlease SELECT-LEASE @ <= if false exit then
         openepoch SELECT-EPOCH @ < if false exit then
         openmodulebefore SELECT-MODULE @ < if false exit then
         openbuildbefore SELECT-BUILD @ < if false exit then
         session IR-CTX:SESSION-GEN-OF SELECT-GEN !
         openlease SELECT-LEASE !
         openepoch SELECT-EPOCH !
         openmoduleafter SELECT-MODULE !
         openbuildafter SELECT-BUILD !
      then
   then
   session SELECT-SERIAL !
   openlease SELECT-OPEN-LEASE !
   openepoch SELECT-OPEN-EPOCH !
   openmodulebefore SELECT-OPEN-MODULE-BEFORE !
   openmoduleafter SELECT-OPEN-MODULE-AFTER !
   openbuildbefore SELECT-OPEN-BUILD-BEFORE !
   openbuildafter SELECT-OPEN-BUILD-AFTER !
   gen SELECT-GEN @ max SELECT-GEN !
   epoch SELECT-EPOCH @ max SELECT-EPOCH !
   lease SELECT-LEASE @ max SELECT-LEASE !
   module SELECT-MODULE @ max SELECT-MODULE !
   build SELECT-BUILD @ max SELECT-BUILD !
   true ;

: SELECT-RANGE-HISTORY ( ptr u8 n ptr u8 -- bool )
   {: bytes:ptr size:n expected:ptr :}
   bytes size NFILE-OBJECT:SPLIT-RANGE
      {: unit:ptr unitu:n signature:ptr scope:ptr
         session:n module:n gen:n epoch:n before:n lease:n openlease:n
         openepoch:n beforemodule:n openmodulebefore:n openmoduleafter:n
         beforegen:n beforeepoch:n beforelease:n
         build:n beforebuild:n openbuildbefore:n openbuildafter:n :}
   unit drop unitu drop scope drop
   expected 32 signature 32 STR= 0= if false exit then
   session module gen epoch before lease openlease openepoch beforemodule
      openmodulebefore openmoduleafter beforegen beforeepoch beforelease
      build beforebuild openbuildbefore openbuildafter SELECT-HISTORY ;
: SELECT-RANGE ( n n n ptr u8 n -- bool )
   {: h:n end:n dirty:n bytes:ptr size:n :}
   CHAIN-SIGNATURE$ drop {: expected:ptr :}
   bytes size expected SELECT-RANGE-HISTORY 0= if false exit then
   bytes size NFILE-OBJECT:SPLIT-RANGE
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
   unit unitu h NFILE-OBJECT:KEY-OF$ s" FILE"
      NUNIT-OBJECT:PREPARE-BYTES-NAMED {: prepared:n :}
   prepared NUNIT-OBJECT:PREPARED-CHECKER-OF$
      CHECKER-PREPARE prepared swap
      NUNIT-OBJECT:PREPARED-CHECKER-ID!
   scope NUNIT-OBJECT:PROJECTED-COUNTS SCOPE-PREPARE
   prepared NUNIT-OBJECT:PREPARED-INPUTS-OF$
      {: inputs:ptr inputu:n :}
   h h NFILE-OBJECT:POSITION 2 / end dirty inputs inputu
      CHAIN-RANGE-BYTES
   h prepared NFILE-OBJECT:SELECT-PREPARED
   true ;

\ Both tree walks recurse directly. A load-time defer here becomes a declared
\ XT root in the AOT window and retains this build-only cache code in images.
: SELECT-LOAD ( n ptr u8 n -- n bool )
   {: h:n bytes:ptr size:n :}
   bytes size NFILE-OBJECT:LOAD-DETAIL
      {: path:ptr pathu:n root:ptr rootu:n key:ptr keyu:n
         sourceu:n stampu:n sec:n ns:n :}
   stampu drop sec drop ns drop
   h NFILE-OBJECT:POSITION 1+ 2 / path pathu root rootu
      CHAIN-LOAD-AT
   path pathu root rootu key keyu KEY-MATCH? 0= if 0 false exit then
   key keyu ARTIFACT-FOR {: artifact:ptr artifactu:n :}
   artifact artifactu path pathu root rootu key keyu sourceu
      NFILE-OBJECT:TRY-OPEN {: child:n hit:bool :}
   hit 0= if 0 false exit then
   h child NFILE-OBJECT:SELECT-CHILD
   child true ;

: SELECT-TREE-BODY ( n -- bool ) {: h:n :}
   begin
      h NFILE-OBJECT:NEXT
      {: kind:n end:n mode:n bytes:ptr size:n more:bool :}
      more
   while
      kind NFILE-OBJECT:RANGE-KIND = if
         h end mode bytes size SELECT-RANGE
      else
         kind NFILE-OBJECT:LOAD-KIND <> if BAD then
         h bytes size SELECT-LOAD
         if recurse else drop false then
      then
      0= if h NFILE-OBJECT:REWIND false exit then
   repeat
   h NFILE-OBJECT:REWIND true ;

: SELECT-REGISTRY-READY? ( -- bool )
   NLEASE:IDLE? 0= if false exit then
   IR-CTX:REGISTRY-IDLE? if true exit then
   \ A child may inherit the exact compiler session built by its parent.
   \ A root cache cannot identify a preexisting session from another load.
   DEPTH @ 1 > NCOMP:SESSION-LIVE? and
      IR-CTX:REGISTRY-SESSION-ONLY? and ;

: SELECT-CURRENT ( -- )
   NCOMP:SESSION-SERIAL SELECT-SERIAL !
   NCOMP:SESSION-LEASE SELECT-OPEN-LEASE !
   NCOMP:SESSION-EPOCH SELECT-OPEN-EPOCH !
   NCOMP:SESSION-MODULE-BEFORE SELECT-OPEN-MODULE-BEFORE !
   NCOMP:SESSION-MODULE-AFTER SELECT-OPEN-MODULE-AFTER !
   NCOMP:SESSION-BUILD-BEFORE SELECT-OPEN-BUILD-BEFORE !
   NCOMP:SESSION-BUILD-AFTER SELECT-OPEN-BUILD-AFTER !
   IR-CTX:HISTORY {: gen:n epoch:n :}
   gen SELECT-GEN ! epoch SELECT-EPOCH !
   NLEASE:HISTORY SELECT-LEASE !
   IR-ID:SERIAL-HISTORY SELECT-MODULE !
   IR-BUILD:HISTORY SELECT-BUILD ! ;

: START-RANGE ( -- )
   CWORK:LAYOUT-MARK F-LAYOUT-BEFORE F!
   NCOMP:SESSION-SERIAL F-SESSION-BEFORE F!
   IR-ID:SERIAL-HISTORY F-MODULE-BEFORE F!
   IR-CTX:HISTORY {: gen:n epoch:n :}
   gen F-GEN-BEFORE F! epoch F-EPOCH-BEFORE F!
   NLEASE:HISTORY F-LEASE-BEFORE F!
   IR-BUILD:HISTORY F-BUILD-BEFORE F!
   NUNIT-CAPTURE:ENTER
   MARK-CHECKER
   NFILE-INPUTS:MARK F-RANGE-INPUT F!
   0 F-DIRTY F! 0 F-STAMPED F! 0 F-STOPPED F! ;
: STAMP-START ( -- )
   PATH$ ROOT$
   F-META-SIZE F@ F-META-SEC F@ F-META-NS F@
      NFILE-INPUTS:ADD-STAMP ;
: STAMP ( -- )
   F-DIRTY F@ 0<> F-STAMPED F@ 0= and if
      STAMP-START
      1 F-STAMPED F!
   then ;
: PRODUCED? ( -- bool )
   NUNIT-CAPTURE:CODE$ nip 0 >
   NUNIT-CAPTURE:RECORDS$ nip 0 > or
   NUNIT-CAPTURE:DATA$ nip 0 > or
   NUNIT-CAPTURE:NEW-WIDS 0 > or
   NUNIT-CAPTURE:NEW-LITERALS 0 > or ;
: STORE-RANGE ( n n n ptr u8 n ptr u8 n ptr u8 n n n n n n n bool -- bool )
   {: h:n end:n dirty:n key:ptr keyu:n inputs:ptr inputu:n
      signature:ptr signatureu:n before:n beforemodule:n beforegen:n
      beforeepoch:n beforelease:n beforebuild:n strict:bool :}
   signatureu 32 <> if BAD then
   CHECKER$ {: checker:ptr checkeru:n :}
   strict if
      key keyu checker checkeru s" FILE" inputs inputu
         NUNIT-OBJECT:PACK-NAMED-INPUTS true
   else
      key keyu checker checkeru inputs inputu
         NUNIT-OBJECT:PACK-FILE-INPUTS?
   then {: bytes:ptr size:n admitted:bool :}
   admitted 0= if false exit then
   SCOPE-SAVE
   NCOMP:SESSION-SERIAL {: session:n :}
   IR-CTX:SESSION-SERIAL session = {: compatible:bool :}
   IR-ID:SERIAL-HISTORY {: module:n :}
   IR-CTX:HISTORY {: gen:n epoch:n :}
   NLEASE:HISTORY {: lease:n :}
   h end dirty bytes size signature SCOPE-BUF
      session module gen epoch before lease
      NCOMP:SESSION-LEASE NCOMP:SESSION-EPOCH beforemodule
      NCOMP:SESSION-MODULE-BEFORE NCOMP:SESSION-MODULE-AFTER
      beforegen beforeepoch beforelease
      IR-BUILD:HISTORY beforebuild
      NCOMP:SESSION-BUILD-BEFORE NCOMP:SESSION-BUILD-AFTER
      NFILE-OBJECT:ADD-RANGE
   compatible ;
: STOP-RANGE ( n n -- )
   {: end:n inputend:n :}
   F-STOPPED F@ 0<> if BAD then
   NUNIT-CAPTURE:FINISH-UNIT
   CWORK:LAYOUT-MARK F-LAYOUT-BEFORE F@ <> if 0 F-CACHEABLE F! then
   PRODUCED? if 1 F-DIRTY F! then
   F-DIRTY F@ 0<> F-STAMPED F@ 0= and if
      STAMP NFILE-INPUTS:MARK
   else inputend then {: upto:n :}
   F-PENDING F@ 0<> if
      F-PENDING-REQUIRES F@ NUNIT-CAPTURE:LIMIT-REQUIRES
   then
   F-RANGE-INPUT F@ upto NFILE-INPUTS:SPAN
      {: inputs:ptr inputu:n :}
   CHAIN-SIGNATURE$ {: signature:ptr signatureu:n :}
   UNIT end F-DIRTY F@ KEY$ inputs inputu signature signatureu
      F-SESSION-BEFORE F@ F-MODULE-BEFORE F@ F-GEN-BEFORE F@
      F-EPOCH-BEFORE F@ F-LEASE-BEFORE F@ F-BUILD-BEFORE F@ false
      STORE-RANGE {: compatible:bool :}
   compatible 0= if
      0 F-CACHEABLE F!
   then
   F-RANGES F@ end F-DIRTY F@ F-RANGE-INPUT F@ upto CHAIN-RANGE
   F-RANGES F@ 1+ F-RANGES F!
   NUNIT-OBJECT:CLOSE-OBJECT
   NUNIT-CAPTURE:LEAVE-FRAME
   1 F-STOPPED F! ;

: BEFORE ( n n n n -- n bool )
   {: frame:n op:n rec:n entry:n :}
   frame DEPTH @ 1- <> if BAD then
   op SOURCE-EXEC:WORD = if
      op rec entry SOURCE-UNIT:LOADER-KIND {: kind:n :}
      kind SOURCE-UNIT:NONE <> if
         SOURCE-EXEC:LOADER-CUT? 0= if BAD then
         kind F-PENDING F!
         NUNIT-CAPTURE:REQUIRE-MARK F-PENDING-REQUIRES F!
         STAMP
         NFILE-INPUTS:MARK F-LOADER-INPUT F!
         0 false exit
      then
   then
   op SOURCE-EXEC:SOURCE-END <> if 1 F-DIRTY F! then
   0 false ;

: PENDING-END ( -- n )
   SRCLOC:CURSOR @ NULL-PTR -
   data-base SRCLOC:INB-CELL + @ -
   {: end:n :}
   end 0 < end F-SOURCE-U F@ > or if BAD then end ;

: BEFORE-CHILD ( ptr u8 n ptr u8 n -- )
   {: child:ptr childu:n root:ptr rootu:n :}
   F-PENDING F@ 0= if
      SOURCE-UNIT:PARENT-ACTION$ {: mode:n path:ptr pathu:n :}
      path drop pathu drop
      mode SOURCE-UNIT:LOAD-INCLUDE <
      mode SOURCE-UNIT:LOAD-REQUIRE > or if BAD then
      F-READ F@ 0<> if BAD then
      NUNIT-CAPTURE:REQUIRE-MARK {: count:n :}
      mode SOURCE-UNIT:LOAD-REQUIRE = if
         count 0= if BAD then count 1-
      else count then F-PENDING-REQUIRES F!
      NFILE-INPUTS:MARK F-LOADER-INPUT F!
      PENDING-END F-DYNAMIC-END F!
      3 F-PENDING F!
   then
   F-PENDING F@ 3 <> if PENDING-END F-DYNAMIC-END F! then
   F-READ F@ 0= if
      F-DYNAMIC-END F@ F-LOADER-INPUT F@ STOP-RANGE
   then
   child childu root rootu CHAIN-LOAD ;

: AFTER ( n n n -- )
   {: frame:n ticket:n rc:n :}
   ticket drop
   frame DEPTH @ 1- <> if BAD then
   F-PENDING F@ 0= if exit then
   rc 0<> if exit then
   F-STOPPED F@ 0= if
      \ A required file already provided itself; there was no child load.
      1 F-DIRTY F!
      0 F-PENDING F!
      exit
   then
   SOURCE-EXEC:POSITION {: rel:n balanced:bool :}
   rel F-CURSOR F@ + {: end:n :}
   balanced drop
   SOURCE-EXEC:LOADER-CUT? 0= if BAD then
   0 F-PENDING F!
   START-RANGE ;

: COMPILE-FILE ( ptr u8 n -- )
   {: source:ptr sourceu:n :}
   0 F-READ F!
   0 F-CURSOR F!
   START-RANGE
   DEPTH @ 1- source sourceu
      [: BEFORE ;] [: AFTER ;]
      [: F-CURSOR F@ F-SOURCE-U F@ F-CONT F@ SOURCE-XT execute ;]
      SOURCE-EXEC:RUN {: rc:n :}
   rc 0<> if rc throw then
   F-SOURCE-U F@ NFILE-INPUTS:MARK STOP-RANGE
   F-INPUT F@ NFILE-INPUTS:BYTES-FROM {: inputs:ptr inputu:n :}
   F-CACHEABLE F@ 0<> if
      UNIT ARTIFACT$ inputs inputu NFILE-OBJECT:WRITE
   then
   1 COMPILE-N +! ;

\ A saved open session must have exactly the serial the source minted. Move
\ past skipped retired handles first, then let the ordinary compiler open it.
\ A close precedes final highwaters: closing after them would add an epoch
\ absent from the source range, and leave the wrong state for its sibling.
: REPLAY-SESSION ( n n n n n n n n -- )
   {: session:n before:n openlease:n openepoch:n
      openmodulebefore:n openmoduleafter:n
      openbuildbefore:n openbuildafter:n :}
   before NCOMP:SESSION-SERIAL <> if BAD then
   IR-CTX:SESSION-SERIAL before <> if BAD then
   session before = if
      openlease NCOMP:SESSION-LEASE <> if BAD then
      openepoch NCOMP:SESSION-EPOCH <> if BAD then
      openmodulebefore NCOMP:SESSION-MODULE-BEFORE <> if BAD then
      openmoduleafter NCOMP:SESSION-MODULE-AFTER <> if BAD then
      openbuildbefore NCOMP:SESSION-BUILD-BEFORE <> if BAD then
      openbuildafter NCOMP:SESSION-BUILD-AFTER <> if BAD then
      exit
   then
   session 0<> if
      session IR-CTX:SESSION-GEN-OF {: wantgen:n :}
      IR-CTX:HISTORY drop wantgen >= if BAD then
   then
   before 0<> if IR-CTX:SESSION-CLOSE then
   session 0<> if
      openlease 0= if BAD then
      NLEASE:HISTORY openlease >= if BAD then
      IR-CTX:EPOCH openepoch > if BAD then
      IR-ID:SERIAL-HISTORY openmodulebefore > if BAD then
      IR-BUILD:HISTORY openbuildbefore > if BAD then
      session IR-CTX:SESSION-GEN-OF 1- openepoch IR-CTX:REPLAY-HISTORY
      openlease 1- NLEASE:REPLAY-HISTORY
      openmodulebefore IR-ID:REPLAY-SERIAL
      openbuildbefore IR-BUILD:REPLAY-HISTORY
      NCOMP:SESSION-ENSURE
      session NCOMP:SESSION-SERIAL <> if BAD then
      openlease NCOMP:SESSION-LEASE <> if BAD then
      openepoch NCOMP:SESSION-EPOCH <> if BAD then
      openmodulebefore NCOMP:SESSION-MODULE-BEFORE <> if BAD then
      openmoduleafter NCOMP:SESSION-MODULE-AFTER <> if BAD then
      openbuildbefore NCOMP:SESSION-BUILD-BEFORE <> if BAD then
      openbuildafter NCOMP:SESSION-BUILD-AFTER <> if BAD then
   then ;

: RESTORE-RANGE ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: bytes:ptr size:n key:ptr keyu:n expected:ptr prepared:n :}
   bytes size NFILE-OBJECT:SPLIT-RANGE
      {: unitbytes:ptr unitu:n signature:ptr scope:ptr
         session:n module:n gen:n epoch:n before:n lease:n openlease:n
         openepoch:n beforemodule:n openmodulebefore:n openmoduleafter:n
         beforegen:n beforeepoch:n beforelease:n
         build:n beforebuild:n openbuildbefore:n openbuildafter:n :}
   unitbytes drop unitu drop key drop keyu drop
   expected 32 signature 32 STR= 0= if BAD then
   module IR-ID:SERIAL-CHECK drop
   gen epoch IR-CTX:CHECK-HISTORY
   lease NLEASE:CHECK-HISTORY
   beforemodule IR-ID:SERIAL-HISTORY <> if BAD then
   IR-CTX:HISTORY {: livegen:n liveepoch:n :}
   livegen beforegen <> liveepoch beforeepoch <> or if BAD then
   NLEASE:HISTORY beforelease <> if BAD then
   IR-BUILD:HISTORY beforebuild <> if BAD then
   session before openlease openepoch openmodulebefore openmoduleafter
      openbuildbefore openbuildafter REPLAY-SESSION
   module IR-ID:REPLAY-SERIAL
   gen epoch IR-CTX:REPLAY-HISTORY
   lease NLEASE:REPLAY-HISTORY
   build IR-BUILD:REPLAY-HISTORY
   IMPORTER {: importer :}
   importer prepared true NUNIT-OBJECT:INSTALL-PREPARED 2drop
   prepared NUNIT-OBJECT:PREPARED-CHECKER-ID CHECKER-INSTALL
   scope SCOPE-INSTALL
   NUNIT-OBJECT:PREPARED-INPUTS$ NFILE-INPUTS:APPEND
   NUNIT-OBJECT:CLOSE-OBJECT ;

: REPLAY-RANGE ( n n n ptr u8 n -- )
   {: h:n end:n dirty:n bytes:ptr size:n :}
   NFILE-INPUTS:MARK {: first:n :}
   CHAIN-SIGNATURE$ drop {: expected:ptr :}
   bytes size KEY$ expected h NFILE-OBJECT:SELECTED-PREPARED RESTORE-RANGE
   F-RANGES F@ end dirty first NFILE-INPUTS:MARK CHAIN-RANGE
   F-RANGES F@ 1+ F-RANGES F!
   end F-CURSOR F! ;

: REPLAY-BODY ( n -- ) {: h:n :}
   h UNIT <> if BAD then
   1 F-READ F!
   begin
      UNIT NFILE-OBJECT:NEXT
      {: kind:n end:n mode:n bytes:ptr size:n more:bool :}
      more
   while
      kind NFILE-OBJECT:RANGE-KIND = if
         h end mode bytes size REPLAY-RANGE
      else
         kind NFILE-OBJECT:LOAD-KIND <> if BAD then
         bytes size NFILE-OBJECT:LOAD-DETAIL
            {: path:ptr pathu:n root:ptr rootu:n key:ptr keyu:n
               sourceu:n stampu:n sec:n ns:n :}
         key drop keyu drop sourceu drop
         path pathu root rootu CHAIN-LOAD
         mode SOURCE-UNIT:LOAD-REQUIRE = if
            path pathu REQUIRE-REG:APPEND
         then
         h NFILE-OBJECT:SELECTED-CHILD
         PUSH
         dup F-UNIT F!
         dup NFILE-OBJECT:KEY-OF$ drop KEY$ drop 64 BYTE-COPY
         1 F-READ F!
         0 F-CURSOR F!
         recurse
         POP
         path pathu root rootu stampu sec ns NFILE-INPUTS:ADD-STAMP
         end F-CURSOR F!
      then
   repeat
   1 HIT-N +! ;

: LOAD-FILE ( ptr u8 n ptr u8 n ptr u8 n [ n n -- ] -- )
   {: path:ptr pathu:n root:ptr rootu:n source:ptr sourceu:n q :}
   DEPTH @ 0<> if path pathu root rootu BEFORE-CHILD then
   PUSH
   sourceu F-SOURCE-U F!
   q CONT-N F-CONT F!
   0 F-CURSOR F!
   READ-SIZE @ F-META-SIZE F!
   READ-SEC @ F-META-SEC F!
   READ-NS @ F-META-NS F!
   path pathu root rootu
      F-META-SIZE F@ F-META-SEC F@ F-META-NS F@
      NFILE-INPUTS:ADD-STAMP
   path pathu root rootu FILE-KEY
   ARTIFACT-PATH
   ARTIFACT$ path pathu root rootu KEY$ sourceu
      NFILE-OBJECT:TRY-OPEN {: h:n hit:bool :}
   HASH-CTX SELECT-START SHA256-CTX-BYTES BYTE-COPY
   SELECT-CURRENT
   hit SELECT-REGISTRY-READY? and
      IR-CTX:SESSION-SERIAL SELECT-SERIAL @ = and
      if CHECKER-BEGIN h SELECT-TREE-BODY else false then
      {: selected:bool :}
   SELECT-START HASH-CTX SHA256-CTX-BYTES BYTE-COPY
   selected if
      h F-UNIT F! h REPLAY-BODY
      \ A compiled parent writes before BUILD ends. Include the selected
      \ subtree's queries now, while its own input mark is still active.
      h NFILE-OBJECT:INPUTS$ NFILE-INPUTS:APPEND
      NUNIT-OBJECT:DISCARD-PREPARED
      CHECKER-END
   else
      NUNIT-OBJECT:DISCARD-PREPARED
      CHECKER-END
      path pathu root rootu KEY$ sourceu
         NFILE-OBJECT:START F-UNIT F!
      source sourceu COMPILE-FILE
   then
   F-META-SIZE F@ F-META-SEC F@ F-META-NS F@
      {: size:n sec:n ns:n :}
   DEPTH @ 1 > if
      SOURCE-UNIT:PARENT-ACTION$ {: mode:n action:ptr actionu:n :}
      action drop actionu drop
      DEPTH @ 2 - FRAME {: parent:ptr :}
      parent F-UNIT + CELL-VIEW @
      parent F-DYNAMIC-END + CELL-VIEW @
      mode UNIT size sec ns NFILE-OBJECT:ADD-LOAD
   then
   POP
   DEPTH @ 0<> if
      path pathu root rootu size sec ns NFILE-INPUTS:ADD-STAMP
      F-PENDING F@ 3 = if
         0 F-PENDING F!
         START-RANGE
      then
   then ;

: CANON ( ptr u8 n -- ptr u8 n bool )
   {: path:ptr pathu:n :}
   path pathu SOURCE-ROOT:CANON-OS {: answer:ptr answeru:n exists:bool :}
   path pathu answer answeru exists NFILE-INPUTS:ADD-CANON
   answer answeru exists ;
: READ ( ptr u8 n ptr u8 n -- ptr u8 n )
   {: path:ptr pathu:n root:ptr rootu:n :}
   path pathu FILE? 0= if
      \ Let the source loader name an unopenable path before metadata can throw.
      path pathu root rootu SOURCE-ROOT:READ-OS 2drop
   then
   path pathu FILE-META {: size:n sec:n ns:n csec:n cns:n :}
   csec drop cns drop
   size READ-SIZE ! sec READ-SEC ! ns READ-NS !
   path pathu root rootu SOURCE-ROOT:READ-OS ;
: FILE-INPUT ( ptr u8 n -- ) s" " NFILE-INPUTS:ADD-MTIME ;
: PRINT-COUNTS ( -- )
   s" hb-build: file unit hits " type HIT-N @ .
   s"  compiled " type COMPILE-N @ . cr ;
variable REPORT-COUNTS
: CLEAR-SCRATCH ( -- )
   \ These adjacent declarations hold only the current FILE build's hashes,
   \ cache paths and saved scope. A finished build has no use for their bytes.
   SCOPE-BUF 22 cells + HASH-CTX - 0 ?do
      0 HASH-CTX i + c!
   loop ;
: CLOSE ( -- )
   BUILD-INPUT:RESET
   CHECKER-END
   NULL-PTR APP-PATH ! 0 APP-PATH-U !
   SOURCE-UNIT:RESET SOURCE-INPUT:RESET
   NFILE-OBJECT:CLOSE NUNIT-OBJECT:CLOSE
   NULL-PTR INPUTS-PATH ! 0 INPUTS-PATH-U !
   FRAMES-RELEASE 0 DEPTH !
   CLEAR-SCRATCH
   \ BUILD printed the counts before finally called CLOSE. These scalars
   \ describe only this FILE build, including the last file READ saw, which
   \ differs between cold child compilation and warm parent replay.
   0 FRAME-BYTES !
   0 HIT-N ! 0 COMPILE-N !
   0 SELECT-SERIAL ! 0 SELECT-OPEN-LEASE !
   0 SELECT-GEN ! 0 SELECT-EPOCH ! 0 SELECT-LEASE !
   0 SELECT-MODULE ! 0 SELECT-OPEN-EPOCH !
   0 SELECT-OPEN-MODULE-BEFORE ! 0 SELECT-OPEN-MODULE-AFTER !
   0 SELECT-BUILD ! 0 SELECT-OPEN-BUILD-BEFORE ! 0 SELECT-OPEN-BUILD-AFTER !
   0 READ-SIZE ! 0 READ-SEC ! 0 READ-NS ! ;
: RUN-APP ( -- ) APP-PATH @ APP-PATH-U @ script-required ;
: BUILD ( -- )
   NFILE-INPUTS:RESET
   [: FILE-INPUT ;] BUILD-INPUT:USE
   0 HIT-N ! 0 COMPILE-N ! 0 DEPTH !
   ARTIFACT-OFF FS-PATH-CAP + FRAME-BYTES !
   FILE-ROOT$ MAKE-DIRS
   NUNIT-CAPTURE:START-FILE-PREFIX
   [: CANON ;] [: READ ;] SOURCE-INPUT:USE
   [: LOAD-FILE ;] SOURCE-UNIT:USE
   ['] RUN-APP NUNIT-CAPTURE:SESSION-WITH
   INPUTS-PATH @ NULL-PTR <> if
      NFILE-INPUTS:BYTES$ {: bytes:ptr u:n :}
      u 0= if BAD then
      INPUTS-PATH @ INPUTS-PATH-U @ bytes u ATOMIC-WRITE-FILE
   then
   REPORT-COUNTS @ if PRINT-COUNTS then ;

: LOAD-WITH-INPUTS-RUN ( ptr u8 n ptr u8 n ptr u8 n bool -- )
   {: path:ptr pathu:n mode:ptr modeu:n manifest:ptr manifestu:n report:bool :}
   report REPORT-COUNTS !
   path APP-PATH ! pathu APP-PATH-U !
   manifest INPUTS-PATH ! manifestu INPUTS-PATH-U !
   mode modeu CHAIN-BEGIN
   ['] BUILD ['] CLOSE finally ;

public

: CAPTURE-PRODUCER ( -- )
   SOURCE-PRODUCER:HASH$ {: a:ptr u:n :}
   u 32 <> if BAD then
   a SAVED-PRODUCER u BYTE-COPY
   1 SAVED-PRODUCER? ! ;

: LOAD ( ptr u8 n ptr u8 n -- )
   {: path:ptr pathu:n mode:ptr modeu:n :}
   true REPORT-COUNTS !
   path APP-PATH ! pathu APP-PATH-U !
   NULL-PTR INPUTS-PATH ! 0 INPUTS-PATH-U !
   mode modeu CHAIN-BEGIN
   ['] BUILD ['] CLOSE finally ;

: LOAD-WITH-INPUTS ( ptr u8 n ptr u8 n ptr u8 n -- )
   true LOAD-WITH-INPUTS-RUN ;

: LOAD-WITH-INPUTS-QUIET ( ptr u8 n ptr u8 n ptr u8 n -- )
   false LOAD-WITH-INPUTS-RUN ;

\ The explicit command stores one complete selected source as an HFO range.
\ Its native range includes nested loads, so no dependent cache files are
\ needed to move or import the named artifact.
: EXPLICIT-START ( -- )
   NLEASE:IDLE? 0= if BAD then
   IR-CTX:REGISTRY-IDLE? IR-CTX:REGISTRY-SESSION-ONLY? or
      0= if BAD then
   IR-CTX:SESSION-SERIAL NCOMP:SESSION-SERIAL <> if BAD then
   MARK-CHECKER
   SELECT-CURRENT
   SELECT-SERIAL @ EX-SESSION-BEFORE !
   SELECT-MODULE @ EX-MODULE-BEFORE !
   SELECT-GEN @ EX-GEN-BEFORE !
   SELECT-EPOCH @ EX-EPOCH-BEFORE !
   SELECT-LEASE @ EX-LEASE-BEFORE !
   SELECT-BUILD @ EX-BUILD-BEFORE !
   CWORK:LAYOUT-MARK EX-LAYOUT-BEFORE ! ;

: EXPLICIT-HISTORY? ( -- bool )
   NCOMP:SESSION-SERIAL {: session:n :}
   IR-ID:SERIAL-HISTORY {: module:n :}
   IR-CTX:HISTORY {: gen:n epoch:n :}
   NLEASE:HISTORY {: lease:n :}
   session module gen epoch EX-SESSION-BEFORE @ lease
      NCOMP:SESSION-LEASE NCOMP:SESSION-EPOCH EX-MODULE-BEFORE @
      NCOMP:SESSION-MODULE-BEFORE NCOMP:SESSION-MODULE-AFTER
      EX-GEN-BEFORE @ EX-EPOCH-BEFORE @ EX-LEASE-BEFORE @
      IR-BUILD:HISTORY EX-BUILD-BEFORE @
      NCOMP:SESSION-BUILD-BEFORE NCOMP:SESSION-BUILD-AFTER
      SELECT-HISTORY ;

: EXPLICIT-EXPORT ( ptr u8 n ptr u8 n ptr u8 n ptr u8 ptr u8 n n ptr u8 n -- )
   {: path:ptr pathu:n root:ptr rootu:n key:ptr keyu:n
      signature:ptr artifact:ptr artifactu:n sourceu:n
      inputs:ptr inputu:n :}
   CWORK:LAYOUT-MARK EX-LAYOUT-BEFORE @ <> if BAD then
   path pathu root rootu key keyu sourceu NFILE-OBJECT:START {: h:n :}
   h sourceu 1 key keyu inputs inputu signature 32
      EX-SESSION-BEFORE @ EX-MODULE-BEFORE @ EX-GEN-BEFORE @
      EX-EPOCH-BEFORE @ EX-LEASE-BEFORE @ EX-BUILD-BEFORE @ true
      STORE-RANGE {: compatible:bool :}
   compatible 0= if BAD then
   EXPLICIT-HISTORY? 0= if BAD then
   h artifact artifactu inputs inputu NFILE-OBJECT:WRITE ;

: EXPLICIT-IMPORT ( ptr u8 n ptr u8 n ptr u8 n ptr u8 ptr u8 n n -- )
   {: path:ptr pathu:n root:ptr rootu:n key:ptr keyu:n
      signature:ptr artifact:ptr artifactu:n sourceu:n :}
   artifact artifactu path pathu root rootu key keyu sourceu
      NFILE-OBJECT:TRY-OPEN {: h:n hit:bool :}
   hit 0= if BAD then
   h NFILE-OBJECT:NEXT
      {: kind:n end:n dirty:n bytes:ptr size:n more:bool :}
   more 0= kind NFILE-OBJECT:RANGE-KIND <> or
      end sourceu <> or if BAD then
   h NFILE-OBJECT:NEXT
      {: extra-kind:n extra-end:n extra-mode:n extra:ptr extrau:n extra?:bool :}
   extra-kind drop extra-end drop extra-mode drop extra drop extrau drop
   extra? if BAD then
   bytes size signature SELECT-RANGE-HISTORY 0= if BAD then
   CHECKER-BEGIN
   bytes 8 + bytes LE:U64@ key keyu s" FILE"
      NUNIT-OBJECT:PREPARE-BYTES-NAMED {: prepared:n :}
   prepared NUNIT-OBJECT:PREPARED-CHECKER-OF$
      CHECKER-PREPARE prepared swap
      NUNIT-OBJECT:PREPARED-CHECKER-ID!
   bytes 8 + bytes LE:U64@ + 32 +
      NUNIT-OBJECT:PROJECTED-COUNTS SCOPE-PREPARE
   bytes size key keyu signature prepared RESTORE-RANGE
   NUNIT-OBJECT:DISCARD-PREPARED
   CHECKER-END
   dirty drop ;

: EXPLICIT-CLOSE ( -- )
   CHECKER-END
   NFILE-OBJECT:CLOSE NUNIT-OBJECT:CLOSE
   0 EX-SESSION-BEFORE ! 0 EX-MODULE-BEFORE !
   0 EX-GEN-BEFORE ! 0 EX-EPOCH-BEFORE ! 0 EX-LEASE-BEFORE !
   0 EX-BUILD-BEFORE ! 0 EX-LAYOUT-BEFORE !
   0 SELECT-SERIAL ! 0 SELECT-MODULE !
   0 SELECT-GEN ! 0 SELECT-EPOCH ! 0 SELECT-LEASE !
   0 SELECT-OPEN-LEASE ! 0 SELECT-OPEN-EPOCH !
   0 SELECT-OPEN-MODULE-BEFORE ! 0 SELECT-OPEN-MODULE-AFTER !
   0 SELECT-BUILD ! 0 SELECT-OPEN-BUILD-BEFORE !
   0 SELECT-OPEN-BUILD-AFTER ! ;

;package
