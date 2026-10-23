\ Automatic native definition reuse. The ordinary source loader always runs;
\ the pending native dispatch selects one definition after its source actions.
require src/lib/errors.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/build-input.f
require src/lib/build-cache.f
require src/lib/engine-id.f
require src/lib/le.f
require src/lib/memory.f
require src/os/env-base.f
require src/core/sha256.f
require src/core/source-producer.f
require src/core/include.f
require src/core/checker-owner-abi.f
require src/core/checker-owner-extension.f
require src/habu/aot-arm.f
require src/habu/layout.f
require src/habu/source-exec.f
require src/habu/terminal-call.f
require src/habu/xref.f
require src/habu/native-body-abi.f
require src/compiler/native/compiler.f
require tools/build-target.f
require tools/build-producer.f
require tools/native-file-inputs.f
require tools/native-file-reference.f
require tools/native-unit-dependency.f
require tools/native-unit-origin.f
require tools/native-unit-object.f
require tools/native-unit-file.f

package NDEF-CACHE
private

$4B8 constant LIVE-FIXED-OFF
0 constant F-SRC
8 constant F-SRC-U
16 constant F-ROOT
24 constant F-ROOT-U
32 constant F-Q
40 constant F-INPUT
48 constant F-INPUT-U
56 constant F-PRIOR
\ The load kind that opened the file.
64 constant F-KIND
\ The input rows below F-MARK belong to a file object already.
72 constant F-MARK
\ The bytes as read. F-SRC is the copy the interpreter runs, which differs
\ from them when the file starts with `#!` (include.f LOAD-BYTES).
80 constant F-RAW
88 constant F-RAW-U
\ Where the loading file's source stood when it loaded this one; -1 for
\ the outermost file or a load from outside that source.
96 constant F-AT
\ TASK's user-slot watermark when the file opened, and 1 when TASK was bound
\ to read it.
104 constant F-SLOTS
112 constant F-SLOTS-BOUND
\ The file's object key, taken when it opened: 64 hex digits.
120 constant F-KEY
\ The origin sink the file opened under (NUNIT-OBJECT SINK-*), 1 when the
\ builder fed its rows to the early journal, the builder's prefix row count
\ and the early journal's serial when it opened, and its first record.
184 constant F-OSINK
192 constant F-OFED
200 constant F-OCUR
208 constant F-OSER
216 constant F-OFIRST
224 constant FRAME-BYTES
\ A file object name: "hb-file-", 64 hex digits, ".hfo".
76 constant OBJECT-NAME-BYTES

\ An object entry: magic | checksum over the bytes after it | the HBNUNIT2
\ file object.
16 constant ENTRY-HEAD
$cbf29ce484222325 constant FNV-BASIS
$100000001b3 constant FNV-PRIME

\ One FRAME-BYTES record per open file depth.
DYNAMIC-BUFFER FRAMES u8
DYNAMIC-BUFFER PENDING-KEY u8
\ One entry read and one image written at a time: DISPATCH-BUSY serialises
\ every lookup and store.
DYNAMIC-BUFFER ENTRY u8
DYNAMIC-BUFFER STORE-IMAGE u8
variable DEPTH
variable SESSION
variable SEEDED
\ The dictionary count when the process's first session opened. The records
\ below it are the seed's; CLOSE leaves it, so a later session's files cannot
\ name an earlier session's records as the seed's. It is a process cell, as
\ NUNIT-OBJECT's image floor is: an image stores it as 0, so the process
\ booted from it takes its own at its first session.
variable BOOT-N
BOOT-N NUNIT-OBJECT:PROCESS-CELL
variable NATIVE?
variable REPORT?
\ Files installed from their objects in place of being run this session.
variable REUSED-N
variable COMPILE-N
variable DISPATCH-BUSY
variable DISPATCH-ORIGINAL
variable BORROWED-ORIGINAL
variable BORROW-ACTIVE
variable BORROW-ACCEPT
variable DISPATCH-ROLE-KNOWN
create DISPATCH-ROLE 32 allot
variable CHECKER-ROLE-KNOWN
create CHECKER-ROLE 32 allot
PTR-VARIABLE PUBLICATION-OWNER
variable PUBLICATION-CLEAR-XT
variable TFAM-RESET-XT
variable CON-RESET-XT
variable OPAQUE-XT
PTR-VARIABLE ORIGIN-RECORD
variable ORIGIN-GETTER
PTR-VARIABLE CUT-CELL
\ Each XREF:BEFORE-RETIRE! bound to the file objects' retire hook, the running
\ kernel's and each XREF a build loads after it, with its record ordinal and
\ whether that record was sealed when bound. Every bound setter is sealed or
\ has a live record, hence live code: a cut notice (RETIRE-CUT) drops each
\ unsealed setter whose record it removes before its code goes
\ (src/habu/xref.f TRUNCATE), and a sealed setter's code is never truncated:
\ the one cut below the seal, seed-ndict!, removes records and keeps code
\ (src/habu/prefix-rewind.f), SEAL-DICT-GUARD dies on a FORGET or HIDE below
\ the seal, and every rollback returns to a mark taken after it. Without the
\ kernel's cut cell no setter is bound. Cells past RETIRE-N hold 0: a
\ setter is a session's address, and an image saved after the session would
\ keep it and differ from run to run.
4 constant RETIRE-CAP
RETIRE-CAP TYPED-BUFFER RETIRE-XTS n
RETIRE-CAP TYPED-BUFFER RETIRE-ORDS n
RETIRE-CAP TYPED-BUFFER RETIRE-SEALED bool
variable RETIRE-N
variable UNOBSERVE-XT
variable TARGET-UNIT-USE
variable TARGET-INPUT-RESET
variable TARGET-ENV-RESET
variable TARGET-SOURCE-BOUND
variable HOST-INPUT-RESET
variable HOST-SOURCE-BOUND
variable HOST-SOURCE-READY
variable HOST-UNIT-USE
variable HOST-INPUT-USE
variable HOST-ENV-USE
variable HOST-ENV-RESET
create HASH-CTX SHA256-CTX-BYTES allot
create HASH-RAW 32 allot
create KEY-HEX 64 allot
create HASH-CELL CELL allot
create OBJECT-NAME OBJECT-NAME-BYTES allot
create FILE-KEY-HEX 64 allot
create RESULTS-HEX 64 allot
create PATH-BUF FS-PATH-CAP allot

CAST: DISPATCH-XT ( n -- [ ptr u8 n -- ] )
CAST: DISPATCH>N ( [ ptr u8 n -- ] -- n )
CAST: BORROW-ACCEPT-XT ( n -- [ ptr u8 n -- ] )
CAST: ACTION-XT ( n -- [ -- ] )
CAST: ORIGIN-STATE-XT ( n -- [ -- bool bool ] )
CAST: ROLE-XT ( n -- [ -- ptr u8 n bool ] )
CAST: ROLE-ID-XT ( n -- [ n -- ptr u8 n bool ] )
CAST: OPAQUE-XT-TYPE ( n -- [ -- bool ] )
CAST: ORIGIN-CELL-XT ( n -- [ -- ptr u8 ] )
CAST: CUT-CELL-XT ( n -- [ -- ptr u8 ] )
CAST: USER-NEXT-XT ( n -- [ [ ptr u8 -- ] -- ] )
CAST: CUT-SLOT ( ptr u8 -- ptr [ n -- ] )
CAST: RETIRE-SET-XT ( n -- [ n -- ] )
CAST: RETIRE>N ( [ ptr n -- ] -- n )
CAST: OBSERVE-XT ( n -- [ [ ptr n -- ] -- ] )
CAST: ORIGIN-TAKE-XT ( n -- [ ptr u8 n -- ] )
CAST: COMPLETION-CELL ( ptr u8 -- ptr [ ptr u8 n -- ] )
CAST: UNIT-CURRENT-XT
   ( n -- [ -- [ ptr u8 n ptr u8 n ptr u8 n [ n n -- ] -- ] ] )
CAST: UNIT-USE-XT
   ( n -- [ [ ptr u8 n ptr u8 n ptr u8 n [ n n -- ] -- ] -- ] )
CAST: INPUT-USE-XT
   ( n -- [ [ ptr u8 n -- ptr u8 n bool ] [ ptr u8 n ptr u8 n -- ptr u8 n ] -- ] )
CAST: ENV-USE-XT ( n -- [ [ ptr u8 n ptr u8 n -- ] -- ] )
CAST: SOURCE-READ-XT ( n -- [ ptr u8 n ptr u8 n -- ptr u8 n ] )
CAST: PUB-USE-XT ( n -- [ [ n -- ] -- ] )
CAST: TFAM-REF-USE-XT
   ( n -- [ [ n -- ptr u8 n bool ] [ ptr u8 n -- n bool ] [ ptr u8 n ptr u8 n ptr u8 n -- bool ] [ n -- ] [ n -- ptr u8 n bool ] [ ptr u8 n -- n bool ] [ n ptr u8 n -- ] -- ] )
CAST: CON-USE-XT
   ( n -- [ [ n -- ptr u8 n bool ] [ n ptr u8 n -- ] -- ] )
CAST: FIXED-READ-XT ( n -- [ ptr n -- n n bool ] )
CAST: CONTRACT-XT ( n -- [ n -- ptr u8 n bool ] )
CAST: PRIMITIVE-XT ( n -- [ n n -- n bool ] )
CAST: IDENTITY-XT ( n -- [ n -- ptr u8 n ptr u8 n n ] )
CAST: ORIGIN-XT ( n -- [ ptr u8 n n ptr u8 n -- n ] )
CAST: PARENT-ACTION-XT ( n -- [ -- n ptr u8 n ] )
CAST: ACTIVE-SOURCE-XT ( n -- [ -- ptr u8 n ptr u8 n ptr u8 n ] )
CAST: UNIT>N
   ( [ ptr u8 n ptr u8 n ptr u8 n [ n n -- ] -- ] -- n )
CAST: N>UNIT
   ( n -- [ ptr u8 n ptr u8 n ptr u8 n [ n n -- ] -- ] )
CAST: Q>N ( [ n n -- ] -- n )
CAST: N>Q ( n -- [ n n -- ] )

TYPED-VARIABLE PRIOR-UNIT [ ptr u8 n ptr u8 n ptr u8 n [ n n -- ] -- ]
TYPED-VARIABLE HOST-PRIOR-UNIT [ ptr u8 n ptr u8 n ptr u8 n [ n n -- ] -- ]
TYPED-VARIABLE OUTER-BODY [ -- ]
\ The builder's prefix rows while it loads the checked prefix (PRE-BOUND 1):
\ its row cursor and whether rows stay its own, its row visitor, its append.
TYPED-VARIABLE PRE-MARK-HOOK [ -- n bool ]
TYPED-VARIABLE PRE-EACH-HOOK [ n [ n n n ptr u8 n ptr u8 n -- ] -- ]
TYPED-VARIABLE PRE-IMPORT-HOOK [ n n n ptr u8 n ptr u8 n -- ]
variable PRE-BOUND
\ The closing file's journal rows read so far, and 1 when its rows were fed.
variable HARVEST-N
variable HARVEST-FED
CAST: MARK-XT ( n -- [ -- n n bool ] )
CAST: HOOK-XT ( n -- [ -- bool ] )
CAST: ROOT-XT ( n -- [ -- ptr u8 n ] )
CAST: ROWS-XT ( n -- [ n [ n n n n ptr u8 n ptr u8 n n n -- ] -- ] )
CAST: EARLY-IMPORT-XT ( n -- [ n n n n ptr u8 n ptr u8 n -- ] )
CAST: FULL-IMPORT-XT ( n -- [ n n ptr u8 n ptr u8 n n n -- ] )
\ NFILE-REF:PACKAGE-OF visibilities and the checker's.
1 constant PKG-PUBLIC
2 constant PKG-PRIVATE
3 constant PKG-RETIRED
1 constant SYM-PRIVATE
2 constant SYM-PUBLIC
PTR-VARIABLE APP-PATH
variable APP-PATH-U
PTR-VARIABLE REQUEST-PATH
variable REQUEST-PATH-U
PTR-VARIABLE REQUEST-MANIFEST
variable REQUEST-MANIFEST-U
PTR-VARIABLE DISPATCH-SOURCE
variable DISPATCH-SOURCE-U
PTR-VARIABLE STAGED-REASON
variable STAGED-REASON-U
variable PENDING-KEY-U

: ORDINARY-UNIT ( ptr u8 n ptr u8 n ptr u8 n [ n n -- ] -- )
   {: path:ptr pathu:n root:ptr rootu:n source:ptr sourceu:n q :}
   0 sourceu q execute ;

: BAD ( -- ) E-NUNIT-PROFILE throw ;

: OWNER ( -- ptr u8 )
   data-base NCOMP-DISPATCH:DECL-CELL + 0 ptr-field @ ;

: TARGET-XT? ( ptr u8 n -- n )
   XREF-FIND dup XREF-FOUND? 0= if drop 0 exit then
   dup XREF-RETIRED? if drop 0 exit then
   XREF-START dup cp@ >= if drop 0 then ;

: TARGET-XT ( ptr u8 n -- n )
   TARGET-XT? dup 0= if BAD then ;

: ENV-MISSING ( -- )
   s" native-cache: engine lacks ENV-INPUT hooks" 74 die ;

: ENV-XT ( ptr u8 n -- n )
   TARGET-XT? dup 0= if drop ENV-MISSING then ;

: OWNER-STRUCTURAL? ( -- bool )
   OWNER {: owner:ptr :}
   owner NULL-PTR = if false exit then
   owner CHECKER-OWNER-ABI:HEADER-BYTES - CELL-VIEW @
      CHECKER-OWNER-ABI:MAGIC <> if false exit then
   owner CELL - CELL-VIEW @ CHECKER-OWNER-EXT:OWNER-BYTES < if false exit then
   owner CHECKER-OWNER-EXT:RECORD-CONTRACT-OFF + CELL-VIEW @ 0<>
   owner CHECKER-OWNER-EXT:PRIMITIVE-STATE-OFF + CELL-VIEW @ 0<> and
   owner CHECKER-OWNER-ABI:VERIFY-SYM-IDENTITY-OFF + CELL-VIEW @ 0<> and
   owner CHECKER-OWNER-ABI:ORIGIN-SYM-OFF + CELL-VIEW @ 0<> and
   owner LIVE-FIXED-OFF + CELL-VIEW @ 0<> and ;

: OWNER-READY? ( -- bool )
   OWNER-STRUCTURAL?
   s" CHECKER-REG:PUBLICATION-USE" TARGET-XT? 0<> and
   s" CHECKER-REG:PUBLICATION-CLEAR" TARGET-XT? 0<> and ;

: BORROW-OWNER-READY? ( -- bool )
   OWNER-STRUCTURAL? 0= if false exit then
   PUBLICATION-OWNER @ OWNER = ;

: LIVE-CONTRACT ( n -- ptr u8 n bool )
   OWNER CHECKER-OWNER-EXT:RECORD-CONTRACT-OFF + CELL-VIEW @
      CONTRACT-XT execute ;
: LIVE-PRIMITIVE ( n n -- n bool )
   OWNER CHECKER-OWNER-EXT:PRIMITIVE-STATE-OFF + CELL-VIEW @
      PRIMITIVE-XT execute ;
: LIVE-IDENTITY ( n -- ptr u8 n ptr u8 n n )
   OWNER CHECKER-OWNER-ABI:VERIFY-SYM-IDENTITY-OFF + CELL-VIEW @
      IDENTITY-XT execute ;
: LIVE-ORIGIN ( ptr u8 n n ptr u8 n -- n )
   OWNER CHECKER-OWNER-ABI:ORIGIN-SYM-OFF + CELL-VIEW @
      ORIGIN-XT execute ;
: LIVE-FIXED ( n -- n n bool )
   XREF-REC
   OWNER LIVE-FIXED-OFF + CELL-VIEW @
      FIXED-READ-XT execute ;

: ACTUAL-COMPILER$ ( -- ptr u8 n bool )
   s" SOURCE-PRODUCER:ACTIVE-COMPILER$" TARGET-XT?
      dup 0= if drop NULL-PTR 0 false exit then
      ROLE-XT execute ;

: PIN-COMPILER ( -- )
   ACTUAL-COMPILER$ {: role:ptr roleu:n known:bool :}
   known roleu 32 = and if
      role DISPATCH-ROLE 32 BYTE-COPY
      1 DISPATCH-ROLE-KNOWN !
   else 0 DISPATCH-ROLE-KNOWN ! then ;

: COMPILER-ROLE$ ( -- ptr u8 n bool )
   DISPATCH-ROLE DISPATCH-ROLE-KNOWN @ if 32 else 0 then
      DISPATCH-ROLE-KNOWN @ 0<> ;

: CHECKER-ROLE$ ( -- ptr u8 n bool )
   CHECKER-ROLE CHECKER-ROLE-KNOWN @ if 32 else 0 then
      CHECKER-ROLE-KNOWN @ 0<> ;

: PIN-CHECKER ( ptr u8 n bool -- )
   {: digest:ptr size:n known:bool :}
   known size 32 = and if
      digest CHECKER-ROLE 32 BYTE-COPY
      1 CHECKER-ROLE-KNOWN !
   else 0 CHECKER-ROLE-KNOWN ! then
   ['] CHECKER-ROLE$ NUNIT-DEP:USE-CHECKER-ROLE ;

: PIN-HOST-CHECKER ( -- )
   SOURCE-PRODUCER:CHECKER SOURCE-PRODUCER:ROLE$ PIN-CHECKER ;

: PIN-TARGET-CHECKER ( -- )
   s" SOURCE-PRODUCER:ROLE$" TARGET-XT?
      dup 0= if drop exit then
   SOURCE-PRODUCER:CHECKER swap ROLE-ID-XT execute
      {: digest:ptr size:n known:bool :}
   known if
      size 32 <> if BAD then
      digest size known PIN-CHECKER
   then ;

: PUB-USE ( [ n -- ] -- )
   OWNER-READY? 0= if drop exit then
   s" CHECKER-REG:PUBLICATION-USE" TARGET-XT?
      dup 0= if drop drop exit then
      PUB-USE-XT execute
   s" CHECKER-REG:PUBLICATION-CLEAR" TARGET-XT PUBLICATION-CLEAR-XT !
   OWNER PUBLICATION-OWNER ! ;

: BIND-OWNER-FACTS ( -- )
   ['] LIVE-CONTRACT NFILE-REF:USE-LIVE-CONTRACT
   ['] NFILE-REF:CONTRACT$ NUNIT-DEP:USE-CONTRACT
   ['] LIVE-PRIMITIVE NUNIT-DEP:USE-PRIMITIVE-STATE
   ['] LIVE-IDENTITY NUNIT-DEP:USE-SYM-IDENTITY
   ['] LIVE-ORIGIN NUNIT-DEP:USE-ORIGIN-SYM
   ['] LIVE-FIXED NFILE-REF:USE-FIXED-READ ;

: BIND-OWNER ( -- )
   OWNER-READY? 0= if exit then
   BIND-OWNER-FACTS
   ['] NFILE-REF:ON-PUBLISHED PUB-USE ;

: CLEAR-PUBLICATION ( -- )
   PUBLICATION-OWNER @ NULL-PTR = if exit then
   PUBLICATION-CLEAR-XT @ ACTION-XT execute
   NULL-PTR PUBLICATION-OWNER ! 0 PUBLICATION-CLEAR-XT ! ;

: BIND-NOMINALS ( -- )
   OWNER-READY? 0= if exit then
   s" TFAM:FILE-REF-RESET" TARGET-XT TFAM-RESET-XT !
   s" CHECKER-REG:FILE-CON-RESET" TARGET-XT CON-RESET-XT !
   s" TFAM:FILE-IDENTITY$" TARGET-XT
   s" TFAM:FILE-FIND" TARGET-XT NFILE-REF:BIND-FAMILY
   ['] NFILE-REF:FAMILY-KEY$
   ['] NFILE-REF:FAMILY-BY-KEY
   ['] NFILE-REF:FAMILY-KEY-MATCH?
   ['] NFILE-REF:CON-DECLARED
   ['] NFILE-REF:CON-KEY$
   ['] NFILE-REF:CON-BY-KEY
   ['] NFILE-REF:BIND-CON
      s" TFAM:FILE-REF-USE" TARGET-XT TFAM-REF-USE-XT execute
   ['] NFILE-REF:CON-KEY$ ['] NFILE-REF:BIND-CON
      s" CHECKER-REG:FILE-CON-USE" TARGET-XT CON-USE-XT execute ;

: CLEAR-NOMINALS ( -- )
   TFAM-RESET-XT @ dup 0<> if ACTION-XT execute else drop then
   CON-RESET-XT @ dup 0<> if ACTION-XT execute else drop then
   0 TFAM-RESET-XT ! 0 CON-RESET-XT ! ;

: FRAME ( -- ptr u8 )
   DEPTH @ 0= if BAD then DEPTH @ 1- FRAME-BYTES * FRAMES ;
: F@ ( n -- n ) FRAME + CELL-VIEW @ ;
: F! ( n n -- ) FRAME + CELL-VIEW ! ;
\ hb-build exports its resolved root as HABU_BUILD_CACHE to every child that
\ opens the definition cache (build-support.f BF-PREPARE-ENV), so a maker
\ whose image is saved never touches BUILD-CACHE's buffers; ENV-INPUT:CURRENT$
\ stores nothing and is not observed. A top-level run started without the
\ variable resolves the same tiers through BUILD-CACHE itself.
: ROOT$ ( -- ptr u8 n )
   s" HABU_BUILD_CACHE" ENV-INPUT:CURRENT$ dup 0 > if exit then
   2drop BUILD-CACHE:ROOT$ ;

: HASH-N ( n -- )
   HASH-CELL ! HASH-CTX HASH-CELL CELL SHA256-FEED ;
: HASH-S ( ptr u8 n -- )
   {: a:ptr u:n :}
   u HASH-N
   u 0 > if HASH-CTX a u SHA256-FEED then ;
: HASH-START ( ptr u8 n -- )
   HASH-CTX SHA256-BEGIN HASH-S ;

: DIAG-W2 ( ptr u8 n -- )
   {: bytes:ptr size:n :}
   0 begin dup size < while
      dup {: sent:n :}
      2 bytes sent + size sent - write
      dup 0 <= if drop E-FS-IO throw then
      +
   repeat drop ;

: DIAG-N ( n -- )
   {: value:n :}
   value 10 >= if value 10 / recurse then
   s" 0123456789" drop value 10 mod + 1 DIAG-W2 ;

: REFUSE ( ptr u8 n ptr u8 n -- )
   {: path:ptr pathu:n reason:ptr reasonu:n :}
   s" native-cache: entry refused " DIAG-W2
   path pathu DIAG-W2
   s" : " DIAG-W2
   reason reasonu DIAG-W2
   S\" \n" DIAG-W2 ;

public

\ FNV-1a 64 over whole cells, then the tail bytes. Every target is
\ little-endian, so a cell read matches the bytes on disk on every host.
: CHECKSUM ( ptr u8 n -- n )
   {: bytes:ptr size:n :}
   size CELL / CELL * {: whole:n :}
   FNV-BASIS
   whole 0 ?do bytes i + CELL-VIEW @ xor FNV-PRIME * CELL +loop
   size whole ?do bytes i + c@ xor FNV-PRIME * loop ;

private

\ Read one cache file into ENTRY through one descriptor, sized by that
\ descriptor, so a same-key writer renaming a larger entry in or a pruner
\ removing it mid-read cannot fail the build. A missing file is a miss; a
\ short, foreign or damaged one is refused on fd 2 and is a miss too, which
\ the next store replaces.
: CACHE-READ ( ptr u8 n ptr u8 n n -- n bool )
   {: path:ptr pathu:n magic:ptr magicu:n head:n :}
   path pathu FS:OPEN-SIZED {: file:fd size:n found:bool :}
   found 0= if 0 false exit then
   size head < if
      file FD>N close
      path pathu s" short" REFUSE 0 false exit
   then
   \ A failed reserve leaves the descriptor open; its throw ends the build.
   size ENTRY-RESERVE
   file 0 ENTRY size FS:READ-SIZED
   0 ENTRY 8 magic magicu STR= 0= if
      path pathu s" magic" REFUSE 0 false exit
   then
   0 ENTRY 16 + size 16 - CHECKSUM 0 ENTRY 8 + LE:U64@ <> if
      path pathu s" checksum" REFUSE 0 false exit
   then
   size true ;

\ Publish the image built in STORE-IMAGE under path, sealing its checksum.
: IMAGE-WRITE ( ptr u8 n n -- )
   {: path:ptr pathu:n size:n :}
   pathu FS-PATH-CAP > if E-FS-PATH throw then
   0 STORE-IMAGE 16 + size 16 - CHECKSUM 0 STORE-IMAGE 8 + LE:U64!
   path pathu 0 STORE-IMAGE size ATOMIC-WRITE-FILE ;

: IMAGE ( ptr u8 n ptr u8 n -- n )
   {: magic:ptr magicu:n object:ptr objectu:n :}
   magicu 8 <> objectu 0 < or if BAD then
   ENTRY-HEAD objectu + {: size:n :}
   size STORE-IMAGE-RESERVE
   magic 0 STORE-IMAGE 8 BYTE-COPY
   object 0 STORE-IMAGE ENTRY-HEAD + objectu BYTE-COPY
   size ;

\ The object file for a file key: hb-file-, the key, .hfo in the cache root.
: FILE-PATH ( ptr u8 n -- ptr u8 n )
   {: key:ptr keyu:n :}
   keyu 64 <> if BAD then
   s" hb-file-" {: pre:ptr preu:n :}
   s" .hfo" {: suf:ptr sufu:n :}
   pre OBJECT-NAME preu BYTE-COPY
   key OBJECT-NAME preu + keyu BYTE-COPY
   suf OBJECT-NAME preu + keyu + sufu BYTE-COPY
   ROOT$ OBJECT-NAME OBJECT-NAME-BYTES PATH-BUF JOIN-PATH
   PATH-BUF swap ;

: ZERO-SCRATCH ( ptr u8 n -- )
   {: bytes:ptr size:n :}
   size 0 ?do 0 bytes i + c! loop ;

\ Room for one more frame. The only fallible step of an open, taken before
\ the file is pushed so a failure leaves nothing to undo.
: FRAME-RESERVE ( -- ) DEPTH @ 1+ FRAME-BYTES * FRAMES-RESERVE ;

\ Open the reserved frame, zeroed as a fresh one: a file closed at this
\ depth left its record there.
: FILE-OPEN ( -- )
   1 DEPTH +!
   FRAME FRAME-BYTES ZERO-SCRATCH ;

: FILE-CLOSE ( -- ) -1 DEPTH +! ;

: FRAME-PATH ( -- ptr u8 n ) FRAME F-INPUT + 0 ptr-field @ F-INPUT-U F@ ;
: FRAME-ROOT ( -- ptr u8 n ) FRAME F-ROOT + 0 ptr-field @ F-ROOT-U F@ ;
: KEPT-KEY ( -- ptr u8 n ) FRAME F-KEY + 64 ;

: ORIGIN-HOOK-ALLOWED? ( -- bool )
   check@ {: hook:n :}
   hook 0= if true exit then
   ORIGIN-RECORD @ dup NULL-PTR = if drop false exit then
   dup NATIVE-BODY-ABI:ORIGIN-HOOK cells + CELL-VIEW @ hook =
   swap NATIVE-BODY-ABI:ORIGIN-TAKE cells + CELL-VIEW @ 0<> and ;

\ The checker the compile runs under: 0 checking off, 1 the owner's hook, 2
\ any other hook. Another hook is process code no key can name.
: CHECK-STATE ( -- n )
   check@ 0= if 0 exit then
   ORIGIN-HOOK-ALLOWED? if 1 else 2 then ;

\ The ambient compile state both keys hash: the tier, the checker state, the
\ open borrow and the dispatch role.
: AMBIENT-HASH ( -- )
   data-base NCOMP-DISPATCH:TIER-CELL + @ HASH-N
   CHECK-STATE HASH-N
   BORROW-ACTIVE @ HASH-N
   DISPATCH-ROLE 32 HASH-S ;

: DEFINITION-KEY ( ptr u8 n ptr u8 n -- ptr u8 n )
   {: source:ptr sourceu:n pending:ptr pendingu:n :}
   s" habu-native-definition-v9" HASH-START
   BUILD-TARGET:CURRENT RTARGET:PROFILE$ HASH-S
   s" HBNUNIT2-definition-v2" HASH-S
   AMBIENT-HASH
   pending pendingu HASH-S
   source sourceu HASH-S
   HASH-CTX HASH-RAW SHA256-END
   HASH-RAW KEY-HEX SHA256>HEX
   KEY-HEX 64 ;

: PENDING-COPY ( -- ptr u8 n )
   NFILE-REF:PENDING-KEY$ {: key:ptr keyu:n :}
   keyu 0 <= if BAD then
   keyu PENDING-KEY-RESERVE
   key 0 PENDING-KEY keyu BYTE-COPY
   keyu PENDING-KEY-U !
   0 PENDING-KEY keyu ;

: OPAQUE? ( -- bool )
   OPAQUE-XT @ dup 0= if drop true exit then
   OPAQUE-XT-TYPE execute ;

: ADMISSION-REASON$ ( -- ptr u8 n )
   DEPTH @ 0= if s" no-source-frame" exit then
   data-base TRUSTED-CELL + CELL-VIEW @ 0<> if s" trusted" exit then
   ORIGIN-RECORD @ NULL-PTR = if s" missing-origin-capability" exit then
   DISPATCH-ROLE-KNOWN @ 0= if s" unknown-role" exit then
   BORROW-ACTIVE @ if
      BORROW-OWNER-READY? 0= if s" unavailable-owner" exit then
   else OWNER-READY? 0= if s" unavailable-owner" exit then then
   OPAQUE? if s" opaque" exit then
   ORIGIN-HOOK-ALLOWED? 0= if s" incompatible-origin-hook" exit then
   BORROW-ACTIVE @ if s" " exit then
   NUNIT-ORIGIN:PENDING-AVAILABLE? 0= if
      s" missing-pending-origin" exit
   then
   s" " ;

: ARM-NATIVE ( [ ptr u8 n -- ] -- bool )
   {: completion :}
   ORIGIN-RECORD @ dup NULL-PTR = if drop false exit then
   NATIVE-BODY-ABI:PENDING-TAKE cells + COMPLETION-CELL
      completion swap xt!
   true ;

: ARM-WARM-ORIGIN ( -- )
   BORROW-ACTIVE @ if exit then
   data-base TRUSTED-CELL + CELL-VIEW @ 0<> if exit then
   ORIGIN-RECORD @ dup NULL-PTR = if drop exit then
   {: record:ptr :}
   record NATIVE-BODY-ABI:ORIGIN-HOOK cells + CELL-VIEW @ {: hook:n :}
   hook 0= check@ hook <> or if exit then
   record NATIVE-BODY-ABI:ORIGIN-TAKE cells + CELL-VIEW @ dup 0= if
      drop exit
   then
   ORIGIN-TAKE-XT ARM-NATIVE drop ;

\ The span of the current definition's name token in the innermost file's
\ source, read where the origin journal reads it (ENGINE-ORIGIN SOURCE-NAME);
\ 0 0 when the name came from other text.
: DEF-SPAN ( -- n n )
   DEPTH @ 0= if 0 0 exit then
   FRAME F-SRC + 0 ptr-field @ {: src:ptr :}
   src NULL-PTR = if 0 0 exit then
   F-SRC-U F@ {: size:n :}
   data-base PEND-CELL + @ 0= if DEF-TKA-CELL else PENDTKA-CELL then
      data-base + CELL-VIEW @ {: token:n :}
   token src NULL-PTR BYTE-VIEW - - {: at:n :}
   at 0 < at size >= or if 0 0 exit then
   at begin
      dup size < if src over + c@ 32 > else false then
   while 1+ repeat {: end:n :}
   end at = if 0 0 exit then
   at end ;

: SOURCE-AT ( -- n )
   DEPTH @ 0= if -1 exit then
   F-SRC-U F@ 0= if -1 exit then
   FRAME F-SRC + 0 ptr-field @ NULL-PTR = if -1 exit then
   data-base INP-CELL + CELL-VIEW @ {: cursor:n :}
   FRAME F-SRC + 0 ptr-field @ NULL-PTR BYTE-VIEW - {: source:n :}
   cursor source - {: at:n :}
   at 0 >= at F-SRC-U F@ <= and if at else -1 then ;

: DIAG-PATH? ( -- bool )
   DEPTH @ 0= if false exit then
   F-INPUT-U F@ 0= if false exit then
   SOURCE-AT 0 >= ;

: DIAG-FALLBACK ( ptr u8 n -- )
   {: reason:ptr reasonu:n :}
   NUNIT-DEP:DIAG? @ 0= if exit then
   s" native-cache: compile-attempt reason=" DIAG-W2
   reason reasonu DIAG-W2
   ndict@ XREF-REC XREF-NAME$ {: name:ptr nameu:n :}
   nameu 0 > if s"  name=" DIAG-W2 name nameu DIAG-W2 then
   DIAG-PATH? if
      s"  path=" DIAG-W2
      FRAME F-INPUT + 0 ptr-field @ F-INPUT-U F@ DIAG-W2
   then
   S\" \n" DIAG-W2 ;

: COMPILE-ORIGINAL ( ptr u8 n ptr u8 n -- )
   {: source:ptr sourceu:n reason:ptr reasonu:n :}
   reason reasonu DIAG-FALLBACK
   source sourceu DISPATCH-ORIGINAL @ DISPATCH-XT execute
   1 COMPILE-N +! ;

: COMPILE-STAGED ( -- )
   DISPATCH-SOURCE @ DISPATCH-SOURCE-U @
      STAGED-REASON @ STAGED-REASON-U @ COMPILE-ORIGINAL ;

\ A definition another one's dispatch interrupts compiles uncaptured, so its
\ file has no object.
: BUSY ( ptr u8 n -- )
   s" busy" NUNIT-OBJECT:FILE-REFUSE
   s" busy" COMPILE-ORIGINAL ;

\ Compile the staged definition under capture, for reason. With store it is
\ admitted: a definition the compiler calls cacheable and free of ordered
\ dependencies is published. It is packed into its file's object while one
\ is open.
: COLD-CANDIDATE ( ptr u8 n n ptr u8 n bool -- )
   {: key:ptr keyu:n ord:n reason:ptr reasonu:n store:bool :}
   reason STAGED-REASON ! reasonu STAGED-REASON-U !
   ord NUNIT-DEP:BEGIN-DEFINITION
   NUNIT-OBJECT:DEFINITION-BEGIN
   ['] COMPILE-STAGED catch {: rc:n :}
   rc 0<> if
      NUNIT-OBJECT:DISCARD-DEFINITION
      NUNIT-DEP:CLOSE
      NFILE-REF:PENDING-RELEASE
      rc throw
   then
   NUNIT-DEP:COMPILED!
   NUNIT-OBJECT:DEFINITION-END {: eligible:bool :}
   eligible if
      NUNIT-OBJECT:DEFINITION-CACHEABLE? {: cacheable:bool :}
      store cacheable 0= and if
         s" not-cacheable" ord NUNIT-OBJECT:REPORT-STORE-REFUSED
      then
      store cacheable and {: admitted:bool :}
      admitted if NUNIT-DEP:DEFINITION-EXPORT else NULL-PTR 0 false then
         {: deps:ptr depsu:n storable:bool :}
      storable if
         ord 0 PENDING-KEY PENDING-KEY-U @ NFILE-REF:PUBLISH-DEFINITION
      then
      NUNIT-OBJECT:FILE-OPEN? if
         key keyu deps depsu NUNIT-OBJECT:PACK-DEFINITION
            {: object:ptr objectu:n :}
         object objectu key keyu ord DEF-SPAN
            NUNIT-OBJECT:FILE-ADD-DEFINITION
      then
      storable 0= if
         admitted if
            s" ordered-dependency" ord NUNIT-OBJECT:REPORT-STORE-REFUSED
         then
         NFILE-REF:PENDING-RELEASE
      then
   else NFILE-REF:PENDING-RELEASE then
   NUNIT-OBJECT:DISCARD-DEFINITION
   NUNIT-DEP:CLOSE ;

\ A definition the cache does not admit still compiles under capture while
\ its file has an object open; it is never looked up or stored.
: DISPATCH-DEFINITION ( ptr u8 n -- )
   {: source:ptr sourceu:n :}
   DISPATCH-BUSY @ if source sourceu BUSY exit then
   ADMISSION-REASON$ {: reason:ptr reasonu:n :}
   reasonu 0 > NUNIT-OBJECT:FILE-OPEN? 0= and if
      source sourceu reason reasonu COMPILE-ORIGINAL exit
   then
   1 DISPATCH-BUSY !
   ndict@ {: ord:n :}
   PENDING-COPY {: pending:ptr pendingu:n :}
   source sourceu pending pendingu DEFINITION-KEY
      {: key:ptr keyu:n :}
   reasonu 0 > if
      key keyu ord reason reasonu false COLD-CANDIDATE
   else
      key keyu ord s" eligible-cache-miss" true COLD-CANDIDATE
   then
   0 DISPATCH-BUSY ! ;

: DISPATCH-STAGED ( -- )
   DISPATCH-SOURCE @ DISPATCH-SOURCE-U @ DISPATCH-DEFINITION ;

: WRAPPED-DISPATCH ( -- )
   DISPATCH-BUSY @ if DISPATCH-SOURCE @ DISPATCH-SOURCE-U @ BUSY exit then
   ADMISSION-REASON$ {: reason:ptr reasonu:n :}
   reasonu 0= NUNIT-OBJECT:FILE-OPEN? or if
      ['] DISPATCH-STAGED NUNIT-OBJECT:WITH-DEFINITIONS
   else
      DISPATCH-SOURCE @ DISPATCH-SOURCE-U @ reason reasonu
         COMPILE-ORIGINAL
   then ;

: DISPATCH-GUARDED ( ptr u8 n -- )
   DISPATCH-BUSY @ if BUSY exit then
   DISPATCH-SOURCE-U ! DISPATCH-SOURCE !
   ['] WRAPPED-DISPATCH catch {: rc:n :}
   0 DISPATCH-BUSY !
   rc 0<> if NFILE-REF:PENDING-RELEASE rc throw then ;

: BIND-OPAQUE ( -- )
   s" native-pending-opaque?" XREF-FIND
   dup XREF-FOUND? 0= if drop 0 OPAQUE-XT ! exit then
   XREF-START dup TERMINAL-CALL:IN-ENGINE-TEXT? 0= if drop 0 then
   OPAQUE-XT ! ;

: PIN-ORIGIN-GETTER ( -- )
   0 ORIGIN-GETTER !
   s" native-origin-cell" TARGET-XT?
      dup 0= if drop exit then
   dup TERMINAL-CALL:IN-ENGINE-TEXT? 0= if drop exit then
   ORIGIN-GETTER ! ;

\ Keep the first kept bound setters; clear the cells of the rest.
: RETIRE-KEEP ( n -- )
   {: kept:n :}
   RETIRE-N @ kept ?do
      0 i RETIRE-XTS !  0 i RETIRE-ORDS !  false i RETIRE-SEALED !
   loop
   kept RETIRE-N ! ;

\ Clear the retire hook of each bound setter.
: RETIRE-UNSET ( -- )
   RETIRE-N @ 0 ?do 0 i RETIRE-XTS @ RETIRE-SET-XT execute loop
   0 RETIRE-KEEP ;

\ A cut to count, noticed while the records it removes and their code stand:
\ clear and forget each unsealed bound setter whose record it removes. A
\ sealed setter stays bound, as its code keeps retiring through its own hook
\ after an engine build's rewind.
: RETIRE-CUT ( n -- )
   {: count:n :}
   0 RETIRE-N @ 0 ?do
      {: kept:n :}
      i RETIRE-XTS @ {: xt:n :}
      i RETIRE-SEALED @ 0= i RETIRE-ORDS @ count >= and if
         0 xt RETIRE-SET-XT execute
         kept
      else
         xt kept RETIRE-XTS !
         i RETIRE-ORDS @ kept RETIRE-ORDS !
         i RETIRE-SEALED @ kept RETIRE-SEALED !
         kept 1+
      then
   loop RETIRE-KEEP ;

\ Bind setter xt with its record ordinal, sealed when that record lies below a
\ set image seal.
: RETIRE-ADD ( n -- )
   {: xt:n :}
   s" XREF:BEFORE-RETIRE!" XREF-FIND NFILE-REF:REC-ORD {: ord:n :}
   xt RETIRE-N @ RETIRE-XTS !
   ord RETIRE-N @ RETIRE-ORDS !
   ord SEAL-NDICT@ < SEAL-NDICT@ 0<> and RETIRE-N @ RETIRE-SEALED !
   1 RETIRE-N +! ;

: RETIRE-BOUND? ( n -- bool )
   {: xt:n :}
   RETIRE-N @ 0 ?do
      i RETIRE-XTS @ xt = if true unloop exit then
   loop false ;

\ The open file objects see every retirement through each XREF's
\ XREF:BEFORE-RETIRE!, looked up at the session's open and at each file's open
\ and close. An engine build's rewind cuts the running kernel's records but not
\ its code, which keeps retiring through its own hook, and the build then loads
\ a new XREF: a setter stays bound while it is sealed or its record lives, so
\ a bound xt is always a live setter. Every open file refuses when no setter
\ is bound: its retirements would pass unseen. A new setter refuses every open
\ file too, as its hook held nothing from its definition until now
\ (src/habu/xref.f retires words through its own before it closes), and when
\ the list is full it stays unbound, so the next open finds it new again.
\ Without the cut cell nothing would drop a cut setter, and every file refuses
\ cut-notice-absent: no setter is bound.
: BIND-RETIRE ( -- )
   CUT-CELL @ NULL-PTR = if exit then
   s" XREF:BEFORE-RETIRE!" TARGET-XT? {: xt:n :}
   xt 0= if
      RETIRE-N @ 0= if s" retire-hook-absent" NUNIT-OBJECT:REFUSE-ALL then
      exit
   then
   xt RETIRE-BOUND? 0= if
      s" retire-hook-new" NUNIT-OBJECT:REFUSE-ALL
      RETIRE-N @ RETIRE-CAP = if exit then
      xt RETIRE-ADD
   then
   ['] NUNIT-OBJECT:FILE-RETIRE RETIRE>N xt RETIRE-SET-XT execute ;

\ The kernel's capture cut notice, to count.
: CUT-NOTICE ( n -- )
   dup RETIRE-CUT NUNIT-OBJECT:FILE-CUT ;

\ The open file objects and the bound setters follow every dictionary cut
\ through the running kernel's one capture cut cell, held for the session. A
\ kernel without it leaves the cell unbound, and every file object refuses
\ (NUNIT-OBJECT FILE-BEGIN).
: BIND-CUT ( -- )
   s" native-unit-cut-cell" NUNIT-CAPTURE:OPTIONAL-PRODUCER-XT
      dup 0= if drop exit then
   CUT-CELL-XT execute {: cell:ptr :}
   cell CELL-VIEW @ 0<> if BAD then
   ['] CUT-NOTICE cell CUT-SLOT xt!
   cell CUT-CELL !
   BIND-RETIRE ;

: UNBIND-CUT ( -- )
   RETIRE-UNSET
   CUT-CELL @ {: cell:ptr :}
   cell NULL-PTR = if exit then
   0 cell CELL-VIEW !
   NULL-PTR CUT-CELL ! ;

\ The open file objects see each word the source runs through the running
\ kernel's run observer, held for the session, and read its resolution legs
\ through the running checker owner. A kernel without either observes nothing,
\ and every file object refuses (NUNIT-OBJECT FILE-BEGIN).
: OBSERVE ( -- )
   NUNIT-CAPTURE:RUN-OBSERVER? 0= if exit then
   s" CHECKER-OWNER:RESOLUTION-TOP" NUNIT-CAPTURE:BOUND-PRODUCER-XT
      NUNIT-DEP:USE-TOP
   [: NUNIT-OBJECT:REFUSE-WORD ;] NUNIT-DEP:USE-REFUSE
   s" NHOST:UNOBSERVE-RUNS" NUNIT-CAPTURE:BOUND-PRODUCER-XT {: undo:n :}
   [: NFILE-REF:REC-ORD NUNIT-DEP:OBSERVE-RUN ;]
      s" NHOST:OBSERVE-RUNS" NUNIT-CAPTURE:BOUND-PRODUCER-XT OBSERVE-XT execute
   undo UNOBSERVE-XT ! ;

: UNOBSERVE ( -- )
   UNOBSERVE-XT @ {: undo:n :}
   undo 0= if exit then
   0 UNOBSERVE-XT !
   undo ACTION-XT execute
   0 NUNIT-DEP:USE-TOP
   NUNIT-DEP:DROP-REFUSE ;

: BIND-ORIGIN-RECORD ( -- )
   ORIGIN-GETTER @ dup 0= if drop NULL-PTR ORIGIN-RECORD ! exit then
   ORIGIN-CELL-XT execute ORIGIN-RECORD ! ;

: BIND-DISPATCH ( -- )
   data-base NCOMP-DISPATCH:XT-CELL + CELL-VIEW @ {: actual:n :}
   actual ['] DISPATCH-GUARDED DISPATCH>N <> if
      actual DISPATCH-ORIGINAL !
      PIN-COMPILER
   then
   actual 0= if 0 DISPATCH-ROLE-KNOWN ! exit then
   ['] COMPILER-ROLE$ NUNIT-DEP:USE-COMPILER-ROLE
   ['] DISPATCH-GUARDED NCOMP:DISPATCH!
   BIND-ORIGIN-RECORD ;

: UNBIND-DISPATCH ( -- )
   NULL-PTR ORIGIN-RECORD !
   data-base NCOMP-DISPATCH:XT-CELL + CELL-VIEW @
      ['] DISPATCH-GUARDED DISPATCH>N = if
      DISPATCH-ORIGINAL @ DISPATCH-XT NCOMP:DISPATCH!
   then
   0 DISPATCH-ORIGINAL ! 0 DISPATCH-ROLE-KNOWN ! ;

: BORROW-DISPATCH ( [ ptr u8 n -- ] [ ptr u8 n -- ] -- )
   DISPATCH>N BORROW-ACCEPT !
   DISPATCH-ORIGINAL @ BORROWED-ORIGINAL !
   DISPATCH>N DISPATCH-ORIGINAL !
   1 BORROW-ACTIVE !
   ['] DISPATCH-GUARDED NCOMP:DISPATCH! ;

: RETURN-DISPATCH ( -- )
   BORROWED-ORIGINAL @ DISPATCH-ORIGINAL !
   0 BORROWED-ORIGINAL !
   0 BORROW-ACTIVE ! 0 BORROW-ACCEPT ! ;

: CANON ( ptr u8 n -- ptr u8 n bool )
   {: path:ptr pathu:n :}
   path pathu SOURCE-ROOT:CANON-OS {: answer:ptr answeru:n found:bool :}
   path pathu answer answeru found NFILE-INPUTS:ADD-CANON
   answer answeru found ;

: FILE-INPUT ( ptr u8 n -- ) s" " NFILE-INPUTS:ADD-MTIME ;

: UNIT-RUN ( -- )
   FRAME F-INPUT + 0 ptr-field @ F-INPUT-U F@
   FRAME F-ROOT + 0 ptr-field @ F-ROOT-U F@
   FRAME F-SRC + 0 ptr-field @ F-SRC-U F@
   F-Q F@ N>Q F-PRIOR F@ N>UNIT execute ;

\ The file's source runs under the engine's source observer, whose callbacks
\ judge each seed word the top level runs (NUNIT-DEP RUN-BEFORE, RUN-AFTER).
\ The frame arms when the loader evaluates exactly the source span; a child
\ load pushes its own frame.
: OBSERVED-RUN ( -- )
   FRAME F-SRC + 0 ptr-field @ F-SRC-U F@ {: source:ptr sourceu:n :}
   DEPTH @ source sourceu ['] NUNIT-DEP:RUN-BEFORE ['] NUNIT-DEP:RUN-AFTER
      [: UNIT-RUN ;] SOURCE-EXEC:RUN {: rc:n :}
   rc 0<> if rc throw then ;

: FILE-SUCCESS ( -- )
   NATIVE? @ 0= if exit then
   FRAME F-INPUT + 0 ptr-field @ F-INPUT-U F@
   FRAME F-ROOT + 0 ptr-field @ F-ROOT-U F@
      BUILD-PRODUCER:COMPILER-DONE
   FRAME F-INPUT + 0 ptr-field @ F-INPUT-U F@
   FRAME F-ROOT + 0 ptr-field @ F-ROOT-U F@
      SOURCE-ROOT:RELATIVE
      s" src/habu/native-runtime.f" STR= if
      NUNIT-CAPTURE:BIND-PRODUCER
   then ;

\ A file's instance name: its path relative to the root that resolved it, and
\ role 0 under the engine's source root, 1 under any other. The same sources
\ built from another tree path name the same instances; reuse soundness rests
\ on each definition's own key and the live re-check of what it consumed.
: FILE-NAME ( ptr u8 n ptr u8 n -- ptr u8 n n )
   {: path:ptr pathu:n root:ptr rootu:n :}
   rootu 0= if path pathu 1 exit then
   path pathu root rootu SOURCE-ROOT:RELATIVE
   root rootu SOURCE-ROOT:ENGINE$ STR= if 0 else 1 then ;

\ The file frame's dependency rows, while the cache diagnostics are on.
: FRAME-REPORT ( -- )
   NUNIT-DEP:DIAG? @ 0= if exit then
   FRAME F-INPUT + 0 ptr-field @ F-INPUT-U F@ NUNIT-DEP:FILE-REPORT ;

\ The input rows the innermost file recorded since its mark join its object.
: ROWS-FLUSH ( -- )
   F-MARK F@ NFILE-INPUTS:MARK NFILE-INPUTS:SPAN
      NUNIT-OBJECT:FILE-INPUTS+
   NFILE-INPUTS:MARK F-MARK F! ;

: SOURCE-ROW ( -- )
   FRAME-PATH FRAME F-RAW + 0 ptr-field @ F-RAW-U F@
      NFILE-INPUTS:ADD-SOURCE ;

variable USER-SEEN
: USER-SEEN! ( ptr u8 -- ) CELL-VIEW @ USER-SEEN ! ;

\ TASK's user-slot watermark, and whether TASK is bound to read it.
: USER-MARK ( -- n bool )
   s" TASK:OWNED-USER-NEXT" NUNIT-CAPTURE:OPTIONAL-PRODUCER-XT
      dup 0= if drop 0 false exit then
   ['] USER-SEEN! swap USER-NEXT-XT execute
   USER-SEEN @ true ;

\ Whether the innermost file took TASK user slots: a replay restores none.
\ A file during which TASK came to be bound has no watermark to compare and
\ counts as taking them.
: SLOTS-MOVED? ( -- bool )
   USER-MARK {: mark:n bound:bool :}
   bound 0= if false exit then
   F-SLOTS-BOUND F@ 0= if true exit then
   mark F-SLOTS F@ <> ;

\ The early journal's serial, and whether it records rows; 0 false without
\ the target's journal.
: EARLY-MARK ( -- n bool )
   s" ENGINE-ORIGIN-EARLY:FILE-MARK" NUNIT-ORIGIN:JOURNAL-XT
   dup 0= if drop 0 false exit then
   MARK-XT execute {: serial:n phase:n armed:bool :}
   serial armed ;

\ Whether the target's early journal's hook is the installed check hook, so
\ an armed journal records the file's definitions; false without it.
: EARLY-HOOK? ( -- bool )
   s" ENGINE-ORIGIN-EARLY:FILE-HOOK?" NUNIT-ORIGIN:JOURNAL-XT
   dup 0= if drop false exit then
   HOOK-XT execute ;

\ The full journal's arm flag and whether its hook is the installed check
\ hook; false false without the target's journal.
: FULL-STATE ( -- bool bool )
   s" ENGINE-ORIGIN:FILE-STATE" NUNIT-ORIGIN:JOURNAL-XT
   dup 0= if drop false false exit then
   ORIGIN-STATE-XT execute ;

\ The sink of a file loaded while a journal can record a row no sink holds:
\ the open refuses the file and the hit check misses. It is never stored.
-1 constant SINK-UNSUPPORTED

\ Where a definition's origin row goes now (NUNIT-OBJECT SINK-*, or
\ SINK-UNSUPPORTED), and true when the builder feeds it to the early journal
\ with no symbol. The journals gate their observers differently: the early
\ hook records every checked definition whether or not the early journal is
\ armed, its export observer records while that journal is armed, and its
\ CAST: observer always records; the full journal's hook, export and CAST:
\ observers record while the full journal is armed. Full ARM arms the early
\ journal too. A sink holds rows from one journal only, so a state where
\ any other observer can record has none. The arm flags and the hooks are
\ read; the installed CAST: observer is not. It records only for a CAST:
\ declaration, whose file's object is never installed
\ (NUNIT-OBJECT:UNSUPPORTED$), so admitting that kind must first make SINK
\ read it.
: SINK ( -- n bool )
   PRE-BOUND @ 0<> if
      PRE-MARK-HOOK @ execute nip if NUNIT-OBJECT:SINK-PREFIX false exit then
      NUNIT-OBJECT:SINK-EARLY true exit
   then
   FULL-STATE {: full:bool full-hook:bool :}
   EARLY-MARK nip {: early:bool :}
   EARLY-HOOK? {: early-hook:bool :}
   full 0= early 0= and early-hook 0= and if NUNIT-OBJECT:SINK-NONE false exit then
   full full-hook and if NUNIT-OBJECT:SINK-FULL false exit then
   full 0= early and early-hook and if NUNIT-OBJECT:SINK-EARLY false exit then
   SINK-UNSUPPORTED false ;

\ The open file's object key names its place in the load as its records'
\ keys name it, not its text: this engine and target, the ambient compile
\ state, and the file's instance key (NFILE-REF:PUSH-FILE): the loader chain,
\ the kind, path and root that opened it, and its occurrence under that
\ loader. The object's stored declaration and import keys start with the
\ instance key, so a key that omitted the loader would find an object whose
\ keys this load cannot reproduce.
: FILE-KEY ( -- ptr u8 n )
   ENGINE-ID:KEY$ {: engine:ptr engineu:n :}
   s" habu-native-file-v3" HASH-START
   engine engineu HASH-S
   BUILD-TARGET:CURRENT RTARGET:PROFILE$ HASH-S
   NATIVE? @ HASH-N
   AMBIENT-HASH
   NFILE-REF:INSTANCE-KEY$ HASH-S
   \ A full journal's rows locate definitions relative to the target's engine
   \ root; a target without SOURCE-ROOT hashes a fixed marker instead.
   SINK drop NUNIT-OBJECT:SINK-FULL = if
      s" SOURCE-ROOT:ENGINE$" NUNIT-ORIGIN:JOURNAL-XT
      dup 0= if drop s" no-engine-root" else ROOT-XT execute then HASH-S
   then
   HASH-CTX HASH-RAW SHA256-END
   HASH-RAW FILE-KEY-HEX SHA256>HEX
   FILE-KEY-HEX 64 ;

: ORIGIN-SINK-REFUSE ( -- ) s" origin-sink" NUNIT-OBJECT:FILE-REFUSE ;

\ The open file's sink, cursors and first record, stored as it opens. A file
\ opened with no sink is refused.
: ORIGIN-OPEN ( -- )
   SINK {: kind:n fed:bool :}
   kind F-OSINK F!
   fed if 1 else 0 then F-OFED F!
   PRE-BOUND @ 0<> if PRE-MARK-HOOK @ execute drop else 0 then F-OCUR F!
   EARLY-MARK drop F-OSER F!
   ndict@ F-OFIRST F!
   kind SINK-UNSUPPORTED = if ORIGIN-SINK-REFUSE exit then
   kind NUNIT-OBJECT:ORIGIN-SINK! ;

\ The checker symbol of the record at ord under name, derived from the
\ record; false for a retired package or without a ready checker owner.
: ROW-SYM ( n ptr u8 n -- n bool )
   {: ord:n name:ptr nameu:n :}
   OWNER-READY? 0= if 0 false exit then
   ord XREF-REC NFILE-REF:PACKAGE-OF {: pkg:ptr pkgu:n vis:n :}
   vis PKG-RETIRED = if 0 false exit then
   vis PKG-PUBLIC = if SYM-PUBLIC else
      vis PKG-PRIVATE = if SYM-PRIVATE else 0 then
   then {: sym:n :}
   pkg pkgu sym name nameu LIVE-ORIGIN true ;

\ The one record of the closing file whose code starts at xt.
: FULL-ORD ( n -- n bool )
   {: xt:n :}
   0 0
   ndict@ F-OFIRST F@ ?do
      i XREF-REC XREF-START xt = if drop i swap 1+ swap then
   loop
   {: hits:n ord:n :}
   ord hits 1 = ;

: ORIGIN-REFUSE ( -- ) s" origin-record" NUNIT-OBJECT:FILE-REFUSE ;

: PREFIX-ROW ( n n n ptr u8 n ptr u8 n -- )
   1 HARVEST-N +! NUNIT-OBJECT:ORIGIN+ ;

\ An early journal row: a fed row has no symbol, any other the record's.
: EARLY-ROW ( n n n n ptr u8 n ptr u8 n n n -- )
   {: serial:n phase:n ord:n sym:n path:ptr pathu:n name:ptr nameu:n
      at:n end:n :}
   1 HARVEST-N +!
   HARVEST-FED @ 0<> if
      sym 0<> if ORIGIN-REFUSE exit then
   else
      ord name nameu ROW-SYM {: derived:n known:bool :}
      known 0= derived sym <> or if ORIGIN-REFUSE exit then
   then
   ord at end path pathu name nameu NUNIT-OBJECT:ORIGIN+ ;

\ A full journal row names its definition by xt: the file's one record there.
: FULL-ROW ( n n n n ptr u8 n ptr u8 n n n -- )
   {: serial:n phase:n xt:n sym:n path:ptr pathu:n name:ptr nameu:n
      at:n end:n :}
   1 HARVEST-N +!
   xt FULL-ORD {: ord:n unique:bool :}
   unique 0= if ORIGIN-REFUSE exit then
   ord name nameu ROW-SYM {: derived:n known:bool :}
   known 0= derived sym <> or if ORIGIN-REFUSE exit then
   ord at end path pathu name nameu NUNIT-OBJECT:ORIGIN+ ;

\ Visit the journal rows of word name past serial mark.
: JOURNAL-ROWS ( n [ n n n n ptr u8 n ptr u8 n n n -- ] ptr u8 n -- )
   NUNIT-ORIGIN:JOURNAL-XT {: mark:n visit rows:n :}
   rows 0= if exit then
   mark visit rows ROWS-XT execute ;

\ Store the closing file's origin rows: those its sink recorded since it
\ opened, under the same sink. A file with child loads keeps only its sink
\ kind: the install replays only leaves. The journal's serial advances once
\ per row it recorded, and not at all for a file whose rows go elsewhere.
: ORIGIN-HARVEST ( -- )
   NUNIT-OBJECT:FILE-LEAF? 0= if exit then
   SINK {: kind:n fed:bool :}
   fed if 1 else 0 then {: fedn:n :}
   kind F-OSINK F@ <> fedn F-OFED F@ <> or if ORIGIN-SINK-REFUSE exit then
   0 HARVEST-N !
   fedn HARVEST-FED !
   kind NUNIT-OBJECT:SINK-PREFIX = if
      PRE-MARK-HOOK @ execute drop F-OCUR F@ < if ORIGIN-SINK-REFUSE exit then
      F-OCUR F@ ['] PREFIX-ROW PRE-EACH-HOOK @ execute
   then
   kind NUNIT-OBJECT:SINK-EARLY = if
      F-OSER F@ ['] EARLY-ROW s" ENGINE-ORIGIN-EARLY:FILE-ROWS" JOURNAL-ROWS
   then
   kind NUNIT-OBJECT:SINK-FULL = if
      F-OSER F@ ['] FULL-ROW s" ENGINE-ORIGIN:FILE-ROWS" JOURNAL-ROWS
   then
   kind NUNIT-OBJECT:SINK-EARLY >= if HARVEST-N @ else 0 then
   EARLY-MARK drop F-OSER F@ - <> if ORIGIN-SINK-REFUSE then ;

\ Retire the checker's scratch at a file boundary (CHECKER-REG:FILE-SCRATCH-
\ RETIRE), so a file's DATA comparison and pin scan never see it and a hit,
\ which runs nothing, leaves the state a compile leaves. The live dictionary's
\ word is the target's from the point its checker.f defines it, before the
\ handover: the target's PRIM: declarations already run its signature parser.
\ Before that point the target has no checker scratch, and nothing is retired.
: SCRATCH-RETIRE ( -- )
   s" CHECKER-REG:FILE-SCRATCH-RETIRE" TARGET-XT?
   dup 0= if drop exit then
   ACTION-XT execute ;

\ Open the object of the file just pushed; its first row is its own source's
\ stat and digest. Rows join an object only at ROWS-FLUSH, so the row recorded before
\ FILE-BEGIN is still this file's, and a throw leaves no object to end.
: OBJECT-OPEN ( -- )
   FILE-KEY {: key:ptr keyu:n :}
   key FRAME F-KEY + keyu BYTE-COPY
   NFILE-INPUTS:MARK F-MARK F!
   USER-MARK if F-SLOTS F! 1 else drop 0 then F-SLOTS-BOUND F!
   SOURCE-ROW
   \ The parent's segment closes in FILE-BEGIN, and this file's before-image
   \ follows it: both start from rested scratch.
   SCRATCH-RETIRE
   FRAME-PATH FRAME-ROOT F-KIND F@ F-AT F@ NUNIT-OBJECT:FILE-BEGIN
   ORIGIN-OPEN
   NUNIT-OBJECT:DEF-OPEN? 0= if
      s" def-open-absent" NUNIT-OBJECT:FILE-REFUSE
   then
   \ The install's writers are tier-1-only (prims.f def-open/def-close).
   data-base NCOMP-DISPATCH:TIER-CELL + @ 1 <> if
      s" tier" NUNIT-OBJECT:FILE-REFUSE
   then
   \ A hit would skip what another hook refuses, so the file compiles live.
   CHECK-STATE 2 = if s" checker-hook" NUNIT-OBJECT:FILE-REFUSE then
   BIND-RETIRE ;

\ The first 16 hex digits of SHA-256 over the result digests of the innermost
\ file's defs definitions, in ordinal order.
: RESULTS$ ( n -- ptr u8 n ) {: defs:n :}
   HASH-CTX SHA256-BEGIN
   defs 0 ?do
      i NUNIT-OBJECT:FILE-ORD NFILE-REF:RESULT-DIGEST 0= if BAD then
      {: digest:ptr :}
      HASH-CTX digest 32 SHA256-FEED
   loop
   HASH-CTX HASH-RAW SHA256-END
   HASH-RAW RESULTS-HEX SHA256>HEX
   RESULTS-HEX 16 ;

\ Write the innermost file's object and read it back on every build; the
\ cache diagnostics only print what was written or why the file has none. A
\ file that leaves its scope or takes TASK user slots changes state a replay
\ does not restore, so it has none.
: OBJECT-CLOSE ( -- )
   SCRATCH-RETIRE                  \ before the segment's DATA comparison
   ROWS-FLUSH
   BIND-RETIRE
   NUNIT-DEP:DIAG? @ if
      s" native-file: image path=" DIAG-W2 FRAME-PATH DIAG-W2
      s"  copied=" DIAG-W2 NUNIT-OBJECT:IMAGE-BYTES DIAG-N
      S\" \n" DIAG-W2
   then
   NUNIT-DEP:FILE-EXPORT nip nip 0= if
      s" ordered-dependency" NUNIT-OBJECT:FILE-REFUSE
   then
   NUNIT-DEP:FILE-SCOPE-KEPT? 0= if s" scope-exit" NUNIT-OBJECT:FILE-REFUSE then
   SLOTS-MOVED? if s" task-slots" NUNIT-OBJECT:FILE-REFUSE then
   ORIGIN-HARVEST
   \ Encoding binds the earlier records the file's slots name, so the rows
   \ are exported again after it.
   NUNIT-OBJECT:FILE-ENCODE
   \ A file that leaves the key's ambient state changed has an effect past
   \ its end the install does not replay. Encoding closes the last segment
   \ first, so a kernel cell that segment left changed is the reason named.
   FILE-KEY KEPT-KEY STR= 0= if s" ambient-changed" NUNIT-OBJECT:FILE-REFUSE then
   NUNIT-DEP:FILE-EXPORT {: deps:ptr depsu:n storable:bool :}
   storable 0= if s" ordered-dependency" NUNIT-OBJECT:FILE-REFUSE then
   NUNIT-OBJECT:FILE-REFUSED? if
      NUNIT-DEP:DIAG? @ 0= if exit then
      s" native-file: refused path=" DIAG-W2 FRAME-PATH DIAG-W2
      s"  reason=" DIAG-W2 NUNIT-OBJECT:REASON$ DIAG-W2
      S\" \n" DIAG-W2 exit
   then
   KEPT-KEY {: key:ptr keyu:n :}
   key keyu deps depsu NUNIT-OBJECT:PACK-FILE {: obj:ptr obju:n :}
   s" HBFOBJ01" obj obju IMAGE {: size:n :}
   key keyu FILE-PATH size IMAGE-WRITE
   key keyu FILE-PATH s" HBFOBJ01" ENTRY-HEAD CACHE-READ
      {: got:n found:bool :}
   found 0= got size <> or if BAD then
   0 ENTRY ENTRY-HEAD + size ENTRY-HEAD - key keyu F-SRC-U F@
      NUNIT-OBJECT:OPEN-FILE
   NUNIT-OBJECT:ORIGIN-FORMED? 0= if BAD then
   NUNIT-OBJECT:FILE-COUNTS
      {: defs:n rows:n inputs:n recs:n data:n reqs:n loads:n segs:n slots:n
         upds:n pins:n checks:n ffi:n :}
   NUNIT-DEP:DIAG? @ 0= if exit then
   s" native-file: object path=" DIAG-W2 FRAME-PATH DIAG-W2
   s"  key=" DIAG-W2 key keyu DIAG-W2
   s"  defs=" DIAG-W2 defs DIAG-N
   s"  rows=" DIAG-W2 rows DIAG-N
   s"  inputs=" DIAG-W2 inputs DIAG-N
   s"  records=" DIAG-W2 recs DIAG-N
   s"  data=" DIAG-W2 data DIAG-N
   s"  requires=" DIAG-W2 reqs DIAG-N
   s"  loads=" DIAG-W2 loads DIAG-N
   s"  segments=" DIAG-W2 segs DIAG-N
   s"  slots=" DIAG-W2 slots DIAG-N
   s"  updates=" DIAG-W2 upds DIAG-N
   s"  pins=" DIAG-W2 pins DIAG-N
   s"  ffi=" DIAG-W2 ffi DIAG-N
   s"  checks=" DIAG-W2 checks DIAG-N
   s"  bytes=" DIAG-W2 size DIAG-N
   s"  results=" DIAG-W2 defs RESULTS$ DIAG-W2
   s"  src=" DIAG-W2
   defs 0 ?do
      i 0 > if s" ," DIAG-W2 then
      i NUNIT-OBJECT:FILE-SPAN swap DIAG-N s" -" DIAG-W2 DIAG-N
   loop
   S\" \n" DIAG-W2 ;

\ The size of the object image CACHE-READ left in ENTRY, header included.
variable OBJECT-U

: OBJECT-BYTES ( -- ptr u8 n ) 0 ENTRY ENTRY-HEAD + OBJECT-U @ ENTRY-HEAD - ;

\ Check the object's framing, then view its FILE sections: OPEN-FILE ends
\ by releasing its view.
: OPEN-OBJECT ( -- )
   OBJECT-BYTES KEPT-KEY F-SRC-U F@ NUNIT-OBJECT:OPEN-FILE
   OBJECT-BYTES NUNIT-FILE:ARCH-AARCH64 s" FILE" KEPT-KEY
      NUNIT-FILE:READ-VIEW ;

\ Read the innermost file's object and view it; a missing, damaged or
\ malformed one is a miss.
: OBJECT-READ? ( -- bool )
   KEPT-KEY FILE-PATH s" HBFOBJ01" ENTRY-HEAD CACHE-READ {: size:n found:bool :}
   found 0= if false exit then
   size OBJECT-U !
   ['] OPEN-OBJECT catch {: rc:n :}
   rc 0= if true exit then
   rc E-NUNIT-PROFILE = rc E-NUNIT-FILE = or if false exit then
   rc throw ;

\ The viewed object's input and dependency rows hold for this session.
: OBJECT-FRESH? ( -- bool )
   11 NUNIT-FILE:SECTION$ NFILE-INPUTS:FRESH? 0= if false exit then
   14 NUNIT-FILE:SECTION$ 2dup NUNIT-DEP:FORMAT 0 swap NUNIT-DEP:CHECK-ROWS ;

\ A fresh object's record name is live where the install would define it:
\ the file compiles live, and the language refuses the duplicate.
: MISS-NAME-LIVE ( ptr u8 n -- )
   {: name:ptr nameu:n :}
   NUNIT-DEP:DIAG? @ 0= if exit then
   s" native-file: miss path=" DIAG-W2 FRAME-PATH DIAG-W2
   s"  reason=name-live word=" DIAG-W2 name nameu DIAG-W2
   S\" \n" DIAG-W2 ;

\ A fresh object's FFI rows expect an index or a selection the registry no
\ longer gives: the file compiles live against the registry as it stands.
: MISS-FFI-ROWS ( -- )
   NUNIT-DEP:DIAG? @ 0= if exit then
   s" native-file: miss path=" DIAG-W2 FRAME-PATH DIAG-W2
   S\"  reason=ffi-rows\n" DIAG-W2 ;

\ A miss the hit check names: the viewed object is dropped and the file
\ loads from source.
: OBJECT-MISS ( ptr u8 n -- )
   {: why:ptr whyu:n :}
   NUNIT-FILE:RESET-KEEP
   NUNIT-DEP:DIAG? @ 0= if exit then
   s" native-file: miss path=" DIAG-W2 FRAME-PATH DIAG-W2
   s"  reason=" DIAG-W2 why whyu DIAG-W2
   S\" \n" DIAG-W2 ;

\ The target journal word an installed row of sink kind is imported
\ through; 0 for the other kinds or when the target lacks it.
: IMPORT-XT ( n -- n )
   {: kind:n :}
   kind NUNIT-OBJECT:SINK-EARLY = if
      s" ENGINE-ORIGIN-EARLY:FILE-IMPORT" NUNIT-ORIGIN:JOURNAL-XT exit
   then
   kind NUNIT-OBJECT:SINK-FULL = if
      s" ENGINE-ORIGIN:FILE-IMPORT" NUNIT-ORIGIN:JOURNAL-XT exit
   then
   0 ;

\ The viewed object's origin rows can go to the active sink: it is the one
\ they were stored under, and a journal sink has its import and, for rows
\ that carry a symbol, a ready checker owner to derive it.
: ORIGIN-SINK? ( -- bool )
   SINK {: kind:n fed:bool :}
   NUNIT-OBJECT:ORIGIN-KIND kind <> if false exit then
   kind NUNIT-OBJECT:SINK-EARLY < if true exit then
   fed 0= OWNER-READY? 0= and if false exit then
   kind IMPORT-XT 0<> ;

\ The viewed object's checker slice is prepared and its plan kept for the
\ install; false when the checker refuses the slice, which keeps no plan.
\ Every other code is not the object's and leaves.
: CHECKER-PREPARED? ( -- bool )
   ['] NUNIT-OBJECT:PREPARE-CHECKER catch {: rc:n :}
   rc 0= if true exit then
   rc E-NUNIT-PROFILE <> if rc throw then
   false ;

\ The innermost file has a fresh object this session can install. The
\ checker slice is prepared last: everything the checker refuses about it is
\ a miss decided before any install side effect, so a hit never throws.
: OBJECT-HIT? ( -- bool )
   NUNIT-OBJECT:FILE-REFUSED? if false exit then
   OBJECT-READ? 0= if false exit then
   NUNIT-OBJECT:ORIGIN-FORMED? 0= if s" origin-format" OBJECT-MISS false exit then
   ORIGIN-SINK? 0= if s" origin-sink" OBJECT-MISS false exit then
   \ Freshness is checked only for an object the session can install: the
   \ check reads files and records rows into the innermost file's object.
   NUNIT-OBJECT:UNSUPPORTED$ dup 0<> if OBJECT-MISS false exit then 2drop
   OBJECT-FRESH? 0= if NUNIT-FILE:RESET-KEEP false exit then
   NUNIT-OBJECT:FFI-ROWS-HOLD? 0= if MISS-FFI-ROWS NUNIT-FILE:RESET-KEEP false exit then
   NUNIT-OBJECT:LIVE-NAME? if MISS-NAME-LIVE NUNIT-FILE:RESET-KEEP false exit then
   2drop
   CHECKER-PREPARED? 0= if s" checker-prepare" OBJECT-MISS false exit then
   true ;

\ The installed object's sink kind, 1 when its rows are fed, and the
\ journal import its rows go through.
variable IMPORT-KIND
variable IMPORT-FED
variable IMPORT-AT

\ Append one installed origin row through its sink, as the run would have.
\ The hit check found a ready checker owner for a row that carries a
\ symbol, and ord's record is one the install just made in a live
\ wordlist, so its symbol is known.
: ORIGIN-IMPORT ( n n n ptr u8 n ptr u8 n -- )
   {: ord:n at:n end:n path:ptr pathu:n name:ptr nameu:n :}
   IMPORT-KIND @ NUNIT-OBJECT:SINK-PREFIX = if
      ord at end path pathu name nameu PRE-IMPORT-HOOK @ execute exit
   then
   IMPORT-FED @ 0<> if 0 else ord name nameu ROW-SYM drop then {: sym:n :}
   IMPORT-KIND @ NUNIT-OBJECT:SINK-EARLY = if
      ord sym at end path pathu name nameu IMPORT-AT @ EARLY-IMPORT-XT execute
      exit
   then
   sym ord path pathu name nameu at end IMPORT-AT @ FULL-IMPORT-XT execute ;

\ Append the installed object's origin rows once, in their stored order.
: ORIGIN-INSTALL ( -- )
   NUNIT-OBJECT:ORIGIN-KIND {: kind:n :}
   kind IMPORT-KIND !
   SINK nip if 1 else 0 then IMPORT-FED !
   kind IMPORT-XT IMPORT-AT !
   ['] ORIGIN-IMPORT NUNIT-OBJECT:ORIGIN-EACH ;

\ Install the viewed object in place of running the file. The session
\ manifest keeps the input rows the skipped load would have recorded.
: INSTALL-OBJECT ( -- )
   11 NUNIT-FILE:SECTION$ NFILE-INPUTS:APPEND
   OBJECT-BYTES KEPT-KEY NUNIT-OBJECT:INSTALL-OBJECT
   ORIGIN-INSTALL
   KEPT-KEY FILE-PATH BUILD-CACHE:USED drop
   1 REUSED-N +!
   NUNIT-DEP:DIAG? @ 0= if exit then
   NUNIT-OBJECT:FILE-COUNTS
      {: defs:n rows:n inputs:n recs:n data:n reqs:n loads:n segs:n slots:n
         upds:n pins:n checks:n ffi:n :}
   s" native-file: reused path=" DIAG-W2 FRAME-PATH DIAG-W2
   s"  records=" DIAG-W2 recs DIAG-N
   s"  segments=" DIAG-W2 segs DIAG-N
   s"  ffi=" DIAG-W2 ffi DIAG-N
   S\" \n" DIAG-W2 ;

: FILE-UNIT-WITH ( ptr u8 n ptr u8 n ptr u8 n [ n n -- ] [ ptr u8 n ptr u8 n ptr u8 n [ n n -- ] -- ] bool -- )
   {: path:ptr pathu:n root:ptr rootu:n source:ptr sourceu:n q prior target:bool :}
   SESSION @ 0= if
      path pathu root rootu source sourceu q prior execute exit
   then
   DEPTH @ 0= if 0
   else
      target if
         s" SOURCE-UNIT:PARENT-ACTION$" TARGET-XT
            PARENT-ACTION-XT execute
      else SOURCE-UNIT:PARENT-ACTION$ then
         {: mode:n asked:ptr askedu:n :}
      asked drop askedu drop mode
   then {: kind:n :}
   \ The loader's frame still holds the bytes as read; `source` is the copy
   \ the interpreter runs.
   target if
      s" SOURCE-ROOT:ACTIVE-SOURCE" TARGET-XT ACTIVE-SOURCE-XT execute
   else SOURCE-ROOT:ACTIVE-SOURCE then
      {: raw:ptr rawu:n :} 2drop 2drop
   \ The rows the parent recorded so far join its object before the child's
   \ opens.
   DEPTH @ 0 > if ROWS-FLUSH then
   DEPTH @ 0 > if SOURCE-AT else -1 then {: at:n :}
   FRAME-RESERVE
   path pathu root rootu FILE-NAME kind NFILE-REF:PUSH-FILE
   FILE-OPEN
   path FRAME F-INPUT + 0 ptr-field !
   pathu F-INPUT-U F!
   source FRAME F-SRC + 0 ptr-field !
   sourceu F-SRC-U F!
   raw FRAME F-RAW + 0 ptr-field !
   rawu F-RAW-U F!
   root FRAME F-ROOT + 0 ptr-field !
   rootu F-ROOT-U F!
   q Q>N F-Q F!
   prior UNIT>N F-PRIOR F!
   kind F-KIND F!
   at F-AT F!
   ['] OBJECT-OPEN catch {: open-rc:n :}
   open-rc 0<> if FILE-CLOSE NFILE-REF:POP-FILE open-rc throw then
   \ The before-image follows BEGIN-UNIT's observer hooks, and the parent's
   \ reopened segment's follows CLOSE-UNIT's, which may unhook them, so the
   \ capture's own writes are never a file's change to earlier DATA.
   NUNIT-DEP:BEGIN-UNIT
   NUNIT-OBJECT:SEGMENT-IMAGE
   \ Both paths observe the file's source: an engine build that installs it
   \ keeps the producer digests a run would.
   NATIVE? @ if path pathu root rootu source sourceu BUILD-PRODUCER:OBSERVE then
   OBJECT-HIT? {: hit:bool :}
   hit if ['] INSTALL-OBJECT else ['] OBSERVED-RUN then catch {: unit-rc:n :}
   \ A hit's install ends the plan OBJECT-HIT? kept; one that threw before
   \ it leaves the plan for this release.
   NUNIT-OBJECT:RELEASE-CHECKER
   ['] NUNIT-OBJECT:DEFINITIONS-CLOSE catch {: plan-rc:n :}
   ['] FRAME-REPORT catch {: report-rc:n :}
   unit-rc 0= plan-rc 0= and hit 0= and
      if ['] OBJECT-CLOSE catch else 0 then
      {: object-rc:n :}
   NUNIT-OBJECT:FILE-END
   NUNIT-DEP:CLOSE-UNIT
   NUNIT-OBJECT:SEGMENT-IMAGE
   unit-rc 0= plan-rc 0= and
      if ['] FILE-SUCCESS catch else 0 then
      {: finish-rc:n :}
   FILE-CLOSE
   \ The rows recorded so far belong to the closed file's object.
   DEPTH @ 0 > if NFILE-INPUTS:MARK F-MARK F! then
   ['] NFILE-REF:POP-FILE catch {: pop-rc:n :}
   unit-rc 0<> if unit-rc throw then
   finish-rc 0<> if finish-rc throw then
   plan-rc 0<> if plan-rc throw then
   report-rc 0<> if report-rc throw then
   pop-rc 0<> if pop-rc throw then
   object-rc 0<> if object-rc throw then ;

: FILE-UNIT ( ptr u8 n ptr u8 n ptr u8 n [ n n -- ] -- )
   PRIOR-UNIT @ true FILE-UNIT-WITH ;

: HOST-FILE-UNIT ( ptr u8 n ptr u8 n ptr u8 n [ n n -- ] -- )
   HOST-PRIOR-UNIT @ false FILE-UNIT-WITH ;

: PIN-HOST-SOURCE ( -- )
   \ Engine loading replaces the dictionary; retain the host loader and
   \ environment hooks so both source owners can observe and detach cleanly.
   s" SOURCE-UNIT:CURRENT" TARGET-XT? {: current:n :}
   s" SOURCE-UNIT:USE" TARGET-XT? {: use:n :}
   s" SOURCE-INPUT:USE" TARGET-XT? {: input:n :}
   s" SOURCE-INPUT:RESET" TARGET-XT? {: input-reset:n :}
   s" ENV-INPUT:USE" TARGET-XT? {: env-use:n :}
   s" ENV-INPUT:RESET" TARGET-XT? {: env-reset:n :}
   current 0= use 0= or input 0= or
      input-reset 0= or if 0 HOST-SOURCE-READY ! exit then
   current UNIT-CURRENT-XT execute HOST-PRIOR-UNIT !
   use HOST-UNIT-USE !
   input HOST-INPUT-USE !
   input-reset HOST-INPUT-RESET !
   env-use HOST-ENV-USE !
   env-reset HOST-ENV-RESET !
   1 HOST-SOURCE-READY ! ;

: BIND-HOST-SOURCE ( -- )
   HOST-SOURCE-BOUND @ if exit then
   HOST-SOURCE-READY @ 0= if BAD then
   ['] HOST-FILE-UNIT HOST-UNIT-USE @ UNIT-USE-XT execute
   1 HOST-SOURCE-BOUND !
   HOST-ENV-USE @ 0= HOST-ENV-RESET @ 0= and
      NATIVE? @ and TARGET-SOURCE-BOUND @ 0= and 0= if
      HOST-ENV-USE @ 0= HOST-ENV-RESET @ 0= or if ENV-MISSING then
      ['] NFILE-INPUTS:ADD-ENV HOST-ENV-USE @ ENV-USE-XT execute
   then
   \ An old donor's GETENV has no observer. Only the engine's retained-host
   \ phase admits its absence; target REBIND requires the replacement hook.
   ['] CANON ['] SOURCE-ROOT:READ-OS HOST-INPUT-USE @ INPUT-USE-XT execute ;

: BIND-TARGET-SOURCE ( -- )
   s" SOURCE-UNIT:CURRENT" TARGET-XT UNIT-CURRENT-XT execute
      {: prior :}
   s" SOURCE-UNIT:USE" TARGET-XT TARGET-UNIT-USE !
   s" SOURCE-INPUT:RESET" TARGET-XT TARGET-INPUT-RESET !
   s" ENV-INPUT:RESET" ENV-XT TARGET-ENV-RESET !
   prior UNIT>N ['] FILE-UNIT UNIT>N <> if
      prior PRIOR-UNIT !
      ['] FILE-UNIT TARGET-UNIT-USE @ UNIT-USE-XT execute
   then
   1 TARGET-SOURCE-BOUND !
   ['] NFILE-INPUTS:ADD-ENV s" ENV-INPUT:USE" ENV-XT
      ENV-USE-XT execute
   ['] CANON s" SOURCE-ROOT:READ-OS" TARGET-XT SOURCE-READ-XT
      s" SOURCE-INPUT:USE" TARGET-XT
      INPUT-USE-XT execute ;

: SEED ( -- )
   SEEDED @ if exit then
   BOOT-N @ NFILE-REF:BIND-SEED
   1 SEEDED !
   NATIVE? @ if AOT-ARM:B0 @ else 0 then NUNIT-ORIGIN:BIND
   BIND-OWNER ;

: PRINT-COUNTS ( -- )
   s" files reused " type REUSED-N @ .
   s"  compiled " type COMPILE-N @ . ;

: CLEAR-SCRATCH ( -- )
   0 REUSED-N ! 0 COMPILE-N !
   0 NUNIT-DEP:DIAG? !
   0 HOST-INPUT-RESET ! 0 HOST-UNIT-USE ! 0 HOST-INPUT-USE !
   0 HOST-ENV-USE ! 0 HOST-ENV-RESET ! 0 TARGET-ENV-RESET !
   NULL-PTR APP-PATH ! 0 APP-PATH-U !
   NULL-PTR REQUEST-PATH ! 0 REQUEST-PATH-U !
   NULL-PTR REQUEST-MANIFEST ! 0 REQUEST-MANIFEST-U !
   NULL-PTR DISPATCH-SOURCE ! 0 DISPATCH-SOURCE-U !
   NULL-PTR STAGED-REASON ! 0 STAGED-REASON-U !
   OBJECT-NAME OBJECT-NAME-BYTES ZERO-SCRATCH FILE-KEY-HEX 64 ZERO-SCRATCH
   RESULTS-HEX 64 ZERO-SCRATCH
   HASH-CTX SHA256-CTX-BYTES ZERO-SCRATCH
   HASH-RAW 32 ZERO-SCRATCH
   KEY-HEX 64 ZERO-SCRATCH HASH-CELL CELL ZERO-SCRATCH
   PATH-BUF FS-PATH-CAP ZERO-SCRATCH ;

\ The process's first session takes the seed's record count.
: BOOT-TAKE ( -- )
   BOOT-N @ 0<> if exit then
   ndict@ BOOT-N ! ;

: OPEN ( -- )
   1 SESSION !
   BOOT-TAKE
   s" HABU_NATIVE_CACHE_DIAG" GETENV s" 1" STR= NUNIT-DEP:DIAG? !
   0 SEEDED ! 0 DEPTH !
   0 REUSED-N ! 0 COMPILE-N ! 0 DISPATCH-BUSY !
   0 DISPATCH-ORIGINAL ! 0 DISPATCH-ROLE-KNOWN !
   0 BORROWED-ORIGINAL ! 0 BORROW-ACTIVE ! 0 BORROW-ACCEPT !
   0 CHECKER-ROLE-KNOWN !
   NULL-PTR PUBLICATION-OWNER !
   NULL-PTR ORIGIN-RECORD !
   0 PUBLICATION-CLEAR-XT !
   0 TFAM-RESET-XT ! 0 CON-RESET-XT !
   0 HOST-SOURCE-BOUND ! 0 TARGET-SOURCE-BOUND !
   0 HOST-SOURCE-READY ! 0 HOST-UNIT-USE ! 0 HOST-INPUT-USE !
   0 HOST-INPUT-RESET !
   0 HOST-ENV-USE ! 0 HOST-ENV-RESET !
   0 TARGET-INPUT-RESET !
   0 TARGET-ENV-RESET !
   0 TARGET-UNIT-USE !
   NFILE-INPUTS:RESET
   ['] FILE-INPUT BUILD-INPUT:USE
   \ Resolved here, before RUN and APP-LOAD bind the ENV-INPUT observers, so
   \ the cache root never becomes a build input. BUILD-CACHE:ROOT$ creates
   \ the directory it selects; an exported root may not exist yet.
   ROOT$ MAKE-DIRS
   ['] XREF-REC NFILE-REF:RECORD-AT!
   NUNIT-CAPTURE:BIND-PRODUCER
   PIN-HOST-CHECKER
   PIN-HOST-SOURCE
   PIN-ORIGIN-GETTER
   BIND-DISPATCH
   BIND-OPAQUE
   BIND-CUT
   \ The engine build rewinds the dictionary before loading its prefix. Its
   \ namespace publications must already reach the retained source owner.
   BIND-OWNER
   BIND-NOMINALS
   OBSERVE ;

: CLOSE ( -- )
   BUILD-INPUT:RESET
   CLEAR-PUBLICATION
   UNBIND-DISPATCH
   begin DEPTH @ 0 > while
      FILE-CLOSE
      NFILE-REF:POP-FILE
   repeat
   TARGET-SOURCE-BOUND @ if
      PRIOR-UNIT @ TARGET-UNIT-USE @ UNIT-USE-XT execute
      TARGET-INPUT-RESET @ ACTION-XT execute
      TARGET-ENV-RESET @ ACTION-XT execute
   then
   HOST-SOURCE-BOUND @ if
      HOST-PRIOR-UNIT @ HOST-UNIT-USE @ UNIT-USE-XT execute
      HOST-INPUT-RESET @ ACTION-XT execute
      HOST-ENV-RESET @ dup 0<> if ACTION-XT execute else drop then
   then
   \ Restore each loader before releasing its continuation from the host heap.
   ['] ORDINARY-UNIT PRIOR-UNIT !
   ['] ORDINARY-UNIT HOST-PRIOR-UNIT !
   UNOBSERVE
   NUNIT-DEP:RESET NUNIT-DEP:UNBIND
   NUNIT-ORIGIN:RESET
   UNBIND-CUT
   NUNIT-OBJECT:DEFINITIONS-CLOSE
   NUNIT-OBJECT:CLOSE
   CLEAR-NOMINALS
   NFILE-REF:CLOSE
   FRAMES-RELEASE PENDING-KEY-RELEASE
   ENTRY-RELEASE STORE-IMAGE-RELEASE
   0 DEPTH ! 0 SESSION ! 0 SEEDED !
   0 DISPATCH-BUSY ! 0 DISPATCH-ROLE-KNOWN !
   0 CHECKER-ROLE-KNOWN !
   0 ORIGIN-GETTER !
   0 OPAQUE-XT ! 0 PENDING-KEY-U ! 0 OBJECT-U !
   REPORT? @ if PRINT-COUNTS then
   CLEAR-SCRATCH ;

: OUTER-RUN ( -- )
   OPEN OUTER-BODY @ execute ;

: APP-RUN ( -- ) APP-PATH @ APP-PATH-U @ script-required ;

: REFRESH-CANONICAL-ORIGIN ( -- )
   s" ENGINE-ORIGIN:FILE-STATE" TARGET-XT? dup 0= if drop exit then
   ORIGIN-STATE-XT execute {: armed:bool canonical:bool :}
   armed drop
   canonical if
      s" ENGINE-ORIGIN:INSTALL" TARGET-XT ACTION-XT execute
   then ;

public

: ARM-NATIVE-ACCEPT ( [ ptr u8 n -- ] -- bool ) ARM-NATIVE ;

\ Bind the builder's prefix rows while it loads the checked prefix.
: PREFIX-ORIGIN-USE ( [ -- n bool ] [ n [ n n n ptr u8 n ptr u8 n -- ] -- ] [ n n n ptr u8 n ptr u8 n -- ] -- )
   PRE-IMPORT-HOOK ! PRE-EACH-HOOK ! PRE-MARK-HOOK ! 1 PRE-BOUND ! ;
: PREFIX-ORIGIN-CLEAR ( -- ) 0 PRE-BOUND ! ;

: WITH ( ptr u8 n [ -- ] -- )
   {: mode:ptr modeu:n body :}
   mode modeu s" engine" STR= NATIVE? !
   true REPORT? !
   body OUTER-BODY !
   ['] OUTER-RUN ['] CLOSE finally ;

: WITH-QUIET ( ptr u8 n [ -- ] -- )
   {: mode:ptr modeu:n body :}
   mode modeu s" engine" STR= NATIVE? !
   false REPORT? !
   body OUTER-BODY !
   ['] OUTER-RUN ['] CLOSE finally ;

: RUN ( [ -- ] -- )
   SEED BIND-HOST-SOURCE execute ;

: REBIND ( -- )
   SESSION @ 0= if exit then
   SEED
   BIND-TARGET-SOURCE
   BIND-OWNER
   PIN-TARGET-CHECKER
   PIN-ORIGIN-GETTER
   BIND-DISPATCH ;

: HANDOVER-PREPARE ( -- )
   SESSION @ 0= if exit then
   CLEAR-NOMINALS ;
: HANDOVER-BIND ( -- )
   SESSION @ 0= if exit then
   BUILD-PRODUCER:CURRENT$ PIN-CHECKER
   NFILE-REF:RESET-CONSTRUCTORS
   BIND-OWNER
   BIND-NOMINALS ;
: RETAIN-HOST-CHECKER ( -- )
   PIN-HOST-CHECKER ;

: BORROW ( [ ptr u8 n -- ] [ ptr u8 n -- ] -- )
   SESSION @ 0= if drop NCOMP:DISPATCH! exit then
   BORROW-DISPATCH ;
: RETURN-BORROW ( -- )
   SESSION @ 0= if exit then
   RETURN-DISPATCH ;
: SUSPEND-DISPATCH ( -- )
   SESSION @ 0= if exit then
   data-base NCOMP-DISPATCH:XT-CELL + CELL-VIEW @
      ['] DISPATCH-GUARDED DISPATCH>N = if
      DISPATCH-ORIGINAL @ DISPATCH-XT NCOMP:DISPATCH!
   then ;

: DEFINITIONS-CLOSE ( -- )
   SESSION @ 0= if exit then
   NUNIT-OBJECT:DEFINITIONS-CLOSE ;

: APP-LOAD ( ptr u8 n ptr u8 n bool -- )
   {: path:ptr pathu:n manifest:ptr manifestu:n report:bool :}
   report REPORT? !
   SEED BIND-HOST-SOURCE BIND-OWNER
   REFRESH-CANONICAL-ORIGIN
   BIND-DISPATCH
   path APP-PATH ! pathu APP-PATH-U !
   APP-RUN
   manifest NULL-PTR <> if
      NFILE-INPUTS:BYTES$ {: bytes:ptr size:n :}
      size 0= if BAD then
      manifest manifestu bytes size ATOMIC-WRITE-FILE
   then ;

: REQUEST-RUN ( -- )
   REQUEST-PATH @ REQUEST-PATH-U @
   REQUEST-MANIFEST @ REQUEST-MANIFEST-U @ REPORT? @ 0<> APP-LOAD ;

: LOAD ( ptr u8 n ptr u8 n -- )
   {: path:ptr pathu:n mode:ptr modeu:n :}
   path REQUEST-PATH ! pathu REQUEST-PATH-U !
   NULL-PTR REQUEST-MANIFEST ! 0 REQUEST-MANIFEST-U !
   mode modeu ['] REQUEST-RUN WITH ;

: LOAD-WITH-INPUTS ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: path:ptr pathu:n mode:ptr modeu:n manifest:ptr manifestu:n :}
   path REQUEST-PATH ! pathu REQUEST-PATH-U !
   manifest REQUEST-MANIFEST ! manifestu REQUEST-MANIFEST-U !
   mode modeu ['] REQUEST-RUN WITH ;

;package
