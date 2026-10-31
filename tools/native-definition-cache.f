\ native-definition-cache.f - automatic native definition reuse. The ordinary
\ source loader always runs; the pending native dispatch selects one
\ definition after its source actions. Each file's object lives at
\ BUILD-OUT:OBJ$ (tools/build-out.f).
require src/lib/errors.f
require src/lib/string.f
require src/lib/fmt.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/build-input.f
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
require tools/build-out.f
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
\ The input rows below F-MARK belong to a file object already.
64 constant F-MARK
\ The bytes as read. F-SRC is the copy the interpreter runs, which differs
\ from them when the file starts with `#!` (include.f LOAD-BYTES).
72 constant F-RAW
80 constant F-RAW-U
\ The file's object key, taken when it opened: 64 hex digits.
88 constant F-KEY
\ The origin sink the file's harvest segment opened under (NUNIT-OBJECT
\ SINK-*), 1 when the builder fed its rows to the early journal, the
\ builder's prefix row count and the early journal's serial when it opened,
\ and the file's first record.
152 constant F-OSINK
160 constant F-OFED
168 constant F-OCUR
176 constant F-OSER
184 constant F-OFIRST
\ The file's object bytes as OBJECT-READ? copied them: their offset in
\ OBJECTS and their length.
192 constant F-OBJ
200 constant F-OBJ-U
\ Nonzero while the file's warm run installs through its object
\ (REUSE-RUN); its successful loads so far; the install place kept across
\ a child's unit (PLACE-SAVE), five cells.
208 constant F-REUSE
216 constant F-LOADS
224 constant F-SAVE
\ The heads of the file's run that missed (REUSE-HEAD), plus one
\ when its installs stopped (LOAD-ORDER).
264 constant F-MISS
272 constant FRAME-BYTES

\ An object entry: magic | checksum over the bytes after it | the HBNUNIT2
\ file object.
16 constant ENTRY-HEAD
$cbf29ce484222325 constant FNV-BASIS
$100000001b3 constant FNV-PRIME

\ One FRAME-BYTES record per open file depth.
DYNAMIC-BUFFER FRAMES u8
\ One entry read and one image written at a time: DISPATCH-BUSY serialises
\ every lookup and store.
DYNAMIC-BUFFER ENTRY u8
DYNAMIC-BUFFER STORE-IMAGE u8
\ The open files' object bytes as a stack, each file's region above its
\ loader's, 8-aligned: a reuse run loads children while its own object is
\ open, and their reads must leave the loader's bytes in place. Growth moves the
\ buffer, so a frame keeps its region's offset; OBJECTS-U is the top.
DYNAMIC-BUFFER OBJECTS u8
variable OBJECTS-U
variable DEPTH
variable SESSION
variable SEEDED
\ The dictionary count when the process's first session opened. The records
\ below it are the seed's; CLOSE leaves it, so a later session's files cannot
\ name an earlier session's records as the seed's. It is a process cell: an
\ image stores it as 0, so the process booted from it takes its own at its
\ first session.
variable BOOT-N
BOOT-N NUNIT-OBJECT:PROCESS-CELL
variable NATIVE?
variable REPORT?
\ Files this session entered, each once at its frame after OBJECT-OPEN,
\ whether it then runs through its object or compiles; the files compiled
\ are FILES-N - REUSED-N.
variable FILES-N
\ Files whose reuse run found every head of their run in their
\ objects. A run in which any head no row holds compiled counts as compiled.
variable REUSED-N
\ Definitions installed at their heads, and definitions compiled.
variable INSTALLED-N
variable COMPILED-N
\ Whether this session's counts are the build's: the application or engine
\ source load (WITH), never the link-support or writer session (WITH-QUIET).
\ REPORT? only gates printing them.
variable COUNTED?
\ HABU_BUILD_COUNTS as OPEN read it, before any observer binds, so it is no
\ build input: where a counted session whose body succeeded writes
\ `<reused> <compiled>` and a newline before CLOSE, for hb-build's report
\ (tools/hb-build-lib.f HBB-COUNTS-READ). Empty writes nothing.
PATH-CAP BUFFER: COUNTS-PATH
variable COUNTS-PATH-U
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
variable UNOBSERVE-XT
variable TARGET-UNIT-USE
variable TARGET-INPUT-RESET
variable TARGET-ENV-RESET
variable TARGET-SOURCE-BOUND
\ The target's SOURCE-UNIT:LOADER-KIND once its source is bound, else 0.
variable TARGET-KIND
variable HOST-INPUT-RESET
variable HOST-SOURCE-BOUND
variable HOST-SOURCE-READY
variable HOST-UNIT-USE
variable HOST-INPUT-USE
variable HOST-ENV-USE
variable HOST-ENV-RESET
create HASH-CTX SHA256-CTX-BYTES allot
create HASH-RAW 32 allot
create HASH-CELL CELL allot
create FILE-KEY-HEX 64 allot
create RESULTS-HEX 64 allot

CAST: DISPATCH-XT ( n -- [ ptr u8 n -- ] )
CAST: DISPATCH>N ( [ ptr u8 n -- ] -- n )
CAST: ACTION-XT ( n -- [ -- ] )
CAST: ORIGIN-STATE-XT ( n -- [ -- bool bool ] )
CAST: ROLE-XT ( n -- [ -- ptr u8 n bool ] )
CAST: OPAQUE-XT-TYPE ( n -- [ -- bool ] )
CAST: ORIGIN-CELL-XT ( n -- [ -- ptr u8 ] )
CAST: CUT-CELL-XT ( n -- [ -- ptr u8 ] )
CAST: CUT-SLOT ( ptr u8 -- ptr [ n -- ] )
CAST: OBSERVE-XT ( n -- [ [ ptr n -- ] -- ] )
CAST: KIND-XT ( n -- [ n n n -- n ] )
CAST: SLOTS-OBSERVE-XT ( n -- [ [ [ -- ptr n ] n n -- ] -- ] )
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
CAST: REQUEST-XT ( n -- [ -- n ptr u8 n ] )
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
\ A drained row's full path, the engine's source root joined to its own.
PATH-CAP BUFFER: DRAINED-PATH
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

: ORIGIN-HOOK-ALLOWED? ( -- bool )
   check@ {: hook:n :}
   hook 0= if true exit then
   ORIGIN-RECORD @ dup NULL-PTR = if drop false exit then
   dup NATIVE-BODY-ABI:ORIGIN-HOOK cells + CELL-VIEW @ hook =
   swap NATIVE-BODY-ABI:ORIGIN-TAKE cells + CELL-VIEW @ 0<> and ;

\ The pinned role names the checker under no hook or the engine origin's;
\ under any other hook the checker that judges is unknown.
: CHECKER-ROLE$ ( -- ptr u8 n bool )
   CHECKER-ROLE-KNOWN @ 0<> ORIGIN-HOOK-ALLOWED? and {: known:bool :}
   CHECKER-ROLE known if 32 else 0 then known ;

: PIN-CHECKER ( ptr u8 n bool -- )
   {: digest:ptr size:n known:bool :}
   known size 32 = and if
      digest CHECKER-ROLE 32 BYTE-COPY
      1 CHECKER-ROLE-KNOWN !
   else 0 CHECKER-ROLE-KNOWN ! then
   ['] CHECKER-ROLE$ NUNIT-DEP:USE-CHECKER-ROLE ;

: PIN-HOST-CHECKER ( -- )
   SOURCE-PRODUCER:CHECKER SOURCE-PRODUCER:ROLE$ PIN-CHECKER ;

\ A target build reads the driver's latest freeze, which FOLLOW-HOOK renews
\ at each later hook.
: FOLLOW-BUILD-CHECKER ( -- )
   ['] BUILD-PRODUCER:CHECKER$ NUNIT-DEP:USE-CHECKER-ROLE ;

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

\ ON-PUBLISHED keeps a re-published row's key because every cut clears the rows
\ it removes (CUT-NOTICE). A kernel without the cut cell lowers the count
\ unseen, so the observer stays unbound; every file object refuses then
\ (NUNIT-OBJECT FILE-BEGIN).
: BIND-OWNER ( -- )
   OWNER-READY? 0= if exit then
   BIND-OWNER-FACTS
   CUT-CELL @ NULL-PTR = if exit then
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
: HASH-N ( n -- )
   HASH-CELL ! HASH-CTX HASH-CELL CELL SHA256-FEED ;
: HASH-S ( ptr u8 n -- )
   {: a:ptr u:n :}
   u HASH-N
   u 0 > if HASH-CTX a u SHA256-FEED then ;
: HASH-START ( ptr u8 n -- )
   HASH-CTX SHA256-BEGIN HASH-S ;

: DIAG-N ( n -- )
   {: value:n :}
   value 10 >= if value 10 / recurse then
   s" 0123456789" drop value 10 mod + 1 NUNIT-DEP:DIAG-W2 ;

: REFUSE ( ptr u8 n ptr u8 n -- )
   {: path:ptr pathu:n reason:ptr reasonu:n :}
   s" native-cache: entry refused " NUNIT-DEP:DIAG-W2
   path pathu NUNIT-DEP:DIAG-W2
   s" : " NUNIT-DEP:DIAG-W2
   reason reasonu NUNIT-DEP:DIAG-W2
   S\" \n" NUNIT-DEP:DIAG-W2 ;

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
\ descriptor, so a same-key writer renaming a larger entry in or another
\ build discarding it mid-read cannot fail the build. A missing file is a miss; a
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
   pathu PATH-CAP > if E-FS-PATH throw then
   0 STORE-IMAGE 16 + size 16 - CHECKSUM 0 STORE-IMAGE 8 + LE:U64!
   path pathu 0 STORE-IMAGE size FS:ATOMIC-WRITE-FILE ;

: IMAGE ( ptr u8 n ptr u8 n -- n )
   {: magic:ptr magicu:n object:ptr objectu:n :}
   magicu 8 <> objectu 0 < or if BAD then
   ENTRY-HEAD objectu + {: size:n :}
   size STORE-IMAGE-RESERVE
   magic 0 STORE-IMAGE 8 BYTE-COPY
   object 0 STORE-IMAGE ENTRY-HEAD + objectu BYTE-COPY
   size ;

: FILE-PATH ( ptr u8 n -- ptr u8 n )
   dup 64 <> if BAD then
   BUILD-OUT:OBJ$ ;

\ Remove the object at path; a file that has none, or whose object another
\ build removed, is not a failure.
: DISCARD ( ptr u8 n -- )
   [: 2dup FS:REMOVE-FILE ;] catch {: a:ptr u:n code:n :}
   code 0= if exit then
   a u FS:TRY-LSTAT 0= if exit then
   code throw ;

: ZERO-SCRATCH ( ptr u8 n -- )
   {: bytes:ptr size:n :}
   size 0 ?do 0 bytes i + c! loop ;

\ Room for one more frame, the only fallible step of an open, taken before
\ the file is pushed so a failure leaves nothing to undo.
: FRAME-RESERVE ( -- ) DEPTH @ 1+ FRAME-BYTES * FRAMES-RESERVE ;

\ Open the reserved frame, zeroed as a fresh one: a file closed at this
\ depth left its record there. Its object region starts at the top.
: FILE-OPEN ( -- )
   1 DEPTH +!
   FRAME FRAME-BYTES ZERO-SCRATCH
   OBJECTS-U @ F-OBJ F! ;

: FILE-CLOSE ( -- )
   F-OBJ F@ OBJECTS-U ! -1 DEPTH +! ;

: FRAME-PATH ( -- ptr u8 n ) FRAME F-INPUT + 0 ptr-field @ F-INPUT-U F@ ;
: KEPT-KEY ( -- ptr u8 n ) FRAME F-KEY + 64 ;
: OBJECT-BYTES ( -- ptr u8 n ) F-OBJ F@ OBJECTS F-OBJ-U F@ ;

\ Whether a check hook is installed: 1 or 0.
: CHECK-STATE ( -- n ) check@ 0<> if 1 else 0 then ;

\ The ambient compile state the file key hashes: the tier, the checker state, the
\ open borrow and the dispatch role.
: AMBIENT-HASH ( -- )
   data-base NCOMP-DISPATCH:TIER-CELL + @ HASH-N
   CHECK-STATE HASH-N
   BORROW-ACTIVE @ HASH-N
   DISPATCH-ROLE 32 HASH-S ;

: OPAQUE? ( -- bool )
   OPAQUE-XT @ dup 0= if drop true exit then
   OPAQUE-XT-TYPE execute ;

: ADMISSION-REASON$ ( -- ptr u8 n )
   DEPTH @ 0= if s" no-source-frame" exit then
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

\ The buffer the interpreter reads the current definition's name token from,
\ size bytes up to the input's end (INE-CELL), and the token's span at to end
\ in it, read where the origin journal reads it (ENGINE-ORIGIN SOURCE-NAME):
\ the innermost file's source, or a text its run evaluates, from the input
\ buffer's start (SRCLOC:INB-CELL). A token in the file's source is placed
\ from the source's start, where the journal locates it (ENGINE-ORIGIN
\ LOCATION). NULL-PTR 0 0 0 when the token lies outside the buffer read.
: DEF-SPAN ( -- ptr u8 n n n )
   DEPTH @ 0= if NULL-PTR 0 0 0 exit then
   FRAME F-SRC + 0 ptr-field @ {: src:ptr :}
   src NULL-PTR = if NULL-PTR 0 0 0 exit then
   data-base PEND-CELL + @ 0= if DEF-TKA-CELL else PENDTKA-CELL then
      data-base + CELL-VIEW @ {: token:n :}
   data-base SRCLOC:INB-CELL + 0 ptr-field @ {: inb:ptr :}
   data-base INE-CELL + CELL-VIEW @ {: last:n :}
   token inb NULL-PTR BYTE-VIEW - < token last >= or if NULL-PTR 0 0 0 exit then
   src NULL-PTR BYTE-VIEW - {: first:n :}
   token first >= token first F-SRC-U F@ + < and if src else inb then
      {: buf:ptr :}
   buf NULL-PTR BYTE-VIEW - {: base:n :}
   last base - {: size:n :}
   token base - {: at:n :}
   at begin
      dup size < if buf over + c@ 32 > else false then
   while 1+ repeat {: end:n :}
   end at = if NULL-PTR 0 0 0 exit then
   buf size at end ;

: SOURCE-AT ( -- n )
   DEPTH @ 0= if -1 exit then
   F-SRC-U F@ 0= if -1 exit then
   FRAME F-SRC + 0 ptr-field @ NULL-PTR = if -1 exit then
   data-base INP-CELL + CELL-VIEW @ {: cursor:n :}
   FRAME F-SRC + 0 ptr-field @ NULL-PTR BYTE-VIEW - {: source:n :}
   cursor source - {: at:n :}
   at 0 >= at F-SRC-U F@ <= and if at else -1 then ;

\ The current definition's name start and its source text, from the name's
\ start to the input cursor past its body, in the buffer DEF-SPAN reads it
\ from; empty when the name lies outside the buffer read.
: DEF-TEXT ( -- n ptr u8 n )
   DEF-SPAN {: buf:ptr size:n at:n end:n :}
   data-base INP-CELL + CELL-VIEW @ buf NULL-PTR BYTE-VIEW - - {: body:n :}
   end 0= body end < or body size > or if 0 NULL-PTR 0 exit then
   at buf at + body at - ;

: DIAG-PATH? ( -- bool )
   DEPTH @ 0= if false exit then
   F-INPUT-U F@ 0= if false exit then
   SOURCE-AT 0 >= ;

: DIAG-FALLBACK ( ptr u8 n -- )
   {: reason:ptr reasonu:n :}
   NUNIT-DEP:DIAG? @ 0= if exit then
   s" native-cache: compile-attempt reason=" NUNIT-DEP:DIAG-W2
   reason reasonu NUNIT-DEP:DIAG-W2
   ndict@ XREF-REC XREF-NAME$ {: name:ptr nameu:n :}
   nameu 0 > if s"  name=" NUNIT-DEP:DIAG-W2 name nameu NUNIT-DEP:DIAG-W2 then
   DIAG-PATH? if
      s"  path=" NUNIT-DEP:DIAG-W2
      FRAME F-INPUT + 0 ptr-field @ F-INPUT-U F@ NUNIT-DEP:DIAG-W2
   then
   S\" \n" NUNIT-DEP:DIAG-W2 ;

: COMPILE-ORIGINAL ( ptr u8 n ptr u8 n -- )
   {: source:ptr sourceu:n reason:ptr reasonu:n :}
   reason reasonu DIAG-FALLBACK
   1 COMPILED-N +!
   source sourceu DISPATCH-ORIGINAL @ DISPATCH-XT execute ;

: COMPILE-STAGED ( -- )
   DISPATCH-SOURCE @ DISPATCH-SOURCE-U @
      STAGED-REASON @ STAGED-REASON-U @ COMPILE-ORIGINAL ;

\ A definition another one's dispatch interrupts compiles uncaptured, so its
\ file has no object.
: BUSY ( ptr u8 n -- )
   s" busy" s" " s" " NUNIT-OBJECT:REFUSE-KEEP
   s" busy" COMPILE-ORIGINAL ;

\ Compile the staged definition under capture, for reason. With store it is
\ admitted: a definition the compiler calls cacheable and free of ordered
\ dependencies is published. It joins its file's object while one is open.
: COLD-CANDIDATE ( n ptr u8 n bool -- )
   {: ord:n reason:ptr reasonu:n store:bool :}
   reason STAGED-REASON ! reasonu STAGED-REASON-U !
   ord NUNIT-DEP:COMPILING!
   NUNIT-OBJECT:DEFINITION-BEGIN
   NUNIT-OBJECT:FILE-OPEN? if NUNIT-OBJECT:CHECK-MARK then
   ['] COMPILE-STAGED catch {: rc:n :}
   rc 0<> if
      NUNIT-OBJECT:FILE-OPEN? if NUNIT-OBJECT:CHECK-DROP then
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
      admitted if NUNIT-DEP:DEFINITION-STORABLE? else false then
         {: storable:bool :}
      storable if ord NFILE-REF:PUBLISH-DEFINITION then
      ord DEF-TEXT NUNIT-OBJECT:FILE-ADD-DEFINITION
      storable 0= if
         admitted if
            s" ordered-dependency" ord NUNIT-OBJECT:REPORT-STORE-REFUSED
         then
         NFILE-REF:PENDING-RELEASE
      then
   else NFILE-REF:PENDING-RELEASE then
   NUNIT-OBJECT:FILE-OPEN? if NUNIT-OBJECT:CHECK-DROP then
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
   NFILE-REF:PENDING-KEY$ 2drop
   reasonu 0 > if
      ord reason reasonu false COLD-CANDIDATE
   else
      ord s" eligible-cache-miss" true COLD-CANDIDATE
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

: CUT-NOTICE ( n -- )
   dup NUNIT-OBJECT:FILE-CUT NFILE-REF:DICT-CUT ;

\ The open file objects follow every dictionary cut
\ through the running kernel's one capture cut cell, held for the session. A
\ kernel without it leaves the cell unbound, and every file object refuses
\ (NUNIT-OBJECT FILE-BEGIN).
: BIND-CUT ( -- )
   s" native-unit-cut-cell" NUNIT-CAPTURE:OPTIONAL-PRODUCER-XT
      dup 0= if drop exit then
   CUT-CELL-XT execute {: cell:ptr :}
   cell CELL-VIEW @ 0<> if BAD then
   ['] CUT-NOTICE cell CUT-SLOT xt!
   cell CUT-CELL ! ;

: UNBIND-CUT ( -- )
   CUT-CELL @ {: cell:ptr :}
   cell NULL-PTR = if exit then
   0 cell CELL-VIEW !
   NULL-PTR CUT-CELL ! ;

\ The token runs the host's source loader, by the loader words' original
\ entries: a file's own word named like one is not a loader.
: HOST-LOADER? ( n n n -- bool )
   SOURCE-UNIT:LOADER-KIND SOURCE-UNIT:NONE <> ;

: TARGET-LOADER? ( n n n -- bool )
   {: op:n rec:n entry:n :}
   TARGET-KIND @ 0= if false exit then
   op rec entry TARGET-KIND @ KIND-XT execute SOURCE-UNIT:NONE <> ;

: LOADER? ( n n n -- bool )
   {: op:n rec:n entry:n :}
   op rec entry HOST-LOADER? if true exit then
   op rec entry TARGET-LOADER? ;

\ The run observer's callback: a word the source runs is a row of the open
\ files, except a loader, whose action is the file's LOAD row instead
\ (LOAD-BEFORE).
: RUN-SEEN ( ptr n -- )
   {: rec:ptr :}
   SOURCE-EXEC:WORD 0 rec XREF-START LOADER? if
      rec NFILE-REF:REC-ORD NUNIT-DEP:OBSERVE-LEGS drop exit
   then
   rec NFILE-REF:REC-ORD NUNIT-DEP:OBSERVE-RUN ;

\ The open file objects see each word the source runs through the running
\ kernel's run observer, held for the session, and read its resolution legs
\ through the running checker owner. A kernel without either observes nothing,
\ and every file object refuses (NUNIT-OBJECT FILE-BEGIN). The slot words
\ TASK:+USER makes are reported to NUNIT-DEP, when the kernel's TASK reports
\ them; without it a slot word refuses as any data-base reader does.
: OBSERVE ( -- )
   NUNIT-CAPTURE:RUN-OBSERVER? 0= if exit then
   s" CHECKER-OWNER:RESOLUTION-TOP" NUNIT-CAPTURE:BOUND-PRODUCER-XT
      NUNIT-DEP:USE-TOP
   [: NUNIT-OBJECT:REFUSE-WORD ;] NUNIT-DEP:USE-REFUSE
   s" NHOST:UNOBSERVE-RUNS" NUNIT-CAPTURE:BOUND-PRODUCER-XT {: undo:n :}
   ['] RUN-SEEN
      s" NHOST:OBSERVE-RUNS" NUNIT-CAPTURE:BOUND-PRODUCER-XT OBSERVE-XT execute
   undo UNOBSERVE-XT !
   s" TASK:OBSERVE-SLOTS" NUNIT-CAPTURE:OPTIONAL-PRODUCER-XT dup 0<> if
      [: NUNIT-DEP:USER-MADE ;] swap SLOTS-OBSERVE-XT execute
   else drop then ;

: UNOBSERVE ( -- )
   UNOBSERVE-XT @ {: undo:n :}
   s" TASK:UNOBSERVE-SLOTS" NUNIT-CAPTURE:OPTIONAL-PRODUCER-XT dup 0<> if
      ACTION-XT execute
   else drop then
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

: FILE-INPUT ( ptr u8 n -- ) s" " NFILE-INPUTS:ADD-CTIME ;

\ The innermost reuse run installs no more definitions, and the rest of the
\ file compiles live: its loads left the order its object holds them in (a
\ word that loads a child loads none when another file loaded it first, and
\ a failed load has no LOAD row). OBJECT-CLOSE then stores the object again
\ or, as a cold build does, refuses and removes it.
: LOAD-ORDER ( -- )
   F-MISS F@ 1+ F-MISS F!
   0 F-REUSE F! ;

\ The innermost file's pending load action of load kind kind and requested
\ path returned, resuming at source offset off, ok or not
\ (NUNIT-OBJECT:LOAD-FINISH); a reuse run views its object again and,
\ when ok and its object's next LOAD row is this load, maps its next
\ segment, else stops installing (LOAD-ORDER).
: LOAD-END ( n ptr u8 n n bool -- )
   {: kind:n path:ptr pathu:n off:n ok:bool :}
   kind path pathu off ok NUNIT-OBJECT:LOAD-FINISH
   F-REUSE F@ 0= if exit then
   OBJECT-BYTES KEPT-KEY NUNIT-OBJECT:VIEW-FILE
   ok 0= if LOAD-ORDER exit then
   F-LOADS F@ kind path pathu off NUNIT-OBJECT:LOAD-HELD? 0= if
      LOAD-ORDER exit
   then
   F-LOADS F@ NUNIT-OBJECT:EV-AFTER-LOAD NUNIT-OBJECT:SEGMENT-MAP
   1 F-LOADS F@ + F-LOADS F! ;

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

: FRAME-REPORT ( -- )
   NUNIT-DEP:DIAG? @ 0= if exit then
   FRAME F-INPUT + 0 ptr-field @ F-INPUT-U F@ NUNIT-DEP:FILE-REPORT ;

: ROWS-FLUSH ( -- )
   F-MARK F@ NFILE-INPUTS:MARK NFILE-INPUTS:SPAN
   FRAME F-ROOT + 0 ptr-field @ F-ROOT-U F@ NFILE-INPUTS:TREE-OUT
      NUNIT-OBJECT:FILE-INPUTS+
   NFILE-INPUTS:MARK F-MARK F! ;

: SOURCE-ROW ( -- )
   FRAME-PATH FRAME F-RAW + 0 ptr-field @ F-RAW-U F@
      NFILE-INPUTS:ADD-SOURCE ;

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
\ a harvest segment opened under it refuses its file (ORIGIN-MARK), and no
\ SEGMENT row records it, so a head seeking under it misses (HEAD-SEEK).
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
\ declaration, which a warm run never installs from an object: REUSE-HEAD
\ installs `:` and TRUSTED: definitions only, so installing that kind must
\ first make SINK read it.
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
\ state, and the file's tree-relative instance key (NFILE-REF:PUSH-FILE): its
\ parent, its role, its path relative to its root, its kind and its
\ occurrence under that parent. The object's stored declaration and import
\ keys start with the instance key, so a key that omitted the loader would
\ find an object whose keys this load cannot reproduce. Its input rows are
\ stored relative to the file's root and the engine's source root
\ (NFILE-INPUTS:TREE-OUT), so an identical tree at another path shares the
\ object and a differing one fails its rows.
: FILE-KEY ( -- ptr u8 n )
   ENGINE-ID:KEY$ {: engine:ptr engineu:n :}
   s" habu-native-file" HASH-START
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

\ The innermost file's origin rows cannot be harvested, for cause: it
\ refuses and compiles live.
: ORIGIN-SINK-REFUSE ( ptr u8 n -- )
   s" origin-sink" 2swap s" " NUNIT-OBJECT:REFUSE-KEEP ;

\ Opens the innermost file's harvest segment under the live sink, which its
\ object segment records for a warm install; an unsupported sink refuses the file.
: ORIGIN-MARK ( -- )
   SINK {: kind:n fed:bool :}
   kind F-OSINK F!
   fed if 1 else 0 then F-OFED F!
   PRE-BOUND @ 0<> if PRE-MARK-HOOK @ execute drop else 0 then F-OCUR F!
   EARLY-MARK drop F-OSER F!
   kind SINK-UNSUPPORTED = if s" unsupported" ORIGIN-SINK-REFUSE exit then
   kind NUNIT-OBJECT:ORIGIN-SINK! ;

\ The open file's sink, cursors and first record, stored as it opens.
: ORIGIN-OPEN ( -- )
   ORIGIN-MARK
   ndict@ F-OFIRST F! ;

\ The checker symbol of the record at ord, derived from the package and
\ visibility of its wordlist and its own name there: a qualified definition's
\ row names it with the qualifier its record does not hold. False for a
\ retired record or without a ready checker owner.
: ROW-SYM ( n -- n bool )
   {: ord:n :}
   OWNER-READY? 0= if 0 false exit then
   ord XREF-REC {: rec:ptr :}
   rec NFILE-REF:PACKAGE-OF {: pkg:ptr pkgu:n vis:n :}
   vis PKG-RETIRED = if 0 false exit then
   vis PKG-PUBLIC = if SYM-PUBLIC else
      vis PKG-PRIVATE = if SYM-PRIVATE else 0 then
   then {: sym:n :}
   pkg pkgu sym rec XREF-NAME$ LIVE-ORIGIN true ;

\ The records made since the closing file opened, its children's included,
\ whose code starts at xt and which the row of symbol sym may name, and the
\ last of them: a live record holding sym, or a retired record. An EXPORT
\ shares its word's xt, never its symbol. A retired record's wordlist no
\ longer gives its symbol, so its row keeps the one the journal recorded while
\ it was live. A harvest reads only the rows its segment recorded (a child's
\ open harvests its parent's first), so a retired record such a row names was
\ made and retired by that segment's own top level: a warm load makes the
\ word live from its head, installs the row, and the same `undefine` retires
\ it again.
: FULL-ORD ( n n -- n n )
   {: xt:n sym:n :}
   0 0
   ndict@ F-OFIRST F@ ?do
      i XREF-REC XREF-START xt = if
         i ROW-SYM swap sym = and i XREF-REC XREF-RETIRED? or
            if drop i swap 1+ swap then
      then
   loop
   {: hits:n ord:n :}
   ord hits ;

\ The innermost file's origin row of name cannot be stored, for cause: it
\ refuses and compiles live.
: ORIGIN-REFUSE ( ptr u8 n ptr u8 n -- )
   {: cause:ptr causeu:n name:ptr nameu:n :}
   s" origin-record" cause causeu name nameu NUNIT-OBJECT:REFUSE-KEEP ;

\ The one record of the closing file at xt that the row of name and symbol
\ sym names, live holding sym or retired; when there is none or more, the
\ innermost file refuses.
: FULL-REC? ( n n ptr u8 n -- n bool )
   {: xt:n sym:n name:ptr nameu:n :}
   xt sym FULL-ORD {: ord:n hits:n :}
   hits 0= if s" symbol" name nameu ORIGIN-REFUSE ord false exit then
   hits 1 > if s" xt" name nameu ORIGIN-REFUSE ord false exit then
   ord true ;

: PREFIX-ROW ( n n n ptr u8 n ptr u8 n -- )
   1 HARVEST-N +! NUNIT-OBJECT:ORIGIN+ ;

\ An early journal row: a fed row has no symbol, any other the live
\ record's, or for a retired record the one the row holds (FULL-ORD).
: EARLY-ROW ( n n n n ptr u8 n ptr u8 n n n -- )
   {: serial:n phase:n ord:n sym:n path:ptr pathu:n name:ptr nameu:n
      at:n end:n :}
   1 HARVEST-N +!
   HARVEST-FED @ 0<> if
      sym 0<> if s" fed-symbol" name nameu ORIGIN-REFUSE exit then
   else
      ord XREF-REC XREF-RETIRED? 0= if
         ord ROW-SYM {: derived:n known:bool :}
         known 0= if s" no-symbol" name nameu ORIGIN-REFUSE exit then
         derived sym <> if s" symbol" name nameu ORIGIN-REFUSE exit then
      then
   then
   ord at end path pathu name nameu NUNIT-OBJECT:ORIGIN+ ;

\ A full journal row names its definition by xt: the file's one record there.
: FULL-ROW ( n n n n ptr u8 n ptr u8 n n n -- )
   {: serial:n phase:n xt:n sym:n path:ptr pathu:n name:ptr nameu:n
      at:n end:n :}
   1 HARVEST-N +!
   xt sym name nameu FULL-REC? {: ord:n found:bool :}
   found 0= if exit then
   ord at end path pathu name nameu NUNIT-OBJECT:ORIGIN+ ;

\ A full journal row the file's own top level drained from the early journal
\ (ENGINE-ORIGIN:FILE-DRAIN): a warm load installs it as the early row it
\ was, and the same drain makes it this row again. The drain resolved a
\ missing symbol and made the path relative to the engine's source root: a
\ live record must hold the row's symbol, and the row keeps the full path the
\ early journal held.
: DRAINED-ROW ( n n n n ptr u8 n ptr u8 n n n -- )
   {: serial:n phase:n xt:n sym:n path:ptr pathu:n name:ptr nameu:n
      at:n end:n :}
   1 HARVEST-N +!
   xt sym name nameu FULL-REC? {: ord:n found:bool :}
   found 0= if exit then
   pathu 0= if ord at end path pathu name nameu NUNIT-OBJECT:ORIGIN+ exit then
   s" SOURCE-ROOT:ENGINE$" NUNIT-ORIGIN:JOURNAL-XT {: root-xt:n :}
   root-xt 0= if s" no-root" name nameu ORIGIN-REFUSE exit then
   root-xt ROOT-XT execute {: root:ptr rootu:n :}
   rootu 1+ pathu + {: fullu:n :}
   fullu PATH-CAP > if E-FS-CAPACITY throw then
   root DRAINED-PATH rootu BYTE-COPY
   47 DRAINED-PATH rootu + c!
   path DRAINED-PATH rootu 1+ + pathu BYTE-COPY
   ord at end DRAINED-PATH fullu name nameu NUNIT-OBJECT:ORIGIN+ ;

\ Visit the journal rows of word name past serial mark.
: JOURNAL-ROWS ( n [ n n n n ptr u8 n ptr u8 n n n -- ] ptr u8 n -- )
   NUNIT-ORIGIN:JOURNAL-XT {: mark:n visit rows:n :}
   rows 0= if exit then
   mark visit rows ROWS-XT execute ;

\ The innermost file's harvest segment recorded a row since its cursors:
\ both journals share the early journal's serial, and the builder's prefix
\ rows have their own count.
: ORIGIN-ROWS? ( -- bool )
   EARLY-MARK drop F-OSER F@ <> if true exit then
   PRE-BOUND @ 0= if false exit then
   PRE-MARK-HOOK @ execute drop F-OCUR F@ <> ;

\ Store the rows the innermost file's closing harvest segment recorded, as a
\ load action or the file's end closes it: those its sink recorded since the
\ cursors, under the same sink. A child's rows lie between two segments'
\ and are its own: the cursors move past them when it ends, and the next
\ segment opens under the sink live then. A segment whose sink changed
\ while open holds rows of neither sink alone, so it refuses when it
\ recorded any. The journal's serial advances once per row the segment
\ recorded, and not at all for a file whose rows go elsewhere.
: ORIGIN-HARVEST ( -- )
   SINK {: kind:n fed:bool :}
   fed if 1 else 0 then {: fedn:n :}
   kind F-OSINK F@ <> fedn F-OFED F@ <> or if
      ORIGIN-ROWS? if s" changed" ORIGIN-SINK-REFUSE then exit
   then
   0 HARVEST-N !
   fedn HARVEST-FED !
   kind NUNIT-OBJECT:SINK-PREFIX = if
      PRE-MARK-HOOK @ execute drop F-OCUR F@ <
         if s" prefix-rewound" ORIGIN-SINK-REFUSE exit then
      F-OCUR F@ ['] PREFIX-ROW PRE-EACH-HOOK @ execute
   then
   kind NUNIT-OBJECT:SINK-EARLY = if
      F-OSER F@ ['] DRAINED-ROW s" ENGINE-ORIGIN:FILE-ROWS" JOURNAL-ROWS
      F-OSER F@ ['] EARLY-ROW s" ENGINE-ORIGIN-EARLY:FILE-ROWS" JOURNAL-ROWS
   then
   kind NUNIT-OBJECT:SINK-FULL = if
      F-OSER F@ ['] FULL-ROW s" ENGINE-ORIGIN:FILE-ROWS" JOURNAL-ROWS
   then
   kind NUNIT-OBJECT:SINK-EARLY >= if HARVEST-N @ else 0 then
   EARLY-MARK drop F-OSER F@ - <> if s" count" ORIGIN-SINK-REFUSE then ;

\ Open the object of the file just pushed; its first row is its own source's
\ stat and digest. Rows join an object only at ROWS-FLUSH, so the row recorded before
\ FILE-BEGIN is still this file's, and a throw leaves no object to end.
: OBJECT-OPEN ( -- )
   FILE-KEY {: key:ptr keyu:n :}
   key FRAME F-KEY + keyu BYTE-COPY
   NFILE-INPUTS:MARK F-MARK F!
   SOURCE-ROW
   NUNIT-OBJECT:FILE-BEGIN
   ORIGIN-OPEN
   NUNIT-OBJECT:DEF-OPEN? 0= if
      s" def-open-absent" s" " s" " NUNIT-OBJECT:REFUSE-KEEP
   then
   \ The install's writers are tier-1-only (prims.f def-open/def-close): a
   \ file compiled at another tier holds no definition and its object would
   \ be rewritten on every warm build, so it stores none.
   data-base NCOMP-DISPATCH:TIER-CELL + @ {: tier:n :}
   tier 1 <> if
      NUNIT-DEP:DIAG? @ if
         s" native-cache: tier value=" NUNIT-DEP:DIAG-W2 tier DIAG-N
         s"  path=" NUNIT-DEP:DIAG-W2 FRAME-PATH NUNIT-DEP:DIAG-W2
         S\" \n" NUNIT-DEP:DIAG-W2
      then
      s" tier" s" " s" " NUNIT-OBJECT:REFUSE-KEEP
   then
   \ An open shadow files a second machine's routine for each definition as
   \ it compiles (src/compiler/native/shadow.f). A hit installs the records
   \ without compiling them and an object carries no shadow routine, so the
   \ link of a reused file's records would find none: the file compiles live.
   NSHADOW:OPEN? if s" shadow-open" s" " s" " NUNIT-OBJECT:REFUSE-KEEP then ;

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

: FILE-LINE ( ptr u8 n ptr u8 n -- )
   {: label:ptr labelu:n why:ptr whyu:n :}
   s" native-file: " NUNIT-DEP:DIAG-W2 label labelu NUNIT-DEP:DIAG-W2
   s"  path=" NUNIT-DEP:DIAG-W2 FRAME-PATH NUNIT-DEP:DIAG-W2
   s"  reason=" NUNIT-DEP:DIAG-W2 why whyu NUNIT-DEP:DIAG-W2
   S\" \n" NUNIT-DEP:DIAG-W2 ;

: REFUSED-LINE ( ptr u8 n -- )
   {: why:ptr whyu:n :}
   NUNIT-DEP:DIAG? @ 0= if exit then
   s" refused" why whyu FILE-LINE ;

\ Write the innermost file's object and read it back on every build; the
\ cache diagnostics only print what was written or why the file has none. A
\ refused file's earlier object is removed, so no later build reads it.
: OBJECT-CLOSE ( -- )
   ROWS-FLUSH
   ORIGIN-HARVEST
   NUNIT-OBJECT:FILE-ENCODE
   NUNIT-OBJECT:FILE-REFUSED? if
      NUNIT-OBJECT:REASON$ REFUSED-LINE KEPT-KEY FILE-PATH DISCARD exit
   then
   KEPT-KEY {: key:ptr keyu:n :}
   key keyu NUNIT-OBJECT:PACK-FILE {: obj:ptr obju:n :}
   s" HBFOBJ01" obj obju IMAGE {: size:n :}
   key keyu FILE-PATH size IMAGE-WRITE
   key keyu FILE-PATH s" HBFOBJ01" ENTRY-HEAD CACHE-READ
      {: got:n found:bool :}
   found 0= got size <> or if BAD then
   0 ENTRY ENTRY-HEAD + size ENTRY-HEAD - key keyu F-SRC-U F@
      NUNIT-OBJECT:OPEN-FILE
   NUNIT-OBJECT:ORIGIN-FORMED? 0= if BAD then
   NUNIT-OBJECT:FILE-COUNTS
      {: defs:n inputs:n recs:n data:n loads:n segs:n checks:n :}
   NUNIT-DEP:DIAG? @ 0= if exit then
   s" native-file: object path=" NUNIT-DEP:DIAG-W2 FRAME-PATH NUNIT-DEP:DIAG-W2
   s"  key=" NUNIT-DEP:DIAG-W2 key keyu NUNIT-DEP:DIAG-W2
   s"  defs=" NUNIT-DEP:DIAG-W2 defs DIAG-N
   s"  inputs=" NUNIT-DEP:DIAG-W2 inputs DIAG-N
   s"  records=" NUNIT-DEP:DIAG-W2 recs DIAG-N
   s"  data=" NUNIT-DEP:DIAG-W2 data DIAG-N
   s"  loads=" NUNIT-DEP:DIAG-W2 loads DIAG-N
   s"  segments=" NUNIT-DEP:DIAG-W2 segs DIAG-N
   s"  checks=" NUNIT-DEP:DIAG-W2 checks DIAG-N
   s"  bytes=" NUNIT-DEP:DIAG-W2 size DIAG-N
   s"  results=" NUNIT-DEP:DIAG-W2 defs RESULTS$ NUNIT-DEP:DIAG-W2
   s"  src=" NUNIT-DEP:DIAG-W2
   defs 0 ?do
      i 0 > if s" ," NUNIT-DEP:DIAG-W2 then
      i NUNIT-OBJECT:FILE-SPAN DIAG-N
   loop
   S\" \n" NUNIT-DEP:DIAG-W2 ;


\ Check the object's framing, then view its FILE sections: OPEN-FILE ends
\ by releasing its view.
: OPEN-OBJECT ( -- )
   OBJECT-BYTES KEPT-KEY F-SRC-U F@ NUNIT-OBJECT:OPEN-FILE
   OBJECT-BYTES NUNIT-FILE:ARCH-AARCH64 s" FILE" KEPT-KEY
      NUNIT-FILE:READ-VIEW ;

\ A miss the hit check names: the viewed object is dropped and the file
\ loads from source.
: OBJECT-MISS ( ptr u8 n -- )
   {: why:ptr whyu:n :}
   NUNIT-FILE:RESET-KEEP
   NUNIT-DEP:DIAG? @ 0= if exit then
   s" native-file: miss path=" NUNIT-DEP:DIAG-W2 FRAME-PATH NUNIT-DEP:DIAG-W2
   s"  reason=" NUNIT-DEP:DIAG-W2 why whyu NUNIT-DEP:DIAG-W2
   S\" \n" NUNIT-DEP:DIAG-W2 ;

\ Read the innermost file's object into its OBJECTS region and view it; a
\ missing, damaged, empty or malformed one is a miss, and a malformed one is
\ named.
: OBJECT-READ? ( -- bool )
   KEPT-KEY FILE-PATH s" HBFOBJ01" ENTRY-HEAD CACHE-READ {: size:n found:bool :}
   found 0= if false exit then
   size ENTRY-HEAD - {: u:n :}
   u 0= if false exit then
   F-OBJ F@ {: at:n :}
   at u + 7 + -8 and {: top:n :}
   top OBJECTS-RESERVE
   0 ENTRY ENTRY-HEAD + at OBJECTS u BYTE-COPY
   u F-OBJ-U F!
   top OBJECTS-U !
   ['] OPEN-OBJECT catch {: rc:n :}
   rc 0= if true exit then
   rc E-NUNIT-PROFILE = rc E-NUNIT-FILE = or if
      s" object-format" OBJECT-MISS false exit
   then
   rc throw ;

\ Input rows of the viewed object hold for this session.
: INPUTS-FRESH? ( ptr u8 n -- bool )
   FRAME F-ROOT + 0 ptr-field @ F-ROOT-U F@ NFILE-INPUTS:TREE-IN
   NFILE-INPUTS:FRESH? ;

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

\ The active sink can take installed origin rows: a journal sink has its
\ import and, for rows that carry a symbol, a ready checker owner to derive
\ it. A row installs only under the sink its segment recorded
\ (NUNIT-OBJECT:HEAD-SEEK).
: ORIGIN-SINK? ( -- bool )
   SINK {: kind:n fed:bool :}
   kind NUNIT-OBJECT:SINK-EARLY < if true exit then
   fed 0= OWNER-READY? 0= and if false exit then
   kind IMPORT-XT 0<> ;

\ The sink kind the row being installed recorded under, the live one, 1
\ when its rows are fed, and the journal import its rows go through.
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
   IMPORT-FED @ 0<> if 0 else ord ROW-SYM drop then {: sym:n :}
   IMPORT-KIND @ NUNIT-OBJECT:SINK-EARLY = if
      ord sym at end path pathu name nameu IMPORT-AT @ EARLY-IMPORT-XT execute
      exit
   then
   sym ord path pathu name nameu at end IMPORT-AT @ FULL-IMPORT-XT execute ;

\ Set the import for the rows of the definition being installed: the live
\ sink kind, whether the sink is fed, and the journal import.
: ORIGIN-SETUP ( -- )
   SINK {: kind:n fed:bool :}
   kind IMPORT-KIND !
   fed if 1 else 0 then IMPORT-FED !
   kind IMPORT-XT IMPORT-AT ! ;

\ ---- reuse: a warm file runs its source, installing at each head ------------
\ (NUNIT-OBJECT INSTALL-ROW). Every `:` and TRUSTED: head of a file's run
\ calls HEAD-HOOK through NCOMP-DISPATCH:BODY-HOOK-CELL: the engine's
\ (src/habu/habu2.f C-BODY-HOOK) and OUTER's (src/habu/definers.f DEF-HEAD).

CAST: HOOK>N ( [ -- ptr u8 bool ] -- n )

\ A miss of the warm object leaves no mark and its body compiles live; it counts
\ in F-MISS unless it is a live head (LOAD-ORDER counts its own).
: REUSE-HEAD ( -- ptr u8 bool )
   DEF-SPAN {: buf:ptr size:n at:n end:n :}
   end 0= if NULL-PTR false exit then
   buf size at SINK drop NUNIT-OBJECT:HEAD-SEEK
      {: len:n hit:bool live:bool :}
   live if NULL-PTR false exit then
   hit ORIGIN-SINK? and 0= if F-MISS F@ 1+ F-MISS F! NULL-PTR false exit then
   F-LOADS F@ NUNIT-OBJECT:LOAD-PASSED? if LOAD-ORDER NULL-PTR false exit then
   buf at + size at - NUNIT-OBJECT:HEAD-PREPARE? 0= if
      F-MISS F@ 1+ F-MISS F! NULL-PTR false exit
   then
   NUNIT-DEP:CLOSE
   ORIGIN-SETUP
   NUNIT-OBJECT:INSTALL-ROW {: ord:n rel:n :}
   rel at ['] ORIGIN-IMPORT NUNIT-OBJECT:ORIGIN-OF
   ord at NUNIT-OBJECT:FILE-ADD-INSTALLED
   NUNIT-OBJECT:CHECK-DROP
   NUNIT-OBJECT:DISCARD-DEFINITION
   1 INSTALLED-N +!
   buf at + len + true ;

\ The pending definition's `;` reaches DISPATCH-DEFINITION: only tier 1's `;`
\ (src/habu/habu2.f EM-COMPILE) and OUTER's (src/habu/definers.f DEF-COMPILE)
\ call the dispatch cell, at the tier the head latched in DEF-TIER-CELL; the
\ cell holds DISPATCH-GUARDED unless SUSPEND-DISPATCH gave it back to the
\ compiler; DISPATCH-GUARDED hands a definition made while a dispatch runs to
\ BUSY. Inside a file's run its object is open, so WRAPPED-DISPATCH goes on to
\ DISPATCH-DEFINITION for any admission reason.
: DISPATCHED? ( -- bool )
   data-base NCOMP-DISPATCH:DEF-TIER-CELL + @ 1 =
   data-base NCOMP-DISPATCH:XT-CELL + CELL-VIEW @
      ['] DISPATCH-GUARDED DISPATCH>N = and
   DISPATCH-BUSY @ 0= and ;

\ Every `:` or TRUSTED: head while a file runs whose `;` reaches the
\ dispatch: open the head's definition frame, so what runs while its body is
\ read is the definition's rows, then, in a warm file, try the object
\ (REUSE-HEAD).
\ COLD-CANDIDATE compiles the body in the frame; one the engine abandoned on a
\ throw closes at the next head or file boundary (NUNIT-DEP:BEGIN-DEFINITION).
\ Any other head, such as one a definition's compile reads (DISPATCH-BUSY),
\ compiles live, as the cold build compiles it without a row, and top-level
\ runs after its `;` stay the file's rows.
: HEAD-HOOK ( -- ptr u8 bool )
   DEPTH @ 0= if NULL-PTR false exit then
   DISPATCHED? 0= if NULL-PTR false exit then
   ndict@ NUNIT-DEP:BEGIN-DEFINITION
   F-REUSE F@ 0= if NULL-PTR false exit then
   REUSE-HEAD ;

: HOOK-CELL ( -- ptr n ) data-base NCOMP-DISPATCH:BODY-HOOK-CELL + CELL-VIEW ;

: UNIT-RUN ( [ n n -- ] -- )
   {: q :}
   FRAME F-INPUT + 0 ptr-field @ F-INPUT-U F@
   FRAME F-ROOT + 0 ptr-field @ F-ROOT-U F@
   FRAME F-SRC + 0 ptr-field @ F-SRC-U F@
   q F-PRIOR F@ N>UNIT execute ;

\ The observer's before callback. A loader token is never a run: at a loader
\ cut within the file's source its action starts (NUNIT-OBJECT:LOAD-ACTION),
\ and the ticket is 1, plus 2 when the target's loader runs it; anywhere
\ else it refuses the file, and the ticket is 0. Any other token's ticket is
\ RUN-BEFORE's doubled.
: LOAD-BEFORE ( n n n n ptr u8 n -- n bool )
   {: unit:n op:n rec:n entry:n tok:ptr toku:n :}
   op rec entry LOADER? if
      SOURCE-EXEC:LOADER-CUT? SOURCE-AT 0 >= and if
         SOURCE-AT NUNIT-OBJECT:LOAD-ACTION
         op rec entry HOST-LOADER? if 1 else 3 then false exit
      then
      s" load-boundary" s" " s" " NUNIT-OBJECT:REFUSE-KEEP
      0 false exit
   then
   unit op rec entry tok toku NUNIT-DEP:RUN-BEFORE {: ticket:n judged:bool :}
   ticket 2 * judged ;

\ The observer's after callback: RUN-AFTER with RUN-BEFORE's ticket, or a
\ loader action's end with the loader's request, ok when it returned with
\ the file balanced at the position it resumes from. The file's next
\ segment opens, under the sink live then: the harvest closes at a load that
\ brought in no child too. A child's unit has already harvested the segment
\ before it and marked past the child's rows (FILE-UNIT-WITH), so after a
\ child this harvest finds no row.
: LOAD-AFTER ( n n n -- )
   {: unit:n ticket:n rc:n :}
   ticket 1 and 0= if unit ticket 1 rshift rc NUNIT-DEP:RUN-AFTER exit then
   ticket 2 and 0<> if
      s" SOURCE-UNIT:ACTION$" TARGET-XT REQUEST-XT execute
   else SOURCE-UNIT:ACTION$ then {: kind:n path:ptr pathu:n :}
   SOURCE-EXEC:POSITION {: off:n balanced:bool :}
   kind path pathu off rc 0= balanced and LOAD-END
   ORIGIN-HARVEST ORIGIN-MARK ;

\ Run the file's source through run under the engine's source observer,
\ whose callbacks judge each seed word the top level runs (NUNIT-DEP
\ RUN-BEFORE, RUN-AFTER) and each loader action it takes (LOAD-BEFORE,
\ LOAD-AFTER). The frame arms when the loader evaluates exactly the source
\ span; a child load pushes its own frame.
: SOURCE-RUN ( [ -- ] -- )
   {: run :}
   FRAME F-SRC + 0 ptr-field @ F-SRC-U F@ {: source:ptr sourceu:n :}
   DEPTH @ source sourceu ['] LOAD-BEFORE ['] LOAD-AFTER run
      SOURCE-EXEC:RUN {: rc:n :}
   rc 0<> if rc throw then ;

: OBSERVED-RUN ( -- ) [: F-Q F@ N>Q UNIT-RUN ;] SOURCE-RUN ;

\ Run the innermost file's source with its object open for REUSE-HEAD. A run
\ in which any head missed or whose installs stopped (LOAD-ORDER) counts the
\ file compiled, and the driver closes its object again (OBJECT-CLOSE).
: REUSE-RUN ( -- )
   OBJECT-BYTES KEPT-KEY NUNIT-OBJECT:INSTALL-BEGIN
   0 NUNIT-OBJECT:SEGMENT-MAP
   1 F-REUSE F! 0 F-LOADS F! 0 F-MISS F!
   NUNIT-OBJECT:ORIGIN-INDEX
   ['] OBSERVED-RUN catch {: rc:n :}
   F-REUSE F@ 0= {: order:bool :}
   0 F-REUSE F!
   NUNIT-OBJECT:INSTALL-STOP
   rc 0<> if rc throw then
   F-MISS F@ 0<> if
      order if s" load-order" else s" head-stale" then REFUSED-LINE exit
   then
   1 REUSED-N +!
   NUNIT-DEP:DIAG? @ 0= if exit then
   s" native-file: reused path=" NUNIT-DEP:DIAG-W2
   FRAME-PATH NUNIT-DEP:DIAG-W2 S\" \n" NUNIT-DEP:DIAG-W2 ;

\ A file installing through its object keeps its install place across a
\ child's unit, whose reuse run begins its own install over it: a loader
\ cut and a word the file's top level runs (a `required` inside a run
\ definition) load a child alike. The file views its object and indexes its
\ origin rows again after the child.
: PLACE-SAVE ( -- )
   F-REUSE F@ 0= if exit then
   NUNIT-OBJECT:INSTALL-SAVE
   F-SAVE 32 + F! F-SAVE 24 + F! F-SAVE 16 + F! F-SAVE 8 + F! F-SAVE F! ;

: PLACE-RESTORE ( -- )
   F-REUSE F@ 0= if exit then
   F-SAVE F@ F-SAVE 8 + F@ F-SAVE 16 + F@ F-SAVE 24 + F@ F-SAVE 32 + F@
      NUNIT-OBJECT:INSTALL-RESTORE
   OBJECT-BYTES KEPT-KEY NUNIT-OBJECT:VIEW-FILE
   NUNIT-OBJECT:ORIGIN-INDEX ;

\ The innermost file has an object whose installed rows the live origin sink
\ is ready to take (ORIGIN-SINK?; each row's own sink is checked at its head,
\ NUNIT-OBJECT:HEAD-SEEK), and whose input rows, the file's own source row
\ included, all hold. A stale object never installs, since every read checks
\ its inputs: the file compiles live, whole, and a successful run writes the
\ object again or, refused, removes it.
: REUSE-HIT? ( -- bool )
   OBJECT-READ? 0= if false exit then
   NUNIT-OBJECT:ORIGIN-FORMED? 0= if s" origin-format" OBJECT-MISS false exit then
   ORIGIN-SINK? 0= if s" origin-sink" OBJECT-MISS false exit then
   11 NUNIT-FILE:SECTION$ INPUTS-FRESH? 0= if
      s" object-stale" OBJECT-MISS false exit
   then
   true ;

: FILE-UNIT-WITH ( ptr u8 n ptr u8 n ptr u8 n [ n n -- ] [ ptr u8 n ptr u8 n ptr u8 n [ n n -- ] -- ] bool -- )
   {: path:ptr pathu:n root:ptr rootu:n source:ptr sourceu:n q prior target:bool :}
   SESSION @ 0= if
      path pathu root rootu source sourceu q prior execute exit
   then
   DEPTH @ 0= if 0 NULL-PTR 0
   else
      target if
         s" SOURCE-UNIT:PARENT-ACTION$" TARGET-XT
            REQUEST-XT execute
      else SOURCE-UNIT:PARENT-ACTION$ then
   then {: kind:n asked:ptr askedu:n :}
   \ A child the parent's top level loads from inside a word it runs, with
   \ no definition open, starts the parent's load action at the position of
   \ the token whose run is pending; the action ends after the child closes.
   DEPTH @ 0 > if
      NUNIT-OBJECT:LOAD-PENDING? 0= SOURCE-AT 0 >= and
         data-base PEND-CELL + @ 0= and
   else false then {: run:bool :}
   run if SOURCE-AT else -1 then {: at:n :}
   run if at NUNIT-OBJECT:LOAD-ACTION then
   \ The loader's frame still holds the bytes as read; `source` is the copy
   \ the interpreter runs.
   target if
      s" SOURCE-ROOT:ACTIVE-SOURCE" TARGET-XT ACTIVE-SOURCE-XT execute
   else SOURCE-ROOT:ACTIVE-SOURCE then
      {: raw:ptr rawu:n :} 2drop 2drop
   \ The rows the parent recorded so far join its object before the child's
   \ opens.
   DEPTH @ 0 > if ROWS-FLUSH ORIGIN-HARVEST PLACE-SAVE then
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
   ['] OBJECT-OPEN catch {: open-rc:n :}
   open-rc 0<> if
      FILE-CLOSE
      NFILE-REF:POP-FILE open-rc throw
   then
   1 FILES-N +!
   NUNIT-DEP:BEGIN-UNIT
   NUNIT-OBJECT:SCRATCH-RETIRE
   \ Both paths observe the file's source: an engine build that installs it
   \ keeps the producer digests a run would.
   NATIVE? @ if path pathu root rootu source sourceu BUILD-PRODUCER:OBSERVE then
   REUSE-HIT? {: hit:bool :}
   \ The head hook runs for this file's heads and is restored for the loader.
   HOOK-CELL @ {: hook:n :}
   ['] HEAD-HOOK HOOK>N HOOK-CELL !
   hit if ['] REUSE-RUN else ['] OBSERVED-RUN then catch {: unit-rc:n :}
   hook HOOK-CELL !
   \ A reuse run installs each checker plan it prepares; one that threw
   \ between a preparation and its install leaves that plan for this release.
   NUNIT-OBJECT:RELEASE-CHECKER
   ['] NUNIT-OBJECT:DEFINITIONS-CLOSE catch {: plan-rc:n :}
   ['] FRAME-REPORT catch {: report-rc:n :}
   \ A reuse run in which a head missed or whose installs stopped
   \ (LOAD-ORDER) closes its object again.
   unit-rc 0= plan-rc 0= and hit 0= F-MISS F@ 0<> or and
      if ['] OBJECT-CLOSE catch else 0 then
      {: object-rc:n :}
   NUNIT-OBJECT:FILE-END
   NUNIT-DEP:CLOSE-UNIT
   unit-rc 0= plan-rc 0= and
      if ['] FILE-SUCCESS catch else 0 then
      {: finish-rc:n :}
   FILE-CLOSE
   \ The rows recorded so far belong to the closed file's object. The
   \ parent's next harvest segment opens under the sink live now, the one
   \ the object segment its load action's end opens records.
   DEPTH @ 0 > if NFILE-INPUTS:MARK F-MARK F! ORIGIN-MARK PLACE-RESTORE then
   ['] NFILE-REF:POP-FILE catch {: pop-rc:n :}
   \ The parent resumes inside the word that loaded, ok when the child closed
   \ without a throw.
   run if
      kind asked askedu at
      unit-rc plan-rc or report-rc or object-rc or finish-rc or pop-rc or 0=
      LOAD-END
   then
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
   s" SOURCE-UNIT:LOADER-KIND" TARGET-XT TARGET-KIND !
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
   s"  compiled " type FILES-N @ REUSED-N @ - .
   s"  definitions installed " type INSTALLED-N @ .
   s"  compiled " type COMPILED-N @ . ;

: COUNTS-PATH-READ ( -- )
   s" HABU_BUILD_COUNTS" GETENV {: a:ptr u:n :}
   0 COUNTS-PATH-U !
   u 0= if exit then
   u PATH-CAP > if E-FS-CAPACITY throw then
   a COUNTS-PATH u BYTE-COPY
   u COUNTS-PATH-U ! ;

: COUNTS-WRITE ( -- )
   COUNTED? @ 0= COUNTS-PATH-U @ 0= or if exit then
   SB-RESET
   REUSED-N @ FMT:SB-U STR-SPACE SB-APPEND-C
   FILES-N @ REUSED-N @ - FMT:SB-U 10 SB-APPEND-C
   COUNTS-PATH COUNTS-PATH-U @ SB$ FS:ATOMIC-WRITE-FILE ;

: CLEAR-SCRATCH ( -- )
   0 REUSED-N ! 0 FILES-N ! 0 INSTALLED-N ! 0 COMPILED-N !
   0 NUNIT-DEP:DIAG? !
   0 HOST-INPUT-RESET ! 0 HOST-UNIT-USE ! 0 HOST-INPUT-USE !
   0 HOST-ENV-USE ! 0 HOST-ENV-RESET ! 0 TARGET-ENV-RESET !
   NULL-PTR APP-PATH ! 0 APP-PATH-U !
   NULL-PTR REQUEST-PATH ! 0 REQUEST-PATH-U !
   NULL-PTR REQUEST-MANIFEST ! 0 REQUEST-MANIFEST-U !
   NULL-PTR DISPATCH-SOURCE ! 0 DISPATCH-SOURCE-U !
   NULL-PTR STAGED-REASON ! 0 STAGED-REASON-U !
   COUNTS-PATH PATH-CAP ZERO-SCRATCH 0 COUNTS-PATH-U !
   FILE-KEY-HEX 64 ZERO-SCRATCH
   RESULTS-HEX 64 ZERO-SCRATCH
   HASH-CTX SHA256-CTX-BYTES ZERO-SCRATCH
   HASH-RAW 32 ZERO-SCRATCH
   HASH-CELL CELL ZERO-SCRATCH ;

: BOOT-TAKE ( -- )
   BOOT-N @ 0<> if exit then
   ndict@ BOOT-N ! ;

: OPEN ( -- )
   1 SESSION !
   BOOT-TAKE
   s" HABU_NATIVE_CACHE_DIAG" GETENV s" 1" STR= NUNIT-DEP:DIAG? !
   COUNTS-PATH-READ
   0 SEEDED ! 0 DEPTH !
   0 REUSED-N ! 0 FILES-N ! 0 DISPATCH-BUSY !
   0 DISPATCH-ORIGINAL ! 0 DISPATCH-ROLE-KNOWN !
   0 BORROWED-ORIGINAL ! 0 BORROW-ACTIVE ! 0 BORROW-ACCEPT !
   0 CHECKER-ROLE-KNOWN !
   NULL-PTR PUBLICATION-OWNER !
   NULL-PTR ORIGIN-RECORD !
   0 PUBLICATION-CLEAR-XT !
   0 TFAM-RESET-XT ! 0 CON-RESET-XT !
   0 HOST-SOURCE-BOUND ! 0 TARGET-SOURCE-BOUND ! 0 TARGET-KIND !
   0 HOST-SOURCE-READY ! 0 HOST-UNIT-USE ! 0 HOST-INPUT-USE !
   0 HOST-INPUT-RESET !
   0 HOST-ENV-USE ! 0 HOST-ENV-RESET !
   0 TARGET-INPUT-RESET !
   0 TARGET-ENV-RESET !
   0 TARGET-UNIT-USE !
   NFILE-INPUTS:RESET
   ['] FILE-INPUT BUILD-INPUT:USE
   \ Made here so a tree whose output directory cannot be made fails before
   \ the application runs.
   BUILD-OUT:ROOT$ 2drop
   ['] XREF-REC NFILE-REF:RECORD-AT!
   NUNIT-OBJECT:BIND-LOCAL
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
   FRAMES-RELEASE
   ENTRY-RELEASE STORE-IMAGE-RELEASE OBJECTS-RELEASE
   0 DEPTH ! 0 SESSION ! 0 SEEDED !
   0 DISPATCH-BUSY ! 0 DISPATCH-ROLE-KNOWN !
   0 CHECKER-ROLE-KNOWN !
   0 ORIGIN-GETTER !
   0 IMPORT-KIND ! 0 IMPORT-FED ! 0 IMPORT-AT !
   0 HARVEST-N ! 0 HARVEST-FED !
   0 OPAQUE-XT ! 0 OBJECTS-U !
   REPORT? @ if PRINT-COUNTS then
   CLEAR-SCRATCH ;

\ The body returned, so every file it entered has left its frame (DEPTH 0) and
\ FILES-N is final; a failed body writes no counts.
: OUTER-RUN ( -- )
   OPEN OUTER-BODY @ execute COUNTS-WRITE ;

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

: PREFIX-ORIGIN-USE ( [ -- n bool ] [ n [ n n n ptr u8 n ptr u8 n -- ] -- ] [ n n n ptr u8 n ptr u8 n -- ] -- )
   PRE-IMPORT-HOOK ! PRE-EACH-HOOK ! PRE-MARK-HOOK ! 1 PRE-BOUND ! ;
: PREFIX-ORIGIN-CLEAR ( -- ) 0 PRE-BOUND ! ;

: WITH ( ptr u8 n [ -- ] -- )
   {: mode:ptr modeu:n body :}
   mode modeu s" engine" STR= NATIVE? !
   true REPORT? !
   true COUNTED? !
   body OUTER-BODY !
   ['] OUTER-RUN ['] CLOSE finally ;

: WITH-QUIET ( ptr u8 n [ -- ] -- )
   {: mode:ptr modeu:n body :}
   mode modeu s" engine" STR= NATIVE? !
   false REPORT? !
   false COUNTED? !
   body OUTER-BODY !
   ['] OUTER-RUN ['] CLOSE finally ;

: RUN ( [ -- ] -- )
   SEED BIND-HOST-SOURCE execute ;

: REBIND ( -- )
   SESSION @ 0= if exit then
   SEED
   BIND-TARGET-SOURCE
   BIND-OWNER
   FOLLOW-BUILD-CHECKER
   PIN-ORIGIN-GETTER
   BIND-DISPATCH ;

: HANDOVER-PREPARE ( -- )
   SESSION @ 0= if exit then
   CLEAR-NOMINALS ;
: HANDOVER-BIND ( -- )
   SESSION @ 0= if exit then
   FOLLOW-BUILD-CHECKER
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
      manifest manifestu bytes size FS:ATOMIC-WRITE-FILE
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
