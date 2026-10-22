\ Automatic native definition reuse. The ordinary source loader always runs;
\ the pending native dispatch selects one definition after its source actions.
require src/lib/errors.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/build-input.f
require src/lib/build-cache.f
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
64 constant FRAME-BYTES
\ A cache file name: "hb-def-", 64 hex digits, ".hbd".
75 constant NAME-BYTES

\ An object entry: magic | checksum over the bytes after it | the HBNUNIT2
\ definition object.
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
variable NATIVE?
variable REPORT?
variable DIAG?
variable HIT-N
variable EARLY-PREFIX-HIT-N
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
variable KERNEL-ROLE-KNOWN
create KERNEL-ROLE 32 allot
PTR-VARIABLE PUBLICATION-OWNER
variable PUBLICATION-CLEAR-XT
variable TFAM-RESET-XT
variable CON-RESET-XT
variable OPAQUE-XT
PTR-VARIABLE ORIGIN-RECORD
variable ORIGIN-GETTER
TYPED-VARIABLE TARGET-READ [ ptr u8 n ptr u8 n -- ptr u8 n ]
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
create NAME NAME-BYTES allot
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
CAST: UNIT>N
   ( [ ptr u8 n ptr u8 n ptr u8 n [ n n -- ] -- ] -- n )
CAST: N>UNIT
   ( n -- [ ptr u8 n ptr u8 n ptr u8 n [ n n -- ] -- ] )
CAST: Q>N ( [ n n -- ] -- n )
CAST: N>Q ( n -- [ n n -- ] )

TYPED-VARIABLE PRIOR-UNIT [ ptr u8 n ptr u8 n ptr u8 n [ n n -- ] -- ]
TYPED-VARIABLE HOST-PRIOR-UNIT [ ptr u8 n ptr u8 n ptr u8 n [ n n -- ] -- ]
TYPED-VARIABLE OUTER-BODY [ -- ]
PTR-VARIABLE APP-PATH
variable APP-PATH-U
PTR-VARIABLE REQUEST-PATH
variable REQUEST-PATH-U
PTR-VARIABLE REQUEST-MANIFEST
variable REQUEST-MANIFEST-U
PTR-VARIABLE DISPATCH-SOURCE
variable DISPATCH-SOURCE-U
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

: PIN-KERNEL ( -- )
   SOURCE-PRODUCER:KERNEL SOURCE-PRODUCER:ROLE$
      {: digest:ptr size:n known:bool :}
   known if
      size 32 <> if BAD then
      digest KERNEL-ROLE 32 BYTE-COPY
   then
   known KERNEL-ROLE-KNOWN ! ;

: ACTUAL-KERNEL$ ( -- ptr u8 n bool )
   KERNEL-ROLE KERNEL-ROLE-KNOWN @ if 32 else 0 then
      KERNEL-ROLE-KNOWN @ 0<> ;

: DIAG-W2 ( ptr u8 n -- )
   {: bytes:ptr size:n :}
   0 begin dup size < while
      dup {: sent:n :}
      2 bytes sent + size sent - write
      dup 0 <= if drop E-FS-IO throw then
      +
   repeat drop ;

: REFUSE ( ptr u8 n ptr u8 n -- )
   {: path:ptr pathu:n reason:ptr reasonu:n :}
   s" native-cache: entry refused " DIAG-W2
   path pathu DIAG-W2
   s" : " DIAG-W2
   reason reasonu DIAG-W2
   S\" \n" DIAG-W2 ;

\ FNV-1a 64 over whole cells, then the tail bytes. Every target is
\ little-endian, so a cell read matches the bytes on disk on every host.
: CHECKSUM ( ptr u8 n -- n )
   {: bytes:ptr size:n :}
   size CELL / CELL * {: whole:n :}
   FNV-BASIS
   whole 0 ?do bytes i + CELL-VIEW @ xor FNV-PRIME * CELL +loop
   size whole ?do bytes i + c@ xor FNV-PRIME * loop ;

\ The object entry for a definition key, as this checker and kernel read it:
\ hb-def-, hex(HASH-RAW), .hbd in the cache root.
: ENTRY-PATH ( ptr u8 n -- ptr u8 n )
   s" habu-definition-entry-v1" HASH-START
   HASH-S
   CHECKER-ROLE$ drop HASH-S
   ACTUAL-KERNEL$ drop HASH-S
   HASH-CTX HASH-RAW SHA256-END
   s" hb-def-" {: pre:ptr preu:n :}
   s" .hbd" {: suf:ptr sufu:n :}
   pre NAME preu BYTE-COPY
   HASH-RAW NAME preu + SHA256>HEX
   suf NAME preu + 64 + sufu BYTE-COPY
   ROOT$ NAME preu 64 + sufu + PATH-BUF JOIN-PATH
   PATH-BUF swap ;

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

: ENTRY-WRITE ( ptr u8 n ptr u8 n -- )
   {: key:ptr keyu:n object:ptr objectu:n :}
   keyu 64 <> objectu 0 < or if BAD then
   ENTRY-HEAD objectu + {: size:n :}
   size STORE-IMAGE-RESERVE
   s" HBDOBJ01" drop 0 STORE-IMAGE 8 BYTE-COPY
   object 0 STORE-IMAGE ENTRY-HEAD + objectu BYTE-COPY
   key keyu ENTRY-PATH size IMAGE-WRITE ;

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

: DEFINITION-KEY ( ptr u8 n ptr u8 n -- ptr u8 n )
   {: source:ptr sourceu:n pending:ptr pendingu:n :}
   s" habu-native-definition-v9" HASH-START
   BUILD-TARGET:CURRENT RTARGET:PROFILE$ HASH-S
   s" HBNUNIT2-definition-v2" HASH-S
   data-base NCOMP-DISPATCH:TIER-CELL + @ HASH-N
   check@ 0= if 0 else 1 then HASH-N
   BORROW-ACTIVE @ HASH-N
   DISPATCH-ROLE 32 HASH-S
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

: ORIGIN-HOOK-ALLOWED? ( -- bool )
   check@ {: hook:n :}
   hook 0= if true exit then
   ORIGIN-RECORD @ dup NULL-PTR = if drop false exit then
   dup NATIVE-BODY-ABI:ORIGIN-HOOK cells + CELL-VIEW @ hook =
   swap NATIVE-BODY-ABI:ORIGIN-TAKE cells + CELL-VIEW @ 0<> and ;

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

\ Install the prepared definition from the entry at path, which ENTRY holds
\ until DISCARD-DEFINITION. USED dates the entry after the read; a prune
\ that takes it in between costs nothing, since this session has its bytes.
: INSTALL-ENTRY ( ptr u8 n n ptr u8 n -- )
   {: path:ptr pathu:n ord:n source:ptr sourceu:n :}
   NUNIT-OBJECT:INSTALL-DEFINITION ord <> if BAD then
   BORROW-ACTIVE @ if
      source sourceu BORROW-ACCEPT @ BORROW-ACCEPT-XT execute
   else ARM-WARM-ORIGIN then
   ord 0 PENDING-KEY PENDING-KEY-U @ NFILE-REF:PUBLISH-DEFINITION
   NUNIT-OBJECT:DISCARD-DEFINITION
   path pathu BUILD-CACHE:USED drop
   1 HIT-N +!
   BORROW-ACTIVE @ if
      NUNIT-ORIGIN:PENDING-AVAILABLE? 0= if 1 EARLY-PREFIX-HIT-N +! then
   then ;

\ The entry for key, checked against today's live bindings before it is
\ installed.
: FIND-HIT ( ptr u8 n n ptr u8 n -- bool )
   {: key:ptr keyu:n ord:n source:ptr sourceu:n :}
   keyu 64 <> if BAD then
   key keyu ENTRY-PATH {: path:ptr pathu:n :}
   path pathu s" HBDOBJ01" ENTRY-HEAD CACHE-READ {: size:n found:bool :}
   found 0= if false exit then
   0 ENTRY ENTRY-HEAD + size ENTRY-HEAD - key keyu
      NUNIT-OBJECT:PREPARE-DEFINITION? 0= if
      NUNIT-OBJECT:DISCARD-DEFINITION false exit
   then
   path pathu ord source sourceu INSTALL-ENTRY
   true ;

: DIAG-PATH? ( -- bool )
   DEPTH @ 0= if false exit then
   F-INPUT-U F@ 0= F-SRC-U F@ 0= or if false exit then
   FRAME F-SRC + 0 ptr-field @ NULL-PTR = if false exit then
   data-base INP-CELL + CELL-VIEW @ {: cursor:n :}
   FRAME F-SRC + 0 ptr-field @ NULL-PTR BYTE-VIEW - {: source:n :}
   cursor source >= if cursor source - F-SRC-U F@ <= else false then ;

: DIAG-FALLBACK ( ptr u8 n -- )
   {: reason:ptr reasonu:n :}
   DIAG? @ 0= if exit then
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
      s" eligible-cache-miss" COMPILE-ORIGINAL ;

: COLD-CANDIDATE ( ptr u8 n n -- )
   {: key:ptr keyu:n ord:n :}
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
      NUNIT-DEP:EXPORT {: deps:ptr depsu:n storable:bool :}
      storable if
         ord 0 PENDING-KEY PENDING-KEY-U @ NFILE-REF:PUBLISH-DEFINITION
         key keyu deps depsu NUNIT-OBJECT:PACK-DEFINITION
            {: object:ptr objectu:n :}
         key keyu object objectu ENTRY-WRITE
      else
         s" ordered-dependency" ord NUNIT-OBJECT:REPORT-STORE-REFUSED
         NFILE-REF:PENDING-RELEASE
      then
   else NFILE-REF:PENDING-RELEASE then
   NUNIT-OBJECT:DISCARD-DEFINITION
   NUNIT-DEP:CLOSE ;

: DISPATCH-DEFINITION ( ptr u8 n -- )
   {: source:ptr sourceu:n :}
   DISPATCH-BUSY @ if source sourceu s" busy" COMPILE-ORIGINAL exit then
   ADMISSION-REASON$ {: reason:ptr reasonu:n :}
   reasonu 0 > if
      source sourceu reason reasonu COMPILE-ORIGINAL exit
   then
   1 DISPATCH-BUSY !
   ndict@ {: ord:n :}
   PENDING-COPY {: pending:ptr pendingu:n :}
   source sourceu pending pendingu DEFINITION-KEY
      {: key:ptr keyu:n :}
   key keyu ord source sourceu FIND-HIT 0= if
      key keyu ord COLD-CANDIDATE
   then
   0 DISPATCH-BUSY ! ;

: DISPATCH-STAGED ( -- )
   DISPATCH-SOURCE @ DISPATCH-SOURCE-U @ DISPATCH-DEFINITION ;

: WRAPPED-DISPATCH ( -- )
   DISPATCH-BUSY @ if
      DISPATCH-SOURCE @ DISPATCH-SOURCE-U @ s" busy" COMPILE-ORIGINAL
      exit
   then
   ADMISSION-REASON$ {: reason:ptr reasonu:n :}
   reasonu 0= if
      ['] DISPATCH-STAGED NUNIT-OBJECT:WITH-DEFINITIONS
   else
      DISPATCH-SOURCE @ DISPATCH-SOURCE-U @ reason reasonu
         COMPILE-ORIGINAL
   then ;

: DISPATCH-GUARDED ( ptr u8 n -- )
   DISPATCH-BUSY @ if s" busy" COMPILE-ORIGINAL exit then
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

: READ-WITH ( ptr u8 n ptr u8 n [ ptr u8 n ptr u8 n -- ptr u8 n ] -- ptr u8 n )
   {: path:ptr pathu:n root:ptr rootu:n reader :}
   path pathu root rootu reader execute
      {: source:ptr sourceu:n :}
   path pathu root rootu source sourceu NFILE-INPUTS:ADD-ROOT
   source sourceu ;

: HOST-READ ( ptr u8 n ptr u8 n -- ptr u8 n )
   ['] SOURCE-ROOT:READ-OS READ-WITH ;

: TARGET-READ-SOURCE ( ptr u8 n ptr u8 n -- ptr u8 n )
   TARGET-READ @ READ-WITH ;

: FILE-INPUT ( ptr u8 n -- ) s" " NFILE-INPUTS:ADD-MTIME ;

: UNIT-RUN ( -- )
   NATIVE? @ if
      FRAME F-INPUT + 0 ptr-field @ F-INPUT-U F@
      FRAME F-ROOT + 0 ptr-field @ F-ROOT-U F@
      FRAME F-SRC + 0 ptr-field @ F-SRC-U F@
         BUILD-PRODUCER:OBSERVE
   then
   FRAME F-INPUT + 0 ptr-field @ F-INPUT-U F@
   FRAME F-ROOT + 0 ptr-field @ F-ROOT-U F@
   FRAME F-SRC + 0 ptr-field @ F-SRC-U F@
   F-Q F@ N>Q F-PRIOR F@ N>UNIT execute ;

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
   DIAG? @ 0= if exit then
   FRAME F-INPUT + 0 ptr-field @ F-INPUT-U F@ NUNIT-DEP:FILE-REPORT ;

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
   FRAME-RESERVE
   path pathu root rootu FILE-NAME kind NFILE-REF:PUSH-FILE
   FILE-OPEN
   path FRAME F-INPUT + 0 ptr-field !
   pathu F-INPUT-U F!
   source FRAME F-SRC + 0 ptr-field !
   sourceu F-SRC-U F!
   root FRAME F-ROOT + 0 ptr-field !
   rootu F-ROOT-U F!
   q Q>N F-Q F!
   prior UNIT>N F-PRIOR F!
   NUNIT-DEP:BEGIN-UNIT
   ['] UNIT-RUN catch {: unit-rc:n :}
   ['] NUNIT-OBJECT:DEFINITIONS-CLOSE catch {: plan-rc:n :}
   ['] FRAME-REPORT catch {: report-rc:n :}
   NUNIT-DEP:CLOSE-UNIT
   unit-rc 0= plan-rc 0= and
      if ['] FILE-SUCCESS catch else 0 then
      {: finish-rc:n :}
   FILE-CLOSE
   ['] NFILE-REF:POP-FILE catch {: pop-rc:n :}
   unit-rc 0<> if unit-rc throw then
   finish-rc 0<> if finish-rc throw then
   plan-rc 0<> if plan-rc throw then
   report-rc 0<> if report-rc throw then
   pop-rc 0<> if pop-rc throw then ;

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
   ['] CANON ['] HOST-READ HOST-INPUT-USE @ INPUT-USE-XT execute ;

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
   s" SOURCE-ROOT:READ-OS" TARGET-XT SOURCE-READ-XT TARGET-READ !
   1 TARGET-SOURCE-BOUND !
   ['] NFILE-INPUTS:ADD-ENV s" ENV-INPUT:USE" ENV-XT
      ENV-USE-XT execute
   ['] CANON ['] TARGET-READ-SOURCE s" SOURCE-INPUT:USE" TARGET-XT
      INPUT-USE-XT execute ;

: SEED ( -- )
   SEEDED @ if exit then
   NFILE-REF:BIND-SEED
   1 SEEDED !
   NATIVE? @ if AOT-ARM:B0 @ else 0 then NUNIT-ORIGIN:BIND
   BIND-OWNER ;

: PRINT-COUNTS ( -- )
   s" definition hits " type HIT-N @ .
   s" early prefix hits " type EARLY-PREFIX-HIT-N @ .
   s"  compiled " type COMPILE-N @ . ;

: CLEAR-SCRATCH ( -- )
   0 HIT-N ! 0 EARLY-PREFIX-HIT-N ! 0 COMPILE-N !
   0 DIAG? !
   0 HOST-INPUT-RESET ! 0 HOST-UNIT-USE ! 0 HOST-INPUT-USE !
   0 HOST-ENV-USE ! 0 HOST-ENV-RESET ! 0 TARGET-ENV-RESET !
   NULL-PTR APP-PATH ! 0 APP-PATH-U !
   NULL-PTR REQUEST-PATH ! 0 REQUEST-PATH-U !
   NULL-PTR REQUEST-MANIFEST ! 0 REQUEST-MANIFEST-U !
   NULL-PTR DISPATCH-SOURCE ! 0 DISPATCH-SOURCE-U !
   HASH-CTX SHA256-CTX-BYTES ZERO-SCRATCH
   HASH-RAW 32 ZERO-SCRATCH
   KEY-HEX 64 ZERO-SCRATCH HASH-CELL CELL ZERO-SCRATCH
   NAME NAME-BYTES ZERO-SCRATCH PATH-BUF FS-PATH-CAP ZERO-SCRATCH ;

: OPEN ( -- )
   1 SESSION !
   s" HABU_NATIVE_CACHE_DIAG" GETENV s" 1" STR= DIAG? !
   0 SEEDED ! 0 DEPTH !
   0 HIT-N ! 0 EARLY-PREFIX-HIT-N ! 0 COMPILE-N ! 0 DISPATCH-BUSY !
   0 DISPATCH-ORIGINAL ! 0 DISPATCH-ROLE-KNOWN !
   0 BORROWED-ORIGINAL ! 0 BORROW-ACTIVE ! 0 BORROW-ACCEPT !
   0 CHECKER-ROLE-KNOWN !
   0 KERNEL-ROLE-KNOWN !
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
   PIN-KERNEL
   PIN-HOST-SOURCE
   PIN-ORIGIN-GETTER
   BIND-DISPATCH
   BIND-OPAQUE
   \ The engine build rewinds the dictionary before loading its prefix. Its
   \ namespace publications must already reach the retained source owner.
   BIND-OWNER
   BIND-NOMINALS ;

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
   NUNIT-DEP:RESET NUNIT-DEP:UNBIND
   NUNIT-ORIGIN:RESET
   NUNIT-OBJECT:DEFINITIONS-CLOSE
   NUNIT-OBJECT:CLOSE
   CLEAR-NOMINALS
   NFILE-REF:CLOSE
   FRAMES-RELEASE PENDING-KEY-RELEASE
   ENTRY-RELEASE STORE-IMAGE-RELEASE
   0 DEPTH ! 0 SESSION ! 0 SEEDED !
   0 DISPATCH-BUSY ! 0 DISPATCH-ROLE-KNOWN !
   0 CHECKER-ROLE-KNOWN !
   0 KERNEL-ROLE-KNOWN ! 0 ORIGIN-GETTER !
   0 OPAQUE-XT ! 0 PENDING-KEY-U !
   ['] SOURCE-ROOT:READ-OS TARGET-READ !
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
