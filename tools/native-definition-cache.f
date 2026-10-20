\ Automatic native definition reuse. The ordinary source loader always runs;
\ the pending native dispatch selects one definition after its source actions.
require src/lib/errors.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/build-input.f
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

8 constant MAGIC-U
224 constant ROW-HEAD
104 constant ROW-FILE-HASH
136 constant ROW-CONTEXT
168 constant ROW-NAME-AT
176 constant ROW-BODY-AT
184 constant ROW-RESUME-AT
192 constant ROW-SPAN-HASH
CHECKER-OWNER-ABI:PARSE-IMM-OFF constant PARSE-IMM-OFF
32 constant INDEX-ROW-BYTES
16 constant INDEX-USED-OFF
24 constant INDEX-CONTEXT-OFF
64 constant INDEX-CAP-BYTES
$4B8 constant LIVE-FIXED-OFF
FS-PATH-CAP 296 + constant FRAME-BYTES
0 constant F-PATH-U
8 constant F-BYTES
16 constant F-CAP
24 constant F-USED
32 constant F-DIRTY
40 constant F-SRC
48 constant F-SRC-U
56 constant F-ROOT
64 constant F-ROOT-U
72 constant F-Q
80 constant F-INPUT
88 constant F-INPUT-U
96 constant F-PRIOR
104 constant F-INDEX
112 constant F-INDEX-CAP
120 constant F-INDEX-N
128 constant F-PATH
FS-PATH-CAP 128 + constant F-FILE-HASH
FS-PATH-CAP 160 + constant F-CONTEXT
FS-PATH-CAP 192 + constant F-NAME-AT
FS-PATH-CAP 200 + constant F-BODY-AT
FS-PATH-CAP 208 + constant F-RESUME-AT
FS-PATH-CAP 216 + constant F-STAGE-ORD
FS-PATH-CAP 224 + constant F-STAGED
FS-PATH-CAP 232 + constant F-SPAN-HASH
FS-PATH-CAP 264 + constant F-READER-BYTES
FS-PATH-CAP 272 + constant F-READER-U
FS-PATH-CAP 280 + constant F-READER-CAP
FS-PATH-CAP 288 + constant F-READER-VALID

DYNAMIC-BUFFER FRAMES ptr u8
DYNAMIC-BUFFER PENDING-KEY u8
variable DEPTH
variable SESSION
variable SEEDED
variable NATIVE?
variable REPORT?
variable DIAG?
variable HIT-N
variable EARLY-PREFIX-HIT-N
variable COMPILE-N
variable BODY-SKIP-N
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
PTR-VARIABLE BODY-RECORD
variable BODY-GETTER
variable BODY-READER-GETTER
variable BODY-PASSIVE-ENTRY
PTR-VARIABLE BODY-READER-CELL
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
PTR-VARIABLE OPEN-PATH
variable OPEN-PATH-U
PTR-VARIABLE OPEN-ROOT
variable OPEN-ROOT-U
create HASH-CTX SHA256-CTX-BYTES allot
create HASH-RAW 32 allot
create SOURCE-RAW 32 allot
create KEY-HEX 64 allot
create HASH-CELL CELL allot
create NAME 76 allot
create PATH-BUF FS-PATH-CAP allot
create STORED-PRODUCER 32 allot
create CONTEXT-RAW 32 allot

CAST: DISPATCH-XT ( n -- [ ptr u8 n -- ] )
CAST: DISPATCH>N ( [ ptr u8 n -- ] -- n )
CAST: BORROW-ACCEPT-XT ( n -- [ ptr u8 n -- ] )
CAST: ACTION-XT ( n -- [ -- ] )
CAST: ORIGIN-STATE-XT ( n -- [ -- bool bool ] )
CAST: ROLE-XT ( n -- [ -- ptr u8 n bool ] )
CAST: ROLE-ID-XT ( n -- [ n -- ptr u8 n bool ] )
CAST: OPAQUE-XT-TYPE ( n -- [ -- bool ] )
CAST: BODY-CELL-XT ( n -- [ -- ptr u8 ] )
CAST: BODY-PASSIVE-XT ( n -- [ ptr u8 n -- bool ] )
CAST: BODY-REUSE>N ( [ ptr u8 n n n -- n bool ] -- n )
CAST: BODY-REUSE-CELL ( ptr u8 -- ptr [ ptr u8 n n n -- n bool ] )
CAST: READER-CELL ( ptr u8 -- ptr [ ptr u8 n bool -- ] )
CAST: READER>N ( [ ptr u8 n bool -- ] -- n )
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
   DEPTH @ 0= if BAD then DEPTH @ 1- FRAMES @ ;
: F@ ( n -- n ) FRAME + CELL-VIEW @ ;
: F! ( n n -- ) FRAME + CELL-VIEW ! ;
: FPATH$ ( -- ptr u8 n ) FRAME F-PATH + F-PATH-U F@ ;
: FB ( -- ptr u8 ) FRAME F-BYTES + 0 ptr-field @ ;
: FB! ( ptr u8 -- ) FRAME F-BYTES + 0 ptr-field ! ;
: FU ( -- n ) F-USED F@ ;
: RB ( -- ptr u8 ) FRAME F-READER-BYTES + 0 ptr-field @ ;
: RB! ( ptr u8 -- ) FRAME F-READER-BYTES + 0 ptr-field ! ;

: READER-CLEAR ( -- )
   0 F-READER-U F! 0 F-READER-VALID F! ;

: READER-RELEASE ( -- )
   RB dup NULL-PTR <> if
      F-READER-CAP F@ munmap 0<> if E-MEM-UNMAP throw then
   else drop then
   NULL-PTR RB! 0 F-READER-CAP F! READER-CLEAR ;

: READER-ROOM ( n -- )
   {: need:n :}
   need 0 < need MEM-MAX-N F-READER-U F@ - > or if BAD then
   F-READER-U F@ need + F-READER-CAP F@ <= if exit then
   F-READER-U F@ need + F-READER-CAP F@ 2 * max 256 max
      {: cap:n :}
   cap MEM-ALLOC-BYTES drop {: fresh:ptr :}
   F-READER-U F@ 0 > if RB fresh F-READER-U F@ BYTE-COPY then
   RB dup NULL-PTR <> if
      F-READER-CAP F@ munmap 0<> if E-MEM-UNMAP throw then
   else drop then
   fresh RB! cap F-READER-CAP F! ;

: READER-OBSERVE ( ptr u8 n bool -- )
   {: spelling:ptr spellingu:n passive:bool :}
   DEPTH @ 0= if exit then
   F-STAGED F@ 0= F-READER-VALID F@ 0= or if exit then
   passive 0= spellingu 0= or if READER-CLEAR exit then
   spellingu 8 + READER-ROOM
   spellingu RB F-READER-U F@ + LE:U64!
   spelling RB F-READER-U F@ 8 + + spellingu BYTE-COPY
   F-READER-U F@ spellingu 8 + + F-READER-U F! ;

: READER-OWNED? ( -- bool )
   BODY-READER-CELL @ dup NULL-PTR = if drop false exit then
   CELL-VIEW @ ['] READER-OBSERVE READER>N = ;

: READER-TRANSFER ( -- )
   F-STAGED F@ 0= F-READER-VALID F@ 0= or
   READER-OWNED? 0= or if READER-CLEAR exit then
   0 begin dup F-READER-U F@ < while
      RB over + LE:U64@ {: len:n :}
      RB over 8 + + len NUNIT-DEP:BODY-READER+
      len 8 + +
   repeat drop
   NUNIT-DEP:READER-COMPLETE!
   READER-CLEAR ;

: FILE-ROOT$ ( -- ptr u8 n )
   \ Cache storage belongs to the loader, not the application's input record.
   s" HABU_BUILD_CACHE" ENV-INPUT:CURRENT$ dup 0 > if exit then
   2drop s" .out" ;

: HASH-N ( n -- )
   HASH-CELL ! HASH-CTX HASH-CELL CELL SHA256-FEED ;
: HASH-S ( ptr u8 n -- )
   {: a:ptr u:n :}
   u HASH-N
   u 0 > if HASH-CTX a u SHA256-FEED then ;
: HASH-START ( ptr u8 n -- )
   HASH-CTX SHA256-BEGIN HASH-S ;

: HASH-FILE ( ptr u8 n -- )
   s" habu-definition-file-v1" HASH-START
   HASH-S
   HASH-CTX FRAME F-FILE-HASH + SHA256-END ;

: HASH-SPAN ( ptr u8 n n n -- )
   {: source:ptr sourceu:n first:n resume:n :}
   first 0 < resume first < or resume sourceu > or if BAD then
   s" habu-definition-span-v1" HASH-START
   source first + resume first - HASH-S
   resume sourceu = if
      0 HASH-N
   else
      1 HASH-N
      HASH-CTX source resume + 1 SHA256-FEED
   then
   HASH-CTX HASH-RAW SHA256-END ;

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

: CONTEXT-HASH? ( ptr u8 n -- bool )
   {: pending:ptr pendingu:n :}
   DISPATCH-ROLE-KNOWN @ 0= CHECKER-ROLE-KNOWN @ 0= or if false exit then
   ACTUAL-KERNEL$
      {: kernel:ptr kernelu:n known:bool :}
   known 0= if false exit then
   kernelu 32 <> if BAD then
   s" habu-definition-reader-v5" HASH-START
   BUILD-TARGET:CURRENT RTARGET:PROFILE$ HASH-S
   data-base NCOMP-DISPATCH:TIER-CELL + @ HASH-N
   check@ 0= if 0 else 1 then HASH-N
   BORROW-ACTIVE @ HASH-N
   DISPATCH-ROLE 32 HASH-S
   CHECKER-ROLE 32 HASH-S
   kernel kernelu HASH-S
   pending pendingu HASH-S
   HASH-CTX CONTEXT-RAW SHA256-END
   true ;

\ One stable container per canonical source and target. Its name is unchanged
\ when the source bytes change, so a later load can find surviving definitions.
: CONTAINER-PATH ( ptr u8 n ptr u8 n -- ptr u8 n )
   {: path:ptr pathu:n root:ptr rootu:n :}
   s" habu-definitions-container-v3" HASH-START
   BUILD-TARGET:CURRENT RTARGET:PROFILE$ HASH-S
   path pathu HASH-S root rootu HASH-S
   HASH-CTX HASH-RAW SHA256-END
   s" hb-def-" drop NAME 7 BYTE-COPY
   HASH-RAW NAME 7 + SHA256>HEX
   s" .hbd" drop NAME 71 + 4 BYTE-COPY
   FILE-ROOT$ NAME 75 PATH-BUF JOIN-PATH
   PATH-BUF swap ;

: FILE-FRESH ( -- )
   8 MEM-ALLOC-BYTES drop FB!
   8 F-CAP F! 8 F-USED F!
   s" HBDEF004" drop FB 8 BYTE-COPY ;

: FILE-DROP-BYTES ( -- )
   FB dup NULL-PTR <> if
      F-CAP F@ munmap 0<> if E-MEM-UNMAP throw then
   else drop then
   NULL-PTR FB! 0 F-CAP F! 0 F-USED F! ;

: ROW-END ( n -- n bool )
   {: at:n :}
   FU at - ROW-HEAD < if 0 false exit then
   FB at + 64 + LE:U64@ {: objectu:n :}
   objectu 0 < objectu FU at ROW-HEAD + - > or if 0 false exit then
   at ROW-HEAD + objectu + true ;

: FILE-VALID? ( -- bool )
   FU MAGIC-U < if false exit then
   FB 8 s" HBDEF004" STR= 0= if false exit then
   MAGIC-U begin dup FU < while
      ROW-END 0= if drop false exit then
   repeat FU = ;

: INDEX-PTR ( -- ptr u8 )
   FRAME F-INDEX + 0 ptr-field @ ;
: INDEX-PTR! ( ptr u8 -- )
   FRAME F-INDEX + 0 ptr-field ! ;
: INDEX-CAP ( -- n ) F-INDEX-CAP F@ ;
: INDEX-N ( -- n ) F-INDEX-N F@ ;
: INDEX-ROW ( n -- ptr u8 )
   INDEX-ROW-BYTES * INDEX-PTR + ;
: INDEX-BUCKET ( n -- ptr u8 )
   8 * INDEX-CAP INDEX-ROW-BYTES * + INDEX-PTR + ;
: INDEX-CONTEXT-BUCKET ( n -- ptr u8 )
   8 * INDEX-CAP INDEX-ROW-BYTES 16 + * + INDEX-PTR + ;

: KEY-BUCKET ( ptr u8 -- n )
   {: key:ptr :}
   0 64 0 ?do 33 * key i + c@ + loop
   INDEX-CAP 2 * 1- and ;

: CONTEXT-BUCKET ( ptr u8 -- n )
   {: context:ptr :}
   0 32 0 ?do 33 * context i + c@ + loop
   INDEX-CAP 2 * 1- and ;

\ Context links retain reverse row order, including after a span is refreshed.
: INDEX-CONTEXT+ ( ptr u8 -- )
   {: row:ptr :}
   INDEX-PTR {: index:ptr :}
   row index - INDEX-ROW-BYTES / 1+ {: id:n :}
   FB row LE:U64@ + ROW-CONTEXT + CONTEXT-BUCKET INDEX-CONTEXT-BUCKET
   begin dup LE:U64@ id > while
      LE:U64@ 1- INDEX-ROW-BYTES * index + INDEX-CONTEXT-OFF +
   repeat
   dup LE:U64@ row INDEX-CONTEXT-OFF + LE:U64!
   id swap LE:U64! ;

: INDEX-CONTEXT- ( ptr u8 -- )
   {: row:ptr :}
   INDEX-PTR {: index:ptr :}
   row index - INDEX-ROW-BYTES / 1+ {: id:n :}
   FB row LE:U64@ + ROW-CONTEXT + CONTEXT-BUCKET INDEX-CONTEXT-BUCKET
   begin dup LE:U64@ id <> while
      LE:U64@ 1- INDEX-ROW-BYTES * index + INDEX-CONTEXT-OFF +
   repeat
   row INDEX-CONTEXT-OFF + LE:U64@ swap LE:U64! ;

: INDEX-ADD ( n -- )
   {: off:n :}
   FB off + KEY-BUCKET {: bucket:n :}
   INDEX-N INDEX-ROW {: row:ptr :}
   off row LE:U64!
   bucket INDEX-BUCKET LE:U64@ row 8 + LE:U64!
   0 row INDEX-USED-OFF + LE:U64!
   INDEX-N 1+ bucket INDEX-BUCKET LE:U64!
   row INDEX-CONTEXT+
   INDEX-N 1+ F-INDEX-N F! ;

: INDEX-ROOM ( -- )
   INDEX-N INDEX-CAP < if exit then
   INDEX-PTR {: old:ptr :}
   INDEX-CAP {: oldcap:n :}
   INDEX-N {: oldn:n :}
   oldcap 0= if 16 else oldcap 2 * then {: cap:n :}
   cap INDEX-CAP-BYTES * MEM-ALLOC-BYTES drop {: fresh:ptr :}
   fresh INDEX-PTR! cap F-INDEX-CAP F! 0 F-INDEX-N F!
   oldn 0 ?do
      old i INDEX-ROW-BYTES * + LE:U64@ INDEX-ADD
      old i INDEX-ROW-BYTES * + INDEX-USED-OFF + LE:U64@
         i INDEX-ROW INDEX-USED-OFF + LE:U64!
   loop
   old NULL-PTR <> if
      old oldcap INDEX-CAP-BYTES * munmap 0<> if E-MEM-UNMAP throw then
   then ;

: INDEX-OPEN ( -- )
   INDEX-ROOM
   MAGIC-U begin dup FU < while
      dup ROW-END 0= if drop BAD then {: next:n :}
      INDEX-ROOM dup INDEX-ADD
      drop next
   repeat drop ;

: INDEX-DROP ( -- )
   INDEX-PTR dup NULL-PTR <> if
      INDEX-CAP INDEX-CAP-BYTES * munmap 0<> if E-MEM-UNMAP throw then
   else drop then
   NULL-PTR INDEX-PTR!
   0 F-INDEX-CAP F! 0 F-INDEX-N F! ;

: FILE-OPEN-BODY ( -- )
   FRAME F-INPUT + 0 ptr-field @ F-INPUT-U F@
   FRAME F-ROOT + 0 ptr-field @ F-ROOT-U F@
      CONTAINER-PATH {: cache:ptr cacheu:n :}
   cacheu FS-PATH-CAP > if E-FS-PATH throw then
   cache FRAME F-PATH + cacheu BYTE-COPY cacheu F-PATH-U F!
   FPATH$ FILE? 0= if FILE-FRESH INDEX-OPEN exit then
   FPATH$ FILE-SIZE {: size:n :}
   size 0 <= if FILE-FRESH 1 F-DIRTY F! INDEX-OPEN exit then
   size MEM-ALLOC-BYTES drop FB!
   size F-CAP F! size F-USED F!
   FPATH$ FB size READ-ALL size <> if E-FS-IO throw then
   FILE-VALID? 0= if
      FILE-DROP-BYTES FILE-FRESH 1 F-DIRTY F!
   then
   INDEX-OPEN ;

: FILE-ROOM ( n -- )
   {: need:n :}
   need 0 < need MEM-MAX-N FU - > or if BAD then
   FU need + F-CAP F@ <= if exit then
   FU {: used:n :}
   FU need + F-CAP F@ 2 * max 4096 max {: cap:n :}
   cap MEM-ALLOC-BYTES drop {: fresh:ptr :}
   FB fresh used BYTE-COPY
   FILE-DROP-BYTES
   fresh FB! cap F-CAP F!
   used F-USED F!
   ;

: ROW-SIZE ( n -- n )
   dup ROW-END 0= if drop BAD then swap - ;

: COPY-ROW ( n n -- n )
   {: dest:n at:n :}
   at ROW-SIZE {: size:n :}
   dest at <> if FB at + FB dest + size BYTE-COPY then
   dest size + ;

\ BYTE-COPY moves forward, so retained rows can move toward the header while
\ their source bytes remain intact. No row offsets are used after file close.
: FILE-COMPACT ( -- )
   MAGIC-U INDEX-N 0 ?do
      i INDEX-ROW INDEX-USED-OFF + LE:U64@ 0<> if
         i INDEX-ROW LE:U64@ COPY-ROW
      then
   loop
   dup FU <> if 1 F-DIRTY F! then
   F-USED F! ;

: FILE-WRITE ( -- )
   FILE-COMPACT
   F-DIRTY F@ 0= if exit then
   FPATH$ FB FU ATOMIC-WRITE-FILE ;

: FILE-CLOSE ( bool -- )
   {: success:bool :}
   success if ['] FILE-WRITE catch else 0 then {: write-rc:n :}
   ['] FILE-DROP-BYTES catch {: bytes-rc:n :}
   ['] INDEX-DROP catch {: index-rc:n :}
   ['] READER-RELEASE catch {: reader-rc:n :}
   FRAME FRAME-BYTES munmap {: frame-rc:n :}
   -1 DEPTH +!
   write-rc 0<> if write-rc throw then
   bytes-rc 0<> if bytes-rc throw then
   index-rc 0<> if index-rc throw then
   reader-rc 0<> if reader-rc throw then
   frame-rc 0<> if E-MEM-UNMAP throw then ;

: FILE-ABORT ( -- ) false FILE-CLOSE ;

: FILE-OPEN ( ptr u8 n ptr u8 n -- )
   {: path:ptr pathu:n root:ptr rootu:n :}
   DEPTH @ 1+ FRAMES-RESERVE
   FRAME-BYTES MEM-ALLOC-BYTES drop {: frame:ptr :}
   frame DEPTH @ FRAMES !
   1 DEPTH +!
   path FRAME F-INPUT + 0 ptr-field !  pathu F-INPUT-U F!
   root FRAME F-ROOT + 0 ptr-field !  rootu F-ROOT-U F!
   0 F-PATH-U F!
   NULL-PTR FB! 0 F-CAP F! 0 F-USED F! 0 F-DIRTY F!
   NULL-PTR INDEX-PTR! 0 F-INDEX-CAP F! 0 F-INDEX-N F!
   NULL-PTR RB! 0 F-READER-CAP F! READER-CLEAR
   0 F-STAGED F!
   ['] FILE-OPEN-BODY catch {: rc:n :}
   rc 0<> if
      \ Release the partial frame while retaining the original open failure.
      ['] FILE-ABORT catch drop
      rc throw
   then ;

: FILE-OPEN-STAGED ( -- )
   OPEN-PATH @ OPEN-PATH-U @ OPEN-ROOT @ OPEN-ROOT-U @ FILE-OPEN ;

: WRITE-ROW-SPAN ( ptr u8 -- )
   {: row:ptr :}
   FRAME F-FILE-HASH + row ROW-FILE-HASH + 32 BYTE-COPY
   F-STAGED F@ 0<> if
      FRAME F-CONTEXT + row ROW-CONTEXT + 32 BYTE-COPY
      F-NAME-AT F@ row ROW-NAME-AT + LE:U64!
      F-BODY-AT F@ row ROW-BODY-AT + LE:U64!
      F-RESUME-AT F@ row ROW-RESUME-AT + LE:U64!
      FRAME F-SPAN-HASH + row ROW-SPAN-HASH + 32 BYTE-COPY
   else
      32 0 ?do 0 row ROW-CONTEXT + i + c! loop
      -1 row ROW-NAME-AT + LE:U64!
      -1 row ROW-BODY-AT + LE:U64!
      -1 row ROW-RESUME-AT + LE:U64!
      32 0 ?do 0 row ROW-SPAN-HASH + i + c! loop
   then ;

: ROW-SPAN! ( ptr u8 -- )
   {: row:ptr :}
   row INDEX-CONTEXT-
   FB row LE:U64@ + WRITE-ROW-SPAN
   row INDEX-CONTEXT+ ;

: STORE ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: key:ptr keyu:n object:ptr objectu:n producer:ptr produceru:n :}
   keyu 64 <> produceru 32 <> or if BAD then
   ROW-HEAD objectu + {: size:n :}
   FU {: at:n :}
   size FILE-ROOM
   \ FILE-ROOM may move storage but the payload and dependency buffers belong
   \ to their own owners, not this container.
   at F-USED F!
   key FB at + 64 BYTE-COPY
   objectu FB at 64 + + LE:U64!
   producer FB at 72 + + 32 BYTE-COPY
   FB at + WRITE-ROW-SPAN
   FB at + ROW-HEAD + {: payload:ptr :}
   object payload objectu BYTE-COPY
   at size + F-USED F!
   INDEX-ROOM at INDEX-ADD
   1 INDEX-N 1- INDEX-ROW INDEX-USED-OFF + LE:U64!
   1 F-DIRTY F! ;

: HASH-SOURCE ( ptr u8 n -- ptr u8 n )
   s" habu-definition-source-v1" HASH-START
   HASH-S
   HASH-CTX SOURCE-RAW SHA256-END
   SOURCE-RAW 32 ;

: DEFINITION-KEY ( ptr u8 n ptr u8 n -- ptr u8 n )
   {: source:ptr sourceu:n pending:ptr pendingu:n :}
   s" habu-native-definition-v6" HASH-START
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

: STAGE-MATCH? ( n ptr u8 n -- bool )
   {: ord:n pending:ptr pendingu:n :}
   F-STAGED F@ 0= if false exit then
   F-STAGE-ORD F@ ord <> if false exit then
   pending pendingu CONTEXT-HASH? 0= if false exit then
   FRAME F-CONTEXT + 32 CONTEXT-RAW 32 STR= ;

: STAGE-RESUME ( -- )
   F-STAGED F@ 0= if exit then
   data-base INP-CELL + CELL-VIEW @ {: cursor:n :}
   FRAME F-SRC + 0 ptr-field @ NULL-PTR BYTE-VIEW - {: source:n :}
   cursor source < cursor source - F-SRC-U F@ > or if
      0 F-STAGED F! exit
   then
   cursor source - F-BODY-AT F@ < if 0 F-STAGED F! exit then
   cursor source - F-RESUME-AT F!
   FRAME F-SRC + 0 ptr-field @ F-SRC-U F@
      F-NAME-AT F@ F-RESUME-AT F@ HASH-SPAN
   HASH-RAW FRAME F-SPAN-HASH + 32 BYTE-COPY ;

: OPAQUE? ( -- bool )
   OPAQUE-XT @ dup 0= if drop true exit then
   OPAQUE-XT-TYPE execute ;

: BODY-HOOK-ALLOWED? ( -- bool )
   check@ {: hook:n :}
   hook 0= if true exit then
   BODY-RECORD @ dup NULL-PTR = if drop false exit then
   dup NATIVE-BODY-ABI:ORIGIN-HOOK cells + CELL-VIEW @ hook =
   swap NATIVE-BODY-ABI:ORIGIN-TAKE cells + CELL-VIEW @ 0<> and ;

: ADMISSION-REASON$ ( -- ptr u8 n )
   DEPTH @ 0= if s" no-source-frame" exit then
   data-base TRUSTED-CELL + CELL-VIEW @ 0<> if s" trusted" exit then
   BODY-RECORD @ NULL-PTR = if s" missing-body-capability" exit then
   DISPATCH-ROLE-KNOWN @ 0= if s" unknown-role" exit then
   BORROW-ACTIVE @ if
      BORROW-OWNER-READY? 0= if s" unavailable-owner" exit then
   else OWNER-READY? 0= if s" unavailable-owner" exit then then
   OPAQUE? if s" opaque" exit then
   BODY-HOOK-ALLOWED? 0= if s" incompatible-body-hook" exit then
   BORROW-ACTIVE @ if s" " exit then
   NUNIT-ORIGIN:PENDING-AVAILABLE? 0= if
      s" missing-pending-origin" exit
   then
   s" " ;

: CANDIDATE? ( -- bool ) ADMISSION-REASON$ nip 0= ;

: BODY-ROW? ( ptr u8 n n -- bool )
   {: row:ptr name-at:n body-at:n :}
   row ROW-NAME-AT + LE:U64@ name-at <>
   row ROW-BODY-AT + LE:U64@ body-at <> or if false exit then
   row ROW-RESUME-AT + LE:U64@ {: resume:n :}
   resume body-at < resume F-SRC-U F@ > or if false exit then
   row ROW-FILE-HASH + 32 FRAME F-FILE-HASH + 32 STR= 0= if
      false exit
   then
   row ROW-CONTEXT + 32 FRAME F-CONTEXT + 32 STR= ;

: READER-READY? ( -- bool )
   BORROW-ACTIVE @ if false exit then
   check@ 0= if false exit then
   data-base POLICY-NDICT-CELL + CELL-VIEW @ 0<> if false exit then
   READER-OWNED? 0= if false exit then
   BODY-PASSIVE-ENTRY @ 0= if false exit then
   OWNER-STRUCTURAL? 0= if false exit then
   s" NEUTRAL-PARSE-IMM?" TARGET-XT?
      dup 0= if drop false exit then
   OWNER PARSE-IMM-OFF + CELL-VIEW @ = ;

: BODY-PASSIVE? ( ptr u8 n -- bool )
   READER-READY? 0= if 2drop false exit then
   BODY-PASSIVE-ENTRY @ BODY-PASSIVE-XT execute ;

: BODY-SHIFTED? ( ptr u8 n n -- n bool )
   {: row:ptr name-at:n body-at:n :}
   READER-READY? 0= if 0 false exit then
   row ROW-CONTEXT + 32 FRAME F-CONTEXT + 32 STR= 0= if
      0 false exit
   then
   row ROW-NAME-AT + LE:U64@ {: old-name:n :}
   row ROW-BODY-AT + LE:U64@ {: old-body:n :}
   row ROW-RESUME-AT + LE:U64@ {: old-resume:n :}
   old-name 0 < old-body old-name < or
   old-resume old-body < or if 0 false exit then
   old-body old-name - body-at name-at - <> if 0 false exit then
   old-resume old-name - {: span:n :}
   span F-SRC-U F@ name-at - > if 0 false exit then
   name-at span + {: resume:n :}
   resume body-at < if 0 false exit then
   FRAME F-SRC + 0 ptr-field @ F-SRC-U F@
      name-at resume HASH-SPAN
   HASH-RAW 32 row ROW-SPAN-HASH + 32 STR= 0= if
      0 false exit
   then
   HASH-RAW FRAME F-SPAN-HASH + 32 BYTE-COPY
   resume true ;

: BODY-MATCH ( ptr u8 n n -- n bool bool )
   {: row:ptr name-at:n body-at:n :}
   row name-at body-at BODY-ROW? if
      row ROW-RESUME-AT + LE:U64@ false true exit
   then
   row name-at body-at BODY-SHIFTED? if true true exit then
   drop 0 false false ;

: ARM-NATIVE ( [ ptr u8 n -- ] -- bool )
   {: completion :}
   BODY-RECORD @ dup NULL-PTR = if drop false exit then
   NATIVE-BODY-ABI:PENDING-TAKE cells + COMPLETION-CELL
      completion swap xt!
   true ;

: ARM-WARM-ORIGIN ( -- )
   BORROW-ACTIVE @ if exit then
   data-base TRUSTED-CELL + CELL-VIEW @ 0<> if exit then
   BODY-RECORD @ dup NULL-PTR = if drop exit then
   {: record:ptr :}
   record NATIVE-BODY-ABI:ORIGIN-HOOK cells + CELL-VIEW @ {: hook:n :}
   hook 0= check@ hook <> or if exit then
   record NATIVE-BODY-ABI:ORIGIN-TAKE cells + CELL-VIEW @ dup 0= if
      drop exit
   then
   ORIGIN-TAKE-XT ARM-NATIVE drop ;

: INSTALL-ROW ( ptr u8 n ptr u8 n bool -- )
   {: row:ptr ord:n source:ptr sourceu:n update:bool :}
   row LE:U64@ {: at:n :}
   FB at 72 + + STORED-PRODUCER 32 BYTE-COPY
   NUNIT-OBJECT:INSTALL-DEFINITION ord <> if BAD then
   BORROW-ACTIVE @ if
      source sourceu BORROW-ACCEPT @ BORROW-ACCEPT-XT execute
   else ARM-WARM-ORIGIN then
   ord 0 PENDING-KEY PENDING-KEY-U @ STORED-PRODUCER 32
      NFILE-REF:PUBLISH-DEFINITION
   update if row ROW-SPAN! 1 F-DIRTY F! then
   NUNIT-OBJECT:DISCARD-DEFINITION
   1 HIT-N +!
   BORROW-ACTIVE @ if
      NUNIT-ORIGIN:PENDING-AVAILABLE? 0= if 1 EARLY-PREFIX-HIT-N +! then
   then ;

: BODY-TRY-ROW ( ptr u8 n n n -- n bool )
   {: row:ptr ord:n name-at:n body-at:n :}
   row LE:U64@ {: at:n :}
   FB at + name-at body-at BODY-MATCH
      {: resume:n edited:bool match:bool :}
   match 0= if 0 false exit then
   FB at 64 + + LE:U64@ {: objectu:n :}
   FB at ROW-HEAD + + objectu FB at + 64
      edited if NUNIT-OBJECT:PREPARE-DEFINITION-COMPLETE?
      else NUNIT-OBJECT:PREPARE-DEFINITION? then
   0= if
      NUNIT-OBJECT:DISCARD-DEFINITION 0 false exit
   then
   edited if
      resume F-RESUME-AT F!
   then
   row ord NULL-PTR 0 edited INSTALL-ROW
   1 BODY-SKIP-N +!
   resume true ;

: FIND-BODY ( n n n -- n bool )
   {: ord:n name-at:n body-at:n :}
   INDEX-PTR {: index:ptr :}
   FRAME F-CONTEXT + CONTEXT-BUCKET INDEX-CONTEXT-BUCKET LE:U64@
   begin dup 0<> while
      1- INDEX-ROW-BYTES * index + {: row:ptr :}
      row ord name-at body-at BODY-TRY-ROW if
         1 row INDEX-USED-OFF + LE:U64!
         true exit
      then drop
      row INDEX-CONTEXT-OFF + LE:U64@
   repeat drop 0 false ;

: BODY-REUSE ( ptr u8 n n n -- n bool )
   {: source:ptr sourceu:n name-at:n body-at:n :}
   DEPTH @ 0= if 0 false exit then
   source FRAME F-SRC + 0 ptr-field @ <>
   sourceu F-SRC-U F@ <> or if 0 false exit then
   name-at 0 < body-at name-at < or
   body-at sourceu > or if 0 false exit then
   CANDIDATE? 0= if 0 false exit then
   ndict@ {: ord:n :}
   PENDING-COPY {: pending:ptr pendingu:n :}
   pending pendingu CONTEXT-HASH? 0= if
      NFILE-REF:PENDING-RELEASE 0 false exit
   then
   CONTEXT-RAW FRAME F-CONTEXT + 32 BYTE-COPY
   ord F-STAGE-ORD F!
   name-at F-NAME-AT F!
   body-at F-BODY-AT F!
   -1 F-RESUME-AT F!
   1 F-STAGED F!
   READER-CLEAR
   READER-READY? if 1 F-READER-VALID F! then
   ord name-at body-at FIND-BODY
   dup if
      0 F-STAGED F! READER-CLEAR
      exit
   then
   NFILE-REF:PENDING-RELEASE ;

\ All candidates in the stable source container are checked against today's
\ live bindings before any installation. The last matching variant wins.
: TRY-ROW ( ptr u8 ptr u8 n n ptr u8 n -- bool )
   {: row:ptr key:ptr keyu:n ord:n source:ptr sourceu:n :}
   row LE:U64@ {: at:n :}
   keyu 64 <> if BAD then
   FB at + 64 key keyu STR= 0= if false exit then
   FB at 64 + + LE:U64@ {: objectu:n :}
   FB at ROW-HEAD + + objectu key keyu
      NUNIT-OBJECT:PREPARE-DEFINITION? 0= if
      NUNIT-OBJECT:DISCARD-DEFINITION false exit
   then
   row ord source sourceu F-STAGED F@ 0<> INSTALL-ROW
   true ;

: FIND-HIT ( ptr u8 n n ptr u8 n -- bool )
   {: key:ptr keyu:n ord:n source:ptr sourceu:n :}
   keyu 64 <> if BAD then
   INDEX-PTR {: index:ptr :}
   key KEY-BUCKET INDEX-BUCKET LE:U64@
   begin dup 0<> while
      1- INDEX-ROW-BYTES * index + {: row:ptr :}
      row key keyu ord source sourceu TRY-ROW if
         1 row INDEX-USED-OFF + LE:U64!
         true exit
      then
      row 8 + LE:U64@
   repeat drop false ;

: DIAG-W2 ( ptr u8 n -- )
   {: bytes:ptr size:n :}
   0 begin dup size < while
      dup {: sent:n :}
      2 bytes sent + size sent - write
      dup 0 <= if drop E-FS-IO throw then
      +
   repeat drop ;

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

: COLD-CANDIDATE ( ptr u8 n ptr u8 n n -- )
   {: source:ptr sourceu:n key:ptr keyu:n ord:n :}
   ord NUNIT-DEP:BEGIN-DEFINITION
   READER-TRANSFER
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
      NUNIT-DEP:EXPORT {: deps:ptr depsu:n :}
      source sourceu HASH-SOURCE NUNIT-DEP:FINAL-PRODUCER$
         {: producer:ptr produceru:n known:bool :}
      known if
         ord 0 PENDING-KEY PENDING-KEY-U @ producer produceru
            NFILE-REF:PUBLISH-DEFINITION
         key keyu deps depsu NUNIT-OBJECT:PACK-DEFINITION
            {: object:ptr objectu:n :}
         key keyu object objectu producer produceru STORE
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
   ord pending pendingu STAGE-MATCH? 0= if 0 F-STAGED F! then
   source sourceu pending pendingu DEFINITION-KEY
      {: key:ptr keyu:n :}
   key keyu ord source sourceu FIND-HIT 0= if
      source sourceu key keyu ord COLD-CANDIDATE
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
   DEPTH @ 0 > if STAGE-RESUME then
   ['] WRAPPED-DISPATCH catch {: rc:n :}
   0 DISPATCH-BUSY !
   DEPTH @ 0 > if 0 F-STAGED F! READER-CLEAR then
   rc 0<> if NFILE-REF:PENDING-RELEASE rc throw then ;

: BIND-OPAQUE ( -- )
   s" native-pending-opaque?" XREF-FIND
   dup XREF-FOUND? 0= if drop 0 OPAQUE-XT ! exit then
   XREF-START dup TERMINAL-CALL:IN-ENGINE-TEXT? 0= if drop 0 then
   OPAQUE-XT ! ;

: PIN-BODY-GETTER ( -- )
   0 BODY-GETTER ! 0 BODY-READER-GETTER ! 0 BODY-PASSIVE-ENTRY !
   s" native-body-reuse-cell" TARGET-XT?
      dup 0= if drop exit then
   dup TERMINAL-CALL:IN-ENGINE-TEXT? 0= if drop exit then
   BODY-GETTER !
   s" native-body-reader-cell" TARGET-XT?
      dup 0= if drop exit then
   dup TERMINAL-CALL:IN-ENGINE-TEXT? 0= if drop exit then
   BODY-READER-GETTER !
   s" native-body-passive?" TARGET-XT?
      dup 0= if drop exit then
   dup TERMINAL-CALL:IN-ENGINE-TEXT? 0= if drop exit then
   BODY-PASSIVE-ENTRY ! ;

: BIND-BODY-REUSE ( -- )
   NULL-PTR BODY-RECORD !
   NULL-PTR BODY-READER-CELL !
   BODY-GETTER @ dup 0= if drop exit then
   BODY-CELL-XT execute dup BODY-RECORD !
   dup NULL-PTR = if drop exit then
   ['] BODY-REUSE swap NATIVE-BODY-ABI:REUSE cells +
      BODY-REUSE-CELL xt!
   BODY-READER-GETTER @ dup 0= if drop exit then
   BODY-PASSIVE-ENTRY @ 0= if drop exit then
   BODY-CELL-XT execute dup BODY-READER-CELL !
   dup NULL-PTR = if drop exit then
   ['] READER-OBSERVE swap READER-CELL xt!
   ['] BODY-PASSIVE? NUNIT-DEP:USE-BODY-PASSIVE ;

: UNBIND-BODY-REUSE ( -- )
   BODY-READER-CELL @ dup NULL-PTR <> if
      dup CELL-VIEW @ ['] READER-OBSERVE READER>N = if
         0 swap CELL-VIEW !
      else drop then
   else drop then
   NULL-PTR BODY-READER-CELL !
   BODY-RECORD @ dup NULL-PTR = if drop exit then
   dup NATIVE-BODY-ABI:REUSE cells + CELL-VIEW @
      ['] BODY-REUSE BODY-REUSE>N = if
      0 swap NATIVE-BODY-ABI:REUSE cells + CELL-VIEW !
   else drop then
   NULL-PTR BODY-RECORD ! ;

: BIND-DISPATCH ( -- )
   data-base NCOMP-DISPATCH:XT-CELL + CELL-VIEW @ {: actual:n :}
   actual ['] DISPATCH-GUARDED DISPATCH>N <> if
      actual DISPATCH-ORIGINAL !
      PIN-COMPILER
   then
   actual 0= if 0 DISPATCH-ROLE-KNOWN ! exit then
   ['] COMPILER-ROLE$ NUNIT-DEP:USE-COMPILER-ROLE
   ['] DISPATCH-GUARDED NCOMP:DISPATCH!
   BIND-BODY-REUSE ;

: UNBIND-DISPATCH ( -- )
   UNBIND-BODY-REUSE
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

: FILE-CLOSE-OK ( -- ) true FILE-CLOSE ;
: FILE-CLOSE-FAIL ( -- ) false FILE-CLOSE ;

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
   path pathu root rootu kind NFILE-REF:PUSH-FILE
   path OPEN-PATH ! pathu OPEN-PATH-U !
   root OPEN-ROOT ! rootu OPEN-ROOT-U !
   ['] FILE-OPEN-STAGED catch {: open-rc:n :}
   open-rc 0<> if
      \ The open failure owns the result; its frame has already been released.
      ['] NFILE-REF:POP-FILE catch drop
      open-rc throw
   then
   path FRAME F-INPUT + 0 ptr-field !
   pathu F-INPUT-U F!
   source FRAME F-SRC + 0 ptr-field !
   sourceu F-SRC-U F!
   source sourceu HASH-FILE
   root FRAME F-ROOT + 0 ptr-field !
   rootu F-ROOT-U F!
   q Q>N F-Q F!
   prior UNIT>N F-PRIOR F!
   ['] UNIT-RUN catch {: unit-rc:n :}
   ['] NUNIT-OBJECT:DEFINITIONS-CLOSE catch {: plan-rc:n :}
   unit-rc 0= plan-rc 0= and
      if ['] FILE-SUCCESS catch else 0 then
      {: finish-rc:n :}
   unit-rc 0= plan-rc 0= and finish-rc 0= and
      if ['] FILE-CLOSE-OK else ['] FILE-CLOSE-FAIL then
      catch {: close-rc:n :}
   ['] NFILE-REF:POP-FILE catch {: pop-rc:n :}
   unit-rc 0<> if unit-rc throw then
   finish-rc 0<> if finish-rc throw then
   plan-rc 0<> if plan-rc throw then
   close-rc 0<> if close-rc throw then
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
   s" body skips " type BODY-SKIP-N @ .
   s" early prefix hits " type EARLY-PREFIX-HIT-N @ .
   s"  compiled " type COMPILE-N @ . ;

: ZERO-SCRATCH ( ptr u8 n -- )
   {: bytes:ptr size:n :}
   size 0 ?do 0 bytes i + c! loop ;

: CLEAR-SCRATCH ( -- )
   0 HIT-N ! 0 BODY-SKIP-N ! 0 EARLY-PREFIX-HIT-N ! 0 COMPILE-N !
   0 DIAG? !
   0 HOST-INPUT-RESET ! 0 HOST-UNIT-USE ! 0 HOST-INPUT-USE !
   0 HOST-ENV-USE ! 0 HOST-ENV-RESET ! 0 TARGET-ENV-RESET !
   NULL-PTR OPEN-PATH ! 0 OPEN-PATH-U !
   NULL-PTR OPEN-ROOT ! 0 OPEN-ROOT-U !
   NULL-PTR APP-PATH ! 0 APP-PATH-U !
   NULL-PTR REQUEST-PATH ! 0 REQUEST-PATH-U !
   NULL-PTR REQUEST-MANIFEST ! 0 REQUEST-MANIFEST-U !
   NULL-PTR DISPATCH-SOURCE ! 0 DISPATCH-SOURCE-U !
   HASH-CTX SHA256-CTX-BYTES ZERO-SCRATCH
   HASH-RAW 32 ZERO-SCRATCH SOURCE-RAW 32 ZERO-SCRATCH
   KEY-HEX 64 ZERO-SCRATCH HASH-CELL CELL ZERO-SCRATCH
   NAME 76 ZERO-SCRATCH PATH-BUF FS-PATH-CAP ZERO-SCRATCH
   STORED-PRODUCER 32 ZERO-SCRATCH CONTEXT-RAW 32 ZERO-SCRATCH ;

: OPEN ( -- )
   1 SESSION !
   s" HABU_NATIVE_CACHE_DIAG" GETENV s" 1" STR= DIAG? !
   0 SEEDED ! 0 DEPTH !
   0 HIT-N ! 0 BODY-SKIP-N ! 0 EARLY-PREFIX-HIT-N ! 0 COMPILE-N ! 0 DISPATCH-BUSY !
   0 DISPATCH-ORIGINAL ! 0 DISPATCH-ROLE-KNOWN !
   0 BORROWED-ORIGINAL ! 0 BORROW-ACTIVE ! 0 BORROW-ACCEPT !
   0 CHECKER-ROLE-KNOWN !
   0 KERNEL-ROLE-KNOWN !
   NULL-PTR PUBLICATION-OWNER !
   NULL-PTR BODY-RECORD !
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
   FILE-ROOT$ MAKE-DIRS
   ['] XREF-REC NFILE-REF:RECORD-AT!
   NUNIT-CAPTURE:BIND-PRODUCER
   PIN-HOST-CHECKER
   PIN-KERNEL
   PIN-HOST-SOURCE
   PIN-BODY-GETTER
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
      false FILE-CLOSE
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
   0 DEPTH ! 0 SESSION ! 0 SEEDED !
   0 DISPATCH-BUSY ! 0 DISPATCH-ROLE-KNOWN !
   0 CHECKER-ROLE-KNOWN !
   0 KERNEL-ROLE-KNOWN ! 0 BODY-GETTER !
   0 BODY-READER-GETTER ! 0 BODY-PASSIVE-ENTRY !
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
   PIN-BODY-GETTER
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
