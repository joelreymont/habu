\ Native unit object: code and dictionary rows, source-declared relocation
\ sites, DATA and typed cells, literal rows, wordlists and checker facts.
\ Address-bearing fields use local offsets, literal rows or exact-prefix
\ references. The source key binds that prefix and target to the object.

require src/lib/errors.f
require src/lib/string.f
require src/lib/le.f
require src/lib/memory.f
require src/habu/layout.f
require src/habu/xref.f
require src/habu/code-span.f
require src/habu/address-carrier.f
require src/lib/ffi-abi.f
require src/compiler/native/a64ir.f
require src/compiler/native/branch.f
require src/compiler/native/string.f
require src/compiler/workspace.f
require tools/native-unit-capture.f
require tools/native-file-reference.f
require tools/native-unit-dependency.f
require tools/native-unit-file.f
require tools/native-file-inputs.f

package NUNIT-OBJECT

private

32 constant CALL-BYTES
40 constant ADDR-BYTES
40 constant SLOT-BYTES
4 constant INSN-BYTES
CAST: CODE-BYTES ( n -- ptr u8 )
CAST: LITERAL-ATTACH-XT ( n -- [ ptr u8 n n n -- ] )
defer LITERAL-ATTACH ( ptr u8 n n n -- )

DYNAMIC-BUFFER CODE-BUF u8
DYNAMIC-BUFFER REC-BUF u8
DYNAMIC-BUFFER CALL-SITES n
DYNAMIC-BUFFER ADDR-SITES n
DYNAMIC-BUFFER LITERAL-ADDR n
DYNAMIC-BUFFER LITERAL-LEN n
DYNAMIC-BUFFER LITERAL-LOCAL u8
DYNAMIC-BUFFER LITERAL-SEGS n
variable CODE-U
variable REC-U
variable LITERAL-N
variable LITERAL-CURSOR
variable LITERAL-SEG-N
variable LITERAL-PRIOR
variable REQUIRE-CURSOR
DYNAMIC-BUFFER PROTECTION u8
variable PROTECTION-U
variable IMPORT-CODE
variable IMPORT-REC
variable IMPORT-WID
variable IMPORT-DATA-BASE
: BAD ( -- ) E-NUNIT-PROFILE throw ;

\ Each prepared range owns the section bytes that its publication consumes.
\ Section offsets stay valid while other ranges are prepared or installed.
17 constant PREP-SECTIONS
0 constant P-SIZE
1 constant P-CODE
2 constant P-REC
3 constant P-WID
4 constant P-DATA
5 constant P-CODE-U
6 constant P-REC-U
7 constant P-WID-U
8 constant P-DATA-U
9 constant P-LIT-OFF
10 constant P-LIT-N
11 constant P-CALL-OFF
12 constant P-CALL-N
13 constant P-ADDR-OFF
14 constant P-ADDR-N
\ The header's (offset, size) pairs for each section follow the fixed fields.
15 constant P-SECTIONS
P-SECTIONS PREP-SECTIONS 2 * + cells constant PREP-HEAD
DYNAMIC-BUFFER PREP-P ptr u8
DYNAMIC-BUFFER PREP-U n
variable PREP-N
PTR-VARIABLE PREP-ACTIVE
variable PROJECT-ON
variable PROJECT-CODE
variable PROJECT-REC
variable PROJECT-WID
variable PROJECT-DATA
variable PREP-CURSOR

: PREP-ROOM ( n -- ) {: size:n :}
   size 0 < size MEM-MAX-N PREP-CURSOR @ - > or if BAD then
   size PREP-CURSOR +! ;
: PREP-TABLE-ROOM ( n n -- ) {: rows:n width:n :}
   rows 0 < rows MEM-MAX-N PREP-CURSOR @ - width / > or if BAD then
   rows width * PREP-ROOM ;

: P@ ( ptr u8 n -- n ) {: block:ptr field:n :}
   block field cells + CELL-VIEW @ ;
: P! ( n ptr u8 n -- ) {: value:n block:ptr field:n :}
   value block field cells + CELL-VIEW ! ;
: SECTION-OFF ( n -- n ) 2 * P-SECTIONS + ;
: SECTION$ ( n -- ptr u8 n ) {: slot:n :}
   PREP-ACTIVE @ NULL-PTR = if slot NUNIT-FILE:SECTION$ exit then
   PREP-ACTIVE @ {: block:ptr :}
   block slot SECTION-OFF P@ {: off:n :}
   block slot SECTION-OFF 1+ P@ {: size:n :}
   block off + size ;

: PROJECT-START ( -- )
   cp@ PROJECT-CODE ! ndict@ PROJECT-REC !
   AOT-ARM:WIDN PROJECT-WID ! AOT-ARM:HERE-N PROJECT-DATA !
   1 PROJECT-ON ! ;

: PREP-RECORD ( n -- ptr n ) {: ord:n :}
   ord ndict@ < if ord XREF-REC exit then
   PREP-N @ 0 ?do
      i PREP-P @ {: block:ptr :}
      block NULL-PTR <> if
         block P-REC P@ {: first:n :}
         ord first >= ord first block P-REC-U P@ DREC / + < and if
            block block 1 SECTION-OFF P@ + ord first - DREC * +
               CELL-VIEW
            unloop exit
         then
      then
   loop
   BAD ;

: PROJECT-RETIRED-UPTO? ( n n -- bool ) {: ord:n count:n :}
   count 0 ?do
      i PREP-P @ {: block:ptr :}
      block NULL-PTR <> if
         block 12 SECTION-OFF 1+ P@ 8 / 0 ?do
            block block 12 SECTION-OFF P@ + i 8 * + LE:U64@
               ord = if true unloop unloop exit then
         loop
      then
   loop false ;

: PROJECT-RETIRED? ( n -- bool )
   PREP-N @ 1- 0 max PROJECT-RETIRED-UPTO? ;

: PREFIX-WID ( n -- n ) {: ord:n :}
   ord PREP-RECORD XREF-WORDLIST
   ord PROJECT-RETIRED? if drop XREF-RETIRED-WL then ;

: PREP-ALLOC ( -- n )
   PREP-HEAD PREP-CURSOR !
   PREP-SECTIONS 0 ?do
      i NUNIT-FILE:SECTION$ nip {: u:n :}
      u PREP-ROOM
   loop
   2 cells PREP-ROOM
   10 NUNIT-FILE:SECTION$ nip 16 - 0 max 24 / 4 cells PREP-TABLE-ROOM
   2 NUNIT-FILE:SECTION$ nip CALL-BYTES / CELL PREP-TABLE-ROOM
   3 NUNIT-FILE:SECTION$ nip ADDR-BYTES / CELL PREP-TABLE-ROOM
   PREP-N @ {: id:n :}
   id 1+ PREP-P-RESERVE id 1+ PREP-U-RESERVE
   PREP-CURSOR @ MEM-ALLOC-BYTES drop {: block:ptr :}
   PREP-CURSOR @ block P-SIZE P!
   PREP-HEAD PREP-CURSOR !
   PREP-SECTIONS 0 ?do
      i NUNIT-FILE:SECTION$ {: src:ptr u:n :}
      PREP-CURSOR @ block i SECTION-OFF P!
      u block i SECTION-OFF 1+ P!
      src block PREP-CURSOR @ + u BYTE-COPY
      u PREP-CURSOR +!
   loop
   PREP-CURSOR @ block P-LIT-OFF P!
   2 cells PREP-ROOM
   10 NUNIT-FILE:SECTION$ nip 16 - 0 max 24 / 4 cells PREP-TABLE-ROOM
   PREP-CURSOR @ block P-CALL-OFF P!
   2 NUNIT-FILE:SECTION$ nip CALL-BYTES / CELL PREP-TABLE-ROOM
   PREP-CURSOR @ block P-ADDR-OFF P!
   3 NUNIT-FILE:SECTION$ nip ADDR-BYTES / CELL PREP-TABLE-ROOM
   block id PREP-P ! block P-SIZE P@ id PREP-U !
   1 PREP-N +!
   block PREP-ACTIVE !
   id ;

: PREP-USE ( n -- ) {: id:n :}
   id 0 < id PREP-N @ >= or if BAD then
   id PREP-P @ dup NULL-PTR = if drop BAD then PREP-ACTIVE ! ;

: PREP-DROP ( n -- ) {: id:n :}
   id 0 < id PREP-N @ >= or if BAD then
   id PREP-P @ {: block:ptr :}
   block NULL-PTR = if exit then
   PREP-ACTIVE @ block = if NULL-PTR PREP-ACTIVE ! then
   block id PREP-U @ munmap 0<> if E-MEM-UNMAP throw then
   NULL-PTR id PREP-P ! 0 id PREP-U ! ;

: PREP-LIT-ROW ( n -- ptr u8 ) {: row:n :}
   PREP-ACTIVE @ {: block:ptr :}
   block block P-LIT-OFF P@ + 2 cells + row 4 cells * + ;

: CODE-PTR ( -- ptr u8 )
   CODE-U @ 0= if NULL-PTR exit then 0 CODE-BUF ;
: REC-PTR ( -- ptr u8 )
   REC-U @ 0= if NULL-PTR exit then 0 REC-BUF ;

: SITE-CK ( n n -- ) {: site:n span:n :}
   site 0 < site INSN-BYTES mod 0<> or if BAD then
   span CODE-U @ > site CODE-U @ span - > or if BAD then ;

: PROTECTED? ( n -- bool ) {: wid:n :}
   wid 0 < wid PROT-WID-MAX >= or if BAD then
   data-base PROT-BITS-OFF + BYTE-VIEW wid 3 rshift + c@
   wid 7 and rshift 1 and 0<> ;

: TARGET-XT ( ptr u8 n -- n )
   XREF-FIND dup XREF-FOUND? 0= if drop BAD then
   XREF-START ;

: FORGET-FFI-CACHE ( -- )
   \ The retained loader's FFI serves the process before any target FILE can
   \ define FFI:FORGET-CACHE. Clear the same loader's cache.
   FFI:FORGET-CACHE ;

: PLAN-LITERAL-BEGIN ( n -- ) NSTR:PREPARE-BEGIN ;
: PLAN-LITERAL-ADDR ( ptr u8 n -- n ) NSTR:PREPARE-INTERN ;
: PLAN-LITERAL-END ( -- n ) NSTR:PREPARE-DATA-END ;
: PLAN-LITERAL-ADVANCE ( n -- ) NSTR:PREPARE-ADVANCE ;
: PLAN-LITERAL-SEGMENTS ( ptr u8 n n n n -- ) NSTR:PREPARE-SEGMENTS ;
: PLAN-LITERAL-TAILS ( -- n n ) NSTR:PREPARE-TAILS ;
: PLAN-LITERAL-CLOSE ( -- ) NSTR:PREPARE-CLOSE ;

: PRIVATE-XT? ( ptr u8 n ptr u8 n -- n bool )
   {: pkg:ptr pkgu:n name:ptr nameu:n :}
   pkg pkgu XREF-NAMESPACE-WL XREF-FIND-WL
   dup XREF-FOUND? 0= if drop 0 false exit then
   XREF-PKG-PRIVATE name nameu rot XREF-FIND-WL
   dup XREF-FOUND? 0= if drop BAD then
   XREF-START true ;

: PRIVATE-XT ( ptr u8 n ptr u8 n -- n )
   PRIVATE-XT? 0= if drop BAD then ;

\ The importer and literal pool belong to the retained compiler. Resolve the
\ private seam before a fresh target can define another NSTR namespace.
: BIND-LITERAL-ATTACH ( -- )
   s" NSTR" s" ATTACH-SEGMENTS" PRIVATE-XT LITERAL-ATTACH-XT
      is LITERAL-ATTACH ;
BIND-LITERAL-ATTACH

CAST: PROVIDED-XT ( n -- [ ptr u8 n -- ] )
variable FILE-REQUIRES

: COPY-IMPORT ( n -- ) {: slot:n :}
   slot SECTION$ {: a:ptr size:n :}
   slot 0= if
      size 0 < size INSN-BYTES mod 0<> or if BAD then
      size 0= if 0 CODE-U ! exit then
      size CODE-BUF-RESERVE
      a 0 CODE-BUF size BYTE-COPY size CODE-U ! exit
   then
   slot 1 = if
      size 0 < size DREC mod 0<> or if BAD then
      size 0= if 0 REC-U ! exit then
      size REC-BUF-RESERVE
      a 0 REC-BUF size BYTE-COPY size REC-U ! exit
   then
   BAD ;

: IMPORT-RECORD ( n -- ) {: row:n :}
   row DREC * REC-BUF {: dst:ptr :}
   dst 16 + LE:U64@ {: flags:n :}
   flags DNAME-EXT and 0<> if
      dst 24 + LE:U64@ {: off:n :}
      off 0 < off CODE-U @ >= or if BAD then
      flags DNAME-LEN-MASK and CODE-U @ off - > if BAD then
      IMPORT-CODE @ off + dst 24 + LE:U64!
   else
      flags DNAME-LEN-MASK and 16 > if BAD then
   then
   dst 40 + LE:U64@ {: wid:n :}
   wid XREF-NAMESPACE-WL = if
      dst LE:U64@ NFILE-REF:REF>WID dst LE:U64!
      dst 8 + LE:U64@ NFILE-REF:REF>WID dst 8 + LE:U64!
      exit
   then
   wid NFILE-REF:REF>WID dst 40 + LE:U64!
   dst LE:U64@ {: off:n :}
   off 0 >= if BAD then
   -1 off - NFILE-REF:REF>CODE dst LE:U64! ;

: IMPORT-RECORDS ( -- )
   REC-U @ DREC / 0 ?do i IMPORT-RECORD loop ;

: PREPARE-RETIRE ( -- )
   12 SECTION$ {: rows:ptr size:n :}
   size 8 mod 0<> if BAD then
   size 8 / 0 ?do
      rows i 8 * + LE:U64@ NFILE-REF:REF>RECORD {: ord:n :}
      ord 0 < ord IMPORT-REC @ >= or if BAD then
      ord PREFIX-WID 0 < if BAD then
      ord rows i 8 * + LE:U64!
   loop ;

: IMPORT-RETIRE ( -- )
   12 SECTION$ {: rows:ptr size:n :}
   size 8 / 0 ?do
      rows i 8 * + LE:U64@ XREF-REC XREF:RETIRE
   loop ;

: PREFIX-PROTECTED? ( n -- bool ) {: wid:n :}
   wid AOT-ARM:WIDN < if wid PROTECTED? exit then
   PREP-N @ 0 ?do
      i PREP-P @ {: block:ptr :}
      block NULL-PTR <> if
         block P-WID P@ {: first:n :}
         wid first >= wid first block P-WID-U P@ + < and if
            block block 4 SECTION-OFF P@ + wid first - + c@
               0<> unloop exit
         then
      then
   loop false ;

: PREPARE-CAPACITY ( -- )
   IMPORT-CODE @ dbase@ <
   CODE-U @ REGION IMPORT-CODE @ dbase@ - - > or if BAD then
   IMPORT-REC @ DICT-CAP >
   REC-U @ DREC / DICT-CAP IMPORT-REC @ - > or if BAD then
   IMPORT-DATA-BASE @ DATA-VA VA>N < if BAD then
   PLAN-LITERAL-END DATA-VA VA>N - DATA-SIZE > if BAD then
   data-base SEAL-NDICT-CELL + CELL-VIEW @ 0= if exit then
   REC-U @ DREC / 0 ?do
      i DREC * REC-BUF CELL-VIEW {: rec:ptr :}
      rec XREF-WORDLIST {: wid:n :}
      wid XREF-NAMESPACE-WL <>
      wid XREF-RETIRED-WL <> and
      wid 0 >= and if
         wid IMPORT-WID @ < if
            wid PREFIX-PROTECTED? if BAD then
         then
      then
   loop ;

: ROW-SECTION ( n n n -- ptr u8 ) {: slot:n row:n width:n :}
   slot SECTION$ {: a:ptr size:n :}
   size width mod 0<> if BAD then
   row 0 < row size width / >= or if BAD then
   a row width * + ;

: IMPORT-CALL ( n -- ) {: row:n :}
   2 row CALL-BYTES ROW-SECTION {: src:ptr :}
   src LE:U64@ {: site:n :}
   site INSN-BYTES SITE-CK
   src 8 + LE:U64@ {: kind:n :}
   kind NEMIT:CALL <> kind NEMIT:TAIL <> and if BAD then
   src 16 + LE:U64@ 0<> if BAD then
   src 24 + LE:U64@ NFILE-REF:REF>CODE {: target:n :}
   target src 24 + LE:U64!
   site CODE-BUF {: at:ptr :}
   at LE:U32@ {: old:n :}
   kind NEMIT:CALL = if
      old NBR:BL? 0= if BAD then
      IMPORT-CODE @ site + target NBR:BL-WORD at LE:U32!
   else
      old NBR:B? 0= if BAD then
      IMPORT-CODE @ site + target NBR:B-WORD at LE:U32!
   then ;

: IMPORT-CALLS ( -- )
   2 SECTION$ nip {: size:n :}
   size CALL-BYTES mod 0<> if BAD then
   size CALL-BYTES / 0 ?do i IMPORT-CALL loop ;

: PREPARE-LITERALS ( -- )
   10 SECTION$ {: src:ptr size:n :}
   size 16 < if BAD then
   src LE:U64@ {: segments:n :}
   src 8 + LE:U64@ {: count:n :}
   segments 0 < segments size 16 - 16 / > or if BAD then
   count 0 < count size 16 - segments 16 * - 24 / > or if BAD then
   segments LITERAL-SEG-N !
   count LITERAL-N !
   count PREP-ACTIVE @ P-LIT-N P!
   count 0 > if
      count LITERAL-ADDR-RESERVE
      count LITERAL-LEN-RESERVE
      count LITERAL-LOCAL-RESERVE
   then
   16 segments 16 * + LITERAL-CURSOR !
   count 0 ?do
      size LITERAL-CURSOR @ - 24 < if BAD then
      src LITERAL-CURSOR @ + {: row:ptr :}
      i PREP-LIT-ROW {: planned:ptr :}
      row LE:U64@ {: mode:n :}
      row 8 + LE:U64@ {: ref:n :}
      row 16 + LE:U64@ {: bytes:n :}
      bytes 0 < if BAD then
      24 LITERAL-CURSOR +!
      mode 0= if
         ref 0<> bytes size LITERAL-CURSOR @ - > or if BAD then
         src LITERAL-CURSOR @ + bytes
            PLAN-LITERAL-ADDR {: address:n :}
         address i LITERAL-ADDR !
         0 i LITERAL-LOCAL c!
         0 planned LE:U64!
         LITERAL-CURSOR @ planned 8 + LE:U64!
         bytes planned 16 + LE:U64!
         address planned 24 + LE:U64!
         bytes LITERAL-CURSOR +!
      else mode 1 = if
         6 SECTION$ nip {: data-size:n :}
         ref 0 < ref data-size > or
         bytes data-size ref - > or if BAD then
         ref i LITERAL-ADDR !
         1 i LITERAL-LOCAL c!
         1 planned LE:U64!
         ref planned 8 + LE:U64!
         bytes planned 16 + LE:U64!
      else BAD then then
      bytes i LITERAL-LEN !
   loop
   LITERAL-CURSOR @ size <> if BAD then ;

: PREPARE-LITERAL-TAILS ( -- )
   LITERAL-SEG-N @ 0= if exit then
   PLAN-LITERAL-TAILS
   PREP-ACTIVE @ dup P-LIT-OFF P@ + {: tails:ptr :}
   tails 8 + LE:U64! tails LE:U64! ;

: RESOLVE-LOCAL-LITERALS ( -- )
   LITERAL-N @ 0 ?do
      i LITERAL-LOCAL c@ 0<> if
         i LITERAL-ADDR @ IMPORT-DATA-BASE @ + i LITERAL-ADDR !
         i LITERAL-ADDR @ i PREP-LIT-ROW 24 + LE:U64!
      then
   loop ;

: PREPARE-LITERAL-SEGMENTS ( -- )
   LITERAL-SEG-N @ 0= if exit then
   10 SECTION$ drop {: src:ptr :}
   6 SECTION$ {: data:ptr data-size:n :}
   LITERAL-SEG-N @ LITERAL-SEGS-RESERVE
   NSTR:SEGMENT-BYTES {: required:n :}
   0 LITERAL-PRIOR !
   LITERAL-SEG-N @ 0 ?do
      src 16 + i 16 * + {: row:ptr :}
      row LE:U64@ {: off:n :}
      row 8 + LE:U64@ {: span:n :}
      off LITERAL-PRIOR @ < span required <> or off data-size > or
      span data-size off - > or if BAD then
      data off + NULL-PTR BYTE-VIEW - i LITERAL-SEGS !
      off span + LITERAL-PRIOR !
   loop
   PREP-ACTIVE @ dup P-LIT-OFF P@ + {: tails:ptr :}
   0 LITERAL-SEGS BYTE-VIEW LITERAL-SEG-N @
   tails LE:U64@ tails 8 + LE:U64@
   LITERAL-SEG-N @ 1- 16 * 10 SECTION$ drop 16 + + LE:U64@
      IMPORT-DATA-BASE @ +
   PLAN-LITERAL-SEGMENTS ;

: INSTALL-LITERALS ( -- )
   PREP-ACTIVE @ P-LIT-N P@ 0 ?do
      i PREP-LIT-ROW {: row:ptr :}
      row LE:U64@ 0= if
         10 SECTION$ drop row 8 + LE:U64@ +
         row 16 + LE:U64@
         NSTR:INTERN drop
      then
   loop ;

: FINALIZE-LITERALS ( -- )
   10 SECTION$ drop {: src:ptr :}
   src LE:U64@ {: segments:n :}
   segments 0= if exit then
   segments LITERAL-SEGS-RESERVE
   segments 0 ?do
      src 16 + i 16 * + LE:U64@ IMPORT-DATA-BASE @ +
         i LITERAL-SEGS !
   loop
   PREP-ACTIVE @ dup P-LIT-OFF P@ + {: tails:ptr :}
   0 LITERAL-SEGS BYTE-VIEW segments
   tails LE:U64@ tails 8 + LE:U64@
   LITERAL-ATTACH ;

: IMPORT-LITERAL-REF ( n n -- n ) {: ordinal:n addend:n :}
   ordinal 0 < ordinal LITERAL-N @ >= or if BAD then
   addend 0 < addend ordinal LITERAL-LEN @ > or if BAD then
   ordinal LITERAL-ADDR @ addend + ;

: IMPORT-ADDR ( n -- ) {: row:n :}
   3 row ADDR-BYTES ROW-SECTION {: src:ptr :}
   src LE:U64@ {: site:n :}
   src 8 + LE:U64@ {: kind:n :}
   kind A64IR:ADDR-CODE = if ADDRESS-CARRIER:ADDR-CHAIN-BYTES
   else kind A64IR:ADDR-DATA = if ADDRESS-CARRIER:DATA-CHAIN-BYTES
   else BAD then then {: width:n :}
   site width SITE-CK
   src 16 + LE:U64@ 0<> if BAD then
   src 24 + LE:U64@ {: ref:n :}
   src 32 + LE:U64@ 0<> if BAD then
   kind A64IR:ADDR-CODE = if
      ref NFILE-REF:REF>CODE
   else
      ref NFILE-REF:REF-KIND NFILE-REF:LITERAL = if
         ref NFILE-REF:REF>LITERAL IMPORT-LITERAL-REF
      else ref 0 NFILE-REF:REF>DATA NULL-PTR BYTE-VIEW - then
   then {: target:n :}
   site CODE-BUF {: carrier:ptr :}
   carrier 0 CODE-BUF CODE-U @ + ADDRESS-CARRIER:CHAIN-SIZE
      width <> if BAD then
   carrier target width ADDRESS-CARRIER:SET-CHAIN-VALUE ;

: IMPORT-ADDRS ( -- )
   3 SECTION$ nip {: size:n :}
   size ADDR-BYTES mod 0<> if BAD then
   size ADDR-BYTES / 0 ?do i IMPORT-ADDR loop ;

: IMPORT-PROTECTION ( -- )
   4 SECTION$ {: a:ptr size:n :}
   size PROT-WID-MAX > if BAD then
   size 0= if 0 PROTECTION-U ! exit then
   size PROTECTION-RESERVE
   a 0 PROTECTION size BYTE-COPY
   size PROTECTION-U !
   size 0 ?do i PROTECTION c@ 1 > if BAD then loop ;

: OUTSIDE-REGION? ( n -- bool ) {: target:n :}
   target dbase@ < if true exit then
   target dbase@ REGION + >= ;

\ The relocation sites the publisher maps (src/compiler/native/publish.f
\ NPUB:IMPORT-UNIT), as unit offsets it adds its code pointer to: every prefix
\ call that leaves the region, and every address chain. An empty list has no
\ storage, and the publisher reads no cell of it.
: CALL-SITES$ ( -- ptr n n )
   2 SECTION$ nip CALL-BYTES / {: rows:n :}
   rows CALL-SITES-RESERVE
   0 rows 0 ?do
      2 i CALL-BYTES ROW-SECTION {: src:ptr :}
      src 8 + LE:U64@ NEMIT:CALL =
      src 24 + LE:U64@ OUTSIDE-REGION? and if
         src LE:U64@ over CALL-SITES ! 1+
      then
   loop {: u:n :}
   u 0= if NULL-PTR 0 exit then
   0 CALL-SITES u ;

: ADDR-SITES$ ( -- ptr n n )
   3 SECTION$ nip ADDR-BYTES / {: rows:n :}
   rows 0= if NULL-PTR 0 exit then
   rows ADDR-SITES-RESERVE
   rows 0 ?do
      3 i ADDR-BYTES ROW-SECTION LE:U64@ i ADDR-SITES !
   loop
   0 ADDR-SITES rows ;

: RESTORE-PROTECTION ( -- )
   4 SECTION$ drop {: bits:ptr :}
   PROTECTION-U @ 0 ?do
      bits i + c@ 0<> if IMPORT-WID @ i + prot-wid-add then
   loop ;

: IMPORT-DATA ( -- )
   6 SECTION$ {: src:ptr size:n :}
   size allot
   src IMPORT-DATA-BASE @ CODE-BYTES size BYTE-COPY ;

: PREPARE-UPDATES ( -- )
   7 SECTION$ {: src:ptr size:n :}
   size 16 mod 0<> if BAD then
   size 16 / 0 ?do
      src i 16 * + {: row:ptr :}
      row LE:U64@ 1 NFILE-REF:REF>DATA
         NULL-PTR BYTE-VIEW - {: site:n :}
      site IMPORT-DATA-BASE @ >= if BAD then
      site IMPORT-DATA-BASE @ NUNIT-CAPTURE:OWNED-BYTE? 0= if BAD then
      row 8 + LE:U64@ {: byte:n :}
      byte 0 < byte 255 > or if BAD then
      site row LE:U64!
   loop ;

: IMPORT-UPDATES ( -- )
   7 SECTION$ {: src:ptr size:n :}
   size 16 / 0 ?do
      src i 16 * + LE:U64@ {: site:n :}
      src i 16 * 8 + + LE:U64@ {: byte:n :}
      byte site CODE-BYTES c!
   loop ;

: PREPARE-REQUIRES ( -- )
   8 SECTION$ {: src:ptr size:n :}
   0 REQUIRE-CURSOR !
   begin REQUIRE-CURSOR @ size < while
      size REQUIRE-CURSOR @ - 8 < if BAD then
      src REQUIRE-CURSOR @ + LE:U64@ {: bytes:n :}
      bytes 0 <= bytes PATH-CAP > or
      bytes size REQUIRE-CURSOR @ 8 + - > or if BAD then
      8 bytes + REQUIRE-CURSOR +!
   repeat ;

: IMPORT-REQUIRES ( -- )
   8 SECTION$ {: src:ptr size:n :}
   0 REQUIRE-CURSOR !
   begin REQUIRE-CURSOR @ size < while
      src REQUIRE-CURSOR @ + LE:U64@ {: bytes:n :}
      src REQUIRE-CURSOR @ 8 + + bytes
      FILE-REQUIRES @ if s" SOURCE-UNIT:PROVIDED-RESOLVED"
      else s" provided" then TARGET-XT PROVIDED-XT execute
      8 bytes + REQUIRE-CURSOR +!
   repeat ;

TRUSTED: STORE-TYPED ( n n n -- ) {: site:n target:n raw:n :}
   raw SNAP-RELOC:XTCELL-DATA-TAG and 0<> if
      site CODE-BYTES ptr-cell-mark
      target site CODE-BYTES CELL-VIEW !
   else
      target site CODE-BYTES xt!
   then ;

: SLOT-ROWS-COMPATIBLE? ( ptr u8 n n -- bool )
   {: rows:ptr count:n raw:n :}
   raw SNAP-RELOC:XTCELL-OFF-MASK and {: off:n :}
   count 0 ?do
      rows i SLOT-BYTES * + LE:U64@ {: prior:n :}
      prior SNAP-RELOC:XTCELL-OFF-MASK and off =
      prior raw <> and if false unloop exit then
   loop true ;

: SLOT-COMPATIBLE? ( n n -- bool ) {: raw:n row:n :}
   raw ADDRESS-CELLS:COMPATIBLE? 0= if false exit then
   PREP-N @ 1- 0 ?do
      i PREP-P @ {: block:ptr :}
      block NULL-PTR <> if
         block block 9 SECTION-OFF P@ +
         block 9 SECTION-OFF 1+ P@ SLOT-BYTES / raw
            SLOT-ROWS-COMPATIBLE? 0= if false unloop exit then
      then
   loop
   9 SECTION$ drop row raw SLOT-ROWS-COMPATIBLE? ;

: PREPARE-SLOT ( n -- ) {: row:n :}
   9 row SLOT-BYTES ROW-SECTION {: src:ptr :}
   src LE:U64@ CELL NFILE-REF:REF>DATA
      NULL-PTR BYTE-VIEW - {: site:n :}
   site DATA-VA VA>N - {: off:n :}
   src 8 + LE:U64@ {: flags:n :}
   flags 0 < flags 3 > or if BAD then
   off flags 2 and 0<> if SNAP-RELOC:XTCELL-DATA-TAG or then
      {: raw:n :}
   raw row SLOT-COMPATIBLE? 0= if BAD then
   flags 1 and {: place:n :}
   place 1 = if
      site IMPORT-DATA-BASE @ < if BAD then
      6 SECTION$ nip IMPORT-DATA-BASE @ +
         site CELL + < if BAD then
   else place 0= if
      site IMPORT-DATA-BASE @ NUNIT-CAPTURE:OWNED-CELL? 0= if BAD then
   else BAD then then
   src 16 + LE:U64@ 0<> if BAD then
   src 24 + LE:U64@ {: ref:n :}
   src 32 + LE:U64@ 0<> if BAD then
   ref 0= if 0
   else ref NFILE-REF:REF-KIND {: kind:n :}
      kind NFILE-REF:CODE = if ref NFILE-REF:REF>CODE
      else kind NFILE-REF:DATA = if
         ref 0 NFILE-REF:REF>DATA NULL-PTR BYTE-VIEW -
      else kind NFILE-REF:LITERAL = if
         ref NFILE-REF:REF>LITERAL IMPORT-LITERAL-REF
      else BAD then then then
   then {: target:n :}
   target src 24 + LE:U64!
   raw src LE:U64! ;

: PREPARE-SLOTS ( -- )
   9 SECTION$ nip {: size:n :}
   size SLOT-BYTES mod 0<> if BAD then
   size SLOT-BYTES / 0 ?do i PREPARE-SLOT loop ;

: IMPORT-SLOT ( n -- ) {: row:n :}
   9 row SLOT-BYTES ROW-SECTION {: src:ptr :}
   src LE:U64@ {: raw:n :}
   raw SNAP-RELOC:XTCELL-OFF-MASK and DATA-VA VA>N + {: site:n :}
   src 24 + LE:U64@ {: target:n :}
   site target raw STORE-TYPED ;

: IMPORT-SLOTS ( -- )
   9 SECTION$ nip {: size:n :}
   size SLOT-BYTES / 0 ?do i IMPORT-SLOT loop ;

: SAVE-PREPARED-CODE ( -- )
   0 SECTION$ drop {: code:ptr :}
   CODE-PTR code CODE-U @ BYTE-COPY
   1 SECTION$ drop {: recs:ptr :}
   REC-PTR recs REC-U @ BYTE-COPY ;

: SAVE-PREPARED-SITES ( -- )
   CALL-SITES$ {: calls:ptr calln:n :}
   PREP-ACTIVE @ {: block:ptr :}
   calln block P-CALL-N P!
   calls BYTE-VIEW block block P-CALL-OFF P@ +
      calln cells BYTE-COPY
   ADDR-SITES$ {: addrs:ptr addrn:n :}
   addrn block P-ADDR-N P!
   addrs BYTE-VIEW block block P-ADDR-OFF P@ +
      addrn cells BYTE-COPY ;

: PREPARED-CALLS$ ( -- ptr n n )
   PREP-ACTIVE @ {: block:ptr :}
   block block P-CALL-OFF P@ + CELL-VIEW block P-CALL-N P@ ;
: PREPARED-ADDRS$ ( -- ptr n n )
   PREP-ACTIVE @ {: block:ptr :}
   block block P-ADDR-OFF P@ + CELL-VIEW block P-ADDR-N P@ ;

: PREPARE-OPEN ( -- )
   PROJECT-ON @ 0= if BAD then
   0 COPY-IMPORT 1 COPY-IMPORT
   IMPORT-PROTECTION
   PROJECT-CODE @ IMPORT-CODE ! PROJECT-REC @ IMPORT-REC !
   PROJECT-WID @ IMPORT-WID !
   PREP-ACTIVE @ {: block:ptr :}
   IMPORT-CODE @ block P-CODE P! IMPORT-REC @ block P-REC P!
   IMPORT-WID @ block P-WID P!
   CODE-U @ block P-CODE-U P! REC-U @ block P-REC-U P!
   PROTECTION-U @ block P-WID-U P!
   IMPORT-WID @ PROTECTION-U @ + PROT-WID-MAX > if BAD then
   PREPARE-LITERALS
   PLAN-LITERAL-END IMPORT-DATA-BASE !
   IMPORT-DATA-BASE @ block P-DATA P!
   6 SECTION$ nip {: datau:n :}
   datau block P-DATA-U P!
   PREPARE-LITERAL-TAILS
   13 SECTION$ IMPORT-REC @ IMPORT-CODE @ CODE-U @
      IMPORT-WID @ PROTECTION-U @ IMPORT-DATA-BASE @ datau
      NFILE-REF:PREPARE
   PREPARE-RETIRE
   IMPORT-RECORDS NFILE-REF:INDEX-PREPARED-WIDS IMPORT-CALLS
   RESOLVE-LOCAL-LITERALS
   PREPARE-UPDATES PREPARE-REQUIRES
   datau PLAN-LITERAL-ADVANCE
   PREPARE-LITERAL-SEGMENTS
   IMPORT-ADDRS PREPARE-SLOTS
   PREPARE-CAPACITY
   SAVE-PREPARED-SITES SAVE-PREPARED-CODE
   PROJECT-CODE @ CODE-U @ + PROJECT-CODE !
   PROJECT-REC @ REC-U @ DREC / + PROJECT-REC !
   PROJECT-WID @ PROTECTION-U @ + PROJECT-WID !
   PLAN-LITERAL-END PROJECT-DATA ! ;

public

: BEGIN-PREPARATION ( -- )
   PROJECT-ON @ if BAD then
   PROJECT-START
   ['] PREP-RECORD NFILE-REF:RECORD-AT!
   PROJECT-DATA @ PLAN-LITERAL-BEGIN ;

: DISCARD-PREPARED ( -- )
   PREP-N @ 0 ?do i PREP-DROP loop
   PROJECT-ON @ if PLAN-LITERAL-CLOSE then
   0 PROJECT-ON !
   0 PROJECT-CODE ! 0 PROJECT-REC ! 0 PROJECT-WID ! 0 PROJECT-DATA ! ;

: DEP-CHECK ( -- bool )
   14 NUNIT-FILE:SECTION$ NUNIT-DEP:CHECK ;

: PREPARE-NAMED ( ptr u8 n ptr u8 n ptr u8 n -- n )
   {: path:ptr pathu:n key:ptr keyu:n name:ptr nameu:n :}
   PROJECT-ON @ 0= if BEGIN-PREPARATION then
   path pathu NUNIT-FILE:ARCH-AARCH64 name nameu key keyu NUNIT-FILE:READ
   11 NUNIT-FILE:SECTION$ NFILE-INPUTS:CHECK
   name nameu s" FILE" STR= if DEP-CHECK 0= if BAD then then
   PREP-ALLOC {: id:n :}
   NUNIT-FILE:CLOSE
   ['] PREPARE-OPEN catch {: rc:n :}
   rc 0<> if DISCARD-PREPARED rc throw then
   NULL-PTR PREP-ACTIVE !
   id ;

\ The importer is the running compiler's NPUB:IMPORT-FILE-UNIT.
: INSTALL-OPEN ( [ ptr u8 n ptr u8 n ptr n n ptr n n -- ] -- ptr u8 n )
   {: importer :}
   PREP-ACTIVE @ {: block:ptr :}
   block P-CODE P@ IMPORT-CODE ! block P-REC P@ IMPORT-REC !
   block P-WID P@ IMPORT-WID ! block P-DATA P@ IMPORT-DATA-BASE !
   block P-CODE-U P@ CODE-U ! block P-REC-U P@ REC-U !
   block P-WID-U P@ PROTECTION-U !
   INSTALL-LITERALS
   IMPORT-DATA IMPORT-UPDATES FINALIZE-LITERALS
   PROTECTION-U @ 0 ?do wordlist drop loop
   0 SECTION$ 1 SECTION$ DREC /
   PREPARED-CALLS$ PREPARED-ADDRS$ importer execute
   IMPORT-RETIRE
   IMPORT-SLOTS
   CALL-SITES-RELEASE ADDR-SITES-RELEASE
   RESTORE-PROTECTION
   IMPORT-REQUIRES
   5 SECTION$ ;

: INSTALL-PREPARED ( [ ptr u8 n ptr u8 n ptr n n ptr n n -- ] n bool -- ptr u8 n )
   {: importer id:n file?:bool :}
   id PREP-USE
   file? FILE-REQUIRES !
   file? if FORGET-FFI-CACHE then
   importer INSTALL-OPEN
   file? if NUNIT-CAPTURE:ARM-IMPORTED-CALLBACKS then ;

: IMPORT-NAMED ( [ ptr u8 n ptr u8 n ptr n n ptr n n -- ] ptr u8 n ptr u8 n ptr u8 n -- ptr u8 n )
   {: importer path:ptr pathu:n key:ptr keyu:n name:ptr nameu:n :}
   path pathu key keyu name nameu PREPARE-NAMED {: id:n :}
   importer id name nameu s" FILE" STR= INSTALL-PREPARED ;

: CLOSE-OBJECT ( -- )
   PREP-ACTIVE @ NULL-PTR <> if
      PREP-N @ 0 ?do
         i PREP-P @ PREP-ACTIVE @ = if i PREP-DROP leave then
      loop
   then
   0 IMPORT-CODE ! 0 IMPORT-REC ! 0 IMPORT-WID !
   0 IMPORT-DATA-BASE !
   CODE-BUF-RELEASE REC-BUF-RELEASE
   LITERAL-ADDR-RELEASE LITERAL-LEN-RELEASE
   LITERAL-LOCAL-RELEASE LITERAL-SEGS-RELEASE
   PROTECTION-RELEASE
   NUNIT-FILE:CLOSE ;

: CLOSE ( -- )
   CLOSE-OBJECT
   OUTER:SCOPE-CLOSE
   PREP-N @ 0 ?do i PREP-DROP loop
   PREP-P-RELEASE PREP-U-RELEASE
   0 PREP-N ! 0 PROJECT-ON !
   0 PROJECT-CODE ! 0 PROJECT-REC ! 0 PROJECT-WID ! 0 PROJECT-DATA !
   0 PREP-CURSOR !
   NFILE-INPUTS:CLOSE
   NFILE-REF:CLOSE
   NUNIT-CAPTURE:CLOSE
   CALL-SITES-RELEASE ADDR-SITES-RELEASE
   \ All remaining sizes and cursors describe only this unit's import.
   \ CLOSE-OBJECT is also used between replay steps, so retire them here.
   0 CODE-U ! 0 REC-U ! 0 LITERAL-N ! 0 LITERAL-CURSOR !
   0 LITERAL-SEG-N ! 0 LITERAL-PRIOR !
   0 REQUIRE-CURSOR ! 0 PROTECTION-U ! 0 FILE-REQUIRES ! ;

private

\ A definition payload uses the same outer envelope as FILE but owns no FILE
\ rows or DATA prefix. Section 1 is one 80-byte metadata row;
\ section 15 carries the function facts taken by the live native publisher.
40 constant DEF-CALL-BYTES
40 constant DEF-ADDR-BYTES
80 constant DEF-META-BYTES
DYNAMIC-BUFFER DEF-CODE u8
DYNAMIC-BUFFER DEF-CALL u8
DYNAMIC-BUFFER DEF-ADDR u8
DYNAMIC-BUFFER DEF-LIT u8
DYNAMIC-BUFFER DEF-HOST u8
DYNAMIC-BUFFER DEF-CHECK u8
DYNAMIC-BUFFER DEF-CALL-SITES n
DYNAMIC-BUFFER DEF-ADDR-SITES n
DYNAMIC-BUFFER DEF-LIT-ADDR n
DYNAMIC-BUFFER DEF-LIT-LEN n
DEF-META-BYTES CWORK-SPAN DEF-META u8
variable DEF-CODE-U
variable DEF-CALL-U
variable DEF-ADDR-U
variable DEF-LIT-U
variable DEF-HOST-U
variable DEF-CHECK-U
variable DEF-LIT-N
variable DEF-CALL-SITE-N
variable DEF-ADDR-SITE-N
variable DEF-PENDING
variable DEF-CODE-BASE
variable DEF-RECORDED
variable DEF-WIDE
variable DEF-MIN-IN
variable DEF-ACTIVE
variable DEF-EMITTED
variable DEF-FINISHED
variable DEF-PREPARED
variable DEF-CHECK-ID
variable DEF-PLAN-ON
variable DEF-REQUIRE-COMPLETE
variable DEF-MODULE-DELTA
variable DEF-GEN-DELTA
variable DEF-EPOCH-DELTA
variable DEF-BUILD-DELTA

SRCLOC:PATH-CELL RESERVED-PTR-U8-CELL DEF-DIAG-PATH

: DEF-DIAG-W2 ( ptr u8 n -- )
   {: bytes:ptr size:n :}
   0 begin dup size < while
      dup {: sent:n :}
      2 bytes sent + size sent - write
      dup 0 <= if drop E-FS-IO throw then
      +
   repeat drop ;

public
: REPORT-STORE-REFUSED ( ptr u8 n n -- )
   {: reason:ptr reasonu:n ord:n :}
   s" HABU_NATIVE_CACHE_DIAG" GETENV s" 1" STR= 0= if exit then
   s" native-cache: cache-store-refused reason=" DEF-DIAG-W2
   reason reasonu DEF-DIAG-W2
   s"  name=" DEF-DIAG-W2 ord XREF-REC XREF-NAME$ DEF-DIAG-W2
   data-base SRCLOC:PATHLEN-CELL + @ {: pathu:n :}
   pathu 0 > if
      s"  path=" DEF-DIAG-W2 DEF-DIAG-PATH @ pathu DEF-DIAG-W2
   then
   S\" \n" DEF-DIAG-W2 ;
private

CAST: DEF-ONE-XT ( n -- [ n -- n ] )
CAST: DEF-TWO-XT ( n -- [ n n -- n ] )
CAST: DEF-PTR-XT ( n -- [ n -- ptr u8 ] )
CAST: DEF-NUM-XT ( n -- [ -- n ] )
CAST: DEF-FACTS-XT ( n -- [ n -- n n n n n n ] )
CAST: DEF-PREP-HOST-XT ( n -- [ n n ptr u8 n ptr u8 n -- ] )
CAST: DEF-PUBLISH-XT ( n -- [ ptr u8 n n ptr n n ptr n n bool n -- n ] )
CAST: DEF-ACTION-XT ( n -- [ -- ] )
CAST: DEF-CHECK-EXPORT-XT ( n -- [ [ ptr u8 n -- n ] -- ptr u8 n ] )
CAST: DEF-CHECK-PREP-XT ( n -- [ ptr u8 n [ n n -- ptr u8 ] -- n ] )
CAST: DEF-CHECK-INSTALL-XT ( n -- [ n -- ] )
CAST: DEF-HOST-ABANDON-XT ( n -- [ -- ] )
CAST: DEF-PREP-BEGIN-XT ( n -- [ n -- ] )
CAST: DEF-LITERAL-XT ( n -- [ ptr u8 n -- n ] )
CAST: DEF-SOURCE-XT ( n -- [ n -- ptr u8 n ] )
CAST: DEF-HISTORY-XT ( n -- [ -- n n n n ] )
CAST: DEF-REPLAY-XT ( n -- [ n n n n -- ] )

: DEF-TARGET ( ptr u8 n -- n ) NUNIT-CAPTURE:BOUND-PRODUCER-XT ;
: DEF-ONE ( n ptr u8 n -- n )
   DEF-TARGET DEF-ONE-XT execute ;
: DEF-TWO ( n n ptr u8 n -- n )
   DEF-TARGET DEF-TWO-XT execute ;
: DEF-NUM ( ptr u8 n -- n )
   DEF-TARGET DEF-NUM-XT execute ;
: DEF-ELIGIBLE? ( -- bool )
   s" NCOMP:DEFINITION-CACHEABLE?"
      NUNIT-CAPTURE:OPTIONAL-PRODUCER-XT dup 0= if drop false exit then
   DEF-NUM-XT execute 0= if false exit then
   s" NCOMP:DEFINITION-HISTORY" NUNIT-CAPTURE:OPTIONAL-PRODUCER-XT 0<>
   s" NCOMP:REPLAY-DEFINITION" NUNIT-CAPTURE:OPTIONAL-PRODUCER-XT 0<> and
   s" NSTR:PREPARE-BEGIN" NUNIT-CAPTURE:OPTIONAL-PRODUCER-XT 0<> and
   s" NSTR:PREPARE-INTERN" NUNIT-CAPTURE:OPTIONAL-PRODUCER-XT 0<> and
   s" NSTR:PREPARE-DATA-END" NUNIT-CAPTURE:OPTIONAL-PRODUCER-XT 0<> and
   s" NSTR:PREPARE-CLOSE" NUNIT-CAPTURE:OPTIONAL-PRODUCER-XT 0<> and
   s" NSTR:INTERN" NUNIT-CAPTURE:OPTIONAL-PRODUCER-XT 0<> and
   s" NSTR:SOURCE-BEGIN" NUNIT-CAPTURE:OPTIONAL-PRODUCER-XT 0<> and
   s" NSTR:SOURCE-END" NUNIT-CAPTURE:OPTIONAL-PRODUCER-XT 0<> and
   s" NSTR:SOURCE-COUNT" NUNIT-CAPTURE:OPTIONAL-PRODUCER-XT 0<> and
   s" NSTR:SOURCE@" NUNIT-CAPTURE:OPTIONAL-PRODUCER-XT 0<> and
   s" NSTR:SOURCE-CLOSE" NUNIT-CAPTURE:OPTIONAL-PRODUCER-XT 0<> and
   s" NHOST:PREPARE-IMPORT" NUNIT-CAPTURE:OPTIONAL-PRODUCER-XT 0<> and
   s" NHOST:ABANDON-TAKEN" NUNIT-CAPTURE:OPTIONAL-PRODUCER-XT 0<> and
   s" NPUB:IMPORT-PENDING" NUNIT-CAPTURE:OPTIONAL-PRODUCER-XT 0<> and ;
: DEF-HISTORY-CAPTURE ( -- )
   s" NCOMP:DEFINITION-HISTORY" DEF-TARGET DEF-HISTORY-XT execute
      DEF-BUILD-DELTA ! DEF-EPOCH-DELTA ! DEF-GEN-DELTA !
      DEF-MODULE-DELTA ! ;
: DEF-HISTORY-CHECK ( -- )
   DEF-MODULE-DELTA @ 0 <= DEF-GEN-DELTA @ 0 <= or
   DEF-EPOCH-DELTA @ 0 <= or DEF-BUILD-DELTA @ 0 <= or if BAD then
   DEF-MODULE-DELTA @ IR-ID:SERIAL-CHECK drop
   DEF-GEN-DELTA @ DEF-EPOCH-DELTA @ IR-CTX:CHECK-HISTORY
   DEF-BUILD-DELTA @ IR-BUILD:CHECK-HISTORY ;
: DEF-HISTORY-REPLAY ( -- )
   DEF-MODULE-DELTA @ DEF-GEN-DELTA @ DEF-EPOCH-DELTA @ DEF-BUILD-DELTA @
   s" NCOMP:REPLAY-DEFINITION" DEF-TARGET DEF-REPLAY-XT execute ;
: DEF-PREP-BEGIN ( n -- )
   s" NSTR:PREPARE-BEGIN" DEF-TARGET DEF-PREP-BEGIN-XT execute ;
: DEF-PREP-INTERN ( ptr u8 n -- n )
   s" NSTR:PREPARE-INTERN" DEF-TARGET DEF-LITERAL-XT execute ;
: DEF-PREP-DATA-END ( -- n )
   s" NSTR:PREPARE-DATA-END" DEF-NUM ;
: DEF-INTERN ( ptr u8 n -- n )
   s" NSTR:INTERN" DEF-TARGET DEF-LITERAL-XT execute ;
: DEF-SOURCE-BEGIN ( -- )
   s" NSTR:SOURCE-BEGIN" DEF-TARGET DEF-ACTION-XT execute ;
: DEF-SOURCE-END ( -- )
   s" NSTR:SOURCE-END"
      NUNIT-CAPTURE:OPTIONAL-PRODUCER-XT dup 0= if drop exit then
   DEF-ACTION-XT execute ;
: DEF-SOURCE-COUNT ( -- n )
   s" NSTR:SOURCE-COUNT" DEF-NUM ;
: DEF-SOURCE@ ( n -- ptr u8 n )
   s" NSTR:SOURCE@" DEF-TARGET DEF-SOURCE-XT execute ;
: DEF-ABANDON-HOST ( -- )
   s" NHOST:ABANDON-TAKEN"
      NUNIT-CAPTURE:OPTIONAL-PRODUCER-XT dup 0= if drop exit then
   DEF-HOST-ABANDON-XT execute ;
: DEF-BYTES ( n -- ptr u8 )
   s" NART:BYTES" DEF-TARGET DEF-PTR-XT execute ;

: DEF-OWNER ( -- ptr u8 )
   data-base NCOMP-DISPATCH:DECL-CELL + 0 ptr-field @ ;
: DEF-CHECK-AVAILABLE? ( -- bool )
   DEF-OWNER {: owner:ptr :}
   owner NULL-PTR = if false exit then
   owner 16 - CELL-VIEW @ CHECKER-OWNER-ABI:MAGIC <> if false exit then
   owner 8 - CELL-VIEW @ $508 < if false exit then
   5 0 ?do
      owner $4D0 i cells + + CELL-VIEW @ 0= if false unloop exit then
   loop
   owner $500 + CELL-VIEW @ 0<> ;
: DEF-CHECK-XT ( n -- n )
   DEF-OWNER + CELL-VIEW @ dup 0= if BAD then ;
: DEF-CHECK-IDLE? ( -- bool )
   $500 DEF-CHECK-XT DEF-NUM-XT execute 0<> ;
: DEF-CHECK-MARK ( -- )
   $4D0 DEF-CHECK-XT DEF-ACTION-XT execute ;
: DEF-CHECK-DISCARD ( -- )
   DEF-CHECK-AVAILABLE? if $4F0 DEF-CHECK-XT DEF-ACTION-XT execute then ;

: DEF-CHECK-REF ( ptr u8 n -- n )
   {: rec:ptr bytes:n :}
   bytes DREC <> if BAD then
   rec NULL-PTR BYTE-VIEW - dbase@ - {: off:n :}
   off 0 < off DREC mod 0<> or if BAD then
   off DREC / NFILE-REF:REF-RECORD ;
: DEF-CHECK-RESOLVE ( n n -- ptr u8 )
   {: ref:n bytes:n :}
   bytes DREC <> if BAD then
   ref NFILE-REF:REF>RECORD XREF-REC BYTE-VIEW ;
: DEF-CHECK-EXPORT ( -- )
   ['] DEF-CHECK-REF $4D8 DEF-CHECK-XT
      DEF-CHECK-EXPORT-XT execute {: source:ptr bytes:n :}
   bytes 0 < if BAD then
   bytes DEF-CHECK-RESERVE
   source 0 DEF-CHECK bytes BYTE-COPY
   bytes DEF-CHECK-U ! ;
: DEF-CHECK-PREPARE ( ptr u8 n -- n )
   ['] DEF-CHECK-RESOLVE $4E0 DEF-CHECK-XT
      DEF-CHECK-PREP-XT execute ;
: DEF-CHECK-INSTALL ( n -- )
   $4E8 DEF-CHECK-XT DEF-CHECK-INSTALL-XT execute ;

: DEF-HOST-N ( n -- )
   {: value:n :}
   DEF-HOST-U @ 8 + DEF-HOST-RESERVE
   value DEF-HOST-U @ DEF-HOST LE:U64!
   8 DEF-HOST-U +! ;
: DEF-LIT-NUM ( n -- )
   {: value:n :}
   DEF-LIT-U @ 8 + DEF-LIT-RESERVE
   value DEF-LIT-U @ DEF-LIT LE:U64!
   8 DEF-LIT-U +! ;
: DEF-FIRST-ADDR ( n n -- n )
   {: address:n limit:n :}
   limit 0 ?do
      address i DEF-LIT-ADDR @ = if i unloop exit then
   loop
   limit ;

: DEF-CAPTURE-LITERALS ( -- )
   0 DEF-LIT-U !
   DEF-SOURCE-COUNT DEF-LIT-N !
   DEF-LIT-N @ DEF-LIT-ADDR-RESERVE
   DEF-LIT-N @ DEF-LIT-LEN-RESERVE
   DEF-LIT-N @ 0 ?do
      i DEF-SOURCE@ {: body:ptr bytes:n :}
      body NULL-PTR BYTE-VIEW - {: address:n :}
      address i DEF-LIT-ADDR !
      bytes i DEF-LIT-LEN !
      bytes DEF-LIT-NUM
      address i DEF-FIRST-ADDR DEF-LIT-NUM
      DEF-LIT-U @ bytes + DEF-LIT-RESERVE
      body DEF-LIT-U @ DEF-LIT bytes BYTE-COPY
      bytes DEF-LIT-U +!
   loop ;

: DEF-CALL+ ( n n n n n -- )
   {: site:n kind:n ref:n loc:n known:n :}
   DEF-CALL-U @ DEF-CALL-BYTES + DEF-CALL-RESERVE
   DEF-CALL-U @ DEF-CALL {: dst:ptr :}
   site dst LE:U64!
   kind dst 8 + LE:U64!
   ref dst 16 + LE:U64!
   loc dst 24 + LE:U64!
   known dst 32 + LE:U64!
   DEF-CALL-BYTES DEF-CALL-U +! ;
: DEF-ADDR+ ( n n n -- )
   {: site:n kind:n ref:n :}
   DEF-ADDR-U @ DEF-ADDR-BYTES + DEF-ADDR-RESERVE
   DEF-ADDR-U @ DEF-ADDR {: dst:ptr :}
   site dst LE:U64!
   kind dst 8 + LE:U64!
   0 dst 16 + LE:U64!
   ref dst 24 + LE:U64!
   0 dst 32 + LE:U64!
   DEF-ADDR-BYTES DEF-ADDR-U +! ;

: DEF-CAPTURE-CALLS ( n n n -- )
   {: emission:n fn:n size:n :}
   emission s" NART:CALL-SITES" DEF-ONE 0 ?do
      emission i s" NART:CALL-SITE@" DEF-TWO {: site:n :}
      emission i s" NART:CALL-KIND@" DEF-TWO {: kind:n :}
      emission i s" NART:CALL-TARGET@" DEF-TWO {: target:n :}
      site 0 < site 4 + size > or if BAD then
      target NFILE-REF:REF-CODE {: ref:n :}
      emission i s" NART:CALL-LOC@" DEF-TWO {: loc:n :}
      emission i s" NART:CALL-IMPL@" DEF-TWO 0<> if 1 else 0 then
         {: known:n :}
      site kind ref loc known DEF-CALL+
      target fn < target fn size + >= or if
         site DEF-CODE {: at:ptr :}
         kind NEMIT:CALL = if
            fn site + target NBR:BL-WORD at LE:U32!
         else
            fn site + target NBR:B-WORD at LE:U32!
         then
      then
   loop ;

: DEF-ADDR-REF ( n n n -- n )
   {: kind:n target:n owner:n :}
   kind A64IR:ADDR-CODE = if target NFILE-REF:REF-CODE exit then
   owner -2 <= if
      owner negate 2 - {: row:n :}
      row DEF-SOURCE-COUNT >= if BAD then
      row DEF-SOURCE@ {: body:ptr bytes:n :}
      target body NULL-PTR BYTE-VIEW - - {: addend:n :}
      addend 0 < addend bytes > or if BAD then
      row addend NFILE-REF:REF-LITERAL exit
   then
   owner 0 < if BAD then
   owner NFILE-REF:DATA-ORIGIN-BASE 0= if drop BAD then
   target <> if BAD then
   owner NUNIT-DEP:BIND-ORD
   owner 0 0 1 NFILE-REF:REF-DECL-DATA ;

: DEF-CAPTURE-ADDRS ( n n -- )
   {: emission:n size:n :}
   emission s" NART:ADDR-SITES" DEF-ONE 0 ?do
      emission i s" NART:ADDR-SITE@" DEF-TWO {: site:n :}
      emission i s" NART:ADDR-SITE-KIND@" DEF-TWO {: kind:n :}
      emission i s" NART:ADDR-SITE-ORIGIN@" DEF-TWO {: owner:n :}
      kind A64IR:ADDR-CODE = if ADDRESS-CARRIER:ADDR-CHAIN-BYTES
      else kind A64IR:ADDR-DATA = if ADDRESS-CARRIER:DATA-CHAIN-BYTES
      else BAD then then {: width:n :}
      site 0 < site width + size > or if BAD then
      site DEF-CODE {: carrier:ptr :}
      carrier 0 DEF-CODE size + ADDRESS-CARRIER:CHAIN-SIZE
         width <> if BAD then
      carrier width ADDRESS-CARRIER:CHAIN-VALUE {: target:n :}
      kind target owner DEF-ADDR-REF {: ref:n :}
      site kind ref DEF-ADDR+
      carrier 0 width ADDRESS-CARRIER:SET-CHAIN-VALUE
   loop ;

: DEF-CAPTURE-HOST ( n -- )
   {: emission:n :}
   0 DEF-HOST-U !
   s" NHOST:TAKEN-FUNCTIONS" DEF-NUM {: funs:n :}
   funs 0 <= if BAD then
   funs DEF-HOST-N
   funs 0 ?do
      i s" NHOST:TAKEN-FACTS" DEF-TARGET DEF-FACTS-XT execute
         {: len:n din:n dout:n reason:n loc:n types:n :}
      emission i s" NART:FUNCTION-OFFSET@" DEF-TWO DEF-HOST-N
      len DEF-HOST-N din DEF-HOST-N dout DEF-HOST-N
      reason DEF-HOST-N loc DEF-HOST-N types DEF-HOST-N
      types 0 > if
         types 0 ?do
            j i s" NHOST:TAKEN-TYPE@" DEF-TWO DEF-HOST-N
         loop
      then
   loop ;

: DEF-REGISTER-FUNCTIONS ( -- )
   0 DEF-HOST LE:U64@ {: funs:n :}
   8
   funs 0 ?do
      {: cursor:n :}
      cursor DEF-HOST {: row:ptr :}
      i 0<> if
         DEF-CODE-BASE @ row LE:U64@ +
            DEF-PENDING @ NFILE-REF:REGISTER-IMPL
      then
      cursor 7 cells + row 48 + LE:U64@ 0 max cells +
   loop drop ;

: DEF-CAPTURE ( n n n n -- )
   {: emission:n idx:n fn:n size:n :}
   DEF-ACTIVE @ 0= if exit then
   DEF-EMITTED @ if BAD then
   idx DEF-PENDING @ <> size 0 <= or if BAD then
   fn DEF-CODE-BASE !
   size DEF-CODE-RESERVE
   emission DEF-BYTES 0 DEF-CODE size BYTE-COPY
   size DEF-CODE-U !
   fn size idx NFILE-REF:BEGIN-DEFINITION-BOUNDS
   emission fn size DEF-CAPTURE-CALLS
   emission size DEF-CAPTURE-ADDRS
   emission DEF-CAPTURE-HOST
   1 DEF-EMITTED ! ;

variable DEF-REFUSED
variable DEF-HELD-E
variable DEF-HELD-IDX
variable DEF-HELD-FN
variable DEF-HELD-SIZE
: DEF-CAPTURE-HELD ( -- )
   DEF-HELD-E @ DEF-HELD-IDX @ DEF-HELD-FN @ DEF-HELD-SIZE @
      DEF-CAPTURE ;
: DEF-OBSERVE ( n n n n -- )
   {: emission:n idx:n fn:n size:n :}
   DEF-ACTIVE @ 0= if exit then
   emission DEF-HELD-E ! idx DEF-HELD-IDX !
   fn DEF-HELD-FN ! size DEF-HELD-SIZE !
   ['] DEF-CAPTURE-HELD catch {: rc:n :}
   rc 0= if exit then
   rc E-NUNIT-PROFILE <> if rc throw then
   s" capture-profile-unsupported" DEF-PENDING @ REPORT-STORE-REFUSED
   1 DEF-REFUSED ! ;

: DEF-FIELDS ( -- )
   DEF-PENDING @ XREF-REC {: rec:ptr :}
   rec XREF-START DEF-CODE-BASE @ <> if BAD then
   rec XREF-RAW-LEN DEF-RECORDED !
   rec XREF-WIDE? DEF-WIDE !
   rec XREF-FLAGS DNAME-MIN-IN-MASK and 52 rshift DEF-MIN-IN ! ;

: DEF-META! ( -- )
   DEF-RECORDED @ DEF-META LE:U64!
   DEF-CODE-U @ DEF-META 8 + LE:U64!
   DEF-WIDE @ if 1 else 0 then DEF-META 16 + LE:U64!
   DEF-MIN-IN @ DEF-META 24 + LE:U64!
   DEF-LIT-N @ DEF-META 32 + LE:U64!
   0 DEF-META 40 + LE:U64!
   DEF-MODULE-DELTA @ DEF-META 48 + LE:U64!
   DEF-GEN-DELTA @ DEF-META 56 + LE:U64!
   DEF-EPOCH-DELTA @ DEF-META 64 + LE:U64!
   DEF-BUILD-DELTA @ DEF-META 72 + LE:U64! ;

: DEF-PACK-SECTIONS ( ptr u8 n -- )
   {: deps:ptr depsu:n :}
   NUNIT-FILE:RESET-KEEP
   17 0 ?do i NULL-PTR 0 NUNIT-FILE:SECTION! loop
   0 0 DEF-CODE DEF-CODE-U @ NUNIT-FILE:SECTION!
   1 DEF-META DEF-META-BYTES NUNIT-FILE:SECTION!
   DEF-CALL-U @ 0 > if
      2 0 DEF-CALL DEF-CALL-U @ NUNIT-FILE:SECTION!
   then
   DEF-ADDR-U @ 0 > if
      3 0 DEF-ADDR DEF-ADDR-U @ NUNIT-FILE:SECTION!
   then
   DEF-CHECK-U @ 0 > if
      5 0 DEF-CHECK DEF-CHECK-U @ NUNIT-FILE:SECTION!
   then
   DEF-LIT-U @ 0 > if
      10 0 DEF-LIT DEF-LIT-U @ NUNIT-FILE:SECTION!
   then
   13 DEF-PENDING @ DEF-PENDING @ 1+ NFILE-REF:ENCODE
      NUNIT-FILE:SECTION!
   14 deps depsu NUNIT-FILE:SECTION!
   15 0 DEF-HOST DEF-HOST-U @ NUNIT-FILE:SECTION! ;

: DEF-RESET ( -- )
   0 DEF-ACTIVE ! 0 DEF-EMITTED ! 0 DEF-FINISHED !
   0 DEF-PREPARED ! 0 DEF-REFUSED !
   0 DEF-CODE-U ! 0 DEF-CALL-U ! 0 DEF-ADDR-U !
   0 DEF-LIT-U ! 0 DEF-HOST-U ! 0 DEF-CHECK-U !
   0 DEF-LIT-N ! 0 DEF-CALL-SITE-N ! 0 DEF-ADDR-SITE-N !
   0 DEF-PENDING ! 0 DEF-CODE-BASE ! 0 DEF-CHECK-ID !
   0 DEF-MODULE-DELTA ! 0 DEF-GEN-DELTA !
   0 DEF-EPOCH-DELTA ! 0 DEF-BUILD-DELTA !
   0 DEF-HELD-E ! 0 DEF-HELD-IDX ! 0 DEF-HELD-FN ! 0 DEF-HELD-SIZE !
   0 DEF-PLAN-ON ! 0 DEF-REQUIRE-COMPLETE ! ;

: DEF-DROP-PLAN ( -- n )
   DEF-SOURCE-END
   ['] DEF-CHECK-DISCARD catch
   ['] DEF-ABANDON-HOST catch
   over 0<> if drop else nip then
   NFILE-REF:END-CAPTURE
   DEF-RESET ;

: DEF-READ-META ( -- )
   1 NUNIT-FILE:SECTION$ {: meta:ptr size:n :}
   size DEF-META-BYTES <> if BAD then
   meta LE:U64@ DEF-RECORDED !
   meta 8 + LE:U64@ DEF-CODE-U !
   meta 16 + LE:U64@ dup 0<> over 1 <> and if BAD then 0<> DEF-WIDE !
   meta 24 + LE:U64@ DEF-MIN-IN !
   meta 32 + LE:U64@ DEF-LIT-N !
   meta 40 + LE:U64@ 0<> if BAD then
   meta 48 + LE:U64@ DEF-MODULE-DELTA !
   meta 56 + LE:U64@ DEF-GEN-DELTA !
   meta 64 + LE:U64@ DEF-EPOCH-DELTA !
   meta 72 + LE:U64@ DEF-BUILD-DELTA !
   ['] DEF-HISTORY-CHECK catch {: history-rc:n :}
   history-rc E-IR-MODULE-EXHAUSTED =
   history-rc E-IR-CTX-SERIALS = or
   history-rc E-IR-BUILD-SERIALS = or if BAD then
   history-rc 0<> if history-rc throw then
   DEF-RECORDED @ CODE-SPAN:VALID? 0= if BAD then
   DEF-RECORDED @ CODE-SPAN:BYTES DEF-CODE-U @ <> if BAD then
   DEF-CODE-U @ 0 <= DEF-CODE-U @ 3 and 0<> or if BAD then
   DEF-MIN-IN @ 0 < DEF-MIN-IN @ 255 > or if BAD then ;

: DEF-READ-LITERALS ( -- )
   10 NUNIT-FILE:SECTION$ {: src:ptr size:n :}
   0 DEF-LIT-U !
   DEF-LIT-N @ DEF-LIT-ADDR-RESERVE
   DEF-LIT-N @ DEF-LIT-LEN-RESERVE
   DEF-LIT-N @ 0 ?do
      size DEF-LIT-U @ - 16 < if BAD then
      src DEF-LIT-U @ + LE:U64@ {: bytes:n :}
      src DEF-LIT-U @ 8 + + LE:U64@ {: first:n :}
      first 0 < first i > or if BAD then
      bytes 0 < bytes size DEF-LIT-U @ 16 + - > or if BAD then
      bytes i DEF-LIT-LEN !
      src DEF-LIT-U @ 16 + + bytes DEF-PREP-INTERN
         {: address:n :}
      address i DEF-LIT-ADDR !
      address i DEF-FIRST-ADDR first <> if BAD then
      bytes 16 + DEF-LIT-U +!
   loop
   DEF-LIT-U @ size <> if BAD then ;

: DEF-PREP-CALL ( n -- )
   {: row:n :}
   2 NUNIT-FILE:SECTION$ drop row DEF-CALL-BYTES * + {: src:ptr :}
   src LE:U64@ {: site:n :}
   src 8 + LE:U64@ {: kind:n :}
   site 0 < DEF-CODE-U @ 4 < or if BAD then
   site DEF-CODE-U @ 4 - > if BAD then
   kind NEMIT:CALL <> kind NEMIT:TAIL <> and if BAD then
   src 16 + LE:U64@ NFILE-REF:REF>CODE {: target:n :}
   row DEF-CALL-BYTES * DEF-CALL {: dst:ptr :}
   src dst DEF-CALL-BYTES BYTE-COPY
   target dst 16 + LE:U64!
   site DEF-CODE {: at:ptr :}
   kind NEMIT:CALL = if
      at LE:U32@ NBR:BL? 0= if BAD then
      DEF-CODE-BASE @ site + target NBR:BL-WORD at LE:U32!
      target dbase@ < target dbase@ REGION + >= or if
         site DEF-CALL-SITE-N @ DEF-CALL-SITES !
         1 DEF-CALL-SITE-N +!
      then
   else
      at LE:U32@ NBR:B? 0= if BAD then
      DEF-CODE-BASE @ site + target NBR:B-WORD at LE:U32!
   then ;

: DEF-PREP-CALLS ( -- )
   2 NUNIT-FILE:SECTION$ {: src:ptr size:n :}
   src drop
   size DEF-CALL-BYTES mod 0<> if BAD then
   size DEF-CALL-RESERVE
   size DEF-CALL-U !
   size DEF-CALL-BYTES / DEF-CALL-SITES-RESERVE
   0 DEF-CALL-SITE-N !
   size DEF-CALL-BYTES / 0 ?do i DEF-PREP-CALL loop ;

: DEF-LIT-REF ( n -- n )
   NFILE-REF:REF>LITERAL {: row:n addend:n :}
   row 0 < row DEF-LIT-N @ >= or if BAD then
   addend 0 < addend row DEF-LIT-LEN @ > or if BAD then
   row DEF-LIT-ADDR @ addend + ;

: DEF-PREP-ADDR ( n -- )
   {: row:n :}
   3 NUNIT-FILE:SECTION$ drop row DEF-ADDR-BYTES * + {: src:ptr :}
   src LE:U64@ {: site:n :}
   src 8 + LE:U64@ {: kind:n :}
   kind A64IR:ADDR-CODE = if ADDRESS-CARRIER:ADDR-CHAIN-BYTES
   else kind A64IR:ADDR-DATA = if ADDRESS-CARRIER:DATA-CHAIN-BYTES
   else BAD then then {: width:n :}
   site 0 < width DEF-CODE-U @ > or if BAD then
   site DEF-CODE-U @ width - > if BAD then
   src 16 + LE:U64@ 0<> src 32 + LE:U64@ 0<> or if BAD then
   src 24 + LE:U64@ {: ref:n :}
   kind A64IR:ADDR-CODE = if ref NFILE-REF:REF>CODE else
      ref NFILE-REF:REF-KIND NFILE-REF:LITERAL = if
         ref DEF-LIT-REF
      else ref 0 NFILE-REF:REF>DATA NULL-PTR BYTE-VIEW - then
   then {: target:n :}
   site DEF-CODE {: carrier:ptr :}
   carrier 0 DEF-CODE DEF-CODE-U @ + ADDRESS-CARRIER:CHAIN-SIZE
      width <> if BAD then
   carrier target width ADDRESS-CARRIER:SET-CHAIN-VALUE
   site row DEF-ADDR-SITES ! ;

: DEF-PREP-ADDRS ( -- )
   3 NUNIT-FILE:SECTION$ {: src:ptr size:n :}
   src drop
   size DEF-ADDR-BYTES mod 0<> if BAD then
   size DEF-ADDR-RESERVE
   size DEF-ADDR-U !
   size DEF-ADDR-BYTES / DEF-ADDR-SITES-RESERVE
   size DEF-ADDR-BYTES / DEF-ADDR-SITE-N !
   size DEF-ADDR-BYTES / 0 ?do i DEF-PREP-ADDR loop ;

: DEF-PREP-OPEN ( -- )
   DEF-CHECK-AVAILABLE? 0= if BAD then
   DEF-REQUIRE-COMPLETE @ if
      14 NUNIT-FILE:SECTION$ NUNIT-DEP:COMPLETE? 0= if BAD then
   then
   14 NUNIT-FILE:SECTION$ NUNIT-DEP:CHECK 0= if BAD then
   DEF-READ-META
   0 NUNIT-FILE:SECTION$ {: source:ptr size:n :}
   size DEF-CODE-U @ <> if BAD then
   size DEF-CODE-RESERVE
   source 0 DEF-CODE size BYTE-COPY
   cp@ DEF-CODE-BASE !
   ['] XREF-REC NFILE-REF:RECORD-AT!
   13 NUNIT-FILE:SECTION$
      ndict@ DEF-CODE-BASE @ DEF-CODE-U @
      0 0 0 0 NFILE-REF:PREPARE
   AOT-ARM:HERE-N DEF-PREP-BEGIN
   1 DEF-PLAN-ON !
   DEF-READ-LITERALS
   DEF-PREP-DATA-END DATA-VA VA>N - DATA-SIZE > if BAD then
   DEF-PREP-CALLS DEF-PREP-ADDRS
   15 NUNIT-FILE:SECTION$ {: funcs:ptr funcu:n :}
   DEF-CODE-BASE @ DEF-CODE-U @ funcs funcu
      DEF-CALL-U @ 0= if NULL-PTR else 0 DEF-CALL then DEF-CALL-U @
      s" NHOST:PREPARE-IMPORT" DEF-TARGET DEF-PREP-HOST-XT execute
   5 NUNIT-FILE:SECTION$ DEF-CHECK-PREPARE DEF-CHECK-ID !
   1 DEF-PREPARED ! ;

: DEF-COPY-LITERALS ( -- )
   10 NUNIT-FILE:SECTION$ drop {: src:ptr :}
   0
   DEF-LIT-N @ 0 ?do
      {: cursor:n :}
      src cursor + LE:U64@ {: bytes:n :}
      src cursor 16 + + bytes DEF-INTERN
         i DEF-LIT-ADDR @ <> if BAD then
      cursor bytes 16 + +
   loop drop ;

: DEF-INSTALL ( -- n )
   DEF-PREPARED @ 0= if BAD then
   DEF-HISTORY-REPLAY
   DEF-COPY-LITERALS
   0 DEF-CODE DEF-CODE-U @ DEF-RECORDED @
      DEF-CALL-SITE-N @ 0= if NULL-PTR else 0 DEF-CALL-SITES then
      DEF-CALL-SITE-N @
      DEF-ADDR-SITE-N @ 0= if NULL-PTR else 0 DEF-ADDR-SITES then
      DEF-ADDR-SITE-N @
      DEF-WIDE @ DEF-MIN-IN @
      s" NPUB:IMPORT-PENDING" DEF-TARGET DEF-PUBLISH-XT execute
      {: ord:n :}
   DEF-CHECK-ID @ DEF-CHECK-INSTALL
   ord ;

0 CWORK-VARIABLE DEF-IN-BYTES ptr u8
0 CWORK-VARIABLE DEF-IN-SIZE n
0 CWORK-VARIABLE DEF-IN-KEY ptr u8
0 CWORK-VARIABLE DEF-IN-KEY-U n
: DEF-PREPARE-BODY ( -- )
   DEF-IN-BYTES @ DEF-IN-SIZE @
      NUNIT-FILE:ARCH-AARCH64 s" DEFINITION"
      DEF-IN-KEY @ DEF-IN-KEY-U @ NUNIT-FILE:READ-BYTES-REUSE
   DEF-PREP-OPEN ;

public

: WITH-DEFINITIONS ( [ -- ] -- )
   ['] DEF-OBSERVE swap NUNIT-CAPTURE:WITH-OBSERVER ;

: DEFINITION-BEGIN ( -- )
   DEF-RESET
   DEF-ELIGIBLE? 0= if
      s" begin-ineligible" ndict@ REPORT-STORE-REFUSED
      1 DEF-REFUSED ! exit
   then
   DEF-CHECK-AVAILABLE? 0= if
      s" checker-unavailable" ndict@ REPORT-STORE-REFUSED
      1 DEF-REFUSED ! exit
   then
   DEF-CHECK-IDLE? 0= if
      s" checker-busy" ndict@ REPORT-STORE-REFUSED
      1 DEF-REFUSED ! exit
   then
   ndict@ DEF-PENDING !
   1 DEF-ACTIVE !
   DEF-CHECK-MARK
   DEF-SOURCE-BEGIN ;

: DEFINITION-END ( -- bool )
   DEF-ACTIVE @ 0= if DEF-SOURCE-END false exit then
   0 DEF-ACTIVE !
   DEF-ELIGIBLE? 0= if
      s" end-ineligible" DEF-PENDING @ REPORT-STORE-REFUSED
      1 DEF-REFUSED !
   then
   DEF-EMITTED @ 0= DEF-REFUSED @ 0<> or if
      DEF-REFUSED @ 0<> if s" capture-refused" else s" no-emission" then
         DEF-PENDING @ REPORT-STORE-REFUSED
      DEF-SOURCE-END
      DEF-CHECK-DISCARD
      NFILE-REF:END-CAPTURE false exit
   then
   ndict@ DEF-PENDING @ 1+ <> if BAD then
   DEF-FIELDS
   DEF-REGISTER-FUNCTIONS
   DEF-CHECK-EXPORT
   DEF-CAPTURE-LITERALS
   DEF-SOURCE-END
   DEF-HISTORY-CAPTURE
   DEF-META!
   1 DEF-FINISHED !
   true ;

: PACK-DEFINITION ( ptr u8 n ptr u8 n -- ptr u8 n )
   {: key:ptr keyu:n deps:ptr depsu:n :}
   DEF-FINISHED @ 0= if BAD then
   deps depsu DEF-PACK-SECTIONS
   NUNIT-FILE:ARCH-AARCH64 s" DEFINITION" key keyu NUNIT-FILE:PACK ;

: PREPARE-DEFINITION-KIND? ( ptr u8 n ptr u8 n bool -- bool )
   {: bytes:ptr size:n key:ptr keyu:n complete:bool :}
   DEF-PLAN-ON @ if BAD then
   DEF-RESET
   complete DEF-REQUIRE-COMPLETE !
   DEF-ELIGIBLE? 0= if false exit then
   DEF-CHECK-AVAILABLE? 0= if false exit then
   DEF-CHECK-IDLE? 0= if false exit then
   bytes DEF-IN-BYTES ! size DEF-IN-SIZE !
   key DEF-IN-KEY ! keyu DEF-IN-KEY-U !
   ['] DEF-PREPARE-BODY catch {: rc:n :}
   rc 0= if true exit then
   DEF-DROP-PLAN {: cleanup:n :}
   rc E-NUNIT-PROFILE = rc E-NUNIT-FILE = or if
      cleanup 0<> if cleanup throw then
      false exit
   then
   rc throw ;

: PREPARE-DEFINITION? ( ptr u8 n ptr u8 n -- bool )
   false PREPARE-DEFINITION-KIND? ;

: PREPARE-DEFINITION-COMPLETE? ( ptr u8 n ptr u8 n -- bool )
   true PREPARE-DEFINITION-KIND? ;

: INSTALL-DEFINITION ( -- n )
   DEF-INSTALL ;

: DISCARD-DEFINITION ( -- )
   DEF-DROP-PLAN dup 0<> if throw then drop
   NUNIT-FILE:RESET-KEEP ;

: DEFINITIONS-CLOSE ( -- )
   s" NSTR:SOURCE-CLOSE"
      NUNIT-CAPTURE:OPTIONAL-PRODUCER-XT dup 0<> if
         DEF-ACTION-XT execute
      else drop then
   s" NSTR:PREPARE-CLOSE"
      NUNIT-CAPTURE:OPTIONAL-PRODUCER-XT dup 0<> if
         DEF-ACTION-XT execute
      else drop then
   DEF-RESET ;

;package
