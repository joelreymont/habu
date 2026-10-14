\ Explicit file object: code and dictionary rows, source-declared relocation
\ sites, DATA and typed cells, literal rows, wordlists and checker facts.
\ Address-bearing fields use local offsets, literal rows or exact-prefix
\ references. The source key binds that prefix and target to the object.

require src/lib/errors.f
require src/lib/le.f
require src/lib/memory.f
require src/habu/xref.f
require src/habu/address-carrier.f
require src/habu/aot-data-cell.f
require src/habu/sites.f
require src/compiler/native/a64ir.f
require src/compiler/native/branch.f
require tools/native-unit-capture.f
require tools/native-unit-file.f
require tools/native-file-inputs.f

package NUNIT-OBJECT

private

0 constant UNIT-REF
1 constant PREFIX-REF
2 constant LOCAL-DATA-REF
3 constant PREFIX-DATA-REF
4 constant NULL-REF
5 constant LITERAL-REF
\ Exact interior prefix code address, relative to dbase@ and before FIRST-CODE.
6 constant PREFIX-CODE-REF
\ Local WID refs use -2-row; sentinels lie below that bounded range.
0 PROT-WID-MAX - 2 - constant GLOBAL-WID-REF
GLOBAL-WID-REF 1- constant RETIRED-WID-REF
32 constant CALL-BYTES
40 constant ADDR-BYTES
40 constant SLOT-BYTES
4 constant INSN-BYTES
CAST: CODE-BYTES ( n -- ptr u8 )
CAST: ACTION-XT ( n -- [ -- ] )

DYNAMIC-BUFFER CODE-BUF u8
DYNAMIC-BUFFER REC-BUF u8
DYNAMIC-BUFFER CALL-BUF u8
DYNAMIC-BUFFER ADDR-BUF u8
DYNAMIC-BUFFER CALL-SITES n
DYNAMIC-BUFFER ADDR-SITES n
DYNAMIC-BUFFER DATA-BUF u8
DYNAMIC-BUFFER UPDATE-BUF u8
DYNAMIC-BUFFER REQUIRE-BUF u8
DYNAMIC-BUFFER SLOT-BUF u8
DYNAMIC-BUFFER LITERAL-BUF u8
DYNAMIC-BUFFER LITERAL-ADDR n
DYNAMIC-BUFFER LITERAL-LEN n
DYNAMIC-BUFFER LITERAL-LOCAL u8
DYNAMIC-BUFFER LITERAL-SEGS n
DYNAMIC-BUFFER RETIRE-BUF u8
variable CODE-U
variable REC-U
variable CALL-U
variable ADDR-U
variable DATA-U
variable UPDATE-U
variable REQUIRE-U
variable RETIRE-U
variable SLOT-U
variable LITERAL-U
variable LITERAL-N
variable LITERAL-CURSOR
variable LITERAL-OUT
variable LITERAL-SEG-N
variable LITERAL-OLD-LAST
variable LITERAL-OLD-WRITE
variable LITERAL-PRIOR
variable LITERAL-BEST
variable LITERAL-ADDEND
variable REQUIRE-CURSOR
DYNAMIC-BUFFER PROTECTION u8
variable PROTECTION-U
variable IMPORT-CODE
variable IMPORT-REC
variable IMPORT-WID
variable IMPORT-DATA-BASE
PTR-VARIABLE INPUTS-CHECK-P
variable INPUTS-CHECK-U
: BAD ( -- ) E-NUNIT-PROFILE throw ;

\ Each prepared range owns the section bytes that its publication consumes.
\ Section offsets stay valid while other ranges are prepared or installed.
16 constant PREP-SECTIONS
42 cells constant PREP-HEAD
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
15 constant P-CHECKER
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
: SECTION-OFF ( n -- n ) PREP-SECTIONS swap 2 * + ;
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

: PROJECT-RETIRED? ( n -- bool ) {: ord:n :}
   PREP-N @ 1- 0 ?do
      i PREP-P @ {: block:ptr :}
      block NULL-PTR <> if
         block 12 SECTION-OFF 1+ P@ 8 / 0 ?do
            block block 12 SECTION-OFF P@ + i 8 * + LE:U64@
               ord = if true unloop unloop exit then
         loop
      then
   loop false ;

: PREFIX-WID ( n -- n ) {: ord:n :}
   ord PREP-RECORD XREF-WORDLIST
   ord PROJECT-RETIRED? if drop XREF-RETIRED-WL then ;

: PREP-ALLOC ( -- n )
   PREP-HEAD PREP-CURSOR !
   13 0 ?do
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
   13 0 ?do
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

: CODE-START ( -- n ) NUNIT-CAPTURE:FIRST-CODE ;
: CODE-END ( -- n ) CODE-START CODE-U @ + ;
: WID-FIRST ( -- n ) NUNIT-CAPTURE:FIRST-WID ;

: CODE-PROFILE ( -- )
   NUNIT-CAPTURE:CODE$ {: a:ptr size:n :}
   size 0 < size INSN-BYTES mod 0<> or if BAD then
   size 0= if 0 CODE-U ! exit then
   size CODE-BUF-RESERVE
   a 0 CODE-BUF size BYTE-COPY
   size CODE-U ! ;

: RECORD-PROFILE ( -- )
   NUNIT-CAPTURE:RECORDS$ {: a:ptr size:n :}
   size 0 < size DREC mod 0<> or if BAD then
   size 0= if 0 REC-U ! exit then
   size REC-BUF-RESERVE
   a 0 REC-BUF size BYTE-COPY
   size REC-U ! ;

: CODE-PTR ( -- ptr u8 )
   CODE-U @ 0= if NULL-PTR exit then 0 CODE-BUF ;
: REC-PTR ( -- ptr u8 )
   REC-U @ 0= if NULL-PTR exit then 0 REC-BUF ;

: WID-SLOT ( n -- n ) {: wid:n :}
   wid 0= if GLOBAL-WID-REF exit then
   wid XREF-RETIRED-WL = if RETIRED-WID-REF exit then
   wid WID-FIRST >= if
      wid WID-FIRST NUNIT-CAPTURE:NEW-WIDS + >= if BAD then
      -2 wid WID-FIRST - - exit
   then
   NUNIT-CAPTURE:FIRST-REC 0 ?do
      i XREF-REC {: rec:ptr :}
      rec XREF-WORDLIST XREF-NAMESPACE-WL = if
         rec XREF-PKG-PUBLIC wid = if i 2 * unloop exit then
         rec XREF-PKG-PRIVATE wid = if i 2 * 1+ unloop exit then
      then
   loop
   BAD ;

: WID-IMPORT ( n -- n ) {: ref:n :}
   ref 0 >= if
      ref 2 / {: ord:n :}
      ord IMPORT-REC @ >= if BAD then
      ord PREP-RECORD {: rec:ptr :}
      rec XREF-WORDLIST XREF-NAMESPACE-WL <> if BAD then
      ref 1 and 0<> if rec XREF-PKG-PRIVATE else rec XREF-PKG-PUBLIC then exit
   then
   ref GLOBAL-WID-REF = if 0 exit then
   ref RETIRED-WID-REF = if XREF-RETIRED-WL exit then
   -2 ref - {: row:n :}
   row 0 < row PROTECTION-U @ >= or if BAD then
   IMPORT-WID @ row + ;

: PREFIX-ORD? ( n -- n bool ) {: target:n :}
   NUNIT-CAPTURE:FIRST-REC 0 ?do
      i XREF-REC {: rec:ptr :}
      i NUNIT-CAPTURE:PREFIX-WID@ {: wid:n :}
      wid XREF-NAMESPACE-WL <>
      wid XREF-RETIRED-WL <> and if
         rec XREF-START target = if i true unloop exit then
      then
   loop
   0 false ;

: PREFIX-ORD ( n -- n )
   PREFIX-ORD? if exit then drop BAD ;

: RECORD-ONE ( n -- ) {: row:n :}
   row NUNIT-CAPTURE:FIRST-REC + XREF-REC {: rec:ptr :}
   row DREC * REC-BUF {: dst:ptr :}
   rec XREF-EXT? if
      rec XREF-NAME-LEN {: bytes:n :}
      dst 24 + LE:U64@ {: name:n :}
      name CODE-START < name CODE-END > or if BAD then
      bytes CODE-END name - > if BAD then
      name CODE-START - dst 24 + LE:U64!
   then
   rec XREF-WORDLIST XREF-NAMESPACE-WL = if
      rec XREF-PKG-PUBLIC WID-SLOT dst LE:U64!
      rec XREF-PKG-PRIVATE WID-SLOT dst 8 + LE:U64!
      exit
   then
   rec XREF-WORDLIST WID-SLOT dst 40 + LE:U64!
   rec XREF-START {: start:n :}
   start CODE-START >= start CODE-END < and if
      start CODE-START - dst LE:U64!
   else
      start PREFIX-ORD -1 swap - dst LE:U64!
   then ;

: RECORDS-PROFILE ( -- )
   REC-U @ DREC / 0 ?do i RECORD-ONE loop ;

: RETIRE-PROFILE ( -- )
   0 RETIRE-U !
   NUNIT-CAPTURE:FIRST-REC 0 ?do
      i NUNIT-CAPTURE:PREFIX-WID@ {: old:n :}
      i XREF-REC XREF-WORDLIST {: current:n :}
      old current <> if
         old 0 < current XREF-RETIRED-WL <> or if BAD then
         RETIRE-U @ 8 + RETIRE-BUF-RESERVE
         i RETIRE-U @ RETIRE-BUF LE:U64!
         8 RETIRE-U +!
      then
   loop ;

: TARGET-REF ( n -- n n ) {: target:n :}
   target CODE-START >= target CODE-END < and if
      UNIT-REF target CODE-START - exit
   then
   target PREFIX-ORD? if PREFIX-REF swap exit then drop
   target dbase@ < target CODE-START >= or if BAD then
   PREFIX-CODE-REF target dbase@ - ;

: SITE-CK ( n n -- ) {: site:n span:n :}
   site 0 < site INSN-BYTES mod 0<> or if BAD then
   span CODE-U @ > site CODE-U @ span - > or if BAD then ;

: CALL-ONE ( n -- ) {: row:n :}
   row NUNIT-CAPTURE:CALL@ {: site:n kind:n target:n :}
   site INSN-BYTES SITE-CK
   target TARGET-REF {: domain:n ref:n :}
   row CALL-BYTES * CALL-BUF {: dst:ptr :}
   site dst LE:U64!
   kind dst 8 + LE:U64!
   domain dst 16 + LE:U64!
   ref dst 24 + LE:U64!
   domain PREFIX-REF = if
      site CODE-START + {: at:n :}
      kind NEMIT:CALL = if at at NBR:BL-WORD
      else kind NEMIT:TAIL = if at at NBR:B-WORD
      else BAD then then
      site CODE-BUF LE:U32!
   then ;

: CALLS-PROFILE ( -- )
   NUNIT-CAPTURE:CALLS CALL-BYTES * {: size:n :}
   size 0 > if size CALL-BUF-RESERVE then
   size CALL-U !
   NUNIT-CAPTURE:CALLS 0 ?do i CALL-ONE loop ;

\ At a shared row boundary Phil assigns the address to the later row.
: LITERAL-ADDRESS ( n -- n n bool ) {: target:n :}
   -1 LITERAL-BEST !
   LITERAL-N @ 0 ?do
      i LITERAL-ADDR @ {: base:n :}
      target base >= if
         target base - {: off:n :}
         off i LITERAL-LEN @ <= if
            i LITERAL-BEST ! off LITERAL-ADDEND !
         then
      then
   loop
   LITERAL-BEST @ 0 < if 0 0 false exit then
   LITERAL-BEST @ LITERAL-ADDEND @ true ;

: ADDRESS-REF ( n n -- n n n ) {: kind:n target:n :}
   kind A64IR:ADDR-CODE = if target TARGET-REF 0 exit then
   target LITERAL-ADDRESS if {: ordinal:n addend:n :}
      LITERAL-REF ordinal addend exit
   then 2drop
   NUNIT-CAPTURE:FIRST-DATA {: first:n :}
   target first >= target first DATA-U @ + < and if
      LOCAL-DATA-REF target first - 0 exit
   then
   target DATA-VA VA>N < target first >= or if BAD then
   PREFIX-DATA-REF target DATA-VA VA>N - 0 ;

: ADDR-SITE ( n n -- ) {: site:n kind:n :}
   kind A64IR:ADDR-CODE = if ADDRESS-CARRIER:ADDR-CHAIN-BYTES
   else kind A64IR:ADDR-DATA = if ADDRESS-CARRIER:DATA-CHAIN-BYTES
   else BAD then then {: width:n :}
   site width SITE-CK
   site CODE-BUF {: carrier:ptr :}
   carrier 0 CODE-BUF CODE-U @ + ADDRESS-CARRIER:CHAIN-SIZE
      width <> if BAD then
   carrier width ADDRESS-CARRIER:CHAIN-VALUE {: target:n :}
   kind target ADDRESS-REF {: domain:n ref:n addend:n :}
   ADDR-U @ ADDR-BYTES + ADDR-BUF-RESERVE
   ADDR-U @ ADDR-BUF {: dst:ptr :}
   site dst LE:U64!
   kind dst 8 + LE:U64!
   domain dst 16 + LE:U64!
   ref dst 24 + LE:U64!
   addend dst 32 + LE:U64!
   carrier 0 width ADDRESS-CARRIER:SET-CHAIN-VALUE
   ADDR-BYTES ADDR-U +! ;

: ADDR-ONE ( n -- )
   NUNIT-CAPTURE:ADDR@ ADDR-SITE ;

: ADDR-ROW? ( n -- bool ) {: site:n :}
   ADDR-U @ ADDR-BYTES / 0 ?do
      i ADDR-BYTES * ADDR-BUF LE:U64@ site = if true unloop exit then
   loop false ;

\ Adapt Phil's DIRECT-CARRIER seam: the source's site map identifies a
\ non-NPUB address carrier, and its dictionary record identifies its owner.
: DIRECT-OWNER? ( n n -- bool ) {: site:n width:n :}
   NUNIT-CAPTURE:RECORDS$ nip DREC / 0 ?do
      NUNIT-CAPTURE:FIRST-REC i + XREF-REC {: rec:ptr :}
      rec XREF-WORDLIST XREF-NAMESPACE-WL <> if
         rec XREF-START CODE-START site + =
         rec XREF-CODE-BYTES width 4 + >= and if true unloop exit then
         \ `defer` emits its dispatch-cell carrier inside the recorded body.
         \ The source site map is authoritative, but require the engine's
         \ defer trailer before treating an interior carrier as owned.
         rec XREF-START {: first:n :}
         first rec XREF-LEN + {: meta:n :}
         CODE-START site + first >=
         CODE-START site + width + meta <= and
         meta 16 + CODE-END <= and if
            meta CODE-START - CODE-BUF LE:U64@ DEFER-MAGIC = if
               true unloop exit
            then
         then
      then
   loop false ;

: DIRECT-SITE ( n n -- ) {: off:n sitekind:n :}
   sitekind SNAP-RELOC:SITE-ADDR <> if exit then
   off CODE-START dbase@ - - {: site:n :}
   site ADDR-ROW? if exit then
   site CODE-BUF {: carrier:ptr :}
   carrier 0 CODE-BUF CODE-U @ + ADDRESS-CARRIER:CHAIN-SIZE
      {: width:n :}
   width ADDRESS-CARRIER:DATA-CHAIN-BYTES <>
   width ADDRESS-CARRIER:ADDR-CHAIN-BYTES <> and if BAD then
   site width DIRECT-OWNER? 0= if BAD then
   width ADDRESS-CARRIER:DATA-CHAIN-BYTES = if A64IR:ADDR-DATA
   else A64IR:ADDR-CODE then {: kind:n :}
   kind A64IR:ADDR-DATA = if
      carrier width ADDRESS-CARRIER:CHAIN-VALUE {: target:n :}
      target NUNIT-CAPTURE:FIRST-DATA <
      target NUNIT-CAPTURE:FIRST-DATA DATA-U @ + >= or if BAD then
   then
   site kind ADDR-SITE ;

: ADDRS-PROFILE ( -- )
   0 ADDR-U !
   NUNIT-CAPTURE:ADDRS 0 ?do i ADDR-ONE loop
   CODE-START dbase@ - CODE-U @ ['] DIRECT-SITE SITES:EACH-IN-SPAN ;

: PROTECTED? ( n -- bool ) {: wid:n :}
   wid 0 < wid PROT-WID-MAX >= or if BAD then
   data-base PROT-BITS-OFF + BYTE-VIEW wid 3 rshift + c@
   wid 7 and rshift 1 and 0<> ;

: PROTECTION-PROFILE ( -- )
   NUNIT-CAPTURE:NEW-WIDS {: count:n :}
   count PROTECTION-RESERVE
   count PROTECTION-U !
   count 0 ?do
      WID-FIRST i + PROTECTED? if 1 else 0 then
      i PROTECTION c!
   loop ;

: DATA-PROFILE ( -- )
   NUNIT-CAPTURE:DATA$ {: a:ptr size:n :}
   size 0 < if BAD then
   size 0= if 0 DATA-U ! exit then
   size DATA-BUF-RESERVE
   a 0 DATA-BUF size BYTE-COPY
   size DATA-U ! ;

: DATA-REF ( n -- n n ) {: target:n :}
   NUNIT-CAPTURE:FIRST-DATA {: first:n :}
   target first >= target first DATA-U @ + < and if
      LOCAL-DATA-REF target first - exit
   then
   target DATA-VA VA>N < target first >= or if BAD then
   PREFIX-DATA-REF target DATA-VA VA>N - ;

CAST: LITERAL-SEGMENTS-XT ( n -- [ -- n ] )
CAST: LITERAL-ROWS-XT ( n -- [ n -- n n n n ] )
CAST: LITERAL-SPAN-XT ( n -- [ n -- n n ] )
CAST: LITERAL-INTERN-XT ( n -- [ ptr u8 n -- n ] )
CAST: LITERAL-ATTACH-XT ( n -- [ ptr u8 n n n -- ] )
CAST: LITERAL-PLAN-BEGIN-XT ( n -- [ n -- ] )
CAST: LITERAL-PLAN-ADDR-XT ( n -- [ ptr u8 n -- n ] )
CAST: LITERAL-PLAN-END-XT ( n -- [ -- n ] )
CAST: LITERAL-PLAN-ADVANCE-XT ( n -- [ n -- ] )
CAST: LITERAL-PLAN-SEGS-XT ( n -- [ ptr u8 n n n n -- ] )
CAST: LITERAL-PLAN-TAILS-XT ( n -- [ -- n n ] )
CAST: N-CELLS ( n -- ptr n )

: TARGET-XT ( ptr u8 n -- n )
   XREF-FIND dup XREF-FOUND? 0= if drop BAD then
   XREF-START ;

: FORGET-FFI-CACHE ( -- )
   s" FFI:FORGET-CACHE" TARGET-XT ACTION-XT execute ;

: PLAN-LITERAL-BEGIN ( n -- )
   s" NSTR:PREPARE-BEGIN" TARGET-XT
      LITERAL-PLAN-BEGIN-XT execute ;
: PLAN-LITERAL-ADDR ( ptr u8 n -- n )
   s" NSTR:PREPARE-INTERN" TARGET-XT
      LITERAL-PLAN-ADDR-XT execute ;
: PLAN-LITERAL-END ( -- n )
   s" NSTR:PREPARE-DATA-END" TARGET-XT
      LITERAL-PLAN-END-XT execute ;
: PLAN-LITERAL-ADVANCE ( n -- )
   s" NSTR:PREPARE-ADVANCE" TARGET-XT
      LITERAL-PLAN-ADVANCE-XT execute ;
: PLAN-LITERAL-SEGMENTS ( ptr u8 n n n n -- )
   s" NSTR:PREPARE-SEGMENTS" TARGET-XT
      LITERAL-PLAN-SEGS-XT execute ;
: PLAN-LITERAL-TAILS ( -- n n )
   s" NSTR:PREPARE-TAILS" TARGET-XT
      LITERAL-PLAN-TAILS-XT execute ;
: PLAN-LITERAL-CLOSE ( -- )
   s" NSTR:PREPARE-CLOSE" TARGET-XT ACTION-XT execute ;

: PRIVATE-LITERAL-XT ( ptr u8 n -- n )
   s" NSTR" XREF-NAMESPACE-WL XREF-FIND-WL
   dup XREF-FOUND? 0= if drop BAD then
   XREF-PKG-PRIVATE XREF-FIND-WL
   dup XREF-FOUND? 0= if drop BAD then
   XREF-START ;

\ Phil's literal codec records rows in source order. Rows added to an old
\ arena need their body bytes; complete new segments are already in DATA, so
\ their rows carry local offsets and their owner extents are listed up front.
: LITERAL-ROW-PROFILE ( n n n n bool -- )
   {: arena:n off:n bytes:n cap:n local?:bool :}
   off 0 < bytes 0 < or off cap > or
   bytes cap off - > or if BAD then
   LITERAL-OUT @ LITERAL-N @ >= if BAD then
   arena off + {: address:n :}
   address LITERAL-OUT @ LITERAL-ADDR !
   bytes LITERAL-OUT @ LITERAL-LEN !
   local? if
      address NUNIT-CAPTURE:FIRST-DATA - {: relative:n :}
      relative 0 < relative DATA-U @ > or
      bytes DATA-U @ relative - > or if BAD then
      LITERAL-U @ 24 + LITERAL-BUF-RESERVE
      1 LITERAL-U @ LITERAL-BUF LE:U64!
      relative LITERAL-U @ 8 + LITERAL-BUF LE:U64!
      bytes LITERAL-U @ 16 + LITERAL-BUF LE:U64!
      24 LITERAL-U +!
   else
      LITERAL-U @ 24 + bytes + LITERAL-BUF-RESERVE
      0 LITERAL-U @ LITERAL-BUF LE:U64!
      0 LITERAL-U @ 8 + LITERAL-BUF LE:U64!
      bytes LITERAL-U @ 16 + LITERAL-BUF LE:U64!
      address CODE-BYTES LITERAL-U @ 24 + LITERAL-BUF
         bytes BYTE-COPY
      24 bytes + LITERAL-U +!
   then
   1 LITERAL-OUT +! ;

: LITERALS-PROFILE ( -- )
   16 LITERAL-U !
   NUNIT-CAPTURE:NEW-LITERALS {: count:n :}
   count LITERAL-N !
   s" NSTR:SEGMENTS" TARGET-XT LITERAL-SEGMENTS-XT execute
      {: segments:n :}
   NUNIT-CAPTURE:FIRST-SEGMENTS {: old:n :}
   segments old < if BAD then
   segments old - {: fresh:n :}
   fresh LITERAL-SEG-N !
   16 fresh 16 * + LITERAL-BUF-RESERVE
   fresh 0 LITERAL-BUF LE:U64!
   count 8 LITERAL-BUF LE:U64!
   fresh 0 ?do
      old i + s" NSTR:SOURCE-SEGMENT-SPAN" TARGET-XT
         LITERAL-SPAN-XT execute {: owner:n span:n :}
      owner NUNIT-CAPTURE:FIRST-DATA - {: off:n :}
      off 0 < span 0 <= or off DATA-U @ > or
      span DATA-U @ off - > or if BAD then
      off LITERAL-U @ LITERAL-BUF LE:U64!
      span LITERAL-U @ 8 + LITERAL-BUF LE:U64!
      16 LITERAL-U +!
   loop
   count 0 > if
      count LITERAL-ADDR-RESERVE
      count LITERAL-LEN-RESERVE
   then
   0 LITERAL-CURSOR ! 0 LITERAL-OUT !
   segments 0 ?do
      i s" NSTR:SOURCE-ROWS" TARGET-XT LITERAL-ROWS-XT execute
         {: arena:n rows:n offsets:n lengths:n :}
      i s" NSTR:SOURCE-SPAN" TARGET-XT LITERAL-SPAN-XT execute
         {: span:n cap:n :}
      span arena <> if BAD then
      rows 0 ?do
         LITERAL-CURSOR @ i + NUNIT-CAPTURE:FIRST-LITERAL >= if
            i cells offsets + N-CELLS @ {: off:n :}
            i cells lengths + N-CELLS @ {: bytes:n :}
            arena off bytes cap j old >= LITERAL-ROW-PROFILE
         then
      loop
      rows LITERAL-CURSOR +!
   loop
   LITERAL-OUT @ count <> if BAD then ;

: SLOT-OWNED? ( n -- bool ) {: site:n :}
   site NUNIT-CAPTURE:FIRST-DATA >= if
      site NUNIT-CAPTURE:FIRST-DATA DATA-U @ + CELL - <= exit
   then
   site NUNIT-CAPTURE:FIRST-DATA NUNIT-CAPTURE:OWNED-CELL? ;

: SLOT-OVERLAP? ( n n -- bool ) {: at:n bytes:n :}
   SLOT-U @ SLOT-BYTES / 0 ?do
      i SLOT-BYTES * SLOT-BUF LE:U64@ {: raw:n :}
      raw SNAP-RELOC:XTCELL-OFF-MASK and DATA-VA VA>N + {: first:n :}
      at first CELL + < at bytes + first > and if true unloop exit then
   loop false ;

variable FILE-STRICT
variable FILE-ADMITTED
variable FILE-CHECK
variable ADMIT-LAST
\ A saved builder may replay without exporting; admission state ends here.
: ADMISSION-RESET ( -- )
   0 FILE-STRICT !
   0 FILE-ADMITTED !
   0 FILE-CHECK !
   0 ADMIT-LAST ! ;
: DECLINE-CELL ( n n -- ) {: site:n value:n :}
   0 FILE-ADMITTED !
   FILE-STRICT @ 0= if exit then
   value AOT-CELL:TEXT-PTR? if site value AOT-CELL:REFUSE-UNDECLARED then
   site value AOT-CELL:REFUSE-MAPPED ;

: SLOT-ONE ( n -- ) {: raw:n :}
   raw SNAP-RELOC:XTCELL-OFF-MASK and DATA-VA VA>N + {: site:n :}
   site SLOT-OWNED? 0= if exit then
   site CODE-BYTES CELL-VIEW @ {: target:n :}
   target 0= if NULL-REF 0 0
   else raw SNAP-RELOC:XTCELL-DATA-TAG and 0<> if
      target LITERAL-ADDRESS if {: ord:n addend:n :}
         LITERAL-REF ord addend
      else
         2drop
         \ FILE cannot encode a builder-only mapping as a DATA reference. Name
         \ the declared cell here, before a unit can publish that address.
         target AOT-CELL:DATA-ADDRESS? 0= if
            target AOT-CELL:TEXT-PTR? target AOT-CELL:MAPPED? or if
               site target DECLINE-CELL exit
            then
         then
         target DATA-REF 0
      then
   else target TARGET-REF 0 then then
      {: domain:n ref:n addend:n :}
   SLOT-U @ SLOT-BYTES + SLOT-BUF-RESERVE
   SLOT-U @ SLOT-BUF {: dst:ptr :}
   raw dst LE:U64!
   site NUNIT-CAPTURE:FIRST-DATA >= if 1 else 0 then dst 8 + LE:U64!
   domain dst 16 + LE:U64!
   ref dst 24 + LE:U64!
   addend dst 32 + LE:U64!
   site NUNIT-CAPTURE:FIRST-DATA >= if
      site NUNIT-CAPTURE:FIRST-DATA - {: off:n :}
      CELL 0 ?do 0 off i + DATA-BUF c! loop
   then
   SLOT-BYTES SLOT-U +! ;

: SLOTS-PROFILE ( -- )
   0 SLOT-U !
   ADDRESS-CELLS:LIVE-SPAN {: rows:ptr count:n :}
   count 0 ?do
      FILE-ADMITTED @ 0= if leave then
      rows i cells + @ SLOT-ONE
   loop ;

\ Sparse changes to owned preloaded DATA, as DATA-base offset/u8 pairs.
: UPDATE-BYTE ( n n -- ) {: site:n previous:n :}
   site CODE-BYTES c@ previous = if exit then
   site 1 SLOT-OVERLAP? if exit then
   UPDATE-U @ 16 + UPDATE-BUF-RESERVE
   site DATA-VA VA>N - UPDATE-U @ UPDATE-BUF LE:U64!
   site CODE-BYTES c@ UPDATE-U @ 8 + UPDATE-BUF LE:U64!
   16 UPDATE-U +! ;

: UPDATE-SPAN ( n ptr u8 n -- )
   {: first:n before:ptr size:n :}
   size 0 ?do first i + before i + c@ UPDATE-BYTE loop ;

: UPDATES-PROFILE ( -- )
   NUNIT-CAPTURE:PREFIX-BEFORE$ {: before:ptr size:n :}
   NUNIT-CAPTURE:PREFIX-DATA {: first:n :}
   0 UPDATE-U !
   first before size UPDATE-SPAN
   NUNIT-CAPTURE:OWNED-SPANS 0 ?do
      i NUNIT-CAPTURE:OWNED@ UPDATE-SPAN
   loop ;

CAST: LITERAL-OWNER-XT ( n -- [ n -- ptr u8 n bool ] )
: TARGET-LITERAL-BYTE? ( n -- bool )
   s" NSTR:OWNER-ROW" TARGET-XT LITERAL-OWNER-XT execute
   {: body:ptr size:n found:bool :}
   body drop size drop found ;
: ADMIT-CELL ( n -- ) {: site:n :}
   FILE-ADMITTED @ 0= if exit then
   site CELL SLOT-OVERLAP? if exit then
   site CODE-BYTES CELL-VIEW @ {: value:n :}
   value AOT-CELL:TEXT-PTR? value AOT-CELL:MAPPED? or if
      site AOT-CELL:NONCELL-WINDOW? 0=
      site [: TARGET-LITERAL-BYTE? ;] AOT-CELL:LITERAL-OVERLAP? 0= and if
         site value DECLINE-CELL
      then
   then ;
: ADMIT-START ( n -- n ) CELL 1- - DATA-VA VA>N max ;
: ADMIT-END ( n -- n ) DATA-VA VA>N DATA-SIZE + CELL - min ;
: ADMIT-FRESH ( -- )
   DATA-U @ 0= if exit then
   NUNIT-CAPTURE:FIRST-DATA {: first:n :}
   first DATA-U @ + 1- ADMIT-END {: last:n :}
   first ADMIT-START
   begin dup last <= FILE-ADMITTED @ 0<> and while
      dup ADMIT-CELL 1+
   repeat drop ;
: ADMIT-UPDATES ( -- )
   -1 ADMIT-LAST !
   UPDATE-U @ 16 / 0 ?do
      FILE-ADMITTED @ 0= if leave then
      i 16 * UPDATE-BUF LE:U64@ DATA-VA VA>N + {: byte:n :}
      byte ADMIT-LAST @ < if -1 ADMIT-LAST ! then
      byte ADMIT-START
      begin dup byte ADMIT-END <= FILE-ADMITTED @ 0<> and while
         dup ADMIT-LAST @ > if dup ADMIT-CELL dup ADMIT-LAST ! then
         1+
      repeat drop
   loop ;
: ADMIT-FILE ( -- bool )
   AOT-ARM:B0 @ AOT-CELL:PREPARE
   ADMIT-FRESH
   FILE-ADMITTED @ if ADMIT-UPDATES then
   AOT-CELL:RESET
   FILE-ADMITTED @ 0<> ;

CAST: REQUIRE-LEN-XT ( n -- [ n -- n ] )
CAST: REQUIRE-SLOT-XT ( n -- [ n -- ptr u8 ] )
CAST: PROVIDED-XT ( n -- [ ptr u8 n -- ] )
variable FILE-REQUIRES

: REQUIRE-LEN ( n -- n )
   s" REQUIRE-REG:LEN" TARGET-XT REQUIRE-LEN-XT execute ;
: REQUIRE-SLOT ( n -- ptr u8 )
   s" REQUIRE-REG:SLOT" TARGET-XT REQUIRE-SLOT-XT execute ;

: REQUIRES-PROFILE ( -- )
   0 REQUIRE-U !
   NUNIT-CAPTURE:NEW-REQUIRES 0 ?do
      NUNIT-CAPTURE:FIRST-REQUIRE i + {: row:n :}
      row REQUIRE-LEN {: bytes:n :}
      bytes 0 <= bytes PATH-CAP > or if BAD then
      REQUIRE-U @ 8 + bytes + REQUIRE-BUF-RESERVE
      bytes REQUIRE-U @ REQUIRE-BUF LE:U64!
      row REQUIRE-SLOT REQUIRE-U @ 8 + REQUIRE-BUF bytes BYTE-COPY
      8 bytes + REQUIRE-U +!
   loop ;

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

: IMPORT-REF ( n n -- n ) {: domain:n ref:n :}
   domain UNIT-REF = if
      ref 0 < ref CODE-U @ >= or ref INSN-BYTES mod 0<> or if BAD then
      IMPORT-CODE @ ref + exit
   then
   domain PREFIX-REF = if
      ref 0 < ref IMPORT-REC @ >= or if BAD then
      ref PREP-RECORD {: rec:ptr :}
      ref PREFIX-WID {: wid:n :}
      wid XREF-NAMESPACE-WL = wid XREF-RETIRED-WL = or if BAD then
      rec XREF-START exit
   then
   domain PREFIX-CODE-REF = if
      ref 0 < ref INSN-BYTES mod 0<> or
      ref IMPORT-CODE @ dbase@ - >= or if BAD then
      dbase@ ref + exit
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
      dst LE:U64@ WID-IMPORT dst LE:U64!
      dst 8 + LE:U64@ WID-IMPORT dst 8 + LE:U64!
      exit
   then
   wid WID-IMPORT dst 40 + LE:U64!
   dst LE:U64@ {: off:n :}
   off 0 < if
      -1 off - {: ord:n :}
      ord IMPORT-REC @ >= if BAD then
      ord PREP-RECORD XREF-START dst LE:U64!
   else
      off CODE-U @ >= off INSN-BYTES mod 0<> or if BAD then
      IMPORT-CODE @ off + dst LE:U64!
   then ;

: IMPORT-RECORDS ( -- )
   REC-U @ DREC / 0 ?do i IMPORT-RECORD loop ;

: PREPARE-RETIRE ( -- )
   12 SECTION$ {: rows:ptr size:n :}
   size 8 mod 0<> if BAD then
   size 8 / 0 ?do
      rows i 8 * + LE:U64@ {: ord:n :}
      ord 0 < ord IMPORT-REC @ >= or if BAD then
      i 0<> if
         rows i 1- 8 * + LE:U64@ ord >= if BAD then
      then
      ord PREFIX-WID 0 < if BAD then
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
   src 16 + LE:U64@ src 24 + LE:U64@ IMPORT-REF {: target:n :}
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
   s" NSTR:SEGMENT-BYTES" TARGET-XT
      LITERAL-SEGMENTS-XT execute {: required:n :}
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
         s" NSTR:INTERN" TARGET-XT LITERAL-INTERN-XT execute drop
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
   s" ATTACH-SEGMENTS" PRIVATE-LITERAL-XT
      LITERAL-ATTACH-XT execute ;

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
   src 16 + LE:U64@ {: domain:n :}
   src 24 + LE:U64@ {: ref:n :}
   src 32 + LE:U64@ {: addend:n :}
   kind A64IR:ADDR-CODE = if
      addend 0<> if BAD then
      domain ref IMPORT-REF
   else
      ref 0 < if BAD then
      domain LITERAL-REF = if ref addend IMPORT-LITERAL-REF
      else
         addend 0<> if BAD then
         domain LOCAL-DATA-REF = if
         6 SECTION$ nip ref <= if BAD then
         IMPORT-DATA-BASE @ ref +
         else domain PREFIX-DATA-REF = if
            DATA-VA VA>N ref + dup IMPORT-DATA-BASE @ >= if BAD then
         else BAD then then
      then
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
      src 16 + LE:U64@ PREFIX-REF =
      src 16 + LE:U64@ PREFIX-CODE-REF = or
      src 8 + LE:U64@ NEMIT:CALL = and
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
   IMPORT-DATA-BASE @ DATA-VA VA>N - {: span:n :}
   size 16 / 0 ?do
      src i 16 * + LE:U64@ {: off:n :}
      off 0 < off span >= or if BAD then
      DATA-VA VA>N off + {: site:n :}
      site IMPORT-DATA-BASE @ NUNIT-CAPTURE:OWNED-BYTE? 0= if BAD then
      src i 16 * 8 + + LE:U64@ {: byte:n :}
      byte 0 < byte 255 > or if BAD then
   loop ;

: IMPORT-UPDATES ( -- )
   7 SECTION$ {: src:ptr size:n :}
   size 16 / 0 ?do
      src i 16 * + LE:U64@ DATA-VA VA>N + {: site:n :}
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
   src LE:U64@ {: raw:n :}
   raw row SLOT-COMPATIBLE? 0= if BAD then
   raw SNAP-RELOC:XTCELL-OFF-MASK and DATA-VA VA>N + {: site:n :}
   src 8 + LE:U64@ {: place:n :}
   place 1 = if
      site IMPORT-DATA-BASE @ < if BAD then
      6 SECTION$ nip IMPORT-DATA-BASE @ +
         site CELL + < if BAD then
   else place 0= if
      site IMPORT-DATA-BASE @ NUNIT-CAPTURE:OWNED-CELL? 0= if BAD then
   else BAD then then
   src 16 + LE:U64@ {: domain:n :}
   src 24 + LE:U64@ {: ref:n :}
   src 32 + LE:U64@ {: addend:n :}
   domain NULL-REF = if ref 0<> addend 0<> or if BAD then 0
   else domain LOCAL-DATA-REF = if
      addend 0<> if BAD then
      ref 0 < 6 SECTION$ nip ref <= or if BAD then
      IMPORT-DATA-BASE @ ref +
   else domain PREFIX-DATA-REF = if
      addend 0<> if BAD then
      ref 0 < if BAD then
      DATA-VA VA>N ref + dup IMPORT-DATA-BASE @ >= if BAD then
   else domain LITERAL-REF = if
      ref addend IMPORT-LITERAL-REF
   else
      addend 0<> if BAD then
      domain ref IMPORT-REF
   then then then then {: target:n :}
   target src 24 + LE:U64! ;

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

\ A parent file's observer spans a child hit too. The ordinary publisher has
\ no NART emission for imported bytes, so copy the already validated sites
\ from this child's object into the parent's append-only capture log.
: LOG-IMPORTED-SITES ( -- )
   2 SECTION$ nip CALL-BYTES / 0 ?do
      2 i CALL-BYTES ROW-SECTION {: row:ptr :}
      IMPORT-CODE @ row LE:U64@ +
      row 8 + LE:U64@
      row 24 + LE:U64@
      NUNIT-CAPTURE:IMPORT-CALL
   loop
   3 SECTION$ nip ADDR-BYTES / 0 ?do
      3 i ADDR-BYTES ROW-SECTION {: row:ptr :}
      IMPORT-CODE @ row LE:U64@ + row 8 + LE:U64@
      NUNIT-CAPTURE:IMPORT-ADDR
   loop ;

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
   PREPARE-RETIRE
   IMPORT-RECORDS IMPORT-CALLS
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

CAST: PREPARE-XT ( n -- [ -- ] )
: PREPARE-STORAGE ( -- )
   s" CHECKER-STORAGE-PREPARE" TARGET-XT {: xt:n :}
   xt AOT-ARM:B0 @ < xt cp@ >= or if BAD then
   xt PREPARE-XT execute ;

public

: BEGIN-PREPARATION ( -- )
   PROJECT-ON @ if BAD then
   PROJECT-START
   PROJECT-DATA @ PLAN-LITERAL-BEGIN ;

: DISCARD-PREPARED ( -- )
   PREP-N @ 0 ?do i PREP-DROP loop
   PROJECT-ON @ if PLAN-LITERAL-CLOSE then
   0 PROJECT-ON !
   0 PROJECT-CODE ! 0 PROJECT-REC ! 0 PROJECT-WID ! 0 PROJECT-DATA ! ;

: PROJECTED-COUNTS ( -- n n ) PROJECT-REC @ PROJECT-WID @ ;

: PREPARE-BYTES-NAMED ( ptr u8 n ptr u8 n ptr u8 n -- n )
   {: bytes:ptr size:n key:ptr keyu:n name:ptr nameu:n :}
   PROJECT-ON @ 0= if BEGIN-PREPARATION then
   bytes size NUNIT-FILE:ARCH-AARCH64 name nameu key keyu NUNIT-FILE:READ-BYTES
   11 NUNIT-FILE:SECTION$ NFILE-INPUTS:FORMAT
   name nameu s" FILE" STR= if NUNIT-CAPTURE:REFRESH-OWNERS then
   PREP-ALLOC {: id:n :}
   NUNIT-FILE:CLOSE
   ['] PREPARE-OPEN catch {: rc:n :}
   rc 0<> if DISCARD-PREPARED rc throw then
   NULL-PTR PREP-ACTIVE !
   id ;

: PREPARE-NAMED ( ptr u8 n ptr u8 n ptr u8 n -- n )
   {: path:ptr pathu:n key:ptr keyu:n name:ptr nameu:n :}
   PROJECT-ON @ 0= if BEGIN-PREPARATION then
   path pathu NUNIT-FILE:ARCH-AARCH64 name nameu key keyu NUNIT-FILE:READ
   11 NUNIT-FILE:SECTION$ NFILE-INPUTS:CHECK
   name nameu s" FILE" STR= if NUNIT-CAPTURE:REFRESH-OWNERS then
   PREP-ALLOC {: id:n :}
   NUNIT-FILE:CLOSE
   ['] PREPARE-OPEN catch {: rc:n :}
   rc 0<> if DISCARD-PREPARED rc throw then
   NULL-PTR PREP-ACTIVE !
   id ;

: PREPARE-SECTIONS-WITH-INPUTS ( ptr u8 n ptr u8 n -- bool )
   {: checker:ptr checku:n inputs:ptr inputu:n :}
   CODE-PROFILE RECORD-PROFILE RECORDS-PROFILE RETIRE-PROFILE
   DATA-PROFILE LITERALS-PROFILE SLOTS-PROFILE
   FILE-ADMITTED @ 0= if ADMISSION-RESET false exit then
   CALLS-PROFILE ADDRS-PROFILE PROTECTION-PROFILE
   UPDATES-PROFILE REQUIRES-PROFILE
   FILE-CHECK @ if ADMIT-FILE 0= if ADMISSION-RESET false exit then then
   NUNIT-FILE:RESET
   0 CODE-PTR CODE-U @ NUNIT-FILE:SECTION!
   1 REC-PTR REC-U @ NUNIT-FILE:SECTION!
   2 CALL-U @ 0= if CODE-PTR else 0 CALL-BUF then CALL-U @ NUNIT-FILE:SECTION!
   3 ADDR-U @ 0= if CODE-PTR else 0 ADDR-BUF then ADDR-U @ NUNIT-FILE:SECTION!
   4 PROTECTION-U @ 0= if CODE-PTR else 0 PROTECTION then
      PROTECTION-U @ NUNIT-FILE:SECTION!
   5 checker checku NUNIT-FILE:SECTION!
   6 DATA-U @ 0= if CODE-PTR else 0 DATA-BUF then
      DATA-U @ NUNIT-FILE:SECTION!
   7 UPDATE-U @ 0= if CODE-PTR else 0 UPDATE-BUF then UPDATE-U @ NUNIT-FILE:SECTION!
   8 REQUIRE-U @ 0= if CODE-PTR else 0 REQUIRE-BUF then REQUIRE-U @ NUNIT-FILE:SECTION!
   9 SLOT-U @ 0= if CODE-PTR else 0 SLOT-BUF then SLOT-U @ NUNIT-FILE:SECTION!
   10 LITERAL-U @ 0= if CODE-PTR else 0 LITERAL-BUF then
      LITERAL-U @ NUNIT-FILE:SECTION!
   11 inputs inputu NUNIT-FILE:SECTION!
   12 RETIRE-U @ 0= if CODE-PTR else 0 RETIRE-BUF then
      RETIRE-U @ NUNIT-FILE:SECTION!
   ADMISSION-RESET true ;

: PREPARE-SECTIONS ( ptr u8 n -- bool )
   NFILE-INPUTS:BYTES$ PREPARE-SECTIONS-WITH-INPUTS ;

: PREPARE-EXPORT ( ptr u8 n bool -- ) {: name:ptr nameu:n strict:bool :}
   PREPARE-STORAGE
   strict FILE-STRICT !
   1 FILE-ADMITTED !
   name nameu s" FILE" STR= dup FILE-CHECK ! if
      \ The map read resolves foreign symbols; clear that cache afterward so
      \ admission does not mistake its own newly owned pointer for source DATA.
      PROC-MAPS:RELOAD
      FORGET-FFI-CACHE
   then ;

: PACK-NAMED-INPUTS ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- ptr u8 n )
   {: key:ptr keyu:n checker:ptr checku:n name:ptr nameu:n
      inputs:ptr inputu:n :}
   name nameu true PREPARE-EXPORT
   checker checku inputs inputu PREPARE-SECTIONS-WITH-INPUTS drop
   NUNIT-FILE:ARCH-AARCH64 name nameu key keyu NUNIT-FILE:PACK ;

: PACK-FILE-INPUTS? ( ptr u8 n ptr u8 n ptr u8 n -- ptr u8 n bool )
   {: key:ptr keyu:n checker:ptr checku:n inputs:ptr inputu:n :}
   s" FILE" false PREPARE-EXPORT
   checker checku inputs inputu PREPARE-SECTIONS-WITH-INPUTS 0= if
      NULL-PTR 0 false exit
   then
   NUNIT-FILE:ARCH-AARCH64 s" FILE" key keyu NUNIT-FILE:PACK true ;

: PACK-NAMED ( ptr u8 n ptr u8 n ptr u8 n -- ptr u8 n )
   {: key:ptr keyu:n checker:ptr checku:n name:ptr nameu:n :}
   name nameu true PREPARE-EXPORT
   checker checku PREPARE-SECTIONS drop
   NUNIT-FILE:ARCH-AARCH64 name nameu key keyu NUNIT-FILE:PACK ;

: EXPORT-NAMED ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- )
   {: path:ptr pathu:n key:ptr keyu:n checker:ptr checku:n name:ptr nameu:n :}
   name nameu true PREPARE-EXPORT
   checker checku PREPARE-SECTIONS drop
   path pathu NUNIT-FILE:ARCH-AARCH64 name nameu key keyu NUNIT-FILE:WRITE ;

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
   LOG-IMPORTED-SITES
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

: PREPARED-INPUTS$ ( -- ptr u8 n ) 11 SECTION$ ;
: PREPARED-INPUTS-OF$ ( n -- ptr u8 n ) {: id:n :}
   id 0 < id PREP-N @ >= or if BAD then
   id PREP-P @ {: block:ptr :}
   block NULL-PTR = if BAD then
   block block 11 SECTION-OFF P@ +
   block 11 SECTION-OFF 1+ P@ ;

: PREPARED-CHECKER-OF$ ( n -- ptr u8 n n n ) {: id:n :}
   id PREP-USE
   PREP-ACTIVE @ {: block:ptr :}
   5 SECTION$
   block P-DATA P@ DATA-VA VA>N - {: floor:n :}
   floor dup block P-DATA-U P@ + ;

: PREPARED-CHECKER-ID! ( n n -- ) {: id:n checker:n :}
   id PREP-USE
   checker PREP-ACTIVE @ P-CHECKER P! ;

: PREPARED-CHECKER-ID ( n -- n ) {: id:n :}
   id PREP-USE
   PREP-ACTIVE @ P-CHECKER P@ ;

: IMPORT-BYTES-NAMED ( [ ptr u8 n ptr u8 n ptr n n ptr n n -- ] ptr u8 n ptr u8 n ptr u8 n -- ptr u8 n )
   {: importer bytes:ptr size:n key:ptr keyu:n name:ptr nameu:n :}
   bytes size key keyu name nameu PREPARE-BYTES-NAMED {: id:n :}
   importer id name nameu s" FILE" STR= INSTALL-PREPARED ;

: IMPORT-NAMED ( [ ptr u8 n ptr u8 n ptr n n ptr n n -- ] ptr u8 n ptr u8 n ptr u8 n -- ptr u8 n )
   {: importer path:ptr pathu:n key:ptr keyu:n name:ptr nameu:n :}
   path pathu key keyu name nameu PREPARE-NAMED {: id:n :}
   importer id name nameu s" FILE" STR= INSTALL-PREPARED ;

: CHECK-INPUTS ( -- )
   INPUTS-CHECK-P @ INPUTS-CHECK-U @ NFILE-INPUTS:CHECK ;

: INPUTS-VALID? ( ptr u8 n ptr u8 n ptr u8 n -- bool )
   {: path:ptr pathu:n key:ptr keyu:n name:ptr nameu:n :}
   path pathu NUNIT-FILE:ARCH-AARCH64 name nameu key keyu NUNIT-FILE:READ
   11 NUNIT-FILE:SECTION$ {: inputs:ptr inputu:n :}
   inputs INPUTS-CHECK-P ! inputu INPUTS-CHECK-U !
   ['] CHECK-INPUTS catch {: rc:n :}
   NUNIT-FILE:CLOSE
   rc 0= if true exit then
   rc E-NUNIT-PROFILE = if false exit then
   rc throw ;

: CLOSE-OBJECT ( -- )
   PREP-ACTIVE @ NULL-PTR <> if
      PREP-N @ 0 ?do
         i PREP-P @ PREP-ACTIVE @ = if i PREP-DROP leave then
      loop
   then
   0 IMPORT-CODE ! 0 IMPORT-REC ! 0 IMPORT-WID !
   0 IMPORT-DATA-BASE ! 0 LITERAL-OLD-LAST ! 0 LITERAL-OLD-WRITE !
   NULL-PTR INPUTS-CHECK-P ! 0 INPUTS-CHECK-U !
   CODE-BUF-RELEASE REC-BUF-RELEASE CALL-BUF-RELEASE ADDR-BUF-RELEASE
   DATA-BUF-RELEASE UPDATE-BUF-RELEASE REQUIRE-BUF-RELEASE
   RETIRE-BUF-RELEASE
   SLOT-BUF-RELEASE
   LITERAL-BUF-RELEASE LITERAL-ADDR-RELEASE LITERAL-LEN-RELEASE
   LITERAL-LOCAL-RELEASE LITERAL-SEGS-RELEASE
   PROTECTION-RELEASE
   NUNIT-FILE:CLOSE ;

: CLOSE ( -- )
   CLOSE-OBJECT
   PREP-N @ 0 ?do i PREP-DROP loop
   PREP-P-RELEASE PREP-U-RELEASE
   0 PREP-N ! 0 PROJECT-ON !
   0 PROJECT-CODE ! 0 PROJECT-REC ! 0 PROJECT-WID ! 0 PROJECT-DATA !
   0 PREP-CURSOR !
   NFILE-INPUTS:CLOSE
   NUNIT-CAPTURE:CLOSE
   CALL-SITES-RELEASE ADDR-SITES-RELEASE
   \ All remaining sizes and cursors describe only this unit's export or import.
   \ CLOSE-OBJECT is also used between replay steps, so retire them here.
   0 CODE-U ! 0 REC-U ! 0 CALL-U ! 0 ADDR-U !
   0 DATA-U ! 0 UPDATE-U ! 0 REQUIRE-U ! 0 RETIRE-U ! 0 SLOT-U !
   0 LITERAL-U ! 0 LITERAL-N ! 0 LITERAL-CURSOR ! 0 LITERAL-OUT !
   0 LITERAL-SEG-N ! 0 LITERAL-PRIOR ! 0 LITERAL-BEST ! 0 LITERAL-ADDEND !
   0 REQUIRE-CURSOR ! 0 PROTECTION-U ! 0 FILE-REQUIRES ! ;

;package
