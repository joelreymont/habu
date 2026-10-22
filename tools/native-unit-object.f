\ Native unit object for one cached definition: code and dictionary rows,
\ source-declared relocation sites, DATA and typed cells, literal rows and
\ checker facts. Address-bearing fields use local offsets, literal rows or
\ imported declaration identities.

require src/lib/errors.f
require src/lib/string.f
require src/lib/le.f
require src/lib/memory.f
require src/core/type-family.f
require src/habu/layout.f
require src/habu/xref.f
require src/habu/code-span.f
require src/habu/address-carrier.f
require src/compiler/native/a64ir.f
require src/compiler/native/branch.f
require src/compiler/workspace.f
require tools/native-unit-capture.f
require tools/native-file-reference.f
require tools/native-unit-dependency.f
require tools/native-unit-file.f
require tools/native-file-inputs.f

package NUNIT-OBJECT

private

: BAD ( -- ) E-NUNIT-PROFILE throw ;

\ A FILE object holds every compiled colon definition of one source file with
\ the file's input and dependency rows. Slot 0 holds the definitions' sections
\ back to back; slot 2 one INDEX-BYTES row per definition: its ordinal past
\ the file's first record, then the length of each DEF-SLOTS section in slot
\ order; slot 11 the input rows; slot 14 the dependency rows. Every other slot
\ is empty. A definition's own dependency slot stays out: the file's rows
\ cover it.
42031 constant DEF-SLOTS
18437 constant FILE-SLOTS
14 constant DEPS-SLOT
72 constant INDEX-BYTES
\ One frame per open file: the four buffers' used sizes when it opened, and
\ its first record ordinal. A child file's bytes follow its parent's and are
\ dropped when the child closes.
0 constant FO-DEFS
1 constant FO-INDEX
2 constant FO-INPUTS
3 constant FO-WHY
4 constant FO-FIRST
5 constant FO-CELLS
DYNAMIC-BUFFER FILE-DEFS u8
DYNAMIC-BUFFER FILE-INDEX u8
DYNAMIC-BUFFER FILE-INPUTS u8
DYNAMIC-BUFFER FILE-WHY u8
DYNAMIC-BUFFER FILE-FRAMES n
\ Each index row's dictionary ordinal, so a dictionary cut can find the rows
\ it retired. Never written to an object.
DYNAMIC-BUFFER FILE-ORDS n
variable DEFS-U
variable INDEX-U
variable INPUTS-U
variable WHY-U
variable FO-DEPTH
variable OPEN-DEFS
variable OPEN-ROWS
variable OPEN-INPUTS

\ Field n of file frame k, outermost first.
: FRAME ( n n -- ptr n ) swap FO-CELLS * + FILE-FRAMES ;

\ Field n of the innermost file frame.
: FO ( n -- ptr n )
   FO-DEPTH @ 0 <= if BAD then
   FO-DEPTH @ 1- swap FRAME ;

: DEFS+ ( ptr u8 n -- )
   {: a:ptr u:n :}
   u 0= if exit then
   DEFS-U @ u + FILE-DEFS-RESERVE
   a DEFS-U @ FILE-DEFS u BYTE-COPY
   u DEFS-U +! ;
: INPUTS+ ( ptr u8 n -- )
   {: a:ptr u:n :}
   u 0= if exit then
   INPUTS-U @ u + FILE-INPUTS-RESERVE
   a INPUTS-U @ FILE-INPUTS u BYTE-COPY
   u INPUTS-U +! ;
: WHY+ ( ptr u8 n -- )
   {: a:ptr u:n :}
   u 0= if exit then
   WHY-U @ u + FILE-WHY-RESERVE
   a WHY-U @ FILE-WHY u BYTE-COPY
   u WHY-U +! ;
: INDEX-N ( n -- )
   {: value:n :}
   INDEX-U @ 8 + FILE-INDEX-RESERVE
   value INDEX-U @ FILE-INDEX LE:U64!
   8 INDEX-U +! ;

: DEFS$ ( -- ptr u8 n )
   FO-DEFS FO @ {: first:n :}
   DEFS-U @ first = if NULL-PTR 0 exit then
   first FILE-DEFS DEFS-U @ first - ;
: INDEX$ ( -- ptr u8 n )
   FO-INDEX FO @ {: first:n :}
   INDEX-U @ first = if NULL-PTR 0 exit then
   first FILE-INDEX INDEX-U @ first - ;
: INPUTS$ ( -- ptr u8 n )
   FO-INPUTS FO @ {: first:n :}
   INPUTS-U @ first = if NULL-PTR 0 exit then
   first FILE-INPUTS INPUTS-U @ first - ;

public

\ Open a file frame at the file's first record ordinal.
: FILE-BEGIN ( -- )
   FO-DEPTH @ 1+ FO-CELLS * FILE-FRAMES-RESERVE
   1 FO-DEPTH +!
   DEFS-U @ FO-DEFS FO !
   INDEX-U @ FO-INDEX FO !
   INPUTS-U @ FO-INPUTS FO !
   WHY-U @ FO-WHY FO !
   ndict@ FO-FIRST FO !
   \ A kernel without the cut notice lowers the count unseen, and a row it
   \ retired would stay in the object.
   s" native-unit-cut-cell" NUNIT-CAPTURE:OPTIONAL-PRODUCER-XT 0= if
      s" cut-notice-absent" WHY+
   then
   \ A kernel without the run observer leaves the frame without the rows of
   \ the words its source runs (NDEF-CACHE OPEN).
   NUNIT-CAPTURE:RUN-OBSERVER? 0= if
      s" run-observer-absent" WHY+
   then ;

: FILE-END ( -- )
   FO-DEFS FO @ DEFS-U !
   FO-INDEX FO @ INDEX-U !
   FO-INPUTS FO @ INPUTS-U !
   FO-WHY FO @ WHY-U !
   -1 FO-DEPTH +! ;

: FILE-OPEN? ( -- bool ) FO-DEPTH @ 0 > ;
: FILE-REFUSED? ( -- bool ) WHY-U @ FO-WHY FO @ > ;

\ The innermost file keeps its first refusal reason; it writes no object.
: FILE-REFUSE ( ptr u8 n -- )
   {: reason:ptr reasonu:n :}
   FILE-OPEN? 0= if exit then
   FILE-REFUSED? if exit then
   reason reasonu WHY+ ;

: REASON$ ( -- ptr u8 n )
   FO-WHY FO @ {: first:n :}
   WHY-U @ first = if NULL-PTR 0 exit then
   first FILE-WHY WHY-U @ first - ;

: FILE-INPUTS+ ( ptr u8 n -- )
   FILE-OPEN? 0= if 2drop exit then
   INPUTS+ ;

: CLOSE ( -- )
   0 DEFS-U ! 0 INDEX-U ! 0 INPUTS-U ! 0 WHY-U ! 0 FO-DEPTH !
   0 OPEN-DEFS ! 0 OPEN-ROWS ! 0 OPEN-INPUTS !
   FILE-DEFS-RELEASE FILE-INDEX-RELEASE FILE-INPUTS-RELEASE
   FILE-WHY-RELEASE FILE-FRAMES-RELEASE FILE-ORDS-RELEASE
   NUNIT-FILE:CLOSE
   NFILE-SCOPE:SCOPE-CLOSE
   NFILE-INPUTS:CLOSE
   NFILE-REF:CLOSE
   NUNIT-CAPTURE:CLOSE ;

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
variable DEF-CACHEABLE
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
   NUNIT-CAPTURE:AARCH64-PROFILE? 0= if false exit then
   s" NCOMP:DEFINITION-CACHEABLE?" NUNIT-CAPTURE:OPTIONAL-PRODUCER-XT 0<>
   s" NCOMP:DEFINITION-HISTORY" NUNIT-CAPTURE:OPTIONAL-PRODUCER-XT 0<> and
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
\ Why the compiler's current definition is no unit capture can record, or
\ nothing. DOESB marks a does> clause open in it, and an object holds only
\ plain colon definitions.
: DEF-UNPACKABLE$ ( -- ptr u8 n )
   data-base DOESB-CELL + CELL-VIEW @ 0<> if s" does-pending" exit then
   s" " ;
\ Whether the compiler would let the current definition stand alone in a
\ cache entry. One it would not still joins its file's object.
: DEF-CACHEABLE-NOW? ( -- bool )
   s" NCOMP:DEFINITION-CACHEABLE?" DEF-NUM 0<> ;
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
   NUNIT-CAPTURE:PROFILE-CK
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

\ The capture layer cannot hold definition ord: no cache entry, and no object
\ for its file.
: REFUSED ( ptr u8 n n -- )
   {: reason:ptr reasonu:n ord:n :}
   reason reasonu ord REPORT-STORE-REFUSED
   reason reasonu FILE-REFUSE
   1 DEF-REFUSED ! ;

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
   s" capture-profile-unsupported" DEF-PENDING @ REFUSED ;

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
   0 DEF-PREPARED ! 0 DEF-REFUSED ! 0 DEF-CACHEABLE !
   0 DEF-CODE-U ! 0 DEF-CALL-U ! 0 DEF-ADDR-U !
   0 DEF-LIT-U ! 0 DEF-HOST-U ! 0 DEF-CHECK-U !
   0 DEF-LIT-N ! 0 DEF-CALL-SITE-N ! 0 DEF-ADDR-SITE-N !
   0 DEF-PENDING ! 0 DEF-CODE-BASE ! 0 DEF-CHECK-ID !
   0 DEF-MODULE-DELTA ! 0 DEF-GEN-DELTA !
   0 DEF-EPOCH-DELTA ! 0 DEF-BUILD-DELTA !
   0 DEF-HELD-E ! 0 DEF-HELD-IDX ! 0 DEF-HELD-FN ! 0 DEF-HELD-SIZE !
   0 DEF-PLAN-ON ! ;

: DEF-DROP-PLAN ( -- n )
   DEF-SOURCE-END
   ['] DEF-CHECK-DISCARD catch
   ['] DEF-ABANDON-HOST catch
   over 0<> if drop else nip then
   NFILE-REF:END-CAPTURE
   NFILE-REF:DISCARD-PREPARED
   DEF-RESET ;

\ The metadata row's own bounds, with no session state: the recorded length
\ matches the code bytes, the code is whole instructions, the flags are in range.
: META-CK ( ptr u8 n -- )
   {: meta:ptr size:n :}
   size DEF-META-BYTES <> if BAD then
   meta 16 + LE:U64@ dup 0<> swap 1 <> and if BAD then
   meta 40 + LE:U64@ 0<> if BAD then
   meta LE:U64@ {: recorded:n :}
   meta 8 + LE:U64@ {: code-u:n :}
   recorded CODE-SPAN:VALID? 0= if BAD then
   recorded CODE-SPAN:BYTES code-u <> if BAD then
   code-u 0 <= code-u 3 and 0<> or if BAD then
   meta 24 + LE:U64@ {: min-in:n :}
   min-in 0 < min-in 255 > or if BAD then ;

: DEF-READ-META ( -- )
   1 NUNIT-FILE:SECTION$ {: meta:ptr size:n :}
   meta size META-CK
   meta LE:U64@ DEF-RECORDED !
   meta 8 + LE:U64@ DEF-CODE-U !
   meta 16 + LE:U64@ 0<> DEF-WIDE !
   meta 24 + LE:U64@ DEF-MIN-IN !
   meta 32 + LE:U64@ DEF-LIT-N !
   meta 48 + LE:U64@ DEF-MODULE-DELTA !
   meta 56 + LE:U64@ DEF-GEN-DELTA !
   meta 64 + LE:U64@ DEF-EPOCH-DELTA !
   meta 72 + LE:U64@ DEF-BUILD-DELTA !
   ['] DEF-HISTORY-CHECK catch {: history-rc:n :}
   history-rc E-IR-MODULE-EXHAUSTED =
   history-rc E-IR-CTX-SERIALS = or
   history-rc E-IR-BUILD-SERIALS = or if BAD then
   history-rc 0<> if history-rc throw then ;

\ Literal row number row at off in the literal section: its byte length and
\ the first row with the same address, both within bounds.
: LIT-ROW ( ptr u8 n n n -- n n )
   {: src:ptr size:n off:n row:n :}
   size off - 16 < if BAD then
   src off + LE:U64@ {: bytes:n :}
   src off 8 + + LE:U64@ {: first:n :}
   first 0 < first row > or if BAD then
   bytes 0 < bytes size off 16 + - > or if BAD then
   bytes first ;

: DEF-READ-LITERALS ( -- )
   10 NUNIT-FILE:SECTION$ {: src:ptr size:n :}
   0 DEF-LIT-U !
   DEF-LIT-N @ DEF-LIT-ADDR-RESERVE
   DEF-LIT-N @ DEF-LIT-LEN-RESERVE
   DEF-LIT-N @ 0 ?do
      src size DEF-LIT-U @ i LIT-ROW {: bytes:n first:n :}
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
   14 NUNIT-FILE:SECTION$ NUNIT-DEP:CHECK 0= if BAD then
   DEF-READ-META
   0 NUNIT-FILE:SECTION$ {: source:ptr size:n :}
   size DEF-CODE-U @ <> if BAD then
   size DEF-CODE-RESERVE
   source 0 DEF-CODE size BYTE-COPY
   cp@ DEF-CODE-BASE !
   ['] XREF-REC NFILE-REF:RECORD-AT!
   13 NUNIT-FILE:SECTION$
      ndict@ DEF-CODE-BASE @ DEF-CODE-U @ NFILE-REF:PREPARE
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
   NUNIT-CAPTURE:PROFILE-CK
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
   \ NDEF-CACHE keeps its entry buffer stable through install and discard.
   DEF-IN-BYTES @ DEF-IN-SIZE @
      NUNIT-FILE:ARCH-AARCH64 s" DEFINITION"
      DEF-IN-KEY @ DEF-IN-KEY-U @ NUNIT-FILE:READ-VIEW
   DEF-PREP-OPEN ;

public

: WITH-DEFINITIONS ( [ -- ] -- )
   ['] DEF-OBSERVE swap NUNIT-CAPTURE:WITH-OBSERVER ;

: DEFINITION-BEGIN ( -- )
   DEF-RESET
   DEF-ELIGIBLE? 0= if s" begin-ineligible" ndict@ REFUSED exit then
   DEF-UNPACKABLE$ {: why:ptr whyu:n :}
   whyu 0 > if why whyu ndict@ REFUSED exit then
   DEF-CHECK-AVAILABLE? 0= if
      s" checker-unavailable" ndict@ REFUSED exit
   then
   DEF-CHECK-IDLE? 0= if s" checker-busy" ndict@ REFUSED exit then
   ndict@ DEF-PENDING !
   1 DEF-ACTIVE !
   DEF-CACHEABLE-NOW? if 1 DEF-CACHEABLE ! then
   DEF-CHECK-MARK
   DEF-SOURCE-BEGIN ;

: DEFINITION-END ( -- bool )
   DEF-ACTIVE @ 0= if DEF-SOURCE-END false exit then
   0 DEF-ACTIVE !
   DEF-ELIGIBLE? 0= if s" end-ineligible" DEF-PENDING @ REFUSED then
   DEF-UNPACKABLE$ {: why:ptr whyu:n :}
   whyu 0 > if why whyu DEF-PENDING @ REFUSED then
   DEF-EMITTED @ 0= DEF-REFUSED @ 0<> or if
      DEF-REFUSED @ 0<> if s" capture-refused" else s" no-emission" then
         DEF-PENDING @ REFUSED
      DEF-SOURCE-END
      DEF-CHECK-DISCARD
      NFILE-REF:END-CAPTURE false exit
   then
   ndict@ DEF-PENDING @ 1+ <> if BAD then
   DEF-CACHEABLE-NOW? 0= if 0 DEF-CACHEABLE ! then
   DEF-FIELDS
   DEF-REGISTER-FUNCTIONS
   DEF-CHECK-EXPORT
   DEF-CAPTURE-LITERALS
   DEF-SOURCE-END
   DEF-HISTORY-CAPTURE
   DEF-META!
   1 DEF-FINISHED !
   true ;

\ Whether the compiler let the finished definition stand alone in a cache
\ entry both before and after compiling it.
: DEFINITION-CACHEABLE? ( -- bool )
   DEF-FINISHED @ 0= if BAD then
   DEF-CACHEABLE @ 0<> ;

: PACK-DEFINITION ( ptr u8 n ptr u8 n -- ptr u8 n )
   {: key:ptr keyu:n deps:ptr depsu:n :}
   NUNIT-CAPTURE:PROFILE-CK
   DEF-FINISHED @ 0= if BAD then
   deps depsu DEF-PACK-SECTIONS
   NUNIT-FILE:ARCH-AARCH64 s" DEFINITION" key keyu NUNIT-FILE:PACK ;

: PREPARE-DEFINITION? ( ptr u8 n ptr u8 n -- bool )
   {: bytes:ptr size:n key:ptr keyu:n :}
   DEF-PLAN-ON @ if BAD then
   DEF-RESET
   DEF-ELIGIBLE? 0= if false exit then
   DEF-CHECK-AVAILABLE? 0= if false exit then
   DEF-CHECK-IDLE? 0= if false exit then
   bytes DEF-IN-BYTES ! size DEF-IN-SIZE !
   key DEF-IN-KEY ! keyu DEF-IN-KEY-U !
   ['] DEF-PREPARE-BODY catch {: rc:n :}
   rc 0= if true exit then
   DEF-DROP-PLAN {: cleanup:n :}
   rc E-NUNIT-PROFILE = rc E-NUNIT-FILE = or
   rc TFAM:E-UNIT-FAMILY-CHANGED = or if
      cleanup 0<> if cleanup throw then
      false exit
   then
   rc throw ;

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

\ Definition ord of the innermost open file joins its object: the payload's
\ sections in DEF-SLOTS order, and one index row. The payload's own dependency
\ slot stays out; any other slot is a payload this format cannot hold. Its
\ result digest is bound even when the file is refused, so the digest never
\ depends on whether the file writes an object.
: FILE-ADD-DEFINITION ( ptr u8 n ptr u8 n n -- )
   {: bytes:ptr size:n key:ptr keyu:n ord:n :}
   FILE-OPEN? 0= if exit then
   bytes size NUNIT-FILE:ARCH-AARCH64 s" DEFINITION" key keyu
      NUNIT-FILE:READ-VIEW
   ord NFILE-REF:BIND-RESULT
   FILE-REFUSED? if NUNIT-FILE:RESET-KEEP exit then
   INDEX-U @ INDEX-BYTES / {: row:n :}
   row 1+ FILE-ORDS-RESERVE
   ord row FILE-ORDS !
   ord FO-FIRST FO @ - dup 0 < if BAD then INDEX-N
   17 0 ?do
      i NUNIT-FILE:SECTION$ {: a:ptr u:n :}
      DEF-SLOTS i rshift 1 and 0<> if
         a u DEFS+ u INDEX-N
      else
         i DEPS-SLOT <> u 0<> and if BAD then
      then
   loop
   NUNIT-FILE:RESET-KEEP ;

\ The dictionary ordinal of the innermost file's definition row n.
: FILE-ORD ( n -- n ) {: row:n :}
   FO-INDEX FO @ INDEX-BYTES / row + {: at:n :}
   row 0 < at INDEX-U @ INDEX-BYTES / >= or if BAD then
   at FILE-ORDS @ ;

\ The innermost file's object bytes with its dependency rows deps.
: PACK-FILE ( ptr u8 n ptr u8 n -- ptr u8 n )
   {: key:ptr keyu:n deps:ptr depsu:n :}
   FILE-REFUSED? if BAD then
   NUNIT-FILE:RESET-KEEP
   17 0 ?do i NULL-PTR 0 NUNIT-FILE:SECTION! loop
   0 DEFS$ NUNIT-FILE:SECTION!
   2 INDEX$ NUNIT-FILE:SECTION!
   11 INPUTS$ NUNIT-FILE:SECTION!
   DEPS-SLOT deps depsu NUNIT-FILE:SECTION!
   NUNIT-FILE:ARCH-AARCH64 s" FILE" key keyu NUNIT-FILE:PACK ;

private

\ The summed section lengths of one index row, each within room bytes.
: ROW-SIZE ( ptr u8 n -- n )
   {: row:ptr room:n :}
   0 8 0 ?do
      {: used:n :}
      row i 1+ 8 * + LE:U64@ {: len:n :}
      len 0 < len room used - > or if BAD then
      used len +
   loop ;

public

\ The kernel's capture cut notice: the dictionary count drops to kept, which
\ retires every definition at or past it and gives its slot to the next
\ definition. Rows follow ordinal order across the open files, so the retired
\ ones are a suffix; drop it, and pull back each frame's marks the cut
\ crosses, its first ordinal included.
: FILE-CUT ( n -- )
   {: kept:n :}
   INDEX-U @ INDEX-BYTES / {: rows:n :}
   0 begin
      dup rows < if dup FILE-ORDS @ kept < else false then
   while 1+ repeat {: cut:n :}
   cut rows < if
      0 cut 0 ?do
         {: off:n :}
         i INDEX-BYTES * FILE-INDEX DEFS-U @ off - ROW-SIZE off +
      loop {: off:n :}
      cut INDEX-BYTES * INDEX-U !
      off DEFS-U !
   then
   FO-DEPTH @ 0 ?do
      i FO-INDEX FRAME @ INDEX-U @ min i FO-INDEX FRAME !
      i FO-DEFS FRAME @ DEFS-U @ min i FO-DEFS FRAME !
      i FO-FIRST FRAME @ kept min i FO-FIRST FRAME !
   loop ;

private

\ The framing of the definition payload in the current sections. References
\ stay encoded: decoding them binds session records (NFILE-REF:PREPARE).
: PAYLOAD-CK ( -- )
   1 NUNIT-FILE:SECTION$ {: meta:ptr metau:n :}
   meta metau META-CK
   0 NUNIT-FILE:SECTION$ nip meta 8 + LE:U64@ <> if BAD then
   2 NUNIT-FILE:SECTION$ nip DEF-CALL-BYTES mod 0<> if BAD then
   3 NUNIT-FILE:SECTION$ nip DEF-ADDR-BYTES mod 0<> if BAD then
   10 NUNIT-FILE:SECTION$ {: lits:ptr litsu:n :}
   0 meta 32 + LE:U64@ 0 ?do
      {: off:n :}
      lits litsu off i LIT-ROW drop 16 + off +
   loop
   litsu <> if BAD then
   13 NUNIT-FILE:SECTION$ nip 0= if BAD then ;

public

\ Check a FILE object's framing: only FILE-SLOTS hold bytes, the dependency
\ and input rows are well formed, and each index row frames one definition
\ payload, in increasing ordinal order, covering the definitions slot exactly.
: OPEN-FILE ( ptr u8 n ptr u8 n -- )
   {: bytes:ptr size:n key:ptr keyu:n :}
   bytes size NUNIT-FILE:ARCH-AARCH64 s" FILE" key keyu NUNIT-FILE:READ-VIEW
   17 0 ?do
      FILE-SLOTS i rshift 1 and 0= if
         i NUNIT-FILE:SECTION$ nip 0<> if BAD then
      then
   loop
   DEPS-SLOT NUNIT-FILE:SECTION$ NUNIT-DEP:FORMAT OPEN-ROWS !
   11 NUNIT-FILE:SECTION$ NFILE-INPUTS:ROW-COUNT OPEN-INPUTS !
   \ SECTIONS-VIEW replaces the table, so keep the FILE spans first.
   0 NUNIT-FILE:SECTION$ {: defs:ptr defsu:n :}
   2 NUNIT-FILE:SECTION$ {: index:ptr indexu:n :}
   indexu INDEX-BYTES mod 0<> if BAD then
   indexu INDEX-BYTES / {: count:n :}
   -1 0
   count 0 ?do
      {: prev:n off:n :}
      index i INDEX-BYTES * + {: row:ptr :}
      row LE:U64@ {: ord:n :}
      ord prev <= if BAD then
      row defsu off - ROW-SIZE {: total:n :}
      DEF-SLOTS row 8 + defs off + total NUNIT-FILE:SECTIONS-VIEW
      PAYLOAD-CK
      ord off total +
   loop
   nip defsu <> if BAD then
   count OPEN-DEFS !
   NUNIT-FILE:RESET-KEEP ;

\ The definitions, dependency rows and input rows OPEN-FILE found.
: FILE-COUNTS ( -- n n n )
   OPEN-DEFS @ OPEN-ROWS @ OPEN-INPUTS @ ;

;package
