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
\ the file's input and dependency rows, and what the file adds to the
\ dictionary and DATA. Slot 0 holds the definitions' sections back to back;
\ slot 2 one INDEX-BYTES row per definition: its ordinal past the file's first
\ record, the length of each DEF-SLOTS section in slot order, then the span of
\ its name token in the file's source, start and end offsets, 0 0 when the
\ name came from other text: what the origin journal records; slot 11 the
\ input rows; slot 14 the dependency rows. The file's own records fall in
\ segments, each a maximal run between child loads: slot 12 holds one
\ EVENT-BYTES row per segment and per child load, in load order. Over the
\ segments in order, slot 1 holds one RECORD-BYTES row per record, slot 6 the
\ DATA they allotted, slot 8 their require rows (length, then path) and slot
\ 4 one byte per wordlist they made, 1 when it is protected. Slot 3 holds the
\ names record and load rows point at, slot 13 the references record rows
\ name, in NFILE-REF ENCODE format with no declarations, and slot 16 the
\ namespace at the file's end. Every other slot is empty. A definition's own
\ dependency slot stays out: the file's rows cover it.
42031 constant DEF-SLOTS
\ Slots 0 1 2 3 4 6 8 11 12 13 14 16.
96607 constant FILE-SLOTS
14 constant DEPS-SLOT
\ An index row's name span: after the ordinal and the eight section lengths.
72 constant SPAN-AT
80 constant SPAN-END
88 constant INDEX-BYTES
\ A record row: kind, flags, name offset and length in slot 3, wordlist
\ reference, then by kind the value, DATA reference, index row or public
\ wordlist reference, and a package's private wordlist reference.
56 constant RECORD-BYTES
\ An event row. SEGMENT: kind, first ordinal past the file's first record,
\ records, DATA bytes, requires, wordlists, 0. LOAD: kind, load kind, source
\ offset in the loading file, path offset and length, root offset and length.
56 constant EVENT-BYTES
\ Record row kinds; 1 to 3 are the fixed kinds NCOMP:COMPILE-FIXED takes.
0 constant RK-BARE
1 constant RK-VAL
2 constant RK-ADDR
3 constant RK-DEFER
4 constant RK-COLON
5 constant RK-PACKAGE
1 constant EV-SEGMENT
2 constant EV-LOAD
\ One frame per open file: the buffers' used sizes when it opened, its first
\ record ordinal, the open segment's marks (first ordinal, HERE, require
\ count, wordlist counter), the require counter it opened with, and 1 once
\ the retire hook moved while it was open. A child file's bytes follow its
\ parent's and are dropped when the child closes.
0 constant FO-DEFS
1 constant FO-INDEX
2 constant FO-INPUTS
3 constant FO-WHY
4 constant FO-FIRST
5 constant FO-EVENTS
6 constant FO-NAMES
7 constant FO-SEG-ORD
8 constant FO-SEG-HERE
9 constant FO-SEG-REQ
10 constant FO-SEG-WID
11 constant FO-REQ-XT
12 constant FO-HOOK
13 constant FO-CELLS
\ An event row in memory, cell 1 an ordinal in both kinds so a dictionary
\ cut finds the rows it retired. SEGMENT: kind, first ordinal, records, HERE,
\ DATA bytes, first require, requires, first wordlist, wordlists. LOAD: kind,
\ the child's first ordinal, load kind, source offset, path offset and
\ length, root offset and length in FILE-NAMES, 0.
9 constant EV-CELLS
DYNAMIC-BUFFER FILE-DEFS u8
DYNAMIC-BUFFER FILE-INDEX u8
DYNAMIC-BUFFER FILE-INPUTS u8
DYNAMIC-BUFFER FILE-WHY u8
DYNAMIC-BUFFER FILE-FRAMES n
DYNAMIC-BUFFER FILE-EVENTS n
DYNAMIC-BUFFER FILE-NAMES u8
\ The closing file's record, DATA, require, protection and event sections,
\ back to back.
DYNAMIC-BUFFER FILE-OUT u8
\ Each index row's dictionary ordinal, so a dictionary cut can find the rows
\ it retired. Never written to an object.
DYNAMIC-BUFFER FILE-ORDS n
variable DEFS-U
variable INDEX-U
variable INPUTS-U
variable WHY-U
variable EVENTS-N
variable NAMES-U
variable OUT-U
variable RECS-AT
variable DATA-AT
variable REQS-AT
variable PROT-AT
variable EVS-AT
variable FO-DEPTH
\ The closing file's next definition index row while its records encode.
variable ROW-AT
variable OPEN-DEFS
variable OPEN-ROWS
variable OPEN-INPUTS
variable OPEN-RECS
variable OPEN-DATA
variable OPEN-REQS
variable OPEN-WIDS
variable OPEN-LOADS
variable OPEN-SEGS
CAST: REQ-COUNT-XT ( n -- [ -- n ] )
CAST: REQ-LEN-XT ( n -- [ n -- n ] )
CAST: REQ-SLOT-XT ( n -- [ n -- ptr u8 ] )

\ Field n of file frame k, outermost first.
: FRAME ( n n -- ptr n ) swap FO-CELLS * + FILE-FRAMES ;

\ Field n of the innermost file frame.
: FO ( n -- ptr n )
   FO-DEPTH @ 0 <= if BAD then
   FO-DEPTH @ 1- swap FRAME ;

\ Field n of event row k.
: EV ( n n -- ptr n ) swap EV-CELLS * + FILE-EVENTS ;

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
\ The name's offset in FILE-NAMES.
: NAMES+ ( ptr u8 n -- n )
   {: a:ptr u:n :}
   NAMES-U @ {: at:n :}
   u 0= if at exit then
   at u + FILE-NAMES-RESERVE
   a at FILE-NAMES u BYTE-COPY
   u NAMES-U +!
   at ;
: OUT-N ( n -- )
   {: value:n :}
   OUT-U @ 8 + FILE-OUT-RESERVE
   value OUT-U @ FILE-OUT LE:U64!
   8 OUT-U +! ;
: OUT-S ( ptr u8 n -- )
   {: a:ptr u:n :}
   u 0= if exit then
   OUT-U @ u + FILE-OUT-RESERVE
   a OUT-U @ FILE-OUT u BYTE-COPY
   u OUT-U +! ;
: OUT-B ( n -- )
   {: value:n :}
   OUT-U @ 1+ FILE-OUT-RESERVE
   value OUT-U @ FILE-OUT c!
   1 OUT-U +! ;
: EV+ ( n n n n n n n n n -- )
   {: kind:n ord:n c2:n c3:n c4:n c5:n c6:n c7:n c8:n :}
   EVENTS-N @ {: row:n :}
   row 1+ EV-CELLS * FILE-EVENTS-RESERVE
   kind row 0 EV ! ord row 1 EV ! c2 row 2 EV ! c3 row 3 EV !
   c4 row 4 EV ! c5 row 5 EV ! c6 row 6 EV ! c7 row 7 EV ! c8 row 8 EV !
   1 EVENTS-N +! ;

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
: NAMES$ ( -- ptr u8 n )
   FO-NAMES FO @ {: first:n :}
   NAMES-U @ first = if NULL-PTR 0 exit then
   first FILE-NAMES NAMES-U @ first - ;
: OUT$ ( n n -- ptr u8 n )
   {: first:n last:n :}
   last first = if NULL-PTR 0 exit then
   first FILE-OUT last first - ;

\ The DATA bytes at address va.
: DATA-PTR ( n -- ptr u8 )
   {: va:n :}
   data-base BYTE-VIEW va DATA-VA VA>N - + ;

\ The require registry's counter, absent before include.f loads.
: REQ-XT ( -- n ) s" REQUIRE-REG:COUNT" NUNIT-CAPTURE:OPTIONAL-PRODUCER-XT ;
: REQ-NOW ( n -- n )
   {: xt:n :}
   xt 0= if 0 exit then
   xt REQ-COUNT-XT execute ;

public

: FILE-OPEN? ( -- bool ) FO-DEPTH @ 0 > ;
: FILE-REFUSED? ( -- bool ) WHY-U @ FO-WHY FO @ > ;

\ The innermost file keeps its first refusal reason; it writes no object.
: FILE-REFUSE ( ptr u8 n -- )
   {: reason:ptr reasonu:n :}
   FILE-OPEN? 0= if exit then
   FILE-REFUSED? if exit then
   reason reasonu WHY+ ;

private

\ FILE-REFUSE naming the record the reason is about.
: REFUSE-WORD ( ptr u8 n ptr n -- )
   {: reason:ptr reasonu:n rec:ptr :}
   FILE-OPEN? 0= if exit then
   FILE-REFUSED? if exit then
   reason reasonu WHY+
   s"  word=" WHY+
   rec XREF-NAME$ WHY+ ;

\ Open the innermost file's next segment at the present cursors.
: SEGMENT-OPEN ( -- )
   ndict@ FO-SEG-ORD FO !
   AOT-ARM:HERE-N FO-SEG-HERE FO !
   FO-REQ-XT FO @ REQ-NOW FO-SEG-REQ FO !
   AOT-ARM:WIDN FO-SEG-WID FO ! ;

\ Close the innermost file's open segment as one SEGMENT row, or none when
\ it added no record, DATA, require or wordlist. HERE or the wordlist counter
\ going back, or a require counter other than the one the file opened with,
\ refuses it.
: SEGMENT-CLOSE ( -- )
   FO-SEG-ORD FO @ {: first:n :}
   ndict@ first - {: count:n :}
   count 0 < if BAD then
   AOT-ARM:HERE-N FO-SEG-HERE FO @ - {: bytes:n :}
   AOT-ARM:WIDN FO-SEG-WID FO @ - {: wids:n :}
   bytes 0 < if s" data-rewound" FILE-REFUSE exit then
   wids 0 < if s" wordlist-rewound" FILE-REFUSE exit then
   REQ-XT FO-REQ-XT FO @ <> if s" requires-unknown" FILE-REFUSE exit then
   FO-REQ-XT FO @ REQ-NOW FO-SEG-REQ FO @ - {: reqs:n :}
   reqs 0 < if s" requires-unknown" FILE-REFUSE exit then
   count bytes or reqs or wids or 0= if exit then
   EV-SEGMENT first count FO-SEG-HERE FO @ bytes
      FO-SEG-REQ FO @ reqs FO-SEG-WID FO @ wids EV+ ;

\ Record ord is the innermost file's own: in its open segment or one it
\ closed.
: OWN? ( n -- bool )
   {: ord:n :}
   ord 0 < if false exit then
   ord FO-SEG-ORD FO @ >= if true exit then
   EVENTS-N @ FO-EVENTS FO @ ?do
      i 0 EV @ EV-SEGMENT = if
         ord i 1 EV @ >= ord i 1 EV @ i 2 EV @ + < and if
            true unloop exit
         then
      then
   loop false ;

public

\ Open a file frame at the file's first record ordinal. A child's load ends
\ its parent's segment with a LOAD row: the load kind, where the parent's
\ source stood, below zero outside it, which refuses the parent, and the
\ child's path and root.
: FILE-BEGIN ( ptr u8 n ptr u8 n n n -- )
   {: path:ptr pathu:n root:ptr rootu:n kind:n at:n :}
   FILE-OPEN? if
      SEGMENT-CLOSE
      at 0 < if s" load-outside-source" FILE-REFUSE then
      EV-LOAD ndict@ kind at path pathu NAMES+ pathu
         root rootu NAMES+ rootu 0 EV+
   then
   FO-DEPTH @ 1+ FO-CELLS * FILE-FRAMES-RESERVE
   1 FO-DEPTH +!
   DEFS-U @ FO-DEFS FO !
   INDEX-U @ FO-INDEX FO !
   INPUTS-U @ FO-INPUTS FO !
   WHY-U @ FO-WHY FO !
   ndict@ FO-FIRST FO !
   EVENTS-N @ FO-EVENTS FO !
   NAMES-U @ FO-NAMES FO !
   REQ-XT FO-REQ-XT FO !
   0 FO-HOOK FO !
   SEGMENT-OPEN
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

\ Close the innermost frame; its parent's next segment opens.
: FILE-END ( -- )
   FO-DEFS FO @ DEFS-U !
   FO-INDEX FO @ INDEX-U !
   FO-INPUTS FO @ INPUTS-U !
   FO-WHY FO @ WHY-U !
   FO-EVENTS FO @ EVENTS-N !
   FO-NAMES FO @ NAMES-U !
   -1 FO-DEPTH +!
   FILE-OPEN? if SEGMENT-OPEN then ;

\ The retire hook (XREF:BEFORE-RETIRE!): a record the innermost file did not
\ make, an earlier or a child file's, belongs to another object, which this
\ one cannot change, so the file refuses.
: FILE-RETIRE ( ptr n -- )
   {: rec:ptr :}
   FILE-OPEN? 0= if exit then
   rec NFILE-REF:REC-ORD OWN? if exit then
   s" retires-earlier" rec REFUSE-WORD ;

\ The retire hook moved to another XREF: every open file may have missed a
\ retirement.
: HOOK-CHANGED ( -- )
   FO-DEPTH @ 0 ?do 1 i FO-HOOK FRAME ! loop ;

: REASON$ ( -- ptr u8 n )
   FO-WHY FO @ {: first:n :}
   WHY-U @ first = if NULL-PTR 0 exit then
   first FILE-WHY WHY-U @ first - ;

: FILE-INPUTS+ ( ptr u8 n -- )
   FILE-OPEN? 0= if 2drop exit then
   INPUTS+ ;

: CLOSE ( -- )
   0 DEFS-U ! 0 INDEX-U ! 0 INPUTS-U ! 0 WHY-U ! 0 FO-DEPTH !
   0 EVENTS-N ! 0 NAMES-U ! 0 OUT-U ! 0 ROW-AT !
   0 OPEN-DEFS ! 0 OPEN-ROWS ! 0 OPEN-INPUTS !
   0 OPEN-RECS ! 0 OPEN-DATA ! 0 OPEN-REQS ! 0 OPEN-WIDS !
   0 OPEN-LOADS ! 0 OPEN-SEGS !
   FILE-DEFS-RELEASE FILE-INDEX-RELEASE FILE-INPUTS-RELEASE
   FILE-WHY-RELEASE FILE-FRAMES-RELEASE FILE-ORDS-RELEASE
   FILE-EVENTS-RELEASE FILE-NAMES-RELEASE FILE-OUT-RELEASE
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
\ sections in DEF-SLOTS order, and one index row with its name span at end.
\ The payload's own dependency slot stays out; any other slot is a payload
\ this format cannot hold. Its result digest is bound even when the file is
\ refused, so the digest never depends on whether the file writes an object.
: FILE-ADD-DEFINITION ( ptr u8 n ptr u8 n n n n -- )
   {: bytes:ptr size:n key:ptr keyu:n ord:n at:n end:n :}
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
   at INDEX-N end INDEX-N
   NUNIT-FILE:RESET-KEEP ;

private

\ The index of the innermost file's definition row n among all index rows.
: INDEX-ROW ( n -- n ) {: row:n :}
   FO-INDEX FO @ INDEX-BYTES / row + {: at:n :}
   row 0 < at INDEX-U @ INDEX-BYTES / >= or if BAD then
   at ;

public

\ The dictionary ordinal of the innermost file's definition row n.
: FILE-ORD ( n -- n ) INDEX-ROW FILE-ORDS @ ;

\ The name span of the innermost file's definition row n.
: FILE-SPAN ( n -- n n )
   INDEX-ROW INDEX-BYTES * {: off:n :}
   off SPAN-AT + FILE-INDEX LE:U64@ off SPAN-END + FILE-INDEX LE:U64@ ;

private

\ Event row k is a SEGMENT row.
: SEG? ( n -- bool ) 0 EV @ EV-SEGMENT = ;

\ Wordlist wid as a reference: local, indexed over the wordlists the closing
\ file's segments made in order, else one made before the file.
: WID-REF ( n -- n )
   {: wid:n :}
   0 EVENTS-N @ FO-EVENTS FO @ ?do
      {: base:n :}
      i SEG? if
         wid i 7 EV @ >= wid i 7 EV @ i 8 EV @ + < and if
            NFILE-REF:WID base wid + i 7 EV @ - 0 NFILE-REF:REF-LOCAL
            unloop exit
         then
         base i 8 EV @ +
      else base then
   loop drop
   wid NFILE-REF:REF-WID ;

\ DATA address va as a reference into the closing file's DATA section, 0
\ when no segment of the file allotted it. A segment's end counts as its
\ own, so a word made after its last allotment still resolves.
: DATA-REF ( n -- n )
   {: va:n :}
   0 EVENTS-N @ FO-EVENTS FO @ ?do
      {: base:n :}
      i SEG? if
         va i 3 EV @ >= va i 3 EV @ i 4 EV @ + <= and if
            NFILE-REF:DATA base va + i 3 EV @ - 0 NFILE-REF:REF-LOCAL
            unloop exit
         then
         base i 4 EV @ +
      else base then
   loop drop 0 ;

\ The closing file's definition index row of record ord, -1 when it has none.
\ Records encode in ordinal order, so ROW-AT only moves forward.
: ORD-ROW ( n -- n )
   {: ord:n :}
   INDEX-U @ INDEX-BYTES / {: rows:n :}
   begin
      ROW-AT @ rows < if ROW-AT @ FILE-ORDS @ ord < else false then
   while 1 ROW-AT +! repeat
   ROW-AT @ rows < if ROW-AT @ FILE-ORDS @ ord = else false then
   if ROW-AT @ FO-INDEX FO @ INDEX-BYTES / - else -1 then ;

: PACKAGE-SHAPE ( ptr n -- n n n bool )
   {: rec:ptr :}
   rec XREF-PKG-PUBLIC WID-REF {: pub:n :}
   rec XREF-PKG-PRIVATE WID-REF {: priv:n :}
   RK-PACKAGE pub priv pub 0<> priv 0<> and ;

: ADDR-SHAPE ( n n -- n n n bool )
   {: kind:n va:n :}
   va DATA-REF {: ref:n :}
   kind ref 0 ref 0<> ;

\ Record ord's row kind, value and private wordlist reference, and whether
\ they encode. A does> clause or instance refuses the file.
: RECORD-SHAPE ( n -- n n n bool )
   {: ord:n :}
   ord XREF-REC {: rec:ptr :}
   rec XREF-WORDLIST XREF-NAMESPACE-WL = if rec PACKAGE-SHAPE exit then
   rec XREF-RETIRED? if RK-BARE 0 0 true exit then
   ord 0 > if ord 1- XREF-DOES-COMPANION? else false then if
      s" does-pending" rec REFUSE-WORD RK-BARE 0 0 false exit
   then
   rec XREF-START 0= if RK-BARE 0 0 true exit then
   rec XREF-FLAGS DKIND:MASK and DKIND:CAST = if RK-BARE 0 0 true exit then
   ord NFILE-REF:FIXED@ {: fkind:n fvalue:n known:bool :}
   known fkind CHECKER-OWNER-ABI:RECORD-FIXED-VAL = and if
      RK-VAL fvalue 0 true exit
   then
   known fkind CHECKER-OWNER-ABI:RECORD-FIXED-ADDR = and if
      RK-ADDR fvalue ADDR-SHAPE exit
   then
   rec NDICT:REC-DEFER-CELL {: cell:n :}
   cell 0<> if RK-DEFER cell ADDR-SHAPE exit then
   rec XREF-FLAGS DNAME-ALLOC and 0<> if
      s" does-pending" rec REFUSE-WORD RK-BARE 0 0 false exit
   then
   ord ORD-ROW {: row:n :}
   RK-COLON row 0 row 0 >= ;

\ One RECORD-BYTES row for record ord.
: RECORD-ROW ( n -- )
   {: ord:n :}
   ord XREF-REC {: rec:ptr :}
   ord RECORD-SHAPE {: kind:n value:n priv:n known:bool :}
   kind RK-PACKAGE = if 0 else rec XREF-WORDLIST WID-REF then {: wl:n :}
   known 0= kind RK-PACKAGE <> wl 0= and or if
      s" record-kind-unknown" rec REFUSE-WORD exit
   then
   rec XREF-NAME$ {: name:ptr nameu:n :}
   kind OUT-N
   rec XREF-FLAGS DNAME-LEN-MASK invert and OUT-N
   name nameu NAMES+ FO-NAMES FO @ - OUT-N
   nameu OUT-N
   wl OUT-N value OUT-N priv OUT-N ;

: RECORDS-OUT ( -- )
   EVENTS-N @ FO-EVENTS FO @ ?do
      i SEG? if
         i 1 EV @ i 2 EV @ + i 1 EV @ ?do i RECORD-ROW loop
      then
   loop ;

: DATA-OUT ( -- )
   EVENTS-N @ FO-EVENTS FO @ ?do
      i SEG? if i 3 EV @ DATA-PTR i 4 EV @ OUT-S then
   loop ;

\ Require idx as its length and path.
: REQ-ROW ( n n n -- )
   {: idx:n lenxt:n slotxt:n :}
   idx lenxt REQ-LEN-XT execute {: len:n :}
   len 0 <= if s" requires-unknown" FILE-REFUSE exit then
   len OUT-N
   idx slotxt REQ-SLOT-XT execute len OUT-S ;

: REQS-OUT ( -- )
   s" REQUIRE-REG:LEN" NUNIT-CAPTURE:OPTIONAL-PRODUCER-XT {: lenxt:n :}
   s" REQUIRE-REG:SLOT" NUNIT-CAPTURE:OPTIONAL-PRODUCER-XT {: slotxt:n :}
   REQ-XT FO-REQ-XT FO @ <> lenxt 0= or slotxt 0= or {: unknown:bool :}
   EVENTS-N @ FO-EVENTS FO @ ?do
      i SEG? if
         i 6 EV @ 0<> unknown and if
            s" requires-unknown" FILE-REFUSE unloop exit
         then
         i 5 EV @ i 6 EV @ + i 5 EV @ ?do i lenxt slotxt REQ-ROW loop
      then
   loop ;

: PROT-OUT ( -- )
   EVENTS-N @ FO-EVENTS FO @ ?do
      i SEG? if
         i 7 EV @ i 8 EV @ + i 7 EV @ ?do
            i XREF-WID-PROTECTED? if 1 else 0 then OUT-B
         loop
      then
   loop ;

\ Event row k as its EVENT-BYTES row.
: EVENT-OUT ( n -- )
   {: row:n :}
   row SEG? if
      EV-SEGMENT OUT-N row 1 EV @ FO-FIRST FO @ - OUT-N
      row 2 EV @ OUT-N row 4 EV @ OUT-N row 6 EV @ OUT-N
      row 8 EV @ OUT-N 0 OUT-N exit
   then
   EV-LOAD OUT-N row 2 EV @ OUT-N row 3 EV @ OUT-N
   row 4 EV @ FO-NAMES FO @ - OUT-N row 5 EV @ OUT-N
   row 6 EV @ FO-NAMES FO @ - OUT-N row 7 EV @ OUT-N ;

public

\ Encode the innermost file's records, DATA, requires, protection bytes and
\ events into FILE-OUT, closing its last segment. A file whose retire hook
\ moved while it was open, or whose records cannot all encode, refuses.
: FILE-ENCODE ( -- )
   FILE-OPEN? 0= if BAD then
   SEGMENT-CLOSE
   FO-HOOK FO @ 0<> if s" retire-hook-new" FILE-REFUSE then
   FILE-REFUSED? if exit then
   NFILE-REF:BEGIN-FILE-REFS
   0 OUT-U !
   FO-INDEX FO @ INDEX-BYTES / ROW-AT !
   0 RECS-AT ! RECORDS-OUT
   OUT-U @ DATA-AT ! DATA-OUT
   OUT-U @ REQS-AT ! REQS-OUT
   OUT-U @ PROT-AT ! PROT-OUT
   OUT-U @ EVS-AT !
   EVENTS-N @ FO-EVENTS FO @ ?do i EVENT-OUT loop ;

\ The innermost file's object bytes with its dependency rows deps, after
\ FILE-ENCODE.
: PACK-FILE ( ptr u8 n ptr u8 n -- ptr u8 n )
   {: key:ptr keyu:n deps:ptr depsu:n :}
   FILE-REFUSED? if BAD then
   NUNIT-FILE:RESET-KEEP
   17 0 ?do i NULL-PTR 0 NUNIT-FILE:SECTION! loop
   0 DEFS$ NUNIT-FILE:SECTION!
   1 RECS-AT @ DATA-AT @ OUT$ NUNIT-FILE:SECTION!
   2 INDEX$ NUNIT-FILE:SECTION!
   3 NAMES$ NUNIT-FILE:SECTION!
   4 PROT-AT @ EVS-AT @ OUT$ NUNIT-FILE:SECTION!
   6 DATA-AT @ REQS-AT @ OUT$ NUNIT-FILE:SECTION!
   8 REQS-AT @ PROT-AT @ OUT$ NUNIT-FILE:SECTION!
   11 INPUTS$ NUNIT-FILE:SECTION!
   12 EVS-AT @ OUT-U @ OUT$ NUNIT-FILE:SECTION!
   13 0 0 NFILE-REF:ENCODE NUNIT-FILE:SECTION!
   DEPS-SLOT deps depsu NUNIT-FILE:SECTION!
   16 NFILE-SCOPE:SCOPE-INPUT$ NUNIT-FILE:SECTION!
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

\ Whether a cut to kept retires a closed child load of the innermost file:
\ its LOAD row, at the child's first ordinal, or a record of the child's
\ range, which ends where the next event or the open segment begins. The
\ child's object is written, and this file's would lose the load.
: CUTS-CHILD? ( n -- bool ) {: kept:n :}
   FILE-OPEN? 0= if false exit then
   -1 EVENTS-N @ FO-EVENTS FO @ ?do i SEG? 0= if drop i then loop
      {: last:n :}
   last 0 < if false exit then
   last 1+ EVENTS-N @ < if last 1+ 1 EV @ else FO-SEG-ORD FO @ then
      {: end:n :}
   kept last 1 EV @ <= kept end < or ;

public

\ The kernel's capture cut notice: the dictionary count drops to kept, which
\ retires every definition at or past it and gives its slot to the next
\ definition. Index and event rows follow ordinal order across the open files,
\ so the retired ones are a suffix; drop it, end a SEGMENT row the cut
\ crosses at the cut, and pull back each frame's marks the cut crosses, its
\ first ordinal and open segment included. A cut into a closed child load
\ refuses the innermost file.
: FILE-CUT ( n -- )
   {: kept:n :}
   kept CUTS-CHILD? if s" cut-below-child" FILE-REFUSE then
   kept NUNIT-DEP:DICT-CUT
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
   0 begin
      dup EVENTS-N @ < if dup 1 EV @ kept < else false then
   while 1+ repeat {: evcut:n :}
   evcut EVENTS-N !
   evcut 0 > if evcut 1- SEG? else false then if
      evcut 1- 2 EV @ kept evcut 1- 1 EV @ - min evcut 1- 2 EV !
   then
   FO-DEPTH @ 0 ?do
      i FO-INDEX FRAME @ INDEX-U @ min i FO-INDEX FRAME !
      i FO-DEFS FRAME @ DEFS-U @ min i FO-DEFS FRAME !
      i FO-FIRST FRAME @ kept min i FO-FIRST FRAME !
      i FO-EVENTS FRAME @ EVENTS-N @ min i FO-EVENTS FRAME !
      i FO-SEG-ORD FRAME @ kept min i FO-SEG-ORD FRAME !
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

\ Field k of the u64 row at row.
: U64-AT ( ptr u8 n -- n ) 8 * + LE:U64@ ;

\ off and len frame a span within room bytes.
: SPAN-CK ( n n n -- )
   {: off:n len:n room:n :}
   off 0 < len 0 < or off room > or len room off - > or if BAD then ;

\ ref names one of refs reference rows.
: REF-CK? ( n n -- bool )
   {: ref:n refs:n :}
   ref 0 > ref refs <= and ;

\ A SEGMENT event row starting at or past end, the previous segment's last
\ ordinal; the new end.
: SEGMENT-CK ( ptr u8 n -- n )
   {: row:ptr end:n :}
   row 1 U64-AT {: first:n :}
   row 2 U64-AT {: count:n :}
   row 3 U64-AT {: bytes:n :}
   row 4 U64-AT {: reqs:n :}
   row 5 U64-AT {: wids:n :}
   first end < count 0 < or bytes 0 < or reqs 0 < or wids 0 < or if BAD then
   row 6 U64-AT 0<> if BAD then
   count OPEN-RECS +! bytes OPEN-DATA +! reqs OPEN-REQS +!
   wids OPEN-WIDS +! 1 OPEN-SEGS +!
   first count + ;

\ A LOAD event row: where the loading file's source stood, and the child's
\ path and root in the names section.
: LOAD-CK ( ptr u8 n -- )
   {: row:ptr namesu:n :}
   row 1 U64-AT 0 < row 2 U64-AT 0 < or if BAD then
   row 4 U64-AT 0= if BAD then
   row 3 U64-AT row 4 U64-AT namesu SPAN-CK
   row 5 U64-AT row 6 U64-AT namesu SPAN-CK
   1 OPEN-LOADS +! ;

: EVENTS-CK ( ptr u8 n n -- )
   {: evs:ptr evsu:n namesu:n :}
   evsu EVENT-BYTES mod 0<> if BAD then
   0 evsu EVENT-BYTES / 0 ?do
      {: end:n :}
      evs i EVENT-BYTES * + {: row:ptr :}
      row 0 U64-AT {: kind:n :}
      kind EV-SEGMENT = if row end SEGMENT-CK else
         kind EV-LOAD <> if BAD then
         row namesu LOAD-CK end
      then
   loop drop ;

\ One record row: a known kind, its name in the names section, and the
\ references and index row its kind names.
: RECORD-CK ( ptr u8 n n n -- )
   {: row:ptr namesu:n refs:n defs:n :}
   row 0 U64-AT {: kind:n :}
   row 4 U64-AT {: wl:n :}
   row 5 U64-AT {: value:n :}
   row 6 U64-AT {: priv:n :}
   kind RK-BARE < kind RK-PACKAGE > or if BAD then
   row 2 U64-AT row 3 U64-AT namesu SPAN-CK
   kind RK-PACKAGE = if
      wl 0<> value refs REF-CK? 0= or priv refs REF-CK? 0= or if BAD then
      exit
   then
   wl refs REF-CK? 0= priv 0<> or if BAD then
   kind RK-ADDR = kind RK-DEFER = or if
      value refs REF-CK? 0= if BAD then exit
   then
   kind RK-COLON = if value 0 < value defs >= or if BAD then exit then
   kind RK-BARE = value 0<> and if BAD then ;

\ The reference section: an NFILE-REF ENCODE header with no declarations
\ and room for its reference rows; their count.
: REFS-CK ( ptr u8 n -- n )
   {: refs:ptr refsu:n :}
   refsu 24 < if BAD then
   refs 2 U64-AT 0<> if BAD then
   refs 0 U64-AT {: cnt:n :}
   cnt 0 < cnt refsu 24 - 32 / > or if BAD then
   cnt ;

: RECORDS-CK ( ptr u8 n n n n -- )
   {: recs:ptr recsu:n namesu:n refs:n defs:n :}
   recsu OPEN-RECS @ RECORD-BYTES * <> if BAD then
   OPEN-RECS @ 0 ?do
      recs i RECORD-BYTES * + namesu refs defs RECORD-CK
   loop ;

: PROT-CK ( ptr u8 n -- )
   {: prot:ptr protu:n :}
   protu OPEN-WIDS @ <> if BAD then
   protu 0 ?do prot i + c@ 1 > if BAD then loop ;

\ Each require row: its length, then that many path bytes.
: REQUIRES-CK ( ptr u8 n -- )
   {: reqs:ptr reqsu:n :}
   0 OPEN-REQS @ 0 ?do
      {: off:n :}
      reqsu off - 8 < if BAD then
      reqs off + LE:U64@ {: len:n :}
      len 0 <= len reqsu off - 8 - > or if BAD then
      off 8 + len +
   loop
   reqsu <> if BAD then ;

\ The FILE object's segment framing: events, records, DATA, protection
\ bytes and requires agree with one another, given its defs index rows.
: SEGMENTS-CK ( n -- )
   {: defs:n :}
   0 OPEN-RECS ! 0 OPEN-DATA ! 0 OPEN-REQS ! 0 OPEN-WIDS !
   0 OPEN-LOADS ! 0 OPEN-SEGS !
   3 NUNIT-FILE:SECTION$ nip {: namesu:n :}
   12 NUNIT-FILE:SECTION$ namesu EVENTS-CK
   13 NUNIT-FILE:SECTION$ REFS-CK {: refs:n :}
   1 NUNIT-FILE:SECTION$ namesu refs defs RECORDS-CK
   6 NUNIT-FILE:SECTION$ nip OPEN-DATA @ <> if BAD then
   4 NUNIT-FILE:SECTION$ PROT-CK
   8 NUNIT-FILE:SECTION$ REQUIRES-CK ;

public

\ Check a FILE object's framing: only FILE-SLOTS hold bytes, the dependency
\ and input rows are well formed, the segment sections agree, and each index
\ row frames one definition payload, in increasing ordinal order, covering the
\ definitions slot exactly, and spans a name within the source's srcu bytes.
: OPEN-FILE ( ptr u8 n ptr u8 n n -- )
   {: bytes:ptr size:n key:ptr keyu:n srcu:n :}
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
   count SEGMENTS-CK
   -1 0
   count 0 ?do
      {: prev:n off:n :}
      index i INDEX-BYTES * + {: row:ptr :}
      row LE:U64@ {: ord:n :}
      ord prev <= if BAD then
      row SPAN-AT + LE:U64@ {: at:n :}
      row SPAN-END + LE:U64@ {: end:n :}
      at 0 < at end > or end srcu > or if BAD then
      row defsu off - ROW-SIZE {: total:n :}
      DEF-SLOTS row 8 + defs off + total NUNIT-FILE:SECTIONS-VIEW
      PAYLOAD-CK
      ord off total +
   loop
   nip defsu <> if BAD then
   count OPEN-DEFS !
   NUNIT-FILE:RESET-KEEP ;

\ What OPEN-FILE found: definitions, dependency rows, input rows, records,
\ DATA bytes, requires, loads and segments.
: FILE-COUNTS ( -- n n n n n n n n )
   OPEN-DEFS @ OPEN-ROWS @ OPEN-INPUTS @ OPEN-RECS @ OPEN-DATA @
   OPEN-REQS @ OPEN-LOADS @ OPEN-SEGS @ ;

;package
