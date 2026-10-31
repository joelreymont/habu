\ Native unit object for one cached definition: code and dictionary rows,
\ source-declared relocation sites, DATA and typed cells, literal rows and
\ checker facts. Address-bearing fields use local offsets, literal rows or
\ imported declaration identities.

require src/lib/errors.f
require src/lib/string.f
require src/lib/le.f
require src/lib/memory.f
require src/core/sha256.f
require src/core/type-family.f
require src/habu/layout.f
require src/habu/xref.f
require src/habu/code-span.f
require src/habu/address-carrier.f
require src/habu/aot-data-cell.f
require src/habu/aot-arm.f
require src/compiler/native/a64ir.f
require src/compiler/native/branch.f
require src/compiler/native/dict.f
require src/compiler/native/compiler.f
require src/compiler/workspace.f
require tools/native-unit-capture.f
require tools/native-file-reference.f
require tools/native-unit-dependency.f
require tools/native-unit-file.f
require tools/native-file-inputs.f

package NUNIT-OBJECT

private

: BAD ( -- ) E-NUNIT-PROFILE throw ;

\ A FILE object holds every compiled `:` and TRUSTED: definition of one source
\ file with the file's input rows. Slot 0 holds the definitions' sections back
\ to back; slot 2 one INDEX-BYTES row per definition: its ordinal past the
\ file's first record, the length of each of its nine DEF-SLOTS sections in slot
\ order, then the length of its source text, from its name's start past its
\ body's `;` in the buffer of the file's run it was read from, its head's kind
\ (HEAD-KIND), that text's SHA-256 digest, the empty text's when the name lay in
\ none, and the SHA-256 digest of the file's own records, wordlists, DATA and
\ loads before its head (PREMISE), which a warm install's head must match. A
\ definition's slot 14 section holds the dependency rows its compile recorded,
\ which its head must still pass. Slot 11 holds the input rows. The file's own
\ records fall in segments, each a maximal run between child loads: slot 12
\ holds one EVENT-BYTES row per segment and per child load, in load order. A
\ warm load re-runs the file's top level, which makes its records, wordlists,
\ DATA, constants and slot values again: slots 1, 4, 6, 7, 8, 9, 10, 14, 15, 16,
\ 17 and 19 are empty. Slot 3 holds the names the event and origin rows point
\ at, and slot 13 the references the checker slices name, in NFILE-REF ENCODE
\ format with no declarations. Slot 5 holds the checker state the file added,
\ one slice before each child load and one at its end: the slice count, the
\ count of slot 13 references the slices name, those references, then each
\ slice's length and bytes padded to a cell. Slot 18 holds one ORIGIN-BYTES row
\ per definition its segment's origin sink recorded (its SEGMENT event row names
\ the sink), in the order its segments recorded them: its ordinal past the
\ file's first record, its name token's span from the start of its definition's
\ name in the source, and the name's and path's offsets and lengths in slot 3, a
\ zero span and path when the row is unlocated. A record with no index row text
\ has no row: a warm load runs the top level that makes it, which records its
\ rows.
58415 constant DEF-SLOTS
9 constant DEF-SECTIONS
14 constant DEPS-SLOT
\ An index row's source text: after the ordinal and the section lengths, its
\ length, its head's kind, then its 32-byte digest; then the head's 32-byte
\ premise digest.
DEF-SECTIONS 1+ 8 * constant SPAN-SIZE
SPAN-SIZE 8 + constant SPAN-HEAD
SPAN-HEAD 8 + constant SPAN-DIGEST
SPAN-DIGEST 32 + constant SPAN-PREMISE
SPAN-PREMISE 32 + constant INDEX-BYTES
\ An event row. SEGMENT: kind, first ordinal past the file's first record,
\ records, DATA bytes, wordlists, where in a cell HERE stood at its open, 0, and
\ the origin sink (SINK-*) live at its open.
\ LOAD: kind, load kind, source offset in the loading file, the requested
\ path's offset and length, the source offset the loading file resumes at, 0,
\ and the file's input bytes when the action started. Every load action writes
\ one: a loader token's at a loader cut of the file's own source, whether it
\ ran its child or found it loaded, once it returned with the loading file
\ balanced at its resume offset; and a child the file's top level loads from
\ inside a word it runs, once the child closed without a throw, at the input
\ cursor while the token whose run loaded it runs, as both its source and
\ resume offsets. Unused cells are 0.
88 constant EVENT-BYTES
\ An origin row: ordinal, start, end, name offset and length, path offset and
\ length.
56 constant ORIGIN-BYTES

public

\ Where a file segment's origin rows went as it loaded: no sink, the
\ builder's prefix rows, the early journal, the full journal.
0 constant SINK-NONE
1 constant SINK-PREFIX
2 constant SINK-EARLY
3 constant SINK-FULL

\ An event row's kind: a segment of the file's own records or a child load.
1 constant EV-SEGMENT
2 constant EV-LOAD

private

\ One frame per open file: the buffers' used sizes when it opened, its first
\ record ordinal, the open segment's marks (first ordinal, HERE, wordlist
\ counter), the checker slice bytes and reference entries when it opened, its
\ checker slice count, 1 while a checker mark is open, the origin bytes when
\ it opened, 1 while a load action is pending, the pending action's first
\ ordinal, source offset and input bytes, the open segment's origin sink,
\ and for its premise context (ABSORB) the ordinal it absorbed records up
\ to, the event row it absorbs next and its first PREMISE-SEGS entry. A
\ child file's bytes follow its parent's and are dropped when the child
\ closes.
0 constant FO-DEFS
1 constant FO-INDEX
2 constant FO-INPUTS
3 constant FO-WHY
4 constant FO-FIRST
5 constant FO-EVENTS
6 constant FO-NAMES
7 constant FO-SEG-ORD
8 constant FO-SEG-HERE
9 constant FO-SEG-WID
10 constant FO-CHECK
11 constant FO-CKORD
12 constant FO-SLICES
13 constant FO-CKMARK
14 constant FO-ORIGIN
15 constant FO-ACT
16 constant FO-ACT-ORD
17 constant FO-ACT-AT
18 constant FO-ACT-INPUTS
19 constant FO-SEG-SINK
20 constant FO-THROUGH
21 constant FO-EV-AT
22 constant FO-SEGS
23 constant FO-CELLS
\ An event row in memory, cell 1 an ordinal in both kinds so a dictionary cut
\ finds the rows it retired. SEGMENT: kind, first ordinal, records, HERE, DATA
\ bytes, first wordlist, wordlists, its origin sink, then zeros. LOAD: kind,
\ the first ordinal past the action's start, load kind, source offset, path
\ offset and length in
\ FILE-NAMES, the resume offset, 0, the file's input bytes at the action's
\ start, then zeros.
12 constant EV-CELLS
DYNAMIC-BUFFER FILE-DEFS u8
DYNAMIC-BUFFER FILE-INDEX u8
DYNAMIC-BUFFER FILE-INPUTS u8
DYNAMIC-BUFFER FILE-WHY u8
DYNAMIC-BUFFER FILE-FRAMES n
DYNAMIC-BUFFER FILE-EVENTS n
DYNAMIC-BUFFER FILE-NAMES u8
\ The open files' slot 18 bytes back to back.
DYNAMIC-BUFFER FILE-ORIGIN u8
variable ORIGIN-U
\ The closing file's event and checker sections, back to back.
DYNAMIC-BUFFER FILE-OUT u8
\ Each index row's dictionary ordinal, so a dictionary cut can find the rows
\ it retired. Never written to an object.
DYNAMIC-BUFFER FILE-ORDS n
\ Each index row's name start in the buffer of its file's run it was read
\ from, -1 when the row holds no text. Never written to an object.
DYNAMIC-BUFFER FILE-HEADS n
\ Each index row's checker state before its slice: CHECK-U and CKORD-N,
\ two cells per row, so a dictionary cut can drop the slices and references
\ of the rows it retired. Never written to an object.
DYNAMIC-BUFFER FILE-ROW-MARKS n
\ The context SPAN-HASH digests through.
DYNAMIC-BUFFER SPAN-CTX u8
\ SEEK-MATCH?'s digest of the live head's span.
DYNAMIC-BUFFER SEEK-HASH u8
\ One SHA-256 context per open file, its premise so far (ABSORB).
DYNAMIC-BUFFER PREMISE-CTX u8
\ The copy of a file's premise context that PREMISE finishes.
DYNAMIC-BUFFER PREMISE-OUT u8
\ PREMISE-N's little-endian cell.
DYNAMIC-BUFFER PREMISE-CELL u8
\ The pending definition's premise, taken at its head (DEFINITION-BEGIN).
DYNAMIC-BUFFER DEF-PREMISE u8
\ Per SEGMENT row an open file's premise absorbed, after its parent's: the
\ event row, then the file's own records, DATA bytes and wordlists before it.
DYNAMIC-BUFFER PREMISE-SEGS n
4 constant PSEG-CELLS
\ Premise tags, negative where a record's items start with its name length.
-1 constant PREMISE-SEGMENT
-2 constant PREMISE-LOAD
-3 constant PREMISE-CUT-MARK
\ The viewed object's origin rows by the ordinal they hold, past the file's
\ first record: the ordinal's first row plus one, 0 for none, and each row's
\ next row of the same ordinal plus one; ORIGIN-ORDS ordinals are indexed.
DYNAMIC-BUFFER ORIGIN-HEAD n
DYNAMIC-BUFFER ORIGIN-NEXT n
variable ORIGIN-ORDS
\ The open files' checker slices, each its length, then its bytes padded to
\ a cell, and the references they name: two cells per entry, a record's
\ ordinal and -1, or a DATA address and its byte count.
DYNAMIC-BUFFER FILE-CHECK u8
DYNAMIC-BUFFER FILE-CKORD n
variable CHECK-U
variable CKORD-N
variable CHECK-AT
variable OPEN-CHECKS
CAST: PROCESS-CELL-XT ( n -- [ ptr n -- ] )
\ Set when this engine's registry declares no process cells.
variable PROCESS-ABSENT
variable DEFS-U
variable INDEX-U
variable INPUTS-U
variable WHY-U
variable EVENTS-N
variable PSEG-N
variable NAMES-U
variable OUT-U
variable FO-DEPTH
variable OPEN-DEFS
variable OPEN-INPUTS
variable OPEN-RECS
variable OPEN-DATA
\ Where in a cell HERE stood when the open object's segment opened.
variable OPEN-REM
variable OPEN-WIDS
variable OPEN-LOADS
variable OPEN-SEGS
\ Whether the open FILE object's slot 18 is well formed: a malformed one is
\ a miss the hit check names.
variable OPEN-ORIGIN-OK
\ The object being installed and its key, and the next definition index row
\ and its payload's offset in the definitions slot.
PTR-VARIABLE OBJ-BYTES
variable OBJ-SIZE
PTR-VARIABLE OBJ-KEY
variable OBJ-KEY-U
variable INS-ROW
variable INS-DEFS-OFF
\ The installing files' segment map, MAP-CELLS cells per installed SEGMENT
\ row, innermost file last: the row's event index; its first own record row
\ and record count; its first wordlist and count among the file's own; its
\ first byte and count in the file's DATA section; its first ordinal past
\ the file's first record; then where the install put them: its first
\ record's ordinal, its first wordlist and its DATA address. The installing
\ file's rows start at INS-MAP-AT and INS-SEGS of them are installed. A
\ child file's rows follow its parent's.
11 constant MAP-CELLS
0 constant M-SEG
1 constant M-ROW
2 constant M-ROWS
3 constant M-WID
4 constant M-WIDS
5 constant M-DATA
6 constant M-BYTES
7 constant M-ORD
8 constant M-LIVE-ORD
9 constant M-LIVE-WID
10 constant M-LIVE-DATA
DYNAMIC-BUFFER INS-MAP n
variable INS-MAP-AT
variable INS-SEGS
\ The installing file's next checker slice.
variable INS-SLICE
\ The checker section and its count of references while a slice imports.
PTR-VARIABLE IMPORT-SEC
variable IMPORT-NREFS
\ The importing slice's offset in the checker section.
variable IMPORT-OFF
CAST: DEF-ACTION-XT ( n -- [ -- ] )
CAST: DEF-NUM-XT ( n -- [ -- n ] )

\ Field n of file frame k, outermost first.
: FRAME ( n n -- ptr n ) swap FO-CELLS * + FILE-FRAMES ;

: FO ( n -- ptr n )
   FO-DEPTH @ 0 <= if BAD then
   FO-DEPTH @ 1- swap FRAME ;

: EV ( n n -- ptr n ) swap EV-CELLS * + FILE-EVENTS ;

\ Field n of segment map row m.
: INS-CELL ( n n -- ptr n ) swap MAP-CELLS * + INS-MAP ;

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
\ The end of open file k's refusal reasons, where the next frame's begin.
: WHY-END ( n -- n )
   {: k:n :}
   k 1+ FO-DEPTH @ < if k 1+ FO-WHY FRAME @ else WHY-U @ then ;
\ Append a to open file k's refusal reasons; the later frames' reasons move
\ up by its length.
: FRAME-WHY-KEEP ( ptr u8 n n -- )
   {: a:ptr u:n k:n :}
   u 0= if exit then
   k WHY-END {: at:n :}
   WHY-U @ u + FILE-WHY-RESERVE
   WHY-U @ begin dup at > while
      1- dup FILE-WHY c@ over u + FILE-WHY c!
   repeat drop
   a at FILE-WHY u BYTE-COPY
   u WHY-U +!
   FO-DEPTH @ k 1+ ?do u i FO-WHY FRAME +! loop ;
\ Write a on fd 2 under the cache diagnostics.
: WHY+ ( ptr u8 n -- )
   {: a:ptr u:n :}
   NUNIT-DEP:DIAG? @ if a u NUNIT-DEP:DIAG-W2 then ;
: INDEX-N ( n -- )
   {: value:n :}
   INDEX-U @ 8 + FILE-INDEX-RESERVE
   value INDEX-U @ FILE-INDEX LE:U64!
   8 INDEX-U +! ;
\ The SHA-256 digest of text u bytes long into out.
: SPAN-HASH ( ptr u8 n ptr u8 -- )
   {: text:ptr u:n out:ptr :}
   SHA256-CTX-BYTES SPAN-CTX-RESERVE
   0 SPAN-CTX text u out SHA256-IN ;
\ The pending definition's head: 1 under TRUSTED:, 0 under `:`. The text an
\ index row digests starts at the name, so a row matches only a head of its
\ kind.
: HEAD-KIND ( -- n )
   data-base TRUSTED-CELL + CELL-VIEW @ 0<> if 1 else 0 then ;
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
: EV+ ( n n n n n n n n n -- )
   {: kind:n ord:n c2:n c3:n c4:n c5:n c6:n c7:n c8:n :}
   EVENTS-N @ {: row:n :}
   row 1+ EV-CELLS * FILE-EVENTS-RESERVE
   kind row 0 EV ! ord row 1 EV ! c2 row 2 EV ! c3 row 3 EV !
   c4 row 4 EV ! c5 row 5 EV ! c6 row 6 EV ! c7 row 7 EV ! c8 row 8 EV !
   0 row 9 EV ! 0 row 10 EV ! 0 row 11 EV !
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
: ORIGIN$ ( -- ptr u8 n )
   FO-ORIGIN FO @ {: first:n :}
   ORIGIN-U @ first = if NULL-PTR 0 exit then
   first FILE-ORIGIN ORIGIN-U @ first - ;
: ORIGIN-N ( n -- )
   {: value:n :}
   ORIGIN-U @ 8 + FILE-ORIGIN-RESERVE
   value ORIGIN-U @ FILE-ORIGIN LE:U64!
   8 ORIGIN-U +! ;
: OUT$ ( n n -- ptr u8 n )
   {: first:n last:n :}
   last first = if NULL-PTR 0 exit then
   first FILE-OUT last first - ;

: FRAME-REFUSED? ( n -- bool )
   {: k:n :}
   k WHY-END k FO-WHY FRAME @ > ;

public

: FILE-OPEN? ( -- bool ) FO-DEPTH @ 0 > ;
: FILE-REFUSED? ( -- bool ) FO-DEPTH @ 1- FRAME-REFUSED? ;

private

\ Under the cache diagnostics, one line for a reason a refused file meets
\ after the first, which it keeps.
: REFUSE-LINE ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: why:ptr whyu:n cause:ptr causeu:n name:ptr nameu:n :}
   NUNIT-DEP:DIAG? @ 0= if exit then
   s" native-file: reason " NUNIT-DEP:DIAG-W2 why whyu NUNIT-DEP:DIAG-W2
   causeu 0<> if s"  " NUNIT-DEP:DIAG-W2 cause causeu NUNIT-DEP:DIAG-W2 then
   nameu 0<> if s"  word=" NUNIT-DEP:DIAG-W2 name nameu NUNIT-DEP:DIAG-W2 then
   S\" \n" NUNIT-DEP:DIAG-W2 ;

\ Open file k refuses for real, keeping the reason, its cause when not empty
\ and, when name is not empty, the definition it is about: it stores no
\ object, so a warm build compiles it live. A file already refused keeps
\ its first reason.
: FRAME-KEEP ( ptr u8 n ptr u8 n ptr u8 n n -- )
   {: why:ptr whyu:n cause:ptr causeu:n name:ptr nameu:n k:n :}
   k FRAME-REFUSED? if
      why whyu cause causeu name nameu REFUSE-LINE exit
   then
   why whyu k FRAME-WHY-KEEP
   causeu 0<> if s"  " k FRAME-WHY-KEEP then
   cause causeu k FRAME-WHY-KEEP
   nameu 0= if exit then
   s"  word=" k FRAME-WHY-KEEP
   name nameu k FRAME-WHY-KEEP ;

public

\ FRAME-KEEP of the innermost file.
: REFUSE-KEEP ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: why:ptr whyu:n cause:ptr causeu:n name:ptr nameu:n :}
   FILE-OPEN? 0= if exit then
   why whyu cause causeu name nameu FO-DEPTH @ 1- FRAME-KEEP ;

\ A record that gets no object row refuses the innermost file for real.
: ROWLESS ( ptr u8 n ptr n -- )
   {: cause:ptr causeu:n rec:ptr :}
   s" rowless-record" cause causeu rec XREF-NAME$ REFUSE-KEEP ;

\ REFUSE-KEEP naming the record.
: REFUSE-WORD ( ptr u8 n ptr n -- )
   {: reason:ptr reasonu:n rec:ptr :}
   reason reasonu s" " rec XREF-NAME$ REFUSE-KEEP ;

private

\ The engine word name's xt, 0 when this engine lacks it.
: ENGINE-XT ( ptr u8 n -- n )
   XREF-FIND dup XREF-FOUND? 0= if drop 0 exit then
   dup XREF-RETIRED? if drop 0 exit then
   XREF-START dup cp@ >= if drop 0 then ;

public

\ Retire the checker's scratch (CHECKER-REG:FILE-SCRATCH-RETIRE) as a file's
\ segment opens, and as one closes unrefused; its chain refuses a declaration transaction open
\ across a file load (src/core/decl-event.f). The live dictionary's word is
\ the target's from the point its checker.f defines it, before the handover:
\ the target's PRIM: declarations already run its signature parser. Before
\ that point the target has no checker scratch, and nothing is retired.
: SCRATCH-RETIRE ( -- )
   s" CHECKER-REG:FILE-SCRATCH-RETIRE" ENGINE-XT
   dup 0= if drop exit then
   DEF-ACTION-XT execute ;

private

: SEGMENT-OPEN ( -- )
   ndict@ FO-SEG-ORD FO !
   AOT-ARM:HERE-N FO-SEG-HERE FO !
   AOT-ARM:WIDN FO-SEG-WID FO ! ;

\ Whether the live address records among the count ordinals from first stand
\ at or past the segment's start, each at or past the one before.
: ADDR-ORDERED? ( n n -- bool )
   {: first:n count:n :}
   FO-SEG-HERE FO @
   first count + first ?do
      i XREF-REC XREF-RETIRED? 0= if
         i NFILE-REF:FIXED@ {: kind:n va:n known:bool :}
         known kind CHECKER-OWNER-ABI:RECORD-FIXED-ADDR = and if
            va over < if drop false unloop exit then
            drop va
         then
      then
   loop
   drop true ;

\ The end of the DATA the innermost file's last SEGMENT row holds, or the
\ open segment's start when it has none.
: ROWS-END ( -- n )
   FO-SEG-HERE FO @
   EVENTS-N @ FO-EVENTS FO @ ?do
      i 0 EV @ EV-SEGMENT = if drop i 3 EV @ i 4 EV @ + then
   loop ;

\ Close the innermost file's open segment as one SEGMENT row. A segment that
\ added something but cannot be a row, because it went back or opened inside
\ the file's last row, refuses its file: a warm run installs only from rows.
: SEGMENT-CLOSE ( -- )
   FO-SEG-ORD FO @ {: first:n :}
   ndict@ first - {: count:n :}
   count 0 < if BAD then
   AOT-ARM:HERE-N FO-SEG-HERE FO @ - {: bytes:n :}
   AOT-ARM:WIDN FO-SEG-WID FO @ - {: wids:n :}
   count 0 > bytes 0 > or wids 0 > or {: added:bool :}
   bytes 0 < FO-SEG-HERE FO @ ROWS-END < or
   first count ADDR-ORDERED? 0= or wids 0 < or if
      added if s" segment-unordered" s" " s" " REFUSE-KEEP then exit
   then
   FILE-REFUSED? 0= if SCRATCH-RETIRE then
   added 0= if exit then
   EV-SEGMENT first count FO-SEG-HERE FO @ bytes
      FO-SEG-WID FO @ wids FO-SEG-SINK FO @ 0 EV+ ;

\ The end of open file k's event rows, where the next frame's begin.
: EV-HI ( n -- n )
   {: k:n :}
   k 1+ FO-DEPTH @ < if k 1+ FO-EVENTS FRAME @ else EVENTS-N @ then ;

: DEF-OWNER ( -- ptr u8 )
   data-base NCOMP-DISPATCH:DECL-CELL + 0 ptr-field @ ;

CAST: CK-EXPORT-XT ( n -- [ [ ptr u8 n -- n ] -- ptr u8 n ] )

: OWNER-CELL ( n -- n ) DEF-OWNER + CELL-VIEW @ ;
: CHECK-XT ( n -- n ) OWNER-CELL dup 0= if BAD then ;
\ The checker unit code the retained compiler names, 0 when absent.
: UNIT-CODE-XT ( ptr u8 n -- n ) NUNIT-CAPTURE:OPTIONAL-PRODUCER-XT ;
: UNIT-CODE ( ptr u8 n -- n ) UNIT-CODE-XT DEF-NUM-XT execute ;

\ The engine's checker marks, exports and imports file checker state, and
\ names the codes it throws when it cannot.
: FILE-CHECK-AVAILABLE? ( -- bool )
   DEF-OWNER {: owner:ptr :}
   owner NULL-PTR = if false exit then
   owner 16 - CELL-VIEW @ CHECKER-OWNER-ABI:MAGIC <> if false exit then
   owner 8 - CELL-VIEW @ CHECKER-OWNER-ABI:PENDING-DISCARD-OFF CELL + <
      if false exit then
   CHECKER-OWNER-ABI:PENDING-MARK-OFF OWNER-CELL 0=
   CHECKER-OWNER-ABI:PENDING-EXPORT-REFS-OFF OWNER-CELL 0= or
   CHECKER-OWNER-ABI:PENDING-PREPARE-OFF OWNER-CELL 0= or
   CHECKER-OWNER-ABI:PENDING-INSTALL-OFF OWNER-CELL 0= or
   CHECKER-OWNER-ABI:PENDING-DISCARD-OFF OWNER-CELL 0= or
   if false exit then
   s" CHECKER-REG:E-UNIT-STATE" UNIT-CODE-XT 0<>
   s" CHECKER-REG:E-UNIT-FORMAT" UNIT-CODE-XT 0<> and
   s" TFAM:E-REG-AOT-REFUSED" UNIT-CODE-XT 0<> and
   s" TFAM:E-UNIT-FAMILY-CHANGED" UNIT-CODE-XT 0<> and ;

\ rc is a checker unit code: the checker cannot mark or export this state.
: CHECK-CODE? ( n -- bool )
   {: rc:n :}
   rc s" CHECKER-REG:E-UNIT-STATE" UNIT-CODE =
   rc s" CHECKER-REG:E-UNIT-FORMAT" UNIT-CODE = or ;

\ A slice is one definition's pending unit, whose install leaves the
\ checker's closed definition as its `;` did (checker.f
\ PENDING-CLOSED-RESULT), so a following INTRINSIC tags it.
: CHECK-MARK-RUN ( -- )
   CHECKER-OWNER-ABI:PENDING-MARK-OFF CHECK-XT DEF-ACTION-XT execute ;

: CHECK-DISCARD ( -- )
   CHECKER-OWNER-ABI:PENDING-DISCARD-OFF CHECK-XT DEF-ACTION-XT execute ;

public

\ Mark the checker state where the innermost file's next slice starts; a
\ state the checker cannot mark refuses the file.
: CHECK-MARK ( -- )
   FILE-CHECK-AVAILABLE? 0= if
      s" checker-unavailable" s" " s" " REFUSE-KEEP exit
   then
   ['] CHECK-MARK-RUN catch {: rc:n :}
   rc 0= if 1 FO-CKMARK FO ! exit then
   rc s" CHECKER-REG:E-UNIT-STATE" UNIT-CODE = if
      s" checker-mark" s" " s" " REFUSE-KEEP exit
   then
   rc throw ;

\ Drop the innermost file's open mark without a slice.
: CHECK-DROP ( -- )
   FO-CKMARK FO @ 0= if exit then
   0 FO-CKMARK FO !
   CHECK-DISCARD ;

private

: SEG? ( n -- bool ) 0 EV @ EV-SEGMENT = ;

\ The closing file's record row of its own record ord, -1 when another file
\ made it.
: OWN-ROW ( n -- n )
   {: ord:n :}
   0 EVENTS-N @ FO-EVENTS FO @ ?do
      {: base:n :}
      i SEG? if
         i 1 EV @ {: first:n :}
         ord first >= ord first i 2 EV @ + < and if
            base ord + first - unloop exit
         then
         base i 2 EV @ +
      else base then
   loop drop -1 ;

\ Field n of PREMISE-SEGS entry e.
: PSEG ( n n -- ptr n ) swap PSEG-CELLS * + PREMISE-SEGS ;

\ The end of open file k's PREMISE-SEGS entries.
: PSEG-HI ( n -- n )
   {: k:n :}
   k 1+ FO-DEPTH @ < if k 1+ FO-SEGS FRAME @ else PSEG-N @ then ;

\ Whether open file k has an open segment: it is the innermost file and no
\ load action is pending.
: OPEN-SEG? ( n -- bool )
   {: k:n :}
   k 1+ FO-DEPTH @ = k FO-ACT FRAME @ 0= and ;

\ Open file k's own count over its absorbed SEGMENT rows: the PREMISE-SEGS
\ cell base holds the count before a row, its event row's cell cell the row's.
: OWN-TOTAL ( n n n -- n )
   {: cell:n base:n k:n :}
   k PSEG-HI {: hi:n :}
   hi k FO-SEGS FRAME @ = if 0 exit then
   hi 1- base PSEG @ hi 1- 0 PSEG @ cell EV @ + ;

\ The offset of value among open file k's own, in the latest absorbed
\ SEGMENT row whose event row cell cell starts at or before it, counted from
\ the PREMISE-SEGS cell base; -1 when that row's count, the next cell, ends
\ before it. Its end counts when end is true. Rows start in order.
: OWN-AT ( n n n bool n -- n )
   {: value:n cell:n base:n end:bool k:n :}
   k FO-SEGS FRAME @ {: lo:n :}
   lo k PSEG-HI begin 2dup < while
      2dup + 2 / {: l:n h:n m:n :}
      m 0 PSEG @ cell EV @ value > if l m else m 1+ h then
   repeat drop 1- {: e:n :}
   e lo < if -1 exit then
   e 0 PSEG @ {: row:n :}
   value row cell EV @ - {: off:n :}
   row cell 1+ EV @ {: count:n :}
   off count < end off count = and or if e base PSEG @ off + else -1 then ;

\ The offset of DATA address va in open file k's own DATA, its SEGMENT rows'
\ bytes then its open segment's; -1 when none holds it.
: OWN-DATA ( n n -- n )
   {: va:n k:n :}
   k OPEN-SEG? if
      k FO-SEG-HERE FRAME @ {: at:n :}
      va at >= va AOT-ARM:HERE-N <= and if 4 2 k OWN-TOTAL va + at - exit then
   then
   va 3 2 true k OWN-AT ;

\ Wordlist wid's index among open file k's own wordlists, counted as
\ OWN-DATA counts bytes; -1 when the file did not make it.
: OWN-WID ( n n -- n )
   {: wid:n k:n :}
   k OPEN-SEG? if
      k FO-SEG-WID FRAME @ {: first:n :}
      wid first >= wid AOT-ARM:WIDN < and if 6 3 k OWN-TOTAL wid + first - exit then
   then
   wid 5 3 false k OWN-AT ;

\ Open file k's premise context.
: FRAME-CTX ( n -- ptr u8 ) SHA256-CTX-BYTES * PREMISE-CTX ;

: PREMISE-N ( n ptr u8 -- )
   {: value:n ctx:ptr :}
   8 PREMISE-CELL-RESERVE
   value 0 PREMISE-CELL LE:U64!
   ctx 0 PREMISE-CELL 8 SHA256-FEED ;

\ Feed open file k's premise context the records from ordinal from to to that
\ it has not absorbed: each one's name, its body's offset in the file's own
\ DATA and its wordlist's index among the file's own, -1 for none, and for a
\ package's record its two wordlists'.
: ABSORB-RECORDS ( n n n -- )
   {: from:n to:n k:n :}
   from k FO-THROUGH FRAME @ max {: start:n :}
   to start max start ?do
      i XREF-REC {: rec:ptr :}
      rec XREF-NAME$ {: name:ptr nameu:n :}
      nameu k FRAME-CTX PREMISE-N
      k FRAME-CTX name nameu SHA256-FEED
      i NFILE-REF:DATA-ORIGIN-BASE {: va:n known:bool :}
      known if va k OWN-DATA else -1 then k FRAME-CTX PREMISE-N
      rec XREF-WORDLIST {: wl:n :}
      wl k OWN-WID k FRAME-CTX PREMISE-N
      wl XREF-NAMESPACE-WL = if
         1 k FRAME-CTX PREMISE-N
         rec XREF-PKG-PUBLIC k OWN-WID k FRAME-CTX PREMISE-N
         rec XREF-PKG-PRIVATE k OWN-WID k FRAME-CTX PREMISE-N
      else 0 k FRAME-CTX PREMISE-N then
   loop
   to k FO-THROUGH FRAME @ max k FO-THROUGH FRAME ! ;

\ Feed open file k's premise context the event rows it has not absorbed, in
\ order, then its open segment's records: a LOAD row's tag, or a SEGMENT
\ row's records, tag and record, DATA byte and wordlist counts.
: ABSORB ( n -- )
   {: k:n :}
   k EV-HI k FO-EV-AT FRAME @ {: hi:n at:n :}
   hi at max at ?do
      i SEG? if
         2 1 k OWN-TOTAL 4 2 k OWN-TOTAL 6 3 k OWN-TOTAL {: r:n d:n w:n :}
         PSEG-N @ {: e:n :}
         e 1+ PSEG-CELLS * PREMISE-SEGS-RESERVE
         i e 0 PSEG ! r e 1 PSEG ! d e 2 PSEG ! w e 3 PSEG !
         1 PSEG-N +!
         i 1 EV @ dup i 2 EV @ + k ABSORB-RECORDS
         PREMISE-SEGMENT k FRAME-CTX PREMISE-N
         i 2 EV @ k FRAME-CTX PREMISE-N
         i 4 EV @ k FRAME-CTX PREMISE-N
         i 6 EV @ k FRAME-CTX PREMISE-N
      else PREMISE-LOAD k FRAME-CTX PREMISE-N then
   loop
   hi at max k FO-EV-AT FRAME !
   k OPEN-SEG? if k FO-SEG-ORD FRAME @ ndict@ k ABSORB-RECORDS then ;

\ The innermost file's premise at its head into the 32 bytes at out: its
\ context once absorbed, finished on a copy after its open segment's record,
\ DATA byte and wordlist counts (native-file-frame-e2e.f PREMISE-REBUILT).
: PREMISE ( ptr u8 -- )
   {: out:ptr :}
   FO-DEPTH @ 1- {: k:n :}
   k ABSORB
   SHA256-CTX-BYTES PREMISE-OUT-RESERVE
   k FRAME-CTX 0 PREMISE-OUT SHA256-CTX-BYTES BYTE-COPY
   k OPEN-SEG? if
      ndict@ k FO-SEG-ORD FRAME @ - 0 PREMISE-OUT PREMISE-N
      AOT-ARM:HERE-N k FO-SEG-HERE FRAME @ - 0 PREMISE-OUT PREMISE-N
      AOT-ARM:WIDN k FO-SEG-WID FRAME @ - 0 PREMISE-OUT PREMISE-N
   then
   0 PREMISE-OUT out SHA256-END ;

\ Add a run of count own records from ordinal first to a cut to kept's
\ place among the file's own records, and whether the cut retires one.
: CUT-PLACE ( n bool n n n -- n bool )
   {: place:n retired:bool first:n count:n kept:n :}
   kept first - 0 max count min place +
   retired first count + kept > or ;

\ Before a cut to kept, open file k absorbs the records the cut retires and,
\ when one is its own, the cut's place among its own records.
: PREMISE-CUT ( n n -- )
   {: kept:n k:n :}
   k ABSORB
   0 false
   k PSEG-HI k FO-SEGS FRAME @ ?do
      i 0 PSEG @ {: row:n :}
      row 1 EV @ row 2 EV @ kept CUT-PLACE
   loop
   k OPEN-SEG? if
      k FO-SEG-ORD FRAME @ ndict@ over - kept CUT-PLACE
   then {: place:n retired:bool :}
   retired if
      PREMISE-CUT-MARK k FRAME-CTX PREMISE-N place k FRAME-CTX PREMISE-N
   then
   kept k FO-THROUGH FRAME @ min k FO-THROUGH FRAME ! ;

\ The 1-based index of the innermost file's reference entry key bytes,
\ appended when new.
: CHECK-ENTRY ( n n -- n )
   {: key:n bytes:n :}
   FO-CKORD FO @ {: first:n :}
   CKORD-N @ first ?do
      i 2 * FILE-CKORD @ key = i 2 * 1+ FILE-CKORD @ bytes = and if
         i first - 1+ unloop exit
      then
   loop
   CKORD-N @ {: row:n :}
   row 1+ 2 * FILE-CKORD-RESERVE
   key row 2 * FILE-CKORD ! bytes row 2 * 1+ FILE-CKORD !
   1 CKORD-N +!
   row first - 1+ ;

\ The checker's export callback: a dictionary record as its ordinal, any
\ other storage as its DATA address and size.
: CHECK-REF ( ptr u8 n -- n )
   {: at:ptr bytes:n :}
   at NULL-PTR BYTE-VIEW - dbase@ - {: off:n :}
   bytes DREC = off 0 >= and off ndict@ DREC * < and if
      off DREC mod 0<> if BAD then
      off DREC / -1 CHECK-ENTRY exit
   then
   at data-base BYTE-VIEW - DATA-VA VA>N + bytes CHECK-ENTRY ;

\ Append the checker state since the mark as one slice: its length, then
\ its bytes padded to a cell. The checker's buffer is shared, so the bytes
\ are copied before anything else runs. Then the file depends on each live
\ record an earlier file made that the slice newly names, so the rows of
\ the interval the slice closes precede the next load's.
: CHECK-EXPORT-RUN ( -- )
   CKORD-N @ {: entries:n :}
   ['] CHECK-REF CHECKER-OWNER-ABI:PENDING-EXPORT-REFS-OFF CHECK-XT
      CK-EXPORT-XT execute {: source:ptr bytes:n :}
   bytes 0 < if BAD then
   bytes 7 + -8 and {: padded:n :}
   CHECK-U @ {: at:n :}
   at CELL + padded + FILE-CHECK-RESERVE
   bytes at FILE-CHECK LE:U64!
   bytes 0<> if source at CELL + FILE-CHECK bytes BYTE-COPY then
   padded bytes ?do 0 at CELL + i + FILE-CHECK c! loop
   CELL padded + CHECK-U +!
   1 FO-SLICES FO +!
   CKORD-N @ entries ?do
      i 2 * FILE-CKORD @ {: key:n :}
      i 2 * 1+ FILE-CKORD @ 0 < key ndict@ < and if
         key OWN-ROW 0 < key XREF-REC XREF-RETIRED? 0= and if
            key NUNIT-DEP:BIND-ORD
         then
      then
   loop ;

public

\ Close the innermost file's open mark as a slice. A refused file exports
\ nothing; state the checker cannot export refuses it.
: CHECK-EXPORT ( -- )
   FO-CKMARK FO @ 0= if exit then
   FILE-REFUSED? if CHECK-DROP exit then
   ['] CHECK-EXPORT-RUN catch {: rc:n :}
   CHECK-DROP
   rc 0= if exit then
   rc CHECK-CODE? if s" checker-export" s" " s" " REFUSE-KEEP exit then
   rc throw ;

private

SRCLOC:PATH-CELL RESERVED-PTR-U8-CELL DEF-DIAG-PATH

\ The DATA record ord's origin names, and whether it names any, while the
\ record is live.
: REC-DATA? ( n -- n bool )
   {: ord:n :}
   ord NFILE-REF:DATA-ORIGIN-BASE {: base:n known:bool :}
   base known ord XREF-REC XREF-RETIRED? 0= and ;

\ The live record whose DATA starts nearest at or below va, and that start;
\ -1 when none does.
: OWNER-AT ( n -- n n )
   {: va:n :}
   -1 0
   ndict@ 0 ?do
      {: best:n at:n :}
      i REC-DATA? {: base:n known:bool :}
      known base va <= and base at >= best 0 < or and
      if i base else best at then
   loop ;

public

\ Whether the innermost file has a load action pending.
: LOAD-PENDING? ( -- bool ) FILE-OPEN? if FO-ACT FO @ 0<> else false then ;

\ Open a file frame at the file's first record ordinal. A child opens only
\ inside a load action of its parent (LOAD-ACTION); one its parent loads any
\ other way, as from an evaluated text or while a definition compiles,
\ refuses the parent.
: FILE-BEGIN ( -- )
   FILE-OPEN? LOAD-PENDING? 0= and if
      s" load-boundary" s" " s" " REFUSE-KEEP
   then
   \ The parent's PREMISE-SEGS entries precede the child's.
   FILE-OPEN? if FO-DEPTH @ 1- ABSORB then
   FO-DEPTH @ 1+ FO-CELLS * FILE-FRAMES-RESERVE
   1 FO-DEPTH +!
   DEFS-U @ FO-DEFS FO !
   INDEX-U @ FO-INDEX FO !
   INPUTS-U @ FO-INPUTS FO !
   WHY-U @ FO-WHY FO !
   ndict@ FO-FIRST FO !
   ndict@ FO-THROUGH FO !
   EVENTS-N @ FO-EV-AT FO !
   PSEG-N @ FO-SEGS FO !
   FO-DEPTH @ SHA256-CTX-BYTES * PREMISE-CTX-RESERVE
   FO-DEPTH @ 1- FRAME-CTX SHA256-BEGIN
   EVENTS-N @ FO-EVENTS FO !
   NAMES-U @ FO-NAMES FO !
   ORIGIN-U @ FO-ORIGIN FO !
   SINK-NONE FO-SEG-SINK FO !
   CHECK-U @ FO-CHECK FO !
   CKORD-N @ FO-CKORD FO !
   0 FO-SLICES FO !
   0 FO-CKMARK FO !
   0 FO-ACT FO !
   SEGMENT-OPEN
   \ A kernel without the cut notice lowers the count unseen, and a row it
   \ retired would stay in the object.
   s" native-unit-cut-cell" NUNIT-CAPTURE:OPTIONAL-PRODUCER-XT 0= if
      s" cut-notice-absent" s" " s" " REFUSE-KEEP
   then
   \ A kernel without the run observer leaves the frame without the rows of
   \ the words its source runs (NDEF-CACHE OPEN).
   NUNIT-CAPTURE:RUN-OBSERVER? 0= if
      s" run-observer-absent" s" " s" " REFUSE-KEEP
   then
   \ Without process cells TASK's slot observer and the seed's dictionary
   \ count (NDEF-CACHE BOOT-N) would survive into an image.
   PROCESS-ABSENT @ if s" process-cell-absent" s" " s" " REFUSE-KEEP then ;

\ Close the innermost frame; its parent's next segment opens when the
\ parent's loader action finishes (LOAD-FINISH).
: FILE-END ( -- )
   FO-DEFS FO @ DEFS-U !
   FO-INDEX FO @ INDEX-U !
   FO-INPUTS FO @ INPUTS-U !
   FO-WHY FO @ WHY-U !
   FO-EVENTS FO @ EVENTS-N !
   FO-NAMES FO @ NAMES-U !
   FO-ORIGIN FO @ ORIGIN-U !
   FO-CHECK FO @ CHECK-U !
   FO-CKORD FO @ CKORD-N !
   FO-SEGS FO @ PSEG-N !
   -1 FO-DEPTH +! ;

\ The innermost file's load action starts at source offset at: a loader
\ token's at a loader cut, or the token's whose run loads a child from a
\ word. The action ends the file's segment.
: LOAD-ACTION ( n -- )
   {: at:n :}
   FILE-OPEN? 0= if exit then
   SEGMENT-CLOSE
   ndict@ FO-ACT-ORD FO !
   at FO-ACT-AT FO !
   INPUTS-U @ FO-INPUTS FO @ - FO-ACT-INPUTS FO !
   1 FO-ACT FO ! ;

\ The pending load action of load kind kind and requested path returned.
\ When ok, the action is the file's LOAD row, resuming at source offset off;
\ otherwise the file has no LOAD row for it, and a warm run of the file stops
\ installing there (NDEF-CACHE LOAD-END). Either way the file's next segment
\ opens and the checker's scratch retires after the child's capture closed.
: LOAD-FINISH ( n ptr u8 n n bool -- )
   {: kind:n path:ptr pathu:n off:n ok:bool :}
   FILE-OPEN? 0= if exit then
   ok if
      EV-LOAD FO-ACT-ORD FO @ kind FO-ACT-AT FO @ path pathu NAMES+ pathu off
         0 FO-ACT-INPUTS FO @ EV+
   then
   SEGMENT-OPEN SCRATCH-RETIRE
   0 FO-ACT FO ! ;

\ The innermost file's open segment, or the one its pending load action's end
\ opens (LOAD-FINISH), opened under origin sink kind.
: ORIGIN-SINK! ( n -- )
   FILE-OPEN? 0= if drop exit then
   FO-SEG-SINK FO ! ;

private

\ The name start of the innermost file's index row for ord, -1 when it has
\ none or the row holds no text.
: ORD-HEAD ( n -- n )
   {: ord:n :}
   INDEX-U @ INDEX-BYTES / FO-INDEX FO @ INDEX-BYTES / ?do
      i FILE-ORDS @ ord = if i FILE-HEADS @ unloop exit then
   loop -1 ;

public

\ The innermost file's origin row: the definition at ord, its name token's
\ span at to end in the file at path, and its name. A row naming no record
\ of the file refuses the file. A located span is stored from its
\ definition's name start, so the row holds no file position; a definition
\ whose index row holds no text stores no row.
: ORIGIN+ ( n n n ptr u8 n ptr u8 n -- )
   {: ord:n at:n end:n path:ptr pathu:n name:ptr nameu:n :}
   ord FO-FIRST FO @ < ord ndict@ >= or if
      s" origin-record" s" foreign" name nameu REFUSE-KEEP exit
   then
   ord ORD-HEAD {: head:n :}
   head 0 < if exit then
   pathu 0<> at head < and if
      s" origin-record" s" before-head" name nameu REFUSE-KEEP exit
   then
   pathu 0= if 0 else head then {: base:n :}
   FO-NAMES FO @ {: names:n :}
   ord FO-FIRST FO @ - ORIGIN-N
   at base - ORIGIN-N end base - ORIGIN-N
   name nameu NAMES+ names - ORIGIN-N nameu ORIGIN-N
   pathu 0= if 0 else path pathu NAMES+ names - then ORIGIN-N
   pathu ORIGIN-N ;

: REASON$ ( -- ptr u8 n )
   FO-WHY FO @ {: first:n :}
   WHY-U @ first = if NULL-PTR 0 exit then
   first FILE-WHY WHY-U @ first - ;

: FILE-INPUTS+ ( ptr u8 n -- )
   FILE-OPEN? 0= if 2drop exit then
   INPUTS+ ;

: CLOSE ( -- )
   0 DEFS-U ! 0 INDEX-U ! 0 INPUTS-U ! 0 WHY-U ! 0 FO-DEPTH !
   0 EVENTS-N ! 0 NAMES-U ! 0 ORIGIN-U ! 0 OUT-U !
   0 OPEN-DEFS ! 0 OPEN-INPUTS !
   0 OPEN-RECS ! 0 OPEN-DATA ! 0 OPEN-WIDS !
   0 OPEN-LOADS ! 0 OPEN-SEGS ! 0 OPEN-REM !
   0 OPEN-ORIGIN-OK !
   NULL-PTR OBJ-BYTES ! 0 OBJ-SIZE ! NULL-PTR OBJ-KEY ! 0 OBJ-KEY-U !
   0 INS-ROW ! 0 INS-DEFS-OFF !
   0 INS-MAP-AT ! 0 INS-SEGS ! 0 INS-SLICE ! INS-MAP-RELEASE
   NULL-PTR IMPORT-SEC ! 0 IMPORT-NREFS ! 0 IMPORT-OFF !
   0 CHECK-U ! 0 CKORD-N ! 0 CHECK-AT ! 0 OPEN-CHECKS !
   FILE-CHECK-RELEASE FILE-CKORD-RELEASE
   FILE-DEFS-RELEASE FILE-INDEX-RELEASE FILE-INPUTS-RELEASE
   FILE-WHY-RELEASE FILE-FRAMES-RELEASE FILE-ORDS-RELEASE FILE-HEADS-RELEASE
   FILE-ROW-MARKS-RELEASE
   SPAN-CTX-RELEASE SEEK-HASH-RELEASE
   PREMISE-CTX-RELEASE PREMISE-OUT-RELEASE PREMISE-CELL-RELEASE
   DEF-PREMISE-RELEASE PREMISE-SEGS-RELEASE 0 PSEG-N !
   ORIGIN-HEAD-RELEASE ORIGIN-NEXT-RELEASE 0 ORIGIN-ORDS !
   FILE-EVENTS-RELEASE FILE-NAMES-RELEASE FILE-ORIGIN-RELEASE FILE-OUT-RELEASE
   NUNIT-FILE:CLOSE
   NFILE-SCOPE:SCOPE-CLOSE
   NFILE-INPUTS:CLOSE
   NFILE-REF:CLOSE
   NUNIT-CAPTURE:CLOSE ;

\ Declare a cell whose value belongs to this process through
\ IMAGE-LIFECYCLE:PROCESS-CELL. It is looked up: the previous-generation host's
\ baked registry has none, and then every file refuses process-cell-absent.
: PROCESS-CELL ( ptr n -- )
   s" IMAGE-LIFECYCLE" -1 search-wl {: wid:n :}
   wid 0= if drop 1 PROCESS-ABSENT ! exit then
   s" PROCESS-CELL" wid search-wl {: xt:n :}
   xt 0= if drop 1 PROCESS-ABSENT ! exit then
   xt PROCESS-CELL-XT execute ;

private

CAST: OBSERVER-CELL-XT ( n -- [ -- ptr n ] )
\ TASK's slot observer belongs to the build: a process cell, so an image
\ captured inside a file stores 0 while this builder keeps it. Looked up as
\ PROCESS-CELL is: an engine without it reports no slot words.
: TASK-OBSERVER-CELL ( -- )
   s" TASK" -1 search-wl {: wid:n :}
   wid 0= if exit then
   s" SLOT-OBSERVER-CELL" wid search-wl {: xt:n :}
   xt 0= if exit then
   xt OBSERVER-CELL-XT execute PROCESS-CELL ;

TASK-OBSERVER-CELL

\ A definition payload is its DEF-SLOTS sections in the FILE object, framed
\ by its index row. Section 1 is one 80-byte metadata row; section 15
\ carries the function facts taken by the live native publisher.
40 constant DEF-CALL-BYTES
40 constant DEF-ADDR-BYTES
80 constant DEF-META-BYTES
\ The site kinds section 2 call rows and section 3 address rows carry: the
\ format's own numbering, equal to the values the objects already on disk hold.
\ Named here because the tool runs on the host, the window is the tree's
\ compiler, and the object outlives both.
0 constant CALL
1 constant TAIL
1 constant ADDR-DATA
2 constant ADDR-CODE
\ The window's numbering of the same kinds, read at each definition's begin
\ because BIND-PRODUCER re-points the bound names when src/habu/native-runtime.f
\ finishes (tools/native-definition-cache.f), and cleared by DEF-RESET: a
\ builder saved after a cold build must match one saved after a warm build.
variable WIN-CALL
variable WIN-TAIL
variable WIN-ADDR-DATA
variable WIN-ADDR-CODE
DYNAMIC-BUFFER DEF-CODE u8
DYNAMIC-BUFFER DEF-CALL u8
DYNAMIC-BUFFER DEF-ADDR u8
DYNAMIC-BUFFER DEF-LIT u8
DYNAMIC-BUFFER DEF-HOST u8
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
variable DEF-LIT-N
variable DEF-CALL-SITE-N
variable DEF-ADDR-SITE-N
variable DEF-PENDING
variable DEF-CODE-BASE
variable DEF-RECORDED
variable DEF-WIDE
variable DEF-MIN-IN
variable DEF-DOES-OFF
variable DEF-ACTIVE
variable DEF-EMITTED
variable DEF-FINISHED
variable DEF-PREPARED
variable DEF-PLAN-ON
variable DEF-CACHEABLE
\ The captured definition's HEAD-KIND, taken at its begin: its `;` clears
\ the trusted cell before the row is written.
variable DEF-HEAD
variable DEF-MODULE-DELTA
variable DEF-GEN-DELTA
variable DEF-EPOCH-DELTA
variable DEF-BUILD-DELTA

public
: REPORT-STORE-REFUSED ( ptr u8 n n -- )
   {: reason:ptr reasonu:n ord:n :}
   NUNIT-DEP:DIAG? @ 0= if exit then
   s" native-cache: cache-store-refused reason=" NUNIT-DEP:DIAG-W2
   reason reasonu NUNIT-DEP:DIAG-W2
   s"  name=" NUNIT-DEP:DIAG-W2 ord XREF-REC XREF-NAME$ NUNIT-DEP:DIAG-W2
   data-base SRCLOC:PATHLEN-CELL + @ {: pathu:n :}
   pathu 0 > if
      s"  path=" NUNIT-DEP:DIAG-W2 DEF-DIAG-PATH @ pathu NUNIT-DEP:DIAG-W2
   then
   S\" \n" NUNIT-DEP:DIAG-W2 ;
private

CAST: DEF-ONE-XT ( n -- [ n -- n ] )
CAST: DEF-TWO-XT ( n -- [ n n -- n ] )
CAST: DEF-PTR-XT ( n -- [ -- ptr u8 ] )
CAST: DEF-FACTS-XT ( n -- [ n -- n n n n n n ] )
CAST: DEF-PREP-HOST-XT ( n -- [ n n ptr u8 n ptr u8 n -- ] )
CAST: DEF-PUBLISH-XT ( n -- [ ptr u8 n n ptr n n ptr n n bool n -- n ] )
CAST: DEF-PUBLISH-DOES-XT ( n -- [ ptr u8 n n ptr n n ptr n n bool n n -- n ] )
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
   s" NSTR:SOURCE-SPAN" NUNIT-CAPTURE:OPTIONAL-PRODUCER-XT 0<> and
   s" NSTR:INTERN"NUNIT-CAPTURE:OPTIONAL-PRODUCER-XT 0<> and
   s" NSTR:SOURCE-BEGIN" NUNIT-CAPTURE:OPTIONAL-PRODUCER-XT 0<> and
   s" NSTR:SOURCE-END" NUNIT-CAPTURE:OPTIONAL-PRODUCER-XT 0<> and
   s" NSTR:SOURCE-COUNT" NUNIT-CAPTURE:OPTIONAL-PRODUCER-XT 0<> and
   s" NSTR:SOURCE@" NUNIT-CAPTURE:OPTIONAL-PRODUCER-XT 0<> and
   s" NSTR:SOURCE-CLOSE" NUNIT-CAPTURE:OPTIONAL-PRODUCER-XT 0<> and
   s" NHOST:PREPARE-IMPORT" NUNIT-CAPTURE:OPTIONAL-PRODUCER-XT 0<> and
   s" NHOST:ABANDON-TAKEN" NUNIT-CAPTURE:OPTIONAL-PRODUCER-XT 0<> and
   s" NPUB:IMPORT-PENDING" NUNIT-CAPTURE:OPTIONAL-PRODUCER-XT 0<> and
   s" NCOMP:DEFINITION-DOES-OFFSET" NUNIT-CAPTURE:OPTIONAL-PRODUCER-XT 0<> and
   s" NPUB:IMPORT-PENDING-DOES" NUNIT-CAPTURE:OPTIONAL-PRODUCER-XT 0<> and ;
\ Whether the compiler would let the current definition stand alone in a
\ cache entry. One it would not joins its file's object as a live head
\ (DEF-SECTION).
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
: DEF-PREP-CLOSE ( -- )
   s" NSTR:PREPARE-CLOSE" DEF-TARGET DEF-ACTION-XT execute ;
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
: DEF-BYTES ( -- ptr u8 )
   s" NEMIT:BYTES" DEF-TARGET DEF-PTR-XT execute ;

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

\ Whether the window names the four site kinds; one without them caches no
\ definition.
: WIN-KINDS? ( -- bool )
   s" NEMIT:CALL" NUNIT-CAPTURE:OPTIONAL-PRODUCER-XT 0<>
   s" NEMIT:TAIL" NUNIT-CAPTURE:OPTIONAL-PRODUCER-XT 0<> and
   s" NEMIT:ADDR-DATA" NUNIT-CAPTURE:OPTIONAL-PRODUCER-XT 0<> and
   s" NEMIT:ADDR-CODE" NUNIT-CAPTURE:OPTIONAL-PRODUCER-XT 0<> and ;
: WIN-KINDS-READ ( -- )
   s" NEMIT:CALL" DEF-NUM WIN-CALL !
   s" NEMIT:TAIL" DEF-NUM WIN-TAIL !
   s" NEMIT:ADDR-DATA" DEF-NUM WIN-ADDR-DATA !
   s" NEMIT:ADDR-CODE" DEF-NUM WIN-ADDR-CODE ! ;

\ The object's kind of the window's call kind and address kind.
: >CALL-KIND ( n -- n )
   {: kind:n :}
   kind WIN-CALL @ = if CALL
   else kind WIN-TAIL @ = if TAIL
   else BAD then then ;
: >ADDR-KIND ( n -- n )
   {: kind:n :}
   kind WIN-ADDR-CODE @ = if ADDR-CODE
   else kind WIN-ADDR-DATA @ = if ADDR-DATA
   else BAD then then ;

: DEF-CAPTURE-CALLS ( n n -- )
   {: fn:n size:n :}
   s" NEMIT:CALL-SITES" DEF-NUM 0 ?do
      i s" NEMIT:CALL-SITE@" DEF-ONE {: site:n :}
      i s" NEMIT:CALL-KIND@" DEF-ONE >CALL-KIND {: kind:n :}
      i s" NEMIT:CALL-TARGET@" DEF-ONE {: target:n :}
      site 0 < site 4 + size > or if BAD then
      target NFILE-REF:REF-CODE {: ref:n :}
      i s" NEMIT:CALL-LOC@" DEF-ONE {: loc:n :}
      i s" NEMIT:CALL-IMPL@" DEF-ONE 0<> if 1 else 0 then
         {: known:n :}
      site kind ref loc known DEF-CALL+
      target fn < target fn size + >= or if
         site DEF-CODE {: at:ptr :}
         kind CALL = if
            fn site + target NBR:BL-WORD at LE:U32!
         else
            fn site + target NBR:B-WORD at LE:U32!
         then
      then
   loop ;

: DEF-ADDR-REF ( n n n -- n )
   {: kind:n target:n owner:n :}
   kind ADDR-CODE = if target NFILE-REF:REF-CODE exit then
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

: DEF-CAPTURE-ADDRS ( n -- )
   {: size:n :}
   s" NEMIT:ADDR-SITES" DEF-NUM 0 ?do
      i s" NEMIT:ADDR-SITE@" DEF-ONE {: site:n :}
      i s" NEMIT:ADDR-SITE-KIND@" DEF-ONE >ADDR-KIND {: kind:n :}
      i s" NEMIT:ADDR-SITE-ORIGIN@" DEF-ONE {: owner:n :}
      kind ADDR-CODE = if ADDRESS-CARRIER:ADDR-CHAIN-BYTES
      else ADDRESS-CARRIER:DATA-CHAIN-BYTES then {: width:n :}
      site 0 < site width + size > or if BAD then
      site DEF-CODE {: carrier:ptr :}
      carrier 0 DEF-CODE size + ADDRESS-CARRIER:CHAIN-SIZE
         width <> if BAD then
      carrier width ADDRESS-CARRIER:CHAIN-VALUE {: target:n :}
      kind target owner DEF-ADDR-REF {: ref:n :}
      site kind ref DEF-ADDR+
      carrier 0 width ADDRESS-CARRIER:SET-CHAIN-VALUE
   loop ;

: DEF-CAPTURE-HOST ( -- )
   0 DEF-HOST-U !
   s" NHOST:TAKEN-FUNCTIONS" DEF-NUM {: funs:n :}
   funs 0 <= if BAD then
   funs DEF-HOST-N
   funs 0 ?do
      i s" NHOST:TAKEN-FACTS" DEF-TARGET DEF-FACTS-XT execute
         {: len:n din:n dout:n reason:n loc:n types:n :}
      i s" NEMIT:FUNCTION-OFFSET@" DEF-ONE DEF-HOST-N
      len DEF-HOST-N din DEF-HOST-N dout DEF-HOST-N
      reason DEF-HOST-N loc DEF-HOST-N types DEF-HOST-N
      types 0 > if
         types 0 ?do
            j i s" NHOST:TAKEN-TYPE@" DEF-TWO DEF-HOST-N
         loop
      then
   loop ;

\ Register each hidden function's entry under the record whose code holds it:
\ a does> definer's clause record, the one after it, spans the body from the
\ clause on, so a reference to the clause or to code inside it names that
\ record, as an imported definer's published spans resolve it.
: DEF-FUNCTION-OWNER ( n -- n )
   {: off:n :}
   DEF-DOES-OFF @ 0<> off DEF-DOES-OFF @ >= and if DEF-PENDING @ 1+ exit then
   DEF-PENDING @ ;

: DEF-REGISTER-FUNCTIONS ( -- )
   0 DEF-HOST LE:U64@ {: funs:n :}
   8
   funs 0 ?do
      {: cursor:n :}
      cursor DEF-HOST {: row:ptr :}
      i 0<> if
         row LE:U64@ {: off:n :}
         DEF-CODE-BASE @ off + off DEF-FUNCTION-OWNER NFILE-REF:REGISTER-IMPL
      then
      cursor 7 cells + row 48 + LE:U64@ 0 max cells +
   loop drop ;

: DEF-CAPTURE ( n n n -- )
   {: idx:n fn:n size:n :}
   DEF-ACTIVE @ 0= if exit then
   NUNIT-CAPTURE:PROFILE-CK
   DEF-EMITTED @ if BAD then
   idx DEF-PENDING @ <> size 0 <= or if BAD then
   fn DEF-CODE-BASE !
   size DEF-CODE-RESERVE
   DEF-BYTES 0 DEF-CODE size BYTE-COPY
   size DEF-CODE-U !
   fn size idx NFILE-REF:BEGIN-DEFINITION-BOUNDS
   fn size DEF-CAPTURE-CALLS
   size DEF-CAPTURE-ADDRS
   DEF-CAPTURE-HOST
   1 DEF-EMITTED ! ;

variable DEF-REFUSED

\ The capture layer cannot hold definition ord: no cache entry.
: REFUSED ( ptr u8 n n -- )
   {: reason:ptr reasonu:n ord:n :}
   reason reasonu ord REPORT-STORE-REFUSED
   1 DEF-REFUSED ! ;

variable DEF-HELD-IDX
variable DEF-HELD-FN
variable DEF-HELD-SIZE
: DEF-CAPTURE-HELD ( -- )
   DEF-HELD-IDX @ DEF-HELD-FN @ DEF-HELD-SIZE @
      DEF-CAPTURE ;
: DEF-OBSERVE ( n n n -- )
   {: idx:n fn:n size:n :}
   DEF-ACTIVE @ 0= if exit then
   idx DEF-HELD-IDX !
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
   DEF-DOES-OFF @ DEF-META 40 + LE:U64!
   DEF-MODULE-DELTA @ DEF-META 48 + LE:U64!
   DEF-GEN-DELTA @ DEF-META 56 + LE:U64!
   DEF-EPOCH-DELTA @ DEF-META 64 + LE:U64!
   DEF-BUILD-DELTA @ DEF-META 72 + LE:U64! ;

\ The finished definition's section in DEF-SLOTS slot slot. Slot 5 is empty:
\ the file's segment carries the checker slice. Slot 14 holds the rows of the
\ definition frame open around it, empty (a live head, HEAD-SEEK) when a row
\ is ORDERED or the compile does work no install repeats (DEF-CACHEABLE;
\ NDEFINITION:CALLBACK-STUB@'s C entry).
: DEF-SECTION ( n -- ptr u8 n )
   {: slot:n :}
   slot 0 = if 0 DEF-CODE DEF-CODE-U @ exit then
   slot 1 = if DEF-META DEF-META-BYTES exit then
   slot 2 = if DEF-CALL-U @ 0= if NULL-PTR 0 exit then
      0 DEF-CALL DEF-CALL-U @ exit then
   slot 3 = if DEF-ADDR-U @ 0= if NULL-PTR 0 exit then
      0 DEF-ADDR DEF-ADDR-U @ exit then
   slot 5 = if NULL-PTR 0 exit then
   slot 10 = if DEF-LIT-U @ 0= if NULL-PTR 0 exit then
      0 DEF-LIT DEF-LIT-U @ exit then
   slot 13 = if DEF-PENDING @ dup 1+ NFILE-REF:ENCODE exit then
   slot DEPS-SLOT = if
      DEF-CACHEABLE @ 0= NUNIT-DEP:DEFINITION-STORABLE? 0= or if
         NULL-PTR 0 exit
      then
      NUNIT-DEP:DEFINITION-EXPORT exit
   then
   slot 15 = if 0 DEF-HOST DEF-HOST-U @ exit then
   BAD ;

: DEF-RESET ( -- )
   0 DEF-ACTIVE ! 0 DEF-EMITTED ! 0 DEF-FINISHED !
   0 DEF-PREPARED ! 0 DEF-REFUSED ! 0 DEF-CACHEABLE !
   0 DEF-CODE-U ! 0 DEF-CALL-U ! 0 DEF-ADDR-U !
   0 DEF-LIT-U ! 0 DEF-HOST-U !
   0 DEF-LIT-N ! 0 DEF-CALL-SITE-N ! 0 DEF-ADDR-SITE-N !
   0 DEF-PENDING ! 0 DEF-CODE-BASE ! 0 DEF-DOES-OFF !
   0 DEF-MODULE-DELTA ! 0 DEF-GEN-DELTA !
   0 DEF-EPOCH-DELTA ! 0 DEF-BUILD-DELTA !
   0 DEF-HELD-IDX ! 0 DEF-HELD-FN ! 0 DEF-HELD-SIZE !
   0 DEF-PLAN-ON !
   0 WIN-CALL ! 0 WIN-TAIL ! 0 WIN-ADDR-DATA ! 0 WIN-ADDR-CODE ! ;

: DEF-DROP-PLAN ( -- n )
   DEF-SOURCE-END
   ['] DEF-ABANDON-HOST catch
   NFILE-REF:END-CAPTURE
   NFILE-REF:DISCARD-PREPARED
   DEF-RESET ;

\ The metadata row's own bounds, with no session state: the recorded length
\ matches the code bytes, the code is whole instructions, the flags are in
\ range.
: META-CK ( ptr u8 n -- )
   {: meta:ptr size:n :}
   size DEF-META-BYTES <> if BAD then
   meta 16 + LE:U64@ dup 0<> swap 1 <> and if BAD then
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
   meta 40 + LE:U64@ DEF-DOES-OFF !
   meta 48 + LE:U64@ DEF-MODULE-DELTA !
   meta 56 + LE:U64@ DEF-GEN-DELTA !
   meta 64 + LE:U64@ DEF-EPOCH-DELTA !
   meta 72 + LE:U64@ DEF-BUILD-DELTA !
   ['] DEF-HISTORY-CHECK catch {: history-rc:n :}
   history-rc E-IR-MODULE-EXHAUSTED =
   history-rc E-IR-CTX-SERIALS = or
   history-rc E-IR-BUILD-SERIALS = or if BAD then
   history-rc 0<> if history-rc throw then ;

\ Literal row number row at off in the literal section: its byte length,
\ within the section, and the first row with the same address, within
\ bounds.
: LIT-ROW ( ptr u8 n n n -- n n )
   {: src:ptr size:n off:n row:n :}
   size off - 16 < if BAD then
   src off + LE:U64@ {: bytes:n :}
   src off 8 + + LE:U64@ {: first:n :}
   first 0 < first row > or if BAD then
   bytes 0 < bytes size off 16 + - > or if BAD then
   bytes first ;

\ Whether each row shares as the capture recorded.
: DEF-PLAN-LITERALS? ( -- bool )
   10 NUNIT-FILE:SECTION$ {: src:ptr size:n :}
   0 DEF-LIT-U !
   DEF-LIT-N @ DEF-LIT-ADDR-RESERVE
   DEF-LIT-N @ DEF-LIT-LEN-RESERVE
   true
   DEF-LIT-N @ 0 ?do
      src size DEF-LIT-U @ i LIT-ROW {: bytes:n first:n :}
      bytes i DEF-LIT-LEN !
      src DEF-LIT-U @ 16 + + bytes DEF-PREP-INTERN
         {: address:n :}
      address i DEF-LIT-ADDR !
      address i DEF-FIRST-ADDR first = and
      bytes 16 + DEF-LIT-U +!
   loop
   DEF-LIT-U @ size <> if BAD then ;

\ HEAD-SEEK found the capture's sharing in this pool (SHARING-HELD?).
: DEF-READ-LITERALS ( -- )
   DEF-PLAN-LITERALS? 0= if BAD then ;

: DEF-PREP-CALL ( n -- )
   {: row:n :}
   2 NUNIT-FILE:SECTION$ drop row DEF-CALL-BYTES * + {: src:ptr :}
   src LE:U64@ {: site:n :}
   src 8 + LE:U64@ {: kind:n :}
   site 0 < DEF-CODE-U @ 4 < or if BAD then
   site DEF-CODE-U @ 4 - > if BAD then
   kind CALL <> kind TAIL <> and if BAD then
   src 16 + LE:U64@ NFILE-REF:REF>CODE {: target:n :}
   row DEF-CALL-BYTES * DEF-CALL {: dst:ptr :}
   src dst DEF-CALL-BYTES BYTE-COPY
   target dst 16 + LE:U64!
   site DEF-CODE {: at:ptr :}
   kind CALL = if
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
   kind ADDR-CODE = if ADDRESS-CARRIER:ADDR-CHAIN-BYTES
   else kind ADDR-DATA = if ADDRESS-CARRIER:DATA-CHAIN-BYTES
   else BAD then then {: width:n :}
   site 0 < width DEF-CODE-U @ > or if BAD then
   site DEF-CODE-U @ width - > if BAD then
   src 16 + LE:U64@ 0<> src 32 + LE:U64@ 0<> or if BAD then
   src 24 + LE:U64@ {: ref:n :}
   kind ADDR-CODE = if ref NFILE-REF:REF>CODE else
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

public

: WITH-DEFINITIONS ( [ -- ] -- )
   ['] DEF-OBSERVE swap NUNIT-CAPTURE:WITH-OBSERVER ;

: DEFINITION-BEGIN ( -- )
   DEF-RESET
   HEAD-KIND DEF-HEAD !
   FILE-OPEN? if 32 DEF-PREMISE-RESERVE 0 DEF-PREMISE PREMISE then
   DEF-ELIGIBLE? WIN-KINDS? and 0= if
      s" begin-ineligible" ndict@ REFUSED
      s" begin-ineligible" s" " s" " REFUSE-KEEP exit
   then
   WIN-KINDS-READ
   ndict@ DEF-PENDING !
   1 DEF-ACTIVE !
   DEF-CACHEABLE-NOW? if 1 DEF-CACHEABLE ! then
   DEF-SOURCE-BEGIN ;

: DEFINITION-END ( -- bool )
   DEF-ACTIVE @ 0= if DEF-SOURCE-END false exit then
   0 DEF-ACTIVE !
   DEF-ELIGIBLE? 0= if
      s" end-ineligible" DEF-PENDING @ REFUSED
      s" end-ineligible" s" " s" " REFUSE-KEEP
   then
   DEF-EMITTED @ 0= DEF-REFUSED @ 0<> or if
      DEF-REFUSED @ 0= if
         s" no-emission" DEF-PENDING @ REFUSED
         s" no-emission" s" " s" " REFUSE-KEEP
      then
      DEF-SOURCE-END
      NFILE-REF:END-CAPTURE false exit
   then
   s" NCOMP:DEFINITION-DOES-OFFSET" DEF-NUM DEF-DOES-OFF !
   \ A does> definer published its clause as the next record.
   ndict@ DEF-PENDING @ 1+ DEF-DOES-OFF @ 0<> if 1+ then <> if BAD then
   DEF-CACHEABLE-NOW? 0= if 0 DEF-CACHEABLE ! then
   DEF-FIELDS
   DEF-REGISTER-FUNCTIONS
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

private

\ Keep the index row the innermost file appended at index, its payload at
\ defs, for the definition at ord whose name starts at head, -1 when none:
\ the checker state since its CHECK-MARK becomes its slice. A refused file
\ drops the payload, and so does a definition with no slice.
: ROW-KEEP ( n n n n -- )
   {: ord:n head:n defs:n index:n :}
   FILE-REFUSED? if defs DEFS-U ! index INDEX-U ! exit then
   \ A definition the checker could not mark or export has no row.
   FO-CKMARK FO @ 0= if
      s" checker-mark" ord XREF-REC ROWLESS
      defs DEFS-U ! index INDEX-U ! exit
   then
   FO-SLICES FO @ {: slices:n :}
   CHECK-U @ {: check:n :}
   CKORD-N @ {: entries:n :}
   CHECK-EXPORT
   FO-SLICES FO @ slices = if
      s" checker-export" ord XREF-REC ROWLESS
      defs DEFS-U ! index INDEX-U ! exit
   then
   index INDEX-BYTES / {: row:n :}
   row 1+ 2 * FILE-ROW-MARKS-RESERVE
   check row 2 * FILE-ROW-MARKS ! entries row 2 * 1+ FILE-ROW-MARKS !
   row 1+ FILE-ORDS-RESERVE
   ord row FILE-ORDS !
   row 1+ FILE-HEADS-RESERVE
   head row FILE-HEADS ! ;

public

\ The finished definition ord joins the innermost open file's object: its
\ sections in DEF-SLOTS order, and one index row with its head's kind
\ (DEF-HEAD) and its source text, the textu bytes from its name's start at
\ at past its body's end in the buffer of the file's run it was read from,
\ empty when its name lay in none, then the premise its head took. Its
\ result digest is bound from that payload viewed as a warm install views
\ it, also when the file is refused, so the digest never depends on whether
\ the file writes an object.
: FILE-ADD-DEFINITION ( n n ptr u8 n -- )
   {: ord:n at:n text:ptr textu:n :}
   FILE-OPEN? 0= if exit then
   NUNIT-CAPTURE:PROFILE-CK
   DEF-FINISHED @ 0= if BAD then
   DEFS-U @ {: defs:n :}
   INDEX-U @ {: index:n :}
   ord FO-FIRST FO @ - dup 0 < if BAD then INDEX-N
   NUNIT-FILE:SECTION-COUNT 0 ?do
      DEF-SLOTS i rshift 1 and 0<> if
         i DEF-SECTION {: a:ptr u:n :}
         a u DEFS+ u INDEX-N
      then
   loop
   textu INDEX-N
   DEF-HEAD @ INDEX-N
   INDEX-U @ 32 + FILE-INDEX-RESERVE
   text textu INDEX-U @ FILE-INDEX SPAN-HASH
   32 INDEX-U +!
   INDEX-U @ 32 + FILE-INDEX-RESERVE
   0 DEF-PREMISE INDEX-U @ FILE-INDEX 32 BYTE-COPY
   32 INDEX-U +!
   DEF-SLOTS index 8 + FILE-INDEX defs FILE-DEFS DEFS-U @ defs -
      NUNIT-FILE:SECTIONS-VIEW
   ord NFILE-REF:BIND-RESULT
   NUNIT-FILE:RESET-KEEP
   ord textu 0= if -1 else at then defs index ROW-KEEP ;

private

\ The index of the innermost file's definition row n among all index rows.
: INDEX-ROW ( n -- n ) {: row:n :}
   FO-INDEX FO @ INDEX-BYTES / row + {: at:n :}
   row 0 < at INDEX-U @ INDEX-BYTES / >= or if BAD then
   at ;

public

: FILE-ORD ( n -- n ) INDEX-ROW FILE-ORDS @ ;

\ The source text length of the innermost file's definition row n.
: FILE-SPAN ( n -- n )
   INDEX-ROW INDEX-BYTES * SPAN-SIZE + FILE-INDEX LE:U64@ ;

private

\ The closing file's segment that allotted all the bytes at va: its start,
\ the bytes' offset in the file's DATA section and its length; 0 -1 0 when
\ none did. A segment's end counts as its own, so a word made after its last
\ allotment still resolves.
: OWN-SEG ( n n -- n n n )
   {: va:n bytes:n :}
   0 EVENTS-N @ FO-EVENTS FO @ ?do
      {: base:n :}
      i SEG? if
         va i 3 EV @ >= va bytes + i 3 EV @ i 4 EV @ + <= and if
            i 3 EV @ base va + i 3 EV @ - i 4 EV @ unloop exit
         then
         base i 4 EV @ +
      else base then
   loop drop 0 -1 0 ;

\ The offset in the closing file's DATA section of the bytes at va, -1 when
\ no segment of the file allotted them all.
: OWN-OFF ( n n -- n ) OWN-SEG drop nip ;

\ A DATA address read as bytes.
CAST: N>U8 ( n -- ptr u8 )

\ Record key as a FILE object reference, 0 after it refuses the closing file:
\ a record the file made, retired or not (the warm run's live top level
\ makes it), or a live record an earlier file made under a declaration key.
: RECORD-TARGET ( n -- n )
   {: key:n :}
   key ndict@ >= if s" checker-ref-retired" s" " s" " REFUSE-KEEP 0 exit then
   key OWN-ROW {: row:n :}
   row 0 >= if row key NFILE-REF:FILE-REF-RECORD exit then
   key XREF-REC XREF-RETIRED? if
      s" checker-ref-retired" s" " key XREF-REC XREF-NAME$ REFUSE-KEEP 0 exit
   then
   \ A record an earlier session made has no key to import: BIND-ORD has
   \ ordered the file on it (CHECK-EXPORT-RUN).
   key NFILE-REF:DECL-KEY? nip nip 0= if
      s" ordered-dependency" s" " key XREF-REC XREF-NAME$ REFUSE-KEEP 0 exit
   then
   row key NFILE-REF:FILE-REF-RECORD ;

\ Event row k as its EVENT-BYTES row. A SEGMENT row holds where in a cell
\ HERE stood at the segment's open: `align` and every definer that aligns
\ its body pad from there, so the DATA bytes hold that remainder's padding.
\ Its cell 6 is 0 and cell 7 its origin sink.
: EVENT-OUT ( n -- )
   {: row:n :}
   row SEG? if
      EV-SEGMENT OUT-N row 1 EV @ FO-FIRST FO @ - OUT-N
      row 2 EV @ OUT-N row 4 EV @ OUT-N row 6 EV @ OUT-N
      row 3 EV @ 1 cells mod OUT-N 0 OUT-N
      row 7 EV @ OUT-N 0 OUT-N 0 OUT-N 0 OUT-N exit
   then
   EV-LOAD OUT-N row 2 EV @ OUT-N row 3 EV @ OUT-N
   row 4 EV @ FO-NAMES FO @ - OUT-N row 5 EV @ OUT-N
   row 6 EV @ OUT-N 0 OUT-N row 8 EV @ OUT-N
   0 OUT-N 0 OUT-N 0 OUT-N ;

\ An address into an earlier file's DATA as a reference through the
\ declaration that owns it, 0 when no keyed declaration does or the offset
\ lies past its extent. The file depends on that declaration's DATA row,
\ whose digest covers the extent, so an edit that moves its end, and with it
\ the record the offset names, is stale.
: EARLIER-REF ( n -- n )
   {: va:n :}
   va AOT-CELL:DATA-ADDRESS? 0= if 0 exit then
   va OWNER-AT {: ord:n base:n :}
   ord NFILE-REF:DECL-KEY? nip nip 0= if 0 exit then
   va base - {: addend:n :}
   ord NUNIT-DEP:DATA-ORD addend <= if 0 exit then
   ord addend 0 1 NFILE-REF:REF-DECL-DATA ;

: WHY-N ( n -- )
   {: value:n :}
   value 10 >= if value 10 / recurse then
   s" 0123456789" drop value 10 mod + 1 WHY+ ;


\ Checker reference entry key bytes as a FILE object reference, 0 after it
\ refuses the closing file: a record it made, retired or not (the warm run's
\ live top level makes it), or a live record an earlier file made, else
\ storage in its own or an earlier declaration's DATA.
: CHECK-TARGET ( n n -- n )
   {: key:n bytes:n :}
   bytes 0 < if key RECORD-TARGET exit then
   key bytes OWN-OFF {: off:n :}
   off 0 >= if NFILE-REF:DATA off 0 NFILE-REF:REF-LOCAL exit then
   key EARLIER-REF {: ref:n :}
   ref 0= if s" checker-ref-unowned" s" " s" " REFUSE-KEEP then
   ref ;

\ The closing file's checker section: its slice count, its reference
\ count, each reference, then its slices.
: CHECK-OUT ( -- )
   FO-CKORD FO @ {: first:n :}
   FO-SLICES FO @ OUT-N
   CKORD-N @ first - OUT-N
   CKORD-N @ first ?do
      i 2 * FILE-CKORD @ i 2 * 1+ FILE-CKORD @ CHECK-TARGET {: ref:n :}
      ref 0= if unloop exit then
      ref OUT-N
   loop
   FO-CHECK FO @ {: from:n :}
   CHECK-U @ from - {: bytes:n :}
   bytes 0<> if from FILE-CHECK bytes OUT-S then ;

public

\ Encode the innermost file's events and checker slices into FILE-OUT,
\ closing its last segment. A file whose checker slices are not one per
\ index row throws; one whose checker references cannot all encode refuses
\ and stops there. A file refused before encoding encodes nothing.
: FILE-ENCODE ( -- )
   FILE-OPEN? 0= if BAD then
   SEGMENT-CLOSE
   FILE-REFUSED? if exit then
   \ One checker slice per index row.
   INDEX-U @ FO-INDEX FO @ - INDEX-BYTES / {: rows:n :}
   FO-SLICES FO @ rows <> if
      s" slice-mismatch slices=" WHY+ FO-SLICES FO @ WHY-N
      s"  rows=" WHY+ rows WHY-N S\" \n" WHY+ BAD
   then
   NFILE-REF:BEGIN-FILE-REFS
   0 OUT-U !
   EVENTS-N @ FO-EVENTS FO @ ?do i EVENT-OUT loop
   OUT-U @ CHECK-AT !
   FILE-REFUSED? if exit then
   CHECK-OUT ;

\ The innermost file's object bytes, after FILE-ENCODE.
: PACK-FILE ( ptr u8 n -- ptr u8 n )
   {: key:ptr keyu:n :}
   FILE-REFUSED? if BAD then
   NUNIT-FILE:RESET-KEEP
   NUNIT-FILE:SECTION-COUNT 0 ?do i NULL-PTR 0 NUNIT-FILE:SECTION! loop
   0 DEFS$ NUNIT-FILE:SECTION!
   2 INDEX$ NUNIT-FILE:SECTION!
   3 NAMES$ NUNIT-FILE:SECTION!
   11 INPUTS$ NUNIT-FILE:SECTION!
   18 ORIGIN$ NUNIT-FILE:SECTION!
   5 CHECK-AT @ OUT-U @ OUT$ NUNIT-FILE:SECTION!
   12 0 CHECK-AT @ OUT$ NUNIT-FILE:SECTION!
   13 0 0 NFILE-REF:ENCODE NUNIT-FILE:SECTION!
   NUNIT-FILE:ARCH-AARCH64 s" FILE" key keyu NUNIT-FILE:PACK ;

private

\ The summed section lengths of one index row, each within room bytes.
: ROW-SIZE ( ptr u8 n -- n )
   {: row:ptr room:n :}
   0 DEF-SECTIONS 0 ?do
      {: used:n :}
      row i 1+ 8 * + LE:U64@ {: len:n :}
      len 0 < len room used - > or if BAD then
      used len +
   loop ;

\ Whether a cut to kept retires what open file k's pending loader action
\ found: its LOAD row, appended when the action finishes, would follow a
\ segment the cut crossed.
: ACT-CUT? ( n n -- bool ) {: kept:n k:n :}
   k FO-ACT FRAME @ 0<> k FO-ACT-ORD FRAME @ kept < 0= and ;

\ Whether a cut to kept drops a row of open file k: its last, as rows follow
\ ordinal order, or the one its pending loader action would append.
: ROWS-CUT? ( n n -- bool ) {: kept:n k:n :}
   kept k ACT-CUT? if true exit then
   k EV-HI {: hi:n :}
   hi k FO-EVENTS FRAME @ > 0= if false exit then
   hi 1- 1 EV @ kept < 0= ;

\ Whether a cut to kept reaches below the innermost file's open segment or
\ drops an event row, such as the LOAD row an open file's pending loader
\ action would append when the cut is its child's first ordinal.
: CUT-BELOW? ( n -- bool ) {: kept:n :}
   kept FO-SEG-ORD FO @ < if true exit then
   FO-DEPTH @ 0 ?do kept i ACT-CUT? if true unloop exit then loop
   EVENTS-N @ 0= if false exit then
   EVENTS-N @ 1- 1 EV @ kept < 0= ;

public

\ The kernel's capture cut notice: the dictionary count drops to kept, which
\ retires every definition at or past it and gives its slot to the next
\ definition. Index and event rows follow ordinal order across the open files,
\ so the retired ones are a suffix; drop it with the retired rows' payloads,
\ checker slices and checker references, and pull back each frame's marks
\ the cut crosses, its first ordinal, slice count and open segment included,
\ after each frame's premise absorbs the records the cut retires (PREMISE-CUT).
\ Only a cut within the innermost file's open segment keeps the objects
\ whole: any other refuses the innermost file and each open file whose rows
\ it drops. A SEGMENT row the cut crosses stays as it was: its file's LOAD
\ row at the segment's end follows it or is pending (LOAD-ACTION), so its
\ file refuses.
: FILE-CUT ( n -- )
   {: kept:n :}
   FILE-OPEN? if kept CUT-BELOW? if
      s" cut-below-segment" s" " s" " REFUSE-KEEP
      FO-DEPTH @ 0 ?do
         kept i ROWS-CUT? if
            s" cut-below-segment" s" " s" " i FRAME-KEEP
         then
      loop
   then then
   FO-DEPTH @ 0 ?do kept i PREMISE-CUT loop
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
      cut 2 * FILE-ROW-MARKS @ CHECK-U !
      cut 2 * 1+ FILE-ROW-MARKS @ CKORD-N !
   then
   0 begin
      dup EVENTS-N @ < if dup 1 EV @ kept < else false then
   while 1+ repeat {: evcut:n :}
   evcut EVENTS-N !
   begin
      PSEG-N @ 0 > if PSEG-N @ 1- 0 PSEG @ evcut < 0= else false then
   while -1 PSEG-N +! repeat
   FO-DEPTH @ 0 ?do
      \ Frame i's rows end where frame i+1's begin; one slice per row.
      i 1+ FO-DEPTH @ < if i 1+ FO-INDEX FRAME @ else rows INDEX-BYTES * then
      i FO-INDEX FRAME @ INDEX-U @ max - INDEX-BYTES / 0 max
      negate i FO-SLICES FRAME +!
      i FO-INDEX FRAME @ INDEX-U @ min i FO-INDEX FRAME !
      i FO-CHECK FRAME @ CHECK-U @ min i FO-CHECK FRAME !
      i FO-CKORD FRAME @ CKORD-N @ min i FO-CKORD FRAME !
      i FO-DEFS FRAME @ DEFS-U @ min i FO-DEFS FRAME !
      i FO-FIRST FRAME @ kept min i FO-FIRST FRAME !
      i FO-EVENTS FRAME @ EVENTS-N @ min i FO-EVENTS FRAME !
      i FO-EV-AT FRAME @ EVENTS-N @ min i FO-EV-AT FRAME !
      i FO-SEGS FRAME @ PSEG-N @ min i FO-SEGS FRAME !
      i FO-SEG-ORD FRAME @ kept min i FO-SEG-ORD FRAME !
      i FO-ACT-ORD FRAME @ kept min i FO-ACT-ORD FRAME !
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
   13 NUNIT-FILE:SECTION$ nip 0= if BAD then
   DEPS-SLOT NUNIT-FILE:SECTION$ {: deps:ptr depsu:n :}
   depsu 0<> if deps depsu NUNIT-DEP:FORMAT drop then ;

: U64-AT ( ptr u8 n -- n ) 8 * + LE:U64@ ;

: SPAN-CK ( n n n -- )
   {: off:n len:n room:n :}
   off 0 < len 0 < or off room > or len room off - > or if BAD then ;

\ ref names one of refs reference rows.
: REF-CK? ( n n -- bool )
   {: ref:n refs:n :}
   ref 0 > ref refs <= and ;

\ A SEGMENT event row starting at or past end, the previous segment's last
\ ordinal, with a cell 6 mark no lower than last, the previous segment's,
\ and a SINK-* origin sink; the new end and mark. The first segment's
\ remainder is the object's.
: SEGMENT-CK ( ptr u8 n n -- n n )
   {: row:ptr end:n last:n :}
   row 1 U64-AT {: first:n :}
   row 2 U64-AT {: count:n :}
   row 3 U64-AT {: bytes:n :}
   row 4 U64-AT {: wids:n :}
   row 5 U64-AT {: rem:n :}
   row 6 U64-AT {: mark:n :}
   row 7 U64-AT {: sink:n :}
   first end < count 0 < or bytes 0 < or wids 0 < or if BAD then
   sink SINK-NONE < sink SINK-FULL > or if BAD then
   rem 0 < rem 1 cells >= or if BAD then
   mark last < if BAD then
   OPEN-SEGS @ 0= if rem OPEN-REM ! then
   count OPEN-RECS +! bytes OPEN-DATA +! wids OPEN-WIDS +! 1 OPEN-SEGS +!
   first count + mark ;

\ The viewed object's SEGMENT row whose span holds ordinal ord past the
\ file's first record, and true; false when ord is none of its own records.
: ORD-SEG ( n -- ptr u8 bool )
   {: ord:n :}
   12 NUNIT-FILE:SECTION$ {: evs:ptr evsu:n :}
   evsu EVENT-BYTES / 0 ?do
      evs i EVENT-BYTES * + {: row:ptr :}
      row 0 U64-AT EV-SEGMENT = if
         ord row 1 U64-AT >= ord row 1 U64-AT row 2 U64-AT + < and if
            row true unloop exit
         then
      then
   loop
   NULL-PTR false ;

: OWN-ORD? ( n -- bool ) ORD-SEG nip ;

\ A LOAD event row: an include or a require, where the loading file's source
\ stood, the requested path in the names section, a resume offset within the
\ file's srcu source bytes, a cell 6 mark, and input bytes within their
\ section. The resume offset, mark and input bytes are no fewer than the
\ previous LOAD row's: one word may load several children at one offset. The
\ input rows since the previous LOAD row are whole rows.
: LOAD-CK ( ptr u8 n n n n n -- n n n )
   {: row:ptr namesu:n srcu:n last-rows:n last-in:n last-at:n :}
   row 1 U64-AT {: kind:n :}
   kind SOURCE-UNIT:LOAD-INCLUDE <> kind SOURCE-UNIT:LOAD-REQUIRE <> and
      if BAD then
   row 2 U64-AT 0 < if BAD then
   row 4 U64-AT 0= if BAD then
   row 3 U64-AT row 4 U64-AT namesu SPAN-CK
   row 5 U64-AT {: resume:n :}
   resume last-at < resume srcu > or if BAD then
   row 6 U64-AT {: rows:n :}
   rows last-rows < if BAD then
   row 7 U64-AT {: inputs:n :}
   row 8 U64-AT row 9 U64-AT or row 10 U64-AT or 0<> if BAD then
   11 NUNIT-FILE:SECTION$ {: ins:ptr insu:n :}
   inputs last-in < inputs insu > or if BAD then
   ins last-in + inputs last-in - NFILE-INPUTS:ROW-COUNT drop
   1 OPEN-LOADS +!
   rows inputs resume ;

: EVENTS-CK ( ptr u8 n n n -- )
   {: evs:ptr evsu:n namesu:n srcu:n :}
   evsu EVENT-BYTES mod 0<> if BAD then
   0 0 0 0 0 evsu EVENT-BYTES / 0 ?do
      {: end:n mark:n rows:n inputs:n at:n :}
      evs i EVENT-BYTES * + {: row:ptr :}
      row 0 U64-AT {: kind:n :}
      kind EV-SEGMENT = if row end mark SEGMENT-CK rows inputs at else
         kind EV-LOAD <> if BAD then
         end mark row namesu srcu rows inputs at LOAD-CK
      then
   loop 2drop 2drop drop ;

\ The reference section: an NFILE-REF ENCODE header with no declarations
\ and room for its reference rows; their count.
: REFS-CK ( ptr u8 n -- n )
   {: refs:ptr refsu:n :}
   refsu 24 < if BAD then
   refs 2 U64-AT 0<> if BAD then
   refs 0 U64-AT {: cnt:n :}
   cnt 0 < cnt refsu 24 - 32 / > or if BAD then
   cnt ;

\ The checker section: one slice per index row of defs, each
\ reference one of refs reference rows, and slices of at least two cells,
\ zero padded to a cell, covering the section exactly.
: CHECKS-CK ( ptr u8 n n n -- )
   {: sec:ptr secu:n refs:n defs:n :}
   secu 16 < if BAD then
   sec 0 U64-AT {: slices:n :}
   sec 1 U64-AT {: nrefs:n :}
   slices defs <> if BAD then
   nrefs 0 < nrefs secu 16 - 8 / > or if BAD then
   nrefs 0 ?do sec i 2 + U64-AT refs REF-CK? 0= if BAD then loop
   nrefs 2 + 8 *
   slices 0 ?do
      {: at:n :}
      secu at - 8 - {: room:n :}
      room 0 < if BAD then
      sec at + LE:U64@ {: len:n :}
      len 16 < len room > or if BAD then
      len 7 + -8 and {: padded:n :}
      padded room > if BAD then
      padded len ?do sec at 8 + i + + c@ 0<> if BAD then loop
      at 8 + padded +
   loop
   secu <> if BAD then
   slices OPEN-CHECKS ! ;

: SPAN-IN? ( n n n -- bool )
   {: off:n u:n size:n :}
   off 0 >= u 0 >= and off size <= and u size off - <= and ;

\ Slot 18's framing: whole rows, each naming one of the file's records whose
\ segment had a sink, a name and path within namesu bytes of slot 3, and an
\ ordered span, zero when the row has no path.
: ORIGIN-FRAMED? ( ptr u8 n n -- bool )
   {: sec:ptr secu:n namesu:n :}
   secu ORIGIN-BYTES mod 0<> if false exit then
   secu ORIGIN-BYTES / 0 ?do
      sec i ORIGIN-BYTES * + {: row:ptr :}
      row 0 U64-AT ORD-SEG {: seg:ptr own:bool :}
      own 0= if unloop false exit then
      seg 7 U64-AT SINK-NONE = if unloop false exit then
      row 1 U64-AT {: at:n :}
      row 2 U64-AT {: end:n :}
      at 0 < at end > or if unloop false exit then
      row 3 U64-AT row 4 U64-AT namesu SPAN-IN? 0= if unloop false exit then
      row 5 U64-AT row 6 U64-AT {: pathoff:n pathu:n :}
      pathoff pathu namesu SPAN-IN? 0= if unloop false exit then
      pathu 0= at end or 0<> and if unloop false exit then
   loop
   true ;

: ORIGIN-CK ( ptr u8 n n -- )
   {: sec:ptr secu:n namesu:n :}
   sec secu namesu ORIGIN-FRAMED? if 1 else 0 then OPEN-ORIGIN-OK ! ;

\ The FILE object's segment framing: events and checker slices agree with
\ one another, given its defs index rows and its srcu source bytes.
: SEGMENTS-CK ( n n -- )
   {: defs:n srcu:n :}
   0 OPEN-RECS ! 0 OPEN-DATA ! 0 OPEN-WIDS !
   0 OPEN-LOADS ! 0 OPEN-SEGS !
   0 OPEN-CHECKS !
   0 OPEN-REM !
   3 NUNIT-FILE:SECTION$ nip {: namesu:n :}
   13 NUNIT-FILE:SECTION$ {: rows:ptr rowsu:n :}
   rows rowsu REFS-CK {: refs:n :}
   12 NUNIT-FILE:SECTION$ namesu srcu EVENTS-CK
   5 NUNIT-FILE:SECTION$ refs defs CHECKS-CK
   18 NUNIT-FILE:SECTION$ namesu ORIGIN-CK ;

\ View the object's FILE sections again; READ-VIEW keeps the caller's bytes.
: FILE-VIEW ( -- )
   OBJ-BYTES @ OBJ-SIZE @ NUNIT-FILE:ARCH-AARCH64 s" FILE"
   OBJ-KEY @ OBJ-KEY-U @ NUNIT-FILE:READ-VIEW ;

\ Capture index at as the installing file's map rows place it, latest row
\ first: the row whose span, cells base and span, holds it, its end too when
\ end is true, gives cell live plus the offset. An index no installed row
\ holds is malformed.
: MAP-THROUGH ( n n n n bool -- n )
   {: at:n base:n span:n live:n end:bool :}
   INS-SEGS @ 0 ?do
      INS-MAP-AT @ INS-SEGS @ + 1- i - {: m:n :}
      at m base INS-CELL @ - {: off:n :}
      m span INS-CELL @ {: u:n :}
      off 0 >= off u < end off u = and or and if
         m live INS-CELL @ off + unloop exit
      then
   loop
   BAD ;

\ A LOCAL reference's kind, item and addend as the live record ordinal,
\ wordlist or DATA address: own record rows, the file's wordlists and its
\ DATA section map through the segments the install made. A DATA offset at
\ one segment's end and the next one's start is the later segment's, the one
\ that installs an address body there.
: MAP-LOCAL ( n n n -- n )
   {: kind:n item:n addend:n :}
   item 0 < addend 0 < or if BAD then
   kind NFILE-REF:DATA = if
      item addend + M-DATA M-BYTES M-LIVE-DATA true MAP-THROUGH exit
   then
   addend 0<> if BAD then
   kind NFILE-REF:RECORD = if item M-ROW M-ROWS M-LIVE-ORD false MAP-THROUGH exit then
   kind NFILE-REF:WID = if item M-WID M-WIDS M-LIVE-WID false MAP-THROUGH exit then
   BAD ;

\ An ordinal past the file's first record, as an index or origin row holds
\ it, as the live ordinal of the record the install made.
: ORD>LIVE ( n -- n ) M-ORD M-ROWS M-LIVE-ORD false MAP-THROUGH ;

\ Make the file's own references the prepared table, resolved through the
\ segment map.
: FILE-REFS ( -- )
   13 NUNIT-FILE:SECTION$ NFILE-REF:PREPARE-FILE ;

\ The checker's import callback: reference idx as a record (bytes DREC) or a DATA
\ span; a LOCAL record is SEGMENT-MAP's slot, made later: XREF-REC reads
\ nothing.
: CHECK-RESOLVE ( n n -- ptr u8 )
   {: idx:n bytes:n :}
   idx 0 <= idx IMPORT-NREFS @ > or if BAD then
   IMPORT-SEC @ idx 1+ U64-AT {: ref:n :}
   ref NFILE-REF:REF-KIND NFILE-REF:RECORD = if
      bytes DREC <> if BAD then
      ref NFILE-REF:REF>RECORD XREF-REC BYTE-VIEW exit
   then
   ref NFILE-REF:REF-OWNER NFILE-REF:LOCAL = if
      ref NFILE-REF:REF>LOCAL-DATA N>U8 exit
   then
   ref bytes NFILE-REF:REF>DATA ;

CAST: CK-PREP-XT ( n -- [ ptr u8 n [ n n -- ptr u8 ] -- n ] )
CAST: CK-INSTALL-XT ( n -- [ n -- ] )

\ The plan PREPARE-SLICE? keeps open for INSTALL-CHECKER; -1 when none is.
variable CHECK-PLAN
-1 CHECK-PLAN !

\ Prepare the slice at IMPORT-OFF: its length and bytes.
: PREPARE-RUN ( -- )
   IMPORT-SEC @ {: sec:ptr :}
   IMPORT-OFF @ {: at:n :}
   sec at + LE:U64@ {: len:n :}
   sec at 8 + + len ['] CHECK-RESOLVE
      CHECKER-OWNER-ABI:PENDING-PREPARE-OFF CHECK-XT CK-PREP-XT execute
   CHECK-PLAN ! ;

\ The checker's install ends the plan whether it installs or throws, and
\ keeps an open mark, so the plan is forgotten before it runs.
: INSTALL-RUN ( -- )
   CHECKER-OWNER-ABI:PENDING-INSTALL-OFF CHECK-XT {: xt:n :}
   CHECK-PLAN @ -1 CHECK-PLAN !
   xt CK-INSTALL-XT execute ;

public

\ End the kept checker plan, which will not be installed; nothing when none
\ is kept. The checker's PENDING-DISCARD drops the checker's mark with it.
: RELEASE-CHECKER ( -- )
   CHECK-PLAN @ 0 < if exit then
   -1 CHECK-PLAN !
   CHECK-DISCARD ;

private

\ Prepare row k's checker slice, kept for INSTALL-CHECKER; false, keeping none,
\ on TFAM:E-UNIT-FAMILY-CHANGED; any other checker refusal is malformed (BAD).
: PREPARE-SLICE? ( n -- bool )
   {: k:n :}
   FILE-REFS
   5 NUNIT-FILE:SECTION$ drop {: sec:ptr :}
   sec IMPORT-SEC ! sec 1 U64-AT IMPORT-NREFS !
   k sec 0 U64-AT >= if BAD then
   \ Past the slice count, the reference count and the references, each
   \ slice is its length and bytes padded to a cell.
   sec 1 U64-AT 2 + 8 * k 0 ?do
      {: at:n :}
      sec at + LE:U64@ 7 + -8 and at + 8 +
   loop IMPORT-OFF !
   ['] PREPARE-RUN catch {: rc:n :}
   rc s" TFAM:E-UNIT-FAMILY-CHANGED" UNIT-CODE = if false exit then
   rc CHECK-CODE? rc s" TFAM:E-REG-AOT-REFUSED" UNIT-CODE = or if BAD then
   rc 0<> if rc throw then
   true ;

\ Install the kept plan on the records the install made, which ends it.
\ A state the checker refuses here is a malformed object.
: INSTALL-CHECKER ( -- )
   CHECK-PLAN @ 0 < if BAD then
   ['] INSTALL-RUN catch {: rc:n :}
   rc CHECK-CODE? if BAD then
   rc 0<> if rc throw then ;

public

: BIND-LOCAL ( -- ) ['] MAP-LOCAL NFILE-REF:LOCAL-AT! ;

private

\ View the payload index row row frames at offset off of the definitions
\ slot defs, defsu bytes; its size. The view replaces the FILE sections.
: PAYLOAD-AT ( ptr u8 ptr u8 n n -- n )
   {: row:ptr defs:ptr defsu:n off:n :}
   row defsu off - ROW-SIZE {: total:n :}
   DEF-SLOTS row 8 + defs off + total NUNIT-FILE:SECTIONS-VIEW
   total ;

\ View definition index row row's payload, the next in the definitions slot:
\ index rows pair with colon records in ordinal order.
: PAYLOAD-VIEW ( n -- )
   {: row:n :}
   row INS-ROW @ <> if BAD then
   2 NUNIT-FILE:SECTION$ drop row INDEX-BYTES * +
   0 NUNIT-FILE:SECTION$ INS-DEFS-OFF @ PAYLOAD-AT
   INS-DEFS-OFF +! 1 INS-ROW +! ;

\ Prepare the viewed payload against the pending record def-open made: its
\ code, literals, calls, addresses and host facts. A payload carries no
\ checker slice; the file's segment imports one.
: PREPARE-DEFINITION-BODY ( -- )
   5 NUNIT-FILE:SECTION$ nip 0<> if BAD then
   DEF-READ-META
   0 NUNIT-FILE:SECTION$ {: source:ptr size:n :}
   size DEF-CODE-U @ <> if BAD then
   size DEF-CODE-RESERVE
   source 0 DEF-CODE size BYTE-COPY
   cp@ DEF-CODE-BASE !
   ['] XREF-REC NFILE-REF:RECORD-AT!
   \ def-open does not advance ndict@: the pending record is the payload's.
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
   1 DEF-PREPARED ! ;

\ Prepare index row row's payload. It was read back when its object was
\ written and the hit check passed, its literal sharing included, so one that
\ does not prepare is malformed.
: PREPARE-FILE-DEFINITION ( n -- )
   {: row:n :}
   DEF-PLAN-ON @ if BAD then
   DEF-RESET
   DEF-ELIGIBLE? 0= if BAD then
   row PAYLOAD-VIEW
   ['] PREPARE-DEFINITION-BODY catch {: rc:n :}
   rc 0<> if DEF-DROP-PLAN drop rc throw then ;

\ Publish the prepared payload's code at the pending record; its ordinal.
: INSTALL-FILE-DEFINITION ( -- n )
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
   DEF-DOES-OFF @ 0= if
      s" NPUB:IMPORT-PENDING" DEF-TARGET DEF-PUBLISH-XT execute exit
   then
   DEF-DOES-OFF @
      s" NPUB:IMPORT-PENDING-DOES" DEF-TARGET DEF-PUBLISH-DOES-XT execute ;

\ The viewed object's event row k and the rows in slot 12.
: OBJ-EV ( n -- ptr u8 ) {: k:n :} 12 NUNIT-FILE:SECTION$ drop k EVENT-BYTES * + ;
: OBJ-EVS ( -- n ) 12 NUNIT-FILE:SECTION$ nip EVENT-BYTES / ;

\ The sums over the viewed object's event rows before row seg: its SEGMENT
\ rows' records, wordlists and DATA bytes, and its LOAD rows.
: SEG-BASES ( n -- n n n n )
   {: seg:n :}
   0 0 0 0 seg 0 ?do
      {: rows:n wids:n bytes:n loads:n :}
      i OBJ-EV {: ev:ptr :}
      ev 0 U64-AT {: kind:n :}
      kind EV-SEGMENT = if
         rows ev 2 U64-AT + wids ev 4 U64-AT + bytes ev 3 U64-AT + loads
      else
         rows wids bytes loads kind EV-LOAD = if 1+ then
      then
   loop ;

\ Add segment row seg's map row, its own records from row rows, its
\ wordlists from wbase and its DATA from dbase among the file's, at the
\ cursors where its install puts them.
: MAP-ROW ( n n n n -- )
   {: seg:n rows:n wbase:n dbase:n :}
   seg OBJ-EV {: ev:ptr :}
   INS-MAP-AT @ INS-SEGS @ + {: m:n :}
   m 1+ MAP-CELLS * INS-MAP-RESERVE
   seg m M-SEG INS-CELL ! rows m M-ROW INS-CELL !
   ev 2 U64-AT m M-ROWS INS-CELL !
   wbase m M-WID INS-CELL ! ev 4 U64-AT m M-WIDS INS-CELL !
   dbase m M-DATA INS-CELL ! ev 3 U64-AT m M-BYTES INS-CELL !
   ev 1 U64-AT m M-ORD INS-CELL !
   ndict@ m M-LIVE-ORD INS-CELL ! AOT-ARM:WIDN m M-LIVE-WID INS-CELL !
   AOT-ARM:HERE-N m M-LIVE-DATA INS-CELL !
   1 INS-SEGS +! ;

\ Plan the viewed payload's literals into the open literal plan as the
\ install does (DEF-READ-LITERALS); PAYLOAD-CK framed their rows.
: PAYLOAD-PLAN ( -- )
   1 NUNIT-FILE:SECTION$ drop 32 + LE:U64@ {: lits:n :}
   10 NUNIT-FILE:SECTION$ drop {: src:ptr :}
   0
   lits 0 ?do
      {: cursor:n :}
      src cursor + LE:U64@ {: bytes:n :}
      src cursor 16 + + bytes DEF-PREP-INTERN drop
      cursor bytes 16 + +
   loop drop ;

\ Frame count index rows: each frames one payload of defs, in increasing
\ ordinal order; the end of the last payload. Each payload's literals are
\ planned into the open plan.
: DEFS-CK ( ptr u8 n ptr u8 n -- n )
   {: defs:ptr defsu:n index:ptr count:n :}
   -1 0
   count 0 ?do
      {: prev:n off:n :}
      index i INDEX-BYTES * + {: row:ptr :}
      row LE:U64@ {: ord:n :}
      ord prev <= if BAD then
      row SPAN-SIZE + LE:U64@ 0 < if BAD then
      row defs defsu off PAYLOAD-AT {: total:n :}
      PAYLOAD-CK
      PAYLOAD-PLAN
      ord off total +
   loop nip ;


\ Plan the file's literals into the live pool, writing nothing: each
\ payload's as DEFS-CK frames them; the end of the last payload. The caller
\ closes the plan.
: POOL-PLAN ( ptr u8 n ptr u8 n -- n )
   {: defs:ptr defsu:n index:ptr count:n :}
   AOT-ARM:HERE-N DEF-PREP-BEGIN
   defs defsu index count DEFS-CK ;

public

\ Check a FILE object's framing: the input rows are well formed, the segment
\ sections agree, and the index rows frame the definitions slot exactly
\ (DEFS-CK), and plans the file's literals (POOL-PLAN). A FILE object with
\ definitions comes only from a capture that had the literal planners
\ (DEF-ELIGIBLE?), so the object write path has them; an engine without them
\ misses at read on E-NUNIT-PROFILE.
: OPEN-FILE ( ptr u8 n ptr u8 n n -- )
   {: bytes:ptr size:n key:ptr keyu:n srcu:n :}
   bytes size NUNIT-FILE:ARCH-AARCH64 s" FILE" key keyu NUNIT-FILE:READ-VIEW
   11 NUNIT-FILE:SECTION$ NFILE-INPUTS:ROW-COUNT OPEN-INPUTS !
   \ SECTIONS-VIEW replaces the table, so keep the FILE spans first.
   0 NUNIT-FILE:SECTION$ {: defs:ptr defsu:n :}
   2 NUNIT-FILE:SECTION$ {: index:ptr indexu:n :}
   indexu INDEX-BYTES mod 0<> if BAD then
   indexu INDEX-BYTES / {: count:n :}
   count srcu SEGMENTS-CK
   count 0= if
      0
   else
      defs defsu index count ['] POOL-PLAN ['] DEF-PREP-CLOSE finally
   then
   defsu <> if BAD then
   count OPEN-DEFS !
   NUNIT-FILE:RESET-KEEP ;

\ What OPEN-FILE found: definitions, input rows, records, DATA bytes, loads,
\ segments and checker slices.
: FILE-COUNTS ( -- n n n n n n n )
   OPEN-DEFS @ OPEN-INPUTS @ OPEN-RECS @ OPEN-DATA @
   OPEN-LOADS @ OPEN-SEGS @ OPEN-CHECKS @ ;

\ Begin installing the FILE object bytes holds under key at the next
\ ordinal, its segment map rows after the installing parent's.
: INSTALL-BEGIN ( ptr u8 n ptr u8 n -- )
   {: bytes:ptr size:n key:ptr keyu:n :}
   bytes OBJ-BYTES ! size OBJ-SIZE ! key OBJ-KEY ! keyu OBJ-KEY-U !
   FILE-VIEW
   INS-MAP-AT @ INS-SEGS @ + INS-MAP-AT ! 0 INS-SEGS !
   0 INS-ROW ! 0 INS-DEFS-OFF ! 0 INS-SLICE ! ;

\ Stop the install: the view released and the file's map rows dropped.
\ INS-MAP-AT stays at the file's first row, where the installing parent's
\ rows end: a sibling's rows start there again, and the parent's own rows
\ stay as INSTALL-RESTORE finds them.
: INSTALL-STOP ( -- )
   NUNIT-FILE:RESET-KEEP
   0 INS-SEGS ! ;

\ The installing file's place, kept across a child load: its next index row
\ and payload offset, its map rows and its next checker slice.
: INSTALL-SAVE ( -- n n n n n )
   INS-ROW @ INS-DEFS-OFF @ INS-MAP-AT @ INS-SEGS @ INS-SLICE @ ;
: INSTALL-RESTORE ( n n n n n -- )
   INS-SLICE ! INS-SEGS ! INS-MAP-AT ! INS-DEFS-OFF ! INS-ROW ! ;

\ View the FILE object bytes holds under key again after a child load, with
\ the counts OPEN-FILE found for it: the object passed OPEN-FILE before.
: VIEW-FILE ( ptr u8 n ptr u8 n -- )
   {: bytes:ptr size:n key:ptr keyu:n :}
   bytes OBJ-BYTES ! size OBJ-SIZE ! key OBJ-KEY ! keyu OBJ-KEY-U !
   FILE-VIEW
   11 NUNIT-FILE:SECTION$ NFILE-INPUTS:ROW-COUNT OPEN-INPUTS !
   2 NUNIT-FILE:SECTION$ nip INDEX-BYTES / OPEN-DEFS !
   0 OPEN-RECS ! 0 OPEN-DATA ! 0 OPEN-WIDS !
   0 OPEN-LOADS ! 0 OPEN-SEGS ! 0 OPEN-REM !
   OBJ-EVS 0 ?do
      i OBJ-EV {: ev:ptr :}
      ev 0 U64-AT {: kind:n :}
      kind EV-SEGMENT = if
         OPEN-SEGS @ 0= if ev 5 U64-AT OPEN-REM ! then
         ev 2 U64-AT OPEN-RECS +! ev 3 U64-AT OPEN-DATA +!
         ev 4 U64-AT OPEN-WIDS +! 1 OPEN-SEGS +!
      else
         kind EV-LOAD = if 1 OPEN-LOADS +! then
      then
   loop
   5 NUNIT-FILE:SECTION$ drop LE:U64@ OPEN-CHECKS ! ;

\ The FILE object OPEN-FILE found has a well formed slot 18.
: ORIGIN-FORMED? ( -- bool ) OPEN-ORIGIN-OK @ 0<> ;

private

\ The viewed object's origin row count and row i.
: ORIGIN-ROWS ( -- n ) 18 NUNIT-FILE:SECTION$ nip ORIGIN-BYTES / ;
: ORIGIN-ROW ( n -- ptr u8 )
   18 NUNIT-FILE:SECTION$ drop swap ORIGIN-BYTES * + ;

\ Hand origin row row to each: its record's live ordinal, name token span,
\ path and name. A located span is placed from head, its definition's live
\ name start.
: ORIGIN-VISIT ( ptr u8 n [ n n n ptr u8 n ptr u8 n -- ] -- )
   {: row:ptr head:n each :}
   3 NUNIT-FILE:SECTION$ drop {: names:ptr :}
   row 6 U64-AT 0<> if head else 0 then {: base:n :}
   row 0 U64-AT ORD>LIVE row 1 U64-AT base + row 2 U64-AT base +
   names row 5 U64-AT + row 6 U64-AT
   names row 3 U64-AT + row 4 U64-AT
   each execute ;

public

\ Index the viewed object's origin rows by the ordinal they hold, each
\ ordinal's rows chained in row order. A child's reuse run indexes its own
\ object, so a file indexes again after each child unit (NDEF-CACHE
\ PLACE-RESTORE).
: ORIGIN-INDEX ( -- )
   0 ORIGIN-ORDS !
   ORIGIN-ROWS {: rows:n :}
   rows 0= if exit then
   0 rows 0 ?do i ORIGIN-ROW 0 U64-AT 1+ max loop {: ords:n :}
   ords ORIGIN-HEAD-RESERVE
   rows ORIGIN-NEXT-RESERVE
   ords 0 ?do 0 i ORIGIN-HEAD ! loop
   rows 0 ?do
      rows 1- i - {: r:n :}
      r ORIGIN-ROW 0 U64-AT {: ord:n :}
      ord ORIGIN-HEAD @ r ORIGIN-NEXT !
      r 1+ ord ORIGIN-HEAD !
   loop
   ords ORIGIN-ORDS ! ;

\ Visit the origin rows of the definition INSTALL-ROW just installed at
\ ordinal ord past the file's first record, its name starting at head in
\ the buffer it was read from, in row order, as compiling it would have
\ recorded them at its end. A row is keyed on its record, never on its name
\ span, which an unlocated row leaves zero.
: ORIGIN-OF ( n n [ n n n ptr u8 n ptr u8 n -- ] -- )
   {: ord:n head:n each :}
   ord ORIGIN-ORDS @ >= if exit then
   ord ORIGIN-HEAD @ begin dup 0<> while
      1- dup ORIGIN-ROW head each ORIGIN-VISIT
      ORIGIN-NEXT @
   repeat drop ;

\ Whether this engine has the record writers OUTER's definers run as a reused
\ file's top level runs again (src/habu/definers.f DEF-RECORD); OBJECT-OPEN
\ refuses an object without them (def-open-absent).
: DEF-OPEN? ( -- bool )
   s" def-open" ENGINE-XT 0<> s" def-close" ENGINE-XT 0<> and
   s" def-create" ENGINE-XT 0<> and
   s" imm-mark" ENGINE-XT 0<> and s" namespace-record" ENGINE-XT 0<> and
   s" prot-wid-add" ENGINE-XT 0<> and s" def-cast" ENGINE-XT 0<> and
   s" does-patch" ENGINE-XT 0<> and s" wide-mark" ENGINE-XT 0<> and
   s" min-in-mark" ENGINE-XT 0<> and s" rep-mark" ENGINE-XT 0<> and
   s" def-defer" ENGINE-XT 0<> and
   s" wordlist" ENGINE-XT 0<> and s" namespace-private" ENGINE-XT 0<> and ;

\ ---- reuse: a definition installed where its source reaches its head -------
\ A warm load runs the source of a file whose bytes are unchanged again. At
\ each `:` or TRUSTED: head the live text from the name's start is matched
\ against the viewed object's next index row by head kind, length and digest;
\ a matching row's payload and its checker slice install at the pending record
\ the head opened. A head with no row (BUSY, ROWLESS, a definition a cut
\ retired) matches none and compiles live. A head whose text or kind an
\ earlier file's word changed misses and compiles live; later heads are
\ compared with that row, so they miss unless one repeats its text. A
\ matching row with no dependency rows (DEF-SECTION), or met while the
\ compiler would not let its definition stand alone, is a live head: it
\ compiles live and the object stays kept.

: INDEX-ROW@ ( n -- ptr u8 ) 2 NUNIT-FILE:SECTION$ drop swap INDEX-BYTES * + ;

\ Pass the next index row: its definition compiles live.
: PASS-ROW ( -- )
   INS-ROW @ INDEX-ROW@ {: row:ptr :}
   row 0 NUNIT-FILE:SECTION$ nip INS-DEFS-OFF @ - ROW-SIZE INS-DEFS-OFF +!
   1 INS-ROW +! ;

\ Index row r holds the live head's kind and its source text is the head
\ text, room bytes long at most.
: SEEK-MATCH? ( n ptr u8 n -- bool )
   {: r:n text:ptr room:n :}
   r INDEX-ROW@ {: row:ptr :}
   row SPAN-HEAD + LE:U64@ HEAD-KIND <> if false exit then
   row SPAN-SIZE + LE:U64@ {: len:n :}
   len 0= len room > or if false exit then
   32 SEEK-HASH-RESERVE
   text len 0 SEEK-HASH SPAN-HASH
   0 SEEK-HASH 32 row SPAN-DIGEST + 32 STR= ;

\ Under the cache diagnostics, the line labelled label naming the live head
\ text, room bytes long at most: the name is its first token.
: HEAD-LINE ( ptr u8 n ptr u8 n -- )
   {: label:ptr labelu:n text:ptr room:n :}
   NUNIT-DEP:DIAG? @ 0= if exit then
   0 begin dup room < if text over + c@ STR-SPACE > else false then while 1+ repeat
   {: nameu:n :}
   s" native-file: " NUNIT-DEP:DIAG-W2 label labelu NUNIT-DEP:DIAG-W2
   s"  name=" NUNIT-DEP:DIAG-W2 text nameu NUNIT-DEP:DIAG-W2
   data-base SRCLOC:PATHLEN-CELL + @ {: pathu:n :}
   pathu 0 > if
      s"  path=" NUNIT-DEP:DIAG-W2 DEF-DIAG-PATH @ pathu NUNIT-DEP:DIAG-W2
   then
   S\" \n" NUNIT-DEP:DIAG-W2 ;

private

\ Pass the matched row of the live head whose text is text, room bytes long
\ at most: it compiles live (HEAD-SEEK).
: LIVE-HEAD ( ptr u8 n -- n bool bool )
   {: text:ptr room:n :}
   s" live-head" text room HEAD-LINE
   PASS-ROW 0 false true ;

\ Whether index row r's premise is the open file's at the live head.
: PREMISE-HELD? ( n -- bool )
   {: r:n :}
   32 SEEK-HASH-RESERVE
   0 SEEK-HASH PREMISE
   0 SEEK-HASH 32 r INDEX-ROW@ SPAN-PREMISE + 32 STR= ;

public

\ Whether the viewed payload's literal rows, planned into the pool as it is
\ now, share addresses as the capture's did. The code holds that sharing:
\ elaborate.f EMIT-KIND-LIT-OWNED gives literals at one address one value.
: SHARING-HELD? ( -- bool )
   1 NUNIT-FILE:SECTION$ drop 32 + LE:U64@ DEF-LIT-N !
   AOT-ARM:HERE-N DEF-PREP-BEGIN
   ['] DEF-PLAN-LITERALS? ['] DEF-PREP-CLOSE finally
   0 DEF-LIT-N ! 0 DEF-LIT-U ! ;

\ Whether the next index row's source text is the one the buffer being read
\ holds at at, size bytes long from src. A hit answers its text's length and
\ true false. A matching row with no dependency rows, or whose head the
\ compiler would not now let stand alone (DEF-CACHEABLE-NOW?), is a live head,
\ and one whose segment recorded another origin sink than kind, the live one,
\ or whose dependency rows, literal sharing or premise no longer hold is a
\ miss: each is passed, and answers 0 false true or 0 false false. A head the
\ next row does not hold is a miss too, passes none and answers 0 false false.
\ A definition the source compiled live since the last head captured through
\ NUNIT-FILE and released the file's view, so the file is viewed again
\ first.
: HEAD-SEEK ( ptr u8 n n n -- n bool bool )
   {: src:ptr size:n at:n kind:n :}
   FILE-VIEW
   INS-ROW @ {: row:n :}
   src at + {: text:ptr :}
   size at - {: room:n :}
   row OPEN-DEFS @ >= if 0 false false exit then
   row text room SEEK-MATCH? 0= if 0 false false exit then
   DEF-CACHEABLE-NOW? 0= if text room LIVE-HEAD exit then
   row INDEX-ROW@ 0 NUNIT-FILE:SECTION$ INS-DEFS-OFF @ PAYLOAD-AT drop
   DEPS-SLOT NUNIT-FILE:SECTION$ {: deps:ptr depsu:n :}
   depsu 0<> if deps depsu NUNIT-DEP:CHECK else false then
   dup if drop SHARING-HELD? then {: held:bool :}
   FILE-VIEW
   depsu 0= if text room LIVE-HEAD exit then
   row INDEX-ROW@ LE:U64@ ORD-SEG {: seg:ptr own:bool :}
   own if seg 7 U64-AT kind = else false then 0= if
      s" origin-sink" text room HEAD-LINE
      PASS-ROW 0 false false exit
   then
   held 0= if
      s" stale-head" text room HEAD-LINE
      PASS-ROW 0 false false exit
   then
   row PREMISE-HELD? 0= if
      s" premise" text room HEAD-LINE
      PASS-ROW 0 false false exit
   then
   row INDEX-ROW@ SPAN-SIZE + LE:U64@ true false ;

\ Mark the checker and prepare the slice of HEAD-SEEK's hit; false misses it.
\ Runs after LOAD-PASSED?: rows past a LOAD row map at NDEF-CACHE LOAD-END.
: HEAD-PREPARE? ( ptr u8 n -- bool )
   {: text:ptr room:n :}
   CHECK-MARK
   INS-ROW @ PREPARE-SLICE? if true exit then
   CHECK-DROP
   s" stale-slice" text room HEAD-LINE
   PASS-ROW false ;

\ Install the next row at the pending record, then HEAD-PREPARE?'s plan over the
\ file's view again; the live ordinal and the ordinal past the file's first.
: INSTALL-ROW ( -- n n )
   INS-ROW @ {: row:n :}
   NFILE-REF:PENDING-KEY$ 2drop
   FILE-VIEW
   row PREPARE-FILE-DEFINITION
   INSTALL-FILE-DEFINITION {: ord:n :}
   ord NFILE-REF:PUBLISH-PREPARED
   ord NFILE-REF:BIND-RESULT
   DEF-DROP-PLAN dup 0<> if throw then drop
   FILE-VIEW
   INSTALL-CHECKER
   ord row INDEX-ROW@ LE:U64@ ;

\ The definition INSTALL-ROW just installed at live ordinal ord from the
\ viewed object's index row before INS-ROW, its name starting at at, joins
\ the innermost open file's object. Its sections, source text length, head
\ kind, digest and premise are the viewed row's bytes the install resolved,
\ copied as they are (HEAD-SEEK matched the premise); only its ordinal past
\ the file's first is live. The install bound its result.
: FILE-ADD-INSTALLED ( n n -- )
   {: ord:n at:n :}
   FILE-OPEN? 0= if exit then
   INS-ROW @ 1- INDEX-ROW@ {: old:ptr :}
   \ PAYLOAD-VIEW advanced INS-DEFS-OFF past the row's payload.
   old INS-DEFS-OFF @ ROW-SIZE {: total:n :}
   DEFS-U @ {: defs:n :}
   INDEX-U @ {: index:n :}
   ord FO-FIRST FO @ - dup 0 < if BAD then INDEX-N
   0 NUNIT-FILE:SECTION$ drop INS-DEFS-OFF @ total - + total DEFS+
   INDEX-BYTES 8 - {: rest:n :}
   INDEX-U @ rest + FILE-INDEX-RESERVE
   old 8 + INDEX-U @ FILE-INDEX rest BYTE-COPY
   rest INDEX-U +!
   ord at defs index ROW-KEEP ;

\ Map event row seg, when it is a SEGMENT, at the present cursors, where the
\ source's run makes its records.
: SEGMENT-MAP ( n -- )
   {: seg:n :}
   seg OBJ-EVS >= if exit then
   seg OBJ-EV 0 U64-AT EV-SEGMENT <> if exit then
   seg SEG-BASES drop {: rows:n wbase:n dbase:n :}
   seg rows wbase dbase MAP-ROW ;

\ The event row of the viewed object's LOAD row k, counted from 0, or OBJ-EVS
\ when it has fewer.
: LOAD-EV ( n -- n )
   {: k:n :}
   0 OBJ-EVS 0 ?do
      i OBJ-EV 0 U64-AT EV-LOAD = if
         dup k = if drop i unloop exit then
         1+
      then
   loop drop OBJ-EVS ;

\ The event row after the viewed object's LOAD row k, counted from 0.
: EV-AFTER-LOAD ( n -- n ) LOAD-EV 1+ OBJ-EVS min ;

\ Whether the viewed object's LOAD row k, counted from 0, is a load of load
\ kind kind and requested path resuming at source offset off: the load a
\ warm run made is the one its object holds there.
: LOAD-HELD? ( n n ptr u8 n n -- bool )
   {: k:n kind:n path:ptr pathu:n off:n :}
   k LOAD-EV {: e:n :}
   e OBJ-EVS >= if false exit then
   e OBJ-EV {: ev:ptr :}
   ev 1 U64-AT kind <> ev 5 U64-AT off <> or if false exit then
   3 NUNIT-FILE:SECTION$ drop ev 3 U64-AT + ev 4 U64-AT path pathu STR= ;

\ Whether the index row HEAD-SEEK just matched (INS-ROW) lies in a segment
\ after the viewed object's LOAD row k, counted from 0: a warm run that
\ reaches that row without the load has passed it, from its file's source
\ or from a text its run evaluates alike.
: LOAD-PASSED? ( n -- bool )
   {: k:n :}
   k LOAD-EV {: e:n :}
   e OBJ-EVS >= if false exit then
   INS-ROW @ INDEX-ROW@ LE:U64@ {: ord:n :}
   OBJ-EVS e 1+ ?do
      i OBJ-EV {: ev:ptr :}
      ev 0 U64-AT EV-SEGMENT = if ord ev 1 U64-AT >= unloop exit then
   loop false ;

;package
