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
require src/habu/sites.f
require src/habu/address-carrier.f
require src/habu/address-cells.f
require src/habu/aot-data-cell.f
require src/habu/aot-arm.f
require src/compiler/native/a64ir.f
require src/compiler/native/branch.f
require src/compiler/native/dict.f
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
\ EVENT-BYTES row per segment and per child load, in load order, each
\ segment row followed by the FFI registry rows its code declared. Over the
\ segments in order, slot 1 holds one RECORD-BYTES row per record, slot 6 the
\ DATA they allotted, slot 8 their require rows (length, then path) and slot
\ 4 one byte per wordlist they made, 1 when it is protected. Slot 3 holds the
\ names record and load rows point at, slot 13 the references record and slot
\ rows name, in NFILE-REF ENCODE format with no declarations, and slot 16 the
\ namespace at the file's end. Slot 9 holds one SLOT-BYTES row per declared
\ address cell the segments declared, in registration order, and slot 10 the literal bodies slot
\ rows point into, in definition literal framing; a slot cell in the file's
\ DATA is zero in slot 6. A write to an earlier file's address cell is a slot
\ row too, and slot 7 holds one UPDATE-BYTES row per other earlier DATA byte
\ the segments changed outside the storage owners' spans. Slot 15 holds one
\ PIN-BYTES row per owner of a DATA address a stored cell holds raw: the
\ owner's reference, the closing file's segment or an earlier declaration,
\ then its DATA base at capture. Slot 5 holds the checker state the file
\ added, one slice before each child load and one at its end: the slice
\ count, the count of slot 13 references the slices name, those references,
\ then each slice's length and bytes padded to a cell. Slot 17 holds the body
\ of each constant record row, in row order: its recorded code length, then
\ the code bytes its load emitted. Slot 18 holds the origin sink the file
\ loaded under, then one ORIGIN-BYTES row per definition the sink recorded,
\ in the order its segments recorded them: its ordinal past the file's
\ first record, its name token's span, and the
\ name's and path's offsets and lengths in slot 3, a zero span and path when
\ the row is unlocated. Slot 19 holds the output operations the file's top
\ level issued, segment after segment, one record each: its kind (OUT-EMIT or
\ OUT-WRITE) and byte length, then its bytes, an emit's character as one
\ cell. A segment that issued one adds one OUT row after its SEGMENT and FFI
\ rows, also when it adds no SEGMENT row. A definition's own dependency slot
\ stays out: the file's rows cover it.
42031 constant DEF-SLOTS
14 constant DEPS-SLOT
\ An index row's name span: after the ordinal and the eight section lengths.
72 constant SPAN-AT
80 constant SPAN-END
88 constant INDEX-BYTES
\ A record row: kind, flags, name offset and length in slot 3, wordlist
\ reference, then by kind the body's offset in slot 17, DATA reference, index
\ row or public wordlist reference, and a package's private wordlist
\ reference.
56 constant RECORD-BYTES
\ An event row. SEGMENT: kind, first ordinal past the file's first record,
\ records, DATA bytes, requires, wordlists, where in a cell HERE stood at its
\ open, and the file's dependency row count at its close. LOAD: kind, load
\ kind, source offset in the loading file, path offset and length, root
\ offset and length, 1 when the load was clean, the source offset the
\ loading file resumes at, and the file's dependency row count and input
\ bytes when the child opened. A clean load is a loader action that started
\ at a loader cut and returned with the loading file balanced at its resume
\ offset; a file with any other load stores no object. FFI-LIB: kind, path
\ offset and length, the library's index. FFI-FN: kind, symbol offset and
\ length, library index, argument and spill counts, the function's index.
\ FFI-SCOPE: kind, role (0 the declarer's selection when the segment opened,
\ 1 when it closed), library index, the scope wordlist's reference and 1
\ when a scope was stated. TASK: kind, TASK's user-slot watermark at the
\ segment's open and at its close. OUT: kind, the bytes' offset in slot 19
\ and their length. Unused cells are 0.
88 constant EVENT-BYTES
\ An origin row: ordinal, start, end, name offset and length, path offset and
\ length.
56 constant ORIGIN-BYTES

public

\ Where a file's origin rows went as it loaded: no sink, the builder's prefix
\ rows, the early journal, the full journal.
0 constant SINK-NONE
1 constant SINK-PREFIX
2 constant SINK-EARLY
3 constant SINK-FULL

private

\ A slot row: the cell's DATA reference, its kind, its value's reference,
\ 0 for a null value, and the slot 12 row of the segment that declared the
\ cell, NO-SEGMENT for a cell an earlier file declared.
32 constant SLOT-BYTES
-1 constant NO-SEGMENT
0 constant SLOT-XT
1 constant SLOT-DATA
\ An update row: the changed byte's reference and its value at the close.
16 constant UPDATE-BYTES
\ Record row kinds; 1 to 3 are the fixed kinds: a constant, an address, a
\ deferred word. RK-RETIRED, RK-CODELESS and RK-CAST have no body of their
\ own: a record retired in place, one with no code, and a cast, whose code is
\ the engine's empty checked body. A does> definer's clause row follows its
\ colon row, and a word a does> definer made is an address patched to run
\ that clause. No row holds RK-NONE, the kind of a record that refuses.
0 constant RK-NONE
1 constant RK-VAL
2 constant RK-ADDR
3 constant RK-DEFER
4 constant RK-COLON
5 constant RK-PACKAGE
6 constant RK-RETIRED
7 constant RK-CODELESS
8 constant RK-CAST
9 constant RK-CLAUSE
10 constant RK-DOES-WORD

public

\ An event row's kind: a segment of the file's own records, a child load, or
\ the output a segment's top level wrote.
1 constant EV-SEGMENT
2 constant EV-LOAD
7 constant EV-OUT

private

\ After a SEGMENT row, in order: the declarer's selection at the segment's
\ open, one row per library or function
\ declaration it made, reused rows included, and the selection at its close
\ when that differs.
3 constant EV-FFI-LIB
4 constant EV-FFI-FN
5 constant EV-FFI-SCOPE
\ Last of the rows after a SEGMENT row whose segment moved TASK's user-slot
\ watermark, before its OUT row: the watermark at its open, which the slot
\ offsets it baked assume, and at its close, which a replay leaves. A
\ segment that leaves it in place has none, so a parent is not charged with
\ the slots its children took.
6 constant EV-TASK
\ One frame per open file: the buffers' used sizes when it opened, its first
\ record ordinal, the open segment's marks (first ordinal, HERE, require
\ count, wordlist counter, address cell rows), the require counter it opened
\ with, the address cell row before the open segment's first, the bytes its
\ segments' before-images copied, the earlier DATA sites recorded when it
\ opened, the checker slice bytes and reference entries when it opened, its
\ checker slice count, 1 while a checker mark is open, the open segment's
\ FFI registry marks: the selected bank's next function index, the tables'
\ room and the declarer's selection (library, scope wordlist, 1 when a scope
\ was stated), the origin bytes when it opened, its sink cell first, the
\ segment DATA bytes and the DATA floor (POOL-FLOOR) when it opened, the
\ output bytes when it and its open segment opened, the LOAD row its
\ pending loader action claimed (ACT-NONE, ACT-OPEN), and the open
\ segment's TASK user-slot watermark with 1 when TASK was bound to read it.
\ A child file's bytes and sites follow its parent's and are dropped when
\ the child closes.
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
12 constant FO-SEG-SLOT
13 constant FO-SEG-PREV
14 constant FO-IMAGE
15 constant FO-SITES
16 constant FO-CHECK
17 constant FO-CKORD
18 constant FO-SLICES
19 constant FO-CKMARK
20 constant FO-SEG-FN
21 constant FO-SEG-ROOM
22 constant FO-SEG-SEL-LIB
23 constant FO-SEG-SEL-WID
24 constant FO-SEG-SEL-SET
25 constant FO-ORIGIN
26 constant FO-SEGB
27 constant FO-SEG-POOL
28 constant FO-OUTPUT
29 constant FO-SEG-OUT
30 constant FO-ACT-ROW
31 constant FO-SEG-USER
32 constant FO-SEG-USER-BOUND
33 constant FO-CELLS
\ FO-ACT-ROW with no loader action of the file's own token pending, and with
\ one pending that has loaded no child yet; any other value is the LOAD row
\ its child opened.
-2 constant ACT-NONE
-1 constant ACT-OPEN
\ An event row in memory, cell 1 an ordinal in both kinds so a dictionary
\ cut finds the rows it retired. SEGMENT: kind, first ordinal, records, HERE,
\ DATA bytes, first require, requires, first wordlist, wordlists, first
\ address cell row, rows, the first and last rows' values, the file's
\ dependency row count at its close and its DATA bytes' offset in SEG-DATA.
\ LOAD: kind, the child's first ordinal,
\ load kind, source offset, path offset and length, root offset and length
\ in FILE-NAMES, 0, 1 once the loader action finished clean, the resume
\ offset, the file's dependency row count and input bytes at the child's
\ open, then zeros. OUT: kind, the segment's first ordinal, the bytes'
\ offset in FILE-OUTPUT and their length, then zeros.
15 constant EV-CELLS
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
\ The open files' slot 19 bytes back to back.
DYNAMIC-BUFFER FILE-OUTPUT u8
variable OUTPUT-U
\ The closing file's record, DATA, require, protection and event sections,
\ back to back.
DYNAMIC-BUFFER FILE-OUT u8
\ Each closed segment's DATA bytes as its close found them, innermost file
\ last: a child or a later segment writes an earlier segment's DATA after it
\ closes, and the object carries the bytes the segment left.
DYNAMIC-BUFFER SEG-DATA u8
variable SEG-DATA-U
\ Each index row's dictionary ordinal, so a dictionary cut can find the rows
\ it retired. Never written to an object.
DYNAMIC-BUFFER FILE-ORDS n
\ The closing file's slot literal rows, and each row's body address.
DYNAMIC-BUFFER FILE-LITS u8
DYNAMIC-BUFFER FILE-LIT-ADDRS n
\ The closing file's constant bodies, slot 17, and the count of relocation
\ sites in the body being stored.
DYNAMIC-BUFFER FILE-VALS u8
variable VALS-U
variable VAL-SITES
\ The open files' checker slices, each its length, then its bytes padded to
\ a cell, and the references they name: two cells per entry, a record's
\ ordinal and -1, or a DATA address and its byte count.
DYNAMIC-BUFFER FILE-CHECK u8
DYNAMIC-BUFFER FILE-CKORD n
variable CHECK-U
variable CKORD-N
variable CHECK-AT
variable OPEN-CHECKS
variable LITS-U
variable LITS-N
variable SLOTS-AT
variable LITS-AT
variable OPEN-SLOTS
\ The target's NSTR:OWNER-ROW while the closing file encodes, 0 when absent.
variable OWNER-ROW-XT
\ The before-image of earlier DATA: DATA from IMAGE-FLOOR to HERE when the
\ open segment's image was taken. Taken once the segment opens
\ (SEGMENT-IMAGE), only while a file compiles, and compared at the
\ segment's close. The floor belongs to the process, not the session: HERE
\ at the process's first image, so a top-level load sees its writes to every
\ earlier load's DATA. Every file frame takes its first image as it opens
\ (NDEF-CACHE FILE-UNIT-WITH), before its source runs or its object installs,
\ so a warm build takes the cold build's floor. Below it lie the kernel's and
\ the tools' cells, which the tools write while a file compiles; only the
\ cells KCELL names are observed there. Any other cell below the floor that
\ a file changes through a trusted or foreign word is not observed, and its
\ object is stored without that effect. The floor is a process cell
\ (PROCESS-CELL below): an image stores it as 0, so the process booted from it
\ takes its own floor at its first image.
DYNAMIC-BUFFER IMAGE u8
variable IMAGE-FLOOR
CAST: PROCESS-CELL-XT ( n -- [ ptr n -- ] )
\ Set when this engine's registry declares no process cells.
variable PROCESS-ABSENT
variable IMAGE-U
\ The kernel cells below the floor as the open segment's image found them,
\ and whether it took them.
4 constant KCELLS
KCELLS TYPED-BUFFER KCELL-WAS n
variable KCELLS-HELD
\ The earlier DATA bytes the open files' segments changed, each once per
\ file and ascending within it, innermost file last. Encoding drops address
\ cells and the storage owners' spans, and keeps the rest as updates.
DYNAMIC-BUFFER FILE-SITES n
variable SITES-N
\ The address cell rows the closing file's earlier-DATA writes emitted.
DYNAMIC-BUFFER FILE-SLOTW n
variable SLOTW-N
\ The storage owners' spans while the closing file encodes, base and end.
DYNAMIC-BUFFER OWNER-SPANS n
variable OWNER-N
\ Each of the closing file's sites' owning record and that record's DATA
\ start, two cells per site.
DYNAMIC-BUFFER SITE-OWNS n
variable UPDS-AT
variable OPEN-UPDS
16 constant PIN-BYTES
\ Each pinned owner while the closing file encodes: its DATA base and its
\ record, -1 for one of the file's own segments.
DYNAMIC-BUFFER PIN-OWNS n
variable PINS-N
variable PINS-AT
variable OPEN-PINS
\ The open segment's FFI declarations, reused or appended, and its
\ declarers' selection writes, as the registry's observer reports them: kind
\ and index pairs in order. Segments never nest (FILE-BEGIN closes the
\ parent's before the child's opens), so one journal serves. The observer is
\ set exactly while a segment is open.
DYNAMIC-BUFFER FFI-OBS n
variable OBS-N
\ FFI:OBSERVE's kinds: a library, a function, a selection write (index 0).
1 constant OBS-LIB
2 constant OBS-FN
3 constant OBS-SCOPE
variable OBS-ON
variable OPEN-FFI
variable OPEN-OUT
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
\ Where in a cell HERE stood when the open object's segment opened.
variable OPEN-REM
variable OPEN-REQS
variable OPEN-WIDS
variable OPEN-LOADS
variable OPEN-SEGS
\ One bit per record kind the open FILE object's rows hold.
variable OPEN-KINDS
\ The lowest DATA offset the next address record row may name, and the end of
\ its segment's DATA.
variable OPEN-ADDR
variable OPEN-SEG-END
\ Whether the live literal pool takes every literal of the open FILE
\ object's colon bodies without opening a segment.
variable OPEN-POOL-ROOM
\ The file's wordlists its package rows own, in the order they made them, -1
\ once a row owns others.
variable OPEN-OWNED
\ The end of the slot 17 entries the open FILE object's rows have framed.
variable OPEN-VALS
\ The open FILE object's origin sink kind, and whether its slot 18 is well
\ formed: a malformed one is a miss the hit check names.
variable OPEN-SINK
variable OPEN-ORIGIN-OK
\ The object being installed and its key, the file's first record, and the
\ next definition index row and its payload's offset in the definitions slot.
PTR-VARIABLE OBJ-BYTES
variable OBJ-SIZE
PTR-VARIABLE OBJ-KEY
variable OBJ-KEY-U
variable INS-FIRST
variable INS-ROW
variable INS-DEFS-OFF
\ The installing segment's DATA address, its byte count and how many of
\ them are filled.
variable INS-DATA
variable INS-DATA-U
variable INS-DATA-AT
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
CAST: REQ-COUNT-XT ( n -- [ -- n ] )
CAST: DEF-ACTION-XT ( n -- [ -- ] )
CAST: DEF-NUM-XT ( n -- [ -- n ] )
CAST: FFI-OBSERVE-XT ( n -- [ [ n n -- ] -- ] )
CAST: FFI-SELECTION-XT ( n -- [ -- n n bool ] )
CAST: FFI-LIB-ROW-XT ( n -- [ n -- ptr u8 n ] )
CAST: FFI-FN-ROW-XT ( n -- [ n -- ptr u8 n n n n ] )
CAST: FFI-PATH-XT ( n -- [ ptr u8 n -- n ] )
CAST: FFI-FN-XT ( n -- [ ptr u8 n n n n -- n ] )
CAST: FFI-SELECT-XT ( n -- [ n n bool -- ] )
CAST: REQ-LEN-XT ( n -- [ n -- n ] )
CAST: REQ-SLOT-XT ( n -- [ n -- ptr u8 ] )
CAST: USER-NEXT-XT ( n -- [ [ ptr u8 -- ] -- ] )

\ Field n of file frame k, outermost first.
: FRAME ( n n -- ptr n ) swap FO-CELLS * + FILE-FRAMES ;

\ Field n of the innermost file frame.
: FO ( n -- ptr n )
   FO-DEPTH @ 0 <= if BAD then
   FO-DEPTH @ 1- swap FRAME ;

\ Field n of event row k.
: EV ( n n -- ptr n ) swap EV-CELLS * + FILE-EVENTS ;

\ Field n of segment map row m, and of the installing segment's row.
: INS-CELL ( n n -- ptr n ) swap MAP-CELLS * + INS-MAP ;
: CUR@ ( n -- n ) {: c:n :} INS-MAP-AT @ INS-SEGS @ + 1- c INS-CELL @ ;

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
: FRAME-WHY+ ( ptr u8 n n -- )
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
: WHY+ ( ptr u8 n -- ) FO-DEPTH @ 1- FRAME-WHY+ ;
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
   0 row 9 EV ! 0 row 10 EV ! 0 row 11 EV ! 0 row 12 EV ! 0 row 13 EV !
   0 row 14 EV !
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
   first FILE-ORIGIN ORIGIN-U @ first - ;
: OUTPUT$ ( -- ptr u8 n )
   FO-OUTPUT FO @ {: first:n :}
   OUTPUT-U @ first = if NULL-PTR 0 exit then
   first FILE-OUTPUT OUTPUT-U @ first - ;
: ORIGIN-N ( n -- )
   {: value:n :}
   ORIGIN-U @ 8 + FILE-ORIGIN-RESERVE
   value ORIGIN-U @ FILE-ORIGIN LE:U64!
   8 ORIGIN-U +! ;
: VALS$ ( -- ptr u8 n )
   VALS-U @ 0= if NULL-PTR 0 exit then
   0 FILE-VALS VALS-U @ ;
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

\ Whether this engine's FFI registry reports its declarations. The observer
\ decides for every FFI producer: they land together.
: FFI-BOUND? ( -- bool ) s" FFI:OBSERVE" NUNIT-CAPTURE:OPTIONAL-PRODUCER-XT 0<> ;
: FFI-XT ( ptr u8 n -- n ) NUNIT-CAPTURE:BOUND-PRODUCER-XT ;
: FFI-N ( ptr u8 n -- n ) FFI-XT DEF-NUM-XT execute ;
: FFI-SELECTION ( -- n n bool ) s" FFI-DECL:SELECTION" FFI-XT FFI-SELECTION-XT execute ;

\ TASK hands its user-slot watermark cell to a claimant: USER-MARK reads it,
\ and whether TASK is bound to; INSTALL-TASK stores USER-PUT through it.
variable USER-SEEN
variable USER-PUT
: USER-SEEN! ( ptr u8 -- ) CELL-VIEW @ USER-SEEN ! ;
: USER-PUT! ( ptr u8 -- ) CELL-VIEW USER-PUT @ swap ! ;
: USER-XT ( -- n ) s" TASK:OWNED-USER-NEXT" NUNIT-CAPTURE:OPTIONAL-PRODUCER-XT ;
: USER-MARK ( -- n bool )
   USER-XT dup 0= if drop 0 false exit then
   ['] USER-SEEN! swap USER-NEXT-XT execute
   USER-SEEN @ true ;

\ The registry's observer: journal one declaration of the open segment.
: OBS+ ( n n -- )
   {: kind:n idx:n :}
   OBS-N @ 1+ 2 * FFI-OBS-RESERVE
   kind OBS-N @ 2 * FFI-OBS ! idx OBS-N @ 2 * 1+ FFI-OBS !
   1 OBS-N +! ;
: OBS-SET ( -- ) ['] OBS+ s" FFI:OBSERVE" FFI-XT FFI-OBSERVE-XT execute 1 OBS-ON ! ;
\ OBS-ON guards it: before the producers bind, FFI-BOUND? would throw.
: OBS-CLEAR ( -- )
   OBS-ON @ 0= if exit then
   s" FFI:UNOBSERVE" FFI-XT DEF-ACTION-XT execute 0 OBS-ON ! ;

\ Whether open file k holds a refusal reason.
: FRAME-REFUSED? ( n -- bool )
   {: k:n :}
   k WHY-END k FO-WHY FRAME @ > ;

\ Open file k keeps its first refusal reason; it writes no object.
: FRAME-REFUSE ( ptr u8 n n -- )
   {: reason:ptr reasonu:n k:n :}
   k FRAME-REFUSED? if exit then
   reason reasonu k FRAME-WHY+ ;

public

: FILE-OPEN? ( -- bool ) FO-DEPTH @ 0 > ;
: FILE-REFUSED? ( -- bool ) FO-DEPTH @ 1- FRAME-REFUSED? ;

\ The innermost file keeps its first refusal reason; it writes no object.
: FILE-REFUSE ( ptr u8 n -- )
   FILE-OPEN? 0= if 2drop exit then
   FO-DEPTH @ 1- FRAME-REFUSE ;

\ Every open file refuses.
: REFUSE-ALL ( ptr u8 n -- )
   {: reason:ptr reasonu:n :}
   FO-DEPTH @ 0 ?do reason reasonu i FRAME-REFUSE loop ;

private

\ A slot 19 record's kind: the output operation the top level issued.
1 constant OUT-EMIT
2 constant OUT-WRITE
16 constant OUT-HEAD-BYTES

\ Room for a record of u bytes, and its header. The reserve is the only
\ fallible step, taken before anything is written.
: OUT-HEAD ( n n -- )
   {: kind:n u:n :}
   OUTPUT-U @ OUT-HEAD-BYTES + u + FILE-OUTPUT-RESERVE
   kind OUTPUT-U @ FILE-OUTPUT LE:U64!
   u OUTPUT-U @ 8 + FILE-OUTPUT LE:U64!
   OUT-HEAD-BYTES OUTPUT-U +! ;

public

\ Record a write the top level issued, in the innermost open file.
: OUTPUT+ ( ptr u8 n -- )
   {: a:ptr u:n :}
   FILE-OPEN? 0= if exit then
   OUT-WRITE u OUT-HEAD
   a OUTPUT-U @ FILE-OUTPUT u BYTE-COPY
   u OUTPUT-U +! ;

\ Record an emit the top level issued, in the innermost open file.
: EMIT+ ( n -- )
   {: c:n :}
   FILE-OPEN? 0= if exit then
   OUT-EMIT 8 OUT-HEAD
   c OUTPUT-U @ FILE-OUTPUT LE:U64!
   8 OUTPUT-U +! ;

\ The output bytes the open files hold: a run that wrote moves it.
: OUTPUT-MARK ( -- n ) OUTPUT-U @ ;

private

\ FRAME-REFUSE of open file k naming the record the reason is about.
: FRAME-REFUSE-WORD ( ptr u8 n ptr n n -- )
   {: reason:ptr reasonu:n rec:ptr k:n :}
   k FRAME-REFUSED? if exit then
   reason reasonu k FRAME-WHY+
   s"  word=" k FRAME-WHY+
   rec XREF-NAME$ k FRAME-WHY+ ;

public

\ FILE-REFUSE naming the record the reason is about.
: REFUSE-WORD ( ptr u8 n ptr n -- )
   {: reason:ptr reasonu:n rec:ptr :}
   FILE-OPEN? 0= if exit then
   reason reasonu rec FO-DEPTH @ 1- FRAME-REFUSE-WORD ;

private

\ The live address cell declarations.
: SLOT-COUNT ( -- n ) ADDRESS-CELLS:LIVE-SPAN nip ;

\ Address cell row idx is live and holds raw. Rows are only appended, and a
\ removal (ADDRESS-CELLS:REMOVE-XT-SPAN, KEEP-BELOW) compacts them in order,
\ so a removal at or below a recorded row leaves another value or none there.
: ROW-HOLDS? ( n n -- bool )
   {: idx:n raw:n :}
   ADDRESS-CELLS:LIVE-SPAN {: rows:ptr count:n :}
   idx count >= if false exit then
   rows idx cells + @ raw = ;

\ Kernel cell k below the floor that a file may change only in passing: its
\ DATA offset and its name. A file that leaves one changed refuses. These are
\ the compiler tier selection, the executable-build scope's depth and saved
\ tier, and the optimizing compiler's dispatch.
: KCELL ( n -- n ptr u8 n )
   {: k:n :}
   k 0 = if NCOMP-DISPATCH:TIER-CELL s" tier" exit then
   k 1 = if NCOMP-DISPATCH:BUILD-DEPTH-CELL s" build-depth" exit then
   k 2 = if NCOMP-DISPATCH:BUILD-TIER-CELL s" build-tier" exit then
   k 3 = if NCOMP-DISPATCH:XT-CELL s" dispatch" exit then
   BAD ;

: KCELL@ ( n -- n )
   KCELL 2drop {: off:n :}
   data-base off + CELL-VIEW @ ;

: KCELLS-TAKE ( -- )
   KCELLS 0 ?do i KCELL@ i KCELL-WAS ! loop
   1 KCELLS-HELD ! ;

\ Refuse the innermost file when the open segment left a kernel cell other
\ than its image found it, naming the first such cell.
: KCELLS-CHECK ( -- )
   KCELLS-HELD @ 0= if exit then
   0 KCELLS-HELD !
   KCELLS 0 ?do
      i KCELL@ i KCELL-WAS @ <> if
         FILE-REFUSED? 0= if
            s" kernel-cell-changed cell=" WHY+ i KCELL WHY+ drop
         then unloop exit
      then
   loop ;

\ Forget the kernel cells the last image found: the dispatch cell holds a code
\ address, which a saved image must not carry from one build process.
: KCELLS-RELEASE ( -- )
   KCELLS 0 ?do 0 i KCELL-WAS ! loop
   0 KCELLS-HELD ! ;

\ Mark the innermost file's open segment's first address cell row and the
\ value of the row before it.
: SLOTS-MARK ( -- )
   SLOT-COUNT {: slot:n :}
   slot FO-SEG-SLOT FO !
   slot 0 > if slot 1- ADDRESS-CELLS:ROW@ else 0 then FO-SEG-PREV FO ! ;

\ Copy DATA from the process floor to HERE as the open segment's
\ before-image, counting the bytes against the innermost file, and mark the
\ segment's address cell rows and kernel cells, after the hooks the caller
\ set for the segment, so their writes are never the file's. A refused file
\ takes no image. The floor is HERE at the process's first image, lowered to
\ HERE when a rewound dictionary leaves HERE below it.
: IMAGE-TAKE ( -- )
   SLOTS-MARK
   KCELLS-TAKE
   AOT-ARM:HERE-N {: top:n :}
   IMAGE-FLOOR @ 0= top IMAGE-FLOOR @ < or if top IMAGE-FLOOR ! then
   top IMAGE-FLOOR @ - {: bytes:n :}
   0 IMAGE-U !
   FILE-REFUSED? bytes 0= or if exit then
   bytes IMAGE-RESERVE
   IMAGE-FLOOR @ DATA-PTR 0 IMAGE bytes BYTE-COPY
   bytes IMAGE-U !
   bytes FO-IMAGE FO +! ;

\ The first index from lo below hi whose site is not below va, hi when none.
: SITE-AT ( n n n -- n )
   {: va:n :}
   begin 2dup < while
      2dup + 2 / dup FILE-SITES @ va < if rot drop 1+ swap else nip then
   repeat drop ;

\ Record the earlier DATA byte at va as changed, once per innermost file,
\ keeping its sites ascending. A segment's sites arrive ascending, so an
\ insertion moves only the later segments' sites above it.
: SITE+ ( n -- )
   {: va:n :}
   FO-SITES FO @ SITES-N @ va SITE-AT {: at:n :}
   at SITES-N @ < if at FILE-SITES @ va = if exit then then
   SITES-N @ 1+ FILE-SITES-RESERVE
   SITES-N @ begin dup at > while
      dup 1- FILE-SITES @ over FILE-SITES ! 1-
   repeat drop
   va at FILE-SITES !
   1 SITES-N +! ;

\ Record each byte of width bytes at offset off past floor that differs
\ between live and before.
: BYTE-SITES ( n n n ptr u8 ptr u8 -- )
   {: floor:n off:n width:n live:ptr before:ptr :}
   width 0 ?do
      live off + i + c@ before off + i + c@ <> if floor off + i + SITE+ then
   loop ;

\ Record each earlier DATA byte that differs from the open segment's
\ before-image, comparing a cell at a time.
: IMAGE-DIFF ( -- )
   IMAGE-U @ {: size:n :}
   0 IMAGE-U !
   size 0= if exit then
   IMAGE-FLOOR @ {: floor:n :}
   floor DATA-PTR {: live:ptr :}
   0 IMAGE {: before:ptr :}
   size CELL / CELL * {: bulk:n :}
   bulk 0 ?do
      live i + CELL-VIEW @ before i + CELL-VIEW @ <> if
         floor i CELL live before BYTE-SITES
      then
   CELL +loop
   floor bulk size bulk - live before BYTE-SITES ;

\ The engine's DATA floor. The literal pool raises it past each segment it
\ opens at HERE (src/compiler/native/string.f PUBLISH), so a segment that
\ leaves it changed holds pool bytes a replay cannot put back.
: POOL-FLOOR ( -- n ) data-base DATA-FLOOR-CELL + CELL-VIEW @ ;

\ Open the innermost file's next segment at the present cursors.
: SEGMENT-OPEN ( -- )
   ndict@ FO-SEG-ORD FO !
   AOT-ARM:HERE-N FO-SEG-HERE FO !
   POOL-FLOOR FO-SEG-POOL FO !
   OUTPUT-U @ FO-SEG-OUT FO !
   FO-REQ-XT FO @ REQ-NOW FO-SEG-REQ FO !
   AOT-ARM:WIDN FO-SEG-WID FO !
   0 IMAGE-U ! 0 KCELLS-HELD !
   USER-MARK if 1 else 0 then FO-SEG-USER-BOUND FO ! FO-SEG-USER FO !
   0 NUNIT-DEP:USER-READ !
   FFI-BOUND? 0= if exit then
   s" FFI:NEXT-FN" FFI-N FO-SEG-FN FO !
   s" FFI:MARK" FFI-N FO-SEG-ROOM FO !
   FFI-SELECTION if 1 else 0 then FO-SEG-SEL-SET FO !
   FO-SEG-SEL-WID FO ! FO-SEG-SEL-LIB FO !
   0 OBS-N ! OBS-SET ;

\ The open segment's FFI declarations against its open marks: a row in the
\ bank the segment did not select refuses ffi-bank, and that names the
\ cause, so it goes first; a table that grew refuses ffi-grown. A selection
\ write names no row. Whether the declarer's selection changed.
: FFI-SEG-CK ( -- bool )
   s" FFI:MAKER-BANK" FFI-N {: bank:n :}
   FO-SEG-FN FO @ bank and {: own:n :}
   OBS-N @ 0 ?do
      i 2 * FFI-OBS @ OBS-SCOPE <> if
         i 2 * 1+ FFI-OBS @ bank and own <> if s" ffi-bank" FILE-REFUSE then
      then
   loop
   s" FFI:MARK" FFI-N FO-SEG-ROOM FO @ <> if s" ffi-grown" FILE-REFUSE then
   FFI-SELECTION {: lib:n wid:n stated:bool :}
   stated if 1 else 0 then FO-SEG-SEL-SET FO @ <>
   wid FO-SEG-SEL-WID FO @ <> or lib FO-SEG-SEL-LIB FO @ <> or ;

\ The open segment's FFI rows after its SEGMENT row, whose first ordinal
\ they carry. A segment that declared or wrote the selection records the
\ selection it opened with: a selecting word that leaves the selection as it
\ found it adds no other row, and a hit must find that selection live to
\ leave the one a cold run would. A segment with no FFI event depends on no
\ FFI state, so it records none: an ambient selection it cannot key would
\ refuse it (EVENT-OUT). A selection write carries no row of its own.
: FFI-ROWS+ ( n bool -- )
   {: first:n sel:bool :}
   FFI-BOUND? 0= if exit then
   OBS-N @ 0<> if
      EV-FFI-SCOPE first 0 FO-SEG-SEL-LIB FO @ FO-SEG-SEL-WID FO @
         FO-SEG-SEL-SET FO @ 0 0 0 EV+
   then
   OBS-N @ 0 ?do
      i 2 * FFI-OBS @ {: kind:n :}
      i 2 * 1+ FFI-OBS @ {: idx:n :}
      kind OBS-LIB = if
         idx s" FFI:LIB-ROW@" FFI-XT FFI-LIB-ROW-XT execute {: path:ptr u:n :}
         EV-FFI-LIB first path u NAMES+ u idx 0 0 0 0 EV+
      then
      kind OBS-FN = if
         idx s" FFI:FN-ROW@" FFI-XT FFI-FN-ROW-XT execute
            {: name:ptr u:n lib:n argc:n spills:n :}
         EV-FFI-FN first name u NAMES+ u lib argc spills idx 0 EV+
      then
   loop
   sel 0= if exit then
   FFI-SELECTION {: lib:n wid:n stated:bool :}
   EV-FFI-SCOPE first 1 lib wid stated if 1 else 0 then 0 0 0 EV+ ;

\ Whether the live address records among the count ordinals from first stand
\ at or past the segment's start, each at or past the one before: a replay
\ fills the segment's DATA forward, up to each record in turn.
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

\ The open segment's OUT row, first its first ordinal, when its top level
\ wrote output.
: OUTPUT-ROW+ ( n -- )
   {: first:n :}
   OUTPUT-U @ FO-SEG-OUT FO @ - {: u:n :}
   u 0= if exit then
   EV-OUT first FO-SEG-OUT FO @ u 0 0 0 0 0 EV+ ;

\ Close the innermost file's open segment as one SEGMENT row, or none when
\ it added no record, DATA, require, wordlist or address cell row. HERE or
\ an address record going back, the segment opening below the end of the
\ file's last SEGMENT row (a child rewound DATA into it), the DATA floor
\ moving (a literal pool segment opened at HERE), the wordlist counter going
\ back, a require counter other than the one the file opened with, or an
\ address cell row removed below the segment's first refuses it. The row
\ records the values of its first and last address cell rows, which the
\ file's encoding checks again; a segment that moved TASK's user-slot
\ watermark, either way, or ran a reader of it adds its TASK row. The earlier
\ DATA bytes the segment changed are recorded whether or not it adds a row; a
\ kernel cell it left changed refuses it. Output its top level wrote adds an
\ OUT row after the rest.
: SEGMENT-CLOSE ( -- )
   OBS-CLEAR
   NUNIT-DEP:USER-READ @ 0<>  0 NUNIT-DEP:USER-READ ! {: read:bool :}
   FO-SEG-ORD FO @ {: first:n :}
   ndict@ first - {: count:n :}
   count 0 < if BAD then
   AOT-ARM:HERE-N FO-SEG-HERE FO @ - {: bytes:n :}
   AOT-ARM:WIDN FO-SEG-WID FO @ - {: wids:n :}
   bytes 0 < if s" data-rewound" FILE-REFUSE exit then
   FO-SEG-HERE FO @ ROWS-END < if s" data-rewound" FILE-REFUSE exit then
   first count ADDR-ORDERED? 0= if s" data-rewound" FILE-REFUSE exit then
   POOL-FLOOR FO-SEG-POOL FO @ <> if s" literal-pool" FILE-REFUSE exit then
   wids 0 < if s" wordlist-rewound" FILE-REFUSE exit then
   REQ-XT FO-REQ-XT FO @ <> if s" requires-unknown" FILE-REFUSE exit then
   FO-REQ-XT FO @ REQ-NOW FO-SEG-REQ FO @ - {: reqs:n :}
   reqs 0 < if s" requires-unknown" FILE-REFUSE exit then
   FO-SEG-SLOT FO @ {: slot:n :}
   slot 0 > if
      slot 1- FO-SEG-PREV FO @ ROW-HOLDS? 0= if
         s" slots-rewound" FILE-REFUSE exit
      then
   then
   \ A segment that ran a TASK watermark reader or moved the watermark, with
   \ TASK bound at its open and close, records it; a read leaves exit = entry.
   FO-SEG-USER FO @ {: entry:n :}
   USER-MARK {: user:n bound:bool :}
   bound FO-SEG-USER-BOUND FO @ 1 = and
      user entry <> read or and {: task:bool :}
   KCELLS-CHECK
   FILE-REFUSED? 0= if IMAGE-DIFF then
   FFI-BOUND? if FFI-SEG-CK else false then {: sel:bool :}
   SLOT-COUNT slot - {: slots:n :}
   count bytes or reqs or wids or slots or 0= OBS-N @ 0= and sel 0= and
      task 0= and if first OUTPUT-ROW+ exit then
   EV-SEGMENT first count FO-SEG-HERE FO @ bytes
      FO-SEG-REQ FO @ reqs FO-SEG-WID FO @ wids EV+
   EVENTS-N @ 1- {: row:n :}
   slot row 9 EV ! slots row 10 EV ! NUNIT-DEP:FILE-ROWS row 13 EV !
   slots 0 > if
      slot ADDRESS-CELLS:ROW@ row 11 EV !
      slot slots + 1- ADDRESS-CELLS:ROW@ row 12 EV !
   then
   SEG-DATA-U @ {: at:n :}
   at row 14 EV !
   bytes 0 > if
      at bytes + SEG-DATA-RESERVE
      FO-SEG-HERE FO @ DATA-PTR at SEG-DATA bytes BYTE-COPY
      bytes SEG-DATA-U +!
   then
   first sel FFI-ROWS+
   task if EV-TASK first entry user 0 0 0 0 0 EV+ then
   first OUTPUT-ROW+ ;

\ The end of open file k's event rows, where the next frame's begin.
: EV-HI ( n -- n )
   {: k:n :}
   k 1+ FO-DEPTH @ < if k 1+ FO-EVENTS FRAME @ else EVENTS-N @ then ;

\ Record ord lies in one of open file k's closed segments.
: SEGS-OWN? ( n n -- bool )
   {: ord:n k:n :}
   k EV-HI k FO-EVENTS FRAME @ ?do
      i 0 EV @ EV-SEGMENT = if
         ord i 1 EV @ >= ord i 1 EV @ i 2 EV @ + < and if
            true unloop exit
         then
      then
   loop false ;

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
   owner 8 - CELL-VIEW @ CHECKER-OWNER-ABI:UNIT-PREPARE-REFS-OFF CELL + <
      if false exit then
   CHECKER-OWNER-ABI:UNIT-MARK-OFF OWNER-CELL 0=
   CHECKER-OWNER-ABI:UNIT-PREPARE-BEGIN-OFF OWNER-CELL 0= or
   CHECKER-OWNER-ABI:UNIT-PREPARE-END-OFF OWNER-CELL 0= or
   CHECKER-OWNER-ABI:UNIT-INSTALL-OFF OWNER-CELL 0= or
   CHECKER-OWNER-ABI:UNIT-EXPORT-REFS-OFF OWNER-CELL 0= or
   CHECKER-OWNER-ABI:UNIT-PREPARE-REFS-OFF OWNER-CELL 0= or
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

\ rc is a type-family code a preparation throws: a plan refusal, or a FILE
\ dependency whose family contract changed since the object was built.
: FAMILY-CODE? ( n -- bool )
   {: rc:n :}
   rc s" TFAM:E-REG-AOT-REFUSED" UNIT-CODE =
   rc s" TFAM:E-UNIT-FAMILY-CHANGED" UNIT-CODE = or ;

: CHECK-MARK-RUN ( -- )
   CHECKER-OWNER-ABI:UNIT-MARK-OFF CHECK-XT DEF-ACTION-XT execute ;

\ Mark the checker state where the innermost file's next slice starts; a
\ state the checker cannot mark refuses the file.
: CHECK-MARK ( -- )
   FILE-CHECK-AVAILABLE? 0= if s" checker-unavailable" FILE-REFUSE exit then
   ['] CHECK-MARK-RUN catch {: rc:n :}
   rc 0= if 1 FO-CKMARK FO ! exit then
   rc s" CHECKER-REG:E-UNIT-STATE" UNIT-CODE = if
      s" checker-mark" FILE-REFUSE exit
   then
   rc throw ;

\ Event row k is a SEGMENT row.
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
   ['] CHECK-REF CHECKER-OWNER-ABI:UNIT-EXPORT-REFS-OFF CHECK-XT
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

\ Close the innermost file's open mark as a slice. A refused file exports
\ nothing; state the checker cannot export refuses it.
: CHECK-EXPORT ( -- )
   FO-CKMARK FO @ 0= if exit then
   0 FO-CKMARK FO !
   FILE-REFUSED? if exit then
   ['] CHECK-EXPORT-RUN catch {: rc:n :}
   rc 0= if exit then
   rc CHECK-CODE? if s" checker-export" FILE-REFUSE exit then
   rc throw ;

public

\ Open a file frame at the file's first record ordinal. A child's load ends
\ its parent's segment with a LOAD row: the load kind, where the parent's
\ source stood, below zero outside it, which refuses the parent, and the
\ child's path and root. The replay loads the child in the scope the parent
\ opened with, so a parent whose search order, current wordlist or package
\ at the load differs from it is refused. The object carries the earlier
\ DATA a file changed as it stood at the file's end, where the child saw it
\ as it stood at the load, so a parent that changed earlier DATA before a
\ load is refused.
: FILE-BEGIN ( ptr u8 n ptr u8 n n n -- )
   {: path:ptr pathu:n root:ptr rootu:n kind:n at:n :}
   FILE-OPEN? if
      SEGMENT-CLOSE
      SITES-N @ FO-SITES FO @ > if s" earlier-write-before-load" FILE-REFUSE then
      at 0 < if s" load-outside-source" FILE-REFUSE then
      NUNIT-DEP:FILE-SCOPE-KEPT? 0= if s" scope-at-load" FILE-REFUSE then
      EVENTS-N @ {: row:n :}
      EV-LOAD ndict@ kind at path pathu NAMES+ pathu
         root rootu NAMES+ rootu 0 EV+
      \ The pending loader action claims the row; a load no loader token of
      \ the file started at a loader cut, as from a colon word or an
      \ evaluated text, stays unclean and FILE-ENCODE refuses it.
      FO-ACT-ROW FO @ ACT-OPEN = if row FO-ACT-ROW FO ! then
      CHECK-EXPORT
      NUNIT-DEP:FILE-ROWS row 11 EV !
      INPUTS-U @ FO-INPUTS FO @ - row 12 EV !
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
   ORIGIN-U @ FO-ORIGIN FO !
   OUTPUT-U @ FO-OUTPUT FO !
   SEG-DATA-U @ FO-SEGB FO !
   ORIGIN-U @ 8 + FILE-ORIGIN-RESERVE
   SINK-NONE ORIGIN-U @ FILE-ORIGIN LE:U64!
   8 ORIGIN-U +!
   REQ-XT FO-REQ-XT FO !
   0 FO-IMAGE FO !
   SITES-N @ FO-SITES FO !
   CHECK-U @ FO-CHECK FO !
   CKORD-N @ FO-CKORD FO !
   0 FO-SLICES FO !
   0 FO-CKMARK FO !
   ACT-NONE FO-ACT-ROW FO !
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
   then
   \ Without process cells the seed and the floor would survive into an image.
   PROCESS-ABSENT @ if s" process-cell-absent" WHY+ then
   \ Without the registry's observer a declaration would leave no row.
   FFI-BOUND? 0= if s" ffi-mark-absent" WHY+ then
   CHECK-MARK ;

\ Close the innermost frame; its parent's next segment opens.
: FILE-END ( -- )
   FO-DEFS FO @ DEFS-U !
   FO-INDEX FO @ INDEX-U !
   FO-INPUTS FO @ INPUTS-U !
   FO-WHY FO @ WHY-U !
   FO-EVENTS FO @ EVENTS-N !
   FO-NAMES FO @ NAMES-U !
   FO-ORIGIN FO @ ORIGIN-U !
   FO-OUTPUT FO @ OUTPUT-U !
   FO-SEGB FO @ SEG-DATA-U !
   FO-SITES FO @ SITES-N !
   FO-CHECK FO @ CHECK-U !
   FO-CKORD FO @ CKORD-N !
   -1 FO-DEPTH +!
   \ A hit or a throw pops a file without its segment's close.
   FILE-OPEN? if SEGMENT-OPEN CHECK-MARK else OBS-CLEAR then ;

\ A loader token of the innermost file starts its action at a loader cut:
\ the child it loads claims its LOAD row (FILE-BEGIN).
: LOAD-ACTION ( -- ) FILE-OPEN? if ACT-OPEN FO-ACT-ROW FO ! then ;

\ The pending loader action returned; when ok, the loading file stood
\ balanced at offset off, and the LOAD row it claimed is clean.
: LOAD-FINISH ( n bool -- )
   {: off:n ok:bool :}
   FILE-OPEN? 0= if exit then
   FO-ACT-ROW FO @ {: row:n :}
   ok row 0 >= and if 1 row 9 EV ! off row 10 EV ! then
   ACT-NONE FO-ACT-ROW FO ! ;

\ The innermost file loaded under origin sink kind.
: ORIGIN-SINK! ( n -- ) FO-ORIGIN FO @ FILE-ORIGIN LE:U64! ;

\ The innermost file's origin row: the definition at ord, its name token's
\ span at to end in the file at path, and its name. A row naming no record
\ of the file refuses the file.
: ORIGIN+ ( n n n ptr u8 n ptr u8 n -- )
   {: ord:n at:n end:n path:ptr pathu:n name:ptr nameu:n :}
   ord FO-FIRST FO @ < ord ndict@ >= or if
      s" origin-record" FILE-REFUSE exit
   then
   FO-NAMES FO @ {: names:n :}
   ord FO-FIRST FO @ - ORIGIN-N
   at ORIGIN-N end ORIGIN-N
   name nameu NAMES+ names - ORIGIN-N nameu ORIGIN-N
   pathu 0= if 0 else path pathu NAMES+ names - then ORIGIN-N
   pathu ORIGIN-N ;

\ The retire hook (XREF:BEFORE-RETIRE!). An object's record rows carry no
\ time: a replay makes a retired row retired where the row stands. So only a
\ record of the innermost file's open segment may retire, with no load or
\ closed segment between its making and its retirement. Any other record's
\ object cannot hold the retirement in place: an earlier file's, a child's,
\ or a closed segment of this or an open parent file, which what loaded since
\ has seen live. The innermost file and every open file whose segments made
\ the record refuse.
: FILE-RETIRE ( ptr n -- )
   {: rec:ptr :}
   FILE-OPEN? 0= if exit then
   rec NFILE-REF:REC-ORD {: ord:n :}
   ord FO-SEG-ORD FO @ >= if exit then
   s" retires-earlier" rec REFUSE-WORD
   FO-DEPTH @ 1- 0 ?do
      ord i SEGS-OWN? if s" retires-earlier" rec i FRAME-REFUSE-WORD then
   loop ;

\ Take the before-image of the innermost file's open segment, against which
\ the segment's close finds the earlier DATA it changed. The caller takes it
\ after setting its own hooks for the segment, so their writes fall outside
\ the compared interval (NDEF-CACHE FILE-UNIT-WITH); a segment without one
\ records no changes.
: SEGMENT-IMAGE ( -- ) FILE-OPEN? if IMAGE-TAKE then ;

\ The bytes the innermost file's before-images copied.
: IMAGE-BYTES ( -- n ) FO-IMAGE FO @ ;

: REASON$ ( -- ptr u8 n )
   FO-WHY FO @ {: first:n :}
   WHY-U @ first = if NULL-PTR 0 exit then
   first FILE-WHY WHY-U @ first - ;

: FILE-INPUTS+ ( ptr u8 n -- )
   FILE-OPEN? 0= if 2drop exit then
   INPUTS+ ;

: CLOSE ( -- )
   OBS-CLEAR 0 OBS-N ! 0 OPEN-FFI ! FFI-OBS-RELEASE
   0 DEFS-U ! 0 INDEX-U ! 0 INPUTS-U ! 0 WHY-U ! 0 FO-DEPTH !
   0 EVENTS-N ! 0 NAMES-U ! 0 ORIGIN-U ! 0 OUT-U ! 0 ROW-AT ! 0 SEG-DATA-U !
   0 OUTPUT-U ! 0 OPEN-OUT !
   0 OPEN-DEFS ! 0 OPEN-ROWS ! 0 OPEN-INPUTS !
   0 OPEN-RECS ! 0 OPEN-DATA ! 0 OPEN-REQS ! 0 OPEN-WIDS !
   0 OPEN-LOADS ! 0 OPEN-SEGS ! 0 OPEN-SLOTS ! 0 OPEN-KINDS ! 0 OPEN-OWNED !
   0 OPEN-REM ! 0 OPEN-ADDR ! 0 OPEN-SEG-END !
   0 OPEN-SINK ! 0 OPEN-ORIGIN-OK !
   false OPEN-POOL-ROOM !
   NULL-PTR OBJ-BYTES ! 0 OBJ-SIZE ! NULL-PTR OBJ-KEY ! 0 OBJ-KEY-U !
   0 INS-FIRST ! 0 INS-ROW ! 0 INS-DEFS-OFF !
   0 INS-DATA ! 0 INS-DATA-U ! 0 INS-DATA-AT !
   0 INS-MAP-AT ! 0 INS-SEGS ! 0 INS-SLICE ! INS-MAP-RELEASE
   NULL-PTR IMPORT-SEC ! 0 IMPORT-NREFS ! 0 IMPORT-OFF !
   0 LITS-U ! 0 LITS-N ! 0 OWNER-ROW-XT ! 0 VALS-U ! 0 OPEN-VALS !
   0 IMAGE-U ! KCELLS-RELEASE 0 SITES-N ! 0 SLOTW-N ! 0 OWNER-N !
   0 OPEN-UPDS ! 0 PINS-N ! 0 OPEN-PINS !
   0 CHECK-U ! 0 CKORD-N ! 0 CHECK-AT ! 0 OPEN-CHECKS !
   0 DATA-AT ! 0 REQS-AT ! 0 PROT-AT ! 0 EVS-AT ! 0 SLOTS-AT ! 0 LITS-AT !
   0 UPDS-AT ! 0 PINS-AT ! 0 VAL-SITES !
   IMAGE-RELEASE FILE-SITES-RELEASE FILE-SLOTW-RELEASE OWNER-SPANS-RELEASE
   SEG-DATA-RELEASE
   SITE-OWNS-RELEASE PIN-OWNS-RELEASE
   FILE-LITS-RELEASE FILE-LIT-ADDRS-RELEASE FILE-VALS-RELEASE
   FILE-CHECK-RELEASE FILE-CKORD-RELEASE
   FILE-DEFS-RELEASE FILE-INDEX-RELEASE FILE-INPUTS-RELEASE
   FILE-WHY-RELEASE FILE-FRAMES-RELEASE FILE-ORDS-RELEASE
   FILE-EVENTS-RELEASE FILE-NAMES-RELEASE FILE-ORIGIN-RELEASE FILE-OUT-RELEASE
   FILE-OUTPUT-RELEASE
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

IMAGE-FLOOR PROCESS-CELL

private

CAST: OBSERVER-CELL-XT ( n -- [ -- ptr n ] )
\ The registry's observer belongs to the open segment: a process cell, so an
\ image captured inside one stores 0 while this builder keeps it. Looked up
\ as PROCESS-CELL is: an engine without it reports no declarations.
: FFI-OBSERVER-CELL ( -- )
   s" FFI" -1 search-wl {: wid:n :}
   wid 0= if exit then
   s" OBSERVER-CELL" wid search-wl {: xt:n :}
   xt 0= if exit then
   xt OBSERVER-CELL-XT execute PROCESS-CELL ;

FFI-OBSERVER-CELL

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
   NUNIT-DEP:DIAG? @ 0= if exit then
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
: DEF-BYTES ( n -- ptr u8 )
   s" NART:BYTES" DEF-TARGET DEF-PTR-XT execute ;

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
   DEF-DOES-OFF @ DEF-META 40 + LE:U64!
   DEF-MODULE-DELTA @ DEF-META 48 + LE:U64!
   DEF-GEN-DELTA @ DEF-META 56 + LE:U64!
   DEF-EPOCH-DELTA @ DEF-META 64 + LE:U64!
   DEF-BUILD-DELTA @ DEF-META 72 + LE:U64! ;

: DEF-PACK-SECTIONS ( ptr u8 n -- )
   {: deps:ptr depsu:n :}
   NUNIT-FILE:RESET-KEEP
   NUNIT-FILE:SECTION-COUNT 0 ?do i NULL-PTR 0 NUNIT-FILE:SECTION! loop
   0 0 DEF-CODE DEF-CODE-U @ NUNIT-FILE:SECTION!
   1 DEF-META DEF-META-BYTES NUNIT-FILE:SECTION!
   DEF-CALL-U @ 0 > if
      2 0 DEF-CALL DEF-CALL-U @ NUNIT-FILE:SECTION!
   then
   DEF-ADDR-U @ 0 > if
      3 0 DEF-ADDR DEF-ADDR-U @ NUNIT-FILE:SECTION!
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
   0 DEF-LIT-U ! 0 DEF-HOST-U !
   0 DEF-LIT-N ! 0 DEF-CALL-SITE-N ! 0 DEF-ADDR-SITE-N !
   0 DEF-PENDING ! 0 DEF-CODE-BASE ! 0 DEF-DOES-OFF !
   0 DEF-MODULE-DELTA ! 0 DEF-GEN-DELTA !
   0 DEF-EPOCH-DELTA ! 0 DEF-BUILD-DELTA !
   0 DEF-HELD-E ! 0 DEF-HELD-IDX ! 0 DEF-HELD-FN ! 0 DEF-HELD-SIZE !
   0 DEF-PLAN-ON ! ;

: DEF-DROP-PLAN ( -- n )
   DEF-SOURCE-END
   ['] DEF-ABANDON-HOST catch
   NFILE-REF:END-CAPTURE
   NFILE-REF:DISCARD-PREPARED
   DEF-RESET ;

\ The metadata row's own bounds, with no session state: the recorded length
\ matches the code bytes, the code is whole instructions, the flags are in range.
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
   DEF-ELIGIBLE? 0= if s" begin-ineligible" ndict@ REFUSED exit then
   ndict@ DEF-PENDING !
   1 DEF-ACTIVE !
   DEF-CACHEABLE-NOW? if 1 DEF-CACHEABLE ! then
   DEF-SOURCE-BEGIN ;

: DEFINITION-END ( -- bool )
   DEF-ACTIVE @ 0= if DEF-SOURCE-END false exit then
   0 DEF-ACTIVE !
   DEF-ELIGIBLE? 0= if s" end-ineligible" DEF-PENDING @ REFUSED then
   DEF-EMITTED @ 0= DEF-REFUSED @ 0<> or if
      DEF-REFUSED @ 0<> if s" capture-refused" else s" no-emission" then
         DEF-PENDING @ REFUSED
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

: PACK-DEFINITION ( ptr u8 n ptr u8 n -- ptr u8 n )
   {: key:ptr keyu:n deps:ptr depsu:n :}
   NUNIT-CAPTURE:PROFILE-CK
   DEF-FINISHED @ 0= if BAD then
   deps depsu DEF-PACK-SECTIONS
   NUNIT-FILE:ARCH-AARCH64 s" DEFINITION" key keyu NUNIT-FILE:PACK ;

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
   NUNIT-FILE:SECTION-COUNT 0 ?do
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

\ DATA address va as a reference into the closing file's DATA section, 0
\ when no segment of the file allotted it.
: DATA-REF ( n -- n )
   {: va:n :}
   va 0 OWN-OFF {: off:n :}
   off 0 < if 0 exit then
   NFILE-REF:DATA off 0 NFILE-REF:REF-LOCAL ;

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

\ Code memory read as bytes, the one address a stored body is copied from.
CAST: N>U8 ( n -- ptr u8 )
\ Store rec's constant body in FILE-VALS as one slot 17 entry: its recorded
\ length, then the code bytes its load emitted, copied back on a hit. The
\ body is position independent unless it holds a relocation site, which a
\ byte copy cannot carry: then -1, else the entry's offset.
: VAL-SITE ( n n -- ) 2drop 1 VAL-SITES +! ;
: VAL-ENTRY ( ptr n -- n )
   {: rec:ptr :}
   rec XREF-START {: start:n :}
   rec XREF-CODE-BYTES {: size:n :}
   0 VAL-SITES !
   start dbase@ - size [: VAL-SITE ;] SITES:EACH-IN-SPAN
   VAL-SITES @ 0<> if -1 exit then
   VALS-U @ {: at:n :}
   at 8 + size + FILE-VALS-RESERVE
   rec XREF-RAW-LEN at FILE-VALS LE:U64!
   start N>U8 at 8 + FILE-VALS size BYTE-COPY
   at 8 + size + VALS-U !
   at ;

\ Record key as a FILE object reference, 0 after it refuses the closing file:
\ a record the file made, retired or not (its object makes the record again),
\ or a live record an earlier file made under a declaration key.
: RECORD-TARGET ( n -- n )
   {: key:n :}
   key ndict@ >= if s" checker-ref-retired" FILE-REFUSE 0 exit then
   key OWN-ROW {: row:n :}
   row 0 >= if row key NFILE-REF:FILE-REF-RECORD exit then
   key XREF-REC XREF-RETIRED? if
      s" checker-ref-retired" FILE-REFUSE 0 exit
   then
   \ A record an earlier session made has no key to import: BIND-ORD has
   \ ordered the file on it (CHECK-EXPORT-RUN for a checker reference, the
   \ run that made a does> word for its definer).
   key NFILE-REF:DECL-KEY? nip nip 0= if
      s" ordered-dependency" FILE-REFUSE 0 exit
   then
   row key NFILE-REF:FILE-REF-RECORD ;

\ The does> definer whose clause starts at address d, searched back from
\ record ord, -1 when none: the clause record follows its definer as the
\ definer's companion.
: DOES-DEFINER ( n n -- n )
   {: ord:n d:n :}
   0 ord 1- ?do
      i 0 > if
         i XREF-REC XREF-START d = if
            i 1- XREF-DOES-COMPANION? if i 1- unloop exit then
         then
      then
   -1 +loop -1 ;

\ A word a does> definer made: its address body, and the definer that the
\ branch ending its code reaches through the definer's clause.
: DOES-SHAPE ( n ptr n -- n n n bool )
   {: ord:n rec:ptr :}
   rec XREF-START rec XREF-CODE-BYTES + 4 - {: slot:n :}
   slot N>U8 LE:U32@ {: w:n :}
   w NBR:B? if ord slot w NBR:B-TARGET DOES-DEFINER else -1 then {: def:n :}
   def 0 < if s" does-clause-unknown" rec REFUSE-WORD RK-NONE 0 0 false exit then
   rec AOT-CELL:ALLOC-BASE DATA-REF {: body:n :}
   def RECORD-TARGET {: ref:n :}
   RK-DOES-WORD body ref body 0<> ref 0<> and ;

\ Record ord's row kind, value and private wordlist reference, and whether
\ they encode.
: RECORD-SHAPE ( n -- n n n bool )
   {: ord:n :}
   ord XREF-REC {: rec:ptr :}
   rec XREF-WORDLIST XREF-NAMESPACE-WL = if rec PACKAGE-SHAPE exit then
   rec XREF-RETIRED? if RK-RETIRED 0 0 true exit then
   ord 0 > if ord 1- XREF-DOES-COMPANION? else false then if
      RK-CLAUSE 0 0 true exit
   then
   rec XREF-START 0= if RK-CODELESS 0 0 true exit then
   rec XREF-FLAGS DKIND:MASK and DKIND:CAST = if RK-CAST 0 0 true exit then
   ord NFILE-REF:FIXED@ {: fkind:n fvalue:n known:bool :}
   known fkind CHECKER-OWNER-ABI:RECORD-FIXED-VAL = and if
      rec VAL-ENTRY {: at:n :}
      at 0 < if s" val-sites" rec REFUSE-WORD RK-NONE 0 0 false exit then
      RK-VAL at 0 true exit
   then
   known fkind CHECKER-OWNER-ABI:RECORD-FIXED-ADDR = and if
      RK-ADDR fvalue ADDR-SHAPE exit
   then
   rec NDICT:REC-DEFER-CELL {: cell:n :}
   cell 0<> if RK-DEFER cell ADDR-SHAPE exit then
   rec XREF-FLAGS DNAME-ALLOC and 0<> if
      rec XREF-FLAGS DKIND:MASK and 0= if ord rec DOES-SHAPE exit then
      s" does-pending" rec REFUSE-WORD RK-NONE 0 0 false exit
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
      i SEG? if i 4 EV @ 0 > if i 14 EV @ SEG-DATA i 4 EV @ OUT-S then then
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

\ Event row k as its EVENT-BYTES row. A SEGMENT row holds where in a cell
\ HERE stood at the segment's open: `align` and every definer that aligns
\ its body pad from there, so the DATA bytes hold that remainder's padding.
\ Its last cell is the file's dependency row count at its close, the rows a
\ replay checks before installing it.
: EVENT-OUT ( n -- )
   {: row:n :}
   row SEG? if
      EV-SEGMENT OUT-N row 1 EV @ FO-FIRST FO @ - OUT-N
      row 2 EV @ OUT-N row 4 EV @ OUT-N row 6 EV @ OUT-N
      row 8 EV @ OUT-N row 3 EV @ 1 cells mod OUT-N row 13 EV @ OUT-N
      0 OUT-N 0 OUT-N 0 OUT-N exit
   then
   row 0 EV @ {: kind:n :}
   kind EV-FFI-LIB = if
      kind OUT-N row 2 EV @ FO-NAMES FO @ - OUT-N row 3 EV @ OUT-N
      row 4 EV @ OUT-N 0 OUT-N 0 OUT-N 0 OUT-N 0 OUT-N
      0 OUT-N 0 OUT-N 0 OUT-N exit
   then
   kind EV-FFI-FN = if
      kind OUT-N row 2 EV @ FO-NAMES FO @ - OUT-N row 3 EV @ OUT-N
      row 4 EV @ OUT-N row 5 EV @ OUT-N row 6 EV @ OUT-N row 7 EV @ OUT-N
      0 OUT-N 0 OUT-N 0 OUT-N 0 OUT-N exit
   then
   \ The scope's reference is taken here: BEGIN-FILE-REFS resets the
   \ references after the segments close. It binds no owner: FFI-ROWS-HOLD?
   \ misses when an imported wordlist's package is gone.
   kind EV-FFI-SCOPE = if
      row 4 EV @ WID-REF {: ref:n :}
      ref 0= if s" ffi-scope-unknown" FILE-REFUSE then
      kind OUT-N row 2 EV @ OUT-N row 3 EV @ OUT-N ref OUT-N
      row 5 EV @ OUT-N 0 OUT-N 0 OUT-N 0 OUT-N
      0 OUT-N 0 OUT-N 0 OUT-N exit
   then
   kind EV-OUT = if
      kind OUT-N row 2 EV @ FO-OUTPUT FO @ - OUT-N row 3 EV @ OUT-N
      0 OUT-N 0 OUT-N 0 OUT-N 0 OUT-N 0 OUT-N
      0 OUT-N 0 OUT-N 0 OUT-N exit
   then
   kind EV-TASK = if
      kind OUT-N row 2 EV @ OUT-N row 3 EV @ OUT-N
      0 OUT-N 0 OUT-N 0 OUT-N 0 OUT-N 0 OUT-N
      0 OUT-N 0 OUT-N 0 OUT-N exit
   then
   EV-LOAD OUT-N row 2 EV @ OUT-N row 3 EV @ OUT-N
   row 4 EV @ FO-NAMES FO @ - OUT-N row 5 EV @ OUT-N
   row 6 EV @ FO-NAMES FO @ - OUT-N row 7 EV @ OUT-N row 9 EV @ OUT-N
   row 10 EV @ OUT-N row 11 EV @ OUT-N row 12 EV @ OUT-N ;

CAST: OWNER-ROW-FN ( n -- [ n -- ptr u8 n bool ] )
CAST: CELL-OWNERS-FN ( n -- [ [ ptr u8 n -- ] -- ] )

\ Value v's literal body in the target's pool, and whether one holds it.
: OWNER-ROW? ( n -- ptr u8 n bool )
   {: v:n :}
   OWNER-ROW-XT @ 0= if NULL-PTR 0 false exit then
   v OWNER-ROW-XT @ OWNER-ROW-FN execute ;

: LIT-HIT? ( n -- bool ) OWNER-ROW? {: body:ptr size:n found:bool :} found ;

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

\ An earlier file's DATA at va, owned by record ord whose DATA starts at
\ base, as a reference through that declaration, 0 when it is not keyed.
\ The file depends on that declaration as a compiled reference to it would
\ (DEF-ADDR-REF).
: REF-AT ( n n n -- n )
   {: va:n ord:n base:n :}
   ord 0 < if 0 exit then
   ord NFILE-REF:DECL-KEY? nip nip 0= if 0 exit then
   ord NUNIT-DEP:BIND-ORD
   ord va base - 0 1 NFILE-REF:REF-DECL-DATA ;

\ A slot value into an earlier file's DATA as a reference through the
\ declaration that owns it, 0 when no keyed declaration does or the offset
\ lies past its extent. The file depends on that declaration's DATA row,
\ whose digest covers the extent, so an edit that moves its end, and with it
\ the record the offset names, is stale.
: EARLIER-REF ( n -- n )
   {: va:n :}
   va AOT-CELL:DATA-ADDRESS? 0= if 0 exit then
   va OWNER-AT {: ord:n base:n :}
   ord 0 < if 0 exit then
   ord NFILE-REF:DECL-KEY? nip nip 0= if 0 exit then
   va base - {: addend:n :}
   ord NUNIT-DEP:DATA-ORD addend <= if 0 exit then
   ord addend 0 1 NFILE-REF:REF-DECL-DATA ;

\ OWNER-AT for each of the closing file's sites, in one pass over the
\ dictionary instead of one per site. A record is a candidate at the first
\ site its DATA start does not exceed; a later site's candidate starts above
\ every earlier site, so a running maximum carries each owner upward.
: SITE-OWNERS ( -- )
   FO-SITES FO @ {: first:n :}
   SITES-N @ first - {: count:n :}
   count 0= if exit then
   count 2 * SITE-OWNS-RESERVE
   count 0 ?do -1 i 2 * SITE-OWNS ! 0 i 2 * 1+ SITE-OWNS ! loop
   ndict@ 0 ?do
      i REC-DATA? if
         {: base:n :}
         first SITES-N @ base SITE-AT first - {: at:n :}
         at count < if
            at 2 * SITE-OWNS @ 0 < base at 2 * 1+ SITE-OWNS @ >= or if
               i at 2 * SITE-OWNS ! base at 2 * 1+ SITE-OWNS !
            then
         then
      else drop then
   loop
   -1 0 count 0 ?do
      i 2 * SITE-OWNS @ 0 >= if 2drop i 2 * SITE-OWNS @ i 2 * 1+ SITE-OWNS @ then
      2dup i 2 * 1+ SITE-OWNS ! i 2 * SITE-OWNS !
   loop 2drop ;

: WHY-N ( n -- )
   {: value:n :}
   value 10 >= if value 10 / recurse then
   s" 0123456789" drop value 10 mod + 1 WHY+ ;

: WHY-HEX ( n -- )
   {: value:n :}
   value 4 rshift 0<> if value 4 rshift recurse then
   s" 0123456789abcdef" drop value 15 and + 1 WHY+ ;

\ The record that owns the byte at site and the byte's offset in it, or -
\ and its offset in DATA when no record does.
: WHY-SITE ( n -- )
   {: site:n :}
   s" owner=" WHY+
   site OWNER-AT {: ord:n base:n :}
   ord 0 < if s" -" WHY+ site DATA-VA VA>N -
   else ord XREF-REC XREF-NAME$ WHY+ site base - then {: off:n :}
   s"  offset=" WHY+ off WHY-N ;

\ The closing file keeps a raw address its DATA holds at site, so it
\ refuses, naming the record that owns the cell and the cell's offset in it.
: DECLINE-CELL ( n n -- )
   {: site:n value:n :}
   FILE-REFUSED? if exit then
   s" raw-cell " WHY+ site WHY-SITE
   s"  value=0x" WHY+ value WHY-HEX ;

\ The owner of DATA address v: its DATA base and its record, -1 when the
\ closing file's segment holding v is the owner, else the earlier
\ declaration whose DATA holds it, -2 when neither does.
: PIN-OWNER ( n -- n n )
   {: v:n :}
   v 0 OWN-SEG {: start:n off:n len:n :}
   off 0 >= if start -1 exit then
   v OWNER-AT {: ord:n base:n :}
   ord 0 < if 0 -2 exit then
   ord FO-SEG-ORD FO @ >= ord FO-DEPTH @ 1- SEGS-OWN? or if 0 -2 exit then
   base ord ;

\ The reference to the owner of v PIN-OWNER found, 0 when no keyed
\ declaration is that owner.
: PIN-REF ( n n n -- n )
   {: v:n base:n ord:n :}
   ord -2 = if 0 exit then
   ord 0 >= if base ord base REF-AT exit then
   v 0 OWN-SEG {: start:n off:n len:n :}
   NFILE-REF:DATA off v - start + 0 NFILE-REF:REF-LOCAL ;

\ The cell at at holds DATA address v raw: one PIN-BYTES row pins the owner
\ of v, once per owner, so an install at another layout misses. A v no
\ keyed owner holds refuses the file.
: PIN ( n n -- )
   {: at:n v:n :}
   v PIN-OWNER {: base:n ord:n :}
   PINS-N @ 0 ?do
      i 2 * PIN-OWNS @ base = i 2 * 1+ PIN-OWNS @ ord = and if
         unloop exit
      then
   loop
   v base ord PIN-REF {: ref:n :}
   ref 0= if
      FILE-REFUSED? 0= if
         s" pin-unowned " WHY+ at WHY-SITE s"  value=0x" WHY+ v WHY-HEX
      then exit
   then
   PINS-N @ 1+ 2 * PIN-OWNS-RESERVE
   base PINS-N @ 2 * PIN-OWNS ! ord PINS-N @ 2 * 1+ PIN-OWNS !
   1 PINS-N +!
   ref OUT-N base OUT-N ;

\ The literal row of the body at body, bytes long: an earlier row of the
\ same body, else a new one.
: LIT-ROW-OF ( ptr u8 n -- n )
   {: body:ptr bytes:n :}
   body NULL-PTR BYTE-VIEW - {: address:n :}
   LITS-N @ 0 ?do
      i FILE-LIT-ADDRS @ address = if i unloop exit then
   loop
   LITS-N @ {: row:n :}
   row 1+ FILE-LIT-ADDRS-RESERVE
   address row FILE-LIT-ADDRS !
   LITS-U @ 16 + bytes + FILE-LITS-RESERVE
   bytes LITS-U @ FILE-LITS LE:U64!
   row LITS-U @ 8 + FILE-LITS LE:U64!
   bytes 0<> if body LITS-U @ 16 + FILE-LITS bytes BYTE-COPY then
   16 bytes + LITS-U +!
   1 LITS-N +!
   row ;

\ The record row and code offset of the closing file's own record whose
\ code holds v; -1 0 when none does.
: OWN-CODE ( n -- n n )
   {: v:n :}
   0 EVENTS-N @ FO-EVENTS FO @ ?do
      {: base:n :}
      i SEG? if
         i 1 EV @ {: first:n :}
         first i 2 EV @ + first ?do
            i XREF-REC {: rec:ptr :}
            rec XREF-START {: start:n :}
            rec XREF-RETIRED? 0= start 0<> and
            v start >= and v start rec XREF-CODE-BYTES + < and if
               base i + first - v start - unloop unloop exit
            then
         loop
         base i 2 EV @ +
      else base then
   loop drop -1 0 ;

\ Checker reference entry key bytes as a FILE object reference, 0 after it
\ refuses the closing file: a record it made, retired or not (its object
\ makes the record again), or a live record an earlier file made, else
\ storage in its own or an earlier declaration's DATA.
: CHECK-TARGET ( n n -- n )
   {: key:n bytes:n :}
   bytes 0 < if key RECORD-TARGET exit then
   key bytes OWN-OFF {: off:n :}
   off 0 >= if NFILE-REF:DATA off 0 NFILE-REF:REF-LOCAL exit then
   key EARLIER-REF {: ref:n :}
   ref 0= if s" checker-ref-unowned" FILE-REFUSE then
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

\ An xt cell's value as a reference, and whether it has one. Code of an
\ earlier record makes the file depend on that record. REF-CODE's local
\ branch never applies here: BEGIN-FILE-REFS zeroes CAP-CODE-U, so the
\ file's own code is found by OWN-CODE.
: XT-TARGET ( n -- n bool )
   {: v:n :}
   v 0= if 0 true exit then
   v OWN-CODE {: row:n addend:n :}
   row 0 >= if NFILE-REF:CODE row addend NFILE-REF:REF-LOCAL true exit then
   v NFILE-REF:IMPL-OWNER {: ord:n found:bool :}
   found 0= if 0 false exit then
   ord NUNIT-DEP:BIND-ORD
   v NFILE-REF:REF-CODE true ;

\ DATA address va is the end of a closing file's segment that a child load
\ follows: the child's first DATA starts there too, or, when it allotted
\ none, the file's next segment or its here after its last load. A
\ segment closes only at a load (FILE-BEGIN) or the file's end, so a LOAD row
\ lies between any two SEGMENT rows and each LOAD row tests the end of the
\ segment before it.
: LOAD-BOUNDARY? ( n -- bool )
   {: va:n :}
   0
   EVENTS-N @ FO-EVENTS FO @ ?do
      {: end:n :}
      i SEG? if i 3 EV @ i 4 EV @ + else
         i 0 EV @ EV-LOAD = va end = and if unloop true exit then
         end
      then
   loop drop false ;

\ A data cell's value as a reference, and whether it has one: a literal
\ body by its bytes, the file's own DATA, or an earlier declaration's. The
\ file's DATA ends where a child it loads starts, and its reference would
\ name the file's DATA after the load, or its here after its last load, so
\ such a value refuses the file.
: DATA-TARGET ( n -- n bool )
   {: v:n :}
   v 0= if 0 true exit then
   v OWNER-ROW? {: body:ptr size:n found:bool :}
   found if
      body size LIT-ROW-OF v body NULL-PTR BYTE-VIEW - -
         NFILE-REF:REF-LITERAL true exit
   then
   v DATA-REF {: own:n :}
   own 0<> if
      v LOAD-BOUNDARY? if s" slot-boundary" FILE-REFUSE then
      own true exit
   then
   v EARLIER-REF {: ref:n :}
   ref ref 0<> ;

\ An earlier file's address cell at site as a reference through the
\ declaration whose DATA starts at it, 0 when no keyed declaration owns it.
\ A site inside a declaration's DATA refuses the file: only the record that
\ declared the cell names it.
: SLOT-SITE ( n -- n )
   {: site:n :}
   site AOT-CELL:DATA-ADDRESS? 0= if 0 exit then
   site OWNER-AT {: ord:n base:n :}
   ord 0 >= base site <> and if s" slot-site-offset" FILE-REFUSE 0 exit then
   site ord base REF-AT ;

\ One SLOT-BYTES row for the declaration raw, which the segment at slot 12
\ row seg declared. A cell in the file's own DATA holds the value its
\ segment's close left, which its DATA section carries, and is zeroed there,
\ since the row restores it.
: SLOT-ROW ( n n -- )
   {: raw:n seg:n :}
   raw SNAP-RELOC:XTCELL-OFF-MASK and DATA-VA VA>N + {: site:n :}
   raw SNAP-RELOC:XTCELL-DATA-TAG and 0<> {: data:bool :}
   site CELL OWN-OFF {: off:n :}
   off 0 >= if DATA-AT @ off + FILE-OUT LE:U64@
   else site DATA-PTR LE:U64@ then {: value:n :}
   off 0 >= if NFILE-REF:DATA off 0 NFILE-REF:REF-LOCAL
   else site SLOT-SITE then {: sref:n :}
   FILE-REFUSED? if exit then
   sref 0= if s" slot-site-unowned " WHY+ site WHY-SITE exit then
   data if value DATA-TARGET else value XT-TARGET then {: tref:n known:bool :}
   FILE-REFUSED? if exit then
   known 0= if site value DECLINE-CELL exit then
   sref OUT-N data if SLOT-DATA else SLOT-XT then OUT-N tref OUT-N seg OUT-N
   off 0 >= if 0 DATA-AT @ off + FILE-OUT LE:U64! then ;

\ The rows each of the closing file's segments declared. A segment whose
\ first or last row no longer holds the value it closed with lost a row to a
\ removal, so the file refuses.
: SLOTS-OUT ( -- )
   ADDRESS-CELLS:LIVE-SPAN drop {: rows:ptr :}
   EVENTS-N @ FO-EVENTS FO @ ?do
      i SEG? if
         i FO-EVENTS FO @ - {: seg:n :}
         i 9 EV @ {: first:n :}
         i 10 EV @ {: held:n :}
         held 0 > if
            first i 11 EV @ ROW-HOLDS?
            first held + 1- i 12 EV @ ROW-HOLDS? and 0= if
               s" slots-rewound" FILE-REFUSE unloop exit
            then
            first held + first ?do
               rows i cells + @ seg SLOT-ROW
               FILE-REFUSED? if unloop unloop exit then
            loop
         then
      then
   loop ;

\ The checker owner's storage cell producer, and whether it has one.
: CHECKER-CELLS ( -- n bool )
   DEF-OWNER {: owner:ptr :}
   owner NULL-PTR = if 0 false exit then
   owner 16 - CELL-VIEW @ CHECKER-OWNER-ABI:MAGIC <> if 0 false exit then
   owner 8 - CELL-VIEW @ CHECKER-OWNER-ABI:UNIT-STORAGE-CELLS-OFF CELL + <
      if 0 false exit then
   owner CHECKER-OWNER-ABI:UNIT-STORAGE-CELLS-OFF + CELL-VIEW @
   dup 0<> ;

\ The byte at va as the closing file's DATA section holds it, slot cells
\ zeroed, when one of its segments allotted va; else the live byte.
: SEG-BYTE ( n -- n )
   {: va:n :}
   0 EVENTS-N @ FO-EVENTS FO @ ?do
      {: base:n :}
      i SEG? if
         va i 3 EV @ >= va i 3 EV @ i 4 EV @ + < and if
            DATA-AT @ base + va + i 3 EV @ - FILE-OUT c@ unloop exit
         then
         base i 4 EV @ +
      else base then
   loop drop va DATA-PTR c@ ;

\ The little-endian cell at va, byte by byte through SEG-BYTE.
: WINDOW@ ( n -- n )
   {: va:n :}
   0 CELL 0 ?do 8 lshift va CELL 1- + i - SEG-BYTE or loop ;

\ The cell window at holds value: a DATA address there that no owner
\ declares pins its owner, and a code or mapped one refuses the file.
: ADMIT-AT ( n n -- )
   {: at:n value:n :}
   value AOT-CELL:DATA-ADDRESS? {: data:bool :}
   value AOT-CELL:TEXT-PTR? value AOT-CELL:MAPPED? or data or 0= if exit then
   at AOT-CELL:NONCELL-WINDOW? if exit then
   at [: LIT-HIT? ;] AOT-CELL:LITERAL-OVERLAP? if exit then
   data if at value PIN else at value DECLINE-CELL then ;

\ Every cell window reaching into the segment at va, bytes long, whose DATA
\ starts at base in the file's DATA section: from the window starting CELL-1
\ bytes before it to the last inside it. A code or mapped address there that
\ no owner declares refuses the file.
: ADMIT-SPAN ( n n n -- )
   {: va:n bytes:n base:n :}
   va CELL 1- - DATA-VA VA>N max {: from:n :}
   va bytes + CELL - 1+ from max from ?do
      i {: at:n :}
      at va >= if DATA-AT @ base + at + va - FILE-OUT LE:U64@
      else at WINDOW@ then {: value:n :}
      at value ADMIT-AT
      FILE-REFUSED? if unloop exit then
   loop ;

\ Every whole cell holding an earlier DATA byte the closing file updates,
\ each once while the bytes ascend: the cell at the cell-aligned offset in
\ the declaration that owns the byte (SITE-OWNERS), never a window across
\ two cells.
: ADMIT-UPDATES ( -- )
   -1 SITES-N @ FO-SITES FO @ ?do
      {: last:n :}
      i FILE-SITES @ {: va:n :}
      va 0= if last else
         i FO-SITES FO @ - 2 * 1+ SITE-OWNS @ {: base:n :}
         va base - CELL / CELL * base + {: at:n :}
         at last <> if
            \ An own site's update carries the live byte, past its segment's.
            va 1 OWN-OFF 0 >= if at DATA-PTR LE:U64@ else at WINDOW@ then
               {: value:n :}
            at value ADMIT-AT
            FILE-REFUSED? if unloop exit then
         then
         at
      then
   loop drop ;

: PX ( ptr u8 n -- n ) NUNIT-CAPTURE:OPTIONAL-PRODUCER-XT ;

CAST: OWNER-PAIR-FN ( n -- [ [ ptr u8 n -- ] [ ptr u8 n -- ] -- ] )
CAST: OWNER-CELL-FN ( n -- [ [ ptr u8 -- ] -- ] )

\ Keep span p, size bytes, as one a storage owner restores itself.
: OWNER+ ( ptr u8 n -- )
   {: p:ptr size:n :}
   size 0= if exit then
   p NULL-PTR BYTE-VIEW - {: base:n :}
   OWNER-N @ 1+ 2 * OWNER-SPANS-RESERVE
   base OWNER-N @ 2 * OWNER-SPANS !
   base size + OWNER-N @ 2 * 1+ OWNER-SPANS !
   1 OWNER-N +! ;

: CELL-OWNER ( n -- )
   {: xt:n :}
   xt 0= if exit then
   [: OWNER+ ;] xt CELL-OWNERS-FN execute ;

: PAIR-OWNER ( n -- )
   {: xt:n :}
   xt 0= if exit then
   [: OWNER+ ;] [: OWNER+ ;] xt OWNER-PAIR-FN execute ;

: ONE-CELL-OWNER ( n -- )
   {: xt:n :}
   xt 0= if exit then
   [: CELL OWNER+ ;] xt OWNER-CELL-FN execute ;

\ The spans the storage owners restore themselves, each owner the session's
\ bound producer. An absent owner excludes nothing, so a write there stays an
\ update. The checker's storage is program state: a later file's store into
\ it is an update or, in an address cell, a slot row (EARLIER-SLOTS).
: OWNERS-GATHER ( -- )
   0 OWNER-N !
   s" NSTR:CELL-SPANS" PX CELL-OWNER
   s" FFI:SCAN-SPANS" PX PAIR-OWNER
   s" FFI-DECL:SCAN-SPANS" PX PAIR-OWNER
   s" FFI-CB:SCAN-SPANS" PX PAIR-OWNER
   s" TASK:OWNED-CELLS" PX PAIR-OWNER
   s" TASK:OWNED-CALLBACK-ROWS" PX CELL-OWNER
   s" DYNAMIC-STORAGE:OWNED-CELLS" PX ONE-CELL-OWNER
   s" REQUIRE-REG:CELL-SPANS" PX CELL-OWNER ;

\ DATA address va lies in a storage owner's span.
: OWNED? ( n -- bool )
   {: va:n :}
   OWNER-N @ 0 ?do
      va i 2 * OWNER-SPANS @ >= va i 2 * 1+ OWNER-SPANS @ < and if
         true unloop exit
      then
   loop false ;

\ The live address cell row whose cell holds va, -1 when none does.
: CELL-ROW ( n -- n )
   {: va:n :}
   ADDRESS-CELLS:LIVE-SPAN {: rows:ptr count:n :}
   count 0 ?do
      rows i cells + @ SNAP-RELOC:XTCELL-OFF-MASK and DATA-VA VA>N +
         {: site:n :}
      va site >= va site CELL + < and if i unloop exit then
   loop -1 ;

\ Address cell row idx is one the closing file's segments declared.
: OWN-ROW? ( n -- bool )
   {: idx:n :}
   EVENTS-N @ FO-EVENTS FO @ ?do
      i SEG? if
         idx i 9 EV @ >= idx i 9 EV @ i 10 EV @ + < and if
            true unloop exit
         then
      then
   loop false ;

\ Address cell row idx already has an earlier-DATA slot row.
: SLOTW? ( n -- bool )
   {: idx:n :}
   SLOTW-N @ 0 ?do
      i FILE-SLOTW @ idx = if true unloop exit then
   loop false ;

: SLOTW+ ( n -- )
   {: idx:n :}
   SLOTW-N @ 1+ FILE-SLOTW-RESERVE
   idx SLOTW-N @ FILE-SLOTW !
   1 SLOTW-N +! ;

\ Drop each changed earlier DATA site that lies in an excluded owner's span,
\ which holds no program state. A site in the closing file's own DATA is a
\ later segment's write into an earlier one, whose bytes and slot cells the
\ object carries as that segment's close left them: an update. A changed
\ address cell another file declared is a slot row, once per cell; the
\ remaining sites are updates.
: EARLIER-SLOTS ( -- )
   0 SLOTW-N !
   OWNERS-GATHER
   SITES-N @ FO-SITES FO @ ?do
      i FILE-SITES @ {: va:n :}
      va 1 OWN-OFF 0 < if va OWNED? if
         0 i FILE-SITES !
      else
         va CELL-ROW {: row:n :}
         row 0 >= if
            0 i FILE-SITES !
            row OWN-ROW? 0= row SLOTW? 0= and if
               row SLOTW+
               ADDRESS-CELLS:LIVE-SPAN drop row cells + @ NO-SEGMENT SLOT-ROW
               FILE-REFUSED? if unloop exit then
            then
         then
      then then
   loop ;

\ One UPDATE-BYTES row per remaining site: its reference into the file's own
\ DATA or through the declaration that owns it, and its byte now. A site no
\ keyed declaration owns refuses the file.
: UPDATES-OUT ( -- )
   SITES-N @ FO-SITES FO @ ?do
      i FILE-SITES @ {: va:n :}
      va 0<> if
         i FO-SITES FO @ - 2 * {: at:n :}
         va 1 OWN-OFF 0 >= if va DATA-REF
         else va at SITE-OWNS @ at 1+ SITE-OWNS @ REF-AT then {: ref:n :}
         ref 0= if s" update-unowned " WHY+ va WHY-SITE unloop exit then
         ref OUT-N va DATA-PTR c@ OUT-N
      then
   loop ;

\ Refuse the closing file when its DATA, slot cells zeroed, or a cell holding
\ a byte it updates, holds a code or mapped address no storage owner
\ declares; pin the owner of each DATA address such a cell holds.
: ADMIT-FILE ( -- )
   CHECKER-CELLS {: checker:n known:bool :}
   s" NSTR:CELL-SPANS" PX {: lit:n :}
   s" FFI:SCAN-SPANS" PX {: ffi:n :}
   s" FFI-DECL:SCAN-SPANS" PX {: decl:n :}
   s" FFI:FORGET-CACHE" PX {: forget:n :}
   known 0= lit 0= or ffi 0= or decl 0= or forget 0= or
   OWNER-ROW-XT @ 0= or if
      s" cell-owners-absent" FILE-REFUSE exit
   then
   DATA-AT @ REQS-AT @ = UPDS-AT @ PINS-AT @ = and if exit then
   \ The map read resolves foreign symbols; clear that cache afterward so
   \ admission does not mistake its own newly owned pointer for source DATA.
   PROC-MAPS:RELOAD
   forget DEF-ACTION-XT execute
   AOT-ARM:B0 @ checker CELL-OWNERS-FN lit ffi decl
      s" TASK:OWNED-CALLBACK-ROWS" PX s" FFI-CB:SCAN-SPANS" PX
      AOT-CELL:PREPARE-WITH-OWNERS
   0 EVENTS-N @ FO-EVENTS FO @ ?do
      {: base:n :}
      i SEG? if
         i 3 EV @ i 4 EV @ base ADMIT-SPAN
         base i 4 EV @ +
      else base then
      FILE-REFUSED? if drop 0 leave then
   loop drop
   FILE-REFUSED? 0= if ADMIT-UPDATES then
   AOT-CELL:RESET ;

public

\ Encode the innermost file's records, DATA, requires, protection bytes,
\ events, slots, slot literals, checker slices, updates and pins into
\ FILE-OUT, closing its last segment. A file with a load that was not clean,
\ that lacks a checker slice per load, whose records, slots, checker
\ references or updates cannot all encode, or whose DATA or updated cells
\ hold an undeclared address, refuses.
: FILE-ENCODE ( -- )
   FILE-OPEN? 0= if BAD then
   SEGMENT-CLOSE
   CHECK-EXPORT
   0 EVENTS-N @ FO-EVENTS FO @ ?do
      i 0 EV @ EV-LOAD = if
         i 9 EV @ 1 <> if s" load-boundary" FILE-REFUSE then
         1+
      then
   loop {: loads:n :}
   FILE-REFUSED? if exit then
   FO-SLICES FO @ loads 1+ <> if s" checker-slices" FILE-REFUSE exit then
   NFILE-REF:BEGIN-FILE-REFS
   0 OUT-U !
   FO-INDEX FO @ INDEX-BYTES / ROW-AT !
   0 VALS-U !
   0 RECS-AT ! RECORDS-OUT
   OUT-U @ DATA-AT ! DATA-OUT
   OUT-U @ REQS-AT ! REQS-OUT
   OUT-U @ PROT-AT ! PROT-OUT
   OUT-U @ EVS-AT !
   EVENTS-N @ FO-EVENTS FO @ ?do i EVENT-OUT loop
   s" NSTR:OWNER-ROW" PX OWNER-ROW-XT !
   0 LITS-U ! 0 LITS-N !
   OUT-U @ SLOTS-AT ! SLOTS-OUT
   FILE-REFUSED? 0= if SITE-OWNERS EARLIER-SLOTS then
   OUT-U @ LITS-AT !
   LITS-U @ 0<> if 0 FILE-LITS LITS-U @ OUT-S then
   OUT-U @ CHECK-AT !
   FILE-REFUSED? if exit then
   CHECK-OUT
   OUT-U @ UPDS-AT !
   FILE-REFUSED? 0= if UPDATES-OUT then
   OUT-U @ PINS-AT ! 0 PINS-N !
   FILE-REFUSED? 0= if ADMIT-FILE then ;

\ The innermost file's object bytes with its dependency rows deps, after
\ FILE-ENCODE.
: PACK-FILE ( ptr u8 n ptr u8 n -- ptr u8 n )
   {: key:ptr keyu:n deps:ptr depsu:n :}
   FILE-REFUSED? if BAD then
   NUNIT-FILE:RESET-KEEP
   NUNIT-FILE:SECTION-COUNT 0 ?do i NULL-PTR 0 NUNIT-FILE:SECTION! loop
   0 DEFS$ NUNIT-FILE:SECTION!
   1 RECS-AT @ DATA-AT @ OUT$ NUNIT-FILE:SECTION!
   2 INDEX$ NUNIT-FILE:SECTION!
   3 NAMES$ NUNIT-FILE:SECTION!
   4 PROT-AT @ EVS-AT @ OUT$ NUNIT-FILE:SECTION!
   6 DATA-AT @ REQS-AT @ OUT$ NUNIT-FILE:SECTION!
   8 REQS-AT @ PROT-AT @ OUT$ NUNIT-FILE:SECTION!
   11 INPUTS$ NUNIT-FILE:SECTION!
   18 ORIGIN$ NUNIT-FILE:SECTION!
   19 OUTPUT$ NUNIT-FILE:SECTION!
   9 SLOTS-AT @ LITS-AT @ OUT$ NUNIT-FILE:SECTION!
   10 LITS-AT @ CHECK-AT @ OUT$ NUNIT-FILE:SECTION!
   5 CHECK-AT @ UPDS-AT @ OUT$ NUNIT-FILE:SECTION!
   7 UPDS-AT @ PINS-AT @ OUT$ NUNIT-FILE:SECTION!
   15 PINS-AT @ OUT-U @ OUT$ NUNIT-FILE:SECTION!
   12 EVS-AT @ SLOTS-AT @ OUT$ NUNIT-FILE:SECTION!
   13 0 0 NFILE-REF:ENCODE NUNIT-FILE:SECTION!
   DEPS-SLOT deps depsu NUNIT-FILE:SECTION!
   16 NFILE-SCOPE:SCOPE-INPUT$ NUNIT-FILE:SECTION!
   17 VALS$ NUNIT-FILE:SECTION!
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

\ Whether a cut to kept drops a row of open file k: its last, as rows follow
\ ordinal order.
: ROWS-CUT? ( n n -- bool ) {: kept:n k:n :}
   k EV-HI {: hi:n :}
   hi k FO-EVENTS FRAME @ > 0= if false exit then
   hi 1- 1 EV @ kept < 0= ;

\ Whether a cut to kept reaches below the innermost file's open segment or
\ drops an event row, such as the load of an open child whose first ordinal
\ is the cut.
: CUT-BELOW? ( n -- bool ) {: kept:n :}
   kept FO-SEG-ORD FO @ < if true exit then
   EVENTS-N @ 0= if false exit then
   EVENTS-N @ 1- 1 EV @ kept < 0= ;

public

\ The kernel's capture cut notice: the dictionary count drops to kept, which
\ retires every definition at or past it and gives its slot to the next
\ definition. Index and event rows follow ordinal order across the open files,
\ so the retired ones are a suffix; drop it and pull back each frame's marks
\ the cut crosses, its first ordinal and open segment included. Only a cut
\ within the innermost file's open segment keeps the objects whole: any other
\ refuses the innermost file and each open file whose rows it drops. A
\ SEGMENT row the cut crosses stays as it was: its file's LOAD row at the
\ segment's end follows it (FILE-BEGIN), so its file refuses.
: FILE-CUT ( n -- )
   {: kept:n :}
   FILE-OPEN? if kept CUT-BELOW? if
      s" cut-below-segment" FILE-REFUSE
      FO-DEPTH @ 0 ?do
         kept i ROWS-CUT? if s" cut-below-segment" i FRAME-REFUSE then
      loop
   then then
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
   FO-DEPTH @ 0 ?do
      i FO-INDEX FRAME @ INDEX-U @ min i FO-INDEX FRAME !
      i FO-DEFS FRAME @ DEFS-U @ min i FO-DEFS FRAME !
      i FO-FIRST FRAME @ kept min i FO-FIRST FRAME !
      i FO-EVENTS FRAME @ EVENTS-N @ min i FO-EVENTS FRAME !
      i FO-SEG-ORD FRAME @ kept min i FO-SEG-ORD FRAME !
      i FO-ACT-ROW FRAME @ EVENTS-N @ >= if
         ACT-NONE i FO-ACT-ROW FRAME !
      then
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

\ Reference ref names a row of kind in the reference section refs; a LOCAL
\ row names the file's own item at most max, with no addend.
: REF-ROW-CK ( ptr u8 n n n -- )
   {: refs:ptr ref:n kind:n max:n :}
   ref refs 0 U64-AT REF-CK? 0= if BAD then
   refs 24 + ref 1- 32 * + {: row:ptr :}
   row 0 U64-AT kind <> if BAD then
   row 1 U64-AT NFILE-REF:LOCAL <> if exit then
   row 2 U64-AT {: item:n :}
   item 0 < item max > or row 3 U64-AT 0<> or if BAD then ;

\ A SEGMENT event row starting at or past end, the previous segment's last
\ ordinal, with a dependency row mark no lower than last, the previous
\ segment's, and within the file's rows; the new end and mark. The first
\ segment's remainder is the object's.
: SEGMENT-CK ( ptr u8 n n -- n n )
   {: row:ptr end:n last:n :}
   row 1 U64-AT {: first:n :}
   row 2 U64-AT {: count:n :}
   row 3 U64-AT {: bytes:n :}
   row 4 U64-AT {: reqs:n :}
   row 5 U64-AT {: wids:n :}
   row 6 U64-AT {: rem:n :}
   row 7 U64-AT {: mark:n :}
   first end < count 0 < or bytes 0 < or reqs 0 < or wids 0 < or if BAD then
   rem 0 < rem 1 cells >= or if BAD then
   mark last < mark OPEN-ROWS @ > or if BAD then
   OPEN-SEGS @ 0= if rem OPEN-REM ! then
   count OPEN-RECS +! bytes OPEN-DATA +! reqs OPEN-REQS +!
   wids OPEN-WIDS +! 1 OPEN-SEGS +!
   first count + mark ;

\ A LOAD event row: an include or a require, where the loading file's source
\ stood, the child's path and root in the names section, a clean load, a
\ resume offset within the file's srcu source bytes, and dependency rows and
\ input bytes, both no fewer than the previous LOAD row's, within their
\ sections. The input rows since the previous LOAD row are whole rows.
: LOAD-CK ( ptr u8 n n n n -- n n )
   {: row:ptr namesu:n srcu:n last-rows:n last-in:n :}
   row 1 U64-AT {: kind:n :}
   kind SOURCE-UNIT:LOAD-INCLUDE <> kind SOURCE-UNIT:LOAD-REQUIRE <> and
      if BAD then
   row 2 U64-AT 0 < if BAD then
   row 4 U64-AT 0= if BAD then
   row 3 U64-AT row 4 U64-AT namesu SPAN-CK
   row 5 U64-AT row 6 U64-AT namesu SPAN-CK
   row 7 U64-AT 1 <> if BAD then
   row 8 U64-AT {: resume:n :}
   resume 0 < resume srcu > or if BAD then
   row 9 U64-AT {: rows:n :}
   rows last-rows < rows OPEN-ROWS @ > or if BAD then
   row 10 U64-AT {: inputs:n :}
   11 NUNIT-FILE:SECTION$ {: ins:ptr insu:n :}
   inputs last-in < inputs insu > or if BAD then
   ins last-in + inputs last-in - NFILE-INPUTS:ROW-COUNT drop
   1 OPEN-LOADS +!
   rows inputs ;

\ An FFI row after a segment: a library path or symbol a span of the names
\ section without a 0 byte, both of which the engine refuses, counts within
\ its limits, and a scope row's wordlist a reference the file names.
: FFI-CK ( ptr u8 ptr u8 n ptr u8 -- )
   {: row:ptr names:ptr namesu:n refs:ptr :}
   FFI-BOUND? 0= if BAD then
   1 OPEN-FFI +!
   row 0 U64-AT {: kind:n :}
   kind EV-FFI-SCOPE = if
      row 1 U64-AT {: role:n :}
      row 4 U64-AT {: stated:n :}
      role 0 < role 1 > or stated 0 < or stated 1 > or if BAD then
      refs row 3 U64-AT NFILE-REF:WID OPEN-WIDS @ 1- REF-ROW-CK
      row 5 U64-AT row 6 U64-AT or 0<> if BAD then exit
   then
   row 1 U64-AT {: off:n :}
   row 2 U64-AT {: len:n :}
   off len namesu SPAN-CK
   len 0 ?do names off + i + c@ 0= if BAD then loop
   kind EV-FFI-LIB = if
      \ FFI's PATH-ROOM: a path of 1 to PATH-CAP bytes.
      len 1 < len PATH-CAP > or if BAD then
      row 4 U64-AT row 5 U64-AT or row 6 U64-AT or 0<> if BAD then exit
   then
   len 1 < len s" FFI:SYMBOL-CAP" FFI-N > or if BAD then
   s" FFI:MAX-ARGS" FFI-N {: cap:n :}
   row 4 U64-AT {: argc:n :}
   row 5 U64-AT {: spills:n :}
   argc 0 < argc cap > or spills 0 < or spills cap > or if BAD then ;

: EVENTS-CK ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: evs:ptr evsu:n names:ptr namesu:n refs:ptr srcu:n :}
   evsu EVENT-BYTES mod 0<> if BAD then
   0 0 0 0 EV-LOAD evsu EVENT-BYTES / 0 ?do
      {: end:n mark:n rows:n inputs:n prev:n :}
      evs i EVENT-BYTES * + {: row:ptr :}
      row 0 U64-AT {: kind:n :}
      kind EV-SEGMENT <> kind EV-LOAD <> and kind EV-TASK <> and
         row 7 U64-AT 0<> and if BAD then
      kind EV-SEGMENT = if row end mark SEGMENT-CK rows inputs else
         kind EV-LOAD = if end mark row namesu srcu rows inputs LOAD-CK else
            kind EV-OUT = if end mark rows inputs else
               \ FFI rows follow their segment's row, and a TASK row the
               \ last of them.
               prev EV-LOAD = prev EV-TASK = or if BAD then
               kind EV-TASK <> if
                  kind EV-FFI-LIB < kind EV-FFI-SCOPE > or if BAD then
                  row names namesu refs FFI-CK
               then end mark rows inputs
            then
         then
      then
      kind
   loop 2drop 2drop drop ;

\ Reference ref of the reference section refs is the file's own wordlist item.
: LOCAL-WID? ( ptr u8 n n -- bool )
   {: refs:ptr ref:n item:n :}
   refs 24 + ref 1- 32 * + {: row:ptr :}
   row 1 U64-AT NFILE-REF:LOCAL = row 2 U64-AT item = and ;

\ A package row whose checked wordlist references value and priv: as
\ namespace-record makes them, the file's next wordlist and the one after.
: OWNED+ ( ptr u8 n n -- )
   {: refs:ptr value:n priv:n :}
   OPEN-OWNED @ {: next:n :}
   next 0 < if exit then
   refs value next LOCAL-WID? refs priv next 1+ LOCAL-WID? and
   if next 2 + else -1 then OPEN-OWNED ! ;

\ A constant row's slot 17 entry at offset at: it starts where the row
\ before left off, and its recorded length is a valid code span whose
\ bytes, whole instructions, follow within the slot. NPUB:IMPORT-PENDING
\ refuses anything else, so an install never throws on it.
: VAL-CK ( n -- )
   {: at:n :}
   17 NUNIT-FILE:SECTION$ {: vals:ptr valsu:n :}
   at OPEN-VALS @ <> at 8 + valsu > or if BAD then
   vals at + LE:U64@ {: recorded:n :}
   recorded CODE-SPAN:VALID? 0= if BAD then
   recorded CODE-SPAN:BYTES {: size:n :}
   size 0 <= size 3 and 0<> or if BAD then
   size valsu at - 8 - > if BAD then
   at 8 + size + OPEN-VALS ! ;

\ One record row: a known kind, its name in the names section, and the
\ references, body and index row its kind names: its wordlists the file's
\ own or earlier, a constant's body in slot 17, an address a place in its own
\ segment's DATA, its end included, at or past the address before it, a
\ does> word's the same with its definer's record, a deferred word's cell a
\ place in the file's DATA, a clause row nothing past its name, and no value
\ for a record with no body of its own.
: RECORD-CK ( ptr u8 n ptr u8 n -- )
   {: row:ptr namesu:n refs:ptr defs:n :}
   row 0 U64-AT {: kind:n :}
   row 4 U64-AT {: wl:n :}
   row 5 U64-AT {: value:n :}
   row 6 U64-AT {: priv:n :}
   kind RK-VAL < kind RK-DOES-WORD > or if BAD then
   1 kind lshift OPEN-KINDS @ or OPEN-KINDS !
   row 2 U64-AT row 3 U64-AT namesu SPAN-CK
   OPEN-WIDS @ 1- {: wmax:n :}
   kind RK-PACKAGE = if
      wl 0<> if BAD then
      refs value NFILE-REF:WID wmax REF-ROW-CK
      refs priv NFILE-REF:WID wmax REF-ROW-CK
      refs value priv OWNED+ exit
   then
   refs wl NFILE-REF:WID wmax REF-ROW-CK
   kind RK-DOES-WORD = if
      refs priv NFILE-REF:RECORD OPEN-RECS @ 1- REF-ROW-CK
   else priv 0<> if BAD then then
   kind RK-VAL = if value VAL-CK exit then
   kind RK-ADDR = kind RK-DOES-WORD = or if
      refs value NFILE-REF:DATA OPEN-SEG-END @ REF-ROW-CK
      refs 24 + value 1- 32 * + {: ref:ptr :}
      ref 1 U64-AT NFILE-REF:LOCAL <> if BAD then
      ref 2 U64-AT {: item:n :}
      item OPEN-ADDR @ < if BAD then
      item OPEN-ADDR ! exit
   then
   kind RK-DEFER = if
      refs value NFILE-REF:DATA OPEN-DATA @ REF-ROW-CK exit
   then
   kind RK-COLON = if value 0 < value defs >= or if BAD then exit then
   kind RK-RETIRED >= kind RK-CLAUSE <= and value 0<> and if BAD then ;

\ The reference section: an NFILE-REF ENCODE header with no declarations
\ and room for its reference rows; their count.
: REFS-CK ( ptr u8 n -- n )
   {: refs:ptr refsu:n :}
   refsu 24 < if BAD then
   refs 2 U64-AT 0<> if BAD then
   refs 0 U64-AT {: cnt:n :}
   cnt 0 < cnt refsu 24 - 32 / > or if BAD then
   cnt ;

\ Each segment's record rows, in the order of the SEGMENT rows EVENTS-CK
\ counted, with the span of that segment's DATA in the file's.
: RECORDS-CK ( ptr u8 n n ptr u8 n -- )
   {: recs:ptr recsu:n namesu:n refs:ptr defs:n :}
   recsu OPEN-RECS @ RECORD-BYTES * <> if BAD then
   12 NUNIT-FILE:SECTION$ {: evs:ptr evsu:n :}
   0 0
   evsu EVENT-BYTES / 0 ?do
      {: rec:n at:n :}
      evs i EVENT-BYTES * + {: ev:ptr :}
      ev 0 U64-AT EV-SEGMENT = if
         ev 2 U64-AT {: count:n :}
         at OPEN-ADDR !
         at ev 3 U64-AT + OPEN-SEG-END !
         count 0 ?do
            recs rec i + RECORD-BYTES * + namesu refs defs RECORD-CK
         loop
         rec count + OPEN-SEG-END @
      else rec at then
   loop 2drop ;

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

\ The slot literal rows, back to back, each its own first row.
: SLOT-LITS-CK ( ptr u8 n -- )
   {: lits:ptr litsu:n :}
   0 0 begin over litsu < while
      {: off:n row:n :}
      lits litsu off row LIT-ROW {: bytes:n first:n :}
      first row <> if BAD then
      off 16 + bytes + row 1+
   repeat 2drop ;

\ Each slot row: a cell reference, a known kind and its value's reference or
\ 0, given refs reference rows.
: SLOTS-CK ( ptr u8 n n -- )
   {: slots:ptr slotsu:n refs:n :}
   slotsu SLOT-BYTES mod 0<> if BAD then
   slotsu SLOT-BYTES / {: count:n :}
   count 0 ?do
      slots i SLOT-BYTES * + {: row:ptr :}
      row 0 U64-AT refs REF-CK? 0= if BAD then
      row 1 U64-AT {: kind:n :}
      kind SLOT-XT <> kind SLOT-DATA <> and if BAD then
      row 2 U64-AT {: target:n :}
      target 0<> target refs REF-CK? 0= and if BAD then
   loop
   count OPEN-SLOTS ! ;

\ Each update row: a byte's reference and its value, given refs reference
\ rows.
: UPDATES-CK ( ptr u8 n n -- )
   {: upds:ptr updsu:n refs:n :}
   updsu UPDATE-BYTES mod 0<> if BAD then
   updsu UPDATE-BYTES / {: count:n :}
   count 0 ?do
      upds i UPDATE-BYTES * + {: row:ptr :}
      row 0 U64-AT refs REF-CK? 0= if BAD then
      row 1 U64-AT {: byte:n :}
      byte 0 < byte 255 > or if BAD then
   loop
   count OPEN-UPDS ! ;

\ Each pin row: an owner's reference, given refs reference rows, then its
\ DATA base at capture, which only an install compares.
: PINS-CK ( ptr u8 n n -- )
   {: pins:ptr pinsu:n refs:n :}
   pinsu PIN-BYTES mod 0<> if BAD then
   pinsu PIN-BYTES / {: count:n :}
   count 0 ?do
      pins i PIN-BYTES * + 0 U64-AT refs REF-CK? 0= if BAD then
   loop
   count OPEN-PINS ! ;

\ The checker section: one slice per load and one for the file, each
\ reference one of refs reference rows, and slices of at least two cells,
\ zero padded to a cell, covering the section exactly.
: CHECKS-CK ( ptr u8 n n -- )
   {: sec:ptr secu:n refs:n :}
   secu 16 < if BAD then
   sec 0 U64-AT {: slices:n :}
   sec 1 U64-AT {: nrefs:n :}
   slices OPEN-LOADS @ 1+ <> if BAD then
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

\ off and u frame bytes within size bytes.
: SPAN-IN? ( n n n -- bool )
   {: off:n u:n size:n :}
   off 0 >= u 0 >= and off size <= and u size off - <= and ;

\ Ordinal ord past the file's first record is one of the viewed object's own
\ records: inside some SEGMENT row's span.
: OWN-ORD? ( n -- bool )
   {: ord:n :}
   12 NUNIT-FILE:SECTION$ {: evs:ptr evsu:n :}
   evsu EVENT-BYTES / 0 ?do
      evs i EVENT-BYTES * + {: row:ptr :}
      row 0 U64-AT EV-SEGMENT = if
         ord row 1 U64-AT >= ord row 1 U64-AT row 2 U64-AT + < and if
            true unloop exit
         then
      then
   loop
   false ;

\ Slot 18's framing: a sink kind, then whole rows, none without a sink, each naming one of the
\ file's records, a name and path within namesu bytes of slot 3, and an
\ ordered span, zero when the row has no path.
: ORIGIN-FRAMED? ( ptr u8 n n -- bool )
   {: sec:ptr secu:n namesu:n :}
   secu 8 < if false exit then
   secu 8 - ORIGIN-BYTES mod 0<> if false exit then
   sec LE:U64@ {: kind:n :}
   kind SINK-NONE < kind SINK-FULL > or if false exit then
   kind SINK-NONE = secu 8 > and if false exit then
   secu 8 - ORIGIN-BYTES / 0 ?do
      sec 8 + i ORIGIN-BYTES * + {: row:ptr :}
      row 0 U64-AT {: ord:n :}
      ord OWN-ORD? 0= if unloop false exit then
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
   sec secu namesu ORIGIN-FRAMED? if
      1 OPEN-ORIGIN-OK !  sec LE:U64@ OPEN-SINK !
   else 0 OPEN-ORIGIN-OK !  SINK-NONE OPEN-SINK ! then ;

\ The FILE object's segment framing: events, records, DATA, protection
\ bytes, requires, slots, updates and pins agree with one another, given its defs
\ index rows and its srcu source bytes.
: SEGMENTS-CK ( n n -- )
   {: defs:n srcu:n :}
   0 OPEN-RECS ! 0 OPEN-DATA ! 0 OPEN-REQS ! 0 OPEN-WIDS !
   0 OPEN-LOADS ! 0 OPEN-SEGS ! 0 OPEN-SLOTS ! 0 OPEN-UPDS ! 0 OPEN-PINS !
   0 OPEN-CHECKS ! 0 OPEN-KINDS ! 0 OPEN-OWNED ! 0 OPEN-VALS !
   0 OPEN-FFI ! 0 OPEN-REM !
   3 NUNIT-FILE:SECTION$ {: names:ptr namesu:n :}
   13 NUNIT-FILE:SECTION$ {: rows:ptr rowsu:n :}
   rows rowsu REFS-CK {: refs:n :}
   12 NUNIT-FILE:SECTION$ names namesu rows srcu EVENTS-CK
   1 NUNIT-FILE:SECTION$ namesu rows defs RECORDS-CK
   17 NUNIT-FILE:SECTION$ nip OPEN-VALS @ <> if BAD then
   6 NUNIT-FILE:SECTION$ nip OPEN-DATA @ <> if BAD then
   4 NUNIT-FILE:SECTION$ PROT-CK
   8 NUNIT-FILE:SECTION$ REQUIRES-CK
   9 NUNIT-FILE:SECTION$ refs SLOTS-CK
   10 NUNIT-FILE:SECTION$ SLOT-LITS-CK
   5 NUNIT-FILE:SECTION$ refs CHECKS-CK
   7 NUNIT-FILE:SECTION$ refs UPDATES-CK
   15 NUNIT-FILE:SECTION$ refs PINS-CK
   18 NUNIT-FILE:SECTION$ namesu ORIGIN-CK ;

\ Install: the hit path. Every engine writer is reached by name through
\ NEED-XT in this block, the install boundary: REC-WIDE and REC-MIN-IN are
\ checked through their casts, the rest are TRUSTED: rows.

\ The engine word name's xt, 0 when this engine lacks it.
: ENGINE-XT ( ptr u8 n -- n )
   XREF-FIND dup XREF-FOUND? 0= if drop 0 exit then
   dup XREF-RETIRED? if drop 0 exit then
   XREF-START dup cp@ >= if drop 0 then ;

: NEED-XT ( ptr u8 n -- n ) ENGINE-XT dup 0= if BAD then ;

CAST: REC-OPEN-XT ( n -- [ ptr u8 n n n -- ] )
CAST: REC-NAMESPACE-XT ( n -- [ ptr u8 n bool -- n ] )
CAST: REC-PROTECT-XT ( n -- [ n -- ] )
CAST: REC-PATCH-XT ( n -- [ n ptr u8 n -- ] )
CAST: REC-MIN-IN-XT ( n -- [ n n -- ] )

\ name, wid and kind: open the pending record at ndict@ (prims.f def-open).
TRUSTED: REC-OPEN ( ptr u8 n n n -- )
   s" def-open" NEED-XT REC-OPEN-XT execute ;
\ End the pending record as the engine's `constant` and `;` end one.
TRUSTED: REC-CLOSE ( -- ) s" def-close" NEED-XT DEF-ACTION-XT execute ;
\ End the pending DKIND:ADDR record as the engine's `create` ends one.
TRUSTED: REC-CREATE ( -- ) s" def-create" NEED-XT DEF-ACTION-XT execute ;
TRUSTED: REC-IMM ( -- ) s" imm-mark" NEED-XT DEF-ACTION-XT execute ;
\ End the pending DKIND:CAST record as the engine's `cast:` ends one.
TRUSTED: REC-CAST ( -- ) s" def-cast" NEED-XT DEF-ACTION-XT execute ;
\ Set DNAME-WIDE on the newest record.
: REC-WIDE ( -- ) s" wide-mark" NEED-XT DEF-ACTION-XT execute ;
\ Record ord and its minimum input arity: or it into DNAME-MIN-IN.
: REC-MIN-IN ( n n -- ) s" min-in-mark" NEED-XT REC-MIN-IN-XT execute ;
\ name and private?: publish a namespace row; its ordinal.
TRUSTED: REC-NAMESPACE ( ptr u8 n bool -- n )
   s" namespace-record" NEED-XT REC-NAMESPACE-XT execute ;
TRUSTED: REC-PROTECT ( n -- ) s" prot-wid-add" NEED-XT REC-PROTECT-XT execute ;
\ Patch the word `create` just ended to run the clause at entry as `does>`
\ does, with no signature: the file's checker slice carries its contract.
TRUSTED: REC-PATCH ( n -- )
   NULL-PTR 0 s" does-patch" NEED-XT REC-PATCH-XT execute ;

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

\ The checker's import callback: reference idx of the slice as the storage
\ the export named, a dictionary record (bytes DREC) or a DATA span. It runs
\ before the install makes the records of the segment the slice follows: a
\ LOCAL record reference is the ordinal the install gives that record (the
\ segment's map row places it at the ndict@ the install takes), and its
\ address is the record slot's by arithmetic (XREF-REC reads nothing). The
\ checker turns it back into that ordinal (UP-READ-LINKS, REC>INDEX) and the
\ plan keeps the ordinal. A LOCAL DATA span is where the segment's map row
\ places the file's DATA, as the install then lays it out.
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

: IMPORT-END ( -- )
   CHECKER-OWNER-ABI:UNIT-PREPARE-END-OFF CHECK-XT DEF-ACTION-XT execute ;

\ Prepare the slice at IMPORT-OFF: its length and bytes.
: PREPARE-RUN ( -- )
   IMPORT-SEC @ {: sec:ptr :}
   IMPORT-OFF @ {: at:n :}
   sec at + LE:U64@ {: len:n :}
   sec at 8 + + len ['] CHECK-RESOLVE
      CHECKER-OWNER-ABI:UNIT-PREPARE-REFS-OFF CHECK-XT CK-PREP-XT execute
   CHECK-PLAN ! ;

\ A begun plan the preparation refuses is ended before the refusal leaves.
: PREPARE-BODY ( -- )
   CHECKER-OWNER-ABI:UNIT-PREPARE-BEGIN-OFF CHECK-XT DEF-ACTION-XT execute
   ['] PREPARE-RUN catch {: rc:n :}
   rc 0<> if IMPORT-END rc throw then ;

: INSTALL-RUN ( -- )
   CHECK-PLAN @ CHECKER-OWNER-ABI:UNIT-INSTALL-OFF CHECK-XT CK-INSTALL-XT
      execute ;

public

\ End the kept checker plan, which will not be installed; nothing when none
\ is kept. The checker's PREPARE-END resets every projection and plan row the
\ preparation set, so the state is the one before PREPARE-SLICE?.
: RELEASE-CHECKER ( -- )
   CHECK-PLAN @ 0 < if exit then
   -1 CHECK-PLAN !
   IMPORT-END ;

private

\ Prepare the viewed object's checker slice k on the state the file's mark
\ saw, before the install side effects of the segment it follows, and keep
\ the plan for INSTALL-CHECKER; true when it prepared. The file's
\ references are prepared as INSTALL-SEGMENT prepares them, through the
\ segment map, which holds the row of the segment the slice follows at the
\ cursors its install takes. A slice the checker refuses (its unit codes, a
\ type-family plan refusal, a FILE dependency whose family contract
\ changed) is false, and a refusal keeps no plan. The object checksum is
\ the integrity boundary: an object that passes it was written by this
\ engine's writer, so the checker's own refusals are misses, while its
\ fail-stop validators (checker.f ASIG-GRAPH-DIE, type-family.f
\ REG-AOT-PARAM?) stay fail-stop for an object altered and resealed.
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
   ['] PREPARE-BODY catch {: rc:n :}
   rc CHECK-CODE? rc FAMILY-CODE? or if false exit then
   rc 0<> if rc throw then
   true ;

\ Install the kept plan on the records INSTALL-SEGMENT made, then end it. A
\ state the checker refuses here is a malformed object.
: INSTALL-BODY ( -- ) ['] INSTALL-RUN ['] RELEASE-CHECKER finally ;

: INSTALL-CHECKER ( -- )
   CHECK-PLAN @ 0 < if BAD then
   ['] INSTALL-BODY catch {: rc:n :}
   rc CHECK-CODE? if BAD then
   rc 0<> if rc throw then ;

public

\ Bind the segment map as the resolver of FILE objects' own references.
: BIND-LOCAL ( -- ) ['] MAP-LOCAL NFILE-REF:LOCAL-AT! ;

\ Install the checker slices from the next one through slice n, the one the
\ file left before its load n, or at its end when n is its load count. Each
\ is the plan kept: slice 0 prepared by PREPARE-CHECKER, a later slice that
\ follows a segment by PREPARE-SEGMENT before that segment installed, and
\ one that follows no segment by PREPARE-CHECKS on the state the loads
\ before it left.
: INSTALL-CHECKS ( n -- )
   {: n:n :}
   INS-SLICE @ n > if exit then
   n 1+ INS-SLICE @ ?do INSTALL-CHECKER loop
   n 1+ INS-SLICE ! ;

\ Prepare checker slice n, one that follows no segment, on the state the
\ loads before it left, and keep its plan for INSTALL-CHECKS: "" when it
\ prepared, else the reason it did not, with no plan kept.
: PREPARE-CHECKS ( n -- ptr u8 n )
   PREPARE-SLICE? if s" " exit then
   s" checker-prepare" ;

private

\ A package row: its namespace record and the wordlists its references name.
\ RECORD-CK requires the private reference.
: INSTALL-PACKAGE ( ptr u8 n n n -- )
   {: name:ptr nameu:n value:n priv:n :}
   name nameu true REC-NAMESPACE XREF-REC {: rec:ptr :}
   rec XREF-PKG-PUBLIC value NFILE-REF:REF>WID <> if BAD then
   rec XREF-PKG-PRIVATE priv NFILE-REF:REF>WID <> if BAD then ;

\ A constant row at slot 17 entry at: its record, and the body its load
\ emitted copied back through NPUB:IMPORT-PENDING. The cold AArch64 load
\ compiles `constant` in the kernel (habu2.f C-CONSTANT, C-LIT): a MOVZ/MOVK
\ chain, a push and a return, position independent, so the copy is that
\ load's code; VAL-ENTRY stored no body with a relocation site, and
\ RECORD-CK and UNSUPPORTED$ checked what the import refuses. It leaves
\ the definition-head scratch (BODYBUF/BODYLEN, DEF-TKA, DEF-TKL, DEF-WL) as
\ the previous head left it. Every head rewrites them before reading; across
\ definitions only `package`'s dictionary-full message reads DEF-TKA/DEF-TKL,
\ and an image saved next keeps that BODYBUF prefix (snap-lib.f
\ SND-ZERO-BODY-TAIL), which the booted image's next head rewrites unread.
: INSTALL-VAL ( ptr u8 n n n -- )
   {: name:ptr nameu:n wl:n at:n :}
   17 NUNIT-FILE:SECTION$ drop at + {: entry:ptr :}
   entry LE:U64@ {: recorded:n :}
   name nameu wl DKIND:VAL REC-OPEN
   entry 8 + recorded CODE-SPAN:BYTES recorded NULL-PTR 0 NULL-PTR 0 false 0
      s" NPUB:IMPORT-PENDING" NUNIT-CAPTURE:OPTIONAL-PRODUCER-XT
      DEF-PUBLISH-XT execute drop
   REC-CLOSE ;

\ Fill the segment's DATA from the cursor up to offset off: the object's
\ bytes copied to here and allotted. No install writer allots DATA between
\ fills, so here stands at the cursor. The segment's bytes start in slot 6
\ where its map row places them.
: DATA-FILL ( n -- )
   {: off:n :}
   INS-DATA-AT @ {: at:n :}
   off at < off INS-DATA-U @ > or if BAD then
   INS-DATA @ at + {: va:n :}
   AOT-ARM:HERE-N va <> if BAD then
   off at - {: u:n :}
   u allot
   6 NUNIT-FILE:SECTION$ drop M-DATA CUR@ + at + va DATA-PTR u BYTE-COPY
   off INS-DATA-AT ! ;

\ An address row: the segment's DATA filled up to its body, then its record
\ ended as `create` ends one, DNAME-CELL kept for a `variable`. def-create
\ rounds DP up to a cell and its code pushes that address, so the body is
\ the one the row names only while DP stays put.
: INSTALL-ADDR ( ptr u8 n n n n -- )
   {: name:ptr nameu:n wl:n value:n flags:n :}
   value NFILE-REF:REF>LOCAL-DATA {: body:n :}
   body INS-DATA @ - DATA-FILL
   name nameu wl DKIND:ADDR flags DNAME-CELL and or REC-OPEN
   REC-CREATE
   AOT-ARM:HERE-N body <> if BAD then ;

\ The body of the viewed FILE object's slot literal row row; SLOT-LITS-CK
\ framed the rows.
: SLOT-LIT$ ( n -- ptr u8 n )
   {: row:n :}
   10 NUNIT-FILE:SECTION$ {: src:ptr size:n :}
   0
   row 0 ?do
      {: off:n :}
      src size off i LIT-ROW drop off 16 + +
   loop {: off:n :}
   src size off row LIT-ROW drop {: bytes:n :}
   src off 16 + + bytes ;

\ Slot row row's value: 0, or its value reference resolved by kind and owner.
\ The file's own code and DATA map through the segments the install made;
\ an earlier record's through its declaration. A literal body is interned
\ into the pool, where OPEN-FILE found room for it, plus the addend.
: SLOT-VALUE ( ptr u8 -- n )
   {: row:ptr :}
   row 2 U64-AT {: ref:n :}
   ref 0= if 0 exit then
   ref NFILE-REF:REF-KIND {: kind:n :}
   ref NFILE-REF:REF-OWNER NFILE-REF:LOCAL = {: local:bool :}
   kind NFILE-REF:CODE = if
      local if ref NFILE-REF:REF>LOCAL-CODE else ref NFILE-REF:REF>CODE then
      exit
   then
   kind NFILE-REF:DATA = if
      local if ref NFILE-REF:REF>LOCAL-DATA exit then
      ref 0 NFILE-REF:REF>DATA NULL-PTR BYTE-VIEW - exit
   then
   ref NFILE-REF:REF>LITERAL {: item:n addend:n :}
   item SLOT-LIT$ DEF-INTERN addend + ;

\ Address cell site and slot kind: declare the cell as the engine's writers
\ do, ptr-cell-mark for a DATA cell, xt! for an xt cell, the only xt
\ declarer, storing the value the cell holds.
TRUSTED: SLOT-DECLARE ( n n -- )
   {: site:n kind:n :}
   kind SLOT-DATA = if site ptr-cell-mark else site @ site xt! then ;

\ value, address cell site and slot kind: store value at site as the
\ engine's writers do, declaring the cell first: ptr-cell-mark then the
\ store for a DATA cell, xt! for an xt cell. Declaring a cell the registry
\ already holds adds no row.
TRUSTED: SLOT-STORE ( n n n -- )
   {: value:n site:n kind:n :}
   kind SLOT-DATA = if
      site ptr-cell-mark
      value site !
   else
      value site xt!
   then ;

\ The address of slot row row's cell: a LOCAL one in the file's DATA as the
\ segments placed it, an EXTERNAL one through the declaration that owns it.
: SLOT-CELL ( ptr u8 -- n )
   {: row:ptr :}
   row 0 U64-AT {: ref:n :}
   ref NFILE-REF:REF-OWNER NFILE-REF:LOCAL = if
      ref NFILE-REF:REF>LOCAL-DATA exit
   then
   ref 0 NFILE-REF:REF>DATA NULL-PTR BYTE-VIEW - ;

\ Install slot row row: its value stored in its cell.
: SLOT-ROW! ( ptr u8 -- )
   {: row:ptr :}
   row SLOT-VALUE row SLOT-CELL row 1 U64-AT SLOT-STORE ;

\ The viewed object's slot row k.
: SLOT-AT ( n -- ptr u8 ) 9 NUNIT-FILE:SECTION$ drop swap SLOT-BYTES * + ;
: SLOT-ROWS ( -- n ) 9 NUNIT-FILE:SECTION$ nip SLOT-BYTES / ;

\ Declare, in registration order, the slot rows the segment at slot 12 row
\ seg declared, as its cold load did: a cell in the file's DATA with its
\ value, an earlier file's cell with the value it holds, since the value
\ the row carries is the one at the file's end (EARLIER-SLOTS!).
: SEGMENT-SLOTS ( n -- )
   {: seg:n :}
   SLOT-ROWS 0 ?do
      i SLOT-AT {: row:ptr :}
      row 3 U64-AT seg = if
         row 0 U64-AT NFILE-REF:REF-OWNER NFILE-REF:LOCAL = if
            row SLOT-ROW!
         else
            row SLOT-CELL row 1 U64-AT SLOT-DECLARE
         then
      then
   loop ;

\ Store, in row order, the slot rows whose cell is an earlier file's: the
\ value it held at the file's end. A segment declared the cell, or an
\ earlier file did.
: EARLIER-SLOTS! ( -- )
   SLOT-ROWS 0 ?do
      i SLOT-AT {: row:ptr :}
      row 0 U64-AT NFILE-REF:REF-OWNER NFILE-REF:EXTERNAL = if row SLOT-ROW! then
   loop ;

\ A does> word: its address row installed, then patched to run its definer's
\ clause, the record after the definer, with the flags the patch set.
: INSTALL-DOES ( n n -- )
   {: priv:n flags:n :}
   priv NFILE-REF:REF>RECORD 1+ XREF-REC XREF-START REC-PATCH
   flags DNAME-WIDE and 0<> if REC-WIDE then
   flags DNAME-MIN-IN-MASK and 52 rshift {: minin:n :}
   minin 0<> if ndict@ 1- minin REC-MIN-IN then ;

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
\ written and OBJECT-FRESH? passed, so one that does not prepare is malformed.
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

\ A colon row and its index row, opened, published and ended as `:` and `;`
\ do. Binding the pending key advances its group's occurrence as a live
\ definition does, and the payload's declaration must match it. The live
\ record binds its result from the payload's sections, the bytes the
\ compiled definition bound from, so its digest, edges and result row are
\ the compiled record's. The payload view and its prepared table replaced
\ the file's, so both are viewed again.
: INSTALL-COLON ( ptr u8 n n n -- )
   {: name:ptr nameu:n wl:n row:n :}
   name nameu wl 0 REC-OPEN
   NFILE-REF:PENDING-KEY$ 2drop
   row PREPARE-FILE-DEFINITION
   INSTALL-FILE-DEFINITION {: ord:n :}
   ord NFILE-REF:PUBLISH-PREPARED
   ord NFILE-REF:BIND-RESULT
   REC-CLOSE
   DEF-DROP-PLAN dup 0<> if throw then drop
   FILE-VIEW FILE-REFS ;

\ A cast row: its record ended as `cast:` ends one, then the facts the cold
\ registrar latched restored from its flags, as DEF-CAST-FACTS drains them.
\ The checker row comes with the segment's checker slice.
: INSTALL-CAST ( ptr u8 n n n -- )
   {: name:ptr nameu:n wl:n flags:n :}
   name nameu wl DKIND:CAST REC-OPEN
   REC-CAST
   flags DNAME-WIDE and 0<> if REC-WIDE then
   flags DNAME-MIN-IN-MASK and 52 rshift {: mi:n :}
   mi 0<> if ndict@ 1- mi REC-MIN-IN then ;

\ The viewed object's record row k, and its name.
: OBJ-ROW ( n -- ptr u8 ) 1 NUNIT-FILE:SECTION$ drop swap RECORD-BYTES * + ;
: ROW-NAME$ ( ptr u8 -- ptr u8 n )
   {: row:ptr :}
   3 NUNIT-FILE:SECTION$ drop row 2 U64-AT + row 3 U64-AT ;

\ Whether record row k's name is live in the wordlist the install would
\ define it into. A row into a LOCAL wordlist names one the install makes,
\ with nothing in it before then.
: ROW-LIVE? ( n -- bool )
   OBJ-ROW {: row:ptr :}
   row 0 U64-AT RK-PACKAGE = if XREF-NAMESPACE-WL
   else
      row 4 U64-AT dup NFILE-REF:REF-OWNER NFILE-REF:LOCAL = if
         drop false exit
      then
      NFILE-REF:REF>WID
   then {: wid:n :}
   row ROW-NAME$ wid search-wl 0<> ;

\ Own record row k at the next ordinal, the one the segment map gives it.
: INSTALL-RECORD ( n -- )
   {: k:n :}
   k OBJ-ROW {: row:ptr :}
   row 0 U64-AT {: kind:n :}
   row 1 U64-AT {: flags:n :}
   row ROW-NAME$ {: name:ptr nameu:n :}
   \ The definer's install published its clause row.
   kind RK-CLAUSE = if
      ndict@ 1- NFILE-REF:RECORD k 0 MAP-LOCAL <> if BAD then exit
   then
   ndict@ NFILE-REF:RECORD k 0 MAP-LOCAL <> if BAD then
   kind RK-PACKAGE = if
      name nameu row 5 U64-AT row 6 U64-AT INSTALL-PACKAGE
   else
      name nameu row 4 U64-AT NFILE-REF:REF>WID row 5 U64-AT
      kind RK-VAL = if INSTALL-VAL else
      kind RK-ADDR = if flags INSTALL-ADDR else
      kind RK-CAST = if drop flags INSTALL-CAST else
      kind RK-DOES-WORD = if
         flags INSTALL-ADDR  row 6 U64-AT flags INSTALL-DOES
      else
         kind RK-COLON <> if BAD then INSTALL-COLON
      then then then then
   then
   flags DNAME-IMM and 0<> if REC-IMM then ;

\ The viewed object's event row k and the rows in slot 12.
: OBJ-EV ( n -- ptr u8 ) {: k:n :} 12 NUNIT-FILE:SECTION$ drop k EVENT-BYTES * + ;
: OBJ-EVS ( -- n ) 12 NUNIT-FILE:SECTION$ nip EVENT-BYTES / ;
\ An FFI-LIB or FFI-FN row's path or symbol, and the index it stores.
: FFI-NAME$ ( ptr u8 -- ptr u8 n )
   {: row:ptr :}
   3 NUNIT-FILE:SECTION$ drop row 1 U64-AT + row 2 U64-AT ;
: FFI-IDX ( ptr u8 -- n )
   {: row:ptr :}
   row 0 U64-AT EV-FFI-LIB = if row 3 U64-AT else row 6 U64-AT then ;


\ The index event row k's first earlier row of its kind with the same
\ descriptor stores, or -1: within one install the earlier row declares it.
: EARLIER-IDX ( n -- n )
   {: k:n :}
   k OBJ-EV {: row:ptr :}
   row 0 U64-AT {: kind:n :}
   row FFI-NAME$ {: name:ptr u:n :}
   k 0 ?do
      i OBJ-EV {: prev:ptr :}
      prev 0 U64-AT kind = if
         prev FFI-NAME$ name u STR=
         kind EV-FFI-FN = if
            prev 3 U64-AT row 3 U64-AT = and
            prev 4 U64-AT row 4 U64-AT = and
            prev 5 U64-AT row 5 U64-AT = and
         then
         if prev FFI-IDX unloop exit then
      then
   loop
   -1 ;

: FFI-LIB-INDEX ( ptr u8 n -- n ) s" FFI:LIB-INDEX" FFI-XT FFI-PATH-XT execute ;
: FFI-FN-INDEX ( ptr u8 n n n n -- n ) s" FFI:FN-INDEX" FFI-XT FFI-FN-XT execute ;

\ The FFI rows after event row seg, replayed: each library and function
\ declared again answers the index it stores, as FFI-ROWS-HOLD? found, and
\ the declarer's selection the segment left is selected. The replay is the
\ install's own, so the open segment's observer does not journal it.
: INSTALL-FFI ( n -- )
   {: seg:n :}
   OPEN-FFI @ 0= if exit then
   OBS-ON @ 0<> {: on:bool :}
   OBS-CLEAR
   OBJ-EVS seg 1+ ?do
      i OBJ-EV {: row:ptr :}
      row 0 U64-AT {: kind:n :}
      kind EV-FFI-LIB < kind EV-FFI-SCOPE > or if leave then
      kind EV-FFI-LIB = if
         row FFI-NAME$ s" FFI:LIBRARY-PATH" FFI-XT FFI-PATH-XT execute
         row FFI-IDX <> if BAD then
      then
      kind EV-FFI-FN = if
         row FFI-NAME$ row 3 U64-AT row 4 U64-AT row 5 U64-AT
         s" FFI:DECLARE-SPILLED" FFI-XT FFI-FN-XT execute
         row FFI-IDX <> if BAD then
      then
      kind EV-FFI-SCOPE = row 1 U64-AT 1 = and if
         row 2 U64-AT row 3 U64-AT NFILE-REF:REF>WID row 4 U64-AT 0<>
         s" FFI-DECL:SELECT" FFI-XT FFI-SELECT-XT execute
      then
   loop
   on if OBS-SET then ;

\ The sums over the viewed object's event rows before row seg: its SEGMENT
\ rows' records, wordlists, DATA bytes and requires, and its LOAD rows.
: SEG-BASES ( n -- n n n n n )
   {: seg:n :}
   0 0 0 0 0 seg 0 ?do
      {: rows:n wids:n bytes:n reqs:n loads:n :}
      i OBJ-EV {: ev:ptr :}
      ev 0 U64-AT {: kind:n :}
      kind EV-SEGMENT = if
         rows ev 2 U64-AT + wids ev 5 U64-AT + bytes ev 3 U64-AT +
         reqs ev 4 U64-AT + loads
      else
         rows wids bytes reqs loads kind EV-LOAD = if 1+ then
      then
   loop ;

\ Whether segment row ev, its records from own row rows, installs as its
\ load did: DATA bytes and address bodies match the cold load's only while
\ HERE sits where in a cell it sat at the segment's open.
: SEG-ALIGNED? ( ptr u8 n -- bool )
   {: ev:ptr rows:n :}
   ev 3 U64-AT 0<>
   ev 2 U64-AT 0 ?do
      rows i + OBJ-ROW 0 U64-AT {: kind:n :}
      kind RK-ADDR = kind RK-DOES-WORD = or or
   loop
   AOT-ARM:HERE-N 1 cells mod ev 6 U64-AT <> and 0= ;

\ The event row after segment row seg's FFI and TASK rows.
: TAIL-END ( n -- n )
   {: seg:n :}
   OBJ-EVS seg 1+ ?do
      i OBJ-EV 0 U64-AT {: kind:n :}
      kind EV-FFI-LIB < kind EV-TASK > or if i unloop exit then
   loop
   OBJ-EVS ;

\ The event row after segment row seg's FFI and TASK rows and its OUT row.
: SEG-NEXT ( n -- n )
   TAIL-END {: next:n :}
   next OBJ-EVS < if
      next OBJ-EV 0 U64-AT EV-OUT = if next 1+ exit then
   then
   next ;

\ Segment row seg's TASK row, the last of its FFI and TASK rows, or -1.
: TASK-ROW ( n -- n )
   {: seg:n :}
   seg TAIL-END 1- {: last:n :}
   last seg > if last OBJ-EV 0 U64-AT EV-TASK = if last exit then then
   -1 ;

\ Segment row seg's TASK row, installed: the watermark the segment left is
\ stored through the cell TASK hands its claimants. TASK-ROW-HOLDS? found
\ TASK bound.
: INSTALL-TASK ( n -- )
   TASK-ROW {: row:n :}
   row 0 < if exit then
   USER-XT {: xt:n :}
   xt 0= if BAD then
   row OBJ-EV 2 U64-AT USER-PUT !
   ['] USER-PUT! xt USER-NEXT-XT execute ;

\ The offset in slot 8 of require row k.
: REQ-OFF ( n -- n )
   {: k:n :}
   8 NUNIT-FILE:SECTION$ drop {: sec:ptr :}
   0 k 0 ?do {: off:n :} sec off + LE:U64@ off + 8 + loop ;

\ The require rows of segment row seg, reqs from row base, provided again.
\ When a require load follows the segment, its last row is the one
\ `required` stored before that load, which the load stores again.
: INSTALL-REQUIRES ( n n n -- )
   {: seg:n base:n reqs:n :}
   seg SEG-NEXT {: next:n :}
   next OBJ-EVS < if
      next OBJ-EV {: ev:ptr :}
      ev 0 U64-AT EV-LOAD = ev 1 U64-AT SOURCE-UNIT:LOAD-REQUIRE = and
   else false then {: required:bool :}
   required if reqs 1- else reqs then {: n:n :}
   n 0 < if BAD then
   8 NUNIT-FILE:SECTION$ drop {: sec:ptr :}
   base REQ-OFF n 0 ?do
      {: off:n :}
      sec off + LE:U64@ {: len:n :}
      sec off + 8 + len SOURCE-UNIT:PROVIDED-RESOLVED
      off 8 + len +
   loop drop ;

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
   wbase m M-WID INS-CELL ! ev 5 U64-AT m M-WIDS INS-CELL !
   dbase m M-DATA INS-CELL ! ev 3 U64-AT m M-BYTES INS-CELL !
   ev 1 U64-AT m M-ORD INS-CELL !
   ndict@ m M-LIVE-ORD INS-CELL ! AOT-ARM:WIDN m M-LIVE-WID INS-CELL !
   AOT-ARM:HERE-N m M-LIVE-DATA INS-CELL !
   1 INS-SEGS +! ;

public

\ Prepare the viewed object's checker slice 0 before any install side
\ effect, as PREPARE-SLICE? does, and keep its plan for the install. A
\ leading segment's records resolve through its map row at the present
\ cursors, where the install puts them, in a map frame past the installing
\ parent's rows as INSTALL-BEGIN opens the file's; the parent's frame is
\ restored, so a refusal leaves the map as it found it.
: PREPARE-CHECKER ( -- )
   INS-MAP-AT @ {: at:n :}
   INS-SEGS @ {: segs:n :}
   at segs + INS-MAP-AT ! 0 INS-SEGS !
   0 OBJ-EV 0 U64-AT EV-SEGMENT = if 0 0 0 0 MAP-ROW then
   [: 0 PREPARE-SLICE? 0= if BAD then ;] catch {: rc:n :}
   at INS-MAP-AT ! segs INS-SEGS !
   rc 0<> if rc throw then ;

\ Event row seg, a segment: its records with its DATA bytes, the wordlists
\ they make and their protection, its require rows, its FFI rows, its TASK
\ row, then the checker slices through its own. The file's first segment
\ starts at the file's first ordinal and adds its map row here; one after a
\ load starts wherever the loads left the dictionary, and PREPARE-SEGMENT
\ added its map row and prepared its checker slice. Every refusal here is a
\ malformed
\ object.
: INSTALL-SEGMENT ( n -- )
   {: seg:n :}
   seg OBJ-EV {: ev:ptr :}
   ev 0 U64-AT EV-SEGMENT <> if BAD then
   seg SEG-BASES {: rows:n wbase:n dbase:n rbase:n loads:n :}
   ev 1 U64-AT {: first:n :}
   ev 2 U64-AT {: count:n :}
   ev 3 U64-AT {: bytes:n :}
   ev 4 U64-AT {: reqs:n :}
   ev 5 U64-AT {: wids:n :}
   loads 0= if
      INS-FIRST @ first + ndict@ <> if BAD then
      seg rows wbase dbase MAP-ROW
   then
   AOT-ARM:HERE-N INS-DATA ! bytes INS-DATA-U ! 0 INS-DATA-AT !
   FILE-REFS
   count 0 ?do rows i + INSTALL-RECORD loop
   bytes DATA-FILL
   seg SEGMENT-SLOTS
   AOT-ARM:WIDN M-LIVE-WID CUR@ wids + <> if BAD then
   4 NUNIT-FILE:SECTION$ drop wbase + {: prot:ptr :}
   wids 0 ?do prot i + c@ 1 = if M-LIVE-WID CUR@ i + REC-PROTECT then loop
   seg rbase reqs INSTALL-REQUIRES
   seg INSTALL-FFI
   seg INSTALL-TASK
   loads INSTALL-CHECKS ;

private

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
\ ordinal order, and spans a name within srcu source bytes; the end of the
\ last payload. Each payload's literals are planned into the open plan.
: DEFS-CK ( ptr u8 n ptr u8 n n -- n )
   {: defs:ptr defsu:n index:ptr count:n srcu:n :}
   -1 0
   count 0 ?do
      {: prev:n off:n :}
      index i INDEX-BYTES * + {: row:ptr :}
      row LE:U64@ {: ord:n :}
      ord prev <= if BAD then
      row SPAN-AT + LE:U64@ {: at:n :}
      row SPAN-END + LE:U64@ {: end:n :}
      at 0 < end srcu > or if BAD then
      at end < at end or 0= or 0= if BAD then
      row defs defsu off PAYLOAD-AT {: total:n :}
      PAYLOAD-CK
      PAYLOAD-PLAN
      ord off total +
   loop nip ;


\ Plan the viewed FILE object's slot literal rows lits into the open plan
\ as SLOT-VALUE interns them; SLOT-LITS-CK framed the rows.
: SLOT-LITS-PLAN ( ptr u8 n -- )
   {: lits:ptr litsu:n :}
   0 begin dup litsu < while
      {: off:n :}
      lits off + LE:U64@ {: bytes:n :}
      lits off 16 + + bytes DEF-PREP-INTERN drop
      off 16 + bytes +
   repeat drop ;


\ Plan the file's literals into the live pool, writing nothing: its slot
\ literal rows lits, then each payload's as DEFS-CK frames them; the end of
\ the last payload. OPEN-POOL-ROOM says whether the pool takes them all
\ without opening a segment, which would land at HERE, inside the DATA the
\ file's cold load laid out. A child loaded between the object's segments
\ can still use the room up; SEGMENT-ROOM? checks that later segment's
\ payloads before its install. The caller closes the plan.
: POOL-PLAN ( ptr u8 n ptr u8 n ptr u8 n n -- n )
   {: lits:ptr litsu:n defs:ptr defsu:n index:ptr count:n srcu:n :}
   AOT-ARM:HERE-N {: start:n :}
   start DEF-PREP-BEGIN
   lits litsu SLOT-LITS-PLAN
   defs defsu index count srcu DEFS-CK
   DEF-PREP-DATA-END start = OPEN-POOL-ROOM ! ;

\ Plan the literals of the n payloads from index row row, at offset off of
\ the definitions slot defs, defsu bytes, into one plan begun at HERE, as
\ DEFS-CK plans the file's; whether the pool takes them all without opening
\ a segment. The caller closes the plan.
: ROOM-CK ( ptr u8 ptr u8 n n n n -- bool )
   {: index:ptr defs:ptr defsu:n row:n off:n n:n :}
   AOT-ARM:HERE-N {: start:n :}
   start DEF-PREP-BEGIN
   off n 0 ?do
      {: at:n :}
      index row i + INDEX-BYTES * + defs defsu at PAYLOAD-AT {: total:n :}
      PAYLOAD-PLAN
      at total +
   loop drop
   DEF-PREP-DATA-END start = ;

\ Whether segment row ev's payloads, the next ones for its own colon
\ records from row rows, take the literal pool's room at HERE, which the
\ loads before the segment moved: DEFS-CK found the file's took it at
\ HERE when the object opened. Nothing is written, and the FILE sections
\ are viewed again.
: SEGMENT-ROOM? ( ptr u8 n -- bool )
   {: ev:ptr rows:n :}
   0 ev 2 U64-AT 0 ?do rows i + OBJ-ROW 0 U64-AT RK-COLON = if 1+ then loop
   {: n:n :}
   n 0= if true exit then
   2 NUNIT-FILE:SECTION$ drop 0 NUNIT-FILE:SECTION$ INS-ROW @ INS-DEFS-OFF @ n
      ['] ROOM-CK ['] DEF-PREP-CLOSE finally
   FILE-VIEW ;

public

\ Prepare event row seg, a segment after a load, on the state the loads
\ left and before any of its install side effects: its DATA alignment, its
\ payloads' literal pool room, its map row and its checker slice, whose
\ plan is kept for INSTALL-SEGMENT. "" when it prepared, else the reason it
\ did not, with the map and the checker as they were. A code PREPARE-SLICE?
\ rethrows ends the run, so it leaves the map row in place.
: PREPARE-SEGMENT ( n -- ptr u8 n )
   {: seg:n :}
   seg OBJ-EV {: ev:ptr :}
   seg SEG-BASES {: rows:n wbase:n dbase:n rbase:n loads:n :}
   ev rows SEG-ALIGNED? 0= if s" data-alignment" exit then
   ev rows SEGMENT-ROOM? 0= if s" literal-pool" exit then
   seg rows wbase dbase MAP-ROW
   loads PREPARE-SLICE? if s" " exit then
   -1 INS-SEGS +!
   s" checker-prepare" ;

\ Check a FILE object's framing: the dependency and input rows are well
\ formed, the segment sections agree, and the index rows frame the
\ definitions slot exactly (DEFS-CK), and plans the file's literals
\ (POOL-PLAN). A FILE object with definitions or slot literal rows comes
\ only from a capture that had the literal planners (DEF-ELIGIBLE?), so the
\ object write path has them; an engine without them misses at read on
\ E-NUNIT-PROFILE.
: OPEN-FILE ( ptr u8 n ptr u8 n n -- )
   {: bytes:ptr size:n key:ptr keyu:n srcu:n :}
   bytes size NUNIT-FILE:ARCH-AARCH64 s" FILE" key keyu NUNIT-FILE:READ-VIEW
   DEPS-SLOT NUNIT-FILE:SECTION$ NUNIT-DEP:FORMAT OPEN-ROWS !
   11 NUNIT-FILE:SECTION$ NFILE-INPUTS:ROW-COUNT OPEN-INPUTS !
   \ SECTIONS-VIEW replaces the table, so keep the FILE spans first.
   0 NUNIT-FILE:SECTION$ {: defs:ptr defsu:n :}
   2 NUNIT-FILE:SECTION$ {: index:ptr indexu:n :}
   10 NUNIT-FILE:SECTION$ {: lits:ptr litsu:n :}
   indexu INDEX-BYTES mod 0<> if BAD then
   indexu INDEX-BYTES / {: count:n :}
   count srcu SEGMENTS-CK
   count 0= litsu 0= and if
      true OPEN-POOL-ROOM ! 0
   else
      lits litsu defs defsu index count srcu
         ['] POOL-PLAN ['] DEF-PREP-CLOSE finally
   then
   defsu <> if BAD then
   count OPEN-DEFS !
   NUNIT-FILE:RESET-KEEP ;

\ What OPEN-FILE found: definitions, dependency rows, input rows, records,
\ DATA bytes, requires, loads, segments, slots, updates, pins, checker
\ slices, FFI rows and output bytes.
: FILE-COUNTS ( -- n n n n n n n n n n n n n n )
   OPEN-DEFS @ OPEN-ROWS @ OPEN-INPUTS @ OPEN-RECS @ OPEN-DATA @
   OPEN-REQS @ OPEN-LOADS @ OPEN-SEGS @ OPEN-SLOTS @ OPEN-UPDS @
   OPEN-PINS @ OPEN-CHECKS @ OPEN-FFI @ OPEN-OUT @ ;

\ Segment row seg's first record row whose name is live in the wordlist the
\ install would define it into, and true; NULL-PTR 0 false when none.
\ def-open and namespace-record refuse a live name with exit 83 and no message
\ (habu2.f FRESH,), where the live compile gives the language's own refusal,
\ so such an object is a miss decided before any install side effect. The
\ file's references are prepared as INSTALL-SEGMENT prepares them; a row
\ into one of the file's own wordlists resolves none. Nothing is written to
\ the dictionary or the manifest. A BAD from REF>WID is the object's
\ malformation: its record rows passed OBJECT-FRESH? and its scope rows
\ FFI-ROWS-HOLD?.
: LIVE-NAME? ( n -- ptr u8 n bool )
   {: seg:n :}
   seg OBJ-EV {: ev:ptr :}
   ev 0 U64-AT EV-SEGMENT <> if BAD then
   seg SEG-BASES 2drop 2drop {: rows:n :}
   FILE-REFS
   ev 2 U64-AT 0 ?do
      rows i + ROW-LIVE? if rows i + OBJ-ROW ROW-NAME$ true unloop exit then
   loop
   NULL-PTR 0 false ;


\ Whether the facts the viewed object's FFI rows consumed still hold, declaring
\ nothing: the index each declaration would answer, like a resolution row, and
\ the declarer's selection the segment opened with. A row expects the index
\ of an earlier row of the object with its descriptor, else the live row's,
\ else the next new one. A function's library must be one DECLARE-SPILLED
\ takes at the replay, and every scope row's imported wordlist must still
\ have its package, which no dependency row checks, so a hit never throws.
\ Only the FFI rows after segment row seg are checked, after the segments
\ before it installed. A selection its open found in one of the file's own
\ wordlists resolves through the segments the install made; the first
\ segment's predates them. The references are prepared as LIVE-NAME?
\ prepares them.
: FFI-ROWS-HOLD? ( n -- bool )
   {: seg:n :}
   OPEN-FFI @ 0= if true exit then
   seg SEG-BASES drop drop drop nip {: wids:n :}
   FILE-REFS
   s" FFI:NEXT-LIB" FFI-N {: nextlib:n :}
   s" FFI:NEXT-FN" FFI-N {: nextfn:n :}
   s" FFI:MAKER-BANK" FFI-N {: bank:n :}
   0 0 seg TAIL-END seg 1+ ?do
      {: libs:n fns:n :}
      i OBJ-EV {: row:ptr :}
      row 0 U64-AT {: kind:n :}
      kind EV-FFI-SCOPE = if
         row 1 U64-AT 0= {: entry:bool :}
         row 3 U64-AT {: ref:n :}
         ref NFILE-REF:REF-OWNER NFILE-REF:LOCAL = entry 0= wids 0= or and if
            \ A segment's closing selection may be a wordlist it makes,
            \ which no live fact checks; an opening one before the file made
            \ any wordlist cannot hold.
            entry if false unloop exit then
         else
            ref NFILE-REF:REF>WID? 0= if drop false unloop exit then
            {: want:n :}
            entry if
               FFI-SELECTION {: lib:n wid:n stated:bool :}
               lib row 2 U64-AT <> wid want <> or
                  stated if 1 else 0 then row 4 U64-AT <> or
                  if false unloop exit then
            then
         then
      then
      kind EV-FFI-FN = if
         row 3 U64-AT {: lib:n :}
         lib 0<> if
            lib 0 < lib bank and nextlib bank and <> or
               lib bank invert and 1 < or lib nextlib libs + >= or
               if false unloop exit then
         then
      then
      kind EV-FFI-LIB = kind EV-FFI-FN = or if
         i EARLIER-IDX {: earlier:n :}
         earlier 0 >= if earlier libs fns else
            kind EV-FFI-LIB = if
               row FFI-NAME$ FFI-LIB-INDEX {: live:n :}
               live 0<> if live libs fns else nextlib libs + libs 1+ fns then
            else
               row FFI-NAME$ row 3 U64-AT row 4 U64-AT row 5 U64-AT FFI-FN-INDEX
                  {: live:n :}
               live 0 >= if live libs fns else nextfn fns + libs fns 1+ then
            then
         then {: want:n libs2:n fns2:n :}
         want row FFI-IDX <> if false unloop exit then
         libs2 fns2
      else libs fns then
   loop
   2drop true ;

\ Whether TASK's user-slot watermark stands where segment row seg's TASK
\ row found it at the segment's open: the slot offsets the segment baked
\ assume it. A segment that neither read nor moved the watermark has no row
\ and holds.
: TASK-ROW-HOLDS? ( n -- bool )
   TASK-ROW {: row:n :}
   row 0 < if true exit then
   USER-MARK {: user:n bound:bool :}
   bound user row OBJ-EV 1 U64-AT = and ;

\ Begin installing the FILE object bytes holds under key at the next
\ ordinal, its segment map rows after the installing parent's.
: INSTALL-BEGIN ( ptr u8 n ptr u8 n -- )
   {: bytes:ptr size:n key:ptr keyu:n :}
   bytes OBJ-BYTES ! size OBJ-SIZE ! key OBJ-KEY ! keyu OBJ-KEY-U !
   FILE-VIEW
   INS-MAP-AT @ INS-SEGS @ + INS-MAP-AT ! 0 INS-SEGS !
   ndict@ INS-FIRST ! 0 INS-ROW ! 0 INS-DEFS-OFF ! 0 INS-SLICE ! ;

\ Stop the install: the view released and the file's map rows dropped.
\ INS-MAP-AT stays at the file's first row, where the installing parent's
\ rows end: a sibling's rows start there again, and the parent's own rows
\ stay as INSTALL-RESTORE finds them.
: INSTALL-STOP ( -- )
   NUNIT-FILE:RESET-KEEP
   0 INS-SEGS ! ;

\ End the install, its checker slices installed: every segment and
\ definition installed, then INSTALL-STOP.
: INSTALL-END ( -- )
   FILE-REFS EARLIER-SLOTS!
   INS-SEGS @ OPEN-SEGS @ <> INS-ROW @ OPEN-DEFS @ <> or if BAD then
   INSTALL-STOP ;

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
   DEPS-SLOT NUNIT-FILE:SECTION$ NUNIT-DEP:FORMAT OPEN-ROWS !
   11 NUNIT-FILE:SECTION$ NFILE-INPUTS:ROW-COUNT OPEN-INPUTS !
   2 NUNIT-FILE:SECTION$ nip INDEX-BYTES / OPEN-DEFS !
   0 OPEN-RECS ! 0 OPEN-DATA ! 0 OPEN-REQS ! 0 OPEN-WIDS !
   0 OPEN-LOADS ! 0 OPEN-SEGS ! 0 OPEN-FFI ! 0 OPEN-REM !
   OBJ-EVS 0 ?do
      i OBJ-EV {: ev:ptr :}
      ev 0 U64-AT {: kind:n :}
      kind EV-SEGMENT = if
         OPEN-SEGS @ 0= if ev 6 U64-AT OPEN-REM ! then
         ev 2 U64-AT OPEN-RECS +! ev 3 U64-AT OPEN-DATA +!
         ev 4 U64-AT OPEN-REQS +! ev 5 U64-AT OPEN-WIDS +! 1 OPEN-SEGS +!
      else
         kind EV-LOAD = if 1 OPEN-LOADS +! then
         kind EV-FFI-LIB < kind EV-FFI-SCOPE > or 0= if 1 OPEN-FFI +! then
      then
   loop
   5 NUNIT-FILE:SECTION$ drop LE:U64@ OPEN-CHECKS !
   9 NUNIT-FILE:SECTION$ nip SLOT-BYTES / OPEN-SLOTS !
   7 NUNIT-FILE:SECTION$ nip UPDATE-BYTES / OPEN-UPDS !
   15 NUNIT-FILE:SECTION$ nip PIN-BYTES / OPEN-PINS !
   18 NUNIT-FILE:SECTION$ drop LE:U64@ OPEN-SINK !
   19 NUNIT-FILE:SECTION$ nip OPEN-OUT ! ;

\ The viewed object's event rows: their count and row k's kind.
: EVENT-COUNT ( -- n ) OBJ-EVS ;
: EVENT-KIND ( n -- n ) OBJ-EV 0 U64-AT ;

\ LOAD row k's load kind, the child's path and its root, and the source
\ offset the loading file resumes at.
: LOAD-ROW ( n -- n ptr u8 n ptr u8 n n )
   OBJ-EV {: ev:ptr :}
   3 NUNIT-FILE:SECTION$ drop {: names:ptr :}
   ev 1 U64-AT names ev 3 U64-AT + ev 4 U64-AT
   names ev 5 U64-AT + ev 6 U64-AT ev 8 U64-AT ;

\ Issue OUT row k's recorded operations again, in their order: each emit's
\ character to qemit, each write's bytes to qwrite.
: OUT-EACH ( n [ n -- ] [ ptr u8 n -- ] -- )
   {: k:n qemit qwrite :}
   k OBJ-EV {: ev:ptr :}
   19 NUNIT-FILE:SECTION$ drop ev 1 U64-AT + {: a:ptr :}
   ev 2 U64-AT {: u:n :}
   0 begin dup u < while
      {: at:n :}
      a at + {: rec:ptr :}
      rec 8 + LE:U64@ {: len:n :}
      rec OUT-HEAD-BYTES + {: bytes:ptr :}
      rec LE:U64@ OUT-EMIT =
         if bytes LE:U64@ qemit execute else bytes len qwrite execute then
      at OUT-HEAD-BYTES + len +
   repeat drop ;

\ LOAD row k's dependency row count and input row offset: what the file
\ had recorded before the load.
: LOAD-ENDS ( n -- n n )
   OBJ-EV {: ev:ptr :}
   ev 9 U64-AT ev 10 U64-AT ;

\ The FILE object OPEN-FILE found has a well formed slot 18; only then is its
\ origin sink kind ORIGIN-KIND.
: ORIGIN-FORMED? ( -- bool ) OPEN-ORIGIN-OK @ 0<> ;
: ORIGIN-KIND ( -- n ) OPEN-SINK @ ;

\ Visit each origin row of the segment just installed, in row order: its
\ record's live ordinal, name token span, path and name. The rows stand in
\ segment order, so visiting each segment's as it installs keeps the order
\ a cold load records them in, a child's rows between its loader's. It reads
\ the view the install holds and leaves it open for the events after it.
: ORIGIN-EACH ( [ n n n ptr u8 n ptr u8 n -- ] -- )
   {: each :}
   18 NUNIT-FILE:SECTION$ {: sec:ptr secu:n :}
   3 NUNIT-FILE:SECTION$ drop {: names:ptr :}
   M-ORD CUR@ {: first:n :}
   M-ROWS CUR@ {: count:n :}
   secu 8 - ORIGIN-BYTES / 0 ?do
      sec 8 + i ORIGIN-BYTES * + {: row:ptr :}
      row 0 U64-AT {: ord:n :}
      ord first >= ord first count + < and if
         ord ORD>LIVE row 1 U64-AT row 2 U64-AT
         names row 5 U64-AT + row 6 U64-AT
         names row 3 U64-AT + row 4 U64-AT
         each execute
      then
   loop ;

\ The FILE object OPEN-FILE found is one the install replays: package,
\ constant, address, cast and colon records, DATA bytes, slot rows and require
\ rows in segments between child loads, on a checker that imports. Later
\ lanes widen the rest. Slot 12 holds no SEGMENT row for a run that added
\ nothing to the dictionary, DATA or the require registry, so a file whose
\ runs added nothing has no segment to replay and loads from source. The install makes
\ wordlists only through package rows, so every wordlist the file made must
\ be one they own. The first segment's data alignment is checked here, when
\ it opens the file; a later one's at its install. The answer names the
\ first part it cannot replay, for the miss; empty when it can install the
\ whole object.
: UNSUPPORTED$ ( -- ptr u8 n )
   OPEN-SEGS @ 0= if s" empty-file" exit then
   OPEN-UPDS @ 0<> if s" unsupported-updates" exit then
   OPEN-PINS @ 0<> if s" unsupported-pins" exit then
   OPEN-OWNED @ OPEN-WIDS @ <> if s" unsupported-wordlists" exit then
   FILE-CHECK-AVAILABLE? 0= if s" checker-unavailable" exit then
   OPEN-KINDS @ 1 RK-VAL lshift and 0<>
   s" NPUB:IMPORT-PENDING" NUNIT-CAPTURE:OPTIONAL-PRODUCER-XT 0= and
      if s" unsupported-import-pending" exit then
   OPEN-KINDS @ 1 RK-RETIRED lshift and 0<> if s" unsupported-retired" exit then
   OPEN-KINDS @ 1 RK-CODELESS lshift and 0<> if s" unsupported-codeless" exit then
   OPEN-KINDS @ 1 RK-DEFER lshift and 0<> if s" unsupported-defer" exit then
   0 EVENT-KIND EV-SEGMENT = if
      0 OBJ-EV 0 SEG-ALIGNED? 0= if s" data-alignment" exit then
   then
   OPEN-POOL-ROOM @ 0= if s" literal-pool" exit then
   s" " ;

\ This engine has every record writer the install block executes.
: DEF-OPEN? ( -- bool )
   s" def-open" ENGINE-XT 0<> s" def-close" ENGINE-XT 0<> and
   s" def-create" ENGINE-XT 0<> and
   s" imm-mark" ENGINE-XT 0<> and s" namespace-record" ENGINE-XT 0<> and
   s" prot-wid-add" ENGINE-XT 0<> and s" def-cast" ENGINE-XT 0<> and
   s" does-patch" ENGINE-XT 0<> and s" wide-mark" ENGINE-XT 0<> and
   s" min-in-mark" ENGINE-XT 0<> and ;

;package
