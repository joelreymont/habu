\ linker.f - the cross-build: WITH opens a data space for the build's target, runs the body there and closes it, and
\ each part of a file links its object or compiles (docs/architecture.md "Builds").

require src/lib/prelude.f
require src/lib/error.f
require src/lib/string.f
require src/lib/memory.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/byte-buffer.f
require src/lib/le.f
require src/lib/sort.f
require src/lib/build-out.f
require src/lib/fmt.f
require src/habu/layout.f
require src/habu/boot-cells.f
require src/habu/xref.f
require src/habu/prims.f                \ PRIM-SPEC: the owned primitives' rows
require src/habu/address-cells.f
require src/habu/interpreter.f
require src/habu/intern.f
require src/core/include.f
require src/core/compile-reads.f
require src/compiler/target/model.f
require src/compiler/target/native-binding.f
require src/compiler/native/abi.f
require src/compiler/native/shadow.f
require src/compiler/native/publish.f
require src/habu/link-band.f

package LINKER
private

\ The linker's failed build: -9690..-9699, minted here, in the package its codes
\ belong to (docs/forth-card.md § 6).
-9690 constant E-LINKER-FIRST
-9699 constant E-LINKER-LAST

\ The open's progress, the last step completed. Past open, loading: a file's load runs, from its key
\ to its last BUILT row, the loads it requests included; returned: its body returned; failed: a load threw.
ENUM step none mapped shadowed moved checked required objects routed open loading returned failed ;ENUM
TYPED-VARIABLE PROGRESS step

public
-9690 s" a file of this build failed or DP moved back in its space: the build loads no other file and writes no object or program" ERROR:CODE E-FAILED
-9691 s" a file of this build changed an earlier record or code, or stored into a cell no file holds" ERROR:CODE E-STORE
-9693 s" a file of this build made a wordlist no package of it holds" ERROR:CODE E-WORDLIST
-9694 s" a file of this build left a task running at its end: its object would hold data the task can still change" ERROR:CODE E-LIVE-TASK
private

TYPED-VARIABLE ENGINE? bool          \ the mode is "engine"
TYPED-VARIABLE KERNEL? bool          \ the mode is "kernel": hb's own layout, no program
TYPED-VARIABLE TARGET RTARGET:resolved-target   \ the target the build's program runs on
variable BASE                        \ the space's address, its distance from NULL-PTR
variable SPACE-SIZE                  \ the space's size; MAP maps ROUTED-BYTES more below BASE
variable FIRST-REC                   \ ndict@ at the open: records from here are the space's
variable OPEN-DP                     \ here's distance once open; header data is [BASE, OPEN-DP)
variable OPEN-TASKS                  \ hb's live tasks at the open (TASKS-LIVE-CELL)
TYPED-VARIABLE BODY [ -- ]
variable COMPILED-N                  \ files the last build compiled
variable LINKED-N                    \ files the last build linked
TYPED-VARIABLE PRIOR-RUNNER [ ptr u8 n ptr u8 n ptr u8 n [ n n -- ] -- ]

\ The host cells a build changes; RESTORE puts them back.
variable SAVED-DP
variable SAVED-FLOOR
variable SAVED-CEILING
variable SAVED-DPHW
variable SAVED-DATA-FLOOR
variable SAVED-GLOBAL-WL
variable SAVED-NAMESPACE-WL
variable SAVED-TYPES-WL
variable SAVED-COMMA-RUN
variable SAVED-COMMA-END
variable SAVED-COMMA-BASE
variable SAVED-LASTC
variable SAVED-CURRENT
variable SAVED-SIG-POOL
variable SAVED-SIG-LEN
variable SAVED-FLAGS                 \ hb's newest record's flags (NEWEST-FLAGS)
variable SAVED-EXIT-HOOK             \ hb's process-exit vector (ARM-EXIT)

$40 constant NORESERVE               \ macOS MAP_NORESERVE

: HOST ( n -- ptr n )
   {: off:n :}
   data-base BYTE-VIEW off + CELL-VIEW ;

: HOST@ ( n -- n ) HOST @ ;
: HOST! ( n n -- ) HOST ! ;
: DATA-N ( -- n ) data-base NULL-PTR - ;

: SPACE-AT ( n -- ptr n )
   {: off:n :}
   NULL-PTR BYTE-VIEW BASE @ + off + CELL-VIEW ;

\ The open space's bytes, [BASE, DP).
: SPACE-DATA ( -- ptr u8 n )
   NULL-PTR BYTE-VIEW BASE @ +  DP-CELL HOST@ BASE @ - ;

: ROWS ( -- ptr n n ) SPACE-DATA ADDRESS-CELLS:DATA-SPAN ;

\ A row's cell offset from the space's start, and its kind (layout.f).
: ROW-OFF ( n -- n ) SNAP-RELOC:XTCELL-OFF-MASK and ;
: SCRATCH-ROW? ( n -- bool ) SNAP-RELOC:XTCELL-SCRATCH-TAG and 0<> ;
: DATA-ROW? ( n -- bool ) SNAP-RELOC:XTCELL-DATA-TAG and 0<> ;
: XT-ROW? ( n -- bool )
   SNAP-RELOC:XTCELL-DATA-TAG SNAP-RELOC:XTCELL-SCRATCH-TAG or and 0= ;

: CELL-VALUE ( n -- n ) ROW-OFF SPACE-AT @ ;

\ Runs only while IMAGE-BASE-CELL is 0 (before IMAGE-IN or after IMAGE-OUT), so no marker reaches
\ the space's table (habu2.f EMIT-MARK) and its index is released unlocked.
: UNMAP ( -- )
   ADDRESS-CELLS:INDEX-CELL SPACE-AT ADDRESS-CELLS:INDEX-RELEASE
   NULL-PTR BYTE-VIEW BASE @ ROUTED-BYTES - +  SPACE-SIZE @ ROUTED-BYTES +  munmap
   0<> if E-MEM-UNMAP throw then ;

\ Above hb's DATA: below it CHECKER-OWNER-GUARD refuses with E-COMP-OWNER.
: HINT ( -- n ) DATA-N DATA-SIZE + ;

\ A build for another target than hb's own.
: CROSS? ( -- bool ) TARGET @ RTARGET:HOST-TARGET RTARGET:SAME? 0= ;

\ The emissions NSHADOW:PUBLISH files per definition: a cross build's target and host copies, or one.
: COPIES ( -- n ) CROSS? if 2 else 1 then ;

\ The space's DATA starts, ends and stops DP where the kernel the link adopts
\ reads them (ARM64:READ); a kernel build's, a cross build's, or one no object
\ was read for, where hb's does (src/arch/wasm/profile.f WPROF mirrors hb's
\ band through DATA-START).
: OBJECT-LAYOUT? ( -- bool ) KERNEL? @ 0= CROSS? 0= and ARM64:READ? and ;

: LAYOUT-START ( -- n )
   OBJECT-LAYOUT? if s" DATA-START" ARM64:SYM exit then
   DATA-START ;

: LAYOUT-SIZE ( -- n )
   OBJECT-LAYOUT? if s" DATA-SIZE" ARM64:SYM exit then
   DATA-SIZE ;

: LAYOUT-CEILING ( -- n )
   OBJECT-LAYOUT? if s" DP-CEILING-OFF" ARM64:SYM exit then
   DATA-SIZE PROF-CNT-BYTES - ;

\ ---- the object ----------------------------------------------------------------
\ LOADERS stacks a row per file whose compile or link is running: its key, then 1
\ for a link, 0 for a compile. LOADS holds each load's path, its offset in KEY-PATHS, 8 bytes; a
\ key is that offset and the count of the build's earlier loads of the path. BUILT holds a row per
\ piece compiled or linked: its file's key, its number in the file, its time
\ (COMPILED-TIME as it compiled), its first and end record index, its first and end
\ emission, the data pointer as the piece starts and ends, 1 when the piece holds a
\ header-data address (HOLDS-HEADER), else 0, then the piece's stores time (PIECE-STORES-TIME).
\ A FILES row is a BUILT row's first FILES-ROW-BYTES bytes, its time 0 while no use names
\ the piece whole (REF-FILE).
16 constant KEY-BYTES
KEY-BYTES 80 + constant BUILT-BYTES
KEY-BYTES constant BUILT-PIECE
BUILT-PIECE 8 + constant BUILT-TIME
BUILT-TIME 8 + constant BUILT-FIRST
BUILT-FIRST 16 + constant BUILT-EM
BUILT-EM 16 + constant BUILT-DP
BUILT-DP 16 + constant BUILT-HEADER
BUILT-HEADER 8 + constant BUILT-STORES
KEY-BYTES 16 + constant FILES-ROW-BYTES
KEY-BYTES 8 + constant LOADER-BYTES
create LOADERS BUF:HDR-BYTES allot
create LOADS BUF:HDR-BYTES allot
create BUILT BUF:HDR-BYTES allot
create REFS BUF:HDR-BYTES allot      \ the object's FILES rows
\ The object's WORDS rows (WORD-ROW+), one per earlier word it uses: its FILES row k from 1, its sum and count
\ (REC-IDS) and its time (WORD-TIMES); WORD-RECS holds each row's record index.
create WORD-ROWS BUF:HDR-BYTES allot
create WORD-RECS BUF:HDR-BYTES allot
\ The object's EXTENTS rows, one per earlier extent a data reference names (USED-EXT+): its FILES
\ row k from 1, its kind, sum and count and its time (BUILT-EXTS).
create USED-EXTS BUF:HDR-BYTES allot
40 constant USED-EXT-BYTES
create REQUESTS BUF:HDR-BYTES allot  \ the object's REQUESTS rows
create PIECES BUF:HDR-BYTES allot    \ the object's PIECES
create TIMES BUF:HDR-BYTES allot     \ the object's TIMES: the part's block (TIMES+)
create NAMED-ROWS BUF:HDR-BYTES allot \ the BUILT rows the compiling part's source and evaluated texts name whole, 8 bytes each
create NAMED-RECS BUF:HDR-BYTES allot \ the records with a time the running scan hit, 8 bytes each, in hit order
create SPELLS BUF:HDR-BYTES allot    \ the object's NAMES: SPELL-TEXT's HIT sums, each open a 0 then its sum
create RAN-RECS BUF:HDR-BYTES allot  \ the records the compiling part ran, 8 bytes each (RAN-RECORD)
\ The object's REACHED: per piece whose data or stores RAN-FILES+ reached, its FILES row and its stores time, 8 bytes each.
create REACHED BUF:HDR-BYTES allot
create PENDING-PATHS BUF:HDR-BYTES allot \ the paths the checked part's earlier acts provided, each a FIELD+
create REQ-PATH BUF:HDR-BYTES allot  \ the path a linked request spells
create UNDER-PATH BUF:HDR-BYTES allot \ RESOLVE-UNDER's answer under another root
create ENTRY-ROOT BUF:HDR-BYTES allot \ the root current as the part started (MARKS)
create EXIT-ROOT BUF:HDR-BYTES allot \ the root current as the part ended
create PART-READS BUF:HDR-BYTES allot \ the reads the part's compile made (READS-KEEP)
create PART-TEXTS BUF:HDR-BYTES allot \ the texts the part's compile evaluated (READS-KEEP)
create OBJECT BUF:HDR-BYTES allot    \ the object WRITE-OBJECT lays or READ-OBJECT reads
create PRIOR BUF:HDR-BYTES allot     \ the compiling file's object as the last build left it
create CODE-COPY BUF:HDR-BYTES allot \ an emission's bytes as the link fills and places them
create LATE BUF:HDR-BYTES allot      \ the file's late private wordlists
create LOAD-PATHS BUF:HDR-BYTES allot \ each load's key, then its path below its root as a FIELD+
create KEY-PATHS BUF:HDR-BYTES allot  \ each loaded or named file's path once, a FIELD+; a key's first cell is its offset
create OBJ-PATHS BUF:HDR-BYTES allot  \ the object's PATHS as WRITE-OBJECT lays it
create OBJ-NAME BUF:HDR-BYTES allot   \ an object's name below BUILD-OUT:OBJ$'s directory
DYNAMIC-BUFFER BUILT-PATHS n         \ BUILT row r's path: its FIELD+'s offset in LOAD-PATHS
KEY-BYTES BUFFER: FILE-KEY
\ An object's INPUT: the source's stamp (STAMP-BYTES), then the part's entry root, its exit root and its compile's
\ reads (COMPILE-READS), each a FIELD+. An UNLINKABLE part's stamp is 0 0 -1 and its three fields are empty.
3 8 * constant STAMP-BYTES
STAMP-BYTES BUFFER: INPUT-STAMP      \ the source's stamp (INPUT!)

\ clock_gettime writes a struct timespec: seconds, then nanoseconds, eight bytes each.
$10 constant TIMESPEC-BYTES
TIMESPEC-BYTES BUFFER: CLOCK-SPEC
PROCESS-SYMBOLS
FUNCTION: CLOCK-GETTIME-CALL clock_gettime ( n ptr u8 -- i32 )
   1 TIMESPEC-BYTES WRITES-BYTES
;FUNCTION

\ Nanoseconds since the epoch on clock 0, CLOCK_REALTIME on macOS and Linux: a piece's time.
: NOW ( -- n )
   0 CLOCK-SPEC CLOCK-GETTIME-CALL 0<> if E-COMP-STATE throw then
   CLOCK-SPEC LE:U64@ 1000000000 *  CLOCK-SPEC 8 + LE:U64@ + ;

DYNAMIC-BUFFER ENTRY-OFFS n          \ a read object's entries' offsets in OBJECT (PART-FRESH?)
DYNAMIC-BUFFER REC-KINDS n           \ its record r's kind
DYNAMIC-BUFFER EM-RECS n             \ its emission e's record index
DYNAMIC-BUFFER REC-EMS n             \ its record r's emission, -1 for none
DYNAMIC-BUFFER PRIVS n               \ the packages holding a private wordlist: file, index
variable PRIVS-N
DYNAMIC-BUFFER GRANTS n              \ the built packages a checked piece gives a private wordlist, by record
variable GRANTS-N
DYNAMIC-BUFFER EM-OFFS n             \ its emissions' offsets in OBJECT, each at its S
variable LAST-CALL                   \ the checked emission's last call row's site, -1 before its first (EMISSION-CK)
variable LAST-ADDR                   \ and its last address row's site, -1 before its first
DYNAMIC-BUFFER FILE-ROWS n           \ its FILES row k, from 1, as that file's BUILT row index
DYNAMIC-BUFFER DOES-AT n             \ its KIND-DOES entries: the offset field's place in OBJECT, the emission
DYNAMIC-BUFFER HOST-CALLS n          \ the host copy's call sites out of hb's region
variable HOST-CALLS-N
DYNAMIC-BUFFER ALIASES n             \ the linked file's aliases, each its record index, then its source's
variable ALIASES-N
DYNAMIC-BUFFER HOST-ADDRS n          \ its data address sites
variable HOST-ADDRS-N

\ The owned primitives' records the open lays (OWNED-RECORDS), OWNED-CELLS cells each: hb's
\ kernel record's index, then the offset in OWNED-TEXT and length of its package, name and facts text.
7 constant OWNED-CELLS
DYNAMIC-BUFFER OWNED n
variable OWNED-N
DYNAMIC-BUFFER OWNED-TEXT u8
variable OWNED-TEXT-N

\ As a file's compile starts (COPY-BELOW): the space's cells BELOW, the records
\ [FIRST-REC - 1, FILE-REC), and NSHADOW:PATCHES.
create OLD-SPACE BUF:HDR-BYTES allot
create OLD-RECS BUF:HDR-BYTES allot
variable OLD-PATCHES
DYNAMIC-BUFFER STORED n              \ the stores' cells as distances from NULL-PTR (FIND-STORES)
variable STORED-N
\ The running piece's anchors (ANCHOR+), 3 cells each: the address as a distance from NULL-PTR, its
\ rank at one address (1 a record, 2 a string), the record's index or the string's body.
DYNAMIC-BUFFER ANCHORS n
variable ANCHORS-N
DYNAMIC-BUFFER ORDER n               \ EXTENTS!'s sorts: anchor numbers, then string extents
\ The running piece's extents in address order (EXTENTS!, OWN-IDS!), EXT-CELLS cells: start from NULL-PTR, kind, sum,
\ count; span in PIECES and TOP-ROW (EXTENT+); first slot and count in EXT-ROWS (GROUP-ROWS); bytes; time (TIMES+).
11 constant EXT-CELLS
DYNAMIC-BUFFER EXTS n
variable EXTS-N
DYNAMIC-BUFFER PICKED n              \ the piece's cell rows in the space's order, each then its extent (GROUP-ROWS)
DYNAMIC-BUFFER EXT-ROWS n            \ the same rows grouped by extent
DYNAMIC-BUFFER LAST-HELD n           \ a file's record r's last held facts entry, -1 for none (FACTS+)
\ Per word as PIECE+ writes it, SPAN-CELLS cells: entry offset and length, emission or -1, text offset and length (0 0
\ for none), TOP-ROW, the bits of the outside rows that name it (NAMED-RETIRED, NAMED-LATE), RECORD+'s answer.
8 constant SPAN-CELLS
1 constant NAMED-RETIRED             \ a retirement row (FACT+)
2 constant NAMED-LATE                \ a late private wordlist (LATE+)
DYNAMIC-BUFFER WORD-SPANS n
DYNAMIC-BUFFER EM-SPANS n
variable WORDS-N                     \ the piece's words
variable TOP-ROW                     \ the highest FILES row REF-FILE answered since it was zeroed
DYNAMIC-BUFFER WORD-TIMES n          \ the build's word times by record index, 0 for none (REC-TIME)
\ Record r's identity, which no earlier part changes: its NAME-SUM at 2r, 0 for no name, its
\ count at 2r + 1, from 0, among the earlier records of its part with that sum (IDENTIFY).
DYNAMIC-BUFFER REC-IDS n
variable IDENTIFIED                  \ every record below it that a piece holds has its identity
\ Slot s holds at 6s the key's first cell plus 1 (0 while empty), its second, a file key's part or a key -2 or -3's
\ kind, sum, count, value; at most half full, so a probe ends. A link reads the -2 slots its PART-FRESH? filled (OWN-IDS!).
DYNAMIC-BUFFER PLACES n
DYNAMIC-BUFFER OLD-PLACES n          \ PLACES' slots as it grows
variable PLACES-CAP                  \ its slots, a power of two
variable PLACES-N                    \ its filled slots
variable OWN-SERIAL                  \ the last check's number (OWN-IDS!)
\ Record r's mark: the serial of the object that last gave it a WORDS row (WORD-ROW+), 0 for none.
\ Each OBJECT-OPEN takes the next of SERIALS, which only grows, so no object takes another's mark.
DYNAMIC-BUFFER LISTED-BY n
\ Build-table row b's mark: the serial of the object that last gave it an EXTENTS row (USED-EXT+).
DYNAMIC-BUFFER USED-BY n
\ BUILT row r's mark at 2r, the serial of the object that last asked for its FILES row (REF-FILE), and that row from 1
\ at 2r + 1; only OBJECT-OPEN, under a new serial, and REF-FILE's append change FILES rows, so the mark names its row.
DYNAMIC-BUFFER REF-OF n
\ Record r's BUILT row plus 1, 0 for a record no row holds (BUILT+); its cells from FIRST-REC up
\ to REC-ROW-N are written.
DYNAMIC-BUFFER REC-ROW n
variable REC-ROW-N
\ The build table (BUILT+), per built part's extent: BUILT row, kind, sum, count, start from NULL-PTR, length, time;
\ a part's extents in address order, then its laid texts (TEXT-EXTS+). ROW-EXTS: row r's first at 2r, count at 2r + 1.
7 constant BUILT-EXT-CELLS
DYNAMIC-BUFFER BUILT-EXTS n
variable BUILT-EXTS-N
DYNAMIC-BUFFER ROW-EXTS n
variable SERIALS
variable OBJECT-SERIAL
\ Record r's mark: the serial of the object that last took it as a seed (RAN-RECORD).
DYNAMIC-BUFFER RAN-BY n
4 8 * constant WORD-ROW-BYTES
DYNAMIC-BUFFER RETIRED n             \ the piece's retired records (RETIRED!): index, then wordlist cell
variable RETIRED-N                   \ its records
DYNAMIC-BUFFER COPY-CELLS n          \ the image copy WRITE patches (link-copy.f), zero-padded cells
DYNAMIC-BUFFER LISTED n              \ the DATA list's offsets, sorted before WRITE lays them (link-data.f)
variable LISTED-N
\ Each header cell a piece stored, compiled or linked: the cell's offset from the space and
\ the piece's BUILT row. A stripped link reads a header address cell only once no file or a
\ live one stored it (link-copy.f).
DYNAMIC-BUFFER HSTORE-AT n
DYNAMIC-BUFFER HSTORE-ROW n
variable HSTORES-N
\ Each store a piece made below its own data (STORER+) and each earlier record it changed (CHANGER+): pair p holds at
\ 2p the BUILT row holding the cell or the record, -1 for header data, and at 2p + 1 the piece's row.
DYNAMIC-BUFFER STORERS n
variable STORERS-N                   \ its pairs
\ BUILT row r's marks from RAN-FILES+'s walk: 1 an extent of its data reached, 2 its stores
\ reached (WALK-STORERS).
DYNAMIC-BUFFER WALKED n
DYNAMIC-BUFFER REACH n               \ the cells the walk reached, each once, in EDGES' form (EDGE+)
variable REACH-N
\ A record's, emission's or build-table row's cell holds REACH-STAMP once the running walk reached
\ it; each walk takes the next stamp, so no cell is cleared.
DYNAMIC-BUFFER REC-SEEN n
DYNAMIC-BUFFER EM-SEEN n
DYNAMIC-BUFFER EXT-SEEN n
variable REACH-STAMP
TYPED-VARIABLE HEADER-SEEN bool      \ the walk reached a header-data reference
DYNAMIC-BUFFER EDGES n               \ the build's references, compiled or linked, in order (EDGE+)
variable EDGES-N                     \ its cells
DYNAMIC-BUFFER REC-EDGES n           \ record r's EDGES: the first at 2r, the count at 2r + 1
DYNAMIC-BUFFER EM-EDGES n            \ the same by emission number
DYNAMIC-BUFFER ROW-EDGES n           \ the same by BUILT row, for its STORES values
DYNAMIC-BUFFER EXT-EDGES n           \ the same by build-table row, for its cell rows

\ ndict@, CHECK:HELD-FACTS, NSHADOW:RECORDS, NSHADOW:EMISSIONS, here's distance from NULL-PTR, WIDN-CELL,
\ the loader's EVENT-COUNT and INTERPRETER:RUN-COUNT as a file's load or its piece after a request starts (MARKS).
variable FILE-REC
variable FILE-HELD
variable FILE-TFAM                   \ TFAM:TFAM-N@ as the running piece starts
variable FILE-TYPES                  \ CHECK:TYPE-END as the running piece starts
variable FILE-ROW
variable FILE-EM
variable FILE-DP
variable FILE-SRC                    \ the compiling file's source, its distance from NULL-PTR (LOAD)
variable FILE-SRC-U                  \ its length in bytes
variable FILE-PART                   \ the part compiling or linking, from 0
variable FILE-OFF                    \ the offset in the source the compiling or linking part starts at
variable FILE-WID                    \ the first wordlist id the file's compile can mint
variable FILE-EVENT                  \ the first event of the loader's log the piece made
variable FILE-RUN                    \ the first entry of INTERPRETER's run log the piece made
variable FILE-END                    \ here's distance as the piece ends
\ The running piece holds a header-data address outside its STORES rows' cells: a tag-4
\ reference written (DATA-REF+) or resolved (RESOLVE).
TYPED-VARIABLE HOLDS-HEADER bool
TYPED-VARIABLE UNDER-KNOWN bool     \ whether RESOLVE-UNDER's path under another root is known
\ The running piece changes a record below FILE-REC (CHANGER+). FACT+ sets it too, for the records PIECE-PLACE's
\ `false LINK-FACTS` hands CHANGER+ (LINK-RETIRED, LINK-TEXT) only after COMPILED-TIME reads this.
TYPED-VARIABLE CHANGED-EARLIER bool
variable FILE-CELLS                  \ the space's address-cell rows as the file's compile starts
PATH-CAP BUFFER: FILE-PATH           \ the file's path, which a refused reference names
variable FILE-PATH-N
variable STORES-AT                   \ the offset in OBJECT of STORES' count, of the piece placed or object read
variable FACTS-AT                    \ the offset in OBJECT of FACTS' count, of the piece placed or object read
variable ACTS-AT                    \ the offset in OBJECT of ACTS' count, of the piece checked
\ The compiling part never links (WRITE-OBJECT): no stamp, a start or end off TOP-SCOPE?, an unclean cut
\ ending it or the part before (NEST), a load it asked for the runner never ran (ACTS+).
TYPED-VARIABLE UNLINKABLE bool
\ What LOAD's loop over the file's parts does next: link part FILE-PART at FILE-OFF, compile it,
\ or, once NEST cut the compiling part at a clean request, go back to linking the next.
ENUM turn link compile cut ;ENUM
TYPED-VARIABLE TURN turn

\ The magic, key, part number and time, FILE-OFF, then per section INPUT FILES REQUESTS NAMES REACHED PATHS WORDS
\ EXTENTS TIMES PIECES an offset and a length (0 0 for an empty FILES to REACHED, WORDS or EXTENTS); then the sections.
8 KEY-BYTES + 24 + constant PAIRS-AT        \ the header's first pair, INPUT's
PAIRS-AT 10 2 * 8 * + constant HEADER-BYTES
\ WORDS, EXTENTS and TIMES, which hold times, precede PIECES, which test/file-objects.f compares
\ across out roots.
HEADER-BYTES 32 - constant TIMES-FIELD      \ the header's TIMES pair
HEADER-BYTES 16 - constant PIECES-FIELD     \ the header's last pair: PIECES' offset, then its length
\ TIMES: word count, per word WORD-CELLS cells (time, entry, emission and text spans, its bound record's time or 0, the
\ outside rows' bits, sum, count); extent count, per extent EXT-BYTES (time, span offset and length, kind, sum, count);
\ then the stores entry: stores time, STORES span offset and length.
11 constant WORD-CELLS
WORD-CELLS 8 * constant WORD-BYTES
48 constant EXT-BYTES

\ A record's kind in PIECES.
0 constant KIND-DEF
1 constant KIND-PACKAGE
2 constant KIND-ALIAS
3 constant KIND-DOES                 \ a `does>` definer's `;does` record
4 constant KIND-PRIVATE              \ a package gaining its private wordlist after its record
5 constant KIND-SEALED               \ an earlier file's package this file sealed
6 constant KIND-TYPE                 \ a type entry: a family's record in the types wordlist

\ NAMES maps each name the build's records define, by NAME-SUM, to the record (INDEX-RECS) and, when private, its
\ package's key (NAME-PKG). HIT marks what the running source names: timed records in HIT-STAMPS, whole pieces in HITS.
DYNAMIC-BUFFER NAMES n               \ slot s: the sum at 3s, 0 while empty, the record (INDEX-RECS), the package key
DYNAMIC-BUFFER OLD-NAMES n           \ NAMES' slots as it grows
variable NAMES-CAP                   \ its slots, a power of two
variable NAMES-N                     \ its filled slots
variable INDEXED                     \ the record index NAMES holds every record below
DYNAMIC-BUFFER OPENED n              \ the keys of the packages the running file's source opens
variable OPENED-N
DYNAMIC-BUFFER HITS n                \ BUILT row r's mark: 0, or named whole by the running file's source
TYPED-VARIABLE BODY-HIT bool         \ the source names a record the build's body made
\ Record r's stamp: HIT-STAMP once the running scan hit it, HIT-STAMP + 1 once WORDS-CK found it
\ listed; any other value is a mark of an earlier scan, since HITS-CLEAR adds 2 to HIT-STAMP.
DYNAMIC-BUFFER HIT-STAMPS n
variable HIT-STAMP
variable HIT-N                       \ the records the running scan hit and WORDS-CK has not found listed
\ The sums SPELLS holds since its last open, the running run: slot s holds a sum at 2s and its
\ run's id at 2s+1, and a slot of an earlier run is empty.
DYNAMIC-BUFFER RUN-SUMS n
variable RUN-CAP                     \ its slots, a power of two
variable RUN-N                       \ the running run's filled slots
variable RUN-ID                      \ the running run's id, from 1
variable RUN-AT                      \ the offset in SPELLS of the running run's first sum

: SLOT-SUM ( n -- ptr n ) 3 * NAMES ;
: SLOT-ROW ( n -- ptr n ) 3 * 1 + NAMES ;
: SLOT-PKG ( n -- ptr n ) 3 * 2 + NAMES ;

\ NAMES with cap slots, all empty.
: NAMES-CLEAR ( n -- )
   {: cap:n :}
   cap 3 * NAMES-RESERVE
   cap 3 * 0 ?do 0 i NAMES ! loop
   cap NAMES-CAP !  0 NAMES-N ! ;

\ RUN-SUMS with cap slots, all empty: no run's id is 0.
: RUN-CLEAR ( n -- )
   {: cap:n :}
   cap 2 * RUN-SUMS-RESERVE
   cap 2 * 0 ?do 0 i RUN-SUMS ! loop
   cap RUN-CAP !  0 RUN-N ! ;

: DISPOSE-OBJECTS ( -- )
   WORD-SPANS-RELEASE  EM-SPANS-RELEASE  WORD-TIMES-RELEASE  LISTED-BY-RELEASE  USED-BY-RELEASE  REC-IDS-RELEASE  PLACES-RELEASE  OLD-PLACES-RELEASE  REF-OF-RELEASE  REC-ROW-RELEASE  TIMES BUF:DISPOSE
   RAN-BY-RELEASE  WALKED-RELEASE  REACH-RELEASE  REC-SEEN-RELEASE  EM-SEEN-RELEASE  EXT-SEEN-RELEASE  HSTORE-AT-RELEASE  HSTORE-ROW-RELEASE  STORERS-RELEASE
   EDGES-RELEASE  REC-EDGES-RELEASE  EM-EDGES-RELEASE  ROW-EDGES-RELEASE  EXT-EDGES-RELEASE  LISTED-RELEASE  COPY-CELLS-RELEASE  LAST-HELD-RELEASE  RETIRED-RELEASE  STORED-RELEASE
   ANCHORS-RELEASE  ORDER-RELEASE  EXTS-RELEASE  PICKED-RELEASE  EXT-ROWS-RELEASE  BUILT-EXTS-RELEASE  ROW-EXTS-RELEASE
   ENTRY-OFFS-RELEASE  REC-KINDS-RELEASE  EM-RECS-RELEASE  REC-EMS-RELEASE  PRIVS-RELEASE  GRANTS-RELEASE  EM-OFFS-RELEASE
   FILE-ROWS-RELEASE
   DOES-AT-RELEASE  HOST-CALLS-RELEASE  HOST-ADDRS-RELEASE  ALIASES-RELEASE
   BUILT-PATHS-RELEASE  LOAD-PATHS BUF:DISPOSE  KEY-PATHS BUF:DISPOSE  OBJ-PATHS BUF:DISPOSE  OBJ-NAME BUF:DISPOSE  CODE-COPY BUF:DISPOSE  OLD-RECS BUF:DISPOSE  OLD-SPACE BUF:DISPOSE
   LATE BUF:DISPOSE  PRIOR BUF:DISPOSE  OBJECT BUF:DISPOSE  REQ-PATH BUF:DISPOSE  UNDER-PATH BUF:DISPOSE
   ENTRY-ROOT BUF:DISPOSE  EXIT-ROOT BUF:DISPOSE  PART-READS BUF:DISPOSE  PART-TEXTS BUF:DISPOSE
   PENDING-PATHS BUF:DISPOSE  REACHED BUF:DISPOSE  RAN-RECS BUF:DISPOSE
   SPELLS BUF:DISPOSE  NAMED-RECS BUF:DISPOSE  NAMED-ROWS BUF:DISPOSE  PIECES BUF:DISPOSE  REQUESTS BUF:DISPOSE  WORD-RECS BUF:DISPOSE  WORD-ROWS BUF:DISPOSE  USED-EXTS BUF:DISPOSE  REFS BUF:DISPOSE
   BUILT BUF:DISPOSE  LOADS BUF:DISPOSE  LOADERS BUF:DISPOSE  RUN-SUMS-RELEASE  HIT-STAMPS-RELEASE  HITS-RELEASE  OPENED-RELEASE
   OLD-NAMES-RELEASE  NAMES-RELEASE  OWNED-RELEASE  OWNED-TEXT-RELEASE ;

: INIT-BUFFERS ( -- )
   LOADERS 64 BUF:N>BLEN BUF:INIT
   LOADS 256 BUF:N>BLEN BUF:INIT
   BUILT 256 BUF:N>BLEN BUF:INIT
   REFS 256 BUF:N>BLEN BUF:INIT
   WORD-ROWS 256 BUF:N>BLEN BUF:INIT
   USED-EXTS 256 BUF:N>BLEN BUF:INIT
   WORD-RECS 256 BUF:N>BLEN BUF:INIT
   REQUESTS 256 BUF:N>BLEN BUF:INIT
   PIECES 256 BUF:N>BLEN BUF:INIT
   TIMES 256 BUF:N>BLEN BUF:INIT
   NAMED-ROWS 256 BUF:N>BLEN BUF:INIT
   NAMED-RECS 256 BUF:N>BLEN BUF:INIT
   SPELLS 256 BUF:N>BLEN BUF:INIT
   RAN-RECS 256 BUF:N>BLEN BUF:INIT
   REACHED 256 BUF:N>BLEN BUF:INIT
   PENDING-PATHS 256 BUF:N>BLEN BUF:INIT
   REQ-PATH 256 BUF:N>BLEN BUF:INIT
   UNDER-PATH 256 BUF:N>BLEN BUF:INIT
   ENTRY-ROOT 256 BUF:N>BLEN BUF:INIT
   EXIT-ROOT 256 BUF:N>BLEN BUF:INIT
   PART-READS 256 BUF:N>BLEN BUF:INIT
   PART-TEXTS 256 BUF:N>BLEN BUF:INIT
   OBJECT 256 BUF:N>BLEN BUF:INIT
   PRIOR 256 BUF:N>BLEN BUF:INIT
   CODE-COPY 256 BUF:N>BLEN BUF:INIT
   LATE 256 BUF:N>BLEN BUF:INIT
   LOAD-PATHS 256 BUF:N>BLEN BUF:INIT
   KEY-PATHS 256 BUF:N>BLEN BUF:INIT
   OBJ-PATHS 256 BUF:N>BLEN BUF:INIT
   OBJ-NAME 256 BUF:N>BLEN BUF:INIT
   OLD-SPACE 256 BUF:N>BLEN BUF:INIT
   OLD-RECS 256 BUF:N>BLEN BUF:INIT
   0 HSTORES-N !  0 STORERS-N !  0 STORED-N !  0 HOST-CALLS-N !  0 HOST-ADDRS-N !  0 ALIASES-N !  0 GRANTS-N !
   0 EDGES-N !  0 BUILT-EXTS-N !
   1024 NAMES-CLEAR  1024 RUN-CLEAR  FIRST-REC @ INDEXED !  0 IDENTIFIED !  0 OWN-SERIAL !  FIRST-REC @ REC-ROW-N !
   1024 6 * PLACES-RESERVE  1024 6 * 0 ?do 0 i PLACES ! loop  1024 PLACES-CAP !  0 PLACES-N ! ;

\ All or none: disposing a header never set up, or disposed, is a no-op.
: INIT-OBJECTS ( -- )
   0 ENTRY-N !  0 ENTRY-CAP !
   ['] INIT-BUFFERS catch {: rc:n :}
   rc 0<> if DISPOSE-OBJECTS rc throw then ;

: BYTES$ ( ptr a -- ptr u8 n ) BUF:SPAN$ BUF:BLEN>N ;

\ OBJECT's bytes, which every field read of OBJECT reads: READ-OBJECT and WRITE-OBJECT, the
\ only words that move them, take them again (OBJ-TAKE) before they return, as RD-OPEN does.
PTR-VARIABLE OBJ-BASE

\ Answers OBJECT's length.
: OBJ-TAKE ( -- n ) OBJECT BYTES$ swap OBJ-BASE ! ;

: OBJ-PTR ( n -- ptr u8 ) OBJ-BASE @ swap + ;

: SPAN+ ( ptr u8 n ptr a -- ) {: a:ptr u:n buf:ptr :}
   a u BUF:N>BLEN buf BUF:APPEND-SPAN ;

\ n as eight bytes, low byte first.
: U64+ ( n ptr a -- ) {: v:n buf:ptr :}
   v  buf 8 BUF:N>BLEN BUF:GROW SPAN:$ drop  LE:U64! ;

\ A key field: its length as a U64+, then its bytes.
: FIELD+ ( ptr u8 n ptr a -- ) {: a:ptr u:n buf:ptr :}
   u buf U64+  a u buf SPAN+ ;

\ The offset of the path's FIELD+ in the buffer of FIELD+s, added when it holds none.
: PATH-IN ( ptr u8 n ptr a -- n )
   {: a:ptr u:n buf:ptr :}
   buf BYTES$ {: p:ptr pu:n :}
   0 begin dup pu < while
      {: at:n :}
      p at + LE:U64@ {: len:n :}
      p at + 8 + len  a u  STR= if at exit then
      at 8 + len +
   repeat
   drop  a u buf FIELD+  pu ;

\ The path the key at the address names (KEY-PATHS).
: CANON$ ( ptr u8 -- ptr u8 n )
   LE:U64@ {: at:n :}
   KEY-PATHS BYTES$ drop {: a:ptr :}
   a at + 8 +  a at + LE:U64@ ;

\ The key of the file whose compile or link is running, empty for the build's body.
: LOADER$ ( -- ptr u8 n )
   LOADERS BYTES$ {: a:ptr u:n :}
   u 0= if a 0 exit then
   a u LOADER-BYTES - +  KEY-BYTES ;

\ The running file is being linked, so a file it requests links too.
: LINKING? ( -- bool )
   LOADERS BYTES$ {: a:ptr u:n :}
   u 0= if false exit then
   a u 8 - + LE:U64@ 1 = ;

\ The build's earlier loads of the path at the offset in KEY-PATHS (LOADS).
: SEEN ( n -- n )
   {: at:n :}
   LOADS BYTES$ {: rows:ptr rowsu:n :}
   0  rowsu 8 / 0 ?do
      rows i 8 * + LE:U64@ at = if 1 + then
   loop ;

\ FILE-KEY for a load of the path: its offset in KEY-PATHS, then its earlier loads (SEEN); the
\ offset goes onto LOADS.
: KEY! ( ptr u8 n -- )
   {: path:ptr pathu:n :}
   path pathu KEY-PATHS PATH-IN {: at:n :}
   at FILE-KEY LE:U64!
   at SEEN FILE-KEY 8 + LE:U64!
   at LOADS U64+ ;

\ FILE-KEY onto the stack, true for a link.
: PUSH-KEY ( bool -- )
   {: link:bool :}
   FILE-KEY KEY-BYTES LOADERS SPAN+
   link if 1 else 0 then LOADERS U64+ ;

\ The stack's top back into FILE-KEY, and off the stack.
: POP-KEY ( -- )
   LOADER$ {: top:ptr topu:n :}
   top FILE-KEY topu BYTE-COPY
   LOADERS BYTES$ LOADER-BYTES - BUF:N>BLEN LOADERS BUF:REPLACE ;

: PATH! ( ptr u8 n -- )
   {: path:ptr pathu:n :}
   pathu PATH-CAP min {: u:n :}
   path FILE-PATH u BYTE-COPY  u FILE-PATH-N ! ;

\ FILE-KEY's load into LOAD-PATHS, with the path below the root that resolved it, as the
\ program's map names the file.
: PATH+ ( ptr u8 n ptr u8 n -- )
   SOURCE-ROOT:RELATIVE {: rel:ptr relu:n :}
   FILE-KEY KEY-BYTES LOAD-PATHS SPAN+
   rel relu LOAD-PATHS FIELD+ ;

\ The offset in LOAD-PATHS of FILE-KEY's path; a piece's BUILT row follows its file's load.
: KEY-PATH ( -- n )
   LOAD-PATHS BYTES$ {: a:ptr u:n :}
   0 begin dup u < while
      a over + KEY-BYTES  FILE-KEY KEY-BYTES  STR= if KEY-BYTES + exit then
      KEY-BYTES +  a over + LE:U64@ +  8 +
   repeat
   drop E-COMP-STATE throw ;

\ The object of FILE-KEY's part FILE-PART. A source named a.f.2 or a.f+1 shares the name of a.f's second load or
\ second part: each fails the other's object at HEADER-CK's key and compiles.
: OBJECT$ ( -- ptr u8 n )
   OBJ-NAME BUF:CLEAR
   TARGET @ RTARGET:PROFILE$ OBJ-NAME SPAN+  s" /" OBJ-NAME SPAN+
   ENGINE? @ if s" engine" else KERNEL? @ if s" kernel" else s" app" then then OBJ-NAME SPAN+
   FILE-KEY CANON$ {: p:ptr pu:n :}
   pu 0 >  if p c@ FS:SLASH = else false then 0= if s" /" OBJ-NAME SPAN+ then
   p pu OBJ-NAME SPAN+
   FILE-KEY 8 + LE:U64@ {: n:n :}
   n 0 > if  s" ." OBJ-NAME SPAN+  n 1 + FMT:INT$ OBJ-NAME SPAN+  then
   FILE-PART @ {: k:n :}
   k 0 > if  s" +" OBJ-NAME SPAN+  k FMT:INT$ OBJ-NAME SPAN+  then
   s" .hfo" OBJ-NAME SPAN+
   OBJ-NAME BYTES$ BUILD-OUT:OBJ$ ;

\ The object path's directory made, so the object can be written there.
: OBJECT-DIR ( ptr u8 n -- )
   {: p:ptr pu:n :}
   pu begin dup 0 > if p over 1 - + c@ FS:SLASH <> else false then while 1 - repeat
   1 - {: du:n :}
   p du FS:MAKE-DIRS ;

\ ---- the records ---------------------------------------------------------------
\ No address reaches PIECES, so larger earlier files leave its bytes alone (test/file-objects.f).

: BUILT-N ( -- n ) BUILT BYTES$ {: a:ptr u:n :}  u BUILT-BYTES / ;

: BUILT-AT ( n -- ptr u8 )
   {: row:n :}
   BUILT BYTES$ {: a:ptr u:n :}  a row BUILT-BYTES * + ;

: HOLDS? ( n n -- bool )
   {: idx:n row:n :}
   row BUILT-AT BUILT-FIRST + {: at:ptr :}
   at LE:U64@ idx <=  at 8 + LE:U64@ idx >  and ;

\ The BUILT row that holds the record index, and true; 0 false when none does.
: REC-ROW@ ( n -- n bool )
   {: idx:n :}
   idx FIRST-REC @ <  idx REC-ROW-N @ >=  or if 0 false exit then
   idx REC-ROW @ {: r:n :}
   r 0= if 0 false exit then
   r 1 - true ;

\ The BUILT row that holds the record index; E-COMP-STATE when none does.
: BUILT-ROW ( n -- n )
   REC-ROW@ 0= if drop E-COMP-STATE throw then ;

\ The BUILT row whose [first, end) data pointer holds v, and true; false when none.
: DP-ROW ( n -- n bool )
   {: v:n :}
   BUILT-N 0 ?do
      i BUILT-AT BUILT-DP + {: at:ptr :}
      at LE:U64@ v <=  at 8 + LE:U64@ v >  and if i true unloop exit then
   loop
   0 false ;

\ The last BUILT row whose data ends at v, and true; false when none. A piece with
\ no data, as the open's records are, holds no byte and ends nothing.
: DP-END-ROW ( n -- n bool )
   {: v:n :}
   BUILT-N 0 ?do
      BUILT-N 1 - i - {: row:n :}
      row BUILT-AT BUILT-DP + {: at:ptr :}
      at 8 + LE:U64@ v =  at LE:U64@ v <>  and if row true unloop exit then
   loop
   0 false ;

: TOPPED ( n -- n ) dup TOP-ROW @ max TOP-ROW ! ;

\ The object's FILES row from 0 naming the BUILT row's piece by its file's key and its number, the
\ count of rows for none: REF-OF's row when its mark is the object's serial, else the first match.
: FIND-REF ( n -- n )
   {: row:n :}
   row 1 + 2 * REF-OF-RESERVE
   row 2 * REF-OF @ OBJECT-SERIAL @ = if  row 2 * 1 + REF-OF @ 1 -  exit then
   row BUILT-AT {: at:ptr :}
   REFS BYTES$ {: refs:ptr refsu:n :}
   refsu FILES-ROW-BYTES / {: n:n :}
   n 0 ?do
      refs i FILES-ROW-BYTES * +  KEY-BYTES 8 +  at KEY-BYTES 8 +  STR= if i unloop exit then
   loop
   n ;

\ The FILES row from 1 naming the BUILT row's piece, added on first use and marked in REF-OF. Its time is the piece's
\ once a use is whole, else 0: only WORDS and EXTENTS rows and REACHED's stores times decide.
: REF-FILE ( n bool -- n )
   {: row:n whole:bool :}
   row BUILT-AT {: at:ptr :}
   REFS BYTES$ {: refs:ptr refsu:n :}
   refsu FILES-ROW-BYTES / {: n:n :}
   row FIND-REF {: i:n :}
   OBJECT-SERIAL @ row 2 * REF-OF !  i 1 + row 2 * 1 + REF-OF !
   i n < if
      refs i FILES-ROW-BYTES * + {: r:ptr :}
      whole  r BUILT-TIME + LE:U64@ 0=  and if
         at BUILT-TIME + LE:U64@  r BUILT-TIME +  LE:U64!
      then
      i 1 + TOPPED exit
   then
   at FILES-ROW-BYTES REFS SPAN+
   whole 0= if  0  REFS BYTES$ drop  n FILES-ROW-BYTES * +  BUILT-TIME +  LE:U64!  then
   n 1 + TOPPED ;

\ Record r's time in WORD-TIMES, 0 for none.
: REC-TIME ( n -- n )
   {: r:n :}
   r 1 + WORD-TIMES-RESERVE  r WORD-TIMES @ ;

\ Record r's sum and count in REC-IDS, which IDENTIFY wrote.
: REC-ID ( n -- n n )
   {: r:n :}
   r 2 * REC-IDS @  r 2 * 1 + REC-IDS @ ;

\ The object's WORDS row for record idx of FILES row k's piece, unless the object already has one
\ for the record (LISTED-BY).
: WORD-ROW+ ( n n -- )
   {: k:n idx:n :}
   idx 1 + LISTED-BY-RESERVE
   idx LISTED-BY @ OBJECT-SERIAL @ = if exit then
   OBJECT-SERIAL @ idx LISTED-BY !
   idx REC-ID {: sum:n count:n :}
   k WORD-ROWS U64+  sum WORD-ROWS U64+  count WORD-ROWS U64+  idx REC-TIME WORD-ROWS U64+
   idx WORD-RECS U64+ ;

\ An earlier record's FILES row, whole (REF-FILE) for a word with no time for its WORDS row to
\ check.
: WORD-REF ( n -- n )
   {: idx:n :}
   idx BUILT-ROW {: row:n :}
   row  idx REC-TIME 0=  REF-FILE ;

\ A record reference: the file, 0 for this one, then the record's sum and count (REC-ID), which no
\ earlier part changes; an earlier piece's record also gets its WORDS row.
: ID-REF+ ( n -- )
   {: idx:n :}
   idx REC-ID {: sum:n count:n :}
   idx FILE-REC @ >= if
      0 PIECES U64+  sum PIECES U64+  count PIECES U64+  exit
   then
   idx WORD-REF {: k:n :}
   k PIECES U64+  sum PIECES U64+  count PIECES U64+
   k idx WORD-ROW+ ;

\ ---- dependencies by name -------------------------------------------------------
\ A spelled record with a time gets a WORDS row; one without names its piece whole, as do the pieces that changed it.

\ FNV-1a over the name folded to upper case, as the dictionary compares names; never 0.
: NAME-SUM ( ptr u8 n -- n )
   {: a:ptr u:n :}
   $811C9DC5  u 0 ?do
      a i + c@ {: c:n :}
      c 97 >=  c 122 <=  and if c 32 - else c then  xor  $1000193 *
   loop
   dup 0= if 1 + then ;

\ Whether slot s of PLACES holds a key other than the key's two cells, third cell, sum and count.
: SLOT-OTHER? ( n n n n n n -- bool )
   {: s:n path:n seen:n third:n sum:n count:n :}
   s 6 * PLACES {: row:ptr :}
   row @ {: first:n :}
   first 0= if false exit then
   first path 1 + <>  row 1 cells + @ seen <>  or
   row 2 cells + @ third <>  or
   row 3 cells + @ sum <>  or  row 4 cells + @ count <>  or ;

\ The slot of PLACES that holds the key's two cells, that third cell, sum and count, else the empty
\ slot the probe from their hash ends at.
: PLACE-SLOT ( n n n n n -- n )
   {: path:n seen:n third:n sum:n count:n :}
   PLACES-CAP @ 1 - {: mask:n :}
   sum  path $9E3779B1 * +  seen $C2B2AE3D * +  count $85EBCA77 * +  third $27D4EB2F * +  mask and
   begin dup path seen third sum count SLOT-OTHER? while 1 + mask and repeat ;

\ The value in the slot PLACE-SLOT answers for the key, third cell and identity, -1 for none.
: PLACE-VALUE ( n n n n n -- n )
   PLACE-SLOT 6 * PLACES {: row:ptr :}
   row @ 0= if -1 exit then
   row 5 cells + @ ;

\ The record with that sum and count of the part whose FILES row is at the address, -1 for none.
: PLACE ( ptr u8 n n -- n )
   {: key:ptr sum:n count:n :}
   key LE:U64@  key 8 + LE:U64@  key KEY-BYTES + LE:U64@  sum count PLACE-VALUE ;

\ The running part's record with that sum and count, -1 for none.
: PART-REC ( n n -- n )
   {: sum:n count:n :}
   FILE-KEY LE:U64@  FILE-KEY 8 + LE:U64@  FILE-PART @  sum count PLACE-VALUE ;

\ The number in its piece of the checked piece's record with that sum and count (OWN-IDS!), -1 for
\ none.
: OWN-REC ( n n -- n )
   {: sum:n count:n :}
   -2 OWN-SERIAL @ 0 sum count PLACE-VALUE ;

\ The number in its part of the checked part's extent of that kind, sum and count (OWN-IDS!), -1
\ for none.
: OWN-EXT ( n n n -- n )
   {: kind:n sum:n count:n :}
   -2 OWN-SERIAL @ kind sum count PLACE-VALUE ;

\ The BUILT-EXTS row of BUILT row row's extent of that kind, sum and count (BUILT+), -1 for none.
: ROW-EXT ( n n n n -- n )
   {: row:n kind:n sum:n count:n :}
   -3 row kind sum count PLACE-VALUE ;

\ Cell f of extent e.
: EXT ( n n -- ptr n ) swap EXT-CELLS * + EXTS ;

\ The extent holding the address: the last whose start is at most it, -1 for none.
: EXT-OF ( n -- n )
   {: at:n :}
   0 EXTS-N @
   begin 2dup < while
      {: lo:n hi:n :}
      lo hi + 2 / {: mid:n :}
      mid 0 EXT @ at <= if mid 1 + hi else lo mid then
   repeat
   drop 1 - ;

\ Cell f of BUILT-EXTS row b.
: BUILT-EXT ( n n -- ptr n ) swap BUILT-EXT-CELLS * + BUILT-EXTS ;

\ The BUILT-EXTS row of BUILT row row's extent holding the address: the last whose start is at most
\ it, -1 for none.
: ROW-EXT-OF ( n n -- n )
   {: at:n row:n :}
   row 2 * ROW-EXTS @ {: first:n :}
   first  first row 2 * 1 + ROW-EXTS @ +
   begin 2dup < while
      {: lo:n hi:n :}
      lo hi + 2 / {: mid:n :}
      mid 4 BUILT-EXT @ at <= if mid 1 + hi else lo mid then
   repeat
   drop 1 -  dup first < if drop -1 then ;


\ PLACES grown to have room for n more slots at half full; growing drops a finished check's slots
\ (first cell -1), and PLACES-N counts the slots put back.
: PLACES-ROOM ( n -- )
   {: n:n :}
   PLACES-CAP @ {: cap:n :}
   n PLACES-N @ +  2 * {: need:n :}
   cap begin dup need < while 2 * repeat {: grown:n :}
   grown cap > if
      cap 6 * OLD-PLACES-RESERVE
      cap 6 * 0 ?do  i PLACES @  i OLD-PLACES !  loop
      grown 6 * PLACES-RESERVE
      grown 6 * 0 ?do  0 i PLACES !  loop
      grown PLACES-CAP !
      0 PLACES-N !
      cap 0 ?do
         i 6 * {: o:n :}
         o OLD-PLACES @ {: first:n :}
         first 0<>  first -1 <>  and if
            first 1 -  o 1 + OLD-PLACES @  o 2 + OLD-PLACES @  o 3 + OLD-PLACES @  o 4 + OLD-PLACES @
            PLACE-SLOT 6 * {: s:n :}
            6 0 ?do  o i + OLD-PLACES @  s i + PLACES !  loop
            1 PLACES-N +!
         then
      loop
   then ;


\ The value v into the empty slot PLACE-SLOT answers; E-COMP-STATE when a slot holds the key, third cell and identity:
\ a second value would hide the first, and a part's records and extents each have their own (IDENTIFY, EXTENTS!).
: PLACE! ( n n n n n n -- )
   {: path:n seen:n third:n sum:n count:n v:n :}
   path seen third sum count PLACE-SLOT 6 * {: s:n :}
   s PLACES @ 0<> if E-COMP-STATE throw then
   path 1 + s PLACES !  seen s 1 + PLACES !  third s 2 + PLACES !  sum s 3 + PLACES !
   count s 4 + PLACES !  v s 5 + PLACES !
   1 PLACES-N +! ;


\ The running piece's records from FILE-REC or IDENTIFIED into REC-IDS and PLACES, each counted among part FILE-PART's
\ records.
: IDENTIFY ( -- )
   FILE-REC @ IDENTIFIED @ max {: start:n :}
   ndict@ 2 * REC-IDS-RESERVE
   FILE-KEY LE:U64@  FILE-KEY 8 + LE:U64@ {: path:n seen:n :}
   ndict@ start - 0 max PLACES-ROOM
   ndict@ start ?do
      i XREF-REC XREF-NAME$ {: a:ptr u:n :}
      u 0= if 0 else a u NAME-SUM then {: sum:n :}
      0 begin sum over PART-REC 0 >= while 1 + repeat {: count:n :}
      sum i 2 * REC-IDS !  count i 2 * 1 + REC-IDS !
      path seen FILE-PART @ sum count i PLACE!
   loop
   ndict@ IDENTIFIED ! ;

: SAME? ( n n n n -- bool )
   {: s:n sum:n row:n pkg:n :}
   s SLOT-SUM @ sum =  s SLOT-ROW @ row =  and  s SLOT-PKG @ pkg =  and ;

: TAKEN? ( n n n n -- bool )
   {: s:n sum:n row:n pkg:n :}
   s SLOT-SUM @ 0<>  s sum row pkg SAME? 0=  and ;

\ The slot holding the name, else the first empty one from the sum's.
: SLOT-OF ( n n n -- n )
   {: sum:n row:n pkg:n :}
   NAMES-CAP @ 1 - {: mask:n :}
   sum mask and
   begin dup sum row pkg TAKEN? while 1 + mask and repeat ;

: PUT-NAME ( n n n -- )
   {: sum:n row:n pkg:n :}
   sum row pkg SLOT-OF {: s:n :}
   s SLOT-SUM @ 0<> if exit then
   sum s SLOT-SUM !  row s SLOT-ROW !  pkg s SLOT-PKG !  1 NAMES-N +! ;

\ NAMES with twice its slots, its names put again.
: GROW-NAMES ( -- )
   NAMES-CAP @ {: cap:n :}
   cap 3 * OLD-NAMES-RESERVE
   cap 3 * 0 ?do i NAMES @  i OLD-NAMES ! loop
   cap 2 * NAMES-CLEAR
   cap 0 ?do
      i 3 * OLD-NAMES @ {: sum:n :}
      sum 0<> if sum  i 3 * 1 + OLD-NAMES @  i 3 * 2 + OLD-NAMES @  PUT-NAME then
   loop ;

\ Half NAMES' slots at most are filled, so a probe always ends.
: NAME+ ( n n n -- )
   NAMES-N @ 1 + 2 *  NAMES-CAP @ > if GROW-NAMES then  PUT-NAME ;

\ A package's name up to its first ':', the top-level package a part belongs to.
: TOP-NAME ( ptr u8 n -- ptr u8 n )
   {: a:ptr u:n :}
   u 0 ?do a i + c@ 58 = if a i unloop exit then loop
   a u ;

\ A name's bytes after its last ':', length 0 when it has none or ends in one.
: TAIL-NAME ( ptr u8 n -- ptr u8 n )
   {: a:ptr u:n :}
   u 0 ?do a u + i - 1 - c@ 58 = if a u + i - i unloop exit then loop
   a 0 ;

\ The key of the package whose private wordlist holds the record, its top-level name's
\ NAME-SUM; 0 for a record any file may name.
: NAME-PKG ( n -- n )
   {: idx:n :}
   idx XREF-REC XREF-WORDLIST {: wid:n :}
   wid CHECK:WORDLIST-PACKAGE {: p:n hit:bool :}
   hit 0= if 0 exit then
   p XREF-REC XREF-PKG-PRIVATE wid <> if 0 exit then
   p XREF-REC XREF-NAME$ TOP-NAME NAME-SUM ;

\ The record's name into NAMES with the value. A packaged type's entry is named
\ `A:turn`, but `using A`, `package A` and `A:box<A:turn>` spell only `turn`:
\ its tail is filed too.
: REC-NAME+ ( n n -- )
   {: idx:n v:n :}
   idx XREF-REC XREF-NAME$ {: a:ptr u:n :}
   u 0= if exit then
   idx NAME-PKG {: pkg:n :}
   a u NAME-SUM v pkg NAME+
   idx XREF-REC XREF-WORDLIST BOOT:TYPES-WL <> if exit then
   a u TAIL-NAME {: t:ptr tu:n :}
   tu 0<> if t tu NAME-SUM v pkg NAME+ then ;

\ The records [INDEXED, end) of the row into NAMES, each as -1 when the row is negative, the
\ build's body; -2 - row, its piece named whole, when it has no time (REC-TIME); else its index.
: INDEX-RECS ( n n -- )
   {: end:n row:n :}
   end INDEXED @ ?do
      row 0< if -1 else i REC-TIME 0= if -2 row - else i then then {: v:n :}
      i v REC-NAME+
   loop
   end INDEXED ! ;

\ True for a public name's key, 0, and for a package the running scan opened.
: OPENED? ( n -- bool )
   {: pkg:n :}
   pkg 0= if true exit then
   OPENED-N @ 0 ?do i OPENED @ pkg = if true unloop exit then loop
   false ;

: OPENED+ ( n -- )
   OPENED-N @ 1 + OPENED-RESERVE
   OPENED-N @ OPENED !  1 OPENED-N +! ;

\ A NAMES slot's record hit: the build's body's in BODY-HIT, a piece named whole in HITS, else
\ the record once per scan in HIT-STAMPS, HIT-N and NAMED-RECS.
: HIT-SLOT ( n -- )
   {: v:n :}
   v -1 = if true BODY-HIT ! exit then
   v -1 < if 1  -2 v -  HITS ! exit then
   v 1 + HIT-STAMPS-RESERVE
   v HIT-STAMPS @ HIT-STAMP @ = if exit then
   HIT-STAMP @ v HIT-STAMPS !  1 HIT-N +!  v NAMED-RECS U64+ ;

\ Marks every record NAMES holds the sum for, a private name's only once its package is open.
: HIT ( n -- )
   {: sum:n :}
   NAMES-CAP @ 1 - {: mask:n :}
   sum mask and
   begin dup SLOT-SUM @ 0<> while
      dup SLOT-SUM @ sum = if
         dup SLOT-PKG @ OPENED? if dup SLOT-ROW @ HIT-SLOT then
      then
      1 + mask and
   repeat drop ;

: RUN-TAKEN? ( n n -- bool )
   {: s:n sum:n :}
   s 2 * 1 + RUN-SUMS @ RUN-ID @ =  s 2 * RUN-SUMS @ sum <>  and ;

\ The running run's slot holding the sum, else the first empty one from the sum's.
: RUN-SLOT ( n -- n )
   {: sum:n :}
   RUN-CAP @ 1 - {: mask:n :}
   sum mask and
   begin dup sum RUN-TAKEN? while 1 + mask and repeat ;

\ The sum into the running run, and true when it was not there.
: RUN-SUM+ ( n -- bool )
   {: sum:n :}
   sum RUN-SLOT {: s:n :}
   s 2 * 1 + RUN-SUMS @ RUN-ID @ = if false exit then
   sum s 2 * RUN-SUMS !  RUN-ID @ s 2 * 1 + RUN-SUMS !  1 RUN-N +!  true ;

\ RUN-SUMS with twice its slots, holding the running run's sums from SPELLS.
: GROW-RUN ( -- )
   RUN-CAP @ 2 * RUN-CLEAR
   SPELLS BYTES$ {: a:ptr u:n :}
   u RUN-AT @ - 8 / 0 ?do a RUN-AT @ + i 8 * + LE:U64@ RUN-SUM+ drop loop ;

: RUN! ( -- ) 1 RUN-ID +!  0 RUN-N !  SPELLS BYTES$ nip RUN-AT ! ;

\ SPELL-TEXT's HIT: SPELLS gains the sum once per run, as a repeat with no open between marks
\ nothing more. Half RUN-SUMS' slots at most are filled, so a probe always ends.
: SPELL-HIT ( n -- )
   {: sum:n :}
   RUN-N @ 1 + 2 *  RUN-CAP @ > if GROW-RUN then
   sum RUN-SUM+ if  sum SPELLS U64+  sum HIT  then ;

\ SPELL-TEXT's OPENED+ of a sum OPENED? lacks: SPELLS gains 0, which no NAME-SUM is, then the sum, and a run starts.
: SPELL-OPEN ( n -- )
   {: sum:n :}
   sum OPENED? if exit then
   0 SPELLS U64+  sum SPELLS U64+  sum OPENED+  RUN! ;

\ A byte that ends a spelled name: white space, or ':' and '"', which join a package to its
\ word and a string's quote to its first word.
: BLANK? ( n -- bool ) {: c:n :}  c 32 =  c 9 = or  c 10 = or  c 13 = or ;
: CUT? ( n -- bool ) {: c:n :}  c BLANK?  c 58 = or  c 34 = or ;

\ A byte that ends a type argument in a span: '<', '>' and ','.
: ARG-CUT? ( n -- bool ) {: c:n :}  c 60 =  c 62 = or  c 44 = or ;

variable PART-AT                     \ SPELL-TEXT's running part's first byte
variable ARG-FROM                    \ its running argument's
variable WORD-AT                     \ its running word's
TYPED-VARIABLE JOINED bool           \ the running word holds a ':' or '"'
TYPED-VARIABLE FIRST-WORD bool       \ no word has ended on the running line
TYPED-VARIABLE SKIP bool             \ the running line is a comment
TYPED-VARIABLE AFTER-PACKAGE bool    \ the last word ended was `package`

\ SPELL-HIT for the source's bytes [from, to) when there are any.
: HIT-SPAN ( ptr u8 n n -- )
   {: src:ptr from:n to:n :}
   to from > if src from +  to from -  NAME-SUM SPELL-HIT then ;

\ True when the word [from, to) is `\` and its line's first: the line is a comment. A `\` later
\ in a line may be text a parsing word takes (`[char] \ INDEX-OF`), so it starts none.
: COMMENT? ( ptr u8 n n -- bool )
   {: src:ptr from:n to:n :}
   FIRST-WORD @  to from - 1 =  and if src from + c@ 92 = else false then ;

\ The word [from, to), not empty: the package it names opens when it follows `package`.
: OPENS ( ptr u8 n n -- )
   {: src:ptr from:n to:n :}
   src from +  to from - {: a:ptr u:n :}
   AFTER-PACKAGE @ if a u NAME-SUM SPELL-OPEN then
   a u s" package" XREF-STR=CI AFTER-PACKAGE ! ;

\ The running word ends at byte i, the blank c.
: WORD-END ( ptr u8 n n -- )
   {: src:ptr i:n c:n :}
   src WORD-AT @ i COMMENT?  c 10 <>  and if true SKIP ! exit then
   src PART-AT @ i HIT-SPAN
   ARG-FROM @ PART-AT @ > if src ARG-FROM @ i HIT-SPAN then
   JOINED @ if src WORD-AT @ i HIT-SPAN then
   i WORD-AT @ > if src WORD-AT @ i OPENS  false FIRST-WORD ! then
   c 10 = if true FIRST-WORD ! then
   i 1 + PART-AT !  i 1 + ARG-FROM !  i 1 + WORD-AT !  false JOINED ! ;

\ SPELL-HIT for each name outside comment lines: a span between CUT? bytes, a
\ run of it between ARG-CUT? bytes (`box<turn>`: box<turn>, box, turn) and a
\ joined word (`P:W`: P, W, P:W).
: SPELL-TEXT ( ptr u8 n -- )
   {: src:ptr u:n :}
   0 PART-AT !  0 ARG-FROM !  0 WORD-AT !  false JOINED !
   true FIRST-WORD !  false SKIP !  false AFTER-PACKAGE !   \ a text's last word opens nothing in the next
   u 0 ?do
      src i + c@ {: c:n :}
      SKIP @ if
         c 10 = if
            false SKIP !  true FIRST-WORD !
            i 1 + PART-AT !  i 1 + ARG-FROM !  i 1 + WORD-AT !
         then
      else
         c BLANK? if src i c WORD-END else
            c CUT? if
               src PART-AT @ i HIT-SPAN
               ARG-FROM @ PART-AT @ > if src ARG-FROM @ i HIT-SPAN then
               i 1 + PART-AT !  i 1 + ARG-FROM !  true JOINED !
            else
               c ARG-CUT? if src ARG-FROM @ i HIT-SPAN  i 1 + ARG-FROM ! then
            then
         then
      then
   loop
   SKIP @ 0= if src u 32 WORD-END then ;

\ A scan of the text from an empty SPELLS; SPELL-TEXT after it appends another text's names.
: SPELLED ( ptr u8 n -- )
   {: src:ptr u:n :}
   0 OPENED-N !  SPELLS BUF:CLEAR  RUN!
   src u SPELL-TEXT ;

\ A new scan: no row marked, nor the build's body, nor a record (HIT-STAMPS).
: HITS-CLEAR ( -- )
   BUILT-N 1 + HITS-RESERVE
   BUILT-N 0 ?do 0 i HITS ! loop
   false BODY-HIT !
   2 HIT-STAMP +!  0 HIT-N !  NAMED-RECS BUF:CLEAR ;

\ After KEY!, before LINK-PART: INPUT-STAMP, the active file's change time seconds, nanoseconds and
\ size, and HITS-CLEAR, NAMES first taking the records the build's body made since the last file.
: INPUT! ( -- )
   SOURCE-ROOT:ACTIVE-STAMP {: sec:n ns:n size:n :}
   sec INPUT-STAMP LE:U64!  ns INPUT-STAMP 8 + LE:U64!  size INPUT-STAMP 16 + LE:U64!
   FILE-REC @ -1 INDEX-RECS
   HITS-CLEAR ;

\ False for bytes no file stamp covers (size -1): such a file never links.
: STAMPED? ( -- bool ) INPUT-STAMP 16 + LE:U64@ -1 <> ;

\ After PART-NAMES' scan, before WRITE-OBJECT: NAMED-ROWS from HITS, the pieces the source and its texts name whole, which
\ a requested file's load overwrites.
: NAMED! ( -- )
   NAMED-ROWS BUF:CLEAR
   BUILT-N 0 ?do i HITS @ 0<> if i NAMED-ROWS U64+ then loop ;

\ A record the source spells: its FILES row and WORDS row (WORD-REF), unless the object lists it.
: NAMED-REC+ ( n -- )
   {: idx:n :}
   idx 1 + LISTED-BY-RESERVE
   idx LISTED-BY @ OBJECT-SERIAL @ = if exit then
   idx WORD-REF  idx WORD-ROW+ ;

\ Whether emission e is a cross build's host copy, which the build's process runs and no output holds.
: HOST-COPY? ( n -- bool )
   {: e:n :}
   CROSS?  e NSHADOW:HOST@ e =  and ;

\ The record that owns emission e: its first shadow row's, a definition's, whose emission is e or,
\ for a host copy (HOST-COPY?), the target copy before it; E-COMP-STATE when no row is that
\ emission's or that row is a does> companion's. Emissions rise with the row (SHADOW-ROW).
: EM-OWNER ( n -- n )
   {: h:n :}
   h HOST-COPY? if h 1 - else h then {: e:n :}
   0 NSHADOW:RECORDS
   begin 2dup < while
      {: lo:n hi:n :}
      lo hi + 2 / {: mid:n :}
      mid NSHADOW:EMISSION@ e < if mid 1 + hi else lo mid then
   repeat
   drop {: row:n :}
   row NSHADOW:RECORDS >= if E-COMP-STATE throw then
   row NSHADOW:EMISSION@ e <> if E-COMP-STATE throw then
   row NSHADOW:ENTRY@ 0<> if E-COMP-STATE throw then
   row NSHADOW:RECORD@ ;

\ An earlier piece's emission e: its owning record's rows (NAMED-REC+), so a
\ reader of e's code compiles when that record's time changes.
: EM-NAMED+ ( n -- )
   EM-OWNER {: r:n :}
   r REC-ROW@ nip if r NAMED-REC+ then ;

\ After the files PIECES and REQUESTS reference: FILES gains a whole row for each piece the source
\ names whole, then each record of NAMED-RECS gains its rows (NAMED-REC+).
: NAMED+ ( -- )
   NAMED-ROWS BYTES$ {: a:ptr u:n :}
   u 8 / 0 ?do a i 8 * + LE:U64@ true REF-FILE drop loop
   NAMED-RECS BYTES$ {: r:ptr ru:n :}
   ru 8 / 0 ?do r i 8 * + LE:U64@ NAMED-REC+ loop ;


\ ---- dependencies by run --------------------------------------------------------
\ A part that ran an earlier word depends on what that word reaches through EDGES (docs/architecture.md "Builds").

\ A run log entry's record index onto RAN-RECS, unless the object took it already (RAN-BY); hb's
\ records are no seed.
: RAN-RECORD ( n -- )
   {: idx:n :}
   idx FIRST-REC @ < if exit then
   idx 1 + RAN-BY-RESERVE
   idx RAN-BY @ OBJECT-SERIAL @ = if exit then
   OBJECT-SERIAL @ idx RAN-BY !
   idx RAN-RECS U64+ ;


\ After PIECE+: the records of the words the piece ran onto RAN-RECS, its own among them.
: RAN+ ( -- ) INTERPRETER:RUN-COUNT FILE-RUN @ ?do i INTERPRETER:RUN-RECORD@ RAN-RECORD loop ;


\ The cell's id and kind (EDGE+).
: CELL-ID ( n -- n n ) dup 4 /  swap 3 and ;


\ FILES row k's EXTENTS row for build-table row b, unless the object already has one for it
\ (USED-BY).
: USED-EXT+ ( n n -- )
   {: k:n b:n :}
   b 1 + USED-BY-RESERVE
   b USED-BY @ OBJECT-SERIAL @ = if exit then
   OBJECT-SERIAL @ b USED-BY !
   k USED-EXTS U64+  b 1 BUILT-EXT @ USED-EXTS U64+  b 2 BUILT-EXT @ USED-EXTS U64+
   b 3 BUILT-EXT @ USED-EXTS U64+  b 6 BUILT-EXT @ USED-EXTS U64+ ;

\ The FILES row of BUILT row row, which holds build-table row b, asked for by b's EXTENTS row, or
\ whole for a laid text, which has no time.
: EXT-FILE ( n n -- n )
   {: b:n row:n :}
   b 6 BUILT-EXT @ 0<> {: timed:bool :}
   row  timed 0=  REF-FILE {: k:n :}
   timed if  k b USED-EXT+  then
   k ;


\ Whether the running walk reaches the record, emission or extent cell for the first time; it stamps the cell's
\ REC-SEEN, EM-SEEN or EXT-SEEN with REACH-STAMP.
: FIRST-SEEN? ( n -- bool )
   CELL-ID {: id:n kind:n :}
   kind 0 = if
      id 1 + REC-SEEN-RESERVE
      id REC-SEEN @ REACH-STAMP @ = if false exit then
      REACH-STAMP @ id REC-SEEN !  true exit
   then
   kind 1 = if
      id 1 + EM-SEEN-RESERVE
      id EM-SEEN @ REACH-STAMP @ = if false exit then
      REACH-STAMP @ id EM-SEEN !  true exit
   then
   id 1 + EXT-SEEN-RESERVE
   id EXT-SEEN @ REACH-STAMP @ = if false exit then
   REACH-STAMP @ id EXT-SEEN !  true ;


\ A reached cell onto REACH once; an earlier piece's record, emission
\ (EM-NAMED+) or extent gets its rows, the extent marking its row WALKED.
\ The running piece's extents, from BUILT-EXTS-N on, have no build-table row yet: they are followed and list nothing.
: SEE ( n -- )
   {: cell:n :}
   cell CELL-ID {: id:n kind:n :}
   kind 3 = if true HEADER-SEEN ! exit then
   kind 0 =  id FIRST-REC @ <  and if exit then
   cell FIRST-SEEN? 0= if exit then
   kind 2 = if
      id BUILT-EXTS-N @ < if
         id 0 BUILT-EXT @ {: row:n :}
         row WALKED @ 1 or  row WALKED !
         id row EXT-FILE drop
      then
   then
   REACH-N @ 1 + REACH-RESERVE
   cell REACH-N @ REACH !  1 REACH-N +!
   kind 0 =  id FILE-REC @ <  and if
      id REC-ROW@ nip if id NAMED-REC+ then
   then
   kind 1 =  id FILE-EM @ <  and if id EM-NAMED+ then ;


\ The EDGES span from first, count cells, each given to SEE.
: SEE-SPAN ( n n -- )
   {: first:n count:n :}
   first count +  first ?do i EDGES @ SEE loop ;


\ A reached cell's references, their first EDGES cell and count: a record's REC-EDGES span, an emission's EM-EDGES
\ span, an extent's EXT-EDGES span.
: CELL-EDGES ( n -- n n )
   CELL-ID {: id:n kind:n :}
   kind 0 = if
      id 1 + 2 * REC-EDGES-RESERVE
      id 2 * REC-EDGES @  id 2 * 1 + REC-EDGES @  exit
   then
   kind 1 = if
      id 1 + 2 * EM-EDGES-RESERVE
      id 2 * EM-EDGES @  id 2 * 1 + EM-EDGES @  exit
   then
   id 1 + 2 * EXT-EDGES-RESERVE
   id 2 * EXT-EDGES @  id 2 * 1 + EXT-EDGES @ ;

: FOLLOW ( n -- ) CELL-EDGES SEE-SPAN ;


\ REACH's cells from k on, each followed; REACH-N once every cell is followed.
: FOLLOW-REST ( n -- n )
   begin dup REACH-N @ < while  dup REACH @ FOLLOW  1 +  repeat ;


: HEADER-HELD? ( n -- bool ) BUILT-AT BUILT-HEADER + LE:U64@ 0<> ;

\ The compiling file holds a header-data address: its running piece, or an earlier piece of it.
: OWN-HEADER? ( -- bool )
   HOLDS-HEADER @ if true exit then
   BUILT-N 0 ?do
      i BUILT-AT KEY-BYTES  FILE-KEY KEY-BYTES  STR=  i HEADER-HELD?  and if true unloop exit then
   loop
   false ;


\ Each STORERS piece with stores not yet reached whose holder is WALKED, or is header data once own is true or the walk
\ reached a header-data reference: its stores marked reached, their ROW-EDGES span given to SEE. True if it marked one.
: WALK-STORERS ( bool -- bool )
   {: own:bool :}
   own if true else HEADER-SEEN @ then {: header:bool :}
   false  STORERS-N @ 0 ?do
      i 2 * STORERS @  i 2 * 1 + STORERS @ {: held:n row:n :}
      held -1 = if header else held WALKED @ 0<> then  row WALKED @ 2 and 0=  and if
         row WALKED @ 2 or  row WALKED !
         row 1 + 2 * ROW-EDGES-RESERVE
         row 2 * ROW-EDGES @  row 2 * 1 + ROW-EDGES @  SEE-SPAN
         drop true
      then
   loop ;


\ The walk from RAN-RECS' records, after NAMED+; REACHED takes each WALKED piece's FILES row and its stores time
\ (BUILT-STORES), 0 unless its stores are reached, then -1 when header storers count; REACHED-CK reads it before requests.
: RAN-FILES+ ( -- )
   REACHED BUF:CLEAR
   RAN-RECS BYTES$ nip 0= if exit then
   OWN-HEADER? {: own:bool :}
   BUILT-N 1 + WALKED-RESERVE
   BUILT-N 1 + 0 ?do 0 i WALKED ! loop
   1 REACH-STAMP +!  0 REACH-N !  false HEADER-SEEN !
   RAN-RECS BYTES$ nip 8 / 0 ?do  RAN-RECS BYTES$ drop i 8 * + LE:U64@ 4 * SEE  loop
   0 begin  FOLLOW-REST  own WALK-STORERS 0=  until drop
   BUILT-N 0 ?do
      i WALKED @ {: marks:n :}
      marks 0<> if
         i false REF-FILE REACHED U64+
         marks 2 and 0<> if  i BUILT-AT BUILT-STORES + LE:U64@  else 0 then  REACHED U64+
      then
   loop
   own HEADER-SEEN @ or if -1 REACHED U64+ then ;


\ One reference onto EDGES as 4 * id + kind: 0 a record by index, 1 an emission by shadow number, 2 an extent by its
\ build-table row (the running piece's extent e will be BUILT-EXTS-N plus e), 3 the header data.
: EDGE+ ( n -- )
   {: cell:n :}
   EDGES-N @ {: n:n :}
   n 1 + EDGES-RESERVE  cell n EDGES !  n 1 + EDGES-N ! ;

: REC-EDGE+ ( n -- ) 4 * EDGE+ ;
: EM-EDGE+ ( n -- ) 4 * 1 + EDGE+ ;
: DATA-EDGE+ ( n -- ) 4 * 2 + EDGE+ ;
: HEADER-EDGE+ ( -- ) 3 EDGE+ ;


\ An owner's span of EDGES, from the EDGES-N its references began at to EDGES-N now.
: REC-EDGES! ( n n -- )
   {: r:n start:n :}
   r 1 + 2 * REC-EDGES-RESERVE
   start r 2 * REC-EDGES !  EDGES-N @ start -  r 2 * 1 + REC-EDGES ! ;


: EM-EDGES! ( n n -- )
   {: e:n start:n :}
   e 1 + 2 * EM-EDGES-RESERVE
   start e 2 * EM-EDGES !  EDGES-N @ start -  e 2 * 1 + EM-EDGES ! ;


: ROW-EDGES! ( n n -- )
   {: row:n start:n :}
   row 1 + 2 * ROW-EDGES-RESERVE
   start row 2 * ROW-EDGES !  EDGES-N @ start -  row 2 * 1 + ROW-EDGES ! ;


: EXT-EDGES! ( n n -- )
   {: b:n start:n :}
   b 1 + 2 * EXT-EDGES-RESERVE
   start b 2 * EXT-EDGES !  EDGES-N @ start -  b 2 * 1 + EXT-EDGES ! ;


\ An address in the running part's data: tag 3 or 6 and its offset in the word's or string's extent (EXT-OF), else tag
\ 7 and its offset from FILE-DP. Its edge names the extent holding it, the head included; none when no extent does.
: OWN-DATA-REF+ ( n -- )
   {: v:n :}
   v EXT-OF {: e:n :}
   e 0 >= if
      BUILT-EXTS-N @ e + DATA-EDGE+
      e 1 EXT @ {: kind:n :}
      kind 1 =  kind 2 =  or if
         kind 1 = if 3 else 6 then PIECES U64+  0 PIECES U64+
         e 2 EXT @ PIECES U64+  e 3 EXT @ PIECES U64+  v e 0 EXT @ - PIECES U64+ exit
      then
   then
   7 PIECES U64+  0 PIECES U64+  v FILE-DP @ - PIECES U64+ ;

\ An address in BUILT row row's data, end included, and true: tag 3 or 6 by the build-table row holding it (ROW-EXT-OF)
\ or the head's tag 7, then the offset; its FILES row and edge name that row. False, writing nothing, if none holds it.
: ROW-DATA-REF+ ( n n -- bool )
   {: v:n row:n :}
   v row ROW-EXT-OF {: b:n :}
   b 0< if false exit then
   b 1 BUILT-EXT @  b 4 BUILT-EXT @ {: kind:n start:n :}
   v  start b 5 BUILT-EXT @ +  > if false exit then
   b row EXT-FILE {: k:n :}
   b DATA-EDGE+
   kind 3 = if  7 PIECES U64+  k PIECES U64+  v start - PIECES U64+  true exit  then
   kind 1 = if 3 else 6 then PIECES U64+  k PIECES U64+
   b 2 BUILT-EXT @ PIECES U64+  b 3 BUILT-EXT @ PIECES U64+  v start - PIECES U64+  true ;

\ A data address's reference, and true: a file's data a row names (ROW-DATA-REF+) or the open's header data, else
\ false. A file's end names it only after every span, so a later file's first byte names that later file.
: DATA-REF+ ( n -- bool )
   {: v:n :}
   v FILE-DP @ >=  v FILE-END @ <  and if  v OWN-DATA-REF+ true exit  then
   v DP-ROW {: row:n hit:bool :}
   hit if  v row ROW-DATA-REF+ exit  then
   v BASE @ >=  v OPEN-DP @ <  and if
      true HOLDS-HEADER !  HEADER-EDGE+  4 PIECES U64+  v BASE @ - PIECES U64+ true exit
   then
   v FILE-END @ = if  v OWN-DATA-REF+ true exit  then
   v DP-END-ROW {: end-row:n end-hit:bool :}
   end-hit if  v end-row ROW-DATA-REF+ exit  then
   false ;


\ ---- a refused reference --------------------------------------------------------
\ A value no reference can name ends the build with one error line naming the file, the
\ site and the value, then E-COMP-STATE. The scans run only on refusal.

\ The record whose code holds a, as its name and a's offset in it, hb's below FIRST-REC.
: CODE-VALUE. ( n -- )
   {: a:n :}
   ndict@ 0 ?do
      i XREF-REC XREF-START {: s:n :}
      a s >=  a s i XREF-REC XREF-CODE-BYTES + <  and if
         i FIRST-REC @ < if s" hb's " SAY then
         i XREF-REC XREF-NAME$ SAY  s"  +" SAY  a s - SAY-INT  unloop exit
      then
   loop
   s" code at " SAY  a SAY-INT  s"  in no record" SAY ;


\ Where the data address v lies: hb's DATA, the build's space outside every file's data,
\ or neither.
: DATA-VALUE. ( n -- )
   {: v:n :}
   v DATA-N >=  v DATA-N DATA-SIZE + <  and if s" hb's data +" SAY  v DATA-N - SAY-INT exit then
   v BASE @ >=  v BASE @ SPACE-SIZE @ + <  and if
      s" the build's space +" SAY  v BASE @ - SAY-INT  s" , in no file's data" SAY exit
   then
   v SAY-INT  s" , outside hb's data and the build's space" SAY ;

: VALUE. ( n bool -- ) if CODE-VALUE. else DATA-VALUE. then ;


\ The highest value at most at among emission e's data address values, -1 for none.
: EM-BELOW ( n n -- n )
   {: e:n at:n :}
   -1  e NSHADOW:ADDR-SITES 0 ?do
      e i NSHADOW:ADDR-SITE-KIND@ NEMIT:ADDR-DATA = if
         e i SITE-VALUE {: w:n held:bool :}
         held if
            w at <= if w max then
         then
      then
   loop ;


\ The record nearest below the cell at by the data addresses the build's emissions hold:
\ the first emission filed with the highest such address at most at, as its record's name
\ and at's offset from that address; nothing when none is at most at.
: OWNER. ( n -- )
   {: at:n :}
   -1 -1  NSHADOW:EMISSIONS 0 ?do
      i at EM-BELOW {: w:n :}
      w over > if 2drop i w then
   loop {: e:n v:n :}
   e 0< if exit then
   s"  in " SAY  e ROUTINE.  s"  +" SAY  at v - SAY-INT ;


: REFUSAL ( -- ) s" linker: " SAY  FILE-PATH FILE-PATH-N @ SAY  s" : " SAY ;
: REFUSED ( -- ) s" , which an object cannot name" SAY SAY-CR  E-COMP-STATE throw ;


\ Emission e's site holding v, a code address when code is true.
: REFUSE-SITE ( n n n bool -- )
   {: e:n site:n v:n code:bool :}
   REFUSAL  e ROUTINE.  s"  +" SAY  site SAY-INT  s"  names " SAY  v code VALUE.  REFUSED ;


\ The space's cell at holding v, a code address when code is true.
: REFUSE-CELL ( n n bool -- )
   {: at:n v:n code:bool :}
   REFUSAL  s" the cell at space +" SAY  at BASE @ - SAY-INT  at OWNER.
   s"  holds " SAY  v code VALUE.  REFUSED ;


\ RETIRED from the piece's held retirements, in held order (CHECK:HOLD-RETIRED).
: RETIRED! ( -- )
   0 RETIRED-N !
   CHECK:HELD-FACTS FILE-HELD @ ?do
      i CHECK:HELD-FACT@ {: ix:n a:ptr u:n :}
      ix CHECK:HELD-RETIRED = if
         i CHECK:HELD-RETIRED@ drop {: idx:n wid:n :}
         RETIRED-N @ 1 + 2 * RETIRED-RESERVE
         idx  RETIRED-N @ 2 * RETIRED !   wid  RETIRED-N @ 2 * 1 + RETIRED !
         1 RETIRED-N +!
      then
   loop ;

\ The first of RETIRED's rows retiring record idx, and true; false when the piece retired none.
: RETIRED-AT ( n -- n bool )
   {: idx:n :}
   RETIRED-N @ 0 ?do
      i 2 * RETIRED @ idx = if i true unloop exit then
   loop
   0 false ;

\ The wordlist cell record idx was made with, before the piece retired it.
: MADE-WID ( n -- n )
   {: idx:n :}
   idx RETIRED-AT {: k:n hit:bool :}
   hit if k 2 * 1 + RETIRED @ exit then
   idx XREF-REC XREF-WORDLIST ;

: PKG? ( n -- bool ) MADE-WID BOOT:NAMESPACE-WL = ;

\ A package record of this file whose private wordlist it minted right after its
\ public one: made with a private section, or given one with nothing minted between.
: OWN-PRIVATE? ( n -- bool )
   {: idx:n :}
   idx FILE-REC @ < if false exit then
   idx XREF-REC {: rec:ptr :}
   rec XREF-PKG-PRIVATE {: w:n :}
   w 0<>  w rec XREF-PKG-PUBLIC 1 + =  and ;

\ The wordlist's role: 0 global, 1 primitives, 2 package names, 5 types, or 3 or 4 a package's public or private one
\ followed by the package's record reference (ID-REF+), which is the build's and below the record.
: WORDLIST+ ( n -- )
   {: idx:n :}
   idx MADE-WID {: wid:n :}
   wid BOOT:GLOBAL-WL = if 0 PIECES U64+ exit then
   wid WL:PRIMITIVES = if 1 PIECES U64+ exit then
   wid BOOT:NAMESPACE-WL = if 2 PIECES U64+ exit then
   wid BOOT:TYPES-WL = if 5 PIECES U64+ exit then
   wid CHECK:WORDLIST-PACKAGE {: pkg:n hit:bool :}
   hit 0=  pkg FIRST-REC @ <  or  pkg idx >=  or if E-COMP-STATE throw then
   pkg XREF-REC XREF-PKG-PUBLIC wid = if 3 else 4 then PIECES U64+
   pkg ID-REF+ ;

variable SHADOW-AT                   \ SHADOW-ROW's cursor in the file's shadow rows

: SHADOW-LIVE? ( -- bool ) SHADOW-AT @ NSHADOW:RECORDS < ;

\ The file's shadow row of the record, or -1. Rows are in publication order, so
\ their record indices rise and the cursor, asked in index order, only advances.
: SHADOW-ROW ( n -- n )
   {: idx:n :}
   begin
      SHADOW-LIVE? if SHADOW-AT @ NSHADOW:RECORD@ idx < else false then
   while
      1 SHADOW-AT +!
   repeat
   SHADOW-LIVE? if
      SHADOW-AT @ NSHADOW:RECORD@ idx = if SHADOW-AT @ exit then
   then
   -1 ;

\ An alias's source: the lowest of the build's records before it with its start.
: ALIAS-SOURCE ( n -- n )
   {: idx:n :}
   idx XREF-REC XREF-START {: start:n :}
   idx FIRST-REC @ ?do
      i XREF-REC XREF-START start = if i unloop exit then
   loop
   E-COMP-STATE throw ;

: KIND ( n n -- n )
   {: idx:n row:n :}
   row 0 >= if
      row NSHADOW:ENTRY@ 0 > if KIND-DOES else KIND-DEF then exit
   then
   idx PKG? if KIND-PACKAGE exit then
   idx MADE-WID BOOT:TYPES-WL = if KIND-TYPE exit then
   KIND-ALIAS ;


\ Record r's emission, a cross build's target copy; -1 when no shadow row is r's or r is a does>
\ companion, which owns none.
: OWNER-EM ( n -- n )
   {: r:n :}
   0 NSHADOW:RECORDS
   begin 2dup < while
      {: lo:n hi:n :}
      lo hi + 2 / {: mid:n :}
      mid NSHADOW:RECORD@ r < if mid 1 + hi else lo mid then
   repeat
   drop {: row:n :}
   row NSHADOW:RECORDS >= if -1 exit then
   row NSHADOW:RECORD@ r <> if -1 exit then
   row NSHADOW:ENTRY@ 0<> if -1 exit then
   row NSHADOW:EMISSION@ ;

\ The record's dispatch cell, a defer's trailer cell, as a data reference; 5 for none.
: DISPATCH+ ( ptr n -- )
   {: rec:ptr :}
   rec NDICT:TRAILER-CELL {: cell:n :}
   cell 0= if 5 PIECES U64+ exit then
   cell DATA-REF+ if exit then
   REFUSAL  rec XREF-NAME$ SAY  s" 's dispatch cell is " SAY  cell false VALUE.  REFUSED ;

\ The build's TFAM:TYPE-WORD, the word every type entry's record runs.
: TYPE-SOURCE ( -- n )
   s" TFAM" BOOT:NAMESPACE-WL CHECKER-RESOLVE:WL-PROBE {: pkg:ptr :}
   -1  pkg XREF-FOUND? if
      drop  s" TYPE-WORD" pkg XREF-PKG-PUBLIC CHECKER-RESOLVE:WL-PROBE CHECKER-RESOLVE:REC>INDEX
   then {: src:n :}
   src FIRST-REC @ < if
      REFUSAL  s" a type entry needs the build's own TFAM:TYPE-WORD" SAY SAY-CR  E-COMP-STATE throw
   then
   src ;

\ The kind, name, flags without the name length and EXT, wordlist, then the kind's fields; KIND-PRIVATE and
\ KIND-SEALED entries are LATE+'s and SEAL+'s. Answers the record the entry binds: an alias's or type's source, else -1.
: RECORD+ ( n -- n )
   {: idx:n :}
   idx XREF-REC {: rec:ptr :}
   idx SHADOW-ROW {: row:n :}
   idx row KIND {: kind:n :}
   kind PIECES U64+
   rec XREF-NAME$ PIECES FIELD+
   rec XREF-FLAGS  DNAME-LEN-MASK DNAME-EXT or invert and  PIECES U64+
   idx WORDLIST+
   kind KIND-DEF = if rec DISPATCH+  -1 exit then
   kind KIND-DOES = if row NSHADOW:ENTRY@ PIECES U64+  -1 exit then
   kind KIND-PACKAGE = if
      idx OWN-PRIVATE? if 1 else 0 then PIECES U64+  -1 exit
   then
   kind KIND-TYPE = if TYPE-SOURCE exit then
   idx ALIAS-SOURCE {: src:n :}
   src ID-REF+  src ;

\ Word w's cell c in WORD-SPANS.
: WORD-SPAN ( n n -- ptr n )
   {: w:n c:n :}
   w SPAN-CELLS * c + WORD-SPANS ;

\ The bit into cell 6 of the word record idx is, when it is a record of the running piece.
: NAMED-BY! ( n n -- )
   {: idx:n bit:n :}
   idx FILE-REC @ < if exit then
   idx FILE-REC @ - 6 WORD-SPAN {: at:ptr :}
   at @ bit or  at ! ;

\ The edges to a definition's copies, emission e and, in a cross build, its host copy e + 1: a run
\ executes the host copy.
: COPY-EDGES+ ( n -- )
   {: e:n :}
   e EM-EDGE+  CROSS? if e 1 + EM-EDGE+ then ;

\ RECORD+, then the entry's span, emission (a cross build's target copy), highest FILES row
\ and bound record into WORD-SPANS; PIECE+ zeroed cell 6. E-COMP-STATE for an
\ emission the piece did not file.
: WORD-RECORD+ ( n -- )
   {: idx:n :}
   idx FILE-REC @ - {: w:n :}
   PIECES BYTES$ nip {: at:n :}
   0 TOP-ROW !
   EDGES-N @ {: start:n :}
   idx RECORD+ {: bound:n :}
   idx SHADOW-ROW {: row:n :}
   idx row KIND {: kind:n :}
   kind KIND-DEF =  kind KIND-DOES =  or {: coded:bool :}
   coded if row NSHADOW:EMISSION@ FILE-EM @ - else -1 then {: em:n :}
   coded  em 0<  em NSHADOW:EMISSIONS FILE-EM @ - >=  or  and if E-COMP-STATE throw then
   bound 0 >= if bound REC-EDGE+ then
   coded if  row NSHADOW:EMISSION@ COPY-EDGES+  then
   idx start REC-EDGES!
   at  w 0 WORD-SPAN !
   PIECES BYTES$ nip at -  w 1 WORD-SPAN !
   em  w 2 WORD-SPAN !
   0 w 3 WORD-SPAN !  0 w 4 WORD-SPAN !
   TOP-ROW @  w 5 WORD-SPAN !
   bound  w 7 WORD-SPAN ! ;

\ The late private wordlists in id order, 16 bytes each: the id, then the package.
: LATE-N ( -- n ) LATE BYTES$ {: a:ptr u:n :}  u 16 / ;

: LATE-AT ( n -- ptr u8 )
   {: k:n :}
   LATE BYTES$ {: a:ptr u:n :}  a k 16 * + ;

: LATE-W ( n -- n ) LATE-AT LE:U64@ ;
: LATE-P ( n -- n ) LATE-AT 8 + LE:U64@ ;

: LATE! ( n n n -- )
   {: w:n p:n k:n :}
   k LATE-AT {: at:ptr :}
   w at LE:U64!  p at 8 + LE:U64! ;

: LATE-SHIFT ( n -- )
   {: k:n :}
   k LATE-W  k LATE-P  k 1 + LATE! ;

\ The id and package into their place by id.
: LATE-INSERT ( n n -- )
   {: w:n p:n :}
   LATE-N {: n:n :}
   0  n 0 ?do i LATE-W w < if 1 + then loop {: k:n :}
   w LATE U64+  p LATE U64+
   n k ?do  n k + 1- i - LATE-SHIFT  loop
   w p k LATE! ;

\ True for a build's package record whose private wordlist this file's compile
\ minted (id at least FILE-WID), unless OWN-PRIVATE? holds for it.
: LATE? ( n -- bool )
   {: p:n :}
   p PKG? 0= if false exit then
   p XREF-REC XREF-PKG-PRIVATE FILE-WID @ >=  p OWN-PRIVATE? 0=  and ;

\ The build's package record whose late private wordlist (LATE?) is wid, else -1.
\ A package keeps one record when reopened, so wid names at most one.
: LATE-OF ( n -- n )
   {: wid:n :}
   wid CHECK:WORDLIST-PACKAGE {: p:n hit:bool :}
   hit 0= if -1 exit then
   p FIRST-REC @ < if -1 exit then
   p XREF-REC XREF-PKG-PRIVATE wid <> if -1 exit then
   p LATE? 0= if -1 exit then
   p ;


\ The wordlist ids the file's records account for: a package record's public one,
\ its own private one (OWN-PRIVATE?), and each late private one, among the ids
\ minted since FILE-WID.
: MINTED ( -- n )
   0
   ndict@ FILE-REC @ ?do
      i PKG? if
         1 +
         i OWN-PRIVATE? if 1 + then
      then
   loop
   WIDN-CELL HOST@ FILE-WID @ ?do
      i LATE-OF 0 >= if 1 + then
   loop ;

: LATE-COLLECT ( -- )
   WIDN-CELL HOST@ FILE-WID @ ?do
      i LATE-OF dup 0 >= if i swap LATE-INSERT else drop then
   loop ;

: LATE+ ( n -- )
   LATE-P {: p:n :}
   KIND-PRIVATE PIECES U64+  p ID-REF+  p NAMED-LATE NAMED-BY! ;

\ Writes into PIECES each late private wordlist from the kth whose id is below
\ the mark, and answers the next k.
: LATE-BELOW ( n n -- n )
   {: mark:n :}
   begin
      dup LATE-N < if dup LATE-W mark < else false then
   while
      dup LATE+  1 +
   repeat ;

: LATE-REST ( n -- ) LATE-N swap ?do i LATE+ loop ;

\ A record's mark: a package record's public id, the first it mints; any other
\ record's wordlist id plus one, so that wordlist comes before it.
: RECORD-MARK ( n -- n )
   {: idx:n :}
   idx PKG? if idx XREF-REC XREF-PKG-PUBLIC exit then
   idx MADE-WID 1 + ;

\ A code address's reference, and true: a record's entry, a quotation's function, or hb's own code, tested last since
\ a record's host copy may lie inside hb's code range; false, writing nothing, for any other address.
: CODE-REF+ ( n -- bool )
   {: a:n :}
   a REC-AT {: r:n :}
   r 0 >= if
      r NSHADOW:RECORD@ REC-EDGE+  0 PIECES U64+  r NSHADOW:RECORD@ ID-REF+ true exit
   then
   a NSHADOW:SOURCE-FUNCTION {: qe:n qf:n :}
   qe 0 >= if
      qe EM-EDGE+  2 PIECES U64+  qe EM-OWNER ID-REF+  qf PIECES U64+ true exit
   then
   a ARM64:HOST-KERNEL? if 1 PIECES U64+  a rbase - PIECES U64+ true exit then
   false ;

\ Whether a is the entry of one of hb's own words, which every build's process holds where this
\ hb's image puts it. A package's row holds a wordlist id where a word's holds its entry.
: HB-ENTRY? ( n -- bool )
   {: a:n :}
   FIRST-REC @ 0 ?do
      i XREF-REC XREF-WORDLIST XREF:PACKAGE-WL? 0= if
         i XREF-REC XREF-START a = if true unloop exit then
      then
   loop
   false ;

\ hb's own code a site of emission e may name as an offset from rbase: the kernel's, and in a host
\ copy (HOST-COPY?) any of hb's words, as a target word's host copy calls NDEFINITION:TARGET-ERROR.
: HB-CODE? ( n bool -- bool )
   {: a:n host:bool :}
   a ARM64:HOST-KERNEL? if true exit then
   host if a HB-ENTRY? else false then ;

\ The reference of emission e's site holding v: a function of e itself (OWN-FUNCTION) as a
\ quotation's, naming e's own record; else CODE-REF+; else hb's own code (HB-CODE?).
: SITE-REF+ ( n n -- bool )
   {: e:n v:n :}
   e v OWN-FUNCTION {: f:n :}
   f 0 >= if  e EM-EDGE+  2 PIECES U64+  e EM-OWNER ID-REF+  f PIECES U64+ true exit  then
   v CODE-REF+ if true exit then
   v  e HOST-COPY?  HB-CODE? 0= if false exit then
   1 PIECES U64+  v rbase - PIECES U64+ true ;


\ One emission of CODE+; address values are read from the shadow's bytes:
\ PIECES' copy has them cleared.
: EMISSION-CODE+ ( n -- )
   {: e:n :}
   e NSHADOW:SIZE {: s:n :}
   e NSHADOW:BYTES {: src:ptr :}
   e NSHADOW:PATCH-SITE@ {: p:n :}
   e NSHADOW:CALL-SITES {: calls:n :}
   s PIECES U64+  e NSHADOW:RET-BYTES PIECES U64+
   src s PIECES SPAN+
   PIECES BYTES$ nip s - {: off:n :}
   e NSHADOW:FUNCTIONS {: f:n :}
   f PIECES U64+
   f 0 ?do e i NSHADOW:FUNCTION-OFFSET@ PIECES U64+ loop
   0  calls 0 ?do e i NSHADOW:PATCH-CALL? 0= if 1 + then loop  PIECES U64+
   calls 0 ?do
      e i NSHADOW:PATCH-CALL? 0= if
         e i NSHADOW:CALL-KIND@ {: kind:n :}
         kind NEMIT:CALL <>  kind NEMIT:TAIL <>  and if E-COMP-STATE throw then
         e i NSHADOW:CALL-SITE@ PIECES U64+
         kind NEMIT:TAIL = if 1 else 0 then PIECES U64+
         e i NSHADOW:CALL-TARGET@ {: t:n :}
         e t SITE-REF+ 0= if e  e i NSHADOW:CALL-SITE@  t true REFUSE-SITE then
      then
   loop
   e NSHADOW:ADDR-SITES PIECES U64+
   e NSHADOW:ADDR-SITES 0 ?do
      e i NSHADOW:ADDR-SITE@ {: at:n :}
      e i NSHADOW:ADDR-SITE-KIND@ {: kind:n :}
      kind NEMIT:ADDR-CODE <>  kind NEMIT:ADDR-DATA <>  and if E-COMP-STATE throw then
      e i SITE-VALUE {: v:n held:bool :}
      held 0= if E-COMP-STATE throw then
      0  e NSHADOW:ARCH@  PIECES BYTES$ drop off +  s  at  kind  ADDRESS!
      0= if E-COMP-STATE throw then
      at PIECES U64+
      kind NEMIT:ADDR-CODE = {: code:bool :}
      code if
         0 PIECES U64+  e v SITE-REF+
      else
         1 PIECES U64+  v DATA-REF+
      then
      0= if e at v code REFUSE-SITE then
   loop
   p PIECES U64+
   p 0 >= if
      calls 0 > if calls 1 - else -1 then {: k:n :}
      k 0 >= if e k NSHADOW:PATCH-CALL? else false then if
         e k NSHADOW:CALL-TARGET@ {: t:n :}
         e t SITE-REF+ 0= if e p t true REFUSE-SITE then
      else
         5 PIECES U64+
      then
   then ;

\ EMISSION-CODE+, then its span and the highest FILES row it names into EM-SPANS, three cells.
: EM-CODE+ ( n -- )
   {: e:n :}
   e FILE-EM @ - 3 * {: at3:n :}
   at3 3 + EM-SPANS-RESERVE
   PIECES BYTES$ nip {: at:n :}
   0 TOP-ROW !
   EDGES-N @ {: start:n :}
   e EMISSION-CODE+
   e start EM-EDGES!
   at  at3 EM-SPANS !
   PIECES BYTES$ nip at -  at3 1 + EM-SPANS !
   TOP-ROW @  at3 2 + EM-SPANS ! ;


\ CODE, after the piece's entries, then DATA (DATA+); every number is 8 bytes, low
\ byte first:
\   E                      emissions this file's compile filed, from the file's first; a cross
\                          build's in pairs (COPIES), a definition's target copy in the target's
\                          site encodings and then its host copy (NSHADOW:HOST@)
\   per emission, in number order:
\     S, R                 size in bytes; return bytes (R <= S)
\     S raw bytes          unpadded, as the compile sealed them, except that each address
\                          value is written as 0 in its machine's encoding
\     F, then F offsets    function starts
\     C, then C call rows  site, kind (0 call, 1 tail), reference; sites strictly ascending, and
\                          no does> slot row
\     A, then A addr rows  site, kind (0 code, 1 data), reference; sites strictly ascending
\     P                    the does> slot's site, -1 for none, past the last call row's site; when
\                          P >= 0 its target's reference
\ A reference is a tag, then its fields: 0 a dictionary record (file, its name sum, its count
\ among that part's records with the sum); 1 hb's own code (offset from rbase: the kernel's, or
\ in a host copy any of hb's word entries, HB-CODE?); 2 a function of a definition, a quotation,
\ a clause, a callback's stub or one of the emission's own (file, its owning definition's name
\ sum and count as tag 0's, function); 3 a word's data and 6 a
\ string's (file, the extent's sum and count, offset in the extent, at most its length) or a text
\ an earlier part laid (sum 0 and its FACTS row's number for the extent's); 7 a file's head (file,
\ offset from that file's first data byte, at most the head's length, 0 for this object's part
\ with no extent); 4 the open's header data (offset from BASE); 5 none: a patch target, a cell
\ row's 0 or scratch cell, or a definition with no dispatch cell. A file field is 0 for this
\ object, else its FILES row from 1 (REF-FILE). Tags 3 and 6 name data by its owner, so data that
\ only moved keeps its references' bytes.
: CODE+ ( -- )
   ENTRIES
   NSHADOW:EMISSIONS FILE-EM @ - PIECES U64+
   CROSS? if
      NSHADOW:EMISSIONS FILE-EM @ ?do
         i NSHADOW:HOST@ i 1 + <> if E-COMP-STATE throw then
      2 +loop
   then
   NSHADOW:EMISSIONS FILE-EM @ ?do i EM-CODE+ loop ;

\ The bytes the file's compile laid, [FILE-DP, FILE-END).
: FILE-DATA ( -- ptr u8 n )
   NULL-PTR BYTE-VIEW FILE-DP @ +  FILE-END @ FILE-DP @ - ;

\ The distance from NULL-PTR of a row's cell, when it lies whole in the file's data.
: ROW-AT ( n -- n ) ROW-OFF BASE @ + ;
: IN-FILE? ( n -- bool ) ROW-AT {: at:n :}  at FILE-DP @ >=  at CELL + FILE-END @ <=  and ;

\ A cell row's kind: 0 xt, 1 data, 2 scratch.
: ROW-KIND ( n -- n )
   {: row:n :}
   row SCRATCH-ROW? if 2 exit then
   row DATA-ROW? if 1 else 0 then ;

\ A cell row's reference, from the space's value: PIECES' copy has it cleared.
: CELL-REF+ ( n -- )
   {: row:n :}
   row CELL-VALUE {: v:n :}
   row SCRATCH-ROW?  v 0=  or if 5 PIECES U64+ exit then
   row DATA-ROW? {: data:bool :}
   data if v DATA-REF+ else v CODE-REF+ then if exit then
   row ROW-AT v data 0= REFUSE-CELL ;

: BODY-U ( n -- n ) CELL - BASE @ - SPACE-AT @ ;

\ ---- extents ---------------------------------------------------------------------
\ Extents cut a piece's data at its anchors: kind 1 a record's (REC-ID), 2 a string's (STRING-COUNTS), 3 the head (0 0).

\ An anchor of the given rank at the address, when it lies in [FILE-DP, FILE-END].
: ANCHOR+ ( n n n -- )
   {: at:n rank:n x:n :}
   at FILE-DP @ <  at FILE-END @ >  or if exit then
   ANCHORS-N @ 3 * {: k:n :}
   k 3 + ANCHORS-RESERVE
   at k ANCHORS !  rank k 1 + ANCHORS !  x k 2 + ANCHORS !
   1 ANCHORS-N +! ;

\ A string body's length cell as an anchor, when the cell and the body lie in the piece's data.
: STRING+ ( n -- )
   {: b:n :}
   b CELL - FILE-DP @ <  b FILE-END @ >  or if exit then
   b b BODY-U +  FILE-END @ > if exit then
   b CELL -  2  b ANCHOR+ ;


\ The first data address emission e's code holds, -1 for none.
: EM-DATA ( n -- n )
   {: e:n :}
   e NSHADOW:BYTES {: src:ptr :}
   e NSHADOW:SIZE {: s:n :}
   e NSHADOW:ADDR-SITES 0 ?do
      e i NSHADOW:ADDR-SITE-KIND@ NEMIT:ADDR-DATA = if
         e i NSHADOW:ADDR-SITE@ {: c:n :}
         src c +  src s +  ADDRESS-CARRIER:CHAIN-SIZE {: size:n :}
         size 0 > if  src c +  size ADDRESS-CARRIER:CHAIN-VALUE  unloop exit  then
      then
   loop
   -1 ;


\ Whether anchor j sorts before anchor k: by address, then rank, then record index.
: ANCHOR< ( n n -- bool )
   {: j:n k:n :}
   j 3 * ANCHORS @  k 3 * ANCHORS @ {: a:n b:n :}
   a b <> if a b < exit then
   j 3 * 1 + ANCHORS @  k 3 * 1 + ANCHORS @ {: p:n q:n :}
   p q <> if p q < exit then
   j 3 * 2 + ANCHORS @  k 3 * 2 + ANCHORS @ < ;

\ Whether string extent j sorts before string extent k: by sum, then address.
: EXT-SUM< ( n n -- bool )
   {: j:n k:n :}
   j 2 EXT @  k 2 EXT @ {: a:n b:n :}
   a b <> if a b < exit then
   j k < ;

\ The address where extent e ends.
: EXT-END ( n -- n )
   {: e:n :}
   e 0 EXT @  e 9 EXT @ + ;


\ FNV-1a over the bytes, as NAME-SUM without its case fold; never 0.
: BYTES-SUM ( ptr u8 n -- n )
   {: a:ptr u:n :}
   $811C9DC5  u 0 ?do  a i + c@ xor  $1000193 *  loop
   dup 0= if 1 + then ;

\ The extent at the address onto EXTS, with no span, rows, length or time yet.
: EXT+ ( n n n n -- )
   {: at:n kind:n sum:n count:n :}
   EXTS-N @ {: e:n :}
   e 1 + EXT-CELLS * EXTS-RESERVE
   at e 0 EXT !  kind e 1 EXT !  sum e 2 EXT !  count e 3 EXT !
   EXT-CELLS 4 ?do 0 e i EXT ! loop
   1 EXTS-N +! ;

\ Each string extent's count: its number, in address order, among the piece's strings with its sum.
: STRING-COUNTS ( -- )
   EXTS-N @ ORDER-RESERVE
   0  EXTS-N @ 0 ?do  i 1 EXT @ 2 = if  i over ORDER !  1 +  then  loop {: n:n :}
   n 1 > if 0 ORDER n [: EXT-SUM< ;] SORT:SORT! then
   0 0  n 0 ?do
      {: prev:n c:n :}
      i ORDER @ {: e:n :}
      e 2 EXT @ {: s:n :}
      s prev = if c 1 + else 0 then {: k:n :}
      k e 3 EXT !
      s k
   loop 2drop ;


\ The running piece's extents into EXTS: anchors at each DNAME-ALLOC record's body (EM-DATA) and dispatch cell and each
\ string's length cell, sorted, after a head when D > 0 and no anchor is at FILE-DP; each runs to the next or FILE-END.
: EXTENTS! ( -- )
   0 ANCHORS-N !  0 EXTS-N !
   ndict@ FILE-REC @ ?do
      i XREF-REC {: rec:ptr :}
      rec XREF-FLAGS DNAME-ALLOC and 0<> if
         i OWNER-EM {: e:n :}
         e 0 >= if  e EM-DATA {: a:n :}  a 0 >= if a 1 i ANCHOR+ then  then
      then
      rec NDICT:TRAILER-CELL {: cell:n :}
      cell 0<> if cell 1 i ANCHOR+ then
   loop
   ['] STRING+ INTERN:EACH-STRING
   ANCHORS-N @ {: n:n :}
   n ORDER-RESERVE
   n 0 ?do i i ORDER ! loop
   n 1 > if 0 ORDER n [: ANCHOR< ;] SORT:SORT! then
   FILE-END @ FILE-DP @ > if
      n 0= if true else 0 ORDER @ 3 * ANCHORS @ FILE-DP @ > then
      if FILE-DP @ 3 0 0 EXT+ then
   then
   n 0 ?do
      i ORDER @ 3 * {: k:n :}
      k ANCHORS @  k 1 + ANCHORS @  k 2 + ANCHORS @ {: at:n rank:n x:n :}
      rank 1 = if
         at 1 x REC-ID EXT+
      else
         at 2  NULL-PTR BYTE-VIEW x +  x BODY-U  BYTES-SUM  0 EXT+
      then
   loop
   EXTS-N @ 0 ?do
      i 1 +  EXTS-N @ < if  i 1 + 0 EXT @  else  FILE-END @  then  i 0 EXT @ -  i 9 EXT !
   loop
   STRING-COUNTS ;


\ The running piece's cell rows into EXT-ROWS, grouped by extent and in the space's order within one: a cell not
\ 8-aligned from FILE-DP, crossing its extent's end or ending past the data is E-COMP-STATE.
: GROUP-ROWS ( -- )
   ROWS {: rows:ptr count:n :}
   0  count 0 ?do
      rows i cells + @ {: row:n :}
      row IN-FILE? if
         row ROW-AT {: at:n :}
         at FILE-DP @ - 7 and 0<> if E-COMP-STATE throw then
         at EXT-OF {: e:n :}
         at CELL +  e EXT-END > if E-COMP-STATE throw then
         dup 2 * 2 + PICKED-RESERVE
         row over 2 * PICKED !  e over 2 * 1 + PICKED !
         1 e 8 EXT +!
         1 +
      else
         row ROW-AT FILE-DP @ >= if E-COMP-STATE throw then
      then
   loop {: n:n :}
   0  EXTS-N @ 0 ?do  dup i 7 EXT !  i 8 EXT @ +  0 i 8 EXT !  loop drop
   n EXT-ROWS-RESERVE
   n 0 ?do
      i 2 * PICKED @  i 2 * 1 + PICKED @ {: row:n e:n :}
      row  e 7 EXT @  e 8 EXT @ +  EXT-ROWS !
      1 e 8 EXT +!
   loop ;


\ Extent e onto PIECES (DATA+), its span and the highest FILES row its rows name into EXTS, and
\ its rows' references as the EXT-EDGES of its build-table row to be, BUILT-EXTS-N plus e.
: EXTENT+ ( n -- )
   {: e:n :}
   PIECES BYTES$ nip {: from:n :}
   e 0 EXT @ {: at:n :}
   e EXT-END at - {: u:n :}
   e 1 EXT @ PIECES U64+  e 2 EXT @ PIECES U64+  e 3 EXT @ PIECES U64+  u PIECES U64+
   NULL-PTR BYTE-VIEW at +  u  PIECES SPAN+
   PIECES BYTES$ {: buf:ptr len:n :}
   buf len + u - {: bytes:ptr :}
   e 7 EXT @  e 8 EXT @ {: first:n k:n :}
   k 0 ?do  0  bytes  first i + EXT-ROWS @ ROW-AT at - +  LE:U64!  loop
   k PIECES U64+
   0 TOP-ROW !
   EDGES-N @ {: start:n :}
   k 0 ?do
      first i + EXT-ROWS @ {: row:n :}
      row ROW-AT at - PIECES U64+  row ROW-KIND PIECES U64+  row CELL-REF+
   loop
   BUILT-EXTS-N @ e +  start EXT-EDGES!
   from e 4 EXT !  PIECES BYTES$ nip from - e 5 EXT !  TOP-ROW @ e 6 EXT ! ;

\ DATA, STORES and FACTS, after CODE:
\   D                      the bytes the file's compile laid, FILE-END - FILE-DP
\   X                      the extents (EXTENTS!)
\   X starts               each extent's offset from FILE-DP, ascending, the first 0
\   per extent             kind, sum, count, length: the distance to the next start, the last's to D
\                          length raw bytes, each address cell's 8 bytes written as 0
\                          K, then K cell rows: offset from the extent's start, kind (ROW-KIND),
\                          reference (CELL-REF+); in the order of the space's address-cell rows
\                          (GROUP-ROWS); a kind-2 extent's first 8 bytes are its string's length
\                          and its body follows
\   S, then S store rows   each cell below FILE-DP the compile stored into, other than a scratch
\                          cell, or gave an address-cell row, ascending by address: its
\                          reference (tag 3, 6 or 7 with a FILES row, or 4), kind (ROW-KIND, 3 for
\                          none), bytes (the cell's value for kind 3, else 0), its value's
\                          reference (CELL-REF+ for kinds 0-2, else 5)
\   F, then F facts rows   each entry the file's compile held, in order (FACTS+): CHECK:HELD-RETIRED
\                          then the retired record's reference (ID-REF+) and how many of the
\                          part's own records the compile had made by then; CHECK:HELD-AMENDED, a
\                          record's reference and its text as a FIELD+; CHECK:HELD-LAID for a
\                          laid text or CHECK:HELD-UNDEFINE for an `undefine`, then the text as a
\                          FIELD+
\ An extent's span runs from its kind through its last cell row; the starts lie outside every span,
\ so an extent that only moved keeps its span's bytes.
: DATA+ ( -- )
   GROUP-ROWS
   FILE-END @ FILE-DP @ - PIECES U64+
   EXTS-N @ PIECES U64+
   EXTS-N @ 0 ?do  i 0 EXT @ FILE-DP @ - PIECES U64+  loop
   EXTS-N @ 0 ?do  i EXTENT+  loop ;

\ ---- the stores ----------------------------------------------------------------
\ A header cell below the address-cell rows is found only by the mark xt! and ptr-cell! leave (MARKED+), others BELOW.

\ The compared cells, as [from, to) offsets from BASE: the header above its address-cell rows,
\ the open's data and every earlier file's data.
: BELOW ( -- n n ) SNAP-RELOC:XTCELL-END  FILE-DP @ BASE @ - ;


: SPACE-BYTES ( n -- ptr u8 ) NULL-PTR BYTE-VIEW BASE @ + swap + ;

\ The band's cell that marks the header cell at offset off: a routed store wrote the cell's
\ address there (MAP).
: MARK-AT ( n -- ptr n )
   {: off:n :}
   NULL-PTR BYTE-VIEW  BASE @ ROUTED-BYTES - off +  + CELL-VIEW ;

\ The band's last cell: nonzero once a routed store marked a cell.
: ANY-MARK ( -- ptr n ) NULL-PTR BYTE-VIEW  BASE @ CELL -  + CELL-VIEW ;

: CLEAR-MARKS ( -- )
   ANY-MARK @ 0= if exit then
   DATA-START CELL / 0 ?do 0 i cells MARK-AT ! loop  0 ANY-MARK ! ;


: COPY-REC ( n -- ) XREF-REC BYTE-VIEW DREC OLD-RECS SPAN+ ;


: COPY-RANGE ( n n -- )
   {: from:n to:n :}
   from SPACE-BYTES  to from -  OLD-SPACE SPAN+ ;


\ What the file's compile may change below its own data that no mark shows, as it starts, and
\ no routed store's mark.
: COPY-BELOW ( -- )
   OLD-SPACE BUF:CLEAR  OLD-RECS BUF:CLEAR  CLEAR-MARKS
   BELOW COPY-RANGE
   FILE-REC @ FIRST-REC @ 1 - ?do i COPY-REC loop
   NSHADOW:PATCHES OLD-PATCHES !
   ROWS nip FILE-CELLS ! ;


\ Earlier record k's copy (COPY-BELOW) and its index.
: OLD-REC ( n -- ptr u8 n )
   {: k:n :}
   OLD-RECS BYTES$ drop  k DREC * +  FIRST-REC @ 1 - k + ;

: OLD-RECS-N ( -- n ) OLD-RECS BYTES$ nip DREC / ;


\ True when the file's compile sealed the build's package record, given its copy: the flags
\ differ by DNAME-SEALED alone, which the record now holds.
: SEALED-NOW? ( ptr u8 n -- bool )
   {: old:ptr idx:n :}
   idx FIRST-REC @ < if false exit then
   idx PKG? 0= if false exit then
   old XREF-FLAGS-SLOT cells + CELL-VIEW @ {: was:n :}
   idx XREF-REC XREF-FLAGS {: now:n :}
   was now xor DNAME-SEALED =  now DNAME-SEALED and 0<>  and ;


\ True when a cell of the record differs from its copy, other than a late private wordlist (LATE?),
\ a seal (SEALED-NOW?) or a retirement (RETIRED).
: REC-CHANGED? ( ptr u8 n -- bool )
   {: old:ptr idx:n :}
   idx XREF-REC BYTE-VIEW {: rec:ptr :}
   idx RETIRED-AT nip {: retired:bool :}
   DREC CELL / 0 ?do
      old i cells + CELL-VIEW @  rec i cells + CELL-VIEW @  <> if
         i XREF-LEN-SLOT =  idx LATE?  and
         i XREF-FLAGS-SLOT =  old idx SEALED-NOW?  and  or
         i XREF-WORDLIST-SLOT =  retired  and  or
         0= if true unloop exit then
      then
   loop
   false ;


\ E-STORE when the compile changed an earlier record or patched an earlier emission's does>
\ slot. The shadow changes no other part of an emission once it is published.
: CHECK-EARLIER ( -- )
   OLD-RECS-N 0 ?do i OLD-REC REC-CHANGED? if E-STORE throw then loop
   NSHADOW:PATCHES OLD-PATCHES @ ?do
      i NSHADOW:PATCHED@ FILE-EM @ < if E-STORE throw then
   loop ;


: STORED+ ( n -- )
   {: v:n :}
   STORED-N @ 1 + STORED-RESERVE
   v STORED-N @ STORED !  1 STORED-N +! ;

\ The running piece's store into the cell at v, recorded when the cell is header data; the
\ piece's BUILT row is the next.
: HSTORE+ ( n -- )
   {: v:n :}
   v OPEN-DP @ < 0= if exit then
   HSTORES-N @ 1 + {: n:n :}
   n HSTORE-AT-RESERVE  n HSTORE-ROW-RESERVE
   v BASE @ - HSTORES-N @ HSTORE-AT !
   BUILT-N HSTORES-N @ HSTORE-ROW !
   n HSTORES-N ! ;


\ The pair (holder row held, the running piece's row) onto STORERS unless it is the newest pair;
\ the piece's BUILT row is the next.
: PAIR+ ( n -- )
   {: held:n :}
   STORERS-N @ {: n:n :}
   n 0 > if
      n 1 - 2 * STORERS @ held =  n 1 - 2 * 1 + STORERS @ BUILT-N =  and if exit then
   then
   n 1 + 2 * STORERS-RESERVE
   held n 2 * STORERS !  BUILT-N n 2 * 1 + STORERS !
   n 1 + STORERS-N ! ;

\ The running piece's store into the cell at v, as a STORERS pair. A store into the cell of no
\ BUILT row's data, above the header data, is E-COMP-STATE.
: STORER+ ( n -- )
   {: v:n :}
   v OPEN-DP @ < if -1 else v DP-ROW 0= if E-COMP-STATE throw then then  PAIR+ ;

\ The running piece changed an earlier piece's record (facts, retirement, seal, late wordlist): its name in NAMES names
\ the running piece whole, so a file spelling it depends on that piece; STORERS pairs the piece with the record's row.
: CHANGER+ ( n -- )
   {: idx:n :}
   idx FILE-REC @ >= if exit then
   true CHANGED-EARLIER !
   idx  -2 BUILT-N -  REC-NAME+
   idx BUILT-ROW PAIR+ ;


\ The space's address-cell row of the cell at v, and true; false when it has none.
: ROW-OF ( n -- n bool )
   {: v:n :}
   ROWS {: rows:ptr count:n :}
   count 0 ?do
      rows i cells + @ {: row:n :}
      row ROW-AT v = if row true unloop exit then
   loop
   0 false ;


\ Whether the cell at v has a scratch row: its value is one process's, and a link
\ writes it zero.
: SCRATCH-CELL? ( n -- bool ) ROW-OF if SCRATCH-ROW? else drop false then ;


\ Each cell of [from, to) that differs from its copy in OLD-SPACE, which starts at from, into
\ STORED, other than a scratch cell.
: COMPARE-RANGE ( n n -- )
   {: from:n to:n :}
   OLD-SPACE BYTES$ drop {: old:ptr :}
   from SPACE-BYTES {: now:ptr :}
   to from - CELL / 0 ?do
      old i cells + CELL-VIEW @  now i cells + CELL-VIEW @  <> if
         BASE @ from + i cells + dup SCRATCH-CELL? if drop else STORED+ then
      then
   loop ;


\ A store's cell lies in an earlier file's data or the header data.
: HELD? ( n -- bool )
   {: v:n :}
   v DP-ROW nip  v OPEN-DP @ <  or ;


\ Drops repeats from the sorted list: a changed cell with a new row is one store.
: DEDUPE ( -- )
   STORED-N @ {: n:n :}
   n 0= if exit then
   1  n 1 ?do
      i STORED @  over 1 - STORED @  <> if i STORED @ over STORED !  1 + then
   loop  STORED-N ! ;


\ The header cell at offset o, when a routed store marked it, into STORED; a mark other than the
\ cell's address is a store off a cell boundary: E-COMP-STATE.
: MARKED+ ( n -- )
   {: o:n :}
   o MARK-AT @ {: m:n :}
   m 0= if exit then
   BASE @ o + {: v:n :}
   m v <> if E-COMP-STATE throw then
   v STORED+ ;


\ STORED, ascending: each changed non-scratch cell BELOW, each header cell xt! or ptr-cell! stored (MARKED+) and each
\ new row's cell below FILE-DP, aligned or E-COMP-STATE; E-STORE for a cell no earlier file or the header holds.
: FIND-STORES ( -- )
   0 STORED-N !
   BELOW COMPARE-RANGE
   ANY-MARK @ 0<> if DATA-START CELL / 0 ?do i cells MARKED+ loop then
   ROWS {: rows:ptr count:n :}
   count FILE-CELLS @ ?do
      rows i cells + @ ROW-AT {: at:n :}
      at FILE-DP @ < if
         at BASE @ - 7 and 0<> if E-COMP-STATE throw then
         at STORED+
      then
   loop
   STORED-N @ {: n:n :}
   n 0 > if 0 STORED n [: < ;] SORT:SORT! then
   DEDUPE
   STORED-N @ 0 ?do i STORED @ HELD? 0= if E-STORE throw then loop ;


\ STORES, after DATA's extents (DATA+).
: STORES+ ( -- )
   STORED-N @ PIECES U64+
   STORED-N @ 0 ?do
      i STORED @ {: v:n :}
      HOLDS-HEADER @ {: held:bool :}  EDGES-N @ {: edges:n :}
      v DATA-REF+ 0= if E-COMP-STATE throw then   \ FIND-STORES refused a cell nothing holds
      held HOLDS-HEADER !  edges EDGES-N !        \ the store's cell is no address or edge the piece holds
      v ROW-OF {: row:n hit:bool :}
      hit if
         row ROW-KIND PIECES U64+  0 PIECES U64+  row CELL-REF+
      else
         3 PIECES U64+  v BASE @ - SPACE-AT @ PIECES U64+  5 PIECES U64+
      then
   loop ;


\ ---- the facts texts ------------------------------------------------------------
\ LAST-HELD from the entries the file's compile held; E-STORE for one naming a record no file
\ made.
: LAST-HELD! ( -- )
   ndict@ FILE-REC @ - {: recs:n :}
   recs 0 > if recs LAST-HELD-RESERVE then
   recs 0 ?do -1 i LAST-HELD ! loop
   CHECK:HELD-FACTS FILE-HELD @ ?do
      i CHECK:HELD-FACT@ {: ix:n a:ptr u:n :}
      ix ndict@ >= if E-STORE throw then
      ix FILE-REC @ >= if  i  ix FILE-REC @ - LAST-HELD !  then
   loop ;


\ Every laid text is kept, every text for an earlier file's record, and a record's last text: an
\ earlier one is never read. An amended record's entry is not: the text written again for it is.
: KEPT? ( n -- bool )
   {: k:n :}
   k CHECK:HELD-FACT@ {: ix:n a:ptr u:n :}
   ix CHECK:HELD-AMENDED = if false exit then
   ix FILE-REC @ < if true exit then
   ix FILE-REC @ - LAST-HELD @  k = ;


\ A facts row: a retirement's kind, record (NAMED-BY! marks the running piece's word) and count of
\ the piece's records made before it; a record's text, own or earlier, by reference; a kind's text.
: FACT+ ( n -- )
   {: k:n :}
   k CHECK:HELD-FACT@ {: ix:n a:ptr u:n :}
   ix CHECK:HELD-RETIRED = if
      k CHECK:HELD-RETIRED@ nip {: r:n made:n :}
      r FILE-REC @ < if true CHANGED-EARLIER ! then
      ix PIECES U64+  r ID-REF+  made FILE-REC @ - PIECES U64+  r NAMED-RETIRED NAMED-BY!  exit
   then
   ix 0 >= if
      ix FILE-REC @ < if true CHANGED-EARLIER ! then
      CHECK:HELD-AMENDED PIECES U64+  ix ID-REF+  a u PIECES FIELD+  exit
   then
   ix PIECES U64+  a u PIECES FIELD+ ;


\ FACT+, then for a text of the piece's own record its span into WORD-SPANS.
: WORD-FACT+ ( n -- )
   {: k:n :}
   k CHECK:HELD-FACT@ {: ix:n a:ptr u:n :}
   ix FILE-REC @ < if k FACT+ exit then
   ix FILE-REC @ - {: w:n :}
   PIECES BYTES$ nip {: at:n :}
   k FACT+
   at  w 3 WORD-SPAN !
   PIECES BYTES$ nip at -  w 4 WORD-SPAN ! ;


\ FACTS, after STORES, in held order. Each text is copied before anything writes facts,
\ which can overwrite it (CHECK:HELD-FACT@).
: FACTS+ ( -- )
   LAST-HELD!
   0  CHECK:HELD-FACTS FILE-HELD @ ?do i KEPT? if 1 + then loop  PIECES U64+
   CHECK:HELD-FACTS FILE-HELD @ ?do  i KEPT? if i WORD-FACT+ then  loop ;


: SEALED-N ( -- n ) 0  OLD-RECS-N 0 ?do i OLD-REC SEALED-NOW? if 1 + then loop ;

\ A KIND-SEALED entry for the earlier record when the file's compile sealed it.
: SEAL+ ( ptr u8 n -- )
   {: old:ptr idx:n :}
   old idx SEALED-NOW? 0= if exit then
   KIND-SEALED PIECES U64+  idx ID-REF+  idx CHANGER+ ;


\ An event of the loader's log an ACTS row carries: a require the registry already held, or a
\ provide.
: ACT? ( n -- bool )
   {: ev:n :}
   ev EVENT-KIND@ EV-PROVIDED =
   ev EVENT-KIND@ EV-REQUIRED =  ev EVENT-STATE@ EV-STATE-KNOWN =  and  or ;

\ ACTS from the loader's log (include.f EVENT-RECORD); req >= 0 leaves out the newest event, the ending request's own.
\ Any other event is a load the source caught before the build's runner ran it, which a link cannot repeat: UNLINKABLE.
: ACTS+ ( n -- )
   {: req:n :}
   EVENT-COUNT  req 0 >= if 1 - then {: end:n :}
   EVENT-ON? 0=  end FILE-EVENT @ <  or if E-COMP-STATE throw then
   0  end FILE-EVENT @ ?do i ACT? if 1 + then loop  PIECES U64+
   end FILE-EVENT @ ?do
      i ACT? if
         i EVENT-KIND@ PIECES U64+  i EVENT-SPELLED@ PIECES FIELD+  i SOURCE-EVENT:CURRENT@ PIECES FIELD+
      else
         true UNLINKABLE !
      then
   loop ;


\ Records by index, each late private wordlist before the first record marked above its id, so ids mint in entry order;
\ sealed packages last, so a link seals once the definitions are in. req: 0 a request ends the part, -1 the file ends.
: PIECE+ ( n -- )
   {: req:n :}
   false CHANGED-EARLIER !
   IDENTIFY  EXTENTS!
   LATE BUF:CLEAR
   FILE-ROW @ SHADOW-AT !
   LATE-COLLECT
   ndict@ FILE-REC @ -  LATE-N +  SEALED-N +  PIECES U64+
   ndict@ FILE-REC @ - WORDS-N !
   WORDS-N @ SPAN-CELLS * WORD-SPANS-RESERVE
   WORDS-N @ 0 ?do  0 i 6 WORD-SPAN !  loop
   0  ndict@ FILE-REC @ ?do  i RECORD-MARK LATE-BELOW  i WORD-RECORD+  loop
   LATE-REST
   LATE-N 0 ?do i LATE-P CHANGER+ loop
   OLD-RECS-N 0 ?do i OLD-REC SEAL+ loop
   CODE+  DATA+  EDGES-N @ {: start:n :}
   PIECES BYTES$ nip STORES-AT !  STORES+  BUILT-N start ROW-EDGES!
   PIECES BYTES$ nip FACTS-AT !  FACTS+
   req ACTS+
   req PIECES U64+ ;

\ As the file's compile starts: no FILES, WORDS, EXTENTS, REQUESTS, RAN-RECS, TIMES or piece yet,
\ and a new serial.
: OBJECT-OPEN ( -- )
   REFS BUF:CLEAR  WORD-ROWS BUF:CLEAR  WORD-RECS BUF:CLEAR  USED-EXTS BUF:CLEAR  REQUESTS BUF:CLEAR  PIECES BUF:CLEAR
   RAN-RECS BUF:CLEAR  TIMES BUF:CLEAR
   1 SERIALS +!  SERIALS @ OBJECT-SERIAL ! ;

\ OBJ-PATHS: FILE-KEY's path, then each REFS row's, once each.
: PATHS-LAY ( -- )
   OBJ-PATHS BUF:CLEAR
   FILE-KEY CANON$ OBJ-PATHS PATH-IN drop
   REFS BYTES$ {: refs:ptr refsu:n :}
   refsu FILES-ROW-BYTES / 0 ?do  refs i FILES-ROW-BYTES * + CANON$ OBJ-PATHS PATH-IN drop  loop ;

\ OBJECT's FILES rows at the offset, each key's first cell made its path's offset in OBJ-PATHS.
: KEYS-OUT ( n -- )
   {: files-at:n :}
   REFS BYTES$ {: refs:ptr refsu:n :}
   refsu FILES-ROW-BYTES / 0 ?do
      refs i FILES-ROW-BYTES * + CANON$ OBJ-PATHS PATH-IN
      OBJECT BYTES$ drop files-at + i FILES-ROW-BYTES * +  LE:U64!
   loop ;

\ Part FILE-PART's object with time t, built for PIECE-PLACE's facts and written only with a stamp and for hb's target;
\ INPUT laid out as STAMP-BYTES says; an UNLINKABLE part's stamp is 0 0 -1, so it never links and keeps its times.
: WRITE-OBJECT ( n -- )
   {: t:n :}
   \ FILES ends with the pieces the part's source names (NAMED+), then those a run word reaches
   \ (RAN-FILES+); WORDS lists the words the part refers to (ID-REF+), its source spells (NAMED+)
   \ and a run reaches; EXTENTS the earlier extents its data references name (ROW-DATA-REF+).
   NAMED+  RAN-FILES+  PATHS-LAY
   REFS BYTES$ {: refs:ptr refsu:n :}
   REQUESTS BYTES$ {: reqs:ptr requ:n :}
   SPELLS BYTES$ {: nm:ptr nmu:n :}
   REACHED BYTES$ {: rch:ptr rchu:n :}
   OBJ-PATHS BYTES$ {: pth:ptr pthu:n :}
   WORD-ROWS BYTES$ {: wds:ptr wdsu:n :}
   USED-EXTS BYTES$ {: exts:ptr extsu:n :}
   TIMES BYTES$ {: tms:ptr tmsu:n :}
   PIECES BYTES$ {: pieces:ptr piecesu:n :}
   UNLINKABLE @ if 0 else ENTRY-ROOT BYTES$ nip  EXIT-ROOT BYTES$ nip +  PART-READS BYTES$ nip + then
   STAMP-BYTES + 24 + {: inputu:n :}
   HEADER-BYTES inputu + {: files-at:n :}
   files-at refsu + {: reqs-at:n :}
   reqs-at requ + {: names-at:n :}
   names-at nmu + {: reached-at:n :}
   reached-at rchu + {: paths-at:n :}
   paths-at pthu + {: words-at:n :}
   words-at wdsu + {: exts-at:n :}
   exts-at extsu + {: times-at:n :}
   times-at tmsu + {: pieces-at:n :}
   OBJECT BUF:CLEAR
   s" HABUFOBJ" OBJECT SPAN+
   0 OBJECT U64+  FILE-KEY 8 + LE:U64@ OBJECT U64+
   FILE-PART @ OBJECT U64+  t OBJECT U64+  FILE-OFF @ OBJECT U64+
   HEADER-BYTES OBJECT U64+  inputu OBJECT U64+
   refsu 0<> if files-at else 0 then OBJECT U64+  refsu OBJECT U64+
   requ 0<> if reqs-at else 0 then OBJECT U64+  requ OBJECT U64+
   nmu 0<> if names-at else 0 then OBJECT U64+  nmu OBJECT U64+
   rchu 0<> if reached-at else 0 then OBJECT U64+  rchu OBJECT U64+
   paths-at OBJECT U64+  pthu OBJECT U64+
   wdsu 0<> if words-at else 0 then OBJECT U64+  wdsu OBJECT U64+
   extsu 0<> if exts-at else 0 then OBJECT U64+  extsu OBJECT U64+
   times-at OBJECT U64+  tmsu OBJECT U64+
   pieces-at OBJECT U64+  piecesu OBJECT U64+
   UNLINKABLE @ if
      0 OBJECT U64+  0 OBJECT U64+  -1 OBJECT U64+
      0 OBJECT U64+  0 OBJECT U64+  0 OBJECT U64+
   else
      INPUT-STAMP STAMP-BYTES OBJECT SPAN+
      ENTRY-ROOT BYTES$ OBJECT FIELD+  EXIT-ROOT BYTES$ OBJECT FIELD+  PART-READS BYTES$ OBJECT FIELD+
   then
   refs refsu OBJECT SPAN+
   reqs requ OBJECT SPAN+
   nm nmu OBJECT SPAN+
   rch rchu OBJECT SPAN+
   pth pthu OBJECT SPAN+
   wds wdsu OBJECT SPAN+
   exts extsu OBJECT SPAN+
   tms tmsu OBJECT SPAN+
   pieces piecesu OBJECT SPAN+
   OBJ-TAKE drop                       \ PIECE-PLACE reads the object with no RD-OPEN first
   files-at KEYS-OUT
   pieces-at STORES-AT +!  pieces-at FACTS-AT +!
   STAMPED? 0= if exit then
   \ Unsynced: an object a crash cut short fails HEADER-CK's or PIECE-CK's bounds checks and compiles.
   OBJECT$ {: obj:ptr obju:n :}
   obj obju OBJECT-DIR
   obj obju [: OBJECT BYTES$ FS:WRITE-ALL ;] FS:REPLACE-STAGED ;

\ The running part's extents (EXTS) into the build table as BUILT row BUILT-N's, each also into
\ PLACES under -3; lengths and times come from EXTS, since FILE-END by now may lie past laid texts.
: BUILT-EXTS+ ( -- )
   EXTS-N @ {: n:n :}
   BUILT-N {: row:n :}
   BUILT-EXTS-N @ {: first:n :}
   n PLACES-ROOM
   first n + BUILT-EXT-CELLS * BUILT-EXTS-RESERVE
   n 0 ?do
      first i + {: b:n :}
      i 1 EXT @  i 2 EXT @  i 3 EXT @ {: kind:n sum:n count:n :}
      row b 0 BUILT-EXT !  kind b 1 BUILT-EXT !  sum b 2 BUILT-EXT !  count b 3 BUILT-EXT !
      i 0 EXT @ b 4 BUILT-EXT !  i 9 EXT @ b 5 BUILT-EXT !  i 10 EXT @ b 6 BUILT-EXT !
      -3 row kind sum count b PLACE!
   loop
   n BUILT-EXTS-N +!
   row 1 + 2 * ROW-EXTS-RESERVE
   first row 2 * ROW-EXTS !  n row 2 * 1 + ROW-EXTS ! ;

\ The FACTS row at the offset in OBJECT (FACTS-CK): the next row's offset, and the offset of its
\ text's length cell, -1 for a retirement, which has no text.
: FACT-ROW ( n -- n n )
   {: at:n :}
   at OBJ-PTR LE:U64@ {: ix:n :}
   ix CHECK:HELD-RETIRED = if  at 40 +  -1 exit  then
   ix CHECK:HELD-AMENDED = if at 32 + else at 8 + then {: text:n :}
   text 8 +  text OBJ-PTR LE:U64@ +  text ;

\ BUILT row row's build-table row for the text laid at start, FACTS row count's, with no time and
\ no references, put into PLACES under -3; TEXT-EXTS+ sets its length.
: TEXT-EXT+ ( n n n -- )
   {: count:n start:n row:n :}
   BUILT-EXTS-N @ {: b:n :}
   b 1 + BUILT-EXT-CELLS * BUILT-EXTS-RESERVE
   b 1 + 2 * EXT-EDGES-RESERVE  0 b 2 * EXT-EDGES !  0 b 2 * 1 + EXT-EDGES !
   1 PLACES-ROOM
   row b 0 BUILT-EXT !  2 b 1 BUILT-EXT !  0 b 2 BUILT-EXT !  count b 3 BUILT-EXT !
   start b 4 BUILT-EXT !  0 b 5 BUILT-EXT !  0 b 6 BUILT-EXT !
   -3 row 2 0 count b PLACE!
   1 BUILT-EXTS-N +! ;

\ After BUILT-EXTS+, each text the part laid past its D bytes (in FACTS row order, as LINK-FACTS lays them) for BUILT-N:
\ one whose length cell lies past the last added and below FILE-END; a lower one is an earlier row's or part's.
: TEXT-EXTS+ ( -- )
   EXTS-N @ 0= if FILE-DP @ else EXTS-N @ 1 - EXT-END then {: dend:n :}
   FILE-END @ {: end:n :}
   dend end = if exit then
   BUILT-N {: row:n :}
   BUILT-EXTS-N @ {: first:n :}
   0 OBJ-PTR {: obj:ptr :}
   FACTS-AT @ 8 +  dend 1 -
   obj FACTS-AT @ + LE:U64@ 0 ?do
      {: at:n last:n :}
      at FACT-ROW {: next:n text:n :}
      text 0< if next last else
         obj text 8 + +  obj text + LE:U64@  INTERN:FIND 8 - {: cell:n :}
         cell last >  cell end <  and if
            i cell row TEXT-EXT+  next cell
         else next last then
      then
   loop 2drop
   BUILT-EXTS-N @ {: stop:n :}
   stop first ?do
      i 1 + stop < if i 1 + 4 BUILT-EXT @ else end then  i 4 BUILT-EXT @ -  i 5 BUILT-EXT !
   loop
   row 2 * 1 + ROW-EXTS @  stop first - +  row 2 * 1 + ROW-EXTS ! ;

\ Part FILE-PART's BUILT row with time t and stores time st, its extents and laid texts in the build table
\ (BUILT-EXTS+, TEXT-EXTS+); NAMES takes its records and REC-ROW gives each its row, rows rising.
: BUILT+ ( n n -- )
   {: t:n st:n :}
   BUILT-EXTS+  TEXT-EXTS+
   ndict@ REC-ROW-RESERVE
   FILE-REC @ REC-ROW-N @ ?do  0 i REC-ROW !  loop
   ndict@ FILE-REC @ ?do  BUILT-N 1 + i REC-ROW !  loop
   ndict@ REC-ROW-N !
   IDENTIFY
   ndict@ BUILT-N INDEX-RECS
   BUILT-N 1 + BUILT-PATHS-RESERVE  KEY-PATH BUILT-N BUILT-PATHS !
   FILE-KEY KEY-BYTES BUILT SPAN+
   FILE-PART @ BUILT U64+  t BUILT U64+
   FILE-REC @ BUILT U64+  ndict@ BUILT U64+
   FILE-EM @ BUILT U64+  NSHADOW:EMISSIONS BUILT U64+
   FILE-DP @ BUILT U64+  FILE-END @ BUILT U64+
   HOLDS-HEADER @ if 1 else 0 then BUILT U64+  st BUILT U64+ ;

\ ---- reading an object ---------------------------------------------------------
\ PART-FRESH? checks the part's object in OBJECT; these, ENTRY-OFFS, EM-OFFS and FILE-ROWS hold once it answers true.
variable ENTRIES-N                    \ the object's entries
variable RECS-N                       \ its records: entries neither KIND-PRIVATE nor KIND-SEALED
variable EMS-N                        \ its emissions, E
variable DATA-AT                      \ the offset in OBJECT of DATA's D
variable EXTS-AT                      \ of its X
variable REQS-AT                      \ the offset in OBJECT of its REQUESTS
variable REQS-N                       \ its REQUESTS rows: 1 when a request ends the part, else 0 (REQUESTS-CK)
variable PIECE-AT                     \ the offset in OBJECT of the part's piece (HEADER-CK)
\ Only the check uses these and DOES-AT.
variable FILES-N                      \ its FILES rows
variable DEFS-N                       \ the KIND-DEF entries read so far
variable DOES-N                       \ the KIND-DOES entries read so far, DOES-AT's rows
variable LAST-EM                      \ the entry just read's emission when it is KIND-DEF, else -1

\ The reader over OBJECT: [RD-AT, RD-END), and a failure that sticks until RD-OPEN.
\ A failure also empties the window, so a later read fails on its bounds alone.
ENUM reading good bad ;ENUM
TYPED-VARIABLE READER reading
variable RD-AT
variable RD-END

: BAD ( -- ) construct reading bad READER !  RD-AT @ RD-END ! ;


: GOOD? ( -- bool )
   READER @
   MATCH reading
      good OF true ENDOF
      bad OF false ENDOF
   ;MATCH ;


: RD-OPEN ( n n -- )
   {: at:n end:n :}
   at RD-AT !  end RD-END !
   construct reading good READER !
   OBJ-TAKE {: len:n :}
   at 0<  at end >  or  end len >  or if BAD then ;


\ Advances n bytes, and true; bad, and false, when they pass RD-END.
: RD-STEP ( n -- bool )
   {: u:n :}
   u 0<  u RD-END @ RD-AT @ - >  or if BAD false exit then
   u RD-AT +!  true ;

: RD-SKIP ( n -- ) RD-STEP drop ;


\ The next 8 bytes, low byte first; 0 once bad.
: RD-U64 ( -- n )
   RD-AT @ {: at:n :}
   8 RD-STEP 0= if 0 exit then
   at OBJ-PTR LE:U64@ ;


: RD-EQ ( n -- ) RD-U64 <> if BAD then ;


\ Bad unless the next u bytes equal the span's.
: RD-SAME ( ptr u8 n -- )
   {: a:ptr u:n :}
   RD-AT @ {: at:n :}
   u RD-STEP 0= if exit then
   at OBJ-PTR u  a u  STR= 0= if BAD then ;


\ A count of rows of at least n bytes each; bad, and 0, when the rest cannot hold them.
: RD-COUNT ( n -- n )
   {: row:n :}
   RD-U64 {: k:n :}
   k 0<  k  RD-END @ RD-AT @ - row /  >  or if BAD 0 exit then
   k ;


: OBJ@ ( n -- n ) OBJ-PTR LE:U64@ ;
: PRIOR@ ( n -- n ) PRIOR BYTES$ drop swap + LE:U64@ ;


\ FILE-ROWS' value for a FILES row no BUILT row holds.
-2 constant ROW-ABSENT

\ FILES row k's BUILT row's number at the byte offset (BUILT's layout).
: BUILT@ ( n n -- n )
   {: k:n off:n :}
   k FILE-ROWS @ BUILT-AT off + LE:U64@ ;


: FILE-ROW? ( n -- bool )
   {: k:n :}
   k 1 >=  k FILES-N @ <=  and ;

\ FILES row k, from 1, in OBJECT.
: FILES-ROW ( n -- ptr u8 )
   {: k:n :}
   PAIRS-AT 16 + OBJ@  k 1 - FILES-ROW-BYTES * +  OBJ-PTR ;

\ A FILES row a reference may name: one a BUILT row holds.
: FILE-IN? ( n -- bool )
   {: k:n :}
   k FILE-ROW? 0= if false exit then
   k FILE-ROWS @ ROW-ABSENT <> ;


\ The head's length in FILES row k's part, -1 when the part's first build-table row is no head
\ or it has none.
: HEAD-LEN ( n -- n )
   {: k:n :}
   k FILE-ROWS @ {: row:n :}
   row 2 * 1 + ROW-EXTS @ 0= if -1 exit then
   row 2 * ROW-EXTS @ {: b:n :}
   b 1 BUILT-EXT @ 3 <> if -1 exit then
   b 5 BUILT-EXT @ ;


\ This object's emission e's F, after its S, R and S bytes.
: OWN-FUNS ( n -- n )
   EM-OFFS @ {: at:n :}
   at 16 + at OBJ@ + OBJ@ ;


\ This object's emission e's function j's offset, j below F; the offsets follow F.
: OWN-FUN-AT ( n n -- n )
   {: e:n j:n :}
   e EM-OFFS @ {: at:n :}
   at 24 + at OBJ@ +  j 8 * + OBJ@ ;


\ A reference: its tag and four fields, 0 for absent ones (CODE+ states them).
: RD-REF ( -- n n n n n )
   RD-U64 {: tag:n :}
   tag 0 = if tag RD-U64 RD-U64 RD-U64 0 exit then
   tag 1 = if tag RD-U64 0 0 0 exit then
   tag 2 =  tag 3 =  tag 6 =  or or if tag RD-U64 RD-U64 RD-U64 RD-U64 exit then
   tag 7 = if tag RD-U64 RD-U64 0 0 exit then
   tag 4 = if tag RD-U64 0 0 0 exit then
   tag 5 = if tag 0 0 0 0 exit then
   BAD tag 0 0 0 0 ;


\ The kind of this object's record r, an entry already read; -1 for none.
: REC-KIND ( n -- n )
   {: r:n :}
   r 0<  r RECS-N @ >=  or if -1 exit then
   r REC-KINDS @ ;


\ A record reference (file, sum, count; ID-REF+): this part's record number below the bound
\ (OWN-REC), or an earlier part's record (PLACE); -1 once the object is BAD.
: ID-REF-CK ( n n n n -- n )
   {: file:n sum:n count:n bound:n :}
   GOOD? 0= if -1 exit then
   file 0= if
      sum count OWN-REC {: o:n :}
      o 0<  o bound >=  or if BAD -1 exit then
      o exit
   then
   file FILE-IN? 0= if BAD -1 exit then
   file FILES-ROW sum count PLACE {: r:n :}
   r 0< if BAD then
   r ;


\ A quotation (file, its owner's sum and count, function f): the owner is this part's record with an emission at most
\ top (OWN-REC) or an earlier part's (PLACE) with one (OWNER-EM); f is below that emission's function count.
: FN-REF-CK ( n n n n n -- )
   {: file:n sum:n count:n f:n top:n :}
   f 0< if BAD exit then
   file 0= if
      sum count OWN-REC {: o:n :}
      o 0<  o RECS-N @ >=  or if BAD exit then
      o REC-EMS @ {: e:n :}
      e 0<  e EMS-N @ >=  or  e top >  or if BAD exit then
      f e OWN-FUNS >= if BAD then exit
   then
   file FILE-IN? 0= if BAD exit then
   file FILES-ROW sum count PLACE {: r:n :}
   r 0< if BAD exit then
   r OWNER-EM {: em:n :}
   em 0< if BAD exit then
   f em NSHADOW:FUNCTIONS >= if BAD then ;


\ A code reference (RD-REF) that RESOLVE resolves: an own emission at most top, an own record below bound, and a record
\ (OWN-REC, PLACE) that is no package.
: CODE-REF-CK ( n n n n n n n -- )
   {: tag:n x:n y:n z:n w:n top:n bound:n :}
   GOOD? 0= if exit then
   tag 0 = if
      x y z bound ID-REF-CK {: r:n :}
      r 0< if exit then
      x 0= if  r REC-KIND KIND-PACKAGE = if BAD then exit  then
      r PKG? if BAD then exit
   then
   tag 1 = if rbase x + ARM64:HOST-KERNEL? 0= if BAD then exit then
   tag 2 = if x y z w top FN-REF-CK exit then
   BAD ;


\ A site's code reference of this object's emission top (CODE-REF-CK), hb's own code as HB-CODE? allows: a cross
\ object's odd emission is a host copy (COPIES).
: SITE-REF-CK ( n n n n n n n -- )
   {: tag:n x:n y:n z:n w:n top:n bound:n :}
   GOOD? 0= if exit then
   tag 1 = if  rbase x +  COPIES 2 =  top 1 and 1 =  and  HB-CODE? 0= if BAD then exit  then
   tag x y z w top bound CODE-REF-CK ;


\ A data reference (RD-REF): a word's (tag 3), string's or laid text's (tag 6) extent here (OWN-EXT) or built (ROW-EXT),
\ or a part's head (tag 7, HEAD-LEN; 0 when none here), the offset at most its length; or header data (tag 4).
: DATA-REF-CK ( n n n n n -- )
   {: tag:n x:n y:n z:n w:n :}
   GOOD? 0= if exit then
   tag 3 =  tag 6 =  or if
      w 0< if BAD exit then
      tag 3 = if 1 else 2 then {: kind:n :}
      x 0= if
         kind y z OWN-EXT {: e:n :}
         e 0< if BAD exit then
         w e 9 EXT @ > if BAD then exit
      then
      x FILE-IN? 0= if BAD exit then
      x FILE-ROWS @ kind y z ROW-EXT {: b:n :}
      b 0< if BAD exit then
      w b 5 BUILT-EXT @ > if BAD then exit
   then
   tag 7 = if
      y 0< if BAD exit then
      x 0= if
         EXTS-N @ 0= if  y 0<> if BAD then exit  then
         0 1 EXT @ 3 <> if BAD exit then
         y 0 9 EXT @ > if BAD then exit
      then
      x FILE-IN? 0= if BAD exit then
      y x HEAD-LEN > if BAD then exit
   then
   tag 4 = if
      x 0<  x OPEN-DP @ BASE @ - >=  or if BAD then exit
   then
   BAD ;


\ Whether the FILES row names the BUILT row: its key and piece number, and its time 0 (REF-FILE) or
\ the BUILT row's.
: ROW-FITS? ( ptr u8 ptr u8 -- bool )
   {: f:ptr b:ptr :}
   f KEY-BYTES 8 +  b KEY-BYTES 8 +  STR= 0= if false exit then
   f BUILT-TIME + LE:U64@ {: t:n :}
   t 0=  t  b BUILT-TIME + LE:U64@  =  or ;


\ The BUILT row the FILES row names (ROW-FITS?), and true; false when none.
: BUILT-MATCH ( ptr u8 -- n bool )
   {: at:ptr :}
   BUILT-N 0 ?do
      at i BUILT-AT ROW-FITS? if i true unloop exit then
   loop
   0 false ;


\ FILE-ROWS from 1 for OBJECT's FILES rows: each its BUILT row, else ROW-ABSENT.
: MAP-FILES ( -- )
   FILES-N @ 0 ?do
      i 1 + FILES-ROW {: at:ptr :}
      at BUILT-MATCH if
         i 1 + FILE-ROWS !
      else
         drop  ROW-ABSENT i 1 + FILE-ROWS !
      then
   loop ;


\ FILES' rows, u bytes, mapped (MAP-FILES); PIECE-CK requires every row built (ROWS-CK).
: FILES-CK ( n -- )
   {: u:n :}
   0 FILES-N !
   GOOD? 0= if exit then
   u FILES-ROW-BYTES / {: n:n :}
   u RD-SKIP
   GOOD? 0= if exit then
   n FILES-N !
   n 1 + FILE-ROWS-RESERVE
   MAP-FILES ;


: ROWS-CK ( -- )
   FILES-N @ 0 ?do i 1 + FILE-ROWS @ ROW-ABSENT = if BAD unloop exit then loop ;


\ After FILES-CK: bad when the source or a text it evaluated names a record of the build's body, or names a piece whole
\ (NAMES-CK) that no whole FILES row, one with the BUILT row's time, lists.
: NAMED-CK ( -- )
   GOOD? 0= if exit then
   BODY-HIT @ if BAD exit then
   FILES-N @ 0 ?do
      i 1 + FILE-ROWS @ {: row:n :}
      row 0 >= if
         i 1 + FILES-ROW BUILT-TIME + LE:U64@  row BUILT-AT BUILT-TIME + LE:U64@  =
         row HITS @ 0<>  and if 2 row HITS ! then
      then
   loop
   BUILT-N 0 ?do i HITS @ 1 = if BAD then loop ;


\ REACHED's u bytes, (FILES row, stores time) pairs then -1 (RAN-FILES+), marked in WALKED as it marks: bad unless rows
\ rise in build order, a stores time is 0 or its BUILT row's, and each storer into listed or header data has one not 0.
: REACHED-CK ( n -- )
   {: u:n :}
   GOOD? 0= if exit then
   u 0= if exit then
   BUILT-N 1 + WALKED-RESERVE  BUILT-N 0 ?do 0 i WALKED ! loop
   -1  u 16 / 0 ?do
      {: last:n :}
      RD-U64 RD-U64 {: k:n st:n :}
      k FILE-ROW? 0= if BAD unloop exit then
      k FILE-ROWS @ {: row:n :}
      row 0 >= if
         row last <= if BAD unloop exit then
         st 0<>  st  row BUILT-AT BUILT-STORES + LE:U64@  <>  and if BAD unloop exit then
         st 0<> if 3 else 1 then  row WALKED !  row
      else
         last
      then
   loop
   drop
   u 16 mod 8 = {: header:bool :}
   header if  RD-U64 -1 <> if BAD exit then  then
   STORERS-N @ 0 ?do
      i 2 * STORERS @ {: held:n :}
      held -1 = if header else held WALKED @ 0<> then if
         i 2 * 1 + STORERS @ WALKED @ 2 and 0= if BAD then
      then
   loop ;


\ A name: its length, at least 1, then its bytes.
: NAME-CK ( -- ) RD-U64 {: u:n :}  u 1 < if BAD exit then  u RD-SKIP ;

\ A name: its length, then its bytes, which stay in OBJECT.
: RD-NAME ( -- ptr u8 n )
   RD-U64 {: u:n :}
   RD-AT @ OBJ-PTR {: a:ptr :}
   u RD-SKIP
   a u ;


\ REQUESTS, u bytes from RD-AT (WRITE-OBJECT): no row, or one, the request that ends the part:
\ its kind, resume offset, spelled and loaded paths. REQS-N counts it.
: REQUESTS-CK ( n -- )
   {: u:n :}
   0 REQS-N !
   RD-AT @ u + {: end:n :}
   GOOD?  RD-AT @ end <  and if
      RD-U64 {: kind:n :}
      kind SOURCE-UNIT:LOAD-INCLUDE <>  kind SOURCE-UNIT:LOAD-REQUIRE <>  and if BAD then
      RD-U64 0< if BAD then
      NAME-CK  NAME-CK
      1 REQS-N !
   then
   RD-AT @ end <> if BAD then ;


\ NAMES' u bytes (SPELLS) run as SPELLED and SPELL-TEXT ran them: HIT each sum, OPENED+ the sum after a 0; for a source INPUT-STAMP
\ stamps, HITS, HIT-STAMPS and HIT-N end as a scan's.
: NAMES-CK ( n -- )
   {: u:n :}
   GOOD? 0= if exit then
   0 OPENED-N !
   RD-AT @ u + {: end:n :}
   begin GOOD?  RD-AT @ end <  and while
      RD-U64 {: sum:n :}
      sum 0<> if sum HIT else
         RD-AT @ end < if RD-U64 OPENED+ else BAD then
      then
   repeat ;


\ The REQUESTS row's offset in OBJECT: the kind, the resume offset (NEST), then the two paths.
: REQ-AT ( -- n ) REQS-AT @ ;

\ The REQUESTS row's kind and the offsets of its spelled and loaded paths.
: REQ-FIELDS ( -- n n n )
   REQ-AT {: at:n :}
   at 16 + {: spelled:n :}
   spelled 8 + spelled OBJ@ + {: loaded:n :}
   at OBJ@  spelled  loaded ;

\ The REQUESTS row's resume offset: where the requester's source goes on after the request.
: REQ-RESUME ( -- n ) REQ-AT 8 + OBJ@ ;


\ The FIELD+ at the offset in OBJECT.
: FIELD$ ( n -- ptr u8 n )
   {: at:n :}
   at 8 + OBJ-PTR  at OBJ@ ;


\ The part's time in OBJECT: the header's.
: PIECE-TIME ( -- n ) 8 KEY-BYTES + 8 + OBJ@ ;

\ The offset in OBJECT of TIMES' word count, once HEADER-CK passed.
: TIMES-AT ( -- n ) TIMES-FIELD OBJ@ ;

\ The offset in OBJECT of TIMES' row for extent e, after the word rows and the extent count.
: TIMES-EXT ( n -- n )
   {: e:n :}
   TIMES-AT {: at:n :}
   at 8 +  at OBJ@ WORD-BYTES * +  8 +  e EXT-BYTES * + ;


\ The offset in OBJECT of TIMES' stores entry (STORES-TIMES+), after the extent rows.
: TIMES-STORES ( -- n )
   TIMES-AT {: at:n :}
   at 8 +  at OBJ@ WORD-BYTES * +  OBJ@ TIMES-EXT ;

\ The part's stores time in OBJECT: its TIMES stores entry's, or its time (PIECE-TIME) for a changer's 0.
: PIECE-STORES-TIME ( -- n )
   TIMES-STORES OBJ@ {: st:n :}
   st 0= if PIECE-TIME exit then
   st ;


\ Places the part's records (TIMES order, key -2, a new OWN-SERIAL) and extents (EXTS, laid from FILE-DP); bad on a
\ repeated identity or a span too short for its length cell. EXTENT-CK checks the rest against DATA.
: OWN-IDS! ( -- )
   TIMES-AT {: at:n :}
   1 OWN-SERIAL +!
   at OBJ@ {: n:n :}
   at 8 +  n WORD-BYTES * +  OBJ@ {: x:n :}
   n x + PLACES-ROOM
   n 0 ?do
      at 8 +  i WORD-BYTES * + {: w:n :}
      w 9 8 * + OBJ@  w 10 8 * + OBJ@ {: sum:n count:n :}
      sum count OWN-REC 0 >= if BAD unloop exit then
      -2 OWN-SERIAL @ 0 sum count i PLACE!
   loop
   x EXT-CELLS * EXTS-RESERVE
   x EXTS-N !
   FILE-DP @  x 0 ?do
      {: start:n :}
      i TIMES-EXT {: r:n :}
      r 16 + OBJ@ 32 < if BAD unloop exit then
      r 24 + OBJ@  r 32 + OBJ@  r 40 + OBJ@ {: kind:n sum:n count:n :}
      PIECE-AT @  r 8 + OBJ@ +  24 + OBJ@ {: u:n :}
      kind sum count OWN-EXT 0 >= if BAD unloop exit then
      start i 0 EXT !  kind i 1 EXT !  sum i 2 EXT !  count i 3 EXT !  u i 9 EXT !
      r OBJ@ i 10 EXT !
      -2 OWN-SERIAL @ kind sum count i PLACE!
      start u +
   loop drop ;


\ A TIMES span: an offset and a length inside a piece of plen bytes.
: SPAN-CK ( n -- )
   {: plen:n :}
   RD-U64 RD-U64 {: off:n u:n :}
   off 0<  u 0<  or  off plen >  or  u plen off - >  or if BAD then ;


\ TIMES (TIMES+, STORES-TIMES+) from RD-AT to PIECES at pieces-at: each word's and extent's spans and the stores
\ entry's lie in the piece, which runs to the object's end at len; other fields, a changer's 0 too, are skipped.
: TIMES-CK ( n n -- )
   {: pieces-at:n len:n :}
   GOOD? 0= if exit then
   len pieces-at - {: plen:n :}
   WORD-BYTES RD-COUNT 0 ?do
      8 RD-SKIP  plen SPAN-CK  plen SPAN-CK  plen SPAN-CK  32 RD-SKIP
   loop
   EXT-BYTES RD-COUNT 0 ?do  8 RD-SKIP  plen SPAN-CK  24 RD-SKIP  loop
   8 RD-SKIP  plen SPAN-CK
   RD-AT @ pieces-at <> if BAD then ;


\ A WORDS row's built record: found listed when the running scan hit it (HIT-STAMPS).
: FOUND-LISTED ( n -- )
   {: r:n :}
   r 1 + HIT-STAMPS-RESERVE
   r HIT-STAMPS @ HIT-STAMP @ <> if exit then
   HIT-STAMP @ 1 + r HIT-STAMPS !  -1 HIT-N +! ;

\ WORDS' u bytes (WORD-ROW+): bad when a row's FILES row is missing, unbuilt with time 0 (REF-FILE), or built with no
\ record of its sum and count at its time (PLACE), or a spelled record no row lists (NAMES-CK); ROWS-CK checks the rest.
: WORDS-CK ( n -- )
   {: u:n :}
   GOOD? 0= if exit then
   u WORD-ROW-BYTES / 0 ?do
      RD-U64 RD-U64 RD-U64 RD-U64 {: k:n sum:n count:n t:n :}
      k FILE-ROW? 0= if BAD unloop exit then
      k FILES-ROW BUILT-TIME + LE:U64@ 0=  k FILE-ROWS @ 0<  and if BAD unloop exit then
      k FILE-ROWS @ 0 >= if
         k FILES-ROW sum count PLACE {: r:n :}
         r 0< if BAD unloop exit then
         r REC-TIME  t <> if BAD unloop exit then
         r FOUND-LISTED
      then
   loop
   HIT-N @ 0<> if BAD then ;


\ EXTENTS' u bytes (USED-EXT+): each an extent of a built part (ROW-EXT) with the time the row records, never 0.
: EXTENTS-CK ( n -- )
   {: u:n :}
   GOOD? 0= if exit then
   u USED-EXT-BYTES / 0 ?do
      RD-U64 RD-U64 RD-U64 RD-U64 RD-U64 {: k:n kind:n sum:n count:n t:n :}
      k FILE-IN? 0=  t 0=  or if BAD unloop exit then
      k FILE-ROWS @ kind sum count ROW-EXT {: b:n :}
      b 0< if BAD unloop exit then
      b 6 BUILT-EXT @  t <> if BAD unloop exit then
   loop ;


\ The header (FILE-PART, FILE-OFF), INPUT (INPUT-STAMP, two roots, the reads skipped) and each section before PIECES as
\ WRITE-OBJECT lays it, by its -CK word; PATHS is skipped as KEYS-IN read it. Leaves PIECE-AT at PIECES' offset.
: HEADER-CK ( -- )
   0 FILES-N !  0 REQS-N !
   OBJECT BYTES$ nip {: len:n :}
   0 len RD-OPEN
   len HEADER-BYTES < if BAD exit then
   s" HABUFOBJ" RD-SAME
   FILE-KEY KEY-BYTES RD-SAME
   GOOD? 0= if exit then
   FILE-PART @ RD-EQ  8 RD-SKIP  FILE-OFF @ RD-EQ
   HEADER-BYTES RD-EQ  RD-U64 {: inputu:n :}
   inputu STAMP-BYTES 24 + <  inputu len HEADER-BYTES - >  or if BAD exit then
   HEADER-BYTES inputu + {: files-at:n :}
   RD-U64 RD-U64 {: off:n refsu:n :}
   refsu 0<  refsu FILES-ROW-BYTES mod 0<>  or  refsu len files-at - >  or if BAD exit then
   off  refsu 0<> if files-at else 0 then  <> if BAD then
   files-at refsu + {: reqs-at:n :}
   RD-U64 RD-U64 {: roff:n requ:n :}
   requ 0<  requ len reqs-at - >  or if BAD exit then
   roff  requ 0<> if reqs-at else 0 then  <> if BAD then
   reqs-at requ + {: names-at:n :}
   RD-U64 RD-U64 {: noff:n nmu:n :}
   nmu 0<  nmu 8 mod 0<>  or  nmu len names-at - >  or if BAD exit then
   noff  nmu 0<> if names-at else 0 then  <> if BAD then
   names-at nmu + {: reached-at:n :}
   RD-U64 RD-U64 {: hoff:n rchu:n :}
   rchu 0<  rchu 8 mod 0<>  or  rchu len reached-at - >  or if BAD exit then
   hoff  rchu 0<> if reached-at else 0 then  <> if BAD then
   reached-at rchu + {: paths-at:n :}
   RD-U64 RD-U64 {: poff:n pthu:n :}
   pthu 0<  pthu len paths-at - >  or if BAD exit then
   poff  pthu 0<> if paths-at else 0 then  <> if BAD then
   paths-at pthu + {: words-at:n :}
   RD-U64 RD-U64 {: woff:n wordsu:n :}
   wordsu 0<  wordsu WORD-ROW-BYTES mod 0<>  or  wordsu len words-at - >  or if BAD exit then
   woff  wordsu 0<> if words-at else 0 then  <> if BAD then
   words-at wordsu + {: exts-at:n :}
   RD-U64 RD-U64 {: xoff:n extsu:n :}
   extsu 0<  extsu USED-EXT-BYTES mod 0<>  or  extsu len exts-at - >  or if BAD exit then
   xoff  extsu 0<> if exts-at else 0 then  <> if BAD then
   exts-at extsu + {: times-at:n :}
   RD-U64 RD-U64 {: toff:n timesu:n :}
   timesu 0<  timesu len times-at - >  or if BAD exit then
   toff times-at <> if BAD then
   times-at timesu + {: pieces-at:n :}
   pieces-at RD-EQ  len pieces-at - RD-EQ
   INPUT-STAMP STAMP-BYTES RD-SAME
   NAME-CK NAME-CK                     \ the entry and exit roots, never empty in a stamped part
   RD-U64 {: readsu:n :}
   readsu 0<  readsu 8 mod 0<>  or if BAD exit then
   readsu RD-SKIP
   RD-AT @ files-at <> if BAD exit then
   refsu FILES-CK
   reqs-at REQS-AT !  requ REQUESTS-CK
   nmu NAMES-CK  NAMED-CK
   rchu REACHED-CK
   pthu RD-SKIP
   wordsu WORDS-CK  extsu EXTENTS-CK
   pieces-at len TIMES-CK
   RD-AT @ PIECE-AT ! ;


\ The offset in OBJECT of INPUT's field i after the stamp (0 the entry root, 1 the exit root, 2 the reads), once
\ HEADER-CK passed.
: INPUT-AT ( n -- n )
   {: i:n :}
   HEADER-BYTES STAMP-BYTES +
   i 0 ?do dup OBJ@ 8 + + loop ;

\ Whether the spelling resolves to the canonical directory it resolved to; ROOT? resolves `.` against the working
\ directory and answers false for a spelling it cannot take.
: ROOT-HOLDS? ( ptr u8 n ptr u8 n -- bool )
   {: a:ptr u:n b:ptr v:n :}
   a u SOURCE-ROOT:ROOT? {: c:ptr cu:n ok:bool :}
   ok 0= if false exit then
   c cu b v STR= ;

\ Whether an object's root is the current one or still resolves to itself, as SOURCE-ROOT:WITH and ROOT! need.
: ROOT-STANDS? ( ptr u8 n -- bool )
   {: a:ptr u:n :}
   a u SOURCE-ROOT:CURRENT$ STR= if true exit then
   a u a u ROOT-HOLDS? ;

\ Whether the environment variable holds the value it held, or is absent when value is NULL-PTR.
: ENV-HOLDS? ( ptr u8 n ptr u8 n -- bool )
   {: a:ptr u:n b:ptr v:n :}
   a u ENV-INPUT:CURRENT$ {: e:ptr eu:n :}
   b NULL-PTR - 0= if e NULL-PTR - 0= exit then
   e NULL-PTR - 0= if false exit then
   e eu b v STR= ;

\ Whether the read of the kind (COMPILE-READS) reads the same now.
: READ-HOLDS? ( n ptr u8 n ptr u8 n -- bool )
   {: kind:n a:ptr u:n b:ptr v:n :}
   kind COMPILE-READS:ROOT-READ = if a u b v ROOT-HOLDS? exit then
   kind COMPILE-READS:ENV-READ = if a u b v ENV-HOLDS? exit then
   kind COMPILE-READS:ENV-ABSENT = if a u NULL-PTR 0 ENV-HOLDS? exit then
   false ;

\ Whether every read in INPUT's reads (field 2) reads the same now, in COMPILE-READS' layout: false when a read's
\ lengths run past the field. A link check runs outside any recording, so these lookups record nothing.
: READS-HOLD? ( -- bool )
   2 INPUT-AT FIELD$ {: p:ptr pu:n :}
   0 begin dup pu < while
      {: at:n :}
      at 24 + pu > if false exit then
      p at + LE:U64@  p at + 8 + LE:U64@  p at + 16 + LE:U64@ {: kind:n u:n v:n :}
      u 0<  v 0<  or  u pu >  or  v pu >  or if false exit then
      at 24 + u + v + {: end:n :}
      end pu > if false exit then
      kind  p at + 24 + u  p at + 24 + u + v  READ-HOLDS? 0= if false exit then
      end 7 + 8 / 8 *
   repeat drop
   true ;

\ The path resolved as SOURCE-ROOT:RESOLVE resolves it with root current; the answer lasts until the next resolve.
: RESOLVE-UNDER ( ptr u8 n ptr u8 n -- ptr u8 n bool )
   {: a:ptr u:n root:ptr rootu:n :}
   root rootu SOURCE-ROOT:CURRENT$ STR= if a u SOURCE-ROOT:RESOLVE exit then
   root rootu [: a u SOURCE-ROOT:RESOLVE UNDER-KNOWN ! BUF:N>BLEN UNDER-PATH BUF:REPLACE ;] SOURCE-ROOT:WITH
   UNDER-PATH BYTES$ UNDER-KNOWN @ ;


\ A record reference (file, sum, count) naming a package: an earlier entry of KIND-PACKAGE, or an
\ earlier file's package record. It answers the file and the record ID-REF-CK found, -1 for none.
: PKG-REF-CK ( -- n n )
   RD-U64 RD-U64 RD-U64 {: file:n sum:n count:n :}
   file sum count RECS-N @ ID-REF-CK {: n:n :}
   GOOD?  n -1 <>  and if
      file 0= if
         n REC-KIND KIND-PACKAGE <> if BAD then
      else
         n PKG? 0= if BAD then
      then
   then
   file n ;


\ A package holding a private wordlist, by its file and the record PKG-REF-CK answers: this
\ file's record number, or an earlier file's record.
: PRIV+ ( n n -- )
   {: file:n n:n :}
   file PRIVS-N @ 2 * PRIVS !  n PRIVS-N @ 2 * 1 + PRIVS !
   1 PRIVS-N +! ;


\ A built package, by record index, that an earlier entry of the checked part gave a private wordlist.
: GRANT+ ( n -- )
   GRANTS-N @ 1 + GRANTS-RESERVE
   GRANTS-N @ GRANTS !  1 GRANTS-N +! ;

: GRANTED? ( n -- bool )
   {: r:n :}
   GRANTS-N @ 0 ?do i GRANTS @ r = if true unloop exit then loop
   false ;


\ Whether the package a reference names (file, PKG-REF-CK's record) has a private wordlist: one an earlier entry gave
\ (PRIVS), an earlier file's package record holding one, or a grant (GRANT+).
: PRIVATE? ( n n -- bool )
   {: file:n n:n :}
   PRIVS-N @ 0 ?do
      i 2 * PRIVS @ file =  i 2 * 1 + PRIVS @ n =  and if true unloop exit then
   loop
   file 0= if false exit then
   n GRANTED?  n XREF-REC XREF-PKG-PRIVATE 0<>  or ;


\ A KIND-SEALED entry's package: an earlier file's, unsealed until the link seals it. A package of
\ this file carries its seal in its own record's flags.
: SEALED-CK ( -- )
   PKG-REF-CK {: file:n n:n :}
   GOOD? 0= if exit then
   file 0= if BAD exit then
   n XREF-REC XREF:SEALED? if BAD then ;


\ An entry's flags, which RECORD+ writes without the name length and EXT.
: FLAGS-CK ( -- ) RD-U64  DNAME-LEN-MASK DNAME-EXT or  and 0<> if BAD then ;


\ The wordlist role (WORDLIST+) of an entry of the kind: 2 for KIND-PACKAGE, 5 for KIND-TYPE,
\ else 0, 1, or 3 or 4 then its package; a role 4 package has a private wordlist, as ROLE-WID
\ requires.
: ROLE-CK ( n -- )
   {: kind:n :}
   RD-U64 {: role:n :}
   role 0<  role 5 >  or if BAD exit then
   role 2 =  kind KIND-PACKAGE =  xor if BAD exit then
   role 5 =  kind KIND-TYPE =  xor if BAD exit then
   role 3 = if PKG-REF-CK 2drop exit then
   role 4 <> if exit then
   PKG-REF-CK {: file:n n:n :}
   GOOD? 0= if exit then
   file n PRIVATE? 0= if BAD then ;


\ A KIND-DEF entry's first emission: COPIES times the count of KIND-DEF entries before it.
: EM-NAMED ( -- n )
   DEFS-N @ COPIES *  1 DEFS-N +! ;


\ A package entry's own-private flag; a 1 adds the package, the next record, to PRIVS.
: PKG-FLAG-CK ( -- )
   RD-U64 {: f:n :}
   f 0<>  f 1 <>  and if BAD then
   f 1 = if 0 RECS-N @ PRIV+ then ;


\ A does record's entry offset (DOES-ENTRY-CK) into its definer's emission, the KIND-DEF entry's
\ just before it.
: DOES-CK ( -- )
   DOES-N @ 1 + 2 * DOES-AT-RESERVE
   RD-AT @ DOES-N @ 2 * DOES-AT !  LAST-EM @ DOES-N @ 2 * 1 + DOES-AT !  1 DOES-N +!
   LAST-EM @ 0< if BAD then
   8 RD-SKIP ;


\ A cell's kind, 0-2 (ROW-KIND), and its value's reference (CELL-REF+).
: CELL-REF-CK ( n -- )
   {: kind:n :}
   RD-REF {: tag:n x:n y:n z:n w:n :}
   tag 5 = if kind 0<  kind 2 >  or if BAD then exit then
   kind 0= if tag x y z w EMS-N @ 1 - RECS-N @ CODE-REF-CK exit then
   kind 1 = if tag x y z w DATA-REF-CK exit then
   BAD ;


\ A definition's dispatch cell (DISPATCH+): none, or the start of a word's extent of this part
\ (OWN-EXT) of at least a cell, at an offset from FILE-DP that is a multiple of 8.
: DISPATCH-CK ( -- )
   RD-REF {: tag:n x:n y:n z:n w:n :}
   tag 5 = if exit then
   tag 3 <>  x 0<>  or  w 0<>  or if BAD exit then
   1 y z OWN-EXT {: e:n :}
   e 0< if BAD exit then
   e 9 EXT @ 8 <  e 0 EXT @ FILE-DP @ - 7 and 0<>  or if BAD then ;


\ Entry i: its offset into ENTRY-OFFS, then its fields (RECORD+, LATE+, SEAL+), each record it
\ names by file, sum and count (ID-REF-CK); a record's index is RECS-N as it starts.
: ENTRY-CK ( n -- )
   {: i:n :}
   RD-AT @ i ENTRY-OFFS !
   RD-U64 {: kind:n :}
   kind 0<  kind KIND-TYPE >  or if BAD exit then
   kind KIND-PRIVATE = if
      \ A package holding a private wordlist gains none (INTERPRETER:PACKAGE-PRIVATE refuses it).
      PKG-REF-CK {: file:n n:n :}
      GOOD? if
         file n PRIVATE? if BAD else
            file n PRIV+
            file 0<> if n GRANT+ then
         then
      then
      -1 LAST-EM ! exit
   then
   kind KIND-SEALED = if SEALED-CK  -1 LAST-EM ! exit then
   NAME-CK  FLAGS-CK  kind ROLE-CK
   kind KIND-DEF = if EM-NAMED else -1 then {: em:n :}
   kind KIND-DEF =  GOOD? and if
      RECS-N @ em EM-RECS !
      COPIES 2 = if RECS-N @ em 1 + EM-RECS ! then
   then
   kind KIND-DEF = if DISPATCH-CK then
   kind KIND-PACKAGE = if PKG-FLAG-CK then
   kind KIND-ALIAS = if RD-U64 RD-U64 RD-U64 RECS-N @ ID-REF-CK drop then
   kind KIND-DOES = if DOES-CK then
   em LAST-EM !
   kind RECS-N @ REC-KINDS !  em RECS-N @ REC-EMS !
   1 RECS-N +! ;


\ A piece's entries (PIECE+); 32 bytes is a KIND-PRIVATE or KIND-SEALED entry's size.
: ENTRIES-CK ( -- )
   32 RD-COUNT {: n:n :}
   n ENTRIES-N !  0 RECS-N !  0 DEFS-N !  0 DOES-N !  0 PRIVS-N !  -1 LAST-EM !
   n 0 > if
      n ENTRY-OFFS-RESERVE  n REC-KINDS-RESERVE  n COPIES * EM-RECS-RESERVE  n REC-EMS-RESERVE  n 2 * PRIVS-RESERVE
   then
   n 0 ?do i ENTRY-CK loop ;


\ The machine this object's emission e holds: the host's for a host copy, a cross object's odd
\ emission (COPIES), else the target's.
: EM-ARCH ( n -- CTARGET:arch )
   {: e:n :}
   COPIES 2 =  e 1 and 1 =  and if  RTARGET:HOST-TARGET RTARGET:NATIVE-BINDING
   else  NSHADOW:BINDING  then
   CBIND:TARGET@ CTARGET:ARCH@ ;


\ A call row of emission top, s bytes; its site, above the row before it, holds the compile's
\ unplaced call of the row's kind. A record target precedes top's own: a body naming its word is
\ E-UNDEFINED. A function target may be top's own, as a Wasm RECURSE's row is.
: CALL-CK ( n n -- )
   {: s:n top:n :}
   RD-U64 {: site:n :}
   site LAST-CALL @ <= if BAD then
   site LAST-CALL !
   RD-U64 {: kind:n :}
   kind 0<>  kind 1 <>  and if BAD then
   GOOD? if
      top EM-ARCH  top EM-OFFS @ 16 + OBJ-PTR  s  site
      kind 0= if NEMIT:CALL else NEMIT:TAIL then  CALL?
      0= if BAD then
   then
   RD-REF top  top EM-RECS @  SITE-REF-CK ;


\ An address row of an emission of s bytes, numbered top, its site above the row before it: its
\ site holds an address value of the row's kind in the emission's machine's encoding. A record
\ target precedes top's own, as a call's.
: ADDR-CK ( n n -- )
   {: s:n top:n :}
   RD-U64 {: site:n :}
   site LAST-ADDR @ <= if BAD then
   site LAST-ADDR !
   RD-U64 {: kind:n :}
   kind 0<>  kind 1 <>  and if BAD then
   GOOD? if
      top EM-ARCH  top EM-OFFS @ 16 + OBJ-PTR  s  site
      kind 0= if NEMIT:ADDR-CODE else NEMIT:ADDR-DATA then  ADDRESS@ nip
      0= if BAD then
   then
   kind 0= if RD-REF top  top EM-RECS @  SITE-REF-CK exit then
   kind 1 = if RD-REF DATA-REF-CK exit then
   BAD ;


\ The does> slot: -1, or its site, past the last call row and holding the machine's unpatched slot, and
\ its target: none while no clause patched it, else the record of a does> clause, checked as a call's.
: SLOT-CK ( n n -- )
   {: s:n top:n :}
   RD-U64 {: p:n :}
   p -1 = if exit then
   p LAST-CALL @ <= if BAD then
   GOOD? if
      top EM-ARCH  top EM-OFFS @ 16 + OBJ-PTR  s  p  SLOT?
      0= if BAD exit then
   then
   RD-REF {: tag:n x:n y:n z:n w:n :}
   tag 5 = if exit then
   tag 0 <> if BAD exit then
   tag x y z w top  top EM-RECS @ 1 +  SITE-REF-CK
   GOOD? 0= if exit then
   x 0= if  y z OWN-REC REC-KIND KIND-DOES <> if BAD then  exit  then
   x FILES-ROW y z PLACE {: r:n :}
   r 1 <  if BAD exit then
   r 1 - XREF-DOES-COMPANION? 0= if BAD then ;


\ Emission e (EMISSION-CODE+): its offset into EM-OFFS, then S, R, S bytes, F offsets, calls,
\ addresses and the slot. Each RD-COUNT size is the fewest bytes that row takes.
: EMISSION-CK ( n -- )
   {: e:n :}
   RD-AT @ e EM-OFFS !
   RD-U64 {: s:n :}
   e EM-ARCH s SIZE? 0= if BAD then
   RD-U64 {: r:n :}
   r 0<  r s >  or if BAD then
   s RD-SKIP
   8 RD-COUNT {: f:n :}
   f 1 < if BAD then
   -1  f 0 ?do
      RD-U64 {: off:n :}
      dup off >=  off s >=  or if BAD then
      drop off
   loop drop
   -1 LAST-CALL !  -1 LAST-ADDR !
   32 RD-COUNT 0 ?do s e CALL-CK loop
   32 RD-COUNT 0 ?do s e ADDR-CK loop
   s e SLOT-CK ;


\ Each KIND-DOES entry's offset: function j's offset of its definer's emission e, j from 1 to
\ F - 1, an ENTRY@ as RECORD+ writes it. A host copy has as many functions (PAIRS-CK), so the
\ clause is its function j too.
: DOES-ENTRY-CK ( -- )
   GOOD? 0= if exit then
   DOES-N @ 0 ?do
      i 2 * 1 + DOES-AT @  i 2 * DOES-AT @ OBJ@ {: e:n off:n :}
      false  e OWN-FUNS 1 ?do  e i OWN-FUN-AT off =  or  loop
      0= if BAD unloop exit then
   loop ;


\ A cross object's two copies of a definition have as many functions: both walks emit the same
\ ones, a callback's stub last, and linking pairs them (NSHADOW:SOURCE-FUNS).
: PAIRS-CK ( -- )
   GOOD? 0= if exit then
   COPIES 2 = 0= if exit then
   EMS-N @ 0 ?do
      i OWN-FUNS  i 1 + OWN-FUNS  <> if BAD unloop exit then
   2 +loop ;


\ E, then each emission (CODE+); 60 bytes is a 4-byte emission of one function. Every emission
\ is a KIND-DEF entry's, COPIES of them each.
: CODE-CK ( -- )
   60 RD-COUNT {: n:n :}
   n EMS-N !
   n DEFS-N @ COPIES * <> if BAD then
   n 0 > if n EM-OFFS-RESERVE then
   n 0 ?do i EMISSION-CK loop
   PAIRS-CK
   DOES-ENTRY-CK ;


\ A cell row of the extent of u bytes at offset s in DATA: its cell inside the extent, at an
\ offset in the part that is a multiple of 8.
: CELL-ROW-CK ( n n -- )
   {: s:n u:n :}
   RD-U64 {: off:n :}
   off 0<  off u 8 - >  or  s off + 7 and 0<>  or if BAD then
   RD-U64 CELL-REF-CK ;


\ A string extent of u bytes at the offset in OBJECT: an 8-byte body length, then the body. A string the table holds was
\ laid since by an earlier file, so the part compiles.
: STRING-CK ( n n -- )
   {: at:n u:n :}
   GOOD? 0= if exit then
   u 8 < if BAD exit then
   at OBJ@ {: n:n :}
   n 0<  n u 8 - >  or if BAD exit then
   at 8 + OBJ-PTR  n  INTERN:FIND 0<> if BAD then ;


\ Extent e of DATA's d bytes, RD-AT past the starts: TIMES span, kind (3 only first, 2 with a sum), length to the next
\ start or d and identity as OWN-IDS! put them in EXTS; then bytes, cell rows (CELL-ROW-CK), length cell (STRING-CK).
: EXTENT-CK ( n n -- )
   {: e:n d:n :}
   GOOD? 0= if exit then
   EXTS-AT @ 8 + e 8 * + {: at:n :}
   at OBJ@ {: s:n :}
   e 1 +  EXTS-AT @ OBJ@ < if at 8 + OBJ@ else d then {: end:n :}
   RD-AT @  PIECE-AT @  e TIMES-EXT 8 + OBJ@ +  <> if BAD exit then
   RD-U64 RD-U64 RD-U64 RD-U64 {: kind:n sum:n count:n u:n :}
   kind 1 <  kind 3 >  or  kind 3 =  e 0<>  and  or  kind 2 =  sum 0=  and  or if BAD exit then
   u  end s -  <> if BAD exit then
   kind e 1 EXT @ <>  sum e 2 EXT @ <>  or  count e 3 EXT @ <>  or  u e 9 EXT @ <>  or
   s FILE-DP @ +  e 0 EXT @ <>  or if BAD exit then
   RD-AT @ {: bytes:n :}
   u RD-SKIP
   kind 2 = if bytes u STRING-CK then
   24 RD-COUNT 0 ?do s u CELL-ROW-CK loop ;


\ A store row, as FIND-STORES admits it: a cell 8-aligned from an earlier part's first data byte, in a word's, string's
\ or laid text's extent (tag 3, 6), the head (tag 7; may pass an unaligned end: main.f's D is 98), header data (tag 4).
: STORE-CK ( -- )
   RD-REF {: tag:n x:n y:n z:n w:n :}
   tag 3 =  tag 6 =  tag 7 =  or or  x 1 >=  and  tag 4 =  or 0= if BAD exit then
   tag x y z w DATA-REF-CK
   GOOD? 0= if exit then
   tag 3 =  tag 6 =  or if
      x FILE-ROWS @  tag 3 = if 1 else 2 then  y z ROW-EXT {: b:n :}
      w b 5 BUILT-EXT @ >= if BAD exit then
      b 4 BUILT-EXT @  x BUILT-DP BUILT@ -  w +  7 and 0<> if BAD exit then
   then
   tag 7 = if
      y 7 and 0<> if BAD exit then
      y x HEAD-LEN >= if BAD exit then
   then
   tag 4 = if x 7 and 0<> if BAD exit then then
   RD-U64 RD-U64 {: kind:n v:n :}
   kind 3 = if RD-REF {: t:n a:n b:n c:n d:n :}  t 5 <> if BAD then exit then
   v 0<> if BAD exit then
   kind CELL-REF-CK ;


\ DATA and STORES (DATA+, STORES+): D at least 0; X, TIMES' extent count (EXTS-N), starts ascending
\ from 0, none past D, and none only when D is 0; each extent (EXTENT-CK).
: DATA-CK ( -- )
   RD-AT @ DATA-AT !
   RD-U64 {: d:n :}
   d 0< if BAD then
   RD-AT @ EXTS-AT !
   8 RD-COUNT {: x:n :}
   x 0=  d 0<>  and  x EXTS-N @ <>  or if BAD then
   0  x 0 ?do
      RD-U64 {: s:n :}
      s over <  s d >  or  i 0=  s 0<>  and  or if BAD then
      s max
   loop drop
   x 0 ?do i d EXTENT-CK loop
   RD-AT @ STORES-AT !
   40 RD-COUNT 0 ?do STORE-CK loop ;


\ FACTS (FACTS+): each row a retirement of a record made before it (its count of the piece's records,
\ never below the last retirement's), a text for a record, or a text of kind CHECK:HELD-LAID or -UNDEFINE.
: FACTS-CK ( -- )
   RD-AT @ FACTS-AT !
   0  16 RD-COUNT 0 ?do
      RD-U64 {: last:n ix:n :}
      ix CHECK:HELD-RETIRED =  ix CHECK:HELD-AMENDED =  or if
         RD-U64 RD-U64 RD-U64 {: file:n sum:n count:n :}
         ix CHECK:HELD-AMENDED = if
            file sum count RECS-N @ ID-REF-CK drop  RD-U64 RD-SKIP  last
         else
            RD-U64 {: made:n :}
            made last <  made RECS-N @ >  or if BAD then
            file sum count made ID-REF-CK drop
            made
         then
      else
         ix CHECK:HELD-UNDEFINE <  ix 0 >=  or if BAD then
         RD-U64 RD-SKIP  last
      then
   loop drop ;


\ ACTS (ACTS+): each row a kind, EV-REQUIRED or EV-PROVIDED, a path and the root current as it was spelled.
: ACTS-CK ( -- )
   RD-AT @ ACTS-AT !
   24 RD-COUNT 0 ?do
      RD-U64 {: kind:n :}
      kind EV-REQUIRED <>  kind EV-PROVIDED <>  and if BAD then
      NAME-CK NAME-CK
   loop ;


\ The piece at PIECE-AT after MAP-FILES: its TIMES count equals its records, req (0 when a request ends the part, -1 at
\ the file's end) ends the object, and every FILES row is built (ROWS-CK).
: PIECE-CK ( n -- )
   {: req:n :}
   GOOD? 0= if exit then
   PIECE-AT @  OBJECT BYTES$ nip  RD-OPEN
   OWN-IDS!
   ENTRIES-CK  CODE-CK  DATA-CK  FACTS-CK  ACTS-CK
   TIMES-AT OBJ@  RECS-N @  <> if BAD then
   req RD-EQ
   RD-AT @ RD-END @ <> if BAD then
   ROWS-CK ;


variable READ-FD                     \ READ-INTO's open descriptor until FS:READ-SIZED owns it

\ The buffer grown to the file's size, then the file read into it; a size the buffer cannot
\ hold throws before FS:READ-SIZED owns the descriptor.
: READ-BODY ( fd n ptr a -- )
   {: file:fd size:n buf:ptr :}
   buf BUF:CLEAR
   file  buf size BUF:N>BLEN BUF:GROW SPAN:$
   -1 READ-FD !  FS:READ-SIZED ;

: READ-DONE ( -- )
   READ-FD @ 0 >= if READ-FD @ close then
   -1 READ-FD ! ;

\ The file at the path, whole, into the buffer through one descriptor, so a build renaming
\ a new file over the path cannot tear the read; false when it is missing.
: READ-INTO ( ptr u8 n ptr a -- bool )
   {: path:ptr pathu:n buf:ptr :}
   path pathu FS:OPEN-SIZED {: file:fd size:n found:bool :}
   found 0= if false exit then
   file FD>N READ-FD !
   file size buf [: READ-BODY ;] [: READ-DONE ;] finally
   true ;

\ READ-INTO with its inputs kept beneath its answer, so a catch around it keeps the stack's shape.
: READ-KEEPING ( ptr u8 n ptr a bool -- ptr u8 n ptr a bool )
   drop {: path:ptr pathu:n buf:ptr :}
   path pathu buf  path pathu buf READ-INTO ;

\ READ-INTO, false also when the path cannot be opened (too long, a 0 byte), the file read or
\ a buffer of its size mapped (E-MEM-MAP); any other throw passes.
: READ-WHOLE ( ptr u8 n ptr a -- bool )
   false ['] READ-KEEPING catch {: path pathu buf ok code:n :}
   code 0= if ok exit then
   code E-FS-OPEN =  code E-FS-STAT = or  code E-FS-IO = or
   code E-FS-PATH = or  code E-FFI-NUL = or  code E-MEM-MAP = or  if false exit then
   code throw ;

\ The disk key at the address made a memory key: its first cell, an offset in PATHS' u bytes, becomes
\ that path's offset in KEY-PATHS (PATH-IN); false when the FIELD+ there leaves PATHS.
: KEY-IN ( ptr u8 ptr u8 n -- bool )
   {: key:ptr paths:ptr u:n :}
   key LE:U64@ {: at:n :}
   at 0<  at u 8 - >  or if false exit then
   paths at + LE:U64@ {: len:n :}
   len 0<  len u at - 8 - >  or if false exit then
   paths at + 8 + len KEY-PATHS PATH-IN  key LE:U64!
   true ;

\ The object read into the buffer: true once its header key and FILES rows hold memory keys (KEY-IN);
\ false when the magic, the FILES or PATHS pair or a key leaves the object, which then counts as unread.
: KEYS-IN ( ptr a -- bool )
   {: buf:ptr :}
   buf BYTES$ {: a:ptr len:n :}
   len HEADER-BYTES < if false exit then
   a 8  s" HABUFOBJ" STR= 0= if false exit then
   PAIRS-AT {: pairs:n :}
   a pairs + 16 + LE:U64@  a pairs + 24 + LE:U64@ {: foff:n fu:n :}
   a pairs + 80 + LE:U64@  a pairs + 88 + LE:U64@ {: poff:n pu:n :}
   foff 0<  fu 0<  or  foff len >  or  fu len foff - >  or  fu FILES-ROW-BYTES mod 0<>  or if false exit then
   poff 0<  pu 0<  or  poff len >  or  pu len poff - >  or if false exit then
   a 8 +  a poff +  pu  KEY-IN 0= if false exit then
   fu FILES-ROW-BYTES / 0 ?do
      a foff + i FILES-ROW-BYTES * +  a poff +  pu  KEY-IN 0= if false unloop exit then
   loop
   true ;

\ FILE-KEY's object, whole, into OBJECT, its keys memory keys (KEYS-IN); false when it is missing,
\ cannot be opened or read, or a key leaves it, so the file compiles and its write replaces the object.
: READ-OBJECT ( -- bool )
   OBJECT$ OBJECT READ-WHOLE  OBJ-TAKE drop  0= if false exit then
   OBJECT KEYS-IN ;


\ ---- the last build's object ----------------------------------------------------
\ PRIOR holds the part's object as the last build left it: a piece equal to its old one keeps its time (COMPILED-TIME).

\ The header pair at the offset in PRIOR, an offset and a length, inside PRIOR's length.
: PRIOR-PAIR? ( n -- bool )
   {: at:n :}
   PRIOR BYTES$ nip {: len:n :}
   at PRIOR@  at 8 + PRIOR@ {: off:n u:n :}
   off 0<  u 0<  or  off len >  or  u len off - >  or  0= ;


\ FILE-KEY's object into PRIOR with memory keys (KEYS-IN); true when the magic, FILE-KEY and the FILES,
\ REQUESTS, TIMES and PIECES pairs lie inside it, FILES whole rows. The header's offsets place sections.
: PRIOR-READ ( -- bool )
   OBJECT$ PRIOR READ-WHOLE 0= if false exit then
   PRIOR KEYS-IN 0= if false exit then
   PRIOR BYTES$ {: a:ptr len:n :}
   len HEADER-BYTES < if false exit then
   a 8  s" HABUFOBJ" STR= 0= if false exit then
   a 8 + KEY-BYTES  FILE-KEY KEY-BYTES  STR= 0= if false exit then
   PAIRS-AT {: pairs:n :}
   pairs 16 + PRIOR-PAIR?  pairs 32 + PRIOR-PAIR? and  TIMES-FIELD PRIOR-PAIR? and  PIECES-FIELD PRIOR-PAIR? and
   0= if false exit then
   pairs 24 + PRIOR@ FILES-ROW-BYTES mod 0= ;


\ PRIOR's piece (PRIOR-READ): its offset and length from the PIECES pair, the header's time, and
\ true; zeros and false when the pair leaves PRIOR.
: PRIOR-PIECE ( -- n n n bool )
   PIECES-FIELD PRIOR-PAIR? 0= if 0 0 0 false exit then
   PIECES-FIELD PRIOR@  PIECES-FIELD 8 + PRIOR@  8 KEY-BYTES + 8 + PRIOR@  true ;


\ The count of PRIOR's leading FILES rows from 1 that equal REFS' rows by key and piece number.
: KEPT-ROWS ( -- n )
   REFS BYTES$ {: refs:ptr refsu:n :}
   PAIRS-AT 16 + {: files:n :}
   PRIOR BYTES$ drop  files PRIOR@ + {: old:ptr :}
   files 8 + PRIOR@ FILES-ROW-BYTES /  refsu FILES-ROW-BYTES /  min {: n:n :}
   n 0 ?do
      refs i FILES-ROW-BYTES * +  KEY-BYTES 8 +
      old i FILES-ROW-BYTES * +  KEY-BYTES 8 +  STR= 0= if i unloop exit then
   loop
   n ;

\ PRIOR's FILES rows from 1 equal REFS' rows by key and piece number, every row REFS holds.
: REFS-KEPT? ( -- bool ) KEPT-ROWS  REFS BYTES$ nip FILES-ROW-BYTES /  = ;


\ The count of size-byte rows of the PRIOR TIMES block at the offset, which ends by end, and true;
\ 0 and false when the count or its rows pass end.
: PRIOR-COUNT ( n n n -- n bool )
   {: at:n end:n size:n :}
   at 8 +  end > if 0 false exit then
   at PRIOR@ {: n:n :}
   n 0<  n  end at - 8 -  size /  >  or if 0 false exit then
   n true ;

\ The offset in PRIOR where its TIMES block ends.
: PRIOR-TIMES-END ( -- n ) TIMES-FIELD PRIOR@  TIMES-FIELD 8 + PRIOR@ + ;


\ PRIOR's TIMES block (PRIOR-READ): the offset of its first word and its word count, and true;
\ false when the count leaves TIMES.
: PRIOR-WORDS ( -- n n bool )
   TIMES-FIELD PRIOR@ {: first:n :}
   first PRIOR-TIMES-END WORD-BYTES PRIOR-COUNT {: n:n found:bool :}
   first 8 +  n  found ;

\ PRIOR's extents, after its n words at the offset: the offset of the first and their count; 0 0
\ when the count leaves TIMES.
: PRIOR-EXTS ( n n -- n n )
   {: words:n n:n :}
   words  n WORD-BYTES * + {: at:n :}
   at PRIOR-TIMES-END EXT-BYTES PRIOR-COUNT {: x:n found:bool :}
   found 0= if 0 0 exit then
   at 8 +  x ;

\ PRIOR's stores entry, after its x extents at the offset (PRIOR-EXTS): the stores time, STORES' offset in the old
\ piece and its length; 0 0 0 for no extents' offset or an entry past TIMES.
: PRIOR-STORES ( n n -- n n n )
   {: exts:n x:n :}
   exts  x EXT-BYTES * + {: at:n :}
   exts 0=  at 24 +  PRIOR-TIMES-END >  or if 0 0 0 exit then
   at PRIOR@  at 8 + PRIOR@  at 16 + PRIOR@ ;


\ The old piece (PRIOR-PIECE!): offset in PRIOR, length, TIMES offset of its first word, word count (0 for none),
\ OLD-WORD's next try, KEPT-ROWS, the same three for extents (OLD-EXT), then the stores entry's time, STORES' offset
\ in the old piece and length (PRIOR-STORES), time 0 for none.
variable PRIOR-AT
variable PRIOR-SIZE
variable PRIOR-FIRST-WORD
variable PRIOR-WORDS-N
variable PRIOR-NEXT
variable KEPT-ROWS-N
variable PRIOR-FIRST-EXT
variable PRIOR-EXTS-N
variable PRIOR-EXT-NEXT
variable PRIOR-STORES-TIME
variable PRIOR-STORES-AT
variable PRIOR-STORES-SIZE

\ PRIOR's piece and its TIMES block as the old piece (PRIOR-PIECE, PRIOR-WORDS, PRIOR-EXTS, PRIOR-STORES); none
\ when old is false or either fails.
: PRIOR-PIECE! ( bool -- )
   {: old:bool :}
   0 PRIOR-WORDS-N !  0 PRIOR-NEXT !  0 PRIOR-EXTS-N !  0 PRIOR-EXT-NEXT !
   0 PRIOR-STORES-TIME !  0 PRIOR-STORES-AT !  0 PRIOR-STORES-SIZE !
   old 0= if exit then
   PRIOR-PIECE {: at:n u:n t:n ok:bool :}
   ok 0= if exit then
   PRIOR-WORDS {: words:n n:n found:bool :}
   found 0= if exit then
   at PRIOR-AT !  u PRIOR-SIZE !  words PRIOR-FIRST-WORD !  n PRIOR-WORDS-N !  KEPT-ROWS KEPT-ROWS-N !
   words n PRIOR-EXTS {: exts:n x:n :}
   x PRIOR-EXTS-N !  exts PRIOR-FIRST-EXT !
   exts x PRIOR-STORES  PRIOR-STORES-SIZE !  PRIOR-STORES-AT !  PRIOR-STORES-TIME ! ;


\ Word w's text range in the running piece, which starts at the offset in PIECES: its offset from
\ the piece's first byte and its length, 0 0 for none.
: TEXT-SPAN ( n n -- n n )
   {: start:n w:n :}
   w 4 WORD-SPAN @ {: u:n :}
   u 0= if 0 0 exit then
   w 3 WORD-SPAN @ start -  u ;


\ Word w's emission range, as TEXT-SPAN: its COPIES, which CODE+ writes one after another.
: EM-SPAN ( n n -- n n )
   {: start:n w:n :}
   w 2 WORD-SPAN @ {: em:n :}
   em 0< if 0 0 exit then
   em COPIES + 1 - {: last:n :}
   em 3 * EM-SPANS @ {: at:n :}
   at start -  last 3 * EM-SPANS @  last 3 * 1 + EM-SPANS @ +  at - ;


\ Word w's range s, its entry (0), emission (1) or text (2), as TEXT-SPAN.
: NEW-SPAN ( n n n -- n n )
   {: start:n w:n s:n :}
   s 0= if  w 0 WORD-SPAN @ start -  w 1 WORD-SPAN @  exit then
   s 1 = if start w EM-SPAN exit then
   start w TEXT-SPAN ;


\ The highest FILES row word w's ranges name (TOP-ROW), 0 for none.
: WORD-TOP ( n -- n )
   {: w:n :}
   w 5 WORD-SPAN @ {: top:n :}
   w 2 WORD-SPAN @ {: em:n :}
   em 0< if top exit then
   top  COPIES 0 ?do  em i + 3 * 2 + EM-SPANS @ max  loop ;


\ The offset in PRIOR of the old word with word w's sum and count, -1 for none. The search wraps from after the last
\ word found, so an unchanged file finds each word at once.
: OLD-WORD ( n -- n )
   {: w:n :}
   FILE-REC @ w + REC-ID {: sum:n count:n :}
   PRIOR-WORDS-N @ {: n:n :}
   n 0 ?do
      PRIOR-NEXT @ i + n mod {: o:n :}
      PRIOR-FIRST-WORD @  o WORD-BYTES * + {: at:n :}
      at 9 8 * + PRIOR@ sum =  at 10 8 * + PRIOR@ count =  and if
         o 1 + PRIOR-NEXT !  at unloop exit
      then
   loop
   -1 ;


\ Whether the running piece's range noff, nu (the piece at start in PIECES) holds the old piece's range ooff, ou, which
\ must lie inside the old piece.
: SPAN= ( n n n n n -- bool )
   {: start:n noff:n nu:n ooff:n ou:n :}
   ooff 0<  ou 0<  or  ooff PRIOR-SIZE @ >  or  ou PRIOR-SIZE @ ooff - >  or if false exit then
   nu ou <> if false exit then
   PIECES BYTES$ drop start + noff +  nu  PRIOR BYTES$ drop PRIOR-AT @ + ooff +  ou  STR= ;

\ Whether word w's range s holds the bytes of the old word's at offset old in PRIOR (SPAN=).
: SAME-SPAN? ( n n n n -- bool )
   {: start:n w:n old:n s:n :}
   start w s NEW-SPAN {: noff:n nu:n :}
   old 8 +  s 16 * + {: at:n :}
   start noff nu  at PRIOR@  at 8 + PRIOR@  SPAN= ;


\ The time of the record word w binds (WORD-SPANS' cell 7), 0 for none; the record is an earlier
\ one, so it has its time (REC-TIME).
: BOUND-TIME ( n -- n )
   {: w:n :}
   w 7 WORD-SPAN @ {: b:n :}
   b 0< if 0 exit then
   b REC-TIME ;


\ Word w's time: its old word's (OLD-WORD) when that has one, names no row past KEPT-ROWS-N, holds the same bytes and
\ has w's bound record time and outside bits; else now, as for a word binding a record with no time.
: WORD-TIME ( n n n -- n )
   {: start:n w:n now:n :}
   w BOUND-TIME {: bt:n :}
   w 7 WORD-SPAN @ 0 >=  bt 0=  and if now exit then
   w OLD-WORD {: old:n :}
   old 0< if now exit then
   old PRIOR@ {: t:n :}
   t 0=  w WORD-TOP KEPT-ROWS-N @ >  or if now exit then
   start w old 0 SAME-SPAN? 0= if now exit then
   start w old 1 SAME-SPAN? 0= if now exit then
   start w old 2 SAME-SPAN? 0= if now exit then
   old 7 8 * + PRIOR@  bt <> if now exit then
   old 8 8 * + PRIOR@  w 6 WORD-SPAN @ <> if now exit then
   t ;

\ Word w's range s onto TIMES, its offset then its length (NEW-SPAN).
: SPAN-TIMES+ ( n n n -- )
   NEW-SPAN {: off:n u:n :}
   off TIMES U64+  u TIMES U64+ ;


\ The offset in PRIOR of the old piece's extent with extent e's kind, sum and count, -1 for none;
\ searched as OLD-WORD searches, from PRIOR-EXT-NEXT.
: OLD-EXT ( n -- n )
   {: e:n :}
   e 1 EXT @  e 2 EXT @  e 3 EXT @ {: kind:n sum:n count:n :}
   PRIOR-EXTS-N @ {: n:n :}
   n 0 ?do
      PRIOR-EXT-NEXT @ i + n mod {: o:n :}
      PRIOR-FIRST-EXT @  o EXT-BYTES * + {: at:n :}
      at 24 + PRIOR@ kind =  at 32 + PRIOR@ sum =  and  at 40 + PRIOR@ count =  and if
         o 1 + PRIOR-EXT-NEXT !  at unloop exit
      then
   loop
   -1 ;


\ Extent e's time: its old extent's (OLD-EXT) when that has one, every FILES row its cell rows name
\ is within KEPT-ROWS-N and its span holds the old span's bytes (SPAN=); else now.
: EXT-TIME ( n n n -- n )
   {: start:n e:n now:n :}
   e OLD-EXT {: old:n :}
   old 0< if now exit then
   old PRIOR@ {: t:n :}
   t 0=  e 6 EXT @ KEPT-ROWS-N @ >  or if now exit then
   start  e 4 EXT @ start -  e 5 EXT @  old 8 + PRIOR@  old 16 + PRIOR@  SPAN= 0= if now exit then
   t ;


\ The TIMES block of the piece at start in PIECES up to its stores entry (STORES-TIMES+): word count, per word time
\ (WORD-TIME, also WORD-TIMES), spans, bound time, outside bits, sum, count; extent count, per extent time (EXT-TIME,
\ also EXTS), span, kind, sum, count.
: TIMES+ ( n n -- )
   {: start:n now:n :}
   WORDS-N @ TIMES U64+
   FILE-REC @ WORDS-N @ + WORD-TIMES-RESERVE
   WORDS-N @ 0 ?do
      start i now WORD-TIME {: t:n :}
      t TIMES U64+
      start i 0 SPAN-TIMES+  start i 1 SPAN-TIMES+  start i 2 SPAN-TIMES+
      i BOUND-TIME TIMES U64+  i 6 WORD-SPAN @ TIMES U64+
      FILE-REC @ i + REC-ID {: sum:n count:n :}
      sum TIMES U64+  count TIMES U64+
      t  FILE-REC @ i +  WORD-TIMES !
   loop
   EXTS-N @ TIMES U64+
   EXTS-N @ 0 ?do
      start i now EXT-TIME {: t:n :}
      t TIMES U64+  t i 10 EXT !
      i 4 EXT @ start - TIMES U64+  i 5 EXT @ TIMES U64+
      i 1 EXT @ TIMES U64+  i 2 EXT @ TIMES U64+  i 3 EXT @ TIMES U64+
   loop ;


\ The time of the piece at the offset in PIECES, old when PRIOR-READ found its object: its old piece's when that has
\ a time, the same bytes and REFS-KEPT?, else now, the time TIMES+ gives each word that changed.
: KEPT-TIME ( n n bool -- n )
   {: start:n now:n old:bool :}
   old 0= if now exit then
   PRIOR-PIECE {: at:n u:n t:n ok:bool :}
   ok 0=  t 0=  or if now exit then
   PIECES BYTES$ {: a:ptr len:n :}
   len start - u <> if now exit then
   a start + u  PRIOR BYTES$ drop at + u  STR= 0= if now exit then
   REFS-KEPT? 0= if now exit then
   t ;


\ The stores time of the piece at the offset in PIECES: 0 when CHANGED-EARLIER, an entry no later compile keeps; else
\ the old entry's when nonzero, REFS-KEPT? and STORES holds the old bytes
\ (SPAN=); else now.
: STORES-TIME ( n n n n -- n )
   {: start:n off:n u:n now:n :}
   CHANGED-EARLIER @ if 0 exit then
   PRIOR-STORES-TIME @ {: old:n :}
   old 0= if now exit then
   REFS-KEPT? 0= if now exit then
   start off u  PRIOR-STORES-AT @ PRIOR-STORES-SIZE @  SPAN= 0= if now exit then
   old ;


\ The TIMES block's stores entry for the piece at the offset in PIECES: its stores time (STORES-TIME), its STORES'
\ offset from the piece and length; STORES-AT and FACTS-AT are PIECES offsets until WRITE-OBJECT.
: STORES-TIMES+ ( n n -- )
   {: start:n now:n :}
   STORES-AT @ start -  FACTS-AT @ STORES-AT @ - {: off:n u:n :}
   start off u now STORES-TIME TIMES U64+
   off TIMES U64+  u TIMES U64+ ;


\ The part's time, its piece starting at the offset in PIECES (KEPT-TIME); its TIMES block (TIMES+, STORES-TIMES+).
: COMPILED-TIME ( n -- n )
   {: start:n :}
   NOW {: now:n :}
   PRIOR-READ {: old:bool :}
   old PRIOR-PIECE!
   start now TIMES+
   start now old KEPT-TIME {: t:n :}
   start now STORES-TIMES+
   t ;


\ ---- checking a part ------------------------------------------------------------
\ Each path a part's acts and request name resolves as its compile resolved it; PENDING-PATHS holds earlier acts' paths.

\ Whether an earlier act of the checked part provided the path (PENDING-PATHS).
: PENDING-PATH? ( ptr u8 n -- bool )
   {: a:ptr u:n :}
   PENDING-PATHS BYTES$ {: p:ptr pu:n :}
   0 begin dup pu < while
      {: at:n :}
      p at + LE:U64@ {: len:n :}
      p at + 8 + len  a u  STR= if true exit then
      at 8 + len +
   repeat drop
   false ;

\ Whether the registry holds the path, or an earlier act of the checked part provided it, as a
\ compile of the part would find it.
: REGISTERED? ( ptr u8 n bool -- bool )
   {: a:ptr u:n known:bool :}
   known if true exit then
   a u PENDING-PATH? ;


\ The piece's ACTS rows (ACTS-CK), each path resolved under the root it was spelled in, BAD when that root no longer
\ stands (ROOT-STANDS?): a require's must be REGISTERED?, as its compile found it; a provide's is registered.
: ACTS-WALK ( -- )
   ACTS-AT @  OBJECT BYTES$ nip  RD-OPEN
   RD-U64 0 ?do
      RD-U64 {: kind:n :}
      RD-NAME RD-NAME {: a:ptr u:n root:ptr rootu:n :}
      root rootu ROOT-STANDS? 0= if BAD unloop exit then
      a u root rootu RESOLVE-UNDER {: canon:ptr canonu:n known:bool :}
      kind EV-PROVIDED = if
         canon canonu PENDING-PATHS FIELD+
      else
         canon canonu known REGISTERED? 0= if BAD unloop exit then
      then
   loop ;


\ After INPUT!: true when part FILE-PART's object can stand for its compile: stamped (an UNLINKABLE part's INPUT-STAMP
\ is 0 0 -1), whole and checked, in the current root, its reads, roots and request answering as they did.
: PART-FRESH? ( -- bool )
   STAMPED? 0= if false exit then
   HITS-CLEAR                          \ the check replays NAMES into HITS
   PENDING-PATHS BUF:CLEAR  0 GRANTS-N !
   READ-OBJECT 0= if false exit then
   HEADER-CK
   REQS-N @ 0<> if 0 else -1 then PIECE-CK
   GOOD? 0= if false exit then
   0 INPUT-AT FIELD$  SOURCE-ROOT:CURRENT$  STR= 0= if false exit then
   READS-HOLD? 0= if false exit then
   ACTS-WALK
   GOOD? 0= if false exit then
   1 INPUT-AT FIELD$ ROOT-STANDS? 0= if false exit then
   REQS-N @ 0= if true exit then
   REQ-FIELDS {: kind:n spelled:n loaded:n :}
   spelled FIELD$  1 INPUT-AT FIELD$  RESOLVE-UNDER {: canon:ptr canonu:n known:bool :}
   canon canonu  loaded FIELD$  STR= 0= if false exit then
   canon canonu FS:FILE? 0= if false exit then
   kind SOURCE-UNIT:LOAD-REQUIRE <> if true exit then
   canon canonu known REGISTERED? 0= ;

\ ---- linking an object ---------------------------------------------------------
\ What a link of an object PART-FRESH? passed applies it with: each reference's value (RESOLVE), the non-code entries.
variable LINK-FN                     \ the emission being linked's address: SOURCE-FUNS' at
variable LINK-EM                     \ that emission's number in this object, -1 for none
variable LINK-OWNER                  \ the record that emission makes, -1 for none

: LINK-CLEAR ( -- ) -1 LINK-EM !  -1 LINK-OWNER !  0 LINK-FN ! ;


\ The dictionary index a linked record reference (file, sum, count; ID-REF+) names: this part's
\ record by OWN-REC, an earlier part's by PLACE; E-COMP-STATE when neither finds it.
: ID-REC ( n n n -- n )
   {: file:n sum:n count:n :}
   file 0= if
      sum count OWN-REC {: o:n :}
      o 0< if E-COMP-STATE throw then
      FILE-REC @ o + exit
   then
   file FILES-ROW sum count PLACE {: r:n :}
   r 0< if E-COMP-STATE throw then
   r ;


\ A quotation reference's address (file, owner's sum and count, f), adding the edge to the owner's emission: f of the
\ copy being made, which has no shadow row yet, or the host entry the shadow filed for it (OWNER-EM). A Wasm copy's
\ own function is its offset in the copy (OWN-FUNCTION), a host copy's its address.
: FN-ADDR ( n n n n -- n )
   {: file:n sum:n count:n f:n :}
   file sum count ID-REC {: r:n :}
   r LINK-OWNER @ = if
      FILE-EM @ LINK-EM @ + EM-EDGE+
      LINK-EM @ f OWN-FUN-AT {: off:n :}
      LINK-EM @ EM-ARCH CTARGET-ARCH:WASM CTARGET-ARCH:EQ if off exit then
      LINK-FN @ off +  exit
   then
   r OWNER-EM {: em:n :}
   em 0< if E-COMP-STATE throw then
   em EM-EDGE+
   em f NSHADOW:FUNCTION-SOURCE@ {: addr:n :}
   addr 0= if E-COMP-STATE throw then
   addr ;


\ A reference's value, from RD-REF's tag and fields (CODE+ states them); 0 for none.
: RESOLVE ( n n n n n -- n )
   {: tag:n x:n y:n z:n w:n :}
   tag 0 = if
      x y z ID-REC {: r:n :}
      r ndict@ >= if E-COMP-STATE throw then
      r PKG? if E-COMP-STATE throw then
      r REC-EDGE+  r XREF-REC XREF-START exit
   then
   tag 1 = if rbase x + exit then
   tag 2 = if x y z w FN-ADDR exit then
   tag 3 =  tag 6 =  or if
      tag 3 = if 1 else 2 then {: kind:n :}
      x 0= if
         kind y z OWN-EXT {: e:n :}
         e 0< if E-COMP-STATE throw then
         BUILT-EXTS-N @ e + DATA-EDGE+  e 0 EXT @ w + exit
      then
      x FILE-ROWS @ {: row:n :}
      row kind y z ROW-EXT {: b:n :}
      b 0< if E-COMP-STATE throw then
      b DATA-EDGE+  b 4 BUILT-EXT @ w + exit
   then
   \ A tag-7 edge names the extent holding the address as the compile found it (OWN-DATA-REF+,
   \ ROW-DATA-REF+), none for an address in no extent.
   tag 7 = if
      x 0= if
         FILE-DP @ y + {: at:n :}
         at EXT-OF {: e:n :}
         e 0 >= if BUILT-EXTS-N @ e + DATA-EDGE+ then
         at exit
      then
      x BUILT-DP BUILT@ y + {: at:n :}
      at  x FILE-ROWS @  ROW-EXT-OF {: b:n :}
      b 0 >= if b DATA-EDGE+ then
      at exit
   then
   tag 4 = if true HOLDS-HEADER !  HEADER-EDGE+  BASE @ x + exit then
   tag 5 = if 0 exit then
   E-COMP-STATE throw ;


\ Record idx's flags cell, through the alias as habu2.f C-IMMEDIATE writes it.
: REC-FLAGS ( n -- ptr n )
   XREF-REC-ADDR XREF-FLAGS-SLOT cells +
   REGION-ALIAS:DELTA-CELL HOST@ + {: at:n :}
   NULL-PTR BYTE-VIEW at + CELL-VIEW ;


\ The saved flags into record idx's cell, which keeps its own name length and EXT:
\ RECORD+ saves the flags without them.
: SET-FLAGS ( n n -- )
   {: saved:n idx:n :}
   idx REC-FLAGS {: at:ptr :}
   at @  DNAME-LEN-MASK DNAME-EXT or  and  saved or  at ! ;


\ The wordlist a role names (WORDLIST+); roles 3 and 4 are then the package's reference (file,
\ sum, count; ID-REC), and a package without that wordlist (id 0) is E-COMP-STATE.
: ROLE-WID ( -- n )
   RD-U64 {: role:n :}
   role 0 = if BOOT:GLOBAL-WL exit then
   role 1 = if WL:PRIMITIVES exit then
   role 2 = if BOOT:NAMESPACE-WL exit then
   role 5 = if BOOT:TYPES-WL exit then
   role 3 <>  role 4 <>  and if E-COMP-STATE throw then
   RD-U64 RD-U64 RD-U64 ID-REC XREF-REC {: rec:ptr :}
   role 3 = if rec XREF-PKG-PUBLIC else rec XREF-PKG-PRIVATE then {: wid:n :}
   wid 0= if E-COMP-STATE throw then
   wid ;


\ Entry i, a package (RECORD+), as the next record.
: LINK-PACKAGE ( n -- )
   {: i:n :}
   i ENTRY-OFFS @  OBJECT BYTES$ nip  RD-OPEN
   RD-U64 KIND-PACKAGE <> if E-COMP-STATE throw then
   RD-NAME {: a:ptr u:n :}
   RD-U64 {: flags:n :}
   ROLE-WID {: wid:n :}
   RD-U64 {: own:n :}
   GOOD? 0=  wid BOOT:NAMESPACE-WL <>  or if E-COMP-STATE throw then
   ndict@ {: want:n :}
   a u  own 1 =  INTERPRETER:PACKAGE-RECORD  want <> if E-COMP-STATE throw then
   flags want SET-FLAGS ;


\ Entry i, a late private wordlist (LATE+), given to its package.
: LINK-PRIVATE ( n -- )
   {: i:n :}
   i ENTRY-OFFS @  OBJECT BYTES$ nip  RD-OPEN
   RD-U64 KIND-PRIVATE <> if E-COMP-STATE throw then
   RD-U64 RD-U64 RD-U64 {: file:n sum:n count:n :}
   GOOD? 0= if E-COMP-STATE throw then
   file sum count ID-REC dup CHANGER+ INTERPRETER:PACKAGE-PRIVATE ;


\ Entry i, an earlier file's package this file sealed (SEAL+), sealed again.
: LINK-SEALED ( n -- )
   {: i:n :}
   i ENTRY-OFFS @  OBJECT BYTES$ nip  RD-OPEN
   RD-U64 KIND-SEALED <> if E-COMP-STATE throw then
   RD-U64 RD-U64 RD-U64 {: file:n sum:n count:n :}
   GOOD? 0= if E-COMP-STATE throw then
   file sum count ID-REC dup CHANGER+ XREF-REC XREF:SEAL ;


\ Entry i, an alias (RECORD+), as the next record.
: LINK-ALIAS ( n -- )
   {: i:n :}
   i ENTRY-OFFS @  OBJECT BYTES$ nip  RD-OPEN
   RD-U64 KIND-ALIAS <> if E-COMP-STATE throw then
   RD-NAME {: a:ptr u:n :}
   RD-U64 {: flags:n :}
   ROLE-WID {: wid:n :}
   RD-U64 RD-U64 RD-U64 {: file:n sum:n count:n :}
   GOOD? 0= if E-COMP-STATE throw then
   file sum count ID-REC {: src:n :}
   ndict@ {: want:n :}
   a u src wid INTERPRETER:ALIAS-RECORD
   flags want SET-FLAGS
   EDGES-N @ {: start:n :}  src REC-EDGE+  want start REC-EDGES!
   ALIASES-N @ 2 + ALIASES-RESERVE
   want ALIASES-N @ ALIASES !  src ALIASES-N @ 1 + ALIASES !  2 ALIASES-N +! ;


\ Entry i, a type entry (RECORD+), as the next record; LINK-FACTS gives it its text.
: LINK-TYPE ( n -- )
   {: i:n :}
   i ENTRY-OFFS @  OBJECT BYTES$ nip  RD-OPEN
   RD-U64 KIND-TYPE <> if E-COMP-STATE throw then
   RD-NAME {: a:ptr u:n :}
   RD-U64 {: flags:n :}
   ROLE-WID {: wid:n :}
   GOOD? 0= if E-COMP-STATE throw then
   ndict@ {: want:n :}
   TYPE-SOURCE {: src:n :}
   a u src wid INTERPRETER:ALIAS-RECORD
   flags want SET-FLAGS
   EDGES-N @ {: start:n :}
   src 0 >= if src REC-EDGE+ then  want start REC-EDGES!
   TFAM:ENTRY-LINKED ;

\ ---- linking code --------------------------------------------------------------
\ A definition's code entries: its record, its shadow emission as the compile filed it, and its
\ host copy placed and imported as a prepared body (NPUB:IMPORT-PENDING and its twins).

: HOST-CALL+ ( n -- )
   {: site:n :}
   HOST-CALLS-N @ 1 + HOST-CALLS-RESERVE
   site HOST-CALLS-N @ HOST-CALLS !  1 HOST-CALLS-N +! ;


: HOST-ADDR+ ( n -- )
   {: site:n :}
   HOST-ADDRS-N @ 1 + HOST-ADDRS-RESERVE
   site HOST-ADDRS-N @ HOST-ADDRS !  1 HOST-ADDRS-N +! ;


\ This object's emission e (CODE+) sealed in NEMIT over CODE-COPY: the object's bytes, as the
\ compile sealed them, with each address value written in. The answer is the entry of
\ the clause that patched the slot, 0 for none.
: NEMIT-FILL ( n -- n )
   {: e:n :}
   e EM-OFFS @  OBJECT BYTES$ nip  RD-OPEN
   RD-U64 RD-U64 {: s:n r:n :}
   CODE-COPY BUF:CLEAR
   RD-AT @ OBJ-PTR  CODE-COPY s BUF:N>BLEN BUF:GROW SPAN:$ drop  s BYTE-COPY
   s RD-SKIP
   CODE-COPY BYTES$ drop {: copy:ptr :}
   copy s r  e EM-ARCH  NEMIT:OPEN
   RD-U64 0 ?do RD-U64 NEMIT:FUNCTION+ loop
   0 HOST-CALLS-N !  0 HOST-ADDRS-N !
   RD-U64 0 ?do
      RD-U64 RD-U64 {: site:n k:n :}
      RD-REF RESOLVE {: target:n :}
      k 0= if NEMIT:CALL else NEMIT:TAIL then {: nk:n :}
      site nk target 0 0 NEMIT:CALL-SITE+
      nk NEMIT:CALL =  target NPUB:IN-REGION? 0=  and if site HOST-CALL+ then
   loop
   \ PATCH turns a code chain into adrp+add, with no address-map row; a data chain needs one.
   RD-U64 0 ?do
      RD-U64 RD-U64 {: site:n k:n :}
      RD-REF RESOLVE {: v:n :}
      k 0= if NEMIT:ADDR-CODE else NEMIT:ADDR-DATA then {: nk:n :}
      v  e EM-ARCH  copy  s  site  nk  ADDRESS!  0= if E-COMP-STATE throw then
      site nk -1 NEMIT:ADDR-SITE+
      k 0<> if site HOST-ADDR+ then
   loop
   RD-U64 {: p:n :}
   p 0 >= if
      RD-REF {: tag:n x:n y:n z:n w:n :}
      p NEMIT:PATCH-SLOT!
      tag 5 = if 0 else tag x y z w RESOLVE then
   else
      0
   then {: patch:n :}
   GOOD? 0= if E-COMP-STATE throw then
   NEMIT:SEAL  patch ;


\ The function of this object's emission e, from 1, that starts off bytes in (DOES-ENTRY-CK).
: DOES-FUN ( n n -- n )
   {: e:n off:n :}
   e OWN-FUNS 1 ?do
      e i OWN-FUN-AT off = if i unloop exit then
   loop
   E-COMP-STATE throw ;


\ Entry i, a definition (RECORD+), as the next record, placed and published, a KIND-DOES entry after it its companion;
\ its emission is the file's next, since each earlier MAKE-DEF of the piece published its COPIES. A cross build's
\ target copy e is filled and taken first, then the host copy h, which the build's process runs.
: MAKE-DEF ( n -- )
   {: i:n :}
   NSHADOW:EMISSIONS FILE-EM @ - {: e:n :}
   e COPIES + 1 - {: h:n :}
   i ENTRY-OFFS @  OBJECT BYTES$ nip  RD-OPEN
   RD-U64 KIND-DEF <> if E-COMP-STATE throw then
   RD-NAME {: a:ptr u:n :}
   RD-U64 {: flags:n :}
   ROLE-WID {: wid:n :}
   RD-REF {: tag:n x:n y:n z:n w:n :}
   GOOD? 0= if E-COMP-STATE throw then
   i 1 + ENTRIES-N @ < if i 1 + ENTRY-OFFS @ OBJ@ KIND-DOES = else false then {: does:bool :}
   does if
      i 1 + ENTRY-OFFS @  OBJECT BYTES$ nip  RD-OPEN
      RD-U64 drop  RD-NAME  RD-U64  ROLE-WID  RD-U64
   else
      NULL-PTR 0  0  0  0
   then {: da:ptr du:n dflags:n dwid:n off:n :}
   GOOD? 0= if E-COMP-STATE throw then
   does  tag 5 <>  and if E-COMP-STATE throw then
   does if e off DOES-FUN else 0 then {: j:n :}
   does if h j OWN-FUN-AT else 0 then {: hoff:n :}
   ndict@ {: want:n :}
   a u wid  flags  DKIND:MASK DNAME-CELL or DNAME-ALLOC or  and  INTERPRETER:DEF-RECORD
   NPUB:NEXT-SLOT {: fn:n :}
   fn LINK-FN !  want LINK-OWNER !
   h e <> if
      e LINK-EM !
      EDGES-N @ {: t-start:n :}
      e NEMIT-FILL drop
      FILE-EM @ e +  t-start EM-EDGES!
      does if j NSHADOW:TAKE-DOES else NSHADOW:TAKE then
      NEMIT:CLEAR
   then
   h LINK-EM !
   EDGES-N @ {: em-start:n :}
   h NEMIT-FILL {: patch:n :}
   FILE-EM @ h +  em-start EM-EDGES!
   EDGES-N @ {: start:n :}
   does if j NSHADOW:TAKE-HOST-DOES else NSHADOW:TAKE-HOST then
   fn NSHADOW:SOURCE-FUNS
   NEMIT:CLEAR
   want NSHADOW:PUBLISH
   \ As the does> primitive's patch: PATCH then writes the slot as a branch to the clause.
   patch 0<> if want patch NSHADOW:PATCH-DOES then
   CODE-COPY BYTES$ drop {: copy:ptr :}
   \ The call targets and chains already hold host values, so both resolvers answer what they get.
   NSHADOW:EMISSIONS 1 -  copy  fn  [: ;] [: ;]  ARM64:PATCH
   h EM-OFFS @ {: at:n :}
   at OBJ@  at 8 + OBJ@ {: s:n r:n :}
   r 0= if s CODE-SPAN:EXACT else s r - then {: recorded:n :}
   HOST-CALLS-N @ 0= if NULL-PTR else 0 HOST-CALLS then  HOST-CALLS-N @ {: calls:ptr calln:n :}
   HOST-ADDRS-N @ 0= if NULL-PTR else 0 HOST-ADDRS then  HOST-ADDRS-N @ {: addrs:ptr addrn:n :}
   does if
      copy s recorded calls calln addrs addrn false 0 hoff NPUB:IMPORT-PENDING-DOES
   else
      tag 5 <> if
         copy s calls calln addrs addrn false 0  tag x y z w RESOLVE  NPUB:IMPORT-PENDING-DEFER
      else
         copy s recorded calls calln addrs addrn false 0 NPUB:IMPORT-PENDING
      then
   then
   want <> if E-COMP-STATE throw then
   INTERPRETER:DEF-CLOSE
   \ After publication, as PENDING-CK refuses IMM and INT on a pending row; it also restores the
   \ wide and min-in bits the imports were given false 0 for.
   flags want SET-FLAGS
   FILE-EM @ e + COPY-EDGES+  want start REC-EDGES!
   does if
      want 1 + XREF-REC {: rec:ptr :}
      rec XREF-NAME$ da du STR= 0=  rec XREF-WORDLIST dwid <>  or if E-COMP-STATE throw then
      dflags want 1 + SET-FLAGS
      EDGES-N @ {: does-start:n :}
      FILE-EM @ e + COPY-EDGES+  want 1 + does-start REC-EDGES!
   then
   LINK-CLEAR ;


\ MAKE-DEF; a throw drops the open definition (DEF-ABORT) and an emission not yet filed, and passes on:
\ the build then fails, and its close drops the filed shadow emissions (NSHADOW:CLOSE), the records and
\ the code (FORGET-SPACE).
: LINK-DEF ( n -- )
   [: dup MAKE-DEF ;] catch {: i:n rc:n :}
   rc 0<> if NEMIT:CLEAR  NSHADOW:ABANDON  INTERPRETER:DEF-ABORT  LINK-CLEAR  rc throw then ;

\ ---- linking data --------------------------------------------------------------
\ An xt cell's value is a code address the link writes with xt!.
CAST: >XT ( n -- [ -- ] )
CAST: >XT-CELL ( ptr n -- ptr [ -- ] )

\ v into the cell at distance at, stored and marked as the compile's word for the kind did.
: PUT-CELL ( n n n -- )
   {: v:n kind:n at:n :}
   NULL-PTR BYTE-VIEW at + CELL-VIEW {: p:ptr :}
   kind 0 = if v >XT  p >XT-CELL  xt! exit then
   kind 1 = if v p ptr-cell! exit then
   kind 2 = if p scratch-cell-mark exit then
   kind 3 = if v p ! exit then
   E-COMP-STATE throw ;


\ A pass over the object's extents at FILE-DP plus their starts: 0 copies bytes, 1 puts cells (PUT-CELL) and keeps
\ extent i's references as EXT-EDGES of build-table row BUILT-EXTS-N plus i to be, 2 adopts strings (INTERN:ADOPT).
: LINK-EXTENTS ( n -- )
   {: pass:n :}
   EXTS-AT @  OBJECT BYTES$ nip  RD-OPEN
   8 RD-COUNT {: x:n :}
   x 8 * RD-SKIP
   x 0 ?do
      EXTS-AT @ 8 + i 8 * + OBJ@  FILE-DP @ + {: at:n :}
      RD-U64 RD-U64 RD-U64 RD-U64 {: kind:n sum:n count:n u:n :}
      RD-AT @ {: bytes:n :}
      u RD-SKIP
      GOOD? 0= if E-COMP-STATE throw then
      pass 0 = if  bytes OBJ-PTR  NULL-PTR BYTE-VIEW at +  u BYTE-COPY  then
      pass 2 =  kind 2 =  and if  at 8 + INTERN:ADOPT  then
      EDGES-N @ {: start:n :}
      RD-U64 0 ?do
         RD-U64 RD-U64 {: off:n ck:n :}
         RD-REF {: tag:n rx:n ry:n rz:n rw:n :}
         GOOD? 0= if E-COMP-STATE throw then
         pass 1 = if  tag rx ry rz rw RESOLVE  ck  at off +  PUT-CELL  then
      loop
      pass 1 = if  BUILT-EXTS-N @ i +  start EXT-EDGES!  then
   loop
   GOOD? 0= if E-COMP-STATE throw then ;


\ The object's DATA at here (LINK-EXTENTS' three passes), then its stores, after the file's entries. The caller first
\ sets FILE-DP to here's distance from NULL-PTR, as MARKS does.
: LINK-DATA ( -- )
   LINK-CLEAR
   here NULL-PTR -  FILE-DP @ <> if E-COMP-STATE throw then
   DATA-AT @ OBJ@ {: d:n :}
   d allot
   0 LINK-EXTENTS  1 LINK-EXTENTS  2 LINK-EXTENTS
   EDGES-N @ {: start:n :}
   STORES-AT @  OBJECT BYTES$ nip  RD-OPEN
   RD-U64 0 ?do
      RD-REF {: ctag:n cx:n cy:n cz:n cw:n :}
      RD-U64 RD-U64 {: kind:n v:n :}
      RD-REF {: tag:n x:n y:n z:n w:n :}
      GOOD? 0= if E-COMP-STATE throw then
      HOLDS-HEADER @ {: held:bool :}  EDGES-N @ {: edges:n :}
      ctag cx cy cz cw RESOLVE {: at:n :}
      held HOLDS-HEADER !  edges EDGES-N !        \ the store's cell, as STORES+ keeps it
      kind 3 = if v else tag x y z w RESOLVE then  kind  at  PUT-CELL
      at HSTORE+  at STORER+
   loop
   GOOD? 0= if E-COMP-STATE throw then
   BUILT-N start ROW-EDGES! ;


\ Entry i of the read object linked by its kind; a KIND-DOES entry went with the KIND-DEF before it.
: LINK-ENTRY ( n -- )
   {: i:n :}
   i ENTRY-OFFS @ OBJ@ {: kind:n :}
   kind KIND-DEF = if i LINK-DEF exit then
   kind KIND-PACKAGE = if i LINK-PACKAGE exit then
   kind KIND-ALIAS = if i LINK-ALIAS exit then
   kind KIND-PRIVATE = if i LINK-PRIVATE exit then
   kind KIND-SEALED = if i LINK-SEALED exit then
   kind KIND-TYPE = if i LINK-TYPE exit then
   kind KIND-DOES = if exit then
   E-COMP-STATE throw ;


\ A facts row's record (file, sum, count; ID-REF+) as a dictionary index: a link's by ID-REC; a
\ compile's by PART-REC for file 0, else by PLACE at REFS' row, which starts with the part's key.
: FACT-REC ( bool n n n -- n )
   {: read:bool file:n sum:n count:n :}
   read if file sum count ID-REC exit then
   file 0= if  sum count PART-REC  else
      REFS BYTES$ drop  file 1 - FILES-ROW-BYTES * +  sum count PLACE
   then {: r:n :}
   r 0< if E-COMP-STATE throw then
   r ;

\ A retirement row: a link retires its record again (XREF:RETIRE) in the compile's order (DUE!), so both see one
\ wordlist; either way an earlier piece's record counts as changed by this piece (CHANGER+).
: LINK-RETIRED ( bool -- )
   {: read:bool :}
   RD-U64 RD-U64 RD-U64 {: file:n sum:n count:n :}
   RD-U64 drop
   GOOD? 0= if E-COMP-STATE throw then
   read file sum count FACT-REC {: r:n :}
   read if r XREF-REC XREF:RETIRE then
   file 0<> if r CHANGER+ then ;

\ A record's text held again for it. An earlier file's record's text, which a `TRUST` row or an
\ amender wrote again, also changes that record and is read back.
: LINK-TEXT ( bool -- )
   {: read:bool :}
   RD-U64 RD-U64 RD-U64 {: file:n sum:n count:n :}
   RD-NAME {: a:ptr u:n :}
   GOOD? 0= if E-COMP-STATE throw then
   read file sum count FACT-REC {: r:n :}
   file 0= if a u r CHECK:FACTS! exit then
   r CHANGER+
   a u r CHECK:FACTS!
   read if a u r CHECK:READ-FACTS then ;


\ The FACTS replay: the offset in OBJECT of the next row and the rows left; for a link, the rows through the next
\ retirement row (0 when none) and that row's count of the part's records made before it.
variable REPLAY-AT
variable REPLAY-LEFT
variable DUE-ROWS
variable DUE-MADE

: REPLAY-FROM ( -- ) FACTS-AT @ 8 + REPLAY-AT !  FACTS-AT @ OBJ@ REPLAY-LEFT ! ;


\ DUE-ROWS and DUE-MADE for the first retirement row from REPLAY-AT on.
: DUE! ( -- )
   0 DUE-ROWS !
   REPLAY-AT @  REPLAY-LEFT @ 0 ?do
      {: at:n :}
      at OBJ@ CHECK:HELD-RETIRED = if
         i 1 + DUE-ROWS !  at 32 + OBJ@ DUE-MADE !  unloop exit
      then
      at FACT-ROW drop
   loop drop ;


\ The FACTS row at the reader held again.
: REPLAY-ROW ( bool -- )
   {: read:bool :}
   RD-U64 {: ix:n :}
   ix CHECK:HELD-RETIRED = if read LINK-RETIRED exit then
   ix CHECK:HELD-AMENDED = if read LINK-TEXT exit then
   RD-NAME {: a:ptr u:n :}
   GOOD? 0= if E-COMP-STATE throw then
   ix CHECK:HELD-LAID = if a u CHECK:LAY-FACTS then
   ix CHECK:HELD-UNDEFINE =  read  and if a u CHECK:READ-UNDEFINE then ;


\ The next n FACTS rows held again. The reader opens at REPLAY-AT: MAKE-DEF reopens it per entry.
: REPLAY-ROWS ( bool n -- )
   {: read:bool n:n :}
   REPLAY-AT @  OBJECT BYTES$ nip  RD-OPEN
   n 0 ?do read REPLAY-ROW loop
   GOOD? 0= if E-COMP-STATE throw then
   RD-AT @ REPLAY-AT !  n negate REPLAY-LEFT +! ;


\ Every FACTS row through the last retirement the compile held with no more of the part's records
\ made than the link has made, in row order; the rows after it may name records not made yet.
: REPLAY-DUE ( -- )
   begin
      DUE-ROWS @ 0 > if  DUE-MADE @  ndict@ FILE-REC @ -  <=  else false then
   while
      true DUE-ROWS @ REPLAY-ROWS  DUE!
   repeat ;


\ An entry that makes no record: a late private wordlist, a seal, a does> companion.
: RECORDLESS? ( n -- bool )
   ENTRY-OFFS @ OBJ@ {: kind:n :}
   kind KIND-PRIVATE =  kind KIND-SEALED =  or  kind KIND-DOES =  or ;

\ The entries in order, each record's after the FACTS rows due before it (REPLAY-DUE), so a retirement precedes records
\ made after it and follows a RECORDLESS? entry made before it; then the data. LINK-FACTS replays the rest.
: LINK-OBJECT ( -- )
   LINK-CLEAR
   0 ALIASES-N !
   REPLAY-FROM  DUE!
   ENTRIES-N @ 0 ?do  i RECORDLESS? 0= if REPLAY-DUE then  i LINK-ENTRY  loop
   LINK-DATA ;


\ FACTS replayed, then placed at here with the rest the file held: a link replays the rows LINK-OBJECT left; a cold
\ build, which applied them as it held them, replays every row. Cold and warm builds lay one text.
: LINK-FACTS ( bool -- )
   {: read:bool :}
   read 0= if REPLAY-FROM then
   read REPLAY-LEFT @ REPLAY-ROWS
   FILE-HELD @ CHECK:PLACE-FACTS ;

\ ---- the open ------------------------------------------------------------------
\ Installed while IMAGE-BASE-CELL is 0, so its xt! stores reach hb's own cells.
\ Installing again adds no address-cell row: EMIT-MARK keeps one row per cell.
: INSTALL-LOOP ( -- )
   INTERPRETER:INSTALL-EVALUATE
   INTERPRETER:INSTALL-CREATE ;

\ The address-cell header the boot lays (habu2.f), so EMIT-MARK records the
\ space's address cells in the space.
: LAY-ADDRESS-HEADER ( -- )
   SNAP-RELOC:XTCELL-N-CELL {: h:n :}
   0 h SPACE-AT !
   ADDRESS-CELLS:MAGIC h ADDRESS-CELLS:MAGIC-FIELD + SPACE-AT !
   ADDRESS-CELLS:BOOT-OFF h ADDRESS-CELLS:BASE-FIELD + SPACE-AT !
   ADDRESS-CELLS:BOOT-CAP h ADDRESS-CELLS:CAP-FIELD + SPACE-AT !
   0 h ADDRESS-CELLS:MODE-FIELD + SPACE-AT ! ;

\ The space, with ROUTED-BYTES below BASE: the band where hb's routed header stores mark their
\ cells (habu2.f ROUTE-HEADER), the linker's own; no object, image or program holds it.
: MAP ( -- )
   LAYOUT-SIZE {: size:n :}
   HINT size ROUTED-BYTES + MEM-PROT-RW MEM-MAP-PRIVATE-ANON NORESERVE or
   MEM-ANON-FD MEM-OFF-ZERO mmap {: at:n :}
   at 0 < if E-MEM-MAP throw then
   at ROUTED-BYTES + BASE !  size SPACE-SIZE !
   at HINT < if UNMAP E-MEM-MAP throw then
   construct step mapped PROGRESS !
   LAY-ADDRESS-HEADER ;

\ hb's native shadow, or a cross build's for the target's code.
: SHADOW ( -- )
   TARGET @ RTARGET:NATIVE-BINDING  CROSS? if NSHADOW:OPEN else NSHADOW:OPEN-NATIVE then
   construct step shadowed PROGRESS ! ;

\ The flags cell of hb's newest record: an `immediate` before the space's first
\ definition sets it.
: NEWEST-FLAGS ( -- ptr n ) FIRST-REC @ 1- REC-FLAGS ;

\ A space's own AT-EXIT leaves a runner the close forgets in the exit vector; a
\ no-op ADD arms hb's runner, which runs all of hb's actions, for RESTORE.
: ARM-EXIT ( -- ) [: ;] AT-EXIT:ADD ;

: SAVE ( -- )
   ARM-EXIT  EXIT-HOOK-CELL HOST@ SAVED-EXIT-HOOK !
   NEWEST-FLAGS @ SAVED-FLAGS !
   DP-CELL HOST@ SAVED-DP !
   BOOT:DP-FLOOR-CELL HOST@ SAVED-FLOOR !
   BOOT:DP-CEILING-CELL HOST@ SAVED-CEILING !
   DPHW-CELL HOST@ SAVED-DPHW !
   DATA-FLOOR-CELL HOST@ SAVED-DATA-FLOOR !
   BOOT:GLOBAL-WL-CELL HOST@ SAVED-GLOBAL-WL !
   BOOT:NAMESPACE-WL-CELL HOST@ SAVED-NAMESPACE-WL !
   BOOT:TYPES-WL-CELL HOST@ SAVED-TYPES-WL !
   COMMA-RUN-CELL HOST@ SAVED-COMMA-RUN !
   COMMA-END-CELL HOST@ SAVED-COMMA-END !
   COMMA-BASE-CELL HOST@ SAVED-COMMA-BASE !
   LASTC-CELL HOST@ SAVED-LASTC !
   AOT-CELLS:SIG-POOL-CELL HOST@ SAVED-SIG-POOL !
   AOT-CELLS:SIG-LEN-CELL HOST@ SAVED-SIG-LEN !
   get-current SAVED-CURRENT ! ;

\ DATA-FLOOR-CELL is an offset from hb's data-base (layout.f). The wordlists
\ come before any host cell moves: `wordlist` throws OVERLAY-OPEN in a replay.
: MOVE-IN ( -- )
   wordlist wordlist negate wordlist {: global:n ns:n types:n :}
   ndict@ FIRST-REC !
   SAVE
   BASE @ LAYOUT-START + {: start:n :}
   start DP-CELL HOST!
   start BOOT:DP-FLOOR-CELL HOST!
   BASE @ LAYOUT-CEILING + BOOT:DP-CEILING-CELL HOST!
   0 DPHW-CELL HOST!
   0 LASTC-CELL HOST!
   start DATA-N - DATA-FLOOR-CELL HOST!
   0 AOT-CELLS:SIG-POOL-CELL HOST!    \ a build has no signature pool
   0 AOT-CELLS:SIG-LEN-CELL HOST!
   global BOOT:GLOBAL-WL-CELL HOST!
   ns BOOT:NAMESPACE-WL-CELL HOST!
   types BOOT:TYPES-WL-CELL HOST!
   global set-current
   construct step moved PROGRESS ! ;

: CHECK-IN ( -- )
   SAVED-CEILING @ CHECK:OPEN
   construct step checked PROGRESS ! ;

\ The build's registry starts empty (include.f REQUIRE-SNAPSHOT), the loader logs each act,
\ which a compiling file's object carries (ACTS+), and INTERPRETER logs each word run (RAN+).
: REQUIRE-FRESH ( -- )
   REQUIRE-SNAPSHOT  EVENTS-RESET EVENT-ON  INTERPRETER:RECORD-RUNS
   construct step required PROGRESS ! ;

\ `[: execute ;]` here is E-EXEC-OPAQUE-XT; a quotation parameter is typed.
: RUN-UNIT ( n n [ n n -- ] -- ) execute ;

\ nested: a load another file's load requested, or a compiling file's tail. DP given back while open
\ was given back outside the build's files (END-CK refuses it in one); DPHW-CELL stays nonzero to the close.
: GOING ( bool -- )
   {: nested:bool :}
   PROGRESS @
   MATCH step
      none OF E-COMP-STATE throw ENDOF
      mapped OF E-COMP-STATE throw ENDOF
      shadowed OF E-COMP-STATE throw ENDOF
      moved OF E-COMP-STATE throw ENDOF
      checked OF E-COMP-STATE throw ENDOF
      required OF E-COMP-STATE throw ENDOF
      objects OF E-COMP-STATE throw ENDOF
      routed OF E-COMP-STATE throw ENDOF
      open OF
         nested if E-COMP-STATE throw then
         DPHW-CELL HOST@ 0<> if
            s" linker: data space was given back outside the build's files" SAY
            s"  (a caught failure or a negative allot)" SAY
            s" , so the build loads and writes nothing" SAY SAY-CR
            E-FAILED throw
         then
      ENDOF
      loading OF nested 0= if E-COMP-STATE throw then ENDOF
      returned OF E-COMP-STATE throw ENDOF
      failed OF
         REFUSAL s" its load failed and the failure was caught" SAY
         s" , so the build loads and writes nothing more" SAY SAY-CR
         E-FAILED throw
      ENDOF
   ;MATCH ;


\ The checker reads the file's aliases after its records' texts, which their sources may be.
: READ-ALIASES ( -- )
   ALIASES-N @ 0 ?do  i ALIASES @  i 1 + ALIASES @  CHECK:READ-ALIAS  2 +loop ;


\ As a file's load or its piece after a request starts. After an unclean cut (NEST) ENTRY-ROOT is the requested
\ file's root, but that part is UNLINKABLE, so its INPUT never holds it.
: MARKS ( -- )
   ndict@ FILE-REC !  CHECK:HELD-FACTS FILE-HELD !  0 RETIRED-N !  false HOLDS-HEADER !
   TFAM:TFAM-N@ FILE-TFAM !  CHECK:TYPE-END FILE-TYPES !
   NSHADOW:RECORDS FILE-ROW !  NSHADOW:EMISSIONS FILE-EM !
   here NULL-PTR - FILE-DP !
   WIDN-CELL HOST@ FILE-WID !     \ hb's DATA, where namespace-record and namespace-private mint ids
   EVENT-COUNT FILE-EVENT !  INTERPRETER:RUN-COUNT FILE-RUN !
   SOURCE-ROOT:CURRENT$ BUF:N>BLEN ENTRY-ROOT BUF:REPLACE ;


\ The scope a build's top-level file starts in: no package or part open, the build's global
\ wordlist current (MOVE-IN) and no `using` open. A compile's records, sections and lookups
\ follow the scope, which an object does not record, and a link opens no package: so a file
\ links only when its load starts in this scope, and a file with a piece that starts or ends
\ in another (a package it leaves open, a request inside a package) is UNLINKABLE.
: TOP-SCOPE? ( -- bool )
   PKG-REC-CELL HOST@ 0=  get-current BOOT:GLOBAL-WL =  and  USE-DEPTH-CELL HOST@ 0=  and ;

: SCOPE-CK ( -- ) TOP-SCOPE? 0= if true UNLINKABLE ! then ;


\ The piece's ACTS rows (ACTS-CK) applied as its compile made them, under the root each was spelled in: a provide
\ through `provided`; a require the registry must hold, as PART-FRESH? found it, else E-COMP-STATE.
: LINK-ACTS ( -- )
   ACTS-AT @  OBJECT BYTES$ nip  RD-OPEN
   RD-U64 0 ?do
      RD-U64 {: kind:n :}
      RD-NAME RD-NAME {: a:ptr u:n root:ptr rootu:n :}
      GOOD? 0= if E-COMP-STATE throw then
      kind EV-PROVIDED = if
         root rootu SOURCE-ROOT:CURRENT$ STR= if
            a u provided
         else
            root rootu [: a u provided ;] SOURCE-ROOT:WITH
         then
      else
         a u root rootu RESOLVE-UNDER nip nip 0= if E-COMP-STATE throw then
      then
   loop ;


\ The linked part's word times, from OBJECT's TIMES (PIECE-CK), into WORD-TIMES by its records.
: LINKED-TIMES ( -- )
   TIMES-AT {: at:n :}
   at OBJ@ {: n:n :}
   FILE-REC @ n + WORD-TIMES-RESERVE
   n 0 ?do  at 8 +  i WORD-BYTES * +  OBJ@  FILE-REC @ i +  WORD-TIMES !  loop ;


\ Part FILE-PART's object removed, so the next build compiles the part.
: DROP-OBJECT ( -- ) OBJECT$ FS:REMOVE-FILE ;


\ After INPUT!: true once part FILE-PART checked (PART-FRESH?), linked and joined BUILT; false, applying nothing, when
\ it does not. A throw removes the object (DROP-OBJECT), so the next build compiles the part instead of failing again.
: LINK-PART ( -- bool )
   false [:
      drop PART-FRESH? dup if
         LINK-OBJECT  LINKED-TIMES  true LINK-FACTS  READ-ALIASES  LINK-ACTS
         here NULL-PTR - FILE-END !
         PIECE-TIME PIECE-STORES-TIME BUILT+
      then
   ;] catch {: linked rc:n :}
   rc 0<> if DROP-OBJECT rc throw then
   linked ;

\ The root the linked object's compile left (INPUT's exit root) made current, as its ROOT! made it then, so the
\ request REPLAY loads and the next part resolve from it.
: LINK-ROOT ( -- )
   1 INPUT-AT FIELD$ {: x:ptr xu:n :}
   x xu SOURCE-ROOT:CURRENT$ STR= 0= if x xu SOURCE-ROOT:ROOT! then ;


\ A type entry for each family and linear type the piece declared, none for a linear type
\ its compile read from an entry; its name sits at CP, so `here` stays.
: TYPE-ENTRIES ( -- )
   TFAM:TFAM-N@ FILE-TFAM @ ?do
      i TFAM:ENTRY? 0= if
         ndict@ {: ix:n :}
         i TFAM:ENTRY-NAME$ TYPE-SOURCE BOOT:TYPES-WL INTERPRETER:ALIAS-RECORD
         i TFAM:ENTRY-TEXT$ ix CHECK:FACTS!
         i ix TFAM:ENTRY!
      then
   loop
   CHECK:TYPE-END FILE-TYPES @ ?do
      i CHECK:LINEAR? if
         i CHECK:TYPE-NAME$ CHECK:LINEAR-ENTRY 0 < if
            ndict@ {: ix:n :}
            i CHECK:TYPE-NAME$ TYPE-SOURCE BOOT:TYPES-WL INTERPRETER:ALIAS-RECORD
            i CHECK:LINEAR-TEXT$ ix CHECK:FACTS!
         then
      then
   loop ;


\ A linked file's top level never runs, so what a piece stores in scratch ends with it: every
\ scratch cell in file data, [OPEN-DP, DP), reads zero after each piece.
: RESET-SCRATCH ( -- )
   ROWS {: rows:ptr count:n :}
   count 0 ?do
      rows i cells + @ {: row:n :}
      row SCRATCH-ROW? if
         row ROW-AT OPEN-DP @ >= if  0 row ROW-OFF SPACE-AT !  then
      then
   loop ;


\ E-FAILED once DP moved back, then E-LIVE-TASK for a task left running. Both run before READS-KEEP, so a task
\ cannot read after the part's reads are kept and end before the refusal.
: END-CK ( -- )
   DPHW-CELL HOST@ 0<> if
      REFUSAL s" it gave back data space" SAY
      s"  (a caught failure or a negative allot)" SAY
      s" , so the build writes nothing" SAY SAY-CR
      E-FAILED throw
   then
   TASKS-LIVE-CELL HOST@ OPEN-TASKS @ > if
      REFUSAL s" a task is still running at the file's end" SAY SAY-CR  E-LIVE-TASK throw
   then ;


\ The piece's checks, RESET-SCRATCH, then PIECES (PIECE+): E-STORE (CHECK-EARLIER, FIND-STORES),
\ E-WORDLIST for an id no record holds.
: PIECE-END ( n -- )
   {: req:n :}
   FILE-HELD @ CHECK:AMENDED   \ the facts its `TRUST` rows and amenders changed
   TYPE-ENTRIES
   here NULL-PTR - FILE-END !
   RETIRED!  CHECK-EARLIER  FIND-STORES
   STORED-N @ 0 ?do i STORED @ dup HSTORE+ STORER+ loop
   RESET-SCRATCH
   WIDN-CELL HOST@ FILE-WID @ -  MINTED {: ids:n held:n :}
   ids held > if E-WORDLIST throw then
   ids held < if E-COMP-STATE throw then
   req PIECE+  RAN+ ;


\ Part FILE-PART's texts placed from OBJECT, as a link places them, then its BUILT row with time t.
: PIECE-PLACE ( n -- )
   {: t:n :}
   FILE-HELD @ CHECK:DROP-FACTS  false LINK-FACTS
   here NULL-PTR - FILE-END !          \ the piece's span ends after its texts
   t PIECE-STORES-TIME BUILT+ ;


\ The compiling part's NAMES (NAMED!) as it ends, after READS-KEEP, before PIECE-END, so a part after a request hits the file it loaded:
\ its source from FILE-OFF to end, then PART-TEXTS' texts and, if there are any, both again, so each is scanned with every package one opened.
: PART-NAMES ( n -- )
   {: end:n :}
   HITS-CLEAR
   NULL-PTR BYTE-VIEW FILE-SRC @ + FILE-OFF @ +  end FILE-OFF @ - {: src:ptr srcu:n :}
   src srcu SPELLED
   PART-TEXTS BYTES$ {: p:ptr pu:n :}
   2 0 do
      i 1 =  pu 0 >  and if src srcu SPELL-TEXT then
      0 begin dup pu < while
         {: at:n :}
         p at + 8 + LE:U64@  p at + 16 + LE:U64@ {: u:n v:n :}
         p at + 24 + u SPELL-TEXT
         at 24 + u + v + 7 + 8 / 8 *
      repeat drop
   loop
   NAMED! ;


\ INPUT-STAMP's three cells, which a requested file's load overwrites.
: STAMP@ ( -- n n n )
   INPUT-STAMP LE:U64@  INPUT-STAMP 8 + LE:U64@  INPUT-STAMP 16 + LE:U64@ ;

: STAMP! ( n n n -- )
   INPUT-STAMP 16 + LE:U64!  INPUT-STAMP 8 + LE:U64!  INPUT-STAMP LE:U64! ;


\ Loads the request linked part FILE-PART ends with by a nested LOAD, as a link does; answers the next part's offset.
\ The row is read first: the requested file's load reuses OBJECT and REQ-PATH.
: REPLAY ( -- n )
   REQ-FIELDS {: kind:n spelled:n loaded:n :}
   REQ-RESUME {: resume:n :}
   spelled FIELD$ BUF:N>BLEN REQ-PATH BUF:REPLACE
   STAMP@ {: s:n ns:n size:n :}
   FILE-PART @ {: part:n :}
   true PUSH-KEY
   kind REQ-PATH BYTES$ ['] SOURCE-UNIT:LOAD-PATH ['] POP-KEY finally
   s ns size STAMP!
   part 1 + FILE-PART !
   resume ;


\ The running recording's reads (COMPILE-READS:READS$), each whole: its texts into PART-TEXTS, the others into
\ PART-READS; none when no recording is active.
: READS-KEEP ( -- )
   PART-READS BUF:CLEAR  PART-TEXTS BUF:CLEAR
   COMPILE-READS:READS$ {: p:ptr pu:n :}
   0 begin dup pu < while
      {: at:n :}
      p at + 8 + LE:U64@  p at + 16 + LE:U64@ {: u:n v:n :}
      at 24 + u + v + 7 + 8 / 8 * {: next:n :}
      p at + LE:U64@ COMPILE-READS:TEXT-READ = if
         p at + next at - PART-TEXTS SPAN+
      else
         p at + next at - PART-READS SPAN+
      then
      next
   repeat drop ;

\ The file's parts in turn (TURN): one links (LINK-PART) and replays its request, or compiles to its request's clean cut
\ (NEST) or the end.
: LOAD ( ptr u8 n ptr u8 n ptr u8 n [ n n -- ] -- )
   {: path:ptr pathu:n root:ptr rootu:n source:ptr sourceu:n q :}
   path pathu KEY!
   path pathu PATH!
   path pathu root rootu PATH+
   MARKS
   INPUT!
   0 FILE-PART !  0 FILE-OFF !
   source NULL-PTR - FILE-SRC !  sourceu FILE-SRC-U !
   construct turn link TURN !
   false begin
      {: compiled:bool :}
      TURN @ MATCH turn
         link OF
            TOP-SCOPE? if LINK-PART else false then if
               LINK-ROOT                  \ outside COMPILE-READS:RECORD, so its ROOT! records nothing
               REQS-N @ 0= if
                  compiled true
               else
                  REPLAY FILE-OFF !
                  path pathu PATH!  MARKS
                  source NULL-PTR - FILE-SRC !  sourceu FILE-SRC-U !
                  construct turn link TURN !
                  compiled false
               then
            else
               construct turn compile TURN !
               compiled false
            then
         ENDOF
         compile OF
            OBJECT-OPEN  COPY-BELOW
            STAMPED? 0= UNLINKABLE !  SCOPE-CK
            false PUSH-KEY
            [:
               FILE-OFF @ sourceu q ['] RUN-UNIT ['] POP-KEY finally
               true GOING                 \ E-FAILED: a request failed and the source caught it
               TURN @ MATCH turn          \ the source ended: END-CK before READS-KEEP
                  link OF ENDOF
                  compile OF END-CK ENDOF
                  cut OF ENDOF
               ;MATCH
               READS-KEEP
            ;] COMPILE-READS:RECORD
            TURN @ MATCH turn
               link OF E-COMP-STATE throw ENDOF
               compile OF                 \ the source ended
                  SCOPE-CK
                  sourceu PART-NAMES
                  -1 PIECE-END
                  SOURCE-ROOT:CURRENT$ BUF:N>BLEN EXIT-ROOT BUF:REPLACE   \ the file's load scope is still open
                  0 COMPILED-TIME dup WRITE-OBJECT
                  PIECE-PLACE
                  true true
               ENDOF
               cut OF
                  path pathu PATH!  MARKS
                  construct turn link TURN !
                  true false
               ENDOF
            ;MATCH
         ENDOF
         cut OF E-COMP-STATE throw ENDOF
      ;MATCH
   until
   if 1 COMPILED-N +! else 1 LINKED-N +! then ;


\ Whether event ev of the loader's log is the request that loads path: the piece's, an include or a
\ require the registry did not hold, for that path.
: REQUEST? ( n ptr u8 n -- bool )
   {: ev:n path:ptr pathu:n :}
   ev FILE-EVENT @ < if false exit then
   ev EVENT-KIND@ {: kind:n :}
   kind EV-INCLUDED =
   kind EV-REQUIRED =  ev EVENT-STATE@ EV-STATE-FRESH =  and  or
   ev EVENT-PATH@ path pathu STR=  and ;


\ The next part's offset past request ev's token, -1 unless a clean cut: top scope, host section, empty stack, no open
\ definition, the token in the source, a loader word run last, this file's reader at the token's end (NEST ends it).
: RESUME ( n -- n )
   {: ev:n :}
   ev EVENT-TOK@ {: a:n u:n :}
   FILE-SRC @ {: src:n :}
   TOP-SCOPE? 0= if -1 exit then
   SECTION-CELL HOST@ 0<> if -1 exit then
   ev EVENT-DEPTH@ 0<> if -1 exit then
   PEND-CELL HOST@ 0<> if -1 exit then
   a src <  a u +  src FILE-SRC-U @ +  >  or if -1 exit then
   INTERPRETER:RUN-COUNT FILE-RUN @ <= if -1 exit then
   INTERPRETER:RUN-COUNT 1 - INTERPRETER:RUN-RECORD@ {: idx:n :}
   INTERPRETER:OBSERVE-WORD idx  idx XREF-REC XREF-START  SOURCE-UNIT:LOADER-KIND  SOURCE-UNIT:NONE = if -1 exit then
   SRCLOC:INB-CELL HOST@ src <>  INE-CELL HOST@  src FILE-SRC-U @ +  <>  or
   INP-CELL HOST@  a u +  <>  or if -1 exit then
   a u + src - ;


\ The newest request (REQUEST?) ends the part (names, REQUESTS row, object, BUILT row), then loads its file unrecorded.
\ At a clean cut (RESUME) the next part is LOAD's turn; else it compiles on in this reader, both parts UNLINKABLE.
: NEST ( ptr u8 n ptr u8 n ptr u8 n [ n n -- ] -- )
   {: path:ptr pathu:n root:ptr rootu:n source:ptr sourceu:n q :}
   EVENT-COUNT 1 - {: ev:n :}
   ev path pathu REQUEST? 0= if E-COMP-STATE throw then
   ev EVENT-KIND@ EV-INCLUDED = if SOURCE-UNIT:LOAD-INCLUDE else SOURCE-UNIT:LOAD-REQUIRE then {: mode:n :}
   ev RESUME {: resume:n :}
   resume 0< if true UNLINKABLE ! then
   END-CK READS-KEEP              \ NEST runs inside the requesting part's RUN-UNIT, so the recording is its
   resume 0< if FILE-SRC-U @ else resume then PART-NAMES
   mode REQUESTS U64+  resume REQUESTS U64+
   ev EVENT-SPELLED@ REQUESTS FIELD+  path pathu REQUESTS FIELD+
   0 PIECE-END
   0 COMPILED-TIME {: t:n :}
   ev SOURCE-EVENT:CURRENT@ BUF:N>BLEN EXIT-ROOT BUF:REPLACE   \ CURRENT$ is already the requested file's root
   t WRITE-OBJECT
   t PIECE-PLACE
   align   \ the next part starts at a cell boundary, as include.f LOAD-UNIT leaves here
   STAMP@ {: s:n ns:n size:n :}
   FILE-PART @  FILE-OFF @  FILE-SRC @  FILE-SRC-U @ {: part:n off:n src:n srcu:n :}
   UNLINKABLE @ {: unlinkable:bool :}
   TURN @ {: was:turn :}
   [: path pathu root rootu source sourceu q LOAD ;] COMPILE-READS:UNRECORDED
   s ns size STAMP!
   part 1 + FILE-PART !  off FILE-OFF !  src FILE-SRC !  srcu FILE-SRC-U !
   resume 0< if unlinkable UNLINKABLE ! then
   resume 0 >= if resume FILE-OFF ! then
   LOADER$ {: k:ptr ku:n :}
   k FILE-KEY ku BYTE-COPY
   FILE-KEY CANON$ PATH!
   align
   resume 0< if
      was TURN !
      OBJECT-OPEN  MARKS  COPY-BELOW  SCOPE-CK
   else
      INE-CELL HOST@ INP-CELL HOST!
      construct turn cut TURN !
   then ;


\ The load, nested when a compiling file requested it.
: ENTER ( ptr u8 n ptr u8 n ptr u8 n [ n n -- ] -- )
   LOADER$ nip 0<>  LINKING? 0=  and if NEST else LOAD then
   construct step returned PROGRESS ! ;

\ After a load, whether it returned or threw: one that returned leaves the build open, or its
\ loader's load running; one that threw fails the build, so every later load throws E-FAILED.
: LEFT ( -- )
   PROGRESS @
   MATCH step
      none OF E-COMP-STATE throw ENDOF
      mapped OF E-COMP-STATE throw ENDOF
      shadowed OF E-COMP-STATE throw ENDOF
      moved OF E-COMP-STATE throw ENDOF
      checked OF E-COMP-STATE throw ENDOF
      required OF E-COMP-STATE throw ENDOF
      objects OF E-COMP-STATE throw ENDOF
      routed OF E-COMP-STATE throw ENDOF
      open OF E-COMP-STATE throw ENDOF
      loading OF construct step failed PROGRESS ! ENDOF
      returned OF
         LOADER$ nip 0<> if construct step loading else construct step open then PROGRESS !
      ENDOF
      failed OF ENDOF
   ;MATCH ;


\ A file's load once GOING admits it: ENTER (LOAD, or NEST for a compiling file's request), then
\ LEFT on either exit.
: RUN-FILE ( ptr u8 n ptr u8 n ptr u8 n [ n n -- ] -- )
   LOADER$ nip 0<> GOING
   construct step loading PROGRESS !
   ['] ENTER ['] LEFT finally ;

: OBJECTS-IN ( -- )
   INIT-OBJECTS
   construct step objects PROGRESS ! ;

: ROUTE ( -- )
   SOURCE-UNIT:CURRENT PRIOR-RUNNER !
   [: RUN-FILE ;] SOURCE-UNIT:USE
   construct step routed PROGRESS ! ;

\ From here header stores go to the space (habu2.f ROUTE-HEADER).
: IMAGE-IN ( -- )
   BASE @ BOOT:IMAGE-BASE-CELL HOST!
   construct step open PROGRESS ! ;

\ ---- owned primitives -----------------------------------------------------------
\ A primitive a prims.f package row types (EPPRIM:) is its package's to call: the open lays a
\ record of it in that package's private wordlist, whose code is the kernel's and which carries
\ the primitive's facts text as every word's record does. The package's first file reopens the
\ package the open made (interpreter.f PKG-ENSURE).

\ hb's own record of owned primitive row k in its package's private wordlist, which an hb a build
\ wrote holds; else NULL-PTR. Only a build that states no object, which reads no tree rows, uses it.
: HB-OWNED ( n -- ptr n )
   {: k:n :}
   k PRIM-SPEC:PKG$ BOOT:NAMESPACE-WL CHECKER-RESOLVE:WL-PROBE {: pkg:ptr :}
   pkg XREF-FOUND? 0= if XREF-NULL exit then
   pkg XREF-PKG-PRIVATE {: wid:n :}
   wid 0= if XREF-NULL exit then
   k PRIM-SPEC:NAME$ wid CHECKER-RESOLVE:WL-PROBE ;

: OWNED-COPY ( ptr u8 n -- n n )
   {: a:ptr u:n :}
   OWNED-TEXT-N @ {: off:n :}
   off u + OWNED-TEXT-RESERVE
   a  off OWNED-TEXT  u BYTE-COPY
   off u + OWNED-TEXT-N !
   off u ;

\ Gathers package pa pu's primitive na nu: hb's kernel record of it, and copies of both names and of the facts text
\ CHECK:OWNED-FACTS gives (rec's, else the current rows'). With no text its calls would go unchecked: the open fails.
: OWNED+ ( ptr n ptr u8 n ptr u8 n -- )
   {: rec:ptr pa:ptr pu:n na:ptr nu:n :}
   na nu WL:PRIMITIVES CHECKER-RESOLVE:WL-PROBE {: src:ptr :}
   src XREF-FOUND? 0= if
      s" linker: hb has no primitive " SAY  na nu SAY
      s" , which an owned row of " SAY  pa pu SAY  s"  names" SAY  SAY-CR
      E-COMP-STATE throw
   then
   rec pa pu na nu CHECK:OWNED-FACTS {: a:ptr u:n :}
   u 0= if
      s" linker: no facts text says owned primitive " SAY
      pa pu SAY  s" :" SAY  na nu SAY  SAY-CR
      E-COMP-STATE throw
   then
   OWNED-N @ OWNED-CELLS * {: at:n :}
   at OWNED-CELLS + OWNED-RESERVE
   src CHECKER-RESOLVE:REC>INDEX  at OWNED !
   pa pu OWNED-COPY  at 2 + OWNED !  at 1 + OWNED !
   na nu OWNED-COPY  at 4 + OWNED !  at 3 + OWNED !
   a u OWNED-COPY  at 6 + OWNED !  at 5 + OWNED !
   1 OWNED-N +! ;

\ A build that links the object ARM64:READ read types the kernel's primitives by
\ its rows; a kernel build and a Wasm target's link no arm64 object. WASM? needs the shadow open.
: STATES-PRIMS? ( -- bool ) KERNEL? @ 0=  ARM64:READ? and  WASM? 0= and ;

\ Before MOVE-IN, while hb's namespace and checker stores are current: a build that states no
\ object types its owned primitives by hb's rows and records.
: OWNED-TEXTS ( -- )
   0 OWNED-N !  0 OWNED-TEXT-N !
   STATES-PRIMS? if exit then
   PRIM-SPEC:COUNT 0 ?do
      i PRIM-SPEC:KIND@ PRIM-SPEC:K-PKG-PRIVATE = if
         i HB-OWNED  i PRIM-SPEC:PKG$  i PRIM-SPEC:NAME$  OWNED+
      then
   loop ;

: OWNED@ ( n n -- n ) {: r:n f:n :} r OWNED-CELLS * f + OWNED @ ;

\ Owned row r's text at field f: its offset, then its length.
: OWNED$ ( n n -- ptr u8 n ) {: r:n f:n :} r f OWNED@ OWNED-TEXT  r f 1 + OWNED@ ;

\ Package a u's build record, made with its private wordlist when no earlier row made it.
: OWNER ( ptr u8 n -- ptr n )
   {: a:ptr u:n :}
   a u BOOT:NAMESPACE-WL CHECKER-RESOLVE:WL-PROBE {: pkg:ptr :}
   pkg XREF-FOUND? if pkg exit then
   a u true INTERPRETER:PACKAGE-RECORD XREF-REC ;

\ Owned row r's record, published as MAKE-DEF publishes a definition's, with the kernel's code
\ and hb's record's immediate, wide and minimum-input bits, the ones alias-record keeps.
: MAKE-OWNED ( n -- )
   {: r:n :}
   r 0 OWNED@ XREF-REC {: src:ptr :}
   r 1 OWNED$ OWNER XREF-PKG-PRIVATE {: wid:n :}
   ndict@ {: want:n :}
   r 3 OWNED$ wid 0 INTERPRETER:DEF-RECORD
   src XREF-START  src XREF-RAW-LEN  NPUB:IMPORT-ENTRY  want <> if E-COMP-STATE throw then
   INTERPRETER:DEF-CLOSE
   src XREF-FLAGS  DNAME-IMM DNAME-WIDE or DNAME-MIN-IN-MASK or  and  want SET-FLAGS
   r 5 OWNED$  want CHECK:FACTS! ;

\ MAKE-OWNED; a throw drops the open definition and passes on, failing the open.
: LINK-OWNED ( n -- )
   [: dup MAKE-OWNED ;] catch {: r:n rc:n :}
   rc 0<> if INTERPRETER:DEF-ABORT rc throw then ;

\ After IMAGE-IN, the records OWNED+ gathered as BUILT row 0, laid before any file as header data: key the empty path's, which no
\ load has; piece 0 at time 0, its content following from hb, whose stamp names the objects' dir (BUILD-OUT:OBJ$).
: OWNED-RECORDS ( -- )
   FIRST-REC @ FILE-REC !  NSHADOW:EMISSIONS FILE-EM !  false HOLDS-HEADER !
   OWNED-N @ 0 ?do i LINK-OWNED loop
   0 CHECK:PLACE-FACTS
   s" " KEY-PATHS PATH-IN FILE-KEY LE:U64!  0 FILE-KEY 8 + LE:U64!  0 FILE-PART !
   FILE-KEY KEY-BYTES LOAD-PATHS SPAN+  s" " LOAD-PATHS FIELD+
   here NULL-PTR - dup FILE-DP !  FILE-END !
   0 EXTS-N !  0 0 BUILT+ ;

\ The object's rows, then its owned primitives' texts, which those rows render.
: PRIMS-IN ( -- )
   STATES-PRIMS? 0= if exit then
   ARM64:STATE-PRIMS
   ARM64:OWNED-N 0 ?do  XREF-NULL  i ARM64:OWNED-PKG$  i ARM64:OWNED-NAME$  OWNED+  loop ;

\ OWNED-TEXTS runs once the shadow is open (STATES-PRIMS?) and before MOVE-IN replaces hb's namespace.
: OPEN ( -- )
   0 COMPILED-N !  0 LINKED-N !
   INSTALL-LOOP
   MAP SHADOW OWNED-TEXTS MOVE-IN CHECK-IN PRIMS-IN REQUIRE-FRESH OBJECTS-IN ROUTE IMAGE-IN
   OWNED-RECORDS
   here NULL-PTR - OPEN-DP !
   TASKS-LIVE-CELL HOST@ OPEN-TASKS ! ;

\ ---- the close -----------------------------------------------------------------
: RESTORE ( -- )
   SAVED-CURRENT @ set-current
   SAVED-DP @ DP-CELL HOST!
   SAVED-FLOOR @ BOOT:DP-FLOOR-CELL HOST!
   SAVED-CEILING @ BOOT:DP-CEILING-CELL HOST!
   SAVED-DPHW @ DPHW-CELL HOST!
   SAVED-DATA-FLOOR @ DATA-FLOOR-CELL HOST!
   SAVED-GLOBAL-WL @ BOOT:GLOBAL-WL-CELL HOST!
   SAVED-NAMESPACE-WL @ BOOT:NAMESPACE-WL-CELL HOST!
   SAVED-TYPES-WL @ BOOT:TYPES-WL-CELL HOST!
   SAVED-COMMA-RUN @ COMMA-RUN-CELL HOST!
   SAVED-COMMA-END @ COMMA-END-CELL HOST!
   SAVED-COMMA-BASE @ COMMA-BASE-CELL HOST!
   SAVED-LASTC @ LASTC-CELL HOST!
   SAVED-SIG-POOL @ AOT-CELLS:SIG-POOL-CELL HOST!
   SAVED-SIG-LEN @ AOT-CELLS:SIG-LEN-CELL HOST!
   SAVED-EXIT-HOOK @ EXIT-HOOK-CELL HOST!
   SAVED-FLAGS @ NEWEST-FLAGS ! ;

\ Records still below the count would name bodies in the unmapped space; their
\ code goes back as FORGET-DEFS-FROM gives it.
: FORGET-SPACE ( -- )
   FIRST-REC @ CODE-RECLAIM:FLOOR-FROM {: floor:n :}
   FIRST-REC @ ndict!
   floor CODE-RECLAIM:TRUNCATE ;

\ Undo IMAGE-IN, from open or from a file's loading, returned or failed step.
: IMAGE-OUT ( -- bool )
   0 BOOT:IMAGE-BASE-CELL HOST!  INTERN:CLEANUP
   construct step routed PROGRESS ! true ;

\ Undo the last step that ran; false once none is left.
: UNDO ( -- bool )
   PROGRESS @
   MATCH step
      none OF false ENDOF
      mapped OF
         UNMAP construct step none PROGRESS ! true
      ENDOF
      shadowed OF
         NSHADOW:CLOSE construct step mapped PROGRESS ! true
      ENDOF
      moved OF
         RESTORE FORGET-SPACE construct step shadowed PROGRESS ! true
      ENDOF
      checked OF
         CHECK:CLOSE construct step moved PROGRESS ! true
      ENDOF
      required OF
         INTERPRETER:;RECORD-RUNS  EVENT-OFF EVENTS-RESET REQUIRE-RESTORE construct step checked PROGRESS ! true
      ENDOF
      objects OF
         DISPOSE-OBJECTS construct step required PROGRESS ! true
      ENDOF
      routed OF
         PRIOR-RUNNER @ SOURCE-UNIT:USE construct step objects PROGRESS ! true
      ENDOF
      open OF IMAGE-OUT ENDOF
      loading OF IMAGE-OUT ENDOF
      returned OF IMAGE-OUT ENDOF
      failed OF IMAGE-OUT ENDOF
   ;MATCH ;

: CLOSE ( -- )
   begin UNDO 0= until ;

: RUN ( -- ) OPEN  BODY @ execute ;

: IDLE? ( -- bool )
   PROGRESS @
   MATCH step
      none OF true ENDOF
      mapped OF false ENDOF
      shadowed OF false ENDOF
      moved OF false ENDOF
      checked OF false ENDOF
      required OF false ENDOF
      objects OF false ENDOF
      routed OF false ENDOF
      open OF false ENDOF
      loading OF false ENDOF
      returned OF false ENDOF
      failed OF false ENDOF
   ;MATCH ;

public

\ Run the body in a build's space with no package of hb's open, close on every
\ exit, reopen the caller's package, rethrow; E-COMP-STATE while a build is open.
\ The mode is "engine", "kernel" for a kernel build, or a stripped program's; the
\ target is the one the program runs on, and another than hb's own makes a cross build.
: WITH ( ptr u8 n RTARGET:resolved-target [ -- ] -- )
   {: mode:ptr modeu:n target:RTARGET:resolved-target body :}
   IDLE? 0= if E-COMP-STATE throw then
   target TARGET !
   mode modeu s" engine" STR= ENGINE? !
   mode modeu s" kernel" STR= KERNEL? !
   body BODY !
   [: ['] RUN ['] CLOSE finally ;] INTERPRETER:WITHOUT-PACKAGE ;

\ The kernel object at the path, written by a kernel build: the engine and
\ program builds that follow lay their space as its kernel reads it and link it.
: KERNEL ( ptr u8 n -- ) ARM64:READ ;

\ True while a build's space is open, from WITH's open to the end of its close.
: OPEN? ( -- bool ) IDLE? 0= ;

\ The files the last build compiled and linked, from its open until the next open.
: COMPILED ( -- n ) COMPILED-N @ ;
: LINKED ( -- n ) LINKED-N @ ;

;package
