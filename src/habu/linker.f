\ linker.f - the cross-build's data space: WITH maps a space beside hb's DATA
\ for the build's target, points DP, the wordlists and the image base at it,
\ switches the checker's registered stores to the build's (CHECK:OPEN), routes
\ every file load to the build's runner (SOURCE-UNIT:USE), lays each owned
\ primitive's record in its package as BUILT row 0 (OWNED-RECORDS), runs the
\ body there with no package of hb's open and closes it. The close undoes
\ exactly the open steps that ran, in reverse. Each file load links the file's
\ object when it is fresh (FRESH?) and otherwise compiles the file, writing its
\ object under the out root. An object whose file requested others is fresh
\ only when theirs are too. The loader's log (include.f EVENT-RECORD) names a
\ compile's requests and its registry acts, which its object carries and a link
\ repeats.

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
require src/habu/interpret.f
require src/habu/definers.f
require src/habu/intern.f
require src/core/include.f
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
\ to its last BUILT row, the loads it requests included; returned: a load's body returned (LEFT);
\ failed: a load threw, DP moved back, E-STORE or E-WORDLIST.
ENUM step none mapped shadowed moved checked required objects routed open loading returned failed ;ENUM
TYPED-VARIABLE PROGRESS step

public
-9690 s" a file of this build failed or DP moved back in its space: the build loads no other file and writes no object or program" ERROR:CODE E-FAILED
-9691 s" a file of this build changed an earlier record or code, or stored into a cell no file holds" ERROR:CODE E-STORE
-9692 s" a file a linked file requested, or a linked file's later part, did not check at its turn: the next build compiles them" ERROR:CODE E-STALE
-9693 s" a file of this build made a wordlist no package of it holds" ERROR:CODE E-WORDLIST
-9694 s" a file of this build left a task running at its end: its object would hold data the task can still change" ERROR:CODE E-LIVE-TASK
private

TYPED-VARIABLE ENGINE? bool          \ the mode is "engine"
TYPED-VARIABLE KERNEL? bool          \ the mode is "kernel": hb's own layout, no program
TYPED-VARIABLE TARGET RTARGET:resolved-target   \ the target the build's program runs on
variable BASE                        \ the space's address, its distance from NULL-PTR
variable SPACE-SIZE                  \ the space's size, as MAP mapped it
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
   NULL-PTR BYTE-VIEW BASE @ + SPACE-SIZE @ munmap
   0<> if E-MEM-UNMAP throw then ;

\ Above hb's DATA: below it CHECKER-OWNER-GUARD refuses with E-COMP-OWNER.
: HINT ( -- n ) DATA-N DATA-SIZE + ;

\ A build for another target than hb's own.
: CROSS? ( -- bool ) TARGET @ RTARGET:HOST-TARGET RTARGET:SAME? 0= ;

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
\ emission, and the data pointer as the piece starts and ends. A FILES row is a BUILT
\ row's first FILES-ROW-BYTES bytes.
16 constant KEY-BYTES
KEY-BYTES 64 + constant BUILT-BYTES
KEY-BYTES constant BUILT-PIECE
BUILT-PIECE 8 + constant BUILT-TIME
BUILT-TIME 8 + constant BUILT-FIRST
BUILT-FIRST 16 + constant BUILT-EM
BUILT-EM 16 + constant BUILT-DP
KEY-BYTES 16 + constant FILES-ROW-BYTES
KEY-BYTES 8 + constant LOADER-BYTES
create LOADERS BUF:HDR-BYTES allot
create LOADS BUF:HDR-BYTES allot
create BUILT BUF:HDR-BYTES allot
create REFS BUF:HDR-BYTES allot      \ the object's FILES rows
create REQUESTS BUF:HDR-BYTES allot  \ the object's REQUESTS rows
create PIECES BUF:HDR-BYTES allot    \ the object's PIECES
create TIMES BUF:HDR-BYTES allot     \ the object's TIMES, a block per piece compiled so far
create NAMED-ROWS BUF:HDR-BYTES allot \ the BUILT rows the compiling file's source names, 8 bytes each
create SPELLS BUF:HDR-BYTES allot    \ the object's NAMES: SPELLED's HIT sums, each open a 0 then its sum
create RAN-ROWS BUF:HDR-BYTES allot  \ the BUILT rows whose words the compiling file's pieces ran, each once
create REACHED BUF:HDR-BYTES allot   \ the object's REACHED: the FILES rows of the pieces RAN-FILES+ walked, 8 bytes each
create FRAMES BUF:HDR-BYTES allot    \ each compiling loader's object so far while its request loads
create PARKED BUF:HDR-BYTES allot    \ each checked or linking file's place in its object while its request is checked or loads
create PENDING BUF:HDR-BYTES allot   \ the pieces a root's check passed, in load order, as FILES rows
create PENDING-PATHS BUF:HDR-BYTES allot \ the paths those pieces registered, each a FIELD+
create REQ-PATH BUF:HDR-BYTES allot  \ the path a linked request spells
256 constant OBJECT-DEPTHS
OBJECT-DEPTHS BUF:HDR-BYTES * BUFFER: OBJECTS \ OBJECT at each depth of requests (PARK)
variable OBJECT-DEPTH                \ the frames on PARKED, and OBJECT's index in OBJECTS
: OBJECT-AT ( n -- ptr u8 ) BUF:HDR-BYTES * OBJECTS + ;
\ A key's fields, then the object of the file at this depth; a parked loader's stays in its own (PARK).
: OBJECT ( -- ptr u8 ) OBJECT-DEPTH @ OBJECT-AT ;
create PRIOR BUF:HDR-BYTES allot     \ the compiling file's object as the last build left it
create CODE-COPY BUF:HDR-BYTES allot \ an emission's bytes as the link fills and places them
create LATE BUF:HDR-BYTES allot      \ the file's late private wordlists
create LOAD-PATHS BUF:HDR-BYTES allot \ each load's key, then its path below its root as a FIELD+
create KEY-PATHS BUF:HDR-BYTES allot  \ each loaded or named file's path once, a FIELD+; a key's first cell is its offset
create OBJ-PATHS BUF:HDR-BYTES allot  \ the object's PATHS as WRITE-OBJECT lays it
create OBJ-NAME BUF:HDR-BYTES allot   \ an object's name below BUILD-OUT:OBJ$'s directory
DYNAMIC-BUFFER BUILT-PATHS n         \ BUILT row r's path: its FIELD+'s offset in LOAD-PATHS
KEY-BYTES BUFFER: FILE-KEY
3 8 * constant INPUT-BYTES
INPUT-BYTES BUFFER: INPUT-STAMP      \ the source's stamp (INPUT!), the object's INPUT
KEY-BYTES BUFFER: WANT-KEY           \ a checked request's FILES row as its loader names it: key, piece, time
variable WANT-PIECE
variable WANT-TIME
variable REQ-N                       \ the compiling file's requests so far, REQUESTS' rows

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

DYNAMIC-BUFFER ENTRY-OFFS n          \ a read object's entries' offsets in OBJECT (FRESH?)
DYNAMIC-BUFFER REC-KINDS n           \ its record r's kind
DYNAMIC-BUFFER EM-RECS n             \ its emission e's record index
DYNAMIC-BUFFER PRIVS n               \ the packages holding a private wordlist: file, index
variable PRIVS-N
DYNAMIC-BUFFER GRANTS n              \ the built packages a checked piece gives a private wordlist, by record
variable GRANTS-N
DYNAMIC-BUFFER EM-OFFS n             \ its emissions' offsets in OBJECT, each at its S
DYNAMIC-BUFFER FILE-ROWS n           \ its FILES row k, from 1, as that file's BUILT row index
DYNAMIC-BUFFER USED n                \ FILES row k's mark, from 1: 0, or 1 + its BUILT row once the running piece references it
DYNAMIC-BUFFER USES n                \ the BUILT rows the pieces reference, each piece's together
variable USES-N                      \ its entries
variable USES-FROM                   \ the first entry of the piece whose BUILT row comes next
DYNAMIC-BUFFER BUILT-USES n          \ BUILT row r's entries in USES: the first at 2r, their count at 2r + 1
DYNAMIC-BUFFER DOES-AT n             \ its KIND-DOES entries' emission fields' offsets in OBJECT
DYNAMIC-BUFFER HOST-CALLS n          \ the host copy's call sites out of hb's region
variable HOST-CALLS-N
DYNAMIC-BUFFER ALIASES n             \ the linked file's aliases, each its record index, then its source's
variable ALIASES-N
DYNAMIC-BUFFER HOST-ADDRS n          \ its data address sites
variable HOST-ADDRS-N

\ The owned primitives' records the open lays (OWNED-RECORDS), gathered before MOVE-IN: per
\ prims.f package row, OWNED-CELLS cells: the row, hb's record of the primitive, then its
\ facts text's offset in OWNED-TEXT and length.
4 constant OWNED-CELLS
DYNAMIC-BUFFER OWNED n
variable OWNED-N
DYNAMIC-BUFFER OWNED-TEXT u8
variable OWNED-TEXT-N

\ As a file's compile starts (COPY-BELOW): the space's cells below its data, the records
\ [FIRST-REC - 1, FILE-REC), and NSHADOW:PATCHES.
create OLD-SPACE BUF:HDR-BYTES allot
create OLD-RECS BUF:HDR-BYTES allot
variable OLD-PATCHES
DYNAMIC-BUFFER STORED n              \ the stores' cells as distances from NULL-PTR (FIND-STORES)
variable STORED-N
DYNAMIC-BUFFER STRS n                \ the file's string bodies as distances from NULL-PTR (DATA+)
variable STRS-N
DYNAMIC-BUFFER LAST-HELD n           \ a file's record r's last held facts entry, -1 for none (FACTS+)
\ Per word as PIECE+ writes it, SPAN-CELLS cells: entry offset and length, emission or -1, text offset and
\ length (0 0 for none), highest FILES row named (TOP-ROW); EM-SPANS: per emission offset, length, row.
6 constant SPAN-CELLS
DYNAMIC-BUFFER WORD-SPANS n
DYNAMIC-BUFFER EM-SPANS n
variable WORDS-N                     \ the piece's words
variable TOP-ROW                     \ the highest FILES row REF-FILE answered since it was zeroed
DYNAMIC-BUFFER WORD-TIMES n          \ the build's word times by record index, 0 for none: nothing reads them
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
\ A built piece's stores below its own data (STORER+) and changed earlier records (CHANGER+): pair
\ p holds at 2p the cell's or record's BUILT row, -1 for header data, and at 2p + 1 the piece's row.
DYNAMIC-BUFFER STORERS n
variable STORERS-N                   \ its pairs
DYNAMIC-BUFFER WALKED n              \ row r's mark in RAN-FILES+'s walk and REACHED-CK's, PENDING row q's at BUILT-N + q
DYNAMIC-BUFFER WALK n                \ the rows the walk reached, in the order it reached them
variable WALK-N

\ ndict@, CHECK:HELD-FACTS, NSHADOW:RECORDS, NSHADOW:EMISSIONS, here's distance from NULL-PTR,
\ WIDN-CELL, the loader's EVENT-COUNT and OUTER:RUN-COUNT as a file's load or its piece after a
\ request starts (MARKS).
variable FILE-REC
variable FILE-HELD
variable FILE-TFAM                   \ TFAM:TFAM-N@ as the running piece starts
variable FILE-TYPES                  \ CHECK:TYPE-END as the running piece starts
variable FILE-ROW
variable FILE-EM
variable FILE-DP
variable FILE-WID                    \ the first wordlist id the file's compile can mint
variable FILE-EVENT                  \ the first event of the loader's log the piece made
variable FILE-RUN                    \ the first entry of OUTER's run log the piece made
variable FILE-END                    \ here's distance as the piece ends
variable FILE-CELLS                  \ the space's address-cell rows as the file's compile starts
PATH-CAP BUFFER: FILE-PATH           \ the file's path, which a refused reference names
variable FILE-PATH-N
variable FACTS-AT                    \ the offset in OBJECT of FACTS' count, of the piece placed or object read
variable ACTS-AT                    \ the offset in OBJECT of ACTS' count, of the piece checked
variable USES-AT                    \ the offset in OBJECT of USES' count, of the piece checked
\ The compiling file never links, so it compiles in every build (WRITE-OBJECT): it has no stamp,
\ a piece starts or ends off TOP-SCOPE?, or a load it asked for never reached the runner (ACTS+).
TYPED-VARIABLE UNLINKABLE bool

\ The magic, the key, the last piece's time, then per section INPUT FILES REQUESTS NAMES REACHED PATHS
\ TIMES PIECES an offset and a length (0 0 for an empty FILES to REACHED); then the sections, in order.
8 KEY-BYTES + 8 + 8 2 * 8 * + constant HEADER-BYTES
\ TIMES precedes PIECES, which test/file-objects.f compares across out roots.
HEADER-BYTES 32 - constant TIMES-FIELD      \ the header's TIMES pair
7 constant WORD-CELLS
WORD-CELLS 8 * constant WORD-BYTES

\ A record's kind in PIECES.
0 constant KIND-DEF
1 constant KIND-PACKAGE
2 constant KIND-ALIAS
3 constant KIND-DOES                 \ a `does>` definer's `;does` record
4 constant KIND-PRIVATE              \ a package gaining its private wordlist after its record
5 constant KIND-SEALED               \ an earlier file's package this file sealed
6 constant KIND-TYPE                 \ a type entry: a family's record in the types wordlist

\ NAMES maps each name the build's records define, by its NAME-SUM, to the BUILT row holding the
\ record, -1 for a record the build's body made (INDEX-RECS), and to its package's key when the
\ record is private (NAME-PKG); HITS marks the rows whose records the running file's source names
\ (NAMED).
DYNAMIC-BUFFER NAMES n               \ slot s: the sum at 3s, 0 while empty, the row, the package key
DYNAMIC-BUFFER OLD-NAMES n           \ NAMES' slots as it grows
variable NAMES-CAP                   \ its slots, a power of two
variable NAMES-N                     \ its filled slots
variable INDEXED                     \ the record index NAMES holds every record below
DYNAMIC-BUFFER OPENED n              \ the keys of the packages the running file's source opens
variable OPENED-N
DYNAMIC-BUFFER HITS n                \ BUILT row r's mark: 0, or named by the running file's source
TYPED-VARIABLE BODY-HIT bool         \ the source names a record the build's body made
\ The sums SPELLS holds since its last open, the running run: slot s holds a sum at 2s and its
\ run's id at 2s+1, and a slot of an earlier run is empty.
DYNAMIC-BUFFER RUN-SUMS n
variable RUN-CAP                     \ its slots, a power of two
variable RUN-N                       \ the running run's filled slots
variable RUN-ID                      \ the running run's id, from 1
variable RUN-AT                      \ the offset in SPELLS of the running run's first sum
\ The records of the pieces a root's check passed (PENDING), as PENDING-REC+ lays them: each piece's
\ own, then the earlier records it changes. HIT marks their PENDING-HITS rows as it marks BUILT rows.
DYNAMIC-BUFFER PENDING-RECS n
variable PENDING-RECS-N              \ its records
DYNAMIC-BUFFER PENDING-FIRST n       \ PENDING row p's first record
DYNAMIC-BUFFER PENDING-OWN n         \ PENDING row p's own record count, the records PENDING-KEY and PENDING-CHANGED+ index
DYNAMIC-BUFFER PENDING-SLOTS n       \ the records by sum: slot s holds 1 + a record, 0 while empty
variable PENDING-CAP                 \ its slots, a power of two
DYNAMIC-BUFFER PENDING-HITS n        \ PENDING row p's mark, as HITS marks a BUILT row
\ STORERS' pairs for the pending pieces: at 2i the holder, -1 for header data, a BUILT row or -2 - q
\ for PENDING row q (PENDING-HOLDER), and at 2i + 1 the storing PENDING row.
DYNAMIC-BUFFER PENDING-STORERS n
variable PENDING-STORERS-N           \ its pairs

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

\ PENDING-SLOTS with cap slots, all empty.
: PENDING-SLOTS-CLEAR ( n -- )
   {: cap:n :}
   cap PENDING-SLOTS-RESERVE
   cap 0 ?do 0 i PENDING-SLOTS ! loop
   cap PENDING-CAP ! ;

\ No pending record nor pending pair.
: PENDING-RECS-CLEAR ( -- ) 0 PENDING-RECS-N !  0 PENDING-STORERS-N !  64 PENDING-SLOTS-CLEAR ;

: DISPOSE-OBJECTS ( -- )
   WORD-SPANS-RELEASE  EM-SPANS-RELEASE  WORD-TIMES-RELEASE  TIMES BUF:DISPOSE
   WALKED-RELEASE  WALK-RELEASE  HSTORE-AT-RELEASE  HSTORE-ROW-RELEASE  STORERS-RELEASE  LISTED-RELEASE  COPY-CELLS-RELEASE  LAST-HELD-RELEASE  RETIRED-RELEASE  STRS-RELEASE  STORED-RELEASE
   ENTRY-OFFS-RELEASE  REC-KINDS-RELEASE  EM-RECS-RELEASE  PRIVS-RELEASE  GRANTS-RELEASE  EM-OFFS-RELEASE
   FILE-ROWS-RELEASE  USED-RELEASE  USES-RELEASE  BUILT-USES-RELEASE
   DOES-AT-RELEASE  HOST-CALLS-RELEASE  HOST-ADDRS-RELEASE  ALIASES-RELEASE
   BUILT-PATHS-RELEASE  LOAD-PATHS BUF:DISPOSE  KEY-PATHS BUF:DISPOSE  OBJ-PATHS BUF:DISPOSE  OBJ-NAME BUF:DISPOSE  CODE-COPY BUF:DISPOSE  OLD-RECS BUF:DISPOSE  OLD-SPACE BUF:DISPOSE
   LATE BUF:DISPOSE  PRIOR BUF:DISPOSE  OBJECT-DEPTHS 0 ?do i OBJECT-AT BUF:DISPOSE loop  REQ-PATH BUF:DISPOSE
   PENDING-PATHS BUF:DISPOSE  PENDING BUF:DISPOSE  PARKED BUF:DISPOSE  FRAMES BUF:DISPOSE  REACHED BUF:DISPOSE  RAN-ROWS BUF:DISPOSE
   SPELLS BUF:DISPOSE  NAMED-ROWS BUF:DISPOSE  PIECES BUF:DISPOSE  REQUESTS BUF:DISPOSE  REFS BUF:DISPOSE
   BUILT BUF:DISPOSE  LOADS BUF:DISPOSE  LOADERS BUF:DISPOSE  RUN-SUMS-RELEASE  HITS-RELEASE  OPENED-RELEASE
   OLD-NAMES-RELEASE  NAMES-RELEASE
   PENDING-RECS-RELEASE  PENDING-FIRST-RELEASE  PENDING-STORERS-RELEASE  PENDING-OWN-RELEASE  PENDING-SLOTS-RELEASE  PENDING-HITS-RELEASE ;

: INIT-BUFFERS ( -- )
   LOADERS 64 BUF:N>BLEN BUF:INIT
   LOADS 256 BUF:N>BLEN BUF:INIT
   BUILT 256 BUF:N>BLEN BUF:INIT
   REFS 256 BUF:N>BLEN BUF:INIT
   REQUESTS 256 BUF:N>BLEN BUF:INIT
   PIECES 256 BUF:N>BLEN BUF:INIT
   TIMES 256 BUF:N>BLEN BUF:INIT
   NAMED-ROWS 256 BUF:N>BLEN BUF:INIT
   SPELLS 256 BUF:N>BLEN BUF:INIT
   RAN-ROWS 256 BUF:N>BLEN BUF:INIT
   REACHED 256 BUF:N>BLEN BUF:INIT
   FRAMES 256 BUF:N>BLEN BUF:INIT
   PARKED 256 BUF:N>BLEN BUF:INIT
   PENDING 256 BUF:N>BLEN BUF:INIT
   PENDING-PATHS 256 BUF:N>BLEN BUF:INIT
   REQ-PATH 256 BUF:N>BLEN BUF:INIT
   OBJECT-DEPTHS 0 ?do i OBJECT-AT 256 BUF:N>BLEN BUF:INIT loop  0 OBJECT-DEPTH !
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
   0 USES-N !  0 USES-FROM !
   1024 NAMES-CLEAR  1024 RUN-CLEAR  FIRST-REC @ INDEXED !  PENDING-RECS-CLEAR ;

\ All or none: disposing a header never set up, or disposed, is a no-op.
: INIT-OBJECTS ( -- )
   0 ENTRY-N !  0 ENTRY-CAP !
   ['] INIT-BUFFERS catch {: rc:n :}
   rc 0<> if DISPOSE-OBJECTS rc throw then ;

: BYTES$ ( ptr a -- ptr u8 n ) BUF:SPAN$ BUF:BLEN>N ;

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

\ FILE-KEY's object: the build's target, WITH's mode, the file's path, from the path's second load
\ a dot and the load's number from 2, then .hfo (BUILD-OUT:OBJ$). A source named a.f.2 shares the
\ name of a.f's second load: each fails the other's object at HEADER-CK's key and compiles.
: OBJECT$ ( -- ptr u8 n )
   OBJ-NAME BUF:CLEAR
   TARGET @ RTARGET:PROFILE$ OBJ-NAME SPAN+  s" /" OBJ-NAME SPAN+
   ENGINE? @ if s" engine" else KERNEL? @ if s" kernel" else s" app" then then OBJ-NAME SPAN+
   FILE-KEY CANON$ {: p:ptr pu:n :}
   pu 0 >  if p c@ FS:SLASH = else false then 0= if s" /" OBJ-NAME SPAN+ then
   p pu OBJ-NAME SPAN+
   FILE-KEY 8 + LE:U64@ {: n:n :}
   n 0 > if  s" ." OBJ-NAME SPAN+  n 1 + FMT:INT$ OBJ-NAME SPAN+  then
   s" .hfo" OBJ-NAME SPAN+
   OBJ-NAME BYTES$ BUILD-OUT:OBJ$ ;

\ The object path's directory made, so the object can be written there.
: OBJECT-DIR ( ptr u8 n -- )
   {: p:ptr pu:n :}
   pu begin dup 0 > if p over 1 - + c@ FS:SLASH <> else false then while 1 - repeat
   1 - {: du:n :}
   p du FS:MAKE-DIRS ;

\ ---- the records ---------------------------------------------------------------
\ No address reaches PIECES: a record is named by file and index, code and data by
\ span and offset, so larger earlier files leave its bytes alone (test/file-objects.f).

: BUILT-N ( -- n ) BUILT BYTES$ {: a:ptr u:n :}  u BUILT-BYTES / ;

: BUILT-AT ( n -- ptr u8 )
   {: row:n :}
   BUILT BYTES$ {: a:ptr u:n :}  a row BUILT-BYTES * + ;

: HOLDS? ( n n -- bool )
   {: idx:n row:n :}
   row BUILT-AT BUILT-FIRST + {: at:ptr :}
   at LE:U64@ idx <=  at 8 + LE:U64@ idx >  and ;

\ The BUILT row whose [first, end) holds the record index.
: BUILT-ROW ( n -- n )
   {: idx:n :}
   BUILT-N 0 ?do
      idx i HOLDS? if i unloop exit then
   loop
   E-COMP-STATE throw ;

\ The BUILT row whose [first, end) emission holds the emission number.
: EM-ROW ( n -- n )
   {: e:n :}
   BUILT-N 0 ?do
      i BUILT-AT BUILT-EM + {: at:ptr :}
      at LE:U64@ e <=  at 8 + LE:U64@ e >  and if i unloop exit then
   loop
   E-COMP-STATE throw ;

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

\ The FILES row naming the BUILT row's piece by its file's key, its number and its time,
\ 1-based; added on first use.
: REF-FILE ( n -- n )
   {: row:n :}
   row BUILT-AT {: at:ptr :}
   REFS BYTES$ {: refs:ptr refsu:n :}
   refsu FILES-ROW-BYTES / {: n:n :}
   n 0 ?do
      refs i FILES-ROW-BYTES * +  FILES-ROW-BYTES  at FILES-ROW-BYTES  STR= if
         i 2 + USED-RESERVE  row 1 +  i 1 + USED !
         i 1 + TOPPED unloop exit
      then
   loop
   at FILES-ROW-BYTES REFS SPAN+
   n 2 + USED-RESERVE  row 1 +  n 1 + USED !
   n 1 + TOPPED ;

\ A record reference: the file, 0 for this one, then the index in that file.
: REF+ ( n -- )
   {: idx:n :}
   idx FILE-REC @ >= if
      0 PIECES U64+  idx FILE-REC @ - PIECES U64+  exit
   then
   idx BUILT-ROW {: row:n :}
   row REF-FILE PIECES U64+
   idx  row BUILT-AT BUILT-FIRST + LE:U64@ -  PIECES U64+ ;

\ ---- dependencies by name -------------------------------------------------------
\ A file depends on the files its object references and on every earlier piece that defines, or
\ changed (CHANGER+), a public name its source spells or a private name of a package it opens
\ after `package`, whose parts share scope: a value T0 inlined, a word that now shadows another
\ and a package an earlier file now declares change a compile without leaving a reference.

\ FNV-1a over the name folded to upper case, as the dictionary compares names; never 0.
: NAME-SUM ( ptr u8 n -- n )
   {: a:ptr u:n :}
   $811C9DC5  u 0 ?do
      a i + c@ {: c:n :}
      c 97 >=  c 122 <=  and if c 32 - else c then  xor  $1000193 *
   loop
   dup 0= if 1 + then ;

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

\ The key of the package whose private wordlist holds the record, its top-level name's
\ NAME-SUM; 0 for a record any file may name.
: NAME-PKG ( n -- n )
   {: idx:n :}
   idx XREF-REC XREF-WORDLIST {: wid:n :}
   wid CHECK:WORDLIST-PACKAGE {: p:n hit:bool :}
   hit 0= if 0 exit then
   p XREF-REC XREF-PKG-PRIVATE wid <> if 0 exit then
   p XREF-REC XREF-NAME$ TOP-NAME NAME-SUM ;

\ The records [INDEXED, end) into NAMES as the row's.
: INDEX-RECS ( n n -- )
   {: end:n row:n :}
   end INDEXED @ ?do
      i XREF-REC XREF-NAME$ {: a:ptr u:n :}
      u 0<> if a u NAME-SUM row i NAME-PKG NAME+ then
   loop
   end INDEXED ! ;

: MARK-ROW ( n -- )
   {: row:n :}
   row 0< if true BODY-HIT ! else 1 row HITS ! then ;

\ True for a public name's key, 0, and for a package the running file's source opened.
: OPENED? ( n -- bool )
   {: pkg:n :}
   pkg 0= if true exit then
   OPENED-N @ 0 ?do i OPENED @ pkg = if true unloop exit then loop
   false ;

: OPENED+ ( n -- )
   OPENED-N @ 1 + OPENED-RESERVE
   OPENED-N @ OPENED !  1 OPENED-N +! ;

\ Field f of pending record r (PENDING-RECS).
: PENDING-REC@ ( n n -- n )
   {: r:n f:n :}
   r 4 * f + PENDING-RECS @ ;

: PENDING-N ( -- n ) PENDING BYTES$ nip FILES-ROW-BYTES / ;

\ PENDING-HIT's slot s: its record's PENDING row marked when the record has the sum and an OPENED? key.
: PENDING-SLOT-HIT ( n n -- )
   {: s:n sum:n :}
   s PENDING-SLOTS @ 1 - {: r:n :}
   r 0 PENDING-REC@ sum = if
      r 1 PENDING-REC@ OPENED? if  1  r 3 PENDING-REC@ PENDING-HITS !  then
   then ;

\ Marks every PENDING row a pending record with the sum is in, as HIT marks a BUILT row. Half
\ PENDING-SLOTS' slots at most are filled (PENDING-REC+), so a probe always ends.
: PENDING-HIT ( n -- )
   {: sum:n :}
   PENDING-CAP @ 1 - {: mask:n :}
   sum mask and
   begin dup PENDING-SLOTS @ 0<> while
      dup sum PENDING-SLOT-HIT
      1 + mask and
   repeat drop ;

\ Marks every row NAMES holds the sum for, a private name's only once its package is open, then
\ each pending row holding a record of the sum, by the same rule (PENDING-HIT).
: HIT ( n -- )
   {: sum:n :}
   NAMES-CAP @ 1 - {: mask:n :}
   sum mask and
   begin dup SLOT-SUM @ 0<> while
      dup SLOT-SUM @ sum = if
         dup SLOT-PKG @ OPENED? if dup SLOT-ROW @ MARK-ROW then
      then
      1 + mask and
   repeat drop
   sum PENDING-HIT ;

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

\ SPELLED's HIT: SPELLS gains the sum once per run, as a repeat with no open between marks
\ nothing more. Half RUN-SUMS' slots at most are filled, so a probe always ends.
: SPELL-HIT ( n -- )
   {: sum:n :}
   RUN-N @ 1 + 2 *  RUN-CAP @ > if GROW-RUN then
   sum RUN-SUM+ if  sum SPELLS U64+  sum HIT  then ;

\ SPELLED's OPENED+: SPELLS gains 0, which no NAME-SUM is, then the sum, and a run starts.
: SPELL-OPEN ( n -- )
   {: sum:n :}
   0 SPELLS U64+  sum SPELLS U64+  sum OPENED+  RUN! ;

\ A byte that ends a spelled name: white space, or ':' and '"', which join a package to its
\ word and a string's quote to its first word.
: BLANK? ( n -- bool ) {: c:n :}  c 32 =  c 9 = or  c 10 = or  c 13 = or ;
: CUT? ( n -- bool ) {: c:n :}  c BLANK?  c 58 = or  c 34 = or ;

variable PART-AT                     \ SPELLED's running part's first byte
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
   JOINED @ if src WORD-AT @ i HIT-SPAN then
   i WORD-AT @ > if src WORD-AT @ i OPENS  false FIRST-WORD ! then
   c 10 = if true FIRST-WORD ! then
   i 1 + PART-AT !  i 1 + WORD-AT !  false JOINED ! ;

\ SPELL-HIT for each name the source spells outside its comment lines: each span between CUT? bytes,
\ and each between white space that holds a ':' or '"', so `P:W` names P, W and P:W.
: SPELLED ( ptr u8 n -- )
   {: src:ptr u:n :}
   0 PART-AT !  0 WORD-AT !  false JOINED !  true FIRST-WORD !  false SKIP !
   false AFTER-PACKAGE !  0 OPENED-N !  SPELLS BUF:CLEAR  RUN!
   u 0 ?do
      src i + c@ {: c:n :}
      SKIP @ if
         c 10 = if false SKIP !  true FIRST-WORD !  i 1 + PART-AT !  i 1 + WORD-AT ! then
      else
         c BLANK? if src i c WORD-END else
            c CUT? if src PART-AT @ i HIT-SPAN  i 1 + PART-AT !  true JOINED ! then
         then
      then
   loop
   SKIP @ 0= if src u 32 WORD-END then ;

\ No row marked, BUILT's or PENDING's, nor the build's body.
: HITS-CLEAR ( -- )
   BUILT-N 1 + HITS-RESERVE
   BUILT-N 0 ?do 0 i HITS ! loop
   PENDING-N 0 ?do 0 i PENDING-HITS ! loop
   false BODY-HIT ! ;

\ After KEY!, before FRESH?: INPUT-STAMP, the active file's change time seconds, nanoseconds and
\ size, and HITS-CLEAR, NAMES first taking the records the build's body made since the last file.
: INPUT! ( -- )
   SOURCE-ROOT:ACTIVE-STAMP {: sec:n ns:n size:n :}
   sec INPUT-STAMP LE:U64!  ns INPUT-STAMP 8 + LE:U64!  size INPUT-STAMP 16 + LE:U64!
   FILE-REC @ -1 INDEX-RECS
   HITS-CLEAR ;

\ False for bytes no file stamp covers (size -1): such a file never links.
: STAMPED? ( -- bool ) INPUT-STAMP 16 + LE:U64@ -1 <> ;

\ After SPELLED, before the compile: NAMED-ROWS from HITS, which a requested file's load overwrites.
: NAMED! ( -- )
   NAMED-ROWS BUF:CLEAR
   BUILT-N 0 ?do i HITS @ 0<> if i NAMED-ROWS U64+ then loop ;

\ FILES gains each earlier file the source names, after the files PIECES and REQUESTS reference.
: NAMED+ ( -- )
   NAMED-ROWS BYTES$ {: a:ptr u:n :}
   u 8 / 0 ?do a i 8 * + LE:U64@ REF-FILE drop loop ;


\ ---- dependencies by run --------------------------------------------------------
\ A file that runs an earlier piece's word while it compiles keeps the word's answer, so it
\ depends on every piece that word reaches: the rows the piece references (BUILT-USES) and the
\ pieces that stored into its data or the header data or changed its records (STORERS).

: RAN-KNOWN? ( n -- bool )
   {: idx:n :}
   RAN-ROWS BYTES$ {: a:ptr u:n :}
   u 8 / 0 ?do  idx  a i 8 * + LE:U64@  HOLDS? if true unloop exit then  loop
   false ;

\ The BUILT row that holds the record index, and true; false when none.
: RAN-HOLDER ( n -- n bool )
   {: idx:n :}
   BUILT-N 0 ?do  idx i HOLDS? if i true unloop exit then  loop
   0 false ;

: RAN-ROW+ ( n -- )
   {: row:n :}
   RAN-ROWS BYTES$ {: a:ptr u:n :}
   u 8 / 0 ?do  a i 8 * + LE:U64@ row = if unloop exit then  loop
   row RAN-ROWS U64+ ;


\ A run log entry's record index: true for the running piece's own word; else its earlier piece's
\ BUILT row onto RAN-ROWS. hb's records and the build body's have no row.
: RAN-RECORD ( n -- bool )
   {: idx:n :}
   idx FIRST-REC @ < if false exit then
   idx FILE-REC @ >= if true exit then
   idx RAN-KNOWN? if false exit then
   idx RAN-HOLDER if RAN-ROW+ else drop then
   false ;


\ After PIECE+: the rows of the words the piece ran onto RAN-ROWS. A word of its own reaches what
\ the piece references, its USES entries from USES-FROM.
: RAN+ ( -- )
   false  OUTER:RUN-COUNT FILE-RUN @ ?do  i OUTER:RUN-RECORD@ RAN-RECORD or  loop
   if  USES-N @ USES-FROM @ ?do i USES @ RAN-ROW+ loop  then ;


\ The running piece, at BUILT-N, has no row yet.
: WALK+ ( n -- )
   {: r:n :}
   r BUILT-N >= if exit then
   r WALKED @ 0<> if exit then
   1 r WALKED !
   WALK-N @ 1 + WALK-RESERVE
   r WALK-N @ WALK !
   1 WALK-N +! ;

: ROW-USES+ ( n -- )
   {: r:n :}
   r 2 * BUILT-USES @ {: first:n :}
   first  r 2 * 1 + BUILT-USES @ +  first ?do i USES @ WALK+ loop ;

\ WALK's entries from k on, each giving WALK+ the rows its piece references; WALK-N once every
\ entry is visited.
: WALK-USES ( n -- n )
   begin dup WALK-N @ < while  dup WALK @ ROW-USES+  1 +  repeat ;

\ The piece of each STORERS pair whose holder is the header data or a walked row.
: WALK-STORERS ( -- )
   STORERS-N @ 0 ?do
      i 2 * STORERS @ {: held:n :}
      held -1 = if true else held WALKED @ 0<> then if  i 2 * 1 + STORERS @ WALK+  then
   loop ;


\ FILES gains each piece the rows of RAN-ROWS reach, in BUILT order, after NAMED+'s; REACHED takes
\ each one's FILES row.
: RAN-FILES+ ( -- )
   REACHED BUF:CLEAR
   RAN-ROWS BYTES$ nip 0= if exit then
   BUILT-N 1 + WALKED-RESERVE
   BUILT-N 1 + 0 ?do 0 i WALKED ! loop
   0 WALK-N !
   RAN-ROWS BYTES$ nip 8 / 0 ?do  RAN-ROWS BYTES$ drop i 8 * + LE:U64@ WALK+  loop
   0 begin  WALK-USES  WALK-STORERS  dup WALK-N @ =  until drop
   BUILT-N 0 ?do  i WALKED @ 0<> if i REF-FILE REACHED U64+ then  loop ;


\ A data address's reference, and true: a file's data, or the open's header data; false,
\ writing nothing, for any other address. A file's end names it only after every span, so
\ a later file's first byte names that later file.
: DATA-REF+ ( n -- bool )
   {: v:n :}
   v FILE-DP @ >=  v FILE-END @ <  and if
      3 PIECES U64+  0 PIECES U64+  v FILE-DP @ - PIECES U64+ true exit
   then
   v DP-ROW {: row:n hit:bool :}
   hit if
      3 PIECES U64+  row REF-FILE PIECES U64+
      v  row BUILT-AT BUILT-DP + LE:U64@ -  PIECES U64+ true exit
   then
   v BASE @ >=  v OPEN-DP @ <  and if 4 PIECES U64+  v BASE @ - PIECES U64+ true exit then
   v FILE-END @ = if
      3 PIECES U64+  0 PIECES U64+  v FILE-DP @ - PIECES U64+ true exit
   then
   v DP-END-ROW {: end-row:n end-hit:bool :}
   end-hit if
      3 PIECES U64+  end-row REF-FILE PIECES U64+
      v  end-row BUILT-AT BUILT-DP + LE:U64@ -  PIECES U64+ true exit
   then
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


\ The highest value at most at among emission e's data address chains, -1 for none.
: EM-BELOW ( n n -- n )
   {: e:n at:n :}
   e NSHADOW:BYTES {: src:ptr :}
   e NSHADOW:SIZE {: s:n :}
   -1  e NSHADOW:ADDR-SITES 0 ?do
      e i NSHADOW:ADDR-SITE-KIND@ NEMIT:ADDR-DATA = if
         e i NSHADOW:ADDR-SITE@ {: c:n :}
         src c +  src s +  ADDRESS-CARRIER:CHAIN-SIZE {: size:n :}
         size 0<> if
            src c +  size ADDRESS-CARRIER:CHAIN-VALUE {: w:n :}
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
         i CHECK:HELD-RETIRED@ {: idx:n wid:n :}
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

\ The wordlist's role: 0 global, 1 primitives, 2 package names, 3 or 4 a package's
\ public or private one, then the package, which is the build's and below the record; 5 types.
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
   pkg REF+ ;

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

\ A definition's or does record's emission, numbered from the file's first.
: EMISSION+ ( n -- ) NSHADOW:EMISSION@ FILE-EM @ - PIECES U64+ ;

\ The record's dispatch cell, a defer's trailer cell, as a data reference; 5 for none.
: DISPATCH+ ( ptr n -- )
   {: rec:ptr :}
   rec NDICT:TRAILER-CELL {: cell:n :}
   cell 0= if 5 PIECES U64+ exit then
   cell DATA-REF+ if exit then
   REFUSAL  rec XREF-NAME$ SAY  s" 's dispatch cell is " SAY  cell false VALUE.  REFUSED ;

\ The kind, the name, the flags without the name length and EXT, the wordlist (WORDLIST+), then
\ the kind's fields (DISPATCH+, OWN-PRIVATE?, none for KIND-TYPE); a KIND-PRIVATE entry is
\ LATE+'s, a KIND-SEALED one SEAL+'s.
: RECORD+ ( n -- )
   {: idx:n :}
   idx XREF-REC {: rec:ptr :}
   idx SHADOW-ROW {: row:n :}
   idx row KIND {: kind:n :}
   kind PIECES U64+
   rec XREF-NAME$ PIECES FIELD+
   rec XREF-FLAGS  DNAME-LEN-MASK DNAME-EXT or invert and  PIECES U64+
   idx WORDLIST+
   kind KIND-DEF = if row EMISSION+  rec DISPATCH+ exit then
   kind KIND-DOES = if row EMISSION+  row NSHADOW:ENTRY@ PIECES U64+ exit then
   kind KIND-PACKAGE = if
      idx OWN-PRIVATE? if 1 else 0 then PIECES U64+ exit
   then
   kind KIND-TYPE = if exit then
   idx ALIAS-SOURCE REF+ ;

\ Word w's cell c in WORD-SPANS.
: WORD-SPAN ( n n -- ptr n )
   {: w:n c:n :}
   w SPAN-CELLS * c + WORD-SPANS ;

\ RECORD+, then the entry's span, its emission and the highest FILES row it names into WORD-SPANS,
\ with no text yet. The piece filed the emission an entry names: E-COMP-STATE for another.
: WORD-RECORD+ ( n -- )
   {: idx:n :}
   idx FILE-REC @ - {: w:n :}
   w 1 + SPAN-CELLS * WORD-SPANS-RESERVE
   PIECES BYTES$ nip {: at:n :}
   0 TOP-ROW !
   idx RECORD+
   idx SHADOW-ROW {: row:n :}
   idx row KIND {: kind:n :}
   kind KIND-DEF =  kind KIND-DOES =  or {: coded:bool :}
   coded if row NSHADOW:EMISSION@ FILE-EM @ - else -1 then {: em:n :}
   coded  em 0<  em NSHADOW:EMISSIONS FILE-EM @ - >=  or  and if E-COMP-STATE throw then
   at  w 0 WORD-SPAN !
   PIECES BYTES$ nip at -  w 1 WORD-SPAN !
   em  w 2 WORD-SPAN !
   0 w 3 WORD-SPAN !  0 w 4 WORD-SPAN !
   TOP-ROW @  w 5 WORD-SPAN ! ;

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

: LATE+ ( n -- ) KIND-PRIVATE PIECES U64+  LATE-P REF+ ;

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

\ A code address's reference, and true: a record's entry, a quotation's function, or hb's
\ own code, last since a record's host copy may lie inside hb's code range; false, writing
\ nothing, for any other address.
: CODE-REF+ ( n -- bool )
   {: a:n :}
   a REC-AT {: r:n :}
   r 0 >= if 0 PIECES U64+  r NSHADOW:RECORD@ REF+ true exit then
   a NSHADOW:SOURCE-FUNCTION {: qe:n qf:n :}
   qe 0 >= if
      2 PIECES U64+
      qe FILE-EM @ >= if
         0 PIECES U64+  qe FILE-EM @ - PIECES U64+
      else
         qe EM-ROW {: row:n :}
         row REF-FILE PIECES U64+
         qe  row BUILT-AT BUILT-EM + LE:U64@ -  PIECES U64+
      then
      qf PIECES U64+ true exit
   then
   a ARM64:HOST-KERNEL? if 1 PIECES U64+  a rbase - PIECES U64+ true exit then
   false ;


\ One emission of CODE+; chain values are read from the shadow's bytes:
\ PIECES' copy has them cleared.
: EMISSION-CODE+ ( n -- )
   {: e:n :}
   e NSHADOW:SIZE {: s:n :}
   e NSHADOW:BYTES {: src:ptr :}
   e NSHADOW:PATCH-SITE@ {: p:n :}
   e NSHADOW:CALL-SITES {: calls:n :}
   s PIECES U64+  e NSHADOW:RET-BYTES PIECES U64+
   src s PIECES SPAN+
   PIECES BYTES$ {: buf:ptr bufu:n :}
   bufu s - {: off:n :}
   calls 0 ?do 0  buf off + e i NSHADOW:CALL-SITE@ +  LE:U32! loop
   p 0 >= if 0  buf off + p +  LE:U32! then
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
         t CODE-REF+ 0= if e  e i NSHADOW:CALL-SITE@  t true REFUSE-SITE then
      then
   loop
   e NSHADOW:ADDR-SITES PIECES U64+
   e NSHADOW:ADDR-SITES 0 ?do
      e i NSHADOW:ADDR-SITE@ {: at:n :}
      e i NSHADOW:ADDR-SITE-KIND@ {: kind:n :}
      kind NEMIT:ADDR-CODE <>  kind NEMIT:ADDR-DATA <>  and if E-COMP-STATE throw then
      kind NEMIT:ADDR-CODE = if
         ADDRESS-CARRIER:ADDR-CHAIN-BYTES
      else
         src at +  src s +  ADDRESS-CARRIER:CHAIN-SIZE
      then {: size:n :}
      size 0= if E-COMP-STATE throw then
      src at +  size ADDRESS-CARRIER:CHAIN-VALUE {: v:n :}
      PIECES BYTES$ drop off + at +  0 size ADDRESS-CARRIER:SET-CHAIN-VALUE
      at PIECES U64+
      kind NEMIT:ADDR-CODE = {: code:bool :}
      code if
         0 PIECES U64+  v CODE-REF+
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
         t CODE-REF+ 0= if e p t true REFUSE-SITE then
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
   e EMISSION-CODE+
   at  at3 EM-SPANS !
   PIECES BYTES$ nip at -  at3 1 + EM-SPANS !
   TOP-ROW @  at3 2 + EM-SPANS ! ;


\ CODE, after the piece's entries, then DATA and STRINGS (DATA+); every number is 8 bytes, low
\ byte first:
\   E                      emissions this file's compile filed, from the file's first
\   per emission, in number order:
\     S, R                 size in bytes; return bytes (R <= S)
\     S raw bytes          unpadded; each call site's 4 bytes, the does> slot's 4 bytes
\                          and every 16-bit immediate of each address chain written as 0
\     F, then F offsets    function starts
\     C, then C call rows  site, kind (0 call, 1 tail), reference; no does> slot row
\     A, then A addr rows  site, kind (0 code, 1 data), reference
\     P                    the does> slot's site, -1 for none; when P >= 0 its target's
\                          reference
\ A reference is a tag, then its fields: 0 a dictionary record (file, index in that
\ file); 1 hb's own code (offset from rbase); 2 a quotation (file, emission, function);
\ 3 a file's data (file, offset from that file's first data byte, at most its data size:
\ a body of no bytes at the file's end); 4 the open's header data (offset from BASE); 5 none: a
\ patch target, a cell row's 0 or scratch cell, or a definition with no dispatch cell. A file
\ field is 0 for this object, else its FILES row from 1 (REF-FILE). A cross build's E is 0: its
\ shadow holds the target's code, in the target's site encodings, and it writes no object (LOAD).
: CODE+ ( -- )
   ENTRIES
   CROSS? if 0 PIECES U64+ exit then
   NSHADOW:EMISSIONS FILE-EM @ - PIECES U64+
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

: STRING+ ( n -- )
   {: b:n :}
   b CELL - FILE-DP @ <  b FILE-END @ >  or if exit then
   b b BODY-U +  FILE-END @ > if exit then
   STRS-N @ 1 + STRS-RESERVE
   b STRS-N @ STRS !  1 STRS-N +! ;

\ DATA, STRINGS, STORES and FACTS, after CODE:
\   D                      the bytes the file's compile laid, FILE-END - FILE-DP
\   D raw bytes            from FILE-DP, unpadded; each address cell's 8 bytes written as 0
\   K, then K cell rows    offset from FILE-DP, a multiple of 8 (else E-COMP-STATE), kind
\                          (ROW-KIND), reference (CELL-REF+); in the order of the space's
\                          address-cell rows, only cells inside the D bytes; a cell at or
\                          above FILE-DP that ends past them is E-COMP-STATE
\   N, then N string rows  a body's offset from FILE-DP, then its length; offsets ascending,
\                          only bodies whose length cell and bytes lie inside the D bytes
\   S, then S store rows   each cell below FILE-DP the compile stored into, other than a scratch
\                          cell, or gave an address-cell row, ascending by address: its
\                          reference (tag 3 with a FILES row, or 4), kind (ROW-KIND, 3 for
\                          none), bytes (the cell's value for kind 3, else 0), its value's
\                          reference (CELL-REF+ for kinds 0-2, else 5)
\   F, then F facts rows   each entry the file's compile held, in order (FACTS+): CHECK:HELD-RETIRED
\                          then the retired record's reference (REF+); CHECK:HELD-AMENDED, an
\                          earlier file's record's reference and the text a `TRUST` row or an
\                          amender wrote again for it; else the record's index from FILE-REC, or CHECK:HELD-LAID
\                          for a laid text or CHECK:HELD-UNDEFINE for an `undefine`, then the
\                          text as a FIELD+
: DATA+ ( -- )
   FILE-DATA {: a:ptr d:n :}
   d PIECES U64+  a d PIECES SPAN+
   PIECES BYTES$ {: buf:ptr bufu:n :}
   buf bufu + d - {: at:ptr :}
   ROWS {: rows:ptr count:n :}
   0  count 0 ?do
      rows i cells + @ {: row:n :}
      row IN-FILE? if
         row ROW-AT FILE-DP @ - {: off:n :}
         off 7 and 0<> if E-COMP-STATE throw then
         0  at off +  LE:U64!  1 +
      else
         row ROW-AT FILE-DP @ >= if E-COMP-STATE throw then
      then
   loop  PIECES U64+
   count 0 ?do
      rows i cells + @ {: row:n :}
      row IN-FILE? if
         row ROW-AT FILE-DP @ - PIECES U64+  row ROW-KIND PIECES U64+  row CELL-REF+
      then
   loop
   0 STRS-N !  ['] STRING+ INTERN:EACH-STRING
   STRS-N @ {: n:n :}
   n 0 > if 0 STRS n [: < ;] SORT:SORT! then
   n PIECES U64+
   n 0 ?do
      i STRS @ {: b:n :}
      b FILE-DP @ - PIECES U64+  b BODY-U PIECES U64+
   loop ;

\ ---- the stores ----------------------------------------------------------------
\ A file's compile may store into cells below its data, but must leave earlier records and
\ emissions alone: E-STORE, which fails the build as any throw out of a load does (RUN-FILE).

\ The space's cells a store can reach, as [from, to) offsets from BASE: [0, FILE-DP) less
\ INDEX-CELL and the address-cell header and boot rows, which EMIT-MARK writes.
: EACH-RANGE ( [ n n -- ] -- )
   {: q :}
   0 ADDRESS-CELLS:INDEX-CELL q execute
   ADDRESS-CELLS:INDEX-CELL CELL +  SNAP-RELOC:XTCELL-N-CELL q execute
   SNAP-RELOC:XTCELL-END  FILE-DP @ BASE @ -  q execute ;


: SPACE-BYTES ( n -- ptr u8 ) NULL-PTR BYTE-VIEW BASE @ + swap + ;
: COPY-REC ( n -- ) XREF-REC BYTE-VIEW DREC OLD-RECS SPAN+ ;


: COPY-RANGE ( n n -- )
   {: from:n to:n :}
   from SPACE-BYTES  to from -  OLD-SPACE SPAN+ ;


\ What the file's compile may change below its own data, as it starts.
: COPY-BELOW ( -- )
   OLD-SPACE BUF:CLEAR  OLD-RECS BUF:CLEAR
   [: COPY-RANGE ;] EACH-RANGE
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


variable OLD-AT                      \ COMPARE-RANGE's offset in OLD-SPACE

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

\ The running piece changed record idx of an earlier piece (its facts, a retirement, a seal, a
\ late private wordlist): NAMES takes the record's name as the piece's, so a file that spells it
\ depends on the piece, and STORERS the piece against the record's row. Nothing for its own.
: CHANGER+ ( n -- )
   {: idx:n :}
   idx FILE-REC @ >= if exit then
   idx XREF-REC XREF-NAME$ {: a:ptr u:n :}
   u 0<> if  a u NAME-SUM  BUILT-N  idx NAME-PKG  NAME+  then
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


\ Each cell of [from, to) that differs from the copy at OLD-AT, into STORED, other than a
\ scratch cell.
: COMPARE-RANGE ( n n -- )
   {: from:n to:n :}
   OLD-SPACE BYTES$ drop OLD-AT @ + {: old:ptr :}
   from SPACE-BYTES {: now:ptr :}
   to from - CELL / 0 ?do
      old i cells + CELL-VIEW @  now i cells + CELL-VIEW @  <> if
         BASE @ from + i cells + dup SCRATCH-CELL? if drop else STORED+ then
      then
   loop
   to from - OLD-AT +! ;


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


\ STORED, ascending: each changed cell below FILE-DP but a scratch one, and each new row's cell
\ there, aligned (else E-COMP-STATE, as in DATA+); E-STORE for a cell no earlier file or the
\ header holds.
: FIND-STORES ( -- )
   0 STORED-N !  0 OLD-AT !
   [: COMPARE-RANGE ;] EACH-RANGE
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


\ STORES, after STRINGS (DATA+).
: STORES+ ( -- )
   STORED-N @ PIECES U64+
   STORED-N @ 0 ?do
      i STORED @ {: v:n :}
      v DATA-REF+ 0= if E-COMP-STATE throw then   \ FIND-STORES refused a cell nothing holds
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


\ A facts row: a retirement's kind and record, an earlier file's record's text by reference, any
\ other kind's or record's text.
: FACT+ ( n -- )
   {: k:n :}
   k CHECK:HELD-FACT@ {: ix:n a:ptr u:n :}
   ix CHECK:HELD-RETIRED = if
      ix PIECES U64+  k CHECK:HELD-RETIRED@ drop REF+  exit
   then
   ix 0 >=  ix FILE-REC @ <  and if
      CHECK:HELD-AMENDED PIECES U64+  ix REF+  a u PIECES FIELD+  exit
   then
   ix 0< if ix else ix FILE-REC @ - then PIECES U64+
   a u PIECES FIELD+ ;


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
   KIND-SEALED PIECES U64+  idx REF+  idx CHANGER+ ;


\ An event of the loader's log an ACTS row carries: a require the registry already held, or a
\ provide.
: ACT? ( n -- bool )
   {: ev:n :}
   ev EVENT-KIND@ EV-PROVIDED =
   ev EVENT-KIND@ EV-REQUIRED =  ev EVENT-STATE@ EV-STATE-KNOWN =  and  or ;

\ ACTS, the registry acts of the piece ending with REQUESTS row req, or of the last (-1), from the
\ loader's log (include.f EVENT-RECORD): their count, then each its kind, EV-REQUIRED or
\ EV-PROVIDED, and the path as spelled (FIELD+). The newest event of a piece a request ends is that
\ request's own. Any other event is a load the source caught before the build's runner ran it,
\ which a link could not repeat: the file is UNLINKABLE. E-COMP-STATE when the log stopped.
: ACTS+ ( n -- )
   {: req:n :}
   EVENT-COUNT  req 0 >= if 1 - then {: end:n :}
   EVENT-ON? 0=  end FILE-EVENT @ <  or if E-COMP-STATE throw then
   0  end FILE-EVENT @ ?do i ACT? if 1 + then loop  PIECES U64+
   end FILE-EVENT @ ?do
      i ACT? if
         i EVENT-KIND@ PIECES U64+  i EVENT-SPELLED@ PIECES FIELD+
      else
         true UNLINKABLE !
      then
   loop ;


\ n, a BUILT row index, onto USES.
: USE+ ( n -- )
   USES-N @ 1 + USES-RESERVE
   USES-N @ USES !
   1 USES-N +! ;


\ USES, the FILES rows of 1..n marked in USED, which a request's own row and a file the source only
\ names are not: their count, then each row, ascending; USE+ takes each one's BUILT row.
: USES+ ( -- )
   REFS BYTES$ nip FILES-ROW-BYTES / {: n:n :}
   0  n 0 ?do i 1 + USED @ 0<> if 1 + then loop  PIECES U64+
   n 0 ?do
      i 1 + USED @ {: mark:n :}
      mark 0<> if  i 1 + PIECES U64+  mark 1 - USE+  then
   loop ;


\ The running piece into PIECES: entry count; records by index, each late private wordlist (by id)
\ before the first record marked above its id, so ids mint in entry order; each earlier package the
\ piece sealed, after every record, so a link seals it once the piece's definitions are in; CODE;
\ DATA; STRINGS; STORES; FACTS, whose offset in PIECES it leaves in FACTS-AT; ACTS; USES; then
\ the REQUESTS row that ends the piece, from 0, or -1 after the file's last piece.
: PIECE+ ( n -- )
   {: req:n :}
   REFS BYTES$ nip FILES-ROW-BYTES / {: rows:n :}
   rows 1 + USED-RESERVE  rows 0 ?do 0 i 1 + USED ! loop
   LATE BUF:CLEAR
   FILE-ROW @ SHADOW-AT !
   LATE-COLLECT
   ndict@ FILE-REC @ -  LATE-N +  SEALED-N +  PIECES U64+
   ndict@ FILE-REC @ - WORDS-N !
   0  ndict@ FILE-REC @ ?do  i RECORD-MARK LATE-BELOW  i WORD-RECORD+  loop
   LATE-REST
   LATE-N 0 ?do i LATE-P CHANGER+ loop
   OLD-RECS-N 0 ?do i OLD-REC SEAL+ loop
   CODE+  DATA+  STORES+
   PIECES BYTES$ nip FACTS-AT !  FACTS+
   req ACTS+
   USES+
   req PIECES U64+ ;

\ As the file's compile starts: no FILES, REQUESTS, RAN-ROWS, TIMES or piece yet, and PIECES' piece
\ count 0 until WRITE-OBJECT.
: OBJECT-OPEN ( -- )
   REFS BUF:CLEAR  REQUESTS BUF:CLEAR  PIECES BUF:CLEAR  RAN-ROWS BUF:CLEAR  TIMES BUF:CLEAR  0 REQ-N !
   0 PIECES U64+ ;

\ OBJECT as REFS then PIECES, FACTS-AT moved into it, as a request ends a piece.
: PIECE-LAY ( -- )
   REFS BYTES$ {: refs:ptr refsu:n :}
   OBJECT BUF:CLEAR  refs refsu OBJECT SPAN+  PIECES BYTES$ OBJECT SPAN+
   refsu FACTS-AT +! ;

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

\ FILE-KEY's object after its last piece, whose time is t, built for PIECE-PLACE's facts and written
\ only with a stamp and for hb's own target (LOAD); an UNLINKABLE file's INPUT is 0 0 -1, so it
\ never links and keeps its times.
: WRITE-OBJECT ( n -- )
   {: t:n :}
   \ FILES ends with the named files (NAMED+), then the run words' pieces REACHED lists (RAN-FILES+).
   NAMED+  RAN-FILES+  PATHS-LAY
   REQ-N @ 1 +  PIECES BYTES$ drop  LE:U64!
   REFS BYTES$ {: refs:ptr refsu:n :}
   REQUESTS BYTES$ {: reqs:ptr requ:n :}
   SPELLS BYTES$ {: nm:ptr nmu:n :}
   REACHED BYTES$ {: rch:ptr rchu:n :}
   OBJ-PATHS BYTES$ {: pth:ptr pthu:n :}
   TIMES BYTES$ {: tms:ptr tmsu:n :}
   PIECES BYTES$ {: pieces:ptr piecesu:n :}
   HEADER-BYTES INPUT-BYTES + {: files-at:n :}
   files-at refsu + {: reqs-at:n :}
   reqs-at requ + {: names-at:n :}
   names-at nmu + {: reached-at:n :}
   reached-at rchu + {: paths-at:n :}
   paths-at pthu + {: times-at:n :}
   times-at tmsu + {: pieces-at:n :}
   OBJECT BUF:CLEAR
   s" HABUFOBJ" OBJECT SPAN+
   0 OBJECT U64+  FILE-KEY 8 + LE:U64@ OBJECT U64+
   t OBJECT U64+
   HEADER-BYTES OBJECT U64+  INPUT-BYTES OBJECT U64+
   refsu 0<> if files-at else 0 then OBJECT U64+  refsu OBJECT U64+
   requ 0<> if reqs-at else 0 then OBJECT U64+  requ OBJECT U64+
   nmu 0<> if names-at else 0 then OBJECT U64+  nmu OBJECT U64+
   rchu 0<> if reached-at else 0 then OBJECT U64+  rchu OBJECT U64+
   paths-at OBJECT U64+  pthu OBJECT U64+
   times-at OBJECT U64+  tmsu OBJECT U64+
   pieces-at OBJECT U64+  piecesu OBJECT U64+
   UNLINKABLE @ if
      0 OBJECT U64+  0 OBJECT U64+  -1 OBJECT U64+
   else
      INPUT-STAMP INPUT-BYTES OBJECT SPAN+
   then
   refs refsu OBJECT SPAN+
   reqs requ OBJECT SPAN+
   nm nmu OBJECT SPAN+
   rch rchu OBJECT SPAN+
   pth pthu OBJECT SPAN+
   tms tmsu OBJECT SPAN+
   pieces piecesu OBJECT SPAN+
   files-at KEYS-OUT
   pieces-at FACTS-AT +!
   STAMPED? 0=  CROSS? or if exit then
   \ Unsynced: an object a crash cut short fails HEADER-CK's or PIECE-CK's bounds checks and compiles.
   OBJECT$ {: obj:ptr obju:n :}
   obj obju OBJECT-DIR
   obj obju [: OBJECT BYTES$ FS:WRITE-ALL ;] FS:REPLACE-STAGED ;

\ The piece's BUILT row with its number and time, its file's path and the USES entries since the
\ row before; NAMES takes its records.
: BUILT+ ( n n -- )
   {: piece:n t:n :}
   ndict@ BUILT-N INDEX-RECS
   BUILT-N 1 + BUILT-PATHS-RESERVE  KEY-PATH BUILT-N BUILT-PATHS !
   BUILT-N 1 + 2 * BUILT-USES-RESERVE
   USES-FROM @  BUILT-N 2 * BUILT-USES !
   USES-N @ USES-FROM @ -  BUILT-N 2 * 1 + BUILT-USES !
   USES-N @ USES-FROM !
   FILE-KEY KEY-BYTES BUILT SPAN+
   piece BUILT U64+  t BUILT U64+
   FILE-REC @ BUILT U64+  ndict@ BUILT U64+
   FILE-EM @ BUILT U64+  NSHADOW:EMISSIONS BUILT U64+
   FILE-DP @ BUILT U64+  FILE-END @ BUILT U64+ ;

\ ---- reading an object ---------------------------------------------------------
\ FRESH? reads FILE-KEY's object into OBJECT and checks every field WRITE-OBJECT writes.
\ The link reads these variables, ENTRY-OFFS, EM-OFFS and FILE-ROWS, valid once FRESH? answers true.
variable ENTRIES-N                    \ the object's entries
variable RECS-N                       \ its records: entries neither KIND-PRIVATE nor KIND-SEALED
variable EMS-N                        \ its emissions, E
variable DATA-AT                      \ the offset in OBJECT of DATA's D
variable CELLS-AT                     \ of its K
variable STRS-AT                      \ of STRINGS' N
variable STORES-AT                    \ of STORES' S
variable REQS-AT                      \ the offset in OBJECT of its REQUESTS
variable REQS-N                       \ its REQUESTS rows; it has one piece more
variable PIECE-AT                     \ the offset in OBJECT of the next piece to check
\ Only the check uses these and DOES-AT.
variable FILES-N                      \ its FILES rows
variable DEFS-N                       \ the KIND-DEF entries read so far
variable DOES-N                       \ the KIND-DOES entries read so far, DOES-AT's rows
variable LAST-EM                      \ the entry just read's emission when it is KIND-DEF, else -1
variable OWN-DATA-MAX                 \ the furthest a reference reaches into this file's data

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
   at 0<  at end >  or  end OBJECT BYTES$ nip >  or if BAD then ;


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
   OBJECT BYTES$ drop at + LE:U64@ ;


: RD-EQ ( n -- ) RD-U64 <> if BAD then ;


\ Bad unless the next u bytes equal the span's.
: RD-SAME ( ptr u8 n -- )
   {: a:ptr u:n :}
   RD-AT @ {: at:n :}
   u RD-STEP 0= if exit then
   OBJECT BYTES$ drop at + u  a u  STR= 0= if BAD then ;


\ A count of rows of at least n bytes each; bad, and 0, when the rest cannot hold them.
: RD-COUNT ( n -- n )
   {: row:n :}
   RD-U64 {: k:n :}
   k 0<  k  RD-END @ RD-AT @ - row /  >  or if BAD 0 exit then
   k ;


: OBJ@ ( n -- n ) OBJECT BYTES$ drop swap + LE:U64@ ;
: PRIOR@ ( n -- n ) PRIOR BYTES$ drop swap + LE:U64@ ;


\ FILE-ROWS' value for a FILES row whose piece a root's check passed but no BUILT row holds yet
\ (PENDING), and for one no BUILT row holds and no check passed.
-1 constant ROW-PENDING
-2 constant ROW-ABSENT

\ FILES row k's BUILT row's number at the byte offset (BUILT's layout); not for a pending row.
: BUILT@ ( n n -- n )
   {: k:n off:n :}
   k FILE-ROWS @ BUILT-AT off + LE:U64@ ;


: FILE-ROW? ( n -- bool )
   {: k:n :}
   k 1 >=  k FILES-N @ <=  and ;

\ A FILES row a reference may name: built, or pending.
: FILE-IN? ( n -- bool )
   {: k:n :}
   k FILE-ROW? 0= if false exit then
   k FILE-ROWS @ ROW-ABSENT <> ;

\ A FILES row naming a pending piece: the reference is checked again as that file links.
: PENDING? ( n -- bool )
   {: k:n :}
   k FILE-ROW? 0= if false exit then
   k FILE-ROWS @ ROW-PENDING = ;


: FILE-RECS ( n -- n )
   {: k:n :}
   k BUILT-FIRST 8 + BUILT@  k BUILT-FIRST BUILT@ - ;


: FILE-EMS ( n -- n )
   {: k:n :}
   k BUILT-EM 8 + BUILT@  k BUILT-EM BUILT@ - ;


: FILE-BYTES ( n -- n )
   {: k:n :}
   k BUILT-DP 8 + BUILT@  k BUILT-DP BUILT@ - ;


\ This object's emission e's F, after its S, R and S bytes.
: OWN-FUNS ( n -- n )
   EM-OFFS @ {: at:n :}
   at 16 + at OBJ@ + OBJ@ ;


\ This object's emission e's function j's offset, j below F; the offsets follow F.
: OWN-FUN-AT ( n n -- n )
   {: e:n j:n :}
   e EM-OFFS @ {: at:n :}
   at 24 + at OBJ@ +  j 8 * + OBJ@ ;


\ A reference: its tag and fields, 0 for absent ones (CODE+ states them).
: RD-REF ( -- n n n n )
   RD-U64 {: tag:n :}
   tag 0 = if tag RD-U64 RD-U64 0 exit then
   tag 1 = if tag RD-U64 0 0 exit then
   tag 2 = if tag RD-U64 RD-U64 RD-U64 exit then
   tag 3 = if tag RD-U64 RD-U64 0 exit then
   tag 4 = if tag RD-U64 0 0 exit then
   tag 5 = if tag 0 0 0 exit then
   BAD tag 0 0 0 ;


\ The kind of this object's record r, an entry already read; -1 for none.
: REC-KIND ( n -- n )
   {: r:n :}
   r 0<  r RECS-N @ >=  or if -1 exit then
   r REC-KINDS @ ;


\ A record reference (file, index): this file's index below the bound, or an
\ earlier file's below its record count.
: REC-REF-CK ( n n n -- )
   {: file:n idx:n bound:n :}
   GOOD? 0= if exit then
   idx 0< if BAD exit then
   file 0= if idx bound >= if BAD then exit then
   file FILE-IN? 0= if BAD exit then
   file PENDING? if exit then
   idx file FILE-RECS >= if BAD then ;


\ A quotation (file, emission, function); this file's emission at most top.
: FN-REF-CK ( n n n n -- )
   {: file:n e:n f:n top:n :}
   e 0<  f 0<  or if BAD exit then
   file 0= if
      e EMS-N @ >=  e top >  or if BAD exit then
      f e OWN-FUNS >= if BAD then exit
   then
   file FILE-IN? 0= if BAD exit then
   file PENDING? if exit then
   e file FILE-EMS >= if BAD exit then
   f  file BUILT-EM BUILT@ e + NSHADOW:FUNCTIONS  >= if BAD then ;


\ A code reference (RD-REF) whose own emission is at most top and own record below bound: the
\ records RESOLVE finds when the link resolves it. It names no package, which RESOLVE refuses.
: CODE-REF-CK ( n n n n n n -- )
   {: tag:n x:n y:n z:n top:n bound:n :}
   GOOD? 0= if exit then
   tag 0 = if
      x y bound REC-REF-CK
      GOOD? 0= if exit then
      x 0= if y REC-KIND KIND-PACKAGE = if BAD then exit then
      x PENDING? if exit then
      x BUILT-FIRST BUILT@ y + PKG? if BAD then exit
   then
   tag 1 = if rbase x + ARM64:HOST-KERNEL? 0= if BAD then exit then
   tag 2 = if x y z top FN-REF-CK exit then
   BAD ;


\ A data reference (RD-REF): a file's data, at most its size, or the open's header data.
: DATA-REF-CK ( n n n n -- )
   drop {: tag:n x:n y:n :}
   GOOD? 0= if exit then
   tag 3 = if
      y 0< if BAD exit then
      x 0= if y OWN-DATA-MAX @ max OWN-DATA-MAX ! exit then
      x FILE-IN? 0= if BAD exit then
      x PENDING? if exit then
      y x FILE-BYTES > if BAD then exit
   then
   tag 4 = if
      x 0<  x OPEN-DP @ BASE @ - >=  or if BAD then exit
   then
   BAD ;


\ The BUILT row with the FILES row's key, piece number and time (REF-FILE), and true; false when
\ none.
: BUILT-MATCH ( ptr u8 -- n bool )
   {: at:ptr :}
   BUILT-N 0 ?do
      i BUILT-AT FILES-ROW-BYTES  at FILES-ROW-BYTES  STR= if i true unloop exit then
   loop
   0 false ;


\ FILES row k, from 1, in OBJECT.
: FILES-ROW ( n -- ptr u8 )
   {: k:n :}
   OBJECT BYTES$ drop  HEADER-BYTES INPUT-BYTES +  k 1 - FILES-ROW-BYTES * +  + ;


\ The PENDING row equal to the FILES row, and true; false when none.
: PENDING-AT ( ptr u8 -- n bool )
   {: at:ptr :}
   PENDING BYTES$ {: a:ptr u:n :}
   u FILES-ROW-BYTES / 0 ?do
      a i FILES-ROW-BYTES * + FILES-ROW-BYTES  at FILES-ROW-BYTES  STR= if i true unloop exit then
   loop
   0 false ;


\ FILES row k's piece, built or pending, as a PENDING-STORERS holder: its BUILT row, else -2 - q
\ for its PENDING row q.
: PENDING-HOLDER ( n -- n )
   {: k:n :}
   k PENDING? if  k FILES-ROW PENDING-AT drop  -2 swap -  exit  then
   k FILE-ROWS @ ;

\ The row a holder other than header data has once the pending pieces link: PENDING row q's is
\ BUILT-N + q, as LINK-FILE adds each piece's BUILT row in the order the check passed them.
: HOLDER-ROW ( n -- n )
   {: held:n :}
   held 0 >= if held exit then
   BUILT-N  -2 held -  + ;

\ The pair (holder held, PENDING row p) onto PENDING-STORERS unless it is the newest pair, as PAIR+.
: PENDING-PAIR+ ( n n -- )
   {: held:n p:n :}
   PENDING-STORERS-N @ {: n:n :}
   n 0 > if
      n 1 - 2 * PENDING-STORERS @ held =  n 1 - 2 * 1 + PENDING-STORERS @ p =  and if exit then
   then
   n 1 + 2 * PENDING-STORERS-RESERVE
   held n 2 * PENDING-STORERS !  p n 2 * 1 + PENDING-STORERS !
   n 1 + PENDING-STORERS-N ! ;


\ FILE-ROWS from 1 for OBJECT's FILES rows: each its BUILT row, else ROW-PENDING or ROW-ABSENT.
: MAP-FILES ( -- )
   FILES-N @ 0 ?do
      i 1 + FILES-ROW {: at:ptr :}
      at BUILT-MATCH if
         i 1 + FILE-ROWS !
      else
         drop  at PENDING-AT nip if ROW-PENDING else ROW-ABSENT then  i 1 + FILE-ROWS !
      then
   loop ;


\ FILES' rows, u bytes, mapped (MAP-FILES). A row may name a piece the root's check has not reached,
\ so only the last piece's check (ROWS-CK) requires every row.
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


\ After FILES-CK: bad when the source names a record of the build's body or of a file FILES
\ does not list (NAMES-CK), built or pending (PENDING-HIT): the pending piece links before it.
: NAMED-CK ( -- )
   GOOD? 0= if exit then
   BODY-HIT @ if BAD exit then
   FILES-N @ 0 ?do
      i 1 + FILE-ROWS @ {: row:n :}
      row 0 >= if  row HITS @ 0<> if 2 row HITS ! then  then
      row ROW-PENDING = if
         i 1 + FILES-ROW PENDING-AT if 2 swap PENDING-HITS ! else drop then
      then
   loop
   BUILT-N 0 ?do i HITS @ 1 = if BAD then loop
   PENDING-N 0 ?do i PENDING-HITS @ 1 = if BAD then loop ;


\ REACHED's u bytes, FILES rows: bad when a built or pending piece that stored into header data or into
\ a reached piece's data or records is unreached, or reached pieces link out of order (the later store holds).
: REACHED-CK ( n -- )
   {: u:n :}
   GOOD? 0= if exit then
   u 0= if exit then
   BUILT-N PENDING-N + 1 + {: marks:n :}
   marks WALKED-RESERVE  marks 0 ?do 0 i WALKED ! loop
   -1  u 8 / 0 ?do
      {: last:n :}
      RD-U64 {: k:n :}
      k FILE-ROW? 0= if BAD unloop exit then
      k FILE-ROWS @ {: at:n :}
      at ROW-PENDING = if k PENDING-HOLDER HOLDER-ROW else at then {: row:n :}
      row 0 >= if
         row last <= if BAD unloop exit then
         1 row WALKED !  row
      else
         last
      then
   loop
   drop
   STORERS-N @ 0 ?do
      i 2 * STORERS @ {: held:n :}
      held -1 = if true else held WALKED @ 0<> then if
         i 2 * 1 + STORERS @ WALKED @ 0= if BAD then
      then
   loop
   PENDING-STORERS-N @ 0 ?do
      i 2 * PENDING-STORERS @ {: held:n :}
      held -1 = if true else held HOLDER-ROW WALKED @ 0<> then if
         BUILT-N  i 2 * 1 + PENDING-STORERS @ +  WALKED @ 0= if BAD then
      then
   loop ;


\ A name: its length, at least 1, then its bytes.
: NAME-CK ( -- ) RD-U64 {: u:n :}  u 1 < if BAD exit then  u RD-SKIP ;

\ A name: its length, then its bytes, which stay in OBJECT.
: RD-NAME ( -- ptr u8 n )
   RD-U64 {: u:n :}
   OBJECT BYTES$ drop RD-AT @ + {: a:ptr :}
   u RD-SKIP
   a u ;


\ REQUESTS' rows, u bytes from RD-AT (WRITE-OBJECT), counted into REQS-N.
: REQUESTS-CK ( n -- )
   {: u:n :}
   0 REQS-N !
   RD-AT @ u + {: end:n :}
   begin GOOD?  RD-AT @ end <  and while
      16 RD-SKIP
      RD-U64 {: kind:n :}
      kind SOURCE-UNIT:LOAD-INCLUDE <>  kind SOURCE-UNIT:LOAD-REQUIRE <>  and if BAD then
      NAME-CK  NAME-CK
      RD-U64 FILE-ROW? 0= if BAD then
      1 REQS-N +!
   repeat
   RD-AT @ end <> if BAD then ;


\ NAMES' u bytes from RD-AT (SPELLS), u a multiple of 8, run as SPELLED ran them: HIT for each
\ sum, OPENED+ for the sum after a 0. Against the source INPUT-STAMP stamps, HITS ends as a scan's.
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


\ REQUESTS row k's offset in OBJECT, from 0; the row starts with the time and the length of the
\ piece it ends.
: REQ-AT ( n -- n )
   {: k:n :}
   REQS-AT @  k 0 ?do
      24 +  dup OBJ@ 8 + +  dup OBJ@ 8 + +  8 +
   loop ;

\ The length in bytes of the piece that ends with REQUESTS row k.
: PIECE-LEN ( n -- n ) REQ-AT 8 + OBJ@ ;


\ REQUESTS row k's kind, the offsets of its spelled and loaded paths, and its FILES row.
: REQ-FIELDS ( n -- n n n n )
   REQ-AT 16 + {: at:n :}
   at 8 + {: spelled:n :}
   spelled 8 + spelled OBJ@ + {: loaded:n :}
   at OBJ@  spelled  loaded  loaded 8 + loaded OBJ@ + OBJ@ ;


\ The FIELD+ at the offset in OBJECT.
: FIELD$ ( n -- ptr u8 n )
   {: at:n :}
   OBJECT BYTES$ drop at 8 + +  at OBJ@ ;


\ The number of the piece that ends with REQUESTS row req, or of the last (-1).
: PIECE-N ( n -- n )
   {: req:n :}
   req 0< if REQS-N @ exit then
   req ;

\ The time in OBJECT of the piece that ends with REQUESTS row req, or of the last (-1): the
\ header's.
: PIECE-TIME ( n -- n )
   {: req:n :}
   req 0< if 8 KEY-BYTES + OBJ@ exit then
   req REQ-AT OBJ@ ;


\ The offset in OBJECT of piece k's word count in TIMES, once HEADER-CK passed.
: TIMES-AT ( n -- n )
   {: k:n :}
   TIMES-FIELD OBJ@  k 0 ?do  dup OBJ@ WORD-BYTES * + 8 +  loop ;


\ A TIMES span: an offset and a length inside a piece of plen bytes.
: SPAN-CK ( n -- )
   {: plen:n :}
   RD-U64 RD-U64 {: off:n u:n :}
   off 0<  u 0<  or  off plen >  or  u plen off - >  or if BAD then ;


\ TIMES from RD-AT to PIECES at the offset, in an object of len bytes: a block per piece (TIMES+),
\ each span inside its piece, as long as its REQUESTS row says or to the object's end for the last.
: TIMES-CK ( n n -- )
   {: pieces-at:n len:n :}
   GOOD? 0= if exit then
   pieces-at 8 +
   REQS-N @ 1 + 0 ?do
      {: first:n :}
      i REQS-N @ < if i PIECE-LEN else len first - then {: plen:n :}
      plen 0<  plen len first - >  or if BAD then
      WORD-BYTES RD-COUNT 0 ?do  8 RD-SKIP  plen SPAN-CK  plen SPAN-CK  plen SPAN-CK  loop
      first plen +
   loop drop
   RD-AT @ pieces-at <> if BAD then ;


\ The header and every section before PIECES as WRITE-OBJECT lays them: INPUT against INPUT-STAMP, NAMES
\ replayed (NAMES-CK, NAMED-CK), REACHED-CK, PATHS skipped as KEYS-IN read it, TIMES-CK; then PIECE-AT.
: HEADER-CK ( -- )
   0 FILES-N !  0 REQS-N !
   OBJECT BYTES$ nip {: len:n :}
   0 len RD-OPEN
   HEADER-BYTES INPUT-BYTES + {: files-at:n :}
   len files-at < if BAD exit then
   s" HABUFOBJ" RD-SAME
   FILE-KEY KEY-BYTES RD-SAME
   GOOD? 0= if exit then
   8 RD-SKIP
   HEADER-BYTES RD-EQ  INPUT-BYTES RD-EQ
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
   paths-at pthu + {: times-at:n :}
   RD-U64 RD-U64 {: toff:n timesu:n :}
   timesu 0<  timesu len times-at - >  or if BAD exit then
   toff times-at <> if BAD then
   times-at timesu + {: pieces-at:n :}
   pieces-at RD-EQ  len pieces-at - RD-EQ
   INPUT-STAMP INPUT-BYTES RD-SAME
   refsu FILES-CK
   reqs-at REQS-AT !  requ REQUESTS-CK
   nmu NAMES-CK  NAMED-CK
   rchu REACHED-CK
   pthu RD-SKIP
   pieces-at len TIMES-CK
   REQS-N @ 1 + RD-EQ
   RD-AT @ PIECE-AT ! ;


\ A record reference naming a package: an earlier entry of KIND-PACKAGE, or an earlier file's
\ package record. It answers the file and index it read.
: PKG-REF-CK ( -- n n )
   RD-U64 RD-U64 {: file:n idx:n :}
   file idx RECS-N @ REC-REF-CK
   GOOD? if
      file 0= if
         idx REC-KIND KIND-PACKAGE <> if BAD then
      else
         file PENDING? 0= if  file BUILT-FIRST BUILT@ idx + PKG? 0= if BAD then  then
      then
   then
   file idx ;


\ A package holding a private wordlist, by the file and index a reference names it with.
: PRIV+ ( n n -- )
   {: file:n idx:n :}
   file PRIVS-N @ 2 * PRIVS !  idx PRIVS-N @ 2 * 1 + PRIVS !
   1 PRIVS-N +! ;


\ A built package, by record index, a piece the root's check passed gave a private wordlist.
: GRANT+ ( n -- )
   GRANTS-N @ 1 + GRANTS-RESERVE
   GRANTS-N @ GRANTS !  1 GRANTS-N +! ;

: GRANTED? ( n -- bool )
   {: r:n :}
   GRANTS-N @ 0 ?do i GRANTS @ r = if true unloop exit then loop
   false ;


\ Whether the package a reference names has a private wordlist: an earlier entry gave it one, it is
\ an earlier file's package record holding one or granted one (GRANT+), or a pending piece's,
\ which its link checks again.
: PRIVATE? ( n n -- bool )
   {: file:n idx:n :}
   PRIVS-N @ 0 ?do
      i 2 * PRIVS @ file =  i 2 * 1 + PRIVS @ idx =  and if true unloop exit then
   loop
   file 0= if false exit then
   file PENDING? if true exit then
   file BUILT-FIRST BUILT@ idx + {: r:n :}
   r GRANTED?  r XREF-REC XREF-PKG-PRIVATE 0<>  or ;


\ A KIND-SEALED entry's package: an earlier file's, unsealed until the link seals it. A package of
\ this file carries its seal in its own record's flags.
: SEALED-CK ( -- )
   PKG-REF-CK {: file:n idx:n :}
   GOOD? 0= if exit then
   file 0= if BAD exit then
   file PENDING? if exit then
   file BUILT-FIRST BUILT@ idx + XREF-REC XREF:SEALED? if BAD then ;


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
   PKG-REF-CK {: file:n idx:n :}
   GOOD? 0= if exit then
   file idx PRIVATE? 0= if BAD then ;


\ A KIND-DEF entry's emission: NSHADOW:PUBLISH files one emission per definition, so it is the
\ count of KIND-DEF entries before it.
: EM-NAMED ( -- n )
   RD-U64 {: e:n :}
   e DEFS-N @ <> if BAD then
   1 DEFS-N +!  e ;


\ A package entry's own-private flag; a 1 adds the package, the next record, to PRIVS.
: PKG-FLAG-CK ( -- )
   RD-U64 {: f:n :}
   f 0<>  f 1 <>  and if BAD then
   f 1 = if 0 RECS-N @ PRIV+ then ;


\ A does record's emission, its definer's just before it, then its entry offset (DOES-ENTRY-CK).
: DOES-CK ( -- )
   DOES-N @ 1 + DOES-AT-RESERVE
   RD-AT @ DOES-N @ DOES-AT !  1 DOES-N +!
   RD-U64 {: e:n :}
   LAST-EM @ 0<  e LAST-EM @ <>  or if BAD then
   8 RD-SKIP ;


\ A cell's kind, 0-2 (ROW-KIND), and its value's reference (CELL-REF+).
: CELL-REF-CK ( n -- )
   {: kind:n :}
   RD-REF {: tag:n x:n y:n z:n :}
   tag 5 = if kind 0<  kind 2 >  or if BAD then exit then
   kind 0= if tag x y z EMS-N @ 1 - RECS-N @ CODE-REF-CK exit then
   kind 1 = if tag x y z DATA-REF-CK exit then
   BAD ;


\ A definition's dispatch cell (DISPATCH+): none, or a whole cell of this file's data.
: DISPATCH-CK ( -- )
   RD-REF {: tag:n x:n y:n z:n :}
   tag 5 = if exit then
   tag 3 <>  x 0<>  or  y 0<  or  y 7 and 0<>  or  y 8 + 0<  or if BAD exit then
   y 8 + OWN-DATA-MAX @ max OWN-DATA-MAX ! ;


\ Entry i: its offset into ENTRY-OFFS, then its fields (RECORD+, LATE+, SEAL+); a record's index
\ is RECS-N as it starts.
: ENTRY-CK ( n -- )
   {: i:n :}
   RD-AT @ i ENTRY-OFFS !
   RD-U64 {: kind:n :}
   kind 0<  kind KIND-TYPE >  or if BAD exit then
   kind KIND-PRIVATE = if
      \ A package holding a private wordlist gains none (OUTER:PACKAGE-PRIVATE refuses it).
      PKG-REF-CK {: file:n idx:n :}
      GOOD? if
         file PENDING? if
            file idx PRIV+
         else
            file idx PRIVATE? if BAD else
               file idx PRIV+
               file 0<> if  file BUILT-FIRST BUILT@ idx + GRANT+  then
            then
         then
      then
      -1 LAST-EM ! exit
   then
   kind KIND-SEALED = if SEALED-CK  -1 LAST-EM ! exit then
   NAME-CK  FLAGS-CK  kind ROLE-CK
   kind KIND-DEF = if EM-NAMED else -1 then {: em:n :}
   kind KIND-DEF =  GOOD? and if RECS-N @ em EM-RECS ! then
   kind KIND-DEF = if DISPATCH-CK then
   kind KIND-PACKAGE = if PKG-FLAG-CK then
   kind KIND-ALIAS = if RD-U64 RD-U64 RECS-N @ REC-REF-CK then
   kind KIND-DOES = if DOES-CK then
   em LAST-EM !
   kind RECS-N @ REC-KINDS !
   1 RECS-N +! ;


\ A piece's entries (PIECE+); 24 bytes is a KIND-PRIVATE or KIND-SEALED entry's size.
: ENTRIES-CK ( -- )
   24 RD-COUNT {: n:n :}
   n ENTRIES-N !  0 RECS-N !  0 DEFS-N !  0 DOES-N !  0 PRIVS-N !  -1 LAST-EM !
   0 OWN-DATA-MAX !
   n 0 > if
      n ENTRY-OFFS-RESERVE  n REC-KINDS-RESERVE  n EM-RECS-RESERVE  n 2 * PRIVS-RESERVE
   then
   n 0 ?do i ENTRY-CK loop ;


\ A call row of emission top, s bytes; its site in the object's copy holds the zeroed call EMISSION-CODE+
\ writes. A record target precedes top's own: a body naming its word is E-UNDEFINED, RECURSE files no row.
: CALL-CK ( n n -- )
   {: s:n top:n :}
   RD-U64 {: site:n :}
   site 0<  site 3 and 0<>  or  site s 4 - >  or if BAD then
   GOOD? if
      OBJECT BYTES$ drop  top EM-OFFS @ 16 + +  site +  LE:U32@ 0<> if BAD then
   then
   RD-U64 {: kind:n :}
   kind 0<>  kind 1 <>  and if BAD then
   RD-REF top  top EM-RECS @  CODE-REF-CK ;


\ ADDRESS-CARRIER:CHAIN-SIZE (0, 12 or 16) of the chain at an address row's site in the object's
\ copy of emission e's bytes, s of them.
: OWN-CHAIN ( n n n -- n )
   {: site:n s:n e:n :}
   OBJECT BYTES$ drop  e EM-OFFS @ 16 + + {: src:ptr :}
   src site +  src s +  ADDRESS-CARRIER:CHAIN-SIZE ;


\ An address row of an emission of s bytes, numbered top: a code chain is ADDR-CHAIN-BYTES long,
\ as the link requires, and a data chain is whole. A record target precedes top's own, as a call's.
: ADDR-CK ( n n -- )
   {: s:n top:n :}
   RD-U64 {: site:n :}
   site 0<  site 3 and 0<>  or  site s >=  or if BAD then
   RD-U64 {: kind:n :}
   GOOD? if
      site s top OWN-CHAIN {: size:n :}
      kind 0=  size ADDRESS-CARRIER:ADDR-CHAIN-BYTES <>  and if BAD then
      kind 1 =  size 0=  and if BAD then
   then
   kind 0= if RD-REF top  top EM-RECS @  CODE-REF-CK exit then
   kind 1 = if RD-REF DATA-REF-CK exit then
   BAD ;


\ The does> slot: -1, or its site, holding the zeroed slot EMISSION-CODE+ writes, and its target:
\ none while no clause patched it, else the record of a does> clause, checked as a call's.
: SLOT-CK ( n n -- )
   {: s:n top:n :}
   RD-U64 {: p:n :}
   p -1 = if exit then
   p 0<  p 3 and 0<>  or  p s 4 - >  or if BAD exit then
   GOOD? if
      OBJECT BYTES$ drop  top EM-OFFS @ 16 + +  p +  LE:U32@ 0<> if BAD then
   then
   RD-REF {: tag:n x:n y:n z:n :}
   tag 5 = if exit then
   tag 0 <> if BAD exit then
   tag x y z top  top EM-RECS @ 1 +  CODE-REF-CK
   GOOD? 0= if exit then
   x 0= if  y REC-KIND KIND-DOES <> if BAD then  exit  then
   x PENDING? if exit then
   x BUILT-FIRST BUILT@ y + {: r:n :}
   r 1 <  if BAD exit then
   r 1 - XREF-DOES-COMPANION? 0= if BAD then ;


\ Emission e (EMISSION-CODE+): its offset into EM-OFFS, then S, R, S bytes, F offsets, calls,
\ addresses and the slot. Each RD-COUNT size is the fewest bytes that row takes.
: EMISSION-CK ( n -- )
   {: e:n :}
   RD-AT @ e EM-OFFS !
   RD-U64 {: s:n :}
   s 4 <  s 3 and 0<>  or if BAD then
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
   32 RD-COUNT 0 ?do s e CALL-CK loop
   32 RD-COUNT 0 ?do s e ADDR-CK loop
   s e SLOT-CK ;


\ Each KIND-DOES entry's offset, after its emission e: function j's offset of e, j from 1 to F - 1,
\ an ENTRY@ as RECORD+ writes it.
: DOES-ENTRY-CK ( -- )
   GOOD? 0= if exit then
   DOES-N @ 0 ?do
      i DOES-AT @ {: at:n :}
      at OBJ@  at 8 + OBJ@ {: e:n off:n :}
      false  e OWN-FUNS 1 ?do  e i OWN-FUN-AT off =  or  loop
      0= if BAD unloop exit then
   loop ;


\ E, then each emission (CODE+); 60 bytes is a 4-byte emission of one function. Every emission
\ is one KIND-DEF entry's.
: CODE-CK ( -- )
   60 RD-COUNT {: n:n :}
   n EMS-N !
   n DEFS-N @ <> if BAD then
   n 0 > if n EM-OFFS-RESERVE then
   n 0 ?do i EMISSION-CK loop
   DOES-ENTRY-CK ;


\ A cell row of a file's d data bytes.
: CELL-ROW-CK ( n -- )
   {: d:n :}
   RD-U64 {: off:n :}
   off 0<  off 7 and 0<>  or  off d 8 - >  or if BAD then
   RD-U64 CELL-REF-CK ;


\ A string row after one at offset prev: the 8 bytes before its body in D hold its length. A
\ string the table already holds was laid by an earlier file since, so the file compiles.
: STRING-CK ( n n -- n )
   {: prev:n d:n :}
   RD-U64 RD-U64 {: off:n u:n :}
   GOOD? 0= if prev exit then
   off prev <=  off 8 <  or  off d >  or  u 0<  or  u d off - >  or if BAD prev exit then
   DATA-AT @ off + OBJ@ u <> if BAD then
   GOOD? if  OBJECT BYTES$ drop DATA-AT @ 8 + off + +  u  INTERN:FIND 0<> if BAD then  then
   off ;


\ A store row: a cell FIND-STORES admits by where it starts, at an aligned offset below an earlier
\ file's data end (a cell may run past an unaligned end: main.f's D is 98) or in the header data.
: STORE-CK ( -- )
   RD-REF {: tag:n x:n y:n z:n :}
   tag 3 =  x 1 >=  and  tag 4 =  or 0= if BAD exit then
   tag x y z DATA-REF-CK
   GOOD? 0= if exit then
   tag 3 = if
      y 7 and 0<> if BAD exit then
      x PENDING? 0= if  y x FILE-BYTES >= if BAD exit then  then
   then
   tag 4 = if x 7 and 0<> if BAD exit then then
   RD-U64 RD-U64 {: kind:n v:n :}
   kind 3 = if RD-REF {: t:n a:n b:n c:n :}  t 5 <> if BAD then exit then
   v 0<> if BAD exit then
   kind CELL-REF-CK ;


\ DATA, STRINGS and STORES (DATA+, STORES+).
: DATA-CK ( -- )
   RD-AT @ DATA-AT !
   RD-U64 {: d:n :}
   d RD-SKIP
   RD-AT @ CELLS-AT !
   24 RD-COUNT 0 ?do d CELL-ROW-CK loop
   RD-AT @ STRS-AT !
   0  16 RD-COUNT 0 ?do d STRING-CK loop drop
   RD-AT @ STORES-AT !
   40 RD-COUNT 0 ?do STORE-CK loop
   OWN-DATA-MAX @ d > if BAD then ;


\ FACTS (FACTS+): each row a retirement of a record reference, a text for a record reference, or
\ a text of a kind or of one of the object's records.
: FACTS-CK ( -- )
   RD-AT @ FACTS-AT !
   16 RD-COUNT 0 ?do
      RD-U64 {: ix:n :}
      ix CHECK:HELD-RETIRED =  ix CHECK:HELD-AMENDED =  or if
         RD-U64 RD-U64 {: file:n idx:n :}
         file idx RECS-N @ REC-REF-CK
         ix CHECK:HELD-AMENDED = if RD-U64 RD-SKIP then
      else
         ix CHECK:HELD-UNDEFINE <  ix RECS-N @ >=  or if BAD then
         RD-U64 RD-SKIP
      then
   loop ;


\ ACTS (ACTS+): each row a kind, EV-REQUIRED or EV-PROVIDED, and a path.
: ACTS-CK ( -- )
   RD-AT @ ACTS-AT !
   16 RD-COUNT 0 ?do
      RD-U64 {: kind:n :}
      kind EV-REQUIRED <>  kind EV-PROVIDED <>  and if BAD then
      NAME-CK
   loop ;


\ USES (USES+): each row a FILES row a reference may name, above the row before it.
: USES-CK ( -- )
   RD-AT @ USES-AT !
   0  8 RD-COUNT 0 ?do
      RD-U64 {: prev:n k:n :}
      k FILE-IN? 0=  k prev <=  or if BAD then
      k
   loop drop ;


\ The piece at PIECE-AT after MAP-FILES, ending with REQUESTS row req or -1 at the object's end, as long
\ as that row says (PIECE-LEN), its TIMES block counting its records; PIECE-AT then the next piece's.
: PIECE-CK ( n -- )
   {: req:n :}
   GOOD? 0= if exit then
   PIECE-AT @ {: start:n :}
   start  OBJECT BYTES$ nip  RD-OPEN
   ENTRIES-CK  CODE-CK  DATA-CK  FACTS-CK  ACTS-CK  USES-CK
   req PIECE-N TIMES-AT OBJ@  RECS-N @  <> if BAD then
   req RD-EQ
   req 0 >=  if  RD-AT @ start -  req PIECE-LEN <>  if BAD then  then
   req 0<  RD-AT @ RD-END @ <>  and if BAD then
   req 0< if ROWS-CK then
   RD-AT @ PIECE-AT ! ;


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
   8 KEY-BYTES + 8 + {: pairs:n :}
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
   OBJECT$ OBJECT READ-WHOLE 0= if false exit then
   OBJECT KEYS-IN ;


\ ---- the last build's object ----------------------------------------------------
\ As a piece compiles, PRIOR holds its file's object as the last build left it: a piece whose
\ bytes and references equal its old piece's keeps that piece's time (COMPILED-TIME).

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
   8 KEY-BYTES + 8 + {: pairs:n :}
   pairs 16 + PRIOR-PAIR?  pairs 32 + PRIOR-PAIR? and  pairs 96 + PRIOR-PAIR? and  pairs 112 + PRIOR-PAIR? and
   0= if false exit then
   pairs 24 + PRIOR@ FILES-ROW-BYTES mod 0= ;


\ The PRIOR REQUESTS row at the offset, which ends by rend: time, length, kind, two paths (a
\ length, then its bytes) and a FILES row. The offset after it, and true; false when it passes rend.
: PRIOR-ROW ( n n -- n bool )
   {: at:n rend:n :}
   at 32 + rend > if 0 false exit then
   at 24 + PRIOR@ {: u:n :}
   u 0<  u rend at - 32 - >  or if 0 false exit then
   at 32 + u + {: at2:n :}
   at2 8 + rend > if 0 false exit then
   at2 PRIOR@ {: u2:n :}
   u2 0<  u2 rend at2 - 16 - >  or if 0 false exit then
   at2 16 + u2 + true ;


\ Piece k of PRIOR (PRIOR-READ): its offset, length and time, and true; zeros and false when
\ PRIOR has no such piece or a row leaves REQUESTS. Row k gives the time and length; the piece
\ the rows end at is the last, with the header's time, running to PRIOR's end.
: PRIOR-PIECE ( n -- n n n bool )
   {: k:n :}
   PRIOR BYTES$ nip {: len:n :}
   8 KEY-BYTES + 8 + {: pairs:n :}
   pairs 32 + PRIOR@ {: roff:n :}
   roff  pairs 40 + PRIOR@ + {: rend:n :}
   roff  pairs 112 + PRIOR@ 8 +
   k 0 ?do
      {: at:n piece:n :}
      at rend PRIOR-ROW {: next:n ok:bool :}
      ok 0= if 0 0 0 false unloop exit then
      at 8 + PRIOR@ {: u:n :}
      u 0<  piece len > or  u len piece - > or if 0 0 0 false unloop exit then
      next  piece u +
   loop
   {: row:n first:n :}
   first len > if 0 0 0 false exit then
   row rend = if first  len first -  8 KEY-BYTES + PRIOR@  true exit then
   row rend PRIOR-ROW nip 0= if 0 0 0 false exit then
   row 8 + PRIOR@ {: size:n :}
   size 0<  size len first - >  or if 0 0 0 false exit then
   first size  row PRIOR@  true ;


\ The count of PRIOR's leading FILES rows from 1 that equal REFS' rows by key and piece number.
: KEPT-ROWS ( -- n )
   REFS BYTES$ {: refs:ptr refsu:n :}
   8 KEY-BYTES + 8 + 16 + {: files:n :}
   PRIOR BYTES$ drop  files PRIOR@ + {: old:ptr :}
   files 8 + PRIOR@ FILES-ROW-BYTES /  refsu FILES-ROW-BYTES /  min {: n:n :}
   n 0 ?do
      refs i FILES-ROW-BYTES * +  KEY-BYTES 8 +
      old i FILES-ROW-BYTES * +  KEY-BYTES 8 +  STR= 0= if i unloop exit then
   loop
   n ;

\ PRIOR's FILES rows from 1 equal REFS' rows by key and piece number, every row REFS holds.
: REFS-KEPT? ( -- bool ) KEPT-ROWS  REFS BYTES$ nip FILES-ROW-BYTES /  = ;


\ The word count of the PRIOR TIMES block at the offset, which ends by end, and true; 0 and false
\ when the count or its words pass end.
: PRIOR-COUNT ( n n -- n bool )
   {: at:n end:n :}
   at 8 +  end > if 0 false exit then
   at PRIOR@ {: n:n :}
   n 0<  n  end at - 8 -  WORD-BYTES /  >  or if 0 false exit then
   n true ;


\ Piece k's block in PRIOR's TIMES (PRIOR-READ): the offset of its first word and its word count,
\ and true; false when a count leaves TIMES.
: PRIOR-WORDS ( n -- n n bool )
   {: k:n :}
   TIMES-FIELD PRIOR@ {: first:n :}
   first  TIMES-FIELD 8 + PRIOR@ + {: end:n :}
   first
   k 0 ?do
      {: at:n :}
      at end PRIOR-COUNT {: c:n ok:bool :}
      ok 0= if 0 0 false unloop exit then
      at 8 +  c WORD-BYTES * +
   loop
   {: last:n :}
   last end PRIOR-COUNT {: n:n found:bool :}
   last 8 +  n  found ;


\ The old piece the running piece's words compare with (PRIOR-PIECE!): its offset in PRIOR, its
\ length, the offset in PRIOR of its first word in TIMES, its words, 0 for none, and KEPT-ROWS.
variable PRIOR-AT
variable PRIOR-SIZE
variable PRIOR-FIRST-WORD
variable PRIOR-WORDS-N
variable KEPT-ROWS-N

\ Piece k of PRIOR and its TIMES block as the old piece (PRIOR-PIECE, PRIOR-WORDS); none when old
\ is false or either fails.
: PRIOR-PIECE! ( n bool -- )
   {: k:n old:bool :}
   0 PRIOR-WORDS-N !
   old 0= if exit then
   k PRIOR-PIECE {: at:n u:n t:n ok:bool :}
   ok 0= if exit then
   k PRIOR-WORDS {: words:n n:n found:bool :}
   found 0= if exit then
   at PRIOR-AT !  u PRIOR-SIZE !  words PRIOR-FIRST-WORD !  n PRIOR-WORDS-N !  KEPT-ROWS KEPT-ROWS-N ! ;


\ Word w's text range in the running piece, which starts at the offset in PIECES: its offset from
\ the piece's first byte and its length, 0 0 for none.
: TEXT-SPAN ( n n -- n n )
   {: start:n w:n :}
   w 4 WORD-SPAN @ {: u:n :}
   u 0= if 0 0 exit then
   w 3 WORD-SPAN @ start -  u ;


\ Word w's emission range, as TEXT-SPAN.
: EM-SPAN ( n n -- n n )
   {: start:n w:n :}
   w 2 WORD-SPAN @ {: em:n :}
   em 0< if 0 0 exit then
   em 3 * EM-SPANS @ start -  em 3 * 1 + EM-SPANS @ ;


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
   top  em 3 * 2 + EM-SPANS @  max ;


\ Whether word w's range s holds the bytes of the old word w's, a range of the old piece.
: SAME-SPAN? ( n n n -- bool )
   {: start:n w:n s:n :}
   start w s NEW-SPAN {: noff:n nu:n :}
   PRIOR-FIRST-WORD @  w WORD-BYTES * +  8 +  s 16 * + {: at:n :}
   at PRIOR@  at 8 + PRIOR@ {: ooff:n ou:n :}
   ooff 0<  ou 0<  or  ooff PRIOR-SIZE @ >  or  ou PRIOR-SIZE @ ooff - >  or if false exit then
   nu ou <> if false exit then
   PIECES BYTES$ drop start + noff +  nu  PRIOR BYTES$ drop PRIOR-AT @ + ooff +  ou  STR= ;


\ Word w's time: the old word w's when it has one, every FILES row the word names is within
\ KEPT-ROWS-N, so names the same file in both objects, and its ranges hold the same bytes; else now.
: WORD-TIME ( n n n -- n )
   {: start:n w:n now:n :}
   w PRIOR-WORDS-N @ >= if now exit then
   PRIOR-FIRST-WORD @  w WORD-BYTES * +  PRIOR@ {: t:n :}
   t 0=  w WORD-TOP KEPT-ROWS-N @ >  or if now exit then
   start w 0 SAME-SPAN? 0= if now exit then
   start w 1 SAME-SPAN? 0= if now exit then
   start w 2 SAME-SPAN? 0= if now exit then
   t ;

\ Word w's range s onto TIMES, its offset then its length (NEW-SPAN).
: SPAN-TIMES+ ( n n n -- )
   NEW-SPAN {: off:n u:n :}
   off TIMES U64+  u TIMES U64+ ;


\ The running piece's TIMES block, the piece starting at the offset in PIECES: its word count, then
\ per word its time (WORD-TIME), also into WORD-TIMES, and its entry, emission and text ranges.
: TIMES+ ( n n -- )
   {: start:n now:n :}
   WORDS-N @ TIMES U64+
   FILE-REC @ WORDS-N @ + WORD-TIMES-RESERVE
   WORDS-N @ 0 ?do
      start i now WORD-TIME {: t:n :}
      t TIMES U64+
      start i 0 SPAN-TIMES+  start i 1 SPAN-TIMES+  start i 2 SPAN-TIMES+
      t  FILE-REC @ i +  WORD-TIMES !
   loop ;


\ Piece k's time, the piece starting at the offset in PIECES: its old piece's when that has a time,
\ the same bytes and REFS-KEPT?, else now, the time TIMES+ gives each word that changed.
: COMPILED-TIME ( n n -- n )
   {: start:n k:n :}
   NOW {: now:n :}
   PRIOR-READ {: old:bool :}
   k old PRIOR-PIECE!
   start now TIMES+
   old 0= if now exit then
   k PRIOR-PIECE {: at:n u:n t:n ok:bool :}
   ok 0=  t 0=  or if now exit then
   PIECES BYTES$ {: a:ptr len:n :}
   len start - u <> if now exit then
   a start + u  PRIOR BYTES$ drop at + u  STR= 0= if now exit then
   REFS-KEPT? 0= if now exit then
   t ;


\ After INPUT!: true when FILE-KEY's object reads whole and checks (HEADER-CK); false without a stamp,
\ since an unstamped source's INPUT-STAMP, 0 0 -1, equals an UNLINKABLE file's INPUT.
: HEADER-FRESH? ( -- bool )
   STAMPED? 0= if false exit then
   READ-OBJECT 0= if false exit then
   HEADER-CK  GOOD? ;


\ ---- checking a file's requests --------------------------------------------------
\ A linked piece's request loads its file before the next piece links, and no piece compiles after
\ a linked one: so a file links only when every file its requests loaded, and theirs, checks too.
\ The check walks them in load order before anything links, each piece once MAP-FILES maps the
\ pieces passed before it (PENDING), and applies nothing.

\ While its request is checked or loads, OBJECT is a depth deeper and the file's place is on PARKED:
\ FILE-PATH, FILE-KEY, INPUT-STAMP, then PARK-TAIL: the path's length, PIECE-AT, FILES-N, REQS-N, REQS-AT.
5 8 * constant PARK-TAIL

: PARK ( -- )
   OBJECT-DEPTH @ 1 + OBJECT-DEPTHS = if E-STALE throw then
   FILE-PATH FILE-PATH-N @ PARKED SPAN+
   FILE-KEY KEY-BYTES PARKED SPAN+
   INPUT-STAMP INPUT-BYTES PARKED SPAN+
   FILE-PATH-N @ PARKED U64+
   PIECE-AT @ PARKED U64+  FILES-N @ PARKED U64+  REQS-N @ PARKED U64+  REQS-AT @ PARKED U64+
   1 OBJECT-DEPTH +! ;


\ The top of PARKED back and off it, and OBJECT the loader's again (PARK).
: UNPARK ( -- )
   -1 OBJECT-DEPTH +!
   PARKED BYTES$ {: f:ptr fu:n :}
   f fu + PARK-TAIL - {: t:ptr :}
   t LE:U64@ {: pn:n :}
   fu PARK-TAIL -  KEY-BYTES -  INPUT-BYTES -  pn - {: start:n :}
   f start + {: at:ptr :}
   at FILE-PATH pn BYTE-COPY  pn FILE-PATH-N !
   at pn + FILE-KEY KEY-BYTES BYTE-COPY
   at pn + KEY-BYTES + INPUT-STAMP INPUT-BYTES BYTE-COPY
   t 8 + LE:U64@ PIECE-AT !  t 16 + LE:U64@ FILES-N !
   t 24 + LE:U64@ REQS-N !  t 32 + LE:U64@ REQS-AT !
   f start BUF:N>BLEN PARKED BUF:REPLACE ;


\ Pending record r into PENDING-SLOTS by its sum.
: PENDING-PUT ( n -- )
   {: r:n :}
   PENDING-CAP @ 1 - {: mask:n :}
   r 0 PENDING-REC@ mask and
   begin dup PENDING-SLOTS @ 0<> while 1 + mask and repeat
   r 1 +  swap PENDING-SLOTS ! ;

\ PENDING-SLOTS with twice its slots, every pending record put again.
: PENDING-GROW ( -- )
   PENDING-CAP @ 2 * PENDING-SLOTS-CLEAR
   PENDING-RECS-N @ 0 ?do i PENDING-PUT loop ;

\ A pending record: its name's sum, its package key, its TOP-NAME's sum (0 for a changed record),
\ its PENDING row. Half PENDING-SLOTS' slots at most are filled.
: PENDING-REC+ ( n n n n -- )
   {: sum:n key:n top:n p:n :}
   PENDING-RECS-N @ {: r:n :}
   r 1 + 4 * PENDING-RECS-RESERVE
   sum r 4 * PENDING-RECS !  key r 4 * 1 + PENDING-RECS !
   top r 4 * 2 + PENDING-RECS !  p r 4 * 3 + PENDING-RECS !
   r 1 + PENDING-RECS-N !
   r 1 + 2 *  PENDING-CAP @ > if PENDING-GROW else r PENDING-PUT then ;

\ NAME-PKG's key, once PENDING row p links, for the entry whose role (ROLE-CK) is at the offset:
\ role 4's package's TOP-NAME sum, else 0; 0 past a pending piece's own records (PENDING?).
: PENDING-KEY ( n n -- n )
   {: ro:n p:n :}
   ro OBJ@ 4 <> if 0 exit then
   ro 8 + OBJ@  ro 16 + OBJ@ {: file:n idx:n :}
   file 0= if  p PENDING-FIRST @ idx +  2 PENDING-REC@ exit  then
   file PENDING? if
      file FILES-ROW PENDING-AT drop {: q:n :}
      q PENDING-FIRST @ idx + {: r:n :}
      r  q PENDING-FIRST @  q PENDING-OWN @ +  >= if 0 exit then
      r 2 PENDING-REC@ exit
   then
   file BUILT-FIRST BUILT@ idx + XREF-REC XREF-NAME$ TOP-NAME NAME-SUM ;

\ The entry at the offset in OBJECT as a record of PENDING row p.
: PENDING-ENTRY+ ( n n -- )
   {: at:n p:n :}
   at 8 + FIELD$ {: a:ptr u:n :}
   a u NAME-SUM  at 24 + u + p PENDING-KEY  a u TOP-NAME NAME-SUM  p PENDING-REC+ ;

\ Record idx of an earlier file, which PENDING row p changes, as a pair (PENDING-PAIR+) and, named,
\ a record of p with TOP-NAME sum 0: its name's sum and key, 0 once retired (XREF-RETIRED-WL has no package).
: PENDING-CHANGED+ ( n n bool n -- )
   {: file:n idx:n retired:bool p:n :}
   file 0= if exit then
   file PENDING? if
      file FILES-ROW PENDING-AT drop {: q:n :}
      q PENDING-FIRST @ idx + {: r:n :}
      r  q PENDING-FIRST @  q PENDING-OWN @ +  >= if exit then
      file PENDING-HOLDER p PENDING-PAIR+
      r 0 PENDING-REC@  retired if 0 else r 1 PENDING-REC@ then  0  p PENDING-REC+ exit
   then
   file PENDING-HOLDER p PENDING-PAIR+
   file BUILT-FIRST BUILT@ idx + {: d:n :}
   d XREF-REC XREF-NAME$ {: a:ptr u:n :}
   u 0= if exit then
   a u NAME-SUM  retired if 0 else d NAME-PKG then  0  p PENDING-REC+ ;

\ The earlier records the checked piece's FACTS retirement and amender rows change, as PENDING
\ row p's (PENDING-CHANGED+), read as LINK-FACTS reads them; FACTS-CK passed the rows.
: PENDING-FACTS+ ( n -- )
   {: p:n :}
   FACTS-AT @  OBJECT BYTES$ nip  RD-OPEN
   RD-U64 0 ?do
      RD-U64 {: ix:n :}
      ix CHECK:HELD-RETIRED =  ix CHECK:HELD-AMENDED =  or if
         RD-U64 RD-U64  ix CHECK:HELD-RETIRED =  p PENDING-CHANGED+
         ix CHECK:HELD-AMENDED = if RD-U64 RD-SKIP then
      else
         RD-U64 RD-SKIP
      then
   loop ;

\ The checked piece's records (ENTRIES-CK) as PENDING row p's own, in their order, so a record
\ index is its offset from PENDING-FIRST, then the earlier records it changes; p's mark cleared.
: PENDING-NAMES+ ( n -- )
   {: p:n :}
   p 1 + PENDING-FIRST-RESERVE  p 1 + PENDING-OWN-RESERVE  p 1 + PENDING-HITS-RESERVE
   PENDING-RECS-N @ p PENDING-FIRST !  0 p PENDING-HITS !
   ENTRIES-N @ 0 ?do
      i ENTRY-OFFS @ {: at:n :}
      at OBJ@ {: kind:n :}
      kind KIND-PRIVATE <>  kind KIND-SEALED <>  and if at p PENDING-ENTRY+ then
   loop
   PENDING-RECS-N @  p PENDING-FIRST @ -  p PENDING-OWN !
   ENTRIES-N @ 0 ?do
      i ENTRY-OFFS @ {: at:n :}
      at OBJ@ {: kind:n :}
      kind KIND-PRIVATE =  kind KIND-SEALED =  or if
         at 8 + OBJ@  at 16 + OBJ@  false  p PENDING-CHANGED+
      then
   loop
   p PENDING-FACTS+ ;


\ The checked piece's STORES rows (STORE-CK passed them) as PENDING row p's pairs, read as
\ LINK-DATA reads them: header data -1, else the holder of the target's FILES row.
: PENDING-STORES+ ( n -- )
   {: p:n :}
   STORES-AT @  OBJECT BYTES$ nip  RD-OPEN
   RD-U64 0 ?do
      RD-REF {: tag:n x:n y:n z:n :}
      RD-U64 drop  RD-U64 drop  RD-REF drop drop drop drop
      tag 4 = if -1 else x PENDING-HOLDER then  p PENDING-PAIR+
   loop ;


\ The piece ending with REQUESTS row req, or the last (-1), passed: its records (PENDING-NAMES+)
\ and stores (PENDING-STORES+) pending, then PENDING gains FILE-KEY and its number and time.
: PENDING+ ( n -- )
   {: req:n :}
   PENDING-N PENDING-NAMES+
   PENDING-N PENDING-STORES+
   FILE-KEY KEY-BYTES PENDING SPAN+
   req PIECE-N PENDING U64+  req PIECE-TIME PENDING U64+ ;


\ Forward: a request's check runs its file's pieces (CHECK-PIECES).
defer CHECK-OBJECT ( -- )

\ A requested file's bytes and path, from SOURCE-ROOT:WITH-BYTES: its object, under the key and with
\ the last piece's number and time its loader's FILES row names, checked as its loader's.
: CHECKED ( ptr u8 n ptr u8 n -- )
   {: src:ptr srcu:n path:ptr pathu:n :}
   WANT-KEY FILE-KEY KEY-BYTES BYTE-COPY
   path pathu PATH!
   INPUT!
   HEADER-FRESH? 0= if BAD exit then
   -1 PIECE-TIME WANT-TIME @ <>  REQS-N @ WANT-PIECE @ <>  or if BAD exit then
   CHECK-OBJECT ;

: CHECK-FILE ( ptr u8 n -- ) [: CHECKED ;] SOURCE-ROOT:WITH-BYTES ;


\ Whether a piece the check passed registered the path (PENDING-PATHS).
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

\ Whether the registry holds the path, or a piece the check passed registered it, as a compile of
\ the checked file would find it.
: REGISTERED? ( ptr u8 n bool -- bool )
   {: a:ptr u:n known:bool :}
   known if true exit then
   a u PENDING-PATH? ;


\ REQUESTS row k: the path it spells resolves, as a load from the checked file resolves it, to the
\ path it loaded, a require's not REGISTERED? yet and then registered; then that file checks a depth
\ deeper, short of OBJECT-DEPTHS, so a file nested deeper compiles and no link parks past it.
\ SOURCE-ROOT:RESOLVE is the last loader call before WITH-BYTES, which enters the root it resolved
\ through.
: CHECK-REQUEST ( n -- )
   {: k:n :}
   k REQ-FIELDS {: kind:n spelled:n loaded:n file:n :}
   file FILES-ROW {: at:ptr :}
   at WANT-KEY KEY-BYTES BYTE-COPY
   at BUILT-PIECE + LE:U64@ WANT-PIECE !  at BUILT-TIME + LE:U64@ WANT-TIME !
   spelled FIELD$ SOURCE-ROOT:RESOLVE {: canon:ptr canonu:n known:bool :}
   canon canonu  loaded FIELD$  STR= 0= if BAD exit then
   canon canonu FS:FILE? 0= if BAD exit then
   kind SOURCE-UNIT:LOAD-REQUIRE = if
      canon canonu known REGISTERED? if BAD exit then
      canon canonu PENDING-PATHS FIELD+
   then
   OBJECT-DEPTH @ 1 + OBJECT-DEPTHS = if BAD exit then
   PARK
   canon canonu ['] CHECK-FILE ['] UNPARK finally ;


\ The piece's ACTS rows (ACTS-CK), each path resolved as a load from the checked file resolves
\ it: a require's must be REGISTERED?, as its compile found it; a provide's is registered.
: ACTS-WALK ( -- )
   ACTS-AT @  OBJECT BYTES$ nip  RD-OPEN
   RD-U64 0 ?do
      RD-U64 {: kind:n :}
      RD-NAME SOURCE-ROOT:RESOLVE {: canon:ptr canonu:n known:bool :}
      kind EV-PROVIDED = if
         canon canonu PENDING-PATHS FIELD+
      else
         canon canonu known REGISTERED? 0= if BAD unloop exit then
      then
   loop ;


\ The checked file's pieces in load order, each request's file checked after the piece it ends.
: CHECK-PIECES ( -- )
   REQS-N @ 1 + 0 ?do
      GOOD? 0= if unloop exit then
      i REQS-N @ < if i else -1 then {: req:n :}
      MAP-FILES
      req PIECE-CK
      GOOD? if ACTS-WALK then
      GOOD? if
         req PENDING+
         req 0 >= if req CHECK-REQUEST then
      then
   loop ;

: INSTALL-CHECK ( -- ) [: CHECK-PIECES ;] is CHECK-OBJECT ;
INSTALL-CHECK

\ No piece the check passed, nor its paths, grants or records.
: CHECK-CLEAR ( -- )
   PENDING BUF:CLEAR  PENDING-PATHS BUF:CLEAR  0 GRANTS-N !  PENDING-RECS-CLEAR ;


\ After INPUT!: true when FILE-KEY's object reads whole and checks, with the files its requests
\ loaded (CHECK-OBJECT). It applies nothing. Under a linking file, which checked it, only the
\ header: each piece checks again as it links (LINK-FILE).
: FRESH? ( -- bool )
   HEADER-FRESH? 0= if false exit then
   LINKING? if true exit then
   CHECK-CLEAR
   CHECK-OBJECT
   CHECK-CLEAR
   GOOD? ;

\ ---- linking an object ---------------------------------------------------------
\ The words a link of a fresh object applies it with, once FRESH? answers true: each
\ reference's value (RESOLVE) and the entries that are not code.
variable LINK-FN                     \ the emission being linked's address: SOURCE-FUNS' at
variable LINK-EM                     \ that emission's number in this object, -1 for none

: LINK-CLEAR ( -- ) -1 LINK-EM !  0 LINK-FN ! ;


\ A record reference's file and index as a dictionary index.
: REC-INDEX ( n n -- n )
   {: file:n idx:n :}
   file 0= if FILE-REC @ idx + exit then
   file BUILT-FIRST BUILT@ idx + ;


\ A quotation reference's address: function f of the file's emission e, the host
\ entry the shadow filed for it, which CODE-REF+ wrote through SOURCE-FUNCTION.
: FN-ADDR ( n n n -- n )
   {: file:n e:n f:n :}
   file 0=  e LINK-EM @ =  and if LINK-FN @  e f OWN-FUN-AT + exit then
   file 0= if
      FILE-EM @ e +  dup NSHADOW:EMISSIONS >= if E-COMP-STATE throw then
   else
      file BUILT-EM BUILT@ e +
   then {: em:n :}
   em f NSHADOW:FUNCTION-SOURCE@ {: addr:n :}
   addr 0= if E-COMP-STATE throw then
   addr ;


\ A reference's value, from RD-REF's tag and fields (CODE+ states them); 0 for none.
: RESOLVE ( n n n n -- n )
   {: tag:n x:n y:n z:n :}
   tag 0 = if
      x y REC-INDEX {: r:n :}
      r ndict@ >= if E-COMP-STATE throw then
      r PKG? if E-COMP-STATE throw then
      r XREF-REC XREF-START exit
   then
   tag 1 = if rbase x + exit then
   tag 2 = if x y z FN-ADDR exit then
   tag 3 = if
      x 0= if FILE-DP @ y + exit then
      x BUILT-DP BUILT@ y + exit
   then
   tag 4 = if BASE @ x + exit then
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


\ The wordlist a role names (WORDLIST+); roles 3 and 4 are then the package's reference,
\ and a package without that wordlist (id 0) is E-COMP-STATE.
: ROLE-WID ( -- n )
   RD-U64 {: role:n :}
   role 0 = if BOOT:GLOBAL-WL exit then
   role 1 = if WL:PRIMITIVES exit then
   role 2 = if BOOT:NAMESPACE-WL exit then
   role 5 = if BOOT:TYPES-WL exit then
   role 3 <>  role 4 <>  and if E-COMP-STATE throw then
   RD-U64 RD-U64 REC-INDEX XREF-REC {: rec:ptr :}
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
   a u  own 1 =  OUTER:PACKAGE-RECORD  want <> if E-COMP-STATE throw then
   flags want SET-FLAGS ;


\ Entry i, a late private wordlist (LATE+), given to its package.
: LINK-PRIVATE ( n -- )
   {: i:n :}
   i ENTRY-OFFS @  OBJECT BYTES$ nip  RD-OPEN
   RD-U64 KIND-PRIVATE <> if E-COMP-STATE throw then
   RD-U64 RD-U64 {: file:n idx:n :}
   GOOD? 0= if E-COMP-STATE throw then
   file idx REC-INDEX dup CHANGER+ OUTER:PACKAGE-PRIVATE ;


\ Entry i, an earlier file's package this file sealed (SEAL+), sealed again.
: LINK-SEALED ( n -- )
   {: i:n :}
   i ENTRY-OFFS @  OBJECT BYTES$ nip  RD-OPEN
   RD-U64 KIND-SEALED <> if E-COMP-STATE throw then
   RD-U64 RD-U64 {: file:n idx:n :}
   GOOD? 0= if E-COMP-STATE throw then
   file idx REC-INDEX dup CHANGER+ XREF-REC XREF:SEAL ;


\ Entry i, an alias (RECORD+), as the next record.
: LINK-ALIAS ( n -- )
   {: i:n :}
   i ENTRY-OFFS @  OBJECT BYTES$ nip  RD-OPEN
   RD-U64 KIND-ALIAS <> if E-COMP-STATE throw then
   RD-NAME {: a:ptr u:n :}
   RD-U64 {: flags:n :}
   ROLE-WID {: wid:n :}
   RD-U64 RD-U64 {: file:n idx:n :}
   GOOD? 0= if E-COMP-STATE throw then
   file idx REC-INDEX {: src:n :}
   ndict@ {: want:n :}
   a u src wid OUTER:ALIAS-RECORD
   flags want SET-FLAGS
   ALIASES-N @ 2 + ALIASES-RESERVE
   want ALIASES-N @ ALIASES !  src ALIASES-N @ 1 + ALIASES !  2 ALIASES-N +! ;

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
   a u TYPE-SOURCE wid OUTER:ALIAS-RECORD
   flags want SET-FLAGS
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


\ This object's emission e (CODE+) sealed in NEMIT over CODE-COPY, as its compile sealed it: each
\ call site the compile's unplaced call, each address chain its value, the does> slot a return.
\ The answer is the entry of the clause that patched the slot, 0 for none.
: NEMIT-FILL ( n -- n )
   {: e:n :}
   e EM-OFFS @  OBJECT BYTES$ nip  RD-OPEN
   RD-U64 RD-U64 {: s:n r:n :}
   CODE-COPY BUF:CLEAR
   OBJECT BYTES$ drop RD-AT @ +  CODE-COPY s BUF:N>BLEN BUF:GROW SPAN:$ drop  s BYTE-COPY
   s RD-SKIP
   CODE-COPY BYTES$ drop {: copy:ptr :}
   copy s r  NSHADOW:BINDING CBIND:TARGET@ CTARGET:ARCH@  NEMIT:OPEN
   RD-U64 0 ?do RD-U64 NEMIT:FUNCTION+ loop
   0 HOST-CALLS-N !  0 HOST-ADDRS-N !
   RD-U64 0 ?do
      RD-U64 RD-U64 {: site:n k:n :}
      RD-REF RESOLVE {: target:n :}
      k 0= if NEMIT:CALL else NEMIT:TAIL then {: nk:n :}
      0 nk 0  copy site +  ARM64:CALL!
      site nk target 0 0 NEMIT:CALL-SITE+
      nk NEMIT:CALL =  target NPUB:IN-REGION? 0=  and if site HOST-CALL+ then
   loop
   \ PATCH turns a code chain into adrp+add, with no address-map row; a data chain needs one.
   RD-U64 0 ?do
      RD-U64 RD-U64 {: site:n k:n :}
      RD-REF RESOLVE {: v:n :}
      k 0= if
         ADDRESS-CARRIER:ADDR-CHAIN-BYTES
      else
         copy site +  copy s +  ADDRESS-CARRIER:CHAIN-SIZE
      then {: size:n :}
      copy site +  v size ADDRESS-CARRIER:SET-CHAIN-VALUE
      k 0= if NEMIT:ADDR-CODE else NEMIT:ADDR-DATA then {: nk:n :}
      site nk -1 NEMIT:ADDR-SITE+
      k 0<> if site HOST-ADDR+ then
   loop
   RD-U64 {: p:n :}
   p 0 >= if
      RD-REF {: tag:n x:n y:n z:n :}
      A64ASM:ENC-RET  copy p +  LE:U32!
      p NEMIT:PATCH-SLOT!
      tag 5 = if 0 else tag x y z RESOLVE then
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


\ Entry i, a definition (RECORD+), as the next record, placed and published; a KIND-DOES entry
\ right after it is its companion, the record after that.
: MAKE-DEF ( n -- )
   {: i:n :}
   i ENTRY-OFFS @  OBJECT BYTES$ nip  RD-OPEN
   RD-U64 KIND-DEF <> if E-COMP-STATE throw then
   RD-NAME {: a:ptr u:n :}
   RD-U64 {: flags:n :}
   ROLE-WID {: wid:n :}
   RD-U64 {: e:n :}
   RD-REF {: tag:n x:n y:n z:n :}
   GOOD? 0= if E-COMP-STATE throw then
   i 1 + ENTRIES-N @ < if i 1 + ENTRY-OFFS @ OBJ@ KIND-DOES = else false then {: does:bool :}
   does if
      i 1 + ENTRY-OFFS @  OBJECT BYTES$ nip  RD-OPEN
      RD-U64 drop  RD-NAME  RD-U64  ROLE-WID  RD-U64  RD-U64
   else
      NULL-PTR 0  0  0  e  0
   then {: da:ptr du:n dflags:n dwid:n de:n off:n :}
   GOOD? 0=  de e <>  or if E-COMP-STATE throw then
   does  tag 5 <>  and if E-COMP-STATE throw then
   does if e off DOES-FUN else 0 then {: j:n :}
   ndict@ {: want:n :}
   a u wid  flags  DKIND:MASK DNAME-CELL or DNAME-ALLOC or  and  OUTER:DEF-RECORD
   NPUB:NEXT-SLOT {: fn:n :}
   fn LINK-FN !  e LINK-EM !
   e NEMIT-FILL {: patch:n :}
   does if j NSHADOW:TAKE-DOES else NSHADOW:TAKE then
   fn NSHADOW:SOURCE-FUNS
   NEMIT:CLEAR
   want NSHADOW:PUBLISH
   \ As the does> primitive's patch: PATCH then writes the slot as a branch to the clause.
   patch 0<> if want patch NSHADOW:PATCH-DOES then
   CODE-COPY BYTES$ drop {: copy:ptr :}
   \ The call targets and chains already hold host values, so both resolvers answer what they get.
   NSHADOW:EMISSIONS 1 -  copy  fn  [: ;] [: ;]  ARM64:PATCH
   e EM-OFFS @ {: at:n :}
   at OBJ@  at 8 + OBJ@ {: s:n r:n :}
   r 0= if s CODE-SPAN:EXACT else s r - then {: recorded:n :}
   HOST-CALLS-N @ 0= if NULL-PTR else 0 HOST-CALLS then  HOST-CALLS-N @ {: calls:ptr calln:n :}
   HOST-ADDRS-N @ 0= if NULL-PTR else 0 HOST-ADDRS then  HOST-ADDRS-N @ {: addrs:ptr addrn:n :}
   does if
      copy s recorded calls calln addrs addrn false 0 off NPUB:IMPORT-PENDING-DOES
   else
      tag 5 <> if
         copy s calls calln addrs addrn false 0  tag x y z RESOLVE  NPUB:IMPORT-PENDING-DEFER
      else
         copy s recorded calls calln addrs addrn false 0 NPUB:IMPORT-PENDING
      then
   then
   want <> if E-COMP-STATE throw then
   OUTER:DEF-CLOSE
   \ After publication, as PENDING-CK refuses IMM and INT on a pending row; it also restores the
   \ wide and min-in bits the imports were given false 0 for.
   flags want SET-FLAGS
   does if
      want 1 + XREF-REC {: rec:ptr :}
      rec XREF-NAME$ da du STR= 0=  rec XREF-WORDLIST dwid <>  or if E-COMP-STATE throw then
      dflags want 1 + SET-FLAGS
   then
   LINK-CLEAR ;


\ MAKE-DEF; a throw drops the open definition (DEF-ABORT) and an emission not yet filed, and passes on:
\ the build then fails, and its close drops the filed shadow emissions (NSHADOW:CLOSE), the records and
\ the code (FORGET-SPACE).
: LINK-DEF ( n -- )
   [: dup MAKE-DEF ;] catch {: i:n rc:n :}
   rc 0<> if NEMIT:CLEAR  NSHADOW:ABANDON  OUTER:DEF-ABORT  LINK-CLEAR  rc throw then ;

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


\ The object's DATA bytes at here, then its cells, strings and stores, after the file's
\ entries; the caller sets FILE-DP to here's distance from NULL-PTR first, as RUN-FILE does.
: LINK-DATA ( -- )
   LINK-CLEAR
   here NULL-PTR -  FILE-DP @ <> if E-COMP-STATE throw then
   DATA-AT @ OBJ@ {: d:n :}
   d allot
   OBJECT BYTES$ drop DATA-AT @ 8 + +  NULL-PTR BYTE-VIEW FILE-DP @ +  d BYTE-COPY
   CELLS-AT @  OBJECT BYTES$ nip  RD-OPEN
   RD-U64 0 ?do
      RD-U64 RD-U64 {: off:n kind:n :}
      RD-REF {: tag:n x:n y:n z:n :}
      GOOD? 0= if E-COMP-STATE throw then
      tag x y z RESOLVE  kind  FILE-DP @ off +  PUT-CELL
   loop
   GOOD? 0= if E-COMP-STATE throw then
   STRS-AT @  OBJECT BYTES$ nip  RD-OPEN
   RD-U64 0 ?do
      RD-U64 {: off:n :}
      RD-U64 drop
      GOOD? 0= if E-COMP-STATE throw then
      FILE-DP @ off + INTERN:ADOPT
   loop
   GOOD? 0= if E-COMP-STATE throw then
   STORES-AT @  OBJECT BYTES$ nip  RD-OPEN
   RD-U64 0 ?do
      RD-REF {: ctag:n cx:n cy:n cz:n :}
      RD-U64 RD-U64 {: kind:n v:n :}
      RD-REF {: tag:n x:n y:n z:n :}
      GOOD? 0= if E-COMP-STATE throw then
      ctag cx cy cz RESOLVE {: at:n :}
      kind 3 = if v else tag x y z RESOLVE then  kind  at  PUT-CELL
      at HSTORE+  at STORER+
   loop
   GOOD? 0= if E-COMP-STATE throw then ;


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


\ The read object's entries in their order, then its data.
: LINK-OBJECT ( -- )
   LINK-CLEAR
   0 ALIASES-N !
   ENTRIES-N @ 0 ?do i LINK-ENTRY loop
   LINK-DATA ;


\ The BUILT row REFS' FILES row k, from 1, names: a cold build places from the object it wrote,
\ whose rows no FILES-CK mapped.
: REFS-ROW ( n -- n )
   {: k:n :}
   REFS BYTES$ drop  k 1 - FILES-ROW-BYTES * + {: r:ptr :}
   BUILT-N 0 ?do
      r FILES-ROW-BYTES  i BUILT-AT FILES-ROW-BYTES  STR= if i unloop exit then
   loop
   E-COMP-STATE throw ;

\ A facts row's reference to an earlier piece's record, file not 0, as a dictionary index: a
\ link's through FILE-ROWS (REC-INDEX), a compile's through REFS (REFS-ROW).
: EARLIER-REC ( bool n n -- n )
   {: read:bool file:n idx:n :}
   read if file idx REC-INDEX else  file REFS-ROW BUILT-AT BUILT-FIRST + LE:U64@ idx +  then ;

\ A retirement row's record retired again (XREF:RETIRE), as the file's compile retired it. An
\ earlier piece's record is changed once retired, so a compile and a link see one wordlist.
: LINK-RETIRED ( bool -- )
   {: read:bool :}
   RD-U64 RD-U64 {: file:n idx:n :}
   GOOD? 0= if E-COMP-STATE throw then
   read if file idx REC-INDEX XREF-REC XREF:RETIRE then
   file 0<> if read file idx EARLIER-REC CHANGER+ then ;

\ An earlier file's record's text, written again by a `TRUST` row or an amender, held again for
\ that record and read back.
: LINK-EARLIER ( bool -- )
   {: read:bool :}
   RD-U64 RD-U64 {: file:n idx:n :}
   RD-NAME {: a:ptr u:n :}
   GOOD? 0=  file 0=  or if E-COMP-STATE throw then
   read file idx EARLIER-REC {: r:n :}
   r CHANGER+
   a u r CHECK:FACTS!
   read if a u r CHECK:READ-FACTS then ;

\ OBJECT's FACTS held again, then placed at here with the rest the file held: a cold build
\ places from the object it wrote, a warm one from the object it read, so both lay one text.
\ A link also replays each `undefine` and retirement in the compile's order, and reads back
\ into the checker an earlier file's record's text a later `TRUST` row or amender rewrote; a record's own
\ text is read when a lookup first binds it (the checker's READ-BOUND). A cold build applied
\ them as it held them.
: LINK-FACTS ( bool -- )
   {: read:bool :}
   FACTS-AT @  OBJECT BYTES$ nip  RD-OPEN
   RD-U64 0 ?do
      RD-U64 {: ix:n :}
      ix CHECK:HELD-RETIRED = if read LINK-RETIRED else
      ix CHECK:HELD-AMENDED = if read LINK-EARLIER else
         RD-NAME {: a:ptr u:n :}
         GOOD? 0= if E-COMP-STATE throw then
         ix CHECK:HELD-LAID = if a u CHECK:LAY-FACTS then
         ix CHECK:HELD-UNDEFINE =  read  and if a u CHECK:READ-UNDEFINE then
         ix 0 >= if a u FILE-REC @ ix + CHECK:FACTS! then
      then then
   loop
   GOOD? 0= if E-COMP-STATE throw then
   FILE-HELD @ CHECK:PLACE-FACTS ;

\ ---- the open ------------------------------------------------------------------
\ Installed while IMAGE-BASE-CELL is 0, so its xt! stores reach hb's own cells.
\ Installing again adds no address-cell row: EMIT-MARK keeps one row per cell.
: INSTALL-LOOP ( -- )
   OUTER:INSTALL-EVALUATE
   OUTER:INSTALL-CREATE ;

\ The address-cell header the boot lays (habu2.f), so EMIT-MARK records the
\ space's address cells in the space.
: LAY-ADDRESS-HEADER ( -- )
   SNAP-RELOC:XTCELL-N-CELL {: h:n :}
   0 h SPACE-AT !
   ADDRESS-CELLS:MAGIC h ADDRESS-CELLS:MAGIC-FIELD + SPACE-AT !
   ADDRESS-CELLS:BOOT-OFF h ADDRESS-CELLS:BASE-FIELD + SPACE-AT !
   ADDRESS-CELLS:BOOT-CAP h ADDRESS-CELLS:CAP-FIELD + SPACE-AT !
   0 h ADDRESS-CELLS:MODE-FIELD + SPACE-AT ! ;

: MAP ( -- )
   LAYOUT-SIZE {: size:n :}
   HINT size MEM-PROT-RW MEM-MAP-PRIVATE-ANON NORESERVE or
   MEM-ANON-FD MEM-OFF-ZERO mmap {: at:n :}
   at 0 < if E-MEM-MAP throw then
   at BASE !  size SPACE-SIZE !
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
\ which a compiling file's object carries (ACTS+), and OUTER logs each word run (RAN+).
: REQUIRE-FRESH ( -- )
   REQUIRE-SNAPSHOT  EVENTS-RESET EVENT-ON  OUTER:RECORD-RUNS
   construct step required PROGRESS ! ;

\ `[: execute ;]` here is E-EXEC-OPAQUE-XT; a quotation parameter is typed.
: RUN-UNIT ( n n [ n n -- ] -- ) execute ;

\ A load may start: a top-level one while the build is open; a nested one, or a compiling file's
\ tail, while its loader's load runs. E-FAILED once a load threw or DP moved back in the space
\ (failed): a failed build loads and writes nothing more. E-COMP-STATE in any other step.
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
            construct step failed PROGRESS !  E-FAILED throw
         then
      ENDOF
      loading OF nested 0= if E-COMP-STATE throw then ENDOF
      returned OF E-COMP-STATE throw ENDOF
      failed OF E-FAILED throw ENDOF
   ;MATCH ;


\ The checker reads the file's aliases after its records' texts, which their sources may be.
: READ-ALIASES ( -- )
   ALIASES-N @ 0 ?do  i ALIASES @  i 1 + ALIASES @  CHECK:READ-ALIAS  2 +loop ;


\ As a file's load or its piece after a request starts.
: MARKS ( -- )
   ndict@ FILE-REC !  CHECK:HELD-FACTS FILE-HELD !  0 RETIRED-N !
   TFAM:TFAM-N@ FILE-TFAM !  CHECK:TYPE-END FILE-TYPES !
   NSHADOW:RECORDS FILE-ROW !  NSHADOW:EMISSIONS FILE-EM !
   here NULL-PTR - FILE-DP !
   WIDN-CELL HOST@ FILE-WID !     \ hb's DATA, where namespace-record and namespace-private mint ids
   EVENT-COUNT FILE-EVENT !  OUTER:RUN-COUNT FILE-RUN ! ;


\ The scope a build's top-level file starts in: no package or part open, the build's global
\ wordlist current (MOVE-IN) and no `using` open. A compile's records, sections and lookups
\ follow the scope, which an object does not record, and a link opens no package: so a file
\ links only when its load starts in this scope, and a file with a piece that starts or ends
\ in another (a package it leaves open, a request inside a package) is UNLINKABLE.
: TOP-SCOPE? ( -- bool )
   PKG-REC-CELL HOST@ 0=  get-current BOOT:GLOBAL-WL =  and  USE-DEPTH-CELL HOST@ 0=  and ;

: SCOPE-CK ( -- ) TOP-SCOPE? 0= if true UNLINKABLE ! then ;


\ The header's last pair: PIECES' offset, then its length.
HEADER-BYTES 16 - constant PIECES-FIELD

\ After a linked request's load, whether it returned or threw: the linking file's key and object.
: RETURNED ( -- ) POP-KEY UNPARK ;


\ REQUESTS row k of the linking file: the path it spells loads again, as the compile's request
\ loaded it, under the linking file's key, so the requested file links; it must end with the BUILT
\ row of the FILES row the request names, else E-STALE.
: LINK-REQUEST ( n -- )
   REQ-FIELDS {: kind:n spelled:n loaded:n file:n :}
   REQ-PATH BUF:CLEAR
   spelled FIELD$ REQ-PATH SPAN+       \ OBJECT is the requested file's while it loads
   PARK
   true PUSH-KEY
   kind REQ-PATH BYTES$ ['] SOURCE-UNIT:LOAD-PATH ['] RETURNED finally
   file FILES-ROW BUILT-MATCH {: row:n hit:bool :}
   hit 0=  row BUILT-N 1 - <>  or if E-STALE throw then ;


\ The piece's ACTS rows (ACTS-CK) applied as its compile made them: a provide through
\ `provided`; a require the registry must hold by now, else E-STALE.
: LINK-ACTS ( -- )
   ACTS-AT @  OBJECT BYTES$ nip  RD-OPEN
   RD-U64 0 ?do
      RD-U64 {: kind:n :}
      RD-NAME {: a:ptr u:n :}
      GOOD? 0= if E-COMP-STATE throw then
      kind EV-PROVIDED = if
         a u provided
      else
         a u SOURCE-ROOT:RESOLVE nip nip 0= if E-STALE throw then
      then
   loop ;


\ The checked piece's USES (USES-CK) onto USES as BUILT rows; E-COMP-STATE for a row no BUILT row
\ holds.
: USES-IN ( -- )
   USES-AT @ OBJ@ {: n:n :}
   n 0 ?do
      USES-AT @ 8 + i 8 * + OBJ@ FILE-ROWS @ {: row:n :}
      row 0< if E-COMP-STATE throw then
      row USE+
   loop ;


\ Linked piece k's word times, from OBJECT's TIMES (PIECE-CK), into WORD-TIMES by its records.
: LINKED-TIMES ( n -- )
   TIMES-AT {: at:n :}
   at OBJ@ {: n:n :}
   FILE-REC @ n + WORD-TIMES-RESERVE
   n 0 ?do  at 8 +  i WORD-BYTES * +  OBJ@  FILE-REC @ i +  WORD-TIMES !  loop ;


\ The fresh object linked piece by piece: checked (PIECE-CK), linked with its acts and word times, added
\ to BUILT with its texts, references, number and time, then its request's file linked (LINK-REQUEST).
: LINK-FILE ( -- )
   PIECES-FIELD OBJ@ 8 + PIECE-AT !
   REQS-N @ 1 + 0 ?do
      i 0 > if MARKS then
      i REQS-N @ < if i else -1 then {: req:n :}
      MAP-FILES
      req PIECE-CK
      GOOD? 0= if E-STALE throw then
      LINK-OBJECT  i LINKED-TIMES  true LINK-FACTS  READ-ALIASES  LINK-ACTS
      here NULL-PTR - FILE-END !
      USES-IN
      req PIECE-N  req PIECE-TIME  BUILT+
      req 0 >= if req LINK-REQUEST then
   loop ;


\ FILE-KEY's object removed, so the next build compiles its file.
: DROP-OBJECT ( -- ) OBJECT$ FS:REMOVE-FILE ;


\ After INPUT!: true once the file's object was fresh and linked. A throw out of the check or the
\ link removes the object before it passes, so the next build compiles the file instead of failing
\ the same way.
: TRY-LINK ( bool -- bool )
   drop FRESH? dup if LINK-FILE then ;

: LINKED? ( -- bool )
   false ['] TRY-LINK catch {: linked rc:n :}
   rc 0<> if DROP-OBJECT rc throw then
   linked ;


\ A type entry for each family and linear type the piece declared, none for a linear type
\ its compile read from an entry; its name sits at CP, so `here` stays.
: TYPE-ENTRIES ( -- )
   TFAM:TFAM-N@ FILE-TFAM @ ?do
      i TFAM:ENTRY? 0= if
         ndict@ {: ix:n :}
         i TFAM:ENTRY-NAME$ TYPE-SOURCE BOOT:TYPES-WL OUTER:ALIAS-RECORD
         i TFAM:ENTRY-TEXT$ ix CHECK:FACTS!
         i ix TFAM:ENTRY!
      then
   loop
   CHECK:TYPE-END FILE-TYPES @ ?do
      i CHECK:LINEAR? if
         i CHECK:TYPE-NAME$ CHECK:LINEAR-ENTRY 0 < if
            ndict@ {: ix:n :}
            i CHECK:TYPE-NAME$ TYPE-SOURCE BOOT:TYPES-WL OUTER:ALIAS-RECORD
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


\ The piece's checks, RESET-SCRATCH, then PIECES (PIECE+): E-FAILED once DP moved back, E-LIVE-TASK
\ for a task left running, E-STORE (CHECK-EARLIER, FIND-STORES), E-WORDLIST for an id no record holds.
: PIECE-END ( n -- )
   {: req:n :}
   DPHW-CELL HOST@ 0<> if E-FAILED throw then
   TASKS-LIVE-CELL HOST@ OPEN-TASKS @ > if
      REFUSAL s" a task is still running at the file's end" SAY SAY-CR  E-LIVE-TASK throw
   then
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


\ The piece's texts placed from OBJECT, as a link places them, then its BUILT row with its number
\ and time.
: PIECE-PLACE ( n n -- )
   {: piece:n t:n :}
   FILE-HELD @ CHECK:DROP-FACTS  false LINK-FACTS
   here NULL-PTR - FILE-END !          \ the piece's span ends after its texts
   piece t BUILT+ ;


\ The compiling file's object so far onto FRAMES while a file it requested loads, which reuses its
\ buffers; FRAME-TAIL bytes end each frame, so POP-FRAME finds the lengths from FRAMES' end.
10 8 * constant FRAME-TAIL

: SAVE+ ( ptr a -- n )
   {: buf:ptr :}
   buf BYTES$ {: a:ptr u:n :}
   a u FRAMES SPAN+  u ;

: PUSH-FRAME ( -- )
   PIECES SAVE+  REFS SAVE+  REQUESTS SAVE+  NAMED-ROWS SAVE+  SPELLS SAVE+  RAN-ROWS SAVE+  TIMES SAVE+
   {: p:n r:n rq:n m:n s:n ran:n tm:n :}
   INPUT-STAMP INPUT-BYTES FRAMES SPAN+
   FILE-PATH FILE-PATH-N @ FRAMES SPAN+
   p FRAMES U64+  r FRAMES U64+  rq FRAMES U64+  m FRAMES U64+  s FRAMES U64+  ran FRAMES U64+
   tm FRAMES U64+  FILE-PATH-N @ FRAMES U64+  REQ-N @ FRAMES U64+
   UNLINKABLE @ if 1 else 0 then FRAMES U64+ ;


\ FRAMES' u bytes from at into the buffer, and the offset after them.
: RESTORE+ ( n n ptr a -- n )
   {: at:n u:n buf:ptr :}
   FRAMES BYTES$ drop at +  u BUF:N>BLEN  buf BUF:REPLACE
   at u + ;


\ The top frame back into the compiling file's buffers (PUSH-FRAME), and off FRAMES.
: POP-FRAME ( -- )
   FRAMES BYTES$ {: f:ptr fu:n :}
   f fu + FRAME-TAIL - {: t:ptr :}
   t LE:U64@  t 8 + LE:U64@  t 16 + LE:U64@  t 24 + LE:U64@ {: p:n r:n rq:n m:n :}
   t 32 + LE:U64@  t 40 + LE:U64@  t 48 + LE:U64@ {: s:n ran:n tm:n :}
   t 56 + LE:U64@  t 64 + LE:U64@  t 72 + LE:U64@ {: pn:n reqs:n unlinkable:n :}
   fu FRAME-TAIL -  p r + rq + m + s + ran + tm + INPUT-BYTES + pn + - {: start:n :}
   start p PIECES RESTORE+  r REFS RESTORE+  rq REQUESTS RESTORE+  m NAMED-ROWS RESTORE+
   s SPELLS RESTORE+  ran RAN-ROWS RESTORE+  tm TIMES RESTORE+ {: at:n :}
   f at + INPUT-STAMP INPUT-BYTES BYTE-COPY
   f at + INPUT-BYTES + FILE-PATH pn BYTE-COPY  pn FILE-PATH-N !
   reqs REQ-N !  unlinkable 0<> UNLINKABLE !
   f start BUF:N>BLEN FRAMES BUF:REPLACE ;


\ The file's load: its key and marks, then a link of its fresh object when it starts in TOP-SCOPE?,
\ else a compile of its source, which NEST cuts into pieces at each request, ending with its last
\ piece and its object. A file a linking file requested links or ends the build with E-STALE: no
\ piece compiles after a linked one. A cross build links no object and writes none: an object
\ carries one emission per definition, and a cross build makes two, the target's and the host's
\ that later files run.
: LOAD ( ptr u8 n ptr u8 n ptr u8 n [ n n -- ] -- )
   {: path:ptr pathu:n root:ptr rootu:n source:ptr sourceu:n q :}
   path pathu KEY!
   path pathu PATH!
   path pathu root rootu PATH+
   MARKS
   INPUT!
   TOP-SCOPE? CROSS? 0= and if LINKED? if 1 LINKED-N +! exit then then
   LINKING? if E-STALE throw then
   HITS-CLEAR  source sourceu SPELLED  NAMED!   \ a failed check left its replay in HITS
   OBJECT-OPEN  COPY-BELOW
   CROSS? UNLINKABLE !  STAMPED? 0= if true UNLINKABLE ! then  SCOPE-CK
   false PUSH-KEY
   0 sourceu q ['] RUN-UNIT ['] POP-KEY finally
   true GOING                          \ E-FAILED: a request failed and the source caught it
   SCOPE-CK
   PIECES BYTES$ nip {: start:n :}
   -1 PIECE-END
   1 COMPILED-N +!
   start REQ-N @ COMPILED-TIME {: t:n :}
   t WRITE-OBJECT
   REQ-N @ t PIECE-PLACE ;


\ Whether event ev of the loader's log is the request that loads path: the piece's, an include or a
\ require the registry did not hold, for that path.
: REQUEST? ( n ptr u8 n -- bool )
   {: ev:n path:ptr pathu:n :}
   ev FILE-EVENT @ < if false exit then
   ev EVENT-KIND@ {: kind:n :}
   kind EV-INCLUDED =
   kind EV-REQUIRED =  ev EVENT-STATE@ EV-STATE-FRESH =  and  or
   ev EVENT-PATH@ path pathu STR=  and ;


\ A request of the compiling file, the newest event of the loader's log (REQUEST?, else
\ E-COMP-STATE): its piece ends here, placed with its number and its time (COMPILED-TIME), which
\ starts the request's row with the piece's length, its kind and the path as spelled and as
\ loaded; its object so far waits on FRAMES while the file loads; then the row ends with the
\ FILES row of that file's last piece, and the next piece starts at a cell boundary, as a load
\ leaves here (include.f LOAD-UNIT).
: NEST ( ptr u8 n ptr u8 n ptr u8 n [ n n -- ] -- )
   {: path:ptr pathu:n root:ptr rootu:n source:ptr sourceu:n q :}
   EVENT-COUNT 1 - {: ev:n :}
   ev path pathu REQUEST? 0= if E-COMP-STATE throw then
   ev EVENT-KIND@ EV-INCLUDED = if SOURCE-UNIT:LOAD-INCLUDE else SOURCE-UNIT:LOAD-REQUIRE then {: mode:n :}
   SCOPE-CK
   PIECES BYTES$ nip {: start:n :}
   REQ-N @ PIECE-END
   PIECE-LAY  start REQ-N @ COMPILED-TIME {: t:n :}
   PIECES BYTES$ nip start - {: size:n :}
   REQ-N @ t PIECE-PLACE
   t REQUESTS U64+  size REQUESTS U64+  mode REQUESTS U64+
   ev EVENT-SPELLED@ REQUESTS FIELD+  path pathu REQUESTS FIELD+
   1 REQ-N +!
   align
   PUSH-FRAME
   path pathu root rootu source sourceu q LOAD
   POP-FRAME
   BUILT-N 1 - REF-FILE REQUESTS U64+
   LOADER$ {: k:ptr ku:n :}
   k FILE-KEY ku BYTE-COPY
   align
   MARKS  COPY-BELOW  SCOPE-CK ;


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


\ Links the file when its object is fresh, else compiles it, counting and writing a compile; a
\ request of a compiling file nests (NEST).
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
\ package the open made (packages.f PKG-ENSURE).

\ hb's own record of owned primitive row k in its package's private wordlist, which an hb a build
\ wrote holds; else NULL-PTR.
: HB-OWNED ( n -- ptr n )
   {: k:n :}
   k PRIM-SPEC:PKG$ BOOT:NAMESPACE-WL CHECKER-RESOLVE:WL-PROBE {: pkg:ptr :}
   pkg XREF-FOUND? 0= if XREF-NULL exit then
   pkg XREF-PKG-PRIVATE {: wid:n :}
   wid 0= if XREF-NULL exit then
   k PRIM-SPEC:NAME$ wid CHECKER-RESOLVE:WL-PROBE ;

\ Row k, hb's kernel record of its primitive and the facts text hb's checker gives it, copied:
\ a rendered text lies in a buffer the next render reuses. A primitive no text says would leave
\ its package's calls unchecked in the program the build writes, so the open fails.
: OWNED+ ( n -- )
   {: k:n :}
   k PRIM-SPEC:NAME$ WL:PRIMITIVES CHECKER-RESOLVE:WL-PROBE {: src:ptr :}
   src XREF-FOUND? 0= if E-COMP-STATE throw then
   k HB-OWNED  k PRIM-SPEC:PKG$  k PRIM-SPEC:NAME$  CHECK:OWNED-FACTS {: a:ptr u:n :}
   u 0= if
      s" linker: no facts text says owned primitive " SAY
      k PRIM-SPEC:PKG$ SAY  s" :" SAY  k PRIM-SPEC:NAME$ SAY  SAY-CR
      E-COMP-STATE throw
   then
   OWNED-TEXT-N @ {: off:n :}
   off u + OWNED-TEXT-RESERVE
   a  off OWNED-TEXT  u BYTE-COPY
   off u + OWNED-TEXT-N !
   OWNED-N @ OWNED-CELLS * {: at:n :}
   at OWNED-CELLS + OWNED-RESERVE
   k at OWNED !
   src CHECKER-RESOLVE:REC>INDEX  at 1 + OWNED !
   off at 2 + OWNED !
   u at 3 + OWNED !
   1 OWNED-N +! ;

\ Before MOVE-IN, while hb's namespace and checker stores are current.
: OWNED-TEXTS ( -- )
   0 OWNED-N !  0 OWNED-TEXT-N !
   PRIM-SPEC:COUNT 0 ?do
      i PRIM-SPEC:KIND@ PRIM-SPEC:K-PKG-PRIVATE = if i OWNED+ then
   loop ;

: OWNED@ ( n n -- n ) {: r:n f:n :} r OWNED-CELLS * f + OWNED @ ;

\ Package a u's build record, made with its private wordlist when no earlier row made it.
: OWNER ( ptr u8 n -- ptr n )
   {: a:ptr u:n :}
   a u BOOT:NAMESPACE-WL CHECKER-RESOLVE:WL-PROBE {: pkg:ptr :}
   pkg XREF-FOUND? if pkg exit then
   a u true OUTER:PACKAGE-RECORD XREF-REC ;

\ Owned row r's record, published as MAKE-DEF publishes a definition's, with the kernel's code
\ and hb's record's immediate, wide and minimum-input bits, the ones alias-record keeps.
: MAKE-OWNED ( n -- )
   {: r:n :}
   r 0 OWNED@ {: k:n :}
   r 1 OWNED@ XREF-REC {: src:ptr :}
   k PRIM-SPEC:PKG$ OWNER XREF-PKG-PRIVATE {: wid:n :}
   ndict@ {: want:n :}
   k PRIM-SPEC:NAME$ wid 0 OUTER:DEF-RECORD
   src XREF-START  src XREF-RAW-LEN  NPUB:IMPORT-ENTRY  want <> if E-COMP-STATE throw then
   OUTER:DEF-CLOSE
   src XREF-FLAGS  DNAME-IMM DNAME-WIDE or DNAME-MIN-IN-MASK or  and  want SET-FLAGS
   r 2 OWNED@ OWNED-TEXT  r 3 OWNED@  want CHECK:FACTS! ;

\ MAKE-OWNED; a throw drops the open definition and passes on, failing the open.
: LINK-OWNED ( n -- )
   [: dup MAKE-OWNED ;] catch {: r:n rc:n :}
   rc 0<> if OUTER:DEF-ABORT rc throw then ;

\ After IMAGE-IN, so the texts are held, then laid before any file as the open's header data.
\ The records are BUILT row 0, as a file's piece is, so an object names them by that row: its
\ key is the empty path's, which no file's load has; it is piece 0 at time 0, since what it holds
\ follows from hb, whose stamp names the objects' directory (BUILD-OUT:OBJ$); it has no emission
\ and no data.
: OWNED-RECORDS ( -- )
   FIRST-REC @ FILE-REC !  NSHADOW:EMISSIONS FILE-EM !
   OWNED-N @ 0 ?do i LINK-OWNED loop
   0 CHECK:PLACE-FACTS
   s" " KEY-PATHS PATH-IN FILE-KEY LE:U64!  0 FILE-KEY 8 + LE:U64!
   FILE-KEY KEY-BYTES LOAD-PATHS SPAN+  s" " LOAD-PATHS FIELD+
   here NULL-PTR - dup FILE-DP !  FILE-END !
   0 0 BUILT+ ;

: OPEN ( -- )
   0 COMPILED-N !  0 LINKED-N !
   INSTALL-LOOP
   OWNED-TEXTS
   MAP SHADOW MOVE-IN CHECK-IN REQUIRE-FRESH OBJECTS-IN ROUTE IMAGE-IN
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
         OUTER:;RECORD-RUNS  EVENT-OFF EVENTS-RESET REQUIRE-RESTORE construct step checked PROGRESS ! true
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
   [: ['] RUN ['] CLOSE finally ;] OUTER:WITHOUT-PACKAGE ;

\ The kernel object at the path, written by a kernel build: the engine and
\ program builds that follow lay their space as its kernel reads it and link it.
: KERNEL ( ptr u8 n -- ) ARM64:READ ;

\ True while a build's space is open, from WITH's open to the end of its close.
: OPEN? ( -- bool ) IDLE? 0= ;

\ The files the last build compiled and linked, from its open until the next open.
: COMPILED ( -- n ) COMPILED-N @ ;
: LINKED ( -- n ) LINKED-N @ ;

;package
