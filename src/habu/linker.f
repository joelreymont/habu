\ linker.f - the cross-build's data space: WITH maps a space beside hb's DATA,
\ points DP, the wordlists and the image base at it, switches the checker's
\ registered stores to the build's (CHECK:OPEN), routes every file load to the
\ build's runner (SOURCE-UNIT:USE), runs the body there with no package of
\ hb's open and closes it. The close undoes exactly the open steps that ran,
\ in reverse. A build writes <out>/loads, the files whose load started, in
\ order; the next build's first file load checks each listed file's object
\ (WALK) and links every file when all are fresh, else compiles every file,
\ writing its object under the out root. A file stale after a linked one ends
\ the build (E-STALE).

require src/lib/prelude.f
require src/lib/error.f
require src/lib/string.f
require src/lib/memory.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/byte-buffer.f
require src/lib/le.f
require src/lib/sort.f
require src/lib/engine-id.f
require src/lib/build-out.f
require src/core/sha256.f
require src/habu/layout.f
require src/habu/boot-cells.f
require src/habu/xref.f
require src/habu/address-cells.f
require src/habu/interpret.f
require src/habu/definers.f
require src/habu/intern.f
require src/core/include.f
require src/compiler/native/abi.f
require src/compiler/native/shadow.f
require src/compiler/native/publish.f
require src/habu/link-band.f

\ The linker's failed build: -9690..-9699, minted here, beside the package its
\ codes belong to (docs/forth-card.md § 6).
-9690 constant E-LINKER-FIRST
-9699 constant E-LINKER-LAST

package LINKER
private

\ The open's progress, the last step completed. Past open, loading: a file's load runs, from its key
\ to its BUILT row, and a throw out of it stays there; failed: DP moved back, E-STORE, E-WORDLIST or E-STALE.
ENUM step none mapped shadowed moved checked required objects routed open loading failed ;ENUM
TYPED-VARIABLE PROGRESS step

\ How files load (LINKS?): unread, WALK has not run; open, no file decided yet; compiling, every
\ file compiles; linking, a file linked; stale, a file was stale after a link (E-STALE).
ENUM link-mode unread open compiling linking stale ;ENUM
TYPED-VARIABLE SO-FAR link-mode

public
-9690 s" a file of this build failed or DP moved back in its space: the build loads no other file and writes no object or program" ERROR:CODE E-FAILED
-9691 s" a file of this build changed an earlier record or code, or stored into a cell no file holds" ERROR:CODE E-STORE
-9692 s" a file of this build has no fresh object after a linked file; the next build compiles every file" ERROR:CODE E-STALE
-9693 s" a file of this build made a wordlist no package of it holds" ERROR:CODE E-WORDLIST
private

TYPED-VARIABLE ENGINE? bool          \ the mode is "engine"
variable BASE                        \ the space's address, its distance from NULL-PTR
variable FIRST-REC                   \ ndict@ at the open: records from here are the space's
variable OPEN-DP                     \ here's distance once open; header data is [BASE, OPEN-DP)
TYPED-VARIABLE BODY [ -- ]
variable COMPILED-N                  \ files the last build compiled
TYPED-VARIABLE PRIOR-RUNNER [ ptr u8 n ptr u8 n ptr u8 n [ n n -- ] -- ]

\ The host cells a build changes; RESTORE puts them back.
variable SAVED-DP
variable SAVED-FLOOR
variable SAVED-CEILING
variable SAVED-DPHW
variable SAVED-DATA-FLOOR
variable SAVED-GLOBAL-WL
variable SAVED-NAMESPACE-WL
variable SAVED-COMMA-RUN
variable SAVED-COMMA-END
variable SAVED-COMMA-BASE
variable SAVED-LASTC
variable SAVED-CURRENT
variable SAVED-SIG-POOL
variable SAVED-SIG-LEN
variable SAVED-FLAGS                 \ hb's newest record's flags (NEWEST-FLAGS)

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
   NULL-PTR BYTE-VIEW BASE @ + DATA-SIZE munmap
   0<> if E-MEM-UNMAP throw then ;

\ Above hb's DATA: below it CHECKER-OWNER-GUARD refuses with E-COMP-OWNER.
: HINT ( -- n ) DATA-N DATA-SIZE + ;

\ ---- the object ----------------------------------------------------------------
\ LOADERS stacks the key of each file whose compile is running; LOADS holds a
\ row per load: the loader's key, zeros for the build's body, then the SHA-256
\ of the path under its root. BUILT holds a row per file compiled or linked: its
\ key, its first and end record index, its object's SHA-256, its first and end
\ emission, and the data pointer as its load starts and returns.
32 constant KEY-BYTES
KEY-BYTES 2 * constant ROW-BYTES
ROW-BYTES 48 + constant BUILT-BYTES
create LOADERS BUF:HDR-BYTES allot
create LOADS BUF:HDR-BYTES allot
create STARTED BUF:HDR-BYTES allot   \ a row per started load, in start order: its key, its source's SHA-256
create BUILT BUF:HDR-BYTES allot
create LOAD-LIST BUF:HDR-BYTES allot \ <out>/loads: HABULOAD, then per started load, in start order,
                                     \ its key's KEY-BYTES bytes, then its path and root, each a FIELD+
create WALK-SRC BUF:HDR-BYTES allot  \ a listed file's source as WALK reads it
create REFS BUF:HDR-BYTES allot      \ the object's FILES rows
create PIECES BUF:HDR-BYTES allot    \ the object's PIECES
create OBJECT BUF:HDR-BYTES allot    \ a key's fields, then an object's bytes
create CODE-COPY BUF:HDR-BYTES allot \ an emission's bytes as the link fills and places them
create LATE BUF:HDR-BYTES allot      \ the file's late private wordlists
SHA256-CTX-BYTES BUFFER: SHA-CTX
KEY-BYTES BUFFER: FILE-KEY
ROW-BYTES BUFFER: ROW
KEY-BYTES BUFFER: INPUT-SUM
KEY-BYTES BUFFER: PREFIX-SUM
KEY-BYTES BUFFER: OBJECT-SUM         \ the object's SHA-256 (BUILT+)
KEY-BYTES BUFFER: CK-SUM             \ the SHA-256 of the object past its checksum (HEADER-CK)
KEY-BYTES 2 * BUFFER: KEY-HEX
DYNAMIC-BUFFER ENTRY-OFFS n          \ a read object's entries' offsets in OBJECT (FRESH?)
DYNAMIC-BUFFER EM-OFFS n             \ its emissions' offsets in OBJECT, each at its S
DYNAMIC-BUFFER FILE-ROWS n           \ its FILES row k, from 1, as that file's BUILT row index
DYNAMIC-BUFFER DOES-AT n             \ its KIND-DOES entries' emission fields' offsets in OBJECT
DYNAMIC-BUFFER HOST-CALLS n          \ the host copy's call sites out of hb's region
variable HOST-CALLS-N
DYNAMIC-BUFFER HOST-ADDRS n          \ its data address sites
variable HOST-ADDRS-N

\ As a file's compile starts (COPY-BELOW): the space's cells below its data, the records
\ [FIRST-REC - 1, FILE-REC), and each emission's size, patch site and bytes.
create OLD-SPACE BUF:HDR-BYTES allot
create OLD-RECS BUF:HDR-BYTES allot
create OLD-EMS BUF:HDR-BYTES allot
DYNAMIC-BUFFER STORED n              \ the stores' cells as distances from NULL-PTR (FIND-STORES)
variable STORED-N
DYNAMIC-BUFFER STRS n                \ the file's string bodies as distances from NULL-PTR (DATA+)
variable STRS-N
DYNAMIC-BUFFER COPY-CELLS n          \ the image copy WRITE patches (link-data.f), zero-padded cells
DYNAMIC-BUFFER LISTED n              \ the DATA list's offsets, sorted before WRITE lays them (link-data.f)
variable LISTED-N

\ ndict@, NSHADOW:RECORDS, NSHADOW:EMISSIONS, here's distance from NULL-PTR and WIDN-CELL
\ as a file's load starts; RUN-FILE refuses a nested file, whose marks would overwrite them.
variable FILE-REC
variable FILE-ROW
variable FILE-EM
variable FILE-DP
variable FILE-WID                    \ the first wordlist id the file's compile can mint
variable FILE-END                    \ here's distance as the file's load returns
variable FILE-CELLS                  \ the space's address-cell rows as the file's compile starts

\ The magic, the key, the SHA-256 of every byte after this checksum, and an offset
\ from the file's start and a length for each of INPUT FILES REQUESTS NAMES PIECES,
\ 0 0 while empty; then the sections in the order INPUT FILES PIECES. Every number
\ is eight bytes, low byte first.
8 KEY-BYTES 2 * + 5 2 * 8 * + constant HEADER-BYTES
8 KEY-BYTES 2 * + constant INPUT-BYTES   \ the source's size, its SHA-256, then the prefix (START-SUMS)

\ A record's kind in PIECES.
0 constant KIND-DEF
1 constant KIND-PACKAGE
2 constant KIND-ALIAS
3 constant KIND-DOES                 \ a `does>` definer's `;does` record
4 constant KIND-PRIVATE              \ a package gaining its private wordlist after its record

: DISPOSE-OBJECTS ( -- )
   LISTED-RELEASE  COPY-CELLS-RELEASE  STRS-RELEASE  STORED-RELEASE
   ENTRY-OFFS-RELEASE  EM-OFFS-RELEASE  FILE-ROWS-RELEASE  DOES-AT-RELEASE
   HOST-CALLS-RELEASE  HOST-ADDRS-RELEASE
   CODE-COPY BUF:DISPOSE  OLD-EMS BUF:DISPOSE  OLD-RECS BUF:DISPOSE  OLD-SPACE BUF:DISPOSE
   LATE BUF:DISPOSE  OBJECT BUF:DISPOSE  PIECES BUF:DISPOSE  REFS BUF:DISPOSE  BUILT BUF:DISPOSE
   WALK-SRC BUF:DISPOSE  LOAD-LIST BUF:DISPOSE
   STARTED BUF:DISPOSE  LOADS BUF:DISPOSE  LOADERS BUF:DISPOSE ;

: INIT-BUFFERS ( -- )
   LOADERS 64 BUF:N>BLEN BUF:INIT
   LOADS 256 BUF:N>BLEN BUF:INIT
   STARTED 256 BUF:N>BLEN BUF:INIT
   BUILT 256 BUF:N>BLEN BUF:INIT
   LOAD-LIST 256 BUF:N>BLEN BUF:INIT
   WALK-SRC 256 BUF:N>BLEN BUF:INIT
   REFS 256 BUF:N>BLEN BUF:INIT
   PIECES 256 BUF:N>BLEN BUF:INIT
   OBJECT 256 BUF:N>BLEN BUF:INIT
   CODE-COPY 256 BUF:N>BLEN BUF:INIT
   LATE 256 BUF:N>BLEN BUF:INIT
   OLD-SPACE 256 BUF:N>BLEN BUF:INIT
   OLD-RECS 256 BUF:N>BLEN BUF:INIT
   OLD-EMS 256 BUF:N>BLEN BUF:INIT
   0 STORED-N !  0 HOST-CALLS-N !  0 HOST-ADDRS-N ! ;

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
   8 0 ?do v i 8 * rshift $FF and buf BUF:APPEND-BYTE loop ;

\ A key field: its length as a U64+, then its bytes.
: FIELD+ ( ptr u8 n ptr a -- ) {: a:ptr u:n buf:ptr :}
   u buf U64+  a u buf SPAN+ ;

\ The path with the root and the slash after it removed when it starts there.
: UNDER-ROOT ( ptr u8 n ptr u8 n -- ptr u8 n )
   {: path:ptr pathu:n root:ptr rootu:n :}
   pathu rootu > if
      path pathu root rootu STARTS-WITH?  path rootu + c@ FS:SLASH =  and if
         path rootu + 1 +  pathu rootu - 1 -  exit
      then
   then
   path pathu ;

\ The key of the file whose compile is running, empty for the build's body.
: LOADER$ ( -- ptr u8 n )
   LOADERS BYTES$ {: a:ptr u:n :}
   u 0= if a 0 exit then
   a u KEY-BYTES - +  KEY-BYTES ;

\ The loads already recorded with ROW's loader and path.
: SEEN ( -- n )
   LOADS BYTES$ {: rows:ptr rowsu:n :}
   0  rowsu ROW-BYTES / 0 ?do
      rows i ROW-BYTES * +  ROW-BYTES  ROW ROW-BYTES  STR= if 1 + then
   loop ;

\ FILE-KEY for a load of the path under its root, recorded in LOADS.
: KEY! ( ptr u8 n -- ) {: rel:ptr relu:n :}
   KEY-BYTES 0 ?do 0 ROW i + c! loop
   LOADER$ {: ld:ptr ldu:n :}
   ld ROW ldu BYTE-COPY
   SHA-CTX rel relu ROW KEY-BYTES + SHA256-IN
   SEEN {: n:n :}
   OBJECT BUF:CLEAR
   ENGINE-ID:KEY$ OBJECT FIELD+
   ENGINE? @ if s" engine" else s" app" then OBJECT FIELD+
   s" arm64" OBJECT FIELD+
   rel relu OBJECT FIELD+
   ld ldu OBJECT FIELD+
   8 OBJECT U64+  n OBJECT U64+
   SHA-CTX OBJECT BYTES$ FILE-KEY SHA256-IN
   ROW ROW-BYTES LOADS SPAN+ ;

: PUSH-KEY ( -- ) FILE-KEY KEY-BYTES LOADERS SPAN+ ;

\ The stack's top back into FILE-KEY, and off the stack.
: POP-KEY ( -- )
   LOADER$ {: top:ptr topu:n :}
   top FILE-KEY topu BYTE-COPY
   LOADERS BYTES$ KEY-BYTES - BUF:N>BLEN LOADERS BUF:REPLACE ;

\ After KEY! and before FRESH?: the sums HEADER-CK and WRITE-OBJECT read; the prefix covers
\ only loads started before this one.
: START-SUMS ( ptr u8 n -- )
   {: src:ptr srcu:n :}
   SHA-CTX src srcu INPUT-SUM SHA256-IN
   SHA-CTX STARTED BYTES$ PREFIX-SUM SHA256-IN
   FILE-KEY KEY-BYTES STARTED SPAN+
   INPUT-SUM KEY-BYTES STARTED SPAN+ ;

\ After KEY!, given the path and the root the runner received: FILE-KEY's row onto LOAD-LIST.
: LIST-ROW+ ( ptr u8 n ptr u8 n -- )
   {: path:ptr pathu:n root:ptr rootu:n :}
   FILE-KEY KEY-BYTES LOAD-LIST SPAN+
   path pathu LOAD-LIST FIELD+
   root rootu LOAD-LIST FIELD+ ;

\ ---- the records ---------------------------------------------------------------
\ No address reaches PIECES: a record is named by file and index, code and data by
\ span and offset, so larger earlier files leave its bytes alone (test/file-objects.f).

: BUILT-N ( -- n ) BUILT BYTES$ {: a:ptr u:n :}  u BUILT-BYTES / ;

: BUILT-AT ( n -- ptr u8 )
   {: row:n :}
   BUILT BYTES$ {: a:ptr u:n :}  a row BUILT-BYTES * + ;

: HOLDS? ( n n -- bool )
   {: idx:n row:n :}
   row BUILT-AT KEY-BYTES + {: at:ptr :}
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
      i BUILT-AT ROW-BYTES 16 + + {: at:ptr :}
      at LE:U64@ e <=  at 8 + LE:U64@ e >  and if i unloop exit then
   loop
   E-COMP-STATE throw ;

\ The BUILT row whose [first, end) data pointer holds v, and true; false when none.
: DP-ROW ( n -- n bool )
   {: v:n :}
   BUILT-N 0 ?do
      i BUILT-AT ROW-BYTES 32 + + {: at:ptr :}
      at LE:U64@ v <=  at 8 + LE:U64@ v >  and if i true unloop exit then
   loop
   0 false ;

\ The last BUILT row whose data end is v, and true; false when none.
: DP-END-ROW ( n -- n bool )
   {: v:n :}
   BUILT-N 0 ?do
      BUILT-N 1 - i - {: row:n :}
      row BUILT-AT ROW-BYTES 40 + + LE:U64@ v = if row true unloop exit then
   loop
   0 false ;

\ The FILES row naming the BUILT row's file, 1-based; added on first use.
: REF-FILE ( n -- n )
   {: row:n :}
   row BUILT-AT {: at:ptr :}
   REFS BYTES$ {: refs:ptr refsu:n :}
   refsu ROW-BYTES / {: n:n :}
   n 0 ?do
      refs i ROW-BYTES * + KEY-BYTES  at KEY-BYTES  STR= if i 1 + unloop exit then
   loop
   at KEY-BYTES REFS SPAN+
   at KEY-BYTES 16 + + KEY-BYTES REFS SPAN+
   n 1 + ;

\ A record reference: the file, 0 for this one, then the index in that file.
: REF+ ( n -- )
   {: idx:n :}
   idx FILE-REC @ >= if
      0 PIECES U64+  idx FILE-REC @ - PIECES U64+  exit
   then
   idx BUILT-ROW {: row:n :}
   row REF-FILE PIECES U64+
   idx  row BUILT-AT KEY-BYTES + LE:U64@ -  PIECES U64+ ;


\ A data address's reference: a file's data, or the open's header data. A file's end
\ names it only after every span, so a later file's first byte names that later file.
: DATA-REF+ ( n -- )
   {: v:n :}
   v FILE-DP @ >=  v FILE-END @ <  and if
      3 PIECES U64+  0 PIECES U64+  v FILE-DP @ - PIECES U64+ exit
   then
   v DP-ROW {: row:n hit:bool :}
   hit if
      3 PIECES U64+  row REF-FILE PIECES U64+
      v  row BUILT-AT ROW-BYTES 32 + + LE:U64@ -  PIECES U64+ exit
   then
   v BASE @ >=  v OPEN-DP @ <  and if 4 PIECES U64+  v BASE @ - PIECES U64+ exit then
   v FILE-END @ = if
      3 PIECES U64+  0 PIECES U64+  v FILE-DP @ - PIECES U64+ exit
   then
   v DP-END-ROW {: end-row:n end-hit:bool :}
   end-hit if
      3 PIECES U64+  end-row REF-FILE PIECES U64+
      v  end-row BUILT-AT ROW-BYTES 32 + + LE:U64@ -  PIECES U64+ exit
   then
   E-COMP-STATE throw ;


: PKG? ( n -- bool ) XREF-REC XREF-WORDLIST BOOT:NAMESPACE-WL = ;

\ A slot holding 0 holds no wordlist.
: PKG-HOLDS? ( n n n -- bool )
   {: idx:n wid:n slot:n :}
   wid 0= if false exit then
   idx PKG?  idx XREF-REC slot XREF-CELL@ wid =  and ;

\ A package record of this file whose private wordlist it minted right after its
\ public one: made with a private section, or given one with nothing minted between.
: OWN-PRIVATE? ( n -- bool )
   {: idx:n :}
   idx FILE-REC @ < if false exit then
   idx XREF-REC {: rec:ptr :}
   rec XREF-PKG-PRIVATE {: w:n :}
   w 0<>  w rec XREF-PKG-PUBLIC 1 + =  and ;

\ The wordlist's role: 0 global, 1 primitives, 2 package names, 3 or 4 a package's
\ public or private one, then the package: the build's nearest below (ids are unique).
: WORDLIST+ ( n -- )
   {: idx:n :}
   idx XREF-REC XREF-WORDLIST {: wid:n :}
   wid BOOT:GLOBAL-WL = if 0 PIECES U64+ exit then
   wid WL:PRIMITIVES = if 1 PIECES U64+ exit then
   wid BOOT:NAMESPACE-WL = if 2 PIECES U64+ exit then
   idx FIRST-REC @ ?do
      idx FIRST-REC @ + 1- i -
      dup wid XREF-START-SLOT PKG-HOLDS? if 3 PIECES U64+  REF+ unloop exit then
      dup wid XREF-LEN-SLOT PKG-HOLDS? if 4 PIECES U64+  REF+ unloop exit then
      drop
   loop
   E-COMP-STATE throw ;

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
   idx XREF-REC XREF-WORDLIST BOOT:NAMESPACE-WL = if KIND-PACKAGE exit then
   KIND-ALIAS ;

\ A definition's or does record's emission, numbered from the file's first.
: EMISSION+ ( n -- ) NSHADOW:EMISSION@ FILE-EM @ - PIECES U64+ ;

\ The record's dispatch cell, a defer's trailer cell, as a data reference; 5 for none.
: DISPATCH+ ( ptr n -- )
   NDICT:TRAILER-CELL {: cell:n :}
   cell 0= if 5 PIECES U64+ exit then
   cell DATA-REF+ ;

\ The kind, the name, the flags without the name length and EXT, the wordlist (WORDLIST+), then
\ the kind's fields (DISPATCH+, OWN-PRIVATE?); a KIND-PRIVATE entry is LATE+'s.
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
   idx ALIAS-SOURCE REF+ ;

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

\ The wordlist ids the file's records account for: a package record's public one,
\ its own private one (OWN-PRIVATE?), and each late private one (LATE?).
: MINTED ( -- n )
   0
   ndict@ FILE-REC @ ?do
      i PKG? if
         1 +
         i OWN-PRIVATE? if 1 + then
      then
   loop
   ndict@ FIRST-REC @ ?do
      i LATE? if 1 + then
   loop ;

: LATE-COLLECT ( -- )
   ndict@ FIRST-REC @ ?do
      i LATE? if i XREF-REC XREF-PKG-PRIVATE i LATE-INSERT then
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
   XREF-REC {: rec:ptr :}
   rec XREF-WORDLIST BOOT:NAMESPACE-WL = if rec XREF-PKG-PUBLIC exit then
   rec XREF-WORDLIST 1 + ;

\ A code address's reference: a record's entry, a quotation's function, or hb's
\ own code, last since a record's host copy may lie inside hb's code range.
: CODE-REF+ ( n -- )
   {: a:n :}
   a REC-AT {: r:n :}
   r 0 >= if 0 PIECES U64+  r NSHADOW:RECORD@ REF+ exit then
   a NSHADOW:SOURCE-FUNCTION {: qe:n qf:n :}
   qe 0 >= if
      2 PIECES U64+
      qe FILE-EM @ >= if
         0 PIECES U64+  qe FILE-EM @ - PIECES U64+
      else
         qe EM-ROW {: row:n :}
         row REF-FILE PIECES U64+
         qe  row BUILT-AT ROW-BYTES 16 + + LE:U64@ -  PIECES U64+
      then
      qf PIECES U64+ exit
   then
   a ARM64:HOST-KERNEL? if 1 PIECES U64+  a rbase - PIECES U64+ exit then
   E-COMP-STATE throw ;


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
         e i NSHADOW:CALL-TARGET@ CODE-REF+
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
      kind NEMIT:ADDR-CODE = if
         0 PIECES U64+  v CODE-REF+
      else
         1 PIECES U64+  v DATA-REF+
      then
   loop
   p PIECES U64+
   p 0 >= if
      calls 0 > if calls 1 - else -1 then {: k:n :}
      k 0 >= if e k NSHADOW:PATCH-CALL? else false then if
         e k NSHADOW:CALL-TARGET@ CODE-REF+
      else
         5 PIECES U64+
      then
   then ;


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
\ field is 0 for this object, else its FILES row from 1 (REF-FILE).
: CODE+ ( -- )
   ENTRIES
   NSHADOW:EMISSIONS FILE-EM @ - PIECES U64+
   NSHADOW:EMISSIONS FILE-EM @ ?do i EMISSION-CODE+ loop ;

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
   row DATA-ROW? if v DATA-REF+ else v CODE-REF+ then ;

: BODY-U ( n -- n ) CELL - BASE @ - SPACE-AT @ ;

: STRING+ ( n -- )
   {: b:n :}
   b CELL - FILE-DP @ <  b FILE-END @ >  or if exit then
   b b BODY-U +  FILE-END @ > if exit then
   STRS-N @ 1 + STRS-RESERVE
   b STRS-N @ STRS !  1 STRS-N +! ;

\ DATA, STRINGS and STORES, after CODE:
\   D                      the bytes the file's compile laid, FILE-END - FILE-DP
\   D raw bytes            from FILE-DP, unpadded; each address cell's 8 bytes written as 0
\   K, then K cell rows    offset from FILE-DP, a multiple of 8 (else E-COMP-STATE), kind
\                          (ROW-KIND), reference (CELL-REF+); in the order of the space's
\                          address-cell rows, only cells inside the D bytes
\   N, then N string rows  a body's offset from FILE-DP, then its length; offsets ascending,
\                          only bodies whose length cell and bytes lie inside the D bytes
\   S, then S store rows   each cell below FILE-DP the compile stored into or gave an address-cell
\                          row, ascending by address: its reference (tag 3 with a FILES row, or
\                          4), kind (ROW-KIND, 3 for none), bytes (the cell's value for kind 3,
\                          else 0), its value's reference (CELL-REF+ for kinds 0-2, else 5)
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
\ emissions alone; REFUSE-STORE ends the build as E-FAILED does.
: REFUSE-STORE ( -- ) construct step failed PROGRESS !  E-STORE throw ;

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


: COPY-EM ( n -- )
   {: e:n :}
   e NSHADOW:SIZE {: s:n :}
   s OLD-EMS U64+  e NSHADOW:PATCH-SITE@ OLD-EMS U64+
   s 0 > if e NSHADOW:BYTES s OLD-EMS SPAN+ then ;


\ What the file's compile may change below its own data, as it starts.
: COPY-BELOW ( -- )
   OLD-SPACE BUF:CLEAR  OLD-RECS BUF:CLEAR  OLD-EMS BUF:CLEAR
   [: COPY-RANGE ;] EACH-RANGE
   FILE-REC @ FIRST-REC @ 1 - ?do i COPY-REC loop
   FILE-EM @ 0 ?do i COPY-EM loop
   ROWS nip FILE-CELLS ! ;


\ True when a cell of the record differs from its copy, other than a late private wordlist (LATE?).
: REC-CHANGED? ( ptr u8 n -- bool )
   {: old:ptr idx:n :}
   idx XREF-REC BYTE-VIEW {: rec:ptr :}
   DREC CELL / 0 ?do
      old i cells + CELL-VIEW @  rec i cells + CELL-VIEW @  <> if
         i XREF-LEN-SLOT =  idx LATE?  and 0= if true unloop exit then
      then
   loop
   false ;


\ The emission's entry in the copy, its length and whether the size, patch site or bytes differ.
: EM-CHANGED? ( ptr u8 n -- n bool )
   {: at:ptr e:n :}
   at LE:U64@ {: s:n :}
   s 16 +
   s e NSHADOW:SIZE <>  at 8 + LE:U64@ e NSHADOW:PATCH-SITE@ <>  or if true exit then
   s 0= if false exit then
   at 16 + s  e NSHADOW:BYTES s  STR= 0= ;


\ E-STORE when the compile changed an earlier record or emission.
: CHECK-EARLIER ( -- )
   OLD-RECS BYTES$ {: recs:ptr recsu:n :}
   recsu DREC / 0 ?do
      recs i DREC * +  FIRST-REC @ 1 - i +  REC-CHANGED? if REFUSE-STORE then
   loop
   OLD-EMS BYTES$ drop {: ems:ptr :}
   0  FILE-EM @ 0 ?do
      ems over +  i EM-CHANGED? if REFUSE-STORE then +
   loop drop ;


variable OLD-AT                      \ COMPARE-RANGE's offset in OLD-SPACE

: STORED+ ( n -- )
   {: v:n :}
   STORED-N @ 1 + STORED-RESERVE
   v STORED-N @ STORED !  1 STORED-N +! ;


\ Each cell of [from, to) that differs from the copy at OLD-AT, into STORED.
: COMPARE-RANGE ( n n -- )
   {: from:n to:n :}
   OLD-SPACE BYTES$ drop OLD-AT @ + {: old:ptr :}
   to from - CELL / 0 ?do
      old i cells + CELL-VIEW @  from i cells + SPACE-AT @  <> if
         BASE @ from + i cells + STORED+
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


\ STORED, ascending: each changed cell below FILE-DP and each new row's cell there, aligned (else
\ E-COMP-STATE, as in DATA+); E-STORE for a cell no earlier file or the header holds.
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
   STORED-N @ 0 ?do i STORED @ HELD? 0= if REFUSE-STORE then loop ;


\ The space's address-cell row of the cell at v, and true; false when it has none.
: ROW-OF ( n -- n bool )
   {: v:n :}
   ROWS {: rows:ptr count:n :}
   count 0 ?do
      rows i cells + @ {: row:n :}
      row ROW-AT v = if row true unloop exit then
   loop
   0 false ;


\ STORES, after STRINGS (DATA+).
: STORES+ ( -- )
   STORED-N @ PIECES U64+
   STORED-N @ 0 ?do
      i STORED @ {: v:n :}
      v DATA-REF+
      v ROW-OF {: row:n hit:bool :}
      hit if
         row ROW-KIND PIECES U64+  0 PIECES U64+  row CELL-REF+
      else
         3 PIECES U64+  v BASE @ - SPACE-AT @ PIECES U64+  5 PIECES U64+
      then
   loop ;


\ Piece count 1 (RUN-FILE); entry count; records by index, each late private wordlist (by id) before
\ the first record marked above its id, so ids mint in entry order; CODE; DATA; STRINGS; STORES; REQUEST -1.
: PIECES! ( -- )
   PIECES BUF:CLEAR  REFS BUF:CLEAR  LATE BUF:CLEAR
   FILE-ROW @ SHADOW-AT !
   LATE-COLLECT
   1 PIECES U64+
   ndict@ FILE-REC @ -  LATE-N +  PIECES U64+
   0  ndict@ FILE-REC @ ?do  i RECORD-MARK LATE-BELOW  i RECORD+  loop
   LATE-REST
   CODE+  DATA+  STORES+
   -1 PIECES U64+ ;

\ FILE-KEY's object, given its source's size, with INPUT-SUM and PREFIX-SUM as START-SUMS
\ left them; its checksum sums every byte after it.
: WRITE-OBJECT ( n -- )
   {: srcu:n :}
   PIECES!
   REFS BYTES$ {: refs:ptr refsu:n :}
   PIECES BYTES$ {: pieces:ptr piecesu:n :}
   HEADER-BYTES INPUT-BYTES + {: files-at:n :}
   OBJECT BUF:CLEAR
   s" HABUFOBJ" OBJECT SPAN+
   FILE-KEY KEY-BYTES OBJECT SPAN+
   KEY-BYTES 0 ?do 0 OBJECT BUF:APPEND-BYTE loop
   HEADER-BYTES OBJECT U64+  INPUT-BYTES OBJECT U64+
   refsu 0<> if files-at else 0 then OBJECT U64+  refsu OBJECT U64+
   4 0 ?do 0 OBJECT U64+ loop          \ REQUESTS NAMES, empty
   files-at refsu + OBJECT U64+  piecesu OBJECT U64+
   srcu OBJECT U64+
   INPUT-SUM KEY-BYTES OBJECT SPAN+
   PREFIX-SUM KEY-BYTES OBJECT SPAN+
   refs refsu OBJECT SPAN+
   pieces piecesu OBJECT SPAN+
   OBJECT BYTES$ {: obj:ptr len:n :}
   SHA-CTX  obj 8 KEY-BYTES 2 * + +  len 8 - KEY-BYTES 2 * -  obj 8 KEY-BYTES + +  SHA256-IN
   FILE-KEY KEY-HEX SHA256>HEX
   KEY-HEX KEY-BYTES 2 * BUILD-OUT:OBJ$  OBJECT BYTES$  FS:ATOMIC-WRITE-FILE ;

\ The loaded file's BUILT row, summing OBJECT, the object written or read.
: BUILT+ ( -- )
   SHA-CTX OBJECT BYTES$ OBJECT-SUM SHA256-IN
   FILE-KEY KEY-BYTES BUILT SPAN+
   FILE-REC @ BUILT U64+  ndict@ BUILT U64+
   OBJECT-SUM KEY-BYTES BUILT SPAN+
   FILE-EM @ BUILT U64+  NSHADOW:EMISSIONS BUILT U64+
   FILE-DP @ BUILT U64+  FILE-END @ BUILT U64+ ;

\ ---- reading an object ---------------------------------------------------------
\ FRESH? reads FILE-KEY's object into OBJECT and checks every field WRITE-OBJECT writes.
\ The link reads these variables, ENTRY-OFFS, EM-OFFS and FILE-ROWS, valid once FRESH? answers true.
variable ENTRIES-N                    \ the object's entries
variable RECS-N                       \ its entries other than KIND-PRIVATE: its records
variable EMS-N                        \ its emissions, E
variable DATA-AT                      \ the offset in OBJECT of DATA's D
variable CELLS-AT                     \ of its K
variable STRS-AT                      \ of STRINGS' N
variable STORES-AT                    \ of STORES' S
\ Only the check uses these and DOES-AT.
variable FILES-N                      \ its FILES rows
variable DEFS-N                       \ the KIND-DEF entries read so far
variable DOES-N                       \ the KIND-DOES entries read so far, DOES-AT's rows
variable LAST-EM                      \ the entry just read's emission when it is KIND-DEF, else -1
variable OWN-DATA-MAX                 \ the furthest a reference reaches into this file's data

\ The reader over OBJECT: [RD-AT, RD-END), and a failure that sticks until RD-OPEN.
ENUM reading good bad ;ENUM
TYPED-VARIABLE READER reading
variable RD-AT
variable RD-END

: BAD ( -- ) construct reading bad READER ! ;


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


\ Advances n bytes; bad when they pass RD-END.
: RD-SKIP ( n -- )
   {: u:n :}
   GOOD? 0= if exit then
   u 0<  u RD-END @ RD-AT @ - >  or if BAD exit then
   u RD-AT +! ;


\ The next 8 bytes, low byte first; 0 once bad.
: RD-U64 ( -- n )
   RD-AT @ {: at:n :}
   8 RD-SKIP
   GOOD? 0= if 0 exit then
   OBJECT BYTES$ drop at + LE:U64@ ;


: RD-EQ ( n -- ) RD-U64 <> if BAD then ;


\ Bad unless the next u bytes equal the span's.
: RD-SAME ( ptr u8 n -- )
   {: a:ptr u:n :}
   RD-AT @ {: at:n :}
   u RD-SKIP
   GOOD? 0= if exit then
   OBJECT BYTES$ drop at + u  a u  STR= 0= if BAD then ;


\ A count of rows of at least n bytes each; bad, and 0, when the rest cannot hold them.
: RD-COUNT ( n -- n )
   {: row:n :}
   RD-U64 {: k:n :}
   k 0<  k  RD-END @ RD-AT @ - row /  >  or if BAD 0 exit then
   k ;


: OBJ@ ( n -- n ) OBJECT BYTES$ drop swap + LE:U64@ ;


\ FILES row k's BUILT row's number at the byte offset (BUILT's layout).
: BUILT@ ( n n -- n )
   {: k:n off:n :}
   k FILE-ROWS @ BUILT-AT off + LE:U64@ ;


: FILE-IN? ( n -- bool )
   {: k:n :}
   k 1 >=  k FILES-N @ <=  and ;


: FILE-RECS ( n -- n )
   {: k:n :}
   k KEY-BYTES 8 + BUILT@  k KEY-BYTES BUILT@ - ;


: FILE-EMS ( n -- n )
   {: k:n :}
   k ROW-BYTES 24 + BUILT@  k ROW-BYTES 16 + BUILT@ - ;


: FILE-BYTES ( n -- n )
   {: k:n :}
   k ROW-BYTES 40 + BUILT@  k ROW-BYTES 32 + BUILT@ - ;


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


\ A record reference (file, index): this file's index below the bound, or an
\ earlier file's below its record count.
: REC-REF-CK ( n n n -- )
   {: file:n idx:n bound:n :}
   GOOD? 0= if exit then
   idx 0< if BAD exit then
   file 0= if idx bound >= if BAD then exit then
   file FILE-IN? 0= if BAD exit then
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
   e file FILE-EMS >= if BAD exit then
   f  file ROW-BYTES 16 + BUILT@ e + NSHADOW:FUNCTIONS  >= if BAD then ;


\ A code reference (RD-REF) whose own emission is at most top.
: CODE-REF-CK ( n n n n n -- )
   {: tag:n x:n y:n z:n top:n :}
   GOOD? 0= if exit then
   tag 0 = if x y RECS-N @ REC-REF-CK exit then
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
      y x FILE-BYTES > if BAD then exit
   then
   tag 4 = if
      x 0<  x OPEN-DP @ BASE @ - >=  or if BAD then exit
   then
   BAD ;


\ The BUILT row with the FILES row's key and object SHA-256 (REF-FILE), and true; false when none.
: BUILT-MATCH ( ptr u8 -- n bool )
   {: at:ptr :}
   BUILT-N 0 ?do
      i BUILT-AT {: row:ptr :}
      row KEY-BYTES  at KEY-BYTES  STR=
      row KEY-BYTES 16 + + KEY-BYTES  at KEY-BYTES + KEY-BYTES  STR=  and if i true unloop exit then
   loop
   0 false ;


\ FILES' rows, u bytes, into FILE-ROWS from 1.
: FILES-CK ( n -- )
   {: u:n :}
   0 FILES-N !
   GOOD? 0= if exit then
   u ROW-BYTES / {: n:n :}
   n FILES-N !
   n 1 + FILE-ROWS-RESERVE
   n 0 ?do
      RD-AT @ {: at:n :}
      ROW-BYTES RD-SKIP
      GOOD? if
         OBJECT BYTES$ drop at + BUILT-MATCH if i 1 + FILE-ROWS ! else drop BAD then
      then
   loop ;


\ The header, its checksum, INPUT against the source's size, INPUT-SUM and PREFIX-SUM, and
\ FILES, as WRITE-OBJECT lays them.
: HEADER-CK ( n -- )
   {: srcu:n :}
   0 FILES-N !
   OBJECT BYTES$ nip {: len:n :}
   0 len RD-OPEN
   HEADER-BYTES INPUT-BYTES + {: files-at:n :}
   len files-at < if BAD exit then
   s" HABUFOBJ" RD-SAME
   FILE-KEY KEY-BYTES RD-SAME
   GOOD? 0= if exit then
   OBJECT BYTES$ drop {: obj:ptr :}
   SHA-CTX  obj 8 KEY-BYTES 2 * + +  len 8 - KEY-BYTES 2 * -  CK-SUM  SHA256-IN
   CK-SUM KEY-BYTES RD-SAME
   HEADER-BYTES RD-EQ  INPUT-BYTES RD-EQ
   RD-U64 RD-U64 {: off:n refsu:n :}
   refsu 0<  refsu ROW-BYTES mod 0<>  or  refsu len files-at - >  or if BAD exit then
   off  refsu 0<> if files-at else 0 then  <> if BAD then
   4 0 ?do 0 RD-EQ loop
   files-at refsu + RD-EQ  len files-at - refsu - RD-EQ
   srcu RD-EQ
   INPUT-SUM KEY-BYTES RD-SAME
   PREFIX-SUM KEY-BYTES RD-SAME
   refsu FILES-CK ;


\ The kind of this object's record r, an entry already read.
: REC-KIND ( n -- n )
   {: r:n :}
   0  ENTRIES-N @ 0 ?do
      i ENTRY-OFFS @ OBJ@ {: kind:n :}
      kind KIND-PRIVATE <> if
         dup r = if drop kind unloop exit then
         1 +
      then
   loop
   drop -1 ;


\ A record reference naming a package: an earlier entry of KIND-PACKAGE, or an earlier file's
\ package record.
: PKG-REF-CK ( -- )
   RD-U64 RD-U64 {: file:n idx:n :}
   file idx RECS-N @ REC-REF-CK
   GOOD? 0= if exit then
   file 0= if idx REC-KIND KIND-PACKAGE <> if BAD then exit then
   file KEY-BYTES BUILT@ idx + PKG? 0= if BAD then ;


\ A name: its length, at least 1, then its bytes.
: NAME-CK ( -- ) RD-U64 {: u:n :}  u 1 < if BAD exit then  u RD-SKIP ;

\ An entry's flags, which RECORD+ writes without the name length and EXT.
: FLAGS-CK ( -- ) RD-U64  DNAME-LEN-MASK DNAME-EXT or  and 0<> if BAD then ;


\ The wordlist role (WORDLIST+) of an entry of the kind: 2 for KIND-PACKAGE, else 0, 1, or 3 or
\ 4 then its package.
: ROLE-CK ( n -- )
   {: kind:n :}
   RD-U64 {: role:n :}
   role 0<  role 4 >  or if BAD exit then
   role 2 =  kind KIND-PACKAGE =  xor if BAD exit then
   role 3 >= if PKG-REF-CK then ;


\ A KIND-DEF entry's emission: NSHADOW:PUBLISH files one emission per definition, so it is the
\ count of KIND-DEF entries before it.
: EM-NAMED ( -- n )
   RD-U64 {: e:n :}
   e DEFS-N @ <> if BAD then
   1 DEFS-N +!  e ;


: PKG-FLAG-CK ( -- ) RD-U64 {: f:n :}  f 0<>  f 1 <>  and if BAD then ;


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
   kind 0= if tag x y z EMS-N @ 1 - CODE-REF-CK exit then
   kind 1 = if tag x y z DATA-REF-CK exit then
   BAD ;


\ A definition's dispatch cell (DISPATCH+): none, or a whole cell of this file's data.
: DISPATCH-CK ( -- )
   RD-REF {: tag:n x:n y:n z:n :}
   tag 5 = if exit then
   tag 3 <>  x 0<>  or  y 0<  or  y 7 and 0<>  or  y 8 + 0<  or if BAD exit then
   y 8 + OWN-DATA-MAX @ max OWN-DATA-MAX ! ;


\ Entry i: its offset into ENTRY-OFFS, then its fields (RECORD+, LATE+); a record's index is
\ RECS-N as it starts.
: ENTRY-CK ( n -- )
   {: i:n :}
   RD-AT @ i ENTRY-OFFS !
   RD-U64 {: kind:n :}
   kind 0<  kind KIND-PRIVATE >  or if BAD exit then
   kind KIND-PRIVATE = if PKG-REF-CK  -1 LAST-EM ! exit then
   NAME-CK  FLAGS-CK  kind ROLE-CK
   kind KIND-DEF = if EM-NAMED else -1 then {: em:n :}
   kind KIND-DEF = if DISPATCH-CK then
   kind KIND-PACKAGE = if PKG-FLAG-CK then
   kind KIND-ALIAS = if RD-U64 RD-U64 RECS-N @ REC-REF-CK then
   kind KIND-DOES = if DOES-CK then
   em LAST-EM !
   1 RECS-N +! ;


\ The piece count, 1, then the entries (PIECES!); 24 bytes is a KIND-PRIVATE entry's size.
: ENTRIES-CK ( -- )
   1 RD-EQ
   24 RD-COUNT {: n:n :}
   n ENTRIES-N !  0 RECS-N !  0 DEFS-N !  0 DOES-N !  -1 LAST-EM !  0 OWN-DATA-MAX !
   n 0 > if n ENTRY-OFFS-RESERVE then
   n 0 ?do i ENTRY-CK loop ;


\ A call row of an emission of s bytes, numbered top.
: CALL-CK ( n n -- )
   {: s:n top:n :}
   RD-U64 {: site:n :}
   site 0<  site 3 and 0<>  or  site s 4 - >  or if BAD then
   RD-U64 {: kind:n :}
   kind 0<>  kind 1 <>  and if BAD then
   RD-REF top CODE-REF-CK ;


\ ADDRESS-CARRIER:CHAIN-SIZE (0, 12 or 16) of the chain at an address row's site in the object's
\ copy of emission e's bytes, s of them.
: OWN-CHAIN ( n n n -- n )
   {: site:n s:n e:n :}
   OBJECT BYTES$ drop  e EM-OFFS @ 16 + + {: src:ptr :}
   src site +  src s +  ADDRESS-CARRIER:CHAIN-SIZE ;


\ An address row of an emission of s bytes, numbered top: a code chain is ADDR-CHAIN-BYTES long,
\ as the link requires, and a data chain is whole.
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
   kind 0= if RD-REF top CODE-REF-CK exit then
   kind 1 = if RD-REF DATA-REF-CK exit then
   BAD ;


\ The does> slot: -1, or its site and its target's reference.
: SLOT-CK ( n n -- )
   {: s:n top:n :}
   RD-U64 {: p:n :}
   p -1 = if exit then
   p 0<  p 3 and 0<>  or  p s 4 - >  or if BAD exit then
   RD-REF {: tag:n x:n y:n z:n :}
   tag 5 = if exit then
   tag x y z top CODE-REF-CK ;


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


\ A string row after one at offset prev: the 8 bytes before its body in D hold its length.
: STRING-CK ( n n -- n )
   {: prev:n d:n :}
   RD-U64 RD-U64 {: off:n u:n :}
   GOOD? 0= if prev exit then
   off prev <=  off 8 <  or  off d >  or  u 0<  or  u d off - >  or if BAD prev exit then
   DATA-AT @ off + OBJ@ u <> if BAD then
   off ;


\ A store row: a cell FIND-STORES admits by where it starts, at an aligned offset below an earlier
\ file's data end (a cell may run past an unaligned end: main.f's D is 98) or in the header data.
: STORE-CK ( -- )
   RD-REF {: tag:n x:n y:n z:n :}
   tag 3 =  x 1 >=  and  tag 4 =  or 0= if BAD exit then
   tag x y z DATA-REF-CK
   GOOD? 0= if exit then
   tag 3 = if y 7 and 0<>  y x FILE-BYTES >=  or if BAD exit then then
   tag 4 = if x 7 and 0<> if BAD exit then then
   RD-U64 RD-U64 {: kind:n v:n :}
   kind 3 = if RD-REF {: t:n a:n b:n c:n :}  t 5 <> if BAD then exit then
   v 0<> if BAD exit then
   kind CELL-REF-CK ;


\ DATA, STRINGS and STORES (DATA+, STORES+), then the request list's -1 at the object's end.
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
   -1 RD-EQ
   RD-AT @ RD-END @ <> if BAD then
   OWN-DATA-MAX @ d > if BAD then ;


\ The file at the path, whole, into the buffer through one descriptor, so a build renaming
\ a new file over the path cannot tear the read; false when it is missing.
: READ-INTO ( ptr u8 n ptr a -- bool )
   {: path:ptr pathu:n buf:ptr :}
   path pathu FS:OPEN-SIZED {: file:fd size:n found:bool :}
   found 0= if false exit then
   buf BUF:CLEAR
   file  buf size BUF:N>BLEN BUF:GROW SPAN:$  FS:READ-SIZED
   true ;

\ READ-INTO with its inputs kept beneath its answer, so a catch around it keeps the stack's shape.
: READ-KEEPING ( ptr u8 n ptr a bool -- ptr u8 n ptr a bool )
   drop {: path:ptr pathu:n buf:ptr :}
   path pathu buf  path pathu buf READ-INTO ;

\ READ-INTO, false also when the path cannot be opened (too long, a 0 byte) or the file read;
\ any other throw passes.
: READ-WHOLE ( ptr u8 n ptr a -- bool )
   false ['] READ-KEEPING catch {: path pathu buf ok code:n :}
   code 0= if ok exit then
   code E-FS-OPEN =  code E-FS-STAT = or  code E-FS-IO = or
   code E-FS-PATH = or  code E-FFI-NUL = or  if false exit then
   code throw ;

\ FILE-KEY's object, whole, into OBJECT; false when it is missing or cannot be opened or
\ read, so the file compiles and its write replaces the object.
: READ-OBJECT ( -- bool )
   FILE-KEY KEY-HEX SHA256>HEX
   KEY-HEX KEY-BYTES 2 * BUILD-OUT:OBJ$ OBJECT READ-WHOLE ;


\ After START-SUMS, given the source's size: true when FILE-KEY's object reads whole and its
\ header, INPUT and FILES check (HEADER-CK).
: HEADER-FRESH? ( n -- bool )
   {: srcu:n :}
   READ-OBJECT 0= if false exit then
   srcu HEADER-CK  GOOD? ;


\ After START-SUMS, given the source's size: true when FILE-KEY's object reads whole and
\ checks. It applies nothing.
: FRESH? ( n -- bool )
   HEADER-FRESH? 0= if false exit then
   ENTRIES-CK  CODE-CK  DATA-CK
   GOOD? ;

\ ---- the load list -------------------------------------------------------------
PATH-CAP BUFFER: LIST-PATH
variable LIST-AT                     \ the walk's offset in LOAD-LIST

: LIST-PATH$ ( -- ptr u8 n )
   BUILD-OUT:BASE$ s" loads" LIST-PATH FS:JOIN-PATH {: u:n :}
   LIST-PATH u ;

\ LOAD-LIST to <out>/loads, which the next build's WALK reads.
: WRITE-LIST ( -- ) LIST-PATH$ LOAD-LIST BYTES$ FS:ATOMIC-WRITE-FILE ;
: LIST-RESET ( -- ) LOAD-LIST BUF:CLEAR  s" HABULOAD" LOAD-LIST SPAN+ ;


\ The next u bytes of LOAD-LIST at LIST-AT, past them, and true; false when fewer remain.
: LIST-TAKE ( n -- ptr u8 n bool )
   {: u:n :}
   LOAD-LIST BYTES$ {: a:ptr len:n :}
   LIST-AT @ {: at:n :}
   u 0<  u len at - >  or if a 0 false exit then
   u LIST-AT +!
   a at +  u  true ;


\ The next FIELD+ of LOAD-LIST, and true; false when it does not read whole.
: LIST-FIELD ( -- ptr u8 n bool )
   8 LIST-TAKE {: a:ptr u:n ok:bool :}
   ok 0= if a 0 false exit then
   a LE:U64@ LIST-TAKE ;


\ The next row of LOAD-LIST: true when it reads whole, its source reads from the OS and its object's
\ header is fresh (HEADER-FRESH?), then BUILT+; other SOURCE-INPUT bytes mean compiling or E-STALE once.
: WALK-ROW ( -- bool )
   KEY-BYTES LIST-TAKE {: key:ptr keyu:n key-ok:bool :}
   LIST-FIELD {: path:ptr pathu:n path-ok:bool :}
   LIST-FIELD {: root:ptr rootu:n root-ok:bool :}
   key-ok path-ok and root-ok and 0= if false exit then
   key FILE-KEY keyu BYTE-COPY
   path pathu WALK-SRC READ-WHOLE 0= if false exit then
   WALK-SRC BYTES$ {: src:ptr srcu:n :}
   src srcu SOURCE-ROOT:SHEBANG-COMMENT
   src srcu START-SUMS
   srcu HEADER-FRESH? 0= if false exit then
   BUILT+ true ;


\ True when LOAD-LIST holds the magic and at least one row, and every row is fresh (WALK-ROW).
: WALK-ROWS ( -- bool )
   0 LIST-AT !
   8 LIST-TAKE {: m:ptr mu:n ok:bool :}
   ok 0= if false exit then
   m mu s" HABULOAD" STR= 0= if false exit then
   LOAD-LIST BYTES$ nip {: len:n :}
   LIST-AT @ len = if false exit then
   begin LIST-AT @ len < while
      WALK-ROW 0= if false exit then
   repeat
   true ;


\ No ENTRIES-CK, CODE-CK or DATA-CK: they read what linking the earlier files makes, and a
\ checksum, source, prefix and FILES rows that match name the object a compile wrote.
: WALK ( -- )
   LIST-PATH$ LOAD-LIST READ-WHOLE if WALK-ROWS else false then
   if construct link-mode open else construct link-mode compiling then SO-FAR !
   BUILT BUF:CLEAR  STARTED BUF:CLEAR  OBJECT BUF:CLEAR
   LIST-RESET ;

\ WALK at the build's first file, before its KEY!: a build starting no load reads no list.
: WALK-ONCE ( -- )
   SO-FAR @
   MATCH link-mode
      unread OF WALK ENDOF
      open OF E-COMP-STATE throw ENDOF
      compiling OF ENDOF
      linking OF ENDOF
      stale OF E-COMP-STATE throw ENDOF
   ;MATCH ;

\ ---- linking an object ---------------------------------------------------------
\ The words a link of a fresh object applies it with, once FRESH? answers true: each
\ reference's value (RESOLVE) and the entries that are not code.
variable LINK-SELF                   \ the record index of the entry being linked, -1 for none
variable LINK-FN                     \ the emission being linked's address: SOURCE-FUNS' at
variable LINK-EM                     \ that emission's number in this object, -1 for none

: LINK-CLEAR ( -- ) -1 LINK-SELF !  -1 LINK-EM !  0 LINK-FN ! ;


\ A record reference's file and index as a dictionary index.
: REC-INDEX ( n n -- n )
   {: file:n idx:n :}
   file 0= if FILE-REC @ idx + exit then
   file KEY-BYTES BUILT@ idx + ;


\ The record entering shadow emission e at its first byte: a linear scan over the
\ shadow's rows, as link-band.f ROUTINE. scans.
: EM-RECORD ( n -- n )
   {: e:n :}
   NSHADOW:RECORDS 0 ?do
      i NSHADOW:EMISSION@ e =  i NSHADOW:ENTRY@ 0=  and if i NSHADOW:RECORD@ unloop exit then
   loop
   E-COMP-STATE throw ;


\ A quotation reference's address: function f of the file's emission e. A placed
\ emission's function is where SOURCE-FUNCTION finds it, which makes the sum exact.
: FN-ADDR ( n n n -- n )
   {: file:n e:n f:n :}
   file 0=  e LINK-EM @ =  and if LINK-FN @  e f OWN-FUN-AT + exit then
   file 0= if
      FILE-EM @ e +  dup NSHADOW:EMISSIONS >= if E-COMP-STATE throw then
   else
      file ROW-BYTES 16 + BUILT@ e +
   then {: em:n :}
   em EM-RECORD XREF-REC XREF-START
   em f NSHADOW:FUNCTION-OFFSET@ +  em 0 NSHADOW:FUNCTION-OFFSET@ - {: addr:n :}
   addr NSHADOW:SOURCE-FUNCTION {: qe:n qf:n :}
   qe em <>  qf f <>  or if E-COMP-STATE throw then
   addr ;


\ A reference's value, from RD-REF's tag and fields (CODE+ states them); 0 for none.
\ The entry being linked enters its emission's first function, before its record exists.
: RESOLVE ( n n n n -- n )
   {: tag:n x:n y:n z:n :}
   tag 0 = if
      x y REC-INDEX {: r:n :}
      r LINK-SELF @ = if LINK-FN @  LINK-EM @ 0 OWN-FUN-AT + exit then
      r ndict@ >= if E-COMP-STATE throw then
      r PKG? if E-COMP-STATE throw then
      r XREF-REC XREF-START exit
   then
   tag 1 = if rbase x + exit then
   tag 2 = if x y z FN-ADDR exit then
   tag 3 = if
      x 0= if FILE-DP @ y + exit then
      x ROW-BYTES 32 + BUILT@ y + exit
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


\ A name: its length, then its bytes, which stay in OBJECT.
: RD-NAME ( -- ptr u8 n )
   RD-U64 {: u:n :}
   OBJECT BYTES$ drop RD-AT @ + {: a:ptr :}
   u RD-SKIP
   a u ;


\ The wordlist a role names (WORDLIST+); roles 3 and 4 are then the package's reference,
\ and a package without that wordlist (id 0) is E-COMP-STATE.
: ROLE-WID ( -- n )
   RD-U64 {: role:n :}
   role 0 = if BOOT:GLOBAL-WL exit then
   role 1 = if WL:PRIMITIVES exit then
   role 2 = if BOOT:NAMESPACE-WL exit then
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
   file idx REC-INDEX OUTER:PACKAGE-PRIVATE ;


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
   flags want SET-FLAGS ;

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
: NEMIT-FILL ( n -- )
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
   \ PLACE turns a code chain into adrp+add, with no address-map row; a data chain needs one.
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
   \ A slot a clause patched is not linked yet.
   RD-U64 {: p:n :}
   p 0 >= if
      RD-REF {: tag:n x:n y:n z:n :}
      tag 5 <> if E-COMP-STATE throw then
      A64ASM:ENC-RET  copy p +  LE:U32!
      p NEMIT:PATCH-SLOT!
   then
   GOOD? 0= if E-COMP-STATE throw then
   NEMIT:SEAL ;


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
   want LINK-SELF !  fn LINK-FN !  e LINK-EM !
   e NEMIT-FILL
   does if j NSHADOW:TAKE-DOES else NSHADOW:TAKE then
   fn NSHADOW:SOURCE-FUNS
   NEMIT:CLEAR
   want NSHADOW:PUBLISH
   CODE-COPY BYTES$ drop {: copy:ptr :}
   \ The call targets and chains already hold host values, so both resolvers answer what they get.
   NSHADOW:EMISSIONS 1 -  copy  fn  [: ;] [: ;]  ARM64:PLACE
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
   kind KIND-DOES = if exit then
   E-COMP-STATE throw ;


\ The read object's entries in their order, then its data.
: LINK-OBJECT ( -- )
   LINK-CLEAR
   ENTRIES-N @ 0 ?do i LINK-ENTRY loop
   LINK-DATA ;

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
   HINT DATA-SIZE MEM-PROT-RW MEM-MAP-PRIVATE-ANON NORESERVE or
   MEM-ANON-FD MEM-OFF-ZERO mmap {: at:n :}
   at 0 < if E-MEM-MAP throw then
   at BASE !
   at HINT < if UNMAP E-MEM-MAP throw then
   construct step mapped PROGRESS !
   LAY-ADDRESS-HEADER ;

: SHADOW ( -- )
   NABI:BINDING NSHADOW:OPEN-NATIVE
   construct step shadowed PROGRESS ! ;

\ The flags cell of hb's newest record: an `immediate` before the space's first
\ definition sets it.
: NEWEST-FLAGS ( -- ptr n ) FIRST-REC @ 1- REC-FLAGS ;

: SAVE ( -- )
   NEWEST-FLAGS @ SAVED-FLAGS !
   DP-CELL HOST@ SAVED-DP !
   BOOT:DP-FLOOR-CELL HOST@ SAVED-FLOOR !
   BOOT:DP-CEILING-CELL HOST@ SAVED-CEILING !
   DPHW-CELL HOST@ SAVED-DPHW !
   DATA-FLOOR-CELL HOST@ SAVED-DATA-FLOOR !
   BOOT:GLOBAL-WL-CELL HOST@ SAVED-GLOBAL-WL !
   BOOT:NAMESPACE-WL-CELL HOST@ SAVED-NAMESPACE-WL !
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
   wordlist wordlist negate {: global:n ns:n :}
   ndict@ FIRST-REC !
   SAVE
   BASE @ DATA-START + {: start:n :}
   start DP-CELL HOST!
   start BOOT:DP-FLOOR-CELL HOST!
   BASE @ DATA-SIZE + PROF-CNT-BYTES - BOOT:DP-CEILING-CELL HOST!
   0 DPHW-CELL HOST!
   0 LASTC-CELL HOST!
   start DATA-N - DATA-FLOOR-CELL HOST!
   0 AOT-CELLS:SIG-POOL-CELL HOST!    \ a build has no signature pool
   0 AOT-CELLS:SIG-LEN-CELL HOST!
   global BOOT:GLOBAL-WL-CELL HOST!
   ns BOOT:NAMESPACE-WL-CELL HOST!
   global set-current
   construct step moved PROGRESS ! ;

: CHECK-IN ( -- )
   CHECK:OPEN
   construct step checked PROGRESS ! ;

: REQUIRE-FRESH ( -- )
   REQUIRE-SNAPSHOT
   construct step required PROGRESS ! ;

\ `[: execute ;]` here is E-EXEC-OPAQUE-XT; a quotation parameter is typed.
: RUN-UNIT ( n n [ n n -- ] -- ) execute ;

\ E-FAILED once a file's load threw or DP moved back in the space (PROGRESS loading, failed): a failed
\ build loads and writes nothing more. E-COMP-STATE with no build open.
: GOING ( -- )
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
         DPHW-CELL HOST@ 0<> if
            construct step failed PROGRESS !  E-FAILED throw
         then
      ENDOF
      loading OF E-FAILED throw ENDOF
      failed OF E-FAILED throw ENDOF
   ;MATCH ;


\ After START-SUMS, given the source's size: true when the file links. A stale file while linking
\ sets the mode stale and the step failed before E-STALE, so a later GOING throws E-FAILED.
: LINKS? ( n -- bool )
   {: srcu:n :}
   SO-FAR @
   MATCH link-mode
      unread OF E-COMP-STATE throw ENDOF
      open OF
         srcu FRESH? dup if construct link-mode linking else construct link-mode compiling then
         SO-FAR !
      ENDOF
      compiling OF false ENDOF
      linking OF
         srcu FRESH? 0= if
            construct link-mode stale SO-FAR !
            construct step failed PROGRESS !  E-STALE throw
         then
         true
      ENDOF
      stale OF E-COMP-STATE throw ENDOF
   ;MATCH ;


\ The fresh object as the file's result, added to BUILT; a throw leaves the step at loading.
: LINK-FILE ( -- )
   LINK-OBJECT
   here NULL-PTR - FILE-END !
   BUILT+
   construct step open PROGRESS ! ;


\ Links the file or compiles it as the mode says (LINKS?), counting and writing a compile when DP
\ never moved back. Any throw after GOING ends the build, the next GOING throwing E-FAILED.
: RUN-FILE ( ptr u8 n ptr u8 n ptr u8 n [ n n -- ] -- )
   {: path:ptr pathu:n root:ptr rootu:n source:ptr sourceu:n q :}
   LOADER$ nip 0<> if E-COMP-STATE throw then
   GOING
   construct step loading PROGRESS !
   WALK-ONCE
   path pathu root rootu UNDER-ROOT KEY!
   path pathu root rootu LIST-ROW+
   ndict@ FILE-REC !  NSHADOW:RECORDS FILE-ROW !  NSHADOW:EMISSIONS FILE-EM !
   here NULL-PTR - FILE-DP !
   WIDN-CELL HOST@ FILE-WID !   \ hb's DATA, where namespace-record and namespace-private mint ids
   source sourceu START-SUMS
   sourceu LINKS? if LINK-FILE exit then
   COPY-BELOW
   PUSH-KEY
   0 sourceu q ['] RUN-UNIT ['] POP-KEY finally
   DPHW-CELL HOST@ 0<> if
      construct step failed PROGRESS !  E-FAILED throw
   then
   here NULL-PTR - FILE-END !
   CHECK-EARLIER  FIND-STORES
   WIDN-CELL HOST@ FILE-WID @ -  MINTED {: ids:n held:n :}
   ids held > if construct step failed PROGRESS !  E-WORDLIST throw then
   ids held < if E-COMP-STATE throw then
   1 COMPILED-N +!
   sourceu WRITE-OBJECT
   BUILT+
   construct step open PROGRESS ! ;

: OBJECTS-IN ( -- )
   INIT-OBJECTS
   construct step objects PROGRESS ! ;

: ROUTE ( -- )
   SOURCE-UNIT:CURRENT PRIOR-RUNNER !
   [: RUN-FILE ;] SOURCE-UNIT:USE
   construct step routed PROGRESS ! ;

\ Last: from here header stores go to the space (habu2.f ROUTE-HEADER).
: IMAGE-IN ( -- )
   BASE @ BOOT:IMAGE-BASE-CELL HOST!
   here NULL-PTR - OPEN-DP !
   construct step open PROGRESS ! ;

: OPEN ( -- )
   0 COMPILED-N !  construct link-mode unread SO-FAR !
   INSTALL-LOOP
   MAP SHADOW MOVE-IN CHECK-IN REQUIRE-FRESH OBJECTS-IN ROUTE IMAGE-IN ;

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
   SAVED-COMMA-RUN @ COMMA-RUN-CELL HOST!
   SAVED-COMMA-END @ COMMA-END-CELL HOST!
   SAVED-COMMA-BASE @ COMMA-BASE-CELL HOST!
   SAVED-LASTC @ LASTC-CELL HOST!
   SAVED-SIG-POOL @ AOT-CELLS:SIG-POOL-CELL HOST!
   SAVED-SIG-LEN @ AOT-CELLS:SIG-LEN-CELL HOST!
   SAVED-FLAGS @ NEWEST-FLAGS ! ;

\ Records still below the count would name bodies in the unmapped space; their
\ code goes back as FORGET-DEFS-FROM gives it.
: FORGET-SPACE ( -- )
   FIRST-REC @ CODE-RECLAIM:FLOOR-FROM {: floor:n :}
   FIRST-REC @ ndict!
   floor CODE-RECLAIM:TRUNCATE ;

\ Undo IMAGE-IN, from open or from a file's loading or failed step.
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
         REQUIRE-RESTORE construct step checked PROGRESS ! true
      ENDOF
      objects OF
         DISPOSE-OBJECTS construct step required PROGRESS ! true
      ENDOF
      routed OF
         PRIOR-RUNNER @ SOURCE-UNIT:USE construct step objects PROGRESS ! true
      ENDOF
      open OF IMAGE-OUT ENDOF
      loading OF IMAGE-OUT ENDOF
      failed OF IMAGE-OUT ENDOF
   ;MATCH ;

: CLOSE ( -- )
   begin UNDO 0= until ;

\ The list is written once a load started, even when the body throws; a stale build writes the
\ magic alone, so the next build compiles every file. A throw from the write passes.
: WRITE-STARTED ( -- )
   SO-FAR @
   MATCH link-mode
      unread OF ENDOF
      open OF WRITE-LIST ENDOF
      compiling OF WRITE-LIST ENDOF
      linking OF WRITE-LIST ENDOF
      stale OF LIST-RESET WRITE-LIST ENDOF
   ;MATCH ;

: RUN ( -- ) OPEN [: BODY @ execute ;] [: WRITE-STARTED ;] finally ;

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
      failed OF false ENDOF
   ;MATCH ;

public

\ Run the body in a build's space with no package of hb's open, close on every
\ exit, reopen the caller's package, rethrow; E-COMP-STATE while a build is open.
: WITH ( ptr u8 n [ -- ] -- )
   {: mode:ptr modeu:n body :}
   IDLE? 0= if E-COMP-STATE throw then
   mode modeu s" engine" STR= ENGINE? !
   body BODY !
   [: ['] RUN ['] CLOSE finally ;] OUTER:WITHOUT-PACKAGE ;

\ The files the last build compiled, from its open until the next open.
: COMPILED ( -- n ) COMPILED-N @ ;

;package
