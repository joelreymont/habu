\ zip.f - indexed ZIP reading and byte-preserving replacement.
\
\ Decompression uses libzip (VERSIONED-LIBRARY zip 5, src/lib/zip-ffi.f).
\ Archives and entries are distinct checked identities: every operation checks
\ the archive is live and the entry is its own, and entries are zero-based
\ indices, so duplicate names stay distinct. OPEN is read-only; EDIT opens an
\ existing archive (copy the input first for a new output). Both copy the input
\ once to a private snapshot under $TMPDIR (or /tmp); failure to make it, or an
\ input without an end record, throws ZIP:E-OPEN and releases what it made.
\ There is no in-memory fallback, and later changes to the input path change
\ nothing. All-stored unencrypted archives read members from the snapshot;
\ otherwise OPEN opens libzip. SOURCE$, NAME$ and READ return archive-owned
\ copies valid until a successful COMMIT or CLOSE; SOURCE$ is the original
\ snapshot, which pending changes do not alter.
\ COMPRESSED-SIZE answers the central directory compressed size, ZIP64
\ included, or a pending replacement's stored length, the view READ sees.
\
\ REPLACE copies its input at once (an empty member too); REPLACE-WITH hands a
\ callback an archive-owned span of exactly the length asked. READ sees a
\ pending replacement. REMOVE stages omission on an EDIT handle, idempotently;
\ until COMMIT or CLOSE, COUNT, indices, names and reads keep the opened view,
\ and replacing a removed entry throws ZIP:E-ENTRY. COMMIT writes a compact
\ archive to a temporary in the destination directory and renames it over,
\ keeping permissions and untouched local records byte-for-byte; replacements
\ are STORE with fresh CRC and sizes, ZIP64 fields rebuilt as needed. On success
\ it retires the archive and its entries; on ZIP:E-COMMIT the archive stays live
\ for a retry or CLOSE. CLOSE discards pending changes. A stale handle or a
\ repeated close throws ZIP:E-HANDLE; a foreign, negative or out-of-range entry
\ ZIP:E-ENTRY. OPEN-ENTRY and OPEN-SOURCE give inputs with their own cursors;
\ READ-IN checks length and CRC through EOF and on failure retires the input
\ and throws ZIP:E-READ; CLOSE-ENTRY drains and verifies an unfinished member.
\ ARCHIVE-SLOT and INPUT-SLOT only retype a caller cell. Diagnostics carry no
\ member contents; there is no fixed entry limit.
\
\ STORAGE CLASS. TASK-LOCAL: the stat block and EOF probe byte libzip writes
\ are one TASK:+USER row per task. Archive buffers, entries and inputs are owned by
\ their handle, and each handle has one task owner: separate tasks use distinct
\ handles concurrently and coordinate any transfer. PROCESS-WIDE: the identity
\ registry and serial, under one atomic lock (src/lib/zip-state.f).
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/zip-raw.f

package ZIP
using FFI
using MEM

: PATH-CHECK ( ptr u8 n -- ) {: path:ptr len:n :}
   len SIZE-CHECK len 0= if E-PATH throw then
   len 0 ?do path i + c@ 0= if E-PATH throw then loop ;

: DISCARD-FAILED ( ptr n n -- ) {: node:ptr code:n :}
   node [: dup DISCARD-NODE ;] catch 2drop
   code throw ;

CAST: MAPPED>RAW ( n -- ptr u8 )

$4000 constant FILE-CHUNK

\ The source snapshot is a fresh FS:MKTEMP file: the node keeps its descriptor
\ for the copy and the mapping, and copies its path, which the task's next temp
\ reuses, into room allocated before the file exists, so no failure falls
\ between making the file and the node owning it.
: SNAPSHOT-WORK ( ptr n ptr u8 -- ptr n ptr u8 ) {: node:ptr copy:ptr :}
   s" habu-zip-source" FS:MKTEMP FD>N node SNAP-FD ! {: path:ptr len:n :}
   path len copy CSTR
   copy node SOURCE-PTR ! len node SOURCE-LEN ! 1 node SOURCE-LIVE !
   node copy ;

\ An unusable TMPDIR, or any other refusal to make the snapshot, is E-OPEN. The
\ room for its path is allocated outside the catch, so an allocation failure
\ keeps its own code, as OPEN-WORK's other allocations do.
: SNAPSHOT-OPEN ( ptr n -- ) {: node:ptr :}
   node FS-PATH-CAP 1+ BUFFER-ALLOC {: copy:ptr :}
   node copy [: SNAPSHOT-WORK ;] catch {: code:n :}
   2drop
   code 0<> if E-OPEN throw then ;

: SOURCE-STEP ( ptr n ptr u8 n -- n ) {: node:ptr buf:ptr cap:n :}
   node SOURCE-FD @ buf cap read {: got:n :}
   got 0 < got cap > or if E-READ throw then
   got 0 > if node SNAP-FD @ >FD buf got FD:WRITE-FULL then
   got ;

: COPY-SOURCE ( ptr n -- ) {: node:ptr :}
   node FILE-CHUNK BUFFER-ALLOC {: buf:ptr :}
   buf node COPY-BUF !
   0 begin node buf FILE-CHUNK SOURCE-STEP dup 0 > while SAFE+ repeat
   drop node RAW-LEN ! ;

: CLOSE-SOURCE-FD ( ptr n -- ) {: node:ptr :}
   node SOURCE-FD @ {: fd:n :}
   -1 node SOURCE-FD !
   fd C-CLOSE-FD 0<> if E-READ throw then ;

: NATIVE-STORED? ( ptr n -- bool ) {: node:ptr :}
   node MEMBER-COUNT @ 0 ?do
      node node i MEMBER-AT CENTRAL-PTR {: central:ptr :}
      central 8 + U16@ 1 and 0<>
      central $A + U16@ 0<> or if false unloop exit then
   loop true ;

: OPEN-WORK ( ptr n ptr u8 n n -- ptr n ptr u8 n n )
   {: node:ptr path:ptr len:n flags:n :}
   flags node MODE !
   node len 1+ BUFFER-ALLOC {: cpath:ptr :}
   path len cpath CSTR cpath node PATH-PTR ! len node PATH-LEN !
   node SNAPSHOT-OPEN
   node PATH-PTR @ open-rd {: src:n :}
   src 0 < if E-OPEN throw then src node SOURCE-FD !
   node COPY-SOURCE node CLOSE-SOURCE-FD
   node RAW-LEN @ dup END-BYTES < if E-OPEN throw then
   0 swap 1 2 node SNAP-FD @ 0 mmap {: mapped:n :}
   mapped -1 = if E-OPEN throw then
   mapped MAPPED>RAW node RAW-PTR !
   node PARSE-RAW
   node NATIVE-STORED? 0= if
      node SOURCE-PTR @ flags C-OPEN {: czip:ptr :}
      czip NULL? if E-OPEN throw then czip node OBJECT !
      czip C-COUNT node MEMBER-COUNT @ <> if E-OPEN throw then
   then
   node $10000 BUFFER-ALLOC node RAW-SCRATCH !
   node RAW-PTR @ node RAW-LEN @ munmap 0<> if E-READ throw then
   0 MAPPED>RAW node RAW-PTR !
   node path len flags ;

: OPEN-ATTEMPT ( ptr n ptr u8 n n -- n )
   [: OPEN-WORK ;] catch >r 2drop 2drop r> ;

: OPEN-CHECKED ( ptr u8 n n -- archive )
   {: path:ptr len:n flags:n :}
   path len PATH-CHECK REGISTER-CLEANUP REGISTER {: archive:archive :}
   archive ARCHIVE-NODE {: node:ptr :}
   node path len flags OPEN-ATTEMPT {: code:n :}
   code 0<> if node code DISCARD-FAILED then
   archive ;

: COUNT-NODE ( ptr n -- n ) MEMBER-COUNT @ ;

: INDEX-CHECK ( ptr n n -- ) {: node:ptr idx:n :}
   idx 0 < if E-ENTRY throw then
   idx node COUNT-NODE >= if E-ENTRY throw then ;

: NEW-ENTRY ( ptr n n -- entry )
   {: archive:ptr idx:n :}
   archive idx INDEX-CHECK NEXT-ID archive ENTRY-CELLS cells CHUNK-RESERVE CELL-VIEW
   {: ident:n node:ptr :}
   ident node ID ! idx node INDEX ! node archive ENTRIES ADD-NODE
   ident >ENTRY ;

: NAME-COPY ( ptr n n -- ptr u8 n )
   {: node:ptr idx:n :}
   node idx MEMBER-AT {: member:ptr :}
   node member CENTRAL-NAME {: name:ptr len:n :}
   node len 1 max CHUNK-RESERVE {: dst:ptr :}
   name dst len BYTE-COPY dst len ;

: MEMBER-SIZE ( ptr n n -- n ) {: node:ptr idx:n :}
   node OBJECT @ NULL? if node idx MEMBER-AT PLAIN-SIZE @ exit then
   node OBJECT @ idx STAT-DATA C-STAT 0 <> if E-READ throw then
   STAT-DATA @ STAT-SIZE-VALID and 0= if E-READ throw then
   STAT-DATA STAT-SIZE-CELL cells + @ dup SIZE-CHECK ;

: CRC-FEED ( n ptr u8 n -- n ) {: crc:n data:ptr len:n :}
   crc len 0 ?do data i + c@ CRC-BYTE loop ;

: OPEN-INPUT ( ptr n n -- input ) {: owner:ptr idx:n :}
   owner idx MEMBER-AT {: member:ptr :}
   NEXT-ID {: ident:n :}
   NODE-ALLOC {: node:ptr :}
   ident node ID !
   owner node INPUT-OWNER !
   U32-MAX node INPUT-CRC !
   member REPLACED? if
      member REPLACEMENT @ node INPUT-DATA !
      member REPLACE-LEN @ node INPUT-LEN !
      member REPLACE-CRC @ node INPUT-EXPECT !
   else
      owner OBJECT @ NULL? if
         member PAYLOAD-OFF @ node INPUT-PAYLOAD !
         member PACKED-SIZE @ node INPUT-COMPRESSED !
         2 node INPUT-KIND !
      else
         owner OBJECT @ idx C-FOPEN {: file:ptr :}
         file NULL? if node NODE-FREE E-READ throw then
         file node INPUT-OBJECT !
      then
      member PLAIN-SIZE @ node INPUT-LEN !
      owner member CENTRAL-PTR $10 + U32@ node INPUT-EXPECT !
   then
   node REGISTER-INPUT ;

: OPEN-SOURCE-INPUT ( ptr n -- input )
   {: owner:ptr :}
   NEXT-ID {: ident:n :}
   NODE-ALLOC {: node:ptr :}
   ident node ID !
   owner node INPUT-OWNER ! owner RAW-LEN @ node INPUT-LEN !
   owner SOURCE-PTR @ open-rd {: fd:n :}
   fd 0 < if node NODE-FREE E-OPEN throw then
   fd node INPUT-FD ! 1 node INPUT-KIND !
   node REGISTER-INPUT ;

: INPUT-END ( ptr n -- ) {: node:ptr :}
   node INPUT-OFF @ node INPUT-LEN @ <> if E-READ throw then
   node INPUT-KIND @ 2 = if
      node INPUT-OFF @ node INPUT-COMPRESSED @ <> if E-READ throw then
   then
   node INPUT-KIND @ 1 <> if
      node INPUT-CRC @ U32-MAX xor node INPUT-EXPECT @ <> if E-READ throw then
   then
   1 node INPUT-DONE ! ;

: INPUT-BYTES ( ptr n ptr u8 n -- n ) {: node:ptr dst:ptr cap:n :}
   node INPUT-KIND @ 1 = if node INPUT-FD @ dst cap read exit then
   node INPUT-KIND @ 2 = if
      node INPUT-LEN @ node INPUT-OFF @ -
      node INPUT-COMPRESSED @ node INPUT-OFF @ - min cap min {: len:n :}
      len 0= if 0 exit then
      node INPUT-OWNER @ SNAP-FD @ dst len
         node INPUT-PAYLOAD @ node INPUT-OFF @ + C-PREAD exit
   then
   node INPUT-DATA @ {: data:ptr :}
   data NULL? if
      node INPUT-OBJECT @ dst cap C-FREAD
   else
      node INPUT-LEN @ node INPUT-OFF @ - cap min {: got:n :}
      data node INPUT-OFF @ + dst got BYTE-COPY got
   then ;

: READ-INNER ( ptr n SPAN:span<u8> -- n ) {: node:ptr dst :}
   node INPUT-DONE @ 0<> if 0 exit then
   dst SPAN:$ {: data:ptr cap:n :}
   cap 0= if 0 exit then
   node data cap INPUT-BYTES {: got:n :}
   got 0 < got cap > or if E-READ throw then
   got 0= if node INPUT-END 0 exit then
   node INPUT-OFF @ got SAFE+ dup node INPUT-LEN @ > if E-READ throw then
   node INPUT-OFF !
   node INPUT-KIND @ 1 <> if
      node INPUT-CRC @ data got CRC-FEED node INPUT-CRC !
   then got ;

: DISCARD-INPUT ( ptr n -- n ) {: node:ptr :}
   node UNREGISTER-INPUT
   node INPUT-KIND @ 1 = if
      node INPUT-FD @ C-CLOSE-FD
   else
      node INPUT-KIND @ 2 = if 0 else
         node INPUT-OBJECT @ {: file:ptr :}
         file NULL? if 0 else file C-FCLOSE then
      then
   then {: code:n :}
   node NODE-FREE code ;

: READ-IN-TRY ( ptr n SPAN:span<u8> -- ptr n SPAN:span<u8> ) {: node:ptr dst :}
   node dst READ-INNER node INPUT-LAST ! node dst ;

: READ-IN-CHECKED ( input SPAN:span<u8> -- n ) {: handle:input dst :}
   handle INPUT-NODE {: node:ptr :}
   node dst [: READ-IN-TRY ;] catch >r 2drop r> {: code:n :}
   code 0<> if node DISCARD-INPUT drop code throw then
   node INPUT-LAST @ ;

: CLOSE-IN-WORK ( ptr n -- ptr n ) {: node:ptr :}
   node INPUT-KIND @ 1 = if node exit then
   begin node STAT-DATA BYTE-VIEW STAT-BYTES SPAN:MAKE READ-INNER 0<> while
   repeat node ;

: CLOSE-IN-CHECKED ( input -- )
   INPUT-NODE {: node:ptr :}
   node [: CLOSE-IN-WORK ;] catch {: code:n :}
   drop node DISCARD-INPUT {: close-code:n :}
   code 0<> if code throw then
   close-code 0<> if E-READ throw then ;

: COPY-INPUT ( ptr n ptr u8 n -- ptr n ptr u8 n )
   {: node:ptr buf:ptr len:n :}
   0 begin dup len < while
      dup {: off:n :}
      node buf off + len off - SPAN:MAKE READ-INNER
      dup 0= if E-READ throw then +
   repeat drop
   node buf 1 SPAN:MAKE READ-INNER 0<> if E-READ throw then
   node buf len ;

: COPY-ATTEMPT ( ptr n ptr u8 n -- n )
   [: COPY-INPUT ;] catch >r 2drop drop r> ;

: READ-COPY ( ptr n n -- ptr u8 n ) {: node:ptr idx:n :}
   node idx MEMBER-AT {: member:ptr :}
   member REPLACED? if member REPLACEMENT @ member REPLACE-LEN @ exit then
   node idx MEMBER-SIZE {: len:n :}
   node len BUFFER-ALLOC {: buf:ptr :}
   node idx OPEN-INPUT INPUT-NODE {: input:ptr :}
   input buf len COPY-ATTEMPT {: code:n :}
   input DISCARD-INPUT 0<> code 0<> or if E-READ throw then
   buf len ;

: WRITABLE ( ptr n -- )
   MODE @ READONLY-FLAG and 0 <> if E-READONLY throw then ;

: REPLACE-COPY ( ptr n n ptr u8 n -- ) {: node:ptr idx:n data:ptr len:n :}
   node WRITABLE
   node idx MEMBER-AT {: member:ptr :}
   member REMOVED? if E-ENTRY throw then
   node data len COPY-IN {: buf:ptr size:n :}
   buf member REPLACEMENT ! size member REPLACE-LEN !
   buf size CRC32 member REPLACE-CRC ! 1 node DIRTY ! ;

: FILL-BUFFER ( a ptr u8 n [ a SPAN:span<u8> -- ] -- a ptr u8 n [ a SPAN:span<u8> -- ] )
   {: ctx buf:ptr len:n fill :}
   ctx buf len SPAN:MAKE fill execute
   ctx buf len fill ;

: REPLACE-FILL ( ptr n n n a [ a SPAN:span<u8> -- ] -- )
   {: node:ptr idx:n len:n ctx fill :}
   node WRITABLE node idx MEMBER-AT {: member:ptr :}
   member REMOVED? if E-ENTRY throw then
   node len BUFFER-ALLOC {: buf:ptr :}
   ctx buf len fill [: FILL-BUFFER ;] catch >r 2drop 2drop r> {: code:n :}
   code 0<> if node buf BUFFER-RELEASE code throw then
   buf member REPLACEMENT ! len member REPLACE-LEN !
   buf len CRC32 member REPLACE-CRC ! 1 node DIRTY ! ;

public

: OPEN ( ptr u8 n -- archive ) READONLY-FLAG OPEN-CHECKED ;
: EDIT ( ptr u8 n -- archive ) 0 OPEN-CHECKED ;
: OPEN-FILE ( ptr u8 n -- archive ) OPEN ;
: EDIT-FILE ( ptr u8 n -- archive ) EDIT ;
: SOURCE$ ( archive -- ptr u8 n )
   ARCHIVE-NODE {: node:ptr :}
   node RAW-PTR @ NULL? if
      0 node RAW-LEN @ 1 2 node SNAP-FD @ 0 mmap {: mapped:n :}
      mapped -1 = if E-READ throw then
      mapped MAPPED>RAW node RAW-PTR !
   then
   node RAW-PTR @ node RAW-LEN @ ;
: COUNT ( archive -- n ) ARCHIVE-NODE COUNT-NODE ;
: ENTRY ( archive n -- entry ) swap ARCHIVE-NODE swap NEW-ENTRY ;

: NAME$ ( archive entry -- ptr u8 n )
   swap ARCHIVE-NODE dup rot ENTRY-INDEX NAME-COPY ;

: READ ( archive entry -- ptr u8 n )
   swap ARCHIVE-NODE dup rot ENTRY-INDEX READ-COPY ;

\ The central directory size OPEN validated, ZIP64 included; a pending
\ replacement answers its stored length, the bytes COMMIT writes.
: COMPRESSED-SIZE ( archive entry -- n )
   swap ARCHIVE-NODE dup rot ENTRY-INDEX MEMBER-AT OUTPUT-COMPRESSED ;

: OPEN-ENTRY ( archive entry -- input )
   swap ARCHIVE-NODE dup rot ENTRY-INDEX OPEN-INPUT ;

: OPEN-SOURCE ( archive -- input ) ARCHIVE-NODE OPEN-SOURCE-INPUT ;

\ A caller-owned aligned cell can retain an archive through its lifetime.
: ARCHIVE-SLOT ( ptr n -- ptr archive ) >ARCHIVE-SLOT ;

\ A caller-owned aligned cell can hold an input across a raw callback context.
: INPUT-SLOT ( ptr n -- ptr input ) >INPUT-SLOT ;

: READ-IN ( input SPAN:span<u8> -- n ) READ-IN-CHECKED ;
: CLOSE-ENTRY ( input -- ) CLOSE-IN-CHECKED ;

: REPLACE ( archive entry ptr u8 n -- ) {: archive:archive entry:entry data:ptr len:n :}
   archive ARCHIVE-NODE {: node:ptr :}
   node node entry ENTRY-INDEX data len REPLACE-COPY ;

\ Staged omission keeps the opened view and entry identities stable until close.
: REMOVE ( archive entry -- ) {: archive:archive entry:entry :}
   archive ARCHIVE-NODE {: node:ptr :}
   node entry ENTRY-INDEX {: idx:n :}
   node WRITABLE
   1 node idx MEMBER-AT REMOVED !
   1 node DIRTY ! ;

: REPLACE-WITH ( archive entry n a [ a SPAN:span<u8> -- ] -- )
   {: archive:archive entry:entry len:n ctx fill :}
   archive ARCHIVE-NODE {: node:ptr :}
   node node entry ENTRY-INDEX len ctx fill REPLACE-FILL ;

: COMMIT ( archive -- )
   ARCHIVE-NODE dup COMMIT-RAW DISCARD-NODE ;

: CLOSE ( archive -- ) ARCHIVE-NODE DISCARD-NODE ;

;using
;using
;package
