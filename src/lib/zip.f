\ zip.f - indexed ZIP reading and byte-preserving replacement.
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

: SOURCE-TEMPLATE ( ptr n -- ) {: node:ptr :}
   s" TMPDIR" GETENV dup 0= if 2drop s" /tmp" then
   {: dir:ptr len:n :}
   s" /habu-zip-source-XXXXXX" {: suffix:ptr size:n :}
   node len size + 1+ BUFFER-ALLOC {: data:ptr :}
   dir data len BYTE-COPY suffix data len + size BYTE-COPY
   0 data len size + + c!
   data node SOURCE-PTR ! len size + node SOURCE-LEN ! ;

: SOURCE-STEP ( ptr n ptr u8 n -- n ) {: node:ptr buf:ptr cap:n :}
   node SOURCE-FD @ buf cap read {: got:n :}
   got 0 < got cap > or if E-READ throw then
   got 0 > if node SNAP-FD @ >FD buf got FD-IO:WRITE-FULL then
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

: OPEN-WORK ( ptr n ptr u8 n n -- ptr n ptr u8 n n )
   {: node:ptr path:ptr len:n flags:n :}
   flags node MODE !
   node len 1+ BUFFER-ALLOC {: cpath:ptr :}
   path len cpath CSTR cpath node PATH-PTR ! len node PATH-LEN !
   node SOURCE-TEMPLATE
   node SOURCE-PTR @ node SOURCE-LEN @ 1+ C-MKSTEMP {: fd:n :}
   fd 0 < if E-OPEN throw then
   fd node SNAP-FD ! 1 node SOURCE-LIVE !
   node PATH-PTR @ open-rd {: src:n :}
   src 0 < if E-OPEN throw then src node SOURCE-FD !
   node COPY-SOURCE node CLOSE-SOURCE-FD
   node RAW-LEN @ dup END-BYTES < if E-OPEN throw then
   0 swap 1 2 node SNAP-FD @ 0 mmap {: mapped:n :}
   mapped -1 = if E-OPEN throw then
   mapped MAPPED>RAW node RAW-PTR !
   node SOURCE-PTR @ flags C-OPEN {: czip:ptr :}
   czip NULL? if E-OPEN throw then czip node OBJECT !
   node PARSE-RAW
   node $10000 BUFFER-ALLOC node RAW-SCRATCH !
   node RAW-PTR @ node RAW-LEN @ munmap 0<> if E-READ throw then
   0 MAPPED>RAW node RAW-PTR !
   node path len flags ;

: OPEN-ATTEMPT ( ptr n ptr u8 n n -- n )
   [: OPEN-WORK ;] catch >r 2drop 2drop r> ;

: OPEN-CHECKED ( ptr u8 n n -- archive )
   {: path:ptr len:n flags:n :}
   path len PATH-CHECK REGISTER {: archive:archive :}
   archive ARCHIVE-NODE {: node:ptr :}
   node path len flags OPEN-ATTEMPT {: code:n :}
   code 0<> if node code DISCARD-FAILED then
   archive ;

: COUNT-NODE ( ptr n -- n )
   OBJECT @ C-COUNT dup 0 < if E-READ throw then ;

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

: MEMBER-SIZE ( ptr n n -- n )
   swap OBJECT @ swap STAT-DATA C-STAT 0 <> if E-READ throw then
   STAT-DATA @ STAT-SIZE-VALID and 0= if E-READ throw then
   STAT-DATA STAT-SIZE-CELL cells + @ dup SIZE-CHECK ;

: READ-CHUNK ( ptr u8 ptr u8 n -- n )
   C-FREAD dup 0 <= if E-READ throw then ;

: READ-STEP ( n ptr u8 ptr u8 n -- n ) {: offset:n file:ptr buf:ptr len:n :}
   file buf offset + len offset - READ-CHUNK offset + ;

\ libzip verifies CRC when a read reaches EOF, including empty members.
: READ-EOF ( ptr u8 -- )
   EOF-BYTE 1 C-FREAD 0 <> if E-READ throw then ;

: MEMBER-READ-ALL ( ptr u8 ptr u8 n -- ptr u8 ptr u8 n ) {: file:ptr buf:ptr len:n :}
   0 begin dup len < while
      file buf len READ-STEP
   repeat drop file READ-EOF file buf len ;

: READ-ATTEMPT ( ptr u8 ptr u8 n -- n )
   [: MEMBER-READ-ALL ;] catch >r 2drop drop r> ;

: READ-COPY ( ptr n n -- ptr u8 n ) {: node:ptr idx:n :}
   node idx MEMBER-AT {: member:ptr :}
   member REPLACED? if member REPLACEMENT @ member REPLACE-LEN @ exit then
   node idx MEMBER-SIZE {: len:n :}
   node len BUFFER-ALLOC {: buf:ptr :}
   node OBJECT @ idx C-FOPEN {: file:ptr :}
   file NULL? if E-READ throw then
   file buf len READ-ATTEMPT {: code:n :}
   file C-FCLOSE 0 <> code 0 <> or if E-READ throw then
   buf len ;

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
      owner OBJECT @ idx C-FOPEN {: file:ptr :}
      file NULL? if node NODE-FREE E-READ throw then
      file node INPUT-OBJECT !
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
   node INPUT-KIND @ 0= if
      node INPUT-CRC @ U32-MAX xor node INPUT-EXPECT @ <> if E-READ throw then
   then
   1 node INPUT-DONE ! ;

: INPUT-BYTES ( ptr n ptr u8 n -- n ) {: node:ptr dst:ptr cap:n :}
   node INPUT-KIND @ 1 = if node INPUT-FD @ dst cap read exit then
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
   node INPUT-KIND @ 0= if
      node INPUT-CRC @ data got CRC-FEED node INPUT-CRC !
   then got ;

: DISCARD-INPUT ( ptr n -- n ) {: node:ptr :}
   node UNREGISTER-INPUT
   node INPUT-KIND @ 1 = if
      node INPUT-FD @ C-CLOSE-FD
   else
      node INPUT-OBJECT @ {: file:ptr :}
      file NULL? if 0 else file C-FCLOSE then
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
   node INPUT-KIND @ 0<> if node exit then
   begin node STAT-DATA BYTE-VIEW STAT-BYTES SPAN:MAKE READ-INNER 0<> while
   repeat node ;

: CLOSE-IN-CHECKED ( input -- )
   INPUT-NODE {: node:ptr :}
   node [: CLOSE-IN-WORK ;] catch {: code:n :}
   drop node DISCARD-INPUT {: close-code:n :}
   code 0<> if code throw then
   close-code 0<> if E-READ throw then ;

: WRITABLE ( ptr n -- )
   MODE @ READONLY-FLAG and 0 <> if E-READONLY throw then ;

: REPLACE-COPY ( ptr n n ptr u8 n -- ) {: node:ptr idx:n data:ptr len:n :}
   node WRITABLE node data len COPY-IN {: buf:ptr size:n :}
   node idx MEMBER-AT {: member:ptr :}
   buf member REPLACEMENT ! size member REPLACE-LEN !
   buf size CRC32 member REPLACE-CRC ! 1 node DIRTY ! ;

: FILL-BUFFER ( a ptr u8 n [ a SPAN:span<u8> -- ] -- a ptr u8 n [ a SPAN:span<u8> -- ] )
   {: ctx buf:ptr len:n fill :}
   ctx buf len SPAN:MAKE fill execute
   ctx buf len fill ;

: REPLACE-FILL ( ptr n n n a [ a SPAN:span<u8> -- ] -- )
   {: node:ptr idx:n len:n ctx fill :}
   node WRITABLE node idx MEMBER-AT {: member:ptr :}
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

: OPEN-ENTRY ( archive entry -- input )
   swap ARCHIVE-NODE dup rot ENTRY-INDEX OPEN-INPUT ;

: OPEN-SOURCE ( archive -- input ) ARCHIVE-NODE OPEN-SOURCE-INPUT ;

\ A caller-owned aligned cell can hold an input across a raw callback context.
: INPUT-SLOT ( ptr n -- ptr input ) >INPUT-SLOT ;

: READ-IN ( input SPAN:span<u8> -- n ) READ-IN-CHECKED ;
: CLOSE-ENTRY ( input -- ) CLOSE-IN-CHECKED ;

: REPLACE ( archive entry ptr u8 n -- ) {: archive:archive entry:entry data:ptr len:n :}
   archive ARCHIVE-NODE {: node:ptr :}
   node node entry ENTRY-INDEX data len REPLACE-COPY ;

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
