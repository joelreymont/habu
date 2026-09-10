\ zip.f - indexed ZIP reading and byte-preserving replacement.
require lib/zip-raw.f

package ZIP
using FFI
using MEM

: PATH-CHECK ( ptr u8 n -- ) {: path:ptr len:n :}
   len SIZE-CHECK len 0= if E-PATH throw then
   len 0 ?do path i + c@ 0= if E-PATH throw then loop ;

: OPEN-PATH ( archive ptr u8 n n -- archive ptr u8 n n ) {: archive:archive path:ptr len:n flags:n :}
   archive ARCHIVE-NODE {: node:ptr :}
   node len 1+ BUFFER-ALLOC {: cpath:ptr :}
   path len cpath CSTR cpath flags C-OPEN {: czip:ptr :}
   czip NULL? if E-OPEN throw then
   czip node OBJECT ! flags node MODE !
   cpath node PATH-PTR ! len node PATH-LEN ! node RAW-LOAD
   archive path len flags ;

: OPEN-ATTEMPT ( archive ptr u8 n n -- n )
   [: OPEN-PATH ;] catch >r 2drop 2drop r> ;

: OPEN-CHECKED ( ptr u8 n n -- archive ) {: path:ptr len:n flags:n :}
   path len PATH-CHECK REGISTER {: archive:archive :}
   archive path len flags OPEN-ATTEMPT {: code:n :}
   code 0 <> if archive ARCHIVE-NODE DISCARD-NODE code throw then
   archive ;

: COUNT-NODE ( ptr n -- n )
   OBJECT @ C-COUNT dup 0 < if E-READ throw then ;

: INDEX-CHECK ( ptr n n -- ) {: node:ptr idx:n :}
   idx 0 < if E-ENTRY throw then
   idx node COUNT-NODE >= if E-ENTRY throw then ;

: NEW-ENTRY ( ptr n n -- entry ) {: archive:ptr idx:n :}
   archive idx INDEX-CHECK NEXT-ID NODE-ALLOC {: ident:n node:ptr :}
   ident node ID ! idx node INDEX ! node archive ENTRIES ADD-NODE
   ident >ENTRY ;

: NAME-COPY ( ptr n n -- ptr u8 n ) {: node:ptr idx:n :}
   node idx MEMBER-AT {: member:ptr :}
   node node member CENTRAL-NAME COPY-IN ;

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

: WRITABLE ( ptr n -- )
   MODE @ READONLY-FLAG and 0 <> if E-READONLY throw then ;

: REPLACE-COPY ( ptr n n ptr u8 n -- ) {: node:ptr idx:n data:ptr len:n :}
   node WRITABLE node data len COPY-IN {: buf:ptr size:n :}
   node idx MEMBER-AT {: member:ptr :}
   buf member REPLACEMENT ! size member REPLACE-LEN !
   buf size CRC32 member REPLACE-CRC ! 1 node DIRTY ! ;

public

: OPEN ( ptr u8 n -- archive ) READONLY-FLAG OPEN-CHECKED ;
: EDIT ( ptr u8 n -- archive ) 0 OPEN-CHECKED ;
: SOURCE$ ( archive -- ptr u8 n )
   ARCHIVE-NODE dup RAW-PTR @ swap RAW-LEN @ ;
: COUNT ( archive -- n ) ARCHIVE-NODE COUNT-NODE ;
: ENTRY ( archive n -- entry ) swap ARCHIVE-NODE swap NEW-ENTRY ;

: NAME$ ( archive entry -- ptr u8 n )
   swap ARCHIVE-NODE dup rot ENTRY-INDEX NAME-COPY ;

: READ ( archive entry -- ptr u8 n )
   swap ARCHIVE-NODE dup rot ENTRY-INDEX READ-COPY ;

: REPLACE ( archive entry ptr u8 n -- ) {: archive:archive entry:entry data:ptr len:n :}
   archive ARCHIVE-NODE {: node:ptr :}
   node node entry ENTRY-INDEX data len REPLACE-COPY ;

: COMMIT ( archive -- )
   ARCHIVE-NODE dup COMMIT-RAW DISCARD-NODE ;

: CLOSE ( archive -- ) ARCHIVE-NODE DISCARD-NODE ;

;using
;using
;package
