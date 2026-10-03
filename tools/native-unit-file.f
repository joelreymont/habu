\ The NBR object file owns only portable bytes. Its caller supplies six semantic
\ sections in order: code, dictionary records, calls, address sites, protected
\ wordlists and checker state. No running pointer is written into this header.
\
\ Version 1 header (all integers are little-endian u64): magic at 0, version at
\ 8, architecture at 16, package-name length at 24, package name at 32 (64
\ bytes), source key at 96 (64 bytes), and six section lengths at 160.

require lib/errors.f
require lib/string.f
require lib/fs.f
require lib/fs-mutate.f
require lib/le.f

package NUNIT-FILE

private

8 constant MAGIC-BYTES
8 constant FIELD-BYTES
64 constant NAME-CAP
64 constant KEY-BYTES
6 constant SECTION-COUNT
160 constant LENGTHS-OFF
208 constant HEADER-BYTES
$4000000 constant FILE-CAP
48 constant RECORD-BYTES
1 constant FORMAT-VERSION
1 constant ARCH-ARM64

DYNAMIC-BUFFER FILE-BUF u8
DYNAMIC-BUFFER SECTION-A ptr u8
DYNAMIC-BUFFER SECTION-U n
variable SECTION-MASK
variable READY
variable SECTION-OFF

: BAD ( -- ) E-NUNIT-FILE throw ;

: SLOT-CK ( n -- n ) {: slot:n :}
   slot 0 < slot SECTION-COUNT >= or if BAD then
   slot ;

\ A package name past the header's field is one this format cannot represent.
: META-CK ( n ptr u8 n ptr u8 n -- )
   {: arch:n name:ptr nameu:n key:ptr keyu:n :}
   arch ARCH-ARM64 <> if BAD then
   nameu 1 < if BAD then
   nameu NAME-CAP > if E-NUNIT-PROFILE throw then
   keyu KEY-BYTES <> if BAD then ;

: FIELD! ( n n -- )
   FILE-BUF LE:U64! ;

: FIELD@ ( n -- n )
   FILE-BUF LE:U64@ ;

: SECTION-LEN ( n -- n ) {: slot:n :}
   slot SECTION-U @ ;

: SECTION-LEN! ( n n -- ) {: size:n slot:n :}
   size slot SECTION-U ! ;

: SECTION-PTR ( n -- ptr u8 ) {: slot:n :}
   slot SECTION-A @ ;

: SECTION-PTR! ( ptr u8 n -- ) {: a:ptr slot:n :}
   a slot SECTION-A ! ;

: TABLE-OPEN ( -- )
   SECTION-COUNT SECTION-A-RESERVE
   SECTION-COUNT SECTION-U-RESERVE ;

: SIZE+ ( n n -- n ) {: total:n size:n :}
   size 0 < size FILE-CAP total - > or if BAD then
   total size + ;

: WRITE-SIZE ( -- n )
   SECTION-MASK @ 63 <> if BAD then
   HEADER-BYTES
   SECTION-COUNT 0 ?do
      i SECTION-LEN SIZE+
   loop ;

: CHECK-RECORD-SIZE ( -- )
   1 SECTION-LEN RECORD-BYTES mod 0<> if BAD then ;

: WRITE-HEADER ( n ptr u8 n ptr u8 n -- )
   {: arch:n name:ptr nameu:n key:ptr keyu:n :}
   HEADER-BYTES 0 ?do 0 i FILE-BUF c! loop
   s" HBNUNIT1" drop 0 FILE-BUF MAGIC-BYTES BYTE-COPY
   FORMAT-VERSION 8 FIELD!
   arch 16 FIELD!
   nameu 24 FIELD!
   name 32 FILE-BUF nameu BYTE-COPY
   key 96 FILE-BUF KEY-BYTES BYTE-COPY
   SECTION-COUNT 0 ?do
      i SECTION-LEN LENGTHS-OFF i FIELD-BYTES * + FIELD!
   loop ;

: WRITE-SECTIONS ( -- )
   HEADER-BYTES SECTION-OFF !
   SECTION-COUNT 0 ?do
      \ A zero-byte tail begins at EOF; only the base is indexed.
      i SECTION-PTR 0 FILE-BUF SECTION-OFF @ + i SECTION-LEN BYTE-COPY
      i SECTION-LEN SECTION-OFF +!
   loop ;

: READ-HEADER ( n ptr u8 n ptr u8 n n -- )
   {: arch:n name:ptr nameu:n key:ptr keyu:n size:n :}
   size HEADER-BYTES < if BAD then
   0 FILE-BUF MAGIC-BYTES s" HBNUNIT1" STR= 0= if BAD then
   8 FIELD@ FORMAT-VERSION <> if BAD then
   16 FIELD@ arch <> if BAD then
   24 FIELD@ nameu <> if BAD then
   32 FILE-BUF nameu name nameu STR= 0= if BAD then
   96 FILE-BUF KEY-BYTES key keyu STR= 0= if BAD then ;

: READ-SECTIONS ( n -- ) {: size:n :}
   HEADER-BYTES SECTION-OFF !
   SECTION-COUNT 0 ?do
      LENGTHS-OFF i FIELD-BYTES * + FIELD@ {: bytes:n :}
      bytes size SECTION-OFF @ - > if BAD then
      SECTION-OFF @ bytes SIZE+ SECTION-OFF !
      \ The validated offset may equal size for an empty trailing section.
      0 FILE-BUF SECTION-OFF @ bytes - + i SECTION-PTR!
      bytes i SECTION-LEN!
   loop
   SECTION-OFF @ size <> if BAD then
   CHECK-RECORD-SIZE ;

public

ARCH-ARM64 constant ARCH-AARCH64

: CLOSE ( -- )
   0 READY ! 0 SECTION-MASK !
   FILE-BUF-RELEASE SECTION-A-RELEASE SECTION-U-RELEASE ;

: RESET ( -- )
   CLOSE TABLE-OPEN ;

: SECTION! ( n ptr u8 n -- ) {: slot:n bytes:ptr size:n :}
   slot SLOT-CK drop
   size 0 < size FILE-CAP > or if BAD then
   bytes slot SECTION-PTR!
   size slot SECTION-LEN!
   SECTION-MASK @ 1 slot lshift or SECTION-MASK ! ;

: WRITE ( ptr u8 n n ptr u8 n ptr u8 n -- )
   {: path:ptr pathu:n arch:n name:ptr nameu:n key:ptr keyu:n :}
   arch name nameu key keyu META-CK
   WRITE-SIZE {: size:n :}
   CHECK-RECORD-SIZE
   size FILE-BUF-RESERVE
   arch name nameu key keyu WRITE-HEADER
   WRITE-SECTIONS
   path pathu 0 FILE-BUF size ATOMIC-WRITE-FILE
   FILE-BUF-RELEASE ;

: READ ( ptr u8 n n ptr u8 n ptr u8 n -- )
   {: path:ptr pathu:n arch:n name:ptr nameu:n key:ptr keyu:n :}
   arch name nameu key keyu META-CK
   RESET
   path pathu FILE-SIZE {: size:n :}
   size HEADER-BYTES < size FILE-CAP > or if BAD then
   size FILE-BUF-RESERVE
   path pathu 0 FILE-BUF size READ-ALL size <> if BAD then
   arch name nameu key keyu size READ-HEADER
   size READ-SECTIONS
   1 READY ! ;

: SECTION$ ( n -- ptr u8 n ) {: slot:n :}
   READY @ 0= if BAD then
   slot SLOT-CK dup SECTION-PTR swap SECTION-LEN ;

;package
