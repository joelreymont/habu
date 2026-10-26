\ This object file owns only portable bytes: a header and twenty sections
\ in slot order. Each caller names what its slots hold (NUNIT-OBJECT's
\ DEFINITION and FILE objects); an unused slot is empty.
\
\ Version 16 header (all integers are little-endian u64): magic at 0, version at
\ 8, architecture at 16, package-name length at 24, package name at 32 (64
\ bytes), source key at 96 (64 bytes), and twenty section lengths at 160.

require src/lib/errors.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/le.f
require src/lib/memory.f

package NUNIT-FILE

public

20 constant SECTION-COUNT

private

8 constant MAGIC-BYTES
8 constant FIELD-BYTES
64 constant NAME-CAP
64 constant KEY-BYTES
160 constant LENGTHS-OFF
320 constant HEADER-BYTES
25 constant FORMAT-VERSION
1 constant ARCH-ARM64

DYNAMIC-BUFFER FILE-BUF u8
DYNAMIC-BUFFER SECTION-A ptr u8
DYNAMIC-BUFFER SECTION-U n
variable SECTION-MASK
variable READY

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
   size 0 < size MEM-MAX-N total - > or if BAD then
   total size + ;

: WRITE-SIZE ( -- n )
   SECTION-MASK @ 1 SECTION-COUNT lshift 1- <> if BAD then
   HEADER-BYTES
   SECTION-COUNT 0 ?do
      i SECTION-LEN SIZE+
   loop ;

: WRITE-HEADER ( n ptr u8 n ptr u8 n -- )
   {: arch:n name:ptr nameu:n key:ptr keyu:n :}
   HEADER-BYTES 0 ?do 0 i FILE-BUF c! loop
   s" HBNUNIT2" drop 0 FILE-BUF MAGIC-BYTES BYTE-COPY
   FORMAT-VERSION 8 FIELD!
   arch 16 FIELD!
   nameu 24 FIELD!
   name 32 FILE-BUF nameu BYTE-COPY
   key 96 FILE-BUF KEY-BYTES BYTE-COPY
   SECTION-COUNT 0 ?do
      i SECTION-LEN LENGTHS-OFF i FIELD-BYTES * + FIELD!
   loop ;

: WRITE-SECTIONS ( -- )
   HEADER-BYTES
   SECTION-COUNT 0 ?do
      {: off:n :}
      \ A zero-byte tail begins at EOF; only the base is indexed.
      i SECTION-PTR 0 FILE-BUF off + i SECTION-LEN BYTE-COPY
      off i SECTION-LEN +
   loop drop ;

: READ-HEADER ( n ptr u8 n ptr u8 n ptr u8 n -- )
   {: arch:n name:ptr nameu:n key:ptr keyu:n base:ptr size:n :}
   size HEADER-BYTES < if BAD then
   base MAGIC-BYTES s" HBNUNIT2" STR= 0= if BAD then
   base 8 + LE:U64@ FORMAT-VERSION <> if BAD then
   base 16 + LE:U64@ arch <> if BAD then
   base 24 + LE:U64@ nameu <> if BAD then
   base 32 + nameu name nameu STR= 0= if BAD then
   base 96 + KEY-BYTES key keyu STR= 0= if BAD then ;

: VIEW-SLOT ( ptr u8 n n n n -- n )
   {: base:ptr size:n off:n bytes:n slot:n :}
   bytes 0 < bytes size off - > or if BAD then
   \ The validated offset may equal size for an empty trailing section.
   base off + slot SECTION-PTR!
   bytes slot SECTION-LEN!
   off bytes SIZE+ ;

: READ-SECTIONS ( ptr u8 n -- )
   {: base:ptr size:n :}
   HEADER-BYTES
   SECTION-COUNT 0 ?do
      {: off:n :}
      base size off base LENGTHS-OFF i FIELD-BYTES * + + LE:U64@ i VIEW-SLOT
   loop
   size <> if BAD then ;

public

ARCH-ARM64 constant ARCH-AARCH64

: CLOSE ( -- )
   0 READY ! 0 SECTION-MASK !
   FILE-BUF-RELEASE SECTION-A-RELEASE SECTION-U-RELEASE ;

: RESET ( -- )
   CLOSE TABLE-OPEN ;

\ Definition objects reuse the file and section arenas for a whole source
\ load.  The ordinary FILE reset still releases them at its boundary.
: RESET-KEEP ( -- )
   0 READY ! 0 SECTION-MASK ! TABLE-OPEN ;

: SECTION! ( n ptr u8 n -- ) {: slot:n bytes:ptr size:n :}
   slot SLOT-CK drop
   size 0 < if BAD then
   bytes slot SECTION-PTR!
   size slot SECTION-LEN!
   SECTION-MASK @ 1 slot lshift or SECTION-MASK ! ;

: PACK ( n ptr u8 n ptr u8 n -- ptr u8 n )
   {: arch:n name:ptr nameu:n key:ptr keyu:n :}
   arch name nameu key keyu META-CK
   WRITE-SIZE {: size:n :}
   size FILE-BUF-RESERVE
   arch name nameu key keyu WRITE-HEADER
   WRITE-SECTIONS
   0 FILE-BUF size ;

: WRITE ( ptr u8 n n ptr u8 n ptr u8 n -- )
   {: path:ptr pathu:n arch:n name:ptr nameu:n key:ptr keyu:n :}
   arch name nameu key keyu PACK {: bytes:ptr size:n :}
   path pathu bytes size ATOMIC-WRITE-FILE
   FILE-BUF-RELEASE ;

: READ-BYTES ( ptr u8 n n ptr u8 n ptr u8 n -- )
   {: bytes:ptr size:n arch:n name:ptr nameu:n key:ptr keyu:n :}
   arch name nameu key keyu META-CK
   RESET
   size HEADER-BYTES < if BAD then
   size FILE-BUF-RESERVE
   bytes 0 FILE-BUF size BYTE-COPY
   arch name nameu key keyu 0 FILE-BUF size READ-HEADER
   0 FILE-BUF size READ-SECTIONS
   1 READY ! ;

\ The caller keeps these bytes immutable until the sections are reset or closed.
: READ-VIEW ( ptr u8 n n ptr u8 n ptr u8 n -- )
   {: bytes:ptr size:n arch:n name:ptr nameu:n key:ptr keyu:n :}
   arch name nameu key keyu META-CK
   RESET-KEEP
   arch name nameu key keyu bytes size READ-HEADER
   bytes size READ-SECTIONS
   1 READY ! ;

\ Sections over bytes packed back to back: one u64 length in lengths for each
\ slot set in mask, in slot order; every other slot is empty. The caller keeps
\ both spans immutable until the sections are reset or closed.
: SECTIONS-VIEW ( n ptr u8 ptr u8 n -- )
   {: mask:n lengths:ptr bytes:ptr size:n :}
   mask 0 < mask 1 SECTION-COUNT lshift >= or if BAD then
   RESET-KEEP
   0 0
   SECTION-COUNT 0 ?do
      {: off:n row:n :}
      mask i rshift 1 and 0<> if
         bytes size off lengths row FIELD-BYTES * + LE:U64@ i VIEW-SLOT
         row 1+
      else
         bytes size off 0 i VIEW-SLOT row
      then
   loop
   drop size <> if BAD then
   1 READY ! ;

: READ ( ptr u8 n n ptr u8 n ptr u8 n -- )
   {: path:ptr pathu:n arch:n name:ptr nameu:n key:ptr keyu:n :}
   arch name nameu key keyu META-CK
   RESET
   path pathu FILE-SIZE {: size:n :}
   size HEADER-BYTES < if BAD then
   size FILE-BUF-RESERVE
   path pathu 0 FILE-BUF size READ-ALL size <> if BAD then
   arch name nameu key keyu 0 FILE-BUF size READ-HEADER
   0 FILE-BUF size READ-SECTIONS
   1 READY ! ;

: SECTION$ ( n -- ptr u8 n ) {: slot:n :}
   READY @ 0= if BAD then
   slot SLOT-CK dup SECTION-PTR swap SECTION-LEN ;

;package
