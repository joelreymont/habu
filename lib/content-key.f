\ content-key.f - manifest-hashed content cache keys.
\
\ Requires SHA256 words; native bin/hb already carries src/core/sha256.f.
\ Load after lib/errors.f, lib/string.f, and lib/fs.f.

$40000 constant CK-CAP
$54 constant CK-TEXT-TAG
$46 constant CK-FILE-TAG
$44 constant CK-DIGEST-TAG

create CK-BUF CK-CAP allot
create CK-DG 40 allot
create CK-FILE-DG 40 allot

variable CK-U

: CK-RESET ( -- )
   0 CK-U ! ;

: CK-CAP-CHECK ( n -- ) {: n:n :}
   n 0 < if E-STR-BOUNDS throw then
   CK-U @ n + CK-CAP > if E-STR-CAPACITY throw then ;

: CK-U8+ ( n -- ) {: c:n :}
   1 CK-CAP-CHECK
   c 0 < if E-STR-BOUNDS throw then
   c STR-BYTE-MAX > if E-STR-BOUNDS throw then
   c CK-BUF CK-U @ + c!
   CK-U @ 1+ CK-U ! ;

: CK-BYTES+ ( ptr u8 n -- ) {: a:ptr u:n :}
   u CK-CAP-CHECK
   a CK-BUF CK-U @ + u BYTE-COPY
   CK-U @ u + CK-U ! ;

: CK-FRAG+ ( n ptr u8 n -- ) {: tag:n a:ptr u:n :}
   u 0 < if E-STR-BOUNDS throw then
   u STR-BYTE-MAX > if E-STR-BOUNDS throw then
   tag CK-U8+
   u CK-U8+
   a u CK-BYTES+ ;

: CK-TEXT+ ( ptr u8 n -- )
   CK-TEXT-TAG -rot CK-FRAG+ ;

: CK-DIGEST+ ( ptr u8 -- )
   CK-DIGEST-TAG CK-U8+
   32 CK-U8+
   32 CK-BYTES+ ;

: CK-FILE+ ( ptr u8 n -- ) {: a:ptr u:n :}
   CK-FILE-TAG a u CK-FRAG+
   a u CK-FILE-DG SHA256-FILE dup 0 <> if throw then drop
   CK-FILE-DG CK-DIGEST+ ;

: CK-FINAL ( ptr u8 -- ) {: dst:ptr :}
   CK-BUF CK-U @ dst SHA256 ;

: CK-FINAL-HEX ( ptr u8 -- ) {: hex:ptr :}
   CK-DG CK-FINAL
   CK-DG hex SHA256>HEX ;
