\ content-key.f - manifest-hashed content cache keys.
\
\ The module lives in `package CONTENT-KEY`. A key is built inside one
\ CONTENT-KEY:WITH-FOLD scope: its body is handed a CONTENT-KEY:fold, a linear
\ token naming the scope's own storage, and CONTENT-KEY:TEXT+ / FILE+ /
\ FILE-NAMED+ / DIGEST+ take that fold and give it back. CONTENT-KEY:FINAL /
\ FINAL-HEX write the digest and consume the fold. The checker refuses a body
\ that drops its fold unfinalized or copies it (to finalize twice or fold after
\ FINAL); WITH-FOLD refuses at run time a body that hands its own fold back
\ out unfinalized (E-CK-HANDLE). Because every operation names its own fold,
\ two keys derived at overlapping times cannot mix bytes. FILE+ digests the
\ file's current bytes on every call. Each fold owns a SHA-256 context; its complete preimage is never
\ retained. FILE-NAMED+ reads the physical path but folds only the logical name
\ and the content, so a build that names sources root-relative shares its key
\ across identical trees in different workspaces. No environment variable
\ enters a key.
\
\ STORAGE CLASS. SCOPE-OWNED: each WITH-FOLD allocates one MEM:WITH-BYTES
\ mapping holding the fold's digest context, its file-digest context and its
\ scratch, and releases it when the body returns or throws. The module keeps no
\ storage of its own, so folds nest without bound and tasks fold independently.
\
\ Requires the SHA-256 words; native bin/hb already carries src/core/sha256.f.

require src/lib/errors.f
require src/lib/string.f
require src/lib/memory.f

package CONTENT-KEY

$54 constant CK-TEXT-TAG
$46 constant CK-FILE-TAG
$44 constant CK-DIGEST-TAG
$FFFF constant CK-FRAG-MAX       \ the two length bytes a text or file fragment carries

public

-6920 constant E-CK-FIRST
-6929 constant E-CK-LAST
-6920 constant E-CK-HANDLE   \ a WITH-FOLD body returned with its own fold unfinalized: it finalized another scope's or handed its own out

DEFLINEAR CONTENT-KEY:fold

private

\ A fold's scope bytes, by byte offset: the digest context at 0 (cell-aligned,
\ as WITH-BYTES hands a cell-aligned base), the file-digest context after it
\ (SHA256-CTX-BYTES is a whole number of cells), then the binary digest
\ FINAL-HEX renders, the file digest FILE-NAMED+ folds, the one byte a tag or
\ length is folded through and the byte FINAL sets once it has finalized the
\ fold.
32 constant DIGEST-LEN
SHA256-CTX-BYTES constant FILE-CTX-OFF
FILE-CTX-OFF SHA256-FILE-CTX-BYTES + constant DG-OFF
DG-OFF DIGEST-LEN + constant FILE-DG-OFF
FILE-DG-OFF DIGEST-LEN + constant BYTE-OFF
BYTE-OFF 1+ constant DONE-OFF
DONE-OFF 1+ constant SCOPE-LEN

\ A fold token is its scope's base; only these rows cross it.
LINEAR: MINT ( ptr u8 -- CONTENT-KEY:fold )
LINEAR: ERASE ( CONTENT-KEY:fold -- ptr u8 )

: STATE ( CONTENT-KEY:fold -- CONTENT-KEY:fold ptr u8 )
   ERASE dup MINT swap ;

: U8+ ( ptr u8 n -- ) {: st:ptr c:n :}
   c 0 < if E-STR-BOUNDS throw then
   c STR-BYTE-MAX > if E-STR-BOUNDS throw then
   c st BYTE-OFF + c!
   st st BYTE-OFF + 1 SHA256-FEED ;

: BYTES+ ( ptr u8 ptr u8 n -- ) {: st:ptr a:ptr u:n :}
   st a u SHA256-FEED ;

\ A fragment is its tag, its length in two little-endian bytes and its bytes:
\ FILE+ folds the file's name, and a path of PATH-CAP bytes has to fold whole.
: FRAG+ ( ptr u8 n ptr u8 n -- ) {: st:ptr tag:n a:ptr u:n :}
   u 0 < if E-STR-BOUNDS throw then
   u CK-FRAG-MAX > if E-STR-BOUNDS throw then
   st tag U8+
   st u STR-BYTE-MAX and U8+
   st u 8 rshift U8+
   st a u BYTES+ ;

: DIGEST-FRAG+ ( ptr u8 ptr u8 -- ) {: st:ptr dg:ptr :}
   st CK-DIGEST-TAG U8+
   st DIGEST-LEN U8+
   st dg DIGEST-LEN BYTES+ ;

: FILE-DIGEST! ( ptr u8 ptr u8 n -- ) {: st:ptr a:ptr u:n :}
   st FILE-CTX-OFF + a u st FILE-DG-OFF + SHA256-FILE-IN
   dup 0 <> if throw then drop ;

\ FINAL consumes whichever fold it is handed, and a nested body holds its outer
\ scope's fold as well as its own, so conservation alone admits a body that
\ finalizes the outer fold and hands its own back out. The mark tells the two
\ apart: a body that returns with its own scope unfinalized is refused before
\ the scope releases the storage the escaped token names.
: FINAL! ( ptr u8 -- )
   DONE-OFF + 1 swap c! ;

: FOLD-RUN ( R [ R CONTENT-KEY:fold -- S ] ptr u8 NUM:alloc-byte-len -- S )
   drop {: body st:ptr :}
   st SHA256-BEGIN
   0 st DONE-OFF + c!
   st MINT body execute
   st DONE-OFF + c@ 0= if E-CK-HANDLE throw then ;

public

\ Run `body` with a fresh fold over its own scope storage; the body finalizes
\ the fold, and the storage is released when the body returns or throws.
: WITH-FOLD ( R [ R CONTENT-KEY:fold -- S ] -- S )
   SCOPE-LEN MEM:BYTES-ALLOC-LEN [: FOLD-RUN ;] MEM:WITH-BYTES ;

: TEXT+ ( CONTENT-KEY:fold ptr u8 n -- CONTENT-KEY:fold ) {: a:ptr u:n :}
   STATE CK-TEXT-TAG a u FRAG+ ;

: DIGEST+ ( CONTENT-KEY:fold ptr u8 -- CONTENT-KEY:fold ) {: dg:ptr :}
   STATE dg DIGEST-FRAG+ ;

\ Read the physical path, but fold the supplied logical name. A build can name
\ a source relative to its root without changing where its bytes are read.
: FILE-NAMED+ ( CONTENT-KEY:fold ptr u8 n ptr u8 n -- CONTENT-KEY:fold )
   {: a:ptr u:n name:ptr size:n :}
   STATE {: st:ptr :}
   st CK-FILE-TAG name size FRAG+
   st a u FILE-DIGEST!
   st st FILE-DG-OFF + DIGEST-FRAG+ ;

: FILE+ ( CONTENT-KEY:fold ptr u8 n -- CONTENT-KEY:fold ) {: a:ptr u:n :}
   a u a u FILE-NAMED+ ;

\ Write the fold's 32-byte digest to `dst`, consuming the fold.
: FINAL ( CONTENT-KEY:fold ptr u8 -- ) {: dst:ptr :}
   ERASE {: st:ptr :}
   st dst SHA256-END
   st FINAL! ;

\ Write the fold's digest to `hex` as 64 lower-case hex digits, consuming the
\ fold.
: FINAL-HEX ( CONTENT-KEY:fold ptr u8 -- ) {: hex:ptr :}
   ERASE {: st:ptr :}
   st st DG-OFF + SHA256-END
   st DG-OFF + hex SHA256>HEX
   st FINAL! ;

;package
