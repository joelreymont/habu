\ content-key-test.f - focused tests for content-key digest caching.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/memory.f lib/fs.f lib/fs-mutate.f lib/content-key.f lib/content-key-test.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/content-key.f

$1000 constant CKT-READ-CAP
64 constant CKT-KEY-LEN

variable CKT-ROOT-U
variable CKT-SRC-U
variable CKT-CACHE-U

create CKT-ROOT FS-PATH-CAP allot
create CKT-SRC FS-PATH-CAP allot
create CKT-CACHE FS-PATH-CAP allot
create CKT-KEY1 80 allot
create CKT-KEY2 80 allot
create CKT-READ CKT-READ-CAP allot

: CKT-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u:n dst:ptr lenp:ptr :}
   a dst u BYTE-COPY
   u lenp ! ;

: CKT-PATH! ( ptr u8 n ptr u8 n ptr u8 ptr n -- )
   {: pa:ptr pu:n na:ptr nu:n dst:ptr lenp:ptr :}
   pa pu na nu dst JOIN-PATH lenp ! ;

: CKT-ROOT$ ( -- ptr u8 n )
   CKT-ROOT CKT-ROOT-U @ ;

: CKT-SRC$ ( -- ptr u8 n )
   CKT-SRC CKT-SRC-U @ ;

: CKT-CACHE$ ( -- ptr u8 n )
   CKT-CACHE CKT-CACHE-U @ ;

: CKT-SETUP ( -- )
   CLEANUP-RESET
   s" habu-content-key" TMPDIR-MKDIR CKT-ROOT CKT-ROOT-U CKT-COPY!
   CKT-ROOT$ CLEANUP-TREE+
   CKT-ROOT$ s" src.f" CKT-SRC CKT-SRC-U CKT-PATH!
   CKT-ROOT$ s" content-key.cache" CKT-CACHE CKT-CACHE-U CKT-PATH!
   CK-CACHE-CLEAR!
   CKT-CACHE$ CK-CACHE-PATH! ;

: CKT-KEY! ( ptr u8 -- ) {: dst:ptr :}
   CK-RESET
   s" content-key-test" CK-TEXT+
   CKT-SRC$ CK-FILE+
   dst CK-FINAL-HEX ;

: CKT-LF-COUNT ( ptr u8 n -- n ) {: a:ptr u:n :}
   0 0 begin dup u < while
      dup a + c@ STR-LF = if swap 1+ swap then
      1+
   repeat drop ;

: CKT-CACHE-ROWS ( -- n )
   CKT-CACHE$ CKT-READ CKT-READ-CAP READ-ALL
   CKT-READ swap CKT-LF-COUNT ;

: CKT-CACHE-STABLE-HIT ( -- )
   CKT-SRC$ s" alpha" WRITE-ALL
   CKT-KEY1 CKT-KEY!
   CKT-CACHE$ FILE? TTRUE
   CKT-CACHE-ROWS 1 T=
   CKT-KEY2 CKT-KEY!
   CKT-KEY2 CKT-KEY-LEN CKT-KEY1 CKT-KEY-LEN T$=
   CKT-CACHE-ROWS 1 T= ;

: CKT-CACHE-INVALIDATES ( -- )
   CKT-SRC$ s" beta-more" WRITE-ALL
   CKT-KEY2 CKT-KEY!
   CKT-KEY2 CKT-KEY-LEN CKT-KEY1 CKT-KEY-LEN T$<>
   CKT-CACHE-ROWS 2 T= ;

: CKT-CLEANUP ( -- )
   CK-CACHE-CLEAR!
   CLEANUP-RUN
   CKT-ROOT$ EXISTS? TFALSE ;

: CKT-MAIN ( -- )
   T-RESET
   CKT-SETUP
   CKT-CACHE-STABLE-HIT
   CKT-CACHE-INVALIDATES
   CKT-CLEANUP
   T-REPORT ;

CKT-MAIN
