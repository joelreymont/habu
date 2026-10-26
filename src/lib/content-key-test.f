\ content-key-test.f - focused tests for content-key folds and file keys.
\ Run: bin/hb --load src/lib/content-key-test.f

require src/lib/errors.f
require src/lib/string.f
require src/lib/test.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/ffi-abi.f
require src/lib/content-key.f

\ White-box test: reopen the module's package so the fixtures reach the private
\ fragment constants and call the public builders by their bare package-local
\ names.
package CONTENT-KEY

64 constant CKT-KEY-LEN

variable CKT-ROOT-U
variable CKT-SRC-U

create CKT-ROOT FS-PATH-CAP allot
create CKT-SRC FS-PATH-CAP allot
create CKT-COPY-PATH FS-PATH-CAP allot
create CKT-KEY1 80 allot
create CKT-KEY2 80 allot
create CKT-SEQ-A 80 allot
create CKT-SEQ-B 80 allot
create CKT-INT-A 80 allot
create CKT-INT-B 80 allot

\ msync answers -1 for an address with no mapping and 0 for a mapped page, so
\ it tells whether a fold's scope storage is still mapped (macOS mincore
\ answers 0 for an unmapped page, so it cannot tell).
1 constant CKT-MS-ASYNC                 \ the same flag on macOS and Linux
4096 constant CKT-PAGE

PROCESS-SYMBOLS

FUNCTION: CKT-SYNC-RANGE msync ( n n n -- i32 ) ;FUNCTION

: CKT-MAPPED ( n -- n )
   CKT-PAGE CKT-MS-ASYNC CKT-SYNC-RANGE ;

\ The page a fold's scope storage starts in. WITH-BYTES hands the scope its
\ bytes one frame header into a fresh mapping, and a mapping starts on a page
\ of at least CKT-PAGE bytes, so rounding down to CKT-PAGE finds its base.
: CKT-SCOPE-PAGE ( CONTENT-KEY:fold -- CONTENT-KEY:fold n )
   STATE NULL-PTR - CKT-PAGE negate and ;

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

: CKT-SETUP ( -- )
   CLEANUP-RESET
   s" habu-content-key" HB-TMP-MKDIR CKT-ROOT CKT-ROOT-U CKT-COPY!
   CKT-ROOT$ CLEANUP-TREE+
   CKT-ROOT$ s" src.f" CKT-SRC CKT-SRC-U CKT-PATH! ;

: CKT-CLEANUP ( -- )
   CLEANUP-RUN
   CKT-ROOT$ EXISTS? TFALSE ;

\ ---- overlapping folds ------------------------------------------------------
\ The regression fold tokens exist for. Two folds run one after the other, then
\ the SAME two folds run overlapping - the second opened inside the first and
\ the two folded into in turn, which is what a key derived inside another key's
\ derivation does. The two must agree: a fold's bytes belong to its own token,
\ not to whoever folded last.
\
\ Against a single shared accumulator the overlapping run produced ONE key and
\ handed the same wrong value back for both folds, which turns these
\ equalities red and flips the "the two keys differ" guard to true.
: CKT-SEQ-A-BODY ( CONTENT-KEY:fold -- )
   s" alpha-1" TEXT+
   s" alpha-2" TEXT+
   CKT-SEQ-A FINAL-HEX ;

: CKT-SEQ-B-BODY ( CONTENT-KEY:fold -- )
   s" beta-1" TEXT+
   s" beta-2" TEXT+
   CKT-SEQ-B FINAL-HEX ;

: CKT-FOLD-SEQUENTIAL ( -- )
   [: CKT-SEQ-A-BODY ;] WITH-FOLD
   [: CKT-SEQ-B-BODY ;] WITH-FOLD ;

: CKT-INNER-BODY ( CONTENT-KEY:fold CONTENT-KEY:fold -- CONTENT-KEY:fold )
   s" beta-1" TEXT+
   swap
   s" alpha-2" TEXT+
   swap
   s" beta-2" TEXT+
   CKT-INT-B FINAL-HEX ;

: CKT-OUTER-BODY ( CONTENT-KEY:fold -- )
   s" alpha-1" TEXT+
   [: CKT-INNER-BODY ;] WITH-FOLD
   CKT-INT-A FINAL-HEX ;

: CKT-FOLD-OVERLAPPED ( -- )
   [: CKT-OUTER-BODY ;] WITH-FOLD ;

: CKT-FOLD-OVERLAP-MATCHES ( -- )
   CKT-FOLD-SEQUENTIAL
   CKT-FOLD-OVERLAPPED
   CKT-SEQ-A CKT-KEY-LEN CKT-INT-A CKT-KEY-LEN STR= TTRUE
   CKT-SEQ-B CKT-KEY-LEN CKT-INT-B CKT-KEY-LEN STR= TTRUE
   \ and the two keys are genuinely different keys, so the equalities above are
   \ not both passing on one repeated value.
   CKT-SEQ-A CKT-KEY-LEN CKT-SEQ-B CKT-KEY-LEN STR= TFALSE
   CKT-INT-A CKT-KEY-LEN CKT-INT-B CKT-KEY-LEN STR= TFALSE ;

\ ---- nesting is unbounded ---------------------------------------------------
\ Each fold owns its scope storage, so 64 folds open at once all fold and
\ finish, each to the key one lone fold of the same text gives.
64 constant CKT-NEST-N

variable CKT-NEST-DONE

: CKT-NEST-TEXT ( CONTENT-KEY:fold -- CONTENT-KEY:fold )
   s" nest" TEXT+ ;

: CKT-NEST-REF-BODY ( CONTENT-KEY:fold -- )
   CKT-NEST-TEXT CKT-KEY1 FINAL-HEX ;

defer CKT-NEST ( n -- )

: CKT-NEST-BODY ( n CONTENT-KEY:fold -- )
   CKT-NEST-TEXT
   swap CKT-NEST
   CKT-KEY2 FINAL-HEX
   CKT-KEY1 CKT-KEY-LEN CKT-KEY2 CKT-KEY-LEN STR= if 1 CKT-NEST-DONE +! then ;

: CKT-NEST-IMPL ( n -- )
   dup 0= if drop exit then
   1- [: CKT-NEST-BODY ;] WITH-FOLD ;

: CKT-NEST-INSTALL ( -- ) [: CKT-NEST-IMPL ;] is CKT-NEST ;
CKT-NEST-INSTALL

: CKT-FOLDS-NEST ( -- )
   [: CKT-NEST-REF-BODY ;] WITH-FOLD
   0 CKT-NEST-DONE !
   CKT-NEST-N CKT-NEST
   s" 64 nested folds all succeed" T-LABEL
   CKT-NEST-DONE @ CKT-NEST-N T= ;

\ ---- a throw releases the fold's scope -------------------------------------
\ FILE+ on a missing file throws out of the fold's body, and the scope unmaps
\ its storage on the way out: the scope's page is mapped while the body runs
\ and gone once the throw has left WITH-FOLD.
variable CKT-PAGE-AT
variable CKT-LIVE-RC

: CKT-MISSING-BODY ( CONTENT-KEY:fold -- )
   CKT-SCOPE-PAGE dup CKT-PAGE-AT !
   CKT-MAPPED CKT-LIVE-RC !
   CKT-ROOT$ s" missing.f" CKT-COPY-PATH JOIN-PATH {: u:n :}
   CKT-COPY-PATH u FILE+ CKT-KEY2 FINAL-HEX ;

: CKT-MISSING-FOLD ( -- )
   [: CKT-MISSING-BODY ;] WITH-FOLD ;

: CKT-THROW-RELEASES ( -- )
   [: CKT-MISSING-FOLD ;] catch
   CKT-PAGE-AT @ CKT-MAPPED {: rc:n gone:n :}
   s" FILE+ on a missing file throws the open refusal" T-LABEL
   rc SHA-E-OPEN T=
   s" the fold's scope is mapped while its body runs" T-LABEL
   CKT-LIVE-RC @ 0 T=
   s" the throw unmapped the fold's scope" T-LABEL
   gone -1 T= ;

\ ---- a body must finalize its own fold ------------------------------------
\ A nested body holds the outer fold as well as its own. One that finalizes the
\ outer fold and hands its own out conserves both tokens, so the checker admits
\ it; the inner scope refuses it at run time, before the outer body could fold
\ into the storage that scope releases. Both scopes are unmapped after the
\ refusal, and the next fold works.
variable CKT-OUTER-AT
variable CKT-INNER-AT

: CKT-SWAP-INNER ( CONTENT-KEY:fold CONTENT-KEY:fold -- CONTENT-KEY:fold )
   CKT-SCOPE-PAGE CKT-INNER-AT !
   swap CKT-KEY1 FINAL-HEX ;

: CKT-SWAP-OUTER ( CONTENT-KEY:fold -- )
   CKT-SCOPE-PAGE CKT-OUTER-AT !
   [: CKT-SWAP-INNER ;] WITH-FOLD
   CKT-KEY2 FINAL-HEX ;

: CKT-SWAP-FOLD ( -- )
   [: CKT-SWAP-OUTER ;] WITH-FOLD ;

: CKT-NEST-KEY2-BODY ( CONTENT-KEY:fold -- )
   CKT-NEST-TEXT CKT-KEY2 FINAL-HEX ;

: CKT-HANDLE-REFUSED ( -- )
   s" a body that finalizes the outer fold and hands its own out is refused" T-LABEL
   [: CKT-SWAP-FOLD ;] catch E-CK-HANDLE T=
   s" the refusal unmapped the inner scope" T-LABEL
   CKT-INNER-AT @ CKT-MAPPED -1 T=
   s" the refusal unmapped the outer scope" T-LABEL
   CKT-OUTER-AT @ CKT-MAPPED -1 T=
   s" a fold after the refusal folds and finishes" T-LABEL
   [: CKT-NEST-REF-BODY ;] WITH-FOLD
   [: CKT-NEST-KEY2-BODY ;] WITH-FOLD
   CKT-KEY1 CKT-KEY-LEN CKT-KEY2 CKT-KEY-LEN T$= ;

\ The fold is linear: a body that copies it to finalize twice or to fold after
\ FINAL is refused, and so is one that drops it unfinalized. The first body is
\ the control that the check resolves this package's words.
: CKT-FINAL-CONSUMES ( -- )
   s" a body that finalizes its fold once certifies" T-LABEL
   s" CKT-C-ONCE ( CONTENT-KEY:fold -- ) CKT-KEY1 FINAL-HEX"
   CHECK-QUIET-CANDIDATE! -1 T=
   s" copying a fold to finalize it twice is refused" T-LABEL
   s" CKT-C-TWICE ( CONTENT-KEY:fold -- ) dup CKT-KEY1 FINAL-HEX CKT-KEY2 FINAL-HEX"
   CHECK-QUIET-CANDIDATE! 0 T=
   s" keeping a fold to fold after FINAL is refused" T-LABEL
   s" CKT-C-AFTER ( CONTENT-KEY:fold -- ) >r r@ CKT-KEY1 FINAL-HEX r> CKT-SEQ-A 1 TEXT+ CKT-KEY2 FINAL-HEX"
   CHECK-QUIET-CANDIDATE! 0 T=
   s" dropping a fold unfinalized is refused" T-LABEL
   s" CKT-C-DROP ( CONTENT-KEY:fold -- ) CKT-SEQ-A 1 TEXT+ drop"
   CHECK-QUIET-CANDIDATE! 0 T= ;

: CKT-NAMED-BODY ( ptr u8 n ptr u8 n ptr u8 CONTENT-KEY:fold -- )
   >r {: a:ptr u:n name:ptr size:n key:ptr :} r>
   a u name size FILE-NAMED+ key FINAL-HEX ;

\ Key the file at the first path under the logical name into `key`.
: CKT-NAMED-KEY ( ptr u8 n ptr u8 n ptr u8 -- )
   [: CKT-NAMED-BODY ;] WITH-FOLD ;

: CKT-FILE-BODY ( ptr u8 n ptr u8 CONTENT-KEY:fold -- )
   >r {: a:ptr u:n key:ptr :} r>
   a u FILE+ key FINAL-HEX ;

: CKT-FILE-KEY ( ptr u8 n ptr u8 -- )
   [: CKT-FILE-BODY ;] WITH-FOLD ;

: CKT-LOGICAL-NAME ( -- )
   CKT-ROOT$ s" copy.f" CKT-COPY-PATH JOIN-PATH {: copyu:n :}
   CKT-SRC$ s" first" WRITE-ALL
   CKT-SRC$ CKT-COPY-PATH copyu COPY-FILE-STREAM
   CKT-SRC$ s" source.f" CKT-KEY1 CKT-NAMED-KEY
   CKT-COPY-PATH copyu s" source.f" CKT-KEY2 CKT-NAMED-KEY
   s" one logical name and content ignore the physical location" T-LABEL
   CKT-KEY1 CKT-KEY-LEN CKT-KEY2 CKT-KEY-LEN T$=
   CKT-COPY-PATH copyu s" renamed.f" CKT-KEY2 CKT-NAMED-KEY
   s" different logical names remain different inputs" T-LABEL
   CKT-KEY1 CKT-KEY-LEN CKT-KEY2 CKT-KEY-LEN T$<>
   CKT-COPY-PATH copyu s" second" WRITE-ALL
   CKT-COPY-PATH copyu s" source.f" CKT-KEY2 CKT-NAMED-KEY
   s" the named file is still read from its physical path" T-LABEL
   CKT-KEY1 CKT-KEY-LEN CKT-KEY2 CKT-KEY-LEN T$<>
   CKT-SRC$ CKT-KEY1 CKT-FILE-KEY
   CKT-SRC$ CKT-SRC$ CKT-KEY2 CKT-NAMED-KEY
   s" FILE+ retains its path-as-name contract" T-LABEL
   CKT-KEY1 CKT-KEY-LEN CKT-KEY2 CKT-KEY-LEN T$= ;

\ ---- fragments longer than one byte can count ------------------------------
\ A fragment carries its length in two bytes (FRAG+), so a logical name of
\ any length the tree's paths take folds whole: 256 bytes, one past what one
\ byte counts, and FS-PATH-CAP. Two names that differ only in their last byte
\ fold to different keys, so no byte past the 255th is dropped.
create CKT-NAME CK-FRAG-MAX 1+ allot

: CKT-NAME-FILL ( -- )
   CK-FRAG-MAX 1+ 0 ?do [char] a CKT-NAME i + c! loop ;

: CKT-NAMED ( n ptr u8 -- ) {: u:n key:ptr :}
   CKT-SRC$ CKT-NAME u key CKT-NAMED-KEY ;

: CKT-LONG-NAME ( n -- ) {: u:n :}
   CKT-NAME u + 1- {: last:ptr :}
   u CKT-KEY1 CKT-NAMED
   [char] b last c!
   u CKT-KEY2 CKT-NAMED
   [char] a last c!
   CKT-KEY1 CKT-KEY-LEN CKT-KEY2 CKT-KEY-LEN T$<> ;

\ The second length byte is what keeps preimages apart. Counted in one byte, a
\ 256-byte text would read as an empty one, and its bytes as the fragments that
\ follow it: here a text tag, a length of 254 and 254 bytes, which is exactly
\ what an empty text then a 254-byte text fold.
: CKT-WHOLE-TEXT-BODY ( CONTENT-KEY:fold -- )
   CKT-NAME 256 TEXT+ CKT-KEY1 FINAL-HEX ;

: CKT-SPLIT-TEXT-BODY ( CONTENT-KEY:fold -- )
   CKT-NAME 0 TEXT+ CKT-NAME 2 + 254 TEXT+ CKT-KEY2 FINAL-HEX ;

: CKT-LENGTH-UNAMBIGUOUS ( -- )
   CK-TEXT-TAG CKT-NAME c!
   254 CKT-NAME 1+ c!
   [: CKT-WHOLE-TEXT-BODY ;] WITH-FOLD
   [: CKT-SPLIT-TEXT-BODY ;] WITH-FOLD
   CKT-NAME-FILL
   CKT-KEY1 CKT-KEY-LEN CKT-KEY2 CKT-KEY-LEN T$<> ;

\ One byte past what two bytes count is refused before a byte is folded, and
\ the refusal leaves the fold's scope, which releases it.
: CKT-OVER-BODY ( CONTENT-KEY:fold -- )
   CKT-NAME CK-FRAG-MAX 1+ TEXT+ CKT-KEY1 FINAL-HEX ;

: CKT-OVER-FOLD ( -- )
   [: CKT-OVER-BODY ;] WITH-FOLD ;

: CKT-FRAG-OVER-REFUSED ( -- )
   [: CKT-OVER-FOLD ;] catch E-STR-BOUNDS T= ;

: CKT-LONG-FRAGMENTS ( -- )
   CKT-NAME-FILL
   s" a 256-byte logical name folds, and its last byte counts" T-LABEL
   256 CKT-LONG-NAME
   s" a logical name of FS-PATH-CAP bytes folds, and its last byte counts" T-LABEL
   FS-PATH-CAP CKT-LONG-NAME
   s" a 256-byte text and an empty text then its tail fold apart" T-LABEL
   CKT-LENGTH-UNAMBIGUOUS
   s" a fragment one byte past CK-FRAG-MAX is refused" T-LABEL
   CKT-FRAG-OVER-REFUSED ;

: CKT-MAIN ( -- )
   T-RESET
   CKT-FOLD-OVERLAP-MATCHES
   CKT-FOLDS-NEST
   CKT-SETUP
   CKT-THROW-RELEASES
   CKT-HANDLE-REFUSED
   CKT-FINAL-CONSUMES
   CKT-LOGICAL-NAME
   CKT-LONG-FRAGMENTS
   CKT-CLEANUP
   T-REPORT ;

CKT-MAIN

;package
