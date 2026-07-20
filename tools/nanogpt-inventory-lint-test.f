\ nanogpt-inventory-lint-test.f - red-first fixtures for the nanoGPT inventory lint.
\ Load after lib/test.f and tools/nanogpt-inventory-lint-core.f.
\
\ Each fixture is a minimal inventory doc pointed at the REAL .dots tree, so the
\ three named failure classes fire on a deliberately wrong owner manifest before
\ the live inventory proves clean:
\   - UNKNOWN         : an owner id that exists in no dot file
\   - STATUS-MISMATCH : a closed dot manifested as a live (open) owner
\   - DUPLICATE       : one id listed twice
\ Then the real docs/nanogpt-inventory.md + .dots must lint clean (exit 0).

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require tools/lint/text.f
require tools/nanogpt-inventory-lint-core.f

package NANOGPT-INV

$1000 constant TEST-OUT-CAP
create TEST-OUT TEST-OUT-CAP allot
create TEST-DOC-BUF FS-PATH-CAP allot   variable TEST-DOC-U
create TEST-DIR-BUF FS-PATH-CAP allot   variable TEST-DIR-U

: TEST-DIR$ ( -- ptr u8 n )   TEST-DIR-BUF TEST-DIR-U @ ;
: TEST-DOC$ ( -- ptr u8 n )   TEST-DOC-BUF TEST-DOC-U @ ;

: TEST-PREPARE ( -- )
   CLEANUP-RESET
   s" habu-nanogpt-inventory-lint" TMPDIR-MKDIR {: pa:ptr pu:n :}
   pa TEST-DIR-BUF pu BYTE-COPY  pu TEST-DIR-U !
   TEST-DIR$ CLEANUP-TREE+
   TEST-DIR$ s" inv.md" TEST-DOC-BUF JOIN-PATH TEST-DOC-U ! ;

\ A wrong doc: write CONTENT, lint it against the real .dots, assert it throws
\ and its output names the expected finding TAG.
: TEST-RED ( ptr u8 n ptr u8 n -- ) {: ca:ptr cu:n ta:ptr tu:n :}
   TEST-DOC$ ca cu WRITE-ALL
   TEST-OUT TEST-OUT-CAP LINT-OUT-BUFFER!
   [: TEST-DOC$ s" .dots/" NGI-LINT-AT ;] catch {: rc :}
   LINT-OUT$ {: oa:ptr ou:n :}
   LINT-OUT-BUFFER-OFF
   rc 1 T=
   oa ou ta tu LINT-CONTAINS? TTRUE ;

: FIX-LN ( ptr u8 n -- )   SB-APPEND 10 SB-APPEND-C ;   \ append a doc line + newline
: FIX-UNKNOWN$ ( -- ptr u8 n )
   SB-RESET
   s" # inv" FIX-LN
   s" owner `habu-nonexistent-deadbeef` cited." FIX-LN
   s" ```owners" FIX-LN
   s" open habu-nonexistent-deadbeef" FIX-LN
   s" ```" FIX-LN
   SB$ ;
: FIX-MISMATCH$ ( -- ptr u8 n )
   SB-RESET
   s" # inv" FIX-LN
   s" live owner `habu-cross-entropy-loss-93356943`." FIX-LN
   s" ```owners" FIX-LN
   s" open habu-cross-entropy-loss-93356943" FIX-LN
   s" ```" FIX-LN
   SB$ ;
: FIX-DUPLICATE$ ( -- ptr u8 n )
   SB-RESET
   s" # inv" FIX-LN
   s" owner `habu-gpt-2-composition-a90e901e` twice." FIX-LN
   s" ```owners" FIX-LN
   s" open habu-gpt-2-composition-a90e901e" FIX-LN
   s" open habu-gpt-2-composition-a90e901e" FIX-LN
   s" ```" FIX-LN
   SB$ ;

: TEST-UNKNOWN ( -- )     FIX-UNKNOWN$   s" NGI-UNKNOWN"         TEST-RED ;
: TEST-MISMATCH ( -- )    FIX-MISMATCH$  s" NGI-STATUS-MISMATCH" TEST-RED ;
: TEST-DUPLICATE ( -- )   FIX-DUPLICATE$ s" NGI-DUPLICATE"       TEST-RED ;

\ Green: the committed inventory lints clean against the live .dots tree.
: TEST-LIVE-GREEN ( -- )
   TEST-OUT TEST-OUT-CAP LINT-OUT-BUFFER!
   [: s" docs/nanogpt-inventory.md" s" .dots/" NGI-LINT-AT ;] catch {: rc :}
   LINT-OUT-BUFFER-OFF
   rc 0 T= ;

: TEST-MAIN ( -- )
   T-RESET
   TEST-PREPARE
   TEST-UNKNOWN
   TEST-MISMATCH
   TEST-DUPLICATE
   CLEANUP-RUN
   TEST-LIVE-GREEN
   T-REPORT ;

TEST-MAIN

;package
