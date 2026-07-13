\ refine-lint-test.f - checked fixtures for the TRUSTED mint confinement lint.
\ Red fixture: a scratch source file outside the tree containing an
\ out-of-owner-file mint call must fire with file:line; green: the real tree
\ scan reports 0 findings. Every mint name in this file lives inside a string
\ literal so the lint's own string-body exclusion keeps the tree scan green.
\ Run: bin/hb --load tools/refine-lint-test.f

require tools/date.f
require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require tools/lint/text.f
require tools/lint/token.f
require tools/lint/lib.f
require tools/trust-lint-core.f
require tools/refine-lint-core.f

4096 constant RFLT-OUT-CAP

variable RFLT-ROOT-U
variable RFLT-FILE-U
variable RFLT-MAN-U

create RFLT-ROOT-BUF FS-PATH-CAP allot
create RFLT-FILE-BUF FS-PATH-CAP allot
create RFLT-MAN-BUF FS-PATH-CAP allot
create RFLT-OUT-BUF RFLT-OUT-CAP allot

: RFLT-ROOT$ ( -- ptr u8 n ) RFLT-ROOT-BUF RFLT-ROOT-U @ ;
: RFLT-FILE$ ( -- ptr u8 n ) RFLT-FILE-BUF RFLT-FILE-U @ ;
: RFLT-MAN$ ( -- ptr u8 n ) RFLT-MAN-BUF RFLT-MAN-U @ ;

: RFLT-ROOT! ( ptr u8 n -- ) {: a:ptr u:n :}
   a RFLT-ROOT-BUF u BYTE-COPY
   u RFLT-ROOT-U ! ;

: RFLT-DQ ( -- ) 34 SB-APPEND-C ;

: RFLT-SETUP ( -- )
   RFL-BUFFERS
   RFL-RESET
   RFL-INVENTORY
   RFL-BAD @ 0 T= ;                             \ seed/manifest cross-check is clean

: RFLT-SHAPE ( -- )
   s" n -- CAD-KIND:rows" RFL-MINT-SHAPE? TTRUE
   s" n -- MIR:input-slot" RFL-MINT-SHAPE? TTRUE
   s" ptr n -- ptr CAD-KIND:dim" RFL-MINT-SHAPE? TTRUE
   s" CAD-KIND:rows -- n" RFL-MINT-SHAPE? TFALSE       \ projection direction stays per-site policy
   s" n -- tensor" RFL-MINT-SHAPE? TFALSE              \ bare-nominal mints are seed-only
   s" n n -- matrix<space-global,f32,m,q>" RFL-MINT-SHAPE? TFALSE
   s" ptr u8 n -- bool" RFL-MINT-SHAPE? TFALSE
   s" -- CAD-KIND:dim" RFL-MINT-SHAPE? TFALSE          \ no raw input to refine
   s" n -- rows:" RFL-MINT-SHAPE? TFALSE               \ edge colons are not family tokens
   s" n -- :rows" RFL-MINT-SHAPE? TFALSE ;

: RFLT-DETECT ( -- )
   s" 7 ROWS-REFINE drop" RFL-COUNT-STR 1 T=
   s" 7 MAKI:ROWS-REFINE drop" RFL-COUNT-STR 1 T=      \ qualified reference is still a reference
   s" 7 rows-refine drop" RFL-COUNT-STR 1 T=           \ the dictionary is case-insensitive
   s" 1 RAW>NODE 2 RAW>SLOT" RFL-COUNT-STR 2 T=
   s" 3 N>LBTK drop" RFL-COUNT-STR 1 T= ;              \ seed-only mint without a manifest row

: RFLT-STR-CONTENT$ ( -- ptr u8 n )
   SB-RESET
   s" : RFLT-X ( -- ) s" SB-APPEND RFLT-DQ
   s"  has ROWS-REFINE within" SB-APPEND RFLT-DQ
   s"  2drop ;" SB-APPEND
   SB$ ;

: RFLT-ESC-CONTENT$ ( -- ptr u8 n )
   SB-RESET
   s" S\" SB-APPEND RFLT-DQ
   s"  has ROWS-REFINE bytes\" SB-APPEND RFLT-DQ
   s"  drop" SB-APPEND
   SB$ ;

: RFLT-NO-FALSE-POSITIVE ( -- )
   s" \ prose mentioning ROWS-REFINE in a line comment" RFL-COUNT-STR 0 T=
   s" : F ( n -- n ) dup ; ( ROWS-REFINE in a paren comment )" RFL-COUNT-STR 0 T=
   RFLT-STR-CONTENT$ RFL-COUNT-STR 0 T=                \ s" string bodies are excluded
   RFLT-ESC-CONTENT$ RFL-COUNT-STR 0 T=                \ S\" escaped-string bodies too
   s" MY-ROWS-REFINE drop" RFL-COUNT-STR 0 T=          \ whole-token matching only
   s" ROWS-REFINED drop" RFL-COUNT-STR 0 T=
   s" :ROWS-REFINE drop" RFL-COUNT-STR 0 T= ;          \ edge colon is not a qualifier

: RFLT-CONFINE-POLICY ( -- )
   \ owner file is allowed
   s" maki/tensor.f" s" 1 ROWS-REFINE drop" RFL-COUNT-STR-AT 0 T=
   \ a file cited by the mint's TRUSTED.md Tests cell is allowed
   s" maki/model-ir-test.f" s" 0 RAW>SLOT drop" RFL-COUNT-STR-AT 0 T=
   \ another mint's owner is not this mint's boundary
   s" maki/tensor.f" s" 0 RAW>SLOT drop" RFL-COUNT-STR-AT 1 T=
   \ any other tree file is a finding
   s" maki/eval.f" s" 0 RAW>SLOT drop" RFL-COUNT-STR-AT 1 T= ;

: RFLT-ALLOWLIST ( -- )
   s" maki/eval.f" s" 1 ROWS-REFINE drop" RFL-COUNT-STR-AT 1 T=
   s" ROWS-REFINE" s" maki/eval.f" RFL-ALLOW+
   s" maki/eval.f" s" 1 ROWS-REFINE drop" RFL-COUNT-STR-AT 0 T=
   \ the allow entry is mint-specific
   s" maki/eval.f" s" 1 COLS-REFINE drop" RFL-COUNT-STR-AT 1 T=
   \ reset clears the allowlist
   RFL-RESET
   RFL-INVENTORY
   s" maki/eval.f" s" 1 ROWS-REFINE drop" RFL-COUNT-STR-AT 1 T= ;

: RFLT-RED-PREPARE ( -- )
   CLEANUP-RESET
   s" habu-refine-lint" TMPDIR-MKDIR RFLT-ROOT!
   RFLT-ROOT$ CLEANUP-DIR+
   RFLT-ROOT$ s" mint.f" RFLT-FILE-BUF JOIN-PATH RFLT-FILE-U !
   RFLT-FILE$ CLEANUP+
   RFLT-FILE$ s" : RFLT-FORGE ( n -- n ) ROWS-REFINE ROWS-RAW ;" WRITE-ALL
   RFLT-ROOT$ s" TRUSTED.md" RFLT-MAN-BUF JOIN-PATH RFLT-MAN-U !
   RFLT-MAN$ CLEANUP+ ;

: RFLT-RED-SCAN ( -- ptr u8 n )                 \ captured finding output; RFL-BAD holds count
   0 RFL-BAD !
   RFLT-OUT-BUF RFLT-OUT-CAP LINT-OUT-BUFFER!
   RFLT-FILE$ RFL-SCAN-FILE
   LINT-OUT$
   LINT-OUT-BUFFER-OFF ;

: RFLT-RED ( -- )
   RFLT-RED-SCAN {: oa:ptr ou:n :}
   RFL-BAD @ 1 T=                               \ the mint fires; the projection does not
   oa ou s" REFINE-CONFINE " LINT-CONTAINS? TTRUE
   oa ou s" mint.f:1: " LINT-CONTAINS? TTRUE
   oa ou s" ROWS-REFINE" LINT-CONTAINS? TTRUE
   oa ou s" ` referenced outside owner maki/tensor.f" LINT-CONTAINS? TTRUE
   0 RFL-BAD ! ;

\ ---- synthetic-manifest coverage for the anti-rot ratchet -------------------

: RFLT-LF ( -- ) 10 SB-APPEND-C ;

: RFLT-MAN-HEADER ( -- )
   s" | Word | Effect | Reason | Tests | Site | Last audited |" SB-APPEND RFLT-LF
   s" |------|--------|--------|-------|------|--------------|" SB-APPEND RFLT-LF ;

: RFLT-SHAPE-MAN$ ( -- ptr u8 n )
   SB-RESET RFLT-MAN-HEADER
   s" | RFLT-FAKE-MINT | `n -- CAD-KIND:fake` | test | `maki/fake-test.f` | maki/fake.f | 2026-07-13 |" SB-APPEND RFLT-LF
   SB$ ;

: RFLT-DRIFT-MAN$ ( -- ptr u8 n )
   SB-RESET RFLT-MAN-HEADER
   s" | ROWS-REFINE | `n -- CAD-KIND:rows` | test | `maki/tensor-test.f` | maki/moved.f | 2026-07-13 |" SB-APPEND RFLT-LF
   SB$ ;

: RFLT-MAN-LOAD ( ptr u8 n -- ) {: a:ptr u:n :}
   RFLT-MAN$ a u WRITE-ALL
   RFLT-ROOT$ TRUST-LINT-ROOT!
   TRUST-LINT-RESET
   TL-SCAN-MANIFEST ;

: RFLT-NEW-MINT-RED ( -- )
   RFLT-SHAPE-MAN$ RFLT-MAN-LOAD
   RFL-REPORT-OFF
   0 RFL-BAD !
   RFL-SHAPE-SCAN
   RFL-BAD @ 1 T=                               \ an unseeded mint-shaped row is a finding
   0 RFL-BAD !
   RFL-REPORT-ON ;

: RFLT-DRIFT-RED ( -- )
   RFLT-DRIFT-MAN$ RFLT-MAN-LOAD
   RFL-REPORT-OFF
   0 RFL-BAD !
   RFL-SELECT
   \ 1 SEED-DRIFT (row site moved off the seed owner) + 14 STALE-SEED (every
   \ in-root seed, drifted ROWS-REFINE included, lacks a matching-site row in
   \ the synthetic manifest; the test/-owned seed is exempt)
   RFL-BAD @ RFL-SEED# T=
   0 RFL-BAD !
   RFL-REPORT-ON ;

: RFLT-RESTORE ( -- )
   RFL-RESET
   RFL-INVENTORY                                \ back to the real TRUSTED.md
   RFL-BAD @ 0 T=
   CLEANUP-RUN ;

: RFLT-LIVE ( -- )
   \ the real tree (maki/ lib/ src/ tools/) must be confined -> returns clean
   REFINE-LINT ;

: RFLT-MAIN ( -- )
   T-RESET
   RFLT-SETUP
   RFLT-SHAPE
   RFLT-DETECT
   RFLT-NO-FALSE-POSITIVE
   RFLT-CONFINE-POLICY
   RFLT-ALLOWLIST
   RFLT-RED-PREPARE
   RFLT-RED
   RFLT-NEW-MINT-RED
   RFLT-DRIFT-RED
   RFLT-RESTORE
   RFLT-LIVE
   T-REPORT ;

RFLT-MAIN
