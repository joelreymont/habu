\ refine-lint-test.f - checked fixtures for the TRUSTED mint confinement lint.
\ Red fixture: a scratch source file outside the tree containing an
\ out-of-owner-file mint call must fire with file:line; green: the real tree
\ scan reports 0 findings. Every mint name in this file lives inside a string
\ literal so the lint's own string-body exclusion keeps the tree scan green.
\ Run: bin/hb --load tools/refine-lint-test.f

require lib/date.f
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

package RFL-TEST

4096 constant OUT-CAP

variable ROOT-U
variable FILE-U
variable MAN-U

create ROOT-BUF FS-PATH-CAP allot
create FILE-BUF FS-PATH-CAP allot
create MAN-BUF FS-PATH-CAP allot
create OUT-BUF OUT-CAP allot

: ROOT$ ( -- ptr u8 n ) ROOT-BUF ROOT-U @ ;
: FILE$ ( -- ptr u8 n ) FILE-BUF FILE-U @ ;
: MAN$ ( -- ptr u8 n ) MAN-BUF MAN-U @ ;

: ROOT! ( ptr u8 n -- ) {: a:ptr u:n :}
   a ROOT-BUF u BYTE-COPY
   u ROOT-U ! ;

: DQ ( -- ) 34 SB-APPEND-C ;

: SETUP ( -- )
   RFL:BUFFERS
   RFL:RESET
   RFL:INVENTORY
   RFL:FINDINGS 0 T= ;                             \ seed/manifest cross-check is clean

: SHAPE ( -- )
   s" n -- CAD-KIND:rows" RFL:MINT-SHAPE? TTRUE
   s" n -- MIR:input-slot" RFL:MINT-SHAPE? TTRUE
   s" ptr n -- ptr CAD-KIND:dim" RFL:MINT-SHAPE? TTRUE
   s" CAD-KIND:rows -- n" RFL:MINT-SHAPE? TFALSE       \ projection direction stays per-site policy
   s" n -- tensor" RFL:MINT-SHAPE? TFALSE              \ bare-nominal mints are seed-only
   s" n n -- matrix<space-global,f32,m,q>" RFL:MINT-SHAPE? TFALSE
   s" ptr u8 n -- bool" RFL:MINT-SHAPE? TFALSE
   s" -- CAD-KIND:dim" RFL:MINT-SHAPE? TFALSE          \ no raw input to refine
   s" n -- rows:" RFL:MINT-SHAPE? TFALSE               \ edge colons are not family tokens
   s" n -- :rows" RFL:MINT-SHAPE? TFALSE ;

: DETECT ( -- )
   s" 7 ROWS-REFINE drop" RFL:COUNT-STR 1 T=
   s" 7 MAKI:ROWS-REFINE drop" RFL:COUNT-STR 1 T=      \ qualified reference is still a reference
   s" 7 rows-refine drop" RFL:COUNT-STR 1 T=           \ the dictionary is case-insensitive
   s" 1 RAW>NODE 2 RAW>SLOT" RFL:COUNT-STR 2 T=
   s" 1 RAW>ANODE 2 RAW>ASTREAM 3 RAW>AEVENT" RFL:COUNT-STR 3 T=
   s" 3 N>LBTK drop" RFL:COUNT-STR 1 T= ;              \ seed-only mint without a manifest row

: STR-CONTENT$ ( -- ptr u8 n )
   SB-RESET
   s" : X ( -- ) s" SB-APPEND DQ
   s"  has ROWS-REFINE within" SB-APPEND DQ
   s"  2drop ;" SB-APPEND
   SB$ ;

: ESC-CONTENT$ ( -- ptr u8 n )
   SB-RESET
   s" S\" SB-APPEND DQ
   s"  has ROWS-REFINE bytes\" SB-APPEND DQ
   s"  drop" SB-APPEND
   SB$ ;

: NO-FALSE-POSITIVE ( -- )
   s" \ prose mentioning ROWS-REFINE in a line comment" RFL:COUNT-STR 0 T=
   s" : F ( n -- n ) dup ; ( ROWS-REFINE in a paren comment )" RFL:COUNT-STR 0 T=
   STR-CONTENT$ RFL:COUNT-STR 0 T=                \ s" string bodies are excluded
   ESC-CONTENT$ RFL:COUNT-STR 0 T=                \ S\" escaped-string bodies too
   s" MY-ROWS-REFINE drop" RFL:COUNT-STR 0 T=          \ whole-token matching only
   s" ROWS-REFINED drop" RFL:COUNT-STR 0 T=
   s" :ROWS-REFINE drop" RFL:COUNT-STR 0 T= ;          \ edge colon is not a qualifier

: CONFINE-POLICY ( -- )
   \ owner file is allowed
   s" maki/tensor.f" s" 1 ROWS-REFINE drop" RFL:COUNT-STR-AT 0 T=
   \ a file cited by the mint's TRUSTED.md Tests cell is allowed
   s" maki/model-ir-test.f" s" 0 RAW>SLOT drop" RFL:COUNT-STR-AT 0 T=
   s" maki/async-dag.f" s" 0 RAW>ANODE drop" RFL:COUNT-STR-AT 0 T=
   s" maki/async-dag-test.f" s" 0 RAW>ASTREAM drop" RFL:COUNT-STR-AT 0 T=
   \ another mint's owner is not this mint's boundary
   s" maki/tensor.f" s" 0 RAW>SLOT drop" RFL:COUNT-STR-AT 1 T=
   s" maki/eval/eval.f" s" 0 RAW>AEVENT drop" RFL:COUNT-STR-AT 1 T=
   \ any other tree file is a finding
   s" maki/eval/eval.f" s" 0 RAW>SLOT drop" RFL:COUNT-STR-AT 1 T= ;

: ALLOWLIST ( -- )
   s" maki/eval/eval.f" s" 1 ROWS-REFINE drop" RFL:COUNT-STR-AT 1 T=
   s" ROWS-REFINE" s" maki/eval/eval.f" RFL:ALLOW+
   s" maki/eval/eval.f" s" 1 ROWS-REFINE drop" RFL:COUNT-STR-AT 0 T=
   \ the allow entry is mint-specific
   s" maki/eval/eval.f" s" 1 COLS-REFINE drop" RFL:COUNT-STR-AT 1 T=
   \ reset clears the allowlist
   RFL:RESET
   RFL:INVENTORY
   s" maki/eval/eval.f" s" 1 ROWS-REFINE drop" RFL:COUNT-STR-AT 1 T= ;

: RED-PREPARE ( -- )
   CLEANUP-RESET
   s" habu-refine-lint" TMPDIR-MKDIR ROOT!
   ROOT$ CLEANUP-DIR+
   ROOT$ s" mint.f" FILE-BUF JOIN-PATH FILE-U !
   FILE$ CLEANUP+
   FILE$ s" : FORGE ( n -- n ) ROWS-REFINE ROWS-RAW ;" WRITE-ALL
   ROOT$ s" TRUSTED.md" MAN-BUF JOIN-PATH MAN-U !
   MAN$ CLEANUP+ ;

: RED-SCAN ( -- ptr u8 n )                 \ captured finding output; RFL:FINDINGS holds count
   RFL:CLEAR-FINDINGS
   OUT-BUF OUT-CAP LINT-OUT-BUFFER!
   FILE$ RFL:SCAN-FILE
   LINT-OUT$
   LINT-OUT-BUFFER-OFF ;

: RED ( -- )
   RED-SCAN {: oa:ptr ou:n :}
   RFL:FINDINGS 1 T=                               \ the mint fires; the projection does not
   oa ou s" REFINE-CONFINE " LINT-CONTAINS? TTRUE
   oa ou s" mint.f:1: " LINT-CONTAINS? TTRUE
   oa ou s" ROWS-REFINE" LINT-CONTAINS? TTRUE
   oa ou s" ` referenced outside owner maki/tensor.f" LINT-CONTAINS? TTRUE
   RFL:CLEAR-FINDINGS ;

\ ---- synthetic-manifest coverage for the anti-rot ratchet -------------------

: LF ( -- ) 10 SB-APPEND-C ;

: MAN-HEADER ( -- )
   s" | Word | Effect | Reason | Tests | Site | Last audited |" SB-APPEND LF
   s" |------|--------|--------|-------|------|--------------|" SB-APPEND LF ;

: SHAPE-MAN$ ( -- ptr u8 n )
   SB-RESET MAN-HEADER
   s" | FAKE-MINT | `n -- CAD-KIND:fake` | test | `maki/fake-test.f` | maki/fake.f | 2026-07-13 |" SB-APPEND LF
   SB$ ;

: MAN-LOAD ( ptr u8 n -- ) {: a:ptr u:n :}
   MAN$ a u WRITE-ALL
   ROOT$ TRUST-LINT-ROOT!
   TRUST-LINT-RESET
   TL-SCAN-MANIFEST ;

: NEW-MINT-RED ( -- )
   SHAPE-MAN$ MAN-LOAD
   RFL:REPORT-OFF
   RFL:CLEAR-FINDINGS
   RFL:SHAPE-SCAN
   RFL:FINDINGS 1 T=                               \ an unseeded mint-shaped row is a finding
   RFL:CLEAR-FINDINGS
   RFL:REPORT-ON ;

\ ---- source-derived anti-rot ratchet ----------------------------------------
\ Seed 1 is ROWS-REFINE (owner maki/tensor.f). STALE-SEED liveness now comes from
\ the owner-source declaration, not a manifest row; confinement is name-and-path
\ based, so it is identical for both declarer forms.

: FORM-CONFINED ( ptr u8 n -- ) {: ca:ptr cu:n :}
   \ this declarer form keeps the seed live (no STALE-SEED) ...
   RFL:CLEAR-FINDINGS
   ca cu 1 RFL:STALE-IF-DEAD
   RFL:FINDINGS 0 T=
   \ ... and its raw->nominal forge stays module-private: an out-of-owner
   \ reference is a finding (red), owner + <owner-stem>-test.f are the greens.
   s" maki/eval/eval.f"   s" 1 ROWS-REFINE drop" RFL:COUNT-STR-AT 1 T=
   s" maki/tensor.f"      s" 1 ROWS-REFINE drop" RFL:COUNT-STR-AT 0 T=
   s" maki/tensor-test.f" s" 1 ROWS-REFINE drop" RFL:COUNT-STR-AT 0 T= ;

: DRIFT-RED ( -- )
   RFL:REPORT-OFF
   \ STALE-SEED, source-derived (red-first): a seed whose owner source no longer
   \ declares it (plain `:` colon def, no CAST:/TRUSTED:) trips the ratchet.
   RFL:CLEAR-FINDINGS
   s" : ROWS-REFINE ( n -- CAD-KIND:rows ) ROWS-RAW ;" 1 RFL:STALE-IF-DEAD
   RFL:FINDINGS 1 T=
   \ both declarer forms keep it live and confined (REFINE-CONFINE red-first each):
   s" TRUSTED: ROWS-REFINE ( n -- CAD-KIND:rows ) ;" FORM-CONFINED
   s" CAST: ROWS-REFINE ( n -- CAD-KIND:rows ) ;"    FORM-CONFINED
   \ the manifest Tests cell is no longer consulted: MINT-PATH's old row cited
   \ lib/nominal/nominal-test.f, which is not <owner-stem>-test.f, so absent an
   \ RFL:ALLOW+ entry it is now a finding - the semantics genuinely changed.
   s" lib/nominal/nominal-test.f" s" 1 MINT-PATH drop" RFL:COUNT-STR-AT 1 T=
   RFL:CLEAR-FINDINGS
   RFL:REPORT-ON ;

: RESTORE ( -- )
   RFL:RESET
   RFL:INVENTORY                                \ back to the real TRUSTED.md
   RFL:FINDINGS 0 T=
   CLEANUP-RUN ;

: LIVE ( -- )
   \ the real tree (maki/ lib/ src/ tools/) must be confined -> returns clean
   RFL:RUN ;

public

: MAIN ( -- )
   T-RESET
   SETUP
   SHAPE
   DETECT
   NO-FALSE-POSITIVE
   CONFINE-POLICY
   ALLOWLIST
   RED-PREPARE
   RED
   NEW-MINT-RED
   DRIFT-RED
   RESTORE
   LIVE
   T-REPORT ;

;package

RFL-TEST:MAIN
