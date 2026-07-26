\ refine-lint-core.f - confine raw->nominal refinement mints to their owning files.
\
\ Refinement mints (declared `TRUSTED: NAME ( n -- <nominal family> )` or, once
\ migrated, `CAST: NAME ( ... )`, e.g. ROWS-REFINE `n -- CAD-KIND:rows`) forge
\ nominal values from raw cells. They are package-private by convention, but any
\ maki file can reopen the package and call one bare, minting an unvalidated
\ nominal with no gate failing. This lint pins the convention: every reference to
\ a mint outside its owning file is a finding unless the referencing file is the
\ mint owner's module test (`<owner-stem>-test.f`) or named on the explicit
\ allowlist (ALLOW+). The confinement is name-and-path based, so it is
\ identical for both declarer forms - migrating a mint from TRUSTED: to CAST:
\ neither weakens nor relaxes it.
\
\ The mint set is a seed list of (name, owner) pairs. Its liveness is
\ source-derived: each seed must be declared in its owner file via CAST: or
\ TRUSTED:, else STALE-SEED fires (the declaration was retired or moved - retire
\ or update the seed). A signature-shape scan over TRUSTED.md flags NEW
\ mint-shaped manifest rows (a raw `n` input producing a colon-qualified
\ nominal-family output) that are missing from the seed list (NEW-MINT), so the
\ seed list cannot silently rot as TRUSTED: mints are added. Bare-nominal mints
\ such as RAW>TENSOR (`n -- tensor`) and the importer projections are seed-only:
\ the shape scan covers the qualified CAD-KIND:/MIR: family namespace.
\
\ INTERIM enforcement only: the principled endpoint is the TVK-RAW checker
\ capability (dot habu-nominal-storage-raw-a3430ef2), which closes the mint
\ direction at unification. Retire this lint's mint class when that lands.
\
\ Scan discipline: whole-token matching over the shared PAT-* scanner, so `\`
\ and `( )` comments and string-literal bodies are excluded; matching is
\ case-insensitive (the dictionary is case-insensitive) and also catches
\ qualified `PKG:NAME` references. Scanned roots: maki/ lib/ src/ tools/.
\ Owner liveness reads the owner source; NEW-MINT rows are read through
\ tools/trust-lint-core.f (TL-M-*).
\
\ Load after lib/date.f, lib/errors.f, lib/string.f, lib/memory.f, lib/fs.f,
\ tools/lint/text.f, tools/lint/token.f, tools/lint/lib.f, and
\ tools/trust-lint-core.f.

package RFL

private

$40000 constant STR-CAP     \ trust-lint manifest string store
$80000 constant FILE-CAP    \ largest scanned source watermark (checker.f class)
62 constant SEED#
8 constant ALLOW-MAX
32 constant NUM-CAP

10 constant LF
48 constant ZERO
58 constant COLON

create NUM-BUF NUM-CAP allot
create ONE 1 allot
create STEM-BUF FS-PATH-CAP allot
create ALW-NO ALLOW-MAX cells allot
create ALW-NU ALLOW-MAX cells allot
create ALW-PO ALLOW-MAX cells allot
create ALW-PU ALLOW-MAX cells allot

variable STR-A
variable FILE-A
variable BAD
variable ALLOW#
variable REPORT?
variable NUM-L
variable LINE
variable CUR-A
variable CUR-U
variable LA
variable LU
variable LX
variable LS
variable LE

: STR-A-FIELD ( -- ptr ptr u8 ) STR-A 0 ptr-field ;
: FILE-A-FIELD ( -- ptr ptr u8 ) FILE-A 0 ptr-field ;
: CUR-A-FIELD ( -- ptr ptr u8 ) CUR-A 0 ptr-field ;
: LA-FIELD ( -- ptr ptr u8 ) LA 0 ptr-field ;
: ALW-NO-FIELD ( n -- ptr ptr u8 ) cells ALW-NO + 0 ptr-field ;
: ALW-PO-FIELD ( n -- ptr ptr u8 ) cells ALW-PO + 0 ptr-field ;

: CUR$ ( -- ptr u8 n ) CUR-A-FIELD @ CUR-U @ ;
: CUR! ( ptr u8 n -- ) CUR-U ! CUR-A-FIELD ! ;
: LA@ ( -- ptr u8 ) LA-FIELD @ ;
: LA! ( ptr u8 -- ) LA-FIELD ! ;

: FAIL ( ptr u8 n -- ) 76 die ;

: STR-BUF ( -- ptr u8 )
   STR-A @ 0= if
      STR-CAP MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop STR-A-FIELD !
   then
   STR-A-FIELD @ ;

: FILE-BUF ( -- ptr u8 )
   FILE-A @ 0= if
      FILE-CAP MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop FILE-A-FIELD !
   then
   FILE-A-FIELD @ ;

public

: BUFFERS ( -- )
   STR-BUF STR-CAP
   FILE-BUF FILE-CAP
   TRUST-LINT-BUFFERS! ;
private


\ ---- output ----------------------------------------------------------------

: OUT ( ptr u8 n -- ) {: a:ptr u:n :}
   1 a u LINT-OUT-WRITE ;

: C ( n -- )
   ONE c! ONE 1 OUT ;

: NL ( -- ) LF C ;

: U. ( n -- )
   0 NUM-L !
   dup 0= IF drop ZERO C exit THEN
   begin dup 0 > while
      dup 10 mod ZERO + NUM-BUF NUM-L @ + c!
      10 /
      NUM-L @ 1+ NUM-L !
   repeat drop
   begin NUM-L @ 0 > while
      NUM-L @ 1- NUM-L !
      NUM-BUF NUM-L @ + c@ C
   repeat ;

: BAD+ ( -- ) BAD @ 1+ BAD ! ;

public

: REPORT-ON ( -- ) LINT-TRUE REPORT? ! ;
: REPORT-OFF ( -- ) LINT-FALSE REPORT? ! ;
private


\ ---- mint seed table: (name, owning file) -----------------------------------
\ The confinement set. Owners outside the scanned roots (test/) are seed-only
\ boundaries whose confinement still applies inside the scanned roots.

: SEED-NAME$ ( n -- ptr u8 n )
   case
      0 of s" DIM-REFINE" endof
      1 of s" ROWS-REFINE" endof
      2 of s" COLS-REFINE" endof
      3 of s" SPACE-REFINE" endof
      4 of s" RAW>NODE" endof
      5 of s" RAW>SLOT" endof
      6 of s" RAW>REF" endof
      7 of s" RAW>INPUT-INDEX" endof
      8 of s" RAW>REF-POS" endof
      9 of s" RAW>RGN" endof
      10 of s" RAW>TARGET-ID" endof
      11 of s" RAW>TENSOR" endof
      12 of s" IMP-ROWS-N" endof
      13 of s" IMP-COLS-N" endof
      14 of s" N>LBTK" endof
      15 of s" RAW>ANODE" endof
      16 of s" RAW>ASTREAM" endof
      17 of s" RAW>AEVENT" endof
      18 of s" RAW>DECL" endof
      19 of s" RAW>ELAB" endof
      20 of s" RAW>SOLVED" endof
      21 of s" RAW>LEGAL" endof
      22 of s" RAW>DRAFT" endof
      23 of s" RAW>COMPLETE" endof
      24 of s" RAW>DRAFTED" endof
      25 of s" RAW>VERIFIED" endof
      26 of s" RAW>EMITTED" endof
      27 of s" MINT-BUILD-PROOF" endof
      28 of s" MINT-CERTIFY-PROOF" endof
      29 of s" MINT-GOLDEN-PROOF" endof
      30 of s" MINT-GRADCHECK-PROOF" endof
      31 of s" MINT-PROFILE-PROOF" endof
      32 of s" MINT-GRANT-PROOF" endof
      33 of s" RAW>ARTIFACT-ID" endof
      34 of s" RAW>SCHEMA-ID" endof
      35 of s" RAW>PROMOTED" endof
      \ CAD-NUM B5.1 scalar role mints (dot habu-implement-cad-num-962bf5d9;
      \ retire with TVK-RAW habu-nominal-storage-raw-a3430ef2). Bare-nominal
      \ output (n -- byte-len), so seed-only, not caught by the shape scan.
      36 of s" MINT-BYTE-LEN" endof
      37 of s" MINT-ITEM-COUNT" endof
      38 of s" MINT-CELL-COUNT" endof
      39 of s" MINT-INDEX" endof
      40 of s" MINT-BYTE-OFF" endof
      41 of s" MINT-CELL-OFF" endof
      42 of s" MINT-ALIGNMENT" endof
      43 of s" MINT-POSITIVE-DIVISOR" endof
      44 of s" MINT-ALLOC-BYTE-LEN" endof
      45 of s" MINT-ALLOC-CELL-COUNT" endof
      46 of s" MINT-PATH" endof
      47 of s" MINT-BINDING" endof
      48 of s" MINT-ROW" endof
      \ § 23.9 foreign-id refinement (dot habu-npol-numeric-policy-a90657e1); the
      \ RAW>TARGET-ID / RAW>ARTIFACT-ID shape-scanned mint precedent.
      49 of s" RAW>NUMERIC-POLICY-ID" endof
      \ § 23.9 foreign-id per-family legs; the RAW>TARGET-ID / RAW>ARTIFACT-ID shape.
      50 of s" RAW>PRODUCER-ID" endof     \ dot habu-producer-producer-id-5e016e1f
      51 of s" RAW>CONFIG-ID" endof       \ dot habu-config-config-id-06aa21bd
      \ § 23.9 foreign-id per-family legs owned by the txn/journal dot
      \ habu-v2-txn-journal-d0bc644f; the RAW>TARGET-ID / RAW>ARTIFACT-ID shape.
      52 of s" RAW>AUDIT-EVENT-ID" endof  \ append-only journal sequence (occurrence id)
      53 of s" RAW>REV-ID" endof          \ content-addressed revision id
      \ § 23.9 foreign-id per-family legs owned by the evidence-applicability dot
      \ habu-v2-evidence-applicability-73ac58b9; the RAW>PRODUCER-ID shape.
      54 of s" RAW>OBLIGATION-ID" endof   \ content-addressed by the canonical obligation encoding
      55 of s" RAW>EVIDENCE-ID" endof     \ content-addressed evidence descriptor
      \ § 23.9 machine-facing action registry (dot habu-v2-machine-action-a7357409); the
      \ RAW>OBLIGATION-ID shape. Seeded here to close the NEW-MINT gap its TRUSTED.md row left.
      56 of s" RAW>ACTION-ID" endof       \ content-addressed by the canonical action name
      \ § 23 capability + budget enforcement (dot habu-v2-capability-and-0970a96d). A package-local
      \ CAPTOK:grant nominal (not CAD-KIND), so seed-only - the shape scan covers CAD-KIND:/MIR: only.
      57 of s" RAW>GRANT" endof           \ append-only capability authority-slot refinement
      \ § 23.4 experiment registry (dot habu-v2-experiment-run-7c1d1906); the
      \ RAW>ARTIFACT-ID shape - content-addressed by the canonical run-key digest.
      58 of s" RAW>RUN-ID" endof          \ content-addressed by the interned run-key digest
      \ § "Automatic differential verification" durable suite-id registry (dot
      \ habu-v2-differential-runner-13359019); the RAW>EVIDENCE-ID shape - content-addressed
      \ by the DifferentialSuite digest.
      59 of s" RAW>SUITE-ID" endof         \ content-addressed by the interned suite digest
      \ The GPT-2 bind transaction's prepared value (leaf S6b1) parks its two
      \ linear children - the moved census and the sealed slot table - in the prep
      \ block as raw cells, because a DEFLINEAR carries no fields and an ENUM
      \ payload field cannot name a record transitively holding a linear field.
      \ These two read them back out, each reachable only from GPT2TX:ABORT.
      \ Both retire with the linear-scope combinator capability,
      \ habu-checker-linear-scope-6218899c.
      60 of s" N>CENSUS" endof             \ the sole inverse of CENSUS>N, reached only by ABORT
      61 of s" N>TABLE" endof              \ the sole inverse of TABLE>N, reached only by ABORT
      E-TBL-BOUNDS throw
   endcase ;

: SEED-OWNER$ ( n -- ptr u8 n )
   case
      0 of s" maki/tensor.f" endof
      1 of s" maki/tensor.f" endof
      2 of s" maki/tensor.f" endof
      3 of s" maki/tensor.f" endof
      4 of s" maki/model-ir.f" endof
      5 of s" maki/model-ir.f" endof
      6 of s" maki/model-ir.f" endof
      7 of s" maki/model-ir.f" endof
      8 of s" maki/model-ir.f" endof
      9 of s" maki/fusion-plan.f" endof
      10 of s" maki/target/target.f" endof
      11 of s" maki/tensor-value.f" endof
      12 of s" maki/onnx/import.f" endof
      13 of s" maki/onnx/import.f" endof
      14 of s" test/layout-buffer.f" endof
      15 of s" maki/async-dag.f" endof
      16 of s" maki/async-dag.f" endof
      17 of s" maki/async-dag.f" endof
      18 of s" maki/typestate.f" endof
      19 of s" maki/typestate.f" endof
      20 of s" maki/typestate.f" endof
      21 of s" maki/typestate.f" endof
      22 of s" maki/typestate.f" endof
      23 of s" maki/typestate.f" endof
      24 of s" maki/typestate.f" endof
      25 of s" maki/typestate.f" endof
      26 of s" maki/typestate.f" endof
      27 of s" maki/typestate.f" endof
      28 of s" maki/evidence/schema.f" endof
      29 of s" maki/evidence/schema.f" endof
      30 of s" maki/evidence/schema.f" endof
      31 of s" maki/evidence/schema.f" endof
      32 of s" maki/evidence/policy.f" endof
      33 of s" maki/artifact.f" endof
      34 of s" maki/schema.f" endof
      35 of s" maki/evidence/promote.f" endof
      36 of s" lib/cad-num-types.f" endof
      37 of s" lib/cad-num-types.f" endof
      38 of s" lib/cad-num-types.f" endof
      39 of s" lib/cad-num-types.f" endof
      40 of s" lib/cad-num-types.f" endof
      41 of s" lib/cad-num-types.f" endof
      42 of s" lib/cad-num-types.f" endof
      43 of s" lib/cad-num-types.f" endof
      44 of s" lib/cad-num-types.f" endof
      45 of s" lib/cad-num-types.f" endof
      46 of s" lib/nominal/path.f" endof
      47 of s" lib/nominal/binding.f" endof
      48 of s" lib/nominal/row.f" endof
      49 of s" maki/numpolicy.f" endof
      50 of s" maki/producer.f" endof
      51 of s" maki/config.f" endof
      52 of s" maki/journal.f" endof
      53 of s" maki/rev.f" endof
      54 of s" maki/db/obligation.f" endof
      55 of s" maki/db/evidence.f" endof
      56 of s" maki/db/action.f" endof
      57 of s" maki/db/capability.f" endof
      58 of s" maki/experiment/run.f" endof
      59 of s" maki/db/diff-suite-id.f" endof
      60 of s" maki/infer/gpt2-bind.f" endof
      61 of s" maki/infer/gpt2-bind.f" endof
      E-TBL-BOUNDS throw
   endcase ;

: SEEDED? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   0 begin dup SEED# < while
      dup SEED-NAME$ a u LINT-STR=CI if drop LINT-TRUE exit then
      1+
   repeat drop LINT-FALSE ;

\ ---- allowlist: documented (mint, caller file) exceptions --------------------
\ Empty today. Entries must cite the review that documented the exception.
\ Caller-supplied strings must stay live for the run (s" literals are).

: ALLOW-NAME$ ( n -- ptr u8 n )
   dup ALW-NO-FIELD @ swap cells ALW-NU + @ ;

: ALLOW-PATH$ ( n -- ptr u8 n )
   dup ALW-PO-FIELD @ swap cells ALW-PU + @ ;

public

: ALLOW+ ( ptr u8 n ptr u8 n -- ) {: na:ptr nu:n pa:ptr pu:n :}
   ALLOW# @ ALLOW-MAX >= if s" refine-lint: allowlist full" FAIL then
   na ALLOW# @ ALW-NO-FIELD !
   nu ALW-NU ALLOW# @ cells + !
   pa ALLOW# @ ALW-PO-FIELD !
   pu ALW-PU ALLOW# @ cells + !
   ALLOW# @ 1+ ALLOW# ! ;

private


: ALLOW-LISTED? ( n -- bool ) {: k:n :}
   0 begin dup ALLOW# @ < while
      dup ALLOW-NAME$ k SEED-NAME$ LINT-STR=CI if
         dup ALLOW-PATH$ CUR$ FS-PATH= if drop LINT-TRUE exit then
      then
      1+
   repeat drop LINT-FALSE ;

\ ---- inventory: manifest cross-check + mint-shape scan ----------------------

: FAMILY-TOK? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u COLON LINT-INDEX-OF MATCH option
     none OF LINT-FALSE ENDOF
     some OF {: i:n :} i 0 >  i u 1- <  and ENDOF
   ;MATCH ;

: EFF-DASH-IDX ( -- n )
   0 begin dup SN# @ < while
      dup S@ s" --" LINT-STR= if exit then
      1+
   repeat drop -1 ;

: EFF-RAW-IN? ( n -- bool ) {: dash:n :}
   0 begin dup dash < while
      dup S@ s" n" LINT-STR= if drop LINT-TRUE exit then
      1+
   repeat drop LINT-FALSE ;

: EFF-FAMILY-OUT? ( n -- bool ) {: dash:n :}
   dash 1+ begin dup SN# @ < while
      dup S@ FAMILY-TOK? if drop LINT-TRUE exit then
      1+
   repeat drop LINT-FALSE ;

public

: MINT-SHAPE? ( ptr u8 n -- bool )
   SPLIT-WHITESPACE
   EFF-DASH-IDX dup 0 < if drop LINT-FALSE exit then
   {: dash:n :}
   dash EFF-RAW-IN? 0= if LINT-FALSE exit then
   dash EFF-FAMILY-OUT? ;

private

: STALE-SEED ( n -- ) {: k:n :}
   REPORT? @ if
      s" STALE-SEED refine-lint: `" OUT k SEED-NAME$ OUT
      s" ` is not declared (CAST:/TRUSTED:) in owner " OUT k SEED-OWNER$ OUT
      s" ; retire or update the seed list" OUT NL
   then
   BAD+ ;

: NEW-MINT ( n -- ) {: m:n :}
   REPORT? @ if
      s" NEW-MINT TRUSTED.md " OUT m TL-M-KEY-PATH$ OUT
      s" : `" OUT m TL-M-NAME$ OUT
      s" ` `" OUT m TL-M-EFF$ OUT
      s" ` is mint-shaped but not in the refine-lint seed list" OUT NL
   then
   BAD+ ;

\ ---- source-derived liveness: the owner file must still declare the mint ------
\ STALE-SEED is decided by the OWNER SOURCE declaration (CAST: or TRUSTED:), not
\ a manifest row, so a mint migrated from TRUSTED: to CAST: stays live while a
\ retired or moved declaration trips the ratchet.

: DECLARER-TOK? ( -- bool )                 \ current PAT token is a mint declarer
   s" CAST:" PAT-TOK= if LINT-TRUE exit then
   s" TRUSTED:" PAT-TOK= ;

: DECLARES? ( ptr u8 n ptr u8 n -- bool ) {: ca:ptr cu:n na:ptr nu:n :}
   ca cu PAT-RESET
   begin PAT-READ-TOKEN while
      DECLARER-TOK? if
         PAT-CAP-TOKEN-1 if
            P1A@ P1U @ na nu LINT-STR=CI if LINT-TRUE exit then
         then
      else
         PAT-TOK-STRING? if PAT-SKIP-STRING-BODY then
      then
   repeat LINT-FALSE ;

: CONTENT-LIVE? ( ptr u8 n n -- bool ) {: ca:ptr cu:n k:n :}
   ca cu k SEED-NAME$ DECLARES? ;

public

: STALE-IF-DEAD ( ptr u8 n n -- ) {: ca:ptr cu:n k:n :}
   ca cu k CONTENT-LIVE? 0= if k STALE-SEED then ;

private

: CHECK-LIVE ( n -- ) {: k:n :}
   k SEED-OWNER$ EXISTS? 0= if k STALE-SEED exit then
   k SEED-OWNER$ FILE-BUF FILE-CAP READ-FILE k STALE-IF-DEAD ;

: SELECT ( -- )
   0 begin dup SEED# < while
      dup CHECK-LIVE
      1+
   repeat drop ;

public

: SHAPE-SCAN ( -- )
   0 begin dup TL-M# @ < while
      dup TL-M-EFF$ MINT-SHAPE? if
         dup TL-M-NAME$ SEEDED? 0= if dup NEW-MINT then
      then
      1+
   repeat drop ;

public

: INVENTORY ( -- )
   s" ." TRUST-LINT-ROOT!
   TRUST-LINT-RESET
   TL-SCAN-MANIFEST
   SELECT
   SHAPE-SCAN ;

\ ---- confinement scan --------------------------------------------------------

\ The owner's module test - <owner-stem>-test.f (owner `maki/tensor.f` ->
\ `maki/tensor-test.f`) - may reference the mint. Any other module test that
\ exercises a mint (e.g. a shared package harness) is a documented ALLOW+
\ entry, not an implicit exception.
: STEM$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   a u s" .fs" HAS-EXT? if a u 3 - exit then
   a u s" .f"  HAS-EXT? if a u 2 - exit then
   a u ;

: STEM-TEST$ ( n -- ptr u8 n ) {: k:n :}
   k SEED-OWNER$ STEM$ {: sa:ptr su:n :}
   su 7 + FS-PATH-CAP > if s" refine-lint: stem path too long" FAIL then
   sa STEM-BUF su LINT-BMOVE
   s" -test.f" STEM-BUF su + swap LINT-BMOVE
   STEM-BUF su 7 + ;

: STEM-TEST? ( n -- bool ) {: k:n :}
   CUR$ k STEM-TEST$ FS-PATH= ;

: ALLOWED? ( n -- bool ) {: k:n :}
   CUR$ k SEED-OWNER$ FS-PATH= if LINT-TRUE exit then
   k STEM-TEST? if LINT-TRUE exit then
   k ALLOW-LISTED? ;

: QUAL-TOK? ( n -- bool ) {: k:n :}
   k SEED-NAME$ {: na:ptr nu:n :}
   PTU @ nu 2 + < if LINT-FALSE exit then
   PTA@ PTU @ nu - +  nu  na nu LINT-STR=CI 0= if LINT-FALSE exit then
   PTA@ PTU @ nu - 1- + c@ COLON = ;

: TOK-MINT? ( n -- bool ) {: k:n :}
   PAT-TOK$ k SEED-NAME$ LINT-STR=CI if LINT-TRUE exit then
   k QUAL-TOK? ;

: HIT ( n -- ) {: k:n :}
   REPORT? @ if
      s" REFINE-CONFINE " OUT
      CUR$ OUT COLON C LINE @ U.
      s" : `" OUT k SEED-NAME$ OUT
      s" ` referenced outside owner " OUT k SEED-OWNER$ OUT NL
   then
   BAD+ ;

: MATCH-TOKEN ( -- )
   0 begin dup SEED# < while
      dup TOK-MINT? if
         dup ALLOWED? 0= if dup HIT then
      then
      1+
   repeat drop ;

: STRING-OPENER? ( -- bool )
   PAT-TOK$ LINT-NORMAL-STRING-OPENER? if LINT-TRUE exit then
   PAT-TOK$ LINT-ESC-STRING-OPENER? ;

: SCAN-LINE ( ptr u8 n -- )
   PAT-RESET
   begin PAT-READ-TOKEN while
      STRING-OPENER? if PAT-SKIP-STRING-BODY else MATCH-TOKEN then
   repeat ;

: LINE-LEN ( ptr u8 n -- ptr u8 n )
   dup 0 > IF
      2dup + 1- c@ 13 = IF 1- THEN
   THEN ;

: DO-LINE ( n -- )
   LE !
   LINE @ 1+ LINE !
   LA@ LS @ +  LE @ LS @ -  LINE-LEN
   SCAN-LINE
   LE @ 1+ LS ! ;

: FOR-LINES ( ptr u8 n -- )
   LU ! LA!
   0 LINE !  0 LX !  0 LS !
   begin LX @ LU @ < while
      LA@ LX @ + c@ LF = IF LX @ DO-LINE THEN
      LX @ 1+ LX !
   repeat
   LS @ LU @ < IF LU @ DO-LINE THEN ;

: SCAN-STR ( ptr u8 n ptr u8 n -- ) {: pa:ptr pu:n a:ptr u:n :}
   pa pu CUR!
   a u FOR-LINES ;

\ findings from one string scanned in isolation under the given path
\ (reset -> scan -> count); leaves the run counters untouched.
public

: COUNT-STR-AT ( ptr u8 n ptr u8 n -- n ) {: pa:ptr pu:n a:ptr u:n :}
   REPORT? @ {: report:bool :}
   BAD @ {: prior:n :}
   REPORT-OFF
   0 BAD !
   pa pu a u SCAN-STR
   BAD @ {: found:n :}
   prior BAD !
   report REPORT? !
   found ;

: COUNT-STR ( ptr u8 n -- n ) {: a:ptr u:n :}
   s" rfl-scratch.f" a u COUNT-STR-AT ;

private

: SRC? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" .f" HAS-EXT?  a u s" .fs" HAS-EXT? or ;

public

: SCAN-FILE ( ptr u8 n -- ) {: a:ptr u:n :}
   a u SRC? 0= if exit then
   a u CUR!
   a u FILE-BUF FILE-CAP READ-FILE FOR-LINES ;

: SCAN-ROOT ( ptr u8 n -- ) {: a:ptr u:n :}
   a u DIR? 0= if s" refine-lint: missing scan root" FAIL then
   a u [: SCAN-FILE ;] WALK-FILES ;

\ ---- entry -------------------------------------------------------------------

public

: RESET ( -- )
   0 BAD !
   0 ALLOW# !
   REPORT-ON ;

\ Documented module-test exceptions: the lib/nominal package confines its private
\ representation mints (MINT-PATH/MINT-BINDING/MINT-ROW) but exercises them from
\ shared harnesses, not per-owner <owner-stem>-test.f files. Each cites the owner
\ and the reviewed test (dot habu-epic-type-system-b88c9ecc).
private

: ALLOW-SEED ( -- )
   s" MINT-PATH"    s" lib/nominal/nominal-test.f"      ALLOW+
   s" MINT-BINDING" s" lib/nominal/nominal-prop-test.f" ALLOW+
   s" MINT-ROW"     s" lib/nominal/nominal-test.f"      ALLOW+ ;

: REPORT ( -- )
   s" refine-lint: " OUT SEED# U. s"  mint(s), " OUT
   BAD @ U. s"  finding(s)" OUT NL
   BAD @ 0 > IF 1 throw THEN ;

public

: RUN ( -- )
   BUFFERS
   RESET
   ALLOW-SEED
   INVENTORY
   s" maki" SCAN-ROOT
   s" lib" SCAN-ROOT
   s" src" SCAN-ROOT
   s" tools" SCAN-ROOT
   REPORT ;

\ How many findings the last run recorded, and a way to start a fresh count. The
\ counter itself stays private: exporting the cell would let a caller assign any
\ value to it, including one that hides a finding, whereas these two can only
\ read it or zero it. RESET is the whole-run reset; this clears the count alone,
\ which is what a fixture scanning one string at a time needs.
: FINDINGS ( -- n )
   BAD @ ;

: CLEAR-FINDINGS ( -- )
   0 BAD ! ;

\ The CLI entry. The catch and the LINT-MAIN reporting live here rather than in
\ the wrapper so no lint definition sits outside a package; behavior is the
\ wrapper's previous body unchanged, so stdout, stderr and exit status are the
\ same as before this package existed.
: MAIN ( -- )
   [: RUN ;] catch {: code:n :}
   s" refine-lint" code LINT-MAIN ;

;package
