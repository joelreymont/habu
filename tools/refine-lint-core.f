\ refine-lint-core.f - confine TRUSTED refinement mints to their owning files.
\
\ TRUSTED refinement mints (rows shaped `n -- <nominal family>`, e.g.
\ ROWS-REFINE `n -- CAD-KIND:rows`) forge nominal values from raw cells. They
\ are package-private by convention, but any maki file can reopen the package
\ and call one bare, minting an unvalidated nominal with no gate failing. This
\ lint pins the convention: every reference to a mint outside its owning file
\ is a finding unless the referencing file is cited by the mint's TRUSTED.md
\ Tests cell or named on the explicit allowlist (RFL-ALLOW+).
\
\ The mint set is inventory-driven: an explicit seed list of (name, owner)
\ pairs is cross-checked against TRUSTED.md rows (SEED-DRIFT / STALE-SEED),
\ and a signature-shape scan flags NEW mint-shaped manifest rows (a raw `n`
\ input producing a colon-qualified nominal-family output) that are missing
\ from the seed list (NEW-MINT), so the lint cannot silently rot as mints are
\ added. Bare-nominal mints such as RAW>TENSOR (`n -- tensor`) and the
\ importer projections are seed-only: the shape scan covers the qualified
\ CAD-KIND:/MIR: family namespace where mints are minted today.
\
\ INTERIM enforcement only: the principled endpoint is the TVK-RAW checker
\ capability (dot habu-nominal-storage-raw-a3430ef2), which closes the mint
\ direction at unification. Retire this lint's mint class when that lands.
\
\ Scan discipline: whole-token matching over the shared PAT-* scanner, so `\`
\ and `( )` comments and string-literal bodies are excluded; matching is
\ case-insensitive (the dictionary is case-insensitive) and also catches
\ qualified `PKG:NAME` references. Scanned roots: maki/ lib/ src/ tools/.
\ Manifest rows are read through tools/trust-lint-core.f (TL-M-*).
\
\ Load after tools/date.f, lib/errors.f, lib/string.f, lib/memory.f, lib/fs.f,
\ tools/lint/text.f, tools/lint/token.f, tools/lint/lib.f, and
\ tools/trust-lint-core.f.

$40000 constant RFL-STR-CAP     \ trust-lint manifest string store
$80000 constant RFL-FILE-CAP    \ largest scanned source watermark (checker.f class)
15 constant RFL-SEED#
8 constant RFL-ALLOW-MAX
32 constant RFL-NUM-CAP

10 constant RFL-LF
48 constant RFL-ZERO
58 constant RFL-COLON
96 constant RFL-BTICK

create RFL-NUM-BUF RFL-NUM-CAP allot
create RFL-ONE 1 allot
create RFL-TICK-BUF FS-PATH-CAP 2 + allot
create RFL-ROW-T RFL-SEED# cells allot
create RFL-ALW-NO RFL-ALLOW-MAX cells allot
create RFL-ALW-NU RFL-ALLOW-MAX cells allot
create RFL-ALW-PO RFL-ALLOW-MAX cells allot
create RFL-ALW-PU RFL-ALLOW-MAX cells allot

variable RFL-STR-A
variable RFL-FILE-A
variable RFL-BAD
variable RFL-ALLOW#
variable RFL-REPORT?
variable RFL-NUM-L
variable RFL-LINE
variable RFL-CUR-A
variable RFL-CUR-U
variable RFL-LA
variable RFL-LU
variable RFL-LX
variable RFL-LS
variable RFL-LE

: RFL-STR-A-FIELD ( -- ptr ptr u8 ) RFL-STR-A 0 ptr-field ;
: RFL-FILE-A-FIELD ( -- ptr ptr u8 ) RFL-FILE-A 0 ptr-field ;
: RFL-CUR-A-FIELD ( -- ptr ptr u8 ) RFL-CUR-A 0 ptr-field ;
: RFL-LA-FIELD ( -- ptr ptr u8 ) RFL-LA 0 ptr-field ;
: RFL-ALW-NO-FIELD ( n -- ptr ptr u8 ) cells RFL-ALW-NO + 0 ptr-field ;
: RFL-ALW-PO-FIELD ( n -- ptr ptr u8 ) cells RFL-ALW-PO + 0 ptr-field ;

: RFL-CUR$ ( -- ptr u8 n ) RFL-CUR-A-FIELD @ RFL-CUR-U @ ;
: RFL-CUR! ( ptr u8 n -- ) RFL-CUR-U ! RFL-CUR-A-FIELD ! ;
: RFL-LA@ ( -- ptr u8 ) RFL-LA-FIELD @ ;
: RFL-LA! ( ptr u8 -- ) RFL-LA-FIELD ! ;

: RFL-FAIL ( ptr u8 n -- ) 76 die ;

: RFL-STR-BUF ( -- ptr u8 )
   RFL-STR-A @ 0= if
      RFL-STR-CAP MEM-ALLOC-BYTES drop RFL-STR-A-FIELD !
   then
   RFL-STR-A-FIELD @ ;

: RFL-FILE-BUF ( -- ptr u8 )
   RFL-FILE-A @ 0= if
      RFL-FILE-CAP MEM-ALLOC-BYTES drop RFL-FILE-A-FIELD !
   then
   RFL-FILE-A-FIELD @ ;

: RFL-BUFFERS ( -- )
   RFL-STR-BUF RFL-STR-CAP
   RFL-FILE-BUF RFL-FILE-CAP
   TRUST-LINT-BUFFERS! ;

\ ---- output ----------------------------------------------------------------

: RFL-OUT ( ptr u8 n -- ) {: a:ptr u:n :}
   1 a u LINT-OUT-WRITE ;

: RFL-C ( n -- )
   RFL-ONE c! RFL-ONE 1 RFL-OUT ;

: RFL-NL ( -- ) RFL-LF RFL-C ;

: RFL-U. ( n -- )
   0 RFL-NUM-L !
   dup 0= IF drop RFL-ZERO RFL-C exit THEN
   begin dup 0 > while
      dup 10 mod RFL-ZERO + RFL-NUM-BUF RFL-NUM-L @ + c!
      10 /
      RFL-NUM-L @ 1+ RFL-NUM-L !
   repeat drop
   begin RFL-NUM-L @ 0 > while
      RFL-NUM-L @ 1- RFL-NUM-L !
      RFL-NUM-BUF RFL-NUM-L @ + c@ RFL-C
   repeat ;

: RFL-BAD+ ( -- ) RFL-BAD @ 1+ RFL-BAD ! ;

: RFL-REPORT-ON ( -- ) LINT-TRUE RFL-REPORT? ! ;
: RFL-REPORT-OFF ( -- ) LINT-FALSE RFL-REPORT? ! ;

\ ---- mint seed table: (name, owning file) -----------------------------------
\ The confinement set. Owners outside the scanned roots (test/) are seed-only
\ boundaries whose confinement still applies inside the scanned roots.

: RFL-SEED-NAME$ ( n -- ptr u8 n )
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
      E-TBL-BOUNDS throw
   endcase ;

: RFL-SEED-OWNER$ ( n -- ptr u8 n )
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
      E-TBL-BOUNDS throw
   endcase ;

: RFL-ROW@ ( n -- n ) cells RFL-ROW-T + @ ;
: RFL-ROW! ( n n -- ) {: m:n k:n :} m RFL-ROW-T k cells + ! ;

: RFL-ROW-INIT ( -- )
   0 begin dup RFL-SEED# < while
      -1 over RFL-ROW!
      1+
   repeat drop ;

: RFL-SEEDED? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   0 begin dup RFL-SEED# < while
      dup RFL-SEED-NAME$ a u LINT-STR=CI if drop LINT-TRUE exit then
      1+
   repeat drop LINT-FALSE ;

\ ---- allowlist: documented (mint, caller file) exceptions --------------------
\ Empty today. Entries must cite the review that documented the exception.
\ Caller-supplied strings must stay live for the run (s" literals are).

: RFL-ALLOW-NAME$ ( n -- ptr u8 n )
   dup RFL-ALW-NO-FIELD @ swap cells RFL-ALW-NU + @ ;

: RFL-ALLOW-PATH$ ( n -- ptr u8 n )
   dup RFL-ALW-PO-FIELD @ swap cells RFL-ALW-PU + @ ;

: RFL-ALLOW+ ( ptr u8 n ptr u8 n -- ) {: na:ptr nu:n pa:ptr pu:n :}
   RFL-ALLOW# @ RFL-ALLOW-MAX >= if s" refine-lint: allowlist full" RFL-FAIL then
   na RFL-ALLOW# @ RFL-ALW-NO-FIELD !
   nu RFL-ALW-NU RFL-ALLOW# @ cells + !
   pa RFL-ALLOW# @ RFL-ALW-PO-FIELD !
   pu RFL-ALW-PU RFL-ALLOW# @ cells + !
   RFL-ALLOW# @ 1+ RFL-ALLOW# ! ;

: RFL-ALLOW-LISTED? ( n -- bool ) {: k:n :}
   0 begin dup RFL-ALLOW# @ < while
      dup RFL-ALLOW-NAME$ k RFL-SEED-NAME$ LINT-STR=CI if
         dup RFL-ALLOW-PATH$ RFL-CUR$ FS-PATH= if drop LINT-TRUE exit then
      then
      1+
   repeat drop LINT-FALSE ;

\ ---- inventory: manifest cross-check + mint-shape scan ----------------------

: RFL-FAMILY-TOK? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u RFL-COLON LINT-INDEX-OF MATCH option
     none OF LINT-FALSE ENDOF
     some OF {: i:n :} i 0 >  i u 1- <  and ENDOF
   ;MATCH ;

: RFL-EFF-DASH-IDX ( -- n )
   0 begin dup SN# @ < while
      dup S@ s" --" LINT-STR= if exit then
      1+
   repeat drop -1 ;

: RFL-EFF-RAW-IN? ( n -- bool ) {: dash:n :}
   0 begin dup dash < while
      dup S@ s" n" LINT-STR= if drop LINT-TRUE exit then
      1+
   repeat drop LINT-FALSE ;

: RFL-EFF-FAMILY-OUT? ( n -- bool ) {: dash:n :}
   dash 1+ begin dup SN# @ < while
      dup S@ RFL-FAMILY-TOK? if drop LINT-TRUE exit then
      1+
   repeat drop LINT-FALSE ;

: RFL-MINT-SHAPE? ( ptr u8 n -- bool )
   SPLIT-WHITESPACE
   RFL-EFF-DASH-IDX dup 0 < if drop LINT-FALSE exit then
   {: dash:n :}
   dash RFL-EFF-RAW-IN? 0= if LINT-FALSE exit then
   dash RFL-EFF-FAMILY-OUT? ;

: RFL-SEED-DRIFT ( n n -- ) {: k:n m:n :}
   RFL-REPORT? @ if
      s" SEED-DRIFT TRUSTED.md: `" RFL-OUT k RFL-SEED-NAME$ RFL-OUT
      s" ` row site " RFL-OUT m TL-M-KEY-PATH$ RFL-OUT
      s"  != refine-lint seed owner " RFL-OUT k RFL-SEED-OWNER$ RFL-OUT RFL-NL
   then
   RFL-BAD+ ;

: RFL-STALE-SEED ( n -- ) {: k:n :}
   RFL-REPORT? @ if
      s" STALE-SEED refine-lint: `" RFL-OUT k RFL-SEED-NAME$ RFL-OUT
      s" ` has no TRUSTED.md row at seed owner " RFL-OUT k RFL-SEED-OWNER$ RFL-OUT
      s" ; retire or update the seed list" RFL-OUT RFL-NL
   then
   RFL-BAD+ ;

: RFL-NEW-MINT ( n -- ) {: m:n :}
   RFL-REPORT? @ if
      s" NEW-MINT TRUSTED.md " RFL-OUT m TL-M-KEY-PATH$ RFL-OUT
      s" : `" RFL-OUT m TL-M-NAME$ RFL-OUT
      s" ` `" RFL-OUT m TL-M-EFF$ RFL-OUT
      s" ` is mint-shaped but not in the refine-lint seed list" RFL-OUT RFL-NL
   then
   RFL-BAD+ ;

: RFL-SELECT-SEED ( n -- ) {: k:n :}
   0 begin dup TL-M# @ < while
      dup TL-M-NAME$ k RFL-SEED-NAME$ LINT-STR=CI if
         dup TL-M-KEY-PATH$ k RFL-SEED-OWNER$ FS-PATH= if
            dup k RFL-ROW!
         else
            k over RFL-SEED-DRIFT
         then
      then
      1+
   repeat drop
   k RFL-ROW@ 0 < if
      k RFL-SEED-OWNER$ TL-SCANNED-SITE? if k RFL-STALE-SEED then
   then ;

: RFL-SELECT ( -- )
   RFL-ROW-INIT
   0 begin dup RFL-SEED# < while
      dup RFL-SELECT-SEED
      1+
   repeat drop ;

: RFL-SHAPE-SCAN ( -- )
   0 begin dup TL-M# @ < while
      dup TL-M-EFF$ RFL-MINT-SHAPE? if
         dup TL-M-NAME$ RFL-SEEDED? 0= if dup RFL-NEW-MINT then
      then
      1+
   repeat drop ;

: RFL-INVENTORY ( -- )
   s" ." TRUST-LINT-ROOT!
   TRUST-LINT-RESET
   TL-SCAN-MANIFEST
   RFL-SELECT
   RFL-SHAPE-SCAN ;

\ ---- confinement scan --------------------------------------------------------

: RFL-TICKED$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   u FS-PATH-CAP > if s" refine-lint: path too long" RFL-FAIL then
   RFL-BTICK RFL-TICK-BUF c!
   a RFL-TICK-BUF 1+ u LINT-BMOVE
   RFL-BTICK RFL-TICK-BUF 1+ u + c!
   RFL-TICK-BUF u 2 + ;

: RFL-TESTS-CITED? ( n -- bool ) {: k:n :}
   k RFL-ROW@ dup 0 < if drop LINT-FALSE exit then
   TL-M-TEST$ RFL-CUR$ RFL-TICKED$ LINT-CONTAINS? ;

: RFL-ALLOWED? ( n -- bool ) {: k:n :}
   RFL-CUR$ k RFL-SEED-OWNER$ FS-PATH= if LINT-TRUE exit then
   k RFL-TESTS-CITED? if LINT-TRUE exit then
   k RFL-ALLOW-LISTED? ;

: RFL-QUAL-TOK? ( n -- bool ) {: k:n :}
   k RFL-SEED-NAME$ {: na:ptr nu:n :}
   PTU @ nu 2 + < if LINT-FALSE exit then
   PTA@ PTU @ nu - +  nu  na nu LINT-STR=CI 0= if LINT-FALSE exit then
   PTA@ PTU @ nu - 1- + c@ RFL-COLON = ;

: RFL-TOK-MINT? ( n -- bool ) {: k:n :}
   PAT-TOK$ k RFL-SEED-NAME$ LINT-STR=CI if LINT-TRUE exit then
   k RFL-QUAL-TOK? ;

: RFL-HIT ( n -- ) {: k:n :}
   RFL-REPORT? @ if
      s" REFINE-CONFINE " RFL-OUT
      RFL-CUR$ RFL-OUT RFL-COLON RFL-C RFL-LINE @ RFL-U.
      s" : `" RFL-OUT k RFL-SEED-NAME$ RFL-OUT
      s" ` referenced outside owner " RFL-OUT k RFL-SEED-OWNER$ RFL-OUT RFL-NL
   then
   RFL-BAD+ ;

: RFL-MATCH-TOKEN ( -- )
   0 begin dup RFL-SEED# < while
      dup RFL-TOK-MINT? if
         dup RFL-ALLOWED? 0= if dup RFL-HIT then
      then
      1+
   repeat drop ;

: RFL-STRING-OPENER? ( -- bool )
   PAT-TOK$ LINT-NORMAL-STRING-OPENER? if LINT-TRUE exit then
   PAT-TOK$ LINT-ESC-STRING-OPENER? ;

: RFL-SCAN-LINE ( ptr u8 n -- )
   PAT-RESET
   begin PAT-READ-TOKEN while
      RFL-STRING-OPENER? if PAT-SKIP-STRING-BODY else RFL-MATCH-TOKEN then
   repeat ;

: RFL-LINE-LEN ( ptr u8 n -- ptr u8 n )
   dup 0 > IF
      2dup + 1- c@ 13 = IF 1- THEN
   THEN ;

: RFL-DO-LINE ( n -- )
   RFL-LE !
   RFL-LINE @ 1+ RFL-LINE !
   RFL-LA@ RFL-LS @ +  RFL-LE @ RFL-LS @ -  RFL-LINE-LEN
   RFL-SCAN-LINE
   RFL-LE @ 1+ RFL-LS ! ;

: RFL-FOR-LINES ( ptr u8 n -- )
   RFL-LU ! RFL-LA!
   0 RFL-LINE !  0 RFL-LX !  0 RFL-LS !
   begin RFL-LX @ RFL-LU @ < while
      RFL-LA@ RFL-LX @ + c@ RFL-LF = IF RFL-LX @ RFL-DO-LINE THEN
      RFL-LX @ 1+ RFL-LX !
   repeat
   RFL-LS @ RFL-LU @ < IF RFL-LU @ RFL-DO-LINE THEN ;

: RFL-SCAN-STR ( ptr u8 n ptr u8 n -- ) {: pa:ptr pu:n a:ptr u:n :}
   pa pu RFL-CUR!
   a u RFL-FOR-LINES ;

\ findings from one string scanned in isolation under the given path
\ (reset -> scan -> count); leaves the run counters untouched.
: RFL-COUNT-STR-AT ( ptr u8 n ptr u8 n -- n ) {: pa:ptr pu:n a:ptr u:n :}
   RFL-REPORT? @ {: report:bool :}
   RFL-BAD @ {: prior:n :}
   RFL-REPORT-OFF
   0 RFL-BAD !
   pa pu a u RFL-SCAN-STR
   RFL-BAD @ {: found:n :}
   prior RFL-BAD !
   report RFL-REPORT? !
   found ;

: RFL-COUNT-STR ( ptr u8 n -- n ) {: a:ptr u:n :}
   s" rfl-scratch.f" a u RFL-COUNT-STR-AT ;

: RFL-SRC? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" .f" HAS-EXT?  a u s" .fs" HAS-EXT? or ;

: RFL-SCAN-FILE ( ptr u8 n -- ) {: a:ptr u:n :}
   a u RFL-SRC? 0= if exit then
   a u RFL-CUR!
   a u RFL-FILE-BUF RFL-FILE-CAP READ-FILE RFL-FOR-LINES ;

: RFL-SCAN-ROOT ( ptr u8 n -- ) {: a:ptr u:n :}
   a u DIR? 0= if s" refine-lint: missing scan root" RFL-FAIL then
   a u [: RFL-SCAN-FILE ;] WALK-FILES ;

\ ---- entry -------------------------------------------------------------------

: RFL-RESET ( -- )
   0 RFL-BAD !
   0 RFL-ALLOW# !
   RFL-REPORT-ON
   RFL-ROW-INIT ;

: RFL-REPORT ( -- )
   s" refine-lint: " RFL-OUT RFL-SEED# RFL-U. s"  mint(s), " RFL-OUT
   RFL-BAD @ RFL-U. s"  finding(s)" RFL-OUT RFL-NL
   RFL-BAD @ 0 > IF 1 throw THEN ;

: REFINE-LINT ( -- )
   RFL-BUFFERS
   RFL-RESET
   RFL-INVENTORY
   s" maki" RFL-SCAN-ROOT
   s" lib" RFL-SCAN-ROOT
   s" src" RFL-SCAN-ROOT
   s" tools" RFL-SCAN-ROOT
   RFL-REPORT ;
