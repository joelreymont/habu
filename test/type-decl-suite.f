\ type-decl-suite.f — behavior suite for the TYPEFAMILY/SUMTYPE declaration
\ grammar (src/core/sumtype.f; docs/type-families.md §9, PLAN item 6). Run BY
\ THE ENGINE over stdin, exactly like test/type-family-suite.f:
\     bin/hb < test/type-decl-suite.f
\ Everything here is USER source arriving after the engine sealed the
\ TFAM/TYPE/MATCH system packages (TFAM 2b-ii), so every accepting declaration
\ below is also the post-seal proof: user declarations register families
\ through the baked grammar words without opening any reserved package.
\ A failure prints F<index> + detail; REPORT exits 1 on any fail.

require test/checker-assert.f

variable #FAIL
variable #CASE

: T-FAIL ( -- )
   [char] F emit #CASE @ .
   #FAIL @ 1 + #FAIL ! ;
: T= ( n n -- ) {: got:n want:n :}
   #CASE @ 1 + #CASE !
   got want <> if
      T-FAIL s" assert: expected " type want . s" got " type got . cr
   then ;
: T$= ( ptr u8 n ptr u8 n -- ) {: ga:ptr gu:n wa:ptr wu:n :}
   #CASE @ 1 + #CASE !
   gu wu <> if
      T-FAIL s" assert string len: expected " type wu . s" got " type gu . cr exit
   then
   0 begin dup gu < while
      dup ga + c@  over wa + c@ <> if
         drop T-FAIL s" assert string byte mismatch" type cr exit
      then
      1+
   repeat drop ;

\ substring search (diag packet assertions), engine-suite MEO-CONTAINS? shape.
variable TDC-I
: TDT-AT? ( ptr u8 n ptr u8 n -- bool ) {: h:ptr hu:n n:ptr nu:n :}
   hu nu < if 0 0= 0= exit then
   0 begin dup nu < while
      dup n + c@  over h + c@ <> if drop 0 0= 0= exit then
      1+
   repeat drop 0 0= ;
: TDT-CONTAINS? ( ptr u8 n ptr u8 n -- bool ) {: h:ptr hu:n n:ptr nu:n :}
   0 TDC-I !
   begin TDC-I @ nu + hu <= while
      h TDC-I @ +  nu  n nu TDT-AT? if 0 0= exit then
      TDC-I @ 1+ TDC-I !
   repeat 0 0= 0= ;

\ evaluate a declaration string, returning its throw code (0 = accepted).
\ Checked: the dynamic-evaluate boundary is the existing audited
\ INCLUDE-EVALUATE; the throw code crosses the eval frame into this compiled
\ catch (a TOP-LEVEL catch around evaluate loses the code — see LESSONS.md).
variable TDTE-A   variable TDTE-U
: TDT-EVAL-GO ( -- )
   TDTE-A @ TDTE-U @ INCLUDE-EVALUATE ;
: TDT-EVAL-CATCH ( ptr u8 n -- n )
   TDTE-U ! TDTE-A !
   [: TDT-EVAL-GO ;] catch ;

\ registry high-water baseline: every rejected declaration must restore it.
\ Checked through the PRIM-modeled read-only registry queries.
variable TDB-TFAM   variable TDB-SUMV   variable TDB-SCH
variable TDB-ROOT   variable TDB-STR    variable TDB-PK
variable TDB-PF
: TDT-BASE! ( -- )
   TFAM-N@ TDB-TFAM !   SUMV-N@ TDB-SUMV !
   SCHEMA-N@ TDB-SCH !  SCHEMA-ROOT-N@ TDB-ROOT !
   TF-STR-U@ TDB-STR !  TF-PK-N@ TDB-PK !   PF-N@ TDB-PF ! ;
: TDT-BASE= ( -- )
   TFAM-N@ TDB-TFAM @ T=   SUMV-N@ TDB-SUMV @ T=
   SCHEMA-N@ TDB-SCH @ T=  SCHEMA-ROOT-N@ TDB-ROOT @ T=
   TF-STR-U@ TDB-STR @ T=  TF-PK-N@ TDB-PK @ T=   PF-N@ TDB-PF @ T= ;
: TDT-NEG ( ptr u8 n n -- ) {: a:ptr u:n code:n :}
   TDT-BASE!
   a u TDT-EVAL-CATCH code T=
   TDT-BASE= ;

variable TDF    variable TDOK   variable TDV0
variable TDX    variable TDY    variable TDTC

\ silence expected declaration diagnostics (asserted explicitly further down).
create TDIAG-BUF 8192 allot
TDIAG-BUF 8192 DIAG-BUFFER!

\ ---------------------------------------------------------------------------
\ TYPEFAMILY: registers a TK-CELL family in the global scope, usable in sigs.
\ ---------------------------------------------------------------------------
TYPEFAMILY tdfoo 2
s" " s" tdfoo" TFAM-FIND-IN TDOK ! TDF !
TDOK @ -1 T=
TDF @ TFAM-ARITY@ 2 T=
TDF @ TFAM-KIND@ TK-CELL T=
TDF @ TFAM-CELL? -1 T=
TDF @ TFAM-VIS@ CHECKER-PACKAGE-PUBLIC T=
TDF @ TFAM-PKG$ s" " T$=
TDF @ TFAM-NAME$ s" tdfoo" T$=

s" TDOK-USE ( tdfoo<n,n> -- tdfoo<n,n> )" CHECK-QUIET-CANDIDATE! -1 T=
s" TDBAD-ARITY ( tdfoo<n> -- ) drop" CHECK-QUIET-CANDIDATE! 0 T=
s" TDBAD-UPPER ( Tdfoo<n,n> -- ) drop" CHECK-QUIET-CANDIDATE! 0 T=

\ zero-arity family: the bare token resolves through TFAM, not nominal lookup.
TYPEFAMILY tdzero 0
s" TDOK-ZERO ( tdzero -- tdzero )" CHECK-QUIET-CANDIDATE! -1 T=
s" TDBAD-ZERO-N ( tdzero -- n )" CHECK-QUIET-CANDIDATE! 0 T=

\ ---------------------------------------------------------------------------
\ SUMTYPE: registers a TK-SUM family + SUMV variants + payload schemas, and
\ closes the declaration by wiring the variant range and max payload slots.
\ ---------------------------------------------------------------------------
SUMTYPE tdres 2
  VARIANT ok  a ;VARIANT
  VARIANT err b ;VARIANT
;SUMTYPE
s" " s" tdres" TFAM-FIND-IN TDOK ! TDF !
TDOK @ -1 T=
TDF @ TFAM-KIND@ TK-SUM T=
TDF @ TFAM-SUM? -1 T=
TDF @ TFAM-ARITY@ 2 T=
\ variant range wired at ;SUMTYPE (census contradiction C6 regression):
TDF @ TFAM-VAR-COUNT@ 2 T=
TDF @ TFAM-VAR-START@ TDV0 !
TDV0 @ SUMV-FAM@ TDF @ T=
TDV0 @ SUMV-NAME$ s" ok" T$=
TDV0 @ SUMV-TAG@ 0 T=
TDV0 @ 1 + SUMV-NAME$ s" err" T$=
TDV0 @ 1 + SUMV-TAG@ 1 T=
TDF @ TFAM-SLOTS@ 1 T=
\ payload schemas: ok = paramref 0, err = paramref 1, one cell each.
TDV0 @ SUMV-SCH-COUNT@ 1 T=
TDV0 @ SUMV-PAYCELLS@ 1 T=
TDV0 @ SUMV-SCH-START@ SCHEMA-ROOT@ SCHEMA-PARAM? -1 T=
TDV0 @ SUMV-SCH-START@ SCHEMA-ROOT@ SCHEMA-A@ 0 T=
TDV0 @ 1 + SUMV-SCH-START@ SCHEMA-ROOT@ SCHEMA-A@ 1 T=
\ the sum family name is usable in signatures as a logical type expression.
s" TDOK-RES ( tdres<n,n> -- tdres<n,n> )" CHECK-QUIET-CANDIDATE! -1 T=
s" TDBAD-RES1 ( tdres<n> -- ) drop" CHECK-QUIET-CANDIDATE! 0 T=

\ zero-payload variant: padding-only, paycells 0 (docs §4 option shape).
SUMTYPE tdopt 1
  VARIANT none   ;VARIANT
  VARIANT some a ;VARIANT
;SUMTYPE
s" " s" tdopt" TFAM-FIND-IN TDOK ! TDF !
TDOK @ -1 T=
TDF @ TFAM-VAR-COUNT@ 2 T=
TDF @ TFAM-SLOTS@ 1 T=
TDF @ TFAM-VAR-START@ SUMV-PAYCELLS@ 0 T=
TDF @ TFAM-VAR-START@ SUMV-SCH-COUNT@ 0 T=

\ multi-cell concrete payload (docs §8 parse-result): ptr u8 + n schemas.
SUMTYPE tdparse 1
  VARIANT yes a ;VARIANT
  VARIANT no  ptr u8 n ;VARIANT
;SUMTYPE
s" " s" tdparse" TFAM-FIND-IN TDOK ! TDF !
TDOK @ -1 T=
TDF @ TFAM-SLOTS@ 2 T=
TDF @ TFAM-VAR-START@ 1 + SUMV-PAYCELLS@ 2 T=
TDF @ TFAM-VAR-START@ 1 + SUMV-SCH-START@ SCHEMA-ROOT@ TDX !
TDX @ SCHEMA-PTR? -1 T=
TDX @ SCHEMA-A@ SCHEMA-CON? -1 T=
TDX @ SCHEMA-A@ SCHEMA-A@ s" u8" CON-OF T=
TDF @ TFAM-VAR-START@ 1 + SUMV-SCH-START@ 1 + SCHEMA-ROOT@ TDY !
TDY @ SCHEMA-CON? -1 T=
TDY @ SCHEMA-A@ CC-N T=

\ zero-arity sum (payload-free variants only): the enum-shaped sum.
SUMTYPE tdlight 0
  VARIANT red   ;VARIANT
  VARIANT green ;VARIANT
  VARIANT blue  ;VARIANT
;SUMTYPE
s" " s" tdlight" TFAM-FIND-IN TDOK ! TDF !
TDOK @ -1 T=
TDF @ TFAM-VAR-COUNT@ 3 T=
TDF @ TFAM-SLOTS@ 0 T=
TDF @ TFAM-VAR-START@ 2 + SUMV-TAG@ 2 T=

\ arity above the old 4-arg cap parses through growable schema storage.
SUMTYPE tdwide 8
  VARIANT lo a ;VARIANT
  VARIANT hi h ;VARIANT
;SUMTYPE
s" " s" tdwide" TFAM-FIND-IN TDOK ! TDF !
TDOK @ -1 T=
TDF @ TFAM-ARITY@ 8 T=
TDF @ TFAM-VAR-START@ 1 + SUMV-SCH-START@ SCHEMA-ROOT@ SCHEMA-A@ 7 T=
s" TDOK-WIDE ( tdwide<n,n,n,n,n,n,n,n> -- tdwide<n,n,n,n,n,n,n,n> )" CHECK-QUIET-CANDIDATE! -1 T=

\ mixed payload widths: slots = max across variants.
SUMTYPE tdmix 2
  VARIANT small a ;VARIANT
  VARIANT big a b n ;VARIANT
;SUMTYPE
s" " s" tdmix" TFAM-FIND-IN TDOK ! TDF !
TDOK @ -1 T=
TDF @ TFAM-SLOTS@ 3 T=

\ ---------------------------------------------------------------------------
\ ENUM (item 14, docs §9.3): `ENUM name v0 v1 .. ;ENUM` registers a TK-ENUM
\ family — a zero-payload sum (arity 0, slots 0) — one bare variant name per
\ token becoming a payload-free SUMV row in declaration-tag order. It shares the
\ SUMTYPE close/rollback/constructor path, so the family MATCHes and gets one
\ generated constructor per variant just like an arity-0 zero-payload sum.
\ ---------------------------------------------------------------------------
ENUM tdcolor
  red
  green
  blue
;ENUM
s" " s" tdcolor" TFAM-FIND-IN TDOK ! TDF !
TDOK @ -1 T=
TDF @ TFAM-KIND@ TK-ENUM T=
TDF @ TFAM-ENUM? -1 T=
TDF @ TFAM-SUM? 0 T=
TDF @ TFAM-CELL? 0 T=
TDF @ TFAM-ARITY@ 0 T=
TDF @ TFAM-VIS@ CHECKER-PACKAGE-PUBLIC T=
TDF @ TFAM-VAR-COUNT@ 3 T=
TDF @ TFAM-SLOTS@ 0 T=
\ width is tag-only (docs §18: WIDTH(enum) = tag width = 1).
TDF @ TFAM-WIDTH@ 1 T=
TDF @ TFAM-VAR-START@ TDV0 !
TDV0 @ SUMV-FAM@ TDF @ T=
TDV0 @ SUMV-NAME$ s" red" T$=
TDV0 @ SUMV-TAG@ 0 T=
TDV0 @ SUMV-PAYCELLS@ 0 T=
TDV0 @ SUMV-SCH-COUNT@ 0 T=
TDV0 @ 1 + SUMV-NAME$ s" green" T$=
TDV0 @ 1 + SUMV-TAG@ 1 T=
TDV0 @ 2 + SUMV-NAME$ s" blue" T$=
TDV0 @ 2 + SUMV-TAG@ 2 T=
\ the bare enum tail resolves as a logical type in a signature (arity-0 family).
s" TDE-ID ( tdcolor -- tdcolor )" CHECK-QUIET-CANDIDATE! -1 T=
\ generated constructors: TDCOLOR:GREEN ( -- tdcolor ). A raw n is NOT the enum
\ (docs §23 rejected example): a payload-free enum ctor takes no input and yields
\ the enum type; a bare 0 cannot certify as tdcolor.
s" TDE-MK ( -- tdcolor ) TDCOLOR:GREEN" CHECK-QUIET-CANDIDATE! -1 T=
s" TDE-BAD ( -- tdcolor ) 0" CHECK-QUIET-CANDIDATE! 0 T=
\ a payload-free ctor rejects a spurious input just like the arity-0 sum ctors.
s" TDE-MK2 ( n -- tdcolor ) TDCOLOR:RED" CHECK-QUIET-CANDIDATE! 0 T=

\ ---------------------------------------------------------------------------
\ PRODUCT (item 15, docs §9.4): `PRODUCT name arity FIELD f t .. ;PRODUCT`
\ registers a TK-PRODUCT family — a single-shape record with named PF-* field
\ rows and NO tag. Each `FIELD name type` adds one PF row (family, field tail,
\ field schema root, physical slot) and one cell of width, so TFAM-SLOTS = field
\ count and WIDTH(product) = field cells. Metadata only in this slice: the family
\ resolves in signatures and expands to hidden fields through the generic
\ LAYOUT-PUSH-FIELDS (shared with sums/enums), but no constructor is published.
\ ---------------------------------------------------------------------------
PRODUCT tdpair 2
  FIELD fst a
  FIELD snd b
;PRODUCT
s" " s" tdpair" TFAM-FIND-IN TDOK ! TDF !
TDOK @ -1 T=
TDF @ TFAM-KIND@ TK-PRODUCT T=
TDF @ TFAM-PRODUCT? -1 T=
TDF @ TFAM-SUM? 0 T=
TDF @ TFAM-ENUM? 0 T=
TDF @ TFAM-CELL? 0 T=
TDF @ TFAM-LAYOUT? -1 T=
TDF @ TFAM-ARITY@ 2 T=
TDF @ TFAM-VIS@ CHECKER-PACKAGE-PUBLIC T=
\ width = field cells, NO tag (docs §18: WIDTH(product) = sum of field widths).
TDF @ TFAM-SLOTS@ 2 T=
TDF @ TFAM-WIDTH@ 2 T=
\ two PF field rows, id-keyed by (family, tail), in declaration slot order.
TDF @ TFAM-FLD-COUNT@ 2 T=
TDF @ s" fst" PF-FIND TDOK ! TDX !
TDOK @ -1 T=
TDX @ PF-FAM@ TDF @ T=
TDX @ PF-SLOT@ 0 T=
TDX @ PF-NAME$ s" fst" T$=
TDF @ s" snd" PF-FIND TDOK ! TDY !
TDOK @ -1 T=
TDY @ PF-SLOT@ 1 T=
\ field schema: fst = paramref 0, snd = paramref 1 (one cell each).
TDX @ PF-SCH@ SCHEMA-ROOT@ SCHEMA-PARAM? -1 T=
TDX @ PF-SCH@ SCHEMA-ROOT@ SCHEMA-A@ 0 T=
TDY @ PF-SCH@ SCHEMA-ROOT@ SCHEMA-A@ 1 T=
\ generated-word metadata (item 15): two generator-owned SUMV rows sharing the
\ field schema range, ctor package derived from the (pkg, tail) identity.
TDF @ TFAM-VAR-COUNT@ 2 T=
TDF @ TFAM-VAR-START@ SUMV-NAME$ s" make" T$=
TDF @ TFAM-VAR-START@ 1 + SUMV-NAME$ s" unmake" T$=
TDF @ TFAM-VAR-START@ SUMV-CTOR-PKG$ s" TDPAIR" T$=
TDF @ TFAM-VAR-START@ SUMV-PAYCELLS@ 2 T=
TDF @ TFAM-VAR-START@ SUMV-SCH-COUNT@ 2 T=
TDF @ TFAM-VAR-START@ SUMV-SCH-START@ SCHEMA-ROOT@ SCHEMA-PARAM? -1 T=
\ a concrete-arg product expands to hidden fields in a signature and transports
\ as ONE whole bundle (dup/drop are width-aware, item 12); identity flows.
s" TDP-ID ( tdpair<n,n> -- tdpair<n,n> )" CHECK-QUIET-CANDIDATE! -1 T=
s" TDP-DUP ( tdpair<n,n> -- tdpair<n,n> tdpair<n,n> ) dup" CHECK-QUIET-CANDIDATE! -1 T=
s" TDP-DROP ( tdpair<n,n> -- ) drop" CHECK-QUIET-CANDIDATE! -1 T=
\ hidden fields never masquerade as bare cells: the bundle cannot split into n n.
s" TDP-SPLIT ( tdpair<n,n> -- n n )" CHECK-QUIET-CANDIDATE! 0 T=
\ hidden physical field names never resolve in a public signature.
s" TDP-HID ( @tdpair.slot0<n,n> -- ) drop" CHECK-QUIET-CANDIDATE! 0 T=

\ concrete-field product (arity 0, all fields concrete n): width = field count.
PRODUCT tdpoint 0
  FIELD x n
  FIELD y n
;PRODUCT
s" " s" tdpoint" TFAM-FIND-IN TDOK ! TDF !
TDOK @ -1 T=
TDF @ TFAM-ARITY@ 0 T=
TDF @ TFAM-SLOTS@ 2 T=
TDF @ TFAM-WIDTH@ 2 T=
TDF @ s" x" PF-FIND TDOK ! TDX !   TDOK @ -1 T=
TDX @ PF-SCH@ SCHEMA-ROOT@ SCHEMA-CON? -1 T=
TDX @ PF-SCH@ SCHEMA-ROOT@ SCHEMA-A@ CC-N T=
s" TDPT-ID ( tdpoint -- tdpoint )" CHECK-QUIET-CANDIDATE! -1 T=

\ mixed param + ptr fields: a ptr field is one cell; arity 1 has one param field.
PRODUCT tdbuf 1
  FIELD cap a
  FIELD raw ptr u8
;PRODUCT
s" " s" tdbuf" TFAM-FIND-IN TDOK ! TDF !
TDOK @ -1 T=
TDF @ TFAM-SLOTS@ 2 T=
TDF @ s" raw" PF-FIND TDOK ! TDX !   TDOK @ -1 T=
TDX @ PF-SLOT@ 1 T=
TDX @ PF-SCH@ SCHEMA-ROOT@ SCHEMA-PTR? -1 T=
TDX @ PF-SCH@ SCHEMA-ROOT@ SCHEMA-A@ SCHEMA-CON? -1 T=
TDX @ PF-SCH@ SCHEMA-ROOT@ SCHEMA-A@ SCHEMA-A@ s" u8" CON-OF T=

\ ---------------------------------------------------------------------------
\ item 12 (habu-tfam-12), slice 1 — layout-aware generic stack ops. A logical
\ sum/enum/product layout value is still ONE physical T-PARAM cell at this stage
\ (item 7 kept it one cell; no LAYOUT-PUSH-FIELDS expansion, no published
\ constructors, so a wider-than-one-cell layout value is not even constructible
\ yet). A WHOLE-BUNDLE transport op moves the value as one logical unit and is
\ now accepted: dup/drop/swap/over/nip/rot/-rot/tuck/2dup/2drop/2swap/2over,
\ >r/r>/r@/2>r/2r>/2r@, and locals capture. Every OTHER touch still fails
\ closed: ?dup (branches on the tag cell), control predicates, higher-order
\ apply, arithmetic/compare/store, and hidden '@' field names in a public
\ signature. See docs/type-families.md §17.
\ ---------------------------------------------------------------------------
\ a layout value flows through untouched (identity is fine).
s" TD7-OPT-ID ( tdopt<n> -- tdopt<n> )" CHECK-QUIET-CANDIDATE! -1 T=
s" TD7-ENUM-ID ( tdlight -- tdlight )" CHECK-QUIET-CANDIDATE! -1 T=
\ whole-bundle transport ops move a sum layout value as one logical unit.
s" TD7-DROP ( tdres<n,n> -- ) drop" CHECK-QUIET-CANDIDATE! -1 T=
s" TD7-DUP ( tdres<n,n> -- tdres<n,n> tdres<n,n> ) dup" CHECK-QUIET-CANDIDATE! -1 T=
s" TD7-SWAP ( tdres<n,n> n -- n tdres<n,n> ) swap" CHECK-QUIET-CANDIDATE! -1 T=
s" TD7-OVER ( tdres<n,n> n -- tdres<n,n> n tdres<n,n> ) over" CHECK-QUIET-CANDIDATE! -1 T=
s" TD7-NIP ( tdres<n,n> n -- n ) nip" CHECK-QUIET-CANDIDATE! -1 T=
s" TD7-TOR ( tdres<n,n> -- tdres<n,n> ) >r r>" CHECK-QUIET-CANDIDATE! -1 T=
\ an enum (zero-payload) layout value is one logical cell and transports too.
s" TD7-ENUM-DROP ( tdlight -- ) drop" CHECK-QUIET-CANDIDATE! -1 T=
\ a layout value now captures into a local as one whole bundle.
s" TD7-LOCAL ( tdres<n,n> -- n ) {: x :} x drop 0" CHECK-QUIET-CANDIDATE! -1 T=
\ cell families are unaffected: a one-cell tdfoo value is dropped/duped normally.
s" TD7-CELL-DROP ( tdfoo<n,n> -- n ) drop 0" CHECK-QUIET-CANDIDATE! -1 T=
s" TD7-CELL-DUP ( tdfoo<n,n> -- tdfoo<n,n> tdfoo<n,n> ) dup" CHECK-QUIET-CANDIDATE! -1 T=
\ hidden physical field names never resolve in a public signature.
s" TD7-HID-SLOT ( @tdres.slot0<n,n> -- ) drop" CHECK-QUIET-CANDIDATE! 0 T=
s" TD7-HID-TAG ( @tdopt.tag<n> -- ) drop" CHECK-QUIET-CANDIDATE! 0 T=

\ --- item 12 slice-1: the full generic stack-op surface transports a bundle ---
\ every prim in the dot's list, on a width-1 sum, a wider (slots=3) sum, and an
\ enum, with a cell above/below to prove only the layout bundle moves as a unit.
s" TD12-ROT ( tdres<n,n> n n -- n n tdres<n,n> ) rot" CHECK-QUIET-CANDIDATE! -1 T=
s" TD12-MROT ( n n tdres<n,n> -- tdres<n,n> n n ) -rot" CHECK-QUIET-CANDIDATE! -1 T=
s" TD12-TUCK ( n tdres<n,n> -- tdres<n,n> n tdres<n,n> ) tuck" CHECK-QUIET-CANDIDATE! -1 T=
s" TD12-2DUP ( tdres<n,n> n -- tdres<n,n> n tdres<n,n> n ) 2dup" CHECK-QUIET-CANDIDATE! -1 T=
s" TD12-2DROP ( tdres<n,n> n -- ) 2drop" CHECK-QUIET-CANDIDATE! -1 T=
s" TD12-2SWAP ( tdres<n,n> n n n -- n n tdres<n,n> n ) 2swap" CHECK-QUIET-CANDIDATE! -1 T=
s" TD12-2OVER ( tdres<n,n> n n n -- tdres<n,n> n n n tdres<n,n> n ) 2over" CHECK-QUIET-CANDIDATE! -1 T=
s" TD12-RAT ( tdres<n,n> -- tdres<n,n> tdres<n,n> ) >r r@ r>" CHECK-QUIET-CANDIDATE! -1 T=
s" TD12-2TOR ( tdres<n,n> n -- tdres<n,n> n ) 2>r 2r>" CHECK-QUIET-CANDIDATE! -1 T=
s" TD12-2RAT ( tdres<n,n> n -- tdres<n,n> n tdres<n,n> n ) 2>r 2r@ 2r>" CHECK-QUIET-CANDIDATE! -1 T=
\ a wider sum (tdmix: slots=3) still moves as ONE logical cell at this stage.
s" TD12-WIDE-DUP ( tdmix<n,n> -- tdmix<n,n> tdmix<n,n> ) dup" CHECK-QUIET-CANDIDATE! -1 T=
s" TD12-WIDE-SWAP ( tdmix<n,n> n -- n tdmix<n,n> ) swap" CHECK-QUIET-CANDIDATE! -1 T=
\ transport never lets a layout value satisfy a cell slot: swap keeps the bundle
\ whole, so a signature that splits it (claims a bare n out) still rejects.
s" TD12-SWAP-SPLIT ( tdres<n,n> n -- tdres<n,n> tdres<n,n> ) swap" CHECK-QUIET-CANDIDATE! 0 T=

\ --- item 12 slice-3b + habu-tfam-12-pass: branch-scoped locals lower ----------
\ The pass-2 recompiler used to read the per-CHECK LOCW table by LIVE index
\ AFTER the hook certified, but branch-scoped locals are popped from #LOC at
\ their join and the scalar emitter reuses their frame slots (habu2.f
\ LCFPUSH/LCFPOP) — so these bodies were rejected fail-closed
\ (E-LAYOUT-BRANCH-LOCAL). The bind-sequence width table (checker LOCW-HW +
\ the P2-CARVE-W live replay) lifted the guard: a local bound inside branch
\ scope in a width-aware definition now certifies and lowers position-
\ correctly. Execution proof: test/type-layout-lower-pending.f TLPX-BR*.
s" TD12-BRLOC-IF ( tdres<n,n> n -- tdres<n,n> ) 0 > if {: a :} a else then" CHECK-QUIET-CANDIDATE! -1 T=
s" TD12-BRLOC-CASE ( tdres<n,n> n -- tdres<n,n> ) case 0 of {: a :} a endof endcase" CHECK-QUIET-CANDIDATE! -1 T=
\ a scalar AND a wide local bound in the same branch: certifies.
s" TD12-BRLOC-MIX ( tdres<n,n> n -- tdres<n,n> n ) 0 > if 5 {: s:n :} {: a :} a s else 0 then" CHECK-QUIET-CANDIDATE! -1 T=
\ a purely SCALAR branch local in a width-aware definition (the top-level wide
\ local a triggers pass-2, which replays s's bind by sequence): certifies.
s" TD12-BRLOC-SCALAR ( n tdres<n,n> -- tdres<n,n> n ) {: a :} 0 > if 5 {: s:n :} s else 9 then a swap" CHECK-QUIET-CANDIDATE! -1 T=
\ the previously supported boundary stays certified: a wide local bound at TOP
\ LEVEL, a scalar branch local in a NON-width-aware definition, and a top-level
\ wide local REFERENCED inside both branches.
s" TD12-BROK-TOPWIDE ( n tdres<n,n> -- tdres<n,n> n ) {: a :} a swap" CHECK-QUIET-CANDIDATE! -1 T=
s" TD12-BROK-SCALARBR ( n -- n ) 0 > if 5 {: s:n :} s else 9 then" CHECK-QUIET-CANDIDATE! -1 T=
s" TD12-BROK-REF ( n tdres<n,n> -- tdres<n,n> ) {: a :} 0 > if a else a then" CHECK-QUIET-CANDIDATE! -1 T=

\ --- item 12 slice-1 negatives: non-transport touches still fail closed --------
\ ?dup branches on the tag cell: width-breaking, so it rejects a layout value.
s" TD12-QDUP ( tdres<n,n> -- ) ?dup drop drop" CHECK-QUIET-CANDIDATE! 0 T=
s" TD12-QDUP-ENUM ( tdlight -- ) ?dup drop drop" CHECK-QUIET-CANDIDATE! 0 T=
\ control predicates read the top cell: a layout value is not a flag.
s" TD12-IF ( tdres<n,n> -- ) if then" CHECK-QUIET-CANDIDATE! 0 T=
\ arithmetic/compare/unary inspect the cell: reject a layout value.
s" TD12-ZEQ ( tdres<n,n> -- bool ) 0=" CHECK-QUIET-CANDIDATE! 0 T=
s" TD12-ADD ( tdres<n,n> n -- n ) +" CHECK-QUIET-CANDIDATE! 0 T=
\ higher-order apply (execute) must not consume a layout value as an xt/cell.
s" TD12-EXEC ( tdres<n,n> -- ) execute" CHECK-QUIET-CANDIDATE! 0 T=
\ memory store/fetch is a field coercion outside constructors/MATCH: reject.
s" TD12-STORE ( tdres<n,n> ptr a -- ) !" CHECK-QUIET-CANDIDATE! 0 T=

\ --- item 12: possibly-linear layout transports reject until TFAM 11 ---------
\ A layout family whose args carry a linear con — or an arg still unresolved,
\ which may later bind linear — must not transport: the linear discipline
\ counts concrete linear cons, so a layout bundle would let a copy duplicate
\ (or a drop lose) the hidden payload resource. Identity flow stays legal, and
\ the same family with non-linear args transports freely.
DEFLINEAR tdown
SUMTYPE tdlin 1
  VARIANT hold a ;VARIANT
;SUMTYPE
s" TDLIN-DUP ( tdlin<tdown> -- tdlin<tdown> tdlin<tdown> ) dup" CHECK-QUIET-CANDIDATE! 0 T=
s" TDLIN-DROP ( tdlin<tdown> -- ) drop" CHECK-QUIET-CANDIDATE! 0 T=
s" TDLIN-NIP ( tdlin<tdown> n -- n ) nip" CHECK-QUIET-CANDIDATE! 0 T=
s" TDLIN-2DROP ( tdlin<tdown> n -- ) 2drop" CHECK-QUIET-CANDIDATE! 0 T=
s" TDLIN-LOCAL ( tdlin<tdown> -- ) {: x :} x drop" CHECK-QUIET-CANDIDATE! 0 T=
\ TFAM 11 move-class relaxation: a count-preserving move (>r r>) of a resolved
\ linear bundle conserves the count (LIN-CHECK before=after) and now certifies;
\ copy/drop above still reject. An OPEN-arg (may-later-bind-linear) layout stays
\ fail-closed for every transport (TDLIN-VAR-DUP), including moves — that is the
\ separate delayed-resolution piece.
s" TDLIN-TOR ( tdlin<tdown> -- tdlin<tdown> ) >r r>" CHECK-QUIET-CANDIDATE! -1 T=
\ open-arg move stays rejected (delayed-resolution piece, not this slice).
s" TDLIN-VAR-TOR ( tdlin<a> -- tdlin<a> ) >r r>" CHECK-QUIET-CANDIDATE! 0 T=
\ identity needs no transport bind: the linear-carrying layout still flows.
s" TDLIN-ID ( tdlin<tdown> -- tdlin<tdown> )" CHECK-QUIET-CANDIDATE! -1 T=
\ the same family with a non-linear arg keeps full transport.
s" TDLIN-N-DUP ( tdlin<n> -- tdlin<n> tdlin<n> ) dup" CHECK-QUIET-CANDIDATE! -1 T=
s" TDLIN-N-DROP ( tdlin<n> -- ) drop" CHECK-QUIET-CANDIDATE! -1 T=
\ an unresolved arg may later bind linear: fail closed.
s" TDLIN-VAR-DUP ( tdlin<a> -- tdlin<a> tdlin<a> ) dup" CHECK-QUIET-CANDIDATE! 0 T=
\ the bare linear itself still rejects a raw drop (baseline discipline).
s" TDLIN-BARE ( tdown -- ) drop" CHECK-QUIET-CANDIDATE! 0 T=

\ --- item 12 slice-2/3b: interpret-mode + frame-metadata boundaries (docs §17).
\ A layout value is a whole logical bundle, so introspection and frame-crossing
\ words either see it whole or fail closed — pinned here as regressions.
\ depth/.s report raw physical cells: with hidden-field expansion (slice 3b)
\ they would expose a layout row's physical shape, so they fail closed over any
\ row holding hidden fields PERMANENTLY (TFAM 12 item-5 verdict; the lift is
\ capability dot habu-logical-shape-depth-9686f5c1, docs §17).
s" TD12-DEPTH ( tdres<n,n> -- tdres<n,n> n ) depth" CHECK-QUIET-CANDIDATE! 0 T=
s" TD12-DOTS ( tdres<n,n> -- tdres<n,n> ) .s" CHECK-QUIET-CANDIDATE! 0 T=
\ constant pops one physical cell: a checked-body pop of a layout value rejects
\ PERMANENTLY (TFAM 12 verdict 2026-07-09: docs §17 sanctions reject over
\ multi-cell store; the top-level pop never sees a wide value — DNAME-WIDE
\ dispatch gate — and records the one-cell `-- a` trust for the rest).
s" TD12-CONST ( tdres<n,n> -- ) constant" CHECK-QUIET-CANDIDATE! 0 T=
s" TD12-CONST-N ( n -- ) constant" CHECK-QUIET-CANDIDATE! -1 T=
\ catch/frame words: a layout value FLOWS through a stack-preserving quotation
\ (whole-row absorption) but cannot bind a quotation's polymorphic operand.
s" TD12-CATCH ( tdres<n,n> -- tdres<n,n> n ) [: ;] catch" CHECK-QUIET-CANDIDATE! -1 T=
s" TD12-CATCH-TOUCH ( tdres<n,n> -- n ) [: drop ;] catch" CHECK-QUIET-CANDIDATE! 0 T=
s" TD12-EXEC-Q ( tdres<n,n> [ a -- a ] -- a ) execute" CHECK-QUIET-CANDIDATE! 0 T=
\ nested evaluate is rejected in checked bodies outright (unsafe boundary).
s" TD12-EVAL ( tdres<n,n> ptr u8 n -- tdres<n,n> ) evaluate" CHECK-QUIET-CANDIDATE! 0 T=
\ run-in-stack consumes its three frame args; a layout value below them flows,
\ a layout value in a consumed arg position rejects.
s" TD12-RIS ( tdres<n,n> n ptr u8 n -- tdres<n,n> ) run-in-stack" CHECK-QUIET-CANDIDATE! -1 T=
s" TD12-RIS-BAD ( n ptr u8 tdres<n,n> -- ) run-in-stack" CHECK-QUIET-CANDIDATE! 0 T=
\ throw takes an n code: a layout value is not a throw code.
s" TD12-THROW ( tdres<n,n> -- ) throw" CHECK-QUIET-CANDIDATE! 0 T=

\ --- storable layouts S1 (dot habu-checker-capability-typed-a480c423) --------
\ A width-1 (enum-tier) layout value crosses `!`/`@` through a `ptr family`
\ address; the ADDRESS type carries the family identity, and a var may bind a
\ width-1 non-linear layout pointee under a ptr spine (the typed-address seam:
\ a checked accessor certifies against a variable's `-- ptr a` row). The
\ compiled one-cell ops are the exact lowering, so certification plus the
\ executed round-trip below are the whole proof. W > 1, linear, open-arg, and
\ untyped/mismatched addresses stay fail-closed (S2 / TFAM-11 pins).
SUMTYPE tdmemu 1
  VARIANT uno ;VARIANT
  VARIANT dos ;VARIANT
;SUMTYPE
variable TDS1-MEM
s" TDS1-VP ( -- ptr tdcolor ) TDS1-MEM" CHECK-QUIET-CANDIDATE! -1 T=
s" TDS1-STORE ( tdcolor ptr tdcolor -- ) !" CHECK-QUIET-CANDIDATE! -1 T=
s" TDS1-FETCH ( ptr tdcolor -- tdcolor ) @" CHECK-QUIET-CANDIDATE! -1 T=
s" TDS1-RT ( tdcolor ptr tdcolor -- tdcolor ) tuck ! @" CHECK-QUIET-CANDIDATE! -1 T=
\ a zero-arity width-1 SUM gets the same memory tier as the TK-ENUM.
s" TDS1-SUM0 ( tdlight ptr tdlight -- ) !" CHECK-QUIET-CANDIDATE! -1 T=
\ a parametric width-1 sum qualifies once its args are closed non-linear.
s" TDS1-PAR ( tdmemu<n> ptr tdmemu<n> -- ) !" CHECK-QUIET-CANDIDATE! -1 T=
\ family identity: untyped and mismatched addresses stay rejected.
s" TDS1-BARE ( tdcolor ptr a -- ) !" CHECK-QUIET-CANDIDATE! 0 T=
s" TDS1-BAREF ( ptr a -- tdcolor ) @" CHECK-QUIET-CANDIDATE! 0 T=
s" TDS1-MIX ( tdlight ptr tdcolor -- ) !" CHECK-QUIET-CANDIDATE! 0 T=
\ no n<->enum laundering in either direction.
s" TDS1-NIN ( n ptr tdcolor -- ) !" CHECK-QUIET-CANDIDATE! 0 T=
s" TDS1-NOUT ( ptr tdcolor -- n ) @" CHECK-QUIET-CANDIDATE! 0 T=
\ W > 1 store/fetch waits for the S2 width-aware engine legs.
s" TDS1-WIDE ( tdres<n,n> ptr tdres<n,n> -- ) !" CHECK-QUIET-CANDIDATE! 0 T=
s" TDS1-WIDEF ( ptr tdres<n,n> -- tdres<n,n> ) @" CHECK-QUIET-CANDIDATE! 0 T=
\ linear / open args stay fail-closed even at width 1 (TFAM-11 rule).
s" TDS1-LIN ( tdmemu<tdown> ptr tdmemu<tdown> -- ) !" CHECK-QUIET-CANDIDATE! 0 T=
s" TDS1-OPEN ( tdmemu<a> ptr tdmemu<a> -- ) !" CHECK-QUIET-CANDIDATE! 0 T=
\ executed round-trip: store an enum, fetch it, MATCH the fetched value.
: TDS1-P ( -- ptr tdcolor ) TDS1-MEM ;
: TDS1-PUT ( tdcolor -- ) TDS1-P ! ;
: TDS1-GET ( -- tdcolor ) TDS1-P @ ;
: TDS1-CODE ( -- n )
   TDS1-GET MATCH tdcolor
     red OF 0 ENDOF
     green OF 1 ENDOF
     blue OF 2 ENDOF
   ;MATCH ;
TDCOLOR:GREEN TDS1-PUT TDS1-CODE 1 T=
TDCOLOR:BLUE TDS1-PUT TDS1-CODE 2 T=

\ --- item 12 slice-2: logical width metadata (docs §18 WIDTH function).
s" " s" tdres" TFAM-FIND-IN TDOK ! TDF !
TDOK @ -1 T=
TDF @ TFAM-WIDTH@ 2 T=
s" " s" tdlight" TFAM-FIND-IN TDOK ! TDF !
TDOK @ -1 T=
TDF @ TFAM-WIDTH@ 1 T=
s" " s" tdmix" TFAM-FIND-IN TDOK ! TDF !
TDOK @ -1 T=
TDF @ TFAM-WIDTH@ 4 T=
s" " s" tdfoo" TFAM-FIND-IN TDOK ! TDF !
TDOK @ -1 T=
TDF @ TFAM-WIDTH@ 1 T=

\ --- item 12 slice-2: per-token width facts (the emitter fact surface). One
\ row per LAYOUT operand of a transport op / locals capture: (body token index,
\ operand position 0=top, family-id, registry logical width). Absence = every
\ operand one cell. Token 0 is the definition name; body tokens start at 1.
\ The table is per-CHECK scratch, read here right after each verdict.
s" " s" tdres" TFAM-FIND-IN TDOK ! TDF !
TDOK @ -1 T=
s" " s" tdmix" TFAM-FIND-IN TDOK ! TDX !
TDOK @ -1 T=
s" WF1 ( tdres<n,n> n -- n tdres<n,n> ) swap" CHECK-QUIET-CANDIDATE! -1 T=
WF-N@ 1 T=
0 WF-TOKIX@ 1 T=
0 WF-POS@ 1 T=
0 WF-FAM@ TDF @ T=
0 WF-WIDTH@ 2 T=
\ no layout operands -> no facts.
s" WF2 ( n n -- n n ) swap" CHECK-QUIET-CANDIDATE! -1 T=
WF-N@ 0 T=
\ two layout operands at one op -> one fact per operand, top position first.
s" WF3 ( tdres<n,n> tdmix<n,n> -- tdmix<n,n> tdres<n,n> ) swap" CHECK-QUIET-CANDIDATE! -1 T=
WF-N@ 2 T=
0 WF-POS@ 0 T=
0 WF-FAM@ TDX @ T=
0 WF-WIDTH@ 4 T=
1 WF-POS@ 1 T=
1 WF-FAM@ TDF @ T=
1 WF-WIDTH@ 2 T=
\ return-stack transfers record from the row each op consumes (>r data, r> return).
s" WF4 ( tdres<n,n> -- tdres<n,n> ) >r r>" CHECK-QUIET-CANDIDATE! -1 T=
WF-N@ 2 T=
0 WF-TOKIX@ 1 T=
1 WF-TOKIX@ 2 T=
0 WF-WIDTH@ 2 T=
1 WF-WIDTH@ 2 T=
\ locals capture records the whole group at the :} token.
s" WF5 ( tdres<n,n> n -- n ) {: x y:n :} y" CHECK-QUIET-CANDIDATE! -1 T=
WF-N@ 1 T=
0 WF-TOKIX@ 4 T=
0 WF-POS@ 1 T=
0 WF-FAM@ TDF @ T=
0 WF-WIDTH@ 2 T=
\ facts are per-CHECK scratch: a rejected def may still record rows for tokens
\ past the reject (never consumed — emitters read facts only for certified
\ defs); the NEXT check resets the table, so no stale row can leak forward.
s" WF6 ( tdres<n,n> -- ) ?dup drop drop" CHECK-QUIET-CANDIDATE! 0 T=
WF-N@ 1 T=
s" WF7 ( n -- n )" CHECK-QUIET-CANDIDATE! -1 T=
WF-N@ 0 T=
\ the fact surface is checker-modeled: checked consumers (slice-3 emit helpers)
\ certify against the PRIM rows.
s" TDWF-CHK ( -- n ) WF-N@" CHECK-QUIET-CANDIDATE! -1 T=
s" TDWF-CHK2 ( n -- n ) WF-WIDTH@" CHECK-QUIET-CANDIDATE! -1 T=
s" TDWF-CHK3 ( n -- n ) TFAM-WIDTH@" CHECK-QUIET-CANDIDATE! -1 T=

\ ---------------------------------------------------------------------------
\ package-scoped declarations: family rows carry the active package and the
\ active visibility mode. Plain user packages — no reserved package is opened.
\ ---------------------------------------------------------------------------
package tdpa
public
SUMTYPE tres 1
  VARIANT yes a ;VARIANT
;SUMTYPE
private
TYPEFAMILY tpriv 1
end-package
s" tdpa" s" tres" TFAM-FIND-IN TDOK ! TDF !
TDOK @ -1 T=
TDF @ TFAM-VIS@ CHECKER-PACKAGE-PUBLIC T=
TDF @ TFAM-PKG$ s" tdpa" T$=
TDF @ TFAM-VAR-COUNT@ 1 T=
s" tdpa" s" tpriv" TFAM-FIND-IN TDOK ! TDF !
TDOK @ -1 T=
TDF @ TFAM-VIS@ CHECKER-PACKAGE-PRIVATE T=
\ same tail in a second package registers without aliasing (docs §6).
package tdpb
public
TYPEFAMILY tres 1
end-package
s" tdpb" s" tres" TFAM-FIND-IN TDOK ! TDF !
TDOK @ -1 T=
s" tdpa" s" tres" TFAM-FIND-IN TDOK ! TDX !
TDOK @ -1 T=
TDF @ TDX @ <> -1 T=

\ ---------------------------------------------------------------------------
\ item 8 metadata: a PUBLIC sum/enum family derives its constructor package name
\ (Package Shape) at ;SUMTYPE and stores it in every variant's SV.CTOR-PKG slot;
\ a PRIVATE family exports nothing, so the slot stays empty (the `construct`
\ form is item 9). No runtime constructor word is published in this item yet.
\ ---------------------------------------------------------------------------
\ top-level public `tdres` -> package TDRES on both variants.
s" " s" tdres" TFAM-FIND-IN TDOK ! TDF !   TDOK @ -1 T=
TDF @ TFAM-VAR-START@ TDV0 !
TDV0 @ SUMV-CTOR-PKG$ s" TDRES" T$=
TDV0 @ 1 + SUMV-CTOR-PKG$ s" TDRES" T$=
\ in-package public `tdpa:tres` -> package TDPA-TRES.
s" tdpa" s" tres" TFAM-FIND-IN TDOK ! TDF !   TDOK @ -1 T=
TDF @ TFAM-VAR-START@ TDV0 !
TDV0 @ SUMV-CTOR-PKG$ s" TDPA-TRES" T$=
\ a private sum exports no constructor package: SV.CTOR-PKG stays empty.
package tdp8
private
SUMTYPE tsec 1
  VARIANT hidden a ;VARIANT
;SUMTYPE
end-package
s" tdp8" s" tsec" TFAM-FIND-IN TDOK ! TDF !   TDOK @ -1 T=
TDF @ TFAM-VIS@ CHECKER-PACKAGE-PRIVATE T=
TDF @ TFAM-VAR-START@ TDV0 !
TDV0 @ SUMV-CTOR-PKG$ nip 0 T=

\ ---------------------------------------------------------------------------
\ package-scoped SIG resolution (habu-tfam-4-remainder part 3): unqualified
\ family tokens resolve through the ACTIVE package before the unique public
\ tail; qualified PKG:tail folds the qualifier, requires a lowercase tail, and
\ resolves public rows only (plus the active package's own private rows).
\ ---------------------------------------------------------------------------
package tdpa
s" TPOK-LOCAL ( tres<n> -- tres<n> )" CHECK-QUIET-CANDIDATE! -1 T=
s" TPOK-PRIV ( tpriv<n> -- tpriv<n> )" CHECK-QUIET-CANDIDATE! -1 T=
s" TPOK-QSELF ( tdpa:tpriv<n> -- tdpa:tpriv<n> )" CHECK-QUIET-CANDIDATE! -1 T=
end-package
\ cross-package: a private family never resolves, qualified or not...
s" TXBAD-PRIV ( tpriv<n> -- ) drop" CHECK-QUIET-CANDIDATE! 0 T=
s" TXBAD-QPRIV ( tdpa:tpriv<n> -- ) drop" CHECK-QUIET-CANDIDATE! 0 T=
\ ...two public same-tail families are ambiguous unqualified...
s" TXBAD-AMBIG ( tres<n> -- ) drop" CHECK-QUIET-CANDIDATE! 0 T=
\ ...and qualified references resolve each package distinctly.
s" TQOK-A ( tdpa:tres<n> -- tdpa:tres<n> )" CHECK-QUIET-CANDIDATE! -1 T=
s" TQOK-B ( tdpb:tres<n> -- tdpb:tres<n> )" CHECK-QUIET-CANDIDATE! -1 T=
s" TQOK-FOLD ( TDPA:tres<n> -- tdpa:tres<n> )" CHECK-QUIET-CANDIDATE! -1 T=
s" TQBAD-CASE ( TDPA:Tres<n> -- ) drop" CHECK-QUIET-CANDIDATE! 0 T=
\ same tail, different package: no unification at the SIG-parse level.
s" TQBAD-XUNIFY ( tdpa:tres<n> -- tdpb:tres<n> )" CHECK-QUIET-CANDIDATE! 0 T=
s" TQBAD-XUNIFY2 ( tdpb:tres<n> -- tdpa:tres<n> )" CHECK-QUIET-CANDIDATE! 0 T=
\ inside tdpb, unqualified tres is tdpb's own — never tdpa's same tail.
package tdpb
s" TPOK-OWN ( tres<n> -- tdpb:tres<n> )" CHECK-QUIET-CANDIDATE! -1 T=
s" TPBAD-OTHER ( tres<n> -- tdpa:tres<n> ) " CHECK-QUIET-CANDIDATE! 0 T=
end-package
\ unknown qualifiers and malformed multi-colon tokens reject as unknown types.
s" TQBAD-NOPKG ( nopkg:tres<n> -- ) drop" CHECK-QUIET-CANDIDATE! 0 T=
s" TQBAD-COLONS ( tdpa:tres:x<n> -- ) drop" CHECK-QUIET-CANDIDATE! 0 T=
\ hidden physical names never resolve in public signatures.
s" TQBAD-HIDDEN ( @tdres.tag<n,n> -- ) drop" CHECK-QUIET-CANDIDATE! 0 T=

\ ---------------------------------------------------------------------------
\ negative declarations: named throw code + full registry rollback each time.
\ ---------------------------------------------------------------------------
\ uppercase/mixed-case family names reject before storage.
s" TYPEFAMILY Bad 1" E-TFAM-CASE TDT-NEG
s" TYPEFAMILY tdBAD 1" E-TFAM-CASE TDT-NEG
s" SUMTYPE Res 1 VARIANT ok a ;VARIANT ;SUMTYPE" E-TFAM-CASE TDT-NEG
\ qualified names are illegal in declaration position.
s" TYPEFAMILY pkg:tail 1" E-TFAM-CASE TDT-NEG
\ hyphen edges / doubled hyphens are not canonical tails.
s" TYPEFAMILY td--x 1" E-TFAM-CASE TDT-NEG
s" TYPEFAMILY -tdx 1" E-TFAM-CASE TDT-NEG
\ injection-shaped text is not a canonical name token.
s\" TYPEFAMILY s\" 1" E-TFAM-CASE TDT-NEG
\ reserved signature/type tokens as family names.
s" TYPEFAMILY a 0" E-TDECL-NAME TDT-NEG
s" SUMTYPE n 0 VARIANT x ;VARIANT ;SUMTYPE" E-TDECL-NAME TDT-NEG
s" TYPEFAMILY field 1" E-TDECL-NAME TDT-NEG
s" TYPEFAMILY str 1" E-TDECL-NAME TDT-NEG
s" TYPEFAMILY space-x 1" E-TDECL-NAME TDT-NEG
s" TYPEFAMILY fresh-mask-x 1" E-TDECL-NAME TDT-NEG
\ item 9 reserved token protocol: construct/match/;match may not name a
\ family or a variant (;match already fails the canonical-tail gate).
s" TYPEFAMILY construct 1" E-TDECL-NAME TDT-NEG
s" SUMTYPE match 0 VARIANT x n ;VARIANT ;SUMTYPE" E-TDECL-NAME TDT-NEG
s" SUMTYPE tdcn 1 VARIANT construct a ;VARIANT ;SUMTYPE" E-TDECL-NAME TDT-NEG
s" SUMTYPE tdcn2 1 VARIANT match a ;VARIANT ;SUMTYPE" E-TDECL-NAME TDT-NEG
s" SUMTYPE tdcn3 1 VARIANT ;match a ;VARIANT ;SUMTYPE" E-TFAM-CASE TDT-NEG
\ redeclaring a global family at top level is a same-scope duplicate (the
\ top-level declaring scope IS the global scope, so the collision is a real
\ duplicate — E-TFAM-DUP, not a reserved-name shadow; both classes reject)...
s" TYPEFAMILY ptr 0" E-TFAM-DUP TDT-NEG
s" TYPEFAMILY span 3" E-TFAM-DUP TDT-NEG
\ ...while shadowing a global family from inside a package is reserved.
package tshad
s" TYPEFAMILY span 3" E-TDECL-NAME TDT-NEG
end-package
\ variant names may not collide with any family the declaring scope resolves,
\ in ANY scope: builtin tails, prior user families, and (inside a package)
\ the package's own tails all reject; the verdict matches across scopes.
s" SUMTYPE tdvres 1 VARIANT span a ;VARIANT ;SUMTYPE" E-TDECL-NAME TDT-NEG
TYPEFAMILY tduserfam 1
s" SUMTYPE tdvres2 1 VARIANT tduserfam a ;VARIANT ;SUMTYPE" E-TDECL-NAME TDT-NEG
package tvshad
s" SUMTYPE tdvres3 1 VARIANT span a ;VARIANT ;SUMTYPE" E-TDECL-NAME TDT-NEG
TYPEFAMILY tvloc 1
s" SUMTYPE tdvres4 1 VARIANT tvloc a ;VARIANT ;SUMTYPE" E-TDECL-NAME TDT-NEG
end-package
\ ...and an unreserved variant name in the same shape still accepts.
SUMTYPE tdvok 1
  VARIANT fine a ;VARIANT
;SUMTYPE
s" " s" tdvok" TFAM-FIND-IN TDOK ! TDF !
TDOK @ -1 T=
TDF @ TFAM-VAR-COUNT@ 1 T=
TDF @ TFAM-VAR-START@ SUMV-NAME$ s" fine" T$=
\ control words and grammar keywords are not names.
s" TYPEFAMILY if 1" E-TDECL-NAME TDT-NEG
s" TYPEFAMILY repeat 1" E-TDECL-NAME TDT-NEG
s" SUMTYPE variant 1 VARIANT ok a ;VARIANT ;SUMTYPE" E-TDECL-NAME TDT-NEG
\ value-record names collide.
VALUE-RECORD tdvrec x n END-VALUE-RECORD
s" TYPEFAMILY tdvrec 1" E-TDECL-NAME TDT-NEG
\ duplicate family (same package scope).
s" TYPEFAMILY tdfoo 2" E-TFAM-DUP TDT-NEG
\ bad arity tokens.
s" TYPEFAMILY tdnoar" E-TDECL-ARITY TDT-NEG
s" TYPEFAMILY tdarx x" E-TDECL-ARITY TDT-NEG
s" TYPEFAMILY tdarneg -1" E-TDECL-ARITY TDT-NEG
s" TYPEFAMILY tdarbig 27" E-TDECL-ARITY TDT-NEG
s" SUMTYPE tdarv VARIANT ok a ;VARIANT ;SUMTYPE" E-TDECL-ARITY TDT-NEG
\ malformed sum bodies.
s" SUMTYPE tdempty 1 ;SUMTYPE" E-TDECL-SYNTAX TDT-NEG
s" SUMTYPE tdnoterm 1 VARIANT ok a ;VARIANT" E-TDECL-SYNTAX TDT-NEG
s" SUMTYPE tdstray 1 stray VARIANT ok a ;VARIANT ;SUMTYPE" E-TDECL-SYNTAX TDT-NEG
s" SUMTYPE tdnovn 1 VARIANT ;VARIANT ;SUMTYPE" E-TFAM-CASE TDT-NEG
s" SUMTYPE tdkw 1 VARIANT variant a ;VARIANT ;SUMTYPE" E-TDECL-NAME TDT-NEG
s" SUMTYPE tdvcase 1 VARIANT Ok a ;VARIANT ;SUMTYPE" E-TFAM-CASE TDT-NEG
s" SUMTYPE tdvdup 1 VARIANT ok a ;VARIANT VARIANT ok a ;VARIANT ;SUMTYPE" E-TFAM-DUP TDT-NEG
s" SUMTYPE tdbadptr 1 VARIANT ok ptr ;VARIANT ;SUMTYPE" E-TDECL-SYNTAX TDT-NEG
\ unknown payload types (incl. out-of-arity letters and family applications,
\ which stay rejected until schema instantiation work in items 7/8).
s" SUMTYPE tdpay1 1 VARIANT ok q ;VARIANT ;SUMTYPE" E-TDECL-PAYLOAD TDT-NEG
s" SUMTYPE tdpay2 1 VARIANT ok whatnot ;VARIANT ;SUMTYPE" E-TDECL-PAYLOAD TDT-NEG
s" SUMTYPE tdpay3 1 VARIANT ok tdres<a,a> ;VARIANT ;SUMTYPE" E-TDECL-PAYLOAD TDT-NEG

\ malformed enum declarations (item 14): every reject rolls back to baseline via
\ the shared transactional path (TDT-NEG asserts TDT-BASE=), so no family or
\ variant row survives.
\ empty / unterminated bodies.
s" ENUM tdeempty ;ENUM" E-TDECL-SYNTAX TDT-NEG
s" ENUM tdenoterm red green" E-TDECL-SYNTAX TDT-NEG
\ duplicate variant within one enum.
s" ENUM tdedup red red ;ENUM" E-TFAM-DUP TDT-NEG
\ bad variant names: uppercase, reserved single-letter, grammar keyword, and a
\ name that collides with an existing type family.
s" ENUM tdecase Red ;ENUM" E-TFAM-CASE TDT-NEG
s" ENUM tdesl a ;ENUM" E-TDECL-NAME TDT-NEG
s" ENUM tdekw enum ;ENUM" E-TDECL-NAME TDT-NEG
s" ENUM tdekw2 variant ;ENUM" E-TDECL-NAME TDT-NEG
s" ENUM tdevf tdfoo ;ENUM" E-TDECL-NAME TDT-NEG
\ bad family names: uppercase, reserved single-letter, grammar keyword.
s" ENUM Bad red ;ENUM" E-TFAM-CASE TDT-NEG
s" ENUM a red ;ENUM" E-TDECL-NAME TDT-NEG
s" ENUM sumtype red ;ENUM" E-TDECL-NAME TDT-NEG
\ redeclaring an existing family tail (enum over enum, and enum over a sum).
s" ENUM tdcolor red ;ENUM" E-TFAM-DUP TDT-NEG
s" ENUM tdres red ;ENUM" E-TFAM-DUP TDT-NEG

\ malformed product declarations (item 15): every reject rolls back to baseline
\ via the shared transactional path (TDT-NEG asserts TDT-BASE=, incl. PF-N), so
\ no family, field, or schema row survives a failed product.
\ empty / unterminated bodies.
s" PRODUCT tdpempty 0 ;PRODUCT" E-TDECL-SYNTAX TDT-NEG
s" PRODUCT tdpnoterm 2 FIELD fst a FIELD snd b" E-TDECL-SYNTAX TDT-NEG
\ duplicate field within one product (PF-ADD dup-reject).
s" PRODUCT tdpdup 1 FIELD x a FIELD x a ;PRODUCT" E-TFAM-DUP TDT-NEG
\ unknown / out-of-arity field types, and a dangling ptr.
s" PRODUCT tdpbad 1 FIELD x q ;PRODUCT" E-TDECL-PAYLOAD TDT-NEG
s" PRODUCT tdpoor 0 FIELD x a ;PRODUCT" E-TDECL-PAYLOAD TDT-NEG
s" PRODUCT tdpptr 1 FIELD x ptr ;PRODUCT" E-TDECL-SYNTAX TDT-NEG
\ bad field names: uppercase, grammar keyword.
s" PRODUCT tdpfc 1 FIELD X a ;PRODUCT" E-TFAM-CASE TDT-NEG
s" PRODUCT tdpfk 1 FIELD field a ;PRODUCT" E-TDECL-NAME TDT-NEG
\ a stray token where FIELD is expected.
s" PRODUCT tdpstray 1 stray FIELD x a ;PRODUCT" E-TDECL-SYNTAX TDT-NEG
\ missing arity token.
s" PRODUCT tdpna FIELD x a ;PRODUCT" E-TDECL-ARITY TDT-NEG
\ bad family names: uppercase, reserved single-letter, grammar keyword, and the
\ product/field grammar tokens themselves (reserved case-folded).
s" PRODUCT Bad 1 FIELD x a ;PRODUCT" E-TFAM-CASE TDT-NEG
s" PRODUCT a 0 FIELD x n ;PRODUCT" E-TDECL-NAME TDT-NEG
s" PRODUCT sumtype 0 FIELD x n ;PRODUCT" E-TDECL-NAME TDT-NEG
s" PRODUCT product 0 FIELD x n ;PRODUCT" E-TDECL-NAME TDT-NEG
s" PRODUCT field 0 FIELD x n ;PRODUCT" E-TDECL-NAME TDT-NEG
\ redeclaring an existing family tail (product over product, product over sum).
s" PRODUCT tdpair 2 FIELD a a FIELD b b ;PRODUCT" E-TFAM-DUP TDT-NEG
s" PRODUCT tdres 1 FIELD x a ;PRODUCT" E-TFAM-DUP TDT-NEG
\ product / field are reserved as variant names too (case-folded dictionary).
s" SUMTYPE tdpv 1 VARIANT product a ;VARIANT ;SUMTYPE" E-TDECL-NAME TDT-NEG
s" SUMTYPE tdpv2 1 VARIANT field a ;VARIANT ;SUMTYPE" E-TDECL-NAME TDT-NEG
s" TYPEFAMILY product 1" E-TDECL-NAME TDT-NEG

\ ---------------------------------------------------------------------------
\ declaration diagnostic packet: declaration-shaped, through the standard
\ machinery — no fake declared stack effect, definition fields, or word row.
\ ---------------------------------------------------------------------------
DIAG-BUFFER-OFF
TDIAG-BUF 8192 DIAG-BUFFER!  -1 DIAG-JSON!
s" TYPEFAMILY tdfoo 2" E-TFAM-DUP TDT-NEG
DIAG-BUFFER$ s\" \"code\":\"E-BAD-DECLARATION\"" TDT-CONTAINS? -1 T=
DIAG-BUFFER$ s\" \"repair_class\":\"fix_family_declaration\"" TDT-CONTAINS? -1 T=
DIAG-BUFFER$ s\" \"verdict\":\"rejected\"" TDT-CONTAINS? -1 T=
DIAG-BUFFER$ s\" \"decl\":\"typefamily\"" TDT-CONTAINS? -1 T=
DIAG-BUFFER$ s\" \"family\":\"tdfoo\"" TDT-CONTAINS? -1 T=
DIAG-BUFFER$ s\" \"reason\":\"duplicate family\"" TDT-CONTAINS? -1 T=
DIAG-BUFFER$ s\" \"declared_effect\"" TDT-CONTAINS? 0 T=
DIAG-BUFFER$ s\" \"definition_source\"" TDT-CONTAINS? 0 T=
DIAG-BUFFER$ s\" \"return_stack\"" TDT-CONTAINS? 0 T=
DIAG-BUFFER-OFF  0 DIAG-JSON!
\ prose form names the declaration kind and reason.
TDIAG-BUF 8192 DIAG-BUFFER!
s" SUMTYPE tdvd2 1 VARIANT ok a ;VARIANT VARIANT ok a ;VARIANT ;SUMTYPE" E-TFAM-DUP TDT-NEG
DIAG-BUFFER$ s" bad sumtype declaration" TDT-CONTAINS? -1 T=
DIAG-BUFFER$ s" duplicate variant" TDT-CONTAINS? -1 T=
\ the enum kind token flows into the same declaration-shaped prose packet.
s" ENUM tdediag red red ;ENUM" E-TFAM-DUP TDT-NEG
DIAG-BUFFER$ s" bad enum declaration" TDT-CONTAINS? -1 T=
DIAG-BUFFER$ s" duplicate variant" TDT-CONTAINS? -1 T=

\ ---------------------------------------------------------------------------
\ multi-error load mode: a bad top-level declaration is reported + counted +
\ rolled back, without a fake declared signature, and the load continues.
\ ---------------------------------------------------------------------------
MULTI-ERR-BEGIN
s" SUMTYPE tdme 2 VARIANT ok a ;VARIANT VARIANT ok b ;VARIANT ;SUMTYPE TYPEFAMILY tdcont 1 : TDMEW ( n -- n ) ;" evaluate
MULTI-ERR-END 1 T=
s" " s" tdme" TFAM-FIND-IN TDOK ! drop
TDOK @ 0 T=
s" " s" tdcont" TFAM-FIND-IN TDOK ! drop
TDOK @ -1 T=
s" TDMEW" CHECKER-FIND-USIG -1 T=
\ missing terminator in multi-error mode: reported, counted, load continues.
MULTI-ERR-BEGIN
s" SUMTYPE tdnoe 1 VARIANT ok a ;VARIANT" evaluate
MULTI-ERR-END 1 T=
s" " s" tdnoe" TFAM-FIND-IN TDOK ! drop
TDOK @ 0 T=
\ two bad declarations count separately.
MULTI-ERR-BEGIN
s" TYPEFAMILY Bad1 1 TYPEFAMILY tdok9 1 SUMTYPE tdes 1 ;SUMTYPE" evaluate
MULTI-ERR-END 2 T=
s" " s" tdok9" TFAM-FIND-IN TDOK ! drop
TDOK @ -1 T=
\ a bad declaration does not poison later checks after the mode ends.
s" TDOK-AFTER ( tdfoo<n,n> -- tdfoo<n,n> )" CHECK-QUIET-CANDIDATE! -1 T=
\ unknown-family and wrong-arity SIGNATURES in multi-error mode: reported,
\ counted, and the load continues — but the invalid declared signature must
\ NOT be stored as a cert row (later checks stay sound).
MULTI-ERR-BEGIN
s" : TDSME1 ( nope<n> -- nope<n> ) ; : TDSME2 ( tdfoo<n> -- tdfoo<n> ) ; : TDSME3 ( n -- n ) ;" evaluate
MULTI-ERR-END 2 T=
s" TDSME1" CHECKER-FIND-USIG 0 T=
s" TDSME2" CHECKER-FIND-USIG 0 T=
s" TDSME3" CHECKER-FIND-USIG -1 T=
\ a raw TRUST row with an unparseable signature: counted + reported, no row.
MULTI-ERR-BEGIN
s\" s\" TDTBAD\" s\" nope<n> -- n\" TRUST : TDTOK ( n -- n ) ;" evaluate
MULTI-ERR-END 1 T=
s" TDTBAD" CHECKER-FIND-USIG 0 T=
s" TDTOK" CHECKER-FIND-USIG -1 T=
DIAG-BUFFER-OFF

\ ---------------------------------------------------------------------------
\ item 12 slice-3a: hidden-field substrate (inert). Drives the new checker
\ substrate at TOP-LEVEL interpret (registry words resolve here; NEW never runs,
\ so the terms built below survive across every assert). No CHECK runs after the
\ first term is built. LAYOUT-PUSH-FIELDS is NOT wired into PUSH-LOGICAL yet, so
\ every check above this section already proved user-visible behavior unchanged.
\ ---------------------------------------------------------------------------
variable TD3F    variable TD3M    variable TD3OK
variable TD3LOG  variable TD3MLOG
variable TD3H0   variable TD3H1
variable TD3ROW  variable TD3CUR

\ resolve the tdres (width 2) and tdmix (width 4) families declared above.
s" " s" tdres" TFAM-FIND-IN TD3OK ! TD3F !
TD3OK @ -1 T=
s" " s" tdmix" TFAM-FIND-IN TD3OK ! TD3M !
TD3OK @ -1 T=

\ build a LOGICAL tdres<n,n> term via the same MK-PARAM path SIG parsing drives.
PARAM-SCR-N @
CC-N MK-CON PARAM-SCR+
CC-N MK-CON PARAM-SCR+
s" tdres" TD3F @ MK-PARAM  TD3LOG !
\ a logical layout term is NOT hidden.
TD3LOG @ HIDDEN-PARAM? 0 T=
TD3LOG @ PARAM>HID 0 T=
TD3LOG @ PARAM>FAM TD3F @ T=

\ mint hidden fields for slot 0 (payload) and slot 1 (tag = W-1).
TD3LOG @ 0 MK-HIDDEN TD3H0 !
TD3LOG @ 1 MK-HIDDEN TD3H1 !
TD3H0 @ HIDDEN-PARAM? -1 T=
TD3H1 @ HIDDEN-PARAM? -1 T=
TD3H0 @ HIDDEN-SLOT@ 0 T=
TD3H1 @ HIDDEN-SLOT@ 1 T=
TD3H0 @ PARAM>HID 1 T=          \ slot+1 encoding
TD3H1 @ PARAM>HID 2 T=
TD3H0 @ PARAM>FAM TD3F @ T=
TD3H1 @ PARAM>FAM TD3F @ T=

\ LAYOUT-PUSH-FIELDS on an empty fresh row pushes exactly W=2 cells, tag on top,
\ slot0 deepest (docs §5). Walk top-down: W-1 (tag), then 0, then the base var.
FRESH MK-ROW  TD3ROW !
TD3LOG @ TD3ROW @ LAYOUT-PUSH-FIELDS  TD3CUR !
TD3CUR @ R-RES TAG S-PUSH T=                                  \ top cell present
TD3CUR @ R-RES P>TYPE HIDDEN-SLOT@ 1 T=                       \ ...is the tag (slot W-1)
TD3CUR @ R-RES P>REST R-RES TAG S-PUSH T=                     \ next cell present
TD3CUR @ R-RES P>REST R-RES P>TYPE HIDDEN-SLOT@ 0 T=          \ ...is slot0
TD3CUR @ R-RES P>REST R-RES P>REST R-RES TAG S-ROW T=         \ then the base row var — exactly W cells added

\ unification discipline (UNIFY ( t t -- bool ), self-contained per call).
\ same family + same slot -> pair.
TD3LOG @ 0 MK-HIDDEN  TD3LOG @ 0 MK-HIDDEN  UNIFY -1 T=
\ same family, different slot -> reject.
TD3LOG @ 0 MK-HIDDEN  TD3LOG @ 1 MK-HIDDEN  UNIFY 0 T=
\ hidden never binds a var, even under whole-bundle transport mode.
TD3LOG @ 0 MK-HIDDEN  FRESH MK-VAR  UNIFY 0 T=
1 LAYOUT-XPORT !
TD3LOG @ 0 MK-HIDDEN  FRESH MK-VAR  UNIFY 0 T=
0 LAYOUT-XPORT !
\ hidden never unifies a con.
TD3LOG @ 0 MK-HIDDEN  CC-N MK-CON  UNIFY 0 T=
\ a hidden field never unifies its own logical value.
TD3LOG @ 0 MK-HIDDEN  TD3LOG @  UNIFY 0 T=

\ cross-family: a same-slot hidden field of a DIFFERENT family rejects.
PARAM-SCR-N @
CC-N MK-CON PARAM-SCR+
CC-N MK-CON PARAM-SCR+
s" tdmix" TD3M @ MK-PARAM  TD3MLOG !
TD3MLOG @ 0 MK-HIDDEN  TD3LOG @ 0 MK-HIDDEN  UNIFY 0 T=

\ ---------------------------------------------------------------------------
\ report: "ok" on success, nonzero exit on any failure.
\ ---------------------------------------------------------------------------
: REPORT ( -- )
   #FAIL @ 0 = if s" ok" type cr exit then
   #FAIL @ . s" type-decl-suite: failures" 1 die ;
REPORT
