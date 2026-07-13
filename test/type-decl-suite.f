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
\ whitebox boundary (dot habu-hb-crash-bare-c5be6634): checker-internal colon
\ words probed at top level go through named trusted shims.
TRUSTED: TWX-CHECKER-FIND-USIG ( ptr u8 n -- bool ) CHECKER-FIND-USIG ;
TRUSTED: TWX-FRESH ( -- n ) FRESH ;
TRUSTED: TWX-HIDDEN-PARAM? ( n -- bool ) HIDDEN-PARAM? ;
TRUSTED: TWX-HIDDEN-SLOT@ ( n -- n ) HIDDEN-SLOT@ ;
TRUSTED: TWX-LAYOUT-PUSH-FIELDS ( n n -- n ) LAYOUT-PUSH-FIELDS ;
TRUSTED: TWX-MK-HIDDEN ( n n -- n ) MK-HIDDEN ;
TRUSTED: TWX-TAG ( n -- n ) TAG ;
TRUSTED: TWX-MK-CON ( n -- n ) MK-CON ;
TRUSTED: TWX-MK-VAR ( n -- n ) MK-VAR ;
TRUSTED: TWX-MK-ROW ( n -- n ) MK-ROW ;
TRUSTED: TWX-MK-PARAM ( n ptr u8 n n -- n ) MK-PARAM ;
TRUSTED: TWX-P>TYPE ( n -- n ) P>TYPE ;
TRUSTED: TWX-P>REST ( n -- n ) P>REST ;
TRUSTED: TWX-CON-OF ( ptr u8 n -- n ) CON-OF ;
TRUSTED: TWX-MULTI-ERR-BEGIN ( -- ) MULTI-ERR-BEGIN ;
TRUSTED: TWX-MULTI-ERR-END ( -- n ) MULTI-ERR-END ;
TRUSTED: TWX-NEW ( -- ) NEW ;
TRUSTED: TWX-PAIR ( n n -- ) PAIR ;
TRUSTED: TWX-PARAM-SCR+ ( n -- ) PARAM-SCR+ ;
TRUSTED: TWX-PARAM>FAM ( n -- n ) PARAM>FAM ;
TRUSTED: TWX-PARAM>HID ( n -- n ) PARAM>HID ;
TRUSTED: TWX-PF-FAM@ ( n -- n ) PF-FAM@ ;
TRUSTED: TWX-PF-FIND ( n ptr u8 n -- n bool ) PF-FIND ;
TRUSTED: TWX-PF-NAME$ ( n -- ptr u8 n ) PF-NAME$ ;
TRUSTED: TWX-PF-SCH@ ( n -- n ) PF-SCH@ ;
TRUSTED: TWX-PF-SLOT@ ( n -- n ) PF-SLOT@ ;
TRUSTED: TWX-PUSH-LOGICAL ( n n -- n ) PUSH-LOGICAL ;
TRUSTED: TWX-R-RES ( n -- n ) R-RES ;
TRUSTED: TWX-SCHEMA-A@ ( n -- n ) SCHEMA-A@ ;
TRUSTED: TWX-SCHEMA-APP? ( n -- bool ) SCHEMA-APP? ;
TRUSTED: TWX-SCHEMA-CON? ( n -- bool ) SCHEMA-CON? ;
TRUSTED: TWX-SCHEMA-PARAM? ( n -- bool ) SCHEMA-PARAM? ;
TRUSTED: TWX-SCHEMA-PTR? ( n -- bool ) SCHEMA-PTR? ;
TRUSTED: TWX-SCHEMA-ROOT@ ( n -- n ) SCHEMA-ROOT@ ;
TRUSTED: TWX-SUMV-FAM@ ( n -- n ) SUMV-FAM@ ;
TRUSTED: TWX-SUMV-PAYCELLS@ ( n -- n ) SUMV-PAYCELLS@ ;
TRUSTED: TWX-SUMV-SCH-COUNT@ ( n -- n ) SUMV-SCH-COUNT@ ;
TRUSTED: TWX-SUMV-SCH-START@ ( n -- n ) SUMV-SCH-START@ ;
TRUSTED: TWX-SUMV-TAG@ ( n -- n ) SUMV-TAG@ ;
TRUSTED: TWX-TDECL-POLICY ( n -- ) TDECL-POLICY ;
TRUSTED: TWX-TDECL-THROW ( ptr u8 n ptr u8 n n -- ) TDECL-THROW ;
TRUSTED: TWX-TFAM-CELL? ( n -- bool ) TFAM-CELL? ;
TRUSTED: TWX-TFAM-DECL ( ptr u8 n n ptr u8 n n n -- n ) TFAM-DECL ;
TRUSTED: TWX-TFAM-ENUM? ( n -- bool ) TFAM-ENUM? ;
TRUSTED: TWX-TFAM-FIND-IN ( ptr u8 n ptr u8 n -- n bool ) TFAM-FIND-IN ;
TRUSTED: TWX-TFAM-FLD-COUNT@ ( n -- n ) TFAM-FLD-COUNT@ ;
TRUSTED: TWX-TFAM-FLD-START@ ( n -- n ) TFAM-FLD-START@ ;
TRUSTED: TWX-TFAM-LAYOUT-POLICY@ ( n -- n ) TFAM-LAYOUT-POLICY@ ;
TRUSTED: TWX-TFAM-LAYOUT? ( n -- bool ) TFAM-LAYOUT? ;
TRUSTED: TWX-TFAM-PKG$ ( n -- ptr u8 n ) TFAM-PKG$ ;
TRUSTED: TWX-TFAM-PRODUCT? ( n -- bool ) TFAM-PRODUCT? ;
TRUSTED: TWX-TFAM-SLOTS@ ( n -- n ) TFAM-SLOTS@ ;
TRUSTED: TWX-TFAM-SUM? ( n -- bool ) TFAM-SUM? ;
TRUSTED: TWX-TFAM-VIS@ ( n -- n ) TFAM-VIS@ ;
TRUSTED: TWX-UNIFY ( n n -- bool ) UNIFY ;

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
s" " s" tdfoo" TWX-TFAM-FIND-IN TDOK ! TDF !
TDOK @ -1 T=
TDF @ TFAM-ARITY@ 2 T=
TDF @ TFAM-KIND@ TK-CELL T=
TDF @ TWX-TFAM-CELL? -1 T=
TDF @ TWX-TFAM-VIS@ CHECKER-PACKAGE-PUBLIC T=
TDF @ TWX-TFAM-PKG$ s" " T$=
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
s" " s" tdres" TWX-TFAM-FIND-IN TDOK ! TDF !
TDOK @ -1 T=
TDF @ TFAM-KIND@ TK-SUM T=
TDF @ TWX-TFAM-SUM? -1 T=
TDF @ TFAM-ARITY@ 2 T=
\ variant range wired at ;SUMTYPE (census contradiction C6 regression):
TDF @ TFAM-VAR-COUNT@ 2 T=
TDF @ TFAM-VAR-START@ TDV0 !
TDV0 @ TWX-SUMV-FAM@ TDF @ T=
TDV0 @ SUMV-NAME$ s" ok" T$=
TDV0 @ TWX-SUMV-TAG@ 0 T=
TDV0 @ 1 + SUMV-NAME$ s" err" T$=
TDV0 @ 1 + TWX-SUMV-TAG@ 1 T=
TDF @ TWX-TFAM-SLOTS@ 1 T=
\ payload schemas: ok = paramref 0, err = paramref 1, one cell each.
TDV0 @ TWX-SUMV-SCH-COUNT@ 1 T=
TDV0 @ TWX-SUMV-PAYCELLS@ 1 T=
TDV0 @ TWX-SUMV-SCH-START@ TWX-SCHEMA-ROOT@ TWX-SCHEMA-PARAM? -1 T=
TDV0 @ TWX-SUMV-SCH-START@ TWX-SCHEMA-ROOT@ TWX-SCHEMA-A@ 0 T=
TDV0 @ 1 + TWX-SUMV-SCH-START@ TWX-SCHEMA-ROOT@ TWX-SCHEMA-A@ 1 T=
\ the sum family name is usable in signatures as a logical type expression.
s" TDOK-RES ( tdres<n,n> -- tdres<n,n> )" CHECK-QUIET-CANDIDATE! -1 T=
s" TDBAD-RES1 ( tdres<n> -- ) drop" CHECK-QUIET-CANDIDATE! 0 T=

\ zero-payload variant: padding-only, paycells 0 (docs §4 option shape).
SUMTYPE tdopt 1
  VARIANT none   ;VARIANT
  VARIANT some a ;VARIANT
;SUMTYPE
s" " s" tdopt" TWX-TFAM-FIND-IN TDOK ! TDF !
TDOK @ -1 T=
TDF @ TFAM-VAR-COUNT@ 2 T=
TDF @ TWX-TFAM-SLOTS@ 1 T=
TDF @ TFAM-VAR-START@ TWX-SUMV-PAYCELLS@ 0 T=
TDF @ TFAM-VAR-START@ TWX-SUMV-SCH-COUNT@ 0 T=

\ multi-cell concrete payload (docs §8 parse-result): ptr u8 + n schemas.
SUMTYPE tdparse 1
  VARIANT yes a ;VARIANT
  VARIANT no  ptr u8 n ;VARIANT
;SUMTYPE
s" " s" tdparse" TWX-TFAM-FIND-IN TDOK ! TDF !
TDOK @ -1 T=
TDF @ TWX-TFAM-SLOTS@ 2 T=
TDF @ TFAM-VAR-START@ 1 + TWX-SUMV-PAYCELLS@ 2 T=
TDF @ TFAM-VAR-START@ 1 + TWX-SUMV-SCH-START@ TWX-SCHEMA-ROOT@ TDX !
TDX @ TWX-SCHEMA-PTR? -1 T=
TDX @ TWX-SCHEMA-A@ TWX-SCHEMA-CON? -1 T=
TDX @ TWX-SCHEMA-A@ TWX-SCHEMA-A@ s" u8" TWX-CON-OF T=
TDF @ TFAM-VAR-START@ 1 + TWX-SUMV-SCH-START@ 1 + TWX-SCHEMA-ROOT@ TDY !
TDY @ TWX-SCHEMA-CON? -1 T=
TDY @ TWX-SCHEMA-A@ CC-N T=

\ zero-arity sum (payload-free variants only): the enum-shaped sum.
SUMTYPE tdlight 0
  VARIANT red   ;VARIANT
  VARIANT green ;VARIANT
  VARIANT blue  ;VARIANT
;SUMTYPE
s" " s" tdlight" TWX-TFAM-FIND-IN TDOK ! TDF !
TDOK @ -1 T=
TDF @ TFAM-VAR-COUNT@ 3 T=
TDF @ TWX-TFAM-SLOTS@ 0 T=
TDF @ TFAM-VAR-START@ 2 + TWX-SUMV-TAG@ 2 T=

\ arity above the old 4-arg cap parses through growable schema storage.
SUMTYPE tdwide 8
  VARIANT lo a ;VARIANT
  VARIANT hi h ;VARIANT
;SUMTYPE
s" " s" tdwide" TWX-TFAM-FIND-IN TDOK ! TDF !
TDOK @ -1 T=
TDF @ TFAM-ARITY@ 8 T=
TDF @ TFAM-VAR-START@ 1 + TWX-SUMV-SCH-START@ TWX-SCHEMA-ROOT@ TWX-SCHEMA-A@ 7 T=
s" TDOK-WIDE ( tdwide<n,n,n,n,n,n,n,n> -- tdwide<n,n,n,n,n,n,n,n> )" CHECK-QUIET-CANDIDATE! -1 T=

\ mixed payload widths: slots = max across variants.
SUMTYPE tdmix 2
  VARIANT small a ;VARIANT
  VARIANT big a b n ;VARIANT
;SUMTYPE
s" " s" tdmix" TWX-TFAM-FIND-IN TDOK ! TDF !
TDOK @ -1 T=
TDF @ TWX-TFAM-SLOTS@ 3 T=

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
s" " s" tdcolor" TWX-TFAM-FIND-IN TDOK ! TDF !
TDOK @ -1 T=
TDF @ TFAM-KIND@ TK-ENUM T=
TDF @ TWX-TFAM-ENUM? -1 T=
TDF @ TWX-TFAM-SUM? 0 T=
TDF @ TWX-TFAM-CELL? 0 T=
TDF @ TFAM-ARITY@ 0 T=
TDF @ TWX-TFAM-VIS@ CHECKER-PACKAGE-PUBLIC T=
TDF @ TFAM-VAR-COUNT@ 3 T=
TDF @ TWX-TFAM-SLOTS@ 0 T=
\ width is tag-only (docs §18: WIDTH(enum) = tag width = 1).
TDF @ TFAM-WIDTH@ 1 T=
TDF @ TFAM-VAR-START@ TDV0 !
TDV0 @ TWX-SUMV-FAM@ TDF @ T=
TDV0 @ SUMV-NAME$ s" red" T$=
TDV0 @ TWX-SUMV-TAG@ 0 T=
TDV0 @ TWX-SUMV-PAYCELLS@ 0 T=
TDV0 @ TWX-SUMV-SCH-COUNT@ 0 T=
TDV0 @ 1 + SUMV-NAME$ s" green" T$=
TDV0 @ 1 + TWX-SUMV-TAG@ 1 T=
TDV0 @ 2 + SUMV-NAME$ s" blue" T$=
TDV0 @ 2 + TWX-SUMV-TAG@ 2 T=
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
\ TWX-LAYOUT-PUSH-FIELDS (shared with sums/enums), but no constructor is published.
\ ---------------------------------------------------------------------------
PRODUCT tdpair 2
  FIELD fst a
  FIELD snd b
;PRODUCT
s" " s" tdpair" TWX-TFAM-FIND-IN TDOK ! TDF !
TDOK @ -1 T=
TDF @ TFAM-KIND@ TK-PRODUCT T=
TDF @ TWX-TFAM-PRODUCT? -1 T=
TDF @ TWX-TFAM-SUM? 0 T=
TDF @ TWX-TFAM-ENUM? 0 T=
TDF @ TWX-TFAM-CELL? 0 T=
TDF @ TWX-TFAM-LAYOUT? -1 T=
TDF @ TFAM-ARITY@ 2 T=
TDF @ TWX-TFAM-VIS@ CHECKER-PACKAGE-PUBLIC T=
\ width = field cells, NO tag (docs §18: WIDTH(product) = sum of field widths).
TDF @ TWX-TFAM-SLOTS@ 2 T=
TDF @ TFAM-WIDTH@ 2 T=
\ two PF field rows, id-keyed by (family, tail), in declaration slot order.
TDF @ TWX-TFAM-FLD-COUNT@ 2 T=
TDF @ s" fst" TWX-PF-FIND TDOK ! TDX !
TDOK @ -1 T=
TDX @ TWX-PF-FAM@ TDF @ T=
TDX @ TWX-PF-SLOT@ 0 T=
TDX @ TWX-PF-NAME$ s" fst" T$=
TDF @ s" snd" TWX-PF-FIND TDOK ! TDY !
TDOK @ -1 T=
TDY @ TWX-PF-SLOT@ 1 T=
\ field schema: fst = paramref 0, snd = paramref 1 (one cell each).
TDX @ TWX-PF-SCH@ TWX-SCHEMA-ROOT@ TWX-SCHEMA-PARAM? -1 T=
TDX @ TWX-PF-SCH@ TWX-SCHEMA-ROOT@ TWX-SCHEMA-A@ 0 T=
TDY @ TWX-PF-SCH@ TWX-SCHEMA-ROOT@ TWX-SCHEMA-A@ 1 T=
\ generated-word metadata (item 15): two generator-owned SUMV rows sharing the
\ field schema range, ctor package derived from the (pkg, tail) identity.
TDF @ TFAM-VAR-COUNT@ 2 T=
TDF @ TFAM-VAR-START@ SUMV-NAME$ s" make" T$=
TDF @ TFAM-VAR-START@ 1 + SUMV-NAME$ s" unmake" T$=
TDF @ TFAM-VAR-START@ SUMV-CTOR-PKG$ s" TDPAIR" T$=
TDF @ TFAM-VAR-START@ TWX-SUMV-PAYCELLS@ 2 T=
TDF @ TFAM-VAR-START@ TWX-SUMV-SCH-COUNT@ 2 T=
TDF @ TFAM-VAR-START@ TWX-SUMV-SCH-START@ TWX-SCHEMA-ROOT@ TWX-SCHEMA-PARAM? -1 T=
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
s" " s" tdpoint" TWX-TFAM-FIND-IN TDOK ! TDF !
TDOK @ -1 T=
TDF @ TFAM-ARITY@ 0 T=
TDF @ TWX-TFAM-SLOTS@ 2 T=
TDF @ TFAM-WIDTH@ 2 T=
TDF @ s" x" TWX-PF-FIND TDOK ! TDX !   TDOK @ -1 T=
TDX @ TWX-PF-SCH@ TWX-SCHEMA-ROOT@ TWX-SCHEMA-CON? -1 T=
TDX @ TWX-PF-SCH@ TWX-SCHEMA-ROOT@ TWX-SCHEMA-A@ CC-N T=
s" TDPT-ID ( tdpoint -- tdpoint )" CHECK-QUIET-CANDIDATE! -1 T=

\ mixed param + ptr fields: a ptr field is one cell; arity 1 has one param field.
PRODUCT tdbuf 1
  FIELD cap a
  FIELD raw ptr u8
;PRODUCT
s" " s" tdbuf" TWX-TFAM-FIND-IN TDOK ! TDF !
TDOK @ -1 T=
TDF @ TWX-TFAM-SLOTS@ 2 T=
TDF @ s" raw" TWX-PF-FIND TDOK ! TDX !   TDOK @ -1 T=
TDX @ TWX-PF-SLOT@ 1 T=
TDX @ TWX-PF-SCH@ TWX-SCHEMA-ROOT@ TWX-SCHEMA-PTR? -1 T=
TDX @ TWX-PF-SCH@ TWX-SCHEMA-ROOT@ TWX-SCHEMA-A@ TWX-SCHEMA-CON? -1 T=
TDX @ TWX-PF-SCH@ TWX-SCHEMA-ROOT@ TWX-SCHEMA-A@ TWX-SCHEMA-A@ s" u8" TWX-CON-OF T=

\ ---------------------------------------------------------------------------
\ item 12 (habu-tfam-12), slice 1 — layout-aware generic stack ops. A logical
\ sum/enum/product layout value is still ONE physical T-PARAM cell at this stage
\ (item 7 kept it one cell; no TWX-LAYOUT-PUSH-FIELDS expansion, no published
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

package LOCAL-BIND-CAP-TEST

$4000 constant BUF-CAP
create BUF BUF-CAP allot
variable BUF-N

: RESET ( -- ) 0 BUF-N ! ;
: APPEND ( ptr u8 n -- ) {: a:ptr u:n :}
   BUF-N @ u + BUF-CAP > IF s" local-bind test source overflow" 76 die THEN
   0 BEGIN dup u < WHILE
      a over + c@ BUF BUF-N @ + c!
      BUF-N @ 1 + BUF-N !
      1 +
   REPEAT drop ;
: BIND ( -- )
   s" dup 0 > if 5 {: x:n :} x drop then " APPEND ;
: SOURCE ( -- ptr u8 n )
   RESET
   s" MANY-BINDS ( n -- n ) " APPEND
   257 0 ?do BIND loop
   BUF BUF-N @ ;

public

: RUN ( -- )
   SOURCE CHECK-QUIET-CANDIDATE! -1 T= ;

;package

LOCAL-BIND-CAP-TEST:RUN

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
PRODUCT tdlinp 0
  FIELD owned tdown
  FIELD value n
;PRODUCT
SUMTYPE tdlins 0
  VARIANT owned tdown ;VARIANT
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
\ Concrete schema nodes carry the same ownership obligation as family
\ arguments. Product fields and sum payloads therefore reject copy, drop, and
\ typed memory even though these arity-0 families have no arguments to scan.
s" TDLINC-P-DUP ( tdlinp -- tdlinp tdlinp ) dup" CHECK-QUIET-CANDIDATE! 0 T=
s" TDLINC-P-DROP ( tdlinp -- ) drop" CHECK-QUIET-CANDIDATE! 0 T=
s" TDLINC-P-STORE ( tdlinp ptr tdlinp -- ) !" CHECK-QUIET-CANDIDATE! 0 T=
s" TDLINC-P-FETCH ( ptr tdlinp -- tdlinp ) @" CHECK-QUIET-CANDIDATE! 0 T=
s" TDLINC-P-ID ( tdlinp -- tdlinp )" CHECK-QUIET-CANDIDATE! -1 T=
s" TDLINC-S-DUP ( tdlins -- tdlins tdlins ) dup" CHECK-QUIET-CANDIDATE! 0 T=
s" TDLINC-S-DROP ( tdlins -- ) drop" CHECK-QUIET-CANDIDATE! 0 T=
s" TDLINC-S-STORE ( tdlins ptr tdlins -- ) !" CHECK-QUIET-CANDIDATE! 0 T=
s" TDLINC-S-FETCH ( ptr tdlins -- tdlins ) @" CHECK-QUIET-CANDIDATE! 0 T=
s" TDLINC-S-ID ( tdlins -- tdlins )" CHECK-QUIET-CANDIDATE! -1 T=

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

\ --- storable layouts S1/S2 (dot habu-checker-capability-typed-a480c423) -----
\ A layout value crosses `!`/`@` through a `ptr family`
\ address; the ADDRESS type carries the family identity, and a var may bind a
\ width-1 non-linear layout pointee under a ptr spine (the typed-address seam:
\ only LAYOUT-BUFFER may introduce a family-typed pointer. The
\ compiled one-cell ops are the exact W=1 lowering; pass 2 lowers W>1 from the
\ token's width fact. Linear, open-arg, and untyped/mismatched addresses stay
\ fail-closed.
SUMTYPE tdmemu 1
  VARIANT uno ;VARIANT
  VARIANT dos ;VARIANT
;SUMTYPE
variable TDS1-MEM
s" TDS1-VP ( -- ptr tdcolor ) TDS1-MEM" CHECK-QUIET-CANDIDATE! 0 T=
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
\ W > 1 store/fetch certifies and records the operation bundle width at pos 0.
s" TDS2-WIDE ( tdres<n,n> ptr tdres<n,n> -- ) !" CHECK-QUIET-CANDIDATE! -1 T=
WF-N@ 1 T=  0 WF-OFF@ 43 T=  0 WF-POS@ 0 T=  0 WF-WIDTH@ 2 T=
s" TDS2-WIDEF ( ptr tdres<n,n> -- tdres<n,n> ) @" CHECK-QUIET-CANDIDATE! -1 T=
WF-N@ 1 T=  0 WF-OFF@ 44 T=  0 WF-POS@ 0 T=  0 WF-WIDTH@ 2 T=
\ wide family mismatch and scalar laundering stay rejected.
s" TDS2-WMIX ( tdres<n,n> ptr tdmix<n,n> -- ) !" CHECK-QUIET-CANDIDATE! 0 T=
s" TDS2-WNIN ( n ptr tdres<n,n> -- ) !" CHECK-QUIET-CANDIDATE! 0 T=
s" TDS2-WNOUT ( ptr tdres<n,n> -- n ) @" CHECK-QUIET-CANDIDATE! 0 T=
\ linear / open args stay fail-closed even at width 1 (TFAM-11 rule).
s" TDS1-LIN ( tdmemu<tdown> ptr tdmemu<tdown> -- ) !" CHECK-QUIET-CANDIDATE! 0 T=
s" TDS1-OPEN ( tdmemu<a> ptr tdmemu<a> -- ) !" CHECK-QUIET-CANDIDATE! 0 T=
\ executed round-trip: store an enum, fetch it, MATCH the fetched value.
1 LAYOUT-BUFFER TDS1-BUF tdcolor
: TDS1-P ( -- ptr tdcolor ) 0 TDS1-BUF ;
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

\ Executed W=2 round-trip through sealed typed storage.
1 LAYOUT-BUFFER TDS2-RES-BUF tdres<n,n>
: TDS2-RES-P ( -- ptr tdres<n,n> ) 0 TDS2-RES-BUF ;
: TDS2-RES-PUT ( tdres<n,n> -- ) TDS2-RES-P ! ;
: TDS2-RES-GET ( -- tdres<n,n> ) TDS2-RES-P @ ;
: TDS2-RES-SEED ( -- tdres<n,n> ) 37 TDRES:ERR ;
: TDS2-RES-WRITE ( -- ) TDS2-RES-SEED TDS2-RES-PUT ;
: TDS2-RES-VAL ( -- n )
   TDS2-RES-GET MATCH tdres
     ok OF ENDOF
     err OF ENDOF
   ;MATCH ;
TDS2-RES-WRITE TDS2-RES-VAL 37 T=

\ Arbitrary W=4 family: both the full payload and zero-filled padding survive.
1 LAYOUT-BUFFER TDS2-MIX-BUF tdmix<n,n>
: TDS2-MIX-P ( -- ptr tdmix<n,n> ) 0 TDS2-MIX-BUF ;
: TDS2-MIX-PUT ( tdmix<n,n> -- ) TDS2-MIX-P ! ;
: TDS2-MIX-GET ( -- tdmix<n,n> ) TDS2-MIX-P @ ;
: TDS2-MIX-BIG ( -- tdmix<n,n> ) 91 92 93 TDMIX:BIG ;
: TDS2-MIX-SMALL ( -- tdmix<n,n> ) 41 TDMIX:SMALL ;
: TDS2-MIX-BIG! ( -- ) TDS2-MIX-BIG TDS2-MIX-PUT ;
: TDS2-MIX-SMALL! ( -- ) TDS2-MIX-SMALL TDS2-MIX-PUT ;
: TDS2-MIX-SUM ( -- n )
   TDS2-MIX-GET MATCH tdmix
     small OF ENDOF
     big OF + + ENDOF
   ;MATCH ;
TDS2-MIX-BIG! TDS2-MIX-SUM 276 T=
TDS2-MIX-SMALL!
TDS2-MIX-SUM 41 T=

\ --- closed layout-family payloads -------------------------------------------
\ A closed, non-linear, arity-0 layout family may type a PRODUCT field or SUM
\ payload. Its SC-APP schema carries the resolved family id, and physical slot
\ widths recurse through the referenced family rather than counting one schema
\ root as one cell. Parametric and linear families remain rejected.
\ An S1-tier layout family (sum/enum kind, arity 0, width 1) may type a PRODUCT
\ field: the field schema is a family application (SC-APP) carrying the resolved
\ family-id, PF.SLOT is the cumulative CELL OFFSET (identity while every field
\ is one cell), and MAKE/UNMAKE render the field as its family — the field is
\ born typed, so a swapped enum-field argument order is a checker reject (the
\ dtype/layout guarantee the CAD swap needs). A SELF-referential field is
\ recursive and rejects with E-TDECL-RECURSIVE (item 16 boxed sub-slice 1).
PRODUCT tdprec 0
  FIELD col tdcolor
  FIELD lum tdlight
  FIELD cnt n
;PRODUCT
s" " s" tdprec" TWX-TFAM-FIND-IN TDOK ! TDF !
TDOK @ -1 T=
TDF @ TFAM-KIND@ TK-PRODUCT T=
TDF @ TWX-TFAM-SLOTS@ 3 T=                    \ sum of field cell widths (all 1)
TDF @ TFAM-WIDTH@ 3 T=
TDF @ TWX-TFAM-FLD-START@ TDX !
TDX @ TWX-PF-SLOT@ 0 T=                       \ cumulative cell offsets
TDX @ 1 + TWX-PF-SLOT@ 1 T=
TDX @ 2 + TWX-PF-SLOT@ 2 T=
TDX @ TWX-PF-SCH@ TWX-SCHEMA-ROOT@ TWX-SCHEMA-APP? -1 T=
s" " s" tdcolor" TWX-TFAM-FIND-IN TDOK ! TDY !
TDX @ TWX-PF-SCH@ TWX-SCHEMA-ROOT@ TWX-SCHEMA-A@ TDY @ T=   \ SC-APP carries the enum family-id
TDX @ 2 + TWX-PF-SCH@ TWX-SCHEMA-ROOT@ TWX-SCHEMA-CON? -1 T=
TDF @ TFAM-VAR-START@ TWX-SUMV-PAYCELLS@ 3 T=       \ make/unmake rows carry cell width
\ generated MAKE/UNMAKE consume/produce the fields as their families.
s" TDP1 ( tdcolor tdlight n -- tdprec ) TDPREC:MAKE" CHECK-QUIET-CANDIDATE! -1 T=
s" TDP2 ( tdprec -- tdcolor tdlight n ) TDPREC:UNMAKE" CHECK-QUIET-CANDIDATE! -1 T=
\ the born-typed guarantee: swapped same-width enum fields are a checker reject.
s" TDP3 ( tdlight tdcolor n -- tdprec ) TDPREC:MAKE" CHECK-QUIET-CANDIDATE! 0 T=
\ typed field accessor: a checked composition over UNMAKE.
s" TDP4 ( tdprec -- tdcolor ) TDPREC:UNMAKE drop drop" CHECK-QUIET-CANDIDATE! -1 T=
\ executed round-trip: MAKE, destructure, MATCH the enum field.
: TDP-MK ( -- tdprec ) TDCOLOR:BLUE TDLIGHT:RED 7 TDPREC:MAKE ;
: TDP-COL ( tdprec -- tdcolor ) TDPREC:UNMAKE drop drop ;
: TDP-CODE ( -- n )
   TDP-MK TDP-COL MATCH tdcolor
     red OF 0 ENDOF
     green OF 1 ENDOF
     blue OF 2 ENDOF
   ;MATCH ;
TDP-CODE 2 T=
\ Wide PRODUCT memory uses the same family-typed address contract and records
\ its full W=3 bundle width. A different family or scalar result cannot cross
\ the boundary even when the physical representation is cell-based.
s" TDP-MEM-S ( tdprec ptr tdprec -- ) !" CHECK-QUIET-CANDIDATE! -1 T=
WF-N@ 1 T=  0 WF-OFF@ 35 T=  0 WF-POS@ 0 T=  0 WF-WIDTH@ 3 T=
s" TDP-MEM-F ( ptr tdprec -- tdprec ) @" CHECK-QUIET-CANDIDATE! -1 T=
WF-N@ 1 T=  0 WF-OFF@ 35 T=  0 WF-POS@ 0 T=  0 WF-WIDTH@ 3 T=
s" TDP-MEM-MIX ( tdprec ptr tdmix<n,n> -- ) !" CHECK-QUIET-CANDIDATE! 0 T=
s" TDP-MEM-N ( ptr tdprec -- n ) @" CHECK-QUIET-CANDIDATE! 0 T=
1 LAYOUT-BUFFER TDP-BUF tdprec
: TDP-P ( -- ptr tdprec ) 0 TDP-BUF ;
: TDP-PUT ( tdprec -- ) TDP-P ! ;
: TDP-GET ( -- tdprec ) TDP-P @ ;
: TDP-WRITE ( -- ) TDP-MK TDP-PUT ;
: TDP-MEM-COL ( -- n )
   TDP-GET TDPREC:UNMAKE drop drop MATCH tdcolor
     red OF 0 ENDOF
     green OF 1 ENDOF
     blue OF 2 ENDOF
   ;MATCH ;
: TDP-MEM-LIGHT ( -- n )
   TDP-GET TDPREC:UNMAKE drop nip MATCH tdlight
     red OF 0 ENDOF
     green OF 1 ENDOF
     blue OF 2 ENDOF
   ;MATCH ;
: TDP-MEM-N@ ( -- n ) TDP-GET TDPREC:UNMAKE nip nip ;
TDP-WRITE
TDP-MEM-COL 2 T=  TDP-MEM-LIGHT 0 T=  TDP-MEM-N@ 7 T=
\ Closed wider payloads preserve physical width through products and sums.
SUMTYPE tdpw 0
  VARIANT one n ;VARIANT
;SUMTYPE
s" PRODUCT tdpbad1 0 FIELD r tdres ;PRODUCT" E-TDECL-PAYLOAD TDT-NEG
s" PRODUCT tdpwide 0 FIELD w tdpw ;PRODUCT" TDT-EVAL-CATCH 0 T=
s" " s" tdpwide" TWX-TFAM-FIND-IN TDOK ! TDF !
TDOK @ -1 T=
TDF @ TWX-TFAM-SLOTS@ 2 T=
TDF @ TFAM-WIDTH@ 2 T=
TDF @ TWX-TFAM-FLD-START@ TWX-PF-SCH@ TWX-SCHEMA-ROOT@ TWX-SCHEMA-APP? -1 T=
s" TDP-WIDE-MAKE ( tdpw -- tdpwide ) TDPWIDE:MAKE" CHECK-QUIET-CANDIDATE! -1 T=
\ a SELF-referential field is recursive: item 16 boxed sub-slice 1 rejects it with
\ the §24 recursive-sum diagnostic (E-TDECL-RECURSIVE), not the generic payload one.
s" PRODUCT tdpbad3 0 FIELD s tdpbad3 ;PRODUCT" E-TDECL-RECURSIVE TDT-NEG
s" SUMTYPE tdpnest 0 VARIANT value tdpw ;VARIANT ;SUMTYPE" TDT-EVAL-CATCH 0 T=
s" " s" tdpnest" TWX-TFAM-FIND-IN TDOK ! TDF !
TDOK @ -1 T=
TDF @ TWX-TFAM-SLOTS@ 2 T=
TDF @ TFAM-WIDTH@ 3 T=
TDF @ TFAM-VAR-START@ TWX-SUMV-SCH-START@ TWX-SCHEMA-ROOT@ TWX-SCHEMA-APP? -1 T=
s" TDP-NEST-MAKE ( n -- tdpnest ) construct tdpw one construct tdpnest value" CHECK-QUIET-CANDIDATE! -1 T=
TDT-BASE!

\ --- item 12 slice-2: logical width metadata (docs §18 WIDTH function).
s" " s" tdres" TWX-TFAM-FIND-IN TDOK ! TDF !
TDOK @ -1 T=
TDF @ TFAM-WIDTH@ 2 T=
s" " s" tdlight" TWX-TFAM-FIND-IN TDOK ! TDF !
TDOK @ -1 T=
TDF @ TFAM-WIDTH@ 1 T=
s" " s" tdmix" TWX-TFAM-FIND-IN TDOK ! TDF !
TDOK @ -1 T=
TDF @ TFAM-WIDTH@ 4 T=
s" " s" tdfoo" TWX-TFAM-FIND-IN TDOK ! TDF !
TDOK @ -1 T=
TDF @ TFAM-WIDTH@ 1 T=

\ --- item 12 slice-2: per-token width facts (the emitter fact surface). One
\ row per LAYOUT operand of a transport op / locals capture: (raw source offset,
\ operand position 0=top, family-id, registry logical width). Absence = every
\ operand one cell. Offsets are byte positions in the checked body buffer.
\ The table is per-CHECK scratch, read here right after each verdict.
s" " s" tdres" TWX-TFAM-FIND-IN TDOK ! TDF !
TDOK @ -1 T=
s" " s" tdmix" TWX-TFAM-FIND-IN TDOK ! TDX !
TDOK @ -1 T=
s" WF1 ( tdres<n,n> n -- n tdres<n,n> ) swap" CHECK-QUIET-CANDIDATE! -1 T=
WF-N@ 1 T=
0 WF-OFF@ 37 T=
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
0 WF-OFF@ 33 T=
1 WF-OFF@ 36 T=
0 WF-WIDTH@ 2 T=
1 WF-WIDTH@ 2 T=
\ locals capture records the whole group at the :} token.
s" WF5 ( tdres<n,n> n -- n ) {: x y:n :} y" CHECK-QUIET-CANDIDATE! -1 T=
WF-N@ 1 T=
0 WF-OFF@ 35 T=
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
;package
s" tdpa" s" tres" TWX-TFAM-FIND-IN TDOK ! TDF !
TDOK @ -1 T=
TDF @ TWX-TFAM-VIS@ CHECKER-PACKAGE-PUBLIC T=
TDF @ TWX-TFAM-PKG$ s" tdpa" T$=
TDF @ TFAM-VAR-COUNT@ 1 T=
s" tdpa" s" tpriv" TWX-TFAM-FIND-IN TDOK ! TDF !
TDOK @ -1 T=
TDF @ TWX-TFAM-VIS@ CHECKER-PACKAGE-PRIVATE T=
\ same tail in a second package registers without aliasing (docs §6).
package tdpb
public
TYPEFAMILY tres 1
;package
s" tdpb" s" tres" TWX-TFAM-FIND-IN TDOK ! TDF !
TDOK @ -1 T=
s" tdpa" s" tres" TWX-TFAM-FIND-IN TDOK ! TDX !
TDOK @ -1 T=
TDF @ TDX @ <> -1 T=

\ ---------------------------------------------------------------------------
\ item 8 metadata: a PUBLIC sum/enum family derives its constructor package name
\ (Package Shape) at ;SUMTYPE and stores it in every variant's SV.CTOR-PKG slot;
\ a PRIVATE family exports nothing, so the slot stays empty (the `construct`
\ form is item 9). No runtime constructor word is published in this item yet.
\ ---------------------------------------------------------------------------
\ top-level public `tdres` -> package TDRES on both variants.
s" " s" tdres" TWX-TFAM-FIND-IN TDOK ! TDF !   TDOK @ -1 T=
TDF @ TFAM-VAR-START@ TDV0 !
TDV0 @ SUMV-CTOR-PKG$ s" TDRES" T$=
TDV0 @ 1 + SUMV-CTOR-PKG$ s" TDRES" T$=
\ in-package public `tdpa:tres` -> package TDPA-TRES.
s" tdpa" s" tres" TWX-TFAM-FIND-IN TDOK ! TDF !   TDOK @ -1 T=
TDF @ TFAM-VAR-START@ TDV0 !
TDV0 @ SUMV-CTOR-PKG$ s" TDPA-TRES" T$=
\ a private sum exports no constructor package: SV.CTOR-PKG stays empty.
package tdp8
private
SUMTYPE tsec 1
  VARIANT hidden a ;VARIANT
;SUMTYPE
;package
s" tdp8" s" tsec" TWX-TFAM-FIND-IN TDOK ! TDF !   TDOK @ -1 T=
TDF @ TWX-TFAM-VIS@ CHECKER-PACKAGE-PRIVATE T=
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
;package
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
;package
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
;package
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
;package
\ family declarations must reject a prior in-scope variant tail too; otherwise
\ the collision verdict depends on declaration order.
ENUM tdvowner tdfuture ;ENUM
s" TYPEFAMILY tdfuture 0" E-TDECL-NAME TDT-NEG
package tvorder
ENUM tvowner tvfuture ;ENUM
s" TYPEFAMILY tvfuture 0" E-TDECL-NAME TDT-NEG
;package
\ Package-local variant tails do not reserve unrelated package namespaces.
package tvordera
ENUM tvowner tvlocal ;ENUM
;package
package tvorderb
TYPEFAMILY tvlocal 0
;package
\ ...and an unreserved variant name in the same shape still accepts.
SUMTYPE tdvok 1
  VARIANT fine a ;VARIANT
;SUMTYPE
s" " s" tdvok" TWX-TFAM-FIND-IN TDOK ! TDF !
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
\ unterminated body (no ;SUMTYPE) reports the declaration packet via SUM-NOEND (S2).
s" SUMTYPE tdnoend 1 VARIANT ok a ;VARIANT" E-TDECL-SYNTAX TDT-NEG
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
\ item 16: layout-policy header clause (`POLICY <name>`, docs §22/§24). A missing
\ clause keeps the TWX-TFAM-DECL default (stack-cell-tag, docs §22.1); explicit
\ stack-cell-tag and packed-tag accept on sum/enum/product (packed bakes only a
\ memory ABI descriptor at close - stack shape identical, pinned in
\ test/type-family-suite.f); niche-null/boxed are recognised but reject as
\ not-yet-supported until their per-policy lowering ships (grammar-gated); any other token — including the v1 non-goal `custom`
\ the LAY-* registry tolerates but the grammar must not expose — is an unknown
\ policy; POLICY is a reserved name. Every reject rolls back to baseline (TDT-NEG
\ asserts TDT-BASE=). The recursive-sum reject (docs §24) lands with boxed.
\ ---------------------------------------------------------------------------
\ explicit stack-cell-tag accepts and records the default policy — sum header
\ (after arity), enum header (after name), product header (after arity). The
\ POLICY clause is parsed in the shared CHECKER-DEF*-BODY before any
\ visibility-dependent constructor generation, so these probe the parse with
\ PRIVATE families inside a package: a PUBLIC family publishes constructors and
\ each consumes one slot of the fixed protected-WID seal registry (item 2b),
\ whose ~16/session cap this suite already sits at (dot
\ habu-seal-protwid-cap-6f1c9d2b). Private families skip constructor generation,
\ so they exercise TWX-TDECL-POLICY on sum/enum/product without touching that cap.
package tpol
SUMTYPE tdpol 1 POLICY stack-cell-tag
  VARIANT none   ;VARIANT
  VARIANT some a ;VARIANT
;SUMTYPE
ENUM tdpolen POLICY stack-cell-tag red green blue ;ENUM
PRODUCT tdpolpr 0 POLICY stack-cell-tag FIELD x n FIELD y n ;PRODUCT
\ packed-tag accepts on every header (descriptor coverage: type-family-suite).
SUMTYPE tdpolpk 1 POLICY packed-tag
  VARIANT none   ;VARIANT
  VARIANT some a ;VARIANT
;SUMTYPE
ENUM tdpolpke POLICY packed-tag red green ;ENUM
PRODUCT tdpolpkr 0 POLICY packed-tag FIELD x n ;PRODUCT
\ missing clause still defaults to stack-cell-tag.
SUMTYPE tdpoldef 1
  VARIANT none   ;VARIANT
  VARIANT some a ;VARIANT
;SUMTYPE
;package
s" tpol" s" tdpol" TWX-TFAM-FIND-IN TDOK ! TDF !
TDOK @ -1 T=
TDF @ TFAM-KIND@ TK-SUM T=
TDF @ TWX-TFAM-LAYOUT-POLICY@ TL-STACK-CELL-TAG T=
s" tpol" s" tdpolen" TWX-TFAM-FIND-IN TDOK ! TDF !
TDOK @ -1 T=
TDF @ TFAM-KIND@ TK-ENUM T=
TDF @ TWX-TFAM-LAYOUT-POLICY@ TL-STACK-CELL-TAG T=
s" tpol" s" tdpolpr" TWX-TFAM-FIND-IN TDOK ! TDF !
TDOK @ -1 T=
TDF @ TFAM-KIND@ TK-PRODUCT T=
TDF @ TWX-TFAM-LAYOUT-POLICY@ TL-STACK-CELL-TAG T=
s" tpol" s" tdpoldef" TWX-TFAM-FIND-IN TDOK ! TDF !
TDOK @ -1 T=
TDF @ TWX-TFAM-LAYOUT-POLICY@ TL-STACK-CELL-TAG T=
\ packed-tag readback on every header kind (descriptor values: type-family-suite).
s" tpol" s" tdpolpk" TWX-TFAM-FIND-IN TDOK ! TDF !
TDOK @ -1 T=
TDF @ TWX-TFAM-LAYOUT-POLICY@ TL-PACKED-TAG T=
s" tpol" s" tdpolpke" TWX-TFAM-FIND-IN TDOK ! TDF !
TDOK @ -1 T=
TDF @ TWX-TFAM-LAYOUT-POLICY@ TL-PACKED-TAG T=
s" tpol" s" tdpolpkr" TWX-TFAM-FIND-IN TDOK ! TDF !
TDOK @ -1 T=
TDF @ TWX-TFAM-LAYOUT-POLICY@ TL-PACKED-TAG T=
\ not-yet-supported policies reject on every header (grammar-gated until lowering).
s" SUMTYPE tdpolns2 1 POLICY niche-null VARIANT some a ;VARIANT ;SUMTYPE" E-TDECL-POLICY TDT-NEG
s" SUMTYPE tdpolns3 1 POLICY boxed VARIANT some a ;VARIANT ;SUMTYPE" E-TDECL-POLICY TDT-NEG
s" ENUM tdpolns4 POLICY boxed red green ;ENUM" E-TDECL-POLICY TDT-NEG
s" PRODUCT tdpolns5 0 POLICY niche-null FIELD x n ;PRODUCT" E-TDECL-POLICY TDT-NEG
\ unknown policy names reject (incl. the v1 non-goal `custom`).
s" SUMTYPE tdpolun1 1 POLICY bogus VARIANT some a ;VARIANT ;SUMTYPE" E-TDECL-POLICY TDT-NEG
s" SUMTYPE tdpolun2 1 POLICY custom VARIANT some a ;VARIANT ;SUMTYPE" E-TDECL-POLICY TDT-NEG
\ POLICY keyword with no following name.
s" SUMTYPE tdpolmiss 1 POLICY ;SUMTYPE" E-TDECL-POLICY TDT-NEG
\ POLICY is reserved: it may not name a family or a variant.
s" SUMTYPE policy 1 VARIANT some a ;VARIANT ;SUMTYPE" E-TDECL-NAME TDT-NEG
s" SUMTYPE tdpolrv 1 VARIANT policy a ;VARIANT ;SUMTYPE" E-TDECL-NAME TDT-NEG
s" TYPEFAMILY policy 1" E-TDECL-NAME TDT-NEG

\ ---------------------------------------------------------------------------
\ item 16 boxed sub-slice 1: a DIRECT self-family reference in a payload makes
\ the family recursive, which only the (deferred) boxed policy can represent; so
\ under any non-boxed policy — every family today, since packed/niche/boxed reject
\ at the POLICY clause — a self-ref rejects with the docs §24 recursive-sum
\ diagnostic (E-TDECL-RECURSIVE), not the generic E-TDECL-PAYLOAD "unknown payload
\ type". ptr-wrapped, inline, bare, and product-field forms all reject; the reject
\ rolls back to baseline (TDT-NEG asserts TDT-BASE=). Mutual recursion
\ (A -> B -> A) needs a schema cycle walk and is a later sub-slice.
\ ---------------------------------------------------------------------------
s" SUMTYPE tdrec1 1 VARIANT leaf a ;VARIANT VARIANT node ptr tdrec1<a> ;VARIANT ;SUMTYPE" E-TDECL-RECURSIVE TDT-NEG
s" SUMTYPE tdrec2 1 VARIANT node tdrec2<a> ;VARIANT ;SUMTYPE" E-TDECL-RECURSIVE TDT-NEG
s" SUMTYPE tdrec3 0 VARIANT self tdrec3 ;VARIANT ;SUMTYPE" E-TDECL-RECURSIVE TDT-NEG
s" PRODUCT tdrec4 0 FIELD child tdrec4 ;PRODUCT" E-TDECL-RECURSIVE TDT-NEG
\ a NON-self family payload is unchanged: still the generic unknown-payload reject.
s" SUMTYPE tdrec5 1 VARIANT node tdother<a> ;VARIANT ;SUMTYPE" E-TDECL-PAYLOAD TDT-NEG
\ non-recursive payloads are unaffected: a ptr-to-concrete payload still accepts
\ (private family - no public constructors, so the protected-WID seal cap is
\ untouched, dot habu-seal-protwid-cap-6f1c9d2b).
package tdrp
SUMTYPE tdrec6 1
  VARIANT node ptr u8 ;VARIANT
;SUMTYPE
;package
s" tdrp" s" tdrec6" TWX-TFAM-FIND-IN TDOK ! TDF !
TDOK @ -1 T=
TDF @ TFAM-KIND@ TK-SUM T=

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
\ item 16: a policy reject flows into the same declaration-shaped prose packet —
\ it rides the standard TWX-TDECL-THROW path, so it needs no repair-diagnostics (item
\ 13) change; the richer JSON ADT fields join it unchanged when item 13 lands.
s" SUMTYPE tdpoldg 1 POLICY boxed VARIANT some a ;VARIANT ;SUMTYPE" E-TDECL-POLICY TDT-NEG

DIAG-BUFFER$ s" bad sumtype declaration" TDT-CONTAINS? -1 T=
DIAG-BUFFER$ s" layout policy not yet supported" TDT-CONTAINS? -1 T=

\ C2: a declaration body over TDECL-CAP ($1000) reports the packet, not a raw
\ die (the length check fires ahead of variant parsing, so repeats never matter).
create TDT-BIG 8192 allot   variable TDT-BIG-U
: TDT-BIG-C, ( n -- ) TDT-BIG TDT-BIG-U @ + c!  TDT-BIG-U @ 1+ TDT-BIG-U ! ;
: TDT-BIG-APP ( ptr u8 n -- ) {: a:ptr u:n :}  u 0 ?do a i + c@ TDT-BIG-C, loop ;
: TDT-BIG-SUM$ ( -- ptr u8 n )
   0 TDT-BIG-U !
   s" SUMTYPE tdbig 1 " TDT-BIG-APP
   200 0 ?do s" VARIANT vvvvvvvvvvvvvvvvvvvv n ;VARIANT " TDT-BIG-APP loop
   s" ;SUMTYPE" TDT-BIG-APP
   TDT-BIG TDT-BIG-U @ ;
TDT-BIG-SUM$ E-TDECL-CAP TDT-NEG

\ ---------------------------------------------------------------------------
\ multi-error load mode: a bad top-level declaration is reported + counted +
\ rolled back, without a fake declared signature, and the load continues.
\ ---------------------------------------------------------------------------
TWX-MULTI-ERR-BEGIN
s" SUMTYPE tdme 2 VARIANT ok a ;VARIANT VARIANT ok b ;VARIANT ;SUMTYPE TYPEFAMILY tdcont 1 : TDMEW ( n -- n ) ;" evaluate
TWX-MULTI-ERR-END 1 T=
s" " s" tdme" TWX-TFAM-FIND-IN TDOK ! drop
TDOK @ 0 T=
s" " s" tdcont" TWX-TFAM-FIND-IN TDOK ! drop
TDOK @ -1 T=
s" TDMEW" TWX-CHECKER-FIND-USIG -1 T=
\ missing terminator in multi-error mode: reported, counted, load continues.
TWX-MULTI-ERR-BEGIN
s" SUMTYPE tdnoe 1 VARIANT ok a ;VARIANT" evaluate
TWX-MULTI-ERR-END 1 T=
s" " s" tdnoe" TWX-TFAM-FIND-IN TDOK ! drop
TDOK @ 0 T=
\ two bad declarations count separately.
TWX-MULTI-ERR-BEGIN
s" TYPEFAMILY Bad1 1 TYPEFAMILY tdok9 1 SUMTYPE tdes 1 ;SUMTYPE" evaluate
TWX-MULTI-ERR-END 2 T=
s" " s" tdok9" TWX-TFAM-FIND-IN TDOK ! drop
TDOK @ -1 T=
\ a bad declaration does not poison later checks after the mode ends.
s" TDOK-AFTER ( tdfoo<n,n> -- tdfoo<n,n> )" CHECK-QUIET-CANDIDATE! -1 T=
\ unknown-family and wrong-arity SIGNATURES in multi-error mode: reported,
\ counted, and the load continues — but the invalid declared signature must
\ NOT be stored as a cert row (later checks stay sound).
TWX-MULTI-ERR-BEGIN
s" : TDSME1 ( nope<n> -- nope<n> ) ; : TDSME2 ( tdfoo<n> -- tdfoo<n> ) ; : TDSME3 ( n -- n ) ;" evaluate
TWX-MULTI-ERR-END 2 T=
s" TDSME1" TWX-CHECKER-FIND-USIG 0 T=
s" TDSME2" TWX-CHECKER-FIND-USIG 0 T=
s" TDSME3" TWX-CHECKER-FIND-USIG -1 T=
\ a raw TRUST row with an unparseable signature: counted + reported, no row.
TWX-MULTI-ERR-BEGIN
s\" s\" TDTBAD\" s\" nope<n> -- n\" TRUST : TDTOK ( n -- n ) ;" evaluate
TWX-MULTI-ERR-END 1 T=
s" TDTBAD" TWX-CHECKER-FIND-USIG 0 T=
s" TDTOK" TWX-CHECKER-FIND-USIG -1 T=
DIAG-BUFFER-OFF

\ ---------------------------------------------------------------------------
\ item 12 slice-3a: hidden-field substrate (inert). Drives the new checker
\ substrate at TOP-LEVEL interpret (registry words resolve here; new never runs,
\ so the terms built below survive across every assert). No CHECK runs after the
\ first term is built. TWX-LAYOUT-PUSH-FIELDS is NOT wired into TWX-PUSH-LOGICAL yet, so
\ every check above this section already proved user-visible behavior unchanged.
\ ---------------------------------------------------------------------------
variable TD3F    variable TD3M    variable TD3OK
variable TD3LOG  variable TD3MLOG
variable TD3H0   variable TD3H1
variable TD3ROW  variable TD3CUR

\ resolve the tdres (width 2) and tdmix (width 4) families declared above.
s" " s" tdres" TWX-TFAM-FIND-IN TD3OK ! TD3F !
TD3OK @ -1 T=
s" " s" tdmix" TWX-TFAM-FIND-IN TD3OK ! TD3M !
TD3OK @ -1 T=

\ build a LOGICAL tdres<n,n> term via the same TWX-MK-PARAM path SIG parsing drives.
PARAM-SCR-N @
CC-N TWX-MK-CON TWX-PARAM-SCR+
CC-N TWX-MK-CON TWX-PARAM-SCR+
s" tdres" TD3F @ TWX-MK-PARAM  TD3LOG !
\ a logical layout term is NOT hidden.
TD3LOG @ TWX-HIDDEN-PARAM? 0 T=
TD3LOG @ TWX-PARAM>HID 0 T=
TD3LOG @ TWX-PARAM>FAM TD3F @ T=

\ mint hidden fields for slot 0 (payload) and slot 1 (tag = W-1).
TD3LOG @ 0 TWX-MK-HIDDEN TD3H0 !
TD3LOG @ 1 TWX-MK-HIDDEN TD3H1 !
TD3H0 @ TWX-HIDDEN-PARAM? -1 T=
TD3H1 @ TWX-HIDDEN-PARAM? -1 T=
TD3H0 @ TWX-HIDDEN-SLOT@ 0 T=
TD3H1 @ TWX-HIDDEN-SLOT@ 1 T=
TD3H0 @ TWX-PARAM>HID 1 T=          \ slot+1 encoding
TD3H1 @ TWX-PARAM>HID 2 T=
TD3H0 @ TWX-PARAM>FAM TD3F @ T=
TD3H1 @ TWX-PARAM>FAM TD3F @ T=

\ TWX-LAYOUT-PUSH-FIELDS on an empty fresh row pushes exactly W=2 cells, tag on top,
\ slot0 deepest (docs §5). Walk top-down: W-1 (tag), then 0, then the base var.
TWX-FRESH TWX-MK-ROW  TD3ROW !
TD3LOG @ TD3ROW @ TWX-LAYOUT-PUSH-FIELDS  TD3CUR !
TD3CUR @ TWX-R-RES TWX-TAG S-PUSH T=                                  \ top cell present
TD3CUR @ TWX-R-RES TWX-P>TYPE TWX-HIDDEN-SLOT@ 1 T=                       \ ...is the tag (slot W-1)
TD3CUR @ TWX-R-RES TWX-P>REST TWX-R-RES TWX-TAG S-PUSH T=                     \ next cell present
TD3CUR @ TWX-R-RES TWX-P>REST TWX-R-RES TWX-P>TYPE TWX-HIDDEN-SLOT@ 0 T=          \ ...is slot0
TD3CUR @ TWX-R-RES TWX-P>REST TWX-R-RES TWX-P>REST TWX-R-RES TWX-TAG S-ROW T=         \ then the base row var — exactly W cells added

\ unification discipline (TWX-UNIFY ( t t -- bool ), self-contained per call).
\ same family + same slot -> pair.
TD3LOG @ 0 TWX-MK-HIDDEN  TD3LOG @ 0 TWX-MK-HIDDEN  TWX-UNIFY -1 T=
\ same family, different slot -> reject.
TD3LOG @ 0 TWX-MK-HIDDEN  TD3LOG @ 1 TWX-MK-HIDDEN  TWX-UNIFY 0 T=
\ hidden never binds a var, even under whole-bundle transport mode.
TD3LOG @ 0 TWX-MK-HIDDEN  TWX-FRESH TWX-MK-VAR  TWX-UNIFY 0 T=
1 LAYOUT-XPORT !
TD3LOG @ 0 TWX-MK-HIDDEN  TWX-FRESH TWX-MK-VAR  TWX-UNIFY 0 T=
0 LAYOUT-XPORT !
\ hidden never unifies a con.
TD3LOG @ 0 TWX-MK-HIDDEN  CC-N TWX-MK-CON  TWX-UNIFY 0 T=
\ a hidden field never unifies its own logical value.
TD3LOG @ 0 TWX-MK-HIDDEN  TD3LOG @  TWX-UNIFY 0 T=

\ cross-family: a same-slot hidden field of a DIFFERENT family rejects.
PARAM-SCR-N @
CC-N TWX-MK-CON TWX-PARAM-SCR+
CC-N TWX-MK-CON TWX-PARAM-SCR+
s" tdmix" TD3M @ TWX-MK-PARAM  TD3MLOG !
TD3MLOG @ 0 TWX-MK-HIDDEN  TD3LOG @ 0 TWX-MK-HIDDEN  TWX-UNIFY 0 T=

\ ---------------------------------------------------------------------------
\ report: "ok" on success, nonzero exit on any failure.
\ ---------------------------------------------------------------------------
\ ---------------------------------------------------------------------------
\ derive S1 (dot habu-checker-capability-derive): `DERIVE eq` on a PUBLIC ENUM
\ generates PKG:TAG (discriminant) + PKG:EQ (tag equality) as ORDINARY CHECKED
\ words in the ctor package — no window, no trust, no engine lowering.
\ ---------------------------------------------------------------------------
TDIAG-BUF 8192 DIAG-BUFFER!   \ silence this section's expected declaration rejects
ENUM tdrv DERIVE eq red green blue ;ENUM
: TDRV-EQ-SAME ( -- bool ) TDRV:RED TDRV:RED TDRV:EQ ;
: TDRV-EQ-DIFF ( -- bool ) TDRV:RED TDRV:GREEN TDRV:EQ ;
: TDRV-EQ-LAST ( -- bool ) TDRV:BLUE TDRV:BLUE TDRV:EQ ;
: TDRV-TAG0 ( -- n ) TDRV:RED TDRV:TAG ;
: TDRV-TAG2 ( -- n ) TDRV:BLUE TDRV:TAG ;
TDRV-EQ-SAME -1 T=
TDRV-EQ-DIFF 0 T=
TDRV-EQ-LAST -1 T=
TDRV-TAG0 0 T=
TDRV-TAG2 2 T=

\ the scalar =/0= wall on layout values is UNTOUCHED by derive (TD12-ZEQ kin).
s" TDRV-RAWEQ ( tdrv tdrv -- bool ) =" CHECK-QUIET-CANDIDATE! 0 T=
s" TDRV-RAWZ ( tdrv -- bool ) 0=" CHECK-QUIET-CANDIDATE! 0 T=

\ the derived word is family-exact: the wrong family rejects.
ENUM tdrw one two ;ENUM
s" TDRW-XEQ ( tdrw tdrw -- bool ) TDRV:EQ" CHECK-QUIET-CANDIDATE! 0 T=
\ a non-derived enum grows no eq surface (undefined word: uncheckable).
s" TDRW-NOEQ ( tdrw tdrw -- bool ) TDRW:EQ" CHECK-QUIET-CANDIDATE! 1 T=
\ the derived words are undefine-protected exactly like constructors (a direct
\ new-tail forge dies at the engine prot-wid publish, uncatchable in-process —
\ same enforcement as constructor forges, so the pins here mirror the
\ type-ctor-suite seal pins: undefine + reopen).
s" undefine TDRV:EQ" E-CTOR-PROTECTED TDT-NEG
s" undefine TDRV:TAG" E-CTOR-PROTECTED TDT-NEG
s" package tdrv" E-CTOR-PROTECTED TDT-NEG

\ grammar negatives: deferred/unknown features, kind gates, name collision,
\ missing feature token (DERIVE eats exactly one feature token).
\ (S1 deferred `DERIVE hash`; derive S3 accepts it — pinned positively below.)
s" ENUM tdrb2 DERIVE order aa bb ;ENUM" E-TDECL-DERIVE TDT-NEG
s" ENUM tdrb3 DERIVE frobnicate aa bb ;ENUM" E-TDECL-DERIVE TDT-NEG
s" ENUM tdrb4 DERIVE ;ENUM" E-TDECL-DERIVE TDT-NEG
\ (S1 kind-gated sums/products; derive S2 accepts them — pinned positively in
\ the S2 section below, so the old enum-only rejects are retired.)
\ adjacent `eq` after the clause reads as a redundant FEATURE (order-free
\ list, idempotent); a variant spelled eq must follow another variant.
s" ENUM tdrb7 DERIVE eq other eq ;ENUM" E-TDECL-NAME TDT-NEG
s" ENUM tdrb8 DERIVE eq tag other ;ENUM" E-TDECL-NAME TDT-NEG

\ rollback: a REJECTED derive declaration leaves no residue — the same family
\ name registers cleanly afterwards and its derived words work.
s" ENUM tdrb9 DERIVE eq bad eq ;ENUM" E-TDECL-NAME TDT-NEG
ENUM tdrb9 DERIVE eq aa bb ;ENUM
: TDRB9-EQ ( -- bool ) TDRB9:AA TDRB9:BB TDRB9:EQ ;
TDRB9-EQ 0 T=

\ a variant named eq/tag stays legal WITHOUT derive (no reservation creep).
ENUM tdrx eq neq ;ENUM
s" TDRX-USE ( -- tdrx ) TDRX:EQ" CHECK-QUIET-CANDIDATE! -1 T=

\ ---------------------------------------------------------------------------
\ derive S2: payload sums compare diagonally (outer MATCH binds one value's
\ payloads, inner MATCH the other's; same-variant arms compare scalars with
\ `=`, cross arms are false); products UNMAKE both values and compare
\ field-wise, enum-typed fields through their family's PKG:TAG.
\ ---------------------------------------------------------------------------
SUMTYPE tdsv 0 DERIVE eq
  VARIANT nil ;VARIANT
  VARIANT sing n ;VARIANT
  VARIANT pair n i64 ;VARIANT
;SUMTYPE
: TDSV-EQ-NIL ( -- bool ) TDSV:NIL TDSV:NIL TDSV:EQ ;
: TDSV-EQ-S ( -- bool ) 5 TDSV:SING 5 TDSV:SING TDSV:EQ ;
: TDSV-NEQ-S ( -- bool ) 5 TDSV:SING 6 TDSV:SING TDSV:EQ ;
: TDSV-EQ-P ( -- bool ) 1 2 TDSV:PAIR 1 2 TDSV:PAIR TDSV:EQ ;
: TDSV-NEQ-P1 ( -- bool ) 1 2 TDSV:PAIR 1 3 TDSV:PAIR TDSV:EQ ;
: TDSV-NEQ-P0 ( -- bool ) 1 2 TDSV:PAIR 9 2 TDSV:PAIR TDSV:EQ ;
: TDSV-NEQ-X ( -- bool ) TDSV:NIL 5 TDSV:SING TDSV:EQ ;
: TDSV-NEQ-Y ( -- bool ) 1 2 TDSV:PAIR TDSV:NIL TDSV:EQ ;
: TDSV-TAG-P ( -- n ) 1 2 TDSV:PAIR TDSV:TAG ;
TDSV-EQ-NIL -1 T=
TDSV-EQ-S -1 T=
TDSV-NEQ-S 0 T=
TDSV-EQ-P -1 T=
TDSV-NEQ-P1 0 T=
TDSV-NEQ-P0 0 T=
TDSV-NEQ-X 0 T=
TDSV-NEQ-Y 0 T=
TDSV-TAG-P 2 T=
\ the scalar = wall stays closed on a derived W>1 sum.
s" TDSV-RAW ( tdsv tdsv -- bool ) =" CHECK-QUIET-CANDIDATE! 0 T=

\ product field-wise eq; the col field routes through TDRV:TAG (derived enum).
PRODUCT tdpv 0 DERIVE eq
  FIELD col tdrv
  FIELD amt n
;PRODUCT
: TDPV-EQ ( -- bool ) TDRV:RED 7 TDPV:MAKE TDRV:RED 7 TDPV:MAKE TDPV:EQ ;
: TDPV-NEQ-C ( -- bool ) TDRV:RED 7 TDPV:MAKE TDRV:GREEN 7 TDPV:MAKE TDPV:EQ ;
: TDPV-NEQ-N ( -- bool ) TDRV:RED 7 TDPV:MAKE TDRV:RED 8 TDPV:MAKE TDPV:EQ ;
TDPV-EQ -1 T=
TDPV-NEQ-C 0 T=
TDPV-NEQ-N 0 T=
\ products derive EQ only: no TWX-TAG surface (undefined -> uncheckable), and the
\ derived EQ is undefine-protected like a constructor.
s" TDPV-TAG ( tdpv -- n ) TDPV:TAG" CHECK-QUIET-CANDIDATE! 1 T=
s" undefine TDPV:EQ" E-CTOR-PROTECTED TDT-NEG

\ S2 payload-role gates: pointer, parametric, linear, non-derived enum field,
\ and the sum-variant name collision.
s" SUMTYPE tdsb1 0 DERIVE eq VARIANT hold ptr u8 ;VARIANT ;SUMTYPE" E-TDECL-DERIVE TDT-NEG
s" SUMTYPE tdsb2 1 DERIVE eq VARIANT hold a ;VARIANT ;SUMTYPE" E-TDECL-DERIVE TDT-NEG
s" SUMTYPE tdsb3 0 DERIVE eq VARIANT hold tdown ;VARIANT ;SUMTYPE" E-TDECL-DERIVE TDT-NEG
s" PRODUCT tdsb4 0 DERIVE eq FIELD cc tdrw ;PRODUCT" E-TDECL-DERIVE TDT-NEG
s" SUMTYPE tdsb5 0 DERIVE eq VARIANT eq n ;VARIANT ;SUMTYPE" E-TDECL-NAME TDT-NEG

\ rollback: a rejected S2 derive leaves no residue; the name redeclares clean.
s" SUMTYPE tdsb6 0 DERIVE eq VARIANT hold ptr u8 ;VARIANT ;SUMTYPE" E-TDECL-DERIVE TDT-NEG
SUMTYPE tdsb6 0 DERIVE eq VARIANT hold n ;VARIANT ;SUMTYPE
: TDSB6-EQ ( -- bool ) 4 TDSB6:HOLD 4 TDSB6:HOLD TDSB6:EQ ;
TDSB6-EQ -1 T=

\ ---------------------------------------------------------------------------
\ derive S3: hash — the checked SEMANTIC generator (FNV-1a folded over the
\ variant tag + bound payload scalars per branch; products fold fields, enum
\ fields through their family's PKG:TAG). INVARIANT: equal values hash equal
\ BY CONSTRUCTION (hash folds exactly what eq compares); the differs cases
\ are non-degeneracy smoke only. `DERIVE eq hash` and `DERIVE hash eq` are
\ an order-free feature list; hash-alone derives HASH (+TAG) without EQ.
\ ---------------------------------------------------------------------------
SUMTYPE tdhs 0 DERIVE eq hash
  VARIANT hnil ;VARIANT
  VARIANT hone n ;VARIANT
  VARIANT hpair n i64 ;VARIANT
;SUMTYPE
\ eq true implies hash equal, across every eq-suite case shape.
: TDHS-C1 ( -- bool ) TDHS:HNIL TDHS:HNIL TDHS:EQ ;
: TDHS-H1 ( -- bool ) TDHS:HNIL TDHS:HASH TDHS:HNIL TDHS:HASH = ;
: TDHS-C2 ( -- bool ) 5 TDHS:HONE 5 TDHS:HONE TDHS:EQ ;
: TDHS-H2 ( -- bool ) 5 TDHS:HONE TDHS:HASH 5 TDHS:HONE TDHS:HASH = ;
: TDHS-C3 ( -- bool ) 1 2 TDHS:HPAIR 1 2 TDHS:HPAIR TDHS:EQ ;
: TDHS-H3 ( -- bool ) 1 2 TDHS:HPAIR TDHS:HASH 1 2 TDHS:HPAIR TDHS:HASH = ;
TDHS-C1 -1 T=
TDHS-H1 -1 T=
TDHS-C2 -1 T=
TDHS-H2 -1 T=
TDHS-C3 -1 T=
TDHS-H3 -1 T=
\ non-degeneracy smoke: tag difference, payload difference, payload ORDER.
: TDHS-D1 ( -- bool ) TDHS:HNIL TDHS:HASH 5 TDHS:HONE TDHS:HASH = ;
: TDHS-D2 ( -- bool ) 5 TDHS:HONE TDHS:HASH 6 TDHS:HONE TDHS:HASH = ;
: TDHS-D3 ( -- bool ) 1 2 TDHS:HPAIR TDHS:HASH 2 1 TDHS:HPAIR TDHS:HASH = ;
TDHS-D1 0 T=
TDHS-D2 0 T=
TDHS-D3 0 T=

\ product hash: order-free `DERIVE hash eq`; enum field folds via TDRV:TAG.
PRODUCT tdhp 0 DERIVE hash eq
  FIELD col tdrv
  FIELD amt n
;PRODUCT
: TDHP-C ( -- bool ) TDRV:RED 7 TDHP:MAKE TDRV:RED 7 TDHP:MAKE TDHP:EQ ;
: TDHP-H ( -- bool ) TDRV:RED 7 TDHP:MAKE TDHP:HASH TDRV:RED 7 TDHP:MAKE TDHP:HASH = ;
: TDHP-D1 ( -- bool ) TDRV:RED 7 TDHP:MAKE TDHP:HASH TDRV:GREEN 7 TDHP:MAKE TDHP:HASH = ;
: TDHP-D2 ( -- bool ) TDRV:RED 7 TDHP:MAKE TDHP:HASH TDRV:RED 8 TDHP:MAKE TDHP:HASH = ;
TDHP-C -1 T=
TDHP-H -1 T=
TDHP-D1 0 T=
TDHP-D2 0 T=

\ hash-alone: HASH + TWX-TAG generated, EQ absent (undefined -> uncheckable);
\ the derived HASH is undefine-protected like a constructor.
ENUM tdho DERIVE hash oa ob ;ENUM
: TDHO-H ( -- bool ) TDHO:OA TDHO:HASH TDHO:OA TDHO:HASH = ;
: TDHO-T ( -- n ) TDHO:OB TDHO:TAG ;
TDHO-H -1 T=
TDHO-T 1 T=
s" TDHO-E ( tdho tdho -- bool ) TDHO:EQ" CHECK-QUIET-CANDIDATE! 1 T=
s" undefine TDHO:HASH" E-CTOR-PROTECTED TDT-NEG
\ eq-only families grow no hash surface.
s" TDSV-NOH ( tdsv -- n ) TDSV:HASH" CHECK-QUIET-CANDIDATE! 1 T=

\ grammar: order stays deferred inside the list; unknown first token rejects;
\ a sum variant named hash collides; hash-only ptr/field gates still fire.
s" ENUM tdhb1 DERIVE eq order aa bb ;ENUM" E-TDECL-DERIVE TDT-NEG
s" SUMTYPE tdhb2 0 DERIVE hash VARIANT hash n ;VARIANT ;SUMTYPE" E-TDECL-NAME TDT-NEG
s" SUMTYPE tdhb3 0 DERIVE hash VARIANT hold ptr u8 ;VARIANT ;SUMTYPE" E-TDECL-DERIVE TDT-NEG
s" PRODUCT tdhb4 0 DERIVE hash FIELD cc tdrw ;PRODUCT" E-TDECL-DERIVE TDT-NEG

\ ---------------------------------------------------------------------------
\ typed locals for family types (slice 1): a bare arity-0 family tail is a
\ legal {: x:fam :} annotation. Enum-tier layouts (W=1 sum/enum) assert the
\ one-cell hidden term — the :} bind unifies the captured bundle's tag term
\ against it, reads restore the exact bound term (family id intact, MATCH
\ works), and wrong families reject through the standard mismatch packet.
\ Arity-0 CELL families assert their nominal scalar like a signature.
\ Parametric spellings, arity>0 tails, and W>1 layouts stay fail-closed as
\ named unknown annotations; linear layouts never expand into locals.
\ ---------------------------------------------------------------------------
ENUM tdlv DERIVE eq lva lvb ;ENUM
: TDLV-ID ( tdlv -- tdlv ) {: x:tdlv :} x ;
: TDLV-RT ( -- n ) TDLV:LVB TDLV-ID TDLV:TAG ;
TDLV-RT 1 T=
: TDLV-M ( tdlv -- n ) {: x:tdlv :} x match tdlv lva of 10 endof lvb of 20 endof ;match ;
: TDLV-MA ( -- n ) TDLV:LVA TDLV-M ;
: TDLV-MB ( -- n ) TDLV:LVB TDLV-M ;
TDLV-MA 10 T=
TDLV-MB 20 T=
\ two annotated locals + derived eq through the reads.
: TDLV-EQ2 ( -- bool ) TDLV:LVA TDLV:LVA {: x:tdlv y:tdlv :} x y TDLV:EQ ;
TDLV-EQ2 -1 T=
\ annotated local referenced in both branch arms.
: TDLV-BR ( n tdlv -- n ) {: v:tdlv :} 0 > if v TDLV:TAG else v TDLV:TAG 10 + then ;
: TDLV-BRT ( -- n ) 1 TDLV:LVB TDLV-BR ;
: TDLV-BRF ( -- n ) 0 TDLV:LVB TDLV-BR ;
TDLV-BRT 1 T=
TDLV-BRF 11 T=
\ single-field product (W=1 layout tier) rides the same path.
PRODUCT tdlp 0 FIELD amt n ;PRODUCT
: TDLP-ID ( tdlp -- tdlp ) {: x:tdlp :} x ;
: TDLP-RT ( -- n ) 7 TDLP:MAKE TDLP-ID TDLP:UNMAKE ;
TDLP-RT 7 T=
\ negatives: wrong family, scalar-vs-family both ways, parametric, W>1,
\ arity>0 cell tail — all reject fail-closed.
s" TDLB1 ( tdrw -- tdrw ) {: x:tdrv :} x" CHECK-QUIET-CANDIDATE! 0 T=
s" TDLB2 ( n -- n ) {: x:tdrv :} 5" CHECK-QUIET-CANDIDATE! 0 T=
s" TDLB3 ( tdrv -- tdrv ) {: x:n :} x" CHECK-QUIET-CANDIDATE! 0 T=
s" TDLB4 ( tdres<n,n> -- n ) {: x:tdres<n,n> :} 5" CHECK-QUIET-CANDIDATE! 0 T=
s" TDLB5 ( tdsv -- tdsv ) {: x:tdsv :} x" CHECK-QUIET-CANDIDATE! 0 T=
s" TDLB6 ( n -- n ) {: x:tdfoo :} x" CHECK-QUIET-CANDIDATE! 0 T=
\ bare W>1 bundle locals keep working (item 12 regression).
s" TDLB7 ( tdsv -- tdsv ) {: x :} x" CHECK-QUIET-CANDIDATE! -1 T=
\ arity-0 cell family annotation = the signature nominal.
TYPEFAMILY tdlnom 0
s" TDLB8 ( tdlnom -- tdlnom ) {: x:tdlnom :} x" CHECK-QUIET-CANDIDATE! -1 T=
s" TDLB9 ( n -- tdlnom ) {: x:tdlnom :} x" CHECK-QUIET-CANDIDATE! 0 T=

\ ---------------------------------------------------------------------------
\ E-MISMATCH family-name render (dot habu-checker-diagnostic-renderer): a
\ locals-annotation family term's stored name span points into the shared TKF
\ token-fold buffer, which later body tokens overwrite before DIAG-PRINT runs
\ (pre-fix TDLR1 rendered 'actual: oplnom<>'). The renderer reads the interned
\ registry name from the term's family id, so a locals-sourced term pins the
\ SAME exact row as a signature-declared one, and a foreign-package family
\ renders the qualified pkg:tail spelling.
\ ---------------------------------------------------------------------------
TDIAG-BUF 8192 DIAG-BUFFER!
s" TDLR1 ( tdlnom -- n ) {: q:tdlnom :} q dup drop" CHECK-CANDIDATE! 0 T=
DIAG-BUFFER$ s" expected: n actual: tdlnom<> " TDT-CONTAINS? -1 T=
TDIAG-BUF 8192 DIAG-BUFFER!
s" TDLR2 ( tdlnom -- n ) dup drop" CHECK-CANDIDATE! 0 T=
DIAG-BUFFER$ s" expected: n actual: tdlnom<> " TDT-CONTAINS? -1 T=
\ enum-tier annotation term on the EXPECTED side of the :} bind check
\ (pre-fix rendered 'expected: oplv<>').
TDIAG-BUF 8192 DIAG-BUFFER!
s" TDLR3 ( tdlp -- n ) {: q:tdlv :} q dup drop" CHECK-CANDIDATE! 0 T=
DIAG-BUFFER$ s" expected: tdlv<> actual: tdlp<> " TDT-CONTAINS? -1 T=
\ foreign-package family: the interned name renders qualified pkg:tail.
s" tdlrpk" CHECKER-PACKAGE   CHECKER-PUBLIC
TYPEFAMILY tdlrfam 0
CHECKER-END-PACKAGE
TDIAG-BUF 8192 DIAG-BUFFER!
s" TDLR4 ( tdlrpk:tdlrfam -- n ) {: q:tdlrpk:tdlrfam :} q dup drop" CHECK-CANDIDATE! 0 T=
DIAG-BUFFER$ s" expected: n actual: tdlrpk:tdlrfam<> " TDT-CONTAINS? -1 T=

\ ---------------------------------------------------------------------------
\ E-MISMATCH JSON 'family' hint (dot habu-checker-json-family): the machine
\ packet's family field carries the SAME interned qualified spelling as the
\ rendered expected/actual rows — bare tail for a global-package sum family,
\ pkg:tail for a foreign-package one (pre-fix it rendered the bare tail via
\ TFAM-NAME$, unresolvable for a foreign package). Layout families only: the
\ hint fires through TERM-FAM/LAYOUT-PARAM?, so both fixtures are SUMTYPEs.
\ ---------------------------------------------------------------------------
SUMTYPE tdlrjg 0 VARIANT keep n ;VARIANT ;SUMTYPE
s" tdlrjp" CHECKER-PACKAGE   CHECKER-PUBLIC
SUMTYPE tdlrjs 0 VARIANT keep n ;VARIANT ;SUMTYPE
CHECKER-END-PACKAGE
TDIAG-BUF 8192 DIAG-BUFFER!  -1 DIAG-JSON!
s" TDLRJ1 ( n -- tdlrjg )" CHECK-CANDIDATE! 0 T=
DIAG-BUFFER$ s\" \"family\":\"tdlrjg\"" TDT-CONTAINS? -1 T=
TDIAG-BUF 8192 DIAG-BUFFER!
s" TDLRJ2 ( n -- tdlrjp:tdlrjs )" CHECK-CANDIDATE! 0 T=
DIAG-BUFFER$ s\" \"family\":\"tdlrjp:tdlrjs\"" TDT-CONTAINS? -1 T=
DIAG-BUFFER$ s\" \"family\":\"tdlrjs\"" TDT-CONTAINS? 0 T=
0 DIAG-JSON!
DIAG-BUFFER-OFF

: REPORT ( -- )
   #FAIL @ 0 = if s" ok" type cr exit then
   #FAIL @ . s" type-decl-suite: failures" 1 die ;
REPORT
