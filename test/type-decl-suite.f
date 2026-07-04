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
\ TRUSTED: `evaluate` is the test boundary here, like CHECK-QUIET-CANDIDATE!.
TRUSTED: TDT-EVAL-CATCH ( ptr u8 n -- n )
   ['] evaluate catch
   dup 0= if exit then
   nip nip ;

\ registry high-water baseline: every rejected declaration must restore it.
\ TRUSTED: registry counters are checker-internal prefix words with no public
\ signatures (same boundary the top-level lines in type-family-suite.f use).
variable TDB-TFAM   variable TDB-SUMV   variable TDB-SCH
variable TDB-ROOT   variable TDB-STR    variable TDB-PK
TRUSTED: TDT-BASE! ( -- )
   TFAM-N@ TDB-TFAM !   SUMV-N@ TDB-SUMV !
   SCHEMA-N@ TDB-SCH !  SCHEMA-ROOT-N@ TDB-ROOT !
   TF-STR-U @ TDB-STR ! TF-PK-N @ TDB-PK ! ;
TRUSTED: TDT-BASE= ( -- )
   TFAM-N@ TDB-TFAM @ T=   SUMV-N@ TDB-SUMV @ T=
   SCHEMA-N@ TDB-SCH @ T=  SCHEMA-ROOT-N@ TDB-ROOT @ T=
   TF-STR-U @ TDB-STR @ T= TF-PK-N @ TDB-PK @ T= ;
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
\ redeclaring a global family at top level is a same-scope duplicate...
s" TYPEFAMILY ptr 0" E-TFAM-DUP TDT-NEG
s" TYPEFAMILY span 3" E-TFAM-DUP TDT-NEG
\ ...while shadowing a global family from inside a package is reserved.
package tshad
s" TYPEFAMILY span 3" E-TDECL-NAME TDT-NEG
end-package
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
DIAG-BUFFER-OFF

\ ---------------------------------------------------------------------------
\ report: "ok" on success, nonzero exit on any failure.
\ ---------------------------------------------------------------------------
: REPORT ( -- )
   #FAIL @ 0 = if s" ok" type cr exit then
   #FAIL @ . s" type-decl-suite: failures" 1 die ;
REPORT
