\ type-ctor-suite.f — generated-constructor suite (PLAN item 8, docs
\ /type-families.md §12). Run BY THE ENGINE over stdin, like the type-decl
\ suite:  bin/hb < test/type-ctor-suite.f
\ A PUBLIC arity-0 SUMTYPE generates one checked constructor word per variant
\ in its derived constructor package (RESULT:OK shape): payload cells stay,
\ M-p zero pads and the tag push on top, certified against the declared
\ hidden-field sig through the checker-owned pending-constructor rule — no
\ TRUST anywhere. Parametric (arity > 0) families are possibly-linear layouts
\ under the v1 cell-kinded params (a linear con can flow through a var), so
\ their constructors stay unpublished until TFAM 11's whole-bundle linear
\ counting; their SV.CTOR-PKG metadata is still recorded. Private families
\ generate nothing until item 9's construct form.

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

\ evaluate a declaration string, returning its throw code (0 = accepted);
\ the type-decl-suite INCLUDE-EVALUATE catch shape.
variable TCE-A   variable TCE-U
: TCE-GO ( -- )
   TCE-A @ TCE-U @ INCLUDE-EVALUATE ;
: TCE-CATCH ( ptr u8 n -- n )
   TCE-U ! TCE-A !
   [: TCE-GO ;] catch ;

variable TCF   variable TCOK
\ whitebox boundary (dot habu-hb-crash-bare-c5be6634): checker-internal colon
\ words probed at top level go through named trusted shims.
TRUSTED: TWX-CHECKER-RECORD-SYM ( ptr u8 n -- n ) CHECKER-RECORD-SYM ;
TRUSTED: TWX-FRESH ( -- n ) FRESH ;
TRUSTED: TWX-MULTI-ERR-BEGIN ( -- ) MULTI-ERR-BEGIN ;
TRUSTED: TWX-MULTI-ERR-END ( -- n ) MULTI-ERR-END ;
TRUSTED: TWX-NEW ( -- ) NEW ;
TRUSTED: TWX-SUMV-CTOR-SYM@ ( n -- n ) SUMV-CTOR-SYM@ ;
TRUSTED: TWX-SUMV-PAYCELLS@ ( n -- n ) SUMV-PAYCELLS@ ;
TRUSTED: TWX-SYMS ( -- ptr a ) SYMS ;
TRUSTED: TWX-TFAM-FIND-IN ( ptr u8 n ptr u8 n -- n bool ) TFAM-FIND-IN ;
TRUSTED: TWX-TFAM-VIS@ ( n -- n ) TFAM-VIS@ ;


\ ---------------------------------------------------------------------------
\ top-level public arity-0 sum: constructors exist, certify, enforce payloads.
\ ---------------------------------------------------------------------------
SUMTYPE zres 0
  VARIANT ok  n ;VARIANT
  VARIANT err n ;VARIANT
;SUMTYPE
\ variants spelled like stack words: ZWV:DUP/ZWV:SWAP are qualified constructor
\ defs. Declared EARLY (dot habu-qualified-defs-leak-aadeb5c9 fixed): a qualified
\ def no longer leaks a bare-global `dup`/`swap` effect row, so every later
\ checked body that uses bare dup/swap as PRIMS still certifies (proven below).
SUMTYPE zwv 0
  VARIANT dup n ;VARIANT
  VARIANT swap  ;VARIANT
;SUMTYPE

: ZMK-OK ( n -- zres ) ZRES:OK ;
: ZMK-ERR ( n -- zres ) ZRES:ERR ;
s" GEN-CONCRETE" type cr

\ wrong payload count/type rejects at the call site.
s" ZB1 ( -- zres ) ZRES:OK" CHECK-QUIET-CANDIDATE! 0 T=
s" ZB2 ( ptr u8 -- zres ) ZRES:OK" CHECK-QUIET-CANDIDATE! 0 T=
\ raw cells still cannot forge a layout value (the pending window is closed).
s" ZB3 ( n -- zres ) 0" CHECK-QUIET-CANDIDATE! 0 T=
s" ZB4 ( n n -- zres )" CHECK-QUIET-CANDIDATE! 0 T=

\ ---------------------------------------------------------------------------
\ multi-cell payloads: M > 1, zero padding for narrow variants, ptr payloads.
\ ---------------------------------------------------------------------------
SUMTYPE zmix 0
  VARIANT small n ;VARIANT
  VARIANT big ptr u8 n n ;VARIANT
;SUMTYPE
: ZMK-SMALL ( n -- zmix ) ZMIX:SMALL ;
: ZMK-BIG ( ptr u8 n n -- zmix ) ZMIX:BIG ;
s" GEN-MIX" type cr
s" ZB5 ( n n -- zmix ) ZMIX:BIG" CHECK-QUIET-CANDIDATE! 0 T=

\ ---------------------------------------------------------------------------
\ zero-payload variants: enum-shaped sums and padded empty variants.
\ ---------------------------------------------------------------------------
SUMTYPE zen 0
  VARIANT lit  ;VARIANT
  VARIANT dark ;VARIANT
;SUMTYPE
: ZMK-LIT ( -- zen ) ZEN:LIT ;
: ZMK-DARK ( -- zen ) ZEN:DARK ;
s" GEN-ENUM" type cr

SUMTYPE zopt 0
  VARIANT none   ;VARIANT
  VARIANT some n ;VARIANT
;SUMTYPE
: ZMK-NONE ( -- zopt ) ZOPT:NONE ;
: ZMK-SOME ( n -- zopt ) ZOPT:SOME ;
s" GEN-OPT" type cr

\ ---------------------------------------------------------------------------
\ in-package public family: constructor package derives from (pkg, tail) and
\ the words are callable from global scope; declaring-package state survives.
\ ---------------------------------------------------------------------------
package zpub
public
SUMTYPE tres 0
  VARIANT yes n ;VARIANT
;SUMTYPE
private
TYPEFAMILY zonly 1
end-package
\ package mode continued private after generation ran inside the block.
s" zpub" s" zonly" TWX-TFAM-FIND-IN TCOK ! TCF !   TCOK @ -1 T=
TCF @ TWX-TFAM-VIS@ CHECKER-PACKAGE-PRIVATE T=
\ the generated word is globally addressable; qualified sig type resolves.
: ZMK-YES ( n -- zpub:tres ) ZPUB-TRES:YES ;
s" GEN-PKG" type cr
\ global scope is back: a fresh top-level sum still generates.
SUMTYPE zafter 0
  VARIANT one n ;VARIANT
;SUMTYPE
: ZMK-ONE ( n -- zafter ) ZAFTER:ONE ;
s" GEN-AFTER" type cr

\ ---------------------------------------------------------------------------
\ parametric families publish (item 11 slice 1): the constructor's result is
\ one conservative logical cell while args are vars, expands to the hidden
\ fields where instantiation proves the args non-linear (LOGHID coercion),
\ and genuinely-linear instantiations stay fail-closed.
\ ---------------------------------------------------------------------------
SUMTYPE zpar 1
  VARIANT psome a ;VARIANT
;SUMTYPE
s" " s" zpar" TWX-TFAM-FIND-IN TCOK ! TCF !   TCOK @ -1 T=
TCF @ TFAM-VAR-START@ SUMV-CTOR-PKG$ s" ZPAR" T$=
s" ZB6 ( n -- zpar<n> ) ZPAR:PSOME" CHECK-QUIET-CANDIDATE! -1 T=   \ publishes + certifies

SUMTYPE zpoly 2
  VARIANT ok  a ;VARIANT
  VARIANT err b ;VARIANT
;SUMTYPE
\ concrete instantiation: the 1-cell result expands wide at the boundary.
: ZPMK-OK ( n -- zpoly<n,n> ) ZPOLY:OK ;
: ZPMK-ERR ( n -- zpoly<n,n> ) ZPOLY:ERR ;
\ ptr payload instantiation crosses the coercion with a non-scalar arg.
: ZPMK-PTR ( ptr u8 -- zpoly<ptr u8,n> ) ZPOLY:OK ;
\ generic wrapper: stays one logical cell internally, certifies against the
\ var-arg declared out, and expands at ITS concrete callers.
: ZPMK-G ( a -- zpoly<a,b> ) ZPOLY:OK ;
: ZPMK-USE ( n -- zpoly<n,n> ) ZPMK-G ;
s" GEN-POLY" type cr
\ multi-cell payloads + zero padding through a parametric family.
SUMTYPE zpmix 2
  VARIANT small a ;VARIANT
  VARIANT big a b n ;VARIANT
;SUMTYPE
: ZPMK-SMALL ( n -- zpmix<n,n> ) ZPMIX:SMALL ;
: ZPMK-BIG ( n n n -- zpmix<n,n> ) ZPMIX:BIG ;
s" GEN-POLY-MIX" type cr
\ wrong payloads keep rejecting at the call site.
s" ZP1 ( -- zpoly<n,n> ) ZPOLY:OK" CHECK-QUIET-CANDIDATE! 0 T=
s" ZP2 ( ptr u8 -- zpoly<n,n> ) ZPOLY:OK" CHECK-QUIET-CANDIDATE! 0 T=
s" ZP3 ( n -- zpoly<n,n> ) 0" CHECK-QUIET-CANDIDATE! 0 T=
\ cross-family bundles cannot alias: a zpmix result is not a zpoly.
s" ZP4 ( n -- zpoly<n,n> ) ZPMIX:SMALL" CHECK-QUIET-CANDIDATE! 0 T=
\ genuinely-linear instantiations stay fail-closed (until whole-bundle
\ linear counting): linear-arg layout sigs reject, construction included.
s" ZP5 ( own -- zpoly<own,n> ) ZPOLY:OK" CHECK-QUIET-CANDIDATE! 0 T=
s" ZP6 ( zpoly<own,n> -- zpoly<own,n> )" CHECK-QUIET-CANDIDATE! 0 T=
s" ZP7 ( own -- zpoly<own,n> ) ZPMK-G" CHECK-QUIET-CANDIDATE! 0 T=
\ transports of a still-unresolved parametric result stay rejected inside a
\ body (possibly-linear conservative path).
s" ZP8 ( n -- zpoly<n,n> zpoly<n,n> ) ZPOLY:OK dup" CHECK-QUIET-CANDIDATE! 0 T=

\ ---------------------------------------------------------------------------
\ private families export nothing: no constructor package, no words.
\ ---------------------------------------------------------------------------
package zp8
private
SUMTYPE zsec 0
  VARIANT hide n ;VARIANT
;SUMTYPE
end-package
s" zp8" s" zsec" TWX-TFAM-FIND-IN TCOK ! TCF !   TCOK @ -1 T=
TCF @ TFAM-VAR-START@ SUMV-CTOR-PKG$ nip 0 T=
s" ZB7 ( n -- n ) ZP8-ZSEC:HIDE" CHECK-QUIET-CANDIDATE! 1 T=   \ undefined word -> uncheckable

\ ---------------------------------------------------------------------------
\ rejected declarations generate nothing: a duplicate family neither redefines
\ nor duplicates the existing constructor words (load survives = proof).
\ ---------------------------------------------------------------------------
TWX-MULTI-ERR-BEGIN
s" SUMTYPE zres 0 VARIANT no n ;VARIANT ;SUMTYPE" TCE-CATCH 0 T=
TWX-MULTI-ERR-END 1 T=
: ZMK-OK2 ( n -- zres ) ZRES:OK ;
s" DUP-DECL-SAFE" type cr

\ ---------------------------------------------------------------------------
\ linear payloads stay rejected until TFAM 11 (regression pins).
\ ---------------------------------------------------------------------------
\ concrete linear payload rejects at declaration (v1 payload grammar).
TWX-MULTI-ERR-BEGIN
s" SUMTYPE zlin 0 VARIANT keep own ;VARIANT ;SUMTYPE" TCE-CATCH 0 T=
TWX-MULTI-ERR-END 1 T=
s" " s" zlin" TWX-TFAM-FIND-IN TCOK ! drop   TCOK @ 0 T=

\ ---------------------------------------------------------------------------
\ slice 3: protection. Generated packages are closed-but-callable: `package`
\ cannot open/reopen the derived name (any case), `undefine` of a generated
\ word rejects BEFORE retirement (still callable after the catch), and a new
\ tail cannot certify into the constructor package. SV.CTOR-SYM records the
\ published checker symbol.
\ ---------------------------------------------------------------------------
s" " s" zres" TWX-TFAM-FIND-IN TCOK ! TCF !   TCOK @ -1 T=
TCF @ TFAM-VAR-START@ TWX-SUMV-CTOR-SYM@ 0 <> -1 T=
TCF @ TFAM-VAR-START@ 1 + TWX-SUMV-CTOR-SYM@ 0 <> -1 T=
s" ZRES:OK" TWX-CHECKER-RECORD-SYM  TCF @ TFAM-VAR-START@ TWX-SUMV-CTOR-SYM@  T=
\ package reopen rejects, case-insensitively; state rolls back (a later
\ package still opens cleanly).
s" package zres" TCE-CATCH E-CTOR-PROTECTED T=
s" package ZRES" TCE-CATCH E-CTOR-PROTECTED T=
s" package Zres" TCE-CATCH E-CTOR-PROTECTED T=
s" package zok end-package" TCE-CATCH 0 T=
\ undefine of a generated word rejects before retirement...
s" undefine ZRES:OK" TCE-CATCH E-CTOR-PROTECTED T=
s" undefine zres:ok" TCE-CATCH E-CTOR-PROTECTED T=
\ ...and the constructor is still fully usable afterwards.
: ZMK-OK3 ( n -- zres ) ZRES:OK ;
s" UNDEF-SAFE" type cr
\ a new tail is now caught by the native protected-WID wall; seal-package.f runs
\ that process-exit regression because it cannot be caught in-process.
\ ordinary undefine of a user word still works.
: ZDOOMED ( -- n ) 5 ;
s" undefine ZDOOMED" TCE-CATCH 0 T=
\ native producer populated the protected-WID registry for constructor packages.
data-base PROT-WID-N-CELL + @ 0 > -1 T=

\ ---------------------------------------------------------------------------
\ item 9 slice 2: `construct family variant` — the checker-owned token
\ protocol. Resolution is package-identity-owned (the family must live in the
\ ACTIVE package; cross-package never resolves, public families included —
\ those use the generated words), kind-gated to sum/enum, and folded like
\ every body token. The step effect is the generated-constructor effect built
\ inline from SUMV metadata, so payload arity/type/instantiation rejects are
\ the same as the generated-word call sites. Operand capture runs BEFORE
\ locals and word lookup, so family/variant tokens never resolve as either.
\ Native construct lowering landed with item 10 slice 2 (execution round-trips
\ gate-pinned in GE-CONSTRUCT-EXEC); these stay CHECK-only candidates.
\ ---------------------------------------------------------------------------
s" CN1 ( n -- zres ) construct zres ok" CHECK-QUIET-CANDIDATE! -1 T=
s" CN2 ( n -- zres ) construct ZRES OK" CHECK-QUIET-CANDIDATE! -1 T=   \ folded spelling
s" CN3 ( ptr u8 n n -- zmix ) construct zmix big" CHECK-QUIET-CANDIDATE! -1 T=
s" CN4 ( n -- zmix ) construct zmix small" CHECK-QUIET-CANDIDATE! -1 T=   \ zero-padded narrow variant
s" CN5 ( -- zopt ) construct zopt none" CHECK-QUIET-CANDIDATE! -1 T=   \ empty payload
s" CN6 ( n -- zpar<n> ) construct zpar psome" CHECK-QUIET-CANDIDATE! -1 T=   \ parametric arity 1
s" CN7 ( ptr u8 -- zpoly<ptr u8,n> ) construct zpoly ok" CHECK-QUIET-CANDIDATE! -1 T=
s" CN8 ( n n -- zres ) {: ok:n :} construct zres ok" CHECK-QUIET-CANDIDATE! -1 T=   \ a local `ok` cannot shadow the variant token
s" CN9 ( n -- zres ) [: construct zres ok ;] execute" CHECK-QUIET-CANDIDATE! -1 T=
s" CONSTRUCT-OK" type cr
\ rejects: unknown family, unknown/wrong-family variant, non-sum family,
\ missing operand tokens, payload arity/type/instantiation mismatches,
\ dead-path use, open-arg transport.
s" CB1 ( n -- zres ) construct nosuch ok" CHECK-QUIET-CANDIDATE! 0 T=
s" CB2 ( n -- zres ) construct zres nope" CHECK-QUIET-CANDIDATE! 0 T=
s" CB3 ( n -- zres ) construct zres some" CHECK-QUIET-CANDIDATE! 0 T=   \ zopt's variant
s" CB4 ( n -- zres ) construct span ok" CHECK-QUIET-CANDIDATE! 0 T=   \ cell family: kind-gated
s" CB5 ( n -- zres ) construct zres" CHECK-QUIET-CANDIDATE! 0 T=
s" CB6 ( n -- zres ) construct" CHECK-QUIET-CANDIDATE! 0 T=
s" CB7 ( ptr u8 -- zres ) construct zres ok" CHECK-QUIET-CANDIDATE! 0 T=
s" CB8 ( -- zres ) construct zres ok" CHECK-QUIET-CANDIDATE! 0 T=
s" CB9 ( n n -- zmix ) construct zmix big" CHECK-QUIET-CANDIDATE! 0 T=
s" CB10 ( n -- zres ) exit construct zres ok" CHECK-QUIET-CANDIDATE! 0 T=   \ dead path
s" CB11 ( ptr u8 -- zpoly<n,n> ) construct zpoly ok" CHECK-QUIET-CANDIDATE! 0 T=   \ wrong instantiation
s" CB12 ( n -- zpoly<n,n> zpoly<n,n> ) construct zpoly ok dup" CHECK-QUIET-CANDIDATE! 0 T=   \ open-arg transport
\ variants spelled like stack words are captured operands, never word calls
\ (`zwv` is declared with the other families above; ordering is free now).
s" CN10 ( n -- zwv ) construct zwv dup" CHECK-QUIET-CANDIDATE! -1 T=
s" CN11 ( -- zwv ) construct zwv swap" CHECK-QUIET-CANDIDATE! -1 T=
s" CONSTRUCT-BAD" type cr

\ ---------------------------------------------------------------------------
\ leak-fix regressions (dot habu-qualified-defs-leak-aadeb5c9). `zwv` above
\ published qualified ZWV:DUP/ZWV:SWAP constructor defs. Before the fix each
\ qualified def ALSO recorded its checker effect under the BARE-GLOBAL tail,
\ so bare dup/swap resolved to the constructor effect (shadowing the prim) and
\ arbitrary bare tails certified calls the engine rejects. After the fix a
\ qualified def records only ( pkg, tail ) PUBLIC.
\ 1) bare dup/swap bind the PRIMS, not the leaked constructor rows.
s" LK1 ( n n -- n n ) swap" CHECK-QUIET-CANDIDATE! -1 T=
s" LK2 ( n -- n n ) dup"    CHECK-QUIET-CANDIDATE! -1 T=
\ 2) a plain qualified colon def leaks no bare-global tail: bare `lkw` stays an
\ undefined word (uncheckable 1), it does NOT certify (-1) as it did with the leak.
: lkpkg:lkw ( n -- n ) 1 + ;
s" LK3 ( n -- n ) lkw"       CHECK-QUIET-CANDIDATE! 1 T=
\ 3) the qualified spelling DOES resolve (records under ( pkg, tail ) PUBLIC).
s" LK4 ( n -- n ) lkpkg:lkw" CHECK-QUIET-CANDIDATE! -1 T=
s" LEAK-FIX" type cr
\ ownership: in-package resolution for public AND private families; outside
\ the package neither the bare nor the qualified family token resolves — the
\ cross-package path is the generated constructor word only.
package cnpub
public
SUMTYPE cnres 0
  VARIANT yes n ;VARIANT
;SUMTYPE
s" CP1 ( n -- cnres ) construct cnres yes" CHECK-QUIET-CANDIDATE! -1 T=
private
SUMTYPE cnsec 0
  VARIANT hide n ;VARIANT
;SUMTYPE
s" CP2 ( n -- cnsec ) construct cnsec hide" CHECK-QUIET-CANDIDATE! -1 T=
end-package
s" CB13 ( n -- cnpub:cnres ) construct cnres yes" CHECK-QUIET-CANDIDATE! 0 T=
s" CB14 ( n -- cnpub:cnres ) construct cnpub:cnres yes" CHECK-QUIET-CANDIDATE! 0 T=
s" CP3 ( n -- cnpub:cnres ) CNPUB-CNRES:YES" CHECK-QUIET-CANDIDATE! -1 T=   \ the public cross-package path
s" CONSTRUCT-OWN" type cr

\ ---------------------------------------------------------------------------
\ item 15: product generated words. A PUBLIC product publishes exactly two
\ checked words in its derived package — PKG:MAKE ( fields -- fam<..> ) and
\ PKG:UNMAKE ( fam<..> -- fields ) — both empty-bodied under the k=0 pending
\ window: a product bundle is its field cells in slot order (no tag), so
\ construction/destructure are physical no-ops and runtime round-trips are
\ identity. Same publish/protection stack as sum constructors (SUMV rows).
\ ---------------------------------------------------------------------------
PRODUCT zpt 0
  FIELD x n
  FIELD y n
;PRODUCT
\ metadata: two generator-owned rows, ctor package derived, syms recorded.
s" " s" zpt" TWX-TFAM-FIND-IN TCOK ! TCF !   TCOK @ -1 T=
TCF @ TFAM-VAR-COUNT@ 2 T=
TCF @ TFAM-VAR-START@ SUMV-NAME$ s" make" T$=
TCF @ TFAM-VAR-START@ 1 + SUMV-NAME$ s" unmake" T$=
TCF @ TFAM-VAR-START@ SUMV-CTOR-PKG$ s" ZPT" T$=
TCF @ TFAM-VAR-START@ TWX-SUMV-PAYCELLS@ 2 T=
TCF @ TFAM-VAR-START@ TWX-SUMV-CTOR-SYM@ 0 <> -1 T=
TCF @ TFAM-VAR-START@ 1 + TWX-SUMV-CTOR-SYM@ 0 <> -1 T=
\ checked construction/destructure compile through ordinary calls.
: ZPT-MK ( n n -- zpt ) ZPT:MAKE ;
: ZPT-UN ( zpt -- n n ) ZPT:UNMAKE ;
: ZPT-RT ( -- n n ) 3 4 ZPT:MAKE ZPT:UNMAKE ;
s" GEN-PRODUCT" type cr
\ runtime round-trip: make/unmake are physical no-ops (field order preserved,
\ slot0 deepest — x=3 under y=4).
ZPT-RT 4 T= 3 T=
\ user accessor compositions: destructure + ordinary drops over raw cells.
: ZPT-X ( zpt -- n ) ZPT:UNMAKE drop ;
: ZPT-Y ( zpt -- n ) ZPT:UNMAKE nip ;
: ZPT-RTX ( -- n ) 7 9 ZPT:MAKE ZPT-X ;
: ZPT-RTY ( -- n ) 7 9 ZPT:MAKE ZPT-Y ;
ZPT-RTX 7 T=
ZPT-RTY 9 T=
\ wrong payload count/type rejects at the call site; raw cells cannot forge a
\ product and a bundle cannot split without UNMAKE (window is closed).
s" PB1 ( n -- zpt ) ZPT:MAKE" CHECK-QUIET-CANDIDATE! 0 T=
s" PB2 ( ptr u8 n -- zpt ) ZPT:MAKE" CHECK-QUIET-CANDIDATE! 0 T=
s" PB3 ( n n -- zpt )" CHECK-QUIET-CANDIDATE! 0 T=
s" PB4 ( zpt -- n n )" CHECK-QUIET-CANDIDATE! 0 T=
\ whole-bundle transport still holds for constructed products.
s" PB5 ( zpt -- zpt zpt ) dup" CHECK-QUIET-CANDIDATE! -1 T=
\ construct is kind-gated to sum/enum: no token form for products.
s" PB6 ( n n -- zpt ) construct zpt make" CHECK-QUIET-CANDIDATE! 0 T=
\ ptr-payload product round-trips the mixed field row.
PRODUCT zbuf 0
  FIELD cap n
  FIELD raw ptr u8
;PRODUCT
: ZBUF-MK ( n ptr u8 -- zbuf ) ZBUF:MAKE ;
: ZBUF-UN ( zbuf -- n ptr u8 ) ZBUF:UNMAKE ;
s" GEN-PRODUCT-PTR" type cr
\ parametric products publish both words: MAKE's open result expands at
\ concrete boundaries (LOGHID out), UNMAKE's open input absorbs the caller's
\ hidden run (LOGHID in), and generic wrappers stay one logical cell inside.
PRODUCT zpr 2
  FIELD fst a
  FIELD snd b
;PRODUCT
: ZPR-MK ( n n -- zpr<n,n> ) ZPR:MAKE ;
: ZPR-UN ( zpr<n,n> -- n n ) ZPR:UNMAKE ;
: ZPR-G ( a b -- zpr<a,b> ) ZPR:MAKE ;
: ZPR-GUN ( zpr<a,b> -- a b ) ZPR:UNMAKE ;
: ZPR-USE ( n -- zpr<n,n> ) 5 ZPR-G ;
: ZPR-RT ( -- n n ) 2 8 ZPR:MAKE ZPR:UNMAKE ;
ZPR-RT 8 T= 2 T=
s" GEN-PRODUCT-POLY" type cr
\ wrong instantiation and cross-family aliasing keep rejecting.
s" PP1 ( ptr u8 -- zpr<n,n> ) 0 swap ZPR:MAKE" CHECK-QUIET-CANDIDATE! 0 T=
s" PP2 ( n n -- zpt ) ZPR:MAKE" CHECK-QUIET-CANDIDATE! 0 T=
\ linear instantiations stay fail-closed at the sig/arg-bind layers.
s" PL1 ( own n -- zpr<own,n> ) ZPR:MAKE" CHECK-QUIET-CANDIDATE! 0 T=
s" PL2 ( zpr<own,n> -- zpr<own,n> )" CHECK-QUIET-CANDIDATE! 0 T=
s" PL3 ( zpr<own,n> -- own n ) ZPR:UNMAKE" CHECK-QUIET-CANDIDATE! 0 T=
\ in-package public product: derived package, callable from global scope,
\ package state survives generation.
package zppk
public
PRODUCT prow 0
  FIELD v n
;PRODUCT
end-package
: ZPPK-MK ( n -- zppk:prow ) ZPPK-PROW:MAKE ;
: ZPPK-UN ( zppk:prow -- n ) ZPPK-PROW:UNMAKE ;
s" GEN-PRODUCT-PKG" type cr
\ private products export nothing: no package, no words, no construct form.
package zpsec
private
PRODUCT phid 0
  FIELD v n
;PRODUCT
end-package
s" zpsec" s" phid" TWX-TFAM-FIND-IN TCOK ! TCF !   TCOK @ -1 T=
TCF @ TFAM-VAR-START@ SUMV-CTOR-PKG$ nip 0 T=
s" PS1 ( n -- n ) ZPSEC-PHID:MAKE" CHECK-QUIET-CANDIDATE! 1 T=   \ undefined word -> uncheckable
\ protection: the derived package is closed (reopen/undefine reject), and the
\ generated words stay callable after the rejected attempts.
s" package zpt" TCE-CATCH E-CTOR-PROTECTED T=
s" package ZPT" TCE-CATCH E-CTOR-PROTECTED T=
s" undefine ZPT:MAKE" TCE-CATCH E-CTOR-PROTECTED T=
s" undefine zpt:unmake" TCE-CATCH E-CTOR-PROTECTED T=
: ZPT-MK2 ( n n -- zpt ) ZPT:MAKE ;
s" PRODUCT-PROTECTED" type cr

\ ---------------------------------------------------------------------------
\ report: "ok" on success, nonzero exit on any failure.
\ ---------------------------------------------------------------------------
: REPORT ( -- )
   #FAIL @ 0 = if s" ok" type cr exit then
   #FAIL @ . s" type-ctor-suite: failures" 1 die ;
REPORT
