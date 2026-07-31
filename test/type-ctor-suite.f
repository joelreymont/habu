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
\ counting; their SV.CTOR-NS metadata is still recorded. Private families
\ generate nothing until item 9's construct form.

require test/checker-assert.f
require lib/adt/option.f
require lib/adt/result.f

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

package PROT-WID-RETURN-TEST

variable BEFORE

public

: START ( -- )
   data-base PROT-WID-N-CELL + @ BEFORE ! ;

: FINISH ( -- )
   data-base PROT-WID-N-CELL + @ BEFORE @ 1+ T=
   s" ZRES" XREF-NAMESPACE-WL XREF-FIND-WL
   dup XREF-FOUND? TTRUE
   dup XREF-LEN 0 T=
   dup XREF-FLAGS DNAME-MIN-IN-MASK and 52 rshift NAMESPACE:KIND-TYPE T=
   XREF-START prot-wid? TTRUE ;

;package


\ ---------------------------------------------------------------------------
\ top-level public arity-0 sum: constructors exist, certify, enforce payloads.
\ ---------------------------------------------------------------------------
PROT-WID-RETURN-TEST:START
SUMTYPE zres 0
  VARIANT ok  n ;VARIANT
  VARIANT err n ;VARIANT
;SUMTYPE
PROT-WID-RETURN-TEST:FINISH
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
NEWTYPE zonly 1
;package
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
\ >16-byte escaped ctor package, constructable by readable name
\ (dot habu-raise-or-alias-5d2a6b70). Before the old readability-limit raise
\ (16 -> 32) a presence-slot sum whose escaped package name exceeds 16 got an
\ opaque SHA ctor package (Thex...-TAIL), unwritable in committed source; now it
\ keeps the READABLE escaped name and its constructors are callable by it. This
\ is the real EVID presence-slot casualty shape (EVID-CERTIFY--SLOT = 18 bytes).
\ ---------------------------------------------------------------------------
package evx
public
SUMTYPE certify-slot 0
  VARIANT certify-got n ;VARIANT
  VARIANT certify-none ;VARIANT
;SUMTYPE
private
;package
\ the derived ctor package is the readable escaped name (17 bytes > 16), NOT a SHA fold:
s" evx" s" certify-slot" TWX-TFAM-FIND-IN TCOK ! TCF !   TCOK @ -1 T=
TCF @ TFAM-VAR-START@ SUMV-CTOR-NS$ s" EVX:CERTIFY-SLOT" T$=
\ and the constructors are callable cross-package by that readable name:
: EVX-MK-GOT  ( n -- evx:certify-slot ) EVX-CERTIFY--SLOT:CERTIFY-GOT ;
: EVX-MK-NONE (   -- evx:certify-slot ) EVX-CERTIFY--SLOT:CERTIFY-NONE ;
s" EVX-GET ( n -- evx:certify-slot ) EVX-CERTIFY--SLOT:CERTIFY-GOT"  CHECK-QUIET-CANDIDATE! -1 T=
s" EVX-NON (   -- evx:certify-slot ) EVX-CERTIFY--SLOT:CERTIFY-NONE" CHECK-QUIET-CANDIDATE! -1 T=
\ wrong payload still rejects at the readable call site (no soundness lost):
s" EVX-BAD ( ptr u8 -- evx:certify-slot ) EVX-CERTIFY--SLOT:CERTIFY-GOT" CHECK-QUIET-CANDIDATE! 0 T=
s" GEN-LONG-CTOR" type cr

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
TCF @ TFAM-VAR-START@ SUMV-CTOR-NS$ s" ZPAR" T$=
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
;package
s" zp8" s" zsec" TWX-TFAM-FIND-IN TCOK ! TCF !   TCOK @ -1 T=
TCF @ TFAM-VAR-START@ SUMV-CTOR-NS$ nip 0 T=
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
s" package zok ;package" TCE-CATCH 0 T=
\ undefine of a generated word rejects before retirement...
s" undefine ZRES:OK" TCE-CATCH E-CTOR-PROTECTED T=
s" undefine zres:ok" TCE-CATCH E-CTOR-PROTECTED T=
\ ...and the constructor is still fully usable afterwards.
: ZMK-OK3 ( n -- zres ) ZRES:OK ;
s" UNDEF-SAFE" type cr
\ a new tail is now caught by the native protected-WID wall; seal.f runs
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
;package
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
TCF @ TFAM-VAR-START@ SUMV-CTOR-NS$ s" ZPT" T$=
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
;package
: ZPPK-MK ( n -- zppk:prow ) ZPPK-PROW:MAKE ;
: ZPPK-UN ( zppk:prow -- n ) ZPPK-PROW:UNMAKE ;
s" GEN-PRODUCT-PKG" type cr
\ private products export nothing: no package, no words, no construct form.
package zpsec
private
PRODUCT phid 0
  FIELD v n
;PRODUCT
;package
s" zpsec" s" phid" TWX-TFAM-FIND-IN TCOK ! TCF !   TCOK @ -1 T=
TCF @ TFAM-VAR-START@ SUMV-CTOR-NS$ nip 0 T=
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
\ layout-cap slice 4 (dot habu-checker-capability-layout-9b8540bd): width-aware
\ construct/MATCH lowering. The checker records a per-call-site extra-pad fact
\ (instantiated_pads - declared_pads, WF-XPAD-FLAG) so pass 2 (native
\ EM-ADT-CON-VAR / EM-COMPILE-CALL / EM-ADT-MATCH-OF, gforth mirror) emits the
\ arg-aware pad count. The slice-3 staged reject FLIPS: CLFC1 (generated ctor)
\ and CLFC2 (raw `construct`) now compile AND round-trip; CLFC3 (cell) unchanged.
\ Nested named ADTs land in slice 5 (below). This suite is a candidate-validation
\ `diagnostic` case, so the adversarials' compile-hook stderr is permitted here.
\ ---------------------------------------------------------------------------
PRODUCT clw2 0 FIELD x n FIELD y n ;PRODUCT
SUMTYPE clopt 1 VARIANT none ;VARIANT VARIANT some a ;VARIANT ;SUMTYPE
s" : CLFC1 ( -- clopt<clw2> ) CLOPT:NONE ;" TCE-CATCH 0 T=
s" : CLFC2 ( -- clopt<clw2> ) construct clopt none ;" TCE-CATCH 0 T=
s" : CLFC3 ( n -- clopt<n> ) CLOPT:SOME ;" TCE-CATCH 0 T=
\ end-to-end runtime round-trips: construct a wide bundle, MATCH it back, assert
\ the exact payload cells. Generated ctor (extra=0 SOME, extra=1 NONE), raw
\ `construct` NONE, and MATCH's arg-aware payload skip.
: CLFC-SOME ( n n -- clopt<clw2> ) CLW2:MAKE CLOPT:SOME ;
: CLFC-NONEG ( -- clopt<clw2> ) CLOPT:NONE ;
: CLFC-NONER ( -- clopt<clw2> ) construct clopt none ;
: CLFC-GET ( clopt<clw2> -- n ) MATCH clopt none OF 999 ENDOF some OF CLW2:UNMAKE + ENDOF ;MATCH ;
: CLFC-RT-SOME ( -- n ) 3 4 CLFC-SOME CLFC-GET ;
: CLFC-RT-NONEG ( -- n ) CLFC-NONEG CLFC-GET ;
: CLFC-RT-NONER ( -- n ) CLFC-NONER CLFC-GET ;
CLFC-RT-SOME 7 T=
CLFC-RT-NONEG 999 T=
CLFC-RT-NONER 999 T=
\ wave-B probe shapes as named payload products: result<clw2,n> (W=2 ok payload
\ + cell err) and option<clfs3> (W=3 payload) — the shipping form of the P1/P2/P3
\ raw runs (option<off len>, option<ptr u8 n>, result<n n,n>).
PRODUCT clfs3 0 FIELD a n FIELD b n FIELD c n ;PRODUCT
SUMTYPE clfres 2 VARIANT ok a ;VARIANT VARIANT err b ;VARIANT ;SUMTYPE
: CLFC-ROK ( n n -- clfres<clw2,n> ) CLW2:MAKE CLFRES:OK ;
: CLFC-RERR ( n -- clfres<clw2,n> ) CLFRES:ERR ;
: CLFC-RGET ( clfres<clw2,n> -- n ) MATCH clfres ok OF CLW2:UNMAKE + ENDOF err OF ENDOF ;MATCH ;
: CLFC-RT-OK ( -- n ) 5 6 CLFC-ROK CLFC-RGET ;
: CLFC-RT-ERR ( -- n ) 77 CLFC-RERR CLFC-RGET ;
CLFC-RT-OK 11 T=
CLFC-RT-ERR 77 T=
: CLFC-S3SOME ( n n n -- clopt<clfs3> ) CLFS3:MAKE CLOPT:SOME ;
: CLFC-S3NONE ( -- clopt<clfs3> ) CLOPT:NONE ;
: CLFC-S3GET ( clopt<clfs3> -- n ) MATCH clopt none OF -1 ENDOF some OF CLFS3:UNMAKE + + ENDOF ;MATCH ;
: CLFC-RT-S3 ( -- n ) 1 2 3 CLFC-S3SOME CLFC-S3GET ;
: CLFC-RT-SN ( -- n ) CLFC-S3NONE CLFC-S3GET ;
CLFC-RT-S3 6 T=
CLFC-RT-SN -1 T=
\ ---------------------------------------------------------------------------
\ layout-cap slice 5 (same dot): NESTED named ADTs lower END-TO-END. A named
\ multi-cell instantiation used as the payload arg of ANOTHER family
\ (clopt<clfres<n,n>>) constructs, certifies, lowers, and MATCH-destructures — the
\ slice-3/4 CLFC-NESTED staged reject FLIPS to a real round-trip. Every width site
\ (TFAM-INST-WIDTH@ / TFC-VAR-PAYCELLS / famterm T-WIDTH) already recurses, so the
\ SAME extra-pad model lowers each level: the inner bundle carries its own pads at
\ its own construct site; the outer construct/match adds only the outer delta.
\ ---------------------------------------------------------------------------
\ outer clopt<clfres<n,n>> (W=3) over inner clfres<n,n> (W=2): SOME(ok/err), NONE
\ (generated + raw construct), nested MATCH destructure.
: CN-IOK ( n -- clfres<n,n> ) CLFRES:OK ;
: CN-IERR ( n -- clfres<n,n> ) CLFRES:ERR ;
: CN-SOME ( clfres<n,n> -- clopt<clfres<n,n>> ) CLOPT:SOME ;
: CN-NONE ( -- clopt<clfres<n,n>> ) CLOPT:NONE ;
: CN-NONER ( -- clopt<clfres<n,n>> ) construct clopt none ;
: CN-GET ( clopt<clfres<n,n>> -- n ) MATCH clopt none OF 999 ENDOF some OF MATCH clfres ok OF ENDOF err OF 10 + ENDOF ;MATCH ENDOF ;MATCH ;
: CN-RT-SOMEOK ( -- n ) 5 CN-IOK CN-SOME CN-GET ;
: CN-RT-SOMEERR ( -- n ) 7 CN-IERR CN-SOME CN-GET ;
: CN-RT-NONE ( -- n ) CN-NONE CN-GET ;
: CN-RT-NONER ( -- n ) CN-NONER CN-GET ;
CN-RT-SOMEOK 5 T=
CN-RT-SOMEERR 17 T=
CN-RT-NONE 999 T=
CN-RT-NONER 999 T=
\ deeper: a product leaf inside the nesting (clopt<clfres<clw2,n>>, W=4).
: CND-IOK ( n n -- clfres<clw2,n> ) CLW2:MAKE CLFRES:OK ;
: CND-SOME ( clfres<clw2,n> -- clopt<clfres<clw2,n>> ) CLOPT:SOME ;
: CND-NONE ( -- clopt<clfres<clw2,n>> ) CLOPT:NONE ;
: CND-GET ( clopt<clfres<clw2,n>> -- n ) MATCH clopt none OF 999 ENDOF some OF MATCH clfres ok OF CLW2:UNMAKE + ENDOF err OF ENDOF ;MATCH ENDOF ;MATCH ;
: CND-RT-SOME ( -- n ) 3 4 CND-IOK CND-SOME CND-GET ;
: CND-RT-NONE ( -- n ) CND-NONE CND-GET ;
CND-RT-SOME 7 T=
CND-RT-NONE 999 T=
\ real-compile fail-closed adversarials (rc 70): wrong inner width, cross-family
\ inner, scalar inner, truncated (no inner value). Full diagnostics (E-MISMATCH,
\ arg-aware inner slot render) are pinned in test/type-decl-suite.f.
s" : CN-BADW ( clfres<n,n> -- clopt<clfres<clw2,n>> ) CLOPT:SOME ;" TCE-CATCH 70 T=
s" : CN-BADF ( clopt<n> -- clopt<clfres<n,n>> ) CLOPT:SOME ;" TCE-CATCH 70 T=
s" : CN-BADS ( n -- clopt<clfres<n,n>> ) CLOPT:SOME ;" TCE-CATCH 70 T=
s" : CN-TRUNC ( -- clopt<clfres<n,n>> ) CLOPT:SOME ;" TCE-CATCH 70 T=
\ soundness boundary: an OPEN inner var has an unstable width, so a real compile
\ stays staged fail-closed (CONSTRUCT-WIDE-STAGED-REJECT) rather than lower a
\ width guessed from a=1 cell.
s" : CN-OPEN ( clfres<n,a> -- clopt<clfres<n,a>> ) CLOPT:SOME ;" TCE-CATCH 70 T=
s" LOWER-WIDTH-AWARE-ROUNDTRIP" type cr

\ ---------------------------------------------------------------------------
\ dot habu-fail-closed-on-0ab1e401: ASYMMETRIC growth. The widest-DECLARED variant
\ is not the widest-INSTANTIATED one, so the widest-instantiated variant needs FEWER
\ pad cells than the declared family width reserves (extra pads < 0). Pass-1 lowering
\ keys pads off the DECLARED width, and the add-only pass-2 fact (whose native
\ width-fact certificate requires w >= 1) cannot REMOVE the surplus declared pad, so
\ the certified instantiated width and the only possible lowering permanently DISAGREE.
\ A width contradiction is never a sound value, so it is rejected UNCONDITIONALLY —
\ a real construct/MATCH fails closed (rc 70) AND a CHECK-CANDIDATE probe rejects (0),
\ rather than certify a native bundle wider than its type. This is stronger than the
\ open-var CONSTRUCT-WIDE-STAGED-REJECT (which keeps an unknown-width type nameable):
\ here the width IS known and contradictory. The positive-extra variant of the SAME
\ family + instantiation constructs, and a non-asymmetric instantiation matches — so
\ the reject isolates to the negative extra, not the machinery. (Signed pad
\ corrections would make these lowerable; a separately-tracked deferred capability.)
\ ---------------------------------------------------------------------------
package XPAD-ASYM
public
PRODUCT clx2 0 FIELD x n FIELD y n ;PRODUCT
SUMTYPE clxg 1
  VARIANT wtwo a a ;VARIANT
  VARIANT wthree n n n ;VARIANT
;SUMTYPE
private   \ clx2/clxg stay public (constructed/matched by qualified name); every probe, helper, and the trusted drop leaf are private members of XPAD-ASYM
\ clxg<clx2> (a = width 2): W = 1 + max(wtwo=4, wthree=3) = 5. wtwo extra = -1, wthree = +1.
s" CLXP ( clx2 clx2 -- clxg<clx2> ) construct clxg wtwo" CHECK-QUIET-CANDIDATE! 0 T=            \ width contradiction never certifies, even as a candidate probe
s" : CLXG-BADTWO ( clx2 clx2 -- clxg<clx2> ) construct clxg wtwo ;" TCE-CATCH 70 T=            \ negative-extra construct fails closed
\ The positive-extra variant certifies AND lowers, but the value it makes is
\ PRODUCE-ONLY: no exhaustive MATCH can consume it, because any total MATCH must
\ include the wtwo arm, whose negative-extra unpack fails closed (see CLXG-GET). This
\ is current behavior, documented here; the produce-only policy question is tracked
\ separately by the orchestrator.
s" : CLXG-THREE ( n n n -- clxg<clx2> ) construct clxg wthree ;" TCE-CATCH 0 T=               \ same family + instantiation, positive-extra variant compiles (produce-only, see above)
\ wtwo arm body uses only always-defined words (2drop 0), so the reject can ONLY come
\ from the checker's negative-extra arm reject at 'OF' — not an E-UNDEFINED accessor:
\ inside package XPAD-ASYM the derived ctor package is XPAD-ASYM-CLX2, so a bare
\ CLX2:UNMAKE would itself throw rc 70 and mask the fix (dead probe). This shape flips
\ pre-fix 0 -> post-fix 70 structurally.
s" : CLXG-GET ( clxg<clx2> -- n ) MATCH clxg wtwo OF 2drop 0 ENDOF wthree OF + + ENDOF ;MATCH ;" TCE-CATCH 70 T=   \ MATCH's negative-extra arm fails closed at 'OF'
s" : CLXN-GET ( clxg<n> -- n ) MATCH clxg wtwo OF + ENDOF wthree OF + + ENDOF ;MATCH ;" TCE-CATCH 0 T=   \ non-asymmetric clxg<n>: same machinery matches clean
\ Runtime depth parity for POSITIVE extra. Negative extra never certifies (the
\ certify-time reject above discharges its runtime obligation); positive-extra
\ clxg<clx2> cannot be consumed by any checked exhaustive MATCH, so pass-2 magnitude
\ is verified by measuring the native cell footprint of the certified CLXG-THREE
\ bundle. XPAD-WTHREE-W snapshots the stack depth, builds the bundle through the
\ checked CLXG-THREE, and reads the depth delta; it MUST equal the certified width 5
\ (pass-1 declared-width lowering emits 4 cells, pass-2 WF-XPAD-FLAG adds the one
\ extra pad). Any wrong pass-2 magnitude (4 or 6) flips this assert. It MUST build
\ through the checked-compiled CLXG-THREE — an unchecked in-word `construct` skips
\ pass-2 and reads 4. Everything here is checked except TWX-XPAD-DROP-BUNDLE, whose
\ only job is to drop the measured multi-cell layout value — the one operation the
\ checker cannot express — with a straight-line 2drop 2drop drop over the certified
\ width; its declared ( clxg<clx2> n -- n ) effect keeps the whole call site checked.
variable XPAD-D0                                              \ data-stack depth snapshot taken before the build
: XPAD-MARK ( -- ) depth XPAD-D0 ! ;                         \ checked: snapshot the baseline depth
: XPAD-DELTA ( -- n ) depth XPAD-D0 @ - ;                    \ checked: cells the build added to the stack
TRUSTED: TWX-XPAD-DROP-BUNDLE ( clxg<clx2> n -- n )          \ trusted leaf: drop the measured layout value, keep the count
   >r 2drop 2drop drop r> ;
: XPAD-WTHREE-W ( -- n )                                     \ native cell footprint of the certified clxg<clx2> wthree bundle
   XPAD-MARK 1 2 3 CLXG-THREE XPAD-DELTA TWX-XPAD-DROP-BUNDLE ;
XPAD-WTHREE-W 5 T=
s" LOWER-WIDTH-ASYM-FAILCLOSED" type cr
;package

\ ---------------------------------------------------------------------------
\ dot habu-fail-closed-on-0ab1e401 (tagless pad arithmetic): the negative-extra reject is
\ decided by KIND-CORRECT pad arithmetic. A STRUCTURE/PRODUCT is TAGLESS — it has one
\ product shape and its whole width is payload; only a SUM/ENUM carries a tag cell.
\ Subtracting a tag from a tagless family was the tagless-family regression: a wide parametric structure
\ reported a spurious extra = -1 and was wrongly rejected. With the tag cell subtracted only
\ for tagged families, a wide product's extra is 0 and it certifies with no exemption. A
\ tagged SUM whose non-widest variant needs FEWER cells than the declared family reserves is
\ still a genuine contradiction and fails closed (until signed pass-2, dot
\ habu-signed-pass-2-4fc2b960) — including the INSTANTIATED-TIE case, where the verdict is
\ ORDER-INDEPENDENT (both declaration orders reject), unlike the order-dependent
\ accept/reject a widest-variant discriminator gave. Families use string-eval so the derived
\ accessors resolve at global scope; the runtime value/width check lives in package XPAD-TAGLESS.
\ ---------------------------------------------------------------------------
\ (a) POSITIVE: a parametric STRUCTURE widens with its argument and certifies (extra is now 0,
\ not the old spurious -1 — if the reject still fired, XPGWIDE would read 0). A COMPILED
\ generated constructor at the wide instantiation lowers to an exact bundle: XPGW-RT builds it
\ through XPG:MAKE, and TWX-XPG-CHECK reads the bundle's cells top->bottom (z=3, b=2, a=1) and
\ asserts each value in declaration order. The three value checks prove content and order; the
\ 5551 canary below the build, asserted equal afterward, proves the leaf consumed EXACTLY the
\ bundle (a wrong width or mis-sized consume shifts it) — value+canary parity is the
\ content-and-width proof. A checked MAKE->UNMAKE value round-trip (the SCOUTR precedent,
\ structure-certify-suite.f:118-136) is NOT expressible for a GENERIC family at a wide
\ instantiation, so this trusted read is the only runtime proof: SCOUTR is a CONCRETE (arity-0)
\ structure whose wide field is fixed, so its MAKE/UNMAKE effects are concrete and the wide
\ UNMAKE lowers through the arity-0 hidden-field expansion. xpg is GENERIC (arity 1):
\ structure-make.f generates make(row 0) and unmake(row 1) over the same open-param field
\ schema, a FIXED one-cell-per-param stored effect. Construct has an arg-aware escape —
\ layout-cap slice 3 (checker.f CTOR-STEP-XT) routes a generated ctor whose declared OUTPUT
\ instantiates the param at a multi-cell arg through the width-aware construct step, so the
\ OUTPUT-annotated wide XPG:MAKE certifies. UNMAKE has NO such escape: its stored effect is fixed
\ at one cell per open parameter, and no arg-aware lane instantiates it wide, so `XPG:UNMAKE` on
\ a concrete `xpg<xpginr>` rejects at the widened field slot — an independent generic-wide UNMAKE
\ checker gap tracked by dot habu-instantiate-wide-generic-075aced1. Hence the trusted leaf
\ TWX-XPG-CHECK reads the cells directly.
s" STRUCTURE xpginr 0 FIELD a n FIELD b n ;STRUCTURE" TCE-CATCH 0 T=          \ width-2 structure leaf
s" STRUCTURE xpg 1 FIELD u a FIELD z n ;STRUCTURE" TCE-CATCH 0 T=             \ parametric structure: payload widens with a
s" XPGWIDE ( xpginr n -- xpg<xpginr> ) XPG:MAKE" CHECK-QUIET-CANDIDATE! -1 T= \ wide instantiation certifies (was rejected before the tagless-arithmetic fix)
s" XPGFLAT ( n n n -- xpg<xpginr> ) XPG:MAKE" CHECK-QUIET-CANDIDATE! 0 T=      \ flattened (a b z) is not (xpginr z): still fail-closed
s" XPGCONC ( n n -- xpg<n> ) XPG:MAKE" CHECK-QUIET-CANDIDATE! -1 T=            \ concrete non-widening instantiation certifies
package XPAD-TAGLESS
: XPGW-MK ( xpginr n -- xpg<xpginr> ) XPG:MAKE ;              \ compiled generated STRUCTURE constructor at the wide instantiation
TRUSTED: TWX-XPG-CHECK ( xpg<xpginr> -- )                    \ trusted leaf: read the wide bundle's cells top->bottom (z, b, a) and assert their values in declaration order
   3 T= 2 T= 1 T= ;
: XPGW-RT ( -- )                                             \ build the certified wide bundle and verify its content + exact width
   5551 1 2 XPGINR:MAKE 3 XPGW-MK TWX-XPG-CHECK 5551 T= ;
XPGW-RT
;package
\ (b) NEGATIVE: genuine tagged-sum contradictions that fail closed (candidate 0 / rc 70). Each
\ is exercised through the REAL generated variant constructor (FAM:VARIANT), proven resolvable
\ by a concrete instantiation that certifies — an undefined word would itself be rc 70, so the
\ concrete rc 0 shows the wide rc 70 is a genuine negative-extra reject, not an undefined-word
\ miss. Signed pass-2 (dot habu-signed-pass-2-4fc2b960) will flip these to exact-width forms.
s" PRODUCT xw2 0 FIELD x n FIELD y n ;PRODUCT" TCE-CATCH 0 T=
\ xst<xw2>: stable(n n n, declared 3) is sole argmax both ways; grow(a, declared 1) instantiates
\ to 2 -> extra(grow) = (3-2) - (3-1) = -1 (a non-widest variant that grows).
s" SUMTYPE xst 1 VARIANT stable n n n ;VARIANT VARIANT grow a ;VARIANT ;SUMTYPE" TCE-CATCH 0 T=
s" XSTC ( xw2 -- xst<xw2> ) construct xst grow" CHECK-QUIET-CANDIDATE! 0 T=    \ reserved-construct candidate fails closed
s" : XSTG-CONC ( n -- xst<n> ) XST:GROW ;" TCE-CATCH 0 T=                      \ generated ctor XST:GROW resolves: concrete instantiation certifies
s" : XSTG-WIDE ( xw2 -- xst<xw2> ) XST:GROW ;" TCE-CATCH 70 T=                 \ compiled generated ctor at the wide instantiation fails closed
s" : XSTM ( xst<xw2> -- n ) MATCH xst grow OF 2drop 0 ENDOF stable OF + + ENDOF ;MATCH ;" TCE-CATCH 70 T=   \ MATCH grow arm fails closed at 'OF'
\ tie: tbig wbig(n n n n, declared 4) / wsm(a a, declared 2) at xw2 -> both instantiate to 4
\ (INSTANTIATED TIE); extra(wsm) = (4-4) - (4-2) = -2.
s" SUMTYPE tbig 1 VARIANT wbig n n n n ;VARIANT VARIANT wsm a a ;VARIANT ;SUMTYPE" TCE-CATCH 0 T=
s" TBC ( xw2 xw2 -- tbig<xw2> ) construct tbig wsm" CHECK-QUIET-CANDIDATE! 0 T= \ reserved-construct candidate fails closed
s" : TBWG-CONC ( n n -- tbig<n> ) TBIG:WSM ;" TCE-CATCH 0 T=                   \ generated ctor TBIG:WSM resolves: concrete instantiation certifies
s" : TBWG-WIDE ( xw2 xw2 -- tbig<xw2> ) TBIG:WSM ;" TCE-CATCH 70 T=            \ compiled generated ctor at the instantiated tie fails closed
s" : TBM ( tbig<xw2> -- n ) MATCH tbig wsm OF 2drop 2drop 0 ENDOF wbig OF + + + ENDOF ;MATCH ;" TCE-CATCH 70 T=   \ tie MATCH arm fails closed
\ order-independence pin: the SAME tie family with variants declared in the opposite order fails
\ closed identically through BOTH the generated constructor and MATCH (a widest-variant
\ discriminator flipped on declaration order).
s" SUMTYPE tbig2 1 VARIANT wsm2 a a ;VARIANT VARIANT wbig2 n n n n ;VARIANT ;SUMTYPE" TCE-CATCH 0 T=
s" TB2C ( xw2 xw2 -- tbig2<xw2> ) construct tbig2 wsm2" CHECK-QUIET-CANDIDATE! 0 T=   \ order-swapped reserved-construct candidate fails closed
s" : TB2G-WIDE ( xw2 xw2 -- tbig2<xw2> ) TBIG2:WSM2 ;" TCE-CATCH 70 T=          \ order-swapped generated ctor: identical reject
s" : TB2M ( tbig2<xw2> -- n ) MATCH tbig2 wsm2 OF 2drop 2drop 0 ENDOF wbig2 OF + + + ENDOF ;MATCH ;" TCE-CATCH 70 T=   \ order-swapped MATCH arm: identical reject
s" TAGLESS-ARITH-KEEPS-FAILCLOSED" type cr

\ ---------------------------------------------------------------------------
\ dot habu-universal-enum-parametric-ad011c21: a parametric family APPLICATION
\ and a single-effect QUOTATION as variant payloads construct and MATCH. The
\ nested application rides the landed width-aware construct/MATCH lowering; the
\ quotation payload is one xt cell recovered as its T-QUOT effect in the arm.
\ ---------------------------------------------------------------------------
SUMTYPE pqinn 1 VARIANT yes a ;VARIANT VARIANT no ;VARIANT ;SUMTYPE
SUMTYPE pqhold 0 VARIANT hh pqinn<n> ;VARIANT ;SUMTYPE
: PQ-MK ( n -- pqhold ) PQINN:YES PQHOLD:HH ;
: PQ-GET ( pqhold -- n ) MATCH pqhold hh OF MATCH pqinn yes OF ENDOF no OF 0 ENDOF ;MATCH ENDOF ;MATCH ;
: PQ-RT ( -- n ) 7 PQ-MK PQ-GET ;
PQ-RT 7 T=
SUMTYPE pqact 0 VARIANT run [ n -- n ] ;VARIANT VARIANT nop ;VARIANT ;SUMTYPE
: PQ-APP ( pqact -- n ) MATCH pqact run OF 5 swap execute ENDOF nop OF 0 ENDOF ;MATCH ;
: PQ-RUN ( -- n ) [: 1 + ;] PQACT:RUN PQ-APP ;
PQ-RUN 6 T=
\ a wrong-effect quotation cannot be stored where [ n -- n ] is required (rc 70).
s" : PQ-BADQ ( -- pqact ) [: 0 0= ;] PQACT:RUN ;" TCE-CATCH 70 T=

\ dot habu-sc-quot-full-db4d0518: full effect ROWS per quotation side. A multi-type
\ input side, an empty input side, an empty output side, and a multi-type input (with
\ a ptr element) plus an explicit return clause each construct, MATCH, and execute.
\ [ n n -- n ]: the arm feeds two inputs and executes the recovered xt.
SUMTYPE pq2 0 VARIANT run [ n n -- n ] ;VARIANT VARIANT nop ;VARIANT ;SUMTYPE
: PQ2-APP ( pq2 -- n ) MATCH pq2 run OF 3 4 rot execute ENDOF nop OF 0 ENDOF ;MATCH ;
: PQ2-RUN ( -- n ) [: + ;] PQ2:RUN PQ2-APP ;
PQ2-RUN 7 T=
\ wrong effect ([ n -- n ], not [ n n -- n ]) cannot be stored at the construct site.
s" : PQ2-BADQ ( -- pq2 ) [: 1 + ;] PQ2:RUN ;" TCE-CATCH 70 T=
\ [ -- n ]: empty input side; the arm executes the xt with no inputs.
SUMTYPE pq0 0 VARIANT run [ -- n ] ;VARIANT VARIANT nop ;VARIANT ;SUMTYPE
: PQ0-APP ( pq0 -- n ) MATCH pq0 run OF execute ENDOF nop OF 0 ENDOF ;MATCH ;
: PQ0-RUN ( -- n ) [: 9 ;] PQ0:RUN PQ0-APP ;
PQ0-RUN 9 T=
\ [ n -- ]: empty output side; the xt consumes its input into a sink for observation.
variable PQD-SINK
SUMTYPE pqd 0 VARIANT run [ n -- ] ;VARIANT VARIANT nop ;VARIANT ;SUMTYPE
: PQD-APP ( pqd -- n ) MATCH pqd run OF 5 swap execute PQD-SINK @ ENDOF nop OF 0 ENDOF ;MATCH ;
: PQD-RUN ( -- n ) 0 PQD-SINK ! [: PQD-SINK ! ;] PQD:RUN PQD-APP ;
PQD-RUN 5 T=
\ [ n ptr u8 -- n | n -- n ]: multi-type input with a ptr element and an explicit
\ return clause; the arm sets up the return cell, executes, and recovers both sides.
create PQM-BUF 4 allot  3 PQM-BUF c!
SUMTYPE pqm 0 VARIANT run [ n ptr u8 -- n | n -- n ] ;VARIANT VARIANT nop ;VARIANT ;SUMTYPE
: PQM-APP ( pqm -- n ) MATCH pqm run OF 5 PQM-BUF 100 >r rot execute r> + ENDOF nop OF 0 ENDOF ;MATCH ;
: PQM-RUN ( -- n ) [: drop r> swap >r ;] PQM:RUN PQM-APP ;
PQM-RUN 105 T=
s" PARAMETRIC-QUOT-PAYLOAD" type cr

\ ---------------------------------------------------------------------------
\ dot habu-pass-constructor-family-b9402f5b: the generator body is told WHICH
\ family to generate instead of reading the last-registered one, so a caller can
\ generate a family that is not the last one declared. The SUMTYPE definer runs
\ registration and generation in one transaction; this test splits them:
\ register two families through the shared CHECKER-DEFSUM path, each in its own
\ real declaration transaction, then generate the FIRST one and prove the whole
\ generated plan is that family's. The last-registered family is the second one,
\ so a generator that still read the register would publish ZXB:TWO here
\ instead of ZXA:ONE.
\ ---------------------------------------------------------------------------
package CTOR-FAMILY-TEST

\ whitebox boundary (dot habu-hb-crash-bare-c5be6634): the engine-private
\ registration path, generator, and plan buffer go through named trusted shims.
TRUSTED: DEFSUM ( ptr u8 n ptr u8 n -- ) CHECKER-DEFSUM ;
\ the generator now takes the payload provider its caller chooses; this family
\ is already published, so it gets the committed one the legacy definers use.
TRUSTED: CTOR-BODY ( n -- n ) {: fam:n :}
   TDECL-SUMV-PROVIDER fam TDECL-CTOR-WORDS-BODY ;
TRUSTED: PLAN-N ( -- n ) TDPLAN-N @ ;
TRUSTED: PLAN-NAME$ ( n -- ptr u8 n ) TDPLAN-NAME$ ;
TRUSTED: FAM-REG ( -- n ) TDECL-FAM-REG @ ;

variable FAM-A    \ the family registered first, then left behind by the register
variable FAM-B    \ the family registered last, the one the register still names
variable ASKED    \ family id handed to the generator
variable GOT      \ family id the generator handed back

: REGISTER-A ( -- ) s" zxa" s" 0 VARIANT one n ;VARIANT" DEFSUM ;
: REGISTER-B ( -- ) s" zxb" s" 0 VARIANT two n ;VARIANT" DEFSUM ;
: DECLARE-A ( -- ) [: REGISTER-A ;] GENERATED-DECL:RUN  FAM-REG FAM-A ! ;
: DECLARE-B ( -- ) [: REGISTER-B ;] GENERATED-DECL:RUN  FAM-REG FAM-B ! ;
: GENERATE-BODY ( -- ) ASKED @ CTOR-BODY GOT ! ;

public

\ DECLARE registers both families and generates neither, so a live family that
\ is NOT the one the register names exists to generate.
: DECLARE ( -- ) DECLARE-A DECLARE-B ;
: FIRST ( -- n ) FAM-A @ ;
: SECOND ( -- n ) FAM-B @ ;
: GENERATE ( n -- n ) ASKED ! [: GENERATE-BODY ;] GENERATED-DECL:RUN  GOT @ ;
: PLAN-COUNT ( -- n ) PLAN-N ;
: PLAN-NAME ( n -- ptr u8 n ) PLAN-NAME$ ;

;package

CTOR-FAMILY-TEST:DECLARE
CTOR-FAMILY-TEST:FIRST 0 < 0= -1 T=
\ zxa is live, and is no longer the family the register names.
CTOR-FAMILY-TEST:SECOND CTOR-FAMILY-TEST:FIRST <> -1 T=
\ generate the NON-LAST family: the id comes back unchanged and the whole plan
\ is zxa's single constructor.
CTOR-FAMILY-TEST:FIRST CTOR-FAMILY-TEST:GENERATE CTOR-FAMILY-TEST:FIRST T=
CTOR-FAMILY-TEST:PLAN-COUNT 1 T=
0 CTOR-FAMILY-TEST:PLAN-NAME s" ZXA:ONE" T$=
\ only that family was published: zxa's constructor certifies, while zxb's
\ constructor word does not exist yet (verdict 1 = uncheckable/undefined).
s" ZQ1 ( n -- zxa ) ZXA:ONE" CHECK-QUIET-CANDIDATE! -1 T=
s" ZQ2 ( n -- zxb ) ZXB:TWO" CHECK-QUIET-CANDIDATE! 1 T=
\ the still-ungenerated family generates afterwards through the same call.
CTOR-FAMILY-TEST:SECOND CTOR-FAMILY-TEST:GENERATE CTOR-FAMILY-TEST:SECOND T=
CTOR-FAMILY-TEST:PLAN-COUNT 1 T=
0 CTOR-FAMILY-TEST:PLAN-NAME s" ZXB:TWO" T$=
s" ZQ3 ( n -- zxb ) ZXB:TWO" CHECK-QUIET-CANDIDATE! -1 T=

s" EXPLICIT-CTOR-FAMILY" type cr

\ ---------------------------------------------------------------------------
\ dot habu-constructor-pass-payload-78c7069d: the shared constructor and DERIVE
\ renderer no longer reads payload metadata itself. Its caller hands it a
\ context cell and three quotation capabilities, and payload count, declaration
\ order schema root, and payload cell width are read only through those.
\ Three things are proved here.
\  1. A provider that deliberately disagrees with the committed metadata -- it
\     swaps the two variants' payload views -- changes the checked effect of the
\     generated constructors. A renderer that still read the variant rows itself
\     would publish the committed effects and fail these cases.
\  2. A provider whose answers cannot be true of the family is rejected with the
\     named E-TDECL-PROVIDER before any text is generated: a negative count, an
\     unknown schema root, a cell width that contradicts the schema roots the
\     same provider returned, and a cell width wider than the family's payload
\     slots. The family still generates normally afterwards, so a rejected
\     provider leaves nothing behind.
\  3. A provider over a LIVE declaration token renders a payload constructor
\     inside the transaction that declared it. The committed provider cannot:
\     its reads are bounded by the COMMITTED field high-water and throw
\     E-TFAM-PAYLOAD (type-family.f). That is the wall the unified ENUM front
\     end has to clear; this is the seam's proof that a live provider clears it.
\     Wiring that front end to the generator is a later leaf and is deliberately
\     not done here -- nothing below publishes a word from the live path.
\ ---------------------------------------------------------------------------
package CTOR-PAYPROV-TEST

7132 constant E-COMMITTED-PAYLOAD   \ type-family.f E-TFAM-PAYLOAD
7133 constant E-PROVIDER            \ sumtype.f E-TDECL-PROVIDER

\ whitebox boundary (dot habu-hb-crash-bare-c5be6634): the engine-private
\ registration, event, generator, and plan words go through named trusted shims.
\ The generator's three payload capabilities carry their exact effects across
\ those boundaries, so the checker types every provider this suite builds.
TRUSTED: SUM-DECL ( ptr u8 n ptr u8 n -- ) CHECKER-DEFSUM ;
TRUSTED: PROD-DECL ( ptr u8 n ptr u8 n -- ) CHECKER-DEFPRODUCT ;
TRUSTED: LAST-FAM ( -- n ) TDECL-FAM-REG @ ;
TRUSTED: GEN-FAMILY ( n [ n n n -- n ] [ n n n n -- n ] [ n n n -- n ] n -- n )
   TDECL-CTOR-WORDS-BODY ;
TRUSTED: CAPTURE ( n [ n n n -- n ] [ n n n n -- n ] [ n n n -- n ] n -- ) TDPV-CAPTURE ;
TRUSTED: RENDER-CTOR ( n n -- ) TDECL-CTOR-WORD ;
TRUSTED: SUMV-PROV ( -- n [ n n n -- n ] [ n n n n -- n ] [ n n n -- n ] )
   TDECL-SUMV-PROVIDER ;
TRUSTED: PAY-COUNT ( n -- n ) SUMV-PAY-N ;
TRUSTED: PAY-ROOT ( n n -- n ) SUMV-PAY-ROOT ;
TRUSTED: PAY-CELLS ( n -- n ) SUMV-PAYCELLS@ ;
TRUSTED: ROOT-N ( -- n ) SCHEMA-ROOT-N@ ;
TRUSTED: VAR-START ( n -- n ) TFAM-VAR-START@ ;
TRUSTED: FAM-DECL ( ptr u8 n n ptr u8 n n n -- n ) TFAM-DECL ;
TRUSTED: PKG-PUBLIC ( -- n ) CHECKER-PACKAGE-PUBLIC ;
TRUSTED: SUM-KIND ( -- n ) TK-SUM ;
TRUSTED: CON-CODE ( ptr u8 n -- n ) CON-OF ;
TRUSTED: SCH-CON ( n -- n ) SCHEMA-CON ;
TRUSTED: SCH-ROOT+ ( n -- n ) SCHEMA-ROOT+ ;
TRUSTED: SUMV-COUNT ( -- n ) SUMV-N @ ;
TRUSTED: FLD-COUNT ( -- n ) TYPE-FIELD:COUNT ;
TRUSTED: CELL-BYTES ( -- n ) CELL ;
TRUSTED: VAR-RANGE! ( n n n -- ) TFAM-VAR-RANGE! ;
TRUSTED: FLD-RANGE! ( n n n -- ) TFAM-FLD-RANGE! ;
TRUSTED: SLOTS! ( n n -- ) TFAM-SLOTS! ;
TRUSTED: CTOR-PUBLISH ( n n n -- ) TDECL-CTOR-PUBLISH ;
TRUSTED: PEND-CLEAR ( -- ) CTOR-PEND-CLEAR ;
TRUSTED: CAND-START ( -- ) CHECK-CANDIDATE-START ;
TRUSTED: CAND-DONE ( n -- n ) CHECK-CANDIDATE-DONE ;
TRUSTED: PLAN-BEGIN ( -- ) TDPLAN-BEGIN ;
TRUSTED: PLAN-ROWS ( -- n ) TDPLAN-N @ ;
TRUSTED: PLAN-WORD$ ( n -- ptr u8 n ) TDPLAN-NAME$ ;
TRUSTED: PLAN-DEF$ ( n -- ptr u8 n ) TDPLAN-DEF$ ;

\ --- provider 1: coherent, and deliberately not the committed view. It answers
\ every question about one variant with the OTHER variant's committed payload.
variable SWAP-A   variable SWAP-B
variable SWAP-FAM   variable HOSTILE-FAM   variable PROD-FAM
variable FSHORT-FAM   variable FLONG-FAM
: SWAPPED ( n -- n ) {: vid:n :}
   vid SWAP-A @ = IF SWAP-B @ EXIT THEN SWAP-A @ ;
: SWAP-N ( n n n -- n ) {: ctx:n fam:n vid:n :} vid SWAPPED PAY-COUNT ;
: SWAP-ROOT ( n n n n -- n ) {: ctx:n fam:n vid:n j:n :} vid SWAPPED j PAY-ROOT ;
: SWAP-CELLS ( n n n -- n ) {: ctx:n fam:n vid:n :} vid SWAPPED PAY-CELLS ;
: SWAP-PROV ( -- n [ n n n -- n ] [ n n n n -- n ] [ n n n -- n ] )
   0 [: SWAP-N ;] [: SWAP-ROOT ;] [: SWAP-CELLS ;] ;

\ --- providers 2: each one answer that cannot be true of the family under test.
\ TRUE-* answer honestly so exactly one capability is hostile per fixture. Every
\ capability takes the renderer's full argument list even where it ignores it,
\ because that list is the contract the renderer calls all three through.
variable DONOR                          \ a wider variant, borrowed by WIDE-PROV
: TRUE-N ( n n n -- n ) {: ctx:n fam:n vid:n :} vid PAY-COUNT ;
: TRUE-ROOT ( n n n n -- n ) {: ctx:n fam:n vid:n j:n :} vid j PAY-ROOT ;
: TRUE-CELLS ( n n n -- n ) {: ctx:n fam:n vid:n :} vid PAY-CELLS ;
: NEG-N ( n n n -- n ) {: ctx:n fam:n vid:n :} -1 ;
: ZERO-CELLS ( n n n -- n ) {: ctx:n fam:n vid:n :} 0 ;
: PAST-ROOT ( n n n n -- n ) {: ctx:n fam:n vid:n j:n :} ROOT-N ;
: LEAN-CELLS ( n n n -- n ) {: ctx:n fam:n vid:n :} vid PAY-CELLS 1 - ;
: DONOR-N ( n n n -- n ) {: ctx:n fam:n vid:n :} DONOR @ PAY-COUNT ;
: DONOR-ROOT ( n n n n -- n ) {: ctx:n fam:n vid:n j:n :} DONOR @ j PAY-ROOT ;
: DONOR-CELLS ( n n n -- n ) {: ctx:n fam:n vid:n :} DONOR @ PAY-CELLS ;
: NEG-PROV ( -- n [ n n n -- n ] [ n n n n -- n ] [ n n n -- n ] )
   0 [: NEG-N ;]   [: TRUE-ROOT ;] [: ZERO-CELLS ;] ;
: ROOT-PROV ( -- n [ n n n -- n ] [ n n n n -- n ] [ n n n -- n ] )
   0 [: TRUE-N ;]  [: PAST-ROOT ;] [: TRUE-CELLS ;] ;
: LEAN-PROV ( -- n [ n n n -- n ] [ n n n n -- n ] [ n n n -- n ] )
   0 [: TRUE-N ;]  [: TRUE-ROOT ;] [: LEAN-CELLS ;] ;
: WIDE-PROV ( -- n [ n n n -- n ] [ n n n n -- n ] [ n n n -- n ] )
   0 [: DONOR-N ;] [: DONOR-ROOT ;] [: DONOR-CELLS ;] ;

\ --- providers 3: answers that CHANGE after the first call, and a count with no
\ relation to the family at all. The snapshot is what defeats the first two: the
\ generator asks each capability once per variant, before any text exists, so a
\ later answer has nothing left to corrupt. FLIP-* count their own calls and
\ report the truth once, then a wrong arity forever after.
variable FLIP-N-CALLS   variable FLIP-VID   variable FLIP-SEEN
: FLIP-BASE ( n -- n ) {: vid:n :}   \ count the calls made about THIS variant
   vid FLIP-VID @ <> IF vid FLIP-VID !  0 FLIP-SEEN ! THEN
   FLIP-SEEN @ 1 + FLIP-SEEN !
   FLIP-N-CALLS @ 1 + FLIP-N-CALLS !
   vid PAY-COUNT ;
: FLIP-SHORT-N ( n n n -- n ) {: ctx:n fam:n vid:n :}
   vid FLIP-BASE {: k:n :}
   FLIP-SEEN @ 1 > IF k 1 - EXIT THEN k ;
: FLIP-LONG-N ( n n n -- n ) {: ctx:n fam:n vid:n :}
   vid FLIP-BASE {: k:n :}
   FLIP-SEEN @ 1 > IF k 1 + EXIT THEN k ;
: HUGE-N ( n n n -- n ) {: ctx:n fam:n vid:n :} 3000 ;
: FLIP-SHORT-PROV ( -- n [ n n n -- n ] [ n n n n -- n ] [ n n n -- n ] )
   0 [: FLIP-SHORT-N ;] [: TRUE-ROOT ;] [: TRUE-CELLS ;] ;
: FLIP-LONG-PROV ( -- n [ n n n -- n ] [ n n n n -- n ] [ n n n -- n ] )
   0 [: FLIP-LONG-N ;] [: TRUE-ROOT ;] [: TRUE-CELLS ;] ;
: HUGE-PROV ( -- n [ n n n -- n ] [ n n n n -- n ] [ n n n -- n ] )
   0 [: HUGE-N ;] [: TRUE-ROOT ;] [: TRUE-CELLS ;] ;

\ --- provider 4: the live declaration token's own payload view.
variable LTOK
: LIVE-N ( n n n -- n ) {: ctx:n fam:n vid:n :} ctx fam vid DECL-EVENT:PAYLOAD-N ;
: LIVE-ROOT ( n n n n -- n ) {: ctx:n fam:n vid:n j:n :}
   ctx fam vid j DECL-EVENT:PAYLOAD-SCHEMA@ ;
: LIVE-CELLS ( n n n -- n ) {: ctx:n fam:n vid:n :} ctx fam vid DECL-EVENT:PAYLOAD-CELLS ;
: LIVE-PROV ( -- n [ n n n -- n ] [ n n n n -- n ] [ n n n -- n ] )
   LTOK @ [: LIVE-N ;] [: LIVE-ROOT ;] [: LIVE-CELLS ;] ;

\ --- generation drivers. Each fixture builds its own provider INSIDE the
\ transaction body, because a quotation cannot read its caller's locals and a
\ variable would drop the capability's effect; only the family id travels in a
\ cell. GOT proves the generator handed that same family back.
variable ASKED   variable GOT
: SWAP-BODY ( -- )      SWAP-PROV ASKED @ GEN-FAMILY GOT ! ;
: COMMITTED-BODY ( -- ) SUMV-PROV ASKED @ GEN-FAMILY GOT ! ;
: NEG-BODY ( -- )       NEG-PROV  ASKED @ GEN-FAMILY GOT ! ;
: PNEG-BODY ( -- )      NEG-PROV  PROD-FAM @ GEN-FAMILY GOT ! ;
: ROOT-BODY ( -- )      ROOT-PROV ASKED @ GEN-FAMILY GOT ! ;
: LEAN-BODY ( -- )      LEAN-PROV ASKED @ GEN-FAMILY GOT ! ;
: WIDE-BODY ( -- )      WIDE-PROV ASKED @ GEN-FAMILY GOT ! ;
: FSHORT-BODY ( -- )    FLIP-SHORT-PROV ASKED @ GEN-FAMILY GOT ! ;
: FLONG-BODY ( -- )     FLIP-LONG-PROV  ASKED @ GEN-FAMILY GOT ! ;
: HUGE-BODY ( -- )      HUGE-PROV  PROD-FAM @ GEN-FAMILY GOT ! ;
: SWAP-RUN ( -- )      [: SWAP-BODY ;] GENERATED-DECL:RUN ;
: COMMITTED-RUN ( -- ) [: COMMITTED-BODY ;] GENERATED-DECL:RUN ;
: NEG-RUN ( -- )       [: NEG-BODY ;] GENERATED-DECL:RUN ;
: PNEG-RUN ( -- )      [: PNEG-BODY ;] GENERATED-DECL:RUN ;
: ROOT-RUN ( -- )      [: ROOT-BODY ;] GENERATED-DECL:RUN ;
: LEAN-RUN ( -- )      [: LEAN-BODY ;] GENERATED-DECL:RUN ;
: WIDE-RUN ( -- )      [: WIDE-BODY ;] GENERATED-DECL:RUN ;
: FSHORT-RUN ( -- )    [: FSHORT-BODY ;] GENERATED-DECL:RUN ;
: FLONG-RUN ( -- )     [: FLONG-BODY ;] GENERATED-DECL:RUN ;
: HUGE-RUN ( -- )      [: HUGE-BODY ;] GENERATED-DECL:RUN ;

\ --- the live declaration, driven straight through the shared event
\ transaction exactly as the ENUM front end drives it, then rolled back.
variable LFAM   variable LVID   variable LROOT
variable LVBASE  variable LFBASE   variable LROWS   variable LCOUNT
: LIVE-OPEN ( -- )
   SUMV-COUNT LVBASE !
   FLD-COUNT LFBASE !
   DECL-EVENT:OPEN LTOK !
   LTOK @ LFAM @ DECL-EVENT:DECL LTOK !
   LTOK @ LFAM @ 0 DECL-EVENT:ARITY LTOK !
   LTOK @ LFAM @ s" one" DECL-EVENT:VARIANT LTOK !
   DECL-EVENT:CURRENT-VARIANT LVID !
   LTOK @ LFAM @ s" a" LROOT @ 0 1 0 CELL-BYTES CELL-BYTES 0 DECL-EVENT:FIELD LTOK !
   LTOK @ LFAM @ DECL-EVENT:END-VARIANT LTOK !
   LFAM @ LVBASE @ 1 VAR-RANGE!
   LFAM @ LFBASE @ 1 FLD-RANGE!
   LFAM @ 1 SLOTS!
   LFAM @ LVBASE @ 1 CTOR-PUBLISH ;
: LIVE-RENDER ( -- )       \ render only: publication is the ENUM wiring leaf's
   PLAN-BEGIN
   LIVE-PROV LFAM @ CAPTURE
   LFAM @ LVID @ RENDER-CTOR ;
: COMMITTED-RENDER ( -- )
   PLAN-BEGIN
   SUMV-PROV LFAM @ CAPTURE
   LFAM @ LVID @ RENDER-CTOR ;

\ the snapshot is module state, so a render word that reaches outside the
\ family that was captured must fail closed rather than read a stale row.
: STALE-BODY ( -- )
   PLAN-BEGIN
   SUMV-PROV HOSTILE-FAM @ CAPTURE
   SWAP-FAM @ SWAP-A @ RENDER-CTOR ;
: OVERRUN-BODY ( -- )   \ the right family, a variant row it never captured
   PLAN-BEGIN
   SUMV-PROV SWAP-FAM @ CAPTURE
   SWAP-FAM @ SWAP-A @ 9 + RENDER-CTOR ;

public

\ register zps and zph without generating either, and note the payload rows the
\ hostile providers borrow.
: DECLARE ( -- )
   [: s" zps" s" 0 VARIANT sone n n ;VARIANT VARIANT stwo n ;VARIANT" SUM-DECL ;]
      GENERATED-DECL:RUN
   LAST-FAM SWAP-FAM !
   SWAP-FAM @ VAR-START {: base:n :}
   base SWAP-A !   base 1 + SWAP-B !
   [: s" zph" s" 0 VARIANT hone n ;VARIANT" SUM-DECL ;] GENERATED-DECL:RUN
   LAST-FAM HOSTILE-FAM !
   \ a PRODUCT never reads a payload cell width, so only the count capability's
   \ own check stands between a negative count and malformed derived text.
   [: s" zpp" s" 0 DERIVE eq FIELD ppx n FIELD ppy n" PROD-DECL ;]
      GENERATED-DECL:RUN
   LAST-FAM PROD-FAM !
   \ a one-element, two-cell payload: its single root is a family application
   \ whose family is itself a tag plus one slot. WIDE-PROV lends it to the
   \ one-slot family, so the count bound passes and only the width bound stands.
   [: s" zpu" s" 0 VARIANT uone n ;VARIANT" SUM-DECL ;] GENERATED-DECL:RUN
   [: s" zpw" s" 0 VARIANT wone zpu ;VARIANT" SUM-DECL ;] GENERATED-DECL:RUN
   LAST-FAM VAR-START DONOR !
   \ two more ungenerated families for the call-count flip probes.
   [: s" zpfs" s" 0 DERIVE eq VARIANT fsone n n ;VARIANT VARIANT fstwo n ;VARIANT" SUM-DECL ;]
      GENERATED-DECL:RUN
   LAST-FAM FSHORT-FAM !
   [: s" zpfl" s" 0 DERIVE eq VARIANT flone n n ;VARIANT VARIANT fltwo n ;VARIANT" SUM-DECL ;]
      GENERATED-DECL:RUN
   LAST-FAM FLONG-FAM ! ;
: SWAP-FAMILY ( -- n ) SWAP-FAM @ ;
: HOSTILE-FAMILY ( -- n ) HOSTILE-FAM @ ;
: FLIP-SHORT-FAMILY ( -- n ) FSHORT-FAM @ ;
: FLIP-LONG-FAMILY ( -- n ) FLONG-FAM @ ;
: GENERATE-SWAPPED ( n -- n ) ASKED ! SWAP-RUN GOT @ ;
: GENERATE-COMMITTED ( n -- n ) ASKED ! COMMITTED-RUN GOT @ ;
: NEG-CODE ( n -- n ) ASKED ! [: NEG-RUN ;] catch ;
: ROOT-CODE ( n -- n ) ASKED ! [: ROOT-RUN ;] catch ;
: LEAN-CODE ( n -- n ) ASKED ! [: LEAN-RUN ;] catch ;
: WIDE-CODE ( n -- n ) ASKED ! [: WIDE-RUN ;] catch ;
: PRODUCT-NEG-CODE ( -- n ) [: PNEG-RUN ;] catch ;
: PRODUCT-HUGE-CODE ( -- n ) [: HUGE-RUN ;] catch ;
: FLIP-RESET ( -- ) 0 FLIP-N-CALLS !  -1 FLIP-VID !  0 FLIP-SEEN ! ;
: FLIP-SHORT-CODE ( n -- n ) ASKED ! FLIP-RESET [: FSHORT-RUN ;] catch ;
: FLIP-LONG-CODE ( n -- n ) ASKED ! FLIP-RESET [: FLONG-RUN ;] catch ;
: GENERATED ( -- n ) GOT @ ;
: FLIP-CALLS ( -- n ) FLIP-N-CALLS @ ;
: STALE-CODE ( -- n ) [: STALE-BODY ;] catch ;
: OVERRUN-CODE ( -- n ) [: OVERRUN-BODY ;] catch ;
: PROVIDER-CODE ( -- n ) E-PROVIDER ;
: COMMITTED-PAYLOAD-CODE ( -- n ) E-COMMITTED-PAYLOAD ;

\ the live probe: one candidate frame, one event transaction, both renders, then
\ a full rollback. Returns the committed provider's throw code; the live plan is
\ left in LROWS / readable through LIVE-NAME.
variable LIVE-CODE
: LIVE-PROBE ( -- )
   s" n" CON-CODE SCH-CON SCH-ROOT+ LROOT !
   s" pv" PKG-PUBLIC s" zpl" 0 SUM-KIND FAM-DECL LFAM !
   CAND-START
   LIVE-OPEN
   LTOK @ LFAM @ LVID @ DECL-EVENT:PAYLOAD-N LCOUNT !   \ the live view of the payload
   [: COMMITTED-RENDER ;] catch LIVE-CODE !   \ the committed-bound wall
   PEND-CLEAR
   LIVE-RENDER                                \ the live provider clears it
   PLAN-ROWS LROWS !
   PEND-CLEAR
   LTOK @ DECL-EVENT:ROLLBACK
   0 CAND-DONE drop ;
: LIVE-THROW ( -- n ) LIVE-CODE @ ;
: LIVE-ROWS ( -- n ) LROWS @ ;
: LIVE-NAME ( -- ptr u8 n ) 0 PLAN-WORD$ ;
: LIVE-DEF ( -- ptr u8 n ) 0 PLAN-DEF$ ;
: LIVE-COUNT ( -- n ) LCOUNT @ ;

;package

\ 1. a coherent provider that is NOT the committed view changes what is generated.
CTOR-PAYPROV-TEST:DECLARE
CTOR-PAYPROV-TEST:SWAP-FAMILY 0 < 0= -1 T=
CTOR-PAYPROV-TEST:HOSTILE-FAMILY CTOR-PAYPROV-TEST:SWAP-FAMILY <> -1 T=
\ zps generates under the SWAPPED provider: sone takes stwo's one payload cell
\ and stwo takes sone's two, so both constructors' checked effects follow the
\ provider and contradict the committed rows. This is ADMITTED on purpose. The
\ validation proves a provider's answers are internally consistent and fit the
\ family; the provider is the authority for WHICH payload a variant carries,
\ which is exactly what lets an unpublished declaration generate from its own
\ live view (case 3 below). Callers own the obligation that the view they supply
\ is the view that will be committed.
CTOR-PAYPROV-TEST:SWAP-FAMILY CTOR-PAYPROV-TEST:GENERATE-SWAPPED
   CTOR-PAYPROV-TEST:SWAP-FAMILY T=
s" ZP1 ( n -- zps ) ZPS:SONE" CHECK-QUIET-CANDIDATE! -1 T=
s" ZP2 ( n n -- zps ) ZPS:SONE" CHECK-QUIET-CANDIDATE! 0 T=
s" ZP3 ( n n -- zps ) ZPS:STWO" CHECK-QUIET-CANDIDATE! -1 T=
s" ZP4 ( n -- zps ) ZPS:STWO" CHECK-QUIET-CANDIDATE! 0 T=

\ 1b. a provider whose answers CHANGE after the first call cannot corrupt what is
\ generated: the generator takes the whole payload view once, before any text
\ exists, so the constructors carry the arity that was validated. Both families
\ derive eq, which is the path that re-read the count most often before the
\ snapshot (once per variant per arm). The call counts prove the capability was
\ asked exactly once per variant, not once per read.
CTOR-PAYPROV-TEST:FLIP-SHORT-FAMILY CTOR-PAYPROV-TEST:FLIP-SHORT-CODE 0 T=
CTOR-PAYPROV-TEST:GENERATED CTOR-PAYPROV-TEST:FLIP-SHORT-FAMILY T=
CTOR-PAYPROV-TEST:FLIP-CALLS 2 T=
s" ZF1 ( n n -- zpfs ) ZPFS:FSONE" CHECK-QUIET-CANDIDATE! -1 T=
s" ZF2 ( n -- zpfs ) ZPFS:FSTWO" CHECK-QUIET-CANDIDATE! -1 T=
s" ZF3 ( zpfs zpfs -- bool ) ZPFS:EQ" CHECK-QUIET-CANDIDATE! -1 T=
CTOR-PAYPROV-TEST:FLIP-LONG-FAMILY CTOR-PAYPROV-TEST:FLIP-LONG-CODE 0 T=
CTOR-PAYPROV-TEST:GENERATED CTOR-PAYPROV-TEST:FLIP-LONG-FAMILY T=
CTOR-PAYPROV-TEST:FLIP-CALLS 2 T=
s" ZF4 ( n n -- zpfl ) ZPFL:FLONE" CHECK-QUIET-CANDIDATE! -1 T=
s" ZF5 ( n -- zpfl ) ZPFL:FLTWO" CHECK-QUIET-CANDIDATE! -1 T=
s" ZF6 ( zpfl zpfl -- bool ) ZPFL:EQ" CHECK-QUIET-CANDIDATE! -1 T=

\ 2. every incoherent provider fails closed with the same named code, and the
\ family it was rejected on still generates normally through the committed one.
\ The oversized count is on the PRODUCT path, which reads no cell width, so only
\ the count bound stands between it and an over-long generated word.
CTOR-PAYPROV-TEST:HOSTILE-FAMILY CTOR-PAYPROV-TEST:NEG-CODE  CTOR-PAYPROV-TEST:PROVIDER-CODE T=
CTOR-PAYPROV-TEST:HOSTILE-FAMILY CTOR-PAYPROV-TEST:ROOT-CODE CTOR-PAYPROV-TEST:PROVIDER-CODE T=
CTOR-PAYPROV-TEST:HOSTILE-FAMILY CTOR-PAYPROV-TEST:LEAN-CODE CTOR-PAYPROV-TEST:PROVIDER-CODE T=
CTOR-PAYPROV-TEST:HOSTILE-FAMILY CTOR-PAYPROV-TEST:WIDE-CODE CTOR-PAYPROV-TEST:PROVIDER-CODE T=
CTOR-PAYPROV-TEST:PRODUCT-NEG-CODE CTOR-PAYPROV-TEST:PROVIDER-CODE T=
CTOR-PAYPROV-TEST:PRODUCT-HUGE-CODE CTOR-PAYPROV-TEST:PROVIDER-CODE T=
CTOR-PAYPROV-TEST:HOSTILE-FAMILY CTOR-PAYPROV-TEST:GENERATE-COMMITTED CTOR-PAYPROV-TEST:HOSTILE-FAMILY T=
s" ZP5 ( n -- zph ) ZPH:HONE" CHECK-QUIET-CANDIDATE! -1 T=
\ and a render that reaches outside the captured family fails closed on the
\ snapshot's own family check instead of reading another family's row.
CTOR-PAYPROV-TEST:STALE-CODE CTOR-PAYPROV-TEST:PROVIDER-CODE T=
CTOR-PAYPROV-TEST:OVERRUN-CODE CTOR-PAYPROV-TEST:PROVIDER-CODE T=

\ 3. the committed reader cannot see an unpublished payload declaration and the
\ live provider can, through the SAME renderer.
CTOR-PAYPROV-TEST:LIVE-PROBE
CTOR-PAYPROV-TEST:LIVE-COUNT 1 T=
CTOR-PAYPROV-TEST:LIVE-THROW CTOR-PAYPROV-TEST:COMMITTED-PAYLOAD-CODE T=
CTOR-PAYPROV-TEST:LIVE-ROWS 1 T=
CTOR-PAYPROV-TEST:LIVE-NAME s" PV-ZPL:ONE" T$=
\ the live declaration's own payload cell became the constructor's input and its
\ own cell width set the zero padding (one slot, one cell, so no pads, tag 0).
CTOR-PAYPROV-TEST:LIVE-DEF s" PV-ZPL:ONE ( n -- zpl ) 0 " T$=

s" PAYLOAD-PROVIDER" type cr

\ ---------------------------------------------------------------------------
\ closed one-cell layouts bind generic value vars and normalize bound outputs.
\ ---------------------------------------------------------------------------
package E10CELL
public
ENUM tag red blue ;ENUM
private

: E10-ID ( a -- a ) ;
: E10-TAG>N ( tag -- n )
   MATCH tag red OF 3 ENDOF blue OF 5 ENDOF ;MATCH ;

: E10-OPTION ( tag -- n )
   OPTION:SOME MATCH option
      none OF -1 ENDOF
      some OF E10-TAG>N ENDOF
   ;MATCH ;

: E10-OPTION-Q ( tag -- n )
   [: OPTION:SOME ;] execute MATCH option
      none OF -1 ENDOF
      some OF E10-TAG>N ENDOF
   ;MATCH ;

: E10-OK>N ( result<tag,n> -- n )
   MATCH result
      ok OF E10-TAG>N ENDOF
      err OF 100 + ENDOF
   ;MATCH ;

: E10-ERR>N ( result<n,tag> -- n )
   MATCH result
      ok OF ENDOF
      err OF E10-TAG>N 100 + ENDOF
   ;MATCH ;

using RESULT

: E10-OK ( tag -- n ) OK E10-OK>N ;
: E10-ERR ( tag -- n ) ERR E10-ERR>N ;

;using

: E10-RUN ( -- )
   E10CELL:TAG:RED E10-ID E10-TAG>N 3 T=
   E10CELL:TAG:BLUE E10-OPTION 5 T=
   E10CELL:TAG:RED E10-OPTION-Q 3 T=
   E10CELL:TAG:RED E10-OK 3 T=
   E10CELL:TAG:BLUE E10-ERR 105 T= ;

DEFLINEAR tok
STRUCTURE lin
   FIELD value tok
;STRUCTURE

s" E10-PTR-IN ( ptr a -- ptr tag )" CHECK-QUIET-CANDIDATE! 0 T=
s" E10-PTR-OUT ( ptr tag -- ptr a )" CHECK-QUIET-CANDIDATE! 0 T=
s" E10-RAW ( n -- tag ) here ! here @" CHECK-QUIET-CANDIDATE! 0 T=
s" E10-LINEAR ( lin -- lin ) E10-ID" CHECK-QUIET-CANDIDATE! 0 T=

E10-RUN
;package

\ ---------------------------------------------------------------------------
\ report: "ok" on success, nonzero exit on any failure.
\ ---------------------------------------------------------------------------
: REPORT ( -- )
   #FAIL @ 0 = if s" ok" type cr exit then
   #FAIL @ . s" type-ctor-suite: failures" 1 die ;
REPORT
