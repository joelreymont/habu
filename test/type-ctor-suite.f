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

\ ---------------------------------------------------------------------------
\ top-level public arity-0 sum: constructors exist, certify, enforce payloads.
\ ---------------------------------------------------------------------------
SUMTYPE zres 0
  VARIANT ok  n ;VARIANT
  VARIANT err n ;VARIANT
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
s" zpub" s" zonly" TFAM-FIND-IN TCOK ! TCF !   TCOK @ -1 T=
TCF @ TFAM-VIS@ CHECKER-PACKAGE-PRIVATE T=
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
s" " s" zpar" TFAM-FIND-IN TCOK ! TCF !   TCOK @ -1 T=
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
s" zp8" s" zsec" TFAM-FIND-IN TCOK ! TCF !   TCOK @ -1 T=
TCF @ TFAM-VAR-START@ SUMV-CTOR-PKG$ nip 0 T=
s" ZB7 ( n -- n ) ZP8-ZSEC:HIDE" CHECK-QUIET-CANDIDATE! 1 T=   \ undefined word -> uncheckable

\ ---------------------------------------------------------------------------
\ rejected declarations generate nothing: a duplicate family neither redefines
\ nor duplicates the existing constructor words (load survives = proof).
\ ---------------------------------------------------------------------------
MULTI-ERR-BEGIN
s" SUMTYPE zres 0 VARIANT no n ;VARIANT ;SUMTYPE" TCE-CATCH 0 T=
MULTI-ERR-END 1 T=
: ZMK-OK2 ( n -- zres ) ZRES:OK ;
s" DUP-DECL-SAFE" type cr

\ ---------------------------------------------------------------------------
\ linear payloads stay rejected until TFAM 11 (regression pins).
\ ---------------------------------------------------------------------------
\ concrete linear payload rejects at declaration (v1 payload grammar).
MULTI-ERR-BEGIN
s" SUMTYPE zlin 0 VARIANT keep own ;VARIANT ;SUMTYPE" TCE-CATCH 0 T=
MULTI-ERR-END 1 T=
s" " s" zlin" TFAM-FIND-IN TCOK ! drop   TCOK @ 0 T=

\ ---------------------------------------------------------------------------
\ slice 3: protection. Generated packages are closed-but-callable: `package`
\ cannot open/reopen the derived name (any case), `undefine` of a generated
\ word rejects BEFORE retirement (still callable after the catch), and a new
\ tail cannot certify into the constructor package. SV.CTOR-SYM records the
\ published checker symbol.
\ ---------------------------------------------------------------------------
s" " s" zres" TFAM-FIND-IN TCOK ! TCF !   TCOK @ -1 T=
TCF @ TFAM-VAR-START@ SUMV-CTOR-SYM@ 0 <> -1 T=
TCF @ TFAM-VAR-START@ 1 + SUMV-CTOR-SYM@ 0 <> -1 T=
s" ZRES:OK" CHECKER-RECORD-SYM  TCF @ TFAM-VAR-START@ SUMV-CTOR-SYM@  T=
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
\ Native/Gforth lowering is item 10: certified construct bodies stay
\ uncompilable (engine E-UNDEFINED rc 70, gate-pinned in GE-CONSTRUCT-PENDING).
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
\ variants spelled like stack words are captured operands, never word calls.
SUMTYPE zwv 0
  VARIANT dup n ;VARIANT
  VARIANT swap  ;VARIANT
;SUMTYPE
s" CN10 ( n -- zwv ) construct zwv dup" CHECK-QUIET-CANDIDATE! -1 T=
s" CN11 ( -- zwv ) construct zwv swap" CHECK-QUIET-CANDIDATE! -1 T=
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
s" CONSTRUCT-BAD" type cr
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
\ report: "ok" on success, nonzero exit on any failure.
\ ---------------------------------------------------------------------------
: REPORT ( -- )
   #FAIL @ 0 = if s" ok" type cr exit then
   #FAIL @ . s" type-ctor-suite: failures" 1 die ;
REPORT
