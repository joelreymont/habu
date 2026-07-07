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
\ parametric families: possibly-linear until TFAM 11, so no words publish;
\ constructor-package metadata is still recorded for items 9/11.
\ ---------------------------------------------------------------------------
SUMTYPE zpar 1
  VARIANT psome a ;VARIANT
;SUMTYPE
s" " s" zpar" TFAM-FIND-IN TCOK ! TCF !   TCOK @ -1 T=
TCF @ TFAM-VAR-START@ SUMV-CTOR-PKG$ s" ZPAR" T$=
s" ZB6 ( n -- zpar<n> ) ZPAR:PSOME" CHECK-QUIET-CANDIDATE! 1 T=   \ undefined word -> uncheckable

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
\ a new tail cannot certify into the closed constructor package.
s" : ZRES:EVIL ( -- n ) 7 ;" TCE-CATCH E-CTOR-PROTECTED T=
\ ordinary undefine of a user word still works.
: ZDOOMED ( -- n ) 5 ;
s" undefine ZDOOMED" TCE-CATCH 0 T=

\ ---------------------------------------------------------------------------
\ report: "ok" on success, nonzero exit on any failure.
\ ---------------------------------------------------------------------------
: REPORT ( -- )
   #FAIL @ 0 = if s" ok" type cr exit then
   #FAIL @ . s" type-ctor-suite: failures" 1 die ;
REPORT
