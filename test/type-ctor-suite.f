\ type-ctor-suite.f — generated-constructor suite.
\ A constructor word pushes its payload cells, M-p zero pads, then the tag.
\
\ A parametric family's value is one logical cell while its arguments are
\ variables; where an instantiation proves them non-linear it expands to its
\ hidden fields at the concrete boundary, and a linear instantiation is refused.
\ UNMAKE's open input takes in the caller's hidden cells the same way. A nested
\ application is as wide as its family at its arguments' widths, each parameter
\ one cell at declaration and its argument's width under a term. Products and
\ structures carry no tag, so their whole width is payload and they need no
\ extra pads. A tagged sum reserves its declared width: pass 1 emits pads for
\ the declared width, and the checker records each call site's extra pads
\ (instantiated minus declared) for pass 2 to add. An inner bundle carries its
\ own pads from its own construct site; the outer construct or MATCH adds only
\ the outer difference. Pass 2 can only add pads, so a variant that needs fewer
\ than its declared share (negative extra) fails closed.
\
\ A generated MAKE takes its family arguments from the declared output; UNMAKE
\ runs the same step backwards, seeded only from the live value it takes apart.
\ With no declared output, MAKE and SOME take them from their payloads on the
\ stack. A quotation body has no live input, so a quoted UNMAKE checks through
\ the stored effect and fails closed at a wide instantiation, while a quoted
\ MAKE seeds from the declared output.

require src/lib/adt/result.f
require src/lib/string.f
require src/lib/test.f

using TYPE-DECL

T-RESET

TYPED-VARIABLE TCE-A ptr u8   variable TCE-U
: TCE-GO ( -- )
   TCE-A @ TCE-U @ INCLUDE-EVALUATE ;
: TCE-CATCH ( ptr u8 n -- n )
   TCE-U ! TCE-A !
   [: TCE-GO ;] catch ;

\ An engine family's generated word is protected before anything names the family.
s" undefine SPAN-SPAN:MAKE" TCE-CATCH E-CTOR-PROTECTED T=

: TWX-MULTI-ERR-BEGIN ( -- ) MULTI-ERR-BEGIN ;
: TWX-MULTI-ERR-END ( -- n ) MULTI-ERR-END ;

SUMTYPE zres
  VARIANT ok  n ;VARIANT
  VARIANT err n ;VARIANT
;SUMTYPE
\ Variants spelled like stack words: declared early so every later body that
\ uses bare dup/swap proves the qualified def records no bare-global effect row.
SUMTYPE zwv
  VARIANT dup n ;VARIANT
  VARIANT swap  ;VARIANT
;SUMTYPE

: ZMK-OK ( n -- zres ) ZRES:OK ;
: ZMK-ERR ( n -- zres ) ZRES:ERR ;
s" GEN-CONCRETE" type cr

s" ZB1 ( -- zres ) ZRES:OK" CHECK-QUIET-CANDIDATE! 0 T=
s" ZB2 ( ptr u8 -- zres ) ZRES:OK" CHECK-QUIET-CANDIDATE! 0 T=
\ raw cells cannot forge a layout value.
s" ZB3 ( n -- zres ) 0" CHECK-QUIET-CANDIDATE! 0 T=
s" ZB4 ( n n -- zres )" CHECK-QUIET-CANDIDATE! 0 T=

SUMTYPE zmix
  VARIANT small n ;VARIANT
  VARIANT big ptr u8 n n ;VARIANT
;SUMTYPE
: ZMK-SMALL ( n -- zmix ) ZMIX:SMALL ;
: ZMK-BIG ( ptr u8 n n -- zmix ) ZMIX:BIG ;
s" GEN-MIX" type cr
s" ZB5 ( n n -- zmix ) ZMIX:BIG" CHECK-QUIET-CANDIDATE! 0 T=

SUMTYPE zen
  VARIANT lit  ;VARIANT
  VARIANT dark ;VARIANT
;SUMTYPE
: ZMK-LIT ( -- zen ) ZEN:LIT ;
: ZMK-DARK ( -- zen ) ZEN:DARK ;
s" GEN-ENUM" type cr

SUMTYPE zopt
  VARIANT none   ;VARIANT
  VARIANT some n ;VARIANT
;SUMTYPE
: ZMK-NONE ( -- zopt ) ZOPT:NONE ;
: ZMK-SOME ( n -- zopt ) ZOPT:SOME ;
s" GEN-OPT" type cr

\ in-package public family: constructor package derives from (pkg, tail) and
\ the words are callable from global scope; declaring-package state survives.
package zpub
public
SUMTYPE tres
  VARIANT yes n ;VARIANT
;SUMTYPE
private
NEWTYPE zonly<a>
;package
: ZMK-YES ( n -- zpub:tres ) ZPUB-TRES:YES ;
s" GEN-PKG" type cr
SUMTYPE zafter
  VARIANT one n ;VARIANT
;SUMTYPE
: ZMK-ONE ( n -- zafter ) ZAFTER:ONE ;
s" GEN-AFTER" type cr

\ Escaped constructor packages are called by their readable names across
\ packages, including spellings longer than 32 bytes.
package evx
public
SUMTYPE certify-slot
  VARIANT certify-got n ;VARIANT
  VARIANT certify-none ;VARIANT
;SUMTYPE
private
;package
: EVX-MK-GOT  ( n -- evx:certify-slot ) EVX-CERTIFY--SLOT:CERTIFY-GOT ;
: EVX-MK-NONE (   -- evx:certify-slot ) EVX-CERTIFY--SLOT:CERTIFY-NONE ;
s" EVX-GET ( n -- evx:certify-slot ) EVX-CERTIFY--SLOT:CERTIFY-GOT"  CHECK-QUIET-CANDIDATE! -1 T=
s" EVX-NON (   -- evx:certify-slot ) EVX-CERTIFY--SLOT:CERTIFY-NONE" CHECK-QUIET-CANDIDATE! -1 T=
s" EVX-BAD ( ptr u8 -- evx:certify-slot ) EVX-CERTIFY--SLOT:CERTIFY-GOT" CHECK-QUIET-CANDIDATE! 0 T=

package verylongpackagename
public
SUMTYPE resultresultr
  VARIANT one n ;VARIANT
;SUMTYPE
;package
: LONG-MK-ONE ( n -- verylongpackagename:resultresultr ) VERYLONGPACKAGENAME-RESULTRESULTR:ONE ;
s" LONG-GET ( n -- verylongpackagename:resultresultr ) VERYLONGPACKAGENAME-RESULTRESULTR:ONE" CHECK-QUIET-CANDIDATE! -1 T=
s" GEN-LONG-CTOR" type cr

SUMTYPE zpar<a>
  VARIANT psome a ;VARIANT
;SUMTYPE
s" ZB6 ( n -- zpar<n> ) ZPAR:PSOME" CHECK-QUIET-CANDIDATE! -1 T=

SUMTYPE zpoly<a,b>
  VARIANT ok  a ;VARIANT
  VARIANT err b ;VARIANT
;SUMTYPE
: ZPMK-OK ( n -- zpoly<n,n> ) ZPOLY:OK ;
: ZPMK-ERR ( n -- zpoly<n,n> ) ZPOLY:ERR ;
: ZPMK-PTR ( ptr u8 -- zpoly<ptr u8,n> ) ZPOLY:OK ;
: ZPMK-G ( a -- zpoly<a,b> ) ZPOLY:OK ;
: ZPMK-USE ( n -- zpoly<n,n> ) ZPMK-G ;
s" GEN-POLY" type cr
SUMTYPE zpmix<a,b>
  VARIANT small a ;VARIANT
  VARIANT big a b n ;VARIANT
;SUMTYPE
: ZPMK-SMALL ( n -- zpmix<n,n> ) ZPMIX:SMALL ;
: ZPMK-BIG ( n n n -- zpmix<n,n> ) ZPMIX:BIG ;
s" GEN-POLY-MIX" type cr
s" ZP1 ( -- zpoly<n,n> ) ZPOLY:OK" CHECK-QUIET-CANDIDATE! 0 T=
s" ZP2 ( ptr u8 -- zpoly<n,n> ) ZPOLY:OK" CHECK-QUIET-CANDIDATE! 0 T=
s" ZP3 ( n -- zpoly<n,n> ) 0" CHECK-QUIET-CANDIDATE! 0 T=
\ cross-family bundles cannot alias: a zpmix result is not a zpoly.
s" ZP4 ( n -- zpoly<n,n> ) ZPMIX:SMALL" CHECK-QUIET-CANDIDATE! 0 T=
s" ZP5 ( own -- zpoly<own,n> ) ZPOLY:OK" CHECK-QUIET-CANDIDATE! 0 T=
s" ZP6 ( zpoly<own,n> -- zpoly<own,n> )" CHECK-QUIET-CANDIDATE! 0 T=
s" ZP7 ( own -- zpoly<own,n> ) ZPMK-G" CHECK-QUIET-CANDIDATE! 0 T=
\ transports of a still-unresolved parametric result stay rejected inside a
\ body (possibly-linear conservative path).
s" ZP8 ( n -- zpoly<n,n> zpoly<n,n> ) ZPOLY:OK dup" CHECK-QUIET-CANDIDATE! 0 T=

\ private families export nothing: no constructor package, no words.
package zp8
private
SUMTYPE zsec
  VARIANT hide n ;VARIANT
;SUMTYPE
;package
s" ZB7 ( n -- n ) ZP8-ZSEC:HIDE" CHECK-QUIET-CANDIDATE! 0 T=   \ undefined word -> refused

\ rejected declarations generate nothing: a duplicate family neither redefines
\ nor duplicates the existing constructor words (load survives = proof).
TWX-MULTI-ERR-BEGIN
s" SUMTYPE zres VARIANT no n ;VARIANT ;SUMTYPE" TCE-CATCH 0 T=
TWX-MULTI-ERR-END 1 T=
: ZMK-OK2 ( n -- zres ) ZRES:OK ;
s" DUP-DECL-SAFE" type cr

TWX-MULTI-ERR-BEGIN
s" SUMTYPE zlin VARIANT keep own ;VARIANT ;SUMTYPE" TCE-CATCH 0 T=
TWX-MULTI-ERR-END 1 T=

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
\ a new tail in a generated package rejects in the checker.
s" : ZRES:EXTRA ( -- n ) 1 ;" TCE-CATCH E-CTOR-PROTECTED T=
s" : ZRES:EXTRA2 1 ;" TCE-CATCH E-CTOR-PROTECTED T=
: ZDOOMED ( -- n ) 5 ;
s" undefine ZDOOMED" TCE-CATCH 0 T=

\ Operand capture runs before locals and word lookup, so family/variant tokens
\ never resolve as either.
s" CN1 ( n -- zres ) construct zres ok" CHECK-QUIET-CANDIDATE! -1 T=
s" CN2 ( n -- zres ) construct ZRES OK" CHECK-QUIET-CANDIDATE! -1 T=
s" CN3 ( ptr u8 n n -- zmix ) construct zmix big" CHECK-QUIET-CANDIDATE! -1 T=
s" CN4 ( n -- zmix ) construct zmix small" CHECK-QUIET-CANDIDATE! -1 T=
s" CN5 ( -- zopt ) construct zopt none" CHECK-QUIET-CANDIDATE! -1 T=
s" CN6 ( n -- zpar<n> ) construct zpar psome" CHECK-QUIET-CANDIDATE! -1 T=
s" CN7 ( ptr u8 -- zpoly<ptr u8,n> ) construct zpoly ok" CHECK-QUIET-CANDIDATE! -1 T=
s" CN8 ( n n -- zres ) {: ok:n :} construct zres ok" CHECK-QUIET-CANDIDATE! -1 T=
s" CN9 ( n -- zres ) [: construct zres ok ;] execute" CHECK-QUIET-CANDIDATE! -1 T=
s" CONSTRUCT-OK" type cr
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
s" CB11 ( ptr u8 -- zpoly<n,n> ) construct zpoly ok" CHECK-QUIET-CANDIDATE! 0 T=
s" CB12 ( n -- zpoly<n,n> zpoly<n,n> ) construct zpoly ok dup" CHECK-QUIET-CANDIDATE! 0 T=   \ open-arg transport
s" CN10 ( n -- zwv ) construct zwv dup" CHECK-QUIET-CANDIDATE! -1 T=
s" CN11 ( -- zwv ) construct zwv swap" CHECK-QUIET-CANDIDATE! -1 T=
s" CONSTRUCT-BAD" type cr

\ qualified defs record only ( pkg, tail ) PUBLIC: no bare-global tail.
\ 1) bare dup/swap bind the PRIMS, not zwv's ZWV:DUP/ZWV:SWAP rows.
s" LK1 ( n n -- n n ) swap" CHECK-QUIET-CANDIDATE! -1 T=
s" LK2 ( n -- n n ) dup"    CHECK-QUIET-CANDIDATE! -1 T=
\ 2) a plain qualified colon def leaves bare `lkw` undefined (refused 0).
: lkpkg:lkw ( n -- n ) 1 + ;
s" LK3 ( n -- n ) lkw"       CHECK-QUIET-CANDIDATE! 0 T=
\ 3) the qualified spelling resolves.
s" LK4 ( n -- n ) lkpkg:lkw" CHECK-QUIET-CANDIDATE! -1 T=
s" LEAK-FIX" type cr
\ ownership: in-package resolution for public AND private families; outside
\ the package neither the bare nor the qualified family token resolves.
package cnpub
public
SUMTYPE cnres
  VARIANT yes n ;VARIANT
;SUMTYPE
s" CP1 ( n -- cnres ) construct cnres yes" CHECK-QUIET-CANDIDATE! -1 T=
private
SUMTYPE cnsec
  VARIANT hide n ;VARIANT
;SUMTYPE
s" CP2 ( n -- cnsec ) construct cnsec hide" CHECK-QUIET-CANDIDATE! -1 T=
;package
s" CB13 ( n -- cnpub:cnres ) construct cnres yes" CHECK-QUIET-CANDIDATE! 0 T=
s" CB14 ( n -- cnpub:cnres ) construct cnpub:cnres yes" CHECK-QUIET-CANDIDATE! 0 T=
s" CP3 ( n -- cnpub:cnres ) CNPUB-CNRES:YES" CHECK-QUIET-CANDIDATE! -1 T=   \ the public cross-package path
s" CONSTRUCT-OWN" type cr

\ Product generated words: a PUBLIC product publishes empty-bodied PKG:MAKE and
\ PKG:UNMAKE, since its value is its field cells in slot order (no tag).
PRODUCT zpt
  FIELD x n
  FIELD y n
;PRODUCT
: ZPT-MK ( n n -- zpt ) ZPT:MAKE ;
: ZPT-UN ( zpt -- n n ) ZPT:UNMAKE ;
: ZPT-RT ( -- n n ) 3 4 ZPT:MAKE ZPT:UNMAKE ;
s" GEN-PRODUCT" type cr
\ make/unmake are physical no-ops: slot0 deepest (x=3 under y=4).
ZPT-RT 4 T= 3 T=
: ZPT-X ( zpt -- n ) ZPT:UNMAKE drop ;
: ZPT-Y ( zpt -- n ) ZPT:UNMAKE nip ;
: ZPT-RTX ( -- n ) 7 9 ZPT:MAKE ZPT-X ;
: ZPT-RTY ( -- n ) 7 9 ZPT:MAKE ZPT-Y ;
ZPT-RTX 7 T=
ZPT-RTY 9 T=
\ raw cells cannot forge a product and a bundle cannot split without UNMAKE.
s" PB1 ( n -- zpt ) ZPT:MAKE" CHECK-QUIET-CANDIDATE! 0 T=
s" PB2 ( ptr u8 n -- zpt ) ZPT:MAKE" CHECK-QUIET-CANDIDATE! 0 T=
s" PB3 ( n n -- zpt )" CHECK-QUIET-CANDIDATE! 0 T=
s" PB4 ( zpt -- n n )" CHECK-QUIET-CANDIDATE! 0 T=
s" PB5 ( zpt -- zpt zpt ) dup" CHECK-QUIET-CANDIDATE! -1 T=
\ construct is kind-gated to sum/enum: no token form for products.
s" PB6 ( n n -- zpt ) construct zpt make" CHECK-QUIET-CANDIDATE! 0 T=
PRODUCT zbuf
  FIELD cap n
  FIELD raw ptr u8
;PRODUCT
: ZBUF-MK ( n ptr u8 -- zbuf ) ZBUF:MAKE ;
: ZBUF-UN ( zbuf -- n ptr u8 ) ZBUF:UNMAKE ;
s" GEN-PRODUCT-PTR" type cr
PRODUCT zpr<a,b>
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
s" PP1 ( ptr u8 -- zpr<n,n> ) 0 swap ZPR:MAKE" CHECK-QUIET-CANDIDATE! 0 T=
s" PP2 ( n n -- zpt ) ZPR:MAKE" CHECK-QUIET-CANDIDATE! 0 T=
s" PL1 ( own n -- zpr<own,n> ) ZPR:MAKE" CHECK-QUIET-CANDIDATE! 0 T=
s" PL2 ( zpr<own,n> -- zpr<own,n> )" CHECK-QUIET-CANDIDATE! 0 T=
s" PL3 ( zpr<own,n> -- own n ) ZPR:UNMAKE" CHECK-QUIET-CANDIDATE! 0 T=
\ in-package public product: derived package, callable from global scope,
\ package state survives generation.
package zppk
public
PRODUCT prow
  FIELD v n
;PRODUCT
;package
: ZPPK-MK ( n -- zppk:prow ) ZPPK-PROW:MAKE ;
: ZPPK-UN ( zppk:prow -- n ) ZPPK-PROW:UNMAKE ;
s" GEN-PRODUCT-PKG" type cr
\ private products export nothing: no package, no words, no construct form.
package zpsec
private
PRODUCT phid
  FIELD v n
;PRODUCT
;package
s" PS1 ( n -- n ) ZPSEC-PHID:MAKE" CHECK-QUIET-CANDIDATE! 0 T=   \ undefined word -> refused
s" package zpt" TCE-CATCH E-CTOR-PROTECTED T=
s" package ZPT" TCE-CATCH E-CTOR-PROTECTED T=
s" undefine ZPT:MAKE" TCE-CATCH E-CTOR-PROTECTED T=
s" undefine zpt:unmake" TCE-CATCH E-CTOR-PROTECTED T=
: ZPT-MK2 ( n n -- zpt ) ZPT:MAKE ;
s" PRODUCT-PROTECTED" type cr

\ The adversarials' compile-hook stderr is expected.
PRODUCT clw2 FIELD x n FIELD y n ;PRODUCT
SUMTYPE clopt<a> VARIANT none ;VARIANT VARIANT some a ;VARIANT ;SUMTYPE
s" : CLFC1 ( -- clopt<clw2> ) CLOPT:NONE ;" TCE-CATCH 0 T=
s" : CLFC2 ( -- clopt<clw2> ) construct clopt none ;" TCE-CATCH 0 T=
s" : CLFC3 ( n -- clopt<n> ) CLOPT:SOME ;" TCE-CATCH 0 T=
\ runtime round-trips assert the exact payload cells: generated ctor (extra=0
\ SOME, extra=1 NONE), raw `construct` NONE, and MATCH's arg-aware payload skip.
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
\ named payload products: result<clw2,n> (W=2 ok payload + cell err) and
\ option<clfs3> (W=3 payload).
PRODUCT clfs3 FIELD a n FIELD b n FIELD c n ;PRODUCT
SUMTYPE clfres<a,b> VARIANT ok a ;VARIANT VARIANT err b ;VARIANT ;SUMTYPE
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
\ A declared payload can itself be a nested application. Its published width
\ must include both inner tags, then construct and MATCH keep every cell.
SUMTYPE cnpayload
   VARIANT wrapped clopt<clfres<n,n>> ;VARIANT
   VARIANT nop ;VARIANT
;SUMTYPE
: CNP-WRAP ( clopt<clfres<n,n>> -- cnpayload ) CNPAYLOAD:WRAPPED ;
: CNP-GET ( cnpayload -- n ) MATCH cnpayload wrapped OF CN-GET ENDOF nop OF 999 ENDOF ;MATCH ;
: CNP-RT ( -- n ) 5 CN-IOK CN-SOME CNP-WRAP CNP-GET ;
CNP-RT 5 T=
SUMTYPE cnpayloadg<a>
   VARIANT wrapped clopt<clfres<a,n>> ;VARIANT
   VARIANT nop ;VARIANT
;SUMTYPE
: CNPG-WRAP ( clopt<clfres<clw2,n>> -- cnpayloadg<clw2> ) CNPAYLOADG:WRAPPED ;
: CNPG-GET ( cnpayloadg<clw2> -- n ) MATCH cnpayloadg wrapped OF CND-GET ENDOF nop OF 999 ENDOF ;MATCH ;
: CNPG-RT ( -- n ) 3 4 CND-IOK CND-SOME CNPG-WRAP CNPG-GET ;
CNPG-RT 7 T=
s" : CN-BADW ( clfres<n,n> -- clopt<clfres<clw2,n>> ) CLOPT:SOME ;" TCE-CATCH 70 T=
s" : CN-BADF ( clopt<n> -- clopt<clfres<n,n>> ) CLOPT:SOME ;" TCE-CATCH 70 T=
s" : CN-BADS ( n -- clopt<clfres<n,n>> ) CLOPT:SOME ;" TCE-CATCH 70 T=
s" : CN-TRUNC ( -- clopt<clfres<n,n>> ) CLOPT:SOME ;" TCE-CATCH 70 T=
\ soundness boundary: an OPEN inner var has an unstable width, so a real compile
\ stays staged fail-closed rather than lower a width guessed from a=1 cell.
s" : CN-OPEN ( clfres<n,a> -- clopt<clfres<n,a>> ) CLOPT:SOME ;" TCE-CATCH 70 T=
s" LOWER-WIDTH-AWARE-ROUNDTRIP" type cr

\ Asymmetric growth: when the widest-declared variant is not the widest
\ instantiated one, that variant needs fewer pad cells than the declared width
\ reserves (extra < 0). The positive-extra variant of the same family constructs
\ and a non-asymmetric instantiation matches, so the reject isolates to the
\ negative extra.
package XPAD-ASYM
public
PRODUCT clx2 FIELD x n FIELD y n ;PRODUCT
SUMTYPE clxg<a>
  VARIANT wtwo a a ;VARIANT
  VARIANT wthree n n n ;VARIANT
;SUMTYPE
private
\ clxg<clx2> (a = width 2): W = 1 + max(wtwo=4, wthree=3) = 5. wtwo extra = -1, wthree = +1.
s" CLXP ( clx2 clx2 -- clxg<clx2> ) construct clxg wtwo" CHECK-QUIET-CANDIDATE! 0 T=
s" : CLXG-BADTWO ( clx2 clx2 -- clxg<clx2> ) construct clxg wtwo ;" TCE-CATCH 70 T=
\ The positive-extra variant certifies and lowers, but its value is
\ produce-only: any total MATCH includes the wtwo arm, whose negative-extra
\ unpack fails closed.
s" : CLXG-THREE ( n n n -- clxg<clx2> ) construct clxg wthree ;" TCE-CATCH 0 T=
\ The wtwo arm uses only always-defined words (2drop 0), so its reject can only
\ be the negative-extra arm reject at 'OF': inside XPAD-ASYM the derived ctor
\ package is XPAD-ASYM-CLX2, so a bare CLX2:UNMAKE would itself throw rc 70.
s" : CLXG-GET ( clxg<clx2> -- n ) MATCH clxg wtwo OF 2drop 0 ENDOF wthree OF + + ENDOF ;MATCH ;" TCE-CATCH 70 T=
s" : CLXN-GET ( clxg<n> -- n ) MATCH clxg wtwo OF + ENDOF wthree OF + + ENDOF ;MATCH ;" TCE-CATCH 0 T=
\ No checked exhaustive MATCH consumes clxg<clx2>, so XPAD-WTHREE-W measures the
\ depth delta of the certified CLXG-THREE bundle: the certified width 5, the 4
\ cells pass 1 emits from the declared width and the pad pass 2 adds. The bundle
\ must be built through the checked CLXG-THREE: an unchecked in-word `construct`
\ skips pass 2 and reads 4.
variable XPAD-D0
: XPAD-MARK ( -- ) depth XPAD-D0 ! ;
: XPAD-DELTA ( -- n ) depth XPAD-D0 @ - ;
: TWX-XPAD-DROP-BUNDLE ( clxg<clx2> n -- n ) nip ;
: XPAD-WTHREE-W ( -- n )
   XPAD-MARK 1 2 3 CLXG-THREE XPAD-DELTA TWX-XPAD-DROP-BUNDLE ;
XPAD-WTHREE-W 5 T=
s" LOWER-WIDTH-ASYM-FAILCLOSED" type cr
;package

\ Families use string-eval so the derived accessors resolve at global scope.
\ XPGW-RT runs the checked MAKE->UNMAKE round trip: the field values read
\ top-down (z=3, b=2, a=1) and the 5551 canary below the bundle prove content,
\ order and width.
s" STRUCTURE xpginr FIELD a n FIELD b n ;STRUCTURE" TCE-CATCH 0 T=
s" STRUCTURE xpg<a> FIELD u a FIELD z n ;STRUCTURE" TCE-CATCH 0 T=
s" XPGWIDE ( xpginr n -- xpg<xpginr> ) XPG:MAKE" CHECK-QUIET-CANDIDATE! -1 T=
s" XPGFLAT ( n n n -- xpg<xpginr> ) XPG:MAKE" CHECK-QUIET-CANDIDATE! 0 T=      \ flattened (a b z) is not (xpginr z): still fail-closed
s" XPGCONC ( n n -- xpg<n> ) XPG:MAKE" CHECK-QUIET-CANDIDATE! -1 T=
s" XPGUNW ( xpg<xpginr> -- xpginr n ) XPG:UNMAKE" CHECK-QUIET-CANDIDATE! -1 T=
s" XPGUNFLAT ( xpg<xpginr> -- n n n ) XPG:UNMAKE" CHECK-QUIET-CANDIDATE! 0 T=   \ flattened fields are not (xpginr z): still fail-closed
s" XPGUNC ( xpg<n> -- n n ) XPG:UNMAKE" CHECK-QUIET-CANDIDATE! -1 T=
s" XPGUNMIS ( xpginr n -- xpg<xpginr> ) XPG:UNMAKE" CHECK-QUIET-CANDIDATE! 0 T=  \ UNMAKE is not typed as MAKE
s" XPGUNQ ( xpg<xpginr> -- xpginr n ) [: XPG:UNMAKE ;] execute" CHECK-QUIET-CANDIDATE! 0 T=
s" XPGUNQBAD ( xpg<xpginr> -- n n n ) [: XPG:UNMAKE ;] execute" CHECK-QUIET-CANDIDATE! 0 T=  \ flattened fields still fail closed through the quotation
s" XPGMKQ ( xpginr n -- xpg<xpginr> ) [: XPG:MAKE ;] execute" CHECK-QUIET-CANDIDATE! -1 T=
package XPAD-TAGLESS
: XPGW-MK ( xpginr n -- xpg<xpginr> ) XPG:MAKE ;
: XPGW-RT ( -- )
   5551 1 2 XPGINR:MAKE 3 XPGW-MK XPG:UNMAKE 3 T= XPGINR:UNMAKE 2 T= 1 T= 5551 T= ;
XPGW-RT
\ After the wide input is dropped, the xpg<n> an ordinary MAKE leaves has no
\ layout argument, so UNMAKE takes it apart through the stored effect, directly
\ and through a quotation.
: XPGW-DROP ( xpg<xpginr> -- n n )
   drop 7 8 XPG:MAKE XPG:UNMAKE ;
: XPGW-DROPQ ( xpg<xpginr> -- n n )
   drop 7 8 XPG:MAKE [: XPG:UNMAKE ;] execute ;
: XPGW-DROP-RT ( -- )
   5551 1 2 XPGINR:MAKE 3 XPGW-MK XPGW-DROP 8 T= 7 T= 5551 T=
   5551 1 2 XPGINR:MAKE 3 XPGW-MK XPGW-DROPQ 8 T= 7 T= 5551 T= ;
XPGW-DROP-RT
: XPGW-LIVE ( -- )
   5551 1 2 XPGINR:MAKE 3 XPG:MAKE XPG:UNMAKE 3 T= XPGINR:UNMAKE 2 T= 1 T= 5551 T=
   5551 1 2 XPGINR:MAKE OPTION:SOME
   MATCH option some OF XPGINR:UNMAKE - ENDOF none OF 0 ENDOF ;MATCH -1 T= 5551 T= ;
XPGW-LIVE
;package
\ A concrete instantiation that certifies proves the word resolves, so the wide
\ rc 70 is a negative-extra reject, not an undefined word.
s" PRODUCT xw2 FIELD x n FIELD y n ;PRODUCT" TCE-CATCH 0 T=
\ xst<xw2>: stable(n n n, declared 3) is sole argmax both ways; grow(a, declared 1) instantiates
\ to 2 -> extra(grow) = (3-2) - (3-1) = -1 (a non-widest variant that grows).
s" SUMTYPE xst<a> VARIANT stable n n n ;VARIANT VARIANT grow a ;VARIANT ;SUMTYPE" TCE-CATCH 0 T=
s" XSTC ( xw2 -- xst<xw2> ) construct xst grow" CHECK-QUIET-CANDIDATE! 0 T=
s" : XSTG-CONC ( n -- xst<n> ) XST:GROW ;" TCE-CATCH 0 T=
s" : XSTG-WIDE ( xw2 -- xst<xw2> ) XST:GROW ;" TCE-CATCH 70 T=
s" : XSTM ( xst<xw2> -- n ) MATCH xst grow OF 2drop 0 ENDOF stable OF + + ENDOF ;MATCH ;" TCE-CATCH 70 T=
\ tie: tbig wbig(n n n n, declared 4) / wsm(a a, declared 2) at xw2 -> both instantiate to 4
\ (INSTANTIATED TIE); extra(wsm) = (4-4) - (4-2) = -2.
s" SUMTYPE tbig<a> VARIANT wbig n n n n ;VARIANT VARIANT wsm a a ;VARIANT ;SUMTYPE" TCE-CATCH 0 T=
s" TBC ( xw2 xw2 -- tbig<xw2> ) construct tbig wsm" CHECK-QUIET-CANDIDATE! 0 T=
s" : TBWG-CONC ( n n -- tbig<n> ) TBIG:WSM ;" TCE-CATCH 0 T=
s" : TBWG-WIDE ( xw2 xw2 -- tbig<xw2> ) TBIG:WSM ;" TCE-CATCH 70 T=
s" : TBM ( tbig<xw2> -- n ) MATCH tbig wsm OF 2drop 2drop 0 ENDOF wbig OF + + + ENDOF ;MATCH ;" TCE-CATCH 70 T=
\ order independence: the same tie family with its variants declared in the
\ opposite order fails closed identically through the constructor and MATCH.
s" SUMTYPE tbig2<a> VARIANT wsm2 a a ;VARIANT VARIANT wbig2 n n n n ;VARIANT ;SUMTYPE" TCE-CATCH 0 T=
s" TB2C ( xw2 xw2 -- tbig2<xw2> ) construct tbig2 wsm2" CHECK-QUIET-CANDIDATE! 0 T=
s" : TB2G-WIDE ( xw2 xw2 -- tbig2<xw2> ) TBIG2:WSM2 ;" TCE-CATCH 70 T=
s" : TB2M ( tbig2<xw2> -- n ) MATCH tbig2 wsm2 OF 2drop 2drop 0 ENDOF wbig2 OF + + + ENDOF ;MATCH ;" TCE-CATCH 70 T=
s" TAGLESS-ARITH-KEEPS-FAILCLOSED" type cr

\ wnst's payload wopt<wres<n,n>> is 3 cells, so wnst is 4 wide and its store
\ takes all 4 cells NESTED pushes; wapplied<wleaf> carries wholder<wleaf>'s 3.
s" STRUCTURE wleaf FIELD left n FIELD right n ;STRUCTURE" TCE-CATCH 0 T=
s" STRUCTURE wholder<a> FIELD item a FIELD stamp n ;STRUCTURE" TCE-CATCH 0 T=
s" STRUCTURE wapplied<a> FIELD nest wholder<a> FIELD stamp n ;STRUCTURE" TCE-CATCH 0 T=
s" SUMTYPE wopt<a> VARIANT some a ;VARIANT VARIANT none ;VARIANT ;SUMTYPE" TCE-CATCH 0 T=
s" SUMTYPE wres<a,b> VARIANT ok a ;VARIANT VARIANT err b ;VARIANT ;SUMTYPE" TCE-CATCH 0 T=
s" SUMTYPE wnst VARIANT nested wopt<wres<n,n>> ;VARIANT VARIANT nop ;VARIANT ;SUMTYPE" TCE-CATCH 0 T=
s" STRUCTURE wsx<a> FIELD f init<a,wleaf> ;STRUCTURE" TCE-CATCH 0 T=

s" WAPMK ( wholder<wleaf> n -- wapplied<wleaf> ) WAPPLIED:MAKE" CHECK-QUIET-CANDIDATE! -1 T=
s" WAPFLAT ( wleaf n n -- wapplied<wleaf> ) WAPPLIED:MAKE" CHECK-QUIET-CANDIDATE! 0 T=
s" WAPMIS ( wholder<n> n -- wapplied<wleaf> ) WAPPLIED:MAKE" CHECK-QUIET-CANDIDATE! 0 T=
s" WNSTMK ( wopt<wres<n,n>> -- wnst ) WNST:NESTED" CHECK-QUIET-CANDIDATE! -1 T=
s" WNSTBAD ( wopt<n> -- wnst ) WNST:NESTED" CHECK-QUIET-CANDIDATE! 0 T=
\ An open argument hides the width only where the width reads it. init's first
\ argument is a scope (init<n,wleaf> is E-BAD-SIGNATURE), so a signature names
\ wsx only with its argument open. init reads just its element slot, so
\ wsx<a>'s width is closed: the row expands it and drop takes the whole bundle.
\ wapplied<a>'s width reads a through wholder<a>, so it stays one logical cell
\ that drop's variable cannot bind.
s" WSXDROP ( wsx<a> -- ) drop" CHECK-QUIET-CANDIDATE! -1 T=
s" WAPDROP ( wapplied<a> -- ) drop" CHECK-QUIET-CANDIDATE! 0 T=
\ record-at refuses a widened application: wapplied<wleaf> is 4 cells over 3
\ committed slots. An accessor refuses a family argument at any width, so only
\ record-at reaches the width condition.
s" WAPAT ( ptr wapplied<wleaf> n -- ptr wapplied<wleaf> ) record-at" CHECK-QUIET-CANDIDATE! 0 T=
s" WAPAT1 ( ptr wapplied<n> n -- ptr wapplied<n> ) record-at" CHECK-QUIET-CANDIDATE! -1 T=
\ UNMAKE's empty body must give back exactly the bundle's cells: wapplied<wleaf>
\ is 4 cells and its fields wholder<wleaf> n are 4.
s" WAPUN ( wapplied<wleaf> -- wholder<wleaf> n ) WAPPLIED:UNMAKE" CHECK-QUIET-CANDIDATE! -1 T=
s" WAPUNFLAT ( wapplied<wleaf> -- wleaf n n ) WAPPLIED:UNMAKE" CHECK-QUIET-CANDIDATE! 0 T=

TYPED-VARIABLE WNST-V wnst
TYPED-VARIABLE WAP-V wapplied<wleaf>
TYPED-VARIABLE WAPN-V wapplied<n>
package NESTED-WIDTH-RT
: WNST-OPT ( -- wopt<wres<n,n>> ) 5 WRES:OK WOPT:SOME ;
: WNST-MK ( -- wnst ) WNST-OPT WNST:NESTED ;
: WNST-GET ( wnst -- n ) MATCH wnst nested OF MATCH wopt some OF MATCH wres ok OF ENDOF err OF ENDOF ;MATCH ENDOF none OF 0 ENDOF ;MATCH ENDOF nop OF -1 ENDOF ;MATCH ;
: WAPN-READ ( -- n n n )                    \ item, nested stamp, stamp
   WAPN-V @ WAPPLIED:UNMAKE {: stamp:n :}
   WHOLDER:UNMAKE stamp ;
: WHOLD-MK ( wleaf n -- wholder<wleaf> ) WHOLDER:MAKE ;
: WAP-MK ( wholder<wleaf> n -- wapplied<wleaf> ) WAPPLIED:MAKE ;
: WAP-READ ( -- n n n n )                   \ left, right, nested stamp, stamp
   WAP-V @ WAPPLIED:UNMAKE {: stamp:n :}
   WHOLDER:UNMAKE {: nstamp:n :}
   WLEAF:UNMAKE nstamp stamp ;
public
: RUN ( -- )
   5551
   depth WNST-MK WNST-V ! depth swap - 1 T=      \ the store consumes exactly the 4 cells NESTED pushed (base: 3 of 4)
   WNST-V @ WNST-GET 5 T=
   depth WNST:NOP WNST-V ! depth swap - 1 T=     \ NOP's 3 pads + tag are the same 4 cells
   WNST-V @ WNST-GET -1 T=
   depth 6 7 WHOLDER:MAKE 8 WAPPLIED:MAKE WAPN-V ! depth swap - 1 T=   \ wapplied<n> is 3 cells: wholder<n>'s 2 and the stamp
   WAPN-READ 8 T= 7 T= 6 T=
   depth 1 2 WLEAF:MAKE 3 WHOLD-MK 4 WAP-MK WAP-V ! depth swap - 1 T=
   WAP-READ 4 T= 3 T= 2 T= 1 T=
   5551 T= ;
;package
NESTED-WIDTH-RT:RUN
s" NESTED-APP-WIDTH" type cr

\ A parametric family application and a single-effect quotation as variant
\ payloads construct and MATCH; the quotation payload is one xt cell recovered
\ in the arm.
SUMTYPE pqinn<a> VARIANT yes a ;VARIANT VARIANT no ;VARIANT ;SUMTYPE
SUMTYPE pqhold VARIANT hh pqinn<n> ;VARIANT ;SUMTYPE
: PQ-MK ( n -- pqhold ) PQINN:YES PQHOLD:HH ;
: PQ-GET ( pqhold -- n ) MATCH pqhold hh OF MATCH pqinn yes OF ENDOF no OF 0 ENDOF ;MATCH ENDOF ;MATCH ;
: PQ-RT ( -- n ) 7 PQ-MK PQ-GET ;
PQ-RT 7 T=
SUMTYPE pqact VARIANT run [ n -- n ] ;VARIANT VARIANT nop ;VARIANT ;SUMTYPE
: PQ-APP ( pqact -- n ) MATCH pqact run OF 5 swap execute ENDOF nop OF 0 ENDOF ;MATCH ;
: PQ-RUN ( -- n ) [: 1 + ;] PQACT:RUN PQ-APP ;
PQ-RUN 6 T=
s" : PQ-BADQ ( -- pqact ) [: 0 0= ;] PQACT:RUN ;" TCE-CATCH 70 T=

SUMTYPE pq2 VARIANT run [ n n -- n ] ;VARIANT VARIANT nop ;VARIANT ;SUMTYPE
: PQ2-APP ( pq2 -- n ) MATCH pq2 run OF 3 4 rot execute ENDOF nop OF 0 ENDOF ;MATCH ;
: PQ2-RUN ( -- n ) [: + ;] PQ2:RUN PQ2-APP ;
PQ2-RUN 7 T=
s" : PQ2-BADQ ( -- pq2 ) [: 1 + ;] PQ2:RUN ;" TCE-CATCH 70 T=
SUMTYPE pq0 VARIANT run [ -- n ] ;VARIANT VARIANT nop ;VARIANT ;SUMTYPE
: PQ0-APP ( pq0 -- n ) MATCH pq0 run OF execute ENDOF nop OF 0 ENDOF ;MATCH ;
: PQ0-RUN ( -- n ) [: 9 ;] PQ0:RUN PQ0-APP ;
PQ0-RUN 9 T=
variable PQD-SINK
SUMTYPE pqd VARIANT run [ n -- ] ;VARIANT VARIANT nop ;VARIANT ;SUMTYPE
: PQD-APP ( pqd -- n ) MATCH pqd run OF 5 swap execute PQD-SINK @ ENDOF nop OF 0 ENDOF ;MATCH ;
: PQD-RUN ( -- n ) 0 PQD-SINK ! [: PQD-SINK ! ;] PQD:RUN PQD-APP ;
PQD-RUN 5 T=
create PQM-BUF 4 allot  3 PQM-BUF c!
SUMTYPE pqm VARIANT run [ n ptr u8 -- n ] ;VARIANT VARIANT nop ;VARIANT ;SUMTYPE
: PQM-APP ( pqm -- n ) MATCH pqm run OF 5 PQM-BUF 100 >r rot execute r> + ENDOF nop OF 0 ENDOF ;MATCH ;
: PQM-RUN ( -- n ) [: drop ;] PQM:RUN PQM-APP ;
PQM-RUN 105 T=
s" PARAMETRIC-QUOT-PAYLOAD" type cr

\ A constructor effect may ground a generic payload/result parameter from a
\ closed one-cell nominal family. The binding happens while equal-family result
\ arguments pair, before the same variable meets the constructor payload.
package zpub
public

ENUM rfbtag red blue ;ENUM
ENUM rfberr bad worse ;ENUM
PRODUCT rfbprod FIELD value n ;PRODUCT
NEWTYPE rfbcell<a>
NEWTYPE rfbpair<a,b>
PRODUCT rfbwone<a> FIELD value a ;PRODUCT

\ Each constructor arm grounds its own parameter. The conditional forces the
\ independently checked arms through one joined result, then MATCH proves the
\ exact runtime tag and payload survive.
: RFB-OK  ( rfbtag -- result<rfbtag,rfberr> ) RESULT:OK ;
: RFB-ERR ( rfberr -- result<rfbtag,rfberr> ) RESULT:ERR ;
: RFB-SELECT ( bool -- result<rfbtag,rfberr> )
   if ZPUB-RFBTAG:RED RESULT:OK else ZPUB-RFBERR:BAD RESULT:ERR then ;
: RFB-TAG# ( rfbtag -- n )
   MATCH rfbtag red OF 11 ENDOF blue OF 12 ENDOF ;MATCH ;
: RFB-ERR# ( rfberr -- n )
   MATCH rfberr bad OF 21 ENDOF worse OF 22 ENDOF ;MATCH ;
: RFB-VALUE ( result<rfbtag,rfberr> -- n )
   MATCH result ok OF RFB-TAG# ENDOF err OF RFB-ERR# ENDOF ;MATCH ;

\ Fresh generated-constructor paths exercise the same bidirectional seeding
\ independently of the shared RESULT provider, with width-1 enum and product
\ layout arguments.
: RFB-ZENUM ( rfbtag -- zpoly<rfbtag,rfberr> ) ZPOLY:OK ;
: RFB-ZPROD ( rfbprod -- zpoly<rfbprod,rfberr> ) ZPOLY:OK ;
: RFB-ZENUM# ( zpoly<rfbtag,rfberr> -- n )
   MATCH zpoly ok OF RFB-TAG# ENDOF err OF RFB-ERR# ENDOF ;MATCH ;
: RFB-ZPROD# ( zpoly<rfbprod,rfberr> -- n )
   MATCH zpoly ok OF ZPUB-RFBPROD:UNMAKE ENDOF err OF RFB-ERR# ENDOF ;MATCH ;
: RFB-PROD-OK ( rfbprod -- result<rfbprod,rfberr> ) RESULT:OK ;
: RFB-PROD# ( result<rfbprod,rfberr> -- n )
   MATCH result ok OF ZPUB-RFBPROD:UNMAKE ENDOF err OF RFB-ERR# ENDOF ;MATCH ;
: RFB-RUN ( -- )
   ZPUB-RFBTAG:BLUE RFB-OK RFB-VALUE 12 T=
   ZPUB-RFBERR:WORSE RFB-ERR RFB-VALUE 22 T=
   0 0= RFB-SELECT RFB-VALUE 11 T=
   0 0= 0= RFB-SELECT RFB-VALUE 21 T=
   ZPUB-RFBTAG:BLUE RFB-ZENUM RFB-ZENUM# 12 T=
   29 ZPUB-RFBPROD:MAKE RFB-ZPROD RFB-ZPROD# 29 T=
   31 ZPUB-RFBPROD:MAKE RFB-PROD-OK RFB-PROD# 31 T= ;
RFB-RUN

\ A generic same-family identity supplies the reverse argument orientation.
\ Repeating one parameter propagates its first grounding; a different second
\ grounding and a later inconsistent top-level use both reject.
: RFB-ALIAS ( rfbpair<a,a> -- rfbpair<a,a> ) ;
: RFB-LATER ( a rfbpair<a,b> -- a ) drop ;
s" RFB-A1 ( rfbpair<rfbtag,rfbtag> -- rfbpair<rfbtag,rfbtag> ) RFB-ALIAS"
   CHECK-QUIET-CANDIDATE! -1 T=
s" RFB-A2 ( rfbpair<rfbtag,rfberr> -- rfbpair<rfbtag,rfberr> ) RFB-ALIAS"
   CHECK-QUIET-CANDIDATE! 0 T=
s" RFB-L1 ( rfbtag rfbpair<rfbtag,rfberr> -- rfbtag ) RFB-LATER"
   CHECK-QUIET-CANDIDATE! -1 T=
s" RFB-L2 ( rfberr rfbpair<rfbtag,rfberr> -- rfberr ) RFB-LATER"
   CHECK-QUIET-CANDIDATE! 0 T=

\ The alias variable occurs only in stored positions, so it takes a two-cell
\ term and a term behind a pointer.
: RFB-PTR-ID ( ptr rfbpair<a,a> -- ptr rfbpair<a,a> ) ;
s" RFB-WIDE ( rfbpair<clw2,clw2> -- rfbpair<clw2,clw2> ) RFB-ALIAS"
   CHECK-QUIET-CANDIDATE! -1 T=
s" RFB-PTR ( ptr rfbpair<rfbtag,rfbtag> -- ptr rfbpair<rfbtag,rfbtag> ) RFB-PTR-ID"
   CHECK-QUIET-CANDIDATE! -1 T=

\ Open family terms and one-cell families containing a linear resource cannot
\ ground the alias variable. Nominal family and payload roles stay exact,
\ including after logical values lower to hidden physical fields.
s" RFB-OPEN ( rfbpair<clopt<a>,clopt<a>> -- rfbpair<clopt<a>,clopt<a>> ) RFB-ALIAS"
   CHECK-QUIET-CANDIDATE! 0 T=
s" RFB-LIN ( rfbpair<rfbcell<own>,rfbcell<own>> -- rfbpair<rfbcell<own>,rfbcell<own>> ) RFB-ALIAS"
   CHECK-QUIET-CANDIDATE! 0 T=
s" RFB-XFAM ( rfbtag -- clopt<rfbtag> ) RESULT:OK"
   CHECK-QUIET-CANDIDATE! 0 T=
s" RFB-XPAY ( rfberr -- result<rfbtag,rfberr> ) RESULT:OK"
   CHECK-QUIET-CANDIDATE! 0 T=

\ RESULT:OK still leaves err's argument free.
TYPED-VARIABLE RFB-OV option<rfbtag>
TYPED-VARIABLE RFB-ON option<n>
s" RFB-SOME-DROP ( rfbtag -- ) OPTION:SOME drop" CHECK-QUIET-CANDIDATE! -1 T=
create RFB-DIAG 512 allot
RFB-DIAG 512 DIAG-BUFFER!
s" RFB-SOME-WRONG ( rfbtag -- ) OPTION:SOME RFB-ON !" CHECK-CANDIDATE! 0 T=
DIAG-BUFFER$ s" actual: option<rfbtag>" CONTAINS? TTRUE
DIAG-BUFFER-OFF
s" RFB-OK-FREE ( rfbtag -- ) RESULT:OK drop" CHECK-QUIET-CANDIDATE! 0 T=
: RFB-REST# ( option<rfbtag> -- n ) MATCH option some OF RFB-TAG# ENDOF none OF 0 ENDOF ;MATCH ;
: RFB-SOME-RUN ( -- )
   ZPUB-RFBTAG:BLUE OPTION:SOME RFB-OV !
   RFB-OV @ RFB-REST# 12 T= ;
RFB-SOME-RUN
\ A payload below an open option seeds the constructor: MAKE needs no declared output.
STRUCTURE rfbslot<a> FIELD head a FIELD rest option<a> ;STRUCTURE
: RFB-SLOT-RUN ( -- )
   5551 ZPUB-RFBTAG:BLUE OPTION:NONE ZPUB-RFBSLOT:MAKE ZPUB-RFBSLOT:UNMAKE
   RFB-REST# 0 T= RFB-TAG# 12 T= 5551 T=
   5551 ZPUB-RFBTAG:RED ZPUB-RFBTAG:BLUE OPTION:SOME ZPUB-RFBSLOT:MAKE ZPUB-RFBSLOT:UNMAKE
   RFB-REST# 12 T= RFB-TAG# 11 T= 5551 T= ;
RFB-SLOT-RUN
s" RESULT-FAMILY-BIND" type cr

;package

\ Reserved construct resolves in its owner's global package. The dynamically
\ evaluated definitions are real checked compiles.
s" : RFB-CON-E ( zpub:rfbtag -- result<zpub:rfbtag,zpub:rfberr> ) construct result ok ;"
   TCE-CATCH 0 T=
s" : RFB-CON-P ( zpub:rfbprod -- result<zpub:rfbprod,zpub:rfberr> ) construct result ok ;"
   TCE-CATCH 0 T=
s" : RFB-CON-RUN ( -- ) ZPUB-RFBTAG:RED RFB-CON-E ZPUB:RFB-VALUE 11 T= 37 ZPUB-RFBPROD:MAKE RFB-CON-P ZPUB:RFB-PROD# 37 T= ;"
   TCE-CATCH 0 T=
RFB-CON-RUN
\ Open variables nested below a pointer are not closed family arguments.
\ Neither constructor may pre-seed rfbwone<ptr a> into RESULT's fresh payload
\ variable.
s" : RFB-PTR-GEN-BAD ( zpub:rfbwone<ptr a> -- result<zpub:rfbwone<ptr a>,n> ) RESULT:OK ;"
   TCE-CATCH 70 T=
s" : RFB-PTR-CON-BAD ( zpub:rfbwone<ptr a> -- result<zpub:rfbwone<ptr a>,n> ) construct result ok ;"
   TCE-CATCH 70 T=

T-REPORT

;using
