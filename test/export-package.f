\ export-package.f - interpret-level `EXPORT` in fresh child engines. Inside an
\ open package's public section it publishes an existing word under its own
\ tail; outside a package it is rejected. A package's `global` section defines
\ bare global words whose bodies call the package's private words.

\ Run: bin/hb --load src/lib/error.f src/lib/string.f src/lib/test.f src/lib/memory.f src/lib/fs.f
\   src/lib/fs-mutate.f src/lib/process.f src/lib/process-argv.f
\   test/export-package.f

require src/lib/error.f
require src/lib/string.f
require src/lib/test.f
require src/lib/memory.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/test/ending.f
require src/lib/engine-candidate.f

2048 constant XPK-CAP
70 constant XPK-UNDEF-RC              \ undefined-word child exit status
70 constant XPK-REJECT-RC             \ a rejection's exit status (CHECKER-REJECT-RC)

variable XPK-ROOT-U
variable XPK-CHILD-U
variable XPK-IN-U
variable XPK-OUT-U
variable XPK-ERR-U
variable XPK-EXITED                 \ bool: child completed by exit
variable XPK-RC

create XPK-ROOT-BUF PATH-CAP allot
create XPK-CHILD-BUF PATH-CAP allot
create XPK-IN XPK-CAP allot
create XPK-OUT XPK-CAP allot
create XPK-ERR XPK-CAP allot
create XPK-EMPTY 1 allot

: XPK-COPY! ( ^u8 n ^u8 ^n -- ) {: src:^ u:n dst:^ lenp:^ :}
   u PATH-CAP > if E-FS-CAPACITY throw then
   src dst u BYTE-COPY
   u lenp ! ;

: XPK-ROOT ( -- ^u8 n )   XPK-ROOT-BUF XPK-ROOT-U @ ;
: XPK-CHILD ( -- ^u8 n )  XPK-CHILD-BUF XPK-CHILD-U @ ;
: XPK-IN$ ( -- ^u8 n )    XPK-IN XPK-IN-U @ ;

: XPK-LF ( -- )   10 SB-APPEND-C ;

: XPK-LINE ( ^u8 n -- )   SB-APPEND XPK-LF ;

\ --- forge programs -----------------------------------------------------------
\ Child exits: a primitive source, a trusted primitive and a top-level EXPORT
\ 70 with their records; a sealed package's private word 70.

: XPK-SRC-PKG ( -- )                          \ shared source package XA with XA:W
   s" package XA" XPK-LINE
   s" public" XPK-LINE
   s" : W ( n -- n ) 2 * ;" XPK-LINE
   s" ;package" XPK-LINE ;

: XPK-OK-FORGE$ ( -- ^u8 n )               \ dual-name execution + checked callers
   SB-RESET
   XPK-SRC-PKG
   s" package XB" XPK-LINE
   s" public" XPK-LINE
   s" EXPORT XA:W" XPK-LINE
   s" ;package" XPK-LINE
   s" : U1 ( n -- n ) XB:W ;" XPK-LINE
   s" : U2 ( n -- n ) XA:W ;" XPK-LINE
   s" 7 XA:W . cr" XPK-LINE
   s" 7 XB:W . cr" XPK-LINE
   s" 5 U1 . cr" XPK-LINE
   s" 5 U2 . cr" XPK-LINE
   SB$ ;

: XPK-TOP-FORGE$ ( -- ^u8 n )              \ top-level EXPORT rejects
   SB-RESET
   s" : SQ ( n -- n ) dup * ;" XPK-LINE
   s" EXPORT SQ" XPK-LINE
   SB$ ;

: XPK-UNDEF-FORGE$ ( -- ^u8 n )            \ undefined source rejects
   SB-RESET
   s" package XB" XPK-LINE
   s" public" XPK-LINE
   s" EXPORT NOSUCH-EXPORT-SRC" XPK-LINE
   s" ;package" XPK-LINE
   SB$ ;

: XPK-QDUP-FORGE$ ( -- ^u8 n )             \ top-level ?dup over an enum value
   SB-RESET
   s" SUMTYPE xpklight VARIANT red ;VARIANT VARIANT green ;VARIANT ;SUMTYPE" XPK-LINE
   s" XPKLIGHT:RED ?dup" XPK-LINE
   SB$ ;

: XPK-NONAME-FORGE$ ( -- ^u8 n )           \ EXPORT with no name rejects
   SB-RESET
   s" package XB" XPK-LINE
   s" public" XPK-LINE
   s" EXPORT" XPK-LINE
   SB$ ;

: XPK-DUP-FORGE$ ( -- ^u8 n )              \ second export of the same tail rejects
   SB-RESET
   XPK-SRC-PKG
   s" package XB" XPK-LINE
   s" public" XPK-LINE
   s" EXPORT XA:W" XPK-LINE
   s" EXPORT XA:W" XPK-LINE
   s" ;package" XPK-LINE
   SB$ ;

: XPK-SELF-FORGE$ ( -- ^u8 n )             \ self-export in the same section rejects
   SB-RESET
   s" package XB" XPK-LINE
   s" public" XPK-LINE
   s" : SW ( -- n ) 1 ;" XPK-LINE
   s" EXPORT SW" XPK-LINE
   s" ;package" XPK-LINE
   SB$ ;

: XPK-PRIM-FORGE$ ( -- ^u8 n )             \ primitive source rejects (E-EXPORT-PRIM)
   SB-RESET
   s" package XB" XPK-LINE
   s" public" XPK-LINE
   s" EXPORT dup" XPK-LINE
   s" ;package" XPK-LINE
   SB$ ;

: XPK-JUDGED-FORGE$ ( -- ^u8 n )           \ a word the checker judges (E-EXPORT-PRIM)
   SB-RESET
   s" package XB" XPK-LINE
   s" public" XPK-LINE
   s" EXPORT execute" XPK-LINE
   s" ;package" XPK-LINE
   SB$ ;

: XPK-KEYWORD-FORGE$ ( -- ^u8 n )          \ a top-level keyword (E-EXPORT-UNDEFINED)
   SB-RESET
   s" package XB" XPK-LINE
   s" public" XPK-LINE
   s" EXPORT public" XPK-LINE
   s" ;package" XPK-LINE
   SB$ ;

\ U's public NAME, exported bare from a package under `using U`.
: XPK-USED-FORGE$ ( ^u8 n -- ^u8 n ) {: a:^ u:n :}
   SB-RESET
   s" package U" XPK-LINE
   s" public" XPK-LINE
   s" : " SB-APPEND  a u SB-APPEND  s"  ( -- n ) 7 ;" XPK-LINE
   s" ;package" XPK-LINE
   s" using U" XPK-LINE
   s" package XP" XPK-LINE
   s" public" XPK-LINE
   s" EXPORT " SB-APPEND  a u XPK-LINE
   s" ;package" XPK-LINE
   s" ;using" XPK-LINE
   SB$ ;

\ The part U's public XW, exported bare from its parent XPA under `using U`.
: XPK-PART-FORGE$ ( -- ^u8 n )
   SB-RESET
   s" package XPA" XPK-LINE
   s" private package U" XPK-LINE
   s" public : XW ( -- n ) 7 ;" XPK-LINE
   s" ;package" XPK-LINE
   s" using U" XPK-LINE
   s" public EXPORT XW" XPK-LINE
   s" ;using" XPK-LINE
   s" ;package" XPK-LINE
   SB$ ;

\ Top-level U's public XW, exported bare from XPH, whose part U hides U.
: XPK-HIDDEN-FORGE$ ( -- ^u8 n )
   SB-RESET
   s" package U public : XW ( -- n ) 9 ; ;package" XPK-LINE
   s" using U" XPK-LINE
   s" package XPH private package U public : ZW ( -- n ) 7 ; ;package" XPK-LINE
   s" using U public EXPORT XW ;using" XPK-LINE
   s" ;package" XPK-LINE
   s" ;using" XPK-LINE
   SB$ ;

: XPK-CREATE-FORGE$ ( -- ^u8 n )           \ a primitive spelled as a keyword (E-EXPORT-PRIM)
   SB-RESET
   s" package XB" XPK-LINE
   s" public" XPK-LINE
   s" EXPORT create" XPK-LINE
   s" ;package" XPK-LINE
   SB$ ;

: XPK-UNSAFE-FORGE$ ( -- ^u8 n )           \ trusted primitive rejects (E-EXPORT-UNSAFE)
   SB-RESET
   s" package TICK-ALIAS" XPK-LINE
   s" public" XPK-LINE
   s" EXPORT patch32" XPK-LINE
   s" ;package" XPK-LINE
   SB$ ;

: XPK-DECL-EXPORT-FORGE$ ( -- ^u8 n )      \ a declaration rejects (E-EXPORT-UNSAFE)
   SB-RESET
   s" package XD" XPK-LINE
   s" public" XPK-LINE
   s" EXPORT ENUM" XPK-LINE
   s" ;package" XPK-LINE
   SB$ ;

: XPK-PRIV-FORGE$ ( -- ^u8 n )             \ an unsealed package's private word
   SB-RESET
   s" package XA" XPK-LINE
   s" : PW ( -- n ) 73 ;" XPK-LINE
   s" ;package" XPK-LINE
   s" package XB" XPK-LINE
   s" EXPORT XA:PW" XPK-LINE
   s" public" XPK-LINE
   s" : TAKE ( -- n ) PW ;" XPK-LINE
   s" ;package" XPK-LINE
   s" XB:TAKE . cr" XPK-LINE
   SB$ ;

: XPK-DATA-FORGE$ ( -- ^u8 n )             \ a data record is never internal
   SB-RESET
   s" 42 constant K package P public export K ;package P:K ." XPK-LINE
   SB$ ;

\ Re-exporting a generated constructor (closed but callable) under a second
\ public name is allowed, a checked caller through the alias compiles, and the
\ alias inherits DNAME-WIDE.
: XPK-CTOR-FORGE$ ( -- ^u8 n )
   SB-RESET
   s" SUMTYPE xpw" XPK-LINE
   s"   VARIANT ok n ;VARIANT" XPK-LINE
   s" ;SUMTYPE" XPK-LINE
   s" package XC" XPK-LINE
   s" public" XPK-LINE
   s" EXPORT xpw:ok" XPK-LINE
   s" ;package" XPK-LINE
   s" : MK ( n -- xpw ) XC:ok ;" XPK-LINE
   S\" s\" ctor-alias-ok\" type cr" XPK-LINE
   SB$ ;

: XPK-CTOR-WIDE-SRC-FORGE$ ( -- ^u8 n )    \ interpret gate on the SOURCE ctor
   SB-RESET
   s" SUMTYPE xpw" XPK-LINE
   s"   VARIANT ok n ;VARIANT" XPK-LINE
   s" ;SUMTYPE" XPK-LINE
   s" 1 xpw:ok" XPK-LINE
   S\" s\" ran-wide\" type cr" XPK-LINE
   s" dup" XPK-LINE
   SB$ ;

: XPK-CTOR-WIDE-ALIAS-FORGE$ ( -- ^u8 n )  \ interpret gate on the ALIAS
   SB-RESET
   s" SUMTYPE xpw" XPK-LINE
   s"   VARIANT ok n ;VARIANT" XPK-LINE
   s" ;SUMTYPE" XPK-LINE
   s" package XC" XPK-LINE
   s" public" XPK-LINE
   s" EXPORT xpw:ok" XPK-LINE
   s" ;package" XPK-LINE
   s" 1 XC:ok" XPK-LINE
   S\" s\" ran-wide\" type cr" XPK-LINE
   s" dup" XPK-LINE
   SB$ ;

\ PKG's private P-HELP stays private; its global P-GLOBAL is bare.
: XPK-GLOBAL-PKG ( -- )
   s" package PKG" XPK-LINE
   s" : P-HELP ( -- n ) 42 ;" XPK-LINE
   s" global" XPK-LINE
   s" : P-GLOBAL ( -- n ) P-HELP ;" XPK-LINE
   s" ;package" XPK-LINE ;

: XPK-GLOBAL-FORGE$ ( -- ^u8 n )
   SB-RESET XPK-GLOBAL-PKG
   s" P-GLOBAL . cr" XPK-LINE
   SB$ ;

\ The package, then the line a u.
: XPK-GLOBAL-AFTER$ ( ^u8 n -- ^u8 n ) {: a:^ u:n :}
   SB-RESET XPK-GLOBAL-PKG  a u XPK-LINE  SB$ ;

: XPK-GLOBAL-OUTSIDE$ ( -- ^u8 n )
   SB-RESET
   s" global" XPK-LINE
   s" : P-GLOBAL ( -- n ) 1 ;" XPK-LINE
   SB$ ;

: XPK-END-PACKAGE-OUTSIDE$ ( -- ^u8 n )
   SB-RESET  s" ;package" XPK-LINE  SB$ ;

: XPK-EVAL-OPEN$ ( -- ^u8 n )
   SB-RESET
   S\" s\" package XQ\" evaluate : XD ( -- n ) 1 ; ;package 1 . cr" XPK-LINE
   SB$ ;

: XPK-EVAL-PUBLIC$ ( -- ^u8 n )
   SB-RESET
   S\" s\" package XQ\" evaluate" XPK-LINE
   s" public" XPK-LINE
   s" : XD ( -- n ) 1 ;" XPK-LINE
   s" ;package" XPK-LINE
   s" XQ:XD . cr" XPK-LINE
   SB$ ;

\ A global section after seal-package: its body would bind the sealed private.
: XPK-GLOBAL-SEALED$ ( -- ^u8 n )
   SB-RESET
   s" package P" XPK-LINE
   s" private" XPK-LINE
   s" : HID ( -- n ) 9 ;" XPK-LINE
   s" seal-package" XPK-LINE
   s" global" XPK-LINE
   s" : LATE ( -- n ) HID ;" XPK-LINE
   s" ;package" XPK-LINE
   s" LATE . cr" XPK-LINE
   SB$ ;

\ A STRUCTURE in a sealed package's global section.
: XPK-SEALED-STRUCTURE$ ( -- ^u8 n )
   SB-RESET
   s" package P" XPK-LINE
   s" private" XPK-LINE
   s" STRUCTURE pcell FIELD v n ;STRUCTURE" XPK-LINE
   s" seal-package" XPK-LINE
   s" global" XPK-LINE
   s" STRUCTURE gbox FIELD c pcell ;STRUCTURE" XPK-LINE
   s" ;package" XPK-LINE
   SB$ ;

\ A DEFTYPE over a base in a sealed package's private section.
: XPK-SEALED-DEFTYPE$ ( -- ^u8 n )
   SB-RESET
   s" package P" XPK-LINE
   s" private" XPK-LINE
   s" seal-package" XPK-LINE
   s" DEFTYPE GDT n" XPK-LINE
   s" ;package" XPK-LINE
   SB$ ;

\ A DEFTYPE without a base in a sealed package's private section.
: XPK-SEALED-DEFTYPE-BARE$ ( -- ^u8 n )
   SB-RESET
   s" package P" XPK-LINE
   s" private" XPK-LINE
   s" seal-package" XPK-LINE
   s" DEFTYPE GDU" XPK-LINE
   s" ;package" XPK-LINE
   SB$ ;

\ A STRUCTURE whose header does not parse, in a sealed package's global section.
: XPK-SEALED-PARAM$ ( -- ^u8 n )
   SB-RESET
   s" package P" XPK-LINE
   s" private" XPK-LINE
   s" seal-package" XPK-LINE
   s" global" XPK-LINE
   s" STRUCTURE gbx<a FIELD v n ;STRUCTURE" XPK-LINE
   s" ;package" XPK-LINE
   SB$ ;

\ A qualified name defined in a sealed package's private section.
: XPK-SEALED-QUALIFIED$ ( -- ^u8 n )
   SB-RESET
   s" package P" XPK-LINE
   s" private" XPK-LINE
   s" : HID ( -- n ) 9 ;" XPK-LINE
   s" seal-package" XPK-LINE
   s" : Q:LATE ( -- n ) HID ;" XPK-LINE
   s" ;package" XPK-LINE
   s" Q:LATE . cr" XPK-LINE
   SB$ ;

\ A sealed package's part B, then the line a u in B's global section.
: XPK-SEALED-PART$ ( ^u8 n -- ^u8 n ) {: a:^ u:n :}
   SB-RESET
   s" package U" XPK-LINE
   s" public" XPK-LINE
   s" : UW ( -- n ) 3 ;" XPK-LINE
   s" ;package" XPK-LINE
   s" package P" XPK-LINE
   s" private" XPK-LINE
   s" : HID ( -- n ) 9 ;" XPK-LINE
   s" seal-package" XPK-LINE
   s" package B" XPK-LINE
   s" global" XPK-LINE
   a u XPK-LINE
   s" ;package" XPK-LINE
   s" ;package" XPK-LINE
   SB$ ;

\ A STRUCTURE in package Z, whose constructor package Z-PK is sealed.
: XPK-SEALED-CTOR$ ( -- ^u8 n )
   SB-RESET
   s" package Z-PK" XPK-LINE
   s" public" XPK-LINE
   s" : PW ( -- n ) 1 ;" XPK-LINE
   s" seal-package" XPK-LINE
   s" ;package" XPK-LINE
   s" package Z" XPK-LINE
   s" public" XPK-LINE
   s" STRUCTURE pk FIELD v n ;STRUCTURE" XPK-LINE
   s" ;package" XPK-LINE
   s" 1 . cr" XPK-LINE
   SB$ ;

\ A qualified STRUCTURE B:s in part B of a sealed package.
: XPK-SEALED-PART-STRUCT$ ( -- ^u8 n )
   SB-RESET
   s" package P" XPK-LINE
   s" private" XPK-LINE
   s" : HID ( -- n ) 9 ;" XPK-LINE
   s" seal-package" XPK-LINE
   s" package B" XPK-LINE
   s" STRUCTURE B:s FIELD v n ;STRUCTURE" XPK-LINE
   s" ;package" XPK-LINE
   s" ;package" XPK-LINE
   SB$ ;

\ A MUTUAL in a sealed package.
: XPK-SEALED-MUTUAL$ ( -- ^u8 n )
   SB-RESET
   s" package MU" XPK-LINE
   s" seal-package" XPK-LINE
   s" MUTUAL ma mb" XPK-LINE
   s" STRUCTURE ma FIELD v n ;STRUCTURE" XPK-LINE
   s" STRUCTURE mb FIELD w n ;STRUCTURE" XPK-LINE
   s" ;MUTUAL" XPK-LINE
   s" ;package" XPK-LINE
   SB$ ;

\ A SUMTYPE in package Z, whose constructor package Z-PK is sealed.
: XPK-SEALED-SUM-CTOR$ ( -- ^u8 n )
   SB-RESET
   s" package Z-PK" XPK-LINE
   s" public" XPK-LINE
   s" : PW ( -- n ) 1 ;" XPK-LINE
   s" seal-package" XPK-LINE
   s" ;package" XPK-LINE
   s" package Z" XPK-LINE
   s" public" XPK-LINE
   s" SUMTYPE pk VARIANT pa ;VARIANT ;SUMTYPE" XPK-LINE
   s" ;package" XPK-LINE
   s" 1 . cr" XPK-LINE
   SB$ ;

\ A MUTUAL in a sealed package whose first member has a header.
: XPK-SEALED-MUTUAL-PARAM$ ( -- ^u8 n )
   SB-RESET
   s" package MU" XPK-LINE
   s" seal-package" XPK-LINE
   s" MUTUAL pa<a> pb" XPK-LINE
   s" STRUCTURE pa<a> FIELD v a ;STRUCTURE" XPK-LINE
   s" STRUCTURE pb FIELD w n ;STRUCTURE" XPK-LINE
   s" ;MUTUAL" XPK-LINE
   s" ;package" XPK-LINE
   SB$ ;

\ A MUTUAL whose rejected first entry comes before a sealed package's name.
: XPK-SEALED-MUTUAL-ORDER$ ( -- ^u8 n )
   SB-RESET
   s" package MS" XPK-LINE
   s" seal-package" XPK-LINE
   s" ;package" XPK-LINE
   s" MUTUAL Pa MS:pb" XPK-LINE
   s" STRUCTURE Pa FIELD v n ;STRUCTURE" XPK-LINE
   s" ;MUTUAL" XPK-LINE
   SB$ ;

\ A CALLBACK: in a sealed package.
: XPK-SEALED-CALLBACK$ ( -- ^u8 n )
   SB-RESET
   s" require src/lib/ffi-callback.f" XPK-LINE
   s" package P" XPK-LINE
   s" private" XPK-LINE
   s" : HID ( -- n ) 9 ;" XPK-LINE
   s" seal-package" XPK-LINE
   s" CALLBACK: CB ( -- ) ;CALLBACK" XPK-LINE
   s" ;package" XPK-LINE
   SB$ ;

\ A CALLBACK: whose name is taken.
: XPK-TAKEN-CALLBACK$ ( -- ^u8 n )
   SB-RESET
   s" require src/lib/ffi-callback.f" XPK-LINE
   s" : CB ( -- n ) 9 ;" XPK-LINE
   s" CALLBACK: CB ( -- ) ;CALLBACK" XPK-LINE
   SB$ ;

\ A CALLBACK: with no name.
: XPK-NAMELESS-CALLBACK$ ( -- ^u8 n )
   SB-RESET
   s" require src/lib/ffi-callback.f" XPK-LINE
   s" CALLBACK: ( -- ) ;CALLBACK" XPK-LINE
   SB$ ;

\ A sealed package P, then the section word s su, if any, and the line a u.
: XPK-SEALED-LINE$ ( ^u8 n ^u8 n -- ^u8 n ) {: s:^ su:n a:^ u:n :}
   SB-RESET
   s" package P" XPK-LINE
   s" private" XPK-LINE
   s" : HID ( -- n ) 9 ;" XPK-LINE
   s" seal-package" XPK-LINE
   su 0 > IF s su XPK-LINE THEN
   a u XPK-LINE
   s" ;package" XPK-LINE
   SB$ ;

\ Package XSB exports a private word of the sealed package XSA.
: XPK-SEALED-EXPORT$ ( -- ^u8 n )
   SB-RESET
   s" package XSA" XPK-LINE
   s" private" XPK-LINE
   s" : HID ( -- n ) 9 ;" XPK-LINE
   s" seal-package" XPK-LINE
   s" ;package" XPK-LINE
   s" package XSB" XPK-LINE
   s" EXPORT XSA:HID" XPK-LINE
   s" ;package" XPK-LINE
   SB$ ;

\ A sealed package's part B defines in its private and public sections.
: XPK-PART-SECTIONS$ ( -- ^u8 n )
   SB-RESET
   s" package P" XPK-LINE
   s" private" XPK-LINE
   s" : HID ( -- n ) 9 ;" XPK-LINE
   s" seal-package" XPK-LINE
   s" package B" XPK-LINE
   s" : PV ( -- n ) HID ;" XPK-LINE
   s" public" XPK-LINE
   s" : PB ( -- n ) PV 1+ ;" XPK-LINE
   s" ;package" XPK-LINE
   s" ;package" XPK-LINE
   SB$ ;

\ As XPK-PART-SECTIONS$, with part B using another package.
: XPK-PART-USING$ ( -- ^u8 n )
   SB-RESET
   s" package U" XPK-LINE
   s" public" XPK-LINE
   s" : UW ( -- n ) 3 ;" XPK-LINE
   s" ;package" XPK-LINE
   s" package P" XPK-LINE
   s" private" XPK-LINE
   s" : HID ( -- n ) 9 ;" XPK-LINE
   s" seal-package" XPK-LINE
   s" package B" XPK-LINE
   s" using U" XPK-LINE
   s" : PV ( -- n ) UW HID + ;" XPK-LINE
   s" public" XPK-LINE
   s" : PB ( -- n ) PV ;" XPK-LINE
   s" ;using" XPK-LINE
   s" ;package" XPK-LINE
   s" ;package" XPK-LINE
   SB$ ;

\ A sealed package's part B defines B:PV from its global section.
: XPK-PART-QUALIFIED$ ( -- ^u8 n )
   SB-RESET
   s" package P" XPK-LINE
   s" private" XPK-LINE
   s" : HID ( -- n ) 9 ;" XPK-LINE
   s" seal-package" XPK-LINE
   s" package B" XPK-LINE
   s" global" XPK-LINE
   s" : B:PV ( -- n ) HID ;" XPK-LINE
   s" ;package" XPK-LINE
   s" ;package" XPK-LINE
   SB$ ;

\ Types declared in XG's global section name its private xgcell; the OPAQUE
\ xgbox's generated words go to XG's private section, and the section after
\ it is global again.
: XPK-GLOBAL-TYPES$ ( -- ^u8 n )
   SB-RESET
   s" package XG" XPK-LINE
   s" STRUCTURE xgcell FIELD v n ;STRUCTURE" XPK-LINE
   s" global" XPK-LINE
   s" SUMTYPE xgsum VARIANT at ^xgcell ;VARIANT VARIANT nil ;VARIANT ;SUMTYPE" XPK-LINE
   s" STRUCTURE xgbox OPAQUE FIELD c xgcell ;STRUCTURE" XPK-LINE
   s" ENUM xgopt VARIANT some FIELD c xgcell ;VARIANT VARIANT none ;VARIANT ;ENUM" XPK-LINE
   s" : XG-RUN ( n -- n ) XGCELL-MAKE XGBOX-MAKE XGBOX-UNMAKE XGCELL-UNMAKE 1 + ;" XPK-LINE
   s" ;package" XPK-LINE
   s" 40 XG-RUN . cr" XPK-LINE
   s" XG-XGSUM:nil drop 42 . cr" XPK-LINE
   s" XG-XGOPT:none drop 43 . cr" XPK-LINE
   SB$ ;

\ --- child spawn + outcome capture --------------------------------------------
\ Children run ENGINE-CANDIDATE:PATH$ over piped stdin or under --load.

: XPK-STORE! ( len len PROCESS:ending ^u8 n -- )
   {: outu:len erru:len oc src:^ srcu:n :}
   outu LEN>N XPK-OUT-U !  erru LEN>N XPK-ERR-U !
   oc MATCH PROCESS:ending
     exited OF XPK-RC ! 0 0= XPK-EXITED ! ENDOF
     signaled OF XPK-RC ! 0 0= 0= XPK-EXITED ! ENDOF
     timeout OF
        src srcu XPK-OUT XPK-OUT-U @ XPK-ERR XPK-ERR-U @ T-TIMED-OUT
     ENDOF
   ;MATCH ;

: XPK-IN! ( ^u8 n -- ) {: a:^ u:n :}
   u XPK-CAP > if E-FS-CAPACITY throw then
   a XPK-IN u BYTE-COPY
   u XPK-IN-U ! ;

: XPK-RUN-LOAD ( ^u8 n -- )
   {: src:^ srcu:n :}
   XPK-CHILD src srcu FS:WRITE-ALL
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   XPK-CHILD >LEN PROC-ARGV+
   ENGINE-CANDIDATE:PATH$ >LEN  XPK-EMPTY 0 >LEN  XPK-OUT XPK-CAP >LEN
   XPK-ERR XPK-CAP >LEN  PROCESS:WAIT-FOREVER
   RUN-ARGV-STDIN-CAPTURE-OUTCOME
   src srcu XPK-STORE! ;

: XPK-RUN-STDIN ( ^u8 n -- )
   XPK-IN!
   PROC-ARGV-RESET
   ENGINE-CANDIDATE:PATH$ >LEN  XPK-IN$ >LEN  XPK-OUT XPK-CAP >LEN
   XPK-ERR XPK-CAP >LEN  PROCESS:WAIT-FOREVER
   RUN-ARGV-STDIN-CAPTURE-OUTCOME
   XPK-IN$ XPK-STORE! ;

\ A tools/check.f run's arguments, up to its `--`.
: XPK-CHECK-ARGV ( -- )
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   s" tools/check.f" >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+ ;

\ Adds the child file a --load run wrote to the arguments so far and runs them.
: XPK-CHECK-CHILD ( -- )
   XPK-CHILD >LEN PROC-ARGV+
   ENGINE-CANDIDATE:PATH$ >LEN  XPK-EMPTY 0 >LEN  XPK-OUT XPK-CAP >LEN
   XPK-ERR XPK-CAP >LEN  PROCESS:WAIT-FOREVER
   RUN-ARGV-STDIN-CAPTURE-OUTCOME
   XPK-CHILD XPK-STORE! ;

: XPK-RUN-CHECK ( -- )  XPK-CHECK-ARGV XPK-CHECK-CHILD ;

\ The verifier alone, without the run stage that would print the load's record.
: XPK-RUN-VERIFY ( -- )
   XPK-CHECK-ARGV  s" --verify-only" >LEN PROC-ARGV+  XPK-CHECK-CHILD ;

: XPK-ASSERT-RC ( n -- ) {: rc:n :}
   XPK-EXITED @ TTRUE
   XPK-RC @ rc T= ;

: XPK-OUT? ( ^u8 n -- ) {: a:^ u:n :}
   XPK-OUT XPK-OUT-U @ a u CONTAINS? TTRUE ;

: XPK-ERR? ( ^u8 n -- ) {: a:^ u:n :}
   XPK-ERR XPK-ERR-U @ a u CONTAINS? TTRUE ;

: XPK-PREPARE ( -- )
   FS:CLEANUP-RESET
   s" habu-export-pkg" FS:MKTEMP-DIR {: a:^ u:n :}
   a u XPK-ROOT-BUF XPK-ROOT-U XPK-COPY!
   XPK-ROOT FS:CLEANUP-TREE+
   XPK-ROOT s" forge.f" XPK-CHILD-BUF FS:JOIN-PATH XPK-CHILD-U ! ;

: XPK-CLEANUP ( -- )
   FS:CLEANUP-RUN
   XPK-ROOT FS:EXISTS? TFALSE ;

\ --- cases ---------------------------------------------------------------------

\ The source a u is rejected with record e and text c by stdin, by --load and
\ by the verifier alone.
: XPK-REJECT-ALL ( ^u8 n ^u8 n ^u8 n -- ) {: a:^ u:n e:^ eu:n c:^ cu:n :}
   a u XPK-RUN-STDIN XPK-REJECT-RC XPK-ASSERT-RC  e eu XPK-ERR?  c cu XPK-ERR?
   a u XPK-RUN-LOAD XPK-REJECT-RC XPK-ASSERT-RC  e eu XPK-ERR?  c cu XPK-ERR?
   XPK-RUN-VERIFY XPK-REJECT-RC XPK-ASSERT-RC  e eu XPK-ERR?  c cu XPK-ERR? ;

: XPK-POSITIVES ( -- )
   s" dual-name execution + checked callers" T-LABEL
   XPK-OK-FORGE$ XPK-RUN-STDIN 0 XPK-ASSERT-RC s" 14" XPK-OUT? s" 10" XPK-OUT?
   s" generated ctor re-export allowed; checked alias caller compiles" T-LABEL
   XPK-CTOR-FORGE$ XPK-RUN-STDIN 0 XPK-ASSERT-RC s" ctor-alias-ok" XPK-OUT?
   s" data record exports (--load)" T-LABEL
   XPK-DATA-FORGE$ XPK-RUN-LOAD 0 XPK-ASSERT-RC s" 42" XPK-OUT?
   s" an unsealed package's private word exports (--load)" T-LABEL
   XPK-PRIV-FORGE$ XPK-RUN-LOAD 0 XPK-ASSERT-RC s" 73" XPK-OUT? ;

: XPK-NEGATIVES ( -- )
   s" undefined source rejects (stdin)" T-LABEL
   XPK-UNDEF-FORGE$ XPK-RUN-STDIN XPK-UNDEF-RC XPK-ASSERT-RC
   s" NOSUCH-EXPORT-SRC" XPK-ERR?
   s" undefined source rejects (--load)" T-LABEL
   XPK-UNDEF-FORGE$ XPK-RUN-LOAD XPK-UNDEF-RC XPK-ASSERT-RC
   s" missing name rejects" T-LABEL
   XPK-NONAME-FORGE$ XPK-RUN-STDIN XPK-REJECT-RC XPK-ASSERT-RC
   s" duplicate tail rejects with labeled diagnosis" T-LABEL
   XPK-DUP-FORGE$ XPK-RUN-STDIN XPK-REJECT-RC XPK-ASSERT-RC
   s" error[E-DUPLICATE-DEFINITION]" XPK-ERR?
   s" self-export rejects as duplicate" T-LABEL
   XPK-SELF-FORGE$ XPK-RUN-STDIN XPK-REJECT-RC XPK-ASSERT-RC
   s" primitive source rejects (E-EXPORT-PRIM)" T-LABEL
   XPK-PRIM-FORGE$ XPK-RUN-STDIN XPK-REJECT-RC XPK-ASSERT-RC
   s" error[E-EXPORT-PRIM]" XPK-ERR?  s" `dup`" XPK-ERR?
   s" a word the checker judges rejects (E-EXPORT-PRIM)" T-LABEL
   XPK-JUDGED-FORGE$ s" error[E-EXPORT-PRIM]" s" is built into the checker"
   XPK-REJECT-ALL
   s" a top-level keyword rejects (E-EXPORT-UNDEFINED)" T-LABEL
   XPK-KEYWORD-FORGE$ s" error[E-EXPORT-UNDEFINED]" s" is a top-level keyword"
   XPK-REJECT-ALL
   s" a used package's public rejects bare (E-EXPORT-UNDEFINED)" T-LABEL
   s" XW" XPK-USED-FORGE$ s" error[E-EXPORT-UNDEFINED]"
   s" is a public of used package `U`" XPK-REJECT-ALL
   s" a used package's public HOST rejects bare (E-EXPORT-UNDEFINED)" T-LABEL
   s" HOST" XPK-USED-FORGE$ s" error[E-EXPORT-UNDEFINED]"
   s" is a public of used package `U`" XPK-REJECT-ALL
   s" a used part's public rejects bare, named by its tail" T-LABEL
   XPK-PART-FORGE$ s" error[E-EXPORT-UNDEFINED]"
   s" is a public of used package `XPA:U`" XPK-REJECT-ALL
   s" a used package a part hides rejects bare, with the part to rename" T-LABEL
   XPK-HIDDEN-FORGE$ s" error[E-EXPORT-UNDEFINED]"
   s" = help: rename the part `XPH:U`, which hides package `U` here" XPK-REJECT-ALL
   s" a primitive spelled as a keyword rejects (E-EXPORT-PRIM)" T-LABEL
   XPK-CREATE-FORGE$ s" error[E-EXPORT-PRIM]" s" `create` is a primitive"
   XPK-REJECT-ALL
   s" exporting a trusted primitive rejects (E-EXPORT-UNSAFE)" T-LABEL
   XPK-UNSAFE-FORGE$ XPK-RUN-STDIN XPK-REJECT-RC XPK-ASSERT-RC
   s" error[E-EXPORT-UNSAFE]" XPK-ERR?  s" `patch32`" XPK-ERR?
   s" exporting a declaration names it (E-EXPORT-UNSAFE)" T-LABEL
   XPK-DECL-EXPORT-FORGE$ XPK-RUN-STDIN XPK-REJECT-RC XPK-ASSERT-RC
   s" error[E-EXPORT-UNSAFE]" XPK-ERR?  s" `ENUM` is a declaration" XPK-ERR?
   s" top-level EXPORT rejects (E-EXPORT-NO-PACKAGE)" T-LABEL
   XPK-TOP-FORGE$ XPK-RUN-STDIN XPK-REJECT-RC XPK-ASSERT-RC
   s" error[E-EXPORT-NO-PACKAGE]" XPK-ERR?
   s" top-level ?dup over an enum value is undefined" T-LABEL
   XPK-QDUP-FORGE$ XPK-RUN-STDIN XPK-UNDEF-RC XPK-ASSERT-RC
   s" error[E-UNDEFINED]: cannot find word `?dup`" XPK-ERR? ;

\ A name a global section's package does not publish is E-UNDEFINED.
: XPK-GLOBAL-UNDEFINED ( ^u8 n -- ) {: a:^ u:n :}
   a u T-LABEL
   a u XPK-GLOBAL-AFTER$ XPK-RUN-LOAD XPK-UNDEF-RC XPK-ASSERT-RC
   s" E-UNDEFINED" XPK-ERR? ;

\ Both --load and tools/check.f reject the source a u with the record e eu
\ and, unless it is empty, the caret line c cu.
: XPK-SEALED ( ^u8 n ^u8 n ^u8 n -- ) {: a:^ u:n e:^ eu:n c:^ cu:n :}
   a u XPK-RUN-LOAD
   e eu XPK-ERR?  cu 0 > IF c cu XPK-ERR? THEN  XPK-REJECT-RC XPK-ASSERT-RC
   XPK-RUN-CHECK
   e eu XPK-ERR?  cu 0 > IF c cu XPK-ERR? THEN  XPK-REJECT-RC XPK-ASSERT-RC ;

\ Both --load and tools/check.f accept the source a u.
: XPK-ACCEPT-BOTH ( ^u8 n -- )
   XPK-RUN-LOAD 0 XPK-ASSERT-RC
   XPK-RUN-CHECK 0 XPK-ASSERT-RC ;

: XPK-GLOBAL ( -- )
   s" a global section's word is bare and calls the package's private word" T-LABEL
   XPK-GLOBAL-FORGE$ XPK-RUN-LOAD 0 XPK-ASSERT-RC s" 42" XPK-OUT?
   s" tools/check.f accepts the global section" T-LABEL
   XPK-RUN-CHECK 0 XPK-ASSERT-RC
   s" PKG:P-GLOBAL" XPK-GLOBAL-UNDEFINED
   s" P-HELP" XPK-GLOBAL-UNDEFINED
   s" PKG:P-HELP" XPK-GLOBAL-UNDEFINED
   s" types declared in a global section" T-LABEL
   XPK-GLOBAL-TYPES$ XPK-RUN-LOAD 0 XPK-ASSERT-RC
   s" 41" XPK-OUT?  s" 42" XPK-OUT?  s" 43" XPK-OUT?
   s" tools/check.f accepts types declared in a global section" T-LABEL
   XPK-RUN-CHECK 0 XPK-ASSERT-RC
   s" global outside a package is rejected" T-LABEL
   XPK-GLOBAL-OUTSIDE$ XPK-RUN-LOAD XPK-REJECT-RC XPK-ASSERT-RC
   s" E-PKG-CONTEXT" XPK-ERR?
   s" `;package` outside a package is rejected by every reader" T-LABEL
   XPK-END-PACKAGE-OUTSIDE$ XPK-RUN-LOAD XPK-REJECT-RC XPK-ASSERT-RC
   s" E-PKG-CONTEXT" XPK-ERR?
   XPK-RUN-CHECK XPK-REJECT-RC XPK-ASSERT-RC  s" E-PKG-CONTEXT" XPK-ERR?
   XPK-RUN-VERIFY XPK-REJECT-RC XPK-ASSERT-RC  s" E-PKG-CONTEXT" XPK-ERR?
   s" `;package` of a package rendered text opened is accepted" T-LABEL
   XPK-EVAL-OPEN$ XPK-ACCEPT-BOTH
   s" `public` in a package rendered text opened is accepted" T-LABEL
   XPK-EVAL-PUBLIC$ XPK-ACCEPT-BOTH
   s" a sealed package's global section takes no definition" T-LABEL
   XPK-GLOBAL-SEALED$ XPK-RUN-LOAD XPK-REJECT-RC XPK-ASSERT-RC
   s" E-SEALED-PACKAGE" XPK-ERR?
   s" tools/check.f rejects a sealed package's global definition" T-LABEL
   XPK-RUN-CHECK XPK-REJECT-RC XPK-ASSERT-RC
   s" E-SEALED-PACKAGE" XPK-ERR?
   s" a sealed package takes no qualified definition" T-LABEL
   XPK-SEALED-QUALIFIED$ s" E-SEALED-PACKAGE" s" " XPK-SEALED
   s" a sealed package's private word takes no EXPORT" T-LABEL
   XPK-SEALED-EXPORT$
   s" error[E-SEALED-PACKAGE]: `XSA:HID` is private to `XSA`, which is sealed"
   s" |        ^^^^^^^ private to a sealed package" XPK-SEALED
   s" the verifier alone rejects it" T-LABEL
   XPK-RUN-VERIFY
   s" error[E-SEALED-PACKAGE]: `XSA:HID` is private to `XSA`, which is sealed" XPK-ERR?
   XPK-REJECT-RC XPK-ASSERT-RC
   s" a sealed package's EXPORT is judged at its seal before its source" T-LABEL
   s" " s" EXPORT NOPE:X" XPK-SEALED-LINE$
   s" error[E-SEALED-PACKAGE]: `NOPE:X` would change a sealed package"
   s" |        ^^^^^^ sealed" XPK-SEALED
   s" a sealed package's part takes no global definition" T-LABEL
   s" : LATE ( -- n ) HID ;" XPK-SEALED-PART$ s" E-SEALED-PACKAGE" s" " XPK-SEALED
   s" a sealed package's part takes no global EXPORT" T-LABEL
   s" EXPORT U:UW" XPK-SEALED-PART$ s" E-SEALED-PACKAGE" s" " XPK-SEALED
   s" a sealed package's global section takes no STRUCTURE" T-LABEL
   XPK-SEALED-STRUCTURE$
   s" error[E-SEALED-PACKAGE]: `gbox` would change a sealed package" s" " XPK-SEALED
   s" a sealed package takes no DEFTYPE" T-LABEL
   XPK-SEALED-DEFTYPE$
   s" error[E-SEALED-PACKAGE]: `GDT` would change a sealed package" s" " XPK-SEALED
   s" a sealed package takes no DEFTYPE without a base" T-LABEL
   XPK-SEALED-DEFTYPE-BARE$
   s" error[E-SEALED-PACKAGE]: `GDU` would change a sealed package" s" " XPK-SEALED
   s" a sealed package takes no STRUCTURE whose header does not parse, named as written" T-LABEL
   XPK-SEALED-PARAM$
   s" error[E-SEALED-PACKAGE]: `gbx` would change a sealed package"
   s" |           ^^^ sealed" XPK-SEALED
   s" a sealed package takes no ENUM" T-LABEL
   s" global" s" ENUM ge ge-a ge-b ;ENUM" XPK-SEALED-LINE$
   s" error[E-SEALED-PACKAGE]: `ge` would change a sealed package" s" " XPK-SEALED
   s" a sealed package takes no SUMTYPE" T-LABEL
   s" " s" SUMTYPE gsum VARIANT ga ;VARIANT VARIANT gb ;VARIANT ;SUMTYPE" XPK-SEALED-LINE$
   s" error[E-SEALED-PACKAGE]: `gsum` would change a sealed package" s" " XPK-SEALED
   s" a sealed package takes no parametric NEWTYPE" T-LABEL
   s" " s" NEWTYPE gp<a> a" XPK-SEALED-LINE$
   s" error[E-SEALED-PACKAGE]: `gp` would change a sealed package" s" " XPK-SEALED
   s" a sealed package takes no qualified STRUCTURE" T-LABEL
   s" " s" STRUCTURE q:s FIELD v n ;STRUCTURE" XPK-SEALED-LINE$
   s" error[E-SEALED-PACKAGE]: `q:s` would change a sealed package" s" " XPK-SEALED
   s" a sealed package takes no DYNAMIC-BUFFER, named at its name" T-LABEL
   s" " s" DYNAMIC-BUFFER DBX u8" XPK-SEALED-LINE$
   s" error[E-SEALED-PACKAGE]: `DBX` would change a sealed package"
   s" |                ^^^ sealed" XPK-SEALED
   s" a sealed package takes no HEAP, named as written" T-LABEL
   s" " s" HEAP HX n" XPK-SEALED-LINE$
   s" error[E-SEALED-PACKAGE]: `HX` would change a sealed package"
   s" |      ^^ sealed" XPK-SEALED
   s" a sealed package takes no ARENA, named as written" T-LABEL
   s" " s" ARENA AX n" XPK-SEALED-LINE$
   s" error[E-SEALED-PACKAGE]: `AX` would change a sealed package"
   s" |       ^^ sealed" XPK-SEALED
   s" a sealed package takes no HEAP, rejected before its type" T-LABEL
   s" " s" HEAP HX nosuch" XPK-SEALED-LINE$
   s" error[E-SEALED-PACKAGE]: `HX` would change a sealed package"
   s" |      ^^ sealed" XPK-SEALED
   s" a sealed package takes no TYPED-VARIABLE, rejected before its type" T-LABEL
   s" " s" TYPED-VARIABLE TV nosuch" XPK-SEALED-LINE$
   s" error[E-SEALED-PACKAGE]: `TV` would change a sealed package"
   s" |                ^^ sealed" XPK-SEALED
   s" a storage name that ends in `:` rejects under both loaders" T-LABEL
   SB-RESET s" HEAP P: n" XPK-LINE SB$
   s" error[E-BAD-STORAGE]: the name `P:` ends in `:`" s" " XPK-SEALED
   s" a name whose final `:` dropped still ends in one gets a rename" T-LABEL
   SB-RESET s" TYPED-VARIABLE a:: n" XPK-LINE SB$
   s" error[E-BAD-STORAGE]: the name `a::` ends in `:`"
   s" rename `a::` with at most one inner `:` and none at its end" XPK-SEALED
   s" a sealed package takes no CWORK-VARIABLE, rejected before its type" T-LABEL
   s" " s" 0 CWORK-VARIABLE CV nosuch" XPK-SEALED-LINE$
   s" error[E-SEALED-PACKAGE]: `CV` would change a sealed package"
   s" |                  ^^ sealed" XPK-SEALED
   s" a DEFER-LAYOUT-BUFFER's taken NAME-BIND rejects before its type" T-LABEL
   SB-RESET s" : DL-BIND ( -- n ) 1 ;" XPK-LINE
   s" DEFER-LAYOUT-BUFFER DL nosuch" XPK-LINE SB$
   s" error[E-DUPLICATE-DEFINITION]: `DL-BIND` is already defined in this wordlist"
   s" |                     ^^ already defined" XPK-SEALED
   s" a sealed package takes no DEFTYPE over a base, named in full" T-LABEL
   s" " s" DEFTYPE GD<a> n" XPK-SEALED-LINE$
   s" error[E-SEALED-PACKAGE]: `GD<a>` would change a sealed package"
   s" |         ^^^^^ sealed" XPK-SEALED
   s" a sealed package takes no parametric DEFTYPE header, named at the family" T-LABEL
   s" " s" DEFTYPE GD<a>" XPK-SEALED-LINE$
   s" error[E-SEALED-PACKAGE]: `GD` would change a sealed package"
   s" |         ^^ sealed" XPK-SEALED
   s" a sealed package takes no ARENA, rejected before its taken words" T-LABEL
   SB-RESET s" package P" XPK-LINE  s" private" XPK-LINE
   s" : AX-NEW ( -- n ) 1 ;" XPK-LINE  s" seal-package" XPK-LINE
   s" ARENA AX n" XPK-LINE  s" ;package" XPK-LINE SB$
   s" error[E-SEALED-PACKAGE]: `AX` would change a sealed package"
   s" |       ^^ sealed" XPK-SEALED
   s" a sealed constructor package takes no STRUCTURE's MAKE, named at the family" T-LABEL
   XPK-SEALED-CTOR$
   s" error[E-SEALED-PACKAGE]: `Z-PK:MAKE` would change a sealed package"
   s" |           ^^ sealed" XPK-SEALED
   s" a sealed package's part rejects a qualified STRUCTURE as an unsealed one does" T-LABEL
   XPK-SEALED-PART-STRUCT$ s" bad structure declaration `B:s`" s" " XPK-SEALED
   s" a sealed package takes no MUTUAL" T-LABEL
   XPK-SEALED-MUTUAL$
   s" error[E-SEALED-PACKAGE]: `ma` would change a sealed package" s" " XPK-SEALED
   s" a sealed constructor package takes no SUMTYPE's variant, named at the family" T-LABEL
   XPK-SEALED-SUM-CTOR$
   s" error[E-SEALED-PACKAGE]: `Z-PK:PA` would change a sealed package"
   s" |         ^^ sealed" XPK-SEALED
   s" a sealed package takes no MUTUAL member with a header, named at its family" T-LABEL
   XPK-SEALED-MUTUAL-PARAM$
   s" error[E-SEALED-PACKAGE]: `pa` would change a sealed package"
   s" |        ^^ sealed" XPK-SEALED
   s" a MUTUAL's entries are judged in order, a bad name before a sealed one" T-LABEL
   XPK-SEALED-MUTUAL-ORDER$
   s" error[E-BAD-DECLARATION]: bad mutual declaration `Pa`"
   s" |        ^^ name must be a lowercase family tail" XPK-SEALED
   s" a sealed package takes no CALLBACK:, named at its name" T-LABEL
   XPK-SEALED-CALLBACK$
   s" error[E-SEALED-PACKAGE]: `CB` would change a sealed package"
   s" |           ^^ sealed" XPK-SEALED
   s" a CALLBACK: whose name is taken, named at its name" T-LABEL
   XPK-TAKEN-CALLBACK$
   s" error[E-DUPLICATE-DEFINITION]: `CB` is already defined in this wordlist"
   s" |           ^^ already defined" XPK-SEALED
   s" a CALLBACK: with no name, named at its '('" T-LABEL
   XPK-NAMELESS-CALLBACK$
   s" error[E-FFI-SYNTAX]: expected a callback name, found '('"
   s" |           ^ the definer was reading here" XPK-SEALED
   s" a CALLBACK: whose derived word is taken, named at its name" T-LABEL
   SB-RESET s" require src/lib/ffi-callback.f" XPK-LINE
   s" : CB-BODY ( -- n ) 9 ;" XPK-LINE  s" CALLBACK: CB ( -- ) ;CALLBACK" XPK-LINE SB$
   s" error[E-DUPLICATE-DEFINITION]: `CB-BODY` is already defined in this wordlist"
   s" |           ^^ already defined" XPK-SEALED
   s" an engine word nothing used yet takes no redefinition" T-LABEL
   SB-RESET s" : DEFTYPE ( -- n ) 7 ;" XPK-LINE  s" DEFTYPE . cr" XPK-LINE SB$
   s" error[E-DUPLICATE-DEFINITION]: `DEFTYPE` is already defined in this wordlist"
   s" |   ^^^^^^^ already defined" XPK-SEALED
   s" an export whose tail the package already defines, named at its tail" T-LABEL
   SB-RESET
   s" package U" XPK-LINE  s" public" XPK-LINE  s" : XW ( -- n ) 7 ;" XPK-LINE
   s" ;package" XPK-LINE
   s" package Q" XPK-LINE  s" public" XPK-LINE  s" : XW ( -- n ) 1 ;" XPK-LINE
   s" EXPORT U:XW" XPK-LINE  s" ;package" XPK-LINE  s" 1 . cr" XPK-LINE SB$
   s" error[E-DUPLICATE-DEFINITION]: `XW` is already defined in this wordlist"
   s" |          ^^ already defined" XPK-SEALED
   s" an export whose clause the package already defines, named at its tail" T-LABEL
   SB-RESET
   s" package U" XPK-LINE  s" public" XPK-LINE
   s" : MK ( n -- ) create , does> ( -- n ) @ ;" XPK-LINE  s" ;package" XPK-LINE
   s" package Q" XPK-LINE  s" public" XPK-LINE  s" : MK;does ( -- n ) 1 ;" XPK-LINE
   s" EXPORT U:MK" XPK-LINE  s" ;package" XPK-LINE  s" 1 . cr" XPK-LINE SB$
   s" error[E-DUPLICATE-DEFINITION]: `MK;does` is already defined in this wordlist"
   s" |          ^^ its clause's name is taken" XPK-SEALED
   XPK-RUN-VERIFY  s" |          ^^ its clause's name is taken" XPK-ERR?  XPK-REJECT-RC XPK-ASSERT-RC
   s" a sealed package's part defines in its private and public sections" T-LABEL
   XPK-PART-SECTIONS$ XPK-ACCEPT-BOTH
   s" a sealed package's part defines under a using" T-LABEL
   XPK-PART-USING$ XPK-ACCEPT-BOTH
   s" a sealed package's part defines B:X into itself" T-LABEL
   XPK-PART-QUALIFIED$ XPK-ACCEPT-BOTH
   s" a word spelled as a top-level keyword exports" T-LABEL
   SB-RESET
   s" package XP" XPK-LINE  s" : HOST ( -- n ) 7 ;" XPK-LINE
   s" public" XPK-LINE  s" EXPORT HOST" XPK-LINE  s" ;package" XPK-LINE
   s" XP:HOST . cr" XPK-LINE  SB$ XPK-ACCEPT-BOTH ;

\ The declaration words are their packages' global words over private
\ implementations: a checked body reaches no implementation, and may not call
\ the declaration word itself.
: XPK-DECL-REJECT ( ^u8 n ^u8 n -- ) {: a:^ u:n e:^ eu:n :}
   a u T-LABEL
   SB-RESET a u XPK-LINE SB$ XPK-RUN-LOAD XPK-REJECT-RC XPK-ASSERT-RC
   e eu XPK-ERR? ;

: XPK-DECL ( -- )
   s" : XPK-DECL-ENUM ( -- ) ENUM-DECL:DECLARE-ENUM ;" s" E-UNDEFINED" XPK-DECL-REJECT
   s" : XPK-DECL-SD ( -- ) STRUCTURE-DECL:DECLARE-STRUCTURE ;" s" E-UNDEFINED" XPK-DECL-REJECT
   s" : XPK-DECL-TV ( -- ) LBUF:TYPED-VARIABLE ;" s" E-UNDEFINED" XPK-DECL-REJECT
   s" : XPK-DECL-DS ( -- ) DYNAMIC-STORAGE:DECLARE ;" s" E-UNDEFINED" XPK-DECL-REJECT
   s" : XPK-DECL-AR ( -- ) ARENA-DECL:RUN ;" s" E-UNDEFINED" XPK-DECL-REJECT
   s" : XPK-DECL-SUM ( -- ) TYPE-DECL:TDECL-SUMTYPE ;" s" E-UNDEFINED" XPK-DECL-REJECT
   s" : XPK-DECL-NT ( -- ) STRUCTURE-DECL:SD-WRAP ;" s" E-UNDEFINED" XPK-DECL-REJECT
   s" : XPK-DECL-ENUM ( -- ) ENUM ;" s" E-UNSAFE" XPK-DECL-REJECT
   s" : XPK-DECL-SUMT ( -- ) SUMTYPE ;" s" E-UNSAFE" XPK-DECL-REJECT
   s" : XPK-DECL-USER ( -- ) TASK:+USER ;" T-LABEL
   SB-RESET  s" require src/lib/task.f" XPK-LINE
   s" : XPK-DECL-USER ( -- ) TASK:+USER ;" XPK-LINE
   SB$ XPK-RUN-LOAD XPK-REJECT-RC XPK-ASSERT-RC
   s" E-UNSAFE" XPK-ERR?
   s" : XPK-DECL-RX ( -- ) XU:+USER ;" T-LABEL
   SB-RESET  s" require src/lib/task.f" XPK-LINE
   s" package XU public" XPK-LINE  s" export TASK:+USER" XPK-LINE
   s" ;package" XPK-LINE  s" : XPK-DECL-RX ( -- ) XU:+USER ;" XPK-LINE
   SB$
   s" error[E-UNSAFE]: `XU:+USER` reads source or changes the compiler's or checker's state, which no stack effect describes"
   s" |                      ^^^^^^^^ not allowed in a checked body" XPK-SEALED ;

\ The source ctor and its alias must behave alike at top level: both run and
\ leave their two-cell value, and a primitive over it (dup) is rejected alike
\ (E-LAYOUT-VALUE, same rc), proving the alias carries the constructor's effect; equality is
\ the contract.
variable XPK-WIDE-EXITED   variable XPK-WIDE-RC

: XPK-WIDE-PARITY ( -- )
   s" interpret wide gate: source ctor" T-LABEL
   XPK-CTOR-WIDE-SRC-FORGE$ XPK-RUN-STDIN
   s" ran-wide" XPK-OUT?
   s" error[E-LAYOUT-VALUE]" XPK-ERR?  s" `dup`" XPK-ERR?
   XPK-EXITED @ XPK-WIDE-EXITED !  XPK-RC @ XPK-WIDE-RC !
   XPK-RC @ 0 T<>
   s" interpret wide gate: alias behaves identically" T-LABEL
   XPK-CTOR-WIDE-ALIAS-FORGE$ XPK-RUN-STDIN
   s" ran-wide" XPK-OUT?
   s" error[E-LAYOUT-VALUE]" XPK-ERR?  s" `dup`" XPK-ERR?
   XPK-EXITED @ TTRUE  XPK-WIDE-EXITED @ TTRUE
   XPK-RC @ XPK-WIDE-RC @ T= ;

: XPK-PT-LINES ( -- )
   s" package PT" XPK-LINE
   s" public" XPK-LINE
   s" STRUCTURE point FIELD x n FIELD y n ;STRUCTURE" XPK-LINE
   s" : AT ( n n -- point ) PT-POINT:MAKE ;" XPK-LINE
   s" : SMASH ( point -- point ) 1 throw ;" XPK-LINE
   s" ;package" XPK-LINE
   s" 2 3 PT:AT" XPK-LINE ;

\ A point, then the primitive a u on its own line, run; the record names a u.
: XPK-PT-XPORT ( ^u8 n -- ) {: a:^ u:n :}
   a u T-LABEL
   SB-RESET XPK-PT-LINES  a u XPK-LINE  SB$ XPK-RUN-STDIN
   XPK-REJECT-RC XPK-ASSERT-RC  s" error[E-LAYOUT-VALUE]: `" XPK-ERR?
   SB-RESET s" `" SB-APPEND  a u SB-APPEND
   s" ` at top level would take part of a value held in more than one cell" SB-APPEND
   SB$ XPK-ERR? ;

\ `constant` over a point is rejected at the keyword, before it takes a cell.
: XPK-PT-CONSTANT ( -- )
   s" top-level constant over a point" T-LABEL
   SB-RESET XPK-PT-LINES  s" constant K" XPK-LINE  SB$ XPK-RUN-STDIN
   XPK-REJECT-RC XPK-ASSERT-RC
   s" error[E-LAYOUT-VALUE]: `constant` would take one cell of `pt:point`, a value held in more than one cell"
   XPK-ERR?
   s"  --> <input>:8:1" XPK-ERR?
   s" 8 | constant K" XPK-ERR?
   s"   | ^^^^^^^^ takes one cell" XPK-ERR?
   s"   = help: define a word that leaves the whole value, `: K ( -- pt:point ) ... ;`, in place of the constant"
   XPK-ERR? ;

\ `constant` on an empty stack is rejected at the keyword, before it takes a cell.
: XPK-EMPTY-CONSTANT ( -- )
   s" top-level constant on an empty stack" T-LABEL
   SB-RESET s" constant K" XPK-LINE  SB$ XPK-RUN-STDIN
   XPK-REJECT-RC XPK-ASSERT-RC
   s" error[E-INPUT-UNDERFLOW]: `constant` takes the value `K` names, and the stack is empty"
   XPK-ERR?
   s"  --> <input>:1:1" XPK-ERR?
   s" 1 | constant K" XPK-ERR?
   s"   | ^^^^^^^^ takes one cell" XPK-ERR?
   s"   = help: push the value before `constant`, as `0 constant K`" XPK-ERR? ;

\ A `constant` that took the cell above a point leaves the point on top: the
\ next `constant` is rejected too.
: XPK-PT-CONSTANTS ( -- )
   s" top-level constant after a constant over a point" T-LABEL
   SB-RESET XPK-PT-LINES
   s" 7 constant SEVEN constant K" XPK-LINE  SB$ XPK-RUN-STDIN
   XPK-REJECT-RC XPK-ASSERT-RC
   s" error[E-LAYOUT-VALUE]: `constant` would take one cell of `pt:point`, a value held in more than one cell"
   XPK-ERR?
   s"  --> <input>:8:18" XPK-ERR?
   s" 8 | 7 constant SEVEN constant K" XPK-ERR?
   s"   |                  ^^^^^^^^ takes one cell" XPK-ERR?
   s"   = help: define a word that leaves the whole value, `: K ( -- pt:point ) ... ;`, in place of the constant"
   XPK-ERR? ;

\ At top level each transport primitive over a point, and `constant` over a
\ point or on an empty stack, is rejected before it runs, naming itself.
: XPK-WIDE-TAKE ( -- )
   s" dup" XPK-PT-XPORT  s" swap" XPK-PT-XPORT  s" over" XPK-PT-XPORT
   s" nip" XPK-PT-XPORT  s" tuck" XPK-PT-XPORT  s" rot" XPK-PT-XPORT
   s" -rot" XPK-PT-XPORT  s" 2dup" XPK-PT-XPORT  s" 2drop" XPK-PT-XPORT
   s" 2swap" XPK-PT-XPORT  s" 2over" XPK-PT-XPORT  s" 2>r" XPK-PT-XPORT
   XPK-PT-CONSTANT XPK-EMPTY-CONSTANT XPK-PT-CONSTANTS ;

: XPK-PT-DROP ( ^u8 n ^u8 n -- ) {: w:^ v:n a:^ u:n :}
   w v T-LABEL
   SB-RESET XPK-PT-LINES  a u XPK-LINE  SB$ XPK-RUN-STDIN
   0 XPK-ASSERT-RC  s" 100" XPK-OUT? ;

: XPK-LT-DROP ( -- )
   s" top-level drop of a linear token" T-LABEL
   SB-RESET
   s" package LT" XPK-LINE
   s" public" XPK-LINE
   s" DEFLINEAR LT:tok" XPK-LINE
   s" private" XPK-LINE
   s" LINEAR: MINT ( n -- LT:tok )" XPK-LINE
   s" public" XPK-LINE
   s" : NEW ( n -- LT:tok ) MINT ;" XPK-LINE
   s" ;package" XPK-LINE
   s" 5 LT:NEW drop" XPK-LINE
   SB$ XPK-RUN-STDIN
   XPK-REJECT-RC XPK-ASSERT-RC
   s" error[E-LINEAR-DROP]: a linear value is discarded here" XPK-ERR?
   s" ^^^^ linear value dropped" XPK-ERR? ;

\ An item of open width has no cell count to drop.
: XPK-OPEN-DROP ( -- )
   s" top-level drop of a value of open width" T-LABEL
   SB-RESET
   s" SUMTYPE myopt<a> VARIANT none ;VARIANT VARIANT some a ;VARIANT ;SUMTYPE" XPK-LINE
   s" : XPK-OPEN ( -- myopt<a> ) MYOPT:NONE ;" XPK-LINE
   s" XPK-OPEN drop" XPK-LINE
   SB$ XPK-RUN-STDIN
   XPK-REJECT-RC XPK-ASSERT-RC
   s" error[E-LAYOUT-VALUE]: `drop` cannot drop `myopt<a>`, a value whose cell count is not known here"
   XPK-ERR?
   s"  --> <input>:3:10" XPK-ERR?
   s" 3 | XPK-OPEN drop" XPK-ERR?
   s"   |          ^^^^ cell count not known" XPK-ERR?
   s"   = help: leave the value from a word whose effect gives `myopt<a>` concrete type arguments, as `( -- myopt<n> )`"
   XPK-ERR? ;

\ `drop` over a point, and over one a top-level `catch` left stale, empties the
\ row, so `depth` runs; a linear token's `drop` is rejected as in a body, and
\ one of open width is rejected.
: XPK-WIDE-DROP ( -- )
   s" top-level drop of a point" s" drop depth 100 + . cr" XPK-PT-DROP
   s" top-level drop of a stale point"
   s" ' PT:SMASH catch drop drop depth 100 + . cr" XPK-PT-DROP
   XPK-LT-DROP  XPK-OPEN-DROP ;

\ A user's global `drop`, defined after `undefine drop`, runs once, as any word.
: XPK-USER-DROP ( -- )
   s" a user's global drop runs once at top level" T-LABEL
   SB-RESET
   s" undefine drop : drop ( n n -- ) + . cr ;" XPK-LINE
   s" 1 2 drop depth 100 + . cr" XPK-LINE
   SB$ XPK-RUN-STDIN
   0 XPK-ASSERT-RC  s" 3" XPK-OUT?  s" 100" XPK-OUT? ;

\ A word whose effect carries the point keeps it whole at top level: run by
\ `evaluate`, by a tick and `execute`, and as a `does>` child, then taken by
\ one word, so the load ends clean.
: XPK-WIDE-KEEP ( -- )
   s" top-level evaluate, execute and a does> child keep a point whole" T-LABEL
   SB-RESET XPK-PT-LINES
   s" : XPK-SAME ( PT:point -- PT:point ) ;" XPK-LINE
   s" : XPK-SUM ( PT:point -- n ) PT-POINT:UNMAKE + ;" XPK-LINE
   s" : XPK-KEEPS ( -- ) create does> ( PT:point -- PT:point ) drop ;" XPK-LINE
   s" XPK-KEEPS XPK-KEPT" XPK-LINE
   S\" s\" XPK-SAME\" evaluate" XPK-LINE
   s" ' XPK-SAME execute" XPK-LINE
   s" XPK-KEPT" XPK-LINE
   s" XPK-SUM 100 + . cr" XPK-LINE
   SB$ XPK-RUN-STDIN
   0 XPK-ASSERT-RC  s" 105" XPK-OUT? ;

T-RESET
XPK-PREPARE
XPK-POSITIVES
XPK-NEGATIVES
XPK-WIDE-PARITY
XPK-WIDE-TAKE
XPK-WIDE-DROP
XPK-USER-DROP
XPK-WIDE-KEEP
XPK-GLOBAL
XPK-DECL
XPK-CLEANUP
T-REPORT
s" export-package: ok" type cr
