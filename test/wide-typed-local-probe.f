\ wide-typed-local-probe.f - a wide arity-0 layout value bound to a typed local
\ (dot habu-bind-a-wide-bc67d207). Run:
\     bin/hb --load test/wide-typed-local-probe.f
\
\ The annotation records the layout's TOP (tag) hidden term, the bind records
\ the bundle's full width in LOCW, and every reference reloads all physical
\ cells. This file proves that at RUNTIME: a two-cell and a three-cell record
\ survive a typed local whole through a return, an `if` arm and a `?do` loop,
\ a bundle built inside the body binds the same way, a local read of a W=2 sum
\ still carries its family so `MATCH` dispatches on it, and the annotation is
\ still asserted (a wrong family, a scalar annotation, a parametric spelling
\ and an arity>0 tail all stay refused).
\ It is registered twice in test/gate-stdlib-cases.f, once per tier.

require lib/test.f

package WLP
public

STRUCTURE pair 0
   FIELD left n
   FIELD right n
;STRUCTURE

\ the same physical shape as `pair` under another name: identity is NOMINAL
STRUCTURE pair2 0
   FIELD left n
   FIELD right n
;STRUCTURE

STRUCTURE trip 0
   FIELD a n
   FIELD b n
   FIELD c n
;STRUCTURE

\ a W=2 SUM that is still arity 0, so a local may name it
SUMTYPE res 0
   VARIANT ok  n ;VARIANT
   VARIANT err n ;VARIANT
;SUMTYPE

\ parametric: NOT nameable in a local
SUMTYPE opt 1
   VARIANT some a ;VARIANT
   VARIANT none  ;VARIANT
;SUMTYPE

private

\ ---- the capability: a typed local holds the whole layout value -------------
: WL-ID ( pair -- pair )                  \ returned whole, both cells reloaded
   {: p:pair :}
   p ;

: WL-SUM ( pair -- n )                    \ destructured through the local
   {: p:pair :}
   p WLP-PAIR:UNMAKE + ;

: WL-PICK ( pair bool -- n )              \ reference inside an `if` arm
   {: p:pair f:bool :}
   f if p WLP-PAIR:UNMAKE nip else p WLP-PAIR:UNMAKE drop then ;

: WL-LOOP ( pair n -- n )                 \ reference inside a `?do` body
   {: p:pair k:n :}
   0 k 0 ?do p WLP-PAIR:UNMAKE drop + loop ;

: WL-AFTER ( pair n -- n )                \ reference after a `?do` loop
   {: p:pair k:n :}
   0 k 0 ?do 1 + loop
   p WLP-PAIR:UNMAKE + + ;

: WL-TRIP-ID ( trip -- trip )             \ three cells, all reloaded
   {: t:trip :}
   t ;

: WL-TRIP-SUM ( trip -- n )
   {: t:trip :}
   t WLP-TRIP:UNMAKE + + ;

: WL-MADE ( n n -- n )                    \ bundle built INSIDE the body
   WLP-PAIR:MAKE {: p:pair :}
   p WLP-PAIR:UNMAKE + ;

: WL-MATCH ( res -- n )                   \ a local read still carries the family
   {: r:res :}
   r MATCH WLP:res
      ok  OF 1 + ENDOF
      err OF 1 - ENDOF
   ;MATCH ;

public
: MAIN ( -- )
   7 11 WLP-PAIR:MAKE WL-ID WLP-PAIR:UNMAKE
   11 T= 7 T=                             \ right, then left: the whole value
   13 17 WLP-PAIR:MAKE WL-SUM 30 T=
   3 5 WLP-PAIR:MAKE true WL-PICK 5 T=
   3 5 WLP-PAIR:MAKE false WL-PICK 3 T=
   4 9 WLP-PAIR:MAKE 3 WL-LOOP 12 T=
   4 9 WLP-PAIR:MAKE 0 WL-LOOP 0 T=
   4 9 WLP-PAIR:MAKE 2 WL-AFTER 15 T=
   1 2 3 WLP-TRIP:MAKE WL-TRIP-ID WLP-TRIP:UNMAKE
   3 T= 2 T= 1 T=
   20 30 40 WLP-TRIP:MAKE WL-TRIP-SUM 90 T=
   3 4 WL-MADE 7 T=
   10 WLP-RES:OK WL-MATCH 11 T=
   10 WLP-RES:ERR WL-MATCH 9 T= ;

private
\ ---- checker verdicts: the annotation is asserted, not ignored --------------
: YES ( ptr u8 n -- )   CHECK-QUIET-CANDIDATE! -1 T= ;
: NO  ( ptr u8 n -- )   CHECK-QUIET-CANDIDATE!  0 T= ;

s" WLC-WIDE ( pair -- pair ) {: p:pair :} p " YES
s" WLC-TRIP ( trip -- trip ) {: t:trip :} t " YES
s" WLC-UNTYPED ( opt<n> -- n ) {: o :} 0 " YES
s" WLC-WRONG-FAM ( pair -- n ) {: p:trip :} 0 " NO
s" WLC-WRONG-W2 ( pair -- n ) {: p:res :} 0 " NO   \ same width, other family
\ identity is nominal: pair2 has the SAME two n fields and still does not bind
\ a `pair` local, while its own annotation does.
s" WLC-NOMINAL ( pair2 -- pair ) {: p:pair :} p " NO
s" WLC-NOMINAL-OK ( pair2 -- pair2 ) {: p:pair2 :} p " YES
s" WLC-SCALAR-ANN ( pair -- n ) {: p:n :} 0 " NO
s" WLC-PARAM-ANN ( opt<n> -- n ) {: o:opt<n> :} 0 " NO
s" WLC-ARITY-ANN ( opt<n> -- n ) {: o:opt :} 0 " NO

;package

WLP:MAIN
T-REPORT
