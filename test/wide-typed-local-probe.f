\ wide-typed-local-probe.f - a wide arity-0 layout value bound to a typed local
\ (dot habu-bind-a-wide-bc67d207). Run:
\     bin/hb --load test/wide-typed-local-probe.f
\
\ The annotation records the layout's TOP (tag) hidden term, the bind records
\ the bundle's full width in LOCW, and every reference reloads all physical
\ cells. This file proves that at RUNTIME: a two-cell and a three-cell record
\ survive a typed local whole through a return, an `if` arm and a `?do` loop,
\ a bundle built inside the body binds the same way, a local read of a W=2 sum
\ still carries its family so `MATCH` dispatches on it, a PARAMETRIC instance
\ (`opt<n>`) rides the same path, and the annotation is still asserted (a wrong
\ family, a scalar annotation, a wrong argument and a bare arity>0 tail all
\ stay refused). A 26-cell record, wider than a routine's registers, keeps every
\ cell through swap, rot, -rot, over and nip against narrower values and through
\ a call's take-back, and 33 reals, more than the floating registers, sum
\ exactly.
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

\ twenty-six cells: with a pair, more than the registers a routine may destroy
STRUCTURE wide 0
   FIELD c0 n
   FIELD c1 n
   FIELD c2 n
   FIELD c3 n
   FIELD c4 n
   FIELD c5 n
   FIELD c6 n
   FIELD c7 n
   FIELD c8 n
   FIELD c9 n
   FIELD c10 n
   FIELD c11 n
   FIELD c12 n
   FIELD c13 n
   FIELD c14 n
   FIELD c15 n
   FIELD c16 n
   FIELD c17 n
   FIELD c18 n
   FIELD c19 n
   FIELD c20 n
   FIELD c21 n
   FIELD c22 n
   FIELD c23 n
   FIELD c24 n
   FIELD c25 n
;STRUCTURE

\ a W=2 SUM that is still arity 0, so a local may name it
SUMTYPE res 0
   VARIANT ok  n ;VARIANT
   VARIANT err n ;VARIANT
;SUMTYPE

\ parametric, and nameable in a local: an annotation is read by the SIGNATURE
\ type grammar (dot habu-parse-local-annotations)
SUMTYPE opt 1
   VARIANT some a ;VARIANT
   VARIANT none  ;VARIANT
;SUMTYPE

TYPED-VARIABLE WL-CELL pair

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

\ ---- the parametric instance rides the same path ---------------------------
: WL-OPT-ID ( opt<n> -- opt<n> )          \ a PARAMETRIC bundle, returned whole
   {: o:opt<n> :}
   o ;

: WL-OPT-MATCH ( opt<n> -- n )            \ MATCH dispatches on a parametric local read
   {: o:opt<n> :}
   o MATCH WLP:opt
      some OF 2 * ENDOF
      none OF 0 ENDOF
   ;MATCH ;

\ ---- a bundle and a `:ptr` local in ONE group -------------------------------
\ The group path binds the pointer's inferred pointee under the same transport
\ rule as a group without a bundle (dot habu-bind-a-ptr-ef9f15dc): the pointee
\ here is a layout, which a bare `ptr` local only absorbs in transport.
: WL-GROUP ( pair ptr pair -- n )         \ pointer above the bundle
   {: p:pair q:ptr :}
   p WLP-PAIR:UNMAKE +  q @ WLP-PAIR:UNMAKE +  + ;

: WL-GROUP-BELOW ( ptr pair pair -- n )   \ pointer below the bundle
   {: q:ptr p:pair :}
   p WLP-PAIR:UNMAKE +  q @ WLP-PAIR:UNMAKE +  + ;

\ ---- a record wider than the register pool, shuffled whole -----------------
\ Each shuffle moves more cells than a routine has registers, so tier 1 loads
\ them in a run it cannot hold whole and stores each value it puts away right
\ after that value's own load (src/compiler/native/regalloc.f MB-ANCHOR).
: WL-SWAP2 ( wide pair -- pair wide ) swap ;
: WL-UNSWAP2 ( pair wide -- wide pair ) swap ;
: WL-SWAP1 ( wide n -- n wide ) swap ;
: WL-ROT ( wide n n -- n n wide ) rot ;
: WL-MROT ( n n wide -- wide n n ) -rot ;
: WL-OVER ( wide n -- wide n wide ) over ;
: WL-NIP ( n wide -- wide ) nip ;

: WL-WIDE ( -- wide )
   100 101 102 103 104 105 106 107 108 109 110 111 112
   113 114 115 116 117 118 119 120 121 122 123 124 125
   WLP-WIDE:MAKE ;

: WL-TAKEN ( -- n wide ) WL-WIDE 7 swap ;   \ the call's results taken back

\ One cell against the value it should hold; the next cell down holds one less.
: WL-NEXT= ( n n -- n )
   {: v:n want:n :}
   v want T=  want 1- ;

: WL-WIDE= ( wide -- )                    \ every cell of WL-WIDE, top first
   WLP-WIDE:UNMAKE 125
   WL-NEXT= WL-NEXT= WL-NEXT= WL-NEXT= WL-NEXT= WL-NEXT= WL-NEXT=
   WL-NEXT= WL-NEXT= WL-NEXT= WL-NEXT= WL-NEXT= WL-NEXT= WL-NEXT=
   WL-NEXT= WL-NEXT= WL-NEXT= WL-NEXT= WL-NEXT= WL-NEXT= WL-NEXT=
   WL-NEXT= WL-NEXT= WL-NEXT= WL-NEXT= WL-NEXT=
   99 T= ;

: WL-SHUFFLES ( -- )
   WL-WIDE 3 5 WLP-PAIR:MAKE WL-SWAP2 WL-WIDE= WLP-PAIR:UNMAKE 5 T= 3 T=
   WL-WIDE 3 5 WLP-PAIR:MAKE WL-SWAP2 WL-UNSWAP2
   WLP-PAIR:UNMAKE 5 T= 3 T= WL-WIDE=
   WL-WIDE 7 WL-SWAP1 WL-WIDE= 7 T=
   WL-WIDE 7 9 WL-ROT WL-WIDE= 9 T= 7 T=
   7 9 WL-WIDE WL-MROT 9 T= 7 T= WL-WIDE=
   WL-WIDE 7 WL-OVER WL-WIDE= 7 T= WL-WIDE=
   7 WL-WIDE WL-NIP WL-WIDE=
   WL-TAKEN WL-WIDE= 7 T= ;

\ Thirty-three reals are more than the floating registers: the same holds there.
: WL-FSUM ( r r r r r r r r r r r r r r r r r r r r r r r r r r r r r r r r r -- r )
   f+ f+ f+ f+ f+ f+ f+ f+ f+ f+ f+ f+ f+ f+ f+ f+
   f+ f+ f+ f+ f+ f+ f+ f+ f+ f+ f+ f+ f+ f+ f+ f+ ;

: WL-REALS ( -- )
   1 s>f 2 s>f 3 s>f 4 s>f 5 s>f 6 s>f 7 s>f 8 s>f 9 s>f 10 s>f 11 s>f
   12 s>f 13 s>f 14 s>f 15 s>f 16 s>f 17 s>f 18 s>f 19 s>f 20 s>f 21 s>f
   22 s>f 23 s>f 24 s>f 25 s>f 26 s>f 27 s>f 28 s>f 29 s>f 30 s>f 31 s>f
   32 s>f 33 s>f
   WL-FSUM f>s 561 T= ;

public
: MAIN ( -- )
   1 2 WLP-PAIR:MAKE WL-CELL !
   3 4 WLP-PAIR:MAKE WL-CELL WL-GROUP 10 T=
   WL-CELL 5 6 WLP-PAIR:MAKE WL-GROUP-BELOW 14 T=
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
   10 WLP-RES:ERR WL-MATCH 9 T=
   21 WLP-OPT:SOME WL-OPT-ID WL-OPT-MATCH 42 T=
   WL-SHUFFLES
   WL-REALS ;

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
\ the annotation grammar IS the signature grammar: a parameter list, a nested
\ family and the definition's own type variables all read here.
s" WLC-PARAM-ANN ( opt<n> -- n ) {: o:opt<n> :} 0 " YES
s" WLC-PARAM-ID ( opt<n> -- opt<n> ) {: o:opt<n> :} o " YES
s" WLC-NESTED-ANN ( opt<opt<n>> -- opt<opt<n>> ) {: o:opt<opt<n>> :} o " YES
s" WLC-VAR-ANN ( opt<a> -- opt<a> ) {: o:opt<a> :} o " YES
\ ...and so are its refusals: a bare arity>0 tail, a wrong argument count and a
\ wrong argument all reject with the signature diagnostics.
s" WLC-ARITY-ANN ( opt<n> -- n ) {: o:opt :} 0 " NO
s" WLC-ARITY2-ANN ( opt<n> -- n ) {: o:opt<n,n> :} 0 " NO
s" WLC-WRONG-ARG ( opt<n> -- n ) {: o:opt<pair> :} 0 " NO
\ one COMPLETE type and nothing after it: an empty annotation and a trailing
\ token are both rejected at the annotation, not at `:}`.
s" WLC-EMPTY-ANN ( n -- n ) {: x: :} x " NO
s" WLC-TRAILING-ANN ( opt<n> -- n ) {: o:opt<n>> :} 0 " NO
\ `:ptr` stays the one-token shorthand for an INFERRED pointee — a signature
\ reads `ptr <pointee>`, an annotation has no second token to read. The pointee
\ is inferred from the captured value, and a declared quantifier survives it.
s" WLC-PTR-ANN ( ptr a -- ptr a ) {: p:ptr :} p " YES
s" WLC-PTR-N-ANN ( ptr n -- ptr n ) {: p:ptr :} p " YES
\ `{: p:ptr n :}` is therefore NOT a pointee spelling: `n` is a second local.
s" WLC-PTR-TWO ( ptr n n -- ptr n ) {: p:ptr n :} p " YES
s" WLC-PTR-ONE ( ptr n -- ptr n ) {: p:ptr n :} p " NO
\ a `:ptr` local whose pointee is a layout binds in ONE group with a bundle,
\ above it or below it, exactly as it does in a group without one; the bundle
\ annotation is still asserted in that group.
s" WLC-GROUP-PTR ( pair ptr pair -- n ) {: p:pair q:ptr :} 0 " YES
s" WLC-GROUP-PTR-BELOW ( ptr pair pair -- n ) {: q:ptr p:pair :} 0 " YES
s" WLC-GROUP-PTR-ALONE ( ptr pair n -- n ) {: q:ptr k:n :} k " YES
s" WLC-GROUP-WRONG-FAM ( pair ptr pair -- n ) {: p:trip q:ptr :} 0 " NO

;package

WLP:MAIN
T-REPORT
