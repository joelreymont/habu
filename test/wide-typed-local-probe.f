\ wide-typed-local-probe.f - a wide arity-0 layout value bound to a typed local
\ survives whole at runtime.
\ The annotation records the layout's TOP (tag) hidden term, the bind records
\ the bundle's full width (checker.f LOCW-HW@), and every reference reloads all
\ physical cells.

require src/lib/test.f
require src/lib/adt/option.f
require src/lib/adt/result.f

package WLP
public

STRUCTURE pair
   FIELD left n
   FIELD right n
;STRUCTURE

\ the same physical shape as `pair` under another name: identity is NOMINAL
STRUCTURE pair2
   FIELD left n
   FIELD right n
;STRUCTURE

STRUCTURE trip
   FIELD a n
   FIELD b n
   FIELD c n
;STRUCTURE

\ twenty-six cells: with a pair, more than the registers a routine may destroy
STRUCTURE wide
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
SUMTYPE res
   VARIANT ok  n ;VARIANT
   VARIANT err n ;VARIANT
;SUMTYPE

\ parametric, and nameable in a local: an annotation is read by the SIGNATURE
\ type grammar
SUMTYPE opt<a>
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

\ ---- a constructor's or MATCH's open-width result binds whole ---------------
: WL-INC ( option<n> -- option<n> )
   MATCH option
      none OF OPTION:NONE ENDOF
      some OF 1+ OPTION:SOME ENDOF
   ;MATCH {: r:option<n> :}
   r ;

: WL-INC-BESIDE ( option<n> -- option<n> )   \ beside a scalar in one group
   MATCH option
      none OF OPTION:NONE ENDOF
      some OF 1+ OPTION:SOME ENDOF
   ;MATCH 0 {: r:option<n> z:n :}
   r ;

: WL-SHOW ( option<n> -- n )
   MATCH option
      none OF -1 ENDOF
      some OF ENDOF
   ;MATCH ;

\ ---- a generic call's result binds whole to an untyped local ---------------
\ The call learns the width of its open output (`a` := n gives two cells), so
\ an untyped local takes both: each word returns how far the stack moved.
: WL-WRAP ( a -- option<a> ) OPTION:SOME ;
: WL-KEEPO ( option<n> -- option<n> ) ;
: WL-BIND-CTOR ( -- n ) depth {: d:n :} 3 OPTION:SOME {: c :} depth d - ;
: WL-BIND-WRAP ( -- n ) depth {: d:n :} 3 WL-WRAP {: c :} depth d - ;
: WL-BIND-CAUGHT ( -- n )
   depth {: d:n :} 3 OPTION:SOME ['] WL-KEEPO catch {: c code:n :} depth d - ;
\ `OPTION:NONE` leaves `a` open; the catch's unify with WL-KEEPO resolves it in
\ the window, which then holds the option's two cells.
: WL-BIND-OPEN ( -- n )
   depth {: d:n :} OPTION:NONE ['] WL-KEEPO catch drop {: c :} depth d - ;

\ ---- an open option meets a built one at a join or a loop's back edge --------
\ `OPTION:NONE` leaves `a` open, `5 OPTION:SOME` is two cells: the edge binds
\ the width and the row holds the two cells whichever side came first.
variable WL-RT
: WL-RT? ( -- bool ) WL-RT @ 0 > ;
: WL-EAT ( option<n> -- ) WL-SHOW drop  WL-RT @ 1 - WL-RT ! ;
: WL-LOOP-NONE ( n -- option<n> )
   WL-RT !  OPTION:NONE BEGIN WL-RT? WHILE WL-EAT 5 OPTION:SOME REPEAT ;
: WL-LOOP-SOME ( n -- option<n> )
   WL-RT !  5 OPTION:SOME BEGIN WL-RT? WHILE WL-EAT OPTION:NONE REPEAT ;
\ The loop's exit restores the row saved before the body: `c` takes both cells.
: WL-LOOP-BIND ( n -- n )
   WL-RT !  depth {: d:n :}
   OPTION:NONE BEGIN WL-RT? WHILE WL-EAT 5 OPTION:SOME REPEAT {: c :} depth d - ;
\ The ELSE arm restores the row the IF arm's unify resolved: `c` takes both cells.
: WL-ELSE-BIND ( bool -- n )
   {: f:bool :} depth {: d:n :}
   OPTION:NONE f IF WL-EAT 0 ELSE {: c :} depth d - THEN ;
\ Both options share `a`: eating the top one resolves the one below for `c`.
: WL-TWO-NONE ( -- option<a> option<a> ) OPTION:NONE OPTION:NONE ;
: WL-SHARED-BIND ( -- n ) depth {: d:n :} WL-TWO-NONE WL-EAT {: c :} depth d - ;
\ `c`'s annotation resolves `a` for the untyped sibling, above or below it.
: WL-SIBLING-BELOW ( -- n n )
   depth {: d:n :} WL-TWO-NONE {: x c:option<n> :} depth d - c WL-SHOW ;
: WL-SIBLING-ABOVE ( -- n n )
   depth {: d:n :} WL-TWO-NONE {: c:option<n> x :} depth d - c WL-SHOW ;
\ A resolved option beside a built one: `swap` moves each whole.
: WL-LOOP-SWAP ( n -- n n )
   WL-RT !  OPTION:NONE BEGIN WL-RT? WHILE WL-EAT 5 OPTION:SOME REPEAT
   9 OPTION:SOME swap WL-SHOW swap WL-SHOW ;
: WL-SHARED-SWAP ( -- n n ) WL-TWO-NONE WL-EAT 9 OPTION:SOME swap WL-SHOW swap WL-SHOW ;
: WL-JOIN-NONE ( bool -- n )
   {: f:bool :}
   depth {: d:n :} f IF OPTION:NONE ELSE 5 OPTION:SOME THEN {: c :} depth d - ;
: WL-JOIN-SOME ( bool -- n )
   {: f:bool :}
   depth {: d:n :} f IF 5 OPTION:SOME ELSE OPTION:NONE THEN {: c :} depth d - ;
: WL-JOIN-MATCH ( option<n> -- n )
   {: o :}
   depth {: d:n :}
   o MATCH option
      none OF OPTION:NONE ENDOF
      some OF 1+ OPTION:SOME ENDOF
   ;MATCH {: c :}
   depth d - ;

\ ---- a bundle and a `:ptr` local in ONE group -------------------------------
\ The group path binds the pointer's inferred pointee under the same transport
\ rule as a group without a bundle: the pointee here is a layout, which a bare
\ `ptr` local only absorbs in transport.
: WL-GROUP ( pair ptr pair -- n )         \ pointer above the bundle
   {: p:pair q:ptr :}
   p WLP-PAIR:UNMAKE +  q @ WLP-PAIR:UNMAKE +  + ;

: WL-GROUP-BELOW ( ptr pair pair -- n )   \ pointer below the bundle
   {: q:ptr p:pair :}
   p WLP-PAIR:UNMAKE +  q @ WLP-PAIR:UNMAKE +  + ;

\ ---- a record wider than the register pool, shuffled whole -----------------
\ Each shuffle moves more cells than a routine has registers.
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
   7 77 41 OPTION:SOME WL-INC WL-SHOW 42 T=
   OPTION:NONE WL-INC WL-SHOW -1 T=
   5 OPTION:SOME WL-INC-BESIDE WL-SHOW 6 T=
   77 T= 7 T=                             \ the cells below the locals survive
   WL-BIND-CTOR 0 T=  WL-BIND-WRAP 0 T=  WL-BIND-CAUGHT 0 T=  WL-BIND-OPEN 0 T=
   0 WL-LOOP-NONE WL-SHOW -1 T=  1 WL-LOOP-NONE WL-SHOW 5 T=  2 WL-LOOP-NONE WL-SHOW 5 T=
   0 WL-LOOP-SOME WL-SHOW 5 T=  1 WL-LOOP-SOME WL-SHOW -1 T=  2 WL-LOOP-SOME WL-SHOW -1 T=
   true WL-JOIN-NONE 0 T=  false WL-JOIN-NONE 0 T=
   true WL-JOIN-SOME 0 T=  false WL-JOIN-SOME 0 T=
   OPTION:NONE WL-JOIN-MATCH 0 T=  5 OPTION:SOME WL-JOIN-MATCH 0 T=
   0 WL-LOOP-BIND 0 T=  2 WL-LOOP-BIND 0 T=
   true WL-ELSE-BIND 0 T=  false WL-ELSE-BIND 0 T=
   WL-SHARED-BIND 0 T=  WL-SHARED-SWAP 9 T= -1 T=
   WL-SIBLING-BELOW -1 T= 0 T=  WL-SIBLING-ABOVE -1 T= 0 T=
   0 WL-LOOP-SWAP 9 T= -1 T=  2 WL-LOOP-SWAP 9 T= 5 T=
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
\ a result whose width read an open argument is one logical cell; its family's
\ annotation takes it whole and gives an open width (OPTION:NONE's) its own.
s" WLC-MATCH-SOME ( option<n> -- option<n> ) MATCH option some OF 1+ OPTION:SOME ENDOF none OF OPTION:NONE ENDOF ;MATCH {: r:option<n> :} r " YES
s" WLC-CTOR-NONE ( -- option<n> ) OPTION:NONE {: r:option<n> :} r " YES
s" WLC-CTOR-OK ( n -- result<n,n> ) RESULT:OK {: r:result<n,n> :} r " YES
s" WLC-CTOR-OTHER-FAM ( n -- n ) OPTION:SOME {: r:result<n,n> :} 0 " NO
s" WLC-CTOR-WRONG-ARG ( bool -- n ) OPTION:SOME {: r:option<n> :} 0 " NO
s" WLC-CTOR-UNDERFLOW ( n -- option<n> ) OPTION:SOME {: r:option<n> z:n :} r " NO
\ the cell bound whole keeps the stale lineage `catch` gave it, as a closed
\ bundle does: WL-OUTER's caught result is refused.
variable WL-TICK
: WL-NEW ( -- n ) WL-TICK @ 0= IF 1 throw THEN 31 ;
: WL-NUM ( n -- n ) drop WL-NEW ;   \ a throw path that overwrote its input
: WL-OUTER ( n -- n )
   drop
   5 ['] WL-NUM catch {: code:n :}
   code 0= IF OPTION:SOME ELSE drop OPTION:NONE THEN
   {: r:option<n> :}
   r MATCH option none OF 0 ENDOF some OF ENDOF ;MATCH
   1 throw ;
s" WLC-CTOR-STALE ( n -- n ) ['] WL-OUTER catch drop 1+ " NO
\ a caught cell bound on the success arm re-pushes its own width.
: WL-ANY ( option<a> -- option<a> ) ;
: WL-KEEP ( option<a> -- option<a> ) WL-TICK @ 0= IF WL-ANY 1 throw THEN ;
s" WLC-CTOR-CAUGHT ( n -- n ) OPTION:SOME ['] WL-KEEP catch {: code:n :} code 0= IF {: r:option<n> :} r WL-SHOW EXIT THEN 1 throw " YES
s" WLC-CTOR-CAUGHT-WIDE ( n -- option<n> n ) OPTION:SOME ['] WL-KEEP catch {: code:n :} code 0= IF {: r:option<n> :} r WL-SHOW EXIT THEN 1 throw " NO

;package

WLP:MAIN
T-REPORT
