\ native-rename-rows.f - whole-value stack renames through production code.

\ Tier 1 first: the whole-value moves and the width refusals that name them
\ come from native elaboration.
1 set-tier

require test/compiler/native-eval-fixture.f
require lib/test.f
require lib/adt/option.f
require src/compiler/native/compiler.f

package NRR

private

: OPTION>N ( option<n> -- n )
   MATCH option
      none OF -1 ENDOF
      some OF ENDOF
   ;MATCH ;

: OPTION-N> ( option<n> n -- n n )
   >r OPTION>N r> ;

: MK ( n -- option<n> )
   OPTION:SOME ;

: SIDE ( n -- n )
   dup 0 > if 1 + else 2 + then ;

: SWAP-BUNDLE ( option<n> n -- n option<n> )
   swap ;

: ROT-BUNDLE ( option<n> n n -- n n option<n> )
   rot ;

: CTOR-BUNDLE ( n n -- option<n> n )
   swap OPTION:SOME swap ;

: USER-BUNDLE ( n n -- option<n> n )
   swap MK swap ;

: OVER-BUNDLE ( option<n> n -- option<n> n option<n> )
   over ;

: DUP-BUNDLE ( option<n> -- option<n> option<n> )
   dup ;

: NIP-BUNDLE ( option<n> n -- n )
   nip ;

: DROP-BUNDLE ( option<n> n -- option<n> )
   drop ;

: ADJACENT-BUNDLES ( option<n> option<n> -- option<n> option<n> )
   swap ;

: JOIN-BUNDLE ( option<n> n -- n option<n> )
   dup 0 > if 1 + else 2 + then swap ;

: LOOP-RENAME-BUNDLE ( option<n> n -- n option<n> )
   3 0 ?do over drop loop swap ;

: CALL-BUNDLE ( option<n> n -- n option<n> )
   SIDE swap ;

: JOIN-RETURN-BUNDLE ( option<n> n -- option<n> )
   0 > if 1 else 2 then drop ;

: TWOC ( -- n n )
   3 5 ;

: STALE-BUNDLE ( option<n> -- n )
   MATCH option
      none OF 0 ENDOF
      some OF TWOC nip + ENDOF
   ;MATCH ;

: PARK-BUNDLE ( option<n> n n -- n option<n> n )
   >r dup 0 > if 1 + else 2 + then swap r> ;

: MATCH-UPPER-BUNDLE ( option<n> option<n> -- n )
   MATCH option
      none OF drop 0 ENDOF
      some OF swap drop ENDOF
   ;MATCH ;

: HOLD-BUNDLE ( n n option<n> -- n n option<n> ) ;
: PASS-BUNDLE ( option<n> -- option<n> ) ;
: MADE-BUNDLE ( n -- option<n> )   OPTION:SOME ;
: LOOP-BUNDLE ( option<n> n n -- option<n> )   ?do loop ;
: INDEPENDENT-VARS ( a b n -- a n b )   swap ;
: PLAIN-CELLS ( n n -- n n )   swap ;

public

ENUM lamp DERIVE eq
   dim
   bright
;ENUM

PRODUCT pt 0
   FIELD x n
   FIELD y n
;PRODUCT

SUMTYPE mixhi 0
   VARIANT nohi ;VARIANT
   VARIANT hi pt n ;VARIANT
;SUMTYPE

SUMTYPE mixlo 0
   VARIANT nolo ;VARIANT
   VARIANT lo n pt ;VARIANT
;SUMTYPE

\ Two parametric families that differ in ONE way: whether the width reads the
\ argument. `sp`'s parameter occurs only as a pointee, so every instantiation is
\ two cells and an OPEN instance is placeable; `box` keeps its parameter as a
\ payload cell, so no width exists before the argument binds and an open
\ instance of it stays refused (dot habu-place-an-open-d7bcba49).
PRODUCT sp 1
   FIELD base ptr a
   FIELD len n
;PRODUCT

PRODUCT box 1
   FIELD it a
   FIELD mark n
;PRODUCT

private

create SP-BYTES 100 allot

: SP-WHOLE ( -- sp<u8> )   SP-BYTES byte-view 100 NRR-SP:MAKE ;
: SP-LEN ( sp<t> -- n )   NRR-SP:UNMAKE nip ;
: SP-TAKE ( sp<t> n -- sp<t> )   {: k :} NRR-SP:UNMAKE drop k NRR-SP:MAKE ;
: SP-SKIP ( sp<t> n -- sp<t> )   {: k :} NRR-SP:UNMAKE k - swap k + swap NRR-SP:MAKE ;

\ The shape that drifted while an open row could not be placed: a call returns an
\ open-argument bundle and a local binds the whole of it.
: SP-TAKE-LEN ( n -- n )   {: k :} SP-WHOLE k SP-TAKE {: w :} w SP-LEN ;
: SP-SKIP-LEN ( n -- n )   {: k :} SP-WHOLE k SP-SKIP {: w :} w SP-LEN ;

\ A `does>` clause's rows are rows: the created word yields a two-cell value and
\ the clause is placed from that row's own per-cell boundaries, so the definer
\ compiles and the word it creates runs (dot habu-give-a-does-97cd0db2). The
\ body is lib/span.f's SPAN-BUFFER: shape - a reach cell and the bytes after it.
: SP-BUFFER: ( n -- )
   create dup , allot
   does> ( -- sp<u8> ) dup @ >r cell+ byte-view r> NRR-SP:MAKE ;

64 SP-BUFFER: SPB

: SPB-LEN ( -- n )
   SPB SP-LEN ;

: SPB-C! ( u8 n -- ) {: v:n i:n :}
   SPB NRR-SP:UNMAKE drop i + {: p:ptr :} v p c! ;

: SPB-C@ ( n -- u8 ) {: i:n :}
   SPB NRR-SP:UNMAKE drop i + {: p:ptr :} p c@ ;

\ The created word's value crossing a call boundary whole.
: SP-LEN-AFTER ( sp<t> n -- n )
   SP-TAKE SP-LEN ;

: ENUM-BUNDLE ( lamp n -- n lamp )
   swap ;

\ Retirement owner: habu-type-isolated-dynamic-244c0e2c.
: TRY ( ptr u8 n -- n )
   NATIVE-EVAL:DEFINE-RC ;

variable TORB-RC
variable MIXHI-RC
variable MIXLO-RC
variable SP-DROP-RC
variable SP-LOCAL-RC
variable SP-PARK-RC
variable SP-REMAKE-RC
variable BOX-OPEN-RC
variable BOX-CLOSED-RC
variable BOX-DOES-RC

: CAPTURE-DYNAMIC-CASES ( -- )
   s" : C-TORB ( option<n> -- option<n> ) >r r> ;" TRY TORB-RC !
   s" : C-MIXHI ( mixhi -- n ) MATCH mixhi nohi OF 0 ENDOF hi OF drop NRR-PT:UNMAKE + ENDOF ;MATCH ;" TRY MIXHI-RC !
   s" : C-MIXLO ( mixlo -- n ) MATCH mixlo nolo OF 0 ENDOF lo OF NRR-PT:UNMAKE + + ENDOF ;MATCH ;" TRY MIXLO-RC !
   \ A row with an OPEN width-free instance BELOW another value: placeable
   \ whatever the body does, because the row has one term per cell.
   s" : C-SP-DROP ( sp<t> n -- sp<t> ) drop ;" TRY SP-DROP-RC !
   s" : C-SP-LOCAL ( sp<t> n -- sp<t> ) {: k :} ;" TRY SP-LOCAL-RC !
   s" : C-SP-PARK ( sp<t> n -- sp<t> ) >r r> drop ;" TRY SP-PARK-RC !
   s" : C-SP-REMAKE ( sp<t> n -- sp<t> ) {: k :} NRR-SP:UNMAKE k - swap k + swap NRR-SP:MAKE ;" TRY SP-REMAKE-RC !
   \ The same row over a family whose width READS the open argument: refused by
   \ name, never guessed; the closed instantiation of it is placed exactly.
   s" : C-BOX-OPEN ( box<t> n -- box<t> ) drop ;" TRY BOX-OPEN-RC !
   s" : C-BOX-CLOSED ( box<n> n -- box<n> ) drop ;" TRY BOX-CLOSED-RC !
   \ The same question at a does> clause: the clause's row carries an open
   \ instance whose width reads the argument, so the row has fewer terms than
   \ cells and no per-cell boundary exists to place it from. Refused at the
   \ definer by name, never guessed.
   s" : C-BOX-DEF: ( -- ) create 0 , does> ( box<t> -- box<t> ) drop ;"
      TRY BOX-DOES-RC ! ;

CAPTURE-DYNAMIC-CASES

: PRIMARY-RENAMES ( -- )
   s" swap and rot move a two-cell value whole" T-LABEL
   43 OPTION:SOME 7 SWAP-BUNDLE OPTION>N 43 T= 7 T=
   43 OPTION:SOME 7 9 ROT-BUNDLE OPTION>N 43 T= 9 T= 7 T=

   s" generated and user-word outputs remain whole when moved" T-LABEL
   5 11 CTOR-BUNDLE OPTION-N> 11 T= 5 T=
   5 11 USER-BUNDLE OPTION-N> 11 T= 5 T= ;

: COPY-DROP-RENAMES ( -- )
   s" over and dup copy whole values" T-LABEL
   43 OPTION:SOME 7 OVER-BUNDLE
   OPTION>N 43 T= 7 T= OPTION>N 43 T=
   43 OPTION:SOME DUP-BUNDLE
   OPTION>N 43 T= OPTION>N 43 T=

   s" nip and drop discard whole values" T-LABEL
   43 OPTION:SOME 7 NIP-BUNDLE 7 T=
   43 OPTION:SOME 7 DROP-BUNDLE OPTION>N 43 T= ;

: SEAM-RENAMES ( -- )
   s" adjacent bundles retain their boundary when exchanged" T-LABEL
   11 OPTION:SOME 29 OPTION:SOME ADJACENT-BUNDLES
   OPTION>N 11 T= OPTION>N 29 T=

   s" a bundle retains its boundary across both join arms" T-LABEL
   43 OPTION:SOME 7 JOIN-BUNDLE OPTION>N 43 T= 8 T=
   43 OPTION:SOME -7 JOIN-BUNDLE OPTION>N 43 T= -5 T=

   s" a bundle retains its boundary across a loop edge and call" T-LABEL
   43 OPTION:SOME 7 LOOP-RENAME-BUNDLE OPTION>N 43 T= 7 T=
   43 OPTION:SOME 7 CALL-BUNDLE OPTION>N 43 T= 8 T=

   s" parked data and a returned join retain the same boundary" T-LABEL
   43 OPTION:SOME 7 9 PARK-BUNDLE 9 T= OPTION>N 43 T= 8 T=
   43 OPTION:SOME 7 JOIN-RETURN-BUNDLE OPTION>N 43 T=
   43 OPTION:SOME -7 JOIN-RETURN-BUNDLE OPTION>N 43 T= ;

: DISPATCH-SEAMS ( -- )
   s" a call after MATCH ignores stale boundary bits" T-LABEL
   43 OPTION:SOME STALE-BUNDLE 48 T=
   OPTION:NONE STALE-BUNDLE 0 T=

   s" MATCH selects the upper of two adjacent bundles" T-LABEL
   11 OPTION:SOME 29 OPTION:SOME MATCH-UPPER-BUNDLE 29 T=
   11 OPTION:SOME OPTION:NONE MATCH-UPPER-BUNDLE 0 T= ;

: UNCHANGED-SHAPES ( -- )
   s" held, passed, made, and loop-carried bundles remain whole" T-LABEL
   5 6 43 OPTION:SOME HOLD-BUNDLE OPTION>N 43 T= 6 T= 5 T=
   43 OPTION:SOME PASS-BUNDLE OPTION>N 43 T=
   7 MADE-BUNDLE OPTION>N 7 T=
   43 OPTION:SOME 3 0 LOOP-BUNDLE OPTION>N 43 T=

   s" independent variables and plain cells remain ordinary renames" T-LABEL
   11 22 33 INDEPENDENT-VARS 22 T= 33 T= 11 T=
   1 2 PLAIN-CELLS 1 T= 2 T=

   s" a one-cell enum is not segmented as a bundle" T-LABEL
   NRR-LAMP:BRIGHT 9 ENUM-BUNDLE NRR-LAMP:BRIGHT NRR-LAMP:EQ TTRUE 9 T= ;

: MIXED-PAYLOADS ( -- )
   s" instantiated mixed-width payload boundaries support both field orders" T-LABEL
   MIXHI-RC @ 0 T= MIXLO-RC @ 0 T=
   NRR-MIXHI:NOHI C-MIXHI 0 T=
   3 5 NRR-PT:MAKE 11 NRR-MIXHI:HI C-MIXHI 8 T=
   NRR-MIXLO:NOLO C-MIXLO 0 T=
   11 3 5 NRR-PT:MAKE NRR-MIXLO:LO C-MIXLO 19 T= ;

: OPEN-ROWS ( -- )
   s" a row carrying an open width-free instance is placed" T-LABEL
   SP-DROP-RC @ 0 T=  SP-LOCAL-RC @ 0 T=
   SP-PARK-RC @ 0 T=  SP-REMAKE-RC @ 0 T=

   s" a call's open-argument result binds whole to a local" T-LABEL
   SP-WHOLE SP-LEN 100 T=
   7 SP-TAKE-LEN 7 T=
   40 SP-SKIP-LEN 60 T=

   s" a does> clause yields a two-cell value and the created word runs" T-LABEL
   SPB-LEN 64 T=
   201 7 SPB-C!  7 SPB-C@ 201 T=
   SPB 9 SP-LEN-AFTER 9 T= ;

: REFUSALS ( -- )
   s" parking one cell of a bundle remains a named refusal" T-LABEL
   TORB-RC @ E-NELAB-BUNDLE T=

   s" an open argument the width READS is refused, its closed twin placed" T-LABEL
   BOX-OPEN-RC @ E-NELAB-BUNDLE T=
   BOX-CLOSED-RC @ 0 T=

   s" a does> clause over that open argument is refused at the definer" T-LABEL
   BOX-DOES-RC @ E-NELAB-BUNDLE T= ;

public

: CASES ( -- )
   PRIMARY-RENAMES
   COPY-DROP-RENAMES
   SEAM-RENAMES
   DISPATCH-SEAMS
   UNCHANGED-SHAPES
   MIXED-PAYLOADS
   OPEN-ROWS
   REFUSALS ;

;package

T-RESET
NRR:CASES
T-REPORT
