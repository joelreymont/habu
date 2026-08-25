\ native-rename-rows.f - whole-value stack renames through production code.

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
: INDEPENDENT-VARS ( a b n -- n a b )   swap ;
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

private

: ENUM-BUNDLE ( lamp n -- n lamp )
   swap ;

\ Retirement owner: habu-type-isolated-dynamic-244c0e2c.
TRUSTED: TRY ( ptr u8 n -- n )
   [: evaluate ;] catch ;

variable TORB-RC
variable MIXHI-RC
variable MIXLO-RC

: CAPTURE-REFUSALS ( -- )
   s" : C-TORB ( option<n> -- option<n> ) >r r> ;" TRY TORB-RC !
   s" : C-MIXHI ( mixhi -- n ) MATCH mixhi nohi OF 0 ENDOF hi OF drop NRR-PT:UNMAKE + ENDOF ;MATCH ;" TRY MIXHI-RC !
   s" : C-MIXLO ( mixlo -- n ) MATCH mixlo nolo OF 0 ENDOF lo OF NRR-PT:UNMAKE + + ENDOF ;MATCH ;" TRY MIXLO-RC ! ;

CAPTURE-REFUSALS

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
   NRR-LAMP:BRIGHT 9 ENUM-BUNDLE NRR-LAMP:BRIGHT T= 9 T= ;

: REFUSALS ( -- )
   s" parking one cell of a bundle remains a named refusal" T-LABEL
   TORB-RC @ E-NELAB-BUNDLE T=

   s" unplaceable mixed-width MATCH payloads remain named refusals" T-LABEL
   MIXHI-RC @ E-NELAB-MATCH T=
   MIXLO-RC @ E-NELAB-MATCH T= ;

public

: CASES ( -- )
   PRIMARY-RENAMES
   COPY-DROP-RENAMES
   SEAM-RENAMES
   DISPATCH-SEAMS
   UNCHANGED-SHAPES
   REFUSALS ;

;package

T-RESET
NRR:CASES
T-REPORT
