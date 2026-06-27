\ ptx.f - checked PTX kernel-header vocabulary.

32 constant PTX-WARP
1024 constant PTX-MAX-BLOCK

variable PTX-BLOCK-N

: PTX-BLOCK-LEGAL? ( n -- bool ) {: n :}
   n 0 > n PTX-MAX-BLOCK <= and n PTX-WARP mod 0= and ;

: PTX-BLOCK-CHECK ( n -- )
   PTX-BLOCK-LEGAL? 0= if E-PTX-BLOCK throw then ;

: %BLOCK ( n -- )
   dup PTX-BLOCK-CHECK PTX-BLOCK-N ! ;

: PTX-BLOCK@ ( -- n )
   PTX-BLOCK-N @ ;

: PTX-PARSE-REQ ( -- ptr u8 n )
   parse-name dup 0= if E-PTX-SYNTAX throw then ;

: PTX-SKIP-ONE ( -- )
   PTX-PARSE-REQ 2drop ;

: GRID: ( -- )
   PTX-SKIP-ONE ; immediate

: WHERE ( -- )
   PTX-SKIP-ONE
   PTX-SKIP-ONE
   PTX-SKIP-ONE ; immediate
