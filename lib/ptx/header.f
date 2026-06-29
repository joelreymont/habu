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

: PTX-EXTENT? ( ptr u8 n -- bool )
   s" extent-" STARTS-WITH? ;

: PTX-BLOCK-NAME>N ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u s" block-" STARTS-WITH? 0= if E-PTX-SYNTAX throw then
   a 6 + u 6 - STR>NUMBER? 0= if E-PTX-SYNTAX throw then
   dup PTX-BLOCK-CHECK ;

: PTX-WHERE-CHECK ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: lhs:ptr lhsu:n op:ptr opu:n rhs:ptr rhsu:n :}
   lhs lhsu PTX-EXTENT? 0= if E-PTX-SYNTAX throw then
   op opu s" <=" STR= 0= if E-PTX-SYNTAX throw then
   rhs rhsu PTX-BLOCK-NAME>N PTX-BLOCK@ <> if E-PTX-BLOCK throw then ;

: PTX-SKIP-ONE ( -- )
   PTX-PARSE-REQ 2drop ;

: GRID: ( -- )
   PTX-SKIP-ONE ; immediate

: WHERE ( -- )
   PTX-PARSE-REQ {: lhs:ptr lhsu:n :}
   PTX-PARSE-REQ {: op:ptr opu:n :}
   PTX-PARSE-REQ {: rhs:ptr rhsu:n :}
   lhs lhsu op opu rhs rhsu PTX-WHERE-CHECK ; immediate
