\ bootstrap-wide-memory-src.f - stage0 wide ADT memory execution/goldens.

\ Gforth stage0 intentionally has no protected-WID registry. Keep the type
\ declaration hook explicit and inert while this fixture exercises codegen.
: BWM-STAGE0-PROT-NOP ( ptr u8 n -- ) 2drop ;
' BWM-STAGE0-PROT-NOP TDECL-PROT-WID-XT !

SUMTYPE bwm2 1
  VARIANT pair a ;VARIANT
  VARIANT other a ;VARIANT
;SUMTYPE

SUMTYPE bwm4 3
  VARIANT quad a b c ;VARIANT
;SUMTYPE

variable BWM-FAILS
variable BWM-CASES

: BWM-FAIL ( -- )
   BWM-FAILS @ 1 + BWM-FAILS ! ;

: BWM= ( n n -- ) {: got:n want:n :}
   BWM-CASES @ 1 + BWM-CASES !
   got want <> if
      BWM-FAIL
      s" case " type BWM-CASES @ .
      s" expected " type want . s" got " type got . cr
   then ;

LAYOUT-BUFFER BWM-MEM2 bwm2<n> 1
LAYOUT-BUFFER BWM-MEM4 bwm4<n,n,n> 1

: BWM-STORE2 ( bwm2<n> -- ) 0 BWM-MEM2 ! ;
: BWM-FETCH2 ( -- bwm2<n> ) 0 BWM-MEM2 @ ;
: BWM-STORE4 ( bwm4<n,n,n> -- ) 0 BWM-MEM4 ! ;
: BWM-FETCH4 ( -- bwm4<n,n,n> ) 0 BWM-MEM4 @ ;
\ Scalar locals preceding a wide memory token pin pass-2 token-index parity.
: BWM-FETCH2-LOCAL ( n -- bwm2<n> ) {: seed:n :} 0 BWM-MEM2 @ ;

: BWM-STORE2-G ( bwm2<n> ptr bwm2<n> -- ) ! ;
: BWM-FETCH2-G ( ptr bwm2<n> -- bwm2<n> ) @ ;
: BWM-STORE4-G ( bwm4<n,n,n> ptr bwm4<n,n,n> -- ) ! ;
: BWM-FETCH4-G ( ptr bwm4<n,n,n> -- bwm4<n,n,n> ) @ ;

: BWM-MK2 ( -- bwm2<n> ) 7 BWM2:PAIR ;
: BWM-MK2B ( -- bwm2<n> ) 8 BWM2:OTHER ;
: BWM-MK4 ( -- bwm4<n,n,n> ) 91 92 93 BWM4:QUAD ;

TRUSTED: BWM-UN2 ( bwm2<n> -- n n ) ;
TRUSTED: BWM-UN4 ( bwm4<n,n,n> -- n n n n ) ;
TRUSTED: BWM-XT ( ptr u8 n -- n ) 0 search-wl ;

: BWM-RUN2 ( -- n n )
   BWM-MK2 BWM-STORE2
   0 BWM-FETCH2-LOCAL BWM-UN2 ;

: BWM-RUN4 ( -- n n n n )
   BWM-MK4 BWM-STORE4
   BWM-FETCH4 BWM-UN4 ;

\ typed-local-lint: allow-bare-local - family locals are not yet annotatable.
: BWM-LOCAL2 ( -- n n n ) BWM-MK2 5 {: r s:n :} s r BWM-UN2 ;
\ typed-local-lint: allow-bare-local - family locals are not yet annotatable.
: BWM-LOCAL4 ( -- n n n n n ) 5 BWM-MK4 {: s:n m :} s m BWM-UN4 ;
\ typed-local-lint: allow-bare-local - family locals are not yet annotatable.
: BWM-BR2 ( n -- n n ) 0 > if BWM-MK2 {: r :} r BWM-UN2
\ typed-local-lint: allow-bare-local - family locals are not yet annotatable.
   else BWM-MK2B {: r :} r BWM-UN2 then ;
\ typed-local-lint: allow-bare-local - family locals are not yet annotatable.
: BWM-BRW ( n -- n n n n ) 0 > if BWM-MK2 {: r :} r BWM-UN2 0 0
\ typed-local-lint: allow-bare-local - family locals are not yet annotatable.
   else BWM-MK4 {: m :} m BWM-UN4 then ;
\ typed-local-lint: allow-bare-local - family locals are not yet annotatable.
: BWM-BRMIX ( n -- n n n ) 0 > if BWM-MK2 5 {: r s:n :} s r BWM-UN2 else 6 7 8 then ;
\ typed-local-lint: allow-bare-local - family locals are not yet annotatable.
: BWM-BROUTER ( n -- n n n n ) BWM-MK4 {: m :}
\ typed-local-lint: allow-bare-local - family locals are not yet annotatable.
   0 > if BWM-MK2 {: r :} r BWM-UN2 drop drop then m BWM-UN4 ;

TRUSTED: BWM-W32 ( n n -- n )
   + dup c@ over 1 + c@ 8 lshift or
   over 2 + c@ 16 lshift or swap 3 + c@ 24 lshift or ;

variable BWM-GXT

: BWM-GOLD ( n n -- ) {: idx:n want:n :}
   BWM-GXT @ idx 4 * BWM-W32 want BWM= ;

: BWM-STORE-GOLD ( ptr u8 n n n n -- )
   {: name:ptr nameu:n width:n sub:n pop:n :}
   name nameu BWM-XT BWM-GXT !
   0 $D10043FF BWM-GOLD  1 $F90003FE BWM-GOLD
   2 $D1002273 BWM-GOLD  3 $F940026A BWM-GOLD
   4 sub BWM-GOLD        5 width BWM-GOLD
   6 $F940128D BWM-GOLD  7 $B400018D BWM-GOLD
   8 $CB14014C BWM-GOLD  9 $D100818D BWM-GOLD
   10 $F10241BF BWM-GOLD 11 $540000A3 BWM-GOLD
   12 $D287970D BWM-GOLD 13 $CB0D018D BWM-GOLD
   14 $F11041BF BWM-GOLD 15 $54000082 BWM-GOLD
   16 $D2800A60 BWM-GOLD
   HB-TARGET-LINUX? if
      17 $D2800BC8 BWM-GOLD  18 $D4000001 BWM-GOLD
   else
      17 $D2800030 BWM-GOLD  18 $D4001001 BWM-GOLD
   then
   19 $F94001CF BWM-GOLD 20 $F900014F BWM-GOLD
   21 $910021CE BWM-GOLD 22 $9100214A BWM-GOLD
   23 $F1000529 BWM-GOLD 24 $54FFFDC1 BWM-GOLD
   25 pop BWM-GOLD
   26 $F94003FE BWM-GOLD 27 $910043FF BWM-GOLD
   28 $D65F03C0 BWM-GOLD ;

: BWM-FETCH-GOLD ( ptr u8 n n -- )
   {: name:ptr nameu:n width:n :}
   name nameu BWM-XT BWM-GXT !
   0 $D10043FF BWM-GOLD  1 $F90003FE BWM-GOLD
   2 $D1002273 BWM-GOLD  3 $F940026A BWM-GOLD
   4 width BWM-GOLD      5 $F940014B BWM-GOLD
   6 $9100214A BWM-GOLD  7 $F900026B BWM-GOLD
   8 $91002273 BWM-GOLD  9 $F1000529 BWM-GOLD
   10 $54FFFF61 BWM-GOLD
   11 $F94003FE BWM-GOLD 12 $910043FF BWM-GOLD
   13 $D65F03C0 BWM-GOLD ;

: BWM-TEST-GOLDENS ( -- )
   s" BWM-STORE2-G" $D2800049 $D100426E $D1004273 BWM-STORE-GOLD
   s" BWM-FETCH2-G" $D2800049 BWM-FETCH-GOLD
   s" BWM-STORE4-G" $D2800089 $D100826E $D1008273 BWM-STORE-GOLD
   s" BWM-FETCH4-G" $D2800089 BWM-FETCH-GOLD ;

: BWM-TEST-RUNTIME ( -- )
   BWM-RUN2 0 BWM= 7 BWM=
   BWM-RUN4 0 BWM= 93 BWM= 92 BWM= 91 BWM=
   BWM-LOCAL2 0 BWM= 7 BWM= 5 BWM=
   BWM-LOCAL4 0 BWM= 93 BWM= 92 BWM= 91 BWM= 5 BWM=
   5 BWM-BR2 0 BWM= 7 BWM=
   -3 BWM-BR2 1 BWM= 8 BWM=
   5 BWM-BRW 0 BWM= 0 BWM= 0 BWM= 7 BWM=
   -3 BWM-BRW 0 BWM= 93 BWM= 92 BWM= 91 BWM=
   1 BWM-BRMIX 0 BWM= 7 BWM= 5 BWM=
   0 BWM-BRMIX 8 BWM= 7 BWM= 6 BWM=
   1 BWM-BROUTER 0 BWM= 93 BWM= 92 BWM= 91 BWM=
   0 BWM-BROUTER 0 BWM= 93 BWM= 92 BWM= 91 BWM= ;

: BWM-REPORT ( -- )
   BWM-FAILS @ 0= if s" ok" type cr exit then
   BWM-FAILS @ . s" bootstrap-wide-memory failures" 1 die ;

BWM-TEST-GOLDENS
BWM-TEST-RUNTIME
BWM-REPORT
