\ bootstrap-wide-memory-src.f - stage0 wide ADT memory execution/goldens.

\ This isolated fixture does not load xref.f's constructor-package registration
\ bridge. Keep the hook inert while the fixture exercises wide codegen.
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

1 LAYOUT-BUFFER BWM-MEM2 bwm2<n>
1 LAYOUT-BUFFER BWM-MEM4 bwm4<n,n,n>

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
package BWM-LOCAL-TEST
: HIDDEN ( -- n ) 13 ;
: HIDDEN2 ( -- n ) 29 ;
public
\ typed-local-lint: allow-bare-local - family locals are not yet annotatable.
: DUAL ( -- n n n n n n )
   BWM-MK2 BWM-MK4 {: r m :} r BWM-UN2 m BWM-UN4 ;
\ typed-local-lint: allow-bare-local - family locals are not yet annotatable.
: DEEP ( -- n n n n n n )
   BWM-MK2 1 2 3 4 {: r a:n b:n c:n d:n :}
   a b c d r BWM-UN2 ;
: MARK ( -- n ) HIDDEN HIDDEN2 + ;
end-package
package BWM-NS-TEST
public
: DUAL ( -- n ) 17 ;
end-package
package BWM-TEST
: ABSENT ( ptr u8 n -- )
   0 search-wl 0= if -1 else 0 then -1 BWM= ;
: CERT-PROBE ( -- n )
   LOWER-CERT:WF-COUNT-CELL
   LOWER-CERT:BIND-COUNT-CELL +
   LOWER-CERT:FETCH-COUNT-CELL +
   LOWER-CERT:HEADER-CELLS +
   LOWER-CERT:WF-CELLS +
   LOWER-CERT:FETCH-CELLS + ;
public
: NAMESPACE ( -- )
   CERT-PROBE 32 BWM=
   BWM-LOCAL-TEST:DUAL 0 BWM= 93 BWM= 92 BWM= 91 BWM= 0 BWM= 7 BWM=
   BWM-LOCAL-TEST:DEEP 0 BWM= 7 BWM= 4 BWM= 3 BWM= 2 BWM= 1 BWM=
   BWM-LOCAL-TEST:MARK 42 BWM=
   BWM-NS-TEST:DUAL 17 BWM=
   s" DUAL" ABSENT
   s" HIDDEN" ABSENT
   s" HIDDEN2" ABSENT
   s" BWM-LOCAL-TEST:DUAL" ABSENT
   s" BWM-LOCAL-TEST:HIDDEN" ABSENT
   s" BWM-LOCAL-TEST:HIDDEN2" ABSENT ;
end-package
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

: BWM-MASK-GOLD ( n n n -- ) {: idx:n mask:n want:n :}
   BWM-GXT @ idx 4 * BWM-W32 mask and want BWM= ;

: BWM-STORE-GOLD ( ptr u8 n n n n -- )
   {: name:ptr nameu:n width:n sub:n pop:n :}
   name nameu BWM-XT BWM-GXT !
   0 $D10043FF BWM-GOLD  1 $F90003FE BWM-GOLD
   2 $D1002273 BWM-GOLD  3 $F940026A BWM-GOLD
   4 sub BWM-GOLD        5 width BWM-GOLD
   6 $FFE0001F $D280000B BWM-MASK-GOLD
   7 $FFE0001F $D2800010 BWM-MASK-GOLD
   8 $FFE0001F $F2A00010 BWM-MASK-GOLD
   9 $FFE0001F $F2C00010 BWM-MASK-GOLD
   10 $FFFFFFFF $D63F0200 BWM-MASK-GOLD
   11 $F94001CF BWM-GOLD 12 $F900014F BWM-GOLD
   13 $910021CE BWM-GOLD 14 $9100214A BWM-GOLD
   15 $F1000529 BWM-GOLD 16 $54FFFF61 BWM-GOLD
   17 pop BWM-GOLD
   18 $F94003FE BWM-GOLD 19 $910043FF BWM-GOLD
   20 $D65F03C0 BWM-GOLD ;

: BWM-FETCH-GOLD ( ptr u8 n n n n -- )
   {: name:ptr nameu:n wop:n tag:n lim:n :}
   name nameu BWM-XT BWM-GXT !
   0 $D10043FF BWM-GOLD  1 $F90003FE BWM-GOLD
   2 $FFE0001F $D2800010 BWM-MASK-GOLD
   3 $FFE0001F $F2A00010 BWM-MASK-GOLD
   4 $FFE0001F $F2C00010 BWM-MASK-GOLD
   5 $FFFFFFFF $D63F0200 BWM-MASK-GOLD
   6 $14000009 BWM-GOLD
   7 1 BWM-GOLD  8 0 BWM-GOLD
   9 tag BWM-GOLD  10 0 BWM-GOLD
   11 lim BWM-GOLD  12 0 BWM-GOLD
   13 0 BWM-GOLD  14 0 BWM-GOLD
   15 $D1002273 BWM-GOLD  16 $F940026A BWM-GOLD
   17 wop BWM-GOLD        18 $F940014B BWM-GOLD
   19 $9100214A BWM-GOLD  20 $F900026B BWM-GOLD
   21 $91002273 BWM-GOLD  22 $F1000529 BWM-GOLD
   23 $54FFFF61 BWM-GOLD
   24 $F94003FE BWM-GOLD  25 $910043FF BWM-GOLD
   26 $D65F03C0 BWM-GOLD ;

: BWM-TEST-GOLDENS ( -- )
   s" BWM-STORE2-G" $D2800049 $D100426E $D1004273 BWM-STORE-GOLD
   s" BWM-FETCH2-G" $D2800049 1 2 BWM-FETCH-GOLD
   s" BWM-STORE4-G" $D2800089 $D100826E $D1008273 BWM-STORE-GOLD
   s" BWM-FETCH4-G" $D2800089 3 1 BWM-FETCH-GOLD ;

: BWM-TEST-RUNTIME ( -- )
   BWM-TEST:NAMESPACE
   BWM-RUN2 0 BWM= 7 BWM=
   BWM-RUN4 0 BWM= 93 BWM= 92 BWM= 91 BWM=
   BWM-LOCAL2 0 BWM= 7 BWM= 5 BWM=
   BWM-LOCAL4 0 BWM= 93 BWM= 92 BWM= 91 BWM= 5 BWM=
   BWM-LOCAL-TEST:DUAL 0 BWM= 93 BWM= 92 BWM= 91 BWM= 0 BWM= 7 BWM=
   BWM-LOCAL-TEST:DEEP 0 BWM= 7 BWM= 4 BWM= 3 BWM= 2 BWM= 1 BWM=
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
