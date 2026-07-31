\ bootstrap-wide-memory-src.f - stage0 wide ADT memory execution/goldens.

\ This isolated fixture does not load the namespace finalization/protection owner.
\ Keep the hook inert while the fixture exercises wide codegen.
: BWM-STAGE0-PROT-NOP ( ptr u8 n -- ) 2drop ;
: BWM-PROT-INSTALL ( -- ) [: BWM-STAGE0-PROT-NOP ;] is TDECL-FINALIZE-XT ;
BWM-PROT-INSTALL
-1 TDECL-FINALIZE-ARMED !

SUMTYPE bwm2 1
  VARIANT pair a ;VARIANT
  VARIANT other a ;VARIANT
;SUMTYPE

SUMTYPE bwm4 3
  VARIANT quad a b c ;VARIANT
;SUMTYPE

package bwm:deep-zone
public
PRODUCT pair 0
  FIELD left n
  FIELD right n
;PRODUCT
;package

variable BWM-FAILS
variable BWM-CASES

: BWM-ORDINARY ( -- ) ;
: BWM-IMMEDIATE ( -- ) ; immediate

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
;package
package BWM-NS-TEST
public
: DUAL ( -- n ) 17 ;
;package
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
;package
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

\ The engine helper call is one direct `BL imm26` when the JIT region maps within
\ BL's +/-128 MiB of __text (native bin/hb, dot habu-map-the-code + habu-aot-repl-bl),
\ and the absolute `movz/movk/movk x16 ; blr x16` chain when the region is far (the
\ Gforth stage0 seed still maps at RBASE-VA). Both are the correct call for their
\ region, so the goldens detect the form at the call site and continue at the
\ instruction after it (index + 1 for BL, index + 4 for the chain).
: BWM-CALL-GOLD ( n -- n ) {: idx:n :}
   BWM-GXT @ idx 4 * BWM-W32 $FC000000 and $94000000 = if
      idx $FC000000 $94000000 BWM-MASK-GOLD  idx 1 + exit
   then
   idx     $FFE0001F $D2800010 BWM-MASK-GOLD
   idx 1 + $FFE0001F $F2A00010 BWM-MASK-GOLD
   idx 2 + $FFE0001F $F2C00010 BWM-MASK-GOLD
   idx 3 + $FFFFFFFF $D63F0200 BWM-MASK-GOLD
   idx 4 + ;

: BWM-STORE-GOLD ( ptr u8 n n n n -- )
   {: name:ptr nameu:n width:n sub:n pop:n :}
   name nameu BWM-XT BWM-GXT !
   0 $D10043FF BWM-GOLD  1 $F90003FE BWM-GOLD
   2 $D1002273 BWM-GOLD  3 $F940026A BWM-GOLD
   4 sub BWM-GOLD        5 width BWM-GOLD
   6 $FFE0001F $D280000B BWM-MASK-GOLD
   7 BWM-CALL-GOLD {: n:n :}                  \ index after the LPROTSPAN call
   n     $F94001CF BWM-GOLD  n 1 + $F900014F BWM-GOLD
   n 2 + $910021CE BWM-GOLD  n 3 + $9100214A BWM-GOLD
   n 4 + $F1000529 BWM-GOLD  n 5 + $54FFFF61 BWM-GOLD
   n 6 + pop BWM-GOLD
   n 7 + $F94003FE BWM-GOLD  n 8 + $910043FF BWM-GOLD
   n 9 + $D65F03C0 BWM-GOLD ;

: BWM-FETCH-GOLD ( ptr u8 n n n n -- )
   {: name:ptr nameu:n wop:n tag:n lim:n :}
   name nameu BWM-XT BWM-GXT !
   0 $D10043FF BWM-GOLD  1 $F90003FE BWM-GOLD
   2 BWM-CALL-GOLD {: n:n :}                  \ index after the LP2VEXEC call
   n $14000009 BWM-GOLD
   n 1 + 1 BWM-GOLD    n 2 + 0 BWM-GOLD
   n 3 + tag BWM-GOLD  n 4 + 0 BWM-GOLD
   n 5 + lim BWM-GOLD  n 6 + 0 BWM-GOLD
   n 7 + 0 BWM-GOLD    n 8 + 0 BWM-GOLD
   n 9 + $D1002273 BWM-GOLD  n 10 + $F940026A BWM-GOLD
   n 11 + wop BWM-GOLD       n 12 + $F940014B BWM-GOLD
   n 13 + $9100214A BWM-GOLD n 14 + $F900026B BWM-GOLD
   n 15 + $91002273 BWM-GOLD n 16 + $F1000529 BWM-GOLD
   n 17 + $54FFFF61 BWM-GOLD
   n 18 + $F94003FE BWM-GOLD n 19 + $910043FF BWM-GOLD
   n 20 + $D65F03C0 BWM-GOLD ;

: BWM-TEST-GOLDENS ( -- )
   s" BWM-STORE2-G" $D2800049 $D100426E $D1004273 BWM-STORE-GOLD
   s" BWM-FETCH2-G" $D2800049 1 2 BWM-FETCH-GOLD
   s" BWM-STORE4-G" $D2800089 $D100826E $D1008273 BWM-STORE-GOLD
   s" BWM-FETCH4-G" $D2800089 3 1 BWM-FETCH-GOLD ;

: BWM-TEST-RUNTIME ( -- )
   s" BWM-ORDINARY" tok-imm? 0 BWM=
   s" BWM-IMMEDIATE" tok-imm? 2 BWM=
   s" BWM-MISSING" tok-imm? 0 BWM=
   BWM-TEST:NAMESPACE
   BWM-RUN2 0 BWM= 7 BWM=
   BWM-RUN4 0 BWM= 93 BWM= 92 BWM= 91 BWM=
   11 22 BWM:DEEP-ZONE:PAIR:MAKE BWM:DEEP-ZONE:PAIR:UNMAKE
      22 BWM= 11 BWM=
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

\ defer / is round-trip through the stage0 engine (dot
\ habu-mirror-defer-is-4461fe23). A fresh defer's dispatch cell holds DEFER-UNSET
\ (fail closed, not garbage); `is` installs a target the word then dispatches to,
\ and a second `is` re-points it. The defer-touching words are TRUSTED: so the
\ check hook is skipped, exactly as an unchecked boot-prefix file is processed
\ (the seed's engine keyword path is what stage0 recovery re-reads at startup).
defer BWM-DEF ( -- n )

TRUSTED: BWM-DEF-A ( -- ) [: 42 ;] is BWM-DEF ;
TRUSTED: BWM-DEF-B ( -- ) [: 99 ;] is BWM-DEF ;
TRUSTED: BWM-CALL-DEF ( -- n ) BWM-DEF ;

: BWM-RD64 ( addr -- x )  dup 0 BWM-W32  swap 4 BWM-W32  32 lshift  or ;

: BWM-TEST-DEFER ( -- )
   s" BWM-DEF" BWM-XT {: xt:n :}
   xt 44 BWM-W32  $46455201 BWM=              \ DEFER-MAGIC low word: meta trailer sits at addr+clen
   xt 48 BWM-W32  $48424445 BWM=              \ DEFER-MAGIC high word
   xt 52 + BWM-RD64 BWM-RD64                  \ the fresh dispatch cell's value
   s" DEFER-UNSET" BWM-XT BWM=                \ = DEFER-UNSET code addr (fail closed, before any is)
   BWM-DEF-A  BWM-CALL-DEF 42 BWM=            \ is installs a target -> dispatch returns 42
   BWM-DEF-B  BWM-CALL-DEF 99 BWM= ;          \ a second is re-points -> 99

\ CHECKED-path defer / is round-trip (dot habu-mirror-checker-defer-6a8a366e).
\ The stage2 engine-hook migration puts `is` inside CHECKED (non-TRUSTED)
\ definitions that compile after the check hook is live, so the mirror C-DEFER
\ must register the defer's declared effect with the checker (the trust usig row
\ plus the checker-defer row), exactly as native habu2.f C-DEFER does. Without
\ that bridge the seed's check hook rejects the installer body with exit 70
\ 'hook: non-certified definition: bwm-cdef! at is'. Unlike the TRUSTED sibling
\ above, BWM-CDEF! is a plain `:` word carrying an xt-effect parameter (the
\ stage2a hook shape `: FOO! ( [E] -- ) is FOO ;`): its `is BWM-CDEF` and the
\ later checked BWM-CDEF calls certify only once the seed's checker learns the
\ defer.
defer BWM-CDEF ( -- n )

: BWM-CDEF! ( [ -- n ] -- )  is BWM-CDEF ;          \ checked installer (xt-effect param)
: BWM-CDEF-A ( -- )  [: 42 ;] BWM-CDEF! ;           \ install a 42-returning target
: BWM-CDEF-B ( -- )  [: 99 ;] BWM-CDEF! ;           \ re-install a 99-returning target
: BWM-CALL-CDEF ( -- n )  BWM-CDEF ;                \ checked call through the defer

: BWM-TEST-CDEFER ( -- )
   s" BWM-CDEF" BWM-XT {: xt:n :}
   xt 44 BWM-W32  $46455201 BWM=              \ DEFER-MAGIC low word: meta trailer sits at addr+clen
   xt 48 BWM-W32  $48424445 BWM=              \ DEFER-MAGIC high word
   xt 52 + BWM-RD64 BWM-RD64                  \ the fresh dispatch cell's value
   s" DEFER-UNSET" BWM-XT BWM=                \ = DEFER-UNSET code addr (fail closed, before any is)
   BWM-CDEF-A  BWM-CALL-CDEF 42 BWM=          \ checked is installs a target -> dispatch returns 42
   BWM-CDEF-B  BWM-CALL-CDEF 99 BWM= ;        \ a second checked is re-points -> 99

variable BWM-CF-ACC

\ Direct stage0 (Gforth-hosted seed) proof of the extended handler frame
\ (dot habu-restore-complete-exec-abb8baca): a caught throw restores the caller's
\ user return-stack depth and loop-stack depth, so this exercises the seed's
\ bootstrap/cg/forth.fs BCATCH/BTHROW mirror at runtime, matching the native
\ test/catch-frame.f. These are checked (the seed models catch's typed-stack
\ restoration, so a throwing quotation's >r/?do state does not escape).
: BWM-CF-RSP ( -- n )    \ r> after a throwing >r quotation = caller's value
   42 >r [: 99 >r 7 throw ;] catch drop r> ;

: BWM-CF-LOOP ( -- n )   \ caller ?do index survives a throwing inner ?do
   0 BWM-CF-ACC !
   3 0 ?do [: 5 0 ?do 9 throw loop ;] catch drop  BWM-CF-ACC @ i + BWM-CF-ACC ! loop
   BWM-CF-ACC @ ;

: BWM-TEST-CATCH-FRAME ( -- )
   BWM-CF-RSP 42 BWM=
   BWM-CF-LOOP 3 BWM= ;

: BWM-REPORT ( -- )
   BWM-FAILS @ 0= if s" ok" type cr exit then
   BWM-FAILS @ . s" bootstrap-wide-memory failures" 1 die ;

BWM-TEST-GOLDENS
BWM-TEST-RUNTIME
BWM-TEST-DEFER
BWM-TEST-CDEFER
BWM-TEST-CATCH-FRAME
BWM-REPORT
