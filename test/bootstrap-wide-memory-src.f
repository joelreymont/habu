\ bootstrap-wide-memory-src.f - stage0 wide ADT memory execution/goldens: the
\ bounds each wide transfer requests, the span and tag it proves, and the
\ values and exact fit it delivers, on the native engine and the stage0 seed.

\ This isolated fixture does not load xref.f's constructor-package registration
\ bridge. Keep the hook inert while the fixture exercises wide codegen.
using TYPE-DECL

package BWM-PROT
private
: STAGE0-NOP ( ptr u8 n -- ) 2drop ;
: INSTALL ( -- ) [: STAGE0-NOP ;] is TYPE-DECL:TDECL-PROT-WID-XT ;
INSTALL
;package

-1 TDECL-PROT-WID-ARMED !
;using

SUMTYPE bwm2 1
  VARIANT pair a ;VARIANT
  VARIANT other a ;VARIANT
;SUMTYPE

SUMTYPE bwm4 3
  VARIANT quad a b c ;VARIANT
;SUMTYPE

variable BWM-FAILS
variable BWM-CASES
align variable BWM-ATOMIC

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

: BWM-TEST-ATOMICS ( -- )
   17 BWM-ATOMIC !
   18 23 BWM-ATOMIC atomic-cas 17 BWM=
   BWM-ATOMIC @ 17 BWM=
   17 42 BWM-ATOMIC atomic-cas 17 BWM=
   BWM-ATOMIC @ 42 BWM=
   93 BWM-ATOMIC atomic! BWM-ATOMIC @ 93 BWM= ;

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
\ The unset target is a sealed engine word that no tick may reach (habu2.f
\ C-BTICK carries the internal-word gate), so a fresh dispatch cell is measured
\ against the other fresh cell: both hold the same non-zero target before any is.
variable BWM-FRESH
: BWM-NONZERO ( n -- ) 0 <> if -1 else 0 then -1 BWM= ;

: BWM-RUN2 ( -- n n )
   BWM-MK2 BWM-STORE2
   0 BWM-FETCH2-LOCAL BWM-UN2 ;

: BWM-RUN4 ( -- n n n n )
   BWM-MK4 BWM-STORE4
   BWM-FETCH4 BWM-UN4 ;

: BWM-LOCAL2 ( -- n n n ) BWM-MK2 5 {: r s:n :} s r BWM-UN2 ;
: BWM-LOCAL4 ( -- n n n n n ) 5 BWM-MK4 {: s:n m :} s m BWM-UN4 ;
package BWM-LOCAL-TEST
: HIDDEN ( -- n ) 13 ;
: HIDDEN2 ( -- n ) 29 ;
public
: DUAL ( -- n n n n n n )
   BWM-MK2 BWM-MK4 {: r m :} r BWM-UN2 m BWM-UN4 ;
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
: BWM-BR2 ( n -- n n ) 0 > if BWM-MK2 {: r :} r BWM-UN2
   else BWM-MK2B {: r :} r BWM-UN2 then ;
: BWM-BRW ( n -- n n n n ) 0 > if BWM-MK2 {: r :} r BWM-UN2 0 0
   else BWM-MK4 {: m :} m BWM-UN4 then ;
: BWM-BRMIX ( n -- n n n ) 0 > if BWM-MK2 5 {: r s:n :} s r BWM-UN2 else 6 7 8 then ;
: BWM-BROUTER ( n -- n n n n ) BWM-MK4 {: m :}
   0 > if BWM-MK2 {: r :} r BWM-UN2 drop drop then m BWM-UN4 ;

TRUSTED: BWM-W32 ( n n -- n )
   + dup c@ over 1 + c@ 8 lshift or
   over 2 + c@ 16 lshift or swap 3 + c@ 24 lshift or ;

variable BWM-GXT

: BWM-GOLD ( n n -- ) {: idx:n want:n :}
   BWM-GXT @ idx 4 * BWM-W32 want BWM= ;

: BWM-MASK-GOLD ( n n n -- ) {: idx:n mask:n want:n :}
   BWM-GXT @ idx 4 * BWM-W32 mask and want BWM= ;

\ ---- what the compiled wide store and fetch must encode ----------------------
\ The goldens below pin the invariants a reader of the code can substantiate,
\ not the whole body: the bounds request each transfer makes and the helper it
\ reaches, the span the store proves before it mutates memory, and the tag
\ descriptor the fetch hands its validator. Width, order and the exact fit are
\ executed (BWM-TEST-RUNTIME, BWM-TEST-EXACT); refusals are process exits and
\ live in test/engine-stack-wide.f.
\
\ An engine helper call is one direct `BL imm26` when the JIT region maps within
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

\ The target of a branch word (BL or B) at a site, from its signed imm26.
: BWM-BRANCH-TARGET ( n n -- n ) {: site:n w:n :}
   w $3FFFFFF and dup $2000000 and 0 <> if $4000000 - then 4 * site + ;

\ The target of the call at idx: the BL's own target, or the address the
\ movz/movk chain loads into x16. Leaves the index after the call.
: BWM-CALL-TARGET ( n -- n n ) {: idx:n :}
   BWM-GXT @ idx 4 * BWM-W32 {: w:n :}
   w $FC000000 and $94000000 = if
      BWM-GXT @ idx 4 * + w BWM-BRANCH-TARGET idx 1 + exit
   then
   w 5 rshift $FFFF and
   BWM-GXT @ idx 1 + 4 * BWM-W32 5 rshift $FFFF and 16 lshift or
   BWM-GXT @ idx 2 + 4 * BWM-W32 5 rshift $FFFF and 32 lshift or
   idx 4 + ;

\ A bounds request: the caller frames x16/x17/LR, states the bytes it needs
\ below and above the data-stack top, and calls the DATA guard. Near the
\ helper (native) the call is a direct BL to the registered helper itself.
\ Far from it (the stage0 seed) the absolute chain consumes x16, so the
\ request parks the below-distance in the frame's fourth slot (SP+24) right
\ after loading it, and the chain reaches a shim that reloads x16 from that
\ slot and branches to the same helper. Either way the guard the request
\ reaches is the one stack-data-entry names. Leaves the index after the frame.
\ The shape of the request at idx, decided from the words after the below
\ load and never from a decoded address: 1 near (movz x17 then BL), 2 far
\ (str x16,[sp,#24], movz x17, movz/movk/movk x16, blr x16), 0 neither. An
\ unknown shape is a failed case that resolves nothing, so a stale engine
\ cannot hand the fixture an address to dereference.
: BWM-W32@ ( n -- n ) BWM-GXT @ swap 4 * BWM-W32 ;

: BWM-REQUEST-SHAPE ( n -- n ) {: idx:n :}
   idx 5 + BWM-W32@ $FFE0001F and $D2800011 =
   idx 6 + BWM-W32@ $FC000000 and $94000000 = and if 1 exit then
   idx 5 + BWM-W32@ $F9000FF0 =
   idx 6 + BWM-W32@ $FFE0001F and $D2800011 = and
   idx 7 + BWM-W32@ $FFE0001F and $D2800010 = and
   idx 8 + BWM-W32@ $FFE0001F and $F2A00010 = and
   idx 9 + BWM-W32@ $FFE0001F and $F2C00010 = and
   idx 10 + BWM-W32@ $D63F0200 = and if 2 exit then
   0 ;

\ Stop before decoding an arbitrary target or reading a guessed continuation.
: BWM-UNRESOLVED ( -- )
   s" bootstrap-wide-memory: invalid stack request" 1 die ;

: BWM-REQUEST-GOLD ( n n n -- n ) {: idx:n below:n above:n :}
   idx     $D10083FF BWM-GOLD                     \ sub sp,sp,#32
   idx 1 + $F90003F0 BWM-GOLD                     \ str x16,[sp]
   idx 2 + $F90007F1 BWM-GOLD                     \ str x17,[sp,#8]
   idx 3 + $F9000BFE BWM-GOLD                     \ str x30,[sp,#16]
   idx 4 + $D2800010 below 5 lshift or BWM-GOLD   \ movz x16,#below
   idx BWM-REQUEST-SHAPE {: shape:n :}
   shape 0= if
      BWM-CASES @ 1 + BWM-CASES !  BWM-FAIL
      s" case " type BWM-CASES @ . s" unknown request shape at " type idx . cr
      BWM-UNRESOLVED
   then
   shape 1 = if
      idx 5 + $D2800011 above 5 lshift or BWM-GOLD   \ movz x17,#above
      BWM-FAILS @ 0 <> if BWM-UNRESOLVED then
      idx 6 + BWM-CALL-TARGET {: target:n n:n :}
      target stack-data-entry BWM=
      n
   else
      idx 6 + $D2800011 above 5 lshift or BWM-GOLD   \ movz x17,#above
      BWM-FAILS @ 0 <> if BWM-UNRESOLVED then
      idx 7 + BWM-CALL-TARGET {: target:n n:n :}
      target 0 BWM-W32 $F9400FF0 BWM=                       \ shim: ldr x16,[sp,#24]
      target 4 BWM-W32 $FC000000 and $14000000 BWM=         \ shim: b helper
      target 4 + target 4 BWM-W32 BWM-BRANCH-TARGET stack-data-entry BWM=
      n
   then {: n:n :}
   n     $F94003F0 BWM-GOLD                       \ ldr x16,[sp]
   n 1 + $F94007F1 BWM-GOLD                       \ ldr x17,[sp,#8]
   n 2 + $F9400BFE BWM-GOLD                       \ ldr x30,[sp,#16]
   n 3 + $910083FF BWM-GOLD                       \ add sp,sp,#32
   n 4 + ;

\ A wide store ( value ptr -- ): before anything moves it asks for the value
\ and its address below the top, pops the address, addresses the value at its
\ full width (a register offset, never an imm12 that a wide value outgrows),
\ and proves the whole destination span with (PROT-SPAN) - x10 destination,
\ x11 bytes, x9 cells - before the first mutating store.
: BWM-STORE-GOLD ( ptr u8 n n -- ) {: name:ptr nameu:n width:n :}
   name nameu BWM-XT BWM-GXT !
   0 $D10043FF BWM-GOLD  1 $F90003FE BWM-GOLD
   2 width 8 + 0 BWM-REQUEST-GOLD {: n:n :}
   n     $D1002273 BWM-GOLD                       \ sub x19,x19,#8
   n 1 + $F940026A BWM-GOLD                       \ ldr x10,[x19]     destination
   n 2 + $D280000E width 5 lshift or BWM-GOLD     \ movz x14,#width
   n 3 + $CB0E026E BWM-GOLD                       \ sub x14,x19,x14   source
   n 4 + $D2800009 width 8 / 5 lshift or BWM-GOLD \ movz x9,#cells
   n 5 + $D280000B width 5 lshift or BWM-GOLD     \ movz x11,#width
   n 6 + BWM-CALL-GOLD drop ;                     \ (PROT-SPAN)

\ A wide fetch ( ptr -- value ): it first calls (LP2VEXEC), whose descriptor
\ follows the return site behind a branch over it - one CHECK row naming the
\ tag cell's offset, the exclusive tag domain and no guards - so an invalid tag
\ is refused before the address is popped. Then it asks for the address below
\ the top and the value's growth above it, and pops the address.
: BWM-FETCH-GOLD ( ptr u8 n n n n -- )
   {: name:ptr nameu:n width:n tag:n lim:n :}
   name nameu BWM-XT BWM-GXT !
   0 $D10043FF BWM-GOLD  1 $F90003FE BWM-GOLD
   2 BWM-CALL-GOLD {: n:n :}                      \ index after the (LP2VEXEC) call
   n $14000009 BWM-GOLD                           \ b over the descriptor
   n 1 + 1 BWM-GOLD    n 2 + 0 BWM-GOLD           \ one CHECK row
   n 3 + tag BWM-GOLD  n 4 + 0 BWM-GOLD           \ tag cell offset
   n 5 + lim BWM-GOLD  n 6 + 0 BWM-GOLD           \ exclusive tag domain
   n 7 + 0 BWM-GOLD    n 8 + 0 BWM-GOLD           \ no guards
   n 9 + 8 width 8 - BWM-REQUEST-GOLD {: m:n :}
   m     $D1002273 BWM-GOLD                       \ sub x19,x19,#8
   m 1 + $F940026A BWM-GOLD                       \ ldr x10,[x19]     source
   m 2 + $D2800009 width 8 / 5 lshift or BWM-GOLD ; \ movz x9,#cells

: BWM-TEST-GOLDENS ( -- )
   s" BWM-STORE2-G" 16 BWM-STORE-GOLD
   s" BWM-FETCH2-G" 16 1 2 BWM-FETCH-GOLD
   s" BWM-STORE4-G" 32 BWM-STORE-GOLD
   s" BWM-FETCH4-G" 32 3 1 BWM-FETCH-GOLD ;

\ The exact fit, executed: each transfer runs on an allocation of exactly the
\ bytes its request states, so the guard admits it at the last byte and the
\ transfer completes there. The destination waits in a local so the callback's
\ peak is the transfer's own: a store peaks at the value plus its address, a
\ fetch at the value that replaced its address. Measured on the native engine:
\ 24, 16, 40 and 32 bytes run, and one cell less exits 102 (a process exit,
\ so that half lives in test/engine-stack-wide.f).
create BWM-POOL 64 allot
1 LAYOUT-BUFFER BWM-SRC2 bwm2<n>
1 LAYOUT-BUFFER BWM-SRC4 bwm4<n,n,n>

: BWM-SOURCES ( -- ) 8 BWM2:OTHER 0 BWM-SRC2 !  1 2 3 BWM4:QUAD 0 BWM-SRC4 ! ;
: BWM-EXACT-STORE2 ( -- ) 0 BWM-MEM2 {: dest :} 0 BWM-SRC2 @ dest ! ;
: BWM-EXACT-FETCH2 ( -- ) 0 BWM-MEM2 @ BWM-UN2 drop drop ;
: BWM-EXACT-STORE4 ( -- ) 0 BWM-MEM4 {: dest :} 0 BWM-SRC4 @ dest ! ;
: BWM-EXACT-FETCH4 ( -- ) 0 BWM-MEM4 @ BWM-UN4 drop drop drop drop ;

: BWM-TEST-EXACT ( -- )
   BWM-SOURCES
   ['] BWM-EXACT-STORE2 BWM-POOL 24 run-in-stack
   BWM-FETCH2 BWM-UN2 1 BWM= 8 BWM=
   ['] BWM-EXACT-FETCH2 BWM-POOL 16 run-in-stack
   ['] BWM-EXACT-STORE4 BWM-POOL 40 run-in-stack
   BWM-FETCH4 BWM-UN4 0 BWM= 3 BWM= 2 BWM= 1 BWM=
   ['] BWM-EXACT-FETCH4 BWM-POOL 32 run-in-stack ;

: BWM-TEST-RUNTIME ( -- )
   s" BWM-ORDINARY" tok-imm? 0 BWM=
   s" BWM-IMMEDIATE" tok-imm? 2 BWM=
   s" BWM-MISSING" tok-imm? 0 BWM=
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

: BWM-RD64 ( n -- n )  dup 0 BWM-W32  swap 4 BWM-W32  32 lshift  or ;

: BWM-TEST-DEFER ( -- )
   s" BWM-DEF" BWM-XT {: xt:n :}
   xt 44 BWM-W32  $46455201 BWM=              \ DEFER-MAGIC low word: meta trailer sits at addr+clen
   xt 48 BWM-W32  $48424445 BWM=              \ DEFER-MAGIC high word
   xt 52 + BWM-RD64 BWM-RD64 BWM-FRESH !      \ the fresh dispatch cell's value: the unset target
   BWM-FRESH @ BWM-NONZERO                    \ fail closed before any is: never a null cell
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
   BWM-FRESH @ BWM=                           \ = the same unset target the trusted defer held
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

\ Created-word effect publication. `create` and `variable` publish `-- ptr a`,
\ `constant` publishes `-- a` and `:` publishes its declared effect, each from
\ its own definer, and the name every row carries is the definition's QUALIFIED
\ spelling. So a word defined as `PKG:TAIL` certifies through `PKG:TAIL`, where
\ a row registered under the bare tail would leave the qualified call unknown.
\ Certification is the assertion here: a missing or bare-named row stops these
\ definitions loading at all. The negative half is test/bootstrap-created-*-src.f.
\ The consumers below declare the concrete kinds they use (`ptr n`, `n`): the
\ strict parametric rule keeps `ptr a` and `a` for effects that stay parametric.
package BWM-PUB ;package

package BWM-PUBLISHED
create BWM-PUB:MADE 1 cells allot
variable BWM-PUB:CELL
41 constant BWM-PUB:KONST
: BWM-PUB:FN ( -- n ) 7 ;
: MADE-PTR ( -- ptr n ) BWM-PUB:MADE ;
: CELL-PTR ( -- ptr n ) BWM-PUB:CELL ;
: KONST-VAL ( -- n ) BWM-PUB:KONST ;
: FN-VAL ( -- n ) BWM-PUB:FN ;
public
: TEST ( -- )
   MADE-PTR drop
   CELL-PTR drop
   KONST-VAL 41 BWM=
   FN-VAL 7 BWM= ;
;package

: BWM-REPORT ( -- )
   BWM-FAILS @ 0= if s" ok" type cr exit then
   BWM-FAILS @ . s" bootstrap-wide-memory failures" 1 die ;

BWM-TEST-GOLDENS
BWM-TEST-RUNTIME
BWM-TEST-EXACT
BWM-TEST-ATOMICS
BWM-TEST-DEFER
BWM-TEST-CDEFER
BWM-TEST-CATCH-FRAME
BWM-PUBLISHED:TEST
BWM-REPORT
