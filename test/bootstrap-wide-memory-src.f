\ bootstrap-wide-memory-src.f - stage0 wide ADT memory execution/goldens: the
\ engine helper each wide transfer reaches, the span and tag it proves, and
\ the values it delivers. Only the Gforth recovery checks run it, through
\ test/bootstrap-wide-memory.fs (tools/bootstrap.sh, test/nf-path-test.f).

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

SUMTYPE bwm2<a>
  VARIANT pair a ;VARIANT
  VARIANT other a ;VARIANT
;SUMTYPE

SUMTYPE bwm4<a,b,c>
  VARIANT quad a b c ;VARIANT
;SUMTYPE

variable BWM-FAILS
variable BWM-CASES
align variable BWM-ATOMIC

: BWM-ORDINARY ( -- ) ;
: BWM-IMMEDIATE ( -- ) ; immediate

: BWM-FAIL ( -- )
   BWM-FAILS @ 1 + BWM-FAILS ! ;

\ The stage0 seed has no inline number printer and cannot load src/lib/fmt.f, so
\ each number closes its own line with `.`'s newline.
: BWM= ( n n -- ) {: got:n want:n :}
   BWM-CASES @ 1 + BWM-CASES !
   got want <> if
      BWM-FAIL
      s" case " type BWM-CASES @ .
      s" expected " type want .
      s" got " type got .
   then ;

: BWM-TEST-ATOMICS ( -- )
   17 BWM-ATOMIC !
   18 23 BWM-ATOMIC atomic-cas 17 BWM=
   BWM-ATOMIC @ 17 BWM=
   17 42 BWM-ATOMIC atomic-cas 17 BWM=
   BWM-ATOMIC @ 42 BWM=
   93 BWM-ATOMIC atomic! BWM-ATOMIC atomic@ 93 BWM=
   7 BWM-ATOMIC atomic-add 93 BWM=
   BWM-ATOMIC atomic@ 100 BWM=
   -13 BWM-ATOMIC atomic-add 100 BWM=
   fence BWM-ATOMIC atomic@ 87 BWM= ;

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

\ The unpackers leave the payload cells deep and the variant tag on top.
: BWM-UN2 ( bwm2<n> -- n n )
   MATCH bwm2 pair OF 0 ENDOF other OF 1 ENDOF ;MATCH ;
: BWM-UN4 ( bwm4<n,n,n> -- n n n n )
   MATCH bwm4 quad OF 0 ENDOF ;MATCH ;
: BWM-XT ( ptr u8 n -- n ) 0 search-wl ;
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
public
: NAMESPACE ( -- )
   LOWER-CERT:WF-COUNT-CELL drop
   LOWER-CERT:BIND-COUNT-CELL drop
   LOWER-CERT:FETCH-COUNT-CELL drop
   LOWER-CERT:HEADER-CELLS drop
   LOWER-CERT:WF-CELLS drop
   LOWER-CERT:FETCH-CELLS drop
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

package BWM-CODE
\ An xt is a code address integer; its instruction words need a byte view.
CAST: >CODE ( n -- ptr u8 )
public
: W32 ( n n -- n )
   + >CODE {: p:ptr :}
   p c@  p 1 + c@ 8 lshift or  p 2 + c@ 16 lshift or  p 3 + c@ 24 lshift or ;
;package

variable BWM-GXT

: BWM-GOLD ( n n -- ) {: idx:n want:n :}
   BWM-GXT @ idx 4 * BWM-CODE:W32 want BWM= ;

: BWM-MASK-GOLD ( n n n -- ) {: idx:n mask:n want:n :}
   BWM-GXT @ idx 4 * BWM-CODE:W32 mask and want BWM= ;

\ The goldens pin the helper each transfer reaches, the span the store proves
\ before it mutates memory, and the tag descriptor the fetch hands its
\ validator; width and order are executed (BWM-TEST-RUNTIME). Refusals are
\ process exits and live in test/engine-stack-wide.f. An engine helper call is
\ one `BL imm26` when the code region maps within BL's +/-128 MiB of __text
\ (native bin/hb), and the absolute `movz/movk/movk x16 ; blr x16` chain when it
\ is far (the Gforth stage0 seed maps at RBASE-VA): the goldens detect the form
\ at the call site and continue after it (index + 1 for BL, index + 4 for the
\ chain).
: BWM-CALL-GOLD ( n -- n ) {: idx:n :}
   BWM-GXT @ idx 4 * BWM-CODE:W32 $FC000000 and $94000000 = if
      idx $FC000000 $94000000 BWM-MASK-GOLD  idx 1 + exit
   then
   idx     $FFE0001F $D2800010 BWM-MASK-GOLD
   idx 1 + $FFE0001F $F2A00010 BWM-MASK-GOLD
   idx 2 + $FFE0001F $F2C00010 BWM-MASK-GOLD
   idx 3 + $FFFFFFFF $D63F0200 BWM-MASK-GOLD
   idx 4 + ;

\ A wide store ( value ptr -- ) proves the whole destination span with
\ (PROT-SPAN), x10 destination, x11 bytes, x9 cells, before the first mutating
\ store. AArch64 writes the base register back in a load or store, so the frame
\ is one `str x30,[sp,#-16]!` and the pop one `ldr x10,[x19,#-8]!`; both engines
\ emit these, so the goldens are the same on each. Both reach the code window
\ through one add/sub-immediate emitter, which folds the byte operand into one
\ word below 4096; every width here is far below it, so the sub at index 2 is
\ one word. Measured on the stage0 seed with `HABU_TARGET=linux-aarch64 gforth
\ test/bootstrap-wide-memory.fs`, decoding the compiled
\ BWM-STORE2-G/BWM-STORE4-G bytes.
: BWM-STORE-GOLD ( ptr u8 n n -- ) {: name:ptr nameu:n width:n :}
   name nameu BWM-XT BWM-GXT !
   0 $F81F0FFE BWM-GOLD                           \ str x30,[sp,#-16]!
   1 $F85F8E6A BWM-GOLD                           \ ldr x10,[x19,#-8]! destination
   2 $D100026E width 10 lshift or BWM-GOLD        \ sub x14,x19,#width source
   3 $D2800009 width 8 / 5 lshift or BWM-GOLD     \ movz x9,#cells
   4 $D280000B width 5 lshift or BWM-GOLD         \ movz x11,#width
   5 BWM-CALL-GOLD drop ;                         \ (PROT-SPAN)

\ A wide fetch ( ptr -- value ) first calls (LP2VEXEC), whose descriptor follows
\ the return site behind a branch over it: one CHECK row naming the tag cell's
\ offset, the exclusive tag domain and no guards, so an invalid tag is refused
\ before the address is popped. Decoded from the compiled
\ BWM-FETCH2-G/BWM-FETCH4-G bytes.
: BWM-FETCH-GOLD ( ptr u8 n n n n -- )
   {: name:ptr nameu:n width:n tag:n lim:n :}
   name nameu BWM-XT BWM-GXT !
   0 $F81F0FFE BWM-GOLD                           \ str x30,[sp,#-16]!
   1 BWM-CALL-GOLD {: n:n :}                      \ index after the (LP2VEXEC) call
   n $14000009 BWM-GOLD                           \ b over the descriptor
   n 1 + 1 BWM-GOLD    n 2 + 0 BWM-GOLD           \ one CHECK row
   n 3 + tag BWM-GOLD  n 4 + 0 BWM-GOLD           \ tag cell offset
   n 5 + lim BWM-GOLD  n 6 + 0 BWM-GOLD           \ exclusive tag domain
   n 7 + 0 BWM-GOLD    n 8 + 0 BWM-GOLD           \ no guards
   n 9 +  $F85F8E6A BWM-GOLD                      \ ldr x10,[x19,#-8]! source
   n 10 + $D2800009 width 8 / 5 lshift or BWM-GOLD ; \ movz x9,#cells

: BWM-TEST-GOLDENS ( -- )
   s" BWM-STORE2-G" 16 BWM-STORE-GOLD
   s" BWM-FETCH2-G" 16 1 2 BWM-FETCH-GOLD
   s" BWM-STORE4-G" 32 BWM-STORE-GOLD
   s" BWM-FETCH4-G" 32 3 1 BWM-FETCH-GOLD ;

\ Every VM stack run-in-stack accepts is a guarded mapping rounded up to a whole
\ STACK-ABI:PAGE-BYTES (GUARDED-EXTENT?, STACK-ABI:E-STACK-UNGUARDED), far
\ larger than these transfers, so no extent fits one exactly: the goldens pin
\ each transfer's byte and cell count. BWM-TEST-SWITCHED runs the wide store on
\ such a mapping and reads the value back through the plain wide fetch once the
\ switch has unwound, so the values moved prove it. Both engines reach
\ src/lib/memory.f, the seed through the same `require` in the source it bakes.
require src/lib/memory.f
PTR-VARIABLE BWM-POOL-A
STACK-ABI:PAGE-BYTES MEM:ALLOC-GUARDED drop BWM-POOL-A !
: BWM-POOL ( -- ptr u8 ) BWM-POOL-A @ ;

: BWM-SWITCHED-STORE ( -- ) BWM-MK2 BWM-STORE2 ;

: BWM-TEST-SWITCHED ( -- )
   ['] BWM-SWITCHED-STORE BWM-POOL STACK-ABI:PAGE-BYTES run-in-stack
   BWM-FETCH2 BWM-UN2 0 BWM= 7 BWM= ;

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

\ defer / is round-trip through the stage0 engine. A fresh defer's dispatch cell
\ holds DEFER-UNSET (fail closed, not garbage); `is` installs a target the word
\ then dispatches to, and a second `is` re-points it.
defer BWM-DEF ( -- n )

: BWM-DEF-A ( -- ) [: 42 ;] is BWM-DEF ;
: BWM-DEF-B ( -- ) [: 99 ;] is BWM-DEF ;
: BWM-CALL-DEF ( -- n ) BWM-DEF ;

: BWM-RD64 ( n -- n )  dup 0 BWM-CODE:W32  swap 4 BWM-CODE:W32  32 lshift  or ;

\ The native compiler can replace the engine's defer body with its own dispatch
\ body. The dictionary records the code length on both paths; the two-cell
\ trailer starts at that boundary.
: BWM-DEFER-META ( n -- n )
   {: xt:n :}
   ndict@ 0 ?do
      i XREF-REC {: rec:ptr :}
      rec XREF-START xt = if
         xt rec XREF-LEN + {: meta:n :}
         meta 0 BWM-CODE:W32 $46455201 =
         meta 4 BWM-CODE:W32 $48424445 = and 0= if
            s" bootstrap-wide-memory: malformed defer trailer" 1 die
         then
         meta unloop exit
      then
   loop
   s" bootstrap-wide-memory: defer record missing" 1 die ;

: BWM-TEST-DEFER ( -- )
   s" BWM-DEF" BWM-XT {: xt:n :}
   xt BWM-DEFER-META 8 + BWM-RD64 BWM-RD64 BWM-FRESH ! \ the fresh dispatch cell's value: the unset target
   BWM-FRESH @ BWM-NONZERO                    \ fail closed before any is: never a null cell
   BWM-DEF-A  BWM-CALL-DEF 42 BWM=
   BWM-DEF-B  BWM-CALL-DEF 99 BWM= ;

\ A checked defer / is round-trip. `is` in a checked (non-TRUSTED) definition
\ compiles after the check hook is live, so the seed's mirror C-DEFER registers
\ the defer's declared effect with the checker (the trust usig row and the
\ checker-defer row), as habu2.f C-DEFER does; without it the seed's hook
\ rejects the installer with exit 70 'hook: non-certified definition: bwm-cdef!
\ at is'. BWM-CDEF! is a `:` word with an xt-effect parameter, the hook shape `:
\ FOO! ( [E] -- ) is FOO ;`.
defer BWM-CDEF ( -- n )

: BWM-CDEF! ( [ -- n ] -- )  is BWM-CDEF ;          \ checked installer (xt-effect param)
: BWM-CDEF-A ( -- )  [: 42 ;] BWM-CDEF! ;
: BWM-CDEF-B ( -- )  [: 99 ;] BWM-CDEF! ;
: BWM-CALL-CDEF ( -- n )  BWM-CDEF ;

: BWM-TEST-CDEFER ( -- )
   s" BWM-CDEF" BWM-XT {: xt:n :}
   xt BWM-DEFER-META 8 + BWM-RD64 BWM-RD64
   BWM-FRESH @ BWM=                           \ = the same unset target BWM-DEF held
   BWM-CDEF-A  BWM-CALL-CDEF 42 BWM=
   BWM-CDEF-B  BWM-CALL-CDEF 99 BWM= ;

variable BWM-CF-ACC

\ The seed's extended handler frame: a caught throw restores the caller's user
\ return-stack and loop-stack depths, through bootstrap/cg/forth.fs
\ BCATCH/BTHROW at runtime, as native test/catch-frame.f does. These are
\ checked: the seed models catch's typed-stack restoration, so a throwing
\ quotation's >r/?do state does not escape.
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
\ `constant` publishes `-- a` and `:` its declared effect, each from its own
\ definer, and every row carries the definition's qualified spelling: a word
\ defined as `PKG:TAIL` certifies through `PKG:TAIL`, where a row under the bare
\ tail would leave the qualified call unknown. A missing or bare-named row stops
\ these definitions loading. The negative half is
\ test/bootstrap-created-*-src.f. The consumers declare the concrete kinds they
\ use (`ptr n`, `n`): the strict parametric rule keeps `ptr a` and `a` for
\ effects that stay parametric.
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
BWM-TEST-SWITCHED
BWM-TEST-ATOMICS
BWM-TEST-DEFER
BWM-TEST-CDEFER
BWM-TEST-CATCH-FRAME
BWM-PUBLISHED:TEST
BWM-REPORT
