\ native-chain.f - checked source programs compiled and executed by bin/hb.
\ Backend encoding, coalescing and spill plans are tested in native-select,
\ native-regalloc, native-emit and native-address-spill.

require lib/test.f
require lib/prelude.f
require src/compiler/native/abi.f
require src/arch/arm64/machine.f

package NCHAIN-TEST
private

create NCH-CELL 1 cells allot
4 BUFFER: NCH-BUF
5 BUFFER: NCH-BUF2

: NCH-SQ ( n -- n )
   dup * ;

: NCH-MAX ( n n -- n )
   2dup < if swap then drop ;

: NCH-BUMP ( n -- n )
   NCH-CELL ! NCH-CELL @ 1+ dup NCH-CELL ! ;

: NCH-LERP ( n n n -- n )
   {: a:n b:n t:n :} b a - t * 100 / a + ;

: NCH-BSUM ( ptr u8 n -- n )
   {: a:ptr u:n :} 0 u 0 ?do i a + c@ + loop ;

: NCH-BFIND ( ptr u8 n n -- n )
   {: a:ptr u:n c:n :} u 0 ?do i a + c@ c = if i unloop exit then loop -1 ;

: NCH-FACT ( n -- n )
   dup 1 <= if drop 1 exit then dup 1- RECURSE * ;

: NCH-SPILL ( n n n -- n )
   {: a:n b:n c:n :} a b + c + 0 a < if 1+ then a + b + c + ;

: NCH-RSPILL ( n -- n )
   {: a:n :} a 5 + a 2 + a 3 + a 4 + + + + 1 a < if a 1- RECURSE + then ;

: NCH-ISLT ( n n -- bool )
   < ;

: NCH-LTKEEP ( n n -- bool )
   < dup if then ;

: NCH-COUNT-CHAR ( ptr u8 n n -- n )
   {: a:ptr u:n c:n :} 0 0 begin dup u < while dup a + c@ c = if swap 1+ swap then
      1+ repeat drop ;

: NCH-WDOWN ( n -- n )
   begin dup 0 > while 1- repeat ;

: NCH-MAX-DIM ( n n -- n )
   {: a:n b:n :} a b > if a else b then ;

: NCH-SUMMAX ( n n -- n )
   2dup > if swap else nip dup then + ;

: NCH-TWOLOOP ( n -- n )
   begin dup 0 > while 1- repeat begin 1+ dup 0 >= until ;

: NCH-TWOIF ( n n -- n )
   2dup > if drop else nip then dup 0 < if 0 swap - then ;

: NCH-NESTW ( n -- n )
   begin dup 0 > while dup begin dup 0 > while 1- repeat drop 1- repeat ;

: NCH-ELSEXIT ( n -- n )
   dup 0 < if drop 0 else drop 7 exit then ;

: NCH-QWALK ( n -- n )
   [: dup 0 > if 1- RECURSE then ;] catch drop 100 + ;

: NCH-QWIDE ( n -- n n )
   [: dup 0 > if 1- RECURSE drop 10 + then ;] catch ;

\ More live arithmetic values than the production register pool can hold,
\ in a recursive routine whose frame also saves the return address.
: NCH-PRESSURE ( n -- n )
   {: a:n :}
   a 1 + a 2 + a 3 + a 4 + a 5 + a 6 + a 7 +
   a 8 + a 9 + a 10 + a 11 + a 12 + a 13 + a 14 +
   a 15 + a 16 + a 17 + a 18 + a 19 + a 20 + a 21 +
   a 22 + a 23 + a 24 + a 25 + a 26 + a 27 + a 28 +
   + + + + + + + + + + + + + + + + + + + + + + + + + + +
   1 a < if a 1- RECURSE + then ;

: BUFFER-MAKE ( -- )
   104 NCH-BUF c! 97 NCH-BUF 1 + c!
   98 NCH-BUF 2 + c! 117 NCH-BUF 3 + c!
   97 NCH-BUF2 c! 97 NCH-BUF2 1 + c! 98 NCH-BUF2 2 + c!
   99 NCH-BUF2 3 + c! 97 NCH-BUF2 4 + c! ;

: ARITHMETIC-CASE ( -- )
   s" source arithmetic and typed locals execute" T-LABEL
   7 NCH-SQ 49 T= 11 NCH-SQ 121 T=
   3 17 40 NCH-LERP 8 T=
   0 100 25 NCH-LERP 25 T=
   -40 0 30 NCH-LERP -28 T= ;

: MEM-CASE ( -- )
   s" a store replaces the old cell and a later load sees the update" T-LABEL
   99 NCH-CELL !
   4000 NCH-BUMP 4001 T=
   NCH-CELL @ 4001 T= ;

: SCAN-CASE ( -- )
   s" byte scans handle full, empty, singleton and absent inputs" T-LABEL
   NCH-BUF 4 NCH-BSUM 416 T=
   NCH-BUF 0 NCH-BSUM 0 T=
   NCH-BUF 4 98 NCH-BFIND 2 T=
   NCH-BUF 4 122 NCH-BFIND -1 T=
   NCH-BUF2 5 97 NCH-COUNT-CHAR 3 T=
   NCH-BUF2 0 97 NCH-COUNT-CHAR 0 T=
   NCH-BUF2 1 97 NCH-COUNT-CHAR 1 T=
   NCH-BUF2 5 122 NCH-COUNT-CHAR 0 T= ;

: BRANCH-CASE ( -- )
   s" branches choose the right arm and retain carried values" T-LABEL
   3 4 NCH-MAX 4 T= 9 -1 NCH-MAX 9 T=
   3 7 NCH-MAX-DIM 7 T= 7 3 NCH-MAX-DIM 7 T=
   5 5 NCH-MAX-DIM 5 T= -4 -9 NCH-MAX-DIM -4 T=
   9 2 NCH-SUMMAX 11 T= 3 7 NCH-SUMMAX 14 T=
   9 2 NCH-TWOIF 9 T= 3 7 NCH-TWOIF 7 T= -8 -3 NCH-TWOIF 3 T=
   -3 NCH-ELSEXIT 0 T= 5 NCH-ELSEXIT 7 T=
   \ XOR checks the full canonical true mask, not just a nonzero result.
   3 4 NCH-ISLT true xor TFALSE 9 -1 NCH-ISLT TFALSE
   3 4 NCH-LTKEEP true xor TFALSE 9 -1 NCH-LTKEEP TFALSE ;

: LOOP-CASE ( -- )
   s" consecutive and nested loops preserve their carried values" T-LABEL
   5 NCH-WDOWN 0 T= 0 NCH-WDOWN 0 T=
   5 NCH-TWOLOOP 1 T= -2 NCH-TWOLOOP 0 T=
   3 NCH-NESTW 0 T= -2 NCH-NESTW -2 T= ;

: RECURSION-CASE ( -- )
   s" recursive calls preserve live values and return through quotations" T-LABEL
   10 NCH-FACT 3628800 T= 1 NCH-FACT 1 T=
   0 NCH-QWALK 100 T= 3 NCH-QWALK 400 T=
   0 NCH-QWIDE 0 T= 0 T= 2 NCH-QWIDE 0 T= 20 T=
   2 3 4 NCH-SPILL 19 T=
   1 NCH-RSPILL 18 T= 3 NCH-RSPILL 66 T=
   s" recursive arithmetic with high register pressure preserves its frame" T-LABEL
   1 NCH-PRESSURE 434 T= 3 NCH-PRESSURE 1386 T= ;

: DROP-ROUTINE ( NEFF:routine -- )
   NEFF-ROUTINE:UNMAKE
   drop drop drop drop drop drop drop drop drop drop drop drop drop drop ;

: POOL-CASES ( -- )
   \ A run of registers is a value and names no machine, so a pool crossing the
   \ engine's own registers is built and then refused by the contract that would
   \ have the routine hold state in them - which is still before any selection.
   s" a pool naming an engine register is refused before selection" T-LABEL
   [: 20 4 NABI:POOL 1 1 0 NABI:LEAF-FRAMED DROP-ROUTINE ;] E-NEFF-GPR TTHROWSQ
   [: 24 4 NABI:POOL 1 1 0 NABI:LEAF-FRAMED DROP-ROUTINE ;] E-NEFF-GPR TTHROWSQ ;

: BINDING-CASE ( -- )
   s" the native binding names the host ABI" T-LABEL
   HB-TARGET-LINUX? if
      NABI:BINDING CBIND:TARGET@ CTARGET:ABI@
         CTARGET-ABI:AAPCS64-LINUX CTARGET-ABI:EQ TTRUE
      exit
   then
   HB-TARGET-MACOS? if
      NABI:BINDING CBIND:TARGET@ CTARGET:ABI@
         CTARGET-ABI:AAPCS64-DARWIN CTARGET-ABI:EQ TTRUE
      exit
   then
   E-CTGT-ABI throw ;

: NR0 ( -- NEFF:routine )   0 4 NABI:POOL 1 0 0 NABI:NORET-FRAMED ;
: CF0 ( -- NEFF:routine )   0 4 NABI:POOL 1 0 0 NABI:CALL-FRAMED ;
: NR4 ( -- NEFF:routine )   0 4 NABI:POOL 1 0 4 NABI:NORET-FRAMED ;
: CF4 ( -- NEFF:routine )   0 4 NABI:POOL 1 0 4 NABI:CALL-FRAMED ;

: R-LINK ( NEFF:control NEFF:link -- NEFF:routine )
   {: c:NEFF:control l:NEFF:link :}
   NEFF-CONV:DSTACK NEFF:SEQ-NONE NEFF:SEQ-NONE NEFF:GPR-NONE
   NEFF:FPR-NONE NEFF:FPR-NONE NEFF:FPR-NONE
   NEFF-NZCV:UNTOUCHED l c
   NEFF:T-CALL 0 0 A64M:MACHINE NEFF:ROUTINE ;

: NORET-TRAIT-CASE ( -- )
   s" both forms declare the direct call, because both really call" T-LABEL
   NR0 NEFF:TRAITS@ NEFF:T-CALL NEFF:TRAITS-HAS? TTRUE
   CF0 NEFF:TRAITS@ NEFF:T-CALL NEFF:TRAITS-HAS? TTRUE ;

: NORET-FIELD-CASE ( -- )
   s" the calling form keeps the caller's return address and a slot for it"
   T-LABEL
   CF0 NEFF:LINK@ NEFF-LINK:PRESERVED NEFF-LINK:EQ TTRUE
   CF0 NEFF:FRAME@ A64M:SP-ALIGN T=
   CF0 NEFF:CONTROL@ NEFF-CONTROL:RETURNS NEFF-CONTROL:EQ TTRUE
   CF0 NEFF:RETURNS? TTRUE

   s" the no-return form declares it destroyed and owns no frame at all" T-LABEL
   NR0 NEFF:LINK@ NEFF-LINK:CLOBBERED NEFF-LINK:EQ TTRUE
   NR0 NEFF:FRAME@ 0 T=
   NR0 NEFF:DELTA@ 0 T=
   NR0 NEFF:CONTROL@ NEFF-CONTROL:NO-RETURN NEFF-CONTROL:EQ TTRUE
   NR0 NEFF:RETURNS? TFALSE ;

: NORET-LAYOUT-CASE ( -- )
   s" the layout keeps a slot for the calling form and none for the other"
   T-LABEL
   NR0 NEFF:TRAITS@ NR0 NEFF:LINK@ A64FRAME:LINK-KEPT? TFALSE
   CF0 NEFF:TRAITS@ CF0 NEFF:LINK@ A64FRAME:LINK-KEPT? TTRUE
   NR0 NEFF:TRAITS@ NR0 NEFF:LINK@ A64FRAME:SPILL-BASE 0 T=
   CF0 NEFF:TRAITS@ CF0 NEFF:LINK@ A64FRAME:SPILL-BASE
      A64FRAME:LINK-SLOT A64IR:SLOT-WIDTH + T= ;

: LINK-SPLIT-CASE ( -- )
   s" a routine control comes back from cannot declare the address destroyed"
   T-LABEL
   [: NEFF-CONTROL:RETURNS NEFF-LINK:CLOBBERED R-LINK DROP-ROUTINE ;]
      E-NEFF-LINK TTHROWSQ
   [: NEFF-CONTROL:TAIL-CALL NEFF-LINK:CLOBBERED R-LINK DROP-ROUTINE ;]
      E-NEFF-LINK TTHROWSQ

   s" so a calling routine that returns always keeps a slot for it" T-LABEL
   NEFF-CONTROL:RETURNS NEFF-LINK:PRESERVED R-LINK
      NEFF:TRAITS@ NEFF-LINK:PRESERVED A64FRAME:LINK-KEPT? TTRUE
   NEFF-CONTROL:TAIL-CALL NEFF-LINK:PRESERVED R-LINK
      NEFF:TRAITS@ NEFF-LINK:PRESERVED A64FRAME:LINK-KEPT? TTRUE

   s" and only one that never comes back may say otherwise" T-LABEL
   NEFF-CONTROL:NO-RETURN NEFF-LINK:CLOBBERED R-LINK
      NEFF:TRAITS@ NEFF-LINK:CLOBBERED A64FRAME:LINK-KEPT? TFALSE ;

: NORET-SPILL-CASE ( -- )
   s" four spill slots fit one form's frame and not the other's" T-LABEL
   NR4 NEFF:FRAME@  4 A64IR:SLOT-WIDTH *  A64M:FRAME-ROUND  T=
   CF4 NEFF:FRAME@  5 A64IR:SLOT-WIDTH *  A64M:FRAME-ROUND  T=
   NR4 NEFF:FRAME@  CF4 NEFF:FRAME@  T<>

   s" and the pointer is declared where a routine that never returns leaves it"
   T-LABEL
   NR4 NEFF:DELTA@  NR4 NEFF:FRAME@ negate  T=
   CF4 NEFF:DELTA@ 0 T= ;
public

: RUN ( -- )
   T-RESET
   BUFFER-MAKE
   ARITHMETIC-CASE MEM-CASE SCAN-CASE BRANCH-CASE LOOP-CASE RECURSION-CASE
   BINDING-CASE POOL-CASES
   NORET-TRAIT-CASE NORET-FIELD-CASE NORET-LAYOUT-CASE
   LINK-SPLIT-CASE NORET-SPILL-CASE
   T-REPORT ;

;package

NCHAIN-TEST:RUN
