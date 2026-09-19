\ clobber-lint.f -- register-clobber analysis for BL-able emitter routines.
\ Self-hosted clobber lint. Run:
\   bin/hb --load tools/lint/text.f tools/lint/token.f tools/lint/lib.f tools/lint/clobber-lint.f

require lib/errors.f
require lib/string.f
require tools/lint/text.f
require tools/lint/token.f
require tools/lint/lib.f

package CLOBBER-CENSUS

\ Observed production floors include qualified labels and wrapped calls.
\ Lower counts need review; dropping package tails must not hide routines.
\ 399 -> 397 when 878e1026 retired BWAITRC (src/habu/habu1.f), whose LNX-OK
\ and LNX-DONE openings were two routines; its calls were not counted (no BL).
397 constant MIN-ROUTINES
625 constant MIN-CALLS

variable ROUTINE-N
variable CALL-N

public

: RESET ( -- )
   0 ROUTINE-N !
   0 CALL-N ! ;

: ROUTINE+ ( -- )
   ROUTINE-N @ 1+ ROUTINE-N ! ;

: CALL+ ( -- )
   CALL-N @ 1+ CALL-N ! ;

: COUNTS ( -- n n )
   ROUTINE-N @ CALL-N @ ;

: PROVE ( -- )
   ROUTINE-N @ MIN-ROUTINES < IF
      s" clobber-lint: routines=" type ROUTINE-N @ .
      s" clobber-lint: routine census below floor" 1 die
   THEN
   CALL-N @ MIN-CALLS < IF
      s" clobber-lint: calls=" type CALL-N @ .
      s" clobber-lint: call census below floor" 1 die
   THEN ;

: REPORT ( -- )
   s" clobber-lint: routines=" type ROUTINE-N @ .
   s" clobber-lint: calls=" type CALL-N @ . ;

;package

package CLOBBER

: NL  ( -- )  10 emit ;

\ Fail closed when a wrapped emitter call has no modeled contract or its
\ register operands cannot be resolved. This claims no reserved error range.
-4801 constant E-CLOBBER-WRAP-UNRESOLVED

\ ---- register sets --------------------------------------------------------
variable WMSK  variable RMSK
variable RX  variable RACC

: CL-BIT  ( n -- n )  1 swap lshift ;
: CL-HAS?  ( n n -- bool )  CL-BIT and 0 <> ;
: CL-ADD  ( n n -- n )  CL-BIT or ;
: CL-WOR  ( n -- )  WMSK @ or WMSK ! ;
: CL-ROR  ( n -- )  RMSK @ or RMSK ! ;

0 28 CL-ADD 31 CL-ADD constant CONTRACT-MASK
0 0 CL-ADD 2 CL-ADD 3 CL-ADD 4 CL-ADD 5 CL-ADD constant KWCMP-MASK
0 8 CL-ADD 16 CL-ADD constant SYS-SCRATCH-MASK

: CL-DIGIT?  ( n -- bool )  dup 47 > swap 58 < and ;
: CL-NUM-REG  ( ptr u8 n -- n ) {: a:ptr u :}
   u 0= if -1 exit then
   0 RACC !  0 RX !
   begin RX @ u < while
      a RX @ + c@ dup CL-DIGIT? 0= if drop -1 exit then
      48 -  RACC @ 10 * + RACC !
      RX @ 1+ RX !
   repeat
   RACC @ 32 < if RACC @ else -1 then ;
: REG-OF  ( ptr u8 n -- n ) {: a:ptr u :}
   a u s" XDS"   LINT-STR= if 19 exit then
   a u s" SP"    LINT-STR= if 31 exit then
   a u s" A"     LINT-STR= if 9 exit then
   a u s" B"     LINT-STR= if 10 exit then
   a u s" C"     LINT-STR= if 11 exit then
   a u s" XREG-RBASE" LINT-STR= if 20 exit then
   a u s" DBASE" LINT-STR= if 26 exit then
   a u s" NDICT" LINT-STR= if 27 exit then
   a u s" CP"    LINT-STR= if 28 exit then
   a u s" DATA"  LINT-STR= if 20 exit then
   a u CL-NUM-REG ;

\ ---- string/token helpers -------------------------------------------------
: START-L?  ( ptr u8 n -- bool ) {: a:ptr u :}
   0 u 0 ?do a i + c@ 58 = if drop i 1+ then loop {: off:n :}
   off u < if a off + c@ LINT-FOLD 108 = else LINT-FALSE then ;
: LABEL-ACCESS? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" LABEL@" LINT-STR=CI IF LINT-TRUE exit THEN
   u 7 > IF a u s" :LABEL@" LINT-SUFFIX? ELSE LINT-FALSE THEN ;
: ENDS-COMMA?  ( ptr u8 n -- bool ) {: a:ptr u :}
   u 0 > if a u 1- + c@ 44 = else LINT-FALSE then ;
: START-DOLLAR?  ( ptr u8 n -- bool ) {: a:ptr u :}
   u 0 > if a c@ 36 = else LINT-FALSE then ;
: LOWER-CHAR?  ( n -- bool )  dup 96 > swap 123 < and ;
: UPPERISH?  ( ptr u8 n -- bool ) {: a:ptr u :}
   0 RX !
   begin RX @ u < while
      a RX @ + c@ LOWER-CHAR? if LINT-FALSE exit then
      RX @ 1+ RX !
   repeat  LINT-TRUE ;
: STOP-MN?  ( ptr u8 n -- bool ) {: a:ptr u :}
   a u s" RET," LINT-STR= if LINT-TRUE exit then
   a u s" B," LINT-STR= ;

\ ---- modeled maps ---------------------------------------------------------
: RETURNS-MASK  ( ptr u8 n -- n ) {: a:ptr u :}
   a u s" WLFIND:LENTRY" LINT-STR=CI if 0 11 CL-ADD 12 CL-ADD exit then
   a u s" Lcfpop" LINT-STR=CI if 0 9 CL-ADD exit then
   a u s" Lkwcmp" LINT-STR=CI if 0 0 CL-ADD exit then
   a u s" Lloc-find" LINT-STR=CI if 0 0 CL-ADD exit then
   a u s" Ltok" LINT-STR=CI if 0 0 CL-ADD exit then
   a u s" Lsrcrd" LINT-STR=CI if 0 9 CL-ADD exit then
   a u s" Lreplroute" LINT-STR=CI if 0 9 CL-ADD exit then
   a u s" Lfind" LINT-STR=CI if 0 11 CL-ADD 12 CL-ADD 13 CL-ADD exit then
   a u s" Lfindused" LINT-STR=CI if 0 11 CL-ADD 12 CL-ADD 13 CL-ADD exit then
   a u s" Lnum" LINT-STR=CI if 0 2 CL-ADD 11 CL-ADD 12 CL-ADD exit then
   a u s" Lvralloc" LINT-STR=CI if 0 14 CL-ADD exit then
   a u s" Lfralloc" LINT-STR=CI if 0 14 CL-ADD exit then
   a u s" Lvpushf" LINT-STR=CI if 0 exit then
   a u s" Lfforcek" LINT-STR=CI if 0 14 CL-ADD exit then
   a u s" Lfbinprep" LINT-STR=CI if 0 13 CL-ADD 14 CL-ADD 15 CL-ADD exit then
   a u s" Lvbit" LINT-STR=CI if 0 8 CL-ADD exit then
   a u s" Lvforcek" LINT-STR=CI if 0 14 CL-ADD exit then
   a u s" Lvtop2c" LINT-STR=CI if 0 11 CL-ADD 12 CL-ADD 13 CL-ADD exit then
   a u s" Lvbinprep" LINT-STR=CI if 0 11 CL-ADD 12 CL-ADD 13 CL-ADD 14 CL-ADD 15 CL-ADD exit then
   a u s" Lvdrop" LINT-STR=CI if 0 13 CL-ADD exit then
   a u s" Lvswapx" LINT-STR=CI if 0 13 CL-ADD exit then
   a u s" Lvnipx" LINT-STR=CI if 0 13 CL-ADD exit then
   a u s" Lvcopy" LINT-STR=CI if 0 13 CL-ADD exit then
   a u s" Lp2cwat" LINT-STR=CI if 0 10 CL-ADD 11 CL-ADD exit then
   0 ;
: PRESERVE-MASK  ( ptr u8 n -- n ) {: a:ptr u :}
   \ The relocation helpers touch syscall scratch only in fatal write/exit
   \ arms; neither x8 nor x16 is written on a returning path.
   a u s" SNAP-RELOC:LCALLS" LINT-STR=CI
   a u s" SNAP-RELOC:LADDRS" LINT-STR=CI or if SYS-SCRATCH-MASK exit then
   \ Every registrar entry saves/restores x0..x17 around its emitted helpers.
   a u s" SNAP-RELOC:Lmark" LINT-STR=CI if $3FFFF exit then
   a u s" SNAP-RELOC:Lptrmark" LINT-STR=CI if $3FFFF exit then
   a u s" SNAP-RELOC:Lindexrelease" LINT-STR=CI if $3FFFF exit then
   \ Recovery uses the same MARK-SAVE/RESTORE register frame as the registrars.
   a u s" SNAP-RELOC:Lrollback" LINT-STR=CI if $3FFFF exit then
   a u s" Lvpushc" LINT-STR=CI if 0 11 CL-ADD exit then
   a u s" Lvpushr" LINT-STR=CI if 0 14 CL-ADD exit then
   a u s" Lvforcek" LINT-STR=CI if 0 5 CL-ADD exit then
   a u s" Lfforcek" LINT-STR=CI if 0 5 CL-ADD exit then
   a u s" Lvpushf" LINT-STR=CI if 0 11 CL-ADD exit then
   a u s" Lvbit" LINT-STR=CI if 0 7 CL-ADD exit then
   a u s" Lbcap" LINT-STR=CI if 0 0 CL-ADD 1 CL-ADD 2 CL-ADD 16 CL-ADD exit then
   a u s" Lbcs" LINT-STR=CI if 0 0 CL-ADD 1 CL-ADD 2 CL-ADD 16 CL-ADD exit then
   \ EMIT-CEMIT frames x1/x12/x13/x30; EMIT-PROT-GROW frames the rest.
   a u s" Lcemit" LINT-STR=CI if
      0 0 CL-ADD 1 CL-ADD 2 CL-ADD 8 CL-ADD 12 CL-ADD 13 CL-ADD
        16 CL-ADD 30 CL-ADD exit then
   a u s" PROT:LGROW" LINT-STR=CI if
      0 0 CL-ADD 2 CL-ADD 8 CL-ADD 16 CL-ADD 30 CL-ADD exit then
   a u s" Laotwidgate" LINT-STR=CI if 0 11 CL-ADD exit then
   a u s" Lprotwidq" LINT-STR=CI if 0 5 CL-ADD 6 CL-ADD 7 CL-ADD 14 CL-ADD exit then
   a u s" Lhidxadd" LINT-STR=CI if
      0 2 CL-ADD 3 CL-ADD 4 CL-ADD 5 CL-ADD 6 CL-ADD 7 CL-ADD 8 CL-ADD
        14 CL-ADD 15 CL-ADD 16 CL-ADD 17 CL-ADD exit
   then
   0 ;

\ ---- wrapped emitter calls ------------------------------------------------
\ A wrapped emitter call is an UPPER-CASE `PKG:CALL` word that expands, at emit
\ time, to a register-move prelude plus a branch-with-link to a shared engine
\ helper. It is neither a bare mnemonic nor a `LABEL@ BL,` triple, so the plain
\ scan would miss its emitted clobbers. PROT-GUARD:CALL (src/habu/habu1.f)
\ moves the caller's (addr,len) register
\ pair into the x10/x11 ABI the resident LPROTSPAN span guard reads, then
\ branches to it. The guard body reads x10/x11 and touches only x12/x13, so the
\ addr/len registers survive as x10=addr, x11=len; the branch additionally
\ clobbers x30. Any other `:CALL` shape, or a call whose operands do not resolve
\ to registers, fails closed via E-CLOBBER-WRAP-UNRESOLVED. C-FIND-GLOBAL is
\ also modeled: its literal label/count arguments feed LFIND and its returned
\ registers follow that helper's ABI.
public
EXPORT CL-ADD
EXPORT E-CLOBBER-WRAP-UNRESOLVED
;package

package CLOBBER-WRAP
using CLOBBER

0 12 CL-ADD 13 CL-ADD constant GUARD-BODY   \ registers the LPROTSPAN body clobbers
0 10 CL-ADD 11 CL-ADD constant GUARD-ABI    \ x10=addr, x11=len on return

: PROT?  ( ptr u8 n -- bool )  s" PROT-GUARD:CALL" LINT-STR= ;

public

: GLOBAL-FIND? ( ptr u8 n -- bool ) s" C-FIND-GLOBAL" LINT-STR= ;

: WRAP?  ( ptr u8 n -- bool ) {: a:ptr u:n :}   \ shape of a wrapped emitter call
   a u GLOBAL-FIND? if LINT-TRUE exit then
   u 6 < if LINT-FALSE exit then
   a u s" :CALL" LINT-SUFFIX? ;

: MASK  ( ptr u8 n n n -- n ) {: a:ptr u:n addr:n len:n :}   \ clobbered registers
   \ C-FIND-GLOBAL loads a literal name, calls LFIND, then restores package
   \ state through x14. Its successful path preserves x0/x1 and clobbers
   \ x2..x17 and x30. A missing name exits the process.
   a u GLOBAL-FIND? if $3FFFC 30 CL-ADD exit then
   a u PROT? if
      GUARD-BODY
      addr 10 <> if 10 CL-ADD then
      len 11 <> if 11 CL-ADD then
      exit
   then
   E-CLOBBER-WRAP-UNRESOLVED throw ;

: READS  ( ptr u8 n n n -- n ) {: a:ptr u:n addr:n len:n :}   \ input registers read
   a u GLOBAL-FIND? if 0 exit then
   a u PROT? if 0 addr CL-ADD len CL-ADD exit then
   E-CLOBBER-WRAP-UNRESOLVED throw ;

: RETURNS  ( ptr u8 n n n -- n ) {: a:ptr u:n addr:n len:n :}   \ registers the call redefines
   a u GLOBAL-FIND? if 0 5 CL-ADD 11 CL-ADD 12 CL-ADD 13 CL-ADD exit then
   a u PROT? if GUARD-ABI exit then
   E-CLOBBER-WRAP-UNRESOLVED throw ;

;using
;package

package CLOBBER

: PSEUDO?  ( ptr u8 n -- bool ) {: a:ptr u :}
   a u s" prot:reserve" LINT-STR=CI if LINT-TRUE exit then
   a u s" mark-save" LINT-STR=CI if LINT-TRUE exit then
   a u s" mark-restore" LINT-STR=CI if LINT-TRUE exit then
   a u s" mark-header" LINT-STR=CI if LINT-TRUE exit then
   a u s" mark-lock" LINT-STR=CI if LINT-TRUE exit then
   a u s" mark-unlock" LINT-STR=CI if LINT-TRUE exit then
   a u s" mark-rows" LINT-STR=CI if LINT-TRUE exit then
   a u s" index-shape" LINT-STR=CI if LINT-TRUE exit then
   a u s" index-probe" LINT-STR=CI if LINT-TRUE exit then
   a u s" index-build" LINT-STR=CI if LINT-TRUE exit then
   a u s" mark-commit-index" LINT-STR=CI if LINT-TRUE exit then
   a u s" g-push" LINT-STR=CI if LINT-TRUE exit then
   a u s" g-pop" LINT-STR=CI if LINT-TRUE exit then
   a u s" g-print9" LINT-STR=CI if LINT-TRUE exit then
   a u s" c-lit" LINT-STR=CI if LINT-TRUE exit then
   a u s" c-call" LINT-STR=CI if LINT-TRUE exit then
   a u s" c-popflag" LINT-STR=CI if LINT-TRUE exit then
   a u s" c-pushcp" LINT-STR=CI if LINT-TRUE exit then
   a u s" c-emitw" LINT-STR=CI if LINT-TRUE exit then
   a u s" c-bback" LINT-STR=CI if LINT-TRUE exit then
   a u s" cf-entry" LINT-STR=CI if LINT-TRUE exit then
   a u s" cfb-entry" LINT-STR=CI if LINT-TRUE exit then
   a u s" fold-entry" LINT-STR=CI if LINT-TRUE exit then
   a u s" vop-entry" LINT-STR=CI if LINT-TRUE exit then
   a u s" vcmp-entry" LINT-STR=CI if LINT-TRUE exit then
   a u s" vshuf-entry" LINT-STR=CI if LINT-TRUE exit then
   a u s" p2w-entry" LINT-STR=CI if LINT-TRUE exit then
   a u s" vun-entry" LINT-STR=CI ;
: INSTR?  ( ptr u8 n -- bool ) {: a:ptr u :}
   a u PSEUDO? if LINT-TRUE exit then
   a u ENDS-COMMA?  a u START-DOLLAR? 0= and  a u UPPERISH? and ;

: MN-W3?  ( ptr u8 n -- bool ) {: a:ptr u :}
   a u s" ADD," LINT-STR= if LINT-TRUE exit then  a u s" SUB," LINT-STR= if LINT-TRUE exit then
   a u s" MUL," LINT-STR= if LINT-TRUE exit then  a u s" AND," LINT-STR= if LINT-TRUE exit then
   a u s" ORR," LINT-STR= if LINT-TRUE exit then  a u s" EOR," LINT-STR= if LINT-TRUE exit then
   a u s" LSLV," LINT-STR= if LINT-TRUE exit then  a u s" LSRV," LINT-STR= if LINT-TRUE exit then
   a u s" SDIV," LINT-STR= ;
: MN-W2I?  ( ptr u8 n -- bool ) {: a:ptr u :}
   a u s" ADDI," LINT-STR= if LINT-TRUE exit then  a u s" SUBI," LINT-STR= if LINT-TRUE exit then
   a u s" LSLI," LINT-STR= if LINT-TRUE exit then  a u s" LSRI," LINT-STR= if LINT-TRUE exit then
   a u s" ASRI," LINT-STR= if LINT-TRUE exit then  a u s" ANDI," LINT-STR= ;
: MN-W1?  ( ptr u8 n -- bool ) {: a:ptr u :}
   a u s" MOVZ," LINT-STR= if LINT-TRUE exit then  a u s" MOVN," LINT-STR= if LINT-TRUE exit then
   a u s" ADR," LINT-STR= if LINT-TRUE exit then  a u s" LIT64," LINT-STR= if LINT-TRUE exit then
   a u s" CSET," LINT-STR= ;
: MN-WRMW?  ( ptr u8 n -- bool ) {: a:ptr u :}
   a u s" MOVK," LINT-STR= if LINT-TRUE exit then  a u s" MOVZHW," LINT-STR= if LINT-TRUE exit then
   a u s" MOVKHW," LINT-STR= if LINT-TRUE exit then  a u s" MOVNHW," LINT-STR= ;
: MN-LD?  ( ptr u8 n -- bool ) {: a:ptr u :}
   a u s" LDR," LINT-STR= if LINT-TRUE exit then  a u s" LDRB," LINT-STR= if LINT-TRUE exit then
   a u s" LDRW," LINT-STR= ;
: MN-ST?  ( ptr u8 n -- bool ) {: a:ptr u :}
   a u s" STR," LINT-STR= if LINT-TRUE exit then  a u s" STRB," LINT-STR= if LINT-TRUE exit then
   a u s" STRW," LINT-STR= ;

\ ---- register extraction/effects -----------------------------------------
16 constant RRMAX
create RRS RRMAX cells allot   variable RR#
variable RK

: RR-CHECK-ROOM ( -- )
   RR# @ RRMAX >= if s" clobber-lint: too many regs in instruction" 1 die then ;
: RR+  ( n -- )
   RR-CHECK-ROOM
   RRS RR# @ cells + !  RR# @ 1+ RR# ! ;
: RR@  ( n -- n )
   dup RR# @ < if RRS swap cells + @ else drop -1 then ;
: COLLECT-REGS  ( n n -- ) {: lo hi :}
   0 RR# !  lo RK !
   begin RK @ hi < while
      RK @ TOK REG-OF dup 0 >= if RR+ else drop then
      RK @ 1+ RK !
   repeat ;
: EW  ( n -- )  RR@ dup 0 >= if WMSK @ swap CL-ADD WMSK ! else drop then ;
: ER  ( n -- )  RR@ dup 0 >= if RMSK @ swap CL-ADD RMSK ! else drop then ;

: PSEUDO-EFFECTS  {: a u :}  ( -- )
   \ PROT:RESERVE reads the byte count in x1 and CP, preserves its LR, and
   \ calls LGROW, whose only unpreserved register is x1.
   a u s" prot:reserve" LINT-STR=CI if
      0 1 CL-ADD 28 CL-ADD CL-ROR 0 1 CL-ADD CL-WOR exit then
   \ These inline registrar helpers use a fixed machine-register ABI.
   a u s" mark-save" LINT-STR=CI if
      $3FFFF 31 CL-ADD CL-ROR 0 31 CL-ADD CL-WOR exit then
   a u s" mark-restore" LINT-STR=CI if
      0 31 CL-ADD CL-ROR $3FFFF 31 CL-ADD CL-WOR exit then
   \ SNAP-RELOC:MARK-HEADER materializes the fixed image base in x16 and
   \ its address-table header in x4. Neither depends on a caller register.
   a u s" mark-header" LINT-STR=CI if 0 4 CL-ADD 16 CL-ADD CL-WOR exit then
   a u s" mark-lock" LINT-STR=CI if
      0 16 CL-ADD CL-ROR 0 6 CL-ADD 7 CL-ADD 17 CL-ADD CL-WOR exit then
   a u s" mark-unlock" LINT-STR=CI if
      0 16 CL-ADD CL-ROR 0 6 CL-ADD 17 CL-ADD CL-WOR exit then
   a u s" mark-rows" LINT-STR=CI if
      0 4 CL-ADD 16 CL-ADD CL-ROR 0 5 CL-ADD 11 CL-ADD CL-WOR exit then
   a u s" index-shape" LINT-STR=CI if
      0 8 CL-ADD CL-ROR 0 6 CL-ADD 7 CL-ADD 10 CL-ADD CL-WOR exit then
   a u s" index-probe" LINT-STR=CI if
      0 5 CL-ADD 8 CL-ADD 10 CL-ADD 12 CL-ADD 13 CL-ADD CL-ROR
      0 6 CL-ADD 7 CL-ADD 9 CL-ADD 11 CL-ADD 14 CL-ADD 17 CL-ADD CL-WOR exit then
   a u s" index-build" LINT-STR=CI if
      0 8 CL-ADD 12 CL-ADD 13 CL-ADD 31 CL-ADD CL-ROR
      $1FFF 14 CL-ADD 16 CL-ADD 17 CL-ADD CL-WOR exit then
   a u s" mark-commit-index" LINT-STR=CI if
      0 4 CL-ADD 13 CL-ADD 16 CL-ADD 31 CL-ADD CL-ROR
      0 7 CL-ADD 8 CL-ADD 11 CL-ADD 13 CL-ADD CL-WOR exit then
   a u s" g-push" LINT-STR=CI if 0 ER 19 ER 19 EW exit then
   a u s" g-pop" LINT-STR=CI if 0 EW 19 ER 19 EW exit then
   a u s" g-print9" LINT-STR=CI if 0 0 CL-ADD 1 CL-ADD 2 CL-ADD 16 CL-ADD CL-WOR  0 9 CL-ADD CL-ROR exit then
   a u s" c-lit" LINT-STR=CI if 0 5 CL-ADD 6 CL-ADD 7 CL-ADD 8 CL-ADD 9 CL-ADD 30 CL-ADD CL-WOR  0 11 CL-ADD CL-ROR exit then
   a u s" c-call" LINT-STR=CI if 0 5 CL-ADD 7 CL-ADD 8 CL-ADD 9 CL-ADD 10 CL-ADD 13 CL-ADD 14 CL-ADD 15 CL-ADD 30 CL-ADD CL-WOR  0 11 CL-ADD 12 CL-ADD CL-ROR exit then
   a u s" c-popflag" LINT-STR=CI if 0 9 CL-ADD 19 CL-ADD CL-WOR exit then
   a u s" c-pushcp" LINT-STR=CI if 0 9 CL-ADD 30 CL-ADD CL-WOR exit then
   a u s" c-emitw" LINT-STR=CI if 0 9 CL-ADD 30 CL-ADD CL-WOR exit then
   a u s" c-bback" LINT-STR=CI if 0 5 CL-ADD 9 CL-ADD 10 CL-ADD 30 CL-ADD CL-WOR  0 9 CL-ADD CL-ROR exit then
   a u s" cf-entry" LINT-STR=CI if KWCMP-MASK CL-WOR exit then
   a u s" cfb-entry" LINT-STR=CI if KWCMP-MASK CL-WOR exit then
   a u s" fold-entry" LINT-STR=CI if KWCMP-MASK CL-WOR exit then
   a u s" vop-entry" LINT-STR=CI if KWCMP-MASK CL-WOR exit then
   a u s" vcmp-entry" LINT-STR=CI if KWCMP-MASK CL-WOR exit then
   a u s" vshuf-entry" LINT-STR=CI if KWCMP-MASK CL-WOR exit then
   a u s" p2w-entry" LINT-STR=CI if KWCMP-MASK CL-WOR exit then
   a u s" vun-entry" LINT-STR=CI if KWCMP-MASK CL-WOR exit then ;

: SYS-EXIT-NAME? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" NR-EXIT" LINT-STR= if LINT-TRUE exit then
   a u s" NR-EXIT-GROUP" LINT-STR= ;

: SYS-EXIT?  ( n n -- bool ) {: lo hi :}
   hi lo <= if LINT-FALSE exit then
   hi 1- TOK SYS-EXIT-NAME? ;
: SYS?  ( ptr u8 n -- bool )
   s" SYS," LINT-STR= ;
: BLR?  ( ptr u8 n -- bool )
   s" BLR," LINT-STR= ;

: EFFECTS  {: a u lo hi :}  ( -- )
   0 WMSK !  0 RMSK !  lo hi COLLECT-REGS
   a u MN-W3? if 0 EW 1 ER 2 ER exit then
   a u MN-W2I? if 0 EW 1 ER exit then
   a u MN-W1? if 0 EW exit then
   a u MN-WRMW? if RR# @ 0 > if 0 EW 0 ER then exit then
   a u MN-LD? if 0 EW 1 ER exit then
   a u MN-ST? if 0 ER 1 ER exit then
   a u s" CMP," LINT-STR= if 0 ER 1 ER exit then
   a u s" CMPI," LINT-STR= if 0 ER exit then
   a u s" CBZ," LINT-STR= if 0 ER exit then
   a u s" CBNZ," LINT-STR= if 0 ER exit then
   a u s" SVC," LINT-STR= if 0 0 CL-ADD CL-WOR  0 0 CL-ADD 1 CL-ADD 2 CL-ADD 16 CL-ADD CL-ROR exit then
   a u s" SYS," LINT-STR= if
      lo hi SYS-EXIT? if 0 0 CL-ADD 8 CL-ADD 16 CL-ADD CL-WOR  0 0 CL-ADD CL-ROR exit then
      0 0 CL-ADD 8 CL-ADD 16 CL-ADD CL-WOR  0 0 CL-ADD 1 CL-ADD 2 CL-ADD CL-ROR exit
   then
   a u s" RET," LINT-STR= if 0 30 CL-ADD CL-ROR exit then
   a u s" BLR," LINT-STR= if 1 18 lshift 1 -  30 CL-ADD CL-WOR  0 ER exit then
   a u PSEUDO? if a u PSEUDO-EFFECTS then ;

\ ---- clobber table + BL graph --------------------------------------------
$800 constant CMAX      \ callable labels plus discovered call targets
$2000 constant EMAX     \ unique BL graph edges
$10000 constant CNBUF-CAP
create CNBUF CNBUF-CAP allot   variable CEND
\ Each collected name's start is an address into the name buffer, so the starts
\ are declared pointer storage; the lengths beside them stay plain cells.
CMAX TYPED-BUFFER CNOFF ptr u8
create CNLEN CMAX cells allot
create CWS CMAX cells allot     variable CN#
create EFROM EMAX cells allot   create ETO EMAX cells allot   variable EN#
variable CX  variable EX

: C-NAME  ( n -- ptr u8 n )
   dup CNOFF @  swap CNLEN swap cells + @ ;
: CWS@  ( n -- n )  CWS swap cells + @ ;
: CWS!  ( n n -- )  CWS swap cells + ! ;
: C-WOR  ( n n -- ) {: idx m :}  idx CWS@ m or idx CWS! ;
: C-FIND  ( ptr u8 n -- n ) {: a:ptr u :}
   0 CX !
   begin CX @ CN# @ < while
      CX @ C-NAME a u LINT-STR=CI if CX @ exit then
      CX @ 1+ CX !
   repeat  -1 ;
: C-ADD  {: a u :}  ( -- idx )
   CN# @ CMAX >= if s" clobber-lint: too many labels" 1 die then
   CEND @ u + CNBUF-CAP > if s" clobber-lint: label store full" 1 die then
   a u CNBUF CEND @ + FOLD-TO
   CNBUF CEND @ +  CN# @ CNOFF !  u CNLEN CN# @ cells + !
   0 CWS CN# @ cells + !
   CEND @ u + CEND !  CN# @ dup 1+ CN# ! ;
: C-ENSURE  {: a u :}  ( -- idx )
   a u C-FIND dup 0 >= if exit then
   drop a u C-ADD ;

PTR-VARIABLE PKG-A
variable PKG-U

\ The unused tail of the name pool is scratch until C-ADD commits a name.
: C-QUALIFIED$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   PKG-U @ 0= a u s" :" CONTAINS? or if a u exit then
   PKG-U @ u + 1+ {: size:n :}
   size CNBUF-CAP CEND @ - > if s" clobber-lint: label store full" 1 die then
   CNBUF CEND @ + {: dst:ptr :}
   PKG-A @ dst PKG-U @ LINT-BMOVE
   58 dst PKG-U @ + c!
   a dst PKG-U @ 1+ + u LINT-BMOVE
   dst size ;

\ Bare labels prefer declarations in their current package, then globals.
\ Explicitly qualified labels already name their owner.
: C-LABEL$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   a u C-QUALIFIED$ C-FIND {: idx:n :}
   idx 0 >= if idx C-NAME else a u then ;
: EDGE?  ( n n -- bool ) {: from to :}
   0 EX !
   begin EX @ EN# @ < while
      EX @ cells EFROM + @ from =  EX @ cells ETO + @ to = and if LINT-TRUE exit then
      EX @ 1+ EX !
   repeat  LINT-FALSE ;
: EDGE+  {: from to :}  ( -- )
   from to EDGE? if exit then
   EN# @ EMAX >= if s" clobber-lint: too many BL edges" 1 die then
   from EFROM EN# @ cells + !  to ETO EN# @ cells + !
   EN# @ 1+ EN# ! ;

\ ---- definitions, labels, and routine regions ----------------------------
$80 constant OMAX
create OPENINGS OMAX cells allot   variable ON#
TYPED-VARIABLE CALA ptr u8   \ start of the callee name CALLEE? resolved
variable DI  variable OX  variable OPLO  variable CALU
variable RNEXT  variable LASTSTOP  variable RDONE  variable CUR

\ The current wrapped call sits at token DI; its two register operands are the
\ immediately preceding tokens (`<addr> <len> PKG:CALL`), inside the operand run
\ starting at OPLO. Resolve them, failing closed if they are missing or are not
\ registers, so the modeled contract never runs on an operand it cannot read.
: WRAP-REGS  ( -- n n )   \ addr len
   \ The global lookup's arguments are an emitter label and byte count, not
   \ target registers. No caller register is read to construct its arguments.
   DI @ TOK CLOBBER-WRAP:GLOBAL-FIND? if 0 0 exit then
   DI @ OPLO @ - 2 < if E-CLOBBER-WRAP-UNRESOLVED throw then
   DI @ 2 - TOK REG-OF  DI @ 1 - TOK REG-OF
   2dup 0 < swap 0 < or if E-CLOBBER-WRAP-UNRESOLVED throw then ;
: WRAP-MASK  ( -- n )   \ registers the wrapped call at DI clobbers
   WRAP-REGS  DI @ TOK 2swap CLOBBER-WRAP:MASK ;
: WRAP-READS  ( -- n )  \ registers the wrapped call at DI reads
   WRAP-REGS  DI @ TOK 2swap CLOBBER-WRAP:READS ;
: WRAP-RETURNS ( -- n ) \ register-valued outputs are resolved from the same operands
   WRAP-REGS  DI @ TOK 2swap CLOBBER-WRAP:RETURNS ;

: DEF-END  {: lo :}  ( -- hi )
   lo 2 + DI !
   begin DI @ TN# @ < while
      DI @ TOK s" ;" LINT-STR= if DI @ exit then
      DI @ 1+ DI !
   repeat  DI @ ;

: SCOPE-STEP ( n -- n ) {: k:n :}
   k TOK s" ;package" LINT-STR=CI if 0 PKG-U ! k exit then
   k TOK s" package" LINT-STR=CI 0= if k exit then
   k 1+ TN# @ >= if s" clobber-lint: missing package name" 1 die then
   k 1+ TOK PKG-U ! PKG-A ! k 1+ ;

\ Declarations precede body analysis, including labels owned by another file.
\ Colon bodies are skipped so emitter operands cannot declare a label.
: DECL-FILE ( ptr u8 n -- )
   LINT-SOURCE:LOAD LINT-SOURCE:TEXT TOKENIZE 0 PKG-U !
   0 begin dup TN# @ < while
      {: k:n :}
      k TOK s" :" LINT-STR= if
         k DEF-END 1+
      else
         k TOK s" variable" LINT-STR=CI k 1+ TN# @ < and if
            k 1+ TOK START-L? if k 1+ TOK C-QUALIFIED$ C-ENSURE drop then
         then
         k SCOPE-STEP 1+
      then
   repeat drop ;
: OPEN@  ( n -- n )  OPENINGS swap cells + @ ;
: OPEN+  ( n -- )
   ON# @ OMAX >= if s" clobber-lint: too many labels in definition" 1 die then
   OPENINGS ON# @ cells + !  ON# @ 1+ ON# ! ;
: LABEL-OPEN? ( n n -- bool ) {: k:n hi:n :}
   k 2 + hi >= if LINT-FALSE exit then
   k TOK START-L?  k 1+ TOK LABEL-ACCESS? and  k 2 + TOK s" LBL," LINT-STR= and ;
: BL-CALL-SITE? ( n n -- bool ) {: k:n hi:n :}
   k 2 + hi >= IF LINT-FALSE exit THEN
   k TOK START-L?
   k 1+ TOK LABEL-ACCESS? and
   k 2 + TOK s" BL," LINT-STR= and ;
: COLLECT-OPENINGS ( n n -- ) {: lo:n hi:n :}
   0 ON# !  lo OX !
   begin OX @ hi < while
      OX @ hi LABEL-OPEN? if OX @ OPEN+ CLOBBER-CENSUS:ROUTINE+ then
      OX @ hi BL-CALL-SITE? if CLOBBER-CENSUS:CALL+ then
      OX @ TOK CLOBBER-WRAP:WRAP? if CLOBBER-CENSUS:CALL+ then
      OX @ 1+ OX !
   repeat ;
: CALLEE? ( n n -- bool ) {: lo:n hi:n :}
   hi lo - 2 < if LINT-FALSE exit then
   hi 1- TOK LABEL-ACCESS? 0= if LINT-FALSE exit then
   hi 2 - TOK START-L? 0= if LINT-FALSE exit then
   hi 2 - TOK C-LABEL$ CALU ! CALA ! LINT-TRUE ;

: ROUTINE-INIT ( n -- ) {: oi :}
   oi OPEN@ 3 + DI !
   oi 1+ RNEXT !
   0 LASTSTOP !
   0 RDONE !
   DI @ OPLO ! ;

: ROUTINE-NEXT-OPEN? ( -- bool )
   RNEXT @ ON# @ <  DI @ RNEXT @ OPEN@ = and ;

: ROUTINE-ADVANCE-OPEN ( -- )
   LASTSTOP @ if
      -1 RDONE !
   else
      RNEXT @ 1+ RNEXT !
      DI @ 3 + DI !
      DI @ OPLO !
   then ;

: ROUTINE-CALL? ( -- bool )
   OPLO @ DI @ CALLEE?
   DI @ TOK s" BL," LINT-STR= and ;

: ROUTINE-ADD-EDGE ( n -- ) {: cidx :}
   CALA @ CALU @ C-ENSURE cidx swap EDGE+ ;

: ROUTINE-ADD-EFFECTS ( n -- ) {: cidx :}
   DI @ TOK OPLO @ DI @ EFFECTS
   cidx WMSK @ C-WOR ;

: ROUTINE-INSTR ( n -- ) {: cidx :}
   ROUTINE-CALL? if
      cidx ROUTINE-ADD-EDGE
   else
      cidx ROUTINE-ADD-EFFECTS
   then
   DI @ TOK STOP-MN? LASTSTOP !
   DI @ 1+ OPLO ! ;

\ A routine that wraps a guard call gains that call's clobbers directly, so the
\ transitive closure carries them into every caller of this routine.
: ROUTINE-WRAP ( n -- ) {: cidx:n :}
   cidx WRAP-MASK C-WOR
   DI @ 1+ OPLO ! ;

: ROUTINE-STEP ( n -- ) {: cidx :}
   ROUTINE-NEXT-OPEN? if
      ROUTINE-ADVANCE-OPEN
   else
      DI @ TOK CLOBBER-WRAP:WRAP? if
         cidx ROUTINE-WRAP
      else
         DI @ TOK INSTR? if cidx ROUTINE-INSTR then
      then
      DI @ 1+ DI !
   then ;

: ROUTINE-SCAN ( n n n -- ) {: cidx oi hi :}
   oi ROUTINE-INIT
   begin DI @ hi < RDONE @ 0= and while
      cidx ROUTINE-STEP
   repeat ;
: PASS1-DEF  {: lo hi :}  ( -- )
   lo hi COLLECT-OPENINGS
   0 OX !
   begin OX @ ON# @ < while
      OX @ OPEN@ TOK C-LABEL$ C-ENSURE CUR !
      CUR @ OX @ hi ROUTINE-SCAN
      OX @ 1+ OX !
   repeat ;

variable WI  variable WE
: PASS1-FILE ( ptr u8 n -- ) {: pa:ptr pu:n :}
   pa pu LINT-SOURCE:LOAD  LINT-SOURCE:TEXT TOKENIZE
   0 PKG-U ! 0 WI !
   begin WI @ TN# @ 1- < while
      WI @ TOK s" :" LINT-STR= if
         WI @ DEF-END WE !
         WI @ 2 + WE @ PASS1-DEF
         WE @ WI !
      else
         WI @ SCOPE-STEP WI !
      then
      WI @ 1+ WI !
   repeat ;

\ ---- clobber closure ------------------------------------------------------
variable CHANGED  variable EFF
: CLOSE-CLOBBERS  ( -- )
   -1 CHANGED !
   begin CHANGED @ while
      0 CHANGED !  0 CX !
      begin CX @ CN# @ < while
         CX @ CWS@ EFF !
         0 EX !
         begin EX @ EN# @ < while
            EX @ cells EFROM + @ CX @ = if
               EFF @  EX @ cells ETO + @ CWS@  or EFF !
            then
            EX @ 1+ EX !
         repeat
         EFF @  CX @ C-NAME PRESERVE-MASK invert and EFF !
         EFF @ CX @ CWS@ <> if EFF @ CX @ CWS!  -1 CHANGED ! then
         CX @ 1+ CX !
      repeat
   repeat ;

\ ---- pass 2: call-site liveness ------------------------------------------
create POIS 32 cells allot   variable DIRTY
variable APPLY-MASK
create WNAME 128 allot       variable WLEN
create NUMBUF 2 allot
variable BAD  variable PR  variable CW  variable RETS  variable CALIDX
variable TRACK-LR

: POIS@ ( n -- n )  POIS swap cells + @ ;
: POIS! ( n n -- )  POIS swap cells + ! ;
: POIS-CLEAR  ( -- )
   0 PR !
   begin PR @ 32 < while -1 PR @ POIS!  PR @ 1+ PR ! repeat ;
: WORD-NAME!  ( n -- )
   TOK dup WLEN !  WNAME FOLD-TO ;
: CRASH-FILE?  ( ptr u8 n -- bool ) {: a:ptr u :}
   a u s" src/habu/crash.f" LINT-STR= ;
: ALLOW?  ( ptr u8 n n n -- bool ) {: fa:ptr fu reg cidx :}
   fa fu CRASH-FILE? 0= if LINT-FALSE exit then
   WNAME WLEN @ s" emit-crash-handler" LINT-STR=CI 0= if LINT-FALSE exit then
   reg 1 = reg 2 = or 0= if LINT-FALSE exit then
   cidx 0 < if LINT-FALSE exit then
   cidx C-NAME s" lhex" LINT-STR=CI ;
: DEC-TYPE  ( n -- ) {: n :}
   n 10 < if 48 n + NUMBUF c!  NUMBUF 1 type
   else 48 n 10 / + NUMBUF c!  48 n 10 mod + NUMBUF 1+ c!  NUMBUF 2 type then ;
: REG-TYPE  ( n -- )  s" x" type DEC-TYPE ;
: FINDING  {: fa fu reg cidx :}  ( -- )
   s" CLOBBER " type fa fu type s"  " type
   WNAME WLEN @ type s" : " type reg REG-TYPE
   s"  written, clobbered by " type cidx C-NAME type
   s" , then read" type NL ;
: NOTE-READS  {: fa fu rmask :}  ( -- )
   0 PR !
   begin PR @ 32 < while
      rmask CONTRACT-MASK invert and PR @ CL-HAS? if
         PR @ 30 = TRACK-LR @ 0= and if
         else
         PR @ POIS@ CALIDX !
         CALIDX @ 0 >= if
            DIRTY @ PR @ CL-HAS? if
               fa fu PR @ CALIDX @ ALLOW? 0= if
                  fa fu PR @ CALIDX @ FINDING  BAD @ 1+ BAD !
               then
               -1 PR @ POIS!
            then
         then
         then
      then
      PR @ 1+ PR !
   repeat ;
: APPLY-WRITES  ( n -- )
   CONTRACT-MASK invert and APPLY-MASK !
   APPLY-MASK @ DIRTY @ or DIRTY !
   0 PR !
   begin PR @ 32 < while
      APPLY-MASK @ PR @ CL-HAS? if -1 PR @ POIS! then
      PR @ 1+ PR !
   repeat ;
: APPLY-RETURNS  ( n -- )
   APPLY-MASK !
   APPLY-MASK @ DIRTY @ or DIRTY !
   0 PR !
   begin PR @ 32 < while
      APPLY-MASK @ PR @ CL-HAS? if -1 PR @ POIS! then
      PR @ 1+ PR !
   repeat ;
: POISON-DIRTY  {: cmask cidx :}  ( -- )
   0 PR !
   begin PR @ 32 < while
      DIRTY @ PR @ CL-HAS?  cmask PR @ CL-HAS? and if
         cidx PR @ POIS!
      then
      PR @ 1+ PR !
   repeat ;
: POISON-SYS-SCRATCH  ( -- )
   SYS-SCRATCH-MASK s" sys" C-ENSURE POISON-DIRTY ;
: POISON-LINK-REGISTER  ( n -- )
   TRACK-LR @ 0= if drop exit then
   0 30 CL-ADD swap POISON-DIRTY ;
: RET-IN-RANGE?  {: lo hi :}  ( -- bool )
   lo DI !
   begin DI @ hi < while
      DI @ TOK s" RET," LINT-STR= if LINT-TRUE exit then
      DI @ 1+ DI !
   repeat
   LINT-FALSE ;

\ Resolved BL to a modeled callee (CALA/CALU were set by CALLEE?): the callee's
\ transitive clobbers, minus its returns and the contract-preserved registers,
\ poison any live caller value; its returns become freshly defined.
: APPLY-CALL  ( -- )
   CALA @ CALU @ RETURNS-MASK RETS !
   CALA @ CALU @ C-FIND CALIDX !
   CALIDX @ 0 >= if CALIDX @ CWS@ else 0 then
   CONTRACT-MASK invert and  RETS @ invert and CW !
   CW @ CALIDX @ POISON-DIRTY
   CALIDX @ POISON-LINK-REGISTER
   RETS @ APPLY-RETURNS ;

\ Modeled wrapped emitter call at token DI: its argument registers are read
\ first, then its move+guard clobbers poison any live value, its emitted branch
\ poisons the link register, and its ABI-return registers (x10/x11 for
\ PROT-GUARD) are redefined.
: APPLY-WRAP  ( ptr u8 n -- ) {: fa:ptr fu:n :}
   fa fu WRAP-READS NOTE-READS
   DI @ TOK C-ENSURE CALIDX !
   WRAP-MASK CALIDX @ POISON-DIRTY
   CALIDX @ POISON-LINK-REGISTER
   WRAP-RETURNS APPLY-RETURNS ;

: PASS2-ENTRY ( -- )
   TRACK-LR @ if 0 30 CL-ADD else 0 then DIRTY !
   POIS-CLEAR ;

\ A direct branch names its target last, optionally through LABEL@.
: TARGET-INDEX ( n -- n )
   1- dup TOK LABEL-ACCESS? if 1- then ;

: DIRECT-JUMP? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" B," LINT-STR= if true exit then
   a u s" BL," LINT-STR= if true exit then
   a u s" BCOND," LINT-STR= if true exit then
   a u s" CBZ," LINT-STR= if true exit then
   a u s" CBNZ," LINT-STR= if true exit then
   a u s" TBZ," LINT-STR= if true exit then
   a u s" TBNZ," LINT-STR= ;

: ENTRY-BETWEEN? ( n n n -- bool ) {: from:n dest:n hi:n :}
   from dest max 1+ from dest min 1+ ?do
      i hi LABEL-OPEN? if true unloop exit then
   loop false ;

\ A jump to a local join can skip a global entry, just like a qualified jump.
\ Keep whole-definition state whenever a reference crosses an entry boundary.
: JUMP-CROSSES? ( n n n -- bool ) {: ref:n lo:n hi:n :}
   ref TOK C-LABEL$ {: a:ptr u:n :}
   hi lo 1+ ?do
      i TOK s" LBL," LINT-STR= if
         i TARGET-INDEX {: dest:n :}
         dest lo >= if
            dest TOK C-LABEL$ a u LINT-STR=CI if
               ref dest hi ENTRY-BETWEEN? if true unloop exit then
            then
         then
      then
   loop false ;

: SEPARATE-ENTRIES? ( n n -- bool ) {: lo:n hi:n :}
   hi lo ?do
      i TOK s" BR," LINT-STR= i TOK s" BLR," LINT-STR= or if
         false unloop exit then
      i lo > i TOK DIRECT-JUMP? and if
         i TARGET-INDEX {: ref:n :}
         ref lo >= if
            ref lo hi JUMP-CROSSES? if false unloop exit then
         then
      then
   loop true ;

: PASS2-DEF  {: fa fu lo hi :}  ( -- )
   lo hi SEPARATE-ENTRIES? {: separate:bool :}
   lo hi RET-IN-RANGE? TRACK-LR !
   PASS2-ENTRY 0 LASTSTOP ! lo OPLO ! lo DI !
   begin DI @ hi < while
      \ Split only independent entries; fall-through always retains state.
      DI @ hi LABEL-OPEN? LASTSTOP @ and separate and if PASS2-ENTRY then
      DI @ TOK CLOBBER-WRAP:WRAP? if
         fa fu APPLY-WRAP
         0 LASTSTOP !
         DI @ 1+ OPLO !
      else
         DI @ TOK INSTR? if
            OPLO @ DI @ CALLEE?  DI @ TOK s" BL," LINT-STR= and if
               APPLY-CALL
            else
               DI @ TOK  OPLO @ DI @ EFFECTS
               fa fu RMSK @ NOTE-READS
               WMSK @ APPLY-WRITES
               DI @ TOK SYS? OPLO @ DI @ SYS-EXIT? 0= and if POISON-SYS-SCRATCH then
               DI @ TOK BLR? if s" blr" C-ENSURE POISON-LINK-REGISTER then
            then
            DI @ TOK STOP-MN? LASTSTOP !
            DI @ 1+ OPLO !
         then
      then
      DI @ 1+ DI !
   repeat ;
: PASS2-FILE ( ptr u8 n -- ) {: pa:ptr pu:n :}
   pa pu LINT-SOURCE:LOAD  LINT-SOURCE:TEXT TOKENIZE
   0 PKG-U ! 0 WI !
   begin WI @ TN# @ 1- < while
      WI @ TOK s" :" LINT-STR= if
         WI @ DEF-END WE !
         WI @ 1+ WORD-NAME!
         pa pu  WI @ 2 + WE @ PASS2-DEF
         WE @ WI !
      else
         WI @ SCOPE-STEP WI !
      then
      WI @ 1+ WI !
   repeat ;

\ ---- driver ---------------------------------------------------------------
: EACH-FILE ( [ ptr u8 n -- ] -- ) {: visit :}
   s" src/habu/habu1.f" visit execute  s" src/habu/habu2.f" visit execute
   s" src/habu/jit.f" visit execute  s" src/habu/regalloc.f" visit execute
   s" src/habu/prof.f" visit execute  s" src/habu/rt.f" visit execute
   s" src/habu/crash.f" visit execute ;
: ALL-PASS1 ( -- )
   0 CN# ! 0 CEND ! 0 EN# !
   [: DECL-FILE ;] EACH-FILE
   [: PASS1-FILE ;] EACH-FILE ;
: ALL-PASS2 ( -- )
   [: PASS2-FILE ;] EACH-FILE ;
: CLOBBER-LINT  ( -- )
   0 PARENS? !  CLOBBER-CENSUS:RESET  ALL-PASS1
   CLOBBER-CENSUS:PROVE  CLOSE-CLOBBERS  0 BAD !  ALL-PASS2
   CLOBBER-CENSUS:REPORT
   BAD @ 0 > if
      s" clobber-lint: " type BAD @ . s"  finding(s)" type NL
      s" clobber-lint: findings" 1 die
   else
      s" clobber-lint: clean" type NL
   then ;
CLOBBER-LINT
;package
