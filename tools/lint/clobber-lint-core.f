\ clobber-lint-core.f - register-clobber analysis for BL-able emitter routines.
\ Run: bin/hb --load tools/lint/clobber-lint.f

require src/lib/error.f
require src/lib/string.f
require tools/lint/text.f
require tools/lint/token.f
require tools/lint/lib.f
require src/lib/fmt.f                        \ FMT:.INT
require src/lib/map.f

package CLOBBER-CENSUS

\ The production floor, qualified labels and wrapped calls included. A lower
\ count needs review: dropping package tails must not hide routines.
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

\ ---- modeled maps ---------------------------------------------------------
: RETURNS-MASK  ( ptr u8 n -- n ) {: a:ptr u :}
   \ LCOUNT publishes the requested count in NDICT; APPEND and RESET use it.
   a u s" EM-DEF-OCC:LCOUNT" LINT-STR=CI
   a u s" EM-DEF-OCC:LAPPEND" LINT-STR=CI or
   a u s" EM-DEF-OCC:LRESET" LINT-STR=CI or if 0 27 CL-ADD exit then
   \ Both LMATCH return arms define the hit flag in x12.
   a u s" EM-DEF-OCC:LMATCH" LINT-STR=CI if 0 12 CL-ADD exit then
   \ LRETIRE answers in x14: nonzero when it retired instead of lowering.
   a u s" EM-DEF-OCC:LRETIRE" LINT-STR=CI if 0 14 CL-ADD exit then
   a u s" WLFIND:LENTRY" LINT-STR=CI if 0 11 CL-ADD 12 CL-ADD exit then
   a u s" Lheadrow" LINT-STR=CI if 0 12 CL-ADD exit then
   \ The callback accessor returns its thunk address in A (x9).
   a u s" Lcallbackaddr" LINT-STR=CI if 0 9 CL-ADD exit then
   \ The source observer leaves the original or callback status in x15.
   a u s" Lsourceexecfinish" LINT-STR=CI if 0 15 CL-ADD exit then
   a u s" Lcfpop" LINT-STR=CI if 0 9 CL-ADD exit then
   a u s" Lkwcmp" LINT-STR=CI if 0 0 CL-ADD exit then
   a u s" Lloc-find" LINT-STR=CI if 0 0 CL-ADD exit then
   a u s" Ltok" LINT-STR=CI if 0 0 CL-ADD exit then
   a u s" Lsrcrd" LINT-STR=CI if 0 9 CL-ADD exit then
   a u s" Lfind" LINT-STR=CI if 0 5 CL-ADD 11 CL-ADD 12 CL-ADD 13 CL-ADD exit then
   a u s" Lfindused" LINT-STR=CI if 0 5 CL-ADD 11 CL-ADD 12 CL-ADD 13 CL-ADD exit then
   \ LFIND's hit shape (x5, x11-x13) plus x6 = records met; at two, x16 and
   \ x12 name the first and second record.
   a u s" Lfindused-core" LINT-STR=CI if
      0 5 CL-ADD 6 CL-ADD 11 CL-ADD 12 CL-ADD 13 CL-ADD 16 CL-ADD exit then
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
   a u s" Lvshared?" LINT-STR=CI if 0 13 CL-ADD exit then
   a u s" Lvdest" LINT-STR=CI if 0 12 CL-ADD 14 CL-ADD exit then
   a u s" Lp2cwat" LINT-STR=CI if 0 10 CL-ADD 11 CL-ADD exit then
   0 ;
: PRESERVE-MASK  ( ptr u8 n -- n ) {: a:ptr u :}
   \ LROOM restores x9-x12; its exhaustion branch throws without returning.
   a u s" EM-DEF-OCC:LROOM" LINT-STR=CI if
      0 9 CL-ADD 10 CL-ADD 11 CL-ADD 12 CL-ADD exit then
   \ LCOUNT frames x9-x14/x30 and leaves x15 untouched. APPEND also frames
   \ x15-x17 around its forwarded checker callback; RESET frames LCOUNT.
   a u s" EM-DEF-OCC:LAPPEND" LINT-STR=CI if
      0 9 CL-ADD 10 CL-ADD 11 CL-ADD 12 CL-ADD 13 CL-ADD 14 CL-ADD
        15 CL-ADD 16 CL-ADD 17 CL-ADD 30 CL-ADD exit then
   \ LRETIRE frames x9-x13 and x15 and writes only its answer, x14.
   a u s" EM-DEF-OCC:LRETIRE" LINT-STR=CI if
      0 9 CL-ADD 10 CL-ADD 11 CL-ADD 12 CL-ADD 13 CL-ADD 15 CL-ADD exit then
   a u s" EM-DEF-OCC:LCOUNT" LINT-STR=CI
   a u s" EM-DEF-OCC:LRESET" LINT-STR=CI or if
      0 9 CL-ADD 10 CL-ADD 11 CL-ADD 12 CL-ADD 13 CL-ADD 14 CL-ADD
        15 CL-ADD 30 CL-ADD exit then
   \ The relocation helpers touch syscall scratch only in fatal write/exit
   \ arms; neither x8 nor x16 is written on a returning path.
   a u s" RELOC-EMIT:LCALLS" LINT-STR=CI
   a u s" RELOC-EMIT:LADDRS" LINT-STR=CI or if SYS-SCRATCH-MASK exit then
   \ Every registrar entry saves/restores x0..x17 around its emitted helpers.
   a u s" RELOC-EMIT:Lmark" LINT-STR=CI if $3FFFF exit then
   a u s" RELOC-EMIT:Lptrmark" LINT-STR=CI if $3FFFF exit then
   a u s" RELOC-EMIT:Lscratchmark" LINT-STR=CI if $3FFFF exit then
   a u s" RELOC-EMIT:Lindexrelease" LINT-STR=CI if $3FFFF exit then
   \ Recovery uses the same MARK-SAVE/RESTORE register frame as the registrars.
   a u s" RELOC-EMIT:Lrollback" LINT-STR=CI if $3FFFF exit then
   \ EM-CODE-INVALIDATE saves these recovery registers around its observer call.
   a u s" Lcodeinv" LINT-STR=CI if
      0 10 CL-ADD 11 CL-ADD 13 CL-ADD 14 CL-ADD 15 CL-ADD 30 CL-ADD exit then
   a u s" Lvpushc" LINT-STR=CI if 0 11 CL-ADD exit then
   a u s" Lvpushr" LINT-STR=CI if 0 14 CL-ADD exit then
   a u s" Lvforcek" LINT-STR=CI if 0 5 CL-ADD exit then
   a u s" Lfforcek" LINT-STR=CI if 0 5 CL-ADD exit then
   a u s" Lvpushf" LINT-STR=CI if 0 11 CL-ADD exit then
   a u s" Lvbit" LINT-STR=CI if 0 7 CL-ADD exit then
   a u s" Lbcap" LINT-STR=CI if 0 0 CL-ADD 1 CL-ADD 2 CL-ADD 16 CL-ADD exit then
   a u s" Lbcs" LINT-STR=CI if 0 0 CL-ADD 1 CL-ADD 2 CL-ADD 16 CL-ADD exit then
   \ EMIT-CEMIT frames x12/x13 and writes no other register but CP.
   a u s" Lcemit" LINT-STR=CI if
      0 0 CL-ADD 1 CL-ADD 2 CL-ADD 8 CL-ADD 12 CL-ADD 13 CL-ADD
        16 CL-ADD 30 CL-ADD exit then
   \ LHEADROW frames x0, x1, x9, x10, x13, x17 and x30 around its probes.
   a u s" Lheadrow" LINT-STR=CI if
      0 0 CL-ADD 1 CL-ADD 9 CL-ADD 10 CL-ADD 13 CL-ADD 17 CL-ADD 30 CL-ADD exit then
   \ INTERP-EMIT:LFINDSHADOW reloads LFIND's outputs from its frame before it returns.
   a u s" INTERP-EMIT:Lfindshadow" LINT-STR=CI if 0 5 CL-ADD 11 CL-ADD 12 CL-ADD 13 CL-ADD exit then
   a u s" Lprotwidq" LINT-STR=CI if 0 5 CL-ADD 6 CL-ADD 7 CL-ADD 14 CL-ADD exit then
   a u s" Lhidxadd" LINT-STR=CI if
      0 2 CL-ADD 3 CL-ADD 4 CL-ADD 5 CL-ADD 6 CL-ADD 7 CL-ADD 8 CL-ADD
        14 CL-ADD 15 CL-ADD 16 CL-ADD 17 CL-ADD exit
   then
   0 ;

\ ---- wrapped emitter calls ------------------------------------------------
\ An UPPER-CASE `PKG:CALL` word emits a register-move prelude and a BL to a
\ shared helper, so neither the mnemonic scan nor a `LABEL@ BL,` triple sees it.
\ PROT-GUARD:CALL (src/habu/habu1.f) moves (addr,len) into x10/x11 for the
\ LPROTSPAN guard, which touches only x12/x13; the BL clobbers x30.
\ C-FIND-GLOBAL feeds literal label/count arguments to LFIND and returns by its
\ ABI. Any other `:CALL` shape, or operands that do not resolve to registers,
\ fails closed via E-CLOBBER-WRAP-UNRESOLVED.
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
   a u s" TADR," LINT-STR=CI if LINT-TRUE exit then
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
   a u s" c-hidx-hash" LINT-STR=CI if LINT-TRUE exit then
   a u s" g-print9" LINT-STR=CI if LINT-TRUE exit then
   a u s" c-lit" LINT-STR=CI if LINT-TRUE exit then
   a u s" c-call" LINT-STR=CI if LINT-TRUE exit then
   a u s" c-popflag" LINT-STR=CI if LINT-TRUE exit then
   a u s" c-pushcp" LINT-STR=CI if LINT-TRUE exit then
   a u s" c-emitw" LINT-STR=CI if LINT-TRUE exit then
   a u s" c-bback" LINT-STR=CI if LINT-TRUE exit then
   a u s" c-qualify-seal-guard" LINT-STR=CI if LINT-TRUE exit then
   a u s" c-trusted-tick?" LINT-STR=CI if LINT-TRUE exit then
   a u s" does-rec:clause-of" LINT-STR=CI if LINT-TRUE exit then
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
   a u s" SDIV," LINT-STR= if LINT-TRUE exit then  a u s" UDIV," LINT-STR= if LINT-TRUE exit then
   a u s" CSEL," LINT-STR= ;
: MN-W2I?  ( ptr u8 n -- bool ) {: a:ptr u :}
   a u s" ADDI," LINT-STR= if LINT-TRUE exit then  a u s" SUBI," LINT-STR= if LINT-TRUE exit then
   a u s" LSLI," LINT-STR= if LINT-TRUE exit then  a u s" LSRI," LINT-STR= if LINT-TRUE exit then
   a u s" ASRI," LINT-STR= if LINT-TRUE exit then  a u s" ANDI," LINT-STR= if LINT-TRUE exit then
   a u s" ORRI," LINT-STR= if LINT-TRUE exit then  a u s" EORI," LINT-STR= if LINT-TRUE exit then
   a u s" ADDI-4K," LINT-STR= if LINT-TRUE exit then  a u s" SUBI-4K," LINT-STR= ;
: MN-W1?  ( ptr u8 n -- bool ) {: a:ptr u :}
   a u s" MOVZ," LINT-STR= if LINT-TRUE exit then  a u s" MOVN," LINT-STR= if LINT-TRUE exit then
   a u s" ADR," LINT-STR= if LINT-TRUE exit then  a u s" LIT64," LINT-STR= if LINT-TRUE exit then
   a u s" CSET," LINT-STR= if LINT-TRUE exit then  a u s" CSETM," LINT-STR= if LINT-TRUE exit then
   a u s" LOFF," LINT-STR= if LINT-TRUE exit then
   \ FMOVDX and FCVTZS write their first, x, register from a d register.
   a u s" FMOVDX," LINT-STR= if LINT-TRUE exit then  a u s" FCVTZS," LINT-STR= if LINT-TRUE exit then
   \ CB-SELF reads the thread base into its register.
   a u s" CB-SELF," LINT-STR= ;
: MN-WRMW?  ( ptr u8 n -- bool ) {: a:ptr u :}
   a u s" MOVK," LINT-STR= if LINT-TRUE exit then  a u s" MOVZHW," LINT-STR= if LINT-TRUE exit then
   a u s" MOVKHW," LINT-STR= if LINT-TRUE exit then  a u s" MOVNHW," LINT-STR= ;
: MN-LD?  ( ptr u8 n -- bool ) {: a:ptr u :}
   a u s" LDR," LINT-STR= if LINT-TRUE exit then  a u s" LDRB," LINT-STR= if LINT-TRUE exit then
   a u s" LDRW," LINT-STR= if LINT-TRUE exit then  a u s" LDAR," LINT-STR= ;
: MN-ST?  ( ptr u8 n -- bool ) {: a:ptr u :}
   a u s" STR," LINT-STR= if LINT-TRUE exit then  a u s" STRB," LINT-STR= if LINT-TRUE exit then
   a u s" STRW," LINT-STR= if LINT-TRUE exit then  a u s" STLR," LINT-STR= ;
\ The data words lay down data, not instructions.
: MN-DATA?  ( ptr u8 n -- bool ) {: a:ptr u :}
   a u s" ," LINT-STR= if LINT-TRUE exit then  a u s" BYTES," LINT-STR= if LINT-TRUE exit then
   a u s" DCQ," LINT-STR= if LINT-TRUE exit then  a u s" DLBL," LINT-STR= ;
\ The barriers and the d register operations touch no x register.
: MN-QUIET?  ( ptr u8 n -- bool ) {: a:ptr u :}
   a u s" ISB," LINT-STR= if LINT-TRUE exit then  a u s" DSB-ISH," LINT-STR= if LINT-TRUE exit then
   a u s" FADD," LINT-STR= if LINT-TRUE exit then  a u s" FSUB," LINT-STR= if LINT-TRUE exit then
   a u s" FMUL," LINT-STR= if LINT-TRUE exit then  a u s" FDIV," LINT-STR= if LINT-TRUE exit then
   a u s" FABS," LINT-STR= if LINT-TRUE exit then  a u s" FNEG," LINT-STR= if LINT-TRUE exit then
   a u s" FSQRT," LINT-STR= if LINT-TRUE exit then  a u s" FCMP," LINT-STR= if LINT-TRUE exit then
   a u s" FCMP0," LINT-STR= ;

\ A modeled mnemonic's kind: the registers it writes and reads, by operand.
0 constant MN-NONE
1 constant MN-WRR      \ writes its first register, reads the next two
2 constant MN-WR       \ writes its first, reads its second
3 constant MN-W        \ writes its first
4 constant MN-KEEP     \ writes part of its first, keeping the rest
5 constant MN-RR       \ reads its first two
6 constant MN-LDRPRE
7 constant MN-STRPOST
8 constant MN-CASAL
9 constant MN-FMOVXD
10 constant MN-R       \ reads its first
11 constant MN-CB      \ reads its first and branches on it
12 constant MN-SVC
13 constant MN-SYS
14 constant MN-RET
15 constant MN-BLR
16 constant MN-B
17 constant MN-COND    \ branches on a flag or a bit it does not model
18 constant MN-BR
19 constant MN-BL
20 constant MN-LBL
21 constant MN-QUIET   \ writes and reads no x register
22 constant MN-TRAP    \ ends the path, touching no register
23 constant MN-DATA    \ never executed: leaves the path as it found it

\ The one list of mnemonics the lint models; EFFECTS, both passes and
\ HAND-MODEL? read it.
: MN-KIND  ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u ENDS-COMMA? 0= if MN-NONE exit then      \ every mnemonic ends in a comma
   a u MN-W3? if MN-WRR exit then
   a u MN-W2I?  a u MN-LD? or if MN-WR exit then
   a u MN-W1? if MN-W exit then
   a u MN-WRMW? if MN-KEEP exit then
   a u MN-ST?  a u s" CMP," LINT-STR= or if MN-RR exit then
   a u s" LDRPRE," LINT-STR= if MN-LDRPRE exit then
   a u s" STRPOST," LINT-STR= if MN-STRPOST exit then
   a u s" CASAL," LINT-STR= if MN-CASAL exit then
   a u s" FMOVXD," LINT-STR=  a u s" SCVTF," LINT-STR= or if MN-FMOVXD exit then
   a u s" CMPI," LINT-STR=  a u s" ICIVAU," LINT-STR= or
   a u s" DCCVAU," LINT-STR= or if MN-R exit then
   a u s" CBZ," LINT-STR=  a u s" CBNZ," LINT-STR= or if MN-CB exit then
   a u s" SVC," LINT-STR= if MN-SVC exit then
   a u s" SYS," LINT-STR= if MN-SYS exit then
   a u s" RET," LINT-STR= if MN-RET exit then
   a u s" BLR," LINT-STR= if MN-BLR exit then
   a u s" B," LINT-STR= if MN-B exit then
   a u s" BCOND," LINT-STR=  a u s" TBZ," LINT-STR= or
   a u s" TBNZ," LINT-STR= or if MN-COND exit then
   a u s" BR," LINT-STR= if MN-BR exit then
   a u s" BL," LINT-STR= if MN-BL exit then
   a u s" LBL," LINT-STR= if MN-LBL exit then
   a u MN-DATA? if MN-DATA exit then
   a u MN-QUIET? if MN-QUIET exit then
   a u s" BRK," LINT-STR= if MN-TRAP exit then
   MN-NONE ;

\ ---- register extraction/effects -----------------------------------------
16 constant RRMAX
create RRS RRMAX cells allot   variable RR#
variable RK
\ Each token's role in its definition, which DEF-LOCALS gives it.
DYNAMIC-BUFFER ROLES u8
0 constant OPERAND
1 constant IN-GROUP     \ `{:`, a name or `:}` of a group
2 constant LOCAL-USE
3 constant CONSUMED     \ an operand a later group binds
4 constant FLOW         \ a host control word, which FLOW-STEP reads

: ROLE@  ( n -- n )  ROLES c@ ;

\ A local, or a token of its group, whatever its spelling.
: LOCAL-AT?  ( n -- bool )  ROLE@ dup IN-GROUP = swap LOCAL-USE = or ;

: RR-CHECK-ROOM ( -- )
   RR# @ RRMAX >= if s" clobber-lint: too many regs in instruction" 1 die then ;
: RR+  ( n -- )
   RR-CHECK-ROOM
   RRS RR# @ cells + !  RR# @ 1+ RR# ! ;
: RR@  ( n -- n )
   dup RR# @ < if RRS swap cells + @ else drop -1 then ;
\ Only an operand token names a register: a local's value and the cells a
\ group binds name none.
: COLLECT-REGS  ( n n -- ) {: lo hi :}
   0 RR# !  lo RK !
   begin RK @ hi < while
      RK @ ROLE@ OPERAND = if
         RK @ TOK REG-OF dup 0 >= if RR+ else drop then
      then
      RK @ 1+ RK !
   repeat ;
: EW  ( n -- )  RR@ dup 0 >= if WMSK @ swap CL-ADD WMSK ! else drop then ;
: ER  ( n -- )  RR@ dup 0 >= if RMSK @ swap CL-ADD RMSK ! else drop then ;

: PSEUDO-EFFECTS  {: a u :}  ( -- )
   \ TADR, writes its destination and scratch; the text base is read via DATA.
   a u s" TADR," LINT-STR=CI if 0 EW 1 EW 0 20 CL-ADD CL-ROR exit then
   \ These inline registrar helpers use a fixed machine-register ABI.
   a u s" mark-save" LINT-STR=CI if
      $3FFFF 31 CL-ADD CL-ROR 0 16 CL-ADD 31 CL-ADD CL-WOR exit then
   a u s" mark-restore" LINT-STR=CI if
      0 31 CL-ADD CL-ROR $3FFFF 31 CL-ADD CL-WOR exit then
   \ RELOC-EMIT:MARK-HEADER loads the table base MARK-SAVE's frame keeps at
   \ SP+184 into x16, and its address-table header into x4.
   a u s" mark-header" LINT-STR=CI if
      0 31 CL-ADD CL-ROR 0 4 CL-ADD 16 CL-ADD CL-WOR exit then
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
   \ G-PUSH and G-POP also move x19 by the data-stack protocol, a stack effect
   \ rather than a clobber; prof.f and crash.f hold x19 across calls, so it
   \ stays tracked everywhere else.
   a u s" g-push" LINT-STR=CI if 0 ER exit then
   \ C-HIDX-HASH ( nr lr hr c3 c4 c7 ) reads the name and writes the hash and
   \ its scratch (habu1.f).
   a u s" c-hidx-hash" LINT-STR=CI if 0 ER 1 ER 2 EW 3 EW 4 EW 5 EW exit then
   a u s" g-pop" LINT-STR=CI if 0 EW exit then
   a u s" g-print9" LINT-STR=CI if 0 0 CL-ADD 1 CL-ADD 2 CL-ADD 16 CL-ADD CL-WOR  0 9 CL-ADD CL-ROR exit then
   a u s" c-lit" LINT-STR=CI if 0 5 CL-ADD 6 CL-ADD 7 CL-ADD 8 CL-ADD 9 CL-ADD 30 CL-ADD CL-WOR  0 11 CL-ADD CL-ROR exit then
   a u s" c-call" LINT-STR=CI if 0 5 CL-ADD 7 CL-ADD 8 CL-ADD 9 CL-ADD 10 CL-ADD 13 CL-ADD 14 CL-ADD 15 CL-ADD 30 CL-ADD CL-WOR  0 11 CL-ADD 12 CL-ADD CL-ROR exit then
   a u s" c-popflag" LINT-STR=CI if 0 9 CL-ADD 19 CL-ADD CL-WOR exit then
   a u s" c-pushcp" LINT-STR=CI if 0 9 CL-ADD 30 CL-ADD CL-WOR exit then
   a u s" c-emitw" LINT-STR=CI if 0 9 CL-ADD 30 CL-ADD CL-WOR exit then
   a u s" c-bback" LINT-STR=CI if 0 5 CL-ADD 9 CL-ADD 10 CL-ADD 30 CL-ADD CL-WOR  0 9 CL-ADD CL-ROR exit then
   \ habu2.f C-QUALIFY-SEAL-GUARD frames x30 across its LHEADROW call.
   a u s" c-qualify-seal-guard" LINT-STR=CI if
      0 2 CL-ADD 3 CL-ADD 4 CL-ADD 5 CL-ADD 6 CL-ADD 7 CL-ADD 8 CL-ADD 9 CL-ADD
        11 CL-ADD 12 CL-ADD 13 CL-ADD 14 CL-ADD 15 CL-ADD 16 CL-ADD 17 CL-ADD
        24 CL-ADD 25 CL-ADD CL-WOR exit then
   \ habu2.f C-TRUSTED-TICK? answers in x17; its query runs through a raw BLR
   \ and only x5 and x11-x13 are restored.
   a u s" c-trusted-tick?" LINT-STR=CI if
      0 0 CL-ADD 1 CL-ADD 2 CL-ADD 3 CL-ADD 4 CL-ADD 6 CL-ADD 7 CL-ADD 8 CL-ADD
        9 CL-ADD 10 CL-ADD 14 CL-ADD 15 CL-ADD 16 CL-ADD 17 CL-ADD CL-WOR exit then
   \ habu2.f DOES-REC:CLAUSE-OF reads the record in x5 and answers in x6.
   a u s" does-rec:clause-of" LINT-STR=CI if
      0 6 CL-ADD 7 CL-ADD 8 CL-ADD 9 CL-ADD 10 CL-ADD 12 CL-ADD 14 CL-ADD
        15 CL-ADD 16 CL-ADD CL-WOR  0 5 CL-ADD CL-ROR exit then
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

\ Control never passes the instruction at token hi, operands from lo: BRK,
\ traps, and an exit system call does not return; BLR, does.
: PATH-END?  ( n n n -- bool ) {: kind:n lo:n hi:n :}
   kind MN-RET =  kind MN-B = or  kind MN-BR = or  kind MN-TRAP = or if
      LINT-TRUE exit then
   kind MN-SYS = if lo hi SYS-EXIT? else LINT-FALSE then ;

\ Where a walk's path stands. An unknown call's path may end; an ended path
\ revives at a label a branch can reach or where FLOW-STEP joins a live one.
0 constant LIVE
1 constant MAY-END
2 constant ENDED
variable PATH

\ The path after the instruction at token hi, of this kind, operands from lo.
: PATH-STEP  ( n n n -- ) {: kind:n lo:n hi:n :}
   kind MN-LBL = if LIVE PATH ! exit then
   kind MN-DATA = if exit then
   kind lo hi PATH-END? if ENDED PATH ! exit then
   PATH @ MAY-END = if LIVE PATH ! then ;

\ ---- path states -----------------------------------------------------------
\ Pass 2's state besides PATH: the registers written since entry (DIRTY) and,
\ for each register, the callee that clobbered it while written, or -1 (POIS).
create POIS 32 cells allot   variable DIRTY

: POIS@ ( n -- n )  POIS swap cells + @ ;
: POIS! ( n n -- )  POIS swap cells + ! ;
: POIS-CLEAR  ( -- )  32 0 do -1 i POIS! loop ;

\ Nothing reaches here: the dead state.
: PATH-DEAD  ( -- )  ENDED PATH !  0 DIRTY !  POIS-CLEAR ;

\ A saved state is a row: two cells its table owns, DIRTY, then 32 POIS cells.
2 constant LS-DIRTY
3 constant LS-POIS
LS-POIS 32 + constant LS-STRIDE


\ Join the DIRTY and POIS cells saved at s into the current state: written or
\ poisoned on either side; a register poisoned on both keeps its current callee.
: JOIN-STATE  ( ptr n -- )
   {: s:ptr :}
   s @ DIRTY @ or DIRTY !
   32 0 do
      i POIS@ 0 <  s i 1+ cells + @ 0 >= and if  s i 1+ cells + @ i POIS!  then
   loop ;


\ The walk's mirror of the host control-flow stack, FLOW# entries, each a state
\ row whose own cells are whether it is a `do` or `?do` entry and its PATH.
DYNAMIC-BUFFER FLOWS n   variable FLOW#
0 constant FL-LOOP
1 constant FL-PATH

: FL  ( n n -- ptr n ) {: e:n field:n :}  e LS-STRIDE * field + FLOWS ;
: FLOW-COPY  ( n n -- ) {: from:n to:n :}  LS-STRIDE 0 do from i FL @ to i FL ! loop ;


\ Exchange the current state with entry e's.
: FLOW-SWAP  ( n -- )
   {: e:n :}
   PATH @  e FL-PATH FL @ PATH !  e FL-PATH FL !
   DIRTY @  e LS-DIRTY FL @ DIRTY !  e LS-DIRTY FL !
   32 0 do  i POIS@  e LS-POIS i + FL @ i POIS!  e LS-POIS i + FL !  loop ;


\ Join entry e's state into the current one: the more live path wins.
: ENTRY>STATE  ( n -- )
   {: e:n :}
   e FL-PATH FL @ PATH @ min PATH !
   e LS-DIRTY FL JOIN-STATE ;


: DEAD>ENTRY  ( n -- ) {: e:n :}  e FLOW-SWAP  PATH-DEAD  e FLOW-SWAP ;
: JOIN>ENTRY  ( n -- ) {: e:n :}  e FLOW-SWAP  e ENTRY>STATE  e FLOW-SWAP ;


: FLOW-SLOT  ( -- n )
   FLOW# @ 1+ LS-STRIDE * FLOWS-RESERVE
   FLOW# @  FLOW# @ 1+ FLOW# ! ;


\ Entry e becomes one holding the current state, a do entry when loop? is true.
: STATE>ENTRY  ( bool n -- )
   {: loop?:bool e:n :}
   loop? bool>n e FL-LOOP FL !
   PATH @ e FL-PATH FL !  DIRTY @ e LS-DIRTY FL !
   32 0 do i POIS@ e LS-POIS i + FL ! loop ;

: FLOW-PUSH  ( bool -- n ) FLOW-SLOT {: loop?:bool e:n :}  loop? e STATE>ENTRY  e ;


\ A dead entry beneath the others, a do entry when loop? is true: a walk that
\ starts inside blocks, as pass 1 does at a routine's label, finds them dead.
: FLOW-FLOOR  ( bool -- )
   {: loop?:bool :}
   FLOW-SLOT begin dup 0 > while  dup 1- over FLOW-COPY  1- repeat drop
   loop? bool>n 0 FL-LOOP FL !  0 DEAD>ENTRY ;


: FLOW-NEED  ( n -- )
   {: n:n :}
   begin FLOW# @ n < while LINT-FALSE FLOW-FLOOR repeat ;


: FLOW-TOP  ( -- n )  1 FLOW-NEED  FLOW# @ 1- ;
: FLOW-POP  ( -- n )  FLOW-TOP  dup FLOW# ! ;


\ The innermost do entry.
: FLOW-LOOP  ( -- n )
   FLOW# @ begin dup 0 > while
      1-  dup FL-LOOP FL @ 0<> if exit then
   repeat drop
   LINT-TRUE FLOW-FLOOR  0 ;


\ An orig holding the current state goes beneath the top entry (ANS `if 1
\ cs-roll`).
: FLOW-WHILE  ( -- )
   1 FLOW-NEED  FLOW-SLOT {: e:n :}
   e 1- e FLOW-COPY  LINT-FALSE e 1- STATE>ENTRY ;


\ A host control word moves the current state through the stack as the host's
\ control flow moves the path. Back edges are not modeled: a loop body is walked once.
: FLOW-STEP  ( n -- )
   TOK {: a:ptr u:n :}
   a u s" if" LINT-STR=CI  a u s" of" LINT-STR=CI or  a u s" begin" LINT-STR=CI or if
      LINT-FALSE FLOW-PUSH drop exit
   then
   a u s" else" LINT-STR=CI if FLOW-TOP FLOW-SWAP exit then
   a u s" then" LINT-STR=CI  a u s" endcase" LINT-STR=CI or
   a u s" loop" LINT-STR=CI or  a u s" +loop" LINT-STR=CI or if
      FLOW-POP ENTRY>STATE exit
   then
   a u s" until" LINT-STR=CI if FLOW-POP drop exit then
   a u s" again" LINT-STR=CI if FLOW-POP drop  PATH-DEAD exit then
   a u s" while" LINT-STR=CI if FLOW-WHILE exit then
   a u s" repeat" LINT-STR=CI if FLOW-POP drop  PATH-DEAD  FLOW-POP ENTRY>STATE exit then
   a u s" do" LINT-STR=CI if LINT-TRUE FLOW-PUSH DEAD>ENTRY exit then
   a u s" ?do" LINT-STR=CI if LINT-TRUE FLOW-PUSH drop exit then
   a u s" leave" LINT-STR=CI if FLOW-LOOP JOIN>ENTRY  PATH-DEAD exit then
   a u s" case" LINT-STR=CI  a u s" match" LINT-STR=CI or if
      LINT-FALSE FLOW-PUSH DEAD>ENTRY exit
   then
   \ Only MATCH's arms leave it: a tag no arm matches dies.
   a u s" ;match" LINT-STR=CI if PATH-DEAD FLOW-POP ENTRY>STATE exit then
   a u s" endof" LINT-STR=CI if
      2 FLOW-NEED  FLOW# @ 2 - JOIN>ENTRY  PATH-DEAD  FLOW-POP ENTRY>STATE
   then ;

\ The instruction at token hi, of this kind, with operands from lo.
: EFFECTS  ( ptr u8 n n n n -- ) {: a:ptr u:n kind:n lo:n hi:n :}
   0 WMSK !  0 RMSK !  lo hi COLLECT-REGS
   kind MN-WRR = if 0 EW 1 ER 2 ER exit then
   kind MN-WR = if 0 EW 1 ER exit then
   kind MN-W = if 0 EW exit then
   kind MN-KEEP = if RR# @ 0 > if 0 EW 0 ER then exit then
   \ A full-width store to the machine stack saves its register, which the lint
   \ does not follow through memory: the save reads only SP.
   kind MN-RR = if
      a u s" STR," LINT-STR=  1 RR@ 31 = and if 1 ER exit then
      0 ER 1 ER exit
   then
   \ The writeback forms move their base, the second register, as well.
   kind MN-LDRPRE = if 0 EW 1 ER 1 EW exit then
   kind MN-STRPOST = if 0 ER 1 ER 1 EW exit then
   \ CASAL Xs, Xt, [Xn] compares Xs and returns the old value in it.
   kind MN-CASAL = if 0 ER 0 EW 1 ER 2 ER exit then
   \ FMOVXD and SCVTF read their second, x, register into their first, a d
   \ register.
   kind MN-FMOVXD = if 1 ER exit then
   kind MN-R =  kind MN-CB = or if 0 ER exit then
   kind MN-SVC = if 0 0 CL-ADD CL-WOR  0 0 CL-ADD 1 CL-ADD 2 CL-ADD 16 CL-ADD CL-ROR exit then
   kind MN-SYS = if
      lo hi SYS-EXIT? if 0 0 CL-ADD 8 CL-ADD 16 CL-ADD CL-WOR  0 0 CL-ADD CL-ROR exit then
      0 0 CL-ADD 8 CL-ADD 16 CL-ADD CL-WOR  0 0 CL-ADD 1 CL-ADD 2 CL-ADD CL-ROR exit
   then
   kind MN-RET = if 0 30 CL-ADD CL-ROR exit then
   kind MN-BLR = if 1 18 lshift 1 -  30 CL-ADD CL-WOR  0 ER exit then
   kind MN-NONE = if a u PSEUDO-EFFECTS then ;

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
variable RNEXT  variable RDONE  variable CUR

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

\ ---- host words -----------------------------------------------------------
\ Pass 1 makes a row for each colon definition, noting whether its body emits;
\ a call sees only rows below BOUND#, those of earlier definitions.
DYNAMIC-BUFFER HOSTS n
0 constant PREV      \ the row of the name's earlier definition, or -1
1 constant EMITTER   \ -1 when its body emits, else 0
2 constant HOST-STRIDE
variable HOST#  variable BOUND#  variable DEF#  variable SEED#
\ Each row's folded name, the name map's key; DECL-FILE sums NAME-CAP.
DYNAMIC-BUFFER HOST-NAMES u8   variable NAME-CAP  variable NAME-END
\ The name map: a folded name in HOST-NAMES to its latest row.
DYNAMIC-BUFFER HOST-MAP n   variable MAP-CAP
\ Each token's UNKNOWN-AT? answer as a cell, UNSEEN until asked, once per pass.
DYNAMIC-BUFFER HREF n
-2 constant UNSEEN
\ The walk's live locals, each its group token and its name's length, and
\ for each open block the count of locals live when it opened.
DYNAMIC-BUFFER LOCALS n   variable LOCAL#
DYNAMIC-BUFFER BLOCKS n   variable BLOCK#

: HOST@  ( n n -- n ) {: row:n field:n :}
   row HOST-STRIDE * field + HOSTS @ ;
: HOST!  ( n n n -- ) {: v:n row:n field:n :}
   v row HOST-STRIDE * field + HOSTS ! ;

: HOSTS-CLOSE  ( -- )
   HOSTS-RELEASE  HOST-NAMES-RELEASE  HOST-MAP-RELEASE  HREF-RELEASE
   ROLES-RELEASE  LOCALS-RELEASE  BLOCKS-RELEASE  FLOWS-RELEASE ;

: MAP-ROW  ( ptr u8 n -- n ) {: a:ptr u:n :}
   0 HOST-MAP MAP-CAP @ >COUNT a u >LEN MAP:GET MATCH option
      none OF -1 ENDOF
      some OF ENDOF
   ;MATCH ;

\ The folded copy of a name in the label pool's unused tail.
: KEY$  ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   u CNBUF-CAP CEND @ - > if s" clobber-lint: label store full" 1 die then
   a u CNBUF CEND @ + FOLD-TO
   CNBUF CEND @ + u ;

: VISIBLE  ( n -- n )
   begin dup BOUND# @ >= while PREV HOST@ repeat ;

\ A word the lint models by hand keeps that model where a scanned file
\ defines it, as habu1.f defines CB-SELF,.
: HAND-MODEL?  ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u MN-KIND MN-NONE <> if LINT-TRUE exit then
   a u PSEUDO? if LINT-TRUE exit then
   a u CLOBBER-WRAP:WRAP? ;

\ A call names the word in its package first, then a global one: its row,
\ or -1.
: HOST-LOOKUP  ( n -- n ) {: k:n :}
   k TOK C-QUALIFIED$ KEY$ MAP-ROW VISIBLE {: row:n :}
   row 0 < if k TOK KEY$ MAP-ROW VISIBLE else row then ;

\ A call is unknown when it emits code the lint does not model: an emitting
\ row or an instruction word with no row, unless HAND-MODEL? covers it.
: CALL-UNKNOWN?  ( n -- bool ) {: k:n :}
   k HOST-LOOKUP {: row:n :}
   row 0 >= if
      row EMITTER HOST@ 0= if LINT-FALSE exit then
   else
      k TOK ENDS-COMMA? 0= if LINT-FALSE exit then
      k TOK INSTR? 0= if LINT-FALSE exit then
   then
   k TOK HAND-MODEL? 0= ;

: UNKNOWN-AT?  ( n -- bool ) {: k:n :}
   k HREF @ UNSEEN = if k CALL-UNKNOWN? bool>n k HREF ! then
   k HREF @ 0 <> ;

: FILE-START  ( -- )
   TN# @ 1 max {: n:n :}
   n HREF-RESERVE  n ROLES-RESERVE  n 2 * LOCALS-RESERVE  n BLOCKS-RESERVE
   TN# @ 0 ?do UNSEEN i HREF !  OPERAND i ROLES c! loop ;

\ A local's name is its text before any `:type`.
: LOCAL-NAME  ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   a u 58 LINT-INDEX-OF MATCH option
      none OF a u ENDOF
      some OF a swap ENDOF
   ;MATCH ;

: BLOCK-OPEN?  ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" if" LINT-STR=CI if LINT-TRUE exit then
   a u s" begin" LINT-STR=CI if LINT-TRUE exit then
   a u s" do" LINT-STR=CI if LINT-TRUE exit then
   a u s" ?do" LINT-STR=CI if LINT-TRUE exit then
   a u s" case" LINT-STR=CI if LINT-TRUE exit then
   a u s" of" LINT-STR=CI ;

: BLOCK-CLOSE?  ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" then" LINT-STR=CI if LINT-TRUE exit then
   a u s" loop" LINT-STR=CI if LINT-TRUE exit then
   a u s" +loop" LINT-STR=CI if LINT-TRUE exit then
   a u s" repeat" LINT-STR=CI if LINT-TRUE exit then
   a u s" until" LINT-STR=CI if LINT-TRUE exit then
   a u s" again" LINT-STR=CI if LINT-TRUE exit then
   a u s" endof" LINT-STR=CI if LINT-TRUE exit then
   a u s" endcase" LINT-STR=CI ;

\ The host control words FLOW-STEP reads.
: FLOW-WORD?  ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u BLOCK-OPEN? if LINT-TRUE exit then
   a u BLOCK-CLOSE? if LINT-TRUE exit then
   a u s" else" LINT-STR=CI if LINT-TRUE exit then
   a u s" while" LINT-STR=CI if LINT-TRUE exit then
   a u s" leave" LINT-STR=CI if LINT-TRUE exit then
   a u s" match" LINT-STR=CI if LINT-TRUE exit then
   a u s" ;match" LINT-STR=CI ;

: LOCAL+  ( n n -- )  LOCAL# @ 2 * {: s:n :}  s 1+ LOCALS !  s LOCALS !  LOCAL# @ 1+ LOCAL# ! ;

\ Whether live local e is spelled a u, byte for byte as a local binds.
: NAMES?  ( ptr u8 n n -- bool ) {: a:ptr u:n e:n :}
   e 2 * 1+ LOCALS @ u <> if LINT-FALSE exit then
   e 2 * LOCALS @ TOK drop u a u LINT-STR= ;

: LIVE-LOCAL?  ( n -- bool )
   TOK {: a:ptr u:n :}
   LOCAL# @ begin dup 0 > while
      1- dup a u rot NAMES? if drop LINT-TRUE exit then
   repeat drop LINT-FALSE ;

\ The live locals when the innermost open block, or the definition, began.
: ARM-BASE  ( -- n )  BLOCK# @ 0= if 0 exit then  BLOCK# @ 1- BLOCKS @ ;
: BLOCK+  ( -- )  LOCAL# @ BLOCK# @ BLOCKS !  BLOCK# @ 1+ BLOCK# ! ;
: ARM-END  ( -- )  ARM-BASE LOCAL# ! ;
: BLOCK-END  ( -- )  ARM-END  BLOCK# @ 1- 0 max BLOCK# ! ;

\ Whether a token spells a number: decimal with an optional sign and within a
\ signed cell, or `$` and one to sixteen hex digits.
: NUMBER?  ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u START-DOLLAR? if a 1+ u 1- HEX>NUMBER? else a u STR>NUMBER? then
   MATCH option
      none OF LINT-FALSE ENDOF
      some OF drop LINT-TRUE ENDOF
   ;MATCH ;

\ A `( … )` comment, which the scan keeps as one token.
: PAREN-COMMENT?  ( ptr u8 n -- bool ) {: a:ptr u:n :}
   u 0= if LINT-FALSE exit then
   a c@ 40 <> if LINT-FALSE exit then
   u 1 = if LINT-TRUE exit then
   a 1+ c@ LINT-WS? ;

\ Whether token k, not a local, ends an operand run as both passes read it: an
\ operand pushed before a host control word (the 0 of `n 0 ?do`) is not read.
: RUN-END-AT?  ( n -- bool ) {: k:n :}
   k TOK INSTR? if LINT-TRUE exit then
   k TOK CLOBBER-WRAP:WRAP? if LINT-TRUE exit then
   k TOK FLOW-WORD? if LINT-TRUE exit then
   k UNKNOWN-AT? ;

\ Whether token t is one value a group can bind, a number or a local's use;
\ a number becomes CONSUMED.
: TAKE-VALUE?  ( n -- bool ) {: t:n :}
   t ROLE@ LOCAL-USE = if LINT-TRUE exit then
   t ROLE@ OPERAND =  t TOK NUMBER? and if
      CONSUMED t ROLES c!  LINT-TRUE exit
   then
   LINT-FALSE ;

\ Whether an operand naming a register sits in token t's run, at t or before:
\ only then can a group past an unreadable token t shift a register's position.
: REG-BEFORE?  ( n n -- bool ) {: lo:n t:n :}
   t begin dup lo >= while
      dup LOCAL-AT? 0= if
         dup RUN-END-AT? if drop LINT-FALSE exit then
         dup ROLE@ OPERAND =  over TOK REG-OF 0 >= and if drop LINT-TRUE exit then
      then
      1-
   repeat drop LINT-FALSE ;

\ A group of n at token k binds the last n numbers or local uses in its run;
\ false when another token comes first and may hide a register (REG-BEFORE?).
: BIND-CELLS  ( n n n -- bool ) {: lo:n k:n n:n :}
   n k begin over 0 > if dup lo > else LINT-FALSE then while
      1- dup TAKE-VALUE? if swap 1- swap else
         dup TOK PAREN-COMMENT? 0= if
            dup RUN-END-AT? 0= if lo swap REG-BEFORE? 0= nip exit then
            nip 0 swap
         then
      then
   repeat 2drop LINT-TRUE ;

\ Set by a group that hid its run's register operands, until the run ends.
variable HIDDEN

\ At the token t that ends a hidden run, an instruction is unknown; a path end's
\ or `LBL,`'s effect on the path does not depend on its registers (`BR,` reads one).
: RUN-HIDDEN  ( n n -- ) {: lo:n t:n :}
   t RUN-END-AT? 0= if exit then
   t TOK INSTR? if
      t TOK MN-KIND {: kind:n :}
      kind MN-LBL <>  kind lo t PATH-END? 0= and if -1 t HREF ! then
   then
   0 HIDDEN ! ;

\ The `{: … :}` group at token k: its names join the innermost open block from
\ its `:}`. Answers the token after its `:}`.
: GROUP-BIND  ( n n n -- n ) {: lo:n k:n hi:n :}
   IN-GROUP k ROLES c!
   k 1+ begin dup hi < if dup TOK s" :}" LINT-STR= 0= else LINT-FALSE then while
      IN-GROUP over ROLES c!  1+
   repeat {: close:n :}
   close hi < if IN-GROUP close ROLES c! then
   lo k close k 1+ - BIND-CELLS 0= if -1 HIDDEN ! then
   close k 1+ ?do i  i TOK LOCAL-NAME nip  LOCAL+ loop
   close 1+ ;

\ A token spelled as a live local is that local, never a keyword, a run end or a
\ call (docs/forth.md "Rules learned by refusal", "Parsing operands are data").
: SCOPE-TOKEN  ( n n -- ) {: lo:n t:n :}
   t LIVE-LOCAL? if LOCAL-USE t ROLES c! exit then
   HIDDEN @ if lo t RUN-HIDDEN then
   t TOK FLOW-WORD? if FLOW t ROLES c! then
   t TOK BLOCK-OPEN? if BLOCK+ exit then
   t TOK s" else" LINT-STR=CI if ARM-END exit then
   t TOK BLOCK-CLOSE? if BLOCK-END then ;

\ One walk over the body, tokens lo to hi, gives each token its role.
: DEF-LOCALS  ( n n -- ) {: lo:n hi:n :}
   0 LOCAL# !  0 BLOCK# !  0 HIDDEN !
   lo begin dup hi < while
      dup TOK s" {:" LINT-STR= if lo swap hi GROUP-BIND else lo over SCOPE-TOKEN 1+ then
   repeat drop ;

\ The folded copy of a row's name in HOST-NAMES, kept while the map holds it.
: HOST-NAME$  ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   NAME-END @ u + NAME-CAP @ > if s" clobber-lint: host name store full" 1 die then
   a u NAME-END @ HOST-NAMES FOLD-TO
   NAME-END @ HOST-NAMES u
   NAME-END @ u + NAME-END ! ;

\ A row for the qualified name, whose body emits or not.
: HOST-ROW  ( ptr u8 n bool -- ) {: a:ptr u:n emits:bool :}
   HOST# @ {: row:n :}
   a u HOST-NAME$ {: key:ptr len:n :}
   key len MAP-ROW row PREV HOST!
   emits bool>n row EMITTER HOST!
   row 0 HOST-MAP MAP-CAP @ >COUNT key len >LEN MAP:SET
   row 1+ HOST# ! ;

\ A row for the definition named by token k.
: HOST-NEW  ( n bool -- ) {: k:n emits:bool :}
   k TOK C-QUALIFIED$ emits HOST-ROW ;

\ After pass 1 reads a definition, the calls after it see its row.
: HOST-BIND  ( -- )  HOST# @ BOUND# ! ;

\ EMITW appends an instruction word the lint cannot read; each spelling gets
\ an emitting row ahead of the scanned files', found as theirs are.
: EMITW-NAMES  ( [ ptr u8 n -- ] -- ) {: visit :}
   s" EMITW" visit execute  s" A64ICODE:EMITW" visit execute ;

: SEED-SIZE  ( ptr u8 n -- )
   nip NAME-CAP @ + NAME-CAP !  SEED# @ 1+ SEED# ! ;

: SEED-ROW  ( ptr u8 n -- )  LINT-TRUE HOST-ROW ;

\ A row for each EMITW spelling and each colon definition DECL-FILE counted.
: HOSTS-OPEN  ( -- )
   0 SEED# !  [: SEED-SIZE ;] EMITW-NAMES
   DEF# @ SEED# @ + {: rows:n :}
   rows HOST-STRIDE * HOSTS-RESERVE
   NAME-CAP @ HOST-NAMES-RESERVE  0 NAME-END !
   rows 2 * MAP-CAP !
   MAP-CAP @ >COUNT MAP:CELL-COUNT COUNT>N HOST-MAP-RESERVE
   0 HOST-MAP MAP-CAP @ >COUNT MAP:INIT
   [: SEED-ROW ;] EMITW-NAMES  HOST-BIND ;

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
\ Colon bodies are skipped so emitter operands cannot declare a label; DEF#
\ counts them and NAME-CAP their qualified names' bytes.
: DECL-FILE ( ptr u8 n -- )
   LINT-SOURCE:LOAD LINT-SOURCE:TEXT TOKENIZE 0 PKG-U !
   0 begin dup TN# @ < while
      {: k:n :}
      k TOK s" :" LINT-STR= if
         DEF# @ 1+ DEF# !
         k 1+ TN# @ < if k 1+ TOK C-QUALIFIED$ nip NAME-CAP @ + NAME-CAP ! then
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
\ The kind of the instruction at token k+2 when tokens k and k+1 name a label
\ through LABEL@, else MN-NONE.
: LABEL-SITE ( n n -- n ) {: k:n hi:n :}
   k 2 + hi >= if MN-NONE exit then
   k TOK START-L? 0= if MN-NONE exit then
   k 1+ TOK LABEL-ACCESS? 0= if MN-NONE exit then
   k 2 + TOK MN-KIND ;
: LABEL-OPEN? ( n n -- bool )  LABEL-SITE MN-LBL = ;
: BL-CALL-SITE? ( n n -- bool )  LABEL-SITE MN-BL = ;
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
   LIVE PATH !  0 FLOW# !
   0 RDONE !
   DI @ OPLO ! ;

: ROUTINE-NEXT-OPEN? ( -- bool )
   RNEXT @ ON# @ >= if LINT-FALSE exit then
   DI @ RNEXT @ OPEN@ = ;

: ROUTINE-ADVANCE-OPEN ( -- )
   PATH @ LIVE <> if
      -1 RDONE !
   else
      RNEXT @ 1+ RNEXT !
      DI @ 3 + DI !
      DI @ OPLO !
   then ;

: ROUTINE-CALL? ( n -- bool ) {: kind:n :}
   OPLO @ DI @ CALLEE?  kind MN-BL = and ;

: ROUTINE-ADD-EDGE ( n -- ) {: cidx :}
   CALA @ CALU @ C-ENSURE cidx swap EDGE+ ;

: ROUTINE-ADD-EFFECTS ( n n -- ) {: cidx:n kind:n :}
   DI @ TOK kind OPLO @ DI @ EFFECTS
   cidx WMSK @ C-WOR ;

: ROUTINE-INSTR ( n -- ) {: cidx :}
   DI @ TOK MN-KIND {: kind:n :}
   kind ROUTINE-CALL? if
      cidx ROUTINE-ADD-EDGE
   else
      cidx kind ROUTINE-ADD-EFFECTS
   then
   kind OPLO @ DI @ PATH-STEP
   DI @ 1+ OPLO ! ;

\ A routine that wraps a guard call gains that call's clobbers directly, so
\ the transitive closure carries them into every caller of this routine.
: ROUTINE-WRAP ( n -- ) {: cidx:n :}
   cidx WRAP-MASK C-WOR
   DI @ 1+ OPLO ! ;

\ The token at DI, read into the clobbers of cidx. An unknown call adds
\ nothing, ends the operand run and may not return.
: REGION-TOKEN  ( n -- ) {: cidx:n :}
   DI @ LOCAL-AT? if exit then
   DI @ ROLE@ FLOW = if DI @ FLOW-STEP  DI @ 1+ OPLO ! exit then
   DI @ TOK CLOBBER-WRAP:WRAP? if cidx ROUTINE-WRAP exit then
   DI @ UNKNOWN-AT? if MAY-END PATH !  DI @ 1+ OPLO ! exit then
   DI @ TOK INSTR? if cidx ROUTINE-INSTR then ;

: ROUTINE-STEP ( n -- ) {: cidx :}
   ROUTINE-NEXT-OPEN? if
      ROUTINE-ADVANCE-OPEN
   else
      cidx REGION-TOKEN
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

: EMITS-AT?  ( n -- bool ) {: k:n :}
   k LOCAL-AT? if LINT-FALSE exit then
   k TOK INSTR? if LINT-TRUE exit then
   k TOK CLOBBER-WRAP:WRAP? if LINT-TRUE exit then
   k UNKNOWN-AT? ;

: EMITS?  ( n n -- bool ) {: lo:n hi:n :}
   lo begin dup hi < while
      dup EMITS-AT? if drop LINT-TRUE exit then
      1+
   repeat  drop LINT-FALSE ;

variable WI  variable WE
: PASS1-FILE ( ptr u8 n -- ) {: pa:ptr pu:n :}
   pa pu LINT-SOURCE:LOAD  LINT-SOURCE:TEXT TOKENIZE  FILE-START
   0 PKG-U ! 0 WI !
   begin WI @ TN# @ 1- < while
      WI @ TOK s" :" LINT-STR= if
         WI @ DEF-END WE !
         WI @ 2 + WE @ DEF-LOCALS
         WI @ 1+  WI @ 2 + WE @ EMITS?  HOST-NEW
         WI @ 2 + WE @ PASS1-DEF
         HOST-BIND
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
variable APPLY-MASK
create WNAME 128 allot       variable WLEN
create NUMBUF 2 allot
variable BAD  variable PR  variable CW  variable RETS  variable CALIDX
variable TRACK-LR

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
\ Only the last walk of a definition reports; the walks before it reach the
\ labels' fixed point.
variable QUIET
: REPORT  ( ptr u8 n n n -- ) {: fa:ptr fu:n reg:n cidx:n :}
   QUIET @ if exit then
   fa fu reg cidx ALLOW? if exit then
   fa fu reg cidx FINDING  BAD @ 1+ BAD ! ;
: NOTE-READS  {: fa fu rmask :}  ( -- )
   0 PR !
   begin PR @ 32 < while
      rmask CONTRACT-MASK invert and PR @ CL-HAS? if
         PR @ 30 = TRACK-LR @ 0= and if
         else
         PR @ POIS@ CALIDX !
         CALIDX @ 0 >= if
            DIRTY @ PR @ CL-HAS? if
               fa fu PR @ CALIDX @ REPORT
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
      DI @ TOK MN-KIND MN-RET = if LINT-TRUE exit then
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

: ENTRY-DIRTY ( -- n )
   TRACK-LR @ if 0 30 CL-ADD else 0 then ;

: PASS2-ENTRY ( -- )
   ENTRY-DIRTY DIRTY !
   POIS-CLEAR ;

\ A direct branch or a placement names its label last, optionally through
\ LABEL@.
: TARGET-INDEX ( n -- n )
   1- dup TOK LABEL-ACCESS? if 1- then ;

\ BL, is a call: it returns, so it is no edge to its label.
: DIRECT-JUMP? ( n -- bool ) {: kind:n :}
   kind MN-B =  kind MN-COND = or  kind MN-CB = or ;

\ ---- pass 2: label states -------------------------------------------------
\ One row per LBL, in the definition: its name token, its name's key, then the
\ state recorded from the branches into it, DIRTY and 32 POIS cells.
DYNAMIC-BUFFER LSTATE n
0 constant LS-NAME
1 constant LS-KEY
variable LBL#  variable LX  variable GROWN

: LS  ( n n -- ptr n ) {: lbl:n field:n :}
   lbl LS-STRIDE * field + LSTATE ;

\ A label's key is its index in the label table; -1 marks a name only its
\ definition holds, as a `LBL {: name :}` local, matched by text as C-LABEL$ does.
: LABEL-KEY  ( n -- n )
   TOK C-QUALIFIED$ C-FIND ;

: LABEL-MATCH?  ( n n n -- bool ) {: key:n name:n lbl:n :}
   lbl LS-KEY LS @ key <> if LINT-FALSE exit then
   key 0 >= if LINT-TRUE exit then
   lbl LS-NAME LS @ TOK  name TOK LINT-STR=CI ;

\ Token k places a label; a local spelled `LBL,` is a value and places none.
: LABEL-AT?  ( n -- bool ) {: k:n :}  k TOK MN-KIND MN-LBL =  k LOCAL-AT? 0= and ;

: COLLECT-LABELS  ( n n -- ) {: lo:n hi:n :}
   0 LBL# !
   hi lo ?do i LABEL-AT? if LBL# @ 1+ LBL# ! then loop
   LBL# @ LS-STRIDE * LSTATE-RESERVE
   0 LX !
   hi lo ?do
      i LABEL-AT? if
         i TARGET-INDEX {: name:n :}
         name LX @ LS-NAME LS !
         name LABEL-KEY LX @ LS-KEY LS !
         0 LX @ LS-DIRTY LS !
         32 0 do -1 LX @ LS-POIS i + LS ! loop
         LX @ 1+ LX !
      then
   loop ;

\ Join the current state into a label's: written or poisoned on either side.
\ A register poisoned on both keeps the label's callee.
: STATE>LABEL  ( n -- ) {: lbl:n :}
   lbl LS-DIRTY LS @ {: was:n :}
   was DIRTY @ or {: now:n :}
   now was <> if now lbl LS-DIRTY LS !  -1 GROWN ! then
   32 0 do
      i POIS@ 0 >=  lbl LS-POIS i + LS @ 0 < and if
         i POIS@ lbl LS-POIS i + LS !  -1 GROWN !
      then
   loop ;

: LABEL>STATE  ( n -- )  LS-DIRTY LS JOIN-STATE ;

: BRANCH-EDGE  ( -- )
   DI @ TARGET-INDEX {: name:n :}
   name LABEL-KEY {: key:n :}
   LBL# @ 0 ?do key name i LABEL-MATCH? if i STATE>LABEL then loop ;

\ A global entry also starts a fresh path from its outside callers.
: PLACE-LABEL  ( n -- ) {: hi:n :}
   LX @ LABEL>STATE
   LX @ LS-NAME LS @ hi LABEL-OPEN? if DIRTY @ ENTRY-DIRTY or DIRTY ! then
   LX @ 1+ LX ! ;

: PASS2-INSTR  ( ptr u8 n n -- ) {: fa:ptr fu:n hi:n :}
   DI @ TOK MN-KIND {: kind:n :}
   PATH @ ENDED =  kind MN-LBL <> and if DI @ 1+ OPLO ! exit then
   OPLO @ DI @ CALLEE?  kind MN-BL = and if
      APPLY-CALL
   else
      DI @ TOK kind OPLO @ DI @ EFFECTS
      fa fu RMSK @ NOTE-READS
      WMSK @ APPLY-WRITES
      kind MN-SYS = OPLO @ DI @ SYS-EXIT? 0= and if POISON-SYS-SCRATCH then
      kind MN-BLR = if s" blr" C-ENSURE POISON-LINK-REGISTER then
   then
   kind DIRECT-JUMP? if BRANCH-EDGE then
   kind MN-LBL = if hi PLACE-LABEL then
   kind OPLO @ DI @ PATH-END? if PATH-DEAD then
   kind OPLO @ DI @ PATH-STEP
   DI @ 1+ OPLO ! ;

\ After an unknown call nothing is known, as at an entry: it may rewrite any
\ register, place a label or not return.
: PASS2-TOKEN  ( ptr u8 n n -- ) {: fa:ptr fu:n hi:n :}
   DI @ LOCAL-AT? if exit then
   DI @ ROLE@ FLOW = if DI @ FLOW-STEP  DI @ 1+ OPLO ! exit then
   DI @ TOK CLOBBER-WRAP:WRAP? if
      PATH @ ENDED <> if fa fu APPLY-WRAP then  DI @ 1+ OPLO ! exit
   then
   DI @ UNKNOWN-AT? if PASS2-ENTRY  MAY-END PATH !  DI @ 1+ OPLO ! exit then
   DI @ TOK INSTR? if fa fu hi PASS2-INSTR then ;

: PASS2-WALK  ( ptr u8 n n n -- ) {: fa:ptr fu:n lo:n hi:n :}
   PASS2-ENTRY  LIVE PATH !  0 FLOW# !  0 LX !  0 GROWN !  lo OPLO !  lo DI !
   begin DI @ hi < while
      fa fu hi PASS2-TOKEN
      DI @ 1+ DI !
   repeat ;

\ Back edges grow a label's state after its placement: walk until no label
\ grows, then once more to report.
: PASS2-DEF  ( ptr u8 n n n -- ) {: fa:ptr fu:n lo:n hi:n :}
   lo hi RET-IN-RANGE? TRACK-LR !
   lo hi COLLECT-LABELS
   -1 QUIET !
   begin fa fu lo hi PASS2-WALK  GROWN @ 0= until
   0 QUIET !
   fa fu lo hi PASS2-WALK ;
\ Pass 1 made one row per definition, in pass 2's order: after a definition,
\ the calls see its row.
: HOST-PASSED  ( -- )  BOUND# @ 1+ BOUND# ! ;

: PASS2-FILE ( ptr u8 n -- ) {: pa:ptr pu:n :}
   pa pu LINT-SOURCE:LOAD  LINT-SOURCE:TEXT TOKENIZE  FILE-START
   0 PKG-U ! 0 WI !
   begin WI @ TN# @ 1- < while
      WI @ TOK s" :" LINT-STR= if
         WI @ DEF-END WE !
         WI @ 1+ WORD-NAME!
         WI @ 2 + WE @ DEF-LOCALS
         pa pu  WI @ 2 + WE @ PASS2-DEF
         HOST-PASSED
         WE @ WI !
      else
         WI @ SCOPE-STEP WI !
      then
      WI @ 1+ WI !
   repeat  LSTATE-RELEASE ;

\ ---- driver ---------------------------------------------------------------
\ The engine's files in load order: a call binds an earlier definition, so
\ every host row precedes its callers.
: EACH-FILE ( [ ptr u8 n -- ] -- ) {: visit :}
   s" src/habu/rt.f" visit execute  s" src/habu/crash.f" visit execute
   s" src/habu/regalloc.f" visit execute  s" src/habu/habu1.f" visit execute
   s" src/habu/jit.f" visit execute  s" src/habu/prof.f" visit execute
   s" src/habu/habu2.f" visit execute ;

: FRESH-SCAN  ( -- )
   0 PARENS? !  0 CN# !  0 CEND !  0 EN# !  CLOBBER-CENSUS:RESET
   0 HOST# !  0 BOUND# !  0 DEF# !  0 NAME-CAP ! ;

: PASS2-START  ( -- )
   SEED# @ BOUND# !  0 BAD ! ;

\ Both passes over the files the enumerator visits, in its order; BAD and
\ the census hold the result.
: ANALYZE  ( [ [ ptr u8 n -- ] -- ] -- ) {: files :}
   FRESH-SCAN
   [: DECL-FILE ;] files execute
   HOSTS-OPEN
   [: PASS1-FILE ;] files execute
   CLOSE-CLOBBERS
   PASS2-START
   [: PASS2-FILE ;] files execute
   HOSTS-CLOSE ;

: CLOBBER-LINT  ( -- )
   [: EACH-FILE ;] ANALYZE
   CLOBBER-CENSUS:PROVE  CLOBBER-CENSUS:REPORT
   BAD @ 0 > if
      s" clobber-lint: " type BAD @ FMT:.INT s"  finding(s)" type NL
      s" clobber-lint: findings" 1 die
   else
      s" clobber-lint: clean" type NL
   then ;
public
: RUN ( -- ) CLOBBER-LINT ;
;package
