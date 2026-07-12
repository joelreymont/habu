\ icode.fs — ICode: SwiftForth-style "assembly in Forth". Mnemonics append
\ abstract instructions (5-cell records: op a b c d) to an IR buffer; opt.fs
\ rewrites the IR (peephole); asm.fs encodes it to ARM64 machine code.
\ Registers/immediates/labels are record FIELDS — optimization and register
\ allocation rewrite structured instructions, never patch bytes. TRUSTED:.

s" cg: icode overflow" exception constant E-IC-OVERFLOW
s" cg: label overflow" exception constant E-LBL-OVERFLOW

\ --- op tags (dense; asm.fs encoder/length tables index by these) ---
variable IOP#  0 IOP# !

: IOP: ( "name" -- )  IOP# @ constant  1 IOP# +! ;
IOP: IOP-MOVZ  IOP: IOP-MOVK  IOP: IOP-MOVN  IOP: IOP-MOV   IOP: IOP-LIT
IOP: IOP-ADD   IOP: IOP-ADDI  IOP: IOP-SUB   IOP: IOP-SUBI
IOP: IOP-MUL   IOP: IOP-SDIV  IOP: IOP-UDIV
IOP: IOP-AND   IOP: IOP-ORR   IOP: IOP-EOR
IOP: IOP-LSLI  IOP: IOP-LSRI  IOP: IOP-ASRI
IOP: IOP-LSLV  IOP: IOP-LSRV  IOP: IOP-ASRV
IOP: IOP-CMP   IOP: IOP-CMPI  IOP: IOP-CSET
IOP: IOP-B     IOP: IOP-BL    IOP: IOP-BCOND IOP: IOP-CBZ   IOP: IOP-CBNZ
IOP: IOP-BR    IOP: IOP-BLR   IOP: IOP-RET   IOP: IOP-ADR
IOP: IOP-LDR   IOP: IOP-STR   IOP: IOP-LDRB  IOP: IOP-STRB
IOP: IOP-LDRPO IOP: IOP-STRPR IOP: IOP-LDPPO IOP: IOP-STPPR
IOP: IOP-SVC   IOP: IOP-NOP   IOP: IOP-ICIV  IOP: IOP-DSB   IOP: IOP-ISB
IOP: IOP-DCCV
IOP: IOP-LABEL IOP: IOP-DEAD  IOP: IOP-BYTES IOP: IOP-DCQ  IOP: IOP-DLBL
IOP: IOP-LDRW  IOP: IOP-STRW  IOP: IOP-BRK
IOP: IOP-ANDI  IOP: IOP-ORRI  IOP: IOP-EORI
IOP: IOP-FMOVXD IOP: IOP-FMOVDX IOP: IOP-FMOVDD
IOP: IOP-FADD IOP: IOP-FSUB IOP: IOP-FMUL IOP: IOP-FDIV
IOP: IOP-FNEG  IOP: IOP-FABS  IOP: IOP-FSQRT IOP: IOP-FCMP IOP: IOP-FCMP0
IOP: IOP-SCVTF IOP: IOP-FCVTZS
IOP# @ constant #IOPS

\ --- condition codes (B.cond / CSET) ---
 0 constant C-EQ   1 constant C-NE   2 constant C-CS   3 constant C-CC
 4 constant C-MI   5 constant C-PL   6 constant C-VS   7 constant C-VC
 8 constant C-HI   9 constant C-LS  10 constant C-GE  11 constant C-LT
12 constant C-GT  13 constant C-LE  14 constant C-AL

\ --- IR record storage ---
5    constant /IC
16384 constant IC-INIT
-1 1 rshift /IC / cell / 2 / constant IC-GROW-MAX
variable IC-CAP
variable ICBUF
variable #IC

: IC-BYTES ( n -- n )  /IC * cells ;

IC-INIT IC-CAP !
IC-INIT IC-BYTES allocate throw ICBUF !

: IC-GROW ( -- )
   IC-CAP @ dup IC-GROW-MAX > if drop E-IC-OVERFLOW throw then
   2* dup IC-BYTES ICBUF @ swap resize throw ICBUF !
   IC-CAP ! ;

: IC-ADDR ( i -- addr )  /IC * cells ICBUF @ + ;

: IC-OP ( i -- op )  IC-ADDR @ ;

: IC-A  ( i -- a )   IC-ADDR cell+ @ ;

: IC-B  ( i -- b )   IC-ADDR 2 cells + @ ;

: IC-C  ( i -- c )   IC-ADDR 3 cells + @ ;

: IC-D  ( i -- d )   IC-ADDR 4 cells + @ ;

: IC, ( a b c d op -- )
   #IC @ IC-CAP @ >= if IC-GROW then
   #IC @ IC-ADDR >r
   r@ !  r@ 4 cells + !  r@ 3 cells + !  r@ 2 cells + !  r> cell+ !
   1 #IC +! ;

\ --- labels (positions bound by asm.fs PASS1; -1 = unplaced) ---
2048 constant MAX-LBL
create LBLPOS MAX-LBL cells allot
variable #LBL

: LBL ( -- lbl )  #LBL @ dup MAX-LBL >= if E-LBL-OVERFLOW throw then  1 #LBL +! ;

: LBL, ( lbl -- )  0 0 0 IOP-LABEL IC, ;

: ICODE-RESET ( -- )
   0 #IC !  0 #LBL !
   MAX-LBL 0 ?do  -1 LBLPOS i cells + !  loop ;

\ --- logical-immediate encoding (encodeBitMasks: plain mask -> N:immr:imms) ---
\ A valid mask is a power-of-2-sized repeating element (2..64 bits) that is a
\ rotated contiguous run of ones. 0 and all-ones are not encodable.
s" cg: bad logical immediate" exception constant E-BADLIMM

: POPC64 ( x -- n )   \ population count
   0 swap  begin dup while  swap over 1 and +  swap 1 rshift  repeat  drop ;

: EMASK ( e -- mask )   \ low-e-bits mask; ARM lshift wraps at 64, so special-case
   dup 64 = if drop -1 else 1 swap lshift 1- then ;

: RORE {: x r e -- x' :}   \ rotate x right by r within an e-bit element
   e EMASK {: m :}
   x m and {: xm :}
   xm r rshift  xm e r - lshift  or  m and ;

: HALVES= {: x e -- f :}   \ low and high halves of the e-bit x equal?
   e 2/ {: h :}  1 h lshift 1- {: m :}
   x m and  x h rshift m and  = ;

: LELEM {: x -- elem e :}   \ smallest power-of-2 repeating element of x
   64 {: e :}
   begin  e 2 >  x e HALVES=  and  while  e 2/ to e  repeat
   x e EMASK and  e ;

: LROT {: elem ones e -- r | -1 :}   \ r such that ROR(ones-run, r, e) = elem
   1 ones lshift 1- {: run :}
   e 0 ?do  run i e RORE elem = if  i unloop exit  then  loop  -1 ;

: LIMM-PACK {: r ones e -- nis :}   \ pack N<<12 | immr<<6 | imms
   e 64 = if $1000 else 0 then
   r 6 lshift or
   e 2* 1- 63 and 63 xor  ones 1- or  or ;

: ENC-LOGIMM {: x -- nis true | x false :}
   x 0=  x -1 =  or if  x false exit  then
   x LELEM {: elem e :}
   elem POPC64 {: ones :}
   ones e = if  x false exit  then
   elem ones e LROT dup 0< if  drop x false exit  then
   ones e LIMM-PACK  true ;

: >LIMM ( mask -- nis )  ENC-LOGIMM 0= if E-BADLIMM throw then ;

\ --- mnemonics ( … -- ); register operands are X-register numbers 0..31 ---
: MOVZ, ( rd imm16 -- )    0 0 IOP-MOVZ IC, ;

: MOVK, ( rd imm16 sh -- ) 0 IOP-MOVK IC, ;     \ sh in {0,16,32,48}

: MOVN, ( rd imm16 -- )    0 0 IOP-MOVN IC, ;

: MOV,  ( rd rm -- )       0 0 IOP-MOV IC, ;

: LIT64,  ( rd x -- )        0 0 IOP-LIT IC, ;    \ 64-bit const, minimal sequence

: ADD,  ( rd rn rm -- )    0 IOP-ADD IC, ;

: ADDI, ( rd rn imm12 -- ) 0 IOP-ADDI IC, ;

: SUB,  ( rd rn rm -- )    0 IOP-SUB IC, ;

: SUBI, ( rd rn imm12 -- ) 0 IOP-SUBI IC, ;

: MUL,  ( rd rn rm -- )    0 IOP-MUL IC, ;

: SDIV, ( rd rn rm -- )    0 IOP-SDIV IC, ;

: UDIV, ( rd rn rm -- )    0 IOP-UDIV IC, ;

: AND,  ( rd rn rm -- )    0 IOP-AND IC, ;

: ORR,  ( rd rn rm -- )    0 IOP-ORR IC, ;

: EOR,  ( rd rn rm -- )    0 IOP-EOR IC, ;

: ANDI, ( rd rn mask -- )  >LIMM 0 IOP-ANDI IC, ;   \ plain mask, encoded here

: ORRI, ( rd rn mask -- )  >LIMM 0 IOP-ORRI IC, ;

: EORI, ( rd rn mask -- )  >LIMM 0 IOP-EORI IC, ;

: LSLI, ( rd rn sh -- )    0 IOP-LSLI IC, ;

: LSRI, ( rd rn sh -- )    0 IOP-LSRI IC, ;

: ASRI, ( rd rn sh -- )    0 IOP-ASRI IC, ;

: LSLV, ( rd rn rm -- )    0 IOP-LSLV IC, ;

: LSRV, ( rd rn rm -- )    0 IOP-LSRV IC, ;

: ASRV, ( rd rn rm -- )    0 IOP-ASRV IC, ;

: CMP,  ( rn rm -- )       0 0 IOP-CMP IC, ;

: CMPI, ( rn imm12 -- )    0 0 IOP-CMPI IC, ;

: CSET, ( rd cond -- )     0 0 IOP-CSET IC, ;

: B,    ( lbl -- )         0 0 0 IOP-B IC, ;

: BL,   ( lbl -- )         0 0 0 IOP-BL IC, ;

: BCOND, ( cond lbl -- )   swap 0 0 IOP-BCOND IC, ;

: CBZ,  ( rt lbl -- )      swap 0 0 IOP-CBZ IC, ;

: CBNZ, ( rt lbl -- )      swap 0 0 IOP-CBNZ IC, ;

: BR,   ( rn -- )          0 0 0 IOP-BR IC, ;

: BLR,  ( rn -- )          0 0 0 IOP-BLR IC, ;

: RET,  ( -- )             0 0 0 0 IOP-RET IC, ;

: ADR,  ( rd lbl -- )      swap 0 0 IOP-ADR IC, ;

: LDR,  ( rt rn off -- )   0 IOP-LDR IC, ;      \ off: 0..32760, 8-aligned

: STR,  ( rt rn off -- )   0 IOP-STR IC, ;

: LDRB, ( rt rn off -- )   0 IOP-LDRB IC, ;     \ off: 0..4095

: STRB, ( rt rn off -- )   0 IOP-STRB IC, ;

: LDRW, ( rt rn off -- )   0 IOP-LDRW IC, ;     \ 32-bit, off 0..16380 (/4)

: STRW, ( rt rn off -- )   0 IOP-STRW IC, ;

: LDR-POST, ( rt rn off -- ) 0 IOP-LDRPO IC, ;  \ off: -256..255

: STR-PRE,  ( rt rn off -- ) 0 IOP-STRPR IC, ;

: LDP-POST, ( rt1 rt2 rn off -- ) IOP-LDPPO IC, ; \ off: -512..504, 8-aligned

: STP-PRE,  ( rt1 rt2 rn off -- ) IOP-STPPR IC, ;

: SVC,  ( imm16 -- )       0 0 0 IOP-SVC IC, ;

: BRK,  ( -- )             0 0 0 0 IOP-BRK IC, ;   \ trap (e.g. on divide-by-zero)

\ floating point (D-registers): operands are register numbers 0..31 of the FP regs
: FMOVXD, ( Dd Xn -- )     0 0 IOP-FMOVXD IC, ;    \ fmov Dd, Xn (bits GPR->FP)

: FMOVDX, ( Xd Dn -- )     0 0 IOP-FMOVDX IC, ;    \ fmov Xd, Dn (bits FP->GPR)

: FMOVDD, ( Dd Dn -- )     0 0 IOP-FMOVDD IC, ;    \ fmov Dd, Dn (FP reg-reg copy)

: FADD,   ( Dd Dn Dm -- )  0 IOP-FADD IC, ;

: FSUB,   ( Dd Dn Dm -- )  0 IOP-FSUB IC, ;

: FMUL,   ( Dd Dn Dm -- )  0 IOP-FMUL IC, ;

: FDIV,   ( Dd Dn Dm -- )  0 IOP-FDIV IC, ;

: FNEG,   ( Dd Dn -- )     0 0 IOP-FNEG IC, ;

: FABS,   ( Dd Dn -- )     0 0 IOP-FABS IC, ;

: FSQRT,  ( Dd Dn -- )     0 0 IOP-FSQRT IC, ;

: FCMP,   ( Dn Dm -- )     0 0 IOP-FCMP IC, ;

: FCMP0,  ( Dn -- )        0 0 0 IOP-FCMP0 IC, ;   \ fcmp Dn, #0.0

: SCVTF,  ( Dd Xn -- )     0 0 IOP-SCVTF IC, ;     \ signed int -> double

: FCVTZS, ( Xd Dn -- )     0 0 IOP-FCVTZS IC, ;    \ double -> signed int (truncate)

: NOP,  ( -- )             0 0 0 0 IOP-NOP IC, ;

: BYTES, ( addr u -- )     0 0 IOP-BYTES IC, ;   \ embed raw bytes (padded to 4)

: DCQ,  ( x -- )           0 0 0 IOP-DCQ IC, ;    \ embed one 64-bit cell (8 bytes)

: DLBL, ( lbl -- )         0 0 0 IOP-DLBL IC, ;   \ embed cell = label's byte offset
\ Fuse a register-operand shift onto the LAST-emitted ALU op (ADD/SUB/AND/ORR/EOR):
\ rewrites its IC-D so the encoder emits e.g. `EOR rd,rn,rm,LSL #k`. d=0 (the
\ default) means LSL #0 = no shift, so unshifted ops are unaffected.
0 constant SH-LSL   1 constant SH-LSR   2 constant SH-ASR

: SHIFT, ( shtype shamt -- )  swap 6 lshift or  #IC @ 1- IC-ADDR 4 cells + ! ;

: ICIVAU, ( rt -- )        0 0 0 IOP-ICIV IC, ;

: DCCVAU, ( rt -- )        0 0 0 IOP-DCCV IC, ;   \ clean dcache to PoU (JIT coherency)

: DSB-ISH, ( -- )          0 0 0 0 IOP-DSB IC, ;

: ISB,  ( -- )             0 0 0 0 IOP-ISB IC, ;
