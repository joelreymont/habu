\ icode.fs — ICode: "assembly in Forth" (SwiftForth-style). Assembler mnemonics
\ append abstract instructions to an IR; an optimizer pass rewrites the IR; then
\ asm.fs encodes each instruction to ARM64 machine code. Registers/immediates are
\ IR operand FIELDS, not bit-holes — so optimization + register allocation work on
\ structured instructions (no byte-patching). This is the core asm layer. TRUSTED:.

\ --- op tags (dense, for table dispatch) ---
0 constant IOP-MOVZ
1 constant IOP-MOVK
2 constant IOP-MOVN
3 constant IOP-ADD
4 constant IOP-ADDI
5 constant IOP-SUB
6 constant IOP-MUL
7 constant IOP-RET
8 constant IOP-SVC
9 constant #IOPS

\ --- IR storage: records of 4 cells ( op a b c ); operand meaning is per-op ---
4    constant /ic
4096 constant MAX-IC
create ICBUF  MAX-IC /ic * cells allot
variable #IC

: ICODE-RESET ( -- )  0 #IC ! ;
: ic[]  ( idx -- addr )  /ic * cells ICBUF + ;
: i.op  ( idx -- op )  ic[] @ ;
: i.a   ( idx -- a )   ic[] cell+ @ ;
: i.b   ( idx -- b )   ic[] 2 cells + @ ;
: i.c   ( idx -- c )   ic[] 3 cells + @ ;

: ICODE, ( a b c op -- )                 \ append one instruction
   #IC @ MAX-IC >= abort" icode overflow"
   #IC @ ic[] dup >r !                   \ op -> +0
   r@ 3 cells + !                        \ c  -> +3
   r@ 2 cells + !                        \ b  -> +2
   r> cell+ !                            \ a  -> +1
   1 #IC +! ;

\ --- assembler mnemonics ( … -- ), trailing "," like SwiftForth/Forth assemblers
: MOVZ, ( rd imm16 -- )      0 IOP-MOVZ ICODE, ;
: MOVK, ( rd imm16 sh -- )   IOP-MOVK ICODE, ;
: MOVN, ( rd imm16 -- )      0 IOP-MOVN ICODE, ;
: ADD,  ( rd rn rm -- )      IOP-ADD  ICODE, ;
: ADDI, ( rd rn imm12 -- )   IOP-ADDI ICODE, ;
: SUB,  ( rd rn rm -- )      IOP-SUB  ICODE, ;
: MUL,  ( rd rn rm -- )      IOP-MUL  ICODE, ;
: RET,  ( -- )               0 0 0 IOP-RET ICODE, ;
: SVC,  ( imm16 -- )         0 0 IOP-SVC ICODE, ;
