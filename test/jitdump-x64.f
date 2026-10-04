\ jitdump-x64.f - the live Intel JIT dump and its bounded byte decoder.
\ Run on a native x86-64 engine: bin/hb --load test/jitdump-x64.f

require lib/test.f
require test/gate-common.f
require src/arch/x86-64/disasm.f
require tools/jitdump-core.f

package JITDUMP-X64-TEST
private

\ Independent x86 encodings: REX.W + SIB/displacement, scalar float,
\ immediate containing RET, relative branch and the one-byte return.
create MEM-CODE $48 c, $8B c, $44 c, $8D c, $F8 c,
create LEA-CODE $48 c, $8D c, $44 c, $8D c, $F8 c,
create FP-CODE $F2 c, $0F c, $10 c, $C1 c,
create FP-ADD $F2 c, $0F c, $58 c, $C1 c,
create FP-CVT $F2 c, $48 c, $0F c, $2A c, $C1 c,
create IMM-CODE $48 c, $B8 c, $C3 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c,
create JMP-CODE $E9 c, $FB c, $FF c, $FF c, $FF c,
create RET-CODE $C3 c,
create WORD-CODE $66 c, $41 c, $89 c, $C0 c,
create WORD-ALU $66 c, $81 c, $C0 c, $34 c, $12 c,
create BYTE-REX $40 c, $88 c, $E0 c,
create BYTE-LEGACY $88 c, $E0 c,
create LOCK-CODE $F0 c, $48 c, $0F c, $C1 c, $08 c,
create F3-CODE $F3 c, $0F c, $1E c, $FA c,
create MOVZX-BYTE $48 c, $0F c, $B6 c, $00 c,
create MOVZX-WORD $48 c, $0F c, $B7 c, $00 c,
create MEM-IMM $48 c, $C7 c, $00 c, $01 c, 0 c, 0 c, 0 c,
create MEM-UNARY $48 c, $F7 c, $10 c,
create MEM-ALU $48 c, $81 c, $00 c, $01 c, 0 c, 0 c, 0 c,
create MEM-FF $48 c, $FF c, $00 c,
create REX-BEFORE-66 $48 c, $66 c, $89 c, $C0 c,
create WORD-CWD $66 c, $99 c,
create REX-NOP $41 c, $90 c,
create BAD-MODRM $48 c, $8B c,
create BAD-SIB $48 c, $8B c, $04 c,
create BAD-IMM $48 c, $B8 c, $01 c,
create BAD-OP $0F c, $FF c,

public
: JIT-IMM ( -- n ) $C3 ;
: JIT-EARLY ( n -- n ) dup 0= if drop 7 exit then drop 9 ;
: JIT-TAIL ( n -- n ) 1+ ;
: JIT-TAIL-WRAP ( n -- n ) JIT-TAIL ;
private

: CHECK-MEM ( -- )
   [: MEM-CODE 5 $1000 X64DIS:DIS1 5 T= ;] GE-CAPTURE-ACTION
   s" mov rax, qword ptr [rbp+rcx*4-8]" s" SIB address and REX width" GE-EXPECT-OUT-HAS
   GT-OUT$ type cr ;

: CHECK-MEM-WIDTH ( -- )
   [: MOVZX-BYTE 4 $1000 X64DIS:DIS1 4 T= ;] GE-CAPTURE-ACTION
   s" movzx rax, byte ptr [rax]" s" byte extension source" GE-EXPECT-OUT-HAS
   [: MOVZX-WORD 4 $1000 X64DIS:DIS1 4 T= ;] GE-CAPTURE-ACTION
   s" movzx rax, word ptr [rax]" s" word extension source" GE-EXPECT-OUT-HAS
   [: MEM-IMM 7 $1000 X64DIS:DIS1 7 T= ;] GE-CAPTURE-ACTION
   s" mov qword ptr [rax], 0x1" s" memory immediate width" GE-EXPECT-OUT-HAS
   [: MEM-UNARY 3 $1000 X64DIS:DIS1 3 T= ;] GE-CAPTURE-ACTION
   s" not qword ptr [rax]" s" memory unary width" GE-EXPECT-OUT-HAS
   [: MEM-ALU 7 $1000 X64DIS:DIS1 7 T= ;] GE-CAPTURE-ACTION
   s" add qword ptr [rax], 0x1" s" memory arithmetic width" GE-EXPECT-OUT-HAS
   [: MEM-FF 3 $1000 X64DIS:DIS1 3 T= ;] GE-CAPTURE-ACTION
   s" inc qword ptr [rax]" s" memory increment width" GE-EXPECT-OUT-HAS
   [: LEA-CODE 5 $1000 X64DIS:DIS1 5 T= ;] GE-CAPTURE-ACTION
   s" lea rax, [rbp+rcx*4-8]" s" address calculation has no memory width" GE-EXPECT-OUT-HAS ;

: CHECK-FP ( -- )
   [: FP-CODE 4 $2000 X64DIS:DIS1 4 T= ;] GE-CAPTURE-ACTION
   s" movsd xmm0, xmm1" s" scalar floating opcode" GE-EXPECT-OUT-HAS
   [: FP-ADD 4 $2000 X64DIS:DIS1 4 T= ;] GE-CAPTURE-ACTION
   s" addsd xmm0, xmm1" s" scalar floating arithmetic" GE-EXPECT-OUT-HAS
   [: FP-CVT 5 $2000 X64DIS:DIS1 5 T= ;] GE-CAPTURE-ACTION
   s" cvtsi2sd xmm0, rcx" s" scalar integer conversion" GE-EXPECT-OUT-HAS ;

: CHECK-PREFIX ( -- )
   [: WORD-CODE 4 $2500 X64DIS:DIS1 4 T= ;] GE-CAPTURE-ACTION
   s" mov r8w, ax" s" 66 and REX select the 16-bit registers" GE-EXPECT-OUT-HAS
   [: WORD-ALU 5 $2500 X64DIS:DIS1 5 T= ;] GE-CAPTURE-ACTION
   s" add ax, 0x1234" s" 66 shortens the group immediate" GE-EXPECT-OUT-HAS
   [: BYTE-REX 3 $2500 X64DIS:DIS1 3 T= ;] GE-CAPTURE-ACTION
   s" mov al, spl" s" REX-only byte register" GE-EXPECT-OUT-HAS
   [: LOCK-CODE 5 $2500 X64DIS:DIS1 5 T= ;] GE-CAPTURE-ACTION
   s" lock xadd qword ptr [rax], rcx" s" lock prefix keeps its operand" GE-EXPECT-OUT-HAS
   [: F3-CODE 4 $2500 X64DIS:DIS1 4 T= ;] GE-CAPTURE-ACTION
   s" endbr64" s" F3 mandatory prefix" GE-EXPECT-OUT-HAS
   [: REX-BEFORE-66 4 $2500 X64DIS:DIS1 4 T= ;] GE-CAPTURE-ACTION
   s" mov ax, ax" s" legacy prefix cancels earlier REX" GE-EXPECT-OUT-HAS
   [: WORD-CWD 2 $2500 X64DIS:DIS1 2 T= ;] GE-CAPTURE-ACTION
   s" cwd" s" operand prefix selects word sign extension" GE-EXPECT-OUT-HAS ;

: CHECK-IMM ( -- )
   [: IMM-CODE 10 $3000 X64DIS:DIS1 10 T= ;] GE-CAPTURE-ACTION
   s" mov rax, 0xc3" s" RET byte stays inside immediate" GE-EXPECT-OUT-HAS ;

: CHECK-CONTROL ( -- )
   [: JMP-CODE 5 $4000 X64DIS:DIS1 5 T= ;] GE-CAPTURE-ACTION
   s" jmp 0x4000" s" rel target uses PC after instruction" GE-EXPECT-OUT-HAS
   [: RET-CODE 1 $5000 X64DIS:DIS1 1 T= ;] GE-CAPTURE-ACTION
   s" ret" s" one-byte return" GE-EXPECT-OUT-HAS ;

: CHECK-REFUSALS ( -- )
   [: BAD-MODRM 2 $6000 X64DIS:DIS1 drop ;] X64DIS:E-TRUNCATED TTHROWSQ
   [: BAD-SIB 3 $6000 X64DIS:DIS1 drop ;] X64DIS:E-TRUNCATED TTHROWSQ
   [: BAD-IMM 3 $6000 X64DIS:DIS1 drop ;] X64DIS:E-TRUNCATED TTHROWSQ
   [: BYTE-LEGACY 2 $6000 X64DIS:DIS1 drop ;] X64DIS:E-UNKNOWN TTHROWSQ
   [: REX-NOP 2 $6000 X64DIS:DIS1 drop ;] X64DIS:E-UNKNOWN TTHROWSQ
   [: BAD-OP 2 $6000 X64DIS:DIS1 drop ;] X64DIS:E-UNKNOWN TTHROWSQ ;

: CHECK-LIVE ( -- )
   [: s" JIT-IMM" JITDUMP:JIT-FIND JITDUMP:JD ;] GE-CAPTURE-ACTION
   s" mov" s" live compiled immediate" GE-EXPECT-OUT-HAS
   s" ret" s" live compiled return" GE-EXPECT-OUT-HAS
   [: s" JIT-EARLY" JITDUMP:JIT-FIND JITDUMP:JD ;] GE-CAPTURE-ACTION
   s" 0x9" s" instructions after early return" GE-EXPECT-OUT-HAS
   [: s" JIT-TAIL-WRAP" JITDUMP:JIT-FIND JITDUMP:JD ;] GE-CAPTURE-ACTION
   s" jmp" s" complete tail branch" GE-EXPECT-OUT-HAS
   [: s" JITDUMP-X64-ALIAS:JIT-IMM" XREF-FIND XREF-START JITDUMP:JD ;]
      GE-CAPTURE-ACTION
   s" mov" s" exported alias shares the original code" GE-EXPECT-OUT-HAS ;

public
: RUN ( -- )
   T-RESET
   s" jitdump-x64" GT-START
   GE-HB-RESET
   CHECK-MEM CHECK-MEM-WIDTH CHECK-FP CHECK-PREFIX CHECK-IMM CHECK-CONTROL
   CHECK-REFUSALS CHECK-LIVE
   GT-CLEANUP
   T-REPORT ;
;package

package JITDUMP-X64-ALIAS
public
EXPORT JITDUMP-X64-TEST:JIT-IMM
;package

package JITDUMP-X64-TEST
public
RUN
;package
