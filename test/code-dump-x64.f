\ code-dump-x64.f - the live Intel code dump and its bounded byte decoder.
\ Needs a native x86-64 engine.

require src/lib/test/assert.f
require test/gate-common.f
require src/arch/x86-64/disasm.f
require src/habu/boot-cells.f
require src/habu/code-bytes.f
require src/lib/fmt.f
require tools/code-dump-core.f
require src/lib/test/runner.f

package CODE-DUMP-X64-TEST
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
create TRAP-CODE $CC c,
create PUSHF-CODE $9C c,
create IRET-CODE $48 c, $CF c,
create SS-CODE $8C c, $D0 c,
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
create PALIGNR-CODE $66 c, $45 c, $0F c, $3A c, $0F c, $C7 c, $04 c,
create REP-MOVSB $F3 c, $A4 c,
create BARE-MOVSB $A4 c,
create FS-LOAD $64 c, $48 c, $8B c, $04 c, $25 c, 0 c, 0 c, 0 c, 0 c,
create FS-LOAD-R12 $64 c, $4C c, $8B c, $24 c, $25 c, $F8 c, $FF c, $FF c, $FF c,
create FS-ADD $64 c, $48 c, $03 c, $04 c, $25 c, 0 c, 0 c, 0 c, 0 c,
create FS-REG $64 c, $48 c, $8B c, $C1 c,

public
: DUMP-IMM ( -- n ) $C3 ;
: DUMP-EARLY ( n -- n ) dup 0= if drop 7 exit then drop 9 ;
\ A branch keeps DUMP-TAIL from being a copy word, so the wrapper's call to it
\ is a tail jump rather than its steps copied in.
: DUMP-TAIL ( n -- n ) dup 0< if negate then ;
: DUMP-TAIL-WRAP ( n -- n ) DUMP-TAIL ;
private

: CHECK-MEM ( -- )
   [: MEM-CODE 5 >str $1000 X64DIS:DIS1 5 T= ;] GE-CAPTURE-ACTION
   s" mov rax, qword ptr [rbp+rcx*4-8]" s" SIB address and REX width" GE-EXPECT-OUT-HAS
   GT-OUT$ type cr ;

: CHECK-MEM-WIDTH ( -- )
   [: MOVZX-BYTE 4 >str $1000 X64DIS:DIS1 4 T= ;] GE-CAPTURE-ACTION
   s" movzx rax, byte ptr [rax]" s" byte extension source" GE-EXPECT-OUT-HAS
   [: MOVZX-WORD 4 >str $1000 X64DIS:DIS1 4 T= ;] GE-CAPTURE-ACTION
   s" movzx rax, word ptr [rax]" s" word extension source" GE-EXPECT-OUT-HAS
   [: MEM-IMM 7 >str $1000 X64DIS:DIS1 7 T= ;] GE-CAPTURE-ACTION
   s" mov qword ptr [rax], 0x1" s" memory immediate width" GE-EXPECT-OUT-HAS
   [: MEM-UNARY 3 >str $1000 X64DIS:DIS1 3 T= ;] GE-CAPTURE-ACTION
   s" not qword ptr [rax]" s" memory unary width" GE-EXPECT-OUT-HAS
   [: MEM-ALU 7 >str $1000 X64DIS:DIS1 7 T= ;] GE-CAPTURE-ACTION
   s" add qword ptr [rax], 0x1" s" memory arithmetic width" GE-EXPECT-OUT-HAS
   [: MEM-FF 3 >str $1000 X64DIS:DIS1 3 T= ;] GE-CAPTURE-ACTION
   s" inc qword ptr [rax]" s" memory increment width" GE-EXPECT-OUT-HAS
   [: LEA-CODE 5 >str $1000 X64DIS:DIS1 5 T= ;] GE-CAPTURE-ACTION
   s" lea rax, [rbp+rcx*4-8]" s" address calculation has no memory width" GE-EXPECT-OUT-HAS ;

: CHECK-FP ( -- )
   [: FP-CODE 4 >str $2000 X64DIS:DIS1 4 T= ;] GE-CAPTURE-ACTION
   s" movsd xmm0, xmm1" s" scalar floating opcode" GE-EXPECT-OUT-HAS
   [: FP-ADD 4 >str $2000 X64DIS:DIS1 4 T= ;] GE-CAPTURE-ACTION
   s" addsd xmm0, xmm1" s" scalar floating arithmetic" GE-EXPECT-OUT-HAS
   [: FP-CVT 5 >str $2000 X64DIS:DIS1 5 T= ;] GE-CAPTURE-ACTION
   s" cvtsi2sd xmm0, rcx" s" scalar integer conversion" GE-EXPECT-OUT-HAS
   [: PALIGNR-CODE 7 >str $2000 X64DIS:DIS1 7 T= ;] GE-CAPTURE-ACTION
   s" palignr xmm8, xmm15, 0x4" s" three-byte opcode and its immediate" GE-EXPECT-OUT-HAS ;

: CHECK-PREFIX ( -- )
   [: WORD-CODE 4 >str $2500 X64DIS:DIS1 4 T= ;] GE-CAPTURE-ACTION
   s" mov r8w, ax" s" 66 and REX select the 16-bit registers" GE-EXPECT-OUT-HAS
   [: WORD-ALU 5 >str $2500 X64DIS:DIS1 5 T= ;] GE-CAPTURE-ACTION
   s" add ax, 0x1234" s" 66 shortens the group immediate" GE-EXPECT-OUT-HAS
   [: BYTE-REX 3 >str $2500 X64DIS:DIS1 3 T= ;] GE-CAPTURE-ACTION
   s" mov al, spl" s" REX-only byte register" GE-EXPECT-OUT-HAS
   [: LOCK-CODE 5 >str $2500 X64DIS:DIS1 5 T= ;] GE-CAPTURE-ACTION
   s" lock xadd qword ptr [rax], rcx" s" lock prefix keeps its operand" GE-EXPECT-OUT-HAS
   [: F3-CODE 4 >str $2500 X64DIS:DIS1 4 T= ;] GE-CAPTURE-ACTION
   s" endbr64" s" F3 mandatory prefix" GE-EXPECT-OUT-HAS
   [: REX-BEFORE-66 4 >str $2500 X64DIS:DIS1 4 T= ;] GE-CAPTURE-ACTION
   s" mov ax, ax" s" legacy prefix cancels earlier REX" GE-EXPECT-OUT-HAS
   [: WORD-CWD 2 >str $2500 X64DIS:DIS1 2 T= ;] GE-CAPTURE-ACTION
   s" cwd" s" operand prefix selects word sign extension" GE-EXPECT-OUT-HAS
   [: REP-MOVSB 2 >str $2500 X64DIS:DIS1 2 T= ;] GE-CAPTURE-ACTION
   s" rep movsb" s" F3 repeats the byte copy" GE-EXPECT-OUT-HAS
   [: FS-LOAD 9 >str $2500 X64DIS:DIS1 9 T= ;] GE-CAPTURE-ACTION
   s" mov rax, qword ptr fs:[0]" s" fs: load from an absolute disp32" GE-EXPECT-OUT-HAS
   [: FS-LOAD-R12 9 >str $2500 X64DIS:DIS1 9 T= ;] GE-CAPTURE-ACTION
   s" mov r12, qword ptr fs:[-8]" s" fs: load into a REX register" GE-EXPECT-OUT-HAS ;

: CHECK-IMM ( -- )
   [: IMM-CODE 10 >str $3000 X64DIS:DIS1 10 T= ;] GE-CAPTURE-ACTION
   s" mov rax, 0xc3" s" RET byte stays inside immediate" GE-EXPECT-OUT-HAS ;

: CHECK-CONTROL ( -- )
   [: JMP-CODE 5 >str $4000 X64DIS:DIS1 5 T= ;] GE-CAPTURE-ACTION
   s" jmp 0x4000" s" rel target uses PC after instruction" GE-EXPECT-OUT-HAS
   [: RET-CODE 1 >str $5000 X64DIS:DIS1 1 T= ;] GE-CAPTURE-ACTION
   s" ret" s" one-byte return" GE-EXPECT-OUT-HAS
   [: TRAP-CODE 1 >str $5000 X64DIS:DIS1 1 T= ;] GE-CAPTURE-ACTION
   s" int3" s" slot padding after code" GE-EXPECT-OUT-HAS
   [: PUSHF-CODE 1 >str $5000 X64DIS:DIS1 1 T= ;] GE-CAPTURE-ACTION
   s" pushfq" s" flags pushed for a serializing iretq" GE-EXPECT-OUT-HAS
   [: IRET-CODE 2 >str $5000 X64DIS:DIS1 2 T= ;] GE-CAPTURE-ACTION
   s" iretq" s" REX.W return from a serializing frame" GE-EXPECT-OUT-HAS
   [: SS-CODE 2 >str $5000 X64DIS:DIS1 2 T= ;] GE-CAPTURE-ACTION
   s" mov eax, ss" s" stack segment read" GE-EXPECT-OUT-HAS ;

: CHECK-REJECTIONS ( -- )
   [: BAD-MODRM 2 >str $6000 X64DIS:DIS1 drop ;] X64DIS:E-TRUNCATED TTHROWSQ
   [: BAD-SIB 3 >str $6000 X64DIS:DIS1 drop ;] X64DIS:E-TRUNCATED TTHROWSQ
   [: BAD-IMM 3 >str $6000 X64DIS:DIS1 drop ;] X64DIS:E-TRUNCATED TTHROWSQ
   [: BYTE-LEGACY 2 >str $6000 X64DIS:DIS1 drop ;] X64DIS:E-UNKNOWN TTHROWSQ
   [: REX-NOP 2 >str $6000 X64DIS:DIS1 drop ;] X64DIS:E-UNKNOWN TTHROWSQ
   [: BAD-OP 2 >str $6000 X64DIS:DIS1 drop ;] X64DIS:E-UNKNOWN TTHROWSQ
   [: BARE-MOVSB 1 >str $6000 X64DIS:DIS1 drop ;] X64DIS:E-UNKNOWN TTHROWSQ
   [: FS-ADD 9 >str $6000 X64DIS:DIS1 drop ;] X64DIS:E-UNKNOWN TTHROWSQ
   [: FS-REG 4 >str $6000 X64DIS:DIS1 drop ;] X64DIS:E-UNKNOWN TTHROWSQ ;

: CHECK-LIVE ( -- )
   [: s" DUMP-IMM" CODE-DUMP:FIND-XT CODE-DUMP:DECODE ;] GE-CAPTURE-ACTION
   s" live copy word" GE-EXPECT-OK
   s" mov" s" live compiled immediate" GE-EXPECT-OUT-HAS
   s" ret" s" live compiled return" GE-EXPECT-OUT-HAS
   [: s" DUMP-EARLY" CODE-DUMP:FIND-XT CODE-DUMP:DECODE ;] GE-CAPTURE-ACTION
   s" 0x9" s" instructions after early return" GE-EXPECT-OUT-HAS
   [: s" DUMP-TAIL-WRAP" CODE-DUMP:FIND-XT CODE-DUMP:DECODE ;] GE-CAPTURE-ACTION
   s" jmp" s" complete tail branch" GE-EXPECT-OUT-HAS
   [: s" CODE-DUMP-X64-ALIAS:DUMP-IMM" XREF-FIND XREF-START CODE-DUMP:DECODE ;]
      GE-CAPTURE-ACTION
   s" mov" s" exported alias shares the original code" GE-EXPECT-OUT-HAS ;

\ A kernel record lies in the engine's text, below dbase@.
: KERNEL-REC? ( ^n -- bool ) {: rec:^ :}
   rec XREF-RETIRED? if false exit then
   rec XREF-WORDLIST BOOT:NAMESPACE-WL = if false exit then
   rec XREF-CODE-BYTES 0 > rec XREF-START dbase@ < and ;

: REC-BYTES ( ^n -- str ) {: rec:^ :}
   rec XREF-START rec XREF-CODE-BYTES CODE-BYTES:AT ;

\ The offset of the instruction SPAN-ALL decodes, kept when SPAN throws.
variable SPAN-AT

: SPAN-ALL ( str -- ) {: s:str :}
   0 begin dup s STR:LEN < while
      {: off:n :}
      off SPAN-AT !
      s STR:DATA off + s STR:LEN off - >str X64DIS:SPAN drop off +
   repeat drop ;

: REC-DECODES? ( ^n -- bool ) {: rec:^ :}
   [: rec REC-BYTES SPAN-ALL ;] catch 0= ;

\ The record's name, the offset where decoding stops and the bytes there, at
\ most an instruction's 15.
: UNDECODED$ ( ^n -- str ) {: rec:^ :}
   rec REC-BYTES {: s:str :}
   SB-RESET rec XREF-NAME$ SB-APPEND s"  +" SB-APPEND
   SPAN-AT @ FMT:INT$ SB-APPEND s" : " SB-APPEND
   s STR:LEN SPAN-AT @ - 15 min 0 ?do
      s STR:DATA SPAN-AT @ + i + c@ 2 FMT:HEX-WIDTH$ SB-APPEND
   loop
   SB$ ;

\ The first kernel record whose bytes are not all instructions, empty when
\ every one decodes: data inside a record does not.
: FIRST-UNDECODED ( -- str )
   ndict@ 0 ?do
      i XREF-REC {: rec:^ :}
      rec KERNEL-REC? if
         rec REC-DECODES? 0= if rec UNDECODED$ unloop exit then
      then
   loop
   s" " ;

: CHECK-RECORDS ( -- )
   FIRST-UNDECODED s" " T$= ;

public
: RUN ( -- )
   T-RESET
   s" code-dump-x64" GT-START
   GE-HB-RESET
   CHECK-MEM CHECK-MEM-WIDTH CHECK-FP CHECK-PREFIX CHECK-IMM CHECK-CONTROL
   CHECK-REJECTIONS CHECK-LIVE CHECK-RECORDS
   GT-CLEANUP
   T-REPORT ;
;package

package CODE-DUMP-X64-ALIAS
public
EXPORT CODE-DUMP-X64-TEST:DUMP-IMM
;package

package CODE-DUMP-X64-TEST
public
RUN
;package
