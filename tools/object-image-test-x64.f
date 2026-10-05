\ x86-64 exit text for the object-image fixture.
require lib/object.f
require lib/byte-buffer.f
require src/arch/x86-64/asm.f
require src/os/linux-x86-64/sys.f

package OBJIMG-TEST

create EXIT-SINK BUF:HDR-BYTES allot

: EXIT-TEXT ( -- )
   EXIT-SINK 32 BUF:N>BLEN BUF:INIT
   X64ASM:RDI X64ASM:RDI EXIT-SINK X64ASM:ENC-XOR-RR
   X64ASM:RAX NR-EXIT-GROUP X64ASM:>IMM32 EXIT-SINK X64ASM:ENC-MOV-RI32
   EXIT-SINK X64ASM:ENC-SYSCALL
   EXIT-SINK BUF:SPAN$ BUF:BLEN>N OBJ:TEXT+
   EXIT-SINK BUF:DISPOSE ;

;package
