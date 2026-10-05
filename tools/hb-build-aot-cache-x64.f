\ x86-64 exit object for the stored-object cache hit fixture.
require tools/hb-build-test-lib.f
require lib/byte-buffer.f
require src/arch/x86-64/asm.f
require src/os/linux-x86-64/sys.f

package HB-BUILD-CLI

create HBT-EXIT-SINK BUF:HDR-BYTES allot

: HBT-ADD-EXIT-TEXT ( -- )
   HBT-EXIT-SINK 32 BUF:N>BLEN BUF:INIT
   X64ASM:RDI X64ASM:RDI HBT-EXIT-SINK X64ASM:ENC-XOR-RR
   X64ASM:RAX NR-EXIT-GROUP X64ASM:>IMM32 HBT-EXIT-SINK X64ASM:ENC-MOV-RI32
   HBT-EXIT-SINK X64ASM:ENC-SYSCALL
   HBT-EXIT-SINK BUF:SPAN$ BUF:BLEN>N OBJ:TEXT+
   HBT-EXIT-SINK BUF:DISPOSE ;

;package
