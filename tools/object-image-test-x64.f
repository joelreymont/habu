\ x86-64 exit text for the object-image fixture: tools/object-image-test.f
\ loads it for an x86-64 target.
require src/lib/object.f
require src/lib/byte-buffer.f
require src/arch/x86-64/asm.f
require src/os/linux-x86-64/sys.f

package OBJIMG-TEST

create EXIT-SINK BUF:HDR-BYTES allot

\ The sink lives from EXIT-CODE to the CODE-TEXT+ that hands its bytes over.
: EXIT-CODE ( -- )
   EXIT-SINK 32 BUF:N>BLEN BUF:INIT
   X64ASM:RDI X64ASM:RDI EXIT-SINK X64ASM:ENC-XOR-RR
   X64ASM:RAX NR-EXIT-GROUP X64ASM:>IMM32 EXIT-SINK X64ASM:ENC-MOV-RI32
   EXIT-SINK X64ASM:ENC-SYSCALL ;

\ Bytes after the exit stub, which never runs them.
: CODE-PAD ( ptr u8 n -- )
   BUF:N>BLEN EXIT-SINK BUF:APPEND-SPAN ;

: CODE-TEXT+ ( -- )
   EXIT-SINK BUF:SPAN$ BUF:BLEN>N OBJ:TEXT+
   EXIT-SINK BUF:DISPOSE ;

;package
