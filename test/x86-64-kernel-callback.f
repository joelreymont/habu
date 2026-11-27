\ x86-64-kernel-callback.f - emitted SysV callback-entry E2E.
\ In the qsort image libc calls a Habu comparator through ffi-call. The two
\ rejection images write an E-CALLBACK record on stderr that names slot 0:
\ unbound "C called a callback whose slot is not bound", no-frame "C called the
\ callback outside the foreign call that passed it".
require src/core/exit-status.f
require test/x86-64-boot-harness.f
require src/lib/vector.f
require src/arch/x86-64/rt.f
require src/habu/kernel-x64.f
require src/lib/test/assert.f
require test/x86-64-peer-harness.f
require src/arch/x86-64/asm.f
require src/arch/x86-64/icode.f

package X64K-CB
using X64ASM
using X64CODE
using X64RT

DATA-START $20000 + constant ARGS
ARGS 8 CELL * + constant VALUES
VALUES 3 CELL * + constant XTS
XTS CELL + constant ROWS
ROWS CB-ROW-BYTES + constant INNER-ARGS
INNER-ARGS 8 CELL * + constant INNER-VALUES
INNER-VALUES 2 CELL * + constant NEST-DEPTH
NEST-DEPTH CELL + constant QSORT-FN
QSORT-FN CELL + constant THUNK

: DATA-REG ( -- r64 ) ENGINE-GPR:X64-RBASE >R64 ;
: N, ( n -- ) X64HARNESS:PUSH, ;
: ROW ( str -- ) X64HARNESS:CALL-ROW, ;
: ARG ( n -- n ) CELL * ARGS + ;

\ The checked dispatch ABI reads C's two pointer arguments through CB-FRAME
\ and writes a signed three-way comparison into its first integer result slot.
: COMPARE, ( -- )
   X64CODE:LBL {: compare:label :}
   RAX 1 >IMM32 ASM-SINK ENC-MOV-RI32  0 G-PUSH
   0 G-POP
   RAX DATA-REG NEST-DEPTH MEM-OFF ASM-SINK ENC-MOV-RM
   RAX RAX ASM-SINK ENC-TEST-RR  C-NE compare JCC,
   RAX 1 >IMM32 ASM-SINK ENC-MOV-RI32
   RAX DATA-REG NEST-DEPTH MEM-OFF ASM-SINK ENC-MOV-MR
   RAX DATA-REG INNER-ARGS MEM-OFF ASM-SINK ENC-LEA  0 G-PUSH
   RAX 4 >IMM32 ASM-SINK ENC-MOV-RI32  0 G-PUSH
   RAX DATA-REG QSORT-FN MEM-OFF ASM-SINK ENC-MOV-RM  0 G-PUSH
   s" ffi-call" ROW  0 G-POP
   RAX 2 >IMM32 ASM-SINK ENC-MOV-RI32
   RAX DATA-REG NEST-DEPTH MEM-OFF ASM-SINK ENC-MOV-MR
   compare X64CODE:LBL,
   RAX DATA-REG CB-FRAME MEM-OFF ASM-SINK ENC-MOV-RM
   RCX RAX MEM-AT ASM-SINK ENC-MOV-RM
   RDX RAX CELL MEM-OFF ASM-SINK ENC-MOV-RM
   RCX RCX MEM-AT ASM-SINK ENC-MOV-RM
   RDX RDX MEM-AT ASM-SINK ENC-MOV-RM
   RCX RDX ASM-SINK ENC-SUB-RR
   RCX RAX MEM-AT ASM-SINK ENC-MOV-MR ;

: SYM, ( str -- ) {: s:str :}
   LBL LBL {: name:label past:label :}
   past JMP,
   name LBL,  s ASM-SINK VEC:APPEND
   0 ASM-SINK VEC:PUSH c!
   past LBL,
   RSI name MOVABS,  X64HARNESS:DLSYM,  0 G-PUSH ;

\ Leave in rax slot 0's C entry: mov r10, 0 and a jump to the thunk. C keeps its
\ own rbp, so the stub names the thunk's cell by its absolute address.
: STUB, ( -- )
   s" callback-entry" ROW  0 G-POP
   RAX DATA-REG THUNK MEM-OFF ASM-SINK ENC-MOV-MR
   [: R10 0 >IMM32 ASM-SINK ENC-MOV-RI32
      RAX LINUX-LAYOUT:DATA-VA THUNK + >IMM64 ASM-SINK ENC-MOV-RI64
      RAX RAX MEM-AT ASM-SINK ENC-MOV-RM
      RAX ASM-SINK ENC-JMP-REG ;] X64HARNESS:ROUTINE,
   RAX swap MOVABS, ;

: SORT-CASE ( -- )
   [: COMPARE, ;] X64HARNESS:ROUTINE, XTS X64HARNESS:LABEL-CELL!,
   XTS CB-XTS X64HARNESS:DATA-ADDR!,
   ROWS CB-ROWS X64HARNESS:DATA-ADDR!,
   0 ROWS X64HARNESS:DATA-ADDR!,
   3 VALUES X64HARNESS:CELL!,
   1 VALUES CELL + X64HARNESS:CELL!,
   2 VALUES 2 CELL * + X64HARNESS:CELL!,
   2 INNER-VALUES X64HARNESS:CELL!,
   1 INNER-VALUES CELL + X64HARNESS:CELL!,
   INNER-VALUES INNER-ARGS X64HARNESS:DATA-ADDR!,
   2 INNER-ARGS CELL + X64HARNESS:CELL!,
   CELL INNER-ARGS 2 CELL * + X64HARNESS:CELL!,
   VALUES 0 ARG X64HARNESS:DATA-ADDR!,
   3 1 ARG X64HARNESS:CELL!,
   CELL 2 ARG X64HARNESS:CELL!,
   STUB,
   RAX DATA-REG 3 ARG MEM-OFF ASM-SINK ENC-MOV-MR
   RAX DATA-REG INNER-ARGS 3 CELL * + MEM-OFF ASM-SINK ENC-MOV-MR
   s" qsort" SYM,  0 G-POP
   RAX DATA-REG QSORT-FN MEM-OFF ASM-SINK ENC-MOV-MR
   ARGS X64HARNESS:PUSH-DATA,  4 N,
   RAX DATA-REG QSORT-FN MEM-OFF ASM-SINK ENC-MOV-RM  0 G-PUSH
   s" ffi-call" ROW  0 G-POP
   1 VALUES X64HARNESS:EXPECT-CELL,
   2 VALUES CELL + X64HARNESS:EXPECT-CELL,
   3 VALUES 2 CELL * + X64HARNESS:EXPECT-CELL,
   1 INNER-VALUES X64HARNESS:EXPECT-CELL,
   2 INNER-VALUES CELL + X64HARNESS:EXPECT-CELL,
   0 CB-XDS X64HARNESS:EXPECT-CELL,
   0 CB-OWNER X64HARNESS:EXPECT-CELL,
   ;

: CALL-ENTRY, ( -- )
   EXIT-STATUS:CALLBACK X64HARNESS:EXPECT-EXIT
   STUB,  RAX ASM-SINK ENC-CALL-REG ;

: UNBOUND-CASE ( -- )
   ROWS CB-ROWS X64HARNESS:DATA-ADDR!,
   CALL-ENTRY, ;

: NO-FRAME-CASE ( -- )
   ROWS CB-ROWS X64HARNESS:DATA-ADDR!,
   0 ROWS X64HARNESS:DATA-ADDR!,
   CALL-ENTRY, ;

: BUILD ( [ -- ] str -- ) {: path:str :}
   false X64HARNESS:BOOT-OPEN,
   execute
   0 X64HARNESS:EXPECT-DEPTH,
   X64HARNESS:EXPECT-BALANCED,
   path X64HARNESS:BOOT-CLOSE, ;

public

: RUN ( -- )
   T-RESET X64HARNESS:INIT
   [: SORT-CASE ;] s" hb-x64-kernel-callback" TMP-PATH BUILD
   [: UNBOUND-CASE ;] s" hb-x64-kernel-callback-unbound" TMP-PATH BUILD
   [: NO-FRAME-CASE ;] s" hb-x64-kernel-callback-no-frame" TMP-PATH BUILD
   X64HARNESS:DISPOSE T-REPORT ;

;using
;using
;package

X64K-CB:RUN
