\ parse-name walks the live source cursor, including its end-of-input answer.
\ tok-imm? follows the primary name binding and ignores used publics.
\ The emitted ELFs remain in HB_TMP as hb-x64-kernel-token and -tok-imm.
require test/x86-64-boot-harness.f
require src/os/linux-x86-64/target-layout.f

package X64K-TOKEN-TEST
using X64ASM
using X64CODE
using X64RT
using X64LAYOUT

: DATA-REG ( -- r64 ) ENGINE-GPR:X64-RBASE >R64 ;

: INPUT, ( ptr u8 n -- )
   X64HARNESS:PUSH-TEXT,
   RAX R64>N G-POP  RCX R64>N G-POP
   RCX DATA-REG X64HARNESS:SCRATCH-OFF MEM-OFF ASM-SINK ENC-MOV-MR
   RCX DATA-REG INP-CELL MEM-OFF ASM-SINK ENC-MOV-MR
   RDX RCX ASM-SINK ENC-MOV-RR
   RDX RAX ASM-SINK ENC-ADD-RR
   RDX DATA-REG INE-CELL MEM-OFF ASM-SINK ENC-MOV-MR ;

: NAME?, ( n n -- ) {: off:n len:n :}
   s" parse-name" X64HARNESS:CALL-ROW,
   len X64HARNESS:EXPECT-POP,
   RAX R64>N G-POP
   RCX DATA-REG X64HARNESS:SCRATCH-OFF MEM-OFF ASM-SINK ENC-MOV-RM
   RAX RCX ASM-SINK ENC-SUB-RR
   RAX R64>N G-PUSH
   off X64HARNESS:EXPECT-POP, ;

: IMM?, ( ptr u8 n n -- ) {: a:ptr u:n want:n :}
   a u X64HARNESS:PUSH-TEXT,
   s" tok-imm?" X64HARNESS:CALL-ROW,
   want X64HARNESS:EXPECT-POP, ;

: SEED-IMM, ( -- )
   s" Tok" 0 DNAME-IMM X64HARNESS:RECORD,
   s" Tok" 8 0 X64HARNESS:RECORD,
   s" Pub" 7 DNAME-IMM X64HARNESS:RECORD,
   s" Use" 9 DNAME-IMM X64HARNESS:RECORD,
   s" No" 0 0 X64HARNESS:RECORD,
   s" Pad" 0 0 X64HARNESS:RECORD,
   s" Pkg" DICT-WL:NAMESPACE 0 X64HARNESS:RECORD, ;

: BUILD-IMM ( -- )
   false X64HARNESS:BOOT-OPEN,
   SEED-IMM,
   s" tOk" 2 IMM?,
   7 PKG-PUB-CELL X64HARNESS:CELL!,
   8 PKG-PRI-CELL X64HARNESS:CELL!,
   s" Tok" 0 IMM?,
   s" Pub" 2 IMM?,
   s" Pkg:Pub" 2 IMM?,
   s" Pkg:Tok" 2 IMM?,
   0 PKG-PUB-CELL X64HARNESS:CELL!,
   0 PKG-PRI-CELL X64HARNESS:CELL!,
   s" Pkg:Tok" 0 IMM?,
   9 USE-WIDS-OFF X64HARNESS:CELL!,
   1 USE-DEPTH-CELL X64HARNESS:CELL!,
   s" Use" 0 IMM?,
   0 X64HARNESS:EXPECT-DEPTH,
   X64HARNESS:EXPECT-BALANCED,
   s" hb-x64-kernel-tok-imm" TMP-PATH X64HARNESS:BOOT-CLOSE, ;

: RUN ( -- )
   T-RESET
   X64HARNESS:INIT
   false X64HARNESS:BOOT-OPEN,
   s"  Alpha  beta " INPUT,
   1 5 NAME?,
   8 4 NAME?,
   13 0 NAME?,
   4 TKL-CELL X64HARNESS:EXPECT-CELL,
   0 X64HARNESS:EXPECT-DEPTH,
   X64HARNESS:EXPECT-BALANCED,
   s" hb-x64-kernel-token" TMP-PATH X64HARNESS:BOOT-CLOSE,
   BUILD-IMM
   X64HARNESS:DISPOSE
   T-REPORT ;

RUN

;using
;using
;using
;using
;package
