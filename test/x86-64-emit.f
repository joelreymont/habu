\ x86-64-emit.f - the linux-x86-64 seam's instruction emitters, run on aarch64.
\
\ src/os/linux-x86-64/{sys,proc-watch,proc-control}.f are ordinary Habu: they
\ append bytes through package X64ASM, so an aarch64 engine can run them and read
\ back exactly what an x86_64 machine would execute. Every case below pins those
\ bytes to a fixed string, and the `llvm-mc:` comment above each one is the
\ source line that produced it - the same discipline, and the same LLVM 22.1.8 on
\ 2026-09-18, as test/compiler/x86-64-asm.f.
\
\ THE TWO COLLABORATORS THIS FILE SUPPLIES. The seam names ASM-SINK, the byte
\ buffer the code stream being emitted appends into, and G-POP / G-PUSH, the
\ engine's stack moves. Both belong to the x86-64 code layer and the x86_64
\ engine body, which habu-cross-build-the-d25a959d writes. ASM-SINK is bound here
\ to a test-owned buffer, which is what makes the bytes readable; G-POP and
\ G-PUSH emit nothing and only record the register they were handed, so a case
\ can check the argument ORDER the process primitives build - the substance of
\ proc-control.f - without claiming to have run code that does not exist.
\
\ WHAT IT CANNOT PROVE. No x86_64 instruction has ever executed. In particular
\ the carry polarity SYS, is built around is pinned as bytes and argued in
\ src/os/linux-x86-64/sys.f, not observed: that waits on the cross-built engine.
\
\ Its own suite, because it loads the x86-64 seam's sys.f, which spells the same
\ syscall-number words as the host's and cannot share a process with it.

require lib/test.f
require lib/byte-buffer.f
require lib/errors.f
require src/arch/x86-64/asm.f

\ ---- the collaborators the seam is written against ---------------------------
create SINK BUF:HDR-BYTES allot

: ASM-SINK ( -- ptr u8 )  SINK ;

16 constant G-LOG-CAP
create G-LOG G-LOG-CAP allot
variable G-LOG-N

: G-LOG! ( n -- )
   G-LOG-N @ G-LOG-CAP >= if E-BUF-BOUNDS throw then
   G-LOG G-LOG-N @ + c!
   G-LOG-N @ 1 + G-LOG-N ! ;

: G-POP ( n -- )   G-LOG! ;
: G-PUSH ( n -- )  G-LOG! ;

s" src/os/linux-x86-64/sys.f" required
s" src/os/linux-x86-64/proc-watch.f" required
s" src/os/linux-x86-64/proc-control.f" required

\ The span reader answers a role; a byte-count comparison takes a raw cell.
\ Projection out of a cell family needs no ownership, so no reopen of NUM
\ (test/compiler/x86-64-asm.f takes the same step for the same reason).
CAST: X64E-BL>RAW ( NUM:byte-len -- n )

package X64-EMIT-TEST
private
using X64ASM

: N>BLEN ( n -- NUM:byte-len )
   NUM:BYTE-LEN
   MATCH NUM:numeric-result
      ok OF ENDOF                                negative OF E-BUF-BOUNDS throw ENDOF
      zero OF E-BUF-BOUNDS throw ENDOF           overflow OF E-BUF-BOUNDS throw ENDOF
      underflow OF E-BUF-BOUNDS throw ENDOF      bad-alignment OF E-BUF-BOUNDS throw ENDOF
      misaligned OF E-BUF-BOUNDS throw ENDOF
   ;MATCH ;

\ ---- comparing a byte span against its expected string -----------------------
\ A malformed expected string is a defect in this suite's own data, so it dies
\ rather than comparing against a wrong byte, exactly as the encoder suite does.
: HEX-DIGIT ( n -- n ) {: c:n :}
   c 48 >= c 57 <= and if c 48 - exit then
   c 97 >= c 102 <= and 0= if FMATH:E-DOMAIN throw then
   c 97 - 10 + ;

: HEX-BYTE ( ptr u8 n -- n ) {: a:ptr i:n :}
   a i 2 * + c@ HEX-DIGIT 4 lshift
   a i 2 * 1 + + c@ HEX-DIGIT or ;

: SPAN=HEX? ( ptr u8 n ptr u8 n -- bool ) {: da:ptr dlen:n ea:ptr eu:n :}
   eu 2 mod 0<> if FMATH:E-DOMAIN throw then
   dlen eu 2 / <> if false exit then
   dlen 0 ?do
      ea i HEX-BYTE da i + c@ <> if false unloop exit then
   loop
   true ;

: SPAN=? ( ptr u8 n ptr u8 n -- bool ) {: aa:ptr au:n ba:ptr bu:n :}
   au bu <> if false exit then
   au 0 ?do
      aa i + c@ ba i + c@ <> if false unloop exit then
   loop
   true ;

: SINK$ ( -- ptr u8 n )
   SINK BUF:SPAN$ X64E-BL>RAW ;

\ Compare what the seam just emitted against the expected string, then empty the
\ sink and the register log for the next case.
: X= ( ptr u8 n -- ) {: ea:ptr eu:n :}
   SINK$ ea eu SPAN=HEX? TTRUE
   SINK BUF:CLEAR
   0 G-LOG-N ! ;

\ Compare a span the seam publishes as data (a stencil) against its string.
: S= ( ptr u8 n ptr u8 n -- ) {: da:ptr dlen:n ea:ptr eu:n :}
   da dlen ea eu SPAN=HEX? TTRUE ;

\ A translator emits a dozen instructions, so its expectation is accumulated one
\ instruction at a time and each piece sits under the llvm-mc line that produced
\ it, rather than as one unreadable string.
256 constant WANT-CAP
create WANT WANT-CAP allot
variable WANT-N

: E+ ( ptr u8 n -- ) {: ea:ptr eu:n :}
   eu 2 mod 0<> if FMATH:E-DOMAIN throw then
   eu 2 / 0 ?do
      WANT-N @ WANT-CAP >= if E-BUF-BOUNDS throw then
      ea i HEX-BYTE WANT WANT-N @ + c!
      WANT-N @ 1 + WANT-N !
   loop ;

: E= ( -- )
   SINK$ WANT WANT-N @ SPAN=? TTRUE
   SINK BUF:CLEAR
   0 WANT-N !
   0 G-LOG-N ! ;

: G-LOG@ ( n -- n ) {: i:n :}
   G-LOG i + c@ ;

\ ---- the trap ----------------------------------------------------------------
\ The number goes in eax and zero-extends into rax; the trap is two bytes; the
\ reconciliation puts -4096 in rcx - free, because `syscall` clobbers rcx and r11
\ - and compares it AGAINST rax, so the borrow the subtraction produces sets CF
\ exactly on the -errno range. `cmp rax, -4096` would set CF on success instead.
: TRAP-CASES ( -- )
   s" SYS, loads the number, traps, then leaves CF set on -errno" T-LABEL
   \ llvm-mc: movl $257, %eax
   \ llvm-mc: syscall
   \ llvm-mc: movq $-4096, %rcx
   \ llvm-mc: cmpq %rax, %rcx
   NR-OPEN SYS,   s" b8010100000f0548c7c100f0ffff4839c1" X=

   s" every syscall number reaches eax as a zero-extending imm32" T-LABEL
   \ llvm-mc: movl $59, %eax
   NR-EXECVE SYS,  s" b83b0000000f0548c7c100f0ffff4839c1" X=

   s" a register is zeroed with the two-byte 32-bit xor" T-LABEL
   \ llvm-mc: xorl %edx, %edx
   RDX ZERO-REG,  s" 31d2" X=
   \ llvm-mc: xorl %r10d, %r10d
   R10 ZERO-REG,  s" 4531d2" X= ;

\ ---- the runtime-emit stencils -----------------------------------------------
: STENCIL-CASES ( -- )
   s" the stencils are the bytes their instructions encode to" T-LABEL
   \ llvm-mc: movl $1, %eax
   SYS-EMIT-WRITE s" b801000000" S=
   \ llvm-mc: movl $231, %eax
   SYS-EMIT-EXIT s" b8e7000000" S=
   \ llvm-mc: syscall
   SYS-EMIT-SVC s" 0f05" S=

   s" and are byte for byte what X64ASM encodes for the same instructions" T-LABEL
   0 >R32 NR-WRITE >IMM32 SINK ENC-MOV32-RI32
   SINK$ SYS-EMIT-WRITE SPAN=? TTRUE   SINK BUF:CLEAR
   0 >R32 NR-EXIT-GROUP >IMM32 SINK ENC-MOV32-RI32
   SINK$ SYS-EMIT-EXIT SPAN=? TTRUE    SINK BUF:CLEAR
   SINK ENC-SYSCALL
   SINK$ SYS-EMIT-SVC SPAN=? TTRUE     SINK BUF:CLEAR ;

\ ---- the kernel-argument translators -----------------------------------------
: OPEN-CASES ( -- )
   s" OS-OPEN-RD builds openat(AT_FDCWD, path, 0, 0)" T-LABEL
   12 OS-OPEN-RD
   s" 4c89e6" E+              \ llvm-mc: movq %r12, %rsi
   s" 48c7c79cffffff" E+      \ llvm-mc: movq $-100, %rdi
   s" 31d2" E+                \ llvm-mc: xorl %edx, %edx
   s" 4531d2" E+              \ llvm-mc: xorl %r10d, %r10d
   s" b801010000" E+          \ llvm-mc: movl $257, %eax
   s" 0f05" E+                \ llvm-mc: syscall
   s" 48c7c100f0ffff" E+      \ llvm-mc: movq $-4096, %rcx
   s" 4839c1" E+              \ llvm-mc: cmpq %rax, %rcx
   E=

   s" one flag bit selects the Linux bit with no branch" T-LABEL
   RSI $8 $400 OS-FLAG-BIT,
   s" 31c9" E+                \ llvm-mc: xorl %ecx, %ecx
   s" 49c7c300040000" E+      \ llvm-mc: movq $1024, %r11
   s" 48f7c608000000" E+      \ llvm-mc: testq $8, %rsi
   s" 490f45cb" E+            \ llvm-mc: cmovneq %r11, %rcx
   s" 4809c8" E+              \ llvm-mc: orq %rcx, %rax
   E=

   s" OS-OPEN-FLAGS keeps the access mode and moves four flag bits" T-LABEL
   OS-OPEN-FLAGS
   s" 4889f0" E+              \ llvm-mc: movq %rsi, %rax
   s" 4883e003" E+            \ llvm-mc: andq $3, %rax
   s" 31c9" E+                \ O_APPEND
   s" 49c7c300040000" E+      \ llvm-mc: movq $1024, %r11
   s" 48f7c608000000" E+      \ llvm-mc: testq $8, %rsi
   s" 490f45cb" E+  s" 4809c8" E+
   s" 31c9" E+                \ O_CREAT
   s" 49c7c340000000" E+      \ llvm-mc: movq $64, %r11
   s" 48f7c600020000" E+      \ llvm-mc: testq $512, %rsi
   s" 490f45cb" E+  s" 4809c8" E+
   s" 31c9" E+                \ O_TRUNC
   s" 49c7c300020000" E+      \ llvm-mc: movq $512, %r11
   s" 48f7c600040000" E+      \ llvm-mc: testq $1024, %rsi
   s" 490f45cb" E+  s" 4809c8" E+
   s" 31c9" E+                \ O_NOCTTY
   s" 49c7c300010000" E+      \ llvm-mc: movq $256, %r11
   s" 48f7c600000200" E+      \ llvm-mc: testq $131072, %rsi
   s" 490f45cb" E+  s" 4809c8" E+
   s" 4889c2" E+              \ llvm-mc: movq %rax, %rdx
   E= ;

: MMAP-CASES ( -- )
   s" OS-MMAP-FLAGS keeps share/private/fixed and moves MAP_ANON to $20" T-LABEL
   OS-MMAP-FLAGS
   s" 4c89d0" E+              \ llvm-mc: movq %r10, %rax
   s" 4883e013" E+            \ llvm-mc: andq $19, %rax
   s" 31c9" E+                \ llvm-mc: xorl %ecx, %ecx
   s" 49c7c320000000" E+      \ llvm-mc: movq $32, %r11
   s" 49f7c200100000" E+      \ llvm-mc: testq $4096, %r10
   s" 490f45cb" E+            \ llvm-mc: cmovneq %r11, %rcx
   s" 4809c8" E+              \ llvm-mc: orq %rcx, %rax
   s" 4989c2" E+              \ llvm-mc: movq %rax, %r10
   E= ;

\ ---- the process primitives --------------------------------------------------
\ The trap tail every one of these ends in: syscall, then the reconciliation.
: TRAP-TAIL+ ( -- )
   s" 0f05" E+                \ llvm-mc: syscall
   s" 48c7c100f0ffff" E+      \ llvm-mc: movq $-4096, %rcx
   s" 4839c1" E+ ;            \ llvm-mc: cmpq %rax, %rcx

: PROC-CASES ( -- )
   s" pidfd_open publishes -1 for an error and the fd otherwise" T-LABEL
   BPROCWATCHOPEN
   G-LOG-N @ 2 T=  0 G-LOG@ 7 T=  1 G-LOG@ 0 T=    \ pid from rdi, rax published
   s" 31f6" E+                \ llvm-mc: xorl %esi, %esi
   s" b8b2010000" E+          \ llvm-mc: movl $434, %eax
   TRAP-TAIL+
   s" 48c7c1ffffffff" E+      \ llvm-mc: movq $-1, %rcx
   s" 480f42c1" E+            \ llvm-mc: cmovbq %rcx, %rax
   E=

   s" kill takes the signal in rsi and the pid in rdi, and publishes rax" T-LABEL
   BKILLERRNO
   G-LOG-N @ 3 T=  0 G-LOG@ 6 T=  1 G-LOG@ 7 T=  2 G-LOG@ 0 T=
   s" b83e000000" E+          \ llvm-mc: movl $62, %eax
   TRAP-TAIL+
   E=

   s" execve takes envp, argv and pathz in rdx, rsi and rdi" T-LABEL
   BEXECVE
   G-LOG-N @ 4 T=  0 G-LOG@ 2 T=  1 G-LOG@ 6 T=  2 G-LOG@ 7 T=  3 G-LOG@ 0 T=
   s" b83b000000" E+          \ llvm-mc: movl $59, %eax
   TRAP-TAIL+
   E= ;

: RUN ( -- )
   T-RESET
   SINK 64 N>BLEN BUF:INIT
   0 G-LOG-N !
   TRAP-CASES
   STENCIL-CASES
   OPEN-CASES
   MMAP-CASES
   PROC-CASES
   SINK BUF:DISPOSE
   T-REPORT ;

RUN
;package
