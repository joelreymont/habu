\ x86-64-emit.f - pins, on aarch64, the bytes the linux-x86-64 seam
\ (src/os/linux-x86-64/{sys,proc-watch,proc-control}.f), src/arch/x86-64/rt.f
\ and X64CODE's label sites emit. Each `llvm-mc:` comment is the line that
\ produced the bytes (LLVM 22.1.8, as test/compiler/x86-64-asm.f). No x86_64
\ instruction runs: SYS,'s carry polarity and linked addresses are observed by
\ the images of test/x86-64-peer-image.f.

require src/lib/test.f
require src/lib/test/outcome.f
require src/lib/test/subject.f
require src/lib/byte-buffer.f
require src/lib/errors.f
require src/lib/fmath.f                       \ FMATH:E-DOMAIN, borrowed for a malformed expected string
require src/arch/x86-64/asm.f
require src/arch/x86-64/icode.f
require src/arch/x86-64/rt.f

\ Own suite: the seam's sys.f redefines host syscall words.
s" src/os/linux-x86-64/sys.f" required
s" src/os/linux-x86-64/proc-watch.f" required
s" src/os/linux-x86-64/proc-control.f" required

package X64-EMIT-TEST
private
using X64ASM
using X64CODE
using X64RT

: N>BLEN ( n -- NUM:byte-len )
   NUM:BYTE-LEN
   MATCH NUM:numeric-result
      ok OF ENDOF                                negative OF E-BUF-BOUNDS throw ENDOF
      zero OF E-BUF-BOUNDS throw ENDOF           overflow OF E-BUF-BOUNDS throw ENDOF
      underflow OF E-BUF-BOUNDS throw ENDOF      bad-alignment OF E-BUF-BOUNDS throw ENDOF
      misaligned OF E-BUF-BOUNDS throw ENDOF
   ;MATCH ;

\ ---- comparing a byte span against its expected string -----------------------
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
   CODE ASM-LEN ;

: X= ( ptr u8 n -- ) {: ea:ptr eu:n :}
   SINK$ ea eu SPAN=HEX? TTRUE
   ASM-RESET ;

: S= ( ptr u8 n ptr u8 n -- ) {: da:ptr dlen:n ea:ptr eu:n :}
   da dlen ea eu SPAN=HEX? TTRUE ;

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
   ASM-RESET
   0 WANT-N ! ;

\ ---- the trap ----------------------------------------------------------------
\ rcx is free for -4096 because `syscall` clobbers rcx and r11. Comparing it
\ AGAINST rax makes the borrow set CF exactly on the -errno range; `cmp rax,
\ -4096` would set CF on success.
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
   0 >R32 NR-WRITE >IMM32 ASM-SINK ENC-MOV32-RI32
   SINK$ SYS-EMIT-WRITE SPAN=? TTRUE   ASM-RESET
   0 >R32 NR-EXIT-GROUP >IMM32 ASM-SINK ENC-MOV32-RI32
   SINK$ SYS-EMIT-EXIT SPAN=? TTRUE    ASM-RESET
   ASM-SINK ENC-SYSCALL
   SINK$ SYS-EMIT-SVC SPAN=? TTRUE     ASM-RESET

   s" EMIT-STENCIL appends each stencil whole, in order, to the code stream" T-LABEL
   SYS-EMIT-WRITE EMIT-STENCIL  SYS-EMIT-SVC EMIT-STENCIL      \ the MATCH bad-tag
   SYS-EMIT-EXIT EMIT-STENCIL   SYS-EMIT-SVC EMIT-STENCIL      \ die's two traps
   s" b801000000" E+          \ llvm-mc: movl $1, %eax
   s" 0f05" E+                \ llvm-mc: syscall
   s" b8e7000000" E+          \ llvm-mc: movl $231, %eax
   s" 0f05" E+                \ llvm-mc: syscall
   E= ;

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
: TRAP-TAIL+ ( -- )
   s" 0f05" E+                \ llvm-mc: syscall
   s" 48c7c100f0ffff" E+      \ llvm-mc: movq $-4096, %rcx
   s" 4839c1" E+ ;            \ llvm-mc: cmpq %rax, %rcx

\ r12 points just past the top cell: a pop retreats then loads, a push stores
\ then advances.
: RETREAT+ ( -- )
   s" 4983ec08" E+ ;          \ llvm-mc: subq $8, %r12

: PUBLISH-RAX+ ( -- )
   s" 49890424" E+            \ llvm-mc: movq %rax, (%r12)
   s" 4983c408" E+ ;          \ llvm-mc: addq $8, %r12

: PROC-CASES ( -- )
   s" pidfd_open takes the pid in rdi and publishes fd or negative errno" T-LABEL
   BPROCWATCHOPEN
   RETREAT+
   s" 498b3c24" E+            \ llvm-mc: movq (%r12), %rdi
   s" 31f6" E+                \ llvm-mc: xorl %esi, %esi
   s" b8b2010000" E+          \ llvm-mc: movl $434, %eax
   TRAP-TAIL+
   PUBLISH-RAX+
   E=

   s" kill takes the signal in rsi and the pid in rdi, and publishes rax" T-LABEL
   BKILLERRNO
   RETREAT+
   s" 498b3424" E+            \ llvm-mc: movq (%r12), %rsi
   RETREAT+
   s" 498b3c24" E+            \ llvm-mc: movq (%r12), %rdi
   s" b83e000000" E+          \ llvm-mc: movl $62, %eax
   TRAP-TAIL+
   PUBLISH-RAX+
   E=

   s" execve takes envp, argv and pathz in rdx, rsi and rdi" T-LABEL
   BEXECVE
   RETREAT+
   s" 498b1424" E+            \ llvm-mc: movq (%r12), %rdx
   RETREAT+
   s" 498b3424" E+            \ llvm-mc: movq (%r12), %rsi
   RETREAT+
   s" 498b3c24" E+            \ llvm-mc: movq (%r12), %rdi
   s" b83b000000" E+          \ llvm-mc: movl $59, %eax
   TRAP-TAIL+
   PUBLISH-RAX+
   E= ;

\ ---- the code layer's label sites ---------------------------------------------
\ Sites are emitted zero and patched by ASM-LINK. $401000 is the -Ttext address
\ GNU ld linked the llvm-mc object at.
$401000 constant LINK-VA

: NOPS ( n -- )  0 ?do ASM-SINK ENC-NOP loop ;

: LINK-FIXTURE ( -- )
   LBL LBL {: a:label b:label :}
   a LBL,
   b JMP,
   C-NE b JCC,
   a CALL,
   b JMP8,
   C-E a JCC8,
   RAX b MOVABS,
   b LBL,
   ASM-SINK ENC-RET ;

\ The forward label binds at the very end of the stream, where no instruction
\ follows it.
: REL8-FWD ( n -- ) {: gap:n :}
   LBL {: e:label :}
   e JMP8,  gap NOPS  e LBL, ;

: REL8-BACK ( n -- ) {: gap:n :}
   LBL {: f:label :}
   f LBL,  gap NOPS  C-NE f JCC8, ;

: LINK-CASES ( -- )
   s" every site kind links to its label, forward and backward" T-LABEL
   LINK-FIXTURE  LINK-VA ASM-LINK
   s" e919000000" E+          \ llvm-mc: {disp32} jmp B
   s" 0f8513000000" E+        \ llvm-mc: {disp32} jne B
   s" e8f0ffffff" E+          \ llvm-mc: call A
   s" eb0c" E+                \ llvm-mc: {disp8} jmp B
   s" 74ec" E+                \ llvm-mc: {disp8} je A
   s" 48b81e10400000000000" E+  \ llvm-mc: movabsq $B, %rax (ld -Ttext=0x401000)
   s" c3" E+                  \ llvm-mc: ret
   E=

   s" a rel8 site reaches 127 bytes ahead and 128 behind" T-LABEL
   127 REL8-FWD  LINK-VA ASM-LINK
   CODE 2 s" eb7f" SPAN=HEX? TTRUE        \ llvm-mc: {disp8} jmp E, 127 bytes to E
   ASM-RESET
   126 REL8-BACK  LINK-VA ASM-LINK
   CODE 126 + 2 s" 7580" SPAN=HEX? TTRUE  \ llvm-mc: {disp8} jne F, 126 bytes after F
   ASM-RESET

   s" linking forgets the stream's sites, so the next stream is not patched" T-LABEL
   LBL {: l:label :}
   l JMP,  l LBL,  LINK-VA ASM-LINK
   s" e900000000" X=          \ llvm-mc: {disp32} jmp B, B next
   5 NOPS  LINK-VA ASM-LINK
   s" 9090909090" X=

   s" a reset stream's sites never reach the longer stream after it" T-LABEL
   LBL {: old:label :}
   old JMP,  3 NOPS  old LBL,
   ASM-RESET
   LBL {: new:label :}
   new LBL,  8 NOPS  new JMP,  LINK-VA ASM-LINK
   s" 9090909090909090" E+
   s" e9f3ffffff" E+          \ llvm-mc: {disp32} jmp T, 8 bytes after T
   E= ;

\ ---- the code layer's refusals ------------------------------------------------
\ A refusal dies before a byte is patched; a child runs each fixture so the
\ suite survives it. A rel32 site out of reach needs a 2 GB stream: not built.
72 constant REFUSE-RC                   \ X64CODE's refusal exit, as src/arch/arm64/icode.f's
$1000 constant CAPTURE-CAP
10000 constant TIMEOUT-MS
create OUT CAPTURE-CAP allot
create ERR CAPTURE-CAP allot

: UNRESOLVED ( -- )  LBL JMP,  LINK-VA ASM-LINK ;
: REL8-FAR-FWD ( -- )  128 REL8-FWD  LINK-VA ASM-LINK ;
: REL8-FAR-BACK ( -- )  127 REL8-BACK  LINK-VA ASM-LINK ;
: REDEFINED ( -- )  LBL {: l:label :}  l LBL,  l LBL, ;
: FOREIGN ( -- )  LBL LABEL>N 1 + >LABEL JMP, ;

: STALE-LINKED ( -- )
   LBL {: old:label :}
   old LBL,  LINK-VA ASM-LINK
   LBL LBL,  old JMP, ;
: STALE-RESET ( -- )
   LBL {: old:label :}
   old LBL,  ASM-RESET
   LBL LBL,  old JMP, ;

: CUT ( -- )
   LBL {: l:label :}
   l JMP,  l LBL,
   ASM-SINK BUF:CLEAR
   LINK-VA ASM-LINK ;

: REFUSES ( ptr u8 n ptr u8 n -- )
   {: source:ptr sourceu:n want:ptr wantu:n :}
   source sourceu OUT CAPTURE-CAP >LEN ERR CAPTURE-CAP >LEN TIMEOUT-MS >MS
   SUBJECT:RUN {: outu:len erru:len oc :}
   source sourceu OUT outu LEN>N ERR erru LEN>N oc REFUSE-RC T-OUTCOME-EXITED=
   outu LEN>N 0 T=
   ERR erru LEN>N want wantu T$= ;

: REFUSAL-CASES ( -- )
   s" a site whose label was never bound refuses the link" T-LABEL
   s" UNRESOLVED" S\" x64code: unresolved label\n" REFUSES
   s" a rel8 site one byte past either end of its reach refuses the link" T-LABEL
   s" REL8-FAR-FWD" S\" x64code: rel8 out of reach\n" REFUSES
   s" REL8-FAR-BACK" S\" x64code: rel8 out of reach\n" REFUSES
   s" a label binds once" T-LABEL
   s" REDEFINED" S\" x64code: label redefined\n" REFUSES
   s" a label number not yet made is refused at its site" T-LABEL
   s" FOREIGN" S\" x64code: unknown label\n" REFUSES
   s" a label held past the link or reset that ended its stream is refused" T-LABEL
   s" STALE-LINKED" S\" x64code: unknown label\n" REFUSES
   s" STALE-RESET" S\" x64code: unknown label\n" REFUSES
   s" a site the sink was cleared under without ASM-RESET refuses the link" T-LABEL
   s" CUT" S\" x64code: label or site past the end of the code\n" REFUSES ;

: RUN ( -- )
   T-RESET
   ASM-SINK 64 N>BLEN BUF:INIT
   TRAP-CASES
   STENCIL-CASES
   OPEN-CASES
   MMAP-CASES
   PROC-CASES
   LINK-CASES
   REFUSAL-CASES
   ASM-SINK BUF:DISPOSE
   T-REPORT ;

RUN
;package
