\ x86-64-kernel-control.f - the control rows of the x86-64 kernel
\ (src/habu/kernel-x64.f CONTROL,) in the booted harness, cross-built for an
\ x86-64 peer. Each image is one case.
require src/core/exit-status.f
require test/x86-64-boot-harness.f
require src/arch/x86-64/rt.f
require src/habu/kernel-x64.f
require src/lib/test/assert.f
require src/os/linux-x86-64/sys.f
require test/x86-64-peer-harness.f
require src/arch/x86-64/asm.f
require src/arch/x86-64/icode.f

package X64K-CONTROL

\ The code the uncaught throws carry, and the reporter's shift of it.
42 constant THROWN-CODE
8 constant REPORT-SHIFT
\ What the clobbering routines leave in every scratch register: a status in
\ [1, 255], so an exit that read one would name it.
9 constant CLOBBER-VALUE
DATA-START $10000 + constant CLOSED-SCRATCH

: DATA-REG ( -- X64ASM:r64 ) ENGINE-GPR:X64-RBASE X64ASM:>R64 ;
: DSTACK-REG ( -- X64ASM:r64 ) ENGINE-GPR:X64-DSTACK X64ASM:>R64 ;
: IMM, ( X64ASM:r64 n -- ) X64ASM:>IMM64 X64CODE:ASM-SINK X64ASM:ENC-MOV-RI64 ;
: ROW, ( str -- ) X64HARNESS:CALL-ROW, ;
: PUSH, ( n -- ) X64HARNESS:PUSH, ;
: EXPECT-POP, ( n -- ) X64HARNESS:EXPECT-POP, ;

\ A routine is code a case hands a row as an xt: ROUTINE jumps over it and binds
\ its entry, ;ROUTINE ends it with ret and answers the entry.
: ROUTINE ( -- label label )
   X64CODE:LBL X64CODE:LBL {: entry:label past:label :}
   past X64CODE:JMP,  entry X64CODE:LBL,
   entry past ;

: ;ROUTINE ( label label -- label ) {: entry:label past:label :}
   X64CODE:ASM-SINK X64ASM:ENC-RET  past X64CODE:LBL,
   entry ;

: PUSH-XT, ( label -- ) {: at:label :}  X64ASM:RAX at X64CODE:MOVABS,  0 X64RT:G-PUSH ;

\ Store a routine's address into a DATA cell: the reporter or the exit hook.
: XT!, ( label n -- ) {: at:label off:n :}
   X64ASM:RAX at X64CODE:MOVABS,
   X64ASM:RAX DATA-REG off X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR ;

: PUSH-CELL, ( n -- ) {: off:n :}
   X64ASM:RAX DATA-REG off X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM  0 X64RT:G-PUSH ;

: EXPECT-CELL, ( n n -- ) {: want:n off:n :}
   off PUSH-CELL,  want EXPECT-POP, ;

: EXPECT-NO-FRAME, ( -- )
   X64ASM:RAX DATA-REG LOOPSP-CELL X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RAX DATA-REG STACK-ABI:LOOP-BASE-CELL X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-SUB-RM
   0 X64RT:G-PUSH  0 EXPECT-POP, ;

\ Give every scratch register CLOBBER-VALUE, as the compiled code a row calls
\ may leave any of them.
: CLOBBER, ( -- )
   X64ASM:RAX CLOBBER-VALUE IMM,  X64ASM:RCX CLOBBER-VALUE IMM,  X64ASM:RDX CLOBBER-VALUE IMM,
   X64ASM:RSI CLOBBER-VALUE IMM,  X64ASM:RDI CLOBBER-VALUE IMM,  X64ASM:R8 CLOBBER-VALUE IMM,
   X64ASM:R9 CLOBBER-VALUE IMM,  X64ASM:R10 CLOBBER-VALUE IMM,  X64ASM:R11 CLOBBER-VALUE IMM, ;

\ ---- routines ----------------------------------------------------------------

: NOTHING ( -- label ) ROUTINE ;ROUTINE ;

: PUSHES ( n -- label ) {: v:n :} ROUTINE v PUSH, ;ROUTINE ;

\ Drop n cells.
: TAKES ( n -- label ) {: cells:n :}
   ROUTINE  cells 0 ?do 0 X64RT:G-POP loop  ;ROUTINE ;

\ ( n -- n ) add 4.
: ADD4 ( -- label )
   ROUTINE  0 X64RT:G-POP  X64ASM:RAX 4 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-ADD-RI8  0 X64RT:G-PUSH  ;ROUTINE ;

\ Leave the data stack two cells below where it was entered.
: UNDERFLOW ( -- label )
   ROUTINE  DSTACK-REG 2 CELL * X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-SUB-RI8  ;ROUTINE ;

: THROWS ( n -- label ) {: code:n :} ROUTINE code PUSH, s" throw" ROW, ;ROUTINE ;

\ Throw 9 from a nested call once the data stack holds two more cells, the
\ return stack two and the machine stack one, and the loop stack's top moved.
: THROWER ( -- label )
   9 THROWS {: inner:label :}
   ROUTINE
   100 PUSH,  101 PUSH,
   1 PUSH,  2 PUSH,  s" 2>r" ROW,
   1 LOOPSP-CELL X64HARNESS:CELL!,
   X64ASM:RBX X64CODE:ASM-SINK X64ASM:ENC-PUSH
   inner X64CODE:CALL,
   ;ROUTINE ;

\ Count a run in the scratch cell.
: CLEANUP ( -- label )
   ROUTINE
   0 X64HARNESS:PUSH-SCRATCH,  0 X64RT:G-POP
   X64ASM:RCX X64ASM:RAX X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-INC
   X64ASM:RCX X64ASM:RAX X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   ;ROUTINE ;

: FINALLY-OF ( label label -- label ) {: body:label cleanup:label :}
   ROUTINE  body PUSH-XT,  cleanup PUSH-XT,  s" finally" ROW,  ;ROUTINE ;

: CLOSED-OF ( label -- label ) {: cb:label :}
   ROUTINE  cb PUSH-XT,  s" source-file-run" ROW,  ;ROUTINE ;

: EVAL-CLEAN ( n -- label ) {: off:n :}
   ROUTINE
   X64ASM:RAX DATA-REG STACK-ABI:BASE-CELL X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RAX DATA-REG CLOSED-SCRATCH off + X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   0 X64RT:G-POP  0 X64RT:G-POP
   ;ROUTINE ;

: EVAL-RESIDUE ( -- label )
   ROUTINE  0 X64RT:G-POP  0 X64RT:G-POP  99 PUSH,  ;ROUTINE ;

: EVAL-THROW ( -- label )
   ROUTINE  0 X64RT:G-POP  0 X64RT:G-POP  29 PUSH,  s" throw" ROW,  ;ROUTINE ;

: EVAL-FLOOR ( -- label )
   ROUTINE  0 X64RT:G-POP  0 X64RT:G-POP  s" execute" ROW,  ;ROUTINE ;

: CLOSED-TEXT ( -- label )
   ROUTINE  s" ok" X64HARNESS:PUSH-TEXT,  s" evaluate-closed" ROW,  ;ROUTINE ;

: THROW-WINDOW ( -- label )
   ROUTINE
   31 PUSH,  s" throw" ROW,
   ;ROUTINE ;

: FLOOR-THROW ( -- label )
   ROUTINE  99 PUSH,  X64KERNEL:FLOORREC-LBL X64CODE:JMP,  ;ROUTINE ;

\ Set the cursor to the physical top and write its final slot before return.
\ The enclosing source-file-run must return without pushing onto that stack.
: FILLS-EXTENT ( -- label )
   ROUTINE
   X64ASM:RCX DATA-REG STACK-ABI:BASE-CELL X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RDX DATA-REG STACK-ABI:CAP-CELL X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RCX X64ASM:RDX X64CODE:ASM-SINK X64ASM:ENC-ADD-RR
   X64ASM:RAX 1 IMM,
   X64ASM:RAX X64ASM:RCX CELL negate X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   DSTACK-REG X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-MOV-RR
   ;ROUTINE ;

\ ( n -- ): exit EXIT-STATUS:FAILURE unless handed THROWN-CODE, then return clobbered.
: REPORTS-BACK ( -- label )
   X64CODE:LBL {: fine:label :}
   ROUTINE
   0 X64RT:G-POP  X64ASM:RAX THROWN-CODE X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-CMP-RI8  X64ASM:C-E fine X64CODE:JCC,
   X64ASM:RDI EXIT-STATUS:FAILURE IMM,  NR-EXIT-GROUP SYS,
   fine X64CODE:LBL,
   CLOBBER,
   ;ROUTINE ;

\ ( n -- ): exit with the code plus REPORT-SHIFT.
: REPORTS-EXIT ( -- label )
   ROUTINE
   7 X64RT:G-POP  X64ASM:RDI REPORT-SHIFT X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-ADD-RI8  NR-EXIT-GROUP SYS,
   ;ROUTINE ;

: RETURNS-CLOBBERED ( -- label ) ROUTINE CLOBBER, ;ROUTINE ;

\ ( -- ): die quietly with rc n.
: DIES ( n -- label ) {: rc:n :}
   ROUTINE  0 PUSH,  0 PUSH,  rc PUSH,  s" die" ROW,  ;ROUTINE ;

\ ---- images ------------------------------------------------------------------
: BUILD-EVALUATE ( bool str -- ) {: negative:bool path:str :}
   negative X64HARNESS:BOOT-OPEN,
   s" 1 2 +" X64HARNESS:PUSH-TEXT,
   2 X64HARNESS:EXPECT-DEPTH,
   2 TAKES PROVIDED-XT:EVALUATE-CELL XT!,
   s" evaluate" ROW,
   0 X64HARNESS:EXPECT-DEPTH,
   path X64HARNESS:BOOT-CLOSE, ;

: BUILD-EVAL-RANGE ( str -- ) {: path:str :}
   false X64HARNESS:BOOT-OPEN,
   s" 1" X64HARNESS:PUSH-TEXT,  0 PUSH,  1 PUSH,
   4 TAKES PROVIDED-XT:EVALUATE-RANGE-CELL XT!,
   s" evaluate-range" ROW,
   0 X64HARNESS:EXPECT-DEPTH,
   path X64HARNESS:BOOT-CLOSE, ;

\ Clean, residue, throw and a real low guard fault return the segment to a pool.
: BUILD-EVAL-CLOSED ( str -- ) {: path:str :}
   false X64HARNESS:BOOT-OPEN,
   0 EVAL-CLEAN PROVIDED-XT:EVALUATE-CLOSED-CELL XT!,
   11 PUSH,  s" ok" X64HARNESS:PUSH-TEXT,  s" evaluate-closed" ROW,
   11 EXPECT-POP,  0 X64HARNESS:EXPECT-DEPTH,
   8 EVAL-CLEAN PROVIDED-XT:EVALUATE-CLOSED-CELL XT!,
   s" ok" X64HARNESS:PUSH-TEXT,  s" evaluate-closed" ROW,
   X64ASM:RAX DATA-REG CLOSED-SCRATCH X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RCX DATA-REG CLOSED-SCRATCH CELL + X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RAX X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-CMP-RR
   X64CODE:LBL {: reused:label :}  X64ASM:C-E reused X64CODE:JCC,
   X64ASM:RDI EXIT-STATUS:FAILURE IMM,  NR-EXIT-GROUP SYS,
   reused X64CODE:LBL,
   EVAL-RESIDUE PROVIDED-XT:EVALUATE-CLOSED-CELL XT!,
   CLOSED-TEXT PUSH-XT,  s" catch" ROW,
   0 EXPECT-POP,
   EVAL-THROW PROVIDED-XT:EVALUATE-CLOSED-CELL XT!,
   CLOSED-TEXT PUSH-XT,  s" catch" ROW,
   29 EXPECT-POP,
   EVAL-FLOOR PROVIDED-XT:EVALUATE-CLOSED-CELL XT!,
   CLOSED-TEXT PUSH-XT,  s" catch" ROW,
   E-UNDERFLOW EXPECT-POP,
   8 EVAL-CLEAN PROVIDED-XT:EVALUATE-CLOSED-CELL XT!,
   s" ok" X64HARNESS:PUSH-TEXT,  s" evaluate-closed" ROW,
   X64ASM:RAX DATA-REG CLOSED-SCRATCH X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RCX DATA-REG CLOSED-SCRATCH CELL + X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RAX X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-CMP-RR
   X64CODE:LBL {: reused-again:label :}  X64ASM:C-E reused-again X64CODE:JCC,
   X64ASM:RDI EXIT-STATUS:FAILURE IMM,  NR-EXIT-GROUP SYS,
   reused-again X64CODE:LBL,
   0 X64HARNESS:EXPECT-DEPTH,
   0 HND-CELL EXPECT-CELL,
   STACK-ABI:BOOT-BYTES STACK-ABI:CAP-CELL EXPECT-CELL,
   X64HARNESS:EXPECT-BALANCED,
   path X64HARNESS:BOOT-CLOSE, ;

\ A throw out of an open code window closes the band before catch resumes.
: BUILD-THROW-CLOSE ( str -- ) {: path:str :}
   false X64HARNESS:BOOT-OPEN,
   5 PUSH,  THROW-WINDOW PUSH-XT,  s" catch" ROW,
   31 EXPECT-POP,  5 EXPECT-POP,
   DICT-SIZE X64HARNESS:PROBE,  X64HARNESS:FAULTED EXPECT-POP,
   0 X64HARNESS:EXPECT-DEPTH,
   X64HARNESS:EXPECT-BALANCED,
   path X64HARNESS:BOOT-CLOSE, ;

\ The data-floor recovery entry empties the faulting stack before it throws.
: BUILD-FLOORREC ( str -- ) {: path:str :}
   false X64HARNESS:BOOT-OPEN,
   5 PUSH,  FLOOR-THROW PUSH-XT,  s" catch" ROW,
   E-UNDERFLOW EXPECT-POP,  5 EXPECT-POP,
   0 X64HARNESS:EXPECT-DEPTH,
   X64HARNESS:EXPECT-BALANCED,
   path X64HARNESS:BOOT-CLOSE, ;

: BUILD-STACK-CLEAR ( str -- ) {: path:str :}
   false X64HARNESS:BOOT-OPEN,
   5 PUSH,  6 PUSH,  s" stack-clear" ROW,
   0 X64HARNESS:EXPECT-DEPTH,
   9 PUSH,  9 EXPECT-POP,
   STACK-ABI:BOOT-BYTES STACK-ABI:CAP-CELL EXPECT-CELL,
   X64HARNESS:EXPECT-BALANCED,
   path X64HARNESS:BOOT-CLOSE, ;

: BUILD-EXECUTE ( str -- ) {: path:str :}
   false X64HARNESS:BOOT-OPEN,
   3 PUSH,  ADD4 PUSH-XT,  s" execute" ROW,  7 EXPECT-POP,
   5 PUSH,  6 PUSH,  s" 2>r" ROW,
   s" 2r@" ROW,  6 EXPECT-POP,  5 EXPECT-POP,
   s" 2r>" ROW,  6 EXPECT-POP,  5 EXPECT-POP,
   0 RSP-CELL EXPECT-CELL,
   NOTHING PUSH-XT,  s" execute-floor" ROW,  0 EXPECT-POP,
   UNDERFLOW PUSH-XT,  s" execute-floor" ROW,  -1 EXPECT-POP,
   0 X64HARNESS:EXPECT-DEPTH,
   X64HARNESS:EXPECT-BALANCED,
   path X64HARNESS:BOOT-CLOSE, ;

\ The throw comes from a nested call that moved the data, return, loop and
\ machine stacks.
: BUILD-CATCH ( str -- ) {: path:str :}
   false X64HARNESS:BOOT-OPEN,
   11 PUSH,
   5 PUSHES PUSH-XT,  s" catch" ROW,  0 EXPECT-POP,  5 EXPECT-POP,
   0 HND-CELL EXPECT-CELL,
   THROWER PUSH-XT,  s" catch" ROW,  9 EXPECT-POP,  11 EXPECT-POP,
   0 X64HARNESS:EXPECT-DEPTH,
   X64HARNESS:EXPECT-BALANCED,
   0 HND-CELL EXPECT-CELL,
   0 RSP-CELL EXPECT-CELL,
   EXPECT-NO-FRAME,
   path X64HARNESS:BOOT-CLOSE, ;

\ The body's code is rethrown even when the cleanup throws too.
: BUILD-FINALLY ( str -- ) {: path:str :}
   false X64HARNESS:BOOT-OPEN,
   5 PUSHES PUSH-XT,  CLEANUP PUSH-XT,  s" finally" ROW,
   5 EXPECT-POP,  1 0 X64HARNESS:EXPECT-SCRATCH,
   7 THROWS CLEANUP FINALLY-OF PUSH-XT,  s" catch" ROW,
   7 EXPECT-POP,  2 0 X64HARNESS:EXPECT-SCRATCH,
   7 THROWS 8 THROWS FINALLY-OF PUSH-XT,  s" catch" ROW,
   7 EXPECT-POP,
   0 X64HARNESS:EXPECT-DEPTH,
   X64HARNESS:EXPECT-BALANCED,
   0 HND-CELL EXPECT-CELL,
   path X64HARNESS:BOOT-CLOSE, ;

\ A clean callback, residue, a nested throw and an exactly full stack return
\ restore the caller's stack and extent.
: BUILD-SOURCE-UNIT ( str -- ) {: path:str :}
   false X64HARNESS:BOOT-OPEN,
   7 PUSH,  NOTHING CLOSED-OF X64CODE:CALL,  7 EXPECT-POP,
   7 PUSH,  5 PUSHES CLOSED-OF PUSH-XT,  s" catch" ROW,
   0 EXPECT-POP,  7 EXPECT-POP,
   7 PUSH,  13 THROWS CLOSED-OF PUSH-XT,  s" catch" ROW,
   13 EXPECT-POP,  7 EXPECT-POP,
   7 PUSH,  FILLS-EXTENT CLOSED-OF PUSH-XT,  s" catch" ROW,
   0 EXPECT-POP,  7 EXPECT-POP,
   0 X64HARNESS:EXPECT-DEPTH,
   STACK-ABI:BOOT-BYTES STACK-ABI:CAP-CELL EXPECT-CELL,
   X64HARNESS:EXPECT-BALANCED,
   path X64HARNESS:BOOT-CLOSE, ;

\ An uncaught THROWN-CODE throw with a reporter installed.
: BUILD-THROW ( [ -- label ] n str -- ) {: status:n path:str :}
   false X64HARNESS:BOOT-OPEN,
   status X64HARNESS:EXPECT-EXIT
   execute UNCGH-CELL XT!,
   THROWN-CODE PUSH,  s" throw" ROW,
   path X64HARNESS:BOOT-CLOSE, ;

\ die with rc n, a message and an exit hook installed. The hook of -die-hook dies
\ itself, and ends the image only if its cell was cleared before the call.
: BUILD-DIE ( [ -- label ] n n str -- ) {: rc:n status:n path:str :}
   false X64HARNESS:BOOT-OPEN,
   status X64HARNESS:EXPECT-EXIT
   execute EXIT-HOOK-CELL XT!,
   s" hb: die" X64HARNESS:PUSH-TEXT,  rc PUSH,  s" die" ROW,
   path X64HARNESS:BOOT-CLOSE, ;

\ die inside a catch, its exit hook throwing THROWN-CODE, with a reporter
\ installed. Before the hook runs, die empties the handler chain, so the throw
\ reaches the reporter instead of resuming the catch and returning from die.
: BUILD-DIE-THROW ( str -- ) {: path:str :}
   false X64HARNESS:BOOT-OPEN,
   THROWN-CODE REPORT-SHIFT + X64HARNESS:EXPECT-EXIT
   REPORTS-EXIT UNCGH-CELL XT!,
   THROWN-CODE THROWS EXIT-HOOK-CELL XT!,
   5 DIES PUSH-XT,  s" catch" ROW,
   path X64HARNESS:BOOT-CLOSE, ;

: BUILD-DIE-WIDE ( str -- ) {: path:str :}
   false X64HARNESS:BOOT-OPEN,
   EXIT-STATUS:UNCAUGHT X64HARNESS:EXPECT-EXIT
   s" hb: die" X64HARNESS:PUSH-TEXT,  256 PUSH,  s" die" ROW,
   path X64HARNESS:BOOT-CLOSE, ;

public

: RUN ( -- )
   T-RESET
   X64HARNESS:INIT
   false s" hb-x64-kernel-control" TMP-PATH BUILD-EVALUATE
   true s" hb-x64-kernel-control-negative" TMP-PATH BUILD-EVALUATE
   s" hb-x64-kernel-eval-range" TMP-PATH BUILD-EVAL-RANGE
   s" hb-x64-kernel-execute" TMP-PATH BUILD-EXECUTE
   s" hb-x64-kernel-catch" TMP-PATH BUILD-CATCH
   s" hb-x64-kernel-finally" TMP-PATH BUILD-FINALLY
   s" hb-x64-kernel-source-unit" TMP-PATH BUILD-SOURCE-UNIT
   s" hb-x64-kernel-eval-closed" TMP-PATH BUILD-EVAL-CLOSED
   s" hb-x64-kernel-throw-close" TMP-PATH BUILD-THROW-CLOSE
   s" hb-x64-kernel-floorrec" TMP-PATH BUILD-FLOORREC
   s" hb-x64-kernel-stack-clear" TMP-PATH BUILD-STACK-CLEAR
   [: REPORTS-BACK ;] THROWN-CODE s" hb-x64-kernel-throw" TMP-PATH BUILD-THROW
   [: REPORTS-EXIT ;] THROWN-CODE REPORT-SHIFT +
   s" hb-x64-kernel-throw-report" TMP-PATH BUILD-THROW
   [: RETURNS-CLOBBERED ;] 5 5 s" hb-x64-kernel-die" TMP-PATH BUILD-DIE
   [: 6 DIES ;] 5 6 s" hb-x64-kernel-die-hook" TMP-PATH BUILD-DIE
   s" hb-x64-kernel-die-throw" TMP-PATH BUILD-DIE-THROW
   s" hb-x64-kernel-die-wide" TMP-PATH BUILD-DIE-WIDE
   X64HARNESS:DISPOSE
   T-REPORT ;

;package

X64K-CONTROL:RUN
