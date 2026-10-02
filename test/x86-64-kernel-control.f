\ x86-64-kernel-control.f - the control rows of the x86-64 kernel
\ (src/habu/kernel-x64.f CONTROL,) in the booted harness, cross-built for an
\ x86-64 peer. Each image is one case, and the peer that runs it must see its
\ status:
\
\    hb-x64-kernel-control           76  evaluate refuses: its fd-2 line, then 76
\    hb-x64-kernel-control-negative  21  the same, expecting the wrong depth
\    hb-x64-kernel-execute            0  execute; 2>r, 2r@ and 2r>; both arms of
\                                        execute-floor
\    hb-x64-kernel-catch              0  catch with no throw, then a throw from a
\                                        nested call that moved the data, return,
\                                        loop and machine stacks
\    hb-x64-kernel-finally            0  the cleanup runs after a body that
\                                        returns and after one that throws, whose
\                                        code is rethrown; a throwing cleanup's
\                                        code wins
\    hb-x64-kernel-run-in-stack       0  a callback on the loop stack's mapping
\                                        returns, then throws; an extent inside
\                                        DATA throws E-STACK-UNGUARDED; each is
\                                        caught
\    hb-x64-kernel-unset-<row>       86  execute, catch, finally cleanup and
\                                        run-in-stack refuse a zero quotation
\                                        with `hb: unset quotation` on fd 2
\    hb-x64-kernel-unguarded         67  that refusal with no handler
\    hb-x64-kernel-throw             42  42 throw with no handler: the reporter
\                                        returns with the scratch registers
\                                        clobbered
\    hb-x64-kernel-throw-report      50  the reporter exits with the code plus 8
\    hb-x64-kernel-throw-corrupt     87  a handler frame whose cursor is below its
\                                        base: `hb: catch frame corrupt` on fd 2
\    hb-x64-kernel-die                5  `hb: die` and LF on fd 2; the exit hook
\                                        returns with the scratch registers
\                                        clobbered
\    hb-x64-kernel-die-hook           6  the exit hook dies with 6, so its cell
\                                        was cleared before the call
\    hb-x64-kernel-die-wide          67  an rc past 255
\
\ The host checks each image's ELF header; running them is the peer's.
require test/x86-64-boot-harness.f

package X64K-CONTROL
using X64ASM
using X64CODE
using X64RT

\ The status a routine exits with when it is handed the wrong code.
99 constant WRONG-RC
\ The code the uncaught throws carry, and the reporter's shift of it.
42 constant THROWN-CODE
8 constant REPORT-SHIFT
\ What the clobbering routines leave in every scratch register: a status in
\ [1, 255], so an exit that read one would name it.
9 constant CLOBBER-VALUE
\ A STACK-ABI:PAGE-BYTES aligned extent inside DATA: run-in-stack refuses it for
\ the one clause no alignment passes.
STACK-ABI:PAGE-BYTES 2 * constant INSIDE-OFF

: DATA-REG ( -- r64 ) ENGINE-GPR:X64-RBASE >R64 ;
: DSTACK-REG ( -- r64 ) ENGINE-GPR:X64-DSTACK >R64 ;
: IMM, ( r64 n -- ) >IMM64 ASM-SINK ENC-MOV-RI64 ;
: ROW, ( ptr u8 n -- ) X64HARNESS:CALL-ROW, ;
: PUSH, ( n -- ) X64HARNESS:PUSH, ;
: EXPECT-POP, ( n -- ) X64HARNESS:EXPECT-POP, ;

\ A routine is code a case hands a row as an xt: ROUTINE jumps over it and binds
\ its entry, ;ROUTINE ends it with ret and answers the entry.
: ROUTINE ( -- label label )
   LBL LBL {: entry:label past:label :}
   past JMP,  entry LBL,
   entry past ;

: ;ROUTINE ( label label -- label ) {: entry:label past:label :}
   ASM-SINK ENC-RET  past LBL,
   entry ;

: PUSH-XT, ( label -- ) {: at:label :}  RAX at MOVABS,  0 G-PUSH ;

\ Store a routine's address into a DATA cell: the reporter or the exit hook.
: XT!, ( label n -- ) {: at:label off:n :}
   RAX at MOVABS,
   RAX DATA-REG off MEM-OFF ASM-SINK ENC-MOV-MR ;

: PUSH-CELL, ( n -- ) {: off:n :}
   RAX DATA-REG off MEM-OFF ASM-SINK ENC-MOV-RM  0 G-PUSH ;

\ Check the DATA cell at an offset holds n.
: EXPECT-CELL, ( n n -- ) {: want:n off:n :}
   off PUSH-CELL,  want EXPECT-POP, ;

\ Push the address n bytes into DATA.
: PUSH-INSIDE, ( n -- ) {: off:n :}
   RAX DATA-REG off MEM-OFF ASM-SINK ENC-LEA  0 G-PUSH ;

\ Check the loop stack's cell n bytes past its base holds n: where a callback
\ run on that mapping pushed.
: EXPECT-LOOP-SLOT, ( n n -- ) {: want:n off:n :}
   RAX DATA-REG STACK-ABI:LOOP-BASE-CELL MEM-OFF ASM-SINK ENC-MOV-RM
   RAX RAX off MEM-OFF ASM-SINK ENC-MOV-RM  0 G-PUSH
   want EXPECT-POP, ;

\ Give every scratch register CLOBBER-VALUE, as the compiled code a row calls
\ may leave any of them.
: CLOBBER, ( -- )
   RAX CLOBBER-VALUE IMM,  RCX CLOBBER-VALUE IMM,  RDX CLOBBER-VALUE IMM,
   RSI CLOBBER-VALUE IMM,  RDI CLOBBER-VALUE IMM,  R8 CLOBBER-VALUE IMM,
   R9 CLOBBER-VALUE IMM,  R10 CLOBBER-VALUE IMM,  R11 CLOBBER-VALUE IMM, ;

\ ---- routines ----------------------------------------------------------------

: NOTHING ( -- label ) ROUTINE ;ROUTINE ;

: PUSHES ( n -- label ) {: v:n :} ROUTINE v PUSH, ;ROUTINE ;

\ ( n -- n ) add 4.
: ADD4 ( -- label )
   ROUTINE  0 G-POP  RAX 4 >IMM8 ASM-SINK ENC-ADD-RI8  0 G-PUSH  ;ROUTINE ;

\ Leave the data stack two cells below where it was entered.
: UNDERFLOW ( -- label )
   ROUTINE  DSTACK-REG 2 CELL * >IMM8 ASM-SINK ENC-SUB-RI8  ;ROUTINE ;

: THROWS ( n -- label ) {: code:n :} ROUTINE code PUSH, s" throw" ROW, ;ROUTINE ;

\ Throw 9 from a nested call once the data stack holds two more cells, the
\ return stack two, the loop stack a frame and the machine stack a cell.
: THROWER ( -- label )
   9 THROWS {: inner:label :}
   ROUTINE
   100 PUSH,  101 PUSH,
   1 PUSH,  2 PUSH,  s" 2>r" ROW,
   1 LOOPSP-CELL X64HARNESS:CELL!,
   RBX ASM-SINK ENC-PUSH
   inner CALL,
   ;ROUTINE ;

\ Count a run in the scratch cell.
: CLEANUP ( -- label )
   ROUTINE
   0 X64HARNESS:PUSH-SCRATCH,  0 G-POP
   RCX RAX MEM-AT ASM-SINK ENC-MOV-RM
   RCX ASM-SINK ENC-INC
   RCX RAX MEM-AT ASM-SINK ENC-MOV-MR
   ;ROUTINE ;

\ Run a body under finally with a cleanup, each an xt.
: FINALLY-OF ( label label -- label ) {: body:label cleanup:label :}
   ROUTINE  body PUSH-XT,  cleanup PUSH-XT,  s" finally" ROW,  ;ROUTINE ;

\ ( -- ) on the extent it runs on: push the data stack's capacity, then the
\ stack pointer less the data stack's base.
: MEASURES ( -- label )
   ROUTINE
   STACK-ABI:CAP-CELL PUSH-CELL,
   RAX DSTACK-REG ASM-SINK ENC-MOV-RR
   RAX DATA-REG STACK-ABI:BASE-CELL MEM-OFF ASM-SINK ENC-SUB-RM
   0 G-PUSH
   ;ROUTINE ;

\ Run a callback on the loop stack's mapping, or on the extent inside DATA.
: ON-LOOP-STACK ( label -- label ) {: cb:label :}
   ROUTINE
   cb PUSH-XT,  STACK-ABI:LOOP-BASE-CELL PUSH-CELL,  STACK-ABI:LOOP-BYTES PUSH,
   s" run-in-stack" ROW,
   ;ROUTINE ;

: PUSH-INSIDE-EXTENT, ( label -- ) {: cb:label :}
   cb PUSH-XT,  INSIDE-OFF PUSH-INSIDE,  STACK-ABI:LOOP-BYTES PUSH, ;

: ON-DATA ( label -- label ) {: cb:label :}
   ROUTINE  cb PUSH-INSIDE-EXTENT,  s" run-in-stack" ROW,  ;ROUTINE ;

\ ( n -- ): exit WRONG-RC unless handed THROWN-CODE, then return clobbered.
: REPORTS-BACK ( -- label )
   LBL {: fine:label :}
   ROUTINE
   0 G-POP  RAX THROWN-CODE >IMM8 ASM-SINK ENC-CMP-RI8  C-E fine JCC,
   RDI WRONG-RC IMM,  NR-EXIT-GROUP SYS,
   fine LBL,
   CLOBBER,
   ;ROUTINE ;

\ ( n -- ): exit with the code plus REPORT-SHIFT.
: REPORTS-EXIT ( -- label )
   ROUTINE
   7 G-POP  RDI REPORT-SHIFT >IMM8 ASM-SINK ENC-ADD-RI8  NR-EXIT-GROUP SYS,
   ;ROUTINE ;

: RETURNS-CLOBBERED ( -- label ) ROUTINE CLOBBER, ;ROUTINE ;

\ ( -- ): die quietly with rc n.
: DIES ( n -- label ) {: rc:n :}
   ROUTINE  0 PUSH,  0 PUSH,  rc PUSH,  s" die" ROW,  ;ROUTINE ;

\ ---- images ------------------------------------------------------------------

: BUILD-EVALUATE ( bool ptr u8 n -- ) {: negative:bool path:ptr pathu:n :}
   negative X64HARNESS:BOOT-OPEN,
   s" 1 2 +" X64HARNESS:PUSH-TEXT,
   2 X64HARNESS:EXPECT-DEPTH,
   s" evaluate" ROW,
   path pathu X64HARNESS:BOOT-CLOSE, ;

: BUILD-EXECUTE ( ptr u8 n -- ) {: path:ptr pathu:n :}
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
   path pathu X64HARNESS:BOOT-CLOSE, ;

: BUILD-CATCH ( ptr u8 n -- ) {: path:ptr pathu:n :}
   false X64HARNESS:BOOT-OPEN,
   11 PUSH,
   5 PUSHES PUSH-XT,  s" catch" ROW,  0 EXPECT-POP,  5 EXPECT-POP,
   0 HND-CELL EXPECT-CELL,
   THROWER PUSH-XT,  s" catch" ROW,  9 EXPECT-POP,  11 EXPECT-POP,
   0 X64HARNESS:EXPECT-DEPTH,
   X64HARNESS:EXPECT-BALANCED,
   0 HND-CELL EXPECT-CELL,
   0 RSP-CELL EXPECT-CELL,
   0 LOOPSP-CELL EXPECT-CELL,
   path pathu X64HARNESS:BOOT-CLOSE, ;

: BUILD-FINALLY ( ptr u8 n -- ) {: path:ptr pathu:n :}
   false X64HARNESS:BOOT-OPEN,
   5 PUSHES PUSH-XT,  CLEANUP PUSH-XT,  s" finally" ROW,
   5 EXPECT-POP,  1 0 X64HARNESS:EXPECT-SCRATCH,
   7 THROWS CLEANUP FINALLY-OF PUSH-XT,  s" catch" ROW,
   7 EXPECT-POP,  2 0 X64HARNESS:EXPECT-SCRATCH,
   7 THROWS 8 THROWS FINALLY-OF PUSH-XT,  s" catch" ROW,
   8 EXPECT-POP,
   0 X64HARNESS:EXPECT-DEPTH,
   X64HARNESS:EXPECT-BALANCED,
   0 HND-CELL EXPECT-CELL,
   path pathu X64HARNESS:BOOT-CLOSE, ;

: BUILD-RUN-IN-STACK ( ptr u8 n -- ) {: path:ptr pathu:n :}
   false X64HARNESS:BOOT-OPEN,
   MEASURES ON-LOOP-STACK CALL,
   0 X64HARNESS:EXPECT-DEPTH,
   STACK-ABI:BOOT-BYTES STACK-ABI:CAP-CELL EXPECT-CELL,
   STACK-ABI:LOOP-BYTES 0 EXPECT-LOOP-SLOT,
   CELL CELL EXPECT-LOOP-SLOT,
   13 THROWS ON-LOOP-STACK PUSH-XT,  s" catch" ROW,  13 EXPECT-POP,
   0 X64HARNESS:EXPECT-DEPTH,
   STACK-ABI:BOOT-BYTES STACK-ABI:CAP-CELL EXPECT-CELL,
   MEASURES ON-DATA PUSH-XT,  s" catch" ROW,
   STACK-ABI:E-STACK-UNGUARDED EXPECT-POP,
   0 X64HARNESS:EXPECT-DEPTH,
   X64HARNESS:EXPECT-BALANCED,
   path pathu X64HARNESS:BOOT-CLOSE, ;

: BUILD-UNGUARDED ( ptr u8 n -- ) {: path:ptr pathu:n :}
   false X64HARNESS:BOOT-OPEN,
   NOTHING PUSH-INSIDE-EXTENT,  s" run-in-stack" ROW,
   path pathu X64HARNESS:BOOT-CLOSE, ;

: BUILD-UNSET-EXEC ( ptr u8 n -- ) {: path:ptr pathu:n :}
   false X64HARNESS:BOOT-OPEN,
   0 PUSH,  s" execute" ROW,
   path pathu X64HARNESS:BOOT-CLOSE, ;

: BUILD-UNSET-CATCH ( ptr u8 n -- ) {: path:ptr pathu:n :}
   false X64HARNESS:BOOT-OPEN,
   0 PUSH,  s" catch" ROW,
   path pathu X64HARNESS:BOOT-CLOSE, ;

: BUILD-UNSET-FINALLY ( ptr u8 n -- ) {: path:ptr pathu:n :}
   false X64HARNESS:BOOT-OPEN,
   55 THROWS PUSH-XT,  0 PUSH,  s" finally" ROW,
   path pathu X64HARNESS:BOOT-CLOSE, ;

: BUILD-UNSET-RUN ( ptr u8 n -- ) {: path:ptr pathu:n :}
   false X64HARNESS:BOOT-OPEN,
   0 PUSH,  STACK-ABI:LOOP-BASE-CELL PUSH-CELL,  STACK-ABI:LOOP-BYTES PUSH,
   s" run-in-stack" ROW,
   path pathu X64HARNESS:BOOT-CLOSE, ;

\ An uncaught THROWN-CODE throw with a reporter installed.
: BUILD-THROW ( [ -- label ] ptr u8 n -- ) {: path:ptr pathu:n :}
   false X64HARNESS:BOOT-OPEN,
   execute UNCGH-CELL XT!,
   THROWN-CODE PUSH,  s" throw" ROW,
   path pathu X64HARNESS:BOOT-CLOSE, ;

\ A handler frame that passes every check but the last: its sentinel and
\ depths hold and its descriptor is the boot stack's, but its cursor is a cell
\ below that stack's base.
: BUILD-THROW-CORRUPT ( ptr u8 n -- ) {: path:ptr pathu:n :}
   false X64HARNESS:BOOT-OPEN,
   RSP STACK-ABI:CATCH-BYTES >IMM32 ASM-SINK ENC-SUB-RI32
   RCX ZERO-REG,
   STACK-ABI:CATCH-BYTES 0 do RCX RSP i MEM-OFF ASM-SINK ENC-MOV-MR CELL +loop
   RCX STACK-ABI:CATCH-MAGIC IMM,
   RCX RSP X64KERNEL:CATCH-SENTINEL MEM-OFF ASM-SINK ENC-MOV-MR
   RCX STACK-ABI:BOOT-BYTES IMM,
   RCX RSP STACK-ABI:CATCH-CAP MEM-OFF ASM-SINK ENC-MOV-MR
   RCX DATA-REG STACK-ABI:BASE-CELL MEM-OFF ASM-SINK ENC-MOV-RM
   RCX RSP STACK-ABI:CATCH-BASE MEM-OFF ASM-SINK ENC-MOV-MR
   RCX CELL >IMM8 ASM-SINK ENC-SUB-RI8
   RCX RSP X64KERNEL:CATCH-DSP MEM-OFF ASM-SINK ENC-MOV-MR
   RSP DATA-REG HND-CELL MEM-OFF ASM-SINK ENC-MOV-MR
   1 PUSH,  s" throw" ROW,
   path pathu X64HARNESS:BOOT-CLOSE, ;

\ die with rc n, a message and an exit hook installed.
: BUILD-DIE ( [ -- label ] n ptr u8 n -- ) {: rc:n path:ptr pathu:n :}
   false X64HARNESS:BOOT-OPEN,
   execute EXIT-HOOK-CELL XT!,
   s" hb: die" X64HARNESS:PUSH-TEXT,  rc PUSH,  s" die" ROW,
   path pathu X64HARNESS:BOOT-CLOSE, ;

: BUILD-DIE-WIDE ( ptr u8 n -- ) {: path:ptr pathu:n :}
   false X64HARNESS:BOOT-OPEN,
   s" hb: die" X64HARNESS:PUSH-TEXT,  256 PUSH,  s" die" ROW,
   path pathu X64HARNESS:BOOT-CLOSE, ;

public

: RUN ( -- )
   T-RESET
   X64HARNESS:INIT
   false s" hb-x64-kernel-control" TMP-PATH BUILD-EVALUATE
   true s" hb-x64-kernel-control-negative" TMP-PATH BUILD-EVALUATE
   s" hb-x64-kernel-execute" TMP-PATH BUILD-EXECUTE
   s" hb-x64-kernel-catch" TMP-PATH BUILD-CATCH
   s" hb-x64-kernel-finally" TMP-PATH BUILD-FINALLY
   s" hb-x64-kernel-run-in-stack" TMP-PATH BUILD-RUN-IN-STACK
   s" hb-x64-kernel-unguarded" TMP-PATH BUILD-UNGUARDED
   s" hb-x64-kernel-unset-execute" TMP-PATH BUILD-UNSET-EXEC
   s" hb-x64-kernel-unset-catch" TMP-PATH BUILD-UNSET-CATCH
   s" hb-x64-kernel-unset-finally" TMP-PATH BUILD-UNSET-FINALLY
   s" hb-x64-kernel-unset-run" TMP-PATH BUILD-UNSET-RUN
   [: REPORTS-BACK ;] s" hb-x64-kernel-throw" TMP-PATH BUILD-THROW
   [: REPORTS-EXIT ;] s" hb-x64-kernel-throw-report" TMP-PATH BUILD-THROW
   s" hb-x64-kernel-throw-corrupt" TMP-PATH BUILD-THROW-CORRUPT
   [: RETURNS-CLOBBERED ;] 5 s" hb-x64-kernel-die" TMP-PATH BUILD-DIE
   [: 6 DIES ;] 5 s" hb-x64-kernel-die-hook" TMP-PATH BUILD-DIE
   s" hb-x64-kernel-die-wide" TMP-PATH BUILD-DIE-WIDE
   X64HARNESS:DISPOSE
   T-REPORT ;

;using
;using
;using
;package

X64K-CONTROL:RUN
