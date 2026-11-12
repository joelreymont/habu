\ A load through a bad address in Habu code throws ENGINE-ERROR:MEMORY-FAULT
\ on the faulting thread: a catch receives it and the program runs on, a task
\ ends with it, and uncaught it exits 110 after one line on fd 2, with no
\ register dump. A fault in foreign code, a jump to an unmapped address or into
\ a guard page and a fault in a signal handler still dump the registers and die
\ by the signal. Each case is its own child: its process's end is what it
\ checks.
require test/gate-common.f

package MEMORY-FAULT-TEST
private

\ A read or fetch of a PROT_NONE page is SIGBUS on macOS and SIGSEGV on Linux.
: FOREIGN-SIG ( -- n ) HB-TARGET-LINUX-KERNEL? if 11 else 10 then ;

\ The uncaught line up to the pc's 16 hex digits and its newline.
: ADDRESS-0$ ( -- ptr u8 n )
   s" E-MEMORY-FAULT habu: memory fault at 0000000000000000 pc " ;

: RUN-BUF ( -- )
   GE-HB-RESET
   GE-HB$ GE-SRC-BUF GE-SRC-U @ GE-TIMEOUT-MS GE-RUN-STDIN ;

\ The child ended by the signal, not by an exit: crash.f EMIT-CRASH-HANDLER
\ re-raises it after the dump. An exit of 128 + n is no signal death.
: KILLED-BY ( n ptr u8 n -- ) {: sig:n label:ptr labelu:n :}
   GT-OUTCOME@ MATCH PROCESS:ending
     exited OF drop label labelu GE-FAIL ENDOF
     signaled OF sig <> if label labelu GE-FAIL then ENDOF
     timeout OF label labelu GE-FAIL ENDOF
   ;MATCH ;

\ fd 2 holds exactly the line: its prefix, the pc and a newline.
: ONE-LINE ( ptr u8 n -- ) {: label:ptr labelu:n :}
   GT-ERR$ ADDRESS-0$ STARTS-WITH? 0= if label labelu GE-FAIL then
   GT-ERR$ nip ADDRESS-0$ nip 17 + <> if label labelu GE-FAIL then ;

: UNCAUGHT ( -- )
   GE-SRC-RESET
   s" TRUSTED: W ( -- ) 0 @ drop ;" GE-SRC-LINE
   s" W" GE-SRC-LINE
   RUN-BUF
   ENGINE-ERROR:MEMORY-FAULT s" an uncaught load from address 0" GE-EXPECT-RC
   s" an uncaught load from address 0" ONE-LINE
   s" PASS: an uncaught fault exits 110 after one line" type cr ;

: CAUGHT ( -- )
   GE-SRC-RESET
   s" require src/lib/task.f" GE-SRC-LINE
   s" : W ( -- ) NULL-PTR BYTE-VIEW c@ . ;" GE-SRC-LINE
   S\" : PLAIN ( -- ) .\" normal word ran\" cr ;" GE-SRC-LINE
   S\" : FAULT-TEXT ( -- ) s\" NULL-PTR BYTE-VIEW c@ .\"" GE-SRC+
   s"  evaluate-closed ;" GE-SRC-LINE
   s" : CATCHES ( -- )" GE-SRC-LINE
   S\"    ['] W catch .\" first catch \" ." GE-SRC-LINE
   S\"    ['] W catch .\" second catch \" ." GE-SRC-LINE
   s"    PLAIN" GE-SRC-LINE
   S\"    ['] FAULT-TEXT catch .\" evaluate-closed catch \" ." GE-SRC-LINE
   S\"    ['] FAULT-TEXT catch .\" evaluate-closed catch again \" ." GE-SRC-LINE
   s"    PLAIN ;" GE-SRC-LINE
   s" CATCHES" GE-SRC-LINE
   s" TASK:MIN-STACK TASK:TASK FAULTER" GE-SRC-LINE
   s" : REPORT ( result<n,n> -- )" GE-SRC-LINE
   s"    MATCH result" GE-SRC-LINE
   S\"      ok OF .\" task returned \" . ENDOF" GE-SRC-LINE
   S\"      err OF .\" task ended with \" . ENDOF" GE-SRC-LINE
   s"    ;MATCH ;" GE-SRC-LINE
   s" : TASK-RUN ( -- )" GE-SRC-LINE
   s"    ['] W FAULTER TASK:ACTIVATE" GE-SRC-LINE
   s"    FAULTER TASK:JOIN REPORT" GE-SRC-LINE
   S\"    .\" main thread runs on\" cr ;" GE-SRC-LINE
   s" TASK-RUN" GE-SRC-LINE
   s" faults caught and a faulting task" GE-HB-RUN-STDIN
   SB-RESET
   S\" first catch 110\nsecond catch 110\nnormal word ran\n" SB-APPEND
   S\" evaluate-closed catch 110\nevaluate-closed catch again 110\n" SB-APPEND
   S\" normal word ran\ntask ended with 110\nmain thread runs on\n" SB-APPEND
   SB$ s" faults caught and a faulting task" GE-EXPECT-OUT
   s" PASS: a caught fault and a faulting task run on" type cr ;

\ strlen reads the cell below the data stack's base, inside its low guard
\ page: the fault's pc is in libc.
: FOREIGN ( -- )
   GE-SRC-RESET
   s" PROCESS-SYMBOLS" GE-SRC-LINE
   s" FUNCTION: STRLEN$ strlen ( ptr u8 -- n ) ;FUNCTION" GE-SRC-LINE
   s" : GUARD-OFF ( -- n ) data-base STACK-ABI:BASE-CELL + @ 8 - ;" GE-SRC-LINE
   s" NULL-PTR BYTE-VIEW GUARD-OFF + STRLEN$ ." GE-SRC-LINE
   RUN-BUF
   FOREIGN-SIG s" a fault in foreign code" KILLED-BY
   s" habu-crash regs [sig" s" a fault in foreign code" GE-EXPECT-ERR-HAS
   s" E-MEMORY-FAULT" s" a fault in foreign code" GE-EXPECT-ERR-LACKS
   s" PASS: a fault in foreign code dies by the signal" type cr ;

\ Habu code jumps to address 16, in the unmapped first page: the fetch faults
\ with pc = the fault address, which says nothing of the code that jumped.
: WILD-JUMP ( -- )
   GE-SRC-RESET
   s" TRUSTED: JUMP16 ( -- ) 16 execute ;" GE-SRC-LINE
   s" JUMP16" GE-SRC-LINE
   RUN-BUF
   11 s" a jump to an unmapped address" KILLED-BY
   s" habu-crash regs [sig" s" a jump to an unmapped address" GE-EXPECT-ERR-HAS
   s" E-MEMORY-FAULT" s" a jump to an unmapped address" GE-EXPECT-ERR-LACKS
   s" PASS: a jump to an unmapped address dies by the signal" type cr ;

\ The same fetch inside the data stack's low guard page, which crash.f rows 4
\ and 5 claim for a read: the fetch rule of row 2 comes first.
: GUARD-JUMP ( -- )
   GE-SRC-RESET
   s" TRUSTED: JUMP-UNDER ( -- ) data-base STACK-ABI:BASE-CELL + @ 8 - execute ;" GE-SRC-LINE
   s" JUMP-UNDER" GE-SRC-LINE
   RUN-BUF
   FOREIGN-SIG s" a jump into a guard page" KILLED-BY
   s" habu-crash regs [sig" s" a jump into a guard page" GE-EXPECT-ERR-HAS
   s" stack bounds" s" a jump into a guard page" GE-EXPECT-ERR-LACKS
   s" PASS: a jump into a guard page dies by the signal" type cr ;

\ A watchpoint at address 16 makes the trap handler itself load from the
\ unmapped first page when TARGET's breakpoint fires.
: TRAP-FAULT ( -- )
   GE-SRC-RESET
   s" 0 set-tier" GE-SRC-LINE
   s" require src/habu/debug.f" GE-SRC-LINE
   s" : TARGET ( n -- n ) 1 + ;" GE-SRC-LINE
   s" NULL-PTR BYTE-VIEW 16 + CELL-VIEW DEBUG:BPW+" GE-SRC-LINE
   s" ' TARGET DEBUG:BP+" GE-SRC-LINE
   s" 2 TARGET ." GE-SRC-LINE
   RUN-BUF
   11 s" a fault in the trap handler" KILLED-BY
   s" habu-crash regs [sig" s" a fault in the trap handler" GE-EXPECT-ERR-HAS
   s" E-MEMORY-FAULT" s" a fault in the trap handler" GE-EXPECT-ERR-LACKS
   s" PASS: a fault in the trap handler dies by the signal" type cr ;

\ The x86-64 engine has no memory-fault throw and no fetch rule, and its crash
\ handler takes a fault in foreign code for a data stack guard (exit 102).
HB-TARGET-LINUX-X86-64? 0= [if]
UNCAUGHT
CAUGHT
GUARD-JUMP
FOREIGN
[then]
WILD-JUMP
\ debug.f's breakpoints are arm64's.
HB-TARGET-LINUX-X86-64? 0= [if]
TRAP-FAULT
[then]

;package
