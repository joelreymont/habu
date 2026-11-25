\ A load through a bad address in Habu code throws 110
\ on the faulting thread: a catch receives it and the program runs on, a task
\ ends with it, and uncaught it exits 110 after its record on fd 2, with no
\ register dump. A fault in foreign code, a jump to an unmapped address and a
\ fault in a signal handler still dump the registers and die by the signal.
\ Each case is its own child: its process's end is what it checks.
require test/gate-common.f
require src/lib/process.f
require src/lib/test/runner.f

package MEMORY-FAULT-TEST
private

\ The uncaught fault's record: the address and the word that read it.
: RECORD-0$ ( -- str )
   SB-RESET
   S\" error[E-MEMORY-FAULT]: a read or write faulted\n" SB-APPEND
   S\"   = address: 0x0000000000000000\n  = in: W\n\n" SB-APPEND
   SB$ ;

: RUN-BUF ( -- )
   GE-HB-RESET
   GE-HB$ GE-SRC-BUF GE-SRC-U @ >str PROCESS:WAIT-FOREVER GE-RUN-STDIN ;

\ The child ended by the signal, not by an exit: crash.f EMIT-CRASH-HANDLER
\ re-raises it after the dump. An exit of 128 + n is no signal death.
: KILLED-BY ( n str -- ) {: sig:n label:str :}
   GT-OUTCOME@ MATCH PROCESS:ending
     exited OF drop label GE-FAIL ENDOF
     signaled OF sig <> if label GE-FAIL then ENDOF
     timeout OF label GE-FAIL ENDOF
   ;MATCH ;

: UNCAUGHT ( -- )
   GE-SRC-RESET
   s" TRUSTED: W ( -- ) 0 @ drop ;" GE-SRC-LINE
   s" W" GE-SRC-LINE
   RUN-BUF
   110 s" an uncaught load from address 0" GE-EXPECT-RC
   RECORD-0$ s" an uncaught load from address 0" GE-EXPECT-ERR
   s" PASS: an uncaught fault exits 110 after its record" type cr ;

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

\ strlen reads address 0, in the unmapped first page: the fault's pc is in libc.
: FOREIGN ( -- )
   GE-SRC-RESET
   s" PROCESS-SYMBOLS" GE-SRC-LINE
   s" FUNCTION: STRLEN$ strlen ( ^u8 -- n ) ;FUNCTION" GE-SRC-LINE
   s" NULL-PTR BYTE-VIEW STRLEN$ ." GE-SRC-LINE
   RUN-BUF
   11 s" a fault in foreign code" KILLED-BY
   s" habu-crash regs [sig" s" a fault in foreign code" GE-EXPECT-ERR-HAS
   s" error[E-MEMORY-FAULT]" s" a fault in foreign code" GE-EXPECT-ERR-LACKS
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
   s" error[E-MEMORY-FAULT]" s" a jump to an unmapped address" GE-EXPECT-ERR-LACKS
   s" PASS: a jump to an unmapped address dies by the signal" type cr ;

\ A watchpoint at address 16 makes the trap handler itself load from the
\ unmapped first page when SUBJECT's breakpoint fires.
: TRAP-FAULT ( -- )
   GE-SRC-RESET
   s" require src/habu/debug.f" GE-SRC-LINE
   s" : SUBJECT ( n -- n ) 1 + ;" GE-SRC-LINE
   s" NULL-PTR BYTE-VIEW 16 + CELL-VIEW DEBUG:BPW+" GE-SRC-LINE
   s" ' SUBJECT DEBUG:BP+" GE-SRC-LINE
   s" 2 SUBJECT ." GE-SRC-LINE
   RUN-BUF
   11 s" a fault in the trap handler" KILLED-BY
   s" habu-crash regs [sig" s" a fault in the trap handler" GE-EXPECT-ERR-HAS
   s" error[E-MEMORY-FAULT]" s" a fault in the trap handler" GE-EXPECT-ERR-LACKS
   s" PASS: a fault in the trap handler dies by the signal" type cr ;

UNCAUGHT
CAUGHT
FOREIGN
WILD-JUMP
TRAP-FAULT

;package
