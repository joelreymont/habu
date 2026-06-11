\ t-stepper.fs — the Forth single-step debugger (src/cg/stepper.fs): STEP evaluates a
\ snippet one token at a time (printing the stack after each) and leaves the final
\ result on the stack. Run: gforth test/t-stepper.fs -e bye
require ../src/cg/stepper.fs
require tester.fs
\ stepping "5 dup * 3 +" prints the trace and leaves 28
T{ s" 5 dup * 3 +" STEP -> 28 }T
T{ s" 10 2 - 4 *"  STEP -> 32 }T
