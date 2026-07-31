\ native-run-fixture.f - publishing an emission into the engine's own code space
\ and calling it. One concern: the engine boundary that makes emitted bytes run.
\
\ WHY THIS EXISTS AT ALL. A table of expected instruction words is necessary and
\ not sufficient: it can only disagree with an emitter that changed, never with
\ one that was always wrong, because the expected words and the emitter can be
\ wrong in the same way. Running the bytes on real arguments and comparing the
\ answer with the source-level arithmetic is the check that has no such hole, and
\ two callers need it - the emission suite, which proves the bytes compute what
\ the shape says, and the comparison harness, which proves the new chain's answer
\ is the old emitter's answer on the same pinned inputs.
\
\ THE OFFSETS COME FROM THE SOURCE MAP, NOT FROM AN INDEX. Each instruction is
\ stored at the byte offset the emitter's own source map records for it, rather
\ than at four times its position. A map that lost a row or moved an offset
\ therefore stops the published routine from running at all, instead of being
\ checked only where a case happens to look.
\
\ TWO TRUSTED WORDS AND NOTHING ELSE. `patch32` is the engine's code-injection
\ primitive and `ffi-call-bounded` is its C-ABI call; both are refused from
\ checked code, so each is wrapped in exactly one trusted word that does that one
\ thing. The address stored to comes from the source map and the word stored
\ comes from the emission, so neither trusted word chooses anything.
\
\ TWO WAYS IN, AND THEY ARE THE TWO CONVENTIONS. A routine compiled under the C
\ ABI takes its arguments in x0 upwards and leaves its result in x0, so it is
\ entered through the engine's bounded foreign call - EXEC0..EXEC3 below. A
\ routine compiled under the data-stack convention takes its arguments out of the
\ caller's data stack and leaves its results there, which is exactly what an
\ ordinary Habu word does, so it is entered the way the engine enters any word:
\ ENTER0..ENTER3 push the routine's address and run `execute`, whose whole body
\ is a branch-and-link to that address with the data-stack pointer live. That is
\ the SAME mechanism the interpreter uses - `EM-INTERPRET-FIND` resolves a word
\ to its code address and branches to it, and `execute` is that branch with the
\ address taken off the stack - so nothing here models a call, and the engine's
\ own dictionary lookup is the only part of the path that is missing.
\
\ WHY `execute` IS THE WHOLE PUBLICATION. An xt in this engine IS a code address:
\ `'` pushes the address the dictionary holds and `execute` branches to it
\ (src/habu/habu1.f BEXEC). Giving the emitted routine a dictionary record would
\ add a name and a lookup and change nothing about how it is entered, so the
\ smallest honest boundary is one trusted word per arity that does nothing but
\ execute an address the caller already has. `src/habu/habu2.f` uses exactly this
\ shape for its own keyword dispatch (`EM-HXT-EXECUTE`).
\
\ NOTHING IN THIS FILE ASSERTS. It defines no case and prints nothing: it is a
\ fixture, not a test, so it never names the harness verdict word and no gate
\ schedules it on its own.

require lib/ffi.f
require src/compiler/native/emit.f

package NRUN

private

TRUSTED: POKE ( n n -- ) patch32 ;

public

\ Store the sealed emission into the free code slot and answer its entry address.
\ It must be called from inside a definition: a top-level `cp@` patch overwrites
\ the line being interpreted.
: PUBLISH ( -- n )
   cp@ {: fn:n :}
   A64EMIT:INSNS {: n:n :}
   n 0 ?do
      i A64EMIT:WORD@  fn i A64EMIT:MAP-OFFSET@ +  POKE
   loop
   fn ;

TRUSTED: EXEC0 ( n -- n ) {: fn:n :}
   FFI:RESET
   FFI:ARGS FFI:REG-LENS 0 fn ffi-call-bounded ;

TRUSTED: EXEC1 ( n n -- n ) {: a:n fn:n :}
   FFI:RESET
   a 0 FFI:VALUE!
   FFI:ARGS FFI:REG-LENS 1 fn ffi-call-bounded ;

TRUSTED: EXEC2 ( n n n -- n ) {: a:n b:n fn:n :}
   FFI:RESET
   a 0 FFI:VALUE!
   b 1 FFI:VALUE!
   FFI:ARGS FFI:REG-LENS 2 fn ffi-call-bounded ;

TRUSTED: EXEC3 ( n n n n -- n ) {: a:n b:n c:n fn:n :}
   FFI:RESET
   a 0 FFI:VALUE!
   b 1 FFI:VALUE!
   c 2 FFI:VALUE!
   FFI:ARGS FFI:REG-LENS 3 fn ffi-call-bounded ;

\ ---- entering a routine as a Habu word ---------------------------------------
\ The arguments are already on the data stack, which is where a routine compiled
\ under the data-stack convention reads them from; the address goes on top and
\ `execute` branches to it. Each arity is its own word only because the declared
\ effect has to say how many cells the routine consumes and leaves - the body is
\ the same one word in every case, and none of them touches an argument.
TRUSTED: ENTER0 ( n -- )         execute ;
TRUSTED: ENTER1 ( n n -- n )     execute ;
TRUSTED: ENTER2 ( n n n -- n )   execute ;
TRUSTED: ENTER3 ( n n n n -- n ) execute ;

;package
