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
\ THERE IS NO CALLING-CONVENTION BINDING YET (dot habu-bind-arm64-arg-f76afa3a).
\ A block argument gets the next free register of the routine's own pool and a
\ returned value stays where it was computed. For the straight-line shapes both
\ callers run, that happens to be the C ABI's own registers, which is what makes
\ calling them meaningful - and every caller asserts that the returned value
\ really is in register zero before it calls, so a change in allocation reds on
\ the assertion instead of silently comparing whatever x0 held.
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

;package
