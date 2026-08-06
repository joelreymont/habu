\ codegen-compare-cabi.f - calling a C twin. One concern: the call shapes the
\ reference column reaches its functions through.
\
\ FOURTEEN SHAPES, AND WHY THE NUMBER MATTERS. Every twin takes longs and
\ doubles and returns one or the other, so the whole reference column is reached
\ through the fourteen signatures below. That number is not an implementation
\ detail: an FFI call costs far more than a `bl`, and the only honest way to
\ subtract that cost is to measure the SAME shape with an empty callee. The
\ harness therefore measures one floor per row, by running the row's own timing
\ body against tools/clang/twins.c's empty function of the same signature, and
\ the difference is the emitted code. A single zero-argument floor would have
\ left every row carrying the marshalling of its own arity.
\
\ WHY THIS IS A TRUSTED BOUNDARY, AND HOW SMALL IT IS. A foreign call is not
\ something the checker can certify: it has to be told that a cell is a valid
\ code address and that the argument buffer holds what the callee's contract
\ expects, and neither is derivable from a habu type. This is that boundary, in
\ one file, one word per shape, each one fixing its own arity and its own
\ register bank. Nothing here takes an arity from a caller and nothing here
\ loops over a length: a call with four integer arguments is a word that stores
\ four integer arguments. Retirement owner: habu-ptx-m1-c-1df1d6e7, the same
\ exact-binding capability lib/ffi-abi.f's other schemas are waiting on.
\
\ THE ARGUMENTS ARE STORED DIRECTLY RATHER THAN THROUGH FFI:VALUE!, and the
\ reason is resolution. FFI:VALUE! bounds-checks a slot index and clears a
\ writable extent per argument, which costs about eleven nanoseconds an
\ argument - four times what the C bodies being measured cost. The floor
\ subtracts it exactly either way, but a floor four times the signal turns every
\ small row into noise, so the marshalling here is the store and nothing else.
\ Nothing is given up: no schema in this file declares a writable extent, so
\ there is no extent to clear, and RESET at load leaves every one of them zero
\ for the life of the process.
\
\ THE ONE THING A CALLER MUST NOT DO is nest one of these inside another, or
\ inside any other FFI call: they all marshal through the one task-scratch
\ argument buffer lib/ffi-abi.f owns. Every caller here is a timing body that
\ finishes its call before the next begins.

require lib/errors.f
require lib/prelude.f
require lib/string.f
require lib/ffi-abi.f
require lib/process-fork.f
require tools/codegen-compare-cc.f

package CODEGEN-CABI

private

64 constant SYM-CAP
256 constant PATH-CAP

SYM-CAP BUFFER: SYM-BYTES
PATH-CAP BUFFER: PATH-BYTES

variable LIB-HANDLE

public

\ Open the reference library. Called once, after the toolchain has built it.
\
\ Mapping it is the BUILDING process's job and nobody else's. dyld is not
\ fork-safe: loading an image that is not already mapped runs its loader, and
\ in a forked child that takes SIGBUS inside dyld itself rather than returning
\ an error (tools/codegen-compare-cc.f, THE OWNER OF THE TREE). An inherited
\ handle is fine and is the supported shape -- the parent maps, the children
\ share the mapping and take this word's first branch -- so what is refused is
\ exactly the case that would have to load: no handle, and not the builder.
\ A gate that forks members which reach the reference column calls
\ CODEGEN-CABI:PREPARE before it forks.
: OPEN ( -- )
   LIB-HANDLE @ 0<> if exit then
   PROC-FORK:CHILD? if E-CODEGEN-CLANG-FORK throw then
   CODEGEN-CC:LIB$ PATH-BYTES FFI:CSTR
   PATH-BYTES FFI:NOW FFI:DLOPEN dup LIB-HANDLE !
   0= if E-CODEGEN-CLANG-SYMBOL throw then
   FFI:RESET ;

\ Build the reference and map it, in THIS process, so that processes forked
\ from here inherit a library they never have to load. Answers whether there is
\ a reference column at all, so a host without a C compiler prepares nothing
\ and every child still reports the column absent for the usual reason.
: PREPARE ( -- bool )
   CODEGEN-CC:READY? dup 0= if exit then
   OPEN ;

\ Whether THIS process holds the reference mapping. A fact reader, minted for
\ the probes that must assert their own precondition: the refusal contract
\ (OPEN's fork guard) is only testable from a chain that maps nothing, and a
\ test that ASSUMES that state red-flips with whichever files shared its
\ process (dot habu-attr-the-compare-2f98fcfc).
: MAPPED? ( -- bool )
   LIB-HANDLE @ 0<> ;

\ Where the twin of this name starts. A name the library does not carry is a
\ claim the comparison made and did not keep, so it throws rather than answering
\ zero and calling it later.
: FN ( ptr u8 n -- n ) {: a:ptr u:n :}
   u SYM-CAP >= if E-CODEGEN-CLANG-SYMBOL throw then
   a u SYM-BYTES FFI:CSTR
   LIB-HANDLE @ SYM-BYTES FFI:DLSYM
   dup 0= if E-CODEGEN-CLANG-SYMBOL throw then ;

\ ---- integer shapes ----------------------------------------------------------
\ Arguments go to x0 upward, which is where AAPCS64 puts an integer argument,
\ and the ninth goes to the caller-packed stack slot, which is where AAPCS64
\ puts an argument that has run out of registers.

TRUSTED: I0 ( n -- n ) {: fn:n :}
   FFI:ARGS FFI:REG-LENS 0 fn ffi-call-bounded ;

TRUSTED: I1 ( n n -- n ) {: a:n fn:n :}
   FFI:ARGS {: p:ptr :}
   a p !
   p FFI:REG-LENS 1 fn ffi-call-bounded ;

TRUSTED: I2 ( n n n -- n ) {: a:n b:n fn:n :}
   FFI:ARGS {: p:ptr :}
   a p !
   b p CELL + !
   p FFI:REG-LENS 2 fn ffi-call-bounded ;

TRUSTED: I3 ( n n n n -- n ) {: a:n b:n c:n fn:n :}
   FFI:ARGS {: p:ptr :}
   a p !
   b p CELL + !
   c p 2 cells + !
   p FFI:REG-LENS 3 fn ffi-call-bounded ;

TRUSTED: I5 ( n n n n n n -- n ) {: a:n b:n c:n d:n e:n fn:n :}
   FFI:ARGS {: p:ptr :}
   a p !
   b p CELL + !
   c p 2 cells + !
   d p 3 cells + !
   e p 4 cells + !
   p FFI:REG-LENS 5 fn ffi-call-bounded ;

TRUSTED: I6 ( n n n n n n n -- n ) {: a:n b:n c:n d:n e:n f:n fn:n :}
   FFI:ARGS {: p:ptr :}
   a p !
   b p CELL + !
   c p 2 cells + !
   d p 3 cells + !
   e p 4 cells + !
   f p 5 cells + !
   p FFI:REG-LENS 6 fn ffi-call-bounded ;

TRUSTED: I9 ( n n n n n n n n n n -- n )
   {: a:n b:n c:n d:n e:n f:n g:n h:n k:n fn:n :}
   FFI:ARGS {: p:ptr :}
   a p !
   b p CELL + !
   c p 2 cells + !
   d p 3 cells + !
   e p 4 cells + !
   f p 5 cells + !
   g p 6 cells + !
   h p 7 cells + !
   k FFI:STACK !
   p FFI:FLOATS FFI:STACK FFI:REG-LENS FFI:STACK-LENS 1 fn ffi-call-abi-bounded ;

\ ---- shapes that answer a double ---------------------------------------------

TRUSTED: ID ( n n -- r ) {: a:n fn:n :}
   FFI:ARGS {: p:ptr :}
   a p !
   p FFI:FLOATS FFI:STACK FFI:REG-LENS FFI:STACK-LENS 0 fn ffi-call-abi-r-bounded ;

TRUSTED: IID ( n n n -- r ) {: a:n b:n fn:n :}
   FFI:ARGS {: p:ptr :}
   a p !
   b p CELL + !
   p FFI:FLOATS FFI:STACK FFI:REG-LENS FFI:STACK-LENS 0 fn ffi-call-abi-r-bounded ;

TRUSTED: IIID ( n n n n -- r ) {: a:n b:n c:n fn:n :}
   FFI:ARGS {: p:ptr :}
   a p !
   b p CELL + !
   c p 2 cells + !
   p FFI:FLOATS FFI:STACK FFI:REG-LENS FFI:STACK-LENS 0 fn ffi-call-abi-r-bounded ;

\ ---- shapes that take a double -----------------------------------------------
\ A double goes to d0 upward, which is a different register bank from the
\ integer arguments, so a call that takes both fills both buffers and neither
\ displaces the other.

TRUSTED: DD ( r n -- r ) {: x:r fn:n :}
   FFI:FLOATS {: q:ptr :}
   x q !
   FFI:ARGS q FFI:STACK FFI:REG-LENS FFI:STACK-LENS 0 fn ffi-call-abi-r-bounded ;

TRUSTED: DDD ( r r n -- r ) {: x:r y:r fn:n :}
   FFI:FLOATS {: q:ptr :}
   x q !
   y q CELL + !
   FFI:ARGS q FFI:STACK FFI:REG-LENS FFI:STACK-LENS 0 fn ffi-call-abi-r-bounded ;

TRUSTED: DDDD ( r r r n -- r ) {: x:r y:r z:r fn:n :}
   FFI:FLOATS {: q:ptr :}
   x q !
   y q CELL + !
   z q 2 cells + !
   FFI:ARGS q FFI:STACK FFI:REG-LENS FFI:STACK-LENS 0 fn ffi-call-abi-r-bounded ;

TRUSTED: DI ( r n -- n ) {: x:r fn:n :}
   FFI:FLOATS {: q:ptr :}
   x q !
   FFI:ARGS q FFI:STACK FFI:REG-LENS FFI:STACK-LENS 0 fn ffi-call-abi-bounded ;

TRUSTED: DIII ( r n n n n -- n ) {: x:r a:n b:n c:n fn:n :}
   FFI:ARGS {: p:ptr :}
   FFI:FLOATS {: q:ptr :}
   x q !
   a p !
   b p CELL + !
   c p 2 cells + !
   p q FFI:STACK FFI:REG-LENS FFI:STACK-LENS 0 fn ffi-call-abi-bounded ;

;package
