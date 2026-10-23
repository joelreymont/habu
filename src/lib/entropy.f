\ entropy.f - bytes from the operating system's random generator.
\
\ FILL writes n bytes from the kernel's cryptographically secure generator
\ into the caller's span, through libc getentropy, which macOS and Linux
\ (glibc 2.25 and later) both provide. getentropy answers at most 256 bytes a
\ call, so FILL asks in chunks of that size. It blocks only until the
\ generator is first seeded at boot. A call the generator refuses is E-FILL;
\ the span may then hold some fresh bytes and is the caller's to discard. A
\ negative length is E-SPAN-LENGTH, before any byte is written.
\
\ STORAGE CLASS. CALLER-OWNED: the module keeps no state.

require src/lib/errors.f
require src/lib/ffi-abi.f

package ENTROPY
public

\ Codes -9540..-9549, the block after UTF16's.
-9540 constant E-FIRST
-9549 constant E-LAST
-9540 constant E-FILL       \ getentropy refused to fill a chunk

private

256 constant CHUNK-MAX      \ getentropy's per-call ceiling, GETENTROPY_MAX

PROCESS-SYMBOLS
FUNCTION: GETENTROPY-CALL getentropy ( ptr u8 n -- i32 )
   0 1 WRITES-ARG
;FUNCTION

public

: FILL ( ptr u8 n -- )
   {: buf len:n :}
   len 0 < if E-SPAN-LENGTH throw then
   len 0 ?do
      buf i +  len i - CHUNK-MAX min  GETENTROPY-CALL 0<> if E-FILL throw then
   CHUNK-MAX +loop ;

;package
