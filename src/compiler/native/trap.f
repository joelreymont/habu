\ trap.f - the one routine a compiled trap branches to, and the message it ends
\ the process with. One concern: turning the name a trap site is about - a
\ family whose tag matched no arm, or a callee that came back - into that
\ message and its exit code.
\
\ A trap site carries its message's address, length and exit code. While the
\ site compiles, elaborate.f TRAP-ARGS copies the bytes into the store source
\ strings use, so each message is built for its site and nothing here outlives
\ it or bounds the count or length of the names a program traps on.

require lib/prelude.f
require lib/errors.f
require src/core/bytes.f
require src/core/engine-error.f

package NTRAP
private

\ ---- the message -------------------------------------------------------------
\ The tag form writes the exact bytes the engine's own inline trap writes, so a
\ compiled MATCH and an interpreted one end the process saying the same thing.
\ The trailing newline of those bytes is die's, written for every non-empty
\ message (src/habu/habu1.f BDIE); the engine's inline trap carries its own.
\ The message is built in a byte row that grows to hold the whole name.
\ Growing moves the row, so its span is taken after the last byte is written,
\ and it holds until the next message is built.
DYNAMIC-BUFFER MSG u8

: PUT$ ( ptr u8 n n -- n ) {: a:ptr u:n at:n :}
   at u + MSG-RESERVE
   a  at MSG  u BYTE-COPY
   at u + ;

: MSG$ ( n -- ptr u8 n ) {: u:n :}
   0 MSG u ;

\ No message could name an empty name.
: NAME-CK ( n -- )
   0 <= if E-NTRAP-NAME throw then ;

public

\ ---- what the chain asks ------------------------------------------------------
\ A scrutinee of the named family whose tag matched no arm exits BAD-TAG.
: BAD-TAG ( ptr u8 n -- ptr u8 n n )
   {: f:ptr u:n :}
   u NAME-CK
   s" hb: bad " 0 PUT$ {: a:n :}
   f u a PUT$ {: b:n :}
   S\" \x20tag" b PUT$ MSG$  ENGINE-ERROR:BAD-TAG ;

\ A named callee that came back exits CODE-CERT: its certificate was false.
: RETURNED ( ptr u8 n -- ptr u8 n n )
   {: w:ptr u:n :}
   u NAME-CK
   s" hb: " 0 PUT$ {: a:n :}
   w u a PUT$ {: b:n :}
   S\" \x20returned" b PUT$ MSG$  ENGINE-ERROR:CODE-CERT ;

\ ---- the routine every trap site branches to ----------------------------------
\ Trap messages are resolved while compiling. The target needs only the engine
\ primitive, including while the source runtime itself is being rebuilt.
: ROUTINE$ ( -- ptr u8 n )
   s" die" ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;package
