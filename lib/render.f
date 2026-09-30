\ render.f - a fixed byte-exact output buffer for tool renderers. A renderer
\ appends into the buffer and hands back its bytes for a byte-for-byte snapshot
\ comparison. Separate from the lib/string.f SB builder (1 KiB) because a full
\ report is larger. Integer text is built with the same digit recursion as
\ lib/fmt.f SB-U.
\
\ The module lives in `package RENDER`. RENDER:RESET clears the shared buffer,
\ RENDER:RB+ appends a counted string, RENDER:RB# appends a signed decimal and
\ RENDER:RB$ hands back the accumulated bytes. The tails keep their bare-operator
\ RB+/RB#/RB$ spelling because a bare `+` public word would shadow the arithmetic
\ `+` used throughout the module. The byte emitter and the buffer cells are
\ package-private. A full buffer throws E-RB-FULL rather than truncate.

package RENDER

$4000 constant RB-CAP                        \ 16 KiB
create RB-BUF RB-CAP allot
variable RB-N  variable RB-CP
-6210 constant E-RB-FULL

public
: RESET ( -- ) 0 RB-N ! ;
private
: RB-C ( n -- )                              \ append one byte
   RB-N @ RB-CAP >= if E-RB-FULL throw then  \ guard: never silently truncate output
   RB-BUF RB-N @ + c!  RB-N @ 1+ RB-N ! ;
public
: RB+ ( ptr u8 n -- ) {: a:ptr u :}          \ append a string
   0 RB-CP ! begin RB-CP @ u < while a RB-CP @ + c@ RB-C  RB-CP @ 1+ RB-CP ! repeat ;
private
: RB-U ( n -- )                              \ append unsigned decimal
   dup 10 < if 48 + RB-C exit then
   dup 10 / RECURSE  10 mod 48 + RB-C ;
public
: RB# ( n -- )                               \ append signed decimal
   dup 0 < if 45 RB-C negate then RB-U ;
: RB$ ( -- ptr u8 n ) RB-BUF RB-N @ ;

;package
