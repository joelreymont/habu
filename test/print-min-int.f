\ `.` and `u.` over MIN-N ($8000000000000000), whose negation is itself. The
\ engine's signed printer (src/habu/rt.f G-PRINT9) negates and then divides
\ unsigned, which reads the negated pattern as its magnitude; a signed divide
\ there writes bytes below '0'. `u.` shares the itoa shape: an edit to either
\ printer must keep both. src/lib/fmt.f's checked renderer is covered by
\ src/lib/fmt-test.f.

require src/lib/test.f
require src/lib/string.f
require src/lib/genio-core.f

package PRINT-MIN-INT
private

$8000000000000000 constant MIN-N
$7FFFFFFFFFFFFFFF constant MAX-N
$100 constant SINK-CAP
$A constant LF
$5E constant SINK-MARK                  \ the device's own state cell

create SINK SINK-CAP allot
variable SINK-N
variable SINK-OVER                      \ a write past the buffer, latched
variable SINK-DEV

\ `.` and `u.` write through the engine's output funnel (src/lib/genio-core.f):
\ the sink is a write-only device appending to a buffer. Each case installs it,
\ prints one number, restores the terminal and only then asserts, so a failing
\ assertion reaches the terminal. A write operation may not throw, so an overrun
\ is latched and checked once at the end.
: SINK-RESET ( -- )
   0 SINK-N !  0 SINK-OVER ! ;

: SINK-PUT ( n -- ) {: c:n :}
   SINK-N @ SINK-CAP >= if 1 SINK-OVER ! exit then
   c SINK SINK-N @ + c!
   SINK-N @ 1+ SINK-N ! ;

: SINK-WRITE ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0 ?do a i + c@ SINK-PUT loop ;

: SINK-KEY ( -- n )
   GENIO:E-IO throw ;

: SINK-READY? ( -- bool )
   0 0= 0= ;

: SINK-READ ( ptr u8 n -- n ) {: a:ptr cap:n :}
   0 ;

: SINK-ACCEPT ( ptr u8 n -- n ) {: a:ptr cap:n :}
   0 ;

: SINK-NOTHING ( -- ) ;

: SINK-BUILD ( -- GENIO:device )
   [: SINK-PUT ;] [: SINK-KEY ;] [: SINK-READY? ;] [: SINK-READ ;]
   [: SINK-WRITE ;] [: SINK-ACCEPT ;] [: SINK-NOTHING ;] [: SINK-NOTHING ;]
   SINK-MARK GENIO:DEVICE ;

: SINK@ ( -- GENIO:device )
   SINK-DEV @ GENIO:>DEVICE ;

\ Both printers end their text with a newline, so the case compares what came
\ before it and asserts the newline separately.
: BODY$ ( -- ptr u8 n )
   SINK-N @ 0= if SINK 0 exit then
   SINK SINK-N @ 1- ;

: TERMINATOR ( -- n )
   SINK-N @ 0= if 0 exit then
   SINK SINK-N @ 1- + c@ ;

: DOT-CAPTURED ( n -- ) {: v:n :}
   SINK-RESET
   SINK@ GENIO:OUTPUT!
   v .
   GENIO:TERMINAL GENIO:OUTPUT! ;

: U-CAPTURED ( n -- ) {: v:n :}
   SINK-RESET
   SINK@ GENIO:OUTPUT!
   v u.
   GENIO:TERMINAL GENIO:OUTPUT! ;

: CHECKED ( ptr u8 n -- ) {: wa:ptr wu:n :}
   BODY$ wa wu T$=
   TERMINATOR LF T=
   SINK-OVER @ 0 T= ;

: T-DOT ( n ptr u8 n -- ) {: v:n wa:ptr wu:n :}
   wa wu T-LABEL
   v DOT-CAPTURED
   wa wu CHECKED ;

: T-U. ( n ptr u8 n -- ) {: v:n wa:ptr wu:n :}
   wa wu T-LABEL
   v U-CAPTURED
   wa wu CHECKED ;

public

: PRINT-MIN-INT-RUN ( -- )
   T-RESET
   SINK-BUILD GENIO:DEVICE>N SINK-DEV !

   MIN-N     s" -9223372036854775808" T-DOT
   MIN-N 1+  s" -9223372036854775807" T-DOT   \ the neighbour that negates cleanly
   MAX-N     s" 9223372036854775807"  T-DOT
   -1        s" -1"                   T-DOT
   0         s" 0"                    T-DOT
   7         s" 7"                    T-DOT

   \ MIN-N's pattern is the magnitude the signed printer needs.
   MIN-N     s" 9223372036854775808"  T-U.
   MIN-N 1+  s" 9223372036854775809"  T-U.
   MAX-N     s" 9223372036854775807"  T-U.
   -1        s" 18446744073709551615" T-U.
   0         s" 0"                    T-U.

   SINK@ GENIO:CLOSE
   T-REPORT ;

;package

PRINT-MIN-INT:PRINT-MIN-INT-RUN
