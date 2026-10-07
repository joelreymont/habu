\ ffi-callback-direct-child.f - C calling a callback entry directly: every
\ argument register into MARSHAL's body, the fallback of UNSET, whose body was
\ never stored, and a full-width n result back through START-B.
\ lib/ffi-callback-test.f runs it and appends it to its transcript as the child
\ `direct`.
\
\ It is a window child because an entry is a bare code address: a FUNCTION:
\ row resolves a symbol and an entry has none, and only package FFI's owner
\ words call an address. test/native-window-owner-child.f loads this file after
\ test/mcode-window-prepare.f, which reopens FFI before the seal and publishes
\ those calls. The sealed product refuses the reopen (`hb: internal engine
\ word`, exit 70), so the child runs on the unsealed engine
\ test/whitebox-child.f names. Each fact prints `ok   <label>` or
\ `FAIL <label>`, then the report `test: ok`, then the window `window: 0`.

require lib/test.f
require test/ffi-callback-fixture.f

package FFI-CB-TEST
using FFI-CB

create WANT-FLOATS                    \ 1.0 .. 8.0 as IEEE 754 doubles
   $3FF0000000000000 , $4000000000000000 , $4008000000000000 , $4010000000000000 ,
   $4014000000000000 , $4018000000000000 , $401C000000000000 , $4020000000000000 ,

TYPED-VARIABLE MARSHAL-RESULT r

\ One fact: asserted under its own label and printed as a transcript line.
: FACT ( bool ptr u8 n -- ) {: ok:bool a:ptr u:n :}
   a u T-LABEL
   ok TTRUE
   ok if s" ok   " else s" FAIL " then type
   a u type cr ;

\ C calling an entry with one integer argument.
: CALL1 ( n n -- n ) {: arg:n fn:n :}
   FFI:RESET
   arg 0 FFI:VALUE!
   1 fn FFI:CALL-AT ;

\ Every argument register loaded: x0..x7 and d0..d7.
: MARSHAL-CALL ( n -- r ) {: fn:n :}
   FFI:RESET
   $123456789ABCDEF0 0 FFI:VALUE!
   $1FFFFFFFE 1 FFI:VALUE!           \ a C int -2 under a dirty high half
   $7FFFFFFFD 2 FFI:VALUE!           \ an unsigned $FFFFFFFD under one
   MARSHAL-BYTE 3 FFI:READABLE!
   40 4 FFI:VALUE!  50 5 FFI:VALUE!  60 6 FFI:VALUE!  70 7 FFI:VALUE!
   1.0 0 FFI:FLOAT!  2.0 1 FFI:FLOAT!  3.0 2 FFI:FLOAT!  4.0 3 FFI:FLOAT!
   5.0 4 FFI:FLOAT!  6.0 5 FFI:FLOAT!  7.0 6 FFI:FLOAT!  8.0 7 FFI:FLOAT!
   0 fn FFI:CALL-ABI-R-AT ;

: FLOAT-BITS ( ptr r -- n )
   BYTE-VIEW CELL-VIEW @ ;

: FLOATS-SEEN? ( -- bool )
   true 8 0 ?do i SEEN-FLOATS FLOAT-BITS WANT-FLOATS i cells + @ = and loop ;

: INTS-SEEN? ( -- bool )
   4 SEEN-INTS @ 40 =
   5 SEEN-INTS @ 50 = and
   6 SEEN-INTS @ 60 = and
   7 SEEN-INTS @ 70 = and ;

: CASE-DIRECT ( -- )
   MARSHAL TASK:SELF-CONTEXT ENTRY MARSHAL-CALL MARSHAL-RESULT !
   0 SEEN-INTS @ $123456789ABCDEF0 = s" n arrives whole" FACT
   1 SEEN-INTS @ -2 = s" i32 is sign-extended from the low half" FACT
   2 SEEN-INTS @ $FFFFFFFD = s" u32 is the low half" FACT
   3 SEEN-INTS @ $5A = s" ptr u8 is the address C passed" FACT
   INTS-SEEN? s" x4..x7 arrive in order" FACT
   FLOATS-SEEN? s" d0..d7 arrive in order" FACT
   MARSHAL-RESULT FLOAT-BITS $4020000000000000 = s" an r result returns in d0" FACT
   MARSHAL FAULT@ 0= s" and nothing faulted" FACT
   MARSHAL UNBIND
   9 UNSET TASK:SELF-CONTEXT ENTRY CALL1 7 = s" a body never stored answers the fallback" FACT
   UNSET FAULT@ E-FFI-CALLBACK-STATE = s" and faults E-FFI-CALLBACK-STATE" FACT
   UNSET UNBIND
   $123456789ABCDEF0 START-B TASK:SELF-CONTEXT ENTRY CALL1
   $123456789ABCDEF0 = s" a normal n result preserves the high half" FACT
   START-B UNBIND ;

T-RESET
CASE-DIRECT
T-REPORT

;using
;package
