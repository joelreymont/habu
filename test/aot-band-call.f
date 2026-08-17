\ aot-band-call.f - the call side of the prelude-band audit.
\
\ TWO WINDOW WORDS, and only one of them calls out of the window. NAMER holds the
\ callee's spelling as a STRING and never calls it; CALLER calls it twice. A
\ refusal that named NAMER, or that named both, would be reading text rather than
\ call sites, so the suite asserts the diagnostic names CALLER and does not name
\ NAMER - the two words are otherwise the same shape and sit in the same window.

require test/aot-band-lib.f

AOT-ARM:WINDOW-OPEN

package AOT-BAND-CALL
public

\ DATA of the window's own, so the window's DATA span is not empty: the case that
\ moves the DATA mark alone needs a base and a top that differ, and an empty span
\ would make that mark legal and the case vacuous.
create WBUF 8 allot

\ The spelling, in the one role that must NOT be a call.
: NAMER ( -- ptr u8 n ) s" AOT-BAND:CALLEE" ;

: CALLER ( n -- n ) AOT-BAND:CALLEE AOT-BAND:CALLEE ;

;package

AOT-ARM:WINDOW-CLOSE
AOT-BAND:GO
