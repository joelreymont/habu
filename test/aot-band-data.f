\ aot-band-data.f - the DATA side of the prelude-band audit.
\
\ HOLDER reads a prelude buffer and calls nothing outside the window, so the call
\ audit has nothing to say about it and the only thing that can refuse it is the
\ address it carries. The window is opened UNARMED on purpose: with the window's
\ start declared, the engine's inliner declines to copy `BUF`'s body and emits a
\ call instead, and the case would test the call audit a second time. Unarmed, the
\ body is copied, the copy keeps this process's address for BUF, and the DATA
\ audit is what stands between that address and a baked pointer at nothing.

require test/aot-band-lib.f

AOT-BAND:OPEN-UNARMED

package AOT-BAND-DATA
public

: HOLDER ( -- n ) AOT-BAND:BUF @ ;

;package

AOT-BAND:CLOSE
AOT-BAND:GO
