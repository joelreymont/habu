\ aot-band-data.f - the DATA side of the prelude-band audit.
\
\ HOLDER re-points a prelude deferred word and calls nothing outside the window, so
\ the call audit has nothing to say about it and the only thing that can refuse it
\ is the address it carries. `is` emits the dispatch cell's address as a recorded
\ chain in HOLDER's own body (test/aot-band-lib.f says why that is the vehicle), and
\ the cell is the prelude's, so the DATA audit is what stands between a prelude
\ address and a baked pointer at nothing.
\
\ THE WINDOW IS ARMED, unlike the version this replaces. That version opened
\ UNARMED to stop the inliner declining to copy a `create`d field's body, which was
\ how the address used to travel; with the inline arm off that vehicle carries
\ nothing (dot habu-decide-the-tier-374c95ff) and `is` needs no help from an
\ unarmed window - the chain is created here rather than copied in, so the decline
\ cannot reach it. AOT-ARM:WINDOW-OPEN-UNARMED is left with no caller for the same
\ reason C-CALL-SCAN-SAFE is: it belongs to the parked vehicle and to that dot.

require test/aot-band-lib.f

AOT-ARM:WINDOW-OPEN

package AOT-BAND-DATA
public

: HOLDER ( -- ) [: 0 ;] is AOT-BAND:SINK ;

;package

AOT-ARM:WINDOW-CLOSE
AOT-BAND:GO
