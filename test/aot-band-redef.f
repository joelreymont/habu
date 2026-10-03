\ aot-band-redef.f - a does> definer replaced through `undefine`.
\
\ A definer's does> clause is a dictionary record of its own, named after the
\ definer with `;does` added (habu2.f DOES-REC). A word the definer creates ends
\ in a branch to that clause, and when the word is in a window the capture bakes
\ the branch as a call site by that name. MKV is defined, undefined and defined
\ again, so two clause records have carried the name MKV;does. SEVEN, made by
\ the second MKV inside the window, must bake the second clause's name, and the
\ name audit asks the engine's own find which record the name answers. It is the
\ second clause only because `undefine` retires a definer's clause with the
\ definer (src/habu/xref.f XREF-RETIRE-INDEX): a clause left live keeps the name,
\ and the find answers the older of two live rows.
\
\ OLD was made by the first MKV before it was undefined, and the loader still
\ runs the first clause for it: retiring a name moves no code. The case prints
\ both values before it captures, so the suite sees the loader and the capture
\ agree on the clause SEVEN runs.
\
\ The suite runs this under an empty band, so the call-band audit admits the
\ package as the target's and the site reaches the name audit.

require test/aot-band-lib.f

package AOT-BAND-REDEF
public

: MKV ( n -- ) create , does> ( -- n ) @ ;
7 MKV OLD
undefine MKV
: MKV ( n -- ) create , does> ( -- n ) @ 1 + ;

;package

\ The does> definer leaves the DATA cursor off a cell boundary, and a window's
\ DATA base must sit on one.
align

AOT-ARM:WINDOW-OPEN
7 AOT-BAND-REDEF:MKV SEVEN
AOT-ARM:WINDOW-CLOSE

s" aot-band-redef: old " type AOT-BAND-REDEF:OLD .
s" aot-band-redef: new " type SEVEN .
AOT-BAND:GO
