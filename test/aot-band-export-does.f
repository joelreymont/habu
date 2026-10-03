\ aot-band-export-does.f - a window word made through the public name EXPORT
\ gave a private does> definer.
\
\ A word a definer creates ends in a branch to the definer's does> clause, which
\ is a dictionary record of its own, named after the definer with `;does` added
\ (habu2.f DOES-REC), in the definer's wordlist. MK is private, so its clause is
\ too, and no qualifier reaches either. `EXPORT MK` publishes the definer under a
\ public record and its clause beside it (habu2.f C-EXPORT), so SEVEN's branch
\ bakes AOT-BAND-EXPORT-DOES:MK;does, the name the export made, exactly as it
\ would for a definer defined public. The case prints the loader's value for
\ SEVEN before it captures, so the suite sees the loader and the capture agree
\ on the clause SEVEN runs.
\
\ The suite runs this under an empty band, so the call-band audit admits the
\ package as the target's and the site reaches the scope audit.

require test/aot-band-lib.f

package AOT-BAND-EXPORT-DOES

: MK ( n -- ) create , does> ( -- n ) @ ;

public
EXPORT MK
;package

\ The does> definer leaves the DATA cursor off a cell boundary, and a window's
\ DATA base must sit on one.
align

AOT-ARM:WINDOW-OPEN
7 AOT-BAND-EXPORT-DOES:MK SEVEN
AOT-ARM:WINDOW-CLOSE

s" aot-band-export-does: seven " type SEVEN .
AOT-BAND:GO
