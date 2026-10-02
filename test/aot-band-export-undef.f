\ aot-band-export-undef.f - the private original of an EXPORTed does> definer
\ undefined while its public alias lives.
\
\ `EXPORT MK` gives the private definer a public record and its does> clause a
\ public record beside it (habu2.f C-EXPORT). `undefine MK` in the private
\ section retires the original and its clause as a pair (src/habu/xref.f
\ XREF-RETIRE-INDEX) and leaves the public pair alone, so the alias still names
\ the definer and its clause. EIGHT, made through the alias inside the window,
\ branches to the clause, and the lowest record carrying that entry is the
\ retired one: the site must travel under the alias's clause, which is the
\ public name that still reaches it. OLD was made by the original before it was
\ undefined and still runs the same clause. The case prints the loader's values
\ for both before it captures.
\
\ The suite runs this under an empty band, so the call-band audit admits the
\ package as the target's and the site reaches the scope audit.

require test/aot-band-lib.f

package AOT-BAND-EXPORT-UNDEF

: MK ( n -- ) create , does> ( -- n ) @ 1 + ;

public
7 MK OLD
EXPORT MK

private
undefine MK
;package

\ The does> definer leaves the DATA cursor off a cell boundary, and a window's
\ DATA base must sit on one.
align

AOT-ARM:WINDOW-OPEN
7 AOT-BAND-EXPORT-UNDEF:MK EIGHT
AOT-ARM:WINDOW-CLOSE

s" aot-band-export-undef: old " type AOT-BAND-EXPORT-UNDEF:OLD .
s" aot-band-export-undef: eight " type EIGHT .
AOT-BAND:GO
