\ xt-cell-band-bad.f - `xt!` rejects a cell outside DATA, before the store.
\ The address-cell table is keyed by a cell's DATA offset, and both its
\ declaration and the link, which gives each listed cell its image value, touch
\ the eight bytes at that offset. The fixture's cell is a fresh anonymous mmap
\ page, which the kernel places far from DATA (measured about 4.6e12 bytes below
\ data-base on macos-arm64).

\ The rejection is a process exit, not a throw: the declarer is engine machine
\ code beneath the catch machinery, and a half-declared address table must not
\ reach a link. STORED prints only if the store went through, so its absence
\ from the expected stdout proves the rejection precedes the mutation.

require src/lib/error.f

package XT-CELL-BAND

3 constant PROT-RW
$1002 constant MAP-ANON
$10000 constant PAGE-BYTES

\ A raw mmap result is a bare integer here; the cast makes it the quotation cell
\ xt! takes, and the point of the case is precisely that it is NOT a DATA cell.
CAST: N>CELL ( n -- ^[ -- n ] )

: PAGE ( -- n )
   0 PAGE-BYTES PROT-RW MAP-ANON -1 0 mmap ;

: TARGET ( -- n )
   4711 ;

: GO ( -- )
   PAGE N>CELL {: cell:^ :}
   s" XT-CELL-BAND-ARMED" type cr
   [: TARGET ;] cell xt!
   s" STORED" type cr ;

GO

;package
