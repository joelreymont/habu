\ xt-cell-band-bad.f - `xt!` refuses a cell outside DATA, before the store.
\ The persisted-address-cell table is keyed by a cell's DATA offset, and its
\ three consumers (the declaration, the snapshot writer's canonicalise, the
\ loader's relocate) touch the eight bytes at that offset. The fixture's cell is
\ a fresh anonymous mmap page, which the kernel places far from DATA (measured
\ about 4.6e12 bytes below data-base on macos-arm64).

\ The refusal is a process exit, not a throw: the declarer is engine machine
\ code beneath the catch machinery, and a half-declared address table must not
\ reach a snapshot. STORED prints only if the store went through, so its absence
\ from the expected stdout proves the refusal precedes the mutation.

require src/lib/errors.f

package XT-CELL-BAND

3 constant PROT-RW
$1002 constant MAP-ANON
$10000 constant PAGE-BYTES

\ A raw mmap result is a bare integer here; the cast makes it the quotation cell
\ xt! takes, and the point of the case is precisely that it is NOT a DATA cell.
CAST: N>CELL ( n -- ptr [ -- n ] )

: PAGE ( -- n )
   0 PAGE-BYTES PROT-RW MAP-ANON -1 0 mmap ;

: TARGET ( -- n )
   4711 ;

: GO ( -- )
   PAGE N>CELL {: cell:ptr :}
   s" XT-CELL-BAND-ARMED" type cr
   [: TARGET ;] cell xt!
   s" STORED" type cr ;

GO

;package
