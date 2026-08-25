\ xt-cell-band-bad.f — `xt!` refuses a cell that does not lie inside DATA, and
\ refuses it BEFORE the store (dot habu-seal-the-declaration-7183177e).
\
\ The engine's persisted-address-cell table is keyed by a cell's offset within
\ DATA, and its three consumers -- the declaration, the snapshot writer's
\ canonicalise and the loader's relocate -- index DATA by that offset and touch
\ the eight bytes there. A cell outside DATA therefore names eight bytes of
\ something else entirely in all three.
\
\ Nothing in the tree should ever offer such a cell now that the declaration
\ coordinator's table cannot move out of the DP heap, so the fixture has to build
\ one deliberately: a fresh anonymous mmap page, which the kernel places nowhere
\ near DATA (measured about 4.6e12 bytes below data-base on macos-arm64). That is
\ exactly the storage the deleted growth path handed to `xt!`, so this is the real
\ shape of the old defect and not a synthetic stand-in.
\
\ The refusal is a process exit rather than a throw because the declarer is engine
\ machine code beneath the catch machinery, and because a half-declared address
\ table must not be allowed to reach a snapshot. STORED is printed only if the
\ store went through; its absence in the expected stdout is the proof that the
\ refusal precedes the mutation.

require lib/errors.f

package XT-CELL-BAND

3 constant PROT-RW
$1002 constant MAP-ANON
$10000 constant PAGE-BYTES

\ A raw mmap result is a bare integer here; nothing else in the file needs it to
\ be anything richer, and the point of the case is precisely that it is NOT a
\ DATA cell.
TRUSTED: N>CELL ( n -- ptr a ) ;

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
