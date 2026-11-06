\ unaligned.f - marks an address cell one byte past a cell boundary, which its
\ object cannot place: the build refuses it E-COMP-STATE.
1 allot  here ptr-cell-mark  8 allot
