\ float-cell.f - the single explicit trusted boundary for storing floats in cells.
\ Habu cells are generic; @/! move an opaque cell and the checker silently coerces
\ it to/from r at use. F@/F! make that reinterpret explicit and asserted ONCE here
\ (TRUSTED:), so float-array code never takes an implicit a<->r boundary by default.
\ A float array is a `ptr r` (cell-addressed); F@ fetches the r at an address, F!
\ stores one. Load before any odin module that keeps floats in cell arrays.

TRUSTED: F@ ( ptr a -- r ) @ ;          \ fetch a float from a cell address
TRUSTED: F! ( r ptr a -- ) ! ;          \ store a float to a cell address
