\ created-body-abi.f - the record flag and engine cells that bound a created
\ word's body. Loaded before checker.f at startup, so the checker and
\ src/habu/layout.f, which loads after it, read one definition.
package CREATED-BODY-ABI
public

\ DNAME-ALLOC, in a dictionary record's flags cell, names the aligned DATA
\ allocation start of a created word. It survives does> because the allocation
\ stays even when the word changes its behavior. A scan excludes only windows
\ that straddle this boundary.
$0000800000000000 constant DNAME-ALLOC

\ The latest run of `,` cells that starts at a created body, [COMMA-RUN,
\ COMMA-END) as addresses, DATA offsets of engine cells. A `,` at COMMA-BASE
\ starts a run there, one at COMMA-END extends it, any other leaves it alone.
\ A path that lowers DP zeroes COMMA-RUN; a zero RUN never equals a body start.
$6A8 constant COMMA-RUN-CELL
$6B0 constant COMMA-END-CELL
\ The start of the latest created body, the address its word pushes.
$6B8 constant COMMA-BASE-CELL

;package
