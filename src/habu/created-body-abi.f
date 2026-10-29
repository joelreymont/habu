\ created-body-abi.f - the record flag and engine cells that bound a created
\ word's body. Loaded before checker.f by each core prefix, so the checker and
\ src/habu/layout.f, which loads after it, read one definition. An engine bakes
\ this file (src/habu/native-runtime.f provides it). Constants only: no DATA.
package CREATED-BODY-ABI
public

\ DNAME-ALLOC, in a dictionary record's flags cell, names the aligned DATA
\ allocation start of a created word. It survives does> because the allocation
\ stays even when the word changes its behavior. A scan excludes only windows
\ that straddle this boundary.
$0000800000000000 constant DNAME-ALLOC

\ The latest run of `,` cells that starts at a created body, [COMMA-RUN,
\ COMMA-END) as addresses, DATA offsets of engine cells. A `,` at COMMA-BASE
\ starts a run there, one at COMMA-END extends it, any other leaves it alone
\ (habu1.f BCOMMA).
\ COMMA-RUN-CELL: written by habu1.f BCOMMA; zeroed by SND-ZERO-LIVE and by
\ every path that lowers DP, beside the DPHW-CELL raise: habu1.f DP-LOWER, (and
\ so BALLOT, habu2.f EM-EVAL-THROW-RECOVER and EM-REPL-RECOVER), interpret.f
\ FRAME-ROLLBACK-CODE and src/core/generated-declaration-dictionary.f
\ DICTIONARY-DP!. A zero RUN never equals a body start.
$6A8 constant COMMA-RUN-CELL
\ COMMA-END-CELL: written by habu1.f BCOMMA, zeroed by SND-ZERO-LIVE.
$6B0 constant COMMA-END-CELL
\ COMMA-BASE-CELL: the start of the latest created body, the address its word
\ pushes, written by habu2.f ADDR-TAIL, zeroed by SND-ZERO-LIVE.
$6B8 constant COMMA-BASE-CELL

;package
