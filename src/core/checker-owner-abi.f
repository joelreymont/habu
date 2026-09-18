\ checker-owner-abi.f - append-only callable fields in one declaration owner.
\ Loaded before checker.f by each core prefix; also require-able by a compiler
\ being rebuilt against an older retained prefix. No engine DATA slots live here.

package CHECKER-OWNER-ABI
public

$0 constant RAW-OFF
$8 constant EFFECT-OFF
$10 constant DEFER-OFF
$18 constant CAST-OFF
$20 constant USING-OFF
$28 constant PACKAGE-OFF
$30 constant PUBLIC-OFF
$38 constant PRIVATE-OFF
$40 constant END-PACKAGE-OFF
$48 constant TRANSFER-OFF
$50 constant SOURCE-ROW-OFF
$58 constant SOURCE-CON-OFF
$60 constant EXPORT-OFF
$68 constant WIDE-OFF
$70 constant RESET-OFF
$78 constant CAPTURE-OFF
$80 constant CHECK-OFF
$88 constant TAPE-INSTALL-OFF
$90 constant TAPE-ARM-OFF
$98 constant TAPE-DISARM-OFF
$A0 constant TAPE-ADVANCE-OFF
$A8 constant DOES-CHECK-OFF
$B0 constant DOES-IN-OFF
$B8 constant DOES-OUT-OFF
$C0 constant DOES-WIDE-OFF
$C8 constant USIG-TRUNCATE-OFF
$D0 constant CALL-CELLS-OFF
$D8 constant CALL-GLUE-OFF
$E0 constant CALL-MATCH-OFF
$E8 constant CALL-QUOT-IN-OFF
$F0 constant CALL-QUOT-OUT-OFF
$F8 constant TRUST-DECL-OFF
$100 constant PARSE-IMM-OFF
$108 constant EFFECT-QUERY-OFF
$110 constant EFFECT-DIN-N-OFF
$118 constant EFFECT-DOUT-N-OFF
$120 constant EFFECT-DIN-CELLS-OFF
$128 constant EFFECT-DOUT-CELLS-OFF
$130 constant EFFECT-DIN-SLOT-OFF
$138 constant EFFECT-DOUT-SLOT-OFF
$140 constant EFFECT-DIN-QUOT-OFF
$148 constant EFFECT-DOUT-QUOT-OFF
$150 constant EFFECT-QUOT-UP-OFF
$158 constant EFFECT-RET-NEUTRAL-OFF
$160 constant EFFECT-QUOT-SIMPLE-OFF
$168 constant EFFECT-CATCH-CELLS-OFF
$170 constant EFFECT-EXEC-CELLS-OFF
$178 constant EFFECT-FINALLY-CELLS-OFF
$180 constant EFFECT-MATCH-CELLS-OFF
$188 constant CTL-DEAD-OFF
$190 constant WF-W-AT-OFF
$198 constant REC-MIN-IN-OFF
$1A0 constant REC-WIDE-PUBLISH-OFF
$1A8 constant CHECK-UNJUDGED-OFF
$1B0 constant FAMILY-MATCH-OFF
$1B8 constant FAMILY-CON-OFF
$1C0 constant FAMILY-VARIANT-OFF
$1C8 constant FAMILY-SLOTS-OFF
$1D0 constant FAMILY-VARIANTS-OFF
$1D8 constant FAMILY-NAME-OFF
$1E0 constant VARIANT-TAG-OFF
$1E8 constant VARIANT-PADS-OFF
$1F0 constant VARIANT-PAY-CELLS-OFF
$1F8 constant VARIANT-PAY-TERMS-OFF

$200 constant PAYLOAD-ARM-OFF
$208 constant PAYLOAD-FREEZE-OFF
$210 constant PAYLOAD-LOOKUP-OFF
$218 constant PAYLOAD-SPANS-OFF
$220 constant PAYLOAD-REG-SAVE-OFF
$228 constant PAYLOAD-DISARM-OFF

\ The does> row's own value boundaries, the shape DIN-SLOT / DOUT-SLOT already
\ give a definition's rows: the clause's term counts and the bundle slot of each
\ term, so the native chain can place a does> row from per-cell facts instead of
\ refusing every clause whose value is wider than a cell. $230 is the frozen
\ certificate (checker-fetch-abi.f), which these rows are appended after.
$238 constant DOES-IN-N-OFF
$240 constant DOES-OUT-N-OFF
$248 constant DOES-IN-SLOT-OFF
$250 constant DOES-OUT-SLOT-OFF
CHECKER-FETCH-ABI:BYTES constant BYTES

\ These cells precede the record; callable offsets and the record pointer stay
\ unchanged. Older records have no descriptor and cannot supply appended fields.
$4842434B4F574E01 constant MAGIC
16 constant HEADER-BYTES

;package
