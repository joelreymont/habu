\ Appended observer ABI loaded beside an older baked NATIVE-OBS-CELLS package.
\ The running kernel exposes this slot only through native-unit-reclaim-cell.
package NATIVE-OBS-EXT
public
$2D18 constant CAPTURE-INVALIDATE
\ Four cells before the FFI buffers. The capability primitive returns their
\ byte address; source loaded on an older kernel uses native-body-abi.f indices.
$3820 constant BODY-REUSE
$3828 constant ORIGIN-HOOK
$3830 constant ORIGIN-TAKE
$3838 constant PENDING-TAKE
;package
