\ Appended observer ABI loaded beside an older baked NATIVE-OBS-CELLS package.
\ The running kernel exposes this slot only through native-unit-cut-cell.
\ CAPTURE-INVALIDATE holds the capture owner ( n -- ): every motion that lowers
\ the dictionary count calls it with the new count before NDICT drops.
package NATIVE-OBS-EXT
public
$2D18 constant CAPTURE-INVALIDATE
;package
