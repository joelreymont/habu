\ past.f - marks the address cell a cell past `here` and lays nothing: a file's
\ end is the aligned cell past its last byte, so the cell lies past it, outside
\ its object, and the build refuses it E-COMP-STATE.
here 1 cells + ptr-cell-mark
