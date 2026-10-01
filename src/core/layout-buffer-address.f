\ Bind fixed-storage address declarations after the library and registrar load.
require src/habu/address-cells.f

: STORAGE-ADDRESS-INSTALL ( -- )
   ['] ADDRESS-CELLS:REMOVE-XT-SPAN is STORAGE-CLEAR-XT ;
STORAGE-ADDRESS-INSTALL
undefine STORAGE-CLEAR-XT
undefine STORAGE-CLEAR-MISSING
undefine STORAGE-CLEAR-DEFAULT
undefine STORAGE-ADDRESS-INSTALL
