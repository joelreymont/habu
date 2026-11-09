\ lower-cert-seal.f — erase lowering-certificate producer authority. Loaded
\ after xref installs `undefine`. Undefining a dispatch cell's name stops a
\ later `is` at it; the cell and its code stay, so compiled callers keep
\ dispatching and the address-cell table keeps relocating the cell.

undefine CHECKER-CERT:INSTALL
undefine CHECKER-CERT:PRODUCE

package CHECKER-CERT
undefine PRODUCER-XT
undefine PRODUCER-SET
;package

package LOWER-CERT
undefine FULL-INSTALL
undefine FULL-PRODUCE-INSTALL
undefine FULL-PRODUCE
undefine DISPATCH-INSTALL
undefine DISPATCH
undefine FULL-XT
undefine FULL-SET
;package
