\ The stripped x86-64 image keeps its sparse DATA and XT rows in RX text.
\ The final 16 bytes locate that table without decoding startup instructions.
package X64AOT-FORMAT

public

$3130544F41343658 constant MAGIC       \ "X64AOT01", little endian
16 constant FOOTER-BYTES
8 constant RUN-HEADER-BYTES
8 constant XT-ROW-BYTES

;package
