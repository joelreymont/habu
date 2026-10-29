\ snap-heap-owner.f - name the owner of a persisted DP-heap cell. A snapshot holds
\ the DP heap verbatim, so a cell holding a live address breaks on restore. DUMP
\ prints `<heap offset> <name>` per heap owner: an offset's owner is the last line
\ not greater than it. CODE-MAP prints `<JIT region offset> <code length> <name>`
\ per word with code, headed by region base and heap top. DKIND:ADDR identifies a
\ created owner; `does>` clears it. Run before the dictionary is retired.

require src/lib/fmt.f
require src/habu/address-carrier.f

package SNAP-HEAP-OWNER

: TRUE ( -- bool )
   0 0= ;

: FALSE ( -- bool )
   TRUE 0= ;

: CHAIN-VALUE ( ptr n -- n ) {: rec:ptr :}
   rec XREF-START-SLOT XREF-PTR@ {: p:ptr :}
   p p rec XREF-CODE-BYTES + ADDRESS-CARRIER:CHAIN-SIZE {: size:n :}
   size 0= if s" heap-owner: malformed DKIND:ADDR carrier" 74 die then
   p size ADDRESS-CARRIER:CHAIN-VALUE ;

: HEAP-OFF ( n -- n ) {: addr:n :}
   addr data-base BYTE-VIEW NULL-PTR BYTE-VIEW - - ;

\ A package record holds wordlist roles in fields [0] and [8], not a code pointer
\ and length (habu2.f EM-SNAPSHOT-REBASE-DICT skips them too): reading its start
\ as an address would fault.
: CODE? ( ptr n -- bool ) {: rec:ptr :}
   rec XREF-WORDLIST XREF-NAMESPACE-WL <> ;

: CREATED? ( ptr n -- bool ) {: rec:ptr :}
   rec CODE? 0= if FALSE exit then
   rec XREF-RETIRED? if FALSE exit then
   rec XREF-FLAGS DKIND:MASK and DKIND:ADDR = ;

\ `.` ends its number with a newline, so row numbers print through FMT:.INT to
\ keep each row on the one line the maps promise.
: HEAP-ROW ( ptr n -- ) {: rec:ptr :}
   rec CHAIN-VALUE HEAP-OFF {: off:n :}
   off 0 < if exit then
   off FMT:.INT space
   rec XREF-NAME$ type cr ;

: HEAP-SLOT ( n -- ) {: idx:n :}
   idx XREF-REC {: rec:ptr :}
   rec CREATED? 0= if exit then
   rec HEAP-ROW ;

: CODE-ROW ( ptr n -- ) {: rec:ptr :}
   rec CODE? 0= if exit then
   rec XREF-START dbase@ - {: off:n :}
   off 0 < if exit then
   off FMT:.INT space
   rec XREF-LEN FMT:.INT space
   rec XREF-NAME$ type cr ;

public

: DUMP ( -- )
   s" heap-owner heap-map" type cr
   ndict@ 0 ?do i HEAP-SLOT loop ;

: CODE-MAP ( -- )
   s" heap-owner region-base " type dbase@ .
   s" heap-owner heap-top " type here data-base - .
   ndict@ 0 ?do i XREF-REC CODE-ROW loop ;

;package
