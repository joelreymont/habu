\ snap-heap-owner.f - name the owner of a persisted DP-heap cell.
\
\ A snapshot image carries the whole DP heap verbatim, so any heap cell that
\ holds a live process address - an execution token in the JIT region, or a
\ pointer the host handed out through mmap - is wrong the moment the image is
\ restored somewhere else. Finding those cells is easy: build the image twice and
\ compare the two DATA payloads. But a byte offset does not say WHICH variable or
\ table is at fault, and guessing an owner from what a cell contains is exactly
\ the mistake this repair is not allowed to make.
\
\ Two maps close that gap, both read out of the live dictionary:
\
\   DUMP      `<heap offset> <name>` for every word that owns a piece of the DP
\             heap. The owner of a drifting offset is the last line whose offset
\             is not greater than it.
\   CODE-MAP  `<JIT region offset> <code length> <name>` for every word that has
\             code, plus the region base and heap top this run got. A program
\             counter caught by a debugger watchpoint on a drifting cell turns
\             into the name of the word that wrote it.
\
\ DKIND:ADDR identifies a created owner, as in tools/data-table-census.f.
\ An ordinary word that uses an address is not its owner; `does>` clears the
\ stamp when it replaces the created behavior. The shared SNAP-RELOC grammar
\ reads the owner's three- or four-half carrier, bounded by its code span.
\
\ Run it from a process that has the source under investigation loaded and has
\ not retired its dictionary yet. It reads the dictionary through
\ src/habu/xref.f, which the native engine provides, and the pure carrier module.

require src/habu/address-carrier.f

package SNAP-HEAP-OWNER

: TRUE ( -- bool )
   0 0= ;

: FALSE ( -- bool )
   TRUE 0= ;

: CHAIN-VALUE ( ptr n -- n ) {: rec:ptr :}
   rec XREF-START-SLOT XREF-PTR@ {: p:ptr :}
   p p rec XREF-CODE-BYTES + SNAP-RELOC:CHAIN-SIZE {: size:n :}
   size 0= if s" heap-owner: malformed DKIND:ADDR carrier" 74 die then
   p size SNAP-RELOC:CHAIN-VALUE ;

: HEAP-OFF ( n -- n ) {: addr:n :}
   addr XREF-N>REC data-base - ;

\ A package record carries raw wordlist roles in fields [0] and [8] instead of a
\ code pointer and a length - habu2.f EM-SNAPSHOT-REBASE-DICT skips them the same
\ way - so reading its start as an address would fault. Ask the record what it is.
: CODE? ( ptr n -- bool ) {: rec:ptr :}
   rec XREF-WORDLIST XREF-NAMESPACE-WL <> ;

: CREATED? ( ptr n -- bool ) {: rec:ptr :}
   rec CODE? 0= if FALSE exit then
   rec XREF-RETIRED? if FALSE exit then
   rec XREF-FLAGS DKIND:MASK and DKIND:ADDR = ;

: HEAP-ROW ( ptr n -- ) {: rec:ptr :}
   rec CHAIN-VALUE HEAP-OFF {: off:n :}
   off 0 < if exit then
   off .
   rec XREF-NAME$ type cr ;

: HEAP-SLOT ( n -- ) {: idx:n :}
   idx XREF-REC {: rec:ptr :}
   rec CREATED? 0= if exit then
   rec HEAP-ROW ;

: CODE-ROW ( ptr n -- ) {: rec:ptr :}
   rec CODE? 0= if exit then
   rec XREF-START dbase@ - {: off:n :}
   off 0 < if exit then
   off .
   rec XREF-LEN .
   rec XREF-NAME$ type cr ;

public

\ every heap owner in dictionary order: `<heap offset> <name>` per line
: DUMP ( -- )
   s" heap-owner heap-map" type cr
   ndict@ 0 ?do i HEAP-SLOT loop ;

\ the whole code map, headed by the region base and heap top this run got
: CODE-MAP ( -- )
   s" heap-owner region-base " type dbase@ .
   s" heap-owner heap-top " type here data-base - . cr
   ndict@ 0 ?do i XREF-REC CODE-ROW loop ;

;package
