\ dynamic-storage.f - checked allocation behind DYNAMIC-BUFFER declarations.
\ A control record contains a mapping pointer and its byte capacity. Element
\ identity and width are fixed by the declaration that owns that record.
\
\ CORE-PREFIX FILE, and that is why the mapping refusals below are this
\ package's own codes rather than the memory codes lib/errors.f owns. The
\ declaration surface that generates calls into here is
\ src/core/layout-buffer.f, which the prefix loads before
\ src/core/lower-cert-seal.f takes the core-prefix mark; a generated build
\ source rewinds to that mark (src/habu/prefix-rewind.f) and then compiles
\ engine files that declare DYNAMIC-BUFFERs, so a runtime living above the mark
\ is undefined exactly where it is needed (measured: `E-UNDEFINED:
\ DYNAMIC-STORAGE:RESERVE` compiling src/habu/aot-decl.f in every hb-build
\ stage source). lib/errors.f loads after the mark, so reaching a code out of it
\ from here is what put this file above the mark.
package DYNAMIC-STORAGE
private

$7FFFFFFFFFFFFFFF constant MAX-BYTES
7121 constant E-SIZE
7138 constant E-MAP                      \ mmap refused the growth this reserve asked for
7139 constant E-UNMAP                    \ munmap refused a mapping this package owns

: EXTENT ( n n -- n ) {: count:n width:n :}
   count 0 < width 0 <= or if E-SIZE throw then
   count MAX-BYTES width / > if E-SIZE throw then
   count width * ;

: CAPACITY ( n n -- n ) {: need:n old:n :}
   old MAX-BYTES 2 / > if need exit then
   need old 2 * 64 max max ;

: COPY ( ptr n ptr n n -- ) {: src:ptr dst:ptr bytes:n :}
   bytes CELL / 0 ?do src i cells + @ dst i cells + ! loop ;

public

: RESERVE ( n ptr n n -- ) {: count:n cb:ptr width:n :}
   count width EXTENT {: need:n :}
   cb cell+ @ {: old:n :}
   need old <= if exit then
   need old CAPACITY {: cap:n :}
   cap map-anon 0< if drop E-MAP throw then {: fresh:ptr :}
   old 0 > if
      cb 0 ptr-field @ fresh old COPY
      cb 0 ptr-field @ old munmap 0< if
         fresh cap munmap drop E-UNMAP throw
      then
   then
   fresh cb 0 ptr-field !
   cap cb cell+ ! ;

: RELEASE ( ptr n -- ) {: cb:ptr :}
   cb cell+ @ {: cap:n :}
   cap 0 > if
      cb 0 ptr-field @ cap munmap 0< if E-UNMAP throw then
   then
   0 cb ! 0 cb cell+ ! ;

;package
