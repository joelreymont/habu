\ The interpreter namespace a cached definition was compiled under: the current
\ package, parent, compilation wordlist and use list, encoded so a later load
\ can tell whether the definition still resolves the same way.
require src/habu/packages.f
require src/lib/le.f
require src/lib/memory.f
require src/lib/string.f
require tools/native-file-reference.f

package OUTER
private

DYNAMIC-BUFFER INPUT-BYTES u8
variable INPUT-U

: FILE-SCOPE-ROOM ( n -- n ) {: size:n :}
   size 0 < size MEM-MAX-N INPUT-U @ - > or if RC-REJECT throw then
   INPUT-U @ size + INPUT-BYTES-RESERVE
   INPUT-U @ dup size + INPUT-U ! ;

: FILE-SCOPE-N ( n -- )
   8 FILE-SCOPE-ROOM INPUT-BYTES LE:U64! ;

: FILE-SCOPE-S ( ptr u8 n -- ) {: bytes:ptr size:n :}
   size FILE-SCOPE-N
   size 0 > if
      size FILE-SCOPE-ROOM INPUT-BYTES bytes swap size BYTE-COPY
   then ;

: FILE-SCOPE-REC ( n -- ) {: ord:n :}
   ord -1 = if NULL-PTR 0 FILE-SCOPE-S exit then
   ord NFILE-REF:DECL-KEY$ FILE-SCOPE-S ;

: FILE-SCOPE-WID ( n -- ) NFILE-REF:WID-KEY$ FILE-SCOPE-S ;

: LIVE-INPUT ( -- )
   0 INPUT-U !
   data-base PKG-REC-CELL + 0 ptr-field @ {: rec:ptr :}
   rec 0= if -1 else rec PKG-INDEX then FILE-SCOPE-REC
   PKG-PARENT-CELL CELL@ FILE-SCOPE-WID
   get-current FILE-SCOPE-WID
   USE-PKG-SAVE-CELL CELL@ FILE-SCOPE-N
   USE-DEPTH-CELL CELL@ {: depth:n :}
   depth FILE-SCOPE-N
   depth 0 ?do i cells USE-WIDS-OFF + CELL@ FILE-SCOPE-WID loop ;

public

: SCOPE-INPUT$ ( -- ptr u8 n )
   LIVE-INPUT
   0 INPUT-BYTES INPUT-U @ ;

: SCOPE-MATCH? ( ptr u8 n -- bool )
   {: bytes:ptr size:n :}
   SCOPE-INPUT$ bytes size STR= ;

: SCOPE-CLOSE ( -- )
   INPUT-BYTES-RELEASE 0 INPUT-U ! ;

;package
