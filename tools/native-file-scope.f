\ The interpreter namespace a cached definition was compiled under: the current
\ package, parent, compilation wordlist and use list, encoded so a later load
\ can tell whether the definition still resolves the same way. The product
\ ships the engine's OUTER package sealed, so this build tool keeps a package
\ of its own and reads the engine cells directly.
require src/lib/errors.f
require src/habu/layout.f
require src/lib/le.f
require src/lib/memory.f
require src/lib/string.f
require tools/native-file-reference.f

package NFILE-SCOPE
private

DYNAMIC-BUFFER INPUT-BYTES u8
variable INPUT-U
\ The first package record the live scope names whose declaration has no
\ key, -1 when it has none. Such a scope cannot be encoded, so it never
\ matches and its frame refuses.
variable UNKEYED-ORD
-1 UNKEYED-ORD !

: FILE-SCOPE-ROOM ( n -- n ) {: size:n :}
   size 0 < size MEM-MAX-N INPUT-U @ - > or if E-NUNIT-PROFILE throw then
   INPUT-U @ size + INPUT-BYTES-RESERVE
   INPUT-U @ dup size + INPUT-U ! ;

: FILE-SCOPE-N ( n -- )
   8 FILE-SCOPE-ROOM INPUT-BYTES LE:U64! ;

: FILE-SCOPE-S ( ptr u8 n -- ) {: bytes:ptr size:n :}
   size FILE-SCOPE-N
   size 0 > if
      size FILE-SCOPE-ROOM INPUT-BYTES bytes swap size BYTE-COPY
   then ;

\ Record ord as the scope's first unkeyed package, standing in no bytes.
: FILE-SCOPE-UNKEYED ( n -- ) {: ord:n :}
   UNKEYED-ORD @ 0 < if ord UNKEYED-ORD ! then
   NULL-PTR 0 FILE-SCOPE-S ;

: FILE-SCOPE-REC ( n -- ) {: ord:n :}
   ord -1 = if NULL-PTR 0 FILE-SCOPE-S exit then
   ord NFILE-REF:DECL-KEY? if FILE-SCOPE-S exit then
   2drop ord FILE-SCOPE-UNKEYED ;

: FILE-SCOPE-WID ( n -- ) {: wid:n :}
   wid NFILE-REF:WID-KEYED? if drop wid NFILE-REF:WID-KEY$ FILE-SCOPE-S exit then
   FILE-SCOPE-UNKEYED ;

: LIVE-INPUT ( -- )
   0 INPUT-U ! -1 UNKEYED-ORD !
   data-base PKG-REC-CELL + 0 ptr-field @ {: rec:ptr :}
   rec NFILE-REF:REC-ORD FILE-SCOPE-REC
   data-base PKG-PARENT-CELL + @ FILE-SCOPE-WID
   get-current FILE-SCOPE-WID
   data-base USE-PKG-SAVE-CELL + @ FILE-SCOPE-N
   data-base USE-DEPTH-CELL + @ {: depth:n :}
   depth FILE-SCOPE-N
   depth 0 ?do data-base i cells USE-WIDS-OFF + + @ FILE-SCOPE-WID loop ;

public

: SCOPE-INPUT$ ( -- ptr u8 n )
   LIVE-INPUT
   0 INPUT-BYTES INPUT-U @ ;

\ The first package record the scope SCOPE-INPUT$ last encoded names
\ without a key, -1 when every name has one.
: SCOPE-UNKEYED ( -- n ) UNKEYED-ORD @ ;

: SCOPE-MATCH? ( ptr u8 n -- bool )
   {: bytes:ptr size:n :}
   SCOPE-INPUT$ bytes size STR= UNKEYED-ORD @ 0 < and ;

: SCOPE-CLOSE ( -- )
   INPUT-BYTES-RELEASE 0 INPUT-U ! -1 UNKEYED-ORD ! ;

;package
