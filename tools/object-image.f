\ object-image.f - native image writer for one cached, already-linked program.
\ This file validates and decodes the object; the target's writer, which
\ LOAD-TARGET below loads, turns the decoded text and DATA into an image:
\ tools/object-image-arm.f for AArch64 Linux and macOS, tools/object-image-x64.f
\ for x86-64 Linux.

require src/lib/errors.f
require src/lib/string.f
require src/lib/memory.f
require src/lib/fs.f
require src/lib/content-key.f
require src/lib/object.f

package OBJIMG
using OBJ

65 constant UP-A
71 constant UP-G
97 constant LOW-A
103 constant LOW-G

DYNAMIC-BUFFER TEXT-STORAGE n
variable TEXT-U
variable DATA-U
variable TEXT-ROW
variable DATA-ROW
variable EXPORT-ROW
variable DEF-ROW

: TEXT-BUF ( -- ptr u8 )
   0 TEXT-STORAGE byte-view ;

: CLEAR ( -- )
   0 TEXT-U !
   0 DATA-U !
   -1 TEXT-ROW !
   -1 DATA-ROW !
   -1 EXPORT-ROW !
   -1 DEF-ROW ! ;

: CLAIM ( n ptr n -- ) {: row:n slot:ptr :}
   slot @ 0 >= if E-OBJ-SCHEMA throw then
   row slot ! ;

: SCAN-ROW ( n -- ) {: row:n :}
   row ROW-TAG$ s" source" STR= if exit then
   row ROW-TAG$ s" target" STR= if exit then
   row ROW-TAG$ s" checker" STR= if exit then
   row ROW-TAG$ s" compiler" STR= if exit then
   row ROW-TAG$ s" text" STR= if row TEXT-ROW CLAIM exit then
   row ROW-TAG$ s" data" STR= if row DATA-ROW CLAIM exit then
   row ROW-TAG$ s" export" STR= if row EXPORT-ROW CLAIM exit then
   row ROW-TAG$ s" def" STR= if row DEF-ROW CLAIM exit then
   E-OBJ-SCHEMA throw ;

: SCAN ( -- )
   0 begin dup ROW-COUNT < while
      dup SCAN-ROW
      1+
   repeat drop
   TEXT-ROW @ 0 < if E-OBJ-SCHEMA throw then
   EXPORT-ROW @ 0 < if E-OBJ-SCHEMA throw then
   DEF-ROW @ 0 < if E-OBJ-SCHEMA throw then ;

: CHECK-DEF-OFF ( -- )
   DEF-ROW @ 1 ROW-FIELD$ STR>NUMBER? MATCH option
      none OF E-OBJ-SCHEMA throw ENDOF
      some OF 0 <> if E-OBJ-SCHEMA throw then ENDOF
   ;MATCH ;

: CHECK-ENTRY ( -- )
   EXPORT-ROW @ 0 ROW-FIELD$ {: name:ptr nameu:n :}
   EXPORT-ROW @ 1 ROW-FIELD$ s" --" STR= 0= if E-OBJ-SCHEMA throw then
   DEF-ROW @ 0 ROW-FIELD$ name nameu STR= 0= if E-OBJ-SCHEMA throw then
   DEF-ROW @ 2 ROW-FIELD$ s" --" STR= 0= if E-OBJ-SCHEMA throw then
   CHECK-DEF-OFF
   name nameu s" MAIN" STR= 0= if E-OBJ-SCHEMA throw then ;

: HEX-NIB ( n -- n ) {: c:n :}
   c STR-ZERO >= c STR-ZERO 10 + < and if c STR-ZERO - exit then
   c LOW-A >= c LOW-G < and if c 87 - exit then
   c UP-A >= c UP-G < and if c 55 - exit then
   E-OBJ-FIELD throw ;

: HEX-BYTE ( ptr u8 n -- n ) {: hex:ptr idx:n :}
   hex idx 2 * + c@ HEX-NIB 4 lshift
   hex idx 2 * 1+ + c@ HEX-NIB or ;

: DECODE-ROW ( n ptr u8 -- )
   {: row:n dest:ptr :}
   row 0 ROW-FIELD$ {: hex:ptr hexu:n :}
   hexu 2 / 0 ?do
      hex i HEX-BYTE dest i + c!
   loop ;

: DECODE ( -- )
   TEXT-ROW @ 0 ROW-FIELD$ nip 2 / {: bytes:n :}
   bytes 0 <= if E-OBJ-SCHEMA throw then
   DATA-ROW @ 0 >= if
      DATA-ROW @ 0 ROW-FIELD$ nip 2 / DATA-U !
   then
   bytes DATA-U @ + {: total:n :}
   total CELL / total CELL mod 0 > if 1+ then
   TEXT-STORAGE-RESERVE
   TEXT-ROW @ TEXT-BUF DECODE-ROW
   DATA-U @ 0 > if DATA-ROW @ TEXT-BUF bytes + DECODE-ROW then
   bytes TEXT-U ! ;

public

\ The object codec validates external record syntax; the cache also checks its
\ content key. This boundary accepts only the program shape hb-build emits.
: LOAD ( -- )
   CLEAR
   SCAN
   CHECK-ENTRY
   DECODE ;

;using
;package

package OBJIMG

: LOAD-TARGET ( -- )
   HB-TARGET-LINUX? HB-TARGET-MACOS? or if
      s" tools/object-image-arm.f" required exit
   then
   HB-TARGET-LINUX-X86-64? if
      s" tools/object-image-x64.f" required exit
   then
   E-OBJ-SCHEMA throw ;

' LOAD-TARGET
;package
execute
