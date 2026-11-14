\ link-map.f - the target's names for a build's records: each record's wordlist
\ id as the target numbers it, and <program>.map, which LINKER:WRITE writes
\ beside the program in the format tools/image-names.f reads (habu-names 1). Its
\ rows, rec counting them, first list the code the program holds in offset
\ order: the start-up, each kept kernel primitive and placed build record, a row
\ for each byte range none of them spans, and the code after the routines to
\ its end. A row spanning the code of the row before it is an alias. The code
\ rows other than aliases tile [0, code end); a record the program holds no
\ code for has no row. Then, tiling on from the code end to the code buffer's,
\ a row for each region the link appended after the code, in offset order, and
\ a pad row for the bytes before one that no row spans. Then a row for each
\ package the build declared.

require src/lib/error.f
require src/lib/le.f
require src/lib/sort.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/byte-buffer.f
require src/habu/layout.f
require src/habu/boot-cells.f
require src/habu/wl.f
require src/habu/xref.f
require src/habu/code-span.f
require src/arch/arm64/icode.f
require src/arch/arm64/link-kernel.f
require src/habu/linker.f
require src/habu/link-band.f

package LINKER
private

\ A wordlist id holds only in the process that made it. An object names none:
\ the shift probe moves no object cell by a wordlist id.

\ The first id the build minted after its global and namespace.
: FIRST-WID ( -- n ) BOOT:GLOBAL-WL  BOOT:NAMESPACE-WL negate  max 1 + ;

: BAD-WID ( n n -- )
   {: wid:n r:n :}
   s" link: record " SAY r XREF-REC XREF-NAME$ SAY s"  holds wordlist " SAY wid SAY-INT
   s" , which the build did not make" SAY SAY-CR
   s" link: a record holds a wordlist the target cannot name" 74 die ;

\ Build record r's wordlist id wid as the target numbers it.
: WID-MAP ( n n -- n )
   {: wid:n r:n :}
   wid BOOT:GLOBAL-WL = if 0 exit then
   wid BOOT:NAMESPACE-WL = if WL:NAMESPACE exit then
   wid 0 >=  wid FIRST-DYNAMIC-WID < and if wid exit then
   wid XREF:RETIRED-WL? if wid exit then
   wid FIRST-WID >=  wid WIDN-CELL HOST@ < and if FIRST-DYNAMIC-WID wid + FIRST-WID - exit then
   wid r BAD-WID ;


create MAP-TEXT BUF:HDR-BYTES allot
create MAP-PATH BUF:HDR-BYTES allot
DYNAMIC-BUFFER MAP-START n           \ row k's code offset in A64ICODE's code buffer, -1 for none
DYNAMIC-BUFFER MAP-BYTES n           \ its length, -1 until MAP-ENDS for an end CUT unbound
DYNAMIC-BUFFER MAP-FILE n            \ build row k's BUILT row, -1 for one the build's body made
DYNAMIC-BUFFER MAP-ORDER n           \ the rows with code, by start, then by row
variable MAP-ORDER-N
DYNAMIC-BUFFER MAP-ALIAS n           \ 1 where a row before it spans the same code
variable MAP-WALK                    \ the BUILT row the record walk has reached
variable MAP-REC                     \ the next map row's rec
variable MAP-AT                      \ the first byte no row so far spans

\ The regions the link appended after the code, in offset order: region i's
\ kind and bytes [start, end).
DYNAMIC-BUFFER REGION-KIND n
DYNAMIC-BUFFER REGION-START n
DYNAMIC-BUFFER REGION-END n
variable REGION-N

0 constant DATA-KIND                 \ the DATA image
1 constant DATA-LIST-KIND            \ the DATA list
2 constant NAMES-KIND                \ an engine's out-of-line names
3 constant RECORDS-KIND              \ an engine's records
4 constant RECORD-LIST-KIND          \ an engine's record list

\ Kernel primitive k is row k, build record r row P + (r - FIRST-REC).
: MAP-N ( -- n ) ARM64:ROW-N  ndict@ +  FIRST-REC @ - ;


\ Kernel row k's code, -1 0 when CUT removed it. CUT unbinds its last label when
\ it removes the code after the body; MAP-ENDS then ends it.
: KERNEL-SPAN ( n -- n n )
   {: k:n :}
   k ARM64:ROW-FIRST {: first:label :}
   first A64ICODE:LBL-BOUND? 0= if -1 0 exit then
   first ARM64:OFFSET {: at:n :}
   k ARM64:ROW-LAST {: last:label :}
   last A64ICODE:LBL-BOUND? 0= if at -1 exit then
   at  last ARM64:OFFSET at - ;


\ Build record r's code: a placed shadow record's [entry, RANGE-END), else the
\ kernel row it enters, else none. Not for a package row, whose cells hold wids.
: RECORD-SPAN ( n -- n n )
   {: r:n :}
   r XREF-REC XREF-START {: a:n :}
   a 0= if -1 0 exit then
   a REC-AT {: s:n :}
   s 0 >= if
      s NSHADOW:EMISSION@ OFFSET@ {: off:n :}
      off 0 < if -1 0 exit then
      off s NSHADOW:ENTRY@ +  s RANGE-END s NSHADOW:ENTRY@ - exit
   then
   a ARM64:KERNEL-ROW {: k:n :}
   k 0 < if -1 0 exit then
   k MAP-START @  k MAP-BYTES @ ;


\ Build record r's BUILT row, -1 for a record the build's body made. Records
\ ascend through BUILT's rows, so the walk keeps its row in MAP-WALK.
: REC-FILE ( n -- n )
   {: r:n :}
   begin
      MAP-WALK @ BUILT-N < if r  MAP-WALK @ BUILT-AT BUILT-FIRST 8 + + LE:U64@  >= else false then
   while 1 MAP-WALK +! repeat
   MAP-WALK @ BUILT-N < if
      r MAP-WALK @ HOLDS? if MAP-WALK @ exit then
   then
   -1 ;


: MAP-FILL ( -- )
   MAP-N MAP-START-RESERVE  MAP-N MAP-BYTES-RESERVE  MAP-N MAP-FILE-RESERVE
   ARM64:ROW-N 0 ?do i KERNEL-SPAN i MAP-BYTES ! i MAP-START !  -1 i MAP-FILE ! loop
   0 MAP-WALK !
   ndict@ FIRST-REC @ ?do
      i FIRST-REC @ - ARM64:ROW-N + {: k:n :}
      i XREF-REC XREF-WORDLIST XREF:PACKAGE-WL? if -1 0 else i RECORD-SPAN then
      k MAP-BYTES !  k MAP-START !
      i REC-FILE k MAP-FILE !
   loop ;


: MAP-BEFORE? ( n n -- bool )
   {: a:n b:n :}
   a MAP-START @  b MAP-START @ {: sa:n sb:n :}
   sa sb <> if sa sb < exit then
   a b < ;

: MAP-SORT ( -- )
   MAP-N MAP-ORDER-RESERVE  0 MAP-ORDER-N !
   MAP-N 0 ?do
      i MAP-START @ 0 >=  i MAP-BYTES @ 0<> and if
         i MAP-ORDER-N @ MAP-ORDER !  1 MAP-ORDER-N +!
      then
   loop
   MAP-ORDER-N @ 1 > if 0 MAP-ORDER MAP-ORDER-N @ [: MAP-BEFORE? ;] SORT:SORT! then ;


\ A kernel row whose end CUT unbound, and a record entering it, end at the next
\ greater start or the kernel's end: an upper bound, which may take in kernel
\ code no row names.
: MAP-ENDS ( -- )
   MAP-ORDER-N @ 0 ?do
      i MAP-ORDER @ {: k:n :}
      k MAP-BYTES @ 0 < if
         k MAP-START @ {: at:n :}
         BAND-AT @  MAP-ORDER-N @ i 1 + ?do
            i MAP-ORDER @ MAP-START @ dup at > if min leave else drop then
         loop
         at - k MAP-BYTES !
      then
   loop ;


\ Rows of one start stand together in MAP-ORDER.
: MAP-ALIASES ( -- )
   MAP-N MAP-ALIAS-RESERVE
   MAP-N 0 ?do 0 i MAP-ALIAS ! loop
   MAP-ORDER-N @ 0 ?do
      i MAP-ORDER @ {: k:n :}
      i begin dup 0 > if 1 - dup MAP-ORDER @ MAP-START @ k MAP-START @ = else false then while
         dup MAP-ORDER @ MAP-BYTES @ k MAP-BYTES @ = if 1 k MAP-ALIAS ! then
      repeat drop
   loop ;


: MAP+ ( ptr u8 n -- ) MAP-TEXT SPAN+ ;
: MAP-NL ( -- ) S\" \n" MAP+ ;

: MAP-INT+ ( n -- )
   dup 0 < if s" -" MAP+ then
   ERROR-TEXT:DIGITS$ MAP+ ;

: MAP-HEX+ ( n -- ) s" 0123456789ABCDEF" drop swap + 1 MAP+ ;

\ A path, its bytes at or below a space and its '%' as %XX, and none as `-`.
: MAP-ESC+ ( ptr u8 n -- )
   {: a:ptr u:n :}
   u 0= if s" -" MAP+ exit then
   u 0 ?do
      a i + c@ {: c:n :}
      c 32 <=  c 37 = or if
         s" %" MAP+  c 4 rshift MAP-HEX+  c 15 and MAP-HEX+
      else a i + 1 MAP+ then
   loop ;

: MAP-NAME+ ( ptr u8 n -- )
   {: a:ptr u:n :}
   u 0= if s" -" MAP+ else a u MAP+ then ;

: REC+ ( -- n ) MAP-REC @  1 MAP-REC +! ;

: MAP-HEAD ( n n n n n -- )
   {: rec:n named:n start:n len:n wid:n :}
   rec MAP-INT+ s"  " MAP+  named MAP-INT+ s"  " MAP+  start MAP-INT+ s"  " MAP+
   len MAP-INT+ s"  " MAP+  wid MAP-INT+ s"  " MAP+ ;

: MAP-LEN ( n -- n ) MAP-BYTES @ CODE-SPAN:EXACT ;

\ The map's one global wid is 0.
: MAP-WID ( n -- n )
   {: wid:n :}
   wid 0=  wid WL:PRIMITIVES = or if 0 else wid then ;

: NAMED ( -- n ) ENGINE? @ if 1 else 0 then ;

: MAP-KIND$ ( n -- ptr u8 n ) MAP-ALIAS @ 0<> if s"  alias " else s"  word " then ;

\ An engine's records are baked into the program it writes; a stripped program's
\ are loaded on the kernel's.
: ORIGIN$ ( -- ptr u8 n ) ENGINE? @ if s" baked " else s" loaded " then ;

\ BUILT row's path below the root that resolved its file; none for -1.
: FILE-PATH$ ( n -- ptr u8 n )
   {: row:n :}
   row 0 < if s" " exit then
   LOAD-PATHS BYTES$ drop  row BUILT-PATHS @ + {: at:ptr :}
   at 8 +  at LE:U64@ ;


: KERNEL-ROW+ ( n -- )
   {: k:n :}
   REC+ NAMED  k MAP-START @  k MAP-LEN  k ARM64:ROW-WID MAP-WID  MAP-HEAD
   k ARM64:ROW-NAME$ MAP-NAME+  k MAP-KIND$ MAP+  s" baked -" MAP+  MAP-NL ;

\ Row k, a build record with code.
: RECORD-ROW+ ( n -- )
   {: k:n :}
   k ARM64:ROW-N - FIRST-REC @ + {: r:n :}
   r XREF-REC {: rec:ptr :}
   REC+ NAMED  k MAP-START @  k MAP-LEN  rec XREF-WORDLIST r WID-MAP MAP-WID  MAP-HEAD
   rec XREF-NAME$ MAP-NAME+  k MAP-KIND$ MAP+  ORIGIN$ MAP+
   k MAP-FILE @ FILE-PATH$ MAP-ESC+  MAP-NL ;

\ Row k, a package: its public and private wid in the start and len columns.
: PACKAGE-ROW+ ( n -- )
   {: k:n :}
   k ARM64:ROW-N - FIRST-REC @ + {: r:n :}
   r XREF-REC {: rec:ptr :}
   REC+ NAMED  rec XREF-PKG-PUBLIC r WID-MAP  rec XREF-PKG-PRIVATE r WID-MAP  -1 MAP-HEAD
   rec XREF-NAME$ MAP-NAME+  s"  namespace " MAP+  ORIGIN$ MAP+
   k MAP-FILE @ FILE-PATH$ MAP-ESC+  MAP-NL ;

: CODE-ROW+ ( n -- ) dup ARM64:ROW-N < if KERNEL-ROW+ else RECORD-ROW+ then ;


\ The code bytes [at, end) the link wrote for no record, if any.
: RUNTIME-ROW+ ( n n -- )
   {: at:n end:n :}
   end at > if
      REC+ 0 at  end at - CODE-SPAN:EXACT  -1 MAP-HEAD
      s" runtime runtime generated -" MAP+  MAP-NL
   then ;

: GAP-ROW+ ( n -- )
   {: end:n :}
   MAP-AT @ {: at:n :}
   REC+ 0 at  end at - CODE-SPAN:EXACT  -1 MAP-HEAD
   at BAND-AT @ <  ENGINE? @ or if s" - word baked -" else s" - word loaded -" then MAP+  MAP-NL
   end MAP-AT ! ;

\ The code bytes no row spans before end, as rows split at the kernel's end.
: GAP ( n -- )
   {: end:n :}
   MAP-AT @ BAND-AT @ <  end BAND-AT @ > and if BAND-AT @ GAP-ROW+ then
   end MAP-AT @ > if end GAP-ROW+ then ;

: CODE-ROWS ( n -- )
   {: end:n :}
   0 ARM64:KERNEL-AT RUNTIME-ROW+
   ARM64:KERNEL-AT MAP-AT !
   MAP-ORDER-N @ 0 ?do
      i MAP-ORDER @ {: k:n :}
      k MAP-START @ GAP
      k CODE-ROW+
      k MAP-START @ k MAP-BYTES @ +  MAP-AT @ max MAP-AT !
   loop
   BAND-END @ GAP
   BAND-END @ end RUNTIME-ROW+ ;

: KIND$ ( n -- ptr u8 n )
   {: k:n :}
   k DATA-KIND = if s" data" exit then
   k DATA-LIST-KIND = if s" data-list" exit then
   k NAMES-KIND = if s" names" exit then
   k RECORDS-KIND = if s" records" exit then
   k RECORD-LIST-KIND = if s" record-list" exit then
   s" link: a map region has no kind past 4" 74 die ;

\ The link appended bytes [start, end) of kind k after the code.
: REGION+ ( n n n -- )
   {: k:n start:n end:n :}
   REGION-N @ {: i:n :}
   i 1 + REGION-KIND-RESERVE  i 1 + REGION-START-RESERVE
   i 1 + REGION-END-RESERVE
   k i REGION-KIND !  start i REGION-START !  end i REGION-END !
   i 1 + REGION-N ! ;

\ The bytes [at, end) the link appended after the code, if any, of the kind.
: TAIL-ROW+ ( ptr u8 n n n -- )
   {: kind:ptr kindu:n at:n end:n :}
   end at > if
      REC+ 0 at  end at - CODE-SPAN:EXACT  -1 MAP-HEAD
      s" - " MAP+  kind kindu MAP+  s"  generated -" MAP+  MAP-NL
   then ;

: REGION-ROW+ ( n -- )
   {: i:n :}
   i REGION-START @ {: start:n :}
   start MAP-AT @ < if s" link: the map's regions overlap" 74 die then
   s" pad" MAP-AT @ start TAIL-ROW+
   i REGION-KIND @ KIND$  start  i REGION-END @  TAIL-ROW+
   i REGION-END @ MAP-AT ! ;

: REGION-ROWS ( n -- )
   MAP-AT !
   REGION-N @ 0 ?do i REGION-ROW+ loop
   MAP-AT @ A64ICODE:ASM-LEN <> if
      s" link: the map's regions do not end at the code buffer's end" 74 die
   then ;

: PACKAGE-ROWS ( -- )
   ndict@ FIRST-REC @ ?do
      i XREF-REC XREF-WORDLIST XREF:PACKAGE-WL? if
         i FIRST-REC @ - ARM64:ROW-N + PACKAGE-ROW+
      then
   loop ;


: MAP-WRITE ( ptr u8 n n -- )
   {: path:ptr pathu:n end:n :}
   MAP-TEXT 4096 BUF:N>BLEN BUF:INIT  MAP-PATH 256 BUF:N>BLEN BUF:INIT
   MAP-FILL MAP-SORT MAP-ENDS MAP-ALIASES
   S\" habu-names 1\ncolumns rec named start len wid name kind origin source\n" MAP+
   0 MAP-REC !  end CODE-ROWS  end REGION-ROWS  PACKAGE-ROWS
   path pathu MAP-PATH SPAN+  s" .map" MAP-PATH SPAN+
   MAP-PATH BYTES$ [: MAP-TEXT BYTES$ FS:WRITE-ALL ;] FS:REPLACE-CACHED ;

: MAP-RELEASE ( -- )
   MAP-TEXT BUF:DISPOSE  MAP-PATH BUF:DISPOSE
   MAP-START-RELEASE  MAP-BYTES-RELEASE  MAP-FILE-RELEASE  MAP-ORDER-RELEASE
   MAP-ALIAS-RELEASE
   REGION-KIND-RELEASE  REGION-START-RELEASE  REGION-END-RELEASE  0 REGION-N ! ;

\ The linked program's map to <path>.map, its code ending at offset end, after
\ the link placed its code and REGION+ recorded what it appended.
: WRITE-MAP ( ptr u8 n n -- ) ['] MAP-WRITE ['] MAP-RELEASE finally ;

;package
