\ link.f - WLINK, src/arch/wasm/link.f, on the product engine.
\
\ Proves that encoded functions, a data image and table slots are written as one
\ Wasm module. A function and no image come out byte for byte, exporting memory
\ and the four wrappers. Functions are numbered kernel, captured, adapter, each
\ in the order added, then the wrappers; types are deduplicated in the order
\ that numbering first uses them, so a function in the aligned frame shares the
\ type of one of no lanes, and only the latter can be the entry. Each call field
\ holds its callee's index. An address field and a data cell hold WPROF's data
\ base plus a target in the image, its length included, or a table slot; the
\ table and its element segment list the slots' functions. Memory's minimum and
\ maximum are the pages the image ends in. W03: 140 functions and 137 types,
\ chained by calls, have sections and bodies that walk out exactly. Refused,
\ each by its code: an entry, call, address site, table slot or cell naming
\ nothing the link holds, an address of kind NONE among them; an address kind
\ outside NONE, DATA and CODE; a site whose field is not padded or lies past its
\ body; a cell past the image, one whose end would wrap past MAX-N included; an
\ entry that takes or answers a lane; and an image of a negative length or one
\ ending past memory32's 4 GiB, MAX-N bytes included.
\
\ THE BYTES ARE WASM-TOOLS'. The first three linked modules are what wasm-tools
\ 1.243.0 assembles for the same modules written as text, each of which
\ validates with WPROF's features; the table, element and data sections of the
\ fourth are too. A call or address field is checked as the padded LEB WLINK
\ writes, where the text assembler writes the shortest.

require lib/test.f
require lib/errors.f
require lib/string.f
require lib/span.f
require src/core/sha256.f
require src/arch/wasm/leb.f
require src/arch/wasm/wstruct.f
require src/arch/wasm/profile.f
require src/arch/wasm/encode.f
require src/arch/wasm/link.f
require test/wasm/w03.f

package WASM-LINK-TEST
private
using WASM-W03

\ ---- the module under test -----------------------------------------------------
1 constant S-TYPE
3 constant S-FUNCTION
4 constant S-TABLE
5 constant S-MEMORY
7 constant S-EXPORT
9 constant S-ELEMENT
10 constant S-CODE
11 constant S-DATA
12 constant ID-MOST

PTR-VARIABLE MOD-A
variable MOD-U
ID-MOST 1+ TYPED-BUFFER SEC-AT n      \ each section's content, where it starts
ID-MOST 1+ TYPED-BUFFER SEC-LEN n
variable IDS                          \ the section ids in order, a hex digit each
variable POS                          \ where the walk reads next
variable WALKED                       \ where the walk of the sections ended

: MBUF ( -- ptr u8 )  MOD-A @ ;

: B@ ( n -- n )  MBUF + c@ ;

: BYTE> ( -- n )
   POS @ B@  1 POS +! ;

: U32> ( -- n )
   MBUF POS @ +  MOD-U @ POS @ -  WLEB:U32@  POS +! ;

: SECTION> ( -- )
   BYTE> U32> {: id:n len:n :}
   IDS @ 4 lshift id or IDS !
   POS @ id SEC-AT !  len id SEC-LEN !
   len POS +! ;

\ The module run calling function e, its sections walked from past the magic
\ and version.
: LINKED ( n -- )
   WLINK:LINK MOD-U ! MOD-A !
   0 IDS !  8 POS !
   begin POS @ MOD-U @ < while SECTION> repeat
   POS @ WALKED ! ;

: FROM ( n -- )  SEC-AT @ POS ! ;

\ Section id's count and whether its entries, each passed by xt, end at its end.
: ENTRIES ( n [ -- ] -- n bool )
   {: id:n xt :}
   id FROM  U32> {: cnt:n :}
   cnt 0 ?do  xt execute  loop
   cnt  POS @  id SEC-AT @ id SEC-LEN @ +  = ;

\ Code entry k's body: where it starts in the module and its size.
: BODY@ ( n -- n n )
   {: k:n :}
   S-CODE FROM  U32> drop
   k 0 ?do  U32> POS +!  loop
   U32> {: u:n :}
   POS @ u ;

\ Function k's type index.
: TYPE-OF ( n -- n )
   {: k:n :}
   S-FUNCTION FROM  U32> drop
   k 0 ?do  U32> drop  loop
   U32> ;

\ ---- bytes in hex ----------------------------------------------------------------
$800 constant HEX-CAP
HEX-CAP BUFFER: GOT-BUF
HEX-CAP BUFFER: WANT-BUF
TYPED-VARIABLE WANT-U len

\ The module's len bytes from at, in hex.
: HEX$ ( n n -- ptr u8 n )
   {: at:n len:n :}
   len 0 ?do  at i + B@  GOT-BUF i 2 * +  BYTE>HEX  loop
   GOT-BUF len 2 * ;

: SECTION$ ( n -- ptr u8 n )
   {: id:n :}
   id SEC-AT @ id SEC-LEN @ HEX$ ;

: WANT-RESET ( -- )
   WANT-U BUF-RESET ;

: W+ ( ptr u8 n -- )
   WANT-BUF HEX-CAP WANT-U BUF-APPEND ;

: WANT$ ( -- ptr u8 n )
   WANT-BUF WANT-U @ LEN>N ;

\ ---- refusals ----------------------------------------------------------------------
: LINK0 ( -- )  0 WLINK:LINK 2drop ;

$7FFFFFFFFFFFFFFF constant MAX-N

\ ---- bodies ----------------------------------------------------------------------
\ RET-BODY and CALL-BODY build a body in WASM-W03's buffer, test/wasm/w03.f.
: PAD10, ( n -- )   ROOM WLEB:S64-PAD! BODY-U +! ;

\ i64.const v dropped, then status 0. The field is at 2.
: ADDR-BODY ( n -- )
   {: v:n :}
   0 BODY-U !
   0 B,  $42 B, v PAD10,  $1A B,  $41 B, 0 B,  $0B B, ;

\ The body built, as a function of in and out lanes, a frame variant and an
\ origin.
: ADD ( n n n WLINK:origin -- n )
   {: in:n out:n var:n o:WLINK:origin :}
   BODY$ in out var o WLINK:FUNCTION+ ;

: KERNEL ( n -- n )
   0 RET-BODY  0 0 0 WLINK-ORIGIN:KERNEL ADD ;

\ ---- whole modules ---------------------------------------------------------------
\ The process's first link, so its image buffers have never held a byte.
: MIN-ROW ( -- )
   WLINK:RESET
   0 KERNEL drop
   BODY-BUF 0 WLINK:DATA!
   0 LINKED
   s" one function, no image: memory and four wrappers exported, as wasm-tools writes it" T-LABEL
   WANT-RESET
   s" 0061736d01000000010e0360017f017f6000017f6000017e0306050001020101050401010404073205066d656d6f72790200" W+
   s" 0372756e00010a7468726f772d636f64650002086f75742d626173650003076f75742d6c656e00040a4905040041000b2700" W+
   s" 418080044180a008360200418080044180a0083602084180800441003602104180800410000b0900418080042903180b0600" W+
   s" 4180a0040b0900418080042802100b0b0801004180a00c0b00" W+
   0 MOD-U @ HEX$ WANT$ T$= ;

\ Handles 0..4 are an adapter, a kernel, a captured, a kernel and a captured
\ function, of 0, 1, 3, 2 and 4 lanes in; each answers status 10 + its handle.
: ORDER-ROW ( -- )
   WLINK:RESET
   10 0 RET-BODY  0 0 0 WLINK-ORIGIN:ADAPTER ADD drop
   11 0 RET-BODY  1 0 0 WLINK-ORIGIN:KERNEL ADD drop
   12 0 RET-BODY  3 0 0 WLINK-ORIGIN:CAPTURED ADD drop
   13 0 RET-BODY  2 0 0 WLINK-ORIGIN:KERNEL ADD drop
   14 0 RET-BODY  4 0 0 WLINK-ORIGIN:CAPTURED ADD drop
   0 LINKED
   s" kernel, captured, adapter, each as added, then the wrappers; types as first used" T-LABEL
   WANT-RESET
   s" 0061736d01000000012c0760027f7e017f60037f7e7e017f60047f7e7e7e017f60057f7e7e7e7e017f60017f017f6000017f" W+
   s" 6000017e030a09000102030405060505050401010404073205066d656d6f727902000372756e00050a7468726f772d636f64" W+
   s" 650006086f75742d626173650007076f75742d6c656e00080a5d090400410b0b0400410d0b0400410c0b0400410e0b040041" W+
   s" 0a0b2700418080044180a008360200418080044180a0083602084180800441003602104180800410040b0900418080042903" W+
   s" 180b06004180a0040b0900418080042802100b0b0801004180a00c0b00" W+
   0 MOD-U @ HEX$ WANT$ T$= ;

\ Kernels 0..4: no lanes; 17 in, in the aligned frame; one in, twice; one out.
: TYPES-ROW ( -- )
   WLINK:RESET
   1 0 RET-BODY  0 0 0 WLINK-ORIGIN:KERNEL ADD drop
   2 0 RET-BODY  17 0 1 WLINK-ORIGIN:KERNEL ADD drop
   3 0 RET-BODY  1 0 0 WLINK-ORIGIN:KERNEL ADD drop
   4 0 RET-BODY  1 0 0 WLINK-ORIGIN:KERNEL ADD drop
   5 1 RET-BODY  0 1 0 WLINK-ORIGIN:KERNEL ADD drop
   0 LINKED
   s" types deduplicated by structure: the aligned frame shares the type of no lanes" T-LABEL
   WANT-RESET
   s" 0061736d01000000011a0560017f017f60027f7e017f60017f027f7e6000017f6000017e030a090000010102030403030504" W+
   s" 01010404073205066d656d6f727902000372756e00050a7468726f772d636f64650006086f75742d626173650007076f7574" W+
   s" 2d6c656e00080a5f09040041010b040041020b040041030b040041040b0600410542000b2700418080044180a00836020041" W+
   s" 8080044180a0083602084180800441003602104180800410000b0900418080042903180b06004180a0040b09004180800428" W+
   s" 02100b0b0801004180a00c0b00" W+
   0 MOD-U @ HEX$ WANT$ T$=
   s" an entry in the aligned frame is refused though its type is the entry's" T-LABEL
   [: 1 WLINK:LINK 2drop ;] E-WENC-ARITY TTHROWSQ
   s" an entry taking a lane is refused" T-LABEL
   [: 2 WLINK:LINK 2drop ;] E-WENC-ARITY TTHROWSQ
   s" an entry answering a lane is refused" T-LABEL
   [: 4 WLINK:LINK 2drop ;] E-WENC-ARITY TTHROWSQ ;

: ADD0 ( n n n -- )
   WLINK-ORIGIN:KERNEL ADD drop ;

\ ---- sites ----------------------------------------------------------------------
\ Two calls, the first one's status dropped: fields at 4 and 13.
: TWO-CALLS ( -- )
   0 BODY-U !
   0 B,  $20 B, 0 B,  $10 B, 0 PAD5,  $1A B,  $20 B, 0 B,  $10 B, 0 PAD5,  $0B B, ;

\ Handle 0, captured, calls kernel 3 and adapter 4.
: CALLS-ROW ( -- )
   WLINK:RESET
   TWO-CALLS  0 0 0 WLINK-ORIGIN:CAPTURED ADD drop
   1 KERNEL drop  2 KERNEL drop  3 KERNEL drop
   4 0 RET-BODY  0 0 0 WLINK-ORIGIN:ADAPTER ADD drop
   0 4 3 WLINK:CALL+
   0 13 4 WLINK:CALL+
   4 LINKED
   s" each call field holds its callee's index, padded: kernel 3 is 2, the adapter 4" T-LABEL
   3 BODY@ HEX$ s" 0020001082808080001a20001084808080000b" T$= ;

24 constant IMAGE-LEN
IMAGE-LEN BUFFER: IMAGE-BUF            \ byte i holds i

: IMAGE-FILL ( -- )
   IMAGE-LEN 0 do  i IMAGE-BUF i + c!  loop ;

\ Kernels 0 and 1 hold a data and a code address, 2 and 3 are slots 1 and 0;
\ the image's cells at 0 and at its last eight bytes hold the same kinds.
: TABLE-ROW ( -- )
   WLINK:RESET
   IMAGE-FILL
   IMAGE-BUF IMAGE-LEN WLINK:DATA!
   0 ADDR-BODY  0 0 0 ADD0
   0 ADDR-BODY  0 0 0 ADD0
   2 KERNEL drop  3 KERNEL drop
   s" table slots number in the order added" T-LABEL
   3 WLINK:TABLE+ 0 T=
   2 WLINK:TABLE+ 1 T=
   0 2 WSTRUCT:ADDR-DATA IMAGE-LEN WLINK:ADDRESS+
   1 2 WSTRUCT:ADDR-CODE 1 WLINK:ADDRESS+
   0 WSTRUCT:ADDR-DATA 8 WLINK:DATA-CELL+
   IMAGE-LEN 8 - WSTRUCT:ADDR-CODE 0 WLINK:DATA-CELL+
   2 LINKED
   s" type, function, table, memory, export, element, code, data, ending at the module's end" T-LABEL
   IDS @ $134579AB T=
   WALKED @ MOD-U @ T=
   s" the table holds two slots and the element segment functions 3 and 2" T-LABEL
   S-TABLE SECTION$ s" 0170010202" T$=
   S-ELEMENT SECTION$ s" 010041000b020302" T$=
   s" a data address is the data base plus its target, the image's length included" T-LABEL
   MBUF MOD-U @  0 BODY@ drop 2 +  WLEB:S64-PAD@  $31018 T=
   s" a code address is its slot" T-LABEL
   MBUF MOD-U @  1 BODY@ drop 2 +  WLEB:S64-PAD@  1 T=
   s" data cells hold the same, low byte first, in one active segment at the data base" T-LABEL
   S-DATA SECTION$ s" 01004180a00c0b18081003000000000008090a0b0c0d0e0f0000000000000000" T$= ;

61441 BUFFER: BIG                      \ zeros, an image one byte past four pages

\ From $31000, 61440 bytes end at 4 pages and one more needs a fifth.
: PAGES-ROW ( -- )
   WLINK:RESET
   0 KERNEL drop
   BIG 61440 WLINK:DATA!
   0 LINKED
   s" an image ending at a page's end: memory's minimum and maximum 4 pages" T-LABEL
   S-MEMORY SECTION$ s" 01010404" T$=
   BIG 61441 WLINK:DATA!
   0 LINKED
   s" one byte more: 5 pages" T-LABEL
   S-MEMORY SECTION$ s" 01010505" T$= ;

\ ---- W03 --------------------------------------------------------------------------
\ WASM-W03:BUILD's module, test/wasm/w03.f.

\ Function k's body in the module is as added, its field holding k + 1.
: CHAIN-OK? ( n -- bool )
   {: k:n :}
   k k 1+ CHAIN-BODY
   k BODY@ {: at:n u:n :}
   MBUF at + u  BODY$  STR= ;

\ Run's body, calling the entry at index 135.
: RUN-135 ( -- )
   WANT-RESET
   s" 00418080044180a008360200418080044180a0083602084180800441003602104180800410" W+
   s" 87010b" W+ ;

: W03-ROW ( -- )
   BUILD
   CHAIN LINKED
   s" W03: the sections walk out to the module's end" T-LABEL
   IDS @ $1357AB T=
   WALKED @ MOD-U @ T=
   s" W03: 137 types walk out to the type section's end" T-LABEL
   S-TYPE [: 1 POS +!  U32> POS +!  U32> POS +! ;] ENTRIES TTRUE 137 T=
   s" W03: 140 type indices walk out to the function section's end" T-LABEL
   S-FUNCTION [: U32> drop ;] ENTRIES TTRUE 140 T=
   s" W03: 140 bodies walk out to the code section's end" T-LABEL
   S-CODE [: U32> POS +! ;] ENTRIES TTRUE 140 T=
   s" W03: types past 127: the chain's last 134, the entry 0, run 135, throw-code 136" T-LABEL
   134 TYPE-OF 134 T=
   135 TYPE-OF 0 T=
   136 TYPE-OF 135 T=
   137 TYPE-OF 136 T=
   s" W03: every body is as added, its call field the next function's index" T-LABEL
   0  CHAIN 0 do  i CHAIN-OK? 0= if 1+ then  loop  0 T=
   s" W03: the entry calls function 0, run calls the entry, 135, in two bytes" T-LABEL
   CHAIN BODY@ HEX$ s" 0020001080808080000b" T$=
   RUN-135  CHAIN 1+ BODY@ HEX$ WANT$ T$=
   s" W03: the wrappers are exported at 136 to 139" T-LABEL
   WANT-RESET
   s" 05066d656d6f727902000372756e0088010a7468726f772d636f646500890108" W+
   s" 6f75742d62617365008a01076f75742d6c656e008b01" W+
   S-EXPORT SECTION$ WANT$ T$= ;

\ ---- refused rows -------------------------------------------------------------------
\ Kernel 0 returns, 1 calls 0 at 4 and 2 holds the image's end at 2; slot 0 is
\ function 0 and the image's cell at 8 its descriptor. It links, so each
\ refusal below is the one row added to it.
: BASE ( -- )
   WLINK:RESET
   IMAGE-FILL
   IMAGE-BUF 16 WLINK:DATA!
   0 KERNEL drop
   0 0 0 0 CALL-BODY  0 0 0 ADD0
   0 ADDR-BODY  0 0 0 ADD0
   1 4 0 WLINK:CALL+
   2 2 WSTRUCT:ADDR-DATA 16 WLINK:ADDRESS+
   0 WLINK:TABLE+ drop
   8 WSTRUCT:ADDR-CODE 0 WLINK:DATA-CELL+
   0 LINKED ;

: ENTRY-ROWS ( -- )
   s" an entry past the functions is refused" T-LABEL
   BASE  [: 3 WLINK:LINK 2drop ;] E-WLINK-UNRESOLVED TTHROWSQ
   s" an entry below zero is refused" T-LABEL
   BASE  [: -1 WLINK:LINK 2drop ;] E-WLINK-UNRESOLVED TTHROWSQ ;

: CALL-ROWS ( -- )
   s" a call of no function is refused" T-LABEL
   BASE  1 4 3 WLINK:CALL+  [: LINK0 ;] E-WLINK-UNRESOLVED TTHROWSQ
   s" a call site in no function is refused" T-LABEL
   BASE  3 4 0 WLINK:CALL+  [: LINK0 ;] E-WLINK-UNRESOLVED TTHROWSQ
   s" a call field that is not padded is refused" T-LABEL
   BASE  0 BODY-U !  0 B, $20 B, 0 B, $10 B, 0 B, $0B B,  0 0 0 ADD0
   3 4 0 WLINK:CALL+  [: LINK0 ;] WLEB:E-UNPADDED TTHROWSQ
   s" a call field past its function's body is refused" T-LABEL
   BASE  1 11 0 WLINK:CALL+  [: LINK0 ;] E-SPAN-RANGE TTHROWSQ ;

: ADDRESS-ROWS ( -- )
   s" an address site in no function is refused" T-LABEL
   BASE  3 2 WSTRUCT:ADDR-DATA 0 WLINK:ADDRESS+  [: LINK0 ;] E-WLINK-UNRESOLVED TTHROWSQ
   s" a data address past the image's end is refused" T-LABEL
   BASE  2 2 WSTRUCT:ADDR-DATA 17 WLINK:ADDRESS+  [: LINK0 ;] E-WLINK-UNRESOLVED TTHROWSQ
   s" a data address below the image is refused" T-LABEL
   BASE  2 2 WSTRUCT:ADDR-DATA -1 WLINK:ADDRESS+  [: LINK0 ;] E-WLINK-UNRESOLVED TTHROWSQ
   s" a code address past the table is refused" T-LABEL
   BASE  2 2 WSTRUCT:ADDR-CODE 1 WLINK:ADDRESS+  [: LINK0 ;] E-WLINK-UNRESOLVED TTHROWSQ
   s" a code address below the table is refused" T-LABEL
   BASE  2 2 WSTRUCT:ADDR-CODE -1 WLINK:ADDRESS+  [: LINK0 ;] E-WLINK-UNRESOLVED TTHROWSQ
   s" an address of kind none names nothing and is refused" T-LABEL
   BASE  2 2 WSTRUCT:ADDR-NONE 0 WLINK:ADDRESS+  [: LINK0 ;] E-WLINK-UNRESOLVED TTHROWSQ
   s" an address of a kind past code is refused" T-LABEL
   BASE  2 2 WSTRUCT:ADDR-CODE 1+ 0 WLINK:ADDRESS+  [: LINK0 ;] E-WSTRUCT-ADDR TTHROWSQ
   s" an address of a kind below none is refused" T-LABEL
   BASE  2 2 WSTRUCT:ADDR-NONE 1- 0 WLINK:ADDRESS+  [: LINK0 ;] E-WSTRUCT-ADDR TTHROWSQ
   s" an address field that is not padded is refused" T-LABEL
   BASE  0 BODY-U !  0 B, $42 B, 0 B, $1A B, $41 B, 0 B, $0B B,  0 0 0 ADD0
   3 2 WSTRUCT:ADDR-DATA 0 WLINK:ADDRESS+  [: LINK0 ;] WLEB:E-UNPADDED TTHROWSQ ;

: SLOT-ROWS ( -- )
   s" a table slot of no function is refused" T-LABEL
   BASE  3 WLINK:TABLE+ drop  [: LINK0 ;] E-WLINK-UNRESOLVED TTHROWSQ
   s" a cell past the image's end is refused" T-LABEL
   BASE  9 WSTRUCT:ADDR-DATA 0 WLINK:DATA-CELL+  [: LINK0 ;] E-SPAN-RANGE TTHROWSQ
   s" a cell below the image is refused" T-LABEL
   BASE  -1 WSTRUCT:ADDR-DATA 0 WLINK:DATA-CELL+  [: LINK0 ;] E-SPAN-RANGE TTHROWSQ
   s" a cell at MAX-N, whose end would wrap, is refused" T-LABEL
   BASE  MAX-N WSTRUCT:ADDR-DATA 0 WLINK:DATA-CELL+  [: LINK0 ;] E-SPAN-RANGE TTHROWSQ
   s" a cell addressing past the image's end is refused" T-LABEL
   BASE  0 WSTRUCT:ADDR-DATA 17 WLINK:DATA-CELL+  [: LINK0 ;] E-WLINK-UNRESOLVED TTHROWSQ
   s" an image of a negative length is refused" T-LABEL
   [: IMAGE-BUF -1 WLINK:DATA! ;] E-SPAN-LENGTH TTHROWSQ ;

\ ---- memory ---------------------------------------------------------------------
$100000000 constant MEMORY32-BYTES     \ 65536 pages of 64 KiB

\ DATA! refuses on the length alone, before it reserves or reads a byte, so BIG
\ need not hold the bytes these lengths name.
: MEMORY-ROW ( -- )
   s" an image ending one byte past memory32's 4 GiB is refused" T-LABEL
   [: BIG  MEMORY32-BYTES WPROF:DATA-BASE - 1+  WLINK:DATA! ;] E-WLINK-MEMORY TTHROWSQ
   s" an image of MAX-N bytes, whose end would wrap, is refused" T-LABEL
   [: BIG MAX-N WLINK:DATA! ;] E-WLINK-MEMORY TTHROWSQ ;

public

: RUN ( -- )
   T-RESET
   MIN-ROW
   ORDER-ROW
   TYPES-ROW
   CALLS-ROW
   TABLE-ROW
   PAGES-ROW
   W03-ROW
   ENTRY-ROWS
   CALL-ROWS
   ADDRESS-ROWS
   SLOT-ROWS
   MEMORY-ROW
   T-REPORT ;

;package

WASM-LINK-TEST:RUN
