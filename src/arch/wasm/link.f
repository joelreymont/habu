\ link.f - WLINK, the Wasm backend's linker: encoded functions, a data image and
\ table slots written as one Wasm module (docs/wasm-backend.md 10.1, 17.1, 17.5).
\
\ A FUNCTION IS A BODY AND A SIGNATURE. FUNCTION+ copies a body as WENC writes
\ one, its local declarations, code and end, with the lanes its emission row
\ states and its frame variant, and answers its handle: the order it was added
\ in. Every lane is an i64 at a word boundary, so a function in its lanes has
\ the Wasm type (i32 ctx, inputs x i64) -> (i32 status, outputs x i64), and one
\ in the aligned frame (i32) -> (i32).
\
\ ONE ORDER NUMBERS EVERYTHING. Functions are numbered by origin, kernel, then
\ captured, then adapter, each in the order it was added, and then come the four
\ wrappers: run, throw-code, out-base and out-len. Types are deduplicated by
\ their encoding, which is structural, in the order that numbering first uses
\ them. A function keeps its own signature beside its type, so the aligned frame
\ and a function of no lanes share a type and only the latter can be the entry.
\ Table slots are numbered from 1 in the order TABLE+ adds them: slot 0 holds
\ no function, a null funcref, so call_indirect of xt 0 traps as native execute
\ of 0 fails. No global is declared: ctx lives in memory at WPROF's ctx base,
\ so the global index space is empty. Nothing is imported and no start
\ function runs.
\
\ A SITE IS PATCHED IN PLACE. A call site is the offset of a padded five-byte
\ field in its caller's body, and the field gets its callee's index. An address
\ site, a padded ten-byte field, and a data cell, eight bytes of the image low
\ byte first, name a kind and a target. A DATA target is an offset into the
\ image, from zero to its length, and is written as WPROF's data base plus it; a
\ CODE target is a table slot, and is written as the slot. An indirect site,
\ the padded five-byte field after a `call_indirect`, is written as the index of
\ the type (i32) -> (i32), every table slot's (src/arch/wasm/dynamic.f). Only
\ the module's copies are patched, so the rows stay as given and LINK can run
\ again.
\
\ THE MODULE. Sections come in the specification's order: type, function, table
\ when a slot was added, memory, export, element with the table from slot 1,
\ code, and data, one active segment at WPROF's data base. The memory's
\ minimum and maximum are the pages holding the image's end, since nothing
\ grows it. The exports are memory and the four wrappers, whose bodies are
\ written here through WENC's opcode bytes because their signatures lie outside
\ WSTRUCT's call row: run stores WPROF's stack base in ctx's stack base and top
\ and 0 in its out-len, calls the entry with WPROF's ctx base and answers its
\ status; throw-code answers ctx's throw code, out-base WPROF's out base, and
\ out-len ctx's out-len.
\
\ REFUSALS. LINK reads every row before it writes. An entry, site, cell or slot
\ naming no function, image byte or slot is E-WLINK-UNRESOLVED, as is an
\ address of kind NONE, which names nothing; a kind outside NONE, DATA and CODE
\ is E-WSTRUCT-ADDR. A field that is not a padded LEB of its width inside its
\ body is WLEB's refusal, and a cell outside the image E-SPAN-RANGE. An entry
\ that takes or answers a lane is E-WENC-ARITY. DATA! refuses, before it
\ reserves a byte, an image whose end passes memory32's 65536 pages:
\ E-WLINK-MEMORY.
\
\ STORAGE CLASS. MODULE-OWNED: the rows, bodies and image live in this package's
\ buffers until RESET, and the module LINK answers until the next LINK.

require lib/prelude.f
require lib/errors.f
require lib/string.f
require lib/span.f
require src/arch/wasm/leb.f
require src/arch/wasm/wstruct.f
require src/arch/wasm/profile.f
require src/arch/wasm/encode.f

\ WLINK's codes, -9830..-9834, in the Wasm backend's block -9800..-9834.
-9830 constant E-WLINK-FIRST
-9834 constant E-WLINK-LAST
-9830 constant E-WLINK-UNRESOLVED  \ an entry, site, cell or slot naming no function, image byte or table slot of the link
-9831 constant E-WLINK-MEMORY      \ an image whose end, from WPROF's data base, passes memory32's 65536 pages

package WLINK
public

\ Where a function comes from, which places it in index order.
ENUM origin
   kernel
   captured
   adapter
;ENUM

private

\ ---- Wasm's numbers ----------------------------------------------------------
$6D736100 constant MAGIC             \ "\0asm", low byte first
1 constant VERSION
1 constant SEC-TYPE
3 constant SEC-FUNCTION
4 constant SEC-TABLE
5 constant SEC-MEMORY
7 constant SEC-EXPORT
9 constant SEC-ELEMENT
10 constant SEC-CODE
11 constant SEC-DATA
$60 constant FUNC-FORM               \ a function type's first byte
$7F constant I32
$7E constant I64
$70 constant FUNCREF
1 constant LIMITS-MAX                \ limits stating a maximum as well
0 constant EXPORT-FUNC
2 constant EXPORT-MEMORY
0 constant ACTIVE                    \ an active segment of table or memory zero
1 constant FIRST-SLOT                \ the first slot holding a function; 0 is null
$0B constant OP-END
2 constant ALIGN-32                  \ an i32 access's natural alignment, 2^2
3 constant ALIGN-64
$10000 constant PAGE-BYTES
$10000 constant PAGES-MAX            \ memory32's most pages
\ The most image bytes those pages hold above the data base.
PAGES-MAX PAGE-BYTES * WPROF:DATA-BASE - constant IMAGE-MAX
10 constant LEB-MOST                 \ the widest LEB, a 64-bit one
8 constant CELL-BYTES                \ a data cell

\ ---- the signatures an emission row states -------------------------------------
1 constant FRAME-ALIGNED             \ the lanes are in the aligned frame
4 constant WRAPPERS                  \ run, throw-code, out-base and out-len
3 constant RANKS

: RANK ( WLINK:origin -- n )
   MATCH origin
      kernel OF 0 ENDOF
      captured OF 1 ENDOF
      adapter OF 2 ENDOF
   ;MATCH ;

\ ---- the rows -------------------------------------------------------------------
DYNAMIC-BUFFER BODIES u8             \ every body, in the order added
variable BODIES-U
variable NF                          \ functions
DYNAMIC-BUFFER FN-AT n               \ each one's body in BODIES
DYNAMIC-BUFFER FN-LEN n
DYNAMIC-BUFFER FN-IN n               \ its lanes in and out, and its frame variant
DYNAMIC-BUFFER FN-OUT n
DYNAMIC-BUFFER FN-VAR n
DYNAMIC-BUFFER FN-RANK n             \ its origin's place in index order
DYNAMIC-BUFFER FN-IDX n              \ its index, once LINK has numbered it
DYNAMIC-BUFFER FN-TYPE n             \ its type's index, likewise
DYNAMIC-BUFFER FN-POS n              \ where LINK wrote its body in the module
variable NCALLS
DYNAMIC-BUFFER CALL-FN n             \ each call site's caller
DYNAMIC-BUFFER CALL-AT n             \ its field, from the start of the body
DYNAMIC-BUFFER CALL-TGT n            \ its callee
variable NADDRS
DYNAMIC-BUFFER ADDR-FN n             \ each address site's function
DYNAMIC-BUFFER ADDR-AT n
DYNAMIC-BUFFER ADDR-KIND n
DYNAMIC-BUFFER ADDR-TGT n
DYNAMIC-BUFFER IMAGE u8              \ the data image
variable IMAGE-U
variable NCELLS
DYNAMIC-BUFFER CELL-AT n             \ each data cell's offset in the image
DYNAMIC-BUFFER CELL-KIND n
DYNAMIC-BUFFER CELL-TGT n
variable NSLOTS                      \ the slots holding a function
DYNAMIC-BUFFER SLOT-FN n             \ the function in slot FIRST-SLOT + k
variable NINDS
DYNAMIC-BUFFER IND-FN n              \ each indirect site's function
DYNAMIC-BUFFER IND-AT n              \ its field, from the start of the body

\ ---- writing the module ------------------------------------------------------
\ The writer runs dry to measure what it would write, so a size precedes what it
\ counts and OUT is reserved for the whole module.
DYNAMIC-BUFFER OUT u8                \ the module
variable AT                          \ where the next byte goes
variable DRY                         \ nonzero while the writer only measures
LEB-MOST SPAN-BUFFER: LEB            \ one LEB, before it is put

\ Never empty: a name, a type, a body or the image.
: PUT-BYTES ( ptr u8 n -- )
   {: a u:n :}
   DRY @ 0= if  a u  AT @ OUT u SPAN:MAKE  SPAN:COPY  then
   u AT +! ;

: PUT-BYTE ( n -- )
   {: v:n :}
   DRY @ 0= if  v AT @ OUT c!  then
   1 AT +! ;

: PUT-LEB ( n -- )
   {: u:n :}
   LEB SPAN:$ drop u PUT-BYTES ;

: PUT-U32 ( n -- )  LEB WLEB:U32! PUT-LEB ;
: PUT-S32 ( n -- )  LEB WLEB:S32! PUT-LEB ;

\ Four bytes, low first.
: PUT-LE32 ( n -- )
   {: v:n :}
   4 0 do  v i 8 * rshift $FF and PUT-BYTE  loop ;

: PUT-OP ( WSTRUCT:opcode -- )
   WENC:OPCODE-BYTE PUT-BYTE ;

\ i32.const of a number below 2^31, as every address of WPROF's layout is.
: I32-CONST ( n -- )
   WSTRUCT-OPCODE:I32-CONST PUT-OP  PUT-S32 ;

\ What body writes, after its size.
: SIZED ( [ -- ] -- )
   {: body :}
   AT @ DRY @ {: at:n dry:n :}
   1 DRY !  body execute
   AT @ at - {: u:n :}
   at AT !  dry DRY !
   u PUT-U32  body execute ;

: SECTION ( n [ -- ] -- )
   {: id:n body :}
   id PUT-BYTE  body SIZED ;

\ ---- index order ----------------------------------------------------------------
: RANKED ( n [ n -- ] -- )
   {: r:n fn :}
   NF @ 0 ?do  i FN-RANK @ r = if  i fn execute  then  loop ;

\ Each function's handle, in index order: by origin, then in the order added.
: IN-ORDER ( [ n -- ] -- )
   {: fn :}
   RANKS 0 do  i fn RANKED  loop ;

variable NEXT                        \ the next index to number
variable ENTRY-IDX

: NUMBER ( n -- )
   NEXT @ swap FN-IDX !
   1 NEXT +! ;

\ ---- types ----------------------------------------------------------------------
\ The longest type: its form, then two vectors of a count's LEB and 256 types.
1 LEB-MOST 256 + 2 * + constant TY-CAP
TY-CAP BUFFER: TY                    \ the type being interned
variable TY-U
DYNAMIC-BUFFER TYPES u8              \ each type once, in the order first used
variable TYPES-U
variable NTYPES
DYNAMIC-BUFFER TYPE-AT n
DYNAMIC-BUFFER TYPE-LEN n
variable WR-I32                      \ () -> (i32): run, out-base and out-len
variable WR-I64                      \ () -> (i64): throw-code
variable WR-DYN                      \ (i32) -> (i32): an indirect call's

: TY-BYTE ( n -- )
   TY-U @ TY + c!
   1 TY-U +! ;

: TY-U32 ( n -- )
   TY TY-U @ +  LEB-MOST SPAN:MAKE  WLEB:U32!  TY-U +! ;

\ ( i32, k x i64 ), a Habu row: its ctx or status lane, then k cells.
: ROW ( n -- )
   {: k:n :}
   k 1+ TY-U32
   I32 TY-BYTE
   k 0 ?do  I64 TY-BYTE  loop ;

\ The type of a function in its lanes, or in the aligned frame (ctx) -> (status).
: FN-TYPE-OF ( n -- )
   {: k:n :}
   k FN-VAR @ FRAME-ALIGNED = if 0 0 else k FN-IN @ k FN-OUT @ then {: in:n out:n :}
   0 TY-U !
   FUNC-FORM TY-BYTE  in ROW  out ROW ;

\ () -> (t), a wrapper's type.
: RESULT-TYPE ( n -- )
   {: t:n :}
   0 TY-U !
   FUNC-FORM TY-BYTE  0 TY-U32  1 TY-U32  t TY-BYTE ;

: TYPE$ ( n -- ptr u8 n )
   {: k:n :}
   k TYPE-AT @ TYPES  k TYPE-LEN @ ;

\ The index of the type in TY, which a type equal to it already has.
: INTERN ( -- n )
   NTYPES @ 0 ?do
      i TYPE$  TY TY-U @  STR= if i unloop exit then
   loop
   NTYPES @ {: k:n :}
   k 1+ TYPE-AT-RESERVE  k 1+ TYPE-LEN-RESERVE
   TYPES-U @ TY-U @ + TYPES-RESERVE
   TY TY-U @  TYPES-U @ TYPES TY-U @ SPAN:MAKE  SPAN:COPY
   TYPES-U @ k TYPE-AT !  TY-U @ k TYPE-LEN !
   TY-U @ TYPES-U +!
   1 NTYPES +!
   k ;

: TYPE-FN ( n -- )
   {: k:n :}
   k FN-TYPE-OF  INTERN k FN-TYPE ! ;

: TYPE-ALL ( -- )
   0 NTYPES !  0 TYPES-U !
   [: TYPE-FN ;] IN-ORDER
   I32 RESULT-TYPE INTERN WR-I32 !
   I64 RESULT-TYPE INTERN WR-I64 !
   NINDS @ 0 > if  0 TY-U !  FUNC-FORM TY-BYTE  0 ROW  0 ROW  INTERN WR-DYN !  then ;

\ ---- the wrappers ---------------------------------------------------------------
\ ctx's field at off := v, an i32.
: CTX! ( n n -- )
   {: v:n off:n :}
   WPROF:CTX-BASE I32-CONST  v I32-CONST
   WSTRUCT-OPCODE:I32-STORE PUT-OP  ALIGN-32 PUT-U32  off PUT-U32 ;

\ ctx's field at off, read by a load of the alignment al.
: CTX@ ( WSTRUCT:opcode n n -- )
   {: op:WSTRUCT:opcode al:n off:n :}
   WPROF:CTX-BASE I32-CONST
   op PUT-OP  al PUT-U32  off PUT-U32 ;

\ Each body declares no local.
: RUN-BODY ( -- )
   0 PUT-U32
   WPROF:STACK-BASE WPROF:CTX-STACK-BASE CTX!
   WPROF:STACK-BASE WPROF:CTX-STACK-TOP CTX!
   0 WPROF:CTX-OUT-LEN CTX!
   WPROF:CTX-BASE I32-CONST
   WSTRUCT-OPCODE:CALL PUT-OP  ENTRY-IDX @ PUT-U32
   OP-END PUT-BYTE ;

: THROW-CODE-BODY ( -- )
   0 PUT-U32
   WSTRUCT-OPCODE:I64-LOAD ALIGN-64 WPROF:CTX-THROW-CODE CTX@
   OP-END PUT-BYTE ;

: OUT-BASE-BODY ( -- )
   0 PUT-U32
   WPROF:OUT-BASE I32-CONST
   OP-END PUT-BYTE ;

: OUT-LEN-BODY ( -- )
   0 PUT-U32
   WSTRUCT-OPCODE:I32-LOAD ALIGN-32 WPROF:CTX-OUT-LEN CTX@
   OP-END PUT-BYTE ;

\ ---- the sections -------------------------------------------------------------
: BODY$ ( n -- ptr u8 n )
   {: k:n :}
   k FN-AT @ BODIES  k FN-LEN @ ;

\ The pages holding every region and the image.
: PAGES ( -- n )
   WPROF:DATA-BASE IMAGE-U @ +  PAGE-BYTES 1- +  PAGE-BYTES / ;

: TYPE-SEC ( -- )
   NTYPES @ PUT-U32
   0 TYPES TYPES-U @ PUT-BYTES ;

\ The wrappers' types follow the functions': run, throw-code, out-base, out-len.
: FUNCTION-SEC ( -- )
   NF @ WRAPPERS + PUT-U32
   [: FN-TYPE @ PUT-U32 ;] IN-ORDER
   WR-I32 @ PUT-U32  WR-I64 @ PUT-U32  WR-I32 @ PUT-U32  WR-I32 @ PUT-U32 ;

: TABLE-SEC ( -- )
   NSLOTS @ FIRST-SLOT + {: n:n :}
   1 PUT-U32
   FUNCREF PUT-BYTE  LIMITS-MAX PUT-BYTE  n PUT-U32  n PUT-U32 ;

: MEMORY-SEC ( -- )
   1 PUT-U32
   LIMITS-MAX PUT-BYTE  PAGES PUT-U32  PAGES PUT-U32 ;

: PUT-EXPORT ( ptr u8 n n n -- )
   {: a u:n kind:n idx:n :}
   u PUT-U32  a u PUT-BYTES  kind PUT-BYTE  idx PUT-U32 ;

\ The wrappers follow the functions, so run's index is their count.
: EXPORT-SEC ( -- )
   WRAPPERS 1+ PUT-U32
   s" memory" EXPORT-MEMORY 0 PUT-EXPORT
   s" run" EXPORT-FUNC NF @ PUT-EXPORT
   s" throw-code" EXPORT-FUNC NF @ 1+ PUT-EXPORT
   s" out-base" EXPORT-FUNC NF @ 2 + PUT-EXPORT
   s" out-len" EXPORT-FUNC NF @ 3 + PUT-EXPORT ;

: ELEMENT-SEC ( -- )
   1 PUT-U32
   ACTIVE PUT-U32  FIRST-SLOT I32-CONST  OP-END PUT-BYTE
   NSLOTS @ PUT-U32
   NSLOTS @ 0 ?do  i SLOT-FN @ FN-IDX @ PUT-U32  loop ;

: CODE-ENTRY ( n -- )
   {: k:n :}
   k FN-LEN @ PUT-U32
   DRY @ 0= if  AT @ k FN-POS !  then
   k BODY$ PUT-BYTES ;

: CODE-SEC ( -- )
   NF @ WRAPPERS + PUT-U32
   [: CODE-ENTRY ;] IN-ORDER
   [: RUN-BODY ;] SIZED
   [: THROW-CODE-BODY ;] SIZED
   [: OUT-BASE-BODY ;] SIZED
   [: OUT-LEN-BODY ;] SIZED ;

variable IMAGE-POS                   \ where LINK wrote the image in the module

: DATA-SEC ( -- )
   1 PUT-U32
   ACTIVE PUT-U32  WPROF:DATA-BASE I32-CONST  OP-END PUT-BYTE
   IMAGE-U @ PUT-U32
   DRY @ 0= if  AT @ IMAGE-POS !  then
   IMAGE-U @ 0 > if  0 IMAGE IMAGE-U @ PUT-BYTES  then ;

: WRITE-MODULE ( -- )
   MAGIC PUT-LE32  VERSION PUT-LE32
   SEC-TYPE [: TYPE-SEC ;] SECTION
   SEC-FUNCTION [: FUNCTION-SEC ;] SECTION
   NSLOTS @ 0 > if  SEC-TABLE [: TABLE-SEC ;] SECTION  then
   SEC-MEMORY [: MEMORY-SEC ;] SECTION
   SEC-EXPORT [: EXPORT-SEC ;] SECTION
   NSLOTS @ 0 > if  SEC-ELEMENT [: ELEMENT-SEC ;] SECTION  then
   SEC-CODE [: CODE-SEC ;] SECTION
   SEC-DATA [: DATA-SEC ;] SECTION ;

\ ---- reading the rows ------------------------------------------------------------
\ A handle of a function the link holds.
: RESOLVED ( n -- n )
   {: k:n :}
   k 0 <  k NF @ >= or if E-WLINK-UNRESOLVED throw then
   k ;

\ The value an address of a kind and a target is written as.
: FINAL ( n n -- n )
   {: kind:n t:n :}
   kind WSTRUCT:ADDR-DATA = if
      t 0 <  t IMAGE-U @ > or if E-WLINK-UNRESOLVED throw then
      WPROF:DATA-BASE t +  exit
   then
   kind WSTRUCT:ADDR-CODE = if
      t FIRST-SLOT <  t NSLOTS @ FIRST-SLOT + >= or if E-WLINK-UNRESOLVED throw then
      t exit
   then
   kind WSTRUCT:ADDR-NONE = if E-WLINK-UNRESOLVED throw then
   E-WSTRUCT-ADDR throw ;

\ Run calls the entry with ctx alone and answers its status, so the entry takes
\ and answers no lane; FUNCTION+ held a function of none to its lanes variant.
: ENTRY-CK ( n -- )
   {: e:n :}
   e RESOLVED drop
   e FN-IN @ 0<>  e FN-OUT @ 0<> or if E-WENC-ARITY throw then ;

: CALL-CK ( n -- )
   {: k:n :}
   k CALL-TGT @ RESOLVED drop
   k CALL-FN @ RESOLVED BODY$  k CALL-AT @  WLEB:U32-PAD@ drop ;

: IND-CK ( n -- )
   {: k:n :}
   k IND-FN @ RESOLVED BODY$  k IND-AT @  WLEB:U32-PAD@ drop ;

: ADDR-CK ( n -- )
   {: k:n :}
   k ADDR-FN @ RESOLVED BODY$  k ADDR-AT @  WLEB:S64-PAD@ drop
   k ADDR-KIND @ k ADDR-TGT @ FINAL drop ;

\ Bounded by the image's length less a cell: at plus a cell wraps near MAX-N.
: CELL-CK ( n -- )
   {: k:n :}
   k CELL-AT @ {: at:n :}
   at 0 <  at IMAGE-U @ CELL-BYTES - > or if E-SPAN-RANGE throw then
   k CELL-KIND @ k CELL-TGT @ FINAL drop ;

\ Every row, before a byte is written.
: ROWS-CK ( n -- )
   ENTRY-CK
   NSLOTS @ 0 ?do  i SLOT-FN @ RESOLVED drop  loop
   NCALLS @ 0 ?do  i CALL-CK  loop
   NINDS @ 0 ?do  i IND-CK  loop
   NADDRS @ 0 ?do  i ADDR-CK  loop
   NCELLS @ 0 ?do  i CELL-CK  loop ;

\ ---- patching the module ---------------------------------------------------------
: MODULE ( -- SPAN:span<u8> )
   0 OUT AT @ SPAN:MAKE ;

\ Eight bytes at off in the module, low first.
: PUT-CELL ( n n -- )
   {: v:n off:n :}
   CELL-BYTES 0 do  v i 8 * rshift $FF and  off i + OUT c!  loop ;

: PATCH ( -- )
   NCALLS @ 0 ?do
      i CALL-TGT @ FN-IDX @  MODULE  i CALL-FN @ FN-POS @ i CALL-AT @ +  WLEB:U32-PATCH
   loop
   NINDS @ 0 ?do
      WR-DYN @  MODULE  i IND-FN @ FN-POS @ i IND-AT @ +  WLEB:U32-PATCH
   loop
   NADDRS @ 0 ?do
      i ADDR-KIND @ i ADDR-TGT @ FINAL  MODULE  i ADDR-FN @ FN-POS @ i ADDR-AT @ +
      WLEB:S64-PATCH
   loop
   NCELLS @ 0 ?do
      i CELL-KIND @ i CELL-TGT @ FINAL  IMAGE-POS @ i CELL-AT @ +  PUT-CELL
   loop ;

: COLUMN+ ( n -- )
   {: k:n :}
   k 1+ FN-AT-RESERVE  k 1+ FN-LEN-RESERVE  k 1+ FN-IN-RESERVE  k 1+ FN-OUT-RESERVE
   k 1+ FN-VAR-RESERVE  k 1+ FN-RANK-RESERVE  k 1+ FN-IDX-RESERVE
   k 1+ FN-TYPE-RESERVE  k 1+ FN-POS-RESERVE ;

public

: RESET ( -- )
   0 NF !  0 BODIES-U !  0 NCALLS !  0 NADDRS !  0 IMAGE-U !  0 NCELLS !  0 NSLOTS !
   0 NINDS ! ;

\ A body as WENC writes it, the lanes in and out its emission row states, the
\ frame variant WENC picks for them and its origin; answers its handle.
: FUNCTION+ ( ptr u8 n n n n WLINK:origin -- n )
   {: a u:n in:n out:n var:n o:WLINK:origin :}
   NF @ {: k:n :}
   k COLUMN+
   BODIES-U @ u + BODIES-RESERVE
   a u  BODIES-U @ BODIES u SPAN:MAKE  SPAN:COPY
   BODIES-U @ k FN-AT !  u k FN-LEN !
   in k FN-IN !  out k FN-OUT !  var k FN-VAR !  o RANK k FN-RANK !
   u BODIES-U +!
   1 NF +!
   k ;

\ Function f's call field at offset at of its body calls the function t.
: CALL+ ( n n n -- )
   {: f:n at:n t:n :}
   NCALLS @ {: k:n :}
   k 1+ CALL-FN-RESERVE  k 1+ CALL-AT-RESERVE  k 1+ CALL-TGT-RESERVE
   f k CALL-FN !  at k CALL-AT !  t k CALL-TGT !
   1 NCALLS +! ;

\ Function f's call_indirect field at offset at of its body names the type
\ (i32) -> (i32).
: INDIRECT+ ( n n -- )
   {: f:n at:n :}
   NINDS @ {: k:n :}
   k 1+ IND-FN-RESERVE  k 1+ IND-AT-RESERVE
   f k IND-FN !  at k IND-AT !
   1 NINDS +! ;

\ Function f's address field at offset at of its body holds the address of a
\ kind and a target.
: ADDRESS+ ( n n n n -- )
   {: f:n at:n kind:n t:n :}
   NADDRS @ {: k:n :}
   k 1+ ADDR-FN-RESERVE  k 1+ ADDR-AT-RESERVE  k 1+ ADDR-KIND-RESERVE
   k 1+ ADDR-TGT-RESERVE
   f k ADDR-FN !  at k ADDR-AT !  kind k ADDR-KIND !  t k ADDR-TGT !
   1 NADDRS +! ;

\ The data image, in place of any before it.
: DATA! ( ptr u8 n -- )
   {: a u:n :}
   u 0 < if E-SPAN-LENGTH throw then
   u IMAGE-MAX > if E-WLINK-MEMORY throw then
   u IMAGE-RESERVE
   u 0 > if  a u  0 IMAGE u SPAN:MAKE  SPAN:COPY  then
   u IMAGE-U ! ;

\ The image's cell at offset at holds the address of a kind and a target.
: DATA-CELL+ ( n n n -- )
   {: at:n kind:n t:n :}
   NCELLS @ {: k:n :}
   k 1+ CELL-AT-RESERVE  k 1+ CELL-KIND-RESERVE  k 1+ CELL-TGT-RESERVE
   at k CELL-AT !  kind k CELL-KIND !  t k CELL-TGT !
   1 NCELLS +! ;

\ Function f in the table's next slot; answers the slot.
: TABLE+ ( n -- n )
   {: f:n :}
   NSLOTS @ {: k:n :}
   k 1+ SLOT-FN-RESERVE
   f k SLOT-FN !
   1 NSLOTS +!
   k FIRST-SLOT + ;

\ The module, run calling the function e.
: LINK ( n -- ptr u8 n )
   {: e:n :}
   e ROWS-CK
   0 NEXT !  [: NUMBER ;] IN-ORDER
   e FN-IDX @ ENTRY-IDX !
   TYPE-ALL
   0 AT !  1 DRY !  WRITE-MODULE
   AT @ {: u:n :}
   u OUT-RESERVE
   0 AT !  0 DRY !  WRITE-MODULE
   PATCH
   0 OUT u ;

;package

WLINK:RESET
