\ link.f - WLINK, the Wasm backend's linker: encoded functions, a data image and
\ table slots written as one Wasm module (docs/xcomp.md "Linking"); the rows come
\ from LINKER's Wasm writer (src/arch/wasm/link-module.f) and
\ src/habu/link-wasm.f. The image's byte at offset off is memory's byte at
\ WPROF:DATA-BASE + off. LINK checks every row before it writes, stores the data
\ cells into its copy of the image and patches only the module, so LINK can run
\ again.
\ STORAGE CLASS. MODULE-OWNED: the rows, bodies and image live in this package's
\ buffers until RESET, and the module LINK answers until the next LINK.
\
\ THE WRAPPERS. The exports are memory and the wrappers. run initializes the
\ task DATA stack header and zeroes its return and loop depths, so a throw
\ uncaught inside a loop leaves no depth for the next host turn; it stores
\ WPROF's stack base in ctx's stack base and top and 0 in its out-len, calls the
\ entry with WPROF's ctx base and answers its status; throw-code answers ctx's
\ throw code, out-base WPROF's out base, out-len ctx's out-len; fault-kind and
\ fault-addr answer ctx's fault kind and address, which a checked fault stores
\ before it traps and no other trap does. reserve, (i64 bytes) -> (i64 address),
\ calls map-anon and answers its address, 0 when refused: a host copies each
\ delivery into pages the module reserved (src/lib/browser/turn.js).
\
\ THE SECTIONS. In the specification's order: type, function, table when a slot
\ was added, memory, export, element from slot 1, code, and data, one active
\ segment per run of the image. Memory's minimum is the pages holding the DATA
\ region and the image, its maximum memory32's 65536, which map-anon grows
\ toward. No global is declared (ctx lives in memory at WPROF's ctx base),
\ nothing is imported and no start function runs.

require src/lib/prelude.f
require src/lib/error.f
require src/lib/string.f
require src/lib/span.f
require src/arch/wasm/leb.f
require src/habu/layout.f
require src/arch/wasm/profile.f
require src/arch/wasm/code.f
require src/compiler/native/emission.f

\ The Wasm backend's error block is -9800..-9834 (src/lib/error.f).
-9830 constant E-WLINK-FIRST
-9834 constant E-WLINK-LAST
-9830 constant E-WLINK-UNRESOLVED  \ an entry, site, cell or slot naming no function, image byte or table slot of the link
-9831 constant E-WLINK-MEMORY      \ an image whose end, from WPROF's DATA base, passes memory32's 65536 pages
-9832 constant E-WLINK-ARITY       \ an entry that takes or answers a lane, or a reserve callee of lanes without map-anon's
-9833 constant E-WLINK-KIND        \ an address kind outside NONE, DATA and CODE

package WLINK
public

\ Where a function comes from, which places it in index order.
ENUM origin
   kernel
   captured
   adapter
;ENUM

private
using WCODE

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
$70 constant FUNCREF
1 constant LIMITS-MAX                \ limits stating a maximum as well
0 constant EXPORT-FUNC
2 constant EXPORT-MEMORY
0 constant ACTIVE                    \ an active segment of table or memory zero
1 constant FIRST-SLOT                \ the first slot holding a function; 0 is null
0 constant ADDR-NONE                 \ the address kind of a field holding no address
WPROF:PAGE-BYTES constant PAGE-BYTES
WPROF:PAGES-MAX constant PAGES-MAX
WPROF:DATA-MAX constant IMAGE-MAX     \ the most image bytes the DATA region holds
\ The shared layout's reserved band through DATA-START, in whole pages.
DATA-START PAGE-BYTES 1- + PAGE-BYTES / PAGE-BYTES * constant DATA-BYTES
10 constant LEB-MOST                 \ the widest LEB, a 64-bit one
8 constant CELL-BYTES                \ a data cell

\ ---- the signatures an emission row states -------------------------------------
1 constant FRAME-ALIGNED             \ the lanes are in the aligned frame
\ run, throw-code, out-base, out-len, fault-kind and fault-addr
6 constant WRAPPERS
variable RESERVES                    \ 1 once RESERVE! named reserve's callee, else 0
variable RES-FN                      \ that callee
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
variable NRUNS                       \ the image's runs of non-zero bytes, once LINK found them
DYNAMIC-BUFFER RUN-AT n              \ each run's first offset in the image
DYNAMIC-BUFFER RUN-LEN n
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
: PUT-S64 ( n -- )  LEB WLEB:S64! PUT-LEB ;

: PUT-LE32 ( n -- )
   {: v:n :}
   4 0 do  v i 8 * rshift $FF and PUT-BYTE  loop ;


\ i32.const of a number below 2^31, as every address of WPROF's layout is.
: I32-CONST ( n -- )
   OP-I32-CONST PUT-BYTE  PUT-S32 ;

: I64-CONST ( n -- )
   OP-I64-CONST PUT-BYTE  PUT-S64 ;

\ i32.const of a memory address, 0 to 2^32 - 1: one from 2^31 up is written as
\ the negative number of the same 32 bits.
: ADDRESS-CONST ( n -- )
   {: a:n :}
   a $7FFFFFFF > if  a $100000000 -  else  a  then  I32-CONST ;

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
\ Functions are numbered by origin (kernel, captured, adapter), each in the order
\ added, then the wrappers: run, throw-code, out-base, out-len, fault-kind,
\ fault-addr and, once RESERVE! named its callee, reserve.
: RANKED ( n [ n -- ] -- )
   {: r:n fn :}
   NF @ 0 ?do  i FN-RANK @ r = if  i fn execute  then  loop ;

: IN-ORDER ( [ n -- ] -- )
   {: fn :}
   RANKS 0 do  i fn RANKED  loop ;

variable NEXT                        \ the next index to number
variable ENTRY-IDX

: NUMBER ( n -- )
   NEXT @ swap FN-IDX !
   1 NEXT +! ;

\ ---- types ----------------------------------------------------------------------
\ Types are deduplicated by their encoding, in the order the numbering first uses
\ them. A function keeps its own signature beside its type: the aligned frame and
\ a function of no lanes share a type, and only the latter can be the entry.
\ The longest type: its form, then two vectors of a count's LEB and 256 types.
1 LEB-MOST 256 + 2 * + constant TY-CAP
TY-CAP BUFFER: TY                    \ the type being interned
variable TY-U
DYNAMIC-BUFFER TYPES u8              \ each type once, in the order first used
variable TYPES-U
variable NTYPES
DYNAMIC-BUFFER TYPE-AT n
DYNAMIC-BUFFER TYPE-LEN n
variable WR-I32                      \ () -> (i32): run, out-base, out-len, fault-kind
variable WR-I64                      \ () -> (i64): throw-code, fault-addr
variable WR-RES                      \ (i64) -> (i64): reserve
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
   RESERVES @ 0<> if
      0 TY-U !  FUNC-FORM TY-BYTE  1 TY-U32  I64 TY-BYTE  1 TY-U32  I64 TY-BYTE
      INTERN WR-RES !
   then
   NINDS @ 0 > if  0 TY-U !  FUNC-FORM TY-BYTE  0 ROW  0 ROW  INTERN WR-DYN !  then ;

\ ---- the wrappers ---------------------------------------------------------------
\ ctx's field at off := v, an i32.
: CTX! ( n n -- )
   {: v:n off:n :}
   WPROF:CTX-BASE I32-CONST  v I32-CONST
   OP-I32-STORE PUT-BYTE  ALIGN-32 PUT-U32  off PUT-U32 ;

\ A full cell in the task-local DATA header; the copied native image holds no
\ Wasm stack address or capacity.
: DATA-CELL! ( n n -- )
   {: v:n off:n :}
   WPROF:DATA-BASE off + I32-CONST  v I64-CONST
   OP-I64-STORE PUT-BYTE  ALIGN-64 PUT-U32  0 PUT-U32 ;

\ The data stack's first cell := local k, an i64.
: STACK-CELL! ( n -- )
   {: k:n :}
   WPROF:STACK-BASE I32-CONST  OP-LOCAL-GET PUT-BYTE  k PUT-U32
   OP-I64-STORE PUT-BYTE  ALIGN-64 PUT-U32  0 PUT-U32 ;

: STACK-CELL@ ( -- )
   WPROF:STACK-BASE I32-CONST
   OP-I64-LOAD PUT-BYTE  ALIGN-64 PUT-U32  0 PUT-U32 ;

\ ctx's field at off, read by a load of the alignment al.
: CTX@ ( n n n -- )
   {: op:n al:n off:n :}
   WPROF:CTX-BASE I32-CONST
   op PUT-BYTE  al PUT-U32  off PUT-U32 ;

\ Each body but reserve's for a callee of lanes declares no local.
: RUN-BODY ( -- )
   0 PUT-U32
   WPROF:STACK-BASE STACK-ABI:BASE-CELL DATA-CELL!
   WPROF:STACK-BYTES STACK-ABI:CAP-CELL DATA-CELL!
   0 RSP-CELL DATA-CELL!
   0 LOOPSP-CELL DATA-CELL!
   WPROF:STACK-BASE WPROF:CTX-STACK-BASE CTX!
   WPROF:STACK-BASE WPROF:CTX-STACK-TOP CTX!
   0 WPROF:CTX-OUT-LEN CTX!
   WPROF:CTX-BASE I32-CONST
   OP-CALL PUT-BYTE  ENTRY-IDX @ PUT-U32
   OP-END PUT-BYTE ;

: THROW-CODE-BODY ( -- )
   0 PUT-U32
   OP-I64-LOAD ALIGN-64 WPROF:CTX-THROW-CODE CTX@
   OP-END PUT-BYTE ;

: OUT-BASE-BODY ( -- )
   0 PUT-U32
   WPROF:OUT-BASE I32-CONST
   OP-END PUT-BYTE ;

: OUT-LEN-BODY ( -- )
   0 PUT-U32
   OP-I32-LOAD ALIGN-32 WPROF:CTX-OUT-LEN CTX@
   OP-END PUT-BYTE ;

: FAULT-KIND-BODY ( -- )
   0 PUT-U32
   OP-I32-LOAD ALIGN-32 WPROF:CTX-FAULT-KIND CTX@
   OP-END PUT-BYTE ;

: FAULT-ADDR-BODY ( -- )
   0 PUT-U32
   OP-I64-LOAD ALIGN-64 WPROF:CTX-FAULT-ADDR CTX@
   OP-END PUT-BYTE ;

\ A frame callee takes the count at the data stack's first cell and answers
\ the address there, the ior above it; the stack is empty between runs.
: FRAME-RESERVE ( -- )
   0 PUT-U32
   WPROF:STACK-BASE CELL-BYTES + WPROF:CTX-STACK-TOP CTX!
   0 STACK-CELL!
   WPROF:CTX-BASE I32-CONST
   OP-CALL PUT-BYTE  RES-FN @ FN-IDX @ PUT-U32
   OP-DROP PUT-BYTE
   STACK-CELL@
   OP-END PUT-BYTE ;

\ A callee of lanes answers status, address and ior; local 1, an i64, holds the
\ address while the status under it drops.
: LANES-RESERVE ( -- )
   1 PUT-U32  1 PUT-U32  I64 PUT-BYTE
   WPROF:CTX-BASE I32-CONST
   OP-LOCAL-GET PUT-BYTE  0 PUT-U32
   OP-CALL PUT-BYTE  RES-FN @ FN-IDX @ PUT-U32
   OP-DROP PUT-BYTE
   OP-LOCAL-SET PUT-BYTE  1 PUT-U32
   OP-DROP PUT-BYTE
   OP-LOCAL-GET PUT-BYTE  1 PUT-U32
   OP-END PUT-BYTE ;

\ Reserve's body, by its callee's frame variant.
: RESERVE-BODY ( -- )
   RES-FN @ FN-VAR @ FRAME-ALIGNED = if  FRAME-RESERVE exit  then
   LANES-RESERVE ;

\ ---- the sections -------------------------------------------------------------
: BODY$ ( n -- ptr u8 n )
   {: k:n :}
   k FN-AT @ BODIES  k FN-LEN @ ;

\ The pages holding every region and the image. The run wrapper and tier 0's
\ code store into the DATA region's header cells whatever the image's length.
: PAGES ( -- n )
   WPROF:DATA-BASE  IMAGE-U @ DATA-BYTES max +  PAGE-BYTES 1- +  PAGE-BYTES / ;

: TYPE-SEC ( -- )
   NTYPES @ PUT-U32
   0 TYPES TYPES-U @ PUT-BYTES ;

: FUNCTION-SEC ( -- )
   NF @ WRAPPERS + RESERVES @ + PUT-U32
   [: FN-TYPE @ PUT-U32 ;] IN-ORDER
   WR-I32 @ PUT-U32  WR-I64 @ PUT-U32  WR-I32 @ PUT-U32  WR-I32 @ PUT-U32
   WR-I32 @ PUT-U32  WR-I64 @ PUT-U32
   RESERVES @ 0<> if  WR-RES @ PUT-U32  then ;

: TABLE-SEC ( -- )
   NSLOTS @ FIRST-SLOT + {: n:n :}
   1 PUT-U32
   FUNCREF PUT-BYTE  LIMITS-MAX PUT-BYTE  n PUT-U32  n PUT-U32 ;

: MEMORY-SEC ( -- )
   1 PUT-U32
   LIMITS-MAX PUT-BYTE  PAGES PUT-U32  PAGES-MAX PUT-U32 ;

: PUT-EXPORT ( ptr u8 n n n -- )
   {: a u:n kind:n idx:n :}
   u PUT-U32  a u PUT-BYTES  kind PUT-BYTE  idx PUT-U32 ;

\ The wrappers follow the functions, so run's index is their count.
: EXPORT-SEC ( -- )
   WRAPPERS 1+ RESERVES @ + PUT-U32
   s" memory" EXPORT-MEMORY 0 PUT-EXPORT
   s" run" EXPORT-FUNC NF @ PUT-EXPORT
   s" throw-code" EXPORT-FUNC NF @ 1+ PUT-EXPORT
   s" out-base" EXPORT-FUNC NF @ 2 + PUT-EXPORT
   s" out-len" EXPORT-FUNC NF @ 3 + PUT-EXPORT
   s" fault-kind" EXPORT-FUNC NF @ 4 + PUT-EXPORT
   s" fault-addr" EXPORT-FUNC NF @ 5 + PUT-EXPORT
   RESERVES @ 0<> if  s" reserve" EXPORT-FUNC NF @ WRAPPERS + PUT-EXPORT  then ;

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
   NF @ WRAPPERS + RESERVES @ + PUT-U32
   [: CODE-ENTRY ;] IN-ORDER
   [: RUN-BODY ;] SIZED
   [: THROW-CODE-BODY ;] SIZED
   [: OUT-BASE-BODY ;] SIZED
   [: OUT-LEN-BODY ;] SIZED
   [: FAULT-KIND-BODY ;] SIZED
   [: FAULT-ADDR-BODY ;] SIZED
   RESERVES @ 0<> if  [: RESERVE-BODY ;] SIZED  then ;

: DATA-SEC ( -- )
   NRUNS @ PUT-U32
   NRUNS @ 0 ?do
      ACTIVE PUT-U32  WPROF:DATA-BASE i RUN-AT @ + ADDRESS-CONST  OP-END PUT-BYTE
      i RUN-LEN @ PUT-U32
      i RUN-AT @ IMAGE  i RUN-LEN @ PUT-BYTES
   loop ;

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
\ An entry, site, cell or slot naming no function, image byte or slot, and an
\ address of kind NONE, are E-WLINK-UNRESOLVED; a kind outside NONE, DATA and
\ CODE is E-WLINK-KIND. A reserve callee naming no function is
\ E-WLINK-UNRESOLVED, and one of lanes without map-anon's (one in, two out)
\ E-WLINK-ARITY, as is an entry that takes or answers a lane. A field that is not
\ a padded LEB of its width inside its body is WLEB's refusal, and a cell outside
\ the image E-SPAN-RANGE.
\ A handle of a function the link holds.
: RESOLVED ( n -- n )
   {: k:n :}
   k 0 <  k NF @ >= or if E-WLINK-UNRESOLVED throw then
   k ;

\ The value an address of a kind and a target is written as: a DATA target is an
\ image offset, 0 to its length, written as WPROF's DATA base plus it; a CODE
\ target is a table slot, written as the slot.
: FINAL ( n n -- n )
   {: kind:n t:n :}
   kind NEMIT:ADDR-DATA = if
      t 0 <  t IMAGE-U @ > or if E-WLINK-UNRESOLVED throw then
      WPROF:DATA-BASE t +  exit
   then
   kind NEMIT:ADDR-CODE = if
      t FIRST-SLOT <  t NSLOTS @ FIRST-SLOT + >= or if E-WLINK-UNRESOLVED throw then
      t exit
   then
   kind ADDR-NONE = if E-WLINK-UNRESOLVED throw then
   E-WLINK-KIND throw ;

\ Run calls the entry with ctx alone and answers its status, so the entry takes
\ and answers no lane; FUNCTION+ held a function of none to its lanes variant.
: ENTRY-CK ( n -- )
   {: e:n :}
   e RESOLVED drop
   e FN-IN @ 0<>  e FN-OUT @ 0<> or if E-WLINK-ARITY throw then ;

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

: RESERVE-CK ( -- )
   RESERVES @ 0= if exit then
   RES-FN @ RESOLVED {: f:n :}
   f FN-VAR @ FRAME-ALIGNED = if exit then
   f FN-IN @ 1 <>  f FN-OUT @ 2 <> or if E-WLINK-ARITY throw then ;

: ROWS-CK ( n -- )
   ENTRY-CK
   RESERVE-CK
   NSLOTS @ 0 ?do  i SLOT-FN @ RESOLVED drop  loop
   NCALLS @ 0 ?do  i CALL-CK  loop
   NINDS @ 0 ?do  i IND-CK  loop
   NADDRS @ 0 ?do  i ADDR-CK  loop
   NCELLS @ 0 ?do  i CELL-CK  loop ;

\ ---- the image's cells and runs ---------------------------------------------------
\ A data cell is eight image bytes, low first, holding its kind and target's FINAL
\ value; LINK stores every cell before it finds the runs.
: IMAGE-CELL! ( n -- )
   {: k:n :}
   k CELL-KIND @ k CELL-TGT @ FINAL  k CELL-AT @  {: v:n off:n :}
   CELL-BYTES 0 do  v i 8 * rshift $FF and  off i + IMAGE c!  loop ;

\ Memory starts zeroed: the data section holds the image's non-zero runs, split
\ at GAP zero bytes: GAP-MIN, over a segment header's 13, doubled to fit RUNS-MAX.
16 constant GAP-MIN
100000 constant RUNS-MAX             \ the most data segments a JS host must load (the JS API's limit)
variable GAP                         \ the zero bytes that end a run
variable RUN-FIRST                   \ the open run's first offset, -1 while none is open
variable RUN-LAST                    \ its last non-zero byte's offset

: RUN-CLOSE ( -- )
   NRUNS @ {: k:n :}
   k 1+ RUN-AT-RESERVE  k 1+ RUN-LEN-RESERVE
   RUN-FIRST @ k RUN-AT !  RUN-LAST @ 1+ RUN-FIRST @ - k RUN-LEN !
   1 NRUNS +!
   -1 RUN-FIRST ! ;

\ Byte at, which is non-zero, ends the open run when GAP zero bytes lie before
\ it, then opens a run or extends the open one.
: RUN-BYTE ( n -- )
   {: at:n :}
   RUN-FIRST @ 0 >=  at RUN-LAST @ - 1- GAP @ >= and if RUN-CLOSE then
   RUN-FIRST @ 0 < if at RUN-FIRST ! then
   at RUN-LAST ! ;

: SCAN-RUNS ( -- )
   0 NRUNS !  -1 RUN-FIRST !
   IMAGE-U @ 0 ?do  i IMAGE c@ 0<> if i RUN-BYTE then  loop
   RUN-FIRST @ 0 >= if RUN-CLOSE then ;

\ The doubling ends: a gap as long as the image leaves one run.
: FIND-RUNS ( -- )
   GAP-MIN GAP !  SCAN-RUNS
   begin  NRUNS @ RUNS-MAX >  while  GAP @ 2 * GAP !  SCAN-RUNS  repeat ;

\ ---- patching the module ---------------------------------------------------------
\ A call site is a padded five-byte field in its caller's body and gets its
\ callee's index. An address site, a padded ten-byte field, gets its kind and
\ target's FINAL value. An indirect site, the padded five-byte field after
\ call_indirect, gets the index of (i32) -> (i32), every table slot's type
\ (src/arch/wasm/dynamic.f).
: MODULE ( -- SPAN:span<u8> )
   0 OUT AT @ SPAN:MAKE ;

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
   loop ;

: COLUMN+ ( n -- )
   {: k:n :}
   k 1+ FN-AT-RESERVE  k 1+ FN-LEN-RESERVE  k 1+ FN-IN-RESERVE  k 1+ FN-OUT-RESERVE
   k 1+ FN-VAR-RESERVE  k 1+ FN-RANK-RESERVE  k 1+ FN-IDX-RESERVE
   k 1+ FN-TYPE-RESERVE  k 1+ FN-POS-RESERVE ;

public

: RESET ( -- )
   0 NF !  0 BODIES-U !  0 NCALLS !  0 NADDRS !  0 IMAGE-U !  0 NCELLS !  0 NSLOTS !
   0 NINDS !  0 RESERVES !  0 NRUNS ! ;

\ A body as a code entry holds it (local declarations, code, end), the lanes in and out
\ its emission row states, its frame variant and its origin; answers its handle,
\ the order it was added in. Every lane is an i64 at a word boundary: a function
\ in its lanes has type (i32 ctx, inputs x i64) -> (i32 status, outputs x i64),
\ one in the aligned frame (i32) -> (i32).
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

\ The data image, in place of any before it. An image whose end from WPROF's
\ DATA base passes memory32's 65536 pages is refused before a byte is reserved.
: DATA! ( ptr u8 n -- )
   {: a u:n :}
   u 0 < if E-SPAN-LENGTH throw then
   u IMAGE-MAX > if E-WLINK-MEMORY throw then
   u IMAGE-RESERVE
   u 0 > if  a u  0 IMAGE u SPAN:MAKE  SPAN:COPY  then
   u IMAGE-U ! ;

\ The image's cell at offset at holds the address of a kind and a target, which
\ LINK stores into the image before it writes the module.
: DATA-CELL+ ( n n n -- )
   {: at:n kind:n t:n :}
   NCELLS @ {: k:n :}
   k 1+ CELL-AT-RESERVE  k 1+ CELL-KIND-RESERVE  k 1+ CELL-TGT-RESERVE
   at k CELL-AT !  kind k CELL-KIND !  t k CELL-TGT !
   1 NCELLS +! ;

\ Function f in the table's next slot; answers the slot. Slots count from 1:
\ slot 0 is a null funcref, so call_indirect of xt 0 traps as native execute of 0
\ fails.
: TABLE+ ( n -- n )
   {: f:n :}
   NSLOTS @ {: k:n :}
   k 1+ SLOT-FN-RESERVE
   f k SLOT-FN !
   1 NSLOTS +!
   k FIRST-SLOT + ;

\ The module exports reserve, calling map-anon, function f: tier 0's routine in
\ the aligned frame or WKERNEL's of lanes; ( bytes -- address ior ) either way.
: RESERVE! ( n -- )
   RES-FN !  1 RESERVES ! ;

\ The module, run calling the function e.
: LINK ( n -- ptr u8 n )
   {: e:n :}
   e ROWS-CK
   NCELLS @ 0 ?do  i IMAGE-CELL!  loop
   FIND-RUNS
   0 NEXT !  [: NUMBER ;] IN-ORDER
   e FN-IDX @ ENTRY-IDX !
   TYPE-ALL
   0 AT !  1 DRY !  WRITE-MODULE
   AT @ {: u:n :}
   u OUT-RESERVE
   0 AT !  0 DRY !  WRITE-MODULE
   PATCH
   0 OUT u ;

;using
;package

WLINK:RESET
