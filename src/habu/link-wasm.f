\ link-wasm.f - WASMLINK, a captured window linked into one Wasm module: the
\ Wasm twin of src/habu/link-x64.f, handing WLINK (src/arch/wasm/link.f) every
\ function, call, address and data cell of the capture (docs/wasm-backend.md
\ 10.1, 17.1).
\
\ - THE FUNCTIONS. A routine is its emission in the capture's shadow
\   (src/habu/aot-decl.f AOT-SHADOW), written by WENC (src/arch/wasm/encode.f):
\   a header, then each function's body, which its header row places and gives
\   its lanes and frame variant. Each function joins the link with an adapter in
\   the table, the slot that is its xt (src/arch/wasm/dynamic.f). A record
\   enters its routine's first function. A `does>` definer never links: its
\   body calls `create`, which no kernel row answers.
\ - THE KERNEL. WKERNEL's rows (src/arch/wasm/kernel.f), and execute's and
\   catch's runtime functions, join every link. A row's call names a host entry,
\   which becomes the name of the engine word it enters before the map is asked.
\   A runtime function has an adapter's type and takes the stack as it stands,
\   so it is its own table slot, the xt a code literal or cell of execute or
\   catch holds.
\ - THE NAMES. A call, a code literal or a code cell naming a word of the
\   engine's own prefix reaches execute's or catch's runtime function, or what
\   WKERNEL:PROVIDER answers: a row, or a word of src/arch/wasm/kernel-words.f,
\   which the window must ship.
\ - THE SITES. A call's field gets its callee, a code literal's or a function
\   address's its target's slot, and a DATA literal's its window offset, which
\   WLINK writes as WPROF's data base plus it.
\ - THE DATA. The window's DATA is the module's data image at WPROF's data base:
\   each present cell of the capture's bitmap holds its value, and each address
\   cell the window holds is a data cell of the link, a DATA cell its target's
\   offset and a code cell its target's slot. A cell outside the window is the
\   engine's, which the module does not carry.
\
\ Refused by name, each before the module is written: a code record with no
\ routine (a variable, a created word, a defer, a tier-0 word), a name no kernel
\ row answers, a name WKERNEL's map or the entry gives that no shipped record
\ carries, and an entry that takes or leaves cells, since run calls it with ctx
\ alone. So is an output path that cannot be written. A refusal dies.

require lib/prelude.f
require lib/string.f
require lib/le.f
require lib/fs.f
require src/habu/layout.f
require src/habu/xref.f
require src/habu/aot-decl.f
require src/arch/wasm/leb.f
require src/arch/wasm/wstruct.f
require src/arch/wasm/encode.f
require src/arch/wasm/link.f
require src/arch/wasm/dynamic.f
require src/arch/wasm/kernel.f

package WASMLINK
using AOT-BUF

\ The rc of a capture this link cannot place, the capture's own refusals', and
\ of an output it cannot write, as the engine's drivers refuse theirs.
74 constant REFUSE-RC
$FFFFFFFF constant PKG-MARK          \ a compact row's wid word on a package row
\ An emission's header: the magic and the function count, then per function its
\ body offset, size, inputs, outputs, frame variant and a pad byte.
8 constant HEAD-BYTES
12 constant ROW-BYTES

DYNAMIC-BUFFER ROUTINE n             \ a shipped row -> the shadow row filing its routine
DYNAMIC-BUFFER FIRST n               \ a shadow row -> its emission's first function
DYNAMIC-BUFFER SLOTS n               \ a function -> its adapter's table slot
DYNAMIC-BUFFER IMG u8                \ the data image
variable K0                          \ WKERNEL's first row
variable EXEC-H
variable CATCH-H
variable FOUND                       \ the function a name reached
variable VAL-AT                      \ the next cell value in the capture's values

: REFUSE ( ptr u8 n -- ) REFUSE-RC die ;

\ ---- the capture's rows ---------------------------------------------------------
: CREC ( n -- ptr u8 )
   {: k:n :}
   AOT-REC-BUF@ AOT-REC-MAX 48 * + k AOT-CREC-ROW * + ;

: CREC@ ( n n -- n )
   {: k:n f:n :}
   k CREC f + LE:U32@ ;

: PKG? ( n -- bool ) 16 CREC@ PKG-MARK = ;

\ A pool entry: its length byte, then its name.
: POOL$ ( n -- ptr u8 n )
   {: off:n :}
   AOT-NAMES-BUF@ off + {: e:ptr :}
   e 1+ e c@ ;

: CREC-NAME$ ( n -- ptr u8 n ) 8 CREC@ POOL$ ;

: SH@ ( n n -- n )
   {: r:n f:n :}
   AOT-SHADOW:REC-BUF@ r AOT-SHADOW:REC-ROW * + f + LE:U32@ ;

: SH-REC ( n -- n ) 0 SH@ ;
: SH-AT ( n -- n ) 4 SH@ ;

: EMISSION ( n -- ptr u8 )
   {: r:n :}
   AOT-SHADOW:CODE-BUF@ r SH-AT + ;

: SITE@ ( n n -- n )
   {: s:n f:n :}
   AOT-SHADOW:SITE-BUF@ s AOT-SHADOW:SITE-ROW * + f + LE:U32@ ;

: XT@ ( n n -- n )
   {: x:n f:n :}
   AOT-SHADOW:XT-BUF@ x AOT-SHADOW:XT-ROW * + f + LE:U32@ ;

: XTOFF@ ( n n -- n )
   {: c:n f:n :}
   AOT-WINDOW:XTOFF-BUF@ c AOT-WINDOW:XTOFF-ROW * + f + LE:U32@ ;

\ ---- an emission's functions -----------------------------------------------------
: FUNS ( ptr u8 -- n ) 4 + LE:U32@ ;

: HEAD ( ptr u8 n -- ptr u8 )
   {: p:ptr k:n :}
   p HEAD-BYTES + k ROW-BYTES * + ;

: BODY-AT ( ptr u8 n -- n ) HEAD LE:U32@ ;

\ The function of emission p whose body starts at offset v, the bodies ascending.
: FUN-AT ( ptr u8 n -- n )
   {: p:ptr v:n :}
   0  p FUNS 0 ?do  p i BODY-AT v < if 1+ then  loop ;

\ The function of emission p whose body holds offset v.
: HOLDER ( ptr u8 n -- n )
   {: p:ptr v:n :}
   -1  p FUNS 0 ?do  p i BODY-AT v <= if 1+ then  loop ;

: FUNCTION+ ( ptr u8 n WLINK:origin -- n )
   {: p:ptr k:n o:WLINK:origin :}
   p k HEAD {: h:ptr :}
   p h LE:U32@ +  h 4 + LE:U32@  h 8 + c@  h 9 + c@  h 10 + c@  o WLINK:FUNCTION+ ;

: ADAPTER+ ( ptr u8 n n -- )
   {: p:ptr k:n f:n :}
   p k HEAD {: h:ptr :}
   f 1+ SLOTS-RESERVE
   f  h 8 + c@  h 9 + c@  WDYN:ADAPTER+  f SLOTS ! ;

\ Emission p's functions, numbered from its first, each with its adapter;
\ answers the first.
: EMISSION+ ( ptr u8 WLINK:origin -- n )
   {: p:ptr o:WLINK:origin :}
   p 0 o FUNCTION+ {: f0:n :}
   p FUNS 1 ?do  p i o FUNCTION+ drop  loop
   p FUNS 0 ?do  p i f0 i + ADAPTER+  loop
   f0 ;

\ ---- the routines ---------------------------------------------------------------
: NO-ROUTINE ( n -- )
   {: k:n :}
   s" wasmlink: window record " type k CREC-NAME$ type s"  has no Wasm routine" type cr
   s" wasmlink: a code record the capture's shadow carries no routine for" REFUSE ;

\ The shadow row filing each shipped record's routine; a package row has none.
: ROUTINES ( -- )
   AOT-REC-N @ ROUTINE-RESERVE
   AOT-REC-N @ 0 ?do  -1 i ROUTINE !  loop
   AOT-SHADOW:REC-N @ 0 ?do  i  i SH-REC ROUTINE !  loop
   AOT-REC-N @ 0 ?do
      i PKG? 0= if  i ROUTINE @ 0 < if i NO-ROUTINE then  then
   loop ;

: ROUTINES+ ( -- )
   AOT-SHADOW:REC-N @ FIRST-RESERVE
   AOT-SHADOW:REC-N @ 0 ?do
      i EMISSION WLINK-ORIGIN:CAPTURED EMISSION+ i FIRST !
   loop ;

\ The function shipped record k enters.
: ENTERS ( n -- n ) ROUTINE @ FIRST @ ;

\ ---- the names ------------------------------------------------------------------
\ The public wid of the shipped package named, or -1.
: PKG-WID ( ptr u8 n -- n )
   {: a:ptr u:n :}
   AOT-REC-N @ 0 ?do
      i PKG? if  i CREC-NAME$ a u STR=CI if i 0 CREC@ unloop exit then  then
   loop
   -1 ;

\ The shipped code record named in wordlist w, or -1.
: IN-WID ( ptr u8 n n -- n )
   {: a:ptr u:n w:n :}
   AOT-REC-N @ 0 ?do
      i PKG? 0= if
         i 16 CREC@ w = if  i CREC-NAME$ a u STR=CI if i unloop exit then  then
      then
   loop
   -1 ;

: UNSHIPPED ( ptr u8 n -- )
   {: a:ptr u:n :}
   s" wasmlink: " type a u type s"  names no record the capture ships" type cr
   s" wasmlink: a name no shipped record answers" REFUSE ;

\ The shipped code record a spelling names, PKG:NAME or a global's NAME.
: SHIPPED ( ptr u8 n -- n )
   {: a:ptr u:n :}
   a u [char] : INDEX-OF MATCH option
      some OF IDX>N {: c:n :}
         a c + 1+  u c - 1-  a c PKG-WID IN-WID ENDOF
      none OF a u 0 IN-WID ENDOF
   ;MATCH {: k:n :}
   k 0 < if a u UNSHIPPED then
   k ;

\ The function an engine-prefix name reaches: execute's or catch's runtime
\ function, or the row or the shipped word WKERNEL's map answers.
: REACH ( ptr u8 n -- n )
   {: a:ptr u:n :}
   a u s" execute" STR=CI if EXEC-H @ exit then
   a u s" catch" STR=CI if CATCH-H @ exit then
   a u WKERNEL:PROVIDER MATCH WKERNEL:provider
      row OF K0 @ + ENDOF
      word OF SHIPPED ENTERS ENDOF
   ;MATCH ;

: ASK ( ptr u8 n -- ptr u8 n )
   2dup REACH FOUND ! ;

: UNANSWERED ( ptr u8 n -- )
   {: a:ptr u:n :}
   s" wasmlink: " type a u type s"  is a name no kernel row answers" type cr
   s" wasmlink: a name WKERNEL's map does not answer" REFUSE ;

\ REACH's function; the map's refusal names the name.
: NAMED ( ptr u8 n -- n )
   {: a:ptr u:n :}
   a u [: ASK ;] catch {: code:n :}
   2drop
   code 0= if FOUND @ exit then
   code E-WLINK-UNRESOLVED = if a u UNANSWERED then
   code throw ;

\ The name of the dictionary's first record entering host address v.
: ENGINE-NAME ( n -- ptr u8 n )
   {: v:n :}
   ndict@ 0 ?do
      i XREF-REC {: rec:ptr :}
      rec XREF-START v = if rec XREF-NAME$ unloop exit then
   loop
   s" " ;

\ ---- the kernel -----------------------------------------------------------------
\ Runtime function h in the table's next slot; answers h.
: RUNTIME+ ( n -- n )
   {: h:n :}
   h 1+ SLOTS-RESERVE
   h WLINK:TABLE+ h SLOTS !
   h ;

: KERNEL-CALL+ ( n -- )
   {: c:n :}
   WENC:BYTES {: p:ptr :}
   c WENC:CALL-SITE@ {: off:n :}
   p off HOLDER {: k:n :}
   K0 @ k +  off p k BODY-AT -  c WENC:CALL-TARGET@ ENGINE-NAME NAMED  WLINK:CALL+ ;

: KERNEL+ ( -- )
   WDYN:EXECUTE+ RUNTIME+ EXEC-H !
   WDYN:CATCH+ RUNTIME+ CATCH-H !
   WKERNEL:ENCODE
   WENC:BYTES WLINK-ORIGIN:KERNEL EMISSION+ K0 !
   WENC:CALL-SITES 0 ?do  i KERNEL-CALL+  loop ;

\ ---- the sites ------------------------------------------------------------------
\ The shadow row whose emission holds shadow code byte v, the rows ascending.
: ROW-AT ( n -- n )
   {: v:n :}
   -1  AOT-SHADOW:REC-N @ 0 ?do  i SH-AT v <= if 1+ then  loop ;

\ The function holding site s's field, and the field's offset in its body.
: PLACE ( n -- n n )
   0 SITE@ {: v:n :}
   v ROW-AT {: r:n :}
   r EMISSION {: p:ptr :}
   v r SH-AT - {: off:n :}
   p off HOLDER {: k:n :}
   r FIRST @ k +  off p k BODY-AT - ;

\ What the capture left in site s's padded ten-byte field.
: CAPTURED ( n -- n )
   0 SITE@ {: v:n :}
   AOT-SHADOW:CODE-BUF@ AOT-SHADOW:CODE-LEN @ v WLEB:S64-PAD@ ;

\ The function a call or a code literal names.
: TARGET ( n -- n )
   8 SITE@ {: t:n :}
   t SITE-TARGET-MASK and {: v:n :}
   t SITE-NAME-TAG and 0<> if v POOL$ NAMED exit then
   v ENTERS ;

\ A function address names a function of its own emission by its body offset.
: FUN-OF ( n -- n )
   {: s:n :}
   s 0 SITE@ ROW-AT {: r:n :}
   r EMISSION s CAPTURED FUN-AT  r FIRST @ + ;

: SITE+ ( n -- )
   {: s:n :}
   s PLACE {: f:n at:n :}
   s 4 SITE@ {: kind:n :}
   kind AOT-SHADOW:CALL = if  f at s TARGET WLINK:CALL+  exit  then
   kind AOT-SHADOW:DATA = if
      f at WSTRUCT:ADDR-DATA  s CAPTURED AOT-DATA-D0 @ -  WLINK:ADDRESS+  exit
   then
   kind AOT-SHADOW:FUN = if s FUN-OF else s TARGET then {: t:n :}
   f at WSTRUCT:ADDR-CODE t SLOTS @ WLINK:ADDRESS+ ;

\ ---- the data ---------------------------------------------------------------------
: PRESENT? ( n -- bool )
   {: c:n :}
   AOT-WINDOW:BM-BUF@ c 3 rshift + c@  c 7 and rshift  1 and 0<> ;

: IMAGE ( -- )
   AOT-DATA-SIZE @ {: size:n :}
   size IMG-RESERVE
   size 0 ?do  0 i IMG c!  loop
   0 VAL-AT !
   AOT-WINDOW:BM-LEN @ 8 * 0 ?do
      i PRESENT? if
         AOT-WINDOW:VAL-BUF@ VAL-AT @ +  AOT-WINDOW:VAL-LEN @ VAL-AT @ -
         AOT-WINDOW:CELL-V@ {: v:n w:n :}
         v  i AOT-WINDOW:CELL-BYTES * IMG  LE:U64!
         w VAL-AT +!
      then
   loop
   0 IMG size WLINK:DATA! ;

\ The shipped record address-cell row c's code cell enters.
: XT-REC ( n -- n )
   {: c:n :}
   AOT-SHADOW:XT-N @ 0 ?do  i 0 XT@ c = if i 4 XT@ unloop exit then  loop
   -1 ;

: CELL+ ( n -- )
   {: c:n :}
   c 0 XTOFF@ {: loc:n :}
   c 4 XTOFF@ {: meta:n :}
   meta AOT-WINDOW:XTOFF-VALUE-MASK and {: v:n :}
   loc AOT-WINDOW:XTOFF-WINDOW-TAG and 0=  v 0= or if exit then
   loc AOT-WINDOW:XTOFF-LOC-MASK and {: at:n :}
   meta AOT-WINDOW:XTOFF-KIND-MASK and {: kind:n :}
   kind AOT-WINDOW:XTOFF-DATA-TAG = if  at WSTRUCT:ADDR-DATA v 1- WLINK:DATA-CELL+  exit  then
   kind AOT-WINDOW:XTOFF-NAME-TAG = if v 1- POOL$ NAMED else c XT-REC ENTERS then {: f:n :}
   at WSTRUCT:ADDR-CODE f SLOTS @ WLINK:DATA-CELL+ ;

\ ---- the module ---------------------------------------------------------------
: CELLS-ENTRY ( ptr u8 n -- )
   {: a:ptr u:n :}
   s" wasmlink: entry " type a u type s"  takes or leaves cells" type cr
   s" wasmlink: an entry that takes or leaves cells, which run cannot call" REFUSE ;

\ The function run calls for the shipped record named, which takes and leaves
\ no cell: its emission's first header row says how many.
: ENTRY ( ptr u8 n -- n )
   {: a:ptr u:n :}
   a u SHIPPED {: k:n :}
   k ROUTINE @ EMISSION 0 HEAD {: h:ptr :}
   h 8 + c@  h 9 + c@  or 0<> if a u CELLS-ENTRY then
   k ENTERS ;

: UNWRITABLE ( ptr u8 n -- )
   {: a:ptr u:n :}
   s" wasmlink: " type a u type s"  cannot be written" type cr
   s" wasmlink: an output path that cannot be written" REFUSE ;

\ The module written to path, the stack as it found it.
: PUT ( ptr u8 n ptr u8 n -- ptr u8 n ptr u8 n )
   {: path:ptr pu:n m:ptr mu:n :}
   path pu m mu WRITE-ALL
   path pu m mu ;

public

\ The capture's window linked as one Wasm module whose run calls the word entry
\ names, PKG:NAME or a global's NAME, written to the file at path.
: LINK ( ptr u8 n ptr u8 n -- )
   {: e:ptr eu:n path:ptr pu:n :}
   ROUTINES
   WLINK:RESET
   ROUTINES+
   e eu ENTRY {: f:n :}
   KERNEL+
   AOT-SHADOW:SITE-N @ 0 ?do  i SITE+  loop
   IMAGE
   AOT-WINDOW:XTOFF-N @ 0 ?do  i CELL+  loop
   path pu  f WLINK:LINK  [: PUT ;] catch {: code:n :}
   2drop 2drop
   code 0<> if path pu UNWRITABLE then ;

;using
;package
