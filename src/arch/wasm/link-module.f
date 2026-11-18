split-str
\ link-module.f - LINKER's Wasm writer: a stripped program's placed emissions and
\ the link's copy of its data, handed to WLINK and written as one module. It
\ rejects by name every kernel word the program reaches that no kernel-words.f
\ word provides and no routine of link-routines.f answers; a kernel word
\ resolves to its routine when the module has one. Every record enters its
\ emission at a function: a definition at function 0, a does> companion at its
\ clause's function (NSHADOW:TAKE-DOES).
require src/core/exit-status.f
require src/lib/error.f
require src/lib/fs-mutate.f
require src/lib/le.f
require src/habu/layout.f
require src/habu/address-cells.f
require src/compiler/native/emission.f
require src/compiler/native/shadow.f
require src/arch/wasm/link.f
require src/arch/wasm/link-kernel.f
require src/arch/wasm/link-routines.f
require src/habu/linker.f
require src/habu/link-band.f
require src/habu/link-copy.f

package LINKER
private

package WASM
private

\ A function handle is the count of functions added before it: emission e's
\ function k is `e FIRST @ k +`.
DYNAMIC-BUFFER FIRST n               \ placed emission e's first function's handle
DYNAMIC-BUFFER SLOTS n               \ function handle h's table slot, 0 for none
DYNAMIC-BUFFER DEAD n                \ 1 when captured function h's body is `unreachable`
DYNAMIC-BUFFER DEAD-BODY u8
variable NFUN                        \ the handles added
DYNAMIC-BUFFER REFUSED n             \ the host addresses no function answers, each once
variable REFUSED-N
DYNAMIC-BUFFER KERNEL-HOST n         \ kernel routine r's host address
DYNAMIC-BUFFER KERNEL-HANDLE n       \ kernel routine r's function handle
variable KERNEL-N                    \ the kernel routines this link added


\ The end of emission e's function k: the next function's offset, or e's size.
: FUNCTION-END ( n n -- n )
   {: e:n k:n :}
   k 1+ e NSHADOW:FUNCTIONS < if e k 1+ NSHADOW:FUNCTION-OFFSET@ exit then
   e NSHADOW:SIZE ;


\ Whether bytes [at, end) of emission e meet a live record's range.
: LIVE-SPAN? ( n n n -- bool )
   {: e:n at:n end:n :}
   e EM-FIRST @
   begin dup 0 >= while
      dup LIVE @ 0<> if
         dup NSHADOW:ENTRY@ end <  over RANGE-END at > and if drop true exit then
      then
      REC-NEXT @
   repeat
   drop false ;

\ No locals, then `unreachable`.
: DEAD-BODY$ ( -- str )
   3 DEAD-BODY-RESERVE
   0 0 DEAD-BODY c!  WCODE:OP-UNREACHABLE 1 DEAD-BODY c!  WCODE:OP-END 2 DEAD-BODY c!
   0 DEAD-BODY 3 ;

\ Placed emission e's functions, added in order as WLINK's captured functions.
\ One no live record's range meets keeps its index with the body `unreachable`
\ and no sites: a placed definer's own unit calls create, `,` and does-patch,
\ which a module has no function for.
: FUNCTIONS+ ( n -- )
   {: e:n :}
   NFUN @ e FIRST !
   e NSHADOW:FUNCTIONS 0 ?do
      e i NSHADOW:FUNCTION-OFFSET@ {: at:n :}
      e i FUNCTION-END {: end:n :}
      e at end LIVE-SPAN? {: live:bool :}
      live if e NSHADOW:BYTES at +  end at - else DEAD-BODY$ then
      0 0 1 WLINK-ORIGIN:CAPTURED WLINK:FUNCTION+ {: h:n :}
      h NFUN @ <> if E-COMP-STATE throw then
      h 1+ SLOTS-RESERVE  0 h SLOTS !
      h 1+ DEAD-RESERVE  live if 0 else 1 then h DEAD !
      1 NFUN +!
   loop ;


\ Emission e's function holding byte offset at.
\ Function 0 exists: every T0 emission opens at least one unit.
: HOLDER ( n n -- n )
   {: e:n at:n :}
   0
   e NSHADOW:FUNCTIONS 0 ?do
      e i NSHADOW:FUNCTION-OFFSET@ at <= if drop i then
   loop ;


: REFUSE ( n -- )
   {: a:n :}
   REFUSED-N @ 0 ?do i REFUSED @ a = if unloop exit then loop
   REFUSED-N @ 1+ REFUSED-RESERVE
   a REFUSED-N @ REFUSED !  1 REFUSED-N +! ;


\ Host address a's kernel routine handle, or -1 when the module has no routine
\ for the kernel word entering there; each routine is added once per link.
: KERNEL-FUNCTION ( n -- n )
   {: a:n :}
   KERNEL-N @ 0 ?do i KERNEL-HOST @ a = if i KERNEL-HANDLE @ unloop exit then loop
   a NAME-AT ROUTINE-BODY {: body:^ bu:n has:bool :}
   has 0= if -1 exit then
   body bu  0 0 1 WLINK-ORIGIN:KERNEL WLINK:FUNCTION+ {: h:n :}
   ROUTINE-SITES 0 ?do  h  i ROUTINE-SITE  WLINK:INDIRECT+  loop
   h NFUN @ <> if E-COMP-STATE throw then
   h 1+ SLOTS-RESERVE  0 h SLOTS !
   1 NFUN +!
   KERNEL-N @ 1+ KERNEL-HOST-RESERVE  KERNEL-N @ 1+ KERNEL-HANDLE-RESERVE
   a KERNEL-N @ KERNEL-HOST !  h KERNEL-N @ KERNEL-HANDLE !
   1 KERNEL-N +!
   h ;


\ Host address a's function handle, or -1 once refused: a record's entry, a
\ quotation's function, then a kernel routine.
: CALLEE ( n -- n )
   {: a:n :}
   a TARGET-REC {: r:n :}
   r 0 >= if
      r NSHADOW:EMISSION@ {: e2:n :}
      e2  r NSHADOW:ENTRY@  OWN-FUNCTION {: j:n :}
      e2 PLACED @ 0= if a REFUSE -1 exit then
      e2 FIRST @ j + exit
   then
   a NSHADOW:SOURCE-FUNCTION {: qe:n f:n :}
   qe 0 >= if
      qe PLACED @ 0<> if qe FIRST @ f + exit then
   then
   a KERNEL-FUNCTION {: k:n :}
   k 0 >= if k exit then
   a REFUSE -1 ;


\ Emission e's site value v: a function of e, else the host address CALLEE resolves.
: SITE-CALLEE ( n n -- n )
   {: e:n v:n :}
   e v OWN-FUNCTION {: k:n :}
   k 0 >= if e FIRST @ k + exit then
   v CALLEE ;


\ Handle h's table slot, given one the first time a code address names h.
: SLOT ( n -- n )
   {: h:n :}
   h SLOTS @ {: s:n :}
   s 0<> if s exit then
   h WLINK:TABLE+ {: new:n :}
   new h SLOTS !
   new ;


\ Emission e's call site c; a call field starts one byte past the site and is
\ counted from its function's first byte. The does> slot a clause patched
\ becomes a call of the clause.
: CALL-SITE+ ( n n -- )
   {: e:n c:n :}
   e c NSHADOW:CALL-SITE@ {: at:n :}
   e at HOLDER {: k:n :}
   e FIRST @ k + {: f:n :}
   f DEAD @ 0<> if exit then
   e  e c NSHADOW:CALL-TARGET@  SITE-CALLEE {: h:n :}
   h 0< if exit then
   at e k NSHADOW:FUNCTION-OFFSET@ - {: off:n :}
   e c NSHADOW:PATCH-CALL? if f off h WLINK:SLOT-CALL+ exit then
   f off 1+ h WLINK:CALL+ ;

: CALLS+ ( n -- )
   {: e:n :}
   e NSHADOW:CALL-SITES 0 ?do e i CALL-SITE+ loop ;


\ Emission e's address site c, its field as a call site's is.
: ADDR-SITE+ ( n n -- )
   {: e:n c:n :}
   e c NSHADOW:ADDR-SITE@ {: at:n :}
   e at HOLDER {: k:n :}
   e FIRST @ k + {: f:n :}
   f DEAD @ 0<> if exit then
   at 1+ e k NSHADOW:FUNCTION-OFFSET@ - {: field:n :}
   e c SITE-VALUE {: v:n has:bool :}
   has 0= if E-COMP-STATE throw then
   e c NSHADOW:ADDR-SITE-KIND@ {: kind:n :}
   kind NEMIT:ADDR-DATA = if f field NEMIT:ADDR-DATA  v DATA-OFFSET  WLINK:ADDRESS+ exit then
   kind NEMIT:ADDR-CODE <> if E-COMP-STATE throw then
   e v SITE-CALLEE {: h:n :}
   h 0< if exit then
   f field NEMIT:ADDR-CODE  h SLOT  WLINK:ADDRESS+ ;

: ADDRS+ ( n -- )
   {: e:n :}
   e NSHADOW:ADDR-SITES 0 ?do e i ADDR-SITE+ loop ;


\ A kept address row: a scratch cell holds 0 in the copy, a data cell its image
\ offset and an xt cell its function's table slot, which WLINK:LINK stores.
: ROW+ ( n -- )
   {: row:n :}
   row IMAGE-OFF {: off:n :}
   off CELL-FITS
   row SCRATCH-ROW? if 0 off COPY-AT LE:U64! exit then
   row CELL-VALUE {: v:n :}
   v 0= if exit then
   row DATA-ROW? if off NEMIT:ADDR-DATA  v DATA-OFFSET  WLINK:DATA-CELL+ exit then
   v CALLEE {: h:n :}
   h 0< if exit then
   off NEMIT:ADDR-CODE  h SLOT  WLINK:DATA-CELL+ ;


\ The record for host address a, through its help; the caller ends it.
: REFUSED. ( n -- )
   {: a:n :}
   a NAME-AT {: name:^ nameu:n :}
   s" E-WASM-NO-KERNEL-WORD" REJECTION:OPEN
   s" the program reaches " REJECTION:EMIT-STR
   nameu 0= if
      s" the kernel code at " REJECTION:EMIT-STR  a REJECTION:EMIT-N
   else
      s" `" REJECTION:EMIT-STR  name nameu REJECTION:EMIT-STR  s" `" REJECTION:EMIT-STR
   then
   s" , which a Wasm module does not provide" REJECTION:EMIT-STR
   REJECTION:HELP
   s" call a word src/arch/wasm/kernel-words.f provides instead, or build for a native target"
   REJECTION:EMIT-STR ;

: REFUSALS ( -- )
   REFUSED-N @ 0= if exit then
   REFUSED-N @ 0 ?do
      i REFUSED @ REFUSED.
      i 1+ REFUSED-N @ < if REJECTION:KEEP then
   loop
   EXIT-STATUS:REJECTED REJECTION:STOP ;


\ Every placed emission's functions, in emission order, before any site is read.
: EMISSIONS+ ( -- )
   WLINK:RESET  0 NFUN !  0 REFUSED-N !  0 KERNEL-N !
   NSHADOW:EMISSIONS FIRST-RESERVE
   NSHADOW:EMISSIONS 0 ?do
      -1 i FIRST !
      i PLACED @ 0<> if i FUNCTIONS+ then
   loop
   NSHADOW:EMISSIONS 0 ?do
      i PLACED @ 0<> if i CALLS+  i ADDRS+ then
   loop ;


\ The copy's kept rows; the index mapping address is zeroed, as link-data.f
\ HEADER-CELLS zeroes it.
: DATA+ ( -- )
   KEPT-COPY
   ['] ROW+ EACH-KEPT
   0 ADDRESS-CELLS:INDEX-CELL COPY-AT LE:U64! ;

public

\ Link the open space's stripped program into one Wasm module at the path, its reserve
\ calling map-anon; the header's DP, bounds and heap start stay as the copy holds them.
: WRITE-MODULE ( str -- )
   {: path:^ pathu:n :}
   false GOING
   APP-ENTRY:XT-CELL SPACE-AT @ {: entry:n :}
   ROUTINES REACH-DATA
   EMISSIONS+
   DATA+
   entry CALLEE {: h:n :}
   s" map-anon" NAMED-ENTRY KERNEL-FUNCTION WLINK:RESERVE!
   REFUSALS
   0 COPY-AT DATA-IMAGE-LEN WLINK:DATA!
   path pathu  h WLINK:LINK  FS:ATOMIC-WRITE-FILE ;

;package
;package
