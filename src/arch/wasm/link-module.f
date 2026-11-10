\ link-module.f - LINKER's Wasm writer: a stripped program's placed emissions and
\ the link's copy of its data, handed to WLINK and written as one module. It
\ refuses by name a defer, a does> clause, a routine entered inside a function
\ and every kernel word the program reaches that no kernel-words.f word
\ provides and no routine of link-routines.f answers; a kernel word resolves to
\ its routine when the module has one.
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


\ Placed emission e's functions, added in order as WLINK's captured functions.
: FUNCTIONS+ ( n -- )
   {: e:n :}
   NFUN @ e FIRST !
   e NSHADOW:FUNCTIONS 0 ?do
      e i NSHADOW:FUNCTION-OFFSET@ {: at:n :}
      e i FUNCTION-END {: end:n :}
      e NSHADOW:BYTES at +  end at -  0 0 1 WLINK-ORIGIN:CAPTURED WLINK:FUNCTION+ {: h:n :}
      h NFUN @ <> if E-COMP-STATE throw then
      h 1+ SLOTS-RESERVE  0 h SLOTS !
      1 NFUN +!
   loop ;


\ Emission e's function holding byte offset at.
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


: INSIDE-FUNCTION ( n -- )
   s" link: " SAY NSHADOW:RECORD@ XREF-REC XREF-NAME$ SAY
   s"  enters its routine inside a function" SAY SAY-CR
   s" link: a routine is entered inside a function" 74 die ;


\ Host address a's kernel routine handle, or -1 when the module has no routine
\ for the kernel word entering there; each routine is added once per link.
: KERNEL-FUNCTION ( n -- n )
   {: a:n :}
   KERNEL-N @ 0 ?do i KERNEL-HOST @ a = if i KERNEL-HANDLE @ unloop exit then loop
   a NAME-AT ROUTINE-BODY {: body:ptr bu:n has:bool :}
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


\ Host address a's function handle, or -1 once a is refused; resolved in
\ LINK-ADDRESS's order, a record's entry before a quotation's function, then a
\ kernel word's routine when the module has one.
: CALLEE ( n -- n )
   {: a:n :}
   a TARGET-REC {: r:n :}
   r 0 >= if
      r NSHADOW:EMISSION@ {: e2:n :}
      e2  r NSHADOW:ENTRY@  OWN-FUNCTION {: j:n :}
      j 0< if r INSIDE-FUNCTION then
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


: CLAUSE-REFUSED ( n -- )
   s" link: " SAY ROUTINE. s"  has a does> clause, which a Wasm module cannot hold" SAY SAY-CR
   s" link: a Wasm module cannot hold a does> clause" 74 die ;


\ Emission e's call site c; a call field starts one byte past the site and is
\ counted from its function's first byte.
: CALL-SITE+ ( n n -- )
   {: e:n c:n :}
   e c NSHADOW:CALL-SITE@ {: at:n :}
   at e NSHADOW:PATCH-SITE@ = if e CLAUSE-REFUSED then
   e at HOLDER {: k:n :}
   e  e c NSHADOW:CALL-TARGET@  SITE-CALLEE {: h:n :}
   h 0< if exit then
   e FIRST @ k +   at 1+ e k NSHADOW:FUNCTION-OFFSET@ -   h   WLINK:CALL+ ;

: CALLS+ ( n -- )
   {: e:n :}
   e NSHADOW:CALL-SITES 0 ?do e i CALL-SITE+ loop ;


\ Emission e's address site c, its field as a call site's is.
: ADDR-SITE+ ( n n -- )
   {: e:n c:n :}
   e c NSHADOW:ADDR-SITE@ {: at:n :}
   e at HOLDER {: k:n :}
   e FIRST @ k + {: f:n :}
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
   row ROW-OFF {: off:n :}
   off CELL + DATA-IMAGE-LEN > if
      s" link: a declared cell past the build's data, at offset " SAY off SAY-INT SAY-CR
      s" link: a declared cell lies past the build's data" 74 die
   then
   row SCRATCH-ROW? if 0 off COPY-AT LE:U64! exit then
   row CELL-VALUE {: v:n :}
   v 0= if exit then
   row DATA-ROW? if off NEMIT:ADDR-DATA  v DATA-OFFSET  WLINK:DATA-CELL+ exit then
   v CALLEE {: h:n :}
   h 0< if exit then
   off NEMIT:ADDR-CODE  h SLOT  WLINK:DATA-CELL+ ;


: REFUSED. ( n -- )
   {: a:n :}
   s" link: a Wasm module has no " SAY
   a NAME-AT {: name:ptr nameu:n :}
   nameu 0= if s" code at " SAY a SAY-INT else name nameu SAY then
   SAY-CR ;

: REFUSALS ( -- )
   REFUSED-N @ 0= if exit then
   REFUSED-N @ 0 ?do i REFUSED @ REFUSED. loop
   s" link: the program reaches kernel words a Wasm module does not provide" 74 die ;


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
   FILL-COPY CUT-DEAD KEEP-STRINGS KEEP-LIST SORT-ROWS
   ['] ROW+ EACH-KEPT
   0 ADDRESS-CELLS:INDEX-CELL COPY-AT LE:U64! ;

public

\ Link the open space's stripped program into one Wasm module at the path, its reserve
\ calling map-anon; the header's DP, bounds and heap start stay as the copy holds them.
: WRITE-MODULE ( ptr u8 n -- )
   {: path:ptr pathu:n :}
   false GOING
   KERNEL? @ if s" link: a kernel build writes no program" 74 die then
   ENGINE? @ if s" link: a Wasm target builds no engine" 74 die then
   0 CHECK:PLACE-FACTS
   APP-ENTRY:XT-CELL SPACE-AT @ {: entry:n :}
   entry 0= if s" link: the build stored no entry for the program" 74 die then
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
