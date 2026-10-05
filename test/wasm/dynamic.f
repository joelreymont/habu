\ dynamic.f - WDYN, src/arch/wasm/dynamic.f, run: definitions compiled through
\ an open Wasm shadow (src/arch/wasm/backend.f), their functions linked by
\ WLINK with an adapter in the table for each, beside the runtime execute and
\ catch, then validated by wasm-tools and run by bun (test/wasm/harness.f).
\ Both tools must be on PATH, so this is no row of the ordinary gate:
\ test/wasm/device.f runs it, as does `bin/hb --load test/wasm/dynamic.f` from
\ the tree's root.
\
\ W06's dynamic half: a 17-input word, a function in the aligned frame,
\ answers through execute of its code literal what its direct call answers.
\ Quotations in their lanes, one taking a cell and answering one and one
\ answering two, reached through execute, take their input off the stack and
\ leave their outputs there. W07: catch of a quotation that throws MIN-N + 1,
\ which a double would round, answers the code whole over the cell beneath the
\ call; catch of a quotation that returns answers zero while ctx.throw-code
\ still holds an earlier code; a trap inside a quotation passes through catch;
\ and execute of an xt whose slot takes more cells than the stack holds traps
\ before its function runs. Execute and catch of xt 0, which names no adapter,
\ trap, and so does execute of an xt whose upper 32 bits are set though its
\ low half names a slot. A package's own execute and catch are ordinary words,
\ and a call to either answers what it answers natively. An entry reports
\ through its last throw's code, and each code is the one the same definition
\ throws natively.
\
\ THE LINK. Each record's emission gives its functions; a call naming one of
\ them by its body offset, or another record by its entry, calls that function,
\ and a call naming the engine's execute, catch or throw, the global
\ wordlist's, calls the runtime function; an address naming a function either
\ way is the slot of that function's adapter. throw is written here, since the
\ engine's primitives have no Wasm body yet: it stores the cell on top in
\ ctx.throw-code and answers status 1.

require lib/test.f
require lib/le.f
require lib/fs.f
require lib/fs-mutate.f
require src/habu/xref.f
require src/compiler/native/shadow.f
require src/arch/wasm/leb.f
require src/arch/wasm/wstruct.f
require src/arch/wasm/profile.f
require src/arch/wasm/link.f
require src/arch/wasm/backend.f
require src/arch/wasm/dynamic.f
require test/wasm/harness.f
require test/wasm/w03.f

package WASM-DYNAMIC
private
using WASM-W03

\ A number as a cell's address, for the load that traps.
CAST: >CELL ( n -- ptr n )
\ A three-input word's xt as an xt of none, for the depth shortfall.
CAST: >NONE ( [ n n n -- ] -- [ -- ] )
\ A number as an xt of none, for the xts that name no adapter.
CAST: >VOID ( n -- [ -- ] )

\ The engine's catch, which the package's own below hides.
: NATIVE-CATCH ( [ -- ] -- n ) catch ;

variable R0                          \ the map's first row of the words below
variable REC-DIRECT
variable REC-EXECUTE
variable REC-LANES
variable REC-CAUGHT
variable REC-RETURNED
variable REC-TRAPPED
variable REC-SHORT
variable REC-ZERO
variable REC-ZERO-CAUGHT
variable REC-HIGH
variable REC-OWN-EXECUTE
variable REC-OWN-CATCH

: OPEN-WASM ( -- ) WBACK:BINDING NSHADOW:OPEN ;

WBACK:INSTALL
OPEN-WASM
NSHADOW:RECORDS R0 !
1 set-tier
\ The first function linked, so its adapter holds slot 1: an xt that reaches
\ it throws 37, where one that reached DROP3's would trap on the empty stack.
: MARK ( -- ) 37 throw ;
: DROP3 ( n n n -- ) drop drop drop ;
: W17 ( n n n n n n n n n n n n n n n n n -- n )
   3 * + 3 * + 3 * + 3 * + 3 * + 3 * + 3 * + 3 * +
   3 * + 3 * + 3 * + 3 * + 3 * + 3 * + 3 * + 3 * + ;
ndict@ REC-DIRECT !
: DIRECT ( -- ) 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 W17 throw ;
ndict@ REC-EXECUTE !
: VIA-EXECUTE ( -- ) 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 ['] W17 execute throw ;
ndict@ REC-LANES !
: VIA-LANES ( -- ) 5 [: 1 + ;] execute [: 1 2 ;] execute + + throw ;
ndict@ REC-CAUGHT !
: CAUGHT ( -- ) 7 [: $8000000000000001 throw ;] catch + throw ;
ndict@ REC-RETURNED !
: RETURNED ( -- ) [: 9 throw ;] catch [: ;] catch + throw ;
ndict@ REC-TRAPPED !
: TRAPPED ( -- ) [: 0 >CELL @ drop ;] catch throw ;
ndict@ REC-SHORT !
: SHORT ( -- ) ['] DROP3 >NONE execute ;
ndict@ REC-ZERO !
: ZERO ( -- ) 0 >VOID execute ;
ndict@ REC-ZERO-CAUGHT !
: ZERO-CAUGHT ( -- ) 0 >VOID catch throw ;
ndict@ REC-HIGH !
: HIGH ( -- ) $100000001 >VOID execute ;
: execute ( n -- n ) 1 + ;
ndict@ REC-OWN-EXECUTE !
: OWN-EXECUTE ( -- ) 4 execute throw ;
: catch ( n -- n ) 1 + ;
ndict@ REC-OWN-CATCH !
: OWN-CATCH ( -- ) 4 catch throw ;
0 set-tier

\ ---- the shadow's map ----------------------------------------------------------
\ The emission header: the magic and the function count, then per function its
\ body offset, size, inputs, outputs and frame variant (src/arch/wasm/encode.f).
8 constant HEAD-BYTES
12 constant ROW-BYTES

\ The row a record was filed under, or -1.
: ROW-OF ( n -- n )
   {: idx:n :}
   -1
   NSHADOW:RECORDS 0 ?do
      i NSHADOW:RECORD@ idx = if drop i leave then
   loop ;

: EM ( n -- n )  NSHADOW:EMISSION@ ;

\ The header field at off of emission e's function k: a size, or a byte.
: HEAD-AT ( n n n -- ptr u8 )
   {: e:n k:n off:n :}
   e NSHADOW:BYTES  HEAD-BYTES k ROW-BYTES * + off + + ;

: HEAD@ ( n n n -- n )  HEAD-AT LE:U32@ ;
: HEAD-C@ ( n n n -- n )  HEAD-AT c@ ;

\ The function of emission e whose body holds offset at.
: HOLDER ( n n -- n )
   {: e:n at:n :}
   -1
   e NSHADOW:FUNCTIONS 0 ?do
      e i 0 HEAD@ {: off:n :}
      at off >=  at off e i 4 HEAD@ + <  and if drop i leave then
   loop ;

\ The function of emission e whose body starts at offset v, or -1.
: AT-OFFSET ( n n -- n )
   {: e:n v:n :}
   -1
   e NSHADOW:FUNCTIONS 0 ?do
      e i NSHADOW:FUNCTION-OFFSET@ v = if drop i leave then
   loop ;

\ The row of the record whose host entry is v, or -1.
: ROW-AT ( n -- n )
   {: v:n :}
   -1
   NSHADOW:RECORDS R0 @ ?do
      i NSHADOW:RECORD@ XREF-REC XREF-START v = if drop i leave then
   loop ;

\ ---- the link -------------------------------------------------------------------
variable NFUN                        \ the captured functions linked
DYNAMIC-BUFFER FUN-H n               \ each one's handle
DYNAMIC-BUFFER FUN-SLOT n            \ and its adapter's slot
DYNAMIC-BUFFER ROW-FUN n             \ each row's first, from R0 on
variable EXEC-H
variable CATCH-H
variable THROW-H

\ Function k of row r, as numbered here.
: FUN ( n n -- n )
   {: r:n k:n :}
   r R0 @ - ROW-FUN @ k + ;

\ The function a target names from row r: one of r's emission at that body
\ offset, else function zero of the record at that entry.
: NAMED ( n n -- n )
   {: r:n v:n :}
   r EM v AT-OFFSET {: k:n :}
   k 0 >= if  r k FUN exit  then
   v ROW-AT {: s:n :}
   s 0 < if  s" wasm dynamic: a target no linked record holds" 1 die  then
   s 0 FUN ;

: CALLEE ( n n -- n )
   {: r:n t:n :}
   t s" execute" 0 search-wl = if  EXEC-H @ exit  then
   t s" catch" 0 search-wl = if  CATCH-H @ exit  then
   t s" throw" 0 search-wl = if  THROW-H @ exit  then
   r t NAMED FUN-H @ ;

\ Row r's functions, each with its adapter.
: FUNCTIONS+ ( n -- )
   {: r:n :}
   r EM {: e:n :}
   NFUN @  r R0 @ - ROW-FUN !
   e NSHADOW:FUNCTIONS 0 ?do
      e i 8 HEAD-C@ {: in:n :}
      e i 9 HEAD-C@ {: out:n :}
      e NSHADOW:BYTES e i 0 HEAD@ +  e i 4 HEAD@  in out  e i 10 HEAD-C@
      WLINK-ORIGIN:CAPTURED WLINK:FUNCTION+ {: h:n :}
      NFUN @ {: k:n :}
      k 1+ FUN-H-RESERVE  k 1+ FUN-SLOT-RESERVE
      h k FUN-H !
      h in out WDYN:ADAPTER+ k FUN-SLOT !
      1 NFUN +!
   loop ;

\ Row r's call and address sites, at their fields' offsets in their bodies.
: SITES+ ( n -- )
   {: r:n :}
   r EM {: e:n :}
   e NSHADOW:CALL-SITES 0 ?do
      e i NSHADOW:CALL-SITE@ {: at:n :}
      e at HOLDER {: k:n :}
      r k FUN FUN-H @  at e k 0 HEAD@ -  r e i NSHADOW:CALL-TARGET@ CALLEE  WLINK:CALL+
   loop
   e NSHADOW:ADDR-SITES 0 ?do
      e i NSHADOW:ADDR-SITE-KIND@ WSTRUCT:ADDR-CODE <> if
         s" wasm dynamic: a DATA address, which this link lays out no image for" 1 die
      then
      e i NSHADOW:ADDR-SITE@ {: at:n :}
      e NSHADOW:BYTES e NSHADOW:SIZE at WLEB:S64-PAD@ {: v:n :}
      e at HOLDER {: k:n :}
      r k FUN FUN-H @  at e k 0 HEAD@ -  WSTRUCT:ADDR-CODE  r v NAMED FUN-SLOT @
      WLINK:ADDRESS+
   loop ;

\ throw: the cell on top into ctx.throw-code, then status 1.
: THROW-BODY ( -- )
   0 BODY-U !
   0 B,                                      \ no local
   $20 B, 0 B,                               \ ctx, where the code goes
   $20 B, 0 B,  $28 B, 2 B, WPROF:CTX-STACK-TOP B,
   $41 B, 8 B,  $6B B,                       \ the top less a cell
   $29 B, 3 B, 0 B,                          \ the cell there
   $37 B, 3 B, WPROF:CTX-THROW-CODE B,
   $41 B, 1 B,
   $0B B, ;

: LINK-ALL ( -- )
   WLINK:RESET
   0 NFUN !
   WDYN:EXECUTE+ EXEC-H !
   WDYN:CATCH+ CATCH-H !
   THROW-BODY  BODY$ 0 0 0 WLINK-ORIGIN:KERNEL WLINK:FUNCTION+ THROW-H !
   NSHADOW:RECORDS R0 @ - ROW-FUN-RESERVE
   NSHADOW:RECORDS R0 @ ?do  i FUNCTIONS+  loop
   NSHADOW:RECORDS R0 @ ?do  i SITES+  loop ;

\ ---- the runs ---------------------------------------------------------------------
FS-PATH-CAP BUFFER: DIR
variable DIR-U
FS-PATH-CAP BUFFER: PATH
variable PATH-U

: SETUP ( -- )
   s" wasm-dynamic" HB-TMP-MKDIR {: a:ptr u:n :}
   a DIR u BYTE-COPY  u DIR-U !
   s" wasm dynamic: modules in " type  DIR u type cr ;

\ The module whose entry is the word of record idx, written as name: whether
\ it validates, and the status its run exits with.
: RUN-REC ( n ptr u8 n -- bool n )
   {: idx:n name:ptr nu:n :}
   DIR DIR-U @ name nu PATH JOIN-PATH PATH-U !
   PATH PATH-U @  idx ROW-OF 0 FUN FUN-H @ WLINK:LINK  WRITE-ALL
   PATH PATH-U @ WASM-HARNESS:VALID?
   PATH PATH-U @ WASM-HARNESS:RUN ;

\ The module of record idx validates, exits 1, and its throw code is the one
\ the word throws natively.
: THROWS-AS-NATIVE ( n ptr u8 n [ -- ] -- )
   {: idx:n name:ptr nu:n word :}
   word NATIVE-CATCH {: want:n :}
   idx name nu RUN-REC 1 T= TTRUE
   WASM-HARNESS:THROW-CODE want T= ;

: TRAPS ( n ptr u8 n -- )
   RUN-REC 2 T= TTRUE ;

: W06-CASE ( -- )
   s" W06: a 17-input word called directly, in the aligned frame, throws its answer as natively" T-LABEL
   REC-DIRECT @ s" direct.wasm" [: DIRECT ;] THROWS-AS-NATIVE
   s" W06: the same word through execute of its code literal's slot answers the same" T-LABEL
   REC-EXECUTE @ s" execute.wasm" [: VIA-EXECUTE ;] THROWS-AS-NATIVE
   s" execute of a quotation in its lanes takes its input off the stack and leaves its outputs there" T-LABEL
   REC-LANES @ s" lanes.wasm" [: VIA-LANES ;] THROWS-AS-NATIVE ;

: W07-CASE ( -- )
   s" W07: catch of a quotation throwing MIN-N + 1 answers the code whole over the cell beneath" T-LABEL
   REC-CAUGHT @ s" caught.wasm" [: CAUGHT ;] THROWS-AS-NATIVE
   s" catch of a quotation that returns answers zero, though ctx holds an earlier code" T-LABEL
   REC-RETURNED @ s" returned.wasm" [: RETURNED ;] THROWS-AS-NATIVE
   s" W07: a trap inside a quotation passes through catch" T-LABEL
   REC-TRAPPED @ s" trapped.wasm" TRAPS
   s" execute of an xt whose slot takes three cells, with none on the stack, traps" T-LABEL
   REC-SHORT @ s" short.wasm" TRAPS ;

: XT-CASE ( -- )
   s" execute of xt 0, which names no adapter, traps" T-LABEL
   REC-ZERO @ s" zero.wasm" TRAPS
   s" catch of xt 0 traps" T-LABEL
   REC-ZERO-CAUGHT @ s" zero-caught.wasm" TRAPS
   s" execute of an xt whose upper 32 bits are set traps, though its low half is MARK's slot" T-LABEL
   REC-HIGH @ s" high.wasm" TRAPS ;

: NAME-CASE ( -- )
   s" a package's own execute is an ordinary call and answers as natively" T-LABEL
   REC-OWN-EXECUTE @ s" own-execute.wasm" [: OWN-EXECUTE ;] THROWS-AS-NATIVE
   s" a package's own catch is an ordinary call and answers as natively" T-LABEL
   REC-OWN-CATCH @ s" own-catch.wasm" [: OWN-CATCH ;] THROWS-AS-NATIVE ;

public

: RUN ( -- )
   T-RESET
   SETUP
   LINK-ALL
   W06-CASE
   W07-CASE
   XT-CASE
   NAME-CASE
   NSHADOW:CLOSE
   T-REPORT ;

;package

WASM-DYNAMIC:RUN
