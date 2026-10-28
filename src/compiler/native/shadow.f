\ shadow.f - a second machine's routine for every definition the native driver
\ publishes, and the map from each published record to it.
\
\ WHAT IT IS FOR. A cross-build runs the target's whole prefix on the host:
\ every declarer and immediate executes as the source loads, so a definition
\ has to be the host's routine, to run now, and the target's, to be written
\ into the target's image. Lowering the target's routines later from the
\ recorded source is not open, because a body's names are bound against the
\ dictionary as it compiles. So while a shadow is open the native driver
\ (src/compiler/native/compiler.f) compiles each definition for the shadow's
\ binding too: in a context nested inside the definition's own and bound to that
\ binding, from the very HIR module NBACK:FREEZE froze for the engine's own
\ selector.
\
\ WHAT IT KEEPS. The shadow's routine is a sealed NEMIT emission like any other,
\ stated by its backend's unplaced row (NBACK:EMIT-UNPLACED): measured from no
\ slot, so every call, branch and address that leaves it is a row and its field
\ is the linker's to write. NEMIT holds one emission and the engine's own
\ emission opens it next, so TAKE copies the bytes and every row here before
\ the nested context leaves. PUBLISH then files the copy under the dictionary
\ record publication commits; the map is keyed by record because a record is
\ how an image names a word. A `does>` definer is two records over one routine:
\ the companion enters where the clause function starts, read off the copied
\ function rows.
\
\ OFFSETS STAY AS NEMIT STATES THEM: a site or a function start is bytes from
\ the first byte of its own emission, and a target is the absolute address the
\ definition's body named on the host, which is what the linker resolves to the
\ target's own.
\
\ LIFETIME. OPEN starts an empty map for one binding. CLOSE gives everything
\ back, and an image capture closes the shadow, so no map outlives the mappings
\ it is stored in. An emission TAKE copied and no publication claimed - the
\ engine's own chain refused the definition after the shadow's sealed - is
\ dropped by ABANDON, which the driver runs as every definition ends, so a copy
\ never outlives the definition it was compiled from.

require src/lib/prelude.f
require src/lib/errors.f
require src/lib/le.f
require src/core/bytes.f
require src/compiler/target.f
require src/compiler/binding.f
require src/compiler/target/native-binding.f
require src/compiler/native/emission.f
require src/compiler/session/lease.f

package NSHADOW
private

TRUSTED: ENTRY-BYTES ( n -- ptr u8 ) ;

variable OPENED                      \ a shadow is open
variable NATIVE                      \ its source already emitted for this binding
TYPED-VARIABLE SH-BIND CBIND:binding \ and the binding it compiles for
TYPED-VARIABLE NATIVE-ADDRS-XT [ ptr u8 -- ]
variable NATIVE-ADDRS-SET            \ a backend installed NATIVE-ADDRS-XT

\ ---- the emissions -----------------------------------------------------------
\ Each column counts only the published emissions; a taken one lies just past
\ the counts until PUBLISH claims it.
DYNAMIC-BUFFER CODE-BUF u8           \ every emission's bytes, one after another
variable CODE-N

variable N-EMS
DYNAMIC-BUFFER EM-AT n               \ where an emission starts in CODE-BUF
DYNAMIC-BUFFER EM-SIZE n             \ how many bytes it is
DYNAMIC-BUFFER EM-RET n              \ how many of them are its trailing return
DYNAMIC-BUFFER EM-FUN0 n             \ its first function row
DYNAMIC-BUFFER EM-FUNS n             \ and how many it has
DYNAMIC-BUFFER EM-CALL0 n            \ its first call row
DYNAMIC-BUFFER EM-CALLS n
DYNAMIC-BUFFER EM-ADDR0 n            \ its first address row
DYNAMIC-BUFFER EM-ADDRS n
DYNAMIC-BUFFER EM-PATCH n            \ created-word does> jump, or -1
DYNAMIC-BUFFER EM-PATCH-TGT n        \ later clause entry, or zero

variable N-FUNS
DYNAMIC-BUFFER FUN-OFF n             \ where a function starts
DYNAMIC-BUFFER FUN-SOURCE n          \ matching function's published host entry, or zero
variable N-CALLS
DYNAMIC-BUFFER CALL-OFF n            \ where a call or leaving branch sits
DYNAMIC-BUFFER CALL-KIND n           \ NEMIT:CALL or NEMIT:TAIL
DYNAMIC-BUFFER CALL-TGT n            \ the absolute address it goes to
variable N-ADDRS
DYNAMIC-BUFFER ADDR-OFF n            \ where an address chain starts
DYNAMIC-BUFFER ADDR-KIND n           \ which kind of address it carries

\ ---- the map -----------------------------------------------------------------
variable N-RECS
DYNAMIC-BUFFER REC-IDX n             \ the dictionary record
DYNAMIC-BUFFER REC-EM n              \ the emission that is its routine
DYNAMIC-BUFFER REC-ENTRY n           \ where in that emission the record enters

\ A definer and its `does>` companion are the most records one routine is.
2 constant RECS-MAX

\ ---- the emission taken and not yet published --------------------------------
variable PENDING                     \ TAKE has copied one past the counts
variable PEND-DOES                   \ it is a definer's, with a companion record
variable PEND-ENTRY                  \ which enters at this offset in it

: OPEN-CK ( -- )
   OPENED @ 0= if E-NSHADOW-STATE throw then ;

: ROW-CK ( n n -- n ) {: i:n count:n :}
   i 0 <  i count >= or if E-NSHADOW-ROW throw then
   i ;

: EM-CK ( n -- n )
   OPEN-CK N-EMS @ ROW-CK ;

: REC-CK ( n -- n )
   OPEN-CK N-RECS @ ROW-CK ;

\ ---- taking a sealed emission -------------------------------------------------
\ Only bytes of the machine the shadow's binding names are its routines. A
\ native shadow takes the engine's own placed emission; OPEN-NATIVE admitted
\ only the engine's own binding.
: TARGET-CK ( -- )
   NEMIT:ARCH SH-BIND @ CBIND:TARGET@ CTARGET:ARCH@ CTARGET-ARCH:EQ
   0= if E-NSHADOW-TARGET throw then
   NATIVE @ 0= if exit then
   NEMIT:PLACED? 0= if E-NSHADOW-TARGET throw then ;

: COPY-BYTES ( -- )
   CODE-N @ {: at:n :}
   NEMIT:SIZE {: size:n :}
   at size + CODE-BUF-RESERVE
   NEMIT:BYTES at CODE-BUF size BYTE-COPY ;

\ A native emission is already placed. Keep its exact recorded rows, while
\ returning only their carried fields to the unplaced form the shadow linker
\ consumes. Internal relative calls need no row and retain their displacement.
5 constant REL32-BYTES
1 constant REL32-FIELD
$E8 constant CALL-OP
$E9 constant TAIL-OP

: NATIVE-CALLS ( -- )
   NEMIT:CALL-SITES 0 ?do
      i NEMIT:CALL-SITE@ {: off:n :}
      off 0 < off REL32-BYTES + NEMIT:SIZE > or if E-NSHADOW-ROW throw then
      i NEMIT:CALL-KIND@ {: kind:n :}
      kind NEMIT:CALL = if CALL-OP else
         kind NEMIT:TAIL = if TAIL-OP else E-NSHADOW-ROW throw then
      then {: opcode:n :}
      CODE-N @ off + CODE-BUF {: p:ptr :}
      p c@ opcode <> if E-NSHADOW-ROW throw then
      0 p REL32-FIELD + LE:U32!
   loop ;

: COPY-FUNS ( -- )
   N-FUNS @ {: k0:n :}
   NEMIT:FUNCTIONS {: n:n :}
   k0 n + FUN-OFF-RESERVE
   k0 n + FUN-SOURCE-RESERVE
   n 0 ?do
      i NEMIT:FUNCTION-OFFSET@ k0 i + FUN-OFF !
      0 k0 i + FUN-SOURCE !
   loop ;

: COPY-CALLS ( -- )
   N-CALLS @ {: k0:n :}
   NEMIT:CALL-SITES {: n:n :}
   k0 n + CALL-OFF-RESERVE
   k0 n + CALL-KIND-RESERVE
   k0 n + CALL-TGT-RESERVE
   n 0 ?do
      i NEMIT:CALL-SITE@ k0 i + CALL-OFF !
      i NEMIT:CALL-KIND@ k0 i + CALL-KIND !
      i NEMIT:CALL-TARGET@ k0 i + CALL-TGT !
   loop ;

: COPY-ADDRS ( -- )
   N-ADDRS @ {: k0:n :}
   NEMIT:ADDR-SITES {: n:n :}
   k0 n + ADDR-OFF-RESERVE
   k0 n + ADDR-KIND-RESERVE
   n 0 ?do
      i NEMIT:ADDR-SITE@ k0 i + ADDR-OFF !
      i NEMIT:ADDR-SITE-KIND@ k0 i + ADDR-KIND !
   loop ;

: EM-RESERVE ( n -- ) {: n:n :}
   n EM-AT-RESERVE  n EM-SIZE-RESERVE  n EM-RET-RESERVE
   n EM-FUN0-RESERVE  n EM-FUNS-RESERVE
   n EM-CALL0-RESERVE  n EM-CALLS-RESERVE
   n EM-ADDR0-RESERVE  n EM-ADDRS-RESERVE
   n EM-PATCH-RESERVE  n EM-PATCH-TGT-RESERVE ;

: COPY-ROW ( -- )
   N-EMS @ {: e:n :}
   e 1+ EM-RESERVE
   CODE-N @ e EM-AT !
   NEMIT:SIZE e EM-SIZE !
   NEMIT:RET-BYTES e EM-RET !
   N-FUNS @ e EM-FUN0 ! NEMIT:FUNCTIONS e EM-FUNS !
   N-CALLS @ e EM-CALL0 ! NEMIT:CALL-SITES e EM-CALLS !
   N-ADDRS @ e EM-ADDR0 ! NEMIT:ADDR-SITES e EM-ADDRS !
   NEMIT:PATCH-SLOT e EM-PATCH !
   0 e EM-PATCH-TGT ! ;

\ Room for the records is made here, so PUBLISH, which runs after publication
\ has committed the engine's own routine, grows nothing and refuses nothing.
: REC-RESERVE ( -- )
   N-RECS @ RECS-MAX + {: n:n :}
   n REC-IDX-RESERVE  n REC-EM-RESERVE  n REC-ENTRY-RESERVE ;

\ A copy taken over one no publication claimed simply replaces it: both lie
\ past the counts.
: COPY ( -- )
   OPEN-CK
   TARGET-CK
   0 PENDING !
   COPY-BYTES
   NATIVE @ 0<> if
      NATIVE-CALLS
      CODE-N @ CODE-BUF NATIVE-ADDRS-XT @ execute
   then
   COPY-FUNS
   COPY-CALLS
   COPY-ADDRS
   COPY-ROW
   REC-RESERVE ;

\ ---- the counts -------------------------------------------------------------
: CLEAR ( -- )
   0 PENDING !  0 PEND-DOES !  0 PEND-ENTRY !
   0 CODE-N !  0 N-EMS !  0 N-FUNS !  0 N-CALLS !  0 N-ADDRS !  0 N-RECS ! ;

: RELEASE-ROWS ( -- )
   CODE-BUF-RELEASE
   EM-AT-RELEASE  EM-SIZE-RELEASE  EM-RET-RELEASE
   EM-FUN0-RELEASE  EM-FUNS-RELEASE
   EM-CALL0-RELEASE  EM-CALLS-RELEASE
   EM-ADDR0-RELEASE  EM-ADDRS-RELEASE
   EM-PATCH-RELEASE  EM-PATCH-TGT-RELEASE
   FUN-OFF-RELEASE  FUN-SOURCE-RELEASE
   CALL-OFF-RELEASE  CALL-KIND-RELEASE  CALL-TGT-RELEASE
   ADDR-OFF-RELEASE  ADDR-KIND-RELEASE
   REC-IDX-RELEASE  REC-EM-RELEASE  REC-ENTRY-RELEASE ;

\ Row k of emission e's rows of one kind, which start at `first` and number
\ `count`.
: SUB-ROW ( n n n -- n ) {: k:n first:n count:n :}
   k count ROW-CK first + ;

: FUN-ROW ( n n -- n ) {: e:n k:n :}
   e EM-CK drop
   k  e EM-FUN0 @  e EM-FUNS @  SUB-ROW ;

: CALL-ROW ( n n -- n ) {: e:n k:n :}
   e EM-CK drop
   k  e EM-CALL0 @  e EM-CALLS @  SUB-ROW ;

: ADDR-ROW ( n n -- n ) {: e:n k:n :}
   e EM-CK drop
   k  e EM-ADDR0 @  e EM-ADDRS @  SUB-ROW ;

\ Whether a clause has been patched into emission e's jump slot: the tail
\ call CALL-SITES reports past its rows. An emission with no slot carries none.
: SLOT-CALL? ( n -- bool ) {: e:n :}
   e EM-PATCH @ 0 >=  e EM-PATCH-TGT @ 0<>  and ;

public

\ ---- opening and closing -----------------------------------------------------
: OPEN ( CBIND:binding -- ) {: b:CBIND:binding :}
   NLEASE:IDLE-CK
   OPENED @ 0<> if E-NSHADOW-STATE throw then
   b CBIND:VALIDATE SH-BIND !
   CLEAR
   0 NATIVE !
   1 OPENED ! ;

\ A native shadow keeps the placed emission's bytes, so each address site still
\ carries the host address it was placed with. The backend whose emissions a
\ native shadow takes installs how its sites carry an address and return to the
\ unplaced form (src/arch/x86-64/passes.f), given the first byte of its copy
\ while the emission stands sealed in NEMIT; OPEN-NATIVE refuses until one has.
: NATIVE-ADDRS! ( [ ptr u8 -- ] -- )
   NATIVE-ADDRS-XT !
   1 NATIVE-ADDRS-SET ! ;

\ The engine's placed emission is compiled for the engine's own binding, so a
\ native shadow opens for no other.
: OPEN-NATIVE ( CBIND:binding -- ) {: b:CBIND:binding :}
   NATIVE-ADDRS-SET @ 0= if E-NSHADOW-STATE throw then
   b RTARGET:HOST-TARGET RTARGET:NATIVE-BINDING CBIND:SAME?
   0= if E-NSHADOW-TARGET throw then
   b OPEN
   1 NATIVE ! ;

\ An idle capture can close it whether a shadow is open or not.
: CLOSE ( -- )
   NLEASE:IDLE-CK
   0 OPENED !
   0 NATIVE !
   CLEAR
   RELEASE-ROWS ;

: OPEN? ( -- bool )
   OPENED @ 0<> ;

: NATIVE? ( -- bool )
   OPEN? NATIVE @ 0<> and ;

: BINDING ( -- CBIND:binding )
   OPEN-CK SH-BIND @ ;

\ ---- what the driver and publication do --------------------------------------
\ Copy the emission of the definition being compiled while it stands sealed in
\ NEMIT, before its backend's RETIRE row clears it.
: TAKE ( -- )
   COPY
   0 PEND-DOES !
   1 PENDING ! ;

\ The same for a `does>` definer, whose clause is function `fun` of the
\ emission: the companion record enters there, so it cannot be where the
\ definer itself enters.
: TAKE-DOES ( n -- )
   {: fun:n :}
   COPY
   N-EMS @ {: e:n :}
   fun  e EM-FUN0 @  e EM-FUNS @  SUB-ROW  FUN-OFF @ {: off:n :}
   off 0 <= if E-NSHADOW-ROW throw then
   off PEND-ENTRY !
   1 PEND-DOES !
   1 PENDING ! ;

\ The x86 def-cast writer publishes a raw one-byte RET identity body. It seals
\ no NEMIT emission because the kernel writes it directly, so that writer
\ reports the exact published entry and span through its fixed-shadow callback.
: TAKE-CAST ( n n -- ) {: at:n size:n :}
   OPEN-CK
   NATIVE @ 0= if E-NSHADOW-STATE throw then
   size 1 <> if E-NSHADOW-ROW throw then
   at ENTRY-BYTES c@ $C3 <> if E-NSHADOW-ROW throw then
   0 PENDING !
   N-EMS @ {: e:n :}
   e 1+ EM-RESERVE
   CODE-N @ e EM-AT !
   size e EM-SIZE !  size e EM-RET !
   N-FUNS @ e EM-FUN0 !  1 e EM-FUNS !
   N-CALLS @ e EM-CALL0 !  0 e EM-CALLS !
   N-ADDRS @ e EM-ADDR0 !  0 e EM-ADDRS !
   -1 e EM-PATCH !  0 e EM-PATCH-TGT !
   CODE-N @ size + CODE-BUF-RESERVE
   at ENTRY-BYTES CODE-N @ CODE-BUF size BYTE-COPY
   N-FUNS @ 1+ FUN-OFF-RESERVE  N-FUNS @ 1+ FUN-SOURCE-RESERVE
   0 N-FUNS @ FUN-OFF !  at N-FUNS @ FUN-SOURCE !
   REC-RESERVE
   0 PEND-DOES !
   1 PENDING ! ;

\ Pair the host emission's function entries with the pending shadow's entries
\ before the host publisher commits either code or dictionary record. Both
\ selectors retain the same frozen HIR function sequence, even when their
\ instruction and block layouts differ. The host emission is the one NEMIT
\ holds sealed, placed at `at`. TAKE reserved the rows; this grows nothing, and
\ a failed publication discards the pending pair with ABANDON.
: SOURCE-FUNS ( n -- ) {: at:n :}
   PENDING @ 0= if exit then
   N-EMS @ {: e:n :}
   NEMIT:FUNCTIONS e EM-FUNS @ <> if E-NSHADOW-ROW throw then
   e EM-FUNS @ 0 ?do
      at i NEMIT:FUNCTION-OFFSET@ +  e EM-FUN0 @ i + FUN-SOURCE !
   loop ;

\ File the taken emission under the record publication has just committed, and
\ a definer's companion under the next. Nonthrowing: publication runs it past
\ its code window, and TAKE made the room. With nothing taken - no shadow is
\ open - there is nothing to file.
: PUBLISH ( n -- ) {: idx:n :}
   PENDING @ 0= if exit then
   N-EMS @ {: e:n :}
   N-RECS @ {: k:n :}
   idx k REC-IDX !  e k REC-EM !  0 k REC-ENTRY !
   PEND-DOES @ 0<> if
      idx 1+ k 1+ REC-IDX !  e k 1+ REC-EM !  PEND-ENTRY @ k 1+ REC-ENTRY !
      k 2 + N-RECS !
   else
      k 1+ N-RECS !
   then
   e EM-SIZE @ CODE-N +!
   e EM-FUNS @ N-FUNS +!
   e EM-CALLS @ N-CALLS +!
   e EM-ADDRS @ N-ADDRS +!
   e 1+ N-EMS !
   0 PENDING ! ;

\ The host's does> primitive patches its created ARM body after CREATE has
\ published, passing the clause's ARM entry, or zero for an empty replacement
\ that restores the body `create` made; record it as the jump slot's
\ relocation target. The x86 body holds a five-byte slot and the capture
\ resolves the tail call through the clause companion's row like any other
\ tail site. A backend with no slot (Wasm) cannot carry the clause, and since
\ a later replacement can still clear the target, nothing is dropped here:
\ ROUTINE? answers the capture when the map is read.
: PATCH-DOES ( n n -- ) {: idx:n target:n :}
   OPENED @ 0= if exit then
   N-RECS @ 0 ?do
      i REC-IDX @ idx = if
         target i REC-EM @ EM-PATCH-TGT !
         unloop exit
      then
   loop
   E-NSHADOW-ROW throw ;

\ Drop a taken emission no publication claimed. Nonthrowing: the driver runs it
\ as every definition ends, on the refusing path and the accepting one alike.
: ABANDON ( -- )
   0 PENDING ! ;

\ ---- the map ----------------------------------------------------------------
\ Records in the order they were published.
: RECORDS ( -- n )
   OPEN-CK N-RECS @ ;

\ The dictionary index of record row k.
: RECORD@ ( n -- n )
   REC-CK REC-IDX @ ;

\ The emission that is its routine.
: EMISSION@ ( n -- n )
   REC-CK REC-EM @ ;

\ Where in that emission it enters: zero for a definition, the clause function's
\ start for a `does>` companion.
: ENTRY@ ( n -- n )
   REC-CK REC-ENTRY @ ;

\ ---- each emission, as NEMIT stated it ---------------------------------------
: EMISSIONS ( -- n )
   OPEN-CK N-EMS @ ;

: SIZE ( n -- n )
   EM-CK EM-SIZE @ ;

\ Its first byte, until the next TAKE moves the storage.
: BYTES ( n -- ptr u8 )
   EM-CK EM-AT @ CODE-BUF ;

: RET-BYTES ( n -- n )
   EM-CK EM-RET @ ;

: FUNCTIONS ( n -- n )
   EM-CK EM-FUNS @ ;

: FUNCTION-OFFSET@ ( n n -- n )
   FUN-ROW FUN-OFF @ ;

\ A quotation has no dictionary record. An exact published host function entry
\ still identifies its HIR sibling in the shadow emission; no interior address
\ or nearest routine span is accepted as an identity.
: SOURCE-FUNCTION ( n -- n n ) {: xt:n :}
   OPEN-CK
   N-EMS @ 0 ?do
      i EM-FUNS @ 0 ?do
         j i FUN-ROW FUN-SOURCE @ xt = if j i unloop unloop exit then
      loop
   loop
   -1 -1 ;

: CALL-SITES ( n -- n )
   EM-CK {: e:n :}
   e EM-CALLS @
   e SLOT-CALL? if 1+ then ;

\ Whether emission e still does what its records do: false once a clause has
\ patched a created body that carries no slot for it (the Wasm backend emits
\ none), so the capture ships that record with no routine and the linker
\ refuses it by name when reached.
: ROUTINE? ( n -- bool )
   EM-CK {: e:n :}
   e EM-PATCH @ 0 >=  e EM-PATCH-TGT @ 0= or ;

: PATCH-CALL? ( n n -- bool ) {: e:n k:n :}
   e SLOT-CALL?  k e EM-CALLS @ = and ;

: CALL-SITE@ ( n n -- n )
   {: e:n k:n :}
   e EM-CK drop
   e k PATCH-CALL? if e EM-PATCH @ exit then
   e k CALL-ROW CALL-OFF @ ;

: CALL-KIND@ ( n n -- n )
   {: e:n k:n :}
   e EM-CK drop
   e k PATCH-CALL? if NEMIT:TAIL exit then
   e k CALL-ROW CALL-KIND @ ;

: CALL-TARGET@ ( n n -- n )
   {: e:n k:n :}
   e EM-CK drop
   e k PATCH-CALL? if e EM-PATCH-TGT @ exit then
   e k CALL-ROW CALL-TGT @ ;

: ADDR-SITES ( n -- n )
   EM-CK EM-ADDRS @ ;

: ADDR-SITE@ ( n n -- n )
   ADDR-ROW ADDR-OFF @ ;

: ADDR-SITE-KIND@ ( n n -- n )
   ADDR-ROW ADDR-KIND @ ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;package
