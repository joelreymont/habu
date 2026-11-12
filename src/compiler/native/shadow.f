\ shadow.f - a second machine's routine for every definition the native driver
\ publishes, mapped from its dictionary record. A cross-build runs the target's
\ prefix on the host and a body's names bind as it compiles, so while a shadow
\ is open src/compiler/native/compiler.f compiles each definition a second time
\ for the shadow's binding, in a nested context, from the HIR module
\ NBACK:FREEZE froze for the engine's own selector.

require src/lib/prelude.f
require src/lib/error.f
require src/lib/le.f
require src/lib/hashmap.f
require src/core/bytes.f
require src/compiler/target.f
require src/compiler/binding.f
require src/compiler/target/native-binding.f
require src/compiler/native/emission.f
require src/compiler/session/lease.f

package NSHADOW
private

CAST: ENTRY-BYTES ( n -- ptr u8 )

\ The shadow is closed; open; or native, open for the engine's own binding
\ with its source already emitted.
ENUM state closed open native ;ENUM
TYPED-VARIABLE SH-STATE state
TYPED-VARIABLE SH-BIND CBIND:binding \ the binding an open shadow compiles for
TYPED-VARIABLE NATIVE-ADDRS-XT [ ptr u8 -- ]
variable NATIVE-ADDRS-SET            \ a backend installed NATIVE-ADDRS-XT

\ ---- the emissions -----------------------------------------------------------
\ Each column counts only the published emissions; a taken one lies just past
\ the counts until PUBLISH claims it. Offsets stay as NEMIT states them: a site
\ or function start is bytes from the first byte of its own emission, and a
\ target is the host address the body named, which the linker resolves to the
\ target's own.
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

\ SOURCE-FUNCTION's table over HM: a host entry to the first published emission
\ and function holding it. It covers emissions [0, SOURCE-N); a query adds the
\ ones published since. Zero SOURCE-CAP marks it empty.
DYNAMIC-BUFFER SOURCE-KEYS n
DYNAMIC-BUFFER SOURCE-USED n
DYNAMIC-BUFFER SOURCE-EM n
DYNAMIC-BUFFER SOURCE-FUN n
variable SOURCE-CAP
variable SOURCE-N

\ ---- the map -----------------------------------------------------------------
\ Keyed by dictionary record, the way an image names a word.
variable N-RECS
DYNAMIC-BUFFER REC-IDX n             \ the dictionary record
DYNAMIC-BUFFER REC-EM n              \ the emission that is its routine
DYNAMIC-BUFFER REC-ENTRY n           \ where in that emission the record enters

\ Each emission PATCH-DOES changed, in the order it changed them.
variable N-PATCHES
DYNAMIC-BUFFER PATCHED n

\ A definer and its `does>` companion are the most records one routine is.
2 constant RECS-MAX

\ ---- the emission taken and not yet published --------------------------------
\ None taken; plain, copied past the counts; or does, a definer's, whose
\ companion record enters at PEND-ENTRY in it.
ENUM pending none plain does ;ENUM
TYPED-VARIABLE PENDING pending
variable PEND-ENTRY

public

: OPEN? ( -- bool )
   SH-STATE @
   MATCH state
      closed OF false ENDOF
      open OF true ENDOF
      native OF true ENDOF
   ;MATCH ;


: NATIVE? ( -- bool )
   SH-STATE @
   MATCH state
      closed OF false ENDOF
      open OF false ENDOF
      native OF true ENDOF
   ;MATCH ;


private

: PENDING? ( -- bool )
   PENDING @
   MATCH pending
      none OF false ENDOF
      plain OF true ENDOF
      does OF true ENDOF
   ;MATCH ;


: DOES-PENDING? ( -- bool )
   PENDING @
   MATCH pending
      none OF false ENDOF
      plain OF false ENDOF
      does OF true ENDOF
   ;MATCH ;


: OPEN-CK ( -- )
   OPEN? 0= if E-NSHADOW-STATE throw then ;

: ROW-CK ( n n -- n ) {: i:n count:n :}
   i 0 <  i count >= or if E-NSHADOW-ROW throw then
   i ;

: EM-CK ( n -- n )
   OPEN-CK N-EMS @ ROW-CK ;

: REC-CK ( n -- n )
   OPEN-CK N-RECS @ ROW-CK ;

\ ---- taking a sealed emission -------------------------------------------------
\ A shadow routine is a sealed NEMIT emission stated by its backend's unplaced
\ row (NBACK:EMIT-UNPLACED): every call, branch and address leaving it is a row
\ whose field the linker writes. NEMIT holds one emission and the engine's own
\ opens it next, so TAKE copies the bytes and rows before the nested context
\ leaves.
\
\ Only bytes of the machine the shadow's binding names are its routines. A
\ native shadow takes the engine's own emission, placed or not; OPEN-NATIVE
\ admitted only the engine's own binding.
: TARGET-CK ( -- )
   NEMIT:ARCH SH-BIND @ CBIND:TARGET@ CTARGET:ARCH@ CTARGET-ARCH:EQ
   0= if E-NSHADOW-TARGET throw then ;

: COPY-BYTES ( -- )
   CODE-N @ {: at:n :}
   NEMIT:SIZE {: size:n :}
   at size + CODE-BUF-RESERVE
   NEMIT:BYTES at CODE-BUF size BYTE-COPY ;

\ A placed native emission keeps its exact recorded rows, while only their
\ carried fields return to the unplaced form the shadow linker consumes.
\ Internal relative calls need no row and retain their displacement.
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

: PATCH+ ( n -- ) {: e:n :}
   N-PATCHES @ 1+ PATCHED-RESERVE
   e N-PATCHES @ PATCHED !  1 N-PATCHES +! ;

\ Whether COPY returns a native shadow's placed emission to the unplaced form,
\ which needs NATIVE-ADDRS!.
: UNPLACE? ( -- bool )
   NATIVE? NEMIT:PLACED? and 0= if false exit then
   NATIVE-ADDRS-SET @ 0= if E-NSHADOW-STATE throw then
   true ;

\ A copy taken over one no publication claimed simply replaces it: both lie
\ past the counts.
: COPY ( -- )
   OPEN-CK
   TARGET-CK
   UNPLACE? {: unplace:bool :}
   construct pending none PENDING !
   COPY-BYTES
   unplace if
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
   construct pending none PENDING !  0 PEND-ENTRY !
   0 CODE-N !  0 N-EMS !  0 N-FUNS !  0 N-CALLS !  0 N-ADDRS !  0 N-RECS !
   0 SOURCE-CAP !  0 SOURCE-N !  0 N-PATCHES ! ;

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
   SOURCE-KEYS-RELEASE  SOURCE-USED-RELEASE  SOURCE-EM-RELEASE  SOURCE-FUN-RELEASE
   REC-IDX-RELEASE  REC-EM-RELEASE  REC-ENTRY-RELEASE  PATCHED-RELEASE ;

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

: SOURCE-SLOT ( n -- n )
   {: key:n :}
   0 SOURCE-KEYS  0 SOURCE-USED  SOURCE-CAP @  key HM:PROBE ;


: SOURCE-FILE ( n n -- )
   {: e:n f:n :}
   e f FUN-ROW FUN-SOURCE @ {: key:n :}
   key SOURCE-SLOT {: s:n :}
   s SOURCE-USED @ 0<> if exit then
   1 s SOURCE-USED !  key s SOURCE-KEYS !  e s SOURCE-EM !  f s SOURCE-FUN ! ;


\ An empty table for n functions. HM:PROBE needs a power-of-two cap; at least
\ twice the functions keeps the table at most half full.
: SOURCE-EMPTY ( n -- )
   2 * 16 max  1 begin 2dup > while 2 * repeat nip {: cap:n :}
   cap SOURCE-CAP !
   cap SOURCE-KEYS-RESERVE  cap SOURCE-USED-RESERVE
   cap SOURCE-EM-RESERVE  cap SOURCE-FUN-RESERVE
   0 SOURCE-USED cap HM:CLEAR
   0 SOURCE-N ! ;


\ Adds the emissions published since the last query, refiling every one into a
\ larger table once their functions would fill more than half of it. Published
\ rows never change until CLEAR, so filing in publication order keeps the first.
: SOURCE-INDEX ( -- )
   N-FUNS @ {: funs:n :}
   SOURCE-CAP @ 0=  funs 2 * SOURCE-CAP @ >  or if funs SOURCE-EMPTY then
   N-EMS @ SOURCE-N @ ?do
      i EM-FUNS @ 0 ?do j i SOURCE-FILE loop
   loop
   N-EMS @ SOURCE-N ! ;

public

\ ---- opening and closing -----------------------------------------------------
\ CLOSE gives everything back, and an image capture closes the shadow, so no map
\ outlives the mappings it is stored in.
: OPEN ( CBIND:binding -- ) {: b:CBIND:binding :}
   NLEASE:IDLE-CK
   OPEN? if E-NSHADOW-STATE throw then
   b CBIND:VALIDATE SH-BIND !
   CLEAR
   construct state open SH-STATE ! ;

\ A placed emission's address sites carry the host address it was placed with.
\ A backend that hands a native shadow a placed emission installs how its sites
\ carry an address and return to the unplaced form (src/arch/x86-64/passes.f),
\ given the first byte of its copy while the emission stands sealed in NEMIT;
\ COPY refuses such an emission until one has.
: NATIVE-ADDRS! ( [ ptr u8 -- ] -- )
   NATIVE-ADDRS-XT !
   1 NATIVE-ADDRS-SET ! ;

\ The engine's own emission is compiled for the engine's own binding, so a
\ native shadow opens for no other.
: OPEN-NATIVE ( CBIND:binding -- ) {: b:CBIND:binding :}
   b RTARGET:HOST-TARGET RTARGET:NATIVE-BINDING CBIND:SAME?
   0= if E-NSHADOW-TARGET throw then
   b OPEN
   construct state native SH-STATE ! ;

\ An idle capture can close it whether a shadow is open or not.
: CLOSE ( -- )
   NLEASE:IDLE-CK
   construct state closed SH-STATE !
   CLEAR
   RELEASE-ROWS ;

: BINDING ( -- CBIND:binding )
   OPEN-CK SH-BIND @ ;

\ ---- what the driver and publication do --------------------------------------
\ Copy the emission of the definition being compiled while it stands sealed in
\ NEMIT, before its backend's RETIRE row clears it.
: TAKE ( -- )
   COPY
   construct pending plain PENDING ! ;

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
   construct pending does PENDING ! ;

\ The x86 def-cast writer publishes a raw one-byte RET identity body. It seals
\ no NEMIT emission because the kernel writes it directly, so that writer
\ reports the exact published entry and span through its fixed-shadow callback.
: TAKE-CAST ( n n -- ) {: at:n size:n :}
   OPEN-CK
   NATIVE? 0= if E-NSHADOW-STATE throw then
   size 1 <> if E-NSHADOW-ROW throw then
   at ENTRY-BYTES c@ $C3 <> if E-NSHADOW-ROW throw then
   construct pending none PENDING !
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
   construct pending plain PENDING ! ;

\ Pairs the pending copy's functions with the host emission's, placed at `at`,
\ before publication commits: both walks emit the same functions, a stub last.
: SOURCE-FUNS ( n -- ) {: at:n :}
   PENDING? 0= if exit then
   N-EMS @ {: e:n :}
   NEMIT:FUNCTIONS
   e EM-FUNS @ <> if E-NSHADOW-ROW throw then
   e EM-FUNS @ 0 ?do
      at i NEMIT:FUNCTION-OFFSET@ +  e EM-FUN0 @ i + FUN-SOURCE !
   loop ;

\ File the taken emission under the record publication has just committed, and
\ a definer's companion under the next. Nonthrowing: publication runs it past
\ its code window, and TAKE made the room.
: PUBLISH ( n -- ) {: idx:n :}
   PENDING? 0= if exit then
   N-EMS @ {: e:n :}
   N-RECS @ {: k:n :}
   idx k REC-IDX !  e k REC-EM !  0 k REC-ENTRY !
   DOES-PENDING? if
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
   construct pending none PENDING ! ;

\ The host's does> primitive patches its created ARM body after CREATE has
\ published, passing the clause's ARM entry, or zero for an empty replacement
\ that restores the body `create` made; record it as the jump slot's
\ relocation target. The x86 body holds a five-byte slot and the capture
\ resolves the tail call through the clause companion's row like any other
\ tail site. A backend with no slot (Wasm) cannot carry the clause, and since
\ a later replacement can still clear the target, nothing is dropped here:
\ ROUTINE? answers the capture when the map is read. The patched emission joins
\ PATCHES.
: PATCH-DOES ( n n -- ) {: idx:n target:n :}
   OPEN? 0= if exit then
   N-RECS @ 0 ?do
      i REC-IDX @ idx = if
         target i REC-EM @ EM-PATCH-TGT !
         i REC-EM @ PATCH+
         unloop exit
      then
   loop
   E-NSHADOW-ROW throw ;

\ Drop a taken emission no publication claimed. Nonthrowing: compiler.f
\ RETIRE-BODY runs it as every tier-1 definition ends, refused or accepted.
: ABANDON ( -- )
   construct pending none PENDING ! ;

\ ---- the map ----------------------------------------------------------------
\ Records in the order they were published.
: RECORDS ( -- n )
   OPEN-CK N-RECS @ ;

: RECORD@ ( n -- n )
   REC-CK REC-IDX @ ;

: EMISSION@ ( n -- n )
   REC-CK REC-EM @ ;

\ The does> patches since the shadow opened, and the emission the k-th changed.
: PATCHES ( -- n )
   OPEN-CK N-PATCHES @ ;

: PATCHED@ ( n -- n )
   OPEN-CK N-PATCHES @ ROW-CK PATCHED @ ;

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

\ Its published host entry, or zero when publication filed none.
: FUNCTION-SOURCE@ ( n n -- n )
   FUN-ROW FUN-SOURCE @ ;

\ A quotation has no dictionary record. An exact published host function entry
\ still identifies its HIR sibling in the shadow emission; no interior address
\ or nearest routine span is accepted as an identity.
: SOURCE-FUNCTION ( n -- n n ) {: xt:n :}
   OPEN-CK
   SOURCE-INDEX
   xt SOURCE-SLOT {: s:n :}
   s SOURCE-USED @ 0= if -1 -1 exit then
   s SOURCE-EM @  s SOURCE-FUN @ ;

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

\ Emission e's does> jump slot, or -1, whether or not a clause has patched it.
: PATCH-SITE@ ( n -- n )
   EM-CK EM-PATCH @ ;

: ADDR-SITES ( n -- n )
   EM-CK EM-ADDRS @ ;

: ADDR-SITE@ ( n n -- n )
   ADDR-ROW ADDR-OFF @ ;

: ADDR-SITE-KIND@ ( n n -- n )
   ADDR-ROW ADDR-KIND @ ;

seal-package

;package
