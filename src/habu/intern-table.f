\ intern-table.f - the string tables, package INTERN's first part: a build
\ space's and hb's own. Loaded before checker.f by each core prefix so the
\ checker can reach it; src/habu/intern.f reopens INTERN.

package INTERN
private

\ error.f owns E-INTERN-REWIND but loads later: the same (code, name)
\ is registered here.
-8638 constant E-INTERN-REWIND

variable HV

\ FNV-1a; the top bit can run into the sign.
: FNV ( ptr u8 n -- n )
   {: a:ptr u:n :}
   $811C9DC5 HV !
   u 0 ?do
      a i + c@ HV @ xor  $1000193 *  HV !
   loop
   HV @ ;

\ ---- a table ------------------------------------------------------------------
\ Four cells: the slots' address, the slot count, the filled count and the
\ highest text's end; addresses are distances from NULL-PTR, 0 for none.
1024 constant FIRST-SLOTS

: SIZE-CELL ( ptr n -- ptr n ) cell+ ;              \ a power of two
: FILLED-CELL ( ptr n -- ptr n ) cell+ cell+ ;
: HIGHEST-CELL ( ptr n -- ptr n ) 3 cells + ;

\ These cells name memory this process mapped, so a save writes them null and
\ the saved program's TABLE maps a fresh table.
variable SPACE-IMAGE     \ the image base the space's table serves, 0 for none
SPACE-IMAGE scratch-cell-mark
create SPACE-TABLE here scratch-cell-mark 4 cells allot
create OWN-TABLE here scratch-cell-mark 4 cells allot

: IMAGE-BASE ( -- n ) data-base BOOT:IMAGE-BASE-CELL + @ ;
: SPACE? ( -- bool ) IMAGE-BASE 0 <> ;

: CELL-AT ( n n -- ptr n )
   {: base:n k:n :}
   NULL-PTR BYTE-VIEW base + k cells + CELL-VIEW ;


: SLOT-CELL ( ptr n n -- ptr n )
   {: t:ptr k:n :}
   t @ k CELL-AT ;


\ A slot holds a body's address, 0 when empty; the length is the cell before it.
: BODY$ ( n -- ptr u8 n )
   {: body:n :}
   NULL-PTR BYTE-VIEW body +
   body CELL - 0 CELL-AT @ ;


: SLOT-ENDS? ( n ptr n ptr u8 n -- bool )
   {: k:n t:ptr a:ptr u:n :}
   t k SLOT-CELL @ {: body:n :}
   body 0= if 0 0= exit then
   body BODY$ a u CORE-STR= ;


\ The slot holding the body equal to a u, else the empty slot it goes in: one
\ exists, since a table is never more than half full (LAY).
: SLOT-FOR ( ptr n ptr u8 n -- n )
   {: t:ptr a:ptr u:n :}
   t SIZE-CELL @ 1- {: mask:n :}
   a u FNV mask and
   begin dup t a u SLOT-ENDS? 0= while 1+ mask and repeat ;


: SLOT-PUT ( ptr n n -- )
   {: t:ptr body:n :}
   body  t  t body BODY$ SLOT-FOR  SLOT-CELL !
   1 t FILLED-CELL +!
   body BODY$ nip body +  t HIGHEST-CELL @  max  t HIGHEST-CELL ! ;


\ The slots grow, so they are mapped outside DATA.
: TABLE-MAP ( n -- n )
   cells map-anon 0 <> if drop s" intern: table mmap failed" 76 die then
   NULL-PTR - ;


: TABLE-RELEASE ( n n -- )
   {: base:n slots:n :}
   base 0 CELL-AT slots cells munmap 0 <> if
      s" intern: table munmap failed" 76 die
   then ;


: TABLE-OPEN ( ptr n -- )
   {: t:ptr :}
   FIRST-SLOTS TABLE-MAP t !
   FIRST-SLOTS t SIZE-CELL !
   0 t FILLED-CELL !
   0 t HIGHEST-CELL ! ;


\ To forget it rebuilds: a cleared slot would cut a later body's probe chain.
: REBUILD ( ptr n n n -- )
   {: t:ptr size:n limit:n :}
   t @ {: old:n :}
   t SIZE-CELL @ {: slots:n :}
   size TABLE-MAP t !
   size t SIZE-CELL !
   0 t FILLED-CELL !
   0 t HIGHEST-CELL !
   slots 0 ?do
      old i CELL-AT @ {: body:n :}
      body 0 <> if
         body BODY$ nip body + limit <= if t body SLOT-PUT then
      then
   loop
   old slots TABLE-RELEASE ;


\ The limit is above every address.
: WIDEN ( ptr n -- )
   {: t:ptr :}
   t  t SIZE-CELL @ 2 *  -1 1 rshift  REBUILD ;


\ One `allot` checks the bounds and zeroes before the copy; no `,`, so the
\ length cell never joins a created body's grid. It ends aligned, as `create`.
\ Every body takes a byte past its text, so no other datum starts at its
\ address or one past its last byte: a stripped link keeps a body by an
\ address in it alone.
: LAY ( ptr n ptr u8 n -- n )
   {: t:ptr a:ptr u:n :}
   t FILLED-CELL @ 1+ 2 *  t SIZE-CELL @ > if t WIDEN then
   align here NULL-PTR - {: head:n :}
   CELL u 1 + + allot
   u head 0 CELL-AT !
   head CELL + {: address:n :}
   NULL-PTR BYTE-VIEW address + {: dst:ptr :}
   u 0 ?do a i + c@ dst i + c! loop
   align
   t address SLOT-PUT
   address ;

public

\ Frees the table of the build whose space is closing.
: CLEANUP ( -- )
   SPACE-IMAGE @ 0= if exit then
   SPACE-TABLE @  SPACE-TABLE SIZE-CELL @  TABLE-RELEASE
   0 SPACE-TABLE !  0 SPACE-IMAGE ! ;


\ Hands visit each body the build's table holds, as its distance from NULL-PTR.
\ Bodies go stale only once DP moves back, and the build then writes nothing.
: EACH-STRING ( [ n -- ] -- )
   {: visit :}
   SPACE-IMAGE @ 0=  SPACE-IMAGE @ IMAGE-BASE <>  or if exit then
   SPACE-TABLE SIZE-CELL @ 0 ?do
      SPACE-TABLE i SLOT-CELL @ {: body:n :}
      body 0 <> if body visit execute then
   loop ;

private

: SPACE-OPEN ( -- )
   CLEANUP
   SPACE-TABLE TABLE-OPEN
   IMAGE-BASE SPACE-IMAGE ! ;


: TABLE ( -- ptr n )
   SPACE? if
      SPACE-IMAGE @ IMAGE-BASE <> if SPACE-OPEN then
      SPACE-TABLE exit
   then
   OWN-TABLE @ 0= if OWN-TABLE TABLE-OPEN then
   OWN-TABLE ;


\ In a space the refusal comes before any lookup: a found address may sit above
\ a lowered DP.
: REWIND-REFUSE ( -- )
   SPACE? if
      data-base DPHW-CELL + @ 0 <> if E-INTERN-REWIND throw then
   then ;


\ The address cell comes first: the other cells mean nothing while it is 0.
: TABLE-CUT ( ptr n n -- )
   {: t:ptr limit:n :}
   t @ 0= if exit then
   t HIGHEST-CELL @ limit <= if exit then
   t  t SIZE-CELL @  limit REBUILD ;

public

\ The address of the body equal to a u in the current table, as its distance
\ from NULL-PTR (DP-CELL's units); 0 when none.
: FIND ( ptr u8 n -- n )
   {: a:ptr u:n :}
   REWIND-REFUSE
   TABLE {: t:ptr :}
   t  t a u SLOT-FOR  SLOT-CELL @ ;


: TEXT ( ptr u8 n -- n )
   {: a:ptr u:n :}
   a u FIND {: body:n :}
   body 0 <> if body exit then
   TABLE a u LAY ;


\ Forgets every text that ends above n (DP-CELL's units).
: KEEP-BELOW ( n -- )
   {: limit:n :}
   SPACE? if
      SPACE-IMAGE @ IMAGE-BASE <> if exit then
      SPACE-TABLE limit TABLE-CUT exit
   then
   OWN-TABLE limit TABLE-CUT ;

private

\ src/habu/intern.f STRING's entry while SPACE? holds.
: SPACE-STRING ( ptr u8 n -- n ) TEXT ;


\ src/habu/intern.f ADOPT's entry while SPACE? holds: puts a body the space's data
\ already holds into its table and answers 0; when the table holds its text, puts
\ nothing and answers that body.
: SPACE-ADOPT ( n -- n )
   {: body:n :}
   TABLE {: t:ptr :}
   t  t body BODY$ SLOT-FOR  SLOT-CELL @ {: held:n :}
   held 0 <> if held exit then
   t FILLED-CELL @ 1+ 2 *  t SIZE-CELL @ > if t WIDEN then
   t body SLOT-PUT
   0 ;

;package
