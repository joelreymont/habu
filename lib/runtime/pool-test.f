\ pool-test.f - RT-POOL through its load path: pools opened, shut and refused
\ by token, reserves charged once per slot, stale and dead handles, a 100,000
\ object chain reclaimed in bounded steps, an object released for good read
\ only by its running enumerator, quotas and ceilings that answer oom and
\ change nothing, the emergency pool kept apart, and the typed routes each
\ token refuses. The white-box rows at the end drive what no caller can reach
\ in a test: a count of 2^64 - 1, a slot's last generation, a refused mapping
\ whose chunks a child process cannot read, and a full schema registry.
\ Run: bin/hb --load lib/runtime/pool-test.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/test/outcome.f
require lib/runtime/handle.f
require lib/runtime/pool.f

package RPT

\ ---- fixtures ---------------------------------------------------------------

8 BUFFER: WIRE

\ The handle of a slot at a generation, through its eight wire bytes.
: >WIRE-HANDLE ( n n -- RT-HANDLE:handle )
   {: slot:n gen:n :}
   4 0 do slot i 8 * rshift $FF and WIRE i + c! loop
   4 0 do gen i 8 * rshift $FF and WIRE 4 + i + c! loop
   WIRE 8 RT-HANDLE:BYTES>HANDLE ;

\ A handle as one number, slot low and generation high, through the public readers.
: BITS ( RT-HANDLE:handle -- n )
   {: h:RT-HANDLE:handle :}
   h RT-HANDLE:GENERATION 32 lshift h RT-HANDLE:SLOT or ;

: SLOT# ( RT-HANDLE:handle -- n )   RT-HANDLE:SLOT ;
: GEN# ( RT-HANDLE:handle -- n )   RT-HANDLE:GENERATION ;

\ The granted handle, or the null one for oom.
: ID ( RT-POOL:reservation -- RT-HANDLE:handle )
   MATCH RT-POOL:reservation
      granted OF ENDOF
      oom OF RT-HANDLE:NULL ENDOF
   ;MATCH ;

: OOM? ( RT-POOL:reservation -- bool )
   MATCH RT-POOL:reservation
      granted OF drop false ENDOF
      oom OF true ENDOF
   ;MATCH ;

: PAGES ( -- RT-POOL:kind )
   RT--POOL-KIND:pages ;

: TRANSIENT ( -- RT-POOL:kind )
   RT--POOL-KIND:transient ;

: JOBS ( -- RT-POOL:kind )
   RT--POOL-KIND:jobs ;

: PACKETS ( -- RT-POOL:kind )
   RT--POOL-KIND:packets ;

: EMERGENCY ( -- RT-POOL:kind )
   RT--POOL-KIND:emergency ;

: NULL-CODE ( -- n )
   RT-POOL:E-RT-POOL-NULL ;

: SHUT-CODE ( -- n )
   RT-POOL:E-RT-POOL-SHUT ;

: DEAD ( -- n )
   RT-POOL:E-RT-POOL-DEAD ;

: STALE ( -- n )
   RT-HANDLE:E-RT-HANDLE-STALE ;

: FOREIGN ( -- n )
   RT-HANDLE:E-RT-HANDLE-FOREIGN ;

\ ---- schemas ----------------------------------------------------------------

\ Every enumerator counts its calls here.
variable EDGES

: NO-EDGE ( RT-HANDLE:handle n RT-POOL:pools -- RT-HANDLE:handle )
   {: h e:n p:RT-POOL:pools :}
   1 EDGES +!
   RT-HANDLE:NULL ;

\ A link's one child is in payload cell 0.
: LINK-EDGE ( RT-HANDLE:handle n RT-POOL:pools -- RT-HANDLE:handle )
   {: h e:n p:RT-POOL:pools :}
   1 EDGES +!
   e 0<> if RT-HANDLE:NULL exit then
   h 0 p RT-POOL:REF@ ;

\ A pair's children are in payload cells 0 and 1; its edges skip a null one.
: PAIR-EDGE ( RT-HANDLE:handle n RT-POOL:pools -- RT-HANDLE:handle )
   {: h e:n p:RT-POOL:pools :}
   1 EDGES +!
   h 0 p RT-POOL:REF@ {: a :}
   h 1 p RT-POOL:REF@ {: b :}
   a RT-HANDLE:NULL? if
      e 0= if b exit then
      RT-HANDLE:NULL exit
   then
   e 0= if a exit then
   e 1 = if b exit then
   RT-HANDLE:NULL ;

\ A trip has no child; its enumerator throws E-MEM-SIZE once TRIPS is set,
\ and clears it.
variable TRIPS

: TRIP-EDGE ( RT-HANDLE:handle n RT-POOL:pools -- RT-HANDLE:handle )
   {: h e:n p:RT-POOL:pools :}
   1 EDGES +!
   TRIPS @ 0<> if 0 TRIPS ! E-MEM-SIZE throw then
   RT-HANDLE:NULL ;

TYPED-VARIABLE LINK RT-POOL:schema
TYPED-VARIABLE PAIR RT-POOL:schema
TYPED-VARIABLE TRIP RT-POOL:schema
TYPED-VARIABLE PAGE RT-POOL:schema
TYPED-VARIABLE JOB RT-POOL:schema
TYPED-VARIABLE PACKET RT-POOL:schema
TYPED-VARIABLE DIAG RT-POOL:schema
\ Never written: the zero schema token.
TYPED-VARIABLE NO-SCHEMA RT-POOL:schema

: SCHEMAS ( -- )
   TRANSIENT [: LINK-EDGE ;] RT-POOL:SCHEMA LINK !
   TRANSIENT [: PAIR-EDGE ;] RT-POOL:SCHEMA PAIR !
   TRANSIENT [: TRIP-EDGE ;] RT-POOL:SCHEMA TRIP !
   PAGES [: NO-EDGE ;] RT-POOL:SCHEMA PAGE !
   JOBS [: NO-EDGE ;] RT-POOL:SCHEMA JOB !
   PACKETS [: NO-EDGE ;] RT-POOL:SCHEMA PACKET !
   EMERGENCY [: NO-EDGE ;] RT-POOL:SCHEMA DIAG ! ;

SCHEMAS

\ ---- ledgers ----------------------------------------------------------------

5 TYPED-BUFFER KINDS-OF RT-POOL:kind

: FILL-KINDS ( -- )
   PAGES 0 KINDS-OF !
   TRANSIENT 1 KINDS-OF !
   JOBS 2 KINDS-OF !
   PACKETS 3 KINDS-OF !
   EMERGENCY 4 KINDS-OF ! ;

FILL-KINDS

\ Each pool's charged and mapped bytes, as SNAPSHOT found them.
10 TYPED-BUFFER SNAP n

: SNAPSHOT ( RT-POOL:pools -- )
   {: p:RT-POOL:pools :}
   5 0 do
      i KINDS-OF @ p RT-POOL:CHARGED i 2 * SNAP !
      i KINDS-OF @ p RT-POOL:MAPPED i 2 * 1 + SNAP !
   loop ;

\ Whether every ledger of the pools is as SNAPSHOT found it.
: UNCHANGED? ( RT-POOL:pools -- bool )
   {: p:RT-POOL:pools :}
   true
   5 0 do
      i KINDS-OF @ p RT-POOL:CHARGED i 2 * SNAP @ = and
      i KINDS-OF @ p RT-POOL:MAPPED i 2 * 1 + SNAP @ = and
   loop ;

\ ---- lifecycle --------------------------------------------------------------

RT-POOL:POOLS-CELLS TYPED-BUFFER CELLS-A n
TYPED-VARIABLE PA RT-POOL:pools
\ Never written: the zero pools token.
TYPED-VARIABLE PZ RT-POOL:pools
\ The pools REFUSED-ALL tries.
TYPED-VARIABLE PQ RT-POOL:pools

\ Every word given PQ's pools is refused by the code before it reads a cell.
: REFUSED-ALL ( n -- )
   {: code:n :}
   [: PQ @ RT-POOL:SHUTDOWN ;] code TTHROWSQ
   [: LINK @ PQ @ RT-POOL:RESERVE OOM? drop ;] code TTHROWSQ
   [: DIAG @ PQ @ RT-POOL:RESERVE-EMERGENCY OOM? drop ;] code TTHROWSQ
   [: 1 1 >WIRE-HANDLE PQ @ RT-POOL:RETAIN ;] code TTHROWSQ
   [: 1 1 >WIRE-HANDLE PQ @ RT-POOL:RELEASE ;] code TTHROWSQ
   [: 1 1 >WIRE-HANDLE 0 PQ @ RT-POOL:CELL@ drop ;] code TTHROWSQ
   [: 0 1 1 >WIRE-HANDLE 0 PQ @ RT-POOL:CELL! ;] code TTHROWSQ
   [: 1 1 >WIRE-HANDLE 0 PQ @ RT-POOL:REF@ drop ;] code TTHROWSQ
   [: RT-HANDLE:NULL 1 1 >WIRE-HANDLE 0 PQ @ RT-POOL:REF! ;] code TTHROWSQ
   [: 1 PQ @ RT-POOL:RECLAIM-STEP drop ;] code TTHROWSQ
   [: 0 PAGES PQ @ RT-POOL:QUOTA! ;] code TTHROWSQ
   [: PAGES PQ @ RT-POOL:CHARGED drop ;] code TTHROWSQ
   [: PAGES PQ @ RT-POOL:MAPPED drop ;] code TTHROWSQ ;

: LIFECYCLE ( -- )
   0 CELLS-A RT-POOL:INIT PA !
   s" INIT maps the whole emergency pool and no other chunk" T-LABEL
   EMERGENCY PA @ RT-POOL:MAPPED RT-POOL:EMERGENCY-CEILING T=
   PAGES PA @ RT-POOL:MAPPED 0 T=
   TRANSIENT PA @ RT-POOL:MAPPED 0 T=
   JOBS PA @ RT-POOL:MAPPED 0 T=
   PACKETS PA @ RT-POOL:MAPPED 0 T=
   s" and charges nothing" T-LABEL
   EMERGENCY PA @ RT-POOL:CHARGED 0 T=
   PAGES PA @ RT-POOL:CHARGED 0 T=
   s" cells that hold open pools are not opened again" T-LABEL
   [: 0 CELLS-A RT-POOL:INIT drop ;] RT-POOL:E-RT-POOL-HELD TTHROWSQ
   s" and the pools there go on" T-LABEL
   EMERGENCY PA @ RT-POOL:MAPPED RT-POOL:EMERGENCY-CEILING T=
   PA @ RT-POOL:SHUTDOWN
   s" pools SHUTDOWN has closed refuse every word" T-LABEL
   PA @ PQ !
   SHUT-CODE REFUSED-ALL
   s" and their cells are never opened again" T-LABEL
   [: 0 CELLS-A RT-POOL:INIT drop ;] RT-POOL:E-RT-POOL-HELD TTHROWSQ ;

: UNOPENED ( -- )
   s" the zero pools token is refused by every word" T-LABEL
   PZ @ PQ !
   NULL-CODE REFUSED-ALL
   s" and null cells by INIT" T-LABEL
   [: NULL-PTR RT-POOL:INIT drop ;] NULL-CODE TTHROWSQ ;

\ ---- reserving and references -----------------------------------------------

RT-POOL:POOLS-CELLS TYPED-BUFFER CELLS-B n
TYPED-VARIABLE PB RT-POOL:pools
TYPED-VARIABLE H-A RT-HANDLE:handle
TYPED-VARIABLE H-B RT-HANDLE:handle
TYPED-VARIABLE H-C RT-HANDLE:handle

: LAST-CELL ( -- n )
   TRANSIENT RT-POOL:PAYLOAD-CELLS 1 - ;

: FIRST-RESERVES ( -- )
   0 CELLS-B RT-POOL:INIT PB !
   LINK @ PB @ RT-POOL:RESERVE ID H-A !
   s" a reserve maps its pool one chunk" T-LABEL
   TRANSIENT PB @ RT-POOL:MAPPED RT-POOL:CHUNK-BYTES T=
   s" and charges its pool one slot" T-LABEL
   TRANSIENT PB @ RT-POOL:CHARGED 128 T=
   PAGES PB @ RT-POOL:CHARGED 0 T=
   s" an object's handle names its slot at generation 1" T-LABEL
   H-A @ GEN# 1 T=
   PAGE @ PB @ RT-POOL:RESERVE ID H-C !
   s" the first page is slot 1, the transient pool's slots lie past the pages" T-LABEL
   H-C @ SLOT# 1 T=
   H-A @ SLOT# H-C @ SLOT# > TTRUE
   LINK @ PB @ RT-POOL:RESERVE ID H-B !
   s" a pool issues its slots in order, all from its one chunk" T-LABEL
   H-B @ SLOT# H-A @ SLOT# 1 + T=
   TRANSIENT PB @ RT-POOL:MAPPED RT-POOL:CHUNK-BYTES T=
   s" a payload starts zeroed" T-LABEL
   H-A @ 0 PB @ RT-POOL:CELL@ 0 T=
   H-A @ LAST-CELL PB @ RT-POOL:CELL@ 0 T=
   s" a payload cell holds what CELL! wrote" T-LABEL
   77 H-A @ LAST-CELL PB @ RT-POOL:CELL!
   H-A @ LAST-CELL PB @ RT-POOL:CELL@ 77 T=
   s" and the next slot's first cell is its own" T-LABEL
   H-B @ 0 PB @ RT-POOL:CELL@ 0 T=
   s" a cell outside the payload is refused" T-LABEL
   [: H-A @ -1 PB @ RT-POOL:CELL@ drop ;] RT-POOL:E-RT-POOL-CELL TTHROWSQ
   [: H-A @ LAST-CELL 1 + PB @ RT-POOL:CELL@ drop ;] RT-POOL:E-RT-POOL-CELL TTHROWSQ
   [: 1 H-A @ LAST-CELL 1 + PB @ RT-POOL:CELL! ;] RT-POOL:E-RT-POOL-CELL TTHROWSQ
   [: H-A @ LAST-CELL 1 + PB @ RT-POOL:REF@ drop ;] RT-POOL:E-RT-POOL-CELL TTHROWSQ
   [: H-B @ H-A @ LAST-CELL 1 + PB @ RT-POOL:REF! ;] RT-POOL:E-RT-POOL-CELL TTHROWSQ
   s" and so is a schema SCHEMA never answered" T-LABEL
   [: NO-SCHEMA @ PB @ RT-POOL:RESERVE OOM? drop ;] RT-POOL:E-RT-POOL-SCHEMA TTHROWSQ
   [: NO-SCHEMA @ PB @ RT-POOL:RESERVE-EMERGENCY OOM? drop ;] RT-POOL:E-RT-POOL-SCHEMA TTHROWSQ ;

\ H-A is held once and holds 77 in its last cell; H-B is held once.
: REFERENCES ( -- )
   H-A @ PB @ RT-POOL:RETAIN
   H-A @ PB @ RT-POOL:RETAIN
   H-A @ PB @ RT-POOL:RETAIN
   s" an object held four times is charged once" T-LABEL
   TRANSIENT PB @ RT-POOL:CHARGED 256 T=
   H-A @ PB @ RT-POOL:RELEASE
   H-A @ PB @ RT-POOL:RELEASE
   H-A @ PB @ RT-POOL:RELEASE
   s" and stays held until its last release" T-LABEL
   H-A @ LAST-CELL PB @ RT-POOL:CELL@ 77 T=
   0 EDGES !
   H-A @ PB @ RT-POOL:RELEASE
   s" which visits nothing" T-LABEL
   EDGES @ 0 T=
   s" an object released for good is not released again" T-LABEL
   [: H-A @ PB @ RT-POOL:RELEASE ;] DEAD TTHROWSQ
   s" nor retained, read or written" T-LABEL
   [: H-A @ PB @ RT-POOL:RETAIN ;] DEAD TTHROWSQ
   [: H-A @ 0 PB @ RT-POOL:CELL@ drop ;] DEAD TTHROWSQ
   [: 1 H-A @ 0 PB @ RT-POOL:CELL! ;] DEAD TTHROWSQ
   [: H-A @ 0 PB @ RT-POOL:REF@ drop ;] DEAD TTHROWSQ
   [: RT-HANDLE:NULL H-A @ 0 PB @ RT-POOL:REF! ;] DEAD TTHROWSQ
   s" nor held in another object, whose cell keeps what it held" T-LABEL
   [: H-A @ H-B @ 0 PB @ RT-POOL:REF! ;] DEAD TTHROWSQ
   H-B @ 0 PB @ RT-POOL:REF@ RT-HANDLE:NULL? TTRUE
   s" it is still charged until it is reclaimed" T-LABEL
   TRANSIENT PB @ RT-POOL:CHARGED 256 T=
   s" reclaiming it visits its one edge, past its last child" T-LABEL
   8 PB @ RT-POOL:RECLAIM-STEP 1 T=
   EDGES @ 1 T=
   TRANSIENT PB @ RT-POOL:CHARGED 128 T=
   s" an empty queue answers no edge" T-LABEL
   8 PB @ RT-POOL:RECLAIM-STEP 0 T=
   s" a reclaimed object's handle is stale" T-LABEL
   [: H-A @ PB @ RT-POOL:RELEASE ;] STALE TTHROWSQ
   [: H-A @ PB @ RT-POOL:RETAIN ;] STALE TTHROWSQ
   [: H-A @ 0 PB @ RT-POOL:CELL@ drop ;] STALE TTHROWSQ
   [: H-A @ 0 PB @ RT-POOL:REF@ drop ;] STALE TTHROWSQ
   s" its slot comes back at the next generation, zeroed" T-LABEL
   LINK @ PB @ RT-POOL:RESERVE ID H-C !
   H-C @ SLOT# H-A @ SLOT# T=
   H-C @ GEN# 2 T=
   H-C @ LAST-CELL PB @ RT-POOL:CELL@ 0 T=
   s" while the handle it replaced stays stale" T-LABEL
   [: H-A @ PB @ RT-POOL:RETAIN ;] STALE TTHROWSQ
   H-C @ PB @ RT-POOL:RELEASE
   8 PB @ RT-POOL:RECLAIM-STEP 1 T= ;

: FORGED ( -- )
   s" the null handle is foreign to the pools" T-LABEL
   [: RT-HANDLE:NULL PB @ RT-POOL:RETAIN ;] FOREIGN TTHROWSQ
   [: RT-HANDLE:NULL 0 PB @ RT-POOL:CELL@ drop ;] FOREIGN TTHROWSQ
   s" and so is a slot past every pool" T-LABEL
   [: $FFFFFFFF 1 >WIRE-HANDLE PB @ RT-POOL:RELEASE ;] FOREIGN TTHROWSQ
   s" a slot never issued is stale, its chunk never mapped" T-LABEL
   [: 100 1 >WIRE-HANDLE PB @ RT-POOL:RETAIN ;] STALE TTHROWSQ
   [: H-B @ SLOT# 1 + 1 >WIRE-HANDLE 0 PB @ RT-POOL:CELL@ drop ;] STALE TTHROWSQ
   s" and so is a live slot at another generation" T-LABEL
   [: H-B @ SLOT# 2 >WIRE-HANDLE PB @ RT-POOL:RETAIN ;] STALE TTHROWSQ
   s" while its own generation is live" T-LABEL
   H-B @ SLOT# 1 >WIRE-HANDLE 0 PB @ RT-POOL:CELL@ 0 T= ;

: BUDGETS ( -- )
   s" a step's budget is at least one edge" T-LABEL
   [: 0 PB @ RT-POOL:RECLAIM-STEP drop ;] RT-POOL:E-RT-POOL-BUDGET TTHROWSQ
   [: -1 PB @ RT-POOL:RECLAIM-STEP drop ;] RT-POOL:E-RT-POOL-BUDGET TTHROWSQ
   1 PB @ RT-POOL:RECLAIM-STEP 0 T= ;

\ ---- a long chain -----------------------------------------------------------

100000 constant CHAIN
64 constant STEP-BUDGET

RT-POOL:POOLS-CELLS TYPED-BUFFER CELLS-C n
TYPED-VARIABLE PC RT-POOL:pools
TYPED-VARIABLE ROOT RT-HANDLE:handle
TYPED-VARIABLE SECOND RT-HANDLE:handle
TYPED-VARIABLE PREV RT-HANDLE:handle
TYPED-VARIABLE NEW RT-HANDLE:handle
variable STEPS
variable OVERS
variable MISCOUNTS

\ CHAIN links, each held only by the one before it, ROOT by the test.
: BUILD-CHAIN ( -- )
   LINK @ PC @ RT-POOL:RESERVE ID ROOT !
   ROOT @ PREV !
   CHAIN 1 do
      LINK @ PC @ RT-POOL:RESERVE ID NEW !
      NEW @ PREV @ 0 PC @ RT-POOL:REF!
      NEW @ PC @ RT-POOL:RELEASE
      NEW @ PREV !
   loop
   ROOT @ 0 PC @ RT-POOL:REF@ SECOND ! ;

\ Step until a step answers less than its budget, counting the steps, those
\ that answered more than the budget and those whose answer was not the edges
\ the enumerators saw.
: RECLAIM-ALL ( -- )
   0 STEPS ! 0 OVERS ! 0 MISCOUNTS !
   begin
      0 EDGES !
      STEP-BUDGET PC @ RT-POOL:RECLAIM-STEP
      dup STEP-BUDGET > if 1 OVERS +! then
      dup EDGES @ <> if 1 MISCOUNTS +! then
      1 STEPS +!
      STEP-BUDGET <
   until ;

\ A link costs two edges, its child and the end past it, and the last link
\ one, so the chain is 2 * CHAIN - 1 edges.
: CHAIN-RECLAIM ( -- )
   0 CELLS-C RT-POOL:INIT PC !
   BUILD-CHAIN
   s" a chain of 100,000 objects is charged a slot each" T-LABEL
   TRANSIENT PC @ RT-POOL:CHARGED CHAIN 128 * T=
   0 EDGES !
   ROOT @ PC @ RT-POOL:RELEASE
   s" releasing its root visits no edge and frees nothing" T-LABEL
   EDGES @ 0 T=
   TRANSIENT PC @ RT-POOL:CHARGED CHAIN 128 * T=
   s" and leaves the root's child held" T-LABEL
   SECOND @ 0 PC @ RT-POOL:REF@ RT-HANDLE:NULL? TFALSE
   s" one step reclaims what its budget covers and no more" T-LABEL
   0 EDGES !
   STEP-BUDGET PC @ RT-POOL:RECLAIM-STEP STEP-BUDGET T=
   EDGES @ STEP-BUDGET T=
   TRANSIENT PC @ RT-POOL:CHARGED CHAIN STEP-BUDGET 2 / - 128 * T=
   RECLAIM-ALL
   s" the rest takes many steps, none over its budget" T-LABEL
   STEPS @ 1 + 2 CHAIN * 1 - STEP-BUDGET 1 - + STEP-BUDGET / T=
   OVERS @ 0 T=
   s" each answering the edges its enumerators visited" T-LABEL
   MISCOUNTS @ 0 T=
   s" until every link is reclaimed" T-LABEL
   TRANSIENT PC @ RT-POOL:CHARGED 0 T=
   STEP-BUDGET PC @ RT-POOL:RECLAIM-STEP 0 T=
   [: ROOT @ PC @ RT-POOL:RETAIN ;] STALE TTHROWSQ
   [: SECOND @ PC @ RT-POOL:RETAIN ;] STALE TTHROWSQ
   [: NEW @ PC @ RT-POOL:RETAIN ;] STALE TTHROWSQ
   s" and its chunks stay mapped" T-LABEL
   TRANSIENT PC @ RT-POOL:MAPPED CHAIN 128 * RT-POOL:CHUNK-BYTES 1 - + RT-POOL:CHUNK-BYTES / RT-POOL:CHUNK-BYTES * T=
   PC @ RT-POOL:SHUTDOWN ;

\ ---- shared children --------------------------------------------------------

RT-POOL:POOLS-CELLS TYPED-BUFFER CELLS-D n
TYPED-VARIABLE PD RT-POOL:pools
TYPED-VARIABLE H-X RT-HANDLE:handle
TYPED-VARIABLE H-Y RT-HANDLE:handle
TYPED-VARIABLE H-Z RT-HANDLE:handle

: RESERVE-PAIR ( -- RT-HANDLE:handle )
   PAIR @ PD @ RT-POOL:RESERVE ID ;

: RESERVE-LINK ( -- RT-HANDLE:handle )
   LINK @ PD @ RT-POOL:RESERVE ID ;

\ H-X and H-Y both hold H-Z.
: SHARED ( -- )
   0 CELLS-D RT-POOL:INIT PD !
   RESERVE-PAIR H-X !
   RESERVE-PAIR H-Y !
   RESERVE-LINK H-Z !
   H-Z @ H-X @ 0 PD @ RT-POOL:REF!
   H-Z @ H-Y @ 1 PD @ RT-POOL:REF!
   H-Z @ PD @ RT-POOL:RELEASE
   s" a cell REF! filled holds the child" T-LABEL
   H-X @ 0 PD @ RT-POOL:REF@ BITS H-Z @ BITS T=
   H-Y @ 1 PD @ RT-POOL:REF@ BITS H-Z @ BITS T=
   s" a child two parents hold outlives the first" T-LABEL
   H-X @ PD @ RT-POOL:RELEASE
   16 PD @ RT-POOL:RECLAIM-STEP 2 T=
   H-Z @ 0 PD @ RT-POOL:CELL@ 0 T=
   TRANSIENT PD @ RT-POOL:CHARGED 2 128 * T=
   s" and goes with the second" T-LABEL
   H-Y @ PD @ RT-POOL:RELEASE
   16 PD @ RT-POOL:RECLAIM-STEP 3 T=
   [: H-Z @ PD @ RT-POOL:RETAIN ;] STALE TTHROWSQ
   TRANSIENT PD @ RT-POOL:CHARGED 0 T= ;

\ REF! over a cell that holds a child releases that child.
: REPLACED ( -- )
   RESERVE-PAIR H-X !
   RESERVE-LINK H-Y !
   RESERVE-LINK H-Z !
   H-Y @ H-X @ 0 PD @ RT-POOL:REF!
   H-Z @ H-X @ 0 PD @ RT-POOL:REF!
   s" a child replaced in its cell is released" T-LABEL
   H-Y @ PD @ RT-POOL:RELEASE
   16 PD @ RT-POOL:RECLAIM-STEP 1 T=
   [: H-Y @ PD @ RT-POOL:RETAIN ;] STALE TTHROWSQ
   s" and the cell holds its replacement" T-LABEL
   H-X @ 0 PD @ RT-POOL:REF@ BITS H-Z @ BITS T=
   s" storing the null handle releases the child too" T-LABEL
   RT-HANDLE:NULL H-X @ 0 PD @ RT-POOL:REF!
   H-X @ 0 PD @ RT-POOL:REF@ RT-HANDLE:NULL? TTRUE
   H-Z @ PD @ RT-POOL:RELEASE
   16 PD @ RT-POOL:RECLAIM-STEP 1 T=
   s" a child stored again in its own cell stays held" T-LABEL
   RESERVE-LINK H-Y !
   H-Y @ H-X @ 1 PD @ RT-POOL:REF!
   H-Y @ H-X @ 1 PD @ RT-POOL:REF!
   H-Y @ PD @ RT-POOL:RELEASE
   16 PD @ RT-POOL:RECLAIM-STEP 0 T=
   H-Y @ 0 PD @ RT-POOL:CELL@ 0 T=
   H-X @ PD @ RT-POOL:RELEASE
   16 PD @ RT-POOL:RECLAIM-STEP 3 T=
   TRANSIENT PD @ RT-POOL:CHARGED 0 T= ;

\ H-X holds H-Y, both released, and a step of one edge stops inside H-X.
: BETWEEN-STEPS ( -- )
   RESERVE-PAIR H-X !
   RESERVE-LINK H-Y !
   H-Y @ H-X @ 0 PD @ RT-POOL:REF!
   H-Y @ PD @ RT-POOL:RELEASE
   H-X @ PD @ RT-POOL:RELEASE
   1 PD @ RT-POOL:RECLAIM-STEP 1 T=
   s" an object a step stopped inside is not read between steps" T-LABEL
   [: H-X @ 0 PD @ RT-POOL:CELL@ drop ;] DEAD TTHROWSQ
   [: H-X @ 0 PD @ RT-POOL:REF@ drop ;] DEAD TTHROWSQ
   s" while its enumerator reads it at the next" T-LABEL
   16 PD @ RT-POOL:RECLAIM-STEP 2 T=
   TRANSIENT PD @ RT-POOL:CHARGED 0 T= ;

\ H-Z is a trip released for good, whose enumerator throws once.
: THROWN ( -- )
   TRIP @ PD @ RT-POOL:RESERVE ID H-Z !
   H-Z @ PD @ RT-POOL:RELEASE
   1 TRIPS !
   s" an enumerator's throw goes on out of the step" T-LABEL
   [: 8 PD @ RT-POOL:RECLAIM-STEP drop ;] E-MEM-SIZE TTHROWSQ
   s" and leaves its object unreadable" T-LABEL
   [: H-Z @ 0 PD @ RT-POOL:CELL@ drop ;] DEAD TTHROWSQ
   s" the next step visits that edge again and reclaims the object" T-LABEL
   8 PD @ RT-POOL:RECLAIM-STEP 1 T=
   [: H-Z @ 0 PD @ RT-POOL:CELL@ drop ;] STALE TTHROWSQ
   PD @ RT-POOL:SHUTDOWN ;

\ ---- quotas and ceilings ----------------------------------------------------

RT-POOL:POOLS-CELLS TYPED-BUFFER CELLS-E n
RT-POOL:POOLS-CELLS TYPED-BUFFER CELLS-F n
RT-POOL:POOLS-CELLS TYPED-BUFFER CELLS-G n
RT-POOL:POOLS-CELLS TYPED-BUFFER CELLS-H n
TYPED-VARIABLE PE RT-POOL:pools
variable GRANTS

\ Reserve from the schema until a reserve answers oom, counting the grants.
: FILL ( RT-POOL:schema -- )
   {: sc:RT-POOL:schema :}
   0 GRANTS !
   begin sc PE @ RT-POOL:RESERVE OOM? 0= while 1 GRANTS +! repeat ;

: QUOTAS ( -- )
   0 CELLS-E RT-POOL:INIT PE !
   2 4096 * PAGES PE @ RT-POOL:QUOTA!
   PAGE @ PE @ RT-POOL:RESERVE ID H-A !
   PAGE @ PE @ RT-POOL:RESERVE ID drop
   PE @ SNAPSHOT
   s" a reserve over its pool's quota answers oom" T-LABEL
   PAGE @ PE @ RT-POOL:RESERVE OOM? TTRUE
   s" and leaves every ledger unchanged" T-LABEL
   PE @ UNCHANGED? TTRUE
   s" and every slot: the next page granted is the third" T-LABEL
   RT-POOL:PAGES-CEILING PAGES PE @ RT-POOL:QUOTA!
   PAGE @ PE @ RT-POOL:RESERVE ID SLOT# 3 T=
   s" a quota below the charge holds every reserve" T-LABEL
   0 PAGES PE @ RT-POOL:QUOTA!
   PE @ SNAPSHOT
   PAGE @ PE @ RT-POOL:RESERVE OOM? TTRUE
   PE @ UNCHANGED? TTRUE
   s" until a release is reclaimed, which a quota does not hold" T-LABEL
   H-A @ PE @ RT-POOL:RELEASE
   8 PE @ RT-POOL:RECLAIM-STEP 1 T=
   PAGES PE @ RT-POOL:CHARGED 2 4096 * T=
   s" a quota runs from zero to its class's ceiling" T-LABEL
   [: -1 PAGES PE @ RT-POOL:QUOTA! ;] RT-POOL:E-RT-POOL-QUOTA TTHROWSQ
   [: RT-POOL:PAGES-CEILING 1 + PAGES PE @ RT-POOL:QUOTA! ;] RT-POOL:E-RT-POOL-QUOTA TTHROWSQ
   [: RT-POOL:SCRATCH-CEILING 1 + JOBS PE @ RT-POOL:QUOTA! ;] RT-POOL:E-RT-POOL-QUOTA TTHROWSQ
   RT-POOL:SCRATCH-CEILING JOBS PE @ RT-POOL:QUOTA!
   PE @ RT-POOL:SHUTDOWN ;

\ The packets pool alone in its 16 MiB class.
: CEILINGS ( -- )
   0 CELLS-F RT-POOL:INIT PE !
   PACKET @ PE @ RT-POOL:RESERVE ID H-A !
   PACKET @ FILL
   s" a pool grows a chunk at a time to its class's ceiling" T-LABEL
   PACKETS PE @ RT-POOL:MAPPED RT-POOL:INGRESS-CEILING T=
   PACKETS PE @ RT-POOL:CHARGED RT-POOL:INGRESS-CEILING T=
   GRANTS @ 1 + RT-POOL:INGRESS-CEILING 4096 / T=
   PE @ SNAPSHOT
   s" a reserve past it answers oom and leaves every ledger unchanged" T-LABEL
   PACKET @ PE @ RT-POOL:RESERVE OOM? TTRUE
   PE @ UNCHANGED? TTRUE
   s" a reclaimed slot is reserved again, its chunk still mapped" T-LABEL
   H-A @ PE @ RT-POOL:RELEASE
   8 PE @ RT-POOL:RECLAIM-STEP 1 T=
   PACKETS PE @ RT-POOL:MAPPED RT-POOL:INGRESS-CEILING T=
   PACKET @ PE @ RT-POOL:RESERVE ID SLOT# H-A @ SLOT# T=
   PE @ RT-POOL:SHUTDOWN ;

\ Job states fill the 24 MiB class they share with transient storage.
: SHARED-CLASS ( -- )
   0 CELLS-G RT-POOL:INIT PE !
   JOB @ FILL
   s" job states take the whole of the class they share" T-LABEL
   JOBS PE @ RT-POOL:MAPPED RT-POOL:SCRATCH-CEILING T=
   PE @ SNAPSHOT
   s" so transient storage answers oom within its own quota" T-LABEL
   LINK @ PE @ RT-POOL:RESERVE OOM? TTRUE
   PE @ UNCHANGED? TTRUE
   TRANSIENT PE @ RT-POOL:CHARGED 0 T=
   s" and the emergency pool is left alone" T-LABEL
   EMERGENCY PE @ RT-POOL:MAPPED RT-POOL:EMERGENCY-CEILING T=
   EMERGENCY PE @ RT-POOL:CHARGED 0 T=
   PE @ RT-POOL:SHUTDOWN ;

\ ---- the emergency pool -----------------------------------------------------

variable DIAGS

: EMERGENCIES ( -- )
   0 CELLS-H RT-POOL:INIT PE !
   s" RESERVE never takes from the emergency pool" T-LABEL
   [: DIAG @ PE @ RT-POOL:RESERVE OOM? drop ;] RT-POOL:E-RT-POOL-EMERGENCY TTHROWSQ
   s" and RESERVE-EMERGENCY takes only from it" T-LABEL
   [: LINK @ PE @ RT-POOL:RESERVE-EMERGENCY OOM? drop ;] RT-POOL:E-RT-POOL-EMERGENCY TTHROWSQ
   TRANSIENT PE @ RT-POOL:MAPPED 0 T=
   s" from the chunks INIT mapped, never more" T-LABEL
   0 DIAGS !
   begin DIAG @ PE @ RT-POOL:RESERVE-EMERGENCY OOM? 0= while 1 DIAGS +! repeat
   DIAGS @ RT-POOL:EMERGENCY-CEILING 128 / T=
   EMERGENCY PE @ RT-POOL:MAPPED RT-POOL:EMERGENCY-CEILING T=
   EMERGENCY PE @ RT-POOL:CHARGED RT-POOL:EMERGENCY-CEILING T=
   PE @ RT-POOL:SHUTDOWN ;

\ ---- routes from a number ---------------------------------------------------

4096 BUFFER: CHECK-TEXT

\ The checker refuses the candidate, and its diagnostic names the types.
: REFUSED ( ptr u8 n ptr u8 n -- )
   {: src:ptr srcu:n want:ptr wantu:n :}
   CHECK-TEXT 4096 DIAG-BUFFER!
   src srcu CHECK-CANDIDATE! 0 T=
   DIAG-BUFFER$ want wantu CONTAINS? TTRUE
   DIAG-BUFFER-OFF ;

: CERTIFIED ( ptr u8 n -- )
   CHECK-QUIET-CANDIDATE! -1 T= ;

: CHECKER ( -- )
   s" a number where pools are expected is refused" T-LABEL
   s" RPT-N-AS-POOLS ( RT-POOL:kind n -- n ) RT-POOL:MAPPED"
   s" actual: rt-pool:kind n" REFUSED
   s" while pools there certify" T-LABEL
   s" RPT-POOLS-AS-POOLS ( RT-POOL:kind RT-POOL:pools -- n ) RT-POOL:MAPPED" CERTIFIED
   s" a number where a schema is expected is refused" T-LABEL
   s" RPT-N-AS-SCHEMA ( n RT-POOL:pools -- RT-POOL:reservation ) RT-POOL:RESERVE"
   s" actual: n rt-pool:pools" REFUSED
   s" a handle where pools are expected is refused" T-LABEL
   s" RPT-HANDLE-AS-POOLS ( RT-HANDLE:handle RT-HANDLE:handle -- ) RT-POOL:RELEASE"
   s" actual: rt-handle:handle rt-handle:handle" REFUSED ;

\ A cast is declared at top level, where the text runs.
: CASTS ( -- )
   s" no cast turns a number or cells into pools outside RT-POOL" T-LABEL
   s" CAST: RPT-FORGE-POOLS ( n -- RT-POOL:pools )" TEST-EVAL:RC E-CAST-OWNER T=
   s" CAST: RPT-POOLS-AT ( ptr n -- RT-POOL:pools )" TEST-EVAL:RC E-CAST-OWNER T=
   s" nor a number into a schema" T-LABEL
   s" CAST: RPT-FORGE-SCHEMA ( n -- RT-POOL:schema )" TEST-EVAL:RC E-CAST-OWNER T= ;

$400 constant CHILD-CAP
10000 constant CHILD-MS
CHILD-CAP BUFFER: CHILD-OUT
CHILD-CAP BUFFER: CHILD-ERR

\ A forked child of this image loads the text and exits 70, having named the
\ word undefined.
: UNDEFINED ( ptr u8 n ptr u8 n -- )
   {: src:ptr srcu:n want:ptr wantu:n :}
   src srcu CHILD-OUT CHILD-CAP >LEN CHILD-ERR CHILD-CAP >LEN CHILD-MS >MS SUBJECT:RUN
   {: outu:len erru:len oc :}
   src srcu CHILD-OUT outu LEN>N CHILD-ERR erru LEN>N oc CHECKER-REJECT-RC T-OUTCOME-EXITED=
   CHILD-ERR erru LEN>N want wantu CONTAINS? TTRUE ;

: CONVERTERS ( -- )
   s" the converters of pools are undefined outside RT-POOL" T-LABEL
   s" : RPT-U1 ( ptr n -- RT-POOL:pools ) RT-POOL:>POOLS ;" s" E-UNDEFINED: RT-POOL:>POOLS" UNDEFINED
   s" : RPT-U2 ( RT-POOL:pools -- n ) RT-POOL:POOLS>N ;" s" E-UNDEFINED: RT-POOL:POOLS>N" UNDEFINED
   s" and so are a schema's" T-LABEL
   s" : RPT-U3 ( n -- RT-POOL:schema ) RT-POOL:>SCHEMA ;" s" E-UNDEFINED: RT-POOL:>SCHEMA" UNDEFINED
   s" : RPT-U4 ( RT-POOL:schema -- n ) RT-POOL:SCHEMA>N ;" s" E-UNDEFINED: RT-POOL:SCHEMA>N" UNDEFINED
   s" and the constructor of the pools' state" T-LABEL
   s" : RPT-U5 ( -- ) 0 0 0 0 0 RT--POOL-STATE:MAKE drop ;" s" E-UNDEFINED: RT--POOL-STATE:MAKE" UNDEFINED ;

: MAIN ( -- )
   T-RESET
   LIFECYCLE
   UNOPENED
   FIRST-RESERVES
   REFERENCES
   FORGED
   BUDGETS
   PB @ RT-POOL:SHUTDOWN
   CHAIN-RECLAIM
   SHARED
   REPLACED
   BETWEEN-STEPS
   THROWN
   QUOTAS
   CEILINGS
   SHARED-CLASS
   EMERGENCIES
   CHECKER
   CASTS
   CONVERTERS ;

MAIN

;package

\ ---- white-box --------------------------------------------------------------
\ What no caller reaches in a test: a count of 2^64 - 1, a slot's last
\ generation and a mapping the OS refuses. Only RT-POOL writes a header or maps
\ a chunk, so these rows run inside it.
package RT-POOL

POOLS-CELLS TYPED-BUFFER RPT-CELLS-W n
POOLS-CELLS TYPED-BUFFER RPT-CELLS-I n
TYPED-VARIABLE RPT-PW pools
TYPED-VARIABLE RPT-SW schema
TYPED-VARIABLE RPT-SP schema
TYPED-VARIABLE RPT-H RT-HANDLE:handle
TYPED-VARIABLE RPT-H2 RT-HANDLE:handle
variable RPT-S
variable RPT-MAPS

: RPT-NO-EDGE ( RT-HANDLE:handle n pools -- RT-HANDLE:handle )
   {: h e:n p:pools :}
   RT-HANDLE:NULL ;

: RPT-SCHEMAS ( -- )
   RT--POOL-KIND:transient [: RPT-NO-EDGE ;] SCHEMA RPT-SW !
   RT--POOL-KIND:pages [: RPT-NO-EDGE ;] SCHEMA RPT-SP ! ;

RPT-SCHEMAS

: RPT-ID ( reservation -- RT-HANDLE:handle )
   MATCH reservation
      granted OF ENDOF
      oom OF RT-HANDLE:NULL ENDOF
   ;MATCH ;

: RPT-OOM? ( reservation -- bool )
   MATCH reservation
      granted OF drop false ENDOF
      oom OF true ENDOF
   ;MATCH ;

\ The header of the slot of RPT-PW's pools the handle's bits name, live or not.
: RPT-HEAD ( RT-HANDLE:handle -- ptr head )
   HANDLE>BITS RPT-PW @ OPEN-CELLS {: v:n cs:ptr :}
   v cs WHERE cs SLOT-AT >HEAD ;

: RPT-OVERFLOW ( -- )
   0 RPT-CELLS-W INIT RPT-PW !
   RPT-SW @ RPT-PW @ RESERVE RPT-ID RPT-H !
   LAST-COUNT RPT-H @ RPT-HEAD HEAD-COUNT !
   s" an object held 2^64 - 1 times is not retained again" T-LABEL
   [: RPT-H @ RPT-PW @ RETAIN ;] E-RT-POOL-OVERFLOW TTHROWSQ
   s" and its count does not wrap" T-LABEL
   RPT-H @ RPT-HEAD HEAD-COUNT @ LAST-COUNT T=
   s" it is still released" T-LABEL
   RPT-H @ RPT-PW @ RELEASE
   RPT-H @ RPT-HEAD HEAD-COUNT @ LAST-COUNT 1 - T= ;

TYPED-VARIABLE RPT-PARENT RT-HANDLE:handle
TYPED-VARIABLE RPT-CHILD RT-HANDLE:handle

\ The child's count is driven to 2^64 - 1 while the parent's cell holds it.
: RPT-SAME-REF ( -- )
   RPT-SW @ RPT-PW @ RESERVE RPT-ID RPT-PARENT !
   RPT-SW @ RPT-PW @ RESERVE RPT-ID RPT-CHILD !
   RPT-CHILD @ RPT-PARENT @ 0 RPT-PW @ REF!
   LAST-COUNT RPT-CHILD @ RPT-HEAD HEAD-COUNT !
   s" REF! of the child its cell holds retains nothing, at 2^64 - 1 too" T-LABEL
   [: RPT-CHILD @ RPT-PARENT @ 0 RPT-PW @ REF! ;] 0 TTHROWSQ
   RPT-CHILD @ RPT-HEAD HEAD-COUNT @ LAST-COUNT T=
   s" and the cell still holds the child" T-LABEL
   RPT-PARENT @ 0 RPT-PW @ REF@ HANDLE>BITS RPT-CHILD @ HANDLE>BITS T= ;

\ The generations a slot runs through are driven to its last by writing the
\ free slot's stamp: generation 2^32 - 2.
: RPT-RETIRE ( -- )
   RPT-SW @ RPT-PW @ RESERVE RPT-ID RPT-H2 !
   RPT-H2 @ RPT-PW @ RELEASE
   4 RPT-PW @ RECLAIM-STEP 1 T=
   LAST-GEN 1 - RPT-H2 @ RPT-HEAD HEAD-STAMP !
   RPT-SW @ RPT-PW @ RESERVE RPT-ID RPT-H !
   s" a slot's last generation is 2^32 - 1" T-LABEL
   RPT-H @ HANDLE>BITS U32-MAX and RPT-H2 @ HANDLE>BITS U32-MAX and T=
   RPT-H @ HANDLE>BITS 32 rshift LAST-GEN T=
   RPT-H @ RPT-PW @ RELEASE
   4 RPT-PW @ RECLAIM-STEP 1 T=
   s" returning it there retires the slot: the next is a fresh one" T-LABEL
   RPT-SW @ RPT-PW @ RESERVE RPT-ID HANDLE>BITS
   RPT-H2 @ HANDLE>BITS U32-MAX and 1 + 1 32 lshift or T=
   s" and the slot's last handle is stale" T-LABEL
   [: RPT-H @ RPT-PW @ RETAIN ;] RT-HANDLE:E-RT-HANDLE-STALE TTHROWSQ ;

: RPT-REFUSE-MAP ( ptr ptr u8 -- ptr ptr u8 )
   E-MEM-MAP throw ;

: RPT-OTHER-THROW ( ptr ptr u8 -- ptr ptr u8 )
   E-MEM-SIZE throw ;

\ RPT-PW's pages pool has mapped no chunk.
: RPT-MAPPING ( -- )
   RPT-SP @ SCHEMA# RPT-S !
   s" a growth the OS refuses answers oom" T-LABEL
   RPT-S @ RPT-PW @ OPEN-CELLS [: RPT-REFUSE-MAP ;] RESERVE-IN RPT-OOM? TTRUE
   s" and leaves the pool's ledgers unchanged" T-LABEL
   RT--POOL-KIND:pages RPT-PW @ MAPPED 0 T=
   RT--POOL-KIND:pages RPT-PW @ CHARGED 0 T=
   s" any other throw from the mapping goes on" T-LABEL
   [: RPT-S @ RPT-PW @ OPEN-CELLS [: RPT-OTHER-THROW ;] RESERVE-IN RPT-OOM? drop ;] E-MEM-SIZE TTHROWSQ
   RT--POOL-KIND:pages RPT-PW @ MAPPED 0 T=
   s" and the OS's chunk is the pool's next" T-LABEL
   RPT-SP @ RPT-PW @ RESERVE RPT-OOM? TFALSE
   RT--POOL-KIND:pages RPT-PW @ MAPPED CHUNK-BYTES T=
   RPT-PW @ SHUTDOWN ;

\ Where RPT-THIRD-REFUSED mapped its two chunks.
2 TYPED-BUFFER RPT-GIVEN ptr u8

\ A mapper that maps two chunks, saving where, then refuses.
: RPT-THIRD-REFUSED ( ptr ptr u8 -- ptr ptr u8 )
   RPT-MAPS @ 2 >= if E-MEM-MAP throw then
   MAP-CHUNK
   dup @ RPT-MAPS @ RPT-GIVEN !
   1 RPT-MAPS +! ;

: RPT-READ-GIVEN ( n -- )
   RPT-GIVEN @ c@ drop ;

: RPT-READ-HELD ( -- )
   EMERGENCY# 0 RPT-PW @ OPEN-CELLS ENTRY @ c@ drop ;

$1000 constant RPT-CAP
10000 constant RPT-CHILD-MS
\ The exit of a process the engine's fault handler ends (src/habu/crash.f).
$86 constant RPT-FAULT-RC
RPT-CAP BUFFER: RPT-OUT
RPT-CAP BUFFER: RPT-ERR

\ A forked child of this image runs the text and exits with the code.
: RPT-EXITS ( ptr u8 n n -- )
   {: src:ptr srcu:n want:n :}
   src srcu RPT-OUT RPT-CAP >LEN RPT-ERR RPT-CAP >LEN RPT-CHILD-MS >MS SUBJECT:RUN
   {: outu:len erru:len oc :}
   src srcu RPT-OUT outu LEN>N RPT-ERR erru LEN>N oc want T-OUTCOME-EXITED= ;

: RPT-INIT-REFUSED ( -- )
   0 RPT-MAPS !
   s" INIT throws a refused mapping of the emergency pool" T-LABEL
   [: 0 RPT-CELLS-I [: RPT-THIRD-REFUSED ;] INIT-WITH drop ;] E-MEM-MAP TTHROWSQ
   s" having given back the chunks it mapped" T-LABEL
   EMERGENCY# 0 RPT-CELLS-I POOL-OF POOL-CHUNKS @ 0 T=
   s" to the OS: a child that reads either of them faults" T-LABEL
   s" 0 RPT-READ-GIVEN" RPT-FAULT-RC RPT-EXITS
   s" 1 RPT-READ-GIVEN" RPT-FAULT-RC RPT-EXITS
   s" and leaves its cells to INIT again" T-LABEL
   0 RPT-CELLS-I INIT RPT-PW !
   RT--POOL-KIND:emergency RPT-PW @ MAPPED EMERGENCY-CEILING T=
   s" whose chunks a child reads" T-LABEL
   s" RPT-READ-HELD" 0 RPT-EXITS
   RPT-PW @ SHUTDOWN ;

: RPT-SCHEMAS-FULL ( -- )
   MAX-SCHEMAS SCHEMAS @ - 0 ?do RT--POOL-KIND:pages [: RPT-NO-EDGE ;] SCHEMA drop loop
   s" a schema past MAX-SCHEMAS is refused" T-LABEL
   [: RT--POOL-KIND:pages [: RPT-NO-EDGE ;] SCHEMA drop ;] E-RT-POOL-SCHEMA TTHROWSQ ;

RPT-OVERFLOW
RPT-SAME-REF
RPT-RETIRE
RPT-MAPPING
RPT-INIT-REFUSED
RPT-SCHEMAS-FULL

;package

T-REPORT
