\ ir-arena.f - checked compiler IR arena tests.
\
\ Proves the sections 6.2-6.3 arena contract of src/compiler/ir/arena.f:
\ append-only cells behind nominal indices, mark/rollback, geometric growth
\ under a committed ceiling with contents preserved, named overflow that
\ leaves the arena usable, cross-owner rejection for indices, marks, and
\ contexts, ABORT and FREEZE consuming the builder, fail-closed staleness
\ after context teardown, whole-range release through the owning context
\ (shown by reusing the registry across more contexts than it has slots and
\ by two same-size full fills back to back), and checker fixtures proving
\ the private cast window is sealed.

require lib/test.f
require lib/memory.f
require test/checker-assert.f
require src/compiler/ir/arena.f

package IR-ARENA-TEST
private

8 constant TSEED                     \ pins the pre-doubling seed capacity
64 constant TSLOTS                   \ pins the arena registry capacity
2048 constant TBIG                   \ full-fill ceiling; its doubling chain fits one mapping
$20000 constant TMAP-BYTES           \ pins the context mapping size

\ ---- fixtures ----------------------------------------------------------------
: BND ( -- CBIND:binding )
   CTARGET-ARCH:AARCH64 CTARGET-ABI:AAPCS64-DARWIN CTARGET-ENDIAN:LITTLE
   CTARGET-PTR--WIDTH:BITS64
   CTARGET:F-BASE CTARGET:F-FP CTARGET:WITH CTARGET:CONTRACT
   CNUM-OVERFLOW:TRAP CNUM-FLOAT--MODEL:IEEE754 CNUM-CONTRACTION:FORBIDDEN
   CNUM-FAST--MATH:BIT-EXACT CNUM-COMPARE:IEEE754-UNORDERED CNUM:POLICY
   CBIND:BIND ;

: ACCT-PROBE-BODY ( ptr u8 CAD-NUM:alloc-byte-len -- bool )
   drop {: p:ptr :}
   $A5 p c! p c@ $A5 = ;

: ACCT-PROBE ( -- )
   TMAP-BYTES MEM:BYTES-ALLOC-LEN [: ACCT-PROBE-BODY ;] MEM:WITH-BYTES TTRUE ;

\ ---- creation binds a live, empty arena --------------------------------------
: CRT-BODY ( IR-CTX:ctx -- bool n n n )
   {: c:IR-CTX:ctx :}
   c 16 IR-ARENA:NEW {: a:IR-ARENA:arena :}
   a IR-ARENA:LIVE?
   a IR-ARENA:USED
   c a 11 IR-ARENA:PUSH IR-ARENA:ORD
   a IR-ARENA:USED ;

: CREATE-CASE ( -- )
   s" creation binds a live empty arena" T-LABEL
   BND [: CRT-BODY ;] IR-CTX:WITH-CONTEXT
   1 T= 0 T= 0 T= TTRUE ;

: CL-ZERO-BODY ( IR-CTX:ctx -- )
   0 IR-ARENA:NEW drop ;

: CL-ZERO ( -- )
   BND [: CL-ZERO-BODY ;] IR-CTX:WITH-CONTEXT ;

: CL-NEG-BODY ( IR-CTX:ctx -- )
   -4 IR-ARENA:NEW drop ;

: CL-NEG ( -- )
   BND [: CL-NEG-BODY ;] IR-CTX:WITH-CONTEXT ;

: CL-HUGE-BODY ( IR-CTX:ctx -- )
   $100000000 IR-ARENA:NEW drop ;

: CL-HUGE ( -- )
   BND [: CL-HUGE-BODY ;] IR-CTX:WITH-CONTEXT ;

: CEIL-CASES ( -- )
   s" a zero ceiling is rejected at creation" T-LABEL
   [: CL-ZERO ;] E-IR-ARENA-CEIL TTHROWSQ
   s" a negative ceiling is rejected at creation" T-LABEL
   [: CL-NEG ;] E-IR-ARENA-CEIL TTHROWSQ
   s" a ceiling past the ordinal range is rejected at creation" T-LABEL
   [: CL-HUGE ;] E-IR-ARENA-CEIL TTHROWSQ ;

\ ---- mark and rollback -------------------------------------------------------
: MR-BODY ( IR-CTX:ctx -- n n n n n )
   {: c:IR-CTX:ctx :}
   c 16 IR-ARENA:NEW {: a:IR-ARENA:arena :}
   c a 1 IR-ARENA:PUSH drop
   c a 2 IR-ARENA:PUSH drop
   c a 3 IR-ARENA:PUSH drop
   a IR-ARENA:MARK {: m:IR-ARENA:mark :}
   c a 4 IR-ARENA:PUSH drop
   c a 5 IR-ARENA:PUSH drop
   a IR-ARENA:USED
   a m IR-ARENA:ROLLBACK
   a IR-ARENA:USED
   a a 2 IR-ARENA:NTH IR-ARENA:PEEK
   c a 6 IR-ARENA:PUSH IR-ARENA:ORD
   a IR-ARENA:USED ;

: MR-CASE ( -- )
   s" a rollback truncates to the mark and appends continue there" T-LABEL
   BND [: MR-BODY ;] IR-CTX:WITH-CONTEXT
   4 T= 3 T= 3 T= 3 T= 5 T= ;

\ An index minted past the mark is dead after the rollback truncates it.
: MR-DEAD-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 16 IR-ARENA:NEW {: a:IR-ARENA:arena :}
   c a 1 IR-ARENA:PUSH drop
   a IR-ARENA:MARK {: m:IR-ARENA:mark :}
   c a 2 IR-ARENA:PUSH {: x:IR-ARENA:cell-id :}
   a m IR-ARENA:ROLLBACK
   a x IR-ARENA:PEEK drop ;

: MR-DEAD ( -- )
   BND [: MR-DEAD-BODY ;] IR-CTX:WITH-CONTEXT ;

\ A mark whose cursor a deeper rollback already truncated cannot resurrect it.
: MR-RESURRECT-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 16 IR-ARENA:NEW {: a:IR-ARENA:arena :}
   c a 1 IR-ARENA:PUSH drop
   a IR-ARENA:MARK {: m1:IR-ARENA:mark :}
   c a 2 IR-ARENA:PUSH drop
   a IR-ARENA:MARK {: m2:IR-ARENA:mark :}
   a m1 IR-ARENA:ROLLBACK
   a m2 IR-ARENA:ROLLBACK ;

: MR-RESURRECT ( -- )
   BND [: MR-RESURRECT-BODY ;] IR-CTX:WITH-CONTEXT ;

: MR-REJECT-CASES ( -- )
   s" an index past the rollback cursor is dead" T-LABEL
   [: MR-DEAD ;] E-IR-ARENA-BOUND TTHROWSQ
   s" a truncated mark cannot resurrect cells" T-LABEL
   [: MR-RESURRECT ;] E-IR-ARENA-MARK TTHROWSQ ;

\ ---- geometric growth preserves contents -------------------------------------
: GROW-FILL ( IR-CTX:ctx IR-ARENA:arena n -- )
   {: c:IR-CTX:ctx a:IR-ARENA:arena cnt:n :}
   cnt 0 ?do
      c a i 7 * 1+ IR-ARENA:PUSH drop
   loop ;

: GROW-VERIFY ( IR-ARENA:arena n -- bool )
   {: a:IR-ARENA:arena cnt:n :}
   0 0 =
   cnt 0 ?do
      a a i IR-ARENA:NTH IR-ARENA:PEEK i 7 * 1+ = and
   loop ;

\ 20 cells across the 8 -> 16 -> 32 doublings; every cell reads back.
: GROW-BODY ( IR-CTX:ctx -- bool n )
   {: c:IR-CTX:ctx :}
   c 32 IR-ARENA:NEW {: a:IR-ARENA:arena :}
   c a 20 GROW-FILL
   a 20 GROW-VERIFY
   a IR-ARENA:USED ;

: GROW-CASE ( -- )
   s" growth across doublings preserves every cell" T-LABEL
   BND [: GROW-BODY ;] IR-CTX:WITH-CONTEXT
   20 T= TTRUE ;

\ ---- overflow at the committed ceiling ---------------------------------------
: OV-RUN-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 4 IR-ARENA:NEW {: a:IR-ARENA:arena :}
   5 0 ?do
      c a i IR-ARENA:PUSH drop
   loop ;

: OV-RUN ( -- )
   BND [: OV-RUN-BODY ;] IR-CTX:WITH-CONTEXT ;

: OV-FIFTH ( IR-CTX:ctx IR-ARENA:arena -- IR-CTX:ctx IR-ARENA:arena )
   2dup 99 IR-ARENA:PUSH drop ;

\ The ceiling hit mutates nothing: the arena stays readable and appendable
\ after a rollback opens room (the file-documented usable-after-full choice).
: OV-USABLE-BODY ( IR-CTX:ctx -- n n n n )
   {: c:IR-CTX:ctx :}
   c 4 IR-ARENA:NEW {: a:IR-ARENA:arena :}
   a IR-ARENA:MARK {: m0:IR-ARENA:mark :}
   c a 4 GROW-FILL
   c a [: OV-FIFTH ;] catch {: c2:IR-CTX:ctx a2:IR-ARENA:arena rc:n :}
   rc
   a2 IR-ARENA:USED
   a2 a2 3 IR-ARENA:NTH IR-ARENA:PEEK
   a2 m0 IR-ARENA:ROLLBACK
   c2 a2 77 IR-ARENA:PUSH IR-ARENA:ORD ;

: OV-CASES ( -- )
   s" the fifth append into a four-cell ceiling is a named overflow" T-LABEL
   [: OV-RUN ;] E-IR-ARENA-FULL TTHROWSQ
   s" a full arena stays usable: contents intact, rollback reopens room" T-LABEL
   BND [: OV-USABLE-BODY ;] IR-CTX:WITH-CONTEXT
   0 T= 22 T= 4 T= E-IR-ARENA-FULL T= ;

\ ---- cross-owner rejection ---------------------------------------------------
: XO-PEEK-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 IR-ARENA:NEW {: a:IR-ARENA:arena :}
   c 8 IR-ARENA:NEW {: b:IR-ARENA:arena :}
   c a 7 IR-ARENA:PUSH {: x:IR-ARENA:cell-id :}
   b x IR-ARENA:PEEK drop ;

: XO-PEEK ( -- )
   BND [: XO-PEEK-BODY ;] IR-CTX:WITH-CONTEXT ;

: XO-MARK-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 IR-ARENA:NEW {: a:IR-ARENA:arena :}
   c 8 IR-ARENA:NEW {: b:IR-ARENA:arena :}
   a IR-ARENA:MARK {: m:IR-ARENA:mark :}
   b m IR-ARENA:ROLLBACK ;

: XO-MARK ( -- )
   BND [: XO-MARK-BODY ;] IR-CTX:WITH-CONTEXT ;

: XO-CTX-INNER ( IR-ARENA:arena IR-CTX:ctx -- )
   swap 5 IR-ARENA:PUSH drop ;

: XO-CTX-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 IR-ARENA:NEW
   BND [: XO-CTX-INNER ;] IR-CTX:WITH-CONTEXT ;

: XO-CTX ( -- )
   BND [: XO-CTX-BODY ;] IR-CTX:WITH-CONTEXT ;

: XO-CASES ( -- )
   s" an index minted by arena A rejects on arena B" T-LABEL
   [: XO-PEEK ;] E-IR-ARENA-OWNER TTHROWSQ
   s" a mark minted by arena A rejects on arena B" T-LABEL
   [: XO-MARK ;] E-IR-ARENA-OWNER TTHROWSQ
   s" a foreign live context rejects as the growth allocator" T-LABEL
   [: XO-CTX ;] E-IR-ARENA-OWNER TTHROWSQ ;

\ ---- abort consumes the builder ----------------------------------------------
: AB-USED-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 IR-ARENA:NEW {: a:IR-ARENA:arena :}
   c a 1 IR-ARENA:PUSH drop
   a IR-ARENA:ABORT
   a IR-ARENA:USED drop ;

: AB-USED ( -- )
   BND [: AB-USED-BODY ;] IR-CTX:WITH-CONTEXT ;

: AB-PUSH-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 IR-ARENA:NEW {: a:IR-ARENA:arena :}
   a IR-ARENA:ABORT
   c a 2 IR-ARENA:PUSH drop ;

: AB-PUSH ( -- )
   BND [: AB-PUSH-BODY ;] IR-CTX:WITH-CONTEXT ;

: AB-FREEZE-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 IR-ARENA:NEW {: a:IR-ARENA:arena :}
   a IR-ARENA:ABORT
   a IR-ARENA:FREEZE drop ;

: AB-FREEZE ( -- )
   BND [: AB-FREEZE-BODY ;] IR-CTX:WITH-CONTEXT ;

: AB-TWICE-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 IR-ARENA:NEW {: a:IR-ARENA:arena :}
   a IR-ARENA:ABORT
   a IR-ARENA:ABORT ;

: AB-TWICE ( -- )
   BND [: AB-TWICE-BODY ;] IR-CTX:WITH-CONTEXT ;

\ A slot reused after ABORT does not revive the old handle: the generation,
\ not the registry position, is the identity.
: AB-REUSE-BODY ( IR-CTX:ctx -- bool bool )
   {: c:IR-CTX:ctx :}
   c 8 IR-ARENA:NEW {: old:IR-ARENA:arena :}
   old IR-ARENA:ABORT
   c 8 IR-ARENA:NEW {: fresh:IR-ARENA:arena :}
   fresh IR-ARENA:LIVE?
   old IR-ARENA:LIVE? ;

: AB-CASES ( -- )
   s" reading an aborted arena rejects" T-LABEL
   [: AB-USED ;] E-IR-ARENA-STALE TTHROWSQ
   s" appending to an aborted arena rejects" T-LABEL
   [: AB-PUSH ;] E-IR-ARENA-STALE TTHROWSQ
   s" freezing an aborted arena rejects" T-LABEL
   [: AB-FREEZE ;] E-IR-ARENA-STALE TTHROWSQ
   s" aborting twice rejects" T-LABEL
   [: AB-TWICE ;] E-IR-ARENA-STALE TTHROWSQ
   s" a reused registry slot does not revive an aborted handle" T-LABEL
   BND [: AB-REUSE-BODY ;] IR-CTX:WITH-CONTEXT
   TFALSE TTRUE ;

\ ---- freeze publishes an immutable view --------------------------------------
: FZ-BODY ( IR-CTX:ctx -- n n n )
   {: c:IR-CTX:ctx :}
   c 8 IR-ARENA:NEW {: a:IR-ARENA:arena :}
   c a 10 IR-ARENA:PUSH {: pre:IR-ARENA:cell-id :}
   c a 20 IR-ARENA:PUSH drop
   a IR-ARENA:FREEZE {: v:IR-ARENA:view :}
   v IR-ARENA:SIZE
   v pre IR-ARENA:AT
   v v 1 IR-ARENA:FROZEN-NTH IR-ARENA:AT ;

: FZ-CASE ( -- )
   s" frozen readers serve indices minted before and after the freeze" T-LABEL
   BND [: FZ-BODY ;] IR-CTX:WITH-CONTEXT
   20 T= 10 T= 2 T= ;

: FZ-PUSH-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 IR-ARENA:NEW {: a:IR-ARENA:arena :}
   a IR-ARENA:FREEZE drop
   c a 1 IR-ARENA:PUSH drop ;

: FZ-PUSH ( -- )
   BND [: FZ-PUSH-BODY ;] IR-CTX:WITH-CONTEXT ;

: FZ-ROLL-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 IR-ARENA:NEW {: a:IR-ARENA:arena :}
   c a 1 IR-ARENA:PUSH drop
   a IR-ARENA:MARK {: m:IR-ARENA:mark :}
   a IR-ARENA:FREEZE drop
   a m IR-ARENA:ROLLBACK ;

: FZ-ROLL ( -- )
   BND [: FZ-ROLL-BODY ;] IR-CTX:WITH-CONTEXT ;

: FZ-ABORT-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 IR-ARENA:NEW {: a:IR-ARENA:arena :}
   a IR-ARENA:FREEZE drop
   a IR-ARENA:ABORT ;

: FZ-ABORT ( -- )
   BND [: FZ-ABORT-BODY ;] IR-CTX:WITH-CONTEXT ;

: FZ-NTH-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 IR-ARENA:NEW {: a:IR-ARENA:arena :}
   c a 1 IR-ARENA:PUSH drop
   a IR-ARENA:FREEZE {: v:IR-ARENA:view :}
   v 1 IR-ARENA:FROZEN-NTH drop ;

: FZ-NTH ( -- )
   BND [: FZ-NTH-BODY ;] IR-CTX:WITH-CONTEXT ;

\ An index truncated by a rollback stays dead against the frozen count.
: FZ-BOUND-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 IR-ARENA:NEW {: a:IR-ARENA:arena :}
   c a 1 IR-ARENA:PUSH drop
   a IR-ARENA:MARK {: m:IR-ARENA:mark :}
   c a 2 IR-ARENA:PUSH {: x:IR-ARENA:cell-id :}
   a m IR-ARENA:ROLLBACK
   a IR-ARENA:FREEZE {: v:IR-ARENA:view :}
   v x IR-ARENA:AT drop ;

: FZ-BOUND ( -- )
   BND [: FZ-BOUND-BODY ;] IR-CTX:WITH-CONTEXT ;

: FZ-FOREIGN-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 IR-ARENA:NEW {: a:IR-ARENA:arena :}
   c 8 IR-ARENA:NEW {: b:IR-ARENA:arena :}
   c b 9 IR-ARENA:PUSH {: x:IR-ARENA:cell-id :}
   a IR-ARENA:FREEZE {: v:IR-ARENA:view :}
   v x IR-ARENA:AT drop ;

: FZ-FOREIGN ( -- )
   BND [: FZ-FOREIGN-BODY ;] IR-CTX:WITH-CONTEXT ;

: FZ-REJECT-CASES ( -- )
   s" appending through a frozen builder handle rejects" T-LABEL
   [: FZ-PUSH ;] E-IR-ARENA-FROZEN TTHROWSQ
   s" rolling back across a freeze rejects" T-LABEL
   [: FZ-ROLL ;] E-IR-ARENA-FROZEN TTHROWSQ
   s" aborting across a freeze rejects" T-LABEL
   [: FZ-ABORT ;] E-IR-ARENA-FROZEN TTHROWSQ
   s" a frozen reader bounds-checks the ordinal" T-LABEL
   [: FZ-NTH ;] E-IR-ARENA-BOUND TTHROWSQ
   s" a truncated index stays dead against the frozen count" T-LABEL
   [: FZ-BOUND ;] E-IR-ARENA-BOUND TTHROWSQ
   s" a foreign index rejects on a frozen view" T-LABEL
   [: FZ-FOREIGN ;] E-IR-ARENA-OWNER TTHROWSQ ;

\ ---- staleness after context teardown ----------------------------------------
: ESC-BODY ( IR-CTX:ctx -- IR-ARENA:arena )
   {: c:IR-CTX:ctx :}
   c 8 IR-ARENA:NEW {: a:IR-ARENA:arena :}
   c a 1 IR-ARENA:PUSH drop
   a ;

: DEAD-ARENA ( -- IR-ARENA:arena )
   BND [: ESC-BODY ;] IR-CTX:WITH-CONTEXT ;

: ST-USED ( -- )
   DEAD-ARENA IR-ARENA:USED drop ;

: ST-MARK ( -- )
   DEAD-ARENA IR-ARENA:MARK drop ;

: ST-FREEZE ( -- )
   DEAD-ARENA IR-ARENA:FREEZE drop ;

: ST-PUSH-INNER ( IR-ARENA:arena IR-CTX:ctx -- )
   swap 9 IR-ARENA:PUSH drop ;

: ST-PUSH ( -- )
   DEAD-ARENA
   BND [: ST-PUSH-INNER ;] IR-CTX:WITH-CONTEXT ;

: ESC-VIEW-BODY ( IR-CTX:ctx -- IR-ARENA:view )
   {: c:IR-CTX:ctx :}
   c 8 IR-ARENA:NEW {: a:IR-ARENA:arena :}
   c a 1 IR-ARENA:PUSH drop
   a IR-ARENA:FREEZE ;

: ST-VIEW ( -- )
   BND [: ESC-VIEW-BODY ;] IR-CTX:WITH-CONTEXT
   IR-ARENA:SIZE drop ;

: ST-CASES ( -- )
   s" an arena handle is dead after its context ends" T-LABEL
   DEAD-ARENA IR-ARENA:LIVE? TFALSE
   s" reading after context teardown rejects" T-LABEL
   [: ST-USED ;] E-IR-ARENA-STALE TTHROWSQ
   s" marking after context teardown rejects" T-LABEL
   [: ST-MARK ;] E-IR-ARENA-STALE TTHROWSQ
   s" freezing after context teardown rejects" T-LABEL
   [: ST-FREEZE ;] E-IR-ARENA-STALE TTHROWSQ
   s" appending with a fresh live context still rejects the dead arena" T-LABEL
   [: ST-PUSH ;] E-IR-ARENA-STALE TTHROWSQ
   s" a frozen view is dead after its context ends" T-LABEL
   [: ST-VIEW ;] E-IR-ARENA-STALE TTHROWSQ ;

\ ---- registry capacity and whole-range release -------------------------------
\ The one-past-capacity reject is caught inside the live context, so the
\ context exits normally and its 64 arenas are reclaimed by the next sweep
\ instead of lingering behind a throw-aborted registry slot.
: SL-65TH ( IR-CTX:ctx -- IR-CTX:ctx )
   dup 1 IR-ARENA:NEW drop ;

: SL-BODY ( IR-CTX:ctx -- n )
   {: c:IR-CTX:ctx :}
   TSLOTS 0 ?do
      c 1 IR-ARENA:NEW drop
   loop
   c [: SL-65TH ;] catch nip ;

: SL-CASE ( -- )
   s" the registry fills to its capacity and rejects one more, named" T-LABEL
   BND [: SL-BODY ;] IR-CTX:WITH-CONTEXT
   E-IR-ARENA-SLOTS T= ;

: WR-ONE-BODY ( IR-CTX:ctx -- n )
   {: c:IR-CTX:ctx :}
   c 8 IR-ARENA:NEW {: a:IR-ARENA:arena :}
   c a 1 IR-ARENA:PUSH drop
   a IR-ARENA:USED ;

\ Fill an arena to its full TBIG ceiling: the doubling chain 8..TBIG plus the
\ live span fits one context mapping only if every earlier context released
\ its whole range, so running it twice proves the release.
: WR-FULL-BODY ( IR-CTX:ctx -- bool n )
   {: c:IR-CTX:ctx :}
   c TBIG IR-ARENA:NEW {: a:IR-ARENA:arena :}
   c a TBIG GROW-FILL
   a a TBIG 1- IR-ARENA:NTH IR-ARENA:PEEK TBIG 1- 7 * 1+ =
   a IR-ARENA:USED ;

: WR-CASES ( -- )
   s" teardown releases registry slots across more contexts than slots" T-LABEL
   TSLOTS 2 * 0 ?do
      BND [: WR-ONE-BODY ;] IR-CTX:WITH-CONTEXT 1 T=
   loop
   s" a torn-down context releases its arenas' whole range" T-LABEL
   BND [: WR-FULL-BODY ;] IR-CTX:WITH-CONTEXT
   TBIG T= TTRUE
   BND [: WR-FULL-BODY ;] IR-CTX:WITH-CONTEXT
   TBIG T= TTRUE
   ACCT-PROBE ;

\ ---- the checker seals the cast window ---------------------------------------
: CHECKER-CASES ( -- )
   s" IRA-FORGE ( n -- IR-ARENA:arena )"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" IRA-ERASE ( IR-ARENA:arena -- n )"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" IRA-ID-FORGE ( n -- IR-ARENA:cell-id )"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" IRA-ID-ERASE ( IR-ARENA:cell-id -- n )"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" IRA-VIEW-FORGE ( n -- IR-ARENA:view )"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" IRA-MARK-FORGE ( n -- IR-ARENA:mark )"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" IRA-VIEWLESS ( IR-ARENA:arena IR-ARENA:cell-id -- n ) IR-ARENA:AT"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" IRA-CTXLESS ( IR-ARENA:arena n -- IR-ARENA:cell-id ) IR-ARENA:PUSH"
      CHECK-QUIET-CANDIDATE! 0 T= ;

\ ---- run ---------------------------------------------------------------------
\ Every throw-through fixture runs inside one outermost harness context, so a
\ context aborted by a throw (its registry slot lingers until the nearest
\ enclosing live context leaves, per context.f) is reclaimed by the harness
\ exit instead of leaking a live-looking owner for the rest of the process.
\ The exact-capacity and whole-range cases run after that exit, on a registry
\ the next sweep can fully reclaim.
: HARNESS-BODY ( IR-CTX:ctx -- )
   drop
   CREATE-CASE
   CEIL-CASES
   MR-CASE
   MR-REJECT-CASES
   GROW-CASE
   OV-CASES
   XO-CASES
   AB-CASES
   FZ-CASE
   FZ-REJECT-CASES
   ST-CASES ;

public

: RUN ( -- )
   T-RESET
   BND [: HARNESS-BODY ;] IR-CTX:WITH-CONTEXT
   SL-CASE
   WR-CASES
   CHECKER-CASES
   T-REPORT ;

;package

IR-ARENA-TEST:RUN
