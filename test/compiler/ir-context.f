\ ir-context.f - checked owned-compiler-context tests.
\
\ Proves the section 6.2 ownership contract of src/compiler/ir/context.f:
\ serials never reuse across sibling contexts or within one context, failure
\ names the owning context, stale and double use reject, teardown releases
\ every live child (shown through the MEM:WITH-BYTES accounting: after a
\ context ends, a fresh mapping of the same size succeeds and no per-context
\ state leaks into the reused slot), and the serial-exhaustion path is driven
\ through the production ceiling parameter rather than a synthetic hook.

require lib/test.f
require lib/memory.f
require test/checker-assert.f
require lib/image-lifecycle.f
require src/compiler/ir/context.f
require src/compiler/ir/arena.f

package IR-CTX-TEST
private

$80000 constant TMAP-BYTES           \ independent allocation probe size
64 constant TDEPTH-MAX               \ pins the registry capacity

\ ---- fixtures ----------------------------------------------------------------
: BND ( -- CBIND:binding )
   CTARGET-ARCH:AARCH64 CTARGET-ABI:AAPCS64-DARWIN CTARGET-ENDIAN:LITTLE
   CTARGET-PTR--WIDTH:BITS64
   CTARGET:F-BASE CTARGET:F-FP CTARGET:WITH CTARGET:CONTRACT
   CNUM-OVERFLOW:TRAP CNUM-FLOAT--MODEL:IEEE754 CNUM-CONTRACTION:FORBIDDEN
   CNUM-FAST--MATH:BIT-EXACT CNUM-COMPARE:IEEE754-UNORDERED CNUM:POLICY
   CBIND:BIND ;

: BND2 ( -- CBIND:binding )
   CTARGET-ARCH:PTX CTARGET-ABI:PTX-KERNEL CTARGET-ENDIAN:LITTLE
   CTARGET-PTR--WIDTH:BITS64
   CTARGET:F-BASE CTARGET:F-FP CTARGET:WITH CTARGET:F-MMA CTARGET:WITH
   CTARGET:CONTRACT
   CNUM-OVERFLOW:WRAP CNUM-FLOAT--MODEL:FLUSH-DENORMAL CNUM-CONTRACTION:ALLOWED
   CNUM-FAST--MATH:APPROXIMATE CNUM-COMPARE:ASSUME-ORDERED CNUM:POLICY
   CBIND:BIND ;

\ A coherent target without floating point paired with a contraction-allowed
\ policy: each half validates alone, the pair does not, and only the generated
\ constructor can assemble it. WITH-CONTEXT must reject it.
: FORGED-BND ( -- CBIND:binding )
   CTARGET-ARCH:AARCH64 CTARGET-ABI:AAPCS64-DARWIN CTARGET-ENDIAN:LITTLE
   CTARGET-PTR--WIDTH:BITS64 CTARGET:F-BASE CTARGET:CONTRACT
   CNUM-OVERFLOW:WRAP CNUM-FLOAT--MODEL:IEEE754 CNUM-CONTRACTION:ALLOWED
   CNUM-FAST--MATH:REASSOCIATE CNUM-COMPARE:IEEE754-UNORDERED CNUM:POLICY
   CBIND-BINDING:MAKE ;

\ ---- creation binds a live, empty context ------------------------------------
: CRT-BODY ( IR-CTX:ctx -- bool n n bool )
   dup IR-CTX:LIVE? swap
   dup IR-CTX:MINTED swap
   dup IR-CTX:SCRATCH-USED swap
   IR-CTX:BINDING@ BND CBIND:SAME? ;

: CREATE-CASE ( -- )
   s" creation binds a live empty context" T-LABEL
   BND [: CRT-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE 0 T= 0 T= TTRUE ;

\ The context returns the exact binding it was created with, not a fixed one.
: B2-BODY ( IR-CTX:ctx -- bool bool )
   dup IR-CTX:BINDING@ BND2 CBIND:SAME? swap
   IR-CTX:BINDING@ BND CBIND:SAME? ;

: BINDING-CASE ( -- )
   s" context owns the exact binding it was created with" T-LABEL
   BND2 [: B2-BODY ;] IR-CTX:WITH-CONTEXT
   TFALSE TTRUE ;

: FORGED-RUN ( -- )
   FORGED-BND [: CRT-BODY ;] IR-CTX:WITH-CONTEXT
   drop drop drop drop ;

: FORGED-CASE ( -- )
   s" creation revalidates a forged binding" T-LABEL
   [: FORGED-RUN ;] E-CBIND-CONTRACT TTHROWSQ ;

\ ---- serials never reuse -----------------------------------------------------
: TWO-BODY ( IR-CTX:ctx -- IR-ID:ir-module-id IR-ID:ir-module-id )
   dup IR-CTX:NEW-MODULE nip swap
   IR-CTX:NEW-MODULE nip ;

: WITHIN-DISTINCT ( -- )
   s" module serials never reuse within one context" T-LABEL
   BND [: TWO-BODY ;] IR-CTX:WITH-CONTEXT
   IR-ID:MODULE-SAME? TFALSE ;

: SIB-BODY ( IR-CTX:ctx -- IR-ID:ir-module-id )
   IR-CTX:NEW-MODULE nip ;

: SIBLING-DISTINCT ( -- )
   s" module serials never reuse across sibling contexts" T-LABEL
   BND [: SIB-BODY ;] IR-CTX:WITH-CONTEXT
   BND [: SIB-BODY ;] IR-CTX:WITH-CONTEXT
   IR-ID:MODULE-SAME? TFALSE ;

: SER-BODY ( IR-CTX:ctx -- n )
   IR-CTX:SERIAL ;

: CTX-SERIAL-DISTINCT ( -- )
   s" context serials are nonzero and never reuse" T-LABEL
   BND [: SER-BODY ;] IR-CTX:WITH-CONTEXT
   BND [: SER-BODY ;] IR-CTX:WITH-CONTEXT
   {: a:n b:n :}
   a 0 > TTRUE
   b 0 > TTRUE
   a b <> TTRUE ;

: INVALID-SERIAL-CASES ( -- )
   s" generation-zero and out-of-range serials are not live" T-LABEL
   0 IR-CTX:SERIAL-LIVE? TFALSE
   TDEPTH-MAX 1- IR-CTX:SERIAL-LIVE? TFALSE
   -1 IR-CTX:SERIAL-LIVE? TFALSE
   $7FFFFFFFFFFFFFFF IR-CTX:SERIAL-LIVE? TFALSE ;

\ ---- serial exhaustion through the production ceiling parameter --------------
: EX-OK-BODY ( IR-CTX:ctx -- n )
   dup IR-CTX:NEW-MODULE 2drop
   dup IR-CTX:NEW-MODULE 2drop
   IR-CTX:MINTED ;

: EX-BODY ( IR-CTX:ctx -- )
   dup IR-CTX:NEW-MODULE 2drop
   dup IR-CTX:NEW-MODULE 2drop
   IR-CTX:NEW-MODULE 2drop ;

: EX-RUN ( -- )
   BND 2 [: EX-BODY ;] IR-CTX:WITH-CONTEXT-BOUND ;

: CEIL-ZERO ( -- )
   BND 0 [: SER-BODY ;] IR-CTX:WITH-CONTEXT-BOUND drop ;

: CEIL-NEG ( -- )
   BND -3 [: SER-BODY ;] IR-CTX:WITH-CONTEXT-BOUND drop ;

: CEIL-HUGE ( -- )
   BND $80000000 [: SER-BODY ;] IR-CTX:WITH-CONTEXT-BOUND drop ;

: EXHAUST-CASES ( -- )
   s" a bounded context mints up to its ceiling" T-LABEL
   BND 2 [: EX-OK-BODY ;] IR-CTX:WITH-CONTEXT-BOUND 2 T=
   s" minting past the ceiling is a named exhaustion" T-LABEL
   [: EX-RUN ;] E-IR-CTX-SERIALS TTHROWSQ
   s" a zero ceiling is rejected at creation" T-LABEL
   [: CEIL-ZERO ;] E-IR-CTX-CEILING TTHROWSQ
   s" a negative ceiling is rejected at creation" T-LABEL
   [: CEIL-NEG ;] E-IR-CTX-CEILING TTHROWSQ
   s" a ceiling past the serial range is rejected at creation" T-LABEL
   [: CEIL-HUGE ;] E-IR-CTX-CEILING TTHROWSQ ;

\ ---- stale and double use reject ---------------------------------------------
: KEEP-BODY ( IR-CTX:ctx -- IR-CTX:ctx ) ;

: DEAD-CTX ( -- IR-CTX:ctx )
   BND [: KEEP-BODY ;] IR-CTX:WITH-CONTEXT ;

: STALE-MINT ( -- )
   DEAD-CTX IR-CTX:NEW-MODULE 2drop ;

: STALE-BINDING ( -- )
   DEAD-CTX IR-CTX:BINDING@ BND CBIND:SAME? drop ;

: STALE-SCRATCH ( -- )
   DEAD-CTX 8 IR-CTX:SCRATCH-TAKE 2drop ;

: STALE-METRIC ( -- )
   DEAD-CTX IR-CTX:MINTED drop ;

: STALE-CASES ( -- )
   s" a handle is dead after its context ends" T-LABEL
   DEAD-CTX IR-CTX:LIVE? TFALSE
   s" minting on a dead handle rejects" T-LABEL
   [: STALE-MINT ;] E-IR-CTX-STALE TTHROWSQ
   s" reading the binding on a dead handle rejects" T-LABEL
   [: STALE-BINDING ;] E-IR-CTX-STALE TTHROWSQ
   s" scratch on a dead handle rejects" T-LABEL
   [: STALE-SCRATCH ;] E-IR-CTX-STALE TTHROWSQ
   s" metrics on a dead handle reject" T-LABEL
   [: STALE-METRIC ;] E-IR-CTX-STALE TTHROWSQ ;

\ A retired slot reused by a sibling does not resurrect the old handle: the
\ generation, not the registry position, is the identity.
: REUSE-INNER ( IR-CTX:ctx IR-CTX:ctx -- IR-CTX:ctx bool )
   {: old:IR-CTX:ctx fresh:IR-CTX:ctx :}
   old IR-CTX:SERIAL IR-CTX:SERIAL-LIVE? TFALSE
   fresh IR-CTX:MINTED 0 T=
   fresh IR-CTX:LIVE?
   old IR-CTX:LIVE? 0= and
   old swap ;

: SLOT-REUSE-CASE ( -- )
   s" a reused registry slot does not revive the old handle" T-LABEL
   DEAD-CTX
   BND [: REUSE-INNER ;] IR-CTX:WITH-CONTEXT
   TTRUE
   IR-CTX:LIVE? TFALSE ;

\ ---- failure returns the owner -----------------------------------------------
\ After the named rejection is caught, the very handle that failed still names
\ its owning context serial, mirroring how an IR-ID value names its owner.
: OA-USE ( IR-CTX:ctx -- IR-CTX:ctx )
   dup IR-CTX:MINTED drop ;

: OWNER-CASE ( -- )
   s" a caught failure still names the owning context" T-LABEL
   DEAD-CTX dup IR-CTX:SERIAL {: s:n :}
   [: OA-USE ;] catch
   E-IR-CTX-STALE T=
   IR-CTX:SERIAL s T=
   s 0 > TTRUE ;

\ ---- scratch ownership -------------------------------------------------------
: SCR-BODY ( IR-CTX:ctx -- bool bool n bool )
   {: c:IR-CTX:ctx :}
   c 5 IR-CTX:SCRATCH-TAKE {: a:ptr au:n :}
   c 16 IR-CTX:SCRATCH-TAKE {: b:ptr bu:n :}
   au 5 = bu 16 = and
   $5A a c! a c@ $5A =
   c IR-CTX:SCRATCH-USED
   b a - 8 = ;

: SCR-CASE ( -- )
   s" scratch spans are aligned, writable, and metered" T-LABEL
   BND [: SCR-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE 24 T= TTRUE TTRUE ;

: SCR-GROW-BODY ( IR-CTX:ctx -- bool n ) {: c:IR-CTX:ctx :}
   c 5 IR-CTX:SCRATCH-TAKE drop {: a:ptr :}
   $5A a c!
   c $200000 IR-CTX:SCRATCH-TAKE drop {: b:ptr :}
   $6B b c! $7C b $1FFFFF + c!
   c $300000 IR-CTX:SCRATCH-TAKE drop {: d:ptr :}
   $8D d c! $9E d $2FFFFF + c!
   a c@ $5A = b c@ $6B = and b $1FFFFF + c@ $7C = and
   d c@ $8D = and d $2FFFFF + c@ $9E = and
   c IR-CTX:SCRATCH-USED ;

: SCR-HUGE-BODY ( IR-CTX:ctx -- )
   $7FFFFFFFFFFFFFFF IR-CTX:SCRATCH-TAKE 2drop ;

: SCR-HUGE ( -- )
   BND [: SCR-HUGE-BODY ;] IR-CTX:WITH-CONTEXT ;

: SCR-ZERO-BODY ( IR-CTX:ctx -- )
   0 IR-CTX:SCRATCH-TAKE 2drop ;

: SCR-ZERO ( -- )
   BND [: SCR-ZERO-BODY ;] IR-CTX:WITH-CONTEXT ;

: SCR-NEG-BODY ( IR-CTX:ctx -- )
   -1 IR-CTX:SCRATCH-TAKE 2drop ;

: SCR-NEG ( -- )
   BND [: SCR-NEG-BODY ;] IR-CTX:WITH-CONTEXT ;

: SCR-LIMIT-CASES ( -- )
   s" scratch growth preserves earlier spans and meters every allocation" T-LABEL
   BND [: SCR-GROW-BODY ;] IR-CTX:WITH-CONTEXT $500008 T= TTRUE
   s" a request that cannot include alignment and a header rejects" T-LABEL
   [: SCR-HUGE ;] E-IR-CTX-SCRATCH TTHROWSQ
   s" a zero-byte request rejects" T-LABEL
   [: SCR-ZERO ;] E-IR-CTX-SIZE TTHROWSQ
   s" a negative request rejects" T-LABEL
   [: SCR-NEG ;] E-IR-CTX-SIZE TTHROWSQ ;

\ ---- the offset interface ----------------------------------------------------
\ A span taken as an OFFSET is a position in the region rather than an address,
\ which is the form a record living inside the region may hold. Two takes step
\ the cursor by the aligned size of the first, and two SIBLING contexts are
\ handed the same position - which is the release, stated as a number.
: OFF-BODY ( IR-CTX:ctx -- n n )
   {: c:IR-CTX:ctx :}
   c 5 IR-CTX:SCRATCH-OFFSET {: a:n :}
   c 16 IR-CTX:SCRATCH-OFFSET {: b:n :}
   a
   b a - ;

: OFF-FIRST ( IR-CTX:ctx -- n )
   5 IR-CTX:SCRATCH-OFFSET ;

: ENCLOSED-TAKE ( IR-CTX:ctx IR-CTX:ctx -- )
   drop
   8 IR-CTX:SCRATCH-TAKE 2drop ;

: ENCLOSED-OUTER ( IR-CTX:ctx -- )
   BND [: ENCLOSED-TAKE ;] IR-CTX:WITH-CONTEXT ;

: ENCLOSED-RUN ( -- )
   BND [: ENCLOSED-OUTER ;] IR-CTX:WITH-CONTEXT ;

: OFF-CASES ( -- )
   s" a scratch take answers a position and the next one is aligned past it" T-LABEL
   BND [: OFF-BODY ;] IR-CTX:WITH-CONTEXT
   8 T= 0 > TTRUE
   s" two sibling contexts are handed the same position" T-LABEL
   BND [: OFF-FIRST ;] IR-CTX:WITH-CONTEXT
   BND [: OFF-FIRST ;] IR-CTX:WITH-CONTEXT T=
   s" a take for a context a deeper one encloses is refused" T-LABEL
   [: ENCLOSED-RUN ;] E-IR-CTX-NESTED TTHROWSQ
   s" the offset width and the region's limit are one number" T-LABEL
   1 IR-CTX:SCRATCH-OFFSET-BITS lshift IR-CTX:SCRATCH-LIMIT T= ;

\ ---- the release epoch -------------------------------------------------------
\ The counter moves when a release hands bytes back and at no other time, so
\ two objects allotted at one offset on either side of a release carry
\ different stamps and a stamp taken inside one context is still current
\ everywhere inside it.
variable EP-BEFORE

: EP-BODY ( IR-CTX:ctx -- n )
   drop
   IR-CTX:EPOCH ;

: EP-NESTED-BODY ( IR-CTX:ctx -- n )
   drop
   BND [: EP-BODY ;] IR-CTX:WITH-CONTEXT drop
   IR-CTX:EPOCH ;

: EP-CASES ( -- )
   s" the epoch stands still inside a context and moves when it leaves" T-LABEL
   IR-CTX:EPOCH EP-BEFORE !
   BND [: EP-BODY ;] IR-CTX:WITH-CONTEXT EP-BEFORE @ T=
   IR-CTX:EPOCH EP-BEFORE @ 1+ T=
   s" a nested context's release moves it for the context outside it" T-LABEL
   IR-CTX:EPOCH EP-BEFORE !
   BND [: EP-NESTED-BODY ;] IR-CTX:WITH-CONTEXT EP-BEFORE @ 1+ T= ;

\ ---- teardown releases every live child --------------------------------------
: DIRTY-BODY ( IR-CTX:ctx -- )
   dup IR-CTX:NEW-MODULE 2drop
   64 IR-CTX:SCRATCH-TAKE 2drop ;

: FRESH-BODY ( IR-CTX:ctx -- n n )
   dup IR-CTX:MINTED swap IR-CTX:SCRATCH-USED ;

: FRESH-CASE ( -- )
   s" no per-context state leaks into a sibling context" T-LABEL
   BND [: DIRTY-BODY ;] IR-CTX:WITH-CONTEXT
   BND [: FRESH-BODY ;] IR-CTX:WITH-CONTEXT
   0 T= 0 T= ;

\ The WITH-BYTES accounting proof: after a context ends, a fresh mapping of the
\ same size maps, is writable, and unmaps.
: ACCT-PROBE-BODY ( ptr u8 NUM:alloc-byte-len -- bool )
   drop {: p:ptr :}
   $A5 p c! p c@ $A5 = ;

: ACCT-PROBE ( -- )
   TMAP-BYTES MEM:BYTES-ALLOC-LEN [: ACCT-PROBE-BODY ;] MEM:WITH-BYTES TTRUE ;

: ACCT-CASE ( -- )
   s" teardown returns the context mapping to the OS" T-LABEL
   BND [: DIRTY-BODY ;] IR-CTX:WITH-CONTEXT
   ACCT-PROBE ;

\ ---- nesting -----------------------------------------------------------------
: NEST-INNER ( IR-CTX:ctx IR-CTX:ctx -- IR-CTX:ctx bool )
   {: outer:IR-CTX:ctx inner:IR-CTX:ctx :}
   inner IR-CTX:LIVE?
   outer IR-CTX:LIVE? and
   inner IR-CTX:SERIAL outer IR-CTX:SERIAL <> and
   outer swap ;

: NEST-OUTER ( IR-CTX:ctx -- bool bool n )
   dup IR-CTX:NEW-MODULE 2drop
   BND [: NEST-INNER ;] IR-CTX:WITH-CONTEXT
   swap
   dup IR-CTX:LIVE? swap
   IR-CTX:MINTED ;

: NEST-CASE ( -- )
   s" nested contexts are distinct and leave the outer intact" T-LABEL
   BND [: NEST-OUTER ;] IR-CTX:WITH-CONTEXT
   1 T= TTRUE TTRUE ;

\ ---- unbound module slots are fail-closed ------------------------------------
: UB-SRC-BODY ( IR-CTX:ctx -- )
   IR-CTX:SOURCES@ ;

: UB-SRC ( -- )
   BND [: UB-SRC-BODY ;] IR-CTX:WITH-CONTEXT ;

: UB-DIAG-BODY ( IR-CTX:ctx -- )
   IR-CTX:DIAG@ ;

: UB-DIAG ( -- )
   BND [: UB-DIAG-BODY ;] IR-CTX:WITH-CONTEXT ;

: UB-WIT-BODY ( IR-CTX:ctx -- )
   IR-CTX:WITNESSES@ ;

: UB-WIT ( -- )
   BND [: UB-WIT-BODY ;] IR-CTX:WITH-CONTEXT ;

\ ---- registry depth: exhaustion, and healing by the enclosing exit -----------
defer DEEP-STEP ( n IR-CTX:ctx -- n )

: DEEP-IMPL ( n IR-CTX:ctx -- n )
   {: c:IR-CTX:ctx :}
   1 -
   dup 0 > if
      BND [: DEEP-STEP ;] IR-CTX:WITH-CONTEXT
   else
      c IR-CTX:LIVE? TTRUE
      c IR-CTX:SERIAL IR-CTX:SERIAL-LIVE? TTRUE
      c IR-CTX:MINTED 0 T=
   then ;

: DEEP-INSTALL ( -- )
   [: DEEP-IMPL ;] is DEEP-STEP ;

: DEEP-RUN ( n -- n )
   BND [: DEEP-STEP ;] IR-CTX:WITH-CONTEXT ;

: DEPTH-THROW ( -- )
   100 DEEP-RUN drop ;

\ Runs the throw-through fixtures inside one harness context: each aborted
\ context leaves no reachable handle, the harness's own normal exit truncates
\ every abandoned registry slot back to its entry depth, and the mapping
\ accounting stays clean.
: HARNESS-BODY ( IR-CTX:ctx -- )
   drop
   s" an unbound source-registry slot rejects use" T-LABEL
   [: UB-SRC ;] E-IR-CTX-UNBOUND TTHROWSQ
   s" an unbound diagnostic-sink slot rejects use" T-LABEL
   [: UB-DIAG ;] E-IR-CTX-UNBOUND TTHROWSQ
   s" an unbound witness-allocator slot rejects use" T-LABEL
   [: UB-WIT ;] E-IR-CTX-UNBOUND TTHROWSQ
   s" an aborted context still released its mapping" T-LABEL
   ACCT-PROBE
   s" nesting past the registry capacity is a named error" T-LABEL
   [: DEPTH-THROW ;] E-IR-CTX-DEPTH TTHROWSQ ;

\ The 40-deep nest runs while earlier throw-through fixtures still hold a few
\ retired slots under the outer harness, so it proves the harness exit
\ reclaimed the ~55 slots its own aborted subtree left behind; the exact
\ full-capacity proof runs after the outer context ends (see RUN).
: HARNESS-CASE ( -- )
   BND [: HARNESS-BODY ;] IR-CTX:WITH-CONTEXT
   s" the harness exit reclaims its abandoned subtree" T-LABEL
   40 DEEP-RUN 0 T= ;

\ ---- a body that throws retires its context anyway ---------------------------
\ These run at TOP LEVEL, outside every context, and that is the whole point.
\ Every case above runs inside one outer context whose normal exit truncates the
\ registry, so none of them can see a slot that was never given back; the
\ failure path they miss is the one a compiler driver takes when it catches a
\ compilation error and carries on.
\
\ Until the entry caught its own body, both of these were false. The registry
\ went on reporting the abandoned serial LIVE although MEM:WITH-BYTES had
\ already released its mapping - and IR-ARENA and IR-BUILD both decide whether
\ their handles are usable by asking exactly that. The depth never came back
\ either, so sixty-five caught failures filled the sixty-four-slot registry and
\ every later entry answered E-IR-CTX-DEPTH instead of doing its work, which
\ replaced the body's own error with a capacity error about the previous
\ sixty-five.
variable ABANDONED

: THROW-BODY ( IR-CTX:ctx -- )
   dup IR-CTX:SERIAL ABANDONED !
   dup $200000 IR-CTX:SCRATCH-TAKE drop $5A swap c!
   dup $300000 IR-CTX:SCRATCH-TAKE drop $6B swap c!
   IR-CTX:SOURCES@ ;

: THROW-RUN ( -- )
   BND [: THROW-BODY ;] IR-CTX:WITH-CONTEXT ;

\ One more failure than the registry has slots, each caught where a driver would
\ catch it: at the top level, with no enclosing context to clean up after them.
: THROW-MANY ( -- )
   TDEPTH-MAX 1+ 0 ?do
      [: THROW-RUN ;] catch drop
   loop ;

: ABANDON-CASES ( -- )
   s" a body that throws reports its own error" T-LABEL
   [: THROW-RUN ;] E-IR-CTX-UNBOUND TTHROWSQ
   s" and its context is retired anyway: the serial it named is not live" T-LABEL
   ABANDONED @ IR-CTX:SERIAL-LIVE? TFALSE
   s" more caught failures than the registry holds do not fill it" T-LABEL
   THROW-MANY
   TDEPTH-MAX DEEP-RUN 0 T=
   s" and the error a body throws after them is still its own" T-LABEL
   [: THROW-RUN ;] E-IR-CTX-UNBOUND TTHROWSQ ;

\ ---- the checker seals the handle family -------------------------------------
: CHECKER-CASES ( -- )
   s" IRC-FORGE ( n -- IR-CTX:ctx )"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" IRC-ERASE ( IR-CTX:ctx -- n )"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" IRC-RAW ( n n -- ptr u8 n ) IR-CTX:SCRATCH-TAKE"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" IRC-MIX ( IR-CTX:ctx IR-CTX:ctx -- bool ) IR-ID:MODULE-SAME?"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" IRC-CTX-AS-KEY ( IR-CTX:ctx n -- IR-ID:ir-source-id ) IR-ID:PACK-SOURCE"
      CHECK-QUIET-CANDIDATE! 0 T= ;

\ Every context case runs inside one outermost context, so the registry slots
\ retired by throw-through fixtures (a context aborted by a throw keeps its
\ slots until the nearest enclosing live context leaves) all belong to this
\ context's subtree and its normal exit reclaims them.
: CASES-BODY ( IR-CTX:ctx -- )
   drop
   CREATE-CASE
   BINDING-CASE
   FORGED-CASE
   WITHIN-DISTINCT
   SIBLING-DISTINCT
   CTX-SERIAL-DISTINCT
   INVALID-SERIAL-CASES
   EXHAUST-CASES
   STALE-CASES
   SLOT-REUSE-CASE
   OWNER-CASE
   SCR-CASE
   SCR-LIMIT-CASES
   FRESH-CASE
   ACCT-CASE
   NEST-CASE
   HARNESS-CASE ;

\ Embedded bindings survive the same public table and ownership paths.

: EMBED-BND ( CTARGET:arch CTARGET:abi -- CBIND:binding )
   CTARGET-ENDIAN:LITTLE CTARGET-PTR--WIDTH:BITS32 CTARGET:F-BASE CTARGET:CONTRACT
   CNUM-OVERFLOW:TRAP CNUM-FLOAT--MODEL:IEEE754 CNUM-CONTRACTION:FORBIDDEN
   CNUM-FAST--MATH:BIT-EXACT CNUM-COMPARE:IEEE754-UNORDERED CNUM:POLICY
   CBIND:BIND ;

: EMBED-CASE ( CTARGET:arch CTARGET:abi -- )
   {: a:CTARGET:arch b:CTARGET:abi :}
   a b EMBED-BND [: IR-CTX:BINDING@ ;] IR-CTX:WITH-CONTEXT
   a b EMBED-BND CBIND:SAME? TTRUE ;

: EMBEDDED ( -- )
   CTARGET-ARCH:A32 CTARGET-ABI:AAPCS32 EMBED-CASE
   CTARGET-ARCH:THUMB2 CTARGET-ABI:AAPCS32 EMBED-CASE
   CTARGET-ARCH:C66X CTARGET-ABI:C6000-EABI EMBED-CASE ;

\ ---- a session and the definitions inside it ---------------------------------
\ A session is an unscoped context that outlives each definition, and a
\ definition is an ordinary child context nested inside it: everything the
\ definition takes goes back when it ends, on the ordinary path and on a throw,
\ while everything the session took before its first definition stays readable
\ for the whole load.
1 TYPED-BUFFER SESS-CTX IR-CTX:ctx
1 TYPED-BUFFER SESS-KEEP IR-ARENA:arena
1 TYPED-BUFFER SESS-INNER IR-ARENA:arena
variable SESS-USED

: SESS-C ( -- IR-CTX:ctx ) 0 SESS-CTX @ ;

: CTX-TAKE-ARENA ( IR-CTX:ctx n -- IR-ARENA:arena )
   {: c:IR-CTX:ctx v:n :}
   c 8 IR-ARENA:NEW {: a:IR-ARENA:arena :}
   c a v IR-ARENA:PUSH drop
   a ;

: DEF-TAKE ( IR-CTX:ctx -- )
   33 CTX-TAKE-ARENA 0 SESS-INNER ! ;

: SESS-STEP ( -- )
   BND [: DEF-TAKE ;] IR-CTX:WITH-CONTEXT ;

: SESS-READ-INNER ( -- ) 0 SESS-INNER @ 0 IR-ARENA:READ drop ;
: SESS-READ-KEEP ( -- n ) 0 SESS-KEEP @ 0 IR-ARENA:READ ;

: DEF-THROWER ( IR-CTX:ctx -- )
   DEF-TAKE
   E-IR-CTX-STATE throw ;

: SESS-THROW-STEP ( -- )
   BND [: DEF-THROWER ;] IR-CTX:WITH-CONTEXT ;

: SESS-CLOSE-NOW ( -- ) IR-CTX:SESSION-CLOSE ;
: DEF-CLOSER ( IR-CTX:ctx -- ) drop SESS-CLOSE-NOW ;
: SESS-CLOSE-STEP ( -- )
   BND [: DEF-CLOSER ;] IR-CTX:WITH-CONTEXT ;

: SESS-REOPEN ( -- ) BND IR-CTX:SESSION-OPEN drop ;

\ ---- a session belongs at the bottom of the registry -------------------------
\ A session outlives every scope, so it is the bottom row of the registry or it
\ is nothing. Opening one INSIDE a scope is what the first tier-1 definition of
\ a load asks for when its caller already holds a context of its own, and being
\ served it was the bug: the session took the row above the caller's, the
\ caller's own exit then retired the DEEPEST row - the session's - and what was
\ left was the caller's row live over a mapping that had been released (reading
\ an arena of it died of SIGSEGV), a session answering live with its context
\ retired, and E-IR-CTX-STATE from the next definition and from the next capture
\ ever after.
\
\ It is refused by name instead, and the scope that was refused keeps its OWN
\ row: what it took still reads inside it, and goes stale when IT exits, because
\ its exit retires the slot it took rather than whichever row is deepest.
1 TYPED-BUFFER INSIDE-KEEP IR-ARENA:arena

: OPEN-INSIDE ( -- ) BND IR-CTX:SESSION-OPEN drop ;

: READ-INSIDE ( -- n ) 0 INSIDE-KEEP @ 0 IR-ARENA:READ ;

: INSIDE-BODY ( IR-CTX:ctx -- )
   55 CTX-TAKE-ARENA 0 INSIDE-KEEP !
   s" a session opened inside a context is refused" T-LABEL
   [: OPEN-INSIDE ;] E-IR-CTX-STATE TTHROWSQ
   s" and no session stands afterwards" T-LABEL
   IR-CTX:SESSION-LIVE? TFALSE
   s" while the refused scope still reads what it took" T-LABEL
   READ-INSIDE 55 T= ;

: INSIDE-CASES ( -- )
   BND [: INSIDE-BODY ;] IR-CTX:WITH-CONTEXT
   s" and its exit retires the slot IT took, so that arena is stale" T-LABEL
   [: READ-INSIDE drop ;] E-IR-ARENA-STALE TTHROWSQ ;

\ A session is process-wide and cannot be nested, and a tier-1 load opens the
\ compiler's own and keeps it until the image is captured - so this process may
\ already hold one before the first case here runs. Standing it down is exactly
\ what a capture does, and the compiler opens a fresh one for its next
\ definition; the cases below are about the session THIS file takes.
: SESS-STAND-DOWN ( -- )
   IR-CTX:SESSION-LIVE? 0= if exit then
   IMAGE-LIFECYCLE:PREPARE ;

: SESSION-CASES ( -- )
   SESS-STAND-DOWN
   s" no session is open before one is" T-LABEL
   IR-CTX:SESSION-LIVE? TFALSE

   BND IR-CTX:SESSION-OPEN 0 SESS-CTX !
   s" the session is live once opened" T-LABEL
   IR-CTX:SESSION-LIVE? TTRUE

   s" a second session is refused while one is open" T-LABEL
   [: SESS-REOPEN ;] E-IR-CTX-STATE TTHROWSQ

   SESS-C 99 CTX-TAKE-ARENA 0 SESS-KEEP !
   SESS-C IR-CTX:SCRATCH-USED SESS-USED !

   s" closing the session inside a definition is refused" T-LABEL
   [: SESS-CLOSE-STEP ;] E-IR-CTX-STATE TTHROWSQ
   IR-CTX:SESSION-LIVE? TTRUE

   s" an arena taken inside a definition is stale after it" T-LABEL
   SESS-STEP
   [: SESS-READ-INNER ;] E-IR-ARENA-STALE TTHROWSQ

   s" a definition that throws leaves nothing behind either" T-LABEL
   [: SESS-THROW-STEP ;] E-IR-CTX-STATE TTHROWSQ
   [: SESS-READ-INNER ;] E-IR-ARENA-STALE TTHROWSQ
   SESS-C IR-CTX:SCRATCH-USED SESS-USED @ T=

   s" ten definitions leave the session's scratch where it started" T-LABEL
   10 0 ?do SESS-STEP loop
   SESS-C IR-CTX:SCRATCH-USED SESS-USED @ T=

   s" what the session took before its first definition still reads" T-LABEL
   SESS-READ-KEEP 99 T=

   IR-CTX:SESSION-CLOSE
   s" the session is gone once closed" T-LABEL
   IR-CTX:SESSION-LIVE? TFALSE
   s" closing a session that is not open is refused" T-LABEL
   [: SESS-CLOSE-NOW ;] E-IR-CTX-STATE TTHROWSQ
   s" what the session held is stale once it is closed" T-LABEL
   [: SESS-READ-KEEP drop ;] E-IR-ARENA-STALE TTHROWSQ

   INSIDE-CASES ;

\ ---- a capture takes the session with it -------------------------------------
\ The registry must be empty before an image is captured, and a session is a
\ registry row that no scope will ever leave. It is given back by an image
\ lifecycle callback the session itself registers when it is taken, so a
\ capture closes an idle session and refuses one with a definition running.
: DEF-PREPARE ( IR-CTX:ctx -- ) drop IMAGE-LIFECYCLE:PREPARE ;

: SESS-PREPARE-INSIDE ( -- )
   BND [: DEF-PREPARE ;] IR-CTX:WITH-CONTEXT ;

: CAPTURE-CASES ( -- )
   s" a capture closes an idle session" T-LABEL
   BND IR-CTX:SESSION-OPEN drop
   IMAGE-LIFECYCLE:PREPARE
   IR-CTX:SESSION-LIVE? TFALSE

   s" a capture taken inside a definition is refused" T-LABEL
   BND IR-CTX:SESSION-OPEN drop
   [: SESS-PREPARE-INSIDE ;] E-IR-CTX-STATE TTHROWSQ
   s" and leaves the session standing" T-LABEL
   IR-CTX:SESSION-LIVE? TTRUE
   IR-CTX:SESSION-CLOSE ;
public

: RUN ( -- )
   T-RESET
   SESSION-CASES
   CAPTURE-CASES
   EMBEDDED
   DEEP-INSTALL
   BND [: CASES-BODY ;] IR-CTX:WITH-CONTEXT
   s" the outer exit reclaims every retired slot" T-LABEL
   TDEPTH-MAX DEEP-RUN 0 T=
   ABANDON-CASES
   OFF-CASES
   EP-CASES
   CHECKER-CASES
   T-REPORT ;

;package

IR-CTX-TEST:RUN
