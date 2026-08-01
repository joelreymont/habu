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
require src/compiler/ir/context.f

package IR-CTX-TEST
private

$20000 constant TMAP-BYTES           \ pins the context mapping size
$20000 128 - constant TSCR-CAP       \ pins the scratch capacity behind the header
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

: SCR-CAP-BODY ( IR-CTX:ctx -- n )
   dup TSCR-CAP IR-CTX:SCRATCH-TAKE 2drop
   IR-CTX:SCRATCH-USED ;

: SCR-OVER-BODY ( IR-CTX:ctx -- )
   dup TSCR-CAP 8 - IR-CTX:SCRATCH-TAKE 2drop
   16 IR-CTX:SCRATCH-TAKE 2drop ;

: SCR-OVER ( -- )
   BND [: SCR-OVER-BODY ;] IR-CTX:WITH-CONTEXT ;

: SCR-HUGE-BODY ( IR-CTX:ctx -- )
   TMAP-BYTES IR-CTX:SCRATCH-TAKE 2drop ;

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
   s" scratch fills exactly to the mapping capacity" T-LABEL
   BND [: SCR-CAP-BODY ;] IR-CTX:WITH-CONTEXT TSCR-CAP T=
   s" a request past the remaining capacity rejects" T-LABEL
   [: SCR-OVER ;] E-IR-CTX-SCRATCH TTHROWSQ
   s" a request past the whole mapping rejects" T-LABEL
   [: SCR-HUGE ;] E-IR-CTX-SCRATCH TTHROWSQ
   s" a zero-byte request rejects" T-LABEL
   [: SCR-ZERO ;] E-IR-CTX-SIZE TTHROWSQ
   s" a negative request rejects" T-LABEL
   [: SCR-NEG ;] E-IR-CTX-SIZE TTHROWSQ ;

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
: ACCT-PROBE-BODY ( ptr u8 CAD-NUM:alloc-byte-len -- bool )
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
   drop 1 -
   dup 0 > if
      BND [: DEEP-STEP ;] IR-CTX:WITH-CONTEXT
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

public

: RUN ( -- )
   T-RESET
   DEEP-INSTALL
   BND [: CASES-BODY ;] IR-CTX:WITH-CONTEXT
   s" the outer exit reclaims every retired slot" T-LABEL
   TDEPTH-MAX DEEP-RUN 0 T=
   CHECKER-CASES
   T-REPORT ;

;package

IR-CTX-TEST:RUN
