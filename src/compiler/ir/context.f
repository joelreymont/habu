\ context.f - the owned compiler context: one explicit owner per compilation.
\
\ docs/compiler-ir-design.md section 6.2. Every compilation gets exactly one
\ context, and the context is the single owner of the compilation's target and
\ numerical policy, its module serial allocation, and its scratch memory. The
\ slots for the source registry, the diagnostic sink, and the witness allocator
\ exist from birth but stay in an explicit unbound state that rejects use until
\ those modules land; nothing is stubbed silently.
\
\ OWNERSHIP SHAPE. A context can only be obtained inside a quotation passed to
\ WITH-CONTEXT, which is built over MEM:WITH-BYTES, so the mapping that backs
\ every context-owned allocation is released on both the normal path and the
\ throw path by construction. This file adds no MEM:RELEASE-BYTES call site of
\ its own. Habu's linear type facilities should ultimately enforce this
\ ownership; until then the design-sanctioned mechanism is the explicit
\ generation token below with fail-closed lifecycle checks.
\
\ STALE HANDLES. A handle is the context's generation: a nonzero, monotonic,
\ never-reused serial taken from this package's atomic counter. A registry keeps
\ (generation, mapping base) for the contexts that are live right now; every
\ operation resolves its handle against that registry and a miss throws
\ E-IR-CTX-STALE. Checked code cannot keep a handle across a throw - the throw
\ truncates the data stack to the catch point, and re-minting a stored raw cell
\ back into the handle family is sealed to this package - so a context abandoned
\ by a throw leaves no reachable handle. Its registry slots are reclaimed when
\ the nearest enclosing live context leaves normally: leaving truncates the
\ registry back to the depth saved at entry, which releases every live child in
\ one step. A throw caught outside every context can therefore retire registry
\ slots for the rest of the process; the capacity below bounds that, the
\ exhaustion error is named, and the linear-ownership work removes the whole
\ mechanism.
\
\ PERSISTED STATE. All per-context state lives in the context's own mapping as
\ eight-byte little-endian slots written with the canonical CDIGEST slot words:
\ the module-mint count and ceiling, the scratch cursor, the unbound module
\ slots, and the bound target/policy pair. The pair is persisted as the stable
\ wire codes read straight out of the components' canonical preimages
\ (CTARGET:ENCODE / CNUM:ENCODE), and reading it back reconstructs the value
\ through the validating constructors and CBIND:BIND, so a forged or corrupted
\ header can never produce an unvalidated binding. Creation round-trips the
\ staged codes through that same decoder and compares with CBIND:SAME?, so a
\ component schema change that renumbers a wire code fails closed at creation
\ instead of decoding a different machine later.
\
\ CONCURRENCY. The generation counter is atomic, so serials stay unique if
\ tasks race. The registry itself is one process-wide stack and assumes the
\ current single-task compilation discipline, like the digest state it builds
\ on (src/compiler/digest.f header); concurrent compilation needs per-task
\ registries first.

require lib/prelude.f
require lib/errors.f
require lib/memory.f
require src/compiler/digest.f
require src/compiler/binding.f
require src/compiler/ir/id.f

package IR-CTX
public

NEWTYPE ctx 0

private

CAST: MINT-CTX ( n -- IR-CTX:ctx )
CAST: CTX>N ( IR-CTX:ctx -- n )

\ ---- capacities and layout ---------------------------------------------------
$7FFFFFFF constant SERIAL-CEILING    \ production per-context module ceiling; the
                                     \ full IR-ID module serial range
$7FFFFFFF constant GEN-MAX           \ context generation ceiling
64 constant DEPTH-MAX                \ live + retired registry slots
$80000 constant MAP-BYTES            \ one 512K mapping per context. It was 64K
                                     \ while one module of the machine dialect
                                     \ cost about seventeen kilobytes; the
                                     \ dialect's byte-width memory forms tipped
                                     \ a geometrically grown table over its next
                                     \ doubling and took registration past
                                     \ twenty-seven, and the spill lowering
                                     \ holds TWO modules of that dialect in one
                                     \ context - the old module it reads and the
                                     \ rewritten one it builds. That took it to
                                     \ 128K. Compiling a real definition that
                                     \ spills holds THREE: the source module the
                                     \ elaborator filled is still live when the
                                     \ machine module is rewritten, because the
                                     \ whole run is one context. Each time it is
                                     \ a real pass of the compiler rather than a
                                     \ fixture, so the mapping is what gives.
                                     \ Instruction combining
                                     \ (src/compiler/native/combine.f) makes it
                                     \ FOUR for a definition that both combines
                                     \ and spills - the source module, the
                                     \ selected one, the combined one it writes
                                     \ from that, and the spill lowering's - so
                                     \ 256K refused with E-IR-CTX-SCRATCH the
                                     \ first time the pass ran over the whole
                                     \ comparison corpus, and this is 512K for
                                     \ the same reason the last two doublings
                                     \ were. A pass that holds one more module
                                     \ at once is what moves this number; a
                                     \ pass that hands its input straight back,
                                     \ as combining does for a routine with no
                                     \ pair in it, holds none and does not.

\ Header slots inside the mapping, one CDIGEST slot each.
0 constant HF-MINTED                 \ modules minted by this context
1 constant HF-CEIL                   \ this context's module ceiling
2 constant HF-OFF                    \ scratch cursor, bytes from the base
3 constant HF-CODE0                  \ first of the ten binding wire-code slots
13 constant HF-SOURCES               \ source registry slot (unbound)
14 constant HF-DIAG                  \ diagnostic sink slot (unbound)
15 constant HF-WITNESS               \ witness allocator slot (unbound)
16 constant HDR-SLOTS
HDR-SLOTS CDIGEST:SLOT-BYTES * constant HDR-BYTES
MAP-BYTES HDR-BYTES - constant SCRATCH-CAP
0 constant SLOT-UNBOUND

\ Wire-code slots relative to a ten-slot code window: five target fields then
\ five policy fields, in each component's canonical preimage order.
0 constant CS-ARCH
1 constant CS-ABI
2 constant CS-ENDIAN
3 constant CS-PTRW
4 constant CS-FEAT
5 constant CS-OVF
6 constant CS-FLOAT
7 constant CS-CONTR
8 constant CS-FAST
9 constant CS-CMP
10 constant CODES#
2 constant COMP-CODE0                \ first semantic slot of a component preimage
5 constant COMP-CODE#                \ semantic slots per component preimage
7 CDIGEST:SLOT-BYTES * constant COMP-PRE-BYTES

\ ---- registry storage --------------------------------------------------------
here CELL 1- and CELL swap - CELL 1- and allot
variable GEN-CELL
0 GEN-CELL !
variable DEPTH
0 DEPTH !
create GENS DEPTH-MAX cells allot
create BASES DEPTH-MAX cells allot
create BODIES DEPTH-MAX cells allot
create STAGE CODES# CDIGEST:SLOT-BYTES * allot

: GEN@ ( n -- n )
   cells GENS + @ ;

: GEN! ( n n -- )
   cells GENS + ! ;

\ The quotation the context at this depth is running. It is parked beside that
\ context's generation, in the registry's own stack, because the entry below has
\ to catch the body and a checked catch takes a stack-neutral quotation while
\ this body consumes the minted handle and leaves the caller's own result row.
\ Parking it per depth rather than in one cell is what makes nesting need no
\ save-and-restore ceremony: each level's body is written before the depth
\ reaches it, exactly as its generation is.
: BODY@ ( n -- n )
   cells BODIES + @ ;

: BODY! ( n n -- )
   cells BODIES + ! ;

: BASE-FIELD ( n -- ptr ptr u8 )
   cells BASES + 0 ptr-field ;

\ ---- header slot access ------------------------------------------------------
: HDR@ ( ptr u8 n -- n )
   CDIGEST:SLOT@ ;

: HDR! ( n ptr u8 n -- )
   CDIGEST:SLOT! ;

: CNT-OK ( n -- n )
   dup 0 < if E-IR-CTX-STATE throw then ;

: OFF-OK ( n -- n )
   dup HDR-BYTES < over MAP-BYTES > or if E-IR-CTX-STATE throw then ;

\ ---- generation serials ------------------------------------------------------
: GEN-NEXT-N ( n -- n )
   dup 0 < over GEN-MAX >= or if E-IR-CTX-SERIALS throw then
   1+ ;

: TRY-GEN ( -- n bool )
   GEN-CELL atomic@ {: current:n :}
   current GEN-NEXT-N {: next:n :}
   current next GEN-CELL atomic-cas current =
   if next 0 0= else 0 0 0 <> then ;

: TAKE-GEN ( -- n )
   begin
      TRY-GEN dup 0=
   while
      2drop
   repeat
   drop ;

\ ---- handle resolution -------------------------------------------------------
: FIND-SLOT ( n -- n )
   {: g:n :}
   -1
   DEPTH @ 0 ?do
      g i GEN@ = if drop i leave then
   loop ;

: RESOLVE ( IR-CTX:ctx -- ptr u8 )
   CTX>N FIND-SLOT
   dup 0 < if E-IR-CTX-STALE throw then
   BASE-FIELD @ ;

\ ---- binding wire codes: decode ----------------------------------------------
\ The codes are the components' stable canonical wire codes; a slot outside the
\ published vocabulary is corrupted context state.
: N>ARCH ( n -- CTARGET:arch )
   case
      0 of CTARGET-ARCH:AARCH64 endof
      1 of CTARGET-ARCH:PTX endof
      E-IR-CTX-STATE throw
   endcase ;

: N>ABI ( n -- CTARGET:abi )
   case
      0 of CTARGET-ABI:AAPCS64-DARWIN endof
      1 of CTARGET-ABI:AAPCS64-LINUX endof
      2 of CTARGET-ABI:PTX-KERNEL endof
      E-IR-CTX-STATE throw
   endcase ;

: N>ENDIAN ( n -- CTARGET:endian )
   case
      0 of CTARGET-ENDIAN:LITTLE endof
      1 of CTARGET-ENDIAN:BIG endof
      E-IR-CTX-STATE throw
   endcase ;

: N>PTRW ( n -- CTARGET:ptr-width )
   case
      0 of CTARGET-PTR--WIDTH:BITS32 endof
      1 of CTARGET-PTR--WIDTH:BITS64 endof
      E-IR-CTX-STATE throw
   endcase ;

: N>OVF ( n -- CNUM:overflow )
   case
      0 of CNUM-OVERFLOW:WRAP endof
      1 of CNUM-OVERFLOW:TRAP endof
      E-IR-CTX-STATE throw
   endcase ;

: N>FLOAT ( n -- CNUM:float-model )
   case
      0 of CNUM-FLOAT--MODEL:IEEE754 endof
      1 of CNUM-FLOAT--MODEL:FLUSH-DENORMAL endof
      E-IR-CTX-STATE throw
   endcase ;

: N>CONTR ( n -- CNUM:contraction )
   case
      0 of CNUM-CONTRACTION:FORBIDDEN endof
      1 of CNUM-CONTRACTION:ALLOWED endof
      E-IR-CTX-STATE throw
   endcase ;

: N>FAST ( n -- CNUM:fast-math )
   case
      0 of CNUM-FAST--MATH:BIT-EXACT endof
      1 of CNUM-FAST--MATH:REASSOCIATE endof
      2 of CNUM-FAST--MATH:APPROXIMATE endof
      E-IR-CTX-STATE throw
   endcase ;

: N>CMP ( n -- CNUM:compare )
   case
      0 of CNUM-COMPARE:IEEE754-UNORDERED endof
      1 of CNUM-COMPARE:TOTAL-ORDER endof
      2 of CNUM-COMPARE:ASSUME-ORDERED endof
      E-IR-CTX-STATE throw
   endcase ;

\ Reconstruct the target contract from a ten-slot code window. Every value goes
\ back through the validating constructors, so the result is always a coherent
\ contract or a named throw.
: CODES-TARGET@ ( ptr u8 -- CTARGET:contract )
   dup CS-ARCH HDR@ N>ARCH swap
   dup CS-ABI HDR@ N>ABI swap
   dup CS-ENDIAN HDR@ N>ENDIAN swap
   dup CS-PTRW HDR@ N>PTRW swap
   CS-FEAT HDR@ CTARGET:FEATURE-SET
   CTARGET:CONTRACT ;

: CODES-POLICY@ ( ptr u8 -- CNUM:numeric-policy )
   dup CS-OVF HDR@ N>OVF swap
   dup CS-FLOAT HDR@ N>FLOAT swap
   dup CS-CONTR HDR@ N>CONTR swap
   dup CS-FAST HDR@ N>FAST swap
   CS-CMP HDR@ N>CMP
   CNUM:POLICY ;

: CODES-BINDING@ ( ptr u8 -- CBIND:binding )
   dup CODES-TARGET@ swap CODES-POLICY@ CBIND:BIND ;

: CODES-AT ( ptr u8 n -- ptr u8 )
   CDIGEST:SLOT-BYTES * + ;

\ ---- binding wire codes: stage on creation -----------------------------------
\ Copy one component's five semantic preimage slots into the staging window.
: STAGE-CODES! ( ptr u8 n n -- )
   {: dst0:n :}
   COMP-PRE-BYTES <> if E-IR-CTX-STATE throw then
   COMP-CODE# 0 ?do
      dup COMP-CODE0 i + HDR@
      STAGE dst0 i + HDR!
   loop
   drop ;

: STAGE-TARGET! ( ptr u8 n -- )
   CS-ARCH STAGE-CODES! ;

: STAGE-POLICY! ( ptr u8 n -- )
   CS-OVF STAGE-CODES! ;

\ Persist a validated binding as wire codes and prove the codes decode back to
\ the same binding before any context is built on them.
: STAGE-BINDING ( CBIND:binding -- )
   dup CBIND:TARGET@ CTARGET:ENCODE STAGE-TARGET!
   dup CBIND:POLICY@ CNUM:ENCODE STAGE-POLICY!
   STAGE CODES-BINDING@ CBIND:SAME? 0= if E-IR-CTX-STATE throw then ;

: STAGE>HDR ( ptr u8 -- )
   CODES# 0 ?do
      STAGE i HDR@
      over HF-CODE0 i + HDR!
   loop
   drop ;

\ ---- context entry and teardown ----------------------------------------------
: CEIL-OK ( n -- )
   dup 1 < over SERIAL-CEILING > or if E-IR-CTX-CEILING throw then
   drop ;

: DEPTH-ROOM ( -- )
   DEPTH @ DEPTH-MAX >= if E-IR-CTX-DEPTH throw then ;

: CTX-ALLOC-LEN ( -- CAD-NUM:alloc-byte-len )
   MAP-BYTES MEM:BYTES-ALLOC-LEN ;

\ Install one registry slot: record the mapping base, reset the counters, mark
\ the not-yet-landed module slots unbound, and copy the staged binding codes.
: CTX-INSTALL ( n ptr u8 n -- )
   {: slot:n :}
   dup slot BASE-FIELD !
   swap over HF-CEIL HDR!
   0 over HF-MINTED HDR!
   HDR-BYTES over HF-OFF HDR!
   SLOT-UNBOUND over HF-SOURCES HDR!
   SLOT-UNBOUND over HF-DIAG HDR!
   SLOT-UNBOUND over HF-WITNESS HDR!
   STAGE>HDR ;

\ ---- leaving, on both paths ---------------------------------------------------
\ RETIREMENT IS UNCONDITIONAL, AND THAT IS THE WHOLE OF THIS SECTION. These two
\ writes used to sit after the body with nothing catching it, so a body that
\ threw skipped both: MEM:WITH-BYTES released the mapping on its way out, and
\ the registry went on reporting that serial LIVE over storage that was gone.
\ IR-ARENA and IR-BUILD both decide whether their own handles are usable by
\ asking IR-CTX:SERIAL-LIVE?, so that answer was the difference between a
\ refusal and a read through a dangling pointer. The depth never came back
\ either, so sixty-five caught failures filled the registry and every later
\ entry answered E-IR-CTX-DEPTH instead of doing its work - the body's own error
\ replaced by a capacity error about the previous sixty-five.
\
\ TRUNCATING THE DEPTH IS WHAT RELEASES THE CHILDREN. FIND-SLOT scans only below
\ DEPTH, so putting it back to the entry depth stops every slot at or above it
\ from resolving, in one step; the next install rewrites the slot it lands in
\ before the depth reaches it again. That is why this is the same pair of writes
\ the normal path always made, and not a second, larger cleanup.
: CTX-RETIRE ( n -- ) {: at:n :}
   0 at GEN!
   at DEPTH ! ;

\ Run the body of the context at the current depth, which is this scope's own
\ frame: CTX-ENTER writes the depth one above the frame it installed and then
\ calls CE-SCOPE, which calls this as the first thing inside the catch, so
\ nothing runs in between that could have pushed another. The handle is minted
\ here, out of the generation the registry already holds, so no sealed handle
\ has to travel through the parking. Note that the RETIREMENT does not read the
\ depth - CE-SCOPE holds its frame in a local - so a body that somehow left the
\ depth elsewhere cannot make this word retire somebody else's frame.
\
\ Trusted for one reason: it executes a fetched execution token whose effect -
\ the caller's own result row - the checker cannot state.
\ Retirement owner: habu-epic-type-habu-a34713f0.
TRUSTED: CE-RUN ( -- )
   DEPTH @ 1- dup GEN@ MINT-CTX swap BODY@ execute ;

\ Run it, retire this context whatever became of it, and then let the body's
\ error out. Trusted for the catch alone: a checked catch takes a stack-neutral
\ quotation and this one leaves the caller's result row. The retirement above it
\ is ordinary checked code, and `at` is a local because a local is the only
\ storage that survives both paths out of one frame - on the throw path the data
\ stack is truncated to the catch point, and on the normal path the body's
\ result row is sitting on top of anything that was left there.
\ typed-local-lint: allow-bare-local - the caught code is the caller's, and its
\ effect is the row-polymorphic one this word's own signature carries.
TRUSTED: CE-SCOPE ( R [ R IR-CTX:ctx -- S ] n -- S )
   {: at:n :}
   at BODY!
   [: CE-RUN ;] catch
   at CTX-RETIRE
   dup 0 <> if throw then
   drop ;

\ The WITH-BYTES body: build the context in the fresh mapping, run the caller's
\ quotation with the minted handle, then retire this slot and every deeper one
\ before the mapping is released.
: CTX-ENTER ( R [ R IR-CTX:ctx -- S ] n ptr u8 CAD-NUM:alloc-byte-len -- S )
   drop
   DEPTH-ROOM
   DEPTH @ TAKE-GEN {: at:n g:n :}
   at CTX-INSTALL
   g at GEN!
   at 1+ DEPTH !
   at CE-SCOPE ;

public

\ ---- creation ----------------------------------------------------------------
\ WITH-CONTEXT-BOUND is the one entry point; the module-serial ceiling is a
\ creation parameter so the exhaustion path is testable, and WITH-CONTEXT is
\ the production spelling that passes the full serial range. The binding is
\ revalidated before anything is built on it.
: WITH-CONTEXT-BOUND ( R CBIND:binding n [ R IR-CTX:ctx -- S ] -- S )
   {: ceil:n body :} \ typed-local-lint: allow-bare-local - body carries the row-polymorphic quotation effect
   ceil CEIL-OK
   CBIND:VALIDATE STAGE-BINDING
   body ceil CTX-ALLOC-LEN [: CTX-ENTER ;] MEM:WITH-BYTES ;

: WITH-CONTEXT ( R CBIND:binding [ R IR-CTX:ctx -- S ] -- S )
   SERIAL-CEILING swap WITH-CONTEXT-BOUND ;

\ ---- identity ----------------------------------------------------------------
\ The context's own serial. A pure projection: it still answers on a stale
\ handle, so a caught failure can always name the owning context.
: SERIAL ( IR-CTX:ctx -- n )
   CTX>N ;

: LIVE? ( IR-CTX:ctx -- bool )
   CTX>N FIND-SLOT 0 < 0= ;

\ Liveness observed through a raw serial. Child modules that outlive a single
\ call (the IR arena) can persist their owner only as the context serial,
\ because handles are sealed nominals a stored raw cell cannot re-mint; they
\ still must observe owner teardown fail-closed before touching context-owned
\ storage. A boolean probe mints no handle and exposes no pointer, so it adds
\ no forging or access power beyond what LIVE? already publishes.
: SERIAL-LIVE? ( n -- bool )
   FIND-SLOT 0 < 0= ;

\ ---- bound target and policy -------------------------------------------------
: BINDING@ ( IR-CTX:ctx -- CBIND:binding )
   RESOLVE HF-CODE0 CODES-AT CODES-BINDING@ ;

\ ---- module allocation -------------------------------------------------------
private

\ Reserve one mint against the context ceiling. The count is written before the
\ IR-ID take, so a global serial-exhaustion throw can never leave a module this
\ context took but did not account for.
: MINT-TAKE ( ptr u8 -- )
   dup HF-MINTED HDR@ CNT-OK
   over HF-CEIL HDR@ CNT-OK {: m:n c:n :}
   m c >= if E-IR-CTX-SERIALS throw then
   m 1+ swap HF-MINTED HDR! ;

public

\ Mint a module identity owned by this context. Serials come from the IR-ID
\ authority, so they are nonzero, monotonic, and never reused - across sibling
\ contexts as well as within one - and this context's ceiling bounds how many
\ it may take.
: NEW-MODULE ( IR-CTX:ctx -- IR-ID:ir-module-key IR-ID:ir-module-id )
   RESOLVE MINT-TAKE IR-ID:NEW-MODULE ;

: MINTED ( IR-CTX:ctx -- n )
   RESOLVE HF-MINTED HDR@ CNT-OK ;

\ ---- scratch -----------------------------------------------------------------
private

: ALIGN8 ( n -- n )
   7 + 8 / 8 * ;

public

\ Bump-allocate a byte span from the context's mapping. The span dies with the
\ context; exhaustion and bad sizes are named errors, never a wrap.
: SCRATCH-TAKE ( IR-CTX:ctx n -- ptr u8 n )
   {: need:n :}
   need 1 < if E-IR-CTX-SIZE throw then
   need SCRATCH-CAP > if E-IR-CTX-SCRATCH throw then
   RESOLVE
   dup HF-OFF HDR@ OFF-OK {: off:n :}
   need ALIGN8 {: step:n :}
   off step + MAP-BYTES > if E-IR-CTX-SCRATCH throw then
   off step + over HF-OFF HDR!
   off + need ;

: SCRATCH-USED ( IR-CTX:ctx -- n )
   RESOLVE HF-OFF HDR@ OFF-OK HDR-BYTES - ;

\ ---- not-yet-landed module slots ---------------------------------------------
private

: SLOT-CHECK ( ptr u8 n -- )
   HDR@ SLOT-UNBOUND <> if E-IR-CTX-STATE throw then
   E-IR-CTX-UNBOUND throw ;

public

\ Fail-closed unbound slots: each throws E-IR-CTX-UNBOUND until its owning
\ module lands and binds it, and anything else in the slot is corrupted state.
: SOURCES@ ( IR-CTX:ctx -- )
   RESOLVE HF-SOURCES SLOT-CHECK ;

: DIAG@ ( IR-CTX:ctx -- )
   RESOLVE HF-DIAG SLOT-CHECK ;

: WITNESSES@ ( IR-CTX:ctx -- )
   RESOLVE HF-WITNESS SLOT-CHECK ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;package
