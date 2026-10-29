\ context.f - the owned compiler context: one explicit owner per compilation of
\ its target binding, numeric policy, module serials and scratch memory.
\ Scratch is one process-wide region with a bump cursor: entering a context
\ marks the cursor, leaving releases to the mark, and nothing is freed in
\ between. Only the innermost context may take (TOP-CK).

require src/lib/prelude.f
require src/lib/errors.f
require src/lib/memory.f
require src/compiler/binding.f
require src/compiler/ir/id.f
require src/lib/image-lifecycle.f
require src/compiler/workspace.f

package IR-CTX
public

NEWTYPE ctx

private

CAST: MINT-CTX ( n -- IR-CTX:ctx )
CAST: CTX>N ( IR-CTX:ctx -- n )

\ ---- capacities and layout ---------------------------------------------------
$7FFFFFFF constant SERIAL-CEILING    \ production per-context module ceiling; the
                                     \ full IR-ID module serial range
$7FFFFFFF constant GEN-MAX           \ context generation ceiling
64 constant DEPTH-MAX                \ live + retired registry slots
6 constant SLOT-BITS
DEPTH-MAX 1- constant SLOT-MASK
\ The region is reserved, not committed: pages cost nothing until touched. The
\ engine self-build's high-water was 2,885,144 bytes (2026-09-16). The size is a
\ width because a child packing an offset beside other fields needs the width.
26 constant OFFSET-BITS               \ the width of a region offset
1 OFFSET-BITS lshift constant REGION-BYTES   \ 64 MiB of reserved scratch address space
$7FFFFFFFF constant EPOCH-MAX         \ region release epoch ceiling

STRUCTURE header DERIVE addr
   FIELD minted n
   FIELD ceil n
   FIELD used n
   FIELD mark n
   FIELD binding CBIND:binding
;STRUCTURE

CAST: HEADER-PTR ( ptr u8 -- ptr header )

\ ---- registry storage --------------------------------------------------------
\ The registry is one process-wide stack and assumes single-task compilation,
\ like src/compiler/digest.f; only the generation counter is atomic.
align
variable GEN-CELL
0 GEN-CELL !
variable DEPTH
0 DEPTH !
DEPTH-MAX CWORK-SPAN HANDLES n
DEPTH-MAX CWORK-BUFFER BASES ptr u8
DEPTH-MAX CWORK-SPAN ENTERED n   \ scope depth -> the slot that scope took
0 CWORK-VARIABLE ENTERED-N n

: HANDLE! ( n n -- )
   cells HANDLES + ! ;

: CNT-OK ( n -- n )
   dup 0 < if E-IR-CTX-STATE throw then ;

\ ---- the scratch region ------------------------------------------------------
\ Mapped on the first take and unmapped only at capture: per-compilation
\ mappings paid the kernel for the same pages again per definition.
align
PTR-VARIABLE REGION-BASE
NULL-PTR REGION-BASE !
variable REGION-HERE
0 REGION-HERE !

: REGION-ALLOC-LEN ( -- NUM:alloc-byte-len )
   REGION-BYTES MEM:BYTES-ALLOC-LEN ;

\ ---- the release epoch -------------------------------------------------------
\ EPOCH counts releases so a reused offset cannot pass for the one it replaced.
\ A child stamps the epoch into the identity cell it keeps at an offset and
\ compares its reference's stamp with that cell, never with the current epoch:
\ a session-lived object is older than the counter by every definition since.
variable REGION-EPOCH
0 REGION-EPOCH !

: EPOCH-BUMP ( -- )
   REGION-EPOCH @ dup EPOCH-MAX >= if E-IR-CTX-SERIALS throw then
   1+ REGION-EPOCH ! ;

\ ---- who is told where the region is -----------------------------------------
\ A child resolving offsets keeps the base itself (it reads it millions of times
\ per compilation), so this package pushes the base when the mapping appears
\ and NULL-PTR when it goes. Installed once, for RETIRE-CHILDREN!'s reason.
defer REBASE-CHILD ( ptr u8 -- )

variable REBASE-SET
0 REBASE-SET !

: KEEP-BASE ( ptr u8 -- )
   drop ;

: REBASE-RESET ( -- )
   [: KEEP-BASE ;] is REBASE-CHILD ;
REBASE-RESET

public

: REGION-REBASE! ( [ ptr u8 -- ] -- )
   REBASE-SET @ 0<> if E-IR-CTX-STATE throw then
   1 REBASE-SET !
   is REBASE-CHILD ;

private

\ The cursor is zero whenever there is no mapping: only capture preparation
\ unmaps, and it runs with no context open.
: REGION-OPEN ( -- )
   REGION-BASE @ NULL-PTR = 0= if exit then
   REGION-ALLOC-LEN MEM:ALLOC-BYTES drop REGION-BASE !
   0 REGION-HERE !
   REGION-BASE @ REBASE-CHILD ;

: REGION-CLOSE ( -- )
   REGION-BASE @ NULL-PTR = if exit then
   REGION-BASE @ REGION-ALLOC-LEN MEM:RELEASE-BYTES
   NULL-PTR REGION-BASE !
   0 REGION-HERE !
   NULL-PTR REBASE-CHILD ;

: ALIGN8 ( n -- n ) 7 + 8 / 8 * ;

\ Refuses before the cursor moves, so a full region stays usable.
: REGION-ALLOT ( n -- n ) {: need:n :}
   REGION-OPEN
   need ALIGN8 {: step:n :}
   REGION-HERE @ {: off:n :}
   step REGION-BYTES off - > if E-IR-CTX-SCRATCH throw then
   off step + REGION-HERE !
   off ;

\ The only place an offset becomes an address; unchecked, because every caller
\ holds an offset the cursor has already passed.
: REGION-AT ( n -- ptr u8 )
   REGION-BASE @ + ;

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
: PACK-HANDLE ( n n -- n )
   swap SLOT-BITS lshift or ;

\ A handle carries its registry slot in its low bits and the row holds the whole
\ handle. The load is inlined: SERIAL-LIVE? runs it on every IR arena
\ resolution, and a call frame was most of the probe's cost.
: FIND-SLOT ( n -- n )
   dup 0= if drop -1 exit then
   dup SLOT-MASK and tuck cells HANDLES + @ = if else drop -1 then ;

: RESOLVE ( IR-CTX:ctx -- ptr u8 )
   CTX>N FIND-SLOT
   dup 0 < if E-IR-CTX-STALE throw then
   BASES @ ;

\ A span taken for a context that a deeper live one encloses lies above the
\ deeper mark, so the deeper release would reclaim it while the owner still
\ names it.
: TOP-CK ( n -- )
   DEPTH @ 1- <> if E-IR-CTX-NESTED throw then ;

: RESOLVE-TOP ( IR-CTX:ctx -- ptr u8 )
   CTX>N FIND-SLOT
   dup 0 < if E-IR-CTX-STALE throw then
   dup TOP-CK
   BASES @ ;

\ ---- child registries --------------------------------------------------------
\ Child registries (the IR arena registry) keep rows owned by this context's
\ serial. Teardown announces the dying serial before releasing storage, because
\ a scoped arena reader resolves once and has no later touch to probe on.
\ Installed once: a second install is refused, since replacing the first would
\ silently stop its rows being retired.
defer RETIRE-CHILDREN ( n -- )

variable CHILDREN-SET
0 CHILDREN-SET !

: KEEP-CHILDREN ( n -- )
   drop ;

: CHILDREN-RESET ( -- )
   [: KEEP-CHILDREN ;] is RETIRE-CHILDREN ;
CHILDREN-RESET

public

\ Takes the retirement as an xt so the installing package keeps it private.
: RETIRE-CHILDREN! ( [ n -- ] -- )
   CHILDREN-SET @ 0<> if E-IR-CTX-STATE throw then
   1 CHILDREN-SET !
   is RETIRE-CHILDREN ;

private

\ ---- context entry and teardown ----------------------------------------------
: CEIL-OK ( n -- )
   dup 1 < over SERIAL-CEILING > or if E-IR-CTX-CEILING throw then
   drop ;

: DEPTH-ROOM ( -- )
   DEPTH @ DEPTH-MAX >= if E-IR-CTX-DEPTH throw then ;

: CTX-INSTALL ( CBIND:binding n ptr u8 n n -- )
   {: binding:CBIND:binding ceil:n base:ptr slot:n mark:n :}
   base slot BASES !
   0 base HEADER-PTR HEADER-MINTED !
   ceil base HEADER-PTR HEADER-CEIL !
   0 base HEADER-PTR HEADER-USED !
   mark base HEADER-PTR HEADER-MARK !
   binding base HEADER-PTR HEADER-BINDING ! ;

\ ---- leaving, on both paths --------------------------------------------------
\ Retires the slot its owner took, which is not always the deepest. DEPTH and the
\ cursor fall back only when this slot is the top: rows above a non-top slot are
\ live and read storage above its mark, which the top's release reclaims later.
\ A release is also the epoch step.
: CTX-RETIRE ( n -- ) {: at:n :}
   at cells HANDLES + @ RETIRE-CHILDREN
   0 at HANDLE!
   at DEPTH @ 1- = if
      at BASES @ HEADER-PTR HEADER-MARK @ REGION-HERE !
      EPOCH-BUMP
      at DEPTH !
   then ;

\ ---- which slot a scope took -------------------------------------------------
\ A cleanup quotation cannot read its word's locals, so a scope records its slot
\ here. `finally` nests scopes strictly; the registry also holds the session's
\ row, which no scope leaves. The bound checks are rechecks of DEPTH-ROOM.
: ENTERED-PUSH ( n -- )
   ENTERED-N @ dup DEPTH-MAX >= if E-IR-CTX-DEPTH throw then
   {: at:n k:n :}
   at k cells ENTERED + !
   k 1+ ENTERED-N ! ;

: ENTERED-POP ( -- n )
   ENTERED-N @ 1- {: k:n :}
   k 0 < if E-IR-CTX-STATE throw then
   k ENTERED-N !
   k cells ENTERED + @ ;

: CE-CLEANUP ( -- )
   ENTERED-POP CTX-RETIRE ;

\ The one place a registry slot is taken; CTX-RETIRE is the one place it is given
\ up. The mark is read before the header is allotted, so the release returns the
\ header too.
: CTX-TAKE ( CBIND:binding n -- IR-CTX:ctx n )
   {: binding:CBIND:binding ceil:n :}
   DEPTH-ROOM
   DEPTH @ TAKE-GEN {: at:n g:n :}
   REGION-OPEN
   REGION-HERE @ {: mark:n :}
   binding ceil HEADER-BYTES REGION-ALLOT REGION-AT
   at mark CTX-INSTALL
   g at PACK-HANDLE at HANDLE!
   at 1+ DEPTH !
   g at PACK-HANDLE MINT-CTX at ;

\ Retires the slot on the ordinary path and on a throw alike.
: CTX-ENTER ( R CBIND:binding [ R IR-CTX:ctx -- S ] n -- S )
   {: body ceil:n :}
   ceil CTX-TAKE ENTERED-PUSH body [: CE-CLEANUP ;] finally ;

public

\ ---- creation ----------------------------------------------------------------
\ The module-serial ceiling is a parameter so exhaustion is testable; WITH-CONTEXT
\ passes the full range. The binding is revalidated.
: WITH-CONTEXT-BOUND ( R CBIND:binding n [ R IR-CTX:ctx -- S ] -- S )
   {: ceil:n body :}
   ceil CEIL-OK
   CBIND:VALIDATE body ceil CTX-ENTER ;

: WITH-CONTEXT ( R CBIND:binding [ R IR-CTX:ctx -- S ] -- S )
   SERIAL-CEILING swap WITH-CONTEXT-BOUND ;

\ ---- identity ----------------------------------------------------------------
\ Answers on a stale handle too, so a caught failure can name its context.
: SERIAL ( IR-CTX:ctx -- n )
   CTX>N ;

: LIVE? ( IR-CTX:ctx -- bool )
   CTX>N FIND-SLOT 0 < 0= ;

\ For children that can persist their owner only as a raw serial (the IR arena).
\ Zero is tested first: it masks to slot 0 and a retired row holds zero.
: SERIAL-LIVE? ( n -- bool )
   dup 0= if drop 0 0 <> exit then
   dup SLOT-MASK and cells HANDLES + @ = ;

\ ---- bound target and policy -------------------------------------------------
: BINDING@ ( IR-CTX:ctx -- CBIND:binding )
   RESOLVE HEADER-PTR HEADER-BINDING @ ;

\ ---- module allocation -------------------------------------------------------
private

\ The count is written before the IR-ID take, so a serial-exhaustion throw cannot
\ leave an unaccounted module.
: MINT-TAKE ( ptr u8 -- )
   dup HEADER-PTR HEADER-MINTED @ CNT-OK
   over HEADER-PTR HEADER-CEIL @ CNT-OK {: m:n c:n :}
   m c >= if E-IR-CTX-SERIALS throw then
   m 1+ swap HEADER-PTR HEADER-MINTED ! ;

public

\ Serials are IR-ID's: nonzero, monotonic, never reused across contexts.
: NEW-MODULE ( IR-CTX:ctx -- IR-ID:ir-module-key IR-ID:ir-module-id )
   RESOLVE MINT-TAKE IR-ID:NEW-MODULE ;

\ ---- scratch -----------------------------------------------------------------
\ Spans are taken as offsets, since a captured image may carry no pointer.
public

\ A span keeps its address and bytes until its context leaves, so an offset may
\ pass between passes without a stamp of its own.
: SCRATCH-OFFSET ( IR-CTX:ctx n -- n ) {: c:IR-CTX:ctx need:n :}
   need 1 < if E-IR-CTX-SIZE throw then
   need REGION-BYTES > if E-IR-CTX-SCRATCH throw then
   c RESOLVE-TOP {: base:ptr :}
   base HEADER-PTR HEADER-USED @ CNT-OK {: used:n :}
   need REGION-ALLOT {: off:n :}
   used need ALIGN8 + base HEADER-PTR HEADER-USED !
   off ;

: SCRATCH-TAKE ( IR-CTX:ctx n -- ptr u8 n ) {: c:IR-CTX:ctx need:n :}
   c need SCRATCH-OFFSET REGION-AT need ;

: SCRATCH-LIMIT ( -- n ) REGION-BYTES ;

\ How wide a region offset is, for a child that packs one beside other fields.
: SCRATCH-OFFSET-BITS ( -- n ) OFFSET-BITS ;

\ Stamped into a record stored at a region offset; see REGION-EPOCH.
: EPOCH ( -- n ) REGION-EPOCH @ ;

\ A FILE range can carry retired context handles and region offsets in DATA.
\ Before importing it, preserve the highest identities the cold compilation
\ reached. Earlier or unrelated work may already have advanced either count.
: HISTORY ( -- n n ) GEN-CELL atomic@ REGION-EPOCH @ ;

private

: GEN-ENSURE ( n -- ) {: want:n :}
   begin
      GEN-CELL atomic@ {: old:n :}
      old want >= if exit then
      old want GEN-CELL atomic-cas old =
   until ;

: EPOCH-ENSURE ( n -- )
   REGION-EPOCH @ max REGION-EPOCH ! ;

public

: CHECK-HISTORY ( n n -- )
   {: gen:n epoch:n :}
   gen 0 < gen GEN-MAX > or epoch 0 < or epoch EPOCH-MAX > or
      if E-IR-CTX-SERIALS throw then ;

: REPLAY-HISTORY ( n n -- )
   {: gen:n epoch:n :}
   gen epoch CHECK-HISTORY
   gen GEN-ENSURE epoch EPOCH-ENSURE ;

public

\ No compilation context survives into a captured runtime. Registry payloads
\ share the transient workspace; only the generation and region epochs persist.
: CAPTURE-PREPARE ( -- )
   DEPTH @ 0<> if E-IR-CTX-STATE throw then
   ENTERED-N @ 0<> if E-IR-CTX-STATE throw then
   REGION-CLOSE ;

\ ---- the compilation session -------------------------------------------------
\ A session is an unscoped context for what one SOURCE LOAD builds once and every
\ definition reads (dialect identities, the source-word model, their interner);
\ before it this was rebuilt for all 3,079 definitions. It is installed through
\ CTX-TAKE at the bottom of the region and closed through CTX-RETIRE, so no
\ definition's release reaches it. Each definition is an ordinary nested
\ WITH-CONTEXT, because builder registry rows are freed only at a context
\ teardown. It lives until capture or process exit.
\ A second session, a session opened inside any context, and a close while a
\ context is open inside it are E-IR-CTX-STATE.
private

1 TYPED-BUFFER SESSION-CTX IR-CTX:ctx
variable SESSION-SLOT

: SESSION-FORGET ( -- )
   -1 SESSION-SLOT ! ;
SESSION-FORGET

: SESSION-CK ( -- )
   SESSION-SLOT @ 0 < if E-IR-CTX-STATE throw then ;

\ The slot comes from CTX-TAKE's answer, not a second read of DEPTH.
: SESSION-INSTALL ( CBIND:binding -- )
   SERIAL-CEILING CTX-TAKE SESSION-SLOT !
   0 SESSION-CTX ! ;

: SESSION-RETIRE ( -- )
   SESSION-SLOT @ CTX-RETIRE
   SESSION-FORGET ;

\ One opened inside a scope would die with that scope while answering as live.
: REGISTRY-EMPTY-CK ( -- )
   DEPTH @ 0<> if E-IR-CTX-STATE throw then ;

\ A context open inside the session reads storage that dies with this row.
: SESSION-DEEPEST-CK ( -- )
   DEPTH @ 1- SESSION-SLOT @ <> if E-IR-CTX-STATE throw then ;

\ ---- what the session's owner stands down ------------------------------------
\ The owner's flags (a dialect prototype, a registered vocabulary) describe
\ storage that dies with this row, so SESSION-CLOSE runs the stand-down before
\ retiring the row; otherwise a reader gets E-IR-ARENA-STALE from an ordinary
\ question. Admission refuses while another root or task owns the compiler.
\ Installed once, for RETIRE-CHILDREN!'s reason.
defer SESSION-ADMISSION ( -- )
defer SESSION-STAND-DOWN ( -- )

variable STAND-SET
0 STAND-SET !

: KEEP-STANDING ( -- ) ;

: STAND-RESET ( -- )
   [: KEEP-STANDING ;] is SESSION-ADMISSION
   [: KEEP-STANDING ;] is SESSION-STAND-DOWN ;
STAND-RESET

\ Once admitted, the row goes back whatever the stand-down did; the first error
\ is the one reported.
: SESSION-END ( -- )
   [: SESSION-STAND-DOWN ;] catch {: rc:n :}
   SESSION-RETIRE
   rc 0<> if rc throw then ;

public

: SESSION-STAND-DOWN! ( [ -- ] [ -- ] -- )
   STAND-SET @ 0<> if E-IR-CTX-STATE throw then
   1 STAND-SET !
   is SESSION-STAND-DOWN
   is SESSION-ADMISSION ;

: SESSION-LIVE? ( -- bool )
   SESSION-SLOT @ 0 < 0= ;

\ A FILE import cannot alias saved handles against an unrelated live context.
\ A compiler session is the only permitted live row while importing its child.
: REGISTRY-IDLE? ( -- bool ) DEPTH @ 0= ;
: REGISTRY-SESSION-ONLY? ( -- bool )
   SESSION-LIVE? DEPTH @ 1 = and ;

\ The one open session occupies slot zero. FILE metadata stores this identity
\ so replay can distinguish a continuing session from a close and a new open.
: SESSION-SERIAL ( -- n )
   SESSION-LIVE? if 0 SESSION-CTX @ SERIAL else 0 then ;
: SESSION-GEN-OF ( n -- n )
   dup 0= if exit then
   dup 0 < over SLOT-MASK and 0<> or if E-IR-CTX-SERIALS throw then
   SLOT-BITS rshift dup 0 <= over GEN-MAX > or if E-IR-CTX-SERIALS throw then ;

: SESSION-CLOSE ( -- )
   SESSION-CK
   SESSION-DEEPEST-CK
   SESSION-ADMISSION
   SESSION-END ;

private

variable HOOKED
0 HOOKED !

\ Runs before IR-CTX clears its registry at capture, so an idle session closes
\ first and an open definition context is refused by the session's name. The
\ flag clears only on success: a throwing callback stays registered for retry.
: SESSION-AT-CAPTURE ( -- )
   SESSION-LIVE? if SESSION-CLOSE then
   0 HOOKED ! ;

\ Registered when a session is taken, not at load: PREPARE removes every
\ callback it runs and an image does not carry the registry, so a load-time
\ registration would never recur in an engine built from an image.
: HOOK-CAPTURE ( -- )
   HOOKED @ 0<> if exit then
   1 HOOKED !
   [: SESSION-AT-CAPTURE ;] IMAGE-LIFECYCLE:REGISTER ;

public

\ The binding is revalidated as in WITH-CONTEXT-BOUND, with the production
\ ceiling. A first tier-1 definition compiled inside a caller's own context is
\ refused here by REGISTRY-EMPTY-CK.
: SESSION-OPEN ( CBIND:binding -- IR-CTX:ctx )
   SESSION-LIVE? if E-IR-CTX-STATE throw then
   REGISTRY-EMPTY-CK
   SERIAL-CEILING CEIL-OK
   DEPTH-ROOM
   HOOK-CAPTURE
   CBIND:VALIDATE
   SESSION-INSTALL
   0 SESSION-CTX @ ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;package
