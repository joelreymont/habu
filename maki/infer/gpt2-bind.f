\ gpt2-bind.f - the GPT-2 bind transaction's PREPARE phase and the prepared-bind
\ capability (package GPT2TX; inference design rev-4 correction 3 leaf S6b1,
\ blackboard 20260724-191041.846, with the rev-5 amendments and the 2026-07-26
\ redesign recorded in .dots/habu-bind-txn-prepare-eaa50b5b.md).
\
\ CONCERN: decide, once and completely, whether a published tensor census can be
\ bound as the GPT-2 model a validated configuration describes - and if it can,
\ hand back a single linear value holding everything the commit needs. Binding is
\ a transaction in two halves. This file is the first half: it either REFUSES and
\ gives the census back untouched, or it produces a `prep`. The second half (the
\ two commit leaves) turns a prep into a model and cannot refuse.
\
\ WHY ALL THE VALIDATION IS HERE. A commit allocates an arena or takes a file
\ mapping away from its census; both are steps that must not discover a problem
\ half-finished. So every question that can have a wrong answer is asked here,
\ before any resource moves: the census holds exactly the tensors this geometry
\ implies, each one is F32 with the exact rank and dims the role declares, each
\ has a mapping offset, each role's slot is the slot this enumeration expects,
\ and all the arithmetic the commits will do - each row's end, and the running
\ sum of every extent that the packed arena will need - already proved it fits a
\ cell. What is left for a commit is memory, and memory only.
\
\ THE TWO PASSES, AND WHY THE ORDER IS THE CONTRACT. Pass one (PLAN) validates
\ every row and writes the validated (mapping-offset, extent) pairs into this
\ package's private row scratch. Pass two (FILL-TABLE) creates the WSTORE table
\ and populates it from those raw cells. Nothing in pass two can refuse: the slot
\ count was range-checked, each row was written exactly once so no slot can be
\ double-set or left unset, and each pair already passed its role narrowing and
\ its row-end check. That is why the table is created only after the last row is
\ validated - reversing the two would put a fallible step after a live table with
\ no owner to dispose it.
\
\ WHAT IS STILL FALLIBLE AFTER VALIDATION, AND HOW IT IS HELD. Two steps remain and
\ both can fail on memory alone: building the table, and allocating the prep block.
\ Neither is allowed to throw out of PREPARE. A throw at either point would unwind
\ past the linear census - and, at the second, past the sealed table as well -
\ stranding owners the checker cannot see and leaving a caller with no way to
\ recover them. So each runs under its own `catch`, and a failure becomes an
\ ordinary refusal: the first returns rejected(census, code); the second gives the
\ table back through WSTORE:TABLE-DISPOSE and then returns rejected(census, code).
\ The block allocation is deliberately hoisted OUT of the mint for this reason -
\ with the block already in hand the mint is total, so the stretch where the census
\ and table are raw cells contains nothing that can fail.
\
\ WHY THE ROW SCRATCH IS STATIC. It is sized by SAFET:MAX-TENSORS, the largest
\ census the loader will ever publish, so it needs no allocation and has no
\ failure path of its own. A census bigger than that cannot exist, and a census
\ whose count does not equal 4 + 13*nlayer is refused before any row is written,
\ so the scratch is always big enough for the census that reaches pass one.
\
\ REJECTION RETURNS THE CENSUS. Every refusal answers rejected(census, code) with
\ the census exactly as it arrived - same tensors, same mapping, still usable, so
\ a caller can try a different configuration on it or release it. That is why
\ validation runs inside a `catch` over a stack-preserving quotation (the
\ SAFET:LOAD discipline): a throw would unwind past the linear census token and
\ strand it, and the checker cannot see that happen. The codes a caller sees are
\ either this package's own E-GX-* or whichever GPT2BIND code fired underneath -
\ E-GB-LAYER and E-GB-EXTENT are both reachable that way.
\
\ WHAT PREPARE CANNOT REFUSE ON, AND WHY THAT IS NOT A HOLE. GPT2BIND's identity
\ assertion (E-GB-FOREIGN) cannot fire from this entry point: PREPARE mints every
\ layer identity from the very configuration it is validating against, since
\ GPT2BIND:LAYER is the sole layerid constructor, so TID-SLOT is always comparing a
\ configuration with itself. Two configurations that differ only in a field no
\ tensor reflects - tied embeddings, say - therefore both bind the same census, and
\ they SHOULD: nothing in a tensor census distinguishes them. What separates them is
\ the cfgkey this file captures into the prep, which is what lets a commit refuse a
\ model built against the other configuration. The identity is captured here and
\ enforced downstream, not refused here.
\
\ THE PREP IS OPAQUE AND LINEAR. `prep` is a DEFLINEAR token that IS its private
\ block, the WSTORE tbuilder/table shape. The block holds the moved census token,
\ the sealed table token, the four cells of the minting configuration's cfgkey,
\ nlayer, and the plan the transaction was validated against. It has NO public
\ accessor: nothing hands out the census, the table, a slot row, or a pointer, so
\ through the PUBLIC surface the only things a holder can do with a prep are commit
\ it (the later leaves) or ABORT it. ABORT is total - table back through
\ WSTORE:TABLE-DISPOSE, census back through SAFET:RELEASE, then the block - and it
\ is the same disposal the commit leaves reuse when they run out of memory.
\
\ HOW STRONG THAT OPACITY ACTUALLY IS. It is a public-surface guarantee, not a
\ sealed one. WSTORE closes its package with the prot-wid seal, so a foreign file
\ that reopens `package WSTORE` is refused; GPT2TX cannot do the same, because this
\ module's own acceptance suite reopens `package GPT2TX` to reach the row
\ projections the leaf contract asks it to check, and the seal would refuse the
\ suite too. The consequence is stated plainly: any file that reopens this package
\ can call the erasures and take the census or the table straight out of a prep
\ block. Nothing here prevents that; what stands against it is that the erasures
\ are package-private (proved by the unresolvable-outside probes in the suite), that
\ refine-lint confines the two inverse mints to this file, and that GPT2TX:LIVE
\ makes an abandoned prep observable. A sealed package that a test can still reach
\ needs the sealed-destructure capability; until then this is the honest boundary.
\
\ THE AUDITED ERASURE, AND WHAT IT COSTS. A DEFLINEAR is one cell and carries no
\ fields, and an ENUM payload field cannot name a STRUCTURE that transitively
\ holds a linear field (measured; tracked by habu-checker-enum-payload-9e1ae6cc),
\ so a bundle of linear children cannot be expressed as a typed record today. The
\ census and table therefore cross into the prep block through package-private
\ trusted erasures and come back out the same way. Inside the block the checker
\ can no longer see them, which is exactly the guarantee this file gives up: from
\ MINT until ABORT, "the census is owned once" is enforced by this file's
\ structure - one mint site, one exit site, no accessor - and not by the type
\ system. The linear-scope combinator (habu-checker-linear-scope-6218899c) and
\ pointer-lifetime/region types (habu-checker-ptr-lifetime-f59d1e9d) are the
\ capabilities that would let a linear bundle be a checked value; when they land,
\ these erasures and their TRUSTED.md rows go away.
\
\ SLOT ENUMERATION. Slots are the dense GPT2BIND numbering: 0..3 are the
\ checkpoint globals in grole declaration order, then slot 4 + 13*layer + role
\ ordinal. This file walks 0..count-1, rebuilds the tid each slot, and asserts
\ GPT2BIND:TID-SLOT answers that same slot - a round trip that pins the slot
\ formula in both directions, so an off-by-one in either place is a refusal rather
\ than a silently transposed weight. The result is also bound-checked against the
\ census count before it reaches the scratch, even though TID-SLOT enforces its
\ own range: the value indexes memory here, so this file checks it here.
\
\ DTYPE IS F32 FOR EVERY TENSOR, NO EXCEPTIONS. The pinned checkpoint
\ (SHA-256 248dfc3911869ec493c76e65bf2fcf7f615828b0254c12b473182f0f81d3a707)
\ stores every tensor as F32 including every h.N.attn.bias causal-mask buffer, so
\ the mask gets no exemption; a half-precision or bf16 export of the same weights
\ is a different artifact and is refused, not silently accepted.
\
\ maki -> habu only. Owns -5660..-5674.

require lib/prelude.f
require lib/adt/option.f
require lib/adt/result.f
require lib/cad-num-arithmetic.f
require lib/memory.f
require maki/infer/safetensors.f
require maki/infer/weight-store.f
require maki/infer/model-config.f
require maki/infer/gpt2-roles.f

package GPT2TX

public

\ ---- named rejection codes (this module owns -5660..-5674) --------------------
-5660 constant E-GX-COUNT    \ census tensor count is not 4 + 13*nlayer
-5661 constant E-GX-KEY      \ a role's exact HF key is not in the census
-5662 constant E-GX-DTYPE    \ a census tensor is not F32
-5663 constant E-GX-RANK     \ a census tensor's rank is not the role's rank
-5664 constant E-GX-SHAPE    \ a census tensor's dim is not the role's dim
-5665 constant E-GX-OFFSET   \ the census reports no mapping offset for a tensor
-5666 constant E-GX-SLOT     \ a role's slot is out of range, or not the slot walked
-5667 constant E-GX-EXTENT   \ an extent, a row end, or the packed prefix sum overflows
-5668 constant E-GX-RENDER   \ a role's HF key did not fit the private render buffer
-5669 constant E-GX-ALIAS    \ two roles resolved to one census tensor

\ ---- the prepared bind: opaque, linear, no accessors -------------------------
DEFLINEAR GPT2TX:prep

\ ---- what PREPARE answers ----------------------------------------------------
\ prepared carries the whole transaction; rejected gives the census back beside
\ the code that refused it.
ENUM prep-result 0
   VARIANT prepared FIELD p GPT2TX:prep ;VARIANT
   VARIANT rejected FIELD c SAFET:census FIELD code n ;VARIANT
;ENUM

private

\ ---- audited representation boundary -----------------------------------------
\ The prep token IS its block, so the mint/read/consume leaves are identities or
\ a one-cell duplication; the checker cannot express "this pointer is a live
\ GPT2TX block". All of these are package-private and have no public inverse.
TRUSTED: MINT-PREP ( ptr u8 -- GPT2TX:prep ) ;

TRUSTED: PREP>BLOCK ( GPT2TX:prep -- GPT2TX:prep ptr n )
   dup ;

TRUSTED: TAKE-PREP ( GPT2TX:prep -- ptr n ) ;

TRUSTED: BLK>BYTES ( ptr n -- ptr u8 ) ;

\ The two linear children, parked in the block as raw cells. See the header on
\ what this gives up and which checker capabilities retire it.
TRUSTED: CENSUS>N ( SAFET:census -- n ) ;
TRUSTED: N>CENSUS ( n -- SAFET:census ) ;
TRUSTED: TABLE>N ( WSTORE:table -- n ) ;
TRUSTED: N>TABLE ( n -- WSTORE:table ) ;

\ ---- geometry and layout constants -------------------------------------------
4 constant NGLOBAL             \ |grole|: the checkpoint-global tensors
13 constant NBLOCK             \ |brole|: tensors per transformer block
$7FFFFFFFFFFFFFFF constant MAX-N
64 constant KEY-CAP            \ the GPT2BIND KEY-CAP bound: longest HF key is 39 bytes

\ prep block: the two moved owners, nlayer, the four cfgkey cells, then the plan
\ the transaction was validated against. The plan travels IN the prep, not in this
\ package's scratch: a later PREPARE - including one that refuses - overwrites the
\ scratch while an earlier prep is still alive, so a commit leaf reading process
\ statics would read another transaction's numbers.
0 constant P-CEN
1 constant P-TBL
2 constant P-NL
3 constant P-K0
7 constant P-CNT               \ rows validated, and the sealed table's slot count
8 constant P-SUM               \ prefix sum of every extent (the packed arena size)
9 constant P-CELLS

\ row scratch: (mapping offset, extent) per slot, sized by the largest census the
\ loader can publish, so pass one never allocates (see the header)
0 constant R-OFF
1 constant R-LEN
2 constant R-CELLS

create KEY-BUF KEY-CAP allot                    \ private key render landing pad
create ROWS SAFET:MAX-TENSORS R-CELLS * cells allot
create SEEN SAFET:MAX-TENSORS cells allot       \ census ids already claimed by a role

\ These three are SCRATCH for one PREPARE call and nothing else reads them
\ afterwards: the next call overwrites them, whether it succeeds or refuses. What
\ outlives the call is the copy MINT writes into the prep block (P-CNT / P-SUM).
variable SUM-N                                  \ running sum of extents (packed-arena pre-clear)
variable PLAN-N                                 \ the census count pass one validated

\ Parked between PREPARE's guarded steps, because a quotation under `catch` cannot
\ hand a linear value back out - the two paths would have different stack shapes.
variable PEND-TBL                               \ the sealed table, as a raw cell
PTR-VARIABLE PEND-BLK                           \ the prep block the mint will adopt

variable LIVE-N                                 \ undisposed prep blocks (accounting only)

: ROW-CAP ( -- n )  SAFET:MAX-TENSORS ;

\ ---- row scratch accessors ----------------------------------------------------
: ROW! ( n n n -- ) {: row:n off:n len:n :}
   off  ROWS row R-CELLS * R-OFF + cells +  !
   len  ROWS row R-CELLS * R-LEN + cells +  ! ;

: ROW@ ( n n -- n ) {: row:n col:n :}
   ROWS row R-CELLS * col + cells + @ ;

\ ---- census-id claims: the role->tensor map must be one-to-one -----------------
\ Matching counts and matching shapes are not enough. If two roles ever render to
\ the same census key - a vocabulary collision, or a checkpoint that names two
\ tensors alike - every per-row check still passes: both roles find the same
\ tensor, its dtype and shape satisfy both, and the counts still agree. The table
\ would then point two slots at one tensor while some other tensor is never
\ claimed, and the model would silently carry a duplicated weight. Counting is no
\ defense either, because the count is right. Only claiming is: each census id may
\ be taken exactly once, and with count == census count a walk that claims every id
\ without collision has claimed all of them.
: CLAIM-CLEAR ( n -- ) {: count:n :}
   count 0 ?do  0 SEEN i cells + !  loop ;

: CLAIM ( n n -- ) {: id:n count:n :}
   id 0 <  id count >=  or if E-GX-SLOT throw then
   SEEN id cells + @ 0 <> if E-GX-ALIAS throw then
   1 SEEN id cells + ! ;

\ ---- option and result plumbing ----------------------------------------------
\ A missing census answer is a refusal with this file's own code, never a -1
\ sentinel: the none arm throws, so only the some arm produces a row.
: NEED ( option<n> n -- n ) {: code:n :}
   MATCH option
      none OF code throw ENDOF
      some OF ENDOF
   ;MATCH ;

: RES-CODE ( result<n,n> -- n )                 \ 0 for ok, the named code for err
   MATCH result
      ok  OF drop 0 ENDOF
      err OF ENDOF
   ;MATCH ;

\ ---- validated byte roles -----------------------------------------------------
\ WSTORE:SLOT! consumes narrowed offset/extent roles. Every refusal of the
\ narrowing is an extent fault here, which is why pass two cannot meet one.
: FIX-OFF ( CAD-NUM:numeric-result<CAD-NUM:byte-off> -- CAD-NUM:byte-off )
   MATCH CAD-NUM:numeric-result
      ok OF ENDOF                              negative OF E-GX-EXTENT throw ENDOF
      zero OF E-GX-EXTENT throw ENDOF           overflow OF E-GX-EXTENT throw ENDOF
      underflow OF E-GX-EXTENT throw ENDOF      bad-alignment OF E-GX-EXTENT throw ENDOF
      misaligned OF E-GX-EXTENT throw ENDOF
   ;MATCH ;

: FIX-LEN ( CAD-NUM:numeric-result<CAD-NUM:byte-len> -- CAD-NUM:byte-len )
   MATCH CAD-NUM:numeric-result
      ok OF ENDOF                              negative OF E-GX-EXTENT throw ENDOF
      zero OF E-GX-EXTENT throw ENDOF           overflow OF E-GX-EXTENT throw ENDOF
      underflow OF E-GX-EXTENT throw ENDOF      bad-alignment OF E-GX-EXTENT throw ENDOF
      misaligned OF E-GX-EXTENT throw ENDOF
   ;MATCH ;

: >OFF ( n -- CAD-NUM:byte-off )   CAD-NUM:BYTE-OFF FIX-OFF ;
: >LEN ( n -- CAD-NUM:byte-len )   CAD-NUM:BYTE-LEN FIX-LEN ;

\ ---- slot ordinal -> role (declaration order; the tables pin the role counts) --
: G-ROLE ( n -- GPT2BIND:grole )
   case
      0 of GPT2BIND-GROLE:WTE   endof
      1 of GPT2BIND-GROLE:WPE   endof
      2 of GPT2BIND-GROLE:LNF-G endof
      3 of GPT2BIND-GROLE:LNF-B endof
      E-GX-SLOT throw
   endcase ;

: B-ROLE ( n -- GPT2BIND:brole )
   case
      0  of GPT2BIND-BROLE:LN1-G   endof
      1  of GPT2BIND-BROLE:LN1-B   endof
      2  of GPT2BIND-BROLE:MASK    endof
      3  of GPT2BIND-BROLE:QKV-W   endof
      4  of GPT2BIND-BROLE:QKV-B   endof
      5  of GPT2BIND-BROLE:APROJ-W endof
      6  of GPT2BIND-BROLE:APROJ-B endof
      7  of GPT2BIND-BROLE:LN2-G   endof
      8  of GPT2BIND-BROLE:LN2-B   endof
      9  of GPT2BIND-BROLE:FC-W    endof
      10 of GPT2BIND-BROLE:FC-B    endof
      11 of GPT2BIND-BROLE:MPROJ-W endof
      12 of GPT2BIND-BROLE:MPROJ-B endof
      E-GX-SLOT throw
   endcase ;

\ The tid this file expects at a slot. LAYER is the sole layerid constructor, so
\ the identity a block tid carries is this mcfg's own by construction - which is
\ what makes the TID-SLOT round trip below a real assertion about the FILE rather
\ than about the identity.
: SLOT>TID ( MDLCFG:mcfg n -- MDLCFG:mcfg GPT2BIND:tid ) {: slot:n :}
   slot NGLOBAL < if
      slot G-ROLE GPT2BIND-TID:GLOBAL exit
   then
   slot NGLOBAL - {: rel:n :}
   rel NBLOCK / GPT2BIND:LAYER
   rel NBLOCK mod B-ROLE GPT2BIND-TID:BLOCK ;

\ ---- per-row validation -------------------------------------------------------
\ The slot the role claims must be in range for the census AND be the slot this walk
\ is on.
\
\ The range test is structurally undominatable and kept anyway. PREPARE mints every
\ layerid through GPT2BIND:LAYER, which validates the index, and TID-SLOT revalidates
\ the embedded one before it multiplies, so the slot it returns is already inside
\ [0, census) unconditionally - no fixture reaching this word through PREPARE can
\ make this test fire, and deleting it leaves the whole suite green. It stays because
\ the rev-5 contract puts the bound at the CONSUMER: this value is about to index the
\ row scratch, and a check at the point of indexing does not depend on another
\ package continuing to guarantee a range it is free to change.
\
\ The equality test is the half that carries information a fixture can reach - it
\ round-trips the slot formula, so an off-by-one on either side is a refusal instead
\ of a weight silently filed under the wrong role.
: V-SLOT ( MDLCFG:mcfg n n -- MDLCFG:mcfg ) {: slot:n count:n :}
   slot SLOT>TID GPT2BIND:TID-SLOT {: got:n :}
   got 0 <  got count >=  or if E-GX-SLOT throw then
   got slot <> if E-GX-SLOT throw then ;

\ The census id for a role's exact HF key, rendered through the public copy-out.
: FIND-KEY ( SAFET:census GPT2BIND:tid -- SAFET:census n )
   KEY-BUF KEY-CAP GPT2BIND:COPY-KEY? E-GX-RENDER NEED {: klen:n :}
   KEY-BUF klen SAFET:FIND E-GX-KEY NEED ;

: V-DTYPE ( SAFET:census n -- SAFET:census ) {: id:n :}
   id SAFET:DTYPE? E-GX-DTYPE NEED
   SAFET:DT-F32 <> if E-GX-DTYPE throw then ;

: V-DIM ( SAFET:census n n n -- SAFET:census ) {: id:n axis:n want:n :}
   id axis SAFET:DIM? E-GX-SHAPE NEED
   want <> if E-GX-SHAPE throw then ;

\ Rank first, then every dim the rank declares. The trailing 1s TID-SHAPE pads
\ with are not census axes, so they are not looked up; the rank equality is what
\ makes that safe.
: V-SHAPE ( SAFET:census n n n n n n -- SAFET:census )
   {: id:n rank:n d0:n d1:n d2:n d3:n :}
   id SAFET:RANK? E-GX-RANK NEED
   rank <> if E-GX-RANK throw then
   id 0 d0 V-DIM
   rank 1 > if id 1 d1 V-DIM then
   rank 2 > if id 2 d2 V-DIM then
   rank 3 > if id 3 d3 V-DIM then ;

: ROW-OF ( SAFET:census n -- SAFET:census n n ) {: id:n :}
   id SAFET:MAP-OFFSET? E-GX-OFFSET NEED {: off:n :}
   id SAFET:NBYTES? E-GX-EXTENT NEED {: len:n :}
   off len ;

\ Two independent overflow facts, both required before a commit can be arithmetic
\ free: this row's own end (off + len), and the running sum of every extent, which
\ is the size of the packed arena COMMIT-ALLOCATED will ask for.
\ The first two tests are structurally undominatable from a fixture: SAFET:NBYTES?
\ derives its answer from a validated shape whose dims are all positive, and
\ MAP-OFFSET? is 8 + header length + a nonnegative begin, so no safetensors file
\ this loader will publish can present a zero extent or a negative offset. They are
\ kept because they are the preconditions the two overflow tests below are stated
\ against - a negative offset would make the row-end comparison meaningless - and a
\ future census producer is not obliged to preserve either property. The two
\ overflow tests ARE fixture-reachable and are pinned at their exact boundaries by
\ the V-ARITH leg in the acceptance suite.
: V-ARITH ( n n -- ) {: off:n len:n :}
   len 0 <= if E-GX-EXTENT throw then
   off 0 < if E-GX-EXTENT throw then
   MAX-N len - off < if E-GX-EXTENT throw then
   SUM-N @ {: sum:n :}
   MAX-N len - sum < if E-GX-EXTENT throw then
   sum len + SUM-N ! ;

\ A configuration value cannot be a typed local (only arity-0 nominal families
\ can), so the mcfg rides the return stack for the stretch where the CENSUS has to
\ be on top - the MDLCFG whole-bundle-transport idiom. One park, one unpark, and
\ every scalar in between is a named local.
: V-ROW ( SAFET:census MDLCFG:mcfg n n -- SAFET:census MDLCFG:mcfg )
   {: slot:n count:n :}
   slot count V-SLOT
   slot SLOT>TID GPT2BIND:TID-SHAPE
   {: rank:n d0:n d1:n d2:n d3:n :}
   slot SLOT>TID
   swap >r                                      \ ( census tid ), the mcfg parked
   FIND-KEY {: id:n :}
   id count CLAIM
   id V-DTYPE
   id rank d0 d1 d2 d3 V-SHAPE
   id ROW-OF {: off:n len:n :}
   off len V-ARITH
   slot off len ROW!
   r> ;

\ ---- pass one: validate everything, write no table ----------------------------
\ Stack-preserving so `catch` accepts it and a refusal leaves the census exactly
\ where it was (see the header). Nothing here creates or consumes an owner.
: PLAN ( SAFET:census MDLCFG:mcfg -- SAFET:census MDLCFG:mcfg )
   0 SUM-N !
   GPT2BIND:CENSUS-COUNT {: count:n :}
   count 0 <=  count ROW-CAP >  or if E-GX-COUNT throw then
   count PLAN-N !
   count CLAIM-CLEAR
   >r SAFET:COUNT {: have:n :} r>                \ the mcfg parks while the census answers
   have count <> if E-GX-COUNT throw then
   count 0 ?do  i count V-ROW  loop ;

\ ---- pass two: build the sealed table from validated rows ---------------------
\ Infallible by construction except for memory: see the header's two-pass note.
: FILL-TABLE ( n -- WSTORE:table ) {: count:n :}
   count WSTORE:TABLE-NEW
   count 0 ?do
      i  i R-OFF ROW@ >OFF  i R-LEN ROW@ >LEN  WSTORE:SLOT!
   loop
   WSTORE:SEAL ;

\ ---- the prep block ------------------------------------------------------------
: PREP-ALLOC ( -- CAD-NUM:alloc-byte-len )
   P-CELLS cells MEM:BYTES-ALLOC-LEN ;

\ The one site where the census and table stop being checker-tracked values, and it
\ is TOTAL: the caller hands in a block that is already allocated, so nothing
\ between the first erasure and the last cell write can throw. That is deliberate -
\ the erasures are the window where a throw would strand an owner the checker can
\ no longer see, so the only fallible step (the allocation) is kept outside it,
\ where PREPARE guards it and can still dispose both owners by hand.
: MINT ( SAFET:census WSTORE:table ptr u8 n n n n n -- GPT2TX:prep )
   {: k0:n k1:n k2:n k3:n nl:n :}
   MINT-PREP
   PREP>BLOCK {: blk:ptr :}
   swap TABLE>N blk P-TBL cells + !
   swap CENSUS>N blk P-CEN cells + !
   nl  blk P-NL cells + !
   k0  blk P-K0 0 + cells + !
   k1  blk P-K0 1 + cells + !
   k2  blk P-K0 2 + cells + !
   k3  blk P-K0 3 + cells + !
   PLAN-N @ blk P-CNT cells + !
   SUM-N @ blk P-SUM cells + !
   1 LIVE-N +! ;

\ ---- package-private projections (for the commit leaves and the test seam) -----
\ None of these is public: a prep never hands out its census, its table, a row, or
\ a pointer. What they do expose is what a commit needs to mint a model - the depth
\ and the captured identity - plus what the acceptance fixtures need to see.
: PREP-NL ( GPT2TX:prep -- GPT2TX:prep n )
   PREP>BLOCK P-NL cells + @ ;

\ The plan a prep was built against. A commit leaf takes the arena size and the
\ slot count from HERE, never from this package's scratch: the scratch belongs to
\ the most recent PREPARE call, and that call may have refused while an earlier
\ prep is still alive and waiting to commit.
: PREP-COUNT ( GPT2TX:prep -- GPT2TX:prep n )
   PREP>BLOCK P-CNT cells + @ ;

: PREP-SUM ( GPT2TX:prep -- GPT2TX:prep n )     \ bytes the packed arena will need
   PREP>BLOCK P-SUM cells + @ ;

\ ---- scratch readers, for this package's own acceptance suite only -------------
\ These describe the LAST PREPARE call, successful or not, and nothing outside the
\ suite may read them; the prep-owned readers above are what a commit uses. They
\ exist so a fixture can check the validated rows without a public accessor.
: PLAN-COUNT ( -- n )
   PLAN-N @ ;

: PLAN-SUM ( -- n )
   SUM-N @ ;

: PLAN-ROW ( n -- n n )                         \ mapping offset + byte extent
   dup R-OFF ROW@ swap R-LEN ROW@ ;

\ How many census ids the last walk claimed. A successful walk of n slots that
\ claimed n distinct ids has claimed every id exactly once - CLAIM refuses a
\ repeat, so n claims over a set of n ids can only be a bijection. That is the
\ injectivity statement, and reading it here is what ties the claim set to the
\ walk rather than merely to its own unit leg.
: PLAN-CLAIMED ( n -- n ) {: count:n :}
   0
   count 0 ?do  SEEN i cells + @ 0 <> if 1 + then  loop ;

: PREP-KEY ( GPT2TX:prep -- GPT2TX:prep MDLCFG:cfgkey )
   PREP>BLOCK {: blk:ptr :}
   blk P-K0 0 + cells + @
   blk P-K0 1 + cells + @
   blk P-K0 2 + cells + @
   blk P-K0 3 + cells + @
   MDLCFG-CFGKEY:MAKE ;

\ ---- the two guarded steps after validation ------------------------------------
\ Both run under `catch` with an empty stack effect, so the census sitting below
\ them is untouched on either path and the two branches have the same shape - the
\ reason each parks its product in a package cell instead of returning it. Memory
\ is the only thing either can fail on, and a failure has to become a REFUSAL
\ rather than a throw: a throw here would unwind past the census (and, in the
\ second step, the sealed table) and strand owners the checker can no longer see.
: TABLE-STEP ( -- )
   PLAN-N @ FILL-TABLE TABLE>N PEND-TBL ! ;

: BLOCK-STEP ( -- )
   PREP-ALLOC MEM:ALLOC-BYTES drop PEND-BLK ! ;

\ Gives the sealed table back when the block allocation failed. A failure to
\ release takes precedence in the reported code - it is the more proximate fault
\ and swallowing it would hide a real leak - otherwise the original cause stands.
: TBL-BACK ( WSTORE:table n -- n ) {: cause:n :}
   WSTORE:TABLE-DISPOSE RES-CODE {: rc:n :}
   rc 0 <> if rc else cause then ;

public

\ ---- the transaction's first half ---------------------------------------------
\ Either the census can be bound as this configuration's GPT-2 and the whole
\ transaction comes back as one linear prep, or it cannot and the census comes
\ back untouched beside the code that refused it. Nothing in between: no partial
\ table, no moved mapping, no allocation left over on any refusal path.
: PREPARE ( SAFET:census MDLCFG:mcfg -- prep-result )
   [: PLAN ;] catch {: code:n :}
   code 0 <> if
      drop
      code GPT2TX-PREP--RESULT:REJECTED exit
   then
   MDLCFG:NLAYER@ {: nl:n :}
   MDLCFG:CFGKEY@ MDLCFG-CFGKEY:UNMAKE {: k0:n k1:n k2:n k3:n :}
   drop
   [: TABLE-STEP ;] catch {: tcode:n :}
   tcode 0 <> if tcode GPT2TX-PREP--RESULT:REJECTED exit then
   [: BLOCK-STEP ;] catch {: bcode:n :}
   bcode 0 <> if
      PEND-TBL @ N>TABLE bcode TBL-BACK
      GPT2TX-PREP--RESULT:REJECTED exit
   then
   PEND-TBL @ N>TABLE
   PEND-BLK @ k0 k1 k2 k3 nl MINT
   GPT2TX-PREP--RESULT:PREPARED ;

\ ---- total disposal ------------------------------------------------------------
\ The exit for a prep a caller decided not to commit, and the cleanup the commit
\ leaves reuse when they run out of memory. Everything is given back before
\ anything is reported: the table, then the census, then the block, and only then
\ does a failed release surface - so a bad release cannot strand the owners that
\ had not been reached yet.
: ABORT ( GPT2TX:prep -- )
   TAKE-PREP {: blk:ptr :}
   blk P-TBL cells + @ N>TABLE WSTORE:TABLE-DISPOSE RES-CODE {: tc:n :}
   blk P-CEN cells + @ N>CENSUS SAFET:RELEASE
   blk BLK>BYTES PREP-ALLOC MEM:RELEASE-BYTES
   -1 LIVE-N +!
   tc 0 <> if tc throw then ;

\ ---- leak accounting (decides nothing; the WSTORE:LIVE pattern) -----------------
\ Undisposed prep blocks. The census and the table inside a prep are raw cells the
\ checker cannot follow, so a prep abandoned between PREPARE and ABORT takes both
\ of them with it and no type error is raised. This counter is what makes that
\ visible, and the acceptance suite asserts it around every transaction.
: LIVE ( -- n )
   LIVE-N @ ;

;package
