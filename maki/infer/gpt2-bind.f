\ gpt2-bind.f - the whole GPT-2 bind transaction: the PREPARE phase and the
\ prepared-bind capability (leaf S6b1), then the compare-and-commit half that turns
\ a prep into a bound model (leaf S6b3). Package GPT2TX; inference design rev-4
\ correction 3, blackboard 20260724-191041.846, with the rev-5 amendments and the
\ 2026-07-26 redesigns recorded in .dots/habu-bind-txn-prepare-eaa50b5b.md and
\ .dots/habu-bind-txn-commit-ccf652d2.md.
\
\ CONCERN: decide, once and completely, whether a published tensor census can be
\ bound as the GPT-2 model a validated configuration describes - and if it can,
\ hand back a single linear value holding everything the commit needs, then turn
\ that value into a model that owns the checkpoint's residency.
\
\ THE TRANSACTION IN THREE STEPS, AND WHERE EACH CAN REFUSE. PREPARE either REFUSES
\ and gives the census back untouched, or it produces a `prep`. CHECK either REFUSES
\ and gives the PREP back untouched - it is the sole place the captured identity is
\ compared, and it also performs the one resource move that can fail - or it produces
\ a `checked-prep`. COMMIT-MAPPED cannot refuse at all: by the time a checked prep
\ exists, every question with a wrong answer has been asked and every step that could
\ fail has run. Two of the three halves refuse; the third is total, and the type
\ system is what keeps them in that order.
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
-5670 constant E-GX-FOREIGN  \ the prep was built against a different configuration
-5671 constant E-GX-IMAGE    \ the census no longer holds the checkpoint's bytes

\ ---- the prepared bind: opaque, linear, no accessors -------------------------
DEFLINEAR GPT2TX:prep

\ ---- what PREPARE answers ----------------------------------------------------
\ prepared carries the whole transaction; rejected gives the census back beside
\ the code that refused it.
ENUM prep-result 0
   VARIANT prepared FIELD p GPT2TX:prep ;VARIANT
   VARIANT rejected FIELD c SAFET:census FIELD code n ;VARIANT
;ENUM

\ ---- the compared bind: a prep whose identity matched, and whose mapping moved ----
\ A DISTINCT linear type, and CHECK is its only maker. That is what turns "the
\ identity was compared" from a rule COMMIT-MAPPED follows into a precondition the
\ CHECKER enforces: the commit cannot be reached without a value only the comparison
\ produces, and because a checked prep IS the prep after comparison, no mismatch
\ between a proof and the thing it is about is expressible.
DEFLINEAR GPT2TX:checked-prep

\ ---- what CHECK answers ----------------------------------------------------------
\ matched carries the whole transaction forward; refused gives the prep back beside
\ the code that turned it down, still live and still ABORTable. The arm is named for
\ what it DOES, not for one of its causes: it carries the foreign-identity refusal and
\ the surfaced memory refusal alike (see WHAT THE REFUSED ARM CARRIES, below).
ENUM check-result 0
   VARIANT matched FIELD c GPT2TX:checked-prep ;VARIANT
   VARIANT refused FIELD p GPT2TX:prep FIELD code n ;VARIANT
;ENUM

\ ---- the bound model -------------------------------------------------------------
\ The private-mint proof is an arity-0 NEWTYPE, the MDLCFG:cfg-proof shape: a
\ zero-field STRUCTURE fails closed as a product field, so a nominal cell family is
\ the one shape that can ride inside the record while staying constructible ONLY
\ through this package's trusted mint. A raw n in the proof slot is a checker reject.
NEWTYPE mdl-proof 0

\ A model owns its residency as ONE field, which is why WSTORE:resident exists: a
\ store is a two-owner bundle, and a record field may name a bare linear owner but
\ not a compound that transitively contains one (measured; the checker capability is
\ habu-checker-enum-payload-9e1ae6cc).
\
\ THE RECORD IS LINEAR BY CONTAINMENT, AND THAT HAS A CONSEQUENCE. Because the
\ resident field is linear, the checker refuses to duplicate or discard a model, which
\ is what makes the residency impossible to leak or double-free. It also means there
\ is NO non-consuming read of a model: `dup` is a reject, so every field read goes
\ through UNMAKE and rebuilds the record (see MODEL-NL / MODEL-KEY).
\
\ EXACTLY WHAT THE PROOF GUARANTEES, AND WHAT IT DOES NOT. The proof makes a model
\ unforgeable from NOTHING: `mdl-proof` has no constructor outside this package, so no
\ foreign file can assemble a model out of a residency and three scalars. That is the
\ whole of the guarantee. It does NOT make a genuine model tamper-proof, because the
\ generated UNMAKE is public, and three things follow that a reader should not have to
\ discover for themselves:
\   - the RESIDENCY CAN BE EXTRACTED. A holder can UNMAKE a real model and keep the
\     WSTORE:resident, then dispose it through WSTORE:RESIDENT-DISPOSE directly -
\     behind the model's back, bypassing MODEL-DISPOSE entirely;
\   - THE SCALARS CAN BE FORGED. UNMAKE also yields the proof, so the record can be
\     rebuilt around a DIFFERENT depth or a different captured cfgkey while carrying
\     the original proof. A model claiming twelve layers is therefore evidence that
\     this package built it, never that twelve is the depth PREPARE validated;
\   - so the proof answers "did GPT2TX mint this?" and never "has this been altered
\     since?".
\ It is the same caveat MDLCFG:mcfg and maki/typestate.f ART:built carry, and closing
\ it needs the sealed-destructure capability, habu-checker-sealed-destructure-d967fc03.
\ All three holes are pinned as ACCEPT candidates from OUTSIDE this package in
\ gpt2-bind-test.f's GPT2TX-DR section, so when that capability lands those pins fail
\ and this paragraph retires with them.
STRUCTURE gpt2-model 0
   FIELD res WSTORE:resident
   FIELD nl n
   FIELD key MDLCFG:cfgkey
   FIELD tok mdl-proof
;STRUCTURE

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

\ A checked prep is the SAME block as the prep it came from, with the census cell
\ replaced by the mapping CHECK moved out of that census, so its mint and its consume
\ are the same identities MINT-PREP and TAKE-PREP are.
TRUSTED: MINT-CHECKED ( ptr u8 -- GPT2TX:checked-prep ) ;
TRUSTED: TAKE-CHECKED ( GPT2TX:checked-prep -- ptr n ) ;

\ The moved file mapping, parked in that same cell. Same discipline and same cost as
\ the census and table crossings above: package-private, no public inverse, and from
\ the park until the recovery the checker cannot see the owner - what holds "owned
\ exactly once" is one park site in CHECK, one recovery site per exit (ABORT-CHECKED
\ and COMMIT-MAPPED), no accessor, plus the SAFET counters the suite asserts.
\ Retires with the linear-scope combinator habu-checker-linear-scope-6218899c.
TRUSTED: MAPPING>N ( SAFET:mapping -- n ) ;
TRUSTED: N>MAPPING ( n -- SAFET:mapping ) ;

\ The model's private-mint proof (the MDLCFG:MINT-CFG-PROOF shape).
TRUSTED: MINT-MDL-PROOF ( -- mdl-proof )  0 ;

\ ---- geometry and layout constants -------------------------------------------
4 constant NGLOBAL             \ |grole|: the checkpoint-global tensors
13 constant NBLOCK             \ |brole|: tensors per transformer block
$7FFFFFFFFFFFFFFF constant MAX-N
64 constant KEY-CAP            \ the GPT2BIND KEY-CAP bound: longest HF key is 39 bytes

\ prep block: the two moved owners, nlayer, the four cfgkey cells, the plan's
\ aggregates, and then the plan ITSELF - one row per validated slot. The plan travels
\ IN the prep, not in this package's scratch: a later PREPARE - including one that
\ refuses - overwrites the scratch while an earlier prep is still alive, so a commit
\ leaf reading process statics would read another transaction's numbers.
\
\ WHY THE ROWS ARE HERE AND NOT JUST THE AGGREGATES. P-CNT and P-SUM alone tell a
\ commit how big its arena is and how many slots it has, and that was enough for the
\ MAPPED arm, which needs no rows: it serves weights out of the file mapping through
\ the table PREPARE already sealed. An ALLOCATED commit needs more. To fill a PACKED
\ arena it must know, per slot, the extent (to place the row and advance the running
\ offset) and the CENSUS ID (SAFET:COPY-DATA? is keyed by id, and it is the only copy
\ the census offers). Neither survived a call anywhere: the sealed table has no public
\ row reader by design, and the walk spent each id on its checks and dropped it. So a
\ commit could only have reached its plan through the scratch - which is exactly the
\ corruption the paragraph above forbids, and it would have type-checked in silence.
\ Carrying the rows is what makes "a commit reads its plan from the prep it was given"
\ true of the whole plan rather than of its two summary numbers.
0 constant P-CEN
0 constant P-MAP               \ the same cell in a CHECKED block: the moved mapping
1 constant P-TBL
2 constant P-NL
3 constant P-K0
7 constant P-CNT               \ rows validated, and the sealed table's slot count
8 constant P-SUM               \ prefix sum of every extent (the packed arena size)
9 constant P-ROWS              \ first carried row cell; the block is P-ROWS + 3*count
3 constant P-ROW-CELLS
0 constant PR-OFF              \ the tensor's mapping-base-relative byte offset
1 constant PR-LEN              \ its byte extent
2 constant PR-ID               \ its census id, which is how the copy names it

\ row STAGING for one PLAN walk: (mapping offset, extent, census id) per slot, sized
\ by the largest census the loader can publish so pass one never allocates (see the
\ header). Nothing outside a single PREPARE call reads it - MINT copies the staged rows
\ into that call's own block, and the next call overwrites the staging buffer whether
\ it succeeds or refuses. The block copy is what any later reader must use.
0 constant R-OFF
1 constant R-LEN
2 constant R-ID
3 constant R-CELLS

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

\ ---- row staging accessors ----------------------------------------------------
: ROW! ( n n n n -- ) {: row:n off:n len:n id:n :}
   off  ROWS row R-CELLS * R-OFF + cells +  !
   len  ROWS row R-CELLS * R-LEN + cells +  !
   id   ROWS row R-CELLS * R-ID  + cells +  ! ;

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
   slot off len id ROW!                         \ the id is carried, not just spent
   r> ;

\ ---- pass one: validate everything, write no table ----------------------------
\ Stack-preserving so `catch` accepts it and a refusal leaves the census exactly
\ where it was (see the header). Nothing here creates or consumes an owner.
\
\ THE FIRST QUESTION IS WHETHER THE CENSUS STILL HAS BYTES AT ALL, and it has to be
\ asked here even though no per-row check needs it. A census that has already given up
\ its image through SAFET:DETACH-MAPPING keeps answering every reader this pass
\ consults: its count, its dtypes, its shapes, and MAP-OFFSET?, which is arithmetic on
\ the header geometry rather than access to any bytes. So an imageless census passes
\ every row - and the model built from it would own a residency of zero bytes, with the
\ fault surfacing at the first weight read as E-EXTENT, by which time the mapping and
\ the table have been deconstructed and no catch can recover them. Refusing it here
\ costs a caller nothing: the census comes back exactly as it arrived, still answering
\ its metadata and still disposable through SAFET:RELEASE.
: PLAN ( SAFET:census MDLCFG:mcfg -- SAFET:census MDLCFG:mcfg )
   0 SUM-N !
   GPT2BIND:CENSUS-COUNT {: count:n :}
   count 0 <=  count ROW-CAP >  or if E-GX-COUNT throw then
   count PLAN-N !
   count CLAIM-CLEAR
   >r                                            \ the mcfg parks while the census answers
   SAFET:MAP-LEN {: mlen:n :}
   SAFET:COUNT {: have:n :}
   r>
   mlen 0 <= if E-GX-IMAGE throw then
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
\ The block is sized by the count it will carry, and the bound is re-checked HERE,
\ at the point that turns a count into memory, rather than trusted from PLAN. PLAN
\ already refuses a count outside (0, ROW-CAP], so no census reaching this word can
\ make the guard fire - it stays for the reason V-SLOT's range test stays: this is
\ where the number becomes an allocation size and a multiplication, and a check at
\ that point does not depend on a caller continuing to guarantee a range it is free
\ to change. It also bounds the product: with ROW-CAP at SAFET's 2048-tensor cap the
\ widest block this can ask for is 9 + 3*2048 = 6153 cells, so the cell arithmetic
\ cannot overflow and MEM:BYTES-ALLOC-LEN validates the byte length after it.
: BLOCK-CELLS ( n -- n ) {: count:n :}
   count 0 <=  count ROW-CAP >  or if E-GX-COUNT throw then
   count P-ROW-CELLS * P-ROWS + ;

: PREP-ALLOC ( n -- CAD-NUM:alloc-byte-len )
   BLOCK-CELLS cells MEM:BYTES-ALLOC-LEN ;

\ ---- carried row cells --------------------------------------------------------
\ One address helper, so the row stride is written once. Every carried-row read and
\ write below goes through it; an off-by-one in the layout is therefore a single
\ place to be wrong rather than six.
: ROW-CELL ( ptr n n n -- ptr n ) {: blk:ptr row:n col:n :}
   blk P-ROWS row P-ROW-CELLS * + col + cells + ;

\ The single release path for a prep or checked-prep block, shared by all three exits
\ (ABORT, ABORT-CHECKED, COMMIT-MAPPED) so the length arithmetic and the counter live
\ in one place - the WSTORE:BLK-FREE discipline. This is package memory being given
\ back, the same unguarded free WSTORE's DISPOSE arms perform on their own blocks:
\ munmap of a region this process mapped, at the length it recorded, whose only
\ failure mode is a programming error rather than a runtime condition.
\ The block says how big it is: P-CNT is written by MINT and never rewritten, so the
\ release reads its own length out of the block it is about to give back. That is what
\ keeps this word's signature - and therefore the bodies of all three exits - unchanged
\ now that the length depends on the count.
: FREE-BLOCK ( ptr n -- )
   {: blk:ptr :}
   blk P-CNT cells + @ {: count:n :}
   blk BLK>BYTES count PREP-ALLOC MEM:RELEASE-BYTES
   -1 LIVE-N +! ;

\ The one site where the census and table stop being checker-tracked values, and it
\ is TOTAL: the caller hands in a block that is already allocated, so nothing
\ between the first erasure and the last cell write can throw. That is deliberate -
\ the erasures are the window where a throw would strand an owner the checker can
\ no longer see, so the only fallible step (the allocation) is kept outside it,
\ where PREPARE guards it and can still dispose both owners by hand.
\ Copies this walk's staged rows into the block that will outlive the walk. It runs
\ inside MINT's total stretch: the cells were reserved by the allocation PREPARE
\ already guarded, so there is nothing here that can fail.
\
\ THE COUNT COMES FROM THE BLOCK, WHICH IS WHAT MAKES THAT SAFE. The block is
\ self-describing: the step that sized the allocation wrote the count into P-CNT, and
\ every later size computation - this copy, the row readers, the release - reads it back
\ out. That is the WSTORE table-block discipline (TABLE-NEW writes the slot count,
\ BLK-FREE derives the length from it), and it is the difference between "these two
\ numbers are computed from the same variable so they agree" and "there is only one
\ number". Taking this count from the staging counter instead would let a block sized for
\ one count be filled to another, and the overflow would land in the allocation's own
\ slack where nothing observes it - measured, not hypothesised.
: COPY-ROWS ( ptr n n -- ) {: blk:ptr count:n :}
   count 0 ?do
      i R-OFF ROW@  blk i PR-OFF ROW-CELL !
      i R-LEN ROW@  blk i PR-LEN ROW-CELL !
      i R-ID  ROW@  blk i PR-ID  ROW-CELL !
   loop ;

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
   SUM-N @ blk P-SUM cells + !
   blk  blk P-CNT cells + @  COPY-ROWS   \ P-CNT was written by the allocating step
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

\ ---- the carried plan, one row at a time ---------------------------------------
\ What an ALLOCATED commit walks: for each slot, where the tensor sits in the mapping
\ frame, how many bytes it is, and the census id that names it to SAFET:COPY-DATA?.
\ Read off the block, so what comes back belongs to THIS transaction no matter how
\ many PREPARE calls have run since. The block-level reader is the one a commit uses -
\ it already holds the block pointer - and the prep-level wrapper exists for a holder
\ that has not taken the block apart yet.
: BLK-ROW ( ptr n n -- n n n ) {: blk:ptr row:n :}
   blk row PR-OFF ROW-CELL @
   blk row PR-LEN ROW-CELL @
   blk row PR-ID  ROW-CELL @ ;

: PREP-ROW ( GPT2TX:prep n -- GPT2TX:prep n n n ) {: row:n :}
   PREP>BLOCK row BLK-ROW ;

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

\ The captured identity, read straight off a block. Both the prep reader below and the
\ commit share it, so the four cells are assembled in exactly one place.
: BLK-KEY ( ptr n -- MDLCFG:cfgkey ) {: blk:ptr :}
   blk P-K0 0 + cells + @
   blk P-K0 1 + cells + @
   blk P-K0 2 + cells + @
   blk P-K0 3 + cells + @
   MDLCFG-CFGKEY:MAKE ;

: PREP-KEY ( GPT2TX:prep -- GPT2TX:prep MDLCFG:cfgkey )
   PREP>BLOCK BLK-KEY ;

\ ---- is this prep a stranger to the configuration about to consume it? ----------
\ The question a commit must answer before it moves any resource. PREPARE cannot
\ ask it: it mints every layer identity from the configuration it is validating, so
\ its identity assertion always compares a configuration with itself (see the header
\ note on E-GB-FOREIGN). What PREPARE does instead is CAPTURE the configuration's
\ content identity into the prep, and this is where that capture is spent - the
\ prep's captured cfgkey against the consuming configuration's own, through the
\ MDLCFG comparison, which is the only authority on cfgkey equality.
\
\ Geometry cannot substitute for it. Two configurations of one geometry differing
\ only in a field no tensor reflects - tied embeddings, say - bind the SAME census
\ and pass every per-row check, so nothing a commit could re-derive from the table
\ tells them apart; only the captured identity does. Neither value is a typed local
\ (only single-cell nominal families can be), so both keys ride the MDLCFG
\ whole-bundle-transport idiom.
: PREP-FOREIGN? ( GPT2TX:prep MDLCFG:mcfg -- GPT2TX:prep MDLCFG:mcfg bool )
   MDLCFG:CFGKEY@ >r                            \ the consuming key parks
   swap PREP-KEY                                \ ( mcfg prep cfgkey ), the captured key
   r> MDLCFG:CFGKEY= 0=
   >r swap r> ;                                 \ ( prep mcfg bool )

\ ---- reading a bound model -----------------------------------------------------
\ A model holds a linear resident, so the record is linear by containment and `dup` is
\ a checker reject: there is no non-consuming read. Each reader therefore UNMAKEs and
\ rebuilds, which is total, and the rebuilt record carries a freshly minted proof - the
\ proof asserts "minted inside this package", which is exactly as true of the rebuild.
\ These are the shape the forward pass will read geometry through.
: MODEL-NL ( gpt2-model -- gpt2-model n )
   GPT2TX-GPT2--MODEL:UNMAKE {: tok:mdl-proof :}
   >r {: nl:n :}                                \ the key parks; ( res )
   nl r> MINT-MDL-PROOF GPT2TX-GPT2--MODEL:MAKE
   nl ;

\ The key is a plain record, so it is the one field that can simply be copied: one copy
\ answers the caller, the other goes back into the rebuilt model.
: MODEL-KEY ( gpt2-model -- gpt2-model MDLCFG:cfgkey )
   GPT2TX-GPT2--MODEL:UNMAKE {: tok:mdl-proof :}
   dup >r                                       \ ( res nl key ), the copy parks
   MINT-MDL-PROOF GPT2TX-GPT2--MODEL:MAKE
   r> ;

\ ---- the two guarded steps after validation ------------------------------------
\ Both run under `catch` with an empty stack effect, so the census sitting below
\ them is untouched on either path and the two branches have the same shape - the
\ reason each parks its product in a package cell instead of returning it. Memory
\ is the only thing either can fail on, and a failure has to become a REFUSAL
\ rather than a throw: a throw here would unwind past the census (and, in the
\ second step, the sealed table) and strand owners the checker can no longer see.
: TABLE-STEP ( -- )
   PLAN-N @ FILL-TABLE TABLE>N PEND-TBL ! ;

\ Allocates the block AND stamps the count it was sized for into it, so the block
\ describes its own extent from the moment it exists. Every later size computation reads
\ that cell rather than recomputing from a counter (see COPY-ROWS).
: BLOCK-STEP ( -- )
   PLAN-N @ {: count:n :}
   count PREP-ALLOC MEM:ALLOC-BYTES drop PEND-BLK !
   count PEND-BLK @ P-CNT cells + ! ;

\ Gives the sealed table back when the block allocation failed. A failure to
\ release takes precedence in the reported code - it is the more proximate fault
\ and swallowing it would hide a real leak - otherwise the original cause stands.
: TBL-BACK ( WSTORE:table n -- n ) {: cause:n :}
   WSTORE:TABLE-DISPOSE RES-CODE {: rc:n :}
   rc 0 <> if rc else cause then ;

\ Folds the outcomes of two releases performed in sequence into the one code a caller
\ will see, the same precedence rule TBL-BACK states: the LATER failure wins, because
\ it is the more proximate fault, and reporting only the earlier one would hide it.
\ Neither is dropped on the floor - if the first release failed and the second
\ succeeded, the first is what surfaces. The alternative, reporting the first and
\ discarding the second, is how a real leak goes unnoticed.
: FOLD-CODE ( n n -- n ) {: first:n later:n :}
   later 0 <> if later else first then ;

\ ---- the transaction's one reachable failure, guarded ------------------------------
\ Taking the file mapping out of the census is the ONLY step in the whole bind that
\ can fail for a reason a caller cannot control: SAFET:DETACH-MAPPING allocates a
\ record first and throws E-MEM-MAP if that fails, leaving the census byte-identical.
\ It is guarded HERE, inside the word that can still answer with a refusal, which is
\ exactly why COMMIT-MAPPED downstream has no failure path left to guard.
\
\ Shape: a caught quotation must be stack-preserving and cannot read the caller's
\ locals, so the census crosses through a package cell and the mapping comes back the
\ same way (the PREPARE TABLE-STEP arrangement). On the throwing path nothing has
\ moved - the record allocation fails before any census cell is written - so the cell
\ still holds a whole census and CHECK rebuilds the prep around it.
variable PEND-CEN                               \ the census in, then the imageless census out
variable PEND-MAP                               \ the mapping the census gave up

: DETACH-STEP ( -- )
   PEND-CEN @ N>CENSUS
   SAFET:DETACH-MAPPING
   MAPPING>N PEND-MAP !
   CENSUS>N PEND-CEN ! ;

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
   blk FREE-BLOCK
   tc 0 <> if tc throw then ;

\ ---- the transaction's second half, part one: compare, then move ------------------
\ CHECK is the RECOVERABLE half of the commit, and the split is the whole design. It
\ answers two questions in order and both can refuse: is this prep this
\ configuration's, and can the mapping be taken out of its census. Everything that can
\ fail lives here, where a refusal is a VALUE - the prep comes back whole, still
\ ABORTable - because a throw out of a word that consumed a linear argument strands
\ that argument beyond any caller's catch. What survives to COMMIT-MAPPED therefore has
\ nothing left to fail.
\
\ THE IDENTITY COMPARE IS FIRST, AND BEFORE ANY RESOURCE MOVES. A prep built against
\ another configuration is
\ refused with the prep untouched: no mapping detached, no census released, no cell
\ rewritten, every counter where PREPARE left it. That ordering is the contract - the
\ compare cannot be after the detach, because the detach is terminal and a refusal then
\ would have nothing to give back.
\
\ WHAT THE REFUSED ARM CARRIES. Two different refusals, and its code says which:
\ E-GX-FOREIGN when the prep was built against another configuration, or the surfaced
\ E-MEM-MAP when the mapping could not be moved out of the census. That is why the arm
\ is not called `foreign` - the name would have described one cause and lied about the
\ other - and it is the same way PREPARE's rejected arm already surfaces whichever code
\ fired underneath it.
: CHECK ( GPT2TX:prep MDLCFG:mcfg -- check-result )
   PREP-FOREIGN? if
      drop
      E-GX-FOREIGN GPT2TX-CHECK--RESULT:REFUSED exit
   then
   drop                                         \ the configuration has answered
   TAKE-PREP {: blk:ptr :}
   blk P-CEN cells + @ PEND-CEN !
   [: DETACH-STEP ;] catch {: code:n :}
   code 0 <> if
      blk BLK>BYTES MINT-PREP                   \ nothing moved: the prep is whole again
      code GPT2TX-CHECK--RESULT:REFUSED exit
   then
   PEND-CEN @ N>CENSUS SAFET:RELEASE            \ the census has given up its image
   PEND-MAP @ blk P-MAP cells + !
   blk BLK>BYTES MINT-CHECKED
   GPT2TX-CHECK--RESULT:MATCHED ;

\ ---- total disposal of a compared prep ---------------------------------------------
\ The exit for a checked prep a holder decided not to commit. Its census is already
\ gone, so what it owns is the mapping and the table; both are given back, and the
\ block after them, BEFORE anything is reported - the ABORT discipline, so a failing
\ release cannot strand the owners the word had not reached yet.
\
\ The two release outcomes are folded into one reported code rather than checked in
\ sequence. Checking them in sequence looks equivalent and is not: it would report the
\ table's code and silently discard the mapping's, so a failed munmap of the
\ checkpoint - the one that leaves kernel state behind - could vanish behind a package
\ free that also failed. FOLD-CODE states which one wins and why.
: ABORT-CHECKED ( GPT2TX:checked-prep -- )
   TAKE-CHECKED {: blk:ptr :}
   blk P-TBL cells + @ N>TABLE WSTORE:TABLE-DISPOSE RES-CODE {: tc:n :}
   blk P-MAP cells + @ N>MAPPING SAFET:UNMAP-MAPPING RES-CODE {: mc:n :}
   blk FREE-BLOCK
   tc mc FOLD-CODE {: code:n :}
   code 0 <> if code throw then ;

\ ---- the transaction's second half, part two: the commit --------------------------
\ TOTAL. There is no catch in this word and no step in it that can fail for any reason
\ a caller could cause: the identity was decided by CHECK, the mapping was moved by
\ CHECK, and what remains is reading cells, one package-block free, and three total
\ constructions - WSTORE-STORE:MAPPED, WSTORE:HOLD (total by the reserved residency
\ cells), and the record mint. That is the point of the checked-prep type: the fallible
\ work is behind a value the checker demands, so the terminal stretch has nothing left
\ to guard and nothing that could strand the mapping it just took ownership of.
\
\ The block free is placed while the mapping and the table are still raw cells, so no
\ owner the checker tracks is live across it; it is the same unguarded package free
\ every WSTORE DISPOSE performs, and FREE-BLOCK says what its failure would mean.
\
\ GPT2TX:LIVE drops here: the prep block is gone and its two owners have moved into the
\ store the model holds.
: COMMIT-MAPPED ( GPT2TX:checked-prep -- gpt2-model )
   TAKE-CHECKED {: blk:ptr :}
   blk P-NL cells + @ {: nl:n :}
   blk BLK-KEY >r                               \ the captured identity, parked whole
   blk P-MAP cells + @ {: mc:n :}
   blk P-TBL cells + @ {: tc:n :}
   blk FREE-BLOCK
   mc N>MAPPING  tc N>TABLE  WSTORE-STORE:MAPPED  WSTORE:HOLD
   nl  r>  MINT-MDL-PROOF
   GPT2TX-GPT2--MODEL:MAKE ;

\ ---- the model's exit --------------------------------------------------------------
\ The single way a bound model's memory goes back: unwrap in-package and hand the
\ residency to its owner. ok carries what WSTORE gave back - the checkpoint mapping's
\ own byte length for a mapped model.
: MODEL-DISPOSE ( gpt2-model -- result<n,n> )
   GPT2TX-GPT2--MODEL:UNMAKE {: tok:mdl-proof :}
   drop drop                                    \ the captured key, then the depth
   WSTORE:RESIDENT-DISPOSE ;

\ ---- leak accounting (decides nothing; the WSTORE:LIVE pattern) -----------------
\ Undisposed prep blocks. The census and the table inside a prep are raw cells the
\ checker cannot follow, so a prep abandoned between PREPARE and ABORT takes both
\ of them with it and no type error is raised. This counter is what makes that
\ visible, and the acceptance suite asserts it around every transaction.
: LIVE ( -- n )
   LIVE-N @ ;

;package
