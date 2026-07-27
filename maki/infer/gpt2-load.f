\ gpt2-load.f - checked GPT-2 checkpoint loading. Package GPT2LOAD.
\
\ PREPARE validates the complete checkpoint before ownership moves. It checks
\ that checkpoint bytes still exist, the configuration names GPT-2, the tensor
\ count matches the layer count, every required tensor name exists, every tensor
\ is F32 with the required rank and shape, every slot and mapping range is valid,
\ packed-size arithmetic cannot overflow, and each checkpoint tensor serves one
\ role. Rejection returns the original parsed tensor index unchanged.
\
\ A prepared-load is one opaque linear value. Its private block owns the parsed
\ tensor index, the sealed file-offset table, the validated configuration
\ identity, and every validated tensor span. When each release succeeds,
\ DISCARD-PREPARED releases all of it, while RETURN-TENSOR-INDEX releases the
\ table and block but returns the original parsed tensor index. A release throw
\ can interrupt either operation until habu-make-owned-release-79de2b5c lands.
\
\ CHECK-MAPPED compares configuration identity before moving the file mapping.
\ Rejection returns the prepared load unchanged. Success produces mapped-ready,
\ whose only outcomes are DISCARD-MAPPED or LOAD-MAPPED. LOAD-MAPPED has no
\ caller-controlled rejection because validation and ownership transfer already
\ happened.
\
\ CHECK-COPY performs the same identity check without moving the parsed tensor
\ index. Success produces copy-ready. LOAD-COPIED then copies every validated
\ tensor into one copy buffer and builds a table over that buffer. When each
\ release succeeds, allocation or copy failure releases every acquired resource
\ and the input. A release throw can interrupt later cleanup until
\ habu-make-owned-release-79de2b5c lands.
\
\ Raw blocks cannot hold linear owners in a checker-visible form. The
\ package-private TRUSTED conversions below store those owners as cells until
\ the next state transition. TRUSTED.md inventories every conversion,
\ refine-lint confines each inverse to this file, and the tests assert that
\ private conversion names remain unreachable from outside the package. The
\ linear-scope and pointer-lifetime dots own removal of these boundaries.
\
\ The copied table is validated structurally during construction, but a model
\ cannot yet expose scoped reads from its WSTORE:resident. The resident-read
\ capability must land before forward execution can consume either storage path.
\
\ maki -> habu only. Owns -5660..-5673.

require lib/prelude.f
require lib/adt/option.f
require lib/adt/result.f
require lib/cad-num-arithmetic.f
require lib/memory.f
require maki/infer/safetensors.f
require maki/infer/weight-store.f
require maki/infer/model-config.f
require maki/infer/gpt2-tensor.f

package GPT2LOAD

public

\ ---- named rejection codes (this module owns -5660..-5673) --------------------
-5660 constant E-TENSOR-COUNT       \ tensor count is not 4 + 13*layer count
-5661 constant E-TENSOR-NAME        \ a role's exact checkpoint tensor name is not in the parsed tensor index
-5662 constant E-ELEMENT-TYPE       \ a tensor is not F32
-5663 constant E-TENSOR-RANK        \ a tensor's rank is not the role's rank
-5664 constant E-TENSOR-SHAPE       \ a tensor's dimension is not the role's dimension
-5665 constant E-MAPPING-OFFSET      \ the parsed tensor index reports no mapping offset for a tensor
-5666 constant E-TENSOR-SLOT        \ a role's slot is out of range, or not the slot walked
-5667 constant E-BYTE-SIZE        \ an extent, a row end, or the packed prefix sum overflows
-5668 constant E-NAME-TOO-LONG   \ a role's checkpoint tensor name did not fit the private render buffer
-5669 constant E-DUPLICATE-TENSOR       \ two roles resolved to one tensor
-5670 constant E-CONFIG-MISMATCH      \ the prepared-load was built against a different configuration
-5671 constant E-NO-CHECKPOINT-BYTES    \ the parsed tensor index no longer holds the checkpoint's bytes
-5672 constant E-SHORT-COPY        \ a validated span did not copy out of the parsed tensor index whole
-5673 constant E-MODEL-FAMILY    \ the normalized configuration is not GPT-2

\ ---- the prepared load: opaque, linear, no accessors -------------------------
DEFLINEAR GPT2LOAD:prepared-load

\ ---- what PREPARE answers ----------------------------------------------------
\ prepared carries the whole in-progress load; rejected gives the parsed tensor index back beside
\ the code that rejected it.
ENUM prepare-result 0
   VARIANT prepared FIELD load GPT2LOAD:prepared-load ;VARIANT
   VARIANT rejected FIELD tensor-index SAFET:census FIELD code n ;VARIANT
;ENUM

\ ---- configuration-checked mapped storage --------------------------------------
\ CHECK-MAPPED is the only constructor of `mapped-ready`. That makes successful
\ configuration comparison and mapping transfer static preconditions of
\ LOAD-MAPPED rather than rules its body must remember.
DEFLINEAR GPT2LOAD:mapped-ready

\ ---- what CHECK-MAPPED answers ----------------------------------------------------------
\ `ready` carries mapped-ready state. `rejected` returns the unchanged prepared
\ load with either E-CONFIG-MISMATCH or the defensive
\ E-NO-CHECKPOINT-BYTES code.
ENUM mapped-check-result 0
   VARIANT ready FIELD load GPT2LOAD:mapped-ready ;VARIANT
   VARIANT rejected FIELD load GPT2LOAD:prepared-load FIELD code n ;VARIANT
;ENUM

\ ---- configuration-checked copied storage --------------------------------------
\ A copied model still needs the parsed tensor index because it copies checkpoint
\ bytes from it. A mapped model instead owns the detached file mapping. Those
\ states cannot share one linear type, so CHECK-COPY is the only constructor of
\ `copy-ready`. It compares configuration identity without moving any resource.
DEFLINEAR GPT2LOAD:copy-ready

\ ---- what CHECK-COPY answers ----------------------------------------------------
\ `ready` carries copy-ready state. `rejected` returns the unchanged prepared
\ load with E-CONFIG-MISMATCH. The variants match mapped-check-result so callers
\ can handle both checks with the same pattern.
ENUM copy-check-result 0
   VARIANT ready FIELD load GPT2LOAD:copy-ready ;VARIANT
   VARIANT rejected FIELD load GPT2LOAD:prepared-load FIELD code n ;VARIANT
;ENUM

\ ---- the loaded model ---------------------------------------------------------
\ The private zero-field NEWTYPE is an unforgeable construction proof. A raw
\ number cannot fill the proof field.
NEWTYPE model-proof 0

\ The linear `weights` field makes the model linear, so it cannot be copied or
\ dropped. Field reads consume and rebuild the record. The proof prevents an
\ outside package from constructing a model from unrelated values, but the
\ generated public UNMAKE still lets a holder dismantle a genuine model and
\ rebuild it with changed scalar fields. The outside-package tests pin that
\ known checker gap. It retires with the sealed-destructure capability tracked
\ by habu-checker-sealed-destructure-d967fc03.
STRUCTURE gpt2-model 0
   FIELD weights WSTORE:resident
   FIELD layer-count n
   FIELD config-key MDLCFG:cfgkey
   FIELD proof model-proof
;STRUCTURE

private

\ ---- audited representation boundary -----------------------------------------
\ A prepared-load is represented by its block. These package-private conversions
\ create, view, or consume that representation. The checker cannot yet prove
\ that the pointer names a live GPT2LOAD block.
TRUSTED: BLOCK>PREPARED ( ptr u8 -- GPT2LOAD:prepared-load ) ;

TRUSTED: VIEW-PREPARED-BLOCK ( GPT2LOAD:prepared-load -- GPT2LOAD:prepared-load ptr n )
   dup ;

TRUSTED: TAKE-PREPARED-BLOCK ( GPT2LOAD:prepared-load -- ptr n ) ;

TRUSTED: BLOCK>BYTE-POINTER ( ptr n -- ptr u8 ) ;

\ The parsed tensor index and table are stored in the block as raw cells. See the
\ header for the missing checker capabilities that require this boundary.
TRUSTED: TENSOR-INDEX>CELL ( SAFET:census -- n ) ;
TRUSTED: CELL>TENSOR-INDEX ( n -- SAFET:census ) ;
TRUSTED: TABLE>CELL ( WSTORE:table -- n ) ;
TRUSTED: CELL>TABLE ( n -- WSTORE:table ) ;

\ Mapped-ready state is the same block with its parsed-tensor-index cell replaced
\ by the mapping that CHECK-MAPPED detached.
TRUSTED: BLOCK>MAPPED-READY ( ptr u8 -- GPT2LOAD:mapped-ready ) ;
TRUSTED: TAKE-MAPPED-BLOCK ( GPT2LOAD:mapped-ready -- ptr n ) ;

\ Copy-ready state is the same block with nothing replaced because CHECK-COPY
\ moves no resource.
TRUSTED: BLOCK>COPY-READY ( ptr u8 -- GPT2LOAD:copy-ready ) ;
TRUSTED: TAKE-COPY-BLOCK ( GPT2LOAD:copy-ready -- ptr n ) ;

\ The file mapping is stored in the same cell. CHECK-MAPPED is the only storing
\ path; DISCARD-MAPPED and LOAD-MAPPED are the only restoring paths. SAFET's
\ counters make a stranded mapping visible.
\ Retires with the linear-scope combinator habu-checker-linear-scope-6218899c.
TRUSTED: MAPPING>CELL ( SAFET:mapping -- n ) ;
TRUSTED: CELL>MAPPING ( n -- SAFET:mapping ) ;

\ LOAD-COPIED stores its owned byte buffer in a package cell while later steps
\ run under `catch`. A caught quotation must be stack-neutral, so the linear
\ buffer cannot pass directly between those steps. Each path restores it once.
\ Retires with the linear-scope combinator habu-checker-linear-scope-6218899c.
TRUSTED: BUFFER>CELL ( WSTORE:buffer -- n ) ;
TRUSTED: CELL>BUFFER ( n -- WSTORE:buffer ) ;

\ The model's private construction proof, following MDLCFG's proof pattern.
TRUSTED: MAKE-MODEL-PROOF ( -- model-proof )  0 ;

\ ---- geometry and layout constants -------------------------------------------
4 constant GLOBAL-TENSOR-COUNT             \ |global-role|: the checkpoint-global tensors
13 constant LAYER-ROLE-COUNT             \ |layer-role|: tensors per transformer block
$7FFFFFFFFFFFFFFF constant MAX-CELL
64 constant NAME-CAPACITY            \ longest GPT2TENSOR name is 39 bytes

\ prepared-load block: the two moved owners, layer count, four configuration-key
\ cells, row count, total bytes, and one row per validated slot. The validated rows travel
\ IN the prepared-load, not in this package's scratch: a later PREPARE - including one that
\ rejects - overwrites the scratch while an earlier prepared-load is still alive, so a load
\ step reading process statics would read another load's numbers.
\
\ WHY THE STAGED-ROWS ARE HERE AND NOT JUST THE AGGREGATES. LOAD-ROW-COUNT-CELL and LOAD-TOTAL-BYTES-CELL alone tell a
\ load how big its copy buffer is and how many slots it has, and that was enough for the
\ mapped-storage path, which needs no rows: it serves weights out of the file mapping through
\ the table PREPARE already sealed. A copied load needs more. To fill a packed
\ copy buffer it must know, per slot, the extent (to place the row and advance the running
\ offset) and tensor ID (SAFET:COPY-DATA? is keyed by ID, and it is the only copy
\ the parsed tensor index offers). Neither survived a call anywhere: the sealed table has no public
\ row reader by design, and the walk used each ID for validation and dropped it. So a
\ load could only have reached its validated rows through the scratch - which is exactly the
\ corruption the paragraph above forbids, and it would have type-checked in silence.
\ Carrying the rows makes the load read every validated row from the prepared-load
\ it was given, rather than trusting only the row count and total bytes.
0 constant LOAD-TENSOR-INDEX-CELL
0 constant LOAD-MAPPING-CELL               \ the same cell in a mapped-ready block
1 constant LOAD-TABLE-CELL
2 constant LOAD-LAYER-COUNT-CELL
3 constant LOAD-CONFIG-KEY-CELL
7 constant LOAD-ROW-COUNT-CELL               \ rows validated, and the sealed table's slot count
8 constant LOAD-TOTAL-BYTES-CELL               \ prefix sum of every extent (the copy-buffer size)
9 constant LOAD-ROWS-CELL              \ first carried row cell; the block is LOAD-ROWS-CELL + 3*count
3 constant LOAD-ROW-CELLS
0 constant LOAD-ROW-OFFSET              \ the tensor's mapping-base-relative byte offset
1 constant LOAD-ROW-LENGTH              \ its byte extent
2 constant LOAD-ROW-ID               \ its tensor ID, which is how the copy names it

\ row STAGING for one VALIDATE-CHECKPOINT walk: (mapping offset, extent, tensor ID) per slot, sized
\ by the largest parsed tensor index the loader can publish so pass one never allocates (see the
\ header). Nothing outside a single PREPARE call reads it - BUILD-PREPARED-LOAD copies the staged rows
\ into that call's own block, and the next call overwrites the staging buffer whether
\ it succeeds or rejects. The block copy is what any later reader must use.
0 constant STAGED-ROW-OFFSET
1 constant STAGED-ROW-LENGTH
2 constant STAGED-ROW-ID
3 constant STAGED-ROW-CELLS

create NAME-BUFFER NAME-CAPACITY allot                   \ private name render landing pad
create STAGED-ROWS SAFET:MAX-TENSORS STAGED-ROW-CELLS * cells allot
create CLAIMED-TENSORS SAFET:MAX-TENSORS cells allot       \ tensor IDs already claimed by a role

\ These three are SCRATCH for one PREPARE call and nothing else reads them
\ afterwards: the next call overwrites them, whether it succeeds or rejects. What
\ outlives the call is the copy BUILD-PREPARED-LOAD writes into the prepared-load block (LOAD-ROW-COUNT-CELL / LOAD-TOTAL-BYTES-CELL).
variable STAGED-TOTAL-BYTES-CELL                                  \ running sum of extents (packed-copy-buffer pre-clear)
variable STAGED-ROW-COUNT-CELL                                 \ the parsed tensor index count pass one validated

\ Saved between PREPARE's guarded steps because a quotation under `catch` cannot
\ hand a linear value back out - the two paths would have different stack shapes.
variable PENDING-TABLE                               \ the sealed table, as a raw cell
PTR-VARIABLE PENDING-BLOCK                           \ the prepared-load block the create will adopt

variable LIVE-PREPARED-COUNT                                 \ undisposed prepared-load blocks (accounting only)

: MAX-ROW-COUNT ( -- n )  SAFET:MAX-TENSORS ;

\ ---- row staging accessors ----------------------------------------------------
: STAGE-ROW ( n n n n -- ) {: row:n off:n len:n id:n :}
   off  STAGED-ROWS row STAGED-ROW-CELLS * STAGED-ROW-OFFSET + cells +  !
   len  STAGED-ROWS row STAGED-ROW-CELLS * STAGED-ROW-LENGTH + cells +  !
   id   STAGED-ROWS row STAGED-ROW-CELLS * STAGED-ROW-ID  + cells +  ! ;

: STAGED-ROW-CELL ( n n -- n ) {: row:n col:n :}
   STAGED-ROWS row STAGED-ROW-CELLS * col + cells + @ ;

\ ---- tensor-ID claims: the role-to-tensor map must be one-to-one ------------
\ Matching counts and matching shapes are not enough. If two roles ever render to
\ the same parsed tensor index name - a vocabulary collision, or a checkpoint that names two
\ tensors alike - every per-row check still passes: both roles find the same
\ tensor, its element type and shape satisfy both, and the counts still agree. The table
\ would then point two slots at one tensor while some other tensor is never
\ claimed, and the model would silently carry a duplicated weight. Counting is no
\ defense either, because the count is right. Only claiming is: each tensor ID may
\ be taken exactly once, and with count equal to tensor count, a walk that claims every ID
\ without collision has claimed all of them.
: CLEAR-CLAIMS ( n -- ) {: count:n :}
   count 0 ?do  0 CLAIMED-TENSORS i cells + !  loop ;

: CLAIM-TENSOR ( n n -- ) {: id:n count:n :}
   id 0 <  id count >=  or if E-TENSOR-SLOT throw then
   CLAIMED-TENSORS id cells + @ 0 <> if E-DUPLICATE-TENSOR throw then
   1 CLAIMED-TENSORS id cells + ! ;

\ ---- extract option and result values ----------------------------------------
\ A missing parsed tensor index answer is a rejection with this file's own code, never a -1
\ sentinel: the `none` variant throws, so only `some` produces a row.
: REQUIRE-VALUE ( option<n> n -- n ) {: code:n :}
   MATCH option
      none OF code throw ENDOF
      some OF ENDOF
   ;MATCH ;

: RESULT-CODE ( result<n,n> -- n )                 \ 0 for ok, the named code for err
   MATCH result
      ok  OF drop 0 ENDOF
      err OF ENDOF
   ;MATCH ;

\ ---- validated byte roles -----------------------------------------------------
\ WSTORE:SLOT! consumes narrowed offset/extent roles. Every rejection of the
\ narrowing is an extent fault here, which is why pass two cannot meet one.
: REQUIRE-OFFSET ( CAD-NUM:numeric-result<CAD-NUM:byte-off> -- CAD-NUM:byte-off )
   MATCH CAD-NUM:numeric-result
      ok OF ENDOF                              negative OF E-BYTE-SIZE throw ENDOF
      zero OF E-BYTE-SIZE throw ENDOF               overflow OF E-BYTE-SIZE throw ENDOF
      underflow OF E-BYTE-SIZE throw ENDOF          bad-alignment OF E-BYTE-SIZE throw ENDOF
      misaligned OF E-BYTE-SIZE throw ENDOF
   ;MATCH ;

: REQUIRE-LENGTH ( CAD-NUM:numeric-result<CAD-NUM:byte-len> -- CAD-NUM:byte-len )
   MATCH CAD-NUM:numeric-result
      ok OF ENDOF                              negative OF E-BYTE-SIZE throw ENDOF
      zero OF E-BYTE-SIZE throw ENDOF               overflow OF E-BYTE-SIZE throw ENDOF
      underflow OF E-BYTE-SIZE throw ENDOF          bad-alignment OF E-BYTE-SIZE throw ENDOF
      misaligned OF E-BYTE-SIZE throw ENDOF
   ;MATCH ;

: CHECKED-OFFSET ( n -- CAD-NUM:byte-off )   CAD-NUM:BYTE-OFF REQUIRE-OFFSET ;
: CHECKED-LENGTH ( n -- CAD-NUM:byte-len )   CAD-NUM:BYTE-LEN REQUIRE-LENGTH ;

\ ---- slot ordinal -> role (declaration order; the tables pin the role counts) --
: GLOBAL-ROLE ( n -- GPT2TENSOR:global-role )
   case
      0 of GPT2TENSOR-GLOBAL--ROLE:WTE   endof
      1 of GPT2TENSOR-GLOBAL--ROLE:WPE   endof
      2 of GPT2TENSOR-GLOBAL--ROLE:LNF-G endof
      3 of GPT2TENSOR-GLOBAL--ROLE:LNF-B endof
      E-TENSOR-SLOT throw
   endcase ;

: LAYER-ROLE ( n -- GPT2TENSOR:layer-role )
   case
      0  of GPT2TENSOR-LAYER--ROLE:LN1-G   endof
      1  of GPT2TENSOR-LAYER--ROLE:LN1-B   endof
      2  of GPT2TENSOR-LAYER--ROLE:MASK    endof
      3  of GPT2TENSOR-LAYER--ROLE:QKV-W   endof
      4  of GPT2TENSOR-LAYER--ROLE:QKV-B   endof
      5  of GPT2TENSOR-LAYER--ROLE:APROJ-W endof
      6  of GPT2TENSOR-LAYER--ROLE:APROJ-B endof
      7  of GPT2TENSOR-LAYER--ROLE:LN2-G   endof
      8  of GPT2TENSOR-LAYER--ROLE:LN2-B   endof
      9  of GPT2TENSOR-LAYER--ROLE:FC-W    endof
      10 of GPT2TENSOR-LAYER--ROLE:FC-B    endof
      11 of GPT2TENSOR-LAYER--ROLE:MPROJ-W endof
      12 of GPT2TENSOR-LAYER--ROLE:MPROJ-B endof
      E-TENSOR-SLOT throw
   endcase ;

\ The tensor-id this file expects at a slot. LAYER-ID is the sole layer-id constructor, so
\ the identity a layer tensor-id carries is this configuration's own by construction - which is
\ what makes the SLOT round trip below a real assertion about the FILE rather
\ than about the identity.
: TENSOR-ID-FOR-SLOT ( MDLCFG:mcfg n -- MDLCFG:mcfg GPT2TENSOR:tensor-id ) {: slot:n :}
   slot GLOBAL-TENSOR-COUNT < if
      slot GLOBAL-ROLE GPT2TENSOR-TENSOR--ID:GLOBAL exit
   then
   slot GLOBAL-TENSOR-COUNT - {: rel:n :}
   rel LAYER-ROLE-COUNT / GPT2TENSOR:LAYER-ID
   rel LAYER-ROLE-COUNT mod LAYER-ROLE GPT2TENSOR-TENSOR--ID:LAYER ;

\ ---- per-row validation -------------------------------------------------------
\ The slot the role claims must be in range for the parsed tensor index AND be the slot this walk
\ is on.
\
\ The range test cannot be reached through the public loader and is kept anyway. PREPARE creates every
\ layer-id through GPT2TENSOR:LAYER-ID, which validates the index, and SLOT revalidates
\ the embedded one before it multiplies, so the slot it returns is already inside
\ [0, parsed tensor index) unconditionally - no fixture reaching this word through PREPARE can
\ make this test fire, and deleting it leaves the whole suite green. It stays because
\ the range check belongs at the consumer: this value is about to index the
\ row scratch, and a check at the point of indexing does not depend on another
\ package continuing to guarantee a range it is free to change.
\
\ The equality test carries the information a fixture can reach: it
\ round-trips the slot formula, so an off-by-one on either side is a rejection instead
\ of a weight silently filed under the wrong role.
: CHECK-SLOT ( MDLCFG:mcfg n n -- MDLCFG:mcfg ) {: slot:n count:n :}
   slot TENSOR-ID-FOR-SLOT GPT2TENSOR:SLOT {: got:n :}
   got 0 <  got count >=  or if E-TENSOR-SLOT throw then
   got slot <> if E-TENSOR-SLOT throw then ;

\ The tensor ID for a role's exact checkpoint tensor name, rendered through the
\ public copy-out.
: FIND-TENSOR ( SAFET:census GPT2TENSOR:tensor-id -- SAFET:census n )
   NAME-BUFFER NAME-CAPACITY GPT2TENSOR:COPY-NAME? E-NAME-TOO-LONG REQUIRE-VALUE {: name-len:n :}
   NAME-BUFFER name-len SAFET:FIND E-TENSOR-NAME REQUIRE-VALUE ;

: CHECK-ELEMENT-TYPE ( SAFET:census n -- SAFET:census ) {: id:n :}
   id SAFET:DTYPE? E-ELEMENT-TYPE REQUIRE-VALUE
   SAFET:DT-F32 <> if E-ELEMENT-TYPE throw then ;

: CHECK-DIMENSION ( SAFET:census n n n -- SAFET:census ) {: id:n axis:n want:n :}
   id axis SAFET:DIM? E-TENSOR-SHAPE REQUIRE-VALUE
   want <> if E-TENSOR-SHAPE throw then ;

\ Rank first, then every dim the rank declares. The trailing 1s SHAPE pads
\ with are not parsed tensor index axes, so they are not looked up; the rank equality is what
\ makes that safe.
: CHECK-TENSOR-SHAPE ( SAFET:census n n n n n n -- SAFET:census )
   {: id:n rank:n d0:n d1:n d2:n d3:n :}
   id SAFET:RANK? E-TENSOR-RANK REQUIRE-VALUE
   rank <> if E-TENSOR-RANK throw then
   id 0 d0 CHECK-DIMENSION
   rank 1 > if id 1 d1 CHECK-DIMENSION then
   rank 2 > if id 2 d2 CHECK-DIMENSION then
   rank 3 > if id 3 d3 CHECK-DIMENSION then ;

: READ-TENSOR-SPAN ( SAFET:census n -- SAFET:census n n ) {: id:n :}
   id SAFET:MAP-OFFSET? E-MAPPING-OFFSET REQUIRE-VALUE {: off:n :}
   id SAFET:NBYTES? E-BYTE-SIZE REQUIRE-VALUE {: len:n :}
   off len ;

\ Two independent overflow facts, both required before a load can be arithmetic
\ free: this row's own end (off + len), and the running sum of every extent, which
\ is the size of the copy buffer LOAD-COPIED will ask for.
\ The first two tests cannot be reached through the public loader: SAFET:NBYTES?
\ derives its answer from a validated shape whose dims are all positive, and
\ MAP-OFFSET? is 8 + header length + a nonnegative begin, so no safetensors file
\ this loader will publish can present a zero extent or a negative offset. They are
\ kept because they are the preconditions the two overflow tests below are stated
\ against - a negative offset would make the row-end comparison meaningless - and a
\ future parsed tensor index producer is not obliged to preserve either property. The two
\ overflow tests ARE fixture-reachable and are pinned at their exact boundaries by
\ the CHECK-BYTE-RANGES test in the acceptance suite.
: CHECK-BYTE-RANGES ( n n -- ) {: off:n len:n :}
   len 0 <= if E-BYTE-SIZE throw then
   off 0 < if E-BYTE-SIZE throw then
   MAX-CELL len - off < if E-BYTE-SIZE throw then
   STAGED-TOTAL-BYTES-CELL @ {: sum:n :}
   MAX-CELL len - sum < if E-BYTE-SIZE throw then
   sum len + STAGED-TOTAL-BYTES-CELL ! ;

\ A configuration value cannot be a typed local (only arity-0 nominal families
\ can), so the configuration is stored on the return stack while the parsed tensor index has to
\ be on top. One save, one restore, and
\ every scalar in between is a named local.
: CHECK-AND-STAGE-ROW ( SAFET:census MDLCFG:mcfg n n -- SAFET:census MDLCFG:mcfg )
   {: slot:n count:n :}
   slot count CHECK-SLOT
   slot TENSOR-ID-FOR-SLOT GPT2TENSOR:SHAPE
   {: rank:n d0:n d1:n d2:n d3:n :}
   slot TENSOR-ID-FOR-SLOT
   swap >r                                      \ ( tensor-index tensor-id ), configuration saved
   FIND-TENSOR {: id:n :}
   id count CLAIM-TENSOR
   id CHECK-ELEMENT-TYPE
   id rank d0 d1 d2 d3 CHECK-TENSOR-SHAPE
   id READ-TENSOR-SPAN {: off:n len:n :}
   off len CHECK-BYTE-RANGES
   slot off len id STAGE-ROW                         \ carry the ID for copying
   r> ;

\ ---- pass one: validate everything, write no table ----------------------------
\ Stack-preserving so `catch` accepts it and a rejection leaves the parsed tensor index exactly
\ where it was (see the header). Nothing here creates or consumes an owner.
\
\ First confirm that the parsed tensor index still owns checkpoint bytes, even
\ though no per-row check needs them. A parsed tensor index that has given up
\ its image through SAFET:DETACH-MAPPING keeps answering every reader this pass
\ consults: its count, its dtypes, its shapes, and MAP-OFFSET?, which is arithmetic on
\ the header geometry rather than access to any bytes. So a parsed tensor index without checkpoint bytes passes
\ every row - and the model built from it would own a residency of zero bytes, with the
\ fault surfacing at the first weight read as E-EXTENT, by which time the mapping and
\ the table have been deconstructed and no catch can restore them. Refusing it here
\ costs a caller nothing: the parsed tensor index comes back exactly as it arrived, still answering
\ its metadata and still disposable through SAFET:RELEASE.
: CHECK-MODEL-FAMILY ( MDLCFG:mcfg -- MDLCFG:mcfg )
   MDLCFG:FAMILY@
   MODEL-FAMILY:GPT2 MODEL-FAMILY:EQ 0= if E-MODEL-FAMILY throw then ;

: VALIDATE-CHECKPOINT ( SAFET:census MDLCFG:mcfg -- SAFET:census MDLCFG:mcfg )
   CHECK-MODEL-FAMILY
   0 STAGED-TOTAL-BYTES-CELL !
   GPT2TENSOR:COUNT {: count:n :}
   count 0 <=  count MAX-ROW-COUNT >  or if E-TENSOR-COUNT throw then
   count STAGED-ROW-COUNT-CELL !
   count CLEAR-CLAIMS
   >r                                            \ save the configuration while the parsed tensor index answers
   SAFET:MAP-LEN {: mapping-length:n :}
   SAFET:COUNT {: actual-count:n :}
   r>
   mapping-length 0 <= if E-NO-CHECKPOINT-BYTES throw then
   actual-count count <> if E-TENSOR-COUNT throw then
   count 0 ?do  i count CHECK-AND-STAGE-ROW  loop ;

\ ---- pass two: build the sealed table from validated rows ---------------------
\ Infallible by construction except for memory: see the header's two-pass note.
: BUILD-MAPPED-TABLE ( n -- WSTORE:table ) {: count:n :}
   count WSTORE:TABLE-NEW
   count 0 ?do
      i  i STAGED-ROW-OFFSET STAGED-ROW-CELL CHECKED-OFFSET  i STAGED-ROW-LENGTH STAGED-ROW-CELL CHECKED-LENGTH  WSTORE:SLOT!
   loop
   WSTORE:SEAL ;

\ ---- the prepared-load block ------------------------------------------------------------
\ The block is sized by the count it will carry, and the range is rechecked HERE,
\ at the point that turns a count into memory, rather than trusting earlier validation.
\ VALIDATE-CHECKPOINT already rejects a count outside (0, MAX-ROW-COUNT], so no parsed tensor index reaching this word can
\ make the guard fire - it stays for the reason CHECK-SLOT's range test stays: this is
\ where the number becomes an allocation size and a multiplication, and a check at
\ that point does not depend on a caller continuing to guarantee a range it is free
\ to change. It also bounds the product: with MAX-ROW-COUNT at SAFET's 2048-tensor cap the
\ widest block this can ask for is 9 + 3*2048 = 6153 cells, so the cell arithmetic
\ cannot overflow and MEM:BYTES-ALLOC-LEN validates the byte length after it.
: PREPARED-BLOCK-CELLS ( n -- n ) {: count:n :}
   count 0 <=  count MAX-ROW-COUNT >  or if E-TENSOR-COUNT throw then
   count LOAD-ROW-CELLS * LOAD-ROWS-CELL + ;

: PREPARED-BLOCK-SIZE ( n -- CAD-NUM:alloc-byte-len )
   PREPARED-BLOCK-CELLS cells MEM:BYTES-ALLOC-LEN ;

\ ---- carried row cells --------------------------------------------------------
\ One address helper, so the row stride is written once. Every carried-row read and
\ write below goes through it; an off-by-one in the layout is therefore a single
\ place to be wrong rather than six.
: PREPARED-ROW-CELL ( ptr n n n -- ptr n ) {: blk:ptr row:n col:n :}
   blk LOAD-ROWS-CELL row LOAD-ROW-CELLS * + col + cells + ;

\ The single release path for a prepared-load, mapped-ready, or copy-ready block
\ keeps the length arithmetic and accounting in one place. MEM:RELEASE-BYTES can
\ throw E-MEM-UNMAP. Until habu-make-owned-release-79de2b5c makes owned release
\ uncatchably fatal, a caller that catches that throw can resume after an
\ interrupted cleanup; this file must not claim total release.
\ The block says how big it is: LOAD-ROW-COUNT-CELL is written by BUILD-PREPARED-LOAD and never rewritten, so the
\ release reads its own length out of the block it is about to give back. That is what
\ keeps this word's signature - and therefore the bodies of all three exits - unchanged
\ now that the length depends on the count.
\ It matters that this length and the ALLOCATION's length are one number rather than two
\ that agree: a block sized for one count and released at another leaves the difference
\ mapped, and an under-sized block filled to the larger count writes into the
\ allocation's own slack, where nothing observes it - measured, not hypothesised.
: RELEASE-PREPARED-BLOCK ( ptr n -- )
   {: blk:ptr :}
   blk LOAD-ROW-COUNT-CELL cells + @ {: count:n :}
   blk BLOCK>BYTE-POINTER count PREPARED-BLOCK-SIZE MEM:RELEASE-BYTES
   -1 LIVE-PREPARED-COUNT +! ;

\ The parsed tensor index and table stop being checker-tracked values here.
\ Allocation happens before this conversion. The conversion and row copy invoke
\ no fallible operation, so no throw can abandon the hidden owners before the
\ block is fully populated.
\
\ THE COUNT COMES FROM THE BLOCK, WHICH IS WHAT MAKES THAT SAFE. The block is
\ self-describing: the step that sized the allocation wrote the count into LOAD-ROW-COUNT-CELL, and
\ every later size computation - this copy, the row readers, the release - reads it back
\ out. That is the WSTORE table-block discipline (TABLE-NEW writes the slot count,
\ BLK-FREE derives the length from it), and it is the difference between "these two
\ numbers are computed from the same variable so they agree" and "there is only one
\ number".
: COPY-STAGED-ROWS ( ptr n n -- ) {: blk:ptr count:n :}
   count 0 ?do
      i STAGED-ROW-OFFSET STAGED-ROW-CELL  blk i LOAD-ROW-OFFSET PREPARED-ROW-CELL !
      i STAGED-ROW-LENGTH STAGED-ROW-CELL  blk i LOAD-ROW-LENGTH PREPARED-ROW-CELL !
      i STAGED-ROW-ID  STAGED-ROW-CELL  blk i LOAD-ROW-ID  PREPARED-ROW-CELL !
   loop ;

: BUILD-PREPARED-LOAD ( SAFET:census WSTORE:table ptr u8 n n n n n -- GPT2LOAD:prepared-load )
   {: k0:n k1:n k2:n k3:n layer-count:n :}
   BLOCK>PREPARED
   VIEW-PREPARED-BLOCK {: blk:ptr :}
   swap TABLE>CELL blk LOAD-TABLE-CELL cells + !
   swap TENSOR-INDEX>CELL blk LOAD-TENSOR-INDEX-CELL cells + !
   layer-count  blk LOAD-LAYER-COUNT-CELL cells + !
   k0  blk LOAD-CONFIG-KEY-CELL 0 + cells + !
   k1  blk LOAD-CONFIG-KEY-CELL 1 + cells + !
   k2  blk LOAD-CONFIG-KEY-CELL 2 + cells + !
   k3  blk LOAD-CONFIG-KEY-CELL 3 + cells + !
   STAGED-TOTAL-BYTES-CELL @ blk LOAD-TOTAL-BYTES-CELL cells + !
   blk  blk LOAD-ROW-COUNT-CELL cells + @  COPY-STAGED-ROWS   \ LOAD-ROW-COUNT-CELL was written by the allocating step
   1 LIVE-PREPARED-COUNT +! ;

\ ---- package-private projections for loading and tests -----------------------
\ None of these is public: a prepared-load never hands out its parsed tensor index, its table, a row, or
\ a pointer. What they do expose is what a load needs to create a model - the layer count
\ and the captured identity - plus what the acceptance fixtures need to see.
: PREPARED-LAYER-COUNT ( GPT2LOAD:prepared-load -- GPT2LOAD:prepared-load n )
   VIEW-PREPARED-BLOCK LOAD-LAYER-COUNT-CELL cells + @ ;

\ The row count and total bytes a prepared load was built against. Loading reads
\ them from HERE, never from this package's scratch: the scratch belongs to
\ the most recent PREPARE call, and that call may have rejected while an earlier
\ prepared-load is still alive and waiting to load.
: PREPARED-ROW-COUNT ( GPT2LOAD:prepared-load -- GPT2LOAD:prepared-load n )
   VIEW-PREPARED-BLOCK LOAD-ROW-COUNT-CELL cells + @ ;

: PREPARED-TOTAL-BYTES ( GPT2LOAD:prepared-load -- GPT2LOAD:prepared-load n )     \ bytes the copy buffer will need
   VIEW-PREPARED-BLOCK LOAD-TOTAL-BYTES-CELL cells + @ ;

\ ---- validated rows carried by the prepared load ------------------------------
\ What a copied load walks: for each slot, where the tensor sits in the mapping
\ offset base, how many bytes it is, and the tensor ID that names it to SAFET:COPY-DATA?.
\ Read off the block, so what comes back belongs to THIS load no matter how
\ many PREPARE calls have run since. The block-level reader is the one a load uses -
\ it already holds the block pointer - and the prepared-load wrapper exists for a holder
\ that has not taken the block apart yet.
: BLOCK-ROW ( ptr n n -- n n n ) {: blk:ptr row:n :}
   blk row LOAD-ROW-OFFSET PREPARED-ROW-CELL @
   blk row LOAD-ROW-LENGTH PREPARED-ROW-CELL @
   blk row LOAD-ROW-ID  PREPARED-ROW-CELL @ ;

: PREPARED-ROW ( GPT2LOAD:prepared-load n -- GPT2LOAD:prepared-load n n n ) {: row:n :}
   VIEW-PREPARED-BLOCK row BLOCK-ROW ;

\ ---- scratch readers, for this package's own acceptance suite only -------------
\ These describe the LAST PREPARE call, successful or not, and nothing outside the
\ suite may read them; the prepared-load readers above are what a load uses. They
\ exist so a fixture can check the validated rows without a public accessor.
: STAGED-ROW-COUNT ( -- n )
   STAGED-ROW-COUNT-CELL @ ;

: STAGED-TOTAL-BYTES ( -- n )
   STAGED-TOTAL-BYTES-CELL @ ;

: STAGED-ROW ( n -- n n )                         \ mapping offset + byte extent
   dup STAGED-ROW-OFFSET STAGED-ROW-CELL swap STAGED-ROW-LENGTH STAGED-ROW-CELL ;

\ How many tensor IDs the last walk claimed. A successful walk of n slots that
\ claimed n distinct IDs has claimed every ID exactly once - CLAIM-TENSOR rejects a
\ repeat, so n claims over a set of n IDs can only be a bijection. That is the
\ injectivity statement, and reading it here is what ties the claim set to the
\ walk rather than merely to an isolated unit test.
: CLAIMED-TENSOR-COUNT ( n -- n ) {: count:n :}
   0
   count 0 ?do  CLAIMED-TENSORS i cells + @ 0 <> if 1 + then  loop ;

\ The captured identity, read straight off a block. Both the prepared-load reader below and the
\ load share it, so the four cells are assembled in exactly one place.
: BLOCK-CONFIG-KEY ( ptr n -- MDLCFG:cfgkey ) {: blk:ptr :}
   blk LOAD-CONFIG-KEY-CELL 0 + cells + @
   blk LOAD-CONFIG-KEY-CELL 1 + cells + @
   blk LOAD-CONFIG-KEY-CELL 2 + cells + @
   blk LOAD-CONFIG-KEY-CELL 3 + cells + @
   MDLCFG-CFGKEY:MAKE ;

: PREPARED-CONFIG-KEY ( GPT2LOAD:prepared-load -- GPT2LOAD:prepared-load MDLCFG:cfgkey )
   VIEW-PREPARED-BLOCK BLOCK-CONFIG-KEY ;

\ ---- was this load prepared for a different configuration? -------------------
\ The question a load must answer before it moves any resource. PREPARE cannot
\ ask it: it creates every layer identity from the configuration it is validating, so
\ its identity assertion always compares a configuration with itself (see the header
\ note on E-CONFIG-MISMATCH). PREPARE instead records the configuration's content
\ identity in the prepared load. Here MDLCFG compares that recorded key with the
\ consuming configuration's key; MDLCFG defines configuration-key equality.
\
\ Geometry cannot substitute for it. Two configurations of one geometry differing
\ only in a field no tensor reflects - tied embeddings, say - load the SAME parsed tensor index
\ and pass every per-row check, so nothing a load could re-derive from the table
\ tells them apart; only the captured identity does. Neither value is a typed local
\ (only single-cell nominal families can be), so both complete keys stay on the
\ stack.
: OTHER-CONFIG? ( GPT2LOAD:prepared-load MDLCFG:mcfg -- GPT2LOAD:prepared-load MDLCFG:mcfg bool )
   MDLCFG:CFGKEY@ >r                            \ save the consuming key
   swap PREPARED-CONFIG-KEY                     \ ( configuration prepared-load configuration-key ), the captured key
   r> MDLCFG:CFGKEY= 0=
   >r swap r> ;                                 \ ( prepared-load configuration bool )

\ ---- reading a loaded model -----------------------------------------------------
\ A model holds a linear resident, so the record is linear by containment and `dup` is
\ a checker reject: there is no non-consuming read. Each reader therefore UNMAKEs and
\ rebuilds without allocation or release, and the rebuilt record carries a fresh proof - the
\ proof asserts "created inside this package", which is exactly as true of the rebuild.
\ These are the shape the forward pass will read geometry through.
: MODEL-LAYER-COUNT ( gpt2-model -- gpt2-model n )
   GPT2LOAD-GPT2--MODEL:UNMAKE {: proof:model-proof :}
   >r {: layer-count:n :}                       \ save the key; ( weights )
   layer-count r> MAKE-MODEL-PROOF GPT2LOAD-GPT2--MODEL:MAKE
   layer-count ;

\ The key is a plain record, so it is the one field that can simply be copied: one copy
\ answers the caller, the other goes back into the rebuilt model.
: MODEL-CONFIG-KEY ( gpt2-model -- gpt2-model MDLCFG:cfgkey )
   GPT2LOAD-GPT2--MODEL:UNMAKE {: proof:model-proof :}
   dup >r                                       \ ( weights layer-count key ), save the returned copy
   MAKE-MODEL-PROOF GPT2LOAD-GPT2--MODEL:MAKE
   r> ;

\ ---- the two guarded steps after validation ------------------------------------
\ Both run under `catch` with an empty stack effect, so the parsed tensor index sitting below
\ them is untouched on either path and the two branches have the same shape - the
\ reason each saves its result in a package cell instead of returning it. Memory
\ is the only thing either can fail on, and a failure has to become a rejection
\ rather than a throw: a throw here would unwind past the parsed tensor index (and, in the
\ second step, the sealed table) and abandon owners the checker can no longer see.
: BUILD-PENDING-TABLE ( -- )
   STAGED-ROW-COUNT-CELL @ BUILD-MAPPED-TABLE TABLE>CELL PENDING-TABLE ! ;

\ Allocates the block AND stamps the count it was sized for into it, so the block
\ describes its own extent from the moment it exists. Every later size computation reads
\ that cell rather than recomputing from a counter (see COPY-STAGED-ROWS).
: ALLOCATE-PENDING-BLOCK ( -- )
   STAGED-ROW-COUNT-CELL @ {: count:n :}
   count PREPARED-BLOCK-SIZE MEM:ALLOC-BYTES drop PENDING-BLOCK !
   count PENDING-BLOCK @ LOAD-ROW-COUNT-CELL cells + ! ;

\ Gives the sealed table back when the block allocation failed. A failure to
\ release takes precedence in the reported code - it is the more proximate fault
\ and swallowing it would hide a real leak - otherwise the original cause stands.
: RELEASE-TABLE-AFTER-ERROR ( WSTORE:table n -- n ) {: cause:n :}
   WSTORE:TABLE-DISPOSE RESULT-CODE {: release-code:n :}
   release-code 0 <> if release-code else cause then ;

\ Combines the results of two releases performed in sequence into the one code a caller
\ will see, the same precedence rule RELEASE-TABLE-AFTER-ERROR states: the LATER failure wins, because
\ it is the more proximate fault, and reporting only the earlier one would hide it.
\ Neither is dropped on the floor - if the first release failed and the second
\ succeeded, the first is what surfaces. The alternative, reporting the first and
\ discarding the second, is how a real leak goes unnoticed.
: MERGE-RELEASE-CODES ( n n -- n ) {: first:n later:n :}
   later 0 <> if later else first then ;

\ ---- the copied load's four fallible steps -----------------------------------
\ Unlike mapped storage, copied storage allocates a weight buffer, a table over
\ that buffer, and a buffer owner. Each allocation or copy step is guarded and
\ saves its owner in a package cell because a caught quotation must be
\ stack-neutral. Ordinary failure cleanup is tested. A release throw can still
\ interrupt later cleanup until habu-make-owned-release-79de2b5c lands.
PTR-VARIABLE COPY-BLOCK                             \ the copy-ready block being consumed
PTR-VARIABLE RAW-COPY-BUFFER                           \ the packed weight buffer
variable OWNED-COPY-BUFFER                                 \ the buffer that owns it, as a raw cell
variable COPY-TABLE                                 \ the copy-offset table, as a raw cell

\ Where slot n's bytes live in the copy buffer: the sum of every earlier extent. Both
\ the table build and the copy walk ask THIS word, so a row's copy-buffer address has exactly
\ one definition - two loops each keeping their own running total would look equivalent
\ and would silently misplace every weight after the first divergence.
: COPY-OFFSET ( ptr n n -- n ) {: blk:ptr slot:n :}
   0
   slot 0 ?do  blk i LOAD-ROW-LENGTH PREPARED-ROW-CELL @ +  loop ;

\ The copy buffer's byte length, in one place. It was written out three times - to allocate, to
\ adopt and to release - and three copies of an expression that must agree is how a
\ four-byte shortening survives into allocation slack where nothing observes it. This is
\ the WSTORE:BLK-FREE discipline: the size of a thing is computed where the thing is
\ described, once, and every user asks for it.
: COPY-BUFFER-SIZE ( -- CAD-NUM:alloc-byte-len )
   COPY-BLOCK @ LOAD-TOTAL-BYTES-CELL cells + @ MEM:BYTES-ALLOC-LEN ;

: ALLOCATE-COPY-BUFFER ( -- )
   COPY-BUFFER-SIZE MEM:ALLOC-BYTES drop RAW-COPY-BUFFER ! ;

\ created before the table and the copies so that a failure in either of those can give
\ the copy buffer back through one exit: a buffer owns the bytes, and WSTORE:BUFFER-DISPOSE
\ releases the record and the bytes together. Without that exit this path would have had
\ to release the raw copy buffer by hand at every later cleanup branch, duplicating WSTORE's own free.
: OWN-COPY-BUFFER ( -- )
   RAW-COPY-BUFFER @ COPY-BUFFER-SIZE WSTORE:BUFFER BUFFER>CELL OWNED-COPY-BUFFER ! ;

\ The copy-offset table: the same slots as the prepared-load's table, but each row placed where
\ the copy buffer put it rather than where the file did. Population cannot reject - the
\ count was range-checked, every extent was validated, and the running offsets are
\ bounded by the prefix sum CHECK-BYTE-RANGES already proved fits a cell. SEAL allocates nothing -
\ it retypes the block TABLE-NEW already made - so the ONE thing that can fail here is
\ TABLE-NEW's own allocation.
\ Population is stack-preserving, which is what lets BUILD-COPY-TABLE guard it and give the
\ BUILDER back if a row is ever rejected. It cannot be rejected by any prepared-load PREPARE built -
\ CHECK-BYTE-RANGES proved every row end and the whole prefix sum fit a cell - but an unreachable
\ rejection that leaks is still a leak, and WSTORE:BUILDER-DISPOSE is the exit that makes
\ handling it possible at all. SEAL's own rejection (E-UNSET) is the one case still not
\ recoverable, because a `catch` over it would have to hold a builder on one path and a
\ table on the other. It is unreachable here for a stronger reason - the loop writes every
\ slot exactly once - and it retires with habu-checker-linear-scope-6218899c.
: FILL-COPY-TABLE ( WSTORE:tbuilder -- WSTORE:tbuilder )
   COPY-BLOCK @ {: blk:ptr :}
   blk LOAD-ROW-COUNT-CELL cells + @ {: count:n :}
   count 0 ?do
      i  blk i COPY-OFFSET CHECKED-OFFSET  blk i LOAD-ROW-LENGTH PREPARED-ROW-CELL @ CHECKED-LENGTH  WSTORE:SLOT!
   loop ;

: BUILD-COPY-TABLE ( -- )
   COPY-BLOCK @ LOAD-ROW-COUNT-CELL cells + @ WSTORE:TABLE-NEW
   [: FILL-COPY-TABLE ;] catch {: code:n :}
   code 0 <> if
      WSTORE:BUILDER-DISPOSE RESULT-CODE {: builder-code:n :}
      code builder-code MERGE-RELEASE-CODES throw
   then
   WSTORE:SEAL TABLE>CELL COPY-TABLE ! ;

\ Copies every validated span out of the parsed tensor index and into its copy-buffer slot. This is the
\ step the carried tensor IDs exist for: SAFET:COPY-DATA? names a tensor by ID, and it
\ TRUNCATES to the capacity it is given rather than rejecting, so the returned length is
\ compared against the validated extent. A short or missing copy is E-SHORT-COPY -
\ it cannot happen for a parsed tensor index that passed PREPARE, and it is checked because a silent
\ partial copy would produce a model of plausible-looking wrong weights.
: COPY-WEIGHTS ( -- )
   COPY-BLOCK @ {: blk:ptr :}
   blk LOAD-TENSOR-INDEX-CELL cells + @ CELL>TENSOR-INDEX
   blk LOAD-ROW-COUNT-CELL cells + @ {: count:n :}
   count 0 ?do
      blk i LOAD-ROW-LENGTH PREPARED-ROW-CELL @ {: len:n :}
      blk i LOAD-ROW-ID  PREPARED-ROW-CELL @ {: id:n :}
      id  RAW-COPY-BUFFER @ blk i COPY-OFFSET BYTE+  len  SAFET:COPY-DATA?
      E-SHORT-COPY REQUIRE-VALUE
      len <> if E-SHORT-COPY throw then
   loop
   TENSOR-INDEX>CELL blk LOAD-TENSOR-INDEX-CELL cells + ! ;

\ ---- copied-load cleanup ----------------------------------------------------------
\ The prepared-load's own two owners and its block, in the DISCARD-PREPARED order: the table it was holding
\ (a file-offset table this path never uses), then the parsed tensor index, then the block. It is also
\ the SUCCESS path's cleanup, which is the point - the load consumes those three
\ whether it finishes or fails, so there is one word for it and the failure cleanup branches differ
\ only in what they release before reaching it.
: RELEASE-COPY-INPUT ( ptr n n -- n ) {: blk:ptr cause:n :}
   blk LOAD-TABLE-CELL cells + @ CELL>TABLE WSTORE:TABLE-DISPOSE RESULT-CODE {: table-code:n :}
   blk LOAD-TENSOR-INDEX-CELL cells + @ CELL>TENSOR-INDEX SAFET:RELEASE
   blk RELEASE-PREPARED-BLOCK
   cause table-code MERGE-RELEASE-CODES ;

: RELEASE-RAW-COPY-BUFFER ( -- )                          \ raw copy buffer before a buffer owns it
   RAW-COPY-BUFFER @ COPY-BUFFER-SIZE MEM:RELEASE-BYTES ;

: RELEASE-COPY-BUFFER ( -- n )                          \ the buffer that owns the copied bytes
   OWNED-COPY-BUFFER @ CELL>BUFFER WSTORE:BUFFER-DISPOSE RESULT-CODE ;

: RELEASE-COPY-TABLE ( -- n )                          \ the copy-offset table
   COPY-TABLE @ CELL>TABLE WSTORE:TABLE-DISPOSE RESULT-CODE ;

public

\ ---- prepare the load ---------------------------------------------------------
\ Either the parsed tensor index can be loaded as this configuration's GPT-2 and the whole
\ in-progress load comes back as one linear prepared-load, or it cannot and the parsed tensor index comes
\ back untouched beside the code that rejected it. Nothing in between: no partial
\ table, no moved mapping, no allocation left over on any rejection path.
: PREPARE ( SAFET:census MDLCFG:mcfg -- prepare-result )
   [: VALIDATE-CHECKPOINT ;] catch {: code:n :}
   code 0 <> if
      drop
      code GPT2LOAD-PREPARE--RESULT:REJECTED exit
   then
   MDLCFG:NLAYER@ {: layer-count:n :}
   MDLCFG:CFGKEY@ MDLCFG-CFGKEY:UNMAKE {: k0:n k1:n k2:n k3:n :}
   drop
   [: BUILD-PENDING-TABLE ;] catch {: table-code:n :}
   table-code 0 <> if table-code GPT2LOAD-PREPARE--RESULT:REJECTED exit then
   [: ALLOCATE-PENDING-BLOCK ;] catch {: block-code:n :}
   block-code 0 <> if
      PENDING-TABLE @ CELL>TABLE block-code RELEASE-TABLE-AFTER-ERROR
      GPT2LOAD-PREPARE--RESULT:REJECTED exit
   then
   PENDING-TABLE @ CELL>TABLE
   PENDING-BLOCK @ k0 k1 k2 k3 layer-count BUILD-PREPARED-LOAD
   GPT2LOAD-PREPARE--RESULT:PREPARED ;

\ ---- discard a prepared load -------------------------------------------------
\ This requests release of the table, parsed tensor index, and block in that
\ order. A block-release throw can interrupt the sequence described by the
\ public effect; habu-make-owned-release-79de2b5c owns the total-release fix.
: DISCARD-PREPARED ( GPT2LOAD:prepared-load -- )
   TAKE-PREPARED-BLOCK {: blk:ptr :}
   blk LOAD-TABLE-CELL cells + @ CELL>TABLE WSTORE:TABLE-DISPOSE RESULT-CODE {: table-code:n :}
   blk LOAD-TENSOR-INDEX-CELL cells + @ CELL>TENSOR-INDEX SAFET:RELEASE
   blk RELEASE-PREPARED-BLOCK
   table-code 0 <> if table-code throw then ;

\ ---- end the prepared load and return the parsed tensor index ----------------
\ DISCARD-PREPARED ends the whole load. This ends the prepared load and gives back what the caller
\ handed in: the table goes back through WSTORE:TABLE-DISPOSE, the block is freed, and
\ the parsed tensor index comes out of the block exactly as it went in - same tensors, same mapping,
\ still answering its readers, still releasable through SAFET:RELEASE and still
\ usable by another PREPARE. It is DISCARD-PREPARED's body with one owner kept instead of
\ released.
\
\ WHY IT EXISTS. Before it, the only thing a holder could do with a prepared-load it was not
\ going to load was throw it away, and that threw the parsed tensor index away with it. A
\ dispatcher cannot work that way: a rejection decided AFTER the prepared-load exists still has to
\ answer with the parsed tensor index the caller supplied, because that parsed tensor index is the caller's and
\ nothing about it was wrong. This is the word that makes "reject late, hand back what
\ you were given" expressible at all, and it is why PREPARE's `rejected` variant
\ and a later stage's rejection can carry the same payload.
\
\ RELEASE ORDER. The table is disposed first. The parsed tensor index is rebuilt
\ only after the block release succeeds, so no checker-visible owner crosses that
\ release. MEM:RELEASE-BYTES can still throw before the parsed tensor index is
\ returned; habu-make-owned-release-79de2b5c owns that release invariant.
: RETURN-TENSOR-INDEX ( GPT2LOAD:prepared-load -- SAFET:census )
   TAKE-PREPARED-BLOCK {: blk:ptr :}
   blk LOAD-TENSOR-INDEX-CELL cells + @ {: index-cell:n :}
   blk LOAD-TABLE-CELL cells + @ CELL>TABLE WSTORE:TABLE-DISPOSE RESULT-CODE {: table-code:n :}
   blk RELEASE-PREPARED-BLOCK
   table-code 0 <> if index-cell CELL>TENSOR-INDEX SAFET:RELEASE table-code throw then
   index-cell CELL>TENSOR-INDEX ;

\ ---- check the load: compare, then move -------------------------------------------
\ CHECK-MAPPED compares identity first, then asks SAFET to detach the mapping.
\ Either rejection returns the prepared load whole and still discardable. A
\ ready result has no caller-controlled rejection left for LOAD-MAPPED.
\
\ THE IDENTITY COMPARE IS FIRST, AND BEFORE ANY RESOURCE MOVES. A prepared-load built against
\ another configuration is
\ rejected with the prepared-load untouched: no mapping detached, no parsed tensor index released, no cell
\ rewritten, every counter where PREPARE left it. That ordering is the contract - the
\ compare cannot be after the detach, because the detach is terminal and a rejection then
\ would have nothing to give back.
\
\ The defensive `empty` variant cannot arise from a prepared load built through
\ the public path: PREPARE rejects a parsed tensor index without checkpoint bytes
\ and exposes no parsed-tensor-index accessor afterwards. CHECK-MAPPED still
\ handles it before creating mapped-ready state.
: CHECK-MAPPED ( GPT2LOAD:prepared-load MDLCFG:mcfg -- mapped-check-result )
   OTHER-CONFIG? if
      drop
      E-CONFIG-MISMATCH GPT2LOAD-MAPPED--CHECK--RESULT:REJECTED exit
   then
   drop                                         \ the configuration has answered
   TAKE-PREPARED-BLOCK {: blk:ptr :}
   blk LOAD-TENSOR-INDEX-CELL cells + @ CELL>TENSOR-INDEX SAFET:DETACH-MAPPING
   MATCH SAFET:map-take
      moved OF
         swap SAFET:RELEASE
         MAPPING>CELL blk LOAD-MAPPING-CELL cells + !
         blk BLOCK>BYTE-POINTER BLOCK>MAPPED-READY
         GPT2LOAD-MAPPED--CHECK--RESULT:READY
      ENDOF
      empty OF
         TENSOR-INDEX>CELL drop
         blk BLOCK>BYTE-POINTER BLOCK>PREPARED
         E-NO-CHECKPOINT-BYTES GPT2LOAD-MAPPED--CHECK--RESULT:REJECTED
      ENDOF
   ;MATCH ;

\ ---- release mapped-ready state -----------------------------------------------
\ The exit for mapped-ready state a holder decided not to load. Its parsed tensor index is already
\ gone, so it owns the mapping, table, and block. Their release results are
\ combined only if the intervening block release succeeds.
\
\ The two release outcomes are folded into one reported code rather than checked in
\ sequence. Checking them in sequence looks equivalent and is not: it would report the
\ table's code and silently discard the mapping's, so a failed munmap of the
\ checkpoint - the one that leaves kernel state behind - could vanish behind a package
\ free that also failed. MERGE-RELEASE-CODES states which one wins and why.
: DISCARD-MAPPED ( GPT2LOAD:mapped-ready -- )
   TAKE-MAPPED-BLOCK {: blk:ptr :}
   blk LOAD-TABLE-CELL cells + @ CELL>TABLE WSTORE:TABLE-DISPOSE RESULT-CODE {: table-code:n :}
   blk LOAD-MAPPING-CELL cells + @ CELL>MAPPING SAFET:UNMAP-MAPPING RESULT-CODE {: mapping-code:n :}
   blk RELEASE-PREPARED-BLOCK
   table-code mapping-code MERGE-RELEASE-CODES {: code:n :}
   code 0 <> if code throw then ;

\ ---- load mapped storage -----------------------------------------------------
\ No caller-controlled condition can reject this word: CHECK-MAPPED already
\ validated identity and moved the mapping. The package-block release can still
\ throw E-MEM-UNMAP before the raw owner cells are rebuilt. The total-release fix
\ belongs to habu-make-owned-release-79de2b5c.
\
\ The block free is placed while the mapping and the table are still raw cells, so no
\ owner the checker tracks is live across it; it is the same unguarded package free
\ every WSTORE DISPOSE performs, and RELEASE-PREPARED-BLOCK says what its failure would mean.
\
\ GPT2LOAD:LIVE-PREPARED-LOADS drops here: the prepared-load block is gone and its two owners have moved into the
\ store the model holds.
: LOAD-MAPPED ( GPT2LOAD:mapped-ready -- gpt2-model )
   TAKE-MAPPED-BLOCK {: blk:ptr :}
   blk LOAD-LAYER-COUNT-CELL cells + @ {: layer-count:n :}
   blk BLOCK-CONFIG-KEY >r                               \ save captured identity
   blk LOAD-MAPPING-CELL cells + @ {: mapping-cell:n :}
   blk LOAD-TABLE-CELL cells + @ {: table-cell:n :}
   blk RELEASE-PREPARED-BLOCK
   mapping-cell CELL>MAPPING  table-cell CELL>TABLE  WSTORE-STORE:MAPPED  WSTORE:HOLD
   layer-count  r>  MAKE-MODEL-PROOF
   GPT2LOAD-GPT2--MODEL:MAKE ;

\ ---- check copied storage: compare, and move nothing --------------------------
\ The same question CHECK-MAPPED asks first, and here it is the ONLY question: was
\ this prepared-load prepared for this configuration? Nothing else can reject, because nothing moves - no mapping is
\ detached, no parsed tensor index released, no cell rewritten, no memory taken. The prepared-load is simply
\ retyped into copy-ready, which is why this check has no resource failure and why its
\ `rejected` variant returns the prepared load untouched and still discardable.
\
\ The identity compare being FIRST is still the contract even with nothing to undo: the
\ load downstream takes real memory, and a prepared-load for a different configuration must be rejected before any
\ of that starts rather than after.
: CHECK-COPY ( GPT2LOAD:prepared-load MDLCFG:mcfg -- copy-check-result )
   OTHER-CONFIG? if
      drop
      E-CONFIG-MISMATCH GPT2LOAD-COPY--CHECK--RESULT:REJECTED exit
   then
   drop                                         \ the configuration has answered
   TAKE-PREPARED-BLOCK BLOCK>BYTE-POINTER BLOCK>COPY-READY
   GPT2LOAD-COPY--CHECK--RESULT:READY ;

\ ---- release copy-ready state -------------------------------------------------
\ The exit for copy-ready state a holder decided not to load. CHECK-COPY moved
\ nothing, so this state owns exactly what the prepared load owned.
: DISCARD-COPY ( GPT2LOAD:copy-ready -- )
   TAKE-COPY-BLOCK {: blk:ptr :}
   blk LOAD-TABLE-CELL cells + @ CELL>TABLE WSTORE:TABLE-DISPOSE RESULT-CODE {: table-code:n :}
   blk LOAD-TENSOR-INDEX-CELL cells + @ CELL>TENSOR-INDEX SAFET:RELEASE
   blk RELEASE-PREPARED-BLOCK
   table-code 0 <> if table-code throw then ;

\ ---- the copied load -------------------------------------------------------------
\ Copies the whole model into one packed buffer and returns a copied resident
\ store. Unlike LOAD-MAPPED, this word allocates memory and can fail. Its
\ allocation-failure tests prove cleanup when each release succeeds. They do not
\ prove cleanup after E-MEM-UNMAP: any release below may interrupt later cleanup.
\ habu-make-owned-release-79de2b5c owns that total-release correction.
\
\ The copy-buffer size, slot count, and every row come from the
\ copy-ready block and nothing else - never this package's scratch, which belongs to the
\ most recent PREPARE call rather than to this load.
\
\ Every ordinary success and allocation-failure branch reaches
\ RELEASE-COPY-INPUT once. A preceding release throw can prevent that call.
: LOAD-COPIED ( GPT2LOAD:copy-ready -- gpt2-model )
   TAKE-COPY-BLOCK COPY-BLOCK !
   [: ALLOCATE-COPY-BUFFER ;] catch {: alloc-code:n :}
   alloc-code 0 <> if COPY-BLOCK @ alloc-code RELEASE-COPY-INPUT throw then
   [: OWN-COPY-BUFFER ;] catch {: buffer-code:n :}
   buffer-code 0 <> if
      RELEASE-RAW-COPY-BUFFER
      COPY-BLOCK @ buffer-code RELEASE-COPY-INPUT throw
   then
   [: BUILD-COPY-TABLE ;] catch {: table-code:n :}
   table-code 0 <> if
      table-code RELEASE-COPY-BUFFER MERGE-RELEASE-CODES {: cleanup-code:n :}
      COPY-BLOCK @ cleanup-code RELEASE-COPY-INPUT throw
   then
   [: COPY-WEIGHTS ;] catch {: copy-code:n :}
   copy-code 0 <> if
      copy-code RELEASE-COPY-TABLE MERGE-RELEASE-CODES RELEASE-COPY-BUFFER MERGE-RELEASE-CODES {: cleanup-code:n :}
      COPY-BLOCK @ cleanup-code RELEASE-COPY-INPUT throw
   then
   COPY-BLOCK @ LOAD-LAYER-COUNT-CELL cells + @ {: layer-count:n :}
   COPY-BLOCK @ BLOCK-CONFIG-KEY >r                          \ save captured identity
   COPY-BLOCK @ 0 RELEASE-COPY-INPUT {: input-code:n :}
   input-code 0 <> if
      r> drop
      input-code RELEASE-COPY-TABLE MERGE-RELEASE-CODES RELEASE-COPY-BUFFER MERGE-RELEASE-CODES throw
   then
   OWNED-COPY-BUFFER @ CELL>BUFFER  COPY-TABLE @ CELL>TABLE  WSTORE-STORE:ALLOCATED  WSTORE:HOLD
   layer-count  r>  MAKE-MODEL-PROOF
   GPT2LOAD-GPT2--MODEL:MAKE ;

\ ---- the model's exit --------------------------------------------------------------
\ The single way a loaded model's memory goes back: unwrap in-package and hand the
\ residency to its owner. ok carries what WSTORE gave back - the checkpoint mapping's
\ own byte length for a mapped model.
: RELEASE-MODEL ( gpt2-model -- result<n,n> )
   GPT2LOAD-GPT2--MODEL:UNMAKE {: proof:model-proof :}
   drop drop                                    \ the captured key, then the layer count
   WSTORE:RESIDENT-DISPOSE ;

\ ---- leak accounting (decides nothing; the WSTORE:LIVE pattern) -----------------
\ Undisposed prepared-load blocks. The parsed tensor index and the table inside a prepared-load are raw cells the
\ checker cannot follow, so a prepared-load abandoned between PREPARE and DISCARD-PREPARED takes both
\ of them with it and no type error is raised. This counter is what makes that
\ visible, and the acceptance suite asserts it around every in-progress load.
: LIVE-PREPARED-LOADS ( -- n )
   LIVE-PREPARED-COUNT @ ;

;package
