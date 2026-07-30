\ gpt2-prepare-test.f - GPT2LOAD checkpoint-load acceptance for PREPARE.
\ PREPARE, its rejections, its validated tensor rows, DISCARD-PREPARED, and its
\ configuration identity.
\
\ The other two load phases have their own suites, because one file
\ carrying all three grew past what the repository's own signature lint can read:
\   maki/infer/gpt2-mapped-test.f  the mapped-storage path - CHECK-MAPPED, LOAD-MAPPED, the model
\   maki/infer/gpt2-copy-test.f  the copied-storage path - CHECK-COPY, LOAD-COPIED
\ All three run on maki/infer/gpt2-checkpoint-fixture.f, which owns the synthetic-checkpoint
\ emitter, the leak counters and the shared assertion helpers. That file's header
\ explains how the fixtures are generated and why these suites reopen package GPT2LOAD.
\
\ What "the parsed tensor index is still usable" means for each rejection. Two forms of
\ rejection exist and they can prove different things. When the CONFIGURATION is
\ wrong for a good parsed tensor index, the same parsed tensor index is handed back and a second PREPARE with
\ the right configuration returns prepared - the full property. When the parsed tensor index
\ itself is damaged no configuration can ever accept it, so those tests prove the
\ production behavior instead: the parsed tensor index still answers its own readers, a second PREPARE
\ returns the SAME code (nothing was consumed or mutated on the way out), and it
\ then releases with every leak counter back at zero. Both shapes are exercised.
\
\ ON ALTERNATE CONFIGURATION IDENTITY, AND WHAT THE FAMILY GUARD DOES NOT CLAIM. PREPARE rejects a
\ non-GPT-2 model family before tensor validation. It cannot produce E-CONFIG-MISMATCH for
\ two GPT-2 configurations, and the reason is structural rather than an omission:
\ PREPARE creates every layer identity from the very configuration it is validating
\ against (GPT2TENSOR:LAYER-ID is the sole layer-id constructor), so the
\ identity assertion inside SLOT always compares a configuration with itself.
\ The alternate-configuration-key test
\ therefore proves the separate contract: two GPT-2 configurations of the SAME
\ geometry differing in one behavioral field that no tensor reflects (tied
\ embeddings) both load the same parsed tensor index, and the two prepared loads carry DIFFERENT captured
\ configuration keys. That captured key lets a load reject a model built against the other
\ configuration, so this test pins the capture instead of confusing family rejection
\ with downstream identity comparison.

require maki/infer/gpt2-checkpoint-fixture.f

package GPT2LOAD

variable EXPECTED-OFFSET                            \ the probed row read off the parsed tensor index
variable EXPECTED-LENGTH
variable EXPECTED-TENSOR-ID                             \ and the tensor ID that named it
variable ARITH-OFF  variable ARITH-LEN                  \ CHECK-BYTE-RANGES boundary arguments
variable CLAIM-ID  variable CLAIM-COUNT               \ CLAIM-TENSOR boundary arguments

: NON-GPT2-CONFIG ( -- MDLCFG:mcfg )
   2 8 10000.0 0.000001 MDLCFG-ARCH:LLAMA
   1 MODEL-ELEMENT-TYPE CONTEXT-SIZE VOCAB-SIZE LAYER-COUNT EMBED-SIZE HEAD-COUNT false 1 2 MDLCFG:BUILD ;

using SAFET

\ ---- consuming a prepare-result ---------------------------------------------------
\ Both variants consume their payload, so no test can forget a linear value.
: EXPECT-PREPARED ( GPT2LOAD:prepare-result -- )
   MATCH GPT2LOAD:prepare-result
      prepared OF DISCARD-PREPARED ENDOF
      rejected OF
         s" expected prepared, got rejection code" T-LABEL
         . cr
         RELEASE
         0 0= 0= TTRUE
      ENDOF
   ;MATCH ;

\ Gives the parsed tensor index back beside the code, so a test can inspect it.
: EXPECT-REJECTED ( GPT2LOAD:prepare-result n -- SAFET:census ) {: want:n :}
   MATCH GPT2LOAD:prepare-result
      prepared OF
         s" expected a rejection, got prepared" T-LABEL
         0 0= 0= TTRUE
         DISCARD-PREPARED
         FIXTURE-PATH LOAD                           \ keep the row shape after failure
      ENDOF
      rejected OF
         {: code:n :}
         code want T=
      ENDOF
   ;MATCH ;

\ ---- the happy path ------------------------------------------------------------
\ The probed rows are cross-checked against the parsed tensor index, not against a second copy
\ of the module's arithmetic: the row PREPARE validated must be the mapping offset
\ and byte length the parsed tensor index itself reports for that very tensor. Slot 6 is
\ layer 0's attn.bias causal-mask buffer - rank 4, the only shape of its kind - and
\ slot 7 (ATTENTION-WEIGHT-SLOT, in the fixture) is layer 0's c_attn Conv1D weight, the
\ [in,out] matrix. Neither is a global or a plain vector, so both are rows that would
\ move first if the slot walk drifted, and they sit either side of the global/block
\ boundary arithmetic.
6 constant MASK-SLOT                        \ h.0.attn.bias (mask, rank 4)

: SAVE-TENSOR-INDEX-ROW ( SAFET:census n -- SAFET:census ) {: slot:n :}
   MATCHING-CONFIG slot ROLE-NAME-LENGTH {: name-len:n :}
   drop                                         \ the configuration copy
   ROLE-NAME-BUFFER name-len FIND OPTION-VALUE {: id:n :}
   id MAP-OFFSET? OPTION-VALUE EXPECTED-OFFSET !
   id NBYTES? OPTION-VALUE EXPECTED-LENGTH !
   id EXPECTED-TENSOR-ID ! ;

: CHECK-STAGED-ROW ( n -- ) {: slot:n :}           \ the validated row is the parsed tensor index's own
   slot STAGED-ROW {: off:n len:n :}
   off EXPECTED-OFFSET @ T=
   len EXPECTED-LENGTH @ T= ;

\ The same assertion against what the prepared load carries, which is what a load will read.
\ Stability under an interfering PREPARE is proved elsewhere; this assertion proves
\ that the carried row is the RIGHT row. A snapshot compare cannot see a row that was
\ wrong in both copies. The tensor ID is checked too, because an ID that names the
\ wrong tensor would copy the wrong bytes into a correctly sized and placed span.
: CHECK-PREPARED-ROW ( GPT2LOAD:prepared-load n -- GPT2LOAD:prepared-load ) {: slot:n :}
   slot PREPARED-ROW {: off:n len:n id:n :}
   off EXPECTED-OFFSET @ T=
   len EXPECTED-LENGTH @ T=
   id  EXPECTED-TENSOR-ID @ T= ;

: EXPECT-PREPARED-ROW ( GPT2LOAD:prepare-result n -- ) {: slot:n :}
   MATCH GPT2LOAD:prepare-result
      prepared OF slot CHECK-PREPARED-ROW DISCARD-PREPARED ENDOF
      rejected OF
         s" expected prepared, got rejection code" T-LABEL
         . cr
         RELEASE
         0 0= 0= TTRUE
      ENDOF
   ;MATCH ;

: T-PREPARE-OK ( -- )
   s" a matching parsed tensor index and configuration yield prepared" T-LABEL
   RESET-FIXTURE  WRITE-CHECKPOINT
   FIXTURE-PATH LOAD
   MASK-SLOT SAVE-TENSOR-INDEX-ROW
   MATCHING-CONFIG PREPARE
   s" the mask row the prepared load CARRIES is the parsed tensor index's own offset, extent and id" T-LABEL
   MASK-SLOT EXPECT-PREPARED-ROW
   s" the mask row is the parsed tensor index's own offset and extent" T-LABEL
   MASK-SLOT CHECK-STAGED-ROW
   s" validation counted one row per tensor" T-LABEL
   STAGED-ROW-COUNT TENSOR-COUNT T=
   \ Every tensor ID is claimed exactly once: the walk is a bijection onto the parsed tensor index,
   \ so no tensor is assigned twice and none is left unassigned.
   s" the walk claimed every tensor exactly once" T-LABEL
   TENSOR-COUNT CLAIMED-TENSOR-COUNT TENSOR-COUNT T=
   \ The sum is the assertion that proves the WALK reached every slot: STAGED-ROW-COUNT is
   \ written before the walk starts and survives a rejection, so it says how many rows
   \ were intended, never how many were validated. Only the accumulated extent can
   \ come out equal to the whole data section if a slot was skipped.
   s" the walk covered the parsed tensor index: the prefix sum is the whole data section" T-LABEL
   STAGED-TOTAL-BYTES DATA-OFFSET @ T=
   s" a Conv1D row probes the same way" T-LABEL
   FIXTURE-PATH LOAD
   ATTENTION-WEIGHT-SLOT SAVE-TENSOR-INDEX-ROW
   MATCHING-CONFIG PREPARE
   ATTENTION-WEIGHT-SLOT EXPECT-PREPARED-ROW
   ATTENTION-WEIGHT-SLOT CHECK-STAGED-ROW
   \ A second load must start from a cleared accumulator, not add to the
   \ first: the same parsed tensor index over the same configuration has to report the same sum.
   s" a second PREPARE reports the same sum, so the accumulator was reset" T-LABEL
   STAGED-TOTAL-BYTES DATA-OFFSET @ T=
   EXPECT-NO-LEAK ;

: T-REJECT-MODEL-FAMILY ( -- )
   s" a non-GPT-2 configuration is rejected before tensor validation" T-LABEL
   RESET-FIXTURE WRITE-CHECKPOINT
   FIXTURE-PATH LOAD
   -1 STAGED-ROW-COUNT-CELL !
   -1 STAGED-TOTAL-BYTES-CELL !
   EXPECT-TENSOR-INDEX-RESOURCES
   NON-GPT2-CONFIG PREPARE
   E-MODEL-FAMILY EXPECT-REJECTED
   STAGED-ROW-COUNT-CELL @ -1 T=
   STAGED-TOTAL-BYTES-CELL @ -1 T=
   EXPECT-TENSOR-INDEX-RESOURCES
   SAFET:COUNT TENSOR-COUNT T=
   MATCHING-CONFIG PREPARE
   EXPECT-PREPARED
   EXPECT-NO-LEAK ;

\ ---- configuration rejections: the parsed tensor index still loads afterwards ----------------
: T-REJECT-CONFIG ( -- )
   s" one layer too many is rejected, and the same parsed tensor index still loads" T-LABEL
   RESET-FIXTURE  WRITE-CHECKPOINT
   FIXTURE-PATH LOAD
   EXTRA-LAYER-CONFIG PREPARE
   E-TENSOR-COUNT EXPECT-REJECTED                \ ( parsed tensor index )
   MATCHING-CONFIG PREPARE                             \ the very same parsed tensor index, right config
   EXPECT-PREPARED
   EXPECT-NO-LEAK
   s" a wider embedding is rejected on shape, and the parsed tensor index still loads" T-LABEL
   FIXTURE-PATH LOAD
   WIDE-EMBEDDING-CONFIG PREPARE
   E-TENSOR-SHAPE EXPECT-REJECTED
   MATCHING-CONFIG PREPARE
   EXPECT-PREPARED
   EXPECT-NO-LEAK ;

\ ---- tensor-index rejections: no configuration can accept it -------------------------------
\ Each test proves the parsed tensor index came back unchanged: it still answers its own reader,
\ a second PREPARE returns the same code, and it releases with nothing left over.
: EXPECT-DAMAGE-REJECTED ( n n n -- ) {: slot:n kind:n want:n :}
   slot kind SET-DAMAGE  WRITE-CHECKPOINT
   FIXTURE-PATH LOAD
   MATCHING-CONFIG PREPARE
   want EXPECT-REJECTED                      \ ( parsed tensor index )
   SAFET:COUNT TENSOR-COUNT kind DAMAGE-EXTRA = if 1 + then T=
   MATCHING-CONFIG PREPARE
   want EXPECT-REJECTED
   RELEASE
   EXPECT-NO-LEAK ;

: T-REJECT-TENSOR-INDEX ( -- )
   s" a tensor that is not F32 is rejected" T-LABEL
   2 DAMAGE-ELEMENT-TYPE E-ELEMENT-TYPE EXPECT-DAMAGE-REJECTED
   s" a mask tensor that is not F32 is rejected too (no exemption)" T-LABEL
   6 DAMAGE-ELEMENT-TYPE E-ELEMENT-TYPE EXPECT-DAMAGE-REJECTED
   s" a tensor with the wrong rank is rejected" T-LABEL
   0 DAMAGE-RANK E-TENSOR-RANK EXPECT-DAMAGE-REJECTED
   s" a tensor with one wrong dim is rejected" T-LABEL
   0 DAMAGE-SHAPE E-TENSOR-SHAPE EXPECT-DAMAGE-REJECTED
   s" a role whose exact key is absent is rejected" T-LABEL
   5 DAMAGE-NAME E-TENSOR-NAME EXPECT-DAMAGE-REJECTED
   s" one tensor beyond the parsed tensor index is rejected on count" T-LABEL
   -1 DAMAGE-EXTRA E-TENSOR-COUNT EXPECT-DAMAGE-REJECTED
   \ Slot 5 carries slot 4's name, so the parsed tensor index holds "h.0.ln_1.weight" twice and
   \ "h.0.ln_1.bias" not at all, with the count still right. The shadowed role looks
   \ up its OWN key and does not find it, so this is rejected as a missing key rather
   \ than as a collision - see the claim-set test for why that distinction matters.
   s" a parsed tensor index naming one tensor twice is rejected" T-LABEL
   5 DAMAGE-DUPLICATE-NAME E-TENSOR-NAME EXPECT-DAMAGE-REJECTED ;

\ ---- a parsed tensor index with no bytes left cannot be loaded --------------------------------
\ This state is reachable through the public surface:
\ SAFET:DETACH-MAPPING moves a parsed tensor index's image out, and the parsed tensor index keeps answering every
\ metadata reader afterwards - count, dtypes, shapes, and MAP-OFFSET?, which is pure
\ arithmetic on the header geometry it still holds. So a parsed tensor index without checkpoint bytes satisfies
\ every per-row question PREPARE asks, and without this rejection it loads: the model that
\ came out would own a residency of zero bytes. Its first value-level weight read
\ would safely return NONE with the store preserved, but the loader would already
\ have published a model that cannot execute. The missing bytes must be refused here,
\ in the phase whose job is to ask every question that has a wrong answer while a
\ rejection is still free.
\
\ For a parsed tensor index without checkpoint bytes, `rejected` returns the same
\ owner and a code, as it does for every other
\ rejection: the parsed tensor index comes back EXACTLY as it arrived. It is without checkpoint bytes, and that is not
\ damage this call did - it is the state the caller handed in. It still answers its own
\ metadata readers, and SAFET:RELEASE still disposes it (freeing the metadata only,
\ since it no longer owns any bytes), so the caller loses nothing when it is rejected.
: T-REJECT-NO-CHECKPOINT-BYTES ( -- )
   s" a parsed tensor index whose image has already left is rejected" T-LABEL
   RESET-FIXTURE  WRITE-CHECKPOINT
   FIXTURE-PATH LOAD                                 \ ( parsed tensor index )
   DETACH-MAPPING EXPECT-DETACHED-MAPPING            \ ( parsed tensor index mapping ) - the image leaves
   swap MATCHING-CONFIG PREPARE                        \ ( mapping prepare-result )
   E-NO-CHECKPOINT-BYTES EXPECT-REJECTED                \ ( mapping parsed tensor index )
   s" and the rejected parsed tensor index still answers, and still releases" T-LABEL
   SAFET:COUNT TENSOR-COUNT T=
   MAP-LEN 0 T=                                 \ without checkpoint bytes, exactly as it arrived
   RELEASE                                      \ ( mapping )
   UNMAP-MAPPING RESULT-CODE 0 T=
   EXPECT-NO-LEAK ;

\ ---- the claim set: one tensor per role ---------------------------------------
\ Why this test is not driven through a fixture. Every role looks up its own name and
\ SAFET:FIND answers the first tensor carrying it, so two roles can only land on one
\ tensor ID if two roles render the same name - a collision in the GPT2TENSOR
\ vocabulary, not in any checkpoint. No parsed tensor index mutation can produce it: duplicating
\ a name in the file leaves the shadowed role's name absent, which is rejected one
\ check earlier (the test above pins exactly that). So the collision the claim set
\ defends against is unreachable from today's vocabulary, and a fixture that
\ pretended otherwise would be theatre.
\
\ The guard still earns its place. Counts and shapes cannot detect this: with two
\ roles on one tensor the count is right, both roles' shape checks pass, and PREPARE
\ would hand back a table with two slots pointing at one tensor while another tensor
\ was never claimed, producing silently duplicated weights and wrong results. Only
\ claiming detects it. So the invariant is tested where it is
\ decided, on the production word, with the arguments crossing through package cells
\ because a quotation cannot read the caller's locals.
: SET-TENSOR-CLAIM-ARGS ( n n -- ) {: id:n count:n :}
   id CLAIM-ID !  count CLAIM-COUNT ! ;

: RUN-TENSOR-CLAIM ( -- )
   CLAIM-ID @ CLAIM-COUNT @ CLAIM-TENSOR ;

: T-TENSOR-CLAIMS ( -- )
   s" a tensor ID may be claimed once" T-LABEL
   TENSOR-COUNT CLEAR-CLAIMS
   3 TENSOR-COUNT SET-TENSOR-CLAIM-ARGS  [: RUN-TENSOR-CLAIM ;] 0 TTHROWSQ
   s" claiming it a second time is rejected" T-LABEL
   3 TENSOR-COUNT SET-TENSOR-CLAIM-ARGS  [: RUN-TENSOR-CLAIM ;] E-DUPLICATE-TENSOR TTHROWSQ
   s" a different id is still free" T-LABEL
   4 TENSOR-COUNT SET-TENSOR-CLAIM-ARGS  [: RUN-TENSOR-CLAIM ;] 0 TTHROWSQ
   s" clearing the set frees every id again" T-LABEL
   TENSOR-COUNT CLEAR-CLAIMS
   3 TENSOR-COUNT SET-TENSOR-CLAIM-ARGS  [: RUN-TENSOR-CLAIM ;] 0 TTHROWSQ
   s" an id outside the parsed tensor index is rejected before it indexes anything" T-LABEL
   TENSOR-COUNT TENSOR-COUNT SET-TENSOR-CLAIM-ARGS  [: RUN-TENSOR-CLAIM ;] E-TENSOR-SLOT TTHROWSQ
   -1 TENSOR-COUNT SET-TENSOR-CLAIM-ARGS  [: RUN-TENSOR-CLAIM ;] E-TENSOR-SLOT TTHROWSQ
   TENSOR-COUNT CLEAR-CLAIMS ;

\ ---- the extent arithmetic, pinned at its exact boundaries ---------------------
\ These overflow tests prove the bounds before a later load allocates
\ anything, so they are pinned where they actually decide: one byte either side of
\ the cell maximum. A quotation cannot read the caller's locals, so CHECK-BYTE-RANGES's two
\ arguments cross through package cells. The accumulator is set explicitly per case
\ rather than reached through a fixture, because no parsed tensor index can produce extents this
\ large - the boundary is unreachable from a file but entirely reachable in
\ arithmetic, which is exactly why it is tested here and not through PREPARE.
: SET-BYTE-RANGE-ARGS ( n n -- ) {: off:n len:n :}
   off ARITH-OFF !  len ARITH-LEN ! ;

: RUN-BYTE-RANGE-CHECK ( -- )
   ARITH-OFF @ ARITH-LEN @ CHECK-BYTE-RANGES ;

: T-BYTE-RANGES ( -- )
   s" a row end exactly at the cell maximum is accepted" T-LABEL
   0 STAGED-TOTAL-BYTES-CELL !  MAX-CELL 16 - 16 SET-BYTE-RANGE-ARGS  [: RUN-BYTE-RANGE-CHECK ;] 0 TTHROWSQ
   s" a row end one byte past it is rejected" T-LABEL
   0 STAGED-TOTAL-BYTES-CELL !  MAX-CELL 16 - 1 + 16 SET-BYTE-RANGE-ARGS  [: RUN-BYTE-RANGE-CHECK ;] E-BYTE-SIZE TTHROWSQ
   s" a prefix sum exactly at the cell maximum is accepted" T-LABEL
   MAX-CELL 16 - STAGED-TOTAL-BYTES-CELL !  0 16 SET-BYTE-RANGE-ARGS  [: RUN-BYTE-RANGE-CHECK ;] 0 TTHROWSQ
   STAGED-TOTAL-BYTES-CELL @ MAX-CELL T=
   s" a prefix sum one byte past it is rejected" T-LABEL
   MAX-CELL 16 - 1 + STAGED-TOTAL-BYTES-CELL !  0 16 SET-BYTE-RANGE-ARGS  [: RUN-BYTE-RANGE-CHECK ;] E-BYTE-SIZE TTHROWSQ
   s" a zero extent and a negative offset are rejected" T-LABEL
   0 STAGED-TOTAL-BYTES-CELL !  0 0 SET-BYTE-RANGE-ARGS  [: RUN-BYTE-RANGE-CHECK ;] E-BYTE-SIZE TTHROWSQ
   0 STAGED-TOTAL-BYTES-CELL !  -1 16 SET-BYTE-RANGE-ARGS  [: RUN-BYTE-RANGE-CHECK ;] E-BYTE-SIZE TTHROWSQ
   0 STAGED-TOTAL-BYTES-CELL ! ;

\ ---- the block grows with the count, and the growth is bounded -----------------
\ The block is now sized by a multiplication, so the boundaries of that arithmetic are
\ pinned here rather than left to the parsed tensor index that happens to arrive. The cap case is
\ the one that matters for memory: at MAX-ROW-COUNT the block is the widest this package can
\ ever ask for, and it is still an ordinary cell count.
: TEST-BLOCK-CELLS ( n -- n )  PREPARED-BLOCK-CELLS ;

: T-BLOCK-CELLS ( -- )
   s" the block carries the nine fixed cells plus three per row" T-LABEL
   1 TEST-BLOCK-CELLS LOAD-ROWS-CELL LOAD-ROW-CELLS + T=
   TENSOR-COUNT TEST-BLOCK-CELLS LOAD-ROWS-CELL TENSOR-COUNT LOAD-ROW-CELLS * + T=
   s" the real checkpoint's 160 rows make a 489-cell block" T-LABEL
   160 TEST-BLOCK-CELLS 489 T=
   s" a count at the parsed tensor index cap is accepted and is still a cell count" T-LABEL
   MAX-ROW-COUNT TEST-BLOCK-CELLS LOAD-ROWS-CELL MAX-ROW-COUNT LOAD-ROW-CELLS * + T=
   MAX-ROW-COUNT TEST-BLOCK-CELLS MAX-CELL < TTRUE
   s" a count of zero, a negative count, and one past the cap are rejected" T-LABEL
   [: 0 TEST-BLOCK-CELLS drop ;]          E-TENSOR-COUNT TTHROWSQ
   [: -1 TEST-BLOCK-CELLS drop ;]         E-TENSOR-COUNT TTHROWSQ
   [: MAX-ROW-COUNT 1 + TEST-BLOCK-CELLS drop ;] E-TENSOR-COUNT TTHROWSQ ;

\ ---- DISCARD-PREPARED gives everything back -----------------------------------------------
\ The counters are the proof: the parsed tensor index owner, its kernel mapping, and the WSTORE
\ table block all return to zero through DISCARD-PREPARED alone.
: T-DISCARD-PREPARED ( -- )
   s" DISCARD-PREPARED disposes the parsed tensor index, its mapping, and the table" T-LABEL
   RESET-FIXTURE  WRITE-CHECKPOINT
   FIXTURE-PATH LOAD
   MATCHING-CONFIG PREPARE
   MATCH GPT2LOAD:prepare-result
      prepared OF
         EXPECT-PREPARED-RESOURCES                           \ the prepared load owns the mapping and the table
         PREPARED-LAYER-COUNT LAYER-COUNT T=                       \ and the layer count a load will use
         DISCARD-PREPARED
      ENDOF
      rejected OF
         s" DISCARD-PREPARED test could not reach a prepared value" T-LABEL
         . cr RELEASE
         0 0= 0= TTRUE
      ENDOF
   ;MATCH
   EXPECT-NO-LEAK ;

\ ---- snapshot of every validated row carried by a prepared load ------------------------
\ The aggregates (count and sum) were always enough to catch a load that sized its
\ copy buffer from another load's numbers. They are not enough to catch a load that
\ reads another load's ROWS: two parsed tensor indexes of one geometry agree on both
\ aggregates and disagree on every row. So every carried row is copied aside
\ before the interfering PREPARE and compared cell for cell afterwards.
3 constant SNAPSHOT-ROW-CELLS                         \ (mapping offset, extent, tensor ID)
create ROW-SNAPSHOT TENSOR-COUNT SNAPSHOT-ROW-CELLS * cells allot
variable SNAPSHOT-COUNT

: SNAPSHOT-CELL ( n n -- ptr n ) {: row:n col:n :}
   ROW-SNAPSHOT row SNAPSHOT-ROW-CELLS * col + cells + ;

: SAVE-ROWS ( GPT2LOAD:prepared-load -- GPT2LOAD:prepared-load )   \ copy every carried row aside
   PREPARED-ROW-COUNT {: count:n :}
   count SNAPSHOT-COUNT !
   count 0 ?do
      i PREPARED-ROW {: off:n len:n id:n :}
      off  i 0 SNAPSHOT-CELL !
      len  i 1 SNAPSHOT-CELL !
      id   i 2 SNAPSHOT-CELL !
   loop ;

: ROWS-UNCHANGED ( GPT2LOAD:prepared-load -- GPT2LOAD:prepared-load )   \ every carried row unmoved, in order
   PREPARED-ROW-COUNT SNAPSHOT-COUNT @ T=
   SNAPSHOT-COUNT @ {: count:n :}
   count 0 ?do
      i PREPARED-ROW {: off:n len:n id:n :}
      off  i 0 SNAPSHOT-CELL @ T=
      len  i 1 SNAPSHOT-CELL @ T=
      id   i 2 SNAPSHOT-CELL @ T=
   loop ;

\ How many of a prepared-load's carried rows differ from the snapshot. The interfering
\ load is measured with this before its own prepared-load is thrown away: if its rows did
\ NOT differ, the fixture would be interfering with nothing and the stability assertion
\ above would pass for the wrong reason.
variable ROW-DIFFS

variable SAVED-TOTAL-BYTES                                \ the live prepared-load's own prefix sum, before any
                                                \ interfering emit rewrites DATA-OFFSET

: COUNT-ROW-DIFFS ( GPT2LOAD:prepared-load -- GPT2LOAD:prepared-load )
   0 ROW-DIFFS !
   PREPARED-ROW-COUNT {: count:n :}
   count 0 ?do
      i PREPARED-ROW {: off:n len:n id:n :}
      off  i 0 SNAPSHOT-CELL @ <>
      len  i 1 SNAPSHOT-CELL @ <>  or
      id   i 2 SNAPSHOT-CELL @ <>  or
      if ROW-DIFFS @ 1 + ROW-DIFFS ! then
   loop ;

\ Runs a WHOLE second load that succeeds, over the same model with every mapping
\ offset shifted. Its aggregates match the first's exactly - same tensor count, same
\ extents, same prefix sum - so only the rows can tell the two apart, which is what
\ makes it the right interference for a row-carrying block.
\ The interference that moves EXTENTS. Its rows disagree with the live prepared-load's in the two
\ columns the copied-storage path actually reads, so a load taking its rows from the scratch
\ would size and place every span wrongly - which the offset-only variant cannot show.
: PREPARE-WIDE ( -- )
   WRITE-WIDE-CHECKPOINT
   SHIFTED-PATH LOAD WIDE-EMBEDDING-CONFIG PREPARE
   MATCH GPT2LOAD:prepare-result
      prepared OF
         s" the wider load agrees on the tensor count" T-LABEL
         PREPARED-ROW-COUNT TENSOR-COUNT T=
         s" and disagrees on the prefix sum and on every row" T-LABEL
         PREPARED-TOTAL-BYTES SAVED-TOTAL-BYTES @ <> TTRUE
         COUNT-ROW-DIFFS
         ROW-DIFFS @ TENSOR-COUNT T=
         DISCARD-PREPARED
      ENDOF
      rejected OF
         s" the wider parsed tensor index failed to prepare" T-LABEL
         . cr RELEASE
         0 0= 0= TTRUE
      ENDOF
   ;MATCH ;

: PREPARE-SHIFTED ( -- )
   WRITE-SHIFTED-CHECKPOINT
   SHIFTED-PATH LOAD MATCHING-CONFIG PREPARE
   MATCH GPT2LOAD:prepare-result
      prepared OF
         s" the interfering load agrees on both aggregates" T-LABEL
         PREPARED-ROW-COUNT TENSOR-COUNT T=
         PREPARED-TOTAL-BYTES DATA-OFFSET @ T=
         s" and disagrees on every single row" T-LABEL
         COUNT-ROW-DIFFS
         ROW-DIFFS @ TENSOR-COUNT T=
         DISCARD-PREPARED
      ENDOF
      rejected OF
         s" the offset-shifted parsed tensor index failed to prepare" T-LABEL
         . cr RELEASE
         0 0= 0= TTRUE
      ENDOF
   ;MATCH ;

\ ---- the prepared load carries its own validated rows ----------------------------------
\ A prepared load must preserve its validated rows if another PREPARE runs before
\ it is consumed. After the first PREPARE returns, a second PREPARE runs and
\ rejects. That second call rewrites this package's scratch - STAGED-ROW-COUNT and
\ STAGED-TOTAL-BYTES now describe it, not the live prepared load. A load reading those statics would
\ size its copy buffer from another load's numbers. So the prepared-load's own copy is read
\ here and asserted to be unmoved, with the scratch proved to have moved underneath
\ it in the same breath.
: T-PREPARED-LOAD-OWNS-VALIDATED-ROWS ( -- )
   s" a prepared-load carries its own validated rows, not this package's scratch" T-LABEL
   RESET-FIXTURE  WRITE-CHECKPOINT
   FIXTURE-PATH LOAD MATCHING-CONFIG PREPARE
   MATCH GPT2LOAD:prepare-result
      prepared OF
         PREPARED-ROW-COUNT TENSOR-COUNT T=
         PREPARED-TOTAL-BYTES DATA-OFFSET @ T=
         DATA-OFFSET @ SAVED-TOTAL-BYTES !
         SAVE-ROWS
         FIXTURE-PATH LOAD EXTRA-LAYER-CONFIG PREPARE
         E-TENSOR-COUNT EXPECT-REJECTED RELEASE
         s" the rejected call did move the scratch" T-LABEL
         STAGED-ROW-COUNT TENSOR-COUNT <> TTRUE
         s" and the live prepared-load still reports its own validated rows" T-LABEL
         PREPARED-ROW-COUNT TENSOR-COUNT T=
         PREPARED-TOTAL-BYTES DATA-OFFSET @ T=
         s" every one of its carried rows is byte-identical" T-LABEL
         ROWS-UNCHANGED
         PREPARE-SHIFTED
         s" and they are still byte-identical after a SUCCEEDING load" T-LABEL
         PREPARED-ROW-COUNT TENSOR-COUNT T=
         PREPARED-TOTAL-BYTES DATA-OFFSET @ T=
         ROWS-UNCHANGED
         PREPARE-WIDE
         s" and after one whose extents differ, which is what this load reads" T-LABEL
         PREPARED-ROW-COUNT TENSOR-COUNT T=
         PREPARED-TOTAL-BYTES SAVED-TOTAL-BYTES @ T=
         ROWS-UNCHANGED
         DISCARD-PREPARED
      ENDOF
      rejected OF
         s" validated-row ownership test could not prepare" T-LABEL
         . cr RELEASE
         0 0= 0= TTRUE
      ENDOF
   ;MATCH
   EXPECT-NO-LEAK ;

\ ---- return the parsed tensor index ----------------------------------------------------
\ RETURN-TENSOR-INDEX converts a prepared load back into the parsed tensor index
\ it was built from. On an ordinary successful release, the block is gone, the table is gone,
\ and the parsed tensor index is not merely alive but UNCHANGED. The counters answer the first two,
\ and they catch a leaky implementation - a RETURN-TENSOR-INDEX that forgot the
\ table leaves the WSTORE counter high and nothing else moves.
\
\ The third claim needs the parsed tensor index to be used, because "still an owner" and "still
\ usable" are different properties and only the counters see the first. So the parsed tensor index
\ that comes back is handed straight to another PREPARE, and that load's validated
\ rows are compared with those the FIRST prepared-load carried - count, prefix sum, and every
\ carried row, cell for cell. A parsed tensor index that came back subtly damaged - an image quietly
\ detached, a reader partly consumed, or the wrong parsed tensor index entirely - passes every counter
\ assertion above and fails right here.
: T-RETURN-TENSOR-INDEX ( -- )
   s" RETURN-TENSOR-INDEX hands the parsed tensor index back and disposes everything else" T-LABEL
   RESET-FIXTURE  WRITE-CHECKPOINT
   FIXTURE-PATH LOAD MATCHING-CONFIG PREPARE
   MATCH GPT2LOAD:prepare-result
      prepared OF
         EXPECT-PREPARED-RESOURCES                             \ parsed tensor index, mapping, table and block
         SAVE-ROWS                             \ the validated rows this prepared-load carries
         RETURN-TENSOR-INDEX                               \ ( parsed tensor index )
         s" the prepared-load block and the sealed table are gone, and the parsed tensor index is not" T-LABEL
         EXPECT-TENSOR-INDEX-RESOURCES
         s" the parsed tensor index it answers with still reports its own tensors and image" T-LABEL
         SAFET:COUNT TENSOR-COUNT T=
         MAP-LEN 0 > TTRUE
         s" and it loads again with exactly the rows the first prepared-load carried" T-LABEL
         MATCHING-CONFIG PREPARE
         MATCH GPT2LOAD:prepare-result
            prepared OF
               PREPARED-ROW-COUNT TENSOR-COUNT T=
               PREPARED-TOTAL-BYTES DATA-OFFSET @ T=
               ROWS-UNCHANGED
               DISCARD-PREPARED
            ENDOF
            rejected OF
               s" the returned parsed tensor index would not prepare again, code" T-LABEL
               . cr RELEASE
               0 0= 0= TTRUE
            ENDOF
         ;MATCH
      ENDOF
      rejected OF
         s" RETURN-TENSOR-INDEX test could not prepare" T-LABEL
         . cr RELEASE
         0 0= 0= TTRUE
      ENDOF
   ;MATCH
   EXPECT-NO-LEAK ;

\ ---- alternate identity: same parsed tensor index, different configuration key ----------------
using MDLCFG-CFGKEY

: SAVE-PREPARED-KEY ( GPT2LOAD:prepared-load -- GPT2LOAD:prepared-load )
   PREPARED-CONFIG-KEY UNMAKE {: k0:n k1:n k2:n k3:n :}
   k0 CAPTURED-KEY0 !  k1 CAPTURED-KEY1 !  k2 CAPTURED-KEY2 !  k3 CAPTURED-KEY3 ! ;

: PREPARED-KEY-DIFFERS? ( GPT2LOAD:prepared-load -- GPT2LOAD:prepared-load bool )
   PREPARED-CONFIG-KEY UNMAKE {: k0:n k1:n k2:n k3:n :}
   k0 CAPTURED-KEY0 @ <>  k1 CAPTURED-KEY1 @ <>  or
   k2 CAPTURED-KEY2 @ <>  or  k3 CAPTURED-KEY3 @ <>  or ;

;using

: SAVE-PREPARED-KEY-OF ( MDLCFG:mcfg -- )             \ prepare, save the key, discard
   FIXTURE-PATH LOAD swap PREPARE
   MATCH GPT2LOAD:prepare-result
      prepared OF SAVE-PREPARED-KEY DISCARD-PREPARED ENDOF
      rejected OF
         s" alternate-configuration test could not prepare" T-LABEL
         . cr RELEASE
         0 0= 0= TTRUE
      ENDOF
   ;MATCH ;

: T-CONFIG-IDENTITIES ( -- )
   s" two configurations of one geometry both load the same parsed tensor index" T-LABEL
   RESET-FIXTURE  WRITE-CHECKPOINT
   MATCHING-CONFIG SAVE-PREPARED-KEY-OF                      \ stashes A's captured key
   EXPECT-NO-LEAK
   s" and the captured key is that configuration's own, cell for cell" T-LABEL
   MATCHING-CONFIG SAVE-CONFIG-KEY
   KEY-MATCHES-CONFIG
   s" and the prepared-load captures a DIFFERENT identity for each" T-LABEL
   FIXTURE-PATH LOAD OTHER-IDENTITY-CONFIG PREPARE
   MATCH GPT2LOAD:prepare-result
      prepared OF PREPARED-KEY-DIFFERS? TTRUE DISCARD-PREPARED ENDOF
      rejected OF
         s" alternate configuration B could not prepare" T-LABEL
         . cr RELEASE
         0 0= 0= TTRUE
      ENDOF
   ;MATCH
   EXPECT-NO-LEAK ;

\ ---- compare the prepared and requested configurations on the production word ------------
\ The mapped-load rejection rests on this one comparison, so it is tested on
\ the production word rather than on a re-derivation of it. It lives with the PREPARE
\ suite because OTHER-CONFIG? asks its question of a prepared load, and the captured key it
\ reads is the one the alternate-configuration test above pins. The prepared load is always built from
\ configuration A; what varies is the configuration that would consume it. A compare
\ that was deleted, or that compared a configuration with itself, would still pass
\ the first assertion and fail the second.
: TEST-CONFIG-DIFFERS? ( MDLCFG:mcfg -- bool )           \ the configuration requests the load
   FIXTURE-PATH LOAD MATCHING-CONFIG PREPARE                \ prepared for configuration A
   MATCH GPT2LOAD:prepare-result
      prepared OF                               \ ( configuration prepared-load )
         swap OTHER-CONFIG?                     \ ( prepared-load configuration bool )
         >r drop DISCARD-PREPARED r>
      ENDOF
      rejected OF                               \ ( configuration parsed tensor index code )
         s" configuration comparison test could not prepare" T-LABEL
         . cr RELEASE drop
         0 0= 0= TTRUE
         0 0=                                   \ keep the row shape after failure
      ENDOF
   ;MATCH ;

: T-DIFFERENT-CONFIG ( -- )
   s" a prepared-load matches the configuration it was prepared for" T-LABEL
   RESET-FIXTURE  WRITE-CHECKPOINT
   MATCHING-CONFIG TEST-CONFIG-DIFFERS? 0= TTRUE
   EXPECT-NO-LEAK
   s" a different configuration does not match, though it loads the same parsed tensor index" T-LABEL
   OTHER-IDENTITY-CONFIG TEST-CONFIG-DIFFERS? TTRUE
   EXPECT-NO-LEAK ;

;using

\ ---- checker rules for PREPARE ownership ---------------------------------------
\ The mapped and copied phases have the same static rule shape and live with the
\ suite that owns them: gpt2-mapped-test.f for the mapped-storage path and the model,
\ gpt2-copy-test.f for copy-ready.
: T-CHECKER-PREPARE ( -- )
   s" a prepared-load cannot be built from a raw cell or pointer" T-LABEL
   s" LOAD-BAD-PREPARED-RAW-CELL ( n -- GPT2LOAD:prepared-load ) " EXPECT-CHECKER-REJECTION
   s" LOAD-BAD-PREPARED-RAW-POINTER ( ptr u8 -- GPT2LOAD:prepared-load ) " EXPECT-CHECKER-REJECTION
   s" a prepared-load is linear: no copy, no discard, no store" T-LABEL
   s" LOAD-BAD-DUP ( GPT2LOAD:prepared-load -- GPT2LOAD:prepared-load GPT2LOAD:prepared-load ) dup" EXPECT-CHECKER-REJECTION
   s" LOAD-BAD-DROP ( GPT2LOAD:prepared-load -- ) drop" EXPECT-CHECKER-REJECTION
   s" LOAD-BAD-STORE ( GPT2LOAD:prepared-load ptr n -- ) !" EXPECT-CHECKER-REJECTION
   s" DISCARD-PREPARED consumes its prepared-load exactly once" T-LABEL
   s" LOAD-BAD-DISCARD-PREPARED-TWICE ( GPT2LOAD:prepared-load -- ) GPT2LOAD:DISCARD-PREPARED GPT2LOAD:DISCARD-PREPARED" EXPECT-CHECKER-REJECTION
   s" LOAD-BAD-DISCARD-PREPARED-RETURNS-CONSUMED-VALUE ( GPT2LOAD:prepared-load -- GPT2LOAD:prepared-load ) GPT2LOAD:DISCARD-PREPARED" EXPECT-CHECKER-REJECTION
   \ Both prepared-load exits consume the value and differ only in what they
   \ return, so the same static negatives apply to each.
   s" RETURN-TENSOR-INDEX consumes its prepared-load exactly once and answers with the parsed tensor index" T-LABEL
   s" LOAD-OK-RETURN-TENSOR-INDEX ( GPT2LOAD:prepared-load -- SAFET:census ) GPT2LOAD:RETURN-TENSOR-INDEX" EXPECT-CHECKER-ACCEPTANCE
   s" LOAD-BAD-RETURN-TENSOR-INDEX-TWICE ( GPT2LOAD:prepared-load -- SAFET:census ) GPT2LOAD:RETURN-TENSOR-INDEX GPT2LOAD:RETURN-TENSOR-INDEX" EXPECT-CHECKER-REJECTION
   s" LOAD-BAD-RETURN-TENSOR-INDEX-AFTER-DISCARD-PREPARED ( GPT2LOAD:prepared-load -- SAFET:census ) GPT2LOAD:DISCARD-PREPARED GPT2LOAD:RETURN-TENSOR-INDEX" EXPECT-CHECKER-REJECTION
   s" LOAD-BAD-DISCARD-PREPARED-AFTER-RETURN-TENSOR-INDEX ( GPT2LOAD:prepared-load -- SAFET:census ) GPT2LOAD:RETURN-TENSOR-INDEX GPT2LOAD:DISCARD-PREPARED" EXPECT-CHECKER-REJECTION
   s" LOAD-BAD-RETURN-TENSOR-INDEX-RETURNS-CONSUMED-VALUE ( GPT2LOAD:prepared-load -- GPT2LOAD:prepared-load SAFET:census ) GPT2LOAD:RETURN-TENSOR-INDEX" EXPECT-CHECKER-REJECTION
   s" LOAD-BAD-RETURN-TENSOR-INDEX-DROPPED ( GPT2LOAD:prepared-load -- ) GPT2LOAD:RETURN-TENSOR-INDEX" EXPECT-CHECKER-REJECTION
   s" LOAD-BAD-RETURN-TENSOR-INDEX-WITHOUT-INPUT ( -- SAFET:census ) GPT2LOAD:RETURN-TENSOR-INDEX" EXPECT-CHECKER-REJECTION
   s" the exit belongs to a prepared-load, and answers with a parsed tensor index and nothing else" T-LABEL
   s" LOAD-BAD-RETURN-TENSOR-INDEX-MAPPED-READY ( GPT2LOAD:mapped-ready -- SAFET:census ) GPT2LOAD:RETURN-TENSOR-INDEX" EXPECT-CHECKER-REJECTION
   s" LOAD-BAD-RETURN-TENSOR-INDEX-COPY-READY ( GPT2LOAD:copy-ready -- SAFET:census ) GPT2LOAD:RETURN-TENSOR-INDEX" EXPECT-CHECKER-REJECTION
   s" LOAD-BAD-RETURN-TENSOR-INDEX-TENSOR-INDEX ( SAFET:census -- SAFET:census ) GPT2LOAD:RETURN-TENSOR-INDEX" EXPECT-CHECKER-REJECTION
   s" LOAD-BAD-RETURN-TENSOR-INDEX-MAPPING ( GPT2LOAD:prepared-load -- SAFET:mapping ) GPT2LOAD:RETURN-TENSOR-INDEX" EXPECT-CHECKER-REJECTION
   s" prepare-result payloads cannot cross roles" T-LABEL
   s" LOAD-BAD-PREPARED-TENSOR-INDEX ( SAFET:census -- GPT2LOAD:prepare-result ) GPT2LOAD-PREPARE--RESULT:PREPARED" EXPECT-CHECKER-REJECTION
   s" LOAD-BAD-REJECTED-PREPARED ( GPT2LOAD:prepared-load n -- GPT2LOAD:prepare-result ) GPT2LOAD-PREPARE--RESULT:REJECTED" EXPECT-CHECKER-REJECTION
   s" LOAD-BAD-DISCARD-PREPARED-TENSOR-INDEX ( SAFET:census -- ) GPT2LOAD:DISCARD-PREPARED" EXPECT-CHECKER-REJECTION
   s" LOAD-BAD-RESULT-DROPPED ( SAFET:census MDLCFG:mcfg -- ) GPT2LOAD:PREPARE" EXPECT-CHECKER-REJECTION
   s" PREPARE answers about the parsed tensor index it is given, never ambient state" T-LABEL
   s" LOAD-BAD-WITHOUT-INPUT ( MDLCFG:mcfg -- GPT2LOAD:prepare-result ) GPT2LOAD:PREPARE" EXPECT-CHECKER-REJECTION
   s" the public surface resolves (controls)" T-LABEL
   s" LOAD-OK-PREPARE ( SAFET:census MDLCFG:mcfg -- GPT2LOAD:prepare-result ) GPT2LOAD:PREPARE" EXPECT-CHECKER-ACCEPTANCE
   s" LOAD-OK-DISCARD-PREPARED ( GPT2LOAD:prepared-load -- ) GPT2LOAD:DISCARD-PREPARED" EXPECT-CHECKER-ACCEPTANCE
   s" LOAD-OK-LIVE-PREPARED-LOADS ( -- n ) GPT2LOAD:LIVE-PREPARED-LOADS" EXPECT-CHECKER-ACCEPTANCE
   \ The owner-to-cell conversions are the whole soundness cost of this module, so their confinement
   \ is asserted, not assumed. A candidate names them qualified, the way a different configuration
   \ file would have to; each is unresolvable, which is what package-private means
   \ here. This tests the private representation claim in gpt2-load.f's header;
   \ refine-lint separately confines the inverse conversions to that file, because
   \ these probes cannot speak for a file that REOPENS the package.
   s" the PREPARE phase's audited owner-to-cell conversions stay package-private" T-LABEL
   s" LOAD-BAD-BLOCK-TO-PREPARED ( ptr u8 -- GPT2LOAD:prepared-load ) GPT2LOAD:BLOCK>PREPARED" EXPECT-PRIVATE-NAME
   s" LOAD-BAD-VIEW-PREPARED-BLOCK ( GPT2LOAD:prepared-load -- GPT2LOAD:prepared-load ptr n ) GPT2LOAD:VIEW-PREPARED-BLOCK" EXPECT-PRIVATE-NAME
   s" LOAD-BAD-TAKE-PREPARED-BLOCK ( GPT2LOAD:prepared-load -- ptr n ) GPT2LOAD:TAKE-PREPARED-BLOCK" EXPECT-PRIVATE-NAME
   s" LOAD-BAD-BLOCK-TO-BYTE-POINTER ( ptr n -- ptr u8 ) GPT2LOAD:BLOCK>BYTE-POINTER" EXPECT-PRIVATE-NAME
   s" LOAD-BAD-TENSOR-INDEX-TO-CELL ( SAFET:census -- n ) GPT2LOAD:TENSOR-INDEX>CELL" EXPECT-PRIVATE-NAME
   s" LOAD-BAD-CELL-TO-TENSOR-INDEX ( n -- SAFET:census ) GPT2LOAD:CELL>TENSOR-INDEX" EXPECT-PRIVATE-NAME
   s" LOAD-BAD-TABLE-TO-CELL ( WSTORE:table -- n ) GPT2LOAD:TABLE>CELL" EXPECT-PRIVATE-NAME
   s" LOAD-BAD-CELL-TO-TABLE ( n -- WSTORE:table ) GPT2LOAD:CELL>TABLE" EXPECT-PRIVATE-NAME ;

: RUN-PREPARE ( -- )
   T-RESET
   SAVE-BASELINE
   T-CHECKER-PREPARE
   T-TENSOR-CLAIMS
   T-BYTE-RANGES
   T-BLOCK-CELLS
   T-PREPARE-OK       EXPECT-NO-LEAK
   T-REJECT-MODEL-FAMILY  EXPECT-NO-LEAK
   T-REJECT-CONFIG       EXPECT-NO-LEAK
   T-REJECT-TENSOR-INDEX    EXPECT-NO-LEAK
   T-REJECT-NO-CHECKPOINT-BYTES EXPECT-NO-LEAK
   T-DISCARD-PREPARED            EXPECT-NO-LEAK
   T-PREPARED-LOAD-OWNS-VALIDATED-ROWS EXPECT-NO-LEAK
   T-RETURN-TENSOR-INDEX       EXPECT-NO-LEAK
   T-CONFIG-IDENTITIES EXPECT-NO-LEAK
   T-DIFFERENT-CONFIG  EXPECT-NO-LEAK
   DELETE-FIXTURES ;

RUN-PREPARE
T-REPORT

;package
