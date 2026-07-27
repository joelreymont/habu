\ gpt2-bind-test.f - GPT2TX bind-transaction acceptance: the PREPARE half (rev-4 S6b1).
\ PREPARE, its refusals, the plan it carries, ABORT, and the identity it captures.
\
\ The other two halves of the transaction have their own suites, because one file
\ carrying all three grew past what the repository's own signature lint can read:
\   maki/infer/gpt2-check-test.f  the mapped arm - CHECK, COMMIT-MAPPED, the model
\   maki/infer/gpt2-alloc-test.f  the allocated arm - CHECK-ALLOC, COMMIT-ALLOCATED
\ All three run on maki/infer/gpt2-bind-fixture.f, which owns the synthetic-checkpoint
\ emitter, the leak counters and the shared assertion helpers. That file's header
\ explains how the fixtures are generated and why these suites reopen package GPT2TX.
\
\ WHAT "THE CENSUS IS STILL USABLE" MEANS ON EACH KIND OF REFUSAL. Two shapes of
\ rejection exist and they can prove different things. When the CONFIGURATION is
\ wrong for a good census, the same census is handed back and a second PREPARE with
\ the right configuration returns prepared - the full property. When the CENSUS
\ ITSELF is damaged no configuration can ever accept it, so those legs prove the
\ other half instead: the census still answers its own readers, a second PREPARE
\ returns the SAME code (nothing was consumed or mutated on the way out), and it
\ then releases with every leak counter back at zero. Both shapes are exercised.
\
\ ON THE IDENTITY TWIN, AND WHAT THE FAMILY GUARD DOES NOT CLAIM. PREPARE refuses a
\ non-GPT-2 model family before tensor planning. It cannot produce E-GB-FOREIGN for
\ two GPT-2 configurations, and the reason is structural rather than an omission:
\ PREPARE mints every layer identity from the very configuration it is validating
\ against (GPT2BIND:LAYER is the sole layerid constructor), so the identity assertion
\ inside TID-SLOT always compares a configuration with itself. The twin-cfgkey leg
\ therefore proves the separate contract: two GPT-2 configurations of the SAME
\ geometry differing in one behavioral field that no tensor reflects (tied
\ embeddings) both bind the same census, and the two preps carry DIFFERENT captured
\ cfgkeys. That captured key lets a commit refuse a model built against the other
\ configuration, so this leg pins the capture instead of confusing family rejection
\ with downstream identity comparison.

require maki/infer/gpt2-bind-fixture.f

package GPT2TX

variable TX-WANT-OFF                            \ the probed row read off the census
variable TX-WANT-LEN
variable TX-WANT-ID                             \ and the census id that named it
variable TX-AO  variable TX-AL                  \ V-ARITH boundary-leg arguments
variable TX-CID  variable TX-CCNT               \ CLAIM boundary-leg arguments

: TX-CFG-LLAMA ( -- MDLCFG:mcfg )
   2 8 10000.0 0.000001 MDLCFG-ARCH:LLAMA
   1 TX-DT TX-NC TX-NV TX-NL TX-NE TX-NH false 1 2 MDLCFG:BUILD ;

: BIND-TAKE-MOVED ( SAFET:map-take -- SAFET:mapping )
   MATCH SAFET:map-take
      moved OF ENDOF
      empty OF E-GX-IMAGE throw ENDOF
   ;MATCH ;

\ ---- consuming a prep-result ---------------------------------------------------
\ Both arms consume their payload, so no leg can forget a linear value.
: TX-EXPECT-PREPARED ( GPT2TX:prep-result -- )
   MATCH GPT2TX:prep-result
      prepared OF ABORT ENDOF
      rejected OF
         s" expected prepared, got refusal code" T-LABEL
         . cr
         SAFET:RELEASE
         0 0= 0= TTRUE
      ENDOF
   ;MATCH ;

\ Gives the census back beside the code, so a leg can keep questioning it.
: TX-EXPECT-REJECTED ( GPT2TX:prep-result n -- SAFET:census ) {: want:n :}
   MATCH GPT2TX:prep-result
      prepared OF
         s" expected a refusal, got prepared" T-LABEL
         0 0= 0= TTRUE
         ABORT
         TX-PATH SAFET:LOAD                     \ keep the row shape; the leg already failed
      ENDOF
      rejected OF
         {: code:n :}
         code want T=
      ENDOF
   ;MATCH ;

\ ---- the happy path ------------------------------------------------------------
\ The probed rows are cross-checked against the CENSUS, not against a second copy
\ of the module's arithmetic: the row PREPARE validated must be the mapping offset
\ and byte length the census itself reports for that very tensor. Slot 6 is
\ layer 0's attn.bias causal-mask buffer - rank 4, the only shape of its kind - and
\ slot 7 (TX-PROBE-CONV, in the fixture) is layer 0's c_attn Conv1D weight, the
\ [in,out] matrix. Neither is a global or a plain vector, so both are rows that would
\ move first if the slot walk drifted, and they sit either side of the global/block
\ boundary arithmetic.
6 constant TX-PROBE-SLOT                        \ h.0.attn.bias (mask, rank 4)

: TX-RECORD-PROBE ( SAFET:census n -- SAFET:census ) {: slot:n :}
   TX-CFG-A slot TX-KEY-LEN {: klen:n :}
   drop                                         \ the mcfg copy
   TX-KBUF klen SAFET:FIND TX-OPT-VAL {: id:n :}
   id SAFET:MAP-OFFSET? TX-OPT-VAL TX-WANT-OFF !
   id SAFET:NBYTES? TX-OPT-VAL TX-WANT-LEN !
   id TX-WANT-ID ! ;

: TX-PROBE-MATCHES ( n -- ) {: slot:n :}        \ the validated row is the census's own
   slot PLAN-ROW {: off:n len:n :}
   off TX-WANT-OFF @ T=
   len TX-WANT-LEN @ T= ;

\ The same assertion against what the PREP carries, which is what a commit will read.
\ Stability under an interfering PREPARE is proved elsewhere; this is the other half -
\ that the carried row is the RIGHT row. A snapshot compare cannot see a plan that was
\ wrong in both copies, and the census id is checked too, because an id that names the
\ wrong tensor would copy the wrong bytes into a correctly sized and placed span.
: TX-CARRIED-MATCHES ( GPT2TX:prep n -- GPT2TX:prep ) {: slot:n :}
   slot PREP-ROW {: off:n len:n id:n :}
   off TX-WANT-OFF @ T=
   len TX-WANT-LEN @ T=
   id  TX-WANT-ID @ T= ;

: TX-EXPECT-CARRIED ( GPT2TX:prep-result n -- ) {: slot:n :}
   MATCH GPT2TX:prep-result
      prepared OF slot TX-CARRIED-MATCHES ABORT ENDOF
      rejected OF
         s" expected prepared, got refusal code" T-LABEL
         . cr
         SAFET:RELEASE
         0 0= 0= TTRUE
      ENDOF
   ;MATCH ;

: T-PREPARE-OK ( -- )
   s" a matching census and configuration yield prepared" T-LABEL
   TX-CLEAN!  TX-LAY
   TX-PATH SAFET:LOAD
   TX-PROBE-SLOT TX-RECORD-PROBE
   TX-CFG-A PREPARE
   s" the mask row the PREP CARRIES is the census's own offset, extent and id" T-LABEL
   TX-PROBE-SLOT TX-EXPECT-CARRIED
   s" the mask row is the census's own offset and extent" T-LABEL
   TX-PROBE-SLOT TX-PROBE-MATCHES
   s" the plan counted one row per census tensor" T-LABEL
   PLAN-COUNT TX-CNT T=
   \ Every census id claimed exactly once: the walk is a bijection onto the census,
   \ so no tensor is bound twice and none is left unbound.
   s" the walk claimed every census tensor exactly once" T-LABEL
   TX-CNT PLAN-CLAIMED TX-CNT T=
   \ The sum is the assertion that proves the WALK reached every slot: PLAN-COUNT is
   \ written before the walk starts and survives a refusal, so it says how many rows
   \ were intended, never how many were validated. Only the accumulated extent can
   \ come out equal to the whole data section if a slot was skipped.
   s" the walk covered the census: the prefix sum is the whole data section" T-LABEL
   PLAN-SUM TX-DOFF @ T=
   s" a Conv1D row probes the same way" T-LABEL
   TX-PATH SAFET:LOAD
   TX-PROBE-CONV TX-RECORD-PROBE
   TX-CFG-A PREPARE
   TX-PROBE-CONV TX-EXPECT-CARRIED
   TX-PROBE-CONV TX-PROBE-MATCHES
   \ A second transaction must start from a cleared accumulator, not add to the
   \ first: the same census over the same configuration has to report the same sum.
   s" a second PREPARE reports the same sum, so the accumulator was reset" T-LABEL
   PLAN-SUM TX-DOFF @ T=
   TX-NO-LEAK ;

: T-REJECT-FAMILY ( -- )
   s" a non-GPT-2 configuration is refused before plan work" T-LABEL
   TX-CLEAN! TX-LAY
   TX-PATH SAFET:LOAD
   -1 PLAN-N !
   -1 SUM-N !
   TX-CENSUS-ONLY
   TX-CFG-LLAMA PREPARE
   E-GX-FAMILY TX-EXPECT-REJECTED
   PLAN-N @ -1 T=
   SUM-N @ -1 T=
   TX-CENSUS-ONLY
   SAFET:COUNT TX-CNT T=
   TX-CFG-A PREPARE
   TX-EXPECT-PREPARED
   TX-NO-LEAK ;

\ ---- refusals caused by the CONFIGURATION: the census still binds afterwards ----
: T-REJECT-CFG ( -- )
   s" one layer too many is refused, and the same census still binds" T-LABEL
   TX-CLEAN!  TX-LAY
   TX-PATH SAFET:LOAD
   TX-CFG-DEEP PREPARE
   E-GX-COUNT TX-EXPECT-REJECTED                \ ( census )
   TX-CFG-A PREPARE                             \ the very same census, right config
   TX-EXPECT-PREPARED
   TX-NO-LEAK
   s" a wider embedding is refused on shape, and the census still binds" T-LABEL
   TX-PATH SAFET:LOAD
   TX-CFG-WIDE PREPARE
   E-GX-SHAPE TX-EXPECT-REJECTED
   TX-CFG-A PREPARE
   TX-EXPECT-PREPARED
   TX-NO-LEAK ;

\ ---- refusals caused by the CENSUS: no configuration can accept it -------------
\ Each leg proves the census came back unmutated: it still answers its own reader,
\ a second PREPARE returns the same code, and it releases with nothing left over.
: TX-REJECT-DAMAGED ( n n n -- ) {: slot:n kind:n want:n :}
   slot kind TX-DAMAGE!  TX-LAY
   TX-PATH SAFET:LOAD
   TX-CFG-A PREPARE
   want TX-EXPECT-REJECTED                      \ ( census )
   SAFET:COUNT TX-CNT kind TX-BK-EXTRA = if 1 + then T=
   TX-CFG-A PREPARE
   want TX-EXPECT-REJECTED
   SAFET:RELEASE
   TX-NO-LEAK ;

: T-REJECT-CENSUS ( -- )
   s" a tensor that is not F32 is refused" T-LABEL
   2 TX-BK-DTYPE E-GX-DTYPE TX-REJECT-DAMAGED
   s" a mask tensor that is not F32 is refused too (no exemption)" T-LABEL
   6 TX-BK-DTYPE E-GX-DTYPE TX-REJECT-DAMAGED
   s" a tensor with the wrong rank is refused" T-LABEL
   0 TX-BK-RANK E-GX-RANK TX-REJECT-DAMAGED
   s" a tensor with one wrong dim is refused" T-LABEL
   0 TX-BK-SHAPE E-GX-SHAPE TX-REJECT-DAMAGED
   s" a role whose exact key is absent is refused" T-LABEL
   5 TX-BK-KEY E-GX-KEY TX-REJECT-DAMAGED
   s" one tensor beyond the census is refused on count" T-LABEL
   -1 TX-BK-EXTRA E-GX-COUNT TX-REJECT-DAMAGED
   \ Slot 5 carries slot 4's name, so the census holds "h.0.ln_1.weight" twice and
   \ "h.0.ln_1.bias" not at all, with the count still right. The shadowed role looks
   \ up its OWN key and does not find it, so this is refused as a missing key rather
   \ than as a collision - see the claim-set leg for why that distinction matters.
   s" a census naming one tensor twice is refused" T-LABEL
   5 TX-BK-ALIAS E-GX-KEY TX-REJECT-DAMAGED ;

\ ---- a census with no bytes left cannot be bound --------------------------------
\ The state this leg builds is reachable through the PUBLIC surface alone and by design:
\ SAFET:DETACH-MAPPING moves a census's image out, and the census keeps answering every
\ metadata reader afterwards - count, dtypes, shapes, and MAP-OFFSET?, which is pure
\ arithmetic on the header geometry it still holds. So an imageless census satisfies
\ every per-row question PREPARE asks, and without this refusal it binds: the model that
\ came out would own a residency of zero bytes, and the first weight read would throw
\ E-EXTENT with the mapping and the table already deconstructed by the MATCH, where no
\ catch can reach them (WSTORE's own header states that strand). The fault has to be
\ caught here, in the half whose whole job is to ask every question that has a wrong
\ answer while a refusal is still free.
\
\ WHAT rejected(census, code) MEANS FOR AN IMAGELESS CENSUS. The same as for every other
\ refusal: the census comes back EXACTLY as it arrived. It is imageless, and that is not
\ damage this call did - it is the state the caller handed in. It still answers its own
\ metadata readers, and SAFET:RELEASE still disposes it (freeing the metadata only,
\ since it no longer owns any bytes), so the caller loses nothing by being refused.
: T-REJECT-IMAGELESS ( -- )
   s" a census whose image has already left is refused" T-LABEL
   TX-CLEAN!  TX-LAY
   TX-PATH SAFET:LOAD                           \ ( census )
   SAFET:DETACH-MAPPING BIND-TAKE-MOVED         \ ( census mapping ) - the image leaves
   swap TX-CFG-A PREPARE                        \ ( mapping prep-result )
   E-GX-IMAGE TX-EXPECT-REJECTED                \ ( mapping census )
   s" and the refused census still answers, and still releases" T-LABEL
   SAFET:COUNT TX-CNT T=
   SAFET:MAP-LEN 0 T=                           \ imageless, exactly as it arrived
   SAFET:RELEASE                                \ ( mapping )
   SAFET:UNMAP-MAPPING RES-CODE 0 T=
   TX-NO-LEAK ;

\ ---- the claim set: one census tensor per role ---------------------------------
\ Why this leg is not driven through a fixture. Every role looks up its OWN key and
\ SAFET:FIND answers the first tensor carrying it, so two roles can only land on one
\ census id if two roles RENDER THE SAME KEY - a collision in the GPT2BIND
\ vocabulary, not in any checkpoint. No census mutation can produce it: duplicating
\ a name in the file leaves the shadowed role's key absent, which is refused one
\ check earlier (the leg above pins exactly that). So the collision the claim set
\ defends against is unreachable from today's vocabulary, and a fixture that
\ pretended otherwise would be theatre.
\
\ The guard still earns its place. Counts and shapes cannot detect this: with two
\ roles on one tensor the count is right, both roles' shape checks pass, and PREPARE
\ would hand back a table with two slots pointing at one tensor while another tensor
\ was never claimed - a silently duplicated weight, discovered a leaf or two later as
\ wrong numbers. Only claiming detects it. So the invariant is tested where it is
\ decided, on the production word, with the arguments crossing through package cells
\ because a quotation cannot read the caller's locals.
: TX-CLAIM! ( n n -- ) {: id:n count:n :}
   id TX-CID !  count TX-CCNT ! ;

: TX-CLAIM ( -- )
   TX-CID @ TX-CCNT @ CLAIM ;

: T-CLAIM ( -- )
   s" a census id may be claimed once" T-LABEL
   TX-CNT CLAIM-CLEAR
   3 TX-CNT TX-CLAIM!  [: TX-CLAIM ;] 0 TTHROWSQ
   s" claiming it a second time is refused" T-LABEL
   3 TX-CNT TX-CLAIM!  [: TX-CLAIM ;] E-GX-ALIAS TTHROWSQ
   s" a different id is still free" T-LABEL
   4 TX-CNT TX-CLAIM!  [: TX-CLAIM ;] 0 TTHROWSQ
   s" clearing the set frees every id again" T-LABEL
   TX-CNT CLAIM-CLEAR
   3 TX-CNT TX-CLAIM!  [: TX-CLAIM ;] 0 TTHROWSQ
   s" an id outside the census is refused before it indexes anything" T-LABEL
   TX-CNT TX-CNT TX-CLAIM!  [: TX-CLAIM ;] E-GX-SLOT TTHROWSQ
   -1 TX-CNT TX-CLAIM!  [: TX-CLAIM ;] E-GX-SLOT TTHROWSQ
   TX-CNT CLAIM-CLEAR ;

\ ---- the extent arithmetic, pinned at its exact boundaries ---------------------
\ These two overflow tests are what let a commit leaf allocate without checking
\ anything, so they are pinned where they actually decide: one byte either side of
\ the cell maximum. A quotation cannot read the caller's locals, so V-ARITH's two
\ arguments cross through package cells. The accumulator is set explicitly per case
\ rather than reached through a fixture, because no census can produce extents this
\ large - the boundary is unreachable from a file but entirely reachable in
\ arithmetic, which is exactly why it is tested here and not through PREPARE.
: TX-ARITH! ( n n -- ) {: off:n len:n :}
   off TX-AO !  len TX-AL ! ;

: TX-ARITH ( -- )
   TX-AO @ TX-AL @ V-ARITH ;

: T-ARITH ( -- )
   s" a row end exactly at the cell maximum is accepted" T-LABEL
   0 SUM-N !  MAX-N 16 - 16 TX-ARITH!  [: TX-ARITH ;] 0 TTHROWSQ
   s" a row end one byte past it is refused" T-LABEL
   0 SUM-N !  MAX-N 16 - 1 + 16 TX-ARITH!  [: TX-ARITH ;] E-GX-EXTENT TTHROWSQ
   s" a prefix sum exactly at the cell maximum is accepted" T-LABEL
   MAX-N 16 - SUM-N !  0 16 TX-ARITH!  [: TX-ARITH ;] 0 TTHROWSQ
   SUM-N @ MAX-N T=
   s" a prefix sum one byte past it is refused" T-LABEL
   MAX-N 16 - 1 + SUM-N !  0 16 TX-ARITH!  [: TX-ARITH ;] E-GX-EXTENT TTHROWSQ
   s" a zero extent and a negative offset are refused" T-LABEL
   0 SUM-N !  0 0 TX-ARITH!  [: TX-ARITH ;] E-GX-EXTENT TTHROWSQ
   0 SUM-N !  -1 16 TX-ARITH!  [: TX-ARITH ;] E-GX-EXTENT TTHROWSQ
   0 SUM-N ! ;

\ ---- the block grows with the count, and the growth is bounded -----------------
\ The block is now sized by a multiplication, so the boundaries of that arithmetic are
\ pinned here rather than left to the census that happens to arrive. The cap case is
\ the one that matters for memory: at ROW-CAP the block is the widest this package can
\ ever ask for, and it is still an ordinary cell count.
: TX-CELLS-AT ( n -- n )  BLOCK-CELLS ;

: T-BLOCK-CELLS ( -- )
   s" the block carries the nine fixed cells plus three per row" T-LABEL
   1 TX-CELLS-AT P-ROWS P-ROW-CELLS + T=
   TX-CNT TX-CELLS-AT P-ROWS TX-CNT P-ROW-CELLS * + T=
   s" the real checkpoint's 160 rows make a 489-cell block" T-LABEL
   160 TX-CELLS-AT 489 T=
   s" a count at the census cap is accepted and is still a cell count" T-LABEL
   ROW-CAP TX-CELLS-AT P-ROWS ROW-CAP P-ROW-CELLS * + T=
   ROW-CAP TX-CELLS-AT MAX-N < TTRUE
   s" a count of zero, a negative count, and one past the cap are refused" T-LABEL
   [: 0 TX-CELLS-AT drop ;]          E-GX-COUNT TTHROWSQ
   [: -1 TX-CELLS-AT drop ;]         E-GX-COUNT TTHROWSQ
   [: ROW-CAP 1 + TX-CELLS-AT drop ;] E-GX-COUNT TTHROWSQ ;

\ ---- ABORT gives everything back -----------------------------------------------
\ The counters are the proof: the census owner, its kernel mapping, and the WSTORE
\ table block all return to zero through ABORT alone.
: T-ABORT ( -- )
   s" ABORT disposes the census, its mapping, and the table" T-LABEL
   TX-CLEAN!  TX-LAY
   TX-PATH SAFET:LOAD
   TX-CFG-A PREPARE
   MATCH GPT2TX:prep-result
      prepared OF
         TX-HELD                                \ the prep holds the mapping and the table
         PREP-NL TX-NL T=                       \ and the depth a commit will mint from
         ABORT
      ENDOF
      rejected OF
         s" ABORT leg could not reach a prepared value" T-LABEL
         . cr SAFET:RELEASE
         0 0= 0= TTRUE
      ENDOF
   ;MATCH
   TX-NO-LEAK ;

\ ---- row-granularity snapshot of a prep's carried plan -------------------------
\ The aggregates (count and sum) were always enough to catch a commit that sized its
\ arena from another transaction's numbers. They are NOT enough to catch a commit that
\ reads another transaction's ROWS: two censuses of one geometry agree on both
\ aggregates and disagree on every row. So the whole carried plan is copied aside
\ before the interfering PREPARE and compared cell for cell afterwards.
3 constant TX-ROW-CELLS                         \ (mapping offset, extent, census id)
create TX-ROWSNAP TX-CNT TX-ROW-CELLS * cells allot
variable TX-SNAP-N

: TX-SNAP-CELL ( n n -- ptr n ) {: row:n col:n :}
   TX-ROWSNAP row TX-ROW-CELLS * col + cells + ;

: TX-ROWS-SNAP ( GPT2TX:prep -- GPT2TX:prep )   \ copy every carried row aside
   PREP-COUNT {: count:n :}
   count TX-SNAP-N !
   count 0 ?do
      i PREP-ROW {: off:n len:n id:n :}
      off  i 0 TX-SNAP-CELL !
      len  i 1 TX-SNAP-CELL !
      id   i 2 TX-SNAP-CELL !
   loop ;

: TX-ROWS-SAME ( GPT2TX:prep -- GPT2TX:prep )   \ every carried row unmoved, in order
   PREP-COUNT TX-SNAP-N @ T=
   TX-SNAP-N @ {: count:n :}
   count 0 ?do
      i PREP-ROW {: off:n len:n id:n :}
      off  i 0 TX-SNAP-CELL @ T=
      len  i 1 TX-SNAP-CELL @ T=
      id   i 2 TX-SNAP-CELL @ T=
   loop ;

\ How many of a prep's carried rows differ from the snapshot. The interfering
\ transaction is measured with this before its own prep is thrown away: if its rows did
\ NOT differ, the fixture would be interfering with nothing and the stability assertion
\ above would pass for the wrong reason.
variable TX-DIFFN

variable TX-SUM0                                \ the live prep's own prefix sum, before any
                                                \ interfering emit rewrites TX-DOFF

: TX-ROWS-DIFF-COUNT ( GPT2TX:prep -- GPT2TX:prep )
   0 TX-DIFFN !
   PREP-COUNT {: count:n :}
   count 0 ?do
      i PREP-ROW {: off:n len:n id:n :}
      off  i 0 TX-SNAP-CELL @ <>
      len  i 1 TX-SNAP-CELL @ <>  or
      id   i 2 TX-SNAP-CELL @ <>  or
      if TX-DIFFN @ 1 + TX-DIFFN ! then
   loop ;

\ Runs a WHOLE second transaction that succeeds, over the same model with every mapping
\ offset shifted. Its aggregates match the first's exactly - same tensor count, same
\ extents, same prefix sum - so only the rows can tell the two apart, which is what
\ makes it the right interference for a row-carrying block.
\ The interference that moves EXTENTS. Its rows disagree with the live prep's in the two
\ columns the allocated arm actually reads, so a commit taking its plan from the scratch
\ would size and place every span wrongly - which the offset-only variant cannot show.
: TX-INTERFERE-WIDE ( -- )
   TX-LAY-WIDE
   TX-PATH2 SAFET:LOAD TX-CFG-WIDE PREPARE
   MATCH GPT2TX:prep-result
      prepared OF
         s" the wider transaction agrees on the tensor count" T-LABEL
         PREP-COUNT TX-CNT T=
         s" and disagrees on the prefix sum and on every row" T-LABEL
         PREP-SUM TX-SUM0 @ <> TTRUE
         TX-ROWS-DIFF-COUNT
         TX-DIFFN @ TX-CNT T=
         ABORT
      ENDOF
      rejected OF
         s" the wider census failed to prepare" T-LABEL
         . cr SAFET:RELEASE
         0 0= 0= TTRUE
      ENDOF
   ;MATCH ;

: TX-INTERFERE-SHIFTED ( -- )
   TX-LAY-SHIFTED
   TX-PATH2 SAFET:LOAD TX-CFG-A PREPARE
   MATCH GPT2TX:prep-result
      prepared OF
         s" the interfering transaction agrees on both aggregates" T-LABEL
         PREP-COUNT TX-CNT T=
         PREP-SUM TX-DOFF @ T=
         s" and disagrees on every single row" T-LABEL
         TX-ROWS-DIFF-COUNT
         TX-DIFFN @ TX-CNT T=
         ABORT
      ENDOF
      rejected OF
         s" the offset-shifted census failed to prepare" T-LABEL
         . cr SAFET:RELEASE
         0 0= 0= TTRUE
      ENDOF
   ;MATCH ;

\ ---- the prep carries its own plan --------------------------------------------
\ The scenario a commit leaf has to survive: it is handed a prep, and between the
\ PREPARE that built it and the commit that consumes it, another transaction runs
\ and REFUSES. That second call rewrites this package's scratch - PLAN-COUNT and
\ PLAN-SUM now describe it, not the live prep. A commit reading those statics would
\ size its arena from another transaction's numbers. So the prep's own copy is read
\ here and asserted to be unmoved, with the scratch proved to have moved underneath
\ it in the same breath.
: T-PREP-OWNS-PLAN ( -- )
   s" a prep carries its own plan, not this package's scratch" T-LABEL
   TX-CLEAN!  TX-LAY
   TX-PATH SAFET:LOAD TX-CFG-A PREPARE
   MATCH GPT2TX:prep-result
      prepared OF
         PREP-COUNT TX-CNT T=
         PREP-SUM TX-DOFF @ T=
         TX-DOFF @ TX-SUM0 !
         TX-ROWS-SNAP
         TX-PATH SAFET:LOAD TX-CFG-DEEP PREPARE
         E-GX-COUNT TX-EXPECT-REJECTED SAFET:RELEASE
         s" the refused call did move the scratch" T-LABEL
         PLAN-COUNT TX-CNT <> TTRUE
         s" and the live prep still reports its own plan" T-LABEL
         PREP-COUNT TX-CNT T=
         PREP-SUM TX-DOFF @ T=
         s" every one of its carried rows is byte-identical" T-LABEL
         TX-ROWS-SAME
         TX-INTERFERE-SHIFTED
         s" and they are still byte-identical after a SUCCEEDING transaction" T-LABEL
         PREP-COUNT TX-CNT T=
         PREP-SUM TX-DOFF @ T=
         TX-ROWS-SAME
         TX-INTERFERE-WIDE
         s" and after one whose EXTENTS differ, which is what this arm reads" T-LABEL
         PREP-COUNT TX-CNT T=
         PREP-SUM TX-SUM0 @ T=
         TX-ROWS-SAME
         ABORT
      ENDOF
      rejected OF
         s" prep-owned-plan leg could not prepare" T-LABEL
         . cr SAFET:RELEASE
         0 0= 0= TTRUE
      ENDOF
   ;MATCH
   TX-NO-LEAK ;

\ ---- the prep-to-census exit ----------------------------------------------------
\ RELINQUISH claims to convert a prep back into the census it was built from, TOTALLY,
\ and that is three claims at once: the prep block is gone, the sealed table is gone,
\ and the census is not merely alive but UNCHANGED. The counters answer the first two,
\ and they are the half a leaky implementation fails - a RELINQUISH that forgot the
\ table leaves the WSTORE counter high and nothing else moves.
\
\ The third claim needs the census to be SPENT, because "still an owner" and "still
\ usable" are different properties and only the counters see the first. So the census
\ that comes back is handed straight to another PREPARE, and the plan that transaction
\ builds is compared with the plan the FIRST prep carried - count, prefix sum, and every
\ carried row, cell for cell. A census that came back subtly damaged - an image quietly
\ detached, a reader half-consumed, the wrong census entirely - passes every counter
\ assertion above and fails right here.
: T-RELINQUISH ( -- )
   s" RELINQUISH hands the census back and disposes everything else" T-LABEL
   TX-CLEAN!  TX-LAY
   TX-PATH SAFET:LOAD TX-CFG-A PREPARE
   MATCH GPT2TX:prep-result
      prepared OF
         TX-HELD                                  \ census, mapping, table and block
         TX-ROWS-SNAP                             \ the plan this prep was built on
         RELINQUISH                               \ ( census )
         s" the prep block and the sealed table are gone, and the census is not" T-LABEL
         TX-CENSUS-ONLY
         s" the census it answers with still reports its own tensors and image" T-LABEL
         SAFET:COUNT TX-CNT T=
         SAFET:MAP-LEN 0 > TTRUE
         s" and it binds again, on exactly the plan the first prep carried" T-LABEL
         TX-CFG-A PREPARE
         MATCH GPT2TX:prep-result
            prepared OF
               PREP-COUNT TX-CNT T=
               PREP-SUM TX-DOFF @ T=
               TX-ROWS-SAME
               ABORT
            ENDOF
            rejected OF
               s" the relinquished census would not prepare again, code" T-LABEL
               . cr SAFET:RELEASE
               0 0= 0= TTRUE
            ENDOF
         ;MATCH
      ENDOF
      rejected OF
         s" relinquish leg could not prepare" T-LABEL
         . cr SAFET:RELEASE
         0 0= 0= TTRUE
      ENDOF
   ;MATCH
   TX-NO-LEAK ;

\ ---- the identity twin: same census, different captured cfgkey -----------------
: TX-STASH-KEY ( GPT2TX:prep -- GPT2TX:prep )
   PREP-KEY MDLCFG-CFGKEY:UNMAKE {: k0:n k1:n k2:n k3:n :}
   k0 TX-KA0 !  k1 TX-KA1 !  k2 TX-KA2 !  k3 TX-KA3 ! ;

: TX-KEY-DIFFERS? ( GPT2TX:prep -- GPT2TX:prep bool )
   PREP-KEY MDLCFG-CFGKEY:UNMAKE {: k0:n k1:n k2:n k3:n :}
   k0 TX-KA0 @ <>  k1 TX-KA1 @ <>  or
   k2 TX-KA2 @ <>  or  k3 TX-KA3 @ <>  or ;

: TX-PREP-KEY-OF ( MDLCFG:mcfg -- )             \ prepare, stash the key, abort
   TX-PATH SAFET:LOAD swap PREPARE
   MATCH GPT2TX:prep-result
      prepared OF TX-STASH-KEY ABORT ENDOF
      rejected OF
         s" twin leg could not prepare" T-LABEL
         . cr SAFET:RELEASE
         0 0= 0= TTRUE
      ENDOF
   ;MATCH ;

: T-TWIN ( -- )
   s" two configurations of one geometry both bind the same census" T-LABEL
   TX-CLEAN!  TX-LAY
   TX-CFG-A TX-PREP-KEY-OF                      \ stashes A's captured key
   TX-NO-LEAK
   s" and the captured key is that configuration's own, cell for cell" T-LABEL
   TX-CFG-A TX-CFG-KEY!
   TX-KEY-IS-CFG
   s" and the prep captures a DIFFERENT identity for each" T-LABEL
   TX-PATH SAFET:LOAD TX-CFG-B PREPARE
   MATCH GPT2TX:prep-result
      prepared OF TX-KEY-DIFFERS? TTRUE ABORT ENDOF
      rejected OF
         s" twin B could not prepare" T-LABEL
         . cr SAFET:RELEASE
         0 0= 0= TTRUE
      ENDOF
   ;MATCH
   TX-NO-LEAK ;

\ ---- the foreign-model compare, on the production word --------------------------
\ The commit half's whole refusal rests on this one comparison, so it is tested on
\ the production word rather than on a re-derivation of it. It lives with the PREPARE
\ suite because PREP-FOREIGN? asks its question of a PREP, and the captured key it
\ reads is the one the twin leg above pins. The prep is always built from
\ configuration A; what varies is the configuration that would CONSUME it. A compare
\ that was deleted, or that compared a configuration with itself, would still pass
\ the first assertion and fail the second.
: TX-FOREIGN? ( MDLCFG:mcfg -- bool )           \ the mcfg is the CONSUMING one
   TX-PATH SAFET:LOAD TX-CFG-A PREPARE          \ the prep is configuration A's
   MATCH GPT2TX:prep-result
      prepared OF                               \ ( mcfg prep )
         swap PREP-FOREIGN?                     \ ( prep mcfg bool )
         >r drop ABORT r>
      ENDOF
      rejected OF                               \ ( mcfg census code )
         s" foreign-compare leg could not prepare" T-LABEL
         . cr SAFET:RELEASE drop
         0 0= 0= TTRUE
         0 0=                                   \ keep the row shape; the leg failed
      ENDOF
   ;MATCH ;

: T-FOREIGN-COMPARE ( -- )
   s" a prep is not foreign to the configuration that built it" T-LABEL
   TX-CLEAN!  TX-LAY
   TX-CFG-A TX-FOREIGN? 0= TTRUE
   TX-NO-LEAK
   s" and the identity twin IS foreign, though it binds the same census" T-LABEL
   TX-CFG-B TX-FOREIGN? TTRUE
   TX-NO-LEAK ;

\ ---- static half: the checker enforces the PREPARE phase's ownership rules -----
\ The compare-and-commit half's static rules are the same shape and live with the
\ suite that owns them: gpt2-check-test.f for the mapped arm and the model,
\ gpt2-alloc-test.f for the allocated witness.
: T-CHECKER ( -- )
   s" a prep cannot be forged from a raw cell or a pointer" T-LABEL
   s" GX-BAD-FORGE-N ( n -- GPT2TX:prep ) " TX-REJECTED
   s" GX-BAD-FORGE-P ( ptr u8 -- GPT2TX:prep ) " TX-REJECTED
   s" a prep is linear: no copy, no discard, no store" T-LABEL
   s" GX-BAD-DUP ( GPT2TX:prep -- GPT2TX:prep GPT2TX:prep ) dup" TX-REJECTED
   s" GX-BAD-DROP ( GPT2TX:prep -- ) drop" TX-REJECTED
   s" GX-BAD-STORE ( GPT2TX:prep ptr n -- ) !" TX-REJECTED
   s" ABORT consumes its prep exactly once" T-LABEL
   s" GX-BAD-ABORT-TWICE ( GPT2TX:prep -- ) GPT2TX:ABORT GPT2TX:ABORT" TX-REJECTED
   s" GX-BAD-ABORT-KEEPS ( GPT2TX:prep -- GPT2TX:prep ) GPT2TX:ABORT" TX-REJECTED
   \ The prep's two total exits are linear in the same way and differ only in what
   \ they answer with, so the same shape of negative applies to both: spent twice,
   \ spent and kept, spent and discarded, or spent after the other exit already ran.
   s" RELINQUISH consumes its prep exactly once and answers with the census" T-LABEL
   s" GX-OK-RELINQUISH ( GPT2TX:prep -- SAFET:census ) GPT2TX:RELINQUISH" TX-ACCEPTED
   s" GX-BAD-RELINQ-TWICE ( GPT2TX:prep -- SAFET:census ) GPT2TX:RELINQUISH GPT2TX:RELINQUISH" TX-REJECTED
   s" GX-BAD-RELINQ-AFTER-ABORT ( GPT2TX:prep -- SAFET:census ) GPT2TX:ABORT GPT2TX:RELINQUISH" TX-REJECTED
   s" GX-BAD-ABORT-AFTER-RELINQ ( GPT2TX:prep -- SAFET:census ) GPT2TX:RELINQUISH GPT2TX:ABORT" TX-REJECTED
   s" GX-BAD-RELINQ-KEEPS ( GPT2TX:prep -- GPT2TX:prep SAFET:census ) GPT2TX:RELINQUISH" TX-REJECTED
   s" GX-BAD-RELINQ-DROPPED ( GPT2TX:prep -- ) GPT2TX:RELINQUISH" TX-REJECTED
   s" GX-BAD-RELINQ-AMBIENT ( -- SAFET:census ) GPT2TX:RELINQUISH" TX-REJECTED
   s" the exit belongs to a prep, and answers with a census and nothing else" T-LABEL
   s" GX-BAD-RELINQ-CHECKED ( GPT2TX:checked-prep -- SAFET:census ) GPT2TX:RELINQUISH" TX-REJECTED
   s" GX-BAD-RELINQ-ALLOC ( GPT2TX:checked-prep-alloc -- SAFET:census ) GPT2TX:RELINQUISH" TX-REJECTED
   s" GX-BAD-RELINQ-CENSUS ( SAFET:census -- SAFET:census ) GPT2TX:RELINQUISH" TX-REJECTED
   s" GX-BAD-RELINQ-MAPPING ( GPT2TX:prep -- SAFET:mapping ) GPT2TX:RELINQUISH" TX-REJECTED
   s" prep-result payloads cannot cross roles" T-LABEL
   s" GX-BAD-PREPARED-CENSUS ( SAFET:census -- GPT2TX:prep-result ) GPT2TX-PREP--RESULT:PREPARED" TX-REJECTED
   s" GX-BAD-REJECTED-PREP ( GPT2TX:prep n -- GPT2TX:prep-result ) GPT2TX-PREP--RESULT:REJECTED" TX-REJECTED
   s" GX-BAD-ABORT-CENSUS ( SAFET:census -- ) GPT2TX:ABORT" TX-REJECTED
   s" GX-BAD-RESULT-DROPPED ( SAFET:census MDLCFG:mcfg -- ) GPT2TX:PREPARE" TX-REJECTED
   s" PREPARE answers about the census it is given, never ambient state" T-LABEL
   s" GX-BAD-AMBIENT ( MDLCFG:mcfg -- GPT2TX:prep-result ) GPT2TX:PREPARE" TX-REJECTED
   s" the public surface resolves (controls)" T-LABEL
   s" GX-OK-PREPARE ( SAFET:census MDLCFG:mcfg -- GPT2TX:prep-result ) GPT2TX:PREPARE" TX-ACCEPTED
   s" GX-OK-ABORT ( GPT2TX:prep -- ) GPT2TX:ABORT" TX-ACCEPTED
   s" GX-OK-LIVE ( -- n ) GPT2TX:LIVE" TX-ACCEPTED
   \ The erasures are the whole soundness cost of this module, so their confinement
   \ is asserted, not assumed. A candidate names them qualified, the way a foreign
   \ file would have to; each is unresolvable, which is what package-private means
   \ here. This is the probe half of the opacity claim in gpt2-bind.f's header - the
   \ other half is refine-lint confining the two inverse mints to that file, because
   \ these probes cannot speak for a file that REOPENS the package.
   s" the PREPARE phase's audited erasures stay package-private" T-LABEL
   s" GX-BAD-MINT-PREP ( ptr u8 -- GPT2TX:prep ) GPT2TX:MINT-PREP" UNRESOLVED
   s" GX-BAD-PREP-BLOCK ( GPT2TX:prep -- GPT2TX:prep ptr n ) GPT2TX:PREP>BLOCK" UNRESOLVED
   s" GX-BAD-TAKE-PREP ( GPT2TX:prep -- ptr n ) GPT2TX:TAKE-PREP" UNRESOLVED
   s" GX-BAD-BLK-BYTES ( ptr n -- ptr u8 ) GPT2TX:BLK>BYTES" UNRESOLVED
   s" GX-BAD-CENSUS-N ( SAFET:census -- n ) GPT2TX:CENSUS>N" UNRESOLVED
   s" GX-BAD-N-CENSUS ( n -- SAFET:census ) GPT2TX:N>CENSUS" UNRESOLVED
   s" GX-BAD-TABLE-N ( WSTORE:table -- n ) GPT2TX:TABLE>N" UNRESOLVED
   s" GX-BAD-N-TABLE ( n -- WSTORE:table ) GPT2TX:N>TABLE" UNRESOLVED ;

: RUN-PREPARE ( -- )
   T-RESET
   TX-BASELINE!
   T-CHECKER
   T-CLAIM
   T-ARITH
   T-BLOCK-CELLS
   T-PREPARE-OK       TX-NO-LEAK
   T-REJECT-FAMILY    TX-NO-LEAK
   T-REJECT-CFG       TX-NO-LEAK
   T-REJECT-CENSUS    TX-NO-LEAK
   T-REJECT-IMAGELESS TX-NO-LEAK
   T-ABORT            TX-NO-LEAK
   T-PREP-OWNS-PLAN   TX-NO-LEAK
   T-RELINQUISH       TX-NO-LEAK
   T-TWIN             TX-NO-LEAK
   T-FOREIGN-COMPARE  TX-NO-LEAK
   TX-CLEANUP ;

RUN-PREPARE
T-REPORT

;package
