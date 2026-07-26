\ gpt2-bind-test.f - GPT2TX bind-transaction PREPARE acceptance (rev-4 S6b1).
\
\ WHY THIS SUITE REOPENS package GPT2TX. What the leaf contract asks the fixtures
\ to prove is which rows the transaction validated, and no public word hands a row,
\ a census, or a table out of a prep - that opacity is the design, so it cannot
\ also be the thing the fixtures read through. The suite therefore runs INSIDE the
\ package (the lib/process-pty-handle-test.f arrangement) and reads the validated
\ rows through the package-private projections. Every word defined here is prefixed
\ TX- or T- so nothing shadows the module's own vocabulary.
\
\ THE FIXTURES ARE GENERATED THROUGH THE PRODUCTION VOCABULARY. A tiny geometry
\ (nlayer 2, nembd 4, nhead 2, nctx 8, nvocab 5 - thirty tensors) is emitted as a
\ real synthetic safetensors file and loaded back through the real mmap path, the
\ safetensors-test SYNTH-PATH pattern. The header rows are not hand-written JSON:
\ each tensor's name comes from GPT2BIND:COPY-KEY? and its shape from
\ GPT2BIND:TID-SHAPE, so a fixture that drifts from the role vocabulary cannot be
\ written. Byte extents and data offsets are computed from the EMITTED dims, so
\ every corrupted variant is still a file the safetensors parser itself accepts -
\ which is what makes each refusal PREPARE's verdict and not the parser's.
\
\ THE CORRUPTION KNOBS. One builder emits every variant: TX-BAD-SLOT names the slot
\ to damage and TX-BAD-KIND how - wrong dtype, an extra rank, one wrong dim, a
\ misspelled key, or one tensor beyond the census.
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
\ ON THE IDENTITY TWIN, AND WHAT PREPARE CAN AND CANNOT REFUSE. The leaf contract
\ asks for a "foreign configuration" reject leg. PREPARE cannot produce one, and
\ the reason is structural rather than an omission: PREPARE mints every layer
\ identity from the very configuration it is validating against (GPT2BIND:LAYER is
\ the sole layerid constructor), so the identity assertion inside TID-SLOT always
\ compares a configuration with itself and E-GB-FOREIGN is unreachable from this
\ entry point. The honest test of what PREPARE does own is the twin-cfgkey leg: two
\ configurations of the SAME geometry differing in one behavioral field that no
\ tensor reflects (tied embeddings) both bind the same census - correctly, because
\ nothing in a tensor census can tell them apart - and the two preps carry
\ DIFFERENT captured cfgkeys. That captured key is what lets a commit refuse a
\ model built against the other configuration, so this leg pins the capture instead
\ of pretending PREPARE can reject on it.

require lib/prelude.f
require lib/adt/option.f
require lib/adt/result.f
require lib/string.f
require lib/fs.f
require lib/test.f
require test/checker-assert.f
require maki/infer/gpt2-bind.f

package GPT2TX

\ ---- the tiny fixture geometry -------------------------------------------------
2 constant TX-NL                                \ nlayer
4 constant TX-NE                                \ nembd
2 constant TX-NH                                \ nhead
8 constant TX-NC                                \ nctx
5 constant TX-NV                                \ nvocab
30 constant TX-CNT                              \ 4 + 13*nlayer

4 constant TX-F32-SZ                            \ F32 element size
2 constant TX-BF16-SZ                           \ the wrong-dtype variant's element size

\ ---- corruption knobs ----------------------------------------------------------
0 constant TX-BK-NONE
1 constant TX-BK-DTYPE                          \ emit BF16 where the role wants F32
2 constant TX-BK-RANK                           \ emit one extra trailing 1 dim
3 constant TX-BK-SHAPE                          \ emit d0+1
4 constant TX-BK-KEY                            \ misspell the key
5 constant TX-BK-EXTRA                          \ emit one tensor beyond the census
6 constant TX-BK-ALIAS                          \ emit the PREVIOUS slot's key on this row

variable TX-BAD-SLOT                            \ which slot to damage, -1 for none
variable TX-BAD-KIND

\ ---- buffers -------------------------------------------------------------------
$8000 constant TX-IMG-CAP
$4000 constant TX-JCAP
64 constant TX-KCAP

create TX-IMG TX-IMG-CAP allot
create TX-JBUF TX-JCAP allot
create TX-KBUF TX-KCAP allot

variable TX-JLEN                                \ bytes of header JSON emitted
variable TX-DOFF                                \ running data-section offset
variable TX-IMGLEN
variable TX-WANT-OFF                            \ the probed row read off the census
variable TX-WANT-LEN
variable TX-KA0  variable TX-KA1
variable TX-KA2  variable TX-KA3                \ one twin's captured cfgkey cells
variable TX-KB0  variable TX-KB1
variable TX-KB2  variable TX-KB3                \ that configuration's own cfgkey cells
variable TX-BASE-MAP                            \ leak-counter baselines at suite entry
variable TX-BASE-OWN
variable TX-BASE-WS
variable TX-BASE-PREP
variable TX-AO  variable TX-AL                  \ V-ARITH boundary-leg arguments
variable TX-CID  variable TX-CCNT               \ CLAIM boundary-leg arguments

34 constant TX-DQ
44 constant TX-COMMA
58 constant TX-COLON
91 constant TX-LBRACK
93 constant TX-RBRACK
120 constant TX-LOWER-X
123 constant TX-LBRACE
125 constant TX-RBRACE
48 constant TX-ZERO-CH

\ ---- JSON emission -------------------------------------------------------------
: TX-J+ ( ptr u8 n -- )
   dup >r
   TX-JBUF TX-JLEN @ + swap BYTE-COPY
   TX-JLEN @ r> + TX-JLEN ! ;

: TX-J+C ( n -- )
   TX-JBUF TX-JLEN @ + c!
   TX-JLEN @ 1 + TX-JLEN ! ;

: TX-J+U ( n -- )                               \ nonnegative decimal
   dup 10 < if TX-ZERO-CH + TX-J+C exit then
   dup 10 / RECURSE
   10 mod TX-ZERO-CH + TX-J+C ;

: TX-J+STR ( ptr u8 n -- )                      \ "text"
   TX-DQ TX-J+C  TX-J+  TX-DQ TX-J+C ;

: TX-J+MEM ( ptr u8 n -- )                      \ "text":
   TX-J+STR TX-COLON TX-J+C ;

\ ---- the role's own key and shape, straight from GPT2BIND ----------------------
: TX-KEY-LEN ( MDLCFG:mcfg n -- MDLCFG:mcfg n )   \ render the slot's HF key
   SLOT>TID TX-KBUF TX-KCAP GPT2BIND:COPY-KEY? E-GX-RENDER NEED ;

: TX-SHAPE ( MDLCFG:mcfg n -- MDLCFG:mcfg n n n n n )
   SLOT>TID GPT2BIND:TID-SHAPE ;

\ ---- emitted dims: the role's shape with this slot's damage applied ------------
: TX-DAMAGED? ( n -- bool ) {: slot:n :}
   TX-BAD-SLOT @ slot = ;

: TX-KIND? ( n n -- bool ) {: slot:n kind:n :}
   slot TX-DAMAGED?  TX-BAD-KIND @ kind =  and ;

: TX-EMIT-RANK ( n n -- n ) {: slot:n rank:n :}
   slot TX-BK-RANK TX-KIND? if rank 1 + else rank then ;

: TX-EMIT-D0 ( n n -- n ) {: slot:n d0:n :}
   slot TX-BK-SHAPE TX-KIND? if d0 1 + else d0 then ;

: TX-ELEM-SZ ( n -- n ) {: slot:n :}
   slot TX-BK-DTYPE TX-KIND? if TX-BF16-SZ else TX-F32-SZ then ;

: TX-DTYPE$ ( n -- ptr u8 n ) {: slot:n :}
   slot TX-BK-DTYPE TX-KIND? if s" BF16" else s" F32" then ;

\ ---- one census row ------------------------------------------------------------
\ Answers the element count of what it emitted, so the byte extent always matches
\ the dims the parser will read back.
: TX-J+DIMS ( n n n n n -- n )
   {: rank:n d0:n d1:n d2:n d3:n :}
   TX-LBRACK TX-J+C
   d0 TX-J+U
   rank 1 > if TX-COMMA TX-J+C d1 TX-J+U then
   rank 2 > if TX-COMMA TX-J+C d2 TX-J+U then
   rank 3 > if TX-COMMA TX-J+C d3 TX-J+U then
   rank 4 > if TX-COMMA TX-J+C 1 TX-J+U then    \ the rank damage's extra trailing 1
   TX-RBRACK TX-J+C
   d0 d1 * d2 * d3 * ;

: TX-J+SPAN ( n -- )                            \ "data_offsets":[begin,end]
   {: nb:n :}
   s" data_offsets" TX-J+MEM
   TX-LBRACK TX-J+C
   TX-DOFF @ TX-J+U
   TX-COMMA TX-J+C
   TX-DOFF @ nb + TX-J+U
   TX-RBRACK TX-J+C
   TX-DOFF @ nb + TX-DOFF ! ;

: TX-J+BODY ( n n n n n n -- )                  \ slot rank d0 d1 d2 d3
   {: slot:n rank:n d0:n d1:n d2:n d3:n :}
   TX-LBRACE TX-J+C
   s" dtype" TX-J+MEM  slot TX-DTYPE$ TX-J+STR  TX-COMMA TX-J+C
   s" shape" TX-J+MEM
   slot rank TX-EMIT-RANK  slot d0 TX-EMIT-D0  d1 d2 d3 TX-J+DIMS
   slot TX-ELEM-SZ *                            \ ( byte extent of what was emitted )
   TX-COMMA TX-J+C
   TX-J+SPAN
   TX-RBRACE TX-J+C ;

\ The misspelled variant appends one byte to the rendered name, so the census holds
\ a key no role can ever ask for and the role's own key is absent.
: TX-J+KEY ( n n -- ) {: slot:n klen:n :}
   slot TX-BK-KEY TX-KIND? if
      TX-LOWER-X TX-KBUF klen + c!
      TX-KBUF klen 1 + TX-J+STR
   else
      TX-KBUF klen TX-J+STR
   then
   TX-COLON TX-J+C ;

\ Which slot's key this row actually carries. The alias variant hands a row the
\ PREVIOUS slot's name, so two roles resolve to one census tensor while every
\ per-row check still passes - the collision the claim set exists to catch.
: TX-KEY-SLOT ( n -- n ) {: slot:n :}
   slot TX-BK-ALIAS TX-KIND? if slot 1 - else slot then ;

: TX-J+ROW ( MDLCFG:mcfg n -- MDLCFG:mcfg ) {: slot:n :}
   slot 0 > if TX-COMMA TX-J+C then
   slot TX-KEY-SLOT TX-KEY-LEN {: klen:n :}
   slot klen TX-J+KEY
   slot TX-SHAPE {: rank:n d0:n d1:n d2:n d3:n :}
   slot rank d0 d1 d2 d3 TX-J+BODY ;

\ one tensor beyond the census: well-formed, but a name no role owns
: TX-J+EXTRA ( -- )
   TX-COMMA TX-J+C
   s" surplus.weight" TX-J+MEM
   TX-LBRACE TX-J+C
   s" dtype" TX-J+MEM  s" F32" TX-J+STR  TX-COMMA TX-J+C
   s" shape" TX-J+MEM  TX-LBRACK TX-J+C  1 TX-J+U  TX-RBRACK TX-J+C
   TX-COMMA TX-J+C
   TX-F32-SZ TX-J+SPAN
   TX-RBRACE TX-J+C ;

\ ---- image assembly ------------------------------------------------------------
: TX-HDR! ( n -- ) {: hl:n :}                   \ 8-byte little-endian header length
   8 0 ?do  hl i 8 * rshift $FF and  TX-IMG i + c!  loop ;

: TX-ZERO-DATA ( n n -- ) {: base:n nb:n :}     \ tensor bytes are all zero
   nb 0 ?do  0 TX-IMG base i + + c!  loop ;

: TX-ASSEMBLE ( -- )
   TX-JLEN @ {: hl:n :}
   hl TX-HDR!
   TX-JBUF TX-IMG 8 + hl BYTE-COPY
   8 hl + TX-DOFF @ TX-ZERO-DATA
   8 hl + TX-DOFF @ + TX-IMGLEN ! ;

: TX-BUILD ( MDLCFG:mcfg -- MDLCFG:mcfg )
   0 TX-JLEN !  0 TX-DOFF !
   TX-LBRACE TX-J+C
   TX-CNT 0 ?do  i TX-J+ROW  loop
   TX-BAD-KIND @ TX-BK-EXTRA = if TX-J+EXTRA then
   TX-RBRACE TX-J+C
   TX-ASSEMBLE ;

: TX-PATH ( -- ptr u8 n )  s" /tmp/hb-gpt2tx-synth.safetensors" ;

: TX-CLEANUP ( -- )  TX-PATH FS-PATHZ unlink drop ;

: TX-CLEAN! ( -- )
   -1 TX-BAD-SLOT !  TX-BK-NONE TX-BAD-KIND ! ;

: TX-DAMAGE! ( n n -- ) {: slot:n kind:n :}
   slot TX-BAD-SLOT !  kind TX-BAD-KIND ! ;

\ ---- fixture configurations (all through the sole MDLCFG constructor) ----------
: TX-DT ( -- MAKI:dtype )  MAKI-DTYPE:DF32 ;
: TX-EPS ( -- r )  0.00001 ;

: TX-CFG-A ( -- MDLCFG:mcfg )
   TX-EPS true MDLCFG-ARCH:GPT2
   1 TX-DT TX-NC TX-NV TX-NL TX-NE TX-NH true 4 4 MDLCFG:BUILD ;

\ the identity twin: SAME geometry, one census-invisible field flipped
: TX-CFG-B ( -- MDLCFG:mcfg )
   TX-EPS true MDLCFG-ARCH:GPT2
   1 TX-DT TX-NC TX-NV TX-NL TX-NE TX-NH false 4 4 MDLCFG:BUILD ;

\ one more layer: the census count no longer matches
: TX-CFG-DEEP ( -- MDLCFG:mcfg )
   TX-EPS true MDLCFG-ARCH:GPT2
   1 TX-DT TX-NC TX-NV TX-NL 1 + TX-NE TX-NH true 4 4 MDLCFG:BUILD ;

\ same census count, every embedding-shaped tensor twice as wide
: TX-CFG-WIDE ( -- MDLCFG:mcfg )
   TX-EPS true MDLCFG-ARCH:GPT2
   1 TX-DT TX-NC TX-NV TX-NL TX-NE 2 * TX-NH true 4 4 MDLCFG:BUILD ;

\ the real 124M geometry for the presence-gated leg
: TX-CFG-124M ( -- MDLCFG:mcfg )
   TX-EPS true MDLCFG-ARCH:GPT2
   1 TX-DT 1024 50257 12 768 12 true 50256 50256 MDLCFG:BUILD ;

: TX-WRITE ( MDLCFG:mcfg -- MDLCFG:mcfg )
   TX-BUILD
   TX-PATH TX-IMG TX-IMGLEN @ WRITE-ALL ;

: TX-LAY ( -- )                                 \ write the current variant to disk
   TX-CFG-A TX-WRITE drop ;

\ ---- assertion helpers ---------------------------------------------------------
: TX-MISSING ( -- )
   s" required option was NONE" T-LABEL
   0 0= 0= TTRUE ;

: TX-OPT-VAL ( option<n> -- n )
   MATCH option
      none OF TX-MISSING -1 ENDOF
      some OF ENDOF
   ;MATCH ;

\ ---- leak accounting, as a delta against this suite's own entry ----------------
\ The three counters are process-wide, and a combined run reaches this suite after
\ others that leave documented strands of their own (weight-store-test.f ends on 14
\ live WSTORE blocks and 2 SAFET owners by design). Asserting absolute zero would
\ therefore pass standalone and fail in maki/test.f while saying nothing about this
\ suite. Every assertion below is a delta from the entry baseline, so it measures
\ exactly what these fixtures took and gave back.
: TX-BASELINE! ( -- )
   SAFET-MAP:LIVE TX-BASE-MAP !
   SAFET:LIVE-OWNERS TX-BASE-OWN !
   WSTORE:LIVE TX-BASE-WS !
   LIVE TX-BASE-PREP ! ;

: TX-NO-LEAK ( -- )                             \ every owner and mapping given back
   SAFET-MAP:LIVE TX-BASE-MAP @ T=
   SAFET:LIVE-OWNERS TX-BASE-OWN @ T=
   WSTORE:LIVE TX-BASE-WS @ T=
   LIVE TX-BASE-PREP @ T= ;

: TX-HELD ( -- )                                \ exactly one prep's worth is live
   SAFET-MAP:LIVE TX-BASE-MAP @ 1 + T=
   SAFET:LIVE-OWNERS TX-BASE-OWN @ 1 + T=
   WSTORE:LIVE TX-BASE-WS @ 1 + T=
   LIVE TX-BASE-PREP @ 1 + T= ;

: TX-REJECTED ( ptr u8 n -- )
   CHECK-QUIET-CANDIDATE! 0 T= ;

: TX-ACCEPTED ( ptr u8 n -- )
   CHECK-QUIET-CANDIDATE! -1 T= ;

\ Verdict 1 is "the dictionary cannot resolve this token at all", which is how a
\ package-private word looks from outside; it is kept apart from verdict 0, a real
\ type error, so "private" is proved by non-resolution rather than by any accident.
: UNRESOLVED ( ptr u8 n -- )
   CHECK-QUIET-CANDIDATE! 1 T= ;

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
\ slot 7 is layer 0's c_attn Conv1D weight, the [in,out] matrix. Neither is a global
\ or a plain vector, so both are rows that would move first if the slot walk
\ drifted, and they sit either side of the global/block boundary arithmetic.
6 constant TX-PROBE-SLOT                        \ h.0.attn.bias (mask, rank 4)
7 constant TX-PROBE-CONV                        \ h.0.attn.c_attn.weight (Conv1D)

: TX-RECORD-PROBE ( SAFET:census n -- SAFET:census ) {: slot:n :}
   TX-CFG-A slot TX-KEY-LEN {: klen:n :}
   drop                                         \ the mcfg copy
   TX-KBUF klen SAFET:FIND TX-OPT-VAL {: id:n :}
   id SAFET:MAP-OFFSET? TX-OPT-VAL TX-WANT-OFF !
   id SAFET:NBYTES? TX-OPT-VAL TX-WANT-LEN ! ;

: TX-PROBE-MATCHES ( n -- ) {: slot:n :}        \ the validated row is the census's own
   slot PLAN-ROW {: off:n len:n :}
   off TX-WANT-OFF @ T=
   len TX-WANT-LEN @ T= ;

: T-PREPARE-OK ( -- )
   s" a matching census and configuration yield prepared" T-LABEL
   TX-CLEAN!  TX-LAY
   TX-PATH SAFET:LOAD
   TX-PROBE-SLOT TX-RECORD-PROBE
   TX-CFG-A PREPARE
   TX-EXPECT-PREPARED
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
   TX-EXPECT-PREPARED
   TX-PROBE-CONV TX-PROBE-MATCHES
   \ A second transaction must start from a cleared accumulator, not add to the
   \ first: the same census over the same configuration has to report the same sum.
   s" a second PREPARE reports the same sum, so the accumulator was reset" T-LABEL
   PLAN-SUM TX-DOFF @ T=
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
         TX-PATH SAFET:LOAD TX-CFG-DEEP PREPARE
         E-GX-COUNT TX-EXPECT-REJECTED SAFET:RELEASE
         s" the refused call did move the scratch" T-LABEL
         PLAN-COUNT TX-CNT <> TTRUE
         s" and the live prep still reports its own plan" T-LABEL
         PREP-COUNT TX-CNT T=
         PREP-SUM TX-DOFF @ T=
         ABORT
      ENDOF
      rejected OF
         s" prep-owned-plan leg could not prepare" T-LABEL
         . cr SAFET:RELEASE
         0 0= 0= TTRUE
      ENDOF
   ;MATCH
   TX-NO-LEAK ;

\ ---- the identity twin: same census, different captured cfgkey -----------------
: TX-STASH-KEY ( GPT2TX:prep -- GPT2TX:prep )
   PREP-KEY MDLCFG-CFGKEY:UNMAKE {: k0:n k1:n k2:n k3:n :}
   k0 TX-KA0 !  k1 TX-KA1 !  k2 TX-KA2 !  k3 TX-KA3 ! ;

: TX-CFG-KEY! ( MDLCFG:mcfg -- )                \ stash a configuration's OWN key cells
   MDLCFG:CFGKEY@ MDLCFG-CFGKEY:UNMAKE {: k0:n k1:n k2:n k3:n :}
   drop
   k0 TX-KB0 !  k1 TX-KB1 !  k2 TX-KB2 !  k3 TX-KB3 ! ;

\ Cell by cell, in order. A reversed or rotated capture would still differ from the
\ twin's and still pass a difference test, so difference alone proves nothing about
\ correctness; this is the assertion that pins WHICH key was captured.
: TX-KEY-IS-CFG ( -- )
   TX-KA0 @ TX-KB0 @ T=
   TX-KA1 @ TX-KB1 @ T=
   TX-KA2 @ TX-KB2 @ T=
   TX-KA3 @ TX-KB3 @ T= ;

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

\ ---- static half: the checker enforces the transaction's ownership rules -------
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
   s" the audited erasures stay package-private" T-LABEL
   s" GX-BAD-MINT-PREP ( ptr u8 -- GPT2TX:prep ) GPT2TX:MINT-PREP" UNRESOLVED
   s" GX-BAD-PREP-BLOCK ( GPT2TX:prep -- GPT2TX:prep ptr n ) GPT2TX:PREP>BLOCK" UNRESOLVED
   s" GX-BAD-TAKE-PREP ( GPT2TX:prep -- ptr n ) GPT2TX:TAKE-PREP" UNRESOLVED
   s" GX-BAD-BLK-BYTES ( ptr n -- ptr u8 ) GPT2TX:BLK>BYTES" UNRESOLVED
   s" GX-BAD-CENSUS-N ( SAFET:census -- n ) GPT2TX:CENSUS>N" UNRESOLVED
   s" GX-BAD-N-CENSUS ( n -- SAFET:census ) GPT2TX:N>CENSUS" UNRESOLVED
   s" GX-BAD-TABLE-N ( WSTORE:table -- n ) GPT2TX:TABLE>N" UNRESOLVED
   s" GX-BAD-N-TABLE ( n -- WSTORE:table ) GPT2TX:N>TABLE" UNRESOLVED ;

\ ---- presence-gated real artifact ----------------------------------------------
: TX-REAL-PATH ( -- ptr u8 n )  s" gpt2-model/model.safetensors" ;

: T-REAL ( -- )
   TX-REAL-PATH SAFET:PRESENT? 0= if
      s" gpt2-bind: gpt2-model/model.safetensors absent -> real-artifact leg SKIPPED" type cr
      0 0= TTRUE exit
   then
   s" the real gpt2 checkpoint prepares with its full census" T-LABEL
   TX-REAL-PATH SAFET:LOAD TX-CFG-124M PREPARE
   TX-EXPECT-PREPARED
   s" real gpt2 prepared rows=" type PLAN-COUNT dup . cr
   160 T=
   TX-NO-LEAK ;

: RUN ( -- )
   T-RESET
   TX-BASELINE!
   T-CHECKER
   T-CLAIM
   T-ARITH
   T-PREPARE-OK       TX-NO-LEAK
   T-REJECT-CFG       TX-NO-LEAK
   T-REJECT-CENSUS    TX-NO-LEAK
   T-ABORT            TX-NO-LEAK
   T-PREP-OWNS-PLAN   TX-NO-LEAK
   T-TWIN             TX-NO-LEAK
   T-REAL
   s" the whole suite released every owner it took" T-LABEL
   TX-NO-LEAK
   TX-CLEANUP
   T-REPORT ;

RUN

;package
