\ gpt2-bind-test.f - GPT2TX bind-transaction acceptance: the PREPARE half (rev-4
\ S6b1) and the compare-and-commit half (S6b3, redesign 2) - PREPARE, CHECK,
\ COMMIT-MAPPED, and the exits for all three of the transaction's owners.
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
7 constant TX-BK-PAD                            \ prepend __metadata__, shifting every offset

-5698 constant E-TX-FIX                          \ fixture invariant broke (never expected)

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
variable TX-WANT-ID                             \ and the census id that named it
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

\ Every data byte is a function of its POSITION in the data section, and never zero.
\ That is load-bearing for every byte comparison this suite makes: with a constant fill
\ - zeros, as this emitter used to write - a span probe compares zeros with zeros, so it
\ holds for a wrong arena offset, a wrong extent and a wrong census id alike, and the
\ assertion proves only that both sides are the same length. With a position-dependent
\ pattern, reading one byte from the wrong place is visible. Never zero, so a span that
\ was never written at all cannot pass either.
: TX-PAT ( n -- n ) {: i:n :}
   i 31 * 7 + 251 mod 1 + ;

: TX-PAT-DATA ( n n -- ) {: base:n nb:n :}
   nb 0 ?do  i TX-PAT  TX-IMG base i + +  c!  loop ;

\ A `__metadata__` member, which the loader recognises and SKIPS: it commits no row and
\ is not counted as a tensor. Emitting it therefore lengthens the header and nothing
\ else, and every tensor's mapping offset is 8 + header length + its data-section
\ begin - so this variant shifts EVERY row's offset while leaving the tensor count, the
\ dtypes, the shapes, the extents and the prefix sum exactly as they were. That is the
\ interference a commit has to survive, and the one an aggregates-only check cannot see.
: TX-J+PAD ( -- )
   s" __metadata__" TX-J+MEM
   TX-LBRACE TX-J+C
   s" pad" TX-J+MEM  s" shift-every-mapping-offset" TX-J+STR
   TX-RBRACE TX-J+C
   TX-COMMA TX-J+C ;

: TX-ASSEMBLE ( -- )
   TX-JLEN @ {: hl:n :}
   hl TX-HDR!
   TX-JBUF TX-IMG 8 + hl BYTE-COPY
   8 hl + TX-DOFF @ TX-PAT-DATA
   8 hl + TX-DOFF @ + TX-IMGLEN ! ;

: TX-BUILD ( MDLCFG:mcfg -- MDLCFG:mcfg )
   0 TX-JLEN !  0 TX-DOFF !
   TX-LBRACE TX-J+C
   TX-BAD-KIND @ TX-BK-PAD = if TX-J+PAD then
   TX-CNT 0 ?do  i TX-J+ROW  loop
   TX-BAD-KIND @ TX-BK-EXTRA = if TX-J+EXTRA then
   TX-RBRACE TX-J+C
   TX-ASSEMBLE ;

: TX-PATH ( -- ptr u8 n )  s" /tmp/hb-gpt2tx-synth.safetensors" ;

\ The offset-shifted variant lives at a SECOND path deliberately: the first transaction's
\ census holds a live mapping of TX-PATH, and rewriting those bytes underneath it would
\ change what the FIRST census reads - the one thing the interference fixture must not do.
: TX-PATH2 ( -- ptr u8 n )  s" /tmp/hb-gpt2tx-shift.safetensors" ;

: TX-CLEANUP ( -- )
   TX-PATH FS-PATHZ unlink drop
   TX-PATH2 FS-PATHZ unlink drop ;

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

: TX-LAY-WIDE ( -- )                            \ the wider model, at the second path
   TX-CFG-WIDE TX-BUILD
   TX-PATH2 TX-IMG TX-IMGLEN @ WRITE-ALL
   drop ;

: TX-LAY-SHIFTED ( -- )
   TX-BK-PAD TX-BAD-KIND !
   TX-CFG-A TX-BUILD
   TX-PATH2 TX-IMG TX-IMGLEN @ WRITE-ALL
   drop
   TX-BK-NONE TX-BAD-KIND ! ;

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

\ What a committed model owns: the prep block is gone, and the table block and the
\ checkpoint mapping have moved into the residency the model holds.
: TX-MODEL-HELD ( -- )
   SAFET-MAP:LIVE TX-BASE-MAP @ 1 + T=
   SAFET:LIVE-OWNERS TX-BASE-OWN @ 1 + T=
   WSTORE:LIVE TX-BASE-WS @ 1 + T=
   LIVE TX-BASE-PREP @ T= ;

: TX-BYTES= ( ptr u8 n ptr u8 n -- )
   STR= TTRUE ;

\ The ok payload of a release outcome. Reading it is the difference between "the exit
\ reported success" and "the exit gave back the bytes it was holding".
: TX-RES-VAL ( result<n,n> -- n )
   MATCH result
      ok  OF ENDOF
      err OF
         s" a release reported err, code" T-LABEL
         . cr
         0 0= 0= TTRUE
         -1
      ENDOF
   ;MATCH ;

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
   SAFET:DETACH-MAPPING                         \ ( census mapping ) - the image leaves
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

\ ---- the foreign-model compare, on the production word --------------------------
\ The commit half's whole refusal rests on this one comparison, so it is tested on
\ the production word rather than on a re-derivation of it. The prep is always built
\ from configuration A; what varies is the configuration that would CONSUME it. A
\ compare that was deleted, or that compared a configuration with itself, would
\ still pass the first assertion and fail the second.
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

\ ---- the second half: compare, then commit --------------------------------------
\ These three legs walk the transaction one state at a time and prove the same thing
\ about each: every state has an owner and a total exit, so no step in the sequence can
\ leave a resource with nobody to give it back.
\
\ WHY THIS IS THE HONEST FORM OF "NOTHING CAN STRAND". The contract asks for a throw
\ injected at each step boundary. The only step in the whole bind whose failure is
\ reachable is the record allocation inside SAFET:DETACH-MAPPING, and nothing in the
\ suite can force an out-of-memory there - the alternative would be a fault hook in
\ production code, which is not allowed to exist. So the property is pinned the way it
\ is actually decided: the failure is GUARDED inside CHECK, which answers with a value,
\ and the states either side of it are shown to be completely disposable -
\ prep -> ABORT, checked prep -> ABORT-CHECKED, model -> MODEL-DISPOSE - with every
\ counter returning to the suite's entry baseline. COMMIT-MAPPED itself has no guarded
\ step because it has no fallible one; what makes that true is the checked-prep type,
\ not a rule someone has to remember.
: TX-STASH-MKEY ( GPT2TX:gpt2-model -- GPT2TX:gpt2-model )
   MODEL-KEY MDLCFG-CFGKEY:UNMAKE {: k0:n k1:n k2:n k3:n :}
   k0 TX-KA0 !  k1 TX-KA1 !  k2 TX-KA2 !  k3 TX-KA3 ! ;

: T-CHECK-FOREIGN ( -- )
   s" a foreign configuration is refused, and nothing has moved" T-LABEL
   TX-CLEAN!  TX-LAY
   TX-PATH SAFET:LOAD TX-CFG-A PREPARE
   MATCH GPT2TX:prep-result
      prepared OF
         TX-CFG-B CHECK                          \ the twin: same census, other identity
         MATCH GPT2TX:check-result
            matched OF
               s" the identity twin was accepted as this model" T-LABEL
               0 0= 0= TTRUE
               ABORT-CHECKED
            ENDOF
            refused OF
               {: code:n :}
               code E-GX-FOREIGN T=
               s" and the refused prep is whole: still held, still ABORTable" T-LABEL
               TX-HELD
               ABORT
            ENDOF
         ;MATCH
      ENDOF
      rejected OF
         s" foreign leg could not prepare" T-LABEL
         . cr SAFET:RELEASE
         0 0= 0= TTRUE
      ENDOF
   ;MATCH
   TX-NO-LEAK ;

: T-CHECK-MATCH ( -- )
   s" the configuration that built the prep matches it" T-LABEL
   TX-CLEAN!  TX-LAY
   TX-PATH SAFET:LOAD TX-CFG-A PREPARE
   MATCH GPT2TX:prep-result
      prepared OF
         TX-CFG-A CHECK
         MATCH GPT2TX:check-result
            matched OF
               \ The census gave up its image and was released; the mapping it gave up
               \ is now the compared prep's. One owner out, one owner in, so the totals
               \ are exactly where PREPARE left them - and no kernel mapping moved.
               s" the mapping moved out of the census without changing what is owned" T-LABEL
               TX-HELD
               s" and a compared prep that declines to commit disposes totally" T-LABEL
               ABORT-CHECKED
            ENDOF
            refused OF
               {: code:n :}
               s" a prep was called foreign to its own configuration, code" T-LABEL
               code . cr
               0 0= 0= TTRUE
               ABORT
            ENDOF
         ;MATCH
      ENDOF
      rejected OF
         s" match leg could not prepare" T-LABEL
         . cr SAFET:RELEASE
         0 0= 0= TTRUE
      ENDOF
   ;MATCH
   TX-NO-LEAK ;

: T-COMMIT ( -- )
   s" a compared prep commits to a mapped model" T-LABEL
   TX-CLEAN!  TX-LAY
   TX-CFG-A TX-CFG-KEY!                          \ the configuration's own key, to compare
   TX-PATH SAFET:LOAD TX-CFG-A PREPARE
   MATCH GPT2TX:prep-result
      prepared OF
         TX-CFG-A CHECK
         MATCH GPT2TX:check-result
            matched OF
               COMMIT-MAPPED
               s" the prep block is gone and the residency is held" T-LABEL
               TX-MODEL-HELD
               s" the model carries the depth the prep validated" T-LABEL
               MODEL-NL TX-NL T=
               s" and the identity the prep captured, cell for cell" T-LABEL
               TX-STASH-MKEY
               TX-KEY-IS-CFG
               s" and the model's exit gives every owner back" T-LABEL
               MODEL-DISPOSE RES-CODE 0 T=
            ENDOF
            refused OF
               {: code:n :}
               s" commit leg was refused, code" T-LABEL
               code . cr
               0 0= 0= TTRUE
               ABORT
            ENDOF
         ;MATCH
      ENDOF
      rejected OF
         s" commit leg could not prepare" T-LABEL
         . cr SAFET:RELEASE
         0 0= 0= TTRUE
      ENDOF
   ;MATCH
   TX-NO-LEAK ;

\ ---- a refusal leaves the prep genuinely usable, not merely alive ----------------
\ The counters in T-CHECK-FOREIGN show a refused prep is still HELD; they cannot show
\ it is still WORKABLE. This leg spends it: the same prep that was just refused is
\ handed to CHECK again under the configuration that built it, and it commits all the
\ way to a model. That is the property a binder actually relies on when it tries a
\ second configuration on a prep, and nothing weaker demonstrates it - a prep whose
\ block had been half-consumed on the refusal path would pass every counter assertion
\ and fail right here.
: T-REFUSE-THEN-BIND ( -- )
   s" a refused prep still binds under its own configuration" T-LABEL
   TX-CLEAN!  TX-LAY
   TX-CFG-A TX-CFG-KEY!
   TX-PATH SAFET:LOAD TX-CFG-A PREPARE
   MATCH GPT2TX:prep-result
      prepared OF
         TX-CFG-B CHECK                          \ refused: the twin's identity
         MATCH GPT2TX:check-result
            matched OF
               s" the twin was accepted, so the refusal never happened" T-LABEL
               0 0= 0= TTRUE
               ABORT-CHECKED
            ENDOF
            refused OF
               {: code:n :}
               code E-GX-FOREIGN T=
               TX-CFG-A CHECK                    \ the very same prep, its own identity
               MATCH GPT2TX:check-result
                  matched OF
                     COMMIT-MAPPED
                     s" and the model it yields is complete" T-LABEL
                     MODEL-NL TX-NL T=
                     TX-STASH-MKEY
                     TX-KEY-IS-CFG
                     TX-MODEL-HELD
                     MODEL-DISPOSE RES-CODE 0 T=
                  ENDOF
                  refused OF
                     {: c2:n :}
                     s" the refusal damaged the prep: its own configuration was refused, code" T-LABEL
                     c2 . cr
                     0 0= 0= TTRUE
                     ABORT
                  ENDOF
               ;MATCH
            ENDOF
         ;MATCH
      ENDOF
      rejected OF
         s" refuse-then-bind leg could not prepare" T-LABEL
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
   s" the commit half's public surface resolves" T-LABEL
   s" GX-OK-CHECK ( GPT2TX:prep MDLCFG:mcfg -- GPT2TX:check-result ) GPT2TX:CHECK" TX-ACCEPTED
   s" GX-OK-ABORT-CHECKED ( GPT2TX:checked-prep -- ) GPT2TX:ABORT-CHECKED" TX-ACCEPTED
   s" GX-OK-COMMIT ( GPT2TX:checked-prep -- GPT2TX:gpt2-model ) GPT2TX:COMMIT-MAPPED" TX-ACCEPTED
   s" GX-OK-MODEL-DISPOSE ( GPT2TX:gpt2-model -- result<n,n> ) GPT2TX:MODEL-DISPOSE" TX-ACCEPTED
   \ THE POINT OF THE WHOLE SHAPE. The commit cannot be reached with an uncompared prep:
   \ its argument type is one only CHECK produces, so "the identity was compared" is a
   \ static precondition rather than a rule the commit's body has to remember. Deleting
   \ the compare from CHECK cannot restore this candidate either - it is the TYPE that
   \ refuses, so no edit to a body makes an unchecked prep committable.
   s" the commit is unreachable without the compare" T-LABEL
   s" GX-BAD-COMMIT-PREP ( GPT2TX:prep -- GPT2TX:gpt2-model ) GPT2TX:COMMIT-MAPPED" TX-REJECTED
   s" GX-BAD-COMMIT-CENSUS ( SAFET:census -- GPT2TX:gpt2-model ) GPT2TX:COMMIT-MAPPED" TX-REJECTED
   s" GX-BAD-CHECK-CHECKED ( GPT2TX:checked-prep MDLCFG:mcfg -- GPT2TX:check-result ) GPT2TX:CHECK" TX-REJECTED
   s" GX-BAD-CHECK-AMBIENT ( MDLCFG:mcfg -- GPT2TX:check-result ) GPT2TX:CHECK" TX-REJECTED
   s" a checked prep is linear: no copy, no discard, no store, no forging" T-LABEL
   s" GX-BAD-CHK-DUP ( GPT2TX:checked-prep -- GPT2TX:checked-prep GPT2TX:checked-prep ) dup" TX-REJECTED
   s" GX-BAD-CHK-DROP ( GPT2TX:checked-prep -- ) drop" TX-REJECTED
   s" GX-BAD-CHK-STORE ( GPT2TX:checked-prep ptr n -- ) !" TX-REJECTED
   s" GX-BAD-CHK-FORGE ( n -- GPT2TX:checked-prep ) " TX-REJECTED
   s" the two prep kinds do not substitute for each other at any exit" T-LABEL
   s" GX-BAD-ABORT-CHECKED-PREP ( GPT2TX:prep -- ) GPT2TX:ABORT-CHECKED" TX-REJECTED
   s" GX-BAD-ABORT-CHK ( GPT2TX:checked-prep -- ) GPT2TX:ABORT" TX-REJECTED
   s" GX-BAD-CHK-TWICE ( GPT2TX:checked-prep -- ) GPT2TX:ABORT-CHECKED GPT2TX:ABORT-CHECKED" TX-REJECTED
   s" GX-BAD-COMMIT-THEN-ABORT ( GPT2TX:checked-prep -- GPT2TX:gpt2-model ) GPT2TX:COMMIT-MAPPED GPT2TX:ABORT-CHECKED" TX-REJECTED
   s" check-result payloads cannot cross roles" T-LABEL
   s" GX-BAD-MATCHED-PREP ( GPT2TX:prep -- GPT2TX:check-result ) GPT2TX-CHECK--RESULT:MATCHED" TX-REJECTED
   s" GX-BAD-REFUSED-CHK ( GPT2TX:checked-prep n -- GPT2TX:check-result ) GPT2TX-CHECK--RESULT:REFUSED" TX-REJECTED
   s" GX-BAD-CHECK-DROPPED ( GPT2TX:prep MDLCFG:mcfg -- ) GPT2TX:CHECK" TX-REJECTED
   \ A model owns a linear residency, so the RECORD is linear by containment: the
   \ checker refuses to copy or discard it, which is what makes the checkpoint mapping
   \ impossible to leak or to free twice.
   s" a bound model is linear by containment, and cannot be forged" T-LABEL
   s" GX-BAD-MODEL-DUP ( GPT2TX:gpt2-model -- GPT2TX:gpt2-model GPT2TX:gpt2-model ) dup" TX-REJECTED
   s" GX-BAD-MODEL-DROP ( GPT2TX:gpt2-model -- ) drop" TX-REJECTED
   s" GX-BAD-MODEL-STORE ( GPT2TX:gpt2-model ptr n -- ) !" TX-REJECTED
   s" GX-BAD-MODEL-FORGE ( n -- GPT2TX:gpt2-model ) " TX-REJECTED
   s" GX-BAD-MODEL-PROOF-RAW ( WSTORE:resident n MDLCFG:cfgkey n -- GPT2TX:gpt2-model ) GPT2TX-GPT2--MODEL:MAKE" TX-REJECTED
   s" the model's exit consumes it exactly once" T-LABEL
   s" GX-BAD-MD-TWICE ( GPT2TX:gpt2-model -- result<n,n> result<n,n> ) GPT2TX:MODEL-DISPOSE GPT2TX:MODEL-DISPOSE" TX-REJECTED
   s" GX-BAD-MD-KEEPS ( GPT2TX:gpt2-model -- GPT2TX:gpt2-model result<n,n> ) GPT2TX:MODEL-DISPOSE" TX-REJECTED
   s" GX-BAD-MD-DROPPED ( GPT2TX:gpt2-model -- ) GPT2TX:MODEL-DISPOSE" TX-REJECTED
   s" GX-BAD-MD-RESIDENT ( WSTORE:resident -- result<n,n> ) GPT2TX:MODEL-DISPOSE" TX-REJECTED
   s" and the residency inside a model cannot be reached around it" T-LABEL
   s" GX-BAD-MODEL-RD ( GPT2TX:gpt2-model -- result<n,n> ) WSTORE:RESIDENT-DISPOSE" TX-REJECTED
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
   s" GX-BAD-N-TABLE ( n -- WSTORE:table ) GPT2TX:N>TABLE" UNRESOLVED
   s" GX-BAD-MINT-CHECKED ( ptr u8 -- GPT2TX:checked-prep ) GPT2TX:MINT-CHECKED" UNRESOLVED
   s" GX-BAD-TAKE-CHECKED ( GPT2TX:checked-prep -- ptr n ) GPT2TX:TAKE-CHECKED" UNRESOLVED
   s" GX-BAD-MAPPING-N ( SAFET:mapping -- n ) GPT2TX:MAPPING>N" UNRESOLVED
   s" GX-BAD-N-MAPPING ( n -- SAFET:mapping ) GPT2TX:N>MAPPING" UNRESOLVED
   s" GX-BAD-MINT-PROOF ( -- GPT2TX:mdl-proof ) GPT2TX:MINT-MDL-PROOF" UNRESOLVED
   s" GX-BAD-MODEL-NL ( GPT2TX:gpt2-model -- GPT2TX:gpt2-model n ) GPT2TX:MODEL-NL" UNRESOLVED
   s" GX-BAD-MODEL-KEY ( GPT2TX:gpt2-model -- GPT2TX:gpt2-model MDLCFG:cfgkey ) GPT2TX:MODEL-KEY" UNRESOLVED ;

\ ---- presence-gated real artifact ----------------------------------------------
: TX-REAL-PATH ( -- ptr u8 n )  s" gpt2-model/model.safetensors" ;

768 4 * constant TX-PB-LEN                      \ h.0.ln_1.weight: nembd F32 values
create TX-PBA TX-PB-LEN allot                   \ the bytes the census copied out
create TX-PBB TX-PB-LEN allot                   \ the bytes found at the computed offset
variable TX-MOFF

: TX-MAP-BODY ( SAFET:mapping ptr u8 n -- SAFET:mapping ) {: ba:ptr blen:n :}
   ba TX-MOFF @ BYTE+  TX-PBB  TX-PB-LEN  BYTE-COPY ;

\ WHAT THIS PROVES, AND WHAT IT DOES NOT. The mapped store serves a slot as
\ mapping-base + the row's offset, and the row's offset is the census's MAP-OFFSET?.
\ This leg checks that arithmetic against the real checkpoint from both ends: the bytes
\ the census copies out of one real tensor, and the bytes sitting at that tensor's
\ computed offset inside the detached mapping, are the same bytes. Since the mapping IS
\ the file mapped read-only, those are the file's bytes at that offset.
\
\ It stops one link short of reading through the committed model, and the reason is
\ structural rather than an omission: reading a slot through a held resident needs a
\ scoped access that disposes its owner on the throw path (WSTORE:WITH-SLOT throws
\ E-SLOT/E-EXTENT, and a resident cannot be rebuilt around a throw), which is the
\ linear-scope capability and belongs to the forward-pass leaf. The remaining link -
\ that WITH-SLOT over a mapped store returns the bytes at base+offset - is already
\ pinned by the byte-equality legs in weight-store-test.f over both arms.
\ One tensor, named by its real HF key: copy its first bytes out of the census, note
\ the mapping-frame offset the table will carry, then read the mapping at exactly that
\ offset and require the same bytes. Parameterised by key so the leg can walk several
\ tensors of different shapes and file positions rather than trusting one.
: TX-PROBE-ONE ( ptr u8 n -- ) {: ka:ptr ku:n :}
   TX-REAL-PATH SAFET:LOAD
   ka ku SAFET:FIND TX-OPT-VAL {: id:n :}
   id TX-PBA TX-PB-LEN SAFET:COPY-DATA? TX-OPT-VAL TX-PB-LEN T=
   id SAFET:MAP-OFFSET? TX-OPT-VAL TX-MOFF !
   SAFET:DETACH-MAPPING                         \ ( census mapping )
   swap SAFET:RELEASE                           \ ( mapping )
   [: TX-MAP-BODY ;] SAFET:WITH-MAPPING TX-OPT-VAL drop
   SAFET:UNMAP-MAPPING RES-CODE 0 T=
   TX-PBA TX-PB-LEN TX-PBB TX-PB-LEN TX-BYTES= ;

\ Three tensors, chosen so a single lucky offset cannot pass the leg: a rank-1 vector
\ near the start of the data section, the rank-2 Conv1D matrix whose [in,out]
\ orientation the forward pass depends on, and a tensor in the LAST block, whose data
\ sits hundreds of megabytes into the file where a 32-bit offset truncation or a
\ header-length slip would show up and a small-offset probe would not.
: TX-REAL-BYTES ( -- )
   s" a rank-1 vector is byte-identical at its computed mapping offset" T-LABEL
   s" h.0.ln_1.weight" TX-PROBE-ONE
   s" so is the Conv1D matrix the forward pass reads untransposed" T-LABEL
   s" h.0.attn.c_attn.weight" TX-PROBE-ONE
   s" and so is a tensor far into the file, where a truncated offset would show" T-LABEL
   s" h.11.mlp.c_proj.weight" TX-PROBE-ONE ;

548105171 constant TX-REAL-BYTES-N               \ the pinned checkpoint's exact file size

: TX-REAL-COMMIT ( -- )
   TX-CFG-124M TX-CFG-KEY!                      \ the real configuration's own identity
   TX-REAL-PATH SAFET:LOAD TX-CFG-124M PREPARE
   MATCH GPT2TX:prep-result
      prepared OF
         s" real gpt2 prepared rows=" type PREP-COUNT dup . cr
         160 T=
         TX-CFG-124M CHECK
         MATCH GPT2TX:check-result
            matched OF
               COMMIT-MAPPED
               s" the real checkpoint commits to a mapped model of 12 layers" T-LABEL
               MODEL-NL 12 T=
               TX-MODEL-HELD
               s" bound to the real configuration's identity, cell for cell" T-LABEL
               TX-STASH-MKEY
               TX-KEY-IS-CFG
               \ ok is not a token success flag: it carries the byte count WSTORE gave
               \ back, which for a mapped model is the whole checkpoint mapping. Pinning
               \ the exact file size is what proves the model was serving the entire
               \ file rather than some truncated span of it.
               s" and its exit gives back the whole checkpoint mapping, to the byte" T-LABEL
               MODEL-DISPOSE TX-RES-VAL TX-REAL-BYTES-N T=
            ENDOF
            refused OF
               {: code:n :}
               s" the real checkpoint was refused as foreign, code" T-LABEL
               code . cr
               0 0= 0= TTRUE
               ABORT
            ENDOF
         ;MATCH
      ENDOF
      rejected OF
         s" the real checkpoint did not prepare, code" T-LABEL
         . cr SAFET:RELEASE
         0 0= 0= TTRUE
      ENDOF
   ;MATCH ;

: T-REAL ( -- )
   TX-REAL-PATH SAFET:PRESENT? 0= if
      s" gpt2-bind: gpt2-model/model.safetensors absent -> real-artifact leg SKIPPED" type cr
      0 0= TTRUE exit
   then
   s" the real gpt2 checkpoint binds end to end" T-LABEL
   TX-REAL-BYTES
   TX-NO-LEAK
   TX-REAL-COMMIT
   TX-NO-LEAK ;

\ ---- the allocated arm's gate: compare and retype, nothing moves -----------------
: T-CHECK-ALLOC ( -- )
   s" CHECK-ALLOC refuses a foreign prep and moves nothing" T-LABEL
   TX-CLEAN!  TX-LAY
   TX-PATH SAFET:LOAD TX-CFG-A PREPARE
   MATCH GPT2TX:prep-result
      prepared OF
         TX-CFG-B CHECK-ALLOC
         MATCH GPT2TX:check-alloc-result
            matched OF
               s" a foreign configuration must not match" T-LABEL
               ABORT-CHECKED-ALLOC
               0 0= 0= TTRUE
            ENDOF
            refused OF
               E-GX-FOREIGN T=
               s" and the refused prep is still whole and still ABORTable" T-LABEL
               PREP-COUNT TX-CNT T=
               ABORT
            ENDOF
         ;MATCH
      ENDOF
      rejected OF
         s" check-alloc leg could not prepare" T-LABEL
         . cr SAFET:RELEASE
         0 0= 0= TTRUE
      ENDOF
   ;MATCH
   TX-NO-LEAK ;

\ ---- the allocated commit -------------------------------------------------------------
\ Copy correctness is checked against the ARENA the commit actually filled, not against a
\ re-derivation: CA-ARENA is where COMMIT-ALLOCATED put the weights and ARENA-OFF is where
\ it decided each slot goes, so reading those bytes back and comparing them with what the
\ census itself copies out is a comparison between the commit's output and the file. It has
\ to be done this way today because there is no way to read a slot back through a bound
\ model - that needs the scoped access over a held resident which the linear-scope
\ capability provides, exactly as the mapped arm's real-artifact leg records.
$800 constant TX-CB-CAP
create TX-CB-A TX-CB-CAP allot                  \ what the census says the tensor is
create TX-CB-B TX-CB-CAP allot                  \ what the commit put in the arena

: TX-ARENA-BYTES ( n n -- )                     \ arena offset, length -> TX-CB-B
   {: aoff:n len:n :}
   CA-ARENA @ aoff BYTE+  TX-CB-B  len BYTE-COPY ;

\ One slot, compared end to end: the census's own copy of the tensor against the bytes the
\ commit placed at that slot's arena offset. Parameterised by slot so the leg can walk
\ tensors of different rank and file position rather than trusting one.
\ Compares a bounded PREFIX of the span, the way the mapped arm's real-artifact probe
\ does: a weight can be megabytes, and what is being proved is that the commit put THIS
\ tensor's bytes at THIS arena offset - a wrong id, a wrong offset or a shifted row all
\ show up in the first bytes. COPY-DATA? truncates to the capacity it is given, so the
\ prefix length is what it returns and what both sides are compared over.
\ Reads the LAST bytes of a tensor out of the census, which COPY-DATA? cannot do - it
\ copies from the start and truncates to the capacity it is given, so a head-only probe
\ passes for a span that was copied short. The tail is where a truncation shows.
variable TX-TAIL-N

: TX-TAIL-BODY ( SAFET:census ptr u8 n -- SAFET:census ) {: sa:ptr slen:n :}
   slen TX-CB-CAP > if TX-CB-CAP else slen then {: take:n :}
   sa slen take - BYTE+  TX-CB-A  take BYTE-COPY
   take TX-TAIL-N ! ;

\ Head AND tail, because either alone is blind to a real defect: the head misses a copy
\ that was truncated, and a span long enough to exceed the compare buffer is exactly where
\ that happens. Both ends are compared against a fresh census of the same file.
: TX-SPAN-AGREES ( SAFET:census n n n -- SAFET:census )
   {: id:n aoff:n len:n :}
   len TX-CB-CAP > if TX-CB-CAP else len then {: take:n :}
   id TX-CB-A take SAFET:COPY-DATA? TX-OPT-VAL take T=
   aoff take TX-ARENA-BYTES
   TX-CB-A take TX-CB-B take TX-BYTES=
   id [: TX-TAIL-BODY ;] SAFET:WITH-TENSOR TX-OPT-VAL drop
   TX-TAIL-N @ {: tn:n :}
   aoff len + tn -  tn TX-ARENA-BYTES
   TX-CB-A tn TX-CB-B tn TX-BYTES= ;

\ ---- forced failure at each fallible step ----------------------------------------------
\ There is no allocation fault injector in the tree, so each leg forces its step to fail by
\ corrupting ONE field of a real witness block produced by a real PREPARE, and then runs the
\ real COMMIT-ALLOCATED. What is under test is the unwind, and the unwind does not care
\ which code came out of the step - it cares that everything acquired so far, plus the two
\ owners the prep still held, are given back before the code surfaces. Every leg therefore
\ ends at the same place: TX-NO-LEAK, every counter back where the suite found it. That
\ helper compares against the captured baseline rather than against zero, which is what
\ makes these legs correct under maki/test.f - the suites share one process, and the
\ weight-store suite ahead of this one leaves its own documented throw strands behind.

\ An arena size no allocator can satisfy: the step that asks for it fails, and nothing has
\ been acquired yet beyond the prep's own owners.
: TX-WRECK-SUM ( GPT2TX:checked-prep-alloc -- GPT2TX:checked-prep-alloc )
   TAKE-CHECKED-ALLOC {: blk:ptr :}
   MAX-N 16 - blk P-SUM cells + !
   blk BLK>BYTES MINT-CHECKED-ALLOC ;

\ A row extent no arena can lay out: the table step meets it while the arena and its buffer
\ are already live, so this leg is the one that proves the buffer is given back. It wrecks a
\ ROW rather than P-CNT deliberately - P-CNT is how the block describes its own size to the
\ release, so corrupting that would break the unwind itself instead of testing it.
: TX-WRECK-LEN ( GPT2TX:checked-prep-alloc -- GPT2TX:checked-prep-alloc )
   TAKE-CHECKED-ALLOC {: blk:ptr :}
   MAX-N  blk 0 PR-LEN ROW-CELL !
   blk BLK>BYTES MINT-CHECKED-ALLOC ;

\ A census id no census has: the copy walk refuses with the arena, its buffer AND the
\ arena-frame table all live, which is the widest unwind this word has.
: TX-WRECK-ID ( GPT2TX:checked-prep-alloc -- GPT2TX:checked-prep-alloc )
   TAKE-CHECKED-ALLOC {: blk:ptr :}
   $7FFFFFF  blk 0 PR-ID ROW-CELL !
   blk BLK>BYTES MINT-CHECKED-ALLOC ;

\ Each leg builds a real transaction, wrecks the one field that makes its step fail, and
\ runs the real commit. A leg that reaches MODEL-DISPOSE has NOT thrown, and TTHROWSQ says
\ so; the baseline assertion after it is what proves the unwind was complete.
: TX-CA-WITNESS ( -- GPT2TX:check-alloc-result )
   TX-PATH SAFET:LOAD TX-CFG-A PREPARE
   MATCH GPT2TX:prep-result
      prepared OF TX-CFG-A CHECK-ALLOC ENDOF
      rejected OF
         s" forced-failure leg could not prepare" T-LABEL
         . cr SAFET:RELEASE
         E-TX-FIX throw
      ENDOF
   ;MATCH ;

: TX-GATE-REFUSED ( n -- )
   s" forced-failure leg was refused by the gate" T-LABEL
   . cr
   0 0= 0= TTRUE ;

: TX-WRECK-SUM-RUN ( -- )
   TX-CA-WITNESS
   MATCH GPT2TX:check-alloc-result
      matched OF TX-WRECK-SUM COMMIT-ALLOCATED MODEL-DISPOSE TX-RES-VAL drop ENDOF
      refused OF TX-GATE-REFUSED ABORT ENDOF
   ;MATCH ;

: TX-WRECK-LEN-RUN ( -- )
   TX-CA-WITNESS
   MATCH GPT2TX:check-alloc-result
      matched OF TX-WRECK-LEN COMMIT-ALLOCATED MODEL-DISPOSE TX-RES-VAL drop ENDOF
      refused OF TX-GATE-REFUSED ABORT ENDOF
   ;MATCH ;

: TX-WRECK-ID-RUN ( -- )
   TX-CA-WITNESS
   MATCH GPT2TX:check-alloc-result
      matched OF TX-WRECK-ID COMMIT-ALLOCATED MODEL-DISPOSE TX-RES-VAL drop ENDOF
      refused OF TX-GATE-REFUSED ABORT ENDOF
   ;MATCH ;

\ The happy path, and the two span probes that prove the arena holds the file's bytes at the
\ offsets the commit chose. The probes run against a SECOND census of the same file, so what
\ they compare the arena with is the file rather than anything the commit computed.
\ ONE slot pair, used by both halves of the probe. The recording word and the comparing
\ word must name the SAME slots: recording slot A's arena location and then comparing
\ slot B's bytes against it passes whenever the data is uniform and means nothing when it
\ is not, which is exactly the shape this pair replaced.
2 constant TX-PROBE-VEC                         \ ln_f.weight: a rank-1 vector
variable TX-RA-OFF   variable TX-RA-LEN   variable TX-RA-ID
variable TX-RB-OFF   variable TX-RB-LEN   variable TX-RB-ID

variable TX-CA-VEC-OFF                          \ the vector slot's arena offset and extent
variable TX-CA-VEC-LEN
variable TX-CA-CONV-OFF                         \ and the Conv1D slot's
variable TX-CA-CONV-LEN

\ The census id a slot's HF key resolves to, asked of the census on the stack.
: TX-ID-OF-SLOT ( SAFET:census n -- SAFET:census n ) {: slot:n :}
   TX-CFG-A slot TX-KEY-LEN {: klen:n :}
   drop                                         \ the mcfg copy
   TX-KBUF klen SAFET:FIND TX-OPT-VAL ;

: TX-PROBE-SPANS ( -- )
   TX-PATH SAFET:LOAD
   TX-PROBE-VEC TX-ID-OF-SLOT {: vid:n :}
   TX-PROBE-CONV TX-ID-OF-SLOT {: cid:n :}
   s" the rank-1 vector span is the file's own bytes, at its arena offset" T-LABEL
   vid  TX-CA-VEC-OFF @  TX-CA-VEC-LEN @  TX-SPAN-AGREES
   s" and so is the Conv1D matrix the forward pass reads" T-LABEL
   cid  TX-CA-CONV-OFF @  TX-CA-CONV-LEN @  TX-SPAN-AGREES
   SAFET:RELEASE ;


\ Which slot carries a given census id. The real checkpoint's slots are not knowable by
\ eye, so a probe names its tensor by HF key, resolves the key to a census id, and finds
\ the slot from the block's own carried rows - the same rows the commit copied by.
: TX-SLOT-OF-ID ( ptr n n -- n ) {: blk:ptr id:n :}
   -1
   blk P-CNT cells + @ 0 ?do
      blk i PR-ID ROW-CELL @ id = if drop i then
   loop ;

: TX-NOTE-ID ( ptr n n ptr n ptr n -- ) {: blk:ptr id:n ovar:ptr lvar:ptr :}
   blk id TX-SLOT-OF-ID {: slot:n :}
   slot 0 < if E-TX-FIX throw then
   blk slot ARENA-OFF ovar !
   blk slot PR-LEN ROW-CELL @ lvar ! ;

\ ---- the packed layout, asserted as a recurrence -------------------------------------
\ The span probes check two slots. This checks the SHAPE of the whole layout, which is
\ what a two-slot probe cannot: the arena starts at zero, every row begins exactly where
\ the previous one ended - so there is no gap and no overlap anywhere - and the last row
\ ends exactly at the arena size the commit allocated. An arena walk that drifts by one
\ row, or a length that disagrees with the sum, shows up here rather than in whichever
\ two slots the probes happened to pick.
variable TX-WALK-BAD

: TX-ARENA-WALK ( GPT2TX:checked-prep-alloc -- GPT2TX:checked-prep-alloc )
   TAKE-CHECKED-ALLOC {: blk:ptr :}
   blk P-CNT cells + @ {: count:n :}
   0 TX-WALK-BAD !
   s" the packed arena starts at zero" T-LABEL
   blk 0 ARENA-OFF 0 T=
   count 0 ?do
      blk i ARENA-OFF  blk i PR-LEN ROW-CELL @ +  {: end:n :}
      i 1 + count < if
         blk i 1 + ARENA-OFF end <> if 1 TX-WALK-BAD ! then
      else
         end blk P-SUM cells + @ <> if 1 TX-WALK-BAD ! then
      then
   loop
   s" every row begins where the previous ended, and the last ends at the arena size" T-LABEL
   TX-WALK-BAD @ 0 T=
   blk BLK>BYTES MINT-CHECKED-ALLOC ;

: TX-NOTE-ARENA ( GPT2TX:checked-prep-alloc -- GPT2TX:checked-prep-alloc )
   TAKE-CHECKED-ALLOC {: blk:ptr :}
   blk TX-PROBE-VEC ARENA-OFF TX-CA-VEC-OFF !
   blk TX-PROBE-VEC PR-LEN ROW-CELL @ TX-CA-VEC-LEN !
   blk TX-PROBE-CONV ARENA-OFF TX-CA-CONV-OFF !
   blk TX-PROBE-CONV PR-LEN ROW-CELL @ TX-CA-CONV-LEN !
   blk BLK>BYTES MINT-CHECKED-ALLOC ;

\ The real checkpoint, committed to an ALLOCATED model: 548 MB copied span by span into
\ one packed arena. Both probes compare the arena against a second census of the same
\ file, so what they agree with is the file on disk and not any number the commit derived.
: TX-NOTE-REAL ( GPT2TX:checked-prep-alloc -- GPT2TX:checked-prep-alloc )
   TAKE-CHECKED-ALLOC {: blk:ptr :}
   TX-REAL-PATH SAFET:LOAD
   s" h.0.ln_1.weight" SAFET:FIND TX-OPT-VAL {: vid:n :}
   s" h.0.attn.c_attn.weight" SAFET:FIND TX-OPT-VAL {: cid:n :}
   SAFET:RELEASE
   vid TX-RA-ID !  cid TX-RB-ID !
   blk vid TX-RA-OFF TX-RA-LEN TX-NOTE-ID
   blk cid TX-RB-OFF TX-RB-LEN TX-NOTE-ID
   blk BLK>BYTES MINT-CHECKED-ALLOC ;

: TX-REAL-SPANS ( -- )
   TX-REAL-PATH SAFET:LOAD
   s" the real rank-1 vector is the file's own bytes at its arena offset" T-LABEL
   TX-RA-ID @ TX-RA-OFF @ TX-RA-LEN @ TX-SPAN-AGREES
   s" and so is the real Conv1D matrix the forward pass reads" T-LABEL
   TX-RB-ID @ TX-RB-OFF @ TX-RB-LEN @ TX-SPAN-AGREES
   SAFET:RELEASE ;

: T-REAL-ALLOC ( -- )
   TX-REAL-PATH SAFET:PRESENT? 0= if
      s" gpt2-bind: real-artifact allocated leg SKIPPED" type cr exit
   then
   s" the real checkpoint commits to an ALLOCATED model that owns its arena" T-LABEL
   TX-CFG-124M TX-CFG-KEY!
   TX-REAL-PATH SAFET:LOAD TX-CFG-124M PREPARE
   MATCH GPT2TX:prep-result
      prepared OF
         PREP-SUM {: want:n :}
         TX-CFG-124M CHECK-ALLOC
         MATCH GPT2TX:check-alloc-result
            matched OF
               TX-ARENA-WALK
               TX-NOTE-REAL
               COMMIT-ALLOCATED
               MODEL-NL 12 T=
               TX-STASH-MKEY
               TX-KEY-IS-CFG
               TX-REAL-SPANS
               s" and its exit gives back exactly the packed arena" T-LABEL
               MODEL-DISPOSE TX-RES-VAL want T=
               TX-NO-LEAK
            ENDOF
            refused OF TX-GATE-REFUSED ABORT ENDOF
         ;MATCH
      ENDOF
      rejected OF
         s" real allocated leg could not prepare" T-LABEL
         . cr SAFET:RELEASE
         0 0= 0= TTRUE
      ENDOF
   ;MATCH ;

: T-COMMIT-ALLOC ( -- )
   s" a matched prep commits to an allocated model that owns its arena" T-LABEL
   TX-CLEAN!  TX-LAY
   TX-CFG-A TX-CFG-KEY!
   TX-PATH SAFET:LOAD TX-CFG-A PREPARE
   MATCH GPT2TX:prep-result
      prepared OF
         PREP-SUM {: want:n :}
         TX-CFG-A CHECK-ALLOC
         MATCH GPT2TX:check-alloc-result
            matched OF
               TX-ARENA-WALK
               TX-NOTE-ARENA
               COMMIT-ALLOCATED
               s" it reports the depth PREPARE validated" T-LABEL
               MODEL-NL TX-NL T=
               s" and the configuration's identity, cell for cell" T-LABEL
               TX-STASH-MKEY
               TX-KEY-IS-CFG
               TX-PROBE-SPANS
               s" and its exit gives back exactly the packed arena" T-LABEL
               MODEL-DISPOSE TX-RES-VAL want T=
               TX-NO-LEAK
            ENDOF
            refused OF TX-GATE-REFUSED ABORT ENDOF
         ;MATCH
      ENDOF
      rejected OF
         s" allocated-commit leg could not prepare" T-LABEL
         . cr SAFET:RELEASE
         0 0= 0= TTRUE
      ENDOF
   ;MATCH
   TX-NO-LEAK ;

: T-COMMIT-ALLOC-FAILS ( -- )
   s" an arena the allocator cannot satisfy unwinds completely" T-LABEL
   TX-CLEAN!  TX-LAY
   [: TX-WRECK-SUM-RUN ;] E-MEM-MAP TTHROWSQ
   TX-NO-LEAK
   s" a row extent no arena can lay out unwinds the live buffer too" T-LABEL
   [: TX-WRECK-LEN-RUN ;] WSTORE:E-EXTENT TTHROWSQ
   TX-NO-LEAK
   s" a census id the census does not have unwinds buffer and table together" T-LABEL
   [: TX-WRECK-ID-RUN ;] E-GX-COPY TTHROWSQ
   TX-NO-LEAK ;

: RUN ( -- )
   T-RESET
   TX-BASELINE!
   T-CHECKER
   T-CLAIM
   T-ARITH
   T-BLOCK-CELLS
   T-PREPARE-OK       TX-NO-LEAK
   T-REJECT-CFG       TX-NO-LEAK
   T-REJECT-CENSUS    TX-NO-LEAK
   T-REJECT-IMAGELESS TX-NO-LEAK
   T-ABORT            TX-NO-LEAK
   T-PREP-OWNS-PLAN   TX-NO-LEAK
   T-TWIN             TX-NO-LEAK
   T-FOREIGN-COMPARE  TX-NO-LEAK
   T-CHECK-FOREIGN    TX-NO-LEAK
   T-CHECK-MATCH      TX-NO-LEAK
   T-COMMIT           TX-NO-LEAK
   T-REFUSE-THEN-BIND TX-NO-LEAK
   T-CHECK-ALLOC      TX-NO-LEAK
   T-COMMIT-ALLOC     TX-NO-LEAK
   T-COMMIT-ALLOC-FAILS TX-NO-LEAK
   T-REAL-ALLOC       TX-NO-LEAK
   T-REAL
   s" the whole suite released every owner it took" T-LABEL
   TX-NO-LEAK
   TX-CLEANUP ;

RUN

;package

\ ---------------------------------------------------------------------------------
\ package GPT2TX-DR - the destruction-review pins, from OUTSIDE the package.
\
\ WHY THIS SECTION DOES NOT REOPEN GPT2TX. Everything above runs inside the package,
\ because the leaf contract asks the fixtures to read validated rows that no public
\ word hands out. That is also why the legs above cannot speak for what a FOREIGN file
\ can do: inside the package the private words resolve, so an "is this reachable?"
\ question asked from in there answers about the wrong vantage point. This section is
\ the outside vantage point - a package that has never opened GPT2TX - and it pins the
\ gap the model's private-mint proof does NOT close, the MODELPROV-TEST T-KNOWN-GAP
\ convention.
\
\ WHAT THE PROOF DOES AND DOES NOT BUY. `mdl-proof` makes a model unforgeable from
\ nothing: no outside file can conjure the proof, so no outside file can MAKE a model
\ out of thin air. It does not make a REAL model tamper-proof, because the generated
\ UNMAKE is public: a holder of a genuine model can take the residency straight out of
\ it, dispose it behind the model's back, or rebuild the record with a forged depth and
\ the original proof. The three pins below are written as ACCEPT deliberately - they
\ record what compiles TODAY. When the sealed-destructure capability
\ (habu-checker-sealed-destructure-d967fc03) lands, these three legs FAIL, and that
\ failure is the signal to delete them and retire the caveat in gpt2-bind.f's header.
\ The fourth pin is the control that keeps the other three honest: the package's own
\ readers stay unreachable from here, so these are gaps in the GENERATED surface, not
\ an accident of the suite's vantage point.
\ ---------------------------------------------------------------------------------
package GPT2TX-DR

: ACCEPTED ( ptr u8 n -- )
   CHECK-QUIET-CANDIDATE! -1 T= ;

: UNRESOLVED ( ptr u8 n -- )
   CHECK-QUIET-CANDIDATE! 1 T= ;

: T-KNOWN-GAP ( -- )
   s" KNOWN GAP: the generated UNMAKE extracts a real model's residency TODAY" T-LABEL
   s" DRX-UNMAKE ( GPT2TX:gpt2-model -- WSTORE:resident n MDLCFG:cfgkey GPT2TX:mdl-proof ) GPT2TX-GPT2--MODEL:UNMAKE"
      ACCEPTED
   s" KNOWN GAP: so the residency can be disposed behind the model's back" T-LABEL
   s" DRX-STEAL ( GPT2TX:gpt2-model -- result<n,n> ) GPT2TX-GPT2--MODEL:UNMAKE drop drop drop WSTORE:RESIDENT-DISPOSE"
      ACCEPTED
   s" KNOWN GAP: and a real model can be rebuilt with a forged depth" T-LABEL
   s" DRX-FORGE-NL ( GPT2TX:gpt2-model -- GPT2TX:gpt2-model ) GPT2TX-GPT2--MODEL:UNMAKE >r >r drop 99 r> r> GPT2TX-GPT2--MODEL:MAKE"
      ACCEPTED
   s" the proof still refuses a model built from nothing, which is what it is for" T-LABEL
   s" DRX-FORGE-WHOLE ( WSTORE:resident n MDLCFG:cfgkey n -- GPT2TX:gpt2-model ) GPT2TX-GPT2--MODEL:MAKE"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" CONTROL: the package's own readers are unreachable from outside it, so the" T-LABEL
   s" three gaps above are in the GENERATED surface, not in this suite's vantage" T-LABEL
   s" DRX-MODEL-NL ( GPT2TX:gpt2-model -- GPT2TX:gpt2-model n ) GPT2TX:MODEL-NL" UNRESOLVED
   s" DRX-MINT-PROOF ( -- GPT2TX:mdl-proof ) GPT2TX:MINT-MDL-PROOF" UNRESOLVED ;

T-KNOWN-GAP
T-REPORT

;package
