\ gpt2-bind-fixture.f - the synthetic-checkpoint emitter, the leak accounting and the
\ shared assertion vocabulary that the three GPT2TX bind suites all run on.
\
\ WHY THIS IS ITS OWN FILE. The bind transaction has three acceptance suites - the
\ PREPARE phase (gpt2-bind-test.f), the mapped arm (gpt2-check-test.f) and the
\ allocated arm (gpt2-alloc-test.f) - and every one of them needs the same synthetic
\ checkpoint, the same leak counters and the same handful of assertion helpers. Pasting
\ that machinery into each suite would be three copies of one emitter that must agree
\ byte for byte, so it lives here instead, as the library docs/forth.md asks reusable
\ helpers to be. This file defines no test and runs nothing; the suites require it.
\
\ WHY IT REOPENS package GPT2TX. What the three suites prove is which rows the
\ transaction validated, and no public word hands a row, a census, or a table out of a
\ prep - that opacity is the design, so it cannot also be the thing the fixtures read
\ through. The suites therefore run INSIDE the package (the lib/process-pty-handle-test.f
\ arrangement) and read the validated rows through the package-private projections.
\ Every word defined here is prefixed TX- so nothing shadows the module's own vocabulary.
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
variable TX-KA0  variable TX-KA1
variable TX-KA2  variable TX-KA3                \ one owner's captured cfgkey cells
variable TX-KB0  variable TX-KB1
variable TX-KB2  variable TX-KB3                \ that configuration's own cfgkey cells
variable TX-BASE-MAP                            \ leak-counter baselines at suite entry
variable TX-BASE-OWN
variable TX-BASE-WS
variable TX-BASE-PREP

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
\ That is load-bearing for every byte comparison these suites make: with a constant fill
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

\ The pinned real checkpoint, for the presence-gated legs in the two commit suites.
: TX-REAL-PATH ( -- ptr u8 n )  s" gpt2-model/model.safetensors" ;

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

\ the real 124M geometry for the presence-gated legs
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

\ ---- leak accounting, as a delta against a suite's own entry -------------------
\ The three counters are process-wide, and a combined run reaches these suites after
\ others that leave documented strands of their own (weight-store-test.f ends on 14
\ live WSTORE blocks and 2 SAFET owners by design). Asserting absolute zero would
\ therefore pass standalone and fail in maki/test.f while saying nothing about the
\ suite under test. Every assertion below is a delta from the entry baseline, so it
\ measures exactly what these fixtures took and gave back.
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

\ What a relinquished transaction leaves: the prep block and the sealed table are both
\ gone, and the census is still whole - still an owner, still holding its mapping. It is
\ the one state where the four counters disagree with each other, which is exactly why
\ it is worth asserting: a RELINQUISH that forgot the table would satisfy TX-HELD's
\ census legs and this one's census legs alike, and only the WSTORE leg here tells them
\ apart.
: TX-CENSUS-ONLY ( -- )
   SAFET-MAP:LIVE TX-BASE-MAP @ 1 + T=
   SAFET:LIVE-OWNERS TX-BASE-OWN @ 1 + T=
   WSTORE:LIVE TX-BASE-WS @ T=
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

\ ---- the captured identity, read from a configuration and from a bound model ----
\ Slot 7 is layer 0's c_attn Conv1D weight, the [in,out] matrix - not a global and not
\ a plain vector, so it is a row that would move first if the slot walk drifted. Both
\ the PREPARE suite and the allocated arm probe it.
7 constant TX-PROBE-CONV                        \ h.0.attn.c_attn.weight (Conv1D)

: TX-CFG-KEY! ( MDLCFG:mcfg -- )                \ stash a configuration's OWN key cells
   MDLCFG:CFGKEY@ MDLCFG-CFGKEY:UNMAKE {: k0:n k1:n k2:n k3:n :}
   drop
   k0 TX-KB0 !  k1 TX-KB1 !  k2 TX-KB2 !  k3 TX-KB3 ! ;

: TX-STASH-MKEY ( GPT2TX:gpt2-model -- GPT2TX:gpt2-model )
   MODEL-KEY MDLCFG-CFGKEY:UNMAKE {: k0:n k1:n k2:n k3:n :}
   k0 TX-KA0 !  k1 TX-KA1 !  k2 TX-KA2 !  k3 TX-KA3 ! ;

\ Cell by cell, in order. A reversed or rotated capture would still differ from the
\ twin's and still pass a difference test, so difference alone proves nothing about
\ correctness; this is the assertion that pins WHICH key was captured.
: TX-KEY-IS-CFG ( -- )
   TX-KA0 @ TX-KB0 @ T=
   TX-KA1 @ TX-KB1 @ T=
   TX-KA2 @ TX-KB2 @ T=
   TX-KA3 @ TX-KB3 @ T= ;

;package
