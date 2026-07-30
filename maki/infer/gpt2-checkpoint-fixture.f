\ gpt2-checkpoint-fixture.f - the synthetic-checkpoint emitter, the leak accounting and the
\ shared assertion vocabulary that the three GPT2LOAD suites all run on.
\
\ WHY THIS IS ITS OWN FILE. The checkpoint load has three acceptance suites - the
\ PREPARE phase (gpt2-prepare-test.f), the mapped-storage path (gpt2-mapped-test.f) and the
\ copied-storage path (gpt2-copy-test.f) - and every one of them needs the same synthetic
\ checkpoint, the same leak counters and the same handful of assertion helpers. Pasting
\ that machinery into each suite would be three copies of one emitter that must agree
\ byte for byte, so it lives here instead, as the library docs/forth.md asks reusable
\ helpers to be. This file defines no test and runs nothing; the suites require it.
\
\ WHY IT REOPENS package GPT2LOAD. What the three suites prove is which rows the
\ load validated. No public non-consuming accessor exposes a row, parsed tensor
\ index, or table from a prepared-load; RETURN-TENSOR-INDEX exposes the original
\ parsed tensor index only by consuming that load. The suites therefore run
\ INSIDE the package (the lib/process-pty-handle-test.f arrangement) and read the
\ validated rows through the package-private projections.
\ Its helpers use descriptive fixture-specific names so they do not shadow the
\ module's own vocabulary.
\
\ THE FIXTURES ARE GENERATED THROUGH THE PRODUCTION VOCABULARY. A tiny geometry
\ (2 layers, embedding size 4, 2 heads, context 8, vocabulary 5 - thirty tensors) is emitted as a
\ real synthetic safetensors file and loaded back through the real mmap path, the
\ safetensors-test SYNTH-PATH pattern. The header rows are not hand-written JSON:
\ each tensor's name comes from GPT2TENSOR:COPY-NAME? and its shape from
\ GPT2TENSOR:SHAPE, so a fixture that drifts from the role vocabulary cannot be
\ written. Byte extents and data offsets are computed from the EMITTED dims, so
\ every corrupted variant is still a file the safetensors parser itself accepts -
\ which makes each rejection PREPARE's verdict rather than the parser's.
\
\ THE CORRUPTION KNOBS. One builder emits every variant: DAMAGE-SLOT names the slot
\ to damage and DAMAGE-KIND selects wrong element type, extra rank, wrong dimension,
\ misspelled key, duplicate preceding name, extra tensor, or header padding.

require lib/prelude.f
require lib/adt/option.f
require lib/adt/result.f
require lib/string.f
require lib/fs.f
require lib/test.f
require test/checker-assert.f
require maki/infer/gpt2-load.f

package GPT2LOAD

using GPT2TENSOR
using MDLCFG-ARCH
using MDLCFG
using SAFET-MAP
using SAFET
using WSTORE
using MDLCFG-CFGKEY

\ ---- the tiny fixture geometry -------------------------------------------------
2 constant LAYER-COUNT                                \ layer count
4 constant EMBED-SIZE                                \ nembd
2 constant HEAD-COUNT                                \ nhead
8 constant CONTEXT-SIZE                                \ nctx
5 constant VOCAB-SIZE                                \ nvocab
30 constant TENSOR-COUNT                              \ 4 + 13*layer count

4 constant F32-BYTES                            \ F32 element size
2 constant BF16-BYTES                           \ the wrong-element-type variant's size

\ ---- corruption knobs ----------------------------------------------------------
0 constant DAMAGE-NONE
1 constant DAMAGE-ELEMENT-TYPE                          \ emit BF16 where the role wants F32
2 constant DAMAGE-RANK                           \ emit one extra trailing 1 dim
3 constant DAMAGE-SHAPE                          \ emit d0+1
4 constant DAMAGE-NAME                            \ misspell the key
5 constant DAMAGE-EXTRA                          \ emit one tensor beyond the parsed tensor index
6 constant DAMAGE-DUPLICATE-NAME                 \ emit the previous slot's key on this row
7 constant DAMAGE-HEADER-PADDING                            \ prepend __metadata__, shifting every offset

-5698 constant E-BAD-FIXTURE                          \ fixture invariant broke (never expected)

variable DAMAGE-SLOT                            \ which slot to damage, -1 for none
variable DAMAGE-KIND

\ ---- buffers -------------------------------------------------------------------
$8000 constant IMAGE-CAP
$4000 constant JSON-CAP
64 constant ROLE-NAME-CAPACITY

create IMAGE-BUFFER IMAGE-CAP allot
create JSON-BUFFER JSON-CAP allot
create ROLE-NAME-BUFFER ROLE-NAME-CAPACITY allot

variable JSON-LENGTH                                \ bytes of header JSON emitted
variable DATA-OFFSET                                \ running data-section offset
variable IMAGE-LENGTH
variable CAPTURED-KEY0  variable CAPTURED-KEY1
variable CAPTURED-KEY2  variable CAPTURED-KEY3                \ one owner's captured configuration key cells
variable CONFIG-KEY0  variable CONFIG-KEY1
variable CONFIG-KEY2  variable CONFIG-KEY3                \ that configuration's own configuration key cells
variable START-MAPPING-COUNT                            \ leak-counter baselines at suite entry
variable START-OWNER-COUNT
variable START-STORE-COUNT
variable START-PREPARED-COUNT

34 constant JSON-QUOTE
44 constant JSON-COMMA
58 constant JSON-COLON
91 constant JSON-OPEN-BRACKET
93 constant JSON-CLOSE-BRACKET
120 constant JSON-LOWER-X
123 constant JSON-OPEN-BRACE
125 constant JSON-CLOSE-BRACE
48 constant DIGIT-ZERO

\ ---- JSON emission -------------------------------------------------------------
: JSON-APPEND ( ptr u8 n -- )
   dup >r
   JSON-BUFFER JSON-LENGTH @ + swap BYTE-COPY
   JSON-LENGTH @ r> + JSON-LENGTH ! ;

: JSON-CHAR ( n -- )
   JSON-BUFFER JSON-LENGTH @ + c!
   JSON-LENGTH @ 1 + JSON-LENGTH ! ;

: JSON-NUMBER ( n -- )                               \ nonnegative decimal
   dup 10 < if DIGIT-ZERO + JSON-CHAR exit then
   dup 10 / RECURSE
   10 mod DIGIT-ZERO + JSON-CHAR ;

: JSON-STRING ( ptr u8 n -- )                      \ "text"
   JSON-QUOTE JSON-CHAR  JSON-APPEND  JSON-QUOTE JSON-CHAR ;

: JSON-MEMBER ( ptr u8 n -- )                      \ "text":
   JSON-STRING JSON-COLON JSON-CHAR ;

\ ---- the role's own name and shape, straight from GPT2TENSOR ---------------------
: ROLE-NAME-LENGTH ( MDLCFG:mcfg n -- MDLCFG:mcfg n )
   TENSOR-ID-FOR-SLOT ROLE-NAME-BUFFER ROLE-NAME-CAPACITY GPT2TENSOR:COPY-NAME? E-NAME-TOO-LONG REQUIRE-VALUE ;

: ROLE-SHAPE ( MDLCFG:mcfg n -- MDLCFG:mcfg n n n n n )
   TENSOR-ID-FOR-SLOT SHAPE ;

\ ---- emitted dims: the role's shape with this slot's damage applied ------------
: DAMAGED? ( n -- bool ) {: slot:n :}
   DAMAGE-SLOT @ slot = ;

: DAMAGE-KIND? ( n n -- bool ) {: slot:n kind:n :}
   slot DAMAGED?  DAMAGE-KIND @ kind =  and ;

: EMIT-RANK ( n n -- n ) {: slot:n rank:n :}
   slot DAMAGE-RANK DAMAGE-KIND? if rank 1 + else rank then ;

: EMIT-FIRST-DIM ( n n -- n ) {: slot:n d0:n :}
   slot DAMAGE-SHAPE DAMAGE-KIND? if d0 1 + else d0 then ;

: ELEM-BYTES ( n -- n ) {: slot:n :}
   slot DAMAGE-ELEMENT-TYPE DAMAGE-KIND? if BF16-BYTES else F32-BYTES then ;

: ELEMENT-TYPE-NAME ( n -- ptr u8 n ) {: slot:n :}
   slot DAMAGE-ELEMENT-TYPE DAMAGE-KIND? if s" BF16" else s" F32" then ;

\ ---- one parsed tensor index row ------------------------------------------------------------
\ Answers the element count of what it emitted, so the byte extent always matches
\ the dims the parser will read back.
: JSON-DIMENSIONS ( n n n n n -- n )
   {: rank:n d0:n d1:n d2:n d3:n :}
   JSON-OPEN-BRACKET JSON-CHAR
   d0 JSON-NUMBER
   rank 1 > if JSON-COMMA JSON-CHAR d1 JSON-NUMBER then
   rank 2 > if JSON-COMMA JSON-CHAR d2 JSON-NUMBER then
   rank 3 > if JSON-COMMA JSON-CHAR d3 JSON-NUMBER then
   rank 4 > if JSON-COMMA JSON-CHAR 1 JSON-NUMBER then    \ the rank damage's extra trailing 1
   JSON-CLOSE-BRACKET JSON-CHAR
   d0 d1 * d2 * d3 * ;

: JSON-SPAN ( n -- )                            \ "data_offsets":[begin,end]
   {: nb:n :}
   s" data_offsets" JSON-MEMBER
   JSON-OPEN-BRACKET JSON-CHAR
   DATA-OFFSET @ JSON-NUMBER
   JSON-COMMA JSON-CHAR
   DATA-OFFSET @ nb + JSON-NUMBER
   JSON-CLOSE-BRACKET JSON-CHAR
   DATA-OFFSET @ nb + DATA-OFFSET ! ;

: JSON-TENSOR ( n n n n n n -- )                  \ slot rank d0 d1 d2 d3
   {: slot:n rank:n d0:n d1:n d2:n d3:n :}
   JSON-OPEN-BRACE JSON-CHAR
   s" dtype" JSON-MEMBER  slot ELEMENT-TYPE-NAME JSON-STRING  JSON-COMMA JSON-CHAR
   s" shape" JSON-MEMBER
   slot rank EMIT-RANK  slot d0 EMIT-FIRST-DIM  d1 d2 d3 JSON-DIMENSIONS
   slot ELEM-BYTES *                            \ ( byte extent of what was emitted )
   JSON-COMMA JSON-CHAR
   JSON-SPAN
   JSON-CLOSE-BRACE JSON-CHAR ;

\ The misspelled variant appends one byte to the rendered name, so the parsed tensor index holds
\ a key no role can ever ask for and the role's own key is absent.
: JSON-KEY ( n n -- ) {: slot:n name-len:n :}
   slot DAMAGE-NAME DAMAGE-KIND? if
      JSON-LOWER-X ROLE-NAME-BUFFER name-len + c!
      ROLE-NAME-BUFFER name-len 1 + JSON-STRING
   else
      ROLE-NAME-BUFFER name-len JSON-STRING
   then
   JSON-COLON JSON-CHAR ;

\ Which slot's key this row actually carries. The duplicate-name variant gives a
\ row the previous slot's name, leaving this row's required name absent. PREPARE
\ therefore rejects it during name lookup. A two-role claim collision depends on
\ the role vocabulary itself and is tested directly in gpt2-prepare-test.f.
: EMIT-NAME-SLOT ( n -- n ) {: slot:n :}
   slot DAMAGE-DUPLICATE-NAME DAMAGE-KIND? if slot 1 - else slot then ;

: JSON-ROW ( MDLCFG:mcfg n -- MDLCFG:mcfg ) {: slot:n :}
   slot 0 > if JSON-COMMA JSON-CHAR then
   slot EMIT-NAME-SLOT ROLE-NAME-LENGTH {: name-len:n :}
   slot name-len JSON-KEY
   slot ROLE-SHAPE {: rank:n d0:n d1:n d2:n d3:n :}
   slot rank d0 d1 d2 d3 JSON-TENSOR ;

\ one tensor beyond the parsed tensor index: well-formed, but a name no role owns
: JSON-EXTRA ( -- )
   JSON-COMMA JSON-CHAR
   s" surplus.weight" JSON-MEMBER
   JSON-OPEN-BRACE JSON-CHAR
   s" dtype" JSON-MEMBER  s" F32" JSON-STRING  JSON-COMMA JSON-CHAR
   s" shape" JSON-MEMBER  JSON-OPEN-BRACKET JSON-CHAR  1 JSON-NUMBER  JSON-CLOSE-BRACKET JSON-CHAR
   JSON-COMMA JSON-CHAR
   F32-BYTES JSON-SPAN
   JSON-CLOSE-BRACE JSON-CHAR ;

\ ---- image assembly ------------------------------------------------------------
: SET-HEADER-LENGTH ( n -- ) {: hl:n :}                   \ 8-byte little-endian header length
   8 0 ?do  hl i 8 * rshift $FF and  IMAGE-BUFFER i + c!  loop ;

\ Every data byte is a function of its POSITION in the data section, and never zero.
\ That is load-bearing for every byte comparison these suites make: with a constant fill
\ - zeros, as this emitter used to write - a span probe compares zeros with zeros, so it
\ holds for a wrong copy-buffer offset, extent, or tensor ID, and the
\ assertion proves only that both sides are the same length. With a position-dependent
\ pattern, reading one byte from the wrong place is visible. Never zero, so a span that
\ was never written at all cannot pass either.
: PATTERN-BYTE ( n -- n ) {: i:n :}
   i 31 * 7 + 251 mod 1 + ;

: FILL-PATTERN ( n n -- ) {: base:n nb:n :}
   nb 0 ?do  i PATTERN-BYTE  IMAGE-BUFFER base i + +  c!  loop ;

\ A `__metadata__` member, which the loader recognises and SKIPS: it loads no row and
\ is not counted as a tensor. Emitting it therefore lengthens the header and nothing
\ else, and every tensor's mapping offset is 8 + header length + its data-section
\ begin - so this variant shifts EVERY row's offset while leaving the tensor count, the
\ dtypes, the shapes, the extents and the prefix sum exactly as they were. That is the
\ interference a load has to survive, and the one an aggregates-only check cannot see.
: JSON-PAD ( -- )
   s" __metadata__" JSON-MEMBER
   JSON-OPEN-BRACE JSON-CHAR
   s" pad" JSON-MEMBER  s" shift-every-mapping-offset" JSON-STRING
   JSON-CLOSE-BRACE JSON-CHAR
   JSON-COMMA JSON-CHAR ;

: ASSEMBLE-IMAGE ( -- )
   JSON-LENGTH @ {: hl:n :}
   hl SET-HEADER-LENGTH
   JSON-BUFFER IMAGE-BUFFER 8 + hl BYTE-COPY
   8 hl + DATA-OFFSET @ FILL-PATTERN
   8 hl + DATA-OFFSET @ + IMAGE-LENGTH ! ;

: BUILD-IMAGE ( MDLCFG:mcfg -- MDLCFG:mcfg )
   0 JSON-LENGTH !  0 DATA-OFFSET !
   JSON-OPEN-BRACE JSON-CHAR
   DAMAGE-KIND @ DAMAGE-HEADER-PADDING = if JSON-PAD then
   TENSOR-COUNT 0 ?do  i JSON-ROW  loop
   DAMAGE-KIND @ DAMAGE-EXTRA = if JSON-EXTRA then
   JSON-CLOSE-BRACE JSON-CHAR
   ASSEMBLE-IMAGE ;

: FIXTURE-PATH ( -- ptr u8 n )  s" /tmp/hb-gpt2-synth.safetensors" ;

\ The offset-shifted variant lives at a SECOND path deliberately: the first load's
\ parsed tensor index holds a live mapping of FIXTURE-PATH, and rewriting those bytes underneath it would
\ change what the FIRST parsed tensor index reads - the one thing the interference fixture must not do.
: SHIFTED-PATH ( -- ptr u8 n )  s" /tmp/hb-gpt2-shift.safetensors" ;

\ The pinned real checkpoint for the presence-gated tests in the two load suites.
: REAL-CHECKPOINT-PATH ( -- ptr u8 n )  s" gpt2-model/model.safetensors" ;

: DELETE-FIXTURES ( -- )
   FIXTURE-PATH FS-PATHZ unlink drop
   SHIFTED-PATH FS-PATHZ unlink drop ;

: RESET-FIXTURE ( -- )
   -1 DAMAGE-SLOT !  DAMAGE-NONE DAMAGE-KIND ! ;

: SET-DAMAGE ( n n -- ) {: slot:n kind:n :}
   slot DAMAGE-SLOT !  kind DAMAGE-KIND ! ;

\ ---- fixture configurations (all through the sole MDLCFG constructor) ----------
: MODEL-ELEMENT-TYPE ( -- MAKI:dtype )  MAKI-DTYPE:DF32 ;
: NORM-EPSILON ( -- r )  0.00001 ;

: MATCHING-CONFIG ( -- MDLCFG:mcfg )
   NORM-EPSILON true GPT2
   1 MODEL-ELEMENT-TYPE CONTEXT-SIZE VOCAB-SIZE LAYER-COUNT EMBED-SIZE HEAD-COUNT true 4 4 BUILD ;

\ alternate identity: same geometry, one field invisible to the parsed tensor index is flipped
: OTHER-IDENTITY-CONFIG ( -- MDLCFG:mcfg )
   NORM-EPSILON true GPT2
   1 MODEL-ELEMENT-TYPE CONTEXT-SIZE VOCAB-SIZE LAYER-COUNT EMBED-SIZE HEAD-COUNT false 4 4 BUILD ;

\ one more layer: the parsed tensor index count no longer matches
: EXTRA-LAYER-CONFIG ( -- MDLCFG:mcfg )
   NORM-EPSILON true GPT2
   1 MODEL-ELEMENT-TYPE CONTEXT-SIZE VOCAB-SIZE LAYER-COUNT 1 + EMBED-SIZE HEAD-COUNT true 4 4 BUILD ;

\ same parsed tensor index count, every embedding-shaped tensor twice as wide
: WIDE-EMBEDDING-CONFIG ( -- MDLCFG:mcfg )
   NORM-EPSILON true GPT2
   1 MODEL-ELEMENT-TYPE CONTEXT-SIZE VOCAB-SIZE LAYER-COUNT EMBED-SIZE 2 * HEAD-COUNT true 4 4 BUILD ;

\ the real 124M geometry for the presence-gated tests
: REAL-MODEL-CONFIG ( -- MDLCFG:mcfg )
   NORM-EPSILON true GPT2
   1 MODEL-ELEMENT-TYPE 1024 50257 12 768 12 true 50256 50256 BUILD ;

: WRITE-FIXTURE ( MDLCFG:mcfg -- MDLCFG:mcfg )
   BUILD-IMAGE
   FIXTURE-PATH IMAGE-BUFFER IMAGE-LENGTH @ WRITE-ALL ;

: WRITE-CHECKPOINT ( -- )                                 \ write the current variant to disk
   MATCHING-CONFIG WRITE-FIXTURE drop ;

: WRITE-WIDE-CHECKPOINT ( -- )                            \ the wider model, at the second path
   WIDE-EMBEDDING-CONFIG BUILD-IMAGE
   SHIFTED-PATH IMAGE-BUFFER IMAGE-LENGTH @ WRITE-ALL
   drop ;

: WRITE-SHIFTED-CHECKPOINT ( -- )
   DAMAGE-HEADER-PADDING DAMAGE-KIND !
   MATCHING-CONFIG BUILD-IMAGE
   SHIFTED-PATH IMAGE-BUFFER IMAGE-LENGTH @ WRITE-ALL
   drop
   DAMAGE-NONE DAMAGE-KIND ! ;

\ ---- assertion helpers ---------------------------------------------------------
: FAIL-MISSING-OPTION ( -- )
   s" required option was NONE" T-LABEL
   0 0= 0= TTRUE ;

: OPTION-VALUE ( option<n> -- n )
   MATCH option
      none OF FAIL-MISSING-OPTION -1 ENDOF
      some OF ENDOF
   ;MATCH ;

: EXPECT-DETACHED-MAPPING ( SAFET:map-take -- SAFET:mapping )
   MATCH SAFET:map-take
      moved OF ENDOF
      empty OF E-NO-CHECKPOINT-BYTES throw ENDOF
   ;MATCH ;

\ ---- leak accounting, as a delta against a suite's own entry -------------------
\ The four counters are process-wide, and a combined run reaches these suites after
\ other suites with known counter deltas (weight-store-test.f ends on 2
\ live WSTORE blocks and 0 SAFET owners: only its two SEAL-refusal builders remain).
\ Asserting absolute zero would
\ therefore pass standalone and fail in maki/test.f while saying nothing about the
\ suite under test. Every assertion below is a delta from the entry baseline, so it
\ measures exactly what these fixtures took and gave back.
: SAVE-BASELINE ( -- )
   SAFET-MAP:LIVE START-MAPPING-COUNT !
   LIVE-OWNERS START-OWNER-COUNT !
   WSTORE:LIVE START-STORE-COUNT !
   LIVE-PREPARED-LOADS START-PREPARED-COUNT ! ;

: EXPECT-NO-LEAK ( -- )                             \ every owner and mapping given back
   SAFET-MAP:LIVE START-MAPPING-COUNT @ T=
   LIVE-OWNERS START-OWNER-COUNT @ T=
   WSTORE:LIVE START-STORE-COUNT @ T=
   LIVE-PREPARED-LOADS START-PREPARED-COUNT @ T= ;

: EXPECT-PREPARED-RESOURCES ( -- )                           \ exactly one prepared load is live
   SAFET-MAP:LIVE START-MAPPING-COUNT @ 1 + T=
   LIVE-OWNERS START-OWNER-COUNT @ 1 + T=
   WSTORE:LIVE START-STORE-COUNT @ 1 + T=
   LIVE-PREPARED-LOADS START-PREPARED-COUNT @ 1 + T= ;

\ What a loaded model owns: the prepared-load block is gone, and the table block and the
\ checkpoint mapping have moved into the store the model holds.
: EXPECT-MAPPED-MODEL-RESOURCES ( -- )
   SAFET-MAP:LIVE START-MAPPING-COUNT @ 1 + T=
   LIVE-OWNERS START-OWNER-COUNT @ 1 + T=
   WSTORE:LIVE START-STORE-COUNT @ 1 + T=
   LIVE-PREPARED-LOADS START-PREPARED-COUNT @ T= ;

\ What RETURN-TENSOR-INDEX leaves: the prepared-load block and sealed table are
\ gone, and the parsed tensor index is still whole - still an owner, still holding its mapping. It is
\ the one state where the four counters disagree with each other, which is exactly why
\ it is worth asserting: a RETURN-TENSOR-INDEX that forgot the table would satisfy EXPECT-PREPARED-RESOURCES'
\ parsed-tensor-index counters in both states, and only the WSTORE count distinguishes them.
: EXPECT-TENSOR-INDEX-RESOURCES ( -- )
   SAFET-MAP:LIVE START-MAPPING-COUNT @ 1 + T=
   LIVE-OWNERS START-OWNER-COUNT @ 1 + T=
   WSTORE:LIVE START-STORE-COUNT @ T=
   LIVE-PREPARED-LOADS START-PREPARED-COUNT @ T= ;

: ASSERT-BYTES-EQUAL ( ptr u8 n ptr u8 n -- )
   STR= TTRUE ;

\ The ok payload of a release outcome. Reading it is the difference between "the exit
\ reported success" and "the exit gave back the bytes it was holding".
: RESULT-VALUE ( result<n,n> -- n )
   MATCH result
      ok  OF ENDOF
      err OF
         s" a release reported err, code" T-LABEL
         . cr
         0 0= 0= TTRUE
         -1
      ENDOF
   ;MATCH ;

: EXPECT-CHECKER-REJECTION ( ptr u8 n -- )
   CHECK-QUIET-CANDIDATE! 0 T= ;

: EXPECT-CHECKER-ACCEPTANCE ( ptr u8 n -- )
   CHECK-QUIET-CANDIDATE! -1 T= ;

\ Verdict 1 is "the dictionary cannot resolve this token at all", which is how a
\ package-private word looks from outside; it is kept apart from verdict 0, a real
\ type error, so "private" is proved by non-resolution rather than by any accident.
: EXPECT-PRIVATE-NAME ( ptr u8 n -- )
   CHECK-QUIET-CANDIDATE! 1 T= ;

\ ---- the captured identity, read from a configuration and from a loaded model ----
\ Slot 7 is layer 0's c_attn Conv1D weight, the [in,out] matrix - not a global and not
\ a plain vector, so it is a row that would move first if the slot walk drifted. Both
\ the PREPARE suite and the copied-storage path probe it.
7 constant ATTENTION-WEIGHT-SLOT                        \ h.0.attn.c_attn.weight (Conv1D)

: SAVE-CONFIG-KEY ( MDLCFG:mcfg -- )                \ save a configuration's own key cells
   CFGKEY@ UNMAKE {: k0:n k1:n k2:n k3:n :}
   drop
   k0 CONFIG-KEY0 !  k1 CONFIG-KEY1 !  k2 CONFIG-KEY2 !  k3 CONFIG-KEY3 ! ;

: SAVE-MODEL-CONFIG-KEY ( GPT2LOAD:gpt2-model -- GPT2LOAD:gpt2-model )
   MODEL-CONFIG-KEY UNMAKE {: k0:n k1:n k2:n k3:n :}
   k0 CAPTURED-KEY0 !  k1 CAPTURED-KEY1 !  k2 CAPTURED-KEY2 !  k3 CAPTURED-KEY3 ! ;

\ Cell by cell, in order. A reversed or rotated capture would still differ from the
\ other configuration's and still pass a difference test, so difference alone proves nothing about
\ correctness; this is the assertion that pins WHICH key was captured.
: KEY-MATCHES-CONFIG ( -- )
   CAPTURED-KEY0 @ CONFIG-KEY0 @ T=
   CAPTURED-KEY1 @ CONFIG-KEY1 @ T=
   CAPTURED-KEY2 @ CONFIG-KEY2 @ T=
   CAPTURED-KEY3 @ CONFIG-KEY3 @ T= ;

;using
;using
;using
;using
;using
;using
;using

;package
