\ safetensors.f - native safetensors weight loader for the GB10 UMA inference
\ engine (package SAFET; epic habu-epic-gb10-uma-391d12e8, phase 1).
\
\ FORMAT. A safetensors file is: an 8-byte little-endian u64 HEADER LENGTH, then
\ that many bytes of a single JSON object mapping each tensor NAME -> a small
\ object {"dtype": <wire>, "shape": [d0,d1,...], "data_offsets": [begin,end]},
\ then the raw tensor bytes. Each tensor's bytes are the half-open data-section
\ range [begin,end); offsets are relative to the START of the data section (the
\ byte just past the header), NOT the file. An optional "__metadata__" member
\ carries free-form strings and is skipped.
\
\ ZERO-COPY ON UMA. The loader mmaps the file read-only (MAP_PRIVATE) and
\ registers each tensor as a TYPED SPAN over the mapping: (dtype, shape, data
\ pointer = data-section-base + begin, byte length). No tensor bytes are copied -
\ on the GB10's coherent unified memory the mapped pages are already GPU-visible,
\ so the registry hands the forward pass pointers straight into the mapping. Only
\ the small tensor NAMES are copied out (into a private arena) so lookups survive
\ independent of the shared JSON cursor.
\
\ FAIL-CLOSED, NO PARTIAL REGISTRATION. Every malformed input throws a named
\ E-ST-* code and leaves COUNT at 0: tensors are staged into the registry during
\ the parse and only PUBLISHED (COUNT := staged) after the entire header
\ validates. A throw anywhere aborts with COUNT unchanged. A parse failure after
\ mapping unmaps before rethrowing (LOAD), so a rejected file leaks nothing.
\ Structural JSON faults from the streaming reader (lib/json-read.f, E-JR-*) are
\ remapped to E-ST-JSON so the loader presents one error surface.
\
\ STREAMING PARSE (design note). The header is parsed with the zero-allocation
\ pull reader lib/json-read.f, NOT the DOM tool tools/json.f: a DOM materialises
\ one node per JSON value and its fixed pool (JSON-MAX-NODES = 1024) OVERFLOWS on
\ real models - GPT-2 124M has 160 tensors -> ~1209 value nodes - and the node
\ count grows with tensor count without bound, so no fixed DOM cap scales. The
\ pull reader holds only the current token, so it parses any model size in O(1)
\ memory. Members are dispatched by key (order-independent, tolerant of unknown
\ keys), which is the robust long-term shape.
\
\ MODEL CONFIG IS SEPARATE (contract). This loader parses the TENSOR MANIFEST
\ only - names, dtypes, shapes, byte spans. The normalized model config
\ (n_layer / n_head / n_embd / n_vocab / n_ctx) is NOT in the safetensors header;
\ it is a separate artifact (HF config.json) parsed by a separate path and must
\ never be conflated with tensor data. The optional "__metadata__" member is
\ free-form and is SKIPPED, not interpreted as config.
\
\ TENSOR ORIENTATION - ORIGINAL vs PACKED (for the forward + model-pack dots).
\ Tensors are registered in their ORIGINAL on-disk orientation. HuggingFace GPT-2
\ stores its attention/MLP projection weights as `Conv1D`, whose weight is
\ [in_features, out_features] - the TRANSPOSE of a `nn.Linear` weight
\ [out_features, in_features]. Example from openai-community/gpt2: attn.c_attn
\ shape is [768, 2304] (= [n_embd, 3*n_embd]) and mlp.c_fc is [768, 3072], i.e.
\ [in, out]. y = x @ W directly (no transpose), whereas a Linear layer computes
\ y = x @ W^T. The forward pass must NOT transpose Conv1D weights. LayerNorm/bias
\ vectors and the wte [50257,768] / wpe [1024,768] embeddings are unaffected. A
\ later PACKED layout (model-pack dot) would materialise the Linear-oriented /
\ quantized form once; this loader always exposes the original orientation.
\
\ RESIDENCY (contract; measured in docs/gb10-uma-residency.md). A cuMemAlloc
\ device buffer reads at ~273 GB/s vs ~160 GB/s for a directly-mmap'd host pointer
\ on the GB10, so the residency layer copies weights into device buffers when they
\ fit; it MUST then UNMAP the source (see UNMAP) so two full checkpoints are never
\ resident at once, keeping peak load memory bounded (model size + one tensor).
\
\ ABSOLUTE ALIGNMENT (design note). safetensors packs tensor data contiguously
\ with no padding, so each begin is a multiple of its element size RELATIVE to
\ the data section (enforced: E-ST-ALIGN), but the data section itself starts at
\ file byte 8+header_len, which is generally NOT element-aligned (GPT-2: 14291,
\ so wte's absolute pointer is 3 bytes off a 4-byte boundary). The forward pass /
\ residency layer must treat mapped tensor pointers as byte-aligned, not
\ element-aligned - see docs/gb10-uma-residency.md.

require lib/errors.f                     \ E-FS-*, E-MEM-MAP, E-JR-FIRST/E-JR-LAST
require lib/fs.f                          \ FILE-SIZE, FS-PATHZ, FS-U64@, EXISTS?, FILE?
require lib/string.f                      \ STR=, BYTE+, BYTE-COPY
require lib/json-read.f                   \ JR: streaming JSON pull reader

\ ---- named throw codes (owned by this module; block -7600..-7607) ----------
-7600 constant E-ST-HEADER    \ file < 8 bytes, or 8+header_len exceeds the file, or header_len <= 0
-7601 constant E-ST-JSON      \ header JSON malformed, not an object, or a value not an object
-7602 constant E-ST-DTYPE     \ unknown / unsupported dtype wire string
-7603 constant E-ST-SHAPE     \ shape not an array, negative dim, rank overflow, or byte-size mismatch
-7604 constant E-ST-OFFSETS   \ data_offsets not [begin,end], begin>end, out of range, or overlapping
-7605 constant E-ST-ALIGN     \ begin not a multiple of the element size
-7606 constant E-ST-FIELD     \ a tensor object is missing dtype / shape / data_offsets
-7607 constant E-ST-CAP       \ too many tensors, or name arena exhausted

package SAFET

public
\ ---- dtype codes (numerically match maki/tensor.f MAKI:DT-* for the intersecting
\ set, so the forward dot maps SAFET:DTYPE@ -> MAKI dtype by value; the loader is
\ the authority on the safetensors wire dtype and may support a superset) --------
0 constant ST-DT-F32
1 constant ST-DT-F16
2 constant ST-DT-BF16
3 constant ST-DT-U32
4 constant ST-DT-I32
private

8    constant ST-MAX-RANK      \ max tensor rank the registry records
2048 constant ST-CAP           \ max tensors (GPT-2: 160; LLaMA-70B: ~700)
$40000 constant ST-NAME-CAP    \ name arena bytes (256 KiB)
1024 constant ST-SCRATCH-CAP   \ per-name unescape scratch

\ ---- mapping + header geometry ---------------------------------------------
variable ST-BASE-P             \ mapping base as a byte pointer (ptr-field holder)
variable ST-BASE-N             \ mapping base as a numeric address (data-pointer arithmetic)
variable ST-MAP-LEN            \ mapped byte length (= file size)
variable ST-HDR-LEN            \ JSON header byte length (the leading u64)
variable ST-DATA-BASE-N        \ numeric address of the data section (base + 8 + header_len)
variable ST-DATA-LEN           \ data-section byte length (map_len - 8 - header_len)

\ ---- registry (parallel arrays, one row per tensor) ------------------------
variable ST-N                  \ PUBLISHED tensor count (0 until a whole header validates)
variable ST-STAGE              \ staged rows during a parse
create ST-NM-OFF  ST-CAP cells allot           \ name offset into the arena
create ST-NM-LEN  ST-CAP cells allot           \ name length
create ST-DTY     ST-CAP cells allot           \ dtype code (ST-DT-*)
create ST-RANK    ST-CAP cells allot           \ tensor rank
create ST-DATA    ST-CAP cells allot           \ tensor data pointer (numeric address)
create ST-NB      ST-CAP cells allot           \ tensor byte length
create ST-BEG     ST-CAP cells allot           \ data_offsets begin (for overlap checks)
create ST-END     ST-CAP cells allot           \ data_offsets end
create ST-DIMS    ST-CAP ST-MAX-RANK * cells allot   \ dims, row-major by tensor

create ST-NAME-ARENA ST-NAME-CAP allot         \ copied tensor names
variable ST-NM-BUMP                            \ arena bump cursor
create ST-SCRATCH ST-SCRATCH-CAP allot         \ name unescape scratch

\ ---- per-tensor parse accumulators -----------------------------------------
variable ST-CUR-NMOFF variable ST-CUR-NMLEN
variable ST-CUR-DT    variable ST-CUR-ES       \ dtype code + element size
variable ST-CUR-RK
create   ST-CUR-DIMS ST-MAX-RANK cells allot
variable ST-CUR-BEG   variable ST-CUR-END
variable ST-DT-SEEN   variable ST-SH-SEEN variable ST-OFF-SEEN
variable ST-KA        variable ST-KL         \ stashed member-key span
variable ST-OVL

\ ---- pointer reinterprets (audited soundness boundary; the mmap primitive and
\ the registry cells carry raw addresses the checker cannot type) ------------
TRUSTED: ST-N>PTR ( n -- ptr u8 ) ;            \ numeric address -> byte pointer
TRUSTED: ST-PTR>N ( ptr u8 -- n ) ;            \ byte pointer -> numeric address

: ST-BASE-FIELD ( -- ptr ptr u8 )  ST-BASE-P 0 ptr-field ;
: ST-BASE@ ( -- ptr u8 )  ST-BASE-FIELD @ ;
: ST-BASE! ( ptr u8 -- )  ST-BASE-FIELD ! ;

\ ---- error remap: JR structural faults -> one loader surface ---------------
: ST-JR-ERR? ( n -- bool )
   dup E-JR-LAST >= swap E-JR-FIRST <= and ;   \ code in [-3999,-3900]

: ST-REMAP ( n -- )
   dup 0= if drop exit then
   dup ST-JR-ERR? if drop E-ST-JSON throw then
   throw ;

\ ---- dtype wire decode: set ST-CUR-DT + ST-CUR-ES --------------------------
: ST-DECODE-DTYPE ( ptr u8 n -- )
   2dup s" F32"  STR= if 2drop ST-DT-F32  ST-CUR-DT ! 4 ST-CUR-ES ! exit then
   2dup s" F16"  STR= if 2drop ST-DT-F16  ST-CUR-DT ! 2 ST-CUR-ES ! exit then
   2dup s" BF16" STR= if 2drop ST-DT-BF16 ST-CUR-DT ! 2 ST-CUR-ES ! exit then
   2dup s" I32"  STR= if 2drop ST-DT-I32  ST-CUR-DT ! 4 ST-CUR-ES ! exit then
   2dup s" U32"  STR= if 2drop ST-DT-U32  ST-CUR-DT ! 4 ST-CUR-ES ! exit then
   2drop E-ST-DTYPE throw ;

: ST-KEY= ( ptr u8 n -- bool )                 \ literal vs the stashed member key
   ST-KA @ ST-KL @ STR= ;

\ ---- member value readers (cursor at the key; advance past its value) -------
: ST-DO-DTYPE ( -- )
   JR:NEXT JR:T-STR <> if E-ST-DTYPE throw then
   JR:SPAN$ ST-DECODE-DTYPE
   1 ST-DT-SEEN ! ;

: ST-DO-SHAPE ( -- )
   JR:NEXT JR:T-ARR <> if E-ST-SHAPE throw then
   0 ST-CUR-RK !
   begin JR:NEXT dup JR:T-ARR-END <> while
      dup JR:T-INT <> if E-ST-SHAPE throw then drop
      JR:INT dup 0 < if E-ST-SHAPE throw then
      ST-CUR-RK @ ST-MAX-RANK >= if E-ST-SHAPE throw then
      ST-CUR-DIMS ST-CUR-RK @ cells + !
      ST-CUR-RK @ 1+ ST-CUR-RK !
   repeat drop
   1 ST-SH-SEEN ! ;

: ST-DO-OFFSETS ( -- )
   JR:NEXT JR:T-ARR <> if E-ST-OFFSETS throw then
   JR:NEXT dup JR:T-INT <> if E-ST-OFFSETS throw then drop JR:INT ST-CUR-BEG !
   JR:NEXT dup JR:T-INT <> if E-ST-OFFSETS throw then drop JR:INT ST-CUR-END !
   JR:NEXT JR:T-ARR-END <> if E-ST-OFFSETS throw then   \ exactly two elements
   1 ST-OFF-SEEN ! ;

\ ---- validation: fill nothing new, just check the accumulators --------------
: ST-NELEMS ( -- n )
   1 ST-CUR-RK @ 0 ?do ST-CUR-DIMS i cells + @ * loop ;

: ST-OVERLAPS? ( n n -- bool )
   {: b e :}
   0 ST-OVL !
   ST-STAGE @ 0 ?do
      b ST-END i cells + @ <  ST-BEG i cells + @ e <  and
      if 1 ST-OVL ! leave then
   loop
   ST-OVL @ 0 <> ;

: ST-VALIDATE ( -- )
   ST-NELEMS ST-CUR-ES @ * {: expect :}
   ST-CUR-BEG @ 0 < if E-ST-OFFSETS throw then
   ST-CUR-END @ ST-CUR-BEG @ < if E-ST-OFFSETS throw then
   ST-CUR-END @ ST-DATA-LEN @ > if E-ST-OFFSETS throw then
   ST-CUR-BEG @ ST-CUR-ES @ mod 0 <> if E-ST-ALIGN throw then
   ST-CUR-END @ ST-CUR-BEG @ - expect <> if E-ST-SHAPE throw then
   ST-CUR-BEG @ ST-CUR-END @ ST-OVERLAPS? if E-ST-OFFSETS throw then ;

\ ---- one tensor value object: parse members, validate ----------------------
: ST-TENSOR ( -- )                             \ cursor at the value T-OBJ
   0 ST-DT-SEEN ! 0 ST-SH-SEEN ! 0 ST-OFF-SEEN ! 0 ST-CUR-RK !
   begin JR:NEXT dup JR:T-OBJ-END <> while
      dup JR:T-KEY <> if E-ST-JSON throw then drop
      JR:SPAN$ ST-KL ! ST-KA !
      s" dtype"        ST-KEY= if ST-DO-DTYPE   else
      s" shape"        ST-KEY= if ST-DO-SHAPE   else
      s" data_offsets" ST-KEY= if ST-DO-OFFSETS else
         JR:NEXT drop JR:SKIP-VALUE
      then then then
   repeat drop
   ST-DT-SEEN  @ 0= if E-ST-FIELD throw then
   ST-SH-SEEN  @ 0= if E-ST-FIELD throw then
   ST-OFF-SEEN @ 0= if E-ST-FIELD throw then
   ST-VALIDATE ;

\ ---- name copy into the arena ----------------------------------------------
: ST-READ-NAME ( -- )                          \ cursor at the member key (T-KEY)
   ST-SCRATCH ST-SCRATCH-CAP JR:STR {: nlen :}
   nlen ST-NM-BUMP @ + ST-NAME-CAP > if E-ST-CAP throw then
   ST-NM-BUMP @ ST-CUR-NMOFF !
   nlen ST-CUR-NMLEN !
   ST-SCRATCH  ST-NAME-ARENA ST-NM-BUMP @ +  nlen BYTE-COPY
   ST-NM-BUMP @ nlen + ST-NM-BUMP ! ;

: ST-CUR-META? ( -- bool )
   ST-NAME-ARENA ST-CUR-NMOFF @ +  ST-CUR-NMLEN @  s" __metadata__" STR= ;

\ ---- commit a validated tensor into the registry ---------------------------
: ST-STAGE-COMMIT ( -- )
   ST-STAGE @ ST-CAP >= if E-ST-CAP throw then
   ST-STAGE @ {: ri :}
   ST-CUR-NMOFF @  ST-NM-OFF ri cells + !
   ST-CUR-NMLEN @  ST-NM-LEN ri cells + !
   ST-CUR-DT    @  ST-DTY    ri cells + !
   ST-CUR-RK    @  ST-RANK   ri cells + !
   ST-CUR-BEG   @  ST-BEG    ri cells + !
   ST-CUR-END   @  ST-END    ri cells + !
   ST-CUR-END @ ST-CUR-BEG @ -            ST-NB   ri cells + !
   ST-DATA-BASE-N @ ST-CUR-BEG @ +        ST-DATA ri cells + !
   ST-CUR-RK @ 0 ?do
      ST-CUR-DIMS i cells + @
      ST-DIMS ri ST-MAX-RANK * i + cells + !
   loop
   ri 1+ ST-STAGE ! ;

\ ---- top-level object member iteration -------------------------------------
: ST-MEMBERS ( -- )                            \ cursor just past the root T-OBJ
   begin JR:NEXT dup JR:T-OBJ-END <> while
      dup JR:T-KEY <> if E-ST-JSON throw then drop
      ST-READ-NAME
      JR:NEXT                                  \ ( valuekind )
      ST-CUR-META? if
         drop JR:SKIP-VALUE
      else
         JR:T-OBJ <> if E-ST-JSON throw then
         ST-TENSOR
         ST-STAGE-COMMIT
      then
   repeat drop ;

: ST-PARSE-BODY ( -- )
   ST-BASE@ 8 BYTE+ ST-HDR-LEN @ JR:INIT
   JR:NEXT JR:T-OBJ <> if E-ST-JSON throw then
   ST-MEMBERS ;

public

\ ---- lifecycle -------------------------------------------------------------
: RESET ( -- )                                 \ empty the registry (no unmap)
   0 ST-N ! 0 ST-STAGE ! 0 ST-NM-BUMP ! ;

: PARSE ( -- )                                 \ parse the header at ST-BASE/ST-MAP-LEN
   0 ST-N ! 0 ST-STAGE ! 0 ST-NM-BUMP !
   ST-MAP-LEN @ 8 < if E-ST-HEADER throw then
   ST-BASE@ FS-U64@ ST-HDR-LEN !
   ST-HDR-LEN @ 0 <= if E-ST-HEADER throw then
   8 ST-HDR-LEN @ + ST-MAP-LEN @ > if E-ST-HEADER throw then
   ST-BASE-N @ 8 + ST-HDR-LEN @ + ST-DATA-BASE-N !
   ST-MAP-LEN @ 8 - ST-HDR-LEN @ - ST-DATA-LEN !
   [: ST-PARSE-BODY ;] catch ST-REMAP
   ST-STAGE @ ST-N ! ;

: PARSE-SPAN ( ptr u8 n -- )                   \ parse an in-memory image (no mmap)
   {: pa:ptr pu :}
   pa ST-BASE!
   pa ST-PTR>N ST-BASE-N !
   pu ST-MAP-LEN !
   PARSE ;

: MAP ( ptr u8 n -- )                          \ mmap a file read-only into ST-BASE/ST-MAP-LEN
   {: pa:ptr pu :}
   pa pu FILE-SIZE {: sz :}
   sz 8 < if E-ST-HEADER throw then
   pa pu FS-PATHZ 0 0 open {: fd :}
   fd 0 < if E-FS-OPEN throw then
   0 sz 1 2 fd 0 mmap {: base :}
   fd close
   base 0 < if E-MEM-MAP throw then
   base ST-BASE-N !
   base ST-N>PTR ST-BASE!
   sz ST-MAP-LEN ! ;

: UNMAP ( -- )                                 \ release the mapping (tensors invalid afterwards)
   ST-BASE-N @ 0= if exit then
   ST-BASE@ ST-MAP-LEN @ munmap drop
   0 ST-BASE-N ! 0 ST-MAP-LEN ! ;

: LOAD ( ptr u8 n -- )                         \ RESET + MAP + PARSE, unmapping on a parse fault
   RESET
   MAP
   [: PARSE ;] catch {: code :}
   code 0 <> if UNMAP code throw then ;

\ ---- accessors -------------------------------------------------------------
: COUNT ( -- n )  ST-N @ ;
: BASE ( -- ptr u8 )  ST-BASE@ ;
: MAP-LEN ( -- n )  ST-MAP-LEN @ ;
: HDR-LEN ( -- n )  ST-HDR-LEN @ ;

: NAME$ ( n -- ptr u8 n )
   {: id :}
   ST-NAME-ARENA ST-NM-OFF id cells + @ +  ST-NM-LEN id cells + @ ;

: FIND ( ptr u8 n -- n )                      \ tensor id by name, or -1
   {: qa:ptr qu :}
   -1  ST-N @ 0 ?do
      ST-NAME-ARENA ST-NM-OFF i cells + @ +  ST-NM-LEN i cells + @  qa qu STR=
      if drop i leave then
   loop ;

: DTYPE@ ( n -- n )  cells ST-DTY + @ ;
: RANK@ ( n -- n )  cells ST-RANK + @ ;
: DIM@ ( n n -- n )
   {: id axis :}  ST-DIMS id ST-MAX-RANK * axis + cells + @ ;
: DATA@ ( n -- n )  cells ST-DATA + @ ;        \ tensor data pointer as a numeric address
: DATA-PTR ( n -- ptr u8 )  DATA@ ST-N>PTR ;   \ same, as a byte pointer
: NBYTES@ ( n -- n )  cells ST-NB + @ ;
: BEGIN@ ( n -- n )  cells ST-BEG + @ ;
: END@ ( n -- n )  cells ST-END + @ ;

: PRESENT? ( ptr u8 n -- bool )                 \ true iff a readable file exists at path
   {: pa:ptr pu :}
   pa pu EXISTS? 0= if 0 0= 0= exit then
   pa pu FILE? ;

;package
