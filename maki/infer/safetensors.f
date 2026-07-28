\ safetensors.f - native safetensors weight loader for the GB10 UMA inference
\ engine (packages SAFET-MAP and SAFET; epic habu-epic-gb10-uma-391d12e8).
\
\ OWNERSHIP MODEL (the point of this module). Loading a checkpoint is an explicit
\ transaction, not a process-wide registry. SAFET:OPEN begins one isolated LOAD
\ SESSION and hands back a linear `SAFET:session` token. Every later step takes
\ that token explicitly: MAP-FILE or ADOPT gives the session its image, PARSE
\ reads and validates the header into that session's own staging area, DETACH
\ consumes a fully validated session and returns one immutable `SAFET:census`
\ that owns the mapping, and CLOSE consumes a session that will never be
\ published. No public word reads "the current load", so two sessions can be
\ parsed interleaved without cross-talk and a session that fails leaves every
\ other live session byte-identical.
\
\ MAPPED RESIDENCY (the third owner). The residency layer wants the checkpoint's
\ bytes to outlive the census that described them. DETACH-MAPPING moves the file
\ mapping into its own linear `SAFET:mapping` owner and leaves the census answering
\ metadata but no longer lending bytes. Its total `map-take` result distinguishes
\ that first move from a later `empty` attempt. Reading afterwards is MAP-OFFSET?
\ plus WITH-MAPPING; UNMAP-MAPPING ends the mapping's life. The two owners can be
\ disposed independently and in either order.
\
\ TWO OFFSET FRAMES, AND WHY BOTH EXIST. safetensors data_offsets are relative to
\ the DATA SECTION, and that is what BEGIN? / END? report, because that is what
\ the file says and what the format's own overlap and alignment rules are stated
\ in. A mapping, though, starts at file byte 0, so a tensor's first byte inside
\ the mapping is 8 + header_len + begin - which is what MAP-OFFSET? reports.
\ Handing a BEGIN? value to a WITH-MAPPING body reads 8 + header_len bytes too
\ early, i.e. inside the JSON header, and the bytes look plausible. Neither
\ reader is a substitute for the other and each says its frame in its own stack
\ comment.
\
\ The three tokens are DEFLINEAR, so the checker - not a runtime flag - enforces
\ the lifetime rules: a session, census or mapping cannot be duplicated, silently
\ dropped, stored, detached twice, closed twice, or used after it was consumed.
\ Each session owns one private metadata block and, after validation, one mapping
\ record reservation. DETACH transfers the metadata block to the census without
\ copying; the first DETACH-MAPPING moves the separate record.
\
\ NO PUBLIC WORD RETURNS A RAW POINTER. The census answers questions about
\ tensors (count, id by name, dtype, rank, dims, byte length, data-section
\ offsets) and gives access to tensor BYTES only through the scoped WITH-TENSOR
\ body or the copying COPY-DATA? / COPY-NAME? readers, so an address never
\ appears in a public output row and cannot be kept by accident.
\
\ The scoped span is ADVISORY, not enforced. Nothing stops a WITH-TENSOR or
\ WITH-MAPPING body from deliberately stashing the span into a variable and
\ reading it after RELEASE or UNMAP-MAPPING; that reads freed memory and
\ crashes, exactly as it would in any language without region types. Escaping
\ the scope takes a deliberate stash - the checker cannot yet express "this
\ pointer may not outlive this call", so the guarantee here is the shape of the
\ interface, not a proof. Closing that gap needs the pointer-lifetime /
\ region-type checker capability, tracked by the capability dot the orchestrator
\ is queuing for it; when that lands, both WITH- spans become lifetime-bounded
\ and this note goes away. The same wording applies to WITH-MAPPING deliberately:
\ its span is exactly as advisory as WITH-TENSOR's and rests on the same missing
\ capability, so it is claimed no more strongly here than it is there.
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
\ records each tensor as a TYPED SPAN over the mapping: (dtype, shape, data
\ offset = data-section-base + begin, byte length). No tensor bytes are copied -
\ on the GB10's coherent unified memory the mapped pages are already GPU-visible,
\ so WITH-TENSOR hands the forward pass a span straight into the mapping for the
\ duration of the body. Only the small tensor NAMES are copied out (into the
\ session's private arena) so lookups survive independent of the JSON cursor.
\
\ FAIL-CLOSED, NO PARTIAL PUBLICATION. Every malformed input throws a named
\ SAFET:E-* code and leaves the session unpublished: tensors are staged during
\ the parse and the published count is only written by DETACH, which refuses a
\ session whose header never validated (SAFET:E-ORDER). A throw anywhere aborts
\ with the staging area discarded on the next PARSE and the session still
\ closeable. Structural JSON faults from the streaming reader (lib/json-read.f,
\ E-JR-*) are remapped to SAFET:E-JSON so the loader presents one error surface.
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
\ Tensors are recorded in their ORIGINAL on-disk orientation. HuggingFace GPT-2
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
\ fit; it MUST then RELEASE the census that owns the source mapping so two full
\ checkpoints are never resident at once, keeping peak load memory bounded (model
\ size + one tensor).
\
\ ABSOLUTE ALIGNMENT (design note). safetensors packs tensor data contiguously
\ with no padding, so each begin is a multiple of its element size RELATIVE to
\ the data section (enforced: SAFET:E-ALIGN), but the data section itself starts
\ at file byte 8+header_len, which is generally NOT element-aligned (GPT-2: 14291,
\ so wte's absolute pointer is 3 bytes off a 4-byte boundary). The forward pass /
\ residency layer must treat WITH-TENSOR spans as byte-aligned, not
\ element-aligned - see docs/gb10-uma-residency.md.

require lib/errors.f                     \ E-FS-*, E-MEM-MAP/E-MEM-UNMAP, E-JR-FIRST/E-JR-LAST
require lib/fs.f                          \ FILE-SIZE, FS-PATHZ, FS-U64@, EXISTS?, FILE?
require lib/string.f                      \ STR=, BYTE+, BYTE-COPY
require lib/memory.f                      \ MEM: block allocation for one session
require lib/cad-num-arithmetic.f          \ CAD-NUM: the byte-offset/extent algebra the bounded read runs in
require lib/adt/option.f                  \ option<n> for every id-addressed reader
require lib/adt/result.f                  \ result<n,n> for UNMAP-MAPPING's cleanup outcome
require lib/json-read.f                   \ JR: streaming JSON pull reader

\ ---------------------------------------------------------------------------
\ package SAFET-MAP - the host file-mapping seam.
\
\ Two responsibilities live here and nowhere else: turning a path into a
\ read-only private mapping, and giving that mapping back to the kernel. Keeping
\ them in their own package is not decoration - package SAFET publishes OPEN and
\ CLOSE, which would shadow the `open` and `close` syscall words for every later
\ definition inside SAFET. Isolating the syscalls structurally removes that
\ hazard instead of relying on definition order.
\ ---------------------------------------------------------------------------
package SAFET-MAP

private

\ Reinterprets the raw address the mmap primitive returns as a typed byte
\ pointer. The checker cannot express this syscall-result refinement, and this
\ is the only place in the loader that needs it.
TRUSTED: N>PTR ( n -- ptr u8 ) ;

0 constant MAP-ADDR-ANY
1 constant MAP-PROT-READ
2 constant MAP-PRIVATE
0 constant OPEN-RDONLY
0 constant OPEN-MODE
0 constant MAP-OFF-ZERO

\ Live checkpoint mappings held by this process. Pure accounting: nothing in the
\ load path reads it, and it decides nothing. It exists so a caller - in practice
\ the test suite - can assert the leak invariant directly after every load,
\ failure and disposal, instead of trusting that each mapping found its unmap.
variable LIVE-N

public

: LIVE ( -- n )                                \ checkpoint mappings not yet released
   LIVE-N @ ;

: MAP-PATH ( ptr u8 n n -- ptr u8 )            \ path + byte length -> mapping base
   {: pa:ptr pu:n sz:n :}
   sz 0 <= if E-FS-STAT throw then
   pa pu FS-PATHZ OPEN-RDONLY OPEN-MODE open {: fd:n :}
   fd 0 < if E-FS-OPEN throw then
   MAP-ADDR-ANY sz MAP-PROT-READ MAP-PRIVATE fd MAP-OFF-ZERO mmap {: base:n :}
   fd close
   base 0 < if E-MEM-MAP throw then
   1 LIVE-N +!
   base N>PTR ;

: UNMAP ( ptr u8 n -- )                        \ give one mapping back to the kernel
   munmap
   dup 0 < if E-MEM-UNMAP throw then
   drop
   -1 LIVE-N +! ;

\ Seal BOTH wordlists. Without this a later file could execute `package
\ SAFET-MAP` and republish N>PTR, handing out a raw pointer built from the mmap
\ result - non-resolution from outside proves only that the name is not visible,
\ never that the package cannot be reopened and drained.
private
get-current prot-wid-add
public
get-current prot-wid-add
;package

\ ---------------------------------------------------------------------------
\ package SAFET - load sessions, published censuses, and the header parser.
\ ---------------------------------------------------------------------------
package SAFET

public

\ ---- named throw codes (this module owns the block -7600..-7699) -----------
-7600 constant E-HEADER    \ file < 8 bytes, or 8+header_len exceeds the file, or header_len <= 0
-7601 constant E-JSON      \ header JSON malformed, not an object, or a value not an object
-7602 constant E-DTYPE     \ unknown / unsupported dtype wire string
-7603 constant E-SHAPE     \ shape not an array, negative dim, rank overflow, or byte-size mismatch
-7604 constant E-OFFSETS   \ data_offsets not [begin,end], begin>end, out of range, or overlapping
-7605 constant E-ALIGN     \ begin not a multiple of the element size
-7606 constant E-FIELD     \ a tensor object is missing dtype / shape / data_offsets
-7607 constant E-CAP       \ too many tensors, or name arena exhausted
-7608 constant E-ORDER     \ session step taken out of order (image twice, parse first, detach unparsed)

\ ---- dtype codes (numerically match maki/tensor.f MAKI:DT-* for the intersecting
\ set, so the forward dot maps SAFET:DTYPE? -> MAKI dtype by value; the loader is
\ the authority on the safetensors wire dtype and may support a superset) --------
0 constant DT-F32
1 constant DT-F16
2 constant DT-BF16
3 constant DT-U32
4 constant DT-I32

\ ---- the three owner tokens ------------------------------------------------
DEFLINEAR SAFET:session        \ one open, unpublished load transaction
DEFLINEAR SAFET:census         \ one published, immutable tensor census
DEFLINEAR SAFET:mapping        \ one file mapping detached out of a census

\ A detach is total: the first call moves the reserved mapping record; later
\ calls report empty and mint no owner.
ENUM map-take 0
   VARIANT moved FIELD m SAFET:mapping ;VARIANT
   VARIANT empty ;VARIANT
;ENUM

private

\ ---- capacities ------------------------------------------------------------
8    constant MAX-RANK         \ max tensor rank a census records
2048 constant CAP              \ max tensors (GPT-2: 160; LLaMA-70B: ~700)
$40000 constant NAME-CAP       \ name arena bytes (256 KiB)
1024 constant SCRATCH-CAP      \ per-name unescape scratch
$7FFFFFFFFFFFFFFF constant MAX-N
7 constant ROW-CELLS           \ cells per staged/published tensor row

\ ---- one session block -----------------------------------------------------
\ All metadata lives in this allocation, so DETACH publishes by retyping the
\ owner token rather than copying. A validated session also owns the separate
\ mapping-record reservation stored in the appended slot. Cells 0 and 1 hold
\ pointers and are addressed with `ptr-field`
\ (cell INDEXES); every other field is a byte offset used with `+`.
0 constant IMG-IDX             \ ptr u8: image base (mapping or adopted span)
1 constant KEY-IDX             \ ptr u8: current JSON member-key span pointer
2 cells constant MAP-LEN-OFF   \ image byte length
3 cells constant OWNS-MAP-OFF  \ 1 when disposal must unmap the image
4 cells constant HAS-IMG-OFF   \ 1 while this block HAS an image: set by MAP-FILE
                               \ or ADOPT, cleared again by DETACH-MAPPING. It is
                               \ what the session's ordering rules read and what
                               \ byte access checks after a mapping was detached.
5 cells constant HDR-LEN-OFF   \ JSON header byte length (the leading u64)
6 cells constant DATA-LEN-OFF  \ data-section byte length (map_len - 8 - header_len);
                               \ file geometry like HDR-LEN-OFF, so it survives
                               \ DETACH-MAPPING even though MAP-LEN-OFF does not
7 cells constant N-OFF         \ PUBLISHED tensor count (written only by DETACH)
8 cells constant STAGE-OFF     \ staged rows during a parse
9 cells constant BUMP-OFF      \ name-arena bump cursor
10 cells constant PARSED-OFF   \ 1 once a whole header validated
11 cells constant NMOFF-OFF    \ ---- per-tensor parse accumulators ----
12 cells constant NMLEN-OFF
13 cells constant DT-OFF
14 cells constant ES-OFF       \ element size of the current dtype
15 cells constant RK-OFF
16 cells constant BEG-OFF
17 cells constant END-OFF
18 cells constant DT-SEEN-OFF
19 cells constant SH-SEEN-OFF
20 cells constant OFF-SEEN-OFF
21 cells constant KEY-LEN-OFF
22 cells constant OVL-OFF
23 cells constant CUR-DIMS-OFF

\ Regions after the header, in order. NAME-CAP, SCRATCH-CAP and JR storage are
\ cell multiples, so the appended pointer slot stays cell-aligned without moving
\ any existing field or region.
CUR-DIMS-OFF MAX-RANK cells + constant ROWS-OFF
ROWS-OFF CAP ROW-CELLS * cells + constant DIMS-OFF
DIMS-OFF CAP MAX-RANK * cells + constant ARENA-OFF
ARENA-OFF NAME-CAP + constant SCRATCH-OFF
SCRATCH-OFF SCRATCH-CAP + constant JR-OFF
JR-OFF JR:STORAGE-BYTES + constant MR-REC-OFF
MR-REC-OFF 1 cells / constant MR-REC-IDX
MR-REC-OFF 1 cells + constant BLOCK-BYTES

\ ---- tensor row columns ----------------------------------------------------
0 constant C-NMOFF             \ name offset into the arena
1 constant C-NMLEN             \ name length
2 constant C-DTY               \ dtype code (DT-*)
3 constant C-RANK
4 constant C-NB                \ tensor byte length
5 constant C-BEG               \ data_offsets begin (data-section relative)
6 constant C-END               \ data_offsets end

\ ---- one mapping record ----------------------------------------------------
\ A detached mapping outlives its census, so its three-cell record is a separate
\ allocation. PARSE allocates and fills it before publication; the census owns
\ that reservation until DETACH-MAPPING moves it. Cell 0 holds a pointer and is
\ addressed with `ptr-field`; the other two are byte offsets.
0 constant MR-BASE-IDX         \ ptr u8: first byte of the mapping
1 cells constant MR-LEN-OFF    \ mapping byte length
2 cells constant MR-OWNS-OFF   \ 1 when UNMAP-MAPPING must unmap these bytes
3 cells constant MR-BYTES

\ ---- audited representation boundary ---------------------------------------
\ Ten private leaves, one abstraction: minting an owner token from its block,
\ reading the block back out of a token, retyping a validated session as a
\ census, consuming a token to reach its block for disposal, and the block's byte
\ view. The tokens ARE their blocks, so every leaf is an identity or a
\ duplication of one cell; the checker cannot express "this pointer is a live
\ SAFET block". All ten stay package-private and unreachable from outside
\ (proved in safetensors-test.f).
TRUSTED: MINT-SESSION ( ptr u8 -- SAFET:session ) ;

TRUSTED: SESSION>BLOCK ( SAFET:session -- SAFET:session ptr n )
   dup ;

TRUSTED: TAKE-SESSION ( SAFET:session -- ptr n ) ;

TRUSTED: SESSION>CENSUS ( SAFET:session -- SAFET:census ) ;

TRUSTED: CENSUS>BLOCK ( SAFET:census -- SAFET:census ptr n )
   dup ;

TRUSTED: TAKE-CENSUS ( SAFET:census -- ptr n ) ;

TRUSTED: MINT-MAPPING ( ptr u8 -- SAFET:mapping ) ;

TRUSTED: MAPPING>REC ( SAFET:mapping -- SAFET:mapping ptr n )
   dup ;

TRUSTED: TAKE-MAPPING ( SAFET:mapping -- ptr n ) ;

\ The block's byte view. Package-private and applied only to a block address
\ that one of the leaves above just produced.
TRUSTED: BLOCK>BYTES ( ptr n -- ptr u8 ) ;

\ ---- block regions ---------------------------------------------------------
: ARENA ( ptr n -- ptr u8 )
   BLOCK>BYTES ARENA-OFF BYTE+ ;

: SCRATCH ( ptr n -- ptr u8 )
   BLOCK>BYTES SCRATCH-OFF BYTE+ ;

: IMG ( ptr n -- ptr u8 ) {: st:ptr :}
   st IMG-IDX ptr-field @ ;

: DATA-BASE ( ptr n -- ptr u8 ) {: st:ptr :}   \ first byte past the JSON header
   st IMG  8 st HDR-LEN-OFF + @ +  BYTE+ ;

: ROW@ ( ptr n n n -- n ) {: st:ptr row:n col:n :}
   st ROWS-OFF + row ROW-CELLS * col + cells + @ ;

: ROW! ( ptr n n n n -- ) {: st:ptr row:n col:n value:n :}
   value  st ROWS-OFF + row ROW-CELLS * col + cells +  ! ;

: DIMS@ ( ptr n n n -- n ) {: st:ptr id:n axis:n :}
   st DIMS-OFF + id MAX-RANK * axis + cells + @ ;

: DIMS! ( ptr n n n n -- ) {: st:ptr id:n axis:n value:n :}
   value  st DIMS-OFF + id MAX-RANK * axis + cells +  ! ;

: NAME-AT ( ptr n n -- ptr u8 n ) {: st:ptr id:n :}
   st ARENA  st id C-NMOFF ROW@ BYTE+   st id C-NMLEN ROW@ ;

: TENSOR-AT ( ptr n n -- ptr u8 ) {: st:ptr id:n :}
   st DATA-BASE  st id C-BEG ROW@ BYTE+ ;

: ID-OK? ( ptr n n -- bool ) {: st:ptr id:n :}
   id 0 >= id st N-OFF + @ < and ;

: HAS-IMG? ( ptr n -- bool ) {: st:ptr :}
   st HAS-IMG-OFF + @ 0 <> ;

\ The gate on every reader that hands out tensor BYTES. A valid id is not enough:
\ once DETACH-MAPPING has moved the image to a SAFET:mapping owner, this block no
\ longer has any claim on those bytes, and the mapping may already be unmapped.
\ Metadata readers deliberately do NOT ask this - a census keeps describing its
\ tensors after its mapping leaves, which is the whole point of the transfer.
: BYTES-OK? ( ptr n n -- bool ) {: st:ptr id:n :}
   st id ID-OK?  st HAS-IMG?  and ;

\ ---- block lifetime --------------------------------------------------------
\ Live session/census blocks. Pure accounting, exactly like SAFET-MAP:LIVE:
\ nothing in the load path reads it and it decides nothing, but it lets a caller
\ assert after any sequence of loads and failures that every owner reached its
\ CLOSE or RELEASE. A linear token that is abandoned by a throw is invisible to
\ the checker (the stack is unwound past it), so this counter is how "no hidden
\ registry, nothing left over" is actually observed.
variable LIVE-N

: BLOCK-LEN ( -- CAD-NUM:alloc-byte-len )
   BLOCK-BYTES MEM:BYTES-ALLOC-LEN ;

: MR-ALLOC ( -- CAD-NUM:alloc-byte-len )
   MR-BYTES MEM:BYTES-ALLOC-LEN ;

: INIT-BLOCK ( ptr n -- ) {: st:ptr :}
   0 st MAP-LEN-OFF + !   0 st OWNS-MAP-OFF + !  0 st HAS-IMG-OFF + !
   0 st HDR-LEN-OFF + !   0 st DATA-LEN-OFF + !
   0 st N-OFF + !         0 st STAGE-OFF + !     0 st BUMP-OFF + !
   0 st PARSED-OFF + !
   NULL$ drop st MR-REC-IDX ptr-field ! ;

\ The single disposal path behind both CLOSE and RELEASE. It reads the mapping
\ decision out of the block BEFORE freeing the block, so neither outcome can
\ touch released memory and a failing munmap still cannot leak the metadata.
: DISPOSE ( ptr n -- ) {: st:ptr :}
   st OWNS-MAP-OFF + @ {: owns:n :}
   st IMG {: base:ptr :}
   st MAP-LEN-OFF + @ {: len:n :}
   st MR-REC-IDX ptr-field @ 0= 0= if
      st MR-REC-IDX ptr-field @ MR-ALLOC MEM:RELEASE-BYTES
   then
   st BLOCK>BYTES BLOCK-LEN MEM:RELEASE-BYTES
   -1 LIVE-N +!
   owns 0 <> if base len SAFET-MAP:UNMAP then ;

\ ---- mapping records -------------------------------------------------------
: MR-FILL ( ptr n ptr n -- ) {: mr:ptr st:ptr :}   \ copy a block's image cells into a record
   st IMG               mr MR-BASE-IDX ptr-field !
   st MAP-LEN-OFF + @   mr MR-LEN-OFF + !
   st OWNS-MAP-OFF + @  mr MR-OWNS-OFF + ! ;

\ Reserve and fully initialize the record after validation. Re-parsing the same
\ session reuses its reservation, so only the first successful parse allocates.
: RESERVE-MAPPING ( ptr n -- ) {: st:ptr :}
   st MR-REC-IDX ptr-field @ 0= 0= if exit then
   MR-ALLOC MEM:ALLOC-BYTES drop {: mr:ptr :}
   mr st MR-FILL
   mr st MR-REC-IDX ptr-field ! ;

\ The other half of the transfer. Clearing HAS-IMG-OFF is what makes byte access
\ fail closed afterwards (BYTES-OK?), and clearing OWNS-MAP-OFF is what sends the
\ block's later CLOSE / RELEASE down DISPOSE's metadata-only branch. The base and
\ length go too, so nothing that could reconstruct an address survives here.
: CLEAR-IMG ( ptr n -- ) {: st:ptr :}
   NULL$ drop  st IMG-IDX ptr-field !
   0 st MAP-LEN-OFF + !
   0 st OWNS-MAP-OFF + !
   0 st HAS-IMG-OFF + ! ;

: UNMAP-BODY ( ptr u8 n -- ptr u8 n ) {: base:ptr len:n :}   \ stack-preserving, so `catch` accepts it
   base len SAFET-MAP:UNMAP
   base len ;

\ ---- bounded fixed-width reads over a mapping ------------------------------
\ The bound is the length the RECORD carries, not the census's copy: CLEAR-IMG
\ zeroes that copy when the mapping leaves, and a mapping still has to know
\ where it ends afterwards.
4 constant U32-BYTES           \ wire width of a little-endian u32
8  constant SH-B1              \ where each successive byte lands in the result
16 constant SH-B2
24 constant SH-B3

\ Only `ok` is reachable: READ-HEADER refuses an image under 8 bytes before any
\ record is reserved, and E-HEADER is that same image-geometry invariant's code.
: OK-EXTENT ( CAD-NUM:numeric-result<CAD-NUM:byte-len> -- CAD-NUM:byte-len )
   MATCH CAD-NUM:numeric-result
      ok OF ENDOF                          negative OF E-HEADER throw ENDOF
      zero OF E-HEADER throw ENDOF          overflow OF E-HEADER throw ENDOF
      underflow OF E-HEADER throw ENDOF     bad-alignment OF E-HEADER throw ENDOF
      misaligned OF E-HEADER throw ENDOF
   ;MATCH ;

\ These three words are the whole of this module's offset algebra, so CAD-NUM is
\ imported once for them and closed again straight after. Type names stay
\ qualified: `using` imports words, and a family spelling is not one.
using CAD-NUM

: MR-EXTENT ( ptr n -- CAD-NUM:byte-len ) {: mr:ptr :}   \ the record's own length
   mr MR-LEN-OFF + @ BYTE-LEN OK-EXTENT ;

: U32-REACH ( -- CAD-NUM:byte-len )   \ first byte -> last byte of one u32 read
   U32-BYTES 1- BYTE-LEN OK-EXTENT ;

\ The window's start offset, still a role, or NONE. Both ordinal questions are
\ settled here and only here: the advance proves the last byte is representable,
\ then the extent test proves it is inside this mapping. Nothing is erased and no
\ address is formed - answering `some` is what earns the caller the right to
\ advance a pointer by this offset.
: WINDOW? ( ptr n CAD-NUM:byte-off -- option<CAD-NUM:byte-off> )
   {: mr:ptr off:CAD-NUM:byte-off :}
   off U32-REACH ADVANCE-BYTE-OFF
   MATCH CAD-NUM:numeric-result
      ok OF mr MR-EXTENT BYTE-OFF-IN-LEN?
            if off OPTION:SOME else OPTION:NONE then ENDOF
      negative OF OPTION:NONE ENDOF        zero OF OPTION:NONE ENDOF
      overflow OF OPTION:NONE ENDOF        underflow OF OPTION:NONE ENDOF
      bad-alignment OF OPTION:NONE ENDOF   misaligned OF OPTION:NONE ENDOF
   ;MATCH ;

;using

\ The width is a wire constant, not a parameter, so the four byte positions are
\ written out rather than counted. It takes the already-advanced pointer, so no
\ offset role reaches it and none has to be erased for it.
: U32-AT ( ptr u8 -- n ) {: a:ptr :}           \ four bytes, least significant first
   a 0 BYTE@
   a 1 BYTE@ SH-B1 lshift or
   a 2 BYTE@ SH-B2 lshift or
   a 3 BYTE@ SH-B3 lshift or ;

\ ---- error remap: JR structural faults -> one loader surface ---------------
: JR-ERR? ( n -- bool )
   dup E-JR-LAST >= swap E-JR-FIRST <= and ;   \ code in [-3999,-3900]

: REMAP ( n -- )
   dup 0= if drop exit then
   dup JR-ERR? if drop E-JSON throw then
   throw ;

\ ---- dtype wire decode: set the current dtype code + element size ----------
: DECODE-DTYPE ( ptr u8 n ptr n -- ) {: st:ptr :}
   2dup s" F32"  STR= if 2drop DT-F32  st DT-OFF + ! 4 st ES-OFF + ! exit then
   2dup s" F16"  STR= if 2drop DT-F16  st DT-OFF + ! 2 st ES-OFF + ! exit then
   2dup s" BF16" STR= if 2drop DT-BF16 st DT-OFF + ! 2 st ES-OFF + ! exit then
   2dup s" I32"  STR= if 2drop DT-I32  st DT-OFF + ! 4 st ES-OFF + ! exit then
   2dup s" U32"  STR= if 2drop DT-U32  st DT-OFF + ! 4 st ES-OFF + ! exit then
   2drop E-DTYPE throw ;

: KEY= ( ptr u8 n ptr n -- bool ) {: st:ptr :}   \ literal vs the stashed member key
   st KEY-IDX ptr-field @ st KEY-LEN-OFF + @ STR= ;

\ ---- member value readers (cursor at the key; advance past its value) -------
: DO-DTYPE ( JR:reader ptr n -- JR:reader ) {: st:ptr :}
   JR:NEXT JR:T-STR <> if E-DTYPE throw then
   JR:SPAN$ st DECODE-DTYPE
   1 st DT-SEEN-OFF + ! ;

: DO-SHAPE ( JR:reader ptr n -- JR:reader ) {: st:ptr :}
   JR:NEXT JR:T-ARR <> if E-SHAPE throw then
   0 st RK-OFF + !
   begin JR:NEXT dup JR:T-ARR-END <> while
      dup JR:T-INT <> if E-SHAPE throw then drop
      JR:INT dup 0 < if E-SHAPE throw then
      st RK-OFF + @ MAX-RANK >= if E-SHAPE throw then
      st CUR-DIMS-OFF + st RK-OFF + @ cells + !
      st RK-OFF + @ 1+ st RK-OFF + !
   repeat drop
   1 st SH-SEEN-OFF + ! ;

: DO-OFFSETS ( JR:reader ptr n -- JR:reader ) {: st:ptr :}
   JR:NEXT JR:T-ARR <> if E-OFFSETS throw then
   JR:NEXT dup JR:T-INT <> if E-OFFSETS throw then drop JR:INT st BEG-OFF + !
   JR:NEXT dup JR:T-INT <> if E-OFFSETS throw then drop JR:INT st END-OFF + !
   JR:NEXT JR:T-ARR-END <> if E-OFFSETS throw then   \ exactly two elements
   1 st OFF-SEEN-OFF + ! ;

\ ---- validation: check the accumulators against the staged rows ------------
: SHAPE-MUL ( n n -- n ) {: a:n b:n :}
   a 0 < b 0 < or if E-SHAPE throw then
   a 0= b 0= or if 0 exit then
   a MAX-N b / > if E-SHAPE throw then
   a b * ;

: NELEMS ( ptr n -- n ) {: st:ptr :}
   1 st RK-OFF + @ 0 ?do st CUR-DIMS-OFF + i cells + @ SHAPE-MUL loop ;

: OVERLAPS? ( ptr n n n -- bool ) {: st:ptr b:n e:n :}
   0 st OVL-OFF + !
   st STAGE-OFF + @ 0 ?do
      b st i C-END ROW@ <  st i C-BEG ROW@ e <  and
      if 1 st OVL-OFF + ! leave then
   loop
   st OVL-OFF + @ 0 <> ;

: VALIDATE ( ptr n -- ) {: st:ptr :}
   st NELEMS st ES-OFF + @ SHAPE-MUL {: expect:n :}
   st BEG-OFF + @ 0 < if E-OFFSETS throw then
   st END-OFF + @ st BEG-OFF + @ < if E-OFFSETS throw then
   st END-OFF + @ st DATA-LEN-OFF + @ > if E-OFFSETS throw then
   st BEG-OFF + @ st ES-OFF + @ mod 0 <> if E-ALIGN throw then
   st END-OFF + @ st BEG-OFF + @ - expect <> if E-SHAPE throw then
   st st BEG-OFF + @ st END-OFF + @ OVERLAPS? if E-OFFSETS throw then ;

\ ---- one tensor value object: parse members, validate ----------------------
: STASH-KEY ( JR:reader ptr n -- JR:reader ) {: st:ptr :}
   JR:SPAN$ st KEY-LEN-OFF + !  st KEY-IDX ptr-field ! ;

: TENSOR-MEMBER ( JR:reader ptr n -- JR:reader ) {: st:ptr :}
   s" dtype"        st KEY= if st DO-DTYPE   else
   s" shape"        st KEY= if st DO-SHAPE   else
   s" data_offsets" st KEY= if st DO-OFFSETS else
      JR:NEXT drop JR:SKIP-VALUE
   then then then ;

: TENSOR-FIELDS? ( ptr n -- ) {: st:ptr :}
   st DT-SEEN-OFF  + @ 0= if E-FIELD throw then
   st SH-SEEN-OFF  + @ 0= if E-FIELD throw then
   st OFF-SEEN-OFF + @ 0= if E-FIELD throw then ;

: TENSOR ( JR:reader ptr n -- JR:reader ) {: st:ptr :}   \ cursor at the value T-OBJ
   0 st DT-SEEN-OFF + ! 0 st SH-SEEN-OFF + ! 0 st OFF-SEEN-OFF + ! 0 st RK-OFF + !
   begin JR:NEXT dup JR:T-OBJ-END <> while
      dup JR:T-KEY <> if E-JSON throw then drop
      st STASH-KEY
      st TENSOR-MEMBER
   repeat drop
   st TENSOR-FIELDS?
   st VALIDATE ;

\ ---- name copy into the session's arena ------------------------------------
: READ-NAME ( JR:reader ptr n -- JR:reader ) {: st:ptr :}   \ cursor at the member key
   st SCRATCH SCRATCH-CAP JR:STR {: nlen:n :}
   nlen st BUMP-OFF + @ + NAME-CAP > if E-CAP throw then
   st BUMP-OFF + @ st NMOFF-OFF + !
   nlen st NMLEN-OFF + !
   st SCRATCH  st ARENA st BUMP-OFF + @ BYTE+  nlen BYTE-COPY
   st BUMP-OFF + @ nlen + st BUMP-OFF + ! ;

: CUR-META? ( ptr n -- bool ) {: st:ptr :}
   st ARENA st NMOFF-OFF + @ BYTE+  st NMLEN-OFF + @  s" __metadata__" STR= ;

\ ---- commit a validated tensor into the staging rows -----------------------
: COMMIT-DIMS ( ptr n n -- ) {: st:ptr row:n :}
   st RK-OFF + @ 0 ?do
      st row i  st CUR-DIMS-OFF + i cells + @  DIMS!
   loop ;

: COMMIT-ROW ( ptr n n -- ) {: st:ptr row:n :}
   st row C-NMOFF st NMOFF-OFF + @ ROW!
   st row C-NMLEN st NMLEN-OFF + @ ROW!
   st row C-DTY   st DT-OFF + @    ROW!
   st row C-RANK  st RK-OFF + @    ROW!
   st row C-BEG   st BEG-OFF + @   ROW!
   st row C-END   st END-OFF + @   ROW!
   st row C-NB    st END-OFF + @ st BEG-OFF + @ -  ROW! ;

: STAGE-COMMIT ( ptr n -- ) {: st:ptr :}
   st STAGE-OFF + @ CAP >= if E-CAP throw then
   st STAGE-OFF + @ {: row:n :}
   st row COMMIT-ROW
   st row COMMIT-DIMS
   row 1+ st STAGE-OFF + ! ;

\ ---- top-level object member iteration -------------------------------------
: MEMBER-VALUE ( JR:reader n ptr n -- JR:reader ) {: st:ptr :}   \ value kind already read
   st CUR-META? if
      drop JR:SKIP-VALUE
   else
      JR:T-OBJ <> if E-JSON throw then
      st TENSOR
      st STAGE-COMMIT
   then ;

: MEMBERS ( JR:reader ptr n -- JR:reader ) {: st:ptr :}   \ cursor just past the root T-OBJ
   begin JR:NEXT dup JR:T-OBJ-END <> while
      dup JR:T-KEY <> if E-JSON throw then drop
      st READ-NAME
      JR:NEXT
      st MEMBER-VALUE
   repeat drop ;

: RUN-PARSE ( ptr n -- ) {: st:ptr :}
   st JR-OFF + JR:STORAGE-BYTES  st IMG 8 BYTE+  st HDR-LEN-OFF + @  JR:INIT
   JR:NEXT JR:T-OBJ <> if E-JSON throw then
   st MEMBERS JR:CLOSE ;

: PARSE-BODY ( ptr n -- ptr n )                \ stack-preserving, so `catch` accepts it
   dup RUN-PARSE ;

: READ-HEADER ( ptr n -- ) {: st:ptr :}        \ the leading u64 and the derived geometry
   st MAP-LEN-OFF + @ 8 < if E-HEADER throw then
   st IMG FS-U64@ st HDR-LEN-OFF + !
   st HDR-LEN-OFF + @ 0 <= if E-HEADER throw then
   8 st HDR-LEN-OFF + @ + st MAP-LEN-OFF + @ > if E-HEADER throw then
   st MAP-LEN-OFF + @ 8 - st HDR-LEN-OFF + @ - st DATA-LEN-OFF + ! ;

: PARSE-INTO ( ptr n -- ) {: st:ptr :}
   st HAS-IMG-OFF + @ 0= if E-ORDER throw then
   0 st STAGE-OFF + ! 0 st BUMP-OFF + ! 0 st PARSED-OFF + ! 0 st N-OFF + !
   st READ-HEADER
   st [: PARSE-BODY ;] catch nip REMAP
   st RESERVE-MAPPING
   1 st PARSED-OFF + ! ;

: PUBLISH ( ptr n -- ) {: st:ptr :}
   st PARSED-OFF + @ 0= if E-ORDER throw then
   st STAGE-OFF + @ st N-OFF + ! ;

\ The image guard is separate from TAKE-IMAGE because MAP-FILE must refuse a
\ second image BEFORE it maps one: rejecting afterwards would strand a mapping
\ that no owner records and nothing can ever unmap.
: NO-IMAGE? ( ptr n -- ) {: st:ptr :}
   st HAS-IMG-OFF + @ 0 <> if E-ORDER throw then ;

: TAKE-IMAGE ( ptr n ptr u8 n n -- ) {: st:ptr ia:ptr ilen:n owns:n :}
   st NO-IMAGE?
   ia st IMG-IDX ptr-field !
   ilen st MAP-LEN-OFF + !
   owns st OWNS-MAP-OFF + !
   1 st HAS-IMG-OFF + ! ;

\ Name lookup keeps its -1 "not found" sentinel strictly inside the package; the
\ public FIND converts it to option<n> so no caller can compare against -1.
: FIND-ID ( ptr n ptr u8 n -- n ) {: st:ptr qa:ptr qu:n :}
   -1 st N-OFF + @ 0 ?do
      st i NAME-AT qa qu STR=
      if drop i leave then
   loop ;

: ID? ( n -- option<n> )
   dup 0 < if drop OPTION:NONE exit then
   OPTION:SOME ;

\ Shared by every id-addressed scalar reader: one column of one row, or NONE
\ when the id is not a tensor of this census.
: COL? ( SAFET:census n n -- SAFET:census option<n> ) {: id:n col:n :}
   CENSUS>BLOCK {: st:ptr :}
   st id ID-OK? 0= if OPTION:NONE exit then
   st id col ROW@ OPTION:SOME ;

public

\ ---- session lifecycle -----------------------------------------------------
: OPEN ( -- SAFET:session )                    \ begin one isolated load transaction
   BLOCK-LEN MEM:ALLOC-BYTES drop MINT-SESSION
   1 LIVE-N +!
   SESSION>BLOCK INIT-BLOCK ;

: MAP-FILE ( SAFET:session ptr u8 n -- SAFET:session )   \ give the session a mapped file
   {: pa:ptr pu:n :}
   SESSION>BLOCK {: st:ptr :}
   st NO-IMAGE?                                 \ refuse before mapping, never after
   pa pu FILE-SIZE {: sz:n :}
   sz 8 < if E-HEADER throw then
   st  pa pu sz SAFET-MAP:MAP-PATH  sz  1  TAKE-IMAGE ;

: ADOPT ( SAFET:session ptr u8 n -- SAFET:session )      \ borrow a caller-owned image
   {: ia:ptr ilen:n :}
   SESSION>BLOCK {: st:ptr :}
   st ia ilen 0 TAKE-IMAGE ;

: PARSE ( SAFET:session -- SAFET:session )     \ read + validate this session's header
   SESSION>BLOCK PARSE-INTO ;

: DETACH ( SAFET:session -- SAFET:census )     \ consume a validated session, publish it
   SESSION>BLOCK PUBLISH
   SESSION>CENSUS ;

: CLOSE ( SAFET:session -- )                   \ consume a session that will not publish
   TAKE-SESSION DISPOSE ;

\ ---- census disposal -------------------------------------------------------
\ RELEASE has exactly two outcomes, decided by the block's mapping-ownership
\ cell, and DISPOSE implements both:
\
\   owns = 1  the census still owns the file mapping it was detached with, so
\             RELEASE unmaps that mapping and then frees the census metadata.
\             Every census produced from MAP-FILE / LOAD is in this state today.
\   owns = 0  the census does not own its image, so RELEASE frees the census
\             metadata ONLY and leaves the bytes alone. Today this is the
\             ADOPT / LOAD-SPAN case, where the image belongs to the caller.
\
\ The second outcome is the seam the mapping-owner leaf (SAFET:mapping,
\ WITH-MAPPING, UNMAP-MAPPING, DETACH-MAPPING) plugs into: transferring a
\ mapping out of a census only has to clear OWNS-MAP-OFF and hand the base and
\ length to the new owner. RELEASE's contract - consume the census, free
\ everything the census still owns, never unmap what it does not - is already
\ final and does not change when that state arrives.
: RELEASE ( SAFET:census -- )
   TAKE-CENSUS DISPOSE ;

\ ---- mapped residency: moving the file mapping out of a census -------------
\ The first call moves the record PARSE reserved and clears the census image.
\ Every step is total. A later call finds no reservation, changes nothing, and
\ returns empty; it cannot allocate or fabricate an owner.
: DETACH-MAPPING ( SAFET:census -- SAFET:census SAFET:map-take )
   CENSUS>BLOCK {: st:ptr :}
   st MR-REC-IDX ptr-field @ 0= if SAFET-MAP--TAKE:EMPTY exit then
   st MR-REC-IDX ptr-field @ {: mr:ptr :}
   NULL$ drop st MR-REC-IDX ptr-field !
   st CLEAR-IMG
   1 LIVE-N +!
   mr MINT-MAPPING SAFET-MAP--TAKE:MOVED ;

\ Zero-copy scoped access to the whole mapping, the mapping-side twin of
\ WITH-TENSOR: the body sees the mapping's bytes, and MAP-OFFSET? says where in
\ them each tensor starts. This works for an adopted image too - the bytes are
\ the caller's rather than the kernel's, but they are just as readable. For that
\ adopted case the mapping token BORROWS the caller's memory rather than owning
\ it, so the caller's buffer must outlive the mapping; nothing here enforces
\ that, and it is the same missing pointer-lifetime / region-type checker
\ capability named in this file's header.
\ The span is advisory - see the pointer-lifetime /
\ region-type note in this file's header for the capability that will make it
\ enforced; it is claimed no more strongly here than for WITH-TENSOR.
\ Every mapping was minted from a successfully parsed positive-length image, so
\ the body always runs and the returned length needs no optional arm.
\ typed-local-lint: allow-bare-local - `body` carries the quotation effect
\ [ SAFET:mapping ptr u8 n -- SAFET:mapping ], which a local annotation cannot express.
: WITH-MAPPING ( SAFET:mapping [ SAFET:mapping ptr u8 n -- SAFET:mapping ] -- SAFET:mapping n )
   {: body :}
   MAPPING>REC {: mr:ptr :}
   mr MR-LEN-OFF + @ {: len:n :}
   mr MR-BASE-IDX ptr-field @ len body execute
   len ;

\ Four little-endian bytes at a MAP-OFFSET? offset (mapping base = file byte 0),
\ NOT the data-section frame BEGIN? reports. SOME carries the raw bits as stored;
\ decoding them belongs to package F32. NONE when that four-byte window is not
\ wholly inside this mapping.
: U32-LE@? ( SAFET:mapping CAD-NUM:byte-off -- SAFET:mapping option<n> )
   {: off:CAD-NUM:byte-off :}
   MAPPING>REC {: mr:ptr :}
   mr off WINDOW?
   MATCH option
      none OF OPTION:NONE ENDOF
      some OF {: at:CAD-NUM:byte-off :}
              mr MR-BASE-IDX ptr-field @ at CAD-NUM:BYTE+ U32-AT OPTION:SOME
      ENDOF
   ;MATCH ;

\ Ends a mapping's life and reports the outcome as a value instead of a throw.
\ SAFET-MAP:UNMAP stays throw-based like every other syscall wrapper in that
\ package; the conversion happens HERE, at the one site that calls it on this
\ path, so a caller disposing of several owners can see a failed munmap without
\ unwinding past the owners it has not disposed of yet. `ok` carries the byte
\ count given back (0 for a mapping that owned nothing), `err` the named
\ E-MEM-UNMAP code. Like DISPOSE, it reads the record and frees it BEFORE the
\ syscall, so a failing munmap still cannot leak the record.
: UNMAP-MAPPING ( SAFET:mapping -- result<n,n> )
   TAKE-MAPPING {: mr:ptr :}
   mr MR-BASE-IDX ptr-field @ {: base:ptr :}
   mr MR-LEN-OFF + @ {: len:n :}
   mr MR-OWNS-OFF + @ {: owns:n :}
   mr BLOCK>BYTES MR-ALLOC MEM:RELEASE-BYTES
   -1 LIVE-N +!
   owns 0= if 0 RESULT:OK exit then
   base len [: UNMAP-BODY ;] catch {: code:n :}
   2drop
   code 0= if len RESULT:OK else code RESULT:ERR then ;

\ ---- whole-file convenience paths ------------------------------------------
\ Both convenience paths guard EVERYTHING that can throw after OPEN. Taking the
\ image is inside the guarded region, not before it: MAP-FILE alone throws on a
\ missing path, an unstattable path, a file under 8 bytes, and a failed
\ open/mmap, and any of those escaping the catch would strand the session block
\ with no owner to close it. The two words below carry their arguments straight
\ back out so the quotation is stack-preserving, which is what `catch` requires
\ - a quotation cannot read the caller's locals.
: TAKE-FILE+PARSE ( SAFET:session ptr u8 n -- SAFET:session ptr u8 n )
   {: pa:ptr pu:n :}
   pa pu MAP-FILE PARSE
   pa pu ;

: TAKE-SPAN+PARSE ( SAFET:session ptr u8 n -- SAFET:session ptr u8 n )
   {: ia:ptr ilen:n :}
   ia ilen ADOPT PARSE
   ia ilen ;

: LOAD ( ptr u8 n -- SAFET:census )            \ map + parse + publish, closing on any fault
   {: pa:ptr pu:n :}
   OPEN pa pu
   [: TAKE-FILE+PARSE ;] catch {: code:n :}
   2drop
   code 0 <> if CLOSE code throw then
   DETACH ;

: LOAD-SPAN ( ptr u8 n -- SAFET:census )       \ the same over a caller-owned image
   {: ia:ptr ilen:n :}
   OPEN ia ilen
   [: TAKE-SPAN+PARSE ;] catch {: code:n :}
   2drop
   code 0 <> if CLOSE code throw then
   DETACH ;

: PRESENT? ( ptr u8 n -- bool )                \ true iff a readable file exists at path
   {: pa:ptr pu:n :}
   pa pu EXISTS? 0= if 0 0= 0= exit then
   pa pu FILE? ;

: MAX-TENSORS ( -- n )                         \ the largest census this loader publishes
   CAP ;

\ Leak accounting, not load state: it answers how many sessions, censuses and
\ detached mappings have been created and not yet disposed, and nothing about any
\ of them. A throw unwinds the stack past a linear owner without the checker
\ noticing, so this is how a caller (in practice the test suite) checks that every
\ OPEN reached its CLOSE or RELEASE and every moved mapping reached
\ UNMAP-MAPPING. Pair it with SAFET-MAP:LIVE, which counts kernel mappings: a
\ moved result raises this owner count by one without changing that one.
: LIVE-OWNERS ( -- n )
   LIVE-N @ ;

\ ---- census readers (every one takes its census; none reads ambient state) --
: COUNT ( SAFET:census -- SAFET:census n )
   CENSUS>BLOCK N-OFF + @ ;

: MAP-LEN ( SAFET:census -- SAFET:census n )   \ 0 once DETACH-MAPPING took the image
   CENSUS>BLOCK MAP-LEN-OFF + @ ;

: HDR-LEN ( SAFET:census -- SAFET:census n )
   CENSUS>BLOCK HDR-LEN-OFF + @ ;

: FIND ( SAFET:census ptr u8 n -- SAFET:census option<n> )   \ tensor id by name
   {: qa:ptr qu:n :}
   CENSUS>BLOCK {: st:ptr :}
   st qa qu FIND-ID ID? ;

: DTYPE? ( SAFET:census n -- SAFET:census option<n> )
   C-DTY COL? ;

: RANK? ( SAFET:census n -- SAFET:census option<n> )
   C-RANK COL? ;

: NBYTES? ( SAFET:census n -- SAFET:census option<n> )
   C-NB COL? ;

: BEGIN? ( SAFET:census n -- SAFET:census option<n> )   \ data-section relative
   C-BEG COL? ;

: END? ( SAFET:census n -- SAFET:census option<n> )     \ data-section relative
   C-END COL? ;

\ MAPPING-BASE relative, NOT data-section relative: the byte distance from the
\ first byte of the mapping (the file's byte 0) to this tensor's first byte,
\ i.e. 8 + header_len + begin. This is the frame a holder of SAFET:mapping
\ needs, because WITH-MAPPING hands out the mapping base; BEGIN? / END? above
\ are the other frame and the two differ by exactly the 8-byte length prefix
\ plus the JSON header. Reading a mapping at a BEGIN? offset lands inside the
\ header. It keeps answering after DETACH-MAPPING: the frame is arithmetic on
\ the header geometry the census still holds, not access to any bytes.
: MAP-OFFSET? ( SAFET:census n -- SAFET:census option<n> )   \ mapping-base relative
   {: id:n :}
   CENSUS>BLOCK {: st:ptr :}
   st id ID-OK? 0= if OPTION:NONE exit then
   8 st HDR-LEN-OFF + @ +  st id C-BEG ROW@ +  OPTION:SOME ;

: DIM? ( SAFET:census n n -- SAFET:census option<n> )
   {: id:n axis:n :}
   CENSUS>BLOCK {: st:ptr :}
   st id ID-OK? 0= if OPTION:NONE exit then
   axis 0 < axis st id C-RANK ROW@ >= or if OPTION:NONE exit then
   st id axis DIMS@ OPTION:SOME ;

: COPY-NAME? ( SAFET:census n ptr u8 n -- SAFET:census option<n> )   \ SOME copied length
   {: id:n da:ptr dcap:n :}
   CENSUS>BLOCK {: st:ptr :}
   st id ID-OK? 0= if OPTION:NONE exit then
   st id C-NMLEN ROW@ {: nlen:n :}
   nlen dcap > if OPTION:NONE exit then
   st id NAME-AT drop da nlen BYTE-COPY
   nlen OPTION:SOME ;

\ NONE when the id is not a tensor of this census OR when this census no longer
\ holds the image - a census whose mapping was detached copies out no bytes.
: COPY-DATA? ( SAFET:census n ptr u8 n -- SAFET:census option<n> )   \ SOME copied length
   {: id:n da:ptr dcap:n :}
   CENSUS>BLOCK {: st:ptr :}
   st id BYTES-OK? 0= if OPTION:NONE exit then
   dcap 0 < if OPTION:NONE exit then
   st id C-NB ROW@ {: nb:n :}
   dcap nb > if nb else dcap then {: take:n :}
   st id TENSOR-AT da take BYTE-COPY
   take OPTION:SOME ;

\ Zero-copy scoped access: the body sees the tensor's bytes inside the mapping
\ the census owns. NONE (body not run) when the id is not a tensor of this
\ census, and NONE for EVERY id once DETACH-MAPPING has moved the image out -
\ the census cannot lend bytes it no longer owns, and the mapping behind them
\ may already have been unmapped. The span is advisory - a body that
\ deliberately stashes it and reads it after RELEASE reads freed memory; see the
\ pointer-lifetime / region-type note in this file's header for the capability
\ that will make it enforced.
\ typed-local-lint: allow-bare-local - `body` carries the quotation effect
\ [ SAFET:census ptr u8 n -- SAFET:census ], which a local annotation cannot express.
: WITH-TENSOR ( SAFET:census n [ SAFET:census ptr u8 n -- SAFET:census ] -- SAFET:census option<n> )
   {: id:n body :}
   CENSUS>BLOCK {: st:ptr :}
   st id BYTES-OK? 0= if OPTION:NONE exit then
   st id C-NB ROW@ {: nb:n :}
   st id TENSOR-AT nb body execute
   nb OPTION:SOME ;

\ Seal BOTH wordlists, for the same reason SAFET-MAP does. Reopening SAFET would
\ otherwise republish MAPPING>REC or the block leaves, and a caller could take a
\ mapping's base address and read it with no bounds check at all - exactly what
\ the WITH- spans and the copying readers exist to prevent.
private
get-current prot-wid-add
public
get-current prot-wid-add
;package
