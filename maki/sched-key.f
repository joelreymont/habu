\ maki/sched-key.f - schedule cache/replay keys + the cad-5 replay-table seam (cad-4).
\
\ docs/archive/cad-plan.md section 7.4 + section 22.6 (precision in every key). The cache and
\ replay key over a fusion region is (region signature, shape class, dtype key,
\ layout key, alignment class, requested numeric policy, target, engine hash,
\ ptxas version). This file renders that whole key as one string and
\ owns the in-memory key->selection replay table. The families/defaults are
\ maki/schedule.f; the TILE/TUNE wiring is maki/cad.f. One concern: keys + replay.
\
\ Region signature: an FNV-1a 64-bit content hash over the region's node facts
\ (op kind, rows, cols, dtype, layout) in node order, rendered as 16 hex digits.
\ lib/content-key.f's SHA256 keys are a file-content cache (fs paths, mtime, mmap)
\ that is not loadable as an in-memory region hash here, and lib/map.f's hash is a
\ tag-newtype-wrapped map internal; a small documented FNV-1a over the region's node
\ facts is the self-contained content hash. (The separate engine field carries the
\ real SHA-256 content key over bin/hb via lib/engine-id.f; the region signature
\ itself stays an in-memory FNV-1a.)
\
\ Shape class (section 7.4): each extent <= 64 is rendered exactly; a larger extent
\ becomes a power-of-two bucket plus a tail flag ("p128+t" when it is not itself a
\ power of two, "p128" when it is); an unbound extent (0) renders "?". The shape
\ class is a typed `dimclass` (exact/pow2/pow2-tail/unbound) + a magnitude, encoded
\ once by DIM>CLASS and shared by both the render and the typed key below.
\
\ Typed key (dot habu-cad-adt-swap): SK-KEY builds the nine region-derived fields as
\ a typed `skey` STRUCTURE (dimclass/dtype/layout/align enum fields), so a
\ semantic-field role swap at construction is a CHECKER reject, and MAKI-SKEY:EQ
\ compares that region-derived identity. SK-KEY$ stays the ONE durable render of
\ (skey, target, engine, ptxas); the in-memory replay table keys on that render
\ (STR=), a documented durable-text boundary (see the replay-table note) - a
\ typed-column table awaits the W>1 typed store (habu-checker-capability-typed S2).
\
\ Alignment class: the most conservative model-input alignment the region reads
\ (AL-16 when it reads no model input - compiler-allocated buffers are aligned by
\ construction). Target is the caller's validated nominal descriptor rendered as
\ its SEMANTIC FACTS (SK-TARGET+, injective over TARGET:EQUAL?) - never the
\ presentation label, which TARGET:REGISTER does not dedup; the engine hash is
\ the real SHA-256 content key over bin/hb (lib/engine-id.f, resolved
\ engine-side, lazy + cached); ptxas version is the honest "unprobed"
\ placeholder (no ptxas is probed on a host without a device).
\
\ Replay: a growable in-memory key->selection table with GET/PUT. This is the cad-5
\ store SEAM - a query that misses returns (-1 false) so the caller falls back to the
\ closed-form defaults ("unmeasured shape class -> using defaults"), since cad-4 has no
\ measurements (those land in cad-5/cad-6).
\
\ Fail closed: an out-of-range region id or alignment class are named throws.
\ maki -> habu only; sched-key owns -5084..-5085.

require lib/prelude.f
require lib/memory.f
require lib/vector.f
require lib/string.f
require lib/float.f
require lib/fmt.f
require lib/engine-id.f
require maki/model-ir.f
require maki/fusion-plan.f
require maki/schedule.f
require maki/numpolicy.f
require maki/target/target.f

-5084 constant E-SK-REGION     \ region id out of range / empty
-5085 constant E-SK-ALIGN      \ alignment class out of range (AL-* domain)

package MAKI
public

\ ---- typed schedule key: the shape-class family + the SKEY structure record --
\ `dimclass` classifies one tensor extent exactly the way the rendered shape
\ class does (dot habu-cad-adt-swap): an exact extent (<=64), a power-of-two
\ bucket, a pow2 bucket with a non-pow2 tail, or an unbound (0) extent. Encoded
\ as (dimclass, magnitude n) it has EXACTLY the rendered-text identity - proven
\ field-eq == text-eq across the bucket-domain boundaries in sched-key-test.f.
\ DERIVE eq generates MAKI-DIMCLASS:EQ so it can be an enum FIELD of `skey`.
\ Magnitude convention (paired n): exact = the exact extent (<=64); pow2 /
\ pow2-tail = the NEXT-POW2 bucket (extent >64, exactly a pow2 vs with a tail);
\ unbound = 0. (Inline `\` notes inside an ENUM block are a parse error.)
ENUM dimclass DERIVE eq
  exact
  pow2
  pow2-tail
  unbound
;ENUM

\ `skey` is the section-7.4 schedule key as a typed record: region signature
\ hash, the two shape-class dims, and the representative node's dtype / layout /
\ alignment - every semantically distinct field carries its own family, so a
\ dtype/layout (or any enum-field) role swap at construction is a CHECKER reject
\ (the semantic-role hole the string key left open). DERIVE eq gives the typed
\ identity MAKI-SKEY:EQ; every enum field family also derives eq (else the
\ declaration throws). target is the caller-supplied validated nominal descriptor
\ id (maki/target/target.f), rendered into the durable key as its semantic FACTS
\ (SK-TARGET+; injective over TARGET:EQUAL?, so two same-label descriptors never
\ collide); engine / ptxas are per-process invariants (one engine build, one
\ ptxas per running process). None of the three are structure fields: they extend
\ `skey` into the durable identity (skey, target, engine, ptxas). With target and
\ process identity fixed, `skey` field equality corresponds to equality of the
\ durable text. The persistent schedules.rows store stays keyed by that render
\ (see the SK-GET/SK-PUT boundary note below). Fields (slot order, deepest first):
\ rsig = FNV-1a region signature (n); rk/rm = representative rows class+magnitude;
\ ck/cm = representative cols class+magnitude; dt/lay = representative dtype/layout;
\ al = region alignment class (slots deepest-first); pol = requested numeric policy
\ (NPOL:dom, maki/numpolicy.f) - the plan's declared proof domain, so changing the
\ request is a different key (no FP32/TF32 baseline pairing). (Inline `\` notes
\ inside a STRUCTURE block reject.)
STRUCTURE skey 0 DERIVE eq
  FIELD rsig n
  FIELD rk dimclass
  FIELD rm n
  FIELD ck dimclass
  FIELD cm n
  FIELD dt datatype
  FIELD lay layout
  FIELD al align
  FIELD pol NPOL:dom
;STRUCTURE

private

\ ---- FNV-1a 64-bit content hash over the region's node facts ----------------
$cbf29ce484222325 constant FNV-BASIS
$100000001b3       constant FNV-PRIME
variable SK-FOLD               \ scratch for little-endian byte decomposition

: FNV-BYTE ( n n -- n )  xor FNV-PRIME * ;      \ h byte -> h' (64-bit wrap)

: FNV-CELL ( n n -- n ) {: h:n v:n :}           \ fold one integer fact (8 LE bytes)
   v SK-FOLD !
   h
   8 0 ?do  SK-FOLD @ $FF and FNV-BYTE  SK-FOLD @ 8 rshift SK-FOLD !  loop ;

\ dtype/layout fold through their named wire-code boundaries (DTYPE>N/LAYOUT>N);
\ the codes equal the pre-family DT-*/LAY-* values, so persisted hashes are stable
: RSIG-NODE ( n CAD-KIND:node-id -- n )         \ ( h node -- h' ) fold a node's facts
   {: node:CAD-KIND:node-id :}
   node MIR-OP@ OPKIND>N   FNV-CELL
   node MIR-ROWS@ ROWS-RAW FNV-CELL
   node MIR-COLS@ COLS-RAW FNV-CELL
   node MIR-DT@  DTYPE>N  FNV-CELL
   node MIR-LAY@ LAYOUT>N FNV-CELL
   node LN-AFFINE? if node LN-FORM LN-FORM>ATTR FNV-CELL then ;  \ affine LayerNorm shares OP-LAYERNORM's
   \ wire code with the plain form, so its form payload folds here too - else a plain and an affine
   \ LayerNorm over the same shape would collide on one region key. Plain LN keys are unchanged
   \ (nothing extra folds), so only affine-LayerNorm cache/replay keys migrate on upgrade.

: RSIG ( CAD-KIND:region -- n ) {: r:CAD-KIND:region :}   \ region -> content hash (nodes in order)
   FNV-BASIS
   MIR-N@ 0 ?do
      i MIR-NODE-ID {: node:CAD-KIND:node-id :}
      node FP-RID@ r FP-RGN= if node RSIG-NODE then
   loop ;

\ ---- hex render (16 digits, MSB first) into the shared builder --------------
: HEX-NIB ( n -- n )  $F and dup 10 < if $30 + else $37 + then ;
: SK-HEX+ ( n -- ) {: v:n :}
   16 0 ?do  v  15 i - 4 * rshift HEX-NIB SB-APPEND-C  loop ;

\ ---- shape class (exact <= 64, else pow2 bucket + tail flag, ? for unbound) --
\ DIM>CLASS is the canonical encoder (extent -> typed class + magnitude); it is
\ the SINGLE source that both SK-KEY (the typed structure) and DIM-CLASS+ (the
\ durable render) classify through, so field-eq and text-eq can never diverge.
\ DIM>CLASS is public: it is the shape classifier the typed key and the tests
\ share (the field-eq == text-eq contract is pinned over it).
public
: DIM>CLASS ( n -- dimclass n )                 \ extent -> (class, magnitude)
   dup 0=    if drop MAKI-DIMCLASS:UNBOUND 0 exit then
   dup 64 <= if MAKI-DIMCLASS:EXACT swap exit then
   dup POW2? if NEXT-POW2 MAKI-DIMCLASS:POW2 swap exit then
   NEXT-POW2 MAKI-DIMCLASS:POW2-TAIL swap ;
private

: DIMCLASS+ ( dimclass n -- ) {: m:n :}         \ append one dim's class text to SB
   MATCH dimclass
      exact     OF m FMT:SB-INT ENDOF
      pow2      OF $70 SB-APPEND-C m FMT:SB-INT ENDOF
      pow2-tail OF $70 SB-APPEND-C m FMT:SB-INT s" +t" SB-APPEND ENDOF
      unbound   OF s" ?" SB-APPEND ENDOF
   ;MATCH ;

: DIM-CLASS+ ( n -- )  DIM>CLASS DIMCLASS+ ;    \ render one dim through the typed class

: SHAPE-CLASS+ ( CAD-KIND:rows CAD-KIND:cols -- )
   {: rows:CAD-KIND:rows cols:CAD-KIND:cols :}
   rows ROWS-RAW DIM-CLASS+ $78 SB-APPEND-C cols COLS-RAW DIM-CLASS+ ;

\ ---- alignment class over the region's model-input reads --------------------
\ Alignment classes are intrinsically ORDERED (unknown < byte < a4 < a8 < a16)
\ and the region class is their min-fold; ALIGN>N preserves that order, so the
\ fold runs on the ordinal codes. This is align's one named ordinal boundary;
\ the folded n feeds only AL-KEY below.
: NODE-ALIGN ( n CAD-KIND:node-id -- n )        \ ( al node -- al' ) min input-slot alignment
   {: node:CAD-KIND:node-id :}
   node MIR-IN-COUNT@ 0 ?do
      node i MIR-INPUT-IDX MIR-IN@ dup MIR-REF-INPUT?
      if MIR-REF-SLOT MIR-SLOT-AL@ ALIGN>N min else drop then
   loop ;

: REGION-ALIGN ( CAD-KIND:region -- n ) {: r:CAD-KIND:region :}
   AL-16
   MIR-N@ 0 ?do
      i MIR-NODE-ID {: node:CAD-KIND:node-id :}
      node FP-RID@ r FP-RGN= if node NODE-ALIGN then
   loop ;

\ ---- region requested numeric policy over the region's PER-OP domains --------
\ The plan's REQUESTED proof domain for the region = COMPOSE (weakest wins) over
\ each op's INTRINSIC numeric domain (NPOL:OP-DOM: relu/cast exact, add/mul ulp,
\ gelu/silu/matmul relative). This is the HONEST per-OP request: a pure-relu region
\ requests exact, a pure-transcendental elementwise region (only gelu/silu) requests
\ relative - a distinction the old per-class table could NOT make, since elementwise
\ is a MIXED class (exact relu/cast, ulp add/mul, transcendental gelu/silu) with no
\ single honest class default. It is the declared numeric contract that rides in the
\ exact key; a different op mix (a different honest policy) is a different key, so a
\ relative-policy TF32 row never pairs with an exact-policy FP32 baseline. This is the
\ SAME fold cad.f REGION-ACHIEVED runs over the region's achieved op domains; requested
\ and achieved stay distinct axes - PROMOTE-NPOL composes the golden's JUDGED precision
\ into achieved, then ENFORCEs it SATISFIES this requested policy.
: NODE-POL ( NPOL:dom CAD-KIND:node-id -- NPOL:dom ) {: acc:NPOL:dom node:CAD-KIND:node-id :}
   acc  node MIR-OP@ NPOL:OP-DOM  NPOL:COMPOSE ;
: REGION-POL ( CAD-KIND:region -- NPOL:dom ) {: r:CAD-KIND:region :}
   NPOL-DOM:EXACT
   MIR-N@ 0 ?do
      i MIR-NODE-ID {: node:CAD-KIND:node-id :}
      node FP-RID@ r FP-RGN= if node NODE-POL then
   loop ;

: AL-KEY ( n -- ptr u8 n )
   case
      AL-UNKNOWN of s" al?"  endof
      AL-BYTE    of s" al1"  endof
      AL-4       of s" al4"  endof
      AL-8       of s" al8"  endof
      AL-16      of s" al16" endof
      E-SK-ALIGN throw
   endcase ;

\ ---- region validation + representative (output) node -----------------------
\ revalidates the typed handle against the CURRENT plan (a stale region id held
\ across an FP-BUILD rebuild rejects here with sched-key's own code)
: SK-REGION-CK ( CAD-KIND:region -- CAD-KIND:region ) {: r:CAD-KIND:region :}
   r RGN>RAW dup 0 < swap FP-REGION-COUNT >= or if E-SK-REGION throw then
   r ;

: REGION-REP ( CAD-KIND:region -- CAD-KIND:node-id ) {: r:CAD-KIND:region :}   \ last (output) node in the region
   MIR-N@
   begin dup 0 > while
      1- dup MIR-NODE-ID {: node:CAD-KIND:node-id :}
      node FP-RID@ r FP-RGN= if drop node exit then
   repeat
   drop E-SK-REGION throw ;

public

\ ---- key fields: target facts, engine content key, ptxas placeholder ---------
\ Durable target field = the descriptor's SEMANTIC FACTS, never the label:
\ TARGET:REGISTER interns by descriptor but does NOT dedup labels, so two
\ distinct descriptors can share one label - a label-keyed row would replay
\ target-A schedules under target B (schedules.rows/evidence.rows poisoning).
\ Fixed field order + canonical SB-INT integers make the render injective over
\ TARGET:EQUAL? (facts-eq == field-text-eq). The inner separator is ',' so the
\ outer '|' join in SK-KEY+ stays unambiguous (TARGET:FACTS$ is '|'-separated
\ AND builds in the same shared SB, so it cannot be spliced into a live key
\ build; the field is rendered here from the fact accessors instead).
: SK-TARGET+ ( CAD-KIND:target-id -- ) {: t:CAD-KIND:target-id :}
   s" isa=" SB-APPEND t TARGET:ISA@ FMT:SB-INT
   s" ,arch=" SB-APPEND t TARGET:ARCH@ FMT:SB-INT
   s" ,warp=" SB-APPEND t TARGET:WARP@ FMT:SB-INT
   s" ,threads=" SB-APPEND t TARGET:THREADS@ FMT:SB-INT
   s" ,shared=" SB-APPEND t TARGET:SHARED@ FMT:SB-INT
   s" ,caps=" SB-APPEND t TARGET:CAPS@ FMT:SB-INT ;
\ Real engine content key: the SHA-256 of bin/hb, resolved engine-side from the
\ kernel-provided self-path and hashed once on first request, then cached
\ (lib/engine-id.f). It distinguishes schedules produced by different engine builds
\ so a schedules.rows written by one engine is never replayed under another; the
\ lazy+cached hash keeps it off the interactive key-render hot path.
: SK-ENGINE$ ( -- ptr u8 n )  ENGINE-ID:KEY$ ;
: SK-PTXAS$  ( -- ptr u8 n )  s" unprobed" ;       \ no ptxas probed off-device

\ representative (output) node of a region - the default-context source (rowlen/dtype)
: SK-REGION-REP ( CAD-KIND:region -- CAD-KIND:node-id )  SK-REGION-CK REGION-REP ;

\ ---- individual key fields (standalone renders, for inspection + tests) ------
: SK-RSIG$ ( CAD-KIND:region -- ptr u8 n )  SK-REGION-CK RSIG SB-RESET SK-HEX+ SB$ ;
: SK-SHAPE-CLASS$ ( CAD-KIND:rows CAD-KIND:cols -- ptr u8 n )
   SB-RESET SHAPE-CLASS+ SB$ ;
: SK-ALIGN$ ( CAD-KIND:region -- ptr u8 n )  SK-REGION-CK REGION-ALIGN AL-KEY ;

\ ---- the full section 7.4 key as one "|"-joined string ----------------------
: SK-KEY+ ( CAD-KIND:region CAD-KIND:target-id -- )
   {: r:CAD-KIND:region target:CAD-KIND:target-id :}   \ append the key to SB (already reset)
   r REGION-REP {: rep:CAD-KIND:node-id :}
   r RSIG SK-HEX+
   $7C SB-APPEND-C  rep MIR-ROWS@ rep MIR-COLS@ SHAPE-CLASS+
   $7C SB-APPEND-C  rep MIR-DTYPE-KEY  SB-APPEND
   $7C SB-APPEND-C  rep MIR-LAYOUT-KEY SB-APPEND
   $7C SB-APPEND-C  r REGION-ALIGN AL-KEY SB-APPEND
   $7C SB-APPEND-C  r REGION-POL NPOL:NAME SB-APPEND
   $7C SB-APPEND-C  target SK-TARGET+
   $7C SB-APPEND-C  SK-ENGINE$ SB-APPEND
   $7C SB-APPEND-C  SK-PTXAS$  SB-APPEND ;

: SK-KEY$ ( CAD-KIND:region CAD-KIND:target-id -- ptr u8 n )
   {: r:CAD-KIND:region target:CAD-KIND:target-id :}
   r SK-REGION-CK drop  target TARGET:VALIDATE drop
   SB-RESET r target SK-KEY+ SB$ ;

\ ---- the typed region-derived part of the section-7.4 key -------------------
\ SK-KEY builds the nine region-derived facts SK-KEY+ renders, but as a typed
\ `skey` record: the dims through the shared DIM>CLASS encoder, dtype/layout
\ straight off the typed MIR accessors, alignment lifted from REGION-ALIGN's
\ ordinal min-fold through >ALIGN, and the requested numeric policy from REGION-POL.
\ Assembly is positional and typed, so a dtype/layout
\ (or any enum-field) role swap is a checker reject. The families ride the stack
\ into MAKE (they cannot bind into locals); only the region ids are locals.
\ MAKI-SKEY:EQ compares the region-derived part of the durable identity. With
\ target and process identity fixed, its result corresponds to SK-KEY$ text
\ equality (pinned in sched-key-test.f).
: SK-KEY ( CAD-KIND:region -- skey ) {: r:CAD-KIND:region :}
   r SK-REGION-CK drop
   r REGION-REP {: rep:CAD-KIND:node-id :}
   r RSIG
   rep MIR-ROWS@ ROWS-RAW DIM>CLASS
   rep MIR-COLS@ COLS-RAW DIM>CLASS
   rep MIR-DT@
   rep MIR-LAY@
   r REGION-ALIGN >ALIGN
   r REGION-POL
   MAKI-SKEY:MAKE ;

private

\ ---- replay table (cad-5 store seam: in-memory key -> selection) -------------
\ DURABLE-TEXT BOUNDARY (dot habu-cad-adt-swap). This in-memory table keys on the
\ canonical SK-KEY$ RENDER (interned bytes, STR=), NOT on the typed `skey` record,
\ and that is deliberate: the durable store maki/store.f (schedules.rows) is a
\ persistent, line-oriented TEXT file that outlives the IR, and the rehydration
\ path maki/store-replay.f STORE-REPLAY-LOAD replays it back through this table by
\ feeding SCHED-LOAD's callback ONLY the stored key TEXT (store-replay-test.f even
\ replays synthetic "sk<n>" keys that were never region keys). There are no region
\ facts at load time, so a text key cannot be re-keyed into a `skey`, and writing a
\ text-to-structure parser is explicitly out of scope. The typed key still closes the
\ semantic-role hole where it matters - at CONSTRUCTION (SK-KEY assembles typed
\ fields; a role swap is a checker reject) - and SK-KEY$ embeds it alongside target,
\ engine, and ptxas. With target and process identity fixed, STR= over the render
\ corresponds to MAKI-SKEY:EQ over its region-derived fields; across those external
\ identities the durable text intentionally distinguishes values `skey` cannot
\ observe. Migrating this table to
\ parallel typed columns keyed by MAKI-SKEY:EQ waits on the W>1 typed-column store
\ (dot habu-checker-capability-typed-a480c423 S2); until then the table stays
\ text-keyed as the durable store's in-memory mirror.
\ GROWTH, NOT EVICTION (dot habu-maki-sk-table-59bb1d4d). This table is the
\ COMPLETE in-memory mirror of the durable schedules.rows: REPLAY-ENSURE
\ (maki/store-replay.f) merges the whole file, and SK-GET must replay ANY
\ stored selection. An eviction policy would silently drop durable selections
\ (an evicted key's TILE misses -> defaults -> re-certify -> re-PROMOTE appends
\ a duplicate row), thrashing the append-only store the mirror exists to serve.
\ So a full table GROWS instead: the entry columns are lib/vector.f cell
\ vectors (doubling), and the interned key bytes grow by 64K spans
\ (lib/memory.f, the lib/json-write.f pattern). Both allocate lazily at first
\ PUT - never at load time, so no mmap pointer can bake into a snapshot. A
\ fresh session's mirror is bounded by the store read cap (E-STORE-FULL in
\ maki/store.f); in-session growth is bounded by OS allocation (E-MEM-MAP).
32    constant SK-TAB-CAP0           \ boot entry capacity (vectors double on demand)
$2000 constant SK-ARENA-CAP0         \ boot key-arena bytes (~32 facts-based keys)

\ ---- raw table cell -> CAD-NUM role bridges for the typed VEC surface ---------
\ The replay table stores raw cells: a boot capacity, and per-entry key offsets /
\ lengths / selections. The typed VEC surface (package VEC) reads a validated
\ CAD-NUM role - a length/capacity is a `CAD-NUM:item-count`, an entry position is
\ a `CAD-NUM:index` - so a count/index swap at a VEC call is a checker reject.
\ These lift a nonnegative table cell to its role through the PUBLIC CAD-NUM
\ validators (no laundering back to n, no reopened package). The refusal arms are
\ unreachable invariants (a boot capacity and a live entry index are nonnegative);
\ an impossible negative surfaces the vector's own capacity / bounds code. This is
\ the lib/vector.f VECT-N>ITEM / VECT-N>INDEX idiom, kept sched-key-local.
: SK>ITEM ( n -- CAD-NUM:item-count )
   CAD-NUM:ITEM-COUNT
   MATCH CAD-NUM:numeric-result
      ok OF ENDOF                             negative OF E-VEC-CAPACITY throw ENDOF
      zero OF E-VEC-CAPACITY throw ENDOF        overflow OF E-VEC-CAPACITY throw ENDOF
      underflow OF E-VEC-CAPACITY throw ENDOF   bad-alignment OF E-VEC-CAPACITY throw ENDOF
      misaligned OF E-VEC-CAPACITY throw ENDOF
   ;MATCH ;
: SK>INDEX ( n -- CAD-NUM:index )
   CAD-NUM:INDEX
   MATCH CAD-NUM:numeric-result
      ok OF ENDOF                             negative OF E-VEC-BOUNDS throw ENDOF
      zero OF E-VEC-BOUNDS throw ENDOF          overflow OF E-VEC-BOUNDS throw ENDOF
      underflow OF E-VEC-BOUNDS throw ENDOF     bad-alignment OF E-VEC-BOUNDS throw ENDOF
      misaligned OF E-VEC-BOUNDS throw ENDOF
   ;MATCH ;

create SK-KO-VEC  VEC:HEADER-CELLS cells allot   \ per-entry key offset
create SK-KL-VEC  VEC:HEADER-CELLS cells allot   \ per-entry key length
create SK-SEL-VEC VEC:HEADER-CELLS cells allot   \ per-entry selection (candidate index)
variable SK-VECS?                    \ number of entry vectors allocated; 3 means ready
variable SK-ENTRY-N
variable SK-ARENA-A                  \ interned key bytes (lazy mmap; grows)
variable SK-ARENA-CAP
variable SK-ARENA-U

: SK-ARENA-FIELD ( -- ptr ptr u8 )  SK-ARENA-A 0 ptr-field ;
: SK-ARENA@ ( -- ptr u8 )  SK-ARENA-FIELD @ ;

: SK-VEC-INIT1 ( ptr h n -- ) {: vec:ptr step:n :}
   step SK-VECS? @ < if exit then
   vec SK-TAB-CAP0 SK>ITEM VEC:INIT
   step 1+ SK-VECS? ! ;

\ Atomic lazy entry-vector allocation (first PUT). The three entry vectors are ONE
\ owned resource: readiness is published only after all three carry storage. The
\ progress count makes retry resume after the last successful allocation.
: SK-TAB-ENSURE ( -- )
   SK-VECS? @ 3 = if exit then
   SK-KO-VEC  0 SK-VEC-INIT1
   SK-KL-VEC  1 SK-VEC-INIT1
   SK-SEL-VEC 2 SK-VEC-INIT1 ;

: SK-ARENA-COPY-OLD ( ptr u8 -- ) {: dst:ptr :}
   SK-ARENA-U @ 0 > if SK-ARENA@ dst SK-ARENA-U @ BYTE-COPY then ;

: SK-ARENA-SPAN! ( ptr u8 n -- )  SK-ARENA-CAP !  SK-ARENA-FIELD ! ;

: SK-ARENA-GROW ( n -- )             \ total bytes needed; allocates in 64K spans
   dup SK-ARENA-CAP0 < if drop SK-ARENA-CAP0 then
   MEM-ALLOC-64K-SPAN
   over SK-ARENA-COPY-OLD
   SK-ARENA-SPAN! ;

: SK-ARENA-ROOM ( n -- ) {: add:n :} \ ensure room for add more interned bytes
   SK-ARENA-U @ add + SK-ARENA-CAP @ > if SK-ARENA-U @ add + SK-ARENA-GROW then ;

: SK-INTERN ( ptr u8 n -- n n ) {: a:ptr u:n :}
   u SK-ARENA-ROOM
   SK-ARENA-U @ {: off:n :}
   a  SK-ARENA@ off +  u BYTE-COPY
   off u + SK-ARENA-U !
   off u ;

: SK-N ( -- n )
   SK-VECS? @ 3 <> if 0 exit then
   SK-ENTRY-N @ ;

: SK-ENTRY$ ( n -- ptr u8 n ) {: i:n :}
   SK-ARENA@ SK-KO-VEC i SK>INDEX VEC:@ +  SK-KL-VEC i SK>INDEX VEC:@ ;

: SK-FIND ( ptr u8 n -- n ) {: a:ptr u:n :}      \ key -> entry index or -1
   SK-N 0 ?do  a u i SK-ENTRY$ STR= if i unloop exit then  loop  -1 ;

\ ---- reserve / commit: all allocation up front, publish allocation-free -------
\ Every fallible step of an insertion - backing the entry vectors, growing their
\ capacity, growing the key arena - is a RESERVE; the matching COMMIT interns the key
\ and pushes the three cells into already-reserved capacity, so it allocates nothing
\ and cannot throw. A throw during RESERVE leaves the arena bytes below SK-ARENA-U,
\ every vector length, and therefore every query answer byte-identical (only spare
\ capacity may have grown - SK-TAB-RESET already documents grown capacity as
\ persistent); a retry re-reserves over the now-sufficient capacity and commits, with
\ no duplicate entry and no divergent KO/KL/SEL lengths. This is the store's
\ transactional seam: the replay load reserves the whole batch before it applies any
\ row, and the durable write reserves before the file append so the hot publish that
\ follows the append is infallible.
: SK-VEC-RESERVE ( n -- ) {: need:n :}           \ ensure each entry vector holds `need` items
   SK-KO-VEC  need SK>ITEM VEC:ENSURE
   SK-KL-VEC  need SK>ITEM VEC:ENSURE
   SK-SEL-VEC need SK>ITEM VEC:ENSURE ;

: SK-RESERVE ( n n -- ) {: add:n bytes:n :}       \ room for `add` new entries + `bytes` key bytes
   SK-TAB-ENSURE
   SK-N add + SK-VEC-RESERVE
   bytes SK-ARENA-ROOM ;

: SK-APPEND1 ( ptr u8 n n -- ) {: a:ptr u:n sel:n :}  \ publish one NEW entry into reserved capacity
   a u SK-INTERN {: off:n len:n :}               \ arena room reserved -> the copy cannot grow
   off SK-KO-VEC  VEC:PUSH drop                  \ vector capacity reserved -> the pushes cannot grow
   len SK-KL-VEC  VEC:PUSH drop
   sel SK-SEL-VEC VEC:PUSH drop
   SK-ENTRY-N @ 1+ SK-ENTRY-N ! ;

: SK-PLACE ( ptr u8 n n n -- ) {: a:ptr u:n sel:n e:n :}  \ e>=0: update slot; e<0: append (reserved)
   e 0 < 0= if sel SK-SEL-VEC e SK>INDEX VEC:! exit then
   a u sel SK-APPEND1 ;

\ ---- durable-write split (maki/store-replay.f SK-PUT-DURABLE) -------------------
\ STAGE reserves the hot entry for a new key (nothing observable changes); after the
\ durable append lands, COMMIT publishes with a step that cannot fail. A crash or throw
\ between the two is exactly the staged-but-uncommitted state, which no query can see.
: SK-PUT-STAGE ( ptr u8 n -- ) {: a:ptr u:n :}
   a u SK-FIND 0 < if 1 u SK-RESERVE then ;      \ new key -> reserve; existing -> no allocation
: SK-PUT-COMMIT ( ptr u8 n n -- ) {: a:ptr u:n sel:n :}  \ infallible after a matching SK-PUT-STAGE
   a u sel  a u SK-FIND  SK-PLACE ;

public

: SK-TAB-RESET ( -- )                \ empty the table; grown capacity persists
   0 SK-ARENA-U !
   0 SK-ENTRY-N !
   SK-VECS? @ 3 = if
      SK-KO-VEC VEC:CLEAR  SK-KL-VEC VEC:CLEAR  SK-SEL-VEC VEC:CLEAR
   then ;
: SK-TAB-COUNT ( -- n )  SK-N ;

: SK-PUT ( ptr u8 n n -- ) {: a:ptr u:n sel:n :}  \ key selection -> store / update (atomic)
   a u SK-FIND {: e:n :}
   e 0 < if 1 u SK-RESERVE then                  \ NEW key: reserve one entry (the only fallible step)
   a u sel e SK-PLACE ;                           \ publish: in-place update or reserved append (infallible)

\ cad-5 store seam: a miss returns (-1 false) so the caller uses the defaults.
: SK-GET ( ptr u8 n -- n bool ) {: a:ptr u:n :}
   a u SK-FIND {: e:n :}
   e 0 < if -1 false exit then
   SK-SEL-VEC e SK>INDEX VEC:@  true ;

;package
