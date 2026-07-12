\ maki/sched-key.f - schedule cache/replay keys + the cad-5 replay-table seam (cad-4).
\
\ CAD-PLAN section 7.4. The cache and replay key over a fusion region is
\ (region signature, shape class, dtype key, layout key, alignment class, target,
\ engine hash, ptxas version). This file renders that whole key as one string and
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
\ Typed key (dot habu-cad-adt-swap): SK-KEY builds the same eight facts as a typed
\ `skey` PRODUCT (dimclass/dtype/layout/align enum fields), so a semantic-field role
\ swap at construction is a CHECKER reject, and MAKI-SKEY:EQ is the typed identity.
\ SK-KEY$ stays the ONE durable render; the in-memory replay table keys on that
\ render (STR=), a documented durable-text boundary (see the replay-table note) - a
\ typed-column table awaits the W>1 typed store (habu-checker-capability-typed S2).
\
\ Alignment class: the most conservative model-input alignment the region reads
\ (AL-16 when it reads no model input - compiler-allocated buffers are aligned by
\ construction). Target is the "sm_87" v1 constant; the engine hash is the real
\ SHA-256 content key over bin/hb (lib/engine-id.f, resolved engine-side, lazy +
\ cached); ptxas version is the honest "unprobed" placeholder (no ptxas is probed
\ on a host without a device).
\
\ Replay: a bounded in-memory key->selection table with GET/PUT. This is the cad-5
\ store SEAM - a query that misses returns (-1 false) so the caller falls back to the
\ closed-form defaults ("unmeasured shape class -> using defaults"), since cad-4 has no
\ measurements (those land in cad-5/cad-6).
\
\ Fail closed: an out-of-range region id or alignment class and a table/arena overflow
\ are named throws. maki -> habu only; sched-key owns -5084..-5086.

require lib/prelude.f
require lib/string.f
require lib/float.f
require lib/fmt.f
require lib/engine-id.f
require maki/model-ir.f
require maki/fusion-plan.f
require maki/schedule.f

-5084 constant E-SK-REGION     \ region id out of range / empty
-5085 constant E-SK-ALIGN      \ alignment class out of range (AL-* domain)
-5086 constant E-SK-FULL       \ replay table / key arena capacity exceeded

package MAKI
public

\ ---- typed schedule key: the shape-class family + the SKEY product record ----
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
\ declaration throws). target / engine / ptxas are per-process invariants (one
\ target, one engine build, one ptxas per running process): the in-memory key
\ never spans them, so they live in the DURABLE render (SK-KEY$) ONLY and are not
\ product fields. The persistent schedules.rows store stays keyed by that render
\ (see the SK-GET/SK-PUT boundary note below). Fields (slot order, deepest first):
\ rsig = FNV-1a region signature (n); rk/rm = representative rows class+magnitude;
\ ck/cm = representative cols class+magnitude; dt/lay = representative dtype/layout;
\ al = region alignment class. (Inline `\` notes inside a PRODUCT block reject.)
PRODUCT skey 0 DERIVE eq
  FIELD rsig n
  FIELD rk dimclass
  FIELD rm n
  FIELD ck dimclass
  FIELD cm n
  FIELD dt dtype
  FIELD lay layout
  FIELD al align
;PRODUCT

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
   node MIR-LAY@ LAYOUT>N FNV-CELL ;

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
\ the SINGLE source that both SK-KEY (the typed product) and DIM-CLASS+ (the
\ durable render) classify through, so field-eq and text-eq can never diverge.
\ DIM>CLASS is public: it is the shape classifier the typed key and the tests
\ share (the field-eq == text-eq contract is pinned over it).
public
: DIM>CLASS ( e -- dimclass n )                 \ extent -> (class, magnitude)
   dup 0=    if drop MAKI-DIMCLASS:UNBOUND 0 exit then
   dup 64 <= if MAKI-DIMCLASS:EXACT swap exit then
   dup POW2? if NEXT-POW2 MAKI-DIMCLASS:POW2 swap exit then
   NEXT-POW2 MAKI-DIMCLASS:POW2-TAIL swap ;
private

: DIMCLASS+ ( dimclass n -- ) {: m:n :}         \ append one dim's class text to SB
   MATCH dimclass
      exact     OF m SB-INT ENDOF
      pow2      OF $70 SB-APPEND-C m SB-INT ENDOF
      pow2-tail OF $70 SB-APPEND-C m SB-INT s" +t" SB-APPEND ENDOF
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

\ ---- key field placeholders -------------------------------------------------
: SK-TARGET$ ( -- ptr u8 n )  s" sm_87" ;         \ single supported target (v1)
\ Real engine content key: the SHA-256 of bin/hb, resolved engine-side from the
\ kernel-provided self-path and hashed once on first request, then cached
\ (lib/engine-id.f). It distinguishes schedules produced by different engine builds
\ so a schedules.rows written by one engine is never replayed under another; the
\ lazy+cached hash keeps it off the interactive key-render hot path.
: SK-ENGINE$ ( -- ptr u8 n )  ENGINE-KEY$ ;
: SK-PTXAS$  ( -- ptr u8 n )  s" unprobed" ;       \ no ptxas probed off-device

\ representative (output) node of a region - the default-context source (rowlen/dtype)
: SK-REGION-REP ( CAD-KIND:region -- CAD-KIND:node-id )  SK-REGION-CK REGION-REP ;

\ ---- individual key fields (standalone renders, for inspection + tests) ------
: SK-RSIG$ ( CAD-KIND:region -- ptr u8 n )  SK-REGION-CK RSIG SB-RESET SK-HEX+ SB$ ;
: SK-SHAPE-CLASS$ ( CAD-KIND:rows CAD-KIND:cols -- ptr u8 n )
   SB-RESET SHAPE-CLASS+ SB$ ;
: SK-ALIGN$ ( CAD-KIND:region -- ptr u8 n )  SK-REGION-CK REGION-ALIGN AL-KEY ;

\ ---- the full section 7.4 key as one "|"-joined string ----------------------
: SK-KEY+ ( CAD-KIND:region -- ) {: r:CAD-KIND:region :}   \ append the key to SB (already reset)
   r REGION-REP {: rep:CAD-KIND:node-id :}
   r RSIG SK-HEX+
   $7C SB-APPEND-C  rep MIR-ROWS@ rep MIR-COLS@ SHAPE-CLASS+
   $7C SB-APPEND-C  rep MIR-DTYPE-KEY  SB-APPEND
   $7C SB-APPEND-C  rep MIR-LAYOUT-KEY SB-APPEND
   $7C SB-APPEND-C  r REGION-ALIGN AL-KEY SB-APPEND
   $7C SB-APPEND-C  SK-TARGET$ SB-APPEND
   $7C SB-APPEND-C  SK-ENGINE$ SB-APPEND
   $7C SB-APPEND-C  SK-PTXAS$  SB-APPEND ;

: SK-KEY$ ( CAD-KIND:region -- ptr u8 n )  SK-REGION-CK SB-RESET SK-KEY+ SB$ ;

\ ---- the typed section-7.4 key (the durable render's semantic twin) ----------
\ SK-KEY builds the same eight facts SK-KEY+ renders, but as a typed `skey`
\ record: the dims through the shared DIM>CLASS encoder, dtype/layout straight
\ off the typed MIR accessors, alignment lifted from REGION-ALIGN's ordinal
\ min-fold through >ALIGN. Assembly is positional and typed, so a dtype/layout
\ (or any enum-field) role swap is a checker reject. The families ride the stack
\ into MAKE (they cannot bind into locals); only the region ids are locals.
\ MAKI-SKEY:EQ over two SK-KEY values is the typed identity the durable string
\ key serializes (field-eq == SK-KEY$ text-eq, pinned in sched-key-test.f).
: SK-KEY ( CAD-KIND:region -- skey ) {: r:CAD-KIND:region :}
   r SK-REGION-CK drop
   r REGION-REP {: rep:CAD-KIND:node-id :}
   r RSIG
   rep MIR-ROWS@ ROWS-RAW DIM>CLASS
   rep MIR-COLS@ COLS-RAW DIM>CLASS
   rep MIR-DT@
   rep MIR-LAY@
   r REGION-ALIGN >ALIGN
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
\ text->product parser is explicitly out of scope. The typed key still closes the
\ semantic-role hole where it matters - at CONSTRUCTION (SK-KEY assembles typed
\ fields; a role swap is a checker reject) - and SK-KEY$ is an injective encoding
\ of the typed key (field-eq == text-eq, pinned in sched-key-test.f), so STR= over
\ the render decides exactly what MAKI-SKEY:EQ would. Migrating this table to
\ parallel typed columns keyed by MAKI-SKEY:EQ waits on the W>1 typed-column store
\ (dot habu-checker-capability-typed-a480c423 S2); until then the table stays
\ text-keyed as the durable store's in-memory mirror.
32   constant SK-TAB-CAP
$1000 constant SK-ARENA-CAP
create SK-ARENA SK-ARENA-CAP allot   variable SK-ARENA-U
create SK-KO  SK-TAB-CAP cells allot     \ per-entry key offset
create SK-KL  SK-TAB-CAP cells allot     \ per-entry key length
create SK-SEL SK-TAB-CAP cells allot     \ per-entry selection (candidate index)
variable SK-TAB-N

: SK-INTERN ( ptr u8 n -- n n ) {: a:ptr u:n :}
   SK-ARENA-U @ u + SK-ARENA-CAP > if E-SK-FULL throw then
   SK-ARENA-U @ {: off:n :}
   a  SK-ARENA off +  u BYTE-COPY
   off u + SK-ARENA-U !
   off u ;

: SK-ENTRY$ ( n -- ptr u8 n ) {: i:n :}
   SK-ARENA i cells SK-KO + @ +  i cells SK-KL + @ ;

: SK-FIND ( ptr u8 n -- n ) {: a:ptr u:n :}      \ key -> entry index or -1
   SK-TAB-N @ 0 ?do  a u i SK-ENTRY$ STR= if i unloop exit then  loop  -1 ;

public

: SK-TAB-RESET ( -- )  0 SK-ARENA-U !  0 SK-TAB-N ! ;
: SK-TAB-COUNT ( -- n )  SK-TAB-N @ ;

: SK-PUT ( ptr u8 n n -- ) {: a:ptr u:n sel:n :}  \ key selection -> store / update
   a u SK-FIND {: e:n :}
   e 0 < 0= if sel e cells SK-SEL + ! exit then   \ update existing key
   SK-TAB-N @ SK-TAB-CAP >= if E-SK-FULL throw then
   a u SK-INTERN {: off:n len:n :}
   off SK-TAB-N @ cells SK-KO  + !
   len SK-TAB-N @ cells SK-KL  + !
   sel SK-TAB-N @ cells SK-SEL + !
   SK-TAB-N @ 1+ SK-TAB-N ! ;

\ cad-5 store seam: a miss returns (-1 false) so the caller uses the defaults.
: SK-GET ( ptr u8 n -- n bool ) {: a:ptr u:n :}
   a u SK-FIND {: e:n :}
   e 0 < if -1 false exit then
   e cells SK-SEL + @  true ;

end-package
