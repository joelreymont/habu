\ source.f - the compiler source registry: module-local source identities,
\ byte spans, origin chains, and stable content digests.
\
\ docs/compiler-ir-design.md sections 6.2 (the context's source registry
\ member), 6.3 (the source table in the canonical serialization order), 7.1
\ (the stage N0 source tape binds to source digests), and plan item IR-0.2.
\ One registry serves one module. Its rows live on one IR-ARENA arena owned by
\ the compilation context, so the registry dies with its context, freezes with
\ its arena, and adds no storage mechanism of its own. Source identities are
\ the existing IR-ID ir-source-id family packed under the registry's module
\ key; this file mints no parallel identity family and no raw converter.
\
\ ROW SHAPE. The arena carries a three-cell header (format tag, owning module
\ serial, committed source capacity) followed by one six-cell row per source:
\ byte length, the four digest words, and the origin cell. The header binds
\ the arena to exactly one module key, so a foreign key or a foreign module's
\ source-id rejects with a named error before any row is touched. Ownership is
\ the same possession discipline as IR-ARENA: holding the context, the arena,
\ and the module key IS registry ownership, and each of the three is a sealed
\ nominal that checked code cannot forge from raw cells. A holder who bypasses
\ this package and appends raw cells to the arena misaligns the row shape,
\ which every operation rechecks fail-closed (E-IR-SRC-STATE).
\
\ CONTENT AND DIGESTS. The registry does not retain source bytes. A
\ registration digests the presented bytes - SHA-256 over exactly those bytes,
\ via CDIGEST:COMPUTE - and records length, digest, and origin. Equal bytes
\ therefore digest identically regardless of registration order, registry, or
\ module, which is what lets a later context cache deduplicate identical
\ content. Identity never deduplicates: every registration mints the next
\ module-local ordinal, so two registrations of the same bytes are two
\ sources whose content equality is observable only through their digests.
\ The record-preimage tag discipline of digest.f is for structured records;
\ source content is raw bytes, and its digest is the plain content hash the
\ section 7.1 source tape binds to.
\
\ BYTE SPANS ARE ENCODING-AGNOSTIC. A span is (source-id, start, length) in
\ bytes, validated against the registered byte length only: negative bounds,
\ overflowing sums, and spans crossing their source's end reject with
\ E-IR-SRC-SPAN. This layer knows nothing about UTF-8 or any other encoding;
\ the design places text validation at the stage N0 source-tape/lexer
\ boundary (section 7.1), not in the registry.
\
\ ORIGIN CHAINS ARE ACYCLIC BY CONSTRUCTION. An origin (include or expansion
\ parent) is recorded at registration and must name an already registered
\ source of the same registry, so a parent's ordinal is strictly below its
\ child's. Any cycle would need some member to name a not-yet-registered
\ ordinal, and that edge rejects with E-IR-SRC-ORIGIN - a self cycle and a
\ multi-node cycle die identically. Chain walks re-verify the strict decrease
\ on every step (E-IR-SRC-STATE on a corrupted row), so a walk terminates on
\ any registry state.
\
\ The IR-CTX SOURCES@ slot stays fail-closed unbound. An arena handle is a
\ sealed nominal a stored raw cell cannot re-mint (arena.f, STALE HANDLES),
\ so persisting a registry reference inside the context header is its own
\ designed capability (dot habu-bind-ctx-src-df121c31), not a side effect of
\ this file.

require lib/prelude.f
require lib/errors.f
require src/compiler/digest.f
require src/compiler/ir/id.f
require src/compiler/ir/context.f
require src/compiler/ir/arena.f

package IR-SOURCE
public

\ A validated byte span into one registered source. A value, not authority:
\ SPAN mints it validated, and every consumer revalidates with SPAN-CK or
\ FSPAN-CK, so a span assembled by the generated constructor gains nothing.
\ Its source-id field still needs a genuine identity - the checker rejects a
\ raw cell there - and no public word re-mints ids or handles from raw cells.
STRUCTURE span 0
   FIELD src IR-ID:ir-source-id
   FIELD start n
   FIELD len n
;STRUCTURE

private

\ The one raw crossing this package needs: one-way projections of the sealed
\ IR-ID identities onto their serials, for header binding and owner
\ comparison. Nothing in this package re-mints a raw cell into a nominal.
CAST: KEY-SERIAL ( IR-ID:ir-module-key -- n )
CAST: MID-SERIAL ( IR-ID:ir-module-id -- n )

\ ---- layout ------------------------------------------------------------------
$53524331 constant SRC-MAGIC         \ "SRC1": the registry header format tag
0 constant HC-MAGIC
1 constant HC-SERIAL
2 constant HC-CAP
3 constant HDR-CELLS
0 constant OFF-LEN
1 constant OFF-DW0
2 constant OFF-DW1
3 constant OFF-DW2
4 constant OFF-DW3
5 constant OFF-ORG
6 constant ROW-CELLS
0 constant ORG-NONE                  \ origin cell of a root source
public
$FFFFFFFF HDR-CELLS - ROW-CELLS / constant CAP-MAX
private

\ ---- cell access -------------------------------------------------------------
\ Every read below goes through an IR-ARENA reader: the registry is resolved
\ ONCE, at the public word, and the helpers take the resolved reader. The
\ live/frozen twins that used to run down this file collapse into one set,
\ because a reader carries the state it was opened against and refuses the
\ other with the error the handle would have given - the only thing the two
\ entry points still differ in is OPEN-LIVE against OPEN.

\ ---- header and shape --------------------------------------------------------
: SHAPE-CK ( n -- )
   dup HDR-CELLS < if E-IR-SRC-STATE throw then
   HDR-CELLS - ROW-CELLS mod 0 <> if E-IR-SRC-STATE throw then ;

: MAGIC-CK ( n -- )
   SRC-MAGIC <> if E-IR-SRC-STATE throw then ;

: HDR-CK ( IR-ARENA:reader -- )
   {: r:IR-ARENA:reader :}
   r IR-ARENA:RD-SIZE SHAPE-CK
   r HC-MAGIC IR-ARENA:RD@ MAGIC-CK ;

: USED>CNT ( n -- n )
   HDR-CELLS - ROW-CELLS / ;

: CNT ( IR-ARENA:reader -- n )
   IR-ARENA:RD-SIZE USED>CNT ;

\ ---- ownership ---------------------------------------------------------------
: SERIAL-CK ( n n -- )
   <> if E-IR-SRC-OWNER throw then ;

: KEY-CK ( IR-ARENA:reader IR-ID:ir-module-key -- )
   {: r:IR-ARENA:reader key:IR-ID:ir-module-key :}
   r HDR-CK
   r HC-SERIAL IR-ARENA:RD@ key KEY-SERIAL SERIAL-CK ;

: ID-OWNER-SERIAL ( IR-ID:ir-source-id -- n )
   IR-ID:SOURCE-OWNER MID-SERIAL ;

\ Validate a presented source-id against a resolved (header serial, count):
\ minted under this registry's module, ordinal below the registered count.
: ID-CK-N ( n n IR-ID:ir-source-id -- n )
   {: hs:n cnt:n id:IR-ID:ir-source-id :}
   hs id ID-OWNER-SERIAL SERIAL-CK
   id IR-ID:SOURCE-LOCAL
   dup cnt >= if E-IR-SRC-BOUND throw then ;

: ID-CK ( IR-ARENA:reader IR-ID:ir-source-id -- n )
   {: r:IR-ARENA:reader id:IR-ID:ir-source-id :}
   r HDR-CK
   r HC-SERIAL IR-ARENA:RD@ r CNT id ID-CK-N ;

\ ---- row addressing ----------------------------------------------------------
: ROW-CELL ( n n -- n )
   swap ROW-CELLS * HDR-CELLS + + ;

: RC@ ( IR-ARENA:reader n n -- n )
   ROW-CELL IR-ARENA:RD@ ;

\ ---- creation ----------------------------------------------------------------
: CAP-OK ( n -- )
   dup 1 < over CAP-MAX > or if E-IR-SRC-CAP throw then
   drop ;

public

\ Create a module's source registry: an IR-ARENA arena owned by ctx, its cell
\ ceiling committed to exactly cap rows, its header bound to key's module
\ serial. The arena handle plus the key is the registry; both stay with the
\ module owner, and the registry dies with the owning context.
: NEW ( IR-CTX:ctx IR-ID:ir-module-key n -- IR-ARENA:arena )
   {: c:IR-CTX:ctx key:IR-ID:ir-module-key cap:n :}
   cap CAP-OK
   c cap ROW-CELLS * HDR-CELLS + IR-ARENA:NEW {: a:IR-ARENA:arena :}
   c a HDR-CELLS IR-ARENA:RESERVE
   c a SRC-MAGIC IR-ARENA:PUSH drop
   c a key KEY-SERIAL IR-ARENA:PUSH drop
   c a cap IR-ARENA:PUSH drop
   a ;

\ ---- registration ------------------------------------------------------------
private

: ROOM-CK ( IR-ARENA:reader -- )
   {: r:IR-ARENA:reader :}
   r CNT r HC-CAP IR-ARENA:RD@ >= if E-IR-SRC-CAP throw then ;

\ Append one validated row and report its module-local ordinal. The capacity
\ check comes first, so a full registry still answers its own named error; the
\ reservation then makes the whole row's storage real, so none of the six
\ appends can allocate. The digest is in hand before the reservation as well, so
\ between the first cell of the row and the last there is nothing left that can
\ fail - which is what makes the six appends one commit.
\ The reader is opened once and outlives the appends: a PUSH changes neither
\ the registry generation nor the state, and the reader re-reads the row's
\ pointer and count on every call, so it follows the reservation's new span.
: ROW-PUT ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:reader n n CDIGEST:digest -- n )
   CDIGEST-DIGEST:UNMAKE {: w0:n w1:n w2:n w3:n :}
   {: c:IR-CTX:ctx a:IR-ARENA:arena r:IR-ARENA:reader u:n org:n :}
   u 0 < if E-IR-SRC-LEN throw then
   r ROOM-CK
   c a ROW-CELLS IR-ARENA:RESERVE
   r CNT {: l:n :}
   c a u IR-ARENA:PUSH drop
   c a w0 IR-ARENA:PUSH drop
   c a w1 IR-ARENA:PUSH drop
   c a w2 IR-ARENA:PUSH drop
   c a w3 IR-ARENA:PUSH drop
   c a org IR-ARENA:PUSH drop
   l ;

\ The same row, for a caller that has the bytes and no digest of them yet.
: ROW-ADD ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:reader ptr u8 n n -- n )
   {: c:IR-CTX:ctx a:IR-ARENA:arena r:IR-ARENA:reader p u:n org:n :}
   c a r  u org  p u CDIGEST:COMPUTE  ROW-PUT ;

\ An origin must already be a registered source of this same registry, so a
\ parent ordinal is strictly below its child's and no chain can close: a self
\ cycle or any multi-node cycle presents a not-yet-registered ordinal and
\ dies here with a named error.
: ORIGIN-CK ( IR-ARENA:reader IR-ID:ir-source-id -- n )
   {: r:IR-ARENA:reader parent:IR-ID:ir-source-id :}
   r HC-SERIAL IR-ARENA:RD@ parent ID-OWNER-SERIAL SERIAL-CK
   parent IR-ID:SOURCE-LOCAL
   dup r CNT >= if E-IR-SRC-ORIGIN throw then
   1+ ;

public

\ Register a root source: digest the bytes, append the row, mint the next
\ module-local identity under key. The bytes are not retained.
: REGISTER ( IR-CTX:ctx IR-ARENA:arena IR-ID:ir-module-key ptr u8 n -- IR-ID:ir-source-id )
   {: c:IR-CTX:ctx a:IR-ARENA:arena key:IR-ID:ir-module-key p u:n :}
   a IR-ARENA:OPEN-LIVE {: r:IR-ARENA:reader :}
   r key KEY-CK
   c a r p u ORG-NONE ROW-ADD
   key swap IR-ID:PACK-SOURCE ;

\ Register a source whose origin - its include or expansion parent - is an
\ already registered source of this registry.
: REGISTER-FROM ( IR-CTX:ctx IR-ARENA:arena IR-ID:ir-module-key IR-ID:ir-source-id ptr u8 n -- IR-ID:ir-source-id )
   {: c:IR-CTX:ctx a:IR-ARENA:arena key:IR-ID:ir-module-key parent:IR-ID:ir-source-id p u:n :}
   a IR-ARENA:OPEN-LIVE {: r:IR-ARENA:reader :}
   r key KEY-CK
   r parent ORIGIN-CK {: org:n :}
   c a r p u org ROW-ADD
   key swap IR-ID:PACK-SOURCE ;

\ ---- live readers ------------------------------------------------------------
: SOURCES ( IR-ARENA:arena -- n )
   IR-ARENA:OPEN-LIVE dup HDR-CK CNT ;

: LEN@ ( IR-ARENA:arena IR-ID:ir-source-id -- n )
   {: a:IR-ARENA:arena id:IR-ID:ir-source-id :}
   a IR-ARENA:OPEN-LIVE {: r:IR-ARENA:reader :}
   r id ID-CK {: l:n :}
   r l OFF-LEN RC@ ;

\ FOUR DIGEST WORDS OFF ONE RESOLUTION. This is the shape the reader exists
\ for: the row is validated once and its four cells are then four loads.
: DIGEST@ ( IR-ARENA:arena IR-ID:ir-source-id -- CDIGEST:digest )
   {: a:IR-ARENA:arena id:IR-ID:ir-source-id :}
   a IR-ARENA:OPEN-LIVE {: r:IR-ARENA:reader :}
   r id ID-CK {: l:n :}
   r l OFF-DW0 RC@ r l OFF-DW1 RC@ r l OFF-DW2 RC@ r l OFF-DW3 RC@
   CDIGEST-DIGEST:MAKE ;

: ROOT? ( IR-ARENA:arena IR-ID:ir-source-id -- bool )
   {: a:IR-ARENA:arena id:IR-ID:ir-source-id :}
   a IR-ARENA:OPEN-LIVE {: r:IR-ARENA:reader :}
   r id ID-CK {: l:n :}
   r l OFF-ORG RC@ ORG-NONE = ;

private

\ Decode an origin cell against its child's ordinal: a root rejects, and a
\ parent ordinal that fails the strict decrease is a corrupted row.
: ORG-LOCAL ( n n -- n )
   {: l:n org:n :}
   org ORG-NONE = if E-IR-SRC-ROOT throw then
   org 1-
   dup l >= if E-IR-SRC-STATE throw then ;

public

\ The origin parent's identity. Roots have none: probe with ROOT? first;
\ reading a root's origin throws E-IR-SRC-ROOT.
: ORIGIN@ ( IR-ARENA:arena IR-ID:ir-module-key IR-ID:ir-source-id -- IR-ID:ir-source-id )
   {: a:IR-ARENA:arena key:IR-ID:ir-module-key id:IR-ID:ir-source-id :}
   a IR-ARENA:OPEN-LIVE {: r:IR-ARENA:reader :}
   r key KEY-CK
   r id ID-CK {: l:n :}
   key l r l OFF-ORG RC@ ORG-LOCAL IR-ID:PACK-SOURCE ;

\ The origin-chain length down to the root. Each step re-verifies the strict
\ ordinal decrease, so the walk terminates on any registry state.
\ ONE RESOLUTION FOR THE WHOLE WALK. The chain re-verifies the strict ordinal
\ decrease at every step, so the walk still terminates on any registry state;
\ what it no longer does is re-resolve the registry once per step.
: DEPTH ( IR-ARENA:arena IR-ID:ir-source-id -- n )
   {: a:IR-ARENA:arena id:IR-ID:ir-source-id :}
   a IR-ARENA:OPEN-LIVE {: r:IR-ARENA:reader :}
   r id ID-CK
   0 swap
   begin
      r over OFF-ORG RC@ ORG-NONE <>
   while
      r over OFF-ORG RC@ ORG-LOCAL
      swap 1+ swap
   repeat
   drop ;

\ ---- frozen readers ----------------------------------------------------------
\ A frozen module reads its rows through the arena view; the retired builder
\ handle rejects every mutation with E-IR-ARENA-FROZEN.
: FSOURCES ( IR-ARENA:view -- n )
   IR-ARENA:OPEN dup HDR-CK CNT ;

: FLEN@ ( IR-ARENA:view IR-ID:ir-source-id -- n )
   {: v:IR-ARENA:view id:IR-ID:ir-source-id :}
   v IR-ARENA:OPEN {: r:IR-ARENA:reader :}
   r id ID-CK {: l:n :}
   r l OFF-LEN RC@ ;

: FDIGEST@ ( IR-ARENA:view IR-ID:ir-source-id -- CDIGEST:digest )
   {: v:IR-ARENA:view id:IR-ID:ir-source-id :}
   v IR-ARENA:OPEN {: r:IR-ARENA:reader :}
   r id ID-CK {: l:n :}
   r l OFF-DW0 RC@ r l OFF-DW1 RC@ r l OFF-DW2 RC@ r l OFF-DW3 RC@
   CDIGEST-DIGEST:MAKE ;

: FROOT? ( IR-ARENA:view IR-ID:ir-source-id -- bool )
   {: v:IR-ARENA:view id:IR-ID:ir-source-id :}
   v IR-ARENA:OPEN {: r:IR-ARENA:reader :}
   r id ID-CK {: l:n :}
   r l OFF-ORG RC@ ORG-NONE = ;

: FORIGIN@ ( IR-ARENA:view IR-ID:ir-module-key IR-ID:ir-source-id -- IR-ID:ir-source-id )
   {: v:IR-ARENA:view key:IR-ID:ir-module-key id:IR-ID:ir-source-id :}
   v IR-ARENA:OPEN {: r:IR-ARENA:reader :}
   r key KEY-CK
   r id ID-CK {: l:n :}
   key l r l OFF-ORG RC@ ORG-LOCAL IR-ID:PACK-SOURCE ;

: FDEPTH ( IR-ARENA:view IR-ID:ir-source-id -- n )
   {: v:IR-ARENA:view id:IR-ID:ir-source-id :}
   v IR-ARENA:OPEN {: r:IR-ARENA:reader :}
   r id ID-CK
   0 swap
   begin
      r over OFF-ORG RC@ ORG-NONE <>
   while
      r over OFF-ORG RC@ ORG-LOCAL
      swap 1+ swap
   repeat
   drop ;

\ Carry a registered source from a FROZEN registry into this one, copying its
\ length and its digest instead of taking either again. A pass that rewrites a
\ module builds a new module over the same source, and the source it was
\ compiled from is a fact that module already holds: recomputing the digest
\ from the bytes to compare it with the row that holds it proves only that
\ SHA-256 is a function. So the row is copied, the two modules carry the same
\ source identity by construction rather than by agreement, and a chain of
\ passes digests a word's text once instead of once per module.
\
\ ONLY A ROOT CAN BE CARRIED. An origin is a module-local ordinal into the
\ registry that holds it, and this registry has not registered the parent, so a
\ row with one is refused by name rather than carried with an origin that would
\ point at the wrong source here.
: CARRY ( IR-CTX:ctx IR-ARENA:arena IR-ID:ir-module-key IR-ARENA:view IR-ID:ir-source-id -- IR-ID:ir-source-id )
   {: c:IR-CTX:ctx a:IR-ARENA:arena key:IR-ID:ir-module-key v:IR-ARENA:view id:IR-ID:ir-source-id :}
   a IR-ARENA:OPEN-LIVE {: r:IR-ARENA:reader :}
   r key KEY-CK
   v id FROOT? 0= if E-IR-SRC-ORIGIN throw then
   c a r  v id FLEN@  ORG-NONE  v id FDIGEST@  ROW-PUT
   key swap IR-ID:PACK-SOURCE ;

\ ---- byte spans --------------------------------------------------------------
private

\ Bytes only: start and length validate against the registered byte length;
\ encoding validation is the stage N0 tape's concern, not this layer's.
: RANGE-CK ( n n n -- )
   {: sl:n st:n ln:n :}
   st 0 < ln 0 < or if E-IR-SRC-SPAN throw then
   st ln +
   dup 0 < if E-IR-SRC-SPAN throw then
   sl > if E-IR-SRC-SPAN throw then ;

public

\ Mint a validated span into one registered source of this registry.
: SPAN ( IR-ARENA:arena IR-ID:ir-source-id n n -- IR-SOURCE:span )
   {: a:IR-ARENA:arena id:IR-ID:ir-source-id st:n ln:n :}
   a id LEN@ st ln RANGE-CK
   id st ln IR--SOURCE-SPAN:MAKE ;

\ Revalidate a presented span against the live registry: spans are values,
\ so every consumer rechecks the id and the range at the point of use.
: SPAN-CK ( IR-ARENA:arena IR-SOURCE:span -- )
   IR--SOURCE-SPAN:UNMAKE
   {: a:IR-ARENA:arena id:IR-ID:ir-source-id st:n ln:n :}
   a id LEN@ st ln RANGE-CK ;

: FSPAN-CK ( IR-ARENA:view IR-SOURCE:span -- )
   IR--SOURCE-SPAN:UNMAKE
   {: v:IR-ARENA:view id:IR-ID:ir-source-id st:n ln:n :}
   v id FLEN@ st ln RANGE-CK ;

: SPAN-SRC ( IR-SOURCE:span -- IR-ID:ir-source-id )
   IR--SOURCE-SPAN:UNMAKE drop drop ;

: SPAN-START ( IR-SOURCE:span -- n )
   IR--SOURCE-SPAN:UNMAKE drop nip ;

: SPAN-LEN ( IR-SOURCE:span -- n )
   IR--SOURCE-SPAN:UNMAKE nip nip ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;package
