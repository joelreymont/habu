\ ir-source.f - checked compiler source registry tests.
\
\ Proves the IR-0.2 source-registry contract of src/compiler/ir/source.f:
\ registration mints distinct module-local identities under the module key;
\ equal bytes digest identically independent of registration order and
\ registry while identities never deduplicate; byte spans validate against
\ the registered length and reject negative bounds and crossings with named
\ errors; origins must name already-registered sources, so self and
\ multi-node cycle attempts reject and chain walks terminate; foreign keys,
\ foreign source-ids, foreign contexts, and non-registry or misaligned
\ arenas reject; a frozen module serves every row through the arena view
\ while the retired builder handle rejects mutation; context teardown
\ releases everything; and checker fixtures prove the identity families and
\ the registry API stay sealed.

require lib/test.f
require test/checker-assert.f
require test/compiler/ir-starve.f
require src/compiler/ir/source.f

package IR-SOURCE-TEST
private

\ ---- fixtures ----------------------------------------------------------------
: BND ( -- CBIND:binding )
   CTARGET-ARCH:AARCH64 CTARGET-ABI:AAPCS64-DARWIN CTARGET-ENDIAN:LITTLE
   CTARGET-PTR--WIDTH:BITS64
   CTARGET:F-BASE CTARGET:F-FP CTARGET:WITH CTARGET:CONTRACT
   CNUM-OVERFLOW:TRAP CNUM-FLOAT--MODEL:IEEE754 CNUM-CONTRACTION:FORBIDDEN
   CNUM-FAST--MATH:BIT-EXACT CNUM-COMPARE:IEEE754-UNORDERED CNUM:POLICY
   CBIND:BIND ;

\ A fresh (key, registry) pair for cap sources inside ctx.
: REG-NEW ( IR-CTX:ctx n -- IR-ID:ir-module-key IR-ARENA:arena )
   {: c:IR-CTX:ctx cap:n :}
   c IR-CTX:NEW-MODULE drop {: key:IR-ID:ir-module-key :}
   key c key cap IR-SOURCE:NEW ;

\ ---- registration mints module-local identities ------------------------------
: RG-BODY ( IR-CTX:ctx -- n n bool bool n n )
   {: c:IR-CTX:ctx :}
   c IR-CTX:NEW-MODULE {: key:IR-ID:ir-module-key mid:IR-ID:ir-module-id :}
   c key 8 IR-SOURCE:NEW {: a:IR-ARENA:arena :}
   c a key s" : ONE 1 ;" IR-SOURCE:REGISTER {: s0:IR-ID:ir-source-id :}
   c a key s" : TWO 2 ;" IR-SOURCE:REGISTER {: s1:IR-ID:ir-source-id :}
   s0 IR-ID:SOURCE-LOCAL
   s1 IR-ID:SOURCE-LOCAL
   s0 IR-ID:SOURCE-OWNER mid IR-ID:MODULE-SAME?
   s1 IR-ID:SOURCE-OWNER mid IR-ID:MODULE-SAME?
   a IR-SOURCE:SOURCES
   a s0 IR-SOURCE:LEN@ ;

: RG-CASE ( -- )
   s" registration mints distinct module-local identities" T-LABEL
   BND [: RG-BODY ;] IR-CTX:WITH-CONTEXT
   9 T= 2 T= TTRUE TTRUE 1 T= 0 T= ;

\ ---- stable digests; identity never deduplicates -----------------------------
: DG-BODY ( IR-CTX:ctx -- bool bool bool n n )
   {: c:IR-CTX:ctx :}
   c 8 REG-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena :}
   c a key s" shared bytes" IR-SOURCE:REGISTER {: s0:IR-ID:ir-source-id :}
   c a key s" shared bytes" IR-SOURCE:REGISTER {: s1:IR-ID:ir-source-id :}
   c a key s" other bytes" IR-SOURCE:REGISTER {: s2:IR-ID:ir-source-id :}
   \ a second registry in the same context, same bytes at another ordinal
   c 8 REG-NEW {: key2:IR-ID:ir-module-key b:IR-ARENA:arena :}
   c b key2 s" pad" IR-SOURCE:REGISTER drop
   c b key2 s" shared bytes" IR-SOURCE:REGISTER {: t1:IR-ID:ir-source-id :}
   a s0 IR-SOURCE:DIGEST@ a s1 IR-SOURCE:DIGEST@ CDIGEST-DIGEST:EQ
   a s0 IR-SOURCE:DIGEST@ a s2 IR-SOURCE:DIGEST@ CDIGEST-DIGEST:EQ
   a s0 IR-SOURCE:DIGEST@ b t1 IR-SOURCE:DIGEST@ CDIGEST-DIGEST:EQ
   s0 IR-ID:SOURCE-LOCAL
   s1 IR-ID:SOURCE-LOCAL ;

: DG-CASE ( -- )
   s" equal bytes digest identically; ids never deduplicate" T-LABEL
   BND [: DG-BODY ;] IR-CTX:WITH-CONTEXT
   1 T= 0 T= TTRUE TFALSE TTRUE ;

: MT-BODY ( IR-CTX:ctx -- n bool )
   {: c:IR-CTX:ctx :}
   c 4 REG-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena :}
   c a key s" " IR-SOURCE:REGISTER {: s0:IR-ID:ir-source-id :}
   c a key s" " IR-SOURCE:REGISTER {: s1:IR-ID:ir-source-id :}
   a s0 IR-SOURCE:LEN@
   a s0 IR-SOURCE:DIGEST@ a s1 IR-SOURCE:DIGEST@ CDIGEST-DIGEST:EQ ;

: MT-CASE ( -- )
   s" an empty source registers with length zero and a stable digest" T-LABEL
   BND [: MT-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE 0 T= ;

: LN-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 4 REG-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena :}
   s" abc" {: p u:n :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   c a key p -1 IR-SOURCE:REGISTER drop ;

: LN-RUN ( -- )
   BND [: LN-BODY ;] IR-CTX:WITH-CONTEXT ;

: LN-CASE ( -- )
   s" a negative byte length is rejected at registration" T-LABEL
   [: LN-RUN ;] E-IR-SRC-LEN TTHROWSQ ;

\ ---- byte spans --------------------------------------------------------------
: SP-BODY ( IR-CTX:ctx -- n n n )
   {: c:IR-CTX:ctx :}
   c 4 REG-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena :}
   c a key s" 0123456789" IR-SOURCE:REGISTER {: s0:IR-ID:ir-source-id :}
   a a s0 2 5 IR-SOURCE:SPAN IR-SOURCE:SPAN-CK
   a a s0 0 10 IR-SOURCE:SPAN IR-SOURCE:SPAN-CK
   a a s0 10 0 IR-SOURCE:SPAN IR-SOURCE:SPAN-CK
   a s0 2 5 IR-SOURCE:SPAN IR-SOURCE:SPAN-SRC IR-ID:SOURCE-LOCAL
   a s0 2 5 IR-SOURCE:SPAN IR-SOURCE:SPAN-START
   a s0 2 5 IR-SOURCE:SPAN IR-SOURCE:SPAN-LEN ;

: SP-CASE ( -- )
   s" a span validates against its source and projects its fields" T-LABEL
   BND [: SP-BODY ;] IR-CTX:WITH-CONTEXT
   5 T= 2 T= 0 T= ;

: SPX-SETUP ( IR-CTX:ctx -- IR-ARENA:arena IR-ID:ir-source-id )
   {: c:IR-CTX:ctx :}
   c 4 REG-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena :}
   a c a key s" 0123456789" IR-SOURCE:REGISTER ;

: SPX-NEG-START-BODY ( IR-CTX:ctx -- )
   SPX-SETUP -1 3 IR-SOURCE:SPAN IR-SOURCE:SPAN-LEN drop ;

: SPX-NEG-START ( -- )
   BND [: SPX-NEG-START-BODY ;] IR-CTX:WITH-CONTEXT ;

: SPX-NEG-LEN-BODY ( IR-CTX:ctx -- )
   SPX-SETUP 2 -1 IR-SOURCE:SPAN IR-SOURCE:SPAN-LEN drop ;

: SPX-NEG-LEN ( -- )
   BND [: SPX-NEG-LEN-BODY ;] IR-CTX:WITH-CONTEXT ;

: SPX-CROSS-BODY ( IR-CTX:ctx -- )
   SPX-SETUP 6 5 IR-SOURCE:SPAN IR-SOURCE:SPAN-LEN drop ;

: SPX-CROSS ( -- )
   BND [: SPX-CROSS-BODY ;] IR-CTX:WITH-CONTEXT ;

: SPX-PAST-BODY ( IR-CTX:ctx -- )
   SPX-SETUP 11 0 IR-SOURCE:SPAN IR-SOURCE:SPAN-LEN drop ;

: SPX-PAST ( -- )
   BND [: SPX-PAST-BODY ;] IR-CTX:WITH-CONTEXT ;

\ A span assembled by the open generated constructor is a value, not
\ authority: the consumer recheck rejects its forged range.
: SPX-FORGE-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c SPX-SETUP {: a:IR-ARENA:arena s0:IR-ID:ir-source-id :}
   a s0 4 44 IR--SOURCE-SPAN:MAKE IR-SOURCE:SPAN-CK ;

: SPX-FORGE ( -- )
   BND [: SPX-FORGE-BODY ;] IR-CTX:WITH-CONTEXT ;

: SPX-CASES ( -- )
   s" a negative span start is rejected named" T-LABEL
   [: SPX-NEG-START ;] E-IR-SRC-SPAN TTHROWSQ
   s" a negative span length is rejected named" T-LABEL
   [: SPX-NEG-LEN ;] E-IR-SRC-SPAN TTHROWSQ
   s" a span crossing its source's end is rejected named" T-LABEL
   [: SPX-CROSS ;] E-IR-SRC-SPAN TTHROWSQ
   s" a span starting past its source's end is rejected named" T-LABEL
   [: SPX-PAST ;] E-IR-SRC-SPAN TTHROWSQ
   s" a constructor-forged span is rejected by the consumer recheck" T-LABEL
   [: SPX-FORGE ;] E-IR-SRC-SPAN TTHROWSQ ;

\ ---- foreign owners ----------------------------------------------------------
: XO-ID-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 4 REG-NEW {: keya:IR-ID:ir-module-key a:IR-ARENA:arena :}
   c 4 REG-NEW {: keyb:IR-ID:ir-module-key b:IR-ARENA:arena :}
   c a keya s" alpha" IR-SOURCE:REGISTER {: s0:IR-ID:ir-source-id :}
   b s0 IR-SOURCE:LEN@ drop ;

: XO-ID ( -- )
   BND [: XO-ID-BODY ;] IR-CTX:WITH-CONTEXT ;

: XO-KEY-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 4 REG-NEW {: keya:IR-ID:ir-module-key a:IR-ARENA:arena :}
   c 4 REG-NEW {: keyb:IR-ID:ir-module-key b:IR-ARENA:arena :}
   c a keyb s" beta" IR-SOURCE:REGISTER drop ;

: XO-KEY ( -- )
   BND [: XO-KEY-BODY ;] IR-CTX:WITH-CONTEXT ;

: XO-ORG-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 4 REG-NEW {: keya:IR-ID:ir-module-key a:IR-ARENA:arena :}
   c 4 REG-NEW {: keyb:IR-ID:ir-module-key b:IR-ARENA:arena :}
   c b keyb s" theirs" IR-SOURCE:REGISTER {: r0:IR-ID:ir-source-id :}
   c a keya r0 s" kid" IR-SOURCE:REGISTER-FROM drop ;

: XO-ORG ( -- )
   BND [: XO-ORG-BODY ;] IR-CTX:WITH-CONTEXT ;

: XO-CTX-INNER ( IR-ID:ir-module-key IR-ARENA:arena IR-CTX:ctx -- )
   {: key:IR-ID:ir-module-key a:IR-ARENA:arena c2:IR-CTX:ctx :}
   c2 a key s" gamma" IR-SOURCE:REGISTER drop ;

: XO-CTX-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 4 REG-NEW
   BND [: XO-CTX-INNER ;] IR-CTX:WITH-CONTEXT ;

: XO-CTX ( -- )
   BND [: XO-CTX-BODY ;] IR-CTX:WITH-CONTEXT ;

\ A source-id minted inside context A rejects in context B's registry.
: XC-INNER ( IR-ID:ir-source-id IR-CTX:ctx -- )
   {: id:IR-ID:ir-source-id c2:IR-CTX:ctx :}
   c2 4 REG-NEW {: k2:IR-ID:ir-module-key b:IR-ARENA:arena :}
   b id IR-SOURCE:LEN@ drop ;

: XC-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 4 REG-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena :}
   c a key s" xc" IR-SOURCE:REGISTER
   BND [: XC-INNER ;] IR-CTX:WITH-CONTEXT ;

: XC-RUN ( -- )
   BND [: XC-BODY ;] IR-CTX:WITH-CONTEXT ;

: XO-CASES ( -- )
   s" a source-id from registry A rejects on registry B" T-LABEL
   [: XO-ID ;] E-IR-SRC-OWNER TTHROWSQ
   s" a foreign module key rejects on registration" T-LABEL
   [: XO-KEY ;] E-IR-SRC-OWNER TTHROWSQ
   s" a foreign module's source-id rejects as an origin" T-LABEL
   [: XO-ORG ;] E-IR-SRC-OWNER TTHROWSQ
   s" a foreign live context rejects as the registration allocator" T-LABEL
   [: XO-CTX ;] E-IR-ARENA-OWNER TTHROWSQ
   s" a source-id from context A rejects in context B's registry" T-LABEL
   [: XC-RUN ;] E-IR-SRC-OWNER TTHROWSQ ;

\ ---- origin chains -----------------------------------------------------------
: OG-BODY ( IR-CTX:ctx -- bool bool n n n )
   {: c:IR-CTX:ctx :}
   c 8 REG-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena :}
   c a key s" root" IR-SOURCE:REGISTER {: s0:IR-ID:ir-source-id :}
   c a key s0 s" mid" IR-SOURCE:REGISTER-FROM {: s1:IR-ID:ir-source-id :}
   c a key s1 s" leaf" IR-SOURCE:REGISTER-FROM {: s2:IR-ID:ir-source-id :}
   a s0 IR-SOURCE:ROOT?
   a s2 IR-SOURCE:ROOT?
   a s2 IR-SOURCE:DEPTH
   a s0 IR-SOURCE:DEPTH
   a key s2 IR-SOURCE:ORIGIN@ IR-ID:SOURCE-LOCAL ;

: OG-CASE ( -- )
   s" origin chains record parents and walks terminate at the root" T-LABEL
   BND [: OG-BODY ;] IR-CTX:WITH-CONTEXT
   1 T= 0 T= 2 T= TFALSE TTRUE ;

: OG-ROOT-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 4 REG-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena :}
   c a key s" root" IR-SOURCE:REGISTER {: s0:IR-ID:ir-source-id :}
   a key s0 IR-SOURCE:ORIGIN@ drop ;

: OG-ROOT ( -- )
   BND [: OG-ROOT-BODY ;] IR-CTX:WITH-CONTEXT ;

\ A self cycle: the origin names the very id the registration would mint.
\ Origins must already be registered, so the edge dies before the row exists.
: CY-SELF-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 REG-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena :}
   c a key s" a" IR-SOURCE:REGISTER drop
   key 1 IR-ID:PACK-SOURCE {: self:IR-ID:ir-source-id :}
   c a key self s" b" IR-SOURCE:REGISTER-FROM drop ;

: CY-SELF ( -- )
   BND [: CY-SELF-BODY ;] IR-CTX:WITH-CONTEXT ;

\ A three-node cycle a -> c -> b -> a can only be opened by its first member
\ naming the planned third (ordinal 2 here, with only ordinal 0 next to
\ mint): every cycle needs one forward edge, and that edge is the one
\ rejected. Ordinals along a chain strictly decrease, so no later
\ registration can close a cycle either.
: CY-THREE-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 REG-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena :}
   key 2 IR-ID:PACK-SOURCE {: fwd:IR-ID:ir-source-id :}
   c a key fwd s" a" IR-SOURCE:REGISTER-FROM drop ;

: CY-THREE ( -- )
   BND [: CY-THREE-BODY ;] IR-CTX:WITH-CONTEXT ;

: OG-REJECT-CASES ( -- )
   s" reading a root's origin rejects named" T-LABEL
   [: OG-ROOT ;] E-IR-SRC-ROOT TTHROWSQ
   s" a self-cycle origin rejects named" T-LABEL
   [: CY-SELF ;] E-IR-SRC-ORIGIN TTHROWSQ
   s" a three-node cycle's opening edge rejects named" T-LABEL
   [: CY-THREE ;] E-IR-SRC-ORIGIN TTHROWSQ ;

\ ---- unregistered ids --------------------------------------------------------
: BD-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 4 REG-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena :}
   c a key s" only" IR-SOURCE:REGISTER drop
   key 3 IR-ID:PACK-SOURCE {: ghost:IR-ID:ir-source-id :}
   a ghost IR-SOURCE:LEN@ drop ;

: BD-RUN ( -- )
   BND [: BD-BODY ;] IR-CTX:WITH-CONTEXT ;

: BD-CASE ( -- )
   s" an own-module id past the registered count rejects named" T-LABEL
   [: BD-RUN ;] E-IR-SRC-BOUND TTHROWSQ ;

\ ---- capacity ----------------------------------------------------------------
: CAP-ZERO-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 0 REG-NEW IR-ARENA:ABORT drop ;

: CAP-ZERO ( -- )
   BND [: CAP-ZERO-BODY ;] IR-CTX:WITH-CONTEXT ;

: CAP-NEG-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c -3 REG-NEW IR-ARENA:ABORT drop ;

: CAP-NEG ( -- )
   BND [: CAP-NEG-BODY ;] IR-CTX:WITH-CONTEXT ;

: CAP-HUGE-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c $30000000 REG-NEW IR-ARENA:ABORT drop ;

: CAP-HUGE ( -- )
   BND [: CAP-HUGE-BODY ;] IR-CTX:WITH-CONTEXT ;

\ The caught quotation re-pushes its inputs before the throwing call: a value
\ survives the throw only while it physically stays on the data stack, so the
\ triple rides beneath the registration that overflows.
: CAP-THIRD ( IR-CTX:ctx IR-ARENA:arena IR-ID:ir-module-key -- IR-CTX:ctx IR-ARENA:arena IR-ID:ir-module-key )
   {: c:IR-CTX:ctx a:IR-ARENA:arena key:IR-ID:ir-module-key :}
   c a key
   c a key s" three" IR-SOURCE:REGISTER drop ;

\ The one-past-capacity reject fires before any cell is written, so the
\ registry stays whole: count unchanged, rows readable.
: CAP-FULL-BODY ( IR-CTX:ctx -- n n n )
   {: c:IR-CTX:ctx :}
   c 2 REG-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena :}
   c a key s" one" IR-SOURCE:REGISTER drop
   c a key s" two" IR-SOURCE:REGISTER {: s1:IR-ID:ir-source-id :}
   c a key [: CAP-THIRD ;] catch
   {: c2:IR-CTX:ctx a2:IR-ARENA:arena key2:IR-ID:ir-module-key rc:n :}
   rc
   a2 IR-SOURCE:SOURCES
   a2 s1 IR-SOURCE:LEN@ ;

: CAP-CASES ( -- )
   s" a zero registry capacity is rejected at creation" T-LABEL
   [: CAP-ZERO ;] E-IR-SRC-CAP TTHROWSQ
   s" a negative registry capacity is rejected at creation" T-LABEL
   [: CAP-NEG ;] E-IR-SRC-CAP TTHROWSQ
   s" a capacity past the arena ordinal range is rejected at creation" T-LABEL
   [: CAP-HUGE ;] E-IR-SRC-CAP TTHROWSQ
   s" a full registry rejects named and stays readable" T-LABEL
   BND [: CAP-FULL-BODY ;] IR-CTX:WITH-CONTEXT
   3 T= 2 T= E-IR-SRC-CAP T= ;

\ ---- frozen modules own their source rows ------------------------------------
: FZ-BODY ( IR-CTX:ctx -- bool n n bool n n )
   {: c:IR-CTX:ctx :}
   c 4 REG-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena :}
   c a key s" root" IR-SOURCE:REGISTER {: s0:IR-ID:ir-source-id :}
   c a key s0 s" kid" IR-SOURCE:REGISTER-FROM {: s1:IR-ID:ir-source-id :}
   a s0 IR-SOURCE:DIGEST@              \ the live digest rides across the freeze
   a IR-ARENA:FREEZE {: v:IR-ARENA:view :}
   v s0 IR-SOURCE:FDIGEST@ CDIGEST-DIGEST:EQ
   v IR-SOURCE:FSOURCES
   v s0 IR-SOURCE:FLEN@
   v s0 IR-SOURCE:FROOT?
   v s1 IR-SOURCE:FDEPTH
   v key s1 IR-SOURCE:FORIGIN@ IR-ID:SOURCE-LOCAL ;

: FZ-CASE ( -- )
   s" a frozen module serves every source row through the view" T-LABEL
   BND [: FZ-BODY ;] IR-CTX:WITH-CONTEXT
   0 T= 1 T= TTRUE 4 T= 2 T= TTRUE ;

: FZ-SPAN-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 4 REG-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena :}
   c a key s" 0123456789" IR-SOURCE:REGISTER {: s0:IR-ID:ir-source-id :}
   a s0 2 5 IR-SOURCE:SPAN
   a IR-ARENA:FREEZE {: v:IR-ARENA:view :}
   v swap IR-SOURCE:FSPAN-CK ;

: FZ-SPAN-CASE ( -- )
   s" a span minted live revalidates against the frozen view" T-LABEL
   BND [: FZ-SPAN-BODY ;] IR-CTX:WITH-CONTEXT ;

: FZ-PUSH-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 4 REG-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena :}
   c a key s" one" IR-SOURCE:REGISTER drop
   a IR-ARENA:FREEZE drop
   c a key s" two" IR-SOURCE:REGISTER drop ;

: FZ-PUSH ( -- )
   BND [: FZ-PUSH-BODY ;] IR-CTX:WITH-CONTEXT ;

: FZ-READ-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 4 REG-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena :}
   c a key s" one" IR-SOURCE:REGISTER {: s0:IR-ID:ir-source-id :}
   a IR-ARENA:FREEZE drop
   a s0 IR-SOURCE:LEN@ drop ;

: FZ-READ ( -- )
   BND [: FZ-READ-BODY ;] IR-CTX:WITH-CONTEXT ;

: FZ-REJECT-CASES ( -- )
   s" registering through a frozen builder handle rejects" T-LABEL
   [: FZ-PUSH ;] E-IR-ARENA-FROZEN TTHROWSQ
   s" live readers reject the retired builder handle; the view reads" T-LABEL
   [: FZ-READ ;] E-IR-ARENA-FROZEN TTHROWSQ ;

\ ---- non-registry and misaligned arenas --------------------------------------
: RAW-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 IR-ARENA:NEW IR-SOURCE:SOURCES drop ;

: RAW-RUN ( -- )
   BND [: RAW-BODY ;] IR-CTX:WITH-CONTEXT ;

: MAGIC-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 9 IR-ARENA:NEW {: a:IR-ARENA:arena :}
   c a 99 IR-ARENA:PUSH drop
   c a 1 IR-ARENA:PUSH drop
   c a 1 IR-ARENA:PUSH drop
   a IR-SOURCE:SOURCES drop ;

: MAGIC-RUN ( -- )
   BND [: MAGIC-BODY ;] IR-CTX:WITH-CONTEXT ;

\ A holder who bypasses the registry and appends raw cells misaligns the
\ row shape; the next operation rechecks it fail-closed.
: SHAPE-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 4 REG-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena :}
   c a key s" ok" IR-SOURCE:REGISTER drop
   c a 7 IR-ARENA:PUSH drop
   a IR-SOURCE:SOURCES drop ;

: SHAPE-RUN ( -- )
   BND [: SHAPE-BODY ;] IR-CTX:WITH-CONTEXT ;

: STATE-CASES ( -- )
   s" a bare arena is not a source registry" T-LABEL
   [: RAW-RUN ;] E-IR-SRC-STATE TTHROWSQ
   s" a wrong header tag rejects fail-closed" T-LABEL
   [: MAGIC-RUN ;] E-IR-SRC-STATE TTHROWSQ
   s" a misaligned row shape rejects fail-closed" T-LABEL
   [: SHAPE-RUN ;] E-IR-SRC-STATE TTHROWSQ ;

\ ---- a registration is one commit, at the scratch edge -------------------------
\ The registry's six-cell row is six appends, and an append grows the arena by
\ taking a span from the context mapping. Registering at the edge of that
\ mapping therefore used to stop a row half written - five of six cells - and
\ (5 mod 6) is not zero, so every later read of the registry failed its shape
\ check and the module's sources became unreadable. The reservation in ROW-ADD
\ is what makes the six appends one commit.
\
\ The registry's own documented row shape, pinned so this case tests the
\ structure and not a message: a three-cell header, then six cells per source.
3 constant T-HDR-CELLS
6 constant T-ROW-CELLS

\ Depth-neutral: the triple rides beneath the registration that fails, so it
\ survives the throw.
: TORN-ADD ( IR-CTX:ctx IR-ARENA:arena IR-ID:ir-module-key -- IR-CTX:ctx IR-ARENA:arena IR-ID:ir-module-key )
   {: c:IR-CTX:ctx a:IR-ARENA:arena key:IR-ID:ir-module-key :}
   c a key
   c a key s" filler" IR-SOURCE:REGISTER drop ;

\ Register until one registration refuses, and answer its code. The registry
\ has room for sixty-four sources, so the refusal is the arena reaching for a
\ span the starved mapping cannot give it, never a full registry.
: TORN-FILL ( IR-CTX:ctx IR-ARENA:arena IR-ID:ir-module-key -- IR-CTX:ctx IR-ARENA:arena IR-ID:ir-module-key n )
   0
   begin
      drop
      [: TORN-ADD ;] catch
      dup 0<>
   until ;

\ Everything the refusal must have left alone: the registry still reads, it
\ holds exactly the rows that landed, and the first row still has its bytes.
: TORN-INTACT ( IR-ARENA:arena IR-ID:ir-source-id -- IR-ARENA:arena IR-ID:ir-source-id )
   {: a:IR-ARENA:arena s0:IR-ID:ir-source-id :}
   a s0
   a IR-SOURCE:SOURCES 2 <> if E-IR-SRC-STATE throw then
   a s0 IR-SOURCE:LEN@ 16 <> if E-IR-SRC-STATE throw then
   a s0 IR-SOURCE:ROOT? 0= if E-IR-SRC-STATE throw then ;

: TORN-BODY ( IR-CTX:ctx -- n n n )
   {: c:IR-CTX:ctx :}
   c 64 REG-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena :}
   c a key s" the first source" IR-SOURCE:REGISTER {: s0:IR-ID:ir-source-id :}
   c IR-STARVE:EDGE
   c a key TORN-FILL
   {: c2:IR-CTX:ctx a2:IR-ARENA:arena key2:IR-ID:ir-module-key rc:n :}
   rc
   a2 s0 [: TORN-INTACT ;] catch nip nip
   a2 IR-ARENA:USED T-HDR-CELLS - T-ROW-CELLS mod ;

: TORN-CASE ( -- )
   s" a registration refused mid-row leaves whole rows and a readable registry" T-LABEL
   BND [: TORN-BODY ;] IR-CTX:WITH-CONTEXT
   0 T= 0 T= E-IR-CTX-SCRATCH T= ;

\ ---- teardown releases everything --------------------------------------------
: TD-ESC-BODY ( IR-CTX:ctx -- IR-ARENA:arena IR-ID:ir-source-id )
   {: c:IR-CTX:ctx :}
   c 4 REG-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena :}
   a c a key s" td" IR-SOURCE:REGISTER ;

: TD-READ ( -- )
   BND [: TD-ESC-BODY ;] IR-CTX:WITH-CONTEXT
   IR-SOURCE:LEN@ drop ;

: TD-STALE-CASE ( -- )
   s" a registry is dead after its context ends" T-LABEL
   [: TD-READ ;] E-IR-ARENA-STALE TTHROWSQ ;

: TD-FRESH-CASE ( -- )
   s" fresh contexts and registries succeed after teardown" T-LABEL
   4 0 ?do
      BND [: RG-BODY ;] IR-CTX:WITH-CONTEXT
      9 T= 2 T= TTRUE TTRUE 1 T= 0 T=
   loop ;

\ ---- the checker keeps identities and the API sealed -------------------------
: CHECKER-CASES ( -- )
   \ positive control: a well-typed candidate over the same surface certifies,
   \ so the rejections below fail for their stated reason, not a harness typo
   s" IRSRC-POS ( IR-ARENA:arena -- n ) IR-SOURCE:SOURCES"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" IRSRC-ID-FORGE ( n -- IR-ID:ir-source-id )"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" IRSRC-KEY-FORGE ( n -- IR-ID:ir-module-key )"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" IRSRC-SPAN-FORGE ( n n n -- IR-SOURCE:span ) IR--SOURCE-SPAN:MAKE"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" IRSRC-SPAN-CELL ( IR-ARENA:cell-id n n -- IR-SOURCE:span ) IR--SOURCE-SPAN:MAKE"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" IRSRC-CTXLESS ( IR-ARENA:arena IR-ID:ir-module-key ptr u8 n -- IR-ID:ir-source-id ) IR-SOURCE:REGISTER"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" IRSRC-KEYLESS ( IR-CTX:ctx IR-ARENA:arena ptr u8 n -- IR-ID:ir-source-id ) IR-SOURCE:REGISTER"
      CHECK-QUIET-CANDIDATE! 0 T= ;

\ ---- run ---------------------------------------------------------------------
\ Throw-through fixtures run inside one outermost harness context, so a
\ context aborted by a throw is reclaimed by the harness exit instead of
\ lingering for the rest of the process. The teardown-reuse loop runs after
\ that exit, on a registry the next sweep can fully reclaim.
: HARNESS-BODY ( IR-CTX:ctx -- )
   drop
   RG-CASE
   DG-CASE
   MT-CASE
   LN-CASE
   SP-CASE
   SPX-CASES
   XO-CASES
   OG-CASE
   OG-REJECT-CASES
   BD-CASE
   CAP-CASES
   FZ-CASE
   FZ-SPAN-CASE
   FZ-REJECT-CASES
   STATE-CASES
   TD-STALE-CASE ;

public

: RUN ( -- )
   T-RESET
   BND [: HARNESS-BODY ;] IR-CTX:WITH-CONTEXT
   TORN-CASE
   TD-FRESH-CASE
   CHECKER-CASES
   T-REPORT ;

;package

IR-SOURCE-TEST:RUN
