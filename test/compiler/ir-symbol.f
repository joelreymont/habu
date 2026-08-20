\ ir-symbol.f - checked compiler symbol interner tests.
\
\ Proves the IR-0.3 symbol-interning contract of src/compiler/ir/symbol.f:
\ duplicate bytes answer one identity - the same bytes twice and the same
\ bytes among many distinct entries - while distinct bytes mint distinct
\ module-local identities; the content filter is never identity, so a forced
\ same-filter same-length pair of different strings stays distinct through
\ the byte-compare verify step; byte packing round-trips across cell
\ boundaries through EQ? and COPY; capacity for rows and for pool bytes
\ rejects named at the committed ceilings while the stores stay readable and
\ duplicate interns still answer; foreign keys, foreign symbol-ids, foreign
\ contexts, cross-module store pairings, and cross-context identities
\ reject; non-interner, misaligned, and span-forged arenas reject
\ fail-closed; a frozen module serves every reader through the arena views
\ while the retired builder handles reject; context teardown releases
\ everything; and checker fixtures prove the identity families and the API
\ stay sealed.

require lib/test.f
require lib/string.f
require test/checker-assert.f
require test/compiler/ir-starve.f
require src/compiler/ir/symbol.f

package IR-SYM-TEST
private

create CBUF 32 allot

\ ---- fixtures ----------------------------------------------------------------
: BND ( -- CBIND:binding )
   CTARGET-ARCH:AARCH64 CTARGET-ABI:AAPCS64-DARWIN CTARGET-ENDIAN:LITTLE
   CTARGET-PTR--WIDTH:BITS64
   CTARGET:F-BASE CTARGET:F-FP CTARGET:WITH CTARGET:CONTRACT
   CNUM-OVERFLOW:TRAP CNUM-FLOAT--MODEL:IEEE754 CNUM-CONTRACTION:FORBIDDEN
   CNUM-FAST--MATH:BIT-EXACT CNUM-COMPARE:IEEE754-UNORDERED CNUM:POLICY
   CBIND:BIND ;

\ A fresh (key, pool, rows) interner for scap symbols and bcap bytes.
: TAB-NEW ( IR-CTX:ctx n n -- IR-ID:ir-module-key IR-ARENA:arena IR-ARENA:arena )
   {: c:IR-CTX:ctx scap:n bcap:n :}
   c IR-CTX:NEW-MODULE drop {: key:IR-ID:ir-module-key :}
   key c key scap bcap IR-SYM:NEW ;

\ ---- duplicate bytes answer one identity -------------------------------------
: DUP-BODY ( IR-CTX:ctx -- n n n n n bool )
   {: c:IR-CTX:ctx :}
   c IR-CTX:NEW-MODULE {: key:IR-ID:ir-module-key mid:IR-ID:ir-module-id :}
   c key 8 64 IR-SYM:NEW {: a:IR-ARENA:arena r:IR-ARENA:arena :}
   c a r key s" main" IR-SYM:INTERN {: s0:IR-ID:ir-symbol-id :}
   c a r key s" helper" IR-SYM:INTERN {: s1:IR-ID:ir-symbol-id :}
   c a r key s" main" IR-SYM:INTERN {: s2:IR-ID:ir-symbol-id :}
   c a r key s" other" IR-SYM:INTERN drop
   c a r key s" helper" IR-SYM:INTERN {: s3:IR-ID:ir-symbol-id :}
   s0 IR-ID:SYMBOL-LOCAL
   s1 IR-ID:SYMBOL-LOCAL
   s2 IR-ID:SYMBOL-LOCAL
   s3 IR-ID:SYMBOL-LOCAL
   r IR-SYM:SYMBOLS
   s0 IR-ID:SYMBOL-OWNER mid IR-ID:MODULE-SAME? ;

: DUP-CASE ( -- )
   s" duplicate bytes answer one id; distinct bytes mint distinct ids" T-LABEL
   BND [: DUP-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE 3 T= 1 T= 0 T= 1 T= 0 T= ;

\ ---- byte fidelity across cell boundaries ------------------------------------
: BY-BODY ( IR-CTX:ctx -- bool n bool bool bool bool bool bool bool n n )
   {: c:IR-CTX:ctx :}
   c 8 64 TAB-NEW
   {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   c a r key s" 0123456789ABCDEFG" IR-SYM:INTERN {: s17:IR-ID:ir-symbol-id :}
   c a r key s" 01234567" IR-SYM:INTERN {: s8:IR-ID:ir-symbol-id :}
   c a r key s" x" IR-SYM:INTERN {: s1:IR-ID:ir-symbol-id :}
   c a r key s" " IR-SYM:INTERN {: s0:IR-ID:ir-symbol-id :}
   c a r key s" " IR-SYM:INTERN IR-ID:SYMBOL-LOCAL s0 IR-ID:SYMBOL-LOCAL =
   r s17 IR-SYM:LEN@
   a r s17 s" 0123456789ABCDEFG" IR-SYM:EQ?
   a r s17 s" 0123456789ABCDEFH" IR-SYM:EQ?
   a r s8 s" 01234567" IR-SYM:EQ?
   a r s8 s" 0123456" IR-SYM:EQ?
   a r s1 s" x" IR-SYM:EQ?
   a r s1 s" y" IR-SYM:EQ?
   a r s17 CBUF 32 IR-SYM:COPY {: n17:n :}
   CBUF n17 s" 0123456789ABCDEFG" STR=
   r s0 IR-SYM:LEN@
   c a r key s" 012345678" IR-SYM:INTERN IR-ID:SYMBOL-LOCAL ;

: BY-CASE ( -- )
   s" symbol bytes round-trip across cell boundaries" T-LABEL
   BND [: BY-BODY ;] IR-CTX:WITH-CONTEXT
   4 T= 0 T= TTRUE TFALSE TTRUE TFALSE TTRUE TFALSE TTRUE 17 T= TTRUE ;

\ ---- a forced filter collision stays distinct --------------------------------
\ "VGA" and "HRA" share the sixteen-bit content filter (and the byte length),
\ so unifying them is exactly what a filter-as-identity bug would do; only
\ the byte-compare verify step keeps them apart.
: CL-BODY ( IR-CTX:ctx -- bool n n bool bool n )
   {: c:IR-CTX:ctx :}
   c 8 64 TAB-NEW
   {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   s" VGA" IR-SYM:FILTER s" HRA" IR-SYM:FILTER =
   c a r key s" VGA" IR-SYM:INTERN {: sv:IR-ID:ir-symbol-id :}
   c a r key s" HRA" IR-SYM:INTERN {: sh:IR-ID:ir-symbol-id :}
   sv IR-ID:SYMBOL-LOCAL
   sh IR-ID:SYMBOL-LOCAL
   a r sv s" VGA" IR-SYM:EQ?
   a r sh s" HRA" IR-SYM:EQ?
   c a r key s" VGA" IR-SYM:INTERN IR-ID:SYMBOL-LOCAL ;

: CL-CASE ( -- )
   s" a forced filter collision is split by the byte-compare verify" T-LABEL
   BND [: CL-BODY ;] IR-CTX:WITH-CONTEXT
   0 T= TTRUE TTRUE 1 T= 0 T= TTRUE ;

\ ---- a copy span smaller than the symbol -------------------------------------
: CP-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 4 64 TAB-NEW
   {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   c a r key s" 12345678" IR-SYM:INTERN {: s0:IR-ID:ir-symbol-id :}
   a r s0 CBUF 4 IR-SYM:COPY drop ;

: CP-RUN ( -- )
   BND [: CP-BODY ;] IR-CTX:WITH-CONTEXT ;

: CP-CASE ( -- )
   s" a destination span smaller than the symbol rejects named" T-LABEL
   [: CP-RUN ;] E-IR-SYM-RANGE TTHROWSQ ;

\ ---- negative byte length ----------------------------------------------------
: LN-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 4 64 TAB-NEW
   {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   s" abc" {: p u:n :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   c a r key p -1 IR-SYM:INTERN drop ;

: LN-RUN ( -- )
   BND [: LN-BODY ;] IR-CTX:WITH-CONTEXT ;

: LN-CASE ( -- )
   s" a negative byte length is rejected at intern" T-LABEL
   [: LN-RUN ;] E-IR-SYM-LEN TTHROWSQ ;

\ ---- foreign owners ----------------------------------------------------------
: XO-ID-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 4 64 TAB-NEW
   {: keya:IR-ID:ir-module-key a:IR-ARENA:arena ra:IR-ARENA:arena :}
   c 4 64 TAB-NEW
   {: keyb:IR-ID:ir-module-key b:IR-ARENA:arena rb:IR-ARENA:arena :}
   c a ra keya s" alpha" IR-SYM:INTERN {: s0:IR-ID:ir-symbol-id :}
   rb s0 IR-SYM:LEN@ drop ;

: XO-ID ( -- )
   BND [: XO-ID-BODY ;] IR-CTX:WITH-CONTEXT ;

: XO-KEY-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 4 64 TAB-NEW
   {: keya:IR-ID:ir-module-key a:IR-ARENA:arena ra:IR-ARENA:arena :}
   c 4 64 TAB-NEW
   {: keyb:IR-ID:ir-module-key b:IR-ARENA:arena rb:IR-ARENA:arena :}
   c a ra keyb s" beta" IR-SYM:INTERN drop ;

: XO-KEY ( -- )
   BND [: XO-KEY-BODY ;] IR-CTX:WITH-CONTEXT ;

\ Module A's pool paired with module B's rows: the pair coupling rejects
\ before any row span is trusted against the wrong pool.
: XO-PAIR-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 4 64 TAB-NEW
   {: keya:IR-ID:ir-module-key a:IR-ARENA:arena ra:IR-ARENA:arena :}
   c 4 64 TAB-NEW
   {: keyb:IR-ID:ir-module-key b:IR-ARENA:arena rb:IR-ARENA:arena :}
   c b rb keyb s" theirs" IR-SYM:INTERN {: sb:IR-ID:ir-symbol-id :}
   a rb sb s" theirs" IR-SYM:EQ? drop ;

: XO-PAIR ( -- )
   BND [: XO-PAIR-BODY ;] IR-CTX:WITH-CONTEXT ;

: XO-CTX-INNER ( IR-ID:ir-module-key IR-ARENA:arena IR-ARENA:arena IR-CTX:ctx -- )
   {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena c2:IR-CTX:ctx :}
   c2 a r key s" fresh" IR-SYM:INTERN drop ;

: XO-CTX-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 4 64 TAB-NEW
   BND [: XO-CTX-INNER ;] IR-CTX:WITH-CONTEXT ;

: XO-CTX ( -- )
   BND [: XO-CTX-BODY ;] IR-CTX:WITH-CONTEXT ;

\ A duplicate intern allocates nothing, so the context argument is exercised
\ only on the miss path: a foreign context answering an existing symbol is
\ the designed hit behavior, exactly like the ctx-free readers.
: XO-HIT-INNER ( IR-ID:ir-module-key IR-ARENA:arena IR-ARENA:arena IR-CTX:ctx -- n )
   {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena c2:IR-CTX:ctx :}
   c2 a r key s" shared" IR-SYM:INTERN IR-ID:SYMBOL-LOCAL ;

: XO-HIT-BODY ( IR-CTX:ctx -- n )
   {: c:IR-CTX:ctx :}
   c 4 64 TAB-NEW
   {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   c a r key s" shared" IR-SYM:INTERN drop
   key a r
   BND [: XO-HIT-INNER ;] IR-CTX:WITH-CONTEXT ;

: XO-HIT-CASE ( -- )
   s" a duplicate intern is a pure read: no allocator consulted" T-LABEL
   BND [: XO-HIT-BODY ;] IR-CTX:WITH-CONTEXT
   0 T= ;

\ A symbol-id minted inside context A rejects in context B's interner.
: XC-INNER ( IR-ID:ir-symbol-id IR-CTX:ctx -- )
   {: id:IR-ID:ir-symbol-id c2:IR-CTX:ctx :}
   c2 4 64 TAB-NEW
   {: k2:IR-ID:ir-module-key b:IR-ARENA:arena rb:IR-ARENA:arena :}
   rb id IR-SYM:LEN@ drop ;

: XC-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 4 64 TAB-NEW
   {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   c a r key s" xc" IR-SYM:INTERN
   BND [: XC-INNER ;] IR-CTX:WITH-CONTEXT ;

: XC-RUN ( -- )
   BND [: XC-BODY ;] IR-CTX:WITH-CONTEXT ;

: XO-CASES ( -- )
   s" a symbol-id from interner A rejects on interner B" T-LABEL
   [: XO-ID ;] E-IR-SYM-OWNER TTHROWSQ
   s" a foreign module key rejects at intern" T-LABEL
   [: XO-KEY ;] E-IR-SYM-OWNER TTHROWSQ
   s" a cross-module store pairing rejects" T-LABEL
   [: XO-PAIR ;] E-IR-SYM-OWNER TTHROWSQ
   s" a foreign live context rejects as the miss-path allocator" T-LABEL
   [: XO-CTX ;] E-IR-ARENA-OWNER TTHROWSQ
   XO-HIT-CASE
   s" a symbol-id from context A rejects in context B's interner" T-LABEL
   [: XC-RUN ;] E-IR-SYM-OWNER TTHROWSQ ;

\ ---- unregistered ids --------------------------------------------------------
: BD-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 4 64 TAB-NEW
   {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   c a r key s" only" IR-SYM:INTERN drop
   key 3 IR-ID:PACK-SYMBOL {: ghost:IR-ID:ir-symbol-id :}
   r ghost IR-SYM:LEN@ drop ;

: BD-RUN ( -- )
   BND [: BD-BODY ;] IR-CTX:WITH-CONTEXT ;

: BD-CASE ( -- )
   s" an own-module id past the interned count rejects named" T-LABEL
   [: BD-RUN ;] E-IR-SYM-BOUND TTHROWSQ ;

\ ---- capacity ----------------------------------------------------------------
: CAP-ZERO-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 0 64 TAB-NEW IR-ARENA:ABORT IR-ARENA:ABORT drop ;

: CAP-ZERO ( -- )
   BND [: CAP-ZERO-BODY ;] IR-CTX:WITH-CONTEXT ;

: CAP-NEG-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c -3 64 TAB-NEW IR-ARENA:ABORT IR-ARENA:ABORT drop ;

: CAP-NEG ( -- )
   BND [: CAP-NEG-BODY ;] IR-CTX:WITH-CONTEXT ;

: CAP-HUGE-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c $60000000 64 TAB-NEW IR-ARENA:ABORT IR-ARENA:ABORT drop ;

: CAP-HUGE ( -- )
   BND [: CAP-HUGE-BODY ;] IR-CTX:WITH-CONTEXT ;

: BCAP-ZERO-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 4 0 TAB-NEW IR-ARENA:ABORT IR-ARENA:ABORT drop ;

: BCAP-ZERO ( -- )
   BND [: BCAP-ZERO-BODY ;] IR-CTX:WITH-CONTEXT ;

: BCAP-HUGE-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 4 $800000000 TAB-NEW IR-ARENA:ABORT IR-ARENA:ABORT drop ;

: BCAP-HUGE ( -- )
   BND [: BCAP-HUGE-BODY ;] IR-CTX:WITH-CONTEXT ;

\ The caught quotation re-pushes its inputs before the throwing call: the
\ quadruple rides beneath the intern that overflows, so the stores stay
\ readable after the named reject.
: CAPF-THIRD ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key -- IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key )
   {: c:IR-CTX:ctx a:IR-ARENA:arena r:IR-ARENA:arena key:IR-ID:ir-module-key :}
   c a r key
   c a r key s" three" IR-SYM:INTERN drop ;

: CAPF-BODY ( IR-CTX:ctx -- n n n )
   {: c:IR-CTX:ctx :}
   c 2 64 TAB-NEW
   {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   c a r key s" one" IR-SYM:INTERN drop
   c a r key s" two" IR-SYM:INTERN drop
   c a r key [: CAPF-THIRD ;] catch
   {: c2:IR-CTX:ctx a2:IR-ARENA:arena r2:IR-ARENA:arena key2:IR-ID:ir-module-key rc:n :}
   rc
   r2 IR-SYM:SYMBOLS
   c2 a2 r2 key2 s" one" IR-SYM:INTERN IR-ID:SYMBOL-LOCAL ;

: POOLF-NINTH ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key -- IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key )
   {: c:IR-CTX:ctx a:IR-ARENA:arena r:IR-ARENA:arena key:IR-ID:ir-module-key :}
   c a r key
   c a r key s" 9" IR-SYM:INTERN drop ;

: POOLF-BODY ( IR-CTX:ctx -- n n n )
   {: c:IR-CTX:ctx :}
   c 8 8 TAB-NEW
   {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   c a r key s" 12345678" IR-SYM:INTERN drop
   c a r key [: POOLF-NINTH ;] catch
   {: c2:IR-CTX:ctx a2:IR-ARENA:arena r2:IR-ARENA:arena key2:IR-ID:ir-module-key rc:n :}
   rc
   r2 IR-SYM:SYMBOLS
   c2 a2 r2 key2 s" 12345678" IR-SYM:INTERN IR-ID:SYMBOL-LOCAL ;

: CAP-CASES ( -- )
   s" a zero symbol capacity is rejected at creation" T-LABEL
   [: CAP-ZERO ;] E-IR-SYM-CAP TTHROWSQ
   s" a negative symbol capacity is rejected at creation" T-LABEL
   [: CAP-NEG ;] E-IR-SYM-CAP TTHROWSQ
   s" a symbol capacity past the row ordinal range is rejected" T-LABEL
   [: CAP-HUGE ;] E-IR-SYM-CAP TTHROWSQ
   s" a zero byte capacity is rejected at creation" T-LABEL
   [: BCAP-ZERO ;] E-IR-SYM-BYTES TTHROWSQ
   s" a byte capacity past the pool cell range is rejected" T-LABEL
   [: BCAP-HUGE ;] E-IR-SYM-BYTES TTHROWSQ
   s" a full row table rejects named; duplicates still answer" T-LABEL
   BND [: CAPF-BODY ;] IR-CTX:WITH-CONTEXT
   0 T= 2 T= E-IR-SYM-CAP T=
   s" a full byte pool rejects named; duplicates still answer" T-LABEL
   BND [: POOLF-BODY ;] IR-CTX:WITH-CONTEXT
   0 T= 1 T= E-IR-SYM-BYTES T= ;

\ ---- non-interner, misaligned, and span-forged arenas ------------------------
: RAW-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 IR-ARENA:NEW IR-SYM:SYMBOLS drop ;

: RAW-RUN ( -- )
   BND [: RAW-BODY ;] IR-CTX:WITH-CONTEXT ;

\ The two stores are not interchangeable: the pool presented as the row
\ table (and the reverse) is a format-tag reject, not a misread.
: SWAP-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 4 64 TAB-NEW
   {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   c a r key s" swp" IR-SYM:INTERN {: s0:IR-ID:ir-symbol-id :}
   r a s0 s" swp" IR-SYM:EQ? drop ;

: SWAP-RUN ( -- )
   BND [: SWAP-BODY ;] IR-CTX:WITH-CONTEXT ;

: SHAPE-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 4 64 TAB-NEW
   {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   c a r key s" ok" IR-SYM:INTERN drop
   c r 7 IR-ARENA:PUSH drop
   r IR-SYM:SYMBOLS drop ;

: SHAPE-RUN ( -- )
   BND [: SHAPE-BODY ;] IR-CTX:WITH-CONTEXT ;

\ A holder who bypasses the interner and appends an aligned row whose span
\ points past the pool's live cells: the per-access span recheck rejects
\ fail-closed before any pool cell is read.
: FORGE-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 4 64 TAB-NEW
   {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   c a r key s" ok" IR-SYM:INTERN drop
   c r 0 IR-ARENA:PUSH drop
   c r 999 IR-ARENA:PUSH drop
   c r 8 IR-ARENA:PUSH drop
   key 1 IR-ID:PACK-SYMBOL {: forged:IR-ID:ir-symbol-id :}
   a r forged s" 88888888" IR-SYM:EQ? drop ;

: FORGE-RUN ( -- )
   BND [: FORGE-BODY ;] IR-CTX:WITH-CONTEXT ;

: STATE-CASES ( -- )
   s" a bare arena is not a symbol store" T-LABEL
   [: RAW-RUN ;] E-IR-SYM-STATE TTHROWSQ
   s" a swapped store pairing rejects on the format tag" T-LABEL
   [: SWAP-RUN ;] E-IR-SYM-STATE TTHROWSQ
   s" a misaligned row shape rejects fail-closed" T-LABEL
   [: SHAPE-RUN ;] E-IR-SYM-STATE TTHROWSQ
   s" a bypass-forged row span rejects fail-closed" T-LABEL
   [: FORGE-RUN ;] E-IR-SYM-STATE TTHROWSQ ;

\ ---- frozen modules own their symbols ----------------------------------------
: FZ-BODY ( IR-CTX:ctx -- n n bool bool bool )
   {: c:IR-CTX:ctx :}
   c 8 64 TAB-NEW
   {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   c a r key s" root" IR-SYM:INTERN {: s0:IR-ID:ir-symbol-id :}
   c a r key s" kid" IR-SYM:INTERN {: s1:IR-ID:ir-symbol-id :}
   a IR-ARENA:FREEZE {: pv:IR-ARENA:view :}
   r IR-ARENA:FREEZE {: rv:IR-ARENA:view :}
   rv IR-SYM:FSYMBOLS
   rv s0 IR-SYM:FLEN@
   pv rv s0 s" root" IR-SYM:FEQ?
   pv rv s1 s" root" IR-SYM:FEQ?
   pv rv s1 CBUF 32 IR-SYM:FCOPY {: nk:n :}
   CBUF nk s" kid" STR= ;

: FZ-CASE ( -- )
   s" a frozen module serves every symbol through the views" T-LABEL
   BND [: FZ-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TFALSE TTRUE 4 T= 2 T= ;

: FZ-PUSH-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 4 64 TAB-NEW
   {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   c a r key s" one" IR-SYM:INTERN drop
   a IR-ARENA:FREEZE drop
   r IR-ARENA:FREEZE drop
   c a r key s" two" IR-SYM:INTERN drop ;

: FZ-PUSH ( -- )
   BND [: FZ-PUSH-BODY ;] IR-CTX:WITH-CONTEXT ;

: FZ-READ-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 4 64 TAB-NEW
   {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   c a r key s" one" IR-SYM:INTERN {: s0:IR-ID:ir-symbol-id :}
   r IR-ARENA:FREEZE drop
   r s0 IR-SYM:LEN@ drop ;

: FZ-READ ( -- )
   BND [: FZ-READ-BODY ;] IR-CTX:WITH-CONTEXT ;

: FZ-REJECT-CASES ( -- )
   s" interning through retired frozen handles rejects" T-LABEL
   [: FZ-PUSH ;] E-IR-ARENA-FROZEN TTHROWSQ
   s" live readers reject the retired builder handle; the views read" T-LABEL
   [: FZ-READ ;] E-IR-ARENA-FROZEN TTHROWSQ ;

\ ---- an intern is one commit, at the scratch edge ------------------------------
\ An intern writes two arenas: the symbol's bytes into the pool, then its
\ three-cell row into the row table. An append grows an arena by taking a span
\ from the context mapping, so interning at the edge of that mapping used to
\ put the bytes in and stop part way through the row - leaving a row-cell count
\ that three does not divide, an unreadable table, and a pool holding bytes no
\ row accounts for. Both arenas are now reserved before either is touched.
\
\ The interner's own documented shapes, pinned so this case tests structure:
\ a three-cell header on each store, three cells per row.
3 constant T-HDR-CELLS
3 constant T-ROW-CELLS

create TORN-BUF 32 allot
variable TORN-N

\ Each attempt interns a different prefix, so every attempt is a miss that must
\ append; interning the same bytes twice would answer from the table and
\ allocate nothing, and the loop would never end.
: TORN-ADD ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key -- IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key )
   {: c:IR-CTX:ctx a:IR-ARENA:arena r:IR-ARENA:arena key:IR-ID:ir-module-key :}
   c a r key
   TORN-N @ 1+ dup TORN-N ! {: n:n :}
   c a r key s" abcdefghijklmnopqrstuvwxyz" drop n IR-SYM:INTERN drop ;

: TORN-FILL ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key -- IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key n )
   0
   begin
      drop
      [: TORN-ADD ;] catch
      dup 0<>
   until ;

\ Everything the refusal must have left alone: the table reads, holds exactly
\ the one symbol that landed, still answers its bytes, and neither store has
\ advanced a cell past what that symbol needs - two pool cells for sixteen
\ bytes, one row.
: TORN-INTACT ( IR-ARENA:arena IR-ARENA:arena IR-ID:ir-symbol-id -- IR-ARENA:arena IR-ARENA:arena IR-ID:ir-symbol-id )
   {: a:IR-ARENA:arena r:IR-ARENA:arena s0:IR-ID:ir-symbol-id :}
   a r s0
   r IR-SYM:SYMBOLS 1 <> if E-IR-SYM-STATE throw then
   r s0 IR-SYM:LEN@ 16 <> if E-IR-SYM-STATE throw then
   a r s0 s" the first symbol" IR-SYM:EQ? 0= if E-IR-SYM-STATE throw then
   a IR-ARENA:USED T-HDR-CELLS 2 + <> if E-IR-SYM-STATE throw then
   r IR-ARENA:USED T-HDR-CELLS T-ROW-CELLS + <> if E-IR-SYM-STATE throw then ;

: TORN-BODY ( IR-CTX:ctx -- n n )
   {: c:IR-CTX:ctx :}
   0 TORN-N !
   c 64 512 TAB-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   c a r key s" the first symbol" IR-SYM:INTERN {: s0:IR-ID:ir-symbol-id :}
   c IR-STARVE:EDGE
   c a r key TORN-FILL
   {: c2:IR-CTX:ctx a2:IR-ARENA:arena r2:IR-ARENA:arena key2:IR-ID:ir-module-key rc:n :}
   rc
   a2 r2 s0 [: TORN-INTACT ;] catch nip nip nip ;

: TORN-CASE ( -- )
   s" an intern refused mid-row leaves both stores whole and readable" T-LABEL
   BND [: TORN-BODY ;] IR-CTX:WITH-CONTEXT
   0 T= E-IR-CTX-SCRATCH T= ;

\ ---- teardown releases everything --------------------------------------------
: TD-ESC-BODY ( IR-CTX:ctx -- IR-ARENA:arena IR-ID:ir-symbol-id )
   {: c:IR-CTX:ctx :}
   c 4 64 TAB-NEW
   {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   r c a r key s" td" IR-SYM:INTERN ;

: TD-READ ( -- )
   BND [: TD-ESC-BODY ;] IR-CTX:WITH-CONTEXT
   IR-SYM:LEN@ drop ;

: TD-STALE-CASE ( -- )
   s" an interner is dead after its context ends" T-LABEL
   [: TD-READ ;] E-IR-ARENA-STALE TTHROWSQ ;

: TD-FRESH-CASE ( -- )
   s" fresh contexts and interners succeed after teardown" T-LABEL
   4 0 ?do
      BND [: DUP-BODY ;] IR-CTX:WITH-CONTEXT
      TTRUE 3 T= 1 T= 0 T= 1 T= 0 T=
   loop ;

\ ---- the checker keeps identities and the API sealed -------------------------
: CHECKER-CASES ( -- )
   \ positive control: a well-typed candidate over the same surface certifies,
   \ so the rejections below fail for their stated reason, not a harness typo
   s" IRSYM-POS ( IR-ARENA:arena -- n ) IR-SYM:SYMBOLS"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" IRSYM-ID-FORGE ( n -- IR-ID:ir-symbol-id )"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" IRSYM-KEY-FORGE ( n -- IR-ID:ir-module-key )"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" IRSYM-CTXLESS ( IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key ptr u8 n -- IR-ID:ir-symbol-id ) IR-SYM:INTERN"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" IRSYM-KEYLESS ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena ptr u8 n -- IR-ID:ir-symbol-id ) IR-SYM:INTERN"
      CHECK-QUIET-CANDIDATE! 0 T= ;

\ ---- run ---------------------------------------------------------------------
\ Throw-through fixtures run inside one outermost harness context, so a
\ context aborted by a throw is reclaimed by the harness exit instead of
\ lingering for the rest of the process. The teardown-reuse loop runs after
\ that exit, on registries the next sweep can fully reclaim.
: HARNESS-BODY ( IR-CTX:ctx -- )
   drop
   DUP-CASE
   BY-CASE
   CL-CASE
   CP-CASE
   LN-CASE
   XO-CASES
   BD-CASE
   CAP-CASES
   STATE-CASES
   FZ-CASE
   FZ-REJECT-CASES
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

IR-SYM-TEST:RUN
