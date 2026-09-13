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
\ "jest" and "yank" share the sixteen-bit content filter (and the byte
\ length), so unifying them is exactly what a filter-as-identity bug would do;
\ only the byte-compare verify step keeps them apart. Both hash to $615E under
\ IR-SYM:FILTER's FNV-1a, found by enumerating short words against that
\ function and confirmed by the first assertion below, which asks the
\ production filter rather than trusting this comment.
: CL-BODY ( IR-CTX:ctx -- bool n n bool bool n )
   {: c:IR-CTX:ctx :}
   c 8 64 TAB-NEW
   {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   s" jest" IR-SYM:FILTER s" yank" IR-SYM:FILTER =
   c a r key s" jest" IR-SYM:INTERN {: sv:IR-ID:ir-symbol-id :}
   c a r key s" yank" IR-SYM:INTERN {: sh:IR-ID:ir-symbol-id :}
   sv IR-ID:SYMBOL-LOCAL
   sh IR-ID:SYMBOL-LOCAL
   a r sv s" jest" IR-SYM:EQ?
   a r sh s" yank" IR-SYM:EQ?
   c a r key s" jest" IR-SYM:INTERN IR-ID:SYMBOL-LOCAL ;

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
   s" abc" {: p u:n :}
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

\ ---- arena growth preserves symbol bytes and rows ----------------------------
\ The second symbol grows both stores: its four payload cells follow the first
\ symbol's two, and its three-cell row follows the first row. Scratch can grow
\ beyond the former $80000 mapping while earlier identities and bytes survive.
: GROW-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 64 512 TAB-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   c a r key s" the first symbol" IR-SYM:INTERN {: s0:IR-ID:ir-symbol-id :}
   c $80000 IR-CTX:SCRATCH-TAKE 2drop
   c a r key s" abcdefghijklmnopqrstuvwxyz012345" IR-SYM:INTERN {: s1:IR-ID:ir-symbol-id :}
   r IR-SYM:SYMBOLS 2 T=
   a r s0 s" the first symbol" IR-SYM:EQ? TTRUE
   a r s1 s" abcdefghijklmnopqrstuvwxyz012345" IR-SYM:EQ? TTRUE
   c a r key s" the first symbol" IR-SYM:INTERN IR-ID:SYMBOL-LOCAL
      s0 IR-ID:SYMBOL-LOCAL T=
   a IR-ARENA:USED 3 2 + 4 + T=
   r IR-ARENA:USED 3 2 3 * + T= ;

: GROW-CASE ( -- )
   s" growing both symbol stores preserves identities and bytes" T-LABEL
   BND [: GROW-BODY ;] IR-CTX:WITH-CONTEXT ;

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

\ A frozen view that outlives its context. Every reader below resolves the view
\ once and then loads from that resolution, so this is the case the one-shot
\ resolution has to fail closed on: nothing touches the arena between the
\ context dying and the read, and the read still refuses.
: TD-VIEW-BODY ( IR-CTX:ctx -- IR-ARENA:view )
   {: c:IR-CTX:ctx :}
   c 4 64 TAB-NEW
   {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   c a r key s" tdv" IR-SYM:INTERN drop
   r IR-ARENA:FREEZE ;

: TD-VIEW-READ ( -- )
   BND [: TD-VIEW-BODY ;] IR-CTX:WITH-CONTEXT
   IR-SYM:FSYMBOLS drop ;

: TD-VIEW-CASE ( -- )
   s" a frozen view reads stale after its context ends" T-LABEL
   [: TD-VIEW-READ ;] E-IR-ARENA-STALE TTHROWSQ ;

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
   TD-STALE-CASE
   TD-VIEW-CASE ;

\ ---- an interner cloned from a prototype -------------------------------------
\ Keep the attempted clone inputs below CATCH so a refusal can be inspected
\ against the still-live prototype and the context's allocation cursor.
: CLONE-TRY ( IR-CTX:ctx IR-ID:ir-module-key IR-ARENA:arena IR-ARENA:arena n n -- IR-CTX:ctx IR-ID:ir-module-key IR-ARENA:arena IR-ARENA:arena n n )
   {: c:IR-CTX:ctx key:IR-ID:ir-module-key pa:IR-ARENA:arena pr:IR-ARENA:arena
      scap:n bcap:n :}
   c key pa pr scap bcap
   c key pa pr scap bcap IR-SYM:NEW-FROM 2drop ;

: CLONE-REFUSE-BODY ( IR-CTX:ctx -- bool bool bool bool bool )
   {: c:IR-CTX:ctx :}
   c IR-CTX:NEW-MODULE drop {: pk:IR-ID:ir-module-key :}
   c pk 4 32 IR-SYM:NEW {: pa:IR-ARENA:arena pr:IR-ARENA:arena :}
   c pa pr pk s" 12345678" IR-SYM:INTERN drop
   c pa pr pk s" x" IR-SYM:INTERN drop

   c IR-CTX:NEW-MODULE drop {: rk:IR-ID:ir-module-key :}
   c IR-CTX:SCRATCH-USED {: rs0:n :}
   c rk pa pr 1 9 [: CLONE-TRY ;] catch
   {: rc:IR-CTX:ctx rkey:IR-ID:ir-module-key rpa:IR-ARENA:arena
      rpr:IR-ARENA:arena rsc:n rbc:n rerr:n :}
   rerr E-IR-SYM-CAP =
   rc IR-CTX:SCRATCH-USED rs0 =

   c IR-CTX:NEW-MODULE drop {: bk:IR-ID:ir-module-key :}
   c IR-CTX:SCRATCH-USED {: bs0:n :}
   c bk pa pr 2 8 [: CLONE-TRY ;] catch
   {: bc:IR-CTX:ctx bkey:IR-ID:ir-module-key bpa:IR-ARENA:arena
      bpr:IR-ARENA:arena bsc:n bbc:n berr:n :}
   berr E-IR-SYM-BYTES =
   bc IR-CTX:SCRATCH-USED bs0 =
   pr IR-SYM:SYMBOLS 2 = ;

: CLONE-REFUSE-CASE ( -- )
   s" clone ceilings below live occupancy refuse before allocation" T-LABEL
   BND [: CLONE-REFUSE-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE TTRUE TTRUE TTRUE ;

: CLONE-MISS ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key -- IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key )
   {: c:IR-CTX:ctx a:IR-ARENA:arena r:IR-ARENA:arena key:IR-ID:ir-module-key :}
   c a r key
   c a r key s" new" IR-SYM:INTERN drop ;

: CLONE-EXACT-BODY ( IR-CTX:ctx -- bool bool bool bool bool bool bool )
   {: c:IR-CTX:ctx :}
   c IR-CTX:NEW-MODULE drop {: pk:IR-ID:ir-module-key :}
   c pk 4 32 IR-SYM:NEW {: pa:IR-ARENA:arena pr:IR-ARENA:arena :}
   c pa pr pk s" 12345678" IR-SYM:INTERN drop
   c pa pr pk s" x" IR-SYM:INTERN drop

   c IR-CTX:NEW-MODULE drop {: rk:IR-ID:ir-module-key :}
   c rk pa pr 2 9 IR-SYM:NEW-FROM
   {: ra:IR-ARENA:arena rr:IR-ARENA:arena :}
   rr IR-SYM:SYMBOLS 2 =
   c ra rr rk s" x" IR-SYM:INTERN IR-ID:SYMBOL-LOCAL 1 =
   rr IR-SYM:SYMBOLS 2 =
   c ra rr rk [: CLONE-MISS ;] catch
   {: rc:IR-CTX:ctx ra2:IR-ARENA:arena rr2:IR-ARENA:arena
      rk2:IR-ID:ir-module-key rerr:n :}
   rerr E-IR-SYM-CAP =
   rr2 IR-SYM:SYMBOLS 2 =

   c IR-CTX:NEW-MODULE drop {: bk:IR-ID:ir-module-key :}
   c bk pa pr 3 9 IR-SYM:NEW-FROM
   {: ba:IR-ARENA:arena br:IR-ARENA:arena :}
   c ba br bk [: CLONE-MISS ;] catch
   {: bc:IR-CTX:ctx ba2:IR-ARENA:arena br2:IR-ARENA:arena
      bk2:IR-ID:ir-module-key berr:n :}
   berr E-IR-SYM-BYTES =
   br2 IR-SYM:SYMBOLS 2 = ;

: CLONE-EXACT-CASE ( -- )
   s" an exact-fit clone keeps duplicates and enforces its future ceilings" T-LABEL
   BND [: CLONE-EXACT-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE ;

: CLONE-EMPTY-BODY ( IR-CTX:ctx -- bool bool bool bool )
   {: c:IR-CTX:ctx :}
   c IR-CTX:NEW-MODULE drop {: pk:IR-ID:ir-module-key :}
   c pk 4 32 IR-SYM:NEW {: pa:IR-ARENA:arena pr:IR-ARENA:arena :}
   c IR-CTX:NEW-MODULE drop {: ck:IR-ID:ir-module-key :}
   c ck pa pr 1 1 IR-SYM:NEW-FROM
   {: ca:IR-ARENA:arena cr:IR-ARENA:arena :}
   cr IR-SYM:SYMBOLS 0=
   c ca cr ck s" " IR-SYM:INTERN IR-ID:SYMBOL-LOCAL 0=
   c ca cr ck s" " IR-SYM:INTERN IR-ID:SYMBOL-LOCAL 0=
   cr IR-SYM:SYMBOLS 1 = ;

: CLONE-EMPTY-CASE ( -- )
   s" an empty prototype accepts the smallest clone ceilings" T-LABEL
   BND [: CLONE-EMPTY-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE TTRUE TTRUE ;

\ A clone is the same table under a new key: the spelling at ordinal k in the
\ prototype is the spelling at ordinal k in the clone, so a caller that recorded
\ an ordinal against the prototype can mint the identity it names in the new
\ module with no lookup. The prototype is read and never touched.
: PROTO-BODY ( IR-CTX:ctx -- n n n n n n n bool bool )
   {: c:IR-CTX:ctx :}
   c IR-CTX:NEW-MODULE drop {: pk:IR-ID:ir-module-key :}
   c pk 16 128 IR-SYM:NEW {: pa:IR-ARENA:arena pr:IR-ARENA:arena :}
   c pa pr pk s" hir.add" IR-SYM:INTERN {: p0:IR-ID:ir-symbol-id :}
   c pa pr pk s" hir.sub" IR-SYM:INTERN {: p1:IR-ID:ir-symbol-id :}
   \ Two spellings one fold maps together, and two the content filter is free to
   \ collide: both must stay two rows in the clone, exactly as in the prototype.
   c pa pr pk s" HIR.ADD" IR-SYM:INTERN {: p2:IR-ID:ir-symbol-id :}
   c pa pr pk s" ba" IR-SYM:INTERN {: p3:IR-ID:ir-symbol-id :}
   c pa pr pk s" ab" IR-SYM:INTERN {: p4:IR-ID:ir-symbol-id :}
   pr IR-SYM:SYMBOLS {: before:n :}

   c IR-CTX:NEW-MODULE {: ck:IR-ID:ir-module-key cmid:IR-ID:ir-module-id :}
   c ck pa pr 16 128 IR-SYM:NEW-FROM {: ca:IR-ARENA:arena cr:IR-ARENA:arena :}

   \ the same spelling is the same ordinal, and the clone answers it without
   \ appending anything
   c ca cr ck s" hir.sub" IR-SYM:INTERN {: c1:IR-ID:ir-symbol-id :}
   cr IR-SYM:SYMBOLS {: after-hit:n :}

   \ a spelling the prototype never held appends to the clone alone
   c ca cr ck s" hir.brandnew" IR-SYM:INTERN drop
   pr IR-SYM:SYMBOLS {: proto-after:n :}

   before
   cr IR-SYM:SYMBOLS 1-
   p1 IR-ID:SYMBOL-LOCAL
   c1 IR-ID:SYMBOL-LOCAL
   after-hit
   proto-after
   p2 IR-ID:SYMBOL-LOCAL p0 IR-ID:SYMBOL-LOCAL -
   p3 IR-ID:SYMBOL-LOCAL p4 IR-ID:SYMBOL-LOCAL <>
   c1 IR-ID:SYMBOL-OWNER cmid IR-ID:MODULE-SAME? ;

: PROTO-CASE ( -- )
   s" a cloned interner holds the prototype's spellings at the same ordinals" T-LABEL
   BND [: PROTO-BODY ;] IR-CTX:WITH-CONTEXT
   {: before:n cloned:n pord:n cord:n after-hit:n proto-after:n folded:n
      distinct:bool owned:bool :}
   owned TTRUE
   s" two spellings that fold together stay two rows in the clone" T-LABEL
   folded 0 <> TTRUE
   s" two spellings the filter may collide stay distinct in the clone" T-LABEL
   distinct TTRUE
   s" the clone starts with exactly the prototype's rows" T-LABEL
   cloned before T=
   s" the same spelling is the same ordinal on both sides" T-LABEL
   cord pord T=
   s" asking the clone for a spelling it copied appends nothing" T-LABEL
   after-hit before T=
   s" interning into the clone leaves the prototype where it was" T-LABEL
   proto-after before T= ;

public

: RUN ( -- )
   T-RESET
   CLONE-REFUSE-CASE
   CLONE-EXACT-CASE
   CLONE-EMPTY-CASE
   PROTO-CASE
   BND [: HARNESS-BODY ;] IR-CTX:WITH-CONTEXT
   GROW-CASE
   TD-FRESH-CASE
   CHECKER-CASES
   T-REPORT ;

;package

IR-SYM-TEST:RUN
