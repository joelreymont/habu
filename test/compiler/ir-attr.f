\ ir-attr.f - checked compiler attribute-table tests.
\
\ Proves the attribute contract of src/compiler/ir/attr.f: identical
\ attributes intern to one identity for every kind and across construction
\ orders, while every semantic field participates in identity - each kind has
\ a fixture that varies exactly one field and gets a distinct identity;
\ payloads are validated at construction, so a negative string length and a
\ duplicate record key reject named; symbol and type references are validated
\ against the module's own symbol interner and type table, so an id no table
\ vouches for rejects; cross-owner references (foreign module key, foreign
\ module's symbol/type/attribute, an identity minted in another context)
\ reject named; target enum attributes are checked against the context's
\ bound contract, so an attribute describing a different machine rejects
\ while the same value is declarable on a binding that states it; the staged
\ list protocols fail closed on misuse and a rejected end consumes the stage;
\ capacities reject named at the committed ceilings while the stores stay
\ readable and duplicates still answer; readers project every constructor and
\ reject wrong-kind and wrong-family reads; the render fixtures pin the
\ deterministic spelling of every kind independent of interning history;
\ bypass-forged rows reject fail-closed; a frozen module serves every reader
\ through the arena views while the retired builders reject; context teardown
\ releases everything; and checker fixtures prove the identity family and the
\ API stay sealed.

require lib/test.f
require lib/string.f
require test/checker-assert.f
require src/compiler/ir/attr.f

package IR-ATTR-TEST
private

create CBUF 256 allot

\ ---- bindings ----------------------------------------------------------------
\ An AArch64 Darwin contract: baseline and plain floating point, 64-bit
\ pointers, little-endian.
: BND ( -- CBIND:binding )
   CTARGET-ARCH:AARCH64 CTARGET-ABI:AAPCS64-DARWIN CTARGET-ENDIAN:LITTLE
   CTARGET-PTR--WIDTH:BITS64
   CTARGET:F-BASE CTARGET:F-FP CTARGET:WITH CTARGET:CONTRACT
   CNUM-OVERFLOW:TRAP CNUM-FLOAT--MODEL:IEEE754 CNUM-CONTRACTION:FORBIDDEN
   CNUM-FAST--MATH:BIT-EXACT CNUM-COMPARE:IEEE754-UNORDERED CNUM:POLICY
   CBIND:BIND ;

\ A PTX kernel contract: the same attribute values that reject above are
\ declarable here, because legality is the binding's fact.
: PBND ( -- CBIND:binding )
   CTARGET-ARCH:PTX CTARGET-ABI:PTX-KERNEL CTARGET-ENDIAN:LITTLE
   CTARGET-PTR--WIDTH:BITS64
   CTARGET:F-BASE CTARGET:F-FP CTARGET:WITH CTARGET:CONTRACT
   CNUM-OVERFLOW:TRAP CNUM-FLOAT--MODEL:IEEE754 CNUM-CONTRACTION:FORBIDDEN
   CNUM-FAST--MATH:BIT-EXACT CNUM-COMPARE:IEEE754-UNORDERED CNUM:POLICY
   CBIND:BIND ;

\ ---- fixtures ----------------------------------------------------------------
\ A fresh module with its attribute table: rcap attributes, pcap payload cells.
: TAB-NEW ( IR-CTX:ctx n n -- IR-ID:ir-module-key IR-ARENA:arena IR-ARENA:arena )
   {: c:IR-CTX:ctx rcap:n pcap:n :}
   c IR-CTX:NEW-MODULE drop {: key:IR-ID:ir-module-key :}
   key c key rcap pcap IR-ATTR:NEW ;

\ The same module's symbol interner and type table, the two validation
\ authorities a symbol or type reference is checked against.
: SYM-NEW ( IR-CTX:ctx IR-ID:ir-module-key -- IR-ARENA:arena IR-ARENA:arena )
   {: c:IR-CTX:ctx key:IR-ID:ir-module-key :}
   c key 16 128 IR-SYM:NEW ;

: TYP-NEW ( IR-CTX:ctx IR-ID:ir-module-key -- IR-ARENA:arena IR-ARENA:arena )
   {: c:IR-CTX:ctx key:IR-ID:ir-module-key :}
   c key 16 16 IR-TYPE:NEW ;

: SYM-A ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key -- IR-ID:ir-symbol-id )
   s" alpha" IR-SYM:INTERN ;

: SYM-B ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key -- IR-ID:ir-symbol-id )
   s" beta" IR-SYM:INTERN ;

: TY-I64 ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key -- IR-ID:ir-type-id )
   IR--TYPE-WIDTH:W64 IR--TYPE-SIGN:SIGNED IR-TYPE:INT ;

: TY-U8 ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key -- IR-ID:ir-type-id )
   IR--TYPE-WIDTH:W8 IR--TYPE-SIGN:UNSIGNED IR-TYPE:INT ;

\ Copy a literal into the scratch buffer, so a fixture can present the same
\ bytes from a different buffer than the one the literal lives in.
: FILL-CBUF ( ptr u8 n -- n )
   {: p u:n :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   p CBUF u BYTE-COPY
   u ;

: DG-A ( -- CDIGEST:digest )
   1 2 3 4 CDIGEST-DIGEST:MAKE ;

: DG-B ( -- CDIGEST:digest )
   1 2 3 5 CDIGEST-DIGEST:MAKE ;

\ ---- identical values intern to one identity ---------------------------------
\ Scalar kinds: the same value twice answers one identity, a changed value
\ mints the next, and the identity is owned by the minting module.
: IN-BODY ( IR-CTX:ctx -- n n n bool n bool )
   {: c:IR-CTX:ctx :}
   c IR-CTX:NEW-MODULE {: key:IR-ID:ir-module-key mid:IR-ID:ir-module-id :}
   c key 16 64 IR-ATTR:NEW {: a:IR-ARENA:arena r:IR-ARENA:arena :}
   c a r key 7 IR-ATTR:INT {: v0:IR-ID:ir-attr-id :}
   c a r key 8 IR-ATTR:INT {: v1:IR-ID:ir-attr-id :}
   c a r key 7 IR-ATTR:INT {: v2:IR-ID:ir-attr-id :}
   c a r key true IR-ATTR:BOOLEAN {: b0:IR-ID:ir-attr-id :}
   c a r key true IR-ATTR:BOOLEAN {: b1:IR-ID:ir-attr-id :}
   v0 IR-ID:ATTR-LOCAL
   v1 IR-ID:ATTR-LOCAL
   v2 IR-ID:ATTR-LOCAL
   b0 IR-ID:ATTR-LOCAL b1 IR-ID:ATTR-LOCAL =
   r IR-ATTR:ATTRS
   v0 IR-ID:ATTR-OWNER mid IR-ID:MODULE-SAME? ;

: IN-CASE ( -- )
   s" identical values answer one id; a changed value mints a new one" T-LABEL
   BND [: IN-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE 3 T= TTRUE 0 T= 1 T= 0 T= ;

\ Text bytes intern by content, from any buffer; one differing byte and one
\ differing length are each a distinct attribute.
: TX-BODY ( IR-CTX:ctx -- bool n n n n )
   {: c:IR-CTX:ctx :}
   c 16 64 TAB-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   s" nounroll" FILL-CBUF {: u:n :}
   c a r key s" nounroll" IR-ATTR:TEXT {: t0:IR-ID:ir-attr-id :}
   c a r key CBUF u IR-ATTR:TEXT {: t1:IR-ID:ir-attr-id :}
   c a r key s" nounrolx" IR-ATTR:TEXT {: t2:IR-ID:ir-attr-id :}
   c a r key s" nounrol" IR-ATTR:TEXT {: t3:IR-ID:ir-attr-id :}
   t0 IR-ID:ATTR-LOCAL t1 IR-ID:ATTR-LOCAL =
   t0 IR-ID:ATTR-LOCAL
   t2 IR-ID:ATTR-LOCAL
   t3 IR-ID:ATTR-LOCAL
   r IR-ATTR:ATTRS ;

: TX-CASE ( -- )
   s" equal bytes from any buffer are one attribute; a byte or a length differs" T-LABEL
   BND [: TX-BODY ;] IR-CTX:WITH-CONTEXT
   3 T= 2 T= 1 T= 0 T= TTRUE ;

\ Reference kinds: the referenced ordinal is the identity field.
: RF-BODY ( IR-CTX:ctx -- bool n bool n n )
   {: c:IR-CTX:ctx :}
   c 16 64 TAB-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   c key SYM-NEW {: sa:IR-ARENA:arena sr:IR-ARENA:arena :}
   c key TYP-NEW {: ta:IR-ARENA:arena tr:IR-ARENA:arena :}
   c sa sr key SYM-A {: s0:IR-ID:ir-symbol-id :}
   c sa sr key SYM-B {: s1:IR-ID:ir-symbol-id :}
   c ta tr key TY-I64 {: y0:IR-ID:ir-type-id :}
   c ta tr key TY-U8 {: y1:IR-ID:ir-type-id :}
   c a r key sr s0 IR-ATTR:SYMBOL {: m0:IR-ID:ir-attr-id :}
   c a r key sr s0 IR-ATTR:SYMBOL {: m1:IR-ID:ir-attr-id :}
   c a r key sr s1 IR-ATTR:SYMBOL {: m2:IR-ID:ir-attr-id :}
   c a r key tr y0 IR-ATTR:TYPE-REF {: p0:IR-ID:ir-attr-id :}
   c a r key tr y0 IR-ATTR:TYPE-REF {: p1:IR-ID:ir-attr-id :}
   c a r key tr y1 IR-ATTR:TYPE-REF {: p2:IR-ID:ir-attr-id :}
   m0 IR-ID:ATTR-LOCAL m1 IR-ID:ATTR-LOCAL =
   m2 IR-ID:ATTR-LOCAL
   p0 IR-ID:ATTR-LOCAL p1 IR-ID:ATTR-LOCAL =
   p2 IR-ID:ATTR-LOCAL
   r IR-ATTR:ATTRS ;

: RF-CASE ( -- )
   s" a reference interns by referenced ordinal; another referent differs" T-LABEL
   BND [: RF-BODY ;] IR-CTX:WITH-CONTEXT
   4 T= 3 T= TTRUE 1 T= TTRUE ;

\ The two truth values are distinct attributes.
: BL-BODY ( IR-CTX:ctx -- bool n n )
   {: c:IR-CTX:ctx :}
   c 16 16 TAB-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   c a r key true IR-ATTR:BOOLEAN {: b0:IR-ID:ir-attr-id :}
   c a r key false IR-ATTR:BOOLEAN {: b1:IR-ID:ir-attr-id :}
   c a r key true IR-ATTR:BOOLEAN {: b2:IR-ID:ir-attr-id :}
   b0 IR-ID:ATTR-LOCAL b2 IR-ID:ATTR-LOCAL =
   b1 IR-ID:ATTR-LOCAL
   r IR-ATTR:ATTRS ;

: BL-CASE ( -- )
   s" true and false are distinct attributes" T-LABEL
   BND [: BL-BODY ;] IR-CTX:WITH-CONTEXT
   2 T= 1 T= TTRUE ;

\ A digest attribute's four words all participate: four neighbours, each
\ differing in exactly one word, are four more attributes.
: DG-BODY ( IR-CTX:ctx -- bool n )
   {: c:IR-CTX:ctx :}
   c 16 16 TAB-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   c a r key DG-A IR-ATTR:DIGEST {: d0:IR-ID:ir-attr-id :}
   c a r key DG-A IR-ATTR:DIGEST {: d1:IR-ID:ir-attr-id :}
   c a r key 9 2 3 4 CDIGEST-DIGEST:MAKE IR-ATTR:DIGEST drop
   c a r key 1 9 3 4 CDIGEST-DIGEST:MAKE IR-ATTR:DIGEST drop
   c a r key 1 2 9 4 CDIGEST-DIGEST:MAKE IR-ATTR:DIGEST drop
   c a r key DG-B IR-ATTR:DIGEST drop
   d0 IR-ID:ATTR-LOCAL d1 IR-ID:ATTR-LOCAL =
   r IR-ATTR:ATTRS ;

: DG-CASE ( -- )
   s" a digest interns on all four words; each word alone differs" T-LABEL
   BND [: DG-BODY ;] IR-CTX:WITH-CONTEXT
   5 T= TTRUE ;

\ Integer lists: equal element sequences intern, and a changed element, a
\ changed order, and a changed length are each distinct.
: IL-BODY ( IR-CTX:ctx -- bool n n n n )
   {: c:IR-CTX:ctx :}
   c 16 64 TAB-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   IR-ATTR:IL-BEGIN 1 IR-ATTR:IL-ADD 2 IR-ATTR:IL-ADD 3 IR-ATTR:IL-ADD
   c a r key IR-ATTR:INT-LIST {: l0:IR-ID:ir-attr-id :}
   IR-ATTR:IL-BEGIN 1 IR-ATTR:IL-ADD 2 IR-ATTR:IL-ADD 3 IR-ATTR:IL-ADD
   c a r key IR-ATTR:INT-LIST {: l1:IR-ID:ir-attr-id :}
   IR-ATTR:IL-BEGIN 1 IR-ATTR:IL-ADD 2 IR-ATTR:IL-ADD 4 IR-ATTR:IL-ADD
   c a r key IR-ATTR:INT-LIST {: l2:IR-ID:ir-attr-id :}
   IR-ATTR:IL-BEGIN 3 IR-ATTR:IL-ADD 2 IR-ATTR:IL-ADD 1 IR-ATTR:IL-ADD
   c a r key IR-ATTR:INT-LIST {: l3:IR-ID:ir-attr-id :}
   IR-ATTR:IL-BEGIN 1 IR-ATTR:IL-ADD 2 IR-ATTR:IL-ADD
   c a r key IR-ATTR:INT-LIST {: l4:IR-ID:ir-attr-id :}
   l0 IR-ID:ATTR-LOCAL l1 IR-ID:ATTR-LOCAL =
   l2 IR-ID:ATTR-LOCAL
   l3 IR-ID:ATTR-LOCAL
   l4 IR-ID:ATTR-LOCAL
   r IR-ATTR:ATTRS ;

: IL-CASE ( -- )
   s" a list interns on its elements; value, order, and length all differ" T-LABEL
   BND [: IL-BODY ;] IR-CTX:WITH-CONTEXT
   4 T= 3 T= 2 T= 1 T= TTRUE ;

\ Enum attributes: family and member both participate, and two families that
\ share a member code stay apart.
: EN-BODY ( IR-CTX:ctx -- bool n n n )
   {: c:IR-CTX:ctx :}
   c 16 16 TAB-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   c a r key CNUM-FAST--MATH:REASSOCIATE IR-ATTR:FAST-MATH {: e0:IR-ID:ir-attr-id :}
   c a r key CNUM-FAST--MATH:REASSOCIATE IR-ATTR:FAST-MATH {: e1:IR-ID:ir-attr-id :}
   c a r key CNUM-FAST--MATH:APPROXIMATE IR-ATTR:FAST-MATH {: e2:IR-ID:ir-attr-id :}
   c a r key CNUM-OVERFLOW:TRAP IR-ATTR:OVERFLOW {: e3:IR-ID:ir-attr-id :}
   c a r key CNUM-CONTRACTION:ALLOWED IR-ATTR:CONTRACTION {: e4:IR-ID:ir-attr-id :}
   e0 IR-ID:ATTR-LOCAL e1 IR-ID:ATTR-LOCAL =
   e2 IR-ID:ATTR-LOCAL
   e4 IR-ID:ATTR-LOCAL
   r IR-ATTR:ATTRS ;

: EN-CASE ( -- )
   s" an enum interns on family and member; families sharing a code stay apart" T-LABEL
   BND [: EN-BODY ;] IR-CTX:WITH-CONTEXT
   4 T= 3 T= 1 T= TTRUE ;

\ Records: keys, values, and pair count all participate, and the pair
\ presentation order does not - the canonical key order is the identity.
: RC-BODY ( IR-CTX:ctx -- bool bool n n n )
   {: c:IR-CTX:ctx :}
   c 32 64 TAB-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   c key SYM-NEW {: sa:IR-ARENA:arena sr:IR-ARENA:arena :}
   c sa sr key SYM-A {: s0:IR-ID:ir-symbol-id :}
   c sa sr key SYM-B {: s1:IR-ID:ir-symbol-id :}
   c a r key 1 IR-ATTR:INT {: v1:IR-ID:ir-attr-id :}
   c a r key 2 IR-ATTR:INT {: v2:IR-ID:ir-attr-id :}
   IR-ATTR:REC-BEGIN s0 v1 IR-ATTR:REC-PAIR s1 v2 IR-ATTR:REC-PAIR
   c a r key sr IR-ATTR:RECORD {: r0:IR-ID:ir-attr-id :}
   IR-ATTR:REC-BEGIN s1 v2 IR-ATTR:REC-PAIR s0 v1 IR-ATTR:REC-PAIR
   c a r key sr IR-ATTR:RECORD {: r1:IR-ID:ir-attr-id :}
   IR-ATTR:REC-BEGIN s0 v1 IR-ATTR:REC-PAIR s1 v1 IR-ATTR:REC-PAIR
   c a r key sr IR-ATTR:RECORD {: r2:IR-ID:ir-attr-id :}
   IR-ATTR:REC-BEGIN s0 v1 IR-ATTR:REC-PAIR
   c a r key sr IR-ATTR:RECORD {: r3:IR-ID:ir-attr-id :}
   r0 IR-ID:ATTR-LOCAL r1 IR-ID:ATTR-LOCAL =
   r0 IR-ID:ATTR-LOCAL r2 IR-ID:ATTR-LOCAL =
   r2 IR-ID:ATTR-LOCAL
   r3 IR-ID:ATTR-LOCAL
   r IR-ATTR:ATTRS ;

: RC-CASE ( -- )
   s" a record interns on its sorted pairs; pair order does not reach identity" T-LABEL
   BND [: RC-BODY ;] IR-CTX:WITH-CONTEXT
   5 T= 4 T= 3 T= TFALSE TTRUE ;

\ A key change is a distinct record even when both values are equal.
: RCK-BODY ( IR-CTX:ctx -- n n )
   {: c:IR-CTX:ctx :}
   c 32 64 TAB-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   c key SYM-NEW {: sa:IR-ARENA:arena sr:IR-ARENA:arena :}
   c sa sr key SYM-A {: s0:IR-ID:ir-symbol-id :}
   c sa sr key SYM-B {: s1:IR-ID:ir-symbol-id :}
   c a r key 1 IR-ATTR:INT {: v1:IR-ID:ir-attr-id :}
   IR-ATTR:REC-BEGIN s0 v1 IR-ATTR:REC-PAIR
   c a r key sr IR-ATTR:RECORD {: r0:IR-ID:ir-attr-id :}
   IR-ATTR:REC-BEGIN s1 v1 IR-ATTR:REC-PAIR
   c a r key sr IR-ATTR:RECORD {: r1:IR-ID:ir-attr-id :}
   r0 IR-ID:ATTR-LOCAL
   r1 IR-ID:ATTR-LOCAL ;

: RCK-CASE ( -- )
   s" one changed record key is a distinct attribute" T-LABEL
   BND [: RCK-BODY ;] IR-CTX:WITH-CONTEXT
   2 T= 1 T= ;

\ ---- construction order does not reach identity ------------------------------
\ The same attribute built in two tables with different construction
\ histories renders identically; within one table, a value interned after
\ unrelated attributes still answers the identity it already holds.
: ORD-A-BODY ( IR-CTX:ctx -- bool )
   {: c:IR-CTX:ctx :}
   c 16 64 TAB-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   c a r key -5 IR-ATTR:INT {: v:IR-ID:ir-attr-id :}
   a r v CBUF 256 IR-ATTR:RENDER {: n0:n :}
   CBUF n0 s" int(-5)" STR= ;

: ORD-B-BODY ( IR-CTX:ctx -- bool bool )
   {: c:IR-CTX:ctx :}
   c 16 64 TAB-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   c a r key s" pad" IR-ATTR:TEXT drop
   c a r key false IR-ATTR:BOOLEAN drop
   c a r key -5 IR-ATTR:INT {: v0:IR-ID:ir-attr-id :}
   c a r key 12 IR-ATTR:INT drop
   c a r key -5 IR-ATTR:INT {: v1:IR-ID:ir-attr-id :}
   a r v0 CBUF 256 IR-ATTR:RENDER {: n0:n :}
   CBUF n0 s" int(-5)" STR=
   v0 IR-ID:ATTR-LOCAL v1 IR-ID:ATTR-LOCAL = ;

: ORD-CASE ( -- )
   s" the same value renders identically under different histories" T-LABEL
   BND [: ORD-A-BODY ;] IR-CTX:WITH-CONTEXT TTRUE
   BND [: ORD-B-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE ;

\ ---- payload validation ------------------------------------------------------
: BADLEN-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 16 TAB-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   c a r key CBUF -1 IR-ATTR:TEXT drop ;

: BADLEN ( -- )
   BND [: BADLEN-BODY ;] IR-CTX:WITH-CONTEXT ;

: DUPKEY-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 16 32 TAB-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   c key SYM-NEW {: sa:IR-ARENA:arena sr:IR-ARENA:arena :}
   c sa sr key SYM-A {: s0:IR-ID:ir-symbol-id :}
   c a r key 1 IR-ATTR:INT {: v1:IR-ID:ir-attr-id :}
   c a r key 2 IR-ATTR:INT {: v2:IR-ID:ir-attr-id :}
   IR-ATTR:REC-BEGIN s0 v1 IR-ATTR:REC-PAIR s0 v2 IR-ATTR:REC-PAIR
   c a r key sr IR-ATTR:RECORD drop ;

: DUPKEY ( -- )
   BND [: DUPKEY-BODY ;] IR-CTX:WITH-CONTEXT ;

\ A symbol id of this module that the interner never minted: the referenced
\ table is the validation authority and answers with its own named error.
: NOSYM-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 16 TAB-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   c key SYM-NEW {: sa:IR-ARENA:arena sr:IR-ARENA:arena :}
   c sa sr key SYM-A drop
   key sr IR-SYM:SYMBOLS IR-ID:PACK-SYMBOL {: ghost:IR-ID:ir-symbol-id :}
   c a r key sr ghost IR-ATTR:SYMBOL drop ;

: NOSYM ( -- )
   BND [: NOSYM-BODY ;] IR-CTX:WITH-CONTEXT ;

: NOTYPE-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 16 TAB-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   c key TYP-NEW {: ta:IR-ARENA:arena tr:IR-ARENA:arena :}
   c ta tr key TY-I64 drop
   key tr IR-TYPE:TYPES IR-ID:PACK-TYPE {: ghost:IR-ID:ir-type-id :}
   c a r key tr ghost IR-ATTR:TYPE-REF drop ;

: NOTYPE ( -- )
   BND [: NOTYPE-BODY ;] IR-CTX:WITH-CONTEXT ;

\ A staged record value that is not yet a constructed attribute: no forward
\ references, so no record can close a cycle.
: FWDVAL-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 16 32 TAB-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   c key SYM-NEW {: sa:IR-ARENA:arena sr:IR-ARENA:arena :}
   c sa sr key SYM-A {: s0:IR-ID:ir-symbol-id :}
   c a r key 1 IR-ATTR:INT drop
   key r IR-ATTR:ATTRS IR-ID:PACK-ATTR {: ghost:IR-ID:ir-attr-id :}
   IR-ATTR:REC-BEGIN s0 ghost IR-ATTR:REC-PAIR
   c a r key sr IR-ATTR:RECORD drop ;

: FWDVAL ( -- )
   BND [: FWDVAL-BODY ;] IR-CTX:WITH-CONTEXT ;

: PAYLOAD-CASES ( -- )
   s" a negative string length rejects named" T-LABEL
   [: BADLEN ;] E-IR-ATTR-VALUE TTHROWSQ
   s" a duplicate record key rejects named" T-LABEL
   [: DUPKEY ;] E-IR-ATTR-VALUE TTHROWSQ
   s" a symbol the interner never minted rejects at the interner" T-LABEL
   [: NOSYM ;] E-IR-SYM-BOUND TTHROWSQ
   s" a type the type table never built rejects at the type table" T-LABEL
   [: NOTYPE ;] E-IR-TYPE-BOUND TTHROWSQ
   s" a not-yet-constructed record value rejects: no forward references" T-LABEL
   [: FWDVAL ;] E-IR-ATTR-BOUND TTHROWSQ ;

\ ---- cross-owner references --------------------------------------------------
: XM-KEY-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 16 TAB-NEW {: keya:IR-ID:ir-module-key a:IR-ARENA:arena ra:IR-ARENA:arena :}
   c 8 16 TAB-NEW {: keyb:IR-ID:ir-module-key b:IR-ARENA:arena rb:IR-ARENA:arena :}
   c a ra keyb 1 IR-ATTR:INT drop ;

: XM-KEY ( -- )
   BND [: XM-KEY-BODY ;] IR-CTX:WITH-CONTEXT ;

: XM-SYM-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 16 TAB-NEW {: keya:IR-ID:ir-module-key a:IR-ARENA:arena ra:IR-ARENA:arena :}
   c 8 16 TAB-NEW {: keyb:IR-ID:ir-module-key b:IR-ARENA:arena rb:IR-ARENA:arena :}
   c keyb SYM-NEW {: sa:IR-ARENA:arena sr:IR-ARENA:arena :}
   c sa sr keyb SYM-A {: sb:IR-ID:ir-symbol-id :}
   c a ra keya sr sb IR-ATTR:SYMBOL drop ;

: XM-SYM ( -- )
   BND [: XM-SYM-BODY ;] IR-CTX:WITH-CONTEXT ;

: XM-TYPE-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 16 TAB-NEW {: keya:IR-ID:ir-module-key a:IR-ARENA:arena ra:IR-ARENA:arena :}
   c 8 16 TAB-NEW {: keyb:IR-ID:ir-module-key b:IR-ARENA:arena rb:IR-ARENA:arena :}
   c keyb TYP-NEW {: ta:IR-ARENA:arena tr:IR-ARENA:arena :}
   c ta tr keyb TY-I64 {: yb:IR-ID:ir-type-id :}
   c a ra keya tr yb IR-ATTR:TYPE-REF drop ;

: XM-TYPE ( -- )
   BND [: XM-TYPE-BODY ;] IR-CTX:WITH-CONTEXT ;

\ A record whose staged value belongs to another module's table.
: XM-VAL-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 16 32 TAB-NEW {: keya:IR-ID:ir-module-key a:IR-ARENA:arena ra:IR-ARENA:arena :}
   c 16 32 TAB-NEW {: keyb:IR-ID:ir-module-key b:IR-ARENA:arena rb:IR-ARENA:arena :}
   c keya SYM-NEW {: sa:IR-ARENA:arena sr:IR-ARENA:arena :}
   c sa sr keya SYM-A {: s0:IR-ID:ir-symbol-id :}
   c b rb keyb 1 IR-ATTR:INT {: vb:IR-ID:ir-attr-id :}
   IR-ATTR:REC-BEGIN s0 vb IR-ATTR:REC-PAIR
   c a ra keya sr IR-ATTR:RECORD drop ;

: XM-VAL ( -- )
   BND [: XM-VAL-BODY ;] IR-CTX:WITH-CONTEXT ;

\ A record whose staged key belongs to another module's interner.
: XM-RKEY-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 16 32 TAB-NEW {: keya:IR-ID:ir-module-key a:IR-ARENA:arena ra:IR-ARENA:arena :}
   c 16 32 TAB-NEW {: keyb:IR-ID:ir-module-key b:IR-ARENA:arena rb:IR-ARENA:arena :}
   c keyb SYM-NEW {: sa:IR-ARENA:arena sr:IR-ARENA:arena :}
   c sa sr keyb SYM-A {: sb:IR-ID:ir-symbol-id :}
   c a ra keya 1 IR-ATTR:INT {: va:IR-ID:ir-attr-id :}
   IR-ATTR:REC-BEGIN sb va IR-ATTR:REC-PAIR
   c a ra keya sr IR-ATTR:RECORD drop ;

: XM-RKEY ( -- )
   BND [: XM-RKEY-BODY ;] IR-CTX:WITH-CONTEXT ;

\ An attribute id minted inside context A rejects as a record value in
\ context B.
: XC-INNER ( IR-ID:ir-attr-id IR-CTX:ctx -- )
   {: id:IR-ID:ir-attr-id c2:IR-CTX:ctx :}
   c2 16 32 TAB-NEW {: k2:IR-ID:ir-module-key b:IR-ARENA:arena rb:IR-ARENA:arena :}
   c2 k2 SYM-NEW {: sa:IR-ARENA:arena sr:IR-ARENA:arena :}
   c2 sa sr k2 SYM-A {: s0:IR-ID:ir-symbol-id :}
   IR-ATTR:REC-BEGIN s0 id IR-ATTR:REC-PAIR
   c2 b rb k2 sr IR-ATTR:RECORD drop ;

: XC-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 16 32 TAB-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   c a r key 1 IR-ATTR:INT
   BND [: XC-INNER ;] IR-CTX:WITH-CONTEXT ;

: XC-RUN ( -- )
   BND [: XC-BODY ;] IR-CTX:WITH-CONTEXT ;

: OWNER-CASES ( -- )
   s" a foreign module key rejects at construction" T-LABEL
   [: XM-KEY ;] E-IR-ATTR-OWNER TTHROWSQ
   s" a foreign module's symbol rejects as a reference" T-LABEL
   [: XM-SYM ;] E-IR-ATTR-OWNER TTHROWSQ
   s" a foreign module's type rejects as a reference" T-LABEL
   [: XM-TYPE ;] E-IR-ATTR-OWNER TTHROWSQ
   s" a foreign module's attribute rejects as a record value" T-LABEL
   [: XM-VAL ;] E-IR-ATTR-OWNER TTHROWSQ
   s" a foreign module's symbol rejects as a record key" T-LABEL
   [: XM-RKEY ;] E-IR-ATTR-OWNER TTHROWSQ
   s" an attribute from context A rejects in context B's table" T-LABEL
   [: XC-RUN ;] E-IR-ATTR-OWNER TTHROWSQ ;

\ ---- target legality through the context binding -----------------------------
: TGT-ARCH-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 16 TAB-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   c a r key CTARGET-ARCH:PTX IR-ATTR:ARCH drop ;

: TGT-ARCH ( -- )
   BND [: TGT-ARCH-BODY ;] IR-CTX:WITH-CONTEXT ;

: TGT-ABI-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 16 TAB-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   c a r key CTARGET-ABI:AAPCS64-LINUX IR-ATTR:ABI drop ;

: TGT-ABI ( -- )
   BND [: TGT-ABI-BODY ;] IR-CTX:WITH-CONTEXT ;

: TGT-END-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 16 TAB-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   c a r key CTARGET-ENDIAN:BIG IR-ATTR:ENDIAN drop ;

: TGT-END ( -- )
   BND [: TGT-END-BODY ;] IR-CTX:WITH-CONTEXT ;

: TGT-PTRW-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 16 TAB-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   c a r key CTARGET-PTR--WIDTH:BITS32 IR-ATTR:PTR-WIDTH drop ;

: TGT-PTRW ( -- )
   BND [: TGT-PTRW-BODY ;] IR-CTX:WITH-CONTEXT ;

\ The same members are declarable on the binding that states them, and the
\ agreeing values are declarable on the AArch64 binding too.
: TGT-OK-BODY ( IR-CTX:ctx -- bool bool n )
   {: c:IR-CTX:ctx :}
   c 16 16 TAB-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   c a r key CTARGET-ARCH:PTX IR-ATTR:ARCH {: t0:IR-ID:ir-attr-id :}
   c a r key CTARGET-ABI:PTX-KERNEL IR-ATTR:ABI drop
   c a r key CTARGET-ENDIAN:LITTLE IR-ATTR:ENDIAN drop
   c a r key CTARGET-PTR--WIDTH:BITS64 IR-ATTR:PTR-WIDTH drop
   r t0 IR-ATTR:ARCH@ CTARGET-ARCH:PTX CTARGET-ARCH:EQ
   a r t0 CBUF 256 IR-ATTR:RENDER {: n0:n :}
   CBUF n0 s" arch:ptx" STR=
   r IR-ATTR:ATTRS ;

: TGT-CASES ( -- )
   s" an arch attribute naming another machine rejects" T-LABEL
   [: TGT-ARCH ;] E-IR-ATTR-TARGET TTHROWSQ
   s" an ABI attribute the binding contradicts rejects" T-LABEL
   [: TGT-ABI ;] E-IR-ATTR-TARGET TTHROWSQ
   s" a byte-order attribute the binding contradicts rejects" T-LABEL
   [: TGT-END ;] E-IR-ATTR-TARGET TTHROWSQ
   s" a pointer-width attribute the binding contradicts rejects" T-LABEL
   [: TGT-PTRW ;] E-IR-ATTR-TARGET TTHROWSQ
   s" the same target attributes are declarable on the PTX binding" T-LABEL
   PBND [: TGT-OK-BODY ;] IR-CTX:WITH-CONTEXT
   4 T= TTRUE TTRUE ;

\ ---- stage protocol ----------------------------------------------------------
: STG-END-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 16 TAB-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   c a r key IR-ATTR:INT-LIST drop ;

: STG-END ( -- )
   BND [: STG-END-BODY ;] IR-CTX:WITH-CONTEXT ;

\ An integer-list end cannot close a record stage.
: STG-MIX-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 16 TAB-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   IR-ATTR:REC-BEGIN
   c a r key IR-ATTR:INT-LIST drop ;

: STG-MIX ( -- )
   BND [: STG-MIX-BODY ;] IR-CTX:WITH-CONTEXT ;

\ A second begin while one is open rejects; the open stage still ends.
: STG-DBL-BODY ( IR-CTX:ctx -- n n )
   {: c:IR-CTX:ctx :}
   c 8 16 TAB-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   IR-ATTR:IL-BEGIN
   [: IR-ATTR:IL-BEGIN ;] catch
   c a r key IR-ATTR:INT-LIST IR-ID:ATTR-LOCAL ;

\ The thirty-third staged element rejects; the open stage still ends.
: STG-OVER-BODY ( IR-CTX:ctx -- n n )
   {: c:IR-CTX:ctx :}
   c 8 64 TAB-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   IR-ATTR:IL-BEGIN
   32 0 ?do
      i IR-ATTR:IL-ADD
   loop
   [: 99 IR-ATTR:IL-ADD ;] catch
   c a r key IR-ATTR:INT-LIST IR-ID:ATTR-LOCAL ;

\ The caught end re-pushes its inputs before the throwing close, so the
\ quotation stays stack-preserving and the quadruple survives the reject.
: STG-END4 ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key -- IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key )
   {: c:IR-CTX:ctx b:IR-ARENA:arena rb:IR-ARENA:arena key:IR-ID:ir-module-key :}
   c b rb key
   c b rb key IR-ATTR:INT-LIST drop ;

\ A rejected end consumes the stage: the next end has nothing to close.
: STG-CONSUMED-BODY ( IR-CTX:ctx -- n n )
   {: c:IR-CTX:ctx :}
   c 8 16 TAB-NEW {: keya:IR-ID:ir-module-key a:IR-ARENA:arena ra:IR-ARENA:arena :}
   c 8 16 TAB-NEW {: keyb:IR-ID:ir-module-key b:IR-ARENA:arena rb:IR-ARENA:arena :}
   IR-ATTR:IL-BEGIN
   1 IR-ATTR:IL-ADD
   c a ra keyb [: STG-END4 ;] catch
   {: c2:IR-CTX:ctx a2:IR-ARENA:arena ra2:IR-ARENA:arena k2:IR-ID:ir-module-key rc:n :}
   rc
   c2 a2 ra2 k2 [: STG-END4 ;] catch
   {: c3:IR-CTX:ctx a3:IR-ARENA:arena ra3:IR-ARENA:arena k3:IR-ID:ir-module-key rc2:n :}
   rc2 ;

: STG-CASES ( -- )
   s" an end without an open stage rejects" T-LABEL
   [: STG-END ;] E-IR-ATTR-STAGE TTHROWSQ
   s" a list end cannot close a record stage" T-LABEL
   [: STG-MIX ;] E-IR-ATTR-STAGE TTHROWSQ
   s" a begin while a stage is open rejects; the stage still ends" T-LABEL
   BND [: STG-DBL-BODY ;] IR-CTX:WITH-CONTEXT
   0 T= E-IR-ATTR-STAGE T=
   s" a staged list past the ceiling rejects; the stage still ends" T-LABEL
   BND [: STG-OVER-BODY ;] IR-CTX:WITH-CONTEXT
   0 T= E-IR-ATTR-STAGE T=
   s" a rejected end consumes the stage" T-LABEL
   BND [: STG-CONSUMED-BODY ;] IR-CTX:WITH-CONTEXT
   E-IR-ATTR-STAGE T= E-IR-ATTR-OWNER T= ;

\ ---- capacity ----------------------------------------------------------------
: CAP-ZERO-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 0 16 TAB-NEW IR-ARENA:ABORT IR-ARENA:ABORT drop ;

: CAP-ZERO ( -- )
   BND [: CAP-ZERO-BODY ;] IR-CTX:WITH-CONTEXT ;

: CAP-NEG-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c -3 16 TAB-NEW IR-ARENA:ABORT IR-ARENA:ABORT drop ;

: CAP-NEG ( -- )
   BND [: CAP-NEG-BODY ;] IR-CTX:WITH-CONTEXT ;

: CAP-HUGE-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c $60000000 16 TAB-NEW IR-ARENA:ABORT IR-ARENA:ABORT drop ;

: CAP-HUGE ( -- )
   BND [: CAP-HUGE-BODY ;] IR-CTX:WITH-CONTEXT ;

: PCAP-ZERO-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 0 TAB-NEW IR-ARENA:ABORT IR-ARENA:ABORT drop ;

: PCAP-ZERO ( -- )
   BND [: PCAP-ZERO-BODY ;] IR-CTX:WITH-CONTEXT ;

: PCAP-HUGE-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 $100000000 TAB-NEW IR-ARENA:ABORT IR-ARENA:ABORT drop ;

: PCAP-HUGE ( -- )
   BND [: PCAP-HUGE-BODY ;] IR-CTX:WITH-CONTEXT ;

\ The caught quotation re-pushes its inputs before the throwing call, so the
\ stores stay readable after the named reject.
: CAPF-THIRD ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key -- IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key )
   {: c:IR-CTX:ctx a:IR-ARENA:arena r:IR-ARENA:arena key:IR-ID:ir-module-key :}
   c a r key
   c a r key 33 IR-ATTR:INT drop ;

: CAPF-BODY ( IR-CTX:ctx -- n n n )
   {: c:IR-CTX:ctx :}
   c 2 16 TAB-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   c a r key 11 IR-ATTR:INT drop
   c a r key 22 IR-ATTR:INT drop
   c a r key [: CAPF-THIRD ;] catch
   {: c2:IR-CTX:ctx a2:IR-ARENA:arena r2:IR-ARENA:arena key2:IR-ID:ir-module-key rc:n :}
   rc
   r2 IR-ATTR:ATTRS
   c2 a2 r2 key2 11 IR-ATTR:INT IR-ID:ATTR-LOCAL ;

: POOLF-NEXT ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key -- IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key )
   {: c:IR-CTX:ctx a:IR-ARENA:arena r:IR-ARENA:arena key:IR-ID:ir-module-key :}
   c a r key
   IR-ATTR:IL-BEGIN
   7 IR-ATTR:IL-ADD
   8 IR-ATTR:IL-ADD
   c a r key IR-ATTR:INT-LIST drop ;

: POOLF-BODY ( IR-CTX:ctx -- n n n )
   {: c:IR-CTX:ctx :}
   c 8 2 TAB-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   IR-ATTR:IL-BEGIN
   1 IR-ATTR:IL-ADD
   2 IR-ATTR:IL-ADD
   c a r key IR-ATTR:INT-LIST {: l0:IR-ID:ir-attr-id :}
   c a r key [: POOLF-NEXT ;] catch
   {: c2:IR-CTX:ctx a2:IR-ARENA:arena r2:IR-ARENA:arena key2:IR-ID:ir-module-key rc:n :}
   rc
   IR-ATTR:IL-BEGIN
   1 IR-ATTR:IL-ADD
   2 IR-ATTR:IL-ADD
   c2 a2 r2 key2 IR-ATTR:INT-LIST IR-ID:ATTR-LOCAL
   l0 IR-ID:ATTR-LOCAL ;

: CAP-CASES ( -- )
   s" a zero row capacity is rejected at creation" T-LABEL
   [: CAP-ZERO ;] E-IR-ATTR-CAP TTHROWSQ
   s" a negative row capacity is rejected at creation" T-LABEL
   [: CAP-NEG ;] E-IR-ATTR-CAP TTHROWSQ
   s" a row capacity past the ordinal range is rejected" T-LABEL
   [: CAP-HUGE ;] E-IR-ATTR-CAP TTHROWSQ
   s" a zero payload capacity is rejected at creation" T-LABEL
   [: PCAP-ZERO ;] E-IR-ATTR-CAP TTHROWSQ
   s" a payload capacity past the pool cell range is rejected" T-LABEL
   [: PCAP-HUGE ;] E-IR-ATTR-CAP TTHROWSQ
   s" a full row table rejects named; duplicates still answer" T-LABEL
   BND [: CAPF-BODY ;] IR-CTX:WITH-CONTEXT
   0 T= 2 T= E-IR-ATTR-CAP T=
   s" a full payload pool rejects named; duplicate lists still answer" T-LABEL
   BND [: POOLF-BODY ;] IR-CTX:WITH-CONTEXT
   0 T= 0 T= E-IR-ATTR-CAP T= ;

\ ---- readers project every kind ----------------------------------------------
: RD-BODY ( IR-CTX:ctx -- bool bool bool bool bool bool n n n n n n n )
   {: c:IR-CTX:ctx :}
   c 32 64 TAB-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   c key SYM-NEW {: sa:IR-ARENA:arena sr:IR-ARENA:arena :}
   c key TYP-NEW {: ta:IR-ARENA:arena tr:IR-ARENA:arena :}
   c sa sr key SYM-A {: s0:IR-ID:ir-symbol-id :}
   c ta tr key TY-I64 {: y0:IR-ID:ir-type-id :}
   c a r key -12 IR-ATTR:INT {: v0:IR-ID:ir-attr-id :}
   c a r key true IR-ATTR:BOOLEAN {: b0:IR-ID:ir-attr-id :}
   c a r key s" tile" IR-ATTR:TEXT {: x0:IR-ID:ir-attr-id :}
   c a r key sr s0 IR-ATTR:SYMBOL {: m0:IR-ID:ir-attr-id :}
   c a r key tr y0 IR-ATTR:TYPE-REF {: p0:IR-ID:ir-attr-id :}
   c a r key DG-A IR-ATTR:DIGEST {: d0:IR-ID:ir-attr-id :}
   c a r key CNUM-COMPARE:TOTAL-ORDER IR-ATTR:COMPARE {: e0:IR-ID:ir-attr-id :}
   IR-ATTR:IL-BEGIN 4 IR-ATTR:IL-ADD 5 IR-ATTR:IL-ADD
   c a r key IR-ATTR:INT-LIST {: l0:IR-ID:ir-attr-id :}
   IR-ATTR:REC-BEGIN s0 v0 IR-ATTR:REC-PAIR
   c a r key sr IR-ATTR:RECORD {: c0:IR-ID:ir-attr-id :}
   r v0 IR-ATTR:KIND@ IR--ATTR-KIND:INT IR--ATTR-KIND:EQ
   r b0 IR-ATTR:BOOLEAN@
   r e0 IR-ATTR:EFAM@ IR--ATTR-EFAM:COMPARE IR--ATTR-EFAM:EQ
   r e0 IR-ATTR:COMPARE@ CNUM-COMPARE:TOTAL-ORDER CNUM-COMPARE:EQ
   r d0 IR-ATTR:DIGEST@ DG-A CDIGEST-DIGEST:EQ
   a r key c0 0 IR-ATTR:KEY@ IR-ID:SYMBOL-LOCAL s0 IR-ID:SYMBOL-LOCAL =
   r v0 IR-ATTR:INT@
   r x0 IR-ATTR:TEXT-LEN@
   r key m0 IR-ATTR:SYM@ IR-ID:SYMBOL-LOCAL
   r key p0 IR-ATTR:TYPE@ IR-ID:TYPE-LOCAL
   r l0 IR-ATTR:ITEMS@
   a r l0 1 IR-ATTR:ITEM@
   r c0 IR-ATTR:PAIRS@ ;

: RD-CASE ( -- )
   s" readers project every kind's stored payload" T-LABEL
   BND [: RD-BODY ;] IR-CTX:WITH-CONTEXT
   1 T= 5 T= 2 T= 0 T= 0 T= 4 T= -12 T=
   TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE ;

\ Every enum reader projects its own family's member back out. The four
\ target attributes state exactly what this binding states, so they are
\ declarable here.
: LE-BODY ( IR-CTX:ctx -- bool bool bool bool bool bool bool bool )
   {: c:IR-CTX:ctx :}
   c 16 16 TAB-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   c a r key CNUM-OVERFLOW:WRAP IR-ATTR:OVERFLOW {: e0:IR-ID:ir-attr-id :}
   c a r key CNUM-FLOAT--MODEL:FLUSH-DENORMAL IR-ATTR:FLOAT-MODEL {: e1:IR-ID:ir-attr-id :}
   c a r key CNUM-CONTRACTION:ALLOWED IR-ATTR:CONTRACTION {: e2:IR-ID:ir-attr-id :}
   c a r key CNUM-FAST--MATH:BIT-EXACT IR-ATTR:FAST-MATH {: e3:IR-ID:ir-attr-id :}
   c a r key CTARGET-ARCH:AARCH64 IR-ATTR:ARCH {: e4:IR-ID:ir-attr-id :}
   c a r key CTARGET-ABI:AAPCS64-DARWIN IR-ATTR:ABI {: e5:IR-ID:ir-attr-id :}
   c a r key CTARGET-ENDIAN:LITTLE IR-ATTR:ENDIAN {: e6:IR-ID:ir-attr-id :}
   c a r key CTARGET-PTR--WIDTH:BITS64 IR-ATTR:PTR-WIDTH {: e7:IR-ID:ir-attr-id :}
   r e0 IR-ATTR:OVERFLOW@ CNUM-OVERFLOW:WRAP CNUM-OVERFLOW:EQ
   r e1 IR-ATTR:FLOAT-MODEL@ CNUM-FLOAT--MODEL:FLUSH-DENORMAL CNUM-FLOAT--MODEL:EQ
   r e2 IR-ATTR:CONTRACTION@ CNUM-CONTRACTION:ALLOWED CNUM-CONTRACTION:EQ
   r e3 IR-ATTR:FAST-MATH@ CNUM-FAST--MATH:BIT-EXACT CNUM-FAST--MATH:EQ
   r e4 IR-ATTR:ARCH@ CTARGET-ARCH:AARCH64 CTARGET-ARCH:EQ
   r e5 IR-ATTR:ABI@ CTARGET-ABI:AAPCS64-DARWIN CTARGET-ABI:EQ
   r e6 IR-ATTR:ENDIAN@ CTARGET-ENDIAN:LITTLE CTARGET-ENDIAN:EQ
   r e7 IR-ATTR:PTR-WIDTH@ CTARGET-PTR--WIDTH:BITS64 CTARGET-PTR--WIDTH:EQ ;

: LE-CASE ( -- )
   s" every enum reader projects its own family's member" T-LABEL
   BND [: LE-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE ;

\ A record's stored value reads back as the attribute it was built from.
: RV-BODY ( IR-CTX:ctx -- bool n )
   {: c:IR-CTX:ctx :}
   c 16 32 TAB-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   c key SYM-NEW {: sa:IR-ARENA:arena sr:IR-ARENA:arena :}
   c sa sr key SYM-A {: s0:IR-ID:ir-symbol-id :}
   c a r key 42 IR-ATTR:INT {: v0:IR-ID:ir-attr-id :}
   IR-ATTR:REC-BEGIN s0 v0 IR-ATTR:REC-PAIR
   c a r key sr IR-ATTR:RECORD {: c0:IR-ID:ir-attr-id :}
   a r key c0 0 IR-ATTR:VAL@ IR-ID:ATTR-LOCAL v0 IR-ID:ATTR-LOCAL =
   a r key c0 0 IR-ATTR:VAL@ {: got:IR-ID:ir-attr-id :}
   r got IR-ATTR:INT@ ;

: RV-CASE ( -- )
   s" a record value reads back as the attribute it was built from" T-LABEL
   BND [: RV-BODY ;] IR-CTX:WITH-CONTEXT
   42 T= TTRUE ;

: RDK-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 16 TAB-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   c a r key 1 IR-ATTR:INT {: v0:IR-ID:ir-attr-id :}
   r v0 IR-ATTR:BOOLEAN@ drop ;

: RDK-RUN ( -- )
   BND [: RDK-BODY ;] IR-CTX:WITH-CONTEXT ;

\ A reader of the right kind but the wrong enum family rejects too.
: RDF-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 16 TAB-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   c a r key CNUM-OVERFLOW:WRAP IR-ATTR:OVERFLOW {: e0:IR-ID:ir-attr-id :}
   r e0 IR-ATTR:FAST-MATH@ drop ;

: RDF-RUN ( -- )
   BND [: RDF-BODY ;] IR-CTX:WITH-CONTEXT ;

: RDB-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 16 TAB-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   IR-ATTR:IL-BEGIN 1 IR-ATTR:IL-ADD
   c a r key IR-ATTR:INT-LIST {: l0:IR-ID:ir-attr-id :}
   a r l0 1 IR-ATTR:ITEM@ drop ;

: RDB-RUN ( -- )
   BND [: RDB-BODY ;] IR-CTX:WITH-CONTEXT ;

: RDC-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 16 TAB-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   c a r key s" abcdef" IR-ATTR:TEXT {: x0:IR-ID:ir-attr-id :}
   a r x0 CBUF 3 IR-ATTR:TEXT-COPY drop ;

: RDC-RUN ( -- )
   BND [: RDC-BODY ;] IR-CTX:WITH-CONTEXT ;

: RDX-CASES ( -- )
   s" a wrong-kind reader rejects named" T-LABEL
   [: RDK-RUN ;] E-IR-ATTR-KIND TTHROWSQ
   s" a wrong-family enum reader rejects named" T-LABEL
   [: RDF-RUN ;] E-IR-ATTR-KIND TTHROWSQ
   s" an element index past the list rejects named" T-LABEL
   [: RDB-RUN ;] E-IR-ATTR-BOUND TTHROWSQ
   s" a copy span smaller than the text rejects named" T-LABEL
   [: RDC-RUN ;] E-IR-ATTR-RANGE TTHROWSQ ;

\ ---- text copy round trip ----------------------------------------------------
: CP-BODY ( IR-CTX:ctx -- bool n )
   {: c:IR-CTX:ctx :}
   c 8 16 TAB-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   c a r key s" unroll-4" IR-ATTR:TEXT {: x0:IR-ID:ir-attr-id :}
   a r x0 CBUF 256 IR-ATTR:TEXT-COPY {: n0:n :}
   CBUF n0 s" unroll-4" STR=
   n0 ;

: CP-CASE ( -- )
   s" a text attribute copies its exact bytes back out" T-LABEL
   BND [: CP-BODY ;] IR-CTX:WITH-CONTEXT
   8 T= TTRUE ;

\ ---- render fixtures pin identity --------------------------------------------
: REND= ( IR-ARENA:arena IR-ARENA:arena IR-ID:ir-attr-id ptr u8 n -- bool )
   {: a:IR-ARENA:arena r:IR-ARENA:arena id:IR-ID:ir-attr-id p u:n :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   a r id CBUF 256 IR-ATTR:RENDER {: n0:n :}
   CBUF n0 p u STR= ;

: RN-BODY ( IR-CTX:ctx -- bool bool bool bool bool bool bool bool bool )
   {: c:IR-CTX:ctx :}
   c 32 64 TAB-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   c key SYM-NEW {: sa:IR-ARENA:arena sr:IR-ARENA:arena :}
   c key TYP-NEW {: ta:IR-ARENA:arena tr:IR-ARENA:arena :}
   c sa sr key SYM-A drop
   c sa sr key SYM-B {: s1:IR-ID:ir-symbol-id :}
   c ta tr key TY-I64 drop
   c ta tr key TY-U8 {: y1:IR-ID:ir-type-id :}
   c a r key -5 IR-ATTR:INT {: v0:IR-ID:ir-attr-id :}
   c a r key 0 IR-ATTR:INT {: v1:IR-ID:ir-attr-id :}
   c a r key false IR-ATTR:BOOLEAN {: b0:IR-ID:ir-attr-id :}
   c a r key s" tile" IR-ATTR:TEXT {: x0:IR-ID:ir-attr-id :}
   c a r key sr s1 IR-ATTR:SYMBOL {: m0:IR-ID:ir-attr-id :}
   c a r key tr y1 IR-ATTR:TYPE-REF {: p0:IR-ID:ir-attr-id :}
   c a r key CNUM-FAST--MATH:REASSOCIATE IR-ATTR:FAST-MATH {: e0:IR-ID:ir-attr-id :}
   c a r key DG-A IR-ATTR:DIGEST {: d0:IR-ID:ir-attr-id :}
   IR-ATTR:IL-BEGIN 1 IR-ATTR:IL-ADD -2 IR-ATTR:IL-ADD 3 IR-ATTR:IL-ADD
   c a r key IR-ATTR:INT-LIST {: l0:IR-ID:ir-attr-id :}
   a r v0 s" int(-5)" REND=
   a r v1 s" int(0)" REND=
   a r b0 s" bool(false)" REND=
   a r x0 s\" \qtile\q" REND=
   a r m0 s" sym#1" REND=
   a r p0 s" type#1" REND=
   a r e0 s" fast-math:reassociate" REND=
   a r l0 s" ints(1 -2 3)" REND=
   a r d0
      s" digest(0000000000000001000000000000000200000000000000030000000000000004)"
      REND= ;

: RN-CASE ( -- )
   s" render fixtures pin each kind's deterministic spelling" T-LABEL
   BND [: RN-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE ;

: RNR-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 16 TAB-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   c a r key -5 IR-ATTR:INT {: v0:IR-ID:ir-attr-id :}
   a r v0 CBUF 3 IR-ATTR:RENDER drop ;

: RNR-RUN ( -- )
   BND [: RNR-BODY ;] IR-CTX:WITH-CONTEXT ;

: RNR-CASE ( -- )
   s" a render span smaller than the text rejects named" T-LABEL
   [: RNR-RUN ;] E-IR-ATTR-RANGE TTHROWSQ ;

\ A record renders its sorted pairs and recurses into nested values.
: RNN-BODY ( IR-CTX:ctx -- bool )
   {: c:IR-CTX:ctx :}
   c 32 64 TAB-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   c key SYM-NEW {: sa:IR-ARENA:arena sr:IR-ARENA:arena :}
   c sa sr key SYM-A {: s0:IR-ID:ir-symbol-id :}
   c sa sr key SYM-B {: s1:IR-ID:ir-symbol-id :}
   c a r key 1 IR-ATTR:INT {: v1:IR-ID:ir-attr-id :}
   c a r key true IR-ATTR:BOOLEAN {: b1:IR-ID:ir-attr-id :}
   IR-ATTR:REC-BEGIN s1 b1 IR-ATTR:REC-PAIR s0 v1 IR-ATTR:REC-PAIR
   c a r key sr IR-ATTR:RECORD {: r0:IR-ID:ir-attr-id :}
   IR-ATTR:REC-BEGIN s0 r0 IR-ATTR:REC-PAIR
   c a r key sr IR-ATTR:RECORD {: r1:IR-ID:ir-attr-id :}
   a r r1 s" rec(sym#0=rec(sym#0=int(1) sym#1=bool(true)))" REND= ;

: RNN-CASE ( -- )
   s" a nested record renders through the value recursion" T-LABEL
   BND [: RNN-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE ;

\ ---- non-table, misaligned, and forged rows ----------------------------------
: RAW-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 IR-ARENA:NEW IR-ATTR:ATTRS drop ;

: RAW-RUN ( -- )
   BND [: RAW-BODY ;] IR-CTX:WITH-CONTEXT ;

\ The two stores are not interchangeable: the pool presented as the row table
\ (and the reverse) is a format-tag reject, not a misread.
: SWAP-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 16 TAB-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   c a r key 1 IR-ATTR:INT {: v0:IR-ID:ir-attr-id :}
   r a v0 CBUF 256 IR-ATTR:RENDER drop ;

: SWAP-RUN ( -- )
   BND [: SWAP-BODY ;] IR-CTX:WITH-CONTEXT ;

: SHAPE-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 16 TAB-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   c a r key 1 IR-ATTR:INT drop
   c r 7 IR-ARENA:PUSH drop
   r IR-ATTR:ATTRS drop ;

: SHAPE-RUN ( -- )
   BND [: SHAPE-BODY ;] IR-CTX:WITH-CONTEXT ;

\ A bypass-appended row with an unknown kind code rejects at the decoder: the
\ closed world holds even against raw-cell forgery.
: KFORGE-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 16 TAB-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   c a r key 1 IR-ATTR:INT drop
   c r 99 IR-ARENA:PUSH drop
   c r 0 IR-ARENA:PUSH drop
   c r 0 IR-ARENA:PUSH drop
   c r 0 IR-ARENA:PUSH drop
   c r 0 IR-ARENA:PUSH drop
   key 1 IR-ID:PACK-ATTR {: forged:IR-ID:ir-attr-id :}
   r forged IR-ATTR:KIND@ drop ;

: KFORGE-RUN ( -- )
   BND [: KFORGE-BODY ;] IR-CTX:WITH-CONTEXT ;

\ The value-list kind's wire code is reserved, not implemented: a row forged
\ with it decodes as nothing.
: RSVFORGE-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 16 TAB-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   c a r key 1 IR-ATTR:INT drop
   c r 9 IR-ARENA:PUSH drop
   c r 0 IR-ARENA:PUSH drop
   c r 0 IR-ARENA:PUSH drop
   c r 0 IR-ARENA:PUSH drop
   c r 0 IR-ARENA:PUSH drop
   key 1 IR-ID:PACK-ATTR {: forged:IR-ID:ir-attr-id :}
   r forged IR-ATTR:KIND@ drop ;

: RSVFORGE-RUN ( -- )
   BND [: RSVFORGE-BODY ;] IR-CTX:WITH-CONTEXT ;

\ A forged boolean row whose payload cell is neither truth value.
: BFORGE-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 16 TAB-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   c a r key 1 IR-ATTR:INT drop
   c r 1 IR-ARENA:PUSH drop
   c r 4 IR-ARENA:PUSH drop
   c r 0 IR-ARENA:PUSH drop
   c r 0 IR-ARENA:PUSH drop
   c r 0 IR-ARENA:PUSH drop
   key 1 IR-ID:PACK-ATTR {: forged:IR-ID:ir-attr-id :}
   r forged IR-ATTR:BOOLEAN@ drop ;

: BFORGE-RUN ( -- )
   BND [: BFORGE-BODY ;] IR-CTX:WITH-CONTEXT ;

\ A forged enum row whose member code is outside its family's vocabulary.
: EFORGE-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 16 TAB-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   c a r key 1 IR-ATTR:INT drop
   c r 6 IR-ARENA:PUSH drop
   c r 0 IR-ARENA:PUSH drop
   c r 5 IR-ARENA:PUSH drop
   c r 0 IR-ARENA:PUSH drop
   c r 0 IR-ARENA:PUSH drop
   key 1 IR-ID:PACK-ATTR {: forged:IR-ID:ir-attr-id :}
   r forged IR-ATTR:EFAM@ drop ;

: EFORGE-RUN ( -- )
   BND [: EFORGE-BODY ;] IR-CTX:WITH-CONTEXT ;

\ A bypass-appended record row that references itself: the strict-decrease
\ recheck rejects before the renderer can loop.
: CFORGE-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 16 TAB-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   c a r key 1 IR-ATTR:INT drop
   c a 0 IR-ARENA:PUSH drop
   c a 1 IR-ARENA:PUSH drop
   c r 7 IR-ARENA:PUSH drop
   c r 0 IR-ARENA:PUSH drop
   c r 1 IR-ARENA:PUSH drop
   c r 0 IR-ARENA:PUSH drop
   c r 0 IR-ARENA:PUSH drop
   key 1 IR-ID:PACK-ATTR {: forged:IR-ID:ir-attr-id :}
   a r forged CBUF 256 IR-ATTR:RENDER drop ;

: CFORGE-RUN ( -- )
   BND [: CFORGE-BODY ;] IR-CTX:WITH-CONTEXT ;

\ A bypass-appended list row whose pool window crosses the live pool.
: WFORGE-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 16 TAB-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   c a r key 1 IR-ATTR:INT drop
   c r 5 IR-ARENA:PUSH drop
   c r 3 IR-ARENA:PUSH drop
   c r 9 IR-ARENA:PUSH drop
   c r 0 IR-ARENA:PUSH drop
   c r 0 IR-ARENA:PUSH drop
   key 1 IR-ID:PACK-ATTR {: forged:IR-ID:ir-attr-id :}
   a r forged 0 IR-ATTR:ITEM@ drop ;

: WFORGE-RUN ( -- )
   BND [: WFORGE-BODY ;] IR-CTX:WITH-CONTEXT ;

: STATE-CASES ( -- )
   s" a bare arena is not an attribute table" T-LABEL
   [: RAW-RUN ;] E-IR-ATTR-STATE TTHROWSQ
   s" a swapped store pairing rejects on the format tag" T-LABEL
   [: SWAP-RUN ;] E-IR-ATTR-STATE TTHROWSQ
   s" a misaligned row shape rejects fail-closed" T-LABEL
   [: SHAPE-RUN ;] E-IR-ATTR-STATE TTHROWSQ
   s" a forged unknown kind code rejects at the decoder" T-LABEL
   [: KFORGE-RUN ;] E-IR-ATTR-STATE TTHROWSQ
   s" the reserved value-list code decodes as nothing" T-LABEL
   [: RSVFORGE-RUN ;] E-IR-ATTR-STATE TTHROWSQ
   s" a forged boolean payload rejects at the decoder" T-LABEL
   [: BFORGE-RUN ;] E-IR-ATTR-STATE TTHROWSQ
   s" a forged out-of-vocabulary enum member rejects" T-LABEL
   [: EFORGE-RUN ;] E-IR-ATTR-STATE TTHROWSQ
   s" a forged self-referent record rejects on the strict decrease" T-LABEL
   [: CFORGE-RUN ;] E-IR-ATTR-STATE TTHROWSQ
   s" a forged pool window rejects fail-closed" T-LABEL
   [: WFORGE-RUN ;] E-IR-ATTR-STATE TTHROWSQ ;

\ ---- frozen modules own their attributes -------------------------------------
: FZ-BODY ( IR-CTX:ctx -- n bool bool bool n bool bool )
   {: c:IR-CTX:ctx :}
   c 32 64 TAB-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   c key SYM-NEW {: sa:IR-ARENA:arena sr:IR-ARENA:arena :}
   c sa sr key SYM-A {: s0:IR-ID:ir-symbol-id :}
   c a r key -5 IR-ATTR:INT {: v0:IR-ID:ir-attr-id :}
   c a r key s" tile" IR-ATTR:TEXT {: x0:IR-ID:ir-attr-id :}
   c a r key CNUM-OVERFLOW:TRAP IR-ATTR:OVERFLOW {: e0:IR-ID:ir-attr-id :}
   IR-ATTR:REC-BEGIN s0 v0 IR-ATTR:REC-PAIR
   c a r key sr IR-ATTR:RECORD {: c0:IR-ID:ir-attr-id :}
   a IR-ARENA:FREEZE {: pv:IR-ARENA:view :}
   r IR-ARENA:FREEZE {: rv:IR-ARENA:view :}
   rv IR-ATTR:FATTRS
   rv v0 IR-ATTR:FKIND@ IR--ATTR-KIND:INT IR--ATTR-KIND:EQ
   rv e0 IR-ATTR:FOVERFLOW@ CNUM-OVERFLOW:TRAP CNUM-OVERFLOW:EQ
   pv rv key c0 0 IR-ATTR:FVAL@ IR-ID:ATTR-LOCAL v0 IR-ID:ATTR-LOCAL =
   rv v0 IR-ATTR:FINT@
   pv rv x0 CBUF 256 IR-ATTR:FTEXT-COPY {: n0:n :}
   CBUF n0 s" tile" STR=
   pv rv c0 CBUF 256 IR-ATTR:FRENDER {: n1:n :}
   CBUF n1 s" rec(sym#0=int(-5))" STR= ;

: FZ-CASE ( -- )
   s" a frozen module serves every reader through the views" T-LABEL
   BND [: FZ-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE -5 T= TTRUE TTRUE TTRUE 4 T= ;

\ Every frozen payload reader answers through the views.
: FZ2-BODY ( IR-CTX:ctx -- bool bool n n n n n n n )
   {: c:IR-CTX:ctx :}
   c 32 64 TAB-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   c key SYM-NEW {: sa:IR-ARENA:arena sr:IR-ARENA:arena :}
   c key TYP-NEW {: ta:IR-ARENA:arena tr:IR-ARENA:arena :}
   c sa sr key SYM-A {: s0:IR-ID:ir-symbol-id :}
   c ta tr key TY-I64 {: y0:IR-ID:ir-type-id :}
   c a r key s" tile" IR-ATTR:TEXT {: x0:IR-ID:ir-attr-id :}
   c a r key sr s0 IR-ATTR:SYMBOL {: m0:IR-ID:ir-attr-id :}
   c a r key tr y0 IR-ATTR:TYPE-REF {: p0:IR-ID:ir-attr-id :}
   c a r key DG-A IR-ATTR:DIGEST {: d0:IR-ID:ir-attr-id :}
   c a r key false IR-ATTR:BOOLEAN {: b0:IR-ID:ir-attr-id :}
   IR-ATTR:IL-BEGIN 4 IR-ATTR:IL-ADD 5 IR-ATTR:IL-ADD
   c a r key IR-ATTR:INT-LIST {: l0:IR-ID:ir-attr-id :}
   c a r key 9 IR-ATTR:INT {: v0:IR-ID:ir-attr-id :}
   IR-ATTR:REC-BEGIN s0 v0 IR-ATTR:REC-PAIR
   c a r key sr IR-ATTR:RECORD {: c0:IR-ID:ir-attr-id :}
   a IR-ARENA:FREEZE {: pv:IR-ARENA:view :}
   r IR-ARENA:FREEZE {: rv:IR-ARENA:view :}
   rv b0 IR-ATTR:FBOOLEAN@
   rv d0 IR-ATTR:FDIGEST@ DG-A CDIGEST-DIGEST:EQ
   rv x0 IR-ATTR:FTEXT-LEN@
   rv key m0 IR-ATTR:FSYM@ IR-ID:SYMBOL-LOCAL
   rv key p0 IR-ATTR:FTYPE@ IR-ID:TYPE-LOCAL
   rv l0 IR-ATTR:FITEMS@
   pv rv l0 1 IR-ATTR:FITEM@
   rv c0 IR-ATTR:FPAIRS@
   pv rv key c0 0 IR-ATTR:FKEY@ IR-ID:SYMBOL-LOCAL ;

: FZ2-CASE ( -- )
   s" every frozen payload reader answers through the views" T-LABEL
   BND [: FZ2-BODY ;] IR-CTX:WITH-CONTEXT
   0 T= 1 T= 5 T= 2 T= 0 T= 0 T= 4 T= TTRUE TFALSE ;

\ Every frozen enum reader projects its own family's member.
: FZ3-BODY ( IR-CTX:ctx -- bool bool bool bool bool bool bool bool bool bool )
   {: c:IR-CTX:ctx :}
   c 16 16 TAB-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   c a r key CNUM-OVERFLOW:WRAP IR-ATTR:OVERFLOW {: e0:IR-ID:ir-attr-id :}
   c a r key CNUM-FLOAT--MODEL:IEEE754 IR-ATTR:FLOAT-MODEL {: e1:IR-ID:ir-attr-id :}
   c a r key CNUM-CONTRACTION:FORBIDDEN IR-ATTR:CONTRACTION {: e2:IR-ID:ir-attr-id :}
   c a r key CNUM-FAST--MATH:APPROXIMATE IR-ATTR:FAST-MATH {: e3:IR-ID:ir-attr-id :}
   c a r key CNUM-COMPARE:TOTAL-ORDER IR-ATTR:COMPARE {: e4:IR-ID:ir-attr-id :}
   c a r key CTARGET-ARCH:AARCH64 IR-ATTR:ARCH {: e5:IR-ID:ir-attr-id :}
   c a r key CTARGET-ABI:AAPCS64-DARWIN IR-ATTR:ABI {: e6:IR-ID:ir-attr-id :}
   c a r key CTARGET-ENDIAN:LITTLE IR-ATTR:ENDIAN {: e7:IR-ID:ir-attr-id :}
   c a r key CTARGET-PTR--WIDTH:BITS64 IR-ATTR:PTR-WIDTH {: e8:IR-ID:ir-attr-id :}
   r IR-ARENA:FREEZE {: rv:IR-ARENA:view :}
   rv e0 IR-ATTR:FOVERFLOW@ CNUM-OVERFLOW:WRAP CNUM-OVERFLOW:EQ
   rv e1 IR-ATTR:FFLOAT-MODEL@ CNUM-FLOAT--MODEL:IEEE754 CNUM-FLOAT--MODEL:EQ
   rv e2 IR-ATTR:FCONTRACTION@ CNUM-CONTRACTION:FORBIDDEN CNUM-CONTRACTION:EQ
   rv e3 IR-ATTR:FFAST-MATH@ CNUM-FAST--MATH:APPROXIMATE CNUM-FAST--MATH:EQ
   rv e4 IR-ATTR:FCOMPARE@ CNUM-COMPARE:TOTAL-ORDER CNUM-COMPARE:EQ
   rv e5 IR-ATTR:FARCH@ CTARGET-ARCH:AARCH64 CTARGET-ARCH:EQ
   rv e6 IR-ATTR:FABI@ CTARGET-ABI:AAPCS64-DARWIN CTARGET-ABI:EQ
   rv e7 IR-ATTR:FENDIAN@ CTARGET-ENDIAN:LITTLE CTARGET-ENDIAN:EQ
   rv e8 IR-ATTR:FPTR-WIDTH@ CTARGET-PTR--WIDTH:BITS64 CTARGET-PTR--WIDTH:EQ
   rv e4 IR-ATTR:FEFAM@ IR--ATTR-EFAM:COMPARE IR--ATTR-EFAM:EQ ;

: FZ3-CASE ( -- )
   s" every frozen enum reader projects its own family's member" T-LABEL
   BND [: FZ3-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE ;

: FZ-PUSH-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 16 TAB-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   c a r key 1 IR-ATTR:INT drop
   a IR-ARENA:FREEZE drop
   r IR-ARENA:FREEZE drop
   c a r key 2 IR-ATTR:INT drop ;

: FZ-PUSH ( -- )
   BND [: FZ-PUSH-BODY ;] IR-CTX:WITH-CONTEXT ;

: FZ-READ-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 16 TAB-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   c a r key 1 IR-ATTR:INT {: v0:IR-ID:ir-attr-id :}
   r IR-ARENA:FREEZE drop
   r v0 IR-ATTR:KIND@ drop ;

: FZ-READ ( -- )
   BND [: FZ-READ-BODY ;] IR-CTX:WITH-CONTEXT ;

: FZ-REJECT-CASES ( -- )
   s" constructing through retired frozen handles rejects" T-LABEL
   [: FZ-PUSH ;] E-IR-ARENA-FROZEN TTHROWSQ
   s" live readers reject the retired builder handle; the views read" T-LABEL
   [: FZ-READ ;] E-IR-ARENA-FROZEN TTHROWSQ ;

\ ---- teardown releases everything --------------------------------------------
: TD-ESC-BODY ( IR-CTX:ctx -- IR-ARENA:arena IR-ID:ir-attr-id )
   {: c:IR-CTX:ctx :}
   c 8 16 TAB-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   r c a r key 1 IR-ATTR:INT ;

: TD-READ ( -- )
   BND [: TD-ESC-BODY ;] IR-CTX:WITH-CONTEXT
   IR-ATTR:KIND@ drop ;

: TD-STALE-CASE ( -- )
   s" an attribute table is dead after its context ends" T-LABEL
   [: TD-READ ;] E-IR-ARENA-STALE TTHROWSQ ;

: TD-FRESH-CASE ( -- )
   s" fresh contexts and tables succeed after teardown" T-LABEL
   4 0 ?do
      BND [: IN-BODY ;] IR-CTX:WITH-CONTEXT
      TTRUE 3 T= TTRUE 0 T= 1 T= 0 T=
   loop ;

\ ---- the checker keeps identities and the API sealed -------------------------
: CHECKER-CASES ( -- )
   \ positive control: a well-typed candidate over the same surface certifies,
   \ so the rejections below fail for their stated reason, not a harness typo
   s" IRA-POS ( IR-ARENA:arena -- n ) IR-ATTR:ATTRS"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" IRA-ID-FORGE ( n -- IR-ID:ir-attr-id )"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" IRA-CTXLESS ( IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key n -- IR-ID:ir-attr-id ) IR-ATTR:INT"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" IRA-KEYLESS ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ID:ir-symbol-id -- IR-ID:ir-attr-id ) IR-ATTR:SYMBOL"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" IRA-RAW-PAIR ( n n -- ) IR-ATTR:REC-PAIR"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" IRA-RAW-ENUM ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key n -- IR-ID:ir-attr-id ) IR-ATTR:FAST-MATH"
      CHECK-QUIET-CANDIDATE! 0 T= ;

\ ---- run ---------------------------------------------------------------------
\ Throw-through fixtures run inside an outermost harness context, so a context
\ aborted by a throw is reclaimed by that harness exit instead of lingering
\ for the rest of the process. A context abandoned by a throw keeps its
\ registry slot - and therefore its arenas' slots - until the enclosing live
\ context leaves normally, so the rejecting groups are split across several
\ harness contexts and each group's leaked slots are reclaimed before the next
\ group starts. The teardown-reuse loop runs after those exits, on registries
\ the next sweep can fully reclaim.
: HARNESS-VALUES ( IR-CTX:ctx -- )
   drop
   IN-CASE
   TX-CASE
   RF-CASE
   BL-CASE
   DG-CASE
   IL-CASE
   EN-CASE
   RC-CASE
   RCK-CASE
   ORD-CASE
   RD-CASE
   LE-CASE
   RV-CASE
   CP-CASE
   RN-CASE
   RNN-CASE
   FZ-CASE
   FZ2-CASE
   FZ3-CASE ;

: HARNESS-PAYLOAD ( IR-CTX:ctx -- )
   drop
   PAYLOAD-CASES
   STG-CASES
   CAP-CASES ;

: HARNESS-OWNER ( IR-CTX:ctx -- )
   drop
   OWNER-CASES
   TGT-CASES ;

: HARNESS-STATE ( IR-CTX:ctx -- )
   drop
   RDX-CASES
   RNR-CASE
   STATE-CASES
   FZ-REJECT-CASES
   TD-STALE-CASE ;

public

: RUN ( -- )
   T-RESET
   BND [: HARNESS-VALUES ;] IR-CTX:WITH-CONTEXT
   BND [: HARNESS-PAYLOAD ;] IR-CTX:WITH-CONTEXT
   BND [: HARNESS-OWNER ;] IR-CTX:WITH-CONTEXT
   BND [: HARNESS-STATE ;] IR-CTX:WITH-CONTEXT
   TD-FRESH-CASE
   CHECKER-CASES
   T-REPORT ;

;package

IR-ATTR-TEST:RUN
