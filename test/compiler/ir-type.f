\ ir-type.f - checked compiler type-table tests.
\
\ Proves the IR-0.4 type-interning contract of src/compiler/ir/type.f:
\ identical types intern to one identity across constructors and construction
\ orders while distinct field sets mint distinct module-local identities;
\ function-type list equality holds cell for cell (same lists intern, a
\ different arity, element, or parameter/result split differs, and quotation
\ versus code-reference kinds stay apart); the one undefined scalar
\ combination rejects named; float formats and address spaces the bound
\ target contract cannot represent reject at construction on the context's
\ binding; type references validate against the same table so forward
\ references, foreign-module references, and cross-context identities reject
\ and cycles are impossible by construction; the staged function-type
\ protocol fails closed on misuse and consumes a rejected stage; row and
\ list-pool capacity reject named at the committed ceilings while the stores
\ stay readable and duplicates still answer; readers project every
\ constructor and reject wrong-kind reads; the render fixtures pin the
\ deterministic spelling of every constructor independent of interning
\ history; bypass-forged rows reject fail-closed; a frozen module serves
\ every reader through the arena views while the retired builders reject;
\ context teardown releases everything; and checker fixtures prove the
\ identity family and the API stay sealed.

require lib/test.f
require lib/string.f
require test/checker-assert.f
require test/compiler/ir-starve.f
require src/compiler/ir/type.f

package IR-TYPE-TEST
private

create CBUF 64 allot

\ ---- fixtures ----------------------------------------------------------------
\ An AArch64 contract with baseline and plain floating point only: no
\ half-precision formats, one flat address space.
: BND ( -- CBIND:binding )
   CTARGET-ARCH:AARCH64 CTARGET-ABI:AAPCS64-DARWIN CTARGET-ENDIAN:LITTLE
   CTARGET-PTR--WIDTH:BITS64
   CTARGET:F-BASE CTARGET:F-FP CTARGET:WITH CTARGET:CONTRACT
   CNUM-OVERFLOW:TRAP CNUM-FLOAT--MODEL:IEEE754 CNUM-CONTRACTION:FORBIDDEN
   CNUM-FAST--MATH:BIT-EXACT CNUM-COMPARE:IEEE754-UNORDERED CNUM:POLICY
   CBIND:BIND ;

\ A PTX contract with the half-precision formats and the full address-space
\ model.
: PBND ( -- CBIND:binding )
   CTARGET-ARCH:PTX CTARGET-ABI:PTX-KERNEL CTARGET-ENDIAN:LITTLE
   CTARGET-PTR--WIDTH:BITS64
   CTARGET:F-BASE CTARGET:F-FP CTARGET:WITH
   CTARGET:F-FP16 CTARGET:WITH CTARGET:F-BF16 CTARGET:WITH CTARGET:CONTRACT
   CNUM-OVERFLOW:TRAP CNUM-FLOAT--MODEL:IEEE754 CNUM-CONTRACTION:FORBIDDEN
   CNUM-FAST--MATH:BIT-EXACT CNUM-COMPARE:IEEE754-UNORDERED CNUM:POLICY
   CBIND:BIND ;

\ A fresh (key, pool, rows) type table for tcap types and lcap list cells.
: TAB-NEW ( IR-CTX:ctx n n -- IR-ID:ir-module-key IR-ARENA:arena IR-ARENA:arena )
   {: c:IR-CTX:ctx tcap:n lcap:n :}
   c IR-CTX:NEW-MODULE drop {: key:IR-ID:ir-module-key :}
   key c key tcap lcap IR-TYPE:NEW ;

: I64 ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key -- IR-ID:ir-type-id )
   IR--TYPE-WIDTH:W64 IR--TYPE-SIGN:SIGNED IR-TYPE:INT ;

: I32 ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key -- IR-ID:ir-type-id )
   IR--TYPE-WIDTH:W32 IR--TYPE-SIGN:SIGNED IR-TYPE:INT ;

: U8 ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key -- IR-ID:ir-type-id )
   IR--TYPE-WIDTH:W8 IR--TYPE-SIGN:UNSIGNED IR-TYPE:INT ;

\ ---- identical fields intern to one identity ---------------------------------
: IN-BODY ( IR-CTX:ctx -- n n n bool n bool )
   {: c:IR-CTX:ctx :}
   c IR-CTX:NEW-MODULE {: key:IR-ID:ir-module-key mid:IR-ID:ir-module-id :}
   c key 16 16 IR-TYPE:NEW {: a:IR-ARENA:arena r:IR-ARENA:arena :}
   c a r key I32 {: t0:IR-ID:ir-type-id :}
   c a r key IR--TYPE-WIDTH:W32 IR--TYPE-SIGN:UNSIGNED IR-TYPE:INT {: t1:IR-ID:ir-type-id :}
   c a r key I32 {: t2:IR-ID:ir-type-id :}
   c a r key IR--TYPE-FMT:SINGLE IR-TYPE:FLT {: t3:IR-ID:ir-type-id :}
   c a r key IR--TYPE-FMT:SINGLE IR-TYPE:FLT {: t4:IR-ID:ir-type-id :}
   t0 IR-ID:TYPE-LOCAL
   t1 IR-ID:TYPE-LOCAL
   t2 IR-ID:TYPE-LOCAL
   t3 IR-ID:TYPE-LOCAL t4 IR-ID:TYPE-LOCAL =
   r IR-TYPE:TYPES
   t0 IR-ID:TYPE-OWNER mid IR-ID:MODULE-SAME? ;

: IN-CASE ( -- )
   s" identical fields answer one id; a changed field mints a new one" T-LABEL
   BND [: IN-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE 3 T= TTRUE 0 T= 1 T= 0 T= ;

\ ---- pointer interning across constructors -----------------------------------
: PT-BODY ( IR-CTX:ctx -- n bool n n )
   {: c:IR-CTX:ctx :}
   c 16 16 TAB-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   c a r key I64 {: ti:IR-ID:ir-type-id :}
   c a r key IR--TYPE-SPACE:GENERIC ti IR-TYPE:POINTER {: p0:IR-ID:ir-type-id :}
   c a r key IR--TYPE-SPACE:GENERIC ti IR-TYPE:POINTER {: p1:IR-ID:ir-type-id :}
   c a r key I32 {: t32:IR-ID:ir-type-id :}
   c a r key IR--TYPE-SPACE:GENERIC t32 IR-TYPE:POINTER {: p2:IR-ID:ir-type-id :}
   p0 IR-ID:TYPE-LOCAL
   p0 IR-ID:TYPE-LOCAL p1 IR-ID:TYPE-LOCAL =
   p2 IR-ID:TYPE-LOCAL
   r IR-TYPE:TYPES ;

: PT-CASE ( -- )
   s" a pointer built twice over an interned pointee is one id" T-LABEL
   BND [: PT-BODY ;] IR-CTX:WITH-CONTEXT
   4 T= 3 T= TTRUE 1 T= ;

\ ---- function-type list equality ---------------------------------------------
: FN-BODY ( IR-CTX:ctx -- bool n n n n n n )
   {: c:IR-CTX:ctx :}
   c 16 32 TAB-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   c a r key I64 {: ti:IR-ID:ir-type-id :}
   c a r key I32 {: t32:IR-ID:ir-type-id :}
   IR-TYPE:FN-BEGIN ti IR-TYPE:FN-PARAM ti IR-TYPE:FN-PARAM ti IR-TYPE:FN-RESULT
   c a r key IR-TYPE:QUOT {: f0:IR-ID:ir-type-id :}
   IR-TYPE:FN-BEGIN ti IR-TYPE:FN-PARAM ti IR-TYPE:FN-PARAM ti IR-TYPE:FN-RESULT
   c a r key IR-TYPE:QUOT {: f1:IR-ID:ir-type-id :}
   IR-TYPE:FN-BEGIN ti IR-TYPE:FN-PARAM ti IR-TYPE:FN-RESULT
   c a r key IR-TYPE:QUOT {: f2:IR-ID:ir-type-id :}
   IR-TYPE:FN-BEGIN ti IR-TYPE:FN-PARAM t32 IR-TYPE:FN-PARAM ti IR-TYPE:FN-RESULT
   c a r key IR-TYPE:QUOT {: f3:IR-ID:ir-type-id :}
   IR-TYPE:FN-BEGIN ti IR-TYPE:FN-PARAM ti IR-TYPE:FN-RESULT ti IR-TYPE:FN-RESULT
   c a r key IR-TYPE:QUOT {: f4:IR-ID:ir-type-id :}
   IR-TYPE:FN-BEGIN ti IR-TYPE:FN-PARAM ti IR-TYPE:FN-PARAM ti IR-TYPE:FN-RESULT
   c a r key IR-TYPE:CODE-REF {: f5:IR-ID:ir-type-id :}
   f0 IR-ID:TYPE-LOCAL f1 IR-ID:TYPE-LOCAL =
   f0 IR-ID:TYPE-LOCAL
   f2 IR-ID:TYPE-LOCAL
   f3 IR-ID:TYPE-LOCAL
   f4 IR-ID:TYPE-LOCAL
   f5 IR-ID:TYPE-LOCAL
   r IR-TYPE:TYPES ;

: FN-CASE ( -- )
   s" same lists intern; arity, element, split, and kind all differ" T-LABEL
   BND [: FN-BODY ;] IR-CTX:WITH-CONTEXT
   7 T= 6 T= 5 T= 4 T= 3 T= 2 T= TTRUE ;

\ ---- render is independent of interning history ------------------------------
: ORD-A-BODY ( IR-CTX:ctx -- bool )
   {: c:IR-CTX:ctx :}
   c 16 16 TAB-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   c a r key I32 {: t:IR-ID:ir-type-id :}
   c a r key IR--TYPE-SPACE:GENERIC t IR-TYPE:POINTER {: p:IR-ID:ir-type-id :}
   a r p CBUF 64 IR-TYPE:RENDER {: n0:n :}
   CBUF n0 s" ptr<generic,i32>" STR= ;

: ORD-B-BODY ( IR-CTX:ctx -- bool )
   {: c:IR-CTX:ctx :}
   c 16 16 TAB-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   c a r key IR-TYPE:OPAQUE drop
   c a r key IR--TYPE-FMT:SINGLE IR-TYPE:FLT drop
   c a r key U8 drop
   c a r key I32 {: t:IR-ID:ir-type-id :}
   c a r key IR--TYPE-SPACE:GENERIC t IR-TYPE:POINTER {: p:IR-ID:ir-type-id :}
   a r p CBUF 64 IR-TYPE:RENDER {: n0:n :}
   CBUF n0 s" ptr<generic,i32>" STR= ;

: ORD-CASE ( -- )
   s" the same structure renders identically under different histories" T-LABEL
   BND [: ORD-A-BODY ;] IR-CTX:WITH-CONTEXT TTRUE
   BND [: ORD-B-BODY ;] IR-CTX:WITH-CONTEXT TTRUE ;

\ ---- the undefined scalar combination ----------------------------------------
: SC-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 8 TAB-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   c a r key IR--TYPE-WIDTH:W1 IR--TYPE-SIGN:SIGNED IR-TYPE:INT drop ;

: SC-RUN ( -- )
   BND [: SC-BODY ;] IR-CTX:WITH-CONTEXT ;

: SC-CASE ( -- )
   s" a signed one-bit integer is the rejected scalar combination" T-LABEL
   [: SC-RUN ;] E-IR-TYPE-SCALAR TTHROWSQ ;

\ ---- target legality through the context binding -----------------------------
: TGT-F16-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 8 TAB-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   c a r key IR--TYPE-FMT:HALF IR-TYPE:FLT drop ;

: TGT-F16 ( -- )
   BND [: TGT-F16-BODY ;] IR-CTX:WITH-CONTEXT ;

: TGT-BF16-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 8 TAB-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   c a r key IR--TYPE-FMT:BFLOAT IR-TYPE:FLT drop ;

: TGT-BF16 ( -- )
   BND [: TGT-BF16-BODY ;] IR-CTX:WITH-CONTEXT ;

: TGT-SPACE-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 8 TAB-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   c a r key I64 {: ti:IR-ID:ir-type-id :}
   c a r key IR--TYPE-SPACE:SHARED ti IR-TYPE:POINTER drop ;

: TGT-SPACE ( -- )
   BND [: TGT-SPACE-BODY ;] IR-CTX:WITH-CONTEXT ;

\ The same combinations are declarable on a PTX contract, and the PTX table
\ renders them; legality is the binding's fact, not a global's.
: TGT-PTX-BODY ( IR-CTX:ctx -- bool n bool )
   {: c:IR-CTX:ctx :}
   c 16 16 TAB-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   c a r key IR--TYPE-FMT:HALF IR-TYPE:FLT {: th:IR-ID:ir-type-id :}
   c a r key IR--TYPE-FMT:BFLOAT IR-TYPE:FLT drop
   c a r key IR--TYPE-SPACE:SHARED th IR-TYPE:POINTER {: p0:IR-ID:ir-type-id :}
   c a r key IR--TYPE-SPACE:CONST th IR-TYPE:POINTER drop
   a r p0 CBUF 64 IR-TYPE:RENDER {: n0:n :}
   CBUF n0 s" ptr<shared,f16>" STR=
   r IR-TYPE:TYPES
   c a r key IR--TYPE-SPACE:SHARED th IR-TYPE:POINTER IR-ID:TYPE-LOCAL
   p0 IR-ID:TYPE-LOCAL = ;

: TGT-CASES ( -- )
   s" a half-precision float rejects on a target without the feature" T-LABEL
   [: TGT-F16 ;] E-IR-TYPE-TARGET TTHROWSQ
   s" a bfloat16 rejects on a target without the feature" T-LABEL
   [: TGT-BF16 ;] E-IR-TYPE-TARGET TTHROWSQ
   s" a shared-space pointer rejects on the flat AArch64 model" T-LABEL
   [: TGT-SPACE ;] E-IR-TYPE-TARGET TTHROWSQ
   s" the same types are declarable and render on a PTX binding" T-LABEL
   PBND [: TGT-PTX-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE 4 T= TTRUE ;

\ ---- reference form: same table, no forward references, no cycles ------------
: FWD-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 8 TAB-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   c a r key I64 drop
   key r IR-TYPE:TYPES IR-ID:PACK-TYPE {: ghost:IR-ID:ir-type-id :}
   c a r key IR--TYPE-SPACE:GENERIC ghost IR-TYPE:POINTER drop ;

: FWD-RUN ( -- )
   BND [: FWD-BODY ;] IR-CTX:WITH-CONTEXT ;

: XM-PTEE-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 8 TAB-NEW {: keya:IR-ID:ir-module-key a:IR-ARENA:arena ra:IR-ARENA:arena :}
   c 8 8 TAB-NEW {: keyb:IR-ID:ir-module-key b:IR-ARENA:arena rb:IR-ARENA:arena :}
   c a ra keya I64 {: ta:IR-ID:ir-type-id :}
   c b rb keyb IR--TYPE-SPACE:GENERIC ta IR-TYPE:POINTER drop ;

: XM-PTEE ( -- )
   BND [: XM-PTEE-BODY ;] IR-CTX:WITH-CONTEXT ;

: XM-KEY-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 8 TAB-NEW {: keya:IR-ID:ir-module-key a:IR-ARENA:arena ra:IR-ARENA:arena :}
   c 8 8 TAB-NEW {: keyb:IR-ID:ir-module-key b:IR-ARENA:arena rb:IR-ARENA:arena :}
   c a ra keyb I64 drop ;

: XM-KEY ( -- )
   BND [: XM-KEY-BODY ;] IR-CTX:WITH-CONTEXT ;

\ A type-id minted inside context A rejects as a reference in context B.
: XC-INNER ( IR-ID:ir-type-id IR-CTX:ctx -- )
   {: id:IR-ID:ir-type-id c2:IR-CTX:ctx :}
   c2 8 8 TAB-NEW {: k2:IR-ID:ir-module-key b:IR-ARENA:arena rb:IR-ARENA:arena :}
   c2 b rb k2 IR--TYPE-SPACE:GENERIC id IR-TYPE:POINTER drop ;

: XC-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 8 TAB-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   c a r key I64
   BND [: XC-INNER ;] IR-CTX:WITH-CONTEXT ;

: XC-RUN ( -- )
   BND [: XC-BODY ;] IR-CTX:WITH-CONTEXT ;

: STG-XM-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 8 TAB-NEW {: keya:IR-ID:ir-module-key a:IR-ARENA:arena ra:IR-ARENA:arena :}
   c 8 8 TAB-NEW {: keyb:IR-ID:ir-module-key b:IR-ARENA:arena rb:IR-ARENA:arena :}
   c a ra keya I64 {: ta:IR-ID:ir-type-id :}
   IR-TYPE:FN-BEGIN
   ta IR-TYPE:FN-PARAM
   c b rb keyb IR-TYPE:QUOT drop ;

: STG-XM ( -- )
   BND [: STG-XM-BODY ;] IR-CTX:WITH-CONTEXT ;

: STG-FWD-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 8 TAB-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   c a r key I64 drop
   key r IR-TYPE:TYPES IR-ID:PACK-TYPE {: ghost:IR-ID:ir-type-id :}
   IR-TYPE:FN-BEGIN
   ghost IR-TYPE:FN-RESULT
   c a r key IR-TYPE:CODE-REF drop ;

: STG-FWD ( -- )
   BND [: STG-FWD-BODY ;] IR-CTX:WITH-CONTEXT ;

: REF-CASES ( -- )
   s" a not-yet-constructed ordinal rejects as a pointee: no forward refs" T-LABEL
   [: FWD-RUN ;] E-IR-TYPE-BOUND TTHROWSQ
   s" a foreign module's type rejects as a pointee" T-LABEL
   [: XM-PTEE ;] E-IR-TYPE-OWNER TTHROWSQ
   s" a foreign module key rejects at construction" T-LABEL
   [: XM-KEY ;] E-IR-TYPE-OWNER TTHROWSQ
   s" a type-id from context A rejects in context B's table" T-LABEL
   [: XC-RUN ;] E-IR-TYPE-OWNER TTHROWSQ
   s" a staged element from a foreign module rejects at the end" T-LABEL
   [: STG-XM ;] E-IR-TYPE-OWNER TTHROWSQ
   s" a staged forward reference rejects at the end" T-LABEL
   [: STG-FWD ;] E-IR-TYPE-BOUND TTHROWSQ ;

\ ---- stage protocol ----------------------------------------------------------
: STG-END-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 8 TAB-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   c a r key IR-TYPE:QUOT drop ;

: STG-END ( -- )
   BND [: STG-END-BODY ;] IR-CTX:WITH-CONTEXT ;

\ A second begin while one is open rejects; the open stage is then consumed
\ so later fixtures start clean.
: STG-DBL-BODY ( IR-CTX:ctx -- n n )
   {: c:IR-CTX:ctx :}
   c 8 8 TAB-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   IR-TYPE:FN-BEGIN
   [: IR-TYPE:FN-BEGIN ;] catch
   c a r key IR-TYPE:QUOT IR-ID:TYPE-LOCAL ;

\ The thirty-third staged parameter rejects; the open stage still ends.
: STG-OVER-BODY ( IR-CTX:ctx -- n n )
   {: c:IR-CTX:ctx :}
   c 8 64 TAB-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   c a r key I64 {: ti:IR-ID:ir-type-id :}
   IR-TYPE:FN-BEGIN
   32 0 ?do
      ti IR-TYPE:FN-PARAM
   loop
   ti [: dup IR-TYPE:FN-PARAM ;] catch
   {: ti2:IR-ID:ir-type-id rc:n :}
   rc
   c a r key IR-TYPE:QUOT IR-ID:TYPE-LOCAL ;

\ The caught end re-pushes its inputs before the throwing close, so the
\ quotation stays stack-preserving and the quadruple survives the reject.
: STG-END4 ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key -- IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key )
   {: c:IR-CTX:ctx b:IR-ARENA:arena rb:IR-ARENA:arena key:IR-ID:ir-module-key :}
   c b rb key
   c b rb key IR-TYPE:QUOT drop ;

\ A rejected end consumes the stage: the next end has nothing to close.
: STG-CONSUMED-BODY ( IR-CTX:ctx -- n n )
   {: c:IR-CTX:ctx :}
   c 8 8 TAB-NEW {: keya:IR-ID:ir-module-key a:IR-ARENA:arena ra:IR-ARENA:arena :}
   c 8 8 TAB-NEW {: keyb:IR-ID:ir-module-key b:IR-ARENA:arena rb:IR-ARENA:arena :}
   c a ra keya I64 {: ta:IR-ID:ir-type-id :}
   IR-TYPE:FN-BEGIN
   ta IR-TYPE:FN-PARAM
   c b rb keyb [: STG-END4 ;] catch
   {: c2:IR-CTX:ctx b2:IR-ARENA:arena rb2:IR-ARENA:arena k2:IR-ID:ir-module-key rc:n :}
   rc
   c2 b2 rb2 k2 [: STG-END4 ;] catch
   {: c3:IR-CTX:ctx b3:IR-ARENA:arena rb3:IR-ARENA:arena k3:IR-ID:ir-module-key rc2:n :}
   rc2 ;

: STG-CASES ( -- )
   s" an end without an open stage rejects" T-LABEL
   [: STG-END ;] E-IR-TYPE-STAGE TTHROWSQ
   s" a begin while a stage is open rejects; the stage still ends" T-LABEL
   BND [: STG-DBL-BODY ;] IR-CTX:WITH-CONTEXT
   0 T= E-IR-TYPE-STAGE T=
   s" a staged list past the arity ceiling rejects; the stage still ends" T-LABEL
   BND [: STG-OVER-BODY ;] IR-CTX:WITH-CONTEXT
   1 T= E-IR-TYPE-ARITY T=
   s" a rejected end consumes the stage" T-LABEL
   BND [: STG-CONSUMED-BODY ;] IR-CTX:WITH-CONTEXT
   E-IR-TYPE-STAGE T= E-IR-TYPE-OWNER T= ;

\ ---- capacity ----------------------------------------------------------------
: CAP-ZERO-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 0 8 TAB-NEW IR-ARENA:ABORT IR-ARENA:ABORT drop ;

: CAP-ZERO ( -- )
   BND [: CAP-ZERO-BODY ;] IR-CTX:WITH-CONTEXT ;

: CAP-NEG-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c -2 8 TAB-NEW IR-ARENA:ABORT IR-ARENA:ABORT drop ;

: CAP-NEG ( -- )
   BND [: CAP-NEG-BODY ;] IR-CTX:WITH-CONTEXT ;

: CAP-HUGE-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c $60000000 8 TAB-NEW IR-ARENA:ABORT IR-ARENA:ABORT drop ;

: CAP-HUGE ( -- )
   BND [: CAP-HUGE-BODY ;] IR-CTX:WITH-CONTEXT ;

: LCAP-ZERO-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 0 TAB-NEW IR-ARENA:ABORT IR-ARENA:ABORT drop ;

: LCAP-ZERO ( -- )
   BND [: LCAP-ZERO-BODY ;] IR-CTX:WITH-CONTEXT ;

: LCAP-HUGE-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 $100000000 TAB-NEW IR-ARENA:ABORT IR-ARENA:ABORT drop ;

: LCAP-HUGE ( -- )
   BND [: LCAP-HUGE-BODY ;] IR-CTX:WITH-CONTEXT ;

\ The caught quotation re-pushes its inputs before the throwing call, so the
\ stores stay readable after the named reject.
: CAPF-THIRD ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key -- IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key )
   {: c:IR-CTX:ctx a:IR-ARENA:arena r:IR-ARENA:arena key:IR-ID:ir-module-key :}
   c a r key
   c a r key IR--TYPE-FMT:SINGLE IR-TYPE:FLT drop ;

: CAPF-BODY ( IR-CTX:ctx -- n n n )
   {: c:IR-CTX:ctx :}
   c 2 8 TAB-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   c a r key I64 drop
   c a r key U8 drop
   c a r key [: CAPF-THIRD ;] catch
   {: c2:IR-CTX:ctx a2:IR-ARENA:arena r2:IR-ARENA:arena key2:IR-ID:ir-module-key rc:n :}
   rc
   r2 IR-TYPE:TYPES
   c2 a2 r2 key2 I64 IR-ID:TYPE-LOCAL ;

: POOLF-NEXT ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key -- IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key )
   {: c:IR-CTX:ctx a:IR-ARENA:arena r:IR-ARENA:arena key:IR-ID:ir-module-key :}
   c a r key
   c a r key I64 {: ti:IR-ID:ir-type-id :}
   IR-TYPE:FN-BEGIN
   ti IR-TYPE:FN-PARAM
   ti IR-TYPE:FN-RESULT
   c a r key IR-TYPE:QUOT drop ;

: POOLF-BODY ( IR-CTX:ctx -- n n n )
   {: c:IR-CTX:ctx :}
   c 8 2 TAB-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   c a r key I64 {: ti:IR-ID:ir-type-id :}
   IR-TYPE:FN-BEGIN
   ti IR-TYPE:FN-PARAM
   ti IR-TYPE:FN-PARAM
   c a r key IR-TYPE:QUOT {: f0:IR-ID:ir-type-id :}
   c a r key [: POOLF-NEXT ;] catch
   {: c2:IR-CTX:ctx a2:IR-ARENA:arena r2:IR-ARENA:arena key2:IR-ID:ir-module-key rc:n :}
   rc
   IR-TYPE:FN-BEGIN
   ti IR-TYPE:FN-PARAM
   ti IR-TYPE:FN-PARAM
   c2 a2 r2 key2 IR-TYPE:QUOT IR-ID:TYPE-LOCAL
   f0 IR-ID:TYPE-LOCAL ;

: CAP-CASES ( -- )
   s" a zero row capacity is rejected at creation" T-LABEL
   [: CAP-ZERO ;] E-IR-TYPE-CAP TTHROWSQ
   s" a negative row capacity is rejected at creation" T-LABEL
   [: CAP-NEG ;] E-IR-TYPE-CAP TTHROWSQ
   s" a row capacity past the ordinal range is rejected" T-LABEL
   [: CAP-HUGE ;] E-IR-TYPE-CAP TTHROWSQ
   s" a zero list capacity is rejected at creation" T-LABEL
   [: LCAP-ZERO ;] E-IR-TYPE-LIST TTHROWSQ
   s" a list capacity past the pool cell range is rejected" T-LABEL
   [: LCAP-HUGE ;] E-IR-TYPE-LIST TTHROWSQ
   s" a full row table rejects named; duplicates still answer" T-LABEL
   BND [: CAPF-BODY ;] IR-CTX:WITH-CONTEXT
   0 T= 2 T= E-IR-TYPE-CAP T=
   s" a full list pool rejects named; duplicate lists still answer" T-LABEL
   BND [: POOLF-BODY ;] IR-CTX:WITH-CONTEXT
   1 T= 1 T= E-IR-TYPE-LIST T= ;

\ ---- readers project every constructor ---------------------------------------
: RD-BODY ( IR-CTX:ctx -- bool bool bool bool bool n n n n n n )
   {: c:IR-CTX:ctx :}
   c 16 16 TAB-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   c a r key IR--TYPE-WIDTH:W16 IR--TYPE-SIGN:UNSIGNED IR-TYPE:INT {: t0:IR-ID:ir-type-id :}
   c a r key I64 {: ti:IR-ID:ir-type-id :}
   c a r key IR--TYPE-DOMAIN:IO IR-TYPE:TOKEN {: tk:IR-ID:ir-type-id :}
   c a r key IR--TYPE-SPACE:GENERIC ti IR-TYPE:POINTER {: p0:IR-ID:ir-type-id :}
   IR-TYPE:FN-BEGIN ti IR-TYPE:FN-PARAM t0 IR-TYPE:FN-PARAM ti IR-TYPE:FN-RESULT
   c a r key IR-TYPE:QUOT {: f0:IR-ID:ir-type-id :}
   r t0 IR-TYPE:KIND@ IR--TYPE-KIND:INT IR--TYPE-KIND:EQ
   r t0 IR-TYPE:INT@
   IR--TYPE-SIGN:UNSIGNED IR--TYPE-SIGN:EQ swap
   IR--TYPE-WIDTH:W16 IR--TYPE-WIDTH:EQ
   r tk IR-TYPE:TOKEN@ IR--TYPE-DOMAIN:IO IR--TYPE-DOMAIN:EQ
   r key p0 IR-TYPE:POINTER@
   IR-ID:TYPE-LOCAL swap
   IR--TYPE-SPACE:GENERIC IR--TYPE-SPACE:EQ swap
   r f0 IR-TYPE:ARITY@
   a r key f0 0 IR-TYPE:PARAM@ IR-ID:TYPE-LOCAL
   a r key f0 1 IR-TYPE:PARAM@ IR-ID:TYPE-LOCAL
   a r key f0 0 IR-TYPE:RESULT@ IR-ID:TYPE-LOCAL ;

: RD-CASE ( -- )
   s" readers project every constructor's stored fields" T-LABEL
   BND [: RD-BODY ;] IR-CTX:WITH-CONTEXT
   1 T= 0 T= 1 T= 1 T= 2 T= 1 T= TTRUE TTRUE TTRUE TTRUE TTRUE ;

: RDK-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 8 TAB-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   c a r key I64 {: ti:IR-ID:ir-type-id :}
   r ti IR-TYPE:FLT@ drop ;

: RDK-RUN ( -- )
   BND [: RDK-BODY ;] IR-CTX:WITH-CONTEXT ;

: RDB-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 8 TAB-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   c a r key I64 {: ti:IR-ID:ir-type-id :}
   IR-TYPE:FN-BEGIN ti IR-TYPE:FN-PARAM
   c a r key IR-TYPE:QUOT {: f0:IR-ID:ir-type-id :}
   a r key f0 1 IR-TYPE:PARAM@ drop ;

: RDB-RUN ( -- )
   BND [: RDB-BODY ;] IR-CTX:WITH-CONTEXT ;

: RDX-CASES ( -- )
   s" a wrong-kind field reader rejects named" T-LABEL
   [: RDK-RUN ;] E-IR-TYPE-KIND TTHROWSQ
   s" a parameter index past the arity rejects named" T-LABEL
   [: RDB-RUN ;] E-IR-TYPE-BOUND TTHROWSQ ;

\ ---- render fixtures pin identity --------------------------------------------
: REND= ( IR-ARENA:arena IR-ARENA:arena IR-ID:ir-type-id ptr u8 n -- bool )
   {: a:IR-ARENA:arena r:IR-ARENA:arena id:IR-ID:ir-type-id p u:n :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   a r id CBUF 64 IR-TYPE:RENDER {: n0:n :}
   CBUF n0 p u STR= ;

: RN-BODY ( IR-CTX:ctx -- bool bool bool bool bool bool bool bool bool bool )
   {: c:IR-CTX:ctx :}
   c 16 32 TAB-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   c a r key I32 {: t32:IR-ID:ir-type-id :}
   c a r key U8 {: tu8:IR-ID:ir-type-id :}
   c a r key IR--TYPE-WIDTH:W1 IR--TYPE-SIGN:UNSIGNED IR-TYPE:INT {: tb:IR-ID:ir-type-id :}
   c a r key IR--TYPE-FMT:DOUBLE IR-TYPE:FLT {: tf:IR-ID:ir-type-id :}
   c a r key I64 {: ti:IR-ID:ir-type-id :}
   c a r key IR--TYPE-SPACE:GENERIC ti IR-TYPE:POINTER {: p0:IR-ID:ir-type-id :}
   c a r key IR--TYPE-DOMAIN:IO IR-TYPE:TOKEN {: tk:IR-ID:ir-type-id :}
   c a r key IR-TYPE:MASK {: tm:IR-ID:ir-type-id :}
   c a r key IR-TYPE:OPAQUE {: to:IR-ID:ir-type-id :}
   IR-TYPE:FN-BEGIN ti IR-TYPE:FN-PARAM ti IR-TYPE:FN-PARAM ti IR-TYPE:FN-RESULT
   c a r key IR-TYPE:QUOT {: f0:IR-ID:ir-type-id :}
   IR-TYPE:FN-BEGIN
   c a r key IR-TYPE:CODE-REF {: f1:IR-ID:ir-type-id :}
   a r t32 s" i32" REND=
   a r tu8 s" u8" REND=
   a r tb s" u1" REND=
   a r tf s" f64" REND=
   a r p0 s" ptr<generic,i64>" REND=
   a r tk s" token<io>" REND=
   a r tm s" mask" REND=
   a r to s" opaque" REND=
   a r f0 s" [ i64 i64 -- i64 ]" REND=
   a r f1 s" ( -- )" REND= ;

: RN-CASE ( -- )
   s" render fixtures pin each constructor's deterministic spelling" T-LABEL
   BND [: RN-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE ;

\ Nesting: a pointer to a code reference renders through the recursion.
: RNN-BODY ( IR-CTX:ctx -- bool )
   {: c:IR-CTX:ctx :}
   c 16 16 TAB-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   c a r key I64 {: ti:IR-ID:ir-type-id :}
   IR-TYPE:FN-BEGIN ti IR-TYPE:FN-PARAM
   c a r key IR-TYPE:CODE-REF {: f0:IR-ID:ir-type-id :}
   c a r key IR--TYPE-SPACE:GENERIC f0 IR-TYPE:POINTER {: p0:IR-ID:ir-type-id :}
   a r p0 s" ptr<generic,( i64 -- )>" REND= ;

: RNN-CASE ( -- )
   s" a nested type renders through the reference recursion" T-LABEL
   BND [: RNN-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE ;

: RNR-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 8 TAB-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   c a r key I64 {: ti:IR-ID:ir-type-id :}
   c a r key IR--TYPE-SPACE:GENERIC ti IR-TYPE:POINTER {: p0:IR-ID:ir-type-id :}
   a r p0 CBUF 4 IR-TYPE:RENDER drop ;

: RNR-RUN ( -- )
   BND [: RNR-BODY ;] IR-CTX:WITH-CONTEXT ;

: RNR-CASE ( -- )
   s" a render span smaller than the text rejects named" T-LABEL
   [: RNR-RUN ;] E-IR-TYPE-RANGE TTHROWSQ ;

\ ---- non-table, misaligned, and forged rows ----------------------------------
: RAW-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 IR-ARENA:NEW IR-TYPE:TYPES drop ;

: RAW-RUN ( -- )
   BND [: RAW-BODY ;] IR-CTX:WITH-CONTEXT ;

\ The two stores are not interchangeable: the pool presented as the row
\ table (and the reverse) is a format-tag reject, not a misread.
: SWAP-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 8 TAB-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   c a r key I64 {: ti:IR-ID:ir-type-id :}
   r a ti CBUF 64 IR-TYPE:RENDER drop ;

: SWAP-RUN ( -- )
   BND [: SWAP-BODY ;] IR-CTX:WITH-CONTEXT ;

: SHAPE-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 8 TAB-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   c a r key I64 drop
   c r 7 IR-ARENA:PUSH drop
   r IR-TYPE:TYPES drop ;

: SHAPE-RUN ( -- )
   BND [: SHAPE-BODY ;] IR-CTX:WITH-CONTEXT ;

\ A bypass-appended row with an unknown kind code rejects at the decoder:
\ the closed world holds even against raw-cell forgery.
: KFORGE-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 8 TAB-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   c a r key I64 drop
   c r 99 IR-ARENA:PUSH drop
   c r 0 IR-ARENA:PUSH drop
   c r 0 IR-ARENA:PUSH drop
   c r 0 IR-ARENA:PUSH drop
   key 1 IR-ID:PACK-TYPE {: forged:IR-ID:ir-type-id :}
   r forged IR-TYPE:KIND@ drop ;

: KFORGE-RUN ( -- )
   BND [: KFORGE-BODY ;] IR-CTX:WITH-CONTEXT ;

\ A bypass-appended pointer row that references itself: the strict-decrease
\ recheck rejects before the renderer can loop.
: CFORGE-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 8 TAB-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   c a r key I64 drop
   c r 2 IR-ARENA:PUSH drop
   c r 0 IR-ARENA:PUSH drop
   c r 1 IR-ARENA:PUSH drop
   c r 0 IR-ARENA:PUSH drop
   key 1 IR-ID:PACK-TYPE {: forged:IR-ID:ir-type-id :}
   a r forged CBUF 64 IR-TYPE:RENDER drop ;

: CFORGE-RUN ( -- )
   BND [: CFORGE-BODY ;] IR-CTX:WITH-CONTEXT ;

\ A bypass-appended function row whose pool window crosses the live pool.
: SFORGE-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 8 TAB-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   c a r key I64 drop
   c r 3 IR-ARENA:PUSH drop
   c r 5 IR-ARENA:PUSH drop
   c r 9 IR-ARENA:PUSH drop
   c r 0 IR-ARENA:PUSH drop
   key 1 IR-ID:PACK-TYPE {: forged:IR-ID:ir-type-id :}
   a r key forged 0 IR-TYPE:PARAM@ drop ;

: SFORGE-RUN ( -- )
   BND [: SFORGE-BODY ;] IR-CTX:WITH-CONTEXT ;

: STATE-CASES ( -- )
   s" a bare arena is not a type table" T-LABEL
   [: RAW-RUN ;] E-IR-TYPE-STATE TTHROWSQ
   s" a swapped store pairing rejects on the format tag" T-LABEL
   [: SWAP-RUN ;] E-IR-TYPE-STATE TTHROWSQ
   s" a misaligned row shape rejects fail-closed" T-LABEL
   [: SHAPE-RUN ;] E-IR-TYPE-STATE TTHROWSQ
   s" a forged unknown kind code rejects at the decoder" T-LABEL
   [: KFORGE-RUN ;] E-IR-TYPE-STATE TTHROWSQ
   s" a forged self-referent row rejects on the strict decrease" T-LABEL
   [: CFORGE-RUN ;] E-IR-TYPE-STATE TTHROWSQ
   s" a forged pool window rejects fail-closed" T-LABEL
   [: SFORGE-RUN ;] E-IR-TYPE-STATE TTHROWSQ ;

\ ---- frozen modules own their types ------------------------------------------
: FZ-BODY ( IR-CTX:ctx -- n bool n n n bool n bool )
   {: c:IR-CTX:ctx :}
   c 16 16 TAB-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   c a r key I64 {: ti:IR-ID:ir-type-id :}
   c a r key IR--TYPE-SPACE:GENERIC ti IR-TYPE:POINTER {: p0:IR-ID:ir-type-id :}
   IR-TYPE:FN-BEGIN ti IR-TYPE:FN-PARAM ti IR-TYPE:FN-RESULT
   c a r key IR-TYPE:QUOT {: f0:IR-ID:ir-type-id :}
   a IR-ARENA:FREEZE {: pv:IR-ARENA:view :}
   r IR-ARENA:FREEZE {: rv:IR-ARENA:view :}
   rv IR-TYPE:FTYPES
   rv ti IR-TYPE:FKIND@ IR--TYPE-KIND:INT IR--TYPE-KIND:EQ
   rv f0 IR-TYPE:FARITY@
   pv rv key f0 0 IR-TYPE:FPARAM@ IR-ID:TYPE-LOCAL
   pv rv p0 CBUF 64 IR-TYPE:FRENDER {: n0:n :}
   CBUF n0 s" ptr<generic,i64>" STR=
   rv key p0 IR-TYPE:FPOINTER@ IR-ID:TYPE-LOCAL swap
   IR--TYPE-SPACE:GENERIC IR--TYPE-SPACE:EQ ;

: FZ-CASE ( -- )
   s" a frozen module serves every reader through the views" T-LABEL
   BND [: FZ-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE 0 T= TTRUE 0 T= 1 T= 1 T= TTRUE 3 T= ;

: FZ-PUSH-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 8 TAB-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   c a r key I64 drop
   a IR-ARENA:FREEZE drop
   r IR-ARENA:FREEZE drop
   c a r key U8 drop ;

: FZ-PUSH ( -- )
   BND [: FZ-PUSH-BODY ;] IR-CTX:WITH-CONTEXT ;

: FZ-READ-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 8 TAB-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   c a r key I64 {: ti:IR-ID:ir-type-id :}
   r IR-ARENA:FREEZE drop
   r ti IR-TYPE:KIND@ drop ;

: FZ-READ ( -- )
   BND [: FZ-READ-BODY ;] IR-CTX:WITH-CONTEXT ;

: FZ-REJECT-CASES ( -- )
   s" constructing through retired frozen handles rejects" T-LABEL
   [: FZ-PUSH ;] E-IR-ARENA-FROZEN TTHROWSQ
   s" live readers reject the retired builder handle; the views read" T-LABEL
   [: FZ-READ ;] E-IR-ARENA-FROZEN TTHROWSQ ;

\ ---- an intern is one commit, at the scratch edge ------------------------------
\ A type row is four appends, and an append grows the row arena by taking a
\ span from the context mapping. Interning at the edge of that mapping used to
\ stop part way through the four - a row-cell count four does not divide - and
\ every later read of the table failed its shape check. The row's storage is
\ now reserved before its first cell.
\
\ THE ARITHMETIC THIS CASE RESTS ON. An arena is seeded at eight cells. The row
\ table spends three on its header and four on the first type, so the second
\ type is the one that needs a doubled span - and after the starve there is no
\ span to be had. If that ever stops being true the intern below succeeds, its
\ code is zero, and this case fails rather than quietly proving nothing.
3 constant T-HDR-CELLS
4 constant T-ROW-CELLS

: TORN-ADD ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key -- IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key )
   {: c:IR-CTX:ctx a:IR-ARENA:arena r:IR-ARENA:arena key:IR-ID:ir-module-key :}
   c a r key
   c a r key I32 drop ;

\ Everything the refusal must have left alone: the table reads, holds exactly
\ the one type that landed, still answers its fields, and neither store has
\ advanced a cell.
: TORN-INTACT ( IR-ARENA:arena IR-ARENA:arena IR-ID:ir-type-id -- IR-ARENA:arena IR-ARENA:arena IR-ID:ir-type-id )
   {: a:IR-ARENA:arena r:IR-ARENA:arena t0:IR-ID:ir-type-id :}
   a r t0
   r IR-TYPE:TYPES 1 <> if E-IR-TYPE-STATE throw then
   r t0 IR-TYPE:KIND@ IR--TYPE-KIND:INT IR--TYPE-KIND:EQ
   0= if E-IR-TYPE-STATE throw then
   r t0 IR-TYPE:INT@ {: w:IR-TYPE:width sg:IR-TYPE:sign :}
   w IR--TYPE-WIDTH:W64 IR--TYPE-WIDTH:EQ 0= if E-IR-TYPE-STATE throw then
   sg IR--TYPE-SIGN:SIGNED IR--TYPE-SIGN:EQ 0= if E-IR-TYPE-STATE throw then
   a IR-ARENA:USED T-HDR-CELLS <> if E-IR-TYPE-STATE throw then
   r IR-ARENA:USED T-HDR-CELLS T-ROW-CELLS + <> if E-IR-TYPE-STATE throw then ;

: TORN-BODY ( IR-CTX:ctx -- n n )
   {: c:IR-CTX:ctx :}
   c 64 64 TAB-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   c a r key I64 {: t0:IR-ID:ir-type-id :}
   c IR-STARVE:EDGE
   c a r key [: TORN-ADD ;] catch
   {: c2:IR-CTX:ctx a2:IR-ARENA:arena r2:IR-ARENA:arena key2:IR-ID:ir-module-key rc:n :}
   rc
   a2 r2 t0 [: TORN-INTACT ;] catch nip nip nip ;

: TORN-CASE ( -- )
   s" an intern refused mid-row leaves the table whole and readable" T-LABEL
   BND [: TORN-BODY ;] IR-CTX:WITH-CONTEXT
   0 T= E-IR-CTX-SCRATCH T= ;

\ ---- teardown releases everything --------------------------------------------
: TD-ESC-BODY ( IR-CTX:ctx -- IR-ARENA:arena IR-ID:ir-type-id )
   {: c:IR-CTX:ctx :}
   c 8 8 TAB-NEW {: key:IR-ID:ir-module-key a:IR-ARENA:arena r:IR-ARENA:arena :}
   r c a r key I64 ;

: TD-READ ( -- )
   BND [: TD-ESC-BODY ;] IR-CTX:WITH-CONTEXT
   IR-TYPE:KIND@ drop ;

: TD-STALE-CASE ( -- )
   s" a type table is dead after its context ends" T-LABEL
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
   s" IRT-POS ( IR-ARENA:arena -- n ) IR-TYPE:TYPES"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" IRT-ID-FORGE ( n -- IR-ID:ir-type-id )"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" IRT-CTXLESS ( IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key IR-TYPE:width IR-TYPE:sign -- IR-ID:ir-type-id ) IR-TYPE:INT"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" IRT-KEYLESS ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-TYPE:space IR-ID:ir-type-id -- IR-ID:ir-type-id ) IR-TYPE:POINTER"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" IRT-RAW-PARAM ( n -- ) IR-TYPE:FN-PARAM"
      CHECK-QUIET-CANDIDATE! 0 T= ;

\ ---- run ---------------------------------------------------------------------
\ Throw-through fixtures run inside one outermost harness context, so a
\ context aborted by a throw is reclaimed by the harness exit instead of
\ lingering for the rest of the process. The teardown-reuse loop runs after
\ that exit, on registries the next sweep can fully reclaim.
: HARNESS-BODY ( IR-CTX:ctx -- )
   drop
   IN-CASE
   PT-CASE
   FN-CASE
   ORD-CASE
   SC-CASE
   TGT-CASES
   REF-CASES
   STG-CASES
   CAP-CASES
   RD-CASE
   RDX-CASES
   RN-CASE
   RNN-CASE
   RNR-CASE
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

IR-TYPE-TEST:RUN
