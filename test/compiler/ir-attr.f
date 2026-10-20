\ ir-attr.f - the integer and symbol attributes used by compiled modules.

require src/lib/test.f
require src/compiler/ir/attr.f

package IR-ATTR-TEST
private

TYPED-VARIABLE ERR-C IR-CTX:ctx
TYPED-VARIABLE ERR-R IR-ARENA:arena
TYPED-VARIABLE ERR-K IR-ID:ir-module-key
TYPED-VARIABLE ERR-ID IR-ID:ir-attr-id

: CAP-MORE ( -- )
   ERR-C @ ERR-R @ ERR-K @ 8 IR-ATTR:INT drop ;

: FOREIGN-INT ( -- )
   ERR-C @ ERR-R @ ERR-K @ 1 IR-ATTR:INT drop ;

: FOREIGN-SYM ( -- )
   ERR-R @ ERR-K @ ERR-ID @ IR-ATTR:SYM@ drop ;

: BND ( -- CBIND:binding )
   CTARGET-ARCH:AARCH64 CTARGET-ABI:AAPCS64-DARWIN CTARGET-ENDIAN:LITTLE
   CTARGET-PTR--WIDTH:BITS64
   CTARGET:F-BASE CTARGET:F-FP CTARGET:WITH CTARGET:CONTRACT
   CNUM-OVERFLOW:TRAP CNUM-FLOAT--MODEL:IEEE754 CNUM-CONTRACTION:FORBIDDEN
   CNUM-FAST--MATH:BIT-EXACT CNUM-COMPARE:IEEE754-UNORDERED CNUM:POLICY
   CBIND:BIND ;

: VALUES ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c IR-CTX:NEW-MODULE drop {: key:IR-ID:ir-module-key :}
   c key 3 IR-ATTR:NEW {: r:IR-ARENA:arena :}
   c r key -7 IR-ATTR:INT {: a:IR-ID:ir-attr-id :}
   c r key 8 IR-ATTR:INT {: b:IR-ID:ir-attr-id :}
   c r key -7 IR-ATTR:INT IR-ID:ATTR-LOCAL a IR-ID:ATTR-LOCAL T=
   r IR-ATTR:ATTRS 2 T=
   r a IR-ATTR:KIND@ IR--ATTR-KIND:INT IR--ATTR-KIND:EQ TTRUE
   r a IR-ATTR:INT@ -7 T=
   r b IR-ATTR:INT@ 8 T=
   r IR-ARENA:FREEZE {: rv:IR-ARENA:view :}
   rv a IR-ATTR:FKIND@ IR--ATTR-KIND:INT IR--ATTR-KIND:EQ TTRUE
   rv a IR-ATTR:FINT@ -7 T=
   rv IR-ARENA:OPEN a IR-ATTR:RINT@ -7 T= ;

: SYMBOLS ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c IR-CTX:NEW-MODULE drop {: key:IR-ID:ir-module-key :}
   c key 2 IR-ATTR:NEW {: r:IR-ARENA:arena :}
   c key 2 32 IR-SYM:NEW {: sp:IR-ARENA:arena sr:IR-ARENA:arena :}
   c sp sr key s" alpha" IR-SYM:INTERN {: a:IR-ID:ir-symbol-id :}
   c sp sr key s" beta" IR-SYM:INTERN {: b:IR-ID:ir-symbol-id :}
   c r key sr a IR-ATTR:SYMBOL {: x:IR-ID:ir-attr-id :}
   c r key sr a IR-ATTR:SYMBOL IR-ID:ATTR-LOCAL x IR-ID:ATTR-LOCAL T=
   c r key sr b IR-ATTR:SYMBOL {: y:IR-ID:ir-attr-id :}
   x IR-ID:ATTR-LOCAL y IR-ID:ATTR-LOCAL = TFALSE
   r IR-ATTR:ATTRS 2 T=
   r x IR-ATTR:KIND@ IR--ATTR-KIND:SYM IR--ATTR-KIND:EQ TTRUE
   r key x IR-ATTR:SYM@ IR-ID:SYMBOL-LOCAL a IR-ID:SYMBOL-LOCAL T=
   r IR-ARENA:FREEZE {: rv:IR-ARENA:view :}
   rv key y IR-ATTR:FSYM@ IR-ID:SYMBOL-LOCAL b IR-ID:SYMBOL-LOCAL T= ;

: CAPACITY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c IR-CTX:NEW-MODULE drop {: key:IR-ID:ir-module-key :}
   c key 1 IR-ATTR:NEW {: r:IR-ARENA:arena :}
   c r key 7 IR-ATTR:INT {: a:IR-ID:ir-attr-id :}
   c r key 7 IR-ATTR:INT IR-ID:ATTR-LOCAL a IR-ID:ATTR-LOCAL T=
   c ERR-C ! r ERR-R ! key ERR-K !
   [: CAP-MORE ;] E-IR-ATTR-CAP TTHROWSQ
   r IR-ATTR:ATTRS 1 T=
   r a IR-ATTR:INT@ 7 T= ;

: FOREIGN ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c IR-CTX:NEW-MODULE drop {: key:IR-ID:ir-module-key :}
   c IR-CTX:NEW-MODULE drop {: other:IR-ID:ir-module-key :}
   c key 2 IR-ATTR:NEW {: r:IR-ARENA:arena :}
   c ERR-C ! r ERR-R ! other ERR-K !
   [: FOREIGN-INT ;] E-IR-ATTR-OWNER TTHROWSQ
   c r key 1 IR-ATTR:INT {: id:IR-ID:ir-attr-id :}
   id ERR-ID !
   [: FOREIGN-SYM ;] E-IR-ATTR-OWNER TTHROWSQ
   r id IR-ATTR:INT@ 1 T= ;

public

: RUN ( -- )
   T-RESET
   s" integer attributes intern and survive freeze" T-LABEL
   BND [: VALUES ;] IR-CTX:WITH-CONTEXT
   s" symbol attributes retain their module identity" T-LABEL
   BND [: SYMBOLS ;] IR-CTX:WITH-CONTEXT
   s" a full table accepts duplicates but rejects new rows" T-LABEL
   BND [: CAPACITY ;] IR-CTX:WITH-CONTEXT
   s" a foreign key cannot access this table" T-LABEL
   BND [: FOREIGN ;] IR-CTX:WITH-CONTEXT
   T-REPORT ;

;package

IR-ATTR-TEST:RUN
