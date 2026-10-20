\ attr.f - module-local integer and symbol attributes.
\ The context owns one row arena per module. Each distinct (kind, value)
\ pair receives an ir-attr-id under that module's key; freezing the arena
\ preserves the same rows for later compiler passes.

require src/lib/prelude.f
require src/lib/errors.f
require src/compiler/ir/id.f
require src/compiler/ir/context.f
require src/compiler/ir/arena.f
require src/compiler/ir/symbol.f

package IR-ATTR
public

ENUM kind DERIVE eq
   int
   sym
;ENUM

private

CAST: KEY-SERIAL ( IR-ID:ir-module-key -- n )
CAST: MID-SERIAL ( IR-ID:ir-module-id -- n )

$41545231 constant ATR-MAGIC
0 constant HC-MAGIC
1 constant HC-SERIAL
2 constant HC-CAP
3 constant HDR-CELLS
0 constant OFF-KIND
1 constant OFF-VALUE
2 constant ROW-CELLS
0 constant K-INT
3 constant K-SYM

public
$FFFFFFFF HDR-CELLS - ROW-CELLS / constant CAP-MAX
private

: N>KIND ( n -- IR-ATTR:kind )
   case
      K-INT of IR--ATTR-KIND:INT endof
      K-SYM of IR--ATTR-KIND:SYM endof
      E-IR-ATTR-STATE throw
   endcase ;

: HDR-CK ( IR-ARENA:reader -- )
   {: rr:IR-ARENA:reader :}
   rr IR-ARENA:RD-SIZE dup HDR-CELLS < if E-IR-ATTR-STATE throw then
   HDR-CELLS - ROW-CELLS mod 0 <> if E-IR-ATTR-STATE throw then
   rr HC-MAGIC IR-ARENA:RD@ ATR-MAGIC <> if E-IR-ATTR-STATE throw then ;

: CNT ( IR-ARENA:reader -- n )
   IR-ARENA:RD-SIZE HDR-CELLS - ROW-CELLS / ;

: SERIAL-CK ( n n -- )
   <> if E-IR-ATTR-OWNER throw then ;

: KEY-CK ( IR-ARENA:reader IR-ID:ir-module-key -- )
   {: rr:IR-ARENA:reader key:IR-ID:ir-module-key :}
   rr HDR-CK
   rr HC-SERIAL IR-ARENA:RD@ key KEY-SERIAL SERIAL-CK ;

: ID-CK ( IR-ARENA:reader IR-ID:ir-attr-id -- n )
   {: rr:IR-ARENA:reader id:IR-ID:ir-attr-id :}
   rr HDR-CK
   rr HC-SERIAL IR-ARENA:RD@
   id IR-ID:ATTR-OWNER MID-SERIAL SERIAL-CK
   id IR-ID:ATTR-LOCAL dup rr CNT >= if E-IR-ATTR-BOUND throw then ;

: RC@ ( IR-ARENA:reader n n -- n )
   {: rr:IR-ARENA:reader l:n field:n :}
   rr l ROW-CELLS * HDR-CELLS + field + IR-ARENA:RD@ ;

: KIND-CK ( n n -- )
   <> if E-IR-ATTR-KIND throw then ;

: MATCH? ( IR-ARENA:reader n n n -- bool )
   {: rr:IR-ARENA:reader l:n kind:n value:n :}
   rr l OFF-KIND RC@ kind =
   rr l OFF-VALUE RC@ value = and ;

: FIND ( IR-ARENA:reader n n -- n )
   {: rr:IR-ARENA:reader kind:n value:n :}
   -1
   rr CNT 0 ?do
      rr i kind value MATCH? if drop i leave then
   loop ;

: INTERN ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:reader IR-ID:ir-module-key n n -- IR-ID:ir-attr-id )
   {: c:IR-CTX:ctx r:IR-ARENA:arena rr:IR-ARENA:reader key:IR-ID:ir-module-key kind:n value:n :}
   rr key KEY-CK
   rr kind value FIND {: hit:n :}
   hit 0 < 0= if key hit IR-ID:PACK-ATTR exit then
   rr CNT rr HC-CAP IR-ARENA:RD@ >= if E-IR-ATTR-CAP throw then
   rr CNT {: l:n :}
   c r ROW-CELLS IR-ARENA:RESERVE
   c r kind IR-ARENA:PUSH drop
   c r value IR-ARENA:PUSH drop
   key l IR-ID:PACK-ATTR ;

public

: NEW ( IR-CTX:ctx IR-ID:ir-module-key n -- IR-ARENA:arena )
   {: c:IR-CTX:ctx key:IR-ID:ir-module-key cap:n :}
   cap 1 < cap CAP-MAX > or if E-IR-ATTR-CAP throw then
   c cap ROW-CELLS * HDR-CELLS + IR-ARENA:NEW {: r:IR-ARENA:arena :}
   c r HDR-CELLS IR-ARENA:RESERVE
   c r ATR-MAGIC IR-ARENA:PUSH drop
   c r key KEY-SERIAL IR-ARENA:PUSH drop
   c r cap IR-ARENA:PUSH drop
   r ;

: INT ( IR-CTX:ctx IR-ARENA:arena IR-ID:ir-module-key n -- IR-ID:ir-attr-id )
   {: c:IR-CTX:ctx r:IR-ARENA:arena key:IR-ID:ir-module-key value:n :}
   c r r IR-ARENA:OPEN-LIVE key K-INT value INTERN ;

: SYMBOL ( IR-CTX:ctx IR-ARENA:arena IR-ID:ir-module-key IR-ARENA:arena IR-ID:ir-symbol-id -- IR-ID:ir-attr-id )
   {: c:IR-CTX:ctx r:IR-ARENA:arena key:IR-ID:ir-module-key sr:IR-ARENA:arena id:IR-ID:ir-symbol-id :}
   id IR-ID:SYMBOL-OWNER MID-SERIAL key KEY-SERIAL SERIAL-CK
   sr id IR-SYM:LEN@ drop
   c r r IR-ARENA:OPEN-LIVE key K-SYM id IR-ID:SYMBOL-LOCAL INTERN ;

: ATTRS ( IR-ARENA:arena -- n )
   IR-ARENA:OPEN-LIVE dup HDR-CK CNT ;

: KIND@ ( IR-ARENA:arena IR-ID:ir-attr-id -- IR-ATTR:kind )
   {: r:IR-ARENA:arena id:IR-ID:ir-attr-id :}
   r IR-ARENA:OPEN-LIVE {: rr:IR-ARENA:reader :}
   rr rr id ID-CK OFF-KIND RC@ N>KIND ;

: INT@ ( IR-ARENA:arena IR-ID:ir-attr-id -- n )
   {: r:IR-ARENA:arena id:IR-ID:ir-attr-id :}
   r IR-ARENA:OPEN-LIVE {: rr:IR-ARENA:reader :}
   rr id ID-CK {: l:n :}
   rr l OFF-KIND RC@ K-INT KIND-CK
   rr l OFF-VALUE RC@ ;

: SYM@ ( IR-ARENA:arena IR-ID:ir-module-key IR-ID:ir-attr-id -- IR-ID:ir-symbol-id )
   {: r:IR-ARENA:arena key:IR-ID:ir-module-key id:IR-ID:ir-attr-id :}
   r IR-ARENA:OPEN-LIVE {: rr:IR-ARENA:reader :}
   rr key KEY-CK
   rr id ID-CK {: l:n :}
   rr l OFF-KIND RC@ K-SYM KIND-CK
   rr l OFF-VALUE RC@ dup 0 < if E-IR-ATTR-STATE throw then
   key swap IR-ID:PACK-SYMBOL ;

: FATTRS ( IR-ARENA:view -- n )
   IR-ARENA:OPEN dup HDR-CK CNT ;

: FKIND@ ( IR-ARENA:view IR-ID:ir-attr-id -- IR-ATTR:kind )
   {: rv:IR-ARENA:view id:IR-ID:ir-attr-id :}
   rv IR-ARENA:OPEN {: rr:IR-ARENA:reader :}
   rr rr id ID-CK OFF-KIND RC@ N>KIND ;

: RINT@ ( IR-ARENA:reader IR-ID:ir-attr-id -- n )
   {: reader:IR-ARENA:reader id:IR-ID:ir-attr-id :}
   reader IR-ARENA:FROZEN-READER {: rr:IR-ARENA:reader :}
   rr id ID-CK {: l:n :}
   rr l OFF-KIND RC@ K-INT KIND-CK
   rr l OFF-VALUE RC@ ;

: FINT@ ( IR-ARENA:view IR-ID:ir-attr-id -- n )
   {: rv:IR-ARENA:view id:IR-ID:ir-attr-id :}
   rv IR-ARENA:OPEN id RINT@ ;

: FSYM@ ( IR-ARENA:view IR-ID:ir-module-key IR-ID:ir-attr-id -- IR-ID:ir-symbol-id )
   {: rv:IR-ARENA:view key:IR-ID:ir-module-key id:IR-ID:ir-attr-id :}
   rv IR-ARENA:OPEN {: rr:IR-ARENA:reader :}
   rr key KEY-CK
   rr id ID-CK {: l:n :}
   rr l OFF-KIND RC@ K-SYM KIND-CK
   rr l OFF-VALUE RC@ dup 0 < if E-IR-ATTR-STATE throw then
   key swap IR-ID:PACK-SYMBOL ;

;package
