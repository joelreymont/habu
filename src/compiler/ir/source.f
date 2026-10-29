\ source.f - the compiler source registry: module-local source identities,
\ byte spans and origin chains, on one IR-ARENA arena per module. A row holds a
\ source's byte length and origin, not its bytes; every registration mints a new
\ ordinal, even for the same bytes. Spans are bytes only; encoding is checked at
\ the source tape (src/compiler/native/tape.f).
require src/lib/prelude.f
require src/lib/errors.f
require src/compiler/ir/id.f
require src/compiler/ir/context.f
require src/compiler/ir/arena.f

package IR-SOURCE
public

\ A value, not authority: SPAN mints it validated and every consumer revalidates
\ with SPAN-CK. Its source-id field still needs a genuine identity.
STRUCTURE span
   FIELD src IR-ID:ir-source-id
   FIELD start n
   FIELD len n
;STRUCTURE

private

\ One-way projections of IR-ID identities onto serials; nothing here re-mints.
CAST: KEY-SERIAL ( IR-ID:ir-module-key -- n )
CAST: MID-SERIAL ( IR-ID:ir-module-id -- n )

\ ---- layout ------------------------------------------------------------------
$53524331 constant SRC-MAGIC         \ "SRC1": the registry header format tag
0 constant HC-MAGIC
1 constant HC-SERIAL
2 constant HC-CAP
3 constant HDR-CELLS
0 constant OFF-LEN
1 constant OFF-ORG
2 constant ROW-CELLS
0 constant ORG-NONE                  \ origin cell of a root source
public
$FFFFFFFF HDR-CELLS - ROW-CELLS / constant CAP-MAX
private

\ ---- cell access -------------------------------------------------------------
\ The registry is resolved once at the public word into a reader, which refuses
\ the wrong live/frozen state itself.

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

\ The header is bound to key's module serial; the registry dies with the owning
\ context.
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

\ The capacity check comes first so a full registry answers its own error, then
\ the reservation makes both appends infallible. The reader survives PUSH: it
\ re-reads the row's pointer and count on every call.
: ROW-PUT ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:reader n n -- n )
   {: c:IR-CTX:ctx a:IR-ARENA:arena r:IR-ARENA:reader u:n org:n :}
   u 0 < if E-IR-SRC-LEN throw then
   r ROOM-CK
   c a ROW-CELLS IR-ARENA:RESERVE
   r CNT {: l:n :}
   c a u IR-ARENA:PUSH drop
   c a org IR-ARENA:PUSH drop
   l ;

\ An origin must name an already registered source, so a parent's ordinal is
\ below its child's and no chain can close (E-IR-SRC-ORIGIN); walks recheck the
\ strict decrease, so they terminate on any registry state. A self cycle or a
\ multi-node cycle presents a not-yet-registered ordinal.
: ORIGIN-CK ( IR-ARENA:reader IR-ID:ir-source-id -- n )
   {: r:IR-ARENA:reader parent:IR-ID:ir-source-id :}
   r HC-SERIAL IR-ARENA:RD@ parent ID-OWNER-SERIAL SERIAL-CK
   parent IR-ID:SOURCE-LOCAL
   dup r CNT >= if E-IR-SRC-ORIGIN throw then
   1+ ;

public

: REGISTER ( IR-CTX:ctx IR-ARENA:arena IR-ID:ir-module-key ptr u8 n -- IR-ID:ir-source-id )
   {: c:IR-CTX:ctx a:IR-ARENA:arena key:IR-ID:ir-module-key p u:n :}
   a IR-ARENA:OPEN-LIVE {: r:IR-ARENA:reader :}
   r key KEY-CK
   c a r u ORG-NONE ROW-PUT
   key swap IR-ID:PACK-SOURCE ;

: REGISTER-FROM ( IR-CTX:ctx IR-ARENA:arena IR-ID:ir-module-key IR-ID:ir-source-id ptr u8 n -- IR-ID:ir-source-id )
   {: c:IR-CTX:ctx a:IR-ARENA:arena key:IR-ID:ir-module-key parent:IR-ID:ir-source-id p u:n :}
   a IR-ARENA:OPEN-LIVE {: r:IR-ARENA:reader :}
   r key KEY-CK
   r parent ORIGIN-CK {: org:n :}
   c a r u org ROW-PUT
   key swap IR-ID:PACK-SOURCE ;

\ ---- live readers ------------------------------------------------------------
: SOURCES ( IR-ARENA:arena -- n )
   IR-ARENA:OPEN-LIVE dup HDR-CK CNT ;

: LEN@ ( IR-ARENA:arena IR-ID:ir-source-id -- n )
   {: a:IR-ARENA:arena id:IR-ID:ir-source-id :}
   a IR-ARENA:OPEN-LIVE {: r:IR-ARENA:reader :}
   r id ID-CK {: l:n :}
   r l OFF-LEN RC@ ;

\ ---- frozen readers ----------------------------------------------------------
: FSOURCES ( IR-ARENA:view -- n )
   IR-ARENA:OPEN dup HDR-CK CNT ;

: FLEN@ ( IR-ARENA:view IR-ID:ir-source-id -- n )
   {: v:IR-ARENA:view id:IR-ID:ir-source-id :}
   v IR-ARENA:OPEN {: r:IR-ARENA:reader :}
   r id ID-CK {: l:n :}
   r l OFF-LEN RC@ ;

: FROOT? ( IR-ARENA:view IR-ID:ir-source-id -- bool )
   {: v:IR-ARENA:view id:IR-ID:ir-source-id :}
   v IR-ARENA:OPEN {: r:IR-ARENA:reader :}
   r id ID-CK {: l:n :}
   r l OFF-ORG RC@ ORG-NONE = ;

\ For a pass that rewrites a module over the same source. Only a root can be
\ carried: an origin ordinal names a row of the other registry.
: CARRY ( IR-CTX:ctx IR-ARENA:arena IR-ID:ir-module-key IR-ARENA:view IR-ID:ir-source-id -- IR-ID:ir-source-id )
   {: c:IR-CTX:ctx a:IR-ARENA:arena key:IR-ID:ir-module-key v:IR-ARENA:view id:IR-ID:ir-source-id :}
   a IR-ARENA:OPEN-LIVE {: r:IR-ARENA:reader :}
   r key KEY-CK
   v id FROOT? 0= if E-IR-SRC-ORIGIN throw then
   c a r  v id FLEN@  ORG-NONE ROW-PUT
   key swap IR-ID:PACK-SOURCE ;

\ ---- byte spans --------------------------------------------------------------
private

: RANGE-CK ( n n n -- )
   {: sl:n st:n ln:n :}
   st 0 < ln 0 < or if E-IR-SRC-SPAN throw then
   st ln +
   dup 0 < if E-IR-SRC-SPAN throw then
   sl > if E-IR-SRC-SPAN throw then ;

public

: SPAN ( IR-ARENA:arena IR-ID:ir-source-id n n -- IR-SOURCE:span )
   {: a:IR-ARENA:arena id:IR-ID:ir-source-id st:n ln:n :}
   a id LEN@ st ln RANGE-CK
   id st ln IR--SOURCE-SPAN:MAKE ;

: SPAN-CK ( IR-ARENA:arena IR-SOURCE:span -- )
   IR--SOURCE-SPAN:UNMAKE
   {: a:IR-ARENA:arena id:IR-ID:ir-source-id st:n ln:n :}
   a id LEN@ st ln RANGE-CK ;

: SPAN-START ( IR-SOURCE:span -- n )
   IR--SOURCE-SPAN:UNMAKE drop nip ;

: SPAN-LEN ( IR-SOURCE:span -- n )
   IR--SOURCE-SPAN:UNMAKE nip nip ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;package
