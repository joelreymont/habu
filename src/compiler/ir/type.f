\ type.f - the compiler type table: canonical, structurally interned type
\ records shared by every dialect. Two IR-ARENA stores per module: a list pool of
\ function-type (params ++ results) ordinals and a row table of four-cell rows
\ (kind code and three fields).

require src/lib/prelude.f
require src/lib/errors.f
require src/compiler/target.f
require src/compiler/binding.f
require src/compiler/ir/id.f
require src/compiler/ir/context.f
require src/compiler/ir/arena.f

package IR-TYPE
public

\ The wire codes below persist these.
ENUM kind DERIVE eq
   int
   float
   pointer
   quotation
   code-ref
   memory-token
   mask
   opaque
;ENUM

ENUM width DERIVE eq
   w1
   w8
   w16
   w32
   w64
;ENUM

ENUM sign DERIVE eq
   unsigned
   signed
;ENUM

ENUM fmt DERIVE eq
   half
   bfloat
   single
   double
;ENUM

ENUM space DERIVE eq
   generic
   global
   shared
   local
   param
   const
;ENUM

ENUM domain DERIVE eq
   data-mem
   dict
   code-pub
   io
   process
   ffi
;ENUM

private

\ One-way projections of IR-ID identities onto serials; nothing here re-mints.
CAST: KEY-SERIAL ( IR-ID:ir-module-key -- n )
CAST: MID-SERIAL ( IR-ID:ir-module-id -- n )

\ ---- layout ------------------------------------------------------------------
$54594C31 constant TYL-MAGIC         \ "TYL1": the list-pool header format tag
$54595231 constant TYR-MAGIC         \ "TYR1": the row-table header format tag
0 constant HC-MAGIC
1 constant HC-SERIAL
2 constant HC-CAP
3 constant HDR-CELLS
0 constant OFF-KIND
1 constant OFF-A
2 constant OFF-B
3 constant OFF-C
4 constant ROW-CELLS
public
$FFFFFFFF HDR-CELLS - ROW-CELLS / constant CAP-MAX
$FFFFFFFF HDR-CELLS - constant LIST-MAX
private
64 constant ARITY-MAX                \ native signatures carry at most 64 cells

\ ---- stable wire codes -------------------------------------------------------
\ One injective code per family: a code may be added but never renumbered
\ without a schema bump.
0 constant K-INT
1 constant K-FLT
2 constant K-PTR
3 constant K-QUOT
4 constant K-CREF
5 constant K-TOK
6 constant K-MASK
7 constant K-OPQ

: KIND-CODE ( IR-TYPE:kind -- n )
   MATCH kind
      int          OF K-INT ENDOF
      float        OF K-FLT ENDOF
      pointer      OF K-PTR ENDOF
      quotation    OF K-QUOT ENDOF
      code-ref     OF K-CREF ENDOF
      memory-token OF K-TOK ENDOF
      mask         OF K-MASK ENDOF
      opaque       OF K-OPQ ENDOF
   ;MATCH ;

: WIDTH-CODE ( IR-TYPE:width -- n )
   MATCH width
      w1  OF 0 ENDOF
      w8  OF 1 ENDOF
      w16 OF 2 ENDOF
      w32 OF 3 ENDOF
      w64 OF 4 ENDOF
   ;MATCH ;

: SIGN-CODE ( IR-TYPE:sign -- n )
   MATCH sign
      unsigned OF 0 ENDOF
      signed   OF 1 ENDOF
   ;MATCH ;

: FMT-CODE ( IR-TYPE:fmt -- n )
   MATCH fmt
      half   OF 0 ENDOF
      bfloat OF 1 ENDOF
      single OF 2 ENDOF
      double OF 3 ENDOF
   ;MATCH ;

: SPACE-CODE ( IR-TYPE:space -- n )
   MATCH space
      generic OF 0 ENDOF
      global  OF 1 ENDOF
      shared  OF 2 ENDOF
      local   OF 3 ENDOF
      param   OF 4 ENDOF
      const   OF 5 ENDOF
   ;MATCH ;

: DOMAIN-CODE ( IR-TYPE:domain -- n )
   MATCH domain
      data-mem OF 0 ENDOF
      dict     OF 1 ENDOF
      code-pub OF 2 ENDOF
      io       OF 3 ENDOF
      process  OF 4 ENDOF
      ffi      OF 5 ENDOF
   ;MATCH ;

\ ---- wire-code decoders ------------------------------------------------------
\ A stored code outside the vocabulary is a forged row and is refused.
: N>KIND ( n -- IR-TYPE:kind )
   case
      K-INT  of IR--TYPE-KIND:INT endof
      K-FLT  of IR--TYPE-KIND:FLOAT endof
      K-PTR  of IR--TYPE-KIND:POINTER endof
      K-QUOT of IR--TYPE-KIND:QUOTATION endof
      K-CREF of IR--TYPE-KIND:CODE-REF endof
      K-TOK  of IR--TYPE-KIND:MEMORY-TOKEN endof
      K-MASK of IR--TYPE-KIND:MASK endof
      K-OPQ  of IR--TYPE-KIND:OPAQUE endof
      E-IR-TYPE-STATE throw
   endcase ;

: N>WIDTH ( n -- IR-TYPE:width )
   case
      0 of IR--TYPE-WIDTH:W1 endof
      1 of IR--TYPE-WIDTH:W8 endof
      2 of IR--TYPE-WIDTH:W16 endof
      3 of IR--TYPE-WIDTH:W32 endof
      4 of IR--TYPE-WIDTH:W64 endof
      E-IR-TYPE-STATE throw
   endcase ;

: N>SIGN ( n -- IR-TYPE:sign )
   case
      0 of IR--TYPE-SIGN:UNSIGNED endof
      1 of IR--TYPE-SIGN:SIGNED endof
      E-IR-TYPE-STATE throw
   endcase ;

: N>FMT ( n -- IR-TYPE:fmt )
   case
      0 of IR--TYPE-FMT:HALF endof
      1 of IR--TYPE-FMT:BFLOAT endof
      2 of IR--TYPE-FMT:SINGLE endof
      3 of IR--TYPE-FMT:DOUBLE endof
      E-IR-TYPE-STATE throw
   endcase ;

: N>SPACE ( n -- IR-TYPE:space )
   case
      0 of IR--TYPE-SPACE:GENERIC endof
      1 of IR--TYPE-SPACE:GLOBAL endof
      2 of IR--TYPE-SPACE:SHARED endof
      3 of IR--TYPE-SPACE:LOCAL endof
      4 of IR--TYPE-SPACE:PARAM endof
      5 of IR--TYPE-SPACE:CONST endof
      E-IR-TYPE-STATE throw
   endcase ;

: N>DOMAIN ( n -- IR-TYPE:domain )
   case
      0 of IR--TYPE-DOMAIN:DATA-MEM endof
      1 of IR--TYPE-DOMAIN:DICT endof
      2 of IR--TYPE-DOMAIN:CODE-PUB endof
      3 of IR--TYPE-DOMAIN:IO endof
      4 of IR--TYPE-DOMAIN:PROCESS endof
      5 of IR--TYPE-DOMAIN:FFI endof
      E-IR-TYPE-STATE throw
   endcase ;

\ ---- target legality ---------------------------------------------------------
\ Legality comes from the context's binding (IR-CTX:BINDING@): a float format or
\ address space the target lacks is E-IR-TYPE-TARGET. Integer widths are
\ target-neutral.
: CTX-TARGET ( IR-CTX:ctx -- CTARGET:contract )
   IR-CTX:BINDING@ CBIND:TARGET@ ;

: FMT-FEATURE ( IR-TYPE:fmt -- CTARGET:features )
   MATCH fmt
      half   OF CTARGET:F-FP16 ENDOF
      bfloat OF CTARGET:F-BF16 ENDOF
      single OF CTARGET:F-SCALAR-FP ENDOF
      double OF CTARGET:F-SCALAR-FP ENDOF
   ;MATCH ;

: FMT-CK ( IR-CTX:ctx IR-TYPE:fmt -- )
   {: c:IR-CTX:ctx f:fmt :}
   c CTX-TARGET CTARGET:FEATURES@ f FMT-FEATURE CTARGET:HAS? 0= if
      E-IR-TYPE-TARGET throw
   then ;

\ CPU targets use one flat generic space; PTX defines the full six-space model.
: SPACE-LEGAL? ( CTARGET:arch IR-TYPE:space -- bool )
   {: k:CTARGET:arch s:space :}
   k MATCH CTARGET:arch
      aarch64 OF s IR--TYPE-SPACE:GENERIC IR--TYPE-SPACE:EQ ENDOF
      ptx     OF true ENDOF
      a32     OF s IR--TYPE-SPACE:GENERIC IR--TYPE-SPACE:EQ ENDOF
      thumb2  OF s IR--TYPE-SPACE:GENERIC IR--TYPE-SPACE:EQ ENDOF
      c66x    OF s IR--TYPE-SPACE:GENERIC IR--TYPE-SPACE:EQ ENDOF
      x86-64  OF s IR--TYPE-SPACE:GENERIC IR--TYPE-SPACE:EQ ENDOF
      wasm    OF s IR--TYPE-SPACE:GENERIC IR--TYPE-SPACE:EQ ENDOF
   ;MATCH ;

: SPACE-CK ( IR-CTX:ctx IR-TYPE:space -- )
   {: c:IR-CTX:ctx s:space :}
   c CTX-TARGET CTARGET:ARCH@ s SPACE-LEGAL? 0= if
      E-IR-TYPE-TARGET throw
   then ;

\ ---- cell access -------------------------------------------------------------
\ The stores are resolved once at the public word into readers, which refuse
\ the wrong live/frozen state themselves.

\ ---- headers and shape -------------------------------------------------------
: PSHAPE-CK ( n -- )
   HDR-CELLS < if E-IR-TYPE-STATE throw then ;

: RSHAPE-CK ( n -- )
   dup HDR-CELLS < if E-IR-TYPE-STATE throw then
   HDR-CELLS - ROW-CELLS mod 0 <> if E-IR-TYPE-STATE throw then ;

: PMAGIC-CK ( n -- )
   TYL-MAGIC <> if E-IR-TYPE-STATE throw then ;

: RMAGIC-CK ( n -- )
   TYR-MAGIC <> if E-IR-TYPE-STATE throw then ;

: PHDR-CK ( IR-ARENA:reader -- )
   {: pr:IR-ARENA:reader :}
   pr IR-ARENA:RD-SIZE PSHAPE-CK
   pr HC-MAGIC IR-ARENA:RD@ PMAGIC-CK ;

: RHDR-CK ( IR-ARENA:reader -- )
   {: rr:IR-ARENA:reader :}
   rr IR-ARENA:RD-SIZE RSHAPE-CK
   rr HC-MAGIC IR-ARENA:RD@ RMAGIC-CK ;

: USED>CNT ( n -- n )
   HDR-CELLS - ROW-CELLS / ;

: CNT ( IR-ARENA:reader -- n )
   IR-ARENA:RD-SIZE USED>CNT ;

: PCELLS ( IR-ARENA:reader -- n )
   IR-ARENA:RD-SIZE HDR-CELLS - ;

\ ---- ownership ---------------------------------------------------------------
: SERIAL-CK ( n n -- )
   <> if E-IR-TYPE-OWNER throw then ;

\ A cross-module pairing rejects before a row window is trusted against the pool.
: PAIR-CK ( IR-ARENA:reader IR-ARENA:reader -- )
   {: pr:IR-ARENA:reader rr:IR-ARENA:reader :}
   pr PHDR-CK
   rr RHDR-CK
   pr HC-SERIAL IR-ARENA:RD@ rr HC-SERIAL IR-ARENA:RD@ SERIAL-CK ;

: KEY-CK ( IR-ARENA:reader IR-ARENA:reader IR-ID:ir-module-key -- )
   {: pr:IR-ARENA:reader rr:IR-ARENA:reader key:IR-ID:ir-module-key :}
   pr rr PAIR-CK
   rr HC-SERIAL IR-ARENA:RD@ key KEY-SERIAL SERIAL-CK ;

: RKEY-CK ( IR-ARENA:reader IR-ID:ir-module-key -- )
   {: rr:IR-ARENA:reader key:IR-ID:ir-module-key :}
   rr RHDR-CK
   rr HC-SERIAL IR-ARENA:RD@ key KEY-SERIAL SERIAL-CK ;

: ID-OWNER-SERIAL ( IR-ID:ir-type-id -- n )
   IR-ID:TYPE-OWNER MID-SERIAL ;

: ID-CK-N ( n n IR-ID:ir-type-id -- n )
   {: hs:n cnt:n id:IR-ID:ir-type-id :}
   hs id ID-OWNER-SERIAL SERIAL-CK
   id IR-ID:TYPE-LOCAL
   dup cnt >= if E-IR-TYPE-BOUND throw then ;

: ID-CK ( IR-ARENA:reader IR-ID:ir-type-id -- n )
   {: rr:IR-ARENA:reader id:IR-ID:ir-type-id :}
   rr RHDR-CK
   rr HC-SERIAL IR-ARENA:RD@ rr CNT id ID-CK-N ;

\ ---- row and pool addressing -------------------------------------------------
: ROW-CELL ( n n -- n )
   swap ROW-CELLS * HDR-CELLS + + ;

: RC@ ( IR-ARENA:reader n n -- n )
   ROW-CELL IR-ARENA:RD@ ;

: PC@ ( IR-ARENA:reader n -- n )
   HDR-CELLS + IR-ARENA:RD@ ;

: REF-OK ( n n -- n )
   {: l:n raw:n :}
   raw 0 < raw l >= or if E-IR-TYPE-STATE throw then
   raw ;

\ Revalidated on every access, so a forged row cannot read past the pool.
: SPAN-CK-N ( n n n n -- )
   {: pc:n st:n pn:n rn:n :}
   st 0 < pn 0 < or rn 0 < or if E-IR-TYPE-STATE throw then
   st pn + rn + pc > if E-IR-TYPE-STATE throw then ;

: FN-SPAN ( IR-ARENA:reader IR-ARENA:reader n -- n n n )
   {: pr:IR-ARENA:reader rr:IR-ARENA:reader l:n :}
   rr l OFF-A RC@ rr l OFF-B RC@ rr l OFF-C RC@
   {: st:n pn:n rn:n :}
   pr PCELLS st pn rn SPAN-CK-N
   st pn rn ;

: FNKIND? ( n -- bool )
   dup K-QUOT = swap K-CREF = or ;

: FNKIND-CK ( n -- )
   FNKIND? 0= if E-IR-TYPE-KIND throw then ;

\ ---- interning scan ----------------------------------------------------------
\ Every constructor interns by field equality. A referenced type must already
\ exist in this table, so references point strictly downward (E-IR-TYPE-BOUND);
\ by induction over ordinals, id equality is structural equality, so the scan
\ compares stored ordinals without re-walking.
: ROW4-MATCH? ( IR-ARENA:reader n n n n n -- bool )
   {: rr:IR-ARENA:reader l:n k:n x:n y:n z:n :}
   rr l OFF-KIND RC@ k <> if false exit then
   rr l OFF-A RC@ x <> if false exit then
   rr l OFF-B RC@ y <> if false exit then
   rr l OFF-C RC@ z = ;

: SCAN4 ( IR-ARENA:reader n n n n -- n )
   {: rr:IR-ARENA:reader k:n x:n y:n z:n :}
   -1
   rr CNT 0 ?do
      rr i k x y z ROW4-MATCH? if drop i leave then
   loop ;

\ Capacity is checked and the row reserved before the first write, so a full
\ table stays usable and duplicates still answer. The caller's readers survive
\ RESERVE and PUSH: they re-read the row's pointer and count on every call.
: INTERN4 ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:reader IR-ARENA:reader IR-ID:ir-module-key n n n n -- IR-ID:ir-type-id )
   {: c:IR-CTX:ctx r:IR-ARENA:arena pr:IR-ARENA:reader rr:IR-ARENA:reader key:IR-ID:ir-module-key k:n x:n y:n z:n :}
   pr rr key KEY-CK
   rr k x y z SCAN4 {: hit:n :}
   hit 0 < 0= if key hit IR-ID:PACK-TYPE exit then
   rr CNT rr HC-CAP IR-ARENA:RD@ >= if E-IR-TYPE-CAP throw then
   c r ROW-CELLS IR-ARENA:RESERVE
   rr CNT {: l:n :}
   c r k IR-ARENA:PUSH drop
   c r x IR-ARENA:PUSH drop
   c r y IR-ARENA:PUSH drop
   c r z IR-ARENA:PUSH drop
   key l IR-ID:PACK-TYPE ;

\ ---- function-type stage -----------------------------------------------------
0 CWORK-VARIABLE STG-OPEN n
0 CWORK-VARIABLE STG-PN n
0 CWORK-VARIABLE STG-RN n
ARITY-MAX CWORK-SPAN STG-PO n
ARITY-MAX CWORK-SPAN STG-PL n
ARITY-MAX CWORK-SPAN STG-RO n
ARITY-MAX CWORK-SPAN STG-RL n

: PO@ ( n -- n )
   cells STG-PO + @ ;

: PO! ( n n -- )
   cells STG-PO + ! ;

: PL@ ( n -- n )
   cells STG-PL + @ ;

: PL! ( n n -- )
   cells STG-PL + ! ;

: RO@ ( n -- n )
   cells STG-RO + @ ;

: RO! ( n n -- )
   cells STG-RO + ! ;

: RL@ ( n -- n )
   cells STG-RL + @ ;

: RL! ( n n -- )
   cells STG-RL + ! ;

: STG-OPEN-CK ( -- )
   STG-OPEN @ 0= if E-IR-TYPE-STAGE throw then ;

\ The same no-forward-reference rule as POINTER.
: STG-REF-CK ( n n n n -- )
   {: hs:n cnt:n os:n loc:n :}
   os hs <> if E-IR-TYPE-OWNER throw then
   loc 0 < loc cnt >= or if E-IR-TYPE-BOUND throw then ;

: STG-VALIDATE ( n n -- )
   {: hs:n cnt:n :}
   STG-PN @ 0 ?do
      hs cnt i PO@ i PL@ STG-REF-CK
   loop
   STG-RN @ 0 ?do
      hs cnt i RO@ i RL@ STG-REF-CK
   loop ;

: STG-AT ( n -- n )
   {: i:n :}
   i STG-PN @ < if i PL@ else i STG-PN @ - RL@ then ;

: ROWFN-MATCH? ( IR-ARENA:reader IR-ARENA:reader n n -- bool )
   {: pr:IR-ARENA:reader rr:IR-ARENA:reader l:n kc:n :}
   rr l OFF-KIND RC@ kc <> if false exit then
   rr l OFF-B RC@ STG-PN @ <> if false exit then
   rr l OFF-C RC@ STG-RN @ <> if false exit then
   pr rr l FN-SPAN {: st:n pn:n rn:n :}
   pn rn + 0 ?do
      pr st i + PC@ i STG-AT <> if false unloop exit then
   loop
   true ;

: SCANFN ( IR-ARENA:reader IR-ARENA:reader n -- n )
   {: pr:IR-ARENA:reader rr:IR-ARENA:reader kc:n :}
   -1
   rr CNT 0 ?do
      pr rr i kc ROWFN-MATCH? if drop i leave then
   loop ;

\ The stage is consumed first, before the readers open, so neither a rejected
\ end nor a dead table leaves a half-staged list; all checks and reservations
\ precede the first write.
: FN-END ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key n -- IR-ID:ir-type-id )
   {: c:IR-CTX:ctx a:IR-ARENA:arena r:IR-ARENA:arena key:IR-ID:ir-module-key kc:n :}
   STG-OPEN-CK
   0 STG-OPEN !
   a IR-ARENA:OPEN-LIVE r IR-ARENA:OPEN-LIVE {: pr:IR-ARENA:reader rr:IR-ARENA:reader :}
   pr rr key KEY-CK
   key KEY-SERIAL rr CNT STG-VALIDATE
   pr rr kc SCANFN {: hit:n :}
   hit 0 < 0= if key hit IR-ID:PACK-TYPE exit then
   rr CNT rr HC-CAP IR-ARENA:RD@ >= if E-IR-TYPE-CAP throw then
   pr PCELLS STG-PN @ + STG-RN @ + pr HC-CAP IR-ARENA:RD@ > if E-IR-TYPE-LIST throw then
   c a STG-PN @ STG-RN @ + IR-ARENA:RESERVE
   c r ROW-CELLS IR-ARENA:RESERVE
   pr PCELLS {: st:n :}
   STG-PN @ STG-RN @ + 0 ?do
      c a i STG-AT IR-ARENA:PUSH drop
   loop
   rr CNT {: l:n :}
   c r kc IR-ARENA:PUSH drop
   c r st IR-ARENA:PUSH drop
   c r STG-PN @ IR-ARENA:PUSH drop
   c r STG-RN @ IR-ARENA:PUSH drop
   key l IR-ID:PACK-TYPE ;

\ ---- creation checks ---------------------------------------------------------
: ROW-CAP-OK ( n -- )
   dup 1 < over CAP-MAX > or if E-IR-TYPE-CAP throw then
   drop ;

: LIST-CAP-OK ( n -- )
   dup 1 < over LIST-MAX > or if E-IR-TYPE-LIST throw then
   drop ;

public

\ ---- creation ----------------------------------------------------------------
\ Both headers are bound to key's module serial; the table dies with the owning
\ context.
: NEW ( IR-CTX:ctx IR-ID:ir-module-key n n -- IR-ARENA:arena IR-ARENA:arena )
   {: c:IR-CTX:ctx key:IR-ID:ir-module-key tcap:n lcap:n :}
   tcap ROW-CAP-OK
   lcap LIST-CAP-OK
   c lcap HDR-CELLS + IR-ARENA:NEW {: a:IR-ARENA:arena :}
   c a HDR-CELLS IR-ARENA:RESERVE
   c a TYL-MAGIC IR-ARENA:PUSH drop
   c a key KEY-SERIAL IR-ARENA:PUSH drop
   c a lcap IR-ARENA:PUSH drop
   c tcap ROW-CELLS * HDR-CELLS + IR-ARENA:NEW {: r:IR-ARENA:arena :}
   c r HDR-CELLS IR-ARENA:RESERVE
   c r TYR-MAGIC IR-ARENA:PUSH drop
   c r key KEY-SERIAL IR-ARENA:PUSH drop
   c r tcap IR-ARENA:PUSH drop
   a r ;

\ ---- constructors ------------------------------------------------------------
\ A hit allocates nothing. A signed 1-bit integer is E-IR-TYPE-SCALAR.
: INT ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key IR-TYPE:width IR-TYPE:sign -- IR-ID:ir-type-id )
   {: c:IR-CTX:ctx a:IR-ARENA:arena r:IR-ARENA:arena key:IR-ID:ir-module-key w:width s:sign :}
   w WIDTH-CODE {: wc:n :}
   s SIGN-CODE {: sc:n :}
   wc 0= sc 1 = and if E-IR-TYPE-SCALAR throw then
   a IR-ARENA:OPEN-LIVE r IR-ARENA:OPEN-LIVE {: pr:IR-ARENA:reader rr:IR-ARENA:reader :}
   c r pr rr key K-INT wc sc 0 INTERN4 ;

: FLT ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key IR-TYPE:fmt -- IR-ID:ir-type-id )
   {: c:IR-CTX:ctx a:IR-ARENA:arena r:IR-ARENA:arena key:IR-ID:ir-module-key f:fmt :}
   c f FMT-CK
   a IR-ARENA:OPEN-LIVE r IR-ARENA:OPEN-LIVE {: pr:IR-ARENA:reader rr:IR-ARENA:reader :}
   c r pr rr key K-FLT f FMT-CODE 0 0 INTERN4 ;

: POINTER ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key IR-TYPE:space IR-ID:ir-type-id -- IR-ID:ir-type-id )
   {: c:IR-CTX:ctx a:IR-ARENA:arena r:IR-ARENA:arena key:IR-ID:ir-module-key s:space ptee:IR-ID:ir-type-id :}
   c s SPACE-CK
   r IR-ARENA:OPEN-LIVE {: rr:IR-ARENA:reader :}
   rr ptee ID-CK {: pl:n :}
   a IR-ARENA:OPEN-LIVE {: pr:IR-ARENA:reader :}
   c r pr rr key K-PTR s SPACE-CODE pl 0 INTERN4 ;

: TOKEN ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key IR-TYPE:domain -- IR-ID:ir-type-id )
   {: c:IR-CTX:ctx a:IR-ARENA:arena r:IR-ARENA:arena key:IR-ID:ir-module-key d:domain :}
   a IR-ARENA:OPEN-LIVE r IR-ARENA:OPEN-LIVE {: pr:IR-ARENA:reader rr:IR-ARENA:reader :}
   c r pr rr key K-TOK d DOMAIN-CODE 0 0 INTERN4 ;

: MASK ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key -- IR-ID:ir-type-id )
   {: c:IR-CTX:ctx a:IR-ARENA:arena r:IR-ARENA:arena key:IR-ID:ir-module-key :}
   a IR-ARENA:OPEN-LIVE r IR-ARENA:OPEN-LIVE {: pr:IR-ARENA:reader rr:IR-ARENA:reader :}
   c r pr rr key K-MASK 0 0 0 INTERN4 ;

: OPAQUE ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key -- IR-ID:ir-type-id )
   {: c:IR-CTX:ctx a:IR-ARENA:arena r:IR-ARENA:arena key:IR-ID:ir-module-key :}
   a IR-ARENA:OPEN-LIVE r IR-ARENA:OPEN-LIVE {: pr:IR-ARENA:reader rr:IR-ARENA:reader :}
   c r pr rr key K-OPQ 0 0 0 INTERN4 ;

\ ---- function-type stage protocol --------------------------------------------
: FN-BEGIN ( -- )
   STG-OPEN @ 0 <> if E-IR-TYPE-STAGE throw then
   1 STG-OPEN !
   0 STG-PN !
   0 STG-RN ! ;

: FN-PARAM ( IR-ID:ir-type-id -- )
   {: id:IR-ID:ir-type-id :}
   STG-OPEN-CK
   STG-PN @ ARITY-MAX >= if E-IR-TYPE-ARITY throw then
   id ID-OWNER-SERIAL STG-PN @ PO!
   id IR-ID:TYPE-LOCAL STG-PN @ PL!
   STG-PN @ 1+ STG-PN ! ;

: FN-RESULT ( IR-ID:ir-type-id -- )
   {: id:IR-ID:ir-type-id :}
   STG-OPEN-CK
   STG-RN @ ARITY-MAX >= if E-IR-TYPE-ARITY throw then
   id ID-OWNER-SERIAL STG-RN @ RO!
   id IR-ID:TYPE-LOCAL STG-RN @ RL!
   STG-RN @ 1+ STG-RN ! ;

: QUOT ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key -- IR-ID:ir-type-id )
   K-QUOT FN-END ;

: CODE-REF ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key -- IR-ID:ir-type-id )
   K-CREF FN-END ;

\ ---- live readers ------------------------------------------------------------
\ Readers recheck the strict decrease, so recursive walks terminate on any table
\ state.
: TYPES ( IR-ARENA:arena -- n )
   IR-ARENA:OPEN-LIVE dup RHDR-CK CNT ;

: KIND@ ( IR-ARENA:arena IR-ID:ir-type-id -- IR-TYPE:kind )
   {: r:IR-ARENA:arena id:IR-ID:ir-type-id :}
   r IR-ARENA:OPEN-LIVE {: rr:IR-ARENA:reader :}
   rr id ID-CK {: l:n :}
   rr l OFF-KIND RC@ N>KIND ;

private

: KIND-CK ( n n -- )
   <> if E-IR-TYPE-KIND throw then ;

public

: INT@ ( IR-ARENA:arena IR-ID:ir-type-id -- IR-TYPE:width IR-TYPE:sign )
   {: r:IR-ARENA:arena id:IR-ID:ir-type-id :}
   r IR-ARENA:OPEN-LIVE {: rr:IR-ARENA:reader :}
   rr id ID-CK {: l:n :}
   rr l OFF-KIND RC@ K-INT KIND-CK
   rr l OFF-A RC@ N>WIDTH
   rr l OFF-B RC@ N>SIGN ;

: FLT@ ( IR-ARENA:arena IR-ID:ir-type-id -- IR-TYPE:fmt )
   {: r:IR-ARENA:arena id:IR-ID:ir-type-id :}
   r IR-ARENA:OPEN-LIVE {: rr:IR-ARENA:reader :}
   rr id ID-CK {: l:n :}
   rr l OFF-KIND RC@ K-FLT KIND-CK
   rr l OFF-A RC@ N>FMT ;

: TOKEN@ ( IR-ARENA:arena IR-ID:ir-type-id -- IR-TYPE:domain )
   {: r:IR-ARENA:arena id:IR-ID:ir-type-id :}
   r IR-ARENA:OPEN-LIVE {: rr:IR-ARENA:reader :}
   rr id ID-CK {: l:n :}
   rr l OFF-KIND RC@ K-TOK KIND-CK
   rr l OFF-A RC@ N>DOMAIN ;

: POINTER@ ( IR-ARENA:arena IR-ID:ir-module-key IR-ID:ir-type-id -- IR-TYPE:space IR-ID:ir-type-id )
   {: r:IR-ARENA:arena key:IR-ID:ir-module-key id:IR-ID:ir-type-id :}
   r IR-ARENA:OPEN-LIVE {: rr:IR-ARENA:reader :}
   rr key RKEY-CK
   rr id ID-CK {: l:n :}
   rr l OFF-KIND RC@ K-PTR KIND-CK
   rr l OFF-A RC@ N>SPACE
   key l rr l OFF-B RC@ REF-OK IR-ID:PACK-TYPE ;

: ARITY@ ( IR-ARENA:arena IR-ID:ir-type-id -- n n )
   {: r:IR-ARENA:arena id:IR-ID:ir-type-id :}
   r IR-ARENA:OPEN-LIVE {: rr:IR-ARENA:reader :}
   rr id ID-CK {: l:n :}
   rr l OFF-KIND RC@ FNKIND-CK
   rr l OFF-B RC@ rr l OFF-C RC@ ;

: PARAM@ ( IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key IR-ID:ir-type-id n -- IR-ID:ir-type-id )
   {: a:IR-ARENA:arena r:IR-ARENA:arena key:IR-ID:ir-module-key id:IR-ID:ir-type-id i:n :}
   a IR-ARENA:OPEN-LIVE r IR-ARENA:OPEN-LIVE {: pr:IR-ARENA:reader rr:IR-ARENA:reader :}
   pr rr key KEY-CK
   rr id ID-CK {: l:n :}
   rr l OFF-KIND RC@ FNKIND-CK
   pr rr l FN-SPAN {: st:n pn:n rn:n :}
   i 0 < i pn >= or if E-IR-TYPE-BOUND throw then
   key l pr st i + PC@ REF-OK IR-ID:PACK-TYPE ;

: RESULT@ ( IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key IR-ID:ir-type-id n -- IR-ID:ir-type-id )
   {: a:IR-ARENA:arena r:IR-ARENA:arena key:IR-ID:ir-module-key id:IR-ID:ir-type-id i:n :}
   a IR-ARENA:OPEN-LIVE r IR-ARENA:OPEN-LIVE {: pr:IR-ARENA:reader rr:IR-ARENA:reader :}
   pr rr key KEY-CK
   rr id ID-CK {: l:n :}
   rr l OFF-KIND RC@ FNKIND-CK
   pr rr l FN-SPAN {: st:n pn:n rn:n :}
   i 0 < i rn >= or if E-IR-TYPE-BOUND throw then
   key l pr st pn + i + PC@ REF-OK IR-ID:PACK-TYPE ;

\ ---- render ------------------------------------------------------------------
\ The spelling is diagnostic text, independent of ordinals, never parsed.
private

\ cur rides on top so emission chains read left to right.
: EMIT-B ( n ptr u8 n n -- n )
   {: cur:n q cap:n b:n :}
   cur cap >= if E-IR-TYPE-RANGE throw then
   b q cur + c!
   cur 1+ ;

: EMIT-S ( n ptr u8 n ptr u8 n -- n )
   {: cur:n q cap:n p u:n :}
   cur u + cap > if E-IR-TYPE-RANGE throw then
   u 0 ?do
      p i + c@ q cur + i + c!
   loop
   cur u + ;

: SIGN-STR ( n -- ptr u8 n )
   case
      0 of s" u" endof
      1 of s" i" endof
      E-IR-TYPE-STATE throw
   endcase ;

: WIDTH-STR ( n -- ptr u8 n )
   case
      0 of s" 1" endof
      1 of s" 8" endof
      2 of s" 16" endof
      3 of s" 32" endof
      4 of s" 64" endof
      E-IR-TYPE-STATE throw
   endcase ;

: FMT-STR ( n -- ptr u8 n )
   case
      0 of s" f16" endof
      1 of s" bf16" endof
      2 of s" f32" endof
      3 of s" f64" endof
      E-IR-TYPE-STATE throw
   endcase ;

: SPACE-STR ( n -- ptr u8 n )
   case
      0 of s" generic" endof
      1 of s" global" endof
      2 of s" shared" endof
      3 of s" local" endof
      4 of s" param" endof
      5 of s" const" endof
      E-IR-TYPE-STATE throw
   endcase ;

: DOMAIN-STR ( n -- ptr u8 n )
   case
      0 of s" data-mem" endof
      1 of s" dict" endof
      2 of s" code-pub" endof
      3 of s" io" endof
      4 of s" process" endof
      5 of s" ffi" endof
      E-IR-TYPE-STATE throw
   endcase ;

: EMIT-INT ( n ptr u8 n n n -- n )
   {: cur:n q cap:n wc:n sc:n :}
   cur q cap sc SIGN-STR EMIT-S
   q cap wc WIDTH-STR EMIT-S ;

: EMIT-TOKEN ( n ptr u8 n n -- n )
   {: cur:n q cap:n dc:n :}
   cur q cap s" token<" EMIT-S
   q cap dc DOMAIN-STR EMIT-S
   q cap $3E EMIT-B ;

: OPEN-B ( n -- n )
   K-QUOT = if $5B else $28 then ;

: CLOSE-B ( n -- n )
   K-QUOT = if $5D else $29 then ;

\ RECURSE binds the containing word, so the recursive arms stay inline here.
: R-EMIT ( IR-ARENA:reader IR-ARENA:reader ptr u8 n n n -- n )
   {: pr:IR-ARENA:reader rr:IR-ARENA:reader q cap:n cur:n l:n :}
   rr l OFF-KIND RC@ {: k:n :}
   k K-INT = if cur q cap rr l OFF-A RC@ rr l OFF-B RC@ EMIT-INT exit then
   k K-FLT = if cur q cap rr l OFF-A RC@ FMT-STR EMIT-S exit then
   k K-TOK = if cur q cap rr l OFF-A RC@ EMIT-TOKEN exit then
   k K-MASK = if cur q cap s" mask" EMIT-S exit then
   k K-OPQ = if cur q cap s" opaque" EMIT-S exit then
   k K-PTR = if
      cur q cap s" ptr<" EMIT-S
      q cap rr l OFF-A RC@ SPACE-STR EMIT-S
      q cap $2C EMIT-B
      {: cm:n :}
      pr rr q cap cm  l rr l OFF-B RC@ REF-OK  recurse
      q cap $3E EMIT-B
      exit
   then
   k FNKIND? 0= if E-IR-TYPE-STATE throw then
   pr rr l FN-SPAN {: st:n pn:n rn:n :}
   cur q cap k OPEN-B EMIT-B
   pn 0 ?do
      q cap $20 EMIT-B
      {: ci:n :}
      pr rr q cap ci  l pr st i + PC@ REF-OK  recurse
   loop
   q cap $20 EMIT-B
   q cap s" --" EMIT-S
   rn 0 ?do
      q cap $20 EMIT-B
      {: ci:n :}
      pr rr q cap ci  l pr st pn + i + PC@ REF-OK  recurse
   loop
   q cap $20 EMIT-B
   q cap k CLOSE-B EMIT-B ;

public

\ A span too small rejects before the overflowing write.
: RENDER ( IR-ARENA:arena IR-ARENA:arena IR-ID:ir-type-id ptr u8 n -- n )
   {: a:IR-ARENA:arena r:IR-ARENA:arena id:IR-ID:ir-type-id q cap:n :}
   a IR-ARENA:OPEN-LIVE r IR-ARENA:OPEN-LIVE {: pr:IR-ARENA:reader rr:IR-ARENA:reader :}
   pr rr PAIR-CK
   rr id ID-CK {: l:n :}
   pr rr q cap 0 l R-EMIT ;

\ ---- frozen readers ----------------------------------------------------------
: FTYPES ( IR-ARENA:view -- n )
   IR-ARENA:OPEN dup RHDR-CK CNT ;

: FKIND@ ( IR-ARENA:view IR-ID:ir-type-id -- IR-TYPE:kind )
   {: rv:IR-ARENA:view id:IR-ID:ir-type-id :}
   rv IR-ARENA:OPEN {: rr:IR-ARENA:reader :}
   rr id ID-CK {: l:n :}
   rr l OFF-KIND RC@ N>KIND ;

: FINT@ ( IR-ARENA:view IR-ID:ir-type-id -- IR-TYPE:width IR-TYPE:sign )
   {: rv:IR-ARENA:view id:IR-ID:ir-type-id :}
   rv IR-ARENA:OPEN {: rr:IR-ARENA:reader :}
   rr id ID-CK {: l:n :}
   rr l OFF-KIND RC@ K-INT KIND-CK
   rr l OFF-A RC@ N>WIDTH
   rr l OFF-B RC@ N>SIGN ;

: FFLT@ ( IR-ARENA:view IR-ID:ir-type-id -- IR-TYPE:fmt )
   {: rv:IR-ARENA:view id:IR-ID:ir-type-id :}
   rv IR-ARENA:OPEN {: rr:IR-ARENA:reader :}
   rr id ID-CK {: l:n :}
   rr l OFF-KIND RC@ K-FLT KIND-CK
   rr l OFF-A RC@ N>FMT ;

: FTOKEN@ ( IR-ARENA:view IR-ID:ir-type-id -- IR-TYPE:domain )
   {: rv:IR-ARENA:view id:IR-ID:ir-type-id :}
   rv IR-ARENA:OPEN {: rr:IR-ARENA:reader :}
   rr id ID-CK {: l:n :}
   rr l OFF-KIND RC@ K-TOK KIND-CK
   rr l OFF-A RC@ N>DOMAIN ;

: FPOINTER@ ( IR-ARENA:view IR-ID:ir-module-key IR-ID:ir-type-id -- IR-TYPE:space IR-ID:ir-type-id )
   {: rv:IR-ARENA:view key:IR-ID:ir-module-key id:IR-ID:ir-type-id :}
   rv IR-ARENA:OPEN {: rr:IR-ARENA:reader :}
   rr key RKEY-CK
   rr id ID-CK {: l:n :}
   rr l OFF-KIND RC@ K-PTR KIND-CK
   rr l OFF-A RC@ N>SPACE
   key l rr l OFF-B RC@ REF-OK IR-ID:PACK-TYPE ;

: RARITY@ ( IR-ARENA:reader IR-ID:ir-type-id -- n n )
   {: rv:IR-ARENA:reader id:IR-ID:ir-type-id :}
   rv IR-ARENA:FROZEN-READER {: rr:IR-ARENA:reader :}
   rr id ID-CK {: l:n :}
   rr l OFF-KIND RC@ FNKIND-CK
   rr l OFF-B RC@ rr l OFF-C RC@ ;

: FARITY@ ( IR-ARENA:view IR-ID:ir-type-id -- n n )
   {: rv:IR-ARENA:view id:IR-ID:ir-type-id :}
   rv IR-ARENA:OPEN id RARITY@ ;

: FPARAM@ ( IR-ARENA:view IR-ARENA:view IR-ID:ir-module-key IR-ID:ir-type-id n -- IR-ID:ir-type-id )
   {: pv:IR-ARENA:view rv:IR-ARENA:view key:IR-ID:ir-module-key id:IR-ID:ir-type-id i:n :}
   pv IR-ARENA:OPEN rv IR-ARENA:OPEN {: pr:IR-ARENA:reader rr:IR-ARENA:reader :}
   pr rr PAIR-CK
   rr key RKEY-CK
   rr id ID-CK {: l:n :}
   rr l OFF-KIND RC@ FNKIND-CK
   pr rr l FN-SPAN {: st:n pn:n rn:n :}
   i 0 < i pn >= or if E-IR-TYPE-BOUND throw then
   key l pr st i + PC@ REF-OK IR-ID:PACK-TYPE ;

: FRESULT@ ( IR-ARENA:view IR-ARENA:view IR-ID:ir-module-key IR-ID:ir-type-id n -- IR-ID:ir-type-id )
   {: pv:IR-ARENA:view rv:IR-ARENA:view key:IR-ID:ir-module-key id:IR-ID:ir-type-id i:n :}
   pv IR-ARENA:OPEN rv IR-ARENA:OPEN {: pr:IR-ARENA:reader rr:IR-ARENA:reader :}
   pr rr PAIR-CK
   rr key RKEY-CK
   rr id ID-CK {: l:n :}
   rr l OFF-KIND RC@ FNKIND-CK
   pr rr l FN-SPAN {: st:n pn:n rn:n :}
   i 0 < i rn >= or if E-IR-TYPE-BOUND throw then
   key l pr st pn + i + PC@ REF-OK IR-ID:PACK-TYPE ;

: FRENDER ( IR-ARENA:view IR-ARENA:view IR-ID:ir-type-id ptr u8 n -- n )
   {: pv:IR-ARENA:view rv:IR-ARENA:view id:IR-ID:ir-type-id q cap:n :}
   pv IR-ARENA:OPEN rv IR-ARENA:OPEN {: pr:IR-ARENA:reader rr:IR-ARENA:reader :}
   pr rr PAIR-CK
   rr id ID-CK {: l:n :}
   pr rr q cap 0 l R-EMIT ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;package
