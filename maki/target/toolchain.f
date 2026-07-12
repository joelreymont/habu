\ maki/target/toolchain.f - the toolchain identity owner (CAD-KIND:toolchain-id).
\
\ MODEL-CAD-V2 R3. A toolchain is the *semantic* thing that assembled an artifact:
\ which compiler, at which version, against which driver, under which config. Until
\ now nothing owned that fact - lib/ptx/toolchain.f (package PTXTC) resolves a ptxas
\ path and runs it, and maki/sched-key.f serializes display text ("unprobed") into a
\ cache key. Neither is an identity: two different ptxas versions render the same key
\ text, so a schedule measured under one assembler can be replayed under another.
\
\ This file owns the identity instead. A descriptor is immutable once defined:
\ (compiler kind, compiler version, driver kind, driver version, config). Defining
\ one renders its canonical form, digests that form (FNV-1a 64), and interns the row -
\ text included, so every projection returns a stable span rather than a view of a
\ shared render buffer that the next call would overwrite.
\ Equal facts collapse to one identity; any change to a version or to the config
\ produces a different canonical form, a different digest, and therefore a different
\ identity. That is the whole point: the identity is a function of the facts, so it
\ cannot silently alias a toolchain we did not actually run.
\
\ Injectivity (why fields are framed). The canonical form is what the digest names, so
\ the rendering must be *injective*: distinct facts must never render the same bytes.
\ A plain `k=v;k=v` rendering is not - a config of "x;cfg=y" and a driver version of
\ "2;cfg=x" forge each other's field boundaries, and two genuinely different toolchains
\ collapse onto one identity that reports facts neither was defined with. So every
\ field is framed by its own exact byte length: `tag=LLLL:bytes`, LLLL being the length
\ as LEN-DIGITS hex digits. A reader takes the length first and then exactly that many
\ bytes, so no field's *content* can be read as another field's structure. Field order
\ is fixed and the kinds render from a closed audited domain, so the form is a total
\ injective function of the descriptor.
\
\ Identity refinement is private to the package API: callers use DEFINE, ADOPT, and
\ LOOKUP rather than RAW>TC / TC>RAW. Habu does not yet expose a generic owner-package
\ seal; ordinary `package TOOLCHAIN` reopening therefore remains a compiler capability
\ gap. The generated TOOLCHAIN-DISC constructor package is protected automatically by
\ PRODUCT and cannot be reopened. A target-id
\ and a toolchain-id are both one cell and the checker keeps them apart; so do a
\ compiler kind and a driver kind (TOOLCHAIN:compiler, TOOLCHAIN:driver).
\
\ Epochs (why an id is not a row index). RESET empties the table, so a bare row index
\ would be resurrected by the next descriptor to take that row - an id handed out
\ before the RESET would keep answering, naming whatever toolchain now occupies the
\ row. An id is therefore (generation, row) packed into its one cell: RESET advances
\ the generation, which retires every id issued under the old one. The generation does
\ not wrap - exhausting it is TOOLCHAIN:E-EPOCH, not a silent return to a generation whose
\ ids are still held. Every id is checked for its generation *before* its row is used
\ to index anything, so a stale, forged, or out-of-range id throws instead of reading.
\
\ Adapter seam (ADOPT). PTXTC discovers facts about the host: which ptxas is on the
\ path, what it reports as its version, which driver is loaded, what flags we assemble
\ with. Those facts are *audited input*, not identity - PTXTC stays a path/assembler
\ boundary and does not allocate ids. TOOLCHAIN:disc carries all five facts as one
\ checked PRODUCT and ADOPT validates that value before any identity exists: a missing
\ fact is TOOLCHAIN:E-FACT, a compiler or driver outside the audited domain is
\ TOOLCHAIN:E-KIND. There are no independently mutable discovery slots, so a failed or
\ partial round cannot leave facts for the next attempt to mix.
\
\ Capacities are derived, not guessed: the canonical form of a maximal descriptor must
\ fit the string builder (or a valid bounded fact would leak a foreign E-STR-CAPACITY
\ instead of a named toolchain error), and the arena must hold TC-CAP maximal rows (so
\ arena exhaustion is unreachable from DEFINE/ADOPT). LAYOUT-CK proves both at load.
\
\ Fail closed: a stale or forged id, an exhausted generation, a malformed digest, an
\ unknown digest, a digest that collides across two canonical forms, an incomplete
\ discovery, and a full table are all named throws.
\ maki -> habu only; TOOLCHAIN owns -5260..-5267.

require lib/prelude.f
require lib/string.f
require lib/fs.f                  \ BASENAME: the compiler fact is a path
require maki/cad-kinds.f

package TOOLCHAIN
public

-5260 constant E-FACT     \ discovery fact missing/empty: no identity is derivable
-5261 constant E-KIND     \ compiler/driver outside the audited domain
-5262 constant E-CAP      \ table, arena, fact, or destination capacity exceeded
-5263 constant E-ID       \ identity stale, forged, or out of range
-5264 constant E-DIGEST   \ canonical digest text malformed (not 16 hex digits)
-5265 constant E-MISS     \ canonical digest names no interned toolchain
-5266 constant E-COLLIDE  \ two distinct canonical forms share one digest
-5267 constant E-EPOCH    \ no further RESET can retire every outstanding identity

\ A compiler kind and a driver kind are both one cell and must never substitute
\ for each other, nor for a raw count.
TYPEFAMILY compiler 0
TYPEFAMILY driver 0

\ Discovery is one typed value, not five independently mutable global slots. A
\ producer must present one complete round to ADOPT, so failed or partial rounds
\ cannot leave facts for a later call to mix.
PRODUCT disc 0
   FIELD compiler-path ptr u8
   FIELD compiler-path-len n
   FIELD compiler-version ptr u8
   FIELD compiler-version-len n
   FIELD driver-name ptr u8
   FIELD driver-name-len n
   FIELD driver-version ptr u8
   FIELD driver-version-len n
   FIELD config ptr u8
   FIELD config-len n
;PRODUCT

private

\ ---- audited kind domains ---------------------------------------------------
\ The only assembler we run is ptxas; the only driver we run against is CUDA.
\ A discovery fact naming anything else is a toolchain we cannot identify.
0 ENUM+ CC-PTXAS
  constant CC-N

0 ENUM+ DRV-CUDA
  constant DRV-N

\ ---- private identity refinement (never published) ---------------------------
TRUSTED: RAW>TC  ( n -- CAD-KIND:toolchain-id ) ;
TRUSTED: TC>RAW  ( CAD-KIND:toolchain-id -- n ) ;
TRUSTED: RAW>CC  ( n -- TOOLCHAIN:compiler ) ;
TRUSTED: CC>RAW  ( TOOLCHAIN:compiler -- n ) ;
TRUSTED: RAW>DRV ( n -- TOOLCHAIN:driver ) ;
TRUSTED: DRV>RAW ( TOOLCHAIN:driver -- n ) ;

\ ---- primary caps -------------------------------------------------------------
\ These two are the knobs. Everything else in the layout is derived from them, so
\ raising a cap cannot silently outgrow the builder, the arena, or the id encoding.
$20   constant TC-CAP          \ interned toolchain identities
$100  constant FACT-CAP        \ longest single discovery fact

$10   constant KIND-NAME-CAP   \ longest audited kind name render
$10   constant DIGEST-LEN      \ canonical digest render: 16 hex digits
$4    constant LEN-DIGITS      \ a framed field's length prefix: 4 hex digits
$3A   constant FRAME-SEP       \ ':' - ends a length prefix, begins the field bytes

\ the longest field a length prefix can frame
1 LEN-DIGITS 4 * lshift 1- constant LEN-LIMIT

\ ---- id encoding: generation in the high bits, row index in the low bits --------
$10 constant TC-IX-BITS
1 TC-IX-BITS lshift 1- constant TC-IX-MASK      \ the row an id names
TC-IX-MASK 1+          constant TC-IX-LIMIT     \ rows an id can address at all
$20 constant TC-GEN-BITS
1 TC-GEN-BITS lshift 1- constant TC-GEN-MAX     \ last generation; RESET past it throws

\ ---- canonical field tags -------------------------------------------------------
: TAG-CC$     ( -- ptr u8 n )  s" cc=" ;
: TAG-VER$    ( -- ptr u8 n )  s" ;ver=" ;
: TAG-DRV$    ( -- ptr u8 n )  s" ;drv=" ;
: TAG-DRVVER$ ( -- ptr u8 n )  s" ;drvver=" ;
: TAG-CFG$    ( -- ptr u8 n )  s" ;cfg=" ;

5 constant CANON-FIELDS   \ cc, ver, drv, drvver, cfg
2 constant CANON-KINDS    \ cc and drv render an audited kind name
3 constant CANON-FACTS    \ ver, drvver and cfg each render a discovery fact

\ derived from the tags themselves, so renaming a tag cannot desync the cap
TAG-CC$ nip  TAG-VER$ nip +  TAG-DRV$ nip +  TAG-DRVVER$ nip +  TAG-CFG$ nip +
   constant CANON-TAG-BYTES

\ the longest canonical form a valid bounded descriptor can render
CANON-TAG-BYTES
CANON-FIELDS LEN-DIGITS 1+ *  +      \ each field: its length prefix and FRAME-SEP
CANON-KINDS  KIND-NAME-CAP *  +
CANON-FACTS  FACT-CAP *       +
   constant CANON-CAP

\ arena bytes one interned row can need: its canonical form, its three fact copies,
\ and its digest render
CANON-CAP  CANON-FACTS FACT-CAP *  +  DIGEST-LEN +  constant TC-ROW-CAP

\ sized for TC-CAP maximal rows, which is what makes arena exhaustion unreachable
\ from DEFINE / ADOPT
TC-CAP TC-ROW-CAP * constant TC-ARENA-CAP

\ ---- descriptor table (one array per field; rows are immutable once written) --
create TC-ARENA TC-ARENA-CAP allot   variable TC-ARENA-U

create TC-CC   TC-CAP cells allot    \ compiler kind (raw)
create TC-DRV  TC-CAP cells allot    \ driver kind (raw)
create TC-DIG  TC-CAP cells allot    \ canonical digest (FNV-1a 64)
create TC-CVO  TC-CAP cells allot    \ compiler version span
create TC-CVU  TC-CAP cells allot
create TC-DVO  TC-CAP cells allot    \ driver version span
create TC-DVU  TC-CAP cells allot
create TC-CFO  TC-CAP cells allot    \ config span
create TC-CFU  TC-CAP cells allot
create TC-CNO  TC-CAP cells allot    \ canonical form span
create TC-CNU  TC-CAP cells allot
create TC-DGO  TC-CAP cells allot    \ canonical digest render span
create TC-DGU  TC-CAP cells allot
variable TC-N
variable TC-GEN

\ ---- descriptor staging (DEFINE's own record; the adapter has a separate one) --
variable ST-CC
variable ST-DRV
create ST-CVER FACT-CAP allot   variable ST-CVER-U
create ST-DVER FACT-CAP allot   variable ST-DVER-U
create ST-CFG  FACT-CAP allot   variable ST-CFG-U

\ ---- fact copy-in: a fact is present and fits, or there is no identity --------
: FACT! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u:n dst:ptr lenp:ptr :}
   u 0 <= if E-FACT throw then
   u FACT-CAP > if E-CAP throw then
   a dst u BYTE-COPY
   u lenp ! ;

: ST-CVER$ ( -- ptr u8 n )  ST-CVER ST-CVER-U @ ;
: ST-DVER$ ( -- ptr u8 n )  ST-DVER ST-DVER-U @ ;
: ST-CFG$  ( -- ptr u8 n )  ST-CFG  ST-CFG-U  @ ;

\ ---- audited kind names (the canonical spelling of each kind) -----------------
: CC-NAME$ ( n -- ptr u8 n )
   case
      CC-PTXAS of s" ptxas" endof
      E-KIND throw
   endcase ;

: DRV-NAME$ ( n -- ptr u8 n )
   case
      DRV-CUDA of s" cuda" endof
      E-KIND throw
   endcase ;

\ ---- the derived layout must actually hold ------------------------------------
\ Run at load, so a future edit to FACT-CAP / TC-CAP / LEN-DIGITS / a kind name that
\ breaks an assumption fails the load rather than leaking a foreign error (or an
\ unframable field) at run time.
: NAMES-CK ( -- )                          \ every audited kind name must be framable
   CC-N 0 ?do
      i CC-NAME$ nip KIND-NAME-CAP > if E-CAP throw then
   loop
   DRV-N 0 ?do
      i DRV-NAME$ nip KIND-NAME-CAP > if E-CAP throw then
   loop ;

: LAYOUT-CK ( -- )
   CANON-CAP SB-CAP > if E-CAP throw then      \ the canonical form renders through SB
   FACT-CAP LEN-LIMIT > if E-CAP throw then    \ every field length is expressible
   KIND-NAME-CAP LEN-LIMIT > if E-CAP throw then
   TC-CAP TC-IX-LIMIT > if E-ID throw then     \ every row index fits an id
   NAMES-CK ;

LAYOUT-CK
1 TC-GEN !                                 \ generation 0 names no id, so a zero cell is not one

\ ---- kind validation + refinement --------------------------------------------
: CC-CK ( TOOLCHAIN:compiler -- n )
   CC>RAW dup 0 < over CC-N >= or if E-KIND throw then ;

: DRV-CK ( TOOLCHAIN:driver -- n )
   DRV>RAW dup 0 < over DRV-N >= or if E-KIND throw then ;

\ a compiler fact is a path: its basename must name an assembler we audited
: PATH>CC ( ptr u8 n -- TOOLCHAIN:compiler )
   BASENAME s" ptxas" STR= 0= if E-KIND throw then
   CC-PTXAS RAW>CC ;

: NAME>DRV ( ptr u8 n -- TOOLCHAIN:driver )
   s" cuda" STR= 0= if E-KIND throw then
   DRV-CUDA RAW>DRV ;

\ ---- id validation + refinement ----------------------------------------------
\ An id carries the generation it was issued under. Both directions validate before
\ any row is indexed: a retired generation, a forged high bit, or a row past the live
\ table throws rather than reading a row that is not the one the id was issued for.
: ROW>ID ( n -- CAD-KIND:toolchain-id ) {: ix:n :}
   ix 0 < ix TC-N @ >= or if E-ID throw then
   TC-GEN @ TC-IX-BITS lshift  ix or  RAW>TC ;

: ID>ROW ( CAD-KIND:toolchain-id -- n ) {: id:CAD-KIND:toolchain-id :}
   id TC>RAW {: v:n :}
   v TC-IX-BITS rshift TC-GEN @ <> if E-ID throw then   \ retired generation, or forged
   v TC-IX-MASK and {: ix:n :}
   ix TC-N @ >= if E-ID throw then                      \ past the live table
   ix ;

\ ---- interned descriptor text -------------------------------------------------
\ The arena holds TC-CAP maximal rows and COMMIT refuses a TC-CAP+1'th row, so this
\ guard is unreachable through DEFINE / ADOPT. It stays because the bound is what makes
\ that true, and the test drives it directly through the private seam.
: INTERN ( ptr u8 n -- n n ) {: a:ptr u:n :}
   TC-ARENA-U @ u + TC-ARENA-CAP > if E-CAP throw then
   TC-ARENA-U @ {: off:n :}
   a  TC-ARENA off +  u BYTE-COPY
   off u + TC-ARENA-U !
   off u ;

: SPAN$ ( n n -- ptr u8 n ) {: off:n u:n :}
   TC-ARENA off + u ;

: COPY-OUT ( ptr u8 n ptr u8 n -- n ) {: src:ptr u:n dst:ptr cap:n :}
   u cap > if E-CAP throw then
   src dst u BYTE-COPY
   u ;

\ ---- row fields ---------------------------------------------------------------
: CC!    ( n n -- ) {: v:n ix:n :}  v ix cells TC-CC  + ! ;
: DRV!   ( n n -- ) {: v:n ix:n :}  v ix cells TC-DRV + ! ;
: DIG!   ( n n -- ) {: v:n ix:n :}  v ix cells TC-DIG + ! ;
: CVER!  ( n n n -- ) {: off:n u:n ix:n :}
   off ix cells TC-CVO + !   u ix cells TC-CVU + ! ;
: DVER!  ( n n n -- ) {: off:n u:n ix:n :}
   off ix cells TC-DVO + !   u ix cells TC-DVU + ! ;
: CFG!   ( n n n -- ) {: off:n u:n ix:n :}
   off ix cells TC-CFO + !   u ix cells TC-CFU + ! ;
: CANON! ( n n n -- ) {: off:n u:n ix:n :}
   off ix cells TC-CNO + !   u ix cells TC-CNU + ! ;
: DIGHEX! ( n n n -- ) {: off:n u:n ix:n :}
   off ix cells TC-DGO + !   u ix cells TC-DGU + ! ;

: CC@    ( n -- n ) cells TC-CC  + @ ;
: DRV@   ( n -- n ) cells TC-DRV + @ ;
: DIG@   ( n -- n ) cells TC-DIG + @ ;
: CVER$  ( n -- ptr u8 n ) {: ix:n :}
   ix cells TC-CVO + @  ix cells TC-CVU + @  SPAN$ ;
: DVER$  ( n -- ptr u8 n ) {: ix:n :}
   ix cells TC-DVO + @  ix cells TC-DVU + @  SPAN$ ;
: CFG$   ( n -- ptr u8 n ) {: ix:n :}
   ix cells TC-CFO + @  ix cells TC-CFU + @  SPAN$ ;
: CANON$ ( n -- ptr u8 n ) {: ix:n :}
   ix cells TC-CNO + @  ix cells TC-CNU + @  SPAN$ ;
: DIGHEX$ ( n -- ptr u8 n ) {: ix:n :}
   ix cells TC-DGO + @  ix cells TC-DGU + @  SPAN$ ;

\ ---- hex render ----------------------------------------------------------------
: HEX-NIB ( n -- n )  $F and dup 10 < if $30 + else $37 + then ;

: HEX+ ( n n -- ) {: v:n nd:n :}           \ v as nd hex digits, MSB first
   nd 0 ?do  v  nd 1- i - 4 * rshift HEX-NIB SB-APPEND-C  loop ;

\ ---- canonical form -------------------------------------------------------------
\ Each field is framed by its own exact byte length, so a field's content cannot forge
\ another field's boundary: a reader takes LEN-DIGITS hex digits, then FRAME-SEP, then
\ exactly that many bytes. Field order is fixed, so the same facts always render the
\ same bytes and different facts never render the same bytes. This text is what gets
\ digested, and it is kept per row as the audit surface for the id.
: FIELD+ ( ptr u8 n ptr u8 n -- ) {: t:ptr tu:n a:ptr u:n :}
   t tu SB-APPEND
   u LEN-DIGITS HEX+
   FRAME-SEP SB-APPEND-C
   a u SB-APPEND ;

: CANON+ ( -- )
   TAG-CC$     ST-CC  @ CC-NAME$   FIELD+
   TAG-VER$    ST-CVER$            FIELD+
   TAG-DRV$    ST-DRV @ DRV-NAME$  FIELD+
   TAG-DRVVER$ ST-DVER$            FIELD+
   TAG-CFG$    ST-CFG$             FIELD+ ;

\ ---- FNV-1a 64 over the canonical form ----------------------------------------
$cbf29ce484222325 constant FNV-BASIS
$100000001b3      constant FNV-PRIME

: FNV-BYTE ( n n -- n )  xor FNV-PRIME * ;

: FNV$ ( n ptr u8 n -- n ) {: h:n a:ptr u:n :}
   h
   u 0 ?do  a i + c@ FNV-BYTE  loop ;

: DIGEST ( -- n )                          \ digest the canonical form now in SB
   FNV-BASIS SB$ FNV$ ;

\ ---- digest parse (16 hex digits, MSB first) -----------------------------------
: NIB-CK ( n -- ) {: c:n :}
   c $30 >= c $39 <= and if exit then
   c $41 >= c $46 <= and if exit then
   c $61 >= c $66 <= and if exit then
   E-DIGEST throw ;

: NIB> ( n -- n ) {: c:n :}
   c NIB-CK
   c $39 <= if c $30 - exit then
   c $46 <= if c $41 - 10 + exit then
   c $61 - 10 + ;

: HEX> ( ptr u8 n -- n ) {: a:ptr u:n :}
   u DIGEST-LEN <> if E-DIGEST throw then
   0
   u 0 ?do  4 lshift  a i + c@ NIB> or  loop ;

\ ---- lookup by digest ----------------------------------------------------------
: FIND-DIG ( n -- n ) {: dig:n :}          \ digest value -> row index, or -1
   TC-N @ 0 ?do  i DIG@ dig = if i unloop exit then  loop  -1 ;

\ A digest hit is a claim, not a proof. FNV-1a 64 makes an accidental collision
\ vanishingly unlikely, but vanishingly unlikely is not impossible, and handing back
\ an identity that names a *different* toolchain is exactly the mis-identification
\ this owner exists to prevent. So the hit is verified against the canonical form
\ it claims to name, and a broken assumption fails loudly instead of silently.
: HIT-AGREES? ( n ptr u8 n -- bool ) {: hit:n a:ptr u:n :}
   hit CANON$ a u STR= ;

: DIG-HIT ( n -- n ) {: hit:n :}           \ verified row index of a digest hit
   hit SB$ HIT-AGREES? 0= if E-COLLIDE throw then
   hit ;

\ ---- commit a new row (only reached when the digest is not already interned) ----
\ Every descriptor field is copied into the arena, digest render included, so each
\ projection hands back a stable immutable span. Order matters: the canonical form
\ is still live in SB, so intern it before the digest render reuses the builder.
: COMMIT ( n -- CAD-KIND:toolchain-id ) {: dig:n :}
   TC-N @ TC-CAP >= if E-CAP throw then
   TC-N @ {: ix:n :}
   SB$       INTERN ix CANON!
   ST-CVER$  INTERN ix CVER!
   ST-DVER$  INTERN ix DVER!
   ST-CFG$   INTERN ix CFG!
   SB-RESET dig DIGEST-LEN HEX+ SB$ INTERN ix DIGHEX!
   ST-CC  @ ix CC!
   ST-DRV @ ix DRV!
   dig      ix DIG!
   ix 1+ TC-N !
   ix ROW>ID ;

\ ---- staging: written whole by STAGE, consumed by exactly one DEFINE ------------
: STAGE ( TOOLCHAIN:compiler ptr u8 n TOOLCHAIN:driver ptr u8 n ptr u8 n -- )
   {: cc:TOOLCHAIN:compiler cv:ptr cvu:n drv:TOOLCHAIN:driver dv:ptr dvu:n cf:ptr cfu:n :}
   cc  CC-CK  ST-CC  !
   drv DRV-CK ST-DRV !
   cv cvu ST-CVER ST-CVER-U FACT!
   dv dvu ST-DVER ST-DVER-U FACT!
   cf cfu ST-CFG  ST-CFG-U  FACT! ;

\ the fact lengths are what make a staged DEFINE fact present; the kind cells are
\ never read without a preceding STAGE writing them
: STAGE-RESET ( -- )
   0 ST-CVER-U !  0 ST-DVER-U !  0 ST-CFG-U ! ;

public

\ ---- audited kinds -------------------------------------------------------------
: PTXAS ( -- TOOLCHAIN:compiler )  CC-PTXAS RAW>CC ;
: CUDA  ( -- TOOLCHAIN:driver )    DRV-CUDA RAW>DRV ;

: COMPILER-NAME$ ( TOOLCHAIN:compiler -- ptr u8 n )  CC-CK  CC-NAME$ ;
: DRIVER-NAME$   ( TOOLCHAIN:driver   -- ptr u8 n )  DRV-CK DRV-NAME$ ;

: COMPILER= ( TOOLCHAIN:compiler TOOLCHAIN:compiler -- bool )
   {: a:TOOLCHAIN:compiler b:TOOLCHAIN:compiler :}
   a CC>RAW b CC>RAW = ;

: DRIVER= ( TOOLCHAIN:driver TOOLCHAIN:driver -- bool )
   {: a:TOOLCHAIN:driver b:TOOLCHAIN:driver :}
   a DRV>RAW b DRV>RAW = ;

\ Two ids name the same toolchain when they are the same (generation, row). Ids from
\ retired generations therefore compare unequal to live ones, which is what they are.
: ID= ( CAD-KIND:toolchain-id CAD-KIND:toolchain-id -- bool )
   {: a:CAD-KIND:toolchain-id b:CAD-KIND:toolchain-id :}
   a TC>RAW b TC>RAW = ;

\ ---- definition ----------------------------------------------------------------
\ The typed constructor. Equal facts collapse to the identity already interned for
\ them; any changed version or config renders a different canonical form and so gets
\ its own identity. The staging record is consumed here, by exactly one DEFINE.
: DEFINE ( TOOLCHAIN:compiler ptr u8 n TOOLCHAIN:driver ptr u8 n ptr u8 n -- CAD-KIND:toolchain-id )
   STAGE
   SB-RESET CANON+
   DIGEST {: dig:n :}
   dig FIND-DIG {: hit:n :}
   hit 0 >= if  hit DIG-HIT ROW>ID  else  dig COMMIT  then
   STAGE-RESET ;

\ ---- the audited PTXTC discovery adapter ----------------------------------------
\ PTXTC (lib/ptx/toolchain.f) knows which ptxas it resolved and which flags it
\ assembles with; a driver probe knows the loaded driver. Those are facts, not
\ identity. The PRODUCT is the atomic staging record: ADOPT destructures one complete
\ value and owns no mutable discovery state that a failed round could leave behind.
: ADOPT ( TOOLCHAIN:disc -- CAD-KIND:toolchain-id )
   TOOLCHAIN-DISC:UNMAKE
   {: path:ptr pathu:n cv:ptr cvu:n name:ptr nameu:n dv:ptr dvu:n cfg:ptr cfgu:n :}
   path pathu PATH>CC
   cv cvu
   name nameu NAME>DRV
   dv dvu
   cfg cfgu
   DEFINE ;

\ ---- immutable typed projections -------------------------------------------------
: COMPILER@ ( CAD-KIND:toolchain-id -- TOOLCHAIN:compiler )  ID>ROW CC@  RAW>CC ;
: DRIVER@   ( CAD-KIND:toolchain-id -- TOOLCHAIN:driver )    ID>ROW DRV@ RAW>DRV ;

: VERSION+        ( CAD-KIND:toolchain-id -- )  ID>ROW CVER$   SB-APPEND ;
: DRIVER-VERSION+ ( CAD-KIND:toolchain-id -- )  ID>ROW DVER$   SB-APPEND ;
: CONFIG+         ( CAD-KIND:toolchain-id -- )  ID>ROW CFG$    SB-APPEND ;
: CANONICAL+      ( CAD-KIND:toolchain-id -- )  ID>ROW CANON$  SB-APPEND ;
: DIGEST+         ( CAD-KIND:toolchain-id -- )  ID>ROW DIGHEX$ SB-APPEND ;

: VERSION-COPY ( CAD-KIND:toolchain-id ptr u8 n -- n )
   {: id:CAD-KIND:toolchain-id dst:ptr cap:n :}
   id ID>ROW CVER$ dst cap COPY-OUT ;

: DRIVER-VERSION-COPY ( CAD-KIND:toolchain-id ptr u8 n -- n )
   {: id:CAD-KIND:toolchain-id dst:ptr cap:n :}
   id ID>ROW DVER$ dst cap COPY-OUT ;

: CONFIG-COPY ( CAD-KIND:toolchain-id ptr u8 n -- n )
   {: id:CAD-KIND:toolchain-id dst:ptr cap:n :}
   id ID>ROW CFG$ dst cap COPY-OUT ;

: CANONICAL-COPY ( CAD-KIND:toolchain-id ptr u8 n -- n )
   {: id:CAD-KIND:toolchain-id dst:ptr cap:n :}
   id ID>ROW CANON$ dst cap COPY-OUT ;

: DIGEST-COPY ( CAD-KIND:toolchain-id ptr u8 n -- n )
   {: id:CAD-KIND:toolchain-id dst:ptr cap:n :}
   id ID>ROW DIGHEX$ dst cap COPY-OUT ;

\ ---- typed lookup ------------------------------------------------------------------
\ Both take a canonical digest render. A string that is not one is not an unknown
\ toolchain, it is not a digest at all, so KNOWN? throws E-DIGEST rather than
\ answering false: a malformed name must not be reported as a legitimate miss.
: KNOWN? ( ptr u8 n -- bool )
   HEX> FIND-DIG 0 >= ;

: LOOKUP ( ptr u8 n -- CAD-KIND:toolchain-id )
   HEX> FIND-DIG {: hit:n :}
   hit 0 < if E-MISS throw then
   hit ROW>ID ;

: IDS ( -- n )  TC-N @ ;
: FACT-CAPACITY      ( -- n )  FACT-CAP ;
: ID-CAPACITY        ( -- n )  TC-CAP ;
: CANONICAL-CAPACITY ( -- n )  CANON-CAP ;
: DIGEST-SIZE        ( -- n )  DIGEST-LEN ;

\ Empties the table and retires every id issued under the current generation. The
\ generation does not wrap: past TC-GEN-MAX there is no generation left that can
\ retire the ids still held, so RESET throws rather than reissuing one.
: RESET ( -- )
   TC-GEN @ TC-GEN-MAX >= if E-EPOCH throw then
   TC-GEN @ 1+ TC-GEN !
   0 TC-N !
   0 TC-ARENA-U !
   STAGE-RESET ;

\ ---- audited test seam -----------------------------------------------------------
\ These probes expose outcomes, never raw refinements, WIDs, arena spans, or mutable
\ owner cells. They keep low-level invariant tests on the same bounded public seam.
private

variable TEST-ID

: TEST-DEF-A ( -- CAD-KIND:toolchain-id )
   PTXAS s" 12.6.85" CUDA s" 580.65.06" s" -arch=sm_87" DEFINE ;

: TEST-DEF-Z ( -- CAD-KIND:toolchain-id )
   PTXAS s" 99.9" CUDA s" 1.0" s" -arch=sm_90" DEFINE ;

: TEST-STALE-DO ( -- )
   RESET TEST-DEF-A TC>RAW TEST-ID !
   RESET TEST-DEF-Z drop
   TEST-ID @ RAW>TC ID>ROW drop ;

: TEST-FORGE-GEN-DO ( -- )
   TC-GEN @ 1+ TC-IX-BITS lshift RAW>TC ID>ROW drop ;

: TEST-FORGE-ROW-DO ( -- )
   TC-GEN @ TC-IX-BITS lshift TC-N @ or RAW>TC ID>ROW drop ;

: TEST-ROW-NEG-DO  ( -- )  -1 ROW>ID drop ;
: TEST-ROW-HIGH-DO ( -- )  TC-N @ ROW>ID drop ;

: TEST-COLLIDE-DO ( -- )
   RESET TEST-DEF-A drop
   SB-RESET
   s" cc=0005:ptxas;ver=0003:9.9;drv=0004:cuda;drvver=0001:1;cfg=0001:x" SB-APPEND
   0 DIG-HIT drop ;

: TEST-ARENA-DO ( -- )
   TC-ARENA-CAP TC-ARENA-U !
   s" x" INTERN 2drop ;

public

: TEST-REFINEMENTS? ( CAD-KIND:toolchain-id -- bool )
   {: id:CAD-KIND:toolchain-id :}
   id ID>ROW ROW>ID id ID=
   PTXAS CC-CK 0 = and
   CUDA DRV-CK 0 = and ;

: TEST-HIT-AGREES? ( CAD-KIND:toolchain-id ptr u8 n -- bool )
   {: id:CAD-KIND:toolchain-id a:ptr u:n :}
   id ID>ROW a u HIT-AGREES? ;

: TEST-STALE-RC      ( -- n )  [: TEST-STALE-DO ;]     catch ;
: TEST-FORGE-GEN-RC  ( -- n )  [: TEST-FORGE-GEN-DO ;] catch ;
: TEST-FORGE-ROW-RC  ( -- n )  [: TEST-FORGE-ROW-DO ;] catch ;
: TEST-ROW-NEG-RC    ( -- n )  [: TEST-ROW-NEG-DO ;]   catch ;
: TEST-ROW-HIGH-RC   ( -- n )  [: TEST-ROW-HIGH-DO ;]  catch ;
: TEST-COLLIDE-RC    ( -- n )  [: TEST-COLLIDE-DO ;]   catch ;

: TEST-ARENA-RC ( -- n )
   [: TEST-ARENA-DO ;] catch {: rc:n :}
   0 TC-ARENA-U !
   rc ;

: TEST-EPOCH-RC ( -- n )
   TC-GEN @ {: old:n :}
   TC-GEN-MAX TC-GEN !
   [: RESET ;] catch {: rc:n :}
   old TC-GEN !
   rc ;

;package
