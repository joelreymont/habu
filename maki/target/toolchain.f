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
\ Identity refinement is private. RAW>TC / TC>RAW never leave this file, so no caller
\ can turn an `n` into a CAD-KIND:toolchain-id (or read one back out as `n`) - the
\ only ways in are DEFINE (typed facts) and ADOPT (audited discovery facts), and the
\ only way to name an existing one is LOOKUP over its canonical digest. A target-id
\ and a toolchain-id are both one cell and the checker keeps them apart; so do a
\ compiler kind and a driver kind (TOOLCHAIN:compiler, TOOLCHAIN:driver).
\
\ Adapter seam (ADOPT). PTXTC discovers facts about the host: which ptxas is on the
\ path, what it reports as its version, which driver is loaded, what flags we assemble
\ with. Those facts are *audited input*, not identity - PTXTC stays a path/assembler
\ boundary and does not allocate ids. The adapter stages the five facts explicitly
\ (FACT-COMPILER-PATH! .. FACT-CONFIG!) and ADOPT validates them before any identity
\ exists: a missing fact is E-TC-FACT, a compiler or driver outside the audited domain
\ is E-TC-KIND. Incomplete discovery therefore cannot produce a toolchain identity -
\ it fails closed, which is what keeps "unprobed" from becoming a legitimate identity.
\
\ Fail closed: a stale id (used after RESET), a malformed digest, an unknown digest,
\ a digest that collides across two canonical forms, and a full table/arena are all
\ named throws. maki -> habu only; toolchain owns -5260..-5266.

require lib/prelude.f
require lib/string.f
require lib/fs.f                  \ BASENAME: the compiler fact is a path
require maki/cad-kinds.f

-5260 constant E-TC-FACT     \ discovery fact missing/empty: no identity is derivable
-5261 constant E-TC-KIND     \ compiler/driver outside the audited domain
-5262 constant E-TC-CAP      \ toolchain table / string arena capacity exceeded
-5263 constant E-TC-ID       \ toolchain id outside the live table (stale after RESET)
-5264 constant E-TC-DIGEST   \ canonical digest text malformed (not 16 hex digits)
-5265 constant E-TC-MISS     \ canonical digest names no interned toolchain
-5266 constant E-TC-COLLIDE  \ two distinct canonical forms share one digest

package TOOLCHAIN
public

\ A compiler kind and a driver kind are both one cell and must never substitute
\ for each other, nor for a raw count.
TYPEFAMILY compiler 0
TYPEFAMILY driver 0

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

\ ---- descriptor table (one array per field; rows are immutable once written) --
$20   constant TC-CAP          \ interned toolchain identities
$2000 constant TC-ARENA-CAP    \ interned descriptor text (versions, config, canon)
$100  constant FACT-CAP        \ longest single discovery fact
$10   constant DIGEST-LEN      \ canonical digest render: 16 hex digits

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

\ ---- descriptor staging (DEFINE's own record; the adapter has a separate one) --
variable ST-CC
variable ST-DRV
create ST-CVER FACT-CAP allot   variable ST-CVER-U
create ST-DVER FACT-CAP allot   variable ST-DVER-U
create ST-CFG  FACT-CAP allot   variable ST-CFG-U

\ ---- discovery-fact staging (the audited PTXTC adapter seam) ------------------
create FT-PATH  FACT-CAP allot   variable FT-PATH-U    \ compiler path (ptxas)
create FT-CVER  FACT-CAP allot   variable FT-CVER-U    \ compiler version
create FT-DNAME FACT-CAP allot   variable FT-DNAME-U   \ driver name (cuda)
create FT-DVER  FACT-CAP allot   variable FT-DVER-U    \ driver version
create FT-CFG   FACT-CAP allot   variable FT-CFG-U     \ assembler config

\ ---- fact copy-in: a fact is present and fits, or there is no identity --------
: FACT! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u:n dst:ptr lenp:ptr :}
   u 0 <= if E-TC-FACT throw then
   u FACT-CAP > if E-TC-CAP throw then
   a dst u BYTE-COPY
   u lenp ! ;

: FACT-CK ( ptr n -- ) {: lenp:ptr :}      \ a never-set fact is an incomplete fact
   lenp @ 0 <= if E-TC-FACT throw then ;

: ST-CVER$ ( -- ptr u8 n )  ST-CVER ST-CVER-U @ ;
: ST-DVER$ ( -- ptr u8 n )  ST-DVER ST-DVER-U @ ;
: ST-CFG$  ( -- ptr u8 n )  ST-CFG  ST-CFG-U  @ ;

: FT-PATH$  ( -- ptr u8 n )  FT-PATH  FT-PATH-U  @ ;
: FT-CVER$  ( -- ptr u8 n )  FT-CVER  FT-CVER-U  @ ;
: FT-DNAME$ ( -- ptr u8 n )  FT-DNAME FT-DNAME-U @ ;
: FT-DVER$  ( -- ptr u8 n )  FT-DVER  FT-DVER-U  @ ;
: FT-CFG$   ( -- ptr u8 n )  FT-CFG   FT-CFG-U   @ ;

\ ---- audited kind names (the canonical spelling of each kind) -----------------
: CC-NAME$ ( n -- ptr u8 n )
   case
      CC-PTXAS of s" ptxas" endof
      E-TC-KIND throw
   endcase ;

: DRV-NAME$ ( n -- ptr u8 n )
   case
      DRV-CUDA of s" cuda" endof
      E-TC-KIND throw
   endcase ;

\ ---- kind validation + refinement --------------------------------------------
: CC-CK ( TOOLCHAIN:compiler -- n )
   CC>RAW dup 0 < over CC-N >= or if E-TC-KIND throw then ;

: DRV-CK ( TOOLCHAIN:driver -- n )
   DRV>RAW dup 0 < over DRV-N >= or if E-TC-KIND throw then ;

\ a compiler fact is a path: its basename must name an assembler we audited
: PATH>CC ( ptr u8 n -- TOOLCHAIN:compiler )
   BASENAME s" ptxas" STR= 0= if E-TC-KIND throw then
   CC-PTXAS RAW>CC ;

: NAME>DRV ( ptr u8 n -- TOOLCHAIN:driver )
   s" cuda" STR= 0= if E-TC-KIND throw then
   DRV-CUDA RAW>DRV ;

\ ---- id validation + refinement ----------------------------------------------
: ID-CK ( CAD-KIND:toolchain-id -- n )
   TC>RAW dup 0 < over TC-N @ >= or if E-TC-ID throw then ;

: RAW-ID ( n -- CAD-KIND:toolchain-id )
   dup 0 < over TC-N @ >= or if E-TC-ID throw then
   RAW>TC ;

\ ---- interned descriptor text -------------------------------------------------
: INTERN ( ptr u8 n -- n n ) {: a:ptr u:n :}
   TC-ARENA-U @ u + TC-ARENA-CAP > if E-TC-CAP throw then
   TC-ARENA-U @ {: off:n :}
   a  TC-ARENA off +  u BYTE-COPY
   off u + TC-ARENA-U !
   off u ;

: SPAN$ ( n n -- ptr u8 n ) {: off:n u:n :}
   TC-ARENA off + u ;

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

\ ---- canonical form -----------------------------------------------------------
\ Field order is fixed, so the same facts always render the same bytes. This text
\ is what gets digested, and it is kept per row as the audit surface for the id.
: CANON+ ( -- )
   s" cc="      SB-APPEND   ST-CC  @ CC-NAME$  SB-APPEND
   s" ;ver="    SB-APPEND   ST-CVER$          SB-APPEND
   s" ;drv="    SB-APPEND   ST-DRV @ DRV-NAME$ SB-APPEND
   s" ;drvver=" SB-APPEND   ST-DVER$          SB-APPEND
   s" ;cfg="    SB-APPEND   ST-CFG$           SB-APPEND ;

\ ---- FNV-1a 64 over the canonical form ----------------------------------------
$cbf29ce484222325 constant FNV-BASIS
$100000001b3      constant FNV-PRIME

: FNV-BYTE ( n n -- n )  xor FNV-PRIME * ;

: FNV$ ( n ptr u8 n -- n ) {: h:n a:ptr u:n :}
   h
   u 0 ?do  a i + c@ FNV-BYTE  loop ;

: DIGEST ( -- n )                          \ digest the canonical form now in SB
   FNV-BASIS SB$ FNV$ ;

\ ---- digest render / parse (16 hex digits, MSB first) --------------------------
: HEX-NIB ( n -- n )  $F and dup 10 < if $30 + else $37 + then ;

: HEX+ ( n -- ) {: v:n :}
   DIGEST-LEN 0 ?do  v  DIGEST-LEN 1- i - 4 * rshift HEX-NIB SB-APPEND-C  loop ;

: NIB-CK ( n -- ) {: c:n :}
   c $30 >= c $39 <= and if exit then
   c $41 >= c $46 <= and if exit then
   c $61 >= c $66 <= and if exit then
   E-TC-DIGEST throw ;

: NIB> ( n -- n ) {: c:n :}
   c NIB-CK
   c $39 <= if c $30 - exit then
   c $46 <= if c $41 - 10 + exit then
   c $61 - 10 + ;

: HEX> ( ptr u8 n -- n ) {: a:ptr u:n :}
   u DIGEST-LEN <> if E-TC-DIGEST throw then
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
   hit SB$ HIT-AGREES? 0= if E-TC-COLLIDE throw then
   hit ;

\ ---- commit a new row (only reached when the digest is not already interned) ----
\ Every descriptor field is copied into the arena, digest render included, so each
\ projection hands back a stable immutable span. Order matters: the canonical form
\ is still live in SB, so intern it before the digest render reuses the builder.
: COMMIT ( n -- CAD-KIND:toolchain-id ) {: dig:n :}
   TC-N @ TC-CAP >= if E-TC-CAP throw then
   TC-N @ {: ix:n :}
   SB$       INTERN ix CANON!
   ST-CVER$  INTERN ix CVER!
   ST-DVER$  INTERN ix DVER!
   ST-CFG$   INTERN ix CFG!
   SB-RESET dig HEX+ SB$ INTERN ix DIGHEX!
   ST-CC  @ ix CC!
   ST-DRV @ ix DRV!
   dig      ix DIG!
   ix 1+ TC-N !
   ix RAW>TC ;

: STAGE ( TOOLCHAIN:compiler ptr u8 n TOOLCHAIN:driver ptr u8 n ptr u8 n -- )
   {: cc:TOOLCHAIN:compiler cv:ptr cvu:n drv:TOOLCHAIN:driver dv:ptr dvu:n cf:ptr cfu:n :}
   cc  CC-CK  ST-CC  !
   drv DRV-CK ST-DRV !
   cv cvu ST-CVER ST-CVER-U FACT!
   dv dvu ST-DVER ST-DVER-U FACT!
   cf cfu ST-CFG  ST-CFG-U  FACT! ;

\ every adapter fact must be present before any identity is derived from them
: FACTS-CK ( -- )
   FT-PATH-U  FACT-CK
   FT-CVER-U  FACT-CK
   FT-DNAME-U FACT-CK
   FT-DVER-U  FACT-CK
   FT-CFG-U   FACT-CK ;

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

: ID= ( CAD-KIND:toolchain-id CAD-KIND:toolchain-id -- bool )
   {: a:CAD-KIND:toolchain-id b:CAD-KIND:toolchain-id :}
   a TC>RAW b TC>RAW = ;

\ ---- definition ----------------------------------------------------------------
\ The typed constructor. Equal facts collapse to the identity already interned for
\ them; any changed version or config renders a different canonical form and so gets
\ its own identity.
: DEFINE ( TOOLCHAIN:compiler ptr u8 n TOOLCHAIN:driver ptr u8 n ptr u8 n -- CAD-KIND:toolchain-id )
   STAGE
   SB-RESET CANON+
   DIGEST {: dig:n :}
   dig FIND-DIG {: hit:n :}
   hit 0 >= if hit DIG-HIT RAW-ID exit then
   dig COMMIT ;

\ ---- the audited PTXTC discovery adapter ----------------------------------------
\ PTXTC (lib/ptx/toolchain.f) knows which ptxas it resolved and which flags it
\ assembles with; a driver probe knows the loaded driver. Those are facts, not
\ identity. Stage them here and ADOPT turns them into one - or refuses to.
: FACTS-RESET ( -- )
   0 FT-PATH-U !  0 FT-CVER-U !  0 FT-DNAME-U !
   0 FT-DVER-U !  0 FT-CFG-U ! ;

: FACT-COMPILER-PATH!    ( ptr u8 n -- )  FT-PATH  FT-PATH-U  FACT! ;
: FACT-COMPILER-VERSION! ( ptr u8 n -- )  FT-CVER  FT-CVER-U  FACT! ;
: FACT-DRIVER-NAME!      ( ptr u8 n -- )  FT-DNAME FT-DNAME-U FACT! ;
: FACT-DRIVER-VERSION!   ( ptr u8 n -- )  FT-DVER  FT-DVER-U  FACT! ;
: FACT-CONFIG!           ( ptr u8 n -- )  FT-CFG   FT-CFG-U   FACT! ;

: ADOPT ( -- CAD-KIND:toolchain-id )
   FACTS-CK
   FT-PATH$  PATH>CC
   FT-CVER$
   FT-DNAME$ NAME>DRV
   FT-DVER$
   FT-CFG$
   DEFINE ;

\ ---- typed projections -----------------------------------------------------------
: COMPILER@ ( CAD-KIND:toolchain-id -- TOOLCHAIN:compiler )  ID-CK CC@  RAW>CC ;
: DRIVER@   ( CAD-KIND:toolchain-id -- TOOLCHAIN:driver )    ID-CK DRV@ RAW>DRV ;

: VERSION$        ( CAD-KIND:toolchain-id -- ptr u8 n )  ID-CK CVER$ ;
: DRIVER-VERSION$ ( CAD-KIND:toolchain-id -- ptr u8 n )  ID-CK DVER$ ;
: CONFIG$         ( CAD-KIND:toolchain-id -- ptr u8 n )  ID-CK CFG$ ;
: CANONICAL$      ( CAD-KIND:toolchain-id -- ptr u8 n )  ID-CK CANON$ ;

\ The digest render is the identity's public name. It is interned with the rest of
\ the descriptor, so two digests can be held and compared at once - a render into
\ the shared string builder would alias the second call onto the first.
: DIGEST$ ( CAD-KIND:toolchain-id -- ptr u8 n )  ID-CK DIGHEX$ ;

\ ---- typed lookup ------------------------------------------------------------------
: KNOWN? ( ptr u8 n -- bool )
   HEX> FIND-DIG 0 >= ;

: LOOKUP ( ptr u8 n -- CAD-KIND:toolchain-id )
   HEX> FIND-DIG {: hit:n :}
   hit 0 < if E-TC-MISS throw then
   hit RAW>TC ;

: IDS ( -- n )  TC-N @ ;

: RESET ( -- )
   0 TC-N !  0 TC-ARENA-U ! ;

;package
