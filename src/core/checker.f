0 constant T-CON   1 constant T-VAR   2 constant T-PTR
3 constant S-ROW   4 constant S-PUSH
5 constant T-QUOT  6 constant T-ATOM  7 constant T-PARAM
-1 constant UNBOUND
\ --- growable checker arenas --------------------------------------------
\ Shared mmap primitives for the checker's process-local scratch stores. Each
\ store keeps a baked DATA "boot" buffer (stable address across snapshot) and
\ grows into anonymous mmap on demand. Build-time definitions never exceed the
\ boot cap, so a baked engine always bakes the boot pointer; growth is
\ runtime-only and process-local. Snapshot prepare repoints every store at its
\ boot buffer so no mmap address is ever persisted. Growth is geometric so
\ regrow copies each store O(log n) times over a load, not once per grain.
3 constant ARENA-PROT-RW
$1002 constant ARENA-MAP-ANON
-1 constant ARENA-ANON-FD
0 constant ARENA-OFF-ZERO
variable ARENA-CP-I   variable ARENA-UB-I

: ARENA-MMAP-RC ( n -- n )
   0 swap ARENA-PROT-RW ARENA-MAP-ANON ARENA-ANON-FD ARENA-OFF-ZERO mmap
   dup 0 < IF s" checker: arena mmap failed" 76 die THEN ;

TRUSTED: ARENA-RC>PTR ( n -- ptr a ) ;

: ARENA-ALLOC ( n -- ptr a )
   ARENA-MMAP-RC ARENA-RC>PTR ;

: ARENA-COPY ( ptr a ptr a n -- ) {: src:ptr dst:ptr n:n :}   \ n bytes, src->dst
   0 ARENA-CP-I !
   begin ARENA-CP-I @ CELL + n <= while
      src ARENA-CP-I @ + @ dst ARENA-CP-I @ + !
      ARENA-CP-I @ CELL + ARENA-CP-I !
   repeat
   begin ARENA-CP-I @ n < while
      src ARENA-CP-I @ + c@ dst ARENA-CP-I @ + c!
      ARENA-CP-I @ 1 + ARENA-CP-I !
   repeat ;

: ARENA-CELLS-UNBOUND ( ptr a n n -- ) {: base:ptr from:n to:n :}   \ set [from,to) UNBOUND
   from ARENA-UB-I !
   begin ARENA-UB-I @ to < while
      UNBOUND ARENA-UB-I @ cells base + !
      ARENA-UB-I @ 1 + ARENA-UB-I !
   repeat ;

: ARENA-CELLS-ZERO ( ptr a n n -- ) {: base:ptr from:n to:n :}   \ set [from,to) to 0
   from ARENA-UB-I !
   begin ARENA-UB-I @ to < while
      0 ARENA-UB-I @ cells base + !
      ARENA-UB-I @ 1 + ARENA-UB-I !
   repeat ;

\ ARENA-BYTES-GROW ( ptr a n n -- ptr a ) : alloc newbytes, copy oldbytes from
\ base, return the new base. For record/cell stores counted in raw elements the
\ callers reset counters per definition, so no tail init is needed.
: ARENA-BYTES-GROW ( ptr a n n -- ptr a ) {: base:ptr oldbytes:n newbytes:n :}
   newbytes ARENA-ALLOC {: nb:ptr :}
   base nb oldbytes ARENA-COPY
   nb ;

\ REG-GROW1 ( pvar oldbytes newbytes -- ) : grow the buffer held in pvar in place,
\ storing the relocated base back. Shared by every parallel-array registry grow.
: REG-GROW1 ( ptr a n n -- ) {: pv:ptr ob:n nb:n :}
   pv @ ob nb ARENA-BYTES-GROW pv ! ;

\ TV arena: the typevar pool plus every var-id-indexed map grows in lockstep
\ under one shared cap so a fresh var id is a valid index into all of them.
2048 constant MAXTV-INIT       \ initial typevar pool (grows on demand)
variable TV-CAP   MAXTV-INIT TV-CAP !
: MAXTV ( -- n ) TV-CAP @ ;    \ live cap; every var-id array is TV-CAP cells

create TVT-BOOT MAXTV-INIT cells allot      create RVT-BOOT MAXTV-INIT cells allot
create VRC-TV-BOOT MAXTV-INIT cells allot    create VRC-RV-BOOT MAXTV-INIT cells allot
create VRI-TV-BOOT MAXTV-INIT cells allot     create VRI-RV-BOOT MAXTV-INIT cells allot
create EC-TV-BOOT MAXTV-INIT cells allot      create EC-RV-BOOT MAXTV-INIT cells allot
create EI-TV-BOOT MAXTV-INIT cells allot      create EI-RV-BOOT MAXTV-INIT cells allot
\ TVK: per-type-var KIND (nominal-storage raw discipline, habu-nominal-storage-raw).
\ 0 = TVK-ANY (an ordinary polymorphic var); 1 = TVK-RAW (a var minted by a raw
\ storage definer -- here/,/create/variable/constant -- whose cell admits only a
\ plain scalar representation and must NEVER absorb a nominal atom, arity-0
\ family, layout, or nominal-bearing pointer). It rides the shared TV arena so a
\ fresh var id indexes it too, is zeroed (ANY) on grow/reset/trial-rollback, and
\ persists on stored effect records (EN.B on var nodes) across snapshot/AOT.
create TVK-BOOT MAXTV-INIT cells allot
variable TVT-P     variable RVT-P
variable VRC-TV-P  variable VRC-RV-P   variable VRI-TV-P  variable VRI-RV-P
variable EC-TV-P   variable EC-RV-P    variable EI-TV-P   variable EI-RV-P
variable TVK-P

: TV-ARENA-BOOT ( -- )         \ point every var-id store at its boot buffer
   TVT-BOOT TVT-P !            RVT-BOOT RVT-P !
   VRC-TV-BOOT VRC-TV-P !      VRC-RV-BOOT VRC-RV-P !
   VRI-TV-BOOT VRI-TV-P !      VRI-RV-BOOT VRI-RV-P !
   EC-TV-BOOT EC-TV-P !        EC-RV-BOOT EC-RV-P !
   EI-TV-BOOT EI-TV-P !        EI-RV-BOOT EI-RV-P !
   TVK-BOOT TVK-P ! ;
TV-ARENA-BOOT

: TVT ( -- ptr a ) TVT-P @ ;         : RVT ( -- ptr a ) RVT-P @ ;
: TVK ( -- ptr a ) TVK-P @ ;
: VRC-TV ( -- ptr a ) VRC-TV-P @ ;   : VRC-RV ( -- ptr a ) VRC-RV-P @ ;
: VRI-TV ( -- ptr a ) VRI-TV-P @ ;   : VRI-RV ( -- ptr a ) VRI-RV-P @ ;
: EC-TV ( -- ptr a ) EC-TV-P @ ;     : EC-RV ( -- ptr a ) EC-RV-P @ ;
: EI-TV ( -- ptr a ) EI-TV-P @ ;     : EI-RV ( -- ptr a ) EI-RV-P @ ;

: TV-GROW-ONE ( ptr a n n -- ) {: pv:ptr oc:n nc:n :}   \ pv holds base; grow to nc cells
   nc cells ARENA-ALLOC {: nb:ptr :}
   pv @ nb oc cells ARENA-COPY
   nb oc nc ARENA-CELLS-UNBOUND
   nb pv ! ;

\ TVK grows like TV-GROW-ONE but its fresh tail is TVK-ANY (0), not UNBOUND: an
\ un-raised var id is always ANY.
: TVK-GROW-ONE ( ptr a n n -- ) {: pv:ptr oc:n nc:n :}
   nc cells ARENA-ALLOC {: nb:ptr :}
   pv @ nb oc cells ARENA-COPY
   nb oc nc ARENA-CELLS-ZERO
   nb pv ! ;

: TV-GROW ( n -- ) {: need:n :}
   need TV-CAP @ 2 * max {: nc:n :}   \ geometric: at least double
   TV-CAP @ {: oc:n :}
   TVT-P oc nc TV-GROW-ONE       RVT-P oc nc TV-GROW-ONE
   VRC-TV-P oc nc TV-GROW-ONE    VRC-RV-P oc nc TV-GROW-ONE
   VRI-TV-P oc nc TV-GROW-ONE    VRI-RV-P oc nc TV-GROW-ONE
   EC-TV-P oc nc TV-GROW-ONE     EC-RV-P oc nc TV-GROW-ONE
   EI-TV-P oc nc TV-GROW-ONE     EI-RV-P oc nc TV-GROW-ONE
   TVK-P oc nc TVK-GROW-ONE
   nc TV-CAP ! ;

: TV-ENSURE ( n -- ) {: need:n :}   \ ensure cap >= need
   need TV-CAP @ <= IF exit THEN
   need TV-GROW ;

: TVINIT   \ unbind every type and row var (one-time load init; NEW uses TV-RESET)
   0 BEGIN
     dup cells TVT + UNBOUND swap !
     dup cells RVT + UNBOUND swap !
     dup cells TVK + 0 swap !
     1 + dup MAXTV 1 - >
   UNTIL drop ;
TVINIT

0 constant TVK-ANY   \ ordinary polymorphic type var
1 constant TVK-RAW   \ minted by a raw storage definer; admits only plain scalars
: TVK@ ( n -- n ) cells TVK + @ ;
: TVK-RAW? ( n -- bool ) TVK@ TVK-RAW = ;
\ permanent (untrailed) RAW mark for build-time contexts (prim/signature build,
\ freshening); the trailed TVK-RAISE below is used inside unification.
: TVK-RAW! ( n -- ) TVK-RAW swap cells TVK + ! ;

: TAG 7 and ;

: PAY 3 rshift ;

: MK-CON 3 lshift ;

: MK-VAR 3 lshift T-VAR or ;

: MK-ROW 3 lshift S-ROW or ;

1024 constant MAXPTR-INIT       \ ptr terms (grows on demand)
create PTRA-BOOT MAXPTR-INIT cells allot   variable PTRN
variable PTRA-P   variable PTR-CAP
PTRA-BOOT PTRA-P !   MAXPTR-INIT PTR-CAP !
: PTRA ( -- ptr a ) PTRA-P @ ;

: PTR-ENSURE ( n -- ) {: need:n :}
   need PTR-CAP @ <= IF exit THEN
   need PTR-CAP @ 2 * max {: nc:n :}
   PTRA-P @ PTR-CAP @ cells nc cells ARENA-BYTES-GROW PTRA-P !
   nc PTR-CAP ! ;

: MK-PTR ( n -- n )
   PTRN @ 1 + PTR-ENSURE
   PTRN @ cells PTRA + !
   PTRN @ 3 lshift T-PTR or
   PTRN @ 1 + PTRN ! ;

: PTR>INNER PAY cells PTRA + @ ;

\ --- unification trail: TV!/RV! record each speculative var binding here so a
\ failed prim-overload trial undoes them by popping+unbinding (TRIAL-REST) instead
\ of copying the whole TVT/RVT pool. Each entry packs (var-id << 1 | is-row).
\ Reset per definition in NEW; grows into anon mmap on demand; repointed to boot
\ at snapshot (per-definition scratch, no live content across a snapshot).
4096 constant TRAIL-INIT        \ trail entries (grows on demand)
create TRAIL-BOOT TRAIL-INIT cells allot
variable TRAIL-P   variable TRAIL-CAP   variable TRAIL-N
TRAIL-BOOT TRAIL-P !   TRAIL-INIT TRAIL-CAP !   0 TRAIL-N !
: TRAIL ( -- ptr a ) TRAIL-P @ ;
: TRAIL-RESET ( -- ) 0 TRAIL-N ! ;
: TRAIL-ENSURE ( n -- ) {: need:n :}
   need TRAIL-CAP @ <= IF exit THEN
   need TRAIL-CAP @ 2 * max {: nc:n :}
   TRAIL-P @ TRAIL-CAP @ cells nc cells ARENA-BYTES-GROW TRAIL-P !
   nc TRAIL-CAP ! ;
: TRAIL-PUSH ( n n -- ) {: id:n tag:n :}   \ record a mutation to var `id`; tag 0=TVT 1=RVT 2=TVK
   TRAIL-N @ 1 + TRAIL-ENSURE
   id 4 * tag +  TRAIL-N @ cells TRAIL + !
   TRAIL-N @ 1 + TRAIL-N ! ;
: TRAIL-UNWIND ( n -- ) {: mark:n :}     \ pop+undo every mutation above `mark`
   BEGIN TRAIL-N @ mark > WHILE
      TRAIL-N @ 1 - TRAIL-N !
      TRAIL-N @ cells TRAIL + @ {: e:n :}
      e 3 and {: tag:n :}   e 2 rshift {: id:n :}
      tag 0= IF UNBOUND id cells TVT + ! ELSE
      tag 1 = IF UNBOUND id cells RVT + ! ELSE
      TVK-ANY id cells TVK + ! THEN THEN
   REPEAT ;
\ TVK-RAISE ( id -- ) : raise var `id` to TVK-RAW inside unification, trailed so a
\ failed prim-overload trial (TRIAL-REST) or definition reject restores TVK-ANY.
\ Idempotent: a var already RAW records nothing (so no spurious trail growth).
: TVK-RAISE ( n -- )
   dup TVK-RAW? IF drop EXIT THEN
   dup 2 TRAIL-PUSH
   TVK-RAW! ;

\ --- linear/affine kind discipline (habu-linear-kind-inference) --------------
\ Concrete-count conservation only sees linear CONS on the stack. It is defeated
\ by polymorphic laundering: a value copied/dropped while its type is still a VAR
\ that only later unifies with a linear con (KEEP's `over`, an intra-quot
\ `dup FREE`). The kind discipline tracks linearity THROUGH type vars:
\   (a) polarity-aware multiplicity at effect application (LIN-EFF-PASS): a var
\       in an applied effect that binds to a linear con must occur equally on the
\       input and output sides across the whole effect INCL quotation sub-effects
\       (KEEP: a is 1-in / 2-out -> reject; DIP/swap: 1-in / 1-out -> ok);
\   (b) deferred taint (LIN-TAINT / LIN-TAINT-SCAN): a var copied/dropped while
\       still polymorphic is tainted; if it LATER resolves to a linear con the
\       linear was laundered -> reject (catches `[: dup FREE ;]`).
\ The whole discipline is gated on any DEFLINEAR type being declared, so
\ non-linear code (the entire self-build) pays nothing and the hot path is clean.
variable LIN-NDECL   0 LIN-NDECL !     \ count of declared DEFLINEAR types (in scope)
: LIN-ANY? ( -- bool ) LIN-NDECL @ 0 <> ;

\ Taint list: canonical var ids duplicated/dropped while still polymorphic. Only
\ appended on a COMMITTED step (LIN-EFF-PASS runs at OK true), so no failed prim
\ trial ever taints a rolled-back id — trial rollback can never reuse a tainted
\ id. Reset per definition (NEW). Scanned after each token (LIN-TAINT-SCAN).
4096 constant LTNT-INIT
create LTNT-BOOT LTNT-INIT cells allot
variable LTNT-P   variable LTNT-CAP   variable LTNT-N
LTNT-BOOT LTNT-P !   LTNT-INIT LTNT-CAP !   0 LTNT-N !
: LTNT ( -- ptr a ) LTNT-P @ ;
: LIN-TAINT-RESET ( -- ) 0 LTNT-N ! ;
: LTNT-ENSURE ( n -- ) {: need:n :}
   need LTNT-CAP @ <= IF exit THEN
   need LTNT-CAP @ 2 * max {: nc:n :}
   LTNT-P @ LTNT-CAP @ cells nc cells ARENA-BYTES-GROW LTNT-P !
   nc LTNT-CAP ! ;
: LIN-TAINT ( n -- )
   LTNT-N @ 1 + LTNT-ENSURE
   LTNT-N @ cells LTNT + !
   LTNT-N @ 1 + LTNT-N ! ;

\ TRIAL-DEPTH counts open prim-overload trials (TRY-EFF). T-RES/R-RES compress var
\ chains ONLY at depth 0, where every binding is permanent, so a compression write
\ needs no undo (it is a direct TVT/RVT store, not routed through the trail). During
\ an open trial compression is disabled, so it can never re-point a permanent var at
\ a trial-allocated var that TRIAL-REST would then clear (the item-3 hazard).
variable TRIAL-DEPTH   0 TRIAL-DEPTH !
variable TCMP                            \ path-compression walk cursor

: TV@ cells TVT + @ ;

: TV! ( n n -- ) dup 0 TRAIL-PUSH  cells TVT + ! ;

: RV@ cells RVT + @ ;

: RV! ( n n -- ) dup 1 TRAIL-PUSH  cells RVT + ! ;
256 constant MAXQE-INIT        \ quotation effects (din dout rin rout per record); grows on demand
create QEA-BOOT MAXQE-INIT 32 * allot
create QXDA-BOOT MAXQE-INIT cells allot   create QXRA-BOOT MAXQE-INIT cells allot
create QXHA-BOOT MAXQE-INIT cells allot   create QXNA-BOOT MAXQE-INIT cells allot   variable QEN
variable QEA-P   variable QXDA-P   variable QXRA-P   variable QXHA-P   variable QXNA-P
variable QE-CAP
QEA-BOOT QEA-P !   QXDA-BOOT QXDA-P !   QXRA-BOOT QXRA-P !
QXHA-BOOT QXHA-P !   QXNA-BOOT QXNA-P !   MAXQE-INIT QE-CAP !
: QEA ( -- ptr a ) QEA-P @ ;
: QXDA ( -- ptr a ) QXDA-P @ ;   : QXRA ( -- ptr a ) QXRA-P @ ;
: QXHA ( -- ptr a ) QXHA-P @ ;   : QXNA ( -- ptr a ) QXNA-P @ ;

: QE-ENSURE ( n -- ) {: need:n :}
   need QE-CAP @ <= IF exit THEN
   need QE-CAP @ 2 * max {: nc:n :}
   QEA-P @ QE-CAP @ 32 * nc 32 * ARENA-BYTES-GROW QEA-P !
   QXDA-P @ QE-CAP @ cells nc cells ARENA-BYTES-GROW QXDA-P !
   QXRA-P @ QE-CAP @ cells nc cells ARENA-BYTES-GROW QXRA-P !
   QXHA-P @ QE-CAP @ cells nc cells ARENA-BYTES-GROW QXHA-P !
   QXNA-P @ QE-CAP @ cells nc cells ARENA-BYTES-GROW QXNA-P !
   nc QE-CAP ! ;

: MK-QUOT {: din dout rin rout :}   \ ( -- t ) allocate a quot<effect> term
   QEN @ 1 + QE-ENSURE
   QEN @ 32 * QEA + {: a :}
   din a !  dout a 8 + !  rin a 16 + !  rout a 24 + !
   0 QEN @ cells QXHA + !
   0 QEN @ cells QXNA + !
   0 QEN @ cells QXDA + !
   0 QEN @ cells QXRA + !
   QEN @ 3 lshift T-QUOT or  QEN @ 1 + QEN ! ;
: Q>DIN  PAY 32 * QEA + @ ;
: Q>DOUT PAY 32 * QEA + 8 + @ ;
: Q>RIN  PAY 32 * QEA + 16 + @ ;
: Q>ROUT PAY 32 * QEA + 24 + @ ;
: Q>XHAS PAY cells QXHA + @ ;
: Q>XDEAD PAY cells QXNA + @ ;
: Q>XDOUT PAY cells QXDA + @ ;
: Q>XROUT PAY cells QXRA + @ ;
: QX! {: q xhas xdead xd xr :}
   xhas q PAY cells QXHA + !
   xdead q PAY cells QXNA + !
   xd q PAY cells QXDA + !
   xr q PAY cells QXRA + ! ;

512 constant MAXATOM-INIT       \ atom terms (grows on demand)
create ATOMA-BOOT MAXATOM-INIT cells allot
create ATOMU-BOOT MAXATOM-INIT cells allot
create ATOMK-BOOT MAXATOM-INIT cells allot
variable ATOMN
variable RIGID-N
\ Rigid host-allocation identity domains (dot habu-define-rigid-host). A host
\ allocation is stamped by FRESH, RIGID, monotonic-nonreuse identities that ptr T
\ and ordinary type vars cannot express: its host REGION (which allocation), its
\ EXTENT (bounds), and its mutation GENERATION (epoch). Each domain owns a PRIVATE
\ per-check counter (RGN-N/EXT-N/GEN-N); a numeric handle is never itself the
\ authority — the atom's domain qualifies the id in ATOM-OK?, so equal ids from
\ different domains never unify. RIGID-MAX bounds every domain BELOW the sign wrap
\ so a counter EXHAUSTS (throws E-RIGID-EXHAUST) before it could wrap and re-grant
\ a still-live id. Counters reset per check (RIGID-RESET); the bound is settable.
7140 constant E-RIGID-EXHAUST   \ a rigid identity domain exhausted before wrap
variable RGN-N   variable EXT-N   variable GEN-N
variable RIGID-MAX   $4000000000000000 RIGID-MAX !
variable ATOMA-P   variable ATOMU-P   variable ATOMK-P   variable ATOM-CAP
ATOMA-BOOT ATOMA-P !   ATOMU-BOOT ATOMU-P !   ATOMK-BOOT ATOMK-P !   MAXATOM-INIT ATOM-CAP !
: ATOMA ( -- ptr a ) ATOMA-P @ ;
: ATOMU ( -- ptr a ) ATOMU-P @ ;
: ATOMK ( -- ptr a ) ATOMK-P @ ;
: ATOM-ENSURE ( n -- ) {: need:n :}
   need ATOM-CAP @ <= IF exit THEN
   need ATOM-CAP @ 2 * max {: nc:n :}
   ATOMA-P @ ATOM-CAP @ cells nc cells ARENA-BYTES-GROW ATOMA-P !
   ATOMU-P @ ATOM-CAP @ cells nc cells ARENA-BYTES-GROW ATOMU-P !
   ATOMK-P @ ATOM-CAP @ cells nc cells ARENA-BYTES-GROW ATOMK-P !
   nc ATOM-CAP ! ;
: ATOMA-FIELD ( n -- ptr ptr u8 )
   cells ATOMA + 0 ptr-field ;
: RIGID-RESET ( -- )
   1 RIGID-N !   1 RGN-N !   1 EXT-N !   1 GEN-N ! ;
\ Shared catch-all domain: fresh-mask-* and every non-domain fresh atom mint
\ here. It EXHAUSTS at RIGID-MAX exactly like the per-domain counters below, so
\ a wrap can never reuse a still-live id and unify two distinct identities.
: RIGID-FRESH ( -- n )
   RIGID-N @ RIGID-MAX @ >= IF E-RIGID-EXHAUST throw THEN
   RIGID-N @ dup 1+ RIGID-N ! ;
\ Per-domain mint. Each returns the current id then advances, but EXHAUSTS
\ (throws) at RIGID-MAX rather than wrapping into a reused (or non-positive) id.
: RGN-FRESH ( -- n )
   RGN-N @ RIGID-MAX @ >= IF E-RIGID-EXHAUST throw THEN
   RGN-N @ dup 1+ RGN-N ! ;
: EXT-FRESH ( -- n )
   EXT-N @ RIGID-MAX @ >= IF E-RIGID-EXHAUST throw THEN
   EXT-N @ dup 1+ EXT-N ! ;
: GEN-FRESH ( -- n )
   GEN-N @ RIGID-MAX @ >= IF E-RIGID-EXHAUST throw THEN
   GEN-N @ dup 1+ GEN-N ! ;
\ Route a template fresh-atom's (stripped) name to its domain counter. The
\ `fresh-` prefix is already stripped, so the tail begins with the domain word;
\ mask- and any other fresh atom keep the shared RIGID-FRESH space.
: RIGID-AK-MINT ( ptr u8 n -- n ) {: a:ptr u:n :}
   u 7 >= IF a 7 s" region-" CORE-STR= IF RGN-FRESH EXIT THEN THEN
   u 7 >= IF a 7 s" extent-" CORE-STR= IF EXT-FRESH EXIT THEN THEN
   u 4 >= IF a 4 s" gen-"    CORE-STR= IF GEN-FRESH EXIT THEN THEN
   RIGID-FRESH ;
: MK-ATOM-K ( ptr u8 n n -- n ) {: a:ptr u:n k:n :}
   ATOMN @ 1 + ATOM-ENSURE
   a ATOMN @ ATOMA-FIELD !
   u ATOMN @ cells ATOMU + !
   k ATOMN @ cells ATOMK + !
   ATOMN @ 3 lshift T-ATOM or
   ATOMN @ 1 + ATOMN ! ;
: MK-ATOM ( ptr u8 n -- n )
   0 MK-ATOM-K ;
: ATOM>A ( n -- ptr u8 ) PAY ATOMA-FIELD @ ;
: ATOM>U ( n -- n ) PAY cells ATOMU + @ ;
: ATOM>K ( n -- n ) PAY cells ATOMK + @ ;

512 constant MAXPARAM-INIT      \ param terms (grows on demand)
create PARAMA-BOOT MAXPARAM-INIT cells allot
create PARAMU-BOOT MAXPARAM-INIT cells allot
create PARAMC-BOOT MAXPARAM-INIT cells allot
create PARAMFAM-BOOT MAXPARAM-INIT cells allot   \ resolved family-id per param term (identity)
create PARAMOFF-BOOT MAXPARAM-INIT cells allot   \ arg-run start index into the flat PARGP pool
create PARAMHID-BOOT MAXPARAM-INIT cells allot   \ hidden physical-field slot+1 per param term (0 = logical, docs item 12 slice-3a)
\ PARGP is the flat per-param arg pool: each param term stores its arg terms as a
\ contiguous run [PARAMOFF[p], PARAMOFF[p]+argc) here, so a family of ANY arity is
\ stored without a fixed row cap. Cells hold term codes (pointer-free), so a grow
\ is a plain byte copy and it resets in NEW alongside PARAMN. This replaces the old
\ fixed PARAM-MAX-ARGS-strided PARAMARGS row.
2048 constant PARG-INIT
create PARGP-BOOT PARG-INIT cells allot
\ PARAM-SCR is the reentrant parse/replay scratch: it holds the arg terms pushed
\ across ALL currently-open nesting levels, so its depth is a nesting-peak, not a
\ per-param arg count. It grows on demand (a nested family whose args land at a
\ non-zero base must not overflow); per-param arity is uncapped (PARGP grows).
32 constant PARAM-SCR-INIT
create PARAM-SCR-BOOT PARAM-SCR-INIT cells allot
variable PARAMN
variable PARAM-SCR-N
variable PARAM-I
variable PARAMA-P   variable PARAMU-P   variable PARAMC-P   variable PARAMFAM-P
variable PARAMOFF-P   variable PARAMHID-P   variable PARGP-P   variable PARG-N   variable PARG-CAP-V
variable PARAM-SCR-P
variable PARAM-CAP     variable PARAM-SCR-CAP-V
PARAMA-BOOT PARAMA-P !   PARAMU-BOOT PARAMU-P !   PARAMC-BOOT PARAMC-P !
PARAMFAM-BOOT PARAMFAM-P !   PARAMOFF-BOOT PARAMOFF-P !   PARAMHID-BOOT PARAMHID-P !
PARGP-BOOT PARGP-P !   PARG-INIT PARG-CAP-V !   0 PARG-N !
MAXPARAM-INIT PARAM-CAP !
PARAM-SCR-BOOT PARAM-SCR-P !    PARAM-SCR-INIT PARAM-SCR-CAP-V !
: PARAMA ( -- ptr a ) PARAMA-P @ ;
: PARAMU ( -- ptr a ) PARAMU-P @ ;
: PARAMC ( -- ptr a ) PARAMC-P @ ;
: PARAMFAM ( -- ptr a ) PARAMFAM-P @ ;
: PARAMOFF ( -- ptr a ) PARAMOFF-P @ ;
: PARAMHID ( -- ptr a ) PARAMHID-P @ ;
: PARGP ( -- ptr a ) PARGP-P @ ;
: PARG-ENSURE ( n -- ) {: need:n :}    \ room for `need` more arg cells past PARG-N
   PARG-N @ need + PARG-CAP-V @ <= IF exit THEN
   PARG-N @ need + PARG-CAP-V @ 2 * max {: nc:n :}
   PARGP-P @ PARG-CAP-V @ cells nc cells ARENA-BYTES-GROW PARGP-P !
   nc PARG-CAP-V ! ;
: PARAM-SCR ( -- ptr a ) PARAM-SCR-P @ ;
: PARAM-SCR-ENSURE ( -- )         \ room for one more scratch arg (grows the nesting-peak buffer)
   PARAM-SCR-N @ PARAM-SCR-CAP-V @ < IF exit THEN
   PARAM-SCR-N @ 1 + PARAM-SCR-CAP-V @ 2 * max {: nc:n :}
   PARAM-SCR-P @ PARAM-SCR-CAP-V @ cells nc cells ARENA-BYTES-GROW PARAM-SCR-P !
   nc PARAM-SCR-CAP-V ! ;
: PARAM-ENSURE ( n -- ) {: need:n :}
   need PARAM-CAP @ <= IF exit THEN
   need PARAM-CAP @ 2 * max {: nc:n :}
   PARAMA-P @ PARAM-CAP @ cells nc cells ARENA-BYTES-GROW PARAMA-P !
   PARAMU-P @ PARAM-CAP @ cells nc cells ARENA-BYTES-GROW PARAMU-P !
   PARAMC-P @ PARAM-CAP @ cells nc cells ARENA-BYTES-GROW PARAMC-P !
   PARAMFAM-P @ PARAM-CAP @ cells nc cells ARENA-BYTES-GROW PARAMFAM-P !
   PARAMOFF-P @ PARAM-CAP @ cells nc cells ARENA-BYTES-GROW PARAMOFF-P !
   PARAMHID-P @ PARAM-CAP @ cells nc cells ARENA-BYTES-GROW PARAMHID-P !
   nc PARAM-CAP ! ;

\ --- resolved type-family (TFAM) identity for T-PARAM terms. The TFAM registry
\ lives in src/core/type-family.f, loaded AFTER checker.f, so its query words are
\ reached through friend hooks: each is a `defer` with a checker-visible declared
\ effect, given a registry-not-loaded DEFAULT here and rebound with `is` to the
\ real query word by type-family.f at prefix load. Declaring the effect makes a
\ hook call statically effect-known, so the checker never models it as an opaque
\ executed xt (dot habu-checker-exec-of-5923c543). Defaults bind below RES-FALSE
\ (which several need). Hooks reached only after the registry is armed carry no
\ default and stay unset (executing one before install fails closed via
\ DEFER-UNSET) — these are the CONSTRUCT-STEP / MATCH-* / TFAM-CON-LIN query
\ hooks, whose call sites never run before type-family.f installs them.
defer TFAM-RESOLVE-XT ( ptr u8 n ptr u8 n -- n bool )   \ pkg + name -> family id, true | false
defer TFAM-ARITY-XT ( n -- n )                          \ family id -> declared arity
defer TFAM-LAYOUT?-XT ( n -- bool )                     \ family id occupies an ADT layout
defer TFAM-CELL?-XT ( n -- bool )                       \ family id is a scalar cell kind (TK-CELL)
defer TFAM-WIDTH-XT ( n -- n )                           \ declared logical width in stack cells, params-as-cells (docs §18)
defer TFAM-INST-WIDTH-XT ( n -- n )                     \ INSTANTIATED logical width of a resolved layout term, arg-aware (docs §18)
defer TFAM-CON-LIN-XT ( n -- bool )                     \ family schemas contain a concrete linear value
defer CONSTRUCT-FAM-XT ( ptr u8 n -- n bool )           \ item 9 construct family resolve, active package only
defer CONSTRUCT-STEP-XT ( ptr u8 n n -- bool )          \ item 9 construct variant resolve + step effect
defer CTOR-STEP-XT ( n -- bool )                        \ layout-cap slice 3: a generated-constructor CALL whose declared output instantiates a family param at a multi-cell layout arg routes through the arg-aware construct step (the fixed 1-cell-per-param stored effect cannot consume/produce the wide bundle); 0/cell/open args fall through to the ordinary word call
defer MATCH-FAM-XT ( ptr u8 n -- n bool )               \ item 9 MATCH family resolve, signature scope
defer MATCH-VAR-XT ( ptr u8 n n -- n bool )             \ variant tail -> SUMV id within a family
defer MATCH-VTAG-XT ( n -- n )                           \ SUMV id -> declaration-order tag (seen-bitset index)
defer MATCH-VCOUNT-XT ( n -- n )                        \ family id -> variant count (exhaustiveness domain)
defer MATCH-PAY-XT ( n n n -- n )                        \ vid famterm row -- row + instantiated payload
defer FIELD-PROJ-XT ( n n n -- n bool )                 \ field-id off famterm -- fieldterm ok : instantiated field type from a committed field id + the input pointer's family args (dot habu-checker-type-structure)
variable FIELD-FAM   -1 FIELD-FAM !              \ reserved family-id of the internal `field` ctor

\ M5 barrier-uniformity: the `tile` and `uniform` family ids, captured by
\ type-family.f at registration (loaded after checker.f). A word whose declared
\ effect consumes a `tile` and produces a `uniform` is a block reduction whose
\ emitter lowers to a shared-memory reduction with `bar.sync`; that barrier is
\ only sound under block-uniform control. 0 = registry not loaded => no detection.
variable PTX-TILE-FAM      0 PTX-TILE-FAM !
variable PTX-UNIFORM-FAM   0 PTX-UNIFORM-FAM !
\ cp.async pipeline-slot typestate (habu-checker-cp-async-6ba788a5): the
\ `cpp-committed`/`cpp-ready` family ids, captured by type-family.f alongside
\ tile/uniform. The WAIT step of the cp.async double-buffer protocol has the
\ declared shape ( cpp-committed<p> -- cpp-ready<p> ): it retires the copy group
\ and fences with `bar.sync` so the staged tile is block-visible. Like the M5
\ tile->uniform reduction that is the ONLY sound way to make a uniform, this
\ committed->ready shape is the ONLY sound way to make a ready slot, so it is the
\ same block-uniform barrier and composes with CTL-BARRIER at the same choke
\ (PTX-BARRIER-ROWS?/BARRIER-CUR?). 0 = registry not loaded => no detection.
variable PTX-CPCOMMITTED-FAM   0 PTX-CPCOMMITTED-FAM !
variable PTX-CPREADY-FAM       0 PTX-CPREADY-FAM !
\ Forward hook ( n -- ): OR CTL-BARRIER into a symbol's control flags. A `defer`
\ so E-ADD-EFFECT calls it with a statically-known effect; the registry-not-armed
\ DEFAULT (bound below) drops the sym as a no-op, covering the window before
\ PTX-BARRIER-SET installs. Reached from BOTH the checked `:` and the TRUSTED:
\ publish paths through the one E-ADD-EFFECT choke.
defer PTX-BARRIER-SET-XT ( n -- )

\ --- checker package scope state. Declared here (not with the package words
\ further down) so signature parsing (SIG-FAM?) can resolve family tokens
\ through the ACTIVE package scope. The mutators stay in the package block.
0 constant CHECKER-PACKAGE-NONE
1 constant CHECKER-PACKAGE-PRIVATE
2 constant CHECKER-PACKAGE-PUBLIC
$100 constant CHECKER-PACKAGE-CAP
create CHECKER-PACKAGE-NAME CHECKER-PACKAGE-CAP allot
variable CHECKER-PACKAGE-U
variable CHECKER-PACKAGE-MODE

: CHECKER-PACKAGE-ACTIVE? ( -- bool )
   CHECKER-PACKAGE-MODE @ CHECKER-PACKAGE-NONE <> ;

0 constant UK-EXACT
1 constant UK-INPUT
2 constant UK-COERCE
variable UNIFY-KIND
UK-EXACT UNIFY-KIND !

: PARAMA-FIELD ( n -- ptr ptr u8 )
   cells PARAMA + 0 ptr-field ;
: PARAM>NAME-A ( n -- ptr u8 ) PAY PARAMA-FIELD @ ;
: PARAM>NAME-U ( n -- n ) PAY cells PARAMU + @ ;
: PARAM>ARGC ( n -- n ) PAY cells PARAMC + @ ;
: PARAM>FAM ( n -- n ) PAY cells PARAMFAM + @ ;   \ resolved family-id (identity)
: PARAM>OFF ( n -- n ) PAY cells PARAMOFF + @ ;   \ arg-run start index into PARGP
: PARAM>HID ( n -- n ) PAY cells PARAMHID + @ ;   \ hidden physical-field slot+1 (0 = logical, docs item 12 slice-3a)
: PARAM-ARG-IDX ( n n -- ptr n ) {: p idx :}
   p PARAM>OFF idx + cells PARGP + ;
: PARAM>ARG ( n n -- n ) PARAM-ARG-IDX @ ;
: PARAM-SCR+ ( n -- )
   PARAM-SCR-ENSURE
   PARAM-SCR-N @ cells PARAM-SCR + !
   PARAM-SCR-N @ 1 + PARAM-SCR-N ! ;
\ MK-PARAM ( base a u fam -- t ) : build a T-PARAM from the scratch args pushed at
\ [base, PARAM-SCR-N); argc = PARAM-SCR-N - base. `base` is the caller's scratch
\ mark, so nested/replayed param builds are reentrant: MK-PARAM rewinds the shared
\ scratch back to `base` (never to 0), so a parent's already-pushed args survive.
\ The argc args are copied into a fresh contiguous run in the flat PARGP pool
\ (uncapped arity); the run start is recorded in PARAMOFF.
: MK-PARAM {: base:n a:ptr u:n fam:n :}
   PARAMN @ 1 + PARAM-ENSURE
   PARAM-SCR-N @ base - {: argc:n :}
   argc PARG-ENSURE
   PARG-N @ {: start:n :}
   a PARAMN @ PARAMA-FIELD !
   u PARAMN @ cells PARAMU + !
   fam PARAMN @ cells PARAMFAM + !
   argc PARAMN @ cells PARAMC + !
   start PARAMN @ cells PARAMOFF + !
   0 PARAMN @ cells PARAMHID + !   \ new term is logical; MK-HIDDEN/E-INST overwrite for hidden fields

   0 BEGIN dup argc < WHILE          \ data-stack index (RECURSE-safe; ?do clobbers locals)
      dup base + cells PARAM-SCR + @
      over start + cells PARGP + !
      1 +
   REPEAT drop
   argc PARG-N @ + PARG-N !
   base PARAM-SCR-N !
   PARAMN @ 3 lshift T-PARAM or
   PARAMN @ 1 + PARAMN ! ;

4096 constant MAXPUSH-INIT     \ push records (engine-sized bodies need hundreds; grows on demand)
create SPA-BOOT MAXPUSH-INIT 16 * allot   variable SPN
variable SPA-P   variable SPA-CAP
SPA-BOOT SPA-P !   MAXPUSH-INIT SPA-CAP !
: SPA ( -- ptr a ) SPA-P @ ;
: SPA-ENSURE ( n -- ) {: need:n :}
   need SPA-CAP @ <= IF exit THEN
   need SPA-CAP @ 2 * max {: nc:n :}
   SPA-P @ SPA-CAP @ 16 * nc 16 * ARENA-BYTES-GROW SPA-P !
   nc SPA-CAP ! ;

: MK-PUSH ( n n -- n )
   SPN @ 1 + SPA-ENSURE
   SPN @ 2 * cells SPA + {: a:ptr :}
   a 8 + !
   a !
   SPN @ 3 lshift S-PUSH or
   SPN @ 1 + SPN ! ;

: P>TYPE PAY 2 * cells SPA + @ ;

: P>REST PAY 2 * cells SPA + 8 + @ ;

: ISVAR TAG T-VAR = ;

: ISROW TAG S-ROW = ;

: RES-TRUE ( -- bool )
   0 0= ;

: RES-FALSE ( -- bool )
   0 0= 0= ;

\ --- BTC-7 extent-role product/factorization role registry (dot
\ habu-extent-role-product-8e364885, docs/extent-substrate.md §Decision, BTC-7).
\ EXT-PROD-FAM / EXT-REDX-FAM are the built-in `extprod`/`redx` family ids,
\ captured by type-family.f after registration (0 = registry not loaded => no
\ detection; the engine self-check never uses these families). The free-set records
\ extent families that are FREE (outer / batch) factors of a product: EXTPROD:
\ (maki/extent.f) marks a product's factor-0 free at declaration time. The
\ contraction rule (EXT-REDX-BAD-ARG? at SIG-END-PARAM) rejects `redx<E>` when E is
\ a free extent or a whole product, so a contraction (+Σ) over the free #B factor of
\ a folded #B*#T row is a load-time type error — the cross-sequence leak is
\ unrepresentable. Cleared with the family registry (type-family.f TFAM-RESET) so a
\ recycled family id can never inherit a stale free mark.
variable EXT-PROD-FAM   0 EXT-PROD-FAM !
variable EXT-REDX-FAM   0 EXT-REDX-FAM !
$40 constant EXT-FREE-CAP
create EXT-FREE-SET EXT-FREE-CAP cells allot
variable EXT-FREE-N   0 EXT-FREE-N !
: EXT-FREE-CLEAR ( -- ) 0 EXT-FREE-N ! ;
: EXT-FREE? ( n -- bool ) {: f:n :}
   f 0 <= IF RES-FALSE EXIT THEN
   0 BEGIN dup EXT-FREE-N @ < WHILE
      dup cells EXT-FREE-SET + @ f = IF drop RES-TRUE EXIT THEN
      1 +
   REPEAT drop RES-FALSE ;
: EXT-MARK-FREE ( n -- ) {: f:n :}      \ EXTPROD: marks a product's free factor family
   f 0 <= IF EXIT THEN
   f EXT-FREE? IF EXIT THEN              \ idempotent
   EXT-FREE-N @ EXT-FREE-CAP >= IF s" checker: extent free-set overflow" 76 die THEN
   f EXT-FREE-N @ cells EXT-FREE-SET + !
   EXT-FREE-N @ 1 + EXT-FREE-N ! ;

\ The `*` query wrappers callers use: each invokes the effect-known defer, so
\ callers stay unchanged while the fetch/execute of a raw variable is gone.
: TFAM-RESOLVE* ( ptr u8 n ptr u8 n -- n bool ) TFAM-RESOLVE-XT ;
: TFAM-ARITY* ( n -- n ) TFAM-ARITY-XT ;
: TFAM-LAYOUT?* ( n -- bool ) TFAM-LAYOUT?-XT ;
: TFAM-CELL?* ( n -- bool ) TFAM-CELL?-XT ;
: TFAM-WIDTH@* ( n -- n ) TFAM-WIDTH-XT ;

\ Registry-not-loaded DEFAULTS for the TFAM query hooks (wrapped in a word so the
\ `[: ;]` quotations compile; `[: ;] is` is not valid at interpret level). Each
\ default reproduces the old 0-hook fallback: resolve nothing / arity 0 / not a
\ layout / not a cell / one cell / the declared family width / no construct family
\ / no match family / not a wide-ctor call. type-family.f rebinds each hook with
\ `is` to the real query word once the registry exists.
: TFAM-QUERY-DEFAULTS ( -- )
   [: 2drop 2drop 0 RES-FALSE ;] is TFAM-RESOLVE-XT
   [: drop 0 ;] is TFAM-ARITY-XT
   [: drop RES-FALSE ;] is TFAM-LAYOUT?-XT
   [: drop RES-FALSE ;] is TFAM-CELL?-XT
   [: drop 1 ;] is TFAM-WIDTH-XT
   [: PARAM>FAM TFAM-WIDTH@* ;] is TFAM-INST-WIDTH-XT
   [: 2drop 0 RES-FALSE ;] is CONSTRUCT-FAM-XT
   [: 2drop 0 RES-FALSE ;] is MATCH-FAM-XT
   [: drop RES-FALSE ;] is CTOR-STEP-XT
   [: 2drop drop 0 RES-FALSE ;] is FIELD-PROJ-XT ;   \ no registry: field projection fails closed
TFAM-QUERY-DEFAULTS

\ registry-not-armed default for the PTX block-barrier hook: drop the sym (do not
\ flag), exactly what the old 0-hook guard did; checker.f rebinds it once
\ PTX-BARRIER-SET is defined below.
: PTX-BARRIER-DEFAULT ( -- ) [: drop ;] is PTX-BARRIER-SET-XT ;
PTX-BARRIER-DEFAULT

: TV-NEXT? ( n -- n bool )
   dup ISVAR 0= IF RES-FALSE EXIT THEN
   dup PAY TV@ dup UNBOUND = IF
      drop RES-FALSE
   ELSE
      nip RES-TRUE
   THEN ;

: RV-NEXT? ( n -- n bool )
   dup ISROW 0= IF RES-FALSE EXIT THEN
   dup PAY RV@ dup UNBOUND = IF
      drop RES-FALSE
   ELSE
      nip RES-TRUE
   THEN ;

: T-RES-WALK ( n -- n )
   BEGIN TV-NEXT? WHILE REPEAT ;

: T-BOUND-VAR? ( n -- bool ) {: v:n :}   \ a bound type var (has a chain link)?
   v ISVAR 0= IF RES-FALSE EXIT THEN
   v PAY TV@ UNBOUND <> ;

\ T-COMPRESS ( start root -- ) : point every bound var on start..root directly at
\ root. Direct TVT stores (depth 0 only, so permanent — no trail entry).
: T-COMPRESS ( n n -- ) {: root:n :}
   TCMP !
   BEGIN TCMP @ T-BOUND-VAR? WHILE
      TCMP @ PAY TV@ {: nxt:n :}
      root TCMP @ PAY cells TVT + !
      nxt TCMP !
   REPEAT ;

: T-RES ( n -- n )
   TRIAL-DEPTH @ 0 <> IF T-RES-WALK EXIT THEN   \ inside a trial: walk, do not compress
   dup T-RES-WALK {: root:n :}
   root T-COMPRESS
   root ;

: R-RES-WALK ( n -- n )
   BEGIN RV-NEXT? WHILE REPEAT ;

: R-BOUND-VAR? ( n -- bool ) {: v:n :}
   v ISROW 0= IF RES-FALSE EXIT THEN
   v PAY RV@ UNBOUND <> ;

: R-COMPRESS ( n n -- ) {: root:n :}
   TCMP !
   BEGIN TCMP @ R-BOUND-VAR? WHILE
      TCMP @ PAY RV@ {: nxt:n :}
      root TCMP @ PAY cells RVT + !
      nxt TCMP !
   REPEAT ;

: R-RES ( n -- n )
   TRIAL-DEPTH @ 0 <> IF R-RES-WALK EXIT THEN
   dup R-RES-WALK {: root:n :}
   root R-COMPRESS
   root ;
4096 constant MAXUWL           \ unify worklist cells (deep spines queue many pairs)
create UWL MAXUWL cells allot   variable USP   variable UOK
variable UF-ACT   variable UF-EXP   variable UF-SET
\ Parallel per-pair strictness flag, keyed by the pair's base worklist index.
\ Strict pairs (pointer pointees) unify by equality/var-binding only — integer
\ widening (INT-WIDENS?) applies to top-level scalar stack cells, never to a
\ pointer's pointee, so a concrete `ptr u8` never satisfies `ptr cell`/`ptr u32`.
create UWL-STR MAXUWL cells allot   variable CUR-STRICT
\ Parallel per-pair position, keyed like UWL-STR: the expected-row slot a pair
\ belongs to. >= 0 is a slot counted from the TOP of the root expected row (a
\ row element or any subterm pair inside it inherits it); -1 is no position;
\ <= -2 is the root-spine cursor c encoded as -(c+2), carried only by the
\ row-descent pairs themselves. U-FAIL latches the slot into UF-POSN with the
\ failed pair, so a construct payload mismatch can name the exact payload slot
\ even when two slots share one type term (MK-CON interns: same con = same id).
create UWL-POS MAXUWL cells allot   variable CUR-UPOS
variable UF-POSN

: U-PUSH ( n -- )
   USP @ MAXUWL 1 - > IF s" checker: unify worklist full" 76 die THEN
   USP @ cells UWL + !
   USP @ 1 + USP ! ;

: U-POP USP @ 1 - USP ! USP @ cells UWL + @ ;

: PAIR ( n n -- )    \ inherit the enclosing pair's strictness and position
   CUR-STRICT @ USP @ cells UWL-STR + !
   CUR-UPOS @ USP @ cells UWL-POS + !
   swap U-PUSH U-PUSH ;

: PAIR-STRICT ( n n -- )    \ force a strict (no-widen) subterm unification
   -1 USP @ cells UWL-STR + !
   CUR-UPOS @ USP @ cells UWL-POS + !
   swap U-PUSH U-PUSH ;

: UNPAIR ( -- n n )    \ pop a pair; restore its strictness and position
   U-POP U-POP swap
   USP @ cells UWL-STR + @ CUR-STRICT !
   USP @ cells UWL-POS + @ CUR-UPOS ! ;

: U-FAIL ( n n -- ) {: act:n exp:n :}
   UF-SET @ 0= IF
      act UF-ACT !  exp UF-EXP !  -1 UF-SET !
      CUR-UPOS @ dup 0 < IF drop -1 THEN UF-POSN !   \ slot only; spine/none = -1
   THEN
   RES-FALSE UOK ! ;

: FIELD-PARAM? ( n -- bool ) {: t:n :}
   FIELD-FAM @ 0 < IF RES-FALSE EXIT THEN   \ field family not registered (e.g. after TFAM-RESET)
   t T-RES TAG T-PARAM <> IF RES-FALSE EXIT THEN
   t T-RES PARAM>FAM FIELD-FAM @ = ;   \ identity by reserved family-id, not spelling

\ --- M5 barrier-uniformity shape classification -------------------------------
\ A block collective (BLOCK-MAX/BLOCK-SUM) has the declared shape
\ ( tile<..> -- uniform<..> ): it reduces a lane-varying tile to a block-uniform
\ scalar, and its emitter lowers to a shared-memory reduction with `bar.sync`.
\ That barrier is only sound under BLOCK-UNIFORM control (every lane of the block
\ reaches it); a call reached under divergent predication deadlocks. E-ADD-EFFECT
\ detects the shape from the recorded din/dout rows and flags CTL-BARRIER; the
\ call-site check rejects any such call inside an open control frame (#CFC>0).
: ROW-ELT-FAM? ( n n -- bool ) {: t:n fam:n :}   \ term t resolves to a T-PARAM of family fam?
   t T-RES TAG T-PARAM <> IF RES-FALSE EXIT THEN
   t T-RES PARAM>FAM fam = ;

: ROW-HAS-FAM? ( n n -- bool ) {: fam:n :}       \ ( row fam -- bool ) : row mentions family fam?
   fam 0= IF drop RES-FALSE EXIT THEN
   BEGIN dup R-RES TAG S-PUSH = WHILE
      dup R-RES P>TYPE fam ROW-ELT-FAM? IF drop RES-TRUE EXIT THEN
      R-RES P>REST
   REPEAT
   drop RES-FALSE ;

: PTX-REDUCE-ROWS? ( n n -- bool ) {: din:n dout:n :}   \ tile-in / uniform-out block reduction?
   PTX-UNIFORM-FAM @ 0= IF RES-FALSE EXIT THEN
   dout PTX-UNIFORM-FAM @ ROW-HAS-FAM? 0= IF RES-FALSE EXIT THEN
   din PTX-TILE-FAM @ ROW-HAS-FAM? ;

\ cp.async WAIT: committed-in / ready-out. The only sound way to make a ready slot
\ is to retire the copy group and bar.sync-fence it, so this shape is a block
\ barrier exactly like the tile->uniform reduction (M5), and composes at the same
\ choke rather than duplicating it.
: PTX-CPWAIT-ROWS? ( n n -- bool ) {: din:n dout:n :}
   PTX-CPREADY-FAM @ 0= IF RES-FALSE EXIT THEN
   dout PTX-CPREADY-FAM @ ROW-HAS-FAM? 0= IF RES-FALSE EXIT THEN
   din PTX-CPCOMMITTED-FAM @ ROW-HAS-FAM? ;

: PTX-BARRIER-ROWS? ( n n -- bool ) {: din:n dout:n :}   \ block collective (reduction or cp.async fence)?
   din dout PTX-REDUCE-ROWS? IF RES-TRUE EXIT THEN
   din dout PTX-CPWAIT-ROWS? ;

: FIELD-REC ( n -- n )
   0 PARAM>ARG ;

: FIELD-NAME ( n -- n )
   1 PARAM>ARG ;

: FIELD-INNER ( n -- n )
   2 PARAM>ARG ;

: FIELD-ATOM-SAME? ( n n -- bool ) {: a:n b:n :}
   a T-RES TAG T-ATOM <> IF RES-FALSE EXIT THEN
   b T-RES TAG T-ATOM <> IF RES-FALSE EXIT THEN
   a T-RES ATOM>A a T-RES ATOM>U
   b T-RES ATOM>A b T-RES ATOM>U CORE-STR= ;

: FIELD-ID-SAME? ( n n -- bool ) {: a:n b:n :}
   a T-RES FIELD-REC b T-RES FIELD-REC FIELD-ATOM-SAME? 0= IF RES-FALSE EXIT THEN
   a T-RES FIELD-NAME b T-RES FIELD-NAME FIELD-ATOM-SAME? ;

: FIELD-PAIR? ( n n -- bool ) {: got:n want:n :}
   got FIELD-PARAM? want FIELD-PARAM? and 0= IF RES-FALSE EXIT THEN
   got want FIELD-ID-SAME? 0= IF got want U-FAIL RES-TRUE EXIT THEN
   got FIELD-INNER want FIELD-INNER PAIR
   RES-TRUE ;

: FIELD-COERCE? ( n n -- bool ) {: got:n want:n :}
   UNIFY-KIND @ UK-COERCE <> IF RES-FALSE EXIT THEN
   got FIELD-PARAM? IF got FIELD-INNER want PAIR RES-TRUE EXIT THEN
   want FIELD-PARAM? IF got want FIELD-INNER PAIR RES-TRUE EXIT THEN
   RES-FALSE ;

\ occurs check: binding a row var to a spine containing itself would make the
\ row cyclic — including THROUGH a quotation's effect rows (the ω-combinator
\ must reject, never loop). Recursion depth is bounded by term size; the
\ accumulator rides the stack (a shared variable would be clobbered by the
\ recursive calls).
: ROW-OCC? ( n n -- bool ) {: r:n s:n :}
   RES-FALSE s                           \ ( acc cur )
   BEGIN R-RES dup TAG S-PUSH = WHILE
     dup P>TYPE T-RES
     BEGIN dup TAG T-PTR = WHILE PTR>INNER T-RES REPEAT
     dup TAG T-QUOT = IF
       r over Q>DIN RECURSE  swap        \ ( acc cur f1 qt )
       r over Q>DOUT RECURSE  swap       \ ( acc cur f1 f2 qt )
       r over Q>RIN RECURSE  swap        \ ( acc cur f1 f2 f3 qt )
       r swap Q>ROUT RECURSE             \ ( acc cur f1 f2 f3 f4 )
       or or or  rot or swap             \ ( acc' cur )
     ELSE drop THEN
     P>REST
   REPEAT
   r = or ;

1 constant CC-N     2 constant CC-F     3 constant CC-R
4 constant CC-I64   5 constant CC-U8    6 constant CC-U32   7 constant CC-CELL
8 constant CC-CHAR  9 constant CC-STR  10 constant CC-ADDR  11 constant CC-BOOL
12 constant CC-IDX  13 constant CC-LEN  14 constant CC-COUNT 15 constant CC-OFF
16 constant CC-FD   17 constant CC-RC   18 constant CC-PID   19 constant CC-MS
20 constant CC-NS   21 constant CC-TOK  22 constant CC-REG   23 constant CC-LABEL
24 constant CC-VA   25 constant CC-SYMIDX 26 constant CC-ASM
27 constant CC-IMG  28 constant CC-SNAP  29 constant CC-F32
30 constant CC-U16 31 constant CC-MAX
256 constant CT-CAP-INIT       \ signature type table records (grows on demand)
4096 constant CT-STR-INIT       \ signature type string pool (grows on demand)
variable CT-CAP-V   CT-CAP-INIT CT-CAP-V !
: CT-CAP ( -- n ) CT-CAP-V @ ;
variable CT-STR-CAP-V   CT-STR-INIT CT-STR-CAP-V !
: CT-STR-CAP ( -- n ) CT-STR-CAP-V @ ;

0 constant CT-NONE
1 constant CT-INT
2 constant CT-ROLE
3 constant CT-BOOL
4 constant CT-FLOAT
5 constant CT-OBJ
6 constant CT-LINEAR

0 constant CS-NONE
1 constant CS-GENERIC
2 constant CS-SIGNED
3 constant CS-UNSIGNED
4 constant CS-ADDR

\ Registry stores keep a baked DATA boot buffer (stable, always baked because
\ build-time defs never exceed the boot cap) and grow into anon mmap on demand
\ via the shared ARENA-BYTES-GROW layer. The record arrays hold pointers INTO
\ CT-STR; a CT-STR relocation rebases them (CT-STR-REBASE). Snapshot persist
\ bakes any grown store into fresh DATA and rebases (CT-SNAPSHOT-PERSIST).
create CT-NAME-A-BOOT CT-CAP-INIT cells allot
create CT-NAME-U-BOOT CT-CAP-INIT cells allot
create CT-CLASS-BOOT CT-CAP-INIT cells allot
create CT-WIDTH-BOOT CT-CAP-INIT cells allot
create CT-SIGN-BOOT CT-CAP-INIT cells allot
create CT-STR-BOOT CT-STR-INIT allot
variable CT-NAME-A-P   variable CT-NAME-U-P   variable CT-CLASS-P
variable CT-WIDTH-P    variable CT-SIGN-P     variable CT-STR-P
variable CTN
variable CT-STR-U
variable CT-I
variable CT-J
variable CT-DST

: CT-ARENA-BOOT ( -- )          \ point every CT store at its boot buffer
   CT-NAME-A-BOOT CT-NAME-A-P !   CT-NAME-U-BOOT CT-NAME-U-P !
   CT-CLASS-BOOT CT-CLASS-P !     CT-WIDTH-BOOT CT-WIDTH-P !
   CT-SIGN-BOOT CT-SIGN-P !       CT-STR-BOOT CT-STR-P ! ;
CT-ARENA-BOOT
: CT-NAME-A ( -- ptr a ) CT-NAME-A-P @ ;
: CT-NAME-U ( -- ptr a ) CT-NAME-U-P @ ;
: CT-CLASS ( -- ptr a ) CT-CLASS-P @ ;
: CT-WIDTH ( -- ptr a ) CT-WIDTH-P @ ;
: CT-SIGN ( -- ptr a ) CT-SIGN-P @ ;
: CT-STR ( -- ptr u8 ) CT-STR-P @ ;

1 CTN !
0 CT-STR-U !

: CT-NAME-FIELD ( n -- ptr ptr u8 )
   cells CT-NAME-A + 0 ptr-field ;

: CT-DST-FIELD ( -- ptr ptr u8 )
   CT-DST 0 ptr-field ;

: CT-DST@ ( -- ptr u8 )
   CT-DST-FIELD @ ;

: CT-DST! ( ptr u8 -- )
   CT-DST-FIELD ! ;

\ CT-GROW ( need -- ) : geometric grow of the record arrays to hold code `need`.
\ The arrays hold pointers into CT-STR (unmoved here), so a plain cell copy needs
\ no rebase.
: CT-GROW ( n -- ) {: need:n :}
   need CT-CAP-V @ 2 * max {: nc:n :}
   CT-CAP-V @ cells {: ob:n :}   nc cells {: nb:n :}
   CT-NAME-A-P ob nb REG-GROW1   CT-NAME-U-P ob nb REG-GROW1
   CT-CLASS-P ob nb REG-GROW1    CT-WIDTH-P ob nb REG-GROW1
   CT-SIGN-P ob nb REG-GROW1
   nc CT-CAP-V ! ;

: CT-ENSURE ( n -- ) {: need:n :}   \ ensure record cap can index code `need`
   need CT-CAP-V @ < IF exit THEN
   need 1 + CT-GROW ;

\ CT-STR-REBASE ( delta -- ) : a CT-STR relocation moved the pool by delta; add
\ it to every already-stored name pointer so records still resolve.
: CT-STR-REBASE ( n -- ) {: delta:n :}
   1 CT-I !
   begin CT-I @ CTN @ < while
      CT-I @ CT-NAME-FIELD {: fld:ptr :}
      fld @ delta + fld !
      CT-I @ 1 + CT-I !
   repeat ;

: CT-STR-GROW ( n -- ) {: need:n :}
   need CT-STR-CAP-V @ 2 * max {: nc:n :}
   CT-STR-P @ {: old:ptr :}
   old CT-STR-CAP-V @ nc ARENA-BYTES-GROW {: new:ptr :}
   new CT-STR-P !   nc CT-STR-CAP-V !
   new old - CT-STR-REBASE ;

: CT-STR-ENSURE ( n -- ) {: add:n :}   \ ensure room for `add` more string bytes
   CT-STR-U @ add + CT-STR-CAP-V @ <= IF exit THEN
   CT-STR-U @ add + CT-STR-GROW ;

: CT-CODE-CHECK ( n -- )
   dup 0 <= IF s" checker: bad signature type code" 76 die THEN
   drop ;

: CT-ROOM ( n -- )              \ ensure the CT-STR pool holds `n` more bytes
   CT-STR-ENSURE ;

: CT-COPY ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   u CT-ROOM
   CT-STR CT-STR-U @ + CT-DST!
   0 CT-J !
   begin CT-J @ u < while
      a CT-J @ + c@ CT-DST@ CT-J @ + c!
      CT-J @ 1 + CT-J !
   repeat
   CT-STR-U @ u + CT-STR-U !
   CT-DST@ u ;

: CT-ADVANCE ( n -- )
   1 + dup CTN @ > IF CTN ! ELSE drop THEN ;

: CT-SET ( ptr u8 n n n n n -- ) {: a:ptr u:n code:n class:n width:n sign:n :}
   code CT-CODE-CHECK
   code CT-ENSURE
   a u CT-COPY {: dst:ptr len:n :}
   dst code CT-NAME-FIELD !
   len code cells CT-NAME-U + !
   class code cells CT-CLASS + !
   width code cells CT-WIDTH + !
   sign code cells CT-SIGN + !
   code CT-ADVANCE ;

: CT-INIT ( -- )
   s" n"       CC-N      CT-INT   64 CS-GENERIC CT-SET
   s" f"       CC-F      CT-BOOL   1 CS-NONE    CT-SET
   s" r"       CC-R      CT-FLOAT 64 CS-NONE    CT-SET
   s" i64"     CC-I64    CT-INT   64 CS-SIGNED  CT-SET
   s" u8"      CC-U8     CT-INT    8 CS-UNSIGNED CT-SET
   s" u32"     CC-U32    CT-INT   32 CS-UNSIGNED CT-SET
   s" cell"    CC-CELL   CT-INT   64 CS-GENERIC CT-SET
   s" char"    CC-CHAR   CT-INT    8 CS-UNSIGNED CT-SET
   s" str"     CC-STR    CT-OBJ    0 CS-NONE    CT-SET
   s" addr"    CC-ADDR   CT-INT   64 CS-ADDR    CT-SET
   s" bool"    CC-BOOL   CT-BOOL   1 CS-NONE    CT-SET
   s" idx"     CC-IDX    CT-ROLE  64 CS-NONE    CT-SET
   s" len"     CC-LEN    CT-ROLE  64 CS-NONE    CT-SET
   s" count"   CC-COUNT  CT-ROLE  64 CS-NONE    CT-SET
   s" off"     CC-OFF    CT-ROLE  64 CS-NONE    CT-SET
   s" fd"      CC-FD     CT-ROLE  64 CS-NONE    CT-SET
   s" rc"      CC-RC     CT-ROLE  64 CS-NONE    CT-SET
   s" pid"     CC-PID    CT-ROLE  64 CS-NONE    CT-SET
   s" ms"      CC-MS     CT-ROLE  64 CS-NONE    CT-SET
   s" ns"      CC-NS     CT-ROLE  64 CS-NONE    CT-SET
   s" tok"     CC-TOK    CT-ROLE  64 CS-NONE    CT-SET
   s" reg"     CC-REG    CT-ROLE  64 CS-NONE    CT-SET
   s" label"   CC-LABEL  CT-ROLE  64 CS-NONE    CT-SET
   s" va"      CC-VA     CT-ROLE  64 CS-NONE    CT-SET
   s" symidx"  CC-SYMIDX CT-ROLE  64 CS-NONE    CT-SET
   s" asm"     CC-ASM    CT-ROLE  64 CS-NONE    CT-SET
   s" img"     CC-IMG    CT-ROLE  64 CS-NONE    CT-SET
   s" snap"    CC-SNAP   CT-ROLE  64 CS-NONE    CT-SET
   s" f32"     CC-F32    CT-FLOAT 32 CS-NONE    CT-SET
   s" u16"     CC-U16    CT-INT   16 CS-UNSIGNED CT-SET ;

CT-INIT

: CT-CLASS@ ( n -- n )
   cells CT-CLASS + @ ;

: CT-WIDTH@ ( n -- n )
   cells CT-WIDTH + @ ;

: CT-SIGN@ ( n -- n )
   cells CT-SIGN + @ ;

: CT-LIVE? ( n -- bool ) {: code:n :}
   code 0 > code CTN @ < and ;

: CT-INT? ( n -- bool )
   CT-CLASS@ CT-INT = ;

: CT-LINEAR? ( n -- bool )
   CT-CLASS@ CT-LINEAR = ;

: CT-NAME$ ( n -- ptr u8 n )
   dup CT-NAME-FIELD @
   swap cells CT-NAME-U + @ ;

: CT-NAME= ( ptr u8 n n -- bool ) {: a:ptr u:n code:n :}
   code CT-NAME$ a u CORE-STR= ;

: CT-FIND ( ptr u8 n -- n ) {: a:ptr u:n :}
   1 CT-I !
   begin CT-I @ CTN @ < while
      a u CT-I @ CT-NAME= IF CT-I @ exit THEN
      CT-I @ 1 + CT-I !
   repeat 0 ;

: INT-FAM? ( n -- bool ) {: code:n :}
   code CT-INT? ;

: INT-WIDENS? ( n n -- bool ) {: got:n want:n :}
   got want = IF RES-TRUE EXIT THEN
   got INT-FAM? want INT-FAM? and 0= IF RES-FALSE EXIT THEN
   got CC-N = IF RES-TRUE EXIT THEN
   want CC-N = IF RES-TRUE EXIT THEN
   got CT-WIDTH@ want CT-WIDTH@ <= 0= IF RES-FALSE EXIT THEN
   got CT-SIGN@ CS-GENERIC = IF RES-TRUE EXIT THEN
   want CT-SIGN@ CS-GENERIC = IF RES-TRUE EXIT THEN
   got CT-SIGN@ want CT-SIGN@ = IF RES-TRUE EXIT THEN
   got CT-SIGN@ CS-UNSIGNED = want CT-SIGN@ CS-SIGNED = and
   got CT-WIDTH@ want CT-WIDTH@ < and ;

: UNIFY-WIDEN? ( -- bool )
   UNIFY-KIND @ UK-INPUT = IF RES-TRUE EXIT THEN
   UNIFY-KIND @ UK-COERCE = ;

\ CON-OK? ( t1 t2 -- f ) : exact joins require the same concrete code except for
\ generic n/int-family interaction. Input/output checks use the integer lattice:
\ a narrower concrete int can flow into a wider one; widening never applies to
\ nominal roles (pid/fd/rc/idx/len/...), which stay strict.
\ A pointee (CUR-STRICT) admits NEITHER relaxation: an element type is part of
\ the pointer's identity, so two concrete elements must carry the SAME code.
\ Both the width lattice AND generic `n` absorption are value-position rules —
\ `n` is the generic 64-bit stack cell, not a wildcard element — so allowing
\ either one under a pointer let `ptr n` and `ptr u8` alias each other and a
\ caller read a byte span with cell `@` (or a cell span with `c@`) uncaught.
: CON-OK? ( n n -- bool ) {: t1:n t2:n :}
   t1 PAY t2 PAY = IF RES-TRUE EXIT THEN
   CUR-STRICT @ 0 <> IF RES-FALSE EXIT THEN
   UNIFY-WIDEN? IF t1 PAY t2 PAY INT-WIDENS? EXIT THEN
   t1 PAY CC-N = t2 PAY INT-FAM? and IF RES-TRUE EXIT THEN
   t2 PAY CC-N = t1 PAY INT-FAM? and IF RES-TRUE EXIT THEN
   RES-FALSE ;

: ATOM-OK? ( n n -- bool ) {: t1:n t2:n :}
   t1 ATOM>K t2 ATOM>K <> IF RES-FALSE EXIT THEN     \ kinds differ -> reject
   t1 ATOM>K 0 < IF RES-FALSE EXIT THEN              \ rigid template slot -> never unify
   \ Equal non-negative kind is NOT sufficient authority: a fresh rigid id (kind>0)
   \ carries a domain in its name, and per-domain counters can mint equal ids in
   \ different domains, so the name must also match. Same id always implies same
   \ name in the single shared-counter case, so this rejects nothing that unified.
   t1 ATOM>A t1 ATOM>U t2 ATOM>A t2 ATOM>U CORE-STR= ;

: PARAM-FAM-OK? ( n n -- bool ) {: t1:n t2:n :}
   t1 PARAM>FAM t2 PARAM>FAM = ;   \ identity by resolved family-id, not folded spelling

: PARAM-PAIR-ARGS ( n n -- ) {: t1:n t2:n :}
   t1 PARAM>ARGC t2 PARAM>ARGC <> IF t1 t2 U-FAIL EXIT THEN
   t1 t2 PARAM-FAM-OK? 0= IF t1 t2 U-FAIL EXIT THEN
   0 PARAM-I !
   BEGIN PARAM-I @ t1 PARAM>ARGC < WHILE
      t1 PARAM-I @ PARAM>ARG  t2 PARAM-I @ PARAM>ARG  PAIR
      PARAM-I @ 1 + PARAM-I !
   REPEAT ;


\ --- fail-closed depth backstop for the recursive term walkers (TY-OCC?,
\ E-COPY, LIN-TYPE-COUNT). Terms are finite DAGs (the occurs check keeps
\ bindings acyclic) whose STRUCTURAL depth is small — hundreds at most — so a
\ real walk never nears TWALK-MAX-DEPTH. A cyclic or mis-indexed term instead
\ descends without bound; the guard trips far below the native stack limit and
\ dies with a named diagnostic instead of overflowing the stack (SIGSEGV). A
\ call-count budget cannot help: the native stack blows at ~80k frames, so the
\ bound must track DEPTH, not total steps. TWALK-DEEPER/TWALK-SHALLOWER bracket
\ each RECURSE (charge on descent, release when the child returns — exit-safe
\ however the child returns); each public wrapper resets depth before descending.
$2000 constant TWALK-MAX-DEPTH     \ 8192: >> any finite term depth, << native stack limit
variable TWALK-D
: TWALK-RESET ( -- ) 0 TWALK-D ! ;
: TWALK-DEEPER ( -- )
   TWALK-D @ 1 + dup TWALK-D !
   TWALK-MAX-DEPTH > IF s" checker: term walk too deep (cyclic term)" 76 die THEN ;
: TWALK-SHALLOWER ( -- ) TWALK-D @ 1 - TWALK-D ! ;

\ TY-OCC? ( n n -- bool ) : does tyvar v occur in type/row t, descending
\ through quotation effect rows and parameter arguments.
: TY-OCC?* ( n n -- bool ) {: v:n t:n :}
   t R-RES dup TAG S-PUSH = IF
      BEGIN dup TAG S-PUSH = WHILE
         dup P>TYPE v swap TWALK-DEEPER RECURSE TWALK-SHALLOWER IF drop RES-TRUE EXIT THEN
         P>REST R-RES
      REPEAT drop RES-FALSE EXIT
   THEN drop
   t T-RES {: x:n :}
   x TAG T-VAR = IF x PAY v = EXIT THEN
   x TAG T-PTR = IF v x PTR>INNER TWALK-DEEPER RECURSE TWALK-SHALLOWER EXIT THEN
   x TAG T-QUOT = IF
      v x Q>DIN TWALK-DEEPER RECURSE TWALK-SHALLOWER IF RES-TRUE EXIT THEN
      v x Q>DOUT TWALK-DEEPER RECURSE TWALK-SHALLOWER IF RES-TRUE EXIT THEN
      v x Q>RIN TWALK-DEEPER RECURSE TWALK-SHALLOWER IF RES-TRUE EXIT THEN
      v x Q>ROUT TWALK-DEEPER RECURSE TWALK-SHALLOWER
      EXIT
   THEN
   x TAG T-PARAM = IF
      0 BEGIN dup x PARAM>ARGC < WHILE       \ data-stack index (RECURSE-safe)
         x over PARAM>ARG                    \ ( i arg )
         v swap TWALK-DEEPER RECURSE TWALK-SHALLOWER IF drop RES-TRUE EXIT THEN
         1 +
      REPEAT drop
      RES-FALSE EXIT
   THEN
   RES-FALSE ;
: TY-OCC? ( n n -- bool ) TWALK-RESET TY-OCC?* ;

\ --- item 7 (docs/type-families.md §10-11, PLAN item 7, reject-only): a logical
\ sum/enum/product layout value is ONE T-PARAM cell in a signature and is NOT
\ expanded to hidden physical fields until item 12's width-aware lowering can
\ preserve whole bundles across generic stack ops. Until then an ordinary
\ one-cell primitive (dup/drop/swap/over/nip/>r/...) that would bind or consume
\ the logical layout cell fails closed in U-TYPE. Layout identity is the resolved
\ family-id, so a layout cell unifying with the SAME family (the PARAM-PAIR-ARGS
\ arm) flows fine; only a var/con/ptr/atom pairing reaches this guard.
: LAYOUT-PARAM? ( n -- bool ) {: t:n :}
   t T-RES TAG T-PARAM <> IF RES-FALSE EXIT THEN
   t T-RES PARAM>FAM dup 0 < IF drop RES-FALSE EXIT THEN
   TFAM-LAYOUT?* ;
: LAYOUT-EITHER? ( n n -- bool ) {: t1:n t2:n :}
   t1 LAYOUT-PARAM? IF RES-TRUE EXIT THEN
   t2 LAYOUT-PARAM? ;

\ A NOMINAL SCALAR is an arity-0 TK-CELL family T-PARAM (CAD-KIND ids and
\ friends): one raw cell whose whole meaning is its family identity. Every raw
\ bit pattern is a valid value of the family (id 0 included), so it lowers as a
\ plain scalar cell; the family identity itself is introduction-governed like a
\ layout — LAYOUT-BUFFER is the only checked source of `ptr <nominal-scalar>`.
: NOM-SCALAR? ( n -- bool ) {: t0:n :}   \ term resolves to a nominal-scalar family param
   t0 T-RES {: t:n :}
   t TAG T-PARAM <> IF RES-FALSE EXIT THEN
   t PARAM>HID 0 > IF RES-FALSE EXIT THEN
   t PARAM>FAM dup 0 < IF drop RES-FALSE EXIT THEN
   dup TFAM-CELL?* 0= IF drop RES-FALSE EXIT THEN
   TFAM-ARITY* 0 = ;

\ T-WIDTH ( n -- n ) : logical width in stack cells of a resolved type term
\ (docs §18 WIDTH function). Every non-layout term is one cell; a layout-family
\ T-PARAM asks the INSTANTIATED width (arg-aware): the registry walks the term's
\ variant/product schemas substituting each param slot by its resolved arg width,
\ so a layout arg widens the sum payload / product body. Every cell-kinded
\ instantiation (all args width 1) yields the declared TFAM-WIDTH@, so this is
\ behaviour-preserving groundwork while family parameters stay cell-kinded. The
\ the registry-not-loaded default (bound in TFAM-QUERY-DEFAULTS) falls back to the
\ declared family width verbatim (the term's own family, since a cell-kinded
\ instantiation equals the declared width); type-family.f rebinds the hook to the
\ arg-aware TFAM-INST-WIDTH@. This is the type-level fact the WF- surface records.
: T-WIDTH ( n -- n ) {: t:n :}
   t T-RES {: r:n :}
   r TAG T-PARAM <> IF 1 EXIT THEN
   r PARAM>FAM {: fam:n :}
   fam 0 < IF 1 EXIT THEN
   fam TFAM-LAYOUT?* 0= IF 1 EXIT THEN
   r TFAM-INST-WIDTH-XT ;

\ --- item 12 slice-3a/3b: hidden physical-field substrate (docs §10-11, §17-18).
\ PUSH-LOGICAL expands a logical layout T-PARAM in a signature into W hidden
\ physical field terms: payload slots 0..W-2 then the tag at W-1 (docs §5). Each
\ hidden term reuses the logical family-id, name string, and arg terms, but
\ carries a slot+1 in PARAMHID (0 = logical). A hidden term is checker-owned: it
\ pairs only with the SAME family AND slot and never binds a var/con/ptr/atom
\ (docs §10), enforced in U-TYPE below. Whole-bundle transports move the W-cell
\ group by direct row surgery (XPORT-STEP?, further down).
: HIDDEN-PARAM? ( n -- bool ) {: t:n :}     \ resolved term is a hidden physical field
   t T-RES TAG T-PARAM <> IF RES-FALSE EXIT THEN
   t T-RES PARAM>HID 0 > ;
: HIDDEN-SLOT@ ( n -- n ) {: t:n :}         \ physical slot index; dies on a non-hidden term
   t HIDDEN-PARAM? 0= IF s" checker: hidden-slot@ on non-hidden param" 76 die THEN
   t T-RES PARAM>HID 1 - ;
\ MK-HIDDEN ( n n -- n ) : mint the hidden field term for slot `slot:n` of a
\ resolved LOGICAL layout T-PARAM `src0:n`, copying src's name/family-id/arg terms
\ and tagging PARAMHID = slot+1. `slot` ranges over [0,W): 0..W-2 payload slots,
\ W-1 the tag. Non-layout src or out-of-range slot fails closed (die 76).
: MK-HIDDEN ( n n -- n ) {: src0:n slot:n :}
   src0 T-RES {: src:n :}
   src LAYOUT-PARAM? 0= IF s" checker: mk-hidden on non-layout param" 76 die THEN
   src T-WIDTH {: w:n :}                     \ instantiated (arg-aware) hidden-field count
   slot 0 < slot w >= or IF s" checker: mk-hidden slot out of range" 76 die THEN
   PARAM-SCR-N @ {: base:n :}
   0 BEGIN dup src PARAM>ARGC < WHILE        \ copy src's arg run into the reentrant scratch
      src over PARAM>ARG PARAM-SCR+
      1 +
   REPEAT drop
   base src PARAM>NAME-A src PARAM>NAME-U src PARAM>FAM MK-PARAM {: t:n :}
   slot 1 + t PAY cells PARAMHID + !
   t ;
\ MK-LOGICAL ( n -- n ) : the logical family term for a resolved hidden field —
\ same name/family-id/arg terms, PARAMHID 0. Renderer compaction folds a full
\ hidden run back to this logical type (docs §20); checking never consumes it.
: MK-LOGICAL ( n -- n ) {: src0:n :}
   src0 T-RES {: src:n :}
   src HIDDEN-PARAM? 0= IF s" checker: mk-logical on non-hidden param" 76 die THEN
   PARAM-SCR-N @ {: base:n :}
   0 BEGIN dup src PARAM>ARGC < WHILE
      src over PARAM>ARG PARAM-SCR+
      1 +
   REPEAT drop
   base src PARAM>NAME-A src PARAM>NAME-U src PARAM>FAM MK-PARAM ;
\ LAYOUT-PUSH-FIELDS ( n n -- n ) : push `type:n`'s W hidden fields onto `row:n`
\ in physical order — slot0 deepest, tag (W-1) on top (docs §5). Slice 3b calls
\ this from PUSH-LOGICAL; in slice 3a only the new fixtures drive it.
: LAYOUT-PUSH-FIELDS ( n n -- n ) {: type:n row:n :}
   type T-RES {: src:n :}
   src T-WIDTH {: w:n :}                     \ instantiated (arg-aware) hidden-field count
   row 0 BEGIN dup w < WHILE                 \ ( row slot )
      dup src swap MK-HIDDEN                 \ ( row slot hid )
      rot MK-PUSH swap                       \ push hid onto row -> ( row' slot )
      1 +
   REPEAT drop ;
\ PARAM-HID-OK? ( n n -- bool ) : may two resolved T-PARAM terms pair? Neither
\ hidden -> ordinary logical pair; exactly one hidden -> reject (a hidden field
\ must not unify with the logical value); both hidden -> require the SAME slot
\ (slot+1 equality). Family equality is still enforced by PARAM-PAIR-ARGS.
: PARAM-HID-OK? ( n n -- bool ) {: t1:n t2:n :}
   t1 PARAM>HID t2 PARAM>HID {: h1:n h2:n :}
   h1 h2 or 0= IF RES-TRUE EXIT THEN
   h1 0 > h2 0 > and 0= IF RES-FALSE EXIT THEN
   h1 h2 = ;

\ --- item 12 (docs/type-families.md §17): a NON-linear layout value expands to
\ its W hidden physical fields (PUSH-LOGICAL), and whole-bundle transports move
\ the group by direct row surgery (XPORT-STEP?). A POSSIBLY-LINEAR layout stays
\ ONE logical T-PARAM cell (docs §19); for it, a whole-bundle transport op
\ (dup/drop/swap/over/nip/rot/-rot/tuck/2dup/2drop/2swap/2over, >r/r>/r@ and
\ friends, and locals capture) would move the value through its fresh transport
\ var — LAYOUT-XPORT is set by DO-TOK1/LOC-BIND only while checking such an op,
\ and LAYOUT-XPORT-ALLOW? still rejects the possibly-linear bind, keeping the
\ TFAM-11 fail-closed semantics. Every OTHER touch (value-inspecting prims,
\ ?dup, control predicates, higher-order apply, con/ptr/atom pairings) fails
\ closed through the same guards.
variable LAYOUT-XPORT
variable LAYOUT-INTRO
\ --- field-projection armed window (dot habu-checker-type-structure). A sealed,
\ schema-aware window that lets a generated FAMILY:FIELD accessor body mint
\ `ptr <field-type>` from a `ptr family<args>` input and a committed field id —
\ the one shape (byte offset added to a layout pointer, retyped to the field's
\ instantiated schema) that is otherwise fail-closed (`+`/`cell+` preserve the
\ pointee, CAST refuses T-PTR). Armed ONLY by the generative crossing (the path
\ structure-make.f/generate-field use) around the one audited accessor eval, and
\ keyed on the accessor word name; it fires once at the `field-project` op token
\ inside that word's body and disarms. The arming word is pre-hook (checker.f
\ before the check hook) so the seal-time internal-word pass marks it internal —
\ user source can neither execute nor tick it, exactly like CTOR-PEND!.
variable FIELD-PROJ-A   variable FIELD-PROJ-U   0 FIELD-PROJ-U !   \ armed accessor word name span
variable FIELD-PROJ-FID     \ committed field id (the authority: offset/type/role derive from it)
variable FIELD-PROJ-OFF     \ baked byte offset (cross-checked against the committed offset)
variable LBUF-PEND-A
variable LBUF-PEND-U   0 LBUF-PEND-U !

\ Linear guard (dot: "possibly-linear layout copies reject until TFAM 11"): a
\ layout whose family args contain a linear con — or an arg still unresolved,
\ which may LATER bind linear — must not transport. The linear discipline
\ counts concrete linear cons on the rows; a layout T-PARAM hides its payload
\ from that count, so dup/over/tuck/2dup would duplicate the payload resource,
\ drop/nip/2drop would lose it, and locals capture launders the reference
\ count. Fail closed on both proven and possible linears; TFAM 11 replaces
\ this with whole-bundle linear counting.
\ layout-cap slice 5 (dot habu-checker-capability-layout-9b8540bd): linearity is
\ TRANSITIVE through nested layout args. A NESTED family application arg (e.g. the
\ lq2<ltok,n> inside option<lq2<ltok,n>>) is a T-PARAM, not a con, so the direct
\ con/var test misses the linear ltok buried in it; the outer bundle would then
\ dup/drop-launder the resource. LAYOUT-ARG-LINEARISH? RECURSES into such a nested
\ layout (self-recursive on the arg subtree — NOT a forward `defer`, which before
\ `: TRUST` would be captured into the pre-trust pending table and break the
\ old-engine-compat boot property, test/pre-trust-defer.f), treating the whole
\ subtree as possibly-linear if any leaf con is linear or any family is
\ concrete-linear.
: LAYOUT-ARG-LINEARISH? ( n -- bool ) {: t:n :}   \ arg term: linear con, unresolved var, or nested linear layout
   t T-RES {: r:n :}
   r ISVAR IF RES-TRUE EXIT THEN
   r TAG T-PARAM = IF                              \ nested layout arg: any arg linear, or the family concrete-linear
      0 BEGIN dup r PARAM>ARGC < WHILE
         r over PARAM>ARG RECURSE IF drop RES-TRUE EXIT THEN
         1 +
      REPEAT drop
      r PARAM>FAM TFAM-CON-LIN-XT EXIT
   THEN
   r TAG T-CON <> IF RES-FALSE EXIT THEN
   r PAY CT-LINEAR? ;
: LAYOUT-MAYBE-LINEAR? ( n -- bool ) {: p:n :}    \ resolved layout T-PARAM term
   0 BEGIN dup p PARAM>ARGC < WHILE
      p over PARAM>ARG LAYOUT-ARG-LINEARISH? IF drop RES-TRUE EXIT THEN
      1 +
   REPEAT drop
   p PARAM>FAM TFAM-CON-LIN-XT ;

\ --- whole-bundle linear accounting (item 11, docs §19). A sum/enum whose type
\ args include a linear con is ONE linear unit as a value: LAYOUT-LINEAR-COUNT
\ sums the linear cons among a layout term's (flat, cell-kinded) args, sampled
\ once per logical value — LIN-TYPE-COUNT counts a bundle only at its tag cell.
\ LAYOUT-ARGS-OPEN? is the width question (any arg still an unresolved var):
\ width-known layouts expand to hidden fields even when linear, so the checker
\ row and the runtime cells agree; open-arg layouts stay one conservative cell.
\ layout-cap slice 5: a nested layout arg contributes its OWN whole-bundle linear
\ count (recursed), so option<lq2<ltok,n>> carries the one linear unit buried in
\ the inner lq2 — sampled once at the outer bundle's tag by LIN-TYPE-COUNT, so no
\ double count. Self-recursive like LAYOUT-ARG-LINEARISH? above (NOT a forward
\ `defer`, which pre-`: TRUST` would be captured into the pending table). Both the
\ nested branch and LAYOUT-LINEAR-COUNT keep the accumulator on the STACK, so the
\ recursion is reentrant.
: LAYOUT-ARG-LIN-N ( n -- n ) {: t:n :}   \ linear units the arg contributes (con=1, nested layout=recursed subtree)
   t T-RES {: r:n :}
   r TAG T-PARAM = IF                              \ nested layout arg: sum its args' units + its own concrete-linear
      0                                            \ acc on the stack (reentrant)
      0 BEGIN dup r PARAM>ARGC < WHILE             \ ( acc j )
         r over PARAM>ARG RECURSE                   \ ( acc j nj )
         rot + swap                                 \ ( acc' j )
         1 +
      REPEAT drop
      r PARAM>FAM TFAM-CON-LIN-XT IF 1 + THEN
      EXIT
   THEN
   r TAG T-CON <> IF 0 EXIT THEN
   r PAY CT-LINEAR? IF 1 ELSE 0 THEN ;
: LAYOUT-LINEAR-COUNT ( n -- n ) {: p:n :}
   0                                            \ acc on the stack (reentrant)
   0 BEGIN dup p PARAM>ARGC < WHILE             \ ( acc j )
      p over PARAM>ARG LAYOUT-ARG-LIN-N          \ ( acc j nj )
      rot + swap                                 \ ( acc' j )
      1 +
   REPEAT drop
   p PARAM>FAM TFAM-CON-LIN-XT IF 1 + THEN ;
: LAYOUT-LINEAR? ( n -- bool ) LAYOUT-LINEAR-COUNT 0 <> ;
: LAYOUT-ARGS-OPEN? ( n -- bool ) {: p:n :}
   0 BEGIN dup p PARAM>ARGC < WHILE
      p over PARAM>ARG T-RES ISVAR IF drop RES-TRUE EXIT THEN
      1 +
   REPEAT drop RES-FALSE ;

\ --- storable layouts S2 (dot habu-checker-capability-typed-a480c423): every
\ closed non-linear layout with a physical cell may cross typed memory. W=1
\ uses scalar lowering; W>1 lowers from the token's WF fact. W=0 products have
\ no addressable representation and reject. Possibly-linear, including open
\ args that might later become linear, stays rejected until TFAM 11.
: LAYOUT-MEM-OK? ( n -- bool ) {: t:n :}        \ resolved logical layout T-PARAM
   t LAYOUT-MAYBE-LINEAR? IF RES-FALSE EXIT THEN
   t T-WIDTH 0 > ;

: LAYOUT-XPORT-ALLOW? ( n n -- bool ) {: a:n b:n :}
   LAYOUT-XPORT @ 0= IF RES-FALSE EXIT THEN     \ only inside a whole-bundle transport op
   a HIDDEN-PARAM? b HIDDEN-PARAM? or IF RES-FALSE EXIT THEN   \ a hidden field is not a single-cell bundle: never binds a var
   a LAYOUT-PARAM? IF                           \ var <-> layout-param bind: absorb the bundle
      a T-RES LAYOUT-MAYBE-LINEAR? IF RES-FALSE EXIT THEN
      b ISVAR EXIT THEN
   b LAYOUT-PARAM? IF
      b T-RES LAYOUT-MAYBE-LINEAR? IF RES-FALSE EXIT THEN
      a ISVAR EXIT THEN
   RES-FALSE ;                                  \ con/ptr/atom vs layout is never a bundle move

\ Nominal-scalar pointee governance (mirror of the layout pointee-bind rule):
\ inside a pointee (CUR-STRICT), a type variable may not absorb a nominal-scalar
\ family — a raw pointer (variable/create/data-base) would acquire the family
\ identity by ordinary unification, bypassing the LAYOUT-BUFFER introduction
\ form. Only the armed generated-accessor window (LAYOUT-INTRO) may form that
\ bind; nominal scalars are structurally non-linear W=1, so no mem-ok analog is
\ needed. Value-position binds (CUR-STRICT 0) stay ordinary: a nominal scalar is
\ a plain one-cell value everywhere else.
: NOMPTR-BLOCK? ( n n -- bool ) {: a:n b:n :}
   CUR-STRICT @ 0= IF RES-FALSE EXIT THEN
   LAYOUT-INTRO @ 0 <> IF RES-FALSE EXIT THEN
   a NOM-SCALAR? b ISVAR and IF RES-TRUE EXIT THEN
   b NOM-SCALAR? a ISVAR and ;

: LAYOUT-BLOCK? ( n n -- bool ) {: a:n b:n :}   \ a layout pairing this op may NOT form
   a b LAYOUT-EITHER? 0= IF a b NOMPTR-BLOCK? EXIT THEN
   a b LAYOUT-XPORT-ALLOW? IF RES-FALSE EXIT THEN
   LAYOUT-INTRO @ 0 <> CUR-STRICT @ 0 <> and IF
      a HIDDEN-PARAM? b HIDDEN-PARAM? or 0= IF
         a LAYOUT-PARAM? b ISVAR and IF a T-RES LAYOUT-MEM-OK? 0= EXIT THEN
         b LAYOUT-PARAM? a ISVAR and IF b T-RES LAYOUT-MEM-OK? 0= EXIT THEN
      THEN
   THEN
   RES-TRUE ;

\ --- nominal-storage RAW value discipline (habu-nominal-storage-raw). A RAW type
\ var is minted by a raw storage definer (here/create/variable/constant); a fetch
\ from that cell yields the SAME (RAW) var, so `RAW-var ~ nominal-family` in value
\ position is the mint the checker must refuse -- otherwise raw dictionary storage
\ forges the validated arity-0 CAD-KIND identities (target/toolchain/region/node).
\ This is the VALUE-position mirror of NOMPTR-BLOCK? (which governs the pointee
\ position). Ordinary (TVK-ANY) vars are unaffected, so a typed LAYOUT-BUFFER
\ fetch, a constructor, and role/converter words keep binding a family in value
\ position -- only a raw-storage-minted var is fenced. RAW-OK? both DECIDES
\ admissibility and PROPAGATES the kind (a var arg is raised RAW -- the meet; a
\ pointer's pointee is checked recursively). It REJECTS a NOMINAL-FAMILY / LAYOUT
\ value (a T-PARAM) and a LINEAR con, and ADMITS a plain scalar/role con, a plain
\ pointer, and an atom/xt: the engine's own codegen legitimately raw-stores role
\ cons (`LBL RT-LPOS !`) and xts (defer/hook cells like `cold-hook!`). Fencing
\ nominal ROLE atoms (idx/len) out of raw storage too needs that role/xt
\ scratch migrated to typed cells first, so it is a documented follow-on; this
\ dot closes the arity-0 nominal-family / layout mint, the epic's forgery target.
: RAW-OK? ( n -- bool )   \ may a RAW cell absorb resolved `term`? (meets var/pointee RAW)
   T-RES
   dup ISVAR IF PAY TVK-RAISE RES-TRUE EXIT THEN     \ var: meet -> RAW (trailed)
   dup TAG T-PARAM = IF drop RES-FALSE EXIT THEN     \ nominal family / layout -> reject (the mint)
   dup TAG T-CON = IF PAY CT-LINEAR? 0= EXIT THEN    \ plain scalar / role OK; linear con NO
   dup TAG T-PTR = IF PTR>INNER RECURSE EXIT THEN    \ ptr: pointee must also be RAW-admissible
   drop RES-TRUE ;                                    \ atom / xt / row: engine raw-stores these -> admit
: RAW-BLOCK? ( n n -- bool )   \ binding var `vid` to `term` violates the RAW cell discipline?
   over TVK-RAW? 0= IF 2drop RES-FALSE EXIT THEN     \ ordinary var: never blocks
   nip RAW-OK? 0= ;                                   \ RAW var: `term` must be RAW-admissible

: U-TYPE   \ ( t1 t2 -- ) resolve both; bind a var side, or require equal cons
   T-RES swap T-RES swap
   2dup = IF 2drop ELSE
   over TAG T-QUOT =  over TAG T-QUOT =  and IF
     2dup Q>DIN swap Q>DIN swap PAIR
     2dup Q>DOUT swap Q>DOUT swap PAIR
     2dup Q>RIN swap Q>RIN swap PAIR
     Q>ROUT swap Q>ROUT swap PAIR ELSE
   over TAG T-PTR =  over TAG T-PTR =  and IF
     over PTR>INNER over PTR>INNER PAIR-STRICT 2drop ELSE
   over TAG T-ATOM =  over TAG T-ATOM =  and IF
     2dup ATOM-OK? IF 2drop ELSE U-FAIL THEN ELSE
   2dup FIELD-PAIR? IF 2drop ELSE
   2dup FIELD-COERCE? IF 2drop ELSE
   over TAG T-PARAM =  over TAG T-PARAM =  and IF
     2dup PARAM-HID-OK? IF 2dup PARAM-PAIR-ARGS 2drop ELSE U-FAIL THEN ELSE   \ item 12 slice-3a: hidden field pairs only same-family same-slot
   2dup LAYOUT-BLOCK? IF U-FAIL ELSE   \ item 12: only a whole-bundle transport op may bind a layout cell
   over ISVAR IF
     over PAY over TY-OCC? IF U-FAIL ELSE
       over PAY over RAW-BLOCK? IF U-FAIL ELSE swap PAY TV! THEN THEN ELSE   \ raw discipline: RAW var rejects nominal/atom/layout
   dup ISVAR IF
     dup PAY  rot  tuck TY-OCC? IF U-FAIL ELSE
       over PAY over RAW-BLOCK? IF U-FAIL ELSE swap PAY TV! THEN THEN ELSE
   over TAG T-CON =  over TAG T-CON =  and IF
     2dup CON-OK? IF 2drop ELSE U-FAIL THEN
   ELSE U-FAIL THEN THEN THEN THEN THEN THEN THEN THEN THEN THEN THEN ;

\ --- logical<->hidden bundle coercion (item 11 slice 1, docs §18-19). A stored
\ effect keeps a parametric layout value as ONE logical cell whenever an arg is
\ an unresolved var or linear con (PUSH-LOGICAL's possibly-linear rule). At a
\ boundary or call site that cell can meet the W-cell hidden expansion of the
\ SAME family whose args are, by the PUSH-LOGICAL invariant, always fully
\ resolved and non-linear. Unifying the two proves the logical side's args
\ non-linear (they bind pairwise to the hidden side's), so the logical cell
\ expands to its hidden fields and the rows re-pair cell for cell. Genuinely
\ linear instantiations never reach here: linear-arg layout sigs reject at the
\ sig/borrow layer and PARAM-ARG-LIN-BLOCK? below refuses a var~linear-con arg
\ bind, so no hidden group can ever absorb a linear payload. Transport mode
\ (LAYOUT-XPORT) keeps its own group surgery and is excluded.
: LOGHID-AT? ( n n -- bool ) {: rh:n rl:n :}   \ rh top = group tag, rl top = logical same-family layout
   LAYOUT-XPORT @ 0 <> IF RES-FALSE EXIT THEN
   rh P>TYPE T-RES {: th:n :}
   rl P>TYPE T-RES {: tl:n :}
   th HIDDEN-PARAM? 0= IF RES-FALSE EXIT THEN
   tl TAG T-PARAM = 0= IF RES-FALSE EXIT THEN
   tl HIDDEN-PARAM? IF RES-FALSE EXIT THEN
   tl LAYOUT-PARAM? 0= IF RES-FALSE EXIT THEN
   th PARAM>FAM tl PARAM>FAM = 0= IF RES-FALSE EXIT THEN
   th HIDDEN-SLOT@ th T-WIDTH 1 - = ;   \ whole group: tag on top (arg-aware bundle width)
: LOGHID-EXPAND ( n n -- ) {: rh:n rl:n :}   \ expand rl's logical top, re-pair rows
   rl P>TYPE T-RES {: tl:n :}
   tl rl P>REST LAYOUT-PUSH-FIELDS {: re:n :}
   rh re PAIR ;

\ Root-spine position plumbing: a spine row pair carries encoded cursor
\ -(c+2); its type pair gets slot c, its rest pair cursor c+1. Non-spine row
\ pairs (nested rows inside a slot's quotation/param subterm) inherit the
\ enclosing slot unchanged, so nested descent never bumps the cursor.
: UPOS-SPINE? ( n -- bool ) -1 < ;
: UPOS-REST ( n -- n ) dup UPOS-SPINE? IF 1 - THEN ;         \ ENC(c)-1 = ENC(c+1)
: UPOS-TYPE ( n -- n ) dup UPOS-SPINE? IF negate 2 - THEN ;  \ ENC(c) -> slot c

: U-ROW-DESCEND ( n n -- ) {: r1:n r2:n :}
   CUR-UPOS @ {: e:n :}
   e UPOS-REST CUR-UPOS !  r1 P>REST r2 P>REST PAIR
   e UPOS-TYPE CUR-UPOS !  r1 P>TYPE r2 P>TYPE PAIR ;

\ LOGHID expansion re-pairs the whole row with W-1 extra hidden cells, so the
\ spine cursor is no longer slot-aligned past it: drop to no-position (fail
\ safe, positions before the expansion were already latched exactly).
: U-ROW R-RES swap R-RES swap 2dup = IF 2drop ELSE
   over ISROW IF 2dup ROW-OCC? IF 2drop RES-FALSE UOK ! ELSE swap PAY RV! THEN ELSE
   dup ISROW IF 2dup swap ROW-OCC? IF 2drop RES-FALSE UOK ! ELSE PAY RV! THEN ELSE
   2dup LOGHID-AT? IF -1 CUR-UPOS ! LOGHID-EXPAND ELSE
   2dup swap LOGHID-AT? IF -1 CUR-UPOS ! swap LOGHID-EXPAND ELSE
   U-ROW-DESCEND THEN THEN THEN THEN THEN ;

: UNIFY ( n n -- bool )   \ worklist-driven; rows and types interleave
   0 USP !  RES-TRUE UOK !  0 CUR-STRICT !
   0 UF-ACT !  0 UF-EXP !  0 UF-SET !
   -2 CUR-UPOS !  -1 UF-POSN !   \ root row pair opens the spine at cursor 0
   PAIR
   BEGIN USP @ 0 > UOK @ and WHILE
     UNPAIR  over TAG dup S-ROW = swap S-PUSH = or IF U-ROW ELSE U-TYPE THEN
   REPEAT
   UOK @ ;

: UNIFY-EXACT ( n n -- bool )
   UK-EXACT UNIFY-KIND !
   UNIFY ;

: UNIFY-IN ( n n -- bool )
   UK-INPUT UNIFY-KIND !
   UNIFY
   UK-EXACT UNIFY-KIND ! ;

: UNIFY-COERCE ( n n -- bool )
   UK-COERCE UNIFY-KIND !
   UNIFY
   UK-EXACT UNIFY-KIND ! ;
variable FV
0 FV !

: FRESH ( -- n )
   FV @ 1 + TV-ENSURE            \ grow the pool so FV is a valid index
   FV @ dup 1 + FV ! ;

\ TV-RESET ( -- ) : high-water reset — every var id comes from FRESH, so only
\ cells 0..FV-1 can be bound since the previous reset (TVINIT covers load time;
\ TRIAL-REST clears its own FV delta).
: TV-RESET
   0 BEGIN dup FV @ < WHILE
     dup cells TVT + UNBOUND swap !
     dup cells RVT + UNBOUND swap !
     dup cells TVK + 0 swap !
     1 +
   REPEAT drop ;
variable OK   variable DCUR   variable UNCK   variable BROW
variable RCUR   variable RBROW
variable THDROW  variable THRROW  variable THSET
variable XROW  variable XRROW  variable XSET  variable DEADP
variable DEADERR  variable DEADTA  variable DEADTU

: NEW ( -- )
   -1 OK ! 0 UNCK ! 0 SPN ! 0 USP ! TV-RESET 0 FV ! 0 QEN ! 0 PTRN !
   0 LAYOUT-XPORT !  0 LAYOUT-INTRO !
   TRAIL-RESET   0 TRIAL-DEPTH !   LIN-TAINT-RESET
   RIGID-RESET
   0 ATOMN ! 0 PARAMN ! 0 PARAM-SCR-N ! 0 PARG-N !
   FRESH MK-ROW dup BROW ! DCUR !
   FRESH MK-ROW dup RBROW ! RCUR ! ;
variable WAS   variable DEXP   variable DACT   variable FAILSET
variable DF-ACT   variable DF-EXP
\ ADT variant capture (item 13, docs/type-families.md §13): the sum-variant/tag
\ involved at a layout mismatch. CVLIVE holds the SUMV id of the variant a
\ `construct family variant` step is applying (its payload row is what
\ CHECKER-STEP unifies); UF>DIAG latches it into DVAR exactly when the first
\ failure is captured (beside DF-ACT/DF-EXP). -1 = no variant in scope, so a
\ pure-scalar or non-construct mismatch renders no variant/tag.
variable DVAR    -1 DVAR !
variable CVLIVE  -1 CVLIVE !
\ Payload slot of the captured mismatch (item 13): UF-POSN's slot-from-top of
\ the failed expected-row element, latched beside DVAR. Render converts it to
\ the declaration-order payload index; -1 = position unknown (row-level or
\ post-LOGHID failure), so the packet omits payload_pos.
variable DPOS    -1 DPOS !
variable VSIG   variable SGSEEN   variable SGIN   variable SGOUT
variable SGRIN  variable SGROUT  variable SGDBASE  variable SGRBASE
variable SGA  variable SGU
$1000 constant TOKBUF-INIT-CAP
$10000 constant TOKBUF-GRAIN
$7FFFFFFFFFFFFFFF constant TOKBUF-MAX-CAP
3 constant TOKBUF-PROT-RW
$1002 constant TOKBUF-MAP-ANON
-1 constant TOKBUF-ANON-FD
0 constant TOKBUF-OFF-ZERO
create FAILTK-BOOT TOKBUF-INIT-CAP allot
create TKF-BOOT TOKBUF-INIT-CAP allot
create NMB-BOOT TOKBUF-INIT-CAP allot
variable FAILTK-P   variable TKF-P   variable NMB-P   variable TOKBUF-CAP-U
variable FAILTU
FAILTK-BOOT FAILTK-P !   TKF-BOOT TKF-P !   NMB-BOOT NMB-P !
TOKBUF-INIT-CAP TOKBUF-CAP-U !
\ FAILTK-FIELD/TKF-FIELD/NMB-FIELD ( -- ptr ptr u8 )
: FAILTK-FIELD FAILTK-P 0 ptr-field ;
: TKF-FIELD TKF-P 0 ptr-field ;
: NMB-FIELD NMB-P 0 ptr-field ;
\ FAILTK/TKF/NMB ( -- ptr u8 )
: FAILTK FAILTK-FIELD @ ;
: TKF TKF-FIELD @ ;
: NMB NMB-FIELD @ ;
\ FAILTK!/TKF!/NMB! ( ptr u8 -- )
: FAILTK! FAILTK-FIELD ! ;
: TKF! TKF-FIELD ! ;
: NMB! NMB-FIELD ! ;
: TOKBUF-ROUND-CAP {: need :}
   need 0 <= IF s" checker: bad token buffer cap" 76 die THEN
   need TOKBUF-MAX-CAP TOKBUF-GRAIN - > IF s" checker: token buffer too large" 76 die THEN
   need 1 - TOKBUF-GRAIN / 1 + TOKBUF-GRAIN * ;
: TOKBUF-MMAP-RC ( n -- n )
   0 swap TOKBUF-PROT-RW TOKBUF-MAP-ANON TOKBUF-ANON-FD TOKBUF-OFF-ZERO mmap
   dup 0 < IF s" checker: token buffer mmap failed" 76 die THEN ;

TRUSTED: TOKBUF-RC>PTR ( n -- ptr u8 ) ;

: TOKBUF-ALLOC ( n -- ptr u8 )
   TOKBUF-MMAP-RC TOKBUF-RC>PTR ;

: TOKBUF-GROW {: need :}
   need TOKBUF-ROUND-CAP {: cap :}
   cap TOKBUF-ALLOC FAILTK!
   cap TOKBUF-ALLOC TKF!
   cap TOKBUF-ALLOC NMB!
   cap TOKBUF-CAP-U ! ;
: TOKBUF-ENSURE {: need :}
   need TOKBUF-CAP-U @ <= IF exit THEN
   need TOKBUF-GROW ;

: TOKBUF-RESET ( -- )
   FAILTK-BOOT FAILTK!
   TKF-BOOT TKF!
   NMB-BOOT NMB!
   TOKBUF-INIT-CAP TOKBUF-CAP-U !
   0 FAILTU ! ;
variable TOKIX  variable FAILIX  variable DVERD
variable FAILB  variable FAILE
variable TBASE  variable TBLEN  variable TI  variable TSTART
variable JSON-DIAGS   0 JSON-DIAGS !

\ TBASE holds the checked source base pointer (ptr u8); read it through a
\ cell-indexed ptr-field view so byte access keeps its ptr u8 role.
: TBASE-FIELD ( -- ptr ptr u8 )
   TBASE 0 ptr-field ;

: TBASE@ ( -- ptr u8 )
   TBASE-FIELD @ ;

: TBASE! ( ptr u8 -- )
   TBASE-FIELD ! ;

: TADDR ( n -- ptr u8 )
   TBASE@ swap + ;

: TBYTE@ ( n -- n )
   TADDR c@ ;

: DIAG-JSON! ( bool -- )
   JSON-DIAGS ! ;

variable LINC
variable LINP
variable LINBEF
variable LINEXP

: LIN-CON? ( n -- bool )
   T-RES dup TAG T-CON <> IF drop RES-FALSE EXIT THEN
   PAY CT-LINEAR? ;

\ Deferred taint scan (part of the linear kind discipline; storage/gate declared
\ before NEW). Reject if any var tainted by a polymorphic copy/drop (LIN-TAINT)
\ now resolves to a linear con — the linear was laundered (`[: dup FREE ;]`).
variable LTNT-I
: LIN-TAINT-SCAN ( -- )
   LIN-ANY? 0= IF exit THEN
   OK @ 0= IF exit THEN
   0 LTNT-I !
   BEGIN LTNT-I @ LTNT-N @ < WHILE
      LTNT-I @ cells LTNT + @ MK-VAR LIN-CON? IF 0 OK ! THEN
      LTNT-I @ 1 + LTNT-I !
   REPEAT ;

\ FIELD-INNER (and every PARAM>ARG accessor) requires a RESOLVED param term:
\ it indexes the param arena by the term's payload. `t` here is a stack type
\ that is usually a bound var whose payload is a VAR id, not a param index —
\ so the inner descent must go through `t T-RES`, matching the FIELD-PARAM?
\ guard just before it. Descending on the raw var reads an unrelated arena slot
\ and, under accumulated arena state, can point back at `t` (infinite recursion).
variable LTC-P
: LIN-TYPE-COUNT* ( n -- n ) {: t:n :}
   t T-RES TAG case
      T-CON of t LIN-CON? IF 1 ELSE 0 THEN endof
      T-PTR of 0 endof
      T-QUOT of 0 endof
      T-ATOM of 0 endof
      T-PARAM of
         t FIELD-PARAM? IF t T-RES FIELD-INNER TWALK-DEEPER RECURSE TWALK-SHALLOWER ELSE
            t T-RES LTC-P !
            LTC-P @ HIDDEN-PARAM? IF
               LTC-P @ HIDDEN-SLOT@  LTC-P @ T-WIDTH 1 -  = IF
                  LTC-P @ LAYOUT-LINEAR-COUNT               \ one bundle: count at the tag only
               ELSE 0 THEN
            ELSE LTC-P @ LAYOUT-PARAM? IF
               LTC-P @ LAYOUT-LINEAR-COUNT                  \ one conservative logical cell
            ELSE 0 THEN THEN
         THEN
      endof
      0 swap
   endcase ;
: LIN-TYPE-COUNT ( n -- n ) TWALK-RESET LIN-TYPE-COUNT* ;

: LIN-ROW-COUNT ( n -- n ) {: row:n :}
   0 LINC !
   row LINP !
   BEGIN LINP @ R-RES TAG S-PUSH = WHILE
      LINP @ R-RES P>TYPE LIN-TYPE-COUNT LINC @ + LINC !
      LINP @ R-RES P>REST LINP !
   REPEAT
   LINC @ ;

: LIN-TOTAL ( n n -- n )
   LIN-ROW-COUNT swap LIN-ROW-COUNT + ;

: LIN-SNAPSHOT ( -- )
   DCUR @ RCUR @ LIN-TOTAL LINBEF ! ;

: LIN-EXPLICIT? ( n n -- bool )
   LIN-TOTAL 0 <> ;

: LIN-CHECK ( -- )
   DCUR @ RCUR @ LIN-TOTAL LINBEF @ <> IF 0 OK ! THEN ;

: UF>DIAG ( -- )
   UF-SET @ IF
      UF-ACT @ DF-ACT !  UF-EXP @ DF-EXP !  UF-POSN @ DPOS !
   ELSE
      0 DF-ACT !  0 DF-EXP !  -1 DPOS !
   THEN
   CVLIVE @ DVAR ! ;   \ the variant live at the first-failure capture (-1 = none)

\ CONSTRUCT-DECL-TERM ( fam -- term bool ) : the constructed value's type args are
\ named only by the DECLARED output (a payloadless variant like `none`, or a
\ variant that binds only some of the family params, has no other source). Scan
\ SGOUT for a resolved hidden-field bundle of `fam` and return its term (its args
\ are the family's instantiation). This is bidirectional: the construct step
\ copies these args over its fresh param vars (TFC-ARGS!), so a concrete layout
\ arg — e.g. option<tdpbw2> — makes the payload/output PUSH-LOGICAL-expand to the
\ arg-aware width. An OPEN declared output (option<a>) stays one logical cell, is
\ never a hidden field, and is skipped, so cell/open constructions keep their
\ fresh-var behaviour and the boundary coercion unchanged. Nothing found = fall
\ back to fresh vars (multi-cell then fails closed, exactly as before this slice).
variable CDT-ROW
: CONSTRUCT-DECL-TERM ( n -- n bool ) {: fam:n :}
   SGOUT @ CDT-ROW !
   BEGIN CDT-ROW @ R-RES TAG S-PUSH = WHILE
      CDT-ROW @ R-RES P>TYPE T-RES {: t:n :}
      t HIDDEN-PARAM? IF t PARAM>FAM fam = IF t RES-TRUE EXIT THEN THEN
      CDT-ROW @ R-RES P>REST CDT-ROW !
   REPEAT
   0 RES-FALSE ;

\ CONSTRUCT-DECL-MULTICELL? ( fam -- bool ) : does the declared output bind a
\ param of `fam` to a genuinely MULTI-CELL (T-WIDTH>1) layout arg? This is the
\ gate for the generated-constructor intercept: only such a call needs the
\ arg-aware construct step; every cell/open/scalar-arg construction keeps the
\ ordinary stored-effect word call, so no existing construction changes path.
: CONSTRUCT-DECL-MULTICELL? ( n -- n ) {: fam:n :}
   fam TFAM-ARITY* 0= IF 0 EXIT THEN         \ arity-0 families have no TYPE ARGS: a wide arity-0 sum (its width from concrete payload fields) is the existing machinery, never this slice's parametric multi-cell-arg capability
   fam CONSTRUCT-DECL-TERM 0= IF drop 0 EXIT THEN
   {: t:n :}                                 \ a resolved hidden field of fam; its args are the instantiation
   0 BEGIN dup t PARAM>ARGC < WHILE
      t over PARAM>ARG T-WIDTH 1 > IF drop t EXIT THEN
      1 +
   REPEAT drop 0 ;

: CHECKER-STEP {: din dout :}
   din dout LIN-EXPLICIT? LINEXP !
   LINEXP @ 0= IF LIN-SNAPSHOT THEN
   DCUR @ WAS !
   DCUR @ din UNIFY-IN
   dup 0=  FAILSET @ 0=  and  OK @ and  IF din DEXP !  WAS @ DACT !  UF>DIAG  -1 FAILSET ! THEN
   OK @ and OK !
   dout DCUR !
   OK @ LINEXP @ 0= and IF LIN-CHECK THEN ;

\ --- return row: >r r> r@ transfer types between DCUR and RCUR. A definition
\ must leave the return row exactly as it found it (ANS 3.2.3.3) — the final
\ balance check rejects net growth or borrowing; loop joins unify RCUR too.
: RS->R                                    \ >r : data top -> return row
   LIN-SNAPSHOT
   FRESH MK-VAR FRESH MK-ROW {: tv rest :}
   DCUR @  tv rest MK-PUSH  UNIFY OK @ and OK !
   rest DCUR !  tv RCUR @ MK-PUSH RCUR !
   OK @ IF LIN-CHECK THEN ;

: RSR>                                     \ r> : return top -> data row
   LIN-SNAPSHOT
   FRESH MK-VAR FRESH MK-ROW {: tv rest :}
   RCUR @  tv rest MK-PUSH  UNIFY OK @ and OK !
   rest RCUR !  tv DCUR @ MK-PUSH DCUR !
   OK @ IF LIN-CHECK THEN ;

: RSR@                                     \ r@ : peek return top
   LIN-SNAPSHOT
   FRESH MK-VAR FRESH MK-ROW {: tv rest :}
   RCUR @  tv rest MK-PUSH  UNIFY OK @ and OK !
   tv DCUR @ MK-PUSH DCUR !
   OK @ IF LIN-CHECK THEN ;

: RS2->R                                   \ 2>r : data pair -> return row
   LIN-SNAPSHOT
   FRESH MK-VAR FRESH MK-VAR FRESH MK-ROW {: t1 t2 rest :}
   DCUR @  t2 t1 rest MK-PUSH MK-PUSH  UNIFY OK @ and OK !
   rest DCUR !
   t1 RCUR @ MK-PUSH  t2 swap MK-PUSH  RCUR !
   OK @ IF LIN-CHECK THEN ;

: RS2R>                                    \ 2r> : return pair -> data row
   LIN-SNAPSHOT
   FRESH MK-VAR FRESH MK-VAR FRESH MK-ROW {: t1 t2 rest :}
   RCUR @  t2 t1 rest MK-PUSH MK-PUSH  UNIFY OK @ and OK !
   rest RCUR !
   t1 DCUR @ MK-PUSH  t2 swap MK-PUSH  DCUR !
   OK @ IF LIN-CHECK THEN ;

: RS2R@                                    \ 2r@ : peek return pair
   LIN-SNAPSHOT
   FRESH MK-VAR FRESH MK-VAR FRESH MK-ROW {: t1 t2 rest :}
   RCUR @  t2 t1 rest MK-PUSH MK-PUSH  UNIFY OK @ and OK !
   t1 DCUR @ MK-PUSH  t2 swap MK-PUSH  DCUR !
   OK @ IF LIN-CHECK THEN ;
variable QTT  variable QD2  variable QR2

: THROW-EDGE ( -- )
   THSET @ 0= IF DCUR @ THDROW !  RCUR @ THRROW ! THEN
   -1 THSET ! ;

\ A quotation whose declared rows name a concrete linear con is an explicit
\ linear consumer/producer (checked when it was built): its net count change is
\ intended, so skip conservation — exactly as CHECKER-STEP skips a step whose
\ declared effect names a linear. A polymorphic quotation (linear only bound by
\ unification at apply time) is NOT explicit and must conserve.
: RSEXEC-LIN-EXPLICIT? ( n -- bool ) {: q:n :}
   q Q>DIN q Q>DOUT LIN-TOTAL
   q Q>RIN q Q>ROUT LIN-TOTAL + 0 <> ;

variable RSEXEC-EXP    \ quot explicitly names a linear? (captured before unify binds vars)
variable EXEC-OPAQUE   \ RSEXEC rejected an execute of an opaque (untyped-memory) xt this token; DO-TOK1 latches the reason (the reject machinery lives far below RSEXEC)
variable CATCH-OPAQUE  \ RSCATCH rejected a catch of an opaque (untyped-memory) xt this token; latched the same way so the prose names 'catch' not 'execute'

: RSEXEC   \ execute: pop the xt; apply its quot effect (or bind a var to one)
   FRESH MK-VAR FRESH MK-ROW {: tv rest :}
   DCUR @  tv rest MK-PUSH  UNIFY OK @ and OK !
   rest DCUR !
   LIN-SNAPSHOT                          \ linears on the post-pop stack (pre-apply)
   tv T-RES QTT !
   QTT @ TAG T-QUOT = IF
     \ Capture explicitness BEFORE UNIFY-IN: once the quot's fresh vars unify
     \ with the stack they resolve to linears and would look falsely explicit.
     QTT @ RSEXEC-LIN-EXPLICIT? RSEXEC-EXP !
     DCUR @ QTT @ Q>DIN  UNIFY-IN OK @ and OK !
     RCUR @ QTT @ Q>RIN  UNIFY-IN OK @ and OK !
     QTT @ Q>XHAS IF
        THROW-EDGE
     THEN
     QTT @ Q>XDEAD IF
        -1 DEADP !
     ELSE
        QTT @ Q>DOUT DCUR !  QTT @ Q>ROUT RCUR !
        OK @  RSEXEC-EXP @ 0=  and IF LIN-CHECK THEN
     THEN
   ELSE QTT @ TAG T-VAR = IF
     \ Opaque xt: the executed value is a bare type variable, i.e. an xt of
     \ unknown provenance — an xt fetched from untyped memory (a raw variable or
     \ create cell). Its true stack effect is not statically known, so executing
     \ it in checked code is unsound (it launders whatever the stored xt actually
     \ does past the checker) and is rejected here (dot
     \ habu-checker-exec-of-5923c543). DO-TOK1 turns the flag into the named
     \ E-EXEC-OPAQUE-XT diagnostic on the pinned `execute` token. The typed routes
     \ that stay checked are a quotation parameter, a `defer` bound with `is`, and
     \ a typed `xt<effect>` storage cell (all recover a T-QUOT here).
     0 OK !  -1 EXEC-OPAQUE !
   ELSE 0 OK ! THEN THEN ;

variable RSRET

: RSCATCH   \ catch: stack-preserving quotation -> same stack plus throw code
   \ Catchable `throw` is not process no-return. The checker tracks throw paths
   \ as an exceptional edge owned by `catch`; `die` remains separate no-return
   \ metadata because it cannot be recovered by a quotation catch.
   -1 RSRET !
   FRESH MK-VAR FRESH MK-ROW {: tv rest :}
   DCUR @  tv rest MK-PUSH  UNIFY OK @ and OK !
   rest DCUR !
   tv T-RES QTT !
   QTT @ TAG T-QUOT = IF
     DCUR @ QTT @ Q>DIN   UNIFY-IN OK @ and OK !
     RCUR @ QTT @ Q>RIN   UNIFY-IN OK @ and OK !
     QTT @ Q>XDEAD IF
        QTT @ Q>XHAS 0= IF 0 RSRET !  -1 DEADP ! THEN
     ELSE
        DCUR @ QTT @ Q>DOUT  UNIFY-IN OK @ and OK !
        RCUR @ QTT @ Q>ROUT  UNIFY-IN OK @ and OK !
     THEN
   ELSE QTT @ TAG T-VAR = IF
     \ Opaque xt: the caught value is a bare type variable, i.e. an xt of unknown
     \ provenance fetched from untyped memory (a raw variable or create cell). Its
     \ true stack effect is not statically known, so catching it in checked code
     \ launders whatever the stored xt does past the checker just as executing it
     \ would — reject it identically (dot habu-flip-rscatch-opaque-5da02bd5).
     \ DO-TOK1 turns the flag into the E-EXEC-OPAQUE-XT diagnostic named on the
     \ pinned `catch` token. The checked routes are a quotation parameter, a
     \ `defer` bound with `is`, and a typed `xt<effect>` cell (all recover a
     \ T-QUOT here).
     0 OK !  -1 CATCH-OPAQUE !
   ELSE 0 OK ! THEN THEN
   RSRET @ IF 1 MK-CON DCUR @ MK-PUSH DCUR ! THEN ;

variable RSH

: RS-TOK? {: a u :}
   -1 RSH !
   a u s" >r" CORE-STR= IF RS->R ELSE
   a u s" r>" CORE-STR= IF RSR> ELSE
   a u s" r@" CORE-STR= IF RSR@ ELSE
   a u s" 2>r" CORE-STR= IF RS2->R ELSE
   a u s" 2r>" CORE-STR= IF RS2R> ELSE
   a u s" 2r@" CORE-STR= IF RS2R@ ELSE
   a u s" execute" CORE-STR= IF RSEXEC ELSE
   a u s" catch" CORE-STR= IF RSCATCH ELSE
   0 RSH ! THEN THEN THEN THEN THEN THEN THEN THEN
   RSH @ ;

0 constant VR-CON
1 constant VR-VAR
2 constant VR-ROW
3 constant VR-PTR
4 constant VR-PUSH
5 constant VR-QUOT
6 constant VR-ATOM
7 constant VR-PARAM

64 constant VREC-CAP-INIT       \ value-record table records (grows on demand)
512 constant VREC-FIELD-INIT     \ field-node index pool (grows on demand)
$4000 constant VREC-NODE-INIT    \ instantiation nodes (grows on demand)
$10000 constant VREC-STR-INIT    \ value-record string pool (grows on demand)
variable VREC-CAP-V   VREC-CAP-INIT VREC-CAP-V !
: VREC-CAP ( -- n ) VREC-CAP-V @ ;
variable VREC-FIELD-CAP-V   VREC-FIELD-INIT VREC-FIELD-CAP-V !
: VREC-FIELD-CAP ( -- n ) VREC-FIELD-CAP-V @ ;
variable VREC-NODE-CAP-V   VREC-NODE-INIT VREC-NODE-CAP-V !
: VREC-NODE-CAP ( -- n ) VREC-NODE-CAP-V @ ;
variable VREC-STR-CAP-V   VREC-STR-INIT VREC-STR-CAP-V !
: VREC-STR-CAP ( -- n ) VREC-STR-CAP-V @ ;

\ Boot buffers + P pointers; VREC-NAME-A holds pointers into VREC-STR and is
\ rebased on relocation. VR-ATOM/VR-PARAM node VN.A cells store string offsets.
create VREC-NAME-A-BOOT VREC-CAP-INIT cells allot
create VREC-NAME-U-BOOT VREC-CAP-INIT cells allot
create VREC-START-BOOT VREC-CAP-INIT cells allot
create VREC-COUNT-BOOT VREC-CAP-INIT cells allot
create VREC-TVN-BOOT VREC-CAP-INIT cells allot
create VREC-RVN-BOOT VREC-CAP-INIT cells allot
create VREC-FIELDS-BOOT VREC-FIELD-INIT cells allot
create VRN-TAG-BOOT VREC-NODE-INIT cells allot
create VRN-A-BOOT VREC-NODE-INIT cells allot
create VRN-B-BOOT VREC-NODE-INIT cells allot
create VRN-C-BOOT VREC-NODE-INIT cells allot
create VRN-D-BOOT VREC-NODE-INIT cells allot
create VRN-E-BOOT VREC-NODE-INIT cells allot
create VRN-F-BOOT VREC-NODE-INIT cells allot
create VRN-G-BOOT VREC-NODE-INIT cells allot
create VRN-H-BOOT VREC-NODE-INIT cells allot
create VREC-STR-BOOT VREC-STR-INIT allot
variable VREC-NAME-A-P   variable VREC-NAME-U-P   variable VREC-START-P
variable VREC-COUNT-P    variable VREC-TVN-P      variable VREC-RVN-P
variable VREC-FIELDS-P
variable VRN-TAG-P   variable VRN-A-P   variable VRN-B-P   variable VRN-C-P
variable VRN-D-P     variable VRN-E-P   variable VRN-F-P   variable VRN-G-P
variable VRN-H-P     variable VREC-STR-P

: VREC-ARENA-BOOT ( -- )        \ point every VREC store at its boot buffer
   VREC-NAME-A-BOOT VREC-NAME-A-P !   VREC-NAME-U-BOOT VREC-NAME-U-P !
   VREC-START-BOOT VREC-START-P !     VREC-COUNT-BOOT VREC-COUNT-P !
   VREC-TVN-BOOT VREC-TVN-P !         VREC-RVN-BOOT VREC-RVN-P !
   VREC-FIELDS-BOOT VREC-FIELDS-P !
   VRN-TAG-BOOT VRN-TAG-P !   VRN-A-BOOT VRN-A-P !   VRN-B-BOOT VRN-B-P !
   VRN-C-BOOT VRN-C-P !       VRN-D-BOOT VRN-D-P !   VRN-E-BOOT VRN-E-P !
   VRN-F-BOOT VRN-F-P !       VRN-G-BOOT VRN-G-P !   VRN-H-BOOT VRN-H-P !
   VREC-STR-BOOT VREC-STR-P ! ;
VREC-ARENA-BOOT
: VREC-NAME-A ( -- ptr a ) VREC-NAME-A-P @ ;
: VREC-NAME-U ( -- ptr a ) VREC-NAME-U-P @ ;
: VREC-START ( -- ptr a ) VREC-START-P @ ;
: VREC-COUNT ( -- ptr a ) VREC-COUNT-P @ ;
: VREC-TVN ( -- ptr a ) VREC-TVN-P @ ;
: VREC-RVN ( -- ptr a ) VREC-RVN-P @ ;
: VREC-FIELDS ( -- ptr a ) VREC-FIELDS-P @ ;
: VRN-TAG ( -- ptr a ) VRN-TAG-P @ ;
: VRN-A ( -- ptr a ) VRN-A-P @ ;   : VRN-B ( -- ptr a ) VRN-B-P @ ;
: VRN-C ( -- ptr a ) VRN-C-P @ ;   : VRN-D ( -- ptr a ) VRN-D-P @ ;
: VRN-E ( -- ptr a ) VRN-E-P @ ;   : VRN-F ( -- ptr a ) VRN-F-P @ ;
: VRN-G ( -- ptr a ) VRN-G-P @ ;   : VRN-H ( -- ptr a ) VRN-H-P @ ;
: VREC-STR ( -- ptr u8 ) VREC-STR-P @ ;
\ VRC-TV/VRC-RV/VRI-TV/VRI-RV are var-id maps in the growable TV arena (top).
64 constant VRI-AK-INIT
variable VRI-AK-CAP-V   VRI-AK-INIT VRI-AK-CAP-V !
: VRI-AK-CAP ( -- n ) VRI-AK-CAP-V @ ;
create VRI-AK-BOOT VRI-AK-INIT cells allot
variable VRI-AK-P   VRI-AK-BOOT VRI-AK-P !
: VRI-AK ( -- ptr a ) VRI-AK-P @ ;

\ VNARG: flat per-node arg pool for persisted VR-PARAM nodes (uncapped arity).
\ A VR-PARAM node stores argc in VN.C, the arg-run start (into VNARG) in VN.D, and
\ the family-id in VN.H; the argc child node ids live at [start,start+argc). Cells
\ hold node ids (pointer-free), so REG-GROW1 relocation and snapshot bake verbatim.
\ VNARG-N rewinds through the rollback frame (RBF.VNARGN) in lockstep with
\ VREC-NODE-N: a VR-PARAM node and its arg run are allocated together inside one
\ VREC-COPY (never across a frame boundary), so at every RBF-PUSH/POP point all
\ nodes below the VREC-NODE-N mark have runs entirely below the VNARG-N mark —
\ rewinding both retires a rejected scope's runs without dangling a survivor.
$4000 constant VNARG-INIT
create VNARG-BOOT VNARG-INIT cells allot
variable VNARG-P   VNARG-BOOT VNARG-P !
variable VNARG-CAP-V   VNARG-INIT VNARG-CAP-V !
variable VNARG-N   0 VNARG-N !
: VNARG ( -- ptr a ) VNARG-P @ ;
: VNARG-GROW ( n -- ) {: need:n :}
   need VNARG-CAP-V @ 2 * max {: nc:n :}
   VNARG-P VNARG-CAP-V @ cells nc cells REG-GROW1
   nc VNARG-CAP-V ! ;
: VNARG-ENSURE ( n -- ) {: need:n :}      \ room for `need` more cells past VNARG-N
   VNARG-N @ need + VNARG-CAP-V @ <= IF exit THEN
   VNARG-N @ need + VNARG-GROW ;

variable VREC-N
variable VREC-FIELD-N
variable VREC-NODE-N
variable VREC-STR-U
variable VREC-I
variable VREC-J
variable VRC-TVN
variable VRC-RVN

0 VREC-N !
0 VREC-FIELD-N !
1 VREC-NODE-N !
0 VREC-STR-U !

\ --- geometric grow of the VREC stores. Record/field/node arrays hold ids or
\ pointers into VREC-STR (unmoved by these grows), so a plain cell copy suffices;
\ VRI-AK is a sparse UNBOUND-keyed scratch table, so its grown tail is unbound.
: VREC-GROW ( n -- ) {: need:n :}
   need VREC-CAP-V @ 2 * max {: nc:n :}
   VREC-CAP-V @ cells {: ob:n :}   nc cells {: nb:n :}
   VREC-NAME-A-P ob nb REG-GROW1   VREC-NAME-U-P ob nb REG-GROW1
   VREC-START-P ob nb REG-GROW1    VREC-COUNT-P ob nb REG-GROW1
   VREC-TVN-P ob nb REG-GROW1      VREC-RVN-P ob nb REG-GROW1
   nc VREC-CAP-V ! ;
: VREC-ENSURE ( -- )            \ ensure room for the next record id (VREC-N)
   VREC-N @ VREC-CAP-V @ < IF exit THEN
   VREC-N @ 1 + VREC-GROW ;
: VREC-FIELD-GROW ( n -- ) {: need:n :}
   need VREC-FIELD-CAP-V @ 2 * max {: nc:n :}
   VREC-FIELDS-P @ VREC-FIELD-CAP-V @ cells nc cells ARENA-BYTES-GROW VREC-FIELDS-P !
   nc VREC-FIELD-CAP-V ! ;
: VREC-FIELD-ENSURE ( -- )
   VREC-FIELD-N @ VREC-FIELD-CAP-V @ < IF exit THEN
   VREC-FIELD-N @ 1 + VREC-FIELD-GROW ;
: VREC-NODE-GROW ( n -- ) {: need:n :}
   need VREC-NODE-CAP-V @ 2 * max {: nc:n :}
   VREC-NODE-CAP-V @ cells {: ob:n :}   nc cells {: nb:n :}
   VRN-TAG-P ob nb REG-GROW1
   VRN-A-P ob nb REG-GROW1   VRN-B-P ob nb REG-GROW1   VRN-C-P ob nb REG-GROW1
   VRN-D-P ob nb REG-GROW1   VRN-E-P ob nb REG-GROW1   VRN-F-P ob nb REG-GROW1
   VRN-G-P ob nb REG-GROW1   VRN-H-P ob nb REG-GROW1
   nc VREC-NODE-CAP-V ! ;
: VREC-NODE-ENSURE ( -- )
   VREC-NODE-N @ VREC-NODE-CAP-V @ < IF exit THEN
   VREC-NODE-N @ 1 + VREC-NODE-GROW ;
: VRI-AK-GROW ( n -- ) {: need:n :}
   need VRI-AK-CAP-V @ 2 * max {: nc:n :}
   VRI-AK-CAP-V @ {: oc:n :}
   VRI-AK-P @ oc cells nc cells ARENA-BYTES-GROW {: nb:ptr :}
   nb oc nc ARENA-CELLS-UNBOUND
   nb VRI-AK-P !
   nc VRI-AK-CAP-V ! ;
: VRI-AK-ENSURE ( n -- ) {: need:n :}   \ ensure index `need` is valid
   need VRI-AK-CAP-V @ < IF exit THEN
   need 1 + VRI-AK-GROW ;

: VREC-CHECK ( n -- ) {: id:n :}
   id 0 < IF s" checker: bad value-record id" 76 die THEN
   id VREC-N @ >= IF s" checker: bad value-record id" 76 die THEN ;

: VREC-NAME-A-FIELD ( n -- ptr ptr u8 )
   dup VREC-CHECK
   cells VREC-NAME-A + 0 ptr-field ;

: VREC-NAME$ ( n -- ptr u8 n ) {: id:n :}
   id VREC-NAME-A-FIELD @
   id cells VREC-NAME-U + @ ;

: VREC-START@ ( n -- n )
   dup VREC-CHECK
   cells VREC-START + @ ;

: VREC-COUNT@ ( n -- n )
   dup VREC-CHECK
   cells VREC-COUNT + @ ;

: VREC-TVN@ ( n -- n )
   dup VREC-CHECK
   cells VREC-TVN + @ ;

: VREC-RVN@ ( n -- n )
   dup VREC-CHECK
   cells VREC-RVN + @ ;

: VREC-MATCH? ( ptr u8 n n -- bool ) {: a:ptr u:n id:n :}
   id VREC-NAME$ a u CORE-STR= ;

: VREC-FIND ( ptr u8 n -- n bool ) {: a:ptr u:n :}
   0 VREC-I !
   BEGIN VREC-I @ VREC-N @ < WHILE
      a u VREC-I @ VREC-MATCH? IF VREC-I @ RES-TRUE EXIT THEN
      VREC-I @ 1 + VREC-I !
   REPEAT
   0 RES-FALSE ;

: VREC-NODE-CHECK ( n -- ) {: id:n :}
   id 0 <= IF s" checker: bad value-record node" 76 die THEN
   id VREC-NODE-N @ >= IF s" checker: bad value-record node" 76 die THEN ;

: VREC-NODE-SLOT ( n ptr a -- ptr a ) {: id:n base:ptr :}
   id VREC-NODE-CHECK
   base id cells + ;

: VN.TAG@ ( n -- n ) VRN-TAG VREC-NODE-SLOT @ ;
: VN.A@ ( n -- n ) VRN-A VREC-NODE-SLOT @ ;
: VN.B@ ( n -- n ) VRN-B VREC-NODE-SLOT @ ;
: VN.C@ ( n -- n ) VRN-C VREC-NODE-SLOT @ ;
: VN.D@ ( n -- n ) VRN-D VREC-NODE-SLOT @ ;
: VN.E@ ( n -- n ) VRN-E VREC-NODE-SLOT @ ;
: VN.F@ ( n -- n ) VRN-F VREC-NODE-SLOT @ ;
: VN.G@ ( n -- n ) VRN-G VREC-NODE-SLOT @ ;
: VN.H@ ( n -- n ) VRN-H VREC-NODE-SLOT @ ;
: VN.TAG! ( n n -- ) VRN-TAG VREC-NODE-SLOT ! ;
: VN.A! ( n n -- ) VRN-A VREC-NODE-SLOT ! ;
: VN.B! ( n n -- ) VRN-B VREC-NODE-SLOT ! ;
: VN.C! ( n n -- ) VRN-C VREC-NODE-SLOT ! ;
: VN.D! ( n n -- ) VRN-D VREC-NODE-SLOT ! ;
: VN.E! ( n n -- ) VRN-E VREC-NODE-SLOT ! ;
: VN.F! ( n n -- ) VRN-F VREC-NODE-SLOT ! ;
: VN.G! ( n n -- ) VRN-G VREC-NODE-SLOT ! ;
: VN.H! ( n n -- ) VRN-H VREC-NODE-SLOT ! ;
\ VN>ARG ( node i -- childnode ) : the i-th arg node of a persisted VR-PARAM node,
\ read from the flat VNARG pool at [VN.D@, VN.D@+argc). (VN.C@ holds argc.)
: VN>ARG ( n n -- n ) {: node:n i:n :}
   node VN.D@ i + cells VNARG + @ ;

variable VREC-RB-I
\ VREC-STR-REBASE ( n -- ) : a VREC-STR relocation moved the pool by delta; add
\ it to every stored record-name pointer. Node strings store offsets, not ptrs.
: VREC-STR-REBASE ( n -- ) {: delta:n :}
   0 VREC-RB-I !
   BEGIN VREC-RB-I @ VREC-N @ < WHILE
      VREC-RB-I @ VREC-NAME-A-FIELD {: fld:ptr :}
      fld @ delta + fld !
      VREC-RB-I @ 1 + VREC-RB-I !
   REPEAT ;
: VREC-STR-GROW ( n -- ) {: need:n :}
   need VREC-STR-CAP-V @ 2 * max {: nc:n :}
   VREC-STR-P @ {: old:ptr :}
   old VREC-STR-CAP-V @ nc ARENA-BYTES-GROW {: new:ptr :}
   new VREC-STR-P !   nc VREC-STR-CAP-V !
   new old - VREC-STR-REBASE ;
: VREC-STR-ENSURE ( n -- ) {: add:n :}   \ ensure room for `add` more string bytes
   VREC-STR-U @ add + VREC-STR-CAP-V @ <= IF exit THEN
   VREC-STR-U @ add + VREC-STR-GROW ;
: VREC-STR-COPY ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   u VREC-STR-ENSURE
   VREC-STR VREC-STR-U @ + {: dst:ptr :}
   0 VREC-I !
   BEGIN VREC-I @ u < WHILE
      a VREC-I @ + c@ dst VREC-I @ + c!
      VREC-I @ 1 + VREC-I !
   REPEAT
   VREC-STR-U @ u + VREC-STR-U !
   dst u ;

: VREC-NODE-NEW ( n -- n ) {: tag:n :}
   VREC-NODE-ENSURE
   VREC-NODE-N @ {: id:n :}
   id 1 + VREC-NODE-N !
   tag id VN.TAG!
   0 id VN.A! 0 id VN.B! 0 id VN.C! 0 id VN.D!
   0 id VN.E! 0 id VN.F! 0 id VN.G! 0 id VN.H!
   id ;

: VREC-FIELD@ ( n -- n ) {: idx:n :}
   idx 0 < IF s" checker: bad value-record field" 76 die THEN
   idx VREC-FIELD-N @ >= IF s" checker: bad value-record field" 76 die THEN
   idx cells VREC-FIELDS + @ ;

: VREC-FIELD! ( n -- ) {: node:n :}
   VREC-FIELD-ENSURE
   node VREC-FIELD-N @ cells VREC-FIELDS + !
   VREC-FIELD-N @ 1 + VREC-FIELD-N ! ;

: VREC-MAP-RESET-ONE ( ptr a -- ) {: p:ptr :}
   0 BEGIN dup MAXTV < WHILE
      UNBOUND over cells p + !
      1 +
   REPEAT drop ;

: VREC-COPY-RESET ( -- )
   VRC-TV VREC-MAP-RESET-ONE
   VRC-RV VREC-MAP-RESET-ONE
   0 VRC-TVN !
   0 VRC-RVN ! ;

: VREC-TV-ID ( n -- n ) {: id:n :}
   id cells VRC-TV + dup @ UNBOUND = IF
      VRC-TVN @ over !
      VRC-TVN @ 1 + VRC-TVN !
   THEN @ ;

: VREC-RV-ID ( n -- n ) {: id:n :}
   id cells VRC-RV + dup @ UNBOUND = IF
      VRC-RVN @ over !
      VRC-RVN @ 1 + VRC-RVN !
   THEN @ ;

: VREC-COPY-STR ( ptr u8 n n -- ) {: a:ptr u:n node:n :}
   VREC-STR-U @ {: off:n :}
   a u VREC-STR-COPY 2drop
   off node VN.A!
   u node VN.B! ;

: VREC-RES ( n -- n ) {: x:n :}
   x TAG S-ROW = x TAG S-PUSH = or IF x R-RES ELSE x T-RES THEN ;

: VREC-COPY ( n -- n ) {: x:n :}
   x 0= IF 0 EXIT THEN
   x VREC-RES TAG case
      T-CON of
         VR-CON VREC-NODE-NEW {: node:n :}
         x VREC-RES PAY node VN.A!
         node
      endof
      T-VAR of
         VR-VAR VREC-NODE-NEW {: node:n :}
         x VREC-RES PAY VREC-TV-ID node VN.A!
         node
      endof
      S-ROW of
         VR-ROW VREC-NODE-NEW {: node:n :}
         x VREC-RES PAY VREC-RV-ID node VN.A!
         node
      endof
      T-PTR of
         VR-PTR VREC-NODE-NEW {: node:n :}
         x VREC-RES PTR>INNER RECURSE node VN.A!
         node
      endof
      S-PUSH of
         VR-PUSH VREC-NODE-NEW {: node:n :}
         x VREC-RES P>TYPE RECURSE node VN.A!
         x VREC-RES P>REST RECURSE node VN.B!
         node
      endof
      T-QUOT of
         VR-QUOT VREC-NODE-NEW {: node:n :}
         x VREC-RES Q>DIN RECURSE node VN.A!
         x VREC-RES Q>DOUT RECURSE node VN.B!
         x VREC-RES Q>RIN RECURSE node VN.C!
         x VREC-RES Q>ROUT RECURSE node VN.D!
         x VREC-RES Q>XHAS node VN.E!
         x VREC-RES Q>XDEAD node VN.F!
         x VREC-RES Q>XDOUT node VN.G!
         x VREC-RES Q>XROUT node VN.H!
         node
      endof
      T-ATOM of
         VR-ATOM VREC-NODE-NEW {: node:n :}
         x VREC-RES ATOM>A x VREC-RES ATOM>U node VREC-COPY-STR
         x VREC-RES ATOM>K node VN.C!
         node
      endof
      T-PARAM of
         VR-PARAM VREC-NODE-NEW {: node:n :}
         x VREC-RES PARAM>NAME-A x VREC-RES PARAM>NAME-U node VREC-COPY-STR
         x VREC-RES PARAM>ARGC {: argc:n :}
         argc node VN.C!
         x VREC-RES PARAM>FAM node VN.H!            \ resolved family-id (identity)
         \ reserve an argc-cell run in VNARG, then copy children into it. Children
         \ (nested params) allocate their own runs after this one; the run start
         \ index is stable across REG-GROW1 relocation, so re-fetch VNARG per store.
         VNARG-N @ {: start:n :}
         argc VNARG-ENSURE
         start node VN.D!
         argc VNARG-N @ + VNARG-N !
         0 BEGIN dup argc < WHILE            \ data-stack index (RECURSE-safe)
            x VREC-RES over PARAM>ARG RECURSE   \ ( i childid )
            over start + cells VNARG + !        \ ( i )
            1 +
         REPEAT drop
         node
      endof
      0 swap
   endcase ;

: VRI-AK-RESET ( -- )
   0 BEGIN dup VRI-AK-CAP < WHILE
      UNBOUND over cells VRI-AK + !
      1 +
   REPEAT drop ;

: VREC-INST-RESET ( n -- ) {: id:n :}
   VRI-AK-RESET
   0 BEGIN dup id VREC-TVN@ < WHILE
      UNBOUND over cells VRI-TV + !
      1 +
   REPEAT drop
   0 BEGIN dup id VREC-RVN@ < WHILE
      UNBOUND over cells VRI-RV + !
      1 +
   REPEAT drop ;

\ FRESH may grow (relocate) the VRI arena, so re-fetch the slot address after
\ it: a base cached across FRESH would store into the freed buffer.
: VREC-I-TV ( n -- n ) {: id:n :}
   id cells VRI-TV + @ UNBOUND = IF
      FRESH MK-VAR id cells VRI-TV + !
   THEN id cells VRI-TV + @ ;

: VREC-I-RV ( n -- n ) {: id:n :}
   id cells VRI-RV + @ UNBOUND = IF
      FRESH MK-ROW id cells VRI-RV + !
   THEN id cells VRI-RV + @ ;

: VREC-I-AK-IDX ( n -- n )
   negate 1 - ;

: VREC-I-AK ( ptr u8 n n -- ptr u8 n n ) {: a:ptr u:n k:n :}   \ name kept for domain routing
   k 0 >= IF a u k EXIT THEN
   k VREC-I-AK-IDX {: idx:n :}
   idx VRI-AK-ENSURE
   idx cells VRI-AK + dup @ UNBOUND = IF a u RIGID-AK-MINT over ! THEN @
   {: id:n :}
   a u id ;

: VREC-BYTE+ ( ptr u8 n -- ptr u8 )
   + ;

: VREC-I-STR ( n -- ptr u8 n ) {: node:n :}
   VREC-STR node VN.A@ VREC-BYTE+ node VN.B@ ;

: VREC-INST ( n -- n ) {: node:n :}
   node 0= IF 0 EXIT THEN
   node VN.TAG@ case
      VR-CON of node VN.A@ MK-CON endof
      VR-VAR of node VN.A@ VREC-I-TV endof
      VR-ROW of node VN.A@ VREC-I-RV endof
      VR-PTR of node VN.A@ RECURSE MK-PTR endof
      VR-PUSH of node VN.A@ RECURSE node VN.B@ RECURSE MK-PUSH endof
      VR-QUOT of
         node VN.A@ RECURSE
         node VN.B@ RECURSE
         node VN.C@ RECURSE
         node VN.D@ RECURSE
         MK-QUOT
         dup node VN.E@ node VN.F@ node VN.G@ node VN.H@ QX!
      endof
      VR-ATOM of node VREC-I-STR node VN.C@ VREC-I-AK MK-ATOM-K endof
      VR-PARAM of
         node VN.C@ {: argc:n :}
         node VN.D@ {: start:n :}
         PARAM-SCR-N @                              \ reentrant scratch mark (base) on the data stack
         0 BEGIN dup argc < WHILE                   \ data-stack index (RECURSE-safe)
            dup start + cells VNARG + @ RECURSE PARAM-SCR+
            1 +
         REPEAT drop
         node VREC-I-STR node VN.H@ MK-PARAM        \ ( base a u fam -- t )
      endof
      0 swap
   endcase ;

: VREC-PUSH-FIELDS ( n n -- n ) {: row:n id:n :}
   id VREC-INST-RESET
   row
   0 VREC-I !
   BEGIN VREC-I @ id VREC-COUNT@ < WHILE
      id VREC-START@ VREC-I @ + VREC-FIELD@ VREC-INST
      swap MK-PUSH
      VREC-I @ 1 + VREC-I !
   REPEAT ;

\ --- generic signature parser: build a step effect from a textual " in -- out "
\ stack effect. A single lowercase letter is a polymorphic type variable (shared
\ across in/out within one signature); `n` = int (con 1), `f` = flag (con 2).
\ Unknown multi-char tokens mark the signature malformed; row variables are
\ shared so the effect is row-polymorphic.
\ Signature quantifiers retain their source-letter identity for generic effect
\ parsing and diagnostics.  This is not the declaration parameter alphabet.
create NMAP 26 cells allot

: NMAP-RESET 0 BEGIN dup cells NMAP + UNBOUND swap ! 1 + dup 25 > UNTIL drop ;

64 constant FAM-CAP
create FAM-A FAM-CAP cells allot
create FAM-U FAM-CAP cells allot
variable FAM-N
variable FAM-I
variable FAM-K

: FAM-RESET ( -- )
   0 FAM-N ! ;

: FAM-A-FIELD ( n -- ptr ptr u8 )
   cells FAM-A + 0 ptr-field ;

: FAM-A@ ( n -- ptr u8 )
   FAM-A-FIELD @ ;

: FAM-IDX>KEY ( n -- n )
   1+ negate ;

: FAM-MATCH? ( ptr u8 n n -- bool ) {: a:ptr u:n idx:n :}
   idx FAM-A@ idx cells FAM-U + @ a u CORE-STR= ;

: FAM-FIND ( ptr u8 n -- n bool ) {: a:ptr u:n :}
   0 FAM-I !
   BEGIN FAM-I @ FAM-N @ < WHILE
      a u FAM-I @ FAM-MATCH? IF FAM-I @ RES-TRUE EXIT THEN
      FAM-I @ 1 + FAM-I !
   REPEAT
   0 RES-FALSE ;

: FAM-ADD ( ptr u8 n -- n ) {: a:ptr u:n :}
   FAM-N @ FAM-CAP >= IF s" checker: fresh atom table full" 76 die THEN
   a FAM-N @ FAM-A-FIELD !
   u FAM-N @ cells FAM-U + !
   FAM-N @ FAM-IDX>KEY
   FAM-N @ 1 + FAM-N ! ;

: FAM-MARK ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u FAM-FIND IF FAM-IDX>KEY EXIT THEN drop
   a u FAM-ADD ;

: DIGIT? {: c :} c 47 > c 58 < and ;

: LOWER? {: c :} c 96 > c 123 < and ;
variable NRES  variable NDI  variable NDH
0 constant SGBAD-SYNTAX-KIND
1 constant SGBAD-UNKNOWN-KIND
2 constant SGBAD-BAREPTR-KIND
3 constant SGBAD-ARITY-KIND
variable SGBAD
variable SGBAD-A
variable SGBAD-U
variable SGBAD-KIND
variable SGBAD-AR-DECL   \ arity kind: the family's declared arity
variable SGBAD-AR-GOT    \ arity kind: the argument count actually written
variable UNSAFE
variable RETIRED             \ a permanently retired token was named in a checked body
variable LOCALBAD
variable LINLOCBAD           \ a linear-counting value was bound into a {: :} local
variable UNDEFERR
variable QUALBAD
variable QDUPBAD             \ ?dup applied to a layout value (width-breaking; item 12)
variable CAPREQ              \ a TRUSTED-only capability prim (patch32/code-gen sink) called from checked code
\ --- declared-effect parametricity seal (habu-nominal-storage-effect). A rejected
\ definition whose body specialized a declared quantifier to a sealed nominal/layout
\ family, or unified two declared quantifiers together. Set by NP-CHECK post-body.
variable NPBAD               \ non-parametric declared effect detected?
variable NPBAD-KIND          \ 0 = quantifier specialized to a sealed family; 1 = aliasing
variable NPBAD-Q1            \ offending quantifier's source letter (char code)
variable NPBAD-Q2            \ aliasing partner's letter (char code); 0 when kind 0
variable NPBAD-TERM          \ resolved sealed-family term (kind 0), for family naming

: HEXD? {: c :} c DIGIT?  c 96 > c 103 < and or  c 64 > c 71 < and or ;

\ int literal: d+ | -d+ | $h+ | -$h+ (the engine's number tokens)
: ALLDIG? {: a u :}
   0 NDI !  0 NDH !
   u 0 > IF a c@ 45 = IF 1 NDI ! THEN THEN
   u NDI @ > IF a NDI @ + c@ 36 = IF NDI @ 1 + NDI !  1 NDH ! THEN THEN
   u NDI @ - 0 > 0= IF 0 NRES ! ELSE -1 NRES !
     NDI @ BEGIN dup u < WHILE
       NDH @ IF dup a + c@ HEXD? 0= IF 0 NRES ! THEN
       ELSE dup a + c@ DIGIT? 0= IF 0 NRES ! THEN THEN
       1 + REPEAT drop THEN
   NRES @ ;

\ SIG-RAW-MODE: while set, every type var minted by the signature parser is
\ TVK-RAW. verify-source (and the two-stage native definer hook) bracket the
\ effect string of a raw storage definer -- create/variable/constant/PTR-VARIABLE
\ -- with SIG-RAW-DEFINER! so the created word's stored effect carries RAW vars,
\ closing the VALUE-side mint that raw dictionary storage would otherwise publish
\ as an unrestricted polymorphic effect. Off for ordinary sigs, prim builds, and
\ user TRUST rows, so nothing else is affected.
variable SIG-RAW-MODE   0 SIG-RAW-MODE !
: SIG-RAW-DEFINER! ( bool -- ) SIG-RAW-MODE ! ;

\ NB: avoid a 2nd {: :} group here — `{: c :} … {: i :}` mis-reads the slot in the
\ standalone, collapsing every var to one. Compute the slot address on the stack.
: VAR-OF ( n -- n ) {: c:n :}
   c 97 - cells NMAP +
   dup @ UNBOUND = IF FRESH over ! THEN
   @ SIG-RAW-MODE @ IF dup TVK-RAW! THEN MK-VAR ;

\ NB: declare locals at word top, never inside IF/loop (corrupts the locals frame).
\ concrete width types get distinct con codes; n(1)/f(1) stay the GENERIC int
\ (the prim DB and the toolchain's own body use n), and the unifier lets n
\ subsume any int-family code (so '( i64 -- i64 )' over an n-typed prim still
\ checks). r(3)=float. Table-driven to keep the body small (inline-safe).
: CON-OF {: a u :}                      \ multi-char name -> con code, or 0
   a u CT-FIND ;
: SGBAD-CLEAR ( -- )
   -1 SGBAD-AR-DECL !
   -1 SGBAD-AR-GOT !
   0 SGBAD !
   0 SGBAD-A !
   0 SGBAD-U !
   SGBAD-SYNTAX-KIND SGBAD-KIND ! ;

: SGBAD-SET ( ptr u8 n n -- ) {: a u kind :}
   SGBAD @ IF exit THEN
   -1 SGBAD !
   a SGBAD-A !
   u SGBAD-U !
   kind SGBAD-KIND ! ;

: SGBAD-SYNTAX! ( ptr u8 n -- )
   SGBAD-SYNTAX-KIND SGBAD-SET ;
: SGBAD-SYNTAX? ( -- bool )
   SGBAD @ SGBAD-KIND @ SGBAD-SYNTAX-KIND = and ;

: SGBAD-UNKNOWN! ( ptr u8 n -- )
   SGBAD-UNKNOWN-KIND SGBAD-SET ;

: SGBAD-UNKNOWN? ( -- bool )
   SGBAD @ SGBAD-KIND @ SGBAD-UNKNOWN-KIND = and ;
: SGBAD-BAREPTR! ( ptr u8 n -- )
   SGBAD-BAREPTR-KIND SGBAD-SET ;
: SGBAD-BAREPTR? ( -- bool )
   SGBAD @ SGBAD-KIND @ SGBAD-BAREPTR-KIND = and ;
: SGBAD-ARITY! ( ptr u8 n n n -- )  \ family applied to the wrong number of args
   {: a:ptr u:n decl:n got:n :}     \ first-wins: latch the counts with the token
   SGBAD @ 0= IF decl SGBAD-AR-DECL !  got SGBAD-AR-GOT ! THEN
   a u SGBAD-ARITY-KIND SGBAD-SET ;
: SGBAD-ARITY? ( -- bool )
   SGBAD @ SGBAD-KIND @ SGBAD-ARITY-KIND = and ;

: BAD-SIG-TYPE ( ptr u8 n -- n )
   SGBAD-UNKNOWN!
   1 MK-CON ;
: SIG-PREFIX? ( ptr u8 n ptr u8 n -- bool ) {: a:ptr u:n p:ptr v:n :}
   u v < IF RES-FALSE EXIT THEN
   a v p v CORE-STR= ;
: ATOM-TOK? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" space-" SIG-PREFIX? IF RES-TRUE EXIT THEN
   a u s" extent-" SIG-PREFIX? IF RES-TRUE EXIT THEN
   a u s" mask-" SIG-PREFIX? IF RES-TRUE EXIT THEN
   a u s" block-" SIG-PREFIX? IF RES-TRUE EXIT THEN
   a u s" geom-" SIG-PREFIX? IF RES-TRUE EXIT THEN    \ blocked 2-D shared / micro-tile geometry (tile-pipe.f)
   a u s" parity-" SIG-PREFIX? IF RES-TRUE EXIT THEN   \ pipeline double-buffer parity (tile-pipe.f)
   a u s" align-" SIG-PREFIX? ;
: FRESH-ATOM-TOK? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" fresh-region-" SIG-PREFIX? IF RES-TRUE EXIT THEN
   a u s" fresh-extent-" SIG-PREFIX? IF RES-TRUE EXIT THEN
   a u s" fresh-gen-" SIG-PREFIX? IF RES-TRUE EXIT THEN
   a u s" fresh-mask-" SIG-PREFIX? ;
: FRESH-ATOM>TYPE ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u FAM-MARK FAM-K !
   a 6 + u 6 - FAM-K @ MK-ATOM-K ;
\ SIG-FAM? ( ptr u8 n -- n bool ) : resolve a family token through the TFAM
\ registry, replacing the old PARAM-CTOR? whitelist. Returns (family-id true) or
\ (0 false) — always two items, so every caller drops the id on the false path.
\ Resolution is package-scoped (PLAN item 6): an active package resolves its own
\ (private+public) families first, then the unique public tail; top level uses
\ the global scope, where every built-in cell family lives public. Qualified
\ `PKG:tail` tokens, case validation, hidden `@` names, and ambiguity handling
\ live in the installed resolver (type-family.f TFAM-SIG-RESOLVE).
: SIG-FAM? ( ptr u8 n -- n bool ) {: a:ptr u:n :}
   CHECKER-PACKAGE-ACTIVE? IF
      CHECKER-PACKAGE-NAME CHECKER-PACKAGE-U @ a u TFAM-RESOLVE* EXIT
   THEN
   s" " a u TFAM-RESOLVE* ;
\ EXT-MARK-FREE-TAIL ( ptr u8 n -- ) : BTC-7 — mark an extent family FREE by its
\ lowercase tail, resolved through the SAME scope SIG-FAM? uses so the recorded id
\ is exactly the one SIG-END-PARAM reads off a redx<..> arg. EXTPROD: (maki/extent.f)
\ calls it for a product's free (outer) factor. An unresolvable tail is a no-op.
: EXT-MARK-FREE-TAIL ( ptr u8 n -- )
   SIG-FAM? IF EXT-MARK-FREE ELSE drop THEN ;
: TYPE-VAR-TOK? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   u 1 = IF a c@ LOWER? EXIT THEN
   RES-FALSE ;
: TYPE-BAD-CHAR? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   0 begin dup u < while
      a over + c@ dup 60 = swap dup 62 = swap 44 = or or IF drop RES-TRUE EXIT THEN
      1+
   repeat drop RES-FALSE ;
: TYPE-RESERVED? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   u 0= IF RES-TRUE EXIT THEN
   a u VREC-FIND IF drop RES-TRUE EXIT THEN drop
   a u s" field" CORE-STR= IF RES-TRUE EXIT THEN
   a u CT-FIND 0 <> IF RES-TRUE EXIT THEN
   a u SIG-FAM? IF drop RES-TRUE EXIT THEN drop
   a u ATOM-TOK? IF RES-TRUE EXIT THEN
   a u FRESH-ATOM-TOK? IF RES-TRUE EXIT THEN
   a u TYPE-VAR-TOK? IF RES-TRUE EXIT THEN
   a u TYPE-BAD-CHAR? ;

: CT-ADD-LINEAR ( ptr u8 n -- ) {: a:ptr u:n :}
   a u TYPE-RESERVED? IF s" checker: bad or duplicate signature type" 70 die THEN
   a u CTN @ CT-LINEAR 64 CS-NONE CT-SET
   LIN-NDECL @ 1 + LIN-NDECL ! ;   \ un-gate the linear kind discipline
: TOK-TYPE ( ptr u8 n -- n ) {: a:ptr u:n :}  a c@ {: c:n :}
   u 1 = c 110 = and IF 1 MK-CON ELSE          \ 'n' -> generic int (con 1)
   u 1 = c 102 = and IF CC-BOOL MK-CON ELSE     \ 'f' -> bool (a comparison result is a flag, not an int)
   u 1 = c 114 = and IF 3 MK-CON ELSE          \ 'r' -> real/float (con 3)
   a u CON-OF dup 0 <> IF MK-CON ELSE drop     \ i64/u8/u32/cell/char/str/addr/bool
   a u FRESH-ATOM-TOK? IF a u FRESH-ATOM>TYPE ELSE
   a u ATOM-TOK? IF a u MK-ATOM ELSE
   u 1 = c LOWER? and IF c VAR-OF ELSE          \ single letter -> type var
   a u BAD-SIG-TYPE THEN THEN THEN THEN THEN THEN THEN ;

\ W=1 layout local annotation (typed-locals slice 1, dot habu-typed-locals-for):
\ a bare arity-0 family tail resolves like a signature type. An enum-tier
\ layout (W=1 sum/enum) asserts the one-cell hidden term; the group bind
\ (LOC-BUNDLE-BIND) unifies the captured bundle's tag term against it, so a
\ wrong family rejects through the standard mismatch path with family fields.
\ An arity-0 CELL family asserts its nominal scalar param, exactly as a
\ signature would. Parametric spellings (fam<..>), arity>0 tails, and W>1
\ layouts stay fail-closed as unknown local annotations (their slices are
\ dotted); linear layouts never expand into locals (item 12 invariant).
: LOC-ANN-LT? ( ptr u8 n -- bool ) {: a:ptr u:n :}   \ annotation spells <args>?
   0 BEGIN dup u < WHILE
      a over + c@ 60 = IF drop RES-TRUE EXIT THEN
      1 +
   REPEAT drop RES-FALSE ;
: BAD-LOC-ANN ( ptr u8 n -- n )    \ unsupported local annotation: fail fast so the
   BAD-SIG-TYPE                    \ pinned token is the annotation itself, not ':}'
   0 OK !  -1 FAILSET ! ;
: LOC-FAM-ANN ( ptr u8 n n -- n ) {: a:ptr u:n fam:n :}
   fam TFAM-ARITY* 0 <> IF a u BAD-LOC-ANN EXIT THEN
   PARAM-SCR-N @ a u fam MK-PARAM {: pt:n :}
   pt LAYOUT-PARAM? 0= IF pt EXIT THEN             \ arity-0 cell family: nominal scalar
   pt T-WIDTH 1 <> IF a u BAD-LOC-ANN EXIT THEN    \ W>1 layout locals: dotted tail (arity-0 here, so == family width)
   pt 0 MK-HIDDEN ;
: LOCAL-TYPE ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u s" ptr" CORE-STR= IF FRESH MK-VAR MK-PTR EXIT THEN
   a u LOC-ANN-LT? IF a u BAD-LOC-ANN EXIT THEN
   a u SIG-FAM? IF a u rot LOC-FAM-ANN EXIT THEN drop
   a u TOK-TYPE ;

variable SB variable SL variable SI variable SS
variable PKA  variable PKU  variable PKHAVE          \ one-token push-back

: SB-FIELD ( -- ptr ptr u8 )
   SB 0 ptr-field ;

: SS-FIELD ( -- ptr ptr u8 )
   SS 0 ptr-field ;

: PKA-FIELD ( -- ptr ptr u8 )
   PKA 0 ptr-field ;

: SB@ ( -- ptr u8 )
   SB-FIELD @ ;

: SB! ( ptr u8 -- )
   SB-FIELD ! ;

: SS@ ( -- ptr u8 )
   SS-FIELD @ ;

: SS! ( ptr u8 -- )
   SS-FIELD ! ;

: PKA@ ( -- ptr u8 )
   PKA-FIELD @ ;

: PKA! ( ptr u8 -- )
   PKA-FIELD ! ;

: PK! ( ptr u8 n -- )
   PKU !
   PKA!
   -1 PKHAVE ! ;

: PKRESET ( -- )
   0 PKHAVE ! ;
\ NEXT-SIG-TOK ( -- a u ) : next signature token over the SB/SL/SI cursor.
\ Whitespace separates tokens, and `<`, `>`, `,` are single-token delimiters so
\ parametric types can be written without spaces: `span<space-global,f32,extent-n>`.
\ ( a 0 ) at end. Honors one pushed-back token.
: SIG-DELIM-CHAR? ( n -- bool ) {: c:n :}
   c 60 = IF RES-TRUE EXIT THEN
   c 62 = IF RES-TRUE EXIT THEN
   c 44 = ;
: NEXT-SIG-TOK ( -- ptr u8 n )
   PKHAVE @ IF 0 PKHAVE ! PKA@ PKU @ EXIT THEN
   BEGIN SI @ SL @ < SB@ SI @ + c@ 32 = and WHILE SI @ 1 + SI ! REPEAT
   SI @ SL @ < 0= IF SB@ 0 EXIT THEN
   SB@ SI @ + SS!
   SB@ SI @ + c@ SIG-DELIM-CHAR? IF SI @ 1 + SI ! SS@ 1 EXIT THEN
   BEGIN SI @ SL @ < SB@ SI @ + c@ 32 <> and
      SB@ SI @ + c@ SIG-DELIM-CHAR? 0= and WHILE SI @ 1 + SI ! REPEAT
   SS@ SB@ SI @ + SS@ - ;

: UPPER? ( n -- bool ) {: c:n :} c 64 > c 91 < and ;
: ROW-LEAD? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   u 1 <> IF RES-FALSE EXIT THEN
   a c@ UPPER? ;
: DELIM? ( ptr u8 n -- bool )                       \ stack terminator
   {: a:ptr u:n :}
   u 0 = IF RES-TRUE EXIT THEN
   a u s" --" CORE-STR= IF RES-TRUE EXIT THEN
   a u s" ]"  CORE-STR= IF RES-TRUE EXIT THEN
   a u s" |"  CORE-STR= ;

\ SIG-QUOT-XT parses a quotation ([ in -- out | rin -- rout ]) as a family
\ argument (SC-QUOT). It needs PSTACK, defined below SIG-TYPE, so it is a `defer`
\ bound with `is` to SIG-PARSE-QUOT just after PSTACK (same late-binding shape as
\ TFAM-RESOLVE-XT). Left unset until then: it is never reached before install, so
\ an unset call fails closed via DEFER-UNSET rather than the old execute-of-0.
defer SIG-QUOT-XT ( -- n )

\ EXT-REDX-BAD-ARG? ( argterm -- bool ) : BTC-7 contraction rule. `redx<E>` marks
\ extent E as a contraction (+Σ) axis; it is ill-formed when E is a FREE (outer /
\ batch) extent or a whole product `extprod<..>`, because summing the free factor of
\ a folded #B*#T row is the cross-sequence leak. A type-var arg (redx<e>, the generic
\ contraction-entry cast) resolves to no concrete extent, so it is fine.
: EXT-REDX-BAD-ARG? ( n -- bool ) {: t:n :}
   t T-RES {: r:n :}
   r TAG T-PARAM <> IF RES-FALSE EXIT THEN         \ var/con: not a concrete extent
   r PARAM>FAM {: f:n :}
   f EXT-PROD-FAM @ = EXT-PROD-FAM @ 0 <> and IF RES-TRUE EXIT THEN   \ contracting a whole product
   f EXT-FREE? ;                                   \ contracting the free (batch) factor

\ SIG-END-PARAM ( base a u fam -- t ) : close a parsed family application. Reject
\ (family-specific arity diagnostic) when the arg count differs from the family's
\ declared arity; reject a `redx<..>` over a free/product extent (BTC-7 contraction
\ rule) before it can name a summable axis; then build the T-PARAM (MK-PARAM rewinds
\ scratch to `base`).
: SIG-END-PARAM {: base:n a:ptr u:n fam:n :}
   PARAM-SCR-N @ base - {: got:n :}
   got fam TFAM-ARITY* <> IF a u fam TFAM-ARITY* got SGBAD-ARITY! THEN
   fam EXT-REDX-FAM @ =  EXT-REDX-FAM @ 0 <> and  got 1 = and IF
      base cells PARAM-SCR + @ EXT-REDX-BAD-ARG? IF a u SGBAD-SYNTAX! THEN
   THEN
   base a u fam MK-PARAM ;

\ SIG-TYPE ( ptr u8 n -- n ) : one signature type. A registered family token opens
\ `family<arg,...>`; each arg RECURSEs. `base` marks the shared scratch depth on
\ entry so nested params are reentrant (a nested family's MK-PARAM rewinds to its
\ own base, leaving the parent's already-pushed args intact). A bare family token
\ (no `<`) builds a zero-arg application whose arity check rejects arity>0 families.
\ `ptr` is dual: `ptr<space,elem>` resolves as a family here (a T-PARAM), while
\ `ptr elem` (no `<`) must ALWAYS reach the MK-PTR special case below — even when
\ `ptr` is not a live family (e.g. a suite that TFAM-RESETs the registry). So the
\ family branch only builds a real `family<...>`; every no-`<` path (and the
\ not-a-family path) falls through to the shared `ptr`/TOK-TYPE tail.
: SIG-TYPE ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u s" [" CORE-STR= IF SIG-QUOT-XT EXIT THEN   \ `[ in -- out ]` xt<effect> quot (opener consumed)
   a u SIG-FAM? IF {: fam:n :}                        \ ( id ) resolved family; build application
      NEXT-SIG-TOK 2dup s" <" CORE-STR= IF
         2drop PARAM-SCR-N @ {: base:n :}
         BEGIN
            NEXT-SIG-TOK 2dup s" >" CORE-STR= IF 2drop base a u fam SIG-END-PARAM EXIT THEN
            2dup DELIM? IF SGBAD-SYNTAX! base a u fam MK-PARAM EXIT THEN
            2dup s" [" CORE-STR= IF 2drop SIG-QUOT-XT ELSE RECURSE THEN  \ quotation arg (SC-QUOT) or nested type
            PARAM-SCR+
            NEXT-SIG-TOK 2dup s" ," CORE-STR= IF 2drop ELSE
            2dup s" >" CORE-STR= IF 2drop base a u fam SIG-END-PARAM EXIT ELSE
               SGBAD-SYNTAX! base a u fam MK-PARAM EXIT
            THEN THEN
         AGAIN
      ELSE
         PK!                                          \ push back the non-'<' token
         a u s" ptr" CORE-STR= 0= IF                  \ non-ptr family, no '<' -> 0-arg (arity reject)
            PARAM-SCR-N @ a u fam SIG-END-PARAM EXIT
         THEN                                         \ `ptr` (no '<') -> MK-PTR fall-through below
      THEN
   ELSE drop THEN                                     \ not a family: drop the 0 family-id
   a u s" ptr" CORE-STR= IF
      NEXT-SIG-TOK 2dup DELIM? IF a u SGBAD-BAREPTR! PK! 1 MK-CON ELSE RECURSE MK-PTR THEN
   ELSE a u TOK-TYPE THEN ;

create ROWMAP 26 cells allot
: ROWMAP-RESET 0 BEGIN dup cells ROWMAP + UNBOUND swap ! 1 + dup 25 > UNTIL drop ;
: RVAR-OF {: c :}  c 65 - cells ROWMAP +  dup @ UNBOUND = IF FRESH over ! THEN  @ MK-ROW ;

\ SGBAD: the declared signature is malformed (a required '--'/']' delimiter was
\ missing or wrong). A malformed contract must REJECT, never silently parse as
\ some other effect. EXPECT-SIG consumes the next sig token and fails closed if
\ it is not the expected delimiter (EOF reads as a 0-length token -> mismatch).
: EXPECT-SIG {: ea eu :}
   NEXT-SIG-TOK 2dup ea eu CORE-STR= IF 2drop ELSE SGBAD-SYNTAX! THEN ;

\ PUSH-LOGICAL ( type row -- row ) : push one parsed signature type onto a stack
\ row — the single seam for logical-vs-physical layout (docs/type-families.md
\ §10-11, item 12 slice 3b). Every ordinary type and cell family pushes one
\ logical cell (== MK-PUSH). A resolved LOGICAL sum/enum/product layout family
\ expands to its W hidden physical fields — slot0 deepest, tag on top (docs §5);
\ the whole-bundle transport surgery (XPORT-STEP?) and U-TYPE's hidden-field
\ discipline then keep the group intact. A POSSIBLY-LINEAR layout (a linear con
\ arg, or an unresolved var arg that may later bind linear) stays ONE logical
\ cell so the TFAM-11 transport reject + identity flow keep its fail-closed
\ semantics (docs §19) until whole-bundle linear counting lands.
: PUSH-LOGICAL ( n n -- n ) {: t:n row:n :}
   t T-RES {: r:n :}
   r LAYOUT-PARAM?  r HIDDEN-PARAM? 0= and IF
      r LAYOUT-ARGS-OPEN? 0= IF
         r row LAYOUT-PUSH-FIELDS EXIT   \ width known (incl. linear args): rows tell the truth
      THEN
   THEN
   t row MK-PUSH ;

\ PSTACK ( tail -- row ) : parse one stack onto a tail row. A leading single
\ upper-case token names the row (shared by letter); else the passed implicit
\ tail is used. Types fold bottom->top; '[' in -- out [ '|' rin -- rout ] ']'
\ is a quot<effect> (RECURSE for nested stacks; no '|' means rin=rout).
\ tail is a LOCAL so it survives RECURSE; the data stack holds only the row.
: PSTACK ( n -- n ) {: tail:n :}
   NEXT-SIG-TOK 2dup ROW-LEAD? IF
      drop c@ RVAR-OF                                 \ row = named var
   ELSE PK! tail THEN                                 \ push back token; row = tail
   BEGIN
     NEXT-SIG-TOK 2dup DELIM? IF PK! EXIT THEN        \ ( row a u )->PK!->( row ), return
     2dup s" [" CORE-STR= IF
        2drop
        FRESH MK-ROW                                  \ q data row
        FRESH MK-ROW                                  \ q return row
        over RECURSE                                  \ row qd qr qin
        s" --" EXPECT-SIG
        >r >r                                         \ park qin qr
        RECURSE                                       \ row qout
        r>
        NEXT-SIG-TOK 2dup s" |" CORE-STR= IF
           2drop
           dup RECURSE                                \ row qout qr qrin
           s" --" EXPECT-SIG
           >r dup RECURSE                             \ row qout qr qrout
           s" ]" EXPECT-SIG
           swap drop                                  \ row qout qrout
           r> r> 2swap >r rot r>                      \ row qin qout qrin qrout
        ELSE
           2dup s" ]" CORE-STR= IF
              2drop
           ELSE
              SGBAD-SYNTAX!
           THEN
           r> swap >r swap r> dup                     \ row qin qout qrin qrout
        THEN
        MK-QUOT
        swap MK-PUSH
     ELSE
        2dup VREC-FIND IF
           >r 2drop r> VREC-PUSH-FIELDS
        ELSE
           drop SIG-TYPE swap PUSH-LOGICAL
        THEN
     THEN
   AGAIN ;

\ SIG-PARSE-QUOT ( -- n ) : parse a quotation as a family argument (SC-QUOT),
\ with the opening '[' already consumed: "in -- out [ '|' rin -- rout ] ]". Each
\ sub-stack is a full PSTACK; in/out share one fresh data base row and rin/rout
\ share one fresh return base row (row-polymorphic tails). No '|' means the return
\ effect is neutral (rin = rout = the empty return base). Malformed rows (a missing
\ '--' or ']') set SGBAD-SYNTAX! through EXPECT-SIG so the whole signature rejects.
\ Installed into SIG-QUOT-XT so SIG-TYPE (defined above PSTACK) can reach it.
: SIG-PARSE-QUOT ( -- n )
   FRESH MK-ROW {: qdbase:n :}
   FRESH MK-ROW {: qrbase:n :}
   qdbase PSTACK {: qin:n :}                 \ data-in row (stops at '--')
   s" --" EXPECT-SIG
   qdbase PSTACK {: qout:n :}                \ data-out row (same base tail)
   NEXT-SIG-TOK 2dup s" |" CORE-STR= IF
      2drop
      qrbase PSTACK {: qrin:n :}
      s" --" EXPECT-SIG
      qrbase PSTACK {: qrout:n :}
      s" ]" EXPECT-SIG
      qin qout qrin qrout MK-QUOT
   ELSE
      2dup s" ]" CORE-STR= IF 2drop ELSE SGBAD-SYNTAX! THEN
      qin qout qrbase qrbase MK-QUOT         \ no return clause: rin = rout = base
   THEN ;
: SIG-QUOT-INSTALL ( -- ) [: SIG-PARSE-QUOT ;] is SIG-QUOT-XT ;
SIG-QUOT-INSTALL

variable SGHASR                          \ a return-stack clause ( ... | rin -- rout ) present?
variable RR-SHARED                       \ the shared return row, allocated lazily on '|'
variable PD-IN variable PR-IN variable PD-OUT variable PR-OUT variable PD-BASE

: RRTAIL ( -- n )                        \ shared return row (allocate once, on demand)
   RR-SHARED @ dup 0= IF drop FRESH MK-ROW dup RR-SHARED ! THEN ;

\ PSIDE ( dtail -- drow rrow ) : one side = data stack [ '|' return stack ]. No
\ '|' -> rrow = the shared return row so far (0 if no clause anywhere) — CHECK
\ ignores it. The return row is allocated only when a '|' actually appears, so
\ ordinary sigs cost no extra typevars.
: PSIDE ( n -- n n ) {: dtail:n :}
   dtail PSTACK                                   \ data part (stops at | -- ])
   NEXT-SIG-TOK 2dup s" |" CORE-STR= IF
      2drop  -1 SGHASR !  RRTAIL PSTACK           \ ( drow rrow ) explicit return
   ELSE PK! RR-SHARED @ THEN ;                    \ no | here -> shared tail (untouched)

\ PSIG ( -- din dout rin rout ) : data + return rows over the cursor.
: PSIG ( -- n n n n )
   PKRESET NMAP-RESET ROWMAP-RESET FAM-RESET  0 SGHASR !  0 RR-SHARED !
   FRESH MK-ROW dup PD-BASE ! {: dr :}
   dr PSIDE  PR-IN ! PD-IN !
   s" --" EXPECT-SIG                              \ require the top-level '--'
   dr PSIDE  PR-OUT ! PD-OUT !
   PD-IN @ PD-OUT @ PR-IN @ PR-OUT @ ;

\ PARSE-SIG-RAW ( a u -- din dout rin rout ) : the declared effect as four rows
\ (no CHECKER-STEP), for verifying a definition's body against its own ( in -- out ).
: PARSE-SIG-RAW ( ptr u8 n -- n n n n ) {: a:ptr u:n :}
   a SB!
   u SL !
   0 SI !
   PSIG ;

\ Parse one type application for the generative LAYOUT-BUFFER definer. This is
\ the allocation-certificate gate: only a closed, non-linear, addressable
\ layout — or an arity-0 nominal-scalar cell family (width 1, no variants,
\ zero image = family id 0, a valid id) — is admitted, and the exact family
\ id/width are returned to the fixed definer implementation. Ordinary
\ unification never receives this authority.
variable LBI-T
variable LBI-BAD

: CHECKER-LAYOUT-INFO ( ptr u8 n -- n n bool ) {: a:ptr u:n :}
   NEW
   SGBAD-CLEAR
   PKRESET NMAP-RESET ROWMAP-RESET FAM-RESET
   a SB!  u SL !  0 SI !
   NEXT-SIG-TOK dup 0= IF 2drop 0 0 RES-FALSE EXIT THEN
   SIG-TYPE T-RES LBI-T !
   NEXT-SIG-TOK dup 0 <> LBI-BAD ! 2drop
   SGBAD @ 0 <> LBI-BAD @ 0 <> or IF 0 0 RES-FALSE EXIT THEN
   LBI-T @ HIDDEN-PARAM? IF 0 0 RES-FALSE EXIT THEN
   LBI-T @ NOM-SCALAR? IF LBI-T @ PARAM>FAM  LBI-T @ T-WIDTH  RES-TRUE EXIT THEN
   LBI-T @ LAYOUT-PARAM? 0= IF 0 0 RES-FALSE EXIT THEN
   LBI-T @ LAYOUT-MEM-OK? 0= IF 0 0 RES-FALSE EXIT THEN
   LBI-T @ PARAM>FAM  LBI-T @ T-WIDTH  RES-TRUE ;

\ Admissibility gate for the TYPED-VARIABLE / TYPED-BUFFER convenience definers
\ (dot habu-nominal-storage-typed). Superset of CHECKER-LAYOUT-INFO: it admits
\ everything LAYOUT-BUFFER does — an arity-0 nominal scalar family, or a closed
\ non-linear addressable layout family — AND a CLOSED TYPED POINTER: a `ptr`
\ whose pointee chain bottoms out at a nominal scalar or a closed non-linear
\ layout family — AND a CLOSED xt<effect> QUOTATION CELL `[ in -- out ]` (dot
\ habu-typed-xt-storage-ddad4af8): a persistent monomorphic code cell whose @
\ recovers the declared T-QUOT so `HK @ execute` fit-checks the row against E and
\ a typed store fit-checks a word's certified effect against E. A bare `ptr a`/
\ `ptr n`, an open var/arg, a linear value, a hidden field, and any trailing token
\ all reject; a malformed quotation body rejects through SGBAD. LAYOUT-BUFFER keeps
\ its own narrower CHECKER-LAYOUT-INFO gate unchanged, so the two capabilities
\ stay distinct. Width is CELL-uniform (1) for nominal scalars and typed
\ pointers, and the registry width for a layout family.
: STORAGE-PTR-POINTEE-OK? ( n -- bool )   \ resolved pointee admissible for a stored typed pointer
   dup NOM-SCALAR? IF drop RES-TRUE EXIT THEN
   dup LAYOUT-PARAM? IF
      dup LAYOUT-MAYBE-LINEAR? IF drop RES-FALSE EXIT THEN
      LAYOUT-ARGS-OPEN? 0= EXIT THEN
   dup TAG T-PTR = IF PTR>INNER T-RES RECURSE EXIT THEN
   drop RES-FALSE ;
: STORAGE-TYPED-PTR? ( n -- bool )   \ resolved term is a closed typed pointer
   dup TAG T-PTR <> IF drop RES-FALSE EXIT THEN
   PTR>INNER T-RES STORAGE-PTR-POINTEE-OK? ;
: CHECKER-STORAGE-INFO ( ptr u8 n -- n bool ) {: a:ptr u:n :}
   NEW
   SGBAD-CLEAR
   PKRESET NMAP-RESET ROWMAP-RESET FAM-RESET
   a SB!  u SL !  0 SI !
   NEXT-SIG-TOK dup 0= IF 2drop 0 RES-FALSE EXIT THEN
   SIG-TYPE T-RES LBI-T !
   NEXT-SIG-TOK dup 0 <> LBI-BAD ! 2drop
   SGBAD @ 0 <> LBI-BAD @ 0 <> or IF 0 RES-FALSE EXIT THEN
   LBI-T @ HIDDEN-PARAM? IF 0 RES-FALSE EXIT THEN
   LBI-T @ TAG T-QUOT = IF 1 RES-TRUE EXIT THEN       \ closed xt<effect> cell: one code cell (dot habu-typed-xt-storage-ddad4af8)
   LBI-T @ NOM-SCALAR? IF LBI-T @ T-WIDTH RES-TRUE EXIT THEN
   LBI-T @ STORAGE-TYPED-PTR? IF LBI-T @ T-WIDTH RES-TRUE EXIT THEN
   LBI-T @ LAYOUT-PARAM? 0= IF 0 RES-FALSE EXIT THEN
   LBI-T @ LAYOUT-MEM-OK? 0= IF 0 RES-FALSE EXIT THEN
   LBI-T @ T-WIDTH RES-TRUE ;

: VREC-ROOM ( -- )
   VREC-ENSURE ;

: VREC-BEGIN ( ptr u8 n -- n ) {: a:ptr u:n :}
   VREC-ROOM
   VREC-N @ {: id:n :}
   a u VREC-STR-COPY {: dst:ptr len:n :}
   id 1 + VREC-N !
   dst id VREC-NAME-A-FIELD !
   len id cells VREC-NAME-U + !
   VREC-FIELD-N @ id cells VREC-START + !
   0 id cells VREC-COUNT + !
   0 id cells VREC-TVN + !
   0 id cells VREC-RVN + !
   id ;

: VREC-FINISH ( n -- ) {: id:n :}
   VREC-FIELD-N @ id VREC-START@ - {: n:n :}
   n 0 <= IF s" checker: empty value-record" 70 die THEN
   n id cells VREC-COUNT + !
   VRC-TVN @ id cells VREC-TVN + !
   VRC-RVN @ id cells VREC-RVN + ! ;

: VREC-FIELD-WRAP ( ptr u8 n ptr u8 n n -- n )
   {: rec:ptr recu:n fld:ptr fldu:n typ:n :}
   PARAM-SCR-N @ {: base:n :}
   rec recu MK-ATOM PARAM-SCR+
   fld fldu MK-ATOM PARAM-SCR+
   typ PARAM-SCR+
   base s" field" FIELD-FAM @ MK-PARAM ;   \ base a u fam -> field<rec,name,inner>

: VREC-FIELD-STORE ( ptr u8 n ptr u8 n n -- )
   {: rec:ptr recu:n fld:ptr fldu:n typ:n :}
   rec recu fld fldu typ VREC-FIELD-WRAP VREC-COPY VREC-FIELD! ;

: VREC-ATOM-COPY= ( ptr u8 n n -- bool ) {: a:ptr u:n node:n :}
   node VN.TAG@ VR-ATOM <> IF RES-FALSE EXIT THEN
   node VREC-I-STR a u CORE-STR= ;

: VREC-FIELD-NAME= ( ptr u8 n n -- bool ) {: a:ptr u:n node:n :}
   node VN.TAG@ VR-PARAM <> IF RES-FALSE EXIT THEN
   a u node 1 VN>ARG VREC-ATOM-COPY= ;   \ field name is arg[1] of field<rec,name,type>

: VREC-FIELD-DUP? ( ptr u8 n n -- bool ) {: a:ptr u:n id:n :}
   id VREC-START@ VREC-J !
   BEGIN VREC-J @ VREC-FIELD-N @ < WHILE
      a u VREC-J @ VREC-FIELD@ VREC-FIELD-NAME= IF RES-TRUE EXIT THEN
      VREC-J @ 1 + VREC-J !
   REPEAT
   RES-FALSE ;

: VREC-FIELD-BAD? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   u 0= IF RES-TRUE EXIT THEN
   a u DELIM? ;

: VREC-PARSE-FIELDS ( n ptr u8 n ptr u8 n -- )
   {: id:n rec:ptr recu:n fields:ptr fieldsu:n :}
   fields SB! fieldsu SL ! 0 SI !
   PKRESET NMAP-RESET ROWMAP-RESET FAM-RESET SGBAD-CLEAR
   VREC-COPY-RESET
   BEGIN
      NEXT-SIG-TOK dup 0= IF 2drop SGBAD @ IF s" checker: bad value-record field type" 70 die THEN EXIT THEN
      2dup VREC-FIELD-BAD? IF 2dup SGBAD-SYNTAX! 2drop s" checker: bad value-record field" 70 die THEN
      2dup id VREC-FIELD-DUP? IF 2drop s" checker: duplicate value-record field" 70 die THEN
      NEXT-SIG-TOK dup 0= IF 2drop 2drop s" checker: bad value-record field type" 70 die THEN
      SIG-TYPE
      >r rec recu 2swap r> VREC-FIELD-STORE
      SGBAD @ IF s" checker: bad value-record field type" 70 die THEN
   AGAIN ;

: CHECKER-DEFRECORD ( ptr u8 n ptr u8 n -- )
   {: name:ptr nameu:n fields:ptr fieldsu:n :}
   name nameu TYPE-RESERVED? IF s" checker: bad or duplicate value-record type" 70 die THEN
   name nameu VREC-BEGIN {: id:n :}
   id name nameu fields fieldsu VREC-PARSE-FIELDS
   id VREC-FINISH ;

\ Structured internal effects. Textual signatures are source-boundary input
\ only; checker-owned token semantics construct rows directly.
: STEP-TYPE-OUT ( n -- ) {: t:n :}
   FRESH MK-ROW {: rest:n :}
   rest
   t rest MK-PUSH
   CHECKER-STEP ;

: STEP-TYPE-IN ( n -- ) {: t:n :}
   FRESH MK-ROW {: rest:n :}
   t rest MK-PUSH
   rest CHECKER-STEP ;

: STEP-TYPE2-IN ( n n -- ) {: a:n b:n :}
   FRESH MK-ROW {: rest:n :}
   a rest MK-PUSH
   b swap MK-PUSH
   rest CHECKER-STEP ;

: STEP-N-IN ( -- )
   CC-N MK-CON STEP-TYPE-IN ;

: STEP-N-OUT ( -- )
   CC-N MK-CON STEP-TYPE-OUT ;

: STEP-R-OUT ( -- )
   CC-R MK-CON STEP-TYPE-OUT ;

: STEP-BOOL-IN ( -- )
   CC-BOOL MK-CON STEP-TYPE-IN ;

: STEP-NN-IN ( -- )
   CC-N MK-CON CC-N MK-CON STEP-TYPE2-IN ;

: STEP-FETCH ( -- )
   FRESH MK-VAR FRESH MK-ROW {: t:n rest:n :}
   t MK-PTR rest MK-PUSH
   t rest MK-PUSH
   CHECKER-STEP ;

: STEP-STORE ( -- )
   FRESH MK-VAR FRESH MK-ROW {: t:n rest:n :}
   t rest MK-PUSH
   t MK-PTR swap MK-PUSH
   rest CHECKER-STEP ;

variable FP
\ user sigs: certified words recorded as effect records after the structural
\ primitive-effect prefix. The renderer appends user records so later wins.
\ The baked checker image stores canonical typed effect graphs for certified
\ words, not rendered signature strings. The static boot arena must hold that
\ snapshot without relying on process-local mmap state.
$800000 constant USIGS-INIT-CAP
$10000 constant USIGS-GRAIN
$7FFFFFFFFFFFFFFF constant USIGS-MAX-CAP
3 constant USIGS-PROT-RW
$1002 constant USIGS-MAP-ANON
-1 constant USIGS-ANON-FD
0 constant USIGS-OFF-ZERO
variable USIGS-P   variable USIGS-CAP-U   variable UEND
variable USIGS-USER-OFF
variable USIGS-GROW-CAP   variable USIGS-GROW-NEXT
variable CHK-CAND
PTR-VARIABLE USIGS-SNAP-P

: USIGS ( -- ptr u8 ) USIGS-P @ ;

\ USIGS is a byte-addressed store (ptr u8), but its head cell holds a real cell
\ value the checker metadata writes with `!`. USIGS-CELL-AT refines a cell-aligned
\ offset into that byte store to a cell pointer so the head/metadata stores stay
\ typed while the byte-copy paths keep ptr u8.
TRUSTED: USIGS-CELL-AT ( n -- ptr a )
   USIGS swap + ;

: USIGS-HEAD ( -- ptr a )
   0 USIGS-CELL-AT ;

0 USIGS-USER-OFF !
0 CHK-CAND !

\ CONSTRUCT-WIDE-STAGED-REJECT ( -- ) : staged lowering fail-closed for a
\ parametric MULTI-CELL construct/constructor (layout-cap slice 3-4). The checker
\ certifies the effect (the rows are sound), but v1 lowering keys pads off the
\ DECLARED family width (TFL-VPADS / the generated ctor body), which is WRONG for
\ a wide instantiation (option<tdpbw2> tag at slot 2, not slot 1) — the dual
\ emitter is slice 4. So a REAL definition (CHK-CAND = 0) that would reach codegen
\ fails closed here rather than emit declared-width pads; a CHECK-CANDIDATE probe
\ (CHK-CAND ≠ 0) still certifies the type. No new diagnostic code: it renders the
\ ordinary E-REJECTED, and only fires when the construct is genuinely wide
\ (CONSTRUCT-DECL-MULTICELL?), so nothing else moves.
: CONSTRUCT-WIDE-STAGED-REJECT ( -- )
   CHK-CAND @ 0 <> IF EXIT THEN
   0 OK !  -1 FAILSET ! ;

variable UCP-I

\ USIGS-COPY ( ptr a ptr a n -- ) : cell-wise store copy with a byte tail
\ (ARM64 tolerates unaligned cell access; spans can start byte-aligned).
: USIGS-COPY {: src:ptr dst:ptr n:n :}
   0 UCP-I !
   begin UCP-I @ CELL + n <= while
      src UCP-I @ + @ dst UCP-I @ + !
      UCP-I @ CELL + UCP-I !
   repeat
   begin UCP-I @ n < while
      src UCP-I @ + c@ dst UCP-I @ + c!
      UCP-I @ 1 + UCP-I !
   repeat ;

: USIGS-ROUND-CAP {: need :}
   need 0 <= IF s" checker: bad user sig cap" 76 die THEN
   need USIGS-MAX-CAP USIGS-GRAIN - > IF s" checker: user sigs too large" 76 die THEN
   need 1 - USIGS-GRAIN / 1 + USIGS-GRAIN * ;

: USIGS-MMAP-RC ( n -- n )
   0 swap USIGS-PROT-RW USIGS-MAP-ANON USIGS-ANON-FD USIGS-OFF-ZERO mmap
   dup 0 < IF s" checker: user sigs mmap failed" 76 die THEN ;

TRUSTED: USIGS-RC>PTR ( n -- ptr u8 ) ;

: USIGS-ALLOC ( n -- ptr u8 )
   USIGS-MMAP-RC USIGS-RC>PTR ;

: USIGS-CLEAR ( -- )
   0 UEND !
   0 USIGS-HEAD !
   0 USIGS-GROW-CAP !
   0 USIGS-GROW-NEXT ! ;

: USIGS-ALLOC-INIT ( -- )
   USIGS-INIT-CAP USIGS-ALLOC USIGS-P !
   USIGS-INIT-CAP USIGS-CAP-U ! ;

: USIGS-RUNTIME-INIT ( -- )
   USIGS-ALLOC-INIT
   USIGS-CLEAR ;

USIGS-RUNTIME-INIT

: USIGS-RUNTIME-SIZED? ( -- bool )
   USIGS-P @ 0 = 0=
   USIGS-CAP-U @ USIGS-INIT-CAP >= and ;

: USIGS-RESET ( -- )
   USIGS-RUNTIME-SIZED? 0= IF USIGS-ALLOC-INIT THEN
   USIGS-CLEAR
   0 USIGS-USER-OFF ! ;

: USIGS-SNAP@ ( -- ptr u8 )
   USIGS-SNAP-P @ ;

: USIGS-SNAPSHOT-SIZE ( -- n )
   UEND @ CELL + ;

: USIGS-SNAPSHOT-ALLOC ( n -- ptr u8 ) {: n:n :}
   here USIGS-SNAP-P !
   n allot
   USIGS-SNAP@ ;

\ USIGS-POW2-CAP ( n -- n ) : smallest power-of-2 multiple of the grain >= n,
\ so a restored snapshot has append headroom instead of cap == size.
: USIGS-POW2-CAP {: need:n :}
   need USIGS-ROUND-CAP drop                 \ range check only
   USIGS-GRAIN
   begin dup need < while 2 * repeat ;

: USIGS-SNAPSHOT-PERSIST ( -- )
   USIGS-SNAPSHOT-SIZE {: n:n :}
   n USIGS-POW2-CAP {: cap:n :}
   cap USIGS-SNAPSHOT-ALLOC {: dst:ptr :}
   USIGS dst n USIGS-COPY
   dst USIGS-P !
   cap USIGS-CAP-U !
   0 USIGS-GROW-CAP !
   0 USIGS-GROW-NEXT ! ;

\ USIGS-GROW ( n -- ) : geometric growth — at least double the current cap so
\ regrowth copies the store O(log) times, not once per appended grain.
: USIGS-GROW {: need :}
   need USIGS-CAP-U @ 2 * max USIGS-ROUND-CAP USIGS-GROW-CAP !
   USIGS-GROW-CAP @ USIGS-ALLOC USIGS-GROW-NEXT !
   USIGS USIGS-GROW-NEXT @ UEND @ CELL + USIGS-COPY
   USIGS-GROW-NEXT @ USIGS-P !
   USIGS-GROW-CAP @ USIGS-CAP-U ! ;

: USIGS-ENSURE {: need :}
   need USIGS-CAP-U @ <= IF exit THEN
   need USIGS-GROW ;

: UB! {: c :}  c USIGS UEND @ + c!  UEND @ 1 + UEND ! ;

: UBS ( ptr u8 n -- ) {: a:ptr u:n :}
   0 BEGIN
      dup u <
   WHILE
      dup a + c@ UB!
      1 +
   REPEAT drop ;

\ UALIGN ( n -- n )
: UALIGN 7 + $FFFFFFFFFFFFFFF8 and ;

\ UALIGN! ( -- )
: UALIGN! UEND @ UALIGN UEND ! ;

: U!+ ( n -- ) {: x:n :}
   x UEND @ USIGS-CELL-AT !
   UEND @ CELL + UEND ! ;

\ UTERM! ( -- )
: UTERM! 0 UEND @ USIGS-CELL-AT ! ;

: USIGS-RESTORE-END ( n -- )
   UEND !
   UTERM! ;

: USIGS-USER ( -- ptr a )
   USIGS USIGS-USER-OFF @ + ;

: SYM-FOLD-C ( n -- n ) {: c:n :}
   c $41 < if c exit then
   c $5A > if c exit then
   c $20 or ;

: SYM-STR=CI ( ptr u8 n ptr u8 n -- bool ) {: a:ptr u:n b:ptr v:n :}
   u v <> if RES-FALSE exit then
   0 begin dup u < while
      dup a + c@ SYM-FOLD-C
      over b + c@ SYM-FOLD-C <> if drop RES-FALSE exit then
      1+
   repeat drop
   RES-TRUE ;

\ SYM-CAP is a live power-of-2 cap (HIDX masks by SYM-CAP 1 -). It grows
\ geometrically; the HIDX table (sized/masked by SYM-CAP) is rebuilt — rehashed —
\ at the new cap on the next lookup. SYM-STR relocation rebases the PKG-A/NAME-A
\ pointers of every existing record.
$4000 constant SYM-CAP-INIT     \ symbol table records (grows on demand, pow2)
$100000 constant SYM-STR-INIT    \ symbol string pool (grows on demand)
variable SYM-CAP-V   SYM-CAP-INIT SYM-CAP-V !
: SYM-CAP ( -- n ) SYM-CAP-V @ ;
variable SYM-STR-CAP-V   SYM-STR-INIT SYM-STR-CAP-V !
: SYM-STR-CAP ( -- n ) SYM-STR-CAP-V @ ;
0 constant SYM-GLOBAL
1 constant SYM-PRIVATE
2 constant SYM-PUBLIC

0 constant SYM-PKG-A-CELL
1 constant SYM-PKG-U-CELL
2 constant SYM-NAME-A-CELL
3 constant SYM-NAME-U-CELL
4 constant SYM-VIS-CELL
$0 constant SYM-PKG-A-OFF
$8 constant SYM-PKG-U-OFF
$10 constant SYM-NAME-A-OFF
$18 constant SYM-NAME-U-OFF
$20 constant SYM-VIS-OFF
$28 constant SYM-REC
$8 constant SYM-REC-ALIGN
$5 constant SYM-REC-PTR-MASK

: SYM.PKG-A ( ptr a -- ptr ptr a )
   SYM-PKG-A-CELL ptr-field ;

: SYM.PKG-U ( ptr a -- ptr a )
   SYM-PKG-U-OFF + ;

: SYM.NAME-A ( ptr a -- ptr ptr a )
   SYM-NAME-A-CELL ptr-field ;

: SYM.NAME-U ( ptr a -- ptr a )
   SYM-NAME-U-OFF + ;

: SYM.VIS ( ptr a -- ptr a )
   SYM-VIS-OFF + ;

: CHECKER-RECORD-LAYOUT= ( n n -- )
   <> if s" checker: internal record layout mismatch" CORE-LAYOUT-RC die then ;

: CHECKER-RECORD-LAYOUT? ( bool -- )
   0= if s" checker: internal record layout mismatch" CORE-LAYOUT-RC die then ;

: CHECKER-RECORD-FIELD= ( ptr a ptr a n -- )
   + = CHECKER-RECORD-LAYOUT? ;

: SYM-LAYOUT-OFFSETS ( -- )
   SYM-PKG-A-CELL cells SYM-PKG-A-OFF CHECKER-RECORD-LAYOUT=
   SYM-PKG-U-CELL cells SYM-PKG-U-OFF CHECKER-RECORD-LAYOUT=
   SYM-NAME-A-CELL cells SYM-NAME-A-OFF CHECKER-RECORD-LAYOUT=
   SYM-NAME-U-CELL cells SYM-NAME-U-OFF CHECKER-RECORD-LAYOUT=
   SYM-VIS-CELL cells SYM-VIS-OFF CHECKER-RECORD-LAYOUT= ;

: SYM-LAYOUT-SIZE ( -- )
   CELL SYM-REC-ALIGN CHECKER-RECORD-LAYOUT=
   SYM-VIS-OFF CELL + SYM-REC CHECKER-RECORD-LAYOUT=
   SYM-REC SYM-REC-ALIGN mod 0 CHECKER-RECORD-LAYOUT= ;

: SYM-LAYOUT-META ( -- )
   1 SYM-PKG-A-CELL lshift
   1 SYM-NAME-A-CELL lshift or
   SYM-REC-PTR-MASK CHECKER-RECORD-LAYOUT= ;

: SYM-LAYOUT-PTRS ( -- )
   here dup SYM.PKG-A swap SYM-PKG-A-CELL ptr-field = CHECKER-RECORD-LAYOUT?
   here dup SYM.NAME-A swap SYM-NAME-A-CELL ptr-field = CHECKER-RECORD-LAYOUT? ;

: SYM-LAYOUT-ASSERT ( -- )
   SYM-LAYOUT-OFFSETS
   SYM-LAYOUT-SIZE
   SYM-LAYOUT-META
   SYM-LAYOUT-PTRS ;

SYM-LAYOUT-ASSERT

create SYMS-BOOT SYM-CAP-INIT SYM-REC * allot
create SYM-STR-BOOT SYM-STR-INIT allot
variable SYMS-P     SYMS-BOOT SYMS-P !
variable SYM-STR-P  SYM-STR-BOOT SYM-STR-P !
variable SYM-N
variable SYM-STR-U
variable SYM-I
variable SYM-DST
variable SYM-ID

: SYMS ( -- ptr a ) SYMS-P @ ;
: SYM-STR ( -- ptr u8 ) SYM-STR-P @ ;

1 SYM-N !
0 SYM-STR-U !

: SYM-ROW ( n -- ptr a )
   SYM-REC * SYMS + ;

: SYM-PKG-A-FIELD ( n -- ptr ptr a )
   SYM-ROW SYM.PKG-A ;

: SYM-NAME-A-FIELD ( n -- ptr ptr a )
   SYM-ROW SYM.NAME-A ;

: SYM-DST-FIELD ( -- ptr ptr u8 )
   SYM-DST 0 ptr-field ;

: SYM-DST@ ( -- ptr u8 )
   SYM-DST-FIELD @ ;

: SYM-DST! ( ptr u8 -- )
   SYM-DST-FIELD ! ;

: SYM-PKG$ ( n -- ptr u8 n )
   dup SYM-PKG-A-FIELD @
   swap SYM-ROW SYM.PKG-U @ ;

: SYM-NAME$ ( n -- ptr u8 n )
   dup SYM-NAME-A-FIELD @
   swap SYM-ROW SYM.NAME-U @ ;

\ SYM-STR-REBASE ( delta -- ) : a SYM-STR relocation moved the pool by delta; add
\ it to the PKG-A/NAME-A pointer of every existing record so lookups still resolve.
: SYM-STR-REBASE ( n -- ) {: delta:n :}
   1 SYM-I !
   begin SYM-I @ SYM-N @ < while
      SYM-I @ SYM-PKG-A-FIELD {: pf:ptr :}   pf @ delta + pf !
      SYM-I @ SYM-NAME-A-FIELD {: nf:ptr :}  nf @ delta + nf !
      SYM-I @ 1 + SYM-I !
   repeat ;

: SYM-STR-GROW ( n -- ) {: need:n :}
   need SYM-STR-CAP-V @ 2 * max {: nc:n :}
   SYM-STR-P @ {: old:ptr :}
   old SYM-STR-CAP-V @ nc ARENA-BYTES-GROW {: new:ptr :}
   new SYM-STR-P !   nc SYM-STR-CAP-V !
   new old - SYM-STR-REBASE ;

: SYM-STR-ENSURE ( n -- ) {: add:n :}   \ ensure room for `add` more string bytes
   SYM-STR-U @ add + SYM-STR-CAP-V @ <= IF exit THEN
   SYM-STR-U @ add + SYM-STR-GROW ;

: SYM-STR-NEED ( n -- )
   SYM-STR-ENSURE ;

: SYM-COPY-FOLD ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   u SYM-STR-NEED
   SYM-STR SYM-STR-U @ + SYM-DST!
   0 SYM-I !
   begin SYM-I @ u < while
      a SYM-I @ + c@ SYM-FOLD-C SYM-DST@ SYM-I @ + c!
      SYM-I @ 1 + SYM-I !
   repeat
   SYM-STR-U @ u + SYM-STR-U !
   SYM-DST@ u ;

\ typed-local-lint: allow-bare-local - pkg/name preserve ptr u8 roles.
: SYM-MATCH? ( ptr u8 n n ptr u8 n n -- bool )
   {: pkg pkgu:n vis:n name nameu:n id:n :}
   id SYM-ROW SYM.VIS @ vis <> IF RES-FALSE EXIT THEN
   id SYM-PKG$ pkg pkgu SYM-STR=CI 0= IF RES-FALSE EXIT THEN
   id SYM-NAME$ name nameu SYM-STR=CI ;

\ --- symbol hash index + current-state cache. SYMS and the USIGS/NORETS/
\ DFERS/PES stores stay authoritative; this is a process-local mmap cache.
\ Buckets map folded (pkg,vis,name) to a symbol id and mirror SYMS rows
\ exactly: SYM-INTERN pushes new rows front, scope exit retires its rows
\ newest-first (LIFO, so each retired row is at its bucket head), anything
\ else rebuilds from SYMS. Per-symbol cells memoize the CURRENT effect
\ record offset, control flags, defer flag, and first prim slot. A cached
\ cell is valid only while its epoch cell equals HIDX-EPOCH; the sync words
\ catch every store rewind or swap (UEND/USIGS-P for effects, NORET-END for
\ control flags) and bump the epoch, so scope/candidate rollback, signature
\ truncation, forget words, and arena swaps invalidate the whole cache at
\ O(1) and the next lookup re-derives from the authoritative store.
\ Snapshot prepare drops the mapping; a restored image rebuilds lazily.
0 constant HT-BKT
1 constant HT-NEXT
2 constant HT-EFF-V
3 constant HT-EFF-E
4 constant HT-CTL-V
5 constant HT-CTL-E
6 constant HT-DFR-V
7 constant HT-DFR-E
8 constant HT-PRM-V
9 constant HT-PRM-E
10 constant HIDX-TABLES
$CBF29CE484222325 constant HIDX-FNV-BASIS
$100000001B3 constant HIDX-FNV-PRIME
variable HIDX-MEM
variable HIDX-VALID
variable HIDX-EPOCH
variable HIDX-EFF-HI
variable HIDX-EFF-BASE
variable HIDX-CTL-HI
variable HIDX-DFR-HI    \ max DFER-END a cached defer answer depends on (rollback sync)
variable HIDX-H
variable HIDX-I
variable HIDX-CUR

\ HIDX-MEM/HIDX-EFF-BASE hold typed pointers into the mmap cache and USIGS store.
\ A plain variable @ yields a bare cell value, so the store base is read through a
\ cell-indexed ptr-field view (ptr ptr a) to preserve the nested pointer role.
: HIDX-MEM-FIELD ( -- ptr ptr a )
   HIDX-MEM 0 ptr-field ;

: HIDX-MEM@ ( -- ptr a )
   HIDX-MEM-FIELD @ ;

: HIDX-MEM! ( ptr a -- )
   HIDX-MEM-FIELD ! ;

\ HIDX-MEM-NULL: the unallocated-cache sentinel is a null pointer; the checker
\ cannot type a literal 0 as ptr a, so this one-line refinement asserts it.
TRUSTED: HIDX-MEM-NULL ( -- ptr a )
   0 ;

: HIDX-MEM-CLEAR ( -- )
   HIDX-MEM-NULL HIDX-MEM! ;

: HIDX-MEM-READY? ( -- bool )
   HIDX-MEM@ 0= 0= ;

: HIDX-EFF-BASE-FIELD ( -- ptr ptr u8 )
   HIDX-EFF-BASE 0 ptr-field ;

: HIDX-EFF-BASE@ ( -- ptr u8 )
   HIDX-EFF-BASE-FIELD @ ;

: HIDX-EFF-BASE! ( ptr u8 -- )
   HIDX-EFF-BASE-FIELD ! ;

: HIDX-EFF-BASE-CLEAR ( -- )
   USIGS HIDX-EFF-BASE! ;

HIDX-MEM-CLEAR   0 HIDX-VALID !   1 HIDX-EPOCH !
0 HIDX-EFF-HI !   HIDX-EFF-BASE-CLEAR   0 HIDX-CTL-HI !   0 HIDX-DFR-HI !

: HIDX-CELL ( n n -- ptr a ) {: slot:n tbl:n :}
   tbl SYM-CAP * slot + cells HIDX-MEM@ + ;

: HIDX-H+ ( n -- )
   SYM-FOLD-C HIDX-H @ xor HIDX-FNV-PRIME * HIDX-H ! ;

: HIDX-H$ ( ptr u8 n -- ) {: a:ptr u:n :}
   0 begin dup u < while
      dup a + c@ HIDX-H+
      1 +
   repeat drop ;

\ typed-local-lint: allow-bare-local - pkg/name preserve ptr u8 roles.
: HIDX-HASH ( ptr u8 n n ptr u8 n -- n ) {: pkg pkgu:n vis:n name nameu:n :}
   HIDX-FNV-BASIS HIDX-H !
   pkg pkgu HIDX-H$
   vis HIDX-H+
   name nameu HIDX-H$
   HIDX-H @ SYM-CAP 1 - and ;

: HIDX-BKT ( n -- ptr a )
   HT-BKT HIDX-CELL ;

: HIDX-ROW-HASH ( n -- n ) {: id:n :}
   id SYM-PKG$ id SYM-ROW SYM.VIS @ id SYM-NAME$ HIDX-HASH ;

: HIDX-EP0 ( n -- ) {: id:n :}
   0 id HT-EFF-E HIDX-CELL !
   0 id HT-CTL-E HIDX-CELL !
   0 id HT-DFR-E HIDX-CELL !
   0 id HT-PRM-E HIDX-CELL ! ;

: HIDX-SYM+ ( n -- ) {: id:n :}
   id HIDX-EP0
   id HIDX-ROW-HASH HIDX-BKT {: b:ptr :}
   b @ id HT-NEXT HIDX-CELL !
   id b ! ;

: HIDX-SYM-POP ( n -- ) {: id:n :}
   id HIDX-ROW-HASH HIDX-BKT {: b:ptr :}
   b @ id <> IF s" checker: symbol index corrupt" 76 die THEN
   id HT-NEXT HIDX-CELL @ b ! ;

\ HIDX-SYMS-RETIRE ( n -- ) : pop rows [n, SYM-N) before a scope restores SYM-N.
: HIDX-SYMS-RETIRE {: keep:n :}
   HIDX-VALID @ 0= IF EXIT THEN
   SYM-N @ 1 -
   begin dup keep >= while
      dup HIDX-SYM-POP
      1 -
   repeat drop ;

: HIDX-EPOCH+ ( -- )
   HIDX-EPOCH @ 1 + HIDX-EPOCH !
   0 HIDX-EFF-HI !
   0 HIDX-CTL-HI !
   0 HIDX-DFR-HI ! ;

\ HIDX-EFF-SYNC ( -- ) : flush the cache when USIGS rewound below a cached
\ dependency or the store was swapped (grow, reset, external restore).
: HIDX-EFF-SYNC
   UEND @ HIDX-EFF-HI @ <
   USIGS HIDX-EFF-BASE@ <> or IF
      HIDX-EPOCH+
      USIGS HIDX-EFF-BASE!
   THEN ;

: HIDX-MMAP-RC ( -- n )
   0 SYM-CAP HIDX-TABLES * cells USIGS-PROT-RW USIGS-MAP-ANON
   USIGS-ANON-FD USIGS-OFF-ZERO mmap
   dup 0 < IF s" checker: symbol index mmap failed" 76 die THEN ;

TRUSTED: HIDX-RC>PTR ( n -- ptr n ) ;

: HIDX-ALLOC-PTR ( -- ptr n )
   HIDX-MMAP-RC HIDX-RC>PTR ;

: HIDX-ALLOC ( -- )
   HIDX-ALLOC-PTR HIDX-MEM! ;

: HIDX-BKT-CLEAR ( -- )
   0 begin dup SYM-CAP < while
      0 over HT-BKT HIDX-CELL !
      1 +
   repeat drop ;

: HIDX-BUILD ( -- )
   HIDX-MEM-READY? 0= IF HIDX-ALLOC THEN
   HIDX-BKT-CLEAR
   1 HIDX-I !
   begin HIDX-I @ SYM-N @ < while
      HIDX-I @ HIDX-SYM+
      HIDX-I @ 1 + HIDX-I !
   repeat
   HIDX-EPOCH+
   -1 HIDX-VALID ! ;

: HIDX-ENSURE ( -- )
   HIDX-VALID @ 0= IF HIDX-BUILD THEN ;

\ HIDX-RESET ( -- ) : snapshot prepare — the mapping is process-local.
: HIDX-RESET ( -- )
   HIDX-MEM-CLEAR
   0 HIDX-VALID !
   0 HIDX-EFF-HI !
   HIDX-EFF-BASE-CLEAR
   0 HIDX-CTL-HI !
   0 HIDX-DFR-HI ! ;

: HIDX@ ( n n n -- n bool ) {: id:n vt:n et:n :}
   id et HIDX-CELL @ HIDX-EPOCH @ = 0= IF 0 RES-FALSE EXIT THEN
   id vt HIDX-CELL @ RES-TRUE ;

: HIDX! ( n n n n -- ) {: v:n id:n vt:n et:n :}
   v id vt HIDX-CELL !
   HIDX-EPOCH @ id et HIDX-CELL ! ;

\ HIDX-B@/HIDX-B! store a boolean payload (the defer-active flag) so the cached
\ value keeps its bool role instead of a bare cell.
: HIDX-B@ ( n n n -- bool bool ) {: id:n vt:n et:n :}
   id et HIDX-CELL @ HIDX-EPOCH @ = 0= IF RES-FALSE RES-FALSE EXIT THEN
   id vt HIDX-CELL @ 0 <> RES-TRUE ;

: HIDX-B! ( bool n n n -- ) {: v:bool id:n vt:n et:n :}
   v id vt HIDX-CELL !
   HIDX-EPOCH @ id et HIDX-CELL ! ;

: HIDX-EFF@ ( n -- n bool ) HT-EFF-V HT-EFF-E HIDX@ ;
: HIDX-EFF! ( n n -- ) HT-EFF-V HT-EFF-E HIDX! ;
: HIDX-CTL@ ( n -- n bool ) HT-CTL-V HT-CTL-E HIDX@ ;
: HIDX-CTL! ( n n -- ) HT-CTL-V HT-CTL-E HIDX! ;
: HIDX-DFR@ ( n -- bool bool ) HT-DFR-V HT-DFR-E HIDX-B@ ;
: HIDX-DFR! ( bool n -- ) HT-DFR-V HT-DFR-E HIDX-B! ;
: HIDX-PRM@ ( n -- n bool ) HT-PRM-V HT-PRM-E HIDX@ ;
: HIDX-PRM! ( n n -- ) HT-PRM-V HT-PRM-E HIDX! ;

: HIDX-EFF-DEP+ ( n -- )
   HIDX-EFF-HI @ max HIDX-EFF-HI ! ;

: HIDX-CTL-DEP+ ( n -- )
   HIDX-CTL-HI @ max HIDX-CTL-HI ! ;

: HIDX-DFR-DEP+ ( n -- )
   HIDX-DFR-HI @ max HIDX-DFR-HI ! ;

\ typed-local-lint: allow-bare-local - pkg/name preserve ptr u8 roles.
: SYM-FIND ( ptr u8 n n ptr u8 n -- n bool ) {: pkg pkgu:n vis:n name nameu:n :}
   HIDX-ENSURE
   pkg pkgu vis name nameu HIDX-HASH HIDX-BKT @ HIDX-CUR !
   begin HIDX-CUR @ 0 <> while
      pkg pkgu vis name nameu HIDX-CUR @ SYM-MATCH? IF HIDX-CUR @ RES-TRUE EXIT THEN
      HIDX-CUR @ HT-NEXT HIDX-CELL @ HIDX-CUR !
   repeat
   0 RES-FALSE ;

: SYM-PKG! ( ptr u8 n n -- ) {: a:ptr u:n id:n :}
   a u SYM-COPY-FOLD {: dst:ptr len:n :}
   dst id SYM-PKG-A-FIELD !
   len id SYM-ROW SYM.PKG-U ! ;

: SYM-NAME! ( ptr u8 n n -- ) {: a:ptr u:n id:n :}
   a u SYM-COPY-FOLD {: dst:ptr len:n :}
   dst id SYM-NAME-A-FIELD !
   len id SYM-ROW SYM.NAME-U ! ;

\ typed-local-lint: allow-bare-local - pkg/name preserve ptr u8 roles.
: SYM-SET ( ptr u8 n n ptr u8 n n -- ) {: pkg pkgu:n vis:n name nameu:n id:n :}
   pkg pkgu id SYM-PKG!
   name nameu id SYM-NAME!
   vis id SYM-ROW SYM.VIS ! ;

: SYM-CAP-NEXT ( n -- n ) {: need:n :}   \ smallest pow2 >= need, growing from cap
   SYM-CAP-V @ BEGIN dup need < WHILE 2 * REPEAT ;

\ SYM-GROW ( need -- ) : double (pow2) the record array to hold id `need` and drop
\ the HIDX mapping so the next lookup rebuilds — rehashes — at the new cap/mask.
: SYM-GROW ( n -- ) {: need:n :}
   need SYM-CAP-NEXT {: nc:n :}
   SYMS-P @ SYM-CAP-V @ SYM-REC * nc SYM-REC * ARENA-BYTES-GROW SYMS-P !
   nc SYM-CAP-V !
   HIDX-MEM-CLEAR   0 HIDX-VALID !
   HIDX-EPOCH+ ;

: SYM-ENSURE ( -- )             \ ensure room for the next id (SYM-N)
   SYM-N @ SYM-CAP-V @ < IF exit THEN
   SYM-N @ 1 + SYM-GROW ;

\ typed-local-lint: allow-bare-local - pkg/name preserve ptr u8 roles.
: SYM-INTERN ( ptr u8 n n ptr u8 n -- n ) {: pkg pkgu:n vis:n name nameu:n :}
   pkg pkgu vis name nameu SYM-FIND IF EXIT THEN drop
   SYM-ENSURE
   pkgu nameu + SYM-STR-ENSURE   \ reserve the whole record's strings up front so
                                  \ no mid-SYM-SET grow relocates and dangles PKG-A
   SYM-N @ SYM-ID !
   pkg pkgu vis name nameu SYM-ID @ SYM-SET
   SYM-ID @ 1 + SYM-N !
   HIDX-VALID @ IF SYM-ID @ HIDX-SYM+ THEN
   SYM-ID @ ;

\ checker-registry.f - typed checker effect store.
\
\ Loaded from checker.f after the signature parser and before callers need
\ certified word lookup. Source strings are parsed once at boundary adapters;
\ callers instantiate the stored effect graph.

0 constant EFF-DELETED
1 constant EFF-ACTIVE

0 constant EN-CON
1 constant EN-VAR
2 constant EN-ROW
3 constant EN-PTR
4 constant EN-PUSH
5 constant EN-QUOT
6 constant EN-ATOM
7 constant EN-PARAM

0 constant ER-NEXT-CELL
1 constant ER-ACTIVE-CELL
2 constant ER-DIN-CELL
3 constant ER-DOUT-CELL
4 constant ER-RIN-CELL
5 constant ER-ROUT-CELL
6 constant ER-HASR-CELL
7 constant ER-TVN-CELL
8 constant ER-RVN-CELL
9 constant ER-SYM-CELL
10 constant ER-MINI-CELL
$0 constant ER-NEXT-OFF
$8 constant ER-ACTIVE-OFF
$10 constant ER-DIN-OFF
$18 constant ER-DOUT-OFF
$20 constant ER-RIN-OFF
$28 constant ER-ROUT-OFF
$30 constant ER-HASR-OFF
$38 constant ER-TVN-OFF
$40 constant ER-RVN-OFF
$48 constant ER-SYM-OFF
$50 constant ER-MINI-OFF
$58 constant EFF-REC
$8 constant EFF-REC-ALIGN
0 constant EFF-REC-PTR-MASK

: ER.NEXT ( ptr a -- ptr a ) ER-NEXT-OFF + ;
: ER.ACTIVE ( ptr a -- ptr a ) ER-ACTIVE-OFF + ;
: ER.DIN ( ptr a -- ptr a ) ER-DIN-OFF + ;
: ER.DOUT ( ptr a -- ptr a ) ER-DOUT-OFF + ;
: ER.RIN ( ptr a -- ptr a ) ER-RIN-OFF + ;
: ER.ROUT ( ptr a -- ptr a ) ER-ROUT-OFF + ;
: ER.HASR ( ptr a -- ptr a ) ER-HASR-OFF + ;
: ER.TVN ( ptr a -- ptr a ) ER-TVN-OFF + ;
: ER.RVN ( ptr a -- ptr a ) ER-RVN-OFF + ;
: ER.SYM ( ptr a -- ptr a ) ER-SYM-OFF + ;
: ER.MINI ( ptr a -- ptr a ) ER-MINI-OFF + ;

0 constant EN-TAG-CELL
1 constant EN-A-CELL
2 constant EN-B-CELL
3 constant EN-C-CELL
4 constant EN-D-CELL
5 constant EN-E-CELL
6 constant EN-F-CELL
7 constant EN-G-CELL
8 constant EN-H-CELL
$0 constant EN-TAG-OFF
$8 constant EN-A-OFF
$10 constant EN-B-OFF
$18 constant EN-C-OFF
$20 constant EN-D-OFF
$28 constant EN-E-OFF
$30 constant EN-F-OFF
$38 constant EN-G-OFF
$40 constant EN-H-OFF
$48 constant EFF-NODE
$8 constant EFF-NODE-ALIGN
0 constant EFF-NODE-PTR-MASK

: EN.TAG ( ptr a -- ptr a ) EN-TAG-OFF + ;
: EN.A ( ptr a -- ptr a ) EN-A-OFF + ;
: EN.B ( ptr a -- ptr a ) EN-B-OFF + ;
: EN.C ( ptr a -- ptr a ) EN-C-OFF + ;
: EN.D ( ptr a -- ptr a ) EN-D-OFF + ;
: EN.E ( ptr a -- ptr a ) EN-E-OFF + ;
: EN.F ( ptr a -- ptr a ) EN-F-OFF + ;
: EN.G ( ptr a -- ptr a ) EN-G-OFF + ;
: EN.H ( ptr a -- ptr a ) EN-H-OFF + ;

: ER-LAYOUT-OFFSETS-A ( -- )
   ER-NEXT-CELL cells ER-NEXT-OFF CHECKER-RECORD-LAYOUT=
   ER-ACTIVE-CELL cells ER-ACTIVE-OFF CHECKER-RECORD-LAYOUT=
   ER-DIN-CELL cells ER-DIN-OFF CHECKER-RECORD-LAYOUT=
   ER-DOUT-CELL cells ER-DOUT-OFF CHECKER-RECORD-LAYOUT=
   ER-RIN-CELL cells ER-RIN-OFF CHECKER-RECORD-LAYOUT= ;

: ER-LAYOUT-OFFSETS-B ( -- )
   ER-ROUT-CELL cells ER-ROUT-OFF CHECKER-RECORD-LAYOUT=
   ER-HASR-CELL cells ER-HASR-OFF CHECKER-RECORD-LAYOUT=
   ER-TVN-CELL cells ER-TVN-OFF CHECKER-RECORD-LAYOUT=
   ER-RVN-CELL cells ER-RVN-OFF CHECKER-RECORD-LAYOUT=
   ER-SYM-CELL cells ER-SYM-OFF CHECKER-RECORD-LAYOUT= ;

: ER-LAYOUT-TAIL ( -- )
   ER-MINI-CELL cells ER-MINI-OFF CHECKER-RECORD-LAYOUT=
   ER-MINI-OFF CELL + EFF-REC CHECKER-RECORD-LAYOUT=
   CELL EFF-REC-ALIGN CHECKER-RECORD-LAYOUT=
   EFF-REC EFF-REC-ALIGN mod 0 CHECKER-RECORD-LAYOUT=
   EFF-REC-PTR-MASK 0 CHECKER-RECORD-LAYOUT= ;

: ER-LAYOUT-ACCESSORS-A ( -- )
   here dup ER.NEXT swap ER-NEXT-OFF CHECKER-RECORD-FIELD=
   here dup ER.ACTIVE swap ER-ACTIVE-OFF CHECKER-RECORD-FIELD=
   here dup ER.DIN swap ER-DIN-OFF CHECKER-RECORD-FIELD=
   here dup ER.DOUT swap ER-DOUT-OFF CHECKER-RECORD-FIELD=
   here dup ER.RIN swap ER-RIN-OFF CHECKER-RECORD-FIELD=
   here dup ER.ROUT swap ER-ROUT-OFF CHECKER-RECORD-FIELD= ;

: ER-LAYOUT-ACCESSORS-B ( -- )
   here dup ER.HASR swap ER-HASR-OFF CHECKER-RECORD-FIELD=
   here dup ER.TVN swap ER-TVN-OFF CHECKER-RECORD-FIELD=
   here dup ER.RVN swap ER-RVN-OFF CHECKER-RECORD-FIELD=
   here dup ER.SYM swap ER-SYM-OFF CHECKER-RECORD-FIELD=
   here dup ER.MINI swap ER-MINI-OFF CHECKER-RECORD-FIELD= ;

: EN-LAYOUT-OFFSETS-A ( -- )
   EN-TAG-CELL cells EN-TAG-OFF CHECKER-RECORD-LAYOUT=
   EN-A-CELL cells EN-A-OFF CHECKER-RECORD-LAYOUT=
   EN-B-CELL cells EN-B-OFF CHECKER-RECORD-LAYOUT=
   EN-C-CELL cells EN-C-OFF CHECKER-RECORD-LAYOUT=
   EN-D-CELL cells EN-D-OFF CHECKER-RECORD-LAYOUT= ;

: EN-LAYOUT-OFFSETS-B ( -- )
   EN-E-CELL cells EN-E-OFF CHECKER-RECORD-LAYOUT=
   EN-F-CELL cells EN-F-OFF CHECKER-RECORD-LAYOUT=
   EN-G-CELL cells EN-G-OFF CHECKER-RECORD-LAYOUT=
   EN-H-CELL cells EN-H-OFF CHECKER-RECORD-LAYOUT=
   EN-H-OFF CELL + EFF-NODE CHECKER-RECORD-LAYOUT=
   CELL EFF-NODE-ALIGN CHECKER-RECORD-LAYOUT=
   EFF-NODE EFF-NODE-ALIGN mod 0 CHECKER-RECORD-LAYOUT=
   EFF-NODE-PTR-MASK 0 CHECKER-RECORD-LAYOUT= ;

: EN-LAYOUT-ACCESSORS-A ( -- )
   here dup EN.TAG swap EN-TAG-OFF CHECKER-RECORD-FIELD=
   here dup EN.A swap EN-A-OFF CHECKER-RECORD-FIELD=
   here dup EN.B swap EN-B-OFF CHECKER-RECORD-FIELD=
   here dup EN.C swap EN-C-OFF CHECKER-RECORD-FIELD=
   here dup EN.D swap EN-D-OFF CHECKER-RECORD-FIELD= ;

: EN-LAYOUT-ACCESSORS-B ( -- )
   here dup EN.E swap EN-E-OFF CHECKER-RECORD-FIELD=
   here dup EN.F swap EN-F-OFF CHECKER-RECORD-FIELD=
   here dup EN.G swap EN-G-OFF CHECKER-RECORD-FIELD=
   here dup EN.H swap EN-H-OFF CHECKER-RECORD-FIELD= ;

: EFF-LAYOUT-ASSERT ( -- )
   ER-LAYOUT-OFFSETS-A
   ER-LAYOUT-OFFSETS-B
   ER-LAYOUT-TAIL
   ER-LAYOUT-ACCESSORS-A
   ER-LAYOUT-ACCESSORS-B
   EN-LAYOUT-OFFSETS-A
   EN-LAYOUT-OFFSETS-B
   EN-LAYOUT-ACCESSORS-A
   EN-LAYOUT-ACCESSORS-B ;

EFF-LAYOUT-ASSERT

\ EC-TV/EC-RV/EI-TV/EI-RV are var-id maps in the growable TV arena (top).
variable EC-TVN
variable EC-RVN

64 constant EI-AK-CAP
create EI-AK EI-AK-CAP cells allot

variable FEP
variable FEP-OFF
variable CHECKER-REC-SYM
0 CHECKER-REC-SYM !

variable EC-TV-HW
variable EC-RV-HW

: E-MAP-CLEAR ( ptr a n -- ) {: p:ptr hw:n :}
   0 begin dup hw < while
      UNBOUND over cells p + !
      1 +
   repeat drop ;

\ one-time load init; E-COPY-MAPS-RESET clears only the high-water span
EC-TV MAXTV E-MAP-CLEAR   0 EC-TV-HW !
EC-RV MAXTV E-MAP-CLEAR   0 EC-RV-HW !

: E-COPY-MAPS-RESET ( -- )
   EC-TV EC-TV-HW @ E-MAP-CLEAR
   EC-RV EC-RV-HW @ E-MAP-CLEAR
   0 EC-TV-HW !
   0 EC-RV-HW !
   0 EC-TVN !
   0 EC-RVN ! ;

: E-I-AK-RESET ( -- )
   0 begin dup EI-AK-CAP < while
      UNBOUND over cells EI-AK + !
      1 +
   repeat drop ;

: E-TV-ID ( n -- n ) {: id:n :}
   id cells EC-TV + dup @ UNBOUND = if
      EC-TVN @ over !
      EC-TVN @ 1+ EC-TVN !
      id 1+ EC-TV-HW @ max EC-TV-HW !
   then @ ;

: E-RV-ID ( n -- n ) {: id:n :}
   id cells EC-RV + dup @ UNBOUND = if
      EC-RVN @ over !
      EC-RVN @ 1+ EC-RVN !
      id 1+ EC-RV-HW @ max EC-RV-HW !
   then @ ;

: E-OFF ( ptr a -- n )
   USIGS - ;

: E-PTR ( n -- ptr a )
   USIGS + ;

: E-ENSURE-NODE ( -- )
   UEND @ EFF-NODE + CELL + USIGS-ENSURE ;

\ typed-local-lint: allow-bare-local - p preserves ptr a field-owner role.
: E-NODE-INIT ( n ptr a -- ) {: tag:n p :}
   tag p EN.TAG !
   0 p EN.A !  0 p EN.B !  0 p EN.C !  0 p EN.D !
   0 p EN.E !  0 p EN.F !  0 p EN.G !  0 p EN.H ! ;

: E-NODE-NEW ( n -- ptr a ) {: tag:n :}
   E-ENSURE-NODE
   USIGS UEND @ + {: p:ptr :}
   tag p E-NODE-INIT
   UEND @ EFF-NODE + UEND !
   p ;

: E-NODE-OFF ( n -- n )
   E-NODE-NEW E-OFF ;

\ E-ARGS-RESERVE ( n -- n ) : reserve a contiguous argc-cell run in USIGS
\ for a persisted EN-PARAM node's arg offsets (uncapped arity). The run is a byte
\ offset (E-OFF-relative), stable across USIGS relocation. Children copied after
\ the reserve allocate past this run, so their offsets never overlap it.
: E-ARGS-RESERVE ( n -- n ) {: argc:n :}
   UEND @ argc cells + CELL + USIGS-ENSURE
   UEND @
   dup argc cells + UEND ! ;

: E-COPY-STR ( ptr u8 n ptr a -- ) {: a:ptr u:n p:ptr :}
   UEND @ p EN.A !
   u p EN.B !
   UEND @ u + UALIGN CELL + USIGS-ENSURE
   a u UBS
   UALIGN! ;

: E-RES ( n -- n ) {: x:n :}
   x TAG S-ROW = x TAG S-PUSH = or if x R-RES else x T-RES then ;

: E-COPY* ( n -- n ) {: x:n :}
   x 0= if 0 exit then
   x E-RES TAG case
      T-CON of
         EN-CON E-NODE-NEW E-OFF >r
         x E-RES PAY r@ E-PTR EN.A !
         r>
      endof
      T-VAR of
         EN-VAR E-NODE-NEW E-OFF >r
         x E-RES PAY E-TV-ID r@ E-PTR EN.A !
         x E-RES PAY TVK@ r@ E-PTR EN.B !   \ persist the var kind (TVK-ANY/TVK-RAW) for freshening + snapshot
         r>
      endof
      S-ROW of
         EN-ROW E-NODE-NEW E-OFF >r
         x E-RES PAY E-RV-ID r@ E-PTR EN.A !
         r>
      endof
      T-PTR of
         EN-PTR E-NODE-NEW E-OFF >r
         x E-RES PTR>INNER TWALK-DEEPER RECURSE TWALK-SHALLOWER r@ E-PTR EN.A !
         r>
      endof
      S-PUSH of
         EN-PUSH E-NODE-NEW E-OFF >r
         x E-RES P>TYPE TWALK-DEEPER RECURSE TWALK-SHALLOWER r@ E-PTR EN.A !
         x E-RES P>REST TWALK-DEEPER RECURSE TWALK-SHALLOWER r@ E-PTR EN.B !
         r>
      endof
      T-QUOT of
         EN-QUOT E-NODE-NEW E-OFF >r
         x E-RES Q>DIN TWALK-DEEPER RECURSE TWALK-SHALLOWER r@ E-PTR EN.A !
         x E-RES Q>DOUT TWALK-DEEPER RECURSE TWALK-SHALLOWER r@ E-PTR EN.B !
         x E-RES Q>RIN TWALK-DEEPER RECURSE TWALK-SHALLOWER r@ E-PTR EN.C !
         x E-RES Q>ROUT TWALK-DEEPER RECURSE TWALK-SHALLOWER r@ E-PTR EN.D !
         x E-RES Q>XHAS r@ E-PTR EN.E !
         x E-RES Q>XDEAD r@ E-PTR EN.F !
         x E-RES Q>XDOUT r@ E-PTR EN.G !
         x E-RES Q>XROUT r@ E-PTR EN.H !
         r>
      endof
      T-ATOM of
         EN-ATOM E-NODE-NEW E-OFF >r
         x E-RES ATOM>A x E-RES ATOM>U r@ E-PTR E-COPY-STR
         x E-RES ATOM>K r@ E-PTR EN.C !
         r>
      endof
      T-PARAM of
         EN-PARAM E-NODE-NEW E-OFF {: noff:n :}      \ node offset (stable across USIGS grow)
         x E-RES PARAM>NAME-A x E-RES PARAM>NAME-U noff E-PTR E-COPY-STR
         x E-RES PARAM>ARGC {: argc:n :}
         argc noff E-PTR EN.C !
         x E-RES PARAM>FAM noff E-PTR EN.H !         \ resolved family-id (identity)
         x E-RES PARAM>HID noff E-PTR EN.E !         \ hidden physical-field slot+1 (0 = logical; 0 in pre-3a snapshots)
         argc 0 > IF
            argc E-ARGS-RESERVE {: run:n :}          \ argc-cell run in USIGS
            run noff E-PTR EN.D !
            0 BEGIN dup argc < WHILE                 \ data-stack index (RECURSE-safe)
               x E-RES over PARAM>ARG TWALK-DEEPER RECURSE TWALK-SHALLOWER   \ ( i childoff )
               over cells run + E-PTR !                                     \ ( i )
               1 +
            REPEAT drop
         THEN
         noff
      endof
      0 swap
   endcase ;
: E-COPY ( n -- n ) TWALK-RESET E-COPY* ;

: USIG-NEXT ( ptr a -- ptr a )
   ER.NEXT @ E-PTR ;

: USIG-OFF ( ptr a -- n )
   E-OFF ;

\ FEP holds the found active effect record; FEP-OFF stores its USIGS offset+1 so
\ 0 means "no active record" even when a real record lives at offset 0.
: FEP-CLEAR ( -- )
   0 FEP-OFF ! ;

: FEP-SET ( ptr a -- )
   dup FEP !
   USIG-OFF 1 + FEP-OFF ! ;

: FEP-OFF@ ( -- n )
   FEP-OFF @ ;

: FEP-HIT? ( -- bool )
   FEP-OFF@ 0 <> ;

: USIG-END? ( ptr a -- bool )
   @ 0= ;

\ E-REC-START runs the effect-cache sync first: it is the single choke point
\ for USIGS appends, so a rewind (scope/candidate rollback, forget, reset)
\ flushes the cache BEFORE new records can reuse the truncated offsets — a
\ read-time-only check could be masked by rewind-then-regrow.
\ typed-local-lint: allow-bare-local - p preserves ptr a record-owner role.
: E-REC-INIT ( ptr a -- ) {: p :}
   0 p ER.NEXT !  0 p ER.ACTIVE !
   0 p ER.DIN !   0 p ER.DOUT !  0 p ER.RIN !  0 p ER.ROUT !
   0 p ER.HASR !  0 p ER.TVN !   0 p ER.RVN !  0 p ER.MINI !
   CHECKER-REC-SYM @ p ER.SYM ! ;

: E-REC-START ( -- ptr a )
   HIDX-EFF-SYNC
   UEND @ EFF-REC + CELL + USIGS-ENSURE
   USIGS UEND @ + {: p:ptr :}
   p E-REC-INIT
   p EFF-REC + USIGS - UEND !
   p ;

: E-REC-FINISH ( ptr a -- )
   UEND @ swap ER.NEXT !
   UTERM! ;

\ A hidden layout parameter already denotes one physical cell; charging its
\ family's full logical width again would turn a W-cell bundle into W^2 cells.
: ROW-TERM-CELLS ( n -- n ) {: t:n :}
   t T-RES dup HIDDEN-PARAM? if drop 1 exit then
   T-WIDTH ;

\ ROW-CELLS: total physical width of a row's fixed entries — the minimum stack
\ depth any call must provide for that row. Row-polymorphic tails match zero
\ cells, so the fixed prefix IS the floor (dot habu-habu-certified-words-84e84eaf).
: ROW-CELLS ( n -- n ) {: s:n :}
   0 s                                   \ ( acc cur )
   BEGIN R-RES dup TAG S-PUSH = WHILE
      dup P>TYPE ROW-TERM-CELLS rot + swap
      P>REST
   REPEAT drop ;

: EFFECT-MIN-IN ( n -- n )
   ROW-CELLS dup 255 > if s" checker: min-in exceeds record field" 76 die then ;

: E-BUILD-EFFECT ( n n n n bool -- n ) {: din:n dout:n rin:n rout:n hasr:bool :}
   din EFFECT-MIN-IN {: minin:n :}
   E-REC-START E-OFF >r
   E-COPY-MAPS-RESET
   EFF-ACTIVE r@ E-PTR ER.ACTIVE !
   din E-COPY r@ E-PTR ER.DIN !
   dout E-COPY r@ E-PTR ER.DOUT !
   hasr if
      rin E-COPY r@ E-PTR ER.RIN !
      rout E-COPY r@ E-PTR ER.ROUT !
   then
   hasr r@ E-PTR ER.HASR !
   EC-TVN @ r@ E-PTR ER.TVN !
   EC-RVN @ r@ E-PTR ER.RVN !
   minin r@ E-PTR ER.MINI !
   r@ E-PTR E-REC-FINISH
   r> ;

\ Interpret-gate wide scan (habu-tfam-12-interpret checker half): a word whose
\ recorded effect mentions ANY wider-than-cell layout value — producers and
\ consumers, on any of the four rows, including inside quotation sub-effects
\ (a quotation value can be `execute`d at top level) — must never run at the
\ untyped interpret level: its dict record is marked DNAME-WIDE so
\ EM-INTERPRET-FIND / interpret `'` fail closed before a bundle can land on
\ (or be expected from) the interpret stack. A `ptr` to a layout is one cell
\ and stays unmarked; pointers are deref'd only to reach nested quotations.
: ROW-WIDE? ( n -- bool ) {: s:n :}
   RES-FALSE s                           \ ( acc cur )
   BEGIN R-RES dup TAG S-PUSH = WHILE
     dup P>TYPE T-RES
     dup T-WIDTH 1 > IF
       drop nip RES-TRUE swap            \ ( true cur )
     ELSE
       BEGIN dup TAG T-PTR = WHILE PTR>INNER T-RES REPEAT
       dup TAG T-QUOT = IF
         dup Q>DIN RECURSE  swap         \ ( acc cur f1 qt )
         dup Q>DOUT RECURSE  swap        \ ( acc cur f1 f2 qt )
         dup Q>RIN RECURSE  swap         \ ( acc cur f1 f2 f3 qt )
         Q>ROUT RECURSE                  \ ( acc cur f1 f2 f3 f4 )
         or or or  rot or swap           \ ( acc' cur )
       ELSE drop THEN
     THEN
     P>REST
   REPEAT drop ;

\ RECW: latch holding the wide? verdict of the LAST effect record, stored by
\ VALUE at the single record choke point (E-ADD-EFFECT) so a stale value from
\ a candidate/rolled-back check is always overwritten by the current record
\ flow before any publish tail consumes it; CHECK-RESET zeroes it for hook
\ paths that certify without recording. REC-WIDE-PUBLISH is called by the
\ engine publish tails AFTER ndict++ (EM-COMPILE-PUBLISH-TRUSTED/-HOOKED and
\ C-DEFER, habu2.f), where the `wide-mark` prim's newest-published target
\ (ndict-1) is exactly the record whose effect was just recorded.
variable RECW   0 RECW !
: REC-WIDE-PUBLISH ( -- )
   RECW @ 0 <> IF wide-mark THEN
   0 RECW ! ;

\ RECMI: latch holding the din cell count of the LAST effect record — the
\ certified minimum input arity (dot habu-habu-certified-words-84e84eaf).
\ Same discipline as RECW above: stored BY VALUE at the single record choke
\ point (E-ADD-EFFECT), zeroed on deleted/bad rows and by CHECK-RESET, and
\ consumed by the engine publish tails after ndict++ (habu2.f
\ EM-REC-WIDE-PUBLISH reads it through REC-MIN-IN@ and pokes DNAME-MIN-IN
\ bits 52-59 of the just-published record) so EM-INTERPRET-FIND can fail
\ closed on an underdepth bare call before the body runs.
variable RECMI   0 RECMI !
: REC-MIN-IN@ ( -- n )
   RECMI @  0 RECMI ! ;

\ E-ADD-EFFECT/E-ADD-DELETED are the only creators of USER records (prims go
\ through PE-CLOSE/E-BUILD-EFFECT directly), so they own the in-place cache
\ update: the record just built IS the current effect for its symbol. The
\ cache stores offset+1 because offset 0 is legal after USIGS-RESET.
: E-ADD-EFFECT ( n n n n bool -- ) {: din:n dout:n rin:n rout:n hasr:bool :}
   din EFFECT-MIN-IN drop
   din ROW-WIDE?  dout ROW-WIDE? or
   hasr IF rin ROW-WIDE? or  rout ROW-WIDE? or THEN
   RECW !
   din dout rin rout hasr E-BUILD-EFFECT {: off:n :}
   off E-PTR ER.MINI @ RECMI !
   CHECKER-REC-SYM @ 0 <> HIDX-VALID @ and IF
      off 1 + CHECKER-REC-SYM @ HIDX-EFF!
      UEND @ HIDX-EFF-DEP+
   THEN
   CHECKER-REC-SYM @ 0 <>
   din dout PTX-BARRIER-ROWS? and IF
      CHECKER-REC-SYM @ PTX-BARRIER-SET-XT   \ M5: flag the block collective (default no-op before armed)
   THEN ;

: E-ADD-DELETED ( -- )
   0 RECW !
   0 RECMI !
   E-REC-START E-OFF >r
   EFF-DELETED r@ E-PTR ER.ACTIVE !
   r> E-PTR E-REC-FINISH
   CHECKER-REC-SYM @ 0 <> HIDX-VALID @ and IF
      0 CHECKER-REC-SYM @ HIDX-EFF!
      UEND @ HIDX-EFF-DEP+
   THEN ;

\ --- stored-signature intake. USIG-ADD parses a declared/trusted signature
\ into an effect row. A signature that does not parse is a hard stop on the
\ ordinary load path (a baked or TRUSTed effect must never be silently wrong).
\ In a MULTI-ERROR load it must not abort the run and must not store a row: a
\ rejected DEFINITION was already diagnosed and counted by CHECK (the native
\ re-records every published definition's declared sig through TRUST), so its
\ own name is suppressed here; a foreign name — a raw TRUST row — counts as a
\ reject and reports through BADSIG-XT (render.f). Either way no row exists,
\ so later callers reject as undefined instead of trusting a malformed effect.
variable NMA  variable NMU              \ current definition name (set by DO-TOK1)
variable MULTI-ERR      \ multi-error load mode active?
variable MULTI-ERR-N    \ rejected definitions recorded this load
0 MULTI-ERR !   0 MULTI-ERR-N !
\ bad stored-signature diagnostic hook (render.f installs BADSIG-DIAG). A `defer`
\ with a no-op DEFAULT so the diagnostic call is statically effect-known; the
\ default drops the sig+name spans, exactly the old 0-hook fallback.
defer BADSIG-XT ( ptr u8 n ptr u8 n -- )
: BADSIG-DEFAULT ( -- ) [: 2drop 2drop ;] is BADSIG-XT ;
BADSIG-DEFAULT

: MULTI-ERR? ( -- bool ) MULTI-ERR @ 0 <> ;

: USIG-BAD-FOREIGN? ( ptr u8 n -- bool )   \ not the definition CHECK just handled
   NMA @ NMU @ CORE-STR= 0= ;

: USIG-ADD-BAD ( ptr u8 n ptr u8 n -- ) {: sa:ptr su:n na:ptr nu:n :}
   0 RECW !                              \ no record stored: nothing to publish wide
   0 RECMI !                             \ ... and no min-in to poke
   MULTI-ERR? 0= IF
      2 sa su write drop                 \ name the offending stored sig text
      s" : checker: bad stored signature" 76 die
   THEN
   na nu USIG-BAD-FOREIGN? 0= IF EXIT THEN
   1 MULTI-ERR-N +!
   sa su na nu BADSIG-XT ;

: USIG-ADD ( ptr u8 n ptr u8 n -- ) {: sa:ptr su:n na:ptr nu:n :}
   NEW
   SGBAD-CLEAR
   sa su PARSE-SIG-RAW
   SGBAD @ if 2drop 2drop sa su na nu USIG-ADD-BAD exit then
   SGHASR @ E-ADD-EFFECT ;

: USIG-DELETE ( ptr u8 n -- )
   2drop E-ADD-DELETED ;

: USIG-SYM@ ( ptr a -- n )
   ER.SYM @ ;

: USIG-MATCH-SYM? ( ptr a n -- bool ) {: rec:ptr sym:n :}
   rec USIG-SYM@ sym = ;

: USIG-FIND-OFF-SYM ( n -- n bool ) {: sym:n :}
   sym 0= if 0 RES-FALSE exit then
   USIGS-USER FP !
   begin FP @ USIG-END? 0= while
      FP @ sym USIG-MATCH-SYM? if FP @ USIG-OFF RES-TRUE exit then
      FP @ USIG-NEXT FP !
   repeat
   0 RES-FALSE ;

variable FMEND

\ SCAN-USIGS-SYM ( n -- ) : FEP = last ACTIVE record for sym (0 if none or
\ deleted); FMEND = end offset of the last matching record of ANY state — the
\ cache dependency: a rewind below it can change the answer.
: SCAN-USIGS-SYM {: sym:n :}
   FEP-CLEAR
   0 FMEND !
   USIGS-USER FP !
   begin FP @ USIG-END? 0= while
      FP @ sym USIG-MATCH-SYM? if
         FP @ ER.NEXT @ FMEND !
         FP @ dup ER.ACTIVE @ if FEP-SET else drop FEP-CLEAR then
      then
      FP @ USIG-NEXT FP !
   repeat ;

: E-INST-RESET ( ptr a -- ) {: h:ptr :}
   E-I-AK-RESET
   0 begin dup h ER.TVN @ < while
      UNBOUND over cells EI-TV + !
      1 +
   repeat drop
   0 begin dup h ER.RVN @ < while
      UNBOUND over cells EI-RV + !
      1 +
   repeat drop ;

\ FRESH may grow (relocate) the EI arena, so re-fetch the slot address after
\ it: a base cached across FRESH would store into the freed buffer.
: E-I-TV ( n -- n ) {: id:n :}
   id cells EI-TV + @ UNBOUND = if
      FRESH MK-VAR id cells EI-TV + !
   then id cells EI-TV + @ ;

: E-I-RV ( n -- n ) {: id:n :}
   id cells EI-RV + @ UNBOUND = if
      FRESH MK-ROW id cells EI-RV + !
   then id cells EI-RV + @ ;

: E-I-AK-IDX ( n -- n )
   negate 1 - ;

: E-I-AK ( ptr u8 n n -- ptr u8 n n ) {: a:ptr u:n k:n :}   \ name kept for domain routing
   k 0 >= if a u k exit then
   k E-I-AK-IDX dup EI-AK-CAP >= if s" checker: fresh atom inst table full" 76 die then
   cells EI-AK + dup @ UNBOUND = if a u RIGID-AK-MINT over ! then @
   {: id:n :}
   a u id ;

: E-I-STR ( ptr a -- ptr u8 n )
   dup EN.A @ E-PTR swap EN.B @ ;

: E-INST ( n -- n ) {: off:n :}
   off 0= if 0 exit then
   off E-PTR >r
   r@ EN.TAG @ case
      EN-CON of r@ EN.A @ MK-CON r> drop endof
      EN-VAR of
         r@ EN.A @ E-I-TV                                  \ fresh var term
         r@ EN.B @ TVK-RAW = IF dup PAY TVK-RAW! THEN       \ restore persisted RAW kind on the fresh var
         r> drop
      endof
      EN-ROW of r@ EN.A @ E-I-RV r> drop endof
      EN-PTR of r@ EN.A @ RECURSE MK-PTR r> drop endof
      EN-PUSH of r@ EN.A @ RECURSE r@ EN.B @ RECURSE MK-PUSH r> drop endof
      EN-QUOT of
         r@ EN.A @ RECURSE
         r@ EN.B @ RECURSE
         r@ EN.C @ RECURSE
         r@ EN.D @ RECURSE
         MK-QUOT
         dup r@ EN.E @ r@ EN.F @ r@ EN.G @ r@ EN.H @ QX!
         r> drop
      endof
      EN-ATOM of r@ E-I-STR r@ EN.C @ E-I-AK MK-ATOM-K r> drop endof
      EN-PARAM of
         r@ {: np:ptr :}                           \ node ptr (parked value)
         np EN.C @ {: argc:n :}
         np EN.D @ {: run:n :}                      \ arg-run offset in USIGS
         PARAM-SCR-N @                              \ reentrant scratch mark (base) on the data stack
         0 BEGIN dup argc < WHILE                   \ data-stack index (RECURSE-safe)
            dup cells run + E-PTR @ RECURSE PARAM-SCR+
            1 +
         REPEAT drop
         np E-I-STR np EN.H @ MK-PARAM              \ ( base a u fam -- t )
         dup np EN.E @ swap PAY cells PARAMHID + !  \ restore hidden slot+1 (0 in pre-3a snapshots)
         r> drop
      endof
      r> drop 0 swap
   endcase ;

\ --- linear kind: polarity-aware multiplicity of an applied effect ------------
\ EN-MULT tallies occurrences of canonical var LMV in the stored effect subgraph
\ at offset `off`, split by polarity into LMNEG (input side) / LMPOS (output
\ side). A quotation ARGUMENT's rows flip polarity — the word must SUPPLY the
\ quotation's inputs (output side) and RECEIVES its outputs (input side) — so
\ passing a linear into a consumer quotation counts as one output-side use, and
\ KEEP (which also returns it) exceeds one. Mirrors E-INST's node walk.
variable LMNEG  variable LMPOS  variable LMV
: EN-MULT ( n bool -- ) {: off:n pol:bool :}
   off 0= IF exit THEN
   off E-PTR >r
   r@ EN.TAG @ case
      EN-VAR of
         r@ EN.A @ LMV @ = IF
            pol IF LMPOS @ 1 + LMPOS ! ELSE LMNEG @ 1 + LMNEG ! THEN
         THEN
         r> drop
      endof
      EN-PUSH of
         r@ EN.A @ pol RECURSE
         r@ EN.B @ pol RECURSE
         r> drop
      endof
      EN-PTR of
         r@ EN.A @ pol RECURSE
         r> drop
      endof
      EN-QUOT of
         r@ EN.A @ pol 0= RECURSE          \ Din: flipped polarity
         r@ EN.B @ pol RECURSE             \ Dout: kept
         r@ EN.C @ pol 0= RECURSE          \ Rin: flipped
         r@ EN.D @ pol RECURSE             \ Rout: kept
         r> drop
      endof
      EN-PARAM of
         r@ {: np:ptr :}                   \ node ptr (parked value)
         0 BEGIN dup np EN.C @ < WHILE      \ data-stack index (RECURSE-safe)
            dup cells np EN.D @ + E-PTR @ pol RECURSE
            1 +
         REPEAT drop
         r> drop
      endof
      r> drop
   endcase ;

: LIN-VAR-MULT ( ptr a n -- n n ) {: h:ptr v:n :}
   v LMV !  0 LMNEG !  0 LMPOS !
   h ER.DIN @ RES-FALSE EN-MULT
   h ER.DOUT @ RES-TRUE EN-MULT
   h ER.HASR @ 0 <> IF
      h ER.RIN @ RES-FALSE EN-MULT
      h ER.ROUT @ RES-TRUE EN-MULT
   THEN
   LMNEG @ LMPOS @ ;

\ After an effect is applied and its input vars are bound, examine each canonical
\ effect var: if it resolved to a linear con with unequal input/output
\ multiplicity, the linear was copied/dropped/laundered by this effect -> reject
\ (a). If it is still an unbound var but this effect used it non-linearly (copy
\ or drop), taint it for the deferred scan (b).
variable LMI
: LIN-EFF-PASS ( h -- ) {: h:ptr :}
   LIN-ANY? 0= IF exit THEN
   OK @ 0= IF exit THEN
   0 LMI !
   BEGIN LMI @ h ER.TVN @ < WHILE
      LMI @ cells EI-TV + @ {: r:n :}
      r UNBOUND <> IF
         r T-RES {: rr:n :}
         rr LIN-CON? IF
            h LMI @ LIN-VAR-MULT <> IF 0 OK ! THEN
         ELSE
            rr TAG T-VAR = IF
               h LMI @ LIN-VAR-MULT <> IF rr PAY LIN-TAINT THEN
            THEN
         THEN
      THEN
      LMI @ 1 + LMI !
   REPEAT ;

: EFF-APPLY ( ptr a -- ) {: h:ptr :}
   h E-INST-RESET
   h ER.DIN @ E-INST
   h ER.DOUT @ E-INST
   CHECKER-STEP
   h ER.HASR @ 0 <> if
      RCUR @ h ER.RIN @ E-INST UNIFY-IN OK @ and OK !
      h ER.ROUT @ E-INST RCUR !
   then
   h LIN-EFF-PASS ;

: EFF-QUOT ( ptr a -- n ) {: h:ptr :}
   h E-INST-RESET
   h ER.HASR @ 0 <> if
      h ER.DIN @ E-INST
      h ER.DOUT @ E-INST
      h ER.RIN @ E-INST
      h ER.ROUT @ E-INST
   else
      h ER.DIN @ E-INST
      h ER.DOUT @ E-INST
      FRESH MK-ROW dup
   then
   MK-QUOT ;

$200 constant PE-CAP
1 constant PE-ACTIVE
2 constant PE-TRUSTED-ONLY       \ prim is a trust boundary: rejected from CHECKED code, TRUSTED: only

0 constant PE-SYM-CELL
1 constant PE-EFF-CELL
2 constant PE-FLAGS-CELL
$0 constant PE-SYM-OFF
$8 constant PE-EFF-OFF
$10 constant PE-FLAGS-OFF
$18 constant PE-REC
$8 constant PE-REC-ALIGN
0 constant PE-REC-PTR-MASK

: PE.SYM ( ptr a -- ptr a ) PE-SYM-OFF + ;
: PE.EFF ( ptr a -- ptr a ) PE-EFF-OFF + ;
: PE.FLAGS ( ptr a -- ptr a ) PE-FLAGS-OFF + ;

: PE-LAYOUT-ASSERT ( -- )
   PE-SYM-CELL cells PE-SYM-OFF CHECKER-RECORD-LAYOUT=
   PE-EFF-CELL cells PE-EFF-OFF CHECKER-RECORD-LAYOUT=
   PE-FLAGS-CELL cells PE-FLAGS-OFF CHECKER-RECORD-LAYOUT=
   PE-FLAGS-OFF CELL + PE-REC CHECKER-RECORD-LAYOUT=
   CELL PE-REC-ALIGN CHECKER-RECORD-LAYOUT=
   PE-REC PE-REC-ALIGN mod 0 CHECKER-RECORD-LAYOUT=
   PE-REC-PTR-MASK 0 CHECKER-RECORD-LAYOUT=
   here dup PE.SYM swap PE-SYM-OFF CHECKER-RECORD-FIELD=
   here dup PE.EFF swap PE-EFF-OFF CHECKER-RECORD-FIELD=
   here dup PE.FLAGS swap PE-FLAGS-OFF CHECKER-RECORD-FIELD= ;

PE-LAYOUT-ASSERT

create PES PE-CAP PE-REC * allot
variable #PE
variable PE-I

: PE-ROW ( n -- ptr a )
   PE-REC * PES + ;

: PE-SYM@ ( n -- n )
   PE-ROW PE.SYM @ ;

: PE-EFF@ ( n -- n )
   PE-ROW PE.EFF @ ;

: PE-FLAGS@ ( n -- n )
   PE-ROW PE.FLAGS @ ;

: PE-ACTIVE? ( n -- bool )
   PE-FLAGS@ PE-ACTIVE and 0 <> ;

: PE-TRUSTED-ONLY? ( n -- bool )
   PE-FLAGS@ PE-TRUSTED-ONLY and 0 <> ;

: PRIM-CHECK-CAP ( -- )
   #PE @ PE-CAP >= IF s" checker: prim table full" 76 die THEN ;

: PRIM-ADD ( n n n -- ) {: sym:n eff:n flags:n :}
   PRIM-CHECK-CAP
   sym #PE @ PE-ROW PE.SYM !
   eff #PE @ PE-ROW PE.EFF !
   flags #PE @ PE-ROW PE.FLAGS !
   #PE @ 1 + #PE ! ;

\ mark the just-registered prim (last PES slot) as a TRUSTED-only trust boundary,
\ so a CHECKED caller is rejected while a TRUSTED: body (never checked) may use it.
: PRIM-TRUSTED-ONLY! ( -- )
   #PE @ 1 - PE-ROW PE.FLAGS dup @ PE-TRUSTED-ONLY or swap ! ;

variable PRM-FIRST

: PRIM-FIRST-SCAN ( n -- n ) {: sym:n :}
   0 PRM-FIRST !
   0 PE-I !
   begin PE-I @ #PE @ <  PRM-FIRST @ 0 =  and while
      PE-I @ PE-ACTIVE? IF
         PE-I @ PE-SYM@ sym = IF PE-I @ 1 + PRM-FIRST ! THEN
      THEN
      PE-I @ 1 + PE-I !
   repeat
   PRM-FIRST @ ;

\ PRIM-FIRST-IDX ( n -- n ) : first PES slot for sym + 1, 0 = none. The prim
\ table is immutable after load, so the cached slot needs no arena watermark.
: PRIM-FIRST-IDX {: sym:n :}
   sym 0= IF 0 EXIT THEN
   HIDX-ENSURE
   sym HIDX-PRM@ IF EXIT THEN
   drop
   sym PRIM-FIRST-SCAN
   dup sym HIDX-PRM! ;

: PRIM-FIRST-SYM ( n -- n ) {: sym:n :}
   sym PRIM-FIRST-IDX dup 0 = IF EXIT THEN
   1 - PE-EFF@ ;

: PE-SYM-OF ( ptr u8 n -- n ) {: a:ptr u:n :}
   s" " SYM-GLOBAL a u SYM-INTERN ;

variable PE-NA
variable PE-NU
PTR-VARIABLE PE-PKG-A
variable PE-PKG-U
variable PE-BASE
variable PE-DIN
variable PE-DOUT
variable PE-RIN
variable PE-ROUT
variable PE-HASR
variable PE-SYM-ID
variable PE-EFF-ID

: PE-NA@ ( -- ptr u8 )
   PE-NA 0 ptr-field @ ;

: PE-NA! ( ptr u8 -- )
   PE-NA 0 ptr-field ! ;

: PE-OPEN ( ptr u8 n -- ) {: a:ptr u:n :}
   a PE-NA!  u PE-NU !
   NEW
   NMAP-RESET
   ROWMAP-RESET
   SGBAD-CLEAR
   FRESH MK-ROW dup PE-BASE ! dup PE-DIN ! PE-DOUT !
   0 PE-RIN !  0 PE-ROUT !  0 PE-HASR ! ;

: PRIM: ( -- )
   parse-name PE-OPEN ;

: PPRIM: ( -- )
   parse-name PE-PKG-U ! PE-PKG-A !
   parse-name PE-OPEN ;

: PE-CLOSE-SYM ( n -- )
   PE-DIN @ EFFECT-MIN-IN drop
   dup PE-SYM-ID ! CHECKER-REC-SYM !
   PE-DIN @ PE-DOUT @ PE-RIN @ PE-ROUT @ PE-HASR @
   E-BUILD-EFFECT PE-EFF-ID !
   PE-SYM-ID @ PE-EFF-ID @ PE-ACTIVE PRIM-ADD ;

: PE-CLOSE ( -- )
   PE-NA@ PE-NU @ PE-SYM-OF PE-CLOSE-SYM ;

: PRIM; ( -- )
   PE-CLOSE ;

: PPRIM; ( -- )
   PE-PKG-A @ PE-PKG-U @ SYM-PUBLIC PE-NA@ PE-NU @ SYM-INTERN
   PE-CLOSE-SYM ;

: PE-IN ( n -- )
   PE-DIN @ MK-PUSH PE-DIN ! ;

: PE-OUT ( n -- )
   PE-DOUT @ MK-PUSH PE-DOUT ! ;

: PE-A ( -- n ) $61 VAR-OF ;
: PE-B ( -- n ) $62 VAR-OF ;
: PE-C ( -- n ) $63 VAR-OF ;
: PE-D ( -- n ) $64 VAR-OF ;
: PE-E ( -- n ) $65 VAR-OF ;
: PE-N ( -- n ) CC-N MK-CON ;
: PE-F ( -- n ) CC-BOOL MK-CON ;
: PE-R ( -- n ) CC-R MK-CON ;
: PE-U8 ( -- n ) CC-U8 MK-CON ;
: PE-PTR ( n -- n ) MK-PTR ;
: PE-PTR-A ( -- n ) PE-A PE-PTR ;
: PE-PTR-B ( -- n ) PE-B PE-PTR ;
: PE-PTR-C ( -- n ) PE-C PE-PTR ;
: PE-PTR-D ( -- n ) PE-D PE-PTR ;
: PE-PTR-E ( -- n ) PE-E PE-PTR ;
: PE-PTR-N ( -- n ) PE-N PE-PTR ;
: PE-PTR-U8 ( -- n ) PE-U8 PE-PTR ;
: PE-PTR-PTR-B ( -- n ) PE-B PE-PTR PE-PTR ;
\ RAW-kinded prototype var for raw storage-definer prims (here/data-base): the
\ minted pointee is TVK-RAW, so a fetch through it yields a RAW value that cannot
\ launder into a nominal atom/family. E-COPY reads the kind at PE-CLOSE and bakes
\ it onto the stored effect (EN.B), and E-INST re-freshens it per application.
: PE-A-RAW ( -- n ) PE-A dup PAY TVK-RAW! ;
: PE-PTR-A-RAW ( -- n ) PE-A-RAW PE-PTR ;

: PTABLE-START ( -- )
   0 #PE !
   0 UEND !
   UTERM! ;

: PTABLE-END ( -- )
   UEND @ USIGS-USER-OFF !
   UTERM! ;

PTABLE-START

PRIM: dup   PE-A PE-IN  PE-A PE-OUT PE-A PE-OUT PRIM;
PRIM: drop  PE-A PE-IN PRIM;
PRIM: swap  PE-A PE-IN PE-B PE-IN  PE-B PE-OUT PE-A PE-OUT PRIM;
PRIM: over  PE-A PE-IN PE-B PE-IN  PE-A PE-OUT PE-B PE-OUT PE-A PE-OUT PRIM;
PRIM: nip   PE-A PE-IN PE-B PE-IN  PE-B PE-OUT PRIM;
PRIM: tuck  PE-A PE-IN PE-B PE-IN  PE-B PE-OUT PE-A PE-OUT PE-B PE-OUT PRIM;
PRIM: rot   PE-A PE-IN PE-B PE-IN PE-C PE-IN  PE-B PE-OUT PE-C PE-OUT PE-A PE-OUT PRIM;
PRIM: -rot  PE-A PE-IN PE-B PE-IN PE-C PE-IN  PE-C PE-OUT PE-A PE-OUT PE-B PE-OUT PRIM;
PRIM: 2dup  PE-A PE-IN PE-B PE-IN  PE-A PE-OUT PE-B PE-OUT PE-A PE-OUT PE-B PE-OUT PRIM;
PRIM: 2drop PE-A PE-IN PE-B PE-IN PRIM;
PRIM: 2swap PE-A PE-IN PE-B PE-IN PE-C PE-IN PE-D PE-IN
            PE-C PE-OUT PE-D PE-OUT PE-A PE-OUT PE-B PE-OUT PRIM;
PRIM: 2over PE-A PE-IN PE-B PE-IN PE-C PE-IN PE-D PE-IN
            PE-A PE-OUT PE-B PE-OUT PE-C PE-OUT PE-D PE-OUT PE-A PE-OUT PE-B PE-OUT PRIM;

PRIM: +      PE-N PE-IN PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM: +      PE-PTR-A PE-IN PE-N PE-IN  PE-PTR-A PE-OUT PRIM;
PRIM: +      PE-N PE-IN PE-PTR-A PE-IN  PE-PTR-A PE-OUT PRIM;
PRIM: -      PE-N PE-IN PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM: -      PE-PTR-A PE-IN PE-N PE-IN  PE-PTR-A PE-OUT PRIM;
PRIM: -      PE-PTR-A PE-IN PE-PTR-A PE-IN  PE-N PE-OUT PRIM;
PRIM: *      PE-N PE-IN PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM: and    PE-N PE-IN PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM: and    PE-F PE-IN PE-F PE-IN  PE-F PE-OUT PRIM;
PRIM: or     PE-N PE-IN PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM: or     PE-F PE-IN PE-F PE-IN  PE-F PE-OUT PRIM;
PRIM: xor    PE-N PE-IN PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM: xor    PE-F PE-IN PE-F PE-IN  PE-F PE-OUT PRIM;
PRIM: 1+     PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM: 1+     PE-PTR-A PE-IN  PE-PTR-A PE-OUT PRIM;
PRIM: 1-     PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM: 1-     PE-PTR-A PE-IN  PE-PTR-A PE-OUT PRIM;
PRIM: negate PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM: invert PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM: 0=     PE-A PE-IN  PE-F PE-OUT PRIM;
PRIM: 0<     PE-N PE-IN  PE-F PE-OUT PRIM;
PRIM: =      PE-N PE-IN PE-N PE-IN  PE-F PE-OUT PRIM;
PRIM: =      PE-PTR-A PE-IN PE-PTR-A PE-IN  PE-F PE-OUT PRIM;
PRIM: <      PE-N PE-IN PE-N PE-IN  PE-F PE-OUT PRIM;
PRIM: <      PE-PTR-A PE-IN PE-PTR-A PE-IN  PE-F PE-OUT PRIM;
PRIM: >      PE-N PE-IN PE-N PE-IN  PE-F PE-OUT PRIM;
PRIM: >      PE-PTR-A PE-IN PE-PTR-A PE-IN  PE-F PE-OUT PRIM;
PRIM: <>     PE-N PE-IN PE-N PE-IN  PE-F PE-OUT PRIM;
PRIM: <>     PE-PTR-A PE-IN PE-PTR-A PE-IN  PE-F PE-OUT PRIM;
PRIM: <=     PE-N PE-IN PE-N PE-IN  PE-F PE-OUT PRIM;
PRIM: <=     PE-PTR-A PE-IN PE-PTR-A PE-IN  PE-F PE-OUT PRIM;
PRIM: >=     PE-N PE-IN PE-N PE-IN  PE-F PE-OUT PRIM;
PRIM: >=     PE-PTR-A PE-IN PE-PTR-A PE-IN  PE-F PE-OUT PRIM;
PRIM: /      PE-N PE-IN PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM: mod    PE-N PE-IN PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM: /mod   PE-N PE-IN PE-N PE-IN  PE-N PE-OUT PE-N PE-OUT PRIM;
PRIM: abs    PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM: min    PE-N PE-IN PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM: max    PE-N PE-IN PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM: lshift PE-N PE-IN PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM: rshift PE-N PE-IN PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM: cells  PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM: cell+  PE-PTR-A PE-IN  PE-PTR-A PE-OUT PRIM;
PRIM: cell+  PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM: chars  PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM: char+  PE-PTR-A PE-IN  PE-PTR-A PE-OUT PRIM;
PRIM: char+  PE-N PE-IN  PE-N PE-OUT PRIM;

PRIM: @          PE-PTR-A PE-IN  PE-A PE-OUT PRIM;
PRIM: !          PE-A PE-IN PE-PTR-A PE-IN PRIM;
PRIM: ptr-field  PE-PTR-A PE-IN PE-N PE-IN  PE-PTR-PTR-B PE-OUT PRIM;
PRIM: +!         PE-N PE-IN PE-PTR-N PE-IN PRIM;
PRIM: c@         PE-PTR-U8 PE-IN  PE-U8 PE-OUT PRIM;
PRIM: c!         PE-U8 PE-IN PE-PTR-U8 PE-IN PRIM;
PRIM: atomic@    PE-PTR-A PE-IN  PE-A PE-OUT PRIM;
PRIM: atomic!    PE-A PE-IN PE-PTR-A PE-IN PRIM;
PRIM: atomic-add PE-N PE-IN PE-PTR-N PE-IN  PE-N PE-OUT PRIM;
PRIM: atomic-cas PE-A PE-IN PE-A PE-IN PE-PTR-A PE-IN  PE-A PE-OUT PRIM;
PRIM: fence      PRIM;
PRIM: run-in-stack PE-N PE-IN PE-PTR-U8 PE-IN PE-N PE-IN PRIM;
PRIM: count      PE-PTR-U8 PE-IN  PE-PTR-U8 PE-OUT PE-N PE-OUT PRIM;

PRIM: .            PE-N PE-IN PRIM;
PRIM: .s           PRIM;
PRIM: depth        PE-N PE-OUT PRIM;
PRIM: here         PE-PTR-A-RAW PE-OUT PRIM;
PRIM: allot        PE-N PE-IN PRIM;
PRIM: ,            PE-N PE-IN PRIM;
PRIM: c,           PE-N PE-IN PRIM;
PRIM: type         PE-PTR-U8 PE-IN PE-N PE-IN PRIM;
PRIM: script-argc  PE-N PE-OUT PRIM;
PRIM: script-argv$ PE-N PE-IN  PE-PTR-U8 PE-OUT PE-N PE-OUT PRIM;
PRIM: throw        PE-N PE-IN PRIM;
PRIM: die          PE-PTR-U8 PE-IN PE-N PE-IN PE-N PE-IN PRIM;

PRIM: open     PE-PTR-U8 PE-IN PE-N PE-IN PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM: read     PE-N PE-IN PE-PTR-U8 PE-IN PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM: ioctl    PE-N PE-IN PE-N PE-IN PE-PTR-A PE-IN  PE-N PE-OUT PRIM;
PRIM: mmap     PE-N PE-IN PE-N PE-IN PE-N PE-IN PE-N PE-IN PE-N PE-IN PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM: path0    PE-PTR-U8 PE-IN PE-N PE-IN  PE-PTR-U8 PE-OUT PRIM;
PRIM: open-rd  PE-PTR-U8 PE-IN  PE-N PE-OUT PRIM;
PRIM: access   PE-PTR-U8 PE-IN PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM: unlink   PE-PTR-U8 PE-IN  PE-N PE-OUT PRIM;
PRIM: rename   PE-PTR-U8 PE-IN PE-PTR-U8 PE-IN  PE-N PE-OUT PRIM;
PRIM: chmod    PE-PTR-U8 PE-IN PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM: symlink  PE-PTR-U8 PE-IN PE-PTR-U8 PE-IN  PE-N PE-OUT PRIM;
PRIM: readlink PE-PTR-U8 PE-IN PE-PTR-U8 PE-IN PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM: mkdir    PE-PTR-U8 PE-IN PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM: rmdir    PE-PTR-U8 PE-IN  PE-N PE-OUT PRIM;
PRIM: stat64   PE-PTR-U8 PE-IN PE-PTR-U8 PE-IN  PE-N PE-OUT PRIM;
PRIM: lstat64  PE-PTR-U8 PE-IN PE-PTR-U8 PE-IN  PE-N PE-OUT PRIM;
PRIM: getdirentries64
   PE-N PE-IN PE-PTR-U8 PE-IN PE-N PE-IN PE-PTR-N PE-IN  PE-N PE-OUT PRIM;
PRIM: pipe     PE-N PE-OUT PE-N PE-OUT PE-N PE-OUT PRIM;
PRIM: dup2     PE-N PE-IN PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM: fcntl    PE-N PE-IN PE-N PE-IN PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM: poll     PE-PTR-A PE-IN PE-N PE-IN PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM: kill     PE-N PE-IN PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM: setpgid  PE-N PE-IN PE-N PE-IN  PE-N PE-OUT PRIM;

PRIM: spawn-io  PE-PTR-U8 PE-IN PE-N PE-IN PE-N PE-IN PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM: spawn-argv-io
   PE-PTR-U8 PE-IN PE-PTR-A PE-IN PE-N PE-IN PE-N PE-IN PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM: spawn-argv-env-io
   PE-PTR-U8 PE-IN PE-PTR-A PE-IN PE-PTR-A PE-IN PE-N PE-IN PE-N PE-IN PE-N PE-IN
   PE-N PE-OUT PRIM;
PRIM: spawn-argv-env-cwd-io
   PE-PTR-U8 PE-IN PE-PTR-A PE-IN PE-PTR-A PE-IN PE-PTR-U8 PE-IN
   PE-N PE-IN PE-N PE-IN PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM: fork          PE-N PE-OUT PRIM;
PRIM: wait-rc       PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM: wait-status   PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM: patch32       PE-N PE-IN PE-N PE-IN PRIM;
PRIM-TRUSTED-ONLY!                       \ code injection: only a TRUSTED: boundary may emit machine code (F3)
PRIM: snap-rebase PE-N PE-IN PE-N PE-IN PE-N PE-IN PE-N PE-IN PE-N PE-IN PE-N PE-IN PRIM;
PRIM: write         PE-N PE-IN PE-PTR-U8 PE-IN PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM: close         PE-N PE-IN PRIM;
PRIM: close-rc      PE-N PE-IN  PE-N PE-OUT PRIM;   \ close returning host status (0 ok, <0 fail)
PRIM: epoch-seconds PE-N PE-OUT PRIM;
PRIM: mono-ns       PE-N PE-OUT PRIM;
PRIM: prof-on       PE-N PE-IN PRIM;
PRIM: prof-report   PRIM;

PRIM: rbase          PE-N PE-OUT PRIM;
PRIM: cp@            PE-N PE-OUT PRIM;
PRIM: cp!            PE-N PE-IN PRIM;
PRIM: dbase@         PE-N PE-OUT PRIM;
PRIM: check@         PE-N PE-OUT PRIM;
PRIM: ndict@         PE-N PE-OUT PRIM;
PRIM: ndict!         PE-N PE-IN PRIM;
PRIM: SEAL-CAPTURE   PRIM;
PRIM: SEAL-FRIEND    PRIM;
PRIM: DRAIN-PRETRUST PRIM;   \ dot habu-engine-pre-trust-77410827: drains the pending pre-trust defer table
PRIM: data-base      PE-PTR-A PE-OUT PRIM;
PRIM: prot-wid-add   PE-N PE-IN PRIM;
PRIM: prot-wid-room  PE-N PE-OUT PRIM;
PRIM: owner-wid-preflight? PE-N PE-IN PE-N PE-IN PE-N PE-IN  PE-F PE-OUT PRIM;
PRIM: owner-wid-public?    PE-N PE-IN  PE-F PE-OUT PRIM;
PRIM: owner-wid-private?   PE-N PE-IN  PE-F PE-OUT PRIM;
PRIM: owner-wid?           PE-N PE-IN  PE-F PE-OUT PRIM;
PRIM: TFAM-CTOR-WORD? PE-PTR-U8 PE-IN PE-N PE-IN  PE-F PE-OUT PRIM;
PRIM: wordlist       PE-N PE-OUT PRIM;
PRIM: get-current    PE-N PE-OUT PRIM;
PRIM: set-current    PE-N PE-IN PRIM;
PRIM: search-wl      PE-PTR-U8 PE-IN PE-N PE-IN PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM: parse-name     PE-PTR-U8 PE-OUT PE-N PE-OUT PRIM;
PRIM: CORE-STR=      PE-PTR-U8 PE-IN PE-N PE-IN PE-PTR-U8 PE-IN PE-N PE-IN  PE-F PE-OUT PRIM;
PRIM: CORE-STR=CI    PE-PTR-U8 PE-IN PE-N PE-IN PE-PTR-U8 PE-IN PE-N PE-IN  PE-F PE-OUT PRIM;
PRIM: PATHZ          PE-PTR-U8 PE-IN PE-N PE-IN PE-PTR-U8 PE-IN PRIM;
PRIM: PATH0          PE-PTR-U8 PE-IN PE-N PE-IN  PE-PTR-U8 PE-OUT PRIM;
PRIM: RD32           PE-PTR-U8 PE-IN  PE-N PE-OUT PRIM;
PRIM: DIAG-FILE!     PE-PTR-U8 PE-IN PE-N PE-IN PRIM;
PRIM: DIAG-ORIGIN!   PE-N PE-IN PE-N PE-IN PE-N PE-IN PRIM;
PRIM: DIAG-JSON!     PE-F PE-IN PRIM;
PRIM: DIAG-BUFFER!   PE-PTR-U8 PE-IN PE-N PE-IN PRIM;
PRIM: DIAG-BUFFER-OFF PRIM;
PRIM: DIAG-BUFFER$   PE-PTR-U8 PE-OUT PE-N PE-OUT PRIM;
PRIM: CHECKER-SCOPE-START PRIM;
PRIM: CHECKER-SCOPE-FINALIZE PRIM;
PRIM: CHECKER-SCOPE-DONE PRIM;
PRIM: CHECKER-SCOPE-DEPTH PE-N PE-OUT PRIM;
PRIM: CHECK-CANDIDATE! PE-PTR-U8 PE-IN PE-N PE-IN  PE-N PE-OUT PRIM;
\ CHECK / CHECK! are the public verdict words (`s" ..." CHECK! .` is the
\ documented top-level probing idiom); the axioms keep them checker-known so
\ the seal-time internal-word marking pass leaves them executable at top level
\ (dot habu-hb-crash-bare-c5be6634).
PRIM: CHECK  PE-PTR-U8 PE-IN PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM: CHECK! PE-PTR-U8 PE-IN PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM: CHECKER-CANDIDATE-SCOPE-START PRIM;
PRIM: CHECKER-CANDIDATE-SCOPE-DONE PRIM;
PRIM: CHECKER-USIGS-TRUNCATE-FROM PE-PTR-U8 PE-IN PE-N PE-IN PRIM;
PRIM: CHECKER-USIGS-TRUNCATE-FROM-RAW PE-PTR-U8 PE-IN PE-N PE-IN PRIM;
PRIM: CHECKER-UNDEFINE PE-PTR-U8 PE-IN PE-N PE-IN PRIM;
PRIM: CHECKER-UNDEFINE-GUARD PE-PTR-U8 PE-IN PE-N PE-IN PRIM;
PRIM: CHECKER-EXPORT PE-PTR-U8 PE-IN PE-N PE-IN PRIM;
PRIM: CHECKER-PACKAGE-ACTIVE? PE-F PE-OUT PRIM;
PRIM: CHECKER-DEFLINEAR PE-PTR-U8 PE-IN PE-N PE-IN PRIM;
PRIM: CHECKER-DEFRECORD PE-PTR-U8 PE-IN PE-N PE-IN PE-PTR-U8 PE-IN PE-N PE-IN PRIM;
PRIM: CHECKER-DEFFAMILY PE-PTR-U8 PE-IN PE-N PE-IN PE-PTR-U8 PE-IN PE-N PE-IN PRIM;
PRIM: CHECKER-DEFSUM PE-PTR-U8 PE-IN PE-N PE-IN PE-PTR-U8 PE-IN PE-N PE-IN PRIM;
PRIM: CHECKER-DEFSUM-NOEND PE-PTR-U8 PE-IN PE-N PE-IN PE-PTR-U8 PE-IN PE-N PE-IN PRIM;
\ CHECKER-DEFENUM had a row here until the global ENUM keyword moved to the
\ unified front end. Its only caller was sumtype.f's own ENUM definer, and the
\ two tools that used it for metadata-only registration (tools/check-core.f and
\ src/habu/verify-source.f) now register through ENUM-DECL:ED-REPLAY, so the word
\ and its axiom went together.
PRIM: CHECKER-DEFPRODUCT PE-PTR-U8 PE-IN PE-N PE-IN PE-PTR-U8 PE-IN PE-N PE-IN PRIM;
PRIM: CHECKER-LAYOUT-INFO PE-PTR-U8 PE-IN PE-N PE-IN  PE-N PE-OUT PE-N PE-OUT PE-F PE-OUT PRIM;
PRIM: CHECKER-STORAGE-INFO PE-PTR-U8 PE-IN PE-N PE-IN  PE-N PE-OUT PE-F PE-OUT PRIM;
PRIM: CHECKER-DEFLAYOUT-BUFFER
   PE-PTR-U8 PE-IN PE-N PE-IN  PE-PTR-U8 PE-IN PE-N PE-IN
   PE-PTR-U8 PE-IN PE-N PE-IN PRIM;
PRIM: CHECKER-DEFTYPED-BUFFER
   PE-PTR-U8 PE-IN PE-N PE-IN  PE-PTR-U8 PE-IN PE-N PE-IN
   PE-PTR-U8 PE-IN PE-N PE-IN PRIM;
PRIM: CHECKER-DEFTYPED-VARIABLE
   PE-PTR-U8 PE-IN PE-N PE-IN  PE-PTR-U8 PE-IN PE-N PE-IN PRIM;
PRIM: CHECKER-LBUF-NAME-GUARD PE-PTR-U8 PE-IN PE-N PE-IN PRIM;
PRIM: CHECKER-DEFINED? PE-PTR-U8 PE-IN PE-N PE-IN  PE-F PE-OUT PRIM;
\ CAST-PEND! ( name$ -- ) arms the one-shot cast-certification window (defined
\ near CTOR-PEND below). The axiom keeps it checker-known so the roles.f CAST:
\ declarer — an ordinary checked word — can call it; unlike CTOR-PEND!/LBUF-PEND!
\ it is deliberately NOT xref-erased (the rationale rides at the definition).
PRIM: CAST-PEND! PE-PTR-U8 PE-IN PE-N PE-IN PRIM;
\ TRUST is the public top-level effect-declaration word ( name$ effect$ -- ).
\ The axiom keeps it checker-known so the seal-time internal-word marking pass
\ (src/core/internal-mark.f) leaves it executable at top level (dot
\ habu-hb-crash-bare-c5be6634); UNSAFE-TOK? still rejects `trust` inside
\ checked bodies, so the axiom adds no checked-code capability.
PRIM: TRUST PE-PTR-U8 PE-IN PE-N PE-IN PE-PTR-U8 PE-IN PE-N PE-IN PRIM;
\ PTX-BARRIER! ( name$ -- ) is the top-level explicit barrier-declaration word
\ (M5b): it marks a named word a block-uniform barrier so a call reached under
\ divergent control rejects. The axiom keeps it checker-known so the seal-time
\ internal-word marking pass leaves it executable at top level, exactly like
\ TRUST; UNSAFE-TOK? rejects `ptx-barrier!` inside checked bodies, so the axiom
\ adds no checked-code capability. Marking is monotone-safe: it only ever ADDS a
\ rejection under control flow and can never force an unsound acceptance.
PRIM: PTX-BARRIER! PE-PTR-U8 PE-IN PE-N PE-IN PRIM;
PRIM: TFAM-N@ PE-N PE-OUT PRIM;
PRIM: TFAM-WIDTH@ PE-N PE-IN  PE-N PE-OUT PRIM;
\ Public-signature metadata: registry accessors so the checked public-signatures
\ tool can synthesize constructor signatures from TFAM/SUMV metadata (item 13).
\ Read-only registry reads (never hidden fields); NOT trust boundaries.
PRIM: TFAM-NAME$ PE-N PE-IN  PE-PTR-U8 PE-OUT PE-N PE-OUT PRIM;
PRIM: TFAM-ARITY@ PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM: TFAM-DECL-PARAM-COUNT PE-N PE-OUT PRIM;
PRIM: TFAM-DECL-PARAM>CHAR PE-N PE-IN  PE-N PE-OUT PE-F PE-OUT PRIM;
PRIM: TFAM-DECL-CHAR>PARAM PE-N PE-IN  PE-N PE-OUT PE-F PE-OUT PRIM;
PRIM: TFAM-KIND@ PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM: TFAM-PUBLIC? PE-N PE-IN  PE-F PE-OUT PRIM;
PRIM: TFAM-DERIVE-EQ? PE-N PE-IN  PE-F PE-OUT PRIM;
PRIM: TFAM-DERIVE-HASH? PE-N PE-IN  PE-F PE-OUT PRIM;
PRIM: TFAM-VAR-START@ PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM: TFAM-VAR-COUNT@ PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM: SUMV-NAME$ PE-N PE-IN  PE-PTR-U8 PE-OUT PE-N PE-OUT PRIM;
PRIM: SUMV-CTOR-PKG$ PE-N PE-IN  PE-PTR-U8 PE-OUT PE-N PE-OUT PRIM;
PPRIM: TYPE-FIELD COUNT PE-N PE-OUT PPRIM;
PPRIM: TYPE-FIELD TX-DEPTH PE-N PE-OUT PPRIM;
PPRIM: TYPE-FIELD NO-VARIANT PE-N PE-OUT PPRIM;
PPRIM: TYPE-FIELD FIND PE-N PE-IN PE-N PE-IN PE-PTR-U8 PE-IN PE-N PE-IN  PE-N PE-OUT PE-F PE-OUT PPRIM;
PPRIM: TYPE-FIELD EACH PE-N PE-IN PE-N PE-IN PE-N PE-IN  PE-N PE-OUT PE-F PE-OUT PPRIM;
PPRIM: TYPE-FIELD FAMILY@ PE-N PE-IN  PE-N PE-OUT PPRIM;
PPRIM: TYPE-FIELD VARIANT@ PE-N PE-IN  PE-N PE-OUT PPRIM;
PPRIM: TYPE-FIELD NAME$ PE-N PE-IN  PE-PTR-U8 PE-OUT PE-N PE-OUT PPRIM;
PPRIM: TYPE-FIELD SCHEMA@ PE-N PE-IN  PE-N PE-OUT PPRIM;
PPRIM: TYPE-FIELD SLOT@ PE-N PE-IN  PE-N PE-OUT PPRIM;
PPRIM: TYPE-FIELD CELLS@ PE-N PE-IN  PE-N PE-OUT PPRIM;
PPRIM: TYPE-FIELD BYTE-OFF@ PE-N PE-IN  PE-N PE-OUT PPRIM;
PPRIM: TYPE-FIELD BYTES@ PE-N PE-IN  PE-N PE-OUT PPRIM;
PPRIM: TYPE-FIELD ALIGN@ PE-N PE-IN  PE-N PE-OUT PPRIM;
PPRIM: TYPE-FIELD FLAGS@ PE-N PE-IN  PE-N PE-OUT PPRIM;
\ Product-field transaction lifecycle (dots habu-own-product-field-86660116 /
\ habu-type-field-owner-619ec6b5). TYPE-FIELD-OWNER is a pre-hook package, so
\ without these axioms every post-hook caller would need a TRUSTED shim; with
\ them DECL-EVENT calls the qualified API as ordinary checked code. The token is
\ an opaque `n` that OPEN mints and each later phase presents unchanged. PREPARE
\ returns the validated provisional field count, which is what lets DECL-EVENT
\ prove event/field contiguity without reading a raw registry cell. ADD's row is
\ the full field record: token, family, variant, name string, schema root, then
\ the logical slot/cells and physical byte-offset/bytes/alignment/flags, and it
\ returns the same token so callers can thread it.
PPRIM: TYPE-FIELD-OWNER OPEN PE-N PE-OUT PPRIM;
PPRIM: TYPE-FIELD-OWNER ADD PE-N PE-IN PE-N PE-IN PE-N PE-IN PE-PTR-U8 PE-IN PE-N PE-IN PE-N PE-IN PE-N PE-IN PE-N PE-IN PE-N PE-IN PE-N PE-IN PE-N PE-IN PE-N PE-IN  PE-N PE-OUT PPRIM;
PPRIM: TYPE-FIELD-OWNER PREPARE PE-N PE-IN  PE-N PE-OUT PPRIM;
PPRIM: TYPE-FIELD-OWNER COMMIT PE-N PE-IN PPRIM;
PPRIM: TYPE-FIELD-OWNER FINALIZE PE-N PE-IN PPRIM;
PPRIM: TYPE-FIELD-OWNER ROLLBACK PE-N PE-IN PPRIM;
PPRIM: TYPE-FIELD-OWNER TX-SCHEMA-FOR PE-N PE-IN PE-N PE-IN PE-N PE-IN  PE-N PE-OUT PPRIM;
\ TX-CELLS-FOR is the sibling token-scoped provisional query and the eighth
\ public owner word DECL-EVENT calls. It needs its own axiom for the same reason
\ TX-SCHEMA-FOR does: without it the payload-cell readers would keep the one
\ DEV-FLD TRUSTED shim the migration is meant to remove entirely.
PPRIM: TYPE-FIELD-OWNER TX-CELLS-FOR PE-N PE-IN PE-N PE-IN PE-N PE-IN  PE-N PE-OUT PPRIM;
PRIM: WF-N@ PE-N PE-OUT PRIM;
PRIM: WF-OFF@ PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM: WF-POS@ PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM: WF-FAM@ PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM: WF-WIDTH@ PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM: WF-TERM@ PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM: WF-FLAGS@ PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM: WF-WIDE? PE-F PE-OUT PRIM;
PRIM: WF-NEEDS-P2? PE-F PE-OUT PRIM;
PRIM: WF-W-AT PE-N PE-IN  PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM: wide-mark PRIM;
PRIM: REC-WIDE-PUBLISH PRIM;
PRIM: REC-MIN-IN@ PE-N PE-OUT PRIM;
PRIM: LOCW-HW@ PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM: LOCW-HW-N@ PE-N PE-OUT PRIM;
PPRIM: LOWER-CERT MAGIC PE-N PE-OUT PPRIM;
PPRIM: LOWER-CERT VERSION PE-N PE-OUT PPRIM;
PPRIM: LOWER-CERT HEADER-CELLS PE-N PE-OUT PPRIM;
PPRIM: LOWER-CERT MAGIC-CELL PE-N PE-OUT PPRIM;
PPRIM: LOWER-CERT VERSION-CELL PE-N PE-OUT PPRIM;
PPRIM: LOWER-CERT TOTAL-BYTES-CELL PE-N PE-OUT PPRIM;
PPRIM: LOWER-CERT NEEDS-CELL PE-N PE-OUT PPRIM;
PPRIM: LOWER-CERT WF-COUNT-CELL PE-N PE-OUT PPRIM;
PPRIM: LOWER-CERT BIND-COUNT-CELL PE-N PE-OUT PPRIM;
PPRIM: LOWER-CERT FETCH-COUNT-CELL PE-N PE-OUT PPRIM;
PPRIM: LOWER-CERT FETCH-DATA-CELLS-CELL PE-N PE-OUT PPRIM;
PPRIM: LOWER-CERT WF-CELLS PE-N PE-OUT PPRIM;
PPRIM: LOWER-CERT FETCH-CELLS PE-N PE-OUT PPRIM;
PPRIM: LOWER-CERT CHECK-CELLS PE-N PE-OUT PPRIM;
PPRIM: LOWER-CERT GUARD-CELLS PE-N PE-OUT PPRIM;
PPRIM: LOWER-CERT FETCH-FLAG PE-N PE-OUT PPRIM;
PPRIM: LOWER-CERT STORE-FLAG PE-N PE-OUT PPRIM;
PPRIM: LOWER-CERT XPAD-FLAG PE-N PE-OUT PPRIM;
PPRIM: LOWER-CERT BODY-LEN-CELL PE-N PE-OUT PPRIM;
PPRIM: LOWER-CERT BODY-HASH-CELL PE-N PE-OUT PPRIM;
PPRIM: LOWER-CERT FNV-OFFSET PE-N PE-OUT PPRIM;
PPRIM: LOWER-CERT FNV-PRIME PE-N PE-OUT PPRIM;
PPRIM: LOWER-CERT CELL-COUNT PE-N PE-OUT PPRIM;
PPRIM: LOWER-CERT CELL@ PE-N PE-IN PE-N PE-OUT PPRIM;
PPRIM: LOWER-CERT BYTES PE-PTR-U8 PE-OUT PE-N PE-OUT PPRIM;
PRIM-TRUSTED-ONLY!
PPRIM: LOWER-CERT-HOOK HOOK PE-PTR-U8 PE-IN PE-N PE-IN PE-N PE-OUT PPRIM;
PPRIM: CHECKER-CERT INSTALL PE-N PE-IN PPRIM;
PPRIM: CHECKER-CERT PRODUCE PE-PTR-U8 PE-IN PE-N PE-IN PE-N PE-IN PPRIM;
PRIM: P2-LOCSEQ-RESET PRIM;
PRIM: P2-CARVE-W PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM: P2-LIVE-W@ PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM: P2-LIVE-CUM@ PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM: SUMV-N@ PE-N PE-OUT PRIM;
PRIM: TF-STR-U@ PE-N PE-OUT PRIM;
PRIM: TF-PK-N@ PE-N PE-OUT PRIM;
PRIM: LAY-N@ PE-N PE-OUT PRIM;
PRIM: SCHEMA-N@ PE-N PE-OUT PRIM;
PRIM: SCHEMA-ROOT-N@ PE-N PE-OUT PRIM;
PRIM: CHECKER-DEFER PE-PTR-U8 PE-IN PE-N PE-IN PRIM;
PRIM: CHECKER-PACKAGE PE-PTR-U8 PE-IN PE-N PE-IN PRIM;
PRIM: CHECKER-USING PE-PTR-U8 PE-IN PE-N PE-IN PRIM;
PRIM: CHECKER-PUBLIC PRIM;
PRIM: CHECKER-PRIVATE PRIM;
PRIM: CHECKER-END-PACKAGE PRIM;
PRIM: ffi-call       PE-PTR-A PE-IN PE-N PE-IN PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM-TRUSTED-ONLY!
PRIM: ffi-call-n     PE-PTR-A PE-IN PE-N PE-IN PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM-TRUSTED-ONLY!
PRIM: ffi-call-bounded PE-PTR-A PE-IN PE-PTR-B PE-IN PE-N PE-IN PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM-TRUSTED-ONLY!
PRIM: ffi-call-abi-bounded PE-PTR-A PE-IN PE-PTR-B PE-IN PE-PTR-C PE-IN
                           PE-PTR-D PE-IN PE-PTR-E PE-IN PE-N PE-IN PE-N PE-IN
                           PE-N PE-OUT PRIM;
PRIM-TRUSTED-ONLY!
PRIM: ffi-call-abi-r-bounded PE-PTR-A PE-IN PE-PTR-B PE-IN PE-PTR-C PE-IN
                             PE-PTR-D PE-IN PE-PTR-E PE-IN PE-N PE-IN PE-N PE-IN
                             PE-R PE-OUT PRIM;
PRIM-TRUSTED-ONLY!
PRIM: ffi-call-abi   PE-PTR-A PE-IN PE-PTR-B PE-IN PE-PTR-C PE-IN PE-N PE-IN PE-N PE-IN
                     PE-N PE-IN PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM-TRUSTED-ONLY!
PRIM: ffi-call-abi-r PE-PTR-A PE-IN PE-PTR-B PE-IN PE-PTR-C PE-IN PE-N PE-IN PE-N PE-IN
                     PE-N PE-IN PE-N PE-IN  PE-R PE-OUT PRIM;
PRIM-TRUSTED-ONLY!

PRIM: f+      PE-R PE-IN PE-R PE-IN  PE-R PE-OUT PRIM;
PRIM: f-      PE-R PE-IN PE-R PE-IN  PE-R PE-OUT PRIM;
PRIM: f*      PE-R PE-IN PE-R PE-IN  PE-R PE-OUT PRIM;
PRIM: f/      PE-R PE-IN PE-R PE-IN  PE-R PE-OUT PRIM;
PRIM: fnegate PE-R PE-IN  PE-R PE-OUT PRIM;
PRIM: fabs    PE-R PE-IN  PE-R PE-OUT PRIM;
PRIM: fsqrt   PE-R PE-IN  PE-R PE-OUT PRIM;
PRIM: f<      PE-R PE-IN PE-R PE-IN  PE-F PE-OUT PRIM;
PRIM: f>      PE-R PE-IN PE-R PE-IN  PE-F PE-OUT PRIM;
PRIM: f=      PE-R PE-IN PE-R PE-IN  PE-F PE-OUT PRIM;
PRIM: f0<     PE-R PE-IN  PE-F PE-OUT PRIM;
PRIM: f0=     PE-R PE-IN  PE-F PE-OUT PRIM;
PRIM: s>f     PE-N PE-IN  PE-R PE-OUT PRIM;
PRIM: f>s     PE-R PE-IN  PE-N PE-OUT PRIM;
PRIM: f.      PE-R PE-IN PRIM;

PRIM: s"     PE-PTR-U8 PE-OUT PE-N PE-OUT PRIM;
PRIM: c"     PE-PTR-U8 PE-OUT PRIM;
PRIM: ."     PRIM;
PRIM: s\"    PE-PTR-U8 PE-OUT PE-N PE-OUT PRIM;
PRIM: c\"    PE-PTR-U8 PE-OUT PRIM;
PRIM: .\"    PRIM;
PRIM: [']    PE-N PE-OUT PRIM;
PRIM: char   PE-N PE-OUT PRIM;
PRIM: [char] PE-N PE-OUT PRIM;
PRIM: emit   PE-N PE-IN PRIM;
PRIM: cr     PRIM;
PRIM: space  PRIM;
PRIM: u.     PE-N PE-IN PRIM;

PRIM: create   PE-PTR-A PE-OUT PRIM;
PRIM: variable PE-PTR-A PE-OUT PRIM;
PRIM: constant PE-A PE-OUT PRIM;
PRIM: getpid   PE-N PE-OUT PRIM;   \ ( -- pid ) process-identity syscall
PRIM: proc-watch-open PE-N PE-IN PE-N PE-OUT PRIM;   \ ( pid -- fd|-1 ) process-lifetime watch
PRIM: kill-errno PE-N PE-IN PE-N PE-IN  PE-N PE-OUT PRIM;   \ ( pid sig -- 0|-errno ) signal with errno detail
PRIM: execve   PE-PTR-U8 PE-IN PE-PTR-A PE-IN PE-PTR-A PE-IN  PE-N PE-OUT PRIM;   \ ( pathz argv envp -- -errno ) child-side exec; only returns on failure
PRIM: munmap   PE-PTR-U8 PE-IN PE-N PE-IN  PE-N PE-OUT PRIM;   \ ( addr len -- 0|-1 ) release a mapping; consumed by MEM:RELEASE-BYTES
\ BTC-7: EXTPROD: (maki/extent.f) marks a product's free factor via this axiom. The
\ effect keeps it checker-known so the seal-time internal-word marking pass leaves it
\ callable from the candidate-B surface (like CHECKER-DEFFAMILY for EXTENT:). Placed
\ last in the checker prim table so it appends to the ordered manifests. Last checker.f axiom.
PRIM: EXT-MARK-FREE-TAIL PE-PTR-U8 PE-IN PE-N PE-IN PRIM;

PTABLE-END

variable CHECKER-COLON-N
variable CHECKER-COLON-I
variable CHECKER-REC-A
variable CHECKER-REC-U
variable CHECKER-QA
variable CHECKER-QU
variable CHECKER-TA
variable CHECKER-TU

$10000 constant DFER-CAP

0 constant DFER-SYM-CELL
1 constant DFER-FLAG-CELL
$0 constant DFER-SYM-OFF
$8 constant DFER-FLAG-OFF
$10 constant DFER-REC
$8 constant DFER-REC-ALIGN
0 constant DFER-REC-PTR-MASK

: DFER.SYM ( ptr a -- ptr a ) DFER-SYM-OFF + ;
: DFER.FLAG ( ptr a -- ptr a ) DFER-FLAG-OFF + ;

: DFER-LAYOUT-ASSERT ( -- )
   DFER-SYM-CELL cells DFER-SYM-OFF CHECKER-RECORD-LAYOUT=
   DFER-FLAG-CELL cells DFER-FLAG-OFF CHECKER-RECORD-LAYOUT=
   DFER-FLAG-OFF CELL + DFER-REC CHECKER-RECORD-LAYOUT=
   CELL DFER-REC-ALIGN CHECKER-RECORD-LAYOUT=
   DFER-REC DFER-REC-ALIGN mod 0 CHECKER-RECORD-LAYOUT=
   DFER-REC-PTR-MASK 0 CHECKER-RECORD-LAYOUT= ;

DFER-LAYOUT-ASSERT

create DFERS DFER-CAP allot
variable DFER-END
0 DFERS !
0 DFER-END !

: CHECKER-FOLD-C ( n -- n ) {: c:n :}
   c $41 < IF c EXIT THEN
   c $5A > IF c EXIT THEN
   c $20 or ;

: CHECKER-PACKAGE-COPY-C ( ptr u8 n -- ) {: a:ptr i:n :}
   a i + c@ CHECKER-FOLD-C CHECKER-PACKAGE-NAME i + c! ;

: CHECKER-PACKAGE-COPY ( ptr u8 n -- ) {: a:ptr u:n :}
   u CHECKER-PACKAGE-CAP >= IF s" checker: package name too long" 76 die THEN
   0 BEGIN dup u < WHILE
      a over CHECKER-PACKAGE-COPY-C
      1 +
   REPEAT drop
   u CHECKER-PACKAGE-U ! ;

\ --- `using`-scope import mirror (dot habu-using-import-pkg-a07dd7ba). The engine owns
\ the live using depth in one fixed DATA cell (layout.f USE-DEPTH-CELL); the checker reads
\ it through data-base so this parallel table of folded package names is bounded by the
\ identical depth. There is no separate checker counter to drift across the ;using /
\ ;package / end-of-file / throw / REPL boundaries — every boundary just restores the shared
\ engine depth and the checker automatically follows. CHECKER-USING (called by the engine
\ C-USING before it increments the shared depth, and by verify-source likewise) records the
\ name into the slot at the current depth; resolution reads names[0..depth).
16 constant CK-USE-MAX                     \ = layout.f USE-MAX (concurrent usings)
$9C08 constant CK-USE-DEPTH-OFF            \ = layout.f USE-DEPTH-CELL (DATA-relative); engine owns it
create CK-USE-NAMES CK-USE-MAX CHECKER-PACKAGE-CAP * allot
create CK-USE-LENS  CK-USE-MAX cells allot
7140 constant E-USING-AMBIGUOUS            \ bare tail resolves in more than one used public wordlist
variable CK-USED-FOUND                     \ interned sym of the first used-public match while resolving
variable CK-USED-SLOT                      \ used-scan slot of that first match (-1 = none), for the shadow diagnostic

: CK-USE-DEPTH ( -- n )  data-base CK-USE-DEPTH-OFF + @ ;   \ engine-owned live using depth (USE-DEPTH-CELL)
: CK-USE-SLOT ( n -- ptr u8 )  CHECKER-PACKAGE-CAP * CK-USE-NAMES + ;
: CK-USE-LEN@ ( n -- n )  cells CK-USE-LENS + @ ;

\ Slots the used-scope search may read, bounded by the physical CK-USE-MAX-slot
\ mirror (CK-USE-NAMES / CK-USE-LENS). This bound is the invariant that keeps the
\ search safe on EVERY engine, because checker.f is cold-prefix source re-read at
\ every engine boot: an OLDER bootstrap engine that predates the using band (the
\ seed that builds the first using-capable fixpoint) has no USE-DEPTH-CELL, so
\ CK-USE-DEPTH-OFF aliases that engine's DP-heap base and the raw read is
\ unrelated heap data, not a depth. Capping the scan at CK-USE-MAX makes the loop
\ terminate and never index past the mirror no matter what that read returns.
\ Correctness then rests on the mirror's CONTENTS, not on the raw depth value:
\ CHECKER-USING is the sole writer of the mirror and is only ever driven by the
\ engine's C-USING for a real `using`, so an engine with no live using-scope
\ leaves every scanned slot at its zero-initialised length and SYM-FIND matches
\ nothing. On a using-capable engine the depth is always in [0,CK-USE-MAX] (C-USING
\ dies on push overflow), so the cap never binds and the scan is exactly the live
\ usings, unchanged.
: CK-USE-SCAN-N ( -- n )
   CK-USE-DEPTH
   dup 0 < IF drop 0 EXIT THEN
   dup CK-USE-MAX > IF drop CK-USE-MAX THEN ;

: CHECKER-USING ( ptr u8 n -- ) {: a:ptr u:n :}
   u CHECKER-PACKAGE-CAP >= IF s" checker: using name too long" 76 die THEN
   CK-USE-DEPTH {: d:n :}
   d CK-USE-MAX >= IF s" checker: using stack overflow" 76 die THEN
   d CK-USE-SLOT {: dst:ptr :}
   0 BEGIN dup u < WHILE
      dup a + c@ CHECKER-FOLD-C  over dst + c!      \ dst[i] = fold(name[i])
      1 +
   REPEAT drop
   u d cells CK-USE-LENS + ! ;

\ Resolve a bare tail against the live used publics (searched only after the open-scope +
\ global chain missed). A single distinct interned public sym wins; a second distinct sym
\ across the used packages is the ambiguity hard error, matching the engine's used-search.
: CHECKER-USED-SYM ( ptr u8 n -- n ) {: a:ptr u:n :}
   0 CK-USED-FOUND !
   -1 CK-USED-SLOT !
   CK-USE-SCAN-N 0 ?DO
      i CK-USE-SLOT i CK-USE-LEN@ SYM-PUBLIC a u SYM-FIND IF     ( -- sym )
         CK-USED-FOUND @ 0= IF
            i CK-USED-SLOT !
            CK-USED-FOUND !
         ELSE
            dup CK-USED-FOUND @ <> IF E-USING-AMBIGUOUS throw THEN
            drop
         THEN
      ELSE drop THEN
   LOOP
   CK-USED-FOUND @ ;

\ --- global-vs-used-public shadow rejection (dot habu-err-on-global-e62f806c).
\ Resolution order puts the open-scope + global wordlist ahead of the used
\ publics (docs/forth.md Packages), so a bare tail that already hits a global
\ never consults the used scan: a `using` import of the same name is silently
\ dead and the reference binds the GLOBAL with the global's effect. When the two
\ effects differ the mismatch only surfaces later, far from the cause (the
\ data-loader incident, where a kernel `LOAD` shadowed a loader's public `LOAD`);
\ when they coincide the wrong word binds and certifies with no diagnostic at
\ all. This extends the two-used-package ambiguity rule to the global-vs-used
\ collision: a bare tail inside a live `using` scope that resolves to a global
\ while a used package also exports it is ambiguous and fails closed AT THE
\ REFERENCE SITE. Escape hatches: qualify the package word as PKG:WORD (still
\ certifies), or rename the collision to reach the global (the grammar has no
\ bare qualifier for the global "" wordlist).
7141 constant E-USING-SHADOW-GLOBAL
variable USH-TOK-A   variable USH-TOK-U     \ the ambiguous bare token (raw, valid while rendering)
variable USH-GSYM    variable USH-USYM      \ the two colliding syms: global, used public
variable USH-PKG-A   variable USH-PKG-U     \ the used package's folded name (renders PKG:WORD)
defer USHADOW-DIAG-XT ( -- )                \ render.f installs the reference-site diagnostic
: USHADOW-DIAG-DEFAULT ( -- ) [: ;] is USHADOW-DIAG-XT ;
USHADOW-DIAG-DEFAULT

\ gsym has already resolved the bare tail to a global; if a live used public
\ exports the same tail the reference is ambiguous — capture both candidates for
\ the diagnostic and fail closed. CHECKER-USED-SYM throws E-USING-AMBIGUOUS first
\ if TWO used publics also match, so the pre-existing rule keeps precedence.
: CHECKER-USED-SHADOW ( ptr u8 n n -- ) {: a:ptr u:n gsym:n :}
   a u CHECKER-USED-SYM {: usym:n :}
   usym 0= IF EXIT THEN
   a USH-TOK-A !  u USH-TOK-U !
   gsym USH-GSYM !  usym USH-USYM !
   CK-USED-SLOT @ dup 0 >= IF
      dup CK-USE-SLOT USH-PKG-A !  CK-USE-LEN@ USH-PKG-U !
   ELSE
      drop  0 USH-PKG-A !  0 USH-PKG-U !
   THEN
   USHADOW-DIAG-XT
   E-USING-SHADOW-GLOBAL throw ;

\ --- generated-constructor protection (item 8 slice 3). The registry-backed
\ predicates live in type-family.f (loads later) and install into these friend
\ cells; empty cells fail open only in engines with no TFAM registry at all
\ (stage builders, which never see user declarations). Guards: a recorded
\ constructor package cannot be opened/reopened by `package`; a generated
\ constructor word cannot be undefined; a new tail cannot certify into a
\ constructor package (closed-but-callable, PLAN Package Shape).
7111 constant E-CTOR-PROTECTED
7121 constant E-CHECKER-LAYOUT-BUFFER
\ ctor-protection query hooks (item 8), rebound by type-family.f. The
\ registry-not-loaded default reports "not a protected ctor", matching the old
\ 0-hook guards which skipped the throw before the registry existed.
defer CTOR-PKG?-XT ( ptr u8 n -- bool )       \ name is a recorded ctor package?
defer CTOR-WORD?-XT ( ptr u8 n -- bool )      \ name is a generated ctor word?
defer CTOR-EXTEND?-XT ( ptr u8 n -- bool )    \ new tail in a closed ctor package?
: CTOR-PROT-DEFAULTS ( -- )
   [: 2drop RES-FALSE ;] is CTOR-PKG?-XT
   [: 2drop RES-FALSE ;] is CTOR-WORD?-XT
   [: 2drop RES-FALSE ;] is CTOR-EXTEND?-XT ;
CTOR-PROT-DEFAULTS

: CHECKER-UNDEFINE-GUARD ( ptr u8 n -- ) {: a:ptr u:n :}
   a u CTOR-WORD?-XT IF E-CTOR-PROTECTED throw THEN ;

: CHECKER-PACKAGE ( ptr u8 n -- )
   2dup CTOR-PKG?-XT IF E-CTOR-PROTECTED throw THEN
   CHECKER-PACKAGE-COPY
   CHECKER-PACKAGE-PRIVATE CHECKER-PACKAGE-MODE ! ;

: CHECKER-PUBLIC ( -- )
   CHECKER-PACKAGE-ACTIVE? IF CHECKER-PACKAGE-PUBLIC CHECKER-PACKAGE-MODE ! THEN ;

: CHECKER-PRIVATE ( -- )
   CHECKER-PACKAGE-ACTIVE? IF CHECKER-PACKAGE-PRIVATE CHECKER-PACKAGE-MODE ! THEN ;

: CHECKER-END-PACKAGE ( -- )
   CHECKER-PACKAGE-NONE CHECKER-PACKAGE-MODE !
   0 CHECKER-PACKAGE-U ! ;

TRUSTED: CHECKER-CERT-CALL ( ptr u8 n n n -- )
   {: a:ptr u:n verdict:n xt:n :}
   a u verdict xt execute ;

package CHECKER-CERT

variable PRODUCER-XT   0 PRODUCER-XT !

public

: INSTALL ( n -- )
   PRODUCER-XT @ 0 <> if s" checker: lowering certificate producer already installed" 76 die then
   PRODUCER-XT ! ;

: PRODUCE ( ptr u8 n n -- ) {: a:ptr u:n verdict:n :}
   PRODUCER-XT @ 0= if s" checker: lowering certificate producer unavailable" 76 die then
   a u verdict PRODUCER-XT @ CHECKER-CERT-CALL ;

;package

: CHECKER-COLON-SCAN ( ptr u8 n -- ) {: a:ptr u:n :}
   0 CHECKER-COLON-N !
   -1 CHECKER-COLON-I !
   0 BEGIN dup u < WHILE
      a over + c@ $3A = IF
         CHECKER-COLON-N @ 0= IF dup CHECKER-COLON-I ! THEN
         CHECKER-COLON-N @ 1+ CHECKER-COLON-N !
      THEN
      1 +
   REPEAT drop ;

: CHECKER-QA-FIELD ( -- ptr ptr u8 )
   CHECKER-QA 0 ptr-field ;

: CHECKER-TA-FIELD ( -- ptr ptr u8 )
   CHECKER-TA 0 ptr-field ;

: CHECKER-QA@ ( -- ptr u8 )
   CHECKER-QA-FIELD @ ;

: CHECKER-TA@ ( -- ptr u8 )
   CHECKER-TA-FIELD @ ;

: CHECKER-QA! ( ptr u8 -- )
   CHECKER-QA-FIELD ! ;

: CHECKER-TA! ( ptr u8 -- )
   CHECKER-TA-FIELD ! ;

variable CHECKER-QBAD-TOK
$20 constant CK-SEAL-LATCH-OFF          \ = layout.f FRIEND-LATCH-CELL

\ engine FIND parity (habu1.f FIND-QHAS/FIND-QBAD): a leading or trailing first
\ colon keeps the token an ordinary name; a non-edge first colon with a second
\ colon anywhere is a malformed qualified name and must never resolve.
: CHECKER-QUALIFIED? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   0 CHECKER-QBAD-TOK !
   a u CHECKER-COLON-SCAN
   CHECKER-COLON-N @ 0= IF RES-FALSE EXIT THEN
   CHECKER-COLON-I @ 0= IF RES-FALSE EXIT THEN
   CHECKER-COLON-I @ u 1 - = IF RES-FALSE EXIT THEN
   CHECKER-COLON-N @ 1 <> IF -1 CHECKER-QBAD-TOK ! RES-FALSE EXIT THEN
   a CHECKER-QA!
   CHECKER-COLON-I @ CHECKER-QU !
   a CHECKER-COLON-I @ + 1 + CHECKER-TA!
   u CHECKER-COLON-I @ - 1 - CHECKER-TU !
   RES-TRUE ;

: CHECKER-QPKG$ ( -- ptr u8 n )
   CHECKER-QA@ CHECKER-QU @ ;

: CHECKER-QTAIL$ ( -- ptr u8 n )
   CHECKER-TA@ CHECKER-TU @ ;

: CHECKER-SEALED-PKG? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" tfam" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" type" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" match" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" checker-cert" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" lower-cert" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" lower-cert-hook" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" engine-error" CORE-STR=CI ;

: CHECKER-LBUF-NAME-GUARD ( ptr u8 n -- ) {: a:ptr u:n :}
   a u CHECKER-QUALIFIED? drop
   CHECKER-QBAD-TOK @ 0 <> IF E-CHECKER-LAYOUT-BUFFER throw THEN
   data-base CK-SEAL-LATCH-OFF + @ 0 <> IF
      a u CHECKER-QUALIFIED? IF
         CHECKER-QPKG$ CHECKER-SEALED-PKG? IF E-CHECKER-LAYOUT-BUFFER throw THEN
      THEN
   THEN
   a u CTOR-EXTEND?-XT IF E-CTOR-PROTECTED throw THEN ;

\ typed-local-lint: allow-bare-local - a preserves ptr u8 role.
: CHECKER-GLOBAL-SYM ( ptr u8 n -- n ) {: a u:n :}
   s" " SYM-GLOBAL a u SYM-INTERN ;

\ typed-local-lint: allow-bare-local - a preserves ptr u8 role.
: CHECKER-GLOBAL-SYM? ( ptr u8 n -- n ) {: a u:n :}
   s" " SYM-GLOBAL a u SYM-FIND IF EXIT THEN drop 0 ;

\ typed-local-lint: allow-bare-local - pkg/a preserve ptr u8 roles.
: CHECKER-PUBLIC-SYM ( ptr u8 n ptr u8 n -- n ) {: pkg pkgu:n a u:n :}
   pkg pkgu SYM-PUBLIC a u SYM-INTERN ;

\ typed-local-lint: allow-bare-local - pkg/a preserve ptr u8 roles.
: CHECKER-PUBLIC-SYM? ( ptr u8 n ptr u8 n -- n ) {: pkg pkgu:n a u:n :}
   pkg pkgu SYM-PUBLIC a u SYM-FIND IF EXIT THEN drop 0 ;

\ typed-local-lint: allow-bare-local - pkg/a preserve ptr u8 roles.
: CHECKER-PKG-SYM ( ptr u8 n n ptr u8 n -- n ) {: pkg pkgu:n vis:n a u:n :}
   pkg pkgu vis a u SYM-INTERN ;

\ typed-local-lint: allow-bare-local - pkg/a preserve ptr u8 roles.
: CHECKER-PKG-SYM? ( ptr u8 n n ptr u8 n -- n ) {: pkg pkgu:n vis:n a u:n :}
   pkg pkgu vis a u SYM-FIND IF EXIT THEN drop 0 ;

\ typed-local-lint: allow-bare-local - a preserves ptr u8 role.
: CHECKER-RECORD-SYM ( ptr u8 n -- n ) {: a u:n :}
   a u CHECKER-QUALIFIED? IF CHECKER-QPKG$ CHECKER-QTAIL$ CHECKER-PUBLIC-SYM EXIT THEN
   CHECKER-QBAD-TOK @ IF 0 EXIT THEN
   CHECKER-PACKAGE-ACTIVE? IF
      CHECKER-PACKAGE-NAME CHECKER-PACKAGE-U @ CHECKER-PACKAGE-MODE @ a u CHECKER-PKG-SYM EXIT
   THEN
   a u CHECKER-GLOBAL-SYM ;

: CHECKER-FIND-ACTIVE-SYM ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u CHECKER-QUALIFIED? IF CHECKER-QPKG$ CHECKER-QTAIL$ CHECKER-PUBLIC-SYM? EXIT THEN
   CHECKER-QBAD-TOK @ IF 0 EXIT THEN
   CHECKER-PACKAGE-ACTIVE? IF
      CHECKER-PACKAGE-NAME CHECKER-PACKAGE-U @ SYM-PRIVATE a u CHECKER-PKG-SYM? dup 0 <> IF EXIT THEN drop
      CHECKER-PACKAGE-NAME CHECKER-PACKAGE-U @ SYM-PUBLIC a u CHECKER-PKG-SYM? dup 0 <> IF EXIT THEN drop
   THEN
   a u CHECKER-GLOBAL-SYM? dup 0 <> IF
      dup >r a u r> CHECKER-USED-SHADOW      \ throws if a live used public also exports this bare tail
      EXIT
   THEN drop
   a u CHECKER-USED-SYM ;

\ CHECKER-FIND-USIG-SYM ( n -- bool ) : FEP = current active record for sym.
\ Cache value: record offset+1, 0 = none/deleted; a miss re-derives from the
\ arena scan and memoizes both the answer and its watermark dependency.
: CHECKER-FIND-USIG-SYM ( n -- bool ) {: sym:n :}
   sym 0= IF RES-FALSE EXIT THEN
   HIDX-ENSURE
   HIDX-EFF-SYNC
   sym HIDX-EFF@ {: cached:n hit:bool :}
   hit IF
      cached 0 <> IF cached 1 - E-PTR FEP-SET ELSE FEP-CLEAR THEN
   ELSE
      sym SCAN-USIGS-SYM
      FEP-OFF@ sym HIDX-EFF!
      FMEND @ HIDX-EFF-DEP+
   THEN
   FEP-HIT? ;

: CHECKER-FIND-USIG ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u CHECKER-RECORD-SYM CHECKER-FIND-USIG-SYM ;

: CHECKER-USIGS-TRUNCATE-FROM-RAW ( ptr u8 n -- ) {: a:ptr u:n :}
   a u CHECKER-FIND-ACTIVE-SYM USIG-FIND-OFF-SYM 0= IF
      s" checker: missing signature truncation mark" 76 die
   THEN
   UEND !
   UTERM! ;

\ TFAM 2b-iii: a direct post-seal user call would forget the checker's signature
\ for an engine word so it could be redefined (spoof). Reject it fail-closed; the
\ friend/engine load path (latch 0) and the guarded FORGET/HIDE wrappers (which
\ call -RAW only after the ndict-watermark guard passes) reach -RAW unchanged.
\ checker.f loads before layout.f, so the friend-arena latch offset is mirrored
\ here. The failure ABI comes from the earlier engine-error.f package.

: CHECKER-USIGS-TRUNCATE-FROM ( ptr u8 n -- )
   data-base CK-SEAL-LATCH-OFF + @ 0= 0= IF
      2drop s" seal: cannot truncate sealed checker signatures" ENGINE-ERROR:SEAL-VIOLATION die
   THEN
   CHECKER-USIGS-TRUNCATE-FROM-RAW ;

: CHECKER-FIND-ACTIVE-SIG ( ptr u8 n -- ) {: a:ptr u:n :}
   FEP-CLEAR
   a u CHECKER-FIND-ACTIVE-SYM CHECKER-FIND-USIG-SYM drop ;

: FIND-SIG ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u CHECKER-FIND-ACTIVE-SIG
   FEP-HIT? IF RES-TRUE EXIT THEN
   a u CHECKER-FIND-ACTIVE-SYM PRIM-FIRST-SYM
   dup 0 <> IF E-PTR FEP-SET RES-TRUE ELSE drop RES-FALSE THEN ;

\ SIG-MIN-IN: din cell count for a NAME's active effect (user row or prim
\ axiom); -1 when the checker knows no effect. Seal-time consumer:
\ src/core/internal-mark.f pokes known engine-prefix records with it (dot
\ habu-habu-certified-words-84e84eaf).
: SIG-MIN-IN ( ptr u8 n -- n )
   FIND-SIG 0= IF -1 EXIT THEN
   FEP @ ER.MINI @ ;

\ ---- effect-read export API (dot habu-expose-checker-effect-95e853eb) ---------
\ A cold-prefix consumer (src/core/top-row.f, and the tier-2 typed top-row, dot
\ habu-typed-top-tier-589c550f) cannot reach the effect store directly: FIND-SIG,
\ ER.DIN/ER.DOUT, E-PTR and the EN-* node accessors are sig-less colon words, so
\ internal-mark.f marks them DNAME-INT and they fail closed at interpret time. This
\ is the ONE stable, minimal query surface the prefix sees. Each PUBLIC entry carries
\ a declared TRUSTED: signature, so SIG-MIN-IN finds it and internal-mark leaves it
\ top-level callable; the readers project the private EN-node graph onto a coarse,
\ stable family enum, so the graph representation stays free to evolve behind them.
\ Read-only: no store word is exposed and the query never mutates a persisted record.
\ EFFECT-QUERY resolves a NAME's active effect (user row or prim axiom) into query
\ state; the EFFECT-* readers report on the last successful query.
\
\ Family enum (stable ABI; numerically equal to top-row.f TR-* by design):
0 constant EFAM-GRAY        \ EN-VAR / EN-ROW / EN-ATOM / EN-PARAM: no coarse family
1 constant EFAM-SCALAR      \ EN-CON: a value cell
2 constant EFAM-POINTER     \ EN-PTR: a byte / region pointer
3 constant EFAM-XT          \ EN-QUOT: a quotation / xt

variable EFFQ-OK            \ bool: last EFFECT-QUERY resolved an active effect
variable EFFQ-DIN           \ din row offset (E-OFF) of the queried effect, 0 = none
variable EFFQ-DOUT          \ dout row offset

\ The EN-node graph lives in the byte-addressed USIGS arena; USIGS-CELL-AT turns an
\ E-OFF into a cell pointer the checked readers can fetch (E-PTR yields a ptr u8 that
\ cell-fetch rejects). The stored offsets are node-aligned, so this is the same
\ address E-PTR computes, typed for a checked cell read.
: EFF-TAG@ ( n -- n )   USIGS-CELL-AT EN.TAG @ ;    \ node tag at offset n
: EFF-A@   ( n -- n )   USIGS-CELL-AT EN.A @ ;      \ EN.A field (head term offset)
: EFF-B@   ( n -- n )   USIGS-CELL-AT EN.B @ ;      \ EN.B field (rest-of-row offset)

: EFF-TERM-FAM ( n -- n )       \ n = term-node offset (E-OFF); 0 -> gray
   dup 0= if drop EFAM-GRAY exit then
   EFF-TAG@
   dup EN-PTR  = if drop EFAM-POINTER exit then
   dup EN-QUOT = if drop EFAM-XT exit then
   EN-CON = if EFAM-SCALAR exit then
   EFAM-GRAY ;

: EFF-IS-PUSH? ( n -- bool )    \ n = row-node offset; a live fixed EN-PUSH term?
   dup 0= if drop 0 0= 0= exit then
   EFF-TAG@ EN-PUSH = ;

: EFF-ROW-N ( n -- n )          \ count fixed EN-PUSH terms in a stored row (head = top)
   0 swap
   begin dup EFF-IS-PUSH? while
      EFF-B@ swap 1 + swap
   repeat drop ;

: EFF-ROW-FAM ( n n -- n ) {: i:n off:n :}   \ family of the i-th term from the top (0-based)
   off i
   begin dup 0 > while                        \ ( cur i ) advance i EN-PUSH terms
      over EFF-IS-PUSH? 0= if 2drop EFAM-GRAY exit then
      swap EFF-B@ swap 1 -
   repeat drop                                \ ( cur )
   dup EFF-IS-PUSH? if EFF-A@ EFF-TERM-FAM else drop EFAM-GRAY then ;

\ Only EFFECT-QUERY is trusted: it reads FEP / ER.DIN / ER.DOUT (raw checker state
\ the checker cannot type, like its sibling USIGS readers). The four readers below
\ are ordinary checked words over EFF-ROW-N / EFF-ROW-FAM, so the trusted base grows
\ by exactly one site; internal-mark strips their names past the seal, but the query
\ boundary (and the prefix consumers) reach them as compiled calls.
TRUSTED: EFFECT-QUERY ( ptr u8 n -- bool )    \ resolve NAME's active effect into query state
   FIND-SIG dup EFFQ-OK !
   if FEP @ ER.DIN @ EFFQ-DIN !  FEP @ ER.DOUT @ EFFQ-DOUT !
   else 0 EFFQ-DIN !  0 EFFQ-DOUT ! then
   EFFQ-OK @ ;

: EFFECT-DIN-N ( -- n )        EFFQ-DIN @ EFF-ROW-N ;      \ fixed din term count
: EFFECT-DOUT-N ( -- n )       EFFQ-DOUT @ EFF-ROW-N ;     \ fixed dout term count
: EFFECT-DIN-FAM ( i -- n )    EFFQ-DIN @ EFF-ROW-FAM ;    \ EFAM-* of din term i (top = 0)
: EFFECT-DOUT-FAM ( i -- n )   EFFQ-DOUT @ EFF-ROW-FAM ;   \ EFAM-* of dout term i (top = 0)

\ ---- ARM64 contract link: stable link from an emitted primitive/callable
\ contract to its checker primitive-effect (PES) row (dot
\ habu-link-arm64-contracts-8cca6cc1). A typed ARM64 routine effect schema binds
\ each contract to exactly ONE immutable axiom row WITHOUT depending on mutable
\ symbol-interning order or the whole trust-owner lifecycle. The link key is a
\ row's stable identity coordinates — its defining package (empty for a bare
\ PRIM:) and word spelling — resolved against the immutable PES table, together
\ with the row's identity FINGERPRINT so a mutated row is caught. Package-scoped
\ (PRIM-LINK:*, not a global prefix word) and read-only: it never mutates a row.
\
\   COUNT   ( pkg-a pkg-u name-a name-u -- n )        live PES rows for the key
\   RESOLVE ( pkg-a pkg-u name-a name-u -- bool )     true iff exactly one; latch it
\   FP      ( -- fp )                                 identity of the resolved row
\   CHECK   ( pkg-a pkg-u name-a name-u expect-fp -- bool )   the link is sound
\
\ Rejections: an unknown primitive or the wrong package interns to no live row's
\ symbol (COUNT 0); a duplicate spelling — an overloaded prim like `+`, or the
\ path0/PATH0 pair — has no single immutable row (COUNT > 1); a mutated row
\ disagrees with the contract's recorded expected fingerprint (FP mismatch). The
\ fingerprint folds the row's checker-exposed identity — declared din/dout arity,
\ the per-slot EFAM-* family of every din then dout term, and the PE-TRUSTED-ONLY
\ flag — into one cell (bit-packed, marker-led, exact for din+dout <= PL-ARITY-CAP;
\ a wider row fails closed rather than alias). RESOLVE latches the effect through
\ the shared query state above, so a consumer reads the linked row's effect with
\ the same EFFECT-DIN-N/… readers, and FP pins exactly that published family ABI.
package PRIM-LINK

private

variable LINK-IDX          \ PES index of the last uniquely-resolved row
variable LNK-CNT           \ active PES rows for the last scanned key
variable LNK-I  variable LNK-J
24 constant PL-ARITY-CAP   \ din+dout fitting the 62-bit fingerprint (each fam = 2 bits)

\ typed-local-lint: allow-bare-local - pa/na preserve ptr u8 roles.
: KEY-SYM ( ptr u8 n ptr u8 n -- n )  {: pa pu:n na nu:n :}   \ 0 = no such (pkg,spelling) row
   pu 0= if na nu CHECKER-GLOBAL-SYM? exit then
   pa pu na nu CHECKER-PUBLIC-SYM? ;

: SCAN ( n -- )   \ LNK-CNT = active PES rows for sym; LINK-IDX = the (unique) match
   0 LNK-CNT !  0 LINK-IDX !
   {: sym:n :}
   sym 0= if exit then
   0 LNK-I !
   begin LNK-I @ #PE @ < while
      LNK-I @ PE-ACTIVE? if
         LNK-I @ PE-SYM@ sym = if 1 LNK-CNT +!  LNK-I @ LINK-IDX ! then
      then
      LNK-I @ 1+ LNK-I !
   repeat ;

: PUT ( n n n -- n )  {: bits:n w:n :}  w lshift bits or ;

public

: COUNT ( ptr u8 n ptr u8 n -- n )   \ live PES rows sharing the (pkg,spelling) key
   KEY-SYM SCAN LNK-CNT @ ;

: RESOLVE ( ptr u8 n ptr u8 n -- bool )   \ true iff exactly one row; latch it + its effect
   KEY-SYM SCAN
   LNK-CNT @ 1 = if
      LINK-IDX @ PE-EFF@ E-PTR dup ER.DIN @ EFFQ-DIN ! ER.DOUT @ EFFQ-DOUT !
      RES-TRUE EFFQ-OK !  RES-TRUE
   else
      0 EFFQ-DIN !  0 EFFQ-DOUT !  RES-FALSE EFFQ-OK !  RES-FALSE
   then ;

: FP ( -- n )   \ identity fingerprint of the last RESOLVE'd row (call after true RESOLVE)
   EFFECT-DIN-N {: din:n :}  EFFECT-DOUT-N {: dout:n :}
   din dout + PL-ARITY-CAP > if
      s" prim-link: row arity exceeds fingerprint capacity" 76 die then
   1
   din 6 PUT
   dout 6 PUT
   LINK-IDX @ PE-TRUSTED-ONLY? if 1 else 0 then 1 PUT
   0 LNK-J !
   begin LNK-J @ din < while  LNK-J @ EFFECT-DIN-FAM 2 PUT  LNK-J @ 1+ LNK-J ! repeat
   0 LNK-J !
   begin LNK-J @ dout < while LNK-J @ EFFECT-DOUT-FAM 2 PUT LNK-J @ 1+ LNK-J ! repeat ;

: CHECK ( ptr u8 n ptr u8 n n -- bool )   \ sound link: one row whose identity matches
   {: fp:n :}
   RESOLVE if FP fp = else RES-FALSE then ;

;package

\ HIDX-DFR-SYNC ( -- ) : flush the cache when DFERS rewound below a cached defer
\ answer. Rollback frames restore DFER-END, so a cached flag whose scan reached a
\ now-retired tail must be dropped before the stale answer masks the rewind.
: HIDX-DFR-SYNC
   DFER-END @ HIDX-DFR-HI @ < IF HIDX-EPOCH+ THEN ;

: DFER-ENSURE ( n -- )
   DFER-CAP > IF s" checker: defer table full" 76 die THEN ;

: DFER-CUR ( -- ptr a )
   DFERS DFER-END @ + ;

: DFER-NEED ( -- n )
   DFER-END @ DFER-REC + CELL + ;

: DFER-TERM ( -- )
   0 DFERS DFER-END @ + ! ;

\ Scopes DO rewind DFER-END (item 3 made rollback restore it; the watermark below
\ exists precisely for that), so the deferred-target cache cannot assume later-wins
\ permanence: HIDX-DFR-SYNC / HIDX-DFR-DEP+ record the DFER-END a cached answer
\ depends on and flush it (epoch bump) when a rollback rewinds below that mark.
: DFER-ADD-FLAG ( ptr u8 n bool -- ) {: a:ptr u:n flag:bool :}
   HIDX-DFR-SYNC
   DFER-NEED DFER-ENSURE
   a u CHECKER-RECORD-SYM {: sym:n :}
   sym DFER-CUR DFER.SYM !
   flag DFER-CUR DFER.FLAG !
   DFER-END @ DFER-REC + DFER-END !
   DFER-TERM
   sym 0 <> HIDX-VALID @ and IF
      flag sym HIDX-DFR!
      DFER-END @ HIDX-DFR-DEP+
   THEN ;

: DFER-ADD ( ptr u8 n -- )
   RES-TRUE DFER-ADD-FLAG ;

: DFER-DELETE ( ptr u8 n -- )
   RES-FALSE DFER-ADD-FLAG ;

: DFER-NEXT ( ptr a -- ptr a )
   DFER-REC + ;

: DFER-FLAG@ ( ptr a -- bool )
   DFER.FLAG @ 0 <> ;

: DFER-SYM@ ( ptr a -- n )
   DFER.SYM @ ;

: DFER-END? ( ptr a -- bool )
   @ 0= ;

: DFER-MATCH-SYM? ( ptr a n -- bool ) {: rec:ptr sym:n :}
   rec DFER-SYM@ sym = ;

variable DFER-HIT
variable DFER-VALUE

variable DFER-POS

: DFER-SCAN-SYM ( n -- ) {: sym:n :}
   0 DFER-POS !
   begin DFERS DFER-POS @ + DFER-END? 0= while
      DFERS DFER-POS @ + sym DFER-MATCH-SYM? IF
         RES-TRUE DFER-HIT !
         DFERS DFER-POS @ + DFER-FLAG@ DFER-VALUE !
      THEN
      DFER-POS @ DFER-REC + DFER-POS !
   repeat ;

: DFER-FIND-SYM ( n -- bool ) {: sym:n :}
   sym 0= IF RES-FALSE EXIT THEN
   HIDX-ENSURE
   HIDX-DFR-SYNC
   sym HIDX-DFR@ {: cached:bool hit:bool :}
   hit IF cached EXIT THEN
   RES-FALSE DFER-HIT !
   RES-FALSE DFER-VALUE !
   sym DFER-SCAN-SYM
   DFER-HIT @ IF DFER-VALUE @ ELSE RES-FALSE THEN
   dup sym HIDX-DFR!
   DFER-END @ HIDX-DFR-DEP+ ;

: CHECKER-FIND-ACTIVE-DEFER ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u CHECKER-FIND-ACTIVE-SYM DFER-FIND-SYM ;

: CHECKER-RECORD-NAME ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   a u CHECKER-RECORD-SYM CHECKER-REC-SYM !
   a u ;

: CHECKER-DEFER ( ptr u8 n -- )
   CHECKER-RECORD-NAME DFER-ADD ;

: CHECKER-USIG-ADD ( ptr u8 n ptr u8 n -- ) {: sa:ptr su:n na:ptr nu:n :}
   sa su na nu CHECKER-RECORD-NAME USIG-ADD ;

: CHECKER-REC-NAME! ( ptr u8 n -- )
   CHECKER-RECORD-NAME CHECKER-REC-U ! CHECKER-REC-A ! ;

: CHECKER-REC-A@ ( -- ptr u8 )
   CHECKER-REC-A @ ;

: CHECKER-REC-U@ ( -- n )
   CHECKER-REC-U @ ;

: CHECKER-CERT-DUP? ( -- bool )
   CHK-CAND @ 0 <> IF RES-FALSE EXIT THEN
   CHECKER-REC-A@ CHECKER-REC-U@ CHECKER-FIND-USIG ;

: CHECKER-DEFINED? ( ptr u8 n -- bool )
   CHECKER-FIND-USIG ;

: CHECKER-DUP-DEFINITION ( -- )
   $4E throw ;

: CHECKER-USIG-CERT-ADD ( ptr u8 n ptr u8 n -- ) {: sa:ptr su:n na:ptr nu:n :}
   na nu CTOR-EXTEND?-XT IF E-CTOR-PROTECTED throw THEN
   na nu CHECKER-REC-NAME!
   CHECKER-CERT-DUP? IF CHECKER-DUP-DEFINITION THEN
   sa su CHECKER-REC-A@ CHECKER-REC-U@ USIG-ADD ;

$1000 constant LBUF-SIG-CAP
$7FFFFFFFFFFFFFFF constant LBUF-COUNT-MAX
create LBUF-SIG-BUF LBUF-SIG-CAP allot
variable LBUF-SIG-U
variable LBUF-SIG-I
variable LBUF-COUNT-N
variable LBUF-INFO-W

: LBUF-SIG-C, ( n -- ) {: c:n :}
   LBUF-SIG-U @ LBUF-SIG-CAP >= IF E-CHECKER-LAYOUT-BUFFER throw THEN
   c LBUF-SIG-BUF LBUF-SIG-U @ + c!
   LBUF-SIG-U @ 1 + LBUF-SIG-U ! ;

: LBUF-SIG-APP ( ptr u8 n -- ) {: a:ptr u:n :}
   0 LBUF-SIG-I !
   BEGIN LBUF-SIG-I @ u < WHILE
      a LBUF-SIG-I @ + c@ LBUF-SIG-C,
      LBUF-SIG-I @ 1 + LBUF-SIG-I !
   REPEAT ;

: CHECKER-LBUF-COUNT? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   u 0= IF RES-FALSE EXIT THEN
   0 LBUF-COUNT-N !  0 LBUF-SIG-I !
   BEGIN LBUF-SIG-I @ u < WHILE
      a LBUF-SIG-I @ + c@ {: c:n :}
      c 48 < c 58 >= or IF RES-FALSE EXIT THEN
      c 48 - {: d:n :}
      LBUF-COUNT-N @ LBUF-COUNT-MAX d - 10 / > IF RES-FALSE EXIT THEN
      LBUF-COUNT-N @ 10 * d + LBUF-COUNT-N !
      LBUF-SIG-I @ 1 + LBUF-SIG-I !
   REPEAT
   LBUF-COUNT-N @ 0 > ;

: CHECKER-LBUF-EXTENT? ( n n -- bool ) {: count:n width:n :}
   count 0 <= width 0 <= or IF RES-FALSE EXIT THEN
   count LBUF-COUNT-MAX width / > IF RES-FALSE EXIT THEN
   count width * LBUF-COUNT-MAX CELL / <= ;

: CHECKER-LBUF-SIG$ ( ptr u8 n -- ptr u8 n ) {: type:ptr typeu:n :}
   0 LBUF-SIG-U !
   s" n -- ptr " LBUF-SIG-APP
   type typeu LBUF-SIG-APP
   LBUF-SIG-BUF LBUF-SIG-U @ ;

: CHECKER-DEFLAYOUT-BUFFER
   ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: type:ptr typeu:n count:ptr countu:n name:ptr nameu:n :}
   name nameu CHECKER-LBUF-NAME-GUARD
   type typeu CHECKER-LAYOUT-INFO 0= IF 2drop E-CHECKER-LAYOUT-BUFFER throw THEN
   nip LBUF-INFO-W !
   count countu CHECKER-LBUF-COUNT? 0= IF E-CHECKER-LAYOUT-BUFFER throw THEN
   LBUF-COUNT-N @ LBUF-INFO-W @ CHECKER-LBUF-EXTENT? 0= IF E-CHECKER-LAYOUT-BUFFER throw THEN
   type typeu CHECKER-LBUF-SIG$ name nameu CHECKER-USIG-CERT-ADD ;

\ Checker-side registration for the TYPED-BUFFER / TYPED-VARIABLE gate path
\ (verify-source RECORD-TYPED-*). Mirrors CHECKER-DEFLAYOUT-BUFFER but gates on
\ the broader CHECKER-STORAGE-INFO admissibility (typed pointers included) and
\ publishes the convenience accessor's usig. A TYPED-BUFFER accessor is
\ `( n -- ptr <type> )`; a TYPED-VARIABLE accessor is `( -- ptr <type> )`.
: CHECKER-STORAGE-VAR-SIG$ ( ptr u8 n -- ptr u8 n ) {: type:ptr typeu:n :}
   0 LBUF-SIG-U !
   s" -- ptr " LBUF-SIG-APP
   type typeu LBUF-SIG-APP
   LBUF-SIG-BUF LBUF-SIG-U @ ;

: CHECKER-DEFTYPED-BUFFER
   ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: type:ptr typeu:n count:ptr countu:n name:ptr nameu:n :}
   name nameu CHECKER-LBUF-NAME-GUARD
   type typeu CHECKER-STORAGE-INFO 0= IF drop E-CHECKER-LAYOUT-BUFFER throw THEN
   LBUF-INFO-W !
   count countu CHECKER-LBUF-COUNT? 0= IF E-CHECKER-LAYOUT-BUFFER throw THEN
   LBUF-COUNT-N @ LBUF-INFO-W @ CHECKER-LBUF-EXTENT? 0= IF E-CHECKER-LAYOUT-BUFFER throw THEN
   type typeu CHECKER-LBUF-SIG$ name nameu CHECKER-USIG-CERT-ADD ;

: CHECKER-DEFTYPED-VARIABLE
   ( ptr u8 n ptr u8 n -- )
   {: type:ptr typeu:n name:ptr nameu:n :}
   name nameu CHECKER-LBUF-NAME-GUARD
   type typeu CHECKER-STORAGE-INFO 0= IF drop E-CHECKER-LAYOUT-BUFFER throw THEN
   drop                                    \ single cell: extent is one slot, width unused here
   type typeu CHECKER-STORAGE-VAR-SIG$ name nameu CHECKER-USIG-CERT-ADD ;

: CHECKER-USIG-CERT-CURRENT ( ptr u8 n -- ) {: na:ptr nu:n :}
   na nu CHECKER-REC-NAME!
   CHECKER-CERT-DUP? IF CHECKER-DUP-DEFINITION THEN
   BROW @ DCUR @ 0 0 RES-FALSE E-ADD-EFFECT ;

\ Control-effect flags are append-only and later-wins so redefinitions can clear
\ stale metadata. CTL-DEAD means a call has no normal continuation. CTL-THROW
\ means a call may reach a catchable throw edge. CTL-BARRIER (M5) means the call
\ is a block collective whose emitter contains bar.sync, so it is only sound under
\ block-uniform control (rejected inside an open control frame — see BARRIER-CUR?).
1 constant CTL-DEAD
2 constant CTL-THROW
4 constant CTL-BARRIER
$10000 constant NORET-INIT-CAP

0 constant NORET-SYM-CELL
1 constant NORET-FLAG-CELL
$0 constant NORET-SYM-OFF
$8 constant NORET-FLAG-OFF
$10 constant NORET-ENTRY
$8 constant NORET-ENTRY-ALIGN
0 constant NORET-ENTRY-PTR-MASK

: NORET.SYM ( ptr a -- ptr a ) NORET-SYM-OFF + ;
: NORET.FLAG ( ptr a -- ptr a ) NORET-FLAG-OFF + ;

: NORET-LAYOUT-ASSERT ( -- )
   NORET-SYM-CELL cells NORET-SYM-OFF CHECKER-RECORD-LAYOUT=
   NORET-FLAG-CELL cells NORET-FLAG-OFF CHECKER-RECORD-LAYOUT=
   NORET-FLAG-OFF CELL + NORET-ENTRY CHECKER-RECORD-LAYOUT=
   CELL NORET-ENTRY-ALIGN CHECKER-RECORD-LAYOUT=
   NORET-ENTRY NORET-ENTRY-ALIGN mod 0 CHECKER-RECORD-LAYOUT=
   NORET-ENTRY-PTR-MASK 0 CHECKER-RECORD-LAYOUT= ;

NORET-LAYOUT-ASSERT

create NORET-BOOT NORET-INIT-CAP allot
variable NORET-P   variable NORET-CAP-U   variable NORET-END
NORET-BOOT NORET-P !   NORET-INIT-CAP NORET-CAP-U !   0 NORET-END !   0 NORET-BOOT !
variable NORET-POS   variable NORET-FLAG
variable NORET-GROW-CAP   variable NORET-GROW-NEXT

: NORETS ( -- ptr u8 ) NORET-P @ ;

: NORET-CELL ( n -- ptr a ) {: off:n :}
   off NORET-ENTRY-ALIGN 1 - and 0 <> IF s" checker: unaligned no-return cell" 76 die THEN
   NORETS off + ;

: NORET-TERM ( -- )
   0 NORET-END @ NORET-CELL ! ;

: NORET-RESTORE-END ( n -- )
   NORET-END !
   NORET-TERM ;

: NORET-RESET ( -- )
   NORET-BOOT NORET-P !
   NORET-INIT-CAP NORET-CAP-U !
   0 NORET-END !
   0 0 NORET-CELL !
   0 NORET-GROW-CAP !
   0 NORET-GROW-NEXT ! ;

: NORET-BOOT? ( -- bool )
   NORETS NORET-BOOT = ;

: NORET-SNAPSHOT-CAP ( -- )
   NORET-END @ CELL + NORET-INIT-CAP > IF s" checker: no-return snapshot too large" 76 die THEN ;

: NORET-SNAPSHOT-PERSIST ( -- )
   NORET-SNAPSHOT-CAP
   NORET-BOOT? 0= IF NORETS NORET-BOOT NORET-END @ CELL + USIGS-COPY THEN
   NORET-BOOT NORET-P !
   NORET-INIT-CAP NORET-CAP-U !
   0 NORET-GROW-CAP !
   0 NORET-GROW-NEXT ! ;

: NORET-GROW {: need :}
   need NORET-CAP-U @ 2 * max USIGS-ROUND-CAP NORET-GROW-CAP !
   NORET-GROW-CAP @ USIGS-ALLOC NORET-GROW-NEXT !
   NORETS NORET-GROW-NEXT @ NORET-END @ CELL + USIGS-COPY
   NORET-GROW-NEXT @ NORET-P !
   NORET-GROW-CAP @ NORET-CAP-U ! ;

: NORET-ENSURE {: need :}
   need NORET-CAP-U @ <= IF exit THEN
   need NORET-GROW ;

\ TV-SNAP-RESET ( -- ) : repoint the growable TV arena at its boot buffers so
\ no process-local mmap address is persisted. The maps are transient scratch
\ (rebuilt per definition), so no live content is lost, but the boot buffers
\ may still hold stale pre-grow entries — so fully re-establish a clean map
\ state: zero FV (a grown FV would drive an out-of-boot-bounds TV-RESET), and
\ UNBOUND-clear the high-water-reset EC maps (a zeroed EC-TV-HW would otherwise
\ leave those stale boot entries uncleared, corrupting the next compare).
: TV-SNAP-RESET ( -- )
   TV-ARENA-BOOT
   MAXTV-INIT TV-CAP !
   0 FV !
   TVT-BOOT 0 MAXTV-INIT ARENA-CELLS-UNBOUND   \ FV=0 means TV-RESET clears nothing,
   RVT-BOOT 0 MAXTV-INIT ARENA-CELLS-UNBOUND   \ so unbind the boot pool ourselves
   TVK-BOOT 0 MAXTV-INIT ARENA-CELLS-ZERO      \ and reset var kinds to TVK-ANY
   EC-TV MAXTV-INIT E-MAP-CLEAR   0 EC-TV-HW !
   EC-RV MAXTV-INIT E-MAP-CLEAR   0 EC-RV-HW ! ;

\ DECOUPLED-ARENA-SNAP-RESET ( -- ) : repoint the per-definition scratch arenas
\ (push/quot/ptr/atom/param) at their boot buffers and restore their init caps
\ so no grown mmap address is persisted. Their counters reset in NEW, so no live
\ content is lost.
: DECOUPLED-ARENA-SNAP-RESET ( -- )
   SPA-BOOT SPA-P !     MAXPUSH-INIT SPA-CAP !
   PTRA-BOOT PTRA-P !   MAXPTR-INIT PTR-CAP !
   QEA-BOOT QEA-P !     QXDA-BOOT QXDA-P !   QXRA-BOOT QXRA-P !
   QXHA-BOOT QXHA-P !   QXNA-BOOT QXNA-P !   MAXQE-INIT QE-CAP !
   ATOMA-BOOT ATOMA-P !   ATOMU-BOOT ATOMU-P !   ATOMK-BOOT ATOMK-P !   MAXATOM-INIT ATOM-CAP !
   PARAMA-BOOT PARAMA-P !   PARAMU-BOOT PARAMU-P !   PARAMC-BOOT PARAMC-P !
   PARAMFAM-BOOT PARAMFAM-P !   PARAMOFF-BOOT PARAMOFF-P !   MAXPARAM-INIT PARAM-CAP !
   PARGP-BOOT PARGP-P !   PARG-INIT PARG-CAP-V !   \ flat per-param arg pool (resets in NEW)
   PARAM-SCR-BOOT PARAM-SCR-P !   PARAM-SCR-INIT PARAM-SCR-CAP-V !   \ reentrant parse scratch
   VRI-AK-BOOT VRI-AK-P !   VRI-AK-INIT VRI-AK-CAP-V !     \ transient inst scratch
   TRAIL-BOOT TRAIL-P !   TRAIL-INIT TRAIL-CAP !   TRAIL-RESET ; \ unification trail

\ --- registry snapshot persist. The append-only registries (CT/VREC/SYMS) must
\ survive into a built image (later checked loads reference persisted signatures).
\ While a store is still on its baked boot buffer it is captured with the data
\ region — nothing to do. A grown store lives in process-local mmap, so bake it
\ into fresh image DATA (here-allot + copy), USIGS/NORET-style. Record/node arrays
\ hold pointers into their string pool; the string pool is persisted last and its
\ relocation delta rebases those pointers in the just-persisted arrays.
variable REG-PERSIST-DELTA

\ REG-PVAR@/REG-PVAR! read/write a persisted pointer slot through a cell-indexed
\ ptr-field view so the stored pointer keeps its nested ptr role.
: REG-PVAR@ ( ptr a -- ptr a )
   0 ptr-field @ ;

: REG-PVAR! ( ptr a ptr a -- )
   0 ptr-field ! ;

: REG-PERSIST-BUF ( ptr a ptr a n -- bool ) {: pvar:ptr boot:ptr bytes:n :}
   pvar REG-PVAR@ boot = IF RES-FALSE EXIT THEN            \ not grown: boot buffer is baked DATA
   pvar REG-PVAR@ {: old:ptr :}
   here {: dst:ptr :}
   bytes allot
   old dst bytes USIGS-COPY
   dst pvar REG-PVAR!
   dst old - REG-PERSIST-DELTA !
   RES-TRUE ;

: CT-SNAPSHOT-PERSIST ( -- )
   CT-CAP-V @ cells {: ab:n :}
   CT-NAME-A-P CT-NAME-A-BOOT ab REG-PERSIST-BUF drop
   CT-NAME-U-P CT-NAME-U-BOOT ab REG-PERSIST-BUF drop
   CT-CLASS-P CT-CLASS-BOOT ab REG-PERSIST-BUF drop
   CT-WIDTH-P CT-WIDTH-BOOT ab REG-PERSIST-BUF drop
   CT-SIGN-P CT-SIGN-BOOT ab REG-PERSIST-BUF drop
   CT-STR-P CT-STR-BOOT CT-STR-U @ REG-PERSIST-BUF IF
      CT-STR-U @ CT-STR-CAP-V !
      REG-PERSIST-DELTA @ CT-STR-REBASE
   THEN ;

: VREC-SNAPSHOT-PERSIST ( -- )
   VREC-CAP-V @ cells {: rb:n :}
   VREC-NAME-A-P VREC-NAME-A-BOOT rb REG-PERSIST-BUF drop
   VREC-NAME-U-P VREC-NAME-U-BOOT rb REG-PERSIST-BUF drop
   VREC-START-P VREC-START-BOOT rb REG-PERSIST-BUF drop
   VREC-COUNT-P VREC-COUNT-BOOT rb REG-PERSIST-BUF drop
   VREC-TVN-P VREC-TVN-BOOT rb REG-PERSIST-BUF drop
   VREC-RVN-P VREC-RVN-BOOT rb REG-PERSIST-BUF drop
   VREC-FIELDS-P VREC-FIELDS-BOOT VREC-FIELD-CAP-V @ cells REG-PERSIST-BUF drop
   VREC-NODE-CAP-V @ cells {: nb:n :}
   VRN-TAG-P VRN-TAG-BOOT nb REG-PERSIST-BUF drop
   VRN-A-P VRN-A-BOOT nb REG-PERSIST-BUF drop
   VRN-B-P VRN-B-BOOT nb REG-PERSIST-BUF drop
   VRN-C-P VRN-C-BOOT nb REG-PERSIST-BUF drop
   VRN-D-P VRN-D-BOOT nb REG-PERSIST-BUF drop
   VRN-E-P VRN-E-BOOT nb REG-PERSIST-BUF drop
   VRN-F-P VRN-F-BOOT nb REG-PERSIST-BUF drop
   VRN-G-P VRN-G-BOOT nb REG-PERSIST-BUF drop
   VRN-H-P VRN-H-BOOT nb REG-PERSIST-BUF drop
   VNARG-P VNARG-BOOT VNARG-CAP-V @ cells REG-PERSIST-BUF drop
   VREC-STR-P VREC-STR-BOOT VREC-STR-U @ REG-PERSIST-BUF IF
      VREC-STR-U @ VREC-STR-CAP-V !
      REG-PERSIST-DELTA @ VREC-STR-REBASE
   THEN ;

: SYM-SNAPSHOT-PERSIST ( -- )      \ HIDX is dropped by HIDX-RESET; rebuilt on restore
   SYMS-P SYMS-BOOT SYM-CAP-V @ SYM-REC * REG-PERSIST-BUF drop
   SYM-STR-P SYM-STR-BOOT SYM-STR-U @ REG-PERSIST-BUF IF
      SYM-STR-U @ SYM-STR-CAP-V !
      REG-PERSIST-DELTA @ SYM-STR-REBASE
   THEN ;

\ Friend-only extension hook: the package-scoped TFAM/SCHEMA registries live in
\ files loaded after checker.f (src/core/type-schema.f, src/core/type-family.f),
\ so they cannot be named here. They install their combined persist word into
\ this cell; a 0 cell keeps the call a no-op before they load. Same late-binding
\ shape the checker already uses for the source-check hook (`set-check`).
defer REG-EXT-PERSIST-XT ( -- )
defer REG-SCRATCH-SNAP-XT ( -- )
defer REG-LATE-SCRATCH-SNAP-XT ( -- )
\ registry-not-loaded defaults: no extension registry to persist / no scratch to
\ snapshot before type-schema.f / type-family.f / render.f install the real words.
: REG-EXT-DEFAULTS ( -- )
   [: ;] is REG-EXT-PERSIST-XT
   [: ;] is REG-SCRATCH-SNAP-XT
   [: ;] is REG-LATE-SCRATCH-SNAP-XT ;
REG-EXT-DEFAULTS

: CHECKER-SNAPSHOT-PREPARE ( -- )
   TOKBUF-RESET
   HIDX-RESET
   TV-SNAP-RESET
   DECOUPLED-ARENA-SNAP-RESET
   CT-SNAPSHOT-PERSIST
   VREC-SNAPSHOT-PERSIST
   SYM-SNAPSHOT-PERSIST
   USIGS-SNAPSHOT-PERSIST
   HIDX-EFF-BASE-CLEAR
   NORET-SNAPSHOT-PERSIST
   REG-EXT-PERSIST-XT
   REG-SCRATCH-SNAP-XT
   REG-LATE-SCRATCH-SNAP-XT ;

: NORET-REC ( -- ptr a )
   NORET-END @ NORET-CELL ;

: NORET-FLAG@ ( ptr a -- n )
   NORET.FLAG @ ;

: NORET-SYM@ ( ptr a -- n )
   NORET.SYM @ ;

: NORET-NEXT ( ptr a -- ptr a )
   NORET-ENTRY + ;

: NORET-END? ( ptr a -- bool )
   @ 0= ;

\ HIDX-CTL-SYNC ( -- ) : flush the cache when NORETS rewound below a cached
\ dependency. The store swap paths (persist/reset) keep values or rewind END.
: HIDX-CTL-SYNC
   NORET-END @ HIDX-CTL-HI @ < IF HIDX-EPOCH+ THEN ;

\ NORET-ADD syncs first for the same reason as E-REC-START: it is the only
\ NORETS appender, and appending over a rewound tail must flush stale flags
\ before the new entry masks the rewind.
: NORET-ADD-SYM {: sym:n flag:n :}
   HIDX-CTL-SYNC
   NORET-END @ NORET-ENTRY + CELL + NORET-ENSURE
   sym NORET-REC NORET.SYM !
   flag NORET-REC NORET.FLAG !
   NORET-END @ NORET-ENTRY + NORET-END !
   NORET-TERM
   sym 0 <> HIDX-VALID @ and IF
      flag sym HIDX-CTL!
      NORET-END @ HIDX-CTL-DEP+
   THEN ;

: NORET-ADD {: a:ptr u:n flag:n :}
   a u CHECKER-RECORD-SYM flag NORET-ADD-SYM ;

: CHECKER-UNDEFINE ( ptr u8 n -- ) {: a:ptr u:n :}
   a u CHECKER-UNDEFINE-GUARD
   a u CHECKER-RECORD-NAME {: name:ptr nameu:n :}
   name nameu USIG-DELETE
   name nameu DFER-DELETE
   name nameu 0 NORET-ADD ;

: CHECKER-DEFLINEAR ( ptr u8 n -- )
   CT-ADD-LINEAR ;

variable NORET-FMEND

\ NORET-SCAN-SYM ( n -- ) : NORET-FLAG = last flag for sym (later wins);
\ NORET-FMEND = end offset of the last matching entry (cache dependency).
: NORET-SCAN-SYM {: sym:n :}
   0 NORET-FLAG !
   0 NORET-FMEND !
   0 NORET-POS !
   BEGIN NORETS NORET-POS @ + NORET-END? 0= WHILE
      NORETS NORET-POS @ + NORET-SYM@ sym = IF
         NORETS NORET-POS @ + NORET-FLAG@ NORET-FLAG !
         NORET-POS @ NORET-ENTRY + NORET-FMEND !
      THEN
      NORETS NORET-POS @ + NORET-NEXT NORETS - NORET-POS !
   REPEAT ;

: CTL-FLAGS-SYM {: sym:n :}
   sym 0= IF 0 EXIT THEN
   HIDX-ENSURE
   HIDX-CTL-SYNC
   sym HIDX-CTL@ IF EXIT THEN
   drop
   sym NORET-SCAN-SYM
   NORET-FLAG @ sym HIDX-CTL!
   NORET-FMEND @ HIDX-CTL-DEP+
   NORET-FLAG @ ;

: CTL-FLAGS {: a:ptr u:n :}
   a u CHECKER-FIND-ACTIVE-SYM CTL-FLAGS-SYM ;

\ M5: OR CTL-BARRIER into a symbol's control flags (append later-wins, preserving
\ any dead/throw already recorded), then arm the E-ADD-EFFECT hook.
: PTX-BARRIER-SET ( n -- ) {: sym:n :}
   sym CTL-FLAGS-SYM CTL-BARRIER or sym swap NORET-ADD-SYM ;
: PTX-BARRIER-SET-INSTALL ( -- ) [: PTX-BARRIER-SET ;] is PTX-BARRIER-SET-XT ;
PTX-BARRIER-SET-INSTALL

\ M5b explicit per-word barrier declaration. Some block-uniform-barrier words do
\ not carry the tile->uniform (or committed->ready) SHAPE the E-ADD-EFFECT
\ detector keys on: BLOCK-MAX-SELECT emits bar.sync internally but RETURNS a tile
\ (a masked scatter of the arg-max cotangent), so the structural detector misses
\ it. Mark such a word BY NAME after its definition; it then flags CTL-BARRIER and
\ composes at the same BARRIER-CUR?/ALL-CF-UNIFORM? choke as the shape-detected
\ collectives — no parallel registry, the same NORET control-flag path. Unknown
\ names fail closed so a typo can never silently leave a barrier unmarked.
: PTX-BARRIER! ( ptr u8 n -- ) {: a:ptr u:n :}
   a u CHECKER-FIND-ACTIVE-SYM dup 0= IF
      drop s" PTX-BARRIER!: unknown word" 76 die
   ELSE PTX-BARRIER-SET THEN ;

\ CURSYM: the resolved symbol of the current body token (set by DO-TOK, 0 for
\ literals/definers/memory tokens), so the throw/dead classification after the
\ effect application reuses one symbol resolution instead of re-scanning.
variable CURSYM
0 CURSYM !

: CTL-FLAGS-CUR ( -- n )
   CURSYM @ CTL-FLAGS-SYM ;

: DEAD-CUR? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" die" CORE-STR= IF RES-TRUE EXIT THEN
   a u s" throw" CORE-STR= IF RES-TRUE EXIT THEN
   CTL-FLAGS-CUR CTL-DEAD and 0 <> ;

: THROW-CUR? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" throw" CORE-STR= IF RES-TRUE EXIT THEN
   CTL-FLAGS-CUR CTL-THROW and 0 <> ;

\ M5: current token is a block collective (CTL-BARRIER, set by E-ADD-EFFECT). The
\ classification helpers (ROW-HAS-FAM?/PTX-BARRIER-ROWS?) live above E-ADD-EFFECT.
: BARRIER-CUR? ( -- bool )
   CTL-FLAGS-CUR CTL-BARRIER and 0 <> ;

\ Tokens whose execution from a checked body is unsound whatever their
\ admitting row says: the type-DSL block openers (sumtype.f, PRIM: axioms)
\ and the roles.f nominal definers (checked words with certified ( -- )
\ usigs) parse the live input stream and mutate the type registry (or throw
\ at EOF) — side effects no effect row expresses (dots
\ habu-checker-in-body-af7cf855, habu-checker-deftype-deflinear-8e9d1dc5).
\ DO-TOK1 consults this set before DO-TOK's usig/prim lookup, so BARE
\ spellings reject in bodies via both admission paths; qualified spellings
\ cannot arise because EXPORT refuses to mint an alias for these names
\ (E-EXPORT-UNSAFE below). CI compares: DO-TOK1 passes folded tokens,
\ EXPORT-RESOLVE passes raw source tails.
: UNSAFE-TOK? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" evaluate" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" trust" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" ptx-barrier!" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" layout-buffer" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" defer-layout-buffer" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" typed-buffer" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" typed-variable" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" newtype" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" sumtype" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" enum" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" product" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" deflinear" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" value-record" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" cast:" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" set-check" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" set-preflight" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" parse-imm" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" postpone" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" compile," CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" immediate" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" [" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" ]" CORE-STR=CI ;

\ --- retired-token set (dots habu-own-product-field-86660116 /
\ habu-type-field-owner-619ec6b5) --------------------------------------------
\ The raw GLOBAL product-field lifecycle words were retired when package
\ TYPE-FIELD-OWNER became the sole authority over field transactions. Deleting
\ them from the dictionary is NOT the enforcement: an undefined token only makes
\ CHECK return 1 (uncheckable), and any later source can define a global word
\ with the same spelling and get it admitted through an ordinary signature. The
\ rows below make the rejection structural instead — DO-TOK1 consults them
\ BEFORE DO-TOK's usig/prim lookup, so a bare global use rejects with verdict 0
\ whatever a future admitting row might say.
\
\ The invariant is about the retired GLOBAL words, not about the spelling. A
\ package that owns its own word with one of these tails owns a DIFFERENT word,
\ and it must stay live and certifiable: tools/lint/diff-frame.f has had a
\ private `1 constant PF-ADD` ("parser form: add") inside package DIFF-FRAME
\ since long before this retirement, and a context-free blocklist made that file
\ fail to load. So RETIRED-GLOBAL? matches the spelling first (exact and
\ case-folded — DO-TOK1 passes folded tokens — never a prefix, so PF-COMMIT-N
\ and PF-NO-VARIANT are untouched), then asks the ordinary resolver where the
\ token actually binds. It rejects only at the two positions that would have
\ reached the retired raw global: the token resolves at global position, or it
\ resolves nowhere at all. An open-package or used-package hit is someone else's
\ word and passes straight through.
: RETIRED-TOK? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" pf-begin" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" pf-add" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" pf-publish" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" pf-release" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" pf-finalize" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" pf-commit" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" pf-rollback" CORE-STR=CI ;

\ RETIRED-GLOBAL? (the position test) lives beside REJECT-RETIRED below, where
\ the locals table is already in scope.

\ --- unsafe-symbol set (dot habu-checker-unsafety-as-1c537c1f) ----------------
\ Unsafety is a property of the WORD, not its spelling. UNSAFE-TOK? above keeps
\ the canonical name list for the DO-TOK1 body reject and for the symbol-less
\ engine/syntax tokens ([, ], postpone, immediate, compile,, evaluate, trust,
\ set-check, set-preflight) that never intern a checker symbol. The
\ definers/openers (deflinear, value-record, sumtype, enum, product,
\ newtype, layout-buffer)
\ DO intern a symbol, and those are exactly the words that carry a checked
\ signature or a prim axiom — so they are the only ones an EXPORT alias could
\ mint. UNSAFE-SET-SEAL interns their symbol IDs here at seal time; the EXPORT
\ gate (EXPORT-RESOLVE) then refuses to re-export any of them BY IDENTITY: a
\ qualified reference (PKG:DEFLINEAR), a differently-spelled reference, or any
\ future rename that still resolves to the same symbol is rejected, not just the
\ bare canonical spelling the tail-name check catches. Storing IDs (not
\ pointers) keeps the set snapshot-stable; it is monotonic and never rolled back
\ because these are permanent engine-prefix words above every checking watermark.
32 constant UNSAFE-SYM-CAP
create UNSAFE-SYMS UNSAFE-SYM-CAP cells allot
variable UNSAFE-SYM-N
0 UNSAFE-SYM-N !

: UNSAFE-SYM-AT ( n -- ptr a ) {: i:n :}
   UNSAFE-SYMS i cells + ;

: UNSAFE-SYM? ( n -- bool ) {: sym:n :}
   sym 0= IF RES-FALSE EXIT THEN
   0 BEGIN dup UNSAFE-SYM-N @ < WHILE
      dup UNSAFE-SYM-AT @ sym = IF drop RES-TRUE EXIT THEN
      1 +
   REPEAT drop RES-FALSE ;

: UNSAFE-SYM-ADD ( n -- ) {: sym:n :}
   sym 0= IF EXIT THEN
   sym UNSAFE-SYM? IF EXIT THEN
   UNSAFE-SYM-N @ UNSAFE-SYM-CAP >= IF
      s" checker: unsafe-symbol set full" 76 die
   THEN
   sym UNSAFE-SYM-N @ UNSAFE-SYM-AT !
   UNSAFE-SYM-N @ 1 + UNSAFE-SYM-N ! ;

: UNSAFE-NAME-ADD ( ptr u8 n -- )
   CHECKER-FIND-ACTIVE-SYM UNSAFE-SYM-ADD ;

\ Seal the aliasable unsafe words into the identity set. Called once after every
\ definer/opener is interned (end of roles.f). Idempotent: UNSAFE-SYM-ADD dedups,
\ and an unresolved name (symbol 0) is skipped.
: UNSAFE-SET-SEAL ( -- )
   s" deflinear"     UNSAFE-NAME-ADD
   s" value-record"  UNSAFE-NAME-ADD
   s" cast:"         UNSAFE-NAME-ADD
   s" sumtype"       UNSAFE-NAME-ADD
   s" enum"          UNSAFE-NAME-ADD
   s" product"       UNSAFE-NAME-ADD
   s" newtype"    UNSAFE-NAME-ADD
   s" layout-buffer" UNSAFE-NAME-ADD
   s" defer-layout-buffer" UNSAFE-NAME-ADD
   s" typed-buffer"  UNSAFE-NAME-ADD
   s" typed-variable" UNSAFE-NAME-ADD ;

\ --- EXPORT: alias an existing word's checked effect under its own tail -----
\ (dot habu-compiler-pkg-re-688212c1). `EXPORT NAME` in an open package section
\ publishes NAME's effect under NAME's tail in the CURRENT package scope: one
\ word, two names. The alias is a FRESH scheme copy — E-INST instantiates the
\ source record into working terms, E-ADD-EFFECT re-records them (alpha-
\ equivalent) under the new sym — plus the source's defer flag and control-
\ effect flags. Rollback safety is inherited: the alias's SYM/USIG/DFER/NORET
\ rows sit under the standard RBF-PUSH/POP watermarks. Fail-closed:
\ - E-EXPORT-NO-PACKAGE: no open package (outside a package there is no target
\   scope, and a global re-export would un-namespace a package word);
\ - E-EXPORT-SEALED: source qualified into a sealed system package (checker
\   mirror of the native RESTAB set, latch-gated like the engine seal guards);
\ - E-EXPORT-PRIM: source resolves only to a primitive axiom (prims may be
\   overloaded; copying the first row would silently narrow the effect);
\ - E-EXPORT-UNDEFINED: source has no checked signature (undefined, private in
\   a closed package — qualified lookup is public-only — or uncertified);
\ - E-EXPORT-UNSAFE: the source is an unsafe word, caught two ways — its tail is
\   an UNSAFE-TOK? name (openers, roles.f definers, trust/evaluate class), and
\   its resolved symbol is in the UNSAFE-SYM? identity set (dot
\   habu-checker-unsafety-as-1c537c1f). Either way the alias would give the
\   unsafe word a second spelling that escapes the name-keyed body reject; the
\   symbol leg rejects by IDENTITY so a qualified or renamed reference that
\   resolves to the same symbol cannot launder it past this gate;
\ - duplicate tail in the current section (CHECKER-CERT-DUP? -> $4E).
\ INTO a generated ctor package is structurally unreachable: CHECKER-PACKAGE
\ rejects opening one (E-CTOR-PROTECTED), so the current package can never be
\ a ctor package. Re-export FROM a ctor package stays allowed by design
\ (generated words are closed-but-callable).
7112 constant E-EXPORT-NO-PACKAGE
7113 constant E-EXPORT-UNDEFINED
7114 constant E-EXPORT-SEALED
7115 constant E-EXPORT-PRIM
7120 constant E-EXPORT-UNSAFE    \ 7116-7119 = sumtype.f E-TDECL block

\ CAST: legality rejects (dot habu-checked-cast-primitive). A cast declares a
\ retype between two single-cell machine types; the gate below refuses anything
\ wider than that class (7121-7128 = layout-buffer/type-family blocks).
7129 constant E-CAST-ARITY    \ sig is not exactly one input and one output term
7130 constant E-CAST-CLASS    \ in/out is not a single retype-eligible machine cell
7131 constant E-CAST-FAM      \ in/out names an undeclared family/type

\ sealed system-package names: checker mirror of the native RESTAB table
\ (src/habu/habu2.f) — foundational and stable, like CK-SEAL-LATCH-OFF.
: EXPORT-SEAL-GUARD ( ptr u8 n -- ) {: a:ptr u:n :}
   data-base CK-SEAL-LATCH-OFF + @ 0= IF EXIT THEN
   a u CHECKER-QUALIFIED? 0= IF EXIT THEN
   CHECKER-QPKG$ CHECKER-SEALED-PKG? IF E-EXPORT-SEALED throw THEN ;

: EXPORT-TAIL$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   a u CHECKER-QUALIFIED? IF CHECKER-QTAIL$ EXIT THEN
   a u ;

\ PRIM-FIRST-IDX, not PRIM-FIRST-SYM: the latter returns an effect OFFSET,
\ and the first PES row legitimately lives at USIGS offset 0.
: EXPORT-RESOLVE ( ptr u8 n -- ) {: a:ptr u:n :}
   a u EXPORT-TAIL$ UNSAFE-TOK? IF E-EXPORT-UNSAFE throw THEN
   a u CHECKER-FIND-ACTIVE-SYM UNSAFE-SYM? IF E-EXPORT-UNSAFE throw THEN
   a u CHECKER-FIND-ACTIVE-SIG
   FEP-HIT? IF EXIT THEN
   a u CHECKER-FIND-ACTIVE-SYM PRIM-FIRST-IDX 0 <> IF E-EXPORT-PRIM throw THEN
   E-EXPORT-UNDEFINED throw ;

\ EXPORT-EFF-INST ( ptr a -- n n n n bool ) : instantiate the source record's
\ rows into fresh working terms (din dout rin rout hasr, E-ADD-EFFECT intake).
: EXPORT-EFF-INST ( ptr a -- n n n n bool ) {: h:ptr :}
   h E-INST-RESET
   h ER.DIN @ E-INST
   h ER.DOUT @ E-INST
   h ER.HASR @ 0 <> IF
      h ER.RIN @ E-INST
      h ER.ROUT @ E-INST
      RES-TRUE EXIT
   THEN
   0 0 RES-FALSE ;

: EXPORT-RECORD ( ptr u8 n -- ) {: ta:ptr tu:n :}
   ta tu CHECKER-REC-NAME!
   CHECKER-CERT-DUP? IF CHECKER-DUP-DEFINITION THEN ;

: EXPORT-META-COPY ( ptr u8 n -- ) {: a:ptr u:n :}
   a u CHECKER-FIND-ACTIVE-DEFER IF a u EXPORT-TAIL$ DFER-ADD THEN
   a u CTL-FLAGS {: ctl:n :}
   ctl 0 <> IF a u EXPORT-TAIL$ ctl NORET-ADD THEN ;

: CHECKER-EXPORT ( ptr u8 n -- ) {: a:ptr u:n :}
   CHECKER-PACKAGE-ACTIVE? 0= IF E-EXPORT-NO-PACKAGE throw THEN
   a u EXPORT-SEAL-GUARD
   a u EXPORT-RESOLVE
   NEW
   FEP-OFF@ 1 - E-PTR EXPORT-EFF-INST
   a u EXPORT-TAIL$ EXPORT-RECORD
   E-ADD-EFFECT
   a u EXPORT-META-COPY ;

\ Trial save/restore: a prim-overload trial saves the scalar cursors below and the
\ trail height (SV-TRAIL); var bindings are undone via the unification trail (top).
variable SV-FV    variable SV-SPN   variable SV-QEN   variable SV-PTRN
variable SV-OK    variable SV-DCUR  variable SV-RCUR  variable SV-UNCK
variable SV-FSET  variable SV-DEXP  variable SV-DACT  variable SV-DF-ACT  variable SV-DF-EXP
variable SV-DVAR  variable SV-DPOS
variable SV-SGBAD
variable SV-SGBAD-A  variable SV-SGBAD-U  variable SV-SGBAD-KIND
variable SV-SGBAD-AR-DECL  variable SV-SGBAD-AR-GOT
variable SV-SGSEEN  variable SV-SGHASR  variable SV-SGIN  variable SV-SGOUT
variable SV-SGRIN   variable SV-SGROUT
variable SV-THDROW  variable SV-THRROW  variable SV-THSET
variable SV-TRAIL

: TRIAL-SAVE
   FV @ SV-FV !  TRAIL-N @ SV-TRAIL !     \ trail height is the per-TRY-EFF mark
   SPN @ SV-SPN !  QEN @ SV-QEN !  PTRN @ SV-PTRN !
   OK @ SV-OK !  DCUR @ SV-DCUR !  RCUR @ SV-RCUR !  UNCK @ SV-UNCK !
   FAILSET @ SV-FSET !  DEXP @ SV-DEXP !  DACT @ SV-DACT !
   DF-ACT @ SV-DF-ACT !  DF-EXP @ SV-DF-EXP !  DVAR @ SV-DVAR !  DPOS @ SV-DPOS !
   SGBAD @ SV-SGBAD !  SGBAD-A @ SV-SGBAD-A !
   SGBAD-U @ SV-SGBAD-U !  SGBAD-KIND @ SV-SGBAD-KIND !
   SGBAD-AR-DECL @ SV-SGBAD-AR-DECL !  SGBAD-AR-GOT @ SV-SGBAD-AR-GOT !
   SGSEEN @ SV-SGSEEN !  SGHASR @ SV-SGHASR !
   SGIN @ SV-SGIN !  SGOUT @ SV-SGOUT !  SGRIN @ SV-SGRIN !  SGROUT @ SV-SGROUT !
   THDROW @ SV-THDROW !  THRROW @ SV-THRROW !  THSET @ SV-THSET ! ;

: TRIAL-CLEAR-NEW
   SV-FV @ BEGIN dup FV @ < WHILE
      UNBOUND over cells TVT + !  UNBOUND over cells RVT + !
      0 over cells TVK + !
      1 +
   REPEAT drop ;

: TRIAL-REST-SG
   SV-SGBAD @ SGBAD !  SV-SGBAD-A @ SGBAD-A !
   SV-SGBAD-U @ SGBAD-U !  SV-SGBAD-KIND @ SGBAD-KIND !
   SV-SGBAD-AR-DECL @ SGBAD-AR-DECL !  SV-SGBAD-AR-GOT @ SGBAD-AR-GOT !
   SV-SGSEEN @ SGSEEN !  SV-SGHASR @ SGHASR !
   SV-SGIN @ SGIN !  SV-SGOUT @ SGOUT !  SV-SGRIN @ SGRIN !  SV-SGROUT @ SGROUT ! ;

: TRIAL-REST
   SV-TRAIL @ TRAIL-UNWIND       \ undo speculative binds in both pools
   TRIAL-CLEAR-NEW               \ new-var backstop (cells never bound via TV!/RV!)
   SV-FV @ FV !
   SV-SPN @ SPN !  SV-QEN @ QEN !  SV-PTRN @ PTRN !
   SV-OK @ OK !  SV-DCUR @ DCUR !  SV-RCUR @ RCUR !  SV-UNCK @ UNCK !
   SV-FSET @ FAILSET !  SV-DEXP @ DEXP !  SV-DACT @ DACT !
   SV-DF-ACT @ DF-ACT !  SV-DF-EXP @ DF-EXP !  SV-DVAR @ DVAR !  SV-DPOS @ DPOS !
   SV-THDROW @ THDROW !  SV-THRROW @ THRROW !  SV-THSET @ THSET !
   TRIAL-REST-SG ;

variable TSEEN  variable TSOK  variable TFA

: TRY-EFF ( ptr a -- bool ) {: h:ptr :}
   TRIAL-DEPTH @ 1 + TRIAL-DEPTH !       \ open a trial: disables path compression
   TRIAL-SAVE
   h EFF-APPLY
   OK @ SGBAD @ 0= and IF TRIAL-REST-SG RES-TRUE ELSE TRIAL-REST RES-FALSE THEN
   TRIAL-DEPTH @ 1 - TRIAL-DEPTH ! ;     \ (stack-neutral; the bool stays on top)

\ TRY-PRIMS ( n -- bool ) : try each prim overload for sym until one unifies.
\ Starts at the cached first slot and stops at the first success.
: TRY-PRIMS ( n -- bool ) {: sym:n :}
   0 TSEEN !  0 TSOK !  0 TFA !
   sym PRIM-FIRST-IDX dup 0 = IF drop RES-FALSE EXIT THEN
   1 - PE-I !
   begin PE-I @ #PE @ <  TSOK @ 0=  and while
      PE-I @ PE-ACTIVE? IF
         PE-I @ PE-SYM@ sym = IF
            PE-I @ PE-TRUSTED-ONLY? IF        \ reached only from a CHECKED body -> reject fail-closed
               -1 CAPREQ !  0 OK !  -1 FAILSET !  RES-TRUE EXIT THEN
            TSEEN @ 0= IF PE-I @ PE-EFF@ TFA ! THEN
            -1 TSEEN !
            PE-I @ PE-EFF@ E-PTR TRY-EFF IF -1 TSOK ! THEN
         THEN
      THEN
      PE-I @ 1 + PE-I !
   repeat
   TSOK @ 0 <> ;
variable FLD  variable FLI  variable FLO  variable FLC

\ float literal: -?d*.d+ — exactly one dot, >=1 digit after it, digits-before-dot
\ optional. Mirrors the engine number parser (habu1.f EMIT-NUM/C-NUM-DOT) exactly:
\ dot-leading spellings (.5 -.5 .0) ARE engine float literals, and the checker
\ must claim precisely the tokens the runtime claims (GD-LITERAL-FIRST) or a
\ number-shaped word certifies as a call the runtime can never reach.
: FLODIG? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   0 FLD !  0 FLI !  -1 FLO !
   u 2 < IF 0 FLO ! THEN
   a c@ 45 = IF 1 FLI ! THEN
   FLI @ BEGIN dup u < WHILE
     a over + c@ FLC !
     FLC @ 46 = IF FLD @ 0 > IF 0 FLO ! THEN FLD @ 1 + FLD !
     ELSE FLC @ 47 > FLC @ 58 < and 0= IF 0 FLO ! THEN THEN
     1 + REPEAT drop
   FLD @ 1 = FLO @ 0 <> and
   u 0 > IF a u 1 - + c@ 46 = IF drop RES-FALSE THEN THEN ;

: DEFINER-TOK ( ptr u8 n -- bool ) {: a:ptr u:n :}
   SGSEEN @ 0= IF RES-FALSE EXIT THEN
   a u s" create" CORE-STR= IF RES-TRUE EXIT THEN
   a u s" variable" CORE-STR= IF RES-TRUE EXIT THEN
   a u s" constant" CORE-STR= IF STEP-N-IN RES-TRUE EXIT THEN
   RES-FALSE ;

: LITERAL-TOK? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u ALLDIG? IF STEP-N-OUT RES-TRUE EXIT THEN
   a u FLODIG? IF STEP-R-OUT RES-TRUE EXIT THEN
   RES-FALSE ;

: BYTE-CON? ( n -- bool )
   T-RES dup TAG T-CON = IF PAY CC-U8 = EXIT THEN drop RES-FALSE ;

: BYTE-PTR? ( n -- bool )
   T-RES dup TAG T-PTR = IF PTR>INNER BYTE-CON? EXIT THEN drop RES-FALSE ;

: ROW-TOP-BYTE-PTR? ( n -- bool )
   R-RES dup TAG S-PUSH = IF P>TYPE BYTE-PTR? EXIT THEN drop RES-FALSE ;

\ --- item 12 width facts (the emitter fact surface, docs §17-18) -------------
\ Per checked definition the checker records one row for every operation whose
\ lowering needs a logical layout width. Transport/locals rows use logical
\ operand positions; layout-memory rows use position 0 as the operation's
\ bundle width (the address itself stays one cell). An absent row means scalar
\ lowering. Certificate consumers query the table by raw source-byte offset
\ before emitting the operation. The table is per-CHECK scratch.
$0 constant WF.OFF-OFF
$8 constant WF.POS-OFF
$10 constant WF.FAM-OFF
$18 constant WF.W-OFF
$20 constant WF.TERM-OFF
$28 constant WF.FLAGS-OFF
$30 constant WF-REC
$8 constant WF-REC-ALIGN
0 constant WF-REC-PTR-MASK

: WF.OFF ( ptr a -- ptr a ) WF.OFF-OFF + ;
: WF.POS ( ptr a -- ptr a ) WF.POS-OFF + ;
: WF.FAM ( ptr a -- ptr a ) WF.FAM-OFF + ;
: WF.W ( ptr a -- ptr a ) WF.W-OFF + ;
: WF.TERM ( ptr a -- ptr a ) WF.TERM-OFF + ;
: WF.FLAGS ( ptr a -- ptr a ) WF.FLAGS-OFF + ;

: CHECKER-LAYOUT= ( n n -- )
   <> if s" checker: frame layout drift" CORE-LAYOUT-RC die then ;

WF.OFF-OFF 0 cells CHECKER-LAYOUT=
WF.POS-OFF 1 cells CHECKER-LAYOUT=
WF.FAM-OFF 2 cells CHECKER-LAYOUT=
WF.W-OFF 3 cells CHECKER-LAYOUT=
WF.TERM-OFF 4 cells CHECKER-LAYOUT=
WF.FLAGS-OFF 5 cells CHECKER-LAYOUT=
WF-REC 6 cells CHECKER-LAYOUT=
WF-REC-ALIGN CELL CHECKER-LAYOUT=
WF-REC WF-REC-ALIGN mod 0 CHECKER-LAYOUT=
WF-REC-PTR-MASK 0 CHECKER-LAYOUT=
0 WF.OFF WF.OFF-OFF CHECKER-LAYOUT=
0 WF.POS WF.POS-OFF CHECKER-LAYOUT=
0 WF.FAM WF.FAM-OFF CHECKER-LAYOUT=
0 WF.W WF.W-OFF CHECKER-LAYOUT=
0 WF.TERM WF.TERM-OFF CHECKER-LAYOUT=
0 WF.FLAGS WF.FLAGS-OFF CHECKER-LAYOUT=
$80 constant WF-INIT
create WFS-BOOT WF-INIT WF-REC * allot
PTR-VARIABLE WFS-P   WFS-BOOT WFS-P !
variable WF-CAP  WF-INIT WF-CAP !
variable WF-N
variable WF-CURROW   variable WF-POS-I
variable WF-I

1 constant WF-FETCH-FLAG
2 constant WF-STORE-FLAG
4 constant WF-XPAD-FLAG   \ layout-cap slice 4: construct/MATCH extra-pad fact (w = instantiated_pads - declared_pads)

: WFS ( -- ptr a ) WFS-P @ ;

: WF-GROW-CAP ( n -- n ) {: need:n :}
   $7FFFFFFFFFFFFFFF WF-REC / {: lim:n :}
   need 0 <= need lim > or if s" checker: width-fact capacity overflow" 76 die then
   WF-CAP @ lim 2 / <= if WF-CAP @ 2 * need max exit then
   need ;

: WF-ENSURE ( n -- ) {: need:n :}
   need WF-CAP @ <= if exit then
   need WF-GROW-CAP {: cap:n :}
   WFS-P @ WF-CAP @ WF-REC * cap WF-REC * ARENA-BYTES-GROW WFS-P !
   cap WF-CAP ! ;

: WF-ROW ( n -- ptr a ) WF-REC * WFS + ;
: WF-ROW@ ( n -- ptr a ) {: i:n :}
   i 0 < i WF-N @ >= or IF s" checker: bad width-fact index" 76 die THEN
   i WF-ROW ;
: WF-N@ ( -- n ) WF-N @ ;
: WF-OFF@ ( n -- n ) WF-ROW@ WF.OFF @ ;
: WF-POS@ ( n -- n ) WF-ROW@ WF.POS @ ;
: WF-FAM@ ( n -- n ) WF-ROW@ WF.FAM @ ;
: WF-WIDTH@ ( n -- n ) WF-ROW@ WF.W @ ;
: WF-TERM@ ( n -- n ) WF-ROW@ WF.TERM @ ;
: WF-FLAGS@ ( n -- n ) WF-ROW@ WF.FLAGS @ ;

: WF-ROW-COPY ( n n -- ) {: src:n dst:n :}
   src WF-ROW dst WF-ROW WF-REC ARENA-COPY ;

: WF-KEY< ( n n n -- bool ) {: off:n pos:n row:n :}
   off row WF-OFF@ < if RES-TRUE exit then
   off row WF-OFF@ = pos row WF-POS@ < and ;

: WF-KEY= ( n n n -- bool ) {: off:n pos:n row:n :}
   off row WF-OFF@ = pos row WF-POS@ = and ;

: WF-SAME? ( n n n n n -- bool ) {: fam:n term:n w:n flags:n row:n :}
   fam row WF-FAM@ =
   term row WF-TERM@ = and
   w row WF-WIDTH@ = and
   flags row WF-FLAGS@ = and ;

: WF-SEEK-DONE? ( n n -- bool ) {: off:n pos:n :}
   WF-I @ WF-N @ >= if RES-TRUE exit then
   off pos WF-I @ WF-KEY< if RES-TRUE exit then
   off pos WF-I @ WF-KEY= if RES-TRUE exit then
   WF-I @ 1 + WF-I !
   RES-FALSE ;

: WF-ADD-FULL ( n n n n n -- ) {: pos:n fam:n term:n w:n flags:n :}
   TSTART @ {: off:n :}
   0 WF-I !
   begin off pos WF-SEEK-DONE? until
   WF-I @ WF-N @ < if
      off pos WF-I @ WF-KEY= if
         fam term w flags WF-I @ WF-SAME? 0= if s" checker: conflicting width fact" 76 die then
         exit
      then
   then
   WF-N @ 1 + WF-ENSURE
   WF-N @ begin dup WF-I @ > while
      dup 1 - over WF-ROW-COPY
      1 -
   repeat drop
   WF-I @ WF-ROW {: r:ptr :}
   off r WF.OFF !  pos r WF.POS !  fam r WF.FAM !  w r WF.W !
   term r WF.TERM !  flags r WF.FLAGS !
   WF-N @ 1 + WF-N ! ;

: WF-ADD ( n n n -- ) {: pos:n fam:n w:n :}
   pos fam 0 w 0 WF-ADD-FULL ;

: WF-MEM-RECORD ( n n -- ) {: t:n flags:n :}
   0 t PARAM>FAM t t T-WIDTH flags WF-ADD-FULL ;

\ --- storable layouts S2 (dot habu-checker-capability-typed-a480c423): `!`/`@`
\ through a `ptr family<..>` address move the pointee's whole logical value.
\ The ADDRESS type picks the arm — family identity lives in the pointer, so a
\ bare `ptr a` store of a layout value stays rejected, a mismatched family
\ rejects in ordinary unification, an `n` cannot enter an enum slot, and a
\ fetched value is born typed (never a bare n). LAYOUT-MEM-OK? admits every
\ closed non-linear layout width; WF-MEM-RECORD gives pass 2 the registry width
\ before CHECKER-STEP mutates the row. The effect rows push the family's hidden
\ expansion, so the value side pairs hidden-to-hidden like every checked row.
\ A nominal-scalar pointee (arity-0 TK-CELL param) takes the same arm with
\ param-to-param rows — NOMPTR-BLOCK? forbids the generic var route — but
\ records no width fact: every raw cell is a valid value of the family, so it
\ lowers as a plain scalar with no fetch-validation program.
: LAYOUT-MEM-INNER ( -- n bool )   \ resolved DCUR top `ptr fam<..>` -> logical pointee
   DCUR @ R-RES {: node:n :}
   node TAG S-PUSH <> IF 0 RES-FALSE EXIT THEN
   node P>TYPE T-RES {: pt:n :}
   pt TAG T-PTR <> IF 0 RES-FALSE EXIT THEN
   pt PTR>INNER T-RES {: t:n :}
   t HIDDEN-PARAM? IF 0 RES-FALSE EXIT THEN
   t LAYOUT-PARAM? t NOM-SCALAR? or 0= IF 0 RES-FALSE EXIT THEN
   t RES-TRUE ;

: LAYOUT-FETCH-STEP ( n -- ) {: t:n :}   \ ( ptr fam -- fam ) from the pointee term
   t LAYOUT-MEM-OK? 0= IF 0 OK ! -1 FAILSET ! EXIT THEN
   t LAYOUT-PARAM? IF t WF-FETCH-FLAG WF-MEM-RECORD THEN
   FRESH MK-ROW {: rest:n :}
   t MK-PTR rest MK-PUSH
   t rest PUSH-LOGICAL
   CHECKER-STEP ;

: LAYOUT-STORE-STEP ( n -- ) {: t:n :}   \ ( fam ptr fam -- ) from the pointee term
   t LAYOUT-MEM-OK? 0= IF 0 OK ! -1 FAILSET ! EXIT THEN
   t LAYOUT-PARAM? IF t WF-STORE-FLAG WF-MEM-RECORD THEN
   FRESH MK-ROW {: rest:n :}
   t rest PUSH-LOGICAL
   t MK-PTR swap MK-PUSH
   rest CHECKER-STEP ;

\ Byte-pointer cell-op reject (dot habu-checker-diag-for-4c443fcf). `!`/`@` need
\ a cell-width `ptr a`, but ROW-TOP-BYTE-PTR? found a `ptr u8`. STEP-STORE/
\ STEP-FETCH unify a fresh-var pointee, so a byte pointer clears their unify and
\ only the width rule rejects — with no captured pair, so the diagnostic named
\ just the token. Populate the mismatch pair (expected `ptr a`, actual `ptr u8`)
\ so text and JSON render expected/actual like every other mismatch, unless an
\ earlier failure already owns the diagnostic (FAILSET). UF-SET is 0 after a
\ cleared unify, so UF>DIAG clears the ADT family/variant fields.
: MEM-BYTE-PTR-REJECT ( -- )
   0 OK !
   FAILSET @ 0= IF
      PE-U8 MK-PTR FRESH MK-ROW MK-PUSH DACT !
      FRESH MK-VAR MK-PTR FRESH MK-ROW MK-PUSH DEXP !
      UF>DIAG  -1 FAILSET !
   THEN ;

: CELL-FETCH-TOK ( -- )
   LAYOUT-MEM-INNER IF LAYOUT-FETCH-STEP EXIT THEN drop
   DCUR @ ROW-TOP-BYTE-PTR? {: bad :}
   STEP-FETCH
   bad IF MEM-BYTE-PTR-REJECT THEN ;

: CELL-STORE-TOK ( -- )
   LAYOUT-MEM-INNER IF LAYOUT-STORE-STEP EXIT THEN drop
   DCUR @ ROW-TOP-BYTE-PTR? {: bad :}
   STEP-STORE
   bad IF MEM-BYTE-PTR-REJECT THEN ;

: CELL-MEMORY-TOK? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" @" CORE-STR= IF CELL-FETCH-TOK RES-TRUE EXIT THEN
   a u s" !" CORE-STR= IF CELL-STORE-TOK RES-TRUE EXIT THEN
   RES-FALSE ;

: DO-TOK ( ptr u8 n -- ) {: a:ptr u:n :}
   0 CURSYM !
   a u DEFINER-TOK IF EXIT THEN
   a u LITERAL-TOK? IF EXIT THEN
   a u CELL-MEMORY-TOK? IF EXIT THEN
   a u CHECKER-FIND-ACTIVE-SYM CURSYM !
   FEP-CLEAR
   CURSYM @ CHECKER-FIND-USIG-SYM drop
   CURSYM @ CTOR-STEP-XT IF EXIT THEN              \ multi-cell generated-constructor call -> arg-aware step (default rejects pre-registry)
   FEP-HIT? IF FEP @ EFF-APPLY ELSE
   CURSYM @ TRY-PRIMS IF EXIT THEN
   TSEEN @ 0 <> IF TFA @ E-PTR EFF-APPLY ELSE
   CHECKER-QBAD-TOK @ 0 <> IF -1 QUALBAD ! THEN
   -1 UNDEFERR ! -1 UNCK ! THEN THEN ;

: ROW-DROP-1 ( n -- n )            \ row below the top cell; die 76 if it is not a push
   R-RES dup TAG S-PUSH <> IF s" checker: hidden group underruns row" 76 die THEN
   P>REST ;
: ROW-DROP-N ( n n -- n ) {: w:n :}   \ ( row -- row ) below the top w cells
   0 BEGIN dup w < WHILE
      swap ROW-DROP-1 swap
      1 +
   REPEAT drop ;

\ WF-POS-RECORD ( n -- n ) : one LOGICAL operand at a resolved S-PUSH node —
\ record a width fact when it is a layout value (an expanded hidden group, whose
\ width = group cell count = registry width, or a non-expanded possibly-linear
\ logical cell) and return the row below the whole operand.
: WF-POS-RECORD ( n -- n ) {: node:n :}
   node P>TYPE T-RES {: t:n :}
   t HIDDEN-PARAM? IF
      t PARAM>FAM {: fam:n :}
      t T-WIDTH {: w:n :}                       \ arg-aware bundle width (fact + row drop)
      WF-POS-I @  fam  w  WF-ADD
      node  w  ROW-DROP-N EXIT
   THEN
   t LAYOUT-PARAM? IF WF-POS-I @  t PARAM>FAM  t T-WIDTH  WF-ADD THEN
   node P>REST ;

: WF-ROW-SCAN ( n n -- ) {: row:n k:n :}   \ record facts for the top-k logical positions of row
   row WF-CURROW !  0 WF-POS-I !
   BEGIN WF-POS-I @ k <  WF-CURROW @ R-RES TAG S-PUSH =  and WHILE
      WF-CURROW @ R-RES WF-POS-RECORD WF-CURROW !
      WF-POS-I @ 1 + WF-POS-I !
   REPEAT ;

\ operand count each transport op moves as logical values, and the row it reads
\ (r>/r@/2r>/2r@ consume from the return row; everything else the data row).
: WF-XPORT-K ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u s" rot"   CORE-STR= IF 3 EXIT THEN
   a u s" -rot"  CORE-STR= IF 3 EXIT THEN
   a u s" 2swap" CORE-STR= IF 4 EXIT THEN
   a u s" 2over" CORE-STR= IF 4 EXIT THEN
   a u s" swap"  CORE-STR= IF 2 EXIT THEN
   a u s" over"  CORE-STR= IF 2 EXIT THEN
   a u s" nip"   CORE-STR= IF 2 EXIT THEN
   a u s" tuck"  CORE-STR= IF 2 EXIT THEN
   a u s" 2dup"  CORE-STR= IF 2 EXIT THEN
   a u s" 2drop" CORE-STR= IF 2 EXIT THEN
   a u s" 2>r"   CORE-STR= IF 2 EXIT THEN
   a u s" 2r>"   CORE-STR= IF 2 EXIT THEN
   a u s" 2r@"   CORE-STR= IF 2 EXIT THEN
   1 ;
: WF-XPORT-RROW? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" r>"  CORE-STR= IF RES-TRUE EXIT THEN
   a u s" r@"  CORE-STR= IF RES-TRUE EXIT THEN
   a u s" 2r>" CORE-STR= IF RES-TRUE EXIT THEN
   a u s" 2r@" CORE-STR= IF RES-TRUE EXIT THEN
   RES-FALSE ;
: WF-XPORT-RECORD ( ptr u8 n -- ) {: a:ptr u:n :}
   a u WF-XPORT-RROW? IF RCUR ELSE DCUR THEN @
   a u WF-XPORT-K  WF-ROW-SCAN ;

\ --- item 12 slice 3b: emitter-facing width-fact queries. The engine's pass-2
\ recompiler (habu2.f EM-COMPILE-P2WIDE) calls these through the dictionary
\ right after the check hook certifies a definition; the facts are that
\ definition's per-CHECK scratch, still live until the next CHECK.
: WF-WIDE? ( -- bool )
   0 BEGIN dup WF-N @ < WHILE
      dup WF-WIDTH@ 1 > IF drop RES-TRUE EXIT THEN
      1 +
   REPEAT drop RES-FALSE ;
: WF-XPAD? ( -- bool )   \ layout-cap slice 4: any construct/MATCH extra-pad fact (triggers pass-2 even at extra=1)
   0 BEGIN dup WF-N @ < WHILE
      dup WF-FLAGS@ WF-XPAD-FLAG and 0 <> IF drop RES-TRUE EXIT THEN
      1 +
   REPEAT drop RES-FALSE ;
: WF-FETCH? ( n -- bool ) WF-FLAGS@ WF-FETCH-FLAG and 0 <> ;
: WF-NEEDS-P2? ( -- bool )
   WF-WIDE? IF RES-TRUE EXIT THEN
   WF-XPAD? IF RES-TRUE EXIT THEN
   0 BEGIN dup WF-N @ < WHILE
      dup WF-FETCH? IF drop RES-TRUE EXIT THEN
      1 +
   REPEAT drop RES-FALSE ;
: WF-HIT? ( n n n -- bool ) {: i:n off:n pos:n :}   \ row i records (off,pos)?
   i WF-OFF@ off <> IF RES-FALSE EXIT THEN
   i WF-POS@ pos = ;
: WF-W-AT ( n n -- n ) {: off:n pos:n :}   \ width at raw source offset and operand position
   0 BEGIN dup WF-N @ < WHILE
      dup off pos WF-HIT? IF WF-WIDTH@ EXIT THEN
      1 +
   REPEAT drop 1 ;

\ --- item 12 slice 3b: whole-bundle transport row surgery (docs §17) ----------
\ With hidden-field expansion a layout value is W consecutive hidden cells (tag
\ on top, slot0 deepest). A transport op whose top-k logical operands include
\ such a group cannot use the var-based prim effect (a hidden field never binds
\ a var), so the checker rebuilds the consumed row directly: read k logical
\ GROUPS (a hidden tag opens an exactly-W run, validated slot W-1..0 same
\ family — die 76 on a malformed run, an internal invariant; any other cell is
\ a 1-cell group; a row var materializes a fresh 1-cell var group through the
\ normal FRESH+UNIFY mechanism, exactly as the prim effect would — underflow
\ against a short concrete row is the normal UNIFY reject), then write the op's
\ output permutation with MK-PUSH from the group lists. Pure-scalar rows never
\ reach this path: the prim/RS effects stay byte-for-byte. A non-expanded
\ logical layout cell (possibly-linear, docs §19 / TFAM 11) rejects here
\ exactly as the one-cell transport guard rejects it (TDLIN-* unchanged).
64 constant XG-GCAP            \ max logical groups per op (= LOC-CAP for locals capture)
256 constant XG-TCAP           \ max total cells across one op's groups
create XG-TERMS XG-TCAP cells allot
create XG-START XG-GCAP cells allot
create XG-LEN XG-GCAP cells allot
variable XG-N   variable XG-TN   variable XG-ROW

: XG-T+ ( n -- )               \ append one cell term to the group scratch
   XG-TN @ XG-TCAP >= IF s" checker: transport group scratch full" 76 die THEN
   XG-TN @ cells XG-TERMS + !
   XG-TN @ 1 + XG-TN ! ;

: XG-TERM0@ ( n -- n ) {: gi:n :}   \ first (top) cell term of group gi
   XG-START gi cells + @ cells XG-TERMS + @ ;

: XG-CELL-HID@ ( n n -- ) {: fam:n slot:n :}   \ consume one hidden fam/slot cell off XG-ROW
   XG-ROW @ R-RES {: node:n :}
   node TAG S-PUSH <> IF s" checker: hidden group underruns row" 76 die THEN
   node P>TYPE T-RES {: t:n :}
   t HIDDEN-PARAM? 0= IF s" checker: hidden group cell not hidden" 76 die THEN
   t PARAM>FAM fam <> IF s" checker: hidden group family mismatch" 76 die THEN
   t HIDDEN-SLOT@ slot <> IF s" checker: hidden group slot mismatch" 76 die THEN
   t XG-T+
   node P>REST XG-ROW ! ;

: XG-READ-HID ( n -- ) {: t:n :}   \ read the whole W-cell group whose resolved tag is t
   t PARAM>FAM {: fam:n :}
   t T-WIDTH {: w:n :}                          \ arg-aware bundle width
   t HIDDEN-SLOT@ w 1 - <> IF s" checker: hidden tag not on group top" 76 die THEN
   w 1 - BEGIN dup 0 >= WHILE
      fam over XG-CELL-HID@
      1 -
   REPEAT drop ;

: XG-READ-VAR ( -- bool )      \ materialize one 1-cell var group out of the row var
   FRESH MK-VAR {: tv:n :}
   FRESH MK-ROW {: rest:n :}
   XG-ROW @  tv rest MK-PUSH  UNIFY 0= IF 0 OK ! RES-FALSE EXIT THEN
   tv XG-T+
   rest XG-ROW !
   RES-TRUE ;

: XG-READ-GROUP ( -- bool )    \ one logical group at XG-ROW; false = rejected
   XG-ROW @ R-RES {: node:n :}
   node TAG S-ROW = IF XG-READ-VAR EXIT THEN
   node P>TYPE T-RES {: t:n :}
   t HIDDEN-PARAM? IF
      t XG-READ-HID RES-TRUE EXIT THEN   \ linear bundles transport too: the whole M+1 group moves
                                         \ atomically, and XPORT-APPLY's LIN-SNAPSHOT/LIN-CHECK conserves
                                         \ the bundle count — a move keeps before=after (accept), a
                                         \ copy raises it and a drop lowers it (reject). Locals capture
                                         \ (LOC-BIND-GROUPS) removes the bundle from the counted rows at
                                         \ bind, so the same LIN-CHECK still rejects a linear layout local.
   t LAYOUT-PARAM? IF 0 OK ! -1 FAILSET ! RES-FALSE EXIT THEN   \ open-arg layout never transports
   node P>TYPE XG-T+
   node P>REST XG-ROW !
   RES-TRUE ;

: XG-GROUP-DONE ( -- )
   XG-TN @  XG-START XG-N @ cells + @  -  XG-LEN XG-N @ cells + !
   XG-N @ 1 + XG-N ! ;

: XG-READ ( n n -- bool ) {: row:n k:n :}   \ k logical groups off row; XG-ROW = rest
   k XG-GCAP > IF s" checker: transport group cap" 76 die THEN
   row XG-ROW !  0 XG-N !  0 XG-TN !
   0 BEGIN dup k < WHILE
      XG-TN @ XG-START XG-N @ cells + !
      XG-READ-GROUP 0= IF drop RES-FALSE EXIT THEN
      XG-GROUP-DONE
      1 +
   REPEAT drop RES-TRUE ;

: XG-PUSH-GROUP ( n n -- n ) {: gi:n :}   \ ( row -- row ) push group gi, deepest cell first
   XG-START gi cells + @ {: st:n :}
   XG-LEN gi cells + @ {: len:n :}
   st len + 1 - BEGIN dup st >= WHILE
      dup cells XG-TERMS + @
      rot MK-PUSH swap
      1 -
   REPEAT drop ;

: XG-REPLAY ( n ptr u8 n -- n ) {: a:ptr u:n :}   \ ( row -- row ) push groups per index digit
   0 BEGIN dup u < WHILE
      dup a + c@ 48 -
      rot swap XG-PUSH-GROUP swap
      1 +
   REPEAT drop ;

: XG-GROUP-VAR? ( n -- bool ) {: gi:n :}   \ group gi is one still-unbound var cell?
   XG-LEN gi cells + @ 1 <> IF RES-FALSE EXIT THEN
   gi XG-TERM0@ T-RES ISVAR ;

: XG-SEQ-COUNT ( n ptr u8 n -- n ) {: gi:n a:ptr u:n :}   \ occurrences of gi in the digit seq
   0
   0 BEGIN dup u < WHILE
      dup a + c@ 48 - gi = IF swap 1 + swap THEN
      1 +
   REPEAT drop ;

\ a var-typed scalar group copied or dropped by the permutation is a potential
\ linear launder: taint it, exactly as LIN-EFF-PASS taints a prim-effect var
\ with unequal in/out multiplicity. Hidden groups carry no linears (linear
\ layouts never expand) and a concrete linear con hits LIN-CHECK conservation.
: XG-TAINT-SEQ ( ptr u8 n -- ) {: a:ptr u:n :}
   LIN-ANY? 0= IF EXIT THEN
   0 BEGIN dup XG-N @ < WHILE
      dup XG-GROUP-VAR? IF
         dup a u XG-SEQ-COUNT 1 <> IF
            dup XG-TERM0@ T-RES PAY LIN-TAINT
         THEN
      THEN
      1 +
   REPEAT drop ;

\ output permutation of each data transport as group-index digits, bottom->top
\ (group 0 = pre-op top-of-stack operand).
: XP-DATA-SEQ$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   a u s" dup"   CORE-STR= IF s" 00" EXIT THEN
   a u s" drop"  CORE-STR= IF s" " EXIT THEN
   a u s" swap"  CORE-STR= IF s" 01" EXIT THEN
   a u s" over"  CORE-STR= IF s" 101" EXIT THEN
   a u s" nip"   CORE-STR= IF s" 0" EXIT THEN
   a u s" tuck"  CORE-STR= IF s" 010" EXIT THEN
   a u s" rot"   CORE-STR= IF s" 102" EXIT THEN
   a u s" -rot"  CORE-STR= IF s" 021" EXIT THEN
   a u s" 2dup"  CORE-STR= IF s" 1010" EXIT THEN
   a u s" 2drop" CORE-STR= IF s" " EXIT THEN
   a u s" 2swap" CORE-STR= IF s" 1032" EXIT THEN
   a u s" 2over" CORE-STR= IF s" 321032" EXIT THEN
   s" checker: transport seq for non-transport" 76 die ;

: XP-RS? ( ptr u8 n -- bool ) {: a:ptr u:n :}   \ return-stack transfer op?
   a u WF-XPORT-RROW? IF RES-TRUE EXIT THEN
   a u s" >r" CORE-STR= IF RES-TRUE EXIT THEN
   a u s" 2>r" CORE-STR= ;

: XP-RS-KEEP? ( ptr u8 n -- bool ) {: a:ptr u:n :}   \ r@/2r@ re-push the groups on the return row
   a u s" r@" CORE-STR= IF RES-TRUE EXIT THEN
   a u s" 2r@" CORE-STR= ;

: XP-RS-SEQ$ ( ptr u8 n -- ptr u8 n )   \ rs transfer order (bottom->top; RS2->R parity)
   WF-XPORT-K 1 = IF s" 0" ELSE s" 10" THEN ;

: XPORT-RS-APPLY ( ptr u8 n -- ) {: a:ptr u:n :}
   a u WF-XPORT-RROW? IF                          \ r> r@ 2r> 2r@ : consumed from RCUR
      a u XP-RS-KEEP? IF
         XG-ROW @ a u XP-RS-SEQ$ XG-REPLAY RCUR !
      ELSE
         XG-ROW @ RCUR !
      THEN
      DCUR @ a u XP-RS-SEQ$ XG-REPLAY DCUR !
   ELSE                                           \ >r 2>r : consumed from DCUR
      XG-ROW @ DCUR !
      RCUR @ a u XP-RS-SEQ$ XG-REPLAY RCUR !
   THEN ;

: XPORT-APPLY ( ptr u8 n -- ) {: a:ptr u:n :}
   LIN-SNAPSHOT
   a u WF-XPORT-RROW? IF RCUR ELSE DCUR THEN @
   a u WF-XPORT-K  XG-READ 0= IF EXIT THEN
   a u XP-RS? IF
      a u XPORT-RS-APPLY
   ELSE
      XG-ROW @ a u XP-DATA-SEQ$ XG-REPLAY DCUR !
      a u XP-DATA-SEQ$ XG-TAINT-SEQ
   THEN
   OK @ IF LIN-CHECK THEN ;

\ any hidden cell within the top-k CELLS implies a hidden-group operand within
\ the top-k logical operands (a group's cells start at or above its position).
: XP-BUNDLE-IN-K? ( n n -- bool ) {: row0:n k:n :}
   row0
   0 BEGIN dup k < WHILE
      over R-RES TAG S-PUSH <> IF 2drop RES-FALSE EXIT THEN
      over R-RES P>TYPE T-RES HIDDEN-PARAM? IF 2drop RES-TRUE EXIT THEN
      swap R-RES P>REST swap
      1 +
   REPEAT 2drop RES-FALSE ;

: XPORT-STEP? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   LAYOUT-XPORT @ 0= IF RES-FALSE EXIT THEN
   a u WF-XPORT-RROW? IF RCUR ELSE DCUR THEN @
   a u WF-XPORT-K  XP-BUNDLE-IN-K? 0= IF RES-FALSE EXIT THEN
   a u XPORT-APPLY
   RES-TRUE ;

: ROW-ANY-HIDDEN? ( n -- bool )    \ any hidden cell above the row's tail var?
   BEGIN R-RES dup TAG S-PUSH = WHILE
      dup P>TYPE T-RES HIDDEN-PARAM? IF drop RES-TRUE EXIT THEN
      P>REST
   REPEAT drop RES-FALSE ;

\ depth/.s report raw physical cells: over a row holding hidden fields they
\ would expose the physical expansion of a logical value, so they fail closed
\ PERMANENTLY (TFAM 12 item-5 verdict 2026-07-09; docs §17 sanctions reject
\ over logical-shape reporting). The lift — logical-shape introspection that
\ counts whole bundles — is capability dot habu-logical-shape-depth-9686f5c1.
: HIDROW-STEP? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" depth" CORE-STR=  a u s" .s" CORE-STR= or 0= IF RES-FALSE EXIT THEN
   DCUR @ ROW-ANY-HIDDEN? 0= IF RES-FALSE EXIT THEN
   0 OK !  -1 FAILSET !
   RES-TRUE ;

\ --- locals: {: a b :} pops and binds names to type vars; a reference pushes
\ its binding. Groups accumulate (a later group binds only its own names).
: CCOPY ( ptr u8 ptr u8 n -- ) {: a:ptr d:ptr u:n :}
   0 BEGIN dup u < WHILE
      dup a + c@
      over d + c!
      1 +
   REPEAT drop ;
64 constant LOC-CAP            \ max locals per definition (matches compiler frame)
16 constant LOC-NAME-W         \ max local-name bytes (matches compiler LOCN-CELL)
create LOCNB LOC-CAP LOC-NAME-W * allot   create LOCLN LOC-CAP cells allot   create LOCTV LOC-CAP cells allot
create LOCSHOW LOC-CAP cells allot
\ LOCW: logical width of each local's binding. 1 = scalar (LOCTV holds the type
\ var, exactly as before). >1 = a whole hidden-field bundle (item 12 slice 3b):
\ LOCTV holds the group's TAG term and a reference re-expands slot0..tag from it
\ via MK-HIDDEN (LOC-PUSH-REF). Linear layouts never expand, so no linear bundle
\ can reach a local; the bind-time linear check is unchanged.
create LOCW LOC-CAP cells allot
variable #LOC  variable LMODE  variable LGRP  variable LROW  variable LCH  variable LI  variable LRF
\ Emitter-facing width queries (item 12 slice 3b + habu-tfam-12-pass): LOCW is
\ live-indexed and branch-scoped locals are popped at their join (CF-LOC-REST)
\ with their slots rebound by sibling arms, so the pass-2 recompiler must not
\ read LOCW by live index after the verdict. LOCW-HW records every bind
\ occurrence's FINAL width keyed by a monotone bind sequence that is never
\ popped; LOCSEQIX maps a live index to its bind seq while the local is live
\ (written at LOC-ADD, read at the group's :} bind).
LOC-CAP 4 * constant LOC-HW-INIT
$0FFFFFFFFFFFFFFF constant LOC-HW-MAX-CELLS
create LOC-HW-BOOT LOC-HW-INIT cells allot
PTR-VARIABLE LOC-HW-P   LOC-HW-BOOT LOC-HW-P !
variable LOC-HW-CAP     LOC-HW-INIT LOC-HW-CAP !
create LOCSEQIX LOC-CAP cells allot
variable LOCSEQ
: LOC-HW ( -- ptr a ) LOC-HW-P @ ;
: LOC-HW-GROW-CAP ( n -- n ) {: need:n :}
   need 0 <= need LOC-HW-MAX-CELLS > or IF s" checker: local bind capacity overflow" 76 die THEN
   LOC-HW-CAP @ LOC-HW-MAX-CELLS 2 / <= IF LOC-HW-CAP @ 2 * need max EXIT THEN
   need ;
: LOC-HW-ENSURE ( n -- ) {: need:n :}
   need LOC-HW-CAP @ <= IF EXIT THEN
   need LOC-HW-GROW-CAP {: cap:n :}
   LOC-HW-P @ LOC-HW-CAP @ cells cap cells ARENA-BYTES-GROW LOC-HW-P !
   cap LOC-HW-CAP ! ;
: LOCW-HW@ ( n -- n ) {: s:n :}    \ final width of bind occurrence s (die 76 = pass-2 misalignment)
   s 0 <  s LOCSEQ @ >=  or IF s" checker: bad local bind sequence" 76 die THEN
   s cells LOC-HW + @ ;
: LOCW-HW-N@ ( -- n ) LOCSEQ @ ;
\ Pass-2 live-width table: the width-aware recompiler (habu2.f EM-P2-CARVE /
\ EM-P2-LOCREF) replays the certified body's {: groups in bind order — each
\ carve consumes the next bind seq (P2-CARVE-W) and records that local's width
\ under its ENGINE live frame index, so a frame slot reused by a sibling branch
\ reads its own width; checker #LOC joins never skew the frame math.
create P2LW LOC-CAP cells allot
variable P2SEQ
: P2LW-IX-GUARD ( n -- )           \ die 76 outside the engine locals frame
   dup 0 <  over LOC-CAP >=  or IF s" checker: bad pass-2 local index" 76 die THEN
   drop ;
: P2-LOCSEQ-RESET ( -- )           \ engine EM-P2-TRIGGER: the pass-2 re-run starts at bind seq 0
   0 P2SEQ ! ;
: P2-CARVE-W ( n -- n ) {: i:n :}  \ next bind's width, recorded as live local i's width
   i P2LW-IX-GUARD
   P2SEQ @ LOCW-HW@ {: w:n :}
   P2SEQ @ 1 + P2SEQ !
   w i cells P2LW + !
   w ;
: P2-LIVE-W@ ( n -- n ) {: i:n :}  \ live local i's width (recorded at its carve)
   i P2LW-IX-GUARD
   i cells P2LW + @ ;
: P2-LIVE-CUM@ ( n -- n ) {: i:n :}   \ frame cells from the frame top through live local i
   i P2LW-IX-GUARD
   0
   0 BEGIN dup i <= WHILE
      dup cells P2LW + @  rot +  swap
      1 +
   REPEAT drop ;
\ per-local type display hook (render.f installs SHOW-LOCAL-TYPE). A `defer` with a
\ no-op DEFAULT so the call is statically effect-known; the default drops the
\ name span + length + type, matching the old 0-hook skip before render binds it.
defer LOCSHOWXT ( ptr u8 n n -- )
: LOCSHOW-DEFAULT ( -- ) [: 2drop drop ;] is LOCSHOWXT ;
LOCSHOW-DEFAULT
variable #CFC
variable QDEPTH

variable LCO

: LCOLON ( ptr u8 n -- ) {: a:ptr u:n :}   \ LCO = index of the first ':' in a/u, or u
   u LCO !
   0 BEGIN  dup u <  LCO @ u =  and WHILE
     dup a + c@ 58 = IF dup LCO ! THEN
     1 + REPEAT drop ;

\ a typed local `a:n` stores the BARE name (matching the engine) and unifies
\ the local's type var with the asserted type — a wrong use then rejects.
: LOC-SHOW-SUFFIX? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   u 1 = if a c@ 63 = exit then
   RES-FALSE ;

: LOC-SUFFIX$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   a LCO @ + 1 +  u LCO @ - 1 - ;

: LOC-SHOW-OFF! ( n -- ) {: idx:n :}
   0 idx cells LOCSHOW + ! ;

: LOC-SHOW-ON! ( n -- ) {: idx:n :}
   -1 idx cells LOCSHOW + ! ;

: LOC-ANN ( ptr u8 n n -- ) {: a:ptr u:n idx:n :}
   a u LOC-SUFFIX$ LOC-SHOW-SUFFIX? if
      idx LOC-SHOW-ON!
      exit
   then
   a u LOC-SUFFIX$ LOCAL-TYPE {: t:n :}
   t T-RES HIDDEN-PARAM? IF                 \ layout annotation: store the asserted
      t idx cells LOCTV + !  EXIT THEN      \ hidden term (hidden never binds a var)
   t idx cells LOCTV + @ UNIFY OK @ and OK ! ;

: LOC-SHOW-ONE ( n -- ) {: idx:n :}
   idx cells LOCSHOW + @ 0= if exit then
   LOCNB idx LOC-NAME-W * +  idx cells LOCLN + @  idx cells LOCTV + @
   LOCSHOWXT ;

: LOC-SHOW-GROUP ( -- )
   OK @ 0= if exit then
   LGRP @ begin dup #LOC @ < while
      dup LOC-SHOW-ONE
      1 +
   repeat drop ;

\ Over-cap locals fail CLOSED (reject) rather than silently uncheckable: a
\ definition whose local count or name width exceeds the compiler-matched frame
\ was previously skipped by the checker (-1 UNCK !), hiding every stack error in
\ it. LOCALBAD forces verdict 0 so the definition is rejected with a diagnostic.
: LOC-ADD ( ptr u8 n -- ) {: a:ptr u:n :}
   a u LCOLON
   #LOC @ LOC-CAP 1 - >  LCO @ LOC-NAME-W >  or IF
     0 OK !  -1 FAILSET !  -1 LOCALBAD !
   ELSE
     LOCSEQ @ 1 + LOC-HW-ENSURE
     #LOC @ LOC-SHOW-OFF!
     a  LOCNB #LOC @ LOC-NAME-W * +  LCO @ CCOPY
     LCO @ #LOC @ cells LOCLN + !
     FRESH MK-VAR #LOC @ cells LOCTV + !
     1 #LOC @ cells LOCW + !
     LOCSEQ @ #LOC @ cells LOCSEQIX + !    \ this bind's monotone sequence
     1 LOCSEQ @ cells LOC-HW + !
     LOCSEQ @ 1 + LOCSEQ !
     LCO @ u < IF
      a u #LOC @ LOC-ANN
     THEN
     #LOC @ 1 + #LOC ! THEN ;

\ Linear values may not launder through locals. A local reference re-pushes its
\ binding without a LIN-SNAPSHOT/LIN-CHECK-covered step, so the concrete-count
\ conservation discipline never sees the copy (two references duplicate) or the
\ drop (an unreferenced local leaks). Binding a linear con into a local — where
\ the value CONCRETELY resolves linear at bind time — is therefore rejected
\ outright with a dedicated E-LINEAR-LOCAL diagnostic (keep the linear on the
\ stack and factor). Path-sensitive per-reference accounting (consume-exactly-
\ once across every branch) is a separate capability tracked by dot.
: LIN-LOCAL-REJECT ( -- )  0 OK !  -1 FAILSET !  -1 LINLOCBAD ! ;

: LIN-LOCAL-BIND-CHECK ( -- )       \ reject if any just-bound local resolves linear
   LIN-ANY? 0= IF exit THEN
   LGRP @ BEGIN dup #LOC @ < WHILE
     dup cells LOCTV + @ LIN-CON? IF LIN-LOCAL-REJECT THEN
     1 + REPEAT drop ;

\ A local bound to a still-polymorphic var that only LATER resolves to a linear
\ con (deferred laundering, e.g. `( a -- ) {: x :} x x T-FREE-OWN T-FREE-OWN`)
\ escapes the bind-time check. Taint each such reference's var like a stack copy;
\ LIN-TAINT-SCAN then rejects it once the var binds linear.
: LIN-LOCAL-REF-TAINT ( n -- )
   LIN-ANY? 0= IF drop exit THEN
   T-RES dup TAG T-VAR = IF PAY LIN-TAINT ELSE drop THEN ;

\ Bundle-aware bind (item 12 slice 3b): when any captured operand is an expanded
\ hidden-field group, the generic var-row CHECKER-STEP cannot bind it (a hidden
\ field never binds a var). Read the k logical groups directly: a scalar group
\ unifies with its local's type var exactly as the step would; a hidden group
\ stores its TAG term in LOCTV and its cell count in LOCW (an annotated local
\ cannot hold a bundle — reject). Group i (0 = stack top) binds local #LOC-1-i.
: LOC-ANN-BIND-CHECK ( n n -- ) {: gi:n idx:n :}   \ assert the annotation against the bundle
   gi XG-TERM0@  idx cells LOCTV + @  UNIFY
   dup 0=  FAILSET @ 0=  and  OK @ and  IF        \ first failure: capture the exact pair
      idx cells LOCTV + @ FRESH MK-ROW MK-PUSH DEXP !
      gi XG-TERM0@ FRESH MK-ROW MK-PUSH DACT !
      UF>DIAG  -1 FAILSET !
   THEN
   OK @ and OK ! ;
: LOC-BUNDLE-BIND ( n n -- ) {: gi:n idx:n :}
   idx cells LOCTV + @ T-RES ISVAR 0= IF
      gi idx LOC-ANN-BIND-CHECK    \ annotated local: the annotation term must accept
      OK @ 0= IF EXIT THEN         \ this bundle's tag term (scalar annotations reject)
   THEN
   gi XG-TERM0@ T-RES idx cells LOCTV + !
   XG-LEN gi cells + @ idx cells LOCW + !
   XG-LEN gi cells + @  idx cells LOCSEQIX + @ cells LOC-HW + ! ;   \ final width at the bind seq

: LOC-SCALAR-BIND ( n n -- ) {: gi:n idx:n :}
   idx cells LOCTV + @  gi XG-TERM0@  UNIFY OK @ and OK ! ;

: LOC-GROUP-BIND ( n -- ) {: gi:n :}
   #LOC @ 1 - gi - {: idx:n :}
   gi XG-TERM0@ T-RES HIDDEN-PARAM? IF
      gi idx LOC-BUNDLE-BIND
   ELSE
      gi idx LOC-SCALAR-BIND
   THEN ;

: LOC-BIND-GROUPS ( -- )
   LIN-SNAPSHOT
   DCUR @  #LOC @ LGRP @ -  XG-READ 0= IF EXIT THEN
   0 BEGIN dup #LOC @ LGRP @ - < WHILE
      dup LOC-GROUP-BIND
      1 +
   REPEAT drop
   XG-ROW @ DCUR !
   OK @ IF LIN-CHECK THEN ;

: LOC-BIND
   FRESH dup LROW !  MK-ROW LCH !
   LGRP @ BEGIN dup #LOC @ < WHILE
     dup cells LOCTV + @  LCH @ MK-PUSH LCH !
     1 + REPEAT drop
   DCUR @  #LOC @ LGRP @ -  WF-ROW-SCAN   \ width facts at the :} token
   DCUR @  #LOC @ LGRP @ -  XP-BUNDLE-IN-K? IF
      LOC-BIND-GROUPS                  \ a captured operand is a hidden-field bundle
   ELSE
      1 LAYOUT-XPORT !                 \ capturing a local moves the value as one bundle
      LCH @  LROW @ MK-ROW  CHECKER-STEP
      0 LAYOUT-XPORT !
   THEN
   LOC-SHOW-GROUP
   LIN-LOCAL-BIND-CHECK ;

: LOC-TOK {: a u :}
   a u s" :}" CORE-STR= IF 0 LMODE ! LOC-BIND ELSE
   a u s" --" CORE-STR= IF -1 UNCK ! ELSE
   a u LOC-ADD THEN THEN ;

: LOC-REJECT ( -- )
   0 OK !  -1 FAILSET !  -1 LOCALBAD ! ;

: LOC-BEGIN ( -- )
   QDEPTH @ 0 >  DEADP @ or IF LOC-REJECT ELSE
   1 LMODE !  #LOC @ LGRP ! THEN ;

\ LOC-PUSH-REF ( n -- ) : push local idx's binding. A bundle local (LOCW > 1)
\ re-expands its whole group — slot0 deepest up to the stored tag term on top
\ (docs §5); a scalar local pushes its var/term with the deferred-linear taint.
: LOC-PUSH-REF ( n -- ) {: idx:n :}
   idx cells LOCW + @ {: w:n :}
   idx cells LOCTV + @ {: t:n :}
   w 1 > IF
      DCUR @
      0 BEGIN dup w 1 - < WHILE
         dup t swap MK-HIDDEN rot MK-PUSH swap
         1 +
      REPEAT drop
      t swap MK-PUSH DCUR !
   ELSE
      t dup LIN-LOCAL-REF-TAINT  DCUR @ MK-PUSH DCUR !
   THEN ;

\ typed-local-lint: allow-bare-local - a/u preserve the token's ptr u8 role.
: LOC-REF? {: a u :}
   0 LRF !  #LOC @ LI !
   BEGIN LI @ 0 >  LRF @ 0=  and WHILE
     LI @ 1 - LI !
     a u  LOCNB LI @ LOC-NAME-W * +  LI @ cells LOCLN + @  CORE-STR= IF
       QDEPTH @ 0 > IF
          LOC-REJECT
       ELSE
          LI @ LOC-PUSH-REF
       THEN
       -1 LRF ! THEN
   REPEAT  LRF @ ;
\ --- control flow: branch states saved on a CF stack and unified at joins.
\ Both rows are snapshot: A/B = data, RA/RB = return (PLAN: net growth on
\ either row at a back edge is a row-occurs failure).
\ kinds: 1 if  2 if+else  3 begin  4 begin+while  5 do  6 quotation
\        7 case  8 case-of  9 match  10 match-branch (item 9; MF side arena)
\ exit-accumulator save fields: a [: ;] quotation is a nested scope, so its
\ early returns must NOT leak into the enclosing word's accumulator.
$0 constant CF.KND-OFF
$8 constant CF.SA-OFF
$10 constant CF.SB-OFF
$18 constant CF.RA-OFF
$20 constant CF.RB-OFF
$28 constant CF.DED-OFF
$30 constant CF.LN-OFF
$38 constant CF.XRO-OFF
$40 constant CF.XRR-OFF
$48 constant CF.XST-OFF
$50 constant CF.XDP-OFF
$58 constant CF.TXD-OFF
$60 constant CF.TXR-OFF
$68 constant CF.TXS-OFF
$70 constant CF.UNI-OFF
$78 constant CFS-REC
$8 constant CFS-REC-ALIGN
0 constant CFS-REC-PTR-MASK

: CF.KND ( ptr a -- ptr a ) CF.KND-OFF + ;
: CF.SA ( ptr a -- ptr a ) CF.SA-OFF + ;
: CF.SB ( ptr a -- ptr a ) CF.SB-OFF + ;
: CF.RA ( ptr a -- ptr a ) CF.RA-OFF + ;
: CF.RB ( ptr a -- ptr a ) CF.RB-OFF + ;
: CF.DED ( ptr a -- ptr a ) CF.DED-OFF + ;
: CF.LN ( ptr a -- ptr a ) CF.LN-OFF + ;
: CF.XRO ( ptr a -- ptr a ) CF.XRO-OFF + ;
: CF.XRR ( ptr a -- ptr a ) CF.XRR-OFF + ;
: CF.XST ( ptr a -- ptr a ) CF.XST-OFF + ;
: CF.XDP ( ptr a -- ptr a ) CF.XDP-OFF + ;
: CF.TXD ( ptr a -- ptr a ) CF.TXD-OFF + ;
: CF.TXR ( ptr a -- ptr a ) CF.TXR-OFF + ;
: CF.TXS ( ptr a -- ptr a ) CF.TXS-OFF + ;
\ CF.UNI: M5b per-frame block-uniformity. Nonzero when the frame is a control
\ branch whose condition is provably block-uniform (a uniform<bool> predicate),
\ so every lane of the block takes the same path. A block-collective/barrier is
\ sound only when EVERY open frame is uniform (ALL-CF-UNIFORM?).
: CF.UNI ( ptr a -- ptr a ) CF.UNI-OFF + ;

CF.KND-OFF 0 cells CHECKER-LAYOUT=
CF.SA-OFF 1 cells CHECKER-LAYOUT=
CF.SB-OFF 2 cells CHECKER-LAYOUT=
CF.RA-OFF 3 cells CHECKER-LAYOUT=
CF.RB-OFF 4 cells CHECKER-LAYOUT=
CF.DED-OFF 5 cells CHECKER-LAYOUT=
CF.LN-OFF 6 cells CHECKER-LAYOUT=
CF.XRO-OFF 7 cells CHECKER-LAYOUT=
CF.XRR-OFF 8 cells CHECKER-LAYOUT=
CF.XST-OFF 9 cells CHECKER-LAYOUT=
CF.XDP-OFF 10 cells CHECKER-LAYOUT=
CF.TXD-OFF 11 cells CHECKER-LAYOUT=
CF.TXR-OFF 12 cells CHECKER-LAYOUT=
CF.TXS-OFF 13 cells CHECKER-LAYOUT=
CF.UNI-OFF 14 cells CHECKER-LAYOUT=
CFS-REC 15 cells CHECKER-LAYOUT=
CFS-REC-ALIGN CELL CHECKER-LAYOUT=
CFS-REC CFS-REC-ALIGN mod 0 CHECKER-LAYOUT=
CFS-REC-PTR-MASK 0 CHECKER-LAYOUT=
0 CF.KND CF.KND-OFF CHECKER-LAYOUT=
0 CF.SA CF.SA-OFF CHECKER-LAYOUT=
0 CF.SB CF.SB-OFF CHECKER-LAYOUT=
0 CF.RA CF.RA-OFF CHECKER-LAYOUT=
0 CF.RB CF.RB-OFF CHECKER-LAYOUT=
0 CF.DED CF.DED-OFF CHECKER-LAYOUT=
0 CF.LN CF.LN-OFF CHECKER-LAYOUT=
0 CF.XRO CF.XRO-OFF CHECKER-LAYOUT=
0 CF.XRR CF.XRR-OFF CHECKER-LAYOUT=
0 CF.XST CF.XST-OFF CHECKER-LAYOUT=
0 CF.XDP CF.XDP-OFF CHECKER-LAYOUT=
0 CF.TXD CF.TXD-OFF CHECKER-LAYOUT=
0 CF.TXR CF.TXR-OFF CHECKER-LAYOUT=
0 CF.TXS CF.TXS-OFF CHECKER-LAYOUT=
0 CF.UNI CF.UNI-OFF CHECKER-LAYOUT=

create CFS 32 CFS-REC * allot
variable CTMP  variable RTMP  variable INDO
\ EXIT: an early return. XROW accumulates the data row at each exit (all returns,
\ incl. the fall-through at ';', must unify). DEADP marks the current linear path
\ terminated by exit, so the enclosing THEN excludes it from the branch join.
\ CF.DED saves the if-branch's deadness across CF-ELSE. (leave targets the
\ enclosing DO frame's loop-exit row; unloop is a typing no-op — loop control
\ isn't on the typed rows.)
variable RHAS   variable RDIN   variable RDOUT   variable RRIN    variable RROUT

: CF-ROW ( n -- ptr a )
   CFS-REC * CFS + ;

: CF-TOP ( -- ptr a )
   #CFC @ 1 - CF-ROW ;

: CF@DED ( -- n )
   CF-TOP CF.DED @ ;

: CF@DED? ( -- bool )
   CF@DED 0 <> ;

: DEADP? ( -- bool )
   DEADP @ 0 <> ;

: XSET? ( -- bool )
   XSET @ 0 <> ;

: CF-BELOW-CASE? ( -- bool )
   #CFC @ 2 < IF RES-FALSE EXIT THEN
   #CFC @ 2 - CF-ROW CF.KND @ 7 = ;

: CF-CASE-IDX ( -- n )
   #CFC @ 2 - ;

: CF-CASE-HAS? ( n -- bool ) {: idx:n :}
   idx CF-ROW CF.DED @ 0 <> ;

: CF-CASE-HAS! ( n -- ) {: idx:n :}
   -1 idx CF-ROW CF.DED ! ;

: CF-CASE-DATA@ ( n -- n ) {: idx:n :}
   idx CF-ROW CF.SB @ ;

: CF-CASE-RET@ ( n -- n ) {: idx:n :}
   idx CF-ROW CF.RB @ ;

: CF-CASE-DATA! ( n n -- ) {: row:n idx:n :}
   row idx CF-ROW CF.SB ! ;

: CF-CASE-RET! ( n n -- ) {: row:n idx:n :}
   row idx CF-ROW CF.RB ! ;

: CF-PUSH {: k s0 s1 r0 r1 :}
   #CFC @ 31 > IF -1 UNCK ! ELSE
     #CFC @ CF-ROW {: rec:ptr :}
     k rec CF.KND !  s0 rec CF.SA !  s1 rec CF.SB !
     r0 rec CF.RA !  r1 rec CF.RB !
     #LOC @ rec CF.LN !
     0 rec CF.UNI !                        \ default non-uniform; CF-IF marks a uniform<bool> branch
     #CFC @ 1 + #CFC ! THEN ;

: CF@K CF-TOP CF.KND @ ;

: CF@A CF-TOP CF.SA @ ;

: CF@B CF-TOP CF.SB @ ;

: CF@RA CF-TOP CF.RA @ ;

: CF@RB CF-TOP CF.RB @ ;

: CF@LN CF-TOP CF.LN @ ;

: CF-LOC-REST ( -- )
   CF@LN #LOC ! ;

: CF-DROP #CFC @ 1 - #CFC ! ;

: CF-MT? #CFC @ 0 > 0= ;

\ M5b: is EVERY open control frame a proven block-uniform branch? A block
\ collective/barrier is block-uniform-reachable only when no enclosing frame can
\ diverge lanes — one non-uniform frame (a lane-varying if, or ANY begin/do/case/
\ quotation/match frame, which never sets CF.UNI) makes the reach unsound.
: ALL-CF-UNIFORM? ( -- bool )
   0 BEGIN dup #CFC @ < WHILE
      dup CF-ROW CF.UNI @ 0= IF drop RES-FALSE EXIT THEN
      1 +
   REPEAT drop RES-TRUE ;

: CF-FAIL ( -- )
   0 OK !
   -1 FAILSET ! ;

: SUNI {: s :}
   DCUR @ s UNIFY
   dup 0=  FAILSET @ 0=  and  OK @ and  IF s DEXP !  DCUR @ DACT !  UF>DIAG  -1 FAILSET ! THEN
   OK @ and OK ! ;

: SUNI-IN {: s:n :}
   DCUR @ s UNIFY-IN
   dup 0=  FAILSET @ 0=  and  OK @ and  IF s DEXP !  DCUR @ DACT !  UF>DIAG  -1 FAILSET ! THEN
   OK @ and OK ! ;

: SUNI-COERCE {: s:n :}
   DCUR @ s UNIFY-COERCE
   dup 0=  FAILSET @ 0=  and  OK @ and  IF s DEXP !  DCUR @ DACT !  UF>DIAG  -1 FAILSET ! THEN
   OK @ and OK ! ;

: RSUNI {: s :}  RCUR @ s UNIFY OK @ and OK ! ;

: RSUNI-IN {: s:n :}  RCUR @ s UNIFY-IN OK @ and OK ! ;

: ROW-OPEN? ( n -- bool )
   R-RES TAG S-ROW = ;

: CHECK-ROW-NOT-BORROWED ( n -- )
   dup 0= if drop exit then
   ROW-OPEN? 0= if 0 OK ! then ;

: CHECK-NO-BORROW ( -- )
   SGDBASE @ CHECK-ROW-NOT-BORROWED
   SGRBASE @ CHECK-ROW-NOT-BORROWED ;

variable RECEFF   variable RECEFF-ON   variable RECEFF-UEND   variable RECEFF-SYM

: RECEFF-ON? ( -- bool )
   RECEFF-ON @ 0 <> ;

: VSIG-ON? ( -- bool )
   VSIG @ 0 <> ;

: SGSEEN? ( -- bool )
   SGSEEN @ 0 <> ;

: RECURSE-CACHE? ( -- bool )
   VSIG-ON? 0= IF RES-FALSE EXIT THEN
   SGSEEN? 0= IF RES-FALSE EXIT THEN
   RECEFF-ON? ;

\ SIG-EFF-CACHE! ( -- ) : cache the parsed declared sig as an arena effect record
\ so recurse sites instantiate it via E-INST instead of re-parsing the sig text.
\ The record carries sym 0 so signature lookup never sees it.
: SIG-EFF-CACHE!
   SGBAD @ IF EXIT THEN
   SGIN @ EFFECT-MIN-IN drop
   UEND @ RECEFF-UEND !
   CHECKER-REC-SYM @ RECEFF-SYM !
   0 CHECKER-REC-SYM !
   SGIN @ SGOUT @ SGRIN @ SGROUT @ SGHASR @ E-BUILD-EFFECT RECEFF !
   RECEFF-SYM @ CHECKER-REC-SYM !
   -1 RECEFF-ON ! ;

\ SIG-EFF-DROP ( -- ) : truncate the recurse cache record once the body scan is done.
: SIG-EFF-DROP
   RECEFF-ON? 0= IF EXIT THEN
   RECEFF-UEND @ USIGS-RESTORE-END
   0 RECEFF-ON ! ;

: CF-RECURSE-EFF ( ptr a -- ) {: h:ptr :}
   h E-INST-RESET
   h ER.HASR @ RHAS !
   h ER.DIN @ E-INST RDIN !
   h ER.DOUT @ E-INST RDOUT !
   RHAS @ 0 <> IF h ER.RIN @ E-INST RRIN !  h ER.ROUT @ E-INST RROUT ! THEN
   RDIN @ SUNI-IN  RDOUT @ DCUR !
   RHAS @ 0 <> IF RRIN @ RSUNI-IN  RROUT @ RCUR ! THEN
   h LIN-EFF-PASS ;

: CF-RECURSE
   RECURSE-CACHE? IF RECEFF @ E-PTR CF-RECURSE-EFF
   ELSE -1 UNCK ! THEN ;

\ M5b uniform-branch acceptance. `if` normally consumes a plain `bool`. A GPU
\ predicate that is provably identical across every lane of the block is a
\ DISTINCT family `uniform<bool>` (the M2 uniform<T> vs tile<..> split). When the
\ pending condition on top of DCUR is exactly `uniform<bool>`, the `if` is a
\ block-uniform branch: consume it and mark the frame CF.UNI so a collective
\ inside certifies. Any other flag (a bare `bool` from a lane-varying compare, or
\ a `uniform<f32>` which is not a flag at all) keeps the plain STEP-BOOL-IN path.
: COND-UNIFORM? ( -- bool )   \ top of DCUR resolves to uniform<bool>?
   PTX-UNIFORM-FAM @ 0= IF RES-FALSE EXIT THEN
   DCUR @ R-RES dup TAG S-PUSH <> IF drop RES-FALSE EXIT THEN
   P>TYPE T-RES dup TAG T-PARAM <> IF drop RES-FALSE EXIT THEN
   dup PARAM>FAM PTX-UNIFORM-FAM @ <> IF drop RES-FALSE EXIT THEN
   dup PARAM>ARGC 1 <> IF drop RES-FALSE EXIT THEN
   0 PARAM>ARG T-RES dup TAG T-CON <> IF drop RES-FALSE EXIT THEN
   PAY CC-BOOL = ;

: STEP-UNIFORM-BOOL-IN ( -- )   \ consume a uniform<bool> block-uniform predicate
   PARAM-SCR-N @ {: base:n :}
   CC-BOOL MK-CON PARAM-SCR+
   base s" uniform" PTX-UNIFORM-FAM @ MK-PARAM
   STEP-TYPE-IN ;

: CF-IF   \ IF consumes a flag (bool or a block-uniform uniform<bool>)
   COND-UNIFORM? {: uni:bool :}
   uni IF STEP-UNIFORM-BOOL-IN ELSE STEP-BOOL-IN THEN
   #CFC @ {: n0:n :}
   1 DCUR @ 0 RCUR @ 0 CF-PUSH
   uni  #CFC @ n0 >  and IF -1 CF-TOP CF.UNI ! THEN ;

: CF-CASE ( -- )
   7 DCUR @ 0 RCUR @ 0 CF-PUSH
   0 CF-TOP CF.DED ! ;

: CF-CASE-ACCUM ( n -- ) {: idx:n :}
   OK @ 0= IF EXIT THEN
   DEADP @ IF EXIT THEN
   idx CF-CASE-HAS? IF
      idx CF-CASE-DATA@ SUNI
      idx CF-CASE-RET@ RSUNI
   ELSE
      DCUR @ idx CF-CASE-DATA!
      RCUR @ idx CF-CASE-RET!
      idx CF-CASE-HAS!
   THEN ;

: CF-OF ( -- )
   CF-MT? IF CF-FAIL ELSE CF@K 7 <> IF CF-FAIL ELSE
      STEP-N-IN
      CF@A SUNI
      CF@RA RSUNI
      STEP-N-IN
      8 CF@A 0 CF@RA 0 CF-PUSH
   THEN THEN ;

: CF-ENDOF ( -- )
   CF-BELOW-CASE? 0= IF CF-FAIL ELSE CF@K 8 <> IF CF-FAIL ELSE
      CF-CASE-IDX CF-CASE-ACCUM
      CF@A CTMP !  CF@RA RTMP !
      CF-LOC-REST
      0 DEADP !
      CF-DROP
      CTMP @ DCUR !  RTMP @ RCUR !
   THEN THEN ;

: CF-ENDCASE ( -- )
   CF-MT? IF CF-FAIL ELSE CF@K 7 <> IF CF-FAIL ELSE
      DEADP @ 0= IF STEP-N-IN THEN
      #CFC @ 1 - CF-CASE-ACCUM
      CF@DED 0 <> IF
         CF@B DCUR !  CF@RB RCUR !  0 DEADP !
      ELSE
         -1 DEADP !
      THEN
      CF-LOC-REST
      CF-DROP
   THEN THEN ;

: CF-ELSE
   CF-MT? IF CF-FAIL ELSE CF@K 1 <> IF CF-FAIL ELSE
     DEADP @ CF-TOP CF.DED !  0 DEADP !                  \ save if-branch deadness; else runs live
     DCUR @ CTMP !  CF@A DCUR !
     RCUR @ RTMP !  CF@RA RCUR !
     2 CF-TOP CF.KND !
     CTMP @ CF-TOP CF.SB !
     RTMP @ CF-TOP CF.RB !
     CF-LOC-REST
   THEN THEN ;

: CF-THEN-ELSE-MERGE ( -- )
   DEADP? {: else-dead:bool :}
   CF@DED? {: if-dead:bool :}
   else-dead IF
      if-dead IF
         -1 DEADP !
      ELSE
         CF@B DCUR !  CF@RB RCUR !  0 DEADP !
      THEN
   ELSE
      if-dead IF
         0 DEADP !
      ELSE
         CF@B SUNI  CF@RB RSUNI  0 DEADP !
      THEN
   THEN ;

: CF-THEN
   CF-MT? IF CF-FAIL ELSE
     CF@K 1 = IF                                          \ IF ... THEN (no else)
        DEADP? IF CF@A DCUR !  CF@RA RCUR !  0 DEADP !   \ if-branch exited: take fall-through
        ELSE CF@A SUNI  CF@RA RSUNI THEN  CF-LOC-REST  CF-DROP
     ELSE CF@K 2 = IF                                     \ IF ... ELSE ... THEN
        CF-THEN-ELSE-MERGE
        CF-LOC-REST  CF-DROP
     ELSE CF-FAIL THEN THEN THEN ;

: CF-EXIT ( -- )
   XSET @ IF  DCUR @ XROW @ UNIFY OK @ and OK !
              RCUR @ XRROW @ UNIFY OK @ and OK !
   ELSE  DCUR @ XROW !  RCUR @ XRROW !  -1 XSET ! THEN
   -1 DEADP ! ;

: CF-UNLOOP ( -- ) ;

: CF-BEGIN ( -- )
   3 DCUR @ 0 RCUR @ 0 CF-PUSH ;

: CF-UNTIL
   STEP-BOOL-IN
   CF-MT? IF CF-FAIL ELSE CF@K 3 <> IF CF-FAIL ELSE
     CF@A SUNI  CF@A DCUR !  CF@RA RSUNI  CF@RA RCUR !
     CF-LOC-REST  CF-DROP THEN THEN ;

: CF-AGAIN ( -- )
   CF-MT? IF CF-FAIL ELSE CF@K 3 <> IF CF-FAIL ELSE
     CF@A SUNI  CF@A DCUR !  CF@RA RSUNI  CF@RA RCUR !
     CF-LOC-REST  CF-DROP  -1 DEADP ! THEN THEN ;

: CF-WHILE
   STEP-BOOL-IN
   CF-MT? IF CF-FAIL ELSE CF@K 3 <> IF CF-FAIL ELSE
     4 CF-TOP CF.KND !
     DCUR @ CF-TOP CF.SB !
     RCUR @ CF-TOP CF.RB !
   THEN THEN ;

: CF-REPEAT
   CF-MT? IF CF-FAIL ELSE CF@K 4 <> IF CF-FAIL ELSE
     CF@A SUNI  CF@B DCUR !  CF@RA RSUNI  CF@RB RCUR !
     CF-LOC-REST  CF-DROP THEN THEN ;

: CF-DO  STEP-NN-IN  5 DCUR @ 0 RCUR @ 0 CF-PUSH ;

\ At LOOP the exit is always live: ?do/do terminates, and a `leave` jumps here.
\ If the body fall-through is dead (unconditional leave/exit), the back-edge is
\ never taken — skip the body-vs-DO-point unify, but the loop-exit row is still
\ the DO-point row (a zero-trip ?do or a leave both leave exactly that). Live
\ fall-through: the back edge requires a stack-neutral body (CF@A SUNI).
: CF-LOOP
   CF-MT? IF CF-FAIL ELSE CF@K 5 <> IF CF-FAIL ELSE
     DEADP @ IF  0 DEADP !
     ELSE  CF@A SUNI  CF@RA RSUNI  THEN
     CF@A DCUR !  CF@RA RCUR !  CF-LOC-REST  CF-DROP THEN THEN ;

: CF-+LOOP
   STEP-N-IN
   CF-MT? IF CF-FAIL ELSE CF@K 5 <> IF CF-FAIL ELSE
     DEADP @ IF  0 DEADP !
     ELSE  CF@A SUNI  CF@RA RSUNI  THEN
     CF@A DCUR !  CF@RA RCUR !  CF-LOC-REST  CF-DROP THEN THEN ;

: CF-I
   0 INDO !  0 BEGIN dup #CFC @ < WHILE
     dup CF-ROW CF.KND @ 5 = IF -1 INDO ! THEN  1 + REPEAT drop
   INDO @ IF STEP-N-OUT ELSE CF-FAIL THEN ;

: CF-J                                     \ needs two enclosing DO frames
   0 INDO !  0 BEGIN dup #CFC @ < WHILE
     dup CF-ROW CF.KND @ 5 = IF INDO @ 1 + INDO ! THEN  1 + REPEAT drop
   INDO @ 1 > IF STEP-N-OUT ELSE CF-FAIL THEN ;

variable LVDO  variable LVDN
\ CF-FINDDO ( -- ) : LVDO = index of the nearest enclosing DO frame, or -1.
\ Scans top-down and stops at the first DO (kind 5) or quotation boundary
\ (kind 6) — a `leave` inside [: ;] does not escape to an outer loop.
: CF-FINDDO
   -1 LVDO !  0 LVDN !
   #CFC @ 1 -
   BEGIN dup 0 >= LVDN @ 0= and WHILE
     dup CF-ROW CF.KND @ 5 = IF dup LVDO !  -1 LVDN ! THEN
     dup CF-ROW CF.KND @ 6 = IF -1 LVDN ! THEN
     1 - REPEAT drop ;

\ CF-LEAVE : early loop exit. The stack at `leave` must match the loop-exit row
\ (= the DO-point row CF.SA, since the body is stack-neutral); likewise the return
\ row. Then the path to `loop` is dead (CF-LOOP revives the live loop exit).
: CF-LEAVE
   CF-FINDDO
   LVDO @ 0< IF CF-FAIL ELSE
     LVDO @ CF-ROW CF.SA @ SUNI
     LVDO @ CF-ROW CF.RA @ RSUNI
     -1 DEADP ! THEN ;

: CF-QUOT   \ [: — pause the outer inference (incl. its exit state), open a nested one
   6  DCUR @  BROW @  RCUR @  RBROW @  CF-PUSH
   XROW @ CF-TOP CF.XRO !  XRROW @ CF-TOP CF.XRR !
   XSET @ CF-TOP CF.XST !  DEADP @ CF-TOP CF.XDP !
   THDROW @ CF-TOP CF.TXD !  THRROW @ CF-TOP CF.TXR !
   THSET @ CF-TOP CF.TXS !
   0 XSET !  0 DEADP !  0 THSET !
   QDEPTH @ 1 + QDEPTH !
   FRESH MK-ROW dup BROW ! DCUR !
   FRESH MK-ROW dup RBROW ! RCUR ! ;

variable QTMP

: CF-SEMIQ  \ ;] — quot<nested effect> pushed onto the restored outer row
   CF-MT? IF CF-FAIL ELSE CF@K 6 <> IF CF-FAIL ELSE
     XSET @ IF                                   \ fold the quote's OWN early returns into its effect
       DEADP @ IF XROW @ DCUR !  XRROW @ RCUR !
       ELSE DCUR @ XROW @ UNIFY OK @ and OK !  RCUR @ XRROW @ UNIFY OK @ and OK ! THEN
     THEN
     BROW @  DCUR @  RBROW @  RCUR @  MK-QUOT QTMP !
     QTMP @ THSET @ DEADP @ XSET @ 0= and THDROW @ THRROW @ QX!
     CF-TOP CF.XRO @ XROW !  CF-TOP CF.XRR @ XRROW !
     CF-TOP CF.XST @ XSET !  CF-TOP CF.XDP @ DEADP !  \ restore outer exit state
     CF-TOP CF.TXD @ THDROW !  CF-TOP CF.TXR @ THRROW !
     CF-TOP CF.TXS @ THSET !
     QDEPTH @ 1 - QDEPTH !
     CF@B BROW !  CF@RB RBROW !
     CF@RA RCUR !
     QTMP @  CF@A  MK-PUSH DCUR !
     CF-LOC-REST
     CF-DROP THEN THEN ;

\ --- MATCH eliminator (item 9 slice 3, docs/type-families.md §13-14). `MATCH
\ family  variant OF ... ENDOF ...  ;MATCH` is a checker control form: the
\ family and variant tokens are captured in DO-TOK1 before locals/control/word
\ dispatch (the construct discipline), OF/ENDOF are shared with CASE and
\ distinguished by the enclosing CFS frame kind (9 = match, 10 = match
\ branch; CASE stays 7/8). MATCH pops the scrutinee's hidden-field bundle
\ (slot0..tag, verified cell by cell against the resolved family), recovers
\ the family type args from the tag term, and each branch checks with
\ DCUR = base + that variant's instantiated payload. Branch outputs unify at
\ ENDOF; ;MATCH enforces exhaustiveness over the declaration-order tag bitset
\ (v1 has no default branch) and installs the joined rows. Frames live in a
\ growable side arena (MF) with a growable seen-bitset pool (MSEEN) — never a
\ silent-uncheckable path: every structural failure latches MREJ, which
\ CHECK-VERDICT ranks as a hard reject above UNCK, and the CFS headroom check
\ (two frames per match) rejects instead of tripping CF-PUSH's UNCK overflow.
\ v1 scrutinee rule: only a width-expanded bundle matches; an open-arg
\ parametric value (one conservative logical cell, possibly linear) rejects
\ until whole-bundle MATCH consumption lands with the TFAM 11 tail.
$0 constant MF.FAM-OFF
$8 constant MF.TERM-OFF
$10 constant MF.BASE-OFF
$18 constant MF.RBASE-OFF
$20 constant MF.OUT-OFF
$28 constant MF.ROUT-OFF
$30 constant MF.HAS-OFF
$38 constant MF.SEEN-OFF
$40 constant MF.VCNT-OFF
$48 constant MF.TIX-OFF
$50 constant MF-REC
$8 constant MF-REC-ALIGN
0 constant MF-REC-PTR-MASK

: MF.FAM ( ptr a -- ptr a ) MF.FAM-OFF + ;
: MF.TERM ( ptr a -- ptr a ) MF.TERM-OFF + ;
: MF.BASE ( ptr a -- ptr a ) MF.BASE-OFF + ;
: MF.RBASE ( ptr a -- ptr a ) MF.RBASE-OFF + ;
: MF.OUT ( ptr a -- ptr a ) MF.OUT-OFF + ;
: MF.ROUT ( ptr a -- ptr a ) MF.ROUT-OFF + ;
: MF.HAS ( ptr a -- ptr a ) MF.HAS-OFF + ;
: MF.SEEN ( ptr a -- ptr a ) MF.SEEN-OFF + ;
: MF.VCNT ( ptr a -- ptr a ) MF.VCNT-OFF + ;
: MF.TIX ( ptr a -- ptr a ) MF.TIX-OFF + ;

MF.FAM-OFF 0 cells CHECKER-LAYOUT=
MF.TERM-OFF 1 cells CHECKER-LAYOUT=
MF.BASE-OFF 2 cells CHECKER-LAYOUT=
MF.RBASE-OFF 3 cells CHECKER-LAYOUT=
MF.OUT-OFF 4 cells CHECKER-LAYOUT=
MF.ROUT-OFF 5 cells CHECKER-LAYOUT=
MF.HAS-OFF 6 cells CHECKER-LAYOUT=
MF.SEEN-OFF 7 cells CHECKER-LAYOUT=
MF.VCNT-OFF 8 cells CHECKER-LAYOUT=
MF.TIX-OFF 9 cells CHECKER-LAYOUT=
MF-REC 10 cells CHECKER-LAYOUT=
MF-REC-ALIGN CELL CHECKER-LAYOUT=
MF-REC MF-REC-ALIGN mod 0 CHECKER-LAYOUT=
MF-REC-PTR-MASK 0 CHECKER-LAYOUT=
0 MF.FAM MF.FAM-OFF CHECKER-LAYOUT=
0 MF.TERM MF.TERM-OFF CHECKER-LAYOUT=
0 MF.BASE MF.BASE-OFF CHECKER-LAYOUT=
0 MF.RBASE MF.RBASE-OFF CHECKER-LAYOUT=
0 MF.OUT MF.OUT-OFF CHECKER-LAYOUT=
0 MF.ROUT MF.ROUT-OFF CHECKER-LAYOUT=
0 MF.HAS MF.HAS-OFF CHECKER-LAYOUT=
0 MF.SEEN MF.SEEN-OFF CHECKER-LAYOUT=
0 MF.VCNT MF.VCNT-OFF CHECKER-LAYOUT=
0 MF.TIX MF.TIX-OFF CHECKER-LAYOUT=

8 constant MF-CAP-INIT
variable MF-CAP-V   MF-CAP-INIT MF-CAP-V !
create MF-A-BOOT   MF-CAP-INIT MF-REC * allot
variable MF-A-P    MF-A-BOOT MF-A-P !
: MF-ARENA ( -- ptr a ) MF-A-P @ ;
variable MF-DEPTH   0 MF-DEPTH !

: MF-GROW ( -- )
   MF-CAP-V @ 2 * {: nc:n :}
   MF-A-P  MF-CAP-V @ MF-REC *  nc MF-REC *  REG-GROW1
   nc MF-CAP-V ! ;

: MF-ENSURE ( -- )
   MF-DEPTH @ MF-CAP-V @ < IF exit THEN
   MF-GROW ;

: MF-CUR ( -- ptr a )
   MF-DEPTH @ 1 - MF-REC * MF-ARENA + ;

8 constant MSEEN-CAP-INIT
variable MSEEN-CAP-V   MSEEN-CAP-INIT MSEEN-CAP-V !
create MSEEN-BOOT   MSEEN-CAP-INIT cells allot
variable MSEEN-P   MSEEN-BOOT MSEEN-P !
: MSEEN-POOL ( -- ptr a ) MSEEN-P @ ;
variable MSEEN-N   0 MSEEN-N !
variable MSEEN-I

: MSEEN-GROW ( n -- ) {: need:n :}
   need MSEEN-CAP-V @ 2 * max {: nc:n :}
   MSEEN-P  MSEEN-CAP-V @ cells  nc cells  REG-GROW1
   nc MSEEN-CAP-V ! ;

: MSEEN-ENSURE ( n -- ) {: add:n :}
   MSEEN-N @ add + MSEEN-CAP-V @ <= IF exit THEN
   MSEEN-N @ add + MSEEN-GROW ;

: MSEEN-ALLOC ( n -- n ) {: count:n :}   \ zeroed bitset cells for count variants -> offset
   count 63 + 64 / {: k:n :}
   k MSEEN-ENSURE
   MSEEN-N @ {: off:n :}
   0 MSEEN-I !
   BEGIN MSEEN-I @ k < WHILE
      0  MSEEN-POOL off MSEEN-I @ + cells + !
      MSEEN-I @ 1 + MSEEN-I !
   REPEAT
   MSEEN-N @ k + MSEEN-N !
   off ;

: MSEEN-CELL ( n n -- ptr a ) {: off:n tag:n :}
   MSEEN-POOL  off tag 64 / + cells  + ;

: MSEEN-BIT ( n -- n )
   63 and 1 swap lshift ;

: MSEEN-GET ( n n -- bool ) {: off:n tag:n :}
   off tag MSEEN-CELL @  tag MSEEN-BIT  and 0 <> ;

: MSEEN-SET ( n n -- ) {: off:n tag:n :}
   off tag MSEEN-CELL dup @  tag MSEEN-BIT or  swap ! ;

: MSEEN-FULL? ( n n -- bool ) {: off:n count:n :}
   0 MSEEN-I !
   BEGIN MSEEN-I @ count < WHILE
      off MSEEN-I @ MSEEN-GET 0= IF RES-FALSE EXIT THEN
      MSEEN-I @ 1 + MSEEN-I !
   REPEAT RES-TRUE ;

variable MM      \ 0 off | 1 expecting family | 2 expecting variant or ;match | 3 expecting of
variable MPEND   \ pending variant SUMV id between the variant token and its OF (-1 poisoned)
variable MREJ    \ match structural-reject latch: forces verdict 0, never uncheckable

\ --- item 9 slice 4: match/construct reject reasons (docs §24). Failure sites
\ latch a reason code alongside the token pin; render.f maps it to a stable
\ E-MATCH-*/E-CONSTRUCT-* code, repair class, suggestion, and prose. First
\ reason wins and only while the token pin is still open (FAILSET clear), so
\ the reason always describes the pinned token. The nonexhaustive reason also
\ latches (family, seen-bitset offset, variant count) so the renderer can walk
\ the unseen declaration-order tags to variant NAMES.
1 constant MD-FAM-UNKNOWN     \ match family token resolves nothing in signature scope
2 constant MD-FAM-KIND        \ match family is not a sum or enum
3 constant MD-SCRUT           \ top of stack is not a sum/enum bundle
4 constant MD-FAM-MISMATCH    \ top bundle belongs to a different family
5 constant MD-VAR-UNKNOWN     \ variant not declared by the matched family
6 constant MD-VAR-DUP         \ duplicate variant branch
7 constant MD-MISSING-OF      \ variant token not followed by OF
8 constant MD-NONEXH          \ ;MATCH with unseen variants (no default branch in v1)
9 constant MD-STRAY           \ stray ;match / closer over a foreign frame
10 constant MD-TRUNC          \ unterminated match form at definition end
11 constant MD-DEPTH          \ match cannot reserve its two control frames
12 constant MD-QUOT           \ match inside a quotation: open rows carry no scrutinee
13 constant MD-OPEN-ARGS      \ open-arg parametric scrutinee (v1 conservative cell)
14 constant MD-JOIN           \ branch outputs do not unify
15 constant MD-CON-FAM        \ construct family not declared in the active package
16 constant MD-CON-KIND       \ construct family is not a sum or enum
17 constant MD-CON-VAR        \ construct variant not declared by the family
18 constant MD-CON-TRUNC      \ construct missing its family/variant operand
19 constant MD-DIVBAR         \ M5: block collective/barrier reached under divergent control
20 constant MD-EXEC-OPAQUE    \ execute of an opaque xt fetched from untyped memory (RSEXEC T-VAR)
21 constant MD-CATCH-OPAQUE   \ catch of an opaque xt fetched from untyped memory (RSCATCH T-VAR); same E-EXEC-OPAQUE-XT code, prose names 'catch'
22 constant MD-RIGID-REGION   \ two host allocations differ in host region (which allocation)
23 constant MD-RIGID-EXTENT   \ two host allocations differ in extent (bounds)
24 constant MD-RIGID-GEN      \ stale mutation generation: index outlived the container's epoch
25 constant MD-RIGID-XDOM     \ rigid-identity domain confusion: a region/extent/generation used where another is required

variable MDIAG        \ latched reason code (0 = none; reset per definition)
variable MDIAG-FAM    \ nonexhaustive: family id for the name walk
variable MDIAG-SEEN   \ nonexhaustive: seen-bitset offset (MSEEN pool, per-check)
variable MDIAG-VCNT   \ nonexhaustive: variant count

: MDIAG! ( n -- ) {: code:n :}   \ first reason wins, only while the pin is open
   MDIAG @ 0 <> IF EXIT THEN
   FAILSET @ 0 <> IF EXIT THEN
   code MDIAG ! ;

\ Rigid host-identity mismatch naming (dot habu-define-rigid-host). RIGID-ATOM-DOMAIN
\ classifies a resolved term as a rigid-identity atom of a given domain (0 = none).
\ RIGID-DIAG-CLASSIFY runs on the captured mismatch pair after the verdict is a
\ reject: when both sides are rigid-domain atoms it names WHICH bug it is, so a
\ stale index, a wrong region, and a wrong extent are distinguishable rejects.
: RIGID-ATOM-DOMAIN ( n -- n )   \ 0 none / 1 region / 2 extent / 3 gen
   T-RES dup TAG T-ATOM <> IF drop 0 EXIT THEN
   dup ATOM>K 0 <= IF drop 0 EXIT THEN            \ only an instantiated fresh id carries a domain
   dup ATOM>A swap ATOM>U {: a:ptr u:n :}
   u 7 >= IF a 7 s" region-" CORE-STR= IF 1 EXIT THEN THEN
   u 7 >= IF a 7 s" extent-" CORE-STR= IF 2 EXIT THEN THEN
   u 4 >= IF a 4 s" gen-"    CORE-STR= IF 3 EXIT THEN THEN
   0 ;
: RIGID-DIAG-CLASSIFY ( -- )
   MDIAG @ IF EXIT THEN                           \ a match/construct reason already won
   DF-EXP @ 0= IF EXIT THEN
   DF-ACT @ 0= IF EXIT THEN
   DF-EXP @ RIGID-ATOM-DOMAIN {: de:n :}
   DF-ACT @ RIGID-ATOM-DOMAIN {: da:n :}
   de 0= IF EXIT THEN
   da 0= IF EXIT THEN                             \ both sides must be rigid-domain atoms
   de da <> IF MD-RIGID-XDOM MDIAG ! EXIT THEN    \ different domains -> identity confusion
   de 1 = IF MD-RIGID-REGION MDIAG ! EXIT THEN
   de 2 = IF MD-RIGID-EXTENT MDIAG ! EXIT THEN
   MD-RIGID-GEN MDIAG ! ;

: MATCH-REJECT ( -- )
   0 OK !  -1 FAILSET !  -1 MREJ !  0 MM ! ;

: MATCH-BEGIN ( -- )
   1 MM ! ;   \ enter match mode; an unarmed registry fails closed at the family resolve (MATCH-FAM-XT default rejects)

variable MTCH-ROW   variable MTCH-I   variable MTCH-TAGT

: MATCH-SCRUT-CELL? ( n n n -- bool ) {: fam:n w:n j:n :}   \ one bundle level, top-down
   MTCH-ROW @ R-RES TAG S-PUSH <> IF RES-FALSE EXIT THEN
   MTCH-ROW @ R-RES P>TYPE T-RES {: t:n :}
   t HIDDEN-PARAM? 0= IF RES-FALSE EXIT THEN
   t PARAM>FAM fam <> IF RES-FALSE EXIT THEN
   t HIDDEN-SLOT@  w 1 - j -  <> IF RES-FALSE EXIT THEN
   j 0 = IF t MTCH-TAGT ! THEN
   MTCH-ROW @ R-RES P>REST MTCH-ROW !
   RES-TRUE ;

: MATCH-SCRUT? ( n -- bool ) {: fam:n :}   \ verify + walk off the W-cell bundle
   DCUR @ R-RES {: node:n :}
   node TAG S-PUSH <> IF RES-FALSE EXIT THEN
   node P>TYPE T-WIDTH {: w:n :}            \ arg-aware bundle width from the scrutinee tag
   DCUR @ MTCH-ROW !  0 MTCH-I !  0 MTCH-TAGT !
   BEGIN MTCH-I @ w < WHILE
      fam w MTCH-I @ MATCH-SCRUT-CELL? 0= IF RES-FALSE EXIT THEN
      MTCH-I @ 1 + MTCH-I !
   REPEAT RES-TRUE ;

: MATCH-SCRUT-DIAG ( n -- ) {: fam:n :}   \ classify a failed scrutinee for §24
   QDEPTH @ 0 > IF MD-QUOT MDIAG! EXIT THEN
   DCUR @ R-RES TAG S-PUSH <> IF MD-SCRUT MDIAG! EXIT THEN
   DCUR @ R-RES P>TYPE T-RES {: t:n :}
   t LAYOUT-PARAM? 0= IF MD-SCRUT MDIAG! EXIT THEN
   t PARAM>FAM fam <> IF MD-FAM-MISMATCH MDIAG! EXIT THEN
   t HIDDEN-PARAM? 0= IF MD-OPEN-ARGS MDIAG! EXIT THEN
   MD-SCRUT MDIAG! ;

: MATCH-FAM-TOK ( ptr u8 n -- ) {: a:ptr u:n :}
   a u MATCH-FAM-XT 0= IF drop MATCH-REJECT EXIT THEN
   {: fam:n :}
   #CFC @ 30 > IF MD-DEPTH MDIAG! MATCH-REJECT EXIT THEN   \ two CFS frames per match: reject, never UNCK
   fam MATCH-SCRUT? 0= IF fam MATCH-SCRUT-DIAG MATCH-REJECT EXIT THEN
   MF-ENSURE
   MF-DEPTH @ 1 + MF-DEPTH !
   MF-CUR {: r:ptr :}
   fam r MF.FAM !
   MTCH-TAGT @ r MF.TERM !
   MTCH-ROW @ r MF.BASE !
   RCUR @ r MF.RBASE !
   0 r MF.OUT !  0 r MF.ROUT !  0 r MF.HAS !
   fam MATCH-VCOUNT-XT dup r MF.VCNT !
   MSEEN-ALLOC r MF.SEEN !
   TOKIX @ r MF.TIX !
   MTCH-ROW @ DCUR !                         \ bundle popped; branches re-derive their rows
   9 DCUR @ 0 RCUR @ 0 CF-PUSH
   2 MM ! ;

: MATCH-ACCUM ( -- )
   OK @ 0= IF EXIT THEN
   DEADP @ IF EXIT THEN
   MF-CUR MF.HAS @ IF
      FAILSET @ {: fs0:n :}                   \ SUNI pins the token itself: latch the
      MF-CUR MF.OUT @ SUNI                    \ join reason only when THIS unify failed
      MF-CUR MF.ROUT @ RSUNI
      OK @ 0=  fs0 0=  and  MDIAG @ 0=  and IF MD-JOIN MDIAG ! THEN
   ELSE
      DCUR @ MF-CUR MF.OUT !
      RCUR @ MF-CUR MF.ROUT !
      -1 MF-CUR MF.HAS !
   THEN ;

: MATCH-ENDOF ( -- )   \ dispatcher-guarded: top CFS frame is the kind-10 branch
   MATCH-ACCUM
   CF-LOC-REST
   CF-DROP
   0 DEADP !
   MF-CUR MF.BASE @ DCUR !
   MF-CUR MF.RBASE @ RCUR !
   2 MM ! ;

: MATCH-SEMI ( -- )    \ ;match at variant-list level: exhaustiveness + join
   CF-MT? IF MD-STRAY MDIAG! MATCH-REJECT EXIT THEN
   CF@K 9 <> IF MD-STRAY MDIAG! MATCH-REJECT EXIT THEN
   MF-CUR MF.SEEN @ MF-CUR MF.VCNT @ MSEEN-FULL? 0= IF
      MDIAG @ 0=  FAILSET @ 0=  and IF          \ latch the name-walk data with the reason
         MD-NONEXH MDIAG !
         MF-CUR MF.FAM @ MDIAG-FAM !
         MF-CUR MF.SEEN @ MDIAG-SEEN !
         MF-CUR MF.VCNT @ MDIAG-VCNT !
      THEN
      0 OK !  -1 MREJ !
   THEN
   MF-CUR MF.HAS @ IF
      MF-CUR MF.OUT @ DCUR !
      MF-CUR MF.ROUT @ RCUR !
      0 DEADP !
   ELSE
      MF-CUR MF.BASE @ DCUR !
      MF-CUR MF.RBASE @ RCUR !
      -1 DEADP !                             \ every branch exited: no normal continuation
   THEN
   CF-LOC-REST
   CF-DROP
   MF-DEPTH @ 1 - MF-DEPTH !
   0 MM ! ;

: MATCH-VARIANT-TOK ( ptr u8 n -- ) {: a:ptr u:n :}
   a u s" ;match" CORE-STR= IF MATCH-SEMI EXIT THEN
   a u MF-CUR MF.FAM @ MATCH-VAR-XT 0= IF
      drop MD-VAR-UNKNOWN MDIAG! 0 OK !  -1 MREJ !  -1 MPEND !  3 MM !  EXIT THEN
   {: vid:n :}
   vid MATCH-VTAG-XT {: tag:n :}
   MF-CUR MF.SEEN @ tag MSEEN-GET IF MD-VAR-DUP MDIAG! 0 OK !  -1 MREJ ! THEN   \ duplicate variant
   MF-CUR MF.SEEN @ tag MSEEN-SET
   vid MPEND !
   3 MM ! ;

: MATCH-OF-TOK ( ptr u8 n -- ) {: a:ptr u:n :}
   a u s" of" CORE-STR= 0= IF MD-MISSING-OF MDIAG! 0 OK !  -1 MREJ ! THEN   \ variant token without OF
   MF-CUR {: r:ptr :}
   r MF.BASE @ DCUR !
   r MF.RBASE @ RCUR !
   MPEND @ 0 < 0= IF
      MPEND @  r MF.TERM @  DCUR @  MATCH-PAY-XT DCUR !
   THEN
   10  r MF.BASE @  0  r MF.RBASE @  0  CF-PUSH
   0 DEADP !
   0 MM ! ;

: MATCH-TOK ( ptr u8 n -- ) {: a:ptr u:n :}
   MM @ 1 = IF a u MATCH-FAM-TOK EXIT THEN
   MM @ 2 = IF a u MATCH-VARIANT-TOK EXIT THEN
   a u MATCH-OF-TOK ;

: CF-ENDOF-DISPATCH ( -- )   \ ENDOF serves CASE (7/8) and MATCH (10) by frame kind
   CF-MT? IF CF-ENDOF EXIT THEN
   CF@K 10 = IF MATCH-ENDOF EXIT THEN
   CF-ENDOF ;

: CF-TOK? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" [:" CORE-STR= IF CF-QUOT RES-TRUE EXIT THEN
   a u s" ;]" CORE-STR= IF CF-SEMIQ RES-TRUE EXIT THEN
   a u s" if" CORE-STR= IF CF-IF RES-TRUE EXIT THEN
   a u s" else" CORE-STR= IF CF-ELSE RES-TRUE EXIT THEN
   a u s" then" CORE-STR= IF CF-THEN RES-TRUE EXIT THEN
   a u s" case" CORE-STR= IF CF-CASE RES-TRUE EXIT THEN
   a u s" of" CORE-STR= IF CF-OF RES-TRUE EXIT THEN
   a u s" endof" CORE-STR= IF CF-ENDOF-DISPATCH RES-TRUE EXIT THEN
   a u s" endcase" CORE-STR= IF CF-ENDCASE RES-TRUE EXIT THEN
   a u s" ;match" CORE-STR= IF MD-STRAY MDIAG! CF-FAIL RES-TRUE EXIT THEN   \ stray ;match: hard reject
   a u s" begin" CORE-STR= IF CF-BEGIN RES-TRUE EXIT THEN
   a u s" until" CORE-STR= IF CF-UNTIL RES-TRUE EXIT THEN
   a u s" again" CORE-STR= IF CF-AGAIN RES-TRUE EXIT THEN
   a u s" while" CORE-STR= IF CF-WHILE RES-TRUE EXIT THEN
   a u s" repeat" CORE-STR= IF CF-REPEAT RES-TRUE EXIT THEN
   a u s" do" CORE-STR= IF CF-DO RES-TRUE EXIT THEN
   a u s" ?do" CORE-STR= IF CF-DO RES-TRUE EXIT THEN
   a u s" loop" CORE-STR= IF CF-LOOP RES-TRUE EXIT THEN
   a u s" +loop" CORE-STR= IF CF-+LOOP RES-TRUE EXIT THEN
   a u s" i" CORE-STR= IF CF-I RES-TRUE EXIT THEN
   a u s" j" CORE-STR= IF CF-J RES-TRUE EXIT THEN
   a u s" exit" CORE-STR= IF CF-EXIT RES-TRUE EXIT THEN
   a u s" leave" CORE-STR= IF CF-LEAVE RES-TRUE EXIT THEN
   a u s" unloop" CORE-STR= IF CF-UNLOOP RES-TRUE EXIT THEN
   a u s" recurse" CORE-STR= IF CF-RECURSE RES-TRUE EXIT THEN
   RES-FALSE ;
\ first token of the checked text is the word's NAME (skipped, kept for the
\ recorder). RECXT (installed by render.f) records certified sigs by name; DIAGXT
\ (installed by render.f) renders a reject/uncheckable diagnostic. Both are
\ `defer`s with no-op DEFAULTS so the calls are statically effect-known and render
\ rebinds each to the real word. DIAG-QUIET counts nested quiet-check scopes: the
\ candidate-probe harness suppresses diagnostics by bumping it, and the firer
\ skips the render while it is non-zero — replacing the old "zero the hook to
\ suppress" trick, which a `defer` cannot express as data.
variable TOK0
defer RECXT ( ptr u8 n -- )
defer DIAGXT ( -- )
variable DIAG-QUIET
: DIAG-HOOK-DEFAULTS ( -- )
   [: 2drop ;] is RECXT
   [: ;] is DIAGXT ;
DIAG-HOOK-DEFAULTS
variable CTLNEW
\ the engine folds A-Z in keyword and dict matching — fold every token the same
\ way (into a scratch copy: the source text may live in the read-only image).
variable TKFU
variable SKI  variable SKF

: SGBAD-IN-SOURCE? ( -- bool )
   SGBAD-U @ 0= IF RES-FALSE EXIT THEN
   SGBAD-A @ TBASE @ < IF RES-FALSE EXIT THEN
   SGBAD-A @ SGBAD-U @ + TBASE @ TBLEN @ + > IF RES-FALSE EXIT THEN
   RES-TRUE ;

: SGBAD-COPY-TOKEN ( -- )
   SGBAD-U @ TOKBUF-ENSURE
   SGBAD-A @ FAILTK SGBAD-U @ CCOPY
   SGBAD-U @ FAILTU ! ;

: SGBAD-SPAN! ( -- )
   SGBAD-IN-SOURCE? IF
      SGBAD-A @ TBASE @ - FAILB !
      FAILB @ SGBAD-U @ + FAILE !
   ELSE
      TSTART @ FAILB !
      TI @ FAILE !
   THEN ;

: SGBAD-FAIL! ( -- )
   SGBAD @ 0= IF exit THEN
   FAILSET @ IF exit THEN
   SGBAD-COPY-TOKEN
   SGBAD-SPAN!
   0 FAILIX !
   -1 FAILSET ! ;

: CHECKER-BYTE@ ( ptr u8 n -- n )
   + c@ ;

: CHECKER-SC-LEAD? ( n -- bool )
   CHECKER-FOLD-C dup $73 = swap $63 = or ;

: CHECKER-STRING-LEAD? ( n -- bool )
   dup CHECKER-SC-LEAD? swap $2E = or ;

: NORMAL-STRING-OPENER? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   u 2 <> IF RES-FALSE EXIT THEN
   a 1 CHECKER-BYTE@ $22 <> IF RES-FALSE EXIT THEN
   a 0 CHECKER-BYTE@ CHECKER-STRING-LEAD? ;

: ESCAPED-STRING-OPENER? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   u 3 <> IF RES-FALSE EXIT THEN
   a 1 CHECKER-BYTE@ $5C <> IF RES-FALSE EXIT THEN
   a 2 CHECKER-BYTE@ $22 <> IF RES-FALSE EXIT THEN
   a 0 CHECKER-BYTE@ CHECKER-STRING-LEAD? ;

: STRING-OPENER? ( ptr u8 n -- bool )
   2dup NORMAL-STRING-OPENER? IF 2drop RES-TRUE EXIT THEN
   ESCAPED-STRING-OPENER? ;

: PARSE-LIT? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" [char]" CORE-STR= IF RES-TRUE EXIT THEN
   a u s" char" CORE-STR= ;

: SKIP-STRING-PAYLOAD
   TI @ SKI !  0 SKF !
   BEGIN SKI @ TBLEN @ <  SKF @ 0=  and WHILE
      SKI @ TBYTE@ 34 = IF -1 SKF ! ELSE SKI @ 1 + SKI ! THEN
   REPEAT
   SKF @ IF SKI @ 1 + TI ! ELSE TBLEN @ TI ! 0 OK ! THEN ;

\ escape validation mirrors the engine decoder (C-ESC-DECODE-BASIC/C-ESC-HEX-X9,
\ habu2.f): \" \q \\ \a \b \e \l \f \n \r \t \v \z and \xHH / \XHH only.
: ESC-HEX-DIGIT? ( n -- bool ) {: c:n :}
   c $30 >= c $39 <= and IF RES-TRUE EXIT THEN
   c $61 >= c $66 <= and IF RES-TRUE EXIT THEN
   c $41 >= c $46 <= and ;

: ESC-SIMPLE? ( n -- bool ) {: c:n :}
   c $22 = c $5C = or c $61 = or c $62 = or c $65 = or c $66 = or
   c $6C = or c $6E = or c $71 = or c $72 = or c $74 = or c $76 = or
   c $7A = or ;

: ESC-HEX-LEAD? ( n -- bool ) {: c:n :}
   c $78 = c $58 = or ;

: SKIP-ESC-BYTE@ ( -- n )
   SKI @ TBYTE@ ;

\ SKIP-ESC-BAD ( -- ) : invalid escape — spend the rest of the payload so the
\ tail branch rejects the definition exactly like an unterminated string.
: SKIP-ESC-BAD
   TBLEN @ SKI ! ;

: SKIP-ESC-HEX ( -- )   \ SKI at 'x'/'X': require two hex digits, then continue
   SKI @ 2 + TBLEN @ >= IF SKIP-ESC-BAD EXIT THEN
   SKI @ 1 + TBYTE@ ESC-HEX-DIGIT? 0= IF SKIP-ESC-BAD EXIT THEN
   SKI @ 2 + TBYTE@ ESC-HEX-DIGIT? 0= IF SKIP-ESC-BAD EXIT THEN
   SKI @ 3 + SKI ! ;

: SKIP-ESC-SEQ ( -- )   \ SKI at '\'
   SKI @ 1 + SKI !
   SKI @ TBLEN @ >= IF SKIP-ESC-BAD EXIT THEN
   SKIP-ESC-BYTE@ ESC-SIMPLE? IF SKI @ 1 + SKI ! EXIT THEN
   SKIP-ESC-BYTE@ ESC-HEX-LEAD? IF SKIP-ESC-HEX EXIT THEN
   SKIP-ESC-BAD ;

: SKIP-ESCAPED-STRING-PAYLOAD ( -- )
   TI @ SKI !  0 SKF !
   BEGIN SKI @ TBLEN @ <  SKF @ 0=  and WHILE
      SKIP-ESC-BYTE@ 92 = IF SKIP-ESC-SEQ ELSE
         SKIP-ESC-BYTE@ 34 = IF -1 SKF ! ELSE SKI @ 1 + SKI ! THEN
      THEN
   REPEAT
   SKF @ IF SKI @ 1 + TI ! ELSE TBLEN @ TI ! 0 OK ! THEN ;

: SKIP-PARSE-LIT-PAYLOAD ( -- )
   BEGIN TI @ TBLEN @ < IF TI @ TBYTE@ 32 <= ELSE 0 0= 0= THEN WHILE
      TI @ 1 + TI !
   REPEAT
   TI @ TBLEN @ >= IF 0 OK ! exit THEN
   BEGIN TI @ TBLEN @ < IF TI @ TBYTE@ 32 > ELSE 0 0= 0= THEN WHILE
      TI @ 1 + TI !
   REPEAT ;

: DEAD-OWNER! ( ptr u8 n -- )
   DEADTU !  DEADTA ! ;

: DEAD-CLOSE? {: a u :}
   a u s" else"   CORE-STR= IF RES-TRUE EXIT THEN
   a u s" then"   CORE-STR= IF RES-TRUE EXIT THEN
   a u s" loop"   CORE-STR= IF RES-TRUE EXIT THEN
   a u s" +loop"  CORE-STR= IF RES-TRUE EXIT THEN
   a u s" endof"  CORE-STR= IF RES-TRUE EXIT THEN
   a u s" endcase" CORE-STR= IF RES-TRUE EXIT THEN
   a u s" repeat" CORE-STR= IF RES-TRUE EXIT THEN
   a u s" again"  CORE-STR= IF RES-TRUE EXIT THEN
   a u s" ;]"     CORE-STR= IF RES-TRUE EXIT THEN
   RES-FALSE ;

: LIVE-TOKEN? {: a u :}
   DEADP @ 0= IF RES-TRUE EXIT THEN
   a u DEAD-CLOSE? ;

: TOKFOLD ( ptr u8 n -- bool ) {: a:ptr u:n :}
   u TOKBUF-ENSURE
   0 BEGIN dup u < WHILE
     dup a + c@  dup 64 >  over 91 <  and IF 32 or THEN
     over TKF + c!  1 +
   REPEAT drop
   u TKFU !  RES-TRUE ;
: FAIL-SPAN! ( -- )
   TSTART @ FAILB !
   FAILB @ FAILTU @ + FAILE ! ;
: FAIL-PIN! ( ptr u8 n -- )
   {: a:ptr u:n :}
   u TOKBUF-ENSURE
   a FAILTK u CCOPY
   u FAILTU !
   TOKIX @ FAILIX !
   FAIL-SPAN! ;

: CAP-FAIL ( ptr u8 n -- )
   FAILSET @ 0= IF FAIL-PIN! ELSE 2drop THEN ;
create DIAGFB 256 allot   variable DIAGFU
variable DIAGL0  variable DIAGC0  variable DIAGB0
: DIAG-FILE! ( ptr u8 n -- ) {: a:ptr u:n :}
   u 255 > IF s" diag: file path too long" 76 die THEN
   0 BEGIN dup u < WHILE
      dup a + c@  over DIAGFB + c!
      1 +
   REPEAT drop
   u DIAGFU ! ;
: DIAG-ORIGIN! {: line col byte :}
   line DIAGL0 !  col DIAGC0 !  byte DIAGB0 ! ;
\ Set DIAG-ORIGIN! to the FILE position of a definition's name token, given the
\ eval-buffer base ptr, the name-token ptr into that buffer, and the buffer
\ start's own file line/col/byte. Mirrors verify-source ABS-ORIGIN so the native
\ load path reports the same file-relative positions as the re-driver.
variable DOS-OFF  variable DOS-LN  variable DOS-CL  variable DOS-P
: DIAG-ORIGIN-SPAN! {: base:ptr name:ptr bl:n bc:n bb:n :}
   name base - DOS-OFF !                    \ name-token byte offset in the buffer
   1 DOS-LN !  1 DOS-CL !  0 DOS-P !
   BEGIN DOS-P @ DOS-OFF @ < WHILE
      base DOS-P @ + c@ 10 = IF
         DOS-LN @ 1 + DOS-LN !  1 DOS-CL !
      ELSE
         DOS-CL @ 1 + DOS-CL !
      THEN
      DOS-P @ 1 + DOS-P !
   REPEAT
   bl DOS-LN @ + 1 -                        \ abs line
   DOS-LN @ 1 = IF bc DOS-CL @ + 1 - ELSE DOS-CL @ THEN   \ abs column (col carries only on line 1)
   bb DOS-OFF @ +                           \ abs byte_start
   DIAG-ORIGIN! ;
s" <input>" DIAG-FILE!
1 1 0 DIAG-ORIGIN!

\ TRUST: declare a word's effect without checking its body — the native escape
\ hatch (PLAN's TRUSTED:). Callers are checked against the declared sig.
\ Usage:  s" myword" s" n n -- n" trust
: TRUST {: na nu sa su :}
   na nu TOKFOLD drop
   sa su  TKF TKFU @  CHECKER-USIG-ADD ;

\ Pre-trust defer capability (dot habu-engine-pre-trust-77410827): `trust` (above)
\ and `checker-defer` (5208) are both defined now — the earliest safe point — so
\ drain the pending table, replaying trust+checker-defer for every defer declared
\ before this line. DRAIN-PRETRUST is a baked engine primitive (native habu2.f
\ + stage0 mirror forth.fs); it is called here by its bare token at exactly the
\ boot-prefix point where the pending table must be flushed. The engine backstop
\ (BSEALCAP, src/habu/habu1.f) dies fail-closed at SEAL-CAPTURE (exit 73, naming
\ each undrained defer) if the table is non-empty because the drain never ran, so
\ a removed or botched drain can never boot silently.
\ The earlier runtime-lookup transition shim (TRUSTED: DRAIN-PRETRUST-COMPAT,
\ owner habu-checker-exec-of-5923c543, which resolved the prim by `search-wl` so a
\ previous-fixpoint engine lacking the prim could still load this boot prefix) was
\ removed once this tree began declaring real pre-trust defers of its own — the
\ TFAM query hooks TFAM-RESOLVE-XT.. above, before `: TRUST`, captured into the
\ pending table. With those present, an engine without DRAIN-PRETRUST cannot load
\ this prefix regardless of the shim: the bare token is E-UNDEFINED (exit 70) on
\ it, and even the shim's lookup-miss branch would leave the table undrained and
\ hit the exit-73 backstop. The shim therefore protected no engine and only
\ carried a trusted execute-of-plain-n boundary (RSEXEC), so it is gone.
\ The bare token sits between the two unique PTD-REGRESSION-BLANK sentinels so the
\ negative regression (test/pre-trust-defer.f) can blank exactly this region and
\ prove the exit-73 undrained backstop still fires, naming a real prefix defer;
\ keep the sentinels contiguous with it.
\ PTD-REGRESSION-BLANK-BEGIN
DRAIN-PRETRUST
\ PTD-REGRESSION-BLANK-END

\ UNSAFE-TOK? lives above the EXPORT block (it also gates alias minting there).
: REJECT-UNSAFE ( -- )
   -1 UNSAFE !  0 OK !  -1 FAILSET ! ;

\ A live local name, scanned without LOC-REF?'s side effects (that word also
\ pushes the reference or latches a quotation reject).
variable RTL-I                        \ retired-token local scan index
: LOC-NAME? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   0 RTL-I !
   BEGIN RTL-I @ #LOC @ < WHILE
      a u  LOCNB RTL-I @ LOC-NAME-W * +  RTL-I @ cells LOCLN + @  CORE-STR=
         IF RES-TRUE EXIT THEN
      RTL-I @ 1 + RTL-I !
   REPEAT RES-FALSE ;

\ Where a retired-spelling token actually binds. Reject only at the two
\ positions that would have reached the retired raw global: it resolves at
\ global position, or it resolves nowhere. A local, an open-package word, or a
\ used-package public is a DIFFERENT word and passes through — locals shadow
\ retirement exactly as they shadow every other resolution, and the checker
\ resolves a local before any dictionary lookup, so a local reference can never
\ reach global position.
: RETIRED-GLOBAL? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u RETIRED-TOK? 0= IF RES-FALSE EXIT THEN
   a u LOC-NAME? IF RES-FALSE EXIT THEN
   a u CHECKER-FIND-ACTIVE-SYM dup 0= IF drop RES-TRUE EXIT THEN
   a u CHECKER-GLOBAL-SYM? = ;

\ A retired token is reported as undefined, not as an unsafe boundary: UNSAFE's
\ repair advice is "move it behind audited TRUST", which is exactly the wrong
\ instruction here — the word is gone for good and the caller must move to the
\ owning package API. Latching UNDEFERR reuses the existing E-UNDEFINED
\ diagnostic, so the checker verdict and the interpreter's own error name the
\ same failure; RETIRED is what forces verdict 0 in CHECK-VERDICT.
: REJECT-RETIRED ( -- )
   -1 RETIRED !  -1 UNDEFERR !  0 OK !  -1 FAILSET ! ;

\ M5: a block collective/barrier reached inside an open control frame is not
\ block-uniform-reachable; latch the reason on the pinned token, then reject.
\ MDIAG! must precede FAILSET (it latches only while the pin is open).
: REJECT-DIVBAR ( ptr u8 n -- )
   MD-DIVBAR MDIAG!
   FAIL-PIN!
   0 OK !  -1 FAILSET ! ;

\ p5 certificate soundness (dot habu-checker-fitting-arity-70dc94e4): an
\ IMMEDIATE word executes at COMPILE time, so a checked body that names one
\ contributes an EMPTY runtime step while the checker would apply the word's
\ DECLARED effect - a wrong certificate that downstream checked callers then
\ unify against. Until immediates' compile-time expansion is modeled, a
\ signature-carrying live-dictionary immediate in a checked body is a
\ fail-closed reject (the design doc's interim, mirroring the opener
\ treatment). Modeled parsing immediates ([char]/char via PARSE-LIT?) stay
\ green; engine prims are modeled by TRY-PRIMS and carry no usig row;
\ signature-less immediates already land on the uncheckable path; TRUSTED:
\ immediates are audited boundaries outside the usig table.
s" tok-imm?" s" ptr u8 n -- n" TRUST

variable IMMERR

\ --- declared parsing immediates (modeled compile-time expansion, minimal form).
\ A parsing immediate consumes a fixed number of source tokens at COMPILE time
\ and contributes only its declared runtime effect to the body (GRID: eats one
\ header token, WHERE eats three). Declaring one teaches the body scan to skip
\ its payload and exempts it from the wrong-certificate reject below. The
\ declaration is itself a checking-soundness boundary (a wrong count skips live
\ code or eats real tokens), so `parse-imm` is UNSAFE-TOK?-listed: top-level
\ declarations only, never a checked body step. Symbol-keyed like UNSAFE-SYMS;
\ monotonic, never rolled back (a retired symbol id is never reused, so a stale
\ entry can never match a new word).
16 constant PIMM-CAP
create PIMM-SYMS PIMM-CAP cells allot
create PIMM-NS PIMM-CAP cells allot
variable PIMM-N
0 PIMM-N !

: PIMM-IX ( n -- n ) {: sym:n :}   \ declaration index for sym, -1 = undeclared
   sym 0= IF -1 EXIT THEN
   0 BEGIN dup PIMM-N @ < WHILE
      dup cells PIMM-SYMS + @ sym = IF EXIT THEN
      1 +
   REPEAT drop -1 ;

: PIMM-CNT@ ( n -- n )             \ payload token count at declaration index
   cells PIMM-NS + @ ;

package CHECKER-PREFLIGHT

variable TARGET-A
variable TARGET-U
variable BODY-A
variable BODY-U
variable VERDICT
variable ACTIVE

: TARGET-A@ ( -- ptr u8 )
   TARGET-A 0 ptr-field @ ;

: TARGET-A! ( ptr u8 -- )
   TARGET-A 0 ptr-field ! ;

: BODY-A@ ( -- ptr u8 )
   BODY-A 0 ptr-field @ ;

: BODY-A! ( ptr u8 -- )
   BODY-A 0 ptr-field ! ;

: TAIL-WS? ( -- bool )
   TI @ BEGIN dup TBLEN @ < WHILE
      dup TBYTE@ 32 > IF drop RES-FALSE EXIT THEN
      1 +
   REPEAT
   drop RES-TRUE ;

: FORCED? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   ACTIVE @ 0= IF RES-FALSE EXIT THEN
   TAIL-WS? 0= IF RES-FALSE EXIT THEN
   a u TARGET-A@ TARGET-U @ CORE-STR=CI ;

public

: MODELED? ( ptr u8 n -- bool )
   CHECKER-FIND-ACTIVE-SYM PIMM-IX 0 < 0= ;

;package

: PARSE-IMM ( ptr u8 n n -- ) {: a:ptr u:n cnt:n :}
   PIMM-N @ PIMM-CAP < 0= IF s" checker: parse-imm table full" 76 die THEN
   cnt 0 < IF s" checker: parse-imm needs a non-negative token count" 76 die THEN
   a u CHECKER-FIND-ACTIVE-SYM {: sym:n :}
   sym 0= IF s" checker: parse-imm names an unknown word" 76 die THEN
   sym PIMM-N @ cells PIMM-SYMS + !
   cnt PIMM-N @ cells PIMM-NS + !
   PIMM-N @ 1 + PIMM-N ! ;
\ the TRUST row keeps parse-imm out of the internal-word marking (a library
\ declares its parsing immediates at top level; UNSAFE-TOK? already bars it
\ from checked bodies).
s" parse-imm" s" ptr u8 n n --" TRUST

: PIMM-SKIP ( n -- ) {: cnt:n :}   \ skip cnt payload tokens of a declared parsing immediate
   0 BEGIN dup cnt < WHILE
      SKIP-PARSE-LIT-PAYLOAD
      1 +
   REPEAT drop ;

\ PIMM-STREAM: -1 while checking an ENGINE-RECONSTRUCTED definition buffer: the
\ engine already EXECUTED each immediate during compilation, so its parsed
\ payload is structurally ABSENT from the buffer and the scan must NOT skip (it
\ would eat live body tokens - proven by GRID: ceil-n-256 {: x .. :} losing its
\ locals opener at load). This is the DEFAULT, because every hook-driven load
\ path hands reconstructed buffers - including user-installed strict hooks
\ (tools/lint/text.f) the checker cannot see. Only the explicit CANDIDATE scans
\ (CHECK-CANDIDATE!/CHECKER-CANDIDATE-SCOPE-*) receive raw source text where
\ the payload is still present, and they bracket the flag to 0 themselves.
variable PIMM-STREAM
-1 PIMM-STREAM !

package CHECKER-PREFLIGHT

public

: BODY-TOK? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u FORCED? IF RES-TRUE EXIT THEN
   a u PARSE-LIT? IF RES-FALSE EXIT THEN
   a u CHECKER-FIND-ACTIVE-SYM {: sym:n :}
   sym CHECKER-FIND-USIG-SYM 0= IF RES-FALSE EXIT THEN
   sym PIMM-IX 0 < 0= IF RES-FALSE EXIT THEN
   a u tok-imm? 0 <> ;

;package

: REJECT-IMMEDIATE ( -- )
   -1 IMMERR !  0 OK !  -1 FAILSET ! ;

variable ISQ
variable IS-TA
variable IS-TU

\ IS-TA holds a token-start pointer (ptr u8); read/write it through a ptr-field
\ view so the emitted token span keeps its ptr u8 role.
: IS-TA-FIELD ( -- ptr ptr u8 )
   IS-TA 0 ptr-field ;

: IS-TA@ ( -- ptr u8 )
   IS-TA-FIELD @ ;

: IS-TA! ( ptr u8 -- )
   IS-TA-FIELD ! ;

: IS-WS? ( n -- bool )
   32 <= ;

: IS-SKIP-WS ( -- )
   BEGIN TI @ TBLEN @ < WHILE
      TI @ TBYTE@ IS-WS? 0= IF exit THEN
      TI @ 1 + TI !
   REPEAT ;

: IS-NEXT-TOKEN ( -- ptr u8 n bool )
   IS-SKIP-WS
   TI @ TBLEN @ >= IF TBASE@ 0 RES-FALSE EXIT THEN
   TI @ TADDR IS-TA!
   0 IS-TU !
   BEGIN TI @ TBLEN @ < WHILE
      TI @ TBYTE@ IS-WS? IF
         IS-TA@ IS-TU @ RES-TRUE EXIT
      THEN
      IS-TU @ 1 + IS-TU !
      TI @ 1 + TI !
   REPEAT
   IS-TA@ IS-TU @ RES-TRUE ;

: IS-FAIL ( -- )
   0 OK !
   -1 FAILSET ! ;

: IS-QUOT-ROWS ( ptr u8 n -- n )
   PARSE-SIG-RAW
   SGHASR @ 0= IF 2drop FRESH MK-ROW dup THEN
   MK-QUOT ;

: IS-APPLY ( n -- )
   ISQ !
   FRESH MK-ROW {: rest :}
   DCUR @ ISQ @ rest MK-PUSH UNIFY OK @ and OK !
   rest DCUR ! ;

: IS-TARGET-TOK? ( -- bool )
   IS-NEXT-TOKEN 0= IF 2drop RES-FALSE EXIT THEN
   TOKFOLD drop
   RES-TRUE ;

: IS-TOK ( -- )
   IS-TARGET-TOK? 0= IF IS-FAIL EXIT THEN
   TKF TKFU @ CHECKER-FIND-ACTIVE-DEFER 0= IF IS-FAIL EXIT THEN
   TKF TKFU @ CHECKER-FIND-ACTIVE-SIG
   FEP-HIT? 0= IF IS-FAIL EXIT THEN
   FEP @ EFF-QUOT IS-APPLY ;

\ `['] W` (compile-mode tick) retypes from a plain n (`PRIM: [']  PE-N PE-OUT`)
\ to xt<effect(W)>: a T-QUOT of W's certified effect that execute/catch/is
\ fit-check by the existing RSEXEC/RSCATCH/IS-APPLY quotation unification (docs
\ typed-top-level.md §3). Two gates keep this precise-yet-safe:
\  (1) W's identity is only in the checked text on the CANDIDATE/raw-source path
\      (PIMM-STREAM 0, CHECK-CANDIDATE!/CHECK!/stage certify). The engine
\      -reconstructed --load body drops the tick target (C-BTICK consumes it with
\      no LBCAP), so there BTICK-TOK never fires and `[']` keeps its sound
\      over-strict PE-N model (execute rejects, probe p8).
\  (2) the xt<effect> is only materialised when the VERY NEXT token is an
\      xt-effect consumer; otherwise `['] W` yields a plain n exactly as today, so
\      the store-consumer sites (`['] B+ FPRIM-L`, `['] C-PACKAGE CF-ENTRY`, ...)
\      that hand the raw xt cell to a ( .. n .. ) prim are byte-unchanged. A
\      T-QUOT does not unify with a scalar n (sound: no arithmetic on code cells),
\      so this look-ahead is what lets the effect ride only into the fit-check.
\ A sig-less / prim / undefined W pushes a plain n so execute still rejects (over
\ -strict but sound, never a pure-xt launder); an unsafe definer W rejects,
\ closing DIRECT-tick laundering (the stored-xt `@ execute` residual stays dot
\ habu-checker-exec-of-5923c543).
: BTICK-CAND? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" [']" CORE-STR= 0= IF RES-FALSE EXIT THEN
   PIMM-STREAM @ 0 = ;

: BTICK-PUSH ( n -- )                    \ push one row term (xt<effect> quot or plain n)
   DCUR @ MK-PUSH DCUR ! ;

\ execute/catch/is unify the carried effect against the LIVE row (RSEXEC/RSCATCH/
\ IS-APPLY), so a T-QUOT operand fit-checks directly. run-in-stack is deliberately
\ excluded: its xt runs in an ISOLATED frame buffer, so the fit is "frame cells >=
\ e's din arity", not current-row unification - a distinct check left as residual
\ (dot habu-typed-top-xt-096a8f1b follow-up), and it keeps its raw-xt PE-N operand
\ here so `['] W run-in-stack` stays sound rather than falsely rejecting.
: BTICK-XT-CONSUMER? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" execute" CORE-STR=
   a u s" catch" CORE-STR= or
   a u s" is" CORE-STR= or ;

\ A typed xt<effect> storage cell accessor (TYPED-VARIABLE / TYPED-BUFFER, dot
\ habu-typed-xt-storage-ddad4af8) certifies with a top output of `ptr xt<...>`.
\ Recognising it as an xt sink lets `['] W  HK !` retype the tick so the store
\ fit-checks W's certified effect against the cell's declared effect, instead of
\ erasing the tick to a plain n and rejecting on `actual: n` (dot
\ habu-typed-xt-cells-08e1dc2c). Only the immediately-following variable accessor
\ is caught; a buffer slot store (`['] W idx BUF !`) puts the index between the
\ tick and the accessor and stays on the quotation-store surface (docs/effects.md).
: BTICK-XT-CELL-PTR? ( n -- bool )       \ term is `ptr xt<...>` : pointer to a typed xt<effect> cell
   T-RES dup TAG T-PTR <> IF drop RES-FALSE EXIT THEN
   PTR>INNER T-RES TAG T-QUOT = ;
: BTICK-DOUT-XT-CELL? ( ptr a -- bool )  \ effect record's top output term is a typed xt cell pointer
   EFF-QUOT Q>DOUT R-RES dup TAG S-PUSH = IF P>TYPE BTICK-XT-CELL-PTR? EXIT THEN
   drop RES-FALSE ;
: BTICK-STORE-CELL? ( ptr u8 n -- bool ) {: a:ptr u:n :}   \ next token is a typed-xt-cell store accessor
   a u CHECKER-FIND-ACTIVE-SIG FEP-HIT? 0= IF RES-FALSE EXIT THEN
   FEP @ BTICK-DOUT-XT-CELL? ;

: BTICK-NEXT-CONSUMES-XT? ( -- bool )    \ peek (no consume): is the post-target token an xt sink?
   TI @ >r
   IS-NEXT-TOKEN {: a:ptr u:n ok:bool :}
   r> TI !
   ok 0= IF RES-FALSE EXIT THEN
   a u BTICK-XT-CONSUMER? IF RES-TRUE EXIT THEN
   a u BTICK-STORE-CELL? ;

: BTICK-TOK ( -- )                       \ [ '] W : consume W, push xt<effect(W)> or a plain n
   IS-TARGET-TOK? 0= IF IS-FAIL EXIT THEN
   TKF TKFU @ UNSAFE-TOK? IF REJECT-UNSAFE EXIT THEN
   BTICK-NEXT-CONSUMES-XT? 0= IF PE-N BTICK-PUSH EXIT THEN
   TKF TKFU @ CHECKER-FIND-ACTIVE-SIG
   FEP-HIT? IF FEP @ EFF-QUOT BTICK-PUSH ELSE PE-N BTICK-PUSH THEN ;

\ --- item 12 layout stack-op typing (docs/type-families.md §17) --------------
\ Whole-bundle transport tokens. For an EXPANDED (non-linear) layout the group
\ moves by direct row surgery (XPORT-STEP? above); for a non-expanded
\ possibly-linear layout cell the op's effect var may absorb the one cell,
\ gated by LAYOUT-XPORT-ALLOW? (which rejects the possibly-linear bind). ?dup
\ is excluded on purpose — it branches on the top (tag) cell, width-breaking
\ for a sum whose tag 0 is a valid variant.
\ Transport mode keys on the FOLDED TOKEN NAME, and DO-TOK consults user sigs
\ before prims, so a user word spelled dup/swap/... can bind a layout through
\ its polymorphic effect. This is sound because the name implies a builtin or
\ a checked/audited definition: a CHECKED shadow's effect is verified against
\ its body, which can only move the value it binds; a TRUSTED shadow is
\ already an audited manifest boundary (TRUSTED.md row) whose declared effect
\ is the audit's responsibility.
: LAYOUT-XPORT-TOK? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" dup"   CORE-STR= IF RES-TRUE EXIT THEN
   a u s" drop"  CORE-STR= IF RES-TRUE EXIT THEN
   a u s" swap"  CORE-STR= IF RES-TRUE EXIT THEN
   a u s" over"  CORE-STR= IF RES-TRUE EXIT THEN
   a u s" nip"   CORE-STR= IF RES-TRUE EXIT THEN
   a u s" tuck"  CORE-STR= IF RES-TRUE EXIT THEN
   a u s" rot"   CORE-STR= IF RES-TRUE EXIT THEN
   a u s" -rot"  CORE-STR= IF RES-TRUE EXIT THEN
   a u s" 2dup"  CORE-STR= IF RES-TRUE EXIT THEN
   a u s" 2drop" CORE-STR= IF RES-TRUE EXIT THEN
   a u s" 2swap" CORE-STR= IF RES-TRUE EXIT THEN
   a u s" 2over" CORE-STR= IF RES-TRUE EXIT THEN
   a u s" >r"    CORE-STR= IF RES-TRUE EXIT THEN
   a u s" r>"    CORE-STR= IF RES-TRUE EXIT THEN
   a u s" r@"    CORE-STR= IF RES-TRUE EXIT THEN
   a u s" 2>r"   CORE-STR= IF RES-TRUE EXIT THEN
   a u s" 2r>"   CORE-STR= IF RES-TRUE EXIT THEN
   a u s" 2r@"   CORE-STR= ;

: DCUR-TOP-LAYOUT? ( -- bool )     \ resolved top of the data row is a layout param?
   \ LAYOUT-PARAM? is true for a HIDDEN field too (same layout family-id), so an
   \ expanded bundle's tag cell on top rejects ?dup exactly like the old logical cell.
   DCUR @ R-RES dup TAG S-PUSH = IF P>TYPE LAYOUT-PARAM? EXIT THEN drop RES-FALSE ;

: QDUP-STEP? ( ptr u8 n -- bool )  \ ?dup: reject on a layout value; scalar stays unmodeled
   s" ?dup" CORE-STR= 0= IF RES-FALSE EXIT THEN
   DCUR-TOP-LAYOUT? IF
      0 OK !  -1 FAILSET !  -1 QDUPBAD !         \ width-breaking touch of a layout value
   ELSE
      -1 UNDEFERR !  -1 UNCK !                   \ scalar ?dup unmodeled (pre-existing gap; dotted)
   THEN
   RES-TRUE ;

\ --- construct form (item 9, docs/type-families.md §12): `construct family
\ variant` is a reserved checker token protocol, not a word lookup. The two
\ operand tokens are captured in DO-TOK1 BEFORE locals reference, control
\ dispatch, and dictionary lookup (PLAN item 9: family/variant tokens must not
\ collide with locals or words) and resolved through the registry xts installed
\ by type-family.f: the family in the ACTIVE package only (the ownership
\ predicate — cross-package construction never resolves), the variant within
\ that family. The step effect is then applied inline exactly like a generated
\ constructor call (payloads consumed, layout bundle produced, CHECKER-STEP
\ conservation). Stage engines without the registry keep the 0 hooks and fail
\ closed. Native/Gforth lowering is item 10: a certified construct body cannot
\ compile or run until it lands (engine undefined-word reject, pinned in the
\ ctor suite).
variable CONM      \ 0 off | 1 expecting family token | 2 expecting variant token
variable CONFAM    \ resolved family id while CONM = 2

: CONSTRUCT-BEGIN ( -- )
   1 CONM ! ;   \ enter family-token mode; an unarmed registry rejects at the family resolve (CONSTRUCT-FAM-XT default)

\ A failed family resolve still consumes the variant token (poisoned CONFAM
\ -1): the three-token form always captures whole, so the trailing operand can
\ never fall through to locals/word lookup and blur the hard reject into an
\ uncheckable-undefined verdict.
: CONSTRUCT-TOK ( ptr u8 n -- ) {: a:ptr u:n :}
   CONM @ 1 = IF
      a u CONSTRUCT-FAM-XT IF CONFAM ! ELSE drop 0 OK !  -1 CONFAM ! THEN
      2 CONM !
      EXIT THEN
   CONFAM @ 0 < 0= IF
      a u CONFAM @ MATCH-VAR-XT IF CVLIVE ! ELSE drop THEN   \ latch variant SUMV id for the mismatch capture
      a u CONFAM @ CONSTRUCT-STEP-XT 0= IF 0 OK ! THEN
      -1 CVLIVE !                                                      \ variant leaves scope with the step
   THEN
   0 CONM ! ;

\ --- field-projection op (dot habu-checker-type-structure, docs/type-families.md
\ §2.2). `field-project` is a reserved checker operation: inside the sealed armed
\ window it consumes `ptr family<args>` plus a baked byte-offset literal and
\ produces `ptr <instantiated field type>`, deriving the field's family, offset,
\ byte extent, role, and schema from the committed field id via TYPE-FIELD
\ reflection (FIELD-PROJ-XT, bound in type-family.f). It is the single shape the
\ ordinary layout fence refuses (`+`/`cell+` preserve the pointee; CAST refuses
\ T-PTR), so nothing else can retype a layout pointer. Runtime is a plain
\ pointer+offset add (the generator/consumer supplies the `( ptr n -- ptr )`
\ word); the checker judges only the type effect here.
: FIELD-PROJ! ( ptr u8 n n n -- ) {: a:ptr u:n fid:n off:n :}   \ arm: accessor name span + committed field id + baked byte offset
   a FIELD-PROJ-A !  u FIELD-PROJ-U !  fid FIELD-PROJ-FID !  off FIELD-PROJ-OFF ! ;
: FIELD-PROJ-CLEAR ( -- ) 0 FIELD-PROJ-U ! ;
: FIELD-PROJ-MATCH? ( -- bool )   \ armed AND the word under check is the armed accessor
   FIELD-PROJ-U @ 0 >  NMU @ 0 >  and 0= IF RES-FALSE EXIT THEN
   FIELD-PROJ-A @ FIELD-PROJ-U @ NMA @ NMU @ CORE-STR=CI ;   \ NMA is case-folded; the armed name is verbatim
: FIELD-PROJ-REJECT ( -- ) TKF TKFU @ CAP-FAIL  0 OK ! ;   \ pin the field-project token, fail closed
\ Row surgery: peek `ptr family<args>` (second from top; the offset literal is on
\ top), validate the pointer shape, ask FIELD-PROJ-XT for the instantiated field
\ type, then consume (ptr family, offset) and produce (ptr field-type) through the
\ ordinary CHECKER-STEP so linearity/row plumbing stay consistent. The peeked
\ family term is used verbatim in the step's input row so its `family ~ family`
\ pairing is the sanctioned same-family bind, not a fenced var<->layout bind.
: FIELD-PROJ-STEP ( -- )
   FIELD-PROJ-CLEAR                                       \ single shot, even on reject
   DCUR @ R-RES {: r0:n :}                                \ top = baked offset literal
   r0 TAG S-PUSH <> IF FIELD-PROJ-REJECT EXIT THEN
   r0 P>REST R-RES {: r1:n :}                             \ below = ptr family<args>
   r1 TAG S-PUSH <> IF FIELD-PROJ-REJECT EXIT THEN
   r1 P>TYPE T-RES {: pt:n :}
   pt TAG T-PTR <> IF FIELD-PROJ-REJECT EXIT THEN         \ not a pointer
   pt PTR>INNER T-RES {: famterm:n :}
   famterm LAYOUT-PARAM? 0= IF FIELD-PROJ-REJECT EXIT THEN   \ pointee is not a layout family
   FIELD-PROJ-FID @ FIELD-PROJ-OFF @ famterm FIELD-PROJ-XT {: fieldterm:n ok:bool :}
   ok 0= IF FIELD-PROJ-REJECT EXIT THEN                   \ wrong offset / off-past-width / arity / role / uncommitted id
   FRESH MK-ROW {: base:n :}
   famterm MK-PTR base MK-PUSH FRESH MK-VAR swap MK-PUSH {: din:n :}   \ base, ptr family<args>, offset-var (MK-PUSH: type rest -- row)
   fieldterm MK-PTR base MK-PUSH {: dout:n :}                          \ base, ptr field-type
   din dout CHECKER-STEP ;
: FIELD-PROJ-STEP? ( ptr u8 n -- bool )   \ handle a field-project token inside the armed window; report whether handled
   FIELD-PROJ-MATCH? 0= IF 2drop RES-FALSE EXIT THEN
   s" field-project" CORE-STR= 0= IF RES-FALSE EXIT THEN
   FIELD-PROJ-STEP RES-TRUE ;

: DO-TOK1 {: a u :}
   a u TOKFOLD drop
   a u CAP-FAIL
   0 EXEC-OPAQUE !  0 CATCH-OPAQUE !
   TKF TKFU @ LAYOUT-XPORT-TOK? LAYOUT-XPORT !    \ transport op? layout value moves whole
   TOK0 @ IF TKF NMB TKFU @ CCOPY  NMB NMA !  TKFU @ NMU !  0 TOK0 ! ELSE
   TKF TKFU @ LIVE-TOKEN? 0= IF -1 DEADERR ! 0 OK ! ELSE
   LMODE @ IF TKF TKFU @ LOC-TOK ELSE
   CONM @ 0 <> IF TKF TKFU @ CONSTRUCT-TOK ELSE
   MM @ 0 <> IF TKF TKFU @ MATCH-TOK ELSE
   TKF TKFU @ s" construct" CORE-STR= IF CONSTRUCT-BEGIN ELSE
   TKF TKFU @ s" match" CORE-STR= IF MATCH-BEGIN ELSE
   TKF TKFU @ s" {:" CORE-STR= IF LOC-BEGIN ELSE
   TKF TKFU @ UNSAFE-TOK? IF REJECT-UNSAFE ELSE
   TKF TKFU @ RETIRED-GLOBAL? IF REJECT-RETIRED ELSE
   TKF TKFU @ s" is" CORE-STR= IF IS-TOK ELSE
   TKF TKFU @ BTICK-CAND? IF BTICK-TOK ELSE
   OK @ IF TKF TKFU @ s" exit" CORE-STR= IF a u DEAD-OWNER! THEN THEN
   OK @ IF TKF TKFU @ s" leave" CORE-STR= IF a u DEAD-OWNER! THEN THEN
   OK @ IF TKF TKFU @ s" again" CORE-STR= IF a u DEAD-OWNER! THEN THEN
   TKF TKFU @ LOC-REF? 0= IF
   TKF TKFU @ CF-TOK? 0= IF
   TKF TKFU @ QDUP-STEP? 0= IF
   LAYOUT-XPORT @ IF TKF TKFU @ WF-XPORT-RECORD THEN   \ width facts from the pre-op row
   TKF TKFU @ HIDROW-STEP? 0= IF                       \ depth/.s fail closed over hidden cells
   TKF TKFU @ XPORT-STEP? 0= IF                        \ whole-bundle transport row surgery
   TKF TKFU @ RS-TOK? 0= IF
   TKF TKFU @ FIELD-PROJ-STEP? 0= IF                   \ armed FAMILY:FIELD projection: ptr family<args> -> ptr field-type
   TKF TKFU @ CHECKER-PREFLIGHT:BODY-TOK? IF
      a u FAIL-PIN! REJECT-IMMEDIATE
   THEN   \ live immediate with a usig: wrong-certificate reject (p5)
   TKF TKFU @ DO-TOK
   OK @ IF TKF TKFU @ THROW-CUR? IF THROW-EDGE THEN THEN
   OK @ IF TKF TKFU @ DEAD-CUR? IF a u DEAD-OWNER! -1 DEADP ! THEN THEN
   OK @ #CFC @ 0 > and IF BARRIER-CUR? IF ALL-CF-UNIFORM? 0= IF a u REJECT-DIVBAR THEN THEN THEN
   TKF TKFU @ ESCAPED-STRING-OPENER? IF SKIP-ESCAPED-STRING-PAYLOAD ELSE
   TKF TKFU @ NORMAL-STRING-OPENER? IF SKIP-STRING-PAYLOAD THEN THEN
   TKF TKFU @ PARSE-LIT? IF SKIP-PARSE-LIT-PAYLOAD THEN
   \ declared parsing immediate: skip its payload tokens - raw-text scans only
   \ (engine-reconstructed load buffers already lack the payload, PIMM-STREAM).
   PIMM-STREAM @ 0 = IF
      CURSYM @ PIMM-IX dup 0 < 0= IF PIMM-CNT@ PIMM-SKIP ELSE drop THEN
   THEN
   THEN THEN THEN THEN THEN THEN THEN THEN THEN THEN THEN THEN THEN THEN THEN THEN THEN THEN THEN
   EXEC-OPAQUE @ IF MD-EXEC-OPAQUE MDIAG! THEN   \ name the opaque-execute reject on the pinned 'execute' token
   CATCH-OPAQUE @ IF MD-CATCH-OPAQUE MDIAG! THEN   \ name the opaque-catch reject on the pinned 'catch' token
   LIN-TAINT-SCAN
   OK @ 0=  FAILSET @ 0=  and IF -1 FAILSET ! THEN
   UNCK @  FAILSET @ 0=  and IF -1 FAILSET ! THEN
   TOKIX @ 1 + TOKIX ! ;

\ CHECK-RESET ( a u -- )
\ --- multi-error load mode ------------------------------------------------
\ Off by default so the ordinary load path (fixpoint build, gate) keeps the
\ fail-on-first-reject HOOK behavior. When on, a rejected definition still
\ trusts its DECLARED signature (so later definitions check against a known
\ effect instead of cascading undefined-word errors) — unless that signature
\ itself failed to parse (SGBAD), in which case no row is stored and callers
\ reject as undefined (USIG-ADD-BAD) — and the reject is counted so the
\ driver can exit nonzero at end of load. The mode cells and MULTI-ERR? live
\ above USIG-ADD, which shares them.
\ File-relative diagnostic origin for a MULTI-ERR load. The driver evaluates a
\ whole source buffer in one run; per rejected definition the checker re-points
\ DIAG-ORIGIN! to that def's FILE position so JSON positions are file-relative
\ (matching tools/check.f --all-errors). The compiler owns the def name-token
\ position in DATA cell DEF-TKA-CELL; the driver passes that cell's ABSOLUTE
\ address (data-base DEF-TKA-CELL +) so the checker stays free of engine-layout
\ constants it cannot name at bake time.
variable MEO-ON       \ file-relative origin active this load?
variable MEO-BASE     \ eval-buffer base ptr (file byte MEO-BB)
variable MEO-NAMEC    \ absolute addr of the compiler's def name-token cell
variable MEO-BL  variable MEO-BC  variable MEO-BB   \ buffer start's file line/col/byte
0 MEO-ON !

: MULTI-ERR-BEGIN ( -- ) -1 MULTI-ERR !  0 MULTI-ERR-N !  0 MEO-ON ! ;
: MULTI-ERR-END ( -- n ) MULTI-ERR-N @  0 MULTI-ERR !  0 MEO-ON ! ;   \ reject count; clears mode
: MULTI-ERR-ORIGIN! {: base:ptr namec:n bl:n bc:n bb:n :}
   base MEO-BASE !  namec MEO-NAMEC !
   bl MEO-BL !  bc MEO-BC !  bb MEO-BB !  -1 MEO-ON ! ;
: MEO-APPLY ( -- )    \ set DIAG-ORIGIN! to the current def's file position
   MEO-BASE @  MEO-NAMEC @ @  MEO-BL @ MEO-BC @ MEO-BB @  DIAG-ORIGIN-SPAN! ;

\ --- declared-effect parametricity seal (habu-nominal-storage-effect-a60ba885).
\ After a definition's body checks against its declared ( in -- out ), every
\ declared type-variable quantifier must still resolve to a DISTINCT variable.
\ Two ways a body can launder a specialization behind a generic declared face:
\   (1) SPECIALIZATION — the quantifier resolves to a sealed identity-bearing
\       family (arity-0 nominal-scalar or a layout family: the validated CAD-KIND
\       identities and layout bundles). A body that forges such a family behind a
\       declared `a`/`ptr a` is rejected; plain-scalar widening (a := n/u8) stays
\       legal, so the pervasive `( ptr a -- n )` fetch corpus is untouched.
\   (2) ALIASING — two DISTINCT declared quantifiers unify into one variable,
\       breaking injectivity of the quantifier map.
\ It runs ONLY when the body otherwise certified (CHECK OK true), so the pointee
\ NOMPTR-BLOCK mismatch path (which fails unification first, OK 0) is untouched.
\ No new bindings are made — the check only resolves and compares — so the trail
\ that already rolls back the body's speculative binds needs no new discipline;
\ the next definition's NEW/TV-RESET clears every specialization record.
32 constant NP-CAP                                   \ >= 26 single-letter quantifiers
create NP-ORIG NP-CAP cells allot   variable NP-ORIG-N   \ distinct declared quantifier ids
create NP-ROOT NP-CAP cells allot                    \ resolved root id of each seen quantifier
create NP-ROOT-OID NP-CAP cells allot                \ origin quantifier id that first reached a root
variable NP-SEEN-N

: NP-ORIG+ ( n -- )   \ record a distinct declared quantifier original id
   {: id:n :}
   0 BEGIN dup NP-ORIG-N @ < WHILE
      dup cells NP-ORIG + @ id = IF drop EXIT THEN   \ already collected
      1 +
   REPEAT drop
   NP-ORIG-N @ NP-CAP < IF
      id NP-ORIG-N @ cells NP-ORIG + !
      NP-ORIG-N @ 1 + NP-ORIG-N !
   THEN ;

: NP-COLLECT-TERM ( n -- )   \ collect quantifier ids in one declared type term (self + ptr pointee)
   dup ISVAR IF PAY NP-ORIG+ EXIT THEN
   dup TAG T-PTR = IF PTR>INNER TWALK-DEEPER RECURSE TWALK-SHALLOWER EXIT THEN
   drop ;

: NP-COLLECT-ROW ( n -- )   \ collect quantifier ids across one declared stack row
   R-RES
   BEGIN dup TAG S-PUSH = WHILE
      dup P>TYPE NP-COLLECT-TERM
      P>REST R-RES
   REPEAT drop ;

: NP-COLLECT ( -- )   \ every declared quantifier in ( in -- out [| rin -- rout] )
   0 NP-ORIG-N !  TWALK-RESET
   SGIN @ NP-COLLECT-ROW
   SGOUT @ NP-COLLECT-ROW
   SGHASR @ IF SGRIN @ NP-COLLECT-ROW  SGROUT @ NP-COLLECT-ROW THEN ;

: NP-LETTER ( n -- n )   \ generic effect source letter, else '?'; not a declaration position
   {: id:n :}
   0 BEGIN dup 26 < WHILE
      dup cells NMAP + @ id = IF 97 + EXIT THEN
      1 +
   REPEAT drop 63 ;

: NP-FAM ( n -- n )   \ resolved family id of a T-PARAM term, else -1
   T-RES dup TAG T-PARAM = IF PARAM>FAM ELSE drop -1 THEN ;

: NP-SEALED? ( n -- bool )   \ resolved term is an identity-bearing sealed family?
   dup NOM-SCALAR? IF drop RES-TRUE EXIT THEN
   LAYOUT-PARAM? ;

: NP-FAIL-SPEC ( n n -- )   \ quantifier id specialized to sealed family term (first-wins)
   {: oid:n rt:n :}
   NPBAD @ IF EXIT THEN
   -1 NPBAD !  0 NPBAD-KIND !
   oid NP-LETTER NPBAD-Q1 !  0 NPBAD-Q2 !  rt NPBAD-TERM ! ;

: NP-FAIL-ALIAS ( n n -- )   \ two declared quantifiers unified (first-wins)
   {: oid:n other:n :}
   NPBAD @ IF EXIT THEN
   -1 NPBAD !  1 NPBAD-KIND !
   oid NP-LETTER NPBAD-Q1 !  other NP-LETTER NPBAD-Q2 !  0 NPBAD-TERM ! ;

: NP-SEEN-FIND ( n -- n )   \ index of a recorded root, else -1
   {: root:n :}
   0 BEGIN dup NP-SEEN-N @ < WHILE
      dup cells NP-ROOT + @ root = IF EXIT THEN
      1 +
   REPEAT drop -1 ;

: NP-SEEN-ADD ( n n -- )   \ record ( root oid )
   {: root:n oid:n :}
   NP-SEEN-N @ NP-CAP < IF
      root NP-SEEN-N @ cells NP-ROOT + !
      oid  NP-SEEN-N @ cells NP-ROOT-OID + !
      NP-SEEN-N @ 1 + NP-SEEN-N !
   THEN ;

: NP-CHECK-ONE ( n -- )   \ verify one declared quantifier stays a distinct, unspecialized var
   {: oid:n :}
   oid MK-VAR T-RES {: rt:n :}
   rt ISVAR 0= IF
      rt NP-SEALED? IF oid rt NP-FAIL-SPEC THEN
      EXIT
   THEN
   rt PAY NP-SEEN-FIND dup 0 < IF
      drop rt PAY oid NP-SEEN-ADD                     \ first sighting of this root
   ELSE
      oid swap cells NP-ROOT-OID + @ NP-FAIL-ALIAS    \ root already claimed by another quantifier
   THEN ;

\ --- checked-mint output-provenance seal (dot habu-ptx-phantom-preserving,
\ leg 2b). A CHECKED (:) definition may not introduce, as an argument of a
\ register-phantom CELL family in an OUTPUT, a declared type variable that is
\ unbound in its inputs. Producing a cell value of input-unrelated type is a
\ MINT — sound only behind an audited TRUSTED: boundary, whose register<->phantom
\ coercion the checker cannot witness. TRUSTED: words bypass CHECK, so a genuine
\ mint (GRID-CTX minting a fresh block, MK-SPAN minting a span) stays legal there;
\ a `:` wrapper that forges a free-typed phantom through a mint combinator
\ (`( span<..,t,..> gridctx<b,e,m> -- tile<u,b,m> )`, u free) rejects here.
\ Complements NP-CHECK's top-level parametricity seal, which does NOT descend into
\ family arguments. Raw declared identity (PAY, no T-RES): the body's speculative
\ binds (a laundering combinator that unified u:=t) do not hide a var whose
\ declared occurrences are all outputs.
create NP-INVARS NP-CAP cells allot   variable NP-INVARS-N   \ declared input var ids (dedup)
variable NP-CELL-TERM   \ output cell-family term currently scanned (for the diagnostic)
variable NP-OUT-I       \ output cell param arg index (NP-OUT-TERM is non-recursive)

: NP-INVARS+ ( n -- ) {: id:n :}
   0 BEGIN dup NP-INVARS-N @ < WHILE
      dup cells NP-INVARS + @ id = IF drop EXIT THEN  1 +
   REPEAT drop
   NP-INVARS-N @ NP-CAP < IF
      id NP-INVARS-N @ cells NP-INVARS + !  NP-INVARS-N @ 1 + NP-INVARS-N !
   THEN ;
: NP-INVARS-HAS? ( n -- bool ) {: id:n :}
   0 BEGIN dup NP-INVARS-N @ < WHILE
      dup cells NP-INVARS + @ id = IF drop RES-TRUE EXIT THEN  1 +
   REPEAT drop RES-FALSE ;

\ NP-INVARS-WALK ( t -- ) : raw-collect every declared var id reachable in a term
\ OR a stack row, descending through ptr pointees, quotation effect rows, and
\ family arguments. No T-RES: a bound declared var keeps its raw PAY identity.
: NP-INVARS-WALK ( n -- ) {: t:n :}
   t R-RES dup TAG S-PUSH = IF
      BEGIN dup TAG S-PUSH = WHILE
         dup P>TYPE TWALK-DEEPER RECURSE TWALK-SHALLOWER
         P>REST R-RES
      REPEAT drop EXIT
   THEN drop
   t ISVAR IF t PAY NP-INVARS+ EXIT THEN
   t TAG T-PTR = IF t PTR>INNER TWALK-DEEPER RECURSE TWALK-SHALLOWER EXIT THEN
   t TAG T-QUOT = IF
      t Q>DIN  TWALK-DEEPER RECURSE TWALK-SHALLOWER
      t Q>DOUT TWALK-DEEPER RECURSE TWALK-SHALLOWER
      t Q>RIN  TWALK-DEEPER RECURSE TWALK-SHALLOWER
      t Q>ROUT TWALK-DEEPER RECURSE TWALK-SHALLOWER EXIT
   THEN
   t TAG T-PARAM = IF
      0 BEGIN dup t PARAM>ARGC < WHILE            \ data-stack index (RECURSE-safe)
         t over PARAM>ARG TWALK-DEEPER RECURSE TWALK-SHALLOWER
         1 +
      REPEAT drop EXIT
   THEN ;

: NP-CELLFAM? ( n -- bool )   \ raw T-PARAM of a register-phantom cell family
   {: t:n :}   \ NOT a layout product/sum and NOT a checker-owned hidden physical field:
   t TAG T-PARAM = 0= IF RES-FALSE EXIT THEN            \ those carry their own construct/field discipline
   t PARAM>HID 0 > IF RES-FALSE EXIT THEN               \ hidden physical field (expanded layout bundle)
   t PARAM>FAM dup 0 < IF drop RES-FALSE EXIT THEN
   dup TFAM-LAYOUT?* IF drop RES-FALSE EXIT THEN         \ product/sum layout family
   TFAM-CELL?* ;

: NP-FAIL-FREEMINT ( n n -- )   \ ( varid cellterm ) output cell var unbound in inputs (first-wins)
   {: vid:n ct:n :}
   NPBAD @ IF EXIT THEN
   -1 NPBAD !  2 NPBAD-KIND !
   vid NP-LETTER NPBAD-Q1 !  0 NPBAD-Q2 !  ct NPBAD-TERM ! ;

\ NP-FREE-SCAN ( t -- ) : flag any input-unbound var inside a term nested under the
\ output cell family stashed in NP-CELL-TERM. Raw (no T-RES) — a body bind cannot
\ launder the declared var's absence from the inputs.
: NP-FREE-SCAN ( n -- ) {: t:n :}
   t ISVAR IF
      \ only a USER-DECLARED quantifier (a..z, NP-LETTER != '?') can forge a mint;
      \ internal/fresh vars from family machinery are checker-owned, never user-writable.
      t PAY NP-LETTER 63 <>  t PAY NP-INVARS-HAS? 0=  and
      IF t PAY NP-CELL-TERM @ NP-FAIL-FREEMINT THEN EXIT
   THEN
   t TAG T-PTR = IF t PTR>INNER TWALK-DEEPER RECURSE TWALK-SHALLOWER EXIT THEN
   t TAG T-PARAM = IF
      0 BEGIN dup t PARAM>ARGC < WHILE            \ data-stack index (RECURSE-safe)
         t over PARAM>ARG TWALK-DEEPER RECURSE TWALK-SHALLOWER
         NPBAD @ IF drop EXIT THEN
         1 +
      REPEAT drop EXIT
   THEN ;                                         \ T-CON/T-ATOM/T-QUOT: no minted var

: NP-OUT-TERM ( n -- )   \ one output term: if a cell param, seal its var arguments
   {: t:n :}
   t NP-CELLFAM? 0= IF EXIT THEN
   t NP-CELL-TERM !
   0 NP-OUT-I !
   BEGIN NP-OUT-I @ t PARAM>ARGC < WHILE
      t NP-OUT-I @ PARAM>ARG NP-FREE-SCAN
      NPBAD @ IF EXIT THEN
      NP-OUT-I @ 1 + NP-OUT-I !
   REPEAT ;

: NP-OUT-ROW ( n -- )
   R-RES BEGIN dup TAG S-PUSH = WHILE
      dup P>TYPE NP-OUT-TERM
      NPBAD @ IF drop EXIT THEN
      P>REST R-RES
   REPEAT drop ;

\ NP-ROW-HAS-FIELD? ( row -- bool ) : does a declared input row consume a
\ VALUE-RECORD? A record is carried as a reserved `field<rec,name,inner>` term
\ (FIELD-PARAM?), and it can EXISTENTIALLY hold a polymorphic cell field. So a
\ checker-native field accessor (`( rec -- fam<a,..> )`, empty body = width
\ coercion) legitimately surfaces cell vars unbound at the sig surface — a
\ checker-governed introduction, NOT a register mint. The register-phantom
\ families this seal protects (span / gridctx / tile / …) are plain cells and
\ never appear as accessor inputs, so the PTX mint surface is unaffected.
\ (Conservative boundary: a wrapper mixing a record input with a register mint is
\ not sealed — an audited, contrived shape.)
: NP-ROW-HAS-FIELD? ( n -- bool )
   R-RES BEGIN dup TAG S-PUSH = WHILE
      dup P>TYPE FIELD-PARAM? IF drop RES-TRUE EXIT THEN
      P>REST R-RES
   REPEAT drop RES-FALSE ;

: NP-MINT-CHECK ( -- )   \ output cell params may not introduce input-unbound vars
   NPBAD @ IF EXIT THEN
   SGIN @ NP-ROW-HAS-FIELD? IF EXIT THEN
   SGHASR @ IF SGRIN @ NP-ROW-HAS-FIELD? IF EXIT THEN THEN
   0 NP-INVARS-N !  TWALK-RESET
   SGIN @ NP-INVARS-WALK
   SGHASR @ IF SGRIN @ NP-INVARS-WALK THEN
   TWALK-RESET
   SGOUT @ NP-OUT-ROW
   NPBAD @ 0=  SGHASR @  and IF SGROUT @ NP-OUT-ROW THEN ;

: NP-CHECK ( -- )   \ post-body parametricity seal; sets NPBAD on the first violation
   NP-COLLECT
   0 NP-SEEN-N !
   0 BEGIN dup NP-ORIG-N @ < WHILE
      dup cells NP-ORIG + @ NP-CHECK-ONE
      NPBAD @ IF drop EXIT THEN
      1 +
   REPEAT drop
   NP-MINT-CHECK ;

: CHECK-RESET {: a u :}
   u TOKBUF-ENSURE
   a TBASE !  u TBLEN !  NEW
   0 TI !  1 TOK0 !  0 NMU !  0 #LOC !  0 LMODE !  0 #CFC !  0 QDEPTH !  0 CONM !
   0 MM !  0 MPEND !  0 MREJ !  0 MF-DEPTH !  0 MSEEN-N !
   0 MDIAG !  0 MDIAG-FAM !  0 MDIAG-SEEN !  0 MDIAG-VCNT !
   0 FAILSET !  0 DEXP !  0 DACT !  0 DF-ACT !  0 DF-EXP !  -1 DVAR !  -1 CVLIVE !  -1 DPOS !  0 FAILTU !  0 SGSEEN !  0 SGHASR !
   0 SGIN !  0 SGOUT !  0 SGRIN !  0 SGROUT !  0 SGDBASE !  0 SGRBASE !
   0 SGA !  0 SGU !
   0 TOKIX !  0 FAILIX !  0 DVERD !
   0 FAILB !  0 FAILE !  0 XSET !  0 DEADP !  0 DEADERR !  0 DEADTA !  0 DEADTU !
   0 THDROW !  0 THRROW !  0 THSET !
   SGBAD-CLEAR  0 UNSAFE !  0 RETIRED !  0 IMMERR !  0 LOCALBAD !  0 LINLOCBAD !  0 UNDEFERR !  0 QUALBAD !  0 QDUPBAD !  0 CAPREQ !
   0 NPBAD !  0 NPBAD-KIND !  0 NPBAD-Q1 !  0 NPBAD-Q2 !  0 NPBAD-TERM !
   0 LOCSEQ !
   0 WF-N !  0 RECW !  0 RECMI !
   0 RECEFF !  0 RECEFF-ON !  0 RECEFF-UEND ! ;

: CHECK-SCAN ( -- )
   BEGIN TI @ TBLEN @ < WHILE
     BEGIN TI @ TBLEN @ <  TI @ TBYTE@ 32 =  and WHILE TI @ 1 + TI ! REPEAT
     TI @ TBLEN @ < IF
       TI @ TBYTE@ 40 =  TI @ 1 + TBYTE@ 32 =  and IF   \ '( ' (not '(CMP)') -> sig or comment
         TI @ 1 + TI !  TI @ TSTART !             \ sig text starts after '('
         BEGIN TI @ TBLEN @ <  TI @ TBYTE@ 41 <>  and WHILE TI @ 1 + TI ! REPEAT
         \ only the '( ... )' right after the name is the sig; once it is seen
         \ (or body tokens ran) every later '( ... )' is a comment (EM-COMMENT
         \ parity) and must not touch any signature state.
         VSIG @  SGSEEN @ 0= and  TOKIX @ 2 < and  IF
           TSTART @ TADDR SGA !  TI @ TSTART @ - SGU !
           TSTART @ TADDR  TI @ TSTART @ -  PARSE-SIG-RAW   \ ( din dout rin rout )
           SGBAD-FAIL!
           PD-BASE @ SGDBASE !
           RR-SHARED @ SGRBASE !
           SGHASR @ IF
             SGROUT !  dup SGRIN !  RCUR !  SGOUT !  dup SGIN !  DCUR !
           ELSE
             2drop  SGOUT !  dup SGIN !  DCUR !
           THEN  -1 SGSEEN !
           SIG-EFF-CACHE!
         THEN
         TI @ TBLEN @ < IF TI @ 1 + TI ! THEN     \ skip ')'
       ELSE
         TI @ TSTART !
         BEGIN TI @ TBLEN @ <  TI @ TBYTE@ 32 <>  and WHILE TI @ 1 + TI ! REPEAT
         TSTART @ TADDR  TI @ TSTART @ -  DO-TOK1
       THEN
     THEN
   REPEAT ;

: CHECK-FOLD-EXITS ( -- )
   XSET @ IF                                         \ fold early-return states into the output
     DEADP @ IF XROW @ DCUR !  XRROW @ RCUR !         \ every path exited: output = accumulator
     ELSE DCUR @ XROW @ UNIFY OK @ and OK !  RCUR @ XRROW @ UNIFY OK @ and OK ! THEN
   THEN ;

: SGHASR? ( -- bool )
   SGHASR @ 0 <> ;

: CHECK-SIG? ( -- bool )
   VSIG-ON? SGSEEN? and ;

: CHECK-RET-SIG? ( -- bool )
   CHECK-SIG? SGHASR? and ;

: CHECK-VERDICT ( -- n )
   SGBAD @ UNSAFE @ or  RETIRED @ or  IMMERR @ or  LOCALBAD @ or  LINLOCBAD @ or  QDUPBAD @ or  CAPREQ @ or  MREJ @ or  NPBAD @ or 0 <> IF 0 ELSE
   UNCK @ 0 <> IF 1 ELSE OK @ THEN THEN ;

\ CERT-REPOINT-ROWS ( -- ) : restore the REND-SIG contract after certifying.
\ CHECKER-USIG-CERT-ADD -> USIG-ADD runs NEW, which re-initializes BROW/DCUR
\ (and RBROW/RCUR) to a fresh empty pair, wiping the verified declared effect
\ recorded just above — so every certified word rendered bare `--` and the
\ prop-test round-trip amplifier was dead since introduction (dot
\ habu-rend-sig-blanked-b269786b). USIG-ADD's own PARSE-SIG-RAW leaves the
\ re-parsed declared rows in SGIN/SGOUT (SGRIN/SGROUT with a return clause),
\ live until the next CHECK — re-point the render rows at them.
: CERT-REPOINT-ROWS ( -- )
   SGBAD @ 0 <> IF EXIT THEN
   SGIN @ BROW !  SGOUT @ DCUR !
   SGHASR @ 0 <> IF SGRIN @ RBROW !  SGROUT @ RCUR ! THEN ;

\ --- generated-constructor certification (item 8, docs/type-families.md §12).
\ Hidden physical fields are checker-owned, so no user body can produce a
\ layout-typed output — that reject IS the anti-forgery rule. The one intro
\ form is a generated-constructor plan. ;SUMTYPE queues every constructor's
\ symbol and physical pad+tag cell count from SUMV metadata before evaluating
\ the complete generated source once. Boundary unification consumes that sealed
\ queue in source order and verifies each body against its metadata-derived RAW
\ row — the declared inputs untouched underneath, the counted type-n cells
\ (zero pads + tag) on top. The declared hidden-field sig is then recorded
\ through the normal certify path, coercing the raw cells into the layout bundle
\ by construction. Both rows derive from the same SUMV record, and only the
\ sumtype declaration path (sealed against user reopening) can arm the queue.
4 constant CTOR-PEND-CAP-INIT
2 cells constant CTOR-PEND-REC
$7FFFFFFFFFFFFFFF constant CTOR-PEND-BYTE-MAX
CTOR-PEND-BYTE-MAX CTOR-PEND-REC / constant CTOR-PEND-ROW-MAX
create CTOR-PEND-BOOT CTOR-PEND-CAP-INIT CTOR-PEND-REC * allot
PTR-VARIABLE CTOR-PEND-P   CTOR-PEND-BOOT CTOR-PEND-P !
variable CTOR-PEND-CAP     CTOR-PEND-CAP-INIT CTOR-PEND-CAP !
variable CTOR-PEND-COUNT
variable CTOR-PEND-POS
variable CTOR-PEND-K
variable CTOR-PEND-I

: CTOR-PEND-ROW ( n -- ptr a )
   dup 0 < over CTOR-PEND-CAP @ >= or IF
      s" checker: generated constructor plan capacity overflow" 76 die
   THEN
   CTOR-PEND-REC * CTOR-PEND-P @ + ;

: CTOR-PEND-SYM ( n -- ptr a )
   CTOR-PEND-ROW ;

: CTOR-PEND-CELLS ( n -- ptr a )
   CTOR-PEND-ROW CELL + ;

: CTOR-PEND-ROWS>BYTES ( n -- n )
   dup 0 < over CTOR-PEND-ROW-MAX > or IF
      s" checker: generated constructor plan capacity overflow" 76 die
   THEN
   CTOR-PEND-REC * ;

: CTOR-PEND-NEXT-CAP ( -- n )
   CTOR-PEND-COUNT @ CTOR-PEND-ROW-MAX >= IF
      s" checker: generated constructor plan capacity overflow" 76 die
   THEN
   CTOR-PEND-COUNT @ 1 + {: need:n :}
   CTOR-PEND-CAP @ 0 <= CTOR-PEND-CAP @ CTOR-PEND-ROW-MAX > or IF
      s" checker: generated constructor plan capacity overflow" 76 die
   THEN
   CTOR-PEND-CAP @ CTOR-PEND-ROW-MAX 2 / <= IF
      CTOR-PEND-CAP @ 2 * need max EXIT
   THEN
   need ;

: CTOR-PEND-ENSURE ( -- )
   CTOR-PEND-COUNT @ CTOR-PEND-CAP @ < IF EXIT THEN
   CTOR-PEND-NEXT-CAP {: cap:n :}
   CTOR-PEND-P @ CTOR-PEND-CAP @ CTOR-PEND-ROWS>BYTES
      cap CTOR-PEND-ROWS>BYTES ARENA-BYTES-GROW CTOR-PEND-P !
   cap CTOR-PEND-CAP ! ;

: CTOR-PEND-CLEAR ( -- )
   0 CTOR-PEND-COUNT !
   0 CTOR-PEND-POS !
   0 CTOR-PEND-K ! ;

: CTOR-PEND-REWIND ( -- )
   0 CTOR-PEND-POS !
   0 CTOR-PEND-K ! ;

: CTOR-PEND-SYM+ ( n n -- ) {: sym:n cells:n :}
   cells 0 < IF s" checker: negative generated constructor width" 76 die THEN
   CTOR-PEND-ENSURE
   0
   BEGIN dup CTOR-PEND-COUNT @ < WHILE
      dup CTOR-PEND-SYM @ sym = IF
         drop s" checker: duplicate generated constructor plan" 76 die
      THEN
      1 +
   REPEAT
   drop
   sym CTOR-PEND-COUNT @ CTOR-PEND-SYM !
   cells CTOR-PEND-COUNT @ CTOR-PEND-CELLS !
   CTOR-PEND-COUNT @ 1 + CTOR-PEND-COUNT ! ;

: CTOR-PEND! ( ptr u8 n n -- ) {: a:ptr u:n cells:n :}
   CTOR-PEND-CLEAR
   a u CHECKER-RECORD-SYM cells CTOR-PEND-SYM+ ;

: CTOR-PEND-MATCH? ( -- bool )
   CTOR-PEND-POS @ CTOR-PEND-COUNT @ <  NMU @ 0 > and 0= IF RES-FALSE EXIT THEN
   NMA @ NMU @ CHECKER-RECORD-SYM
      CTOR-PEND-POS @ CTOR-PEND-SYM @ <> IF RES-FALSE EXIT THEN
   CTOR-PEND-POS @ CTOR-PEND-CELLS @ CTOR-PEND-K !
   CTOR-PEND-POS @ 1 + CTOR-PEND-POS !
   RES-TRUE ;

: CTOR-PEND-REQUIRE-DONE ( -- )
   CTOR-PEND-POS @ CTOR-PEND-COUNT @ <> IF
      s" checker: incomplete generated constructor plan" 76 die
   THEN ;

: CTOR-EXPECTED-ROW ( -- n )          \ SGIN + planned raw cells on top
   SGIN @
   0 CTOR-PEND-I !
   BEGIN CTOR-PEND-I @ CTOR-PEND-K @ < WHILE
      CC-N MK-CON swap MK-PUSH
      CTOR-PEND-I @ 1 + CTOR-PEND-I !
   REPEAT ;

\ --- checked nominal/family cast certification (dot habu-checked-cast-primitive).
\ CAST: (roles.f) arms this one-shot window with the declared cast name, then
\ drives an ordinary checked colon compile of ": NAME ( in -- out ) <body> ;".
\ On the matching CHECK the body is certified against SGIN — the identity retype
\ ( in -- in ) — while the shared publish tail records the declared ( in -- out )
\ row. It is the CTOR-PEND sibling for the nominal/family retype class instead of
\ generated constructors. CAST-PEND! is a plain allowlist prim (above),
\ deliberately NOT xref-erased: the window only ever admits an ( in -- in )-
\ certified body retyped within the same single-cell machine class, which adds no
\ unsoundness a plain checked word could not already contain — a checked word can
\ already mint a nominal through an existing converter. The legality gate refuses
\ any wider power (non-unit arity, a class/width reinterpret, an undeclared
\ family), so exposing the arming prim grants no forgery the checker lacked.
variable CAST-PEND-A   variable CAST-PEND-U   0 CAST-PEND-U !
: CAST-PEND! ( ptr u8 n -- ) {: a:ptr u:n :}
   u CAST-PEND-U !  a CAST-PEND-A ! ;
: CAST-PEND-CLEAR ( -- ) 0 CAST-PEND-U ! ;
: CAST-PEND-MATCH? ( -- bool )
   CAST-PEND-U @ 0 >  NMU @ 0 >  and 0= IF RES-FALSE EXIT THEN
   CAST-PEND-A @ CAST-PEND-U @ NMA @ NMU @ CORE-STR=CI ;   \ NMA is case-folded; the armed name is verbatim

\ CAST-ROW-1? : the row is exactly one term deep over its base (open or closed).
: CAST-ROW-1? ( n -- bool )
   R-RES dup TAG S-PUSH <> IF drop RES-FALSE EXIT THEN
   P>REST R-RES TAG S-ROW = ;
\ CAST-ROW-TERM : the single top term of a one-deep row.
: CAST-ROW-TERM ( n -- n ) R-RES P>TYPE ;
\ CAST-CELL? : term resolves to a single retype-eligible machine cell — a plain
\ cell (con or type var), an arity-0 family scalar, or a parametric family cell
\ (all width 1). A pointer, quotation, atom, or a W>1 layout term is refused as a
\ class/width reinterpret.
: CAST-CELL? ( n -- bool ) {: t0:n :}
   t0 T-RES {: t:n :}
   t TAG T-CON = IF RES-TRUE EXIT THEN
   t TAG T-VAR = IF RES-TRUE EXIT THEN
   t TAG T-PARAM = IF t T-WIDTH 1 = EXIT THEN
   RES-FALSE ;
\ CAST-CERTIFY : the matched-window certification. Throw the named reject when the
\ declared retype is illegal, else certify the body output against SGIN (the
\ identity ( in -- in ) flow); the shared tail then records the declared row.
: CAST-CERTIFY ( -- )
   SGBAD-UNKNOWN? IF E-CAST-FAM throw THEN
   SGHASR @ 0 <> IF E-CAST-ARITY throw THEN
   SGIN @ CAST-ROW-1? 0= IF E-CAST-ARITY throw THEN
   SGOUT @ CAST-ROW-1? 0= IF E-CAST-ARITY throw THEN
   SGIN @ CAST-ROW-TERM CAST-CELL? 0= IF E-CAST-CLASS throw THEN
   SGOUT @ CAST-ROW-TERM CAST-CELL? 0= IF E-CAST-CLASS throw THEN
   SGIN @ SUNI-COERCE ;

\ Generative layout-buffer authorization. xref.f erases every arming-state
\ dictionary name after compiling the allocator and CHECK, leaving only their
\ baked direct references. User source therefore cannot arm or mutate the
\ one-shot boundary.
: LBUF-PEND! ( ptr u8 n -- ) {: a:ptr u:n :}
   a LBUF-PEND-A !  u LBUF-PEND-U ! ;
: LBUF-PEND-CLEAR ( -- ) 0 LBUF-PEND-U !  0 LAYOUT-INTRO ! ;
: LBUF-PEND-MATCH? ( -- bool )
   LBUF-PEND-U @ 0 > NMU @ 0 > and 0= IF RES-FALSE EXIT THEN
   LBUF-PEND-A @ LBUF-PEND-U @ NMA @ NMU @ CORE-STR=CI ;

: CHECK {: a u :}   \ ( a u -- -1=certified | 0=rejected | 1=uncheckable )
   a u CHECK-RESET
   CHECK-SCAN
   0 LAYOUT-XPORT !                  \ boundary unification is never in transport mode
   SIG-EFF-DROP
   CHECK-FOLD-EXITS
   CONM @ 0 <> IF MD-CON-TRUNC MDIAG! THEN     \ latch truncation BEFORE the boundary
   MM @ 0 <>  MF-DEPTH @ 0 <>  or IF MD-TRUNC MDIAG! THEN   \ unify pins its own mismatch
   CHECK-SIG? IF CHECK-NO-BORROW THEN
   CHECK-SIG? IF
      CTOR-PEND-MATCH? IF
         \ Matching consumes exactly one row from the sealed constructor plan.
         \ TDECL-GEN-EVAL's catch boundary clears it on every exit.
         CTOR-EXPECTED-ROW SUNI-COERCE
      ELSE CAST-PEND-MATCH? IF
         CAST-PEND-CLEAR                            \ single shot, even on reject
         CAST-CERTIFY                               \ identity retype; the tail publishes ( in -- out )
      ELSE
         LBUF-PEND-MATCH? IF
            -1 LAYOUT-INTRO !
            SGOUT @ SUNI-COERCE
            0 LAYOUT-INTRO !
         ELSE
            SGOUT @ SUNI-COERCE
         THEN
      THEN THEN
      OK @ IF SGIN @ BROW !  SGOUT @ DCUR ! THEN    \ record the verified declared effect
   THEN                                        \ SUNI captures declared(exp)/inferred(act)
   LMODE @ 0 <>  #CFC @ 0 <>  or  CONM @ 0 <>  or  MM @ 0 <>  or IF CF-FAIL THEN
   SGHASR @ 0= IF RCUR @ R-RES  RBROW @ R-RES  <> IF 0 OK ! THEN THEN   \ balance (no clause)
   CHECK-RET-SIG? IF
      RCUR @ SGROUT @ UNIFY-COERCE OK @ and OK !
      OK @ IF SGRIN @ RBROW !  SGROUT @ RCUR ! THEN
   THEN
   CHECK-SIG? OK @ and IF NP-CHECK THEN               \ declared quantifiers must stay parametric
   CHECK-VERDICT                                      \ malformed/unsafe/non-parametric rejects
   dup DVERD !
   dup 0= IF RIGID-DIAG-CLASSIFY THEN                 \ name a rigid host-identity mismatch
   dup 0 =  over 1 = JSON-DIAGS @ and  or
   dup MEO-ON @ and IF MEO-APPLY THEN     \ file-relative origin for this def's diagnostic
   DIAG-QUIET @ 0= and IF DIAGXT THEN
   dup -1 = NMU @ 0 > and IF
      0 CTLNEW !
      DEADP @ XSET @ 0= and IF CTLNEW @ CTL-DEAD or CTLNEW ! THEN
      THSET @ IF CTLNEW @ CTL-THROW or CTLNEW ! THEN
      NMA @ NMU @ CTL-FLAGS CTLNEW @ <> IF
         NMA @ NMU @ CTLNEW @ NORET-ADD
      THEN
      CHECK-SIG? IF
         SGA @ SGU @  NMA @ NMU @  CHECKER-USIG-CERT-ADD
         CERT-REPOINT-ROWS
      ELSE
         NMA @ NMU @ RECXT
      THEN
   THEN
   dup 0 =  MULTI-ERR?  and  NMU @ 0 >  and IF          \ reject in multi-error mode:
      1 MULTI-ERR-N +!                                  \ count it (fail-closed exit) and
      CHECK-SIG? SGBAD @ 0= and IF                      \ trust the declared sig so later
         SGA @ SGU @  NMA @ NMU @  CHECKER-USIG-CERT-ADD \ definitions keep checking —
      THEN                                              \ unless the sig itself was bad
   THEN ;

\ ---------------------------------------------------------------------------
\ Transactional rollback-frame STACK. Depth-safe replacement for the old single
\ CAND-*/CSCOPE- slots: every checker scope (CHECKER-SCOPE-START) and candidate
\ probe (CHECK-CANDIDATE-START) pushes a frame holding every mutable high-water
\ mark, and DONE pops it. Nested candidates/scopes (all-errors replay, preverify,
\ CHK-RUN-STATIC-LINTS inside CHK-RUN-SCOPED) therefore cannot overwrite a
\ parent's saved marks. Core marks live in the RBF frame; the TFAM/SUMV/SCHEMA
\ registries hang parallel marks off the REG-EXT-RB-* hooks that type-schema.f /
\ type-family.f install, kept in lockstep because every push pairs with one pop.
\ ---------------------------------------------------------------------------
: REG-EXT-RB-NOOP ( -- )   \ default: no extension registries installed yet
;

defer REG-EXT-RB-SAVE-XT ( -- )
defer REG-EXT-RB-FINALIZE-XT ( -- )
defer REG-EXT-RB-RESTORE-XT ( -- )

: REG-EXT-RB-DEFAULTS ( -- )
   [: REG-EXT-RB-NOOP ;] is REG-EXT-RB-SAVE-XT
   [: REG-EXT-RB-NOOP ;] is REG-EXT-RB-FINALIZE-XT
   [: REG-EXT-RB-NOOP ;] is REG-EXT-RB-RESTORE-XT ;
REG-EXT-RB-DEFAULTS

$0 constant RBF.UEND-OFF
$8 constant RBF.NEND-OFF
$10 constant RBF.SYMN-OFF
$18 constant RBF.SYMU-OFF
$20 constant RBF.CTN-OFF
$28 constant RBF.CTU-OFF
$30 constant RBF.LIN-OFF
$38 constant RBF.VRECN-OFF
$40 constant RBF.VRECF-OFF
$48 constant RBF.VRECND-OFF
$50 constant RBF.VNARGN-OFF
$58 constant RBF.VRECU-OFF
$60 constant RBF.CAND-OFF
$68 constant RBF.VSIG-OFF
$70 constant RBF.PKGMODE-OFF
$78 constant RBF.PKGU-OFF
$80 constant RBF.DFEREND-OFF
$88 constant RBF.COORD-OFF
$90 constant RBF-REC
$8 constant RBF-REC-ALIGN
0 constant RBF-REC-PTR-MASK

\ RBF.COORD is the sole owner mark on a rollback frame: RBF-NO-COORDINATOR means
\ an ordinary checker scope, and any other value is the GENERATED-DECL coordinator
\ depth that owns the frame. One field, one authority — a separate boolean tag
\ would restate what a real depth already says and could drift out of step with it.
-1 constant RBF-NO-COORDINATOR

: RBF.UEND ( ptr a -- ptr a ) RBF.UEND-OFF + ;
: RBF.NEND ( ptr a -- ptr a ) RBF.NEND-OFF + ;
: RBF.SYMN ( ptr a -- ptr a ) RBF.SYMN-OFF + ;
: RBF.SYMU ( ptr a -- ptr a ) RBF.SYMU-OFF + ;
: RBF.CTN ( ptr a -- ptr a ) RBF.CTN-OFF + ;
: RBF.CTU ( ptr a -- ptr a ) RBF.CTU-OFF + ;
: RBF.LIN ( ptr a -- ptr a ) RBF.LIN-OFF + ;
: RBF.VRECN ( ptr a -- ptr a ) RBF.VRECN-OFF + ;
: RBF.VRECF ( ptr a -- ptr a ) RBF.VRECF-OFF + ;
: RBF.VRECND ( ptr a -- ptr a ) RBF.VRECND-OFF + ;
: RBF.VNARGN ( ptr a -- ptr a ) RBF.VNARGN-OFF + ;
: RBF.VRECU ( ptr a -- ptr a ) RBF.VRECU-OFF + ;
: RBF.CAND ( ptr a -- ptr a ) RBF.CAND-OFF + ;
: RBF.VSIG ( ptr a -- ptr a ) RBF.VSIG-OFF + ;
: RBF.PKGMODE ( ptr a -- ptr a ) RBF.PKGMODE-OFF + ;
: RBF.PKGU ( ptr a -- ptr a ) RBF.PKGU-OFF + ;
: RBF.DFEREND ( ptr a -- ptr a ) RBF.DFEREND-OFF + ;
: RBF.COORD ( ptr a -- ptr a ) RBF.COORD-OFF + ;

RBF.UEND-OFF 0 cells CHECKER-LAYOUT=
RBF.NEND-OFF 1 cells CHECKER-LAYOUT=
RBF.SYMN-OFF 2 cells CHECKER-LAYOUT=
RBF.SYMU-OFF 3 cells CHECKER-LAYOUT=
RBF.CTN-OFF 4 cells CHECKER-LAYOUT=
RBF.CTU-OFF 5 cells CHECKER-LAYOUT=
RBF.LIN-OFF 6 cells CHECKER-LAYOUT=
RBF.VRECN-OFF 7 cells CHECKER-LAYOUT=
RBF.VRECF-OFF 8 cells CHECKER-LAYOUT=
RBF.VRECND-OFF 9 cells CHECKER-LAYOUT=
RBF.VNARGN-OFF 10 cells CHECKER-LAYOUT=
RBF.VRECU-OFF 11 cells CHECKER-LAYOUT=
RBF.CAND-OFF 12 cells CHECKER-LAYOUT=
RBF.VSIG-OFF 13 cells CHECKER-LAYOUT=
RBF.PKGMODE-OFF 14 cells CHECKER-LAYOUT=
RBF.PKGU-OFF 15 cells CHECKER-LAYOUT=
RBF.DFEREND-OFF 16 cells CHECKER-LAYOUT=
RBF.COORD-OFF 17 cells CHECKER-LAYOUT=
RBF-REC 18 cells CHECKER-LAYOUT=
RBF-REC-ALIGN CELL CHECKER-LAYOUT=
RBF-REC RBF-REC-ALIGN mod 0 CHECKER-LAYOUT=
RBF-REC-PTR-MASK 0 CHECKER-LAYOUT=
0 RBF.UEND RBF.UEND-OFF CHECKER-LAYOUT=
0 RBF.NEND RBF.NEND-OFF CHECKER-LAYOUT=
0 RBF.SYMN RBF.SYMN-OFF CHECKER-LAYOUT=
0 RBF.SYMU RBF.SYMU-OFF CHECKER-LAYOUT=
0 RBF.CTN RBF.CTN-OFF CHECKER-LAYOUT=
0 RBF.CTU RBF.CTU-OFF CHECKER-LAYOUT=
0 RBF.LIN RBF.LIN-OFF CHECKER-LAYOUT=
0 RBF.VRECN RBF.VRECN-OFF CHECKER-LAYOUT=
0 RBF.VRECF RBF.VRECF-OFF CHECKER-LAYOUT=
0 RBF.VRECND RBF.VRECND-OFF CHECKER-LAYOUT=
0 RBF.VNARGN RBF.VNARGN-OFF CHECKER-LAYOUT=
0 RBF.VRECU RBF.VRECU-OFF CHECKER-LAYOUT=
0 RBF.CAND RBF.CAND-OFF CHECKER-LAYOUT=
0 RBF.VSIG RBF.VSIG-OFF CHECKER-LAYOUT=
0 RBF.PKGMODE RBF.PKGMODE-OFF CHECKER-LAYOUT=
0 RBF.PKGU RBF.PKGU-OFF CHECKER-LAYOUT=
0 RBF.DFEREND RBF.DFEREND-OFF CHECKER-LAYOUT=
0 RBF.COORD RBF.COORD-OFF CHECKER-LAYOUT=

16 constant RBF-CAP-INIT
variable RBF-CAP-V   RBF-CAP-INIT RBF-CAP-V !
create RBF-A-BOOT      RBF-CAP-INIT RBF-REC * allot
variable RBF-A-P       RBF-A-BOOT RBF-A-P !
: RBF-BASE ( -- ptr a ) RBF-A-P @ ;
create RBF-NAME-BOOT   RBF-CAP-INIT CHECKER-PACKAGE-CAP * allot
variable RBF-NAME-P    RBF-NAME-BOOT RBF-NAME-P !
: RBF-NAME-BASE ( -- ptr a ) RBF-NAME-P @ ;
variable RBF-DEPTH   0 RBF-DEPTH !

: RBF-GROW ( -- )
   RBF-CAP-V @ 2 * {: nc:n :}
   RBF-A-P    RBF-CAP-V @ RBF-REC *              nc RBF-REC *              REG-GROW1
   RBF-NAME-P RBF-CAP-V @ CHECKER-PACKAGE-CAP *  nc CHECKER-PACKAGE-CAP *  REG-GROW1
   nc RBF-CAP-V ! ;
: RBF-ENSURE ( -- )
   RBF-DEPTH @ RBF-CAP-V @ < IF exit THEN
   RBF-GROW ;
: RBF-CUR ( -- ptr a )       RBF-DEPTH @ RBF-REC * RBF-BASE + ;
: RBF-NAME-CUR ( -- ptr a )  RBF-DEPTH @ CHECKER-PACKAGE-CAP * RBF-NAME-BASE + ;

\ RBF-SNAP-RESET ( -- ) : snapshot prepare — frames are transient (depth 0 at
\ snapshot), so drop any grown arena buffer back to the baked boot store; the
\ next scope re-grows lazily. Mirrors HIDX-RESET's process-local reset.
: RBF-SNAP-RESET ( -- )
   RBF-DEPTH @ IF s" checker: snapshot inside rollback scope" 76 die THEN
   RBF-A-BOOT RBF-A-P !
   RBF-NAME-BOOT RBF-NAME-P !
   RBF-CAP-INIT RBF-CAP-V !
   0 RBF-DEPTH !
   MF-DEPTH @ IF s" checker: snapshot inside match frame" 76 die THEN
   MF-A-BOOT MF-A-P !          \ MATCH frames are per-definition transient: drop any
   MF-CAP-INIT MF-CAP-V !      \ grown arena back to the baked boot stores
   MSEEN-BOOT MSEEN-P !
   MSEEN-CAP-INIT MSEEN-CAP-V !
   0 MSEEN-N ! ;

: RBF-PUSH ( -- )          \ save every current high-water mark into a new frame
   RBF-ENSURE
   RBF-CUR {: r:ptr :}
   UEND @ r RBF.UEND !
   NORET-END @ r RBF.NEND !
   SYM-N @ r RBF.SYMN !
   SYM-STR-U @ r RBF.SYMU !
   CTN @ r RBF.CTN !
   CT-STR-U @ r RBF.CTU !
   LIN-NDECL @ r RBF.LIN !
   VREC-N @ r RBF.VRECN !
   VREC-FIELD-N @ r RBF.VRECF !
   VREC-NODE-N @ r RBF.VRECND !
   VNARG-N @ r RBF.VNARGN !
   VREC-STR-U @ r RBF.VRECU !
   CHK-CAND @ r RBF.CAND !
   VSIG @ r RBF.VSIG !
   CHECKER-PACKAGE-MODE @ r RBF.PKGMODE !
   CHECKER-PACKAGE-U @ r RBF.PKGU !
   DFER-END @ r RBF.DFEREND !
   RBF-NO-COORDINATOR r RBF.COORD !
   CHECKER-PACKAGE-NAME RBF-NAME-CUR CHECKER-PACKAGE-U @ USIGS-COPY
   RBF-DEPTH @ 1 + RBF-DEPTH !
   REG-EXT-RB-SAVE-XT ;

\ Restore the top frame, running the caller's registry restore first.  The
\ restore action is a parameter rather than a mode flag: an ordinary scope hands
\ over the installed extension hook (which rejects a stale product-field depth),
\ while a declaration frame hands over its own throw-free restore.  A throw
\ inside either action can therefore never leave a latch behind that disables
\ product-field validation for every later pop.
: RBF-POP-WITH ( [ -- ] -- )
   execute                 \ registry restore for the frame this pop retires
   RBF-DEPTH @ 1 - RBF-DEPTH !
   RBF-CUR {: r:ptr :}
   r RBF.UEND @ USIGS-RESTORE-END
   r RBF.NEND @ NORET-RESTORE-END
   r RBF.SYMN @ HIDX-SYMS-RETIRE      \ pop retired hash-index rows before SYM-N rewinds
   r RBF.SYMN @ SYM-N !
   r RBF.SYMU @ SYM-STR-U !
   r RBF.CTN @ CTN !
   r RBF.CTU @ CT-STR-U !
   r RBF.LIN @ LIN-NDECL !
   r RBF.VRECN @ VREC-N !
   r RBF.VRECF @ VREC-FIELD-N !
   r RBF.VRECND @ VREC-NODE-N !
   r RBF.VNARGN @ VNARG-N !
   r RBF.VRECU @ VREC-STR-U !
   r RBF.CAND @ CHK-CAND !
   r RBF.VSIG @ VSIG !
   r RBF.PKGMODE @ CHECKER-PACKAGE-MODE !
   r RBF.PKGU @ CHECKER-PACKAGE-U !
   RBF-NAME-CUR CHECKER-PACKAGE-NAME r RBF.PKGU @ USIGS-COPY
   r RBF.DFEREND @ DFER-END !
   DFER-TERM ;                        \ null-terminate the DFER scan at the restored end

: RBF-POP ( -- )           \ restore every mark from the top frame, retiring index rows
   [: REG-EXT-RB-RESTORE-XT ;] RBF-POP-WITH ;

: RBF-FINALIZE ( -- )     \ keep every published row and release the live savepoint
   REG-EXT-RB-FINALIZE-XT
   RBF-DEPTH @ 1 - RBF-DEPTH ! ;

package CHECKER-DECL-FRAME

: CLOSE-PRIVATE ( -- )
   PE-PKG-A @ PE-PKG-U @ SYM-PRIVATE PE-NA@ PE-NU @ SYM-INTERN
   PE-CLOSE-SYM ;

PPRIM: CHECKER-DECL-FRAME START PE-N PE-IN CLOSE-PRIVATE
PPRIM: CHECKER-DECL-FRAME PREPARE PE-N PE-IN  PE-F PE-OUT CLOSE-PRIVATE
PPRIM: CHECKER-DECL-FRAME ROLLBACK PE-N PE-IN CLOSE-PRIVATE
PPRIM: CHECKER-DECL-FRAME RELEASE CLOSE-PRIVATE

defer TYPES-READY-XT ( n -- bool )
defer TYPES-RESTORE-XT ( -- )
defer TYPES-RELEASE-XT ( -- )

\ The default answers the depth question the deferred word is asked: with no type
\ coordinator installed there is nothing to wait for, so every depth is ready.
: TYPES-READY-OK ( n -- bool ) drop 0 0= ;
: NOOP ( -- ) ;
: TYPES-DEFAULTS ( -- )
   [: TYPES-READY-OK ;] is TYPES-READY-XT
   [: NOOP ;] is TYPES-RESTORE-XT
   [: NOOP ;] is TYPES-RELEASE-XT ;
TYPES-DEFAULTS

: FRAME ( n -- ptr a )       \ rollback frame record at a live depth index
   RBF-REC * RBF-BASE + ;

: TOP ( -- ptr a )
   RBF-DEPTH @ 1 - FRAME ;

: ORDINARY? ( n -- bool )    \ index -- frame is an ordinary checker scope?
   FRAME RBF.COORD @ RBF-NO-COORDINATOR = ;

: DECL-AT? ( n n -- bool ) {: idx:n depth:n :}   \ index, coordinator depth
   idx FRAME RBF.COORD @ depth = ;

: TOP-MATCH? ( n -- bool ) {: depth:n :}
   RBF-DEPTH @ 0= IF 0 0= 0= EXIT THEN
   RBF-DEPTH @ 1 - depth DECL-AT? ;

: RESTORE-TOP ( -- )         \ declaration-owned pop: no product-field rejection
   [: TYPES-RESTORE-XT ;] RBF-POP-WITH ;

\ Is there one more leaked ordinary scope below the ones already counted?  The
\ index is derived from the live depth, so the walk stops at the bottom of the
\ arena instead of reading the record before it.
: ORDINARY-BELOW? ( n -- n bool )
   dup RBF-DEPTH @ >= IF 0 0= 0= EXIT THEN
   dup RBF-DEPTH @ 1 - swap - ORDINARY? ;

: ORDINARY-RUN ( -- n )      \ leaked ordinary scopes above the first coordinator-owned frame
   0
   BEGIN ORDINARY-BELOW? WHILE
      1 +
   REPEAT ;

: POP-ORDINARY ( n -- )      \ discard exactly this many leaked ordinary scopes
   BEGIN dup 0 > WHILE
      RESTORE-TOP
      1 -
   REPEAT
   drop ;

: START ( n -- ) {: depth:n :}
   RBF-PUSH
   TOP {: r:ptr :}
   depth r RBF.COORD ! ;

: PREPARE ( n -- bool ) {: depth:n :}
   depth TOP-MATCH? 0= IF 0 0= 0= EXIT THEN
   RBF-DEPTH @ TYPES-READY-XT ;

: FRAME-MISMATCH ( -- )
   s" checker: declaration rollback frame mismatch" 76 die ;

\ Peek before consuming anything: find this coordinator's frame underneath the
\ leaked ordinary scopes first.  If it is not there the frame stack is beyond
\ repair, so fail closed while the legitimate outer frames are still intact.
: ROLLBACK ( n -- ) {: depth:n :}
   ORDINARY-RUN {: leaked:n :}
   RBF-DEPTH @ leaked - 1 - {: idx:n :}
   idx 0 < IF FRAME-MISMATCH THEN
   idx depth DECL-AT? 0= IF FRAME-MISMATCH THEN
   leaked POP-ORDINARY
   RESTORE-TOP ;

\ RELEASE runs last, after every other participant has already published, so it
\ must not be able to reject.  It therefore drops its own frame instead of going
\ through the ordinary CHECKER-SCOPE-FINALIZE, whose one rejecting condition is a
\ product-field transaction depth that no longer matches the frame.  Today that
\ mismatch is impossible to reach from a declaration body: the only way to shift
\ the field depth across this point is to leave a declaration-event frame open,
\ and src/core/decl-event.f rejects exactly that at the participant's PREPARE
\ (DEV-PART-PROVE -> DEV-TX-OPEN-REQUIRE), with the depths restored.  Keep the
\ delegation anyway — throw-freeness here is a contract of the total-release
\ protocol, not a consequence of that upstream guard, and the upstream guard is
\ free to move.  Dot habu-make-txn-release-c9892c89 owns the structural inventory
\ that will make a rejecting release fail closed.
: RELEASE ( -- )
   TYPES-RELEASE-XT
   RBF-DEPTH @ 1 - RBF-DEPTH ! ;

;package

: CHECKER-SCOPE-START ( -- )
   RBF-PUSH ;

: CHECKER-SCOPE-FINALIZE ( -- )
   RBF-FINALIZE ;

: CHECKER-SCOPE-DONE ( -- )
   RBF-POP ;

: CHECKER-SCOPE-DEPTH ( -- n )
   RBF-DEPTH @ ;

: CHECK-CANDIDATE-START ( -- )
   RBF-PUSH
   -1 CHK-CAND !
   -1 VSIG !
   0 PIMM-STREAM ! ;   \ candidate text is RAW source: parsing-immediate payloads present

: CHECK-CANDIDATE-DONE ( n -- n )
   -1 PIMM-STREAM !    \ back to the engine-stream default (hook loads)
   RBF-POP ;

variable CAND-A   variable CAND-U   variable CAND-VERDICT
: CHECK-CANDIDATE-BODY ( -- )        \ ( -- ) closure: check the stashed source, stash the verdict
   CAND-A @ CAND-U @ CHECK CAND-VERDICT ! ;
\ Throw-safe: a throw inside CHECK must not unwind past the candidate pop, or the
\ rollback frame leaks (RBF-DEPTH stuck, rejected rows survive) and the next probe
\ runs on corrupted state. Mirror CHK-RUN-SCOPED: run the body under catch, pop the
\ frame unconditionally, re-throw the caught code, return the verdict on success.
: CHECK-CANDIDATE! ( ptr u8 n -- n ) {: a:ptr u:n :}
   a CAND-A !  u CAND-U !
   CHECK-CANDIDATE-START
   [: CHECK-CANDIDATE-BODY ;] catch {: rc:n :}
   0 CHECK-CANDIDATE-DONE drop
   rc 0 <> IF rc throw THEN
   CAND-VERDICT @ ;

: CHECKER-CANDIDATE-SCOPE-START ( -- )
   CHECK-CANDIDATE-START ;

: CHECKER-CANDIDATE-SCOPE-DONE ( -- )
   0 CHECK-CANDIDATE-DONE drop ;

\ CHECK! ( a u -- flag ) : like CHECK but VERIFIES the body against a leading
\ ( in -- out ) declared sig (rejects on mismatch). The standalone REPL hook.
: CHECK! ( ptr u8 n -- n ) {: a:ptr u:n :}
   -1 VSIG !
   a u CHECK {: verdict:n :}
   0 VSIG !
   a u verdict CHECKER-CERT:PRODUCE
   verdict ;

package CHECKER-PREFLIGHT

: RUN ( -- )
   BODY-A@ BODY-U @ CHECK! VERDICT ! ;

public

: CHECK! ( ptr u8 n ptr u8 n -- n )
   {: ba:ptr bu:n ta:ptr tu:n :}
   ba BODY-A!  bu BODY-U !  ta TARGET-A!  tu TARGET-U !
   -1 ACTIVE !
   [: RUN ;] catch {: rc:n :}
   0 ACTIVE !
   rc 0 <> IF rc throw THEN
   VERDICT @ ;

;package

: DOES-DIN ( n -- n )
   FRESH MK-VAR MK-PTR swap MK-PUSH ;

: RAW-SIG! ( n n n n -- )
   PD-BASE @ SGDBASE !
   RR-SHARED @ SGRBASE !
   SGHASR @ IF
      SGROUT !  SGRIN !  SGOUT !  SGIN !
   ELSE
      2drop  SGOUT !  SGIN !
   THEN ;

\ CHECK-DOES! ( body-a body-u sig-a sig-u -- verdict ) verifies a DOES> body
\ against a created-word runtime effect.  If the created word is declared
\ `( in -- out )`, the DOES> body must type as `( in ptr a -- out )`: the native
\ CREATE stub pushes the created word's data-field address before branching to
\ the DOES> body.
: CHECK-DOES! {: ba bu sa su :}
   ba bu CHECK-RESET
   0 TOK0 !
   sa su PARSE-SIG-RAW RAW-SIG!
   SGIN @ DOES-DIN dup BROW ! DCUR !
   SGHASR @ IF SGRIN @ dup RBROW ! RCUR ! THEN
   CHECK-SCAN
   CHECK-FOLD-EXITS
   CONM @ 0 <> IF MD-CON-TRUNC MDIAG! THEN     \ latch truncation BEFORE the boundary
   MM @ 0 <>  MF-DEPTH @ 0 <>  or IF MD-TRUNC MDIAG! THEN
   CHECK-NO-BORROW
   SGOUT @ SUNI-COERCE
   OK @ IF SGOUT @ DCUR ! THEN
   LMODE @ 0 <>  #CFC @ 0 <>  or  CONM @ 0 <>  or  MM @ 0 <>  or IF CF-FAIL THEN
   SGHASR @ 0= IF RCUR @ R-RES  RBROW @ R-RES  <> IF 0 OK ! THEN THEN
   SGHASR @ IF RCUR @ SGROUT @ UNIFY-COERCE OK @ and OK ! THEN
   CHECK-VERDICT dup DVERD ! ;
