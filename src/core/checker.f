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
variable TVT-P     variable RVT-P
variable VRC-TV-P  variable VRC-RV-P   variable VRI-TV-P  variable VRI-RV-P
variable EC-TV-P   variable EC-RV-P    variable EI-TV-P   variable EI-RV-P

: TV-ARENA-BOOT ( -- )         \ point every var-id store at its boot buffer
   TVT-BOOT TVT-P !            RVT-BOOT RVT-P !
   VRC-TV-BOOT VRC-TV-P !      VRC-RV-BOOT VRC-RV-P !
   VRI-TV-BOOT VRI-TV-P !      VRI-RV-BOOT VRI-RV-P !
   EC-TV-BOOT EC-TV-P !        EC-RV-BOOT EC-RV-P !
   EI-TV-BOOT EI-TV-P !        EI-RV-BOOT EI-RV-P ! ;
TV-ARENA-BOOT

: TVT ( -- ptr a ) TVT-P @ ;         : RVT ( -- ptr a ) RVT-P @ ;
: VRC-TV ( -- ptr a ) VRC-TV-P @ ;   : VRC-RV ( -- ptr a ) VRC-RV-P @ ;
: VRI-TV ( -- ptr a ) VRI-TV-P @ ;   : VRI-RV ( -- ptr a ) VRI-RV-P @ ;
: EC-TV ( -- ptr a ) EC-TV-P @ ;     : EC-RV ( -- ptr a ) EC-RV-P @ ;
: EI-TV ( -- ptr a ) EI-TV-P @ ;     : EI-RV ( -- ptr a ) EI-RV-P @ ;

: TV-GROW-ONE ( ptr a n n -- ) {: pv:ptr oc:n nc:n :}   \ pv holds base; grow to nc cells
   nc cells ARENA-ALLOC {: nb:ptr :}
   pv @ nb oc cells ARENA-COPY
   nb oc nc ARENA-CELLS-UNBOUND
   nb pv ! ;

: TV-GROW ( n -- ) {: need:n :}
   need TV-CAP @ 2 * max {: nc:n :}   \ geometric: at least double
   TV-CAP @ {: oc:n :}
   TVT-P oc nc TV-GROW-ONE       RVT-P oc nc TV-GROW-ONE
   VRC-TV-P oc nc TV-GROW-ONE    VRC-RV-P oc nc TV-GROW-ONE
   VRI-TV-P oc nc TV-GROW-ONE    VRI-RV-P oc nc TV-GROW-ONE
   EC-TV-P oc nc TV-GROW-ONE     EC-RV-P oc nc TV-GROW-ONE
   EI-TV-P oc nc TV-GROW-ONE     EI-RV-P oc nc TV-GROW-ONE
   nc TV-CAP ! ;

: TV-ENSURE ( n -- ) {: need:n :}   \ ensure cap >= need
   need TV-CAP @ <= IF exit THEN
   need TV-GROW ;

: TVINIT   \ unbind every type and row var (one-time load init; NEW uses TV-RESET)
   0 BEGIN
     dup cells TVT + UNBOUND swap !
     dup cells RVT + UNBOUND swap !
     1 + dup MAXTV 1 - >
   UNTIL drop ;
TVINIT

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
: TRAIL-PUSH ( n n -- ) {: id:n row:n :}   \ record a binding to var `id` (row? 1:0)
   TRAIL-N @ 1 + TRAIL-ENSURE
   id 2 * row +  TRAIL-N @ cells TRAIL + !
   TRAIL-N @ 1 + TRAIL-N ! ;
: TRAIL-UNWIND ( n -- ) {: mark:n :}     \ pop+unbind every binding above `mark`
   BEGIN TRAIL-N @ mark > WHILE
      TRAIL-N @ 1 - TRAIL-N !
      TRAIL-N @ cells TRAIL + @ {: e:n :}
      e 1 and 0= 0= IF UNBOUND e 2 / cells RVT + ! ELSE UNBOUND e 2 / cells TVT + ! THEN
   REPEAT ;

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
   1 RIGID-N ! ;
: RIGID-FRESH ( -- n )
   RIGID-N @ dup 1+ RIGID-N ! ;
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
variable PARAMOFF-P   variable PARGP-P   variable PARG-N   variable PARG-CAP-V
variable PARAM-SCR-P
variable PARAM-CAP     variable PARAM-SCR-CAP-V
PARAMA-BOOT PARAMA-P !   PARAMU-BOOT PARAMU-P !   PARAMC-BOOT PARAMC-P !
PARAMFAM-BOOT PARAMFAM-P !   PARAMOFF-BOOT PARAMOFF-P !
PARGP-BOOT PARGP-P !   PARG-INIT PARG-CAP-V !   0 PARG-N !
MAXPARAM-INIT PARAM-CAP !
PARAM-SCR-BOOT PARAM-SCR-P !    PARAM-SCR-INIT PARAM-SCR-CAP-V !
: PARAMA ( -- ptr a ) PARAMA-P @ ;
: PARAMU ( -- ptr a ) PARAMU-P @ ;
: PARAMC ( -- ptr a ) PARAMC-P @ ;
: PARAMFAM ( -- ptr a ) PARAMFAM-P @ ;
: PARAMOFF ( -- ptr a ) PARAMOFF-P @ ;
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
   nc PARAM-CAP ! ;

\ --- resolved type-family (TFAM) identity for T-PARAM terms. The TFAM registry
\ lives in src/core/type-family.f, loaded AFTER checker.f, so its query words are
\ reached through friend xt hooks installed at prefix load (0 = not yet loaded).
\ The TFAM-RESOLVE*/TFAM-ARITY* wrappers live just below RES-FALSE (which they use).
variable TFAM-RESOLVE-XT   0 TFAM-RESOLVE-XT !   \ ( pkg-a pkg-u name-a name-u -- id true | false )
variable TFAM-ARITY-XT     0 TFAM-ARITY-XT !     \ ( id -- arity )
variable TFAM-LAYOUT?-XT   0 TFAM-LAYOUT?-XT !   \ ( id -- bool ) : family id occupies an ADT layout
variable FIELD-FAM   -1 FIELD-FAM !              \ reserved family-id of the internal `field` ctor

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

\ friend xt wrappers over the TFAM registry query surface (installed by
\ type-family.f at prefix load); a 0 hook resolves nothing / arity 0. Both hooks
\ always return a fixed row (id-or-0 + flag, arity int), never a variable arity.
\ The xt sits ABOVE its data args, so we must not `?dup` it before `execute` (that
\ would leave a stray xt under the args and misalign the call) — branch on a dup.
: TFAM-RESOLVE* ( ptr u8 n ptr u8 n -- n bool )
   TFAM-RESOLVE-XT @ dup 0= IF drop 2drop 2drop 0 RES-FALSE ELSE execute THEN ;
: TFAM-ARITY* ( n -- n )
   TFAM-ARITY-XT @ dup 0= IF 2drop 0 ELSE execute THEN ;
: TFAM-LAYOUT?* ( n -- bool )      \ 0 hook (registry not yet loaded) -> not a layout
   TFAM-LAYOUT?-XT @ dup 0= IF 2drop RES-FALSE ELSE execute THEN ;

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
\ Parallel per-pair strictness flag, keyed by the pair's base worklist index.
\ Strict pairs (pointer pointees) unify by equality/var-binding only — integer
\ widening (INT-WIDENS?) applies to top-level scalar stack cells, never to a
\ pointer's pointee, so a concrete `ptr u8` never satisfies `ptr cell`/`ptr u32`.
create UWL-STR MAXUWL cells allot   variable CUR-STRICT

: U-PUSH ( n -- )
   USP @ MAXUWL 1 - > IF s" checker: unify worklist full" 76 die THEN
   USP @ cells UWL + !
   USP @ 1 + USP ! ;

: U-POP USP @ 1 - USP ! USP @ cells UWL + @ ;

: PAIR ( n n -- )    \ inherit the enclosing pair's strictness
   CUR-STRICT @ USP @ cells UWL-STR + !
   swap U-PUSH U-PUSH ;

: PAIR-STRICT ( n n -- )    \ force a strict (no-widen) subterm unification
   -1 USP @ cells UWL-STR + !
   swap U-PUSH U-PUSH ;

: UNPAIR ( -- n n )    \ pop a pair and restore its strictness into CUR-STRICT
   U-POP U-POP swap
   USP @ cells UWL-STR + @ CUR-STRICT ! ;

: FIELD-PARAM? ( n -- bool ) {: t:n :}
   FIELD-FAM @ 0 < IF RES-FALSE EXIT THEN   \ field family not registered (e.g. after TFAM-RESET)
   t T-RES TAG T-PARAM <> IF RES-FALSE EXIT THEN
   t T-RES PARAM>FAM FIELD-FAM @ = ;   \ identity by reserved family-id, not spelling

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
   got want FIELD-ID-SAME? 0= IF RES-FALSE UOK ! RES-TRUE EXIT THEN
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
: CON-OK? ( n n -- bool ) {: t1:n t2:n :}
   t1 PAY t2 PAY = IF RES-TRUE EXIT THEN
   UNIFY-WIDEN? CUR-STRICT @ 0= and IF t1 PAY t2 PAY INT-WIDENS? EXIT THEN
   t1 PAY CC-N = t2 PAY INT-FAM? and IF RES-TRUE EXIT THEN
   t2 PAY CC-N = t1 PAY INT-FAM? and IF RES-TRUE EXIT THEN
   RES-FALSE ;

: ATOM-OK? ( n n -- bool ) {: t1:n t2:n :}
   t1 ATOM>K t2 ATOM>K <> IF RES-FALSE EXIT THEN
   t1 ATOM>K 0 < IF RES-FALSE EXIT THEN
   t1 ATOM>K 0 = 0= IF RES-TRUE EXIT THEN
   t1 ATOM>A t1 ATOM>U t2 ATOM>A t2 ATOM>U CORE-STR= ;

: PARAM-FAM-OK? ( n n -- bool ) {: t1:n t2:n :}
   t1 PARAM>FAM t2 PARAM>FAM = ;   \ identity by resolved family-id, not folded spelling

: PARAM-PAIR-ARGS ( n n -- ) {: t1:n t2:n :}
   t1 PARAM>ARGC t2 PARAM>ARGC <> IF RES-FALSE UOK ! EXIT THEN
   t1 t2 PARAM-FAM-OK? 0= IF RES-FALSE UOK ! EXIT THEN
   0 PARAM-I !
   BEGIN PARAM-I @ t1 PARAM>ARGC < WHILE
      t1 PARAM-I @ PARAM>ARG  t2 PARAM-I @ PARAM>ARG  PAIR
      PARAM-I @ 1 + PARAM-I !
   REPEAT ;

: U-ROW R-RES swap R-RES swap 2dup = IF 2drop ELSE
   over ISROW IF 2dup ROW-OCC? IF 2drop RES-FALSE UOK ! ELSE swap PAY RV! THEN ELSE
   dup ISROW IF 2dup swap ROW-OCC? IF 2drop RES-FALSE UOK ! ELSE PAY RV! THEN ELSE
   2dup P>TYPE swap P>TYPE swap PAIR P>REST swap P>REST swap PAIR THEN THEN THEN ;

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

\ --- item 12 (docs/type-families.md §17): a logical layout value is still ONE
\ physical T-PARAM cell at this stage (item 7 kept it one cell; no LAYOUT-PUSH-
\ FIELDS expansion, no published constructors, so a wider-than-one-cell layout
\ value is not even constructible at runtime). A whole-bundle transport op
\ (dup/drop/swap/over/nip/rot/-rot/tuck/2dup/2drop/2swap/2over, >r/r>/r@ and
\ friends, and locals capture) moves the value as one logical unit, so its fresh
\ transport var may bind the layout cell. LAYOUT-XPORT is set by DO-TOK1/LOC-BIND
\ only while checking such an op. Every OTHER touch (value-inspecting prims,
\ ?dup, control predicates, higher-order apply, con/ptr/atom pairings) still
\ fails closed exactly as in item 7.
variable LAYOUT-XPORT
: LAYOUT-XPORT-ALLOW? ( n n -- bool ) {: a:n b:n :}
   LAYOUT-XPORT @ 0= IF RES-FALSE EXIT THEN     \ only inside a whole-bundle transport op
   a LAYOUT-PARAM? IF b ISVAR EXIT THEN         \ var <-> layout-param bind: absorb the bundle
   b LAYOUT-PARAM? IF a ISVAR EXIT THEN
   RES-FALSE ;                                  \ con/ptr/atom vs layout is never a bundle move
: LAYOUT-BLOCK? ( n n -- bool ) {: a:n b:n :}   \ a layout pairing this op may NOT form
   a b LAYOUT-EITHER? 0= IF RES-FALSE EXIT THEN
   a b LAYOUT-XPORT-ALLOW? 0= ;

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
     2dup ATOM-OK? IF 2drop ELSE 2drop RES-FALSE UOK ! THEN ELSE
   2dup FIELD-PAIR? IF 2drop ELSE
   2dup FIELD-COERCE? IF 2drop ELSE
   over TAG T-PARAM =  over TAG T-PARAM =  and IF
     2dup PARAM-PAIR-ARGS 2drop ELSE
   2dup LAYOUT-BLOCK? IF 2drop RES-FALSE UOK ! ELSE   \ item 12: only a whole-bundle transport op may bind a layout cell
   over ISVAR IF
     over PAY over TY-OCC? IF 2drop RES-FALSE UOK ! ELSE swap PAY TV! THEN ELSE
   dup ISVAR IF
     dup PAY  rot  tuck TY-OCC? IF 2drop RES-FALSE UOK ! ELSE swap PAY TV! THEN ELSE
   over TAG T-CON =  over TAG T-CON =  and IF
     2dup CON-OK? IF 2drop ELSE 2drop RES-FALSE UOK ! THEN
   ELSE 2drop RES-FALSE UOK ! THEN THEN THEN THEN THEN THEN THEN THEN THEN THEN THEN ;

: UNIFY ( n n -- bool )   \ worklist-driven; rows and types interleave
   0 USP !  RES-TRUE UOK !  0 CUR-STRICT !  PAIR
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
     1 +
   REPEAT drop ;
variable OK   variable DCUR   variable UNCK   variable BROW
variable RCUR   variable RBROW
variable THDROW  variable THRROW  variable THSET
variable XROW  variable XRROW  variable XSET  variable DEADP
variable DEADERR  variable DEADTA  variable DEADTU

: NEW ( -- )
   -1 OK ! 0 UNCK ! 0 SPN ! 0 USP ! TV-RESET 0 FV ! 0 QEN ! 0 PTRN !
   0 LAYOUT-XPORT !
   TRAIL-RESET   0 TRIAL-DEPTH !   LIN-TAINT-RESET
   RIGID-RESET
   0 ATOMN ! 0 PARAMN ! 0 PARAM-SCR-N ! 0 PARG-N !
   FRESH MK-ROW dup BROW ! DCUR !
   FRESH MK-ROW dup RBROW ! RCUR ! ;
variable WAS   variable DEXP   variable DACT   variable FAILSET
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
: LIN-TYPE-COUNT* ( n -- n ) {: t:n :}
   t T-RES TAG case
      T-CON of t LIN-CON? IF 1 ELSE 0 THEN endof
      T-PTR of 0 endof
      T-QUOT of 0 endof
      T-ATOM of 0 endof
      T-PARAM of
         t FIELD-PARAM? IF t T-RES FIELD-INNER TWALK-DEEPER RECURSE TWALK-SHALLOWER ELSE 0 THEN
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

: CHECKER-STEP {: din dout :}
   din dout LIN-EXPLICIT? LINEXP !
   LINEXP @ 0= IF LIN-SNAPSHOT THEN
   DCUR @ WAS !
   DCUR @ din UNIFY-IN
   dup 0=  FAILSET @ 0=  and  OK @ and  IF din DEXP !  WAS @ DACT !  -1 FAILSET ! THEN
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
     \ unknown xt: bind it to a RETURN-PURE quot over the current state (a
     \ return-impure literal quot then fails to unify at the bind — sound).
     FRESH MK-ROW QD2 !
     DCUR @ QD2 @ RCUR @ RCUR @ MK-QUOT QR2 !
     QTT @ PAY QR2 @ TY-OCC? IF 0 OK ! ELSE
       QR2 @ QTT @ PAY TV!
      QD2 @ DCUR !
     THEN
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
     DCUR @ DCUR @ RCUR @ RCUR @ MK-QUOT QR2 !
     QTT @ PAY QR2 @ TY-OCC? IF 0 OK ! ELSE
       QR2 @ QTT @ PAY TV!
     THEN
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

: VREC-I-AK ( n -- n ) {: k:n :}
   k 0 >= IF k EXIT THEN
   k VREC-I-AK-IDX {: idx:n :}
   idx VRI-AK-ENSURE
   idx cells VRI-AK + dup @ UNBOUND = IF
      RIGID-FRESH over !
   THEN @ ;

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
variable UNSAFE
variable LOCALBAD
variable LINLOCBAD           \ a linear-counting value was bound into a {: :} local
variable UNDEFERR
variable QUALBAD
variable QDUPBAD             \ ?dup applied to a layout value (width-breaking; item 12)

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

\ NB: avoid a 2nd {: :} group here — `{: c :} … {: i :}` mis-reads the slot in the
\ standalone, collapsing every var to one. Compute the slot address on the stack.
: VAR-OF ( n -- n ) {: c:n :}
   c 97 - cells NMAP +
   dup @ UNBOUND = IF FRESH over ! THEN
   @ MK-VAR ;

\ NB: declare locals at word top, never inside IF/loop (corrupts the locals frame).
\ concrete width types get distinct con codes; n(1)/f(1) stay the GENERIC int
\ (the prim DB and the toolchain's own body use n), and the unifier lets n
\ subsume any int-family code (so '( i64 -- i64 )' over an n-typed prim still
\ checks). r(3)=float. Table-driven to keep the body small (inline-safe).
: CON-OF {: a u :}                      \ multi-char name -> con code, or 0
   a u CT-FIND ;
: SGBAD-CLEAR ( -- )
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
: SGBAD-ARITY! ( ptr u8 n -- )      \ family applied to the wrong number of args
   SGBAD-ARITY-KIND SGBAD-SET ;
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
   a u s" align-" SIG-PREFIX? ;
: FRESH-ATOM-TOK? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" fresh-extent-" SIG-PREFIX? IF RES-TRUE EXIT THEN
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
: CT-ADD-NOMINAL ( ptr u8 n -- ) {: a:ptr u:n :}
   a u TYPE-RESERVED? IF s" checker: bad or duplicate signature type" 70 die THEN
   a u CTN @ CT-ROLE 64 CS-NONE CT-SET ;

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

: LOCAL-TYPE ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u s" ptr" CORE-STR= IF FRESH MK-VAR MK-PTR ELSE a u TOK-TYPE THEN ;

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
\ argument (SC-QUOT). It needs PSTACK, defined below SIG-TYPE, so it is reached
\ through a friend xt installed just after PSTACK (same late-binding shape as
\ TFAM-RESOLVE-XT). 0 = not yet installed (never reached before install).
variable SIG-QUOT-XT   0 SIG-QUOT-XT !

\ SIG-END-PARAM ( base a u fam -- t ) : close a parsed family application. Reject
\ (family-specific arity diagnostic) when the arg count differs from the family's
\ declared arity, then build the T-PARAM (MK-PARAM rewinds scratch to `base`).
: SIG-END-PARAM {: base:n a:ptr u:n fam:n :}
   PARAM-SCR-N @ base - fam TFAM-ARITY* <> IF a u SGBAD-ARITY! THEN
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
   a u SIG-FAM? IF {: fam:n :}                        \ ( id ) resolved family; build application
      NEXT-SIG-TOK 2dup s" <" CORE-STR= IF
         2drop PARAM-SCR-N @ {: base:n :}
         BEGIN
            NEXT-SIG-TOK 2dup s" >" CORE-STR= IF 2drop base a u fam SIG-END-PARAM EXIT THEN
            2dup DELIM? IF SGBAD-SYNTAX! base a u fam MK-PARAM EXIT THEN
            2dup s" [" CORE-STR= IF 2drop SIG-QUOT-XT @ execute ELSE RECURSE THEN  \ quotation arg (SC-QUOT) or nested type
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
\ §11, PLAN item 7). Every ordinary type and cell family pushes one logical cell
\ (== MK-PUSH). A sum/enum/product layout family ALSO stays ONE logical T-PARAM
\ cell here (reject-only): a layout value is not expanded to hidden physical
\ fields until item 12's width-aware lowering can preserve whole bundles across
\ generic stack ops. Until then an ordinary one-cell primitive that touches a
\ layout cell fails closed in U-TYPE (LAYOUT-EITHER?).
: PUSH-LOGICAL ( n n -- n ) MK-PUSH ;

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
' SIG-PARSE-QUOT SIG-QUOT-XT !

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

BEGIN-STRUCTURE SYM-REC
   PTR-FIELD: SYM.PKG-A
   CELL +FIELD SYM.PKG-U
   PTR-FIELD: SYM.NAME-A
   CELL +FIELD SYM.NAME-U
   CELL +FIELD SYM.VIS
END-STRUCTURE

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
: HIDX-RESET
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

BEGIN-STRUCTURE EFF-REC
   CELL +FIELD ER.NEXT
   CELL +FIELD ER.ACTIVE
   CELL +FIELD ER.DIN
   CELL +FIELD ER.DOUT
   CELL +FIELD ER.RIN
   CELL +FIELD ER.ROUT
   CELL +FIELD ER.HASR
   CELL +FIELD ER.TVN
   CELL +FIELD ER.RVN
   CELL +FIELD ER.SYM
END-STRUCTURE

BEGIN-STRUCTURE EFF-NODE
   CELL +FIELD EN.TAG
   CELL +FIELD EN.A
   CELL +FIELD EN.B
   CELL +FIELD EN.C
   CELL +FIELD EN.D
   CELL +FIELD EN.E
   CELL +FIELD EN.F
   CELL +FIELD EN.G
   CELL +FIELD EN.H
END-STRUCTURE

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
   0 p ER.HASR !  0 p ER.TVN !   0 p ER.RVN !
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

: E-BUILD-EFFECT ( n n n n bool -- n ) {: din:n dout:n rin:n rout:n hasr:bool :}
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
   r@ E-PTR E-REC-FINISH
   r> ;

\ E-ADD-EFFECT/E-ADD-DELETED are the only creators of USER records (prims go
\ through PE-CLOSE/E-BUILD-EFFECT directly), so they own the in-place cache
\ update: the record just built IS the current effect for its symbol. The
\ cache stores offset+1 because offset 0 is legal after USIGS-RESET.
: E-ADD-EFFECT ( n n n n bool -- )
   E-BUILD-EFFECT {: off:n :}
   CHECKER-REC-SYM @ 0 <> HIDX-VALID @ and IF
      off 1 + CHECKER-REC-SYM @ HIDX-EFF!
      UEND @ HIDX-EFF-DEP+
   THEN ;

: E-ADD-DELETED ( -- )
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
variable BADSIG-XT   0 BADSIG-XT !      \ ( sig-a sig-u n name-a name-u n -- )

: MULTI-ERR? ( -- bool ) MULTI-ERR @ 0 <> ;

: USIG-BAD-FOREIGN? ( ptr u8 n -- bool )   \ not the definition CHECK just handled
   NMA @ NMU @ CORE-STR= 0= ;

: USIG-ADD-BAD ( ptr u8 n ptr u8 n -- ) {: sa:ptr su:n na:ptr nu:n :}
   MULTI-ERR? 0= IF
      2 sa su write drop                 \ name the offending stored sig text
      s" : checker: bad stored signature" 76 die
   THEN
   na nu USIG-BAD-FOREIGN? 0= IF EXIT THEN
   1 MULTI-ERR-N +!
   sa su na nu BADSIG-XT @ dup 0= IF drop 2drop 2drop ELSE execute THEN ;

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

: E-I-AK ( n -- n ) {: k:n :}
   k 0 >= if k exit then
   k E-I-AK-IDX dup EI-AK-CAP >= if s" checker: fresh atom inst table full" 76 die then
   cells EI-AK + dup @ UNBOUND = if
      RIGID-FRESH over !
   then @ ;

: E-I-STR ( ptr a -- ptr u8 n )
   dup EN.A @ E-PTR swap EN.B @ ;

: E-INST ( n -- n ) {: off:n :}
   off 0= if 0 exit then
   off E-PTR >r
   r@ EN.TAG @ case
      EN-CON of r@ EN.A @ MK-CON r> drop endof
      EN-VAR of r@ EN.A @ E-I-TV r> drop endof
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

256 constant PE-CAP
1 constant PE-ACTIVE

BEGIN-STRUCTURE PE-REC
   CELL +FIELD PE.SYM
   CELL +FIELD PE.EFF
   CELL +FIELD PE.FLAGS
END-STRUCTURE

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

: PRIM-CHECK-CAP ( -- )
   #PE @ PE-CAP >= IF s" checker: prim table full" 76 die THEN ;

: PRIM-ADD ( n n n -- ) {: sym:n eff:n flags:n :}
   PRIM-CHECK-CAP
   sym #PE @ PE-ROW PE.SYM !
   eff #PE @ PE-ROW PE.EFF !
   flags #PE @ PE-ROW PE.FLAGS !
   #PE @ 1 + #PE ! ;

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

: PE-CLOSE ( -- )
   PE-NA@ PE-NU @ PE-SYM-OF PE-SYM-ID !
   PE-SYM-ID @ CHECKER-REC-SYM !
   PE-DIN @ PE-DOUT @ PE-RIN @ PE-ROUT @ PE-HASR @
   E-BUILD-EFFECT PE-EFF-ID !
   PE-SYM-ID @ PE-EFF-ID @ PE-ACTIVE PRIM-ADD ;

: PRIM; ( -- )
   PE-CLOSE ;

: PE-IN ( n -- )
   PE-DIN @ MK-PUSH PE-DIN ! ;

: PE-OUT ( n -- )
   PE-DOUT @ MK-PUSH PE-DOUT ! ;

: PE-A ( -- n ) $61 VAR-OF ;
: PE-B ( -- n ) $62 VAR-OF ;
: PE-C ( -- n ) $63 VAR-OF ;
: PE-D ( -- n ) $64 VAR-OF ;
: PE-N ( -- n ) CC-N MK-CON ;
: PE-F ( -- n ) CC-BOOL MK-CON ;
: PE-R ( -- n ) CC-R MK-CON ;
: PE-U8 ( -- n ) CC-U8 MK-CON ;
: PE-PTR ( n -- n ) MK-PTR ;
: PE-PTR-A ( -- n ) PE-A PE-PTR ;
: PE-PTR-B ( -- n ) PE-B PE-PTR ;
: PE-PTR-C ( -- n ) PE-C PE-PTR ;
: PE-PTR-N ( -- n ) PE-N PE-PTR ;
: PE-PTR-U8 ( -- n ) PE-U8 PE-PTR ;
: PE-PTR-PTR-B ( -- n ) PE-B PE-PTR PE-PTR ;

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
PRIM: here         PE-PTR-A PE-OUT PRIM;
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
PRIM: snap-rebase PE-N PE-IN PE-N PE-IN PE-N PE-IN PE-N PE-IN PE-N PE-IN PE-N PE-IN PRIM;
PRIM: write         PE-N PE-IN PE-PTR-U8 PE-IN PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM: close         PE-N PE-IN PRIM;
PRIM: epoch-seconds PE-N PE-OUT PRIM;
PRIM: mono-ns       PE-N PE-OUT PRIM;
PRIM: prof-on       PE-N PE-IN PRIM;
PRIM: prof-report   PRIM;

PRIM: rbase          PE-N PE-OUT PRIM;
PRIM: cp@            PE-N PE-OUT PRIM;
PRIM: cp!            PE-N PE-IN PRIM;
PRIM: dbase@         PE-N PE-OUT PRIM;
PRIM: ndict@         PE-N PE-OUT PRIM;
PRIM: ndict!         PE-N PE-IN PRIM;
PRIM: data-base      PE-PTR-A PE-OUT PRIM;
PRIM: wordlist       PE-N PE-OUT PRIM;
PRIM: get-current    PE-N PE-OUT PRIM;
PRIM: set-current    PE-N PE-IN PRIM;
PRIM: search-wl      PE-PTR-U8 PE-IN PE-N PE-IN PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM: parse-name     PE-PTR-U8 PE-OUT PE-N PE-OUT PRIM;
PRIM: CORE-STR=      PE-PTR-U8 PE-IN PE-N PE-IN PE-PTR-U8 PE-IN PE-N PE-IN  PE-F PE-OUT PRIM;
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
PRIM: CHECKER-SCOPE-DONE PRIM;
PRIM: CHECK-CANDIDATE! PE-PTR-U8 PE-IN PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM: CHECKER-CANDIDATE-SCOPE-START PRIM;
PRIM: CHECKER-CANDIDATE-SCOPE-DONE PRIM;
PRIM: CHECKER-USIGS-TRUNCATE-FROM PE-PTR-U8 PE-IN PE-N PE-IN PRIM;
PRIM: CHECKER-UNDEFINE PE-PTR-U8 PE-IN PE-N PE-IN PRIM;
PRIM: CHECKER-DEFTYPE PE-PTR-U8 PE-IN PE-N PE-IN PRIM;
PRIM: CHECKER-DEFLINEAR PE-PTR-U8 PE-IN PE-N PE-IN PRIM;
PRIM: CHECKER-DEFRECORD PE-PTR-U8 PE-IN PE-N PE-IN PE-PTR-U8 PE-IN PE-N PE-IN PRIM;
PRIM: CHECKER-DEFFAMILY PE-PTR-U8 PE-IN PE-N PE-IN PE-PTR-U8 PE-IN PE-N PE-IN PRIM;
PRIM: CHECKER-DEFSUM PE-PTR-U8 PE-IN PE-N PE-IN PE-PTR-U8 PE-IN PE-N PE-IN PRIM;
PRIM: TFAM-N@ PE-N PE-OUT PRIM;
PRIM: SUMV-N@ PE-N PE-OUT PRIM;
PRIM: TF-STR-U@ PE-N PE-OUT PRIM;
PRIM: TF-PK-N@ PE-N PE-OUT PRIM;
PRIM: SCHEMA-N@ PE-N PE-OUT PRIM;
PRIM: SCHEMA-ROOT-N@ PE-N PE-OUT PRIM;
PRIM: CHECKER-DEFER PE-PTR-U8 PE-IN PE-N PE-IN PRIM;
PRIM: CHECKER-PACKAGE PE-PTR-U8 PE-IN PE-N PE-IN PRIM;
PRIM: CHECKER-PUBLIC PRIM;
PRIM: CHECKER-PRIVATE PRIM;
PRIM: CHECKER-END-PACKAGE PRIM;
PRIM: ffi-call       PE-PTR-A PE-IN PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM: ffi-call-n     PE-PTR-A PE-IN PE-N PE-IN PE-N PE-IN  PE-N PE-OUT PRIM;
PRIM: ffi-call-abi   PE-PTR-A PE-IN PE-PTR-B PE-IN PE-PTR-C PE-IN PE-N PE-IN PE-N PE-IN
                     PE-N PE-OUT PRIM;
PRIM: ffi-call-abi-r PE-PTR-A PE-IN PE-PTR-B PE-IN PE-PTR-C PE-IN PE-N PE-IN PE-N PE-IN
                     PE-R PE-OUT PRIM;

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

BEGIN-STRUCTURE DFER-REC
   CELL +FIELD DFER.SYM
   CELL +FIELD DFER.FLAG
END-STRUCTURE

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

: CHECKER-PACKAGE ( ptr u8 n -- )
   CHECKER-PACKAGE-COPY
   CHECKER-PACKAGE-PRIVATE CHECKER-PACKAGE-MODE ! ;

: CHECKER-PUBLIC ( -- )
   CHECKER-PACKAGE-ACTIVE? IF CHECKER-PACKAGE-PUBLIC CHECKER-PACKAGE-MODE ! THEN ;

: CHECKER-PRIVATE ( -- )
   CHECKER-PACKAGE-ACTIVE? IF CHECKER-PACKAGE-PRIVATE CHECKER-PACKAGE-MODE ! THEN ;

: CHECKER-END-PACKAGE ( -- )
   CHECKER-PACKAGE-NONE CHECKER-PACKAGE-MODE !
   0 CHECKER-PACKAGE-U ! ;

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
   a u CHECKER-GLOBAL-SYM? ;

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

: CHECKER-USIGS-TRUNCATE-FROM ( ptr u8 n -- ) {: a:ptr u:n :}
   a u CHECKER-FIND-ACTIVE-SYM USIG-FIND-OFF-SYM 0= IF
      s" checker: missing signature truncation mark" 76 die
   THEN
   UEND !
   UTERM! ;

: CHECKER-FIND-ACTIVE-SIG ( ptr u8 n -- ) {: a:ptr u:n :}
   FEP-CLEAR
   a u CHECKER-FIND-ACTIVE-SYM CHECKER-FIND-USIG-SYM drop ;

: FIND-SIG ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u CHECKER-FIND-ACTIVE-SIG
   FEP-HIT? IF RES-TRUE EXIT THEN
   a u CHECKER-FIND-ACTIVE-SYM PRIM-FIRST-SYM
   dup 0 <> IF E-PTR FEP-SET RES-TRUE ELSE drop RES-FALSE THEN ;

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

: CHECKER-DUP-DEFINITION ( -- )
   $4E throw ;

: CHECKER-USIG-CERT-ADD ( ptr u8 n ptr u8 n -- ) {: sa:ptr su:n na:ptr nu:n :}
   na nu CHECKER-REC-NAME!
   CHECKER-CERT-DUP? IF CHECKER-DUP-DEFINITION THEN
   sa su CHECKER-REC-A@ CHECKER-REC-U@ USIG-ADD ;

: CHECKER-USIG-CERT-CURRENT ( ptr u8 n -- ) {: na:ptr nu:n :}
   na nu CHECKER-REC-NAME!
   CHECKER-CERT-DUP? IF CHECKER-DUP-DEFINITION THEN
   BROW @ DCUR @ 0 0 RES-FALSE E-ADD-EFFECT ;

\ Control-effect flags are append-only and later-wins so redefinitions can clear
\ stale metadata. CTL-DEAD means a call has no normal continuation. CTL-THROW
\ means a call may reach a catchable throw edge.
1 constant CTL-DEAD
2 constant CTL-THROW
$10000 constant NORET-INIT-CAP

BEGIN-STRUCTURE NORET-ENTRY
   CELL +FIELD NORET.SYM
   CELL +FIELD NORET.FLAG
END-STRUCTURE

create NORET-BOOT NORET-INIT-CAP allot
variable NORET-P   variable NORET-CAP-U   variable NORET-END
NORET-BOOT NORET-P !   NORET-INIT-CAP NORET-CAP-U !   0 NORET-END !   0 NORET-BOOT !
variable NORET-POS   variable NORET-FLAG
variable NORET-GROW-CAP   variable NORET-GROW-NEXT

: NORETS ( -- ptr u8 ) NORET-P @ ;

: NORET-CELL ( n -- ptr a ) {: off:n :}
   off 7 and 0 <> IF s" checker: unaligned no-return cell" 76 die THEN
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
variable REG-EXT-PERSIST-XT   0 REG-EXT-PERSIST-XT !

: CHECKER-SNAPSHOT-PREPARE ( -- )
   TOKBUF-RESET
   HIDX-RESET
   TV-SNAP-RESET
   DECOUPLED-ARENA-SNAP-RESET
   CT-SNAPSHOT-PERSIST
   VREC-SNAPSHOT-PERSIST
   SYM-SNAPSHOT-PERSIST
   USIGS-SNAPSHOT-PERSIST
   NORET-SNAPSHOT-PERSIST
   REG-EXT-PERSIST-XT @ dup 0= if drop else execute then ;

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
: NORET-ADD {: a:ptr u:n flag:n :}
   HIDX-CTL-SYNC
   NORET-END @ NORET-ENTRY + CELL + NORET-ENSURE
   a u CHECKER-RECORD-SYM {: sym:n :}
   sym NORET-REC NORET.SYM !
   flag NORET-REC NORET.FLAG !
   NORET-END @ NORET-ENTRY + NORET-END !
   NORET-TERM
   sym 0 <> HIDX-VALID @ and IF
      flag sym HIDX-CTL!
      NORET-END @ HIDX-CTL-DEP+
   THEN ;

: CHECKER-UNDEFINE ( ptr u8 n -- ) {: a:ptr u:n :}
   a u CHECKER-RECORD-NAME {: name:ptr nameu:n :}
   name nameu USIG-DELETE
   name nameu DFER-DELETE
   name nameu 0 NORET-ADD ;

: CHECKER-DEFTYPE ( ptr u8 n -- )
   CT-ADD-NOMINAL ;

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
\ Trial save/restore: a prim-overload trial saves the scalar cursors below and the
\ trail height (SV-TRAIL); var bindings are undone via the unification trail (top).
variable SV-FV    variable SV-SPN   variable SV-QEN   variable SV-PTRN
variable SV-OK    variable SV-DCUR  variable SV-RCUR  variable SV-UNCK
variable SV-FSET  variable SV-DEXP  variable SV-DACT  variable SV-SGBAD
variable SV-SGBAD-A  variable SV-SGBAD-U  variable SV-SGBAD-KIND
variable SV-SGSEEN  variable SV-SGHASR  variable SV-SGIN  variable SV-SGOUT
variable SV-SGRIN   variable SV-SGROUT
variable SV-THDROW  variable SV-THRROW  variable SV-THSET
variable SV-TRAIL

: TRIAL-SAVE
   FV @ SV-FV !  TRAIL-N @ SV-TRAIL !     \ trail height is the per-TRY-EFF mark
   SPN @ SV-SPN !  QEN @ SV-QEN !  PTRN @ SV-PTRN !
   OK @ SV-OK !  DCUR @ SV-DCUR !  RCUR @ SV-RCUR !  UNCK @ SV-UNCK !
   FAILSET @ SV-FSET !  DEXP @ SV-DEXP !  DACT @ SV-DACT !
   SGBAD @ SV-SGBAD !  SGBAD-A @ SV-SGBAD-A !
   SGBAD-U @ SV-SGBAD-U !  SGBAD-KIND @ SV-SGBAD-KIND !
   SGSEEN @ SV-SGSEEN !  SGHASR @ SV-SGHASR !
   SGIN @ SV-SGIN !  SGOUT @ SV-SGOUT !  SGRIN @ SV-SGRIN !  SGROUT @ SV-SGROUT !
   THDROW @ SV-THDROW !  THRROW @ SV-THRROW !  THSET @ SV-THSET ! ;

: TRIAL-CLEAR-NEW
   SV-FV @ BEGIN dup FV @ < WHILE
      UNBOUND over cells TVT + !  UNBOUND over cells RVT + !
      1 +
   REPEAT drop ;

: TRIAL-REST-SG
   SV-SGBAD @ SGBAD !  SV-SGBAD-A @ SGBAD-A !
   SV-SGBAD-U @ SGBAD-U !  SV-SGBAD-KIND @ SGBAD-KIND !
   SV-SGSEEN @ SGSEEN !  SV-SGHASR @ SGHASR !
   SV-SGIN @ SGIN !  SV-SGOUT @ SGOUT !  SV-SGRIN @ SGRIN !  SV-SGROUT @ SGROUT ! ;

: TRIAL-REST
   SV-TRAIL @ TRAIL-UNWIND       \ undo speculative binds in both pools
   TRIAL-CLEAR-NEW               \ new-var backstop (cells never bound via TV!/RV!)
   SV-FV @ FV !
   SV-SPN @ SPN !  SV-QEN @ QEN !  SV-PTRN @ PTRN !
   SV-OK @ OK !  SV-DCUR @ DCUR !  SV-RCUR @ RCUR !  SV-UNCK @ UNCK !
   SV-FSET @ FAILSET !  SV-DEXP @ DEXP !  SV-DACT @ DACT !
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
            TSEEN @ 0= IF PE-I @ PE-EFF@ TFA ! THEN
            -1 TSEEN !
            PE-I @ PE-EFF@ E-PTR TRY-EFF IF -1 TSOK ! THEN
         THEN
      THEN
      PE-I @ 1 + PE-I !
   repeat
   TSOK @ 0 <> ;
variable FLD  variable FLI  variable FLO  variable FLC

: FLODIG? ( ptr u8 n -- bool ) {: a:ptr u:n :}     \ -?d+.d+ (one interior dot) -> float literal
   0 FLD !  0 FLI !  -1 FLO !
   u 3 < IF 0 FLO ! THEN
   a c@ 45 = IF 1 FLI ! THEN
   FLI @ BEGIN dup u < WHILE
     a over + c@ FLC !
     FLC @ 46 = IF FLD @ 0 > IF 0 FLO ! THEN FLD @ 1 + FLD !
     ELSE FLC @ 47 > FLC @ 58 < and 0= IF 0 FLO ! THEN THEN
     1 + REPEAT drop
   FLD @ 1 = FLO @ 0 <> and
   u 0 > IF a u 1 - + c@ 46 = IF drop RES-FALSE THEN THEN
   a FLI @ + c@ 46 = IF drop RES-FALSE THEN ;

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

: CELL-FETCH-TOK ( -- )
   DCUR @ ROW-TOP-BYTE-PTR? {: bad :}
   STEP-FETCH
   bad IF 0 OK ! THEN ;

: CELL-STORE-TOK ( -- )
   DCUR @ ROW-TOP-BYTE-PTR? {: bad :}
   STEP-STORE
   bad IF 0 OK ! THEN ;

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
   FEP-HIT? IF FEP @ EFF-APPLY ELSE
   CURSYM @ TRY-PRIMS IF EXIT THEN
   TSEEN @ 0 <> IF TFA @ E-PTR EFF-APPLY ELSE
   CHECKER-QBAD-TOK @ 0 <> IF -1 QUALBAD ! THEN
   -1 UNDEFERR ! -1 UNCK ! THEN THEN ;

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
variable #LOC  variable LMODE  variable LGRP  variable LROW  variable LCH  variable LI  variable LRF
variable LOCSHOWXT  0 LOCSHOWXT !
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
   a u LOC-SUFFIX$ LOCAL-TYPE
   idx cells LOCTV + @ UNIFY OK @ and OK ! ;

: LOC-SHOW-ONE ( n -- ) {: idx:n :}
   LOCSHOWXT @ 0= if exit then
   idx cells LOCSHOW + @ 0= if exit then
   LOCNB idx LOC-NAME-W * +  idx cells LOCLN + @  idx cells LOCTV + @
   LOCSHOWXT @ execute ;

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
: LOC-ADD {: a u :}
   a u LCOLON
   #LOC @ LOC-CAP 1 - >  LCO @ LOC-NAME-W >  or IF
     0 OK !  -1 FAILSET !  -1 LOCALBAD !
   ELSE
     #LOC @ LOC-SHOW-OFF!
     a  LOCNB #LOC @ LOC-NAME-W * +  LCO @ CCOPY
     LCO @ #LOC @ cells LOCLN + !
     FRESH MK-VAR #LOC @ cells LOCTV + !
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

: LOC-BIND
   FRESH dup LROW !  MK-ROW LCH !
   LGRP @ BEGIN dup #LOC @ < WHILE
     dup cells LOCTV + @  LCH @ MK-PUSH LCH !
     1 + REPEAT drop
   1 LAYOUT-XPORT !                    \ capturing a local moves the value as one bundle
   LCH @  LROW @ MK-ROW  CHECKER-STEP
   0 LAYOUT-XPORT !
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

: LOC-REF? {: a u :}
   0 LRF !  #LOC @ LI !
   BEGIN LI @ 0 >  LRF @ 0=  and WHILE
     LI @ 1 - LI !
     a u  LOCNB LI @ LOC-NAME-W * +  LI @ cells LOCLN + @  CORE-STR= IF
       QDEPTH @ 0 > IF
          LOC-REJECT
       ELSE
          LI @ cells LOCTV + @  dup LIN-LOCAL-REF-TAINT  DCUR @ MK-PUSH DCUR !
       THEN
       -1 LRF ! THEN
   REPEAT  LRF @ ;
\ --- control flow: branch states saved on a CF stack and unified at joins.
\ Both rows are snapshot: A/B = data, RA/RB = return (PLAN: net growth on
\ either row at a back edge is a row-occurs failure).
\ kinds: 1 if  2 if+else  3 begin  4 begin+while  5 do  6 quotation
\ exit-accumulator save fields: a [: ;] quotation is a nested scope, so its
\ early returns must NOT leak into the enclosing word's accumulator.
BEGIN-STRUCTURE CFS-REC
   CELL +FIELD CF.KND
   CELL +FIELD CF.SA
   CELL +FIELD CF.SB
   CELL +FIELD CF.RA
   CELL +FIELD CF.RB
   CELL +FIELD CF.DED
   CELL +FIELD CF.LN
   CELL +FIELD CF.XRO
   CELL +FIELD CF.XRR
   CELL +FIELD CF.XST
   CELL +FIELD CF.XDP
   CELL +FIELD CF.TXD
   CELL +FIELD CF.TXR
   CELL +FIELD CF.TXS
END-STRUCTURE

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

: CF-FAIL ( -- )
   0 OK !
   -1 FAILSET ! ;

: SUNI {: s :}
   DCUR @ s UNIFY
   dup 0=  FAILSET @ 0=  and  OK @ and  IF s DEXP !  DCUR @ DACT !  -1 FAILSET ! THEN
   OK @ and OK ! ;

: SUNI-IN {: s:n :}
   DCUR @ s UNIFY-IN
   dup 0=  FAILSET @ 0=  and  OK @ and  IF s DEXP !  DCUR @ DACT !  -1 FAILSET ! THEN
   OK @ and OK ! ;

: SUNI-COERCE {: s:n :}
   DCUR @ s UNIFY-COERCE
   dup 0=  FAILSET @ 0=  and  OK @ and  IF s DEXP !  DCUR @ DACT !  -1 FAILSET ! THEN
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

: CF-IF  STEP-BOOL-IN  1 DCUR @ 0 RCUR @ 0 CF-PUSH ;   \ IF consumes a flag, not any value

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

: CF-TOK? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" [:" CORE-STR= IF CF-QUOT RES-TRUE EXIT THEN
   a u s" ;]" CORE-STR= IF CF-SEMIQ RES-TRUE EXIT THEN
   a u s" if" CORE-STR= IF CF-IF RES-TRUE EXIT THEN
   a u s" else" CORE-STR= IF CF-ELSE RES-TRUE EXIT THEN
   a u s" then" CORE-STR= IF CF-THEN RES-TRUE EXIT THEN
   a u s" case" CORE-STR= IF CF-CASE RES-TRUE EXIT THEN
   a u s" of" CORE-STR= IF CF-OF RES-TRUE EXIT THEN
   a u s" endof" CORE-STR= IF CF-ENDOF RES-TRUE EXIT THEN
   a u s" endcase" CORE-STR= IF CF-ENDCASE RES-TRUE EXIT THEN
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
\ recorder); RECXT (installed by render.f) records certified sigs by name.
variable TOK0  variable RECXT  0 RECXT !
variable DIAGXT  0 DIAGXT !              \ reject-diagnostic hook (render.f installs)
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
: CAP-FAIL ( ptr u8 n -- )
   FAILSET @ 0= IF
      {: a:ptr u:n :}
      u TOKBUF-ENSURE
      a FAILTK u CCOPY  u FAILTU !  TOKIX @ FAILIX !  FAIL-SPAN!
   ELSE
      2drop
   THEN ;
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

: UNSAFE-TOK? {: a u :}
   a u s" evaluate" CORE-STR= IF RES-TRUE EXIT THEN
   a u s" trust" CORE-STR= IF RES-TRUE EXIT THEN
   a u s" set-check" CORE-STR= IF RES-TRUE EXIT THEN
   a u s" postpone" CORE-STR= IF RES-TRUE EXIT THEN
   a u s" compile," CORE-STR= IF RES-TRUE EXIT THEN
   a u s" immediate" CORE-STR= IF RES-TRUE EXIT THEN
   a u s" [" CORE-STR= IF RES-TRUE EXIT THEN
   a u s" ]" CORE-STR= ;

: REJECT-UNSAFE ( -- )
   -1 UNSAFE !  0 OK !  -1 FAILSET ! ;

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

\ --- item 12 layout stack-op typing (docs/type-families.md §17) --------------
\ Whole-bundle transport tokens: their effect var may absorb a one-cell layout
\ value, because a logical layout value moves as one unit. ?dup is excluded on
\ purpose — it branches on the top (tag) cell, width-breaking for a sum whose
\ tag 0 is a valid variant.
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
   DCUR @ R-RES dup TAG S-PUSH = IF P>TYPE LAYOUT-PARAM? EXIT THEN drop RES-FALSE ;

: QDUP-STEP? ( ptr u8 n -- bool )  \ ?dup: reject on a layout value; scalar stays unmodeled
   s" ?dup" CORE-STR= 0= IF RES-FALSE EXIT THEN
   DCUR-TOP-LAYOUT? IF
      0 OK !  -1 FAILSET !  -1 QDUPBAD !         \ width-breaking touch of a layout value
   ELSE
      -1 UNDEFERR !  -1 UNCK !                   \ scalar ?dup unmodeled (pre-existing gap; dotted)
   THEN
   RES-TRUE ;

: DO-TOK1 {: a u :}
   a u TOKFOLD drop
   a u CAP-FAIL
   TKF TKFU @ LAYOUT-XPORT-TOK? LAYOUT-XPORT !    \ transport op? layout value moves whole
   TOK0 @ IF TKF NMB TKFU @ CCOPY  NMB NMA !  TKFU @ NMU !  0 TOK0 ! ELSE
   TKF TKFU @ LIVE-TOKEN? 0= IF -1 DEADERR ! 0 OK ! ELSE
   LMODE @ IF TKF TKFU @ LOC-TOK ELSE
   TKF TKFU @ s" {:" CORE-STR= IF LOC-BEGIN ELSE
   TKF TKFU @ UNSAFE-TOK? IF REJECT-UNSAFE ELSE
   TKF TKFU @ s" is" CORE-STR= IF IS-TOK ELSE
   OK @ IF TKF TKFU @ s" exit" CORE-STR= IF a u DEAD-OWNER! THEN THEN
   OK @ IF TKF TKFU @ s" leave" CORE-STR= IF a u DEAD-OWNER! THEN THEN
   OK @ IF TKF TKFU @ s" again" CORE-STR= IF a u DEAD-OWNER! THEN THEN
   TKF TKFU @ LOC-REF? 0= IF
   TKF TKFU @ CF-TOK? 0= IF
   TKF TKFU @ QDUP-STEP? 0= IF
   TKF TKFU @ RS-TOK? 0= IF
   TKF TKFU @ DO-TOK
   OK @ IF TKF TKFU @ THROW-CUR? IF THROW-EDGE THEN THEN
   OK @ IF TKF TKFU @ DEAD-CUR? IF a u DEAD-OWNER! -1 DEADP ! THEN THEN
   TKF TKFU @ ESCAPED-STRING-OPENER? IF SKIP-ESCAPED-STRING-PAYLOAD ELSE
   TKF TKFU @ NORMAL-STRING-OPENER? IF SKIP-STRING-PAYLOAD THEN THEN
   TKF TKFU @ PARSE-LIT? IF SKIP-PARSE-LIT-PAYLOAD THEN
   THEN THEN THEN THEN THEN THEN THEN THEN THEN THEN
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

: CHECK-RESET {: a u :}
   u TOKBUF-ENSURE
   a TBASE !  u TBLEN !  NEW
   0 TI !  1 TOK0 !  0 NMU !  0 #LOC !  0 LMODE !  0 #CFC !  0 QDEPTH !
   0 FAILSET !  0 DEXP !  0 DACT !  0 FAILTU !  0 SGSEEN !  0 SGHASR !
   0 SGIN !  0 SGOUT !  0 SGRIN !  0 SGROUT !  0 SGDBASE !  0 SGRBASE !
   0 SGA !  0 SGU !
   0 TOKIX !  0 FAILIX !  0 DVERD !
   0 FAILB !  0 FAILE !  0 XSET !  0 DEADP !  0 DEADERR !  0 DEADTA !  0 DEADTU !
   0 THDROW !  0 THRROW !  0 THSET !
   SGBAD-CLEAR  0 UNSAFE !  0 LOCALBAD !  0 LINLOCBAD !  0 UNDEFERR !  0 QUALBAD !  0 QDUPBAD !
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
   SGBAD @ UNSAFE @ or  LOCALBAD @ or  LINLOCBAD @ or  QDUPBAD @ or 0 <> IF 0 ELSE
   UNCK @ 0 <> IF 1 ELSE OK @ THEN THEN ;

: CHECK {: a u :}   \ ( a u -- -1=certified | 0=rejected | 1=uncheckable )
   a u CHECK-RESET
   CHECK-SCAN
   0 LAYOUT-XPORT !                  \ boundary unification is never in transport mode
   SIG-EFF-DROP
   CHECK-FOLD-EXITS
   CHECK-SIG? IF CHECK-NO-BORROW THEN
   CHECK-SIG? IF
      SGOUT @ SUNI-COERCE
      OK @ IF SGIN @ BROW !  SGOUT @ DCUR ! THEN    \ record the verified declared effect
   THEN                                        \ SUNI captures declared(exp)/inferred(act)
   LMODE @ 0 <>  #CFC @ 0 <>  or IF CF-FAIL THEN
   SGHASR @ 0= IF RCUR @ R-RES  RBROW @ R-RES  <> IF 0 OK ! THEN THEN   \ balance (no clause)
   CHECK-RET-SIG? IF
      RCUR @ SGROUT @ UNIFY-COERCE OK @ and OK !
      OK @ IF SGRIN @ RBROW !  SGROUT @ RCUR ! THEN
   THEN
   CHECK-VERDICT                                      \ malformed/unsafe rejects
   dup DVERD !
   dup 0 =  over 1 = JSON-DIAGS @ and  or
   dup MEO-ON @ and IF MEO-APPLY THEN     \ file-relative origin for this def's diagnostic
   DIAGXT @ 0 <> and IF DIAGXT @ execute THEN
   dup -1 = NMU @ 0 > and IF
      0 CTLNEW !
      DEADP @ XSET @ 0= and IF CTLNEW @ CTL-DEAD or CTLNEW ! THEN
      THSET @ IF CTLNEW @ CTL-THROW or CTLNEW ! THEN
      NMA @ NMU @ CTL-FLAGS CTLNEW @ <> IF
         NMA @ NMU @ CTLNEW @ NORET-ADD
      THEN
      CHECK-SIG? IF
         SGA @ SGU @  NMA @ NMU @  CHECKER-USIG-CERT-ADD
      ELSE
         RECXT @ 0 <> IF NMA @ NMU @ RECXT @ execute THEN
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
variable REG-EXT-RB-SAVE-XT      0 REG-EXT-RB-SAVE-XT !
variable REG-EXT-RB-RESTORE-XT   0 REG-EXT-RB-RESTORE-XT !

BEGIN-STRUCTURE RBF-REC
   CELL +FIELD RBF.UEND
   CELL +FIELD RBF.NEND
   CELL +FIELD RBF.SYMN
   CELL +FIELD RBF.SYMU
   CELL +FIELD RBF.CTN
   CELL +FIELD RBF.CTU
   CELL +FIELD RBF.LIN
   CELL +FIELD RBF.VRECN
   CELL +FIELD RBF.VRECF
   CELL +FIELD RBF.VRECND
   CELL +FIELD RBF.VNARGN
   CELL +FIELD RBF.VRECU
   CELL +FIELD RBF.CAND
   CELL +FIELD RBF.VSIG
   CELL +FIELD RBF.PKGMODE
   CELL +FIELD RBF.PKGU
   CELL +FIELD RBF.DFEREND
END-STRUCTURE

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
   0 RBF-DEPTH ! ;

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
   CHECKER-PACKAGE-NAME RBF-NAME-CUR CHECKER-PACKAGE-U @ USIGS-COPY
   RBF-DEPTH @ 1 + RBF-DEPTH !
   REG-EXT-RB-SAVE-XT @ dup 0= IF drop ELSE execute THEN ;

: RBF-POP ( -- )           \ restore every mark from the top frame, retiring index rows
   REG-EXT-RB-RESTORE-XT @ dup 0= IF drop ELSE execute THEN
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

: CHECKER-SCOPE-START ( -- )
   RBF-PUSH ;

: CHECKER-SCOPE-DONE ( -- )
   RBF-POP ;

: CHECK-CANDIDATE-START ( -- )
   RBF-PUSH
   -1 CHK-CAND !
   -1 VSIG ! ;

: CHECK-CANDIDATE-DONE ( n -- n )
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
: CHECK! {: a u :}  -1 VSIG !  a u CHECK  0 VSIG ! ;

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
   CHECK-NO-BORROW
   SGOUT @ SUNI-COERCE
   OK @ IF SGOUT @ DCUR ! THEN
   LMODE @ 0 <>  #CFC @ 0 <>  or IF CF-FAIL THEN
   SGHASR @ 0= IF RCUR @ R-RES  RBROW @ R-RES  <> IF 0 OK ! THEN THEN
   SGHASR @ IF RCUR @ SGROUT @ UNIFY-COERCE OK @ and OK ! THEN
   CHECK-VERDICT dup DVERD ! ;
