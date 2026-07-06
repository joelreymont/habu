\ opt.f - sound optimization passes over the PTX instruction-table IR (opt-ir.f).
\
\ Everything here is BIT-EXACT: no instruction is reordered, no operand value is
\ recomputed, no rounding mode is changed. Passes only (a) redirect a later read
\ to an earlier register that provably still holds the identical value, and (b)
\ drop a pure register-writing instruction once nothing reads its result. The
\ value pattern reaching every side-effecting op (st/red/atom/bar/ret) is
\ therefore preserved exactly on any target.
\
\ Value numbering runs per straight-line region. A REGION ends at any RESET line
\ (directive / label / `{` / `}` / branch / ret): register contents may differ
\ across a control merge, so value numbers are dropped there. An OPAQUE non-reset
\ line (load / store / barrier / atomic / mma / predicated) does NOT clobber
\ register VNs in general, but is conservatively assumed to redefine EVERY
\ register it names, so no equivalence flows through a value the opaque line may
\ have changed. This treats barriers / volatile / atomics as fences.
\
\ Passes (each has a before/after fixture in lib/ptx/opt-test.f):
\   COPYPROP   mov %d,%s (register copy) -> later reads of %d read %s.  A mov is a
\              bit-exact identity, so this is sound.
\   CONSTFOLD  mov %d,IMM ; a second mov of the SAME immediate is folded to the
\              first constant register (identical immediates share one value).
\   CSE        two identical pure ops with identical source value numbers compute
\              the identical bit pattern (same op, same rounding, IEEE
\              deterministic); the second's reads are redirected to the first.
\   DCE        a pure, side-effect-free instruction whose dst register is read
\              nowhere else (after all redirects) is removed. Loop-safe: a
\              register named anywhere else in the entry is kept (never removed).
\   PEEP       self-move `mov %r,%r` elimination (bit-exact identity).
\
\ NOT implemented (documented, dot habu-ptx-peephole-fma): mul+add -> fma fusion.
\ Our emitters emit mul/add with round-to-nearest (`.rn` or default), so fusing
\ folds two roundings into one and CHANGES the numeric result. It is never sound
\ for these emitters, so the pass refuses it; opt-test.f asserts a `.rn` mul+add
\ pair survives untouched.
\
\ Integration is OPT-IN and OFF by default (PTX-MAYBE-OPT). No emitter calls it
\ yet, so every proven kernel is byte-identical until a consumer opts in with
\ device evidence. Load after lib/ptx/opt-ir.f. Checked Habu.

1  constant OPT-M-COPYPROP
2  constant OPT-M-CONSTFOLD
4  constant OPT-M-CSE
7  constant OPT-M-SUBST       \ COPYPROP|CONSTFOLD|CSE: any read-redirect transform
8  constant OPT-M-DCE
16 constant OPT-M-PEEP
31 constant OPT-M-ALL

65536 constant OPTX-VNMAX

: OPT-ON? ( n n -- bool ) {: mask:n bit:n :} mask bit and 0 <> ;

\ ---- value-numbering state ----
create OPTX-VN-OF  OPTX-MAXSYM cells allot   \ current vn of a symbol id
create OPTX-VN-GEN OPTX-MAXSYM cells allot   \ generation stamp of OPTX-VN-OF[sym]
create OPTX-CANON  OPTX-VNMAX  cells allot   \ canonical register symbol holding a vn
variable OPTX-GEN
variable OPTX-VNEXT

: OPTX-FRESH-VN ( -- n )
   OPTX-VNEXT @ OPTX-VNMAX >= if E-PTX-OPT-OVERFLOW throw then
   OPTX-VNEXT @ dup 1+ OPTX-VNEXT ! ;
: OPTX-CANON-OF ( n -- n ) cells OPTX-CANON + @ ;
: OPTX-CANON! ( n n -- ) {: sym:n v:n :} sym v cells OPTX-CANON + ! ;
: OPTX-SET-VN ( n n -- ) {: v:n sym:n :}
   v OPTX-VN-OF sym cells + !  OPTX-GEN @ OPTX-VN-GEN sym cells + ! ;
: OPTX-VN-CUR ( n -- n ) {: sym:n :}   \ current vn if set this gen, else -1 (no mutation)
   OPTX-VN-GEN sym cells + @ OPTX-GEN @ = if OPTX-VN-OF sym cells + @ else -1 then ;
: OPTX-SYM-VN ( n -- n ) {: sym:n :}   \ vn of a symbol, minting a fresh external vn if unset
   sym OPTX-VN-CUR {: v:n :}
   v -1 <> if v exit then
   OPTX-FRESH-VN {: v2:n :}
   v2 sym OPTX-SET-VN  sym v2 OPTX-CANON!  v2 ;
: OPTX-DEF-FRESH ( n -- ) {: sym:n :}   \ sym takes a brand-new value, becomes its own canonical
   OPTX-FRESH-VN {: v:n :}
   v sym OPTX-SET-VN  sym v OPTX-CANON! ;

\ read this value from its canonical register, but only while that register still
\ holds the value (else keep the original source - never read a clobbered reg).
: OPTX-SUBST ( n -- n ) {: s:n :}
   s OPTX-SYM-VN {: v:n :}
   v OPTX-CANON-OF {: c:n :}
   c s = if s exit then
   c OPTX-VN-CUR v = if c else s then ;
: OPTX-SUBST-FIELD ( n n -- ) {: ix:n f:n :}
   ix f OPTX@ {: s:n :}
   s OPTX-SYM-REG? 0= if exit then
   s OPTX-SUBST {: s2:n :}
   s2 s <> if s2 ix f OPTX! 1 ix OPTX.REW OPTX! then ;
: OPTX-SUBST-SOURCES ( n n -- ) {: ix:n mask:n :}
   mask OPT-M-SUBST OPT-ON? 0= if exit then
   ix OPTX.NSRC OPTX@ {: ns:n :}
   ns 0 > if ix OPTX.S0 OPTX-SUBST-FIELD then
   ns 1 > if ix OPTX.S1 OPTX-SUBST-FIELD then
   ns 2 > if ix OPTX.S2 OPTX-SUBST-FIELD then ;

\ ---- register-token scan over an opaque line ----
: OPTX-ALNUM? ( n -- bool ) {: c:n :}
   c 48 >= c 57 <= and if 0 0= exit then
   c 65 >= c 90 <= and if 0 0= exit then
   c 97 >= c 122 <= and if 0 0= exit then
   c 95 = ;
: OPTX-RCH-ALNUM? ( n n n -- bool ) {: off:n len:n i:n :}
   i len >= if 0 0= 0= exit then
   off i + OPTX-CH OPTX-ALNUM? ;

variable OPTX-RS variable OPTX-RE
variable OPTX-NR-NX variable OPTX-NR-SYM variable OPTX-NR-FOUND
variable OPTX-SCAN-ST

: OPTX-NEXT-REG ( n n n -- n n bool ) {: off:n len:n start:n :}
   start OPTX-RS !
   begin OPTX-RS @ len < while
      off OPTX-RS @ + OPTX-CH OPTX-PCT-CH = if
         OPTX-RS @ 1+ OPTX-RE !
         begin off len OPTX-RE @ OPTX-RCH-ALNUM? while OPTX-RE @ 1+ OPTX-RE ! repeat
         off OPTX-RS @ + OPTX-RE @ OPTX-RS @ - OPTX-INTERN {: sym:n :}
         OPTX-RE @ sym 0 0= exit
      then
      OPTX-RS @ 1+ OPTX-RS !
   repeat
   len OPTX-NONE 0 0= 0= ;
: OPTX-NEXT-REG! ( n n n -- )
   OPTX-NEXT-REG OPTX-NR-FOUND ! OPTX-NR-SYM ! OPTX-NR-NX ! ;

: OPTX-INVALIDATE-REGS ( n n -- ) {: off:n len:n :}
   0 OPTX-SCAN-ST !
   begin off len OPTX-SCAN-ST @ OPTX-NEXT-REG! OPTX-NR-FOUND @ 0 <> while
      OPTX-NR-SYM @ OPTX-DEF-FRESH
      OPTX-NR-NX @ OPTX-SCAN-ST !
   repeat ;

\ ---- available-expression table (CSE), cleared at every region reset ----
create AV-CLASS OPTX-MAXLINE cells allot
create AV-MOFF  OPTX-MAXLINE cells allot
create AV-MLEN  OPTX-MAXLINE cells allot
create AV-SV0   OPTX-MAXLINE cells allot
create AV-SV1   OPTX-MAXLINE cells allot
create AV-SV2   OPTX-MAXLINE cells allot
create AV-NSRC  OPTX-MAXLINE cells allot
create AV-VN    OPTX-MAXLINE cells allot
variable AV-N

variable OPTX-K0 variable OPTX-K1 variable OPTX-K2 variable OPTX-KN
: OPTX-SVN ( n n -- n ) OPTX@ OPTX-SYM-VN ;   \ (already-substituted) source field -> value vn
: OPTX-AV-KEY! ( n -- ) {: ix:n :}
   ix OPTX.NSRC OPTX@ OPTX-KN !
   OPTX-NONE OPTX-K0 !  OPTX-NONE OPTX-K1 !  OPTX-NONE OPTX-K2 !
   OPTX-KN @ 0 > if ix OPTX.S0 OPTX-SVN OPTX-K0 ! then
   OPTX-KN @ 1 > if ix OPTX.S1 OPTX-SVN OPTX-K1 ! then
   OPTX-KN @ 2 > if ix OPTX.S2 OPTX-SVN OPTX-K2 ! then
   ix OPTX.COMMUT OPTX? if
      OPTX-K0 @ OPTX-K1 @ > if OPTX-K0 @ OPTX-K1 @ OPTX-K0 ! OPTX-K1 ! then
   then ;
: OPTX-AV-EQ? ( n n -- bool ) {: ix:n j:n :}   \ ix key (in OPTX-K*) vs AV entry j
   ix OPTX.CLASS OPTX@ AV-CLASS j cells + @ <> if 0 0= 0= exit then
   AV-NSRC j cells + @ OPTX-KN @ <> if 0 0= 0= exit then
   AV-SV0 j cells + @ OPTX-K0 @ <> if 0 0= 0= exit then
   AV-SV1 j cells + @ OPTX-K1 @ <> if 0 0= 0= exit then
   AV-SV2 j cells + @ OPTX-K2 @ <> if 0 0= 0= exit then
   ix OPTX.MOFF OPTX@ OPTX-AT ix OPTX.MLEN OPTX@
   AV-MOFF j cells + @ OPTX-AT AV-MLEN j cells + @ STR= ;
: OPTX-AV-FIND ( n -- n ) {: ix:n :}
   ix OPTX-AV-KEY!
   AV-N @ 0 ?do ix i OPTX-AV-EQ? if i unloop exit then loop
   OPTX-NONE ;
: OPTX-AV-ADD ( n n -- ) {: ix:n v:n :}
   AV-N @ OPTX-MAXLINE >= if exit then
   ix OPTX-AV-KEY!
   AV-N @ {: j:n :}
   ix OPTX.CLASS OPTX@ AV-CLASS j cells + !
   ix OPTX.MOFF OPTX@ AV-MOFF j cells + !  ix OPTX.MLEN OPTX@ AV-MLEN j cells + !
   OPTX-K0 @ AV-SV0 j cells + !  OPTX-K1 @ AV-SV1 j cells + !
   OPTX-K2 @ AV-SV2 j cells + !  OPTX-KN @ AV-NSRC j cells + !
   v AV-VN j cells + !
   AV-N @ 1+ AV-N ! ;

\ ---- per-line value numbering ----
\ record that register d now holds value v; if d ALREADY holds v, instruction ix
\ just rewrites the value the register already has (a redundant store) -> drop it.
\ This is the only sound way to remove a redundant recompute into a REUSED (non-
\ SSA) register: the earlier identical def already left v in d, so the rewrite is
\ a proven no-op. Fresh values never trigger it (d cannot already hold a new vn).
: OPTX-DEFINE ( n n n -- ) {: ix:n v:n d:n :}
   d OPTX-VN-CUR v = if 1 ix OPTX.REMOVED OPTX! then
   v d OPTX-SET-VN ;
: OPTX-VN-CONST ( n -- ) {: ix:n :}
   ix OPTX.S0 OPTX@ {: imm:n :}
   ix OPTX.DST OPTX@ {: d:n :}
   imm OPTX-SYM-VN {: v:n :}
   v OPTX-CANON-OF {: c:n :}
   c OPTX-SYM-REG? c OPTX-VN-CUR v = and 0= if d v OPTX-CANON! then
   ix v d OPTX-DEFINE ;
: OPTX-VN-OP ( n n -- ) {: ix:n mask:n :}
   ix OPTX.DST OPTX@ {: d:n :}
   mask OPT-M-CSE OPT-ON? if
      ix OPTX-AV-FIND {: av:n :}
      av OPTX-NONE <> if ix AV-VN av cells + @ d OPTX-DEFINE exit then
   then
   OPTX-FRESH-VN {: v:n :}
   ix v d OPTX-DEFINE  d v OPTX-CANON!
   mask OPT-M-CSE OPT-ON? if ix v OPTX-AV-ADD then ;
: OPTX-VN-PURE ( n n -- ) {: ix:n mask:n :}
   ix mask OPTX-SUBST-SOURCES
   ix OPTX.CLASS OPTX@ {: cls:n :}
   ix OPTX.DST OPTX@ {: d:n :}
   cls OPTX-C-MOVREG = mask OPT-M-COPYPROP OPT-ON? and if
      ix  ix OPTX.S0 OPTX@ OPTX-SYM-VN  d OPTX-DEFINE exit then
   cls OPTX-C-MOVIMM = mask OPT-M-CONSTFOLD OPT-ON? and if
      ix OPTX-VN-CONST exit then
   ix mask OPTX-VN-OP ;
: OPTX-VN-LINE ( n n -- ) {: ix:n mask:n :}
   ix OPTX.REMOVED OPTX? if exit then   \ a prior iteration dropped it: no VN effect
   ix OPTX.RESET OPTX? if OPTX-GEN @ 1+ OPTX-GEN ! 0 AV-N ! exit then
   ix OPTX.KIND OPTX? 0= if
      ix OPTX.OFF OPTX@ ix OPTX.LEN OPTX@ OPTX-INVALIDATE-REGS exit then
   ix mask OPTX-VN-PURE ;

\ ---- dead-code elimination (use counts over the whole table) ----
create OPTX-USE OPTX-MAXSYM cells allot
: OPTX-USE-CLEAR ( -- ) OPTX-MAXSYM 0 ?do 0 OPTX-USE i cells + ! loop ;
: OPTX-USE+ ( n -- ) {: sym:n :}
   sym 0 >= if OPTX-USE sym cells + dup @ 1+ swap ! then ;
: OPTX-USE-OPAQUE ( n n -- ) {: off:n len:n :}
   0 OPTX-SCAN-ST !
   begin off len OPTX-SCAN-ST @ OPTX-NEXT-REG! OPTX-NR-FOUND @ 0 <> while
      OPTX-NR-SYM @ OPTX-USE+  OPTX-NR-NX @ OPTX-SCAN-ST !
   repeat ;
: OPTX-USE-PURE ( n -- ) {: ix:n :}
   ix OPTX.NSRC OPTX@ {: ns:n :}
   ns 0 > if ix OPTX.S0 OPTX@ OPTX-USE+ then
   ns 1 > if ix OPTX.S1 OPTX@ OPTX-USE+ then
   ns 2 > if ix OPTX.S2 OPTX@ OPTX-USE+ then ;
: OPTX-BUILD-USE ( -- )
   OPTX-USE-CLEAR
   OPTX-N @ 0 ?do
      i OPTX.REMOVED OPTX? 0= if
         i OPTX.KIND OPTX? if i OPTX-USE-PURE
         else i OPTX.OFF OPTX@ i OPTX.LEN OPTX@ OPTX-USE-OPAQUE then
      then
   loop ;
: OPTX-DEAD? ( n -- bool ) {: d:n :}
   d 0 < if 0 0= 0= exit then
   OPTX-USE d cells + @ 0= ;
: OPTX-DCE-ROUND ( -- n )
   OPTX-BUILD-USE
   0 OPTX-N @ 0 ?do
      i OPTX.REMOVED OPTX? 0= i OPTX.KIND OPTX? and i OPTX.SIDEFX OPTX? 0= and
      i OPTX.DST OPTX@ OPTX-DEAD? and if 1 i OPTX.REMOVED OPTX! 1+ then
   loop ;
: OPTX-DCE ( -- ) begin OPTX-DCE-ROUND 0 > 0= until ;

\ ---- peephole: self-move elimination ----
: OPTX-PEEP ( -- )
   OPTX-N @ 0 ?do
      i OPTX.REMOVED OPTX? 0= i OPTX.KIND OPTX? and
      i OPTX.CLASS OPTX@ OPTX-C-MOVREG = and
      i OPTX.DST OPTX@ i OPTX.S0 OPTX@ = and if 1 i OPTX.REMOVED OPTX! then
   loop ;

\ ---- driver ----
\ GEN starts at 1 (not 0) so the zero-initialized OPTX-VN-GEN stamps never read as
\ "set this generation"; OPTX-VN-CUR then correctly reports every symbol unset.
: OPTX-VN-RESET ( -- )
   1 OPTX-GEN !  0 OPTX-VNEXT !  0 AV-N !
   OPTX-MAXSYM 0 ?do 0 OPTX-VN-GEN i cells + ! loop ;
: OPTX-REMOVED-COUNT ( -- n )
   0 OPTX-N @ 0 ?do i OPTX.REMOVED OPTX? if 1+ then loop ;
: PTX-OPT-ONCE ( n -- ) {: mask:n :}
   OPTX-VN-RESET
   OPTX-N @ 0 ?do i mask OPTX-VN-LINE loop
   mask OPT-M-DCE OPT-ON? if OPTX-DCE then
   mask OPT-M-PEEP OPT-ON? if OPTX-PEEP then ;
\ iterate value-numbering + DCE to a fixpoint: a removal can expose a further
\ redundancy on the next pass, and removals are monotonic, so this terminates
\ and makes OPT-PTX idempotent (opt(opt(x)) = opt(x)).
: PTX-OPT ( n -- ) {: mask:n :}
   begin
      OPTX-REMOVED-COUNT {: before:n :}
      mask PTX-OPT-ONCE
      OPTX-REMOVED-COUNT before <>
   while repeat ;

\ OPT-PTX ( ptr u8 n -- ptr u8 n ): optimize a captured module entry-by-entry
\ (entry boundaries are RESET lines, so value numbering never crosses them).
: OPT-PTX ( ptr u8 n -- ptr u8 n )
   PTX-PARSE  OPT-M-ALL PTX-OPT  PTX-RENDER  PTX-RENDER$ ;

\ per-pass entry points for the unit fixtures (each pairs its rule with DCE removal)
: PTX-OPT-COPYPROP ( -- )  OPT-M-COPYPROP OPT-M-DCE or PTX-OPT ;
: PTX-OPT-CONSTFOLD ( -- ) OPT-M-CONSTFOLD OPT-M-DCE or PTX-OPT ;
: PTX-OPT-CSE ( -- )       OPT-M-CSE OPT-M-DCE or PTX-OPT ;
: PTX-OPT-DCE ( -- )       OPT-M-DCE PTX-OPT ;
: PTX-OPT-PEEP ( -- )      OPT-M-PEEP PTX-OPT ;
: PTX-OPT-ALL ( -- )       OPT-M-ALL PTX-OPT ;

\ ---- opt-in wiring: OFF by default; a consumer flips it with device evidence ----
variable PTX-OPT-ENABLED
: PTX-OPT-ON ( -- )  1 PTX-OPT-ENABLED ! ;
: PTX-OPT-OFF ( -- ) 0 PTX-OPT-ENABLED ! ;
: PTX-OPT? ( -- bool ) PTX-OPT-ENABLED @ 0 <> ;
: PTX-MAYBE-OPT ( ptr u8 n -- ptr u8 n )
   PTX-OPT? if OPT-PTX then ;
