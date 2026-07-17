\ internal-mark.f - seal-time internal-word + min-in marking pass (dots
\ habu-hb-crash-bare-c5be6634, habu-habu-certified-words-84e84eaf).
\
\ Loaded as the LAST cold-prefix source (after script-argv.f, before the
\ SEAL-CAPTURE watermark token). Walks every dictionary record the engine
\ prefix defined - from the util.f IMK-NDICT0 prim-boundary watermark to the
\ current ndict - and classifies each global or package COLON record (body
\ entry = the C-CALL-PROLOGUE-INSTR frame setup):
\ - a global or exact public/private package word with no checker-known effect
\   (no certified or trusted signature and no primitive axiom): set DNAME-INT
\   (flags bit 63).
\   The
\   interpret dispatch and interpret-mode tick (habu2.f EM-INTERPRET-FIND /
\   C-TICK) fail closed on the folded flag with
\   `hb: internal engine word: <token>` + rc 70 / catchable RC-REJECT inside
\   evaluate, so the executable top-level name universe equals the checker's:
\   a colon word the checker reports E-UNDEFINED inside checked code cannot be
\   executed or ticked bare at top level either.
\ - a checker-known global or exact public/private package effect with din > 0:
\   poke DNAME-MIN-IN (flags bits 52-59) with the effect's declared input cell
\   count, so EM-INTERPRET-FIND fails
\   closed (`hb: interpret stack underdepth: <token>` + rc 70) on a bare call
\   with fewer interpret-stack cells - closing the same below-base read for
\   the axiom'd/signed engine-prefix words (CHECK, CORE-STR=, ...) that stay
\   top-level executable by design, and re-poking the records certified in the
\   hook window before this file loads (idempotent: the publish tail already
\   OR'd the same byte).
\ Compiled references from explicitly unchecked user code and TRUSTED: bodies
\ are untouched.
\
\ Data records (create/variable/constant and does>-instances; entry is a
\ literal push, not the colon prologue) are exempt: their bodies push an
\ address or value and cannot consume interpret-stack garbage, and the engine
\ auto-trusts exactly this class whenever a check hook is installed
\ (habu2.f C-CALL-TRUST-LASTC-*), so the checker-boot ones are unknown only
\ because they load before the hook exists. The truly dangerous engine cells
\ are owned by the PROT-GUARD friend bands, not by name visibility.
\
\ The flag stores go through the `int-mark` / `min-in-mark` prims (habu1.f
\ BINTMARK / BMININMARK): the dict region is read-only at runtime, so each prim
\ brackets its store with the LPROT RW/RX toggle. Marking is monotonic - no
\ clearing primitive exists - and the pass finishes by marking both prims' own
\ records, so user source can never re-drive or extend the marking.
\
\ Unchecked boundary (whole file): the pass reads raw dictionary-record cells,
\ and its own words must stay checker-unknown so the pass marks THEM internal
\ too - self-sealing, nothing here remains callable from user source once
\ IMK-PASS has run. The check hook is reinstalled (LOWER-CERT-HOOK:INSTALL)
\ before the pass executes.
\ Regression: test/internal-word-gate.f.
0 set-check

variable IMK-I
variable IMK-P
variable IMK-S

32 constant IMK-OWNER-SHIFT
$FFFFFFFF constant IMK-WID-MAX
$7FFFFFFE constant IMK-PKG-MAX
$8000000000000000 constant IMK-PRIVATE-BIT

variable IMK-OWNER-N
variable IMK-OWNER-KEY-N
variable IMK-OWNER-CAP
variable IMK-OWNER-MASK
variable IMK-OWNER-TAB

\ compiled-word entry frame setup (= habu2.f C-CALL-PROLOGUE-INSTR): every
\ colon body starts with it; create/variable/constant/does>-instance bodies
\ start with their literal push instead.
$D10043FF constant IMK-PROLOGUE

: IMK-REC ( n -- a )
   DREC * dbase@ + ;

: IMK-FLAGS@ ( n -- n )
   IMK-REC 16 + @ ;

: IMK-WID ( n -- n )
   IMK-REC 40 + @ ;

: IMK-NAME-A ( n -- a )   \ inline names live at +24; DNAME-EXT names point out of line
   IMK-REC dup 16 + @ DNAME-EXT and 0 <> IF 24 + @ ELSE 24 + THEN ;

: IMK-NAME-U ( n -- n )
   IMK-FLAGS@ DNAME-LEN-MASK and ;

: IMK-INSN0 ( n -- n )    \ first instruction word of the record body
   IMK-REC @ {: b:n :}
   b c@
   b 1 + c@ 8 lshift or
   b 2 + c@ 16 lshift or
   b 3 + c@ 24 lshift or ;

: IMK-COLON? ( n -- bool )
   IMK-INSN0 IMK-PROLOGUE = ;

\ Order matters: package name records (wid -1) store public/private WIDs in
\ their body cells, so the wid gate must short-circuit before any body read.
\ Every other dictionary record that starts with the prologue is a COLON.
: IMK-COLON-WORD? ( n -- bool )
   dup IMK-WID -1 = IF drop RES-FALSE EXIT THEN
   IMK-COLON? ;

: IMK-OWNER-FAIL ( -- )
   s" internal-mark: invalid package owner table" ENGINE-ERROR:SEAL-PACKAGE die ;

: IMK-OWNER-A ( n -- ptr )
   cells IMK-OWNER-TAB @ + ;

\ One packed cell per sparse WID: low 32 bits hold the key, the next 31 hold
\ package-record index + 1, and the high bit distinguishes private visibility.
: IMK-OWNER-PACK ( n n n -- n ) {: wid:n pkg:n vis:n :}
   wid 0 <= IF IMK-OWNER-FAIL THEN
   wid IMK-WID-MAX > IF IMK-OWNER-FAIL THEN
   pkg 0 < IF IMK-OWNER-FAIL THEN
   pkg IMK-PKG-MAX > IF IMK-OWNER-FAIL THEN
   vis SYM-PUBLIC <> vis SYM-PRIVATE <> and IF IMK-OWNER-FAIL THEN
   pkg 1 + IMK-OWNER-SHIFT lshift
   vis SYM-PRIVATE = IF IMK-PRIVATE-BIT or THEN
   wid or ;

: IMK-OWNER+ ( n n n -- ) {: wid:n pkg:n vis:n :}
   IMK-OWNER-N @ IMK-OWNER-CAP @ 2 / >= IF IMK-OWNER-FAIL THEN
   wid pkg vis IMK-OWNER-PACK {: ent:n :}
   wid IMK-OWNER-MASK @ and IMK-S !
   0 IMK-P !
   BEGIN
      IMK-S @ IMK-OWNER-A @ dup 0= IF
         drop ent IMK-S @ IMK-OWNER-A !
         IMK-OWNER-N @ 1 + IMK-OWNER-N !
         EXIT
      THEN
      IMK-WID-MAX and wid = IF IMK-OWNER-FAIL THEN
      IMK-P @ 1 + dup IMK-P ! IMK-OWNER-CAP @ >= IF IMK-OWNER-FAIL THEN
      IMK-S @ 1 + IMK-OWNER-MASK @ and IMK-S !
   AGAIN ;

: IMK-OWNER-PKG+ ( n -- ) {: pkg:n :}
   pkg IMK-REC {: r:ptr :}
   r @ pkg SYM-PUBLIC IMK-OWNER+
   r 8 + @ dup 0 <> IF pkg SYM-PRIVATE IMK-OWNER+ ELSE drop THEN ;

: IMK-OWNER-CLEAR ( -- )
   0 IMK-P !
   BEGIN IMK-P @ IMK-OWNER-CAP @ < WHILE
      0 IMK-P @ IMK-OWNER-A !
      IMK-P @ 1 + IMK-P !
   REPEAT ;

\ Census live package WID keys at source load, then allocate an exact
\ power-of-two table at <= 50% load. Public WIDs are required; a header created
\ by a qualified definition may validly have private WID zero until first use.
\ Later definitions in this file are global, so the census stays stable.
: IMK-OWNER-KEY+ ( n -- )
   dup 0 <= IF drop IMK-OWNER-FAIL THEN
   IMK-WID-MAX > IF IMK-OWNER-FAIL THEN
   IMK-OWNER-KEY-N @ 1 + IMK-OWNER-KEY-N ! ;

: IMK-OWNER-PKG-COUNT ( n -- )
   IMK-REC {: r:ptr :}
   r @ IMK-OWNER-KEY+
   r 8 + @ dup 0= IF drop EXIT THEN
   IMK-OWNER-KEY+ ;

: IMK-OWNER-ALIGN ( -- )
   here negate 1 cells 1 - and allot ;

: IMK-OWNER-ALLOC ( -- )
   0 IMK-OWNER-KEY-N !
   0 IMK-I !
   BEGIN IMK-I @ ndict@ < WHILE
      IMK-I @ IMK-WID -1 = IF IMK-I @ IMK-OWNER-PKG-COUNT THEN
      IMK-I @ 1 + IMK-I !
   REPEAT
   1 IMK-OWNER-CAP !
   BEGIN IMK-OWNER-CAP @ IMK-OWNER-KEY-N @ 2 * < WHILE
      IMK-OWNER-CAP @ 2 * IMK-OWNER-CAP !
   REPEAT
   IMK-OWNER-CAP @ 1 - IMK-OWNER-MASK !
   IMK-OWNER-ALIGN
   here 1 cells 1 - and 0 <> IF IMK-OWNER-FAIL THEN
   here IMK-OWNER-TAB !
   IMK-OWNER-CAP @ cells allot ;

\ Build once from package headers, instead of rescanning ndict for every
\ package COLON. Packing bounds, duplicates, and occupancy fail closed.
: IMK-OWNER-BUILD ( -- )
   IMK-OWNER-CLEAR
   0 IMK-OWNER-N !
   0 IMK-I !
   BEGIN IMK-I @ ndict@ < WHILE
      IMK-I @ IMK-WID -1 = IF IMK-I @ IMK-OWNER-PKG+ THEN
      IMK-I @ 1 + IMK-I !
   REPEAT
   IMK-OWNER-N @ IMK-OWNER-KEY-N @ <> IF IMK-OWNER-FAIL THEN ;

\ Resolve an ordinary record WID to exact package identity and visibility;
\ arbitrary non-package wordlists miss and remain outside this contract.
: IMK-PKG-OWNER ( n -- n n bool ) {: wid:n :}
   wid 0 <= IF 0 0 RES-FALSE EXIT THEN
   wid IMK-WID-MAX > IF 0 0 RES-FALSE EXIT THEN
   wid IMK-OWNER-MASK @ and IMK-S !
   0 IMK-P !
   BEGIN
      IMK-S @ IMK-OWNER-A @ dup 0= IF drop 0 0 RES-FALSE EXIT THEN
      dup IMK-WID-MAX and wid = IF
         dup IMK-OWNER-SHIFT rshift $7FFFFFFF and 1 -
         swap IMK-PRIVATE-BIT and 0 <> IF SYM-PRIVATE ELSE SYM-PUBLIC THEN
         RES-TRUE EXIT
      THEN
      drop
      IMK-P @ 1 + dup IMK-P ! IMK-OWNER-CAP @ >= IF 0 0 RES-FALSE EXIT THEN
      IMK-S @ 1 + IMK-OWNER-MASK @ and IMK-S !
   AGAIN ;

: IMK-PKG-MIN-IN ( n -- n bool ) {: i:n :}
   i IMK-WID IMK-PKG-OWNER 0= IF 2drop -1 RES-FALSE EXIT THEN
   {: pkg:n vis:n :}
   pkg IMK-NAME-A pkg IMK-NAME-U vis
   i IMK-NAME-A i IMK-NAME-U CHECKER-PKG-SYM?
   SIG-MIN-IN-SYM RES-TRUE ;

: IMK-MIN-IN ( n -- n )      \ din cell width of the record's checker effect; -1 unknown
   dup IMK-NAME-A swap IMK-NAME-U SIG-MIN-IN ;

: IMK-EFFECT-MIN-IN ( n -- n bool ) {: i:n :}
   i IMK-WID 0= IF i IMK-MIN-IN RES-TRUE EXIT THEN
   i IMK-PKG-MIN-IN ;

: IMK-CLASSIFY ( n -- ) {: i:n :}   \ unknown -> DNAME-INT; known din>0 -> DNAME-MIN-IN
   i IMK-COLON-WORD? 0= IF EXIT THEN
   i IMK-EFFECT-MIN-IN 0= IF drop EXIT THEN
   {: m:n :}
   m 0 < IF i int-mark EXIT THEN
   m 0 > IF i m min-in-mark THEN ;

: IMK-WALK ( -- )            \ classify every record in [IMK-NDICT0, ndict)
   IMK-NDICT0 @ IMK-I !
   BEGIN IMK-I @ ndict@ < WHILE
      IMK-I @ IMK-CLASSIFY
      IMK-I @ 1 + IMK-I !
   REPEAT ;

: IMK-NAMED? ( n ptr u8 n -- bool ) {: i:n a:ptr u:n :}
   i IMK-NAME-U u = IF i IMK-NAME-A i IMK-NAME-U a u CORE-STR= ELSE 0 0= 0= THEN ;

: IMK-PRIM? ( n -- bool )    \ record n is one of the marking prims themselves
   dup s" int-mark" IMK-NAMED? IF drop 0 0= EXIT THEN
   s" min-in-mark" IMK-NAMED? ;

: IMK-SEAL-PRIM ( -- )       \ close the loop: the marking prims are themselves internal
   0 IMK-I !
   BEGIN IMK-I @ IMK-NDICT0 @ < WHILE
      IMK-I @ IMK-PRIM? IF IMK-I @ int-mark THEN
      IMK-I @ 1 + IMK-I !
   REPEAT ;

: IMK-PASS ( -- )
   IMK-OWNER-BUILD
   IMK-WALK
   IMK-SEAL-PRIM ;

LOWER-CERT-HOOK:INSTALL
IMK-OWNER-ALLOC
IMK-PASS
