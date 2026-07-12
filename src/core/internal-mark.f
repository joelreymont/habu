\ internal-mark.f - seal-time internal-word + min-in marking pass (dots
\ habu-hb-crash-bare-c5be6634, habu-habu-certified-words-84e84eaf).
\
\ Loaded as the LAST cold-prefix source (after script-argv.f, before the
\ SEAL-CAPTURE watermark token). Walks every dictionary record the engine
\ prefix defined - from the util.f IMK-NDICT0 prim-boundary watermark to the
\ current ndict - and classifies each global-wordlist COLON record (body entry
\ = the C-CALL-PROLOGUE-INSTR frame setup):
\ - no checker-known effect (no certified or trusted signature and no
\   primitive axiom; SIG-MIN-IN misses): set DNAME-INT (flags bit 63). The
\   interpret dispatch and interpret-mode tick (habu2.f EM-INTERPRET-FIND /
\   C-TICK) fail closed on the folded flag with
\   `hb: internal engine word: <token>` + rc 70 / catchable RC-REJECT inside
\   evaluate, so the executable top-level name universe equals the checker's:
\   a colon word the checker reports E-UNDEFINED inside checked code cannot be
\   executed or ticked bare at top level either.
\ - checker-known effect with din > 0: poke DNAME-MIN-IN (flags bits 52-59)
\   with the effect's declared input cell count, so EM-INTERPRET-FIND fails
\   closed (`hb: interpret stack underdepth: <token>` + rc 70) on a bare call
\   with fewer interpret-stack cells - closing the same below-base read for
\   the axiom'd/signed engine-prefix words (CHECK, CORE-STR=, ...) that stay
\   top-level executable by design, and re-poking the records certified in the
\   hook window before this file loads (idempotent: the publish tail already
\   OR'd the same byte).
\ Compiled references from explicitly unchecked user code and TRUSTED: bodies
\ are untouched: those are declared trusted boundaries.
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

\ Order matters: IMK-COLON? dereferences the body cell, and only global
\ (wid 0) records are guaranteed to hold a code pointer there - package name
\ records (wid -1) store the package's wordlist id in the body cell, so the
\ wid gate must short-circuit before any body read.
: IMK-GLOBAL-COLON? ( n -- bool )
   dup IMK-WID 0 = IF IMK-COLON? ELSE drop 0 0= 0= THEN ;

: IMK-MIN-IN ( n -- n )      \ din cell width of the record's checker effect; -1 unknown
   dup IMK-NAME-A swap IMK-NAME-U SIG-MIN-IN ;

: IMK-CLASSIFY ( n -- ) {: i:n :}   \ unknown -> DNAME-INT; known din>0 -> DNAME-MIN-IN
   i IMK-GLOBAL-COLON? 0= IF EXIT THEN
   i IMK-MIN-IN {: m:n :}
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
   IMK-WALK
   IMK-SEAL-PRIM ;

LOWER-CERT-HOOK:INSTALL
IMK-PASS
