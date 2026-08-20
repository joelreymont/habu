\ internal-mark.f - seal-time internal-word + min-in marking pass (dots
\ habu-hb-crash-bare-c5be6634, habu-habu-certified-words-84e84eaf).
\
\ Loaded as the LAST cold-prefix source (after script-argv.f, before the
\ SEAL-CAPTURE watermark token). Walks every dictionary record the engine
\ prefix defined - from the util.f IMK-NDICT0 prim-boundary watermark to the
\ current ndict - and classifies each COLON record (body entry = the
\ C-CALL-PROLOGUE-INSTR frame setup) that has a top-level spelling: a global
\ one under its bare name, a package PUBLIC one under its qualified PKG:TAIL
\ name (dot habu-pkg-publics-escape-41532ee7; package privates have no
\ top-level spelling at all - see IMK-WALK-PACKAGES below):
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

: IMK-MARK ( n n -- ) {: i:n m:n :}   \ unknown -> DNAME-INT; known din>0 -> DNAME-MIN-IN
   m 0 < IF i int-mark EXIT THEN
   m 0 > IF i m min-in-mark THEN ;

: IMK-CLASSIFY ( n -- ) {: i:n :}   \ a global record, under its bare name
   i IMK-GLOBAL-COLON? 0= IF EXIT THEN
   i i IMK-MIN-IN IMK-MARK ;

: IMK-WALK ( -- )            \ classify every record in [IMK-NDICT0, ndict)
   IMK-NDICT0 @ IMK-I !
   BEGIN IMK-I @ ndict@ < WHILE
      IMK-I @ IMK-CLASSIFY
      IMK-I @ 1 + IMK-I !
   REPEAT ;

\ ---- package publics, under their qualified name ---------------------------
\ A package word carries its wordlist, not 0, so IMK-WALK above never reaches
\ one - and until dot habu-pkg-publics-escape-41532ee7 that left every package
\ public top-level executable whatever the checker knew: `0 0 SCHEMA-REG:REWIND`
\ wiped the schema registry and exited 0, and `PRIM-LINK:COUNT` read six cells
\ below the interpret base and aborted, the same crash class as the bare U-TYPE
\ this file was written for.
\
\ THE QUALIFIED SPELLING REACHES PUBLICS AND NOTHING ELSE, so publics are the
\ whole of the escape. habu1.f FIND-NMATCH resolves PKG:TAIL by taking the
\ search wid from the package row's [0] - the package's PUBLIC wordlist - and a
\ package PRIVATE word therefore has no top-level spelling at all: bare misses
\ it because it is not global, and qualified misses it because the qualifier
\ never names its wordlist. Nothing to mark, and test/internal-word-gate.f pins
\ that a private stays E-UNDEFINED under its own qualified name.
\
\ THE QUESTION IS THE SAME QUESTION, spelled the way the token is: SIG-MIN-IN of
\ "PKG:TAIL". checker.f CHECKER-FIND-ACTIVE-SYM routes a qualified name straight
\ to CHECKER-PUBLIC-SYM? -> SYM-FIND (checker.f:4212) keyed on (package,
\ SYM-PUBLIC, tail), which is the key PPRIM; interns and the key the checker
\ itself uses at a reference site. So a package public the checker can type stays
\ callable and one it cannot fails closed, by the same rule as a global.
\
\ WHY THE PACKAGE ROWS DRIVE THE LOOP rather than a wid -> package map: the row
\ IS the record that carries both halves of the answer, its public wordlist in
\ [0] and the package name in its own name field, so reading it once per package
\ needs no second table to fall out of step with the dictionary. The wid-0 arm
\ above decides first, so the two passes partition the records no matter what a
\ row's [0] holds. Each package's walk starts AT its own row because no earlier
\ record can be in its public wordlist: habu2.f C-PACKAGE-NEW-RECORD allocates
\ both wids and writes [0] as it publishes the row, so the id does not exist
\ before the row does. Measured on the boot prefix, that is 112k record tests
\ instead of 345k.
\
\ The body-read order IMK-GLOBAL-COLON? guards above is kept for the same
\ reason and by a stronger test: a namespace row carries DICT-WL:NAMESPACE in
\ its wordlist cell and an allocated wordlist id is never that, so matching the
\ id first cannot select a row whose [0] is a number rather than a code pointer.
1024 constant IMK-QCAP        \ qualified-name scratch: package + ':' + tail
create IMK-QBUF IMK-QCAP allot
variable IMK-QU
variable IMK-QI
variable IMK-P               \ package-row cursor (IMK-I carries the inner walk)

: IMK-Q+ ( ptr u8 n -- ) {: a:ptr u:n :}
   IMK-QU @ u + IMK-QCAP > IF
      s" internal-mark: qualified name exceeds scratch" 76 die THEN
   0 IMK-QI !
   BEGIN IMK-QI @ u < WHILE
      a IMK-QI @ + c@ IMK-QBUF IMK-QU @ + c!
      IMK-QU @ 1 + IMK-QU !
      IMK-QI @ 1 + IMK-QI !
   REPEAT ;

: IMK-QUAL ( n n -- ptr u8 n ) {: p:n i:n :}   \ package row, word record -> PKG:TAIL
   0 IMK-QU !
   p IMK-NAME-A p IMK-NAME-U IMK-Q+
   s" :" IMK-Q+
   i IMK-NAME-A i IMK-NAME-U IMK-Q+
   IMK-QBUF IMK-QU @ ;

: IMK-CLASSIFY-PUB ( n n -- ) {: p:n i:n :}
   i IMK-COLON? 0= IF EXIT THEN
   i p i IMK-QUAL SIG-MIN-IN IMK-MARK ;

: IMK-PKG-PUBLICS ( n -- ) {: p:n :}   \ every public colon record of package row p
   p IMK-REC @ {: pub:n :}
   p 1 + IMK-NDICT0 @ max IMK-I !
   BEGIN IMK-I @ ndict@ < WHILE
      IMK-I @ IMK-WID pub = IF p IMK-I @ IMK-CLASSIFY-PUB THEN
      IMK-I @ 1 + IMK-I !
   REPEAT ;

: IMK-WALK-PACKAGES ( -- )   \ classify every package's public wordlist
   0 IMK-P !
   BEGIN IMK-P @ ndict@ < WHILE
      IMK-P @ IMK-WID DICT-WL:NAMESPACE = IF IMK-P @ IMK-PKG-PUBLICS THEN
      IMK-P @ 1 + IMK-P !
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

\ Registry write-protection (dot habu-protect-type-field-04d91409). A din=0
\ registry control cell (variable/create) is a data record, so IMK-WALK exempts
\ it and its bare name stays executable — a bare `<cell> !` mutates the registry
\ past the public API. type-family.f's REG-PROTECT recorded each such cell's
\ dictionary index in REG-PROT-IDX[0, REG-PROT-N); int-mark them so interpret /
\ tick fail closed on the bare name exactly like a sig-less colon word, while the
\ core compiled callers (resolved before this pass) keep working.
: IMK-SEAL-REGISTRY ( -- )
   0 IMK-I !
   BEGIN IMK-I @ REG-PROT-N @ < WHILE
      REG-PROT-IDX IMK-I @ cells + @ int-mark
      IMK-I @ 1 + IMK-I !
   REPEAT ;

: IMK-PASS ( -- )
   IMK-WALK
   IMK-WALK-PACKAGES
   IMK-SEAL-REGISTRY
   IMK-SEAL-PRIM ;

LOWER-CERT-HOOK:INSTALL
IMK-PASS
