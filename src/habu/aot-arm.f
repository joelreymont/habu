\ aot-arm.f — the one writer of the engine's capture-window cells.
\
\ The engine keeps two cells naming the AOT capture window that is currently open:
\ src/habu/layout.f AOT-WINDOW:D0-CELL (its first DATA address) and
\ AOT-WINDOW:B0-CELL (its first code address). habu2.f reads them in the
\ compile-mode inliner (AOT-WINDOW:EMIT-OUTSIDE) so a body holding an address the
\ window cannot describe is CALLED rather than copied. Arming them is a two-cell
\ write, and the two cells only mean anything together, so exactly one word does
\ it. A second writer is drift: half a window is a window whose inliner declines
\ against one axis and copies against the other.
\
\ WHY THAT WORD DOES NOT LIVE IN src/habu/aot-capture.f. The chain capture runs
\ inside a booted bin/hb and its window opens at the FIRST USER TOKEN — before
\ anything the capture tool needs has loaded. Loading aot-capture.f first was
\ tried and refuted by measurement: its own closure requires src/arch/arm64/asm.f
\ and src/arch/arm64/icode.f, the compiler chain requires asm.f too, and `require`
\ is a no-op the second time, so every chain word that calls an asm.f word ends up
\ calling the CAPTURE TOOL's copy of it — a word the booting engine has and no
\ target does. 98 of 18602 call sites, refused by name at aot-capture.f
\ ACAP-SITE-BAND (first: the chain's MASK calling A64ASM's LIMM?). So the arming
\ word has to be loadable with the chain still ahead of it, which means it may
\ depend on src/habu/layout.f and nothing else. That dependency is free where it
\ matters: layout.f is already registered in a booted engine, so requiring it
\ there compiles 0 records, while requiring asm.f compiles 178 (both measured).
\
\ WHO LOADS IT. Two processes, the same way aot-decl.f is loaded by two: the stdin
\ metabuild host compiles it in (manifest slot tools/stdin-closure-lib.f SDC-ARM$,
\ ahead of the driver src/habu/stdin.f whose CAPTURE-REPL calls it), and the chain
\ capture tool requires it in its own prelude behind layout.f alone.
\
\ EVERY CALLER CALLS THIS WORD, not a wrapper. src/habu/aot-capture.f briefly
\ carried a capture-side WINDOW-OPEN that forwarded here; it was deleted, because
\ a second name for a one-writer operation is the one thing that can grow a second
\ body. Callers today: stdin.f CAPTURE-REPL, test/aot-band-lib.f (which also
\ passes 0 0 on purpose, below), and the chain capture tool named above.

package AOT-ARM

\ Raw cell boundary, the same shape aot-capture.f uses for the same two cells:
\ the live DATA base is a `ptr a` and storing a cell through it is what the
\ checker cannot state on its own. Retirement: habu-builder-trust-rows-c5d41af6.
: LIVE ( -- ptr a ) data-base ;
: CELL! ( n ptr a -- ) ! ;
s" CELL!" s" n ptr a --" TRUST

public

\ Arm the window: b0 is the code cursor and d0 the DATA cursor as they stand at
\ the moment the window opens. Both cells are written, always, from this one
\ place. Passing 0 0 disarms — an unarmed window is a real state (the engine then
\ copies pre-window bodies instead of calling them), which test/aot-band-lib.f
\ OPEN-UNARMED uses to put a pre-window DATA literal in front of the capture's
\ DATA audit.
\
\ THE CHECKER'S SIGNATURE CAPTURE STARTS HERE TOO, from the same call rather than
\ a second one: a window opens exactly once, and the rows the checker collects
\ from that moment are the signatures of the words inside it. Two independent
\ arming calls would let a capture take one window's records with another
\ window's signatures.
\
\ IT ARMS ON `0 0` AS WELL, and that is the whole point of keeping the two axes
\ apart. Passing 0 0 disarms the INLINER's window - it is what makes the engine
\ copy a pre-window body instead of calling it, which test/aot-band-lib.f
\ OPEN-UNARMED needs to put a pre-window DATA literal in front of the capture's
\ DATA audit. It does not say "collect nothing": that window is still a window
\ being captured, and its words still need their signatures carried. Tying the
\ collection to the CELLS' VALUE conflated the two and made the band fixtures
\ refuse for the wrong reason (measured: HOLDER reported as uncarried, masking
\ the address refusal the fixture exists to check). SIG-CLOSE below is the only
\ thing that ends the collection.
\ THE TYPE REGISTRY IS MARKED FROM THE SAME CALL, for the same reason: what a
\ capture carries is the registry DELTA the window declared, and the base that
\ delta appends at is whatever the registry held when the window opened. One
\ window, one base, one call - a separate marking call could name a different
\ moment, and family ids taken against the wrong base name other families.
: OPEN ( n n -- ) {: b0:n d0:n :}
   d0 LIVE AOT-WINDOW:D0-CELL + CELL!
   b0 LIVE AOT-WINDOW:B0-CELL + CELL!
   CHECKER-ASIG-ARM
   CHECKER-REG-AOT-MARK ;

\ The window's DEFINITIONS end here, which is not the same fact as the window's
\ SPAN ending - the span deliberately has no close (see aot-capture.f: a capture
\ snapshots [d0, here) and later definitions extend the same window). What ends
\ is the set of definitions the artifact will carry, and everything a capture
\ TOOL compiles after its window - its own icode.f, aot-decl.f, aot-capture.f and
\ artifact writer - is not in that set. Measured on the compiler chain: leaving
\ the store armed past the window collected 8279 rows where the window has 7556,
\ and the 723 extra name words no target engine has. A signature for a word that
\ is not there is worse than a missing one: a definition naming it would certify
\ and then call nothing.
\ THE WINDOW'S TYPES END HERE TOO, and for the same reason its definitions do: a
\ capture tool loads its own assembler and artifact writer after the window, and
\ those declare families of their own. Read live at capture time the registry
\ delta carried them, and the seeded engine measured its base against a registry
\ that had counted types no target has.
: SIG-CLOSE ( -- )
   CHECKER-ASIG-DISARM
   CHECKER-REG-AOT-CLOSE ;

\ The engine's next wordlist id. It is read here, beside the window's other base
\ cursors, because every producer needs it at the same two moments they need those
\ - when the window opens and when it closes - and the pair is the wid span the
\ capture declares (aot-capture.f WID-SPAN).
: WIDN ( -- n ) LIVE WIDN-CELL + @ ;

;package
