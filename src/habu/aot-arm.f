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
\ depend on src/habu/layout.f and nothing else.
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
: OPEN ( n n -- ) {: b0:n d0:n :}
   d0 LIVE AOT-WINDOW:D0-CELL + CELL!
   b0 LIVE AOT-WINDOW:B0-CELL + CELL! ;

;package
