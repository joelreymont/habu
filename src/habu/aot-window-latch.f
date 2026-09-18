\ aot-window-latch.f - the stripped capture window's span cells and the two words
\ that latch it, in a file that requires no lib module.
\
\ WHY IT IS ITS OWN FILE. The window has to open before anything the application
\ could `require` is loaded. A library already loaded when the window opens is the
\ copy the application's own `require` resolves to, so every persistent cell that
\ library owns sits BELOW the span and aot-closure.f DATA-ADDRESS! refuses the
\ image with "address refers to data outside the restored span". That is what any
\ program reading the environment used to get: the linker's src/habu/app-image.f
\ pulls in app-image-core.f, and that requires eight lib modules including
\ lib/process-env.f, so GETENV's table was already below the window before the
\ application was even read. The latch words therefore cannot live in aot-lib.f
\ with the rest of the linker. tools/aot-build-core.f loads THIS file, opens the
\ window, loads the application, latches the span, and only then loads the linker
\ above it. src/habu/aot-decl.f is the other lib-free candidate and is deliberately
\ not used: it is baked into the engine (tools/bootstrap.sh SRC_COMMON,
\ tools/build-fixpoint.f BF-APPEND-COMMON), so a change there costs three
\ generations and buys nothing this file does not give.
require src/habu/layout.f

package AOT-LINK

\ --- persistent data region: the program's compile-time create/variable/allot/
\ ,/s" data lives contiguously from the DATA pointer latched before user
\ compilation to `here`; the source buffer and assembler CODE buffer are separate
\ mmaps, so AOT-LINK never allots either into DATA. src/habu/aot-lib.f emits that
\ span into __text and the entry maps DATA-VA and copies it back to the SAME
\ absolute VA (DATA-VA is a fixed MAP_FIXED VA, so those addresses are
\ load-stable). All other runtime cells stay zero from the fresh anonymous mmap,
\ and that zero is a value only where something declares it one: x20, S0-CELL and
\ DP-CELL get explicit init, and so does every cell named in
\ src/habu/aot-owned-cells.f (aot-lib.f EMIT-OWNED-CELLS - the engine's
\ environment and the dynamic-storage registry). A cell on neither list is
\ refused.
\
\ The span bounds are DATA addresses as integers, the domain the rest of the
\ linker works in, and nothing dereferences them: a cell inside the span is read
\ through aot-closure.f DATA-CELL@, which is where a DATA address becomes a
\ pointer again.
variable BLOB-SRC  variable BLOB-END  variable BLOB-LEN

\ The DATA cursor as one of those integers, which aot-closure.f AOT-DBASE-N and
\ AOT-CP-N get from their numeric primitives and this one has to compute: `here`
\ is the only pointer-valued cursor, while the whole linker - the span bounds, the
\ recorded chain values it compares them against, the offsets it records - works
\ in the same absolute integer domain. DATA is mapped MAP_FIXED at DATA-VA, so the
\ offset from data-base plus that base IS the address, by ordinary checked pointer
\ arithmetic: NOT a trust row, and deliberately not one. src/habu/aot-arm.f carries
\ the same one-liner for package AOT-ARM, which this file's package does not load.
: HERE-N ( -- n ) here BYTE-VIEW data-base BYTE-VIEW - DATA-VA VA>N + ;

: AOT-DATA-START ( -- )
   HERE-N BLOB-SRC !
   \ The retained compiler must intern this application's strings and trap
   \ messages inside the span the stripped image restores.
   NSTR:WINDOW-OPEN ;

\ Latch the end of the span. The string pool AOT-DATA-START opened STAYS the active
\ one while tools/aot-build-core.f loads the linker above BLOB-END, and that is
\ required, not merely tolerated.
\
\ THE POOL ACTIVE AT LINK TIME MUST BE THE APPLICATION'S. The closure is compiled
\ natively by the retained compiler when LINK runs, so a closure word's literals are
\ interned into whichever pool is active THEN - not into the pool that was active
\ when the source was read. A second NSTR:WINDOW-OPEN here, to give the linker's
\ literals a pool of their own, therefore puts every closure literal above BLOB-END,
\ where DATA-ADDRESS! refuses it: measured, an application requiring only
\ lib/string.f is rejected with `caller=STR-LEN value=BLOB-END+131137`, +131137
\ being the first body in that second pool. NSTR publishes no close, and it must
\ not be given one here.
\
\ THE COST, stated because it is real and unmeasured. The linker's own literals
\ consequently land in the application's pool and travel inside the span. The 64 KB
\ page round hides it today - plainapp is 65728 bytes both before and after this
\ change, as is an application using lib/string.f, lib/fs.f and lib/memory.f - so
\ nothing here is evidence that the cost is small, only that it is below one page
\ round for these programs. habu-report-where-an-cdcd7976 is where it gets measured.
: AOT-DATA-SPAN ( -- )
   HERE-N  BLOB-END !
   BLOB-END @ BLOB-SRC @ - dup 0 < IF s" aot: negative data span" 74 die THEN BLOB-LEN ! ;

\ Opening the window is a separate maker script from linking (tools/aot-build-open.f
\ and tools/aot-build-core.f), so linking without it is a reachable mistake rather
\ than an impossible one. It would emit an image whose data span is whatever a zero
\ BLOB-SRC implies, which is why this refuses instead of defaulting: a real span
\ starts at a DATA address and can never be zero.
: AOT-WINDOW-LATCHED ( -- )
   BLOB-SRC @ 0= BLOB-END @ 0= or IF
      s" aot: capture window was not opened before the application loaded" 74 die
   THEN ;

;package
