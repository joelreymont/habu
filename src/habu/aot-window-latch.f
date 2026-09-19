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

\ Latch the application span before loading the linker. tools/aot-build.f saves
\ its active NSTR owner, opens a separate pool for the linker, and switches back
\ before LINK. Closure compilation therefore interns into the application pool;
\ DATA-TARGET's REINTERN-OWNED copies any reached literal from another pool into
\ it. Linker-only literals stay above BLOB-END. Keeping the application pool
\ active during linker loading carried even fs-identity's three NUL-terminated
\ symbol strings into an empty MAIN image (HBT-SIZE-AOT).
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
