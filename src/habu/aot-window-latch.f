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
\ with the rest of the linker. tools/aot-build-open.f loads THIS file, opens the
\ window, loads the application, latches the span, and only then loads the linker
\ above it. src/habu/aot-decl.f is the other lib-free candidate and is deliberately
\ not used: it is baked into the engine (tools/bootstrap.sh SRC_COMMON,
\ tools/build-fixpoint.f BF-APPEND-COMMON), so a change there costs three
\ generations and buys nothing this file does not give.
require src/habu/layout.f
require src/habu/aot-owned-cells.f

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
\ environment and the dynamic-storage registry). A cell named there as CARRIED
\ gets neither: its bytes are COPIED into the carried run below, inside this span,
\ and travel in the blob. A cell on neither list is refused.
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

\ The dictionary record count tools/aot-build-open.f takes right after
\ requiring this file, before it loads anything an application can name. A
\ record above the engine's own seal watermark and below it was loaded by this
\ process ahead of the maker - on the engine this file's own private records,
\ on the keyed linker image the image's whole load (test/preloaded-engine.f
\ rule 3) - and aot-closure.f ADD-CLO refuses to carry one. Between this latch
\ and AOT-DATA-START the opener's baked requires add no record; the AOT-LINK
\ definitions it makes do, above the latch.
variable OPENER-NDICT

\ The window owns a literal pool, and the application owns it only while it
\ loads: a body interned into it lands inside the span and travels in the image.
\ AOT-DATA-START keeps the maker's active pool and opens the application's;
\ AOT-DATA-SPAN hands the maker its pool back, so nothing the maker compiles
\ after the application - tools/aot-build.f, which only the engine's maker
\ compiles, and the linker - adds a byte to the image (tools/hb-build-aot-test.f
\ BUILD-AOT-SPAN-ENGINE). AOT-APP-POOL selects the application's pool again for
\ the link, which copies in the literals it reaches in other pools: AOT-DATA-SPAN
\ closes the pool first, so it keeps room for them inside the span and opens no
\ segment past it.
PTR-VARIABLE MAKER-POOL
PTR-VARIABLE APP-POOL

: AOT-DATA-START ( -- )
   NSTR:ACTIVE MAKER-POOL !
   NSTR:WINDOW-OPEN
   NSTR:ACTIVE APP-POOL !
   \ The application owns the literal bodies, not the compiler's lookup tables.
   \ A segment's owner and row tables precede its arena. Start at the first
   \ segment's arena so its tables stay outside capture like the compiler's
   \ ACTIVE-P. A saved first-segment SOURCE-ROWS pointer then takes the ordinary
   \ unrestored-DATA refusal. A later segment, opened while the application
   \ loads, lies inside the span with its tables.
   0 NSTR:SOURCE-SPAN drop data-base BYTE-VIEW - DATA-VA VA>N + BLOB-SRC ! ;

\ Latch the application span before loading the linker, and hand the maker back
\ its literal pool. Keeping the application's pool active while the linker
\ loaded carried even fs-identity's three NUL-terminated symbol strings into an
\ empty MAIN image (HBT-SIZE-AOT).
\
\ THE CARRIED RUN: room inside the window for the engine constants the application
\ READS. An application that FORMATS an integer, PARSES one or HASHES a string
\ builds stripped because the constants those words reach are carried by name. A
\ baked table an application reaches - the i64 bound digits, SHA-256's round
\ constants - lives below the span, and the image can only get its bytes by
\ carrying them: src/habu/aot-lib.f CARRY-CELLS copies each cell named CARRIED in
\ src/habu/aot-owned-cells.f into this run at link time, and it is inside the
\ window, so the copy travels in the image's own data blob with no entry code and
\ no write below the window. It is reserved HERE, in the last moment DATA still
\ grows inside the span: nothing it names could be given a home after the span
\ is latched. The lib-free ownership visitor is already available for sizing;
\ live claims are collected after the application's cleanup. The
\ reserve is zeroed, so whatever no claim uses is invisible to aot-lib.f
\ EACH-BLOB-RUN and costs the image nothing but address space. WHAT THE CLAIMS DO
\ USE TRAVELS IN EVERY IMAGE: the copies are made for every link, reached or not,
\ and their non-zero bytes are blob content like any other window byte (measured:
\ a hello-world image carries 390 written data bytes, 64 without the copies).
\ docs/native-applications.md states the run's capacity and ownership.
\ The ownership list sizes this run from the same aligned extents it collects
\ after cleanup. Fresh cells need no copy; in-window declarations already travel
\ with the application. The image writes only nonzero bytes from the reserve.
variable CARRY-BYTES
variable CARRY-BASE
PTR-VARIABLE CARRY-P

: CARRY-BASE$ ( -- ptr u8 ) CARRY-P @ ;

\ Cell-aligned, because a carried table is read with `@` at the same interior
\ offsets it had in the engine (sha256's KK is 64 cells), and DATA grows by bytes.
: CARRY-RESERVE ( -- )
   HERE-N 7 and dup 0<> IF 8 swap - allot ELSE drop THEN
   here BYTE-VIEW CARRY-P !
   HERE-N CARRY-BASE !
   CARRY-BYTES @ allot
   CARRY-BYTES @ 0 ?do 0 CARRY-BASE$ i + c! loop ;

: AOT-DATA-SPAN ( -- )
   NSTR:WINDOW-CLOSE
   BLOB-SRC @ AOT-OWNED:CARRY-SIZE CARRY-BYTES !
   CARRY-RESERVE
   HERE-N  BLOB-END !
   BLOB-END @ BLOB-SRC @ - dup 0 < IF s" aot: negative data span" 74 die THEN BLOB-LEN !
   MAKER-POOL @ NSTR:SWITCH ;

\ The link's only interning: aot-closure.f MAPPED-DATA copies each literal the
\ closure reaches from another pool into the ACTIVE one, which must be the pool
\ inside the window.
: AOT-APP-POOL ( -- )
   APP-POOL @ NSTR:SWITCH ;

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
