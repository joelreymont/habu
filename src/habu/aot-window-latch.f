\ aot-window-latch.f - the stripped capture window's span cells and the words
\ that latch it. It requires no lib module: a library loaded when the window
\ opens is the copy the application's `require` resolves to, its cells sit below
\ the span, and aot-closure.f DATA-ADDRESS! refuses the image. tools/aot-builder.f
\ loads this file, opens the window, loads the application, latches the span,
\ then loads the linker above it. aot-decl.f is lib-free but baked into the engine.
require src/habu/layout.f
require src/habu/aot-owned-cells.f

package AOT-LINK

\ The span is the program's compile-time data, contiguous from the DATA pointer
\ latched before user compilation to `here`; the source buffer and assembler
\ CODE buffer are separate mmaps. aot-lib.f emits the span into __text and the
\ entry copies it back to the same fixed DATA-VA. Other runtime cells stay zero
\ from the anonymous mmap, and zero is a value only where declared: x20, S0-CELL
\ and DP-CELL get explicit init, as does every cell in aot-owned-cells.f
\ (aot-link-arm.f EMIT-OWNED-CELLS). A cell named there as CARRIED is copied into the
\ carried run inside this span instead. A cell on neither list is refused.
\
\ The span bounds are DATA addresses as integers; nothing dereferences them.
\ aot-closure.f DATA-CELL@ is where a DATA address becomes a pointer again.
variable BLOB-SRC  variable BLOB-END  variable BLOB-LEN

\ The DATA cursor in the linker's absolute integer domain. aot-capture.f
\ AOT-DBASE-N reads a numeric primitive; `here` is a pointer, so this computes:
\ DATA is mapped at the fixed DATA-VA, so offset plus base IS the address, by
\ checked pointer arithmetic and deliberately not a trust row. aot-arm.f carries
\ the same one-liner for package AOT-ARM, which this package does not load.
: HERE-N ( -- n ) here BYTE-VIEW data-base BYTE-VIEW - DATA-VA VA>N + ;

\ The dictionary record count tools/aot-builder.f refreshes immediately before
\ AOT-DATA-START; every fixed definition a saved maker restores is below it. A
\ record above the engine's seal and below it was loaded ahead of the
\ application (on the engine this file's private records, on a saved maker its
\ support, on the keyed linker image its whole load). The closure refuses to
\ carry a reachable pre-window word that the engine does not bake.
variable OPENER-NDICT
variable WINDOW-CODE-START

\ The application owns the window's literal pool only while it loads: a body
\ interned into it lands inside the span and travels in the image.
\ AOT-DATA-START keeps the maker's pool and opens the application's;
\ AOT-DATA-SPAN closes it and hands the maker its pool back, so nothing compiled
\ after the application adds a byte to the image. AOT-APP-POOL selects the
\ application's pool again for the link, which copies in the literals it reaches
\ in other pools; the closed pool keeps room for them inside the span.
PTR-VARIABLE MAKER-POOL
PTR-VARIABLE APP-POOL

: AOT-DATA-START ( -- )
   cp@ WINDOW-CODE-START !
   FFI:IMAGE-UNPIN
   NSTR:ACTIVE MAKER-POOL !
   NSTR:WINDOW-OPEN
   NSTR:ACTIVE APP-POOL !
   \ The application owns the literal bodies, not the lookup tables: a segment's
   \ owner and row tables precede its arena, so the span starts at the first
   \ segment's arena and its tables stay outside capture, as ACTIVE-P does. A
   \ saved first-segment SOURCE-ROWS pointer takes the unrestored-DATA refusal. A
   \ later segment, opened while the application loads, lies inside the span.
   0 NSTR:SOURCE-SPAN drop data-base BYTE-VIEW - DATA-VA VA>N + BLOB-SRC ! ;

\ Latch the span before loading the linker and hand the maker its pool back:
\ with the application's pool still active, the linker carried fs-identity's
\ three NUL-terminated symbol strings into an empty MAIN image (HBT-SIZE-AOT).
\
\ THE CARRIED RUN: room inside the window for baked engine constants the
\ application reads (the i64 bound digits, SHA-256's round constants), which live
\ below the span. aot-common.f CARRY-CELLS copies each cell named CARRIED in
\ aot-owned-cells.f into this run at link time, so the copy travels in the
\ image's data blob with no entry code and no write below the window. It is
\ reserved here, the last moment DATA grows inside the span. The ownership list
\ sizes it from the same aligned extents it collects after cleanup; fresh cells
\ need no copy and in-window declarations already travel. The reserve is zeroed
\ and only nonzero bytes are written, but the copies are made for every link,
\ reached or not (measured: a hello-world image carries 390 written data bytes,
\ 64 without them). docs/architecture.md "Images" states what the run carries.
variable CARRY-BYTES
variable CARRY-BASE
PTR-VARIABLE CARRY-P
variable FFI-FN-MARK
variable FFI-LIB-MARK
PTR-VARIABLE FFI-FN-BASE-MARK
PTR-VARIABLE FFI-LIB-BASE-MARK
variable FFI-FN-CAP-MARK
variable FFI-LIB-CAP-MARK
variable TASK-USER-MARK

: CARRY-BASE$ ( -- ptr u8 ) CARRY-P @ ;

\ Cell-aligned, because a carried table is read with `@` at the same interior
\ offsets it had in the engine (sha256's KK is 64 cells), and DATA grows by bytes.
: CARRY-RESERVE ( -- )
   HERE-N 7 and dup 0<> IF 8 swap - allot ELSE drop THEN
   here BYTE-VIEW CARRY-P !
   HERE-N CARRY-BASE !
   CARRY-BYTES @ allot ;

: AOT-DATA-SPAN ( -- )
   NSTR:WINDOW-CLOSE
   TASK:#USER TASK-USER-MARK !
   FFI:IMAGE-MARK
   FFI-LIB-BASE-MARK ! FFI-FN-BASE-MARK !
   FFI-LIB-MARK ! FFI-FN-MARK !
   FFI:IMAGE-CAPS FFI-LIB-CAP-MARK ! FFI-FN-CAP-MARK !
   FFI-FN-BASE-MARK @ FFI-FN-CAP-MARK @
   FFI-LIB-BASE-MARK @ FFI-LIB-CAP-MARK @ FFI:IMAGE-PIN
   FFI-FN-BASE-MARK @ FFI-FN-CAP-MARK @
   FFI-LIB-BASE-MARK @ FFI-LIB-CAP-MARK @ AOT-OWNED:FFI-TABLES!
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

\ Opening the window is separate from linking (tools/aot-builder.f OPEN and
\ RUN), so linking without it is reachable. A real span never starts at zero:
\ a zero BLOB-SRC is refused rather than emitted.
: AOT-WINDOW-LATCHED ( -- )
   BLOB-SRC @ 0= BLOB-END @ 0= or IF
      s" aot: capture window was not opened before the application loaded" 74 die
   THEN ;

\ The image restores no mapping, so every dynamic control record in the span is
\ released before the link copies it, as aot-capture.f ACAP-RELEASE-DYNAMIC does
\ for the other captures (src/core/dynamic-storage.f RELEASE-RANGE). Records
\ declared outside the span (the maker's, the linker's above it) stay live.
\ Without it a CODEGEN:BUFFER filled at load time was refused for its text
\ mapping (test/ffi-callback-file-cache.f).
: AOT-DATA-RELEASE ( -- )
   data-base BYTE-VIEW BLOB-SRC @ DATA-VA VA>N - + BLOB-LEN @
   DYNAMIC-STORAGE:RELEASE-RANGE ;

;package
