\ aot-owned-cells.f - the engine cells below the window a stripped image owns.
\ aot-closure.f refuses every other below-window cell an image names: it would read the fresh
\ mapping's zero. A cell is owned because a claim here names it, never by value, address or file;
\ aot-closure.f CLAIMED-CELL? and aot-link-arm.f EMIT-OWNED-CELLS read this one table.
\ Offsets come from the live cell in the linking process: one taken while the engine prefix loads
\ records the building host's DATA offset, which the capture does not relocate.

\ No other requires: a library loaded here would put its data before the application's window.
require src/habu/layout.f
require src/habu/xref.f

package AOT-OWNED
private

\ This linker can be source-loaded by a previous-generation host.
$2D68 constant OWNED-SOURCE-CELL
$2D70 constant OWNED-SOURCE-SIZE-CELL

\ The FFI tables as latched at the application boundary; linker-only declarations may grow the
\ live ones.
DYNAMIC-BUFFER OFFS n
DYNAMIC-BUFFER KINDS n
DYNAMIC-BUFFER LENS n
DYNAMIC-BUFFER DESTS n
variable COUNT
variable ENGINE-WINDOW
PTR-VARIABLE FFI-FN-TABLE
PTR-VARIABLE FFI-LIB-TABLE
variable FFI-FN-CAP
variable FFI-LIB-CAP

\ Kinds: FRESH 0, ENTRY-XT 1, TEXT-BASE 2, CARRIED-CELLS 3, CARRIED-BYTES 4; the
\ entry emitter switches on the stored kind. Runtime inputs (argv, environment, per-instance state)
\ are FRESH or published by the entry; CARRIED cells hold baked values no entry can recompute.
\ OFFS is a DATA address in the linker's integer domain, that of BLOB-SRC/BLOB-END: offset from
\ data-base plus DATA-VA. No raw cell holds a pointer.
\ LENS is the claim's extent: aot-closure.f CLAIMED-AT? admits [cell, cell+len) for every kind, since
\ a buffer is reached at interior offsets; for CARRIED it is also the bytes copied. The length is
\ declared: nothing measures a `create` body from the dictionary.
: CLAIM ( ptr a n n -- ) {: c:ptr k:n len:n :}
   COUNT @ 1+ {: rows:n :}
   rows OFFS-RESERVE  rows KINDS-RESERVE
   rows LENS-RESERVE  rows DESTS-RESERVE
   c NULL-PTR -  data-base NULL-PTR - -  DATA-VA VA>N +  COUNT @ OFFS !
   k COUNT @ KINDS !
   len COUNT @ LENS !
   0 COUNT @ DESTS !
   COUNT @ 1+ COUNT ! ;

public

\ Zero is the cell's correct start: its reader writes it first within one call, or zero is the
\ engine's own boot state. The entry publishes nothing.
: FRESH ( ptr a -- ) 0 8 CLAIM ;

: FRESH-BYTES ( ptr a n -- ) {: c:ptr len:n :}
   len 0 <= if s" aot: a fresh claim needs a positive byte length" 74 die then
   c 0 len CLAIM ;

\ The entry publishes the xt of the image's start word, which a full image stores with
\ APP-IMAGE:START!; a stripped image has no entry record to store it.
: ENTRY-XT ( ptr a -- ) 1 8 CLAIM ;

\ The entry publishes the image's text content base, as habu2.f EM-DATA-INIT does for the engine.
: TEXT-BASE ( ptr a -- ) 2 8 CLAIM ;

\ aot-common.f CARRY-CELLS copies the bytes into the window's carried run (sized by
\ aot-window-latch.f CARRY-RESERVE); aot-closure.f CARRIED-TARGET maps addresses in the range to
\ the copy. The entry publishes nothing.
: CARRIED-CELLS ( ptr a n -- ) {: c:ptr len:n :}
   len 0 <= len CELL mod 0<> or if
      s" aot: a carried cell claim needs whole cells" 74 die then
   c 3 len CLAIM ;

: CARRIED-BYTES ( ptr a n -- ) {: c:ptr len:n :}
   len 0 <= if s" aot: a carried claim needs a positive byte length" 74 die then
   c 4 len CLAIM ;

\ FFI and TASK are baked into C2, but an application may load either inside its window: claim a
\ field only when it lies wholly below the window.
: ENGINE-DATA? ( ptr u8 n -- bool ) {: c:ptr len:n :}
   c BYTE-VIEW data-base BYTE-VIEW - DATA-VA VA>N + len +
   ENGINE-WINDOW @ <= ;

: ENGINE-CELLS ( ptr u8 n -- )
   2dup ENGINE-DATA? if CARRIED-CELLS else 2drop then ;

: ENGINE-BYTES ( ptr u8 n -- )
   2dup ENGINE-DATA? if CARRIED-BYTES else 2drop then ;

: ENGINE-FRESH ( ptr u8 n -- )
   2dup ENGINE-DATA? if FRESH-BYTES else 2drop then ;

: N ( -- n ) COUNT @ ;
: AT ( n -- n ) OFFS @ ;
: LEN ( n -- n ) LENS @ ;
: ENTRY-XT? ( n -- bool ) KINDS @ 1 = ;
: TEXT-BASE? ( n -- bool ) KINDS @ 2 = ;
: CARRIED? ( n -- bool ) {: i:n :} i KINDS @ 3 = i KINDS @ 4 = or ;
: CARRIED-CELLS? ( n -- bool ) KINDS @ 3 = ;

\ Where CARRY-CELLS put the copy, same domain; zero until it runs, before any address maps into it.
: DEST ( n -- n ) DESTS @ ;
: DEST! ( n n -- ) {: at:n i:n :} at i DESTS ! ;

private

CAST: ENV-CELLS-XT ( n -- [ [ ptr u8 -- ] -- ] )

: ENV-CELLS ( -- )
   \ A bootstrap donor can predate the hook and therefore owns no observer.
   s" ENV-INPUT:OWNED-CELLS" XREF-FIND
   dup XREF-FOUND? 0= if drop exit then
   XREF-START ENV-CELLS-XT [: FRESH ;] swap execute ;

\ CARRY-SIZE and COLLECT visit the same extents through this walk; only COLLECT publishes claim
\ rows or inspects live process resources.
: TABLES ( [ ptr u8 n -- ] [ ptr u8 n -- ] [ ptr u8 n -- ] -- )
   {: cell-keep byte-keep fresh :}
   STR-MAX-I64$ STR-I64-DIGITS byte-keep execute
   STR-MIN-I64$ STR-I64-DIGITS byte-keep execute
   KK BYTE-VIEW 64 cells cell-keep execute
   HH0 BYTE-VIEW 8 cells cell-keep execute
   cell-keep fresh FFI:AOT-CONTROLS
   fresh FFI:MAKER-FRESH-CONTROLS
   FFI-FN-TABLE @ FFI-FN-CAP @ FFI-LIB-TABLE @ FFI-LIB-CAP @
   cell-keep byte-keep FFI:AOT-TABLES-AT
   cell-keep fresh TASK:OWNED-CELLS ;

variable CARRY-TOTAL

: CARRY-LEN ( ptr u8 n -- )
   {: c:ptr len:n :}
   c len ENGINE-DATA? if
      len 7 + 8 / 8 * CARRY-TOTAL +!
   then ;

public

: FFI-TABLES! ( ptr u8 n ptr u8 n -- )
   {: fn:ptr fncap:n lib:ptr libcap:n :}
   fn FFI-FN-TABLE !
   fncap FFI-FN-CAP !
   lib FFI-LIB-TABLE !
   libcap FFI-LIB-CAP ! ;

: CARRY-SIZE ( n -- n )
   ENGINE-WINDOW !
   0 CARRY-TOTAL !
   [: CARRY-LEN ;] [: CARRY-LEN ;] [: 2drop ;] TABLES
   CARRY-TOTAL @ ;

private

\ ARGC-CELL/ARGV-CELL/ENVP-CELL are reached through data-base (src/os/env-base.f) and published by
\ the entry out of the registers: nothing to claim. GETENV and ENV=? keep their cursors on the
\ stack. ENV-INPUT's observer
\ starts empty: source tracking belongs to the build process.
\ DYNAMIC-STORAGE's five cells, its DYNAMIC-BUFFER and ARENA registries, stay private: a private
\ word of a baked package is unreachable by qualified name and by reopening the package
\ (E-UNDEFINED for both), so DYNAMIC-STORAGE:OWNED-CELLS hands them over.
\ APP-ENTRY:XT-CELL non-zero means this process is an application: src/os/script-argv.f
\ SCRIPT-ARG-START then takes argv[1] as argument 0. Left at zero, a stripped image reported its
\ second argument as argument 0.
\ EXIT-HOOK-CELL is zero until a library arms it (src/lib/fs-mutate.f FS:CLEANUP+). Unclaimed, the
\ arm word's address is below the window and a stripped image that registers a temp tree is refused.
\ The callback band (layout.f CB-XDS, CB-BAND-BYTES) is eight cells correct at zero until
\ src/lib/ffi-callback.f ENTRY and src/lib/task.f CONTEXT-BIND publish theirs; the cores reach it
\ x20-relative, the claim is for ffi-callback.f's `data-base CB-FRAME +`.
\ RBASE-CELL: `rbase` is an x20-relative load, so no code spells the cell and neither closure scan
\ reaches it; left at zero an image linked cleanly and crashed. Every foreign call depends on it:
\ src/os/linux/layout.f DLOPEN-SLOT/DLSYM-SLOT locate the loader's GOT slots from rbase.
\ The i64 bound digits (src/lib/string.f, baked) are read by STR-PARSE-POS, STR-PARSE-NEG and
\ src/lib/fmt.f INT>NUM: any program that parses or formats an integer reaches them.
\ SHA-256's KK and HH0 (src/core/sha256.f) are read-only tables serving caller-owned contexts.
\ IMAGE-LIFECYCLE (src/lib/image-lifecycle.f): its lock, three counters and table bases are correct
\ at zero in a new process; accessors reach slots behind a base by arithmetic, so only bases are
\ spelled.
\ PZB (src/core/util.f) is the one path scratch, PATH-CAP + 1 bytes: PATHZ writes it and PATH0
\ hands it over within one call.
\ Every other below-window engine cell is refused, including env-base.f's TMP-PATH buffers (TPB
\ TPP TPQ TPS TPU): tools/hb-build-stripped-test.f HBT-STRIPPED-UNOWNED-CELL pins it.
public

\ Collect after the application's one-shot lifecycle cleanup, when the window
\ and every live declaration belong to this link. A saved linker loads this
\ module before it opens its later application's window.
: COLLECT ( n -- ) {: window:n :}
   window ENGINE-WINDOW !
   0 COUNT !
   ENV-CELLS
   [: FRESH ;] DYNAMIC-STORAGE:OWNED-CELLS
   [: FRESH-BYTES ;] IMAGE-LIFECYCLE:OWNED-CELLS
   data-base APP-ENTRY:XT-CELL + ENTRY-XT
   data-base EXIT-HOOK-CELL + FRESH
   data-base OWNED-SOURCE-CELL + FRESH
   data-base OWNED-SOURCE-SIZE-CELL + FRESH
   data-base CB-XDS + CB-BAND-BYTES FRESH-BYTES
   data-base RBASE-CELL + TEXT-BASE
   window TASK:CHECK-OWNED
   [: ENGINE-CELLS ;] [: ENGINE-BYTES ;] [: ENGINE-FRESH ;] TABLES
   PZB PATH-CAP 1 + FRESH-BYTES ;

;package
