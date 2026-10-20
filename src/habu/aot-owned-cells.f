\ aot-owned-cells.f - THE list of engine runtime cells a stripped image OWNS.
\
\ A stripped AOT image (src/habu/aot-lib.f) restores only the application's own
\ DATA window, so every engine cell below that window reads zero out of the fresh
\ anonymous mapping, and src/habu/aot-closure.f refuses an image whose code names
\ one: reading it would silently answer that zero.
\
\ SOME OF THOSE CELLS ARE NOT PERSISTED DATA AT ALL. They are RUNTIME INPUTS, the
\ way argv is - the environment vector the kernel hands this process - or state
\ that starts empty in every runtime instance. A stripped entry can own such a
\ cell exactly as it already owns x20, S0-CELL and DP-CELL: it publishes the ones
\ that carry a value and leaves the rest at the mapping's zero, which is what the
\ engine's own entry does (src/habu/habu2.f EM-DATA-INIT).
\
\ AND SOME ARE DATA THAT MUST TRAVEL. A baked table the application reads - the
\ digits of the i64 bounds, SHA-256's round constants, or FFI declaration
\ metadata - holds a value no entry can recompute and no fresh mapping can
\ supply. Such a cell is CARRIED:
\ the claim names it WITH ITS BYTE LENGTH, src/habu/aot-lib.f CARRY-CELLS copies
\ those bytes into the carried run inside the application's window
\ (src/habu/aot-window-latch.f CARRY-RESERVE, so the image writes them as part of
\ its own data blob), and aot-closure.f CARRIED-TARGET maps every spelled address
\ inside [cell, cell+length) to the copy plus its interior offset - the same
\ target map a re-interned literal goes through. No entry code runs for a carried
\ cell and nothing below the window is written.
\
\ THE LIST IS AT THE BOTTOM OF THIS FILE. A cell is on it because it is NAMED
\ there - never because of its value, its address, or the file it lives in.
\ aot-closure.f CLAIMED-CELL? admits the bytes a claim declares - one cell, or
\ the buffer a length names - and refuses every other cell below the window with
\ the diagnostic it always gave; aot-lib.f EMIT-OWNED-CELLS emits the
\ initialisation each claim declares. One table, two readers, so the walker and
\ the entry cannot disagree about a cell.
\
\ WHY THE LIST IS HERE AND NOT IN THE ENGINE, BESIDE THE CELLS IT NAMES. A claim
\ is a DATA offset, and an offset computed while the engine's own prefix loads
\ does not survive the capture that makes an engine: the prefix is compiled into
\ the BUILDING HOST's DATA, millions of bytes above that host's base, and the
\ image keeps only the captured span, so a cell claimed at prefix-load time
\ records the host's offset and not the image's. A code-spelled address is
\ relocated by the capture; a number in a raw cell is
\ not. The offset therefore has to be taken from the LIVE cell, in the process
\ that links - which is this one. It is the linker's question in any case: which
\ engine cells a stripped entry may own is AOT policy, and src/core and src/os do
\ not otherwise know that AOT exists.
\ These runtime owners are baked into the native engine. Loading libraries here
\ would put their data before the application's capture window.
require src/habu/layout.f
require src/habu/xref.f

package AOT-OWNED
private

\ This linker can be source-loaded by a previous-generation host.
$2D68 constant OWNED-SOURCE-CELL
$2D70 constant OWNED-SOURCE-SIZE-CELL

\ FFI controls and the table allocations latched at the application boundary
\ are visited together. Linker-only declarations may grow the live tables.
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

\ FRESH is 0, IMAGE-BASE is 1, ENTRY-XT is 2, TEXT-BASE is 3,
\ CARRIED-CELLS is 4 and CARRIED-BYTES is 5.
\ The kind is stored because it is what the entry emitter switches on; deriving
\ it back from the cell would be a second answer.
\
\ A claim records the cell as a DATA ADDRESS IN THE LINKER'S INTEGER DOMAIN - the
\ domain BLOB-SRC/BLOB-END and every recorded chain value live in
\ (src/habu/aot-window-latch.f HERE-N computes a cursor the same way). DATA is one
\ MAP_FIXED mapping based at DATA-VA, so the offset from data-base plus that base
\ IS the address, by ordinary checked pointer arithmetic; nothing here holds a
\ pointer in a raw cell (dot habu-refuse-a-ptr-5ad2734e).
\
\ EVERY CLAIM CARRIES A BYTE LENGTH AND EVERY READER READS IT: one cell for the
\ kinds that name a single cell, the declared extent for CARRIED and
\ FRESH-BYTES. The length is the claim's EXTENT - aot-closure.f CLAIMED-AT?
\ admits [cell, cell + length) whatever the kind, because a buffer is reached at
\ an interior offset as often as at its head - and for CARRIED it is also the
\ bytes to copy and the range a spelled address is mapped through.
: CLAIM ( ptr a n n -- ) {: c:ptr k:n len:n :}
   COUNT @ 1+ {: rows:n :}
   rows OFFS-RESERVE  rows KINDS-RESERVE
   rows LENS-RESERVE  rows DESTS-RESERVE
   c BYTE-VIEW data-base BYTE-VIEW - DATA-VA VA>N +  COUNT @ OFFS !
   k COUNT @ KINDS !
   len COUNT @ LENS !
   0 COUNT @ DESTS !
   COUNT @ 1+ COUNT ! ;

public

\ The fresh mapping's zero IS this cell's correct starting value, so the entry
\ publishes nothing for it: either the word that reads it writes it first within
\ one call, or zero is the empty state this engine itself boots with.
: FRESH ( ptr a -- ) 0 8 CLAIM ;

\ A BUFFER owned by the same rule, with its span declared: the entry publishes
\ nothing here either, and every address inside the declared bytes is admitted
\ because a buffer is reached at an interior offset as often as at its head. The
\ length is the declaration, as it is for CARRIED: nothing here measures a
\ `create` body from the dictionary.
: FRESH-BYTES ( ptr a n -- ) {: c:ptr len:n :}
   len 0 <= if s" aot: a fresh claim needs a positive byte length" 74 die then
   c 0 len CLAIM ;

\ The entry publishes this image's own DATA base in the cell - the value the
\ declaring file stores into it when the engine loads.
: IMAGE-BASE ( ptr a -- ) 1 8 CLAIM ;

\ The entry publishes the address of the word this image starts - the token an
\ application image's own source stores with APP-IMAGE:START!. A stripped image
\ has no entry record to store it, so the entry does it for itself.
: ENTRY-XT ( ptr a -- ) 2 8 CLAIM ;

\ The entry publishes this image's own text CONTENT base in the cell - the value
\ the engine's own entry stores there (src/habu/habu2.f EM-DATA-INIT), which is
\ the address of the image's first instruction.
: TEXT-BASE ( ptr a -- ) 3 8 CLAIM ;

\ THE NAMED BYTES TRAVEL. The linker copies [cell, cell+length) into the window's
\ carried run and records where it put them; the walker maps an address inside
\ that range to the copy. The entry publishes nothing - the bytes arrive in the
\ image's data blob like the application's own. The length is the declaration:
\ nothing here measures a `create` body from the dictionary.
: CARRIED-CELLS ( ptr a n -- ) {: c:ptr len:n :}
   len 0 <= len CELL mod 0<> or if
      s" aot: a carried cell claim needs whole cells" 74 die then
   c 4 len CLAIM ;

: CARRIED-BYTES ( ptr a n -- ) {: c:ptr len:n :}
   len 0 <= if s" aot: a carried claim needs a positive byte length" 74 die then
   c 5 len CLAIM ;

\ FFI and TASK are baked into the C2 engine. An application's own require may
\ instead load either one inside its window. Claim a field only when its source
\ is entirely below the window; in-window DATA already travels with the image.
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
: IMAGE-BASE? ( n -- bool ) KINDS @ 1 = ;
: ENTRY-XT? ( n -- bool ) KINDS @ 2 = ;
: TEXT-BASE? ( n -- bool ) KINDS @ 3 = ;
: CARRIED? ( n -- bool ) {: i:n :} i KINDS @ 4 = i KINDS @ 5 = or ;
: CARRIED-CELLS? ( n -- bool ) KINDS @ 4 = ;

\ Where the linker put a carried claim's copy, in the same integer domain: zero
\ until CARRY-CELLS runs, which is before anything maps an address into it.
: DEST ( n -- n ) DESTS @ ;
: DEST! ( n n -- ) {: at:n i:n :} at i DESTS ! ;

private

CAST: ENV-CELLS-XT ( n -- [ [ ptr u8 -- ] -- ] )

: ENV-CELLS ( -- )
   \ A bootstrap donor can predate the hook and therefore owns no observer.
   s" ENV-INPUT:OWNED-CELLS" XREF-FIND
   dup XREF-FOUND? 0= if drop exit then
   XREF-START ENV-CELLS-XT [: FRESH ;] swap execute ;

\ Visit the same extents when reserving the carried run and collecting claims.
\ Only collection inspects live process resources or publishes claim rows.
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

\ ---- THE LIST -----------------------------------------------------------------
\ The engine's environment (src/os/env-base.f). ENV-DATA-PTR holds the DATA base
\ that ARGC-CELL/ARGV-CELL/ENVP-CELL are read through, so the entry republishes it
\ together with those three startup cells themselves. GETENV and ENV=? keep
\ their lookup cursors on each invocation's stack.
\ ENV-INPUT's observer starts empty: source tracking belongs to the build process.
\
\ The dynamic-storage registry (src/core/dynamic-storage.f), which every
\ DYNAMIC-BUFFER declaration - and so src/lib/memory.f MEM:WITH-BYTES - reaches, is
\ named through its own package. Its three cells are private and stay private: a
\ private word of a baked package is reachable from outside it neither by
\ qualified name nor by reopening the package (measured 2026-09-18: E-UNDEFINED
\ for both spellings). DYNAMIC-STORAGE:OWNED-CELLS hands each of the three to the
\ claim below in turn, which keeps the rule intact - a cell is on this list
\ because a name put it there.
\
\ src/lib/memory.f's WITH-BYTES scope stack (MEM:OWNED-CELLS) is on the list for the
\ same two reasons: the file is baked into the engine, so its cells sit below
\ every window, and its three cells - the two DYNAMIC-BUFFER control heads and the
\ scope depth - are correct at zero in a runtime that has allocated nothing yet.
\ The registry claim above is not enough on its own: a WITH-BYTES call names these
\ three before it ever reaches DYNAMIC-STORAGE (measured 2026-09-18: with only the
\ registry claimed, the refusal moved 400 bytes past MUTEX, to WB-BUFFERS).
\
\ APP-ENTRY:XT-CELL (src/habu/layout.f) is the cell that says THIS PROCESS IS AN
\ APPLICATION AND NOT THE ENGINE. An image saved with APP-IMAGE:START! writes the
\ entry word's token into it, and the words that ask the question read it there:
\ src/os/script-argv.f SCRIPT-ARG-START takes the application convention -
\ argument 0 is argv[1] - exactly when it is non-zero. A stripped image is an
\ application with no entry record to write it, so its entry writes its own: the
\ address of the word the image starts, which is the same token by the same
\ meaning. Left at the mapping's zero the image answered the engine's source-list
\ convention and reported its SECOND argument as argument 0.
\
\ EXIT-HOOK-CELL (src/habu/layout.f) is the process-exit vector the stripped
\ entry calls before its own exit(0). ZERO IS ITS CORRECT START - a new process
\ has registered no hook - and the library that wants one arms it inside the
\ running image: src/lib/fs-mutate.f stores its exit hook from the same CLEANUP+
\ call that registers a path, so an application that registers nothing leaves
\ the vector at the mapping's zero and exits exactly as it did before. The claim
\ is what lets a stripped image NAME the cell: without it the arm word's
\ `data-base EXIT-HOOK-CELL +` is an address below the window and the image is
\ refused, so a stripped application could register a temp tree and never clean
\ it - the very leak the vector exists to close.
\
\ THE CALLBACK BAND (src/habu/layout.f CB-XDS, CB-BAND-BYTES) is eight cells
\ every region carries, each correct at zero: no published frame, a free owner,
\ no marshal frame, and no slot table until src/lib/ffi-callback.f ENTRY and
\ src/lib/task.f CONTEXT-BIND publish theirs in the running image. The FFI cores and
\ the callback thunk reach the band x20-relative; the claim is for the source
\ that spells it, src/lib/ffi-callback.f's `data-base CB-FRAME +`.
\
\ RBASE-CELL (src/habu/layout.f) is the live text CONTENT base - the address of
\ the image's first instruction, which the engine's own entry stores at boot
\ (src/habu/habu2.f EM-DATA-INIT) and the `rbase` primitive reads back. It is a
\ runtime input exactly as argv is: one process's text base is no other's. NO
\ CODE SPELLS THE CELL'S ADDRESS, so neither the closure walk's address scan nor
\ the window's cell scan can reach it - `rbase` compiles inline as an
\ x20-relative load - and an image that left it at the mapping's zero was built
\ without a word of complaint and crashed when it ran.
\
\ EVERY FOREIGN CALL IN A STRIPPED IMAGE GOES THROUGH IT. src/os/linux/layout.f
\ DLOPEN-SLOT/DLSYM-SLOT locate the loader's two GOT slots as rbase - CODE-OFF +
\ the text size in the image header + the slot offset, and the image carries
\ those slots: src/os/linux/elf.f emits a dlopen and a dlsym R_AARCH64_GLOB_DAT
\ for them in every image it writes. With the cell at zero, the getpid reduction
\ (`FUNCTION: GETPID-CALL getpid ( -- n ) ;FUNCTION`) read address -$FA0 and took
\ SIGSEGV inside DLSYM-SLOT's `@`; with the cell published the same image
\ resolves the symbol and runs.
\
\ THE i64 BOUND DIGITS (src/lib/string.f) are the first carried cells: STR-PARSE-POS
\ compares its input against STR-MAX-I64$ and STR-PARSE-NEG against
\ STR-MIN-I64$, both STR-I64-DIGITS bytes, and src/lib/fmt.f INT>NUM COPIES
\ STR-MIN-I64$ for the one integer with no positive magnitude. src/lib/string.f is
\ baked, so both tables sit below every window and any program that formats or
\ parses an integer reaches one.
\
\ SHA-256 (src/core/sha256.f) is baked because its read-only KK and HH0
\ tables serve digest contexts owned by callers. The digest implementation
\ itself keeps no mutable cell below the window.
\ THE IMAGE-LIFECYCLE REGISTRY (src/lib/image-lifecycle.f) is the second site a
\ stripped image is refused at, once the path scratch below lets it open a file:
\ its private lock, its two counters and the bases of its two hook tables,
\ claimed fresh for the reason the DYNAMIC-STORAGE registry's three cells are - a
\ new process has registered nothing, so an open lock, two zero counts, no hook
\ mapping and an empty persistent table are its correct start, and that is the
\ zero a fresh anonymous mapping holds. IMAGE-LIFECYCLE:COUNT takes the lock and
\ reads both counters; REGISTER, REGISTER-PERSISTENT and PREPARE spell the two
\ bases, and nothing spells a cell behind either base, because both accessors
\ compute the slot by arithmetic. Registering stores a quotation through `xt!`,
\ whose declaration half a stripped image drops at link (aot-closure.f
\ AOT-DECLARATION?); before that drop the claims below would have admitted the
\ cells and the build was still refused at `site=xt!`.
\ PZB (src/core/util.f) is the tree's one path scratch, claimed fresh with its
\ PATH-CAP + 1 bytes: PATHZ writes the caller's path and its NUL into it and
\ PATH0 hands it to the caller within that one call, so every byte it reads it
\ wrote first.
\
\ NOT ON THE LIST, and refused as loudly as before, is every other engine cell
\ below the window. src/os/env-base.f's own TMP-PATH cursors and buffer (TPB, TPP,
\ TPQ, TPS, TPU) are the nearest miss: same file, same transient character, no
\ claim - so a stripped program calling TMP-PATH still gets the
\ outside-the-restored-span refusal, and tools/hb-build-stripped-test.f
\ HBT-STRIPPED-UNOWNED-CELL pins that. TPB is a `create` table like the carried
\ ones and is not carried either: a table travels because it is named here.
public

\ Collect after the application's one-shot lifecycle cleanup, when the window
\ and every live declaration belong to this link. A saved linker loads this
\ module before it opens its later application's window.
: COLLECT ( n -- ) {: window:n :}
   window ENGINE-WINDOW !
   0 COUNT !
   ENV-DATA-PTR IMAGE-BASE
   ENV-CELLS
   [: FRESH ;] DYNAMIC-STORAGE:OWNED-CELLS
   [: FRESH ;] MEM:OWNED-CELLS
   [: FRESH ;] IMAGE-LIFECYCLE:OWNED-CELLS
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
