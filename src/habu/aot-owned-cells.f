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
\ THE LIST IS AT THE BOTTOM OF THIS FILE. A cell is on it because it is NAMED
\ there - never because of its value, its address, or the file it lives in.
\ aot-closure.f OWNED-CELL? admits a named cell and refuses every other cell below
\ the window with the diagnostic it always gave; aot-lib.f EMIT-OWNED-CELLS emits
\ the initialisation each claim declares. One table, two readers, so the walker
\ and the entry cannot disagree about a cell.
\
\ WHY THE LIST IS HERE AND NOT IN THE ENGINE, BESIDE THE CELLS IT NAMES. A claim
\ is a DATA offset, and an offset computed while the engine's own prefix loads
\ does not survive the capture that makes an engine: the prefix is compiled into
\ the BUILDING HOST's DATA, millions of bytes above that host's base, and the
\ image keeps only the captured span, so a cell claimed at prefix-load time
\ records the host's offset and not the image's. Measured 2026-09-18 on a
\ generation built with the claims in src/os/env-base.f: ENV-QU claimed at load
\ reported offset 16893592 in an engine whose ENV-QU lives at 5466416. A
\ code-spelled address is relocated by the capture; a number in a raw cell is
\ not. The offset therefore has to be taken from the LIVE cell, in the process
\ that links - which is this one. It is the linker's question in any case: which
\ engine cells a stripped entry may own is AOT policy, and src/core and src/os do
\ not otherwise know that AOT exists.
require src/os/env-base.f
require src/core/dynamic-storage.f
require lib/memory.f
require src/habu/layout.f

package AOT-OWNED
private

\ Generous headroom over a list that names single cells; a list that outgrows it
\ dies by name rather than running past the table.
64 constant MAX-CELLS
create OFFS MAX-CELLS cells allot
create KINDS MAX-CELLS cells allot
variable COUNT

\ FRESH is 0 and IMAGE-BASE is 1. The kind is stored because it is what the entry
\ emitter switches on; deriving it back from the cell would be a second answer.
\
\ A claim records the cell as a DATA ADDRESS IN THE LINKER'S INTEGER DOMAIN - the
\ domain BLOB-SRC/BLOB-END and every recorded chain value live in
\ (src/habu/aot-window-latch.f HERE-N computes a cursor the same way). DATA is one
\ MAP_FIXED mapping based at DATA-VA, so the offset from data-base plus that base
\ IS the address, by ordinary checked pointer arithmetic; nothing here holds a
\ pointer in a raw cell (dot habu-refuse-a-ptr-5ad2734e).
: CLAIM ( ptr a n -- ) {: c:ptr k:n :}
   COUNT @ MAX-CELLS >= if s" aot: owned-cell list exceeds its table" 74 die then
   c BYTE-VIEW data-base BYTE-VIEW - DATA-VA VA>N +  COUNT @ cells OFFS + !
   k COUNT @ cells KINDS + !
   COUNT @ 1+ COUNT ! ;

public

\ The fresh mapping's zero IS this cell's correct starting value, so the entry
\ publishes nothing for it: either the word that reads it writes it first within
\ one call, or zero is the empty state this engine itself boots with.
: FRESH ( ptr a -- ) 0 CLAIM ;

\ The entry publishes this image's own DATA base in the cell - the value the
\ declaring file stores into it when the engine loads.
: IMAGE-BASE ( ptr a -- ) 1 CLAIM ;

: N ( -- n ) COUNT @ ;
: AT ( n -- n ) cells OFFS + @ ;
: IMAGE-BASE? ( n -- bool ) cells KINDS + @ 1 = ;

private

\ ---- THE LIST -----------------------------------------------------------------
\ The engine's environment (src/os/env-base.f). ENV-DATA-PTR holds the DATA base
\ that ARGC-CELL/ARGV-CELL/ENVP-CELL are read through, so the entry republishes it
\ together with those three startup cells themselves; the rest are GETENV's and
\ ENV=?'s own cursors, each written from the caller's arguments before it is read.
\
\ The dynamic-storage registry (src/core/dynamic-storage.f), which every
\ DYNAMIC-BUFFER declaration - and so lib/memory.f MEM:WITH-BYTES - reaches, is
\ named through its own package. Its three cells are private and stay private: a
\ private word of a baked package is reachable from outside it neither by
\ qualified name nor by reopening the package (measured 2026-09-18: E-UNDEFINED
\ for both spellings). DYNAMIC-STORAGE:OWNED-CELLS hands each of the three to the
\ claim below in turn, which keeps the rule intact - a cell is on this list
\ because a name put it there.
\
\ lib/memory.f's WITH-BYTES scope stack (MEM:OWNED-CELLS) is on the list for the
\ same two reasons: the file is baked into the engine, so its cells sit below
\ every window, and its three cells - the two DYNAMIC-BUFFER control heads and the
\ scope depth - are correct at zero in a runtime that has allocated nothing yet.
\ The registry claim above is not enough on its own: a WITH-BYTES call names these
\ three before it ever reaches DYNAMIC-STORAGE (measured 2026-09-18: with only the
\ registry claimed, the refusal moved 400 bytes past MUTEX, to WB-BUFFERS).
\
\ NOT ON THE LIST, and refused as loudly as before, is every other engine cell
\ below the window. src/os/env-base.f's own TMP-PATH cursors and buffer (TPB, TPP,
\ TPQ, TPS, TPU) are the nearest miss: same file, same transient character, no
\ claim - so a stripped program calling TMP-PATH still gets the
\ outside-the-restored-span refusal, and tools/hb-build-test.f
\ HBT-STRIPPED-UNOWNED-CELL pins that.
: LIST ( -- )
   ENV-DATA-PTR IMAGE-BASE
   ENV-Z  FRESH
   ENV-A  FRESH
   ENV-U  FRESH
   ENV-QA FRESH
   ENV-QU FRESH
   [: FRESH ;] DYNAMIC-STORAGE:OWNED-CELLS
   [: FRESH ;] MEM:OWNED-CELLS ;

LIST
;package
