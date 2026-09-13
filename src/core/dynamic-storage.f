\ dynamic-storage.f - checked allocation behind DYNAMIC-BUFFER declarations.
\ A control record contains a mapping pointer and its byte capacity. Element
\ identity and width are fixed by the declaration that owns that record.
\
\ CORE-PREFIX FILE, and that is why the mapping refusals below are this
\ package's own codes rather than the memory codes lib/errors.f owns. The
\ declaration surface that generates calls into here is
\ src/core/layout-buffer.f, which the prefix loads before
\ src/core/lower-cert-seal.f takes the core-prefix mark; a generated build
\ source rewinds to that mark (src/habu/prefix-rewind.f) and then compiles
\ engine files that declare DYNAMIC-BUFFERs, so a runtime living above the mark
\ is undefined exactly where it is needed (measured: `E-UNDEFINED:
\ DYNAMIC-STORAGE:RESERVE` compiling src/habu/aot-decl.f in every hb-build
\ stage source). lib/errors.f loads after the mark, so reaching a code out of it
\ from here is what put this file above the mark.
package DYNAMIC-STORAGE
private

$7FFFFFFFFFFFFFFF constant MAX-BYTES
7121 constant E-SIZE
7138 constant E-MAP                      \ mmap refused the growth this reserve asked for
7139 constant E-UNMAP                    \ munmap refused a mapping this package owns

\ ---- the registry of every control record this package owns -------------------
\ WHY A REGISTRY EXISTS AT ALL. An AOT capture copies the window's DATA, so a
\ control record that still holds a mapping when the capture runs bakes a pointer
\ into a dead process beside a capacity that is real. RESERVE's early return on
\ `need <= old` then never replaces the mapping and the generated reader's bounds
\ check passes, so the next generation dereferences it: measured rc 134, SIGSEGV,
\ x11 holding the building process's mmap address. Releasing at the end of the
\ window's load is not enough, because anything that compiles after that point
\ reserves again; the only complete answer is one list the capture can walk, and
\ the only complete list is one the declaring form builds.
\
\ THE REGISTRY IS PROCESS-LOCAL AND NOTHING ABOUT IT SURVIVES AN IMAGE. It lives
\ in a mapping of its own, and the two DATA cells below are a pointer and a count
\ in the pairing every pointer slot in this tree uses: the count is the authority,
\ so `REG-U` at 0 means there is no mapping and the pointer is not read. RELEASE-ALL
\ gives the mapping back and zeroes both cells last of all, after the walk, so an
\ image carries neither the registry nor anything it named. A booted image whose
\ first declaration re-registers builds the registry again from nothing, which is
\ the same state a cold process starts in.
\
\ ONE REGISTRY PER PACKAGE INSTANCE, AND THAT IS THE POINT. A window build loads
\ this file a second time (measured: `src/core/dynamic-storage.f` loads twice in one
\ tools/native-build.f run), so the window's declarations register into the window's
\ instance and the build host's into the host's. The capture walks the window's
\ instance and only that one - see src/habu/aot-capture.f ACAP-RELEASE-DYNAMIC -
\ because the host's records sit below the captured DATA window and are never baked.
$40 constant REG-INIT                    \ entries the first registry mapping holds
1 constant REG-HEAD                      \ cells ahead of the first entry: the capacity

: EXTENT ( n n -- n ) {: count:n width:n :}
   count 0 < width 0 <= or if E-SIZE throw then
   count MAX-BYTES width / > if E-SIZE throw then
   count width * ;

: CAPACITY ( n n -- n ) {: need:n old:n :}
   old MAX-BYTES 2 / > if need exit then
   need old 2 * 64 max max ;

: COPY ( ptr n ptr n n -- ) {: src:ptr dst:ptr bytes:n :}
   bytes CELL / 0 ?do src i cells + @ dst i cells + ! loop ;

create REG 0 ,                           \ the registry mapping; read only when REG-U is non-zero
variable REG-U                           \ entries in use, and the authority on whether REG is live
variable REG-I
variable REG-DIRTY

: REG-BYTES ( n -- n ) {: entries:n :}
   REG-HEAD entries + cells ;

: REG@ ( -- ptr n )  REG 0 ptr-field @ ;

: REG-CAP ( -- n )   REG@ @ ;

: REG-AT ( n -- ptr n ) {: i:n :}
   REG@ REG-HEAD i + cells + ;

: REG-ENTRY ( n -- ptr n ) {: i:n :}
   i REG-AT 0 ptr-field @ ;

: REG-MAP ( n -- ptr n ) {: entries:n :}
   entries REG-BYTES map-anon 0< if drop E-MAP throw then ;

\ Opened by the first REGISTER and never observed empty: the caller stores an entry
\ and raises the count before it returns, and a grow cannot be reached on that first
\ entry, so `REG-U` 0 always means no mapping.
: REG-OPEN ( -- )
   REG-INIT REG-MAP {: fresh:ptr :}
   REG-INIT fresh !
   fresh REG 0 ptr-field ! ;

\ The old mapping is given back only after the copy succeeded, and a refused unmap
\ gives the fresh one back rather than leaving two live: REG still names the old one.
: REG-GROW ( -- )
   REG-CAP {: old:n :}
   old 2 * {: cap:n :}
   cap REG-MAP {: fresh:ptr :}
   REG@ fresh  REG-HEAD old + cells  COPY
   cap fresh !
   REG@ old REG-BYTES munmap 0< if
      fresh cap REG-BYTES munmap drop E-UNMAP throw
   then
   fresh REG 0 ptr-field ! ;

\ EVERY CELL THIS PACKAGE OWNS, and that is the whole point of the word: the two
\ walk cursors are as much a build-time transient as the mapping is. Left at the
\ entry count they reached, they would be baked into every image as residue - a
\ deterministic number, so not a byte-identity break, but a build-time count in a
\ cell no restored image has a reader for, which is exactly what lengthens the
\ generation chain (docs/bootstrap.md). When this returns, DYNAMIC-STORAGE's DATA
\ is zero in all four cells.
: REG-CLOSE ( -- )
   REG@ REG-CAP REG-BYTES munmap 0< if E-UNMAP throw then
   NULL-PTR REG 0 ptr-field !
   0 REG-U !
   0 REG-I !
   0 REG-DIRTY ! ;

public

: RESERVE ( n ptr n n -- ) {: count:n cb:ptr width:n :}
   count width EXTENT {: need:n :}
   cb cell+ @ {: old:n :}
   need old <= if exit then
   need old CAPACITY {: cap:n :}
   cap map-anon 0< if drop E-MAP throw then {: fresh:ptr :}
   old 0 > if
      cb 0 ptr-field @ fresh old COPY
      cb 0 ptr-field @ old munmap 0< if
         fresh cap munmap drop E-UNMAP throw
      then
   then
   fresh cb 0 ptr-field !
   cap cb cell+ ! ;

: RELEASE ( ptr n -- ) {: cb:ptr :}
   cb cell+ @ {: cap:n :}
   cap 0 > if
      cb 0 ptr-field @ cap munmap 0< if E-UNMAP throw then
   then
   0 cb ! 0 cb cell+ ! ;

\ Called once per declaration, by the source the declaring form generates
\ (src/core/layout-buffer.f DBUF-SOURCE), so no caller keeps a list by hand and a
\ new declaration cannot be forgotten. A record enters exactly once and stays: a
\ RELEASE leaves it registered and zero, which is what the walk below wants to see.
: REGISTER ( ptr n -- ) {: cb:ptr :}
   REG-U @ 0= if REG-OPEN then
   REG-U @ REG-CAP >= if REG-GROW then
   cb  REG-U @ REG-AT 0 ptr-field !
   REG-U @ 1 + REG-U ! ;

\ How many records this instance has registered. A booted image answers 0: the
\ capture gave the registry back, so a non-zero answer before the first declaration
\ of a process would mean an image carrying one - which is what test/dynamic-buffer-
\ registry.f reads it for.
: REGISTERED-N ( -- n )  REG-U @ ;

\ How many registered records still hold something. The walk asserts on it, and a
\ test reads it directly: after RELEASE-ALL it answers 0 because there is no
\ registry left to hold a record, which is the same statement.
: DIRTY-N ( -- n )
   0 REG-DIRTY !
   0 REG-I !
   begin REG-I @ REG-U @ < while
      REG-I @ REG-ENTRY {: cb:ptr :}
      cb @ 0 <> cb cell+ @ 0 <> or if REG-DIRTY @ 1 + REG-DIRTY ! then
      REG-I @ 1 + REG-I !
   repeat
   REG-DIRTY @ ;

\ THE CAPTURE'S ONE CALL. Releases every mapping this instance handed out, zeroes
\ both cells of every record it named, and gives the registry itself back last, so
\ the DATA the capture is about to copy holds no mapping, no capacity and no
\ registry. The DIRTY-N assertion is not decoration: RELEASE zeroes a record it
\ could unmap, so a record still holding something here means a mapping this
\ package owns did not come back, which is E-UNMAP's subject.
: RELEASE-ALL ( -- )
   REG-U @ 0= if exit then
   0 REG-I !
   begin REG-I @ REG-U @ < while
      REG-I @ REG-ENTRY RELEASE
      REG-I @ 1 + REG-I !
   repeat
   DIRTY-N 0 <> if E-UNMAP throw then
   REG-CLOSE ;

;package
