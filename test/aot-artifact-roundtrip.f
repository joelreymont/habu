\ aot-artifact-roundtrip.f - src/habu/aot-file.f's writer and reader over a real
\ capture, in a booted bin/hb (dot habu-retire-the-s-4fbc244f).
\
\ WHY THE CAPTURE IS A SMALL WINDOW AND NOT THE CHAIN. The product provides every
\ file the compiler chain's closure names, so `require` is a no-op there and the
\ chain's window comes up empty - tools/aot-chain-capture.f says so by name and
\ only the build's capture host can run it. The FORMAT does not care how large the
\ window was: what it needs exercised is a capture with a non-empty blob, compact
\ records, a name pool, a DATA window with runs in it, a protected-WID row and a
\ closure list. A window of two words, a defer, a written cell and a sealed
\ wordlist has all of those, and this file is the only check in the tree that
\ carries src/habu/aot-file.f through a load at all.
\
\ THE MARKS COME FIRST, then the window, then the artifact writer - the same order
\ tools/aot-chain-capture.f is built in and for the same reasons: the band below
\ the mark is what the capture may call into, and the writer's own buffers must be
\ allotted past the window's last DATA byte so no window word can hold one.
\
\ Save the complete address rows, clear their buffer and the restored counts,
\ then compare the rows after READ. Digest equality alone cannot detect a writer
\ and reader that both truncate the same rows.
\
\ THE PRODUCER KEY is sha256 of the engine running this, so the parent suite can
\ hash bin/hb from the outside and compare against the key in the header. READ is
\ handed the same key, which is what makes the header's producer check pass; a
\ different key here would be refused by name and is test/aot-chain-capture-suite.f's
\ business rather than a mode of this file.
\
\ Run standalone from the repository root:
\   bin/hb --load test/aot-artifact-roundtrip.f -- /tmp/small.aot

package AOTRT
public
ndict@ here  variable PRE-R  variable PRE-D  PRE-D !  PRE-R !
;package

require src/arch/arm64/asm.f
require src/arch/arm64/icode.f
require src/habu/layout.f
require src/habu/aot-decl.f
require src/habu/aot-arm.f
require src/habu/aot-capture.f
require src/compiler/native/string.f

AOT-ARM:WINDOW-OPEN
NSTR:WINDOW-OPEN

package AOTRT-WINDOW
public

\ DATA of the window's own, written at load time so the sparse run table has a
\ run to carry and the DATA span is not empty.
create WCELL 8 allot
7 WCELL !

\ This window's own public wordlist id, latched here and sealed below, so the
\ capture has a protected-WID row of its own to carry: those rows travel
\ window-relative and the section that carries them is one of the five version 6
\ renumbered.
variable WWID
get-current WWID !

: LEAF ( n -- n ) {: v:n :} v 3 * 1 + ;

\ Nothing calls it, and nothing needs to: what the format has to carry is a record
\ and a body in the blob, and a capture takes the whole window whether or not this
\ process ever runs what is in it. Its callee is in the same window, so the
\ capture's call audit has nothing to refuse and nothing to record by name - an
\ in-window call is a relative branch inside the blob that travels with it.
: TRUNK ( n -- n )
   LEAF LEAF LEAF LEAF LEAF LEAF LEAF LEAF ;

\ A DECLARED ADDRESS CELL OF THE WINDOW'S OWN. `is` stores an execution token into
\ a DATA cell, which is what registers a SNAP-RELOC:XTCELL row, and a row whose
\ cell and whose target are both inside the window is the population
\ AOT-CAPTURE:DECLARED-IN counts. Without one the predicate below would hold over
\ two zeroes and prove nothing, so ?XTCELLS refuses a window that has none.
defer HOOK ( n -- n )
: ARM-HOOK ( -- ) [: LEAF ;] is HOOK ;
ARM-HOOK

;package

AOTRT-WINDOW:WWID @ prot-wid-add
AOT-ARM:WINDOW-CLOSE

\ The artifact writer and the identity it needs, both past the window's last
\ record and last DATA byte.
require src/habu/aot-ident.f
require src/habu/fdio.f
require src/habu/aot-file.f

package AOTRT
using AOT-BUF
using AOT-WINDOW
public

$4C constant REFUSE-RC
32 constant SHA-BYTES

create KEY SHA-BYTES allot
create SHA-A SHA-BYTES allot
DYNAMIC-BUFFER SAVED-XTOFF-STORAGE n
: SAVED-XTOFFS ( -- ptr u8 ) 0 SAVED-XTOFF-STORAGE BYTE-VIEW ;
variable SAVED-XTOFF-N

: ART$ ( -- ptr u8 n ) 0 SCRIPT-ARGV$ ;

: ?ARGS ( -- )
   SCRIPT-ARGC 1 >= SCRIPT-ARGC 2 <= and if exit then
   s" aot-artifact-roundtrip: expected an artifact path and optional row-test case"
   REFUSE-RC die ;

\ The engine running this, by the relative path the suite spawns it with;
\ lib/engine-id.f answers the same question with a resolved path and cannot be
\ loaded here. SHA256-FILE's status is checked, so a run from the wrong directory
\ refuses by name instead of writing a key of zeroes into the header.
: ENGINE$ ( -- ptr u8 n ) s" bin/hb" ;

: KEY! ( -- )
   ENGINE$ KEY SHA256-FILE 0 = if exit then
   s" aot-artifact-roundtrip: cannot hash the engine that is running" REFUSE-RC die ;

\ The chain digest READ re-derives from disk, so the list has to name files that
\ are there. Two of the capture's own sources, which is also what the closure
\ section's walk is asked to reproduce.
: CLOSURE! ( -- )
   AOT-IDENT:RESET
   s" src/habu/aot-decl.f" AOT-IDENT:PATH+
   s" src/habu/aot-file.f" AOT-IDENT:PATH+ ;

: CAPTURE ( -- )
   PRE-R @ PRE-D @ AOT-CAPTURE:PRELUDE-MARK
   AOT-ARM:WINDOW$ AOT-CAPTURE:CAPTURE ;

\ Every count the format restores from a section length, cleared so the read has
\ to produce it. The two bases and the window's own wordlist base go with them:
\ READ brings its own.
: FORGET-COUNTS ( -- )
   0 AOT-BLOB-LEN !  0 AOT-REC-N !  0 AOT-SITE-N !  0 AOT-NAMES-LEN !
   0 AOT-DSITE-N !  0 AOT-CSITE-N !  0 XTOFF-N !
   WINDOW-RESET
   0 AOT-XTSITE:N !  0 AOT-BOOTRUN-LEN !  0 AOT-PWIN-N !
   0 AOT-SIG-N !  0 AOT-SIG-STR-LEN !  0 AOT-REG-LEN !
   0 AOT-DATA-SIZE !  0 AOT-DATA-D0 !  0 AOT-CODE-B0 !
   0 AOT-WID-W0 !  0 AOT-WID-SPAN ! ;

: SAME? ( ptr u8 ptr u8 n -- bool ) {: a:ptr b:ptr n:n :}
   n 0 ?do a i + c@ b i + c@ <> if 0 0= 0= unloop exit then loop  0 0= ;

: ?ROUND-TRIP ( -- )
   SHA-A AOT-FILE:SHA$ drop SHA-BYTES SAME? if exit then
   s" aot-artifact-roundtrip: the artifact does not survive its own round trip"
   REFUSE-RC die ;


: SAVE-XTOFFS ( -- )
   XTOFF-N @ dup SAVED-XTOFF-STORAGE-RESERVE SAVED-XTOFF-N !
   XTOFF-BUF@ SAVED-XTOFFS
   SAVED-XTOFF-N @ XTOFF-ROW * BYTE-COPY ;


: CLEAR-XTOFFS ( -- )
   XTOFF-N @ XTOFF-ROW * 0 ?do
      0 XTOFF-BUF@ i + c!
   loop ;


: ?XTOFFS-RESTORED ( -- )
   XTOFF-N @ SAVED-XTOFF-N @ = if
      XTOFF-BUF@ SAVED-XTOFFS
      SAVED-XTOFF-N @ XTOFF-ROW * SAME? if exit then
   then
   s" aot-artifact-roundtrip: the complete address rows were not restored"
   REFUSE-RC die ;

\ THE PREDICATE tools/aot-chain-capture.f ?XTOFF asserts, over a window this process
\ really captured: the row table is the declared address cells inside the window
\ plus the cells outside it whose target is inside their kind's span, and nothing
\ else. The tool's own copy of it can only run in the build's capture host, so this
\ is where the two counters are executed. The first refusal is what keeps the
\ predicate from passing over two zeroes on a window that declared no cell at all.
: ?XTCELLS ( -- )
   AOT-ARM:D0 @ AOT-ARM:D1 @ AOT-CAPTURE:DECLARED-IN {: win:n :}
   AOT-ARM:B0 @ AOT-ARM:B1 @ AOT-ARM:D0 @ AOT-ARM:D1 @ AOT-CAPTURE:TARGETED-OUT {: out:n :}
   win 0 = if
      s" aot-artifact-roundtrip: this window declared no address cell, so the row count proves nothing"
      REFUSE-RC die
   then
   XTOFF-N @ win out + = if exit then
   s" aot-artifact-roundtrip: declared address cell rows=" type XTOFF-N @ .
   s" inside the window=" type win .
   s" outside it targeting it=" type out . cr
   s" aot-artifact-roundtrip: a declared address cell row belongs to neither population"
   REFUSE-RC die ;

: ?RESTORED ( -- )
   AOT-REC-N @ 0 >  AOT-BLOB-LEN @ 0 >  and
   CELL-N @ 0 >  and  AOT-DATA-SIZE @ 0 >  and
   AOT-PWIN-N @ 0 >  and  AOT-NAMES-LEN @ 0 >  and if exit then
   s" aot-artifact-roundtrip: the read left the capture empty" REFUSE-RC die ;

: REPORT ( -- )
   s" roundtrip: recs=" type AOT-REC-N @ .
   s" sites=" type AOT-SITE-N @ .
   s" cells=" type CELL-N @ .
   s" pwin=" type AOT-PWIN-N @ .
   s" xtcells=" type XTOFF-N @ .
   s" dataspan=" type AOT-DATA-SIZE @ . cr
   \ MAIN checked both disjoint live populations and restored every row byte.
   s" xtcells=declared+targeted xtcells-restored=exact" type cr
   s" roundtrip=ok" type cr ;

: MAIN ( -- )
   ?ARGS
   \ The two-argument row fixture reuses this capture without running this case.
   SCRIPT-ARGC 2 = if exit then
   KEY!
   CLOSURE!
   CAPTURE
   ?XTCELLS
   SAVE-XTOFFS
   KEY ART$ AOT-FILE:WRITE
   AOT-FILE:SHA$ drop SHA-A SHA-BYTES BYTE-COPY
   CLEAR-XTOFFS
   FORGET-COUNTS
   KEY ART$ AOT-FILE:READ
   ?XTOFFS-RESTORED
   ?RESTORED
   KEY ART$ AOT-FILE:WRITE
   ?ROUND-TRIP
   REPORT ;

;using
;using
;package

AOTRT:MAIN
