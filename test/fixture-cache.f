\ fixture-cache.f - retention for the keyed gate fixtures in the build cache.
\
\ test/keyed-image.f (for test/fixture-writer.f and test/preloaded-engine.f),
\ test/cold-engine.f and test/whitebox-engine.f each publish one image per key
\ into the build cache - hb-fixture-writer-<key>, hb-app-image-<key>,
\ hb-linker-<key>, hb-cold-<key>, hb-whitebox-<key> - built in a private work
\ directory beside it, <prefix>-<seed>-<attempt>. A key covers the engine and
\ the source closures the image is built from, so every engine rebuild or
\ closure edit publishes a new image beside the old ones, and a build killed
\ before its own cleanup (the gate pool kills a row at its deadline) leaves its
\ work directory behind. Nothing else removes either, so each of the three
\ calls PRUNE once it has published and closed its work directory.
\
\ AN ENTRY GOES ONCE NOTHING HAS USED IT FOR A DAY. Gates on other trees share
\ this cache and run images published under keys of their own: a writer,
\ preloaded host or linker key names its tree's canonical source paths
\ (test/keyed-image.f) and a cold key derives from the writer's, so every
\ checkout has its own, and a whitebox key differs wherever the engine or its
\ closure does. The images run in place, so keeping only the key just published
\ would pull images out from under them. Instead every ENSURE that finds its
\ image dates it to now through USED, so the mtime is the last time any caller
\ settled it, and PRUNE takes only entries whose mtime is RETAIN-SECONDS old: a
\ day, far beyond any build's deadline or any gate's run. A gate settles its
\ images before its pool starts and every row settles them again before using
\ them, so an image in use is never that old; a work directory is dated by its
\ build, which ends within minutes. Only a caller that last settled an image a
\ day ago and runs it again can lose it, and its ENSURE rebuilds it.
\
\ The image just published and the pruner's own claim directory stay whatever
\ their mtime says: a host suspended for a day in the middle of a build would
\ otherwise date the image.
\
\ CLAIMED BY RENAME. Two publishers can prune one family at once. PRUNE makes a
\ claim directory named like a work directory of the family and renames each
\ stale entry into it; a rename is atomic, so exactly one pruner owns each
\ entry, and the other finds the source gone and passes on. The claim directory
\ is registered with the exit registry and removed when the sweep ends; one a
\ killed pruner leaves is a stale work directory to the next sweep.
\
\ PRUNE NEVER FAILS ITS CALLER. It runs after a successful publish, and a gate
\ that has its image must not go red over housekeeping. An entry it cannot
\ claim is reported on fd 2 with its path and code and stays for the next
\ sweep, and a sweep that cannot run at all is reported the same way, after its
\ claim directory is removed: the registration that would have removed it at
\ exit may be what failed. USED likewise reports an image it cannot date and
\ lets the hit stand; only an image already gone makes its caller rebuild.
\
\ Only names these modules make are touched: the image prefix followed by
\ KEY-HEX-LEN hex digits, and the work prefix followed by -<seed>-<attempt>,
\ the two decimal numbers MAKE-TEMP-DIR writes. The root holds other caches too
\ - hb-build-out-<key> is the hb-build tool's - and those are not gate fixtures.
\ test/fixture-cache-test.f checks the age, refresh, family and shape rules
\ through a real publish.

require lib/errors.f
require lib/string.f
require lib/fmt.f
require lib/fs.f
require lib/fs-list.f
require lib/fs-mutate.f
require lib/build-cache.f
require lib/time.f

package FIXTURE-CACHE

86400 constant RETAIN-SECONDS
64 constant KEY-HEX-LEN
$2D constant DASH

TYPED-VARIABLE IMAGE-A ptr u8
TYPED-VARIABLE WORK-A ptr u8
TYPED-VARIABLE KEEP-A ptr u8
TYPED-VARIABLE USED-A ptr u8
create CLAIM-BUF FS-PATH-CAP allot
create SRC-BUF FS-PATH-CAP allot
create DST-BUF FS-PATH-CAP allot

variable IMAGE-U
variable WORK-U
variable KEEP-U
variable CLAIM-U
variable SRC-U
variable DST-U
variable CUTOFF
variable USED-U

: IMAGE$ ( -- ptr u8 n )
   IMAGE-A @ IMAGE-U @ ;

: WORK$ ( -- ptr u8 n )
   WORK-A @ WORK-U @ ;

: KEEP$ ( -- ptr u8 n )
   KEEP-A @ KEEP-U @ ;

: CLAIM$ ( -- ptr u8 n )
   CLAIM-BUF CLAIM-U @ ;

: SRC$ ( -- ptr u8 n )
   SRC-BUF SRC-U @ ;

: DST$ ( -- ptr u8 n )
   DST-BUF DST-U @ ;

: USED$ ( -- ptr u8 n )
   USED-A @ USED-U @ ;

: DIGIT? ( n -- bool ) {: c:n :}
   c $30 >= c $39 <= and ;

: HEX? ( n -- bool ) {: c:n :}
   c DIGIT?
   c $61 >= c $66 <= and or
   c $41 >= c $46 <= and or ;

\ The key an image name ends with.
: KEY-TAIL? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   u KEY-HEX-LEN <> if false exit then
   u 0 ?do
      a i + c@ HEX? 0= if false unloop exit then
   loop
   true ;

\ The index past the run of digits that starts at i.
: DIGITS-END ( ptr u8 n n -- n ) {: a:ptr u:n i:n :}
   i begin dup u < if a over + c@ DIGIT? else false then while 1+ repeat ;

\ -<seed>-<attempt>, what MAKE-TEMP-DIR puts after its prefix.
: TEMP-TAIL? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   u 4 < if false exit then
   a c@ DASH <> if false exit then
   a u 1 DIGITS-END {: mid:n :}
   mid 1 = mid u >= or if false exit then
   a mid + c@ DASH <> if false exit then
   a u mid 1+ DIGITS-END {: end:n :}
   end mid 1+ > end u = and ;

: FAMILY? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u IMAGE$ STARTS-WITH? if a IMAGE-U @ + u IMAGE-U @ - KEY-TAIL? exit then
   a u WORK$ STARTS-WITH? if a WORK-U @ + u WORK-U @ - TEMP-TAIL? exit then
   false ;

: KEPT? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u KEEP$ STR=
   a u CLAIM$ BASENAME STR= or ;

\ The entry's own mtime, never a link target's. An entry already gone is not
\ stale: another pruner has it.
: STALE? ( ptr u8 n -- bool )
   FS-TRY-LSTAT 0= if false exit then
   FS-STAT-MTIME-SEC@ CUTOFF @ < ;

\ One line on fd 2: fixture-cache: cannot <verb> <path>: <code>.
: REPORT ( ptr u8 n ptr u8 n n -- ) {: v:ptr vu:n a:ptr u:n code:n :}
   SB-RESET
   s" fixture-cache: cannot " SB-APPEND
   v vu SB-APPEND
   s"  " SB-APPEND
   a u SB-APPEND
   s" : " SB-APPEND
   code FMT:SB-INT
   S\" \n" SB-APPEND
   2 SB$ write drop ;

\ The rename is the claim. When it fails and the source is gone, another pruner
\ claimed the entry first; any other failure is reported by the caller.
: CLAIM ( -- )
   SRC$ STALE? 0= if exit then
   [: SRC$ DST$ RENAME-FILE ;] catch {: code:n :}
   code 0= if exit then
   SRC$ FS-TRY-LSTAT if code throw then ;

: VISIT ( ptr u8 n -- ) {: a:ptr u:n :}
   a u FAMILY? 0= if exit then
   a u KEPT? if exit then
   BUILD-CACHE:ROOT$ a u SRC-BUF JOIN-PATH SRC-U !
   CLAIM$ a u DST-BUF JOIN-PATH DST-U !
   [: CLAIM ;] catch {: code:n :}
   code 0<> if s" prune" SRC$ code REPORT then ;

: SWEEP ( -- )
   BUILD-CACHE:ROOT$ WORK$ MAKE-TEMP-DIR {: a:ptr u:n :}
   u FS-PATH-CAP > if E-FS-CAPACITY throw then
   a CLAIM-BUF u BYTE-COPY
   u CLAIM-U !
   CLAIM$ CLEANUP-TREE+
   BUILD-CACHE:ROOT$ [: VISIT ;] FS-LIST:EACH
   CLAIM$ REMOVE-TREE ;

\ After a sweep that stopped early: its exit registration may be what failed.
: DROP-CLAIM ( -- )
   CLAIM-U @ 0 = if exit then
   CLAIM$ FS-TRY-LSTAT 0= if exit then
   [: CLAIM$ REMOVE-TREE ;] catch {: code:n :}
   code 0<> if s" remove" CLAIM$ code REPORT then ;

public

\ Remove every entry of one family that nothing has used for RETAIN-SECONDS:
\ the image name prefix, the prefix its work directories were made with, and
\ the image just published, which stays. Failures are reported, never thrown.
: PRUNE ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: image:ptr imageu:n work:ptr worku:n keep:ptr keepu:n :}
   image IMAGE-A !
   imageu IMAGE-U !
   work WORK-A !
   worku WORK-U !
   keep keepu BASENAME KEEP-U ! KEEP-A !
   0 CLAIM-U !
   TIME:EPOCH-SECONDS RETAIN-SECONDS - CUTOFF !
   [: SWEEP ;] catch {: code:n :}
   code 0= if exit then
   DROP-CLAIM
   s" prune" BUILD-CACHE:ROOT$ code REPORT ;

\ Mark an image found on disk as in use by dating it to now, which is what keeps
\ PRUNE from taking it. TRUE when the hit stands: dated, or kept although it
\ cannot be dated (another owner, an immutable flag), which is reported on fd 2.
\ FALSE when the image is gone - a pruner took it after the caller's check - so
\ the caller builds it again.
: USED ( ptr u8 n -- bool )
   USED-U ! USED-A !
   [: USED$ TOUCH ;] catch {: code:n :}
   code 0= if true exit then
   USED$ FS-TRY-LSTAT 0= if false exit then
   s" date" USED$ code REPORT
   true ;

;package
