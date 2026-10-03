\ build-cache.f - the checked build cache: the root every build shares, and the
\ retention that bounds what builds leave in it.
\
\ ROOT$ is the root, selected once per process (docs/stdlib.md). The rest of
\ this header is the retention: USED and PRUNE, which every writer into the
\ cache calls, and the work directories WORK-OPEN gives builds that publish by
\ rename.
\
\ EVERY ENTRY IS KEYED. A writer publishes <prefix><key><suffix>, the key 64 hex
\ digits covering what the entry was made from: hb-build its artifacts,
\ hb-build-out-<key> (tools/hb-build-lib.f), and its object cache, <key>.hbo and
\ <key>.idx (lib/object-resolve.f); the gate its keyed images (test/keyed-image.f,
\ test/cold-engine.f, test/whitebox-engine.f) and its NBR package unit
\ (test/native-unit-image.f). A key moves with every edit to what it covers, so
\ each publish lands beside the entries it replaces, and nothing but PRUNE
\ removes one.
\
\ AN ENTRY GOES ONCE NOTHING HAS USED IT FOR A DAY. Builds on other trees share
\ the cache under keys of their own, and some run an entry in place, so keeping
\ only the key just published would pull entries out from under them. Instead
\ every caller that finds an entry dates it to now through USED, so its mtime is
\ the last time anything used it, and PRUNE takes only entries RETAIN-SECONDS
\ old: a day, far beyond any build's deadline or any gate's run, so an entry in
\ use is never that old. Only a caller that last used an entry a day ago and
\ uses it again can lose it. When a prune renames the entry away before USED
\ dates it, USED answers FALSE and the caller builds the entry again. A prune
\ whose STALE? read the entry before USED dated it still renames it after, so
\ USED answers TRUE and the entry is gone when the caller uses it. The file
\ families take a use that fails while their entry is gone as a miss: the
\ hb-build artifact copy (tools/hb-build-lib.f), the object and its index
\ record (lib/object-resolve.f, lib/object-index.f). A keyed image is run or
\ copied by path, so that interleaving fails the row that uses it, and the next
\ run builds the image again.
\
\ A FAMILY'S LEFTOVERS GO THE SAME WAY. A build killed before its own cleanup
\ (the gate pool kills a row at its deadline) leaves what it was writing: the
\ <name>.tmp-<seed>-<attempt> an ATOMIC-WRITE-FILE reserves beside an entry, or
\ the <work>-<seed>-<attempt> directory MAKE-TEMP-DIR made under the family's
\ own stem. Each is dated by the build that made it, which ends within minutes.
\
\ A WORK DIRECTORY IS HELD WHILE ITS BUILD LIVES. WORK-OPEN makes
\ build-cache-work-<family>-<seed>-<attempt> in the root, beside the entry the
\ build will rename into place, so the publish never crosses a filesystem. The
\ build holds it by an exclusive flock on the directory itself, which the
\ kernel drops only when the last descriptor on it closes: the build's, and
\ every child's that inherited it, such as the builder that writes into the
\ directory. A build removes its own before it lets go (WORK-CLOSE, or the exit
\ registry after a die), and a reboot drops every lock, so a directory nobody
\ holds is a killed build's, and WORK-OPEN first removes every such directory.
\ No pid is read: a reused one proves nothing either way.
\
\ A CACHE ROOT IS ONE KERNEL'S. A lock is seen only by the kernel that keeps
\ it, so a root shared with another kernel is not supported.
\
\ TAKEN ONLY UNDER THE LOCK. A reaper takes a directory only while it holds
\ the lock and once it has checked that the path still names the directory it
\ locked. So two reapers never take one; none takes one from under a build that
\ is publishing or removing it; and a build that made its directory and had
\ not locked it yet finds, once it holds the lock, that the path is gone, and
\ makes another. A filesystem that cannot lock a directory fails WORK-OPEN with
\ E-FS-IO, since a build there would be unprotected; a reaper reports such a
\ directory and leaves it.
\
\ CLAIMED BY RENAME. Two publishers can prune one family at once. PRUNE makes a
\ claim directory, build-cache-claim-<seed>-<attempt>, beside the family and
\ renames each stale entry into it before removing it there. A rename is atomic,
\ so exactly one pruner owns each entry; the other finds the source gone and
\ passes on. A claim directory a killed pruner leaves is a stale entry of every
\ family a day later.
\
\ WHAT IS NOT OURS STAYS. Only names of the family's shapes are touched. An
\ entry that cannot be claimed (another owner under a sticky root, an immutable
\ flag, a directory without write permission) stays where it is. A claimed entry
\ that cannot be removed - something inside it is not ours to delete - is
\ renamed back under its own name, so the claim never holds, and never deletes,
\ what could not be removed; the rest of it, stale and ours, is gone.
\ A cache root is per-user: an entry USED cannot date, such as another user's
\ file, keeps its old mtime, so any publisher's prune can still take it.
\
\ PRUNE NEVER FAILS ITS CALLER. It runs after a successful publish, and a build
\ that has its entry must not fail over housekeeping. Each entry it cannot
\ claim, remove or put back is reported on fd 2 with its path and code and left
\ for the next sweep, and a sweep that cannot run at all is reported the same
\ way. USED likewise reports an entry it cannot date and lets the hit stand.
\
\ AGE IS ONE HOST'S WALL CLOCK AGAINST THE FILESYSTEM'S MTIMES, so a wall-clock
\ step forward of RETAIN-SECONDS or more during a run can make entries still in
\ use stale to another process's sweep. Only the entry just published and the
\ pruner's own claim directory stay whatever their mtime says, and only in that
\ pruner's own sweep: a host suspended for a day in the middle of a build would
\ otherwise age the entry before its publisher uses it.

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/fmt.f
require lib/fs.f
require lib/fs-list.f
require lib/fs-root.f
require lib/fs-mutate.f
require lib/fs-identity.f
require lib/ffi-abi.f
require lib/time.f

package BUILD-CACHE
public

ENUM source none explicit xdg home tmp ;ENUM

private

$2F constant SLASH

create ROOT-BUF FS-PATH-CAP allot
1 LAYOUT-BUFFER SOURCE-BUF source

TYPED-VARIABLE SELECT-A ptr u8
variable SELECT-CAP
variable SELECT-U
variable ROOT-U
variable OVERRIDE?
variable READY?
variable SELECTED-FLAG
variable CAUSE-CODE

: TRUE ( -- bool )
   0 0= ;

: FALSE ( -- bool )
   TRUE 0= ;

: ROOT-BYTES ( -- ptr u8 n )
   ROOT-BUF ROOT-U @ ;

: SELECT-A@ ( -- ptr u8 )
   SELECT-A @ ;

: SELECT-A! ( ptr u8 -- )
   SELECT-A ! ;

: SELECT-ALLOC ( n -- ptr u8 )
   MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop ;

\ Storage flows through MEM:ALLOC-BYTES / MEM:RELEASE-BYTES.
: SELECT-RELEASE ( ptr u8 n -- ) {: a:ptr cap:n :}
   a cap MEM:BYTES-ALLOC-LEN MEM:RELEASE-BYTES ;

\ Install the new span, then release the prior one; the release is LAST, so an
\ alloc that throws leaves the old span owned. No copy: growing invalidates the
\ previous bytes and every caller overwrites what it asked for - SELECT-COPY! and
\ SELECT-JOIN! both BYTE-COPY into the returned pointer, SELECTED>ROOT copies out
\ to the static ROOT-BUF, and no caller sources from the buffer it is growing.
: SELECT-BUF ( n -- ptr u8 ) {: need:n :}
   SELECT-CAP @ need < if
      SELECT-A@ {: old:ptr :}
      SELECT-CAP @ {: oldcap:n :}
      need SELECT-ALLOC SELECT-A!
      need SELECT-CAP !
      oldcap 0 > if old oldcap SELECT-RELEASE then
   then
   SELECT-A@ ;

\ RESET frees the span so the next selection allocates from nothing: a growth
\ measured after RESET does not depend on the roots this process selected before.
: SELECT-FREE ( -- )
   SELECT-CAP @ {: cap:n :}
   cap 0 > if SELECT-A@ cap SELECT-RELEASE 0 SELECT-CAP ! then ;

: SELECT-BYTES ( -- ptr u8 n )
   SELECT-A@ SELECT-U @ ;

: FAIL ( n -- )
   CAUSE-CODE !
   FALSE READY? !
   E-BUILD-PATH throw ;

: ROOT-CAUSE? ( n -- bool )
   dup E-FS-PATH =
   over E-FS-STAT = or
   over E-FS-DIR = or
   over E-FS-IO = or
   swap E-FS-CAPACITY = or ;

: FAIL-ROOT ( n -- )
   dup ROOT-CAUSE? 0= if throw then
   FAIL ;

: ROOT-COPY! ( ptr u8 n -- ) {: a:ptr u:n :}
   0 ROOT-U !
   u 0 <= if E-FS-PATH FAIL then
   u FS-PATH-CAP > if E-FS-CAPACITY FAIL then
   a ROOT-BUF u BYTE-COPY
   u ROOT-U ! ;

: SELECT-COPY! ( ptr u8 n -- ) {: a:ptr u:n :}
   0 SELECT-U !
   u 0 < if E-FS-PATH FAIL then
   u 0= if exit then
   a u SELECT-BUF u BYTE-COPY
   u SELECT-U ! ;

: SELECT-JOIN-NEED ( ptr u8 n n -- n ) {: a:ptr u:n suffixu:n :}
   suffixu MEM-MAX-N u - > if E-FS-CAPACITY FAIL then
   u suffixu +
   a u 1 - + c@ SLASH <> if
      dup MEM-MAX-N = if E-FS-CAPACITY FAIL then
      1+
   then ;

: SELECT-JOIN! ( ptr u8 n ptr u8 n -- )
   {: a:ptr u:n suffix:ptr suffixu:n :}
   0 SELECT-U !
   u 0 <= if E-FS-PATH FAIL then
   suffixu 0 < if E-FS-PATH FAIL then
   a u suffixu SELECT-JOIN-NEED {: need:n :}
   need SELECT-BUF {: dst:ptr :}
   a dst u BYTE-COPY
   a u 1 - + c@ SLASH = if
      suffix dst u + suffixu BYTE-COPY
   else
      SLASH dst u + c!
      suffix dst u 1 + + suffixu BYTE-COPY
   then
   need SELECT-U ! ;

: SELECTED>ROOT ( -- )
   SELECT-BYTES ROOT-COPY! ;

: SOURCE-PTR ( -- ptr source )
   0 SOURCE-BUF ;

: SOURCE! ( source -- )
   SOURCE-PTR ! ;

: SOURCE-VALUE ( -- source )
   SOURCE-PTR @ ;

: SELECT-BEGIN ( source -- )
   SOURCE!
   TRUE SELECTED-FLAG !
   0 CAUSE-CODE !
   FALSE READY? ! ;

: MAKE-ROOT ( -- )
   ROOT-BYTES MAKE-DIRS ;

: MAKE-ROOT-CHECKED ( -- )
   [: MAKE-ROOT ;] catch
   dup 0 <> if FAIL-ROOT then
   drop ;

: PREPARE-ROOT ( -- )
   ROOT-BYTES 2dup EXISTS? if
      2dup DIR? 0= if 2drop E-FS-DIR FAIL then
      2drop
   else
      2drop
      MAKE-ROOT-CHECKED
   then
   ROOT-BYTES FS:WRITABLE-ROOT? 0= if E-FS-IO FAIL then
   0 CAUSE-CODE !
   TRUE READY? ! ;

: SELECT-EXPLICIT ( ptr u8 n -- )
   construct source explicit SELECT-BEGIN
   SELECT-COPY!
   SELECTED>ROOT
   PREPARE-ROOT ;

: SELECT-SUFFIX ( ptr u8 n ptr u8 n source -- )
   {: a:ptr u:n suffix:ptr suffixu:n source:source :}
   source SELECT-BEGIN
   a u suffix suffixu SELECT-JOIN!
   SELECTED>ROOT
   PREPARE-ROOT ;

: NONEMPTY? ( ptr u8 n -- bool )
   nip 0 > ;

: SELECT-ENV ( -- )
   s" HABU_BUILD_CACHE" GETENV 2dup NONEMPTY? if SELECT-EXPLICIT exit then 2drop
   s" XDG_CACHE_HOME" GETENV 2dup NONEMPTY? if
      s" habu-build" construct source xdg SELECT-SUFFIX exit
   then 2drop
   s" HOME" GETENV 2dup NONEMPTY? if
      s" .cache/habu-build" construct source home SELECT-SUFFIX exit
   then 2drop
   s" TMPDIR" GETENV 2dup NONEMPTY? if
      s" habu-build" construct source tmp SELECT-SUFFIX exit
   then 2drop
   0 ROOT-U !
   0 SELECT-U !
   construct source none SOURCE!
   FALSE SELECTED-FLAG !
   E-FS-PATH FAIL ;

: ENSURE ( -- )
   READY? @ 0 <> if exit then
   OVERRIDE? @ 0 <> if
      PREPARE-ROOT
      exit
   then
   SELECT-ENV ;

public

: RESET ( -- )
   0 ROOT-U !
   0 SELECT-U !
   SELECT-FREE
   construct source none SOURCE!
   FALSE OVERRIDE? !
   FALSE READY? !
   FALSE SELECTED-FLAG !
   0 CAUSE-CODE ! ;

: ROOT! ( ptr u8 n -- )
   FALSE OVERRIDE? !
   construct source explicit SELECT-BEGIN
   SELECT-COPY!
   SELECTED>ROOT
   TRUE OVERRIDE? !
   FALSE READY? ! ;

: ROOT$ ( -- ptr u8 n )
   ENSURE
   ROOT-BYTES ;

: SOURCE ( -- source )
   ENSURE
   SOURCE-VALUE ;

: RESOLVE ( -- ptr u8 n source )
   ENSURE
   ROOT-BYTES SOURCE-VALUE ;

: SELECTED? ( -- bool )
   SELECTED-FLAG @ 0 <> ;

: SELECTED-ROOT$ ( -- ptr u8 n )
   SELECT-BYTES ;

: SELECTED-SOURCE ( -- source )
   SOURCE-VALUE ;

: CAUSE ( -- n )
   CAUSE-CODE @ ;

: SOURCE$ ( source -- ptr u8 n )
   MATCH source
      none OF s" none" ENDOF
      explicit OF s" explicit" ENDOF
      xdg OF s" xdg" ENDOF
      home OF s" home" ENDOF
      tmp OF s" tmp" ENDOF
   ;MATCH ;

: CAUSE$ ( -- ptr u8 n )
   CAUSE-CODE @
   dup 0 = if drop s" none" exit then
   dup E-FS-PATH = if drop s" E-FS-PATH" exit then
   dup E-FS-STAT = if drop s" E-FS-STAT" exit then
   dup E-FS-DIR = if drop s" E-FS-DIR" exit then
   dup E-FS-IO = if drop s" E-FS-IO" exit then
   dup E-FS-CAPACITY = if drop s" E-FS-CAPACITY" exit then
   throw ;

\ ---- retention ----------------------------------------------------------------

\ How long an entry stays after its last use.
86400 constant RETAIN-SECONDS

private

64 constant KEY-HEX-LEN
$2D constant DASH

TYPED-VARIABLE PREFIX-A ptr u8
TYPED-VARIABLE SUFFIX-A ptr u8
TYPED-VARIABLE WORK-A ptr u8
TYPED-VARIABLE KEEP-A ptr u8
TYPED-VARIABLE DIR-A ptr u8
TYPED-VARIABLE NAME-A ptr u8
TYPED-VARIABLE USED-A ptr u8
create CLAIM-BUF FS-PATH-CAP allot
create SRC-BUF FS-PATH-CAP allot
create DST-BUF FS-PATH-CAP allot

variable PREFIX-U
variable SUFFIX-U
variable WORK-U
variable KEEP-U
variable DIR-U
variable NAME-U
variable USED-U
variable CLAIM-U
variable SRC-U
variable DST-U
variable CUTOFF

: PREFIX$ ( -- ptr u8 n )
   PREFIX-A @ PREFIX-U @ ;

: SUFFIX$ ( -- ptr u8 n )
   SUFFIX-A @ SUFFIX-U @ ;

: WORK$ ( -- ptr u8 n )
   WORK-A @ WORK-U @ ;

: KEEP$ ( -- ptr u8 n )
   KEEP-A @ KEEP-U @ ;

: DIR$ ( -- ptr u8 n )
   DIR-A @ DIR-U @ ;

: NAME$ ( -- ptr u8 n )
   NAME-A @ NAME-U @ ;

: USED$ ( -- ptr u8 n )
   USED-A @ USED-U @ ;

: CLAIM$ ( -- ptr u8 n )
   CLAIM-BUF CLAIM-U @ ;

: SRC$ ( -- ptr u8 n )
   SRC-BUF SRC-U @ ;

: DST$ ( -- ptr u8 n )
   DST-BUF DST-U @ ;

: CLAIM-STEM$ ( -- ptr u8 n )
   s" build-cache-claim" ;

: ATOMIC-STEM$ ( -- ptr u8 n )
   s" .tmp" ;

: DIGIT? ( n -- bool ) {: c:n :}
   c $30 >= c $39 <= and ;

: HEX? ( n -- bool ) {: c:n :}
   c DIGIT?
   c $61 >= c $66 <= and or
   c $41 >= c $46 <= and or ;

\ The index past the run of digits that starts at i.
: DIGITS-END ( ptr u8 n n -- n ) {: a:ptr u:n i:n :}
   i begin dup u < if a over + c@ DIGIT? else FALSE then while 1+ repeat ;

\ -<seed>-<attempt>, the two decimal numbers MAKE-TEMP-DIR and ATOMIC-WRITE-FILE
\ put after the name they are given.
: TEMP-TAIL? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   u 4 < if FALSE exit then
   a c@ DASH <> if FALSE exit then
   a u 1 DIGITS-END {: mid:n :}
   mid 1 = mid u >= or if FALSE exit then
   a mid + c@ DASH <> if FALSE exit then
   a u mid 1+ DIGITS-END {: end:n :}
   end mid 1+ > end u = and ;

\ <stem>-<seed>-<attempt>.
: TEMP-OF? ( ptr u8 n ptr u8 n -- bool ) {: a:ptr u:n stem:ptr stemu:n :}
   a u stem stemu STARTS-WITH? 0= if FALSE exit then
   a stemu + u stemu - TEMP-TAIL? ;

\ KEY-HEX-LEN hex digits at the start of a span.
: KEY-HEAD? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   u KEY-HEX-LEN < if FALSE exit then
   KEY-HEX-LEN 0 ?do
      a i + c@ HEX? 0= if FALSE unloop exit then
   loop
   TRUE ;

\ What may follow a published name: nothing, or the .tmp-<seed>-<attempt> of an
\ atomic write.
: PUBLISH-TAIL? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   u 0= if TRUE exit then
   a u ATOMIC-STEM$ TEMP-OF? ;

\ <prefix><key><suffix>, alone or with an atomic write's tail.
: KEYED? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u PREFIX$ STARTS-WITH? 0= if FALSE exit then
   a PREFIX-U @ + {: k:ptr :}
   u PREFIX-U @ - {: ku:n :}
   k ku KEY-HEAD? 0= if FALSE exit then
   k KEY-HEX-LEN + {: t:ptr :}
   ku KEY-HEX-LEN - {: tu:n :}
   t tu SUFFIX$ STARTS-WITH? 0= if FALSE exit then
   t SUFFIX-U @ + tu SUFFIX-U @ - PUBLISH-TAIL? ;

\ An entry of the family, a work directory of its builds, or any pruner's claim.
: FAMILY? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u KEYED? if TRUE exit then
   WORK-U @ 0 > if a u WORK$ TEMP-OF? if TRUE exit then then
   a u CLAIM-STEM$ TEMP-OF? ;

\ The entry just published and this sweep's own claim directory.
: KEPT? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u KEEP$ STR=
   a u CLAIM$ BASENAME STR= or ;

\ The entry's own mtime, never a link target's. An entry already gone is not
\ stale: another pruner has it.
: STALE? ( ptr u8 n -- bool )
   FS-TRY-LSTAT 0= if FALSE exit then
   FS-STAT-MTIME-SEC@ CUTOFF @ < ;

: SAY ( ptr u8 n -- ) {: a:ptr u:n :}
   2 a u write drop ;

\ One line on fd 2: build-cache: cannot <verb> <path>: <code>. The path goes out
\ on its own: the string builder's 1 KiB would refuse one near FS-PATH-CAP, and
\ a report must never throw.
: REPORT ( ptr u8 n ptr u8 n n -- ) {: v:ptr vu:n a:ptr u:n code:n :}
   s" build-cache: cannot " SAY
   v vu SAY
   s"  " SAY
   a u SAY
   SB-RESET
   s" : " SB-APPEND
   code FMT:SB-INT
   S\" \n" SB-APPEND
   SB$ SAY ;

\ A claimed entry that cannot be removed goes back under its own name.
: PUT-BACK ( n -- ) {: code:n :}
   s" remove" SRC$ code REPORT
   [: DST$ SRC$ RENAME-FILE ;] catch {: back:n :}
   back 0<> if s" put back" DST$ back REPORT then ;

\ The rename is the claim. When it fails and the source is gone, another pruner
\ claimed the entry first; any other failure is the caller's to report.
: TAKE ( -- )
   DIR$ NAME$ SRC-BUF JOIN-PATH SRC-U !
   SRC$ STALE? 0= if exit then
   CLAIM$ NAME$ DST-BUF JOIN-PATH DST-U !
   [: SRC$ DST$ RENAME-FILE ;] catch {: code:n :}
   code 0<> if
      SRC$ FS-TRY-LSTAT if code throw then
      exit
   then
   [: DST$ REMOVE-TREE ;] catch {: rm:n :}
   rm 0<> if rm PUT-BACK then ;

: VISIT ( ptr u8 n -- ) {: a:ptr u:n :}
   a u FAMILY? 0= if exit then
   a u KEPT? if exit then
   a NAME-A !
   u NAME-U !
   0 SRC-U !
   [: TAKE ;] catch {: code:n :}
   code 0= if exit then
   SRC-U @ 0= if s" prune" NAME$ code REPORT exit then
   s" prune" SRC$ code REPORT ;

: SWEEP ( -- )
   DIR-U @ 0 <= if E-FS-PATH throw then
   DIR$ CLAIM-STEM$ MAKE-TEMP-DIR {: a:ptr u:n :}
   a CLAIM-BUF u BYTE-COPY
   u CLAIM-U !
   DIR$ [: VISIT ;] FS-LIST:EACH ;

\ The claim holds only what could not be put back, so it goes only when empty.
: CLOSE-CLAIM ( -- )
   CLAIM-U @ 0= if exit then
   [: CLAIM$ REMOVE-DIR ;] catch {: code:n :}
   code 0<> if s" remove" CLAIM$ code REPORT then ;

public

\ Remove every entry of one family that nothing has used for RETAIN-SECONDS,
\ from the directory holding the entry just published, which stays: the prefix
\ and suffix around a key, and the stem MAKE-TEMP-DIR made its builds' work
\ directories with (empty when they make none). Failures are reported, never
\ thrown.
: PRUNE ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- )
   {: pre:ptr preu:n suf:ptr sufu:n work:ptr worku:n keep:ptr keepu:n :}
   pre PREFIX-A !
   preu PREFIX-U !
   suf SUFFIX-A !
   sufu SUFFIX-U !
   work WORK-A !
   worku WORK-U !
   keep keepu BASENAME KEEP-U ! KEEP-A !
   keep DIR-A !
   keepu KEEP-U @ - 1- 0 max DIR-U !
   0 CLAIM-U !
   TIME:EPOCH-SECONDS RETAIN-SECONDS - CUTOFF !
   [: SWEEP ;] catch {: code:n :}
   CLOSE-CLAIM
   code 0= if exit then
   s" sweep" DIR$ code REPORT ;

\ Mark an entry found on disk as in use by dating it to now, which is what keeps
\ PRUNE from taking it. TRUE when the hit stands: dated, or kept although it
\ cannot be dated (another owner, an immutable flag), which is reported on fd 2.
\ FALSE when the entry is gone - a pruner took it after the caller's check - so
\ the caller builds it again. TRUE does not hold the entry: a pruner that read it
\ stale before the date can still take it (see the header).
: USED ( ptr u8 n -- bool )
   USED-U ! USED-A !
   [: USED$ TOUCH ;] catch {: code:n :}
   code 0= if TRUE exit then
   USED$ FS-TRY-LSTAT 0= if FALSE exit then
   s" date" USED$ code REPORT
   TRUE ;

\ ---- work directories --------------------------------------------------------

private

2 constant LOCK-EX
4 constant LOCK-NB
35 constant WOULD-BLOCK-MACOS
11 constant WOULD-BLOCK-LINUX
16 constant WORK-TRIES

create STEM-BUF FS-PATH-CAP allot
create HELD-BUF FS-PATH-CAP allot
TYPED-VARIABLE GONE-A ptr u8

variable STEM-U
variable HELD-U
variable GONE-U
variable SETTLED

PROCESS-SYMBOLS
FUNCTION: FLOCK-CALL flock ( n n -- i32 ) ;FUNCTION

\ The descriptor HOLD opened, -1 when none: whoever called HOLD closes it
\ through RELEASE or keeps it as its hold.
variable HOLD-FD
-1 HOLD-FD !

: WORKDIR-STEM$ ( -- ptr u8 n )
   s" build-cache-work-" ;

: STEM$ ( -- ptr u8 n )
   STEM-BUF STEM-U @ ;

: HELD$ ( -- ptr u8 n )
   HELD-BUF HELD-U @ ;

: GONE$ ( -- ptr u8 n )
   GONE-A @ GONE-U @ ;

: COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u:n dst:ptr up:ptr :}
   u FS-PATH-CAP > if E-FS-CAPACITY throw then
   a dst u BYTE-COPY
   u up ! ;

\ The index where the run of digits that ends at i starts.
: DIGITS-START ( ptr u8 n -- n ) {: a:ptr i:n :}
   i begin dup 0 > if a over 1- + c@ DIGIT? else FALSE then while 1- repeat ;

\ Where a name's closing -<seed>-<attempt> starts, or 0 when it has none.
: TAIL-AT ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u DIGITS-START {: s:n :}
   s u = s 2 < or if 0 exit then
   a s 1- + c@ DASH <> if 0 exit then
   a s 1- DIGITS-START {: t:n :}
   t s 1- = t 2 < or if 0 exit then
   a t 1- + c@ DASH <> if 0 exit then
   t 1- ;

\ build-cache-work-<family>-<seed>-<attempt>: a family between the stem and the
\ -<seed>-<attempt> MAKE-TEMP-DIR closes it with.
: WORKDIR? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u WORKDIR-STEM$ STARTS-WITH? 0= if FALSE exit then
   a u TAIL-AT WORKDIR-STEM$ nip > ;

: RELEASE ( -- )
   HOLD-FD @ 0 >= if HOLD-FD @ close then
   -1 HOLD-FD ! ;

: WOULD-BLOCK ( -- n )
   HB-TARGET-MACOS? if WOULD-BLOCK-MACOS else WOULD-BLOCK-LINUX then ;

\ The path still names the directory HOLD-FD locked.
: STILL-NAMED? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u FS-PATHZ open-rd {: again:n :}
   again 0 < if FALSE exit then
   HOLD-FD @ >FD again >FD FS:SAME-OPEN-FILE? {: same:bool :}
   again close
   same ;

\ Lock the directory at a path without waiting. TRUE when this process now
\ holds it; FALSE when it is gone or not a directory, another process holds
\ it, or the path no longer names the directory locked. A lock refused for any
\ other reason is E-FS-IO. Whatever it answers or throws, what it opened is in
\ HOLD-FD.
: HOLD ( ptr u8 n -- bool ) {: a:ptr u:n :}
   RELEASE
   a u FS-TRY-LSTAT 0= if FALSE exit then
   FS-STAT-MODE@ S-IFMT and S-IFDIR <> if FALSE exit then
   a u FS-PATHZ open-rd HOLD-FD !
   HOLD-FD @ 0 < if
      a u EXISTS? if E-FS-OPEN throw then
      FALSE exit
   then
   FFI:ERRNO drop                     \ bound now, not after flock sets errno
   HOLD-FD @ LOCK-EX LOCK-NB or FLOCK-CALL 0<> if
      FFI:ERRNO WOULD-BLOCK = if FALSE exit then
      E-FS-IO throw
   then
   a u STILL-NAMED? ;

\ build-cache-work-<family>, the stem MAKE-TEMP-DIR closes with
\ -<seed>-<attempt>. An empty family is E-FS-PATH: WORKDIR? takes no name made
\ without one, so no reaper would take its directory.
: STEM! ( ptr u8 n -- ) {: fam:ptr famu:n :}
   famu 0 <= if E-FS-PATH throw then
   SB-RESET
   WORKDIR-STEM$ SB-APPEND
   fam famu SB-APPEND
   SB$ STEM-BUF STEM-U COPY! ;

\ A work directory that nothing holds is a killed build's: it is removed where
\ it stands, since the lock admits one reaper at a time.
: REAP-TAKE ( -- )
   DIR$ NAME$ SRC-BUF JOIN-PATH SRC-U !
   SRC$ HOLD 0= if exit then
   SRC$ REMOVE-TREE ;

: REAP-VISIT ( ptr u8 n -- ) {: a:ptr u:n :}
   a u WORKDIR? 0= if exit then
   a NAME-A !
   u NAME-U !
   0 SRC-U !
   [: REAP-TAKE ;] catch {: code:n :}
   RELEASE
   code 0= if exit then
   SRC-U @ 0= if s" reap" NAME$ code REPORT exit then
   s" reap" SRC$ code REPORT ;

: REAP-SWEEP ( -- )
   DIR$ [: REAP-VISIT ;] FS-LIST:EACH ;

\ Failures are reported, never thrown: a build does not fail over another's
\ leftovers.
: REAP ( -- )
   ROOT$ {: r:ptr ru:n :}
   r DIR-A !
   ru DIR-U !
   [: REAP-SWEEP ;] catch {: code:n :}
   code 0= if exit then
   s" reap" DIR$ code REPORT ;

\ Hold the directory just made and register it for removal at exit.
: SETTLE ( -- )
   0 SETTLED !
   HELD$ HOLD 0= if exit then
   HELD$ CLEANUP-TREE+
   TRUE SETTLED ! ;

\ FALSE when a reaper took the directory before this process locked it; the
\ reaper removes it. A refusal removes it here before the throw.
: HOLD-NEW ( -- bool )
   [: SETTLE ;] catch {: code:n :}
   code 0= if SETTLED @ 0<> exit then
   RELEASE
   HELD$ REMOVE-TREE
   code throw ;

public

\ Make a work directory in the root for a build of the family that publishes by
\ rename, after removing every work directory that nothing holds. The path
\ stays valid until the next WORK-OPEN; the descriptor is the hold, which this
\ process's children inherit. The directory is registered for removal at exit,
\ and WORK-CLOSE removes it and releases the hold.
: WORK-OPEN ( ptr u8 n -- ptr u8 n fd )
   STEM!
   REAP
   0 begin
      dup WORK-TRIES >= if E-FS-IO throw then
      ROOT$ STEM$ MAKE-TEMP-DIR HELD-BUF HELD-U COPY!
      HOLD-NEW 0=
   while
      RELEASE
      1+
   repeat drop
   HELD$ HOLD-FD @ >FD
   -1 HOLD-FD ! ;

\ Remove a work directory WORK-OPEN made, then release its hold. A removal
\ that fails still releases it, so the next build reaps what is left.
: WORK-CLOSE ( ptr u8 n fd -- ) {: a:ptr u:n fd:fd :}
   a GONE-A !
   u GONE-U !
   [: GONE$ REMOVE-TREE ;] catch {: code:n :}
   fd FD>N close
   code 0<> if code throw then ;

;package
