\ fs-mutate.f - checked filesystem mutation helpers.
\
\ STORAGE CLASS. TASK-LOCAL for the paths one call stages: the second NUL-padded
\ path FS-MUT-PATHZ2-BUF (RENAME-FILE's, MAKE-SYMLINK's and the stream copy's
\ destination), the unique `.tmp-*` sibling ATOMIC-WRITE-FILE, RESERVE-SIBLING
\ and REPLACE-STAGED name, FS-MUT-ATOMIC-PATH, and
\ MAKE-TEMP-DIR's FS-MUT-TMP-PATH - the one this module RETURNS a span into -
\ are the FS-MUT-ABI band of the per-task DATA region, so two tasks renaming,
\ symlinking, writing atomically or making a temporary directory at once share
\ nothing and neither can replace the other's returned path. The descriptors,
\ cursors and lengths COPY-FILE-STREAM threads are LOCALS of the call.
\ PER CALL: the tree removal. REMOVE-TREE maps a walk context of its own, as
\ src/lib/fs.f's WALK-FILES does, holds its per-depth directory descriptors and
\ dirent blocks there and releases it on return or throw, so two tasks remove
\ two trees at once and a WALK-FILES callback removes a subtree.
\ PROCESS-WIDE: the 8 KiB FS-MUT-COPY-BUF, so COPY-FILE and COPY-FILE-STREAM are
\ this module's one SINGLE-TASK pair (8192 bytes is more than the whole per-task
\ band could carry; a second task that must copy needs its own buffer); and the
\ cleanup registry, which is a PROCESS exit registry and not per-call scratch -
\ REGISTERING is safe from any task, because a registration claims its slot with
\ one atomic-add and writes only that slot, FORGETTING is too, because it empties
\ only the slot naming its path and gives the top back with one atomic-cas, and
\ the RUN belongs to the process.
\
\ CONTRACT. Public names avoid the primitives' (the dictionary is
\ case-insensitive): MAKE-DIR, REMOVE-DIR, MAKE-DIRS, not mkdir or rmdir. After
\ an E-FS-IO from MAKE-DIR, MKDIR-MODE or MAKE-DIRS, FFI:ERRNO names libc's
\ cause; MAKE-DIRS keeps a component mkdir finds existing (EEXIST) only when it
\ is a directory or a link to one. COPY-FILE refuses a file past the caller's
\ capacity with E-FS-CAPACITY instead of truncating. REPLACE-STAGED leaves the
\ destination naming its old file or the whole new one: the sibling is removed
\ if the filler or the rename throws or the filler dies. A destination longer
\ than SIBLING-PATH-MAX is E-SPAN-CAPACITY; one whose last name is longer than
\ SIBLING-NAME-MAX gives a sibling the host refuses, E-FS-OPEN. REMOVE-TREE
\ unlinks a symlink to a directory and never descends into it, a root spelled
\ with a trailing slash (`link/`, `lfile/`, `dang/`) included, and throws on
\ any partial failure. Its root path must fit FS-PATH-CAP; below the root each
\ entry is named relative to its parent's directory descriptor and the walk's
\ frames grow with the depth, so neither path lengths nor depth bound the tree,
\ only the descriptors the process may hold open. A missing root
\ is a no-op; a root that cannot be stat'ed for any other reason is E-FS-STAT,
\ with libc's cause in FS:ERRNO. HB-TMP-MKDIR makes its tree under $HB_TMP
\ when the process was handed one (the caller reaps it, even after a kill),
\ else under $TMPDIR or /tmp; HB-TMP-MKDIR and TMPDIR-MKDIR make it under the
\ root's canonical spelling. CLEANUP-RUN removes registrations newest first,
\ so a parent registered before its child goes after it. Each registration also
\ arms CLEANUP-AT-EXIT in the engine's EXIT-HOOK-CELL, chaining a hook already
\ there to run last, once; a restored or stripped image (docs/architecture.md
\ "Images") starts with an empty table.
\
\ Every buffer this module owns - the second NUL-padded path, the copy buffer,
\ the atomic and temp path builders and the cleanup path stack - is a span, so
\ the copies into them are bounded by the buffer's own reach (E-SPAN-CAPACITY,
\ E-SPAN-RANGE) and not by a hand-written FS-PATH-CAP comparison. What remains
\ hand-written is the path DOMAIN: a negative or empty path is E-FS-PATH and
\ fs.f's one FS-PATH-CAP length gate still runs on every path handed to a
\ syscall through FS-PATHZ / FS-MUT-PATHZ2.
\
require src/lib/errors.f
require src/lib/string.f
require src/lib/fmt.f
require src/lib/fs.f
require src/lib/fs-identity.f
require src/lib/ffi-abi.f
require src/lib/span.f

$FFF constant FS-MUT-MODE-PERM
73 constant FS-MUT-MODE-EXEC
384 constant FS-MUT-MODE-PRIVATE-FILE
448 constant FS-MUT-MODE-PRIVATE-DIR
493 constant FS-MUT-MODE-DIR
8192 constant FS-MUT-COPY-CAP
64 constant FS-MUT-CLEANUP-MAX
64 constant FS-MUT-ATOMIC-RETRIES
64 constant FS-MUT-TMP-RETRIES
0 constant FS-MUT-CLEANUP-FILE
1 constant FS-MUT-CLEANUP-DIR
2 constant FS-MUT-CLEANUP-TREE
$2D constant FS-MUT-DASH
$2E constant FS-MUT-DOT
$2F constant FS-MUT-SLASH
17 constant FS-MUT-EXISTS                    \ EEXIST on both targets

FS-MUT-COPY-CAP SPAN-BUFFER: FS-MUT-COPY-BUF
FS-MUT-CLEANUP-MAX FS-PATH-CAP * SPAN-BUFFER: FS-MUT-CLEANUP-PATHS
create FS-MUT-CLEANUP-US FS-MUT-CLEANUP-MAX cells allot
create FS-MUT-CLEANUP-KINDS FS-MUT-CLEANUP-MAX cells allot

variable FS-MUT-CLEANUP-N

\ THE STAGED PATHS ARE TASK-LOCAL, in the FS-MUT-ABI band of the per-task DATA
\ region: every accessor reads `data-base`, which is the RUNNING task's region,
\ so the path MAKE-TEMP-DIR hands back stays this task's until it calls again,
\ and two tasks in RENAME-FILE, MAKE-SYMLINK or ATOMIC-WRITE-FILE at once share
\ nothing. The band is declared in src/habu/layout.f below FS-ABI and asserted
\ there against every other DATA claim. It is a declared band and not TASK:+USER
\ rows for src/lib/fs.f's reason: tools/native-build-core.f requires this file to
\ build the engine, so a `require src/lib/task.f` here would load the task runtime
\ into the build tool. A region is a fresh zeroed mapping, so a new task's paths
\ start empty exactly as the old SPAN-BUFFERs did.
\
\ layout.f is loaded before this file and cannot see FS-PATHZ-CAP or
\ FS-PATH-CAP, so the band states those widths and this executes the agreement
\ once at load.
: FS-MUT-BAND-AGREE ( -- )
   FS-PATHZ-CAP FS-MUT-ABI:PATHZ2-BYTES <> if E-FS-BAND throw then
   FS-PATH-CAP FS-MUT-ABI:ATOMIC-BYTES <> if E-FS-BAND throw then
   FS-PATH-CAP FS-MUT-ABI:TMP-BYTES <> if E-FS-BAND throw then ;
FS-MUT-BAND-AGREE

\ Each reach is the band width FS-MUT-BAND-AGREE asserted against layout.f, so
\ the span says exactly what this task's region holds.
: FS-MUT-PATHZ2-BUF ( -- SPAN:span<u8> )
   data-base FS-MUT-ABI:PATHZ2-OFF + BYTE-VIEW FS-PATHZ-CAP SPAN:MAKE ;

: FS-MUT-ATOMIC-PATH ( -- SPAN:span<u8> )
   data-base FS-MUT-ABI:ATOMIC-OFF + BYTE-VIEW FS-PATH-CAP SPAN:MAKE ;

: FS-MUT-TMP-PATH ( -- SPAN:span<u8> )
   data-base FS-MUT-ABI:TMP-OFF + BYTE-VIEW FS-PATH-CAP SPAN:MAKE ;

: FS-MUT-PATHZ2 ( ptr u8 n -- ptr u8 )
   FS-MUT-PATHZ2-BUF FS-PATHZ-INTO ;

\ Build and seed tools append to paths in caller-owned bounded storage.
: FS-MUT-SUFFIX-PATH ( ptr u8 n ptr u8 n SPAN:span<u8> -- n ) {: a:ptr u s:ptr su dst :}
   u 0 < if E-FS-PATH throw then
   su 0 < if E-FS-PATH throw then
   a u dst SPAN:COPY
   s su dst u SPAN:SKIP SPAN:COPY
   u su + ;

: FS-MUT-CLEANUP-SLOT ( n -- SPAN:span<u8> ) {: idx :}
   idx 0 < if E-FS-CAPACITY throw then
   idx FS-MUT-CLEANUP-MAX >= if E-FS-CAPACITY throw then
   FS-MUT-CLEANUP-PATHS idx FS-PATH-CAP * FS-PATH-CAP SPAN:SUB ;

: FS-MUT-CLEANUP-U-PTR ( n -- ptr n ) {: idx :}
   idx 0 < if E-FS-CAPACITY throw then
   idx FS-MUT-CLEANUP-MAX >= if E-FS-CAPACITY throw then
   idx cells FS-MUT-CLEANUP-US + ;

: FS-MUT-CLEANUP-KIND-PTR ( n -- ptr n ) {: idx :}
   idx 0 < if E-FS-CAPACITY throw then
   idx FS-MUT-CLEANUP-MAX >= if E-FS-CAPACITY throw then
   idx cells FS-MUT-CLEANUP-KINDS + ;

: REMOVE-FILE ( ptr u8 n -- ) {: a:ptr u :}
   a u FS-PATHZ unlink 0 < if E-FS-IO throw then ;

: RENAME-FILE ( ptr u8 n ptr u8 n -- ) {: src:ptr srcu dst:ptr dstu :}
   src srcu FS-PATHZ dst dstu FS-MUT-PATHZ2 rename 0 < if E-FS-IO throw then ;

package FS-NOREPLACE
PROCESS-SYMBOLS
FUNCTION: LINK-CALL link ( ptr u8 ptr u8 -- i32 ) ;FUNCTION
public
: LINK-NEW? ( ptr u8 ptr u8 -- bool )
   LINK-CALL 0<> if
      FFI:ERRNO FS-MUT-EXISTS = if false exit then
      E-FS-IO throw
   then
   true ;
;package

\ The link claims the destination atomically. The caller owns a regular-file
\ source on the same filesystem until this call returns. Source removal is a
\ second step; if it fails, the published destination stays and E-FS-IO throws.
: RENAME-NOREPLACE ( ptr u8 n ptr u8 n -- bool ) {: src:ptr srcu dst:ptr dstu :}
   src srcu FS-PATHZ dst dstu FS-MUT-PATHZ2 FS-NOREPLACE:LINK-NEW? 0= if false exit then
   src srcu REMOVE-FILE
   true ;

: CHMOD-MODE ( ptr u8 n n -- ) {: a:ptr u mode :}
   a u FS-PATHZ mode chmod 0 < if E-FS-IO throw then ;

: CHMOD-X ( ptr u8 n -- ) {: a:ptr u :}
   a u STAT-MODE FS-MUT-MODE-PERM and FS-MUT-MODE-EXEC or {: mode :}
   a u mode CHMOD-MODE ;

: MAKE-SYMLINK ( ptr u8 n ptr u8 n -- ) {: target:ptr targetu link:ptr linku :}
   target targetu FS-PATHZ link linku FS-MUT-PATHZ2 symlink 0 < if E-FS-IO throw then ;

\ mkdir(2) is bound through libc, the way FS-NOREPLACE binds link(2), so a
\ failure leaves its errno for the caller's FFI:ERRNO: the engine primitive
\ collapses every failure to a bare -1 (src/habu/habu1.f's errno rule).
package FS-MKDIR
PROCESS-SYMBOLS
FUNCTION: MKDIR-CALL mkdir ( ptr u8 n -- i32 ) ;FUNCTION
public
: MADE? ( ptr u8 n -- bool )
   MKDIR-CALL 0= ;
;package

: MKDIR-MODE ( ptr u8 n n -- ) {: a:ptr u mode :}
   a u FS-PATHZ mode FS-MKDIR:MADE? 0= if E-FS-IO throw then ;

: MAKE-DIR ( ptr u8 n -- )
   FS-MUT-MODE-DIR MKDIR-MODE ;

: REMOVE-DIR ( ptr u8 n -- ) {: a:ptr u :}
   a u FS-PATHZ rmdir 0 < if E-FS-IO throw then ;

\ Tree removal names every entry below the root relative to its parent's
\ directory descriptor, held in the walk's per-depth frames, so no path below
\ the root is joined and none is bounded by FS-PATH-CAP. The root is the entry
\ its own path names relative to the working directory. The error paths throw
\ plainly: FS-WALK-UNMAP closes whatever descriptors the frames still hold,
\ however the traversal ended.

\ The entry's own mode, never a link target's, lands in FS-STAT-BUF. A name
\ already gone (ENOENT) is false: a missing root, or an entry removed since its
\ directory was read. Any other failure is E-FS-STAT.
: FS-MUT-LSTAT-AT? ( n ptr u8 n -- bool ) {: parent:n name:ptr u:n :}
   parent name u FS-PATHZ FS-STAT-BUF FS-AT-NOFOLLOW FS-STAT-AT-CALL 0= if true exit then
   FFI:ERRNO FS:ENOENT = if false exit then
   E-FS-STAT FS-ERRNO-FAIL ;


: FS-MUT-UNLINK-AT ( n ptr u8 n n -- ) {: parent:n name:ptr u:n flags:n :}
   parent name u FS-PATHZ flags FS-UNLINK-AT-CALL 0= if exit then
   E-FS-IO FS-ERRNO-FAIL ;


\ The directory opens one depth down, O_NOFOLLOW, so a link swapped in since
\ the stat is refused, never followed. The name is staged before that depth's
\ frame is made: a child's name lies in its parent's dirent block, which
\ growing the frames moves.
: FS-MUT-OPEN-AT ( ptr n n ptr u8 n -- ) {: ctx:ptr parent:n name:ptr u:n :}
   name u FS-PATHZ {: z:ptr :}
   ctx FS-WALK-DEPTH@ 1 + {: d :}
   ctx d FS-ENSURE-DEPTH
   d ctx FS-WALK-DEPTH!
   parent z FS-OPEN-DIR-FLAGS FS-OPEN-AT-CALL
   dup 0 < if drop E-FS-OPEN FS-ERRNO-FAIL then
   ctx FS-FD!
   0 ctx FS-BASE@ ! ;

\ The name an emptied directory is unlinked by, once its traversal is back at
\ its parent's depth. That traversal may have grown the frames and so moved the
\ parent's dirent block: a child's name is read again from its parent's
\ current entry. The root's, at depth -1, is the caller's path and never moves.
: FS-MUT-OWN-NAME ( ptr n ptr u8 n -- ptr u8 n ) {: ctx:ptr name:ptr u:n :}
   ctx FS-WALK-DEPTH@ 0 < if name u exit then
   ctx FS-CUR-ENTRY FS-DIRENT-NAME ;

\ Only a directory is traversed; every other inode, a link to a directory
\ among them, belongs to unlink. A child's name stays in its parent's dirent
\ block, one depth up from the blocks the child's own traversal reads.
: FS-MUT-REMOVE-AT ( ptr n n ptr u8 n -- ) {: ctx:ptr parent:n name:ptr u:n :}
   parent name u FS-MUT-LSTAT-AT? 0= if exit then
   FS-STAT-BUF FS-LIBC-MODE@ S-IFMT and S-IFDIR <> if
      parent name u 0 FS-MUT-UNLINK-AT exit
   then
   ctx parent name u FS-MUT-OPEN-AT
   begin ctx FS-READ-DIR while
      ctx FS-DIR-BLOCK-BEGIN
      begin ctx FS-DIR-MORE? while
         ctx FS-LOAD-ENTRY FS-DIRENT-NAME 2dup FS-SKIP-SELF-ENTRY? if
            2drop
         else
            ctx ctx FS-FD@ 2swap RECURSE
         then
         ctx FS-ADVANCE-ENTRY
      repeat
   repeat
   ctx FS-CLOSE-CUR-DIR
   ctx FS-ASCEND-PATH
   parent ctx name u FS-MUT-OWN-NAME FS-AT-REMOVEDIR FS-MUT-UNLINK-AT ;


\ The root's length to stat. lstat of `link/` resolves the link, so a root
\ whose name without trailing slashes is a symlink - `link/`, `lfile/`,
\ `dang/` - is stat'ed without them and unlinked however it is spelled. Any
\ other root is stat'ed as spelled: `plain/` on a regular file stays ENOTDIR.
\ A root of slashes only keeps one, the filesystem root.
: FS-MUT-ROOT-LEN ( ptr u8 n -- n ) {: a:ptr u :}
   u begin
      dup 1 > if dup 1 - a + c@ FS-SLASH = else false then
   while 1 - repeat {: s :}
   s u < if a s SYMLINK? if s exit then then
   u ;

\ Caught as a whole, the way src/lib/fs.f catches FS-WALK-RUN, so one site closes
\ the descriptors and releases the frames for every way out, a root refused by
\ FS-MUT-ROOT-LEN's stat included.
: FS-MUT-REMOVE-RUN ( ptr n ptr u8 n -- ptr n ptr u8 n ) {: ctx:ptr a:ptr u :}
   ctx FS-AT-CWD a a u FS-MUT-ROOT-LEN FS-MUT-REMOVE-AT
   ctx a u ;

\ The WITH-BYTES body, built as src/lib/fs.f's FS-WALK-SCOPE. No flag guards
\ the context: it is this call's alone, so the process exit hook removes trees
\ through CLEANUP-RUN whatever a task was doing.
: FS-MUT-REMOVE-SCOPE ( ptr u8 n ptr u8 NUM:alloc-byte-len -- )
   drop CELL-VIEW {: a:ptr u ctx:ptr :}
   ctx FS-WALK-MAP
   \ The depth names the directory being emptied: none until the root opens.
   -1 ctx FS-WALK-DEPTH!
   ctx a u [: FS-MUT-REMOVE-RUN ;] catch {: code:n :}
   2drop drop
   ctx FS-WALK-UNMAP
   code 0<> if code throw then ;

: REMOVE-TREE ( ptr u8 n -- ) {: a:ptr u :}
   u 0 <= if FS-CAUSE-CLEAR E-FS-PATH throw then
   a u FS-WALK-HANDLE-BYTES MEM:BYTES-ALLOC-LEN [: FS-MUT-REMOVE-SCOPE ;] MEM:WITH-BYTES ;

\ Only EEXIST can mean the folder is already there, so only it is checked
\ with a stat. Any other failure throws at once, mkdir's errno intact.
: FS-MUT-MKDIR-ONE ( ptr u8 n -- ) {: a:ptr u :}
   a u FS-PATHZ FS-MUT-MODE-DIR FS-MKDIR:MADE? if exit then
   FFI:ERRNO FS-MUT-EXISTS <> if E-FS-IO throw then
   a u DIR? 0= if E-FS-IO throw then ;

: MAKE-DIRS ( ptr u8 n -- ) {: a:ptr u :}
   u 0 <= if E-FS-PATH throw then
   1 begin dup u < while
      a over + c@ FS-MUT-SLASH = if
         dup 0 > if a over FS-MUT-MKDIR-ONE then
      then
      1+
   repeat drop
   a u FS-MUT-MKDIR-ONE ;

\ `cap` is how much of the copy buffer this call may use: the narrowing refuses
\ a cap past the buffer (E-SPAN-RANGE) and a negative one, so neither needs a
\ hand-written comparison. A file larger than `cap` is still READ-ALL's
\ E-FS-CAPACITY.
: COPY-FILE ( ptr u8 n ptr u8 n n -- ) {: src:ptr srcu dst:ptr dstu cap :}
   src srcu FS-MUT-COPY-BUF cap SPAN:TAKE SPAN:$ READ-ALL {: n :}
   dst dstu FS-MUT-COPY-BUF n SPAN:TAKE SPAN:$ WRITE-ALL ;

package FS-COPY
PROCESS-SYMBOLS
FUNCTION: TRUNCATE-CALL ftruncate ( n n -- i32 ) ;FUNCTION
public
: PREPARE-DST ( fd fd -- ) {: source dest :}
   source dest FS:SAME-OPEN-FILE? if E-FS-OPEN throw then
   dest FD>N 0 TRUNCATE-CALL 0<> if E-FS-IO throw then ;
;package

\ THE STREAM COPY'S STATE IS THE CALL'S. The two descriptors, the read and write
\ counts and the write cursor were cells of this module; they are locals and
\ stack values now, so nothing here is left for a second task to find. A
\ descriptor a helper must close on its error path is therefore a PARAMETER: -1
\ stands for "not open yet", which is what CLOSE-COPY-FD skips.
: FS-MUT-CLOSE-COPY-FD ( n -- ) {: fd :}
   fd 0 >= if fd close then ;

: FS-MUT-COPY-THROW ( n n n -- ) {: in out code :}
   in FS-MUT-CLOSE-COPY-FD
   out FS-MUT-CLOSE-COPY-FD
   code throw ;

: FS-MUT-COPY-OPEN-SRC ( ptr u8 n -- n ) {: src:ptr srcu :}
   src srcu FS-PATHZ open-rd {: in :}
   in 0 < if E-FS-OPEN throw then
   in ;

\ Stack-preserving under `catch`: the two descriptors are the quotation's
\ declared window and no path of this body consumes them, so the caller may drop
\ them and read its own locals after the catch.
: FS-MUT-COPY-PREPARE-DST ( n n -- n n )
   2dup >FD swap >FD swap FS-COPY:PREPARE-DST ;

: FS-MUT-COPY-OPEN-DST ( n ptr u8 n -- n ) {: in dst:ptr dstu :}
   dst dstu EXISTS? if
      dst dstu FILE? 0= if in -1 E-FS-OPEN FS-MUT-COPY-THROW then
   then
   dst dstu FS-MUT-PATHZ2
   FS-O-WRONLY FS-O-CREAT or FS-MODE-0644 open {: out :}
   out 0 < if in -1 E-FS-OPEN FS-MUT-COPY-THROW then
   \ Opening must not truncate until the descriptor identities differ.
   in out [: FS-MUT-COPY-PREPARE-DST ;] catch {: code :}
   2drop
   code 0<> if in out code FS-MUT-COPY-THROW then
   out ;

\ The write cursor is the deepest argument, so a loop can leave it on the stack
\ and hand the call its three known values on top.
: FS-MUT-COPY-WRITE-ONE ( n n n n -- n ) {: off in out u :}
   out FS-MUT-COPY-BUF off SPAN:SKIP  u off - SPAN:TAKE SPAN:$ write {: wr :}
   wr 0 <= if in out E-FS-IO FS-MUT-COPY-THROW then
   wr u off - > if in out E-FS-IO FS-MUT-COPY-THROW then
   off wr + ;

: FS-MUT-COPY-WRITE-CHUNK ( n n n -- ) {: u in out :}
   0 begin dup u < while
      in out u FS-MUT-COPY-WRITE-ONE
   repeat drop ;

: FS-MUT-COPY-READ-ONE ( n n -- n ) {: in out :}
   in FS-MUT-COPY-BUF SPAN:$ read {: rd :}
   rd 0 < if in out E-FS-IO FS-MUT-COPY-THROW then
   rd FS-MUT-COPY-CAP > if in out E-FS-IO FS-MUT-COPY-THROW then
   rd ;

: FS-MUT-COPY-PUMP ( n n -- ) {: in out :}
   begin in out FS-MUT-COPY-READ-ONE dup 0 > while
      in out FS-MUT-COPY-WRITE-CHUNK
   repeat drop ;

: COPY-FILE-STREAM ( ptr u8 n ptr u8 n -- ) {: src:ptr srcu dst:ptr dstu :}
   src srcu FS-MUT-COPY-OPEN-SRC {: in :}
   in dst dstu FS-MUT-COPY-OPEN-DST {: out :}
   in out FS-MUT-COPY-PUMP
   in FS-MUT-CLOSE-COPY-FD
   out FS-MUT-CLOSE-COPY-FD ;

: FS-MUT-SB-U ( n -- ) {: n :}
   n 0 < if E-FS-PATH throw then
   n 10 >= if n 10 / RECURSE then
   n 10 mod 48 + SB-APPEND-C ;

: FS-MUT-U-DIGITS ( n -- n ) {: n :}
   n 0 < if E-FS-PATH throw then
   n 10 < if 1 exit then
   n 10 / RECURSE 1+ ;

\ The longest `.tmp-<seed>-<attempt>` suffix RESERVE-SIBLING and
\ ATOMIC-WRITE-FILE append to a path: the largest seed, the largest
\ nonnegative cell `-1 1 rshift`, and the last attempt, so whether a path has
\ room for its sibling never depends on the seed it drew.
6 -1 1 rshift FS-MUT-U-DIGITS +
FS-MUT-ATOMIC-RETRIES 1- FS-MUT-U-DIGITS + constant FS-MUT-SIBLING-SUFFIX-MAX

\ NAME_MAX, the longest name a directory holds on Linux and macOS. The host
\ refuses a longer one, so a sibling past it fails to open (E-FS-OPEN).
255 constant FS-MUT-NAME-MAX

\ The longest path, and the longest last name, a sibling still fits beside.
FS-PATH-CAP FS-MUT-SIBLING-SUFFIX-MAX - constant SIBLING-PATH-MAX
FS-MUT-NAME-MAX FS-MUT-SIBLING-SUFFIX-MAX - constant SIBLING-NAME-MAX

: FS-MUT-ATOMIC-U+ ( n SPAN:span<u8> n -- n ) {: value:n dst off:n :}
   value 0 < if E-FS-PATH throw then
   value 10 >= if value 10 / dst off RECURSE else off then {: next:n :}
   value 10 mod 48 + dst next SPAN:U8!
   next 1+ ;

: FS-MUT-PATH-COLLISION? ( ptr u8 n -- bool ) {: a:ptr u :}
   a u EXISTS? if 0 0= exit then
   a u SYMLINK? ;

: FS-MUT-BUILD-ATOMIC-TMP ( ptr u8 n n n -- ptr u8 n ) {: path:ptr pathu seed attempt :}
   pathu 0 < if E-FS-PATH throw then
   pathu SIBLING-PATH-MAX > if E-SPAN-CAPACITY throw then
   FS-MUT-ATOMIC-PATH {: dst :}
   path pathu dst SPAN:COPY
   s" .tmp-" dst pathu SPAN:SKIP SPAN:COPY
   seed dst pathu 5 + FS-MUT-ATOMIC-U+ {: off:n :}
   FS-MUT-DASH dst off SPAN:U8!
   attempt dst off 1+ FS-MUT-ATOMIC-U+ {: u:n :}
   dst u SPAN:TAKE SPAN:$ ;

package FS-ATOMIC
PROCESS-SYMBOLS
FUNCTION: OPEN-CALL open ( ptr u8 n n -- i32 ) 2 VARIADIC ;FUNCTION
FUNCTION: CLOSE-CALL close ( n -- i32 ) ;FUNCTION

: OPEN-FLAGS ( -- n )
   HB-TARGET-LINUX-KERNEL? if $C1 exit then
   HB-TARGET-MACOS? if $A01 exit then
   E-FS-OPEN throw ;

public

\ Habu's portable `open` flags do not include O_EXCL. This narrow libc call
\ selects the host flag while keeping exclusive creation in one syscall.
: OPEN-EXCLUSIVE ( ptr u8 n -- n )
   OPEN-FLAGS swap OPEN-CALL ;

: CLOSE-FD ( n -- n )
   CLOSE-CALL ;
;package

: FS-MUT-ATOMIC-OPEN-CANDIDATE ( ptr u8 n n -- n ) {: path:ptr pathu mode :}
   path pathu FS-PATHZ mode FS-ATOMIC:OPEN-EXCLUSIVE {: fd :}
   fd 0 >= if fd exit then
   path pathu FS-MUT-PATH-COLLISION? if -1 exit then
   E-FS-OPEN throw ;

: FS-MUT-ATOMIC-RESERVE ( ptr u8 n n n -- ptr u8 n n ) {: path:ptr pathu seed mode :}
   0 begin dup FS-MUT-ATOMIC-RETRIES < while
      dup {: attempt:n :}
      path pathu seed attempt FS-MUT-BUILD-ATOMIC-TMP
      2dup mode FS-MUT-ATOMIC-OPEN-CANDIDATE {: fd:n :}
      fd 0 >= if rot drop fd exit then
      2drop 1+
   repeat drop
   E-FS-IO throw ;

: FS-MUT-ATOMIC-WRITE-FD ( n ptr u8 n -- ) {: fd src:ptr srcu :}
   0 begin dup srcu < while
      {: off:n :}
      fd src off + srcu off - write {: wr:n :}
      wr 0 <= if E-FS-IO throw then
      wr srcu off - > if E-FS-IO throw then
      off wr +
   repeat drop ;

: FS-MUT-ATOMIC-WRITE-RUN ( n ptr u8 n ptr u8 n n -- n ptr u8 n ptr u8 n n )
   {: fd src:ptr srcu temp:ptr tempu mode :}
   fd src srcu FS-MUT-ATOMIC-WRITE-FD
   mode 0 >= if temp tempu mode CHMOD-MODE then
   fd src srcu temp tempu mode ;

: FS-MUT-ATOMIC-RENAME-RUN ( ptr u8 n ptr u8 n -- ptr u8 n ptr u8 n )
   {: temp:ptr tempu path:ptr pathu :}
   temp tempu path pathu RENAME-FILE
   temp tempu path pathu ;

: FS-MUT-ATOMIC-CLEAN-TEMP ( ptr u8 n -- ) {: temp:ptr tempu :}
   temp tempu FS-MUT-PATH-COLLISION? if temp tempu REMOVE-FILE then ;

\ Index of the last component, after its preceding slash, or zero when there
\ is no slash. The source is already a checked, NUL-padded path.
: FS-MUT-ATOMIC-BASE-OFF ( ptr u8 n -- n )
   {: path:ptr pathu:n :}
   pathu begin dup 0 > while
      dup 1 - path + c@ FS-MUT-SLASH = if exit then
      1-
   repeat ;

\ Resolve the complete spelling when it exists, including its final symlink.
\ For a new basename, resolve its parent and append the basename unchanged.
\ The answer is absolute and borrowed from this task's PATHZ2 buffer until the
\ next use of that buffer; callers retaining it copy it.
\ This buffer survives sibling reservation and is used unchanged at rename.
: FS-MUT-ATOMIC-TARGET ( ptr u8 n -- ptr u8 n )
   {: path:ptr pathu:n :}
   path pathu FS-PATHZ {: input:ptr :}
   input FS-MUT-PATHZ2-BUF SPAN:$ drop FS-PATHZ-CAP realpath {: u:n :}
   u -2 = if E-FS-PATH throw then
   u 0 >= if FS-MUT-PATHZ2-BUF SPAN:$ drop u exit then
   input pathu SYMLINK? if E-FS-STAT throw then
   input pathu FS-MUT-ATOMIC-BASE-OFF {: off:n :}
   pathu off - 0= if E-FS-STAT throw then
   input off + pathu off - FS-MUT-ATOMIC-PATH SPAN:COPY
   off 0= if s" ." FS-PATHZ else
      off 1 = if s" /" FS-PATHZ else
         input off 1 - FS-PATHZ
      then
   then {: parent:ptr :}
   parent FS-MUT-PATHZ2-BUF SPAN:$ drop FS-PATHZ-CAP realpath {: parentu:n :}
   parentu -2 = if E-FS-PATH throw then
   parentu 0 < if E-FS-STAT throw then
   FS-MUT-PATHZ2-BUF SPAN:$ drop parentu
   FS-MUT-ATOMIC-PATH SPAN:$ drop pathu off -
   FS-MUT-PATHZ2-BUF FS-PATH-CAP SPAN:TAKE JOIN-PATH-INTO {: targetu:n :}
   FS-MUT-PATHZ2-BUF SPAN:$ drop targetu ;

: FS-MUT-ATOMIC-OLD-MODE ( ptr u8 n -- n ) {: path:ptr pathu :}
   path pathu FS-TRY-STAT if
      FS-STAT-MODE@ dup S-IFMT and S-IFREG = if FS-MUT-MODE-PERM and exit then
      drop
   then
   -1 ;

: FS-MUT-ATOMIC-START ( ptr u8 n n -- ptr u8 n n ptr u8 n n ) {: path:ptr pathu seed :}
   path pathu FS-MUT-ATOMIC-TARGET {: dst:ptr dstu :}
   dst dstu FS-MUT-ATOMIC-OLD-MODE {: mode:n :}
   dst dstu mode
   dst dstu seed
   mode 0 >= if FS-MUT-MODE-PRIVATE-FILE else FS-MODE-0644 then
   FS-MUT-ATOMIC-RESERVE ;

: FS-MUT-ATOMIC-WRITE-SEED ( ptr u8 n ptr u8 n n -- ) {: path:ptr pathu src:ptr srcu seed :}
   srcu 0 < if E-FS-CAPACITY throw then
   path pathu seed FS-MUT-ATOMIC-START {: dst:ptr dstu mode temp:ptr tempu fd :}
   fd src srcu temp tempu mode [: FS-MUT-ATOMIC-WRITE-RUN ;] catch {: write-code:n :}
   2drop 2drop 2drop
   \ Close exactly once, before publish. A failed close may already have freed
   \ the descriptor, so retrying it could close an unrelated recycled fd.
   fd FS-ATOMIC:CLOSE-FD {: close-code:n :}
   write-code 0<> close-code 0<> or if
      temp tempu FS-MUT-ATOMIC-CLEAN-TEMP
      close-code 0<> if E-FS-IO throw then
      write-code throw
   then
   temp tempu dst dstu [: FS-MUT-ATOMIC-RENAME-RUN ;] catch {: rename-code:n :}
   2drop 2drop
   rename-code 0<> if
      temp tempu FS-MUT-ATOMIC-CLEAN-TEMP
      rename-code throw
   then ;

\ Two live processes never share a pid, and the clock tells a reused pid from a
\ sibling its last owner left behind.
: FS-MUT-ATOMIC-SEED ( -- n )
   mono-ns getpid xor ;

: ATOMIC-WRITE-FILE ( ptr u8 n ptr u8 n -- ) {: path:ptr pathu src:ptr srcu :}
   path pathu src srcu FS-MUT-ATOMIC-SEED FS-MUT-ATOMIC-WRITE-SEED ;

\ Create and open the unique `.tmp-*` sibling ATOMIC-WRITE-FILE stages through,
\ for a writer that streams its file or must finish it before RENAME-FILE
\ publishes it. Answers the sibling's path, which stays this task's until its
\ next reservation, and the descriptor. The caller owns both and the removal.
: RESERVE-SIBLING ( ptr u8 n -- ptr u8 n n )
   FS-MUT-ATOMIC-SEED FS-MODE-0644 FS-MUT-ATOMIC-RESERVE ;

\ The longest base MAKE-TEMP-DIR makes a directory in for a prefix of n bytes:
\ its `<base>/<prefix>-<seed>-<attempt>` path still fits FS-PATH-CAP with the
\ largest seed, the largest nonnegative cell, and the last attempt, so whether
\ a base has room for one never depends on the seed it drew.
: TEMP-DIR-BASE-MAX ( n -- n ) {: prefixu:n :}
   FS-PATH-CAP 3 - prefixu -
   -1 1 rshift FS-MUT-U-DIGITS -
   FS-MUT-TMP-RETRIES 1- FS-MUT-U-DIGITS - ;

: FS-MUT-BUILD-TEMP-TRY ( ptr u8 n ptr u8 n n n -- ptr u8 n ) {: base:ptr baseu prefix:ptr prefixu seed attempt :}
   baseu prefixu TEMP-DIR-BASE-MAX > if E-SPAN-CAPACITY throw then
   SB-RESET
   base baseu SB-APPEND
   base baseu FS-JOIN-SEPARATED? 0= if FS-MUT-SLASH SB-APPEND-C then
   prefix prefixu SB-APPEND
   FS-MUT-DASH SB-APPEND-C
   seed FS-MUT-SB-U
   FS-MUT-DASH SB-APPEND-C
   attempt FS-MUT-SB-U
   SB$ {: a:ptr u :}
   a u FS-MUT-TMP-PATH SPAN:COPY
   FS-MUT-TMP-PATH u SPAN:TAKE SPAN:$ ;

: FS-MUT-TMP-COLLISION? ( ptr u8 n -- bool ) {: a:ptr u :}
   a u FS-MUT-PATH-COLLISION? ;

: FS-MUT-MKDIR-CANDIDATE? ( ptr u8 n -- bool ) {: a:ptr u :}
   a u FS-PATHZ FS-MUT-MODE-PRIVATE-DIR mkdir {: rc :}
   rc 0= if 0 0= exit then
   a u FS-MUT-TMP-COLLISION? if 0 0= 0= exit then
   E-FS-IO throw ;

: FS-MUT-MAKE-TEMP-DIR-SEED ( ptr u8 n ptr u8 n n -- ptr u8 n ) {: base:ptr baseu prefix:ptr prefixu seed :}
   0 begin dup FS-MUT-TMP-RETRIES < while
      dup {: attempt:n :}
      base baseu prefix prefixu seed attempt FS-MUT-BUILD-TEMP-TRY
      2dup FS-MUT-MKDIR-CANDIDATE? if rot drop exit then
      2drop 1+
   repeat drop
   E-FS-IO throw ;

\ MAKE-TEMP-DIR makes the directory under the base as given, and the answer
\ keeps the caller's spelling: a relative base gives a relative answer.
: MAKE-TEMP-DIR ( ptr u8 n ptr u8 n -- ptr u8 n )
   mono-ns FS-MUT-MAKE-TEMP-DIR-SEED ;

\ realpath of a temp root into this task's PATHZ2 buffer: the canonical
\ spelling's length, below 0 when realpath refuses the root. MAKE-TEMP-DIR
\ builds and makes its candidates through SB, FS-MUT-TMP-PATH and FS-PATHZ
\ only, so the spelling stands through every attempt.
: FS-MUT-TEMP-REAL ( ptr u8 n -- n )
   FS-PATHZ FS-MUT-PATHZ2-BUF SPAN:$ drop FS-PATHZ-CAP realpath ;

\ The roots read from the environment are resolved before MAKE-TEMP-DIR gets
\ them, so every tree TMPDIR-MKDIR and HB-TMP-MKDIR make has a canonical path:
\ a root reached through a symlink (macOS's /tmp is /private/tmp) would
\ otherwise give paths that differ from the canonical ones the engine records
\ for the same files. TEMP-DIR-BASE-MAX bounds the canonical spelling. A TMPDIR
\ longer than FS-PATH-CAP throws E-FS-PATH from FS-PATHZ; one that realpath
\ refuses throws E-FS-IO.
: TMPDIR-MKDIR ( ptr u8 n -- ptr u8 n ) {: prefix:ptr prefixu :}
   s" TMPDIR" GETENV dup 0= if 2drop s" /tmp" then
   FS-MUT-TEMP-REAL {: real:n :}
   real 0 < if E-FS-IO throw then
   FS-MUT-PATHZ2-BUF SPAN:$ drop real prefix prefixu MAKE-TEMP-DIR ;

\ An HB_TMP that cannot hold the temp directory is refused before anything is
\ made, with one line on fd 2 that names HB_TMP, why, and its value, then a
\ throw. A value past FS-PATH-CAP, or one whose canonical spelling leaves no
\ room for the temp name under it (TEMP-DIR-BASE-MAX), is too long:
\ E-SPAN-CAPACITY; without the check the first dies on FS-PATHZ's E-FS-PATH and
\ the second on MAKE-TEMP-DIR's E-SPAN-CAPACITY, neither naming HB_TMP. A
\ value that is not a directory throws E-FS-DIR; without the check it dies on
\ the E-FS-IO of a mkdir under a missing parent, which is no collision. A
\ directory realpath refuses, as one whose canonical spelling does not fit
\ PATH_MAX (ENAMETOOLONG on Darwin), has no canonical path: E-FS-IO. The check
\ answers the canonical spelling, borrowed from this task's PATHZ2 buffer,
\ which HB-TMP-MKDIR makes the directory under. The line is three
\ writes, as src/core/include.f names a refused source path, and not one SB$
\ write as FS-MUT-EXIT-REPORT below: a value of FS-PATH-CAP bytes with the
\ reason overflows SB-CAP, and SB-APPEND would then throw E-STR-CAPACITY in
\ place of the line and the code.
: FS-MUT-HB-TMP-REFUSE ( ptr u8 n ptr u8 n -- ) {: a:ptr u why:ptr whyu :}
   2 why whyu write drop
   2 a u write drop
   2 S\" \n" write drop ;

: FS-MUT-HB-TMP-LONG ( ptr u8 n -- ) {: a:ptr u :}
   a u s" hb: HB_TMP is too long for a temp directory: " FS-MUT-HB-TMP-REFUSE
   E-SPAN-CAPACITY throw ;

: FS-MUT-HB-TMP-CHECK ( ptr u8 n n -- ptr u8 n ) {: a:ptr u prefixu :}
   u FS-PATH-CAP > if a u FS-MUT-HB-TMP-LONG then
   a u DIR? 0= if
      a u s" hb: HB_TMP is not a directory: " FS-MUT-HB-TMP-REFUSE
      E-FS-DIR throw
   then
   a u FS-MUT-TEMP-REAL {: real:n :}
   real 0 < if
      a u s" hb: HB_TMP has no canonical path: " FS-MUT-HB-TMP-REFUSE
      E-FS-IO throw
   then
   real prefixu TEMP-DIR-BASE-MAX > if a u FS-MUT-HB-TMP-LONG then
   FS-MUT-PATHZ2-BUF SPAN:$ drop real ;

\ The one base resolver for a process that was handed a scratch root. HB_TMP
\ names a directory the caller owns and will remove; a temp tree made under it
\ therefore goes away with it, even for a process that is killed before it can
\ run its own CLEANUP-RUN. With no HB_TMP the tree falls back to TMPDIR (then
\ /tmp), which nobody reaps - so a maker that leaks there leaks for good.
: HB-TMP-MKDIR ( ptr u8 n -- ptr u8 n ) {: prefix:ptr prefixu :}
   s" HB_TMP" GETENV dup 0= if
      2drop prefix prefixu TMPDIR-MKDIR exit
   then
   prefixu FS-MUT-HB-TMP-CHECK prefix prefixu MAKE-TEMP-DIR ;

: CLEANUP-RESET ( -- )
   0 FS-MUT-CLEANUP-N ! ;

: FS-MUT-CLEANUP-REMOVE ( n -- ) {: idx :}
   idx FS-MUT-CLEANUP-SLOT idx FS-MUT-CLEANUP-U-PTR @ SPAN:TAKE SPAN:$ {: a:ptr u :}
   idx FS-MUT-CLEANUP-KIND-PTR @ {: kind :}
   a u SYMLINK? if a u REMOVE-FILE exit then
   a u EXISTS? 0= if exit then
   kind FS-MUT-CLEANUP-TREE = if
      a u REMOVE-TREE
   else
   kind FS-MUT-CLEANUP-DIR = if
      a u REMOVE-DIR
   else
      a u REMOVE-FILE
   then then ;

: CLEANUP-RUN ( -- )
   FS-MUT-CLEANUP-N @ begin dup 0 > while
      1 - dup FS-MUT-CLEANUP-REMOVE
   repeat drop
   CLEANUP-RESET ;

\ ---- the registry runs at process exit ---------------------------------------
\ The table above is walked by CLEANUP-RUN, and an explicit CLEANUP-RUN used to
\ be the only thing that emptied it: a `die`, an uncaught top-level throw or a
\ test report that dies left every registered path on disk. The engine carries a
\ process-exit vector for exactly this (src/habu/layout.f EXIT-HOOK-CELL): it
\ calls the armed word once, with the vector already cleared, immediately before
\ the exit_group of a normal top-level exit, a `die`, an uncaught throw, or a
\ stripped application's return from MAIN.
\
\ THE REGISTRY IS PER PROCESS. A restored image starts with an empty table and
\ the relocated vector, so CLEANUP-AT-EXIT walks nothing there until that
\ process registers a path of its own.
\
\ A removal that fails is REPORTED, never swallowed. CLEANUP-RUN throws on a
\ path it cannot remove, and the process is already on its way out, so the code
\ goes to fd 2 as one line and the exit code the process is carrying is left
\ alone - the engine preserves it across the call. The line is assembled in the
\ shared string builder and the code rendered by fmt, although both live in
\ engine bands below a stripped image's DATA window: a stripped image maps
\ those bands fresh and zeroed, which is all a render buffer needs. Measured: a
\ stripped MAIN that requires src/lib/fmt.f prints -2105 through both FMT:.INT and
\ SB-RESET FMT:SB-INT SB$. The builder is reset here because the process is
\ exiting and nothing that ran before this line reads it again.
: FS-MUT-EXIT-REPORT ( n -- ) {: code :}
   SB-RESET
   s" hb: cleanup at exit threw " SB-APPEND
   code FMT:SB-INT
   S\" \n" SB-APPEND
   2 SB$ write drop ;

\ This file's ONE raw boundary is package FS-EXIT below: the exit vector is a
\ fixed engine DATA cell holding a code pointer, reached through `data-base`.
\ Arming is the only place an opaque xt is handled; calling one back is ordinary
\ checked code, because the saved vector lands in a typed xt cell.
\
\ THE VECTOR IS A CHAIN, not a claim. Arming finds one of three states: zero,
\ and ours goes in; already ours, and nothing happens; FOREIGN - some other
\ component owns the process's single exit slot - and that vector is saved here
\ before ours replaces it, so registering a path never costs the process the
\ hook it already had, and a process whose slot was taken still gets its
\ registry run. CLEANUP-AT-EXIT calls the saved vector last, after the registry:
\ the paths this file was asked to remove are gone before foreign code runs.
\
\ The saved vector is PUT BACK TO THE NO-OP BEFORE IT IS CALLED, for the reason
\ the engine clears its cell before calling us: a chained hook that registers a
\ path of its own re-arms the engine cell and then dies, which re-enters
\ CLEANUP-AT-EXIT, and it has to find the chain empty there rather than itself.
\
\ REGISTERING is what arms it, not loading this file: a stripped application
\ restores no engine cell, so its vector is the fresh mapping's zero until its
\ own CLEANUP+ call fills it, and a process that registers nothing has nothing
\ to run and leaves the vector exactly where it found it.
\
\ The chain cell is a TYPED xt cell holding a word that is always callable, not
\ a raw cell holding an xt or zero. Both halves are load-bearing: the native
\ compiler needs the convention of the call it is compiling, and a raw cell's
\ value carries none (`ncomp: cannot compile FS-MUT-EXIT-CHAIN at execute`,
\ E-NELAB-QUOT, when a stripped image's closure reaches this word); and a cell
\ that is never empty needs no zero test on an exit path, where a stray zero
\ would be a jump to address 0 rather than a caught error.
: FS-MUT-EXIT-NONE ( -- ) ;

TYPED-VARIABLE FS-MUT-EXIT-PREV [ -- ]

: FS-MUT-EXIT-INIT ( -- )
   [: FS-MUT-EXIT-NONE ;] FS-MUT-EXIT-PREV ! ;
FS-MUT-EXIT-INIT

: FS-MUT-EXIT-CHAIN ( -- )
   FS-MUT-EXIT-PREV @
   [: FS-MUT-EXIT-NONE ;] FS-MUT-EXIT-PREV !
   execute ;

: CLEANUP-AT-EXIT ( -- )
   [: CLEANUP-RUN ;] catch {: code :}
   code 0<> if code FS-MUT-EXIT-REPORT then
   FS-MUT-EXIT-CHAIN ;

\ The vector cell holds a code pointer or zero, so it has two accessors: the
\ number the zero and identity tests read, and the quotation slot a nonzero
\ vector is read and every arming written through. Only a private CAST: gives a
\ DATA address a quotation pointee.
package FS-EXIT
private
CAST: >VECTOR ( n -- ptr [ -- ] )
CAST: XT>N ( [ -- ] -- n )

: VECTOR-N ( -- ptr n ) data-base EXIT-HOOK-CELL + ;

: VECTOR ( -- ptr [ -- ] )
   data-base BYTE-VIEW NULL-PTR BYTE-VIEW - EXIT-HOOK-CELL + >VECTOR ;

public
: ARM ( -- )
   VECTOR-N @ {: current:n :}
   current 0= if ['] CLEANUP-AT-EXIT VECTOR ! exit then
   current ['] CLEANUP-AT-EXIT XT>N = if exit then
   VECTOR @ FS-MUT-EXIT-PREV !
   ['] CLEANUP-AT-EXIT VECTOR ! ;
;package

\ REGISTRATION IS SAFE FROM ANY TASK although the table is the process's: the
\ slot is claimed with one atomic-add on the count - `atomic-add` is an engine
\ primitive (src/habu/prims.f), so this needs no src/lib/task.f - and the claimer
\ writes nothing but the slot it was handed. The count is what CLEANUP-RUN walks
\ down from, so a claim past the table's end is GIVEN BACK before the refusal;
\ otherwise a refused 65th path would leave a count no run could walk. The
\ claimed slot's length is zeroed before the copy, so a slot claimed by a
\ registration that then throws is empty rather than whatever the last round
\ left there, and CLEANUP-RUN steps over it.
: FS-MUT-CLEANUP+ ( ptr u8 n n -- ) {: a:ptr u kind :}
   FS-EXIT:ARM
   u 0 < if E-FS-PATH throw then
   kind FS-MUT-CLEANUP-FILE <>
   kind FS-MUT-CLEANUP-DIR <> and
   kind FS-MUT-CLEANUP-TREE <> and if E-FS-IO throw then
   1 FS-MUT-CLEANUP-N atomic-add {: idx :}
   idx FS-MUT-CLEANUP-MAX >= if
      -1 FS-MUT-CLEANUP-N atomic-add drop
      E-FS-CAPACITY throw
   then
   0 idx FS-MUT-CLEANUP-U-PTR !
   a u idx FS-MUT-CLEANUP-SLOT SPAN:COPY
   kind idx FS-MUT-CLEANUP-KIND-PTR !
   u idx FS-MUT-CLEANUP-U-PTR ! ;

: CLEANUP+ ( ptr u8 n -- )
   FS-MUT-CLEANUP-FILE FS-MUT-CLEANUP+ ;

: CLEANUP-DIR+ ( ptr u8 n -- )
   FS-MUT-CLEANUP-DIR FS-MUT-CLEANUP+ ;

: CLEANUP-TREE+ ( ptr u8 n -- )
   FS-MUT-CLEANUP-TREE FS-MUT-CLEANUP+ ;

\ A PATH ITS OWNER HAS REMOVED IS FORGOTTEN, so the table holds only what is
\ still owed: a process that makes and removes a tree per job registers each
\ one and forgets it once it is gone, and its count never walks toward
\ FS-MUT-CLEANUP-MAX. The newest slot naming the path is emptied - the state
\ CLEANUP-RUN steps over - and, when it is the top of the table, given back
\ with one atomic-cas on the count. A claim another task made meanwhile defeats
\ the cas, keeps its own slot, and leaves the emptied one empty. A path no slot
\ names is a no-op; an empty path is E-FS-PATH, since an empty slot is not one.
: FS-MUT-CLEANUP-NAMES? ( ptr u8 n n -- bool ) {: a:ptr u idx :}
   idx FS-MUT-CLEANUP-U-PTR @ u <> if 0 0= 0= exit then
   idx FS-MUT-CLEANUP-SLOT u SPAN:TAKE SPAN:$ a u STR= ;

\ The newest slot naming the path, or -1.
: FS-MUT-CLEANUP-FIND ( ptr u8 n -- n ) {: a:ptr u :}
   FS-MUT-CLEANUP-N @ begin dup 0 > while
      1 -
      dup >r a u r> FS-MUT-CLEANUP-NAMES? if exit then
   repeat
   drop -1 ;

: CLEANUP-FORGET ( ptr u8 n -- ) {: a:ptr u :}
   u 0 <= if E-FS-PATH throw then
   a u FS-MUT-CLEANUP-FIND {: idx :}
   idx 0 < if exit then
   0 idx FS-MUT-CLEANUP-U-PTR !
   idx 1 + idx FS-MUT-CLEANUP-N atomic-cas drop ;

\ ---- replacing a file through its sibling --------------------------------------

\ Stack-preserving under `catch`: the caller still holds the sibling to remove
\ when a step throws.
: FS-MUT-STAGED-RUN ( R ptr u8 n ptr u8 n [ R ptr u8 n -- R ] -- R ptr u8 n ptr u8 n [ R ptr u8 n -- R ] )
   {: path:ptr pathu tmp:ptr tmpu fill :}
   tmp tmpu CLEANUP+
   tmp tmpu fill execute
   tmp tmpu path pathu RENAME-FILE
   path pathu tmp tmpu fill ;

: FS-MUT-STAGED-CLEAN ( ptr u8 n -- ptr u8 n )
   2dup FS-MUT-ATOMIC-CLEAN-TEMP ;

: FS-MUT-STAGED-FORGET ( ptr u8 n -- ptr u8 n )
   2dup CLEANUP-FORGET ;

\ Reporting is best effort after a primary failure: a closed stderr must not
\ replace the fill or rename error. Write the path separately from the 1 KiB
\ string builder so even the longest legal sibling can be named.
: FS-MUT-STAGED-REPORT ( ptr u8 n n ptr u8 n -- )
   {: label:ptr labelu:n code:n path:ptr pathu:n :}
   SB-RESET
   s" hb: staged replacement " SB-APPEND
   label labelu SB-APPEND
   s"  threw " SB-APPEND
   code FMT:SB-INT
   s"  at " SB-APPEND
   2 SB$ write drop
   2 path pathu write drop
   2 S\" \n" write drop ;

\ Replace the file at path with the one fill writes at the path it is handed:
\ the sibling RESERVE-SIBLING creates, its descriptor closed, renamed over path
\ once fill returns. Until then path holds what it held, so it names its old
\ file or the whole new one. The sibling is registered for removal at exit
\ while it is held, so a throw from fill or the rename, or a `die` in fill,
\ takes it with it; only a killed process leaves one behind. A full registry
\ refuses the claim, and the sibling goes then. fill must not reserve a
\ sibling of its own (ATOMIC-WRITE-FILE does): RESERVE-SIBLING's path is this
\ task's only until its next reservation.
\ A caller may pass context below path; fill must return that context unchanged
\ in shape. It can carry typed pointers and complete layout values directly.
: REPLACE-STAGED ( R ptr u8 n [ R ptr u8 n -- R ] -- R )
   {: path:ptr pathu fill :}
   path pathu RESERVE-SIBLING {: tmp:ptr tmpu fd :}
   \ Closed once, before fill opens the sibling by path; ATOMIC-WRITE-FILE says
   \ why a failed close is not retried.
   fd FS-ATOMIC:CLOSE-FD 0<> if
      tmp tmpu [: FS-MUT-STAGED-CLEAN ;] catch {: clean-code:n :}
      2drop
      clean-code 0<> if
         s" close cleanup" clean-code tmp tmpu FS-MUT-STAGED-REPORT
      then
      E-FS-IO throw
   then
   path pathu tmp tmpu fill [: FS-MUT-STAGED-RUN ;] catch {: code:n :}
   drop 2drop 2drop
   code 0<> if
      tmp tmpu [: FS-MUT-STAGED-CLEAN ;] catch {: clean-code:n :}
      2drop
      clean-code 0= if
         tmp tmpu [: FS-MUT-STAGED-FORGET ;] catch {: forget-code:n :}
         2drop
         forget-code 0<> if
            s" cleanup registration" forget-code tmp tmpu FS-MUT-STAGED-REPORT
         then
      else
         s" cleanup" clean-code tmp tmpu FS-MUT-STAGED-REPORT
      then
      code throw
   then
   tmp tmpu CLEANUP-FORGET ;
