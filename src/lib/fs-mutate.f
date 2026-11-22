split-str
\ fs-mutate.f - checked filesystem mutation: directories, links, copies, atomic
\ and staged replacement, temp paths, and the process exit cleanup registry.
\ Public names avoid the primitives' (the dictionary is case-insensitive):
\ MAKE-DIR and REMOVE-DIR, not mkdir or rmdir.
\ STORAGE CLASS. TASK-LOCAL staged paths; PROCESS-WIDE registry.

require src/lib/error.f
require src/lib/string.f
require src/lib/fmt.f
require src/lib/fs.f
require src/lib/fd.f
require src/lib/fs-identity.f
require src/lib/ffi-abi.f
require src/lib/span.f
require src/lib/memory.f
require src/lib/at-exit.f

package FS

private
$FFF constant MODE-PERM
73 constant MODE-EXEC

public
384 constant MODE-PRIVATE-FILE
448 constant MODE-PRIVATE-DIR

private
493 constant MODE-DIR
8192 constant COPY-CAP
64 constant ATOMIC-RETRIES
64 constant TMP-RETRIES
0 constant CLEANUP-FILE
1 constant CLEANUP-DIR
2 constant CLEANUP-TREE
$2D constant DASH
4 constant RENAME-EXCL                \ renameatx_np's RENAME_EXCL
1 constant NO-REPLACE                 \ renameat2's RENAME_NOREPLACE

\ The cleanup registry is one MEM:ALLOC-CELLS row the process owns. Slot i is
\ CLEANUP-STRIDE cells at cell i * CLEANUP-STRIDE: the path's length, its kind,
\ then PATH-CAP path bytes. CLEANUP-N slots are in use, oldest first, of
\ CLEANUP-CAP mapped. A full row doubles (CLEANUP-FIRST slots the first time),
\ and the new row and its capacity are published before the old row is released.
\ CLEANUP-LOCK serializes registration, forgetting, growth and the run, and
\ holds its owner: the address of that task's DATA region, unique per live task.
\
\ The process keeps its registrations and a built program starts with none:
\ CLEANUP-N and CLEANUP-CAP are scratch cells and CLEANUP-ROW a PTR-VARIABLE,
\ each written as 0 in the product.
PATH-CAP CELL 1- + CELL / 2 + constant CLEANUP-STRIDE
8 constant CLEANUP-FIRST
PTR-VARIABLE CLEANUP-ROW
SCRATCH-VARIABLE CLEANUP-CAP

SCRATCH-VARIABLE CLEANUP-N
variable CLEANUP-LOCK

\ The staged paths are task-local, in the FS-MUT-ABI band of the per-task DATA
\ region (`data-base` is the running task's): the path FS:MKTEMP-DIR-IN or
\ FS:MKTEMP answers stays this task's until it calls either again, and a new
\ task's paths start empty.
\
\ layout.f is loaded before this file and cannot see PATHZ-CAP or
\ PATH-CAP, so the band states those widths and this executes the agreement
\ once at load.
: SCRATCH-BAND-AGREE ( -- )
   PATHZ-CAP FS-MUT-ABI:PATHZ2-BYTES <> if E-FS-BAND throw then
   PATH-CAP FS-MUT-ABI:ATOMIC-BYTES <> if E-FS-BAND throw then
   PATH-CAP FS-MUT-ABI:TMP-BYTES <> if E-FS-BAND throw then ;
SCRATCH-BAND-AGREE

: PATHZ2-BUF ( -- SPAN:span<u8> )
   data-base FS-MUT-ABI:PATHZ2-OFF + BYTE-VIEW PATHZ-CAP SPAN:MAKE ;

: ATOMIC-PATH ( -- SPAN:span<u8> )
   data-base FS-MUT-ABI:ATOMIC-OFF + BYTE-VIEW PATH-CAP SPAN:MAKE ;

: TMP-PATH-BUF ( -- SPAN:span<u8> )
   data-base FS-MUT-ABI:TMP-OFF + BYTE-VIEW PATH-CAP SPAN:MAKE ;

: >PATHZ2 ( str -- ^u8 )
   PATHZ2-BUF PATHZ-INTO ;

public
\ A path that does not fit dst is E-FS-CAPACITY, with nothing written: the
\ suffix meets the room the path leaves, which no length can wrap.
: SUFFIX-PATH ( str str SPAN:span<u8> -- n ) {: a:^ u s:^ su dst :}
   CAUSE-CLEAR
   u 0 < if E-FS-PATH throw then
   su 0 < if E-FS-PATH throw then
   dst SPAN:LEN {: cap :}
   su cap u - > if E-FS-CAPACITY throw then
   a u dst SPAN:COPY
   s su dst u SPAN:SKIP SPAN:COPY
   u su + ;

private
: CLEANUP-AT ( n -- ^n ) {: idx :}
   idx 0 < if E-FS-CAPACITY throw then
   idx CLEANUP-CAP @ >= if E-FS-CAPACITY throw then
   CLEANUP-ROW @ idx CLEANUP-STRIDE * cells + ;

: CLEANUP-KIND ( n -- ^n ) CLEANUP-AT CELL + ;

: CLEANUP-SLOT ( n -- SPAN:span<u8> )
   CLEANUP-AT 2 cells + BYTE-VIEW PATH-CAP SPAN:MAKE ;

: CLEANUP-PATH ( n -- str ) {: idx :}
   idx CLEANUP-SLOT idx CLEANUP-AT @ SPAN:TAKE SPAN:$ ;

\ Under the lock. A refused mapping leaves the row, its capacity and N as found.
: CLEANUP-GROW ( -- )
   CLEANUP-CAP @ {: cap :}
   cap 2 * CLEANUP-FIRST max {: grown :}
   grown CLEANUP-STRIDE * MEM:CELLS-ALLOC-COUNT MEM:ALLOC-CELLS {: row:^ :}
   CLEANUP-ROW @ {: old:^ :}
   cap 0 > if
      old BYTE-VIEW row BYTE-VIEW
      CLEANUP-N @ CLEANUP-STRIDE * cells BYTE-COPY
   then
   row CLEANUP-ROW !
   grown CLEANUP-CAP !
   cap 0 > if
      old cap CLEANUP-STRIDE * MEM:CELLS-ALLOC-COUNT MEM:RELEASE-CELLS
   then ;

: CLEANUP-SELF ( -- n ) data-base BYTE-VIEW NULL-PTR BYTE-VIEW - ;

: CLEANUP-ACQUIRE ( -- )
   CLEANUP-SELF {: self :}
   begin 0 self CLEANUP-LOCK atomic-cas 0= until ;

: CLEANUP-RELEASE ( -- ) 0 CLEANUP-LOCK atomic! ;

public
\ A throw sets FS:ERRNO.
: REMOVE-FILE ( str -- ) {: a:^ u :}
   a u >PATHZ unlink 0 < if E-FS-IO throw then ;

\ A throw sets FS:ERRNO.
: RENAME-FILE ( str str -- ) {: src:^ srcu dst:^ dstu :}
   src srcu >PATHZ dst dstu >PATHZ2 rename 0 < if E-FS-IO throw then ;

private
PROCESS-SYMBOLS
FUNCTION: LINK-CALL link ( ^u8 ^u8 -- i32 ) ;FUNCTION
FUNCTION: RENAMEAT2-CALL renameat2 ( n ^u8 n ^u8 n -- i32 ) ;FUNCTION
FUNCTION: RENAMEATX-CALL renameatx_np ( n ^u8 n ^u8 n -- i32 ) ;FUNCTION

\ One rename that never replaces dst, so a throw has not published; false, src
\ kept, if dst names an entry. File systems without it (NFS, 9p) throw E-FS-IO.
: RENAME-NOREPLACE ( n ^u8 ^u8 -- bool )
   {: dir src:^ dst:^ :}
   HB-TARGET-MACOS? if dir src dir dst RENAME-EXCL RENAMEATX-CALL
   else dir src dir dst NO-REPLACE RENAMEAT2-CALL then
   0= if true exit then
   FFI:ERRNO EEXIST = if false exit then
   E-FS-IO ERRNO-FAIL ;

public
\ A throw sets FS:ERRNO.
: CHMOD-MODE ( str n -- ) {: a:^ u mode :}
   a u >PATHZ mode chmod 0 < if E-FS-IO throw then ;

\ A throw sets FS:ERRNO.
: CHMOD-X ( str -- ) {: a:^ u :}
   a u STAT-MODE MODE-PERM and MODE-EXEC or {: mode :}
   a u mode CHMOD-MODE ;

\ A throw sets FS:ERRNO.
: MAKE-SYMLINK ( str str -- ) {: target:^ targetu link:^ linku :}
   target targetu >PATHZ link linku >PATHZ2 symlink 0 < if E-FS-IO throw then ;

\ A hard link; an existing LINK is E-FS-IO, not replaced. A throw sets FS:ERRNO.
: MAKE-LINK ( str str -- )
   {: target:^ targetu link:^ linku :}
   target targetu >PATHZ link linku >PATHZ2 LINK-CALL 0<> if E-FS-IO ERRNO-FAIL then ;

\ A throw sets FS:ERRNO.
: MKDIR-MODE ( str n -- ) {: a:^ u mode :}
   a u >PATHZ mode mkdir 0 < if E-FS-IO throw then ;

\ A throw sets FS:ERRNO.
: MAKE-DIR ( str -- )
   MODE-DIR MKDIR-MODE ;

\ A throw sets FS:ERRNO.
: REMOVE-DIR ( str -- ) {: a:^ u :}
   a u >PATHZ rmdir 0 < if E-FS-IO throw then ;

\ WALK-UNMAP closes whatever descriptors the walk's frames still hold, however
\ the traversal ended, so the error paths below throw plainly.

private
\ The entry's own mode, never a link target's, lands in STAT-BUF. A name
\ already gone (ENOENT) is false: a missing root, or an entry removed since its
\ directory was read. Any other failure is E-FS-STAT.
: LSTAT-AT? ( n str -- bool ) {: parent:n name:^ u:n :}
   parent name u >PATHZ STAT-BUF AT-NOFOLLOW STAT-AT-CALL 0= if true exit then
   FFI:ERRNO ENOENT = if false exit then
   E-FS-STAT ERRNO-FAIL ;


: UNLINK-AT ( n str n -- ) {: parent:n name:^ u:n flags:n :}
   parent name u >PATHZ flags UNLINK-AT-CALL 0= if exit then
   E-FS-IO ERRNO-FAIL ;


\ The directory opens one depth down, O_NOFOLLOW, so a link swapped in since
\ the stat is refused, never followed. The name is staged before that depth's
\ frame is made: a child's name lies in its parent's dirent block, which
\ growing the frames moves.
: OPEN-AT ( ^n n str -- ) {: ctx:^ parent:n name:^ u:n :}
   name u >PATHZ {: z:^ :}
   ctx WALK-DEPTH@ 1 + {: d :}
   ctx d ENSURE-DEPTH
   d ctx WALK-DEPTH!
   parent z OPEN-DIR-FLAGS OPEN-AT-CALL
   dup 0 < if drop E-FS-OPEN ERRNO-FAIL then
   ctx FD!
   0 ctx BASE@ ! ;

\ The name an emptied directory is unlinked by, once its traversal is back at
\ its parent's depth. That traversal may have grown the frames and so moved the
\ parent's dirent block: a child's name is read again from its parent's
\ current entry. The root's, at depth -1, is the caller's path and never moves.
: OWN-NAME ( ^n str -- str ) {: ctx:^ name:^ u:n :}
   ctx WALK-DEPTH@ 0 < if name u exit then
   ctx CUR-ENTRY DIRENT-NAME ;

: REMOVE-AT ( ^n n str -- ) {: ctx:^ parent:n name:^ u:n :}
   parent name u LSTAT-AT? 0= if exit then
   STAT-BUF LIBC-MODE@ S-IFMT and S-IFDIR <> if
      parent name u 0 UNLINK-AT exit
   then
   ctx parent name u OPEN-AT
   begin ctx READ-DIR while
      ctx DIR-BLOCK-BEGIN
      begin ctx DIR-MORE? while
         ctx LOAD-ENTRY DIRENT-NAME 2dup SKIP-SELF-ENTRY? if
            2drop
         else
            ctx ctx FD@ 2swap RECURSE
         then
         ctx ADVANCE-ENTRY
      repeat
   repeat
   ctx CLOSE-CUR-DIR
   ctx ASCEND-PATH
   parent ctx name u OWN-NAME AT-REMOVEDIR UNLINK-AT ;


\ The root's length to stat. lstat of `link/` resolves the link, so a root
\ whose name without trailing slashes is a symlink - `link/`, `lfile/`,
\ `dang/` - is stat'ed without them and unlinked however it is spelled. Any
\ other root is stat'ed as spelled: `plain/` on a regular file stays ENOTDIR.
\ A root of slashes only keeps one, the filesystem root.
: ROOT-LEN ( str -- n ) {: a:^ u :}
   u begin
      dup 1 > if dup 1 - a + c@ SLASH = else false then
   while 1 - repeat {: s :}
   s u < if a s SYMLINK? if s exit then then
   u ;

\ Caught as a whole, so one site closes the descriptors and releases the frames
\ for every way out, a root refused by ROOT-LEN's stat included.
: REMOVE-RUN ( ^n str -- ^n str ) {: ctx:^ a:^ u :}
   ctx AT-CWD a a u ROOT-LEN REMOVE-AT
   ctx a u ;

\ No flag guards the context: it is this call's alone, so the process exit hook
\ removes trees through CLEANUP-RUN whatever a task was doing.
: REMOVE-SCOPE ( str ^u8 NUM:alloc-byte-len -- )
   drop CELL-VIEW {: a:^ u ctx:^ :}
   ctx WALK-MAP
   \ The depth names the directory being emptied: none until the root opens.
   -1 ctx WALK-DEPTH!
   ctx a u [: REMOVE-RUN ;] catch {: code:n :}
   2drop drop
   ctx WALK-UNMAP
   code 0<> if code throw then ;

public
\ Removes the tree at a, unlinking a symlink root even with a trailing slash; a
\ missing root is a no-op. Descriptors bound its depth. A throw sets FS:ERRNO.
: REMOVE-TREE ( str -- ) {: a:^ u :}
   u 0 <= if CAUSE-CLEAR E-FS-PATH throw then
   a u WALK-HANDLE-BYTES MEM:BYTES-ALLOC-LEN [: REMOVE-SCOPE ;] MEM:WITH-BYTES ;

private
\ Only EEXIST can mean the folder is already there, so only it is checked
\ with a stat, which accepts a link to a directory. Any other failure throws at
\ once with mkdir's cause; a non-directory in the way throws with EEXIST.
: MKDIR-ONE ( str -- ) {: a:^ u :}
   a u >PATHZ MODE-DIR mkdir 0 >= if exit then
   ERRNO EEXIST <> if E-FS-IO throw then
   a u DIR? 0= if E-FS-IO EEXIST CAUSE-FAIL then ;

public
\ A throw sets FS:ERRNO.
: MAKE-DIRS ( str -- ) {: a:^ u :}
   u 0 <= if CAUSE-CLEAR E-FS-PATH throw then
   1 begin dup u < while
      a over + c@ SLASH = if
         dup 0 > if a over MKDIR-ONE then
      then
      1+
   repeat drop
   a u MKDIR-ONE ;

private
: PREPARE-DST ( fd fd -- ) {: source dest :}
   source dest SAME-OPEN-FILE? if E-FS-OPEN throw then
   dest 0 FD:RESIZE ;

\ A descriptor a helper must close on its error path is a parameter: -1 stands
\ for "not open yet", which CLOSE-COPY-FD skips.
: CLOSE-COPY-FD ( n -- ) {: fd :}
   fd 0 >= if fd close then ;

: COPY-THROW ( n n n -- ) {: in out code :}
   in CLOSE-COPY-FD
   out CLOSE-COPY-FD
   code throw ;

: COPY-OPEN-SRC ( str -- n ) {: src:^ srcu :}
   src srcu READ-PATHZ open-rd {: in :}
   in 0 < if E-FS-OPEN throw then
   in ;

\ Stack-preserving under `catch`: the two descriptors are the quotation's
\ declared window and no path of this body consumes them, so the caller may drop
\ them and read its own locals after the catch.
: COPY-PREPARE-DST ( n n -- n n )
   2dup >FD swap >FD swap PREPARE-DST ;

\ Runs before the source opens, so a refused destination has nothing to close.
\ The answer lives in the PATHZ2 buffer, which opening the source leaves alone.
: COPY-DST-PATHZ ( str -- ^u8 ) {: dst:^ dstu :}
   dst dstu EXISTS? if
      dst dstu FILE? 0= if E-FS-OPEN throw then
   then
   dst dstu >PATHZ2 ;

: COPY-OPEN-DST ( n ^u8 -- n ) {: in dstz:^ :}
   dstz O-WRONLY O-CREAT or MODE-0644 open {: out :}
   out 0 < if in -1 E-FS-OPEN COPY-THROW then
   \ Opening must not truncate until the descriptor identities differ.
   in out [: COPY-PREPARE-DST ;] catch {: code :}
   2drop
   code 0<> if in out code COPY-THROW then
   out ;

\ The write cursor is the deepest argument, so a loop can leave it on the stack
\ and hand the call its known values on top.
: COPY-WRITE-ONE ( n SPAN:span<u8> n n n -- n )
   {: off buf in out u :}
   out buf off SPAN:SKIP  u off - SPAN:TAKE SPAN:$ write {: wr :}
   wr 0 <= if in out E-FS-IO COPY-THROW then
   wr u off - > if in out E-FS-IO COPY-THROW then
   off wr + ;

: COPY-WRITE-CHUNK ( n SPAN:span<u8> n n -- ) {: u buf in out :}
   0 begin dup u < while
      buf in out u COPY-WRITE-ONE
   repeat drop ;

: COPY-READ-ONE ( SPAN:span<u8> n n -- n ) {: buf in out :}
   in buf SPAN:$ read {: rd :}
   rd 0 < if in out E-FS-IO COPY-THROW then
   rd COPY-CAP > if in out E-FS-IO COPY-THROW then
   rd ;

: COPY-PUMP ( SPAN:span<u8> n n -- ) {: buf in out :}
   begin buf in out COPY-READ-ONE dup 0 > while
      buf in out COPY-WRITE-CHUNK
   repeat drop ;

\ The WITH-BYTES body: the scope's bytes are this copy's buffer, mapped before
\ either file opens, so a refused mapping leaves nothing to close.
: COPY-SCOPE ( str str ^u8 NUM:alloc-byte-len -- )
   MEM:ALLOCATION>SPAN {: src:^ srcu dst:^ dstu buf :}
   dst dstu COPY-DST-PATHZ {: dstz:^ :}
   src srcu COPY-OPEN-SRC {: in :}
   in dstz COPY-OPEN-DST {: out :}
   buf in out COPY-PUMP
   in CLOSE-COPY-FD
   out CLOSE-COPY-FD ;

public
\ Copies src's bytes to dst, a regular file made or truncated, through a buffer
\ of the call's own that a throw releases, so tasks may copy at once.
: COPY-FILE-STREAM ( str str -- )
   COPY-CAP MEM:BYTES-ALLOC-LEN [: COPY-SCOPE ;] MEM:WITH-BYTES ;

private
: U-DIGITS ( n -- n ) {: n :}
   n 0 < if E-FS-PATH throw then
   n 10 < if 1 exit then
   n 10 / RECURSE 1+ ;

: ATOMIC-U+ ( n SPAN:span<u8> n -- n ) {: value:n dst off:n :}
   value 0 < if E-FS-PATH throw then
   value 10 >= if value 10 / dst off RECURSE else off then {: next:n :}
   value 10 mod 48 + dst next SPAN:U8!
   next 1+ ;

: PATH-COLLISION? ( str -- bool ) {: a:^ u :}
   a u EXISTS? if 0 0= exit then
   a u SYMLINK? ;

\ The longest `.tmp-<seed>-<attempt>` name FS:RESERVE-SIBLING and
\ ATOMIC-WRITE-FILE give a sibling: the largest seed, the largest nonnegative
\ cell `-1 1 rshift`, and the last attempt, so whether a directory has room for
\ its sibling never depends on the seed it drew.
6 -1 1 rshift U-DIGITS +
ATOMIC-RETRIES 1- U-DIGITS + constant SIBLING-LEN-MAX

: SIBLING-Z ( -- ^u8 )
   ATOMIC-PATH SPAN:$ drop ;

: ATOMIC-BASE-OFF ( str -- n )
   {: path:^ pathu:n :}
   pathu begin dup 0 > while
      dup 1 - path + c@ SLASH = if exit then
      1-
   repeat ;

\ `<prefix>.tmp-<seed>-<attempt>` in this task's ATOMIC-PATH. The prefix
\ is a destination's directory with its slash, or empty for a name relative to
\ that directory's descriptor. A prefix that leaves no room within PATH-CAP
\ for the longest sibling name is E-FS-CAPACITY whatever the seed, a semantic
\ refusal.
: BUILD-SIBLING ( str n n -- str ) {: pre:^ preu seed attempt :}
   CAUSE-CLEAR
   preu 0 < if E-FS-PATH throw then
   preu SIBLING-LEN-MAX + PATH-CAP > if E-FS-CAPACITY throw then
   ATOMIC-PATH {: dst :}
   pre preu dst SPAN:COPY
   s" .tmp-" dst preu SPAN:SKIP SPAN:COPY
   seed dst preu 5 + ATOMIC-U+ {: off:n :}
   DASH dst off SPAN:U8!
   attempt dst off 1+ ATOMIC-U+ {: u:n :}
   dst u SPAN:TAKE SPAN:$ ;

\ Habu's portable `open` flags do not include O_EXCL. This narrow libc call
\ takes the host's exclusive-create flags, so creation stays one syscall.
PROCESS-SYMBOLS
FUNCTION: OPEN-CALL open ( ^u8 n n -- i32 ) 2 VARIADIC ;FUNCTION

\ O_WRONLY|O_CREAT|O_EXCL for the host.
: EXCL-WRITE-FLAGS ( -- n )
   HB-TARGET-LINUX-KERNEL? if $C1 exit then
   HB-TARGET-MACOS? if $A01 exit then
   E-FS-OPEN throw ;

\ O_RDWR|O_CREAT|O_EXCL for the host.
: EXCL-RDWR-FLAGS ( -- n )
   HB-TARGET-LINUX-KERNEL? if $C2 exit then
   HB-TARGET-MACOS? if $A02 exit then
   E-FS-OPEN throw ;

PROCESS-SYMBOLS
FUNCTION: CLOSE-CALL close ( n -- i32 ) ;FUNCTION

public
: CLOSE-FD ( n -- n )
   CLOSE-CALL ;

private
\ CREATE-NEW's open flags, O_WRONLY|O_CREAT|O_EXCL|O_CLOEXEC. Darwin: xnu
\ bsd/sys/fcntl.h. Linux: include/uapi/asm-generic/fcntl.h on every arch.
: CREATE-NEW-FLAGS ( -- n )
   HB-TARGET-MACOS? if $1 $200 or $800 or $1000000 or exit then
   $1 $40 or $80 or $80000 or ;

\ OPEN-RDWR-NOFOLLOW's open flags, O_RDWR|O_NOFOLLOW|O_CLOEXEC. Darwin: xnu
\ bsd/sys/fcntl.h. Linux arm64: arch/arm64 asm/fcntl.h overrides the generic
\ nofollow bit; x86-64 uses include/uapi/asm-generic/fcntl.h.
: RDWR-NOFOLLOW-FLAGS ( -- n )
   HB-TARGET-MACOS? if $2 $100 or $1000000 or exit then
   HB-TARGET-LINUX-X86-64? if $2 $20000 or $80000 or exit then
   $2 $8000 or $80000 or ;

public
\ A new file at the path, opened write-only and close-on-exec, with mode as
\ open(2) applies it, under the umask. Any entry already at the path, a link
\ included, dangling or not, is E-FS-OPEN with FS:ERRNO EEXIST; every other
\ failure is E-FS-OPEN with its errno in FS:ERRNO.
: CREATE-NEW ( str n -- fd ) {: path:^ pathu mode :}
   path pathu >PATHZ CREATE-NEW-FLAGS mode OPEN-CALL {: file:n :}
   file 0 < if E-FS-OPEN ERRNO-FAIL then
   file >FD ;

\ The existing file at the path, opened for reading and writing, close-on-exec.
\ A link as the last component is refused, never followed: E-FS-OPEN with
\ FS:ERRNO the host's answer, ELOOP on Linux. Every failure is E-FS-OPEN with
\ its errno in FS:ERRNO.
: OPEN-RDWR-NOFOLLOW ( str -- fd ) {: path:^ pathu :}
   path pathu >PATHZ RDWR-NOFOLLOW-FLAGS 0 OPEN-CALL {: file:n :}
   file 0 < if E-FS-OPEN ERRNO-FAIL then
   file >FD ;

private
\ Create and open one candidate with the exclusive flags and mode: its
\ descriptor, -1 when the name is taken, E-FS-OPEN with the host's cause for
\ any other refusal.
: OPEN-CANDIDATE ( str n n -- n )
   {: path:^ pathu flags mode :}
   path pathu >PATHZ flags mode OPEN-CALL {: fd :}
   fd 0 >= if fd exit then
   FFI:ERRNO EEXIST = if -1 exit then
   E-FS-OPEN ERRNO-FAIL ;

: ATOMIC-WRITE-FD ( n str -- ) {: fd src:^ srcu :}
   0 begin dup srcu < while
      {: off:n :}
      fd src off + srcu off - write {: wr:n :}
      wr 0 <= if E-FS-IO throw then
      wr srcu off - > if E-FS-IO throw then
      off wr +
   repeat drop ;

public
\ A throw sets FS:ERRNO.
: ATOMIC-CLEAN-TEMP ( str -- ) {: temp:^ tempu :}
   temp tempu PATH-COLLISION? if temp tempu REMOVE-FILE then ;

private
\ The realpath of the directory holding path's last component, borrowed from
\ this task's PATHZ2 buffer.
: PARENT-REAL ( str -- str )
   {: path:^ pathu:n :}
   path pathu ATOMIC-BASE-OFF {: off:n :}
   off 0= if s" ." >PATHZ else
      off 1 = if s" /" >PATHZ else
         path off 1 - >PATHZ
      then
   then {: parent:^ :}
   parent PATHZ2-BUF SPAN:$ drop PATHZ-CAP realpath {: u:n :}
   u -2 = if CAUSE-CLEAR E-FS-CAPACITY throw then
   u 0 < if E-FS-STAT ERRNO-FAIL then
   PATHZ2-BUF SPAN:$ drop u ;

public
\ Resolve the complete spelling when it exists, including its final symlink.
\ For a new basename, resolve its parent and append the basename unchanged.
\ The answer is absolute and borrowed from this task's PATHZ2 buffer until the
\ next use of that buffer; callers retaining it copy it.
\ This buffer survives sibling reservation and is used unchanged at rename.
: ATOMIC-TARGET ( str -- str )
   {: path:^ pathu:n :}
   path pathu >PATHZ {: input:^ :}
   input PATHZ2-BUF SPAN:$ drop PATHZ-CAP realpath {: u:n :}
   u -2 = if CAUSE-CLEAR E-FS-CAPACITY throw then
   u 0 >= if PATHZ2-BUF SPAN:$ drop u exit then
   input pathu SYMLINK? if E-FS-STAT throw then
   input pathu ATOMIC-BASE-OFF {: off:n :}
   pathu off - 0= if CAUSE-CLEAR E-FS-STAT throw then
   input off + pathu off - ATOMIC-PATH SPAN:COPY
   input pathu PARENT-REAL
   ATOMIC-PATH SPAN:$ drop pathu off -
   PATHZ2-BUF PATH-CAP SPAN:TAKE JOIN-PATH-INTO {: targetu:n :}
   PATHZ2-BUF SPAN:$ drop targetu ;

private
: ATOMIC-OLD-MODE ( str -- n ) {: path:^ pathu :}
   path pathu TRY-STAT if
      STAT-MODE@ dup S-IFMT and S-IFREG = if MODE-PERM and exit then
      drop
   then
   -1 ;

\ Two live processes never share a pid, and the clock tells a reused pid from a
\ sibling its last owner left behind.
: ATOMIC-SEED ( -- n )
   mono-ns getpid xor ;

PROCESS-SYMBOLS
FUNCTION: OPENAT-CALL openat ( n ^u8 n n -- i32 ) 3 VARIADIC ;FUNCTION
FUNCTION: FCHMOD-CALL fchmod ( n n -- i32 ) ;FUNCTION
FUNCTION: RENAMEAT-CALL renameat ( n ^u8 n ^u8 -- i32 ) ;FUNCTION
FUNCTION: FSYNC-CALL fsync ( n -- i32 ) ;FUNCTION
FUNCTION: FCNTL-CALL fcntl ( n n n -- i32 ) 2 VARIADIC ;FUNCTION

\ Darwin's fcntl that flushes the drive's own cache, which its fsync leaves to
\ a power loss: xnu bsd/sys/fcntl.h.
51 constant F-FULLFSYNC

\ Darwin's fcntl that orders writes without a drive flush: xnu bsd/sys/fcntl.h.
85 constant F-BARRIERFSYNC

\ Host errnos: macOS sys/errno.h, Linux asm-generic/errno.h.
25 constant ENOTTY
: ENOTSUP ( -- n )
   HB-TARGET-MACOS? if 45 else 95 then ;

\ A line `<CLASS> habu: <place>: <text>` on fd 2 is written in pieces: a place
\ of PATH-CAP bytes with its text overflows the 1 KiB string builder.
: SAY ( str -- ) {: a:^ u :}
   2 a u write drop ;

\ The code's declared name, else its signed number.
: SAY-CLASS ( n -- ) {: code :}
   code ERROR:NAME$ {: name:^ nameu :}
   nameu 0= if code FMT:INT$ else name nameu then SAY
   s"  habu: " SAY ;

: SAY-TEXT ( str -- )
   s" : " SAY SAY ;

\ `, errno N` when e is a host errno, then the line's end.
: SAY-END ( n -- ) {: e :}
   e 0<> if s" , errno " SAY e FMT:INT$ SAY then
   S\" \n" SAY ;

\ Reporting is best effort after a primary failure: a closed stderr must not
\ replace the fill or rename error. The sibling is named as a directory prefix
\ and a name. The errno is the failed step's FS:ERRNO, which the caller clears
\ before the step; it is read before the first write, which replaces it.
: STAGED-REPORT ( str str n str str -- )
   {: op:^ opu step:^ stepu code path:^ pathu name:^ nameu :}
   ERRNO {: e:n :}
   code SAY-CLASS
   path pathu SAY  name nameu SAY
   s" : " SAY  op opu SAY  s"  " SAY  step stepu SAY
   s"  failed" SAY
   e SAY-END ;

\ Opened for search only: a directory the caller may write and search but not
\ list still opens.
: OPEN-DIR-SEARCH ( str -- n ) {: a:^ u :}
   AT-CWD a u >PATHZ SEARCH-DIR-FLAGS OPEN-AT-CALL {: dir :}
   dir 0 < if E-FS-OPEN ERRNO-FAIL then
   dir ;

\ The absolute target's directory, for every call naming the sibling or the
\ target: what precedes its last slash, or the root.
: ATOMIC-OPEN-DIR ( str -- n ) {: dst:^ dstu :}
   dst dstu ATOMIC-BASE-OFF 1- 1 max {: diru :}
   dst diru OPEN-DIR-SEARCH ;

\ Create the sibling in dir exclusively, its name staged for SIBLING-Z:
\ the name's length and the descriptor.
: ATOMIC-RESERVE-AT ( n n n -- n n )
   {: dir seed mode :}
   0 begin dup ATOMIC-RETRIES < while
      dup {: attempt:n :}
      s" " seed attempt BUILD-SIBLING {: name:^ nameu :}
      nameu ATOMIC-PATH FFI:CSTR! drop
      dir name EXCL-WRITE-FLAGS mode OPENAT-CALL {: fd:n :}
      fd 0 >= if drop nameu fd exit then
      FFI:ERRNO EEXIST <> if E-FS-OPEN ERRNO-FAIL then
      1+
   repeat drop
   E-FS-IO ERRNO-FAIL ;

\ Darwin's fcntl cmd, or fsync on a file system that refuses it and elsewhere.
: FCNTL-SYNC ( n n -- n ) {: fd cmd :}
   HB-TARGET-MACOS? if
      fd cmd 0 FCNTL-CALL 0= if fd exit then
      FFI:ERRNO dup EINVAL = over ENOTTY = or swap ENOTSUP = or
      0= if E-FS-IO ERRNO-FAIL then
   then
   fd FSYNC-CALL 0<> if E-FS-IO ERRNO-FAIL then
   fd ;

: ATOMIC-SYNC ( n -- n ) F-FULLFSYNC FCNTL-SYNC ;

\ Orders the data before the rename; Darwin's barrier spares the drive flush.
: ORDER-SYNC ( n -- n ) F-BARRIERFSYNC FCNTL-SYNC ;

\ Stack-preserving under `catch`.
: ATOMIC-FILL ( n str n -- n str n ) {: fd src:^ srcu mode :}
   fd src srcu ATOMIC-WRITE-FD
   mode 0 >= if
      fd mode FCHMOD-CALL 0<> if E-FS-IO ERRNO-FAIL then
   then
   fd ATOMIC-SYNC drop
   fd src srcu mode ;

\ Runs after the publish, hence E-FS-SYNC. Linux won't fsync a search-only dir
\ (EBADF), so it is reopened to read; one the caller cannot list stays unsynced.
: ATOMIC-SYNC-DIR ( n -- n ) {: dir :}
   dir [: ATOMIC-SYNC ;] catch {: first:n :}
   drop
   first 0= if dir exit then
   ERRNO EBADF <> if E-FS-SYNC throw then
   dir S\" .\z" drop OPEN-DIR-FLAGS OPEN-AT-CALL {: rd:n :}
   rd 0 < if
      FFI:ERRNO EACCES = if dir exit then
      E-FS-SYNC ERRNO-FAIL
   then
   rd [: ATOMIC-SYNC ;] catch {: code:n :}
   drop
   rd CLOSE-FD drop
   code 0<> if E-FS-SYNC throw then
   dir ;

PROCESS-SYMBOLS
FUNCTION: SYNCFS-CALL syncfs ( n -- i32 ) ;FUNCTION

: SYNC-PATH ( str [ n -- n ] -- ) {: sync :}
   >PATHZ open-rd {: fd :}
   fd 0 < if E-FS-OPEN throw then
   fd sync catch {: code:n :}
   drop
   fd CLOSE-FD drop
   code 0<> if code throw then ;

: SYNC-ENTRY ( str -- )
   [: dup FSYNC-CALL 0<> if E-FS-IO ERRNO-FAIL then ;] SYNC-PATH ;

: SYNC-WALKED? ( str -- bool ) {: a:^ u :}
   a u DIR? if a u SYNC-ENTRY then
   true ;

\ The F_FULLFSYNC last flushes the drive's cache once for every fsync before.
: SYNC-EACH ( str -- ) {: a:^ u :}
   a u [: SYNC-WALKED? ;] [: SYNC-ENTRY ;] WALK-FILES-IF
   a u PARENT-REAL SYNC-ENTRY
   a u [: ATOMIC-SYNC ;] SYNC-PATH ;

: SYNC-FS ( str -- )
   [: dup SYNCFS-CALL 0<> if E-FS-IO ERRNO-FAIL then ;] SYNC-PATH ;

public
\ On return directory path, every directory and regular file under it and its
\ name in its parent are on disk. A throw sets FS:ERRNO.
: SYNC-TREE ( str -- )
   HB-TARGET-MACOS? if SYNC-EACH else SYNC-FS then ;

\ Renames from over to and syncs to's directory, opened first: a throw before
\ the rename leaves both names; E-FS-SYNC is the sync's, after the rename.
: RENAME-SYNCED ( str str -- ) {: from:^ fromu to:^ tou :}
   to tou PARENT-REAL OPEN-DIR-SEARCH {: dir :}
   [: from fromu to tou RENAME-FILE dir ATOMIC-SYNC-DIR drop ;]
   catch {: code:n :}
   dir CLOSE-FD drop
   code 0<> if code throw then ;

private

\ Closed exactly once, before publish: a failed close may already have freed
\ the descriptor, so retrying it could close an unrelated recycled fd.
: ATOMIC-CLOSE ( n -- n ) {: fd :}
   fd CLOSE-FD 0<> if E-FS-IO ERRNO-FAIL then
   fd ;

: ATOMIC-PUBLISH ( n ^u8 -- n ^u8 ) {: dir name:^ :}
   dir SIBLING-Z dir name RENAMEAT-CALL 0<> if E-FS-IO ERRNO-FAIL then
   dir name ;

: ATOMIC-UNLINK ( n -- n ) {: dir :}
   dir SIBLING-Z 0 UNLINK-AT-CALL 0<> if E-FS-IO ERRNO-FAIL then
   dir ;

\ Unlinks sib, a NUL-terminated name in dir; a failure is reported under the
\ prefix pre, never thrown, so the error that called the cleanup stands.
: ATOMIC-DISCARD ( n str str -- ) {: dir pre:^ preu sib:^ sibu :}
   CAUSE-CLEAR
   [: dir sib 0 UNLINK-AT-CALL 0<> if E-FS-IO ERRNO-FAIL then ;]
   catch {: clean-code:n :}
   clean-code 0<> if
      s" atomic replacement" s" cleanup" clean-code
      pre preu sib sibu STAGED-REPORT
   then ;

: ATOMIC-ABANDON ( n str n n -- ) {: dir dst:^ off nameu code :}
   ERRNO {: cause :}
   dir dst off SIBLING-Z nameu ATOMIC-DISCARD
   code cause CAUSE-FAIL ;

\ Stack-preserving under `catch`; a throw after the publish leaves the new file.
\ Answers whether it published; a claim on a taken name unlinks the sibling.
: ATOMIC-IN-DIR ( n str str n n bool -- n str str n n bool )
   {: dir dst:^ off src:^ srcu mode seed claim?:bool :}
   mode 0 >= if MODE-PRIVATE-FILE else MODE-0644 then {: create-mode :}
   dir seed create-mode ATOMIC-RESERVE-AT {: nameu fd :}
   fd src srcu mode [: ATOMIC-FILL ;] catch {: write-code:n :}
   2drop 2drop
   write-code 0<> if
      fd CLOSE-FD drop
      dir dst off nameu write-code ATOMIC-ABANDON
   then
   fd [: ATOMIC-CLOSE ;] catch {: close-code:n :}
   drop
   close-code 0<> if dir dst off nameu close-code ATOMIC-ABANDON then
   dir dst off + claim?
   [: if 2dup SIBLING-Z swap RENAME-NOREPLACE else ATOMIC-PUBLISH true then ;]
   catch {: publish-code:n :}
   publish-code 0<> if dir dst off nameu publish-code ATOMIC-ABANDON then
   {: published:bool :}
   2drop
   published if dir ATOMIC-SYNC-DIR drop else dir ATOMIC-UNLINK drop then
   dir dst off src srcu mode seed published ;

\ PATH-CAP bounds only dst: the sibling is named in dst's directory. Private is
\ 0600, else the old regular mode or umasked 0644; false if claimed and taken.
: ATOMIC-WRITE-SEED ( str str n bool bool -- bool )
   {: path:^ pathu src:^ srcu seed private?:bool claim?:bool :}
   srcu 0 < if CAUSE-CLEAR E-FS-CAPACITY throw then
   path pathu ATOMIC-TARGET {: dst:^ dstu :}
   dstu PATHZ2-BUF FFI:CSTR! drop
   claim? if dst dstu TRY-LSTAT if false exit then then
   private? if MODE-PRIVATE-FILE else dst dstu ATOMIC-OLD-MODE then {: mode:n :}
   dst dstu ATOMIC-OPEN-DIR {: dir :}
   dir dst dst dstu ATOMIC-BASE-OFF src srcu mode seed claim?
   [: ATOMIC-IN-DIR ;] catch {: code:n :}
   ERRNO {: cause :}
   dir CLOSE-FD drop
   code 0<> if code cause CAUSE-FAIL then
   {: published:bool :}
   2drop 2drop 2drop drop
   published ;

: RESERVE-MODE ( str n -- str n ) {: path:^ pathu mode :}
   pathu 0 < if CAUSE-CLEAR E-FS-PATH throw then
   path path pathu ATOMIC-BASE-OFF {: pre:^ preu :}
   ATOMIC-SEED {: seed :}
   0 begin dup ATOMIC-RETRIES < while
      dup {: attempt:n :}
      pre preu seed attempt BUILD-SIBLING
      2dup EXCL-WRITE-FLAGS mode OPEN-CANDIDATE {: fd:n :}
      fd 0 >= if rot drop fd exit then
      2drop 1+
   repeat drop
   E-FS-IO ERRNO-FAIL ;

public
\ Opens a new `.tmp-*` sibling of path, the caller's to remove; this task's next
\ sibling, ATOMIC-WRITE-* or ATOMIC-TARGET ends its path. A throw sets FS:ERRNO.
: RESERVE-SIBLING ( str -- str n )
   MODE-0644 RESERVE-MODE ;

\ Replace path's file with src's bytes; a reader or crash sees old or whole new,
\ durable as ATOMIC-SYNC-DIR says. A throw sets FS:ERRNO; one from the
\ directory sync after the rename is E-FS-SYNC, the new file in place.
: ATOMIC-WRITE-FILE ( str str -- )
   {: path:^ pathu src:^ srcu :}
   path pathu src srcu ATOMIC-SEED false false ATOMIC-WRITE-SEED drop ;


\ ATOMIC-WRITE-FILE for a file only its owner may read: sibling and result are
\ 0600 whatever the destination held. A throw sets FS:ERRNO.
: ATOMIC-WRITE-PRIVATE ( str str -- )
   {: path:^ pathu src:^ srcu :}
   path pathu src srcu ATOMIC-SEED true false ATOMIC-WRITE-SEED drop ;


\ ATOMIC-WRITE-PRIVATE to a free name, else false, the entry untouched. A final
\ symlink counts as its target, a dangling one E-FS-STAT. A throw sets FS:ERRNO.
: ATOMIC-CREATE-PRIVATE ( str str -- bool )
   {: path:^ pathu src:^ srcu :}
   path pathu src srcu ATOMIC-SEED true true ATOMIC-WRITE-SEED ;

private
\ FS:ATOMIC-CREATE-FILES is a machine over groups of at most ATOMIC-GROUP
\ files. Each group goes FILL (each free name's sibling created, written and
\ hinted), SYNC (each sibling's data ordered before its rename, then closed
\ once) and PUBLISH (each sibling renamed to its name, or unlinked if the
\ name was taken since); after the last group, DIR-SYNC once. A throw in
\ FILL, SYNC or PUBLISH goes to ABANDON, which closes the group's open
\ descriptors, unlinks its unpublished siblings and rethrows. Linux fsyncs
\ each file; Darwin barriers each, and DIR-SYNC's F_FULLFSYNC flushes the
\ drive once for them all.

\ Files in flight at once: a descriptor stays open from its write to its
\ fsync, since a write error is reported to the descriptor open when it
\ happened, and descriptors are bounded and shared with a server's sockets.
64 constant ATOMIC-GROUP

\ NAME_MAX, the longest name component, in bytes, on Linux and Darwin.
255 constant NAME-MAX

\ sync_file_range(2) and its SYNC_FILE_RANGE_WRITE, Linux's: starts the
\ writeback of a file's dirty pages without waiting for it.
PROCESS-SYMBOLS
FUNCTION: SYNC-RANGE-CALL sync_file_range ( n n n n -- i32 ) ;FUNCTION
2 constant SFR-WRITE

\ Starts each file's writeback as FILL writes it, so the group's blocks are
\ allocated in one journal transaction and each SYNC fsync finds its data
\ under way, as PostgreSQL's pre_sync_fname does. A hint: its result is
\ unread. Darwin has no such call.
: HINT-WRITE ( n -- ) {: fd :}
   HB-TARGET-LINUX-KERNEL? if
      fd 0 0 SFR-WRITE SYNC-RANGE-CALL drop
   then ;

\ A group's slot: the sibling's descriptor, -1 when none is open; the
\ sibling's name length, 0 when no sibling is left to publish or unlink;
\ then the sibling's and the target's names, each NUL-terminated.
SIBLING-LEN-MAX 1+ constant SIB-ROOM
2 cells SIB-ROOM + NAME-MAX 1+ + CELL 1- + CELL / cells constant SLOT-ROOM

\ The scope's bytes: the report prefix `<dir>/` in PREFIX-ROOM, then slots.
: PREFIX-ROOM ( n -- n ) CELL + CELL / cells ;

: GROUP-SLOT ( str -- ^n ) SLOT-ROOM * + CELL-VIEW ;
: SLOT-SIB-LEN ( ^n -- ^n ) CELL + ;
: SLOT-SIB ( ^n -- ^u8 ) BYTE-VIEW 2 cells + ;
: SLOT-NAME ( ^n -- ^u8 ) SLOT-SIB SIB-ROOM + ;

: SLOT-EMPTY ( ^n -- ) {: slot:^ :}
   -1 slot !
   0 slot SLOT-SIB-LEN ! ;

\ FILL for file idx: a taken name is skipped.
: SLOT-FILL ( R n ^n n [ R n -- R str str ] -- R )
   {: dir slot:^ idx body :}
   idx body execute {: name:^ nameu src:^ srcu :}
   nameu 1 < if CAUSE-CLEAR E-FS-PATH throw then
   nameu NAME-MAX > if CAUSE-CLEAR E-FS-CAPACITY throw then
   srcu 0 < if CAUSE-CLEAR E-FS-CAPACITY throw then
   dir name nameu LSTAT-AT? if exit then
   name slot SLOT-NAME nameu BYTE-COPY
   0 slot SLOT-NAME nameu + c!
   dir ATOMIC-SEED MODE-0644 ATOMIC-RESERVE-AT {: sibu fd :}
   fd slot !
   SIBLING-Z slot SLOT-SIB sibu 1+ BYTE-COPY
   sibu slot SLOT-SIB-LEN !
   fd src srcu ATOMIC-WRITE-FD
   fd HINT-WRITE ;

: CREATE-FILL ( R n ^u8 n n [ R n -- R str str ] -- R )
   {: dir slots:^ first size body :}
   size 0 ?do dir slots i GROUP-SLOT first i + body SLOT-FILL loop ;

\ SYNC: the slot is marked closed before the close, which runs once.
: SLOT-SYNC ( ^n -- ) {: slot:^ :}
   slot @ {: fd :}
   fd 0 < if exit then
   fd ORDER-SYNC drop
   -1 slot !
   fd ATOMIC-CLOSE drop ;

: SLOT-PUBLISH ( n ^n -- ) {: dir slot:^ :}
   slot SLOT-SIB-LEN @ 0= if exit then
   dir slot SLOT-SIB slot SLOT-NAME RENAME-NOREPLACE 0= if
      dir slot SLOT-SIB 0 UNLINK-AT-CALL 0<> if E-FS-IO ERRNO-FAIL then
   then
   0 slot SLOT-SIB-LEN ! ;

\ Stack-preserving under `catch`.
: CREATE-GROUP ( R n ^u8 n n [ R n -- R str str ] -- R n str n [ R n -- R str str ] )
   {: dir slots:^ first size body :}
   dir slots first size body CREATE-FILL
   size 0 ?do slots i GROUP-SLOT SLOT-SYNC loop
   size 0 ?do dir slots i GROUP-SLOT SLOT-PUBLISH loop
   dir slots first size body ;

: SLOT-ABANDON ( n str ^n -- ) {: dir pre:^ preu slot:^ :}
   slot @ {: fd :}
   fd 0 >= if fd CLOSE-FD drop then
   slot SLOT-SIB-LEN @ {: sibu :}
   sibu 0 > if dir pre preu slot SLOT-SIB sibu ATOMIC-DISCARD then ;

\ ABANDON: rethrows code with the cause it found, after the group's cleanup.
: CREATE-ABANDON ( n str str n -- )
   {: dir pre:^ preu slots:^ size code :}
   ERRNO {: cause :}
   size 0 ?do dir pre preu slots i GROUP-SLOT SLOT-ABANDON loop
   code cause CAUSE-FAIL ;

: CREATE-SCOPE ( R n str n [ R n -- R str str ] ^u8 NUM:alloc-byte-len -- R )
   MEM:ALLOCATION>SPAN {: dir path:^ pathu count body buf :}
   path pathu s" /" buf SUFFIX-PATH {: preu :}
   buf SPAN:$ drop {: pre:^ :}
   pre pathu PREFIX-ROOM + {: slots:^ :}
   count 0 ?do
      i  count i - ATOMIC-GROUP min {: first size :}
      size 0 ?do slots i GROUP-SLOT SLOT-EMPTY loop
      dir slots first size body [: CREATE-GROUP ;] catch {: code:n :}
      2drop 2drop drop
      code 0<> if dir pre preu slots size code CREATE-ABANDON then
   ATOMIC-GROUP +loop
   dir ATOMIC-SYNC-DIR drop ;

\ Stack-preserving under `catch`.
: CREATE-RUN ( R n str n [ R n -- R str str ] -- R n str n [ R n -- R str str ] )
   {: dir path:^ pathu count body :}
   dir path pathu count body
   pathu PREFIX-ROOM  count ATOMIC-GROUP min SLOT-ROOM * +
   MEM:BYTES-ALLOC-LEN [: CREATE-SCOPE ;] MEM:WITH-BYTES
   dir path pathu count body ;

public
\ Creates each free name in directory path with the bytes body answers for
\ file i, 0 <= i < count, umasked 0644; a taken name is left as it is. The
\ body's answers need last only until its next call. Each name is one
\ component: an empty one is E-FS-PATH, one over NAME_MAX bytes
\ E-FS-CAPACITY. Count 0 does nothing; a negative count is E-FS-CAPACITY.
\ On return every name is on disk as ATOMIC-SYNC-DIR says. A throw unlinks
\ its group's unpublished siblings, reporting on fd 2 one it cannot, and
\ leaves earlier groups' names whole: one in FILL or SYNC publishes none of
\ its group's names, one in PUBLISH keeps those it renamed; E-FS-SYNC leaves
\ every name in place. A throw sets FS:ERRNO.
: ATOMIC-CREATE-FILES ( R str n [ R n -- R str str ] -- R )
   {: path:^ pathu count body :}
   count 0 < if CAUSE-CLEAR E-FS-CAPACITY throw then
   count 0= if exit then
   path pathu OPEN-DIR-SEARCH {: dir :}
   dir path pathu count body [: CREATE-RUN ;] catch {: code:n :}
   ERRNO {: cause :}
   2drop 2drop drop
   dir CLOSE-FD drop
   code 0<> if code cause CAUSE-FAIL then ;


\ The longest base FS:MKTEMP-DIR-IN makes its temp in for a prefix of n bytes:
\ its `<base>/<prefix>-<seed>-<attempt>` path still fits PATH-CAP with the
\ largest seed, the largest nonnegative cell, and the last attempt, so whether
\ a base has room for one never depends on the seed it drew.
: TEMP-DIR-BASE-MAX ( n -- n ) {: prefixu:n :}
   PATH-CAP 3 - prefixu -
   -1 1 rshift U-DIGITS -
   TMP-RETRIES 1- U-DIGITS - ;

private
: BUILD-TEMP-TRY ( str str n n -- str ) {: base:^ baseu prefix:^ prefixu seed attempt :}
   baseu prefixu TEMP-DIR-BASE-MAX > if E-FS-CAPACITY throw then
   TMP-PATH-BUF {: dst :}
   base baseu dst SPAN:COPY
   base baseu JOIN-SEPARATED? if baseu else
      SLASH dst baseu SPAN:U8! baseu 1+
   then {: off:n :}
   prefix prefixu dst off SPAN:SKIP SPAN:COPY
   DASH dst off prefixu + SPAN:U8!
   seed dst off prefixu + 1+ ATOMIC-U+ {: next:n :}
   DASH dst next SPAN:U8!
   attempt dst next 1+ ATOMIC-U+ {: u:n :}
   dst u SPAN:TAKE SPAN:$ ;

: TMP-COLLISION? ( str -- bool ) {: a:^ u :}
   a u PATH-COLLISION? ;

: MKDIR-CANDIDATE? ( str -- bool ) {: a:^ u :}
   a u >PATHZ MODE-PRIVATE-DIR mkdir {: rc :}
   rc 0= if 0 0= exit then
   a u TMP-COLLISION? if 0 0= 0= exit then
   E-FS-IO throw ;

: MKTEMP-DIR-SEED ( str str n -- str ) {: base:^ baseu prefix:^ prefixu seed :}
   0 begin dup TMP-RETRIES < while
      dup {: attempt:n :}
      base baseu prefix prefixu seed attempt BUILD-TEMP-TRY
      2dup MKDIR-CANDIDATE? if rot drop exit then
      2drop 1+
   repeat drop
   E-FS-IO throw ;

: MKTEMP-SEED ( str str n -- str n )
   {: base:^ baseu prefix:^ prefixu seed :}
   0 begin dup TMP-RETRIES < while
      dup {: attempt:n :}
      base baseu prefix prefixu seed attempt BUILD-TEMP-TRY
      2dup EXCL-RDWR-FLAGS MODE-PRIVATE-FILE OPEN-CANDIDATE {: fd:n :}
      fd 0 >= if rot drop fd exit then
      2drop 1+
   repeat drop
   E-FS-IO throw ;

public
\ FS:MKTEMP-DIR-IN makes the directory under the base as given, and the answer
\ keeps the caller's spelling: a relative base gives a relative answer. The
\ answer stays this task's until its next FS:MKTEMP, FS:MKTEMP-DIR or
\ FS:MKTEMP-DIR-IN: a caller that makes two copies the first.
: MKTEMP-DIR-IN ( str str -- str )
   mono-ns MKTEMP-DIR-SEED ;

private
\ realpath of a temp root into this task's PATHZ2 buffer: the canonical
\ spelling's length, below 0 when realpath refuses the root. FS:MKTEMP-DIR-IN
\ and FS:MKTEMP build and make their candidates through TMP-PATH-BUF and
\ >PATHZ only, so the spelling stands through every attempt.
: TEMP-REAL ( str -- n )
   >PATHZ PATHZ2-BUF SPAN:$ drop PATHZ-CAP realpath ;

\ One record naming TMPDIR, its value and why, before anything is made:
\ without it each rejection dies later on an error that does not name TMPDIR.
\ The value comes from the environment, so the record has no place.
: TMPDIR-OPEN ( str str str -- )
   {: a:^ u c:^ cu why:^ whyu :}
   c cu REJECTION:OPEN  why whyu REJECTION:EMIT-STR
   s" TMPDIR" REJECTION:NOTE  a u REJECTION:EMIT-STR ;

\ Throws the code carrying the record, which prints only if nothing catches it:
\ ZIP:OPEN's callers catch FS:MKTEMP's E-FS-DIR and go on.
: TMPDIR-REJECT ( str n -- ) {: h:^ hu code :}
   REJECTION:HELP  h hu REJECTION:EMIT-STR
   REJECTION:SAVE  code REJECTION:PASS ;

: TMPDIR-LONG ( str -- )
   s" E-FS-CAPACITY" s" TMPDIR is too long for a temp path" TMPDIR-OPEN
   s" point TMPDIR at a directory with a shorter path, or unset it to use /tmp"
   E-FS-CAPACITY TMPDIR-REJECT ;

\ DIR? recording no read: a temp made in the root moves its change time, which
\ no compile's answer depends on.
: TMPDIR-DIR? ( str -- bool )
   >PATHZ STAT-Z? 0= if false exit then
   STAT-MODE@ S-IFMT and S-IFDIR = ;

\ The answer is the canonical spelling, borrowed from this task's PATHZ2 buffer.
: TMPDIR-CHECK ( str n -- str ) {: a:^ u prefixu :}
   u PATH-CAP > if a u TMPDIR-LONG then
   a u TMPDIR-DIR? 0= if
      a u s" E-FS-DIR" s" TMPDIR is not a directory" TMPDIR-OPEN
      s" point TMPDIR at an existing directory, or unset it to use /tmp"
      E-FS-DIR TMPDIR-REJECT
   then
   a u TEMP-REAL FFI:ERRNO {: real:n e:n :}
   \ -2: the canonical spelling and its NUL do not fit PATHZ-CAP.
   real -2 = if a u TMPDIR-LONG then
   real 0 < if
      a u s" E-FS-IO" s" TMPDIR has no canonical path" TMPDIR-OPEN
      s" errno" REJECTION:NOTE  e REJECTION:EMIT-U
      s" make every directory on TMPDIR's path searchable, or unset it to use /tmp"
      E-FS-IO TMPDIR-REJECT
   then
   real prefixu TEMP-DIR-BASE-MAX > if a u TMPDIR-LONG then
   PATHZ2-BUF SPAN:$ drop real ;

\ The root is answered in its canonical spelling, so every temp FS:MKTEMP-DIR
\ or FS:MKTEMP makes has a canonical path: a root reached through a symlink
\ (macOS's /tmp is /private/tmp) would otherwise give paths that differ from
\ the canonical ones the engine records for the same files. A caller that hands
\ its child a TMPDIR of its own owns and removes it, so a temp made there goes
\ away with it even when the child is killed before its own CLEANUP-RUN;
\ anywhere else nobody reaps the root, and a maker that leaks there leaks for
\ good.
: TEMP-ROOT ( n -- str )
   {: prefixu:n :}
   s" TMPDIR" GETENV dup 0= if 2drop s" /tmp" then
   prefixu TMPDIR-CHECK ;

public
: MKTEMP-DIR ( str -- str )
   {: prefix:^ prefixu :}
   prefixu TEMP-ROOT prefix prefixu MKTEMP-DIR-IN ;

\ Create and open a new file `<root>/<prefix>-<seed>-<attempt>` under the temp
\ root, read-write and mode 0600, as mkstemp(3) does. Answers its path, which
\ stays this task's until its next FS:MKTEMP, FS:MKTEMP-DIR or
\ FS:MKTEMP-DIR-IN, and the descriptor. The caller owns both and the removal;
\ nothing is registered.
: MKTEMP ( str -- str fd )
   {: prefix:^ prefixu :}
   prefixu TEMP-ROOT prefix prefixu mono-ns MKTEMP-SEED >FD ;

\ One task only: a forked child's inherited paths are its parent's to remove,
\ and a registration another task was in at the fork leaves the lock held.
: CLEANUP-RESET ( -- )
   0 CLEANUP-N !
   CLEANUP-RELEASE ;

private
: CLEANUP-REMOVE ( n -- ) {: idx :}
   idx CLEANUP-PATH {: a:^ u :}
   idx CLEANUP-KIND @ {: kind :}
   a u SYMLINK? if a u REMOVE-FILE exit then
   a u EXISTS? 0= if exit then
   kind CLEANUP-TREE = if
      a u REMOVE-TREE
   else
   kind CLEANUP-DIR = if
      a u REMOVE-DIR
   else
      a u REMOVE-FILE
   then then ;

\ Under the lock. Newest first, so a parent registered before its child goes
\ after it.
: CLEANUP-REMOVE-ALL ( -- )
   CLEANUP-N @ begin dup 0 > while
      1 - dup CLEANUP-REMOVE
   repeat drop
   0 CLEANUP-N ! ;

public
\ THE RUN HOLDS THE LOCK FROM ITS FIRST SLOT TO ITS LAST. A slot's path is read
\ in place, and REMOVE-TREE reads its root's name again once the tree below is
\ gone, so a registration that grew the row (releasing the old one) or a
\ forget that shifted the slots while the run stood would hand it freed or
\ moved bytes. The exit hook runs this on whichever task exits, and that task
\ may already hold the lock: a `die` inside a registration, a forget or a run
\ of its own. It then runs under the hold it has, which no other task can
\ move: every counted slot is whole, since a registration counts its slot only
\ once it is written and a growth publishes the new row before releasing the
\ old.
: CLEANUP-RUN ( -- )
   CLEANUP-LOCK atomic@ CLEANUP-SELF = if
      CLEANUP-REMOVE-ALL exit
   then
   CLEANUP-ACQUIRE
   [: CLEANUP-REMOVE-ALL ;] [: CLEANUP-RELEASE ;] finally ;


private
\ CLEANUP-AT-EXIT is the registry's process exit action.
\ Registering adds it, not loading this file: a stripped application restores
\ no engine cell, so its exit vector is zero until its own CLEANUP+ arms it.
\
\ A removal that fails is reported, never swallowed: the code goes to fd 2 as
\ one line and the exit code the process carries is kept (the engine preserves
\ it across the call). The string builder and fmt live in engine bands below a
\ stripped image's DATA window, which a stripped image maps fresh and zeroed.
\ Nothing that ran before this line reads the builder again, so resetting it
\ is safe.
: EXIT-REPORT ( n -- ) {: code :}
   SB-RESET
   s" hb: cleanup at exit threw " SB-APPEND
   code FMT:SB-INT
   S\" \n" SB-APPEND
   2 SB$ write drop ;

: CLEANUP-AT-EXIT ( -- )
   [: CLEANUP-RUN ;] catch {: code :}
   code 0<> if code EXIT-REPORT then ;

\ REGISTRATION IS SAFE FROM ANY TASK although the table is the process's: it
\ runs under CLEANUP-LOCK, which a refusal releases too. The row grows
\ when it is full, the slot is written, and only then is it counted, so every
\ counted slot names a path and a refused registration - a path past PATH-CAP
\ (E-FS-PATH) or a row that cannot grow - leaves the count it found.
: CLEANUP-APPEND ( str n -- ) {: a:^ u kind :}
   CLEANUP-N @ {: idx :}
   u PATH-CAP > if E-FS-PATH throw then
   idx CLEANUP-CAP @ = if CLEANUP-GROW then
   a u idx CLEANUP-SLOT SPAN:COPY
   u idx CLEANUP-AT !
   kind idx CLEANUP-KIND !
   idx 1+ CLEANUP-N ! ;

: CLEANUP-KIND+ ( str n -- ) {: a:^ u kind :}
   ['] CLEANUP-AT-EXIT AT-EXIT:ADD
   u 0 < if E-FS-PATH throw then
   kind CLEANUP-FILE <>
   kind CLEANUP-DIR <> and
   kind CLEANUP-TREE <> and if E-FS-IO throw then
   CLEANUP-ACQUIRE
   a u kind [: CLEANUP-APPEND ;] [: CLEANUP-RELEASE ;] finally ;

public
: CLEANUP+ ( str -- )
   CLEANUP-FILE CLEANUP-KIND+ ;

: CLEANUP-DIR+ ( str -- )
   CLEANUP-DIR CLEANUP-KIND+ ;

: CLEANUP-TREE+ ( str -- )
   CLEANUP-TREE CLEANUP-KIND+ ;

private
: CLEANUP-NAMES? ( str n -- bool ) {: a:^ u idx :}
   idx CLEANUP-AT @ u <> if 0 0= 0= exit then
   idx CLEANUP-PATH a u STR= ;

: CLEANUP-FIND ( str -- n ) {: a:^ u :}
   CLEANUP-N @ begin dup 0 > while
      1 -
      dup >r a u r> CLEANUP-NAMES? if exit then
   repeat
   drop -1 ;

\ The slots above idx move down one in a single forward copy, which is safe
\ because the destination lies below the source.
: CLEANUP-DROP ( str -- ) {: a:^ u :}
   a u CLEANUP-FIND {: idx :}
   idx 0 < if exit then
   CLEANUP-N @ 1- {: last :}
   idx last < if
      idx 1+ CLEANUP-AT BYTE-VIEW idx CLEANUP-AT BYTE-VIEW
      last idx - CLEANUP-STRIDE * cells BYTE-COPY
   then
   last CLEANUP-N ! ;

public
\ Forgets a path its owner removed, so the table holds only what is still owed.
\ The newest slot naming the path goes and every newer slot moves down one, so
\ the run order keeps no gap. A path no slot names is a no-op; an empty path is
\ E-FS-PATH.
: CLEANUP-FORGET ( str -- ) {: a:^ u :}
   u 0 <= if E-FS-PATH throw then
   CLEANUP-ACQUIRE
   a u [: CLEANUP-DROP ;] [: CLEANUP-RELEASE ;] finally ;

private
\ The sibling is umasked 0644, or 0600 over a regular file; once open for the
\ sync it takes that file's mode plus fill's execute bits, which may deny reads.
: STAGED-SYNC ( str n bool -- )
   {: tmp:^ tmpu mode:n durable:bool :}
   mode 0 >= if
      tmp tmpu TRY-STAT 0= if E-FS-IO throw then
      STAT-MODE@ MODE-EXEC and mode or
   else -1 then {: kept:n :}
   tmp tmpu [:
      kept 0 >= if
         dup kept FCHMOD-CALL 0<> if E-FS-IO ERRNO-FAIL then
      then
      durable if ATOMIC-SYNC else ORDER-SYNC then ;] SYNC-PATH ;

\ Stack-preserving under `catch`, the sibling left to remove.
: STAGED-RUN ( R str str n bool [ R str -- R ] -- R str str n bool [ R str -- R ] )
   {: path:^ pathu tmp:^ tmpu mode:n durable:bool fill :}
   tmp tmpu CLEANUP+
   tmp tmpu fill execute
   tmp tmpu mode durable STAGED-SYNC
   durable if
      tmp tmpu path pathu RENAME-SYNCED
   else
      tmp tmpu path pathu RENAME-FILE
   then
   path pathu tmp tmpu mode durable fill ;

: STAGED-CLEAN ( str -- str )
   2dup ATOMIC-CLEAN-TEMP ;

: STAGED-FORGET ( str -- str )
   2dup CLEANUP-FORGET ;

: STAGED-REPLACE ( R str [ R str -- R ] bool -- R )
   {: path:^ pathu fill durable:bool :}
   path pathu ATOMIC-OLD-MODE {: mode:n :}
   mode 0 >= if MODE-PRIVATE-FILE else MODE-0644 then {: create-mode :}
   path pathu create-mode RESERVE-MODE {: tmp:^ tmpu fd :}
   fd CLOSE-FD 0<> if
      FFI:ERRNO {: close-cause:n :}
      CAUSE-CLEAR
      tmp tmpu [: STAGED-CLEAN ;] catch {: clean-code:n :}
      2drop
      clean-code 0<> if
         s" staged replacement" s" close cleanup" clean-code
         tmp tmpu s" " STAGED-REPORT
      then
      E-FS-IO close-cause CAUSE-FAIL
   then
   path pathu tmp tmpu mode durable fill [: STAGED-RUN ;] catch
   {: code:n :}
   2drop 2drop 2drop drop
   code 0<> if
      ERRNO {: cause:n :}
      CAUSE-CLEAR
      tmp tmpu [: STAGED-CLEAN ;] catch {: clean-code:n :}
      2drop
      clean-code 0= if
         CAUSE-CLEAR
         tmp tmpu [: STAGED-FORGET ;] catch {: forget-code:n :}
         2drop
         forget-code 0<> if
            s" staged replacement" s" cleanup registration" forget-code
            tmp tmpu s" " STAGED-REPORT
         then
      else
         s" staged replacement" s" cleanup" clean-code
         tmp tmpu s" " STAGED-REPORT
      then
      code cause CAUSE-FAIL
   then
   tmp tmpu CLEANUP-FORGET ;

public
\ Renames fill's synced sibling over path, else removes it; fill must not end
\ RESERVE-SIBLING's path. Throws set FS:ERRNO or keep fill's, E-FS-SYNC after.
: REPLACE-STAGED ( R str [ R str -- R ] -- R )
   true STAGED-REPLACE ;

\ REPLACE-STAGED without the directory sync: a crash leaves path's old file,
\ the whole new one, or none where there was none.
: REPLACE-CACHED ( R str [ R str -- R ] -- R )
   false STAGED-REPLACE ;

;package
