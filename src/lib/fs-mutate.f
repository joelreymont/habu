\ fs-mutate.f - checked filesystem mutation: directories, links, copies, atomic
\ and staged replacement, temp paths, and the process exit cleanup registry.
\ Public names avoid the primitives' (the dictionary is case-insensitive):
\ MAKE-DIR and REMOVE-DIR, not mkdir or rmdir.
\ STORAGE CLASS. TASK-LOCAL staged paths; PROCESS-WIDE COPY-BUF and registry.

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
384 constant MODE-PRIVATE-FILE

public
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
17 constant EEXIST                    \ EEXIST on both targets

\ Process-wide, so COPY-FILE-STREAM is single-task: 8192 bytes is
\ more than the per-task band carries, and a second task that copies needs its
\ own buffer.
COPY-CAP SPAN-BUFFER: COPY-BUF

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
variable CLEANUP-CAP   CLEANUP-CAP scratch-cell-mark

\ Atomic cells require native cell alignment.
here data-base - negate 7 and allot
variable CLEANUP-N   CLEANUP-N scratch-cell-mark
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

: >PATHZ2 ( ptr u8 n -- ptr u8 )
   PATHZ2-BUF PATHZ-INTO ;

public
: SUFFIX-PATH ( ptr u8 n ptr u8 n SPAN:span<u8> -- n ) {: a:ptr u s:ptr su dst :}
   u 0 < if E-FS-PATH throw then
   su 0 < if E-FS-PATH throw then
   a u dst SPAN:COPY
   s su dst u SPAN:SKIP SPAN:COPY
   u su + ;

private
: CLEANUP-AT ( n -- ptr n ) {: idx :}
   idx 0 < if E-FS-CAPACITY throw then
   idx CLEANUP-CAP @ >= if E-FS-CAPACITY throw then
   CLEANUP-ROW @ idx CLEANUP-STRIDE * cells + ;

: CLEANUP-KIND ( n -- ptr n ) CLEANUP-AT CELL + ;

: CLEANUP-SLOT ( n -- SPAN:span<u8> )
   CLEANUP-AT 2 cells + BYTE-VIEW PATH-CAP SPAN:MAKE ;

: CLEANUP-PATH ( n -- ptr u8 n ) {: idx :}
   idx CLEANUP-SLOT idx CLEANUP-AT @ SPAN:TAKE SPAN:$ ;

\ Under the lock. A refused mapping leaves the row, its capacity and N as found.
: CLEANUP-GROW ( -- )
   CLEANUP-CAP @ {: cap :}
   cap 2 * CLEANUP-FIRST max {: grown :}
   grown CLEANUP-STRIDE * MEM:CELLS-ALLOC-COUNT MEM:ALLOC-CELLS {: row:ptr :}
   CLEANUP-ROW @ {: old:ptr :}
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
: REMOVE-FILE ( ptr u8 n -- ) {: a:ptr u :}
   a u >PATHZ unlink 0 < if E-FS-IO throw then ;

\ A throw sets FS:ERRNO.
: RENAME-FILE ( ptr u8 n ptr u8 n -- ) {: src:ptr srcu dst:ptr dstu :}
   src srcu >PATHZ dst dstu >PATHZ2 rename 0 < if E-FS-IO throw then ;

private
PROCESS-SYMBOLS
FUNCTION: LINK-CALL link ( ptr u8 ptr u8 -- i32 ) ;FUNCTION
FUNCTION: LINKAT-CALL linkat ( n ptr u8 n ptr u8 n -- i32 ) ;FUNCTION

\ Links NUL-terminated src to dst in dir, then unlinks src; false, src kept, if
\ dst names an entry. A failed unlink is E-FS-IO, dst published. One mount.
: RENAME-NOREPLACE ( n ptr u8 ptr u8 -- bool )
   {: dir src:ptr dst:ptr :}
   dir src dir dst 0 LINKAT-CALL 0<> if
      FFI:ERRNO EEXIST = if false exit then
      E-FS-IO ERRNO-FAIL
   then
   dir src 0 UNLINK-AT-CALL 0<> if E-FS-IO ERRNO-FAIL then
   true ;

public
\ A throw sets FS:ERRNO.
: CHMOD-MODE ( ptr u8 n n -- ) {: a:ptr u mode :}
   a u >PATHZ mode chmod 0 < if E-FS-IO throw then ;

\ A throw sets FS:ERRNO.
: CHMOD-X ( ptr u8 n -- ) {: a:ptr u :}
   a u STAT-MODE MODE-PERM and MODE-EXEC or {: mode :}
   a u mode CHMOD-MODE ;

\ A throw sets FS:ERRNO.
: MAKE-SYMLINK ( ptr u8 n ptr u8 n -- ) {: target:ptr targetu link:ptr linku :}
   target targetu >PATHZ link linku >PATHZ2 symlink 0 < if E-FS-IO throw then ;

\ A hard link; an existing LINK is E-FS-IO, not replaced. A throw sets FS:ERRNO.
: MAKE-LINK ( ptr u8 n ptr u8 n -- )
   {: target:ptr targetu link:ptr linku :}
   target targetu >PATHZ link linku >PATHZ2 LINK-CALL 0<> if E-FS-IO ERRNO-FAIL then ;

private
\ mkdir(2) is bound through libc, so a failure leaves its errno in FFI:ERRNO
\ for ERRNO-FAIL: the engine primitive collapses every failure to a bare -1.
PROCESS-SYMBOLS
FUNCTION: MKDIR-CALL mkdir ( ptr u8 n -- i32 ) ;FUNCTION
: MADE? ( ptr u8 n -- bool )
   MKDIR-CALL 0= ;

public
\ A throw sets FS:ERRNO.
: MKDIR-MODE ( ptr u8 n n -- ) {: a:ptr u mode :}
   a u >PATHZ mode MADE? 0= if E-FS-IO ERRNO-FAIL then ;

\ A throw sets FS:ERRNO.
: MAKE-DIR ( ptr u8 n -- )
   MODE-DIR MKDIR-MODE ;

\ A throw sets FS:ERRNO.
: REMOVE-DIR ( ptr u8 n -- ) {: a:ptr u :}
   a u >PATHZ rmdir 0 < if E-FS-IO throw then ;

\ WALK-UNMAP closes whatever descriptors the walk's frames still hold, however
\ the traversal ended, so the error paths below throw plainly.

private
\ The entry's own mode, never a link target's, lands in STAT-BUF. A name
\ already gone (ENOENT) is false: a missing root, or an entry removed since its
\ directory was read. Any other failure is E-FS-STAT.
: LSTAT-AT? ( n ptr u8 n -- bool ) {: parent:n name:ptr u:n :}
   parent name u >PATHZ STAT-BUF AT-NOFOLLOW STAT-AT-CALL 0= if true exit then
   FFI:ERRNO ENOENT = if false exit then
   E-FS-STAT ERRNO-FAIL ;


: UNLINK-AT ( n ptr u8 n n -- ) {: parent:n name:ptr u:n flags:n :}
   parent name u >PATHZ flags UNLINK-AT-CALL 0= if exit then
   E-FS-IO ERRNO-FAIL ;


\ The directory opens one depth down, O_NOFOLLOW, so a link swapped in since
\ the stat is refused, never followed. The name is staged before that depth's
\ frame is made: a child's name lies in its parent's dirent block, which
\ growing the frames moves.
: OPEN-AT ( ptr n n ptr u8 n -- ) {: ctx:ptr parent:n name:ptr u:n :}
   name u >PATHZ {: z:ptr :}
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
: OWN-NAME ( ptr n ptr u8 n -- ptr u8 n ) {: ctx:ptr name:ptr u:n :}
   ctx WALK-DEPTH@ 0 < if name u exit then
   ctx CUR-ENTRY DIRENT-NAME ;

: REMOVE-AT ( ptr n n ptr u8 n -- ) {: ctx:ptr parent:n name:ptr u:n :}
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
: ROOT-LEN ( ptr u8 n -- n ) {: a:ptr u :}
   u begin
      dup 1 > if dup 1 - a + c@ SLASH = else false then
   while 1 - repeat {: s :}
   s u < if a s SYMLINK? if s exit then then
   u ;

\ Caught as a whole, so one site closes the descriptors and releases the frames
\ for every way out, a root refused by ROOT-LEN's stat included.
: REMOVE-RUN ( ptr n ptr u8 n -- ptr n ptr u8 n ) {: ctx:ptr a:ptr u :}
   ctx AT-CWD a a u ROOT-LEN REMOVE-AT
   ctx a u ;

\ No flag guards the context: it is this call's alone, so the process exit hook
\ removes trees through CLEANUP-RUN whatever a task was doing.
: REMOVE-SCOPE ( ptr u8 n ptr u8 NUM:alloc-byte-len -- )
   drop CELL-VIEW {: a:ptr u ctx:ptr :}
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
: REMOVE-TREE ( ptr u8 n -- ) {: a:ptr u :}
   u 0 <= if CAUSE-CLEAR E-FS-PATH throw then
   a u WALK-HANDLE-BYTES MEM:BYTES-ALLOC-LEN [: REMOVE-SCOPE ;] MEM:WITH-BYTES ;

private
\ Only EEXIST can mean the folder is already there, so only it is checked
\ with a stat, which accepts a link to a directory. Any other failure throws at
\ once with mkdir's cause; a non-directory in the way throws with EEXIST.
: MKDIR-ONE ( ptr u8 n -- ) {: a:ptr u :}
   a u >PATHZ MODE-DIR MADE? if exit then
   FFI:ERRNO EEXIST <> if E-FS-IO ERRNO-FAIL then
   a u DIR? 0= if E-FS-IO EEXIST CAUSE-FAIL then ;

public
\ A throw sets FS:ERRNO.
: MAKE-DIRS ( ptr u8 n -- ) {: a:ptr u :}
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

: COPY-OPEN-SRC ( ptr u8 n -- n ) {: src:ptr srcu :}
   src srcu >PATHZ open-rd {: in :}
   in 0 < if E-FS-OPEN throw then
   in ;

\ Stack-preserving under `catch`: the two descriptors are the quotation's
\ declared window and no path of this body consumes them, so the caller may drop
\ them and read its own locals after the catch.
: COPY-PREPARE-DST ( n n -- n n )
   2dup >FD swap >FD swap PREPARE-DST ;

: COPY-OPEN-DST ( n ptr u8 n -- n ) {: in dst:ptr dstu :}
   dst dstu EXISTS? if
      dst dstu FILE? 0= if in -1 E-FS-OPEN COPY-THROW then
   then
   dst dstu >PATHZ2
   O-WRONLY O-CREAT or MODE-0644 open {: out :}
   out 0 < if in -1 E-FS-OPEN COPY-THROW then
   \ Opening must not truncate until the descriptor identities differ.
   in out [: COPY-PREPARE-DST ;] catch {: code :}
   2drop
   code 0<> if in out code COPY-THROW then
   out ;

\ The write cursor is the deepest argument, so a loop can leave it on the stack
\ and hand the call its three known values on top.
: COPY-WRITE-ONE ( n n n n -- n ) {: off in out u :}
   out COPY-BUF off SPAN:SKIP  u off - SPAN:TAKE SPAN:$ write {: wr :}
   wr 0 <= if in out E-FS-IO COPY-THROW then
   wr u off - > if in out E-FS-IO COPY-THROW then
   off wr + ;

: COPY-WRITE-CHUNK ( n n n -- ) {: u in out :}
   0 begin dup u < while
      in out u COPY-WRITE-ONE
   repeat drop ;

: COPY-READ-ONE ( n n -- n ) {: in out :}
   in COPY-BUF SPAN:$ read {: rd :}
   rd 0 < if in out E-FS-IO COPY-THROW then
   rd COPY-CAP > if in out E-FS-IO COPY-THROW then
   rd ;

: COPY-PUMP ( n n -- ) {: in out :}
   begin in out COPY-READ-ONE dup 0 > while
      in out COPY-WRITE-CHUNK
   repeat drop ;

public
: COPY-FILE-STREAM ( ptr u8 n ptr u8 n -- ) {: src:ptr srcu dst:ptr dstu :}
   src srcu COPY-OPEN-SRC {: in :}
   in dst dstu COPY-OPEN-DST {: out :}
   in out COPY-PUMP
   in CLOSE-COPY-FD
   out CLOSE-COPY-FD ;

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

: PATH-COLLISION? ( ptr u8 n -- bool ) {: a:ptr u :}
   a u EXISTS? if 0 0= exit then
   a u SYMLINK? ;

\ The longest `.tmp-<seed>-<attempt>` name FS:RESERVE-SIBLING and
\ ATOMIC-WRITE-FILE give a sibling: the largest seed, the largest nonnegative
\ cell `-1 1 rshift`, and the last attempt, so whether a directory has room for
\ its sibling never depends on the seed it drew.
6 -1 1 rshift U-DIGITS +
ATOMIC-RETRIES 1- U-DIGITS + constant SIBLING-LEN-MAX

: SIBLING-Z ( -- ptr u8 )
   ATOMIC-PATH SPAN:$ drop ;

: ATOMIC-BASE-OFF ( ptr u8 n -- n )
   {: path:ptr pathu:n :}
   pathu begin dup 0 > while
      dup 1 - path + c@ SLASH = if exit then
      1-
   repeat ;

\ `<prefix>.tmp-<seed>-<attempt>` in this task's ATOMIC-PATH. The prefix
\ is a destination's directory with its slash, or empty for a name relative to
\ that directory's descriptor. A prefix that leaves no room within PATH-CAP
\ for the longest sibling name is E-SPAN-CAPACITY whatever the seed, a semantic
\ refusal.
: BUILD-SIBLING ( ptr u8 n n n -- ptr u8 n ) {: pre:ptr preu seed attempt :}
   CAUSE-CLEAR
   preu 0 < if E-FS-PATH throw then
   preu SIBLING-LEN-MAX + PATH-CAP > if E-SPAN-CAPACITY throw then
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
FUNCTION: OPEN-CALL open ( ptr u8 n n -- i32 ) 2 VARIADIC ;FUNCTION

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
\ Create and open one candidate with the exclusive flags and mode: its
\ descriptor, -1 when the name is taken, E-FS-OPEN with the host's cause for
\ any other refusal.
: OPEN-CANDIDATE ( ptr u8 n n n -- n )
   {: path:ptr pathu flags mode :}
   path pathu >PATHZ flags mode OPEN-CALL {: fd :}
   fd 0 >= if fd exit then
   FFI:ERRNO EEXIST = if -1 exit then
   E-FS-OPEN ERRNO-FAIL ;

: ATOMIC-WRITE-FD ( n ptr u8 n -- ) {: fd src:ptr srcu :}
   0 begin dup srcu < while
      {: off:n :}
      fd src off + srcu off - write {: wr:n :}
      wr 0 <= if E-FS-IO throw then
      wr srcu off - > if E-FS-IO throw then
      off wr +
   repeat drop ;

public
\ A throw sets FS:ERRNO.
: ATOMIC-CLEAN-TEMP ( ptr u8 n -- ) {: temp:ptr tempu :}
   temp tempu PATH-COLLISION? if temp tempu REMOVE-FILE then ;

\ Resolve the complete spelling when it exists, including its final symlink.
\ For a new basename, resolve its parent and append the basename unchanged.
\ The answer is absolute and borrowed from this task's PATHZ2 buffer until the
\ next use of that buffer; callers retaining it copy it.
\ This buffer survives sibling reservation and is used unchanged at rename.
: ATOMIC-TARGET ( ptr u8 n -- ptr u8 n )
   {: path:ptr pathu:n :}
   path pathu >PATHZ {: input:ptr :}
   input PATHZ2-BUF SPAN:$ drop PATHZ-CAP realpath {: u:n :}
   u -2 = if CAUSE-CLEAR E-FS-PATH throw then
   u 0 >= if PATHZ2-BUF SPAN:$ drop u exit then
   input pathu SYMLINK? if E-FS-STAT throw then
   input pathu ATOMIC-BASE-OFF {: off:n :}
   pathu off - 0= if CAUSE-CLEAR E-FS-STAT throw then
   input off + pathu off - ATOMIC-PATH SPAN:COPY
   off 0= if s" ." >PATHZ else
      off 1 = if s" /" >PATHZ else
         input off 1 - >PATHZ
      then
   then {: parent:ptr :}
   parent PATHZ2-BUF SPAN:$ drop PATHZ-CAP realpath {: parentu:n :}
   parentu -2 = if CAUSE-CLEAR E-FS-PATH throw then
   parentu 0 < if E-FS-STAT ERRNO-FAIL then
   PATHZ2-BUF SPAN:$ drop parentu
   ATOMIC-PATH SPAN:$ drop pathu off -
   PATHZ2-BUF PATH-CAP SPAN:TAKE JOIN-PATH-INTO {: targetu:n :}
   PATHZ2-BUF SPAN:$ drop targetu ;

private
: ATOMIC-OLD-MODE ( ptr u8 n -- n ) {: path:ptr pathu :}
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
FUNCTION: OPENAT-CALL openat ( n ptr u8 n n -- i32 ) 3 VARIADIC ;FUNCTION
FUNCTION: FCHMOD-CALL fchmod ( n n -- i32 ) ;FUNCTION
FUNCTION: RENAMEAT-CALL renameat ( n ptr u8 n ptr u8 -- i32 ) ;FUNCTION
FUNCTION: FSYNC-CALL fsync ( n -- i32 ) ;FUNCTION
FUNCTION: FCNTL-CALL fcntl ( n n n -- i32 ) 2 VARIADIC ;FUNCTION

\ Darwin's fcntl that flushes the drive's own cache, which its fsync leaves to
\ a power loss: xnu bsd/sys/fcntl.h.
51 constant F-FULLFSYNC

\ Host errnos: macOS sys/errno.h, Linux asm-generic/errno.h.
9 constant EBADF
25 constant ENOTTY
: ENOTSUP ( -- n )
   HB-TARGET-MACOS? if 45 else 95 then ;

\ Reporting is best effort after a primary failure: a closed stderr must not
\ replace the fill or rename error. The sibling is named as a directory prefix
\ and a name, written apart from the 1 KiB string builder so even the longest
\ legal sibling can be named.
: STAGED-REPORT ( ptr u8 n n ptr u8 n ptr u8 n -- )
   {: label:ptr labelu:n code:n path:ptr pathu:n name:ptr nameu:n :}
   SB-RESET
   s" hb: staged replacement " SB-APPEND
   label labelu SB-APPEND
   s"  threw " SB-APPEND
   code FMT:SB-INT
   s"  at " SB-APPEND
   2 SB$ write drop
   2 path pathu write drop
   2 name nameu write drop
   2 S\" \n" write drop ;

\ The destination's directory, opened for search only for every call that names
\ the sibling or the destination, so a directory the caller may write and
\ search but not list takes the write. The target is absolute, so its
\ directory is what precedes its last slash, or the root.
: ATOMIC-OPEN-DIR ( ptr u8 n -- n ) {: dst:ptr dstu :}
   dst dstu ATOMIC-BASE-OFF 1- 1 max {: diru :}
   AT-CWD dst diru >PATHZ SEARCH-DIR-FLAGS OPEN-AT-CALL {: dir :}
   dir 0 < if E-FS-OPEN ERRNO-FAIL then
   dir ;

\ Create the sibling in dir exclusively, its name staged for SIBLING-Z:
\ the name's length and the descriptor.
: ATOMIC-RESERVE-AT ( n n n -- n n )
   {: dir seed mode :}
   0 begin dup ATOMIC-RETRIES < while
      dup {: attempt:n :}
      s" " seed attempt BUILD-SIBLING {: name:ptr nameu :}
      nameu ATOMIC-PATH FFI:CSTR! drop
      dir name EXCL-WRITE-FLAGS mode OPENAT-CALL {: fd:n :}
      fd 0 >= if drop nameu fd exit then
      FFI:ERRNO EEXIST <> if E-FS-OPEN ERRNO-FAIL then
      1+
   repeat drop
   E-FS-IO ERRNO-FAIL ;

: ATOMIC-SYNC ( n -- n ) {: fd :}
   HB-TARGET-MACOS? if
      fd F-FULLFSYNC 0 FCNTL-CALL 0= if fd exit then
      FFI:ERRNO dup EINVAL = over ENOTTY = or swap ENOTSUP = or
      0= if E-FS-IO ERRNO-FAIL then
   then
   fd FSYNC-CALL 0<> if E-FS-IO ERRNO-FAIL then
   fd ;

\ Stack-preserving under `catch`.
: ATOMIC-FILL ( n ptr u8 n n -- n ptr u8 n n ) {: fd src:ptr srcu mode :}
   fd src srcu ATOMIC-WRITE-FD
   mode 0 >= if
      fd mode FCHMOD-CALL 0<> if E-FS-IO ERRNO-FAIL then
   then
   fd ATOMIC-SYNC drop
   fd src srcu mode ;

\ Linux refuses fsync on a search-only descriptor (EBADF), so dir is reopened
\ to read; one the caller may not list stays unsynced.
: ATOMIC-SYNC-DIR ( n -- n ) {: dir :}
   dir [: ATOMIC-SYNC ;] catch {: first:n :}
   drop
   first 0= if dir exit then
   ERRNO EBADF <> if first throw then
   dir S\" .\z" drop OPEN-DIR-FLAGS OPEN-AT-CALL {: rd:n :}
   rd 0 < if
      FFI:ERRNO EACCES = if dir exit then
      E-FS-IO ERRNO-FAIL
   then
   rd [: ATOMIC-SYNC ;] catch {: code:n :}
   drop
   rd CLOSE-FD drop
   code 0<> if code throw then
   dir ;

\ Closed exactly once, before publish: a failed close may already have freed
\ the descriptor, so retrying it could close an unrelated recycled fd.
: ATOMIC-CLOSE ( n -- n ) {: fd :}
   fd CLOSE-FD 0<> if E-FS-IO ERRNO-FAIL then
   fd ;

: ATOMIC-PUBLISH ( n ptr u8 -- n ptr u8 ) {: dir name:ptr :}
   dir SIBLING-Z dir name RENAMEAT-CALL 0<> if E-FS-IO ERRNO-FAIL then
   dir name ;

: ATOMIC-UNLINK ( n -- n ) {: dir :}
   dir SIBLING-Z 0 UNLINK-AT-CALL 0<> if E-FS-IO ERRNO-FAIL then
   dir ;

: ATOMIC-ABANDON ( n ptr u8 n n n -- ) {: dir dst:ptr off nameu code :}
   ERRNO {: cause :}
   dir [: ATOMIC-UNLINK ;] catch {: clean-code:n :}
   drop
   clean-code 0<> if
      s" cleanup" clean-code dst off SIBLING-Z nameu STAGED-REPORT
   then
   code cause CAUSE-FAIL ;

\ Stack-preserving under `catch`; a throw after the publish leaves the new file.
\ Answers whether it published; a claim on a taken name unlinks the sibling.
: ATOMIC-IN-DIR ( n ptr u8 n ptr u8 n n n bool -- n ptr u8 n ptr u8 n n n bool )
   {: dir dst:ptr off src:ptr srcu mode seed claim?:bool :}
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
: ATOMIC-WRITE-SEED ( ptr u8 n ptr u8 n n bool bool -- bool )
   {: path:ptr pathu src:ptr srcu seed private?:bool claim?:bool :}
   srcu 0 < if CAUSE-CLEAR E-FS-CAPACITY throw then
   path pathu ATOMIC-TARGET {: dst:ptr dstu :}
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

public
\ Opens a new `.tmp-*` sibling of path, the caller's to remove; this task's next
\ sibling, ATOMIC-WRITE-* or ATOMIC-TARGET ends its path. A throw sets FS:ERRNO.
: RESERVE-SIBLING ( ptr u8 n -- ptr u8 n n ) {: path:ptr pathu :}
   pathu 0 < if CAUSE-CLEAR E-FS-PATH throw then
   path path pathu ATOMIC-BASE-OFF {: pre:ptr preu :}
   ATOMIC-SEED {: seed :}
   0 begin dup ATOMIC-RETRIES < while
      dup {: attempt:n :}
      pre preu seed attempt BUILD-SIBLING
      2dup EXCL-WRITE-FLAGS MODE-0644 OPEN-CANDIDATE {: fd:n :}
      fd 0 >= if rot drop fd exit then
      2drop 1+
   repeat drop
   E-FS-IO ERRNO-FAIL ;

\ Replace path's file with src's bytes; a reader or crash sees old or whole new,
\ durable as ATOMIC-SYNC-DIR says. A throw, even past the rename, sets FS:ERRNO.
: ATOMIC-WRITE-FILE ( ptr u8 n ptr u8 n -- )
   {: path:ptr pathu src:ptr srcu :}
   path pathu src srcu ATOMIC-SEED false false ATOMIC-WRITE-SEED drop ;


\ ATOMIC-WRITE-FILE for a file only its owner may read: sibling and result are
\ 0600 whatever the destination held. A throw sets FS:ERRNO.
: ATOMIC-WRITE-PRIVATE ( ptr u8 n ptr u8 n -- )
   {: path:ptr pathu src:ptr srcu :}
   path pathu src srcu ATOMIC-SEED true false ATOMIC-WRITE-SEED drop ;


\ ATOMIC-WRITE-PRIVATE to a free name, else false, the entry untouched. A final
\ symlink counts as its target, a dangling one E-FS-STAT. A throw sets FS:ERRNO.
: ATOMIC-CREATE-PRIVATE ( ptr u8 n ptr u8 n -- bool )
   {: path:ptr pathu src:ptr srcu :}
   path pathu src srcu ATOMIC-SEED true true ATOMIC-WRITE-SEED ;


\ The longest base FS:MKTEMP-DIR-IN makes its temp in for a prefix of n bytes:
\ its `<base>/<prefix>-<seed>-<attempt>` path still fits PATH-CAP with the
\ largest seed, the largest nonnegative cell, and the last attempt, so whether
\ a base has room for one never depends on the seed it drew.
: TEMP-DIR-BASE-MAX ( n -- n ) {: prefixu:n :}
   PATH-CAP 3 - prefixu -
   -1 1 rshift U-DIGITS -
   TMP-RETRIES 1- U-DIGITS - ;

private
: BUILD-TEMP-TRY ( ptr u8 n ptr u8 n n n -- ptr u8 n ) {: base:ptr baseu prefix:ptr prefixu seed attempt :}
   baseu prefixu TEMP-DIR-BASE-MAX > if E-SPAN-CAPACITY throw then
   SB-RESET
   base baseu SB-APPEND
   base baseu JOIN-SEPARATED? 0= if SLASH SB-APPEND-C then
   prefix prefixu SB-APPEND
   DASH SB-APPEND-C
   seed FMT:SB-U
   DASH SB-APPEND-C
   attempt FMT:SB-U
   SB$ {: a:ptr u :}
   a u TMP-PATH-BUF SPAN:COPY
   TMP-PATH-BUF u SPAN:TAKE SPAN:$ ;

: TMP-COLLISION? ( ptr u8 n -- bool ) {: a:ptr u :}
   a u PATH-COLLISION? ;

: MKDIR-CANDIDATE? ( ptr u8 n -- bool ) {: a:ptr u :}
   a u >PATHZ MODE-PRIVATE-DIR mkdir {: rc :}
   rc 0= if 0 0= exit then
   a u TMP-COLLISION? if 0 0= 0= exit then
   E-FS-IO throw ;

: MKTEMP-DIR-SEED ( ptr u8 n ptr u8 n n -- ptr u8 n ) {: base:ptr baseu prefix:ptr prefixu seed :}
   0 begin dup TMP-RETRIES < while
      dup {: attempt:n :}
      base baseu prefix prefixu seed attempt BUILD-TEMP-TRY
      2dup MKDIR-CANDIDATE? if rot drop exit then
      2drop 1+
   repeat drop
   E-FS-IO throw ;

: MKTEMP-SEED ( ptr u8 n ptr u8 n n -- ptr u8 n n )
   {: base:ptr baseu prefix:ptr prefixu seed :}
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
: MKTEMP-DIR-IN ( ptr u8 n ptr u8 n -- ptr u8 n )
   mono-ns MKTEMP-DIR-SEED ;

private
\ realpath of a temp root into this task's PATHZ2 buffer: the canonical
\ spelling's length, below 0 when realpath refuses the root. FS:MKTEMP-DIR-IN
\ and FS:MKTEMP build and make their candidates through SB, TMP-PATH-BUF and
\ >PATHZ only, so the spelling stands through every attempt.
: TEMP-REAL ( ptr u8 n -- n )
   >PATHZ PATHZ2-BUF SPAN:$ drop PATHZ-CAP realpath ;

\ A temp root that cannot hold the temp is refused before anything is made,
\ with one line on fd 2 naming TMPDIR, why, and the root, then a throw: a root
\ past PATH-CAP or with no room for the temp name (TEMP-DIR-BASE-MAX) is
\ E-SPAN-CAPACITY, one that is not a directory E-FS-DIR, and one realpath
\ refuses (a canonical spelling past PATH-CAP) E-FS-IO. Without the check each
\ dies later on an error that does not name TMPDIR. The answer is the canonical
\ spelling, borrowed from this task's PATHZ2 buffer. The line is three writes,
\ not one SB$ write: a root of PATH-CAP bytes with the reason overflows SB-CAP.
: TMPDIR-REFUSE ( ptr u8 n ptr u8 n -- ) {: a:ptr u why:ptr whyu :}
   2 why whyu write drop
   2 a u write drop
   2 S\" \n" write drop ;

: TMPDIR-LONG ( ptr u8 n -- ) {: a:ptr u :}
   a u s" hb: TMPDIR is too long for a temp path: " TMPDIR-REFUSE
   E-SPAN-CAPACITY throw ;

: TMPDIR-CHECK ( ptr u8 n n -- ptr u8 n ) {: a:ptr u prefixu :}
   u PATH-CAP > if a u TMPDIR-LONG then
   a u DIR? 0= if
      a u s" hb: TMPDIR is not a directory: " TMPDIR-REFUSE
      E-FS-DIR throw
   then
   a u TEMP-REAL {: real:n :}
   real 0 < if
      a u s" hb: TMPDIR has no canonical path: " TMPDIR-REFUSE
      E-FS-IO throw
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
: TEMP-ROOT ( n -- ptr u8 n )
   {: prefixu:n :}
   s" TMPDIR" GETENV dup 0= if 2drop s" /tmp" then
   prefixu TMPDIR-CHECK ;

public
: MKTEMP-DIR ( ptr u8 n -- ptr u8 n )
   {: prefix:ptr prefixu :}
   prefixu TEMP-ROOT prefix prefixu MKTEMP-DIR-IN ;

\ Create and open a new file `<root>/<prefix>-<seed>-<attempt>` under the temp
\ root, read-write and mode 0600, as mkstemp(3) does. Answers its path, which
\ stays this task's until its next FS:MKTEMP, FS:MKTEMP-DIR or
\ FS:MKTEMP-DIR-IN, and the descriptor. The caller owns both and the removal;
\ nothing is registered.
: MKTEMP ( ptr u8 n -- ptr u8 n fd )
   {: prefix:ptr prefixu :}
   prefixu TEMP-ROOT prefix prefixu mono-ns MKTEMP-SEED >FD ;

\ One task only: a forked child's inherited paths are its parent's to remove,
\ and a registration another task was in at the fork leaves the lock held.
: CLEANUP-RESET ( -- )
   0 CLEANUP-N !
   CLEANUP-RELEASE ;

private
: CLEANUP-REMOVE ( n -- ) {: idx :}
   idx CLEANUP-PATH {: a:ptr u :}
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
\ counted slot names a path and a refused registration - a path past the slot
\ (E-SPAN-CAPACITY) or a row that cannot grow - leaves the count it found.
: CLEANUP-APPEND ( ptr u8 n n -- ) {: a:ptr u kind :}
   CLEANUP-N @ {: idx :}
   idx CLEANUP-CAP @ = if CLEANUP-GROW then
   a u idx CLEANUP-SLOT SPAN:COPY
   u idx CLEANUP-AT !
   kind idx CLEANUP-KIND !
   idx 1+ CLEANUP-N ! ;

: CLEANUP-KIND+ ( ptr u8 n n -- ) {: a:ptr u kind :}
   ['] CLEANUP-AT-EXIT AT-EXIT:ADD
   u 0 < if E-FS-PATH throw then
   kind CLEANUP-FILE <>
   kind CLEANUP-DIR <> and
   kind CLEANUP-TREE <> and if E-FS-IO throw then
   CLEANUP-ACQUIRE
   a u kind [: CLEANUP-APPEND ;] [: CLEANUP-RELEASE ;] finally ;

public
: CLEANUP+ ( ptr u8 n -- )
   CLEANUP-FILE CLEANUP-KIND+ ;

: CLEANUP-DIR+ ( ptr u8 n -- )
   CLEANUP-DIR CLEANUP-KIND+ ;

: CLEANUP-TREE+ ( ptr u8 n -- )
   CLEANUP-TREE CLEANUP-KIND+ ;

private
: CLEANUP-NAMES? ( ptr u8 n n -- bool ) {: a:ptr u idx :}
   idx CLEANUP-AT @ u <> if 0 0= 0= exit then
   idx CLEANUP-PATH a u STR= ;

: CLEANUP-FIND ( ptr u8 n -- n ) {: a:ptr u :}
   CLEANUP-N @ begin dup 0 > while
      1 -
      dup >r a u r> CLEANUP-NAMES? if exit then
   repeat
   drop -1 ;

\ The slots above idx move down one in a single forward copy, which is safe
\ because the destination lies below the source.
: CLEANUP-DROP ( ptr u8 n -- ) {: a:ptr u :}
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
: CLEANUP-FORGET ( ptr u8 n -- ) {: a:ptr u :}
   u 0 <= if E-FS-PATH throw then
   CLEANUP-ACQUIRE
   a u [: CLEANUP-DROP ;] [: CLEANUP-RELEASE ;] finally ;

private
\ Stack-preserving under `catch`: the caller still holds the sibling to remove
\ when a step throws.
: STAGED-RUN ( R ptr u8 n ptr u8 n [ R ptr u8 n -- R ] -- R ptr u8 n ptr u8 n [ R ptr u8 n -- R ] )
   {: path:ptr pathu tmp:ptr tmpu fill :}
   tmp tmpu CLEANUP+
   tmp tmpu fill execute
   tmp tmpu path pathu RENAME-FILE
   path pathu tmp tmpu fill ;

: STAGED-CLEAN ( ptr u8 n -- ptr u8 n )
   2dup ATOMIC-CLEAN-TEMP ;

: STAGED-FORGET ( ptr u8 n -- ptr u8 n )
   2dup CLEANUP-FORGET ;

public
\ Renames the sibling fill writes over path, or removes it on a throw or `die`;
\ fill must not end RESERVE-SIBLING's path. Throws set FS:ERRNO or keep fill's.
: REPLACE-STAGED ( R ptr u8 n [ R ptr u8 n -- R ] -- R )
   {: path:ptr pathu fill :}
   path pathu RESERVE-SIBLING {: tmp:ptr tmpu fd :}
   fd CLOSE-FD 0<> if
      FFI:ERRNO {: close-cause:n :}
      tmp tmpu [: STAGED-CLEAN ;] catch {: clean-code:n :}
      2drop
      clean-code 0<> if
         s" close cleanup" clean-code tmp tmpu s" " STAGED-REPORT
      then
      E-FS-IO close-cause CAUSE-FAIL
   then
   path pathu tmp tmpu fill [: STAGED-RUN ;] catch {: code:n :}
   drop 2drop 2drop
   code 0<> if
      ERRNO {: cause:n :}
      tmp tmpu [: STAGED-CLEAN ;] catch {: clean-code:n :}
      2drop
      clean-code 0= if
         tmp tmpu [: STAGED-FORGET ;] catch {: forget-code:n :}
         2drop
         forget-code 0<> if
            s" cleanup registration" forget-code tmp tmpu s" " STAGED-REPORT
         then
      else
         s" cleanup" clean-code tmp tmpu s" " STAGED-REPORT
      then
      code cause CAUSE-FAIL
   then
   tmp tmpu CLEANUP-FORGET ;

;package
