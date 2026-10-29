\ fs-mutate.f - checked filesystem mutation: directories, links, copies, atomic
\ and staged replacement, temp paths, and the process exit cleanup registry.
\ Public names avoid the primitives' (the dictionary is case-insensitive):
\ MAKE-DIR and REMOVE-DIR, not mkdir or rmdir. Path checks and the PATH-CAP gate
\ are src/lib/fs.f's; the FS-MUT-ABI band is declared in src/habu/layout.f.
\ STORAGE CLASS. TASK-LOCAL staged paths; PROCESS-WIDE COPY-BUF and registry.

require src/lib/errors.f
require src/lib/string.f
require src/lib/fmt.f
require src/lib/fs.f
require src/lib/fs-identity.f
require src/lib/ffi-abi.f
require src/lib/span.f
require src/lib/memory.f
require src/lib/image-lifecycle.f

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

\ Process-wide, so COPY-FILE and COPY-FILE-STREAM are single-task: 8192 bytes is
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
\ A capture keeps the process's registrations and the image starts empty: N,
\ CAP and ROW are IMAGE-LIFECYCLE process cells, stored as 0 in the image. They
\ are declared at the first registration, not at load: a load inside a native
\ build's capture window would spend slots of the host's 64-slot process-cell
\ table on that window's DATA addresses, which a later snapshot would zero in
\ storage that is no longer the program's. CLEANUP-DECLARED is no process cell:
\ a snapshot keeps the table that holds the declarations, so it keeps the flag.
PATH-CAP CELL 1- + CELL / 2 + constant CLEANUP-STRIDE
8 constant CLEANUP-FIRST
TYPED-VARIABLE CLEANUP-ROW ptr n
variable CLEANUP-CAP
variable CLEANUP-DECLARED

\ Atomic cells require native cell alignment.
here data-base - negate 7 and allot
variable CLEANUP-N
variable CLEANUP-LOCK

\ The row's address is declared as the cell it is: IMAGE-LIFECYCLE:PROCESS-CELL
\ takes ( ptr n ), and an address becomes one only through a private cast.
CAST: >CELL ( n -- ptr n )
: DECLARE-CLEANUP ( -- )
   CLEANUP-DECLARED @ 0<> if exit then
   CLEANUP-N IMAGE-LIFECYCLE:PROCESS-CELL
   CLEANUP-CAP IMAGE-LIFECYCLE:PROCESS-CELL
   CLEANUP-ROW NULL-PTR - >CELL IMAGE-LIFECYCLE:PROCESS-CELL
   1 CLEANUP-DECLARED ! ;

\ The staged paths are task-local, in the FS-MUT-ABI band of the per-task DATA
\ region (`data-base` is the running task's): the path FS:MKTEMP-DIR-IN or
\ FS:MKTEMP answers stays this task's until it calls either again, and a new
\ task's paths start empty. The band is declared in src/habu/layout.f, not as
\ TASK:+USER rows, because tools/native-build-core.f requires this file to
\ build the engine and must not load src/lib/task.f.
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

\ `atomic-cas` and `atomic!` are engine primitives (src/habu/prims.f), so the
\ lock needs no src/lib/task.f.
: CLEANUP-SELF ( -- n ) data-base BYTE-VIEW NULL-PTR BYTE-VIEW - ;

: CLEANUP-ACQUIRE ( -- )
   CLEANUP-SELF {: self :}
   begin 0 self CLEANUP-LOCK atomic-cas 0= until ;

: CLEANUP-RELEASE ( -- ) 0 CLEANUP-LOCK atomic! ;

public
: REMOVE-FILE ( ptr u8 n -- ) {: a:ptr u :}
   a u >PATHZ unlink 0 < if E-FS-IO throw then ;

: RENAME-FILE ( ptr u8 n ptr u8 n -- ) {: src:ptr srcu dst:ptr dstu :}
   src srcu >PATHZ dst dstu >PATHZ2 rename 0 < if E-FS-IO throw then ;

private
PROCESS-SYMBOLS
FUNCTION: LINK-CALL link ( ptr u8 ptr u8 -- i32 ) ;FUNCTION
: LINK-NEW? ( ptr u8 ptr u8 -- bool )
   LINK-CALL 0<> if
      FFI:ERRNO EEXIST = if false exit then
      E-FS-IO throw
   then
   true ;

\ The link claims the destination atomically. The caller owns a regular-file
\ source on the same filesystem until this call returns. Source removal is a
\ second step; if it fails, the published destination stays and E-FS-IO throws.
: RENAME-NOREPLACE ( ptr u8 n ptr u8 n -- bool ) {: src:ptr srcu dst:ptr dstu :}
   src srcu >PATHZ dst dstu >PATHZ2 LINK-NEW? 0= if false exit then
   src srcu REMOVE-FILE
   true ;

public
: CHMOD-MODE ( ptr u8 n n -- ) {: a:ptr u mode :}
   a u >PATHZ mode chmod 0 < if E-FS-IO throw then ;

: CHMOD-X ( ptr u8 n -- ) {: a:ptr u :}
   a u STAT-MODE MODE-PERM and MODE-EXEC or {: mode :}
   a u mode CHMOD-MODE ;

: MAKE-SYMLINK ( ptr u8 n ptr u8 n -- ) {: target:ptr targetu link:ptr linku :}
   target targetu >PATHZ link linku >PATHZ2 symlink 0 < if E-FS-IO throw then ;

\ A hard link; an existing LINK is E-FS-IO, never replaced.
: MAKE-LINK ( ptr u8 n ptr u8 n -- )
   {: target:ptr targetu link:ptr linku :}
   target targetu >PATHZ link linku >PATHZ2 LINK-CALL 0<> if E-FS-IO throw then ;

private
\ mkdir(2) is bound through libc, the way LINK-CALL binds link(2), so a
\ failure leaves its errno in FFI:ERRNO for ERRNO-FAIL: the engine primitive
\ collapses every failure to a bare -1 (src/habu/habu1.f's errno rule).
PROCESS-SYMBOLS
FUNCTION: MKDIR-CALL mkdir ( ptr u8 n -- i32 ) ;FUNCTION
: MADE? ( ptr u8 n -- bool )
   MKDIR-CALL 0= ;

public
: MKDIR-MODE ( ptr u8 n n -- ) {: a:ptr u mode :}
   a u >PATHZ mode MADE? 0= if E-FS-IO ERRNO-FAIL then ;

: MAKE-DIR ( ptr u8 n -- )
   MODE-DIR MKDIR-MODE ;

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

\ Only a directory is traversed; every other inode, a link to a directory
\ among them, belongs to unlink. A child's name stays in its parent's dirent
\ block, one depth up from the blocks the child's own traversal reads.
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

\ Caught as a whole, the way src/lib/fs.f catches WALK-RUN, so one site closes
\ the descriptors and releases the frames for every way out, a root refused by
\ ROOT-LEN's stat included.
: REMOVE-RUN ( ptr n ptr u8 n -- ptr n ptr u8 n ) {: ctx:ptr a:ptr u :}
   ctx AT-CWD a a u ROOT-LEN REMOVE-AT
   ctx a u ;

\ The WITH-BYTES body, built as src/lib/fs.f's WALK-SCOPE. No flag guards
\ the context: it is this call's alone, so the process exit hook removes trees
\ through CLEANUP-RUN whatever a task was doing.
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
\ Removes the tree at a, or unlinks a symlink a names without descending, a
\ root spelled with a trailing slash included; a missing root is a no-op and
\ any partial failure throws. Only the root path meets PATH-CAP: each entry
\ below is named relative to its parent's descriptor, so only the descriptors
\ the process may hold open bound the depth. Each call maps its own walk
\ context, so two tasks remove two trees at once and a WALK-FILES callback
\ removes a subtree.
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
\ `cap` is how much of the copy buffer this call may use: the narrowing refuses
\ a cap past the buffer (E-SPAN-RANGE) and a negative one, so neither needs a
\ hand-written comparison. A file larger than `cap` is still READ-ALL's
\ E-FS-CAPACITY.
: COPY-FILE ( ptr u8 n ptr u8 n n -- ) {: src:ptr srcu dst:ptr dstu cap :}
   src srcu COPY-BUF cap SPAN:TAKE SPAN:$ READ-ALL {: n :}
   dst dstu COPY-BUF n SPAN:TAKE SPAN:$ WRITE-ALL ;

PROCESS-SYMBOLS
FUNCTION: TRUNCATE-CALL ftruncate ( n n -- i32 ) ;FUNCTION
: PREPARE-DST ( fd fd -- ) {: source dest :}
   source dest SAME-OPEN-FILE? if E-FS-OPEN throw then
   dest FD>N 0 TRUNCATE-CALL 0<> if E-FS-IO throw then ;

\ The stream copy's descriptors, counts and write cursor are locals and stack
\ values of the call. A descriptor a helper must close on its error path is a
\ parameter: -1 stands for "not open yet", which CLOSE-COPY-FD skips.
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

\ The calls ATOMIC-WRITE-FILE makes relative to its destination's directory:
\ openat with a mode for the exclusive create, fchmod for the mode copy and
\ renameat to publish. Each answers -1 with the cause in FFI:ERRNO.
PROCESS-SYMBOLS
FUNCTION: OPENAT-CALL openat ( n ptr u8 n n -- i32 ) 3 VARIADIC ;FUNCTION
FUNCTION: FCHMOD-CALL fchmod ( n n -- i32 ) ;FUNCTION
FUNCTION: RENAMEAT-CALL renameat ( n ptr u8 n ptr u8 -- i32 ) ;FUNCTION

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

\ Stack-preserving under `catch`. The write records its own cause, a refused
\ mode copy fchmod's.
: ATOMIC-FILL ( n ptr u8 n n -- n ptr u8 n n ) {: fd src:ptr srcu mode :}
   fd src srcu ATOMIC-WRITE-FD
   mode 0 >= if
      fd mode FCHMOD-CALL 0<> if E-FS-IO ERRNO-FAIL then
   then
   fd src srcu mode ;

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

\ After a first failure the sibling goes; one that cannot go is named on fd 2,
\ as REPLACE-STAGED names its own. The first failure's code and cause are
\ thrown again whatever the removal or the report met.
: ATOMIC-ABANDON ( n ptr u8 n n n -- ) {: dir dst:ptr off nameu code :}
   ERRNO {: cause :}
   dir [: ATOMIC-UNLINK ;] catch {: clean-code:n :}
   drop
   clean-code 0<> if
      s" cleanup" clean-code dst off SIBLING-Z nameu STAGED-REPORT
   then
   code cause CAUSE-FAIL ;

\ The write inside the destination's directory dir, whose name for the
\ destination starts at off. Stack-preserving under `catch`, so the caller
\ closes dir however it ends. The sibling's descriptor is closed once whatever
\ the write met; after a failed write its close can only fail too.
: ATOMIC-IN-DIR ( n ptr u8 n ptr u8 n n n -- n ptr u8 n ptr u8 n n n )
   {: dir dst:ptr off src:ptr srcu mode seed :}
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
   dir dst off + [: ATOMIC-PUBLISH ;] catch {: rename-code:n :}
   2drop
   rename-code 0<> if dir dst off nameu rename-code ATOMIC-ABANDON then
   dir dst off src srcu mode seed ;

\ The sibling and the destination are named relative to the destination's
\ directory, so no sibling path is joined and only the destination's absolute
\ spelling meets PATH-CAP. A private write publishes 0600; any other gives an
\ existing regular destination's mode, else 0644 under the umask.
: ATOMIC-WRITE-SEED ( ptr u8 n ptr u8 n n bool -- )
   {: path:ptr pathu src:ptr srcu seed private?:bool :}
   srcu 0 < if CAUSE-CLEAR E-FS-CAPACITY throw then
   path pathu ATOMIC-TARGET {: dst:ptr dstu :}
   dstu PATHZ2-BUF FFI:CSTR! drop
   private? if MODE-PRIVATE-FILE else dst dstu ATOMIC-OLD-MODE then {: mode:n :}
   dst dstu ATOMIC-OPEN-DIR {: dir :}
   dir dst dst dstu ATOMIC-BASE-OFF src srcu mode seed
   [: ATOMIC-IN-DIR ;] catch {: code:n :}
   2drop 2drop 2drop drop
   ERRNO {: cause :}
   dir CLOSE-FD drop
   code 0<> if code cause CAUSE-FAIL then ;

public
\ Create and open the unique `.tmp-*` sibling in path's directory, for a writer
\ that streams its file or must finish it before RENAME-FILE publishes it.
\ Answers the sibling's path, which stays this task's until its next
\ reservation, and the descriptor. The caller owns both and the removal.
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

: ATOMIC-WRITE-FILE ( ptr u8 n ptr u8 n -- ) {: path:ptr pathu src:ptr srcu :}
   path pathu src srcu ATOMIC-SEED false ATOMIC-WRITE-SEED ;


\ ATOMIC-WRITE-FILE for a file only its owner may read: the sibling is created
\ 0600 and the published file is 0600 whatever the destination held.
: ATOMIC-WRITE-PRIVATE ( ptr u8 n ptr u8 n -- )
   {: path:ptr pathu src:ptr srcu :}
   path pathu src srcu ATOMIC-SEED true ATOMIC-WRITE-SEED ;


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

\ The file twin of MKTEMP-DIR-SEED: each candidate is created and opened
\ read-write, mode 0600, refused when the name exists.
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
\ keeps the caller's spelling: a relative base gives a relative answer.
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
\ refuses (ENAMETOOLONG on Darwin) E-FS-IO. Without the check each dies later
\ on an error that does not name TMPDIR. The answer is the canonical spelling,
\ borrowed from this task's PATHZ2 buffer. The line is three writes, as
\ src/core/include.f names a refused source path, not one SB$ write: a root of
\ PATH-CAP bytes with the reason overflows SB-CAP.
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

\ The one temp root a process reads from its environment: TMPDIR, or /tmp when
\ TMPDIR is unset or empty, checked for a prefix of n bytes and answered in its
\ canonical spelling. Every temp FS:MKTEMP-DIR or FS:MKTEMP makes thus has a
\ canonical path: a root reached through a symlink (macOS's /tmp is
\ /private/tmp) would otherwise give paths that differ from the canonical ones
\ the engine records for the same files. A caller that hands its child a TMPDIR
\ of its own (test/gate-pool.f) owns and removes it, so a temp made there goes
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

\ Its callers are a just-forked child (src/lib/process-fork.f registers it with
\ FORK-CHILD) and single-task fixtures, where no other task can be inside the
\ registry: a registration another task was in when the process forked is part
\ of what the fork copied, so the lock opens with the count, as FORK-CHILD:RESET
\ opens its own.
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
\ moved bytes. A task that registers or forgets meanwhile waits for the run.
\ The exit hook runs this on whichever task exits, and that task may already
\ hold the lock: a `die` inside a registration, a forget or a run of its own.
\ It then runs under the hold it has, which no other task can move: every
\ counted slot is whole, since a registration counts its slot only once it is
\ written and a growth publishes the new row before releasing the old.
: CLEANUP-RUN ( -- )
   CLEANUP-LOCK atomic@ CLEANUP-SELF = if
      CLEANUP-REMOVE-ALL exit
   then
   CLEANUP-ACQUIRE
   [: CLEANUP-REMOVE-ALL ;] [: CLEANUP-RELEASE ;] finally ;


private
\ CLEANUP-AT-EXIT runs from the engine's process-exit vector (src/habu/layout.f
\ EXIT-HOOK-CELL), called once with the vector cleared just before the
\ exit_group of a normal top-level exit, a `die`, an uncaught throw, or a
\ stripped application's return from MAIN. A restored image starts with an
\ empty table and the relocated vector.
\
\ A removal that fails is reported, never swallowed: the code goes to fd 2 as
\ one line and the exit code the process carries is kept (the engine preserves
\ it across the call). The string builder and fmt live in engine bands below a
\ stripped image's DATA window, which a stripped image maps fresh and zeroed.
\ Measured: a stripped MAIN that requires src/lib/fmt.f prints -2105 through
\ both FMT:.INT and SB-RESET FMT:SB-INT SB$. Nothing that ran before this line
\ reads the builder again, so resetting it is safe.
: EXIT-REPORT ( n -- ) {: code :}
   SB-RESET
   s" hb: cleanup at exit threw " SB-APPEND
   code FMT:SB-INT
   S\" \n" SB-APPEND
   2 SB$ write drop ;

\ The exit-hook words are this file's one raw boundary: the vector is a fixed
\ engine DATA cell holding a code pointer, reached through `data-base`. Arming
\ is the only place an opaque xt is handled.
\
\ The vector is a chain, not a claim. Arming finds zero, and ours goes in; ours,
\ and nothing happens; or a foreign vector, which is saved in EXIT-PREV before
\ ours replaces it. CLEANUP-AT-EXIT calls the saved vector last, after the
\ registry. The saved vector is put back to EXIT-NONE before it is called: a
\ chained hook that registers a path re-arms the engine cell and then dies,
\ which re-enters CLEANUP-AT-EXIT, and it must find the chain empty there.
\
\ Registering arms the vector, not loading this file: a stripped application
\ restores no engine cell, so its vector is zero until its own CLEANUP+ fills
\ it, and a process that registers nothing leaves the vector as it found it.
\
\ EXIT-PREV is a typed xt cell holding a word that is always callable, not a
\ raw cell holding an xt or zero: the native compiler needs the convention of
\ the call, which a raw cell's value lacks (`ncomp: cannot compile EXIT-CHAIN
\ at execute`, E-NELAB-QUOT, when a stripped image's closure reaches this
\ word); and a cell that is never empty needs no zero test on an exit path,
\ where a stray zero would jump to address 0.
: EXIT-NONE ( -- ) ;

TYPED-VARIABLE EXIT-PREV [ -- ]

: EXIT-INIT ( -- )
   [: EXIT-NONE ;] EXIT-PREV ! ;
EXIT-INIT

: EXIT-CHAIN ( -- )
   EXIT-PREV @
   [: EXIT-NONE ;] EXIT-PREV !
   execute ;

: CLEANUP-AT-EXIT ( -- )
   [: CLEANUP-RUN ;] catch {: code :}
   code 0<> if code EXIT-REPORT then
   EXIT-CHAIN ;

\ The vector cell holds a code pointer or zero, so it has two accessors: the
\ number the zero and identity tests read, and the quotation slot a nonzero
\ vector is read and every arming written through. Only a private CAST: gives a
\ DATA address a quotation pointee.
CAST: >VECTOR ( n -- ptr [ -- ] )
CAST: XT>N ( [ -- ] -- n )

: EXIT-HOOK-N ( -- ptr n ) data-base EXIT-HOOK-CELL + ;

: EXIT-HOOK ( -- ptr [ -- ] )
   data-base BYTE-VIEW NULL-PTR BYTE-VIEW - EXIT-HOOK-CELL + >VECTOR ;

: ARM-EXIT-HOOK ( -- )
   EXIT-HOOK-N @ {: current:n :}
   current 0= if ['] CLEANUP-AT-EXIT EXIT-HOOK ! exit then
   current ['] CLEANUP-AT-EXIT XT>N = if exit then
   EXIT-HOOK @ EXIT-PREV !
   ['] CLEANUP-AT-EXIT EXIT-HOOK ! ;

\ REGISTRATION IS SAFE FROM ANY TASK although the table is the process's: it
\ runs under CLEANUP-LOCK, which a refusal releases too. The row grows
\ when it is full, the slot is written, and only then is it counted, so every
\ counted slot names a path and a refused registration - a path past the slot
\ (E-SPAN-CAPACITY), a row that cannot grow, a full process-cell table - leaves
\ the count it found.
: CLEANUP-APPEND ( ptr u8 n n -- ) {: a:ptr u kind :}
   DECLARE-CLEANUP
   CLEANUP-N @ {: idx :}
   idx CLEANUP-CAP @ = if CLEANUP-GROW then
   a u idx CLEANUP-SLOT SPAN:COPY
   u idx CLEANUP-AT !
   kind idx CLEANUP-KIND !
   idx 1+ CLEANUP-N ! ;

: CLEANUP-KIND+ ( ptr u8 n n -- ) {: a:ptr u kind :}
   ARM-EXIT-HOOK
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
\ Replace the file at path with the one fill writes at the path it is handed:
\ the sibling FS:RESERVE-SIBLING creates, its descriptor closed, renamed over
\ path once fill returns. Until then path holds what it held, so it names its
\ old file or the whole new one. The sibling is registered for removal at exit
\ while it is held, so a throw from fill or the rename, or a `die` in fill,
\ takes it with it; only a killed process leaves one behind. A refused
\ registration refuses the claim, and the sibling goes then. fill must not
\ reserve a sibling of its own (ATOMIC-WRITE-FILE does): FS:RESERVE-SIBLING's
\ path is this task's only until its next reservation.
\ A caller may pass context below path; fill must return that context unchanged
\ in shape. It can carry typed pointers and complete layout values directly.
: REPLACE-STAGED ( R ptr u8 n [ R ptr u8 n -- R ] -- R )
   {: path:ptr pathu fill :}
   path pathu RESERVE-SIBLING {: tmp:ptr tmpu fd :}
   \ Closed once, before fill opens the sibling by path; ATOMIC-CLOSE says why a
   \ failed close is not retried.
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
