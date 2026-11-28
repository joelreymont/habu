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
: SUFFIX-PATH ( str str SPAN:span<u8> -- n ) {: path:str sfx:str dst :}
   CAUSE-CLEAR
   path STR:LEN 0 < if E-FS-PATH throw then
   sfx STR:LEN 0 < if E-FS-PATH throw then
   dst SPAN:LEN {: cap :}
   sfx STR:LEN cap path STR:LEN - > if E-FS-CAPACITY throw then
   path dst SPAN:COPY
   sfx dst path STR:LEN SPAN:SKIP SPAN:COPY
   path STR:LEN sfx STR:LEN + ;

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

\ Under the lock. A failed mapping leaves the row, its capacity and N as found.
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
: REMOVE-FILE ( str -- ) {: s:str :}
   s >PATHZ unlink 0 < if E-FS-IO throw then ;

\ A throw sets FS:ERRNO.
: RENAME-FILE ( str str -- ) {: src:str dst:str :}
   src >PATHZ dst >PATHZ2 rename 0 < if E-FS-IO throw then ;

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
: CHMOD-MODE ( str n -- ) {: s:str mode :}
   s >PATHZ mode chmod 0 < if E-FS-IO throw then ;

\ A throw sets FS:ERRNO.
: CHMOD-X ( str -- ) {: s:str :}
   s STAT-MODE MODE-PERM and MODE-EXEC or {: mode :}
   s mode CHMOD-MODE ;

\ A throw sets FS:ERRNO.
: MAKE-SYMLINK ( str str -- ) {: target:str link:str :}
   target >PATHZ link >PATHZ2 symlink 0 < if E-FS-IO throw then ;

\ A hard link; an existing LINK is E-FS-IO, not replaced. A throw sets FS:ERRNO.
: MAKE-LINK ( str str -- )
   {: target:str link:str :}
   target >PATHZ link >PATHZ2 LINK-CALL 0<> if E-FS-IO ERRNO-FAIL then ;

\ A throw sets FS:ERRNO.
: MKDIR-MODE ( str n -- ) {: s:str mode :}
   s >PATHZ mode mkdir 0 < if E-FS-IO throw then ;

\ A throw sets FS:ERRNO.
: MAKE-DIR ( str -- )
   MODE-DIR MKDIR-MODE ;

\ A throw sets FS:ERRNO.
: REMOVE-DIR ( str -- ) {: s:str :}
   s >PATHZ rmdir 0 < if E-FS-IO throw then ;

\ WALK-UNMAP closes whatever descriptors the walk's frames still hold, however
\ the traversal ended, so the error paths below throw plainly.

private
\ The entry's own mode, never a link target's, lands in STAT-BUF. A name
\ already gone (ENOENT) is false: a missing root, or an entry removed since its
\ directory was read. Any other failure is E-FS-STAT.
: LSTAT-AT? ( n str -- bool ) {: parent:n name:str :}
   parent name >PATHZ STAT-BUF AT-NOFOLLOW STAT-AT-CALL 0= if true exit then
   FFI:ERRNO ENOENT = if false exit then
   E-FS-STAT ERRNO-FAIL ;


: UNLINK-AT ( n str n -- ) {: parent:n name:str flags:n :}
   parent name >PATHZ flags UNLINK-AT-CALL 0= if exit then
   E-FS-IO ERRNO-FAIL ;


\ The directory opens one depth down, O_NOFOLLOW, so a link swapped in since
\ the stat is rejected, never followed. The name is staged before that depth's
\ frame is made: a child's name lies in its parent's dirent block, which
\ growing the frames moves.
: OPEN-AT ( ^n n str -- ) {: ctx:^ parent:n name:str :}
   name >PATHZ {: z:^ :}
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
: OWN-NAME ( ^n str -- str ) {: ctx:^ name:str :}
   ctx WALK-DEPTH@ 0 < if name exit then
   ctx CUR-ENTRY DIRENT-NAME ;

: REMOVE-AT ( ^n n str -- ) {: ctx:^ parent:n name:str :}
   parent name LSTAT-AT? 0= if exit then
   STAT-BUF LIBC-MODE@ S-IFMT and S-IFDIR <> if
      parent name 0 UNLINK-AT exit
   then
   ctx parent name OPEN-AT
   begin ctx READ-DIR while
      ctx DIR-BLOCK-BEGIN
      begin ctx DIR-MORE? while
         ctx LOAD-ENTRY DIRENT-NAME dup SKIP-SELF-ENTRY? if
            drop
         else
            ctx ctx FD@ rot RECURSE
         then
         ctx ADVANCE-ENTRY
      repeat
   repeat
   ctx CLOSE-CUR-DIR
   ctx ASCEND-PATH
   parent ctx name OWN-NAME AT-REMOVEDIR UNLINK-AT ;


\ The root's length to stat. lstat of `link/` resolves the link, so a root
\ whose name without trailing slashes is a symlink - `link/`, `lfile/`,
\ `dang/` - is stat'ed without them and unlinked however it is spelled. Any
\ other root is stat'ed as spelled: `plain/` on a regular file stays ENOTDIR.
\ A root of slashes only keeps one, the filesystem root.
: ROOT-LEN ( str -- n ) {: path:str :}
   path STR:LEN begin
      dup 1 > if dup 1 - path STR:DATA + c@ SLASH = else false then
   while 1 - repeat {: s :}
   s path STR:LEN < if path STR:DATA s >str SYMLINK? if s exit then then
   path STR:LEN ;

\ Caught as a whole, so one site closes the descriptors and releases the frames
\ for every way out, a root rejected by ROOT-LEN's stat included.
: REMOVE-RUN ( ^n str -- ^n str ) {: ctx:^ s:str :}
   ctx AT-CWD s STR:DATA s ROOT-LEN >str REMOVE-AT
   ctx s ;

\ No flag guards the context: it is this call's alone, so the process exit hook
\ removes trees through CLEANUP-RUN whatever a task was doing.
: REMOVE-SCOPE ( str ^u8 NUM:alloc-byte-len -- )
   drop CELL-VIEW {: s:str ctx:^ :}
   ctx WALK-MAP
   \ The depth names the directory being emptied: none until the root opens.
   -1 ctx WALK-DEPTH!
   ctx s [: REMOVE-RUN ;] catch {: code:n :}
   drop drop
   ctx WALK-UNMAP
   code 0<> if code throw then ;

public
\ Removes the tree at a, unlinking a symlink root even with a trailing slash; a
\ missing root is a no-op. Descriptors bound its depth. A throw sets FS:ERRNO.
: REMOVE-TREE ( str -- ) {: s:str :}
   s STR:LEN 0 <= if CAUSE-CLEAR E-FS-PATH throw then
   s WALK-HANDLE-BYTES MEM:BYTES-ALLOC-LEN [: REMOVE-SCOPE ;] MEM:WITH-BYTES ;

private
\ Only EEXIST can mean the folder is already there, so only it is checked
\ with a stat, which accepts a link to a directory. Any other failure throws at
\ once with mkdir's cause; a non-directory in the way throws with EEXIST.
: MKDIR-ONE ( str -- ) {: s:str :}
   s >PATHZ MODE-DIR mkdir 0 >= if exit then
   ERRNO EEXIST <> if E-FS-IO throw then
   s DIR? 0= if E-FS-IO EEXIST CAUSE-FAIL then ;

public
\ A throw sets FS:ERRNO.
: MAKE-DIRS ( str -- ) {: s:str :}
   s STR:LEN 0 <= if CAUSE-CLEAR E-FS-PATH throw then
   1 begin dup s STR:LEN < while
      s STR:DATA over + c@ SLASH = if
         dup 0 > if s STR:DATA over >str MKDIR-ONE then
      then
      1+
   repeat drop
   s MKDIR-ONE ;

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

: COPY-OPEN-SRC ( str -- n ) {: src:str :}
   src READ-PATHZ open-rd {: in :}
   in 0 < if E-FS-OPEN throw then
   in ;

\ Stack-preserving under `catch`: the two descriptors are the quotation's
\ declared window and no path of this body consumes them, so the caller may drop
\ them and read its own locals after the catch.
: COPY-PREPARE-DST ( n n -- n n )
   2dup >FD swap >FD swap PREPARE-DST ;

\ Runs before the source opens, so a rejected destination has nothing to close.
\ The answer lives in the PATHZ2 buffer, which opening the source leaves alone.
: COPY-DST-PATHZ ( str -- ^u8 ) {: dst:str :}
   dst EXISTS? if
      dst FILE? 0= if E-FS-OPEN throw then
   then
   dst >PATHZ2 ;

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
   in buf read {: rd :}
   rd 0 < if in out E-FS-IO COPY-THROW then
   rd COPY-CAP > if in out E-FS-IO COPY-THROW then
   rd ;

: COPY-PUMP ( SPAN:span<u8> n n -- ) {: buf in out :}
   begin buf in out COPY-READ-ONE dup 0 > while
      buf in out COPY-WRITE-CHUNK
   repeat drop ;

\ The WITH-BYTES body: the scope's bytes are this copy's buffer, mapped before
\ either file opens, so a failed mapping leaves nothing to close.
: COPY-SCOPE ( str str ^u8 NUM:alloc-byte-len -- )
   MEM:ALLOCATION>SPAN {: src:str dst:str buf :}
   dst COPY-DST-PATHZ {: dstz:^ :}
   src COPY-OPEN-SRC {: in :}
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

\ Writes value at off as exactly width digits, zero-padded, and answers the
\ offset past them.
: PADDED-U+ ( n SPAN:span<u8> n n -- n ) {: value:n dst off:n width:n :}
   value 0 < if E-FS-PATH throw then
   width 0= if
      value 0<> if E-FS-PATH throw then
      off exit
   then
   value 10 / dst off width 1- RECURSE {: next:n :}
   value 10 mod 48 + dst next SPAN:U8!
   next 1+ ;

: PATH-COLLISION? ( str -- bool ) {: s:str :}
   s EXISTS? if 0 0= exit then
   s SYMLINK? ;

\ A sibling's seed takes the digits of the largest nonnegative cell, its
\ attempt those of the last attempt.
-1 1 rshift U-DIGITS constant SEED-DIGITS
ATOMIC-RETRIES 1- U-DIGITS constant TRY-DIGITS

\ The length of every `.tmp-<seed>-<attempt>` name FS:RESERVE-SIBLING and
\ ATOMIC-WRITE-FILE give a sibling, so whether a directory has room for its
\ sibling never depends on the seed it drew.
6 SEED-DIGITS + TRY-DIGITS + constant SIBLING-LEN-MAX

: SIBLING-Z ( -- ^u8 )
   ATOMIC-PATH SPAN:$ STR:DATA ;

: ATOMIC-BASE-OFF ( str -- n )
   {: path:str :}
   path STR:LEN begin dup 0 > while
      dup 1 - path STR:DATA + c@ SLASH = if exit then
      1-
   repeat ;

\ `<prefix>.tmp-<seed>-<attempt>` in this task's ATOMIC-PATH, seed and
\ attempt zero-padded so the name is SIBLING-LEN-MAX bytes. The prefix is a
\ destination's directory with its slash, or empty for a name relative to
\ that directory's descriptor. A prefix that leaves no room within PATH-CAP
\ for the sibling name is E-FS-CAPACITY whatever the seed, a semantic
\ rejection.
: BUILD-SIBLING ( str n n -- str ) {: pre:str seed attempt :}
   CAUSE-CLEAR
   pre STR:LEN 0 < if E-FS-PATH throw then
   pre STR:LEN SIBLING-LEN-MAX + PATH-CAP > if E-FS-CAPACITY throw then
   ATOMIC-PATH {: dst :}
   pre dst SPAN:COPY
   s" .tmp-" dst pre STR:LEN SPAN:SKIP SPAN:COPY
   seed dst pre STR:LEN 5 + SEED-DIGITS PADDED-U+ {: off:n :}
   DASH dst off SPAN:U8!
   attempt dst off 1+ TRY-DIGITS PADDED-U+ {: u:n :}
   dst u SPAN:TAKE SPAN:$ ;

\ Habu's portable `open` flags do not include O_EXCL. This narrow libc call
\ takes the host's exclusive-create flags, so creation stays one syscall.
PROCESS-SYMBOLS
FUNCTION: OPEN-CALL open ( ^u8 n n -- i32 ) 2 VARIADIC ;FUNCTION

\ O_WRONLY|O_CREAT|O_EXCL for the host.
: EXCL-WRITE-FLAGS ( -- n )
   HB-TARGET-LINUX? if $C1 exit then
   HB-TARGET-MACOS? if $A01 exit then
   E-FS-OPEN throw ;

\ O_RDWR|O_CREAT|O_EXCL for the host.
: EXCL-RDWR-FLAGS ( -- n )
   HB-TARGET-LINUX? if $C2 exit then
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
: CREATE-NEW ( str n -- fd ) {: path:str mode :}
   path >PATHZ CREATE-NEW-FLAGS mode OPEN-CALL {: file:n :}
   file 0 < if E-FS-OPEN ERRNO-FAIL then
   file >FD ;

\ The existing file at the path, opened for reading and writing, close-on-exec.
\ A link as the last component is rejected, never followed: E-FS-OPEN with
\ FS:ERRNO the host's answer, ELOOP on Linux. Every failure is E-FS-OPEN with
\ its errno in FS:ERRNO.
: OPEN-RDWR-NOFOLLOW ( str -- fd ) {: path:str :}
   path >PATHZ RDWR-NOFOLLOW-FLAGS 0 OPEN-CALL {: file:n :}
   file 0 < if E-FS-OPEN ERRNO-FAIL then
   file >FD ;

private
\ Create and open one candidate with the exclusive flags and mode: its
\ descriptor, -1 when the name is taken, E-FS-OPEN with the host's cause for
\ any other rejection.
: OPEN-CANDIDATE ( str n n -- n )
   {: path:str flags mode :}
   path >PATHZ flags mode OPEN-CALL {: fd :}
   fd 0 >= if fd exit then
   FFI:ERRNO EEXIST = if -1 exit then
   E-FS-OPEN ERRNO-FAIL ;

: ATOMIC-WRITE-FD ( n str -- ) {: fd src:str :}
   0 begin dup src STR:LEN < while
      {: off:n :}
      fd src STR:DATA off + src STR:LEN off - >str write {: wr:n :}
      wr 0 <= if E-FS-IO throw then
      wr src STR:LEN off - > if E-FS-IO throw then
      off wr +
   repeat drop ;

public
\ A throw sets FS:ERRNO.
: ATOMIC-CLEAN-TEMP ( str -- ) {: temp:str :}
   temp PATH-COLLISION? if temp REMOVE-FILE then ;

private
\ The realpath of the directory holding path's last component, borrowed from
\ this task's PATHZ2 buffer.
: PARENT-REAL ( str -- str )
   {: path:str :}
   path ATOMIC-BASE-OFF {: off:n :}
   off 0= if s" ." >PATHZ else
      off 1 = if s" /" >PATHZ else
         path STR:DATA off 1 - >str >PATHZ
      then
   then {: parent:^ :}
   parent PATHZ2-BUF realpath {: u:n :}
   u -2 = if CAUSE-CLEAR E-FS-CAPACITY throw then
   u 0 < if E-FS-STAT ERRNO-FAIL then
   PATHZ2-BUF SPAN:$ STR:DATA u >str ;

public
\ Resolve the complete spelling when it exists, including its final symlink.
\ For a new basename, resolve its parent and append the basename unchanged.
\ The answer is absolute and borrowed from this task's PATHZ2 buffer until the
\ next use of that buffer; callers retaining it copy it.
\ This buffer survives sibling reservation and is used unchanged at rename.
: ATOMIC-TARGET ( str -- str )
   {: path:str :}
   path >PATHZ {: input:^ :}
   input PATHZ2-BUF realpath {: u:n :}
   u -2 = if CAUSE-CLEAR E-FS-CAPACITY throw then
   u 0 >= if PATHZ2-BUF SPAN:$ STR:DATA u >str exit then
   input path STR:LEN >str SYMLINK? if E-FS-STAT throw then
   input path STR:LEN >str ATOMIC-BASE-OFF {: off:n :}
   path STR:LEN off - 0= if CAUSE-CLEAR E-FS-STAT throw then
   input off + path STR:LEN off - >str ATOMIC-PATH SPAN:COPY
   input path STR:LEN >str PARENT-REAL
   ATOMIC-PATH SPAN:$ STR:DATA path STR:LEN off - >str
   PATHZ2-BUF PATH-CAP SPAN:TAKE JOIN-PATH-INTO {: targetu:n :}
   PATHZ2-BUF SPAN:$ STR:DATA targetu >str ;

private
: ATOMIC-OLD-MODE ( str -- n ) {: path:str :}
   path TRY-STAT if
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
: SAY ( str -- ) {: s:str :}
   2 s write drop ;

\ The code's declared name, else its signed number.
: SAY-CLASS ( n -- ) {: code :}
   code ERROR:NAME$ {: name:str :}
   name STR:LEN 0= if code FMT:INT$ else name then SAY
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
   {: op:str step:str code path:str name:str :}
   ERRNO {: e:n :}
   code SAY-CLASS
   path SAY  name SAY
   s" : " SAY  op SAY  s"  " SAY  step SAY
   s"  failed" SAY
   e SAY-END ;

\ Opened for search only: a directory the caller may write and search but not
\ list still opens.
: OPEN-DIR-SEARCH ( str -- n ) {: s:str :}
   AT-CWD s >PATHZ SEARCH-DIR-FLAGS OPEN-AT-CALL {: dir :}
   dir 0 < if E-FS-OPEN ERRNO-FAIL then
   dir ;

\ The absolute target's directory, for every call naming the sibling or the
\ target: what precedes its last slash, or the root.
: ATOMIC-OPEN-DIR ( str -- n ) {: dst:str :}
   dst ATOMIC-BASE-OFF 1- 1 max {: diru :}
   dst STR:DATA diru >str OPEN-DIR-SEARCH ;

\ Create the sibling in dir exclusively, its name staged for SIBLING-Z:
\ the name's length and the descriptor.
: ATOMIC-RESERVE-AT ( n n n -- n n )
   {: dir seed mode :}
   0 begin dup ATOMIC-RETRIES < while
      dup {: attempt:n :}
      s" " seed attempt BUILD-SIBLING {: name:str :}
      name STR:LEN ATOMIC-PATH FFI:CSTR! drop
      dir name STR:DATA EXCL-WRITE-FLAGS mode OPENAT-CALL {: fd:n :}
      fd 0 >= if drop name STR:LEN fd exit then
      FFI:ERRNO EEXIST <> if E-FS-OPEN ERRNO-FAIL then
      1+
   repeat drop
   E-FS-IO ERRNO-FAIL ;

\ Darwin's fcntl cmd, or fsync on a file system that rejects it and elsewhere.
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
: ATOMIC-FILL ( n str n -- n str n ) {: fd src:str mode :}
   fd src ATOMIC-WRITE-FD
   mode 0 >= if
      fd mode FCHMOD-CALL 0<> if E-FS-IO ERRNO-FAIL then
   then
   fd ATOMIC-SYNC drop
   fd src mode ;

\ Runs after the publish, hence E-FS-SYNC. Darwin syncs the search-only dir.
\ Linux opened it O_PATH, which fsync rejects (EBADF), so it syncs the dir
\ reopened to read; one the caller cannot list stays unsynced.
: ATOMIC-SYNC-DIR ( n -- n ) {: dir :}
   HB-TARGET-LINUX? if
      dir S\" .\z" STR:DATA OPEN-DIR-FLAGS OPEN-AT-CALL
   else dir then {: fd:n :}
   fd 0 < if
      FFI:ERRNO EACCES = if dir exit then
      E-FS-SYNC ERRNO-FAIL
   then
   fd [: ATOMIC-SYNC ;] catch {: code:n :}
   drop
   HB-TARGET-LINUX? if fd CLOSE-FD drop then
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

: SYNC-WALKED? ( str -- bool ) {: s:str :}
   s DIR? if s SYNC-ENTRY then
   true ;

\ The F_FULLFSYNC last flushes the drive's cache once for every fsync before.
: SYNC-EACH ( str -- ) {: s:str :}
   s [: SYNC-WALKED? ;] [: SYNC-ENTRY ;] WALK-FILES-IF
   s PARENT-REAL SYNC-ENTRY
   s [: ATOMIC-SYNC ;] SYNC-PATH ;

: SYNC-FS ( str -- )
   [: dup SYNCFS-CALL 0<> if E-FS-IO ERRNO-FAIL then ;] SYNC-PATH ;

public
\ On return directory path, every directory and regular file under it and its
\ name in its parent are on disk. A throw sets FS:ERRNO.
: SYNC-TREE ( str -- )
   HB-TARGET-MACOS? if SYNC-EACH else SYNC-FS then ;

\ Renames from over to and syncs to's directory, opened first: a throw before
\ the rename leaves both names; E-FS-SYNC is the sync's, after the rename.
: RENAME-SYNCED ( str str -- ) {: from:str to:str :}
   to PARENT-REAL OPEN-DIR-SEARCH {: dir :}
   [: from to RENAME-FILE dir ATOMIC-SYNC-DIR drop ;]
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
: ATOMIC-DISCARD ( n str str -- ) {: dir pre:str sib:str :}
   CAUSE-CLEAR
   [: dir sib STR:DATA 0 UNLINK-AT-CALL 0<> if E-FS-IO ERRNO-FAIL then ;]
   catch {: clean-code:n :}
   clean-code 0<> if
      s" atomic replacement" s" cleanup" clean-code
      pre sib STAGED-REPORT
   then ;

: ATOMIC-ABANDON ( n str n n -- ) {: dir pre:str nameu code :}
   ERRNO {: cause :}
   dir pre SIBLING-Z nameu >str ATOMIC-DISCARD
   code cause CAUSE-FAIL ;

\ Stack-preserving under `catch`; a throw after the publish leaves the new file.
\ Answers whether it published; a claim on a taken name unlinks the sibling.
: ATOMIC-IN-DIR ( n str str n n bool -- n str str n n bool )
   {: dir pre:str src:str mode seed claim?:bool :}
   mode 0 >= if MODE-PRIVATE-FILE else MODE-0644 then {: create-mode :}
   dir seed create-mode ATOMIC-RESERVE-AT {: nameu fd :}
   fd src mode [: ATOMIC-FILL ;] catch {: write-code:n :}
   2drop drop
   write-code 0<> if
      fd CLOSE-FD drop
      dir pre nameu write-code ATOMIC-ABANDON
   then
   fd [: ATOMIC-CLOSE ;] catch {: close-code:n :}
   drop
   close-code 0<> if dir pre nameu close-code ATOMIC-ABANDON then
   dir pre STR:DATA pre STR:LEN + claim?
   [: if 2dup SIBLING-Z swap RENAME-NOREPLACE else ATOMIC-PUBLISH true then ;]
   catch {: publish-code:n :}
   publish-code 0<> if dir pre nameu publish-code ATOMIC-ABANDON then
   {: published:bool :}
   2drop
   published if dir ATOMIC-SYNC-DIR drop else dir ATOMIC-UNLINK drop then
   dir pre src mode seed published ;

\ PATH-CAP bounds only dst: the sibling is named in dst's directory. Private is
\ 0600, else the old regular mode or umasked 0644; false if claimed and taken.
: ATOMIC-WRITE-SEED ( str str n bool bool -- bool )
   {: path:str src:str seed private?:bool claim?:bool :}
   src STR:LEN 0 < if CAUSE-CLEAR E-FS-CAPACITY throw then
   path ATOMIC-TARGET {: dst:str :}
   dst STR:LEN PATHZ2-BUF FFI:CSTR! drop
   claim? if dst TRY-LSTAT if false exit then then
   private? if MODE-PRIVATE-FILE else dst ATOMIC-OLD-MODE then {: mode:n :}
   dst ATOMIC-OPEN-DIR {: dir :}
   dir dst STR:DATA dst ATOMIC-BASE-OFF >str src mode seed claim?
   [: ATOMIC-IN-DIR ;] catch {: code:n :}
   ERRNO {: cause :}
   dir CLOSE-FD drop
   code 0<> if code cause CAUSE-FAIL then
   {: published:bool :}
   2drop 2drop drop
   published ;

: RESERVE-MODE ( str n -- str n ) {: path:str mode :}
   path STR:LEN 0 < if CAUSE-CLEAR E-FS-PATH throw then
   path STR:DATA path ATOMIC-BASE-OFF >str {: pre:str :}
   ATOMIC-SEED {: seed :}
   0 begin dup ATOMIC-RETRIES < while
      dup {: attempt:n :}
      pre seed attempt BUILD-SIBLING
      dup EXCL-WRITE-FLAGS mode OPEN-CANDIDATE {: fd:n :}
      fd 0 >= if nip fd exit then
      drop 1+
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
   {: path:str src:str :}
   path src ATOMIC-SEED false false ATOMIC-WRITE-SEED drop ;


\ ATOMIC-WRITE-FILE for a file only its owner may read: sibling and result are
\ 0600 whatever the destination held. A throw sets FS:ERRNO.
: ATOMIC-WRITE-PRIVATE ( str str -- )
   {: path:str src:str :}
   path src ATOMIC-SEED true false ATOMIC-WRITE-SEED drop ;


\ ATOMIC-WRITE-PRIVATE to a free name, else false, the entry untouched. A final
\ symlink counts as its target, a dangling one E-FS-STAT. A throw sets FS:ERRNO.
: ATOMIC-CREATE-PRIVATE ( str str -- bool )
   {: path:str src:str :}
   path src ATOMIC-SEED true true ATOMIC-WRITE-SEED ;

private
\ FS:ATOMIC-CREATE-FILES is a machine over groups of at most ATOMIC-GROUP
\ files. Each group goes FILL (each free name's sibling created, written and
\ hinted), SYNC (each sibling's data ordered before its rename, then closed
\ once) and PUBLISH (each sibling renamed to its name, or unlinked if the
\ name was taken since); after the last group, DIR-SYNC once if a group
\ renamed a name into the directory. A throw in
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
   HB-TARGET-LINUX? if
      fd 0 0 SFR-WRITE SYNC-RANGE-CALL drop
   then ;

\ A group's slot: the sibling's descriptor, -1 when none is open; the
\ sibling's name length, 0 when no sibling is left to publish or unlink;
\ then the sibling's and the target's names, each NUL-terminated.
SIBLING-LEN-MAX 1+ constant SIB-ROOM
2 cells SIB-ROOM + NAME-MAX 1+ + CELL 1- + CELL / cells constant SLOT-ROOM

\ The scope's bytes: the report prefix `<dir>/` in PREFIX-ROOM, then a cell
\ counting the names PUBLISH renamed, then slots.
: PREFIX-ROOM ( n -- n ) CELL + CELL / cells ;

: GROUP-SLOT ( ^u8 n -- ^n ) SLOT-ROOM * + CELL-VIEW ;
: SLOT-SIB-LEN ( ^n -- ^n ) CELL + ;
: SLOT-SIB ( ^n -- ^u8 ) BYTE-VIEW 2 cells + ;
: SLOT-NAME ( ^n -- ^u8 ) SLOT-SIB SIB-ROOM + ;

: SLOT-EMPTY ( ^n -- ) {: slot:^ :}
   -1 slot !
   0 slot SLOT-SIB-LEN ! ;

\ FILL for file idx: a taken name is skipped.
: SLOT-FILL ( R n ^n n [ R n -- R str str ] -- R )
   {: dir slot:^ idx body :}
   idx body execute {: name:str src:str :}
   name STR:LEN 1 < if CAUSE-CLEAR E-FS-PATH throw then
   name STR:LEN NAME-MAX > if CAUSE-CLEAR E-FS-CAPACITY throw then
   src STR:LEN 0 < if CAUSE-CLEAR E-FS-CAPACITY throw then
   dir name LSTAT-AT? if exit then
   name STR:DATA slot SLOT-NAME name STR:LEN BYTE-COPY
   0 slot SLOT-NAME name STR:LEN + c!
   dir ATOMIC-SEED MODE-0644 ATOMIC-RESERVE-AT {: sibu fd :}
   fd slot !
   SIBLING-Z slot SLOT-SIB sibu 1+ BYTE-COPY
   sibu slot SLOT-SIB-LEN !
   fd src ATOMIC-WRITE-FD
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

: SLOT-PUBLISH ( n ^n ^n -- ) {: dir pub:^ slot:^ :}
   slot SLOT-SIB-LEN @ 0= if exit then
   dir slot SLOT-SIB slot SLOT-NAME RENAME-NOREPLACE if
      1 pub +!
   else
      dir slot SLOT-SIB 0 UNLINK-AT-CALL 0<> if E-FS-IO ERRNO-FAIL then
   then
   0 slot SLOT-SIB-LEN ! ;

\ Stack-preserving under `catch`.
: CREATE-GROUP ( R n ^n ^u8 n n [ R n -- R str str ] -- R n ^n ^u8 n n [ R n -- R str str ] )
   {: dir pub:^ slots:^ first size body :}
   dir slots first size body CREATE-FILL
   size 0 ?do slots i GROUP-SLOT SLOT-SYNC loop
   size 0 ?do dir pub slots i GROUP-SLOT SLOT-PUBLISH loop
   dir pub slots first size body ;

: SLOT-ABANDON ( n str ^n -- ) {: dir pre:str slot:^ :}
   slot @ {: fd :}
   fd 0 >= if fd CLOSE-FD drop then
   slot SLOT-SIB-LEN @ {: sibu :}
   sibu 0 > if dir pre slot SLOT-SIB sibu >str ATOMIC-DISCARD then ;

\ ABANDON: rethrows code with the cause it found, after the group's cleanup.
: CREATE-ABANDON ( n str ^u8 n n -- )
   {: dir pre:str slots:^ size code :}
   ERRNO {: cause :}
   size 0 ?do dir pre slots i GROUP-SLOT SLOT-ABANDON loop
   code cause CAUSE-FAIL ;

: CREATE-SCOPE ( R n str n [ R n -- R str str ] ^u8 NUM:alloc-byte-len -- R )
   MEM:ALLOCATION>SPAN {: dir path:str count body buf :}
   path s" /" buf SUFFIX-PATH {: preu :}
   buf SPAN:$ STR:DATA {: pre:^ :}
   pre path STR:LEN PREFIX-ROOM + {: at:^ :}
   at CELL-VIEW {: pub:^ :}
   0 pub !
   at CELL + {: slots:^ :}
   count 0 ?do
      i  count i - ATOMIC-GROUP min {: first size :}
      size 0 ?do slots i GROUP-SLOT SLOT-EMPTY loop
      dir pub slots first size body [: CREATE-GROUP ;] catch {: code:n :}
      2drop 2drop 2drop
      code 0<> if dir pre preu >str slots size code CREATE-ABANDON then
   ATOMIC-GROUP +loop
   pub @ 0 > if dir ATOMIC-SYNC-DIR drop then ;

\ Stack-preserving under `catch`.
: CREATE-RUN ( R n str n [ R n -- R str str ] -- R n str n [ R n -- R str str ] )
   {: dir path:str count body :}
   dir path count body
   path STR:LEN PREFIX-ROOM CELL +  count ATOMIC-GROUP min SLOT-ROOM * +
   MEM:BYTES-ALLOC-LEN [: CREATE-SCOPE ;] MEM:WITH-BYTES
   dir path count body ;

public
\ Creates each free name in directory path with the bytes body answers for
\ file i, 0 <= i < count, umasked 0644; a taken name is left as it is. The
\ body's answers need last only until its next call. Each name is one
\ component: an empty one is E-FS-PATH, one over NAME_MAX bytes
\ E-FS-CAPACITY. Count 0 does nothing; a negative count is E-FS-CAPACITY.
\ On return every name it published is on disk as ATOMIC-SYNC-DIR says. A throw unlinks
\ its group's unpublished siblings, reporting on fd 2 one it cannot, and
\ leaves earlier groups' names whole: one in FILL or SYNC publishes none of
\ its group's names, one in PUBLISH keeps those it renamed; E-FS-SYNC leaves
\ every name in place. A throw sets FS:ERRNO.
: ATOMIC-CREATE-FILES ( R str n [ R n -- R str str ] -- R )
   {: path:str count body :}
   count 0 < if CAUSE-CLEAR E-FS-CAPACITY throw then
   count 0= if exit then
   path OPEN-DIR-SEARCH {: dir :}
   dir path count body [: CREATE-RUN ;] catch {: code:n :}
   ERRNO {: cause :}
   drop drop drop drop
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
: BUILD-TEMP-TRY ( str str n n -- str ) {: base:str prefix:str seed attempt :}
   base STR:LEN prefix STR:LEN TEMP-DIR-BASE-MAX > if
      CAUSE-CLEAR E-FS-CAPACITY throw
   then
   TMP-PATH-BUF {: dst :}
   base dst SPAN:COPY
   base JOIN-SEPARATED? if base STR:LEN else
      SLASH dst base STR:LEN SPAN:U8! base STR:LEN 1+
   then {: off:n :}
   prefix dst off SPAN:SKIP SPAN:COPY
   DASH dst off prefix STR:LEN + SPAN:U8!
   seed dst off prefix STR:LEN + 1+ ATOMIC-U+ {: next:n :}
   DASH dst next SPAN:U8!
   attempt dst next 1+ ATOMIC-U+ {: u:n :}
   dst u SPAN:TAKE SPAN:$ ;

: TMP-COLLISION? ( str -- bool ) {: s:str :}
   s PATH-COLLISION? ;

\ Whether mkdir made the candidate: false when the name is taken, a dangling
\ link included, so the next name is tried. Only EEXIST can mean a taken name,
\ so only it is probed; any other failure throws E-FS-IO at once with mkdir's
\ cause, and an EEXIST the probe finds no entry for throws with EEXIST.
: MKDIR-CANDIDATE? ( str -- bool ) {: s:str :}
   s >PATHZ MODE-PRIVATE-DIR mkdir 0= if 0 0= exit then
   ERRNO EEXIST <> if E-FS-IO throw then
   s TMP-COLLISION? if 0 0= 0= exit then
   E-FS-IO EEXIST CAUSE-FAIL ;

: MKTEMP-DIR-SEED ( str str n -- str ) {: base:str prefix:str seed :}
   0 begin dup TMP-RETRIES < while
      dup {: attempt:n :}
      base prefix seed attempt BUILD-TEMP-TRY
      dup MKDIR-CANDIDATE? if nip exit then
      drop 1+
   repeat drop
   E-FS-IO EEXIST CAUSE-FAIL ;

: MKTEMP-SEED ( str str n -- str n )
   {: base:str prefix:str seed :}
   0 begin dup TMP-RETRIES < while
      dup {: attempt:n :}
      base prefix seed attempt BUILD-TEMP-TRY
      dup EXCL-RDWR-FLAGS MODE-PRIVATE-FILE OPEN-CANDIDATE {: fd:n :}
      fd 0 >= if nip fd exit then
      drop 1+
   repeat drop
   E-FS-IO EEXIST CAUSE-FAIL ;

public
\ FS:MKTEMP-DIR-IN makes the directory under the base as given, and the answer
\ keeps the caller's spelling: a relative base gives a relative answer. The
\ answer stays this task's until its next FS:MKTEMP, FS:MKTEMP-DIR or
\ FS:MKTEMP-DIR-IN: a caller that makes two copies the first. A throw sets
\ FS:ERRNO: E-FS-IO carries mkdir's cause, EEXIST when every name it tried was
\ taken, and E-FS-CAPACITY for a base too long for a temp carries none.
: MKTEMP-DIR-IN ( str str -- str )
   mono-ns MKTEMP-DIR-SEED ;

private
\ realpath of a temp root into this task's PATHZ2 buffer: the canonical
\ spelling's length, below 0 when realpath rejects the root. FS:MKTEMP-DIR-IN
\ and FS:MKTEMP build and make their candidates through TMP-PATH-BUF and
\ >PATHZ only, so the spelling stands through every attempt.
: TEMP-REAL ( str -- n )
   >PATHZ PATHZ2-BUF realpath ;

\ One record naming TMPDIR, its value and why, before anything is made:
\ without it each rejection dies later on an error that does not name TMPDIR.
\ The value comes from the environment, so the record has no place.
: TMPDIR-OPEN ( str str str -- )
   {: s:str c:str why:str :}
   c REJECTION:OPEN  why REJECTION:EMIT-STR
   s" TMPDIR" REJECTION:NOTE  s REJECTION:EMIT-STR ;

\ Throws the code carrying the record, which prints only if nothing catches it:
\ ZIP:OPEN's callers catch FS:MKTEMP's E-FS-DIR and go on.
: TMPDIR-REJECT ( str n -- ) {: h:str code :}
   REJECTION:HELP  h REJECTION:EMIT-STR
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
: TMPDIR-CHECK ( str n -- str ) {: s:str prefixu :}
   s STR:LEN PATH-CAP > if s TMPDIR-LONG then
   s TMPDIR-DIR? 0= if
      s s" E-FS-DIR" s" TMPDIR is not a directory" TMPDIR-OPEN
      s" point TMPDIR at an existing directory, or unset it to use /tmp"
      E-FS-DIR TMPDIR-REJECT
   then
   s TEMP-REAL FFI:ERRNO {: real:n e:n :}
   \ -2: the canonical spelling and its NUL do not fit PATHZ-CAP.
   real -2 = if s TMPDIR-LONG then
   real 0 < if
      s s" E-FS-IO" s" TMPDIR has no canonical path" TMPDIR-OPEN
      s" errno" REJECTION:NOTE  e REJECTION:EMIT-U
      s" make every directory on TMPDIR's path searchable, or unset it to use /tmp"
      E-FS-IO TMPDIR-REJECT
   then
   real prefixu TEMP-DIR-BASE-MAX > if s TMPDIR-LONG then
   PATHZ2-BUF SPAN:$ STR:DATA real >str ;

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
   s" TMPDIR" GETENV dup STR:LEN 0= if drop s" /tmp" then
   prefixu TMPDIR-CHECK ;

public
: MKTEMP-DIR ( str -- str )
   {: prefix:str :}
   prefix STR:LEN TEMP-ROOT prefix MKTEMP-DIR-IN ;

\ Create and open a new file `<root>/<prefix>-<seed>-<attempt>` under the temp
\ root, read-write and mode 0600, as mkstemp(3) does. Answers its path, which
\ stays this task's until its next FS:MKTEMP, FS:MKTEMP-DIR or
\ FS:MKTEMP-DIR-IN, and the descriptor. The caller owns both and the removal;
\ nothing is registered.
: MKTEMP ( str -- str fd )
   {: prefix:str :}
   prefix STR:LEN TEMP-ROOT prefix mono-ns MKTEMP-SEED >FD ;

\ One task only: a forked child's inherited paths are its parent's to remove,
\ and a registration another task was in at the fork leaves the lock held.
: CLEANUP-RESET ( -- )
   0 CLEANUP-N !
   CLEANUP-RELEASE ;

private
: CLEANUP-REMOVE ( n -- ) {: idx :}
   idx CLEANUP-PATH {: path:str :}
   idx CLEANUP-KIND @ {: kind :}
   path SYMLINK? if path REMOVE-FILE exit then
   path EXISTS? 0= if exit then
   kind CLEANUP-TREE = if
      path REMOVE-TREE
   else
   kind CLEANUP-DIR = if
      path REMOVE-DIR
   else
      path REMOVE-FILE
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
\ runs under CLEANUP-LOCK, which a rejection releases too. The row grows
\ when it is full, the slot is written, and only then is it counted, so every
\ counted slot names a path and a rejected registration - a path past PATH-CAP
\ (E-FS-PATH) or a row that cannot grow - leaves the count it found.
: CLEANUP-APPEND ( str n -- ) {: s:str kind :}
   CLEANUP-N @ {: idx :}
   s STR:LEN PATH-CAP > if E-FS-PATH throw then
   idx CLEANUP-CAP @ = if CLEANUP-GROW then
   s idx CLEANUP-SLOT SPAN:COPY
   s STR:LEN idx CLEANUP-AT !
   kind idx CLEANUP-KIND !
   idx 1+ CLEANUP-N ! ;

: CLEANUP-KIND+ ( str n -- ) {: s:str kind :}
   ['] CLEANUP-AT-EXIT AT-EXIT:ADD
   s STR:LEN 0 < if E-FS-PATH throw then
   kind CLEANUP-FILE <>
   kind CLEANUP-DIR <> and
   kind CLEANUP-TREE <> and if E-FS-IO throw then
   CLEANUP-ACQUIRE
   s kind [: CLEANUP-APPEND ;] [: CLEANUP-RELEASE ;] finally ;

public
: CLEANUP+ ( str -- )
   CLEANUP-FILE CLEANUP-KIND+ ;

: CLEANUP-DIR+ ( str -- )
   CLEANUP-DIR CLEANUP-KIND+ ;

: CLEANUP-TREE+ ( str -- )
   CLEANUP-TREE CLEANUP-KIND+ ;

private
: CLEANUP-NAMES? ( str n -- bool ) {: s:str idx :}
   idx CLEANUP-AT @ s STR:LEN <> if 0 0= 0= exit then
   idx CLEANUP-PATH s STR:= ;

: CLEANUP-FIND ( str -- n ) {: s:str :}
   CLEANUP-N @ begin dup 0 > while
      1 -
      dup >r s r> CLEANUP-NAMES? if exit then
   repeat
   drop -1 ;

\ The slots above idx move down one in a single forward copy, which is safe
\ because the destination lies below the source.
: CLEANUP-DROP ( str -- ) {: s:str :}
   s CLEANUP-FIND {: idx :}
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
: CLEANUP-FORGET ( str -- ) {: s:str :}
   s STR:LEN 0 <= if E-FS-PATH throw then
   CLEANUP-ACQUIRE
   s [: CLEANUP-DROP ;] [: CLEANUP-RELEASE ;] finally ;

private
\ The sibling is umasked 0644, or 0600 over a regular file; once open for the
\ sync it takes that file's mode plus fill's execute bits, which may deny reads.
: STAGED-SYNC ( str n bool -- )
   {: tmp:str mode:n durable:bool :}
   mode 0 >= if
      tmp TRY-STAT 0= if E-FS-IO throw then
      STAT-MODE@ MODE-EXEC and mode or
   else -1 then {: kept:n :}
   tmp [:
      kept 0 >= if
         dup kept FCHMOD-CALL 0<> if E-FS-IO ERRNO-FAIL then
      then
      durable if ATOMIC-SYNC else ORDER-SYNC then ;] SYNC-PATH ;

\ Stack-preserving under `catch`, the sibling left to remove.
: STAGED-RUN ( R str str n bool [ R str -- R ] -- R str str n bool [ R str -- R ] )
   {: path:str tmp:str mode:n durable:bool fill :}
   tmp CLEANUP+
   tmp fill execute
   tmp mode durable STAGED-SYNC
   durable if
      tmp path RENAME-SYNCED
   else
      tmp path RENAME-FILE
   then
   path tmp mode durable fill ;

: STAGED-CLEAN ( str -- str )
   dup ATOMIC-CLEAN-TEMP ;

: STAGED-FORGET ( str -- str )
   dup CLEANUP-FORGET ;

: STAGED-REPLACE ( R str [ R str -- R ] bool -- R )
   {: path:str fill durable:bool :}
   path ATOMIC-OLD-MODE {: mode:n :}
   mode 0 >= if MODE-PRIVATE-FILE else MODE-0644 then {: create-mode :}
   path create-mode RESERVE-MODE {: tmp:str fd :}
   fd CLOSE-FD 0<> if
      FFI:ERRNO {: close-cause:n :}
      CAUSE-CLEAR
      tmp [: STAGED-CLEAN ;] catch {: clean-code:n :}
      drop
      clean-code 0<> if
         s" staged replacement" s" close cleanup" clean-code
         tmp s" " STAGED-REPORT
      then
      E-FS-IO close-cause CAUSE-FAIL
   then
   path tmp mode durable fill [: STAGED-RUN ;] catch
   {: code:n :}
   drop drop drop drop drop
   code 0<> if
      ERRNO {: cause:n :}
      CAUSE-CLEAR
      tmp [: STAGED-CLEAN ;] catch {: clean-code:n :}
      drop
      clean-code 0= if
         CAUSE-CLEAR
         tmp [: STAGED-FORGET ;] catch {: forget-code:n :}
         drop
         forget-code 0<> if
            s" staged replacement" s" cleanup registration" forget-code
            tmp s" " STAGED-REPORT
         then
      else
         s" staged replacement" s" cleanup" clean-code
         tmp s" " STAGED-REPORT
      then
      code cause CAUSE-FAIL
   then
   tmp CLEANUP-FORGET ;

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
