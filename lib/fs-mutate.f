\ fs-mutate.f - checked filesystem mutation helpers.
\
\ STORAGE CLASS. TASK-LOCAL for the paths one call stages: the second NUL-padded
\ path FS-MUT-PATHZ2-BUF (RENAME-FILE's, MAKE-SYMLINK's and the stream copy's
\ destination), ATOMIC-WRITE-FILE's unique `.tmp-*` sibling
\ FS-MUT-ATOMIC-PATH, and
\ MAKE-TEMP-DIR's FS-MUT-TMP-PATH - the one this module RETURNS a span into -
\ are the FS-MUT-ABI band of the per-task DATA region, so two tasks renaming,
\ symlinking, writing atomically or making a temporary directory at once share
\ nothing and neither can replace the other's returned path. The descriptors,
\ cursors and lengths COPY-FILE-STREAM threads are LOCALS of the call.
\ CALLER-OWNED: the tree removal. REMOVE-TREE-IN walks the context the caller
\ supplies - a span of lib/fs.f's FS-WALK-BYTES - so two tasks remove two trees
\ at once by holding one each, and a WALK-FILES-IN callback removes a subtree
\ through a SECOND context while its walk stands. REMOVE-TREE is the
\ one-context form over the static FS-WALK-CTX0 and is therefore SINGLE-TASK,
\ and it shares that one context with WALK-FILES.
\ PROCESS-WIDE: the 8 KiB FS-MUT-COPY-BUF, so COPY-FILE and COPY-FILE-STREAM are
\ this module's one SINGLE-TASK pair (8192 bytes is more than the whole per-task
\ band could carry; a second task that must copy needs its own buffer); and the
\ cleanup registry, which is a PROCESS exit registry and not per-call scratch -
\ REGISTERING is safe from any task, because a registration claims its slot with
\ one atomic-add and writes only that slot, and the RUN belongs to the process.
\ See docs/threads.md.
\
\ Every buffer this module owns - the second NUL-padded path, the copy buffer,
\ the atomic and temp path builders and the cleanup path stack - is a span, so
\ the copies into them are bounded by the buffer's own reach (E-SPAN-CAPACITY,
\ E-SPAN-RANGE) and not by a hand-written FS-PATH-CAP comparison. What remains
\ hand-written is the path DOMAIN: a negative or empty path is E-FS-PATH and
\ fs.f's one FS-PATH-CAP length gate still runs on every path handed to a
\ syscall through FS-PATHZ / FS-MUT-PATHZ2.
\
require lib/errors.f
require lib/string.f
require lib/fmt.f
require lib/fs.f
require lib/fs-identity.f
require lib/span.f

$FFF constant FS-MUT-MODE-PERM
73 constant FS-MUT-MODE-EXEC
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
\ rows for lib/fs.f's reason: tools/native-build-core.f requires this file to
\ build the engine, so a `require lib/task.f` here would load the task runtime
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

: CHMOD-MODE ( ptr u8 n n -- ) {: a:ptr u mode :}
   a u FS-PATHZ mode chmod 0 < if E-FS-IO throw then ;

: CHMOD-X ( ptr u8 n -- ) {: a:ptr u :}
   a u STAT-MODE FS-MUT-MODE-PERM and FS-MUT-MODE-EXEC or {: mode :}
   a u mode CHMOD-MODE ;

: MAKE-SYMLINK ( ptr u8 n ptr u8 n -- ) {: target:ptr targetu link:ptr linku :}
   target targetu FS-PATHZ link linku FS-MUT-PATHZ2 symlink 0 < if E-FS-IO throw then ;

: MKDIR-MODE ( ptr u8 n n -- ) {: a:ptr u mode :}
   a u FS-PATHZ mode mkdir 0 < if E-FS-IO throw then ;

: MAKE-DIR ( ptr u8 n -- )
   FS-MUT-MODE-DIR MKDIR-MODE ;

: REMOVE-DIR ( ptr u8 n -- ) {: a:ptr u :}
   a u FS-PATHZ rmdir 0 < if E-FS-IO throw then ;

\ The removal walks the caller's context, so the error paths throw plainly:
\ REMOVE-TREE-IN closes whatever descriptors that context still holds, however
\ the traversal ended. REMOVE-FILE and REMOVE-DIR are exactly the unlink and
\ rmdir this loop needs, and the same E-FS-IO.
: FS-MUT-REMOVE-TREE-PATH ( ptr u8 ptr u8 n -- ) {: ctx:ptr a:ptr u :}
   a u SYMLINK? if a u REMOVE-FILE exit then
   a u EXISTS? 0= if exit then
   \ Only a directory is traversed; every other inode belongs to unlink.
   a u FS-TRY-LSTAT 0= if E-FS-STAT throw then
   FS-STAT-MODE@ S-IFMT and S-IFDIR <> if
      a u REMOVE-FILE exit
   then
   ctx a u FS-OPEN-WALK-DIR
   begin ctx FS-READ-DIR while
      ctx FS-DIR-BLOCK-BEGIN
      begin ctx FS-DIR-MORE? while
         ctx FS-LOAD-ENTRY FS-DIRENT-NAME 2dup FS-SKIP-SELF-ENTRY? if
            2drop
         else
            ctx a u FS-DESCEND-PATH RECURSE
            ctx FS-ASCEND-PATH
         then
         ctx FS-ADVANCE-ENTRY
      repeat
   repeat
   ctx FS-CLOSE-CUR-DIR
   a u REMOVE-DIR ;

\ Caught as a whole, the way lib/fs.f catches FS-WALK-RUN, so one site closes
\ the descriptors for every way out.
: FS-MUT-REMOVE-RUN ( ptr u8 ptr u8 n -- ptr u8 ptr u8 n ) {: ctx:ptr a:ptr u :}
   ctx a u FS-MUT-REMOVE-TREE-PATH
   ctx a u ;

\ The context is the caller's, so two tasks remove two trees at once by holding
\ one each. It does NOT claim FS-WALK-ACTIVE: the process exit hook removes
\ trees through CLEANUP-RUN whatever a task was doing, and a flag would refuse
\ it there. A removal through the context a live walk owns still clobbers that
\ walk - pass a second context, which is what these words are for.
: REMOVE-TREE-IN ( ptr u8 ptr u8 n -- ) {: ctx:ptr a:ptr u :}
   u 0 <= if E-FS-PATH throw then
   ctx FS-FDS-RESET
   0 ctx FS-WALK-DEPTH!
   ctx a u FS-WALK-ROOT!
   ctx  ctx FS-CUR-PATH u SPAN:TAKE SPAN:$  [: FS-MUT-REMOVE-RUN ;] catch {: code:n :}
   2drop drop
   ctx FS-CLOSE-WALK
   code 0<> if code throw then ;

: REMOVE-TREE ( ptr u8 n -- ) {: a:ptr u :}
   FS-WALK-CTX0 a u REMOVE-TREE-IN ;

: FS-MUT-MKDIR-ONE ( ptr u8 n -- ) {: a:ptr u :}
   a u FS-PATHZ FS-MUT-MODE-DIR mkdir {: rc :}
   rc 0 < if
      a u DIR? 0= if E-FS-IO throw then
   then ;

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
FUNCTION: TRUNCATE-CALL ftruncate ( n n -- n ) ;FUNCTION
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
   pathu 6 + seed FS-MUT-U-DIGITS + attempt FS-MUT-U-DIGITS +
   FS-PATH-CAP > if E-SPAN-CAPACITY throw then
   FS-MUT-ATOMIC-PATH {: dst :}
   path pathu dst SPAN:COPY
   s" .tmp-" dst pathu SPAN:SKIP SPAN:COPY
   seed dst pathu 5 + FS-MUT-ATOMIC-U+ {: off:n :}
   FS-MUT-DASH dst off SPAN:U8!
   attempt dst off 1+ FS-MUT-ATOMIC-U+ {: u:n :}
   dst u SPAN:TAKE SPAN:$ ;

package FS-ATOMIC
PROCESS-SYMBOLS
FUNCTION: OPEN-CALL open ( ptr u8 n n -- n ) 2 VARIADIC ;FUNCTION
FUNCTION: CLOSE-CALL close ( n -- n ) ;FUNCTION

: OPEN-FLAGS ( -- n )
   HB-TARGET-LINUX? if $C1 exit then
   HB-TARGET-MACOS? if $A01 exit then
   E-FS-OPEN throw ;

public

\ Habu's portable `open` flags do not include O_EXCL. This narrow libc call
\ selects the host flag while keeping exclusive creation in one syscall.
: OPEN-EXCLUSIVE ( ptr u8 -- n )
   OPEN-FLAGS FS-MODE-0644 OPEN-CALL ;

: CLOSE-FD ( n -- n )
   CLOSE-CALL ;
;package

: FS-MUT-ATOMIC-OPEN-CANDIDATE ( ptr u8 n -- n ) {: path:ptr pathu :}
   path pathu FS-PATHZ FS-ATOMIC:OPEN-EXCLUSIVE {: fd :}
   fd 0 >= if fd exit then
   path pathu FS-MUT-PATH-COLLISION? if -1 exit then
   E-FS-OPEN throw ;

: FS-MUT-ATOMIC-RESERVE ( ptr u8 n n -- ptr u8 n n ) {: path:ptr pathu seed :}
   0 begin dup FS-MUT-ATOMIC-RETRIES < while
      dup {: attempt:n :}
      path pathu seed attempt FS-MUT-BUILD-ATOMIC-TMP
      2dup FS-MUT-ATOMIC-OPEN-CANDIDATE {: fd:n :}
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

: FS-MUT-ATOMIC-WRITE-RUN ( n ptr u8 n -- n ptr u8 n ) {: fd src:ptr srcu :}
   fd src srcu FS-MUT-ATOMIC-WRITE-FD
   fd src srcu ;

: FS-MUT-ATOMIC-RENAME-RUN ( ptr u8 n ptr u8 n -- ptr u8 n ptr u8 n )
   {: temp:ptr tempu path:ptr pathu :}
   temp tempu path pathu RENAME-FILE
   temp tempu path pathu ;

: FS-MUT-ATOMIC-CLEAN-TEMP ( ptr u8 n -- ) {: temp:ptr tempu :}
   temp tempu FS-MUT-PATH-COLLISION? if temp tempu REMOVE-FILE then ;

: FS-MUT-ATOMIC-WRITE-SEED ( ptr u8 n ptr u8 n n -- ) {: path:ptr pathu src:ptr srcu seed :}
   srcu 0 < if E-FS-CAPACITY throw then
   path pathu seed FS-MUT-ATOMIC-RESERVE {: temp:ptr tempu fd :}
   fd src srcu [: FS-MUT-ATOMIC-WRITE-RUN ;] catch {: write-code:n :}
   2drop drop
   \ Close exactly once, before publish. A failed close may already have freed
   \ the descriptor, so retrying it could close an unrelated recycled fd.
   fd FS-ATOMIC:CLOSE-FD {: close-code:n :}
   write-code 0<> close-code 0<> or if
      temp tempu FS-MUT-ATOMIC-CLEAN-TEMP
      close-code 0<> if E-FS-IO throw then
      write-code throw
   then
   temp tempu path pathu [: FS-MUT-ATOMIC-RENAME-RUN ;] catch {: rename-code:n :}
   2drop 2drop
   rename-code 0<> if
      temp tempu FS-MUT-ATOMIC-CLEAN-TEMP
      rename-code throw
   then ;

: ATOMIC-WRITE-FILE ( ptr u8 n ptr u8 n -- ) {: path:ptr pathu src:ptr srcu :}
   path pathu src srcu mono-ns getpid xor FS-MUT-ATOMIC-WRITE-SEED ;

: FS-MUT-BUILD-TEMP-TRY ( ptr u8 n ptr u8 n n n -- ptr u8 n ) {: base:ptr baseu prefix:ptr prefixu seed attempt :}
   SB-RESET
   base baseu SB-APPEND
   FS-MUT-SLASH SB-APPEND-C
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

: MAKE-TEMP-DIR ( ptr u8 n ptr u8 n -- ptr u8 n )
   mono-ns FS-MUT-MAKE-TEMP-DIR-SEED ;

: TMPDIR-MKDIR ( ptr u8 n -- ptr u8 n ) {: prefix:ptr prefixu :}
   s" TMPDIR" GETENV dup 0= if 2drop s" /tmp" then
   prefix prefixu MAKE-TEMP-DIR ;

\ The one base resolver for a process that was handed a scratch root. HB_TMP
\ names a directory the caller owns and will remove; a temp tree made under it
\ therefore goes away with it, even for a process that is killed before it can
\ run its own CLEANUP-RUN. With no HB_TMP the tree falls back to TMPDIR (then
\ /tmp), which nobody reaps - so a maker that leaks there leaks for good.
: HB-TMP-MKDIR ( ptr u8 n -- ptr u8 n ) {: prefix:ptr prefixu :}
   s" HB_TMP" GETENV dup 0= if
      2drop prefix prefixu TMPDIR-MKDIR exit
   then
   prefix prefixu MAKE-TEMP-DIR ;

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
\ stripped MAIN that requires lib/fmt.f prints -2105 through both FMT:.INT and
\ SB-RESET FMT:SB-INT SB$. The builder is reset here because the process is
\ exiting and nothing that ran before this line reads it again.
: FS-MUT-EXIT-REPORT ( n -- ) {: code :}
   SB-RESET
   s" hb: cleanup at exit threw " SB-APPEND
   code FMT:SB-INT
   S\" \n" SB-APPEND
   2 SB$ write drop ;

\ This file's ONE raw boundary is FS-MUT-ARM-EXIT below: the exit vector is a
\ fixed engine DATA cell holding a code pointer, reached through `data-base` the
\ way src/habu/snap.f reads ENGINE-SNAP-XT-CELL. Arming is the only place an
\ opaque xt is handled; calling one back is ordinary checked code, because the
\ saved vector lands in a typed xt cell.
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

TRUSTED: FS-MUT-ARM-EXIT ( -- )
   data-base EXIT-HOOK-CELL + dup @
   dup 0= if drop ['] CLEANUP-AT-EXIT swap ! exit then
   dup ['] CLEANUP-AT-EXIT = if 2drop exit then
   FS-MUT-EXIT-PREV !
   ['] CLEANUP-AT-EXIT swap ! ;

\ REGISTRATION IS SAFE FROM ANY TASK although the table is the process's: the
\ slot is claimed with one atomic-add on the count - `atomic-add` is an engine
\ primitive (src/habu/prims.f), so this needs no lib/task.f - and the claimer
\ writes nothing but the slot it was handed. The count is what CLEANUP-RUN walks
\ down from, so a claim past the table's end is GIVEN BACK before the refusal;
\ otherwise a refused 65th path would leave a count no run could walk. The
\ claimed slot's length is zeroed before the copy, so a slot claimed by a
\ registration that then throws is empty rather than whatever the last round
\ left there, and CLEANUP-RUN steps over it.
: FS-MUT-CLEANUP+ ( ptr u8 n n -- ) {: a:ptr u kind :}
   FS-MUT-ARM-EXIT
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
