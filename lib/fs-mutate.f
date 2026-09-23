\ fs-mutate.f - checked filesystem mutation helpers.
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
64 constant FS-MUT-TMP-RETRIES
0 constant FS-MUT-CLEANUP-FILE
1 constant FS-MUT-CLEANUP-DIR
2 constant FS-MUT-CLEANUP-TREE
$2D constant FS-MUT-DASH
$2E constant FS-MUT-DOT
$2F constant FS-MUT-SLASH

FS-PATHZ-CAP SPAN-BUFFER: FS-MUT-PATHZ2-BUF
FS-MUT-COPY-CAP SPAN-BUFFER: FS-MUT-COPY-BUF
FS-PATH-CAP SPAN-BUFFER: FS-MUT-ATOMIC-PATH
FS-PATH-CAP SPAN-BUFFER: FS-MUT-TMP-PATH
FS-MUT-CLEANUP-MAX FS-PATH-CAP * SPAN-BUFFER: FS-MUT-CLEANUP-PATHS
create FS-MUT-CLEANUP-US FS-MUT-CLEANUP-MAX cells allot
create FS-MUT-CLEANUP-KINDS FS-MUT-CLEANUP-MAX cells allot

variable FS-MUT-COPY-IN
variable FS-MUT-COPY-OUT
variable FS-MUT-COPY-RD
variable FS-MUT-COPY-WR
variable FS-MUT-COPY-OFF
variable FS-MUT-CLEANUP-N

create FS-MUT-ATOMIC-SUFFIX
   FS-MUT-DOT c, 116 c, 109 c, 112 c,

: FS-MUT-PATHZ2 ( ptr u8 n -- ptr u8 )
   FS-MUT-PATHZ2-BUF FS-PATHZ-INTO ;

\ The destination is the caller's span, so the capacity comparison is gone: a
\ base plus suffix longer than the destination is refused by the copy itself.
: FS-MUT-CHECK-SUFFIX-LENS ( n n -- ) {: u su :}
   u 0 < if E-FS-PATH throw then
   su 0 < if E-FS-PATH throw then ;

: FS-MUT-SUFFIX-PATH ( ptr u8 n ptr u8 n SPAN:span<u8> -- n ) {: a:ptr u s:ptr su dst :}
   u su FS-MUT-CHECK-SUFFIX-LENS
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

: FS-MUT-REMOVE-FILE-WALK ( ptr u8 n -- ) {: a:ptr u :}
   a u FS-PATHZ unlink 0 < if E-FS-IO FS-THROW-WALK then ;

: FS-MUT-REMOVE-DIR-WALK ( ptr u8 n -- ) {: a:ptr u :}
   a u FS-PATHZ rmdir 0 < if E-FS-IO FS-THROW-WALK then ;

: FS-MUT-REMOVE-TREE-PATH ( ptr u8 n -- ) {: a:ptr u :}
   a u SYMLINK? if a u FS-MUT-REMOVE-FILE-WALK exit then
   a u EXISTS? 0= if exit then
   \ Only a directory is traversed; every other inode belongs to unlink.
   a u FS-TRY-LSTAT 0= if E-FS-STAT FS-THROW-WALK then
   FS-STAT-MODE@ S-IFMT and S-IFDIR <> if
      a u FS-MUT-REMOVE-FILE-WALK exit
   then
   a u FS-OPEN-WALK-DIR
   begin FS-READ-DIR while
      FS-DIR-BLOCK-BEGIN
      begin FS-DIR-MORE? while
         FS-LOAD-ENTRY
         FS-ENT @ FS-DIRENT-NAME 2dup FS-SKIP-SELF-ENTRY? if
            2drop
         else
            a u 2swap FS-DESCEND-PATH RECURSE
            FS-ASCEND-PATH
         then
         FS-ADVANCE-ENTRY
      repeat
   repeat
   FS-CLOSE-CUR-DIR
   a u FS-MUT-REMOVE-DIR-WALK ;

: REMOVE-TREE ( ptr u8 n -- ) {: a:ptr u :}
   u 0 <= if E-FS-PATH throw then
   FS-FDS-RESET
   0 FS-DEPTH !
   a u FS-WALK-ROOT!
   FS-CUR-PATH u SPAN:TAKE SPAN:$ FS-MUT-REMOVE-TREE-PATH ;

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

: FS-MUT-COPY-RESET ( -- )
   -1 FS-MUT-COPY-IN !
   -1 FS-MUT-COPY-OUT ! ;

: FS-MUT-CLOSE-COPY-FD ( ptr n -- ) {: p:ptr :}
   p @ dup 0 >= if close else drop then
   -1 p ! ;

: FS-MUT-COPY-THROW ( n -- )
   FS-MUT-COPY-IN FS-MUT-CLOSE-COPY-FD
   FS-MUT-COPY-OUT FS-MUT-CLOSE-COPY-FD
   throw ;

: FS-MUT-COPY-OPEN-SRC ( ptr u8 n -- ) {: src:ptr srcu :}
   src srcu FS-PATHZ open-rd FS-MUT-COPY-IN !
   FS-MUT-COPY-IN @ 0 < if E-FS-OPEN FS-MUT-COPY-THROW then ;

: FS-MUT-COPY-CHECK-DST ( ptr u8 n -- ) {: dst:ptr dstu :}
   dst dstu EXISTS? if
      dst dstu FILE? 0= if E-FS-OPEN FS-MUT-COPY-THROW then
   then ;

: FS-MUT-COPY-PREPARE-DST ( -- )
   FS-MUT-COPY-IN @ >FD FS-MUT-COPY-OUT @ >FD FS-COPY:PREPARE-DST ;

: FS-MUT-COPY-OPEN-DST ( ptr u8 n -- ) {: dst:ptr dstu :}
   dst dstu FS-MUT-COPY-CHECK-DST
   dst dstu FS-MUT-PATHZ2
   FS-O-WRONLY FS-O-CREAT or FS-MODE-0644 open FS-MUT-COPY-OUT !
   FS-MUT-COPY-OUT @ 0 < if E-FS-OPEN FS-MUT-COPY-THROW then
   \ Opening must not truncate until the descriptor identities differ.
   [: FS-MUT-COPY-PREPARE-DST ;] catch
   dup 0<> if FS-MUT-COPY-THROW else drop then ;

: FS-MUT-COPY-WRITE-CHUNK ( n -- ) {: u :}
   0 FS-MUT-COPY-OFF !
   begin FS-MUT-COPY-OFF @ u < while
      FS-MUT-COPY-OUT @
      FS-MUT-COPY-BUF FS-MUT-COPY-OFF @ SPAN:SKIP  u FS-MUT-COPY-OFF @ - SPAN:TAKE SPAN:$
      write FS-MUT-COPY-WR !
      FS-MUT-COPY-WR @ 0 <= if E-FS-IO FS-MUT-COPY-THROW then
      FS-MUT-COPY-WR @ u FS-MUT-COPY-OFF @ - > if E-FS-IO FS-MUT-COPY-THROW then
      FS-MUT-COPY-OFF @ FS-MUT-COPY-WR @ + FS-MUT-COPY-OFF !
   repeat ;

: COPY-FILE-STREAM ( ptr u8 n ptr u8 n -- ) {: src:ptr srcu dst:ptr dstu :}
   FS-MUT-COPY-RESET
   src srcu FS-MUT-COPY-OPEN-SRC
   dst dstu FS-MUT-COPY-OPEN-DST
   begin
      FS-MUT-COPY-IN @ FS-MUT-COPY-BUF SPAN:$ read FS-MUT-COPY-RD !
      FS-MUT-COPY-RD @ 0 < if E-FS-IO FS-MUT-COPY-THROW then
      FS-MUT-COPY-RD @ FS-MUT-COPY-CAP > if E-FS-IO FS-MUT-COPY-THROW then
      FS-MUT-COPY-RD @ 0 >
   while
      FS-MUT-COPY-RD @ FS-MUT-COPY-WRITE-CHUNK
   repeat
   FS-MUT-COPY-IN FS-MUT-CLOSE-COPY-FD
   FS-MUT-COPY-OUT FS-MUT-CLOSE-COPY-FD ;

: ATOMIC-WRITE-FILE ( ptr u8 n ptr u8 n -- ) {: path:ptr pathu src:ptr srcu :}
   path pathu FS-MUT-ATOMIC-SUFFIX 4 FS-MUT-ATOMIC-PATH FS-MUT-SUFFIX-PATH {: tempu :}
   FS-MUT-ATOMIC-PATH tempu SPAN:TAKE SPAN:$ {: temp:ptr tu :}
   temp tu src srcu WRITE-ALL
   temp tu path pathu RENAME-FILE ;

: FS-MUT-SB-U ( n -- ) {: n :}
   n 0 < if E-FS-PATH throw then
   n 10 >= if n 10 / RECURSE then
   n 10 mod 48 + SB-APPEND-C ;

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
   a u EXISTS? if 0 0= exit then
   a u SYMLINK? ;

: FS-MUT-MKDIR-CANDIDATE? ( ptr u8 n -- bool ) {: a:ptr u :}
   a u FS-PATHZ FS-MUT-MODE-PRIVATE-DIR mkdir {: rc :}
   rc 0= if 0 0= exit then
   a u FS-MUT-TMP-COLLISION? if 0 0= 0= exit then
   E-FS-IO throw ;

: FS-MUT-MAKE-TEMP-DIR-SEED ( ptr u8 n ptr u8 n n -- ptr u8 n ) {: base:ptr baseu prefix:ptr prefixu seed :}
   0 begin dup FS-MUT-TMP-RETRIES < while
      base baseu prefix prefixu seed over FS-MUT-BUILD-TEMP-TRY
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

: FS-MUT-CLEANUP+ ( ptr u8 n n -- ) {: a:ptr u kind :}
   FS-MUT-ARM-EXIT
   FS-MUT-CLEANUP-N @ FS-MUT-CLEANUP-MAX >= if E-FS-CAPACITY throw then
   u 0 < if E-FS-PATH throw then
   kind FS-MUT-CLEANUP-FILE <>
   kind FS-MUT-CLEANUP-DIR <> and
   kind FS-MUT-CLEANUP-TREE <> and if E-FS-IO throw then
   a u FS-MUT-CLEANUP-N @ FS-MUT-CLEANUP-SLOT SPAN:COPY
   u FS-MUT-CLEANUP-N @ FS-MUT-CLEANUP-U-PTR !
   kind FS-MUT-CLEANUP-N @ FS-MUT-CLEANUP-KIND-PTR !
   FS-MUT-CLEANUP-N @ 1 + FS-MUT-CLEANUP-N ! ;

: CLEANUP+ ( ptr u8 n -- )
   FS-MUT-CLEANUP-FILE FS-MUT-CLEANUP+ ;

: CLEANUP-DIR+ ( ptr u8 n -- )
   FS-MUT-CLEANUP-DIR FS-MUT-CLEANUP+ ;

: CLEANUP-TREE+ ( ptr u8 n -- )
   FS-MUT-CLEANUP-TREE FS-MUT-CLEANUP+ ;
