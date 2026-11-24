\ fs-mutate-test.f - focused tests for src/lib/fs-mutate.f.
\ Run: bin/hb --load src/lib/error.f src/lib/string.f src/lib/test.f src/lib/fs.f src/lib/fs-mutate.f src/lib/fs-mutate-test.f
\ Real special-inode fixtures require mkfifo and socat on PATH.

require src/lib/error.f
require src/lib/string.f
require src/lib/test.f
require src/lib/fs.f
require src/lib/span.f
require src/lib/fs-mutate.f
require src/lib/fmt.f                               \ the TMPDIR child's uncaught code
require src/lib/process-env.f
require src/lib/engine-candidate.f                  \ the TMPDIR child's engine
require src/lib/test/ending.f                      \ the TMPDIR child's outcome
require src/lib/task.f                              \ the two-task rows at the end
require src/lib/test/wait.f                         \ the workers' waits on each other
require src/lib/test/stderr.f                       \ the staged cleanup line

variable FMT-ROOT-U
variable FMT-REMOVE-U
variable FMT-RENAME-SRC-U
variable FMT-RENAME-DST-U
variable FMT-CHMOD-U
variable FMT-COPY-SRC-U
variable FMT-COPY-DST-U
variable FMT-STREAM-SRC-U
variable FMT-STREAM-DST-U
variable FMT-ATOMIC-U
variable FMT-MKDIR-U
variable FMT-NEST-A-U
variable FMT-NEST-B-U
variable FMT-NEST-C-U
variable FMT-TREE-U
variable FMT-TREE-CHILD-U
variable FMT-TREE-GRAND-U
variable FMT-TREE-DOTS-U
variable FMT-TREE-ROOT-FILE-U
variable FMT-TREE-LEAF-U
variable FMT-TREE-DOTS-FILE-U
variable FMT-LINK-TARGET-U
variable FMT-LINK-U
variable FMT-BROKEN-TARGET-U
variable FMT-BROKEN-LINK-U
variable FMT-LINK-DIR-U
variable FMT-LINK-DIR-FILE-U
variable FMT-TREE-DIR-LINK-U
variable FMT-COLLIDE-U
variable FMT-PARENT-A-U
variable FMT-PARENT-B-U
variable FMT-PARENT-LINK-U
variable FMT-PARENT-BOUND-U
variable FMT-STAGED-TMP-U
variable FMT-STAGED-FORGOT-U
variable FMT-SIBLING-MODE

create FMT-ROOT-BUF PATH-CAP allot
create FMT-REMOVE-BUF PATH-CAP allot
create FMT-RENAME-SRC-BUF PATH-CAP allot
create FMT-RENAME-DST-BUF PATH-CAP allot
create FMT-CHMOD-BUF PATH-CAP allot
create FMT-COPY-SRC-BUF PATH-CAP allot
create FMT-COPY-DST-BUF PATH-CAP allot
create FMT-STREAM-SRC-BUF PATH-CAP allot
create FMT-STREAM-DST-BUF PATH-CAP allot
create FMT-ATOMIC-BUF PATH-CAP allot

create FMT-LONG-BUF PATH-CAP 1 + allot

: FMT-LONG! ( -- )
   PATH-CAP 1 + 0 ?do 97 FMT-LONG-BUF i + c! loop ;

: FMT-LONG$ ( -- str )
   FMT-LONG-BUF PATH-CAP 1 + >str ;

: FMT-FULL$ ( -- str )
   FMT-LONG-BUF PATH-CAP >str ;

create FMT-MKDIR-BUF PATH-CAP allot
create FMT-NEST-A-BUF PATH-CAP allot
create FMT-NEST-B-BUF PATH-CAP allot
create FMT-NEST-C-BUF PATH-CAP allot
create FMT-TREE-BUF PATH-CAP allot
create FMT-TREE-CHILD-BUF PATH-CAP allot
create FMT-TREE-GRAND-BUF PATH-CAP allot
create FMT-TREE-DOTS-BUF PATH-CAP allot
create FMT-TREE-ROOT-FILE-BUF PATH-CAP allot
create FMT-TREE-LEAF-BUF PATH-CAP allot
create FMT-TREE-DOTS-FILE-BUF PATH-CAP allot
create FMT-LINK-TARGET-BUF PATH-CAP allot
create FMT-LINK-BUF PATH-CAP allot
create FMT-BROKEN-TARGET-BUF PATH-CAP allot
create FMT-BROKEN-LINK-BUF PATH-CAP allot
create FMT-LINK-DIR-BUF PATH-CAP allot
create FMT-LINK-DIR-FILE-BUF PATH-CAP allot
create FMT-TREE-DIR-LINK-BUF PATH-CAP allot
create FMT-COLLIDE-BUF PATH-CAP allot
create FMT-UNIQUE-BUF PATH-CAP allot
create FMT-PARENT-A-BUF PATH-CAP allot
create FMT-PARENT-B-BUF PATH-CAP allot
create FMT-PARENT-LINK-BUF PATH-CAP allot
create FMT-PARENT-BOUND-BUF PATH-CAP allot
create FMT-STAGED-TMP-BUF PATH-CAP allot
create FMT-STAGED-FORGOT-BUF PATH-CAP allot
create FMT-REAL-BUF FS:PATHZ-CAP allot
create FMT-ERR-PATH-BUF PATH-CAP allot
PATH-CAP 128 + constant FMT-ERR-CAP      \ a staged cleanup line: path and text
create FMT-ERR-BUF FMT-ERR-CAP allot

9217 constant FMT-STREAM-LEN
384 constant FMT-MODE-0600
416 constant FMT-MODE-0640
400 constant FMT-MODE-0620
292 constant FMT-MODE-0444
128 constant FMT-MODE-0200
$16D constant FMT-MODE-0555
13 constant FMT-EACCES                  \ a denied mkdir's errno on both targets
20 constant FMT-ENOTDIR                 \ ENOTDIR on both targets
$1000 constant FMT-S-IFIFO              \ S_IFIFO on both targets
$FFF constant FMT-MODE-PERM             \ st_mode's permission bits
$49 constant FMT-MODE-EXEC              \ 0111, execute for all
$1ED constant FMT-MODE-0755
create FMT-STREAM-SRC-DATA FMT-STREAM-LEN allot
create FMT-STREAM-DST-DATA FMT-STREAM-LEN allot

: FMT-COPY! ( str ^u8 ^n -- ) {: s:str dst:^ up:^ :}
   s STR:LEN PATH-CAP > if E-FS-PATH throw then
   s STR:DATA dst s STR:LEN BYTE-COPY
   s STR:LEN up ! ;

: FMT-ROOT$ ( -- str )
   FMT-ROOT-BUF FMT-ROOT-U @ >str ;

: FMT-REMOVE-PATH ( -- str )
   FMT-REMOVE-BUF FMT-REMOVE-U @ >str ;

: FMT-RENAME-SRC ( -- str )
   FMT-RENAME-SRC-BUF FMT-RENAME-SRC-U @ >str ;

: FMT-RENAME-DST ( -- str )
   FMT-RENAME-DST-BUF FMT-RENAME-DST-U @ >str ;

: FMT-CHMOD-PATH ( -- str )
   FMT-CHMOD-BUF FMT-CHMOD-U @ >str ;

: FMT-COPY-SRC ( -- str )
   FMT-COPY-SRC-BUF FMT-COPY-SRC-U @ >str ;

: FMT-COPY-DST ( -- str )
   FMT-COPY-DST-BUF FMT-COPY-DST-U @ >str ;

: FMT-STREAM-SRC ( -- str )
   FMT-STREAM-SRC-BUF FMT-STREAM-SRC-U @ >str ;

: FMT-STREAM-DST ( -- str )
   FMT-STREAM-DST-BUF FMT-STREAM-DST-U @ >str ;

: FMT-ATOMIC-PATH ( -- str )
   FMT-ATOMIC-BUF FMT-ATOMIC-U @ >str ;

: FMT-PARENT-A ( -- str )
   FMT-PARENT-A-BUF FMT-PARENT-A-U @ >str ;

: FMT-PARENT-B ( -- str )
   FMT-PARENT-B-BUF FMT-PARENT-B-U @ >str ;

: FMT-PARENT-LINK ( -- str )
   FMT-PARENT-LINK-BUF FMT-PARENT-LINK-U @ >str ;

: FMT-PARENT-BOUND ( -- str )
   FMT-PARENT-BOUND-BUF FMT-PARENT-BOUND-U @ >str ;

: FMT-MKDIR-PATH ( -- str )
   FMT-MKDIR-BUF FMT-MKDIR-U @ >str ;

: FMT-NEST-A ( -- str )
   FMT-NEST-A-BUF FMT-NEST-A-U @ >str ;

: FMT-NEST-B ( -- str )
   FMT-NEST-B-BUF FMT-NEST-B-U @ >str ;

: FMT-NEST-C ( -- str )
   FMT-NEST-C-BUF FMT-NEST-C-U @ >str ;

: FMT-TREE ( -- str )
   FMT-TREE-BUF FMT-TREE-U @ >str ;

: FMT-TREE-CHILD ( -- str )
   FMT-TREE-CHILD-BUF FMT-TREE-CHILD-U @ >str ;

: FMT-TREE-GRAND ( -- str )
   FMT-TREE-GRAND-BUF FMT-TREE-GRAND-U @ >str ;

: FMT-TREE-DOTS ( -- str )
   FMT-TREE-DOTS-BUF FMT-TREE-DOTS-U @ >str ;

: FMT-TREE-ROOT-FILE ( -- str )
   FMT-TREE-ROOT-FILE-BUF FMT-TREE-ROOT-FILE-U @ >str ;

: FMT-TREE-LEAF ( -- str )
   FMT-TREE-LEAF-BUF FMT-TREE-LEAF-U @ >str ;

: FMT-TREE-DOTS-FILE ( -- str )
   FMT-TREE-DOTS-FILE-BUF FMT-TREE-DOTS-FILE-U @ >str ;

: FMT-LINK-TARGET ( -- str )
   FMT-LINK-TARGET-BUF FMT-LINK-TARGET-U @ >str ;

: FMT-LINK ( -- str )
   FMT-LINK-BUF FMT-LINK-U @ >str ;

: FMT-BROKEN-TARGET ( -- str )
   FMT-BROKEN-TARGET-BUF FMT-BROKEN-TARGET-U @ >str ;

: FMT-BROKEN-LINK ( -- str )
   FMT-BROKEN-LINK-BUF FMT-BROKEN-LINK-U @ >str ;

: FMT-LINK-DIR ( -- str )
   FMT-LINK-DIR-BUF FMT-LINK-DIR-U @ >str ;

: FMT-LINK-DIR-FILE ( -- str )
   FMT-LINK-DIR-FILE-BUF FMT-LINK-DIR-FILE-U @ >str ;

: FMT-TREE-DIR-LINK ( -- str )
   FMT-TREE-DIR-LINK-BUF FMT-TREE-DIR-LINK-U @ >str ;

: FMT-COLLIDE ( -- str )
   FMT-COLLIDE-BUF FMT-COLLIDE-U @ >str ;

: FMT-TEMP-BASE ( -- str )
   FMT-ROOT$ ;

\ The root prefers HB_SOCK_TMP to TMPDIR: REMOVE-SPECIAL-TREE binds a Unix
\ socket at <root>/remove-tree/root.txt, sun_path holds 107 bytes on Linux and
\ 103 on macOS, and a pool slot's TMPDIR alone is 97 bytes (measured: the
\ listener never binds). HB_SOCK_TMP is the short directory the gate pool makes
\ per slot and removes when the slot retires or is killed (test/gate-pool.f
\ GT-POOL-CHILD-SOCK!). Without it the root goes under
\ TMPDIR, then /tmp. FMT-PREPARE registers the root as a tree, so a throw before
\ FMT-TEST-CLEANUP still removes it at exit.
: FMT-ROOT-COPY! ( str -- ) {: s:str :}
   s STR:DATA FMT-ROOT-BUF s STR:LEN BYTE-COPY
   s STR:LEN FMT-ROOT-U ! ;

: FMT-ROOT! ( -- )
   s" HB_SOCK_TMP" GETENV dup STR:LEN 0 > if FMT-ROOT-COPY! exit then
   drop
   s" habu-fs-mutate" FS:MKTEMP-DIR FMT-ROOT-COPY! ;

: FMT-REMOVE! ( -- )
   FMT-ROOT$ s" remove.txt" FMT-REMOVE-BUF FS:JOIN-PATH FMT-REMOVE-U ! ;

: FMT-RENAME-SRC! ( -- )
   FMT-ROOT$ s" rename-src.txt" FMT-RENAME-SRC-BUF FS:JOIN-PATH FMT-RENAME-SRC-U ! ;

: FMT-RENAME-DST! ( -- )
   FMT-ROOT$ s" rename-dst.txt" FMT-RENAME-DST-BUF FS:JOIN-PATH FMT-RENAME-DST-U ! ;

: FMT-CHMOD! ( -- )
   FMT-ROOT$ s" chmod.txt" FMT-CHMOD-BUF FS:JOIN-PATH FMT-CHMOD-U ! ;

: FMT-COPY-SRC! ( -- )
   FMT-ROOT$ s" copy-src.txt" FMT-COPY-SRC-BUF FS:JOIN-PATH FMT-COPY-SRC-U ! ;

: FMT-COPY-DST! ( -- )
   FMT-ROOT$ s" copy-dst.txt" FMT-COPY-DST-BUF FS:JOIN-PATH FMT-COPY-DST-U ! ;

: FMT-STREAM-SRC! ( -- )
   FMT-ROOT$ s" stream-src.bin" FMT-STREAM-SRC-BUF FS:JOIN-PATH FMT-STREAM-SRC-U ! ;

: FMT-STREAM-DST! ( -- )
   FMT-ROOT$ s" stream-dst.bin" FMT-STREAM-DST-BUF FS:JOIN-PATH FMT-STREAM-DST-U ! ;

: FMT-ATOMIC! ( -- )
   FMT-ROOT$ s" atomic.txt" FMT-ATOMIC-BUF FS:JOIN-PATH FMT-ATOMIC-U ! ;

: FMT-PARENT-PATHS! ( -- )
   FMT-ROOT$ s" parent-a" FMT-PARENT-A-BUF FS:JOIN-PATH FMT-PARENT-A-U !
   FMT-ROOT$ s" parent-b" FMT-PARENT-B-BUF FS:JOIN-PATH FMT-PARENT-B-U !
   FMT-ROOT$ s" parent-link" FMT-PARENT-LINK-BUF FS:JOIN-PATH FMT-PARENT-LINK-U ! ;

: FMT-MKDIR! ( -- )
   FMT-ROOT$ s" mkdir-one" FMT-MKDIR-BUF FS:JOIN-PATH FMT-MKDIR-U ! ;

: FMT-NEST-A! ( -- )
   FMT-ROOT$ s" nested" FMT-NEST-A-BUF FS:JOIN-PATH FMT-NEST-A-U ! ;

: FMT-NEST-B! ( -- )
   FMT-NEST-A s" child" FMT-NEST-B-BUF FS:JOIN-PATH FMT-NEST-B-U ! ;

: FMT-NEST-C! ( -- )
   FMT-NEST-B s" grand" FMT-NEST-C-BUF FS:JOIN-PATH FMT-NEST-C-U ! ;

: FMT-TREE! ( -- )
   FMT-ROOT$ s" remove-tree" FMT-TREE-BUF FS:JOIN-PATH FMT-TREE-U ! ;

: FMT-TREE-CHILD! ( -- )
   FMT-TREE s" child" FMT-TREE-CHILD-BUF FS:JOIN-PATH FMT-TREE-CHILD-U ! ;

: FMT-TREE-GRAND! ( -- )
   FMT-TREE-CHILD s" grand" FMT-TREE-GRAND-BUF FS:JOIN-PATH FMT-TREE-GRAND-U ! ;

: FMT-TREE-DOTS! ( -- )
   FMT-TREE s" .dots" FMT-TREE-DOTS-BUF FS:JOIN-PATH FMT-TREE-DOTS-U ! ;

: FMT-TREE-ROOT-FILE! ( -- )
   FMT-TREE s" root.txt" FMT-TREE-ROOT-FILE-BUF FS:JOIN-PATH FMT-TREE-ROOT-FILE-U ! ;

: FMT-TREE-LEAF! ( -- )
   FMT-TREE-GRAND s" leaf.txt" FMT-TREE-LEAF-BUF FS:JOIN-PATH FMT-TREE-LEAF-U ! ;

: FMT-TREE-DOTS-FILE! ( -- )
   FMT-TREE-DOTS s" kept-by-walk.txt" FMT-TREE-DOTS-FILE-BUF FS:JOIN-PATH FMT-TREE-DOTS-FILE-U ! ;

: FMT-LINK-TARGET! ( -- )
   FMT-ROOT$ s" link-target.txt" FMT-LINK-TARGET-BUF FS:JOIN-PATH FMT-LINK-TARGET-U ! ;

: FMT-LINK! ( -- )
   FMT-ROOT$ s" link.txt" FMT-LINK-BUF FS:JOIN-PATH FMT-LINK-U ! ;

: FMT-BROKEN-TARGET! ( -- )
   FMT-ROOT$ s" no-such-link-target.txt" FMT-BROKEN-TARGET-BUF FS:JOIN-PATH FMT-BROKEN-TARGET-U ! ;

: FMT-BROKEN-LINK! ( -- )
   FMT-ROOT$ s" broken-link.txt" FMT-BROKEN-LINK-BUF FS:JOIN-PATH FMT-BROKEN-LINK-U ! ;

: FMT-LINK-DIR! ( -- )
   FMT-ROOT$ s" link-dir-target" FMT-LINK-DIR-BUF FS:JOIN-PATH FMT-LINK-DIR-U ! ;

: FMT-LINK-DIR-FILE! ( -- )
   FMT-LINK-DIR s" kept.txt" FMT-LINK-DIR-FILE-BUF FS:JOIN-PATH FMT-LINK-DIR-FILE-U ! ;

: FMT-TREE-DIR-LINK! ( -- )
   FMT-TREE s" dir-link" FMT-TREE-DIR-LINK-BUF FS:JOIN-PATH FMT-TREE-DIR-LINK-U ! ;

: FMT-PATHS! ( -- )
   FMT-ROOT!
   FMT-REMOVE!
   FMT-RENAME-SRC!
   FMT-RENAME-DST!
   FMT-CHMOD!
   FMT-COPY-SRC!
   FMT-COPY-DST!
   FMT-STREAM-SRC!
   FMT-STREAM-DST!
   FMT-ATOMIC!
   FMT-PARENT-PATHS!
   FMT-MKDIR!
   FMT-NEST-A!
   FMT-NEST-B!
   FMT-NEST-C!
   FMT-TREE!
   FMT-TREE-CHILD!
   FMT-TREE-GRAND!
   FMT-TREE-DOTS!
   FMT-TREE-ROOT-FILE!
   FMT-TREE-LEAF!
   FMT-TREE-DOTS-FILE!
   FMT-LINK-TARGET!
   FMT-LINK!
   FMT-BROKEN-TARGET!
   FMT-BROKEN-LINK!
   FMT-LINK-DIR!
   FMT-LINK-DIR-FILE!
   FMT-TREE-DIR-LINK! ;

: FMT-WRITE-FIXTURES ( -- )
   FMT-REMOVE-PATH s" delete-me" FS:WRITE-ALL
   FMT-RENAME-SRC s" rename-me" FS:WRITE-ALL
   FMT-CHMOD-PATH s" #!/bin/sh\nexit 0\n" FS:WRITE-ALL
   FMT-COPY-SRC s" copy-src" FS:WRITE-ALL ;

: FMT-WRITE-TREE ( -- )
   FMT-TREE-GRAND FS:MAKE-DIRS
   FMT-TREE-DOTS FS:MAKE-DIRS
   FMT-TREE-ROOT-FILE s" root" FS:WRITE-ALL
   FMT-TREE-LEAF s" leaf" FS:WRITE-ALL
   FMT-TREE-DOTS-FILE s" dot" FS:WRITE-ALL ;

: FMT-PREPARE ( -- )
   FS:CLEANUP-RESET
   FMT-LONG!
   FMT-PATHS!
   FMT-ROOT$ FS:CLEANUP-TREE+
   FMT-WRITE-FIXTURES ;

: FMT-REMOVE-MISSING ( -- )
   s" no-such-habu-fs-remove-file" FS:REMOVE-FILE ;

: FMT-RENAME-MISSING ( -- )
   s" no-such-habu-fs-rename-file" FMT-RENAME-DST FS:RENAME-FILE ;

: FMT-CHMOD-MISSING ( -- )
   s" no-such-habu-fs-chmod-file" FS:CHMOD-X ;

: FMT-CHMOD-MODE-MISSING ( -- )
   s" no-such-habu-fs-chmod-mode-file" FS:MODE-PRIVATE-DIR FS:CHMOD-MODE ;

: FMT-RMDIR-MISSING ( -- )
   s" no-such-habu-fs-rmdir" FS:REMOVE-DIR ;

: FMT-MKDIR-EXISTS ( -- )
   FMT-MKDIR-PATH FS:MAKE-DIR ;

: FMT-REMOVE-TREE-EMPTY ( -- )
   s" " FS:REMOVE-TREE ;

create FMT-READ-BUF 64 allot

: FMT-CLEANUP-PATH-TOO-LONG ( -- )
   FMT-LONG$ FS:CLEANUP+ ;

package FS-TEST
public
\ A relative destination of PATH-CAP bytes, whose absolute spelling under
\ the working directory cannot fit PATH-CAP.
: ATOMIC-TARGET-NO-ROOM ( -- )
   FMT-FULL$ s" x" FS:ATOMIC-WRITE-FILE ;
;package

: FMT-REAL ( str -- str )
   FS:>PATHZ FMT-REAL-BUF FS:PATHZ-CAP realpath {: u:n :}
   u 0 > TTRUE
   FMT-REAL-BUF u >str ;


\ A real chain whose canonical spelling is n bytes, for a test whose subject is
\ that length. Its first name is registered for cleanup.
: FMT-DEEP-DIR ( n -- str )
   {: n:n :}
   FMT-ROOT$ FMT-REAL STR:LEN {: r:n :}
   n r ?do
      i r - 201 mod 0= i n 1- < and if FS:SLASH else 97 then
      FMT-REAL-BUF i + c!
   loop
   FMT-REAL-BUF n >str FS:MAKE-DIRS
   FMT-REAL-BUF r 201 + >str FS:CLEANUP-TREE+
   FMT-REAL-BUF n >str ;


\ A base one byte past the longest FS:MKTEMP-DIR-IN builds in for its prefix,
\ whatever seed it would draw.
: FMT-TEMP-BASE-NO-ROOM ( -- )
   FMT-LONG-BUF s" nr" STR:LEN FS:TEMP-DIR-BASE-MAX 1+ >str s" nr" FS:MKTEMP-DIR-IN drop ;

: FMT-SUFFIX-NO-ROOM ( -- )
   s" ab" s" .c" FMT-READ-BUF 3 SPAN:MAKE FS:SUFFIX-PATH drop ;

: FMT-MAKE-SYMLINK-EXISTS ( -- )
   FMT-LINK-TARGET FMT-LINK FS:MAKE-SYMLINK ;

package FS-TEST
: MAKE-LINK-EXISTS ( -- )
   FMT-LINK-TARGET FMT-LINK FS:MAKE-LINK ;

public
: HARD-LINK ( -- )
   s" a hard link names its target's file" T-LABEL
   FMT-LINK-TARGET s" link-target" FS:WRITE-ALL
   FMT-LINK-TARGET FMT-LINK FS:MAKE-LINK
   FMT-LINK FS:SYMLINK? TFALSE
   FMT-LINK FS:PATH-IDENTITY FMT-LINK-TARGET FS:PATH-IDENTITY
   {: link-dev:n link-ino:n dev:n ino:n :}
   link-dev dev T=
   link-ino ino T=
   s" an existing hard-link name is refused" T-LABEL
   [: MAKE-LINK-EXISTS ;] E-FS-IO TTHROWSQ
   FMT-LINK FS:REMOVE-FILE
   FMT-LINK-TARGET FMT-READ-BUF 64 FS:READ-ALL 11 T=
   FMT-READ-BUF 11 >str s" link-target" T$=
   FMT-LINK-TARGET FS:REMOVE-FILE ;
;package

package FS-TEST
PROCESS-SYMBOLS
FUNCTION: UMASK-CALL umask ( u32 -- u32 ) ;FUNCTION
1 constant F-GETFD
3 constant F-GETFL
3 constant ACCESS-MODE                  \ O_ACCMODE on both targets
$192 constant MODE-0622
create LOCK-PATH-BUF PATH-CAP allot
create LOCK-LINK-BUF PATH-CAP allot
variable CREATED-FD

: LOCK-PATH$ ( str -- str )
   FMT-ROOT$ swap LOCK-PATH-BUF FS:JOIN-PATH LOCK-PATH-BUF swap >str ;

: LOCK-LINK$ ( str -- str )
   FMT-ROOT$ swap LOCK-LINK-BUF FS:JOIN-PATH LOCK-LINK-BUF swap >str ;

: CLOEXEC? ( fd -- bool )
   FD>N F-GETFD 0 fcntl FD:FD-CLOEXEC and FD:FD-CLOEXEC = ;

: ACCESS ( fd -- n )
   FD>N F-GETFL 0 fcntl ACCESS-MODE and ;

\ The new file's descriptor and mode under umask 0, which keeps the group and
\ other write bits a default umask clears; the umask is restored first.
: CREATED-MODE ( -- fd n )
   0 UMASK-CALL {: saved:u32 :}
   [: s" lock.new" LOCK-PATH$ MODE-0622 FS:CREATE-NEW FD>N CREATED-FD ! ;]
   catch {: code:n :}
   saved UMASK-CALL drop
   code 0<> if code throw then
   CREATED-FD @ >FD
   s" lock.new" LOCK-PATH$ FS:STAT-MODE FMT-MODE-PERM and ;

: CREATE-PRESENT ( -- )
   s" lock.new" LOCK-PATH$ FMT-MODE-0600 FS:CREATE-NEW FD>N close ;

: CREATE-DANGLING ( -- )
   s" lock.dangling" LOCK-LINK$ FMT-MODE-0600 FS:CREATE-NEW FD>N close ;

: OPEN-LINK ( -- )
   s" lock.link" LOCK-LINK$ FS:OPEN-RDWR-NOFOLLOW FD>N close ;

: OPEN-MISSING ( -- )
   s" lock.missing" LOCK-PATH$ FS:OPEN-RDWR-NOFOLLOW FD>N close ;

\ Run the quotation, which must throw E-FS-OPEN with cause in FS:ERRNO.
: OPEN-REFUSED ( [ -- ] n -- ) {: cause:n :}
   catch FS:ERRNO {: code:n got:n :}
   code E-FS-OPEN T=
   got cause T= ;

public
\ FS:CREATE-NEW makes a write-only, close-on-exec file at the mode asked, and
\ refuses a present file and a dangling link alike with EEXIST.
: CREATE-NEW ( -- )
   s" a new file takes its mode and is write-only, close-on-exec" T-LABEL
   CREATED-MODE {: file:fd mode:n :}
   mode MODE-0622 T=
   file ACCESS FS:O-WRONLY T=
   file CLOEXEC? TTRUE
   file FD>N close
   s" an exclusive create of a present file is EEXIST" T-LABEL
   [: CREATE-PRESENT ;] FS:EEXIST OPEN-REFUSED
   s" lock.absent" LOCK-PATH$ s" lock.dangling" LOCK-LINK$ FS:MAKE-SYMLINK
   s" an exclusive create of a dangling link is EEXIST" T-LABEL
   [: CREATE-DANGLING ;] FS:EEXIST OPEN-REFUSED
   s" lock.absent" LOCK-PATH$ FS:EXISTS? TFALSE
   T-LABEL-CLEAR
   s" lock.dangling" LOCK-LINK$ FS:REMOVE-FILE
   s" lock.new" LOCK-PATH$ FS:REMOVE-FILE ;

\ FS:OPEN-RDWR-NOFOLLOW opens a present file read-write and close-on-exec,
\ refuses a link to it with ELOOP and a missing path with ENOENT.
: OPEN-RDWR-NOFOLLOW ( -- )
   s" lock.file" LOCK-PATH$ s" x" FS:WRITE-ALL
   s" a no-follow open of a file is read-write, close-on-exec" T-LABEL
   s" lock.file" LOCK-PATH$ FS:OPEN-RDWR-NOFOLLOW {: file:fd :}
   file ACCESS FS:O-RDWR T=
   file CLOEXEC? TTRUE
   file FD>N close
   s" lock.file" LOCK-PATH$ s" lock.link" LOCK-LINK$ FS:MAKE-SYMLINK
   s" a no-follow open of a link is ELOOP" T-LABEL
   [: OPEN-LINK ;] FS:ELOOP OPEN-REFUSED
   s" a no-follow open of a missing path is ENOENT" T-LABEL
   [: OPEN-MISSING ;] FS:ENOENT OPEN-REFUSED
   T-LABEL-CLEAR
   s" lock.link" LOCK-LINK$ FS:REMOVE-FILE
   s" lock.file" LOCK-PATH$ FS:REMOVE-FILE ;
;package

: FMT-TEST-COPY ( -- )
   FMT-COPY-SRC FMT-COPY-DST FS:COPY-FILE-STREAM
   FMT-COPY-DST FS:FILE? TTRUE
   FMT-COPY-DST FMT-READ-BUF 64 FS:READ-ALL 8 T=
   FMT-READ-BUF 8 >str s" copy-src" T$= ;

\ The copy destination followed by a NUL byte.
: FMT-NUL-DST$ ( -- str )
   FMT-COPY-DST {: dst:str :}
   dst STR:DATA FMT-LONG-BUF dst STR:LEN BYTE-COPY
   0 FMT-LONG-BUF dst STR:LEN + c!
   FMT-LONG-BUF BYTE-VIEW dst STR:LEN 1+ >str ;

: FMT-NUL-SECOND-COPY ( -- )
   FMT-COPY-SRC FMT-NUL-DST$ FS:COPY-FILE-STREAM ;

: FMT-TEST-NUL-SECOND-PATH ( -- )
   [: FMT-NUL-SECOND-COPY ;] E-FFI-NUL TTHROWSQ
   FMT-COPY-SRC FS:FILE? TTRUE
   FMT-COPY-DST FMT-READ-BUF 64 FS:READ-ALL 8 T=
   FMT-READ-BUF 8 >str s" copy-src" T$= ;

\ The descriptor open would hand out next: the lowest one free.
: FMT-NEXT-FD ( -- n )
   FMT-ROOT$ FS:OPEN-DIR-READ dup close ;

: FMT-LONG-STREAM-COPY ( -- )
   FMT-LONG! FMT-COPY-SRC FMT-LONG$ FS:COPY-FILE-STREAM ;

: FMT-TEST-STREAM-REFUSED-DST ( -- )
   FMT-NEXT-FD {: before:n :}
   s" FS:COPY-FILE-STREAM to a NUL destination throws and leaves no fd open" T-LABEL
   [: FMT-NUL-SECOND-COPY ;] E-FFI-NUL TTHROWSQ
   FMT-NEXT-FD before T=
   s" FS:COPY-FILE-STREAM to a too-long destination throws and leaves no fd open" T-LABEL
   [: FMT-LONG-STREAM-COPY ;] E-FS-PATH TTHROWSQ
   FMT-NEXT-FD before T= ;

: FMT-FILL-STREAM-DATA ( -- )
   0 begin dup FMT-STREAM-LEN < while
      dup 251 mod over FMT-STREAM-SRC-DATA + c!
      1+
   repeat drop ;

: FMT-TEST-STREAM-COPY ( -- )
   FMT-FILL-STREAM-DATA
   FMT-STREAM-SRC FMT-STREAM-SRC-DATA FMT-STREAM-LEN >str FS:WRITE-ALL
   FMT-STREAM-SRC FMT-STREAM-DST FS:COPY-FILE-STREAM
   FMT-STREAM-DST FS:FILE? TTRUE
   FMT-STREAM-DST FS:FILE-SIZE FMT-STREAM-LEN T=
   FMT-STREAM-DST FMT-STREAM-DST-DATA FMT-STREAM-LEN FS:READ-ALL FMT-STREAM-LEN T=
   FMT-STREAM-DST-DATA FMT-STREAM-LEN >str FMT-STREAM-SRC-DATA FMT-STREAM-LEN >str T$= ;

: FMT-TEST-ATOMIC-WRITE ( -- )
   FMT-ATOMIC-PATH s" old" FS:WRITE-ALL
   FMT-ATOMIC-PATH s" new-data" FS:ATOMIC-WRITE-FILE
   FMT-ATOMIC-PATH FMT-READ-BUF 64 FS:READ-ALL 8 T=
   FMT-READ-BUF 8 >str s" new-data" T$= ;

: FMT-TEST-ATOMIC-ALIASES ( -- )
   SB-RESET s" builder-source" SB-APPEND
   FMT-ATOMIC-PATH SB$ FS:ATOMIC-WRITE-FILE
   FMT-ATOMIC-PATH FMT-READ-BUF 64 FS:READ-ALL 14 T=
   FMT-READ-BUF 14 >str s" builder-source" T$=
   SB-RESET FMT-ATOMIC-PATH SB-APPEND
   SB$ s" builder-target" FS:ATOMIC-WRITE-FILE
   SB$ FMT-ATOMIC-PATH T$=
   FMT-ATOMIC-PATH FMT-READ-BUF 64 FS:READ-ALL 14 T=
   FMT-READ-BUF 14 >str s" builder-target" T$=
   \ Both creation paths apply the process umask to the same requested mode.
   FMT-REMOVE-PATH s" mode-control" FS:WRITE-ALL
   FMT-ATOMIC-PATH FS:STAT-MODE FMT-MODE-PERM and
   FMT-REMOVE-PATH FS:STAT-MODE FMT-MODE-PERM and T=
   FMT-REMOVE-PATH FS:REMOVE-FILE ;

: FMT-TEST-ATOMIC-LINK-MODE ( -- )
   FMT-ATOMIC-PATH FMT-MODE-0600 FS:CHMOD-MODE
   s" atomic.txt" FMT-LINK FS:MAKE-SYMLINK
   s" link.txt" FMT-BROKEN-LINK FS:MAKE-SYMLINK
   FMT-BROKEN-LINK s" through-links" FS:ATOMIC-WRITE-FILE
   FMT-LINK FS:SYMLINK? TTRUE
   FMT-BROKEN-LINK FS:SYMLINK? TTRUE
   FMT-BROKEN-LINK FMT-READ-BUF 64 FS:READ-ALL 13 T=
   FMT-READ-BUF 13 >str s" through-links" T$=
   FMT-ATOMIC-PATH FMT-READ-BUF 64 FS:READ-ALL 13 T=
   FMT-READ-BUF 13 >str s" through-links" T$=
   FMT-ATOMIC-PATH FS:STAT-MODE FMT-MODE-PERM and FMT-MODE-0600 T=
   FMT-BROKEN-LINK FS:REMOVE-FILE
   FMT-LINK FS:REMOVE-FILE ;

: FMT-TEST-ATOMIC-DANGLING-LINK ( -- )
   s" no-such-link-target.txt" FMT-BROKEN-LINK FS:MAKE-SYMLINK
   [: FMT-BROKEN-LINK s" refused" FS:ATOMIC-WRITE-FILE ;] E-FS-STAT TTHROWSQ
   FMT-BROKEN-LINK FS:SYMLINK? TTRUE
   FMT-BROKEN-TARGET FS:EXISTS? TFALSE
   FMT-BROKEN-LINK FS:REMOVE-FILE ;

: FMT-PARENT-OUTPUT ( -- str )
   SB-RESET FMT-PARENT-LINK SB-APPEND s" /entry.f" SB-APPEND SB$ ;

: FMT-PARENT-B-ENTRY ( -- str )
   SB-RESET FMT-PARENT-B SB-APPEND s" /entry.f" SB-APPEND SB$ ;

: FMT-PARENT-A-ENTRY ( -- str )
   SB-RESET FMT-PARENT-A SB-APPEND s" /entry.f" SB-APPEND SB$ ;

: FMT-PARENT-FILL ( str -- )
   s" published" FS:WRITE-ALL ;

: FMT-STAGED-TMP ( -- str )
   FMT-STAGED-TMP-BUF FMT-STAGED-TMP-U @ >str ;

\ The fill's own refusal remains the error when removing its sibling also
\ fails. Replacing the reserved file with a directory makes unlink refuse.
: FMT-STAGED-CLEAN-FAIL ( str -- )
   dup FMT-STAGED-TMP-BUF FMT-STAGED-TMP-U FMT-COPY!
   dup FS:REMOVE-FILE
   FS:MAKE-DIR
   E-FS-CAPACITY throw ;

\ A diagnostic line's class: the code's declared name, else its number.
: FMT-CLASS$ ( n -- str ) {: code :}
   code ERROR:NAME$ {: name:str :}
   name STR:LEN 0= if code FMT:INT$ exit then
   name ;

: FMT-ERR-PATH ( -- str )
   FMT-ROOT$ s" stderr.txt" FMT-ERR-PATH-BUF FS:JOIN-PATH {: u:n :}
   FMT-ERR-PATH-BUF u >str ;

: FMT-STAGED-CAPTURED ( -- )
   FMT-ERR-PATH STDERR:CAPTURE
   [: FMT-ATOMIC-PATH [: FMT-STAGED-CLEAN-FAIL ;] FS:REPLACE-STAGED ;]
   [: STDERR:;CAPTURE ;] finally ;

: FMT-STAGED-WANT ( -- str )
   SB-RESET
   E-FS-IO FMT-CLASS$ SB-APPEND
   s"  habu: " SB-APPEND
   FMT-STAGED-TMP SB-APPEND
   s" : staged replacement cleanup failed, errno " SB-APPEND
   SB$ ;

\ The errno's digits sit between the fixed text and the line's end.
: FMT-STAGED-LINE ( -- )
   FMT-ERR-PATH FMT-ERR-BUF FMT-ERR-CAP FS:READ-ALL {: u:n :}
   FMT-STAGED-WANT {: want:str :}
   u want STR:LEN 1+ > TTRUE
   FMT-ERR-BUF want STR:LEN >str want T$=
   FMT-ERR-BUF u + 1- c@ 10 T=
   FMT-ERR-BUF want STR:LEN + u want STR:LEN - 1- >str STR:>NUMBER? MATCH option
      none OF false TTRUE ENDOF
      some OF 0 > TTRUE ENDOF
   ;MATCH
   FMT-ERR-PATH FS:REMOVE-FILE ;

: FMT-TEST-STAGED-PRIMARY ( -- )
   FMT-ATOMIC-PATH s" previous" FS:WRITE-ALL
   s" staged fill error survives cleanup refusal" T-LABEL
   [: FMT-STAGED-CAPTURED ;] E-FS-CAPACITY TTHROWSQ
   FMT-ATOMIC-PATH FMT-READ-BUF 64 FS:READ-ALL {: u:n :}
   FMT-READ-BUF u >str s" previous" T$=
   s" the cleanup refusal's line names its operation and errno" T-LABEL
   FMT-STAGED-LINE
   FMT-STAGED-TMP FS:DIR? TTRUE
   FMT-STAGED-TMP FS:REMOVE-DIR
   FMT-STAGED-TMP s" removable" FS:WRITE-ALL
   FMT-ATOMIC-PATH FS:REMOVE-FILE ;

: FMT-STAGED-SPAN ( SPAN:span<u8> str -- SPAN:span<u8> )
   {: data path:str :}
   path data SPAN:$ FS:WRITE-ALL
   data ;

: FMT-STAGED-FAIL ( SPAN:span<u8> str -- SPAN:span<u8> )
   {: data path:str :}
   path FMT-STAGED-FORGOT-BUF FMT-STAGED-FORGOT-U FMT-COPY!
   path data SPAN:$ FS:WRITE-ALL
   E-FS-IO throw ;

: FMT-STAGED-FORGOT ( -- str )
   FMT-STAGED-FORGOT-BUF FMT-STAGED-FORGOT-U @ >str ;

: FMT-TEST-STAGED-CONTEXT ( -- )
   s" staged callbacks preserve a complete layout value" T-LABEL
   s" context" dup STR:DATA swap STR:LEN SPAN:MAKE FMT-ATOMIC-PATH [: FMT-STAGED-SPAN ;] FS:REPLACE-STAGED
   SPAN:$ s" context" T$=
   FMT-ATOMIC-PATH FMT-READ-BUF 64 FS:READ-ALL 7 T=
   FMT-READ-BUF 7 >str s" context" T$=
   s" a throwing callback preserves the old file and releases its sibling" T-LABEL
   [: s" partial" dup STR:DATA swap STR:LEN SPAN:MAKE FMT-ATOMIC-PATH
      [: FMT-STAGED-FAIL ;] FS:REPLACE-STAGED SPAN:$ drop ;] E-FS-IO TTHROWSQ
   FMT-ATOMIC-PATH FMT-READ-BUF 64 FS:READ-ALL 7 T=
   FMT-READ-BUF 7 >str s" context" T$=
   FMT-STAGED-FORGOT FS:EXISTS? TFALSE
   \ No slot names the sibling now: FMT-TEST-CLEANUP's run must leave this file.
   FMT-STAGED-FORGOT s" unowned" FS:WRITE-ALL
   s" a staged callback cannot consume its caller's context" T-LABEL
   s" FMT-BAD-STAGED ( n str -- ) [: 2drop drop ;] FS:REPLACE-STAGED"
      CHECK-QUIET-CANDIDATE! 0 T= ;

: FMT-STAGED-MODE-FILL ( str -- ) {: path:str :}
   path FS:STAT-MODE FMT-MODE-PERM and FMT-SIBLING-MODE !
   path s" private" FS:WRITE-ALL ;

: FMT-STAGED-X-FILL ( str -- )
   dup FS:CHMOD-X FMT-STAGED-MODE-FILL ;

: FMT-TEST-STAGED-MODE ( -- )
   FMT-ATOMIC-PATH FS:STAT-MODE FMT-MODE-PERM and {: before:n :}
   FMT-ATOMIC-PATH FMT-MODE-0620 FS:CHMOD-MODE
   s" a staged sibling is owner-only until filled, then takes path's mode" T-LABEL
   FMT-ATOMIC-PATH [: FMT-STAGED-MODE-FILL ;] FS:REPLACE-STAGED
   FMT-SIBLING-MODE @ FMT-MODE-0600 T=
   FMT-ATOMIC-PATH FS:STAT-MODE FMT-MODE-PERM and FMT-MODE-0620 T=
   s" a read-only target is replaced and stays read-only" T-LABEL
   FMT-ATOMIC-PATH s" old" FS:WRITE-ALL
   FMT-ATOMIC-PATH FMT-MODE-0444 FS:CHMOD-MODE
   FMT-ATOMIC-PATH [: FMT-STAGED-MODE-FILL ;] FS:REPLACE-STAGED
   FMT-ATOMIC-PATH FMT-READ-BUF 64 FS:READ-ALL 7 T=
   FMT-READ-BUF 7 >str s" private" T$=
   FMT-ATOMIC-PATH FS:STAT-MODE FMT-MODE-PERM and FMT-MODE-0444 T=
   s" a write-only target is replaced and stays write-only" T-LABEL
   FMT-ATOMIC-PATH FMT-MODE-0200 FS:CHMOD-MODE
   FMT-ATOMIC-PATH s" old" FS:WRITE-ALL
   FMT-ATOMIC-PATH [: FMT-STAGED-MODE-FILL ;] FS:REPLACE-STAGED
   FMT-ATOMIC-PATH FS:STAT-MODE FMT-MODE-PERM and FMT-MODE-0200 T=
   FMT-ATOMIC-PATH FMT-MODE-0600 FS:CHMOD-MODE
   FMT-ATOMIC-PATH FMT-READ-BUF 64 FS:READ-ALL 7 T=
   FMT-READ-BUF 7 >str s" private" T$=
   s" execute bits the fill sets on the sibling are kept" T-LABEL
   FMT-ATOMIC-PATH FS:MODE-0644 FS:CHMOD-MODE
   FMT-ATOMIC-PATH [: FMT-STAGED-X-FILL ;]
      FS:REPLACE-STAGED
   FMT-ATOMIC-PATH FS:STAT-MODE FMT-MODE-PERM and FMT-MODE-0755 T=
   FMT-ATOMIC-PATH before FS:CHMOD-MODE ;

: FMT-TEST-ATOMIC-RELATIVE ( -- )
   s" ./src/lib/../lib/fs-mutate.f" FS:ATOMIC-TARGET
   FMT-PARENT-BOUND-BUF FMT-PARENT-BOUND-U FMT-COPY!
   s" src/lib/fs-mutate.f" FS:ATOMIC-TARGET FMT-PARENT-BOUND T$=
   FMT-PARENT-BOUND-BUF c@ FS:SLASH T= ;

\ Binding an absent basename through a parent link must keep the directory
\ selected at bind time even if that link changes before the staged rename.
: FMT-TEST-ATOMIC-PARENT-LINK ( -- )
   FMT-PARENT-A FS:MAKE-DIR
   FMT-PARENT-B FS:MAKE-DIR
   FMT-PARENT-B-ENTRY s" source" FS:WRITE-ALL
   FMT-PARENT-A FMT-PARENT-LINK FS:MAKE-SYMLINK
   FMT-PARENT-OUTPUT FS:ATOMIC-TARGET
   FMT-PARENT-BOUND-BUF FMT-PARENT-BOUND-U FMT-COPY!
   FMT-PARENT-BOUND FMT-PARENT-A-ENTRY T$=
   FMT-PARENT-LINK FS:REMOVE-FILE
   FMT-PARENT-B FMT-PARENT-LINK FS:MAKE-SYMLINK
   FMT-PARENT-BOUND [: FMT-PARENT-FILL ;] FS:REPLACE-STAGED
   FMT-PARENT-BOUND FMT-READ-BUF 64 FS:READ-ALL 9 T=
   FMT-READ-BUF 9 >str s" published" T$=
   FMT-PARENT-B-ENTRY FMT-READ-BUF 64 FS:READ-ALL 6 T=
   FMT-READ-BUF 6 >str s" source" T$=
   FMT-PARENT-LINK FS:REMOVE-FILE
   FMT-PARENT-BOUND FS:REMOVE-FILE
   FMT-PARENT-B-ENTRY FS:REMOVE-FILE
   FMT-PARENT-A FS:REMOVE-DIR
   FMT-PARENT-B FS:REMOVE-DIR ;

\ A directory at the destination refuses the rename: E-FS-IO, and the emptied
\ parent's removal shows no sibling stayed.
: FMT-TEST-ATOMIC-FAILURE ( -- )
   FMT-MKDIR-PATH s" dest" FMT-COLLIDE-BUF FS:JOIN-PATH FMT-COLLIDE-U !
   FMT-MKDIR-PATH FS:MAKE-DIR
   FMT-COLLIDE FS:MAKE-DIR
   [: FMT-COLLIDE s" replacement" FS:ATOMIC-WRITE-FILE ;] E-FS-IO TTHROWSQ
   FMT-COLLIDE FS:DIR? TTRUE
   FMT-COLLIDE FS:REMOVE-DIR
   FMT-MKDIR-PATH FS:REMOVE-DIR ;

package FS-TEST
$C0 constant MODE-0300
$140 constant MODE-0500
create DROP-BOX-BUF PATH-CAP allot
variable DROP-BOX-U

: DROP-BOX$ ( -- str )
   DROP-BOX-BUF DROP-BOX-U @ >str ;

public
\ A directory its owner may write and search but not list, as a drop box is,
\ takes an atomic write. The mode is restored before any assert; the empty
\ directory's removal is what shows no sibling stayed.
: ATOMIC-SEARCH-DIR ( -- )
   FMT-MKDIR-PATH FS:MAKE-DIR
   FMT-MKDIR-PATH s" drop.txt" DROP-BOX-BUF FS:JOIN-PATH DROP-BOX-U !
   FMT-MKDIR-PATH MODE-0300 FS:CHMOD-MODE
   [: DROP-BOX$ s" dropped" FS:ATOMIC-WRITE-FILE ;] catch
   FMT-MKDIR-PATH FMT-MODE-0755 FS:CHMOD-MODE
   0 T=
   DROP-BOX$ FMT-READ-BUF 64 FS:READ-ALL 7 T=
   FMT-READ-BUF 7 >str s" dropped" T$=
   DROP-BOX$ FS:REMOVE-FILE
   FMT-MKDIR-PATH FS:REMOVE-DIR ;
;package

package FS-TEST
create PRIVATE-DIR-BUF PATH-CAP allot
variable PRIVATE-DIR-U
create PRIVATE-OUT-BUF PATH-CAP allot
variable PRIVATE-OUT-U

: PRIVATE-DIR$ ( -- str ) PRIVATE-DIR-BUF PRIVATE-DIR-U @ >str ;
: PRIVATE-OUT$ ( -- str ) PRIVATE-OUT-BUF PRIVATE-OUT-U @ >str ;


\ The destination holds exactly body at mode.
: PRIVATE-IS ( str n -- )
   {: body:str mode :}
   PRIVATE-OUT$ FMT-READ-BUF 64 FS:READ-ALL body STR:LEN T=
   FMT-READ-BUF body STR:LEN >str body T$=
   PRIVATE-OUT$ FS:STAT-MODE FMT-MODE-PERM and mode T= ;


public
\ FS:ATOMIC-WRITE-PRIVATE publishes 0600 whether the destination is new or
\ held 0644; FS:ATOMIC-WRITE-FILE gives a new destination WRITE-ALL's mode,
\ 0644 under the umask. The emptied directory's removal proves no sibling stayed.
: ATOMIC-PRIVATE ( -- )
   FMT-ROOT$ s" private" PRIVATE-DIR-BUF FS:JOIN-PATH PRIVATE-DIR-U !
   PRIVATE-DIR$ s" out.txt" PRIVATE-OUT-BUF FS:JOIN-PATH PRIVATE-OUT-U !
   PRIVATE-DIR$ FS:MAKE-DIR
   PRIVATE-OUT$ s" first" FS:ATOMIC-WRITE-PRIVATE
   s" first" FMT-MODE-0600 PRIVATE-IS
   PRIVATE-OUT$ FS:MODE-0644 FS:CHMOD-MODE
   PRIVATE-OUT$ s" narrowed" FS:ATOMIC-WRITE-PRIVATE
   s" narrowed" FMT-MODE-0600 PRIVATE-IS
   PRIVATE-OUT$ FS:REMOVE-FILE
   FMT-REMOVE-PATH s" mode-control" FS:WRITE-ALL
   FMT-REMOVE-PATH FS:STAT-MODE FMT-MODE-PERM and {: fresh:n :}
   FMT-REMOVE-PATH FS:REMOVE-FILE
   PRIVATE-OUT$ s" default" FS:ATOMIC-WRITE-FILE
   s" default" fresh PRIVATE-IS
   PRIVATE-OUT$ FS:REMOVE-FILE
   PRIVATE-DIR$ FS:REMOVE-DIR ;


\ FS:ATOMIC-CREATE-PRIVATE publishes 0600 only to a free name; a taken one keeps
\ text and mode, in a 0300 or 0500 directory too; REMOVE-DIR shows no sibling.
: ATOMIC-CREATE ( -- )
   FMT-ROOT$ s" create" PRIVATE-DIR-BUF FS:JOIN-PATH PRIVATE-DIR-U !
   PRIVATE-DIR$ s" out.txt" PRIVATE-OUT-BUF FS:JOIN-PATH PRIVATE-OUT-U !
   PRIVATE-DIR$ FS:MAKE-DIR
   PRIVATE-OUT$ s" first" FS:ATOMIC-CREATE-PRIVATE TTRUE
   s" first" FMT-MODE-0600 PRIVATE-IS
   PRIVATE-OUT$ s" second" FS:ATOMIC-CREATE-PRIVATE TFALSE
   s" first" FMT-MODE-0600 PRIVATE-IS
   PRIVATE-OUT$ FS:REMOVE-FILE
   PRIVATE-OUT$ s" written" FS:WRITE-ALL
   PRIVATE-OUT$ FS:STAT-MODE FMT-MODE-PERM and {: written:n :}
   PRIVATE-OUT$ s" second" FS:ATOMIC-CREATE-PRIVATE TFALSE
   s" written" written PRIVATE-IS
   PRIVATE-OUT$ FS:REMOVE-FILE
   PRIVATE-DIR$ MODE-0300 FS:CHMOD-MODE
   false false
   [: 2drop PRIVATE-OUT$ s" boxed" FS:ATOMIC-CREATE-PRIVATE
      PRIVATE-OUT$ s" again" FS:ATOMIC-CREATE-PRIVATE ;] catch {: code:n :}
   PRIVATE-DIR$ FMT-MODE-0755 FS:CHMOD-MODE
   code 0 T=
   code 0= if TFALSE TTRUE else 2drop then
   s" boxed" FMT-MODE-0600 PRIVATE-IS
   PRIVATE-DIR$ MODE-0500 FS:CHMOD-MODE
   false
   [: drop PRIVATE-OUT$ s" refused" FS:ATOMIC-CREATE-PRIVATE ;]
   catch {: denied:n :}
   PRIVATE-DIR$ FMT-MODE-0755 FS:CHMOD-MODE
   denied 0 T=
   denied 0= if TFALSE else drop then
   s" boxed" FMT-MODE-0600 PRIVATE-IS
   PRIVATE-OUT$ FS:REMOVE-FILE
   PRIVATE-DIR$ FS:REMOVE-DIR ;
;package

package FS-TEST
70 constant MANY-COUNT        \ spans two FS:ATOMIC-CREATE-FILES groups of 64
5 constant MANY-TAKEN         \ a name written before the call
66 constant MANY-THROW-AT     \ in the second group
8192 constant MANY-CAP
create MANY-DIR-BUF PATH-CAP allot
variable MANY-DIR-U
create MANY-PATH-BUF PATH-CAP allot
create MANY-BYTES-BUF MANY-CAP allot
create MANY-GOT-BUF MANY-CAP allot

: MANY-DIR$ ( -- str ) MANY-DIR-BUF MANY-DIR-U @ >str ;

: MANY-PATH$ ( n -- str )
   MANY-DIR$ swap FMT:INT$ MANY-PATH-BUF FS:JOIN-PATH
   MANY-PATH-BUF swap >str ;

\ File i's bytes: 97i + 1 of them, each its offset plus 7i.
: MANY-BYTES ( n -- str ) {: idx :}
   idx 97 * 1+ {: size :}
   size 0 ?do i idx 7 * + 255 and MANY-BYTES-BUF i + c! loop
   MANY-BYTES-BUF size >str ;

\ The body: file i's name and bytes.
: MANY-FILE ( n -- str str ) {: idx :}
   idx MANY-BYTES idx FMT:INT$ swap ;

\ E-FS-BAND is a code FS:ATOMIC-CREATE-FILES never throws itself.
: MANY-THROWS ( n -- str str )
   dup MANY-THROW-AT = if E-FS-BAND throw then
   MANY-FILE ;

\ The body: both files are named `twice`, the second's name taken since FILL.
: MANY-TWICE ( n -- str str ) {: idx :}
   s" twice" idx 0= if s" first" else s" second" then ;

: MANY-HOLDS ( n -- ) {: idx :}
   idx MANY-PATH$ MANY-GOT-BUF MANY-CAP FS:READ-ALL {: gotu :}
   MANY-GOT-BUF gotu >str idx MANY-BYTES T$= ;

: MANY-REMOVE ( n -- )
   0 ?do i MANY-PATH$ FS:REMOVE-FILE loop
   MANY-DIR$ FS:REMOVE-DIR ;

public
\ FS:ATOMIC-CREATE-FILES over two groups: the name taken beforehand keeps its
\ bytes, every other holds its own; a name taken since FILL keeps the bytes
\ published first; and REMOVE-DIR shows no sibling stayed.
: CREATE-MANY ( -- )
   FMT-ROOT$ s" many" MANY-DIR-BUF FS:JOIN-PATH MANY-DIR-U !
   MANY-DIR$ FS:MAKE-DIR
   MANY-TAKEN MANY-PATH$ s" taken" FS:WRITE-ALL
   MANY-DIR$ MANY-COUNT [: MANY-FILE ;] FS:ATOMIC-CREATE-FILES
   s" a name taken before FS:ATOMIC-CREATE-FILES keeps its bytes" T-LABEL
   MANY-TAKEN MANY-PATH$ MANY-GOT-BUF MANY-CAP FS:READ-ALL {: takenu :}
   MANY-GOT-BUF takenu >str s" taken" T$=
   s" every other name FS:ATOMIC-CREATE-FILES made holds its bytes" T-LABEL
   MANY-COUNT 0 ?do i MANY-TAKEN <> if i MANY-HOLDS then loop
   MANY-DIR$ 2 [: MANY-TWICE ;] FS:ATOMIC-CREATE-FILES
   s" a name taken since FILL keeps the first file's bytes" T-LABEL
   MANY-DIR$ s" twice" MANY-PATH-BUF FS:JOIN-PATH {: twiceu :}
   MANY-PATH-BUF twiceu >str MANY-GOT-BUF MANY-CAP FS:READ-ALL {: gotu :}
   MANY-GOT-BUF gotu >str s" first" T$=
   T-LABEL-CLEAR
   MANY-PATH-BUF twiceu >str FS:REMOVE-FILE
   MANY-COUNT MANY-REMOVE ;

\ A body's throw in the second group: the first group's names hold their
\ bytes, none of the second's is made, and REMOVE-DIR shows no sibling.
: CREATE-MANY-THROW ( -- )
   MANY-DIR$ FS:MAKE-DIR
   s" a body's throw passes through FS:ATOMIC-CREATE-FILES" T-LABEL
   [: MANY-DIR$ MANY-COUNT [: MANY-THROWS ;] FS:ATOMIC-CREATE-FILES ;]
   E-FS-BAND TTHROWSQ
   s" ... and the first group's names hold their bytes" T-LABEL
   64 0 ?do i MANY-HOLDS loop
   s" ... and none of the second group's names is made" T-LABEL
   MANY-COUNT 64 ?do i MANY-PATH$ FS:TRY-LSTAT TFALSE loop
   T-LABEL-CLEAR
   64 MANY-REMOVE ;
;package

: FMT-TEST-DIRS ( -- )
   FMT-MKDIR-PATH FS:MAKE-DIR
   FMT-MKDIR-PATH FS:DIR? TTRUE
   FMT-NEST-C FS:MAKE-DIRS
   FMT-NEST-A FS:DIR? TTRUE
   FMT-NEST-B FS:DIR? TTRUE
   FMT-NEST-C FS:DIR? TTRUE ;

\ Under a parent it cannot write, FS:MAKE-DIRS keeps the folder already there
\ and fails the one it must make with mkdir's own FS:ERRNO, EACCES, which the
\ mode restored after it does not replace. Each call is caught so the mode comes
\ back before an assert; the first runs while FMT-NEST-C exists, and a throw from
\ it would leave a folder the exit cleanup cannot remove.
: FMT-TEST-DIRS-DENIED ( -- )
   FMT-NEST-B FMT-MODE-0555 FS:CHMOD-MODE
   [: FMT-NEST-C FS:MAKE-DIRS ;] catch
   FMT-NEST-B FMT-MODE-0755 FS:CHMOD-MODE
   0 T=
   FMT-NEST-C FS:REMOVE-DIR
   FMT-NEST-B FMT-MODE-0555 FS:CHMOD-MODE
   [: FMT-NEST-C FS:MAKE-DIRS ;] catch FS:ERRNO {: code:n cause:n :}
   FMT-NEST-B FMT-MODE-0755 FS:CHMOD-MODE
   code E-FS-IO T=
   cause FMT-EACCES T=
   FMT-NEST-C FS:DIR? TFALSE ;

package FS-TEST
create NAMED-BUF PATH-CAP allot
create NAMED-DIR-BUF PATH-CAP allot

\ The directory each name below goes in; a case that empties it removes it.
: NAMED-DIR$ ( -- str )
   FMT-ROOT$ s" named" NAMED-DIR-BUF FS:JOIN-PATH NAMED-DIR-BUF swap >str ;

: NAMED$ ( n -- str )
   {: n:n :}
   NAMED-DIR$ {: dir:str :}
   dir STR:DATA NAMED-BUF dir STR:LEN BYTE-COPY
   FS:SLASH NAMED-BUF dir STR:LEN + c!
   dir STR:LEN 1+ n + dir STR:LEN 1+ ?do 110 NAMED-BUF i + c! loop
   NAMED-BUF dir STR:LEN 1+ n + >str ;

public
\ Past NAME_MAX, the host refuses the component: E-FS-IO with its cause.
: DIRS-NAME-TOO-LONG ( -- )
   [: 300 NAMED$ FS:MAKE-DIRS ;] catch FS:ERRNO {: code:n cause:n :}
   code E-FS-IO T=
   cause FS:ENAMETOOLONG T=
   NAMED-DIR$ FS:REMOVE-DIR ;

\ The sibling's name never carries the destination's, so any last name the host
\ accepts, up to NAME_MAX, is written, and no sibling stays behind.
: ATOMIC-NAMED ( n -- )
   {: n:n :}
   NAMED-DIR$ FS:MAKE-DIR
   n NAMED$ s" long" FS:ATOMIC-WRITE-FILE
   n NAMED$ FMT-READ-BUF 64 FS:READ-ALL 4 T=
   FMT-READ-BUF 4 >str s" long" T$=
   n NAMED$ FS:REMOVE-FILE
   NAMED-DIR$ FS:REMOVE-DIR ;

\ A last name past NAME_MAX is the host's refusal, named by its cause; the
\ sibling it staged goes.
: ATOMIC-NAME-TOO-LONG ( -- )
   NAMED-DIR$ FS:MAKE-DIR
   [: 300 NAMED$ s" x" FS:ATOMIC-WRITE-FILE ;] catch
   FS:ERRNO {: code:n cause:n :}
   code E-FS-IO T=
   cause FS:ENAMETOOLONG T=
   NAMED-DIR$ FS:REMOVE-DIR ;

\ FS:ATOMIC-TARGET's realpath drops the /. pad, and the target's directory
\ leaves no room within PATH-CAP for even the shortest sibling, /.tmp-0-0.
: ATOMIC-PATH-CAP ( -- )
   PATH-CAP 8 - {: least:n :}
   least PATH-CAP least - 1 and + FMT-DEEP-DIR {: deep:str :}
   deep STR:DATA {: dir:^ :}
   PATH-CAP 2 - deep STR:LEN ?do
      FS:SLASH dir i + c!  [char] . dir i 1+ + c!
   2 +loop
   FS:SLASH dir PATH-CAP 2 - + c!
   [char] x dir PATH-CAP 1- + c!
   dir PATH-CAP >str s" capped" FS:ATOMIC-WRITE-FILE
   dir PATH-CAP >str FMT-READ-BUF 64 FS:READ-ALL 6 T=
   FMT-READ-BUF 6 >str s" capped" T$= ;
;package

\ A base of the longest length FS:MKTEMP-DIR-IN builds in still takes a temp.
: FMT-TEMP-BASE-MAX ( -- )
   s" max" STR:LEN FS:TEMP-DIR-BASE-MAX FMT-DEEP-DIR {: base:str :}
   base s" max" FS:MKTEMP-DIR-IN {: temp:str :}
   temp FS:DIR? TTRUE
   temp STR:DATA base STR:LEN >str base T$= ;

: FMT-TEST-TEMPS ( -- )
   FMT-TEMP-BASE s" one" FS:MKTEMP-DIR-IN dup FS:DIR? TTRUE FS:CLEANUP-DIR+
   FMT-TEMP-BASE s" two" FS:MKTEMP-DIR-IN dup FS:DIR? TTRUE FS:CLEANUP-DIR+
   s" habu-fs-mut" FS:MKTEMP-DIR dup FS:DIR? TTRUE FS:CLEANUP-DIR+
   FMT-TEMP-BASE-MAX ;

\ A base that already ends in a slash, as macOS's TMPDIR does, takes no second
\ one. FMT-TEST-DIRS left FMT-MKDIR-PATH for the base.
: FMT-TEST-TEMP-SLASH ( -- )
   FMT-MKDIR-PATH FMT-COLLIDE-BUF FMT-COLLIDE-U FMT-COPY!
   FS:SLASH FMT-COLLIDE-BUF FMT-COLLIDE-U @ + c!
   FMT-COLLIDE-U @ 1+ FMT-COLLIDE-U !
   FMT-COLLIDE s" slash" FS:MKTEMP-DIR-IN {: temp:str :}
   temp FS:DIR? TTRUE
   FMT-MKDIR-PATH s" slash-" FMT-UNIQUE-BUF FS:JOIN-PATH {: wantu:n :}
   temp FMT-UNIQUE-BUF wantu >str STR:STARTS-WITH? TTRUE
   temp FS:REMOVE-DIR ;

: FMT-TEST-MKTEMP ( -- )
   s" FS:MKTEMP makes a private read-write file under the temp root" T-LABEL
   s" habu-fs-mkf" FS:MKTEMP {: path:str file:fd :}
   path FMT-COLLIDE-BUF FMT-COLLIDE-U FMT-COPY!
   FMT-COLLIDE FS:CLEANUP+
   s" TMPDIR" GETENV dup STR:LEN 0= if drop s" /tmp" then FMT-REAL
   s" habu-fs-mkf-" FMT-UNIQUE-BUF FS:JOIN-PATH {: wantu:n :}
   FMT-COLLIDE FMT-UNIQUE-BUF wantu >str STR:STARTS-WITH? TTRUE
   FMT-COLLIDE FS:STAT-MODE FMT-MODE-PERM and FMT-MODE-0600 T=
   file FD>N s" mkstemp" write 7 T=
   file FD>N FMT-READ-BUF 64 read 0 T=
   file FD>N FS:CLOSE-FD 0 T=
   FMT-COLLIDE FMT-READ-BUF 64 FS:READ-ALL 7 T=
   FMT-READ-BUF 7 >str s" mkstemp" T$=
   s" habu-fs-mkf" FS:MKTEMP {: next:str nextfd:fd :}
   next FS:CLEANUP+
   nextfd FD>N FS:CLOSE-FD 0 T=
   next FMT-COLLIDE T$<> ;

\ Child engines: TMPDIR is read from the process environment, which a running
\ process does not change for itself. A child whose TMPDIR cannot hold a temp
\ directory or file exits nonzero with one stderr line naming TMPDIR and the
\ value, and the uncaught code. One whose TMPDIR reaches a directory through a
\ symlink (macOS /tmp is /private/tmp) makes its directory under the canonical
\ spelling. Paths and child programs live under this test's root.
package FMT-TMPDIR
private

\ A child's stderr line can echo a TMPDIR of nearly PATH-CAP bytes.
PATH-CAP 1+ 4 * constant CAP
10 constant NL

create OUT CAP allot
create ERR CAP allot
create MISSING-BUF PATH-CAP allot
create PROG-BUF PATH-CAP allot
create FILE-PROG-BUF PATH-CAP allot
create CANON-PROG-BUF PATH-CAP allot
create CAUGHT-PROG-BUF PATH-CAP allot
create WANT-BUF PATH-CAP 256 + allot  \ a record's code, message, value and help
variable MISSING-U
variable PROG-U
variable FILE-PROG-U
variable CANON-PROG-U
variable CAUGHT-PROG-U
variable WANT-U

: MISSING$ ( -- str )
   MISSING-BUF MISSING-U @ >str ;

: PROG$ ( -- str )
   PROG-BUF PROG-U @ >str ;

: FILE-PROG$ ( -- str )
   FILE-PROG-BUF FILE-PROG-U @ >str ;

: CANON-PROG$ ( -- str )
   CANON-PROG-BUF CANON-PROG-U @ >str ;

: CAUGHT-PROG$ ( -- str )
   CAUGHT-PROG-BUF CAUGHT-PROG-U @ >str ;

: PATHS! ( -- )
   FMT-ROOT$ s" no-such-tmpdir" MISSING-BUF FS:JOIN-PATH MISSING-U !
   FMT-ROOT$ s" tmpdir-child.f" PROG-BUF FS:JOIN-PATH PROG-U !
   FMT-ROOT$ s" tmpdir-file.f" FILE-PROG-BUF FS:JOIN-PATH FILE-PROG-U !
   FMT-ROOT$ s" tmpdir-canon.f" CANON-PROG-BUF FS:JOIN-PATH CANON-PROG-U !
   FMT-ROOT$ s" tmpdir-caught.f" CAUGHT-PROG-BUF FS:JOIN-PATH CAUGHT-PROG-U ! ;

: PROG-SRC$ ( -- str )
   S\" require src/lib/fs-mutate.f\ns\q tmpdir-child\q FS:MKTEMP-DIR drop\n" ;

: FILE-PROG-SRC$ ( -- str )
   S\" require src/lib/fs-mutate.f\ns\q tmpdir-file\q FS:MKTEMP drop drop\n" ;

: CAUGHT-PROG-SRC$ ( -- str )
   S\" require src/lib/fs-mutate.f\n: GO ( -- ) [: s\q tmpdir-caught\q FS:MKTEMP drop drop ;] catch E-FS-DIR <> if 3 throw then 1 s\q on\q write drop ;\nGO\n" ;

: CANON-PROG-SRC$ ( -- str )
   S\" require src/lib/fs-mutate.f\n: LINE ( str -- ) {: s :} 1 s write drop 1 S\\\q \\n\q write drop ;\ns\q canon\q FS:MKTEMP-DIR LINE\n" ;

\ A directory one byte past the longest TMPDIR with room for the temp name
\ PROG-SRC$ asks for, whose prefix is tmpdir-child.
: LONG$ ( -- str )
   s" tmpdir-child" STR:LEN FS:TEMP-DIR-BASE-MAX 1+ FMT-DEEP-DIR ;

: CHILD-ENV ( str -- ) {: s:str :}
   PROC-ARGV-ENV-RESET
   s" TMPDIR" s PROC-ENV-SET ;

: CAPTURE ( str -- len len PROCESS:ending ) {: prog:str :}
   PROC-ENV-INHERIT-MISSING
   prog PROC-ARGV+
   ENGINE-CANDIDATE:PATH$ OUT CAP >LEN ERR CAP >LEN PROCESS:WAIT-FOREVER
   RUN-ARGV-ENV-CAPTURE-OUTCOME ;

: WANT+ ( str -- ) {: s:str :}
   s STR:DATA WANT-BUF WANT-U @ + s STR:LEN BYTE-COPY
   WANT-U @ s STR:LEN + WANT-U ! ;

\ The prose record of TMPDIR s, made in pieces: a value near PATH-CAP bytes
\ with its texts overflows SB-CAP.
: WANT! ( str str str str -- )
   {: name:str why:str s:str h:str :}
   0 WANT-U !
   s" error[" WANT+  name WANT+  s" ]: " WANT+  why WANT+
   S\" \n  = TMPDIR: " WANT+  s WANT+
   S\" \n  = help: " WANT+  h WANT+  S\" \n\n" WANT+ ;

: WANT$ ( -- str ) WANT-BUF WANT-U @ >str ;

: DIR-WANT! ( str -- ) {: s:str :}
   s" E-FS-DIR" s" TMPDIR is not a directory" s
   s" point TMPDIR at an existing directory, or unset it to use /tmp" WANT! ;

\ The child failed printing WANT$ once, as the record its uncaught throw of code
\ carries, in place of the engine's own record of that throw.
: RECORD-ONCE ( str len len PROCESS:ending n -- )
   {: prog:str outu:len erru:len oc code:n :}
   oc MATCH PROCESS:ending
     exited OF 0 T<> ENDOF
     signaled OF s" exit" s" signal" T-ENDING-MISS ENDOF
     timeout OF prog OUT outu LEN>N >str ERR erru LEN>N >str T-TIMED-OUT ENDOF
   ;MATCH
   ERR erru LEN>N >str WANT$ STR:COUNT-SUB 1 T=
   ERR erru LEN>N >str code TEST-EVAL:THROWN-HEAD$ STR:CONTAINS? TFALSE ;

: REJECTED ( str str n -- )
   {: prog:str s:str code:n :}
   s CHILD-ENV prog CAPTURE {: outu:len erru:len oc :}
   prog outu erru oc code RECORD-ONCE ;

\ tools/check.f checking prog under TMPDIR s: its run makes a temp directory.
: CHECK-REJECTED ( str str n -- )
   {: prog:str s:str code:n :}
   s CHILD-ENV
   s" --load" PROC-ARGV+  s" tools/check.f" PROC-ARGV+
   s" --" PROC-ARGV+
   prog CAPTURE {: outu:len erru:len oc :}
   prog outu erru oc code RECORD-ONCE ;

\ A child that catches FS:MKTEMP's E-FS-DIR goes on and prints no record.
: CAUGHT ( -- )
   MISSING$ CHILD-ENV CAUGHT-PROG$ CAPTURE {: outu:len erru:len oc :}
   CAUGHT-PROG$ OUT outu LEN>N >str ERR erru LEN>N >str oc 0 T-ENDING-EXITED=
   OUT outu LEN>N >str s" on" T$=
   ERR erru LEN>N >str s" " T$= ;

\ With TMPDIR a link to a directory, the line the child prints is its own
\ realpath. The directory is removed by name, so the parent's removal shows the
\ child made nothing else there.
: CANONICAL ( -- )
   FMT-PARENT-A FS:MAKE-DIR
   FMT-PARENT-A FMT-PARENT-LINK FS:MAKE-SYMLINK
   CANON-PROG$ CANON-PROG-SRC$ FS:WRITE-ALL
   CANON-PROG$ FS:CLEANUP+
   FMT-PARENT-LINK CHILD-ENV
   CANON-PROG$ CAPTURE {: outu:len erru:len oc :}
   CANON-PROG$ OUT outu LEN>N >str ERR erru LEN>N >str oc 0 T-ENDING-EXITED=
   OUT outu LEN>N >str NL 0 STR:SPLIT-NEXT drop drop {: td:str :}
   td td FMT-REAL T$=
   td FS:REMOVE-DIR
   FMT-PARENT-LINK FS:REMOVE-FILE
   FMT-PARENT-A FS:REMOVE-DIR ;

public

: RUN ( -- )
   PATHS!
   PROG$ PROG-SRC$ FS:WRITE-ALL
   PROG$ FS:CLEANUP+
   FILE-PROG$ FILE-PROG-SRC$ FS:WRITE-ALL
   FILE-PROG$ FS:CLEANUP+
   s" a missing TMPDIR is rejected by a record naming it" T-LABEL
   MISSING$ DIR-WANT!  PROG$ MISSING$ E-FS-DIR REJECTED
   s" FS:MKTEMP rejects a missing TMPDIR by a record naming it" T-LABEL
   MISSING$ DIR-WANT!  FILE-PROG$ MISSING$ E-FS-DIR REJECTED
   s" tools/check.f rejects a missing TMPDIR by a record naming it" T-LABEL
   MISSING$ DIR-WANT!  PROG$ MISSING$ E-FS-DIR CHECK-REJECTED
   s" a TMPDIR with no room for the temp name is rejected by a record naming it"
   T-LABEL
   s" E-FS-CAPACITY" s" TMPDIR is too long for a temp path" LONG$
   s" point TMPDIR at a directory with a shorter path, or unset it to use /tmp"
   WANT!
   PROG$ LONG$ E-FS-CAPACITY REJECTED
   s" a caught rejection of a missing TMPDIR prints no record" T-LABEL
   CAUGHT-PROG$ CAUGHT-PROG-SRC$ FS:WRITE-ALL
   CAUGHT-PROG$ FS:CLEANUP+
   CAUGHT
   s" a TMPDIR through a link makes a canonical temp directory" T-LABEL
   CANONICAL ;

;package

: FMT-PATH-IDENTITY-MISSING ( -- )
   FMT-BROKEN-TARGET FS:PATH-IDENTITY 2drop ;

: FMT-PATH-IDENTITY-NUL ( -- )
   S\" a\zb" FS:PATH-IDENTITY 2drop ;

\ The link and its target name one file; the copy source is another.
: FMT-TEST-PATH-IDENTITY ( -- )
   s" a symlink and its target have one identity" T-LABEL
   FMT-LINK FS:PATH-IDENTITY FMT-LINK-TARGET FS:PATH-IDENTITY
   {: link-dev:n link-ino:n dev:n ino:n :}
   link-dev dev T=
   link-ino ino T=
   s" two files have different identities" T-LABEL
   FMT-COPY-SRC FS:PATH-IDENTITY {: other-dev:n other-ino:n :}
   other-dev dev = other-ino ino = and TFALSE
   s" a missing path has no identity" T-LABEL
   [: FMT-PATH-IDENTITY-MISSING ;] E-FS-STAT TTHROWSQ
   s" a path holding a NUL has no identity" T-LABEL
   [: FMT-PATH-IDENTITY-NUL ;] E-FFI-NUL TTHROWSQ ;

: FMT-TEST-SYMLINK ( -- )
   FMT-LINK-TARGET s" link-target" FS:WRITE-ALL
   FMT-LINK-TARGET FMT-LINK FS:MAKE-SYMLINK
   FMT-LINK FS:SYMLINK? TTRUE
   FMT-LINK FS:FILE? TTRUE
   FMT-TEST-PATH-IDENTITY
   FMT-LINK FMT-READ-BUF 64 FS:READ-ALL 11 T=
   FMT-READ-BUF 11 >str s" link-target" T$=
   [: FMT-MAKE-SYMLINK-EXISTS ;] E-FS-IO TTHROWSQ
   FMT-LINK FS:REMOVE-FILE
   FMT-LINK FS:SYMLINK? TFALSE
   FMT-LINK-TARGET FS:FILE? TTRUE
   FMT-LINK-TARGET FS:REMOVE-FILE
   FMT-LINK-TARGET FS:EXISTS? TFALSE ;

: FMT-TEST-BROKEN-SYMLINK ( -- )
   FMT-BROKEN-TARGET FMT-BROKEN-LINK FS:MAKE-SYMLINK
   FMT-BROKEN-LINK FS:EXISTS? TFALSE
   FMT-BROKEN-LINK FS:SYMLINK? TTRUE
   \ It still names its target: a file made there reads through it.
   FMT-BROKEN-TARGET s" arrived" FS:WRITE-ALL
   FMT-BROKEN-LINK FMT-READ-BUF 64 FS:READ-ALL 7 T=
   FMT-READ-BUF 7 >str s" arrived" T$=
   FMT-BROKEN-TARGET FS:REMOVE-FILE
   FMT-BROKEN-LINK FS:REMOVE-FILE
   FMT-BROKEN-LINK FS:SYMLINK? TFALSE ;

: FMT-TEST-REMOVE-TREE ( -- )
   FMT-WRITE-TREE
   FMT-TREE-ROOT-FILE FS:FILE? TTRUE
   FMT-TREE-LEAF FS:FILE? TTRUE
   FMT-TREE-DOTS-FILE FS:FILE? TTRUE
   FMT-TREE FS:REMOVE-TREE
   FMT-TREE FS:EXISTS? TFALSE
   FMT-TREE FS:REMOVE-TREE
   s" no-such-habu-fs-remove-tree" FS:REMOVE-TREE ;

: FMT-TEST-REMOVE-TREE-SYMLINK-DIR ( -- )
   FMT-LINK-DIR FS:MAKE-DIR
   FMT-LINK-DIR-FILE s" kept" FS:WRITE-ALL
   FMT-TREE FS:MAKE-DIR
   FMT-LINK-DIR FMT-TREE-DIR-LINK FS:MAKE-SYMLINK
   FMT-TREE-DIR-LINK FS:SYMLINK? TTRUE
   FMT-TREE FS:REMOVE-TREE
   FMT-TREE FS:EXISTS? TFALSE
   FMT-LINK-DIR FS:DIR? TTRUE
   FMT-LINK-DIR-FILE FS:FILE? TTRUE ;

create FMT-ROOT-PATH-BUF PATH-CAP allot

\ FMT-TREE with a trailing slash: lstat of `link/` resolves the link.
: FMT-TREE-SLASH$ ( -- str )
   FMT-TREE {: s:str :}
   s STR:DATA FMT-ROOT-PATH-BUF s STR:LEN BYTE-COPY
   FS:SLASH FMT-ROOT-PATH-BUF s STR:LEN + c!
   FMT-ROOT-PATH-BUF s STR:LEN 1 + >str ;

: FMT-REMOVE-TREE-SLASH ( -- )
   FMT-TREE-SLASH$ FS:REMOVE-TREE ;

: FMT-TEST-REMOVE-TREE-SLASHED-LINK ( -- )
   FMT-LINK-DIR FMT-TREE FS:MAKE-SYMLINK
   FMT-REMOVE-TREE-SLASH
   s" FS:REMOVE-TREE of link/ unlinks the link" T-LABEL
   FMT-TREE FS:SYMLINK? TFALSE
   s" FS:REMOVE-TREE of link/ keeps the target and its file" T-LABEL
   FMT-LINK-DIR FS:DIR? TTRUE
   FMT-LINK-DIR-FILE FS:FILE? TTRUE ;

: FMT-TEST-REMOVE-TREE-DANGLING ( -- )
   FMT-BROKEN-TARGET FMT-BROKEN-LINK FS:MAKE-SYMLINK
   FMT-BROKEN-LINK FS:REMOVE-TREE
   s" FS:REMOVE-TREE unlinks a dangling link root" T-LABEL
   FMT-BROKEN-LINK FS:SYMLINK? TFALSE
   FMT-TREE FS:MAKE-DIR
   FMT-BROKEN-TARGET FMT-TREE-DIR-LINK FS:MAKE-SYMLINK
   FMT-TREE FS:REMOVE-TREE
   s" FS:REMOVE-TREE unlinks a dangling link inside the tree" T-LABEL
   FMT-TREE FS:EXISTS? TFALSE ;

\ kept.txt/x: a root whose parent component is a regular file.
: FMT-REMOVE-TREE-NOTDIR ( -- )
   FMT-LINK-DIR-FILE s" x" FMT-ROOT-PATH-BUF FS:JOIN-PATH {: u:n :}
   FMT-ROOT-PATH-BUF u >str FS:REMOVE-TREE ;

\ kept.txt/: a regular file spelled as a directory.
: FMT-REMOVE-FILE-SLASH ( -- )
   FMT-LINK-DIR-FILE {: s:str :}
   s STR:DATA FMT-ROOT-PATH-BUF s STR:LEN BYTE-COPY
   FS:SLASH FMT-ROOT-PATH-BUF s STR:LEN + c!
   FMT-ROOT-PATH-BUF s STR:LEN 1 + >str FS:REMOVE-TREE ;

: FMT-TEST-REMOVE-TREE-NOTDIR ( -- )
   [: FMT-REMOVE-TREE-NOTDIR ;] catch FS:ERRNO {: code:n cause:n :}
   s" FS:REMOVE-TREE of a root under a file throws E-FS-STAT" T-LABEL
   code E-FS-STAT T=
   s" FS:ERRNO names ENOTDIR" T-LABEL
   cause FMT-ENOTDIR T=
   [: FMT-REMOVE-FILE-SLASH ;] catch FS:ERRNO {: fcode:n fcause:n :}
   s" FS:REMOVE-TREE of file/ throws E-FS-STAT with ENOTDIR" T-LABEL
   fcode E-FS-STAT T=
   fcause FMT-ENOTDIR T=
   s" and keeps the file" T-LABEL
   FMT-LINK-DIR-FILE FS:FILE? TTRUE ;

: FMT-DEEP$ ( n -- str ) {: level:n :}
   FMT-TREE {: s:str :}
   s STR:DATA FMT-ROOT-PATH-BUF s STR:LEN BYTE-COPY
   level 0 ?do
      FS:SLASH FMT-ROOT-PATH-BUF s STR:LEN i 2 * + + c!
      100 FMT-ROOT-PATH-BUF s STR:LEN i 2 * + 1 + + c!
   loop
   FMT-ROOT-PATH-BUF s STR:LEN level 2 * + >str ;

: FMT-TEST-RMDIR-ERRNO ( -- )
   [: FMT-RMDIR-MISSING ;] catch FS:ERRNO {: code:n cause:n :}
   s" removing a missing directory is E-FS-IO with ENOENT" T-LABEL
   code E-FS-IO T=
   cause FS:ENOENT T=
   1 FMT-DEEP$ FS:MAKE-DIRS
   [: FMT-TREE FS:REMOVE-DIR ;] catch FS:ERRNO {: full:n full-cause:n :}
   s" removing a directory that holds one is E-FS-IO with ENOTEMPTY" T-LABEL
   full E-FS-IO T=
   full-cause FS:ENOTEMPTY T=
   FMT-TREE FS:REMOVE-TREE ;

\ A tree deeper than the walk's starting frames whose last directory is
\ locked: the removal grows its frames on the way down, then unlinkat refuses
\ the deepest level with EACCES while every level above it is open. A leaked
\ descriptor would hold the lowest free number.
40 constant FMT-DEEP-LEVELS

: FMT-TEST-REMOVE-TREE-DEPTH-FDS ( -- )
   FMT-DEEP-LEVELS FMT-DEEP$ FS:MAKE-DIRS
   FMT-DEEP-LEVELS 1 - FMT-DEEP$ FMT-MODE-0555 FS:CHMOD-MODE
   FMT-NEXT-FD {: before:n :}
   [: FMT-TREE FS:REMOVE-TREE ;] catch FS:ERRNO {: code:n cause:n :}
   FMT-DEEP-LEVELS 1 - FMT-DEEP$ FMT-MODE-0755 FS:CHMOD-MODE
   s" a deep FS:REMOVE-TREE refused below a locked level is E-FS-IO, EACCES" T-LABEL
   code E-FS-IO T=
   cause FS:EACCES T=
   s" a refused FS:REMOVE-TREE closes every descriptor it opened" T-LABEL
   FMT-NEXT-FD before T=
   FMT-TREE FS:REMOVE-TREE ;

package FMT-SPECIAL

variable LISTENER          \ socat's pid, NO-PID once a probe has reaped it
create MKFIFO-PATH PATH-CAP allot
create SOCAT-PATH PATH-CAP allot
variable MKFIFO-U
variable SOCAT-U


: TOOL ( str ^u8 -- n ) {: name:str dst:^ :}
   name dst FIND-EXECUTABLE MATCH option
      none OF
         2 s" fs-mutate-test: required executable missing: " write drop
         2 name write drop 2 S\" \n" write drop
         E-PROC-PATH throw
      ENDOF
      some OF ENDOF
   ;MATCH ;


: FIND-TOOLS ( -- )
   s" mkfifo" MKFIFO-PATH TOOL MKFIFO-U !
   s" socat" SOCAT-PATH TOOL SOCAT-U ! ;


: MAKE-FIFO ( -- )
   PROC-ARGV-RESET
   FMT-TREE-LEAF PROC-ARGV+
   MKFIFO-PATH MKFIFO-U @ >str -1 >FD -1 >FD -1 >FD PROC-RUN-ARGV-IO-RC
   MATCH result
      ok OF drop ENDOF
      err OF drop E-FS-IO throw ENDOF
   ;MATCH ;


\ A real filesystem socket stays bound until the tree has been removed.
\ The peer must not unlink it during shutdown: FS:REMOVE-TREE owns that claim.
: START-LISTENER ( -- )
   SB-RESET s" UNIX-LISTEN:" SB-APPEND FMT-TREE-ROOT-FILE SB-APPEND
   s" ,unlink-close=0" SB-APPEND
   PROC-ARGV-RESET
   SB$ PROC-ARGV+
   s" /dev/null" PROC-ARGV+
   SOCAT-PATH SOCAT-U @ >str -1 >FD -1 >FD -1 >FD PROC-SPAWN-ARGV-IO
   LISTENER ! ;


: STOP-LISTENER ( -- )
   LISTENER @ 0 > if
      LISTENER @ PROCESS:SIGKILL PROCESS:KILL-RAW drop
      LISTENER @ PROCESS:WAIT drop
   then ;


\ A zero-window probe; it reaps a listener that has ended.
: LISTENER-ENDED? ( -- bool )
   LISTENER @ 0 >MS PROCESS:WAIT-BOUNDED MATCH PROCESS:ending
      timeout OF false ENDOF
      exited OF drop PROCESS:NO-PID LISTENER ! true ENDOF
      signaled OF drop PROCESS:NO-PID LISTENER ! true ENDOF
   ;MATCH ;


\ A listener that ends without binding is E-FS-IO.
: WAIT-LISTENER ( -- )
   begin
      FMT-TREE-ROOT-FILE FS:TRY-LSTAT if
         FS:STAT-MODE@ FS:S-IFMT and $C000 = if exit then
      then
      LISTENER-ENDED? if E-FS-IO throw then
      NULL-PTR 0 10 poll drop
   again ;


: REMOVE-SPECIAL-TREE ( -- )
   WAIT-LISTENER
   s" the child bound a real Unix socket" T-LABEL
   FMT-TREE-ROOT-FILE FS:STAT-MODE FS:S-IFMT and $C000 T=
   s" the nested inode is a FIFO" T-LABEL
   FMT-TREE-LEAF FS:STAT-MODE FS:S-IFMT and FMT-S-IFIFO T=
   FMT-TREE FS:REMOVE-TREE ;


public

: RUN ( -- )
   FIND-TOOLS
   FMT-TREE-GRAND FS:MAKE-DIRS
   MAKE-FIFO
   START-LISTENER
   [: REMOVE-SPECIAL-TREE ;] catch {: rc:n :}
   STOP-LISTENER
   s" FS:REMOVE-TREE unlinks a bound Unix socket and a nested FIFO" T-LABEL
   rc 0 T=
   FMT-TREE FS:EXISTS? TFALSE
   FMT-TREE-ROOT-FILE FS:EXISTS? if FMT-TREE-ROOT-FILE FS:REMOVE-FILE then
   FMT-TREE-LEAF FS:EXISTS? if FMT-TREE-LEAF FS:REMOVE-FILE then
   FMT-TREE FS:REMOVE-TREE ;

;package

\ Each row runs one module word in two tasks at once and asserts what the other
\ task could not disturb. T1 is the witnessed failure, a task's returned temp
\ directory path replaced by another task's: it fails when fs-mutate.f stages
\ that name process-wide. T7 fails when FS:COPY-FILE-STREAM pumps through one
\ process-wide buffer. T4's paths stay registered on purpose:
\ FMT-TEST-CLEANUP's FS:CLEANUP-RUN removes them before the root goes.
package FMT-TASKS
private

$10000 constant STACK-BYTES
STACK-BYTES TASK:TASK WORKER-A
STACK-BYTES TASK:TASK WORKER-B

20 constant ROUNDS
200 constant SWAPS
16 constant REGS

\ Round counters, not flags, so no round clears what the next one sets.
variable A-READY
variable B-DONE
variable T1-BAD
PATH-CAP SPAN-BUFFER: A-TMP
PATH-CAP SPAN-BUFFER: B-TMP
variable A-TMP-U
variable B-TMP-U
PATH-CAP SPAN-BUFFER: A-REG
PATH-CAP SPAN-BUFFER: B-REG

create P-A1 PATH-CAP allot   variable P-A1-U
create P-A2 PATH-CAP allot   variable P-A2-U
create P-B1 PATH-CAP allot   variable P-B1-U
create P-B2 PATH-CAP allot   variable P-B2-U
create P-AW PATH-CAP allot   variable P-AW-U
create P-BW PATH-CAP allot   variable P-BW-U
create P-RA PATH-CAP allot   variable P-RA-U
create P-RAS PATH-CAP allot  variable P-RAS-U
create P-RAF PATH-CAP allot  variable P-RAF-U
create P-RAG PATH-CAP allot  variable P-RAG-U
create P-RB PATH-CAP allot   variable P-RB-U
create P-RBS PATH-CAP allot  variable P-RBS-U
create P-RBF PATH-CAP allot  variable P-RBF-U
create P-RBG PATH-CAP allot  variable P-RBG-U
create P-KEEP PATH-CAP allot variable P-KEEP-U
create P-W5 PATH-CAP allot   variable P-W5-U
create P-W5S PATH-CAP allot  variable P-W5S-U
create P-W5F1 PATH-CAP allot variable P-W5F1-U
create P-W5F2 PATH-CAP allot variable P-W5F2-U
create P-W5D PATH-CAP allot  variable P-W5D-U
create P-W5DS PATH-CAP allot variable P-W5DS-U
create P-W5DF PATH-CAP allot variable P-W5DF-U
create P-CAS PATH-CAP allot  variable P-CAS-U
create P-CAD PATH-CAP allot  variable P-CAD-U
create P-CBS PATH-CAP allot  variable P-CBS-U
create P-CBD PATH-CAP allot  variable P-CBD-U

: A1$ ( -- str ) P-A1 P-A1-U @ >str ;
: A2$ ( -- str ) P-A2 P-A2-U @ >str ;
: B1$ ( -- str ) P-B1 P-B1-U @ >str ;
: B2$ ( -- str ) P-B2 P-B2-U @ >str ;
: AW$ ( -- str ) P-AW P-AW-U @ >str ;
: BW$ ( -- str ) P-BW P-BW-U @ >str ;
: RA$ ( -- str ) P-RA P-RA-U @ >str ;
: RAS$ ( -- str ) P-RAS P-RAS-U @ >str ;
: RAF$ ( -- str ) P-RAF P-RAF-U @ >str ;
: RAG$ ( -- str ) P-RAG P-RAG-U @ >str ;
: RB$ ( -- str ) P-RB P-RB-U @ >str ;
: RBS$ ( -- str ) P-RBS P-RBS-U @ >str ;
: RBF$ ( -- str ) P-RBF P-RBF-U @ >str ;
: RBG$ ( -- str ) P-RBG P-RBG-U @ >str ;
: KEEP$ ( -- str ) P-KEEP P-KEEP-U @ >str ;
: W5$ ( -- str ) P-W5 P-W5-U @ >str ;
: W5S$ ( -- str ) P-W5S P-W5S-U @ >str ;
: W5F1$ ( -- str ) P-W5F1 P-W5F1-U @ >str ;
: W5F2$ ( -- str ) P-W5F2 P-W5F2-U @ >str ;
: W5D$ ( -- str ) P-W5D P-W5D-U @ >str ;
: W5DS$ ( -- str ) P-W5DS P-W5DS-U @ >str ;
: W5DF$ ( -- str ) P-W5DF P-W5DF-U @ >str ;
: CAS$ ( -- str ) P-CAS P-CAS-U @ >str ;
: CAD$ ( -- str ) P-CAD P-CAD-U @ >str ;
: CBS$ ( -- str ) P-CBS P-CBS-U @ >str ;
: CBD$ ( -- str ) P-CBD P-CBD-U @ >str ;
: A-BODY$ ( -- str ) s" task-a wrote this" ;
: B-BODY$ ( -- str ) s" b" ;

: PATHS! ( -- )
   FMT-ROOT$ s" task-a-1.txt" P-A1 FS:JOIN-PATH P-A1-U !
   FMT-ROOT$ s" task-a-2.txt" P-A2 FS:JOIN-PATH P-A2-U !
   FMT-ROOT$ s" task-b-1.txt" P-B1 FS:JOIN-PATH P-B1-U !
   FMT-ROOT$ s" task-b-2.txt" P-B2 FS:JOIN-PATH P-B2-U !
   FMT-ROOT$ s" task-a-atomic.txt" P-AW FS:JOIN-PATH P-AW-U !
   FMT-ROOT$ s" task-b-atomic.txt" P-BW FS:JOIN-PATH P-BW-U !
   FMT-ROOT$ s" rmt-a" P-RA FS:JOIN-PATH P-RA-U !
   RA$ s" sub" P-RAS FS:JOIN-PATH P-RAS-U !
   RA$ s" f.txt" P-RAF FS:JOIN-PATH P-RAF-U !
   RAS$ s" g.txt" P-RAG FS:JOIN-PATH P-RAG-U !
   FMT-ROOT$ s" rmt-b" P-RB FS:JOIN-PATH P-RB-U !
   RB$ s" sub" P-RBS FS:JOIN-PATH P-RBS-U !
   RB$ s" f.txt" P-RBF FS:JOIN-PATH P-RBF-U !
   RBS$ s" g.txt" P-RBG FS:JOIN-PATH P-RBG-U !
   FMT-ROOT$ s" rmt-keep.txt" P-KEEP FS:JOIN-PATH P-KEEP-U !
   FMT-ROOT$ s" w5" P-W5 FS:JOIN-PATH P-W5-U !
   W5$ s" sub" P-W5S FS:JOIN-PATH P-W5S-U !
   W5$ s" f-1.txt" P-W5F1 FS:JOIN-PATH P-W5F1-U !
   W5S$ s" f-2.txt" P-W5F2 FS:JOIN-PATH P-W5F2-U !
   FMT-ROOT$ s" w5-doomed" P-W5D FS:JOIN-PATH P-W5D-U !
   W5D$ s" sub" P-W5DS FS:JOIN-PATH P-W5DS-U !
   W5DS$ s" x.txt" P-W5DF FS:JOIN-PATH P-W5DF-U !
   FMT-ROOT$ s" copy-a-src.bin" P-CAS FS:JOIN-PATH P-CAS-U !
   FMT-ROOT$ s" copy-a-dst.bin" P-CAD FS:JOIN-PATH P-CAD-U !
   FMT-ROOT$ s" copy-b-src.bin" P-CBS FS:JOIN-PATH P-CBS-U !
   FMT-ROOT$ s" copy-b-dst.bin" P-CBD FS:JOIN-PATH P-CBD-U ! ;

: FILE-IS? ( str str -- ) {: path:str body:str :}
   path FMT-READ-BUF 64 FS:READ-ALL body STR:LEN T=
   FMT-READ-BUF body STR:LEN >str body T$= ;

\ A worker's answer: what it returned, which is 0, or the code it threw.
: JOIN-CODE ( ^n -- n )
   TASK:JOIN MATCH result
      ok OF ENDOF
      err OF ENDOF
   ;MATCH ;

\ Joins both workers before reading either; true when both answered 0. A
\ worker's throw ends its peer's wait with the same code, so both may answer it.
: JOIN-BOTH ( -- bool )
   WORKER-A JOIN-CODE WORKER-B JOIN-CODE {: a b :}
   a 0<> if a 0 T= then
   b 0<> if b 0 T= then
   a 0= b 0= and ;

: A-TMP$ ( -- str ) A-TMP A-TMP-U @ SPAN:TAKE SPAN:$ ;
: B-TMP$ ( -- str ) B-TMP B-TMP-U @ SPAN:TAKE SPAN:$ ;

\ A keeps a copy of the path FS:MKTEMP-DIR-IN handed it, waits until B has made one
\ of its own, and only then reads the span it is still holding.
: T1-A-ROUND ( n -- ) {: round :}
   FMT-ROOT$ s" race-a" FS:MKTEMP-DIR-IN {: path:str :}
   path A-TMP SPAN:COPY  path STR:LEN A-TMP-U !
   round A-READY atomic!
   B-DONE round WORKER-B TEST-WAIT:FOR-CELL
   path A-TMP$ STR:= 0= if 1 T1-BAD atomic-add drop then
   A-TMP$ FS:REMOVE-DIR ;

: T1-B-ROUND ( n -- ) {: round :}
   A-READY round WORKER-A TEST-WAIT:FOR-CELL
   FMT-ROOT$ s" race-b" FS:MKTEMP-DIR-IN {: path:str :}
   path B-TMP SPAN:COPY  path STR:LEN B-TMP-U !
   round B-DONE atomic!
   B-TMP$ FS:REMOVE-DIR ;

: T1-A ( -- ) ROUNDS 0 ?do i 1+ T1-A-ROUND loop 0 TASK:RETURN ;
: T1-B ( -- ) ROUNDS 0 ?do i 1+ T1-B-ROUND loop 0 TASK:RETURN ;

: T1-RUN ( -- )
   0 A-READY !  0 B-DONE !  0 T1-BAD !
   ['] T1-A WORKER-A TASK:ACTIVATE
   ['] T1-B WORKER-B TASK:ACTIVATE
   JOIN-BOTH 0= if exit then
   s" another task cannot replace my returned temporary directory path" T-LABEL
   T1-BAD @ 0 T= ;

\ An even round moves the file out, an odd one moves it back, so SWAPS rounds
\ end where they started.
: T2-ROUND ( n str str -- ) {: round p1:str p2:str :}
   round 1 and 0= if p1 p2 FS:RENAME-FILE else p2 p1 FS:RENAME-FILE then ;

: T2-A ( -- ) SWAPS 0 ?do i A1$ A2$ T2-ROUND loop 0 TASK:RETURN ;
: T2-B ( -- ) SWAPS 0 ?do i B1$ B2$ T2-ROUND loop 0 TASK:RETURN ;

: T2-RUN ( -- )
   A1$ A-BODY$ FS:WRITE-ALL
   B1$ B-BODY$ FS:WRITE-ALL
   ['] T2-A WORKER-A TASK:ACTIVATE
   ['] T2-B WORKER-B TASK:ACTIVATE
   JOIN-BOTH 0= if exit then
   s" two tasks renaming at once each moved only their own file" T-LABEL
   A1$ A-BODY$ FILE-IS?
   B1$ B-BODY$ FILE-IS?
   A2$ FS:EXISTS? TFALSE
   B2$ FS:EXISTS? TFALSE
   A1$ FS:REMOVE-FILE
   B1$ FS:REMOVE-FILE ;

: T3-A ( -- ) SWAPS 0 ?do AW$ A-BODY$ FS:ATOMIC-WRITE-FILE loop 0 TASK:RETURN ;
: T3-B ( -- ) SWAPS 0 ?do BW$ B-BODY$ FS:ATOMIC-WRITE-FILE loop 0 TASK:RETURN ;

: T3-RUN ( -- )
   ['] T3-A WORKER-A TASK:ACTIVATE
   ['] T3-B WORKER-B TASK:ACTIVATE
   JOIN-BOTH 0= if exit then
   s" two tasks writing atomically at once each kept their own contents" T-LABEL
   AW$ A-BODY$ FILE-IS?
   BW$ B-BODY$ FILE-IS?
   AW$ FS:REMOVE-FILE
   BW$ FS:REMOVE-FILE ;

\ A replacement keeps the destination's regular mode.
: STAGE-RUN ( -- )
   AW$ s" old" FS:WRITE-ALL
   AW$ FMT-MODE-0640 FS:CHMOD-MODE
   AW$ s" replacement" FS:ATOMIC-WRITE-FILE
   s" the published replacement preserves content and mode" T-LABEL
   AW$ s" replacement" FILE-IS?
   AW$ FS:STAT-MODE FMT-MODE-PERM and FMT-MODE-0640 T=
   AW$ FS:REMOVE-FILE ;

: REG-NAME ( n n SPAN:span<u8> -- str ) {: which i dst :}
   SB-RESET
   FMT-ROOT$ SB-APPEND
   s" /reg-" SB-APPEND
   97 which + SB-APPEND-C
   s" -" SB-APPEND
   i FMT:SB-U
   SB$ {: s:str :}
   s dst SPAN:COPY
   dst s STR:LEN SPAN:TAKE SPAN:$ ;

: T4-REG-ONE ( n n SPAN:span<u8> -- ) {: which i dst :}
   which i dst REG-NAME {: name:str :}
   name s" reg" FS:WRITE-ALL
   name FS:CLEANUP+ ;

: T4-A ( -- ) REGS 0 ?do 0 i A-REG T4-REG-ONE loop 0 TASK:RETURN ;
: T4-B ( -- ) REGS 0 ?do 1 i B-REG T4-REG-ONE loop 0 TASK:RETURN ;

\ A lost registration leaves its file in the root, which FMT-TEST-CLEANUP's run
\ must empty.
: T4-RUN ( -- )
   ['] T4-A WORKER-A TASK:ACTIVATE
   ['] T4-B WORKER-B TASK:ACTIVATE
   JOIN-BOTH drop ;

\ Two tasks remove two trees at once with FS:REMOVE-TREE, and a FS:WALK-FILES
\ callback removes a subtree while its walk stands: each walk owns its context.
variable T6-SEEN
variable T6-BAD

: A-TREE! ( -- )
   RA$ FS:MAKE-DIR  RAS$ FS:MAKE-DIR
   RAF$ A-BODY$ FS:WRITE-ALL  RAG$ A-BODY$ FS:WRITE-ALL ;

: B-TREE! ( -- )
   RB$ FS:MAKE-DIR  RBS$ FS:MAKE-DIR
   RBF$ B-BODY$ FS:WRITE-ALL  RBG$ B-BODY$ FS:WRITE-ALL ;

: T5-A ( -- ) ROUNDS 0 ?do A-TREE! RA$ FS:REMOVE-TREE loop 0 TASK:RETURN ;
: T5-B ( -- ) ROUNDS 0 ?do B-TREE! RB$ FS:REMOVE-TREE loop 0 TASK:RETURN ;

: T5-RUN ( -- )
   KEEP$ s" keep" FS:WRITE-ALL
   ['] T5-A WORKER-A TASK:ACTIVATE
   ['] T5-B WORKER-B TASK:ACTIVATE
   JOIN-BOTH 0= if exit then
   s" two tasks removing trees at once each removed only its own" T-LABEL
   RA$ FS:EXISTS? TFALSE
   RB$ FS:EXISTS? TFALSE
   KEEP$ FS:FILE? TTRUE
   KEEP$ FS:REMOVE-FILE ;

\ The removed tree is not the walked one: a walk holds its directory open, and
\ getdirentries64 on a removed directory's descriptor answers -1, which the walk
\ reports as E-FS-DIR (measured). Two walks in flight cover disjoint trees. The
\ row proves the removal moves no part of the walk's state: every path the
\ callback gets starts at the walked root, and both files are still found.
: UNDER? ( str str -- bool ) {: s:str rs:str :}
   s STR:LEN rs STR:LEN <= if false exit then
   s STR:DATA rs STR:LEN >str rs STR:= ;

: T6-CB ( str -- ) {: s:str :}
   s W5$ UNDER? 0= if 1 T6-BAD +! then
   T6-SEEN @ 0= if W5D$ FS:REMOVE-TREE then
   1 T6-SEEN +! ;

: T6-TREES! ( -- )
   W5$ FS:MAKE-DIR  W5S$ FS:MAKE-DIR
   W5F1$ s" 1" FS:WRITE-ALL  W5F2$ s" 2" FS:WRITE-ALL
   W5D$ FS:MAKE-DIR  W5DS$ FS:MAKE-DIR  W5DF$ s" x" FS:WRITE-ALL ;

: T6-RUN ( -- )
   T6-TREES!
   0 T6-SEEN !  0 T6-BAD !
   W5$ [: T6-CB ;] FS:WALK-FILES
   s" a callback removed a tree, the walk stood" T-LABEL
   T6-BAD @ 0 T=
   T6-SEEN @ 2 T=
   W5D$ FS:EXISTS? TFALSE
   W5F1$ FS:FILE? TTRUE
   W5F2$ FS:FILE? TTRUE
   W5$ FS:REMOVE-TREE
   W5$ FS:EXISTS? TFALSE ;

\ Each source is five 8192-byte copy chunks and an odd remainder. A's byte at i
\ is i mod 251, B's its complement, so the two differ at every offset.
5 8192 * 37 + constant T7-LEN
create T7-A-SRC T7-LEN allot
create T7-B-SRC T7-LEN allot
create T7-A-GOT T7-LEN allot
create T7-B-GOT T7-LEN allot
variable T7-A-BAD
variable T7-B-BAD
variable T7-READY

: T7-FILL ( ^u8 n -- ) {: buf:^ mask :}
   T7-LEN 0 ?do i 251 mod mask xor buf i + c! loop ;

\ Counts a round whose destination is not byte for byte its own source.
: T7-CHECK ( str ^u8 ^u8 ^n -- )
   {: dst:str src:^ got:^ bad:^ :}
   dst got T7-LEN FS:READ-ALL {: u :}
   got u >str src T7-LEN >str STR:= 0= if 1 bad atomic-add drop then ;

: T7-A-ROUND ( -- )
   CAS$ CAD$ FS:COPY-FILE-STREAM
   CAD$ T7-A-SRC T7-A-GOT T7-A-BAD T7-CHECK ;

: T7-B-ROUND ( -- )
   CBS$ CBD$ FS:COPY-FILE-STREAM
   CBD$ T7-B-SRC T7-B-GOT T7-B-BAD T7-CHECK ;

\ Neither worker begins its rounds until both have started, so the copies
\ overlap.
: T7-GO ( ^n -- ) {: peer:^ :}
   1 T7-READY atomic-add drop  T7-READY 2 peer TEST-WAIT:FOR-CELL ;

: T7-A ( -- ) WORKER-B T7-GO ROUNDS 0 ?do T7-A-ROUND loop 0 TASK:RETURN ;
: T7-B ( -- ) WORKER-A T7-GO ROUNDS 0 ?do T7-B-ROUND loop 0 TASK:RETURN ;

: T7-RUN ( -- )
   T7-A-SRC 0 T7-FILL  T7-B-SRC 255 T7-FILL
   CAS$ T7-A-SRC T7-LEN >str FS:WRITE-ALL
   CBS$ T7-B-SRC T7-LEN >str FS:WRITE-ALL
   0 T7-A-BAD !  0 T7-B-BAD !  0 T7-READY !
   ['] T7-A WORKER-A TASK:ACTIVATE
   ['] T7-B WORKER-B TASK:ACTIVATE
   JOIN-BOTH 0= if exit then
   s" two tasks copying files at once each copied only its own bytes" T-LABEL
   T7-A-BAD @ 0 T=
   T7-B-BAD @ 0 T=
   CAS$ FS:REMOVE-FILE  CAD$ FS:REMOVE-FILE
   CBS$ FS:REMOVE-FILE  CBD$ FS:REMOVE-FILE ;

public

: RUN ( -- )
   PATHS!
   T1-RUN
   T2-RUN
   T3-RUN
   STAGE-RUN
   T4-RUN
   T5-RUN
   T6-RUN
   T7-RUN ;

;package

\ A directory and then GROWS files inside it are registered, enough to grow the
\ row several times over; one middle file is forgotten once removed.
\ FS:CLEANUP-RUN must take every file before the directory, a plain rmdir: a
\ slot the growth or the forget's shift lost leaves its file.
package FMT-GROW
private

100 constant GROWS
50 constant GONE

PATH-CAP SPAN-BUFFER: NAME-BUF
PATH-CAP SPAN-BUFFER: DIR-BUF
variable DIR-U

: DIR$ ( -- str ) DIR-BUF DIR-U @ SPAN:TAKE SPAN:$ ;

\ File i's path, valid until the next call.
: NAME$ ( n -- str ) {: i :}
   SB-RESET
   DIR$ SB-APPEND
   s" /f-" SB-APPEND
   i FMT:SB-U
   SB$ {: s:str :}
   s NAME-BUF SPAN:COPY
   NAME-BUF s STR:LEN SPAN:TAKE SPAN:$ ;

public

: REGISTER ( -- )
   SB-RESET
   FMT-ROOT$ SB-APPEND
   s" /grow" SB-APPEND
   SB$ {: s:str :}
   s DIR-BUF SPAN:COPY
   s STR:LEN DIR-U !
   DIR$ FS:MAKE-DIR
   DIR$ FS:CLEANUP-DIR+
   GROWS 0 ?do
      i NAME$ s" grow" FS:WRITE-ALL
      i NAME$ FS:CLEANUP+
   loop
   GONE NAME$ FS:REMOVE-FILE
   GONE NAME$ FS:CLEANUP-FORGET ;

\ After FS:CLEANUP-RUN.
: GONE? ( -- )
   s" FS:CLEANUP-RUN removed every registered file before their directory" T-LABEL
   DIR$ FS:EXISTS? TFALSE ;

;package

\ The worker registers once the run removes the marker, its newest slot. Without
\ FS:CLEANUP-RUN's hold, a run over FILES entries reads freed rows: SIGSEGV.
package FMT-RACE
private

1000 constant FILES
16384 constant REGS

create P-MARK PATH-CAP allot  variable P-MARK-U
create P-NONE PATH-CAP allot  variable P-NONE-U
create P-NAME PATH-CAP allot
$10000 TASK:TASK WORKER

variable DONE

: MARK$ ( -- str ) P-MARK P-MARK-U @ >str ;
: NONE$ ( -- str ) P-NONE P-NONE-U @ >str ;

\ File i's path, valid until the next call.
: NAME$ ( n -- str ) {: i :}
   SB-RESET
   s" f-" SB-APPEND
   i FMT:SB-U
   FMT-ROOT$ SB$ P-NAME FS:JOIN-PATH {: u :}
   P-NAME u >str ;

: PATHS! ( -- )
   FMT-ROOT$ s" mark" P-MARK FS:JOIN-PATH P-MARK-U !
   FMT-ROOT$ s" none" P-NONE FS:JOIN-PATH P-NONE-U ! ;

\ DONE ends the wait when the run returns without taking the marker.
: WORK ( -- )
   begin MARK$ FS:EXISTS? DONE atomic@ 0= and while repeat
   REGS 0 ?do NONE$ FS:CLEANUP+ loop
   0 TASK:RETURN ;

: JOIN-CODE ( -- n )
   WORKER TASK:JOIN MATCH result
      ok OF ENDOF
      err OF ENDOF
   ;MATCH ;

public

: RUN ( -- )
   PATHS!
   FMT-ROOT$ FS:MAKE-DIRS
   FMT-ROOT$ FS:CLEANUP-TREE+
   FILES 0 ?do i NAME$ s" race" FS:WRITE-ALL loop
   MARK$ s" race" FS:WRITE-ALL
   MARK$ FS:CLEANUP+
   0 DONE atomic!
   ['] WORK WORKER TASK:ACTIVATE
   [: FS:CLEANUP-RUN ;] catch {: code :}
   1 DONE atomic!
   JOIN-CODE {: worker :}
   s" the run removed its tree while another task grew the registry" T-LABEL
   code 0 T=
   worker 0 T=
   FMT-ROOT$ FS:EXISTS? TFALSE
   s" the worker's registrations wait for a later run" T-LABEL
   FMT-ROOT$ FS:MAKE-DIRS
   NONE$ s" race" FS:WRITE-ALL
   FS:CLEANUP-RUN
   NONE$ FS:EXISTS? TFALSE
   FMT-ROOT$ FS:REMOVE-TREE ;

;package

\ The root's tree slot is forgotten just before the registry runs, so the
\ registry removes every fixture one by one and the root must then be empty: an
\ rmdir that fails is a fixture nothing registered. A throw before that still
\ leaves the root to the exit cleanup. The root goes as a tree afterwards
\ either way.
: FMT-TEST-CLEANUP ( -- )
   FMT-WRITE-TREE
   FMT-TREE FS:CLEANUP-TREE+
   FMT-RENAME-DST FS:CLEANUP+
   FMT-CHMOD-PATH FS:CLEANUP+
   FMT-COPY-SRC FS:CLEANUP+
   FMT-COPY-DST FS:CLEANUP+
   FMT-STREAM-SRC FS:CLEANUP+
   FMT-STREAM-DST FS:CLEANUP+
   FMT-ATOMIC-PATH FS:CLEANUP+
   FMT-MKDIR-PATH FS:CLEANUP-DIR+
   FMT-NEST-A FS:CLEANUP-TREE+
   FMT-LINK-DIR FS:CLEANUP-TREE+
   FMT-GROW:REGISTER
   FMT-ROOT$ FS:CLEANUP-FORGET
   s" habu-fs-mkf" FS:MKTEMP {: path:str file:fd :}
   file FD>N FS:CLOSE-FD 0 T=
   path FMT-COLLIDE-BUF FMT-COLLIDE-U FMT-COPY!
   [: FS:CLEANUP-RUN ;] catch 0 T=
   s" FS:MKTEMP registers nothing for the run to remove" T-LABEL
   FMT-COLLIDE FS:FILE? dup TTRUE if FMT-COLLIDE FS:REMOVE-FILE then
   s" a staged sibling forgotten after its callback threw outlives the run" T-LABEL
   FMT-STAGED-FORGOT FS:FILE? dup TTRUE if FMT-STAGED-FORGOT FS:REMOVE-FILE then
   FMT-GROW:GONE?
   FMT-COPY-DST FS:EXISTS? TFALSE
   FMT-ATOMIC-PATH FS:EXISTS? TFALSE
   FMT-STAGED-TMP FS:EXISTS? TFALSE
   FMT-MKDIR-PATH FS:EXISTS? TFALSE
   FMT-NEST-A FS:EXISTS? TFALSE
   FMT-LINK-DIR FS:EXISTS? TFALSE
   FMT-TREE FS:EXISTS? TFALSE
   [: FMT-ROOT$ FS:REMOVE-DIR ;] catch 0 T=
   FMT-ROOT$ FS:CLEANUP-TREE+
   FS:CLEANUP-RUN ;

: FS-MUTATE-TEST-MAIN ( -- )
   T-RESET
   FMT-PREPARE
   FMT-REMOVE-PATH FS:FILE? TTRUE
   FMT-REMOVE-PATH FS:REMOVE-FILE
   FMT-REMOVE-PATH FS:EXISTS? TFALSE
   FMT-RENAME-SRC FS:FILE? TTRUE
   FMT-RENAME-DST FS:EXISTS? TFALSE
   FMT-RENAME-SRC FMT-RENAME-DST FS:RENAME-FILE
   FMT-RENAME-SRC FS:EXISTS? TFALSE
   FMT-RENAME-DST FS:FILE? TTRUE
   FMT-CHMOD-PATH FS:CHMOD-X
   FMT-CHMOD-PATH FS:STAT-MODE FMT-MODE-EXEC and FMT-MODE-EXEC = TTRUE
   FMT-CHMOD-PATH FS:MODE-PRIVATE-DIR FS:CHMOD-MODE
   FMT-CHMOD-PATH FS:STAT-MODE FMT-MODE-PERM and FS:MODE-PRIVATE-DIR T=
   FS-TEST:HARD-LINK
   FS-TEST:CREATE-NEW
   FS-TEST:OPEN-RDWR-NOFOLLOW
   FMT-TEST-COPY
   FMT-TEST-NUL-SECOND-PATH
   FMT-TEST-STREAM-REFUSED-DST
   FMT-TEST-STREAM-COPY
   FMT-TEST-ATOMIC-WRITE
   FMT-TEST-ATOMIC-ALIASES
   FMT-TEST-ATOMIC-LINK-MODE
   FMT-TEST-ATOMIC-DANGLING-LINK
   FMT-TEST-ATOMIC-RELATIVE
   FMT-TEST-ATOMIC-PARENT-LINK
   FMT-TEST-STAGED-CONTEXT
   FMT-TEST-STAGED-MODE
   FMT-TEST-ATOMIC-FAILURE
   FS-TEST:ATOMIC-SEARCH-DIR
   FS-TEST:ATOMIC-PRIVATE
   FS-TEST:ATOMIC-CREATE
   FS-TEST:CREATE-MANY
   FS-TEST:CREATE-MANY-THROW
   FMT-TEST-STAGED-PRIMARY
   FMT-TEST-DIRS
   FMT-TEST-DIRS-DENIED
   FS-TEST:DIRS-NAME-TOO-LONG
   250 FS-TEST:ATOMIC-NAMED
   255 FS-TEST:ATOMIC-NAMED
   FS-TEST:ATOMIC-NAME-TOO-LONG
   FS-TEST:ATOMIC-PATH-CAP
   FMT-TEST-TEMPS
   FMT-TEST-TEMP-SLASH
   FMT-TEST-MKTEMP
   FMT-TMPDIR:RUN
   FMT-TEST-SYMLINK
   FMT-TEST-BROKEN-SYMLINK
   s" removing a missing file is E-FS-IO" T-LABEL
   [: FMT-REMOVE-MISSING ;] E-FS-IO TTHROWSQ
   s" renaming a missing file is E-FS-IO" T-LABEL
   [: FMT-RENAME-MISSING ;] E-FS-IO TTHROWSQ
   s" making a missing file executable is E-FS-STAT" T-LABEL
   [: FMT-CHMOD-MISSING ;] E-FS-STAT TTHROWSQ
   s" setting a missing file's mode is E-FS-IO" T-LABEL
   [: FMT-CHMOD-MODE-MISSING ;] E-FS-IO TTHROWSQ
   s" making a directory that exists is E-FS-IO" T-LABEL
   [: FMT-MKDIR-EXISTS ;] E-FS-IO TTHROWSQ
   FMT-LONG!
   s" a cleanup path past PATH-CAP is E-FS-PATH" T-LABEL
   [: FMT-CLEANUP-PATH-TOO-LONG ;] E-FS-PATH TTHROWSQ
   s" an atomic target join past PATH-CAP is E-FS-CAPACITY" T-LABEL
   [: FS-TEST:ATOMIC-TARGET-NO-ROOM ;] E-FS-CAPACITY TTHROWSQ
   s" a suffixed path past its span is E-FS-CAPACITY" T-LABEL
   [: FMT-SUFFIX-NO-ROOM ;] E-FS-CAPACITY TTHROWSQ
   s" a temp base with no room for the name is E-FS-CAPACITY" T-LABEL
   [: FMT-TEMP-BASE-NO-ROOM ;] E-FS-CAPACITY TTHROWSQ
   s" removing a tree at an empty path is E-FS-PATH" T-LABEL
   [: FMT-REMOVE-TREE-EMPTY ;] E-FS-PATH TTHROWSQ
   T-LABEL-CLEAR
   FMT-TEST-REMOVE-TREE
   FMT-SPECIAL:RUN
   FMT-TEST-REMOVE-TREE-SYMLINK-DIR
   FMT-TEST-REMOVE-TREE-SLASHED-LINK
   FMT-TEST-REMOVE-TREE-DANGLING
   FMT-TEST-REMOVE-TREE-NOTDIR
   FMT-TEST-RMDIR-ERRNO
   FMT-TEST-REMOVE-TREE-DEPTH-FDS
   FMT-TASKS:RUN
   FMT-TEST-CLEANUP
   FMT-RACE:RUN
   T-REPORT
   s" fs-mutate-test: ok" type cr ;

FS-MUTATE-TEST-MAIN
