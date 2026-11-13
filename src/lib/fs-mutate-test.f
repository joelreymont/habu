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
require src/lib/test/wait.f                         \ a worker's deadline at the join
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

: FMT-LONG$ ( -- ptr u8 n )
   FMT-LONG-BUF PATH-CAP 1 + ;

: FMT-FULL$ ( -- ptr u8 n )
   FMT-LONG-BUF PATH-CAP ;

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
$16D constant FMT-MODE-0555
13 constant FMT-EACCES                  \ a denied mkdir's errno on both targets
20 constant FMT-ENOTDIR                 \ ENOTDIR on both targets
$1000 constant FMT-S-IFIFO              \ S_IFIFO on both targets
$FFF constant FMT-MODE-PERM             \ st_mode's permission bits
$49 constant FMT-MODE-EXEC              \ 0111, execute for all
$1ED constant FMT-MODE-0755
create FMT-STREAM-SRC-DATA FMT-STREAM-LEN allot
create FMT-STREAM-DST-DATA FMT-STREAM-LEN allot

: FMT-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u dst:ptr up:ptr :}
   u PATH-CAP > if E-FS-PATH throw then
   a dst u BYTE-COPY
   u up ! ;

: FMT-ROOT$ ( -- ptr u8 n )
   FMT-ROOT-BUF FMT-ROOT-U @ ;

: FMT-REMOVE-PATH ( -- ptr u8 n )
   FMT-REMOVE-BUF FMT-REMOVE-U @ ;

: FMT-RENAME-SRC ( -- ptr u8 n )
   FMT-RENAME-SRC-BUF FMT-RENAME-SRC-U @ ;

: FMT-RENAME-DST ( -- ptr u8 n )
   FMT-RENAME-DST-BUF FMT-RENAME-DST-U @ ;

: FMT-CHMOD-PATH ( -- ptr u8 n )
   FMT-CHMOD-BUF FMT-CHMOD-U @ ;

: FMT-COPY-SRC ( -- ptr u8 n )
   FMT-COPY-SRC-BUF FMT-COPY-SRC-U @ ;

: FMT-COPY-DST ( -- ptr u8 n )
   FMT-COPY-DST-BUF FMT-COPY-DST-U @ ;

: FMT-STREAM-SRC ( -- ptr u8 n )
   FMT-STREAM-SRC-BUF FMT-STREAM-SRC-U @ ;

: FMT-STREAM-DST ( -- ptr u8 n )
   FMT-STREAM-DST-BUF FMT-STREAM-DST-U @ ;

: FMT-ATOMIC-PATH ( -- ptr u8 n )
   FMT-ATOMIC-BUF FMT-ATOMIC-U @ ;

: FMT-PARENT-A ( -- ptr u8 n )
   FMT-PARENT-A-BUF FMT-PARENT-A-U @ ;

: FMT-PARENT-B ( -- ptr u8 n )
   FMT-PARENT-B-BUF FMT-PARENT-B-U @ ;

: FMT-PARENT-LINK ( -- ptr u8 n )
   FMT-PARENT-LINK-BUF FMT-PARENT-LINK-U @ ;

: FMT-PARENT-BOUND ( -- ptr u8 n )
   FMT-PARENT-BOUND-BUF FMT-PARENT-BOUND-U @ ;

: FMT-MKDIR-PATH ( -- ptr u8 n )
   FMT-MKDIR-BUF FMT-MKDIR-U @ ;

: FMT-NEST-A ( -- ptr u8 n )
   FMT-NEST-A-BUF FMT-NEST-A-U @ ;

: FMT-NEST-B ( -- ptr u8 n )
   FMT-NEST-B-BUF FMT-NEST-B-U @ ;

: FMT-NEST-C ( -- ptr u8 n )
   FMT-NEST-C-BUF FMT-NEST-C-U @ ;

: FMT-TREE ( -- ptr u8 n )
   FMT-TREE-BUF FMT-TREE-U @ ;

: FMT-TREE-CHILD ( -- ptr u8 n )
   FMT-TREE-CHILD-BUF FMT-TREE-CHILD-U @ ;

: FMT-TREE-GRAND ( -- ptr u8 n )
   FMT-TREE-GRAND-BUF FMT-TREE-GRAND-U @ ;

: FMT-TREE-DOTS ( -- ptr u8 n )
   FMT-TREE-DOTS-BUF FMT-TREE-DOTS-U @ ;

: FMT-TREE-ROOT-FILE ( -- ptr u8 n )
   FMT-TREE-ROOT-FILE-BUF FMT-TREE-ROOT-FILE-U @ ;

: FMT-TREE-LEAF ( -- ptr u8 n )
   FMT-TREE-LEAF-BUF FMT-TREE-LEAF-U @ ;

: FMT-TREE-DOTS-FILE ( -- ptr u8 n )
   FMT-TREE-DOTS-FILE-BUF FMT-TREE-DOTS-FILE-U @ ;

: FMT-LINK-TARGET ( -- ptr u8 n )
   FMT-LINK-TARGET-BUF FMT-LINK-TARGET-U @ ;

: FMT-LINK ( -- ptr u8 n )
   FMT-LINK-BUF FMT-LINK-U @ ;

: FMT-BROKEN-TARGET ( -- ptr u8 n )
   FMT-BROKEN-TARGET-BUF FMT-BROKEN-TARGET-U @ ;

: FMT-BROKEN-LINK ( -- ptr u8 n )
   FMT-BROKEN-LINK-BUF FMT-BROKEN-LINK-U @ ;

: FMT-LINK-DIR ( -- ptr u8 n )
   FMT-LINK-DIR-BUF FMT-LINK-DIR-U @ ;

: FMT-LINK-DIR-FILE ( -- ptr u8 n )
   FMT-LINK-DIR-FILE-BUF FMT-LINK-DIR-FILE-U @ ;

: FMT-TREE-DIR-LINK ( -- ptr u8 n )
   FMT-TREE-DIR-LINK-BUF FMT-TREE-DIR-LINK-U @ ;

: FMT-COLLIDE ( -- ptr u8 n )
   FMT-COLLIDE-BUF FMT-COLLIDE-U @ ;

: FMT-TEMP-BASE ( -- ptr u8 n )
   FMT-ROOT$ ;

\ The root prefers HB_SOCK_TMP to TMPDIR: REMOVE-SPECIAL-TREE binds a Unix
\ socket at <root>/remove-tree/root.txt, sun_path holds 107 bytes on Linux and
\ 103 on macOS, and a pool slot's TMPDIR alone is 97 bytes (measured: the
\ listener never binds, WAIT-LISTENER throws E-FS-IO). HB_SOCK_TMP is the short
\ directory the gate pool makes per slot and removes when the slot retires or is
\ killed (test/gate-pool.f GT-POOL-CHILD-SOCK!). Without it the root goes under
\ TMPDIR, then /tmp. FMT-PREPARE registers the root as a tree, so a throw before
\ FMT-TEST-CLEANUP still removes it at exit.
: FMT-ROOT-COPY! ( ptr u8 n -- ) {: a:ptr u :}
   a FMT-ROOT-BUF u BYTE-COPY
   u FMT-ROOT-U ! ;

: FMT-ROOT! ( -- )
   s" HB_SOCK_TMP" GETENV dup 0 > if FMT-ROOT-COPY! exit then
   2drop
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

: FMT-REAL ( ptr u8 n -- ptr u8 n )
   FS:>PATHZ FMT-REAL-BUF FS:PATHZ-CAP realpath {: u:n :}
   u 0 > TTRUE
   FMT-REAL-BUF u ;


\ A real chain whose canonical spelling is n bytes, for a test whose subject is
\ that length. Its first name is registered for cleanup.
: FMT-DEEP-DIR ( n -- ptr u8 n )
   {: n:n :}
   FMT-ROOT$ FMT-REAL nip {: r:n :}
   n r ?do
      i r - 201 mod 0= i n 1- < and if FS:SLASH else 97 then
      FMT-REAL-BUF i + c!
   loop
   FMT-REAL-BUF n FS:MAKE-DIRS
   FMT-REAL-BUF r 201 + FS:CLEANUP-TREE+
   FMT-REAL-BUF n ;


\ A base one byte past the longest FS:MKTEMP-DIR-IN builds in for its prefix,
\ whatever seed it would draw.
: FMT-TEMP-BASE-NO-ROOM ( -- )
   FMT-LONG-BUF s" nr" nip FS:TEMP-DIR-BASE-MAX 1+ s" nr" FS:MKTEMP-DIR-IN 2drop ;

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
   FMT-READ-BUF 11 s" link-target" T$=
   FMT-LINK-TARGET FS:REMOVE-FILE ;
;package

: FMT-TEST-COPY ( -- )
   FMT-COPY-SRC FMT-COPY-DST FS:COPY-FILE-STREAM
   FMT-COPY-DST FS:FILE? TTRUE
   FMT-COPY-DST FMT-READ-BUF 64 FS:READ-ALL 8 T=
   FMT-READ-BUF 8 s" copy-src" T$= ;

\ The copy destination followed by a NUL byte.
: FMT-NUL-DST$ ( -- ptr u8 n )
   FMT-COPY-DST {: dst:ptr size:n :}
   dst FMT-LONG-BUF size BYTE-COPY
   0 FMT-LONG-BUF size + c!
   FMT-LONG-BUF BYTE-VIEW size 1+ ;

: FMT-NUL-SECOND-COPY ( -- )
   FMT-COPY-SRC FMT-NUL-DST$ FS:COPY-FILE-STREAM ;

: FMT-TEST-NUL-SECOND-PATH ( -- )
   [: FMT-NUL-SECOND-COPY ;] E-FFI-NUL TTHROWSQ
   FMT-COPY-SRC FS:FILE? TTRUE
   FMT-COPY-DST FMT-READ-BUF 64 FS:READ-ALL 8 T=
   FMT-READ-BUF 8 s" copy-src" T$= ;

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
   FMT-STREAM-SRC FMT-STREAM-SRC-DATA FMT-STREAM-LEN FS:WRITE-ALL
   FMT-STREAM-SRC FMT-STREAM-DST FS:COPY-FILE-STREAM
   FMT-STREAM-DST FS:FILE? TTRUE
   FMT-STREAM-DST FS:FILE-SIZE FMT-STREAM-LEN T=
   FMT-STREAM-DST FMT-STREAM-DST-DATA FMT-STREAM-LEN FS:READ-ALL FMT-STREAM-LEN T=
   FMT-STREAM-DST-DATA FMT-STREAM-LEN FMT-STREAM-SRC-DATA FMT-STREAM-LEN T$= ;

: FMT-TEST-ATOMIC-WRITE ( -- )
   FMT-ATOMIC-PATH s" old" FS:WRITE-ALL
   FMT-ATOMIC-PATH s" new-data" FS:ATOMIC-WRITE-FILE
   FMT-ATOMIC-PATH FMT-READ-BUF 64 FS:READ-ALL 8 T=
   FMT-READ-BUF 8 s" new-data" T$= ;

: FMT-TEST-ATOMIC-ALIASES ( -- )
   SB-RESET s" builder-source" SB-APPEND
   FMT-ATOMIC-PATH SB$ FS:ATOMIC-WRITE-FILE
   FMT-ATOMIC-PATH FMT-READ-BUF 64 FS:READ-ALL 14 T=
   FMT-READ-BUF 14 s" builder-source" T$=
   SB-RESET FMT-ATOMIC-PATH SB-APPEND
   SB$ s" builder-target" FS:ATOMIC-WRITE-FILE
   SB$ FMT-ATOMIC-PATH T$=
   FMT-ATOMIC-PATH FMT-READ-BUF 64 FS:READ-ALL 14 T=
   FMT-READ-BUF 14 s" builder-target" T$=
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
   FMT-READ-BUF 13 s" through-links" T$=
   FMT-ATOMIC-PATH FMT-READ-BUF 64 FS:READ-ALL 13 T=
   FMT-READ-BUF 13 s" through-links" T$=
   FMT-ATOMIC-PATH FS:STAT-MODE FMT-MODE-PERM and FMT-MODE-0600 T=
   FMT-BROKEN-LINK FS:REMOVE-FILE
   FMT-LINK FS:REMOVE-FILE ;

: FMT-TEST-ATOMIC-DANGLING-LINK ( -- )
   s" no-such-link-target.txt" FMT-BROKEN-LINK FS:MAKE-SYMLINK
   [: FMT-BROKEN-LINK s" refused" FS:ATOMIC-WRITE-FILE ;] E-FS-STAT TTHROWSQ
   FMT-BROKEN-LINK FS:SYMLINK? TTRUE
   FMT-BROKEN-TARGET FS:EXISTS? TFALSE
   FMT-BROKEN-LINK FS:REMOVE-FILE ;

: FMT-PARENT-OUTPUT ( -- ptr u8 n )
   SB-RESET FMT-PARENT-LINK SB-APPEND s" /entry.f" SB-APPEND SB$ ;

: FMT-PARENT-B-ENTRY ( -- ptr u8 n )
   SB-RESET FMT-PARENT-B SB-APPEND s" /entry.f" SB-APPEND SB$ ;

: FMT-PARENT-A-ENTRY ( -- ptr u8 n )
   SB-RESET FMT-PARENT-A SB-APPEND s" /entry.f" SB-APPEND SB$ ;

: FMT-PARENT-FILL ( ptr u8 n -- )
   s" published" FS:WRITE-ALL ;

: FMT-STAGED-TMP ( -- ptr u8 n )
   FMT-STAGED-TMP-BUF FMT-STAGED-TMP-U @ ;

\ The fill's own refusal remains the error when removing its sibling also
\ fails. Replacing the reserved file with a directory makes unlink refuse.
: FMT-STAGED-CLEAN-FAIL ( ptr u8 n -- )
   2dup FMT-STAGED-TMP-BUF FMT-STAGED-TMP-U FMT-COPY!
   2dup FS:REMOVE-FILE
   FS:MAKE-DIR
   E-FS-CAPACITY throw ;

\ A diagnostic line's class: the code's declared name, else its number.
: FMT-CLASS$ ( n -- ptr u8 n ) {: code :}
   code ERROR:NAME$ {: name:ptr nameu :}
   nameu 0= if code FMT:INT$ exit then
   name nameu ;

: FMT-ERR-PATH ( -- ptr u8 n )
   FMT-ROOT$ s" stderr.txt" FMT-ERR-PATH-BUF FS:JOIN-PATH {: u:n :}
   FMT-ERR-PATH-BUF u ;

: FMT-STAGED-CAPTURED ( -- )
   FMT-ERR-PATH STDERR:CAPTURE
   [: FMT-ATOMIC-PATH [: FMT-STAGED-CLEAN-FAIL ;] FS:REPLACE-STAGED ;]
   [: STDERR:;CAPTURE ;] finally ;

: FMT-STAGED-WANT ( -- ptr u8 n )
   SB-RESET
   E-FS-IO FMT-CLASS$ SB-APPEND
   s"  habu: " SB-APPEND
   FMT-STAGED-TMP SB-APPEND
   s" : staged replacement cleanup failed, errno " SB-APPEND
   SB$ ;

\ The errno's digits sit between the fixed text and the line's end.
: FMT-STAGED-LINE ( -- )
   FMT-ERR-PATH FMT-ERR-BUF FMT-ERR-CAP FS:READ-ALL {: u:n :}
   FMT-STAGED-WANT {: want:ptr wantu:n :}
   u wantu 1+ > TTRUE
   FMT-ERR-BUF wantu want wantu T$=
   FMT-ERR-BUF u + 1- c@ 10 T=
   FMT-ERR-BUF wantu + u wantu - 1- STR>NUMBER? MATCH option
      none OF false TTRUE ENDOF
      some OF 0 > TTRUE ENDOF
   ;MATCH
   FMT-ERR-PATH FS:REMOVE-FILE ;

: FMT-TEST-STAGED-PRIMARY ( -- )
   FMT-ATOMIC-PATH s" previous" FS:WRITE-ALL
   s" staged fill error survives cleanup refusal" T-LABEL
   [: FMT-STAGED-CAPTURED ;] E-FS-CAPACITY TTHROWSQ
   FMT-ATOMIC-PATH FMT-READ-BUF 64 FS:READ-ALL {: u:n :}
   FMT-READ-BUF u s" previous" T$=
   s" the cleanup refusal's line names its operation and errno" T-LABEL
   FMT-STAGED-LINE
   FMT-STAGED-TMP FS:DIR? TTRUE
   FMT-STAGED-TMP FS:REMOVE-DIR
   FMT-STAGED-TMP s" removable" FS:WRITE-ALL
   FMT-ATOMIC-PATH FS:REMOVE-FILE ;

: FMT-STAGED-SPAN ( SPAN:span<u8> ptr u8 n -- SPAN:span<u8> )
   {: data path:ptr pathu:n :}
   path pathu data SPAN:$ FS:WRITE-ALL
   data ;

: FMT-STAGED-FAIL ( SPAN:span<u8> ptr u8 n -- SPAN:span<u8> )
   {: data path:ptr pathu:n :}
   path pathu FMT-STAGED-FORGOT-BUF FMT-STAGED-FORGOT-U FMT-COPY!
   path pathu data SPAN:$ FS:WRITE-ALL
   E-FS-IO throw ;

: FMT-STAGED-FORGOT ( -- ptr u8 n )
   FMT-STAGED-FORGOT-BUF FMT-STAGED-FORGOT-U @ ;

: FMT-TEST-STAGED-CONTEXT ( -- )
   s" staged callbacks preserve a complete layout value" T-LABEL
   s" context" SPAN:MAKE FMT-ATOMIC-PATH [: FMT-STAGED-SPAN ;] FS:REPLACE-STAGED
   SPAN:$ s" context" T$=
   FMT-ATOMIC-PATH FMT-READ-BUF 64 FS:READ-ALL 7 T=
   FMT-READ-BUF 7 s" context" T$=
   s" a throwing callback preserves the old file and releases its sibling" T-LABEL
   [: s" partial" SPAN:MAKE FMT-ATOMIC-PATH
      [: FMT-STAGED-FAIL ;] FS:REPLACE-STAGED SPAN:$ 2drop ;] E-FS-IO TTHROWSQ
   FMT-ATOMIC-PATH FMT-READ-BUF 64 FS:READ-ALL 7 T=
   FMT-READ-BUF 7 s" context" T$=
   FMT-STAGED-FORGOT FS:EXISTS? TFALSE
   \ No slot names the sibling now: FMT-TEST-CLEANUP's run must leave this file.
   FMT-STAGED-FORGOT s" unowned" FS:WRITE-ALL
   s" a staged callback cannot consume its caller's context" T-LABEL
   s" FMT-BAD-STAGED ( n ptr u8 n -- ) [: 2drop drop ;] FS:REPLACE-STAGED"
      CHECK-QUIET-CANDIDATE! 0 T= ;

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
   FMT-READ-BUF 9 s" published" T$=
   FMT-PARENT-B-ENTRY FMT-READ-BUF 64 FS:READ-ALL 6 T=
   FMT-READ-BUF 6 s" source" T$=
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

: DROP-BOX$ ( -- ptr u8 n )
   DROP-BOX-BUF DROP-BOX-U @ ;

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
   FMT-READ-BUF 7 s" dropped" T$=
   DROP-BOX$ FS:REMOVE-FILE
   FMT-MKDIR-PATH FS:REMOVE-DIR ;
;package

package FS-TEST
create PRIVATE-DIR-BUF PATH-CAP allot
variable PRIVATE-DIR-U
create PRIVATE-OUT-BUF PATH-CAP allot
variable PRIVATE-OUT-U

: PRIVATE-DIR$ ( -- ptr u8 n ) PRIVATE-DIR-BUF PRIVATE-DIR-U @ ;
: PRIVATE-OUT$ ( -- ptr u8 n ) PRIVATE-OUT-BUF PRIVATE-OUT-U @ ;


\ The destination holds exactly body at mode.
: PRIVATE-IS ( ptr u8 n n -- )
   {: body:ptr bodyu mode :}
   PRIVATE-OUT$ FMT-READ-BUF 64 FS:READ-ALL bodyu T=
   FMT-READ-BUF bodyu body bodyu T$=
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
: NAMED-DIR$ ( -- ptr u8 n )
   FMT-ROOT$ s" named" NAMED-DIR-BUF FS:JOIN-PATH NAMED-DIR-BUF swap ;

: NAMED$ ( n -- ptr u8 n )
   {: n:n :}
   NAMED-DIR$ {: dir:ptr diru:n :}
   dir NAMED-BUF diru BYTE-COPY
   FS:SLASH NAMED-BUF diru + c!
   diru 1+ n + diru 1+ ?do 110 NAMED-BUF i + c! loop
   NAMED-BUF diru 1+ n + ;

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
   FMT-READ-BUF 4 s" long" T$=
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
   least PATH-CAP least - 1 and + FMT-DEEP-DIR {: dir:ptr diru:n :}
   PATH-CAP 2 - diru ?do
      FS:SLASH dir i + c!  [char] . dir i 1+ + c!
   2 +loop
   FS:SLASH dir PATH-CAP 2 - + c!
   [char] x dir PATH-CAP 1- + c!
   dir PATH-CAP s" capped" FS:ATOMIC-WRITE-FILE
   dir PATH-CAP FMT-READ-BUF 64 FS:READ-ALL 6 T=
   FMT-READ-BUF 6 s" capped" T$= ;
;package

: FMT-TEST-TEMPS ( -- )
   FMT-TEMP-BASE s" one" FS:MKTEMP-DIR-IN 2dup FS:DIR? TTRUE FS:CLEANUP-DIR+
   FMT-TEMP-BASE s" two" FS:MKTEMP-DIR-IN 2dup FS:DIR? TTRUE FS:CLEANUP-DIR+
   s" habu-fs-mut" FS:MKTEMP-DIR 2dup FS:DIR? TTRUE FS:CLEANUP-DIR+ ;

\ A base that already ends in a slash, as macOS's TMPDIR does, takes no second
\ one. FMT-TEST-DIRS left FMT-MKDIR-PATH for the base.
: FMT-TEST-TEMP-SLASH ( -- )
   FMT-MKDIR-PATH FMT-COLLIDE-BUF FMT-COLLIDE-U FMT-COPY!
   FS:SLASH FMT-COLLIDE-BUF FMT-COLLIDE-U @ + c!
   FMT-COLLIDE-U @ 1+ FMT-COLLIDE-U !
   FMT-COLLIDE s" slash" FS:MKTEMP-DIR-IN {: temp:ptr tempu:n :}
   temp tempu FS:DIR? TTRUE
   FMT-MKDIR-PATH s" slash-" FMT-UNIQUE-BUF FS:JOIN-PATH {: wantu:n :}
   temp tempu FMT-UNIQUE-BUF wantu STARTS-WITH? TTRUE
   temp tempu FS:REMOVE-DIR ;

: FMT-TEST-MKTEMP ( -- )
   s" FS:MKTEMP makes a private read-write file under the temp root" T-LABEL
   s" habu-fs-mkf" FS:MKTEMP {: path:ptr pathu file:fd :}
   path pathu FMT-COLLIDE-BUF FMT-COLLIDE-U FMT-COPY!
   FMT-COLLIDE FS:CLEANUP+
   s" TMPDIR" GETENV dup 0= if 2drop s" /tmp" then FMT-REAL
   s" habu-fs-mkf-" FMT-UNIQUE-BUF FS:JOIN-PATH {: wantu:n :}
   FMT-COLLIDE FMT-UNIQUE-BUF wantu STARTS-WITH? TTRUE
   FMT-COLLIDE FS:STAT-MODE FMT-MODE-PERM and FMT-MODE-0600 T=
   file FD>N s" mkstemp" write 7 T=
   file FD>N FMT-READ-BUF 64 read 0 T=
   file FD>N FS:CLOSE-FD 0 T=
   FMT-COLLIDE FMT-READ-BUF 64 FS:READ-ALL 7 T=
   FMT-READ-BUF 7 s" mkstemp" T$=
   s" habu-fs-mkf" FS:MKTEMP {: next:ptr nextu nextfd:fd :}
   next nextu FS:CLEANUP+
   nextfd FD>N FS:CLOSE-FD 0 T=
   next nextu FMT-COLLIDE T$<> ;

\ Child engines: TMPDIR is read from the process environment, which a running
\ process does not change for itself. A child whose TMPDIR cannot hold a temp
\ directory or file exits nonzero with one stderr line naming TMPDIR and the
\ value, and the uncaught code. One whose TMPDIR reaches a directory through a
\ symlink (macOS /tmp is /private/tmp) makes its directory under the canonical
\ spelling. Paths and child programs live under this test's root.
package FMT-TMPDIR
private

$1000 constant CAP
15000 constant TIMEOUT-MS
10 constant NL

create OUT CAP allot
create ERR CAP allot
create MISSING-BUF PATH-CAP allot
create PROG-BUF PATH-CAP allot
create FILE-PROG-BUF PATH-CAP allot
create CANON-PROG-BUF PATH-CAP allot
create WANT-BUF PATH-CAP 128 + allot  \ a class, a value, a reason, the newline
variable MISSING-U
variable PROG-U
variable FILE-PROG-U
variable CANON-PROG-U
variable WANT-U

: MISSING$ ( -- ptr u8 n )
   MISSING-BUF MISSING-U @ ;

: PROG$ ( -- ptr u8 n )
   PROG-BUF PROG-U @ ;

: FILE-PROG$ ( -- ptr u8 n )
   FILE-PROG-BUF FILE-PROG-U @ ;

: CANON-PROG$ ( -- ptr u8 n )
   CANON-PROG-BUF CANON-PROG-U @ ;

: PATHS! ( -- )
   FMT-ROOT$ s" no-such-tmpdir" MISSING-BUF FS:JOIN-PATH MISSING-U !
   FMT-ROOT$ s" tmpdir-child.f" PROG-BUF FS:JOIN-PATH PROG-U !
   FMT-ROOT$ s" tmpdir-file.f" FILE-PROG-BUF FS:JOIN-PATH FILE-PROG-U !
   FMT-ROOT$ s" tmpdir-canon.f" CANON-PROG-BUF FS:JOIN-PATH CANON-PROG-U ! ;

: PROG-SRC$ ( -- ptr u8 n )
   S\" require src/lib/fs-mutate.f\ns\q tmpdir-child\q FS:MKTEMP-DIR 2drop\n" ;

: FILE-PROG-SRC$ ( -- ptr u8 n )
   S\" require src/lib/fs-mutate.f\ns\q tmpdir-file\q FS:MKTEMP drop 2drop\n" ;

: CANON-PROG-SRC$ ( -- ptr u8 n )
   S\" require src/lib/fs-mutate.f\n: LINE ( ptr u8 n -- ) {: a:ptr u :} 1 a u write drop 1 S\\\q \\n\q write drop ;\ns\q canon\q FS:MKTEMP-DIR LINE\n" ;

\ A directory one byte past the longest TMPDIR with room for the temp name
\ PROG-SRC$ asks for, whose prefix is tmpdir-child.
: LONG$ ( -- ptr u8 n )
   s" tmpdir-child" nip FS:TEMP-DIR-BASE-MAX 1+ FMT-DEEP-DIR ;

: CHILD-ENV ( ptr u8 n -- ) {: a:ptr u :}
   PROC-ARGV-ENV-RESET
   s" TMPDIR" >LEN a u >LEN PROC-ENV-SET ;

: CAPTURE ( ptr u8 n -- len len PROCESS:ending ) {: prog:ptr progu :}
   PROC-ENV-INHERIT-MISSING
   prog progu >LEN PROC-ARGV+
   ENGINE-CANDIDATE:PATH$ >LEN OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS
   RUN-ARGV-ENV-CAPTURE-OUTCOME ;

: WANT+ ( ptr u8 n -- ) {: a:ptr u :}
   a WANT-BUF WANT-U @ + u BYTE-COPY
   WANT-U @ u + WANT-U ! ;

\ Not built in SB: a value near PATH-CAP bytes with the reason overflows SB-CAP.
: WANT$ ( n ptr u8 n ptr u8 n -- ptr u8 n )
   {: code a:ptr u why:ptr whyu :}
   0 WANT-U !
   code FMT-CLASS$ WANT+
   s"  habu: " WANT+
   a u WANT+
   s" : " WANT+
   why whyu WANT+
   S\" \n" WANT+
   WANT-BUF WANT-U @ ;

: CODE$ ( n -- ptr u8 n )
   SB-RESET
   s" hb: uncaught throw code " SB-APPEND
   FMT:SB-INT
   SB$ ;

: REFUSED ( ptr u8 n ptr u8 n ptr u8 n n -- )
   {: prog:ptr progu a:ptr u why:ptr whyu code:n :}
   a u CHILD-ENV prog progu CAPTURE {: outu:len erru:len oc :}
   oc MATCH PROCESS:ending
     exited OF 0 T<> ENDOF
     signaled OF s" exit" s" signal" T-ENDING-MISS ENDOF
     timeout OF prog progu OUT outu LEN>N ERR erru LEN>N T-TIMED-OUT ENDOF
   ;MATCH
   ERR erru LEN>N code a u why whyu WANT$ CONTAINS? TTRUE
   ERR erru LEN>N code CODE$ CONTAINS? TTRUE ;

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
   CANON-PROG$ OUT outu LEN>N ERR erru LEN>N oc 0 T-ENDING-EXITED=
   OUT outu LEN>N NL 0 SPLIT-NEXT 2drop {: td:ptr tdu:n :}
   td tdu td tdu FMT-REAL T$=
   td tdu FS:REMOVE-DIR
   FMT-PARENT-LINK FS:REMOVE-FILE
   FMT-PARENT-A FS:REMOVE-DIR ;

public

: RUN ( -- )
   PATHS!
   PROG$ PROG-SRC$ FS:WRITE-ALL
   PROG$ FS:CLEANUP+
   FILE-PROG$ FILE-PROG-SRC$ FS:WRITE-ALL
   FILE-PROG$ FS:CLEANUP+
   s" a missing TMPDIR is refused by name" T-LABEL
   PROG$ MISSING$ s" TMPDIR is not a directory" E-FS-DIR REFUSED
   s" FS:MKTEMP refuses a missing TMPDIR by name" T-LABEL
   FILE-PROG$ MISSING$ s" TMPDIR is not a directory" E-FS-DIR REFUSED
   s" a TMPDIR with no room for the temp name is refused by name" T-LABEL
   PROG$ LONG$ s" TMPDIR is too long for a temp path"
   E-FS-CAPACITY REFUSED
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
   FMT-READ-BUF 11 s" link-target" T$=
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
   FMT-READ-BUF 7 s" arrived" T$=
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
: FMT-TREE-SLASH$ ( -- ptr u8 n )
   FMT-TREE {: a:ptr u:n :}
   a FMT-ROOT-PATH-BUF u BYTE-COPY
   FS:SLASH FMT-ROOT-PATH-BUF u + c!
   FMT-ROOT-PATH-BUF u 1 + ;

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
   FMT-ROOT-PATH-BUF u FS:REMOVE-TREE ;

\ kept.txt/: a regular file spelled as a directory.
: FMT-REMOVE-FILE-SLASH ( -- )
   FMT-LINK-DIR-FILE {: a:ptr u:n :}
   a FMT-ROOT-PATH-BUF u BYTE-COPY
   FS:SLASH FMT-ROOT-PATH-BUF u + c!
   FMT-ROOT-PATH-BUF u 1 + FS:REMOVE-TREE ;

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

: FMT-DEEP$ ( n -- ptr u8 n ) {: level:n :}
   FMT-TREE {: a:ptr u:n :}
   a FMT-ROOT-PATH-BUF u BYTE-COPY
   level 0 ?do
      FS:SLASH FMT-ROOT-PATH-BUF u i 2 * + + c!
      100 FMT-ROOT-PATH-BUF u i 2 * + 1 + + c!
   loop
   FMT-ROOT-PATH-BUF u level 2 * + ;

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

variable LISTENER
create MKFIFO-PATH PATH-CAP allot
create SOCAT-PATH PATH-CAP allot
variable MKFIFO-U
variable SOCAT-U


: TOOL ( ptr u8 n ptr u8 -- len ) {: name:ptr size:n dst:ptr :}
   name size >LEN dst FIND-EXECUTABLE MATCH option
      none OF
         2 s" fs-mutate-test: required executable missing: " write drop
         2 name size write drop 2 S\" \n" write drop
         E-PROC-PATH throw
      ENDOF
      some OF ENDOF
   ;MATCH ;


: FIND-TOOLS ( -- )
   s" mkfifo" MKFIFO-PATH TOOL MKFIFO-U !
   s" socat" SOCAT-PATH TOOL SOCAT-U ! ;


: MAKE-FIFO ( -- )
   PROC-ARGV-RESET
   FMT-TREE-LEAF >LEN PROC-ARGV+
   MKFIFO-PATH MKFIFO-U @ -1 >FD -1 >FD -1 >FD PROC-RUN-ARGV-IO-RC
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
   SB$ >LEN PROC-ARGV+
   s" /dev/null" >LEN PROC-ARGV+
   SOCAT-PATH SOCAT-U @ -1 >FD -1 >FD -1 >FD PROC-SPAWN-ARGV-IO
   LISTENER ! ;


: STOP-LISTENER ( -- )
   LISTENER @ PROCESS:SIGKILL PROCESS:KILL-RAW drop
   LISTENER @ PROCESS:WAIT drop ;


: WAIT-LISTENER ( -- )
   100 0 do
      FMT-TREE-ROOT-FILE FS:TRY-LSTAT if
         FS:STAT-MODE@ FS:S-IFMT and $C000 = if unloop exit then
      then
      NULL-PTR 0 10 poll drop
   loop
   E-FS-IO throw ;


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
5000000000 constant WAIT-NS

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

: A1$ ( -- ptr u8 n ) P-A1 P-A1-U @ ;
: A2$ ( -- ptr u8 n ) P-A2 P-A2-U @ ;
: B1$ ( -- ptr u8 n ) P-B1 P-B1-U @ ;
: B2$ ( -- ptr u8 n ) P-B2 P-B2-U @ ;
: AW$ ( -- ptr u8 n ) P-AW P-AW-U @ ;
: BW$ ( -- ptr u8 n ) P-BW P-BW-U @ ;
: RA$ ( -- ptr u8 n ) P-RA P-RA-U @ ;
: RAS$ ( -- ptr u8 n ) P-RAS P-RAS-U @ ;
: RAF$ ( -- ptr u8 n ) P-RAF P-RAF-U @ ;
: RAG$ ( -- ptr u8 n ) P-RAG P-RAG-U @ ;
: RB$ ( -- ptr u8 n ) P-RB P-RB-U @ ;
: RBS$ ( -- ptr u8 n ) P-RBS P-RBS-U @ ;
: RBF$ ( -- ptr u8 n ) P-RBF P-RBF-U @ ;
: RBG$ ( -- ptr u8 n ) P-RBG P-RBG-U @ ;
: KEEP$ ( -- ptr u8 n ) P-KEEP P-KEEP-U @ ;
: W5$ ( -- ptr u8 n ) P-W5 P-W5-U @ ;
: W5S$ ( -- ptr u8 n ) P-W5S P-W5S-U @ ;
: W5F1$ ( -- ptr u8 n ) P-W5F1 P-W5F1-U @ ;
: W5F2$ ( -- ptr u8 n ) P-W5F2 P-W5F2-U @ ;
: W5D$ ( -- ptr u8 n ) P-W5D P-W5D-U @ ;
: W5DS$ ( -- ptr u8 n ) P-W5DS P-W5DS-U @ ;
: W5DF$ ( -- ptr u8 n ) P-W5DF P-W5DF-U @ ;
: CAS$ ( -- ptr u8 n ) P-CAS P-CAS-U @ ;
: CAD$ ( -- ptr u8 n ) P-CAD P-CAD-U @ ;
: CBS$ ( -- ptr u8 n ) P-CBS P-CBS-U @ ;
: CBD$ ( -- ptr u8 n ) P-CBD P-CBD-U @ ;
: A-BODY$ ( -- ptr u8 n ) s" task-a wrote this" ;
: B-BODY$ ( -- ptr u8 n ) s" b" ;

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

: FILE-IS? ( ptr u8 n ptr u8 n -- ) {: path:ptr pathu body:ptr bodyu :}
   path pathu FMT-READ-BUF 64 FS:READ-ALL bodyu T=
   FMT-READ-BUF bodyu body bodyu T$= ;

\ A worker's answer: what it returned, which is 0, or the code it threw.
: JOIN-CODE ( ptr n -- n )
   TASK:JOIN MATCH result
      ok OF ENDOF
      err OF ENDOF
   ;MATCH ;

\ A code that is neither success nor WAIT-FOR's deadline, the only
\ E-PROC-TIMEOUT a worker here throws.
: FAILED? ( n -- bool )
   {: code :}
   code 0<> code E-PROC-TIMEOUT <> and ;

\ Joins both workers before reading either; true when both answered 0. The two
\ wait on each other, so a failing worker leaves its peer to wait out WAIT-FOR:
\ only the failure is asserted, never that deadline. When neither failed, a
\ deadline is the row's own, named and thrown.
: JOIN-BOTH ( -- bool )
   WORKER-A JOIN-CODE WORKER-B JOIN-CODE {: a b :}
   a FAILED? b FAILED? or if
      a FAILED? if a 0 T= then
      b FAILED? if b 0 T= then
      false exit
   then
   a E-PROC-TIMEOUT = b E-PROC-TIMEOUT = or if
      s" fs-mutate-test WAIT-FOR" TEST-WAIT:EXPIRED
   then
   true ;

\ Waits until the counter reaches want, never for a flag, so no round clears
\ what the next sets. A peer that never comes ends the wait with E-PROC-TIMEOUT.
: WAIT-FOR ( ptr n n -- )
   {: flag want :}
   mono-ns WAIT-NS + {: deadline :}
   begin flag atomic@ want < while
      mono-ns deadline >= if E-PROC-TIMEOUT throw then
      1 >MS TASK:SLEEP
   repeat ;

: A-TMP$ ( -- ptr u8 n ) A-TMP A-TMP-U @ SPAN:TAKE SPAN:$ ;
: B-TMP$ ( -- ptr u8 n ) B-TMP B-TMP-U @ SPAN:TAKE SPAN:$ ;

\ A keeps a copy of the path FS:MKTEMP-DIR-IN handed it, waits until B has made one
\ of its own, and only then reads the span it is still holding.
: T1-A-ROUND ( n -- ) {: round :}
   FMT-ROOT$ s" race-a" FS:MKTEMP-DIR-IN {: path:ptr u :}
   path u A-TMP SPAN:COPY  u A-TMP-U !
   round A-READY atomic!
   B-DONE round WAIT-FOR
   path u A-TMP$ STR= 0= if 1 T1-BAD atomic-add drop then
   A-TMP$ FS:REMOVE-DIR ;

: T1-B-ROUND ( n -- ) {: round :}
   A-READY round WAIT-FOR
   FMT-ROOT$ s" race-b" FS:MKTEMP-DIR-IN {: path:ptr u :}
   path u B-TMP SPAN:COPY  u B-TMP-U !
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
: T2-ROUND ( n ptr u8 n ptr u8 n -- ) {: round p1:ptr p1u p2:ptr p2u :}
   round 1 and 0= if p1 p1u p2 p2u FS:RENAME-FILE else p2 p2u p1 p1u FS:RENAME-FILE then ;

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

: REG-NAME ( n n SPAN:span<u8> -- ptr u8 n ) {: which i dst :}
   SB-RESET
   FMT-ROOT$ SB-APPEND
   s" /reg-" SB-APPEND
   97 which + SB-APPEND-C
   s" -" SB-APPEND
   i FMT:SB-U
   SB$ {: a:ptr u :}
   a u dst SPAN:COPY
   dst u SPAN:TAKE SPAN:$ ;

: T4-REG-ONE ( n n SPAN:span<u8> -- ) {: which i dst :}
   which i dst REG-NAME {: a:ptr u :}
   a u s" reg" FS:WRITE-ALL
   a u FS:CLEANUP+ ;

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
: UNDER? ( ptr u8 n ptr u8 n -- bool ) {: a:ptr u ra:ptr ru :}
   u ru <= if false exit then
   a ru ra ru STR= ;

: T6-CB ( ptr u8 n -- ) {: a:ptr u :}
   a u W5$ UNDER? 0= if 1 T6-BAD +! then
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

: T7-FILL ( ptr u8 n -- ) {: buf:ptr mask :}
   T7-LEN 0 ?do i 251 mod mask xor buf i + c! loop ;

\ Counts a round whose destination is not byte for byte its own source.
: T7-CHECK ( ptr u8 n ptr u8 ptr u8 ptr n -- )
   {: dst:ptr dstu src:ptr got:ptr bad:ptr :}
   dst dstu got T7-LEN FS:READ-ALL {: u :}
   got u src T7-LEN STR= 0= if 1 bad atomic-add drop then ;

: T7-A-ROUND ( -- )
   CAS$ CAD$ FS:COPY-FILE-STREAM
   CAD$ T7-A-SRC T7-A-GOT T7-A-BAD T7-CHECK ;

: T7-B-ROUND ( -- )
   CBS$ CBD$ FS:COPY-FILE-STREAM
   CBD$ T7-B-SRC T7-B-GOT T7-B-BAD T7-CHECK ;

\ Neither worker begins its rounds until both have started, so the copies
\ overlap, or the test fails on WAIT-FOR's deadline.
: T7-GO ( -- ) 1 T7-READY atomic-add drop  T7-READY 2 WAIT-FOR ;

: T7-A ( -- ) T7-GO ROUNDS 0 ?do T7-A-ROUND loop 0 TASK:RETURN ;
: T7-B ( -- ) T7-GO ROUNDS 0 ?do T7-B-ROUND loop 0 TASK:RETURN ;

: T7-RUN ( -- )
   T7-A-SRC 0 T7-FILL  T7-B-SRC 255 T7-FILL
   CAS$ T7-A-SRC T7-LEN FS:WRITE-ALL
   CBS$ T7-B-SRC T7-LEN FS:WRITE-ALL
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
   s" the fs-mutate band is carved directly below FS-ABI" T-LABEL
   FS-MUT-ABI:END FS-ABI:START T=
   s" the WITH-BYTES scope head is carved between USER-BAND and FS-MUT-ABI" T-LABEL
   MEM-ABI:END FS-MUT-ABI:START T=
   MEM-ABI:START USER-BAND:END T=
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

: DIR$ ( -- ptr u8 n ) DIR-BUF DIR-U @ SPAN:TAKE SPAN:$ ;

\ File i's path, valid until the next call.
: NAME$ ( n -- ptr u8 n ) {: i :}
   SB-RESET
   DIR$ SB-APPEND
   s" /f-" SB-APPEND
   i FMT:SB-U
   SB$ {: a:ptr u :}
   a u NAME-BUF SPAN:COPY
   NAME-BUF u SPAN:TAKE SPAN:$ ;

public

: REGISTER ( -- )
   SB-RESET
   FMT-ROOT$ SB-APPEND
   s" /grow" SB-APPEND
   SB$ {: a:ptr u :}
   a u DIR-BUF SPAN:COPY
   u DIR-U !
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

\ Atomic cells require native cell alignment.
here data-base - negate 7 and allot
variable DONE

: MARK$ ( -- ptr u8 n ) P-MARK P-MARK-U @ ;
: NONE$ ( -- ptr u8 n ) P-NONE P-NONE-U @ ;

\ File i's path, valid until the next call.
: NAME$ ( n -- ptr u8 n ) {: i :}
   SB-RESET
   s" f-" SB-APPEND
   i FMT:SB-U
   FMT-ROOT$ SB$ P-NAME FS:JOIN-PATH {: u :}
   P-NAME u ;

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
   s" habu-fs-mkf" FS:MKTEMP {: path:ptr pathu file:fd :}
   file FD>N FS:CLOSE-FD 0 T=
   path pathu FMT-COLLIDE-BUF FMT-COLLIDE-U FMT-COPY!
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
   FMT-TEST-ATOMIC-FAILURE
   FS-TEST:ATOMIC-SEARCH-DIR
   FS-TEST:ATOMIC-PRIVATE
   FS-TEST:ATOMIC-CREATE
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
   s" removing a missing directory is E-FS-IO" T-LABEL
   [: FMT-RMDIR-MISSING ;] E-FS-IO TTHROWSQ
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
   FMT-TEST-REMOVE-TREE-DEPTH-FDS
   FMT-TASKS:RUN
   FMT-TEST-CLEANUP
   FMT-RACE:RUN
   T-REPORT
   s" fs-mutate-test: ok" type cr ;

FS-MUTATE-TEST-MAIN
