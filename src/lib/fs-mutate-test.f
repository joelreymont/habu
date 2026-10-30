\ fs-mutate-test.f - focused tests for src/lib/fs-mutate.f.
\ Run: bin/hb --load src/lib/errors.f src/lib/string.f src/lib/test.f src/lib/fs.f src/lib/fs-mutate.f src/lib/fs-mutate-test.f
\ Real special-inode fixtures require mkfifo and socat on PATH.

require src/lib/errors.f
require src/lib/string.f
require src/lib/test.f
require src/lib/fs.f
require src/lib/span.f
require src/lib/fs-mutate.f
require src/lib/fmt.f                               \ the TMPDIR child's uncaught code
require src/lib/process-env.f
require src/lib/test/outcome.f                      \ the TMPDIR child's outcome
require src/lib/task.f                              \ the two-task rows at the end
require src/lib/test/wait.f                         \ a worker's deadline at the join

\ White-box access: it reopens FS so the private words this test reads (the
\ sibling naming, the staged reserve, the cleanup table, the temp seeds and the
\ mode constants) are reachable here. The definitions land in FS-TEST, so FS's
\ public surface gains nothing for being tested.
package FS
: FS-TEST:BUILD-SIBLING ( ptr u8 n n n -- ptr u8 n ) BUILD-SIBLING ;
: FS-TEST:ATOMIC-OPEN-DIR ( ptr u8 n -- n ) ATOMIC-OPEN-DIR ;
: FS-TEST:ATOMIC-RESERVE-AT ( n n n -- n n ) ATOMIC-RESERVE-AT ;
: FS-TEST:COPY-FILE ( ptr u8 n ptr u8 n n -- ) COPY-FILE ;
: FS-TEST:COPY-CAP ( -- n ) COPY-CAP ;
: FS-TEST:SLASH ( -- n ) SLASH ;
: FS-TEST:READ-LINK ( ptr u8 n SPAN:span<u8> -- n ) READ-LINK ;
: FS-TEST:MODE-PERM ( -- n ) MODE-PERM ;
: FS-TEST:CLEANUP-COUNT ( -- n ) CLEANUP-N @ ;
: FS-TEST:ATOMIC-BASE-OFF ( ptr u8 n -- n ) ATOMIC-BASE-OFF ;
: FS-TEST:ATOMIC-WRITE-SEED ( ptr u8 n ptr u8 n n -- ) false ATOMIC-WRITE-SEED ;
: FS-TEST:MODE-DIR ( -- n ) MODE-DIR ;
: FS-TEST:BUILD-TEMP-TRY ( ptr u8 n ptr u8 n n n -- ptr u8 n ) BUILD-TEMP-TRY ;
: FS-TEST:MKTEMP-DIR-SEED ( ptr u8 n ptr u8 n n -- ptr u8 n ) MKTEMP-DIR-SEED ;
: FS-TEST:TMP-RETRIES ( -- n ) TMP-RETRIES ;
: FS-TEST:MKTEMP-SEED ( ptr u8 n ptr u8 n n -- ptr u8 n n ) MKTEMP-SEED ;
: FS-TEST:ENOTDIR ( -- n ) ENOTDIR ;
: FS-TEST:S-IFIFO ( -- n ) S-IFIFO ;
: FS-TEST:ATOMIC-OLD-MODE ( ptr u8 n -- n ) ATOMIC-OLD-MODE ;
: FS-TEST:MODE-PRIVATE-FILE ( -- n ) MODE-PRIVATE-FILE ;
: FS-TEST:CLEANUP-PATH ( n -- ptr u8 n ) CLEANUP-PATH ;
: FS-TEST:MODE-EXEC ( -- n ) MODE-EXEC ;
;package

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
variable FMT-UNIQUE-U
variable FMT-PARENT-A-U
variable FMT-PARENT-B-U
variable FMT-PARENT-LINK-U
variable FMT-PARENT-BOUND-U
variable FMT-STAGED-TMP-U

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

create FMT-LONG-BUF PATH-CAP 1 + allot      \ one byte past every path buffer

: FMT-LONG! ( -- )
   PATH-CAP 1 + 0 ?do 97 FMT-LONG-BUF i + c! loop ;

: FMT-LONG$ ( -- ptr u8 n )                    \ PATH-CAP + 1 bytes
   FMT-LONG-BUF PATH-CAP 1 + ;

: FMT-FULL$ ( -- ptr u8 n )                    \ exactly PATH-CAP bytes
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
create FMT-REAL-BUF FS:PATHZ-CAP allot

9217 constant FMT-STREAM-LEN
424242 constant FMT-TMP-SEED
424243 constant FMT-ATOMIC-SEED
424244 constant FMT-ATOMIC-FAIL-SEED
384 constant FMT-MODE-0600
416 constant FMT-MODE-0640
$16D constant FMT-MODE-0555             \ listable, not writable
13 constant FMT-EACCES                  \ a denied mkdir's errno on both targets
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

: FMT-UNIQUE ( -- ptr u8 n )
   FMT-UNIQUE-BUF FMT-UNIQUE-U @ ;

: FMT-TEMP-BASE ( -- ptr u8 n )
   FMT-ROOT$ ;

\ HB_SOCK_TMP before TMPDIR: REMOVE-SPECIAL-TREE binds a real Unix socket under
\ this root, at <root>/remove-tree/root.txt, and sun_path holds 107 bytes on
\ Linux and 103 on macOS. A pool slot's TMPDIR is 97 bytes on its own
\ (measured: the listener never binds, WAIT-LISTENER throws E-FS-IO). Under a
\ gate pool the root is the slot's HB_SOCK_TMP, the short directory the pool
\ makes for a child's sockets and removes when the slot retires or is killed
\ (test/gate-pool.f GT-POOL-CHILD-SOCK!), so a row the pool kills leaves no
\ root behind; the socket path there is about 100 bytes under macOS's default
\ TMPDIR. Run on its own, the root goes under TMPDIR, then /tmp.
\ Either way FMT-PREPARE registers it as a tree, so a run that throws before
\ FMT-TEST-CLEANUP still loses the root at exit with whatever is left in it.
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

: FMT-COPY-TOO-SMALL ( -- )
   FMT-COPY-SRC FMT-COPY-DST 3 FS-TEST:COPY-FILE ;

: FMT-REMOVE-TREE-EMPTY ( -- )
   s" " FS:REMOVE-TREE ;

create FMT-READ-BUF 64 allot
PATH-CAP SPAN-BUFFER: FMT-LINK-READ-BUF     \ READ-LINK's destination is a span
\ ---- the buffers this module owns refuse an overrun at the copy ----------------
\ Each of these used to be a hand-written PATH-CAP / COPY-CAP
\ comparison in fs-mutate.f; the destination now carries its reach.
: FMT-COPY-CAP-PAST-BUFFER ( -- )
   FMT-COPY-SRC FMT-COPY-DST FS-TEST:COPY-CAP 1 + FS-TEST:COPY-FILE ;

: FMT-CLEANUP-PATH-TOO-LONG ( -- )
   FMT-LONG$ FS:CLEANUP+ ;

package FS-TEST
public
\ A relative destination of PATH-CAP bytes, whose absolute spelling under
\ the working directory cannot fit PATH-CAP.
: ATOMIC-TARGET-NO-ROOM ( -- )
   FMT-FULL$ s" x" FS:ATOMIC-WRITE-FILE ;
;package

\ The OS's canonical spelling of an existing path, in FMT-REAL-BUF.
: FMT-REAL ( ptr u8 n -- ptr u8 n )
   FS:>PATHZ FMT-REAL-BUF FS:PATHZ-CAP realpath {: u:n :}
   u 0 > TTRUE
   FMT-REAL-BUF u ;


\ A directory whose canonical path is n bytes: the test root's canonical
\ spelling, then names of `a` with a slash every 201st byte, so no name passes
\ NAME_MAX, the last byte never a slash. TMPDIR's length is its canonical
\ spelling's, which only an existing directory has. The first name goes to the
\ cleanup registry, so FMT-TEST-CLEANUP leaves the root empty.
: FMT-DEEP-DIR ( n -- ptr u8 n )
   {: n:n :}
   FMT-ROOT$ FMT-REAL nip {: r:n :}
   n r ?do
      i r - 201 mod 0= i n 1- < and if FS-TEST:SLASH else 97 then
      FMT-REAL-BUF i + c!
   loop
   FMT-REAL-BUF n FS:MAKE-DIRS
   FMT-REAL-BUF r 201 + FS:CLEANUP-TREE+
   FMT-REAL-BUF n ;


\ A base one byte past the longest FS:MKTEMP-DIR-IN builds in for its prefix,
\ whatever seed it would draw.
: FMT-TEMP-BASE-NO-ROOM ( -- )
   FMT-LONG-BUF s" nr" nip FS:TEMP-DIR-BASE-MAX 1+ s" nr" FS:MKTEMP-DIR-IN 2drop ;

: FMT-MAKE-SYMLINK-EXISTS ( -- )
   FMT-LINK-TARGET FMT-LINK FS:MAKE-SYMLINK ;

package FS-TEST
: MAKE-LINK-EXISTS ( -- )
   FMT-LINK-TARGET FMT-LINK FS:MAKE-LINK ;

public
\ A hard link is a second name of the target's file, not a symbolic link.
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

: FMT-LINK-READ-TOO-SMALL ( -- )
   FMT-LINK FMT-LINK-READ-BUF 3 SPAN:TAKE FS-TEST:READ-LINK drop ;

\ A destination window past the buffer is refused at the narrowing: the old
\ ( ptr u8 n ) destination took the capacity on the caller's word.
: FMT-READ-LINK-OVER-CAP ( -- )
   FMT-LINK FMT-LINK-READ-BUF PATH-CAP 1 + SPAN:TAKE FS-TEST:READ-LINK drop ;

: FMT-READ-LINK-NONLINK ( -- )
   FMT-LINK-TARGET FMT-LINK-READ-BUF FS-TEST:READ-LINK drop ;

: FMT-TEST-COPY ( -- )
   FMT-COPY-SRC FMT-COPY-DST 64 FS-TEST:COPY-FILE
   FMT-COPY-DST FS:FILE? TTRUE
   FMT-COPY-DST FMT-READ-BUF 64 FS:READ-ALL 8 T=
   FMT-READ-BUF 8 s" copy-src" T$= ;

: FMT-NUL-SECOND-COPY ( -- )
   FMT-COPY-DST {: dst:ptr size:n :}
   dst FMT-LONG-BUF size BYTE-COPY
   0 FMT-LONG-BUF size + c!
   FMT-COPY-SRC FMT-LONG-BUF BYTE-VIEW size 1+ FS-TEST:COPY-CAP FS-TEST:COPY-FILE ;

: FMT-TEST-NUL-SECOND-PATH ( -- )
   [: FMT-NUL-SECOND-COPY ;] E-FFI-NUL TTHROWSQ
   FMT-COPY-SRC FS:FILE? TTRUE
   FMT-COPY-DST FMT-READ-BUF 64 FS:READ-ALL 8 T=
   FMT-READ-BUF 8 s" copy-src" T$= ;

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
   FMT-ATOMIC-PATH FS:STAT-MODE FS-TEST:MODE-PERM and
   FMT-REMOVE-PATH FS:STAT-MODE FS-TEST:MODE-PERM and T=
   FMT-REMOVE-PATH FS:REMOVE-FILE ;

: FMT-TEST-ATOMIC-LINK-MODE ( -- )
   FMT-ATOMIC-PATH FMT-MODE-0600 FS:CHMOD-MODE
   s" atomic.txt" FMT-LINK FS:MAKE-SYMLINK
   s" link.txt" FMT-BROKEN-LINK FS:MAKE-SYMLINK
   FMT-BROKEN-LINK s" through-links" FS:ATOMIC-WRITE-FILE
   FMT-LINK FS:SYMLINK? TTRUE
   FMT-BROKEN-LINK FS:SYMLINK? TTRUE
   FMT-LINK FMT-LINK-READ-BUF FS-TEST:READ-LINK 10 T=
   FMT-LINK-READ-BUF 10 SPAN:TAKE SPAN:$ s" atomic.txt" T$=
   FMT-BROKEN-LINK FMT-LINK-READ-BUF FS-TEST:READ-LINK 8 T=
   FMT-LINK-READ-BUF 8 SPAN:TAKE SPAN:$ s" link.txt" T$=
   FMT-ATOMIC-PATH FMT-READ-BUF 64 FS:READ-ALL 13 T=
   FMT-READ-BUF 13 s" through-links" T$=
   FMT-ATOMIC-PATH FS:STAT-MODE FS-TEST:MODE-PERM and FMT-MODE-0600 T=
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

: FMT-TEST-STAGED-PRIMARY ( -- )
   FMT-ATOMIC-PATH s" previous" FS:WRITE-ALL
   s" staged fill error survives cleanup refusal" T-LABEL
   [: FMT-ATOMIC-PATH [: FMT-STAGED-CLEAN-FAIL ;] FS:REPLACE-STAGED ;]
      E-FS-CAPACITY TTHROWSQ
   FMT-ATOMIC-PATH FMT-READ-BUF 64 FS:READ-ALL {: u:n :}
   FMT-READ-BUF u s" previous" T$=
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
   path pathu FMT-UNIQUE-BUF FMT-UNIQUE-U FMT-COPY!
   path pathu data SPAN:$ FS:WRITE-ALL
   E-FS-IO throw ;

: FMT-TEST-STAGED-CONTEXT ( -- )
   s" staged callbacks preserve a complete layout value" T-LABEL
   s" context" SPAN:MAKE FMT-ATOMIC-PATH [: FMT-STAGED-SPAN ;] FS:REPLACE-STAGED
   SPAN:$ s" context" T$=
   FMT-ATOMIC-PATH FMT-READ-BUF 64 FS:READ-ALL 7 T=
   FMT-READ-BUF 7 s" context" T$=
   FS-TEST:CLEANUP-COUNT {: before:n :}
   s" a throwing callback preserves the old file and releases its sibling" T-LABEL
   [: s" partial" SPAN:MAKE FMT-ATOMIC-PATH
      [: FMT-STAGED-FAIL ;] FS:REPLACE-STAGED SPAN:$ 2drop ;] E-FS-IO TTHROWSQ
   FMT-ATOMIC-PATH FMT-READ-BUF 64 FS:READ-ALL 7 T=
   FMT-READ-BUF 7 s" context" T$=
   FMT-UNIQUE FS:EXISTS? TFALSE
   FS-TEST:CLEANUP-COUNT before T=
   s" a staged callback cannot consume its caller's context" T-LABEL
   s" FMT-BAD-STAGED ( n ptr u8 n -- ) [: 2drop drop ;] FS:REPLACE-STAGED"
      CHECK-QUIET-CANDIDATE! 0 T= ;

: FMT-TEST-ATOMIC-RELATIVE ( -- )
   s" ./src/lib/../lib/fs-mutate.f" FS:ATOMIC-TARGET
   FMT-PARENT-BOUND-BUF FMT-PARENT-BOUND-U FMT-COPY!
   s" src/lib/fs-mutate.f" FS:ATOMIC-TARGET FMT-PARENT-BOUND T$=
   FMT-PARENT-BOUND-BUF c@ FS-TEST:SLASH T= ;

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

package FS-TEST
public
\ The sibling an atomic write of path stages, for its seed and attempt.
: SIBLING ( ptr u8 n n n -- ptr u8 n ) {: path:ptr pathu seed:n attempt:n :}
   path path pathu FS-TEST:ATOMIC-BASE-OFF seed attempt BUILD-SIBLING ;

: ATOMIC-TEMP ( n n -- ptr u8 n )
   FMT-ATOMIC-PATH 2swap SIBLING ;
;package

: FMT-TEST-ATOMIC-COLLISION ( -- )
   FMT-ATOMIC-SEED 0 FS-TEST:ATOMIC-TEMP
   FMT-COLLIDE-BUF FMT-COLLIDE-U FMT-COPY!
   FMT-COLLIDE s" unrelated" FS:WRITE-ALL
   FMT-ATOMIC-PATH s" replacement" FMT-ATOMIC-SEED FS-TEST:ATOMIC-WRITE-SEED
   FMT-ATOMIC-PATH FMT-READ-BUF 64 FS:READ-ALL 11 T=
   FMT-READ-BUF 11 s" replacement" T$=
   FMT-COLLIDE FMT-READ-BUF 64 FS:READ-ALL 9 T=
   FMT-READ-BUF 9 s" unrelated" T$=
   FMT-ATOMIC-SEED 1 FS-TEST:ATOMIC-TEMP
   FMT-UNIQUE-BUF FMT-UNIQUE-U FMT-COPY!
   FMT-UNIQUE FS:EXISTS? TFALSE
   FMT-COLLIDE FS:REMOVE-FILE ;

: FMT-TEST-ATOMIC-FAILURE ( -- )
   FMT-MKDIR-PATH FS:MAKE-DIR
   FMT-MKDIR-PATH FMT-ATOMIC-FAIL-SEED 0 FS-TEST:SIBLING
   FMT-UNIQUE-BUF FMT-UNIQUE-U FMT-COPY!
   [: FMT-MKDIR-PATH s" replacement" FMT-ATOMIC-FAIL-SEED FS-TEST:ATOMIC-WRITE-SEED ;]
   E-FS-IO TTHROWSQ
   FMT-MKDIR-PATH FS:DIR? TTRUE
   FMT-UNIQUE FS:EXISTS? TFALSE
   FMT-MKDIR-PATH FS:REMOVE-DIR ;

package FS-TEST
$C0 constant MODE-0300                  \ writable and searchable, not listable
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
   FMT-MKDIR-PATH FS-TEST:MODE-DIR FS:CHMOD-MODE
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
   PRIVATE-OUT$ FS:STAT-MODE FS-TEST:MODE-PERM and mode T= ;


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
   FMT-REMOVE-PATH FS:STAT-MODE FS-TEST:MODE-PERM and {: fresh:n :}
   FMT-REMOVE-PATH FS:REMOVE-FILE
   PRIVATE-OUT$ s" default" FS:ATOMIC-WRITE-FILE
   s" default" fresh PRIVATE-IS
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

\ Under a parent it cannot write, FS:MAKE-DIRS keeps the folder already there and
\ fails the one it must make with FS:ERRNO mkdir's own cause, EACCES, which the
\ mode restored after it does not replace. Each FS:MAKE-DIRS under the read-only parent is caught so
\ the mode comes back before an assert. The first runs while FMT-NEST-C is still
\ there, so a throw from it would also leave a folder the exit cleanup cannot
\ remove.
: FMT-TEST-DIRS-DENIED ( -- )
   FMT-NEST-B FMT-MODE-0555 FS:CHMOD-MODE
   [: FMT-NEST-C FS:MAKE-DIRS ;] catch
   FMT-NEST-B FS-TEST:MODE-DIR FS:CHMOD-MODE
   0 T=
   FMT-NEST-C FS:REMOVE-DIR
   FMT-NEST-B FMT-MODE-0555 FS:CHMOD-MODE
   [: FMT-NEST-C FS:MAKE-DIRS ;] catch FS:ERRNO {: code:n cause:n :}
   FMT-NEST-B FS-TEST:MODE-DIR FS:CHMOD-MODE
   code E-FS-IO T=
   cause FMT-EACCES T=
   FMT-NEST-C FS:DIR? TFALSE ;

package FS-TEST
create NAMED-BUF PATH-CAP allot

\ The test root joined with a last name of n bytes, all `n`.
: NAMED$ ( n -- ptr u8 n )
   {: n:n :}
   FMT-ROOT$ {: root:ptr rootu:n :}
   root NAMED-BUF rootu BYTE-COPY
   FS-TEST:SLASH NAMED-BUF rootu + c!
   rootu 1+ n + rootu 1+ ?do 110 NAMED-BUF i + c! loop
   NAMED-BUF rootu 1+ n + ;

public
\ Past NAME_MAX, the host refuses the component: E-FS-IO with its cause.
: DIRS-NAME-TOO-LONG ( -- )
   [: 300 NAMED$ FS:MAKE-DIRS ;] catch FS:ERRNO {: code:n cause:n :}
   code E-FS-IO T=
   cause FS:ENAMETOOLONG T= ;

\ The sibling's name never carries the destination's, so any last name the host
\ accepts, up to NAME_MAX, is written, and no sibling stays behind.
: ATOMIC-NAMED ( n -- )
   {: n:n :}
   n NAMED$ s" long" FMT-ATOMIC-SEED FS-TEST:ATOMIC-WRITE-SEED
   n NAMED$ FMT-READ-BUF 64 FS:READ-ALL 4 T=
   FMT-READ-BUF 4 s" long" T$=
   n NAMED$ FMT-ATOMIC-SEED 0 SIBLING FS:EXISTS? TFALSE
   n NAMED$ FS:REMOVE-FILE ;

\ A last name past NAME_MAX is the host's refusal, named by its cause; the
\ sibling it staged goes.
: ATOMIC-NAME-TOO-LONG ( -- )
   [: 300 NAMED$ s" x" FMT-ATOMIC-SEED FS-TEST:ATOMIC-WRITE-SEED ;] catch
   FS:ERRNO {: code:n cause:n :}
   code E-FS-IO T=
   cause FS:ENAMETOOLONG T=
   300 NAMED$ FMT-ATOMIC-SEED 0 SIBLING FS:EXISTS? TFALSE ;

\ Only the destination's own path meets PATH-CAP: one exactly that long,
\ with a 1-byte last name, is written.
: ATOMIC-PATH-CAP ( -- )
   PATH-CAP 2 - FMT-DEEP-DIR {: dir:ptr diru:n :}
   FS-TEST:SLASH dir diru + c!
   [char] x dir diru 1+ + c!
   dir PATH-CAP s" capped" FS:ATOMIC-WRITE-FILE
   dir PATH-CAP FMT-READ-BUF 64 FS:READ-ALL 6 T=
   FMT-READ-BUF 6 s" capped" T$= ;
;package

: FMT-TEST-TEMPS ( -- )
   FMT-TEMP-BASE s" one" FS:MKTEMP-DIR-IN 2dup FS:DIR? TTRUE FS:CLEANUP-DIR+
   FMT-TEMP-BASE s" two" FS:MKTEMP-DIR-IN 2dup FS:DIR? TTRUE FS:CLEANUP-DIR+
   s" habu-fs-mut" FS:MKTEMP-DIR 2dup FS:DIR? TTRUE FS:CLEANUP-DIR+ ;

: FMT-TEST-TEMP-COLLISION ( -- )
   FMT-TEMP-BASE s" collide" FMT-TMP-SEED 0 FS-TEST:BUILD-TEMP-TRY
   FMT-COLLIDE-BUF FMT-COLLIDE-U FMT-COPY!
   FMT-COLLIDE FS:MAKE-DIR
   FMT-COLLIDE FS:CLEANUP-DIR+
   FMT-TEMP-BASE s" collide" FMT-TMP-SEED FS-TEST:MKTEMP-DIR-SEED
   FMT-UNIQUE-BUF FMT-UNIQUE-U FMT-COPY!
   FMT-UNIQUE FS:DIR? TTRUE
   FMT-UNIQUE FS:CLEANUP-DIR+
   FMT-COLLIDE FMT-UNIQUE T$<>
   FMT-TEMP-BASE s" collide" FMT-TMP-SEED 1 FS-TEST:BUILD-TEMP-TRY
   FMT-COLLIDE-BUF FMT-COLLIDE-U FMT-COPY!
   FMT-COLLIDE FMT-UNIQUE T$= ;

\ A base that already ends in a slash, as macOS's TMPDIR does, takes no second
\ one: the candidate is the one the bare base gives. Building a candidate makes
\ no directory, so the bases need not exist.
: FMT-TEST-TEMP-SLASH ( -- )
   s" /a" s" slash" FMT-TMP-SEED 0 FS-TEST:BUILD-TEMP-TRY
   FMT-COLLIDE-BUF FMT-COLLIDE-U FMT-COPY!
   s" /a/" s" slash" FMT-TMP-SEED 0 FS-TEST:BUILD-TEMP-TRY
   FMT-COLLIDE T$= ;

\ Run the word on every candidate name the prefix gives under FMT-TEMP-BASE
\ with FMT-TMP-SEED.
: FMT-TEMP-EACH ( ptr u8 n [ ptr u8 n -- ] -- )
   {: prefix:ptr prefixu run :}
   0 begin dup FS-TEST:TMP-RETRIES < while
      dup {: attempt:n :}
      FMT-TEMP-BASE prefix prefixu FMT-TMP-SEED attempt FS-TEST:BUILD-TEMP-TRY
      run execute
      1+
   repeat drop ;

: FMT-TEST-TEMP-EXHAUST ( -- )
   s" exhaust" [: FS:MAKE-DIR ;] FMT-TEMP-EACH
   [: FMT-TEMP-BASE s" exhaust" FMT-TMP-SEED FS-TEST:MKTEMP-DIR-SEED 2drop ;]
   E-FS-IO TTHROWSQ
   s" exhaust" [: FS:REMOVE-DIR ;] FMT-TEMP-EACH ;

\ The file is new, regular, mode 0600 and named under the canonical temp root
\ with the prefix; its descriptor reads and writes; nothing is registered for
\ cleanup; a second call answers another file.
: FMT-TEST-MKTEMP ( -- )
   s" FS:MKTEMP makes a private read-write file under the temp root" T-LABEL
   FS-TEST:CLEANUP-COUNT {: before:n :}
   s" habu-fs-mkf" FS:MKTEMP {: path:ptr pathu file:fd :}
   path pathu FMT-COLLIDE-BUF FMT-COLLIDE-U FMT-COPY!
   FS-TEST:CLEANUP-COUNT before T=
   FMT-COLLIDE FS:CLEANUP+
   s" TMPDIR" GETENV dup 0= if 2drop s" /tmp" then FMT-REAL
   s" habu-fs-mkf-" FMT-UNIQUE-BUF FS:JOIN-PATH {: wantu:n :}
   FMT-COLLIDE FMT-UNIQUE-BUF wantu STARTS-WITH? TTRUE
   FMT-COLLIDE FS:STAT-MODE FS-TEST:MODE-PERM and FMT-MODE-0600 T=
   file FD>N s" mkstemp" write 7 T=
   file FD>N FMT-READ-BUF 64 read 0 T=
   file FD>N FS:CLOSE-FD 0 T=
   FMT-COLLIDE FMT-READ-BUF 64 FS:READ-ALL 7 T=
   FMT-READ-BUF 7 s" mkstemp" T$=
   s" habu-fs-mkf" FS:MKTEMP {: next:ptr nextu nextfd:fd :}
   next nextu FS:CLEANUP+
   nextfd FD>N FS:CLOSE-FD 0 T=
   next nextu FMT-COLLIDE T$<> ;

\ A file at attempt 0's name keeps its bytes and the answer is attempt 1's.
: FMT-TEST-MKTEMP-COLLISION ( -- )
   s" FS:MKTEMP passes over a taken name and leaves it" T-LABEL
   FMT-TEMP-BASE s" fcollide" FMT-TMP-SEED 0 FS-TEST:BUILD-TEMP-TRY
   FMT-COLLIDE-BUF FMT-COLLIDE-U FMT-COPY!
   FMT-COLLIDE s" taken" FS:WRITE-ALL
   FMT-TEMP-BASE s" fcollide" FMT-TMP-SEED FS-TEST:MKTEMP-SEED {: fd:n :}
   FMT-UNIQUE-BUF FMT-UNIQUE-U FMT-COPY!
   fd FS:CLOSE-FD 0 T=
   FMT-COLLIDE FMT-READ-BUF 64 FS:READ-ALL 5 T=
   FMT-READ-BUF 5 s" taken" T$=
   FMT-TEMP-BASE s" fcollide" FMT-TMP-SEED 1 FS-TEST:BUILD-TEMP-TRY FMT-UNIQUE T$=
   FMT-UNIQUE FS:FILE-SIZE 0 T=
   FMT-UNIQUE FS:REMOVE-FILE
   FMT-COLLIDE FS:REMOVE-FILE ;

\ Every name taken is E-FS-IO. A base that is a file refuses the open with
\ ENOTDIR, which is no collision: E-FS-OPEN at once, not a retry to E-FS-IO.
: FMT-TEST-MKTEMP-EXHAUST ( -- )
   s" FS:MKTEMP with every name taken throws E-FS-IO" T-LABEL
   s" fexhaust" [: s" taken" FS:WRITE-ALL ;] FMT-TEMP-EACH
   [: FMT-TEMP-BASE s" fexhaust" FMT-TMP-SEED FS-TEST:MKTEMP-SEED drop 2drop ;]
   E-FS-IO TTHROWSQ
   s" FS:MKTEMP under a file throws E-FS-OPEN" T-LABEL
   FMT-TEMP-BASE s" fexhaust" FMT-TMP-SEED 0 FS-TEST:BUILD-TEMP-TRY
   FMT-UNIQUE-BUF FMT-UNIQUE-U FMT-COPY!
   [: FMT-UNIQUE s" under" FMT-TMP-SEED FS-TEST:MKTEMP-SEED drop 2drop ;]
   E-FS-OPEN TTHROWSQ
   s" fexhaust" [: FS:REMOVE-FILE ;] FMT-TEMP-EACH ;

\ ---- the temp roots a child reads from its environment ------------------------
\ A child bin/hb, because TMPDIR is read from the process environment, which a
\ running process does not change for itself. A child whose TMPDIR cannot hold
\ a temp directory or file exits nonzero with one stderr line that names TMPDIR
\ and the value, and the uncaught code. A child whose TMPDIR reaches a directory
\ through a symlink, as macOS's /tmp is /private/tmp, makes its temp
\ directory under the canonical spelling. The
\ paths and the children's programs are under this test's root.
package FMT-TMPDIR
private

$1000 constant CAP
15000 constant TIMEOUT-MS
10 constant NL
200 constant LINK-NAME-U                 \ one link level's target, under NAME_MAX

create OUT CAP allot
create ERR CAP allot
create MISSING-BUF PATH-CAP allot
create PROG-BUF PATH-CAP allot
create FILE-PROG-BUF PATH-CAP allot
create CANON-PROG-BUF PATH-CAP allot
create NOCANON-BUF PATH-CAP allot
create STEP-BUF PATH-CAP allot
create WANT-BUF PATH-CAP 64 + allot   \ a reason, a value, the newline
variable MISSING-U
variable PROG-U
variable FILE-PROG-U
variable CANON-PROG-U
variable NOCANON-U
variable WANT-U

: MISSING$ ( -- ptr u8 n )
   MISSING-BUF MISSING-U @ ;

: PROG$ ( -- ptr u8 n )
   PROG-BUF PROG-U @ ;

: FILE-PROG$ ( -- ptr u8 n )
   FILE-PROG-BUF FILE-PROG-U @ ;

: CANON-PROG$ ( -- ptr u8 n )
   CANON-PROG-BUF CANON-PROG-U @ ;

: NOCANON$ ( -- ptr u8 n )
   NOCANON-BUF NOCANON-U @ ;

: PATHS! ( -- )
   FMT-ROOT$ s" no-such-tmpdir" MISSING-BUF FS:JOIN-PATH MISSING-U !
   FMT-ROOT$ s" tmpdir-child.f" PROG-BUF FS:JOIN-PATH PROG-U !
   FMT-ROOT$ s" tmpdir-file.f" FILE-PROG-BUF FS:JOIN-PATH FILE-PROG-U !
   FMT-ROOT$ s" tmpdir-canon.f" CANON-PROG-BUF FS:JOIN-PATH CANON-PROG-U ! ;

: PROG-SRC$ ( -- ptr u8 n )
   S\" require src/lib/fs-mutate.f\ns\q tmpdir-child\q FS:MKTEMP-DIR 2drop\n" ;

: FILE-PROG-SRC$ ( -- ptr u8 n )
   S\" require src/lib/fs-mutate.f\ns\q tmpdir-file\q FS:MKTEMP drop 2drop\n" ;

\ Prints the directory FS:MKTEMP-DIR makes and a newline.
: CANON-PROG-SRC$ ( -- ptr u8 n )
   S\" require src/lib/fs-mutate.f\n: LINE ( ptr u8 n -- ) {: a:ptr u :} 1 a u write drop 1 S\\\q \\n\q write drop ;\ns\q canon\q FS:MKTEMP-DIR LINE\n" ;

\ A directory one byte past the longest TMPDIR with room for the temp name
\ PROG-SRC$ asks for, whose prefix is tmpdir-child.
: LONG$ ( -- ptr u8 n )
   s" tmpdir-child" nip FS:TEMP-DIR-BASE-MAX 1+ FMT-DEEP-DIR ;

\ One level below NOCANON$: a directory of LINK-NAME-U bytes of `a` and a link L
\ beside it that names it, and NOCANON$ then names L.
: NOCANON-LEVEL ( -- )
   NOCANON$ FMT-LONG-BUF LINK-NAME-U STEP-BUF FS:JOIN-PATH {: diru:n :}
   STEP-BUF diru FS:MAKE-DIR
   NOCANON$ s" L" STEP-BUF FS:JOIN-PATH {: linku:n :}
   FMT-LONG-BUF LINK-NAME-U STEP-BUF linku FS:MAKE-SYMLINK
   STEP-BUF NOCANON-BUF linku BYTE-COPY
   linku NOCANON-U ! ;

\ A directory spelled `<root>/nc/L/L/...` whose canonical spelling is past
\ PATH-CAP: stat follows each L, whose expansion stays short, but every
\ level adds LINK-NAME-U + 1 bytes to the canonical spelling, which realpath
\ then cannot answer.
: NOCANON! ( -- )
   FMT-LONG!
   FMT-ROOT$ s" nc" NOCANON-BUF FS:JOIN-PATH NOCANON-U !
   NOCANON$ FS:MAKE-DIR
   NOCANON$ FS:CLEANUP-TREE+
   NOCANON$ FMT-REAL nip
   begin dup PATH-CAP <= while
      NOCANON-LEVEL LINK-NAME-U 1+ +
   repeat drop ;

\ The child running this program, with TMPDIR the given value, whatever
\ PROC-ENV-SET added after CHILD-ENV, and the rest of the environment inherited.
: CHILD-ENV ( ptr u8 n -- ) {: a:ptr u :}
   PROC-ARGV-ENV-RESET
   s" TMPDIR" >LEN a u >LEN PROC-ENV-SET ;

: CAPTURE ( ptr u8 n -- len len outcome ) {: prog:ptr progu :}
   PROC-ENV-INHERIT-MISSING
   prog progu >LEN PROC-ARGV+
   s" bin/hb" >LEN OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS
   RUN-ARGV-ENV-CAPTURE-OUTCOME ;

: WANT+ ( ptr u8 n -- ) {: a:ptr u :}
   a WANT-BUF WANT-U @ + u BYTE-COPY
   WANT-U @ u + WANT-U ! ;

\ The whole line the child must print: the reason, then the value. Not built
\ in SB: a value near PATH-CAP bytes with the reason overflows SB-CAP.
: WANT$ ( ptr u8 n ptr u8 n -- ptr u8 n ) {: why:ptr whyu a:ptr u :}
   0 WANT-U !
   why whyu WANT+
   a u WANT+
   S\" \n" WANT+
   WANT-BUF WANT-U @ ;

\ The line the child prints for an uncaught code, rendered from the name.
: CODE$ ( n -- ptr u8 n )
   SB-RESET
   s" hb: uncaught throw code " SB-APPEND
   FMT:SB-INT
   SB$ ;

\ A child running this program given this TMPDIR exits nonzero and prints the
\ line for it and the code.
: REFUSED ( ptr u8 n ptr u8 n ptr u8 n n -- )
   {: prog:ptr progu a:ptr u why:ptr whyu code:n :}
   a u CHILD-ENV prog progu CAPTURE {: outu:len erru:len oc :}
   oc MATCH outcome
     exited OF 0 T<> ENDOF
     signaled OF s" exit" s" signal" T-OUTCOME-MISS ENDOF
     timeout OF prog progu OUT outu LEN>N ERR erru LEN>N T-TIMED-OUT ENDOF
   ;MATCH
   ERR erru LEN>N why whyu a u WANT$ CONTAINS? TTRUE
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
   CANON-PROG$ OUT outu LEN>N ERR erru LEN>N oc 0 T-OUTCOME-EXITED=
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
   PROG$ MISSING$ s" hb: TMPDIR is not a directory: " E-FS-DIR REFUSED
   s" FS:MKTEMP refuses a missing TMPDIR by name" T-LABEL
   FILE-PROG$ MISSING$ s" hb: TMPDIR is not a directory: " E-FS-DIR REFUSED
   s" a TMPDIR with no room for the temp name is refused by name" T-LABEL
   PROG$ LONG$ s" hb: TMPDIR is too long for a temp path: "
   E-SPAN-CAPACITY REFUSED
   s" a TMPDIR with no canonical path is refused by name" T-LABEL
   NOCANON! PROG$ NOCANON$ s" hb: TMPDIR has no canonical path: " E-FS-IO
   REFUSED
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
   FMT-LINK FMT-LINK-READ-BUF FS-TEST:READ-LINK FMT-LINK-TARGET-U @ T=
   FMT-LINK-READ-BUF FMT-LINK-TARGET-U @ SPAN:TAKE SPAN:$ FMT-LINK-TARGET T$=
   [: FMT-MAKE-SYMLINK-EXISTS ;] E-FS-IO TTHROWSQ
   [: FMT-LINK-READ-TOO-SMALL ;] E-FS-CAPACITY TTHROWSQ
   [: FMT-READ-LINK-OVER-CAP ;] E-SPAN-RANGE TTHROWSQ
   [: FMT-READ-LINK-NONLINK ;] E-FS-STAT TTHROWSQ
   FMT-LINK FS:REMOVE-FILE
   FMT-LINK FS:SYMLINK? TFALSE
   FMT-LINK-TARGET FS:FILE? TTRUE
   FMT-LINK-TARGET FS:REMOVE-FILE
   FMT-LINK-TARGET FS:EXISTS? TFALSE ;

: FMT-TEST-BROKEN-SYMLINK ( -- )
   FMT-BROKEN-TARGET FMT-BROKEN-LINK FS:MAKE-SYMLINK
   FMT-BROKEN-LINK FS:EXISTS? TFALSE
   FMT-BROKEN-LINK FS:SYMLINK? TTRUE
   FMT-BROKEN-LINK FMT-LINK-READ-BUF FS-TEST:READ-LINK FMT-BROKEN-TARGET-U @ T=
   FMT-LINK-READ-BUF FMT-BROKEN-TARGET-U @ SPAN:TAKE SPAN:$ FMT-BROKEN-TARGET T$=
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
   cause FS-TEST:ENOTDIR T=
   [: FMT-REMOVE-FILE-SLASH ;] catch FS:ERRNO {: fcode:n fcause:n :}
   s" FS:REMOVE-TREE of file/ throws E-FS-STAT with ENOTDIR" T-LABEL
   fcode E-FS-STAT T=
   fcause FS-TEST:ENOTDIR T=
   s" and keeps the file" T-LABEL
   FMT-LINK-DIR-FILE FS:FILE? TTRUE ;

\ FMT-TREE/d/d/... with level one-byte directories below it.
: FMT-DEEP$ ( n -- ptr u8 n ) {: level:n :}
   FMT-TREE {: a:ptr u:n :}
   a FMT-ROOT-PATH-BUF u BYTE-COPY
   level 0 ?do
      FS:SLASH FMT-ROOT-PATH-BUF u i 2 * + + c!
      100 FMT-ROOT-PATH-BUF u i 2 * + 1 + + c!
   loop
   FMT-ROOT-PATH-BUF u level 2 * + ;

\ The descriptor open would hand out next: the lowest one free.
: FMT-NEXT-FD ( -- n )
   FMT-ROOT$ FS:OPEN-DIR-READ dup close ;

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
   FMT-DEEP-LEVELS 1 - FMT-DEEP$ FS-TEST:MODE-DIR FS:CHMOD-MODE
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
   LISTENER @ SIGKILL PROC-KILL-RAW drop
   LISTENER @ PROC-WAIT-STATUS drop ;


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
   FMT-TREE-LEAF FS:STAT-MODE FS:S-IFMT and FS-TEST:S-IFIFO T=
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
   \ Clean up even when the regression fails against the old implementation.
   FMT-TREE-ROOT-FILE FS:EXISTS? if FMT-TREE-ROOT-FILE FS:REMOVE-FILE then
   FMT-TREE-LEAF FS:EXISTS? if FMT-TREE-LEAF FS:REMOVE-FILE then
   FMT-TREE FS:REMOVE-TREE ;

;package

\ ---- two tasks inside one module word at once ---------------------------------
\ Every row here runs the same module word in both tasks and asserts what the
\ other task could not disturb. T1 is the shape of the witnessed failure - a
\ task's RETURNED temporary directory path replaced by another task's - and is
\ red on an engine whose fs-mutate.f stages that name process-wide. The paths
\ T4 registers are left in the registry on purpose: FMT-TEST-CLEANUP's
\ FS:CLEANUP-RUN removes them before the root goes.
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
PATH-CAP SPAN-BUFFER: M-REG

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
   W5DS$ s" x.txt" P-W5DF FS:JOIN-PATH P-W5DF-U ! ;

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

\ Joins both workers before reading either, and answers true when both
\ answered 0. The two wait on each other, so a worker that fails leaves its
\ peer to wait out WAIT-FOR: only the failure is asserted, never that
\ deadline, and the case reads nothing the workers left. When neither failed,
\ a deadline is the row's own, named and thrown.
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

\ A task waits for its peer's round counter rather than a flag, so no round has
\ to clear what the next one sets. A peer that never arrives ends the wait with
\ E-PROC-TIMEOUT instead of hanging the suite: the task's JOIN records it and
\ JOIN-BOTH reads it, never asserting it against zero.
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

\ Inspect the reserved sibling before any bytes are written, then exercise
\ the complete replacement with a final mode distinct from the staging mode.
424245 constant STAGE-SEED

: STAGE-RUN ( -- )
   AW$ s" old" FS:WRITE-ALL
   AW$ FMT-MODE-0640 FS:CHMOD-MODE
   AW$ FS-TEST:ATOMIC-OLD-MODE {: mode :}
   AW$ FS:ATOMIC-TARGET FS-TEST:ATOMIC-OPEN-DIR {: dir :}
   dir STAGE-SEED FS-TEST:MODE-PRIVATE-FILE FS-TEST:ATOMIC-RESERVE-AT {: fd :} drop
   AW$ STAGE-SEED 0 FS-TEST:SIBLING {: temp:ptr tempu :}
   s" the reserved replacement is private before writing" T-LABEL
   temp tempu FS:STAT-MODE FS-TEST:MODE-PERM and FMT-MODE-0600 T=
   temp tempu FS:FILE-SIZE 0 T=
   AW$ s" old" FILE-IS?
   mode FMT-MODE-0640 T=
   AW$ FS:STAT-MODE FS-TEST:MODE-PERM and FMT-MODE-0640 T=
   fd FS:CLOSE-FD 0 T=
   dir close
   temp tempu FS:REMOVE-FILE
   AW$ s" replacement" FS:ATOMIC-WRITE-FILE
   s" the published replacement preserves content and mode" T-LABEL
   AW$ s" replacement" FILE-IS?
   AW$ FS:STAT-MODE FS-TEST:MODE-PERM and FMT-MODE-0640 T=
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

: COUNT-SLOTS ( ptr u8 n n -- n ) {: a:ptr u base :}
   0 REGS 2 * 0 ?do
      base i + FS-TEST:CLEANUP-PATH a u STR= if 1+ then
   loop ;

: CHECK-ONE ( n n n -- ) {: which i base :}
   which i M-REG REG-NAME base COUNT-SLOTS 1 T= ;

: T4-RUN ( -- )
   FS-TEST:CLEANUP-COUNT {: base :}
   ['] T4-A WORKER-A TASK:ACTIVATE
   ['] T4-B WORKER-B TASK:ACTIVATE
   JOIN-BOTH 0= if exit then
   s" every registration from either task claimed a slot of its own" T-LABEL
   FS-TEST:CLEANUP-COUNT base REGS 2 * + T=
   REGS 2 * 0 ?do
      i REGS / i REGS mod base CHECK-ONE
   loop ;

\ ---- each walk owns its context ---------------------------------------------
\ Two tasks remove two trees at once with plain FS:REMOVE-TREE, and a FS:WALK-FILES
\ callback removes a subtree while its walk stands. Both were impossible while
\ the walk stacks were one set for the image: the two removals crossed each
\ other's depth and path slot, and a removal inside a callback clobbered the
\ walk it ran under.
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

\ THE REMOVED TREE IS NOT THE WALKED ONE, and it cannot be: a walk holds the
\ directory it is reading OPEN, and `getdirentries64` on the descriptor of a
\ directory that has since been removed answers -1 here, which the walk reports
\ as E-FS-DIR (measured). Two walks in flight must therefore cover disjoint
\ trees. What this row proves is that the removal moves no part of the walk's
\ own state: every path the callback is handed still starts at the walked root,
\ and both of its files are still found after a whole tree went away through
\ the removal's own context.
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
   T6-RUN ;

;package

\ ---- the registry grows ------------------------------------------------------
\ A directory and then GROWS files inside it are registered, enough to grow
\ the row several times over whatever the rows before left in it; one middle
\ file is forgotten once its owner removed it. Every registration keeps its
\ slot through the growth, the forget closes the gap in order, and
\ FMT-TEST-CLEANUP's FS:CLEANUP-RUN must take every file before the directory:
\ the directory is a plain rmdir, which a file left in it would refuse.
package FMT-GROW
private

100 constant GROWS
50 constant GONE

PATH-CAP SPAN-BUFFER: NAME-BUF
PATH-CAP SPAN-BUFFER: DIR-BUF
variable DIR-U
variable BASE

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

\ Slot BASE+1+at names file i.
: SLOT-IS ( n n -- ) {: at i :}
   BASE @ 1+ at + FS-TEST:CLEANUP-PATH i NAME$ T$= ;

public

: REGISTER ( -- )
   SB-RESET
   FMT-ROOT$ SB-APPEND
   s" /grow" SB-APPEND
   SB$ {: a:ptr u :}
   a u DIR-BUF SPAN:COPY
   u DIR-U !
   DIR$ FS:MAKE-DIR
   FS-TEST:CLEANUP-COUNT BASE !
   DIR$ FS:CLEANUP-DIR+
   GROWS 0 ?do
      i NAME$ s" grow" FS:WRITE-ALL
      i NAME$ FS:CLEANUP+
   loop
   s" the registry grows past every registration and keeps each slot" T-LABEL
   FS-TEST:CLEANUP-COUNT BASE @ 1+ GROWS + T=
   BASE @ FS-TEST:CLEANUP-PATH DIR$ T$=
   GROWS 0 ?do i i SLOT-IS loop
   GONE NAME$ FS:REMOVE-FILE
   GONE NAME$ FS:CLEANUP-FORGET
   s" a forgotten middle slot closes the gap in order" T-LABEL
   FS-TEST:CLEANUP-COUNT BASE @ GROWS + T=
   GONE 0 ?do i i SLOT-IS loop
   GROWS 1- GONE ?do i i 1+ SLOT-IS loop ;

\ After FS:CLEANUP-RUN.
: GONE? ( -- )
   s" FS:CLEANUP-RUN removed the files before their directory" T-LABEL
   DIR$ FS:EXISTS? TFALSE ;

;package

\ ---- the run against a registering task ----------------------------------------
\ FS:CLEANUP-RUN removes a deep tree while another task registers enough paths to
\ grow the row many times over. Each growth releases the row it replaced, and
\ FS:REMOVE-TREE reads its root's name again once the tree below is gone, so a
\ run that held a pointer into a row another task can release read freed
\ storage (measured: SIGSEGV every time). The run must finish, remove the
\ tree and leave the worker's registrations to a later run.
package FMT-RACE
private

400 constant LEVELS
16384 constant REGS

PATH-CAP SPAN-BUFFER: DEEP-BUF
variable DEEP-U
PATH-CAP SPAN-BUFFER: NONE-BUF
variable NONE-U
$10000 TASK:TASK WORKER

\ Atomic cells require native cell alignment.
here data-base - negate 7 and allot
variable GO

: DEEP$ ( -- ptr u8 n ) DEEP-BUF DEEP-U @ SPAN:TAKE SPAN:$ ;
: NONE$ ( -- ptr u8 n ) NONE-BUF NONE-U @ SPAN:TAKE SPAN:$ ;

: PATHS! ( -- )
   SB-RESET
   FMT-ROOT$ SB-APPEND
   LEVELS 0 ?do s" /d" SB-APPEND loop
   SB$ {: a:ptr u :}
   a u DEEP-BUF SPAN:COPY  u DEEP-U !
   SB-RESET
   FMT-ROOT$ SB-APPEND
   s" /none" SB-APPEND
   SB$ {: n:ptr nu :}
   n nu NONE-BUF SPAN:COPY  nu NONE-U ! ;

: WORK ( -- )
   begin GO atomic@ 0= while repeat
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
   DEEP$ FS:MAKE-DIRS
   FMT-ROOT$ FS:CLEANUP-TREE+
   0 GO atomic!
   ['] WORK WORKER TASK:ACTIVATE
   1 GO atomic!
   [: FS:CLEANUP-RUN ;] catch {: code :}
   JOIN-CODE {: worker :}
   s" the run removed its tree while another task grew the registry" T-LABEL
   code 0 T=
   worker 0 T=
   FMT-ROOT$ FS:EXISTS? TFALSE
   FS:CLEANUP-RUN ;

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
   [: FS:CLEANUP-RUN ;] catch 0 T=
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
   FMT-CHMOD-PATH FS:STAT-MODE FS-TEST:MODE-EXEC and FS-TEST:MODE-EXEC = TTRUE
   FMT-CHMOD-PATH FS:MODE-PRIVATE-DIR FS:CHMOD-MODE
   FMT-CHMOD-PATH FS:STAT-MODE FS-TEST:MODE-PERM and FS:MODE-PRIVATE-DIR T=
   FS-TEST:HARD-LINK
   FMT-TEST-COPY
   FMT-TEST-NUL-SECOND-PATH
   FMT-TEST-STREAM-COPY
   FMT-TEST-ATOMIC-WRITE
   FMT-TEST-ATOMIC-ALIASES
   FMT-TEST-ATOMIC-LINK-MODE
   FMT-TEST-ATOMIC-DANGLING-LINK
   FMT-TEST-ATOMIC-RELATIVE
   FMT-TEST-ATOMIC-PARENT-LINK
   FMT-TEST-STAGED-CONTEXT
   FMT-TEST-ATOMIC-COLLISION
   FMT-TEST-ATOMIC-FAILURE
   FS-TEST:ATOMIC-SEARCH-DIR
   FS-TEST:ATOMIC-PRIVATE
   FMT-TEST-STAGED-PRIMARY
   FMT-TEST-DIRS
   FMT-TEST-DIRS-DENIED
   FS-TEST:DIRS-NAME-TOO-LONG
   250 FS-TEST:ATOMIC-NAMED
   255 FS-TEST:ATOMIC-NAMED
   FS-TEST:ATOMIC-NAME-TOO-LONG
   FS-TEST:ATOMIC-PATH-CAP
   FMT-TEST-TEMPS
   FMT-TEST-TEMP-COLLISION
   FMT-TEST-TEMP-SLASH
   FMT-TEST-TEMP-EXHAUST
   FMT-TEST-MKTEMP
   FMT-TEST-MKTEMP-COLLISION
   FMT-TEST-MKTEMP-EXHAUST
   FMT-TMPDIR:RUN
   FMT-TEST-SYMLINK
   FMT-TEST-BROKEN-SYMLINK
   [: FMT-REMOVE-MISSING ;] E-FS-IO TTHROWSQ
   [: FMT-RENAME-MISSING ;] E-FS-IO TTHROWSQ
   [: FMT-CHMOD-MISSING ;] E-FS-STAT TTHROWSQ
   [: FMT-CHMOD-MODE-MISSING ;] E-FS-IO TTHROWSQ
   [: FMT-RMDIR-MISSING ;] E-FS-IO TTHROWSQ
   [: FMT-MKDIR-EXISTS ;] E-FS-IO TTHROWSQ
   [: FMT-COPY-TOO-SMALL ;] E-FS-CAPACITY TTHROWSQ
   [: FMT-COPY-CAP-PAST-BUFFER ;] E-SPAN-RANGE TTHROWSQ
   FMT-LONG!
   [: FMT-CLEANUP-PATH-TOO-LONG ;] E-SPAN-CAPACITY TTHROWSQ
   [: FS-TEST:ATOMIC-TARGET-NO-ROOM ;] E-SPAN-CAPACITY TTHROWSQ
   [: FMT-TEMP-BASE-NO-ROOM ;] E-SPAN-CAPACITY TTHROWSQ
   [: FMT-REMOVE-TREE-EMPTY ;] E-FS-PATH TTHROWSQ
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
