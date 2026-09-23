\ fs-mutate-test.f - focused tests for lib/fs-mutate.f.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/fs.f lib/fs-mutate.f lib/fs-mutate-test.f
\ Real special-inode fixtures require mkfifo and socat on PATH.

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/fs.f
require lib/span.f
require lib/fs-mutate.f
require lib/process-env.f
require lib/task.f                              \ the two-task rows at the end

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

create FMT-ROOT-BUF FS-PATH-CAP allot
create FMT-REMOVE-BUF FS-PATH-CAP allot
create FMT-RENAME-SRC-BUF FS-PATH-CAP allot
create FMT-RENAME-DST-BUF FS-PATH-CAP allot
create FMT-CHMOD-BUF FS-PATH-CAP allot
create FMT-COPY-SRC-BUF FS-PATH-CAP allot
create FMT-COPY-DST-BUF FS-PATH-CAP allot
create FMT-STREAM-SRC-BUF FS-PATH-CAP allot
create FMT-STREAM-DST-BUF FS-PATH-CAP allot
create FMT-ATOMIC-BUF FS-PATH-CAP allot

create FMT-LONG-BUF FS-PATH-CAP 1 + allot      \ one byte past every path buffer

: FMT-LONG! ( -- )
   FS-PATH-CAP 1 + 0 ?do 97 FMT-LONG-BUF i + c! loop ;

: FMT-LONG$ ( -- ptr u8 n )                    \ FS-PATH-CAP + 1 bytes
   FMT-LONG-BUF FS-PATH-CAP 1 + ;

: FMT-FULL$ ( -- ptr u8 n )                    \ exactly FS-PATH-CAP bytes
   FMT-LONG-BUF FS-PATH-CAP ;

create FMT-MKDIR-BUF FS-PATH-CAP allot
create FMT-NEST-A-BUF FS-PATH-CAP allot
create FMT-NEST-B-BUF FS-PATH-CAP allot
create FMT-NEST-C-BUF FS-PATH-CAP allot
create FMT-TREE-BUF FS-PATH-CAP allot
create FMT-TREE-CHILD-BUF FS-PATH-CAP allot
create FMT-TREE-GRAND-BUF FS-PATH-CAP allot
create FMT-TREE-DOTS-BUF FS-PATH-CAP allot
create FMT-TREE-ROOT-FILE-BUF FS-PATH-CAP allot
create FMT-TREE-LEAF-BUF FS-PATH-CAP allot
create FMT-TREE-DOTS-FILE-BUF FS-PATH-CAP allot
create FMT-LINK-TARGET-BUF FS-PATH-CAP allot
create FMT-LINK-BUF FS-PATH-CAP allot
create FMT-BROKEN-TARGET-BUF FS-PATH-CAP allot
create FMT-BROKEN-LINK-BUF FS-PATH-CAP allot
create FMT-LINK-DIR-BUF FS-PATH-CAP allot
create FMT-LINK-DIR-FILE-BUF FS-PATH-CAP allot
create FMT-TREE-DIR-LINK-BUF FS-PATH-CAP allot
create FMT-COLLIDE-BUF FS-PATH-CAP allot
create FMT-UNIQUE-BUF FS-PATH-CAP allot

9217 constant FMT-STREAM-LEN
424242 constant FMT-TMP-SEED
create FMT-STREAM-SRC-DATA FMT-STREAM-LEN allot
create FMT-STREAM-DST-DATA FMT-STREAM-LEN allot

: FMT-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u dst:ptr up:ptr :}
   u FS-PATH-CAP > if E-FS-PATH throw then
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

\ Not HB-TMP-MKDIR: REMOVE-SPECIAL-TREE binds a real Unix socket under this
\ root, and sun_path holds 107 bytes. A pool slot HB_TMP is 97 bytes on its
\ own (measured: the listener never binds, WAIT-LISTENER throws E-FS-IO), so
\ the root takes the short base TMPDIR, then /tmp, and FMT-REMOVE! reaps it.
: FMT-ROOT! ( -- )
   s" habu-fs-mutate" TMPDIR-MKDIR {: a:ptr u :}
   a FMT-ROOT-BUF u BYTE-COPY
   u FMT-ROOT-U ! ;

: FMT-REMOVE! ( -- )
   FMT-ROOT$ s" remove.txt" FMT-REMOVE-BUF JOIN-PATH FMT-REMOVE-U ! ;

: FMT-RENAME-SRC! ( -- )
   FMT-ROOT$ s" rename-src.txt" FMT-RENAME-SRC-BUF JOIN-PATH FMT-RENAME-SRC-U ! ;

: FMT-RENAME-DST! ( -- )
   FMT-ROOT$ s" rename-dst.txt" FMT-RENAME-DST-BUF JOIN-PATH FMT-RENAME-DST-U ! ;

: FMT-CHMOD! ( -- )
   FMT-ROOT$ s" chmod.txt" FMT-CHMOD-BUF JOIN-PATH FMT-CHMOD-U ! ;

: FMT-COPY-SRC! ( -- )
   FMT-ROOT$ s" copy-src.txt" FMT-COPY-SRC-BUF JOIN-PATH FMT-COPY-SRC-U ! ;

: FMT-COPY-DST! ( -- )
   FMT-ROOT$ s" copy-dst.txt" FMT-COPY-DST-BUF JOIN-PATH FMT-COPY-DST-U ! ;

: FMT-STREAM-SRC! ( -- )
   FMT-ROOT$ s" stream-src.bin" FMT-STREAM-SRC-BUF JOIN-PATH FMT-STREAM-SRC-U ! ;

: FMT-STREAM-DST! ( -- )
   FMT-ROOT$ s" stream-dst.bin" FMT-STREAM-DST-BUF JOIN-PATH FMT-STREAM-DST-U ! ;

: FMT-ATOMIC! ( -- )
   FMT-ROOT$ s" atomic.txt" FMT-ATOMIC-BUF JOIN-PATH FMT-ATOMIC-U ! ;

: FMT-MKDIR! ( -- )
   FMT-ROOT$ s" mkdir-one" FMT-MKDIR-BUF JOIN-PATH FMT-MKDIR-U ! ;

: FMT-NEST-A! ( -- )
   FMT-ROOT$ s" nested" FMT-NEST-A-BUF JOIN-PATH FMT-NEST-A-U ! ;

: FMT-NEST-B! ( -- )
   FMT-NEST-A s" child" FMT-NEST-B-BUF JOIN-PATH FMT-NEST-B-U ! ;

: FMT-NEST-C! ( -- )
   FMT-NEST-B s" grand" FMT-NEST-C-BUF JOIN-PATH FMT-NEST-C-U ! ;

: FMT-TREE! ( -- )
   FMT-ROOT$ s" remove-tree" FMT-TREE-BUF JOIN-PATH FMT-TREE-U ! ;

: FMT-TREE-CHILD! ( -- )
   FMT-TREE s" child" FMT-TREE-CHILD-BUF JOIN-PATH FMT-TREE-CHILD-U ! ;

: FMT-TREE-GRAND! ( -- )
   FMT-TREE-CHILD s" grand" FMT-TREE-GRAND-BUF JOIN-PATH FMT-TREE-GRAND-U ! ;

: FMT-TREE-DOTS! ( -- )
   FMT-TREE s" .dots" FMT-TREE-DOTS-BUF JOIN-PATH FMT-TREE-DOTS-U ! ;

: FMT-TREE-ROOT-FILE! ( -- )
   FMT-TREE s" root.txt" FMT-TREE-ROOT-FILE-BUF JOIN-PATH FMT-TREE-ROOT-FILE-U ! ;

: FMT-TREE-LEAF! ( -- )
   FMT-TREE-GRAND s" leaf.txt" FMT-TREE-LEAF-BUF JOIN-PATH FMT-TREE-LEAF-U ! ;

: FMT-TREE-DOTS-FILE! ( -- )
   FMT-TREE-DOTS s" kept-by-walk.txt" FMT-TREE-DOTS-FILE-BUF JOIN-PATH FMT-TREE-DOTS-FILE-U ! ;

: FMT-LINK-TARGET! ( -- )
   FMT-ROOT$ s" link-target.txt" FMT-LINK-TARGET-BUF JOIN-PATH FMT-LINK-TARGET-U ! ;

: FMT-LINK! ( -- )
   FMT-ROOT$ s" link.txt" FMT-LINK-BUF JOIN-PATH FMT-LINK-U ! ;

: FMT-BROKEN-TARGET! ( -- )
   FMT-ROOT$ s" no-such-link-target.txt" FMT-BROKEN-TARGET-BUF JOIN-PATH FMT-BROKEN-TARGET-U ! ;

: FMT-BROKEN-LINK! ( -- )
   FMT-ROOT$ s" broken-link.txt" FMT-BROKEN-LINK-BUF JOIN-PATH FMT-BROKEN-LINK-U ! ;

: FMT-LINK-DIR! ( -- )
   FMT-ROOT$ s" link-dir-target" FMT-LINK-DIR-BUF JOIN-PATH FMT-LINK-DIR-U ! ;

: FMT-LINK-DIR-FILE! ( -- )
   FMT-LINK-DIR s" kept.txt" FMT-LINK-DIR-FILE-BUF JOIN-PATH FMT-LINK-DIR-FILE-U ! ;

: FMT-TREE-DIR-LINK! ( -- )
   FMT-TREE s" dir-link" FMT-TREE-DIR-LINK-BUF JOIN-PATH FMT-TREE-DIR-LINK-U ! ;

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
   FMT-REMOVE-PATH s" delete-me" WRITE-ALL
   FMT-RENAME-SRC s" rename-me" WRITE-ALL
   FMT-CHMOD-PATH s" #!/bin/sh\nexit 0\n" WRITE-ALL
   FMT-COPY-SRC s" copy-src" WRITE-ALL ;

: FMT-WRITE-TREE ( -- )
   FMT-TREE-GRAND MAKE-DIRS
   FMT-TREE-DOTS MAKE-DIRS
   FMT-TREE-ROOT-FILE s" root" WRITE-ALL
   FMT-TREE-LEAF s" leaf" WRITE-ALL
   FMT-TREE-DOTS-FILE s" dot" WRITE-ALL ;

: FMT-PREPARE ( -- )
   CLEANUP-RESET
   FMT-LONG!
   FMT-PATHS!
   FMT-ROOT$ CLEANUP-DIR+
   FMT-WRITE-FIXTURES ;

: FMT-REMOVE-MISSING ( -- )
   s" no-such-habu-fs-remove-file" REMOVE-FILE ;

: FMT-RENAME-MISSING ( -- )
   s" no-such-habu-fs-rename-file" FMT-RENAME-DST RENAME-FILE ;

: FMT-CHMOD-MISSING ( -- )
   s" no-such-habu-fs-chmod-file" CHMOD-X ;

: FMT-CHMOD-MODE-MISSING ( -- )
   s" no-such-habu-fs-chmod-mode-file" FS-MUT-MODE-PRIVATE-DIR CHMOD-MODE ;

: FMT-RMDIR-MISSING ( -- )
   s" no-such-habu-fs-rmdir" REMOVE-DIR ;

: FMT-MKDIR-EXISTS ( -- )
   FMT-MKDIR-PATH MAKE-DIR ;

: FMT-COPY-TOO-SMALL ( -- )
   FMT-COPY-SRC FMT-COPY-DST 3 COPY-FILE ;

: FMT-REMOVE-TREE-EMPTY ( -- )
   s" " REMOVE-TREE ;

create FMT-READ-BUF 64 allot
FS-PATH-CAP SPAN-BUFFER: FMT-LINK-READ-BUF     \ READ-LINK's destination is a span
\ ---- the buffers this module owns refuse an overrun at the copy ----------------
\ Each of these used to be a hand-written FS-PATH-CAP / FS-MUT-COPY-CAP
\ comparison in fs-mutate.f; the destination now carries its reach.
: FMT-COPY-CAP-PAST-BUFFER ( -- )
   FMT-COPY-SRC FMT-COPY-DST FS-MUT-COPY-CAP 1 + COPY-FILE ;

: FMT-CLEANUP-PATH-TOO-LONG ( -- )
   FMT-LONG$ CLEANUP+ ;

: FMT-ATOMIC-SUFFIX-NO-ROOM ( -- )
   FMT-FULL$ s" x" ATOMIC-WRITE-FILE ;

: FMT-MAKE-SYMLINK-EXISTS ( -- )
   FMT-LINK-TARGET FMT-LINK MAKE-SYMLINK ;

: FMT-LINK-READ-TOO-SMALL ( -- )
   FMT-LINK FMT-LINK-READ-BUF 3 SPAN:TAKE READ-LINK drop ;

\ A destination window past the buffer is refused at the narrowing: the old
\ ( ptr u8 n ) destination took the capacity on the caller's word.
: FMT-READ-LINK-OVER-CAP ( -- )
   FMT-LINK FMT-LINK-READ-BUF FS-PATH-CAP 1 + SPAN:TAKE READ-LINK drop ;

: FMT-READ-LINK-NONLINK ( -- )
   FMT-LINK-TARGET FMT-LINK-READ-BUF READ-LINK drop ;

: FMT-TEST-COPY ( -- )
   FMT-COPY-SRC FMT-COPY-DST 64 COPY-FILE
   FMT-COPY-DST FILE? TTRUE
   FMT-COPY-DST FMT-READ-BUF 64 READ-ALL 8 T=
   FMT-READ-BUF 8 s" copy-src" T$= ;

: FMT-NUL-SECOND-COPY ( -- )
   FMT-COPY-DST {: dst:ptr size:n :}
   dst FMT-LONG-BUF size BYTE-COPY
   0 FMT-LONG-BUF size + c!
   FMT-COPY-SRC FMT-LONG-BUF BYTE-VIEW size 1+ FS-MUT-COPY-CAP COPY-FILE ;

: FMT-TEST-NUL-SECOND-PATH ( -- )
   [: FMT-NUL-SECOND-COPY ;] E-FS-PATH-UNSAFE TTHROWSQ
   FMT-COPY-SRC FILE? TTRUE
   FMT-COPY-DST FMT-READ-BUF 64 READ-ALL 8 T=
   FMT-READ-BUF 8 s" copy-src" T$= ;

: FMT-FILL-STREAM-DATA ( -- )
   0 begin dup FMT-STREAM-LEN < while
      dup 251 mod over FMT-STREAM-SRC-DATA + c!
      1+
   repeat drop ;

: FMT-TEST-STREAM-COPY ( -- )
   FMT-FILL-STREAM-DATA
   FMT-STREAM-SRC FMT-STREAM-SRC-DATA FMT-STREAM-LEN WRITE-ALL
   FMT-STREAM-SRC FMT-STREAM-DST COPY-FILE-STREAM
   FMT-STREAM-DST FILE? TTRUE
   FMT-STREAM-DST FILE-SIZE FMT-STREAM-LEN T=
   FMT-STREAM-DST FMT-STREAM-DST-DATA FMT-STREAM-LEN READ-ALL FMT-STREAM-LEN T=
   FMT-STREAM-DST-DATA FMT-STREAM-LEN FMT-STREAM-SRC-DATA FMT-STREAM-LEN T$= ;

: FMT-TEST-ATOMIC-WRITE ( -- )
   FMT-ATOMIC-PATH s" old" WRITE-ALL
   FMT-ATOMIC-PATH s" new-data" ATOMIC-WRITE-FILE
   FMT-ATOMIC-PATH FMT-READ-BUF 64 READ-ALL 8 T=
   FMT-READ-BUF 8 s" new-data" T$= ;

: FMT-TEST-DIRS ( -- )
   FMT-MKDIR-PATH MAKE-DIR
   FMT-MKDIR-PATH DIR? TTRUE
   FMT-NEST-C MAKE-DIRS
   FMT-NEST-A DIR? TTRUE
   FMT-NEST-B DIR? TTRUE
   FMT-NEST-C DIR? TTRUE ;

: FMT-TEST-TEMPS ( -- )
   FMT-TEMP-BASE s" one" MAKE-TEMP-DIR 2dup DIR? TTRUE CLEANUP-DIR+
   FMT-TEMP-BASE s" two" MAKE-TEMP-DIR 2dup DIR? TTRUE CLEANUP-DIR+
   s" habu-fs-mut" HB-TMP-MKDIR 2dup DIR? TTRUE CLEANUP-DIR+ ;

: FMT-TEST-TEMP-COLLISION ( -- )
   FMT-TEMP-BASE s" collide" FMT-TMP-SEED 0 FS-MUT-BUILD-TEMP-TRY
   FMT-COLLIDE-BUF FMT-COLLIDE-U FMT-COPY!
   FMT-COLLIDE MAKE-DIR
   FMT-COLLIDE CLEANUP-DIR+
   FMT-TEMP-BASE s" collide" FMT-TMP-SEED FS-MUT-MAKE-TEMP-DIR-SEED
   FMT-UNIQUE-BUF FMT-UNIQUE-U FMT-COPY!
   FMT-UNIQUE DIR? TTRUE
   FMT-UNIQUE CLEANUP-DIR+
   FMT-COLLIDE FMT-UNIQUE T$<> ;

: FMT-TEST-SYMLINK ( -- )
   FMT-LINK-TARGET s" link-target" WRITE-ALL
   FMT-LINK-TARGET FMT-LINK MAKE-SYMLINK
   FMT-LINK SYMLINK? TTRUE
   FMT-LINK FILE? TTRUE
   FMT-LINK FMT-LINK-READ-BUF READ-LINK FMT-LINK-TARGET-U @ T=
   FMT-LINK-READ-BUF FMT-LINK-TARGET-U @ SPAN:TAKE SPAN:$ FMT-LINK-TARGET T$=
   [: FMT-MAKE-SYMLINK-EXISTS ;] E-FS-IO TTHROWSQ
   [: FMT-LINK-READ-TOO-SMALL ;] E-FS-CAPACITY TTHROWSQ
   [: FMT-READ-LINK-OVER-CAP ;] E-SPAN-RANGE TTHROWSQ
   [: FMT-READ-LINK-NONLINK ;] E-FS-STAT TTHROWSQ
   FMT-LINK REMOVE-FILE
   FMT-LINK SYMLINK? TFALSE
   FMT-LINK-TARGET FILE? TTRUE
   FMT-LINK-TARGET REMOVE-FILE
   FMT-LINK-TARGET EXISTS? TFALSE ;

: FMT-TEST-BROKEN-SYMLINK ( -- )
   FMT-BROKEN-TARGET FMT-BROKEN-LINK MAKE-SYMLINK
   FMT-BROKEN-LINK EXISTS? TFALSE
   FMT-BROKEN-LINK SYMLINK? TTRUE
   FMT-BROKEN-LINK FMT-LINK-READ-BUF READ-LINK FMT-BROKEN-TARGET-U @ T=
   FMT-LINK-READ-BUF FMT-BROKEN-TARGET-U @ SPAN:TAKE SPAN:$ FMT-BROKEN-TARGET T$=
   FMT-BROKEN-LINK REMOVE-FILE
   FMT-BROKEN-LINK SYMLINK? TFALSE ;

: FMT-TEST-REMOVE-TREE ( -- )
   FMT-WRITE-TREE
   FMT-TREE-ROOT-FILE FILE? TTRUE
   FMT-TREE-LEAF FILE? TTRUE
   FMT-TREE-DOTS-FILE FILE? TTRUE
   FMT-TREE REMOVE-TREE
   FMT-TREE EXISTS? TFALSE
   FMT-TREE REMOVE-TREE
   s" no-such-habu-fs-remove-tree" REMOVE-TREE ;

: FMT-TEST-REMOVE-TREE-SYMLINK-DIR ( -- )
   FMT-LINK-DIR MAKE-DIR
   FMT-LINK-DIR-FILE s" kept" WRITE-ALL
   FMT-TREE MAKE-DIR
   FMT-LINK-DIR FMT-TREE-DIR-LINK MAKE-SYMLINK
   FMT-TREE-DIR-LINK SYMLINK? TTRUE
   FMT-TREE REMOVE-TREE
   FMT-TREE EXISTS? TFALSE
   FMT-LINK-DIR DIR? TTRUE
   FMT-LINK-DIR-FILE FILE? TTRUE ;

package FMT-SPECIAL

variable LISTENER
create MKFIFO-PATH FS-PATH-CAP allot
create SOCAT-PATH FS-PATH-CAP allot
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
\ The peer must not unlink it during shutdown: REMOVE-TREE owns that claim.
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
      FMT-TREE-ROOT-FILE FS-TRY-LSTAT if
         FS-STAT-MODE@ S-IFMT and $C000 = if unloop exit then
      then
      NULL-PTR 0 10 poll drop
   loop
   E-FS-IO throw ;


: REMOVE-SPECIAL-TREE ( -- )
   WAIT-LISTENER
   s" the child bound a real Unix socket" T-LABEL
   FMT-TREE-ROOT-FILE STAT-MODE S-IFMT and $C000 T=
   s" the nested inode is a FIFO" T-LABEL
   FMT-TREE-LEAF STAT-MODE S-IFMT and $1000 T=
   FMT-TREE REMOVE-TREE ;


public

: RUN ( -- )
   FIND-TOOLS
   FMT-TREE-GRAND MAKE-DIRS
   MAKE-FIFO
   START-LISTENER
   [: REMOVE-SPECIAL-TREE ;] catch {: rc:n :}
   STOP-LISTENER
   s" REMOVE-TREE unlinks a bound Unix socket and a nested FIFO" T-LABEL
   rc 0 T=
   FMT-TREE EXISTS? TFALSE
   \ Clean up even when the regression fails against the old implementation.
   FMT-TREE-ROOT-FILE EXISTS? if FMT-TREE-ROOT-FILE REMOVE-FILE then
   FMT-TREE-LEAF EXISTS? if FMT-TREE-LEAF REMOVE-FILE then
   FMT-TREE REMOVE-TREE ;

;package

\ ---- two tasks inside one module word at once ---------------------------------
\ Every row here runs the same module word in both tasks and asserts what the
\ other task could not disturb. T1 is the shape of the witnessed failure - a
\ task's RETURNED temporary directory path replaced by another task's - and is
\ red on an engine whose fs-mutate.f stages that name process-wide. The paths
\ T4 registers are left in the registry on purpose: FMT-TEST-CLEANUP's
\ CLEANUP-RUN removes them, above the root it removes last.
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
FS-PATH-CAP SPAN-BUFFER: A-TMP
FS-PATH-CAP SPAN-BUFFER: B-TMP
variable A-TMP-U
variable B-TMP-U
FS-PATH-CAP SPAN-BUFFER: A-REG
FS-PATH-CAP SPAN-BUFFER: B-REG
FS-PATH-CAP SPAN-BUFFER: M-REG

create P-A1 FS-PATH-CAP allot   variable P-A1-U
create P-A2 FS-PATH-CAP allot   variable P-A2-U
create P-B1 FS-PATH-CAP allot   variable P-B1-U
create P-B2 FS-PATH-CAP allot   variable P-B2-U
create P-AW FS-PATH-CAP allot   variable P-AW-U
create P-BW FS-PATH-CAP allot   variable P-BW-U

: A1$ ( -- ptr u8 n ) P-A1 P-A1-U @ ;
: A2$ ( -- ptr u8 n ) P-A2 P-A2-U @ ;
: B1$ ( -- ptr u8 n ) P-B1 P-B1-U @ ;
: B2$ ( -- ptr u8 n ) P-B2 P-B2-U @ ;
: AW$ ( -- ptr u8 n ) P-AW P-AW-U @ ;
: BW$ ( -- ptr u8 n ) P-BW P-BW-U @ ;
: A-BODY$ ( -- ptr u8 n ) s" task-a wrote this" ;
: B-BODY$ ( -- ptr u8 n ) s" b" ;

: PATHS! ( -- )
   FMT-ROOT$ s" task-a-1.txt" P-A1 JOIN-PATH P-A1-U !
   FMT-ROOT$ s" task-a-2.txt" P-A2 JOIN-PATH P-A2-U !
   FMT-ROOT$ s" task-b-1.txt" P-B1 JOIN-PATH P-B1-U !
   FMT-ROOT$ s" task-b-2.txt" P-B2 JOIN-PATH P-B2-U !
   FMT-ROOT$ s" task-a-atomic.txt" P-AW JOIN-PATH P-AW-U !
   FMT-ROOT$ s" task-b-atomic.txt" P-BW JOIN-PATH P-BW-U ! ;

: FILE-IS? ( ptr u8 n ptr u8 n -- ) {: path:ptr pathu body:ptr bodyu :}
   path pathu FMT-READ-BUF 64 READ-ALL bodyu T=
   FMT-READ-BUF bodyu body bodyu T$= ;

: JOIN-OK ( ptr n -- )
   TASK:JOIN MATCH result
      ok OF 0 T= ENDOF
      err OF 0 T= ENDOF
   ;MATCH ;

\ A task waits for its peer's round counter rather than a flag, so no round has
\ to clear what the next one sets. A peer that never arrives ends the row with a
\ thrown code its JOIN reports, instead of hanging the suite.
: WAIT-FOR ( ptr n n -- ) {: flag want :}
   mono-ns WAIT-NS + {: deadline :}
   begin flag atomic@ want < while
      mono-ns deadline >= if E-FS-IO throw then
      1 >MS TASK:SLEEP
   repeat ;

: A-TMP$ ( -- ptr u8 n ) A-TMP A-TMP-U @ SPAN:TAKE SPAN:$ ;
: B-TMP$ ( -- ptr u8 n ) B-TMP B-TMP-U @ SPAN:TAKE SPAN:$ ;

\ A keeps a copy of the path MAKE-TEMP-DIR handed it, waits until B has made one
\ of its own, and only then reads the span it is still holding.
: T1-A-ROUND ( n -- ) {: round :}
   FMT-ROOT$ s" race-a" MAKE-TEMP-DIR {: path:ptr u :}
   path u A-TMP SPAN:COPY  u A-TMP-U !
   round A-READY atomic!
   B-DONE round WAIT-FOR
   path u A-TMP$ STR= 0= if 1 T1-BAD atomic-add drop then
   A-TMP$ REMOVE-DIR ;

: T1-B-ROUND ( n -- ) {: round :}
   A-READY round WAIT-FOR
   FMT-ROOT$ s" race-b" MAKE-TEMP-DIR {: path:ptr u :}
   path u B-TMP SPAN:COPY  u B-TMP-U !
   round B-DONE atomic!
   B-TMP$ REMOVE-DIR ;

: T1-A ( -- ) ROUNDS 0 ?do i 1+ T1-A-ROUND loop 0 TASK:RETURN ;
: T1-B ( -- ) ROUNDS 0 ?do i 1+ T1-B-ROUND loop 0 TASK:RETURN ;

: T1-RUN ( -- )
   0 A-READY !  0 B-DONE !  0 T1-BAD !
   ['] T1-A WORKER-A TASK:ACTIVATE
   ['] T1-B WORKER-B TASK:ACTIVATE
   WORKER-A JOIN-OK
   WORKER-B JOIN-OK
   s" another task cannot replace my returned temporary directory path" T-LABEL
   T1-BAD @ 0 T= ;

\ An even round moves the file out, an odd one moves it back, so SWAPS rounds
\ end where they started.
: T2-ROUND ( n ptr u8 n ptr u8 n -- ) {: round p1:ptr p1u p2:ptr p2u :}
   round 1 and 0= if p1 p1u p2 p2u RENAME-FILE else p2 p2u p1 p1u RENAME-FILE then ;

: T2-A ( -- ) SWAPS 0 ?do i A1$ A2$ T2-ROUND loop 0 TASK:RETURN ;
: T2-B ( -- ) SWAPS 0 ?do i B1$ B2$ T2-ROUND loop 0 TASK:RETURN ;

: T2-RUN ( -- )
   A1$ A-BODY$ WRITE-ALL
   B1$ B-BODY$ WRITE-ALL
   ['] T2-A WORKER-A TASK:ACTIVATE
   ['] T2-B WORKER-B TASK:ACTIVATE
   WORKER-A JOIN-OK
   WORKER-B JOIN-OK
   s" two tasks renaming at once each moved only their own file" T-LABEL
   A1$ A-BODY$ FILE-IS?
   B1$ B-BODY$ FILE-IS?
   A2$ EXISTS? TFALSE
   B2$ EXISTS? TFALSE
   A1$ REMOVE-FILE
   B1$ REMOVE-FILE ;

: T3-A ( -- ) SWAPS 0 ?do AW$ A-BODY$ ATOMIC-WRITE-FILE loop 0 TASK:RETURN ;
: T3-B ( -- ) SWAPS 0 ?do BW$ B-BODY$ ATOMIC-WRITE-FILE loop 0 TASK:RETURN ;

: T3-RUN ( -- )
   ['] T3-A WORKER-A TASK:ACTIVATE
   ['] T3-B WORKER-B TASK:ACTIVATE
   WORKER-A JOIN-OK
   WORKER-B JOIN-OK
   s" two tasks writing atomically at once each kept their own contents" T-LABEL
   AW$ A-BODY$ FILE-IS?
   BW$ B-BODY$ FILE-IS?
   AW$ REMOVE-FILE
   BW$ REMOVE-FILE ;

: REG-NAME ( n n SPAN:span<u8> -- ptr u8 n ) {: which i dst :}
   SB-RESET
   FMT-ROOT$ SB-APPEND
   s" /reg-" SB-APPEND
   97 which + SB-APPEND-C
   s" -" SB-APPEND
   i FS-MUT-SB-U
   SB$ {: a:ptr u :}
   a u dst SPAN:COPY
   dst u SPAN:TAKE SPAN:$ ;

: T4-REG-ONE ( n n SPAN:span<u8> -- ) {: which i dst :}
   which i dst REG-NAME {: a:ptr u :}
   a u s" reg" WRITE-ALL
   a u CLEANUP+ ;

: T4-A ( -- ) REGS 0 ?do 0 i A-REG T4-REG-ONE loop 0 TASK:RETURN ;
: T4-B ( -- ) REGS 0 ?do 1 i B-REG T4-REG-ONE loop 0 TASK:RETURN ;

: SLOT$ ( n -- ptr u8 n ) {: idx :}
   idx FS-MUT-CLEANUP-SLOT idx FS-MUT-CLEANUP-U-PTR @ SPAN:TAKE SPAN:$ ;

: COUNT-SLOTS ( ptr u8 n n -- n ) {: a:ptr u base :}
   0 REGS 2 * 0 ?do
      base i + SLOT$ a u STR= if 1+ then
   loop ;

: CHECK-ONE ( n n n -- ) {: which i base :}
   which i M-REG REG-NAME base COUNT-SLOTS 1 T= ;

: T4-RUN ( -- )
   FS-MUT-CLEANUP-N @ {: base :}
   ['] T4-A WORKER-A TASK:ACTIVATE
   ['] T4-B WORKER-B TASK:ACTIVATE
   WORKER-A JOIN-OK
   WORKER-B JOIN-OK
   s" every registration from either task claimed a slot of its own" T-LABEL
   FS-MUT-CLEANUP-N @ base REGS 2 * + T=
   REGS 2 * 0 ?do
      i REGS / i REGS mod base CHECK-ONE
   loop ;

public

: RUN ( -- )
   s" the fs-mutate band is carved directly below FS-ABI" T-LABEL
   FS-MUT-ABI:BYTES 3080 T=
   FS-MUT-ABI:END FS-ABI:START T=
   FS-MUT-ABI:START USER-BAND:END T=
   PATHS!
   T1-RUN
   T2-RUN
   T3-RUN
   T4-RUN ;

;package

: FMT-TEST-CLEANUP ( -- )
   FMT-WRITE-TREE
   FMT-TREE CLEANUP-TREE+
   FMT-RENAME-DST CLEANUP+
   FMT-CHMOD-PATH CLEANUP+
   FMT-COPY-SRC CLEANUP+
   FMT-COPY-DST CLEANUP+
   FMT-STREAM-SRC CLEANUP+
   FMT-STREAM-DST CLEANUP+
   FMT-ATOMIC-PATH CLEANUP+
   FMT-MKDIR-PATH CLEANUP-DIR+
   FMT-NEST-A CLEANUP-TREE+
   FMT-LINK-DIR CLEANUP-TREE+
   CLEANUP-RUN
   FMT-COPY-DST EXISTS? TFALSE
   FMT-ATOMIC-PATH EXISTS? TFALSE
   FMT-MKDIR-PATH EXISTS? TFALSE
   FMT-NEST-A EXISTS? TFALSE
   FMT-LINK-DIR EXISTS? TFALSE
   FMT-TREE EXISTS? TFALSE
   FMT-ROOT$ EXISTS? TFALSE
   CLEANUP-RUN ;

: FS-MUTATE-TEST-MAIN ( -- )
   T-RESET
   FMT-PREPARE
   FMT-REMOVE-PATH FILE? TTRUE
   FMT-REMOVE-PATH REMOVE-FILE
   FMT-REMOVE-PATH EXISTS? TFALSE
   FMT-RENAME-SRC FILE? TTRUE
   FMT-RENAME-DST EXISTS? TFALSE
   FMT-RENAME-SRC FMT-RENAME-DST RENAME-FILE
   FMT-RENAME-SRC EXISTS? TFALSE
   FMT-RENAME-DST FILE? TTRUE
   FMT-CHMOD-PATH CHMOD-X
   FMT-CHMOD-PATH STAT-MODE FS-MUT-MODE-EXEC and FS-MUT-MODE-EXEC = TTRUE
   FMT-CHMOD-PATH FS-MUT-MODE-PRIVATE-DIR CHMOD-MODE
   FMT-CHMOD-PATH STAT-MODE FS-MUT-MODE-PERM and FS-MUT-MODE-PRIVATE-DIR T=
   FMT-TEST-COPY
   FMT-TEST-NUL-SECOND-PATH
   FMT-TEST-STREAM-COPY
   FMT-TEST-ATOMIC-WRITE
   FMT-TEST-DIRS
   FMT-TEST-TEMPS
   FMT-TEST-TEMP-COLLISION
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
   [: FMT-CLEANUP-PATH-TOO-LONG ;] E-SPAN-CAPACITY TTHROWSQ
   [: FMT-ATOMIC-SUFFIX-NO-ROOM ;] E-SPAN-CAPACITY TTHROWSQ
   [: FMT-REMOVE-TREE-EMPTY ;] E-FS-PATH TTHROWSQ
   FMT-TEST-REMOVE-TREE
   FMT-SPECIAL:RUN
   FMT-TEST-REMOVE-TREE-SYMLINK-DIR
   FMT-TASKS:RUN
   FMT-TEST-CLEANUP
   T-REPORT
   s" fs-mutate-test: ok" type cr ;

FS-MUTATE-TEST-MAIN
