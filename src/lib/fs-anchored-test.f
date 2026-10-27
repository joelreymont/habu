\ Anchored filesystem reads through the public source and native load paths.
require src/lib/test.f
require src/lib/fs-mutate.f
require src/lib/fs-anchored.f
require src/lib/fs-list.f

package FS-ANCHORED-TEST

16 SPAN-BUFFER: OUT
create TMP FS-PATHZ-CAP allot
variable TMP-U
variable ANCHOR
variable OPEN-FDS
create PATHS 16 FS-PATH-CAP * allot
create LONG FS-PATH-CAP 1 + allot

PROCESS-SYMBOLS
FUNCTION: MKFIFO-CALL mkfifo ( ptr u8 n -- i32 ) ;FUNCTION

: TMP$ ( -- ptr u8 n ) TMP TMP-U @ ;
: PATH ( n ptr u8 n -- ptr u8 n )
   {: idx:n name:ptr u:n :}
   PATHS idx FS-PATH-CAP * + {: out:ptr :}
   TMP$ name u out JOIN-PATH out swap ;
: ROOT ( -- ptr u8 n ) 0 s" root" PATH ;
: OUTSIDE ( -- ptr u8 n ) 1 s" outside" PATH ;
: CHILD ( -- ptr u8 n ) 2 s" root/child" PATH ;
: FILE ( -- ptr u8 n ) 3 s" root/child/file" PATH ;
: LINK ( -- ptr u8 n ) 4 s" root/child/link" PATH ;
: NEXT ( -- ptr u8 n ) 5 s" root/next" PATH ;
: MOVED ( -- ptr u8 n ) 6 s" root/moved" PATH ;
: FIFO ( -- ptr u8 n ) 7 s" root/fifo" PATH ;
: BLOCKED ( -- ptr u8 n ) 8 s" root/blocked" PATH ;
: BLOCKED-FILE ( -- ptr u8 n ) 9 s" root/blocked/file" PATH ;
: OUT-FILE ( -- ptr u8 n ) 10 s" outside/file" PATH ;
: REPLACEMENT ( -- ptr u8 n ) 11 s" root/replacement" PATH ;
: DEEP ( -- ptr u8 n ) 12 s" root/child/deep" PATH ;
: DEEP-FILE ( -- ptr u8 n ) 13 s" root/child/deep/file" PATH ;
: DOT-DIR ( -- ptr u8 n ) 14 s" root/child/.cache" PATH ;
: DOT-FILE ( -- ptr u8 n ) 15 s" root/child/.cache/a..b" PATH ;

: FD-ENTRY ( ptr u8 n -- )
   2drop 1 OPEN-FDS +! ;
: FD-COUNT ( -- n )
   0 OPEN-FDS !
   HB-TARGET-MACOS? if s" /dev/fd" else s" /proc/self/fd" then
   [: FD-ENTRY ;] FS-LIST:EACH
   OPEN-FDS @ ;
: READ$ ( fd ptr u8 n -- n ) OUT FS:READ-FILE-AT ;
: READ-CHECK ( fd ptr u8 n ptr u8 n -- )
   {: fd:fd path:ptr u:n want:ptr wantu:n :}
   fd path u READ$ wantu T=
   OUT wantu SPAN:TAKE SPAN:$ want wantu T$= ;

: SETUP ( -- )
   s" habu-anchored" TMPDIR-MKDIR {: a:ptr u:n :}
   a u FS-PATHZ TMP FS-PATHZ-CAP realpath {: actual:n :}
   actual 0 > TTRUE
   actual TMP-U !
   ROOT MAKE-DIR OUTSIDE MAKE-DIR CHILD MAKE-DIR DEEP MAKE-DIR DOT-DIR MAKE-DIR
   FILE s" original" WRITE-ALL
   DEEP-FILE s" deep" WRITE-ALL
   DOT-FILE s" dotted" WRITE-ALL
   OUT-FILE s" original" WRITE-ALL
   REPLACEMENT s" " WRITE-ALL
   BLOCKED MAKE-DIR BLOCKED-FILE s" blocked" WRITE-ALL
   FIFO FS-PATHZ 384 MKFIFO-CALL 0 T=
   FS-PATH-CAP 1 + 0 do [char] a LONG i + c! loop ;

: BASIC ( -- )
   s" /" FS:OPEN-DIR FS:CLOSE
   s" ." FS:OPEN-DIR FS:CLOSE
   ROOT FS:OPEN-DIR {: root:fd :}
   root s" child" FS:OPEN-DIR-AT {: child:fd :}
   child s" file" s" original" READ-CHECK
   child s" file" OUT 8 SPAN:TAKE FS:READ-FILE-AT 8 T=
   OUT 8 SPAN:TAKE SPAN:$ s" original" T$=
   root s" child/deep/file" s" deep" READ-CHECK
   root s" child/.cache/a..b" s" dotted" READ-CHECK
   root s" replacement" OUT 0 SPAN:TAKE FS:READ-FILE-AT 0 T=
   child FS:CLOSE root FS:CLOSE
   ROOT FS:OPEN-DIR {: again:fd :}
   again s" child/file" s" original" READ-CHECK
   again FS:CLOSE ;

: BAD-PATHS ( -- )
   ROOT FS:OPEN-DIR {: root:fd :}
   root FD>N ANCHOR !
   [: s" " FS:OPEN-DIR drop ;] E-FS-PATH TTHROWSQ
   [: s" /tmp/" FS:OPEN-DIR drop ;] E-FS-PATH TTHROWSQ
   [: ANCHOR @ >FD s" " FS:OPEN-DIR-AT drop ;] E-FS-PATH TTHROWSQ
   [: ANCHOR @ >FD s" ." FS:OPEN-DIR-AT drop ;] E-FS-PATH TTHROWSQ
   [: ANCHOR @ >FD s" /child" FS:OPEN-DIR-AT drop ;] E-FS-PATH TTHROWSQ
   [: ANCHOR @ >FD s" child//file" READ$ drop ;] E-FS-PATH TTHROWSQ
   [: ANCHOR @ >FD s" child/" READ$ drop ;] E-FS-PATH TTHROWSQ
   [: ANCHOR @ >FD s" ./child" READ$ drop ;] E-FS-PATH TTHROWSQ
   [: ANCHOR @ >FD s" child/../file" READ$ drop ;] E-FS-PATH TTHROWSQ
   [: ANCHOR @ >FD s" child" drop -1 READ$ drop ;] E-FS-PATH TTHROWSQ
   [: ANCHOR @ >FD s\" bad\zname" READ$ drop ;] E-FS-PATH-UNSAFE TTHROWSQ
   [: ANCHOR @ >FD LONG FS-PATH-CAP 1 + READ$ drop ;] E-FS-PATH TTHROWSQ
   [: ANCHOR @ >FD s" child/file" OUT 7 SPAN:TAKE FS:READ-FILE-AT drop ;] E-FS-CAPACITY TTHROWSQ
   [: ANCHOR @ >FD s" child/file" OUT 0 SPAN:TAKE FS:READ-FILE-AT drop ;] E-FS-CAPACITY TTHROWSQ
   FS:ERRNO 0 T=
   root s" child/file" s" original" READ-CHECK
   root FS:CLOSE ;

: REPLACED ( -- )
   ROOT FS:OPEN-DIR {: root:fd :}
   root FD>N ANCHOR !
   FILE FS-TRY-LSTAT TTRUE
   FS-STAT-MODE@ S-IFMT and S-IFREG = TTRUE
   FILE REMOVE-FILE
   OUT-FILE FILE MAKE-SYMLINK
   [: ANCHOR @ >FD s" child/file" READ$ drop ;] E-FS-OPEN TTHROWSQ
   FILE REMOVE-FILE
   FILE s" original" WRITE-ALL
   root s" child" FS:OPEN-DIR-AT {: child:fd :}
   CHILD MOVED RENAME-FILE
   OUTSIDE CHILD MAKE-SYMLINK
   [: ANCHOR @ >FD s" child/file" READ$ drop ;] E-FS-OPEN TTHROWSQ
   child s" file" s" original" READ-CHECK
   child FS:CLOSE
   CHILD REMOVE-FILE
   MOVED CHILD RENAME-FILE
   root FS:CLOSE ;

: TYPES ( -- )
   ROOT FS:OPEN-DIR {: root:fd :}
   root FD>N ANCHOR !
   [: ANCHOR @ >FD s" child" READ$ drop ;] E-FS-STAT TTHROWSQ
   FS:ERRNO 0 T=
   [: ANCHOR @ >FD s" fifo" READ$ drop ;] E-FS-STAT TTHROWSQ
   FS:ERRNO 0 T=
   root FS:CLOSE ;

: ERRORS ( -- )
   ROOT FS:OPEN-DIR {: root:fd :}
   root FD>N ANCHOR !
   [: ANCHOR @ >FD s" child/deep/missing" READ$ drop ;] E-FS-OPEN TTHROWSQ
   FS:ERRNO FS:ENOENT T=
   root s" child/deep/file" s" deep" READ-CHECK
   BLOCKED FS-PATHZ 0 chmod 0 T=
   [: ANCHOR @ >FD s" blocked/file" READ$ drop ;] E-FS-OPEN TTHROWSQ
   FS:ERRNO FS:EACCES T=
   BLOCKED FS-PATHZ 448 chmod 0 T=
   root s" child/file" s" original" READ-CHECK
   root FS:CLOSE ;

: LEAKS ( -- )
   FD-COUNT drop
   FD-COUNT {: before:n :}
   30 0 do
      ROOT FS:OPEN-DIR {: root:fd :}
      root FD>N ANCHOR !
      s" opening anchor adds one descriptor" T-LABEL
      FD-COUNT before 1+ T=
      root s" child/deep/file" s" deep" READ-CHECK
      s" successful read closes traversal descriptors" T-LABEL
      FD-COUNT before 1+ T=
      [: ANCHOR @ >FD s" child/deep/missing" READ$ drop ;] E-FS-OPEN TTHROWSQ
      s" missing file closes partial traversal descriptors" T-LABEL
      FD-COUNT before 1+ T=
      [: ANCHOR @ >FD s" child/deep/absent" FS:OPEN-DIR-AT drop ;] E-FS-OPEN TTHROWSQ
      s" missing directory closes partial traversal descriptors" T-LABEL
      FD-COUNT before 1+ T=
      [: ANCHOR @ >FD s" child/file" OUT 7 SPAN:TAKE FS:READ-FILE-AT drop ;] E-FS-CAPACITY TTHROWSQ
      s" capacity refusal closes file descriptor" T-LABEL
      FD-COUNT before 1+ T=
      root FS:CLOSE
      s" closing anchor restores descriptor inventory" T-LABEL
      FD-COUNT before T=
   loop
   s" repeated anchored reads leave no descriptors open" T-LABEL
   FD-COUNT before T= ;

public
: MAIN ( -- )
   T-RESET SETUP
   [: BASIC BAD-PATHS REPLACED TYPES ERRORS LEAKS ;]
   [: TMP$ REMOVE-TREE ;] finally
   T-REPORT s" fs-anchored-test: ok" type cr ;
;package
