split-str
\ Anchored filesystem reads through the public source and native load paths.
require src/lib/test.f
require src/lib/fs-mutate.f
require src/lib/fs-anchored.f
require src/lib/fs-list.f
require src/lib/fd.f
require src/lib/fmt.f

package FS-ANCHORED-TEST

16 SPAN-BUFFER: OUT
create TMP FS:PATHZ-CAP allot
variable TMP-U
variable ANCHOR
variable OPEN-FDS
create PATHS 35 PATH-CAP * allot
73 constant MODE-0111
448 constant MODE-0700
create LONG PATH-CAP 1 + allot

PROCESS-SYMBOLS
FUNCTION: MKFIFO-CALL mkfifo ( ^u8 n -- i32 ) ;FUNCTION

: TMP$ ( -- ^u8 n ) TMP TMP-U @ ;
: PATH ( n ^u8 n -- ^u8 n )
   {: idx:n name:^ u:n :}
   PATHS idx PATH-CAP * + {: out:^ :}
   TMP$ name u out FS:JOIN-PATH out swap ;
: ROOT ( -- ^u8 n ) 0 s" root" PATH ;
: OUTSIDE ( -- ^u8 n ) 1 s" outside" PATH ;
: CHILD ( -- ^u8 n ) 2 s" root/child" PATH ;
: FILE ( -- ^u8 n ) 3 s" root/child/file" PATH ;
: LINK ( -- ^u8 n ) 4 s" root/child/link" PATH ;
: NEXT ( -- ^u8 n ) 5 s" root/next" PATH ;
: MOVED ( -- ^u8 n ) 6 s" root/moved" PATH ;
: FIFO ( -- ^u8 n ) 7 s" root/fifo" PATH ;
: BLOCKED ( -- ^u8 n ) 8 s" root/blocked" PATH ;
: BLOCKED-FILE ( -- ^u8 n ) 9 s" root/blocked/file" PATH ;
: OUT-FILE ( -- ^u8 n ) 10 s" outside/file" PATH ;
: REPLACEMENT ( -- ^u8 n ) 11 s" root/replacement" PATH ;
: DEEP ( -- ^u8 n ) 12 s" root/child/deep" PATH ;
: DEEP-FILE ( -- ^u8 n ) 13 s" root/child/deep/file" PATH ;
: DOT-DIR ( -- ^u8 n ) 14 s" root/child/.cache" PATH ;
: DOT-FILE ( -- ^u8 n ) 15 s" root/child/.cache/a..b" PATH ;
: SEARCH ( -- ^u8 n ) 25 s" search" PATH ;
: SEARCH-A ( -- ^u8 n ) 26 s" search/a" PATH ;
: SEARCH-B ( -- ^u8 n ) 27 s" search/a/b" PATH ;
: SEARCH-FILE ( -- ^u8 n ) 28 s" search/a/b/f" PATH ;
: SEARCH-LINK ( -- ^u8 n ) 29 s" search/l" PATH ;
: SEARCH-LINK-B ( -- ^u8 n ) 30 s" search/l/b" PATH ;
: LINKED ( -- ^u8 n ) 31 s" linked" PATH ;
: LINKED-CHILD ( -- ^u8 n ) 32 s" linked/child" PATH ;
: DOT-CHILD ( -- ^u8 n ) 33 s" root/./child" PATH ;
: MISSING-DIR ( -- ^u8 n ) 34 s" root/missing" PATH ;

: NONE-WITH ( option<fd> n -- ) {: o want:n :}
   o MATCH option
      none OF true ENDOF
      some OF FS:CLOSE-DIR false ENDOF
   ;MATCH TTRUE
   FS:ERRNO want T= ;
: SOME ( option<fd> -- fd )
   MATCH option
      none OF false TTRUE -1 >FD ENDOF
      some OF FS:ERRNO 0 T= ENDOF
   ;MATCH ;

: FD-ENTRY ( ^u8 n -- )
   2drop 1 OPEN-FDS +! ;
: FD-COUNT ( -- n )
   0 OPEN-FDS !
   HB-TARGET-MACOS? if s" /dev/fd" else s" /proc/self/fd" then
   [: FD-ENTRY ;] FS-LIST:EACH
   OPEN-FDS @ ;
: READ$ ( fd ^u8 n -- n ) OUT FS:READ-FILE-AT ;
: READ-CHECK ( fd ^u8 n ^u8 n -- )
   {: fd:fd path:^ u:n want:^ wantu:n :}
   fd path u READ$ wantu T=
   OUT wantu SPAN:TAKE SPAN:$ want wantu T$= ;

: READ-CHECK-CLOSE ( fd ^u8 n ^u8 n -- )
   {: fd:fd path:^ u:n want:^ wantu:n :}
   fd path u want wantu READ-CHECK
   fd FS:CLOSE-DIR ;

: SETUP ( -- )
   s" habu-anchored" FS:MKTEMP-DIR {: a:^ u:n :}
   a u FS:>PATHZ TMP FS:PATHZ-CAP realpath {: actual:n :}
   actual 0 > TTRUE
   actual TMP-U !
   ROOT FS:MAKE-DIR OUTSIDE FS:MAKE-DIR CHILD FS:MAKE-DIR DEEP FS:MAKE-DIR DOT-DIR FS:MAKE-DIR
   FILE s" original" FS:WRITE-ALL
   DEEP-FILE s" deep" FS:WRITE-ALL
   DOT-FILE s" dotted" FS:WRITE-ALL
   OUT-FILE s" original" FS:WRITE-ALL
   REPLACEMENT s" " FS:WRITE-ALL
   BLOCKED FS:MAKE-DIR BLOCKED-FILE s" blocked" FS:WRITE-ALL
   FIFO FS:>PATHZ 384 MKFIFO-CALL 0 T=
   PATH-CAP 1 + 0 do [char] a LONG i + c! loop ;

: BASIC ( -- )
   s" /" FS:OPEN-DIR FS:CLOSE-DIR
   s" ." FS:OPEN-DIR FS:CLOSE-DIR
   ROOT FS:OPEN-DIR {: root:fd :}
   root s" child" FS:OPEN-DIR-AT {: child:fd :}
   child s" file" s" original" READ-CHECK
   child s" file" OUT 8 SPAN:TAKE FS:READ-FILE-AT 8 T=
   OUT 8 SPAN:TAKE SPAN:$ s" original" T$=
   root s" child/deep/file" s" deep" READ-CHECK
   root s" child/.cache/a..b" s" dotted" READ-CHECK
   root s" replacement" OUT 0 SPAN:TAKE FS:READ-FILE-AT 0 T=
   child FS:CLOSE-DIR root FS:CLOSE-DIR
   ROOT FS:OPEN-DIR {: again:fd :}
   again s" child/file" s" original" READ-CHECK
   again FS:CLOSE-DIR ;

: BAD-PATHS ( -- )
   ROOT FS:OPEN-DIR {: root:fd :}
   root FD>N ANCHOR !
   [: s" " FS:OPEN-DIR drop ;] E-FS-PATH TTHROWSQ
   [: s" /tmp/" FS:OPEN-DIR drop ;] E-FS-PATH TTHROWSQ
   [: ANCHOR @ >FD s" " FS:OPEN-DIR-AT drop ;] E-FS-PATH TTHROWSQ
   [: ANCHOR @ >FD s" ." FS:OPEN-DIR-AT drop ;] E-FS-PATH TTHROWSQ
   FS:ERRNO 0 T=
   [: ANCHOR @ >FD s" /child" FS:OPEN-DIR-AT drop ;] E-FS-PATH TTHROWSQ
   [: ANCHOR @ >FD s" child//file" READ$ drop ;] E-FS-PATH TTHROWSQ
   [: ANCHOR @ >FD s" child/" READ$ drop ;] E-FS-PATH TTHROWSQ
   [: ANCHOR @ >FD s" ./child" READ$ drop ;] E-FS-PATH TTHROWSQ
   [: ANCHOR @ >FD s" child/../file" READ$ drop ;] E-FS-PATH TTHROWSQ
   [: ANCHOR @ >FD s" child" drop -1 READ$ drop ;] E-FS-PATH TTHROWSQ
   [: ANCHOR @ >FD s\" bad\zname" READ$ drop ;] E-FFI-NUL TTHROWSQ
   [: ANCHOR @ >FD LONG PATH-CAP 1 + READ$ drop ;] E-FS-PATH TTHROWSQ
   [: ANCHOR @ >FD s" child/file" OUT 7 SPAN:TAKE FS:READ-FILE-AT drop ;] E-FS-CAPACITY TTHROWSQ
   [: ANCHOR @ >FD s" child/file" OUT 0 SPAN:TAKE FS:READ-FILE-AT drop ;] E-FS-CAPACITY TTHROWSQ
   FS:ERRNO 0 T=
   root s" child/file" s" original" READ-CHECK
   root s" child/.." FS:TRY-OPEN-DIR-AT FS:EINVAL NONE-WITH
   root s\" bad\zname" FS:TRY-OPEN-DIR-AT FS:EINVAL NONE-WITH
   root s" child/absent" FS:TRY-OPEN-DIR-AT FS:ENOENT NONE-WITH
   root s" child/deep" FS:TRY-OPEN-DIR-AT SOME
   s" file" s" deep" READ-CHECK-CLOSE
   root FS:CLOSE-DIR ;

: REPLACED ( -- )
   ROOT FS:OPEN-DIR {: root:fd :}
   root FD>N ANCHOR !
   FILE FS:TRY-LSTAT TTRUE
   FS:STAT-MODE@ FS:S-IFMT and FS:S-IFREG = TTRUE
   FILE FS:REMOVE-FILE
   OUT-FILE FILE FS:MAKE-SYMLINK
   [: ANCHOR @ >FD s" child/file" READ$ drop ;] E-FS-OPEN TTHROWSQ
   FILE FS:REMOVE-FILE
   FILE s" original" FS:WRITE-ALL
   root s" child" FS:OPEN-DIR-AT {: child:fd :}
   CHILD MOVED FS:RENAME-FILE
   OUTSIDE CHILD FS:MAKE-SYMLINK
   [: ANCHOR @ >FD s" child/file" READ$ drop ;] E-FS-OPEN TTHROWSQ
   child s" file" s" original" READ-CHECK
   child FS:CLOSE-DIR
   CHILD FS:REMOVE-FILE
   MOVED CHILD FS:RENAME-FILE
   root FS:CLOSE-DIR ;

: TYPES ( -- )
   ROOT FS:OPEN-DIR {: root:fd :}
   root FD>N ANCHOR !
   [: ANCHOR @ >FD s" child" READ$ drop ;] E-FS-STAT TTHROWSQ
   FS:ERRNO 0 T=
   [: ANCHOR @ >FD s" fifo" READ$ drop ;] E-FS-STAT TTHROWSQ
   FS:ERRNO 0 T=
   root FS:CLOSE-DIR ;

: ERRORS ( -- )
   ROOT FS:OPEN-DIR {: root:fd :}
   root FD>N ANCHOR !
   [: ANCHOR @ >FD s" child/deep/missing" READ$ drop ;] E-FS-OPEN TTHROWSQ
   FS:ERRNO FS:ENOENT T=
   root s" child/deep/file" s" deep" READ-CHECK
   BLOCKED FS:>PATHZ 0 chmod 0 T=
   [: ANCHOR @ >FD s" blocked/file" READ$ drop ;] E-FS-OPEN TTHROWSQ
   FS:ERRNO FS:EACCES T=
   BLOCKED FS:>PATHZ 448 chmod 0 T=
   root s" child/file" s" original" READ-CHECK
   root FS:CLOSE-DIR ;

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
      root s" child/deep/absent" FS:TRY-OPEN-DIR-AT FS:ENOENT NONE-WITH
      s" a none from TRY-OPEN-DIR-AT closes its traversal" T-LABEL
      FD-COUNT before 1+ T=
      [: ANCHOR @ >FD s" child/file" OUT 7 SPAN:TAKE FS:READ-FILE-AT drop ;] E-FS-CAPACITY TTHROWSQ
      s" capacity refusal closes file descriptor" T-LABEL
      FD-COUNT before 1+ T=
      root FS:CLOSE-DIR
      s" closing anchor restores descriptor inventory" T-LABEL
      FD-COUNT before T=
   loop
   s" repeated anchored reads leave no descriptors open" T-LABEL
   FD-COUNT before T= ;

\ a grants search only: a path through it resolves, a read-open of it is
\ EACCES. l is a link to a.
: SEARCH-SETUP ( -- )
   SEARCH FS:MAKE-DIR SEARCH-A FS:MAKE-DIR SEARCH-B FS:MAKE-DIR
   SEARCH-FILE s" found" FS:WRITE-ALL
   s" a" SEARCH-LINK FS:MAKE-SYMLINK
   SEARCH-A MODE-0111 FS:CHMOD-MODE ;
: SEARCH-OPENS ( -- )
   s" a search-only directory above the path passes" T-LABEL
   [: SEARCH-B FS:OPEN-DIR FS:CLOSE-DIR ;] catch 0 T=
   SEARCH FS:OPEN-DIR {: top:fd :}
   [: top s" a/b/f" s" found" READ-CHECK ;] catch 0 T=
   s" a link above the path is refused" T-LABEL
   [: SEARCH-LINK-B FS:OPEN-DIR drop ;] E-FS-OPEN TTHROWSQ
   [: top s" l/b/f" READ$ drop ;] E-FS-OPEN TTHROWSQ
   top FS:CLOSE-DIR ;
\ REMOVE-TREE cannot list a search-only directory.
: SEARCH-ONLY ( -- )
   SEARCH-SETUP
   [: SEARCH-OPENS ;] [: SEARCH-A MODE-0700 FS:CHMOD-MODE ;] finally ;

HB-TARGET-LINUX-KERNEL? [if]
FS-ABI:S-IFLNK constant S-IFLNK
$1FF constant PERMS
384 constant MODE-0600
variable DIR
variable TEMP
variable PLAIN
create LINK-BUF PATH-CAP allot

: AT-DIR ( -- ^u8 n ) 16 s" root/at" PATH ;
: AT-FILE ( -- ^u8 n ) 17 s" root/at/file" PATH ;
: AT-LINK ( -- ^u8 n ) 18 s" root/at/link" PATH ;
: AT-SUB ( -- ^u8 n ) 19 s" root/at/sub" PATH ;
: AT-SUB-LINK ( -- ^u8 n ) 20 s" root/at/sublink" PATH ;
: AT-A ( -- ^u8 n ) 21 s" root/at/a" PATH ;
: AT-C ( -- ^u8 n ) 22 s" root/at/c" PATH ;
: MV ( -- ^u8 n ) 23 s" root/mv" PATH ;
: MV-FILE ( -- ^u8 n ) 24 s" root/mv/f" PATH ;

: NAMED-SETUP ( -- )
   AT-DIR FS:MAKE-DIR AT-SUB FS:MAKE-DIR MV FS:MAKE-DIR
   AT-FILE s" at-bytes" FS:WRITE-ALL
   s" file" AT-LINK FS:MAKE-SYMLINK
   s" sub" AT-SUB-LINK FS:MAKE-SYMLINK
   AT-A s" one" FS:WRITE-ALL AT-C s" three" FS:WRITE-ALL
   MV-FILE s" anchored" FS:WRITE-ALL ;

: D ( -- fd ) DIR @ >FD ;
: FALSE-WITH ( bool n -- ) {: ok:bool want:n :}
   ok TFALSE FS:ERRNO want T= ;
: SIZE ( fd -- n ) FS:FD-STAMP nip nip ;
: OPENED-SIZE ( option<fd> -- n )
   SOME {: f:fd :} f SIZE f FS:CLOSE-DIR ;
: STAT-SIZE ( ^u8 n -- n ) {: a:^ u:n :}
   D a u FS:TRY-STAT-AT TTRUE FS:STAT-SIZE@ ;
: ABSENT ( ^u8 n -- ) {: a:^ u:n :}
   D a u FS:TRY-STAT-AT FS:ENOENT FALSE-WITH ;
\ Runs q on each name every name word refuses.
: REFUSED ( [ fd ^u8 n -- ] -- ) {: q :}
   D s" " q execute  D s" ." q execute  D s" .." q execute
   D s" sub/x" q execute  D s\" a\zb" q execute
   D LONG PATH-CAP 1 + q execute ;

: READS ( -- )
   D s" file" FS:TRY-OPEN-READ-AT OPENED-SIZE 8 T=
   D s" missing" FS:TRY-OPEN-READ-AT FS:ENOENT NONE-WITH
   D s" link" FS:TRY-OPEN-READ-AT FS:ELOOP NONE-WITH
   [: FS:TRY-OPEN-READ-AT FS:EINVAL NONE-WITH ;] REFUSED ;
: WRITES ( -- )
   D s" file" FS:TRY-OPEN-WRITE-AT SOME {: f:fd :}
   f s" AT" FD:WRITE-FULL f FS:CLOSE-DIR
   D s" file" s" AT-bytes" READ-CHECK
   D s" missing" FS:TRY-OPEN-WRITE-AT FS:ENOENT NONE-WITH
   s" missing" ABSENT
   D s" link" FS:TRY-OPEN-WRITE-AT FS:ELOOP NONE-WITH
   [: FS:TRY-OPEN-WRITE-AT FS:EINVAL NONE-WITH ;] REFUSED ;
: ENTRIES ( -- )
   D s" file" FS:TRY-OPEN-ENTRY-AT OPENED-SIZE 8 T=
   s" an O_PATH open of a symlink pins the link itself" T-LABEL
   D s" link" FS:TRY-OPEN-ENTRY-AT OPENED-SIZE 4 T=
   D s" missing" FS:TRY-OPEN-ENTRY-AT FS:ENOENT NONE-WITH
   [: FS:TRY-OPEN-ENTRY-AT FS:EINVAL NONE-WITH ;] REFUSED ;
\ FD-PATH names the directory the descriptor holds, leaves FMT's text
\ intact, refuses a buffer the name would fill, and refuses a closed
\ descriptor as E-FS-STAT with ENOENT.
: FD-PATHS ( -- )
   $1ED 4 FMT:OCT-WIDTH$ {: mode:^ mu:n :}
   D LINK-BUF PATH-CAP FS:FD-PATH {: u:n :}
   LINK-BUF u AT-DIR T$=
   mode mu s" 0755" T$=
   [: D LINK-BUF AT-DIR nip 1- FS:FD-PATH drop ;]
   E-FS-CAPACITY TTHROWSQ
   D s" sub" FS:TRY-OPEN-DIR-AT SOME {: f:fd :}
   f FS:CLOSE-DIR
   [: f LINK-BUF PATH-CAP FS:FD-PATH drop ;] E-FS-STAT TTHROWSQ
   FS:ERRNO FS:ENOENT T= ;
: DIRS ( -- )
   D s" sub" FS:TRY-OPEN-DIR-AT SOME FS:CLOSE-DIR
   s" a directory open of a symlink is ENOTDIR" T-LABEL
   D s" sublink" FS:TRY-OPEN-DIR-AT FS:ENOTDIR NONE-WITH ;

\ TRY-STAT-AT and TRY-LSTAT agree on the file, field by field.
: SAME ( [ -- n ] -- ) {: q :}
   AT-FILE FS:TRY-LSTAT TTRUE q execute
   D s" file" FS:TRY-STAT-AT TTRUE FS:ERRNO 0 T= q execute T= ;
: STATS ( -- )
   [: FS:STAT-MODE@ ;] SAME  [: FS:STAT-SIZE@ ;] SAME
   [: FS:STAT-MTIME-SEC@ ;] SAME  [: FS:STAT-MTIME-NS@ ;] SAME
   [: FS:STAT-CTIME-SEC@ ;] SAME  [: FS:STAT-CTIME-NS@ ;] SAME
   D s" link" FS:TRY-STAT-AT TTRUE
   FS:STAT-MODE@ FS:S-IFMT and S-IFLNK T=  FS:STAT-SIZE@ 4 T=
   s" missing" ABSENT
   [: FS:TRY-STAT-AT FS:EINVAL FALSE-WITH ;] REFUSED ;

: MAKES ( -- )
   D s" made" 448 FS:TRY-MAKE-DIR-AT TTRUE FS:ERRNO 0 T=
   D s" made" FS:TRY-STAT-AT TTRUE
   FS:STAT-MODE@ FS:S-IFMT and FS:S-IFDIR T=
   FS:STAT-MODE@ PERMS and 448 T=
   D s" made" 448 FS:TRY-MAKE-DIR-AT FS:EEXIST FALSE-WITH
   D s" link" 448 FS:TRY-MAKE-DIR-AT FS:EEXIST FALSE-WITH
   D s" gone" 448 FS:TRY-MAKE-DIR-AT TTRUE
   D s" gone" FS:TRY-OPEN-DIR-AT SOME {: gone:fd :}
   D s" gone" FS:TRY-REMOVE-DIR-AT TTRUE
   s" a removed directory takes no new entry" T-LABEL
   gone s" x" 448 FS:TRY-MAKE-DIR-AT FS:ENOENT FALSE-WITH
   gone FS:CLOSE-DIR
   [: 448 FS:TRY-MAKE-DIR-AT FS:EINVAL FALSE-WITH ;] REFUSED ;

: RENAME-TO ( fd ^u8 n -- bool ) {: d:fd a:^ u:n :}
   D s" b" d a u FS:TRY-RENAME-AT ;
: RENAME-NEW-TO ( fd ^u8 n -- bool ) {: d:fd a:^ u:n :}
   D s" b" d a u FS:TRY-RENAME-NEW-AT ;
: RENAMES ( -- )
   D s" a" D s" b" FS:TRY-RENAME-AT TTRUE
   s" a" ABSENT s" b" STAT-SIZE 3 T=
   D s" b" D s" c" FS:TRY-RENAME-NEW-AT FS:EEXIST FALSE-WITH
   s" b" STAT-SIZE 3 T= s" c" STAT-SIZE 5 T=
   D s" b" D s" c" FS:TRY-RENAME-AT TTRUE
   s" b" ABSENT s" c" STAT-SIZE 3 T=
   D s" c" D s" b" FS:TRY-RENAME-NEW-AT TTRUE
   s" c" ABSENT s" b" STAT-SIZE 3 T=
   D s" a" D s" z" FS:TRY-RENAME-AT FS:ENOENT FALSE-WITH
   D s" a" D s" z" FS:TRY-RENAME-NEW-AT FS:ENOENT FALSE-WITH
   [: D s" b" FS:TRY-RENAME-AT FS:EINVAL FALSE-WITH ;] REFUSED
   [: D s" b" FS:TRY-RENAME-NEW-AT FS:EINVAL FALSE-WITH ;] REFUSED
   [: RENAME-TO FS:EINVAL FALSE-WITH ;] REFUSED
   [: RENAME-NEW-TO FS:EINVAL FALSE-WITH ;] REFUSED
   D s" sub" FS:TRY-OPEN-DIR-AT SOME {: sub:fd :}
   D s" b" sub s" b" FS:TRY-RENAME-AT TTRUE
   s" b" ABSENT  sub s" b" FS:TRY-STAT-AT TTRUE
   sub s" b" D s" b" FS:TRY-RENAME-NEW-AT TTRUE
   sub FS:CLOSE-DIR
   s" b" STAT-SIZE 3 T= ;

: REMOVES ( -- )
   D s" b" FS:TRY-REMOVE-FILE-AT TTRUE FS:ERRNO 0 T=
   s" b" ABSENT
   D s" b" FS:TRY-REMOVE-FILE-AT FS:ENOENT FALSE-WITH
   D s" made" FS:TRY-REMOVE-DIR-AT TTRUE
   s" made" ABSENT
   D s" made" FS:TRY-REMOVE-DIR-AT FS:ENOENT FALSE-WITH
   [: FS:TRY-REMOVE-FILE-AT FS:EINVAL FALSE-WITH ;] REFUSED
   [: FS:TRY-REMOVE-DIR-AT FS:EINVAL FALSE-WITH ;] REFUSED ;

: LINK-AS ( fd ^u8 n -- bool ) {: d:fd a:^ u:n :}
   TEMP @ >FD d a u FS:TRY-LINK-AT ;
: TEMPS ( -- )
   D FS:TEMP-AT {: t:fd :}
   t FD>N TEMP !
   t s" temp-bytes" FD:WRITE-FULL
   t D s" kept" FS:TRY-LINK-AT TTRUE FS:ERRNO 0 T=
   D s" kept" FS:TRY-STAT-AT TTRUE
   FS:STAT-MODE@ PERMS and MODE-0600 T=
   FS:STAT-MODE@ FS:S-IFMT and FS:S-IFREG T=
   D s" kept" s" temp-bytes" READ-CHECK
   t D s" kept" FS:TRY-LINK-AT FS:EEXIST FALSE-WITH
   [: LINK-AS FS:EINVAL FALSE-WITH ;] REFUSED
   t FS:CLOSE-DIR
   D s" kept" FS:TRY-OPEN-READ-AT SOME {: plain:fd :}
   plain FD>N PLAIN !
   [: PLAIN @ >FD FS:TEMP-AT drop ;] E-FS-OPEN TTHROWSQ
   FS:ERRNO FS:ENOTDIR T=
   plain FS:CLOSE-DIR ;

\ The directory TRY-OPEN-DIR-AT opened is the anchor after its rename.
: MOVED-ANCHOR ( -- )
   ROOT FS:OPEN-DIR {: root:fd :}
   root s" mv" FS:TRY-OPEN-DIR-AT SOME {: mv:fd :}
   root s" mv" root s" mv-moved" FS:TRY-RENAME-AT TTRUE
   root s" mv" FS:TRY-STAT-AT FS:ENOENT FALSE-WITH
   mv s" f" FS:TRY-OPEN-READ-AT OPENED-SIZE 8 T=
   mv FS:CLOSE-DIR root FS:CLOSE-DIR ;

: NAMED ( -- )
   NAMED-SETUP
   ROOT FS:OPEN-DIR {: root:fd :}
   root s" at" FS:TRY-OPEN-DIR-AT SOME FD>N DIR !
   root s" fifo" FS:TRY-OPEN-READ-AT SOME FS:CLOSE-DIR
   root FS:CLOSE-DIR
   FD-PATHS READS WRITES ENTRIES DIRS STATS MAKES RENAMES REMOVES
   TEMPS
   MOVED-ANCHOR
   D FS:CLOSE-DIR ;
[then]
HB-TARGET-LINUX-KERNEL? 0= [if]
: NAMED ( -- ) ;
[then]

\ A new file of that name, written and read through the descriptor; only
\ Linux has AT words that write.
HB-TARGET-LINUX-KERNEL? [if]
: WRITE-AT ( fd ^u8 n -- ) {: dir:fd name:^ u:n :}
   dir FS:TEMP-AT {: t:fd :}
   t s" written" FD:WRITE-FULL
   t dir name u FS:TRY-LINK-AT TTRUE
   t FS:CLOSE-DIR
   dir name u s" written" READ-CHECK ;
[then]
HB-TARGET-LINUX-KERNEL? 0= [if]
: WRITE-AT ( fd ^u8 n -- ) 2drop drop ;
[then]

: FOLLOWS-TO ( ^u8 n ^u8 n ^u8 n -- )
   {: path:^ u:n file:^ fu:n name:^ nu:n :}
   path u FS:OPEN-DIR-FOLLOW {: dir:fd :}
   dir file fu s" original" READ-CHECK
   dir name nu WRITE-AT
   dir FS:CLOSE-DIR ;
\ linked is a link to root.
: FOLLOWS ( -- )
   s" root" LINKED FS:MAKE-SYMLINK
   s" a link above the last component is followed" T-LABEL
   LINKED-CHILD s" file" s" via-link" FOLLOWS-TO
   s" a link in the last component is followed" T-LABEL
   LINKED s" child/file" s" via-last" FOLLOWS-TO
   s" a . component resolves" T-LABEL
   DOT-CHILD s" file" s" via-dot" FOLLOWS-TO
   [: FILE FS:OPEN-DIR-FOLLOW drop ;] E-FS-OPEN TTHROWSQ
   FS:ERRNO FS:ENOTDIR T=
   [: MISSING-DIR FS:OPEN-DIR-FOLLOW drop ;] E-FS-OPEN TTHROWSQ
   FS:ERRNO FS:ENOENT T= ;

public
: MAIN ( -- )
   T-RESET SETUP
   [: BASIC BAD-PATHS REPLACED TYPES ERRORS LEAKS
      SEARCH-ONLY NAMED FOLLOWS ;]
   [: TMP$ FS:REMOVE-TREE ;] finally
   T-REPORT s" fs-anchored-test: ok" type cr ;
;package
