\ Anchored filesystem reads through the public source and native load paths.
require src/lib/test/assert.f
require src/lib/fs-mutate.f
require src/lib/fs-anchored.f
require src/lib/fs-list.f
require src/lib/fd.f
require src/lib/fmt.f
require src/lib/task.f
require src/lib/fs.f
require src/lib/ffi-abi.f
require src/lib/heap.f
require src/lib/vector.f

package FS-ANCHORED-TEST

TYPED-VARIABLE BUF ^VEC:vector<u8>
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

: TMP$ ( -- str ) TMP TMP-U @ >str ;
: PATH ( n str -- str )
   {: idx:n name:str :}
   PATHS idx PATH-CAP * + {: out:^ :}
   TMP$ name out FS:JOIN-PATH out swap >str ;
: ROOT ( -- str ) 0 s" root" PATH ;
: OUTSIDE ( -- str ) 1 s" outside" PATH ;
: CHILD ( -- str ) 2 s" root/child" PATH ;
: FILE ( -- str ) 3 s" root/child/file" PATH ;
: LINK ( -- str ) 4 s" root/child/link" PATH ;
: NEXT ( -- str ) 5 s" root/next" PATH ;
: MOVED ( -- str ) 6 s" root/moved" PATH ;
: FIFO ( -- str ) 7 s" root/fifo" PATH ;
: BLOCKED ( -- str ) 8 s" root/blocked" PATH ;
: BLOCKED-FILE ( -- str ) 9 s" root/blocked/file" PATH ;
: OUT-FILE ( -- str ) 10 s" outside/file" PATH ;
: REPLACEMENT ( -- str ) 11 s" root/replacement" PATH ;
: DEEP ( -- str ) 12 s" root/child/deep" PATH ;
: DEEP-FILE ( -- str ) 13 s" root/child/deep/file" PATH ;
: DOT-DIR ( -- str ) 14 s" root/child/.cache" PATH ;
: DOT-FILE ( -- str ) 15 s" root/child/.cache/a..b" PATH ;
: SEARCH ( -- str ) 25 s" search" PATH ;
: SEARCH-A ( -- str ) 26 s" search/a" PATH ;
: SEARCH-B ( -- str ) 27 s" search/a/b" PATH ;
: SEARCH-FILE ( -- str ) 28 s" search/a/b/f" PATH ;
: SEARCH-LINK ( -- str ) 29 s" search/l" PATH ;
: SEARCH-LINK-B ( -- str ) 30 s" search/l/b" PATH ;
: LINKED ( -- str ) 31 s" linked" PATH ;
: LINKED-CHILD ( -- str ) 32 s" linked/child" PATH ;
: DOT-CHILD ( -- str ) 33 s" root/./child" PATH ;
: MISSING-DIR ( -- str ) 34 s" root/missing" PATH ;

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

: FD-ENTRY ( str -- )
   drop 1 OPEN-FDS +! ;
: FD-COUNT ( -- n )
   0 OPEN-FDS !
   HB-TARGET-MACOS? if s" /dev/fd" else s" /proc/self/fd" then
   [: FD-ENTRY ;] FS-LIST:EACH
   OPEN-FDS @ ;
: NEW-BYTES ( -- ^VEC:vector<u8> ) HEAP:ALLOCATOR 0 [type] u8 VEC:NEW ;
\ BUF emptied, then holding the file; answers its count.
: READ$ ( fd str -- n ) {: fd:fd path:str :}
   BUF @ 0 VEC:RESIZE
   fd path BUF @ FS:READ-FILE-AT
   BUF @ VEC:COUNT ;
\ BUF holding kept, ready for a read to append to.
: KEPT ( -- ^VEC:vector<u8> )
   BUF @ 0 VEC:RESIZE  s" kept" BUF @ VEC:APPEND  BUF @ ;
: READ-CHECK ( fd str str -- )
   {: fd:fd path:str want:str :}
   fd path READ$ want STR:LEN T=
   BUF @ VEC:BYTES want T$= ;

: READ-CHECK-CLOSE ( fd str str -- )
   {: fd:fd path:str want:str :}
   fd path want READ-CHECK
   fd FS:CLOSE-DIR ;

: SETUP ( -- )
   s" habu-anchored" FS:MKTEMP-DIR {: made:str :}
   made FS:>PATHZ TMP FS:PATHZ-CAP SPAN:MAKE realpath {: actual:n :}
   actual 0 > TTRUE
   actual TMP-U !
   NEW-BYTES BUF !
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
   BUF @ 0 VEC:RESIZE
   child s" file" BUF @ FS:READ-FILE-AT
   BUF @ VEC:BYTES s" original" T$=
   root s" child/deep/file" s" deep" READ-CHECK
   root s" child/.cache/a..b" s" dotted" READ-CHECK
   root s" replacement" READ$ 0 T=
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
   [: ANCHOR @ >FD s" child" STR:DATA -1 >str READ$ drop ;] E-FS-PATH TTHROWSQ
   [: ANCHOR @ >FD s\" bad\zname" READ$ drop ;] E-FFI-NUL TTHROWSQ
   [: ANCHOR @ >FD LONG PATH-CAP 1 + >str READ$ drop ;] E-FS-PATH TTHROWSQ
   s" a read appends after the bytes the vector holds" T-LABEL
   root s" child/file" KEPT FS:READ-FILE-AT
   BUF @ VEC:BYTES s" keptoriginal" T$=
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
   s" a failed read leaves the vector's count as it was" T-LABEL
   [: ANCHOR @ >FD s" child/file" KEPT FS:READ-FILE-AT ;] E-FS-OPEN TTHROWSQ
   BUF @ VEC:COUNT 4 T=
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
      root FS:CLOSE-DIR
      s" closing anchor restores descriptor inventory" T-LABEL
      FD-COUNT before T=
   loop
   s" repeated anchored reads leave no descriptors open" T-LABEL
   FD-COUNT before T= ;

\ Two tasks walking at once each get a work area: a shared one makes a task
\ read the other's file, throw, or close a descriptor the other holds.
TYPED-VARIABLE OUT-A ^VEC:vector<u8>
TYPED-VARIABLE OUT-B ^VEC:vector<u8>
variable PAR-READY
variable PAR-BAD
2000 constant PAR-ITERS
TASK:MIN-STACK TASK:TASK PAR-A
TASK:MIN-STACK TASK:TASK PAR-B

: PAR-ROUND ( str ^VEC:vector<u8> str -- ) {: p:str buf:^ want:str :}
   buf 0 VEC:RESIZE
   ANCHOR @ >FD p buf FS:READ-FILE-AT
   buf VEC:BYTES want STR:= if exit then
   1 PAR-BAD atomic-add drop ;
: PAR-WORK ( str ^VEC:vector<u8> str -- ) {: p:str buf:^ want:str :}
   1 PAR-READY atomic-add drop
   begin PAR-READY atomic@ 2 < while TASK:PAUSE repeat
   PAR-ITERS 0 do p buf want PAR-ROUND loop
   0 TASK:RETURN ;
: PAR-WORK-A ( -- ) s" child/file" OUT-A @ s" original" PAR-WORK ;
: PAR-WORK-B ( -- ) s" child/deep/file" OUT-B @ s" deep" PAR-WORK ;
: PAR-JOIN ( ^n -- )
   TASK:JOIN MATCH result
      ok OF 0 T= ENDOF
      err OF 0 T= ENDOF
   ;MATCH ;
: PARALLEL ( -- )
   FD-COUNT {: before:n :}
   ROOT FS:OPEN-DIR {: root:fd :}
   root FD>N ANCHOR !
   0 PAR-BAD !  0 PAR-READY !
   NEW-BYTES OUT-A !  NEW-BYTES OUT-B !
   ['] PAR-WORK-A PAR-A TASK:ACTIVATE
   ['] PAR-WORK-B PAR-B TASK:ACTIVATE
   s" each walking task finishes" T-LABEL
   PAR-A PAR-JOIN PAR-B PAR-JOIN
   OUT-A @ VEC:DELETE  OUT-B @ VEC:DELETE
   s" each walking task reads its own file" T-LABEL
   PAR-BAD @ 0 T=
   root FS:CLOSE-DIR
   s" two walking tasks leave no descriptors open" T-LABEL
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
   s" a link above the path is rejected" T-LABEL
   [: SEARCH-LINK-B FS:OPEN-DIR drop ;] E-FS-OPEN TTHROWSQ
   [: top s" l/b/f" READ$ drop ;] E-FS-OPEN TTHROWSQ
   top FS:CLOSE-DIR ;
\ REMOVE-TREE cannot list a search-only directory.
: SEARCH-ONLY ( -- )
   SEARCH-SETUP
   [: SEARCH-OPENS ;] [: SEARCH-A MODE-0700 FS:CHMOD-MODE ;] finally ;

HB-TARGET-LINUX? [if]
FS-ABI:S-IFLNK constant S-IFLNK
$1FF constant PERMS
384 constant MODE-0600
variable DIR
variable TEMP
variable PLAIN
create LINK-BUF PATH-CAP allot

: AT-DIR ( -- str ) 16 s" root/at" PATH ;
: AT-FILE ( -- str ) 17 s" root/at/file" PATH ;
: AT-LINK ( -- str ) 18 s" root/at/link" PATH ;
: AT-SUB ( -- str ) 19 s" root/at/sub" PATH ;
: AT-SUB-LINK ( -- str ) 20 s" root/at/sublink" PATH ;
: AT-A ( -- str ) 21 s" root/at/a" PATH ;
: AT-C ( -- str ) 22 s" root/at/c" PATH ;
: MV ( -- str ) 23 s" root/mv" PATH ;
: MV-FILE ( -- str ) 24 s" root/mv/f" PATH ;

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
: STAT-SIZE ( str -- n ) {: s:str :}
   D s FS:TRY-STAT-AT TTRUE FS:STAT-SIZE@ ;
: ABSENT ( str -- ) {: s:str :}
   D s FS:TRY-STAT-AT FS:ENOENT FALSE-WITH ;
\ Runs q on each name every name word rejects.
: REJECTED ( [ fd str -- ] -- ) {: q :}
   D s" " q execute  D s" ." q execute  D s" .." q execute
   D s" sub/x" q execute  D s\" a\zb" q execute
   D LONG PATH-CAP 1 + >str q execute ;

: READS ( -- )
   D s" file" FS:TRY-OPEN-READ-AT OPENED-SIZE 8 T=
   D s" missing" FS:TRY-OPEN-READ-AT FS:ENOENT NONE-WITH
   D s" link" FS:TRY-OPEN-READ-AT FS:ELOOP NONE-WITH
   [: FS:TRY-OPEN-READ-AT FS:EINVAL NONE-WITH ;] REJECTED ;
: WRITES ( -- )
   D s" file" FS:TRY-OPEN-WRITE-AT SOME {: f:fd :}
   f s" AT" FD:WRITE-FULL f FS:CLOSE-DIR
   D s" file" s" AT-bytes" READ-CHECK
   D s" missing" FS:TRY-OPEN-WRITE-AT FS:ENOENT NONE-WITH
   s" missing" ABSENT
   D s" link" FS:TRY-OPEN-WRITE-AT FS:ELOOP NONE-WITH
   [: FS:TRY-OPEN-WRITE-AT FS:EINVAL NONE-WITH ;] REJECTED ;
: ENTRIES ( -- )
   D s" file" FS:TRY-OPEN-ENTRY-AT OPENED-SIZE 8 T=
   s" an O_PATH open of a symlink pins the link itself" T-LABEL
   D s" link" FS:TRY-OPEN-ENTRY-AT OPENED-SIZE 4 T=
   D s" missing" FS:TRY-OPEN-ENTRY-AT FS:ENOENT NONE-WITH
   [: FS:TRY-OPEN-ENTRY-AT FS:EINVAL NONE-WITH ;] REJECTED ;
\ FD-PATH names the descriptor's directory, leaves FMT's text intact, and
\ rejects a span the name would fill and a closed descriptor (ENOENT).
: FD-PATHS ( -- )
   $1ED 4 FMT:OCT-WIDTH$ {: mode:str :}
   D LINK-BUF PATH-CAP SPAN:MAKE FS:FD-PATH {: u:n :}
   LINK-BUF u >str AT-DIR T$=
   mode s" 0755" T$=
   [: D LINK-BUF AT-DIR STR:LEN 1- SPAN:MAKE FS:FD-PATH drop ;]
   E-FS-CAPACITY TTHROWSQ
   D s" sub" FS:TRY-OPEN-DIR-AT SOME {: f:fd :}
   f FS:CLOSE-DIR
   [: f LINK-BUF PATH-CAP SPAN:MAKE FS:FD-PATH drop ;] E-FS-STAT TTHROWSQ
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
   [: FS:TRY-STAT-AT FS:EINVAL FALSE-WITH ;] REJECTED ;

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
   [: 448 FS:TRY-MAKE-DIR-AT FS:EINVAL FALSE-WITH ;] REJECTED ;

: RENAME-TO ( fd str -- bool ) {: d:fd s:str :}
   D s" b" d s FS:TRY-RENAME-AT ;
: RENAME-NEW-TO ( fd str -- bool ) {: d:fd s:str :}
   D s" b" d s FS:TRY-RENAME-NEW-AT ;
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
   [: D s" b" FS:TRY-RENAME-AT FS:EINVAL FALSE-WITH ;] REJECTED
   [: D s" b" FS:TRY-RENAME-NEW-AT FS:EINVAL FALSE-WITH ;] REJECTED
   [: RENAME-TO FS:EINVAL FALSE-WITH ;] REJECTED
   [: RENAME-NEW-TO FS:EINVAL FALSE-WITH ;] REJECTED
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
   [: FS:TRY-REMOVE-FILE-AT FS:EINVAL FALSE-WITH ;] REJECTED
   [: FS:TRY-REMOVE-DIR-AT FS:EINVAL FALSE-WITH ;] REJECTED ;

: LINK-AS ( fd str -- bool ) {: d:fd s:str :}
   TEMP @ >FD d s FS:TRY-LINK-AT ;
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
   [: LINK-AS FS:EINVAL FALSE-WITH ;] REJECTED
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
HB-TARGET-LINUX? 0= [if]
: NAMED ( -- ) ;
[then]

\ A new file of that name, written and read through the descriptor; only
\ Linux has AT words that write.
HB-TARGET-LINUX? [if]
: WRITE-AT ( fd str -- ) {: dir:fd name:str :}
   dir FS:TEMP-AT {: t:fd :}
   t s" written" FD:WRITE-FULL
   t dir name FS:TRY-LINK-AT TTRUE
   t FS:CLOSE-DIR
   dir name s" written" READ-CHECK ;
[then]
HB-TARGET-LINUX? 0= [if]
: WRITE-AT ( fd str -- ) drop drop ;
[then]

: FOLLOWS-TO ( str str str -- )
   {: path:str file:str name:str :}
   path FS:OPEN-DIR-FOLLOW {: dir:fd :}
   dir file s" original" READ-CHECK
   dir name WRITE-AT
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
   [: BASIC BAD-PATHS REPLACED TYPES ERRORS LEAKS PARALLEL
      SEARCH-ONLY NAMED FOLLOWS ;]
   [: BUF @ VEC:DELETE TMP$ FS:REMOVE-TREE ;] finally
   T-REPORT s" fs-anchored-test: ok" type cr ;
;package
