\ fs-rename-noreplace-test.f - publish without replacing any destination entry.
\ Run: hb --load src/lib/fs-rename-noreplace-test.f

require src/lib/test.f
require src/lib/fs-mutate.f
require src/lib/task.f
require src/lib/process-argv.f
require src/lib/test/wait.f

\ The private FS words this test reads, forwarded into its own package.
package FS
: FS-NOREPLACE-TEST:RENAME-NOREPLACE ( ptr u8 n ptr u8 n -- bool ) RENAME-NOREPLACE ;
: FS-NOREPLACE-TEST:READ-LINK ( ptr u8 n SPAN:span<u8> -- n ) READ-LINK ;
;package

package FS-NOREPLACE-TEST

create ROOT PATH-CAP allot
variable ROOT-U
create PATHS 4 PATH-CAP * allot
create TEXT 64 allot
PATH-CAP SPAN-BUFFER: LINK-TEXT
variable RACE-DEST-U
variable RACE-READY
TYPED-VARIABLE ACL-ON bool
5000000000 constant RACE-WAIT-NS
TASK:MIN-STACK TASK:TASK WORKER-A
TASK:MIN-STACK TASK:TASK WORKER-B

: PATH ( n ptr u8 n -- ptr u8 n ) {: idx name bytes :}
   PATHS idx PATH-CAP * + {: out :}
   out ROOT ROOT-U @ name bytes out FS:JOIN-PATH ;

: SOURCE ( -- ptr u8 n ) 0 s" source" PATH ;
: DEST ( -- ptr u8 n ) 1 s" dest" PATH ;
: SOURCE-B ( -- ptr u8 n ) 2 s" source-b" PATH ;
: RACE-DEST ( -- ptr u8 n ) PATHS 3 PATH-CAP * + RACE-DEST-U @ ;
: BAD-DEST ( -- ptr u8 n ) 1 s" absent/entry" PATH ;

: CONTENT ( ptr u8 n ptr u8 n -- ) {: path:ptr pathu expected:ptr expectedu :}
   path pathu TEXT 64 FS:READ-ALL {: got :}
   TEXT got expected expectedu T$= ;

: SOURCE-CONTENT ( -- ) SOURCE s" source bytes" CONTENT ;

: PUBLISH ( -- )
   SOURCE s" source bytes" FS:WRITE-ALL
   SOURCE DEST RENAME-NOREPLACE TTRUE
   SOURCE FS:TRY-LSTAT TFALSE
   DEST s" source bytes" CONTENT
   DEST FS:REMOVE-FILE ;

: EXISTING-FILE ( -- )
   SOURCE s" source bytes" FS:WRITE-ALL
   DEST s" destination bytes" FS:WRITE-ALL
   SOURCE DEST RENAME-NOREPLACE TFALSE
   SOURCE-CONTENT
   DEST s" destination bytes" CONTENT
   SOURCE FS:REMOVE-FILE DEST FS:REMOVE-FILE ;

: DANGLING-LINK ( -- )
   SOURCE s" source bytes" FS:WRITE-ALL
   s" absent-target" DEST FS:MAKE-SYMLINK
   DEST FS:EXISTS? TFALSE
   DEST FS:SYMLINK? TTRUE
   SOURCE DEST RENAME-NOREPLACE TFALSE
   SOURCE-CONTENT
   DEST FS:SYMLINK? TTRUE
   DEST LINK-TEXT READ-LINK {: got :}
   LINK-TEXT got SPAN:TAKE SPAN:$ s" absent-target" T$=
   SOURCE FS:REMOVE-FILE DEST FS:REMOVE-FILE ;

: LIVE-LINK ( -- )
   SOURCE s" source bytes" FS:WRITE-ALL
   s" source" DEST FS:MAKE-SYMLINK
   DEST FS:FILE? TTRUE
   SOURCE DEST RENAME-NOREPLACE TFALSE
   SOURCE-CONTENT
   DEST FS:SYMLINK? TTRUE
   DEST LINK-TEXT READ-LINK {: got :}
   LINK-TEXT got SPAN:TAKE SPAN:$ s" source" T$=
   SOURCE FS:REMOVE-FILE DEST FS:REMOVE-FILE ;

: EXISTING-DIR ( -- )
   SOURCE s" source bytes" FS:WRITE-ALL
   DEST FS:MAKE-DIR
   SOURCE DEST RENAME-NOREPLACE TFALSE
   SOURCE-CONTENT
   DEST FS:DIR? TTRUE
   SOURCE FS:REMOVE-FILE DEST FS:REMOVE-DIR ;

: SAME-PATH ( -- )
   SOURCE s" source bytes" FS:WRITE-ALL
   SOURCE SOURCE RENAME-NOREPLACE TFALSE
   SOURCE-CONTENT
   SOURCE FS:REMOVE-FILE ;

\ Each task joins the rendezvous and waits for the other. A peer that never
\ arrives ends the wait with E-PROC-TIMEOUT: the task's JOIN records it and main
\ names it in WINNER?.
: RACE-WAIT ( -- )
   mono-ns RACE-WAIT-NS + {: deadline :}
   1 RACE-READY atomic-add drop
   begin RACE-READY atomic@ 2 < while
      mono-ns deadline >= if E-PROC-TIMEOUT throw then
      TASK:PAUSE
   repeat ;

: RACE-A ( -- )
   RACE-WAIT
   SOURCE RACE-DEST RENAME-NOREPLACE
   if 1 else 0 then TASK:RETURN ;

: RACE-B ( -- )
   RACE-WAIT
   SOURCE-B RACE-DEST RENAME-NOREPLACE
   if 1 else 0 then TASK:RETURN ;

\ A worker's E-PROC-TIMEOUT is RACE-WAIT's deadline, the row's own: main names
\ it here, after both joins. Every other code is rethrown as it came.
: WINNER? ( result<n,n> -- bool )
   MATCH result
      ok OF 0<> ENDOF
      err OF
         dup E-PROC-TIMEOUT = if s" fs-rename-noreplace-test RACE-WAIT" TEST-WAIT:EXPIRED then
         throw
      ENDOF
   ;MATCH ;

: RACE ( -- )
   3 s" race-dest" PATH nip RACE-DEST-U !
   SOURCE s" source bytes" FS:WRITE-ALL
   SOURCE-B s" second bytes" FS:WRITE-ALL
   0 RACE-READY !
   ['] RACE-A WORKER-A TASK:ACTIVATE
   ['] RACE-B WORKER-B TASK:ACTIVATE
   WORKER-A TASK:JOIN {: ar:result<n,n> :}
   WORKER-B TASK:JOIN {: br:result<n,n> :}
   ar WINNER? {: a:bool :}
   br WINNER? {: b:bool :}
   a if b TFALSE else b TTRUE then
   a if
      RACE-DEST s" source bytes" CONTENT
      SOURCE FS:TRY-LSTAT TFALSE
      SOURCE-B s" second bytes" CONTENT
      SOURCE-B FS:REMOVE-FILE
   else
      RACE-DEST s" second bytes" CONTENT
      SOURCE-B FS:TRY-LSTAT TFALSE
      SOURCE-CONTENT
      SOURCE FS:REMOVE-FILE
   then
   RACE-DEST FS:REMOVE-FILE ;

: MISSING-SOURCE ( -- ) SOURCE DEST RENAME-NOREPLACE drop ;
: BAD-PARENT ( -- ) SOURCE BAD-DEST RENAME-NOREPLACE drop ;

: OTHER-ERRORS ( -- )
   [: MISSING-SOURCE ;] E-FS-IO TTHROWSQ
   SOURCE FS:TRY-LSTAT TFALSE
   DEST FS:TRY-LSTAT TFALSE
   SOURCE s" source bytes" FS:WRITE-ALL
   [: BAD-PARENT ;] E-FS-IO TTHROWSQ
   SOURCE-CONTENT
   BAD-DEST FS:TRY-LSTAT TFALSE
   SOURCE FS:REMOVE-FILE ;

: CHMOD-RUN ( -- )
   s" /bin/chmod" >LEN -1 >FD -1 >FD -1 >FD PROC-RUN-ARGV-IO-RC
   MATCH result
      ok OF 0<> if E-FS-IO throw then ENDOF
      err OF drop E-FS-IO throw ENDOF
   ;MATCH ;

: ACL-ADD ( -- )
   PROC-ARGV-RESET
   s" +a" >LEN PROC-ARGV+
   s" everyone deny delete" >LEN PROC-ARGV+
   SOURCE >LEN PROC-ARGV+
   CHMOD-RUN ;

: ACL-REMOVE ( ptr u8 n -- )
   {: path:ptr bytes :}
   PROC-ARGV-RESET
   s" -a#" >LEN PROC-ARGV+
   s" 0" >LEN PROC-ARGV+
   path bytes >LEN PROC-ARGV+
   CHMOD-RUN ;

: ACL-CLEAR ( -- )
   ACL-ON @ 0= if exit then
   SOURCE FS:TRY-LSTAT if
      SOURCE ACL-REMOVE
   else
      DEST FS:TRY-LSTAT if DEST ACL-REMOVE then
   then
   false ACL-ON ! ;

\ On macOS this per-inode ACL permits link but denies unlink. The same ACL
\ appears on both names, so CLEAN clears it through whichever name survived.
: UNLINK-ERROR ( -- )
   HB-TARGET-MACOS? 0= if exit then
   SOURCE s" source bytes" FS:WRITE-ALL
   ACL-ADD
   true ACL-ON !
   [: SOURCE DEST RENAME-NOREPLACE drop ;] E-FS-IO TTHROWSQ
   SOURCE FS:FILE? TTRUE
   DEST FS:FILE? TTRUE
   SOURCE-CONTENT
   DEST s" source bytes" CONTENT
   SOURCE DEST FS:SAMEFILE TTRUE ;

: CHECKS ( -- )
   PUBLISH EXISTING-FILE DANGLING-LINK LIVE-LINK EXISTING-DIR SAME-PATH
   RACE OTHER-ERRORS UNLINK-ERROR ;

: SETUP ( -- )
   s" habu-rename-noreplace" FS:MKTEMP-DIR {: path:ptr bytes :}
   path ROOT bytes BYTE-COPY bytes ROOT-U !
   false ACL-ON ! ;

: CLEAN ( -- ) ACL-CLEAR ROOT ROOT-U @ FS:REMOVE-TREE ;
: RUN ( -- ) T-RESET SETUP [: CHECKS ;] [: CLEAN ;] finally T-REPORT ;

RUN
;package
