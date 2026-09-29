\ fs-rename-noreplace-test.f - publish without replacing any destination entry.
\ Run: ~/.local/lib/habu/current/bin/hb --load lib/fs-rename-noreplace-test.f

require lib/test.f
require lib/fs-mutate.f
require lib/task.f
require lib/process-argv.f

package FS-NOREPLACE-TEST

create ROOT FS-PATH-CAP allot
variable ROOT-U
create PATHS 4 FS-PATH-CAP * allot
create TEXT 64 allot
FS-PATH-CAP SPAN-BUFFER: LINK-TEXT
variable RACE-DEST-U
variable RACE-READY
TYPED-VARIABLE ACL-ON bool
5000000000 constant RACE-WAIT-NS
TASK:MIN-STACK TASK:TASK WORKER-A
TASK:MIN-STACK TASK:TASK WORKER-B

: PATH ( n ptr u8 n -- ptr u8 n ) {: idx name bytes :}
   PATHS idx FS-PATH-CAP * + {: out :}
   out ROOT ROOT-U @ name bytes out JOIN-PATH ;

: SOURCE ( -- ptr u8 n ) 0 s" source" PATH ;
: DEST ( -- ptr u8 n ) 1 s" dest" PATH ;
: SOURCE-B ( -- ptr u8 n ) 2 s" source-b" PATH ;
: RACE-DEST ( -- ptr u8 n ) PATHS 3 FS-PATH-CAP * + RACE-DEST-U @ ;
: BAD-DEST ( -- ptr u8 n ) 1 s" absent/entry" PATH ;

: CONTENT ( ptr u8 n ptr u8 n -- ) {: path:ptr pathu expected:ptr expectedu :}
   path pathu TEXT 64 READ-ALL {: got :}
   TEXT got expected expectedu T$= ;

: SOURCE-CONTENT ( -- ) SOURCE s" source bytes" CONTENT ;

: PUBLISH ( -- )
   SOURCE s" source bytes" WRITE-ALL
   SOURCE DEST RENAME-NOREPLACE TTRUE
   SOURCE FS-TRY-LSTAT TFALSE
   DEST s" source bytes" CONTENT
   DEST REMOVE-FILE ;

: EXISTING-FILE ( -- )
   SOURCE s" source bytes" WRITE-ALL
   DEST s" destination bytes" WRITE-ALL
   SOURCE DEST RENAME-NOREPLACE TFALSE
   SOURCE-CONTENT
   DEST s" destination bytes" CONTENT
   SOURCE REMOVE-FILE DEST REMOVE-FILE ;

: DANGLING-LINK ( -- )
   SOURCE s" source bytes" WRITE-ALL
   s" absent-target" DEST MAKE-SYMLINK
   DEST EXISTS? TFALSE
   DEST SYMLINK? TTRUE
   SOURCE DEST RENAME-NOREPLACE TFALSE
   SOURCE-CONTENT
   DEST SYMLINK? TTRUE
   DEST LINK-TEXT READ-LINK {: got :}
   LINK-TEXT got SPAN:TAKE SPAN:$ s" absent-target" T$=
   SOURCE REMOVE-FILE DEST REMOVE-FILE ;

: LIVE-LINK ( -- )
   SOURCE s" source bytes" WRITE-ALL
   s" source" DEST MAKE-SYMLINK
   DEST FILE? TTRUE
   SOURCE DEST RENAME-NOREPLACE TFALSE
   SOURCE-CONTENT
   DEST SYMLINK? TTRUE
   DEST LINK-TEXT READ-LINK {: got :}
   LINK-TEXT got SPAN:TAKE SPAN:$ s" source" T$=
   SOURCE REMOVE-FILE DEST REMOVE-FILE ;

: EXISTING-DIR ( -- )
   SOURCE s" source bytes" WRITE-ALL
   DEST MAKE-DIR
   SOURCE DEST RENAME-NOREPLACE TFALSE
   SOURCE-CONTENT
   DEST DIR? TTRUE
   SOURCE REMOVE-FILE DEST REMOVE-DIR ;

: SAME-PATH ( -- )
   SOURCE s" source bytes" WRITE-ALL
   SOURCE SOURCE RENAME-NOREPLACE TFALSE
   SOURCE-CONTENT
   SOURCE REMOVE-FILE ;

: RACE-WAIT ( -- )
   mono-ns RACE-WAIT-NS + {: deadline :}
   1 RACE-READY atomic-add drop
   begin RACE-READY atomic@ 2 < while
      mono-ns deadline >= if E-FS-IO throw then
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

: WINNER? ( result<n,n> -- bool )
   MATCH result
      ok OF 0<> ENDOF
      err OF throw ENDOF
   ;MATCH ;

: RACE ( -- )
   3 s" race-dest" PATH nip RACE-DEST-U !
   SOURCE s" source bytes" WRITE-ALL
   SOURCE-B s" second bytes" WRITE-ALL
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
      SOURCE FS-TRY-LSTAT TFALSE
      SOURCE-B s" second bytes" CONTENT
      SOURCE-B REMOVE-FILE
   else
      RACE-DEST s" second bytes" CONTENT
      SOURCE-B FS-TRY-LSTAT TFALSE
      SOURCE-CONTENT
      SOURCE REMOVE-FILE
   then
   RACE-DEST REMOVE-FILE ;

: MISSING-SOURCE ( -- ) SOURCE DEST RENAME-NOREPLACE drop ;
: BAD-PARENT ( -- ) SOURCE BAD-DEST RENAME-NOREPLACE drop ;

: OTHER-ERRORS ( -- )
   [: MISSING-SOURCE ;] E-FS-IO TTHROWSQ
   SOURCE FS-TRY-LSTAT TFALSE
   DEST FS-TRY-LSTAT TFALSE
   SOURCE s" source bytes" WRITE-ALL
   [: BAD-PARENT ;] E-FS-IO TTHROWSQ
   SOURCE-CONTENT
   BAD-DEST FS-TRY-LSTAT TFALSE
   SOURCE REMOVE-FILE ;

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
   SOURCE FS-TRY-LSTAT if
      SOURCE ACL-REMOVE
   else
      DEST FS-TRY-LSTAT if DEST ACL-REMOVE then
   then
   false ACL-ON ! ;

\ On macOS this per-inode ACL permits link but denies unlink. The same ACL
\ appears on both names, so CLEAN clears it through whichever name survived.
: UNLINK-ERROR ( -- )
   HB-TARGET-MACOS? 0= if exit then
   SOURCE s" source bytes" WRITE-ALL
   ACL-ADD
   true ACL-ON !
   [: SOURCE DEST RENAME-NOREPLACE drop ;] E-FS-IO TTHROWSQ
   SOURCE FILE? TTRUE
   DEST FILE? TTRUE
   SOURCE-CONTENT
   DEST s" source bytes" CONTENT
   SOURCE DEST FS:SAMEFILE TTRUE ;

: CHECKS ( -- )
   PUBLISH EXISTING-FILE DANGLING-LINK LIVE-LINK EXISTING-DIR SAME-PATH
   RACE OTHER-ERRORS UNLINK-ERROR ;

: SETUP ( -- )
   s" habu-rename-noreplace" HB-TMP-MKDIR {: path:ptr bytes :}
   path ROOT bytes BYTE-COPY bytes ROOT-U !
   false ACL-ON ! ;

: CLEAN ( -- ) ACL-CLEAR ROOT ROOT-U @ REMOVE-TREE ;
: RUN ( -- ) T-RESET SETUP [: CHECKS ;] [: CLEAN ;] finally T-REPORT ;

RUN
;package
