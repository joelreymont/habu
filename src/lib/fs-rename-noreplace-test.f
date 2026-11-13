\ fs-rename-noreplace-test.f - FS:ATOMIC-CREATE-PRIVATE claims only a free name:
\ a taken one, a live link's target or a directory answers false untouched, a
\ dangling link is E-FS-STAT, and of two racing claims exactly one wins.
\ Run: hb --load src/lib/fs-rename-noreplace-test.f

require src/lib/test.f
require src/lib/string.f
require src/lib/fs-mutate.f
require src/lib/fs-list.f
require src/lib/task.f
require src/lib/test/wait.f

package FS-NOREPLACE-TEST

create ROOT PATH-CAP allot
variable ROOT-U
create PATHS 3 PATH-CAP * allot
create TEXT 64 allot
variable RACE-DEST-U
variable RACE-READY
variable ENTRIES
variable SIBLINGS
5000000000 constant RACE-WAIT-NS
TASK:MIN-STACK TASK:TASK WORKER-A
TASK:MIN-STACK TASK:TASK WORKER-B

: PATH ( n ptr u8 n -- ptr u8 n ) {: idx name bytes :}
   PATHS idx PATH-CAP * + {: out :}
   out ROOT ROOT-U @ name bytes out FS:JOIN-PATH ;

: TARGET ( -- ptr u8 n ) 0 s" target" PATH ;
: DEST ( -- ptr u8 n ) 1 s" dest" PATH ;
: RACE-DEST ( -- ptr u8 n ) PATHS 2 PATH-CAP * + RACE-DEST-U @ ;

: CONTENT ( ptr u8 n ptr u8 n -- ) {: path:ptr pathu expected:ptr expectedu :}
   path pathu TEXT 64 FS:READ-ALL {: got :}
   TEXT got expected expectedu T$= ;

: CLAIM ( ptr u8 n -- bool ) s" new bytes" FS:ATOMIC-CREATE-PRIVATE ;

: COUNT-ENTRY ( ptr u8 n -- )
   1 ENTRIES +!
   s" .tmp-" STARTS-WITH? if 1 SIBLINGS +! then ;
: COUNT-ENTRIES ( ptr u8 n -- )
   0 ENTRIES !  0 SIBLINGS !
   [: COUNT-ENTRY ;] FS-LIST:EACH ;

: PUBLISH ( -- )
   DEST CLAIM TTRUE
   DEST s" new bytes" CONTENT
   DEST FS:REMOVE-FILE ;

: EXISTING-FILE ( -- )
   DEST s" destination bytes" FS:WRITE-ALL
   DEST CLAIM TFALSE
   DEST s" destination bytes" CONTENT
   DEST FS:REMOVE-FILE ;

: LIVE-LINK ( -- )
   TARGET s" target bytes" FS:WRITE-ALL
   s" target" DEST FS:MAKE-SYMLINK
   DEST CLAIM TFALSE
   DEST FS:SYMLINK? TTRUE
   TARGET s" target bytes" CONTENT
   TARGET FS:REMOVE-FILE DEST FS:REMOVE-FILE ;

: DANGLING-LINK ( -- )
   s" target" DEST FS:MAKE-SYMLINK
   [: DEST CLAIM drop ;] E-FS-STAT TTHROWSQ
   DEST FS:SYMLINK? TTRUE
   TARGET FS:TRY-LSTAT TFALSE
   DEST FS:REMOVE-FILE ;

: EXISTING-DIR ( -- )
   DEST FS:MAKE-DIR
   DEST CLAIM TFALSE
   DEST FS:DIR? TTRUE
   DEST COUNT-ENTRIES  ENTRIES @ 0 T=
   DEST FS:REMOVE-DIR ;

: RACE-WAIT ( -- )
   mono-ns RACE-WAIT-NS + {: deadline :}
   1 RACE-READY atomic-add drop
   begin RACE-READY atomic@ 2 < while
      mono-ns deadline >= if E-PROC-TIMEOUT throw then
      TASK:PAUSE
   repeat ;

: RACE-A ( -- )
   RACE-WAIT
   RACE-DEST s" first bytes" FS:ATOMIC-CREATE-PRIVATE
   if 1 else 0 then TASK:RETURN ;

: RACE-B ( -- )
   RACE-WAIT
   RACE-DEST s" second bytes" FS:ATOMIC-CREATE-PRIVATE
   if 1 else 0 then TASK:RETURN ;

\ A worker's E-PROC-TIMEOUT is RACE-WAIT's deadline, the row's own: main names
\ it here, after both joins.
: WINNER? ( result<n,n> -- bool )
   MATCH result
      ok OF 0<> ENDOF
      err OF
         dup E-PROC-TIMEOUT = if s" fs-rename-noreplace-test RACE-WAIT" TEST-WAIT:EXPIRED then
         throw
      ENDOF
   ;MATCH ;

: RACE ( -- )
   2 s" race-dest" PATH nip RACE-DEST-U !
   0 RACE-READY !
   ['] RACE-A WORKER-A TASK:ACTIVATE
   ['] RACE-B WORKER-B TASK:ACTIVATE
   WORKER-A TASK:JOIN {: ar:result<n,n> :}
   WORKER-B TASK:JOIN {: br:result<n,n> :}
   ar WINNER? {: a:bool :}
   br WINNER? {: b:bool :}
   a if b TFALSE else b TTRUE then
   a if RACE-DEST s" first bytes" CONTENT else RACE-DEST s" second bytes" CONTENT then
   ROOT ROOT-U @ COUNT-ENTRIES
   SIBLINGS @ 0 T=
   ENTRIES @ 1 T=
   RACE-DEST FS:REMOVE-FILE ;

: CHECKS ( -- )
   PUBLISH EXISTING-FILE LIVE-LINK DANGLING-LINK EXISTING-DIR RACE ;

: SETUP ( -- )
   s" habu-rename-noreplace" FS:MKTEMP-DIR {: path:ptr bytes :}
   path ROOT bytes BYTE-COPY bytes ROOT-U ! ;

: CLEAN ( -- ) ROOT ROOT-U @ FS:REMOVE-TREE ;
: RUN ( -- ) T-RESET SETUP [: CHECKS ;] [: CLEAN ;] finally T-REPORT ;

RUN
;package
