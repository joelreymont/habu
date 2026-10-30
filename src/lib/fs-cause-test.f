\ fs-cause-test.f - real filesystem failures through the public FS load path.
\ Run: bin/hb --load src/lib/fs-cause-test.f

require src/lib/errors.f
require src/lib/test.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/fs-list.f
require src/lib/task.f

\ The private FS words this test reads, forwarded into its own package.
package FS
: FS-CAUSE-TEST:ENOENT ( -- n ) ENOENT ;
: FS-CAUSE-TEST:ENOTDIR ( -- n ) ENOTDIR ;
: FS-CAUSE-TEST:ELOOP ( -- n ) ELOOP ;
;package

package FS-CAUSE-TEST

22 constant DARWIN-EINVAL

create ROOT-BUF PATH-CAP allot
create PATH-BUF PATH-CAP allot
create READ-BUF 16 allot
300 constant LONG-U                 \ one name component past NAME_MAX, 255
create LONG-NAME LONG-U allot
variable ROOT-U
variable BARRIER
variable CAUSE-A
variable CAUSE-B
variable CAUSE-A-LATE
variable CAUSE-B-LATE
variable BAD-CODE
TASK:MIN-STACK TASK:TASK WORKER-A
TASK:MIN-STACK TASK:TASK WORKER-B

: ROOT$ ( -- ptr u8 n ) ROOT-BUF ROOT-U @ ;
: PATH ( ptr u8 n -- ptr u8 n )
   ROOT$ 2swap PATH-BUF FS:JOIN-PATH PATH-BUF swap ;
: SETUP ( -- )
   s" habu-fs-cause" FS:MKTEMP-DIR {: a:ptr u:n :}
   a ROOT-BUF u BYTE-COPY u ROOT-U !
   LONG-U 0 do [char] x LONG-NAME i + c! loop
   s" file" PATH s" a" FS:WRITE-ALL
   s" dir" PATH FS:MAKE-DIR
   s" loop" s" loop" PATH FS:MAKE-SYMLINK ;

: MISSING ( -- ) s" missing" PATH READ-BUF 16 FS:READ-ALL drop ;
: INTERMEDIATE ( -- ) s" file/child" PATH READ-BUF 16 FS:READ-ALL drop ;
: LOOPED ( -- ) s" loop" PATH READ-BUF 16 FS:READ-ALL drop ;
: TOO-LONG ( -- ) LONG-NAME LONG-U PATH READ-BUF 16 FS:READ-ALL drop ;
: TOO-LONG-WRITE ( -- ) LONG-NAME LONG-U PATH s" a" FS:WRITE-ALL ;
: NO-ROOM ( -- ) s" file" PATH READ-BUF 0 FS:READ-ALL drop ;
: DENIED ( -- ) s" dir/child" PATH READ-BUF 16 FS:READ-ALL drop ;
: BAD-PATH ( -- ) LONG-NAME PATH-CAP 1 + FS:>PATHZ drop ;
: REGULAR-LIST ( -- ) s" file" PATH [: 2drop ;] FS-LIST:EACH ;
: MISSING-LIST ( -- ) s" missing" PATH [: 2drop ;] FS-LIST:EACH ;
: MISSING-STAT ( -- ) s" missing" PATH FS:STAT-MODE drop ;
: MISSING-LSTAT ( -- ) s" missing" PATH FS:TRY-LSTAT drop ;
: CALLBACK-FAIL ( ptr u8 n -- ) 2drop MISSING ;
: CALLBACK-LIST ( -- ) ROOT$ [: CALLBACK-FAIL ;] FS-LIST:EACH ;

\ Both tasks hold a different failed open across the same barrier. Neither
\ side may change the other's task-local cause while the calls overlap.
: WAIT-BOTH ( -- )
   mono-ns 5000000000 + {: until:n :}
   begin BARRIER atomic@ 2 < while
      mono-ns until >= if E-PROC-TIMEOUT throw then
      TASK:PAUSE
   repeat ;

: A-BODY ( -- )
   [: MISSING ;] catch E-FS-OPEN <> if 1 BAD-CODE atomic-add drop then
   FS:ERRNO CAUSE-A !
   1 BARRIER atomic-add drop
   WAIT-BOTH
   FS:ERRNO CAUSE-A-LATE !
   0 TASK:RETURN ;

: B-BODY ( -- )
   [: s" AGENTS.md/child" READ-BUF 16 FS:READ-ALL drop ;] catch
   E-FS-OPEN <> if 1 BAD-CODE atomic-add drop then
   FS:ERRNO CAUSE-B !
   1 BARRIER atomic-add drop
   WAIT-BOTH
   FS:ERRNO CAUSE-B-LATE !
   0 TASK:RETURN ;

: JOIN-OK ( ptr n -- )
   TASK:JOIN MATCH result
      ok OF 0 T= ENDOF
      err OF 0 T= ENDOF
   ;MATCH ;

: CHECK-TASKS ( -- )
   0 BARRIER ! 0 BAD-CODE !
   ['] A-BODY WORKER-A TASK:ACTIVATE
   ['] B-BODY WORKER-B TASK:ACTIVATE
   WORKER-A JOIN-OK WORKER-B JOIN-OK
   BAD-CODE @ 0 T=
   CAUSE-A @ ENOENT T=
   CAUSE-A-LATE @ ENOENT T=
   CAUSE-B @ ENOTDIR T=
   CAUSE-B-LATE @ ENOTDIR T= ;

: CHECK-OS ( -- )
   [: MISSING ;] E-FS-OPEN TTHROWSQ  FS:ERRNO ENOENT T=
   s" file" PATH FS:EXISTS? TTRUE  FS:ERRNO 0 T=
   [: MISSING-STAT ;] E-FS-STAT TTHROWSQ  FS:ERRNO ENOENT T=
   MISSING-LSTAT FS:ERRNO ENOENT T=
   s" missing" PATH FS:EXISTS? TFALSE  FS:ERRNO ENOENT T=
   [: MISSING-LIST ;] E-FS-LIST-OPEN TTHROWSQ  FS:ERRNO ENOENT T=
   [: INTERMEDIATE ;] E-FS-OPEN TTHROWSQ  FS:ERRNO ENOTDIR T=
   [: LOOPED ;] E-FS-OPEN TTHROWSQ  FS:ERRNO ELOOP T=
   [: TOO-LONG ;] E-FS-OPEN TTHROWSQ  FS:ERRNO FS:ENAMETOOLONG T=
   [: TOO-LONG-WRITE ;] E-FS-OPEN TTHROWSQ  FS:ERRNO FS:ENAMETOOLONG T=
   s" dir" PATH FS:>PATHZ 0 chmod 0 T=
   [: DENIED ;] E-FS-OPEN TTHROWSQ  FS:ERRNO FS:EACCES T=
   s" dir" PATH FS:>PATHZ 448 chmod 0 T=
   [: MISSING ;] E-FS-OPEN TTHROWSQ  FS:ERRNO ENOENT T=
   [: BAD-PATH ;] E-FS-PATH TTHROWSQ  FS:ERRNO 0 T=
   [: MISSING ;] E-FS-OPEN TTHROWSQ  FS:ERRNO ENOENT T=
   [: NO-ROOM ;] E-FS-CAPACITY TTHROWSQ  FS:ERRNO 0 T=
   [: MISSING ;] E-FS-OPEN TTHROWSQ  FS:ERRNO ENOENT T=
   [: REGULAR-LIST ;] E-FS-LIST-READ TTHROWSQ
   \ getdirentries64 rejects a regular fd with EINVAL on Darwin; Linux's
   \ getdents64 reports ENOTDIR. Preserve the errno the host actually returned.
   HB-TARGET-MACOS? if
      FS:ERRNO DARWIN-EINVAL T=
   else
      FS:ERRNO ENOTDIR T=
   then
   [: CALLBACK-LIST ;] E-FS-OPEN TTHROWSQ
   FS:ERRNO ENOENT T= ;

public
: MAIN ( -- )
   SETUP CHECK-OS CHECK-TASKS
   ROOT$ FS:REMOVE-TREE
   T-REPORT s" fs-cause-test: ok" type cr ;
;package

FS-CAUSE-TEST:MAIN
