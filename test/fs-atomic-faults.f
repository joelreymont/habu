\ fs-atomic-faults.f - atomic writes under host failures a preloaded shim makes.
\ A directory fsync the host accepted fails with EIO: the file's sync passes,
\ the rename lands and the reopened directory's sync fails, so the write is
\ E-FS-SYNC, the target holding the new bytes. renameat2 fails with EINVAL, as
\ on NFS: a claim of a free name is E-FS-IO, the name free and no sibling left.
HB-TARGET-LINUX? ?TARGET

require src/lib/string.f
require src/lib/test.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/fs-list.f
require src/lib/fmt.f
require src/lib/codegen.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/engine-candidate.f

package ATOMIC-FAULTS-TEST
private

$1000 constant IO-CAP
5 constant EIO
22 constant EINVAL

$800 CODEGEN:BUFFER SHIM
$800 CODEGEN:BUFFER PROG
create ROOT PATH-CAP allot   variable ROOT-U
create SHIM-C PATH-CAP allot   variable SHIM-C-U
create SHIM-SO PATH-CAP allot   variable SHIM-SO-U
create TARGET PATH-CAP allot   variable TARGET-U
create FREE PATH-CAP allot   variable FREE-U
create OUT IO-CAP allot
create ERR IO-CAP allot
create GOT IO-CAP allot
variable SIBLINGS

: ROOT$ ( -- ptr u8 n ) ROOT ROOT-U @ ;
: SHIM-C$ ( -- ptr u8 n ) SHIM-C SHIM-C-U @ ;
: SHIM-SO$ ( -- ptr u8 n ) SHIM-SO SHIM-SO-U @ ;
: TARGET$ ( -- ptr u8 n ) TARGET TARGET-U @ ;
: FREE$ ( -- ptr u8 n ) FREE FREE-U @ ;

: C+ ( ptr u8 n -- )
   SHIM CODEGEN:APPEND-STRING
   10 SHIM CODEGEN:APPEND-BYTE ;

: P+ ( ptr u8 n -- )
   PROG CODEGEN:APPEND-STRING
   10 PROG CODEGEN:APPEND-BYTE ;

: SHIM-SOURCE ( -- )
   SHIM CODEGEN:RESET
   s" #define _GNU_SOURCE" C+
   s" #include <dlfcn.h>" C+
   s" #include <errno.h>" C+
   s" #include <sys/stat.h>" C+
   s" int fsync(int fd) {" C+
   s"    int (*real)(int) =" C+
   S\"       (int (*)(int))dlsym(RTLD_NEXT, \"fsync\");" C+
   s"    int rc = real(fd);" C+
   s"    struct stat st;" C+
   s"    if (rc == 0 && fstat(fd, &st) == 0 && S_ISDIR(st.st_mode)) {" C+
   s"       errno = EIO; return -1; }" C+
   s"    return rc; }" C+
   s" int renameat2(int olddirfd, const char *oldpath, int newdirfd," C+
   s"               const char *newpath, unsigned int flags) {" C+
   s"    errno = EINVAL; return -1; }" C+ ;

\ The child prints four lines, one pair per call: the code its write or its
\ claim threw, then FS:ERRNO.
: CHILD-SOURCE ( -- )
   PROG CODEGEN:RESET
   s" require src/lib/fs-mutate.f" P+
   s" require src/lib/fmt.f" P+
   s" : SHOW ( n -- )" P+
   s"    FS:ERRNO {: code:n e:n :}" P+
   s"    code FMT:INT$ type cr e FMT:INT$ type cr ;" P+
   s" : GO ( -- )" P+
   S\"    s\" ATOMIC_TARGET\" GETENV {: p:ptr u:n :}" P+
   S\"    [: p u s\" new bytes\" FS:ATOMIC-WRITE-FILE ;] catch SHOW" P+
   S\"    s\" ATOMIC_FREE\" GETENV {: f:ptr fu:n :}" P+
   S\"    [: f fu s\" new bytes\" FS:ATOMIC-CREATE-PRIVATE drop ;]" P+
   s"    catch SHOW ;" P+
   s" GO" P+ ;

\ cc inherits this process's environment: its driver finds ld through PATH.
: CC ( -- )
   PROC-ARGV-ENV-RESET
   s" -shared" >LEN PROC-ARGV+
   s" -fPIC" >LEN PROC-ARGV+
   s" -std=gnu11" >LEN PROC-ARGV+
   s" -Wall" >LEN PROC-ARGV+
   s" -Werror" >LEN PROC-ARGV+
   s" -o" >LEN PROC-ARGV+
   SHIM-SO$ >LEN PROC-ARGV+
   SHIM-C$ >LEN PROC-ARGV+
   PROC-ENV-INHERIT-MISSING
   s" /usr/bin/cc" >LEN -1 >FD -1 >FD -1 >FD PROC-RUN-ARGV-ENV-IO-RC MATCH result
      ok OF drop ENDOF
      err OF drop s" fs-atomic-faults: cc failed" T-EX-FAIL die ENDOF
   ;MATCH ;

: SETUP ( -- )
   FS:CLEANUP-RESET
   s" habu-atomic-faults" FS:MKTEMP-DIR {: a:ptr u:n :}
   a ROOT u BYTE-COPY  u ROOT-U !
   ROOT$ FS:CLEANUP-TREE+
   ROOT$ s" shim.c" SHIM-C FS:JOIN-PATH SHIM-C-U !
   ROOT$ s" shim.so" SHIM-SO FS:JOIN-PATH SHIM-SO-U !
   ROOT$ s" target" TARGET FS:JOIN-PATH TARGET-U !
   ROOT$ s" free" FREE FS:JOIN-PATH FREE-U !
   SHIM-SOURCE
   SHIM-C$ SHIM CODEGEN:CONTENTS FS:WRITE-ALL
   CC
   CHILD-SOURCE
   TARGET$ s" old bytes" FS:WRITE-ALL ;

\ A fresh engine: LD_PRELOAD reaches only a process it starts.
: RUN ( -- n n n )   \ outu erru rc
   ENGINE-CANDIDATE:PATH$ {: engine:ptr engineu:n :}
   PROG CODEGEN:CONTENTS {: prog:ptr progu:n :}
   PROC-ARGV-ENV-RESET
   s" LD_PRELOAD" >LEN SHIM-SO$ >LEN PROC-ENV+
   s" ATOMIC_TARGET" >LEN TARGET$ >LEN PROC-ENV+
   s" ATOMIC_FREE" >LEN FREE$ >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   engine engineu >LEN prog progu >LEN
   OUT IO-CAP >LEN ERR IO-CAP >LEN PROCESS:WAIT-FOREVER
   RUN-ARGV-ENV-STDIN-CAPTURE-OUTCOME PROCESS:ENDING>RC RC>N
   {: outu:len erru:len rc:n :}
   outu LEN>N erru LEN>N rc ;

: WANT$ ( -- ptr u8 n )
   SB-RESET
   E-FS-SYNC FMT:SB-INT 10 SB-APPEND-C
   EIO FMT:SB-INT 10 SB-APPEND-C
   E-FS-IO FMT:SB-INT 10 SB-APPEND-C
   EINVAL FMT:SB-INT 10 SB-APPEND-C
   SB$ ;

: COUNT-SIBLING ( ptr u8 n -- )
   s" .tmp-" STARTS-WITH? if 1 SIBLINGS +! then ;

: FAULTS-CASE ( -- )
   RUN {: outu:n erru:n rc:n :}
   OUT outu WANT$ STR= 0= rc 0 <> or if ERR erru type cr then
   s" a failed dir sync is E-FS-SYNC, EIO; a refused claim E-FS-IO, EINVAL" T-LABEL
   OUT outu WANT$ T$=
   s" the child engine exits cleanly" T-LABEL
   rc 0 T=
   s" ... and the target holds the new bytes" T-LABEL
   TARGET$ GOT IO-CAP FS:READ-ALL {: gotu:n :}
   GOT gotu s" new bytes" T$=
   s" ... and the claimed name is still free" T-LABEL
   FREE$ FS:TRY-LSTAT TFALSE
   s" ... and no sibling is left" T-LABEL
   0 SIBLINGS !  ROOT$ [: COUNT-SIBLING ;] FS-LIST:EACH
   SIBLINGS @ 0 T= ;

public

: ATOMIC-FAULTS-TEST-MAIN ( -- )
   T-RESET
   SETUP
   FAULTS-CASE
   FS:CLEANUP-RUN
   T-REPORT
   s" fs-atomic-faults: ok" type cr ;

;package

ATOMIC-FAULTS-TEST:ATOMIC-FAULTS-TEST-MAIN
