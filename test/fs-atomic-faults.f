\ fs-atomic-faults.f - atomic writes under host failures a preloaded shim makes.
\ A directory fsync the host accepted fails with EIO: the file's sync passes,
\ the rename lands and the reopened directory's sync fails, so the write is
\ E-FS-SYNC, the target holding the new bytes. renameat2 fails with EINVAL, as
\ on NFS: a claim of a free name is E-FS-IO, the name free and no sibling left.
\ With ATOMIC_FAIL_FILE set a regular file's fsync fails with EIO too, so each
\ call is E-FS-IO before its rename and every file keeps its old bytes.
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
create STAGED PATH-CAP allot   variable STAGED-U
create CACHED PATH-CAP allot   variable CACHED-U
create OUT IO-CAP allot
create ERR IO-CAP allot
create GOT IO-CAP allot
variable SIBLINGS

: ROOT$ ( -- ptr u8 n ) ROOT ROOT-U @ ;
: SHIM-C$ ( -- ptr u8 n ) SHIM-C SHIM-C-U @ ;
: SHIM-SO$ ( -- ptr u8 n ) SHIM-SO SHIM-SO-U @ ;
: TARGET$ ( -- ptr u8 n ) TARGET TARGET-U @ ;
: FREE$ ( -- ptr u8 n ) FREE FREE-U @ ;
: STAGED$ ( -- ptr u8 n ) STAGED STAGED-U @ ;
: CACHED$ ( -- ptr u8 n ) CACHED CACHED-U @ ;

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
   s" #include <stdlib.h>" C+
   s" #include <sys/stat.h>" C+
   s" int fsync(int fd) {" C+
   s"    int (*real)(int) =" C+
   S\"       (int (*)(int))dlsym(RTLD_NEXT, \"fsync\");" C+
   s"    int rc = real(fd);" C+
   s"    struct stat st;" C+
   s"    if (rc != 0 || fstat(fd, &st) != 0) return rc;" C+
   s"    if (S_ISDIR(st.st_mode) ||" C+
   S\"        (S_ISREG(st.st_mode) && getenv(\"ATOMIC_FAIL_FILE\"))) {" C+
   s"       errno = EIO; return -1; }" C+
   s"    return rc; }" C+
   s" int renameat2(int olddirfd, const char *oldpath, int newdirfd," C+
   s"               const char *newpath, unsigned int flags) {" C+
   s"    errno = EINVAL; return -1; }" C+ ;

\ The child prints four pairs of lines, one per call: the code its write,
\ claim or replacement threw, then FS:ERRNO.
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
   s"    catch SHOW" P+
   S\"    s\" ATOMIC_STAGED\" GETENV {: sp:ptr su:n :}" P+
   S\"    [: sp su [: s\" staged bytes\" FS:WRITE-ALL ;]" P+
   s"    FS:REPLACE-STAGED ;] catch SHOW" P+
   S\"    s\" ATOMIC_CACHED\" GETENV {: cp:ptr cu:n :}" P+
   S\"    [: cp cu [: s\" cached bytes\" FS:WRITE-ALL ;]" P+
   s"    FS:REPLACE-CACHED ;] catch SHOW ;" P+
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

: OLD-BYTES ( -- )
   TARGET$ s" old bytes" FS:WRITE-ALL
   STAGED$ s" old bytes" FS:WRITE-ALL
   CACHED$ s" old bytes" FS:WRITE-ALL ;

: SETUP ( -- )
   FS:CLEANUP-RESET
   s" habu-atomic-faults" FS:MKTEMP-DIR {: a:ptr u:n :}
   a ROOT u BYTE-COPY  u ROOT-U !
   ROOT$ FS:CLEANUP-TREE+
   ROOT$ s" shim.c" SHIM-C FS:JOIN-PATH SHIM-C-U !
   ROOT$ s" shim.so" SHIM-SO FS:JOIN-PATH SHIM-SO-U !
   ROOT$ s" target" TARGET FS:JOIN-PATH TARGET-U !
   ROOT$ s" free" FREE FS:JOIN-PATH FREE-U !
   ROOT$ s" staged" STAGED FS:JOIN-PATH STAGED-U !
   ROOT$ s" cached" CACHED FS:JOIN-PATH CACHED-U !
   SHIM-SOURCE
   SHIM-C$ SHIM CODEGEN:CONTENTS FS:WRITE-ALL
   CC
   CHILD-SOURCE
   OLD-BYTES ;

\ A fresh engine: LD_PRELOAD reaches only a process it starts.
: RUN ( bool -- n n n )   \ outu erru rc
   {: fail-file?:bool :}
   ENGINE-CANDIDATE:PATH$ {: engine:ptr engineu:n :}
   PROG CODEGEN:CONTENTS {: prog:ptr progu:n :}
   PROC-ARGV-ENV-RESET
   s" LD_PRELOAD" >LEN SHIM-SO$ >LEN PROC-ENV+
   s" ATOMIC_TARGET" >LEN TARGET$ >LEN PROC-ENV+
   s" ATOMIC_FREE" >LEN FREE$ >LEN PROC-ENV+
   s" ATOMIC_STAGED" >LEN STAGED$ >LEN PROC-ENV+
   s" ATOMIC_CACHED" >LEN CACHED$ >LEN PROC-ENV+
   fail-file? if s" ATOMIC_FAIL_FILE" >LEN s" 1" >LEN PROC-ENV+ then
   PROC-ENV-INHERIT-MISSING
   engine engineu >LEN prog progu >LEN
   OUT IO-CAP >LEN ERR IO-CAP >LEN PROCESS:WAIT-FOREVER
   RUN-ARGV-ENV-STDIN-CAPTURE-OUTCOME
   PROCESS:ENDING>RC RC>N
   {: outu:len erru:len rc:n :}
   outu LEN>N erru LEN>N rc ;

: WANT$ ( -- ptr u8 n )
   SB-RESET
   E-FS-SYNC FMT:SB-INT 10 SB-APPEND-C
   EIO FMT:SB-INT 10 SB-APPEND-C
   E-FS-IO FMT:SB-INT 10 SB-APPEND-C
   EINVAL FMT:SB-INT 10 SB-APPEND-C
   E-FS-SYNC FMT:SB-INT 10 SB-APPEND-C
   EIO FMT:SB-INT 10 SB-APPEND-C
   0 FMT:SB-INT 10 SB-APPEND-C
   0 FMT:SB-INT 10 SB-APPEND-C
   SB$ ;

: FILE-WANT$ ( -- ptr u8 n )
   SB-RESET
   4 0 ?do
      E-FS-IO FMT:SB-INT 10 SB-APPEND-C
      EIO FMT:SB-INT 10 SB-APPEND-C
   loop
   SB$ ;

: COUNT-SIBLING ( ptr u8 n -- )
   s" .tmp-" STARTS-WITH? if 1 SIBLINGS +! then ;

: NO-LEFTOVER ( -- )
   s" ... and the claimed name is still free" T-LABEL
   FREE$ FS:TRY-LSTAT TFALSE
   s" ... and no sibling is left" T-LABEL
   0 SIBLINGS !  ROOT$ [: COUNT-SIBLING ;] FS-LIST:EACH
   SIBLINGS @ 0 T= ;

: FAULTS-CASE ( -- )
   false RUN {: outu:n erru:n rc:n :}
   OUT outu WANT$ STR= 0= rc 0 <> or if ERR erru type cr then
   s" write E-FS-SYNC, EIO; claim E-FS-IO, EINVAL; staged E-FS-SYNC, EIO; cached 0, 0"
   T-LABEL
   OUT outu WANT$ T$=
   s" the child engine exits cleanly" T-LABEL
   rc 0 T=
   s" ... and the target holds the new bytes" T-LABEL
   TARGET$ GOT IO-CAP FS:READ-ALL {: gotu:n :}
   GOT gotu s" new bytes" T$=
   s" ... and staged holds the staged bytes" T-LABEL
   STAGED$ GOT IO-CAP FS:READ-ALL {: stagedu:n :}
   GOT stagedu s" staged bytes" T$=
   s" ... and cached holds the cached bytes" T-LABEL
   CACHED$ GOT IO-CAP FS:READ-ALL {: cachedu:n :}
   GOT cachedu s" cached bytes" T$=
   NO-LEFTOVER ;

: HOLDS-OLD ( ptr u8 n -- )
   GOT IO-CAP FS:READ-ALL {: gotu:n :}
   GOT gotu s" old bytes" T$= ;

: FILE-FAULTS-CASE ( -- )
   OLD-BYTES
   true RUN {: outu:n erru:n rc:n :}
   OUT outu FILE-WANT$ STR= 0= rc 0 <> or if ERR erru type cr then
   s" a file fsync's EIO: write, claim, staged and cached E-FS-IO, EIO"
   T-LABEL
   OUT outu FILE-WANT$ T$=
   s" the child engine exits cleanly" T-LABEL
   rc 0 T=
   s" ... and the target holds the old bytes" T-LABEL
   TARGET$ HOLDS-OLD
   s" ... and staged holds the old bytes" T-LABEL
   STAGED$ HOLDS-OLD
   s" ... and cached holds the old bytes" T-LABEL
   CACHED$ HOLDS-OLD
   NO-LEFTOVER ;

public

: ATOMIC-FAULTS-TEST-MAIN ( -- )
   T-RESET
   SETUP
   FAULTS-CASE
   FILE-FAULTS-CASE
   FS:CLEANUP-RUN
   T-REPORT
   s" fs-atomic-faults: ok" type cr ;

;package

ATOMIC-FAULTS-TEST:ATOMIC-FAULTS-TEST-MAIN
