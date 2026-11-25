\ A copied engine and its source tree recognize baked modules when the
\ invocation directory is the copied tree's parent: the copy holds the first
\ boot file, src/core/exit-status.f, so it is the engine's root (src/core/include.f
\ SOURCE-ROOT:ENGINE$). On macOS the child reruns in a sandbox denying
\ process-info-pidinfo: the tree is still found and PROCESS:OWN-PATH throws
\ E-ENGINE-PATH.
require src/core/exit-status.f
require src/lib/string.f
require src/lib/test/assert.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/process-cwd.f
require src/lib/engine-candidate.f
require test/pcap-result.f
require src/lib/process.f

package BOOT-RELOCATION-E2E
private

$4000 constant IO-CAP
create ROOT PATH-CAP allot variable ROOT-U
create TREE PATH-CAP allot variable TREE-U
create PATH PATH-CAP allot variable PATH-U
create SRC PATH-CAP allot variable SRC-U
create ENGINE PATH-CAP allot variable ENGINE-U
create SANDBOX PATH-CAP allot
create OUT IO-CAP allot
create ERR IO-CAP allot
variable OUT-U
variable ERR-U
variable RC

: ROOT$ ( -- str ) ROOT ROOT-U @ >str ;
: TREE$ ( -- str ) TREE TREE-U @ >str ;
: ENGINE$ ( -- str ) ENGINE ENGINE-U @ >str ;

: PATH! ( str -- )
   TREE$ swap PATH FS:JOIN-PATH PATH-U ! ;

: COPY-SOURCE ( str -- ) {: rel:str :}
   rel PATH!
   SOURCE-ROOT:CWD$ rel SRC FS:JOIN-PATH SRC-U !
   SRC SRC-U @ >str PATH PATH-U @ >str FS:COPY-FILE-STREAM ;

: ENTRY+ ( str -- ) {: s:str :}
   PATH PATH-U @ >str s FS:APPEND-FILE ;

\ With the argument `rejected` the child first asserts that the kernel rejected
\ it its path, so the sandboxed case cannot pass on a host that answers.
: WRITE-ENTRY ( -- )
   s" entry.f" PATH!
   S\" require src/lib/c2-memory.f\nrequire src/lib/process.f\n" ENTRY+
   S\" package BOOT-RELOCATION-CHILD\npublic\n: RUN ( -- )\n" ENTRY+
   S\" SCRIPT-ARGC 0 > if [: PROCESS:OWN-PATH drop ;] catch E-ENGINE-PATH <> if 77 throw then then\n" ENTRY+
   S\" SOURCE-ROOT:CURRENT$ s\q src/lib/c2-memory.f\q SOURCE-ROOT:JOIN REQUIRE-REG:ENGINE-PROVIDES? 0= if 76 throw then\n" ENTRY+
   S\" s\q boot relocation: ok\q type cr ;\n;package\nBOOT-RELOCATION-CHILD:RUN\n" ENTRY+ ;

: SETUP ( -- )
   s" habu-boot-relocation" FS:MKTEMP-DIR {: dir:str :}
   dir FS:CLEANUP-TREE+
   dir STR:DATA ROOT dir STR:LEN BYTE-COPY dir STR:LEN ROOT-U !
   ROOT$ s" tree" TREE FS:JOIN-PATH TREE-U !
   s" bin" PATH! PATH PATH-U @ >str FS:MAKE-DIRS
   s" src/lib" PATH! PATH PATH-U @ >str FS:MAKE-DIRS
   s" src/core" PATH! PATH PATH-U @ >str FS:MAKE-DIRS
   TREE$ s" bin/hb" ENGINE FS:JOIN-PATH ENGINE-U !
   ENGINE-CANDIDATE:PATH$ ENGINE$ FS:COPY-FILE-STREAM
   ENGINE$ FS:CHMOD-X
   s" src/lib/error.f" COPY-SOURCE
   s" src/lib/c2-memory.f" COPY-SOURCE
   s" src/core/exit-status.f" COPY-SOURCE
   WRITE-ENTRY ;

: SAVE ( str str -- ) {: rel:str data:str :}
   ROOT$ rel PATH FS:JOIN-PATH {: pathu:n :}
   PATH pathu >str data FS:WRITE-ALL ;

: ENGINE-ARGV+ ( -- )
   s" --load" PROC-ARGV+
   s" tree/entry.f" PROC-ARGV+ ;

: RUN-CHILD ( str -- ) {: exe:str :}
   PROC-ENV-INHERIT-MISSING
   exe ROOT$
   OUT IO-CAP >LEN ERR IO-CAP >LEN PROCESS:WAIT-FOREVER
   PROC-CWD:RUN-ARGV-ENV-CWD-CAPTURE
   PCAP-RESULT:>N RC ! ERR-U ! OUT-U ! ;

\ Keep the child's output beside the tree and check its verdict.
: VERDICT ( str str str -- )
   {: out:str err:str label:str :}
   out OUT OUT-U @ >str SAVE
   err ERR ERR-U @ >str SAVE
   RC @ 0<> if OUT OUT-U @ >str type ERR ERR-U @ >str type then
   label T-LABEL
   RC @ 0 T=
   OUT OUT-U @ >str s" boot relocation: ok" STR:CONTAINS? TTRUE ;

: PLAIN ( -- )
   PROC-CWD:ARGV-ENV-CWD-RESET
   ENGINE-ARGV+
   ENGINE$ RUN-CHILD
   s" child.out" s" child.err"
   s" the copied engine recognizes its baked module from a foreign cwd" VERDICT ;

: SANDBOX$ ( -- str )
   s" sandbox-exec" SANDBOX FIND-EXECUTABLE MATCH option
      none OF s" boot-relocation: required executable missing on PATH: sandbox-exec"
         EXIT-STATUS:UNAVAILABLE die ENDOF
      some OF ENDOF
   ;MATCH
   SANDBOX swap >str ;

: REJECTED ( -- )
   PROC-CWD:ARGV-ENV-CWD-RESET
   s" -p" PROC-ARGV+
   s" (version 1)(allow default)(deny process-info-pidinfo)" PROC-ARGV+
   ENGINE$ PROC-ARGV+
   ENGINE-ARGV+
   s" --" PROC-ARGV+
   s" rejected" PROC-ARGV+
   SANDBOX$ RUN-CHILD
   s" rejected.out" s" rejected.err"
   s" the copied engine finds its tree where the kernel rejects it its path" VERDICT ;

public

: RUN ( -- )
   T-RESET SETUP PLAIN
   HB-TARGET-MACOS? if REJECTED then
   T-REPORT ;

;package

BOOT-RELOCATION-E2E:RUN
