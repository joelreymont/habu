\ A copied engine and its source tree must recognize baked modules when the
\ invocation directory is the copied tree's parent. The copy holds the engine's
\ first boot file, src/core/util.f, so it is a Habu tree and the engine's root
\ (src/core/include.f SOURCE-ROOT:ENGINE$). On macOS the same child runs again
\ under a sandbox that denies process-info-pidinfo, where proc_pidpath refuses
\ the engine its own path: the tree must still be found, and ENGINE-ID:PATH$
\ must still throw E-ENGINE-PATH. The engine does not bake lib/engine-id.f, so
\ the copy holds it for the child. Keep the children and their logs.
require lib/test.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process-argv.f
require lib/process-env.f
require lib/process-cwd.f
require lib/engine-candidate.f

package BOOT-RELOCATION-E2E
private

$4000 constant IO-CAP
30000 constant TIMEOUT-MS
create ROOT FS-PATH-CAP allot variable ROOT-U
create TREE FS-PATH-CAP allot variable TREE-U
create PATH FS-PATH-CAP allot variable PATH-U
create SRC FS-PATH-CAP allot variable SRC-U
create ENGINE FS-PATH-CAP allot variable ENGINE-U
create SANDBOX FS-PATH-CAP allot
create OUT IO-CAP allot
create ERR IO-CAP allot
variable OUT-U
variable ERR-U
variable RC

: ROOT$ ( -- ptr u8 n ) ROOT ROOT-U @ ;
: TREE$ ( -- ptr u8 n ) TREE TREE-U @ ;
: ENGINE$ ( -- ptr u8 n ) ENGINE ENGINE-U @ ;

: PATH! ( ptr u8 n -- )
   TREE$ 2swap PATH JOIN-PATH PATH-U ! ;

: COPY-SOURCE ( ptr u8 n -- ) {: rel:ptr relu:n :}
   rel relu PATH!
   SOURCE-ROOT:CWD$ rel relu SRC JOIN-PATH SRC-U !
   SRC SRC-U @ PATH PATH-U @ COPY-FILE-STREAM ;

: ENTRY+ ( ptr u8 n -- ) {: a:ptr u:n :}
   PATH PATH-U @ a u APPEND-FILE ;

\ With the argument `refused` the child first asserts that the kernel refused
\ it its path, so the sandboxed case cannot pass on a host that answers.
: WRITE-ENTRY ( -- )
   s" entry.f" PATH!
   S\" require lib/c2-memory.f\nrequire lib/engine-id.f\n" ENTRY+
   S\" package BOOT-RELOCATION-CHILD\npublic\n: RUN ( -- )\n" ENTRY+
   S\" SCRIPT-ARGC 0 > if [: ENGINE-ID:PATH$ 2drop ;] catch E-ENGINE-PATH <> if 77 throw then then\n" ENTRY+
   S\" SOURCE-ROOT:CURRENT$ s\q lib/c2-memory.f\q SOURCE-ROOT:JOIN ENGINE-PROVIDES? 0= if 76 throw then\n" ENTRY+
   S\" s\q boot relocation: ok\q type cr ;\n;package\nBOOT-RELOCATION-CHILD:RUN\n" ENTRY+ ;

: SETUP ( -- )
   s" habu-boot-relocation" HB-TMP-MKDIR {: a:ptr u:n :}
   a ROOT u BYTE-COPY u ROOT-U !
   ROOT$ s" tree" TREE JOIN-PATH TREE-U !
   s" bin" PATH! PATH PATH-U @ MAKE-DIRS
   s" lib/c2-memory" PATH! PATH PATH-U @ MAKE-DIRS
   s" src/core" PATH! PATH PATH-U @ MAKE-DIRS
   TREE$ s" bin/hb" ENGINE JOIN-PATH ENGINE-U !
   ENGINE-CANDIDATE:PATH$ ENGINE$ COPY-FILE-STREAM
   ENGINE$ CHMOD-X
   s" lib/errors.f" COPY-SOURCE
   s" lib/c2-memory.f" COPY-SOURCE
   s" lib/c2-memory/owner-runtime.f" COPY-SOURCE
   s" src/core/util.f" COPY-SOURCE
   s" lib/engine-id.f" COPY-SOURCE
   WRITE-ENTRY ;

: SAVE ( ptr u8 n ptr u8 n -- ) {: rel:ptr relu:n data:ptr size:n :}
   ROOT$ rel relu PATH JOIN-PATH {: pathu:n :}
   PATH pathu data size WRITE-ALL ;

: ENGINE-ARGV+ ( -- )
   s" --load" >LEN PROC-ARGV+
   s" tree/entry.f" >LEN PROC-ARGV+ ;

\ Run the staged argv under the executable, from the copied tree's parent.
: RUN-CHILD ( ptr u8 n -- ) {: exe:ptr exeu:n :}
   PROC-ENV-INHERIT-MISSING
   exe exeu >LEN ROOT$ >LEN
   OUT IO-CAP >LEN ERR IO-CAP >LEN TIMEOUT-MS >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-CAPTURE
   MATCH result
      ok OF PCAP-CAPTURED:UNMAKE {: outu:len erru:len :}
         outu LEN>N OUT-U ! erru LEN>N ERR-U ! 0 RC ! ENDOF
      err OF PCAP-FAILED:UNMAKE {: outu:len erru:len code:rc :}
         outu LEN>N OUT-U ! erru LEN>N ERR-U ! code RC>N RC ! ENDOF
   ;MATCH ;

\ Keep the child's output beside the tree and check its verdict.
: VERDICT ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: out:ptr outu:n err:ptr erru:n label:ptr labelu:n :}
   out outu OUT OUT-U @ SAVE
   err erru ERR ERR-U @ SAVE
   RC @ 0<> if OUT OUT-U @ type ERR ERR-U @ type then
   label labelu T-LABEL
   RC @ 0 T=
   OUT OUT-U @ s" boot relocation: ok" CONTAINS? TTRUE ;

: PLAIN ( -- )
   PROC-CWD:ARGV-ENV-CWD-RESET
   ENGINE-ARGV+
   ENGINE$ RUN-CHILD
   s" child.out" s" child.err"
   s" the copied engine recognizes its baked module from a foreign cwd" VERDICT ;

: SANDBOX$ ( -- ptr u8 n )
   s" sandbox-exec" >LEN SANDBOX FIND-EXECUTABLE MATCH option
      none OF s" boot-relocation: required executable missing on PATH: sandbox-exec" 1 die ENDOF
      some OF LEN>N ENDOF
   ;MATCH
   SANDBOX swap ;

: REFUSED ( -- )
   PROC-CWD:ARGV-ENV-CWD-RESET
   s" -p" >LEN PROC-ARGV+
   s" (version 1)(allow default)(deny process-info-pidinfo)" >LEN PROC-ARGV+
   ENGINE$ >LEN PROC-ARGV+
   ENGINE-ARGV+
   s" --" >LEN PROC-ARGV+
   s" refused" >LEN PROC-ARGV+
   SANDBOX$ RUN-CHILD
   s" refused.out" s" refused.err"
   s" the copied engine finds its tree where the kernel refuses it its path" VERDICT ;

public

: RUN ( -- )
   T-RESET SETUP PLAIN
   HB-TARGET-MACOS? if REFUSED then
   s" boot relocation tree: " type ROOT$ type cr
   T-REPORT ;

;package

BOOT-RELOCATION-E2E:RUN
