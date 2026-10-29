\ A copied engine and its source tree must recognize baked modules when the
\ invocation directory is the copied tree's parent. The copy holds the engine's
\ first boot file, src/core/util.f, so it is a Habu tree and the engine's root
\ (src/core/include.f SOURCE-ROOT:ENGINE$). On macOS the same child runs again
\ under a sandbox that denies process-info-pidinfo, where proc_pidpath refuses
\ the engine its own path: the tree must still be found, and ENGINE-ID:PATH$
\ must still throw E-ENGINE-PATH. The engine does not bake src/lib/engine-id.f, so
\ the copy holds it for the child. Keep the children and their logs.
require src/lib/string.f
require src/lib/test.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/process-cwd.f
require src/lib/engine-candidate.f
require test/pcap-result.f

package BOOT-RELOCATION-E2E
private

$4000 constant IO-CAP
30000 constant TIMEOUT-MS
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

: ROOT$ ( -- ptr u8 n ) ROOT ROOT-U @ ;
: TREE$ ( -- ptr u8 n ) TREE TREE-U @ ;
: ENGINE$ ( -- ptr u8 n ) ENGINE ENGINE-U @ ;

: PATH! ( ptr u8 n -- )
   TREE$ 2swap PATH FS:JOIN-PATH PATH-U ! ;

: COPY-SOURCE ( ptr u8 n -- ) {: rel:ptr relu:n :}
   rel relu PATH!
   SOURCE-ROOT:CWD$ rel relu SRC FS:JOIN-PATH SRC-U !
   SRC SRC-U @ PATH PATH-U @ FS:COPY-FILE-STREAM ;

: ENTRY+ ( ptr u8 n -- ) {: a:ptr u:n :}
   PATH PATH-U @ a u FS:APPEND-FILE ;

\ With the argument `refused` the child first asserts that the kernel refused
\ it its path, so the sandboxed case cannot pass on a host that answers.
: WRITE-ENTRY ( -- )
   s" entry.f" PATH!
   S\" require src/lib/c2-memory.f\nrequire src/lib/engine-id.f\n" ENTRY+
   S\" package BOOT-RELOCATION-CHILD\npublic\n: RUN ( -- )\n" ENTRY+
   S\" SCRIPT-ARGC 0 > if [: ENGINE-ID:PATH$ 2drop ;] catch E-ENGINE-PATH <> if 77 throw then then\n" ENTRY+
   S\" SOURCE-ROOT:CURRENT$ s\q src/lib/c2-memory.f\q SOURCE-ROOT:JOIN ENGINE-PROVIDES? 0= if 76 throw then\n" ENTRY+
   S\" s\q boot relocation: ok\q type cr ;\n;package\nBOOT-RELOCATION-CHILD:RUN\n" ENTRY+ ;

: SETUP ( -- )
   s" habu-boot-relocation" FS:MKTEMP-DIR {: a:ptr u:n :}
   a ROOT u BYTE-COPY u ROOT-U !
   ROOT$ s" tree" TREE FS:JOIN-PATH TREE-U !
   s" bin" PATH! PATH PATH-U @ FS:MAKE-DIRS
   s" src/lib/c2-memory" PATH! PATH PATH-U @ FS:MAKE-DIRS
   s" src/core" PATH! PATH PATH-U @ FS:MAKE-DIRS
   TREE$ s" bin/hb" ENGINE FS:JOIN-PATH ENGINE-U !
   ENGINE-CANDIDATE:PATH$ ENGINE$ FS:COPY-FILE-STREAM
   ENGINE$ FS:CHMOD-X
   s" src/lib/errors.f" COPY-SOURCE
   s" src/lib/c2-memory.f" COPY-SOURCE
   s" src/lib/c2-memory/owner-runtime.f" COPY-SOURCE
   s" src/core/util.f" COPY-SOURCE
   s" src/lib/engine-id.f" COPY-SOURCE
   WRITE-ENTRY ;

: SAVE ( ptr u8 n ptr u8 n -- ) {: rel:ptr relu:n data:ptr size:n :}
   ROOT$ rel relu PATH FS:JOIN-PATH {: pathu:n :}
   PATH pathu data size FS:WRITE-ALL ;

: ENGINE-ARGV+ ( -- )
   s" --load" >LEN PROC-ARGV+
   s" tree/entry.f" >LEN PROC-ARGV+ ;

\ Run the staged argv under the executable, from the copied tree's parent.
: RUN-CHILD ( ptr u8 n -- ) {: exe:ptr exeu:n :}
   PROC-ENV-INHERIT-MISSING
   exe exeu >LEN ROOT$ >LEN
   OUT IO-CAP >LEN ERR IO-CAP >LEN TIMEOUT-MS >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-CAPTURE
   PCAP-RESULT:>N RC ! ERR-U ! OUT-U ! ;

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
