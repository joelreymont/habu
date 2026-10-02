\ A copied engine and its source tree must recognize baked modules when the
\ invocation directory is the copied tree's parent. Keep the child and logs.
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

: SETUP ( -- )
   s" habu-boot-relocation" HB-TMP-MKDIR {: a:ptr u:n :}
   a ROOT u BYTE-COPY u ROOT-U !
   ROOT$ s" tree" TREE JOIN-PATH TREE-U !
   s" bin" PATH! PATH PATH-U @ MAKE-DIRS
   s" lib" PATH! PATH PATH-U @ MAKE-DIRS
   TREE$ s" bin/hb" ENGINE JOIN-PATH ENGINE-U !
   ENGINE-CANDIDATE:PATH$ ENGINE$ COPY-FILE-STREAM
   ENGINE$ CHMOD-X
   s" lib/errors.f" COPY-SOURCE
   s" lib/c2-memory.f" COPY-SOURCE
   s" lib/c2-owner-runtime.f" COPY-SOURCE
   s" entry.f" PATH!
   PATH PATH-U @
      S\" require lib/c2-memory.f\npackage BOOT-RELOCATION-CHILD\npublic\n: RUN ( -- )\n   SOURCE-ROOT:CURRENT$ s\q lib/c2-memory.f\q SOURCE-ROOT:JOIN ENGINE-PROVIDES? 0= if 76 throw then\n   s\q boot relocation: ok\q type cr ;\n;package\nBOOT-RELOCATION-CHILD:RUN\n"
      WRITE-ALL ;

: SAVE ( ptr u8 n ptr u8 n -- ) {: rel:ptr relu:n data:ptr size:n :}
   ROOT$ rel relu PATH JOIN-PATH {: pathu:n :}
   PATH pathu data size WRITE-ALL ;

: RUN-CHILD ( -- )
   PROC-CWD:ARGV-ENV-CWD-RESET
   s" --load" >LEN PROC-ARGV+
   s" tree/entry.f" >LEN PROC-ARGV+
   PROC-ENV-INHERIT-MISSING
   ENGINE$ >LEN ROOT$ >LEN
   OUT IO-CAP >LEN ERR IO-CAP >LEN TIMEOUT-MS >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-CAPTURE
   MATCH result
      ok OF PCAP-CAPTURED:UNMAKE {: outu:len erru:len :}
         outu LEN>N OUT-U ! erru LEN>N ERR-U ! 0 RC ! ENDOF
      err OF PCAP-FAILED:UNMAKE {: outu:len erru:len code:rc :}
         outu LEN>N OUT-U ! erru LEN>N ERR-U ! code RC>N RC ! ENDOF
   ;MATCH
   s" child.out" OUT OUT-U @ SAVE
   s" child.err" ERR ERR-U @ SAVE
   RC @ 0<> if OUT OUT-U @ type ERR ERR-U @ type then
   s" the copied engine recognizes its baked module from a foreign cwd" T-LABEL
   RC @ 0 T=
   OUT OUT-U @ s" boot relocation: ok" CONTAINS? TTRUE ;

public

: RUN ( -- )
   T-RESET SETUP RUN-CHILD
   s" boot relocation tree: " type ROOT$ type cr
   T-REPORT ;

;package

BOOT-RELOCATION-E2E:RUN
