\ A seeded x86-64 build whose captured runtime leaves a prefix-provided row's
\ dispatch cell empty refuses at the registry gate (76, src/habu/
\ primitive-registry.f COMPLETE-PROVIDED, UNFILLED) and promotes no product.
\ Without that refusal the build emits an engine whose `evaluate` stub jumps
\ through the empty cell, and the product dies on its first load. An ARM64
\ record is its assembly body and has no cell, so a host on another machine
\ builds `--target linux-x86-64` with src/arch/x86-64/passes.f loaded before
\ tools/native-build.f. The private tree holds a copy of the checkout's build
\ sources whose src/habu/native-runtime.f SEAL leaves out
\ OUTER:INSTALL-EVALUATE. The sources are copies, not links: a source's identity
\ is its physical path, so a linked one lies outside the tree, and the build
\ refuses 74 when no compiler.f below the tree completes (tools/
\ build-producer.f COMPILER-DONE). The printed tree keeps the build's output.
require src/lib/test.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/process-cwd.f
require src/lib/engine-candidate.f
require src/lib/tree-copy.f

package PREFIX-PROVIDED-UNFILLED
private

$8000 constant IO-CAP
$4000 constant SRC-CAP
600000 constant BUILD-TIMEOUT-MS

create ROOT FS-PATH-CAP allot variable ROOT-U
create PATH FS-PATH-CAP allot
create SRC SRC-CAP allot variable SRC-U
create OUT IO-CAP allot variable OUT-U
create ERR IO-CAP allot variable ERR-U
variable RC

: ROOT$ ( -- ptr u8 n ) ROOT ROOT-U @ ;

: AT-A ( ptr u8 n -- ptr u8 n ) {: rel:ptr relu:n :}
   ROOT$ rel relu PATH JOIN-PATH PATH swap ;

: RUNTIME$ ( -- ptr u8 n ) s" src/habu/native-runtime.f" ;
: INSTALL$ ( -- ptr u8 n ) S\"    OUTER:INSTALL-EVALUATE\n" ;

\ The runtime manifest without the line that fills the evaluate cell.
: WRITE-RUNTIME ( -- )
   RUNTIME$ AT-A SRC SRC-CAP READ-ALL SRC-U !
   SRC SRC-U @ INSTALL$ FIND-SUB MATCH option
      none OF -1 ENDOF
      some OF IDX>N ENDOF
   ;MATCH {: at:n :}
   at 0 < if
      s" prefix-provided-unfilled: native-runtime.f SEAL runs no OUTER:INSTALL-EVALUATE" 1 die
   then
   INSTALL$ nip at + {: past:n :}
   RUNTIME$ AT-A SRC at WRITE-ALL
   RUNTIME$ AT-A SRC past + SRC-U @ past - APPEND-FILE ;

: SETUP ( -- )
   s" prefix-provided-unfilled" HB-TMP-MKDIR {: a:ptr u:n :}
   a ROOT u BYTE-COPY u ROOT-U !
   ROOT$ TREE-COPY:BUILD-SOURCES
   WRITE-RUNTIME ;

: CAPTURE-RESULT ( result<pcap:captured,pcap:failed> -- )
   MATCH result
      ok OF PCAP-CAPTURED:UNMAKE {: outu:len erru:len :}
         outu LEN>N OUT-U ! erru LEN>N ERR-U ! 0 RC ! ENDOF
      err OF PCAP-FAILED:UNMAKE {: outu:len erru:len code:rc :}
         outu LEN>N OUT-U ! erru LEN>N ERR-U ! code RC>N RC ! ENDOF
   ;MATCH ;

\ The production entry, as `bin/hb --load tools/native-build.f -- <out>` runs
\ it, in the private tree, for an x86-64 product on any host.
: BUILD ( -- )
   PROC-CWD:ARGV-ENV-CWD-RESET
   s" --load" >LEN PROC-ARGV+
   HB-TARGET-LINUX-X86-64? 0= if s" src/arch/x86-64/passes.f" >LEN PROC-ARGV+ then
   s" tools/native-build.f" >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   s" hb" AT-A >LEN PROC-ARGV+
   HB-TARGET-LINUX-X86-64? 0= if
      s" --target" >LEN PROC-ARGV+  s" linux-x86-64" >LEN PROC-ARGV+
   then
   PROC-ENV-INHERIT-MISSING
   ENGINE-CANDIDATE:PATH$ >LEN ROOT$ >LEN
   OUT IO-CAP >LEN ERR IO-CAP >LEN BUILD-TIMEOUT-MS >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-CAPTURE CAPTURE-RESULT
   s" build.out" AT-A OUT OUT-U @ WRITE-ALL
   s" build.err" AT-A ERR ERR-U @ WRITE-ALL ;

public

: RUN ( -- )
   T-RESET
   SETUP BUILD
   s" a build whose runtime leaves the evaluate cell empty refuses 76" T-LABEL
   RC @ 76 <> if OUT OUT-U @ type ERR ERR-U @ type then
   RC @ 76 T=
   OUT OUT-U @ S\" captured runtime leaves the dispatch cell empty for prefix-provided primitive evaluate-range\n" CONTAINS? TTRUE
   ERR ERR-U @ s" prims: prefix-provided row not in the captured runtime" CONTAINS? TTRUE
   s" the refused build promotes no product" T-LABEL
   s" hb" AT-A FILE? 0= TTRUE
   T-REPORT
   s" prefix-provided-unfilled tree: " type ROOT$ type cr ;

;package

PREFIX-PROVIDED-UNFILLED:RUN
