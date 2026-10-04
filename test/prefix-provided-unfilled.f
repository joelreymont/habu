\ A seeded build whose captured runtime leaves a prefix-provided row's dispatch
\ cell empty refuses at the registry gate (76, src/habu/primitive-registry.f
\ UNFILLED) and promotes no product. Without that refusal the build emits an
\ engine whose `evaluate` stub jumps through the empty cell, and the product
\ dies on its first load. The private tree links the checkout except
\ src/habu/native-runtime.f, a copy whose SEAL leaves out
\ OUTER:INSTALL-EVALUATE. The printed tree keeps the build's output.
require lib/test.f
require lib/string.f
require lib/fs.f
require lib/fs-mutate.f
require lib/fs-list.f
require lib/process-argv.f
require lib/process-env.f
require lib/process-cwd.f
require lib/engine-candidate.f

package PREFIX-PROVIDED-UNFILLED
private

$8000 constant IO-CAP
$4000 constant SRC-CAP
600000 constant BUILD-TIMEOUT-MS

create ROOT FS-PATH-CAP allot variable ROOT-U
create PATH FS-PATH-CAP allot
create TARGET FS-PATH-CAP allot variable TARGET-U
create REL FS-PATH-CAP allot variable REL-U
create SRC SRC-CAP allot variable SRC-U
create OUT IO-CAP allot variable OUT-U
create ERR IO-CAP allot variable ERR-U
variable RC

: ROOT$ ( -- ptr u8 n ) ROOT ROOT-U @ ;

: AT-A ( ptr u8 n -- ptr u8 n ) {: rel:ptr relu:n :}
   ROOT$ rel relu PATH JOIN-PATH PATH swap ;

: LINK ( ptr u8 n -- ) {: rel:ptr relu:n :}
   SOURCE-ROOT:CWD$ rel relu TARGET JOIN-PATH TARGET-U !
   TARGET TARGET-U @ rel relu AT-A MAKE-SYMLINK ;

: RUNTIME$ ( -- ptr u8 n ) s" src/habu/native-runtime.f" ;
: INSTALL$ ( -- ptr u8 n ) S\"    OUTER:INSTALL-EVALUATE\n" ;

\ Every entry of src but habu, which gets a directory of its own.
: LINK-SRC ( ptr u8 n -- ) {: name:ptr size:n :}
   name size s" habu" STR= if exit then
   s" src" name size REL JOIN-PATH REL-U !
   REL REL-U @ LINK ;

: LINK-HABU ( ptr u8 n -- ) {: name:ptr size:n :}
   name size s" native-runtime.f" STR= if exit then
   s" src/habu" name size REL JOIN-PATH REL-U !
   REL REL-U @ LINK ;

\ The runtime manifest without the line that fills the evaluate cell.
: WRITE-RUNTIME ( -- )
   SOURCE-ROOT:CWD$ RUNTIME$ TARGET JOIN-PATH TARGET-U !
   TARGET TARGET-U @ SRC SRC-CAP READ-ALL SRC-U !
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
   s" lib" LINK s" tools" LINK
   s" src/habu" AT-A MAKE-DIRS
   s" src" [: LINK-SRC ;] FS-LIST:EACH
   s" src/habu" [: LINK-HABU ;] FS-LIST:EACH
   WRITE-RUNTIME ;

: CAPTURE-RESULT ( result<pcap:captured,pcap:failed> -- )
   MATCH result
      ok OF PCAP-CAPTURED:UNMAKE {: outu:len erru:len :}
         outu LEN>N OUT-U ! erru LEN>N ERR-U ! 0 RC ! ENDOF
      err OF PCAP-FAILED:UNMAKE {: outu:len erru:len code:rc :}
         outu LEN>N OUT-U ! erru LEN>N ERR-U ! code RC>N RC ! ENDOF
   ;MATCH ;

\ The production entry, as `bin/hb --load tools/native-build.f -- <out>` runs
\ it, in the private tree.
: BUILD ( -- )
   PROC-CWD:ARGV-ENV-CWD-RESET
   s" --load" >LEN PROC-ARGV+
   s" tools/native-build.f" >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   s" hb" AT-A >LEN PROC-ARGV+
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
   OUT OUT-U @ s" captured runtime leaves the dispatch cell empty for prefix-provided primitive evaluate" CONTAINS? TTRUE
   ERR ERR-U @ s" prims: prefix-provided row not in the captured runtime" CONTAINS? TTRUE
   s" the refused build promotes no product" T-LABEL
   s" hb" AT-A FILE? 0= TTRUE
   T-REPORT
   s" prefix-provided-unfilled tree: " type ROOT$ type cr ;

;package

PREFIX-PROVIDED-UNFILLED:RUN
