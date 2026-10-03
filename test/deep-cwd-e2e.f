\ An engine started from a working directory longer than PATH-CAP refuses it by
\ name on one line: every root it resolves against fits PATH-CAP, and every
\ path below that directory is longer still. The same entry, run from the long
\ directory's parent, which fits, loads. FS-PATH-CAP bounds MAKE-DIRS and a
\ child's cwd, so /bin/mkdir makes the tail past PATH-CAP and /usr/bin/env -C
\ starts the engine in it, both naming it relative to its parent. Keep the
\ children's logs, except the tail: no FS-PATH-CAP-bounded walk (the gate
\ pool's REMOVE-TREE of a slot's HB_TMP) can remove it, so an rm child removes
\ it before the verdict.
require lib/test.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process-argv.f
require lib/process-env.f
require lib/process-cwd.f
require lib/process-command.f
require lib/engine-candidate.f

package DEEP-CWD-E2E
private

$4000 constant IO-CAP
30000 constant TIMEOUT-MS
create ROOT FS-PATH-CAP allot variable ROOT-U
create ENTRY FS-PATH-CAP allot variable ENTRY-U
create ENGINE FS-PATH-CAP allot variable ENGINE-U
create MID FS-PATH-CAP allot variable MID-U
create PATH FS-PATH-CAP allot
create OUT IO-CAP allot
create ERR IO-CAP allot
variable OUT-U
variable ERR-U
variable RC

: ROOT$ ( -- ptr u8 n ) ROOT ROOT-U @ ;
: ENTRY$ ( -- ptr u8 n ) ENTRY ENTRY-U @ ;
: ENGINE$ ( -- ptr u8 n ) ENGINE ENGINE-U @ ;
: MID$ ( -- ptr u8 n ) MID MID-U @ ;

\ TAIL is three 200-byte segments, MID is ROOT$/deep/TAIL and the long
\ directory is MID/TAIL: past PATH-CAP for any ROOT$ that leaves MID within it.
200 constant SEG-BYTES
SEG-BYTES 1 + 3 * 1 - constant TAIL-BYTES
create TAIL TAIL-BYTES allot

: TAIL$ ( -- ptr u8 n ) TAIL TAIL-BYTES ;

\ The first segment of TAIL: removing it below MID removes the whole tail.
: TAIL-TOP$ ( -- ptr u8 n ) TAIL SEG-BYTES ;

: TAIL! ( -- )
   TAIL-BYTES 0 ?do
      [char] d i 1 + SEG-BYTES 1 + mod 0= if drop [char] / then TAIL i + c!
   loop ;

: IN-MID ( ptr u8 n -- ) {: tool:ptr toolu:n :}
   MID$ >LEN PROC-CMD:CWD!
   tool toolu >LEN TIMEOUT-MS >MS PROC-CMD:RUN-RC MATCH result
      ok OF drop ENDOF
      err OF drop PROC-CMD:ERR$ type s" deep-cwd: tool failed" 1 die ENDOF
   ;MATCH ;

: SETUP ( -- )
   s" habu-deep-cwd" HB-TMP-MKDIR {: a:ptr u:n :}
   a ROOT u BYTE-COPY u ROOT-U !
   ENGINE-CANDIDATE:PATH$ SOURCE-ROOT:CANONICAL TTRUE {: e:ptr eu:n :}
   e ENGINE eu BYTE-COPY eu ENGINE-U !
   ROOT$ s" entry.f" ENTRY JOIN-PATH ENTRY-U !
   ENTRY$ S\" package DEEP-CWD-CHILD\npublic\n: RUN ( -- ) s\q deep cwd: ok\q type cr ;\n;package\nDEEP-CWD-CHILD:RUN\n" WRITE-ALL
   TAIL!
   ROOT$ s" deep" PATH JOIN-PATH {: pathu:n :}
   PATH pathu TAIL$ MID JOIN-PATH MID-U !
   MID$ MAKE-DIRS
   PROC-CMD:RESET
   s" -p" >LEN PROC-CMD:ARG+ TAIL$ >LEN PROC-CMD:ARG+
   s" /bin/mkdir" IN-MID ;

: SAVE ( ptr u8 n ptr u8 n -- ) {: rel:ptr relu:n data:ptr size:n :}
   ROOT$ rel relu PATH JOIN-PATH {: pathu:n :}
   PATH pathu data size WRITE-ALL ;

: ENGINE-ARGV+ ( -- )
   ENGINE$ >LEN PROC-ARGV+
   s" --load" >LEN PROC-ARGV+
   ENTRY$ >LEN PROC-ARGV+ ;

\ /usr/bin/env runs the staged argv from MID, after cd to its first argument
\ when it starts with -C.
: RUN-CHILD ( -- )
   PROC-ENV-INHERIT-MISSING
   s" /usr/bin/env" >LEN MID$ >LEN
   OUT IO-CAP >LEN ERR IO-CAP >LEN TIMEOUT-MS >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-CAPTURE
   MATCH result
      ok OF PCAP-CAPTURED:UNMAKE {: outu:len erru:len :}
         outu LEN>N OUT-U ! erru LEN>N ERR-U ! 0 RC ! ENDOF
      err OF PCAP-FAILED:UNMAKE {: outu:len erru:len code:rc :}
         outu LEN>N OUT-U ! erru LEN>N ERR-U ! code RC>N RC ! ENDOF
   ;MATCH ;

: KEEP ( ptr u8 n ptr u8 n -- ) {: out:ptr outu:n err:ptr erru:n :}
   out outu OUT OUT-U @ SAVE
   err erru ERR ERR-U @ SAVE ;

: FITS ( -- )
   PROC-CWD:ARGV-ENV-CWD-RESET
   ENGINE-ARGV+
   RUN-CHILD
   s" fits.out" s" fits.err" KEEP
   RC @ 0<> if OUT OUT-U @ type ERR ERR-U @ type then
   s" the entry loads from a long directory within PATH-CAP" T-LABEL
   RC @ 0 T=
   OUT OUT-U @ s" deep cwd: ok" CONTAINS? TTRUE ;

: DEEP ( -- )
   PROC-CWD:ARGV-ENV-CWD-RESET
   s" -C" >LEN PROC-ARGV+ TAIL$ >LEN PROC-ARGV+
   ENGINE-ARGV+
   RUN-CHILD
   PROC-CMD:RESET
   s" -r" >LEN PROC-CMD:ARG+ TAIL-TOP$ >LEN PROC-CMD:ARG+
   s" /bin/rm" IN-MID
   s" deep.out" s" deep.err" KEEP
   s" a working directory past PATH-CAP exits 74" T-LABEL
   RC @ 74 T=
   s" and prints nothing on stdout" T-LABEL
   OUT-U @ 0 T=
   s" and names itself on one line" T-LABEL
   ERR ERR-U @ S\" source root: the working directory does not resolve to a searchable directory within PATH-CAP bytes\n" T$= ;

public

: RUN ( -- )
   T-RESET SETUP FITS DEEP
   s" deep cwd tree: " type ROOT$ type cr
   T-REPORT ;

;package

DEEP-CWD-E2E:RUN
