\ An engine started from a working directory longer than PATH-CAP refuses it by
\ name on one line: every root it resolves against fits PATH-CAP. The same entry
\ run from the long directory's parent loads. PATH-CAP bounds FS:MAKE-DIRS and a
\ child's cwd, so /bin/mkdir makes the tail and /usr/bin/env -C starts the
\ engine in it, both relative to the parent. The children's logs are kept.
require src/lib/string.f
require src/lib/test.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/fmt.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/process-cwd.f
require src/lib/process-command.f
require src/lib/engine-candidate.f
require test/pcap-result.f

package DEEP-CWD-E2E
private

$4000 constant IO-CAP
30000 constant TIMEOUT-MS
create ROOT PATH-CAP allot variable ROOT-U
create ENTRY PATH-CAP allot variable ENTRY-U
create ENGINE PATH-CAP allot variable ENGINE-U
create MID PATH-CAP allot variable MID-U
create SIDE PATH-CAP allot variable SIDE-U
create PATH PATH-CAP allot
create OUT IO-CAP allot
create ERR IO-CAP allot
variable OUT-U
variable ERR-U
variable RC

: ROOT$ ( -- ptr u8 n ) ROOT ROOT-U @ ;
: ENTRY$ ( -- ptr u8 n ) ENTRY ENTRY-U @ ;
: ENGINE$ ( -- ptr u8 n ) ENGINE ENGINE-U @ ;
: MID$ ( -- ptr u8 n ) MID MID-U @ ;
: SIDE$ ( -- ptr u8 n ) SIDE SIDE-U @ ;

\ TAIL is three 200-byte segments, MID is ROOT$/deep/TAIL and the long
\ directory is MID/TAIL, past PATH-CAP whenever MID fits PATH-CAP. The
\ removed root TOP$ is MID/<first segment>, and it fits PATH-CAP only for a
\ ROOT$ of at most ROOT-MAX bytes; SETUP dies naming a longer root and the
\ bound.
200 constant SEG-BYTES
SEG-BYTES 1 + 3 * 1 - constant TAIL-BYTES
\ 6: the /deep/ after ROOT$; 1: the slash before the first segment.
PATH-CAP 6 - TAIL-BYTES - 1 - SEG-BYTES - constant ROOT-MAX
create TAIL TAIL-BYTES allot

: TAIL$ ( -- ptr u8 n ) TAIL TAIL-BYTES ;

\ The first segment of TAIL: removing it below MID removes the whole tail.
: TAIL-TOP$ ( -- ptr u8 n ) TAIL SEG-BYTES ;

: TAIL! ( -- )
   TAIL-BYTES 0 ?do
      [char] d i 1 + SEG-BYTES 1 + mod 0= if drop [char] / then TAIL i + c!
   loop ;

: IN-MID ( CMD:command ptr u8 n -- CMD:command )
   {: tool:ptr toolu:n :}
   MID$ CMD:CWD!
   tool toolu TIMEOUT-MS >MS CMD:RUN-RC MATCH result
      ok OF drop true ENDOF
      err OF drop CMD:ERR$ type false ENDOF
   ;MATCH {: ran:bool :}
   s" a helper tool run in MID exits 0" T-LABEL
   ran TTRUE ;

: SETUP ( -- )
   s" habu-deep-cwd" FS:MKTEMP-DIR {: a:ptr u:n :}
   a ROOT u BYTE-COPY u ROOT-U !
   u ROOT-MAX > if
      SB-RESET s" deep-cwd: root " SB-APPEND ROOT$ SB-APPEND
      s"  is longer than " SB-APPEND ROOT-MAX FMT:SB-INT
      s"  bytes, so ROOT/deep/TAIL/<segment> passes PATH-CAP" SB-APPEND
      SB$ 1 die
   then
   ENGINE-CANDIDATE:PATH$ SOURCE-ROOT:CANONICAL TTRUE {: e:ptr eu:n :}
   e ENGINE eu BYTE-COPY eu ENGINE-U !
   ROOT$ s" entry.f" ENTRY FS:JOIN-PATH ENTRY-U !
   ENTRY$ S\" package DEEP-CWD-CHILD\npublic\n: RUN ( -- ) s\q deep cwd: ok\q type cr ;\n;package\nDEEP-CWD-CHILD:RUN\n" FS:WRITE-ALL
   TAIL!
   ROOT$ s" deep" PATH FS:JOIN-PATH {: pathu:n :}
   PATH pathu TAIL$ MID FS:JOIN-PATH MID-U !
   MID$ FS:MAKE-DIRS ;

: MAKE-TAIL ( CMD:command -- CMD:command )
   s" -p" CMD:ARG+ TAIL$ CMD:ARG+
   s" /bin/mkdir" IN-MID ;

: SAVE ( ptr u8 n ptr u8 n -- ) {: rel:ptr relu:n data:ptr size:n :}
   ROOT$ rel relu PATH FS:JOIN-PATH {: pathu:n :}
   PATH pathu data size FS:WRITE-ALL ;

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
   PCAP-RESULT:>N RC ! ERR-U ! OUT-U ! ;

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
   s" deep.out" s" deep.err" KEEP
   s" a working directory past PATH-CAP exits 74" T-LABEL
   RC @ 74 T=
   s" and prints nothing on stdout" T-LABEL
   OUT-U @ 0 T=
   s" and names itself on one line" T-LABEL
   ERR ERR-U @ S\" source root: the working directory does not resolve to a searchable directory within PATH-CAP bytes\n" T$= ;


\ PATH holds one answer at a time: each caller consumes it before the next.
: SENTINEL$ ( -- ptr u8 n )
   SIDE$ s" sentinel" PATH FS:JOIN-PATH {: u:n :}
   PATH u ;


: TOP$ ( -- ptr u8 n )
   MID$ TAIL-TOP$ PATH FS:JOIN-PATH {: u:n :}
   PATH u ;


: LINK ( n CMD:command -- CMD:command )
   swap {: linku:n :}
   s" -s" CMD:ARG+ SIDE$ CMD:ARG+ PATH linku CMD:ARG+
   s" /bin/ln" IN-MID ;


\ SIDE is beside the deep tree and holds the sentinel. The link to it sits in
\ the deepest tail directory, past PATH-CAP, so ln makes it relative to MID.
: LINK-SIDE ( -- )
   ROOT$ s" side" SIDE FS:JOIN-PATH SIDE-U !
   SIDE$ FS:MAKE-DIR
   SENTINEL$ s" kept" FS:WRITE-ALL
   TAIL$ s" side" PATH FS:JOIN-PATH [: LINK ;] CMD:WITH-COMMAND ;


: REMOVE-TAIL ( -- )
   LINK-SIDE
   [: TOP$ FS:REMOVE-TREE ;] catch {: code:n :}
   s" FS:REMOVE-TREE removes a tree whose paths pass PATH-CAP" T-LABEL
   code 0 T=
   code 0= if
      s" FS:REMOVE-TREE removed it" T-LABEL
      TOP$ FS:EXISTS? TFALSE
   then
   s" and unlinks a link to a directory without following it" T-LABEL
   SENTINEL$ FS:EXISTS? TTRUE ;

\ FS:REMOVE-TREE removes the tail, whose paths pass PATH-CAP, through directory
\ descriptors, and unlinks the tail's link to SIDE without following it into the
\ sentinel. A failed step is recorded, not fatal, and the cases run under
\ `finally`, so this rm child removes whatever tail a failure or a throw left
\ and finds nothing after a passing run: no run leaves a path past PATH-CAP that
\ nothing can remove by name.
: RM-TAIL ( CMD:command -- CMD:command )
   s" -r" CMD:ARG+ TAIL-TOP$ CMD:ARG+
   s" /bin/rm" IN-MID ;

: CLEAR-TAIL ( -- )
   TOP$ FS:EXISTS? 0= if exit then
   [: RM-TAIL ;] CMD:WITH-COMMAND ;

public

: RUN ( -- )
   T-RESET SETUP
   [: [: MAKE-TAIL ;] CMD:WITH-COMMAND FITS DEEP REMOVE-TAIL ;] [: CLEAR-TAIL ;] finally
   s" deep cwd tree: " type ROOT$ type cr
   T-REPORT ;

;package

DEEP-CWD-E2E:RUN
