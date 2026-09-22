\ Rebuild the checker through the current native owner handoff, then exercise
\ its actual schema rewind and pointer-pool boundaries before sealing.
require lib/test.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process-argv.f
require lib/process-env.f
require test/whitebox-engine.f

package CHECKER-PREFIX-TEST
$4000 constant CAP
create OUT CAP allot
create ERR CAP allot
create ROOT-BUF FS-PATH-CAP allot
create ENGINE-BUF FS-PATH-CAP allot
variable ROOT-U
variable ENGINE-U

: ROOT$ ( -- ptr u8 n ) ROOT-BUF ROOT-U @ ;
: ENGINE$ ( -- ptr u8 n ) ENGINE-BUF ENGINE-U @ ;

\ The child reaches inside the engine it runs on - it reopens the native build
\ window and hands the checker its own prefix - and the sealed product refuses
\ every one of those tokens (test/whitebox-engine.f). So this file names the
\ engine its child needs instead of inheriting one: it puts a private copy of
\ the keyed unsealed engine under its own temp root, which copies the gate's
\ artifact when that already exists and builds it otherwise. A standalone run
\ then measures what the gate row measures, on the same bytes.
: PREPARE ( -- )
   CLEANUP-RESET
   s" native-checker-prefix" TMPDIR-MKDIR {: path:ptr pathu:n :}
   path ROOT-BUF pathu BYTE-COPY pathu ROOT-U !
   ROOT$ CLEANUP-TREE+
   ROOT$ s" hb-whitebox" ENGINE-BUF JOIN-PATH ENGINE-U !
   ENGINE$ WHITEBOX-ENGINE:PROVIDE ;

\ The child forks tools of its own, and lib/engine-candidate.f is what they ask
\ which engine to run: name the whitebox copy there too, the way the gate's
\ SUITE-WB-RUN does, so nothing below this child lands on the sealed product.
: ENGINE-ENV! ( -- )
   PROC-ENV-RESET
   s" HABU_UNDER_TEST" >LEN ENGINE$ >LEN PROC-ENV+
   s" HABU_FIXPOINT_ENGINE" >LEN ENGINE$ >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING ;

: CHECK ( -- )
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   s" test/compiler/aot-mode.f" >LEN PROC-ARGV+
   s" test/native-window-owner-child.f" >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   s" test/compiler/native-checker-storage.f" >LEN PROC-ARGV+
   s" test/compiler/native-prefix-rollback.f" >LEN PROC-ARGV+
   ENGINE-ENV!
   ENGINE$ >LEN
   OUT CAP >LEN ERR CAP >LEN 180000 >MS RUN-ARGV-ENV-CAPTURE
   MATCH result
      ok OF PCAP-CAPTURED:UNMAKE {: ou:len eu:len :}
         OUT ou LEN>N s" window: 0" CONTAINS? 0= eu LEN>N 0<> or if
            OUT ou LEN>N type ERR eu LEN>N type
         then
         OUT ou LEN>N s" window: 0" CONTAINS? TTRUE
         eu LEN>N 0 T=
      ENDOF
      err OF PCAP-FAILED:UNMAKE {: ou:len eu:len rc:rc :}
         OUT ou LEN>N type ERR eu LEN>N type
         rc RC>N 0 T=
      ENDOF
   ;MATCH ;

: RUN ( -- )
   T-RESET
   [: PREPARE CHECK ;] [: CLEANUP-RUN ;] finally
   T-REPORT ;
public
RUN
;package
