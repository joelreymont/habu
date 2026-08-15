\ aot-chain-capture-suite.f - the compiler-chain capture tool, run for real.
\
\ WHAT THIS LOCKS. tools/aot-chain-capture.f captures the native compiler chain
\ inside a booted engine so the metabuild can bake it. Two things make that
\ capture correct and neither of them is visible in its output:
\
\   THE LOAD ORDER. The chain must load INSIDE the window and the capture's own
\   tooling after it closes. Reversed, the tool's closure pulls
\   src/arch/arm64/asm.f in first, the chain's `require` of asm.f becomes a no-op,
\   and every chain word that calls an asm.f word calls a word the target engine
\   has not got - 98 of 18602 call sites, measured. That capture RUNS; only the
\   band audit says the seed it produced could never boot.
\
\   BEING FIRST. The band audit trusts every record BELOW the tool's mark to be a
\   word the target has, which is true of the engine's own surface and of nothing
\   else. Run the tool behind one other file and the marks swallow that file: the
\   capture succeeds, the window is smaller, and the seed calls names by hand-off
\   that no target defines.
\
\ THE ORDER CASE IS A MUTANT DERIVED FROM THE REAL FILE, not a second copy of it:
\ the suite reads tools/aot-chain-capture.f, splices `require src/arch/arm64/asm.f`
\ in front of the line that opens the window, and runs THAT. So the fixture cannot
\ drift from the tool, and the mutation is exactly the refuted ordering. The
\ splice is fail-closed on its anchor: if the anchor line is not present exactly
\ once the suite stops rather than running an unmutated file and passing.
\
\ AND THE SUITE PROVES ITS OWN DETECTOR. test/aot-chain-decoy.f prints the
\ refusal sentence and exits 0. A checker that read text and not exit codes would
\ call that a refusal; the self-test asserts it does not.
\
\ THE IDENTITY CASE READS ONE FACT TWICE. The capture reports the content key of
\ the engine it is running in (lib/engine-id.f, resolving its own executable) and
\ this suite hashes bin/hb from the outside with SHA256-FILE; the two must agree.
\ That is the mechanism the metabuild uses to refuse an artifact some other engine
\ produced, so it is worth a case of its own rather than trust. The closure the
\ capture names comes from the engine's require registry, so the case asserts it
\ starts at the file the window actually required.
\
\ Cost: three child engine runs (~3s), no metabuild. Registered as
\ `SUITE aot-chain-capture` in test/gate-stdlib-cases.f. Run standalone:
\   bin/hb --load test/aot-chain-capture-suite.f

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/test.f
require lib/test/src-shape.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f

package AOT-CHAIN-CAPTURE-TEST

$4000 constant CAP
120000 constant CASE-TIMEOUT-MS
74 constant REFUSE-RC        \ what a refused child exits with
74 constant STOP-RC          \ ... and what this suite exits with when its own fixture has gone vacuous

create OUT CAP allot     variable OUT-U
create ERR CAP allot     variable ERR-U
variable RC

create ROOT-BUF FS-PATH-CAP allot    variable ROOT-U
create MUT-BUF FS-PATH-CAP allot     variable MUT-U
create DIG 32 allot
create DHEX 64 allot

: ROOT$ ( -- ptr u8 n ) ROOT-BUF ROOT-U @ ;
: MUT$ ( -- ptr u8 n )  MUT-BUF MUT-U @ ;
: HB$ ( -- ptr u8 n )   s" bin/hb" ;
: OUT$ ( -- ptr u8 n )  OUT OUT-U @ ;
: ERR$ ( -- ptr u8 n )  ERR ERR-U @ ;
: TOOL$ ( -- ptr u8 n ) s" tools/aot-chain-capture.f" ;

\ The line that opens the window. Splicing in front of it is what puts the
\ capture's own closure ahead of the chain, which is the refuted order.
: ANCHOR$ ( -- ptr u8 n ) s\" \nAOT-CHAIN:OPEN\n" ;
: INJECT$ ( -- ptr u8 n ) s\" require src/arch/arm64/asm.f\n" ;

: SETUP ( -- )
   s" habu-aot-chain" TMPDIR-MKDIR {: a:ptr u:n :}
   a ROOT-BUF u BYTE-COPY  u ROOT-U !
   ROOT$ CLEANUP-TREE+
   ROOT-BUF MUT-BUF ROOT-U @ BYTE-COPY
   s" /hoisted.f" {: sa:ptr su:n :}
   sa MUT-BUF ROOT-U @ + su BYTE-COPY
   ROOT-U @ su + MUT-U ! ;

\ ---- the derived mutant ------------------------------------------------------

: ANCHOR-OFF ( -- n )
   SHAPE:TEXT ANCHOR$ FIND-SUB MATCH option
     none OF -1 ENDOF
     some OF IDX>N ENDOF
   ;MATCH ;

\ Fail-closed on the anchor: an anchor that is missing, or that a later comment
\ has spelled a second time, means the splice would land somewhere else or
\ nowhere. Either way the mutant would stop being the refuted order and the order
\ case would pass by doing nothing.
: ?ANCHOR ( -- )
   ANCHOR$ SHAPE:COUNT 1 = if exit then
   s" aot-chain-capture-suite: window-open anchors in " type TOOL$ type s" =" type
   ANCHOR$ SHAPE:COUNT .
   s" aot-chain-capture-suite: the order case cannot be built" STOP-RC die ;

\ prefix (up to and including the newline that ends the previous line), then the
\ injected require, then the rest starting at the anchor's own line.
: WRITE-MUTANT ( -- )
   ANCHOR-OFF 1 + {: cut:n :}
   SHAPE:TEXT drop {: src:ptr :}
   MUT$ src cut WRITE-ALL
   MUT$ INJECT$ APPEND-FILE
   MUT$ src cut +  SHAPE:TEXT nip cut -  APPEND-FILE ;

: MUTANT-BUILD ( -- )
   TOOL$ SHAPE:LOAD
   ?ANCHOR
   WRITE-MUTANT
   s" the mutant is the tool plus exactly the injected require" T-LABEL
   MUT$ FILE-SIZE  SHAPE:TEXT nip INJECT$ nip + T= ;

\ ---- one child engine run ----------------------------------------------------

: RUN-CASE ( ptr u8 n -- ) {: f:ptr fu:n :}
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   f fu >LEN PROC-ARGV+
   PROC-ENV-RESET
   s" HB_TMP" >LEN ROOT$ >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   HB$ >LEN  OUT CAP >LEN  ERR CAP >LEN  CASE-TIMEOUT-MS >MS
   RUN-ARGV-ENV-CAPTURE
   MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE {: o:len e:len :}
            o LEN>N OUT-U !  e LEN>N ERR-U !  0 RC ! ENDOF
     err OF PCAP-FAILED:UNMAKE {: o:len e:len c:rc :}
            o LEN>N OUT-U !  e LEN>N ERR-U !  c RC>N RC ! ENDOF
   ;MATCH ;

: SAID? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   OUT$ a u CONTAINS?  ERR$ a u CONTAINS? or ;

: DIAG. ( -- )
   s" aot-chain-capture: child stdout:" type cr OUT$ type cr
   s" aot-chain-capture: child stderr:" type cr ERR$ type cr ;

\ A refusal is an exit code AND a sentence. Either alone is a text search.
: REFUSED? ( ptr u8 n -- bool ) {: r:ptr ru:n :}
   RC @ REFUSE-RC = r ru SAID? and ;

: REFUSED ( ptr u8 n -- ) {: r:ptr ru:n :}
   r ru REFUSED? 0= if DIAG. then
   r ru REFUSED? TTRUE ;

\ ---- reading the census the tool prints --------------------------------------

: DIGIT-RUN ( ptr u8 n -- n ) {: a:ptr u:n :}
   0 begin dup u < while
      dup a + c@ STR-DIGIT? 0= if exit then
      1+
   repeat ;

\ The value printed after `label` on the child's stdout, or -1 when the label is
\ absent or carries no digits. Reading the number, not the label, is what makes a
\ tool that printed the labels and stopped fail here.
: FIELD ( ptr u8 n -- n ) {: la:ptr lu:n :}
   OUT$ la lu FIND-SUB MATCH option
     none OF -1 ENDOF
     some OF IDX>N lu + {: off:n :}
             OUT off +  OUT-U @ off -  {: va:ptr vu:n :}
             va vu DIGIT-RUN {: d:n :}
             d 0= if -1 exit then
             va d STR>NUMBER? MATCH option
               none OF -1 ENDOF
               some OF ENDOF
             ;MATCH
     ENDOF
   ;MATCH ;

\ The `want` bytes printed after `label`, or a zero-length string when the label is
\ absent or the line is short. Used for the two digests, which are fixed-width hex.
: TEXT-FIELD ( ptr u8 n n -- ptr u8 n ) {: la:ptr lu:n want:n :}
   OUT$ la lu FIND-SUB MATCH option
     none OF OUT 0 ENDOF
     some OF IDX>N lu + {: off:n :}
             OUT-U @ off - want < if OUT 0 exit then
             OUT off + want
     ENDOF
   ;MATCH ;

: FLOOR ( n n ptr u8 n -- ) {: got:n want:n la:ptr lu:n :}
   la lu T-LABEL
   got want >= 0= if
      s" aot-chain-capture-suite: " type la lu type s"  got " type got .
      s" aot-chain-capture-suite: wanted at least " type want .
   then
   got want >= TTRUE ;

\ ---- the cases ---------------------------------------------------------------

\ The chain capture, run the way the metabuild will run it. The two equalities are
\ the structural half: the capture's own blob and DATA sizes must be the window it
\ measured, so a census printed from stale buffers cannot agree with itself. The
\ floors are the non-vacuity half: this window is the compiler chain (6764 records,
\ 18939 sites, 1.16 MiB of code when this was written), and a capture that fell to
\ a fraction of it captured something else.
: PROBE-REAL ( -- )
   TOOL$ RUN-CASE
   s" the chain capture runs to completion in a booted engine" T-LABEL
   RC @ 0 <> if DIAG. then
   RC @ 0 T=
   s" the captured blob is the window's own code span" T-LABEL
   s" blob=" FIELD  s" codespan=" FIELD T=
   s" the captured DATA is the window's own DATA span" T-LABEL
   s" datasz=" FIELD  s" dataspan=" FIELD T=
   s" blob=" FIELD    1000000 s" the window holds the chain's code" FLOOR
   s" recs=" FIELD       6000 s" ... its dictionary records" FLOOR
   s" sites=" FIELD     15000 s" ... its call sites" FLOOR
   s" dsites=" FIELD     3000 s" ... its DATA-literal sites" FLOOR
   s" xtoff=" FIELD         1 s" ... and its declared address cell" FLOOR
   s" bandrecs=" FIELD      1 s" the prelude band is not empty" FLOOR
   s" bandbytes=" FIELD     1 s" ... on the DATA axis either" FLOOR ;

\ The capture's identity, read from the same run PROBE-REAL just made. The producer
\ key is the interesting one: the tool reports the content key of the binary it is
\ RUNNING (lib/engine-id.f, which resolves its own executable), and this suite
\ hashes bin/hb from the outside. Two independent readings of one fact, compared -
\ which is the whole mechanism the metabuild will use to refuse an artifact that
\ some other engine produced.
: PROBE-IDENT ( -- )
   s" the capture names the closure it loaded" T-LABEL
   s" closure=" FIELD  40 s" closure files" FLOOR
   s" ... starting at the chain root the window required" T-LABEL
   s" first=src/compiler/native/migrate.f" SAID? TTRUE
   s" ... and carries a full-width digest over their bytes" T-LABEL
   s" chaindigest=" 64 TEXT-FIELD nip 64 T=
   s" the producer key is the engine that ran the capture" T-LABEL
   s" bin/hb" DIG SHA256-FILE 0 T=
   DIG DHEX SHA256>HEX
   s" producer=" 64 TEXT-FIELD  DHEX 64 T$= ;

\ The refuted order, spliced into the real file. The diagnostic must name a window
\ word and a callee; `<no record>` on either side would mean the audit fired
\ without being able to say on what, and a reader could not act on it.
: PROBE-ORDER ( -- )
   MUT$ RUN-CASE
   s" the capture's own closure ahead of the chain is refused" T-LABEL
   s" aot-capture: window call into the prelude band" REFUSED
   s" ... naming the window word that makes the call" T-LABEL
   s" aot-capture: window word " SAID? TTRUE
   s" ... and the callee no target defines" T-LABEL
   s" which the booting engine has and no target does" SAID? TTRUE
   s" ... both by name" T-LABEL
   s" <no record>" SAID? 0= TTRUE ;

\ Anything loaded before the tool is refused before a single record is captured.
: PROBE-NOT-FIRST ( -- )
   s" test/aot-chain-preloaded.f" RUN-CASE
   s" a capture that is not the first file loaded is refused" T-LABEL
   s" aot-chain-capture: the capture must be the first file this process loads"
   REFUSED
   s" ... and says how many files got in first" T-LABEL
   s" aot-chain-capture: files loaded before the capture=2" SAID? TTRUE
   s" ... before any window is captured" T-LABEL
   s" recs=" SAID? 0= TTRUE ;

\ Fail-closed proof of this suite's own refusal detector: a child that prints the
\ refusal sentence and exits 0 is not a refusal.
: SELFTEST ( -- )
   s" test/aot-chain-decoy.f" RUN-CASE
   s" the decoy prints the refusal sentence" T-LABEL
   s" aot-capture: window call into the prelude band" SAID? TTRUE
   s" ... and exits 0" T-LABEL
   RC @ 0 T=
   s" ... so the detector must not call it a refusal" T-LABEL
   s" aot-capture: window call into the prelude band" REFUSED? 0= TTRUE ;

: BODY ( -- )
   SETUP
   MUTANT-BUILD
   SELFTEST
   PROBE-REAL
   PROBE-IDENT
   PROBE-ORDER
   PROBE-NOT-FIRST ;

public

: RUN ( -- )
   T-RESET
   CLEANUP-RESET
   [: BODY ;] catch {: code:n :}
   CLEANUP-RUN
   code 0 <> if code throw then
   T-REPORT
   s" aot-chain-capture: ok" type cr ;

;package

AOT-CHAIN-CAPTURE-TEST:RUN
