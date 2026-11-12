\ chain-run.f - the checked generation-chain driver.
\ Run: bin/hb --load tools/chain-run-build.f -- host gen1 gen2 gen3 TMPDIR
\ Each child inherits only the caller's environment plus the private TMPDIR and
\ the engine override naming the engine that runs that build. The host builds
\ gen1, gen1 builds gen2, and gen2 builds gen3 only when the first two differ.
\ tools/chain-run-build.f states the exit statuses.

require src/lib/error.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/process-command.f
require src/lib/codesign.f

package CHAIN-RUN

public

\ The chain's failure status: a non-fixpoint, and every throw the CLI entry
\ (tools/chain-run-build.f) catches other than an expired deadline.
74 constant FAIL-RC

private

\ An engine build takes 5 minutes at most.
300000 constant BUILD-MS
DYNAMIC-BUFFER FILES u8

: ARG ( n -- ptr u8 n ) SCRIPT-ARGV$ ;

: NEED-ARGS ( -- )
   SCRIPT-ARGC 5 <> if
      s" chain-run: host gen1 gen2 gen3 TMPDIR required" 64 die
   then ;

\ A build that failed; its stderr names the file it was loading. A build past
\ BUILD-MS is killed, and CMD:RUN-RC throws E-PROC-TIMEOUT for it.
: BUILD-FAILED ( CMD:command n -- CMD:command )
   drop
   s" chain-run: hb-build stderr: " type CMD:ERR$ type cr
   E-BUILD-STATUS throw ;

: BUILD-IN ( ptr u8 n ptr u8 n ptr u8 n CMD:command -- CMD:command )
   >r {: host:ptr hostu out:ptr outu tmp:ptr tmpu :} r>
   s" --load" CMD:ARG+
   s" tools/hb-build.f" CMD:ARG+
   s" --" CMD:ARG+
   s" --engine" CMD:ARG+
   s" -o" CMD:ARG+
   out outu CMD:ARG+
   s" TMPDIR" tmp tmpu CMD:ENV+
   s" HABU_UNDER_TEST" host hostu CMD:ENV+
   host hostu BUILD-MS >MS CMD:RUN-RC MATCH result
      ok OF drop ENDOF
      err OF BUILD-FAILED ENDOF
   ;MATCH
   out outu FS:EXISTS? 0= if E-BUILD-PATH throw then ;

: BUILD ( ptr u8 n ptr u8 n ptr u8 n -- )
   [: BUILD-IN ;] CMD:WITH-COMMAND ;

\ Both files, A's bytes then B's. The byte reserved past them keeps index AU
\ inside the reserve when B is empty.
: READ-FILES ( ptr u8 n ptr u8 n -- ptr u8 n ptr u8 n ) {: a:ptr au:n b:ptr bu:n :}
   a au FS:FILE-SIZE {: as:n :}
   b bu FS:FILE-SIZE {: bs:n :}
   as bs + 1 + FILES-RESERVE
   a au 0 FILES as FS:READ-ALL as <> if E-FS-IO throw then
   b bu as FILES bs FS:READ-ALL bs <> if E-FS-IO throw then
   0 FILES as  as FILES bs ;

public

\ Two files hold the same image: CODESIGN:SAME-IMAGE? of their whole bytes.
: SAME-FILES? ( ptr u8 n ptr u8 n -- bool )
   [: READ-FILES CODESIGN:SAME-IMAGE? ;] [: FILES-RELEASE ;] finally ;

private

: REPORT ( n -- )
   s" chain-run: fixpoint at generation " type . cr ;

public

: MAIN ( -- )
   NEED-ARGS
   0 ARG 1 ARG 4 ARG BUILD
   1 ARG 2 ARG 4 ARG BUILD
   1 ARG 2 ARG SAME-FILES? if 2 REPORT exit then
   2 ARG 3 ARG 4 ARG BUILD
   2 ARG 3 ARG SAME-FILES? if 3 REPORT exit then
   s" chain-run: generation 3 is not the same image as generation 2" FAIL-RC die ;

;package
