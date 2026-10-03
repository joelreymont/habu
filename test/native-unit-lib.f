\ native-unit-lib.f - the fixture the NBR package unit rows share.
\ test/native-unit-image.f exports the unit of package NBR
\ (src/compiler/native/branch.f) once, from the tree the gate runs in; each row
\ imports it into a native build of its own and owns one claim:
\   test/native-unit-e2e.f   the import, in the exporting tree, writes the
\                            engine that tree's cold build writes, byte for byte
\   test/native-unit-stale.f an import into a tree with another root refuses
\                            the unit and publishes nothing
\ test/gate-stdlib-cases.f registers them as rows of their own, each running
\ one engine build: an export and an import in one row would be two, past the
\ pool's row deadline under load. Each row prints its private directory, which
\ keeps what its import wrote.

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process-cwd.f
require lib/engine-candidate.f
require test/native-unit-image.f

package NATIVE-UNIT-TEST

$8000 constant CAP
600000 constant BUILD-TIMEOUT-MS

create ROOT FS-PATH-CAP allot          variable ROOT-U
create AT-BUF FS-PATH-CAP allot
create OUT CAP allot                   variable OUT-U
create ERR CAP allot                   variable ERR-U
variable RC

: ROOT$ ( -- ptr u8 n ) ROOT ROOT-U @ ;
: OUT$ ( -- ptr u8 n ) OUT OUT-U @ ;

\ A path under the row's private directory; valid until the next AT.
: AT ( ptr u8 n -- ptr u8 n ) {: rel:ptr relu:n :}
   ROOT$ rel relu AT-BUF JOIN-PATH AT-BUF swap ;

: SETUP ( ptr u8 n -- ) {: tag:ptr tagu:n :}
   tag tagu HB-TMP-MKDIR {: a:ptr u:n :}
   a ROOT u BYTE-COPY u ROOT-U ! ;

: CAPTURE-RESULT ( result<pcap:captured,pcap:failed> -- )
   MATCH result
      ok OF PCAP-CAPTURED:UNMAKE {: outu:len erru:len :}
         outu LEN>N OUT-U ! erru LEN>N ERR-U ! 0 RC ! ENDOF
      err OF PCAP-FAILED:UNMAKE {: outu:len erru:len code:rc :}
         outu LEN>N OUT-U ! erru LEN>N ERR-U ! code RC>N RC ! ENDOF
   ;MATCH ;

: SUCCESS ( -- )
   RC @ 0<> if OUT$ type ERR ERR-U @ type then
   RC @ 0 T= ;

\ Import the keyed unit into a native build run in `tree`, writing the unsealed
\ engine to `out` under the row's directory. The unit is settled before argv is
\ staged, because settling it may run the export.
: IMPORT ( ptr u8 n ptr u8 n -- ) {: out:ptr outu:n tree:ptr treeu:n :}
   NATIVE-UNIT-IMAGE:PATH$ {: unit:ptr unitu:n :}
   PROC-CWD:ARGV-ENV-CWD-RESET
   s" HABU_WHITEBOX_IMAGE" >LEN s" 1" >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   s" --load" >LEN PROC-ARGV+
   s" tools/native-unit-build.f" >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   s" --import-unit" >LEN PROC-ARGV+
   unit unitu >LEN PROC-ARGV+
   out outu AT >LEN PROC-ARGV+
   s" whitebox" >LEN PROC-ARGV+
   ENGINE-CANDIDATE:PATH$ >LEN tree treeu >LEN
   OUT CAP >LEN ERR CAP >LEN BUILD-TIMEOUT-MS >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-CAPTURE CAPTURE-RESULT ;

;package
