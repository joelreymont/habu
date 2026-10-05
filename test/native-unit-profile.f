\ The NBR unit belongs to ARM64. A real Intel build entry and the direct
\ capture, object and cache entries refuse it before they consume any source
\ or publish an output. The private directory printed below retains the
\ attempted output path for inspection.

require lib/test.f
require lib/string.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/engine-candidate.f
require tools/native-unit-build-core.f
require test/native-unit-image.f

package NUNIT-PROFILE-TEST

private

$4000 constant CAP
create ROOT FS-PATH-CAP allot
variable ROOT-U
create OUTPUT FS-PATH-CAP allot
variable OUTPUT-U
create UNIT FS-PATH-CAP allot
variable UNIT-U
create OUT CAP allot
create ERR CAP allot
variable OUT-U
variable ERR-U
variable RC
variable CALLED

: ROOT$ ( -- ptr u8 n ) ROOT ROOT-U @ ;
: OUTPUT$ ( -- ptr u8 n ) OUTPUT OUTPUT-U @ ;
: UNIT$ ( -- ptr u8 n ) UNIT UNIT-U @ ;
: KEY$ ( -- ptr u8 n )
   s" 0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef" ;

: SETUP ( -- )
   s" native-unit-profile" HB-TMP-MKDIR {: a:ptr u:n :}
   a ROOT u BYTE-COPY u ROOT-U !
   ROOT$ s" hb-rejected" OUTPUT JOIN-PATH OUTPUT-U !
   ROOT$ s" nbr.unit" UNIT JOIN-PATH UNIT-U ! ;

: MARK ( -- ) 1 CALLED ! ;
: QUERY ( n n -- n ) drop ;
: RUN-UNIT ( -- ) ['] QUERY false NATIVE-BUILD:RUN-UNIT ;
: CAPTURE ( -- ) ['] MARK NUNIT-CAPTURE:WITH ;
: IMPORT ( -- ) UNIT$ KEY$ NUNIT-OBJECT:IMPORT 2drop ;
: EXPORT ( -- ) UNIT$ KEY$ s" checker" NUNIT-OBJECT:EXPORT-UNIT ;

: DIRECT-INTEL ( -- )
   s" an Intel host refuses direct unit operations before capture, file read or cache resolution" T-LABEL
   0 CALLED !
   [: RUN-UNIT ;] E-NUNIT-PROFILE TTHROWSQ
   [: CAPTURE ;] E-NUNIT-PROFILE TTHROWSQ
   CALLED @ 0 T=
   [: IMPORT ;] E-NUNIT-PROFILE TTHROWSQ
   [: EXPORT ;] E-NUNIT-PROFILE TTHROWSQ
   [: NATIVE-UNIT-IMAGE:ENSURE ;] E-NUNIT-PROFILE TTHROWSQ
   OUTPUT$ EXISTS? TFALSE
   UNIT$ EXISTS? TFALSE ;

: SELECT-FOREIGN ( -- )
   s" a selected body on the other architecture refuses before capture or build arguments" T-LABEL
   HB-TARGET-LINUX-X86-64? if s" linux-aarch64"
   else s" linux-x86-64" then
   BUILD-TARGET:SELECT? TTRUE
   [: RUN-UNIT ;] E-NUNIT-PROFILE TTHROWSQ
   0 CALLED !
   [: CAPTURE ;] E-NUNIT-PROFILE TTHROWSQ
   CALLED @ 0 T=
   BUILD-TARGET:HOST! ;

: ARG ( ptr u8 n -- ) >LEN PROC-ARGV+ ;

: BUILD-ENTRY ( -- )
   PROC-ARGV-ENV-RESET
   PROC-ENV-INHERIT-MISSING
   s" --load" ARG
   s" tools/native-unit-build.f" ARG
   s" --" ARG
   s" --export-unit" ARG
   s" NBR" ARG
   UNIT$ ARG
   OUTPUT$ ARG
   ENGINE-CANDIDATE:PATH$ >LEN
   OUT CAP >LEN ERR CAP >LEN 60000 >MS
   RUN-ARGV-ENV-CAPTURE-OUTCOME PROC-OUTCOME>RC RC>N
   {: outu:len erru:len code:n :}
   outu LEN>N OUT-U ! erru LEN>N ERR-U ! code RC ! ;

: ENTRY-INTEL ( -- )
   s" a real Intel unit build refuses the profile without publishing either artifact" T-LABEL
   BUILD-ENTRY
   RC @ 0<> TTRUE
   ERR ERR-U @ s" uncaught throw code -8597" CONTAINS? TTRUE
   OUTPUT$ EXISTS? TFALSE
   UNIT$ EXISTS? TFALSE ;

public

: RUN ( -- )
   T-RESET
   SETUP
   HB-TARGET-LINUX-X86-64? if
      DIRECT-INTEL
      ENTRY-INTEL
   then
   SELECT-FOREIGN
   T-REPORT
   s" native unit profile tree: " type ROOT$ type cr ;

;package

NUNIT-PROFILE-TEST:RUN
