\ Build the current source with a saved same-machine donor whose SNAP-RELOC
\ address-cell header precedes this tree's header. The ordinary native suite
\ uses a matching-layout host, so it cannot exercise this transition.
\ Run: TMPDIR=build <old-hb> --load test/native-layout-transition.f -- <old-hb>
require src/lib/test.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/os/script-argv.f

package NATIVE-LAYOUT-TRANSITION-TEST

$2000 constant CAP
300000 constant BUILD-MS
create OUT CAP allot
create ERR CAP allot
create BUNDLE-DIR PATH-CAP allot
variable BUNDLE-U
create BUNDLE-FILE PATH-CAP allot

: ARG ( ptr u8 n -- ) >LEN PROC-ARGV+ ;

: BUNDLE$ ( -- ptr u8 n ) BUNDLE-DIR BUNDLE-U @ ;

: BUNDLE-FILE? ( ptr u8 n -- bool )
   BUNDLE$ 2swap BUNDLE-FILE FS:JOIN-PATH BUNDLE-FILE swap FS:FILE? ;

: BUILD ( -- )
   SCRIPT-ARGC 1 <> if s" layout transition: one donor path required" 76 die then
   s" build" FS:MAKE-DIRS
   PROC-ARGV-ENV-RESET
   PROC-ENV-INHERIT-MISSING
   s" --load" ARG
   s" tools/native-build.f" ARG
   s" --" ARG
   s" build/hb-layout-transition" ARG
   0 SCRIPT-ARGV$ >LEN
   OUT CAP >LEN ERR CAP >LEN BUILD-MS >MS
   RUN-ARGV-ENV-CAPTURE-OUTCOME PROC-OUTCOME>RC RC>N
   {: outu:len erru:len rc:n :}
   rc 0<> if
      OUT outu LEN>N type ERR erru LEN>N type
   then
   rc 0 T= ;

: BUILD-BUNDLE ( -- )
   SOURCE-ROOT:CWD$ s" build/layout-bundle" BUNDLE-DIR FS:JOIN-PATH BUNDLE-U !
   BUNDLE$ FS:MAKE-DIRS
   PROC-ARGV-ENV-RESET
   PROC-ENV-INHERIT-MISSING
   s" --load" ARG
   s" test/compiler-images-build.f" ARG
   s" --" ARG
   BUNDLE$ ARG
   0 SCRIPT-ARGV$ >LEN
   OUT CAP >LEN ERR CAP >LEN BUILD-MS >MS
   RUN-ARGV-ENV-CAPTURE-OUTCOME PROC-OUTCOME>RC RC>N
   {: outu:len erru:len rc:n :}
   rc 0<> if
      OUT outu LEN>N type ERR erru LEN>N type
   then
   rc 0 T= ;

public
: RUN ( -- )
   T-RESET
   s" old layout donor builds current source" T-LABEL
   BUILD
   s" build/hb-layout-transition" FS:FILE? TTRUE
   s" build/hb-layout-transition.map" FS:FILE? TTRUE
   s" old layout donor builds compiler bundle" T-LABEL
   BUILD-BUNDLE
   s" hb-whitebox-source" BUNDLE-FILE? TTRUE
   s" hb-whitebox-source.map" BUNDLE-FILE? TTRUE
   T-REPORT
   s" native-layout-transition: ok" type cr ;

;package

NATIVE-LAYOUT-TRANSITION-TEST:RUN
