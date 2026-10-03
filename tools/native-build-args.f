\ The native build's command line: one output path, then an optional
\ `whitebox`, then an optional `--target <target>`. tools/native-build.f checks
\ it before the build closure loads, so a refused command line costs an engine
\ boot and not a native compiler; the driver's own entries (NATIVE-BUILD:RUN,
\ :RUN-IMAGE) check it again for their other callers.
\
\ Exit status (NATIVE-BUILD:EXIT-RC): 0 for a promoted build; PROC-TIMEOUT-RC
\ (124, lib/process.f) when a deadline in the build expired; BUILD-RC for a
\ refused command line and for every other failure the driver catches, which
\ it names first as `native-build: uncaught throw code N`; 76 for a broken
\ internal invariant the driver dies on by name.

require lib/string.f
require lib/errors.f
require src/os/script-argv.f
require src/compiler/target.f
require tools/build-target.f

package NATIVE-BUILD

$4A constant BUILD-RC

\ ---- the image class this build was asked for ---------------------------------
\ src/core/internal-mark.f's seal stands down when HABU_WHITEBOX_IMAGE=1 is set
\ for the target load, which is how test/whitebox-engine.f gets the unsealed
\ host the gate's whitebox suites run on. An environment variable reaches this
\ build whether or not anyone meant it to, so it cannot be the authority for
\ what gets promoted: the REQUEST is an argument, spelled after the output path,
\ and the ANSWER comes back out of the finished image in the driver's SMOKE
\ (tools/native-build-core.f). A build that was not asked for a whitebox host
\ and made one anyway stops before it renames the binary into place.
\
\ The two values are ENGINE-INTERNAL:IMAGE-SEALED and :IMAGE-WHITEBOX, which
\ this file cannot name: it is compiled by the host engine, and an older host
\ has no such word. So the wire form is the digit the new image prints and the
\ meaning is named on both sides.
\
\ The cell starts at CLASS-SEALED, which is the safe default and the one a
\ caller that drives RUN-PATH-RC directly rather than RUN gets
\ (tools/build-profile.f): it asks for nothing and is held to the product's
\ check.
0 constant CLASS-SEALED
1 constant CLASS-WHITEBOX
variable CLASS-WANTED

: WHITEBOX-ARG$ ( -- ptr u8 n ) s" whitebox" ;

\ `whitebox` after the output path asks for the unsealed host
\ test/whitebox-engine.f builds. Answers the index of the next argument, so
\ whatever stands there instead is the target argument's to accept or refuse,
\ and a typo cannot quietly produce a product.
: CLASS-ARG! ( -- n )
   CLASS-SEALED CLASS-WANTED !
   SCRIPT-ARGC 2 < if 1 exit then
   1 SCRIPT-ARGV$ WHITEBOX-ARG$ STR= 0= if 1 exit then
   CLASS-WHITEBOX CLASS-WANTED ! 2 ;

\ ---- the target this build is for ----------------------------------------------
\ `--target <target>` sets tools/build-target.f's cell, which chooses the OS
\ sources the build window loads; without it the build is for the engine doing
\ the building. The window's code runs on this engine while it loads, so an
\ image for another machine is a second emission this engine makes as it
\ compiles (docs/x86-64.md "Build and bootstrap"), and a target is only
\ buildable when this engine has a backend for its machine: the target
\ architecture's row must be registered here (src/compiler/target.f
\ REGISTERED?, the one answer to whether a backend is loaded). This entry does
\ not load one. An ARM64 product carries only its own,
\ so `--target linux-x86-64` is refused until the caller has loaded the x86-64
\ backend module ahead of tools/native-build.f.
: TARGET-FLAG$ ( -- ptr u8 n ) s" --target" ;

: ARGS-REFUSE ( -- )
   s" native-build: after the output path come only `whitebox`, then `--target <target>`" BUILD-RC die ;

: TARGET-ARCH ( -- CTARGET:arch )
   BUILD-TARGET:LINUX? if CTARGET-ARCH:AARCH64 exit then
   BUILD-TARGET:MACOS? if CTARGET-ARCH:AARCH64 exit then
   BUILD-TARGET:LINUX-X86-64? if CTARGET-ARCH:X86-64 exit then
   E-CTGT-ABI throw ;

: BACKEND-CK ( -- )
   TARGET-ARCH CTARGET:REGISTERED? if exit then
   s" native-build: the --target machine has no backend loaded; load its backend module before tools/native-build.f" BUILD-RC die ;

\ The argument after `--target` names the target; anything after that is refused.
: TARGET-ARG! ( n -- ) {: at:n :}
   BUILD-TARGET:HOST!
   SCRIPT-ARGC at = if exit then
   SCRIPT-ARGC at 2 + <> if ARGS-REFUSE then
   at SCRIPT-ARGV$ TARGET-FLAG$ STR= 0= if ARGS-REFUSE then
   at 1+ SCRIPT-ARGV$ BUILD-TARGET:SELECT? 0= if
      s" native-build: --target is linux-aarch64, macos-aarch64 or linux-x86-64" BUILD-RC die
   then
   BACKEND-CK ;

public

: BUILD-ARGS! ( -- )
   SCRIPT-ARGC 1 < if
      s" native-build: one explicit output path is required, then an optional `whitebox`, then an optional `--target <target>`" BUILD-RC die
   then
   CLASS-ARG! TARGET-ARG! ;

;package
