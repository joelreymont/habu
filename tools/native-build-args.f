\ The native build's command line: an output path, an optional `whitebox`, an
\ optional `--target <target>`. tools/native-build.f checks it before the build
\ closure loads; NATIVE-BUILD:RUN checks it again for its other callers.
\ Exit status (NATIVE-BUILD:EXIT-RC): 0 promoted; 124 (PROC-TIMEOUT-RC) for an
\ expired deadline; BUILD-RC for a refused command line or a caught failure
\ (`native-build: uncaught throw code N`); 76 for a broken internal invariant.

require src/lib/string.f
require src/lib/errors.f
require src/os/script-argv.f
require src/compiler/target.f
require src/compiler/native/backend.f
require tools/build-target.f

package NATIVE-BUILD

$4A constant BUILD-RC

\ ---- the image class this build was asked for ---------------------------------
\ The wire form is the digit the image prints: the host engine that compiles this
\ file may lack ENGINE-INTERNAL:IMAGE-SEALED and :IMAGE-WHITEBOX.
0 constant CLASS-SEALED
1 constant CLASS-WHITEBOX
variable CLASS-WANTED
variable TARGET-EXPLICIT

: WHITEBOX-ARG$ ( -- ptr u8 n ) s" whitebox" ;

\ `whitebox` after the output path asks for the unsealed host. Answers the index
\ of the next argument, which the target parser accepts or refuses.
: CLASS-ARG! ( -- n )
   CLASS-SEALED CLASS-WANTED !
   SCRIPT-ARGC 2 < if 1 exit then
   1 SCRIPT-ARGV$ WHITEBOX-ARG$ STR= 0= if 1 exit then
   CLASS-WHITEBOX CLASS-WANTED ! 2 ;

\ ---- the target this build is for ----------------------------------------------
\ `--target <target>` sets tools/build-target.f's cell; without it the build is
\ for this engine. A target builds only when its architecture's backend is
\ registered here (NBACK:REGISTERED?, src/compiler/native/backend.f); this entry
\ loads none, so an ARM64 product refuses `--target linux-x86-64` unless the
\ x86-64 passes module was loaded ahead of tools/native-build.f.
: TARGET-FLAG$ ( -- ptr u8 n ) s" --target" ;

: ARGS-REFUSE ( -- )
   s" native-build: after the output path come only `whitebox`, then `--target <target>`" BUILD-RC die ;

: TARGET-ARCH ( -- CTARGET:arch )
   BUILD-TARGET:CURRENT RTARGET:ARCH@ ;

: CORE-CK ( -- )
   BUILD-TARGET:CURRENT RTARGET:CORE MATCH RTARGET:core-result
      supported OF drop ENDOF
      unsupported OF
         s" native-build: the --target profile has no native CORE ABI" BUILD-RC die
      ENDOF
   ;MATCH ;

: BACKEND-CK ( -- )
   TARGET-ARCH NBACK:REGISTERED? if exit then
   s" native-build: the --target machine has no backend loaded; load its backend module before tools/native-build.f" BUILD-RC die ;

\ The argument after `--target` names the target; anything after that is refused.
: TARGET-ARG! ( n -- ) {: at:n :}
   BUILD-TARGET:HOST!
   0 TARGET-EXPLICIT !
   SCRIPT-ARGC at = if exit then
   SCRIPT-ARGC at 2 + <> if ARGS-REFUSE then
   at SCRIPT-ARGV$ TARGET-FLAG$ STR= 0= if ARGS-REFUSE then
   at 1+ SCRIPT-ARGV$ BUILD-TARGET:SELECT? 0= if
      s" native-build: unknown --target profile" BUILD-RC die
   then
   -1 TARGET-EXPLICIT !
   CORE-CK BACKEND-CK ;

public

: WHITEBOX? ( -- bool ) CLASS-WANTED @ CLASS-WHITEBOX = ;
: EXPLICIT-TARGET? ( -- bool ) TARGET-EXPLICIT @ 0<> ;

: BUILD-ARGS! ( -- )
   BUILD-TARGET:IDLE-CK
   SCRIPT-ARGC 1 < if
      s" native-build: one explicit output path is required, then an optional `whitebox`, then an optional `--target <target>`" BUILD-RC die
   then
   CLASS-ARG! TARGET-ARG! ;

;package
