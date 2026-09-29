\ The native build's command line: one output path, then an optional `whitebox`.
\ tools/native-build.f checks it before the build closure loads, so a refused
\ command line costs an engine boot and not a native compiler; the driver's own
\ entries (NATIVE-BUILD:RUN, :RUN-IMAGE) check it again for their other callers.

require lib/string.f
require src/os/script-argv.f

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

public

\ `whitebox` after the output path asks for the unsealed host
\ test/whitebox-engine.f builds; nothing else is a legal second argument, so a
\ typo cannot quietly produce a product.
: CLASS-ARG! ( -- )
   CLASS-SEALED CLASS-WANTED !
   SCRIPT-ARGC 2 < if exit then
   1 SCRIPT-ARGV$ WHITEBOX-ARG$ STR= 0= if
      s" native-build: the only second argument is `whitebox`" BUILD-RC die
   then
   CLASS-WHITEBOX CLASS-WANTED ! ;

: BUILD-ARGS! ( -- )
   SCRIPT-ARGC 1 < SCRIPT-ARGC 2 > or if
      s" native-build: one explicit output path is required, then an optional `whitebox`" BUILD-RC die
   then
   CLASS-ARG! ;

;package
