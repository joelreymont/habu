\ build-target.f - the target a build is for, as one cell its seams read.
\
\ Which target a build makes and which target the engine making it runs on are
\ two questions, because an ARM64 engine cross-builds x86-64. The engine's own
\ HB-TARGET-* predicates (src/os/*/target.f) answer the second and stay the
\ engine's; this cell answers the first. It starts as the engine's own target,
\ so a build nobody pointed elsewhere is a host build, and
\ tools/native-build-args.f points it elsewhere from `--target`.
\
\ ITS READERS are the seams that choose target sources on the building engine:
\ tools/native-build-core.f NB-TARGET-CORE-FILES, which gives the build window
\ its target.f and layout.f, and tools/build-fixpoint.f's stage-source
\ appenders. Inside the window nothing reads it: the window's own target.f
\ answers HB-TARGET-* there, so src/habu/native-runtime.f PROVIDE-TARGET and
\ LOAD-REPL-TERM and src/compiler/native/compiler.f LOAD-PASSES follow the cell
\ by construction rather than by a second read.
\
\ The names are the ones tools/hb-build-lib.f HBB-TARGET-ABI$ keys artifacts by.

require lib/string.f

package BUILD-TARGET
private

\ src/core/cell.f's CORE-LAYOUT-RC, the exit every unknown-target refusal uses.
$4C constant UNKNOWN-RC

0 constant LINUX-AARCH64
1 constant MACOS-AARCH64
2 constant LINUX-X86-64

variable CELL

: LINUX-AARCH64$ ( -- ptr u8 n ) s" linux-aarch64" ;
: MACOS-AARCH64$ ( -- ptr u8 n ) s" macos-aarch64" ;
: LINUX-X86-64$ ( -- ptr u8 n ) s" linux-x86-64" ;

\ The one read of the building engine's own predicates.
: HOST ( -- n )
   HB-TARGET-LINUX? if LINUX-AARCH64 exit then
   HB-TARGET-MACOS? if MACOS-AARCH64 exit then
   HB-TARGET-LINUX-X86-64? if LINUX-X86-64 exit then
   s" build-target: unknown host target" UNKNOWN-RC die ;

public

: LINUX? ( -- bool )         CELL @ LINUX-AARCH64 = ;
: MACOS? ( -- bool )         CELL @ MACOS-AARCH64 = ;
: LINUX-X86-64? ( -- bool )  CELL @ LINUX-X86-64 = ;

\ Point the build at the engine's own target.
: HOST! ( -- )   HOST CELL ! ;

\ Point the build at the named target. A name that is not one of the three
\ leaves the cell alone and answers false.
: SELECT? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u LINUX-AARCH64$ STR= if LINUX-AARCH64 CELL ! true exit then
   a u MACOS-AARCH64$ STR= if MACOS-AARCH64 CELL ! true exit then
   a u LINUX-X86-64$ STR= if LINUX-X86-64 CELL ! true exit then
   false ;

;package

BUILD-TARGET:HOST!
