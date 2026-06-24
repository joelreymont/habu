\ srclist.f - emit canonical compiler source order.

64 constant SL-USAGE-RC

variable SL-DRIVER-A
variable SL-DRIVER-U

: SL-TRUE ( -- bool )
   0 0= ;

: SL-FALSE ( -- bool )
   SL-TRUE 0= ;

: SL-STR= ( ptr u8 n ptr u8 n -- bool ) {: a:ptr u b:ptr v :}
   u v <> if SL-FALSE exit then
   0 begin dup u < while
      dup a + c@ over b + c@ <> if drop SL-FALSE exit then
      1+
   repeat drop SL-TRUE ;

: SL-USAGE ( -- )
   s" usage: bin/hb --load tools/argv.f tools/srclist.f -- [stage2|stdin|snap|build|aot]" SL-USAGE-RC die ;

: SL-DRIVER? ( ptr u8 n -- bool ) {: a:ptr u :}
   a u s" stage2" SL-STR= if SL-TRUE exit then
   a u s" stdin" SL-STR= if SL-TRUE exit then
   a u s" snap" SL-STR= if SL-TRUE exit then
   a u s" build" SL-STR= if SL-TRUE exit then
   a u s" aot" SL-STR= ;

: SL-DRIVER$ ( -- ptr u8 n )
   SCRIPT-ARGC 0= if s" stage2" exit then
   SCRIPT-ARGC 1 <> if SL-USAGE then
   0 SCRIPT-ARGV$ 2dup SL-DRIVER? 0= if SL-USAGE then ;

: SL-DRIVER! ( ptr u8 n -- ) {: a:ptr u :}
   a SL-DRIVER-A !
   u SL-DRIVER-U ! ;

: SL-TARGET-UNKNOWN ( -- )
   s" srclist: unknown target" SL-USAGE-RC die ;

: SL-TARGET-LAYOUT-SYS ( -- )
   HB-TARGET-LINUX? if
      s" src/os/linux/layout.f src/os/linux/sys.f " type
      exit
   then
   HB-TARGET-MACOS? if
      s" src/os/macos/layout.f src/os/macos/sys.f " type
      exit
   then
   SL-TARGET-UNKNOWN ;

: SL-TARGET-IMAGE ( -- )
   HB-TARGET-LINUX? if
      s" src/os/linux/elf.f src/os/linux/sign.f " type
      exit
   then
   HB-TARGET-MACOS? if
      s" src/os/macos/macho.f src/os/macos/sign2.f " type
      exit
   then
   SL-TARGET-UNKNOWN ;

: SL-PREFIX ( -- )
   s" src/arch/arm64/asm.f src/arch/arm64/icode.f " type
   s" src/arch/arm64/mnem.f " type
   SL-TARGET-LAYOUT-SYS
   s" src/core/sha256.f src/core/combinators.f " type
   s" src/habu/layout.f src/habu/treeshake.f src/habu/rt.f src/habu/crash.f " type
   SL-TARGET-IMAGE
   s" src/habu/habu1.f src/habu/prof.f src/habu/regalloc.f " type
   s" src/habu/jit.f src/habu/habu2.f src/habu/" type ;

: SRCLIST-MAIN ( -- )
   SL-DRIVER$ SL-DRIVER!
   SL-PREFIX
   SL-DRIVER-A @ SL-DRIVER-U @ type
   s" .f" type
   cr ;

SRCLIST-MAIN
