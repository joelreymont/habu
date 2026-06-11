\ t-sh-mem.fs — the standalone codegen with MEMORY OPS: compiles "here 42 over ! @"
\ (store via str, load via ldr, HERE = scratch buffer) and the self-signed binary stores
\ 42 then loads it back -> exit 42. Toward the compiler's own language surface (the
\ fixpoint needs @/!). Run: gforth test/t-sh-mem.fs -e bye
require sh-driver.fs
: GEN ( -- )
   0 CL !
   s" selfhost/sha256.f"   slurp-file +B   s"  " +B
   s" selfhost/asm.f"      slurp-file +B   s"  " +B
   s" selfhost/icode.f"    slurp-file +B   s"  " +B
   s" selfhost/util.f"    slurp-file +B   s"  " +B
   s" selfhost/walk.f"     slurp-file +B   s"  " +B
   s" selfhost/vs.f"       slurp-file +B   s"  " +B
   s" selfhost/macho.f"    slurp-file +B   s"  " +B
   s" selfhost/sign2.f"    slurp-file +B   s"  " +B
   s" selfhost/mem-demo.f" slurp-file +B
   CBUF CL @ NF-RUN ;
: RC ( -- n )  s" /tmp/sh-mem-bin; echo $? > /tmp/sh-mem-rc" system
   s" /tmp/sh-mem-rc" slurp-file  s>number? 2drop ;
GEN  T{ RC -> 42 }T
