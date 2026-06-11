\ t-sh-cg.fs — the standalone GENERATES native code with its own toolchain: encoders
\ (asm.fs) + single-pass assembler with labels/branches (icode.fs) assemble a loop
\ computing exit(5+4+3+2+1=15); it emits a self-signed Mach-O (full Mach-O builder (macho.fs+sign2.fs)) with
\ ZERO external tools. Assert the generated binary runs and exits 15.
\ Run: gforth test/t-sh-cg.fs -e bye
require sh-driver.fs
: GEN ( -- )                              \ build the standalone codegen + run it -> /tmp/sh-cg-bin
   0 CL !
   s" selfhost/sha256.fs"   slurp-file +B   s"  " +B
   s" selfhost/asm.fs"      slurp-file +B   s"  " +B
   s" selfhost/icode.fs"    slurp-file +B   s"  " +B
   s" selfhost/macho.fs"    slurp-file +B   s"  " +B
   s" selfhost/sign2.fs"    slurp-file +B   s"  " +B
   s" selfhost/cg-demo.fs"  slurp-file +B
   CBUF CL @ NF-RUN ;
: RC ( -- n )  s" /tmp/sh-cg-bin; echo $? > /tmp/sh-cg-rc" system
   s" /tmp/sh-cg-rc" slurp-file  s>number? 2drop ;
GEN
T{ RC -> 15 }T
