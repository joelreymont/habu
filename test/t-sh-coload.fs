\ t-sh-coload.fs — the WHOLE codegen layer compiled together under the standalone. Each
\ per-file test loads one source + its demo; none proves the layer CO-LOADS (no name
\ clashes, definition order holds, every word in-subset). This concatenates sha256 +
\ macho-min + sign + asm + icode + walk + vs + disasm and runs a sentinel that prints a
\ folded constant. A clean "1806\n" proves the standalone compiled all of it (an
\ out-of-subset word would exit 70 before the sentinel). Toward the compiler fixpoint
\ (the standalone compiling its own compiler). Run: gforth test/t-sh-coload.fs -e bye
require sh-driver.fs
: GEN ( -- a u )
   0 CL !
   s" selfhost/sha256.fs"   slurp-file +B   s"  " +B
   s" selfhost/macho-min.fs" slurp-file +B   s"  " +B
   s" selfhost/sign.fs"     slurp-file +B   s"  " +B
   s" selfhost/asm.fs"      slurp-file +B   s"  " +B
   s" selfhost/icode.fs"    slurp-file +B   s"  " +B
   s" selfhost/walk.fs"     slurp-file +B   s"  " +B
   s" selfhost/vs.fs"       slurp-file +B   s"  " +B
   s" selfhost/disasm.fs"   slurp-file +B   s"  " +B
   s" : GO $700 $206 + . ; GO" +B               \ 1792 + 518 = 2310... use hex sentinel
   CBUF CL @ NF-RUN  NFOUT 2@ ;
T{ GEN  s\" 2310\n" compare 0= -> true }T        \ $700 + $206 = 2310 (hex parse + co-load)
