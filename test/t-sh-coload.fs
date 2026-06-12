\ t-sh-coload.fs — the WHOLE selfhost layer compiled together under the standalone. Each
\ per-file test loads one source + its demo; none proves the layer CO-LOADS (no name
\ clashes, definition order holds, every word in-subset). This concatenates ALL TEN
\ files (sha256 util asm icode mnem macho sign2 checker render disasm) and runs a
\ sentinel that prints a folded constant. A clean "2310\n" proves the standalone compiled
\ all of it (an out-of-subset word would exit 70 before the sentinel). Toward the
\ compiler fixpoint. Run: gforth test/t-sh-coload.fs -e bye
require sh-driver.fs
: GEN ( -- a u )
   0 CL !
   s" src/core/sha256.f" +F  s" src/core/util.f" +F  s" src/arch/arm64/asm.f" +F
   s" src/arch/arm64/icode.f" +F  s" src/arch/arm64/mnem.f" +F  s" src/os/macos/sys.f" +F  s" src/os/macos/macho.f" +F  s" src/os/macos/sign2.f" +F
   s" src/core/checker.f" +F
   s" src/core/render.f" +F  s" src/arch/arm64/disasm.f" +F
   s" : GO $700 $206 + . ; GO" +B               \ hex sentinel: $700+$206 = 2310
   CBUF CL @ NF-RUN  NFOUT 2@ ;
T{ GEN  s\" 2310\n" compare 0= -> true }T        \ all 11 files co-load + hex parse
