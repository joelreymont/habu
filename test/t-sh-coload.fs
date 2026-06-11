\ t-sh-coload.fs — the WHOLE selfhost layer compiled together under the standalone. Each
\ per-file test loads one source + its demo; none proves the layer CO-LOADS (no name
\ clashes, definition order holds, every word in-subset). This concatenates ALL ELEVEN
\ files (sha256 util asm icode mnem macho sign2 walk vs checker render disasm) and runs a
\ sentinel that prints a folded constant. A clean "2310\n" proves the standalone compiled
\ all of it (an out-of-subset word would exit 70 before the sentinel). Toward the
\ compiler fixpoint. Run: gforth test/t-sh-coload.fs -e bye
require sh-driver.fs
: GEN ( -- a u )
   0 CL !
   s" selfhost/sha256.fs" +F  s" selfhost/util.fs" +F  s" selfhost/asm.fs" +F
   s" selfhost/icode.fs" +F  s" selfhost/mnem.fs" +F  s" selfhost/macho.fs" +F  s" selfhost/sign2.fs" +F
   s" selfhost/walk.fs" +F  s" selfhost/vs.fs" +F  s" selfhost/checker.fs" +F
   s" selfhost/render.fs" +F  s" selfhost/disasm.fs" +F
   s" : GO $700 $206 + . ; GO" +B               \ hex sentinel: $700+$206 = 2310
   CBUF CL @ NF-RUN  NFOUT 2@ ;
T{ GEN  s\" 2310\n" compare 0= -> true }T        \ all 11 files co-load + hex parse
