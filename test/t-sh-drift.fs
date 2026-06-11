\ t-sh-drift.fs — DRIFT GUARD: the standalone's hand-transcribed encoders + Mach-O
\ builder must stay byte-identical to caf's. caf and the standalone each emit the
\ exit(42) Mach-O; assert identical (the standalone's asm/macho hasn't drifted).
\ Run: gforth test/t-sh-drift.fs -e bye
require ../src/cg/exec.fs
require nf.fs
require tester.fs
\ caf's unsigned Mach-O for exit(42)
ICODE-RESET 0 42 MOVZ, 16 1 MOVZ, $80 SVC, BUILD-MACHO  s" /tmp/caf42" WRITE-EXE
\ standalone's: macho-min.fs (BUILD) + exit42.fs (SAVE) -> writes /tmp/se-out
create DBUF 16384 allot   variable DLN
: D+ {: a u -- }  a  DBUF DLN @ +  u move  u DLN +! ;
0 DLN !
s" selfhost/macho-min.fs" slurp-file D+   s"  " D+
s" selfhost/exit42.fs"    slurp-file D+
DBUF DLN @ NF-RUN
: DRIFT? ( -- f )  s" /tmp/caf42" slurp-file  s" /tmp/se-out" slurp-file  compare 0= ;
T{ DRIFT? -> true }T
