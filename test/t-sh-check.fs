\ t-sh-check.fs — the SOUND native checker wired as the compile hook. A def of
\ known prims is certified (-1) or rejected on a type error (0); a def the checker
\ can't fully model (control flow, literals, unknown words) is UNCHECKABLE (1) —
\ published but NOT falsely certified. Run: gforth test/t-sh-check.fs -e bye
require sh-driver.fs
: SOUND-OUT ( -- a u )
   0 CL !
   s" selfhost/util.fs"    slurp-file +B   s"  " +B
   s" selfhost/checker.fs"    slurp-file +B   s"  " +B
   s" selfhost/check-demo.fs" slurp-file +B
   CBUF CL @ NF-RUN  NFOUT 2@ ;
\ SQ (dup *)=-1 certified, BAD (dup 0= +)=0 rejected (type error),
\ BR (... IF ... THEN)=1 uncheckable (sound — no false pass), then 7 SQ=49.
T{ SOUND-OUT s\" -1\n0\n1\n49\n" compare 0= -> true }T
