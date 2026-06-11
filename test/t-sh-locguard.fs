\ t-sh-locguard.fs — the engine refuses the two documented locals footguns at
\ COMPILE time (token to stderr + exit 75) instead of corrupting the frame:
\ (1) {: :} inside IF/BEGIN/DO, (2) a local named i/I (shadowed by the loop index).
\ Run: gforth test/t-sh-locguard.fs -e bye
require sh-driver.fs
: RC-OF ( a u -- code )  s" /tmp/nf-lg-bin" FORTH-EXE
   s" /tmp/nf-lg-bin >/dev/null 2>/dev/null; echo $? > /tmp/nf-lg-rc" system
   s" /tmp/nf-lg-rc" slurp-file s>number? 2drop ;
T{ s" : GO 1 if {: x :} then ; GO"          RC-OF -> 75 }T   \ {: inside IF
T{ s" : GO 5 begin {: x :} 0 until ; GO"    RC-OF -> 75 }T   \ {: inside BEGIN
T{ s" : GO 5 {: i :} i . ; GO"              RC-OF -> 75 }T   \ local named i
T{ s" : GO 5 {: I :} 1 . ; GO"              RC-OF -> 75 }T   \ local named I
T{ s" : GO 5 {: ix :} ix . ; GO"            RC-OF ->  0 }T   \ ix is fine (prints 5)
T{ s" : GO {: a b :} a b + . ; 3 4 GO"      RC-OF ->  0 }T   \ normal locals still work
