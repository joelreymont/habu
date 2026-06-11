\ t-sh-die.fs — the die prim ( a u code -- noreturn ): write msg to stderr, exit(code).
\ The in-subset abort for compiler invariant violations (no silent corruption).
\ Run: gforth test/t-sh-die.fs -e bye
require sh-driver.fs
: RC-OF ( a u -- code )  s" /tmp/nf-die-bin" FORTH-EXE
   s" /tmp/nf-die-bin >/dev/null 2>/tmp/nf-die-err; echo $? > /tmp/nf-die-rc" system
   s" /tmp/nf-die-rc" slurp-file s>number? 2drop ;
T{ s\" : GO s\" boom\" 72 die ; GO"  RC-OF -> 72 }T                 \ dies with the code
T{ s" /tmp/nf-die-err" slurp-file s" boom" compare 0= -> true }T    \ msg reached stderr
T{ s\" : GO s\" x\" 9 die ; GO 5 ."  RC-OF -> 9 }T                  \ nothing after die runs
