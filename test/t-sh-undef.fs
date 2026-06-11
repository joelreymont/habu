\ t-sh-undef.fs — the standalone now ERRORS on an undefined word in a : body (was a
\ silent no-op that hid real bugs, e.g. 0< / STR= compiling to nothing). It writes the
\ name to stderr and exit(70). A defined body still runs. Run: gforth test/t-sh-undef.fs -e bye
require nf.fs
require tester.fs
: RC-OF ( src-a src-u -- code )            \ build standalone on src, run, return exit code
   s" /tmp/nf-u-bin" FORTH-EXE
   s" /tmp/nf-u-bin >/dev/null 2>/dev/null; echo $? > /tmp/nf-u-rc" system
   s" /tmp/nf-u-rc" slurp-file s>number? 2drop ;
T{ s" : GO 5 zork drop ; GO"   RC-OF -> 70 }T   \ undefined zork -> exit 70
T{ s" : GO 6 7 * . ; GO"       RC-OF ->  0 }T   \ all defined -> ok (prints 42, exit 0)
