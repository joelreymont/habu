\ t-sh-hex.fs — the standalone's number parser now accepts $hex (with optional leading
\ '-'), not just decimal. Feeds a few hex literals through the standalone and checks the
\ printed (decimal) results: $FF=255, $10 $10 +=32, -$2A=-42, $deadBEEF mixed-case.
\ Run: gforth test/t-sh-hex.fs -e bye
require sh-driver.fs
: HEX-OUT ( a u -- a2 u2 )  0 CL !  +B  CBUF CL @ NF-RUN  NFOUT 2@ ;
T{ s" $FF ."            HEX-OUT  s\" 255\n"        compare 0= -> true }T
T{ s" $10 $10 + ."      HEX-OUT  s\" 32\n"         compare 0= -> true }T
T{ s" -$2A ."           HEX-OUT  s\" -42\n"        compare 0= -> true }T
T{ s" $deadbeef ."      HEX-OUT  s\" 3735928559\n" compare 0= -> true }T
T{ s" $DEADBEEF ."      HEX-OUT  s\" 3735928559\n" compare 0= -> true }T
T{ s" 42 ."             HEX-OUT  s\" 42\n"         compare 0= -> true }T
