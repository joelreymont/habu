\ t-sh-guards.fs — habu's runtime limits fail LOUDLY, never corrupt: dictionary
\ cap (exit 77 at ~1300 words), 17th local (exit 75), and a 1500-deep data stack
\ that the old 2KB region would have overrun. Run: gforth test/t-sh-guards.fs -e bye
require sh-driver.fs
: RC-OF ( a u -- code )  s" /tmp/hb-g-bin" FORTH-EXE
   s" /tmp/hb-g-bin >/dev/null 2>/dev/null; echo $? > /tmp/hb-g-rc" system
   s" /tmp/hb-g-rc" slurp-file s>number? 2drop ;
\ 1301 tiny definitions -> dictionary-full death, not corruption
: MANY ( -- a u )  0 CL !
   1301 0 do  s" : W" +B  i 0 <# #s #> +B  s"  1 ; " +B  loop
   s" 42 ." +B  CBUF CL @ ;
T{ MANY RC-OF -> 77 }T
T{ s" : T {: a b c d e f g h i2 j k l m n o p q :} 1 . ; T"  RC-OF -> 75 }T   \ 17 locals
: DEEP ( -- a u )  0 CL !
   1500 0 do s" 7 " +B loop  1500 0 do s" drop " +B loop  s" 8 ." +B  CBUF CL @ ;
T{ DEEP RC-OF -> 0 }T
