\ t-sh-check.fs — the SOUND native checker wired as the compile hook. A def of
\ known prims is certified (-1) or rejected on a type error (0); a def the checker
\ can't fully model (control flow, literals, unknown words) is UNCHECKABLE (1) —
\ published but NOT falsely certified. Run: gforth test/t-sh-check.fs -e bye
require sh-driver.fs
: SOUND-OUT ( -- a u )
   0 CL !
   s" src/core/util.f"    slurp-file +B   s"  " +B
   s" src/core/checker.f"    slurp-file +B   s"  " +B
   s" test/demos/check-demo.f" slurp-file +B
   CBUF CL @ NF-RUN  NFOUT 2@ ;
\ SQ (dup *)=-1 certified, BAD (dup 1.5 +)=0 rejected (float into int +),
\ BR (... IF ... THEN)=-1 certified (branches modeled, joins unified), 7 SQ=49.
T{ SOUND-OUT s\" -1\n0\n-1\n49\n" compare 0= -> true }T
\ locals + control flow modeling, and recorded user sigs enforced at call sites
: CHK2 ( a u -- a u )  0 CL !
   s" src/core/util.f" +F  s" src/core/checker.f" +F  s" src/core/render.f" +F
   s" : HOOK CHECK dup . ; ' HOOK set-check " +B  +B
   CBUF CL @ NF-RUN  NFOUT 2@ ;
T{ s" : L1 {: a b :} a b + . ;"                    CHK2 s\" -1\n" compare 0= -> true }T
T{ s" : BB {: a :} a 0 > if a a else a then . ;"   CHK2 s\" 0\n"  compare 0= -> true }T
T{ s" : W1 0 begin dup 10 < while 1+ repeat . ;"   CHK2 s\" -1\n" compare 0= -> true }T
T{ s" : D1 0 5 0 do i + loop . ;"                  CHK2 s\" -1\n" compare 0= -> true }T
T{ s" : G 1.5 ; : B G 1 + ;"                       CHK2 s\" -1\n0\n" compare 0= -> true }T
T{ s\" : SD s\" hi\" type ; : TK ['] SD drop ;"    CHK2 s\" -1\n-1\n" compare 0= -> true }T
T{ s" : NG -1 $FF and . ;"                         CHK2 s\" -1\n" compare 0= -> true }T
