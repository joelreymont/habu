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
\ return-row modeling: >r/r>/r@ typed; a definition must leave the return
\ stack balanced (ANS 3.2.3.3) — net push, net pop, or per-iteration loop
\ growth is a type error.
T{ s" : RB 5 >r 3 r@ + r> + . ;"                  CHK2 s\" -1\n" compare 0= -> true }T
T{ s" : RX >r ;"                                  CHK2 s\" 0\n"  compare 0= -> true }T
T{ s" : RY r> drop ;"                             CHK2 s\" 0\n"  compare 0= -> true }T
T{ s" : RZ 0 begin >r 1+ dup 5 >= until drop ;"   CHK2 s\" 0\n"  compare 0= -> true }T
T{ s" : RC 7 >r 1 if r@ else 0 then r> drop + . ;" CHK2 s\" -1\n" compare 0= -> true }T
\ TRUST: declare a sig for a word the checker can't model — callers certify.
\ (recurse makes MYST uncheckable=1; the trusted sig lets USES certify=-1.)
T{ s\" : MYST dup 0 > if 1- recurse then ; s\" myst\" s\" n -- n\" TRUST : USES 5 MYST . ; USES"
   CHK2 s\" 1\n-1\n0\n" compare 0= -> true }T
\ typed local assertions: {: a:n :} binds the local's type var to the
\ asserted type; references use the bare name; a wrong use rejects.
T{ s" : L2 {: a:n :} a 1 + . ;"   CHK2 s\" -1\n" compare 0= -> true }T
T{ s" : LB {: a:r :} a 1 + . ;"   CHK2 s\" 0\n"  compare 0= -> true }T
T{ s" : LR {: x:r :} x f. ;"      CHK2 s\" -1\n" compare 0= -> true }T
\ CREATE...DOES> typing: a word created INSIDE a defining word publishes
\ UNRECORDED (its does>-patched effect is arbitrary — recording `-- n` would
\ be unsound); the author declares it with trust, and callers certify.
T{ s\" : ARR create cells allot does> swap cells + ; s\" a4\" s\" n -- n\" TRUST 4 ARR A4 : USE 2 A4 @ . ; 7 2 A4 ! USE"
   CHK2 s\" 1\n-1\n7\n" compare 0= -> true }T
\ quotation typing: [: ;] infers a quot<effect> type; execute applies it; the
\ omega combinator rejects via the occurs check (never loops); quot-bearing
\ sigs render '?' and stay unrecorded (callers go uncheckable, not unsound).
T{ s" : T0 [: ;] drop ;"                          CHK2 s\" -1\n" compare 0= -> true }T
T{ s" : T1 [: 7 ;] execute . ;"                   CHK2 s\" -1\n" compare 0= -> true }T
T{ s" : TB 1.5 [: 1 + ;] execute . ;"             CHK2 s\" 0\n"  compare 0= -> true }T
T{ s" : W [: dup execute ;] dup execute ;"        CHK2 s\" 0\n"  compare 0= -> true }T
\ Gap2/3: dip's quot sig now RECORDS as a scheme-string and round-trips, so
\ the caller T is CHECKED against it (-1) instead of uncheckable (1) before.
T{ s" : dip swap >r execute r> ; : T 10 3 [: 2 * ;] dip + . ; T" CHK2 s\" -1\n-1\n23\n" compare 0= -> true }T
\ EXIT: an early return. Every return point (each exit + the fall-through at ';')
\ must leave the same stack. unloop is a typing no-op. Dead code after exit in a
\ branch is excluded from the THEN join; a BEGIN..AGAIN returns only via exit.
T{ s" : EX1 {: n :} n 0 < if 0 exit then n ;"            CHK2 s\" -1\n" compare 0= -> true }T
T{ s" : EXB {: n :} n 0 < if 0 0 exit then n ;"          CHK2 s\" 0\n"  compare 0= -> true }T
T{ s" : EXD {: a u :} u 0 ?do a i + c@ 0= if unloop 0 exit then loop -1 ;"  CHK2 s\" -1\n" compare 0= -> true }T
T{ s" : EXDB {: a u :} u 0 ?do a i + c@ 0= if unloop 0 0 exit then loop -1 ;" CHK2 s\" 0\n" compare 0= -> true }T
T{ s" : EXG {: a :} 0 begin dup 9 < while dup a = if drop -1 exit then 1+ repeat drop 0 ;" CHK2 s\" -1\n" compare 0= -> true }T
\ exit inside a [: ;] quotation returns from the QUOTE, not the colon def — its
\ early returns stay scoped to the quote (a nested inference), so the outer word
\ infers normally. (The unsound LEAK is caught by CHECK! verify — see t-sh-verify.)
T{ s" : QXL [: 1 2 3 exit ;] execute ;"           CHK2 s\" -1\n" compare 0= -> true }T   \ quote ( -- n n n )
T{ s" : QXN 5 [: exit ;] execute 1 2 3 4 5 ;"      CHK2 s\" -1\n" compare 0= -> true }T   \ infers ( -- n*6 )
\ divergent exits in the SAME scope are inconsistent -> rejected even by infer mode
T{ s" : XD1 {: n :} n if 7 7 exit then n ;"       CHK2 s\" 0\n"  compare 0= -> true }T
T{ s" : XD2 {: n :} n if 1 exit else 2 2 exit then ;" CHK2 s\" 0\n" compare 0= -> true }T
\ regression: the jit fold helpers must not shadow the FLOAT prims (they
\ were named f+/f-/f* once — any toolchain-loaded engine lost float ops and
\ their checker sigs).
T{ s" : FF 1 1 f+ . ;"   CHK2 s\" 0\n"  compare 0= -> true }T
