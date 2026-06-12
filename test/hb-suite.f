\ hb-suite.f — the behavior suite run BY THE ENGINE ITSELF (bin/hbi), no
\ gforth. Golden/parity tests stay gforth-side (they compare against the
\ bootstrap builder); everything behavioral runs here. A failure prints
\ F<index> and the run exits 1 via the final report.

variable #FAIL
variable #CASE

: T= {: got want :}
   #CASE @ 1 + #CASE !
   got want <> if
     [char] F emit #CASE @ .
     #FAIL @ 1 + #FAIL !
   then ;

\ arithmetic, stack, comparisons
5 dup * 25 T=
1 2 3 rot + + 6 T=
10 3 - 7 T=
-1 $FF and 255 T=
7 2 mod 1 T=
$10 4 lshift $100 T=
5 3 > -1 T=
5 3 <= 0 T=

\ control flow
: TIF dup 5 > if drop 99 else 1 + then ;
3 TIF 4 T=
9 TIF 99 T=
: TLOOP 0 begin 1 + dup 10 >= until ;
TLOOP 10 T=
: TDO 0 5 0 do i + loop ;
TDO 10 T=
: TQDO 0 3 3 ?do 1 + loop ;
TQDO 0 T=
: TPLOOP 0 10 0 do 1 + 2 +loop ;
TPLOOP 5 T=
: TJ 0 3 0 do 4 0 do j + loop loop ;
TJ 12 T=
: TLEAVE 0 10 0 do 1 + dup 4 = if leave then loop ;
TLEAVE 4 T=

\ return stack, exit, recurse
: TRS 1 2 >r 10 + r> + ;
TRS 13 T=
: TEXIT dup 5 > if drop 99 exit then 1 + ;
3 TEXIT 4 T=
7 TEXIT 99 T=
: FIB dup 2 < if drop 1 exit then dup 1- recurse swap 2 - recurse + ;
10 FIB 89 T=

\ locals (typed)
: TLOC {: a:n b:n :} a b + ;
3 4 TLOC 7 T=

\ create/does>
: CONST create , does> @ ;
5 CONST FIVE
FIVE 5 T=
: ARR create cells allot does> swap cells + ;
4 ARR A4
7 2 A4 !
2 A4 @ 7 T=

\ quotations + combinators
: TQ1 5 [: 1 + ;] execute ;
TQ1 6 T=
: dip swap >r execute r> ;
: TDIP 10 3 [: 2 * ;] dip + ;
TDIP 23 T=

\ immediate / postpone / compile,
: IM5 5 ; immediate
: TI IM5 ;
TI 5 T=
: P5 postpone IM5 ; immediate
: TP P5 ;
TP 5 T=

\ child processes: run-rc spawns + waits (paths need a NUL)
create PZB 64 allot
: PATHZ {: a u :}
   0 begin dup u < while  dup a + c@  over PZB + c!  1 + repeat drop
   0 PZB u + c!  PZB ;
s" /usr/bin/true" PATHZ run-rc 0 T=
s" /usr/bin/false" PATHZ run-rc 1 T=

\ report: count + nonzero exit on failure
: REPORT
   #FAIL @ 0 = if [char] o emit [char] k emit cr exit then
   #FAIL @ . s" hb-suite: failures" 1 die ;
REPORT
