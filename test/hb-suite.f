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
: DIP swap >r execute r> ;
: TDIP 10 3 [: 2 * ;] DIP + ;
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


\ floats (the f+ prim must be the FLOAT op — it was once shadowed by a
\ jit fold helper named f+)
: TFP 1.5 2.5 f+ 4.0 f= ;
TFP -1 T=
: TFL 1.5 2.5 f< ;
TFL -1 T=

\ exit inside a quotation targets the QUOTATION's epilogue (scoped chain)
: TQX [: dup 0 > if exit then drop 99 ;] execute ;
5 TQX 5 T=
0 TQX 99 T=

\ empty interpret string
s" " nip 0 T=

\ run-rc spawn failure -> -1 (not a hang or garbage status)
s" /nonexistent-habu-x" PATHZ run-rc -1 T=

\ snapshot-writer intrinsics are sane
dbase@ $300000000 = -1 T=
cp@ dbase@ - 0 > -1 T=
ndict@ 0 > -1 T=

\ register pool stress: 14 live VS values exceed the 13-reg pool (x9..x15,
\ x29, x25, x23, x24, x21, x22) mid-expression -> the 14th allocation takes
\ the spill path; sum proves no value was lost or aliased (1+..+14 = 105)
: TRP 1 2 3 4 5 6 7 8 9 10 11 12 13 14  + + + + + + + + + + + + + ;
TRP 105 T=

\ loop-resident registers: 12 loop-carried values + the counter (13 = the
\ full pool) survive a BEGIN/UNTIL back edge via the two-cell packed snapshot
: TLR 1 2 3 4 5 6 7 8 9 10 11 12  0 begin 1 + dup 3 = until drop  + + + + + + + + + + + ;
TLR 78 T=

\ locals register cache: repeat refs reuse the cached reg (one ldr total)...
: KL {: a :} a a + a + ;
5 KL 15 T=
\ ...a call spills and invalidates (the ref after must reload from the frame)
: KC {: a :} P5 drop a ;
3 KC 3 T=
\ ...and the cache claim survives a BEGIN back edge (loop-resident local)
: KR {: a :} 0 begin a + dup 15 < 0= until ;
5 KR 15 T=
: KW {: a :} 0 begin dup 12 < while a + repeat ;
4 KW 12 T=

\ report: count + nonzero exit on failure
: REPORT
   #FAIL @ 0 = if [char] o emit [char] k emit cr exit then
   #FAIL @ . s" hb-suite: failures" 1 die ;
REPORT
