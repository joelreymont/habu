\ hb-suite.f — the behavior suite run BY THE ENGINE ITSELF (bin/hb), no
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
\ POSTPONE is compiler-manipulating; this fixture tests the runtime primitive,
\ not checked user code.
0 set-check
: P5 postpone IM5 ; immediate
: TP P5 ;
TP 5 T=
' HB-CHECK-HOOK set-check

\ child processes: run-rc spawns + waits (paths need a NUL)
create PZB 64 allot
: PATHZ {: a u :}
   0 begin dup u < while  dup a + c@  over PZB + c!  1 + repeat drop
   0 PZB u + c!  PZB ;
s" /usr/bin/true" PATHZ run-rc 0 T=
s" /usr/bin/false" PATHZ run-rc 1 T=

\ filesystem syscalls
create STB 256 allot
create DBUF 4096 allot
create DIRBASE 8 allot
variable DFD
: U16@ {: a :} a c@ a 1 + c@ 8 lshift or ;
: MODE@ STB 4 + U16@ ;
s" AGENTS.md" PATHZ 0 access 0 T=
s" /nonexistent-habu-fs" PATHZ 0 access -1 T=
s" AGENTS.md" PATHZ STB stat64 0 T=
MODE@ $F000 and $8000 = -1 T=
s" src" PATHZ STB stat64 0 T=
MODE@ $F000 and $4000 = -1 T=
s" src/os/macos" PATHZ open-rd DFD !
DFD @ 0 >= -1 T=
0 DIRBASE !
DFD @ DBUF 4096 DIRBASE getdirentries64 0 > -1 T=
DFD @ close

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

\ time primitives: deterministic shape/range only, never exact wall time
epoch-seconds 1600000000 > -1 T=
: TEPOCH epoch-seconds 1600000000 > ;
TEPOCH -1 T=
mono-ns mono-ns <= -1 T=
: TMONO-ELAPSED mono-ns 0 100000 0 do i + loop drop mono-ns swap - ;
TMONO-ELAPSED 0 > -1 T=

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

\ float VS: d-reg binops (FADD path), dup of a float constant, and a
\ loop-resident float accumulator surviving BEGIN back edges in a d-reg
: TFD 2.0 dup f+ 4.0 f= ;
TFD -1 T=
: TFA {: n :} 0.0 0 begin 1 + swap 1.5 f+ swap dup n = until drop 6.0 f= ;
4 TFA -1 T=
\ a call spills the float (bits to the memory stack); the prim path finishes
: TF5 5 ;
: TFC 0.5 TF5 drop 0.5 f+ 1.0 f= ;
TFC -1 T=

\ float pool: deep expression spills past d8..d15 (10 live floats), the other
\ binops, and a float carried through a quotation
: TFS 1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 9.0 10.0 f+ f+ f+ f+ f+ f+ f+ f+ f+ 55.0 f= ;
TFS -1 T=
: TFM 3.0 4.0 f* 12.0 f= ;
TFM -1 T=
: TFV 10.0 4.0 f/ 2.5 f= ;
TFV -1 T=
: TFQ 2.0 [: 3.0 f+ ;] execute 5.0 f= ;
TFQ -1 T=
: TFG 5.0 3.0 f> ;
TFG -1 T=

\ report: count + nonzero exit on failure
: REPORT
   #FAIL @ 0 = if [char] o emit [char] k emit cr exit then
   #FAIL @ . s" hb-suite: failures" 1 die ;
REPORT
