\ engine-suite.f — the behavior suite run BY THE ENGINE ITSELF (bin/hb), no
\ gforth. A failure prints F<index> and the run exits 1 via the final report.

variable #FAIL
variable #CASE

: T= ( n n -- ) {: got:n want:n :}
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
$AF 175 T=
-$10 -16 T=
: TMOVK-LITS ( -- n n n n ) 0 -1 $123456789ABCDEF0 $FFFFFFFF0000FFFF ;
TMOVK-LITS $FFFFFFFF0000FFFF T= $123456789ABCDEF0 T= -1 T= 0 T=
7 2 mod 1 T=
7 2 /mod 3 T= 1 T=
$10 4 lshift $100 T=
5 3 > -1 T=
5 3 <= 0 T=
-7 abs 7 T=
3 7 min 3 T=
3 7 max 7 T=
1 2 3 4 2swap 2 T= 1 T= 4 T= 3 T=
1 2 3 4 2over 2 T= 1 T= 4 T= 3 T= 2 T= 1 T=
0 ?dup 0 T=
5 ?dup + 10 T=
depth 0 T=
1 2 depth 2 T= 2 T= 1 T=
: TDEPTH ( i64 i64 -- i64 i64 i64 ) depth ;
3 4 TDEPTH 2 T= 4 T= 3 T=
8 cell+ 16 T=
8 char+ 9 T=
8 chars 8 T=
depth 0 T=
1 2 3 depth nip nip nip 3 T=

\ control flow
: TIF ( n -- n ) dup 5 > if drop 99 else 1 + then ;
3 TIF 4 T=
9 TIF 99 T=
: TLOOP ( -- n ) 0 begin 1 + dup 10 >= until ;
TLOOP 10 T=
: TDO ( -- n ) 0 5 0 do i + loop ;
TDO 10 T=
: TQDO ( -- n ) 0 3 3 ?do 1 + loop ;
TQDO 0 T=
: TPLOOP ( -- n ) 0 10 0 do 1 + 2 +loop ;
TPLOOP 5 T=
: TJ ( -- n ) 0 3 0 do 4 0 do j + loop loop ;
TJ 12 T=
: TLEAVE ( -- n ) 0 10 0 do 1 + dup 4 = if leave then loop ;
TLEAVE 4 T=

\ return stack, exit, recurse
: TRS ( -- n ) 1 2 >r 10 + r> + ;
TRS 13 T=
: TRS2 ( -- n ) 1 2 2>r 10 2r> + + ;
TRS2 13 T=
: TRS2@ ( -- n ) 1 2 2>r 2r@ + 2r> + + ;
TRS2@ 6 T=
: TEXIT ( n -- n ) dup 5 > if drop 99 exit then 1 + ;
3 TEXIT 4 T=
7 TEXIT 99 T=
: FIB ( i64 -- i64 ) dup 2 < if drop 1 exit then dup 1- recurse swap 2 - recurse + ;
10 FIB 89 T=

\ locals (typed)
: TLOC ( n n -- n ) {: a:n b:n :} a b + ;
3 4 TLOC 7 T=

\ create/does>
: CONST ( n -- ) create , does> ( -- n ) @ ;
5 CONST FIVE
FIVE 5 T=
: ARR ( n -- ) create cells allot does> ( n -- ptr a ) swap cells + ;
4 ARR A4
7 2 A4 !
2 A4 @ 7 T=
here 3 over c! 65 over 1 + c! 66 over 2 + c! count 3 T= drop
here 10 over ! 5 over +! @ 15 T=

\ quotations + combinators
: TQ1 ( -- n ) 5 [: 1 + ;] execute ;
TQ1 6 T=
: RUN-R> ( [ -- i64 | i64 -- ] -- i64 ) 7 >r execute ;
: TQRIN ( -- n ) [: r> ;] RUN-R> ;
TQRIN 7 T=
: RUN->R ( [ -- | -- i64 ] -- i64 ) execute r> ;
: TQROUT ( -- n ) [: 9 >r ;] RUN->R ;
TQROUT 9 T=
: TDIP ( -- n ) 10 3 [: 2 * ;] DIP + ;
TDIP 23 T=
: TKEEP ( -- n ) 7 [: 1+ ;] KEEP + ;
TKEEP 15 T=
: TBI ( -- n ) 5 [: 1+ ;] [: 2 * ;] BI + ;
TBI 16 T=
: TTRI ( -- n ) 3 [: 1+ ;] [: 2 * ;] [: 3 + ;] TRI + + ;
TTRI 16 T=
: TTIMES ( -- n ) 0 5 [: 1+ ;] TIMES ;
TTIMES 5 T=
create IARR 3 cells allot
1 IARR !  2 IARR cell+ !  3 IARR cell+ cell+ !
: TEACH ( -- n ) 0 IARR 3 [: + ;] EACH ;
TEACH 6 T=
: TFOLD ( -- n ) IARR 3 0 [: + ;] FOLD ;
TFOLD 6 T=
: TMAP ( -- ) IARR 3 [: 1+ ;] MAP ;
TMAP
TFOLD 9 T=
TRUSTED: T-CHECK-REJECTS ( ptr u8 n -- )
   DIAGXT @ >r
   0 DIAGXT !
   CHECK! 0 T=
   r> DIAGXT ! ;
s" CBAD-DIP ( i64 i64 -- i64 ) [: 1+ ;] DIP" T-CHECK-REJECTS
s" CBAD-KEEP ( i64 -- i64 ) [: 1+ ;] KEEP" T-CHECK-REJECTS
s" CBAD-BI ( i64 -- i64 ) [: 1+ ;] [: drop ;] BI" T-CHECK-REJECTS
s" CBAD-TIMES ( i64 -- i64 i64 ) 5 [: 1+ ;] TIMES" T-CHECK-REJECTS
s" CBAD-MAP ( ptr i64 i64 -- i64 ) [: 1+ ;] MAP" T-CHECK-REJECTS
s" CBAD-QLOCAL ( i64 -- i64 ) {: x:n :} [: x ;] execute" T-CHECK-REJECTS
: TROLE-REG ( n -- n ) >REG REG>N ;
7 TROLE-REG 7 T=
: TROLE-LABEL ( n -- n ) >LABEL LABEL>N ;
8 TROLE-LABEL 8 T=
: TROLE-VA ( n -- n ) >VA VA>N ;
9 TROLE-VA 9 T=
: TROLE-SYMIDX ( n -- n ) >SYMIDX SYMIDX>N ;
10 TROLE-SYMIDX 10 T=
s" CBAD-REG-LABEL ( reg label -- reg ) nip" T-CHECK-REJECTS
s" CBAD-VA-SYMIDX ( va symidx -- va ) nip" T-CHECK-REJECTS
: ES-BYTE-FIELD ( ptr n -- ptr ptr u8 ) 0 ptr-field ;
s" CBAD-FIELD ( ptr n n -- ) swap ES-BYTE-FIELD !" T-CHECK-REJECTS

\ immediate / postpone / compile,
: IM5 ( -- n ) 5 ; immediate
: TI ( -- n ) IM5 ;
TI 5 T=
\ POSTPONE is compiler-manipulating; this fixture tests the runtime primitive,
\ not checked user code. TP must compile through P5 while the trusted immediate
\ boundary is active.
TRUSTED: P5 ( -- i64 ) postpone IM5 ; immediate
: TP ( -- n ) P5 ;
TP 5 T=

\ child processes: run-rc spawns + waits (paths need a NUL)
create PZB 64 allot
: PATHZ ( ptr u8 n -- ptr u8 ) {: a:ptr u:n :}
   0 begin dup u < while  dup a + c@  over PZB + c!  1 + repeat drop
   0 PZB u + c!  PZB ;
s" /usr/bin/true" PATHZ run-rc 0 T=
s" /usr/bin/false" PATHZ run-rc 1 T=

\ filesystem syscalls
create STB 256 allot
create DBUF 4096 allot
create RLB 64 allot
create ES-TARGETZ
   65 c, 71 c, 69 c, 78 c, 84 c, 83 c, 46 c, 109 c, 100 c, 0 c,
create ES-LINKZ
   47 c, 116 c, 109 c, 112 c, 47 c, 104 c, 97 c, 98 c, 117 c, 45 c,
   101 c, 110 c, 103 c, 105 c, 110 c, 101 c, 45 c, 115 c, 117 c,
   105 c, 116 c, 101 c, 45 c, 108 c, 105 c, 110 c, 107 c, 0 c,
create DIRBASE 8 allot
variable DFD
: U16@ ( ptr u8 -- n ) {: a:ptr :} a c@ a 1 + c@ 8 lshift or ;
: MODE@ ( -- n ) STB 4 + U16@ ;
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
s" /tmp/habu-engine-suite-mkdir" PATHZ rmdir drop
s" /tmp/habu-engine-suite-mkdir" PATHZ 493 mkdir 0 T=
s" /tmp/habu-engine-suite-mkdir" PATHZ STB stat64 0 T=
MODE@ $F000 and $4000 = -1 T=
s" /tmp/habu-engine-suite-mkdir" PATHZ rmdir 0 T=
ES-LINKZ unlink drop
ES-TARGETZ ES-LINKZ symlink 0 T=
ES-LINKZ STB lstat64 0 T=
MODE@ $F000 and $A000 = -1 T=
ES-LINKZ RLB 64 readlink 9 T=
RLB c@ 65 T=
ES-LINKZ unlink 0 T=
0 4096 3 $1002 -1 0 mmap dup 0 < 0 T= dup 65 swap c! c@ 65 T=

\ floats (the f+ prim must be the FLOAT op — it was once shadowed by a
\ jit fold helper named f+)
: TFP ( -- bool ) 1.5 2.5 f+ 4.0 f= ;
TFP -1 T=
: TFL ( -- bool ) 1.5 2.5 f< ;
TFL -1 T=
: TFNEG ( -- bool ) -1.5 -2.5 f+ -4.0 f= ;
TFNEG -1 T=

\ exit inside a quotation targets the QUOTATION's epilogue (scoped chain)
: TQX ( n -- n ) [: dup 0 > if exit then drop 99 ;] execute ;
5 TQX 5 T=
0 TQX 99 T=

\ empty interpret string
s" " nip 0 T=

\ run-rc spawn failure -> -1 (not a hang or garbage status)
s" /nonexistent-habu-x" PATHZ run-rc -1 T=

\ snapshot-writer intrinsics are sane
$340000000 constant ES-LINUX-DATA-VA
$44000000000 constant ES-MACOS-DATA-VA
: ES-TARGET-UNKNOWN ( -- )
   s" engine-suite: unknown target" 76 die ;

: ES-DATA-VA ( -- n )
   HB-TARGET-LINUX? if ES-LINUX-DATA-VA exit then
   HB-TARGET-MACOS? if ES-MACOS-DATA-VA exit then
   ES-TARGET-UNKNOWN ;
dbase@ $300000000 = -1 T=
data-base ES-DATA-VA = -1 T=
cp@ dbase@ - 0 > -1 T=
ndict@ 0 > -1 T=

\ time primitives: deterministic shape/range only, never exact wall time
epoch-seconds 1600000000 > -1 T=
: TEPOCH ( -- bool ) epoch-seconds 1600000000 > ;
TEPOCH -1 T=
: TEPOCH-DEPTH ( -- bool ) depth >r epoch-seconds drop depth r> = ;
TEPOCH-DEPTH -1 T=
create TEPOCH-BYTE 120 c,
: TEPOCH-AFTER-WRITEERR ( -- bool )
   99 TEPOCH-BYTE 1 write drop epoch-seconds 1600000000 > ;
TEPOCH-AFTER-WRITEERR -1 T=
mono-ns mono-ns <= -1 T=
: TMONO-ELAPSED ( -- n ) mono-ns 0 100000 0 do i + loop drop mono-ns swap - ;
TMONO-ELAPSED 0 > -1 T=

\ register pool stress: 14 live VS values exceed the 13-reg pool (x9..x15,
\ x29, x25, x23, x24, x21, x22) mid-expression -> the 14th allocation takes
\ the spill path; sum proves no value was lost or aliased (1+..+14 = 105)
: TRP ( -- n )
   1 2 3 4 5 6 7 8 9 10 11 12 13 14  + + + + + + + + + + + + + ;
TRP 105 T=

\ loop-resident registers: 12 loop-carried values + the counter (13 = the
\ full pool) survive a BEGIN/UNTIL back edge via the two-cell packed snapshot
: TLR ( -- n )
   1 2 3 4 5 6 7 8 9 10 11 12  0 begin 1 + dup 3 = until drop
   + + + + + + + + + + + ;
TLR 78 T=

\ locals register cache: repeat refs reuse the cached reg (one ldr total)...
: KL ( n -- n ) {: a:n :} a a + a + ;
5 KL 15 T=
\ ...a call spills and invalidates (the ref after must reload from the frame)
: KC ( n -- n ) {: a:n :} P5 drop a ;
3 KC 3 T=
\ ...and the cache claim survives a BEGIN back edge (loop-resident local)
: KR ( n -- n ) {: a:n :} 0 begin a + dup 15 < 0= until ;
5 KR 15 T=
: KW ( n -- n ) {: a:n :} 0 begin dup 12 < while a + repeat ;
4 KW 12 T=

\ Public parser primitive: runtime and immediate paths both use the native
\ tokenizer and advance the caller's input cursor.
: TPN1 ( -- ) parse-name 5 T= c@ 97 T= ;
TPN1 alpha
: TPNI ( -- ) parse-name 4 T= c@ 98 T= ; immediate
: TPN2 ( -- n ) TPNI beta 7 ;
TPN2 7 T=

\ float VS: d-reg binops (FADD path), dup of a float constant, and a
\ loop-resident float accumulator surviving BEGIN back edges in a d-reg
: TFD ( -- bool ) 2.0 dup f+ 4.0 f= ;
TFD -1 T=
: TFA ( n -- bool ) {: n:n :} 0.0 0 begin 1 + swap 1.5 f+ swap dup n = until drop 6.0 f= ;
4 TFA -1 T=
\ a call spills the float (bits to the memory stack); the prim path finishes
: TF5 ( -- n ) 5 ;
: TFC ( -- bool ) 0.5 TF5 drop 0.5 f+ 1.0 f= ;
TFC -1 T=

\ float pool: deep expression spills past d8..d15 (10 live floats), the other
\ binops, and a float carried through a quotation
: TFS ( -- bool )
   1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 9.0 10.0
   f+ f+ f+ f+ f+ f+ f+ f+ f+ 55.0 f= ;
TFS -1 T=
: TFM ( -- bool ) 3.0 4.0 f* 12.0 f= ;
TFM -1 T=
: TFV ( -- bool ) 10.0 4.0 f/ 2.5 f= ;
TFV -1 T=
: TFQ ( -- bool ) 2.0 [: 3.0 f+ ;] execute 5.0 f= ;
TFQ -1 T=
: TFG ( -- bool ) 5.0 3.0 f> ;
TFG -1 T=

\ FFI: AAPCS64 trampoline runtime proof. Inside a compiled word (so cp@ is the
\ stable free code slot, not a transient top-level line buffer) emit a C-ABI
\ leaf `add x0,x0,x1; ret` at cp@ via patch32, then call it through ffi-call
\ with an 8-cell arg buffer [3,4,..]. Proves x0..x7 marshalling, blr, the x0
\ return, and that XDS (x19) survives the C call. Trusted boundary (raw code +
\ foreign call).
create FFI-ARGS 8 cells allot
: TFFI ( -- n )
   3 FFI-ARGS !  4 FFI-ARGS 8 + !
   cp@ {: fn:n :}
   $8B010000 fn patch32            \ add x0, x0, x1   at fn
   $D65F03C0 fn 4 + patch32        \ ret             at fn+4
   FFI-ARGS fn ffi-call ;
TFFI 7 T=

\ report: count + nonzero exit on failure
: REPORT ( -- )
   #FAIL @ 0 = if [char] o emit [char] k emit cr exit then
   #FAIL @ . s" engine-suite: failures" 1 die ;
REPORT
