0 constant T-CON   1 constant T-VAR   2 constant T-PTR
3 constant S-ROW   4 constant S-PUSH
5 constant T-QUOT
-1 constant UNBOUND
2048 constant MAXTV            \ typevar pool (engine-sized bodies allocate hundreds)
create TVT MAXTV cells allot   create RVT MAXTV cells allot

: TVINIT   \ unbind every type and row var
   0 BEGIN
     dup cells TVT + UNBOUND swap !
     dup cells RVT + UNBOUND swap !
     1 + dup MAXTV 1 - >
   UNTIL drop ;

: TAG 7 and ;

: PAY 3 rshift ;

: MK-CON 3 lshift ;

: MK-VAR 3 lshift T-VAR or ;

: MK-ROW 3 lshift S-ROW or ;

: TV@ cells TVT + @ ;

: TV! cells TVT + ! ;

: RV@ cells RVT + @ ;

: RV! cells RVT + ! ;
256 constant MAXQE             \ quotation effects (din dout rin rout per record)
create QEA MAXQE 32 * allot   variable QEN
: MK-QUOT {: din dout rin rout :}   \ ( -- t ) allocate a quot<effect> term
   QEN @ MAXQE 1 - > IF s" checker: out of quot effects" 76 die THEN
   QEN @ 32 * QEA + {: a :}
   din a !  dout a 8 + !  rin a 16 + !  rout a 24 + !
   QEN @ 3 lshift T-QUOT or  QEN @ 1 + QEN ! ;
: Q>DIN  PAY 32 * QEA + @ ;
: Q>DOUT PAY 32 * QEA + 8 + @ ;
: Q>RIN  PAY 32 * QEA + 16 + @ ;
: Q>ROUT PAY 32 * QEA + 24 + @ ;

4096 constant MAXPUSH          \ push records (engine-sized bodies need hundreds; evaluate's recovery guards grew EM-COMPILE)
create SPA MAXPUSH 16 * allot   variable SPN

: MK-PUSH SPN @ MAXPUSH 1 - > IF s" checker: out of pushes" 76 die THEN
   SPN @ 2 * cells SPA + {: a :} a 8 + ! a ! SPN @ 3 lshift S-PUSH or SPN @ 1 + SPN ! ;

: P>TYPE PAY 2 * cells SPA + @ ;

: P>REST PAY 2 * cells SPA + 8 + @ ;

: ISVAR TAG T-VAR = ;

: ISROW TAG S-ROW = ;

: T-RES BEGIN dup ISVAR IF dup PAY TV@ dup UNBOUND = IF drop 0 ELSE nip -1 THEN ELSE 0 THEN WHILE REPEAT ;

: R-RES BEGIN dup ISROW IF dup PAY RV@ dup UNBOUND = IF drop 0 ELSE nip -1 THEN ELSE 0 THEN WHILE REPEAT ;
4096 constant MAXUWL           \ unify worklist cells (deep spines queue many pairs)
create UWL MAXUWL cells allot   variable USP   variable UOK

: U-PUSH USP @ MAXUWL 1 - > IF s" checker: unify worklist full" 76 die THEN
   USP @ cells UWL + ! USP @ 1 + USP ! ;

: U-POP USP @ 1 - USP ! USP @ cells UWL + @ ;

: PAIR swap U-PUSH U-PUSH ;

: UNPAIR U-POP U-POP swap ;

\ occurs check: binding a row var to a spine containing itself would make the
\ row cyclic — including THROUGH a quotation's effect rows (the ω-combinator
\ must reject, never loop). Recursion depth is bounded by term size; the
\ accumulator rides the stack (a shared variable would be clobbered by the
\ recursive calls).
: ROW-OCC? {: r s :}
   0  s                                  \ ( acc cur )
   BEGIN R-RES dup TAG S-PUSH = WHILE
     dup P>TYPE T-RES dup TAG T-QUOT = IF
       r over Q>DIN RECURSE  swap        \ ( acc cur f1 qt )
       r over Q>DOUT RECURSE  swap       \ ( acc cur f1 f2 qt )
       r over Q>RIN RECURSE  swap        \ ( acc cur f1 f2 f3 qt )
       r swap Q>ROUT RECURSE             \ ( acc cur f1 f2 f3 f4 )
       or or or  rot or swap             \ ( acc' cur )
     ELSE drop THEN
     P>REST
   REPEAT
   r = or ;

4 constant CC-I64   5 constant CC-U8    6 constant CC-U32   7 constant CC-CELL
8 constant CC-CHAR  9 constant CC-STR  10 constant CC-ADDR  11 constant CC-BOOL
12 constant CC-MAX
: INT-FAM? {: code :}  code 1 =  code 3 >  code CC-MAX <  and  or ;   \ n + concretes (not float)
\ CON-OK? ( t1 t2 -- f ) : two concrete cons unify iff equal, or one is the
\ generic int n(1) and the other is int-family (n subsumes any int width).
: CON-OK? {: t1 t2 :}
   t1 PAY t2 PAY = IF -1 EXIT THEN
   t1 PAY 1 = t2 PAY INT-FAM? and IF -1 EXIT THEN
   t2 PAY 1 = t1 PAY INT-FAM? and IF -1 EXIT THEN  0 ;

: U-ROW R-RES swap R-RES swap 2dup = IF 2drop ELSE
   over ISROW IF 2dup ROW-OCC? IF 2drop 0 UOK ! ELSE swap PAY RV! THEN ELSE
   dup ISROW IF 2dup swap ROW-OCC? IF 2drop 0 UOK ! ELSE PAY RV! THEN ELSE
   2dup P>TYPE swap P>TYPE swap PAIR P>REST swap P>REST swap PAIR THEN THEN THEN ;

variable TOCC  variable TODN

\ TY-OCC? ( v t -- f ) : does tyvar v occur in t, descending through quot
\ effect rows? One worklist holds both terms and rows (disjoint tag spaces);
\ TODN counts pending items, the items ride the data stack.
: TY-OCC? {: v t :}
   0 TOCC !  1 TODN !  t
   BEGIN TODN @ 0 > WHILE
     TODN @ 1 - TODN !
     dup TAG S-ROW =  over TAG S-PUSH =  or IF
       R-RES dup TAG S-PUSH = IF
         dup P>TYPE swap P>REST
         TODN @ 2 + TODN !
       ELSE drop THEN
     ELSE
       T-RES
       dup TAG T-VAR = IF PAY v = IF -1 TOCC ! THEN ELSE
       dup TAG T-QUOT = IF
         dup Q>DIN swap  dup Q>DOUT swap  dup Q>RIN swap  Q>ROUT
         TODN @ 4 + TODN !
       ELSE drop THEN THEN
     THEN
   REPEAT
   TOCC @ ;

: U-TYPE   \ ( t1 t2 -- ) resolve both; bind a var side, or require equal cons
   T-RES swap T-RES swap
   2dup = IF 2drop ELSE
   over TAG T-QUOT =  over TAG T-QUOT =  and IF
     2dup Q>DIN swap Q>DIN swap PAIR
     2dup Q>DOUT swap Q>DOUT swap PAIR
     2dup Q>RIN swap Q>RIN swap PAIR
     Q>ROUT swap Q>ROUT swap PAIR ELSE
   over ISVAR IF
     over PAY over TY-OCC? IF 2drop 0 UOK ! ELSE swap PAY TV! THEN ELSE
   dup ISVAR IF
     dup PAY  rot  tuck TY-OCC? IF 2drop 0 UOK ! ELSE swap PAY TV! THEN ELSE
   2dup CON-OK? IF 2drop ELSE 2drop 0 UOK ! THEN THEN THEN THEN THEN ;

: UNIFY   \ ( s1 s2 -- ok ) worklist-driven; rows and types interleave
   0 USP !  -1 UOK !  PAIR
   BEGIN USP @ UOK @ and WHILE
     UNPAIR  over TAG dup S-ROW = swap S-PUSH = or IF U-ROW ELSE U-TYPE THEN
   REPEAT
   UOK @ ;
variable FV

: FRESH FV @ MAXTV 1 - > IF s" checker: out of typevars" 76 die THEN  FV @ dup 1 + FV ! ;
variable OK   variable DCUR   variable UNCK   variable BROW
variable RCUR   variable RBROW

: NEW -1 OK ! 0 UNCK ! 0 SPN ! 0 USP ! TVINIT 0 FV ! 0 QEN !  \ QEN: per-check quot-effect pool, parallel to TVINIT
   FRESH MK-ROW dup BROW ! DCUR !
   FRESH MK-ROW dup RBROW ! RCUR ! ;
variable WAS   variable DEXP   variable DACT   variable FAILSET
variable VSIG   variable SGSEEN   variable SGIN   variable SGOUT
variable SGRIN  variable SGROUT
variable SGA  variable SGU
create FAILTK 64 allot   variable FAILTU
variable TOKIX  variable FAILIX  variable DVERD
variable FAILB  variable FAILE
variable JSON-DIAGS   0 JSON-DIAGS !

: STEP {: din dout :}
   DCUR @ WAS !
   DCUR @ din UNIFY
   dup 0=  FAILSET @ 0=  and  OK @ and  IF din DEXP !  WAS @ DACT !  -1 FAILSET ! THEN
   OK @ and OK !  dout DCUR ! ;

\ --- return row: >r r> r@ transfer types between DCUR and RCUR. A definition
\ must leave the return row exactly as it found it (ANS 3.2.3.3) — the final
\ balance check rejects net growth or borrowing; loop joins unify RCUR too.
: RS->R                                    \ >r : data top -> return row
   FRESH MK-VAR FRESH MK-ROW {: tv rest :}
   DCUR @  tv rest MK-PUSH  UNIFY OK @ and OK !
   rest DCUR !  tv RCUR @ MK-PUSH RCUR ! ;

: RSR>                                     \ r> : return top -> data row
   FRESH MK-VAR FRESH MK-ROW {: tv rest :}
   RCUR @  tv rest MK-PUSH  UNIFY OK @ and OK !
   rest RCUR !  tv DCUR @ MK-PUSH DCUR ! ;

: RSR@                                     \ r@ : peek return top
   FRESH MK-VAR FRESH MK-ROW {: tv rest :}
   RCUR @  tv rest MK-PUSH  UNIFY OK @ and OK !
   tv DCUR @ MK-PUSH DCUR ! ;
variable QTT  variable QD2  variable QR2

: RSEXEC   \ execute: pop the xt; apply its quot effect (or bind a var to one)
   FRESH MK-VAR FRESH MK-ROW {: tv rest :}
   DCUR @  tv rest MK-PUSH  UNIFY OK @ and OK !
   rest DCUR !
   tv T-RES QTT !
   QTT @ TAG T-QUOT = IF
     DCUR @ QTT @ Q>DIN  UNIFY OK @ and OK !
     RCUR @ QTT @ Q>RIN  UNIFY OK @ and OK !
     QTT @ Q>DOUT DCUR !  QTT @ Q>ROUT RCUR !
   ELSE QTT @ TAG T-VAR = IF
     \ unknown xt: bind it to a RETURN-PURE quot over the current state (a
     \ return-impure literal quot then fails to unify at the bind — sound).
     FRESH MK-ROW QD2 !
     DCUR @ QD2 @ RCUR @ RCUR @ MK-QUOT QR2 !
     QTT @ PAY QR2 @ TY-OCC? IF 0 OK ! ELSE
       QR2 @ QTT @ PAY TV!
       QD2 @ DCUR !
     THEN
   ELSE 0 OK ! THEN THEN ;

variable RSH

: RS-TOK? {: a u :}
   -1 RSH !
   a u s" >r" STR= IF RS->R ELSE
   a u s" r>" STR= IF RSR> ELSE
   a u s" r@" STR= IF RSR@ ELSE
   a u s" execute" STR= IF RSEXEC ELSE
   0 RSH ! THEN THEN THEN THEN
   RSH @ ;

\ --- generic signature parser: build a step effect from a textual " in -- out "
\ stack effect. A single lowercase letter is a polymorphic type variable (shared
\ across in/out within one signature); `n` = int (con 1), `f` = flag (con 2);
\ anything else folds to int. Row variable is shared so the effect is row-polymorphic.
create NMAP 26 cells allot

: NMAP-RESET 0 BEGIN dup cells NMAP + UNBOUND swap ! 1 + dup 25 > UNTIL drop ;

: DIGIT? {: c :} c 47 > c 58 < and ;

: LOWER? {: c :} c 96 > c 123 < and ;
variable NRES  variable NDI  variable NDH

: HEXD? {: c :} c DIGIT?  c 96 > c 103 < and or  c 64 > c 71 < and or ;

\ int literal: d+ | -d+ | $h+ | -$h+ (the engine's number tokens)
: ALLDIG? {: a u :}
   0 NDI !  0 NDH !
   u 0 > IF a c@ 45 = IF 1 NDI ! THEN THEN
   u NDI @ > IF a NDI @ + c@ 36 = IF NDI @ 1 + NDI !  1 NDH ! THEN THEN
   u NDI @ - 0 > 0= IF 0 NRES ! ELSE -1 NRES !
     NDI @ BEGIN dup u < WHILE
       NDH @ IF dup a + c@ HEXD? 0= IF 0 NRES ! THEN
       ELSE dup a + c@ DIGIT? 0= IF 0 NRES ! THEN THEN
       1 + REPEAT drop THEN
   NRES @ ;

\ NB: avoid a 2nd {: :} group here — `{: c :} … {: i :}` mis-reads the slot in the
\ standalone, collapsing every var to one. Compute the slot address on the stack.
: VAR-OF {: c :}  c 97 - cells NMAP +  dup @ UNBOUND = IF FRESH over ! THEN  @ MK-VAR ;

\ NB: declare locals at word top, never inside IF/loop (corrupts the locals frame).
\ concrete width types get distinct con codes; n(1)/f(1) stay the GENERIC int
\ (the prim DB and the toolchain's own body use n), and the unifier lets n
\ subsume any int-family code (so '( i64 -- i64 )' over an n-typed prim still
\ checks). r(3)=float. Table-driven to keep the body small (inline-safe).
: CON-OF {: a u :}                      \ multi-char name -> con code, or 0
   a u s" i64"  STR= IF CC-I64  EXIT THEN   a u s" u8"   STR= IF CC-U8   EXIT THEN
   a u s" u32"  STR= IF CC-U32  EXIT THEN   a u s" cell" STR= IF CC-CELL EXIT THEN
   a u s" char" STR= IF CC-CHAR EXIT THEN   a u s" str"  STR= IF CC-STR  EXIT THEN
   a u s" addr" STR= IF CC-ADDR EXIT THEN   a u s" bool" STR= IF CC-BOOL EXIT THEN
   a u s" ptr"  STR= IF CC-ADDR EXIT THEN   0 ;
: TOK-TYPE {: a u :}  a c@ {: c :}
   u 1 = c 110 = and IF 1 MK-CON ELSE          \ 'n' -> generic int (con 1)
   u 1 = c 102 = and IF CC-BOOL MK-CON ELSE     \ 'f' -> bool (a comparison result is a flag, not an int)
   u 1 = c 114 = and IF 3 MK-CON ELSE          \ 'r' -> real/float (con 3)
   a u CON-OF dup IF MK-CON ELSE drop          \ i64/u8/u32/cell/char/str/addr/bool
   u 1 = c LOWER? and IF c VAR-OF ELSE          \ single letter -> type var
   1 MK-CON THEN THEN THEN THEN THEN ;
variable SB variable SL variable SI variable SS
variable PKA  variable PKU  variable PKHAVE          \ one-token push-back

: PK!  PKU !  PKA !  -1 PKHAVE ! ;                   \ ( a u -- )
: PKRESET 0 PKHAVE ! ;
\ NEXT-SIG-TOK ( -- a u ) : next whitespace token over the SB/SL/SI cursor;
\ ( a 0 ) at end. Honors one pushed-back token.
: NEXT-SIG-TOK
   PKHAVE @ IF 0 PKHAVE ! PKA @ PKU @ EXIT THEN
   BEGIN SI @ SL @ < SB @ SI @ + c@ 32 = and WHILE SI @ 1 + SI ! REPEAT
   SI @ SL @ < 0= IF SB @ 0 EXIT THEN
   SB @ SI @ + SS !
   BEGIN SI @ SL @ < SB @ SI @ + c@ 32 <> and WHILE SI @ 1 + SI ! REPEAT
   SS @ SB @ SI @ + SS @ - ;

: UPPER? {: c :} c 64 > c 91 < and ;
: ROW-LEAD? {: a u :} u 1 = a c@ UPPER? and ;        \ a single upper letter leads a row
: DELIM? {: a u :}                                   \ stack terminator
   u 0 = IF -1 EXIT THEN
   a u s" --" STR= IF -1 EXIT THEN
   a u s" ]"  STR= IF -1 EXIT THEN
   a u s" |"  STR= ;

create ROWMAP 26 cells allot
: ROWMAP-RESET 0 BEGIN dup cells ROWMAP + UNBOUND swap ! 1 + dup 25 > UNTIL drop ;
: RVAR-OF {: c :}  c 65 - cells ROWMAP +  dup @ UNBOUND = IF FRESH over ! THEN  @ MK-ROW ;

\ SGBAD: the declared signature is malformed (a required '--'/']' delimiter was
\ missing or wrong). A malformed contract must REJECT, never silently parse as
\ some other effect. EXPECT-SIG consumes the next sig token and fails closed if
\ it is not the expected delimiter (EOF reads as a 0-length token -> mismatch).
variable SGBAD
variable UNSAFE
: EXPECT-SIG {: ea eu :}  NEXT-SIG-TOK ea eu STR= 0= IF -1 SGBAD ! THEN ;

\ PSTACK ( tail -- row ) : parse one stack onto a tail row. A leading single
\ upper-case token names the row (shared by letter); else the passed implicit
\ tail is used. Types fold bottom->top; '[' in -- out ']' is a quot<effect>
\ (RECURSE for the nested stacks; rin=rout=one fresh row -> no return effect).
\ tail is a LOCAL so it survives RECURSE; the data stack holds only the row.
: PSTACK {: tail :}
   NEXT-SIG-TOK 2dup ROW-LEAD? IF
      drop c@ RVAR-OF                                 \ row = named var
   ELSE PK! tail THEN                                 \ push back token; row = tail
   BEGIN
     NEXT-SIG-TOK 2dup DELIM? IF PK! EXIT THEN        \ ( row a u )->PK!->( row ), return
     2dup s" [" STR= IF
        2drop
        FRESH MK-ROW                                  \ the quot's shared data row
        dup RECURSE                                   \ quot in-stack (on qrow)
        s" --" EXPECT-SIG                             \ require '--'
        swap RECURSE                                  \ quot out-stack (on qrow)
        s" ]" EXPECT-SIG                              \ require ']'
        FRESH MK-ROW dup MK-QUOT                      \ rin = rout (no return effect)
        swap MK-PUSH
     ELSE
        TOK-TYPE  swap MK-PUSH
     THEN
   AGAIN ;

variable SGHASR                          \ a return-stack clause ( ... | rin -- rout ) present?
variable RR-SHARED                       \ the shared return row, allocated lazily on '|'
variable PD-IN variable PR-IN variable PD-OUT variable PR-OUT

: RRTAIL ( -- rrow )                     \ the shared return row (allocate once, on demand)
   RR-SHARED @ dup 0= IF drop FRESH MK-ROW dup RR-SHARED ! THEN ;

\ PSIDE ( dtail -- drow rrow ) : one side = data stack [ '|' return stack ]. No
\ '|' -> rrow = the shared return row so far (0 if no clause anywhere) — CHECK
\ ignores it. The return row is allocated only when a '|' actually appears, so
\ ordinary sigs cost no extra typevars.
: PSIDE {: dtail :}
   dtail PSTACK                                   \ data part (stops at | -- ])
   NEXT-SIG-TOK 2dup s" |" STR= IF
      2drop  -1 SGHASR !  RRTAIL PSTACK           \ ( drow rrow ) explicit return
   ELSE PK! RR-SHARED @ THEN ;                    \ no | here -> shared tail (untouched)

\ PSIG ( -- din dout rin rout ) : data + return rows over the cursor.
: PSIG
   PKRESET NMAP-RESET ROWMAP-RESET  0 SGHASR !  0 RR-SHARED !
   FRESH MK-ROW {: dr :}
   dr PSIDE  PR-IN ! PD-IN !
   s" --" EXPECT-SIG                              \ require the top-level '--'
   dr PSIDE  PR-OUT ! PD-OUT !
   PD-IN @ PD-OUT @ PR-IN @ PR-OUT @ ;

: PARSE-SIG {: a u :}      a SB ! u SL ! 0 SI !  PSIG 2drop STEP ;

\ PARSE-SIG-RAW ( a u -- din dout rin rout ) : the declared effect as four rows
\ (no STEP), for verifying a definition's body against its own ( in -- out ).
: PARSE-SIG-RAW {: a u :}  a SB ! u SL ! 0 SI !  PSIG ;

\ --- prim table: name/sig pairs [nlen][name][slen][sig]...[0], scanned by FIND-SIG.
\ A data table (not a 26-branch word) because the standalone INLINES colon-word
\ bodies, so a dispatch word with many PARSE-SIG calls overflows. DO-TOK stays small.
\ prim sig table: records [nlen][name][slen][sig], 0-terminated — built from
\ readable strings (PT+ keeps the terminator as it appends).
3072 constant PTAB-CAP
create PTAB PTAB-CAP allot  variable PTP
create SDQN 2 allot  115 SDQN c!  34 SDQN 1 + c!     \ the two chars of `s"`

: PT2+ {: a u :}
   PTP @ u + 2 +  PTAB PTAB-CAP 2 - +  > IF s" checker: prim table full" 76 die THEN
   u PTP @ c!
   0 BEGIN dup u < WHILE  dup a + c@  over PTP @ + 1 + c!  1 + REPEAT drop
   PTP @ 1 + u + PTP !  0 PTP @ c! ;

: PT+ {: na nu sa su :}  na nu PT2+  sa su PT2+ ;

: PTABLE  PTAB PTP !  0 PTAB c!
   s" dup" s" a -- a a" PT+
   s" drop" s" a --" PT+
   s" swap" s" a b -- b a" PT+
   s" over" s" a b -- a b a" PT+
   s" nip" s" a b -- b" PT+
   s" tuck" s" a b -- b a b" PT+
   s" rot" s" a b c -- b c a" PT+
   s" -rot" s" a b c -- c a b" PT+
   s" 2dup" s" a b -- a b a b" PT+
   s" 2drop" s" a b --" PT+
   s" +" s" n n -- n" PT+
   s" -" s" n n -- n" PT+
   s" *" s" n n -- n" PT+
   s" and" s" n n -- n" PT+
   s" or" s" n n -- n" PT+
   s" xor" s" n n -- n" PT+
   s" 1+" s" n -- n" PT+
   s" 1-" s" n -- n" PT+
   s" negate" s" n -- n" PT+
   s" invert" s" n -- n" PT+
   s" 0=" s" n -- f" PT+
   s" 0<" s" n -- f" PT+
   s" =" s" n n -- f" PT+
   s" <" s" n n -- f" PT+
   s" >" s" n n -- f" PT+
   s" <>" s" n n -- f" PT+
   s" <=" s" n n -- f" PT+
   s" >=" s" n n -- f" PT+
   s" /" s" n n -- n" PT+
   s" mod" s" n n -- n" PT+
   s" lshift" s" n n -- n" PT+
   s" rshift" s" n n -- n" PT+
   s" cells" s" n -- n" PT+
   s" @" s" n -- n" PT+
   s" !" s" a n --" PT+
   s" c@" s" n -- n" PT+
   s" c!" s" a n --" PT+
   s" ." s" n --" PT+
   s" .s" s" --" PT+
   s" here" s" -- n" PT+
   s" allot" s" n --" PT+
   s" ," s" n --" PT+
   s" c," s" n --" PT+
   s" type" s" n n --" PT+
   s" throw" s" n --" PT+
   s" die" s" n n n --" PT+
   s" open" s" n n n -- n" PT+
   s" read" s" n n n -- n" PT+
   s" ioctl" s" n n n -- n" PT+
   s" open-rd" s" n -- n" PT+
   s" access" s" n n -- n" PT+
   s" stat64" s" n n -- n" PT+
   s" getdirentries64" s" n n n n -- n" PT+
   s" pipe" s" -- n n n" PT+
   s" dup2" s" n n -- n" PT+
   s" fcntl" s" n n n -- n" PT+
   s" poll" s" n n n -- n" PT+
   s" spawn-io" s" n n n n -- n" PT+
   s" wait-rc" s" n -- n" PT+
   s" patch32" s" n n --" PT+
   s" write" s" n n n -- n" PT+
   s" close" s" n --" PT+
   s" epoch-seconds" s" -- n" PT+
   s" mono-ns" s" -- n" PT+
   s" rbase" s" -- n" PT+
   s" wordlist" s" -- n" PT+
   s" get-current" s" -- n" PT+
   s" set-current" s" n --" PT+
   \ floats: r = real (concrete), distinct from n (int) and f (flag)
   s" f+" s" r r -- r" PT+    s" f-" s" r r -- r" PT+
   s" f*" s" r r -- r" PT+    s" f/" s" r r -- r" PT+
   s" fnegate" s" r -- r" PT+  s" fabs" s" r -- r" PT+  s" fsqrt" s" r -- r" PT+
   s" f<" s" r r -- f" PT+    s" f>" s" r r -- f" PT+   s" f=" s" r r -- f" PT+
   s" f0<" s" r -- f" PT+     s" f0=" s" r -- f" PT+
   s" s>f" s" n -- r" PT+     s" f>s" s" r -- n" PT+    s" f." s" r --" PT+
   \ s" pushes addr+len; ['] pushes an xt. The engine consumes their payload
   \ inline, so only the bare token reaches the body capture.
   SDQN 2 PT2+  s" -- n n" PT2+
   s" [']" s" -- n" PT+
   s" [char]" s" -- n" PT+
   s" emit" s" n --" PT+
   s" cr" s" --" PT+
   s" space" s" --" PT+
   s" u." s" n --" PT+
   \ defining-word kinds (the engine hooks "NAME create" etc. so the name gets
   \ recorded): create/variable are addresses; a constant's cell is untyped.
   s" create" s" -- n" PT+
   s" variable" s" -- n" PT+
   s" constant" s" -- a" PT+ ;
PTABLE
variable FSA  variable FSU  variable FNL  variable FNP  variable FSL  variable FSP  variable FP
\ user sigs: certified words recorded as [len|name|len|sig]*, 0-terminated.
\ Appended by the renderer (RECXT hook); scanned after PTAB so later wins.
create USIGS 32768 allot   0 USIGS c!   variable UEND   0 UEND !

: UB! {: c :}  c USIGS UEND @ + c!  UEND @ 1 + UEND ! ;

: UBS {: a u :}  0 BEGIN dup u < WHILE  dup a + c@ UB!  1 + REPEAT drop ;

: USIG-ADD {: sa su na nu :}
   UEND @ nu + su + 3 + 32766 > IF s" checker: user sigs full" 76 die THEN
   nu UB!  na nu UBS  su UB!  sa su UBS  0 USIGS UEND @ + c! ;

: SCAN-SIGS {: tab a u :}  tab FP !
   BEGIN FP @ c@ dup WHILE                       \ no locals inside the loop (corrupts frame)
     FNL !  FP @ 1 + FNP !
     FNP @ FNL @ + dup c@ FSL ! 1 + FSP !
     a u FNP @ FNL @ STR= IF FSP @ FSA ! FSL @ FSU ! THEN
     FSP @ FSL @ + FP !
   REPEAT drop ;

: FIND-SIG {: a u :}  0 FSU !  PTAB a u SCAN-SIGS  USIGS a u SCAN-SIGS  FSU @ ;
variable FLD  variable FLI  variable FLO  variable FLC

: FLODIG? {: a u :}                        \ -?d+.d+ (one interior dot) -> float literal
   0 FLD !  0 FLI !  -1 FLO !
   u 3 < IF 0 FLO ! THEN
   a c@ 45 = IF 1 FLI ! THEN
   FLI @ BEGIN dup u < WHILE
     a over + c@ FLC !
     FLC @ 46 = IF FLD @ 0 > IF 0 FLO ! THEN FLD @ 1 + FLD !
     ELSE FLC @ 47 > FLC @ 58 < and 0= IF 0 FLO ! THEN THEN
     1 + REPEAT drop
   FLD @ 1 = FLO @ and
   u 0 > IF a u 1 - + c@ 46 = IF drop 0 THEN THEN
   a FLI @ + c@ 46 = IF drop 0 THEN ;

: DO-TOK {: a u :}
   a u FIND-SIG IF FSA @ FSU @ PARSE-SIG ELSE
   a u ALLDIG? IF s" -- n" PARSE-SIG ELSE
   a u FLODIG? IF s" -- r" PARSE-SIG ELSE -1 UNCK ! THEN THEN THEN ;

\ --- locals: {: a b :} pops and binds names to type vars; a reference pushes
\ its binding. Groups accumulate (a later group binds only its own names).
: CCOPY {: a d u :}  0 BEGIN dup u < WHILE  dup a + c@  over d + c!  1 + REPEAT drop ;
create LOCNB 1024 allot   create LOCLN 64 cells allot   create LOCTV 64 cells allot
variable #LOC  variable LMODE  variable LGRP  variable LROW  variable LCH  variable LI  variable LRF

variable LCO

: LCOLON {: a u :}   \ ( a u -- ) LCO = index of the first ':' in a/u, or u
   u LCO !
   0 BEGIN  dup u <  LCO @ u =  and WHILE
     dup a + c@ 58 = IF dup LCO ! THEN
     1 + REPEAT drop ;

\ a typed local `a:n` stores the BARE name (matching the engine) and unifies
\ the local's type var with the asserted type — a wrong use then rejects.
: LOC-ADD {: a u :}
   a u LCOLON
   #LOC @ 63 >  LCO @ 16 >  or IF -1 UNCK ! ELSE
     a  LOCNB #LOC @ 16 * +  LCO @ CCOPY
     LCO @ #LOC @ cells LOCLN + !
     FRESH MK-VAR #LOC @ cells LOCTV + !
     LCO @ u < IF
       a LCO @ + 1 +  u LCO @ - 1 -  TOK-TYPE
       #LOC @ cells LOCTV + @  UNIFY OK @ and OK !
     THEN
     #LOC @ 1 + #LOC ! THEN ;

: LOC-BIND
   FRESH dup LROW !  MK-ROW LCH !
   LGRP @ BEGIN dup #LOC @ < WHILE
     dup cells LOCTV + @  LCH @ MK-PUSH LCH !
     1 + REPEAT drop
   LCH @  LROW @ MK-ROW  STEP ;

: LOC-TOK {: a u :}
   a u s" :}" STR= IF 0 LMODE ! LOC-BIND ELSE
   a u s" --" STR= IF -1 UNCK ! ELSE
   a u LOC-ADD THEN THEN ;

: LOC-REF? {: a u :}
   0 LRF !  #LOC @ LI !
   BEGIN LI @ 0 >  LRF @ 0=  and WHILE
     LI @ 1 - LI !
     a u  LOCNB LI @ 16 * +  LI @ cells LOCLN + @  STR= IF
       LI @ cells LOCTV + @  DCUR @ MK-PUSH DCUR !  -1 LRF ! THEN
   REPEAT  LRF @ ;
\ --- control flow: branch states saved on a CF stack and unified at joins.
\ Both rows are snapshot: A/B = data, RA/RB = return (PLAN: net growth on
\ either row at a back edge is a row-occurs failure).
\ kinds: 1 if  2 if+else  3 begin  4 begin+while  5 do  6 quotation
create CFKND 32 cells allot   create CFSA 32 cells allot   create CFSB 32 cells allot
create CFRA 32 cells allot    create CFRB 32 cells allot   create CFDED 32 cells allot
\ exit-accumulator save slots: a [: ;] quotation is a nested scope, so its early
\ returns must NOT leak into the enclosing word's accumulator (CF-QUOT saves,
\ CF-SEMIQ folds the quote's own exits then restores).
create CFXRO 32 cells allot   create CFXRR 32 cells allot
create CFXST 32 cells allot    create CFXDP 32 cells allot
variable #CFC  variable CTMP  variable RTMP  variable CFH  variable INDO
\ EXIT: an early return. XROW accumulates the data row at each exit (all returns,
\ incl. the fall-through at ';', must unify). DEADP marks the current linear path
\ terminated by exit, so the enclosing THEN excludes it from the branch join.
\ CFDED[i] saves the if-branch's deadness across CF-ELSE. (leave targets the
\ enclosing DO frame's loop-exit row; unloop is a typing no-op — loop control
\ isn't on the typed rows.)
variable XROW  variable XRROW  variable XSET  variable DEADP
variable RSHAS  variable RSGIN  variable RSGOUT  variable RSGRIN  variable RSGROUT
variable RHAS   variable RDIN   variable RDOUT   variable RRIN    variable RROUT
: CF@DED #CFC @ 1 - cells CFDED + @ ;

: CF-PUSH {: k s0 s1 r0 r1 :}
   #CFC @ 31 > IF -1 UNCK ! ELSE
     k #CFC @ cells CFKND + !  s0 #CFC @ cells CFSA + !  s1 #CFC @ cells CFSB + !
     r0 #CFC @ cells CFRA + !  r1 #CFC @ cells CFRB + !
     #CFC @ 1 + #CFC ! THEN ;

: CF@K #CFC @ 1 - cells CFKND + @ ;

: CF@A #CFC @ 1 - cells CFSA + @ ;

: CF@B #CFC @ 1 - cells CFSB + @ ;

: CF@RA #CFC @ 1 - cells CFRA + @ ;

: CF@RB #CFC @ 1 - cells CFRB + @ ;

: CF-DROP #CFC @ 1 - #CFC ! ;

: CF-MT? #CFC @ 0 > 0= ;

: SUNI {: s :}
   DCUR @ s UNIFY
   dup 0=  FAILSET @ 0=  and  OK @ and  IF s DEXP !  DCUR @ DACT !  -1 FAILSET ! THEN
   OK @ and OK ! ;

: RSUNI {: s :}  RCUR @ s UNIFY OK @ and OK ! ;

: SG-SAVE
   SGHASR @ RSHAS !  SGIN @ RSGIN !  SGOUT @ RSGOUT !
   SGRIN @ RSGRIN !  SGROUT @ RSGROUT ! ;
: SG-REST
   RSHAS @ SGHASR !  RSGIN @ SGIN !  RSGOUT @ SGOUT !
   RSGRIN @ SGRIN !  RSGROUT @ SGROUT ! ;
: CF-RECURSE
   VSIG @ SGSEEN @ and IF
      SG-SAVE
      SGA @ SGU @ PARSE-SIG-RAW
      SGHASR @ RHAS !
      RROUT !  RRIN !  RDOUT !  RDIN !
      SG-REST
      RDIN @ SUNI  RDOUT @ DCUR !
      RHAS @ IF RRIN @ RSUNI  RROUT @ RCUR ! THEN
   ELSE -1 UNCK ! THEN ;

: CF-IF  s" bool --" PARSE-SIG  1 DCUR @ 0 RCUR @ 0 CF-PUSH ;   \ IF consumes a flag, not any value

: CF-ELSE
   CF-MT? IF -1 UNCK ! ELSE CF@K 1 <> IF -1 UNCK ! ELSE
     DEADP @ #CFC @ 1 - cells CFDED + !  0 DEADP !       \ save if-branch deadness; else runs live
     DCUR @ CTMP !  CF@A DCUR !
     RCUR @ RTMP !  CF@RA RCUR !
     2 #CFC @ 1 - cells CFKND + !
     CTMP @ #CFC @ 1 - cells CFSB + !
     RTMP @ #CFC @ 1 - cells CFRB + !
   THEN THEN ;

: CF-THEN
   CF-MT? IF -1 UNCK ! ELSE
     CF@K 1 = IF                                          \ IF ... THEN (no else)
        DEADP @ IF CF@A DCUR !  CF@RA RCUR !  0 DEADP !   \ if-branch exited: take fall-through
        ELSE CF@A SUNI  CF@RA RSUNI THEN  CF-DROP
     ELSE CF@K 2 = IF                                     \ IF ... ELSE ... THEN
        DEADP @  CF@DED                                   \ ( else-dead if-dead )
        2dup and IF 2drop -1 DEADP !                      \ both exited -> path stays dead
        ELSE over IF 2drop CF@B DCUR ! CF@RB RCUR ! 0 DEADP !  \ else exited -> take if-branch
        ELSE nip IF 0 DEADP !                             \ if exited -> keep else (in DCUR)
        ELSE CF@B SUNI CF@RB RSUNI 0 DEADP ! THEN THEN THEN
        CF-DROP
     ELSE -1 UNCK ! THEN THEN THEN ;

: CF-EXIT                                                 \ early return: accumulate, kill path
   XSET @ IF  DCUR @ XROW @ UNIFY OK @ and OK !
              RCUR @ XRROW @ UNIFY OK @ and OK !
   ELSE  DCUR @ XROW !  RCUR @ XRROW !  -1 XSET ! THEN
   -1 DEADP ! ;
: CF-UNLOOP ;                                             \ loop control isn't typed -> no-op

: CF-BEGIN  3 DCUR @ 0 RCUR @ 0 CF-PUSH ;

: CF-UNTIL
   s" bool --" PARSE-SIG
   CF-MT? IF -1 UNCK ! ELSE CF@K 3 <> IF -1 UNCK ! ELSE
     CF@A SUNI  CF@A DCUR !  CF@RA RSUNI  CF@RA RCUR !  CF-DROP THEN THEN ;

: CF-AGAIN                              \ unconditional loop: code after AGAIN is unreachable
   CF-MT? IF -1 UNCK ! ELSE CF@K 3 <> IF -1 UNCK ! ELSE
     CF@A SUNI  CF@A DCUR !  CF@RA RSUNI  CF@RA RCUR !  CF-DROP  -1 DEADP ! THEN THEN ;

: CF-WHILE
   s" bool --" PARSE-SIG
   CF-MT? IF -1 UNCK ! ELSE CF@K 3 <> IF -1 UNCK ! ELSE
     4 #CFC @ 1 - cells CFKND + !
     DCUR @ #CFC @ 1 - cells CFSB + !
     RCUR @ #CFC @ 1 - cells CFRB + !
   THEN THEN ;

: CF-REPEAT
   CF-MT? IF -1 UNCK ! ELSE CF@K 4 <> IF -1 UNCK ! ELSE
     CF@A SUNI  CF@B DCUR !  CF@RA RSUNI  CF@RB RCUR !  CF-DROP THEN THEN ;

: CF-DO  s" n n --" PARSE-SIG  5 DCUR @ 0 RCUR @ 0 CF-PUSH ;

\ At LOOP the exit is always live: ?do/do terminates, and a `leave` jumps here.
\ If the body fall-through is dead (unconditional leave/exit), the back-edge is
\ never taken — skip the body-vs-DO-point unify, but the loop-exit row is still
\ the DO-point row (a zero-trip ?do or a leave both leave exactly that). Live
\ fall-through: the back edge requires a stack-neutral body (CF@A SUNI).
: CF-LOOP
   CF-MT? IF -1 UNCK ! ELSE CF@K 5 <> IF -1 UNCK ! ELSE
     DEADP @ IF  0 DEADP !
     ELSE  CF@A SUNI  CF@RA RSUNI  THEN
     CF@A DCUR !  CF@RA RCUR !  CF-DROP THEN THEN ;

: CF-+LOOP
   s" n --" PARSE-SIG
   CF-MT? IF -1 UNCK ! ELSE CF@K 5 <> IF -1 UNCK ! ELSE
     DEADP @ IF  0 DEADP !
     ELSE  CF@A SUNI  CF@RA RSUNI  THEN
     CF@A DCUR !  CF@RA RCUR !  CF-DROP THEN THEN ;

: CF-I
   0 INDO !  0 BEGIN dup #CFC @ < WHILE
     dup cells CFKND + @ 5 = IF -1 INDO ! THEN  1 + REPEAT drop
   INDO @ IF s" -- n" PARSE-SIG ELSE -1 UNCK ! THEN ;

: CF-J                                     \ needs two enclosing DO frames
   0 INDO !  0 BEGIN dup #CFC @ < WHILE
     dup cells CFKND + @ 5 = IF INDO @ 1 + INDO ! THEN  1 + REPEAT drop
   INDO @ 1 > IF s" -- n" PARSE-SIG ELSE -1 UNCK ! THEN ;

variable LVDO  variable LVDN
\ CF-FINDDO ( -- ) : LVDO = index of the nearest enclosing DO frame, or -1.
\ Scans top-down and stops at the first DO (kind 5) or quotation boundary
\ (kind 6) — a `leave` inside [: ;] does not escape to an outer loop.
: CF-FINDDO
   -1 LVDO !  0 LVDN !
   #CFC @ 1 -
   BEGIN dup 0 >= LVDN @ 0= and WHILE
     dup cells CFKND + @ 5 = IF dup LVDO !  -1 LVDN ! THEN
     dup cells CFKND + @ 6 = IF -1 LVDN ! THEN
     1 - REPEAT drop ;

\ CF-LEAVE : early loop exit. The stack at `leave` must match the loop-exit row
\ (= the DO-point row CFSA, since the body is stack-neutral); likewise the return
\ row. Then the path to `loop` is dead (CF-LOOP revives the live loop exit).
: CF-LEAVE
   CF-FINDDO
   LVDO @ 0< IF -1 UNCK ! ELSE
     LVDO @ cells CFSA + @ SUNI
     LVDO @ cells CFRA + @ RSUNI
     -1 DEADP ! THEN ;

: CF-QUOT   \ [: — pause the outer inference (incl. its exit state), open a nested one
   6  DCUR @  BROW @  RCUR @  RBROW @  CF-PUSH
   XROW @ #CFC @ 1 - cells CFXRO + !  XRROW @ #CFC @ 1 - cells CFXRR + !
   XSET @ #CFC @ 1 - cells CFXST + !  DEADP @ #CFC @ 1 - cells CFXDP + !
   0 XSET !  0 DEADP !
   FRESH MK-ROW dup BROW ! DCUR !
   FRESH MK-ROW dup RBROW ! RCUR ! ;

variable QTMP

: CF-SEMIQ  \ ;] — quot<nested effect> pushed onto the restored outer row
   CF-MT? IF -1 UNCK ! ELSE CF@K 6 <> IF -1 UNCK ! ELSE
     XSET @ IF                                   \ fold the quote's OWN early returns into its effect
       DEADP @ IF XROW @ DCUR !  XRROW @ RCUR !
       ELSE DCUR @ XROW @ UNIFY OK @ and OK !  RCUR @ XRROW @ UNIFY OK @ and OK ! THEN
     THEN
     BROW @  DCUR @  RBROW @  RCUR @  MK-QUOT QTMP !
     #CFC @ 1 - cells CFXRO + @ XROW !  #CFC @ 1 - cells CFXRR + @ XRROW !
     #CFC @ 1 - cells CFXST + @ XSET !  #CFC @ 1 - cells CFXDP + @ DEADP !  \ restore outer exit state
     CF@B BROW !  CF@RB RBROW !
     CF@RA RCUR !
     QTMP @  CF@A  MK-PUSH DCUR !
     CF-DROP THEN THEN ;

: CF-TOK? {: a u :}
   -1 CFH !
   a u s" [:" STR= IF CF-QUOT ELSE
   a u s" ;]" STR= IF CF-SEMIQ ELSE
   a u s" if" STR= IF CF-IF ELSE
   a u s" else" STR= IF CF-ELSE ELSE
   a u s" then" STR= IF CF-THEN ELSE
   a u s" begin" STR= IF CF-BEGIN ELSE
   a u s" until" STR= IF CF-UNTIL ELSE
   a u s" again" STR= IF CF-AGAIN ELSE
   a u s" while" STR= IF CF-WHILE ELSE
   a u s" repeat" STR= IF CF-REPEAT ELSE
   a u s" do" STR= IF CF-DO ELSE
   a u s" ?do" STR= IF CF-DO ELSE
   a u s" loop" STR= IF CF-LOOP ELSE
   a u s" +loop" STR= IF CF-+LOOP ELSE
   a u s" i" STR= IF CF-I ELSE
   a u s" j" STR= IF CF-J ELSE
   a u s" exit" STR= IF CF-EXIT ELSE
   a u s" leave" STR= IF CF-LEAVE ELSE
   a u s" unloop" STR= IF CF-UNLOOP ELSE
   a u s" recurse" STR= IF CF-RECURSE ELSE
   0 CFH ! THEN THEN THEN THEN THEN THEN THEN THEN THEN THEN THEN THEN THEN THEN THEN THEN THEN THEN THEN THEN
   CFH @ ;
variable TBASE variable TBLEN variable TI variable TSTART
\ first token of the checked text is the word's NAME (skipped, kept for the
\ recorder); RECXT (installed by render.f) records certified sigs by name.
variable NMA  variable NMU  variable TOK0  variable RECXT  0 RECXT !
variable DIAGXT  0 DIAGXT !              \ reject-diagnostic hook (render.f installs)
\ the engine folds A-Z in keyword and dict matching — fold every token the same
\ way (into a scratch copy: the source text may live in the read-only image).
create TKF 64 allot   create NMB 64 allot   variable TFU

: TOKFOLD {: a u :}
   u 64 > IF 0 ELSE
     0 BEGIN dup u < WHILE
       dup a + c@  dup 64 >  over 91 <  and IF 32 or THEN
       over TKF + c!  1 +
     REPEAT drop  u TFU !  -1 THEN ;
: FAIL-SPAN! ( -- )
   TSTART @ TBASE @ - FAILB !
   FAILB @ TFU @ + FAILE ! ;
: CAP-FAIL ( -- )
   FAILSET @ 0= IF
      TKF FAILTK TFU @ CCOPY  TFU @ FAILTU !  TOKIX @ FAILIX !  FAIL-SPAN!
   THEN ;
: CAP-LONG {: a u :}
   FAILSET @ 0= IF
      a FAILTK u CCOPY  u FAILTU !  TOKIX @ FAILIX !
      TSTART @ TBASE @ - FAILB !  FAILB @ u + FAILE !
   THEN ;

create DIAGFB 256 allot   variable DIAGFU
variable DIAGL0  variable DIAGC0  variable DIAGB0
: DIAG-FILE! {: a u :}
   u 255 > IF s" diag: file path too long" 76 die THEN
   0 BEGIN dup u < WHILE
      dup a + c@  over DIAGFB + c!
      1 +
   REPEAT drop
   u DIAGFU ! ;
: DIAG-ORIGIN! {: line col byte :}
   line DIAGL0 !  col DIAGC0 !  byte DIAGB0 ! ;
s" <input>" DIAG-FILE!
1 1 0 DIAG-ORIGIN!

\ TRUST: declare a word's effect without checking its body — the native escape
\ hatch (PLAN's TRUSTED:). Callers are checked against the declared sig.
\ Usage:  s" myword" s" n n -- n" trust
: TRUST {: na nu sa su :}
   na nu TOKFOLD 0= IF s" trust: name too long" 76 die THEN
   sa su  TKF TFU @  USIG-ADD ;

: UNSAFE-TOK? {: a u :}
   a u s" evaluate" STR= IF -1 EXIT THEN
   a u s" postpone" STR= IF -1 EXIT THEN
   a u s" compile," STR= IF -1 EXIT THEN
   a u s" immediate" STR= IF -1 EXIT THEN
   a u s" [" STR= IF -1 EXIT THEN
   a u s" ]" STR= ;

: REJECT-UNSAFE ( -- )
   -1 UNSAFE !  0 OK !  -1 FAILSET ! ;

: DO-TOK1 {: a u :}
   a u TOKFOLD 0= IF s" <too-long-token>" CAP-LONG  -1 UNCK !  -1 FAILSET ! ELSE
   CAP-FAIL
   TOK0 @ IF TKF NMB TFU @ CCOPY  NMB NMA !  TFU @ NMU !  0 TOK0 ! ELSE
   LMODE @ IF TKF TFU @ LOC-TOK ELSE
   TKF TFU @ s" {:" STR= IF 1 LMODE !  #LOC @ LGRP ! ELSE
   TKF TFU @ UNSAFE-TOK? IF REJECT-UNSAFE ELSE
   TKF TFU @ CF-TOK? 0= IF
   TKF TFU @ RS-TOK? 0= IF
   TKF TFU @ LOC-REF? 0= IF
   TKF TFU @ DO-TOK THEN THEN THEN THEN THEN THEN THEN
   OK @ 0=  FAILSET @ 0=  and IF -1 FAILSET ! THEN
   UNCK @  FAILSET @ 0=  and IF -1 FAILSET ! THEN
   THEN
   TOKIX @ 1 + TOKIX ! ;

: CHECK {: a u :}   \ ( a u -- -1=certified | 0=rejected | 1=uncheckable )
   a TBASE !  u TBLEN !  NEW
   0 TI !  1 TOK0 !  0 NMU !  0 #LOC !  0 LMODE !  0 #CFC !
   0 FAILSET !  0 DEXP !  0 DACT !  0 FAILTU !  0 SGSEEN !  0 SGHASR !
   0 SGIN !  0 SGOUT !  0 SGRIN !  0 SGROUT !  0 SGA !  0 SGU !
   0 TOKIX !  0 FAILIX !  0 DVERD !
   0 FAILB !  0 FAILE !  0 XSET !  0 DEADP !  0 SGBAD !  0 UNSAFE !
   BEGIN TI @ TBLEN @ < WHILE
     BEGIN TI @ TBLEN @ <  TBASE @ TI @ + c@ 32 =  and WHILE TI @ 1 + TI ! REPEAT
     TI @ TBLEN @ < IF
       TBASE @ TI @ + c@ 40 =  TBASE @ TI @ + 1 + c@ 32 =  and IF   \ '( ' (not '(CMP)') -> sig
         TI @ 1 + TI !  TI @ TSTART !             \ sig text starts after '('
         BEGIN TI @ TBLEN @ <  TBASE @ TI @ + c@ 41 <>  and WHILE TI @ 1 + TI ! REPEAT
         VSIG @ IF
           TBASE @ TSTART @ + SGA !  TI @ TSTART @ - SGU !
           TBASE @ TSTART @ +  TI @ TSTART @ -  PARSE-SIG-RAW   \ ( din dout rin rout )
           SGHASR @ IF
             SGROUT !  dup SGRIN !  RCUR !  SGOUT !  dup SGIN !  DCUR !
           ELSE
             2drop  SGOUT !  dup SGIN !  DCUR !
           THEN  -1 SGSEEN !
         THEN
         TI @ TBLEN @ < IF TI @ 1 + TI ! THEN     \ skip ')'
       ELSE
         TBASE @ TI @ + TSTART !
         BEGIN TI @ TBLEN @ <  TBASE @ TI @ + c@ 32 <>  and WHILE TI @ 1 + TI ! REPEAT
         TSTART @  TBASE @ TI @ +  TSTART @ -  DO-TOK1
       THEN
     THEN
   REPEAT
   XSET @ IF                                         \ fold early-return states into the output
     DEADP @ IF XROW @ DCUR !  XRROW @ RCUR !         \ every path exited: output = accumulator
     ELSE DCUR @ XROW @ UNIFY OK @ and OK !  RCUR @ XRROW @ UNIFY OK @ and OK ! THEN
   THEN
   VSIG @ SGSEEN @ and IF
      SGOUT @ SUNI
      OK @ IF SGIN @ BROW !  SGOUT @ DCUR ! THEN    \ record the verified declared effect
   THEN                                        \ SUNI captures declared(exp)/inferred(act)
   LMODE @ 0 <>  #CFC @ 0 <>  or IF -1 UNCK ! THEN
   SGHASR @ 0= IF RCUR @ R-RES  RBROW @ R-RES  <> IF 0 OK ! THEN THEN   \ balance (no clause)
   VSIG @ SGSEEN @ SGHASR @ and and IF
      RCUR @ SGROUT @ UNIFY OK @ and OK !
      OK @ IF SGRIN @ RBROW !  SGROUT @ RCUR ! THEN
   THEN
   SGBAD @ UNSAFE @ or IF 0 ELSE UNCK @ IF 1 ELSE OK @ THEN THEN   \ malformed/unsafe rejects
   dup DVERD !
   dup 0 =  over 1 = JSON-DIAGS @ and  or
   DIAGXT @ 0 <> and IF DIAGXT @ execute THEN
   dup -1 = NMU @ 0 > and IF
      VSIG @ SGSEEN @ and IF
         SGA @ SGU @  NMA @ NMU @  USIG-ADD
      ELSE
         RECXT @ 0 <> IF NMA @ NMU @ RECXT @ execute THEN
      THEN
   THEN ;

\ CHECK! ( a u -- flag ) : like CHECK but VERIFIES the body against a leading
\ ( in -- out ) declared sig (rejects on mismatch). The standalone REPL hook.
: CHECK! {: a u :}  -1 VSIG !  a u CHECK  0 VSIG ! ;
