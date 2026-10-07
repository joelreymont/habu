\ does-clause-record.f - the dictionary record a does> clause gets, and the
\ branch that aims at it.
\
\ WHAT THIS PROVES, AND WHY IT IS NOT A NAME SEARCH. `does>` used to leave the
\ clause body nameless. LDOESPATCH patches the created word's RET into `b D`,
\ and D is an address INSIDE the defining word's compiled body, so the branch
\ names nothing: an AOT capture whose defining word lies outside the window
\ carries a displacement measured against code the target does not have, and
\ three compiler-chain words branched into the middle of PATHZ in a merged
\ engine because of it (dot habu-merged-engine-nmigrate-c970bf04). The clause
\ now carries a dictionary record of its own (src/habu/habu2.f J-DOES), so the
\ branch aims at a record ENTRY and the seed relocates it by name.
\
\ Every case here therefore reads STRUCTURE and not text: the record at the
\ parent's index PLUS ONE, the bytes of its name, its wordlist, the two spans'
\ shared end, and the instruction actually planted at the created word's RET -
\ decoded, so the opcode and the target are both checked.
\
\ THE NAME IS STILL A NAME. A wordlist holds at most one live row per folded
\ name (src/habu/habu1.f WLFIND), and the clause is a row of its parent's
\ wordlist, so a word already holding `<PARENT>;does` there refuses the definer
\ at `does>` with the duplicate-definition code, under both compilers, exactly
\ as that word is refused when it comes second. The same word in another
\ wordlist refuses nothing. An exported definer carries its clause into the
\ export's wordlist, under the same wall.
\
\ Run: bin/hb --load test/does-clause-record.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/test/outcome.f
require lib/test/subject.f
require src/habu/layout.f

package DOESREC-TEST
private

\ A code address is an integer; decoding its instruction needs a byte view.
CAST: N>U8 ( n -- ptr u8 )
: U8>N ( ptr u8 -- n ) NULL-PTR BYTE-VIEW - ;
TRUSTED: MARK-CALL ( n -- ) callmap-set ;
TRUSTED: MARK-ADDR ( n -- ) addrmap-set ;

$FC000000 constant OPC-MASK
$14000000 constant OPC-B
$94000000 constant OPC-BL
$3FFFFFF constant IMM26
$2000000 constant IMM26-SIGN
5 constant SUF-LEN
0 constant GLOBAL-WID

64 constant NAME-CAP
create WANT NAME-CAP allot
variable WANT-U

: W32@ ( n -- n ) {: a:n :}
   a N>U8 {: p:ptr :}
   p c@  p 1+ c@ 8 lshift or  p 2 + c@ 16 lshift or  p 3 + c@ 24 lshift or ;

\ The absolute target of the B/BL at a: sign-extended imm26, scaled, PC-relative.
: TGT ( n -- n ) {: a:n :}
   a W32@ IMM26 and  IMM26-SIGN xor IMM26-SIGN -  2 lshift  a + ;

: OPC ( n -- n ) W32@ OPC-MASK and ;

: IDX ( ptr u8 n -- n ) XREF-FIND-INDEX ;
: START ( n -- n ) XREF-REC XREF-START ;
: LEN ( n -- n ) XREF-REC XREF-LEN ;
: END ( n -- n ) {: k:n :} k START k LEN + ;
: WID ( n -- n ) XREF-REC XREF-WORDLIST ;
: NAME$ ( n -- ptr u8 n ) XREF-REC XREF-NAME$ ;

\ The name the clause must carry: its parent's, plus `;does`.
: WANT! ( ptr u8 n -- ) {: a:ptr u:n :}
   u SUF-LEN + NAME-CAP > if s" does-clause-record: name buffer too small" 76 die then
   a WANT u BYTE-COPY
   s" ;does" {: sa:ptr su:n :}
   sa  WANT u +  su BYTE-COPY
   u SUF-LEN + WANT-U ! ;

: WANT$ ( -- ptr u8 n ) WANT WANT-U @ ;

variable N0  variable N1  variable N2
variable MK-CP

\ ---- the subjects, compiled through the real interpreter ---------------------
\ Each definer is entered so a created word exists to carry the planted branch.
: SUBJECTS ( -- )
   ndict@ N0 !
   s" : DR-PLAIN ( n -- n ) 3 * ;" evaluate-closed
   ndict@ N1 !
   s" : DR-MK ( n -- n ) dup create , 1 + does> ( -- n ) @ ;" evaluate-closed
   cp@ MK-CP !
   ndict@ N2 !
   s" 7 DR-MK DR-SEVEN drop" evaluate-closed
   s" : DR-LONG-DEFINER-NAME ( n -- n ) dup create , 1 + does> ( -- n ) @ ;" evaluate-closed
   s" 9 DR-LONG-DEFINER-NAME DR-NINE drop" evaluate-closed
   \ a package keeps its clause in its own wordlist, whatever the global one holds
   s" : MK;does ( -- n ) 222 ;" evaluate-closed
   s" package DRP public : MK ( n -- n ) dup create , 1 + does> ( -- n ) @ ; ;package" evaluate-closed ;

\ ---- a clause name a live word already holds ---------------------------------
\ Each holder is spelled in another case than the clause would be: the
\ comparison is folded. DUP-DEF-RC is the engine's duplicate-definition code
\ (habu2.f C-DUP-DEF-FAIL).
$4E constant DUP-DEF-RC
variable HELD-ND  variable HELD-CP

: HELD-MARK ( -- )
   ndict@ HELD-ND !  cp@ HELD-CP ! ;

\ After the refusal: nothing published, and the holder still answers.
: ?HELD ( ptr u8 n ptr u8 n -- ) {: d:ptr du:n h:ptr hu:n :}
   s" the refused definer publishes neither record and moves no code" T-LABEL
   ndict@ HELD-ND @ T=
   cp@ HELD-CP @ T=
   d du GLOBAL-WID search-wl 0= TTRUE
   s" the word that holds the name still answers" T-LABEL
   h hu TEST-EVAL:N 111 T= ;

\ Tier 0, the legacy JIT every `--load` and the REPL run: J-DOES.
: HELD-JIT ( -- )
   s" : dr-jit;DOES ( -- n ) 111 ;" evaluate-closed
   HELD-MARK
   s" the legacy compiler refuses a definer whose clause name is held" T-LABEL
   [: s" : DR-JIT ( n -- n ) dup create , 1 + does> ( -- n ) @ ;" evaluate-closed ;]
   DUP-DEF-RC TTHROWSQ
   s" DR-JIT" s" dr-jit;DOES" ?HELD
   s" undefining the holder frees the name for the definer" T-LABEL
   s" undefine dr-jit;DOES" evaluate-closed
   s" : DR-JIT ( n -- n ) dup create , 1 + does> ( -- n ) @ ;" evaluate-closed
   s" 6 DR-JIT DR-JIT-SIX" TEST-EVAL:N 7 T=
   s" DR-JIT-SIX" TEST-EVAL:N 6 T= ;

\ Tier 1, the native chain an executable build runs: NCOMP-EMIT:CAPTURE-DOES.
: HELD-NATIVE ( -- )
   s" : Dr-Held;Does ( -- n ) 111 ;" evaluate-closed
   HELD-MARK
   s" the native compiler refuses a definer whose clause name is held" T-LABEL
   [: s" : DR-HELD ( n -- n ) dup create , 1 + does> ( -- n ) @ ;" evaluate-closed ;]
   DUP-DEF-RC TTHROWSQ
   s" DR-HELD" s" Dr-Held;Does" ?HELD
   s" undefining the holder frees the name for the definer" T-LABEL
   s" undefine Dr-Held;Does" evaluate-closed
   s" : DR-HELD ( n -- n ) dup create , 1 + does> ( -- n ) @ ;" evaluate-closed
   s" 6 DR-HELD DR-HELD-SIX" TEST-EVAL:N 7 T=
   s" DR-HELD-SIX" TEST-EVAL:N 6 T= ;

\ A definition the check hook refuses must leave BOTH slots uncounted.
variable REJ0  variable REJ1

: RUN-REJECTED ( -- )
   ndict@ REJ0 !
   [: s" : DR-BAD ( n -- n ) dup create , DR-NO-SUCH-WORD does> ( -- n ) @ ;" evaluate-closed ;] catch drop
   ndict@ REJ1 ! ;

\ The hook refuses DR-BAD at its parent, before the clause is scanned, and the
\ native compiler's line that follows the hook's must name the definer as it
\ names a plain one. Stderr is the evidence, so the definer is compiled again in
\ a forked child of this tier-1 image (lib/test/subject.f).
$800 constant IO-CAP
10000 constant CHILD-MS
70 constant HOOK-RC  \ src/core/check-hook.f CHECK-RC
create OUT IO-CAP allot
create ERR IO-CAP allot

: REJECTED-NAMED ( -- )
   s" : DR-BAD ( n -- n ) dup create , DR-NO-SUCH-WORD does> ( -- n ) @ ;"
   {: src:ptr srcu:n :}
   src srcu OUT IO-CAP >LEN ERR IO-CAP >LEN CHILD-MS >MS SUBJECT:RUN
   {: outu:len erru:len oc :}
   s" the hook's refusal ends the child with the hook's code" T-LABEL
   src srcu OUT outu LEN>N ERR erru LEN>N oc HOOK-RC T-OUTCOME-EXITED=
   s" the native compiler's line names the definer the hook refused" T-LABEL
   ERR erru LEN>N  S\" ncomp: cannot compile DR-BAD\n" CONTAINS? TTRUE ;

\ ---- the assertions ---------------------------------------------------------
\ The clause of the definer named by a/u: the record one slot above it.
: CLAUSE ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u IDX 1+ ;

: ?NAME ( ptr u8 n -- ) {: a:ptr u:n :}
   a u WANT!
   s" the clause record's name is its parent's plus ;does" T-LABEL
   a u CLAUSE NAME$ WANT$ STR= TTRUE ;

: ?WID ( ptr u8 n -- ) {: a:ptr u:n :}
   s" the clause record is in its parent's wordlist" T-LABEL
   a u CLAUSE WID  a u IDX WID  T= ;

: ?SPAN ( ptr u8 n -- ) {: a:ptr u:n :}
   s" the clause entry is inside its parent's span" T-LABEL
   a u CLAUSE START  a u IDX START >  TTRUE
   s" the clause and its parent end at the shared epilogue" T-LABEL
   a u CLAUSE END  a u IDX END  T= ;

: ?EXT ( ptr u8 n -- ) {: a:ptr u:n :}
   s" the clause record's name is stored out of line" T-LABEL
   a u CLAUSE XREF-REC XREF-EXT? TTRUE ;

\ THE CASE THIS FILE EXISTS FOR: the instruction LDOESPATCH planted at the
\ created word's RET is a B - never a BL, which would corrupt x30 - and its
\ target is the clause record's entry.
: ?BRANCH ( ptr u8 n ptr u8 n -- ) {: da:ptr du:n ca:ptr cu:n :}
   s" the created word's RET holds a branch, not a call" T-LABEL
   ca cu IDX END OPC  OPC-B  T=
   s" ... and it is not a branch-with-link" T-LABEL
   ca cu IDX END OPC  OPC-BL <>  TTRUE
   s" ... and it lands on the clause record's entry" T-LABEL
   ca cu IDX END TGT  da du CLAUSE START  T= ;

: ?FINDABLE ( ptr u8 n -- ) {: a:ptr u:n :}
   a u WANT!
   s" the clause record answers a search of its parent's wordlist" T-LABEL
   WANT$ a u IDX WID search-wl  a u CLAUSE START  T= ;

: PAD4 ( n -- n )
   3 + -4 and ;

: ?NAME-PAD ( -- )
   s" DR-MK" CLAUSE NAME$ {: a:ptr u:n :}
   s" the appended clause name starts past both recorded spans" T-LABEL
   a U8>N  s" DR-MK" IDX END 4 +  T=
   a U8>N  s" DR-MK" CLAUSE END 4 +  T=
   s" its padded name is the only code-space tail after the emission" T-LABEL
   a U8>N u PAD4 + MK-CP @ T=
   s" and the bytes outside the name are deterministic zero padding" T-LABEL
   u PAD4 u ?do a i + c@ 0 T= loop ;

variable FAIL-RESTORE-CP
variable FAIL-CP
variable FAIL-ND
variable FORGET-CP
variable FORGET-ND
variable FORGET-NAME-A

: MAP-BIT@ ( n n -- n ) {: base:n at:n :}
   at dbase@ - {: off:n :}
   data-base base + off 5 rshift + c@
   off 2 rshift 7 and rshift 1 and ;

: CODE-CEILING ( -- n )
   dbase@ REGION + $4000 - ;

: DR-MK-SIZE ( -- n )
   s" DR-MK" IDX LEN 4 + ;

\ Leave exactly enough room for the emitted module, but not for its permanent
\ clause-name pad. The publisher must refuse before either durable pointer moves.
: POST-EMIT-ROLLBACK ( -- )
   cp@ FAIL-RESTORE-CP !
   CODE-CEILING DR-MK-SIZE - dup FAIL-CP ! cp!
   ndict@ FAIL-ND !
   [: s" : DR-PAD-FAIL ( n -- n ) dup create , 1 + does> ( -- n ) @ ;" evaluate-closed ;]
   E-NPUB-ROOM TTHROWSQ
   s" a refusal after native emission leaves CP and both records unpublished" T-LABEL
   cp@ FAIL-CP @ T=
   ndict@ FAIL-ND @ T=
   s" DR-PAD-FAIL" GLOBAL-WID search-wl 0= TTRUE
   FAIL-RESTORE-CP @ cp!
   s" the rejected name and checker signature are reusable" T-LABEL
   s" : DR-PAD-FAIL ( n -- n ) 1 + ;" evaluate-closed
   s" 4 DR-PAD-FAIL" TEST-EVAL:N 5 T= ;

: FORGET-CASE ( -- )
   cp@ FORGET-CP !  ndict@ FORGET-ND !
   s" : DR-FORGET-MK ( n -- n ) dup create , 1 + does> ( -- n ) @ ;" evaluate-closed
   s" 17 DR-FORGET-MK DR-FORGET-CELL" TEST-EVAL:N 18 T=
   s" the definer, hidden clause, and created word add three records" T-LABEL
   ndict@ FORGET-ND @ 3 + T=
   s" DR-FORGET-MK;does" IDX NAME$ drop U8>N dup FORGET-NAME-A !
   dup MARK-CALL 4 + MARK-ADDR
   s" DR-FORGET-MK" FORGET-DEFS-FROM
   s" forgetting the parent reclaims its whole module and both later records" T-LABEL
   ndict@ FORGET-ND @ T=
   cp@ FORGET-CP @ T=
   s" DR-FORGET-MK" GLOBAL-WID search-wl 0= TTRUE
   s" DR-FORGET-MK;does" GLOBAL-WID search-wl 0= TTRUE
   s" DR-FORGET-CELL" GLOBAL-WID search-wl 0= TTRUE
   s" the reclaim clears call and address records from the appended name" T-LABEL
   SNAP-RELOC:CALLMAP-OFF FORGET-NAME-A @ MAP-BIT@ 0 T=
   SNAP-RELOC:ADDRMAP-OFF FORGET-NAME-A @ 4 + MAP-BIT@ 0 T=
   s" its name and checker signature are reusable after reclamation" T-LABEL
   s" : DR-FORGET-MK ( n -- n ) dup create , 1 + does> ( -- n ) @ ;" evaluate-closed
   s" 4 DR-FORGET-MK DR-FORGET-CELL" TEST-EVAL:N 5 T=
   s" DR-FORGET-CELL" TEST-EVAL:N 4 T= ;

\ ---- an exported definer carries its clause ---------------------------------
\ `export` gives a body a second record (habu2.f C-EXPORT), and a does>
\ definer's export gives its clause one too, in the slot above: the clause's
\ entry under the clause's name, in the wordlist the export publishes into, so
\ the qualifier that reaches the alias reaches its clause. Each compiler lays a
\ clause's name out its own way, so the pair is checked under both.

\ The exported definer d$, the qualified clause name k$ and a word w$ made
\ through the alias.
: ?EXPORTED ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: da:ptr du:n ka:ptr ku:n wa:ptr wu:n :}
   s" an exported definer's clause takes the slot above the export" T-LABEL
   ka ku IDX  da du CLAUSE  T=
   s" ... under the clause's own name" T-LABEL
   da du CLAUSE NAME$ s" MK;does" STR= TTRUE
   da du ?WID
   da du ?SPAN
   da du wa wu ?BRANCH ;

: EXPORTED-JIT ( -- )
   s" package DRXJ : MK ( n -- n ) dup create , 1 + does> ( -- n ) @ ; public export MK ;package" evaluate-closed
   s" 7 DRXJ:MK DRXJ-SEVEN drop" evaluate-closed
   s" DRXJ:MK" s" DRXJ:MK;does" s" DRXJ-SEVEN" ?EXPORTED
   s" a word made through the alias runs the clause" T-LABEL
   s" DRXJ-SEVEN" TEST-EVAL:N 7 T= ;

\ Undefining the private original retires its own pair and leaves the export's;
\ undefining the export retires its clause with it (src/habu/xref.f
\ XREF-RETIRE-INDEX), and neither moves code.
: EXPORTED-NATIVE ( -- )
   s" package DRXN : MK ( n -- n ) dup create , 1 + does> ( -- n ) @ ; public export MK ;package" evaluate-closed
   s" 7 DRXN:MK DRXN-SEVEN drop" evaluate-closed
   s" DRXN:MK" s" DRXN:MK;does" s" DRXN-SEVEN" ?EXPORTED
   s" DRXN:MK" XREF-FIND DEF-OCC:SELECT {: alias-slot:n alias-occ:n :}
   s" DRXN:MK;does" XREF-FIND DEF-OCC:SELECT {: clause-slot:n clause-occ:n :}
   alias-occ clause-occ T<>
   s" undefining the private original leaves the exported pair" T-LABEL
   s" package DRXN private undefine MK ;package" evaluate-closed
   alias-slot alias-occ DEF-OCC:RESOLVE XREF-START 0<> TTRUE
   clause-slot clause-occ DEF-OCC:RESOLVE XREF-START 0<> TTRUE
   s" DRXN:MK;does" IDX  s" DRXN:MK" CLAUSE  T=
   s" 8 DRXN:MK DRXN-EIGHT drop" evaluate-closed
   s" DRXN-EIGHT" TEST-EVAL:N 8 T=
   s" creating another word keeps the retained export pair" T-LABEL
   alias-slot alias-occ DEF-OCC:RESOLVE XREF-START 0<> TTRUE
   clause-slot clause-occ DEF-OCC:RESOLVE XREF-START 0<> TTRUE
   s" DRXN:MK" XREF-FIND DEF-OCC:SELECT {: public-slot:n public-occ:n :}
   s" DRXN:MK;does" XREF-FIND DEF-OCC:SELECT {: pub-clause-slot:n pub-clause-occ:n :}
   s" DRXN-SEVEN" XREF-FIND DEF-OCC:SELECT {: earlier-slot:n earlier-occ:n :}
   s" DRXN-EIGHT" XREF-FIND DEF-OCC:SELECT {: later-slot:n later-occ:n :}
   earlier-slot later-slot T<>
   earlier-occ later-occ T<>
   s" undefining the export retires its clause with it" T-LABEL
   s" package DRXN public undefine MK ;package" evaluate-closed
   s" DRXN:MK;does" IDX 0 < TTRUE
   public-slot public-occ DEF-OCC:RESOLVE XREF-START 0<> TTRUE
   pub-clause-slot pub-clause-occ DEF-OCC:RESOLVE XREF-START 0<> TTRUE
   s" ... and the words it made keep their clause" T-LABEL
   s" DRXN-SEVEN" TEST-EVAL:N 7 T= ;

\ A clause name the export's wordlist already holds refuses the export, as it
\ refuses a definer defined there.
: EXPORT-HELD ( -- )
   s" package DRXH public : mk;DOES ( -- n ) 111 ; private : MK ( n -- n ) dup create , 1 + does> ( -- n ) @ ; ;package" evaluate-closed
   HELD-MARK
   s" an export whose clause name its wordlist holds is refused" T-LABEL
   [: s" package DRXH public export MK ;package" evaluate-closed ;] DUP-DEF-RC TTHROWSQ
   s" the refused export publishes neither record and moves no code" T-LABEL
   ndict@ HELD-ND @ T=
   cp@ HELD-CP @ T=
   s" DRXH:MK" IDX 0 < TTRUE
   s" the word that holds the name still answers" T-LABEL
   s" DRXH:mk;DOES" TEST-EVAL:N 111 T= ;

public

\ Before the native chain is selected.
: RUN-JIT ( -- )
   T-RESET
   HELD-JIT
   EXPORTED-JIT ;

: RUN ( -- )
   SUBJECTS
   RUN-REJECTED

   s" a definer with no does> clause publishes one record" T-LABEL
   N1 @ N0 @ 1+ T=
   s" a definer with a does> clause publishes two" T-LABEL
   N2 @ N1 @ 2 + T=

   s" DR-MK" ?NAME
   s" DR-MK" ?WID
   s" DR-MK" ?SPAN
   s" DR-MK" ?EXT
   s" DR-MK" ?FINDABLE
   s" DR-MK" s" DR-SEVEN" ?BRANCH
   ?NAME-PAD

   s" DR-LONG-DEFINER-NAME" ?NAME
   s" DR-LONG-DEFINER-NAME" ?SPAN
   s" DR-LONG-DEFINER-NAME" ?FINDABLE
   s" DR-LONG-DEFINER-NAME" s" DR-NINE" ?BRANCH

   s" a packaged definer keeps its clause in the package's wordlist" T-LABEL
   s" DRP:MK" IDX 1+ WID  s" DRP:MK" IDX WID  T=
   s" ... and that is not the global wordlist" T-LABEL
   s" DRP:MK" IDX WID  GLOBAL-WID <>  TTRUE
   s" ... so the global word holding its clause name stands beside it" T-LABEL
   s" MK;does" TEST-EVAL:N 222 T=

   HELD-NATIVE
   EXPORTED-NATIVE
   EXPORT-HELD

   s" a refused definition counts neither slot" T-LABEL
   REJ1 @ REJ0 @ T=
   REJECTED-NAMED

   POST-EMIT-ROLLBACK
   FORGET-CASE

   T-REPORT
   s" does-clause-record: ok" type cr ;

;package

\ Keep the structural test helpers on the already-booted engine, then switch the
\ definitions SUBJECTS feeds through the interpreter onto the production chain.
\ Loading the chain is not selecting it: tier 0, the legacy JIT `:` compiler, is
\ what every `--load` and the REPL run (src/habu/layout.f NCOMP-DISPATCH:TIER-CELL),
\ and its J-DOES writes the clause name BEFORE the clause body, inside the parent's
\ span. The layout measured below is the native publication's - habu2.f
\ DOES-REC:NATIVE-PRIM appends the permanent name past both spans - so this file
\ selects tier 1 the way an executable build does, ahead of the definitions.
\ The one tier-0 case runs first.
DOESREC-TEST:RUN-JIT
require src/compiler/native/compiler.f
1 set-tier

DOESREC-TEST:RUN
