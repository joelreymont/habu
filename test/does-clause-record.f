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
\ decoded, so the opcode and the target are both checked. Two cases define a
\ decoy word literally NAMED `<PARENT>;does` before the definer runs, so
\ "a record with that name exists" cannot pass for "the clause has that record":
\ what is asserted is that the branch lands on the record the definer made.
\
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f test/does-clause-record.f

require lib/errors.f
require lib/string.f
require lib/test.f
require src/habu/layout.f

package DOESREC-TEST
private

\ `evaluate` is the metaprogramming boundary the checker does not model and the
\ only way to compile a definition from inside a test. The byte view of a code
\ address is the same raw cast the engine's own capture takes to decode an
\ instruction (src/habu/aot-capture.f AOT-N>U8).
TRUSTED: EV ( ptr u8 n -- )
   evaluate ;
TRUSTED: N>U8 ( n -- ptr u8 ) ;

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

variable N0  variable N1  variable N2  variable N3

\ ---- the subjects, compiled through the real interpreter ---------------------
\ Each definer is entered so a created word exists to carry the planted branch.
: SUBJECTS ( -- )
   ndict@ N0 !
   s" : DR-PLAIN ( n -- n ) 3 * ;" EV
   ndict@ N1 !
   s" : DR-MK ( n -- n ) dup create , 1 + does> ( -- n ) @ ;" EV
   ndict@ N2 !
   s" 7 DR-MK DR-SEVEN drop" EV
   s" : DR-LONG-DEFINER-NAME ( n -- n ) dup create , 1 + does> ( -- n ) @ ;" EV
   s" 9 DR-LONG-DEFINER-NAME DR-NINE drop" EV
   \ the decoy: a real word already carrying the name the clause will take
   s" : DR-DECOY;does ( -- n ) 111 ;" EV
   s" : DR-DECOY ( n -- n ) dup create , 1 + does> ( -- n ) @ ;" EV
   s" 5 DR-DECOY DR-FIVE drop" EV
   \ a package keeps its clause in its own wordlist
   s" package DRP public : MK ( n -- n ) dup create , 1 + does> ( -- n ) @ ; ;package" EV
   ndict@ N3 ! ;

\ A definition the check hook refuses must leave BOTH slots uncounted.
variable REJ0  variable REJ1

: RUN-REJECTED ( -- )
   ndict@ REJ0 !
   [: s" : DR-BAD ( n -- n ) dup create , DR-NO-SUCH-WORD does> ( -- n ) @ ;" EV ;] catch drop
   ndict@ REJ1 ! ;

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

public

: RUN ( -- )
   T-RESET
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

   s" DR-LONG-DEFINER-NAME" ?NAME
   s" DR-LONG-DEFINER-NAME" ?SPAN
   s" DR-LONG-DEFINER-NAME" ?FINDABLE
   s" DR-LONG-DEFINER-NAME" s" DR-NINE" ?BRANCH

   \ the decoy carries the same name and is NOT the record the branch names
   s" DR-DECOY" ?NAME
   s" DR-DECOY" ?SPAN
   s" DR-DECOY" s" DR-FIVE" ?BRANCH
   \ the decoy sits one slot BELOW its namesake's parent, because it was defined
   \ immediately before it; a lookup by name answers the later record, which is
   \ exactly why nothing here is asked by name.
   s" the decoy one slot below the definer carries the same name" T-LABEL
   s" DR-DECOY" IDX 1- NAME$  s" DR-DECOY" CLAUSE NAME$  STR= TTRUE
   s" ... in the same wordlist, and is a different record" T-LABEL
   s" DR-DECOY" IDX 1- WID  s" DR-DECOY" CLAUSE WID  T=
   s" DR-DECOY" IDX 1- START  s" DR-DECOY" CLAUSE START  <> TTRUE
   s" the branch lands on the clause and not on the decoy that shares its name" T-LABEL
   s" DR-FIVE" IDX END TGT  s" DR-DECOY" IDX 1- START  <> TTRUE

   s" a packaged definer keeps its clause in the package's wordlist" T-LABEL
   s" DRP:MK" IDX 1+ WID  s" DRP:MK" IDX WID  T=
   s" ... and that is not the global wordlist" T-LABEL
   s" DRP:MK" IDX WID  GLOBAL-WID <>  TTRUE

   s" a refused definition counts neither slot" T-LABEL
   REJ1 @ REJ0 @ T=

   T-REPORT
   s" does-clause-record: ok" type cr ;

;package

DOESREC-TEST:RUN
