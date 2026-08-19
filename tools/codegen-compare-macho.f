\ codegen-compare-macho.f - the per-symbol code size of every C twin, read out
\ of the reference object. One concern: the clang column's byte counts.
\
\ WHY THIS EXISTS AT ALL. The other two columns read a byte count off the
\ subject's own dictionary record, which holds where its code starts and how
\ long it is. A C function has no such record; what it has is a symbol in an
\ object file. So the reference column's bytes are read the way a linker reads
\ them - out of the object clang wrote, through the tools that ship with it -
\ and not estimated, not counted from a disassembly, and not reported by the
\ compiler about itself.
\
\ HOW A SYMBOL'S SIZE IS DERIVED, AND WHY IT IS NOT SIMPLY ASKED FOR. Mach-O
\ symbol table entries carry no size: `llvm-nm --print-size` says so in as many
\ words, and prints zero for every one. What a Mach-O object does carry is each
\ symbol's address and the length of the section it lives in, so a function's
\ size is the distance to the next symbol in the same section, and the last
\ one's is the distance to the section's end. That is exact rather than
\ approximate on this target for a stated reason: arm64 instructions are four
\ bytes and clang aligns a function to four, so no two functions have padding
\ between them - and the check below refuses the whole reading if the sizes ever
\ stop adding up to the section, which is what padding, an unexpected symbol, or
\ a reordering would look like.
\
\ AND ONLY THE EXTERNAL SYMBOLS ARE TWINS, WHICH USED TO BE A PRECONDITION
\ NOBODY CHECKED. Every twin is a plain function and therefore external; the
\ helpers twins.c keeps to itself - c5_long, c4_add1 and the rest - are static,
\ and clang inlines all of them at -O2, so nothing non-external stands in __text
\ except ltmp0, the assembler's label for the section's own head. If clang ever
\ declined to inline one, that helper would appear as a non-external symbol at an
\ address of its OWN, the reader would skip its line, and its bytes would be
\ added to whichever twin happened to sit in front of it. The tiling check could
\ not catch that: the sizes would still sum to the section, because the bytes
\ went to a neighbour rather than going missing. So a second refusal stands
\ beside it. Every non-external __text symbol must name an address an external
\ symbol already names - which is what a label like ltmp0 is - and one that
\ starts a stretch of its own is refused outright.
\
\ WHAT THAT REFUSAL BUYS THE REST OF THE HARNESS, AND EXACTLY WHAT IT DOES NOT.
\ A run that produced a reference column at all is a run in which clang inlined
\ every STATIC helper twins.c has, because the alternative did not build - so a
\ twin's symbol size being the whole of its program is checked rather than hoped
\ for, which is what lets the comparison read a habu row that branches to a
\ callee against a reference row that does not. What is NOT covered is a twin
\ that tail-branches to ANOTHER TWIN: that callee is external, this reader takes
\ it up normally, and nothing here notices. Dot habu-check-no-c-019014f8 carries
\ that check.
\
\ WHAT IS COUNTED AND WHAT IS NOT. __text, which is the instructions. Clang also
\ writes literal pools - __literal8, __literal16, __const - and those are read
\ and reported as one number beside the table rather than folded into any row:
\ they cannot be attributed to a symbol, because a pool entry is shared by
\ whichever functions happen to need that constant. The habu columns have no
\ pools at all, since both materialise a constant with move-wide instructions
\ inside the routine, so a row's byte count is the whole cost there and very
\ nearly the whole cost here. The report prints the pool total so the remainder
\ is visible rather than quietly missing.

require lib/errors.f
require lib/prelude.f
require lib/string.f
require tools/codegen-compare-cc.f
require tools/codegen-compare-text.f

package CODEGEN-MACHO

private

192 constant SYM-MAX
64 constant NAME-MAX
95 constant UNDERSCORE

SYM-MAX NAME-MAX * BUFFER: NAME-BYTES
create NAME-LENS SYM-MAX cells allot
create ADDRS SYM-MAX cells allot
create SIZES SYM-MAX cells allot

\ Where the non-external __text symbols sit. Their names are not kept, because
\ no twin is ever looked up by one; what is kept is the address, which is the
\ only thing the refusal below asks about.
create LOCAL-ADDRS SYM-MAX cells allot

variable LOCAL-N
variable SYM-N
variable TEXT-BYTES-CELL
variable POOL-BYTES-CELL
variable LOADED

: SLOT ( ptr a n -- ptr a )
   cells + ;

: SYM-OK ( n -- n )
   dup 0 < over SYM-N @ >= or if E-CODEGEN-CLANG-SYMBOL throw then ;

: NAME-AT ( n -- ptr u8 )
   NAME-MAX * NAME-BYTES + ;

\ nm writes a C symbol with the leading underscore the linker gives it; dlsym
\ and the harness both name it without one, so the underscore comes off here,
\ where the two spellings meet, rather than at every use.
: STRIP ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   u 0= if a u exit then
   a c@ UNDERSCORE <> if a u exit then
   a 1 +  u 1 - ;

: SYM+ ( ptr u8 n n -- ) {: a:ptr u:n addr:n :}
   SYM-N @ SYM-MAX >= if E-CODEGEN-CLANG-SYMBOL throw then
   u NAME-MAX > if E-CODEGEN-CLANG-SYMBOL throw then
   a  SYM-N @ NAME-AT  u STR-LEN BYTE-COPY-LEN
   u NAME-LENS SYM-N @ SLOT !
   addr ADDRS SYM-N @ SLOT !
   0 SIZES SYM-N @ SLOT !
   SYM-N @ 1+ SYM-N ! ;

\ ---- what nm said ------------------------------------------------------------
\ One data line reads
\
\     0000000000000090 (__TEXT,__text) external _hc1_add3
\
\ and the reading is structural: the address is a hexadecimal word, the section
\ is one word that must be the text section exactly, the linkage is one word
\ that must be `external` exactly - `non-external` is a different word and not a
\ prefix match - and the name is the word after it. A line of any other shape,
\ including nm's own headings, matches nothing and is skipped.

: TEXT-SECTION$ ( -- ptr u8 n )
   s" (__TEXT,__text)" ;

: EXTERNAL$ ( -- ptr u8 n )
   s" external" ;

\ One __text symbol that is not external. Anything whose linkage word is not
\ `external` exactly comes here, `non-external` and any spelling this reader has
\ never seen alike, so an unfamiliar listing fails towards the refusal rather
\ than towards a silent skip.
: LOCAL+ ( n -- ) {: addr:n :}
   LOCAL-N @ SYM-MAX >= if E-CODEGEN-CLANG-SYMBOL throw then
   addr LOCAL-ADDRS LOCAL-N @ SLOT !
   LOCAL-N @ 1+ LOCAL-N ! ;

: NM-LINE ( -- )
   CODEGEN-TEXT:NEXT-HEX 0= if drop exit then
   {: addr:n :}
   CODEGEN-TEXT:NEXT$ 0= if 2drop exit then
   TEXT-SECTION$ STR= 0= if exit then
   CODEGEN-TEXT:NEXT$ 0= if 2drop exit then
   EXTERNAL$ STR= 0= if addr LOCAL+ exit then
   CODEGEN-TEXT:NEXT$ 0= if 2drop exit then
   STRIP addr SYM+ ;

: SCAN-NM ( ptr u8 n -- ) {: a:ptr u:n :}
   a u CODEGEN-TEXT:TEXT!
   begin
      CODEGEN-TEXT:NEXT-LINE 0= if 2drop exit then
      CODEGEN-TEXT:LINE! NM-LINE
   again ;

\ ---- what size said ----------------------------------------------------------
\ One data line reads
\
\     \tSection (__TEXT, __text): 4760
\
\ and is read the same way: three words that must be what they are, then a
\ number. Every other __TEXT section is a literal pool and its bytes are added
\ up separately; the data and unwind segments are not __TEXT and are skipped by
\ the second word alone.

: SECTION$ ( -- ptr u8 n )
   s" Section" ;

: TEXT-SEG$ ( -- ptr u8 n )
   s" (__TEXT," ;

: TEXT-SEC$ ( -- ptr u8 n )
   s" __text):" ;

: SIZE-LINE ( -- )
   CODEGEN-TEXT:NEXT$ 0= if 2drop exit then
   SECTION$ STR= 0= if exit then
   CODEGEN-TEXT:NEXT$ 0= if 2drop exit then
   TEXT-SEG$ STR= 0= if exit then
   CODEGEN-TEXT:NEXT$ 0= if 2drop exit then
   TEXT-SEC$ STR= {: text?:bool :}
   CODEGEN-TEXT:NEXT-NUMBER 0= if drop exit then
   text? if TEXT-BYTES-CELL ! exit then
   POOL-BYTES-CELL @ + POOL-BYTES-CELL ! ;

: SCAN-SIZE ( ptr u8 n -- ) {: a:ptr u:n :}
   a u CODEGEN-TEXT:TEXT!
   begin
      CODEGEN-TEXT:NEXT-LINE 0= if 2drop exit then
      CODEGEN-TEXT:LINE! SIZE-LINE
   again ;

\ ---- addresses into sizes ----------------------------------------------------
\ nm prints its symbols in name order, so the order they were read in says
\ nothing about where they sit. ORDER holds the symbol indices sorted by
\ address, and every size is a distance in that order; the symbols themselves
\ are not moved, so a name lookup still finds what was read.

create ORDER SYM-MAX cells allot

: ORDER@ ( n -- n ) {: k:n :}
   ORDER k SLOT @ ;

: SYM-ADDR ( n -- n ) {: k:n :}
   ADDRS k SLOT @ ;

\ Insertion sort over the index array. A hundred-odd symbols is read once per
\ run, so what this has to be is obviously right rather than fast.
: SORT-ORDER ( -- )
   SYM-N @ 0 ?do i ORDER i SLOT ! loop
   SYM-N @ 1 ?do
      i ORDER@ {: v:n :}
      i
      begin
         dup 0 > if dup 1- ORDER@ SYM-ADDR v SYM-ADDR > else 0 0= 0= then
      while
         dup 1- ORDER@ over ORDER swap SLOT !
         1-
      repeat
      v swap ORDER swap SLOT !
   loop ;

\ The distance from each symbol to the next one in address order, and from the
\ last to the end of the section. Refused outright if the symbols do not tile
\ the section exactly: the first must start at its head, none may be empty, and
\ the last must end at its tail. That is what would break if clang started
\ padding between functions, if a symbol this reader does not expect appeared
\ in __text, or if a size were read off the wrong section - all of which would
\ otherwise show up as a byte column that is quietly wrong.
: SIZE-SYMS ( -- )
   SYM-N @ 0= if E-CODEGEN-CLANG-SIZE throw then
   0 ORDER@ SYM-ADDR 0 <> if E-CODEGEN-CLANG-SIZE throw then
   SYM-N @ 1- 0 ?do
      i 1+ ORDER@ SYM-ADDR  i ORDER@ SYM-ADDR -  {: d:n :}
      d 0 <= if E-CODEGEN-CLANG-SIZE throw then
      d SIZES i ORDER@ SLOT !
   loop
   TEXT-BYTES-CELL @  SYM-N @ 1- ORDER@ SYM-ADDR -  {: last:n :}
   last 0 <= if E-CODEGEN-CLANG-SIZE throw then
   last SIZES SYM-N @ 1- ORDER@ SLOT ! ;

\ ---- and nothing else may start a stretch of the section ---------------------
\ The sizes above are distances between EXTERNAL symbols, so they are the twins'
\ own bytes only while no other symbol stands between two of them. A label like
\ ltmp0 does not: it names an address an external symbol names too, and a
\ distance measured from that address is the same distance either way. A static
\ function clang declined to inline DOES, and its bytes would land on the twin in
\ front of it. So the question asked of every non-external symbol is whether an
\ external one is already there.

: EXTERNAL-AT? ( n -- bool ) {: addr:n :}
   SYM-N @ 0 ?do
      i SYM-ADDR addr = if true unloop exit then
   loop
   false ;

: LOCAL-CK ( -- )
   LOCAL-N @ 0 ?do
      LOCAL-ADDRS i SLOT @ EXTERNAL-AT? 0= if E-CODEGEN-CLANG-LOCAL throw then
   loop ;

public

\ Read one symbol table and one section listing. The two texts are parameters
\ rather than fetched here, so the scheduled test can drive this exact word -
\ the one production uses - over fixtures built to fool it: a non-external
\ symbol, a symbol in another section, a symbol whose NAME is a linkage word,
\ symbols out of address order, a non-external symbol standing between two twins,
\ and a section listing naming __text in the wrong segment. A reader tested only
\ on real nm output is a reader tested on the one input that cannot be wrong.
: LOAD-FROM ( ptr u8 n ptr u8 n -- ) {: na:ptr nu:n sa:ptr su:n :}
   0 SYM-N !
   0 LOCAL-N !
   0 TEXT-BYTES-CELL !
   0 POOL-BYTES-CELL !
   0 LOADED !
   na nu SCAN-NM
   sa su SCAN-SIZE
   TEXT-BYTES-CELL @ 0 <= if E-CODEGEN-CLANG-SIZE throw then
   SORT-ORDER
   SIZE-SYMS
   LOCAL-CK ;

\ Read the reference object once. Everything after this is a lookup.
\
\ THE FLAG MEANS "THE STORE HOLDS THE REFERENCE OBJECT", not "this word has been
\ called", which is why LOAD-FROM clears it: a caller that read some other
\ listing has replaced what is in the store, and the next caller that wants the
\ reference object must get the reference object. The scheduled suite runs the
\ harness's tests and this column's tests in ONE process, and with the flag
\ meaning the weaker thing the second file's fixtures survived into the first
\ file's measurements and every twin lookup failed.
: LOAD ( -- )
   LOADED @ if exit then
   CODEGEN-CC:NM-TEXT$ CODEGEN-CC:SIZE-TEXT$ LOAD-FROM
   -1 LOADED ! ;

: COUNT ( -- n )
   SYM-N @ ;

private

\ Only the search below reads a stored name: a caller names a twin and asks for
\ its size, never the other way round.
: NAME$ ( n -- ptr u8 n ) {: k:n :}
   k SYM-OK NAME-AT
   NAME-LENS k SLOT @ ;

public

\ Which symbol goes by this name, or -1.
: FIND ( ptr u8 n -- n ) {: a:ptr u:n :}
   0 begin dup SYM-N @ < while
      dup NAME$ a u STR= if exit then
      1+
   repeat drop -1 ;

\ How many bytes of machine code the twin of this name occupies. A name the
\ object does not carry is a claim the comparison made and did not keep - a row
\ naming a twin nobody compiled - so it throws rather than answering zero.
: BYTES ( ptr u8 n -- n )
   FIND dup 0 < if drop E-CODEGEN-CLANG-SYMBOL throw then
   {: k:n :}
   SIZES k SLOT @ ;

\ The whole text section, and the literal pools beside it that belong to no one
\ symbol. Both are printed by the report so the remainder is visible.
: SIZE ( n -- n ) {: k:n :}
   SIZES k SYM-OK SLOT @ ;

: TEXT-BYTES ( -- n )
   TEXT-BYTES-CELL @ ;

: POOL-BYTES ( -- n )
   POOL-BYTES-CELL @ ;

private

: INIT ( -- )
   0 LOADED !
   0 LOCAL-N !
   0 SYM-N ! ;

INIT

;package
