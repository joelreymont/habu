\ codegen-compare-clang-test.f - tests for the comparison's third column and its
\ second reference. Run: bin/hb --load tools/codegen-compare-clang-test.f
\
\ WHAT IS ATTACKED HERE, AND WHY EACH ONE MATTERS.
\
\ THE SYMBOL READER. The reference column's byte counts come out of nm and size,
\ so a reader that is fooled by a line reports a wrong number for every row and
\ nothing goes red. It is driven through CODEGEN-MACHO:LOAD-FROM - the same word
\ production uses, with the two texts as parameters - over listings built to
\ fool it: a non-external symbol whose linkage word ENDS in the word a substring
\ match would find, a symbol in another section of the same segment, a symbol
\ whose NAME is `external`, symbols in nm's own name order rather than address
\ order, and a section line naming __text in the wrong segment. Then the real
\ object is read and its sizes are required to tile its text section exactly.
\
\ THE TOKENIZER underneath it, on the two things it has to get right that the
\ committed-table reader never exercises: a tab-indented line, which is how size
\ writes, and a hexadecimal address, which is how nm writes.
\
\ THE CHAIN BASELINE, which is the second reference: a byte count that grew must
\ be a finding and a byte count that shrank must NOT be, because those are two
\ different events and treating them alike is how a baseline either freezes
\ progress or hides a regression. Both directions are attacked, on fixtures
\ built from a real measurement, through the production comparison.
\
\ AND THE REFERENCE COLUMN AS A WHOLE, on the real corpora: every row has a
\ twin, and every twin answers what the engine's word answered. A reference that
\ computed something else would be a faster number for a different program, so
\ that check is a finding in the production run and is asserted here as well.

require lib/test.f
require lib/string.f
require lib/fmt.f
require lib/fs.f
require lib/fs-mutate.f
require tools/codegen-compare-cli.f
require tools/codegen-compare-text.f
require tools/codegen-compare-macho.f
require tools/codegen-compare-clang.f
require tools/codegen-compare-cases.f
require tools/codegen-compare-cases2.f
require tools/codegen-compare-cases3.f
require tools/codegen-compare-cases4.f
require tools/codegen-compare-cases5.f

package CODEGEN-CLANG-TEST

private

$1000 constant FIX-CAP
128 constant PATH-CAP
10 constant NEWLINE-BYTE
32 constant SPACE-BYTE

FIX-CAP BUFFER: FIX-TEXT
variable FIX-U
PATH-CAP BUFFER: PATH-BUF
variable PATH-U
PATH-CAP BUFFER: DIR-BUF
variable DIR-U

: FIX+ ( ptr u8 n -- ) {: a:ptr u:n :}
   FIX-U @ u + FIX-CAP > if E-CODEGEN-COMPARE-CAP throw then
   a FIX-TEXT FIX-U @ + u STR-LEN BYTE-COPY-LEN
   FIX-U @ u + FIX-U ! ;

: FIX-C ( n -- ) {: c:n :}
   FIX-U @ 1+ FIX-CAP > if E-CODEGEN-COMPARE-CAP throw then
   c FIX-TEXT FIX-U @ + c!
   FIX-U @ 1+ FIX-U ! ;

: FIX-NL ( -- )
   NEWLINE-BYTE FIX-C ;

: FIX-SP ( -- )
   SPACE-BYTE FIX-C ;

: FIX-NUM ( n -- )
   SB-RESET FMT:SB-INT SB$ FIX+ ;

: FIX-RESET ( -- )
   0 FIX-U ! ;

: FIX$ ( -- ptr u8 n )
   FIX-TEXT FIX-U @ ;

\ ---- the tokenizer -----------------------------------------------------------

: WORD-IS? ( ptr u8 n -- bool )
   CODEGEN-TEXT:NEXT$ 0= if 2drop 2drop 0 0= 0= exit then
   STR= ;

: TOKENIZER-CASES ( -- )
   s" a tab-indented line splits into words the way a space-indented one does"
   T-LABEL
   S\" \tSection (__TEXT, __text): 4760" CODEGEN-TEXT:LINE!
   s" Section" WORD-IS? TTRUE
   s" (__TEXT," WORD-IS? TTRUE
   s" __text):" WORD-IS? TTRUE
   CODEGEN-TEXT:NEXT-NUMBER TTRUE 4760 T=

   s" a hexadecimal address reads as the number it is" T-LABEL
   s" 000000000000009c hello" CODEGEN-TEXT:LINE!
   CODEGEN-TEXT:NEXT-HEX TTRUE 156 T=

   s" upper case hexadecimal reads the same" T-LABEL
   s" 00000000000004AC" CODEGEN-TEXT:LINE!
   CODEGEN-TEXT:NEXT-HEX TTRUE 1196 T=

   s" a word that is not hexadecimal is refused rather than half read" T-LABEL
   s" 0x9c" CODEGEN-TEXT:LINE!
   CODEGEN-TEXT:NEXT-HEX 0= TTRUE drop

   s" an empty line has no words" T-LABEL
   s" " CODEGEN-TEXT:LINE!
   CODEGEN-TEXT:NEXT$ 0= TTRUE 2drop ;

\ ---- the symbol reader, on listings built to fool it -------------------------
\ Every fixture below is a whole nm listing and a whole size listing, handed to
\ the word production hands the real ones to.

: SIZE-OK$ ( -- ptr u8 n )
   S\" Segment : 8448\n\tSection (__TEXT, __text): 96\n\tSection (__TEXT, __literal16): 32\n\tSection (__DATA, __bss): 8 (zerofill)\n" ;

\ Three real symbols at 0, 32 and 64 in a 96-byte text section, so every size is
\ 32 - and around them every line a reader must NOT take up:
\
\   ltmp0        non-external, and its linkage word ENDS in `external`, which is
\                what a substring match would find. It sits at 0, which is where
\                the assembler's label for the head of __text really sits and
\                where _alpha is: a label names an address, it does not start a
\                stretch of one.
\   _external    a symbol whose NAME is the linkage word. It is at _beta's
\                address for the same reason, so what this line tests is the
\                reading of the line and nothing else.
\   _c1_subject  a symbol in another section of the SAME segment.
\
\ The three real ones are written in nm's own order, which is by name and not by
\ address, so a reader that trusted the input order would size them wrongly.
: NM-FOOL$ ( -- ptr u8 n )
   S\" 0000000000000020 (__TEXT,__text) external _beta\n0000000000000000 (__TEXT,__text) external _alpha\n0000000000000040 (__TEXT,__text) external _gamma\n0000000000000000 (__TEXT,__text) non-external ltmp0\n0000000000000020 (__TEXT,__text) non-external _external\n0000000000000000 (__TEXT,__const) non-external _c1_subject\n0000000000000000 (__LD,__compact_unwind) non-external ltmp1\n" ;

\ The listing the second refusal exists for, and it is the shape a static
\ function clang declined to inline really takes: a non-external symbol at
\ 0x10, which no external symbol names. Every size still adds up to 96 - which
\ is why the tiling check passes it - but sixteen of _alpha's thirty-two bytes
\ belong to the routine at 0x10 and to no twin at all.
: NM-LOCAL-CODE$ ( -- ptr u8 n )
   S\" 0000000000000000 (__TEXT,__text) external _alpha\n0000000000000010 (__TEXT,__text) non-external _c5_long\n0000000000000020 (__TEXT,__text) external _beta\n0000000000000040 (__TEXT,__text) external _gamma\n" ;

\ And the same listing with the helper external, which is what the reader is
\ entitled to assume: now every stretch has a name, and _alpha really is the
\ sixteen bytes the listing above would have charged it thirty-two for.
: NM-LOCAL-NAMED$ ( -- ptr u8 n )
   S\" 0000000000000000 (__TEXT,__text) external _alpha\n0000000000000010 (__TEXT,__text) external _c5_long\n0000000000000020 (__TEXT,__text) external _beta\n0000000000000040 (__TEXT,__text) external _gamma\n" ;

\ A linkage word this reader has never seen, at an address of its own. It must
\ fail towards the refusal and not towards a silent skip: whatever `weird` means,
\ the reader cannot claim the stretch after it belongs to _alpha.
: NM-UNKNOWN-LINKAGE$ ( -- ptr u8 n )
   S\" 0000000000000000 (__TEXT,__text) external _alpha\n0000000000000010 (__TEXT,__text) weird _mystery\n0000000000000020 (__TEXT,__text) external _beta\n0000000000000040 (__TEXT,__text) external _gamma\n" ;

: LOAD-FOOL ( -- )
   NM-FOOL$ SIZE-OK$ CODEGEN-MACHO:LOAD-FROM ;

: READER-CASES ( -- )
   s" only the external symbols of the text section are taken up" T-LABEL
   LOAD-FOOL
   CODEGEN-MACHO:COUNT 3 T=

   s" a symbol whose name is a linkage word is not one" T-LABEL
   s" external" CODEGEN-MACHO:FIND -1 T=

   s" a symbol of another section of the same segment is not taken up" T-LABEL
   s" c1_subject" CODEGEN-MACHO:FIND -1 T=

   s" the leading underscore nm writes is not part of the name" T-LABEL
   s" _alpha" CODEGEN-MACHO:FIND -1 T=
   s" alpha" CODEGEN-MACHO:FIND 0 >= TTRUE

   s" sizes are distances in ADDRESS order, whatever order the listing was in"
   T-LABEL
   s" alpha" CODEGEN-MACHO:BYTES 32 T=
   s" beta" CODEGEN-MACHO:BYTES 32 T=
   s" gamma" CODEGEN-MACHO:BYTES 32 T=

   s" the text section and the literal pools are read apart" T-LABEL
   CODEGEN-MACHO:TEXT-BYTES 96 T=
   CODEGEN-MACHO:POOL-BYTES 32 T= ;

\ A section listing whose __text line is in the WRONG SEGMENT. A reader that
\ matched the section name alone would take 4 as the size of the text section
\ and then refuse every symbol; what must happen instead is that no text section
\ is found at all.
: SIZE-WRONG-SEGMENT$ ( -- ptr u8 n )
   S\" Segment : 8\n\tSection (__DATA, __text): 4\n" ;

\ Symbols that do not tile: the last one starts past the end of the section.
: NM-PAST-END$ ( -- ptr u8 n )
   S\" 0000000000000000 (__TEXT,__text) external _alpha\n0000000000000600 (__TEXT,__text) external _beta\n" ;

\ Symbols that do not tile the other way: two at one address, so one is empty.
: NM-DUPLICATE-ADDR$ ( -- ptr u8 n )
   S\" 0000000000000000 (__TEXT,__text) external _alpha\n0000000000000000 (__TEXT,__text) external _beta\n0000000000000020 (__TEXT,__text) external _gamma\n" ;

\ A listing whose first symbol does not start at the head of the section, which
\ is what a symbol the reader silently dropped would look like.
: NM-GAP-AT-HEAD$ ( -- ptr u8 n )
   S\" 0000000000000020 (__TEXT,__text) external _beta\n0000000000000040 (__TEXT,__text) external _gamma\n" ;

: REFUSAL-CASES ( -- )
   s" a section listing that names __text in another segment finds no section"
   T-LABEL
   [: NM-FOOL$ SIZE-WRONG-SEGMENT$ CODEGEN-MACHO:LOAD-FROM ;]
   E-CODEGEN-CLANG-SIZE TTHROWSQ

   s" a symbol past the end of the section is refused" T-LABEL
   [: NM-PAST-END$ SIZE-OK$ CODEGEN-MACHO:LOAD-FROM ;]
   E-CODEGEN-CLANG-SIZE TTHROWSQ

   s" two symbols at one address is refused" T-LABEL
   [: NM-DUPLICATE-ADDR$ SIZE-OK$ CODEGEN-MACHO:LOAD-FROM ;]
   E-CODEGEN-CLANG-SIZE TTHROWSQ

   s" symbols that do not start at the head of the section are refused" T-LABEL
   [: NM-GAP-AT-HEAD$ SIZE-OK$ CODEGEN-MACHO:LOAD-FROM ;]
   E-CODEGEN-CLANG-SIZE TTHROWSQ

   s" a twin the object does not carry is refused, not answered with zero"
   T-LABEL
   LOAD-FOOL
   [: s" delta" CODEGEN-MACHO:BYTES drop ;]
   E-CODEGEN-CLANG-SYMBOL TTHROWSQ

   \ The four cases below are one claim: text the reader cannot name a twin for
   \ stops the reading. They are worth having separately from the tiling cases
   \ because the tiling check PASSES every one of them - the bytes are all
   \ accounted for, they are simply accounted to the wrong symbol.
   s" a non-external symbol starting a stretch of __text is refused" T-LABEL
   [: NM-LOCAL-CODE$ SIZE-OK$ CODEGEN-MACHO:LOAD-FROM ;]
   E-CODEGEN-CLANG-LOCAL TTHROWSQ

   s" a linkage word the reader does not know is refused, never skipped" T-LABEL
   [: NM-UNKNOWN-LINKAGE$ SIZE-OK$ CODEGEN-MACHO:LOAD-FROM ;]
   E-CODEGEN-CLANG-LOCAL TTHROWSQ

   s" and that refusal is what stops a twin being charged a stranger's bytes"
   T-LABEL
   NM-LOCAL-NAMED$ SIZE-OK$ CODEGEN-MACHO:LOAD-FROM
   s" alpha" CODEGEN-MACHO:BYTES 16 T=
   s" c5_long" CODEGEN-MACHO:BYTES 16 T=

   s" a non-external symbol at an external's address is a label and sizes nothing"
   T-LABEL
   LOAD-FOOL
   CODEGEN-MACHO:COUNT 3 T=
   s" alpha" CODEGEN-MACHO:BYTES 32 T= ;

\ ---- the real object ---------------------------------------------------------
\ The fixtures above say the reader is not fooled. This says the thing it reads
\ is what it thinks: every symbol of the real reference object is a positive
\ whole number of instructions, and together they are the whole text section.

: SIZES-TILE? ( -- bool )
   0
   CODEGEN-MACHO:COUNT 0 ?do i CODEGEN-MACHO:SIZE + loop
   CODEGEN-MACHO:TEXT-BYTES = ;

: ALL-INSTRUCTIONS? ( -- bool )
   0 0=
   CODEGEN-MACHO:COUNT 0 ?do
      i CODEGEN-MACHO:SIZE 0 <= if drop 0 0= 0= leave then
      i CODEGEN-MACHO:SIZE 4 mod 0 <> if drop 0 0= 0= leave then
   loop ;

: REAL-OBJECT-CASES ( -- )
   CODEGEN-MACHO:LOAD

   s" the real object carries every twin the five corpora name" T-LABEL
   s" hc1_add3" CODEGEN-MACHO:FIND 0 >= TTRUE
   s" hc2_t_res_walk" CODEGEN-MACHO:FIND 0 >= TTRUE
   s" hc3_t_rel_l2" CODEGEN-MACHO:FIND 0 >= TTRUE
   s" hc4_store_load" CODEGEN-MACHO:FIND 0 >= TTRUE
   s" hc5_tail_big" CODEGEN-MACHO:FIND 0 >= TTRUE
   s" hf_i9" CODEGEN-MACHO:FIND 0 >= TTRUE

   s" every symbol is a positive whole number of arm64 instructions" T-LABEL
   ALL-INSTRUCTIONS? TTRUE

   s" and together they are the whole text section" T-LABEL
   SIZES-TILE? TTRUE ;

\ ---- the chain baseline ------------------------------------------------------
\ The second reference. A fixture is written from the chain rows of a real pass
\ with one number changed, and the production comparison is run against it.

: PATH! ( ptr u8 n -- ) {: a:ptr u:n :}
   u PATH-CAP > if E-CODEGEN-COMPARE-CAP throw then
   a PATH-BUF u STR-LEN BYTE-COPY-LEN
   u PATH-U ! ;

: PATH$ ( -- ptr u8 n )
   PATH-BUF PATH-U @ ;

: DIR$ ( -- ptr u8 n )
   DIR-BUF DIR-U @ ;

: FIX-WRITE ( -- )
   SB-RESET DIR$ SB-APPEND s" /chain.txt" SB-APPEND SB$ PATH!
   PATH$ FIX$ ATOMIC-WRITE-FILE ;

\ One chain row written with the size the caller chooses, so a fixture differs
\ from the measurement in exactly one number.
: CHAIN-ROW ( n n -- ) {: k:n size:n :}
   CODEGEN-COMPARE:PATH-NEW$ FIX+
   FIX-SP k CODEGEN-COMPARE:NAME$ FIX+
   FIX-SP size FIX-NUM
   FIX-SP k CODEGEN-COMPARE:COST FIX-NUM
   0 begin dup k CODEGEN-COMPARE:OUTPUTS < while
      FIX-SP dup k swap CODEGEN-COMPARE:OUTPUT FIX-NUM
      1+
   repeat drop
   FIX-NL ;

: FIRST-CHAIN-ROW ( -- n )
   0 begin dup CODEGEN-COMPARE:ROWS < while
      dup CODEGEN-COMPARE:PATH@ CODEGEN-COMPARE:PATH-NEW = if exit then
      1+
   repeat drop -1 ;

\ The head of a chain table: its own declared row count, which is what makes a
\ deleted row a finding even when the measurement lost the same word.
: CHAIN-HEADER ( n -- ) {: extra:n :}
   FIX-RESET
   s" rows: " FIX+
   CODEGEN-COMPARE:PATH-NEW CODEGEN-COMPARE:ROWS-OF extra + FIX-NUM FIX-NL ;

\ Every chain row of the pass, written truly except for one row whose size is
\ shifted by the caller's delta. A delta of zero, or a row index of -1, is an
\ honest table.
: CHAIN-ROWS ( n n -- ) {: at:n delta:n :}
   CODEGEN-COMPARE:ROWS 0 ?do
      i CODEGEN-COMPARE:PATH@ CODEGEN-COMPARE:PATH-NEW = if
         i  i CODEGEN-COMPARE:SIZE  i at = if delta + then  CHAIN-ROW
      then
   loop ;

: CHAIN-FIXTURE ( n n -- )
   0 CHAIN-HEADER
   CHAIN-ROWS ;

: CHAIN-FINDINGS ( -- n )
   FIX-WRITE
   CODEGEN-COMPARE:PATH-NEW CODEGEN-BASELINE:PATH!
   PATH$ CODEGEN-BASELINE:LOAD
   CODEGEN-BASELINE:COMPARE
   CODEGEN-COMPARE:PATH-OLD CODEGEN-BASELINE:PATH! ;

\ A table with a row for a word the pass did not measure: the chain used to
\ compile it and no longer does. The declared count is written for the table as
\ it stands, so what the comparison reports is the lost row and not a count that
\ disagrees with the file it is in.
: CHAIN-LOST-ROW ( -- )
   1 CHAIN-HEADER
   -1 0 CHAIN-ROWS
   CODEGEN-COMPARE:PATH-NEW$ FIX+
   FIX-SP s" CODEGEN-CORPUS:A-WORD-THE-CHAIN-HAS-LOST" FIX+
   FIX-SP 16 FIX-NUM
   FIX-SP 1000 FIX-NUM
   FIX-NL ;

: CHAIN-BASELINE-CASES ( -- )
   FIRST-CHAIN-ROW {: k:n :}

   s" an honest chain baseline reports nothing" T-LABEL
   -1 0 CHAIN-FIXTURE
   CHAIN-FINDINGS 0 T=

   s" a chain row that GREW is a regression against ourselves and a finding"
   T-LABEL
   k -4 CHAIN-FIXTURE
   CHAIN-FINDINGS 1 T=

   s" a chain row that SHRANK is progress and is not a finding" T-LABEL
   k 4 CHAIN-FIXTURE
   CHAIN-FINDINGS 0 T=

   s" a row the chain no longer compiles is a finding" T-LABEL
   CHAIN-LOST-ROW
   CHAIN-FINDINGS 1 T= ;

\ ---- the reference column on the real corpora --------------------------------

: REF-ROWS-OF ( -- n )
   CODEGEN-COMPARE:PATH-CLANG CODEGEN-COMPARE:ROWS-OF ;

: EVERY-ROW-TWINNED? ( -- bool )
   0 0=
   CODEGEN-COMPARE:ROWS 0 ?do
      i CODEGEN-COMPARE:PATH@ CODEGEN-COMPARE:PATH-OLD = if
         CODEGEN-COMPARE:PATH-CLANG i CODEGEN-COMPARE:NAME$
         CODEGEN-COMPARE:FIND-ROW 0 < if drop 0 0= 0= leave then
      then
   loop ;

: REF-MISMATCHES ( -- n )
   0
   CODEGEN-COMPARE:ROWS 0 ?do
      i CODEGEN-COMPARE:PATH@ CODEGEN-COMPARE:PATH-CLANG = if
         i CODEGEN-COMPARE:ROW-MATCH? 0= if 1+ then
      then
   loop ;

: CORPUS-REF-CASES ( ptr u8 n n -- ) {: a:ptr u:n want:n :}
   a u T-LABEL
   REF-ROWS-OF want T=
   EVERY-ROW-TWINNED? TTRUE
   REF-MISMATCHES 0 T= ;

: REAL-COLUMN-CASES ( -- )
   CODEGEN-CASES:RUN
   s" every row of the first corpus has a twin that answers what it answers"
   11 CORPUS-REF-CASES
   CODEGEN-CASES2:RUN
   s" every row of the second corpus has a twin that answers what it answers"
   8 CORPUS-REF-CASES
   CODEGEN-CASES3:RUN
   s" every row of the third corpus has a twin that answers what it answers"
   11 CORPUS-REF-CASES
   CODEGEN-CASES4:RUN
   s" every row of the fourth corpus has a twin that answers what it answers"
   13 CORPUS-REF-CASES
   CODEGEN-CASES5:RUN
   s" every row of the fifth corpus has a twin that answers what it answers"
   8 CORPUS-REF-CASES ;

\ ---- an absent toolchain is a result -----------------------------------------
\ Nothing here can uninstall clang, so what is checked is the shape of the
\ answer: the column says whether it is there, and when it is not there it says
\ why in words rather than leaving the report to print two columns in silence.

: ABSENCE-CASES ( -- )
   s" a host with no reference column names what it is missing" T-LABEL
   CODEGEN-CLANG:PRESENT? 0= if
      CODEGEN-CLANG:ABSENT-WHY$ nip 0 > TTRUE
   else
      CODEGEN-CLANG:FLAGS$ nip 0 > TTRUE
   then ;

: SETUP ( -- )
   CLEANUP-RESET
   s" habu-ccreftest" TMPDIR-MKDIR {: a:ptr u:n :}
   u PATH-CAP > if E-CODEGEN-COMPARE-CAP throw then
   a DIR-BUF u STR-LEN BYTE-COPY-LEN
   u DIR-U !
   DIR$ CLEANUP-TREE+
   CODEGEN-BASELINE:QUIET! ;

public

: MAIN ( -- )
   T-RESET
   SETUP
   TOKENIZER-CASES
   READER-CASES
   REFUSAL-CASES
   ABSENCE-CASES
   CODEGEN-CLANG:PRESENT? if
      REAL-OBJECT-CASES
      REAL-COLUMN-CASES
      CHAIN-BASELINE-CASES
   then
   CLEANUP-RUN
   \ The reference build is this process's own temporary tree; a scheduled member
   \ that left one behind would leave one per run.
   CODEGEN-CC:REMOVE
   CODEGEN-BASELINE:LOUD!
   T-REPORT ;

;package

CODEGEN-CLANG-TEST:MAIN
