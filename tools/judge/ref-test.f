\ judge/ref-test.f - the reference column's symbol reader, attacked.
\ Run: bin/hb --load tools/judge/ref-test.f
\
\ WHAT IS ATTACKED HERE, AND WHY IT MATTERS TO THE JUDGE. Every `clang` cell of
\ test/compiler/judge-baseline.txt is a number this reader took out of `nm -m`
\ and `size -m` (tools/judge/pass.f BYTES calls CODEGEN-MACHO:BYTES). A reader
\ that is fooled by one line reports a wrong number for every row of the
\ artifact and nothing goes red: the judge would commit the wrong reference
\ column and then agree with itself about it for ever. So the reader is driven
\ through CODEGEN-MACHO:LOAD-FROM - the same word production uses, with the two
\ listings as parameters - over listings built to fool it.
\
\ THE LISTINGS AND WHAT EACH ONE HIDES.
\
\   a non-external symbol whose LINKAGE WORD ends in the word a substring match
\   would find; a symbol whose NAME is that linkage word; a symbol in another
\   section of the SAME segment; symbols in nm's own name order rather than
\   address order; a section line naming __text in the wrong segment; symbols
\   that do not tile the section, from both ends; and a linkage word this reader
\   has never seen.
\
\ THE TOKENIZER UNDERNEATH IT, on the two things it has to get right that
\ nothing else exercises: a tab-indented line, which is how `size` writes, and a
\ hexadecimal address, which is how `nm` writes.
\
\ AND THEN THE REAL OBJECT: every symbol of it is a positive whole number of
\ arm64 instructions and together they are the whole text section, so the thing
\ the reader is reading is what it thinks it is.
\
\ WHY THIS IS A FILE OF THE JUDGE'S. The reader is the judge's dependency now.
\ It used to be covered by the comparison harness the judge replaced, and when
\ that harness went its only coverage would have gone with it.

require lib/errors.f
require lib/prelude.f
require lib/string.f
require lib/test.f
require tools/codegen-compare-text.f
require tools/codegen-compare-macho.f
require tools/codegen-compare-clang.f

package JUDGE-REF-TEST

private

\ ---- the tokenizer -----------------------------------------------------------

: WORD-IS? ( ptr u8 n -- bool )
   CODEGEN-TEXT:NEXT$ 0= if 2drop 2drop false exit then
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
   true
   CODEGEN-MACHO:COUNT 0 ?do
      i CODEGEN-MACHO:SIZE 0 <= if drop false leave then
      i CODEGEN-MACHO:SIZE 4 mod 0 <> if drop false leave then
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
   SIZES-TILE? TTRUE

   \ What the judge's artifact prints beside its clang flags. The literal pools
   \ are what the per-twin column cannot account for, so the two numbers are
   \ reported together or the remainder goes missing quietly.
   s" the whole text section and the orphan pool are both positive" T-LABEL
   CODEGEN-CLANG:TEXT-BYTES 0 > TTRUE
   CODEGEN-CLANG:POOL-BYTES 0 > TTRUE
   CODEGEN-CLANG:TEXT-BYTES CODEGEN-MACHO:TEXT-BYTES T=
   CODEGEN-CLANG:POOL-BYTES CODEGEN-MACHO:POOL-BYTES T= ;

\ ---- an absent toolchain is a result -----------------------------------------
\ Nothing here can uninstall clang, so what is checked is the shape of the
\ answer: the column says whether it is there, and when it is not there it says
\ why in words rather than leaving the artifact to print a dash column in
\ silence.

: ABSENCE-CASES ( -- )
   s" a host with no reference column names what it is missing" T-LABEL
   CODEGEN-CLANG:PRESENT? 0= if
      CODEGEN-CLANG:ABSENT-WHY$ nip 0 > TTRUE
   else
      CODEGEN-CLANG:FLAGS$ nip 0 > TTRUE
   then ;

public

: MAIN ( -- )
   T-RESET
   TOKENIZER-CASES
   READER-CASES
   REFUSAL-CASES
   ABSENCE-CASES
   CODEGEN-CLANG:PRESENT? if REAL-OBJECT-CASES then
   \ The reference build is this process's own temporary tree; a scheduled
   \ member that left one behind would leave one per run.
   CODEGEN-CC:REMOVE
   T-REPORT ;

;package

JUDGE-REF-TEST:MAIN
