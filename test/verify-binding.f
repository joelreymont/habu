\ verify-binding.f - the binding each reference of a quiet composition selected
\ (src/habu/verify-source.f ON-BINDING) and the word a symbol is (SYM-SOURCE).
\ Run: bin/hb --load test/verify-binding.f
\
\ A design's entry calls a vocabulary and includes files that call it by its
\ own name, through an export and through an export of that export, call words
\ of the same spelling the design defines, undefine an export, or the word an
\ export names, and define that name afresh, and read one file twice. This file
\ loads the vocabulary, so its words have no declaration location, as the
\ image's words have none.
\ How this can fail, each one an assertion below:
\ - a use bound to a word with no location, or one in a loaded file, is missed;
\ - a use in a colon body is missed or named under another file;
\ - a position is not the token's bytes in its own file;
\ - an export, or an export of an export, does not name the word it exports;
\ - a word that is no export names another word;
\ - a design-local word of the vocabulary's spelling counts as the vocabulary's;
\ - a name undefined and defined afresh still names the word it exported;
\ - an export names the word defined afresh in place of the one it exports;
\ - a file read twice is reported once, or at other positions;
\ - an export the engine bakes lost its link in the native capture.

require lib/test.f
require lib/fs.f
require lib/fs-mutate.f
require lib/string.f
require lib/fmt.f
require src/habu/verify-source.f

package VBT-VOCAB
public
: EXTRUDE ( ptr u8 n ptr u8 n ptr u8 n n -- ) 2drop 2drop 2drop drop ;
: MM ( n -- n ) ;
;package

package VERIFY-BINDING-TEST

create ROOT FS-PATH-CAP allot variable ROOT-U
create ENTRY FS-PATH-CAP allot variable ENTRY-U

: ROOT$ ( -- ptr u8 n ) ROOT ROOT-U @ ;
: ENTRY$ ( -- ptr u8 n ) ENTRY ENTRY-U @ ;

: COPY! ( ptr u8 n ptr u8 ptr n -- )
   {: a:ptr u:n dst:ptr len:ptr :}
   a dst u BYTE-COPY u len ! ;

\ A fixture file's bytes, built a line at a time. The entry is written last, so
\ its bytes stay here as the subject's.
1024 constant TEXT-CAP
TEXT-CAP BUFFER: TEXT
TYPED-VARIABLE TEXT-U len

: TEXT$ ( -- ptr u8 n ) TEXT TEXT-U BUF-LEN@ ;
: TEXT+ ( ptr u8 n -- ) TEXT TEXT-CAP TEXT-U BUF-APPEND ;

: WRITE-TEXT ( ptr u8 n -- ) {: a:ptr u:n :}
   ROOT$ a u SOURCE-ROOT:JOIN TEXT$ WRITE-ALL
   TEXT-U BUF-RESET ;

: PLATE-FILE ( -- )
   S\" : VBT-THICK ( -- ) s\" p\" s\" top\" s\" bar\" 5 VBT-VOCAB:MM VBT-VOCAB:EXTRUDE ;\n" TEXT+
   S\" s\" p\" s\" top\" s\" bar\" 3 VBT-VOCAB:MM VBT-VOCAB:EXTRUDE\n" TEXT+
   s" design/plate.f" WRITE-TEXT ;

: ALIAS-FILE ( -- )
   S\" package VBT-ALIAS\npublic\nEXPORT VBT-VOCAB:EXTRUDE\n;package\n" TEXT+
   S\" s\" a\" s\" b\" s\" c\" 1 VBT-ALIAS:EXTRUDE\n" TEXT+
   s" design/alias.f" WRITE-TEXT ;

: CHAIN-FILE ( -- )
   S\" package VBT-ALIAS2\npublic\nEXPORT VBT-ALIAS:EXTRUDE\n;package\n" TEXT+
   S\" s\" a\" s\" b\" s\" c\" 2 VBT-ALIAS2:EXTRUDE\n" TEXT+
   s" design/chain.f" WRITE-TEXT ;

: TWICE-FILE ( -- )
   S\" s\" a\" s\" b\" s\" c\" 4 VBT-VOCAB:MM VBT-VOCAB:EXTRUDE\n" TEXT+
   s" design/twice.f" WRITE-TEXT ;

: REDEFINE-FILE ( -- )
   S\" package VBT-REDEF\npublic\nEXPORT VBT-VOCAB:EXTRUDE\n;package\n" TEXT+
   S\" s\" a\" s\" b\" s\" c\" 6 VBT-REDEF:EXTRUDE\n" TEXT+
   S\" package VBT-REDEF\npublic\nundefine EXTRUDE\n" TEXT+
   S\" : EXTRUDE ( ptr u8 n ptr u8 n ptr u8 n n -- ) 2drop 2drop 2drop drop ;\n" TEXT+
   S\" ;package\ns\" a\" s\" b\" s\" c\" 7 VBT-REDEF:EXTRUDE\n" TEXT+
   s" design/redefine.f" WRITE-TEXT ;

: REDEFINE-SOURCE-FILE ( -- )
   S\" package VBT-PA\npublic\n: X ( n -- n ) 1 + ;\n;package\n" TEXT+
   S\" package VBT-PB\npublic\nEXPORT VBT-PA:X\n;package\n" TEXT+
   S\" package VBT-PA\npublic\nundefine X\n: X ( n -- n ) 2 + ;\n;package\n" TEXT+
   S\" 1 VBT-PB:X drop\n" TEXT+
   s" design/redefine-source.f" WRITE-TEXT ;

: LOCAL-FILE ( -- )
   S\" : MM ( n -- n ) ;\n" TEXT+
   S\" : EXTRUDE ( ptr u8 n ptr u8 n ptr u8 n n -- ) 2drop 2drop 2drop drop ;\n" TEXT+
   S\" s\" a\" s\" b\" s\" c\" 5000 MM EXTRUDE\n" TEXT+
   s" design/local.f" WRITE-TEXT ;

: ENTRY-TEXT ( -- )
   S\" using VBT-VOCAB  s\" subj\" s\" b\" s\" t\" 5 MM EXTRUDE ;using\n" TEXT+
   S\" include design/plate.f\ninclude design/alias.f\ninclude design/chain.f\n" TEXT+
   S\" include design/twice.f\ninclude design/twice.f\n" TEXT+
   S\" include design/redefine.f\ninclude design/local.f\n" TEXT+
   S\" include design/redefine-source.f\n" TEXT+
   S\" ENGINE-INTERNAL:IMAGE-SEALED drop\n" TEXT+
   ROOT$ s" entry.f" SOURCE-ROOT:JOIN ENTRY ENTRY-U COPY!
   ENTRY$ TEXT$ WRITE-ALL ;

: PREP ( -- )
   CLEANUP-RESET
   s" verify-binding" HB-TMP-MKDIR SOURCE-ROOT:CANONICAL TTRUE
   ROOT ROOT-U COPY!
   ROOT$ CLEANUP-TREE+
   ROOT$ s" design" SOURCE-ROOT:JOIN MAKE-DIRS
   TEXT-U BUF-RESET
   PLATE-FILE ALIAS-FILE CHAIN-FILE TWICE-FILE REDEFINE-FILE LOCAL-FILE
   REDEFINE-SOURCE-FILE
   ENTRY-TEXT ;

\ Each binding the composition reports, one line: the file below the root, the
\ use's start and end there, the symbol's identity and that of the word it is,
\ each as package:tail/visibility. The log opens with a newline, so a whole
\ line is found as newline, line, newline.
16384 constant LOG-CAP
LOG-CAP BUFFER: LOG
TYPED-VARIABLE LOG-U len

: LOG$ ( -- ptr u8 n ) LOG LOG-U BUF-LEN@ ;

: LOG-RESET ( -- )
   LOG-U BUF-RESET  10 LOG LOG-CAP LOG-U BUF-APPEND-C ;

: REL$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   a u ROOT$ STARTS-WITH? 0= IF a u EXIT THEN
   a ROOT-U @ 1 + +  u ROOT-U @ 1 + - ;

: ID+ ( n -- )
   VERIFY:SYM-IDENTITY {: pa:ptr pu:n na:ptr nu:n vis:n :}
   pa pu SB-APPEND  58 SB-APPEND-C  na nu SB-APPEND  47 SB-APPEND-C
   vis FMT:SB-U ;

: NOTE ( n n n -- ) {: s:n e:n sym:n :}
   SB-RESET
   VERIFY:FILE$ REL$ SB-APPEND  STR-SPACE SB-APPEND-C
   s FMT:SB-U  STR-SPACE SB-APPEND-C
   e FMT:SB-U  STR-SPACE SB-APPEND-C
   sym ID+  STR-SPACE SB-APPEND-C
   sym VERIFY:SYM-SOURCE ID+  10 SB-APPEND-C
   SB$ LOG LOG-CAP LOG-U BUF-APPEND ;

: SILENT ( n n n -- ) 2drop drop ;

: COMPOSE ( -- n )
   LOG-RESET
   ['] NOTE is VERIFY:ON-BINDING
   CHECKER-SCOPE-START-NEUTRAL
   [: TEXT$ ENTRY$ VERIFY:SOURCE-COMPOSE-QUIET-IN-SCOPE ;] catch
   CHECKER-SCOPE-DONE
   ['] SILENT is VERIFY:ON-BINDING ;

\ Where the line at A U next starts in the log at or after AT; -1 past the last.
: LINE-AT ( n ptr u8 n -- n ) {: at:n a:ptr u:n :}
   LOG at +  LOG-U BUF-LEN@ at -  a u FIND-SUB
   MATCH option
      some OF {: off:idx :} off IDX>N at + ENDOF
      none OF -1 ENDOF
   ;MATCH ;

\ How many lines of the log are the line A U spells as newline, line, newline.
: LINES ( ptr u8 n -- n ) {: a:ptr u:n :}
   0 0
   begin a u LINE-AT dup 0 >= while
      1 + swap 1 + swap
   repeat
   drop ;

public

: MAIN ( -- )
   T-RESET
   PREP
   s" the design composes in full" T-LABEL
   ROOT$ [: COMPOSE 0 T= VERIFY:DEFERRED? TFALSE ;] SOURCE-ROOT:WITH
   s" the subject's uses of words with no location" T-LABEL
   S\" \nentry.f 40 42 vbt-vocab:mm/2 vbt-vocab:mm/2\n" LINES 1 T=
   S\" \nentry.f 43 50 vbt-vocab:extrude/2 vbt-vocab:extrude/2\n" LINES 1 T=
   s" a loaded file's uses, in a colon body and at top level" T-LABEL
   S\" \ndesign/plate.f 43 55 vbt-vocab:mm/2 vbt-vocab:mm/2\n" LINES 1 T=
   S\" \ndesign/plate.f 56 73 vbt-vocab:extrude/2 vbt-vocab:extrude/2\n" LINES 1 T=
   S\" \ndesign/plate.f 100 112 vbt-vocab:mm/2 vbt-vocab:mm/2\n" LINES 1 T=
   S\" \ndesign/plate.f 113 130 vbt-vocab:extrude/2 vbt-vocab:extrude/2\n" LINES 1 T=
   s" an export and an export of it are the word they export" T-LABEL
   S\" \ndesign/alias.f 79 96 vbt-alias:extrude/2 vbt-vocab:extrude/2\n" LINES 1 T=
   S\" \ndesign/chain.f 80 98 vbt-alias2:extrude/2 vbt-vocab:extrude/2\n" LINES 1 T=
   s" a name undefined and defined afresh is its own word" T-LABEL
   S\" \ndesign/redefine.f 79 96 vbt-redef:extrude/2 vbt-vocab:extrude/2\n" LINES 1 T=
   S\" \ndesign/redefine.f 239 256 vbt-redef:extrude/2 vbt-redef:extrude/2\n" LINES 1 T=
   s" an export whose word was undefined is its own word" T-LABEL
   S\" \ndesign/redefine-source.f 164 172 vbt-pb:x/2 vbt-pb:x/2\n" LINES 1 T=
   s" design-local words of the same spelling" T-LABEL
   S\" \ndesign/local.f 112 114 :mm/0 :mm/0\n" LINES 1 T=
   S\" \ndesign/local.f 115 122 :extrude/0 :extrude/0\n" LINES 1 T=
   LOG$ S\" \ndesign/local.f 112 114 vbt-vocab:" CONTAINS? TFALSE
   LOG$ S\" \ndesign/local.f 115 122 vbt-vocab:" CONTAINS? TFALSE
   s" a file read twice is reported for each read" T-LABEL
   S\" \ndesign/twice.f 20 32 vbt-vocab:mm/2 vbt-vocab:mm/2\n" LINES 2 T=
   S\" \ndesign/twice.f 33 50 vbt-vocab:extrude/2 vbt-vocab:extrude/2\n" LINES 2 T=
   \ The one export bin/hb bakes, src/core/internal-mark.f, exports a private
   \ constant: its source is checked to be another word, not by name.
   s" an export the engine bakes keeps its source" T-LABEL
   LOG$ S\" \nentry.f 255 283 engine-internal:image-sealed/2 " CONTAINS? TTRUE
   LOG$ S\" \nentry.f 255 283 engine-internal:image-sealed/2 engine-internal:image-sealed/2\n"
   CONTAINS? TFALSE
   CLEANUP-RUN
   T-REPORT ;

;package

VERIFY-BINDING-TEST:MAIN
