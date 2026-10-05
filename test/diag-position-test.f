\ diag-position-test.f - a checker packet locates its token in the checked file.
\
\ `tools/check.f --json-errors` writes a packet for each definition and
\ declaration its pre-passes refuse, and an editor puts the diagnostic where the
\ packet says: byte_start and byte_end are 0-based offsets in the checked file,
\ end exclusive; line and column are 1-based, the column counted in bytes, and
\ LF ends a line. Each case writes a fixture, runs the checker on it, and
\ asserts the four fields, the file label, and that the fixture's bytes in
\ [byte_start, byte_end) are the packet's token, or the name as written where
\ the packet names its definition by the checker's fold: a body over two
\ lines, a tab,
\ runs of spaces, CR LF, comments, strings, multibyte UTF-8, a signature on its
\ own line, a definition after others, every packet of an --all-errors run, a
\ bare name a global and a used public share, called and as the target of `[']`
\ and of `is`, a public word whose private twin moves other cells and a
\ definition whose inferred effect is not recorded, both at the definition's
\ name, a refused `generates:` row at its name, and one whose name a global
\ and a used public share, and declaration packets: SUMTYPE, NEWTYPE with
\ no arity (at its family name), ENUM (a body token, and a close-stage
\ fault at the family name) and STRUCTURE. The check composes the subject
\ with the files it requires, so a packet in a required file names that
\ file and locates there, after a file it requires in turn has returned,
\ and a packet in the subject after a required file returns locates in
\ the subject: plain, under --all-errors and under --verify-only, the
\ check a language server makes.
\ The child engine is HABU_UNDER_TEST when the gate sets it, else bin/hb.
\
\ Run: bin/hb --load test/diag-position-test.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require tools/json.f
require tools/gate-json-assert-core.f

package DIAG-POSITION-TEST

$1000 constant CAP
120000 constant TIMEOUT-MS
70 constant REJECT-RC
67 constant THROW-RC                  \ a load's uncaught throw

create OUT CAP allot
create ERR CAP allot
create FX CAP allot
create EMPTY 1 allot
create BASE FS-PATH-CAP allot
create FX-P FS-PATH-CAP allot
create DEP CAP allot
create DEP-P FS-PATH-CAP allot
create CANON FS-PATH-CAP allot

variable BASE-U
variable FX-U
variable FX-PU
variable DEP-U
variable DEP-PU
variable ERR-U
variable RC

: BASE$ ( -- ptr u8 n )
   BASE BASE-U @ ;

: FX$ ( -- ptr u8 n )
   FX FX-U @ ;

: FX-PATH$ ( -- ptr u8 n )
   FX-P FX-PU @ ;

: DEP$ ( -- ptr u8 n )
   DEP DEP-U @ ;

: DEP-PATH$ ( -- ptr u8 n )
   DEP-P DEP-PU @ ;

: ERR$ ( -- ptr u8 n )
   ERR ERR-U @ ;

: HB$ ( -- ptr u8 n )
   s" HABU_UNDER_TEST" >LEN PROC-ENV-DEFAULT$? if LEN>N exit then
   2drop
   s" HABU_UNDER_TEST" GETENV dup 0= if
      2drop s" bin/hb" exit
   then ;

: ROOT! ( -- )
   CLEANUP-RESET
   s" habu-diag-position" HB-TMP-MKDIR
   {: a:ptr u:n :}
   a BASE u BYTE-COPY
   u BASE-U !
   BASE$ CLEANUP-TREE+ ;

: LF+ ( -- ) 10 SB-APPEND-C ;
: CR+ ( -- ) 13 SB-APPEND-C ;
: TAB+ ( -- ) 9 SB-APPEND-C ;

\ The string builder's text becomes fixture NAME; a copy stays to cut the
\ packets' byte spans from.
: FIXTURE! ( ptr u8 n -- )
   {: name:ptr nameu:n :}
   BASE$ name nameu FX-P JOIN-PATH FX-PU !
   SB$
   {: a:ptr u:n :}
   a FX u BYTE-COPY
   u FX-U !
   FX-PATH$ FX$ WRITE-ALL ;

\ PATH with every link resolved: the checker names a required file so, and
\ --verify-only its subject too.
: CANON$ ( ptr u8 n -- ptr u8 n )
   FS-PATHZ CANON FS-PATH-CAP realpath {: n:n :}
   n 0 <= IF E-FS-PATH throw THEN
   CANON n ;

\ The fixture becomes the required file later packets name.
: DEP! ( -- )
   FX DEP FX-U @ BYTE-COPY
   FX-U @ DEP-U !
   FX-PATH$ CANON$ {: a:ptr u:n :}
   a DEP-P u BYTE-COPY
   u DEP-PU ! ;

: STORE! ( len len outcome -- )
   MATCH outcome
     exited OF RC ! ENDOF
     signaled OF drop -1 RC ! ENDOF
     timeout OF -1 RC ! ENDOF
   ;MATCH
   LEN>N ERR-U !
   LEN>N drop ;

\ Run the checker on the fixture, FLAG (when not empty) before --json-errors,
\ and split its packets into lines; the checker exits WANT.
: CHECK-EXIT ( ptr u8 n n -- )
   {: flag:ptr flagu:n want:n :}
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   s" tools/check.f" >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   flagu 0 > IF flag flagu >LEN PROC-ARGV+ THEN
   s" --json-errors" >LEN PROC-ARGV+
   FX-PATH$ >LEN PROC-ARGV+
   HB$ >LEN  EMPTY 0 >LEN  OUT CAP >LEN
   ERR CAP >LEN  TIMEOUT-MS >MS  RUN-ARGV-STDIN-CAPTURE-OUTCOME
   STORE!
   RC @ want T=
   ERR$ GJA-SPLIT-LINES ;

: CHECK ( ptr u8 n -- )
   REJECT-RC CHECK-EXIT ;

: INT-FIELD ( n ptr u8 n -- n ) \ a packet's integer field, -1 when absent
   JSON-GET dup -1 = IF EXIT THEN
   GJA-INT ;

\ Packet k of the last run has the string WANT for KEY.
: FIELD= ( n ptr u8 n ptr u8 n -- )
   {: k:n key:ptr keyu:n want:ptr wantu:n :}
   GJA-LINE# @ k > dup TTRUE 0= IF EXIT THEN
   k GJA-LINE$ JSON-PARSE key keyu GJA-REQ JSON-STRING$ want wantu T$= ;

\ Packet k of the last run names file PATH, whose bytes are TEXT, at LINE, COL
\ and bytes [BS, BE), and those bytes of TEXT are SPELL.
: PLACED-IN ( n ptr u8 n n n n n ptr u8 n ptr u8 n -- )
   {: k:n sp:ptr spu:n line:n col:n bs:n be:n path:ptr pathu:n text:ptr textu:n :}
   GJA-LINE# @ k > dup TTRUE 0= IF EXIT THEN
   k GJA-LINE$ JSON-PARSE
   {: root:n :}
   root s" file" GJA-REQ JSON-STRING$ path pathu T$=
   root s" line" INT-FIELD line T=
   root s" column" INT-FIELD col T=
   root s" byte_start" INT-FIELD bs T=
   root s" byte_end" INT-FIELD be T=
   bs 0 >=  be bs >= and  be textu <= and IF
      text bs + be bs - sp spu T$=
   ELSE
      T-FAIL
   THEN ;

\ PLACED-IN, and the packet's token is the bytes it locates.
: AT-IN ( n ptr u8 n n n n n ptr u8 n ptr u8 n -- )
   {: k:n tok:ptr toku:n line:n col:n bs:n be:n path:ptr pathu:n text:ptr textu:n :}
   k tok toku line col bs be path pathu text textu PLACED-IN
   k s" token" tok toku FIELD= ;

\ AT-IN for the fixture.
: AT ( n ptr u8 n n n n n -- )
   FX-PATH$ FX$ AT-IN ;

\ AT-IN for the required file DEP! kept.
: DEP-AT ( n ptr u8 n n n n n -- )
   DEP-PATH$ DEP$ AT-IN ;

: TEST-LINES ( -- )
   s" a body on the line after its name" T-LABEL
   SB-RESET
   s" : H5 ( n -- n )" SB-APPEND LF+
   s"    drop ;" SB-APPEND LF+
   s" lines.f" FIXTURE!
   s" " CHECK
   0 s" drop" 2 4 19 23 AT ;

: TEST-TAB ( -- )
   s" a tab counts one column" T-LABEL
   SB-RESET
   s" : T1 ( n -- n )" SB-APPEND LF+
   TAB+ s"   drop ;" SB-APPEND LF+
   s" tab.f" FIXTURE!
   s" " CHECK
   0 s" drop" 2 4 19 23 AT ;

: TEST-CRLF ( -- )
   s" CR LF ends a line at its LF" T-LABEL
   SB-RESET
   s" : C1 ( n -- n )" SB-APPEND CR+ LF+
   s"    drop ;" SB-APPEND CR+ LF+
   s" crlf.f" FIXTURE!
   s" " CHECK
   0 s" drop" 2 4 20 24 AT ;

: TEST-SPACES ( -- )
   s" runs of spaces between tokens" T-LABEL
   SB-RESET
   s" :   W1    ( -- )      1     NOPE-W1   drop ;" SB-APPEND LF+
   s" spaces.f" FIXTURE!
   s" " CHECK
   0 s" NOPE-W1" 1 29 28 35 AT ;

: TEST-COMMENTS ( -- )
   s" paren and backslash comments in the body" T-LABEL
   SB-RESET
   s" : K1 ( n -- n ) ( a comment ) " SB-APPEND
   92 SB-APPEND-C s"  line comment" SB-APPEND LF+
   s"   1 + " SB-APPEND 92 SB-APPEND-C s"  another" SB-APPEND LF+
   s"   ( more ) NOPE-K1 ;" SB-APPEND LF+
   s" comments.f" FIXTURE!
   s" " CHECK
   0 s" NOPE-K1" 3 12 72 79 AT ;

: TEST-STRING ( -- )
   s" a string with runs of spaces before the token" T-LABEL
   SB-RESET
   s" : Q1 ( -- ) s" SB-APPEND 34 SB-APPEND-C
   s"   two  spaces " SB-APPEND 34 SB-APPEND-C s"  2drop" SB-APPEND LF+
   s"    NOPE-Q1 ;" SB-APPEND LF+
   s" string.f" FIXTURE!
   s" " CHECK
   0 s" NOPE-Q1" 2 4 39 46 AT ;

\ The string opener is followed by one space and a four-byte character.
: TEST-EMOJI-STRING ( -- )
   s" a multibyte character inside a string" T-LABEL
   SB-RESET
   s" : H4 ( n -- n ) s" SB-APPEND 34 SB-APPEND-C
   s"  " SB-APPEND $F0 SB-APPEND-C $9F SB-APPEND-C $98 SB-APPEND-C $80 SB-APPEND-C
   34 SB-APPEND-C s"  2drop drop ;" SB-APPEND LF+
   s" emoji.f" FIXTURE!
   s" " CHECK
   0 s" drop" 1 32 31 35 AT ;

\ Line 1 is a comment holding two- and four-byte characters; line 2 has a
\ two-byte character in a string and in the refused token itself.
: TEST-UTF8 ( -- )
   s" multibyte characters before and inside the token" T-LABEL
   SB-RESET
   92 SB-APPEND-C s"  caf" SB-APPEND $C3 SB-APPEND-C $A9 SB-APPEND-C
   s"  " SB-APPEND $F0 SB-APPEND-C $9F SB-APPEND-C $98 SB-APPEND-C $80 SB-APPEND-C LF+
   s" : U8 ( -- ) s" SB-APPEND 34 SB-APPEND-C
   s"  " SB-APPEND $C3 SB-APPEND-C $A9 SB-APPEND-C s" t" SB-APPEND
   $C3 SB-APPEND-C $A9 SB-APPEND-C 34 SB-APPEND-C
   s"  2drop NOPE-" SB-APPEND $C3 SB-APPEND-C $A9 SB-APPEND-C s"  ;" SB-APPEND LF+
   s" utf8.f" FIXTURE!
   s" " CHECK
   SB-RESET s" NOPE-" SB-APPEND $C3 SB-APPEND-C $A9 SB-APPEND-C
   0 SB$ 2 29 41 48 AT ;

: TEST-ONE-LINE ( -- )
   s" a one-line definition" T-LABEL
   SB-RESET
   s" : F ( n -- n ) drop ;" SB-APPEND LF+
   s" one-line.f" FIXTURE!
   s" " CHECK
   0 s" drop" 1 16 15 19 AT ;

: TEST-PAREN-EMOJI ( -- )
   s" a multibyte character in a comment before the name" T-LABEL
   SB-RESET
   s" ( " SB-APPEND $F0 SB-APPEND-C $9F SB-APPEND-C $98 SB-APPEND-C $80 SB-APPEND-C
   s"  ) : H ( n -- n ) drop ;" SB-APPEND LF+
   s" paren-emoji.f" FIXTURE!
   s" " CHECK
   0 s" drop" 1 25 24 28 AT ;

: TEST-SIGNATURE-LINE ( -- )
   s" a signature on the line after its name" T-LABEL
   SB-RESET
   s" : S3" SB-APPEND LF+
   s"    ( n -- mystery ) ;" SB-APPEND LF+
   s" signature.f" FIXTURE!
   s" " CHECK
   0 s" mystery" 2 11 15 22 AT ;

: TEST-LATER ( -- )
   s" a definition after two that certify" T-LABEL
   SB-RESET
   s" : OK1 ( n -- n ) 1 + ;" SB-APPEND LF+ LF+
   s" : OK2 ( n -- n )" SB-APPEND LF+
   s"    2 * ;" SB-APPEND LF+
   s" : LATE ( -- )" SB-APPEND LF+
   s"    s" SB-APPEND 34 SB-APPEND-C s"  a b" SB-APPEND 34 SB-APPEND-C
   s"  2drop" SB-APPEND LF+
   s"    NOPE-LATE ;" SB-APPEND LF+
   s" later.f" FIXTURE!
   s" " CHECK
   0 s" NOPE-LATE" 7 4 84 93 AT ;

\ SHADE, which package P publishes and the global wordlist defines too, each
\ by the line DECL, and on line 8 the body BODY of a definition under `using P`.
: SHADOW-FIXTURE ( ptr u8 n ptr u8 n -- )
   {: decl:ptr declu:n body:ptr bodyu:n :}
   SB-RESET
   s" package P" SB-APPEND LF+
   s" public" SB-APPEND LF+
   decl declu SB-APPEND LF+
   s" ;package" SB-APPEND LF+
   decl declu SB-APPEND LF+
   s" using P" SB-APPEND LF+
   s" : USE-IT ( -- )" SB-APPEND LF+
   body bodyu SB-APPEND LF+
   s" ;using" SB-APPEND LF+ ;

\ The checker resolves the token's lowercase fold; the packet names the file's
\ spelling where it lies.
: TEST-SHADOW ( -- )
   s" a bare name a global and a used public share" T-LABEL
   s" : SHADE ( -- ) ;" s"    SHADE ;" SHADOW-FIXTURE
   s" shadow.f" FIXTURE!
   s" " REJECT-RC CHECK-EXIT
   ERR$ s" E-STATEMENT-THROW" CONTAINS? TFALSE
   0 s" SHADE" 8 4 87 92 AT
   s" --all-errors" REJECT-RC CHECK-EXIT
   ERR$ s" E-STATEMENT-THROW" CONTAINS? TFALSE
   0 s" SHADE" 8 4 87 92 AT ;

\ `[']` and `is` read their target themselves, past the body walk's own token.
: TEST-TICK-SHADOW ( -- )
   s" that name as a ['] target" T-LABEL
   s" : SHADE ( -- ) ;" s"    ['] SHADE drop ;" SHADOW-FIXTURE
   s" tick-shadow.f" FIXTURE!
   s" " REJECT-RC CHECK-EXIT
   ERR$ s" E-STATEMENT-THROW" CONTAINS? TFALSE
   0 s" SHADE" 8 8 91 96 AT ;

: TEST-IS-SHADOW ( -- )
   s" that name as an is target" T-LABEL
   s" defer SHADE ( -- )" s"    [: ;] is SHADE ;" SHADOW-FIXTURE
   s" is-shadow.f" FIXTURE!
   s" " REJECT-RC CHECK-EXIT
   ERR$ s" E-STATEMENT-THROW" CONTAINS? TFALSE
   0 s" SHADE" 8 13 100 105 AT ;

\ The public F moves other cells than the private F, whose tail it shares: the
\ refusal names the folded tail and is placed at the public definition's name.
: SHADOWED-FIXTURE ( -- )
   SB-RESET
   s" package SAT-A" SB-APPEND LF+
   s" : F ( n n -- n ) + ;" SB-APPEND LF+
   s" public" SB-APPEND LF+ LF+
   s" :   F ( n -- n )" SB-APPEND LF+
   s"    1 + ;" SB-APPEND LF+
   s" ;package" SB-APPEND LF+
   s" shadowed.f" FIXTURE! ;

\ Packet 0 is the refusal of the public F.
: SHADOWED-AT ( ptr u8 n -- )
   {: path:ptr pathu:n :}
   0 s" code" s" E-SHADOWED-ARITY" FIELD=
   0 s" token" s" f" FIELD=
   0 s" F" 5 5 47 48 path pathu FX$ PLACED-IN ;

: TEST-SHADOWED-ARITY ( -- )
   s" a public whose private twin moves other cells" T-LABEL
   SHADOWED-FIXTURE
   s" " REJECT-RC CHECK-EXIT
   FX-PATH$ SHADOWED-AT
   s" --all-errors" REJECT-RC CHECK-EXIT
   FX-PATH$ SHADOWED-AT
   s" --verify-only" CHECK
   GJA-LINE# @ 1 T=
   FX-PATH$ CANON$ SHADOWED-AT ;

\ WIDE's inferred effect has more type variables than a record holds: the
\ checker certifies it and warns, at its name, that the effect is not recorded.
: NOT-RECORDED-TEXT ( -- )
   SB-RESET
   s" defer V14 ( -- a b c d e g h i j k l m o p )" SB-APPEND LF+ LF+
   s" :   WIDE" SB-APPEND LF+
   s"    V14 V14 ;" SB-APPEND LF+ ;

\ The last check's first line is the warning about WIDE, in the file at PATH
\ and placed at its name, and no other line warns.
: NOT-RECORDED ( ptr u8 n -- )
   {: path:ptr pathu:n :}
   0 s" code" s" W-EFFECT-NOT-RECORDED" FIELD=
   0 s" word" s" wide" FIELD=
   0 s" WIDE" 3 5 50 54 path pathu FX$ PLACED-IN
   1 begin dup GJA-LINE# @ < while
      dup GJA-LINE$ s" W-EFFECT-NOT-RECORDED" CONTAINS? TFALSE
      1 +
   repeat drop ;

\ The check before the run writes the warning; the run, which loads what that
\ check checked, does not repeat it, nor does a run that then fails.
: TEST-NOT-RECORDED ( -- )
   s" an inferred effect not recorded" T-LABEL
   NOT-RECORDED-TEXT
   s" not-recorded.f" FIXTURE!
   s" " 0 CHECK-EXIT
   FX-PATH$ NOT-RECORDED
   s" --all-errors" 0 CHECK-EXIT
   FX-PATH$ NOT-RECORDED
   s" --verify-only" 0 CHECK-EXIT
   FX-PATH$ CANON$ NOT-RECORDED
   NOT-RECORDED-TEXT
   s" -1 throw" SB-APPEND LF+
   s" not-recorded-throw.f" FIXTURE!
   s" " THROW-RC CHECK-EXIT
   FX-PATH$ NOT-RECORDED
   s" --all-errors" THROW-RC CHECK-EXIT
   FX-PATH$ NOT-RECORDED ;

\ A refused `generates:` row is the verifier's own packet, at the row's name
\ past a comment line and a run of spaces: the check exits as the engine's
\ uncaught throw does and adds no statement-throw record, and --all-errors and
\ --verify-only count the refusal and go on.
: TEST-GENERATES ( -- )
   s" a generates: row naming no word" T-LABEL
   SB-RESET
   s" : OK1 ( n -- n )" SB-APPEND LF+
   s"    1 + ;" SB-APPEND LF+
   92 SB-APPEND-C s"  a row for a word nothing defines" SB-APPEND LF+
   s" generates:  NOWHERE ( -- n )" SB-APPEND LF+
   s" generates.f" FIXTURE!
   s" " THROW-RC CHECK-EXIT
   ERR$ s" E-STATEMENT-THROW" CONTAINS? TFALSE
   0 s" NOWHERE" 4 13 73 80 AT
   s" --all-errors" CHECK
   GJA-LINE# @ 1 T=
   0 s" NOWHERE" 4 13 73 80 AT
   s" --verify-only" CHECK
   0 s" NOWHERE" 4 13 73 80 FX-PATH$ CANON$ FX$ AT-IN ;

\ The row's registrar resolves its name as a use does.
: TEST-GENERATES-SHADOW ( -- )
   s" a generates: row naming that shared name" T-LABEL
   SB-RESET
   s" : SHADE ( -- ) ;" SB-APPEND LF+
   s" package P" SB-APPEND LF+
   s" public" SB-APPEND LF+
   s" : SHADE ( -- ) ;" SB-APPEND LF+
   s" ;package" SB-APPEND LF+
   s" using P" SB-APPEND LF+
   s" generates: SHADE ( -- n )" SB-APPEND LF+
   s" ;using" SB-APPEND LF+
   s" generates-shadow.f" FIXTURE!
   s" " THROW-RC CHECK-EXIT
   ERR$ s" E-STATEMENT-THROW" CONTAINS? TFALSE
   0 s" SHADE" 7 12 79 84 AT ;

: TEST-ALL-ERRORS ( -- )
   s" every packet of an --all-errors run" T-LABEL
   SB-RESET
   s" : A1 ( n -- n ) drop ;" SB-APPEND LF+
   s" : A2 ( n -- n )" SB-APPEND LF+
   s"    drop ;" SB-APPEND LF+
   s" all-errors.f" FIXTURE!
   s" --all-errors" CHECK
   GJA-LINE# @ 2 T=
   0 s" drop" 1 17 16 20 AT
   1 s" drop" 3 4 42 46 AT ;

\ compose-base.f certifies. It is longer than the files that require it and its
\ lines are short, so a packet located in its bytes by mistake lands on another
\ line and column than its own.
: BASE-FIXTURE ( -- )
   SB-RESET
   s" : BASE-ONE ( n -- n )" SB-APPEND LF+
   7 0 ?do s"    1 +" SB-APPEND LF+ loop
   s"    1 + ;" SB-APPEND LF+
   s" compose-base.f" FIXTURE! ;

\ compose-dep.f requires compose-base.f, then refuses `drop` on line 4; the
\ subject compose.f requires compose-dep.f, then refuses `drop` on line 3.
: COMPOSE-FIXTURE ( -- )
   BASE-FIXTURE
   SB-RESET
   s" require compose-base.f" SB-APPEND LF+ LF+
   s" : DEP-BAD ( n -- n )" SB-APPEND LF+
   s"    drop ;" SB-APPEND LF+
   s" compose-dep.f" FIXTURE!
   DEP!
   SB-RESET
   s" require compose-dep.f" SB-APPEND LF+
   s" : PARENT-BAD ( n -- n )" SB-APPEND LF+
   s"    drop ;" SB-APPEND LF+
   s" compose.f" FIXTURE! ;

: TEST-COMPOSE-DEP ( -- )
   s" a packet in a required file" T-LABEL
   COMPOSE-FIXTURE
   s" " CHECK
   GJA-LINE# @ 1 T=
   0 s" drop" 4 4 48 52 DEP-AT ;

: TEST-COMPOSE-AFTER ( -- )
   s" a packet in the subject after a required file returns" T-LABEL
   BASE-FIXTURE
   SB-RESET
   s" require compose-base.f" SB-APPEND LF+
   s" : AFTER-BAD ( n -- n )" SB-APPEND LF+
   s"    drop ;" SB-APPEND LF+
   s" compose-after.f" FIXTURE!
   s" " CHECK
   GJA-LINE# @ 1 T=
   0 s" drop" 3 4 49 53 AT ;

: TEST-COMPOSE-ALL ( -- )
   s" --all-errors past a required file's refusal to the subject's" T-LABEL
   COMPOSE-FIXTURE
   s" --all-errors" CHECK
   GJA-LINE# @ 2 T=
   0 s" drop" 4 4 48 52 DEP-AT
   1 s" drop" 3 4 49 53 AT ;

\ The check a language server makes goes on past a refusal, as --all-errors
\ does.
: TEST-COMPOSE-VERIFY ( -- )
   s" --verify-only, the check a language server makes" T-LABEL
   COMPOSE-FIXTURE
   s" --verify-only" CHECK
   GJA-LINE# @ 2 T=
   0 s" drop" 4 4 48 52 DEP-AT
   1 s" drop" 3 4 49 53 FX-PATH$ CANON$ FX$ AT-IN ;

: TEST-DECLARATION ( -- )
   s" a declaration packet" T-LABEL
   SB-RESET
   s" SUMTYPE bad-sum" SB-APPEND LF+
   s"    VARIANT Up" SB-APPEND LF+
   s" ;SUMTYPE" SB-APPEND LF+
   s" declaration.f" FIXTURE!
   s" " CHECK
   0 s" VARIANT" 2 4 19 26 AT ;

\ A missing arity has no token, so the family name stands for it.
: TEST-NEWTYPE ( -- )
   s" a NEWTYPE packet at its family name" T-LABEL
   SB-RESET
   s" : N1 ( -- ) ;" SB-APPEND LF+
   s" NEWTYPE plain" SB-APPEND LF+
   s" newtype.f" FIXTURE!
   s" " CHECK
   0 s" plain" 2 9 22 27 AT ;

: TEST-ENUM ( -- )
   s" an ENUM declaration packet" T-LABEL
   SB-RESET
   s" ENUM colour" SB-APPEND LF+
   s"    red red ;ENUM" SB-APPEND LF+
   s" enum.f" FIXTURE!
   s" " CHECK
   0 s" red" 2 8 19 22 AT ;

\ A close-stage fault names the family, read before the body.
: TEST-ENUM-CLOSE ( -- )
   s" an ENUM packet at its family name" T-LABEL
   SB-RESET
   s" ENUM shade light dark ;ENUM" SB-APPEND LF+
   s" ENUM colour" SB-APPEND LF+
   s" ;ENUM" SB-APPEND LF+
   s" enum-close.f" FIXTURE!
   s" " CHECK
   0 s" colour" 2 6 33 39 AT ;

: TEST-STRUCTURE ( -- )
   s" a STRUCTURE declaration packet" T-LABEL
   SB-RESET
   s" STRUCTURE pair 0" SB-APPEND LF+
   s"    FIELD size n" SB-APPEND LF+
   s"    FIELD size n" SB-APPEND LF+
   s" ;STRUCTURE" SB-APPEND LF+
   s" structure.f" FIXTURE!
   s" " CHECK
   0 s" size" 3 10 42 46 AT ;

\ The one-line fixture NAME holding TEXT.
: LINE-FIXTURE ( ptr u8 n ptr u8 n -- )
   {: text:ptr textu:n name:ptr nameu:n :}
   SB-RESET
   text textu SB-APPEND LF+
   name nameu FIXTURE! ;

\ The check gave one packet, a rejection named CODE.
: REJECTED-AS ( ptr u8 n -- )
   {: c:ptr cu:n :}
   GJA-LINE# @ 1 T=
   0 s" code" c cu FIELD=
   0 s" verdict" s" rejected" FIELD= ;

\ Each check of the fixture, plain, --all-errors and --verify-only (which names
\ it by its resolved path), gives one packet, a rejection: CODE at TOK, on LINE
\ at COL, in bytes [BS, BE).
: REFUSED-AT ( ptr u8 n ptr u8 n n n n n -- )
   {: c:ptr cu:n tok:ptr toku:n line:n col:n bs:n be:n :}
   s" " CHECK
   c cu REJECTED-AS
   0 tok toku line col bs be AT
   s" --all-errors" CHECK
   c cu REJECTED-AS
   0 tok toku line col bs be AT
   s" --verify-only" CHECK
   c cu REJECTED-AS
   0 tok toku line col bs be FX-PATH$ CANON$ FX$ AT-IN ;

\ The load compiles a body before it checks the definition at `;`, so a body
\ word it cannot compile, undefined or a local it cannot bind, refuses a
\ definition whose signature does not parse, at that word; any other error in
\ the body leaves the refusal to the signature, at its type.
: TEST-BAD-SIGNATURE ( -- )
   s" a signature that does not parse and an undefined word" T-LABEL
   s" : X ( n -- zz ) NOPE ;" s" badsig-undefined.f" LINE-FIXTURE
   s" E-UNDEFINED" s" NOPE" 1 17 16 20 REFUSED-AT
   s" that signature alone" T-LABEL
   s" : X ( n -- zz ) 1 ;" s" badsig.f" LINE-FIXTURE
   s" E-UNKNOWN-SIGNATURE-TYPE" s" zz" 1 12 11 13 REFUSED-AT
   s" that undefined word alone" T-LABEL
   s" : X ( n -- n ) NOPE ;" s" undefined.f" LINE-FIXTURE
   s" E-UNDEFINED" s" NOPE" 1 16 15 19 REFUSED-AT
   s" that signature and a local name over 16 bytes" T-LABEL
   s" : X ( n -- zz ) {: abcdefghijklmnopq :} 1 ;" s" badsig-local.f" LINE-FIXTURE
   s" E-LOCAL-NAME-TOO-LONG" s" abcdefghijklmnopq" 1 20 19 36 REFUSED-AT
   s" that signature and an unsafe word" T-LABEL
   s" : X ( n -- zz ) evaluate ;" s" badsig-unsafe.f" LINE-FIXTURE
   s" E-UNKNOWN-SIGNATURE-TYPE" s" zz" 1 12 11 13 REFUSED-AT ;

\ The same order without a signature fault: a body word the load cannot
\ compile refuses the definition at that word before any check made at `;`, and
\ otherwise the first token whose check fails does, its verdict included.
: TEST-REFUSAL-ORDER ( -- )
   s" an undefined word before a trust-boundary word" T-LABEL
   s" : X ( n -- n ) NOPE evaluate ;" s" undef-unsafe.f" LINE-FIXTURE
   s" E-UNDEFINED" s" NOPE" 1 16 15 19 REFUSED-AT
   s" an undefined word after a trust-boundary word" T-LABEL
   s" : X ( n -- n ) evaluate NOPE ;" s" unsafe-undef.f" LINE-FIXTURE
   s" E-UNDEFINED" s" NOPE" 1 25 24 28 REFUSED-AT
   s" a malformed qualified call after a trust-boundary word" T-LABEL
   s" : X ( n -- n ) evaluate A:B:C ;" s" unsafe-qual.f" LINE-FIXTURE
   s" E-BAD-QUALIFIED" s" A:B:C" 1 25 24 29 REFUSED-AT
   s" a trust-boundary word before a TRUSTED-only primitive" T-LABEL
   s" : X ( n -- n ) evaluate 1 c2-invoke ;" s" unsafe-cap.f" LINE-FIXTURE
   s" E-UNSAFE" s" evaluate" 1 16 15 23 REFUSED-AT
   s" an unknown construct variant after a trust-boundary word" T-LABEL
   s" SUMTYPE rf 0 VARIANT ok n ;VARIANT ;SUMTYPE : X ( n -- rf ) evaluate construct rf nope ;"
      s" unsafe-variant.f" LINE-FIXTURE
   s" E-CONSTRUCT-UNKNOWN-VARIANT" s" nope" 1 83 82 86 REFUSED-AT
   s" a mismatch before a token of unknown effect" T-LABEL
   s" : X ( n -- n ) dup c@ {: a -- b :} a ;" s" mismatch-unck.f" LINE-FIXTURE
   s" E-MISMATCH" s" c@" 1 20 19 21 REFUSED-AT
   s" a signature that does not parse and a TRUSTED-only primitive" T-LABEL
   s" : X ( n -- zz ) 1 c2-invoke ;" s" badsig-cap.f" LINE-FIXTURE
   s" E-UNKNOWN-SIGNATURE-TYPE" s" zz" 1 12 11 13 REFUSED-AT ;

\ The last check's packet repairs its refusal as CLASS with the suggestion SUG.
: REPAIRED-AS ( ptr u8 n ptr u8 n -- )
   {: cls:ptr clsu:n sug:ptr sugu:n :}
   0 s" repair_class" cls clsu FIELD=
   0 s" suggestion" sug sugu FIELD= ;

\ The repair class and the suggestion name the refusal the code names: a
\ mismatch before an unbalanced return row is the mismatch's to repair.
: TEST-REPAIR-FOLLOWS ( -- )
   s" a mismatch before an unbalanced return row" T-LABEL
   s" : X ( n -- ) >r 1 c@ ;" s" mismatch-rstack.f" LINE-FIXTURE
   s" E-MISMATCH" s" c@" 1 19 18 20 REFUSED-AT
   s" remove_producer" s" Remove an extra producer or drop the surplus value." REPAIRED-AS
   s" that unbalanced return row alone" T-LABEL
   s" : X ( n -- ) >r ;" s" rstack.f" LINE-FIXTURE
   s" E-REJECTED" s" >r" 1 14 13 15 REFUSED-AT
   s" fix_return_stack" s" Balance return-stack transfers before the definition exits." REPAIRED-AS ;

\ A match whose scrutinee does not fit, and a match or case one of whose
\ branches fails, still has live words, in its branches and after it, that the
\ load compiles: the first undefined one refuses the definition.
: TEST-LIVE-BRANCHES ( -- )
   s" an undefined word in a match whose scrutinee does not fit" T-LABEL
   s" SUMTYPE mres 0 VARIANT ok n ;VARIANT ;SUMTYPE : X ( n -- n ) MATCH mres ok OF FIRST ENDOF ;MATCH SECOND ;"
      s" scrutinee-first.f" LINE-FIXTURE
   s" E-UNDEFINED" s" FIRST" 1 79 78 83 REFUSED-AT
   s" that word alone in the branch" T-LABEL
   s" SUMTYPE mres 0 VARIANT ok n ;VARIANT ;SUMTYPE : X ( n -- n ) MATCH mres ok OF NOPE ENDOF ;MATCH ;"
      s" scrutinee-branch.f" LINE-FIXTURE
   s" E-UNDEFINED" s" NOPE" 1 79 78 82 REFUSED-AT
   s" an undefined word after that match" T-LABEL
   s" SUMTYPE mres 0 VARIANT ok n ;VARIANT ;SUMTYPE : X ( n -- n ) MATCH mres ok OF 1 ENDOF ;MATCH SECOND ;"
      s" scrutinee-after.f" LINE-FIXTURE
   s" E-UNDEFINED" s" SECOND" 1 94 93 99 REFUSED-AT
   s" an undefined word after a match branch that fails" T-LABEL
   s" SUMTYPE mres 0 VARIANT ok n ;VARIANT ;SUMTYPE : X ( mres -- n ) MATCH mres ok OF 1 c@ ENDOF ;MATCH NOPE ;"
      s" match-branch-after.f" LINE-FIXTURE
   s" E-UNDEFINED" s" NOPE" 1 100 99 103 REFUSED-AT
   s" an undefined word after a case branch that fails" T-LABEL
   s" : X ( n -- n ) CASE 1 OF 1 c@ ENDOF ENDCASE NOPE ;" s" case-branch-after.f" LINE-FIXTURE
   s" E-UNDEFINED" s" NOPE" 1 45 44 48 REFUSED-AT ;

\ Each check of the fixture certifies it: rc 0 and no packet.
: CERTIFIED ( -- )
   s" " 0 CHECK-EXIT
   GJA-LINE# @ 0 T=
   s" --all-errors" 0 CHECK-EXIT
   GJA-LINE# @ 0 T=
   s" --verify-only" 0 CHECK-EXIT
   GJA-LINE# @ 0 T= ;

\ A word `[']` ticks that does not exist refuses the definition at that word,
\ as an undefined call does: before a signature that does not parse and
\ before an earlier failing check.
: TEST-TICK-TARGET ( -- )
   s" a signature that does not parse and an undefined ticked word" T-LABEL
   s" : X ( n -- zz ) ['] NOPE ;" s" badsig-tick.f" LINE-FIXTURE
   s" E-UNDEFINED" s" NOPE" 1 21 20 24 REFUSED-AT
   s" an undefined ticked word after a trust-boundary word" T-LABEL
   s" : X ( n -- n ) evaluate ['] NOPE ;" s" unsafe-tick.f" LINE-FIXTURE
   s" E-UNDEFINED" s" NOPE" 1 29 28 32 REFUSED-AT
   s" an undefined ticked word alone" T-LABEL
   s" : X ( -- n ) ['] NOPE ;" s" tick-undefined.f" LINE-FIXTURE
   s" E-UNDEFINED" s" NOPE" 1 18 17 21 REFUSED-AT
   s" a defined ticked word" T-LABEL
   s" : X ( -- n ) ['] dup ;" s" tick-defined.f" LINE-FIXTURE
   CERTIFIED ;

\ The fixture NAME holding the line TEXT after a create caller made MADE, a word
\ the engine binds and no checker row types.
: MADE-FIXTURE ( ptr u8 n ptr u8 n -- )
   {: text:ptr textu:n name:ptr nameu:n :}
   SB-RESET
   s" : MK ( -- ) create ;" SB-APPEND LF+
   s" MK MADE" SB-APPEND LF+
   text textu SB-APPEND LF+
   name nameu FIXTURE! ;

\ Packet k of the last check defers the stretch after MK to the run.
: MK-DEFERRED ( n -- )
   {: k:n :}
   k s" code" s" W-CHECK-DEFERRED" FIELD=
   k s" verdict" s" deferred" FIELD=
   k s" MK" 2 1 21 23 FX-PATH$ CANON$ FX$ AT-IN ;

\ The plain check and --all-errors each give one packet, a rejection: CODE at
\ TOK, on LINE at COL, in bytes [BS, BE).
: RUN-REFUSED-AT ( ptr u8 n ptr u8 n n n n n -- )
   {: c:ptr cu:n tok:ptr toku:n line:n col:n bs:n be:n :}
   s" " CHECK
   c cu REJECTED-AS
   0 tok toku line col bs be AT
   s" --all-errors" CHECK
   c cu REJECTED-AS
   0 tok toku line col bs be AT ;

\ --verify-only leaves the stretch after MK to the run: one packet, rc 0.
: VERIFY-DEFERS ( -- )
   s" --verify-only" 0 CHECK-EXIT
   GJA-LINE# @ 1 T=
   0 MK-DEFERRED ;

\ RUN-REFUSED-AT, and --verify-only gives that rejection after the deferral.
: VERIFY-REFUSED-AT ( ptr u8 n ptr u8 n n n n n -- )
   {: c:ptr cu:n tok:ptr toku:n line:n col:n bs:n be:n :}
   c cu tok toku line col bs be RUN-REFUSED-AT
   s" --verify-only" CHECK
   GJA-LINE# @ 2 T=
   0 MK-DEFERRED
   1 s" code" c cu FIELD=
   1 s" verdict" s" rejected" FIELD=
   1 tok toku line col bs be FX-PATH$ CANON$ FX$ AT-IN ;

\ The load of the fixture exits WANT, its error output holding TEXT, or empty
\ when TEXT is.
: LOADED ( ptr u8 n n -- )
   {: text:ptr textu:n want:n :}
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   FX-PATH$ >LEN PROC-ARGV+
   HB$ >LEN  EMPTY 0 >LEN  OUT CAP >LEN
   ERR CAP >LEN  TIMEOUT-MS >MS  RUN-ARGV-STDIN-CAPTURE-OUTCOME
   STORE!
   RC @ want T=
   textu 0= IF ERR-U @ 0 T= EXIT THEN
   ERR$ text textu CONTAINS? TTRUE ;

\ A word the engine binds and no checker row types, as one a create caller
\ made, is no word the load cannot compile: its tick is admitted, and a call to
\ it fails its check in source order, after a signature that does not parse and
\ an earlier failing check, as the load reports. A name nothing defines is
\ still the load's compiler's to refuse, before any check.
: TEST-UNROWED ( -- )
   s" a ticked word the engine binds and no row types" T-LABEL
   s" : X ( -- ) ['] MADE drop ; X" s" made-tick.f" MADE-FIXTURE
   s" " 0 CHECK-EXIT
   GJA-LINE# @ 0 T=
   s" --all-errors" 0 CHECK-EXIT
   GJA-LINE# @ 0 T=
   VERIFY-DEFERS
   s" " 0 LOADED
   s" a signature that does not parse and a call of that word" T-LABEL
   s" : X ( -- zz ) MADE ;" s" made-badsig.f" MADE-FIXTURE
   s" E-UNKNOWN-SIGNATURE-TYPE" s" zz" 3 10 38 40 VERIFY-REFUSED-AT
   s" x at 'zz'" REJECT-RC LOADED
   s" a signature that does not parse and a tick of that word" T-LABEL
   s" : X ( -- zz ) ['] MADE drop ;" s" made-badsig-tick.f" MADE-FIXTURE
   s" E-UNKNOWN-SIGNATURE-TYPE" s" zz" 3 10 38 40 VERIFY-REFUSED-AT
   s" x at 'zz'" REJECT-RC LOADED
   s" that signature, a call of that word and a trust-boundary word" T-LABEL
   s" : X ( -- zz ) MADE evaluate ;" s" made-badsig-unsafe.f" MADE-FIXTURE
   s" E-UNKNOWN-SIGNATURE-TYPE" s" zz" 3 10 38 40 VERIFY-REFUSED-AT
   s" x at 'zz'" REJECT-RC LOADED
   s" a trust-boundary word before a call of that word" T-LABEL
   s" : X ( -- ) evaluate MADE ;" s" made-unsafe.f" MADE-FIXTURE
   s" E-UNSAFE" s" evaluate" 3 12 40 48 RUN-REFUSED-AT
   VERIFY-DEFERS
   s" x at 'evaluate'" REJECT-RC LOADED
   s" a call of that word alone" T-LABEL
   s" : X ( -- ) MADE ;" s" made-call.f" MADE-FIXTURE
   s" E-UNDEFINED" s" MADE" 3 12 40 44 RUN-REFUSED-AT
   VERIFY-DEFERS
   s" x at 'MADE'" REJECT-RC LOADED
   s" a signature that does not parse and a word nothing defines" T-LABEL
   s" : X ( -- zz ) NOPE ;" s" made-nope.f" MADE-FIXTURE
   s" E-UNDEFINED: NOPE" REJECT-RC LOADED
   s" that signature, a call of MADE and a word nothing defines" T-LABEL
   s" : X ( -- zz ) MADE NOPE ;" s" made-call-nope.f" MADE-FIXTURE
   s" E-UNDEFINED: NOPE" REJECT-RC LOADED ;

\ A control-flow word with no structure open, or another structure's, is one
\ the load cannot compile: it refuses the definition at that word, before a
\ signature that does not parse, an earlier failing check and a later
\ undefined word.
: TEST-CONTROL-ORPHAN ( -- )
   s" a signature that does not parse and a then with no if" T-LABEL
   s" : X ( n -- zz ) then ;" s" badsig-then.f" LINE-FIXTURE
   s" E-REJECTED" s" then" 1 17 16 20 REFUSED-AT
   s" a then with no if before an undefined word" T-LABEL
   s" : X ( n -- n ) then NOPE ;" s" then-undefined.f" LINE-FIXTURE
   s" E-REJECTED" s" then" 1 16 15 19 REFUSED-AT
   s" a trust-boundary word before a then with no if" T-LABEL
   s" : X ( n -- n ) evaluate then ;" s" unsafe-then.f" LINE-FIXTURE
   s" E-REJECTED" s" then" 1 25 24 28 REFUSED-AT
   s" an endcase with no case before an undefined word" T-LABEL
   s" : X ( n -- n ) endcase NOPE ;" s" endcase-undefined.f" LINE-FIXTURE
   s" E-REJECTED" s" endcase" 1 16 15 22 REFUSED-AT
   s" a signature that does not parse and a then closing a begin" T-LABEL
   s" : X ( n -- zz ) begin then ;" s" badsig-begin-then.f" LINE-FIXTURE
   s" E-REJECTED" s" then" 1 23 22 26 REFUSED-AT
   s" a leave outside every do before an undefined word" T-LABEL
   s" : X ( n -- n ) leave NOPE ;" s" leave-undefined.f" LINE-FIXTURE
   s" E-REJECTED" s" leave" 1 16 15 20 REFUSED-AT ;

\ The load's compiler takes a second else, as it takes an else over an if's
\ frame: the check fails there, which leaves the refusal to a later undefined
\ word, a signature that does not parse and an earlier trust-boundary word.
: TEST-SECOND-ELSE ( -- )
   s" a second else before an undefined word" T-LABEL
   s" : X ( bool -- ) if else else then NOPE ;" s" else2-undefined.f" LINE-FIXTURE
   s" E-UNDEFINED" s" NOPE" 1 35 34 38 REFUSED-AT
   s" a signature that does not parse and a second else" T-LABEL
   s" : X ( bool -- zz ) if else else then ;" s" badsig-else2.f" LINE-FIXTURE
   s" E-UNKNOWN-SIGNATURE-TYPE" s" zz" 1 15 14 16 REFUSED-AT
   s" a trust-boundary word before a second else" T-LABEL
   s" : X ( bool -- ) evaluate if else else then ;" s" unsafe-else2.f" LINE-FIXTURE
   s" E-UNSAFE" s" evaluate" 1 17 16 24 REFUSED-AT
   s" a second else alone" T-LABEL
   s" : X ( bool -- ) if else else then ;" s" else2.f" LINE-FIXTURE
   s" E-REJECTED" s" else" 1 25 24 28 REFUSED-AT ;

\ A structure still open where its definition ends is one the load cannot
\ compile: it refuses the definition at the `;`, or at the `does>` ending a
\ definer's body, before a signature that does not parse, a called shadowed
\ name, a trust-boundary word and dead code. An open match keeps its reason.
: TEST-CONTROL-OPEN ( -- )
   s" a signature that does not parse and an if open at ;" T-LABEL
   s" : X ( n -- zz ) dup if ;" s" badsig-open.f" LINE-FIXTURE
   s" E-REJECTED" s" ;" 1 24 23 24 REFUSED-AT
   s" a called shadowed name and an IF open at ;" T-LABEL
   s" : SHADE ( -- ) ;" s"    SHADE IF ;" SHADOW-FIXTURE
   s" shade-open.f" FIXTURE!
   s" E-REJECTED" s" ;" 8 13 96 97 REFUSED-AT
   s" a trust-boundary word and an if open at ;" T-LABEL
   s" : X ( n -- n ) evaluate if ;" s" unsafe-open.f" LINE-FIXTURE
   s" E-REJECTED" s" ;" 1 28 27 28 REFUSED-AT
   s" dead code and an if open at ;" T-LABEL
   s" : X ( n -- n ) dup if exit 1 ;" s" dead-open.f" LINE-FIXTURE
   s" E-REJECTED" s" ;" 1 30 29 30 REFUSED-AT
   s" an if open at the does> ending a definer's body" T-LABEL
   s" : CONST ( n -- ) create dup , 0= if does> ( -- n ) @ ;" s" does-open.f" LINE-FIXTURE
   s" E-REJECTED" s" does>" 1 37 36 41 REFUSED-AT
   s" an if open at the ; ending a does> clause" T-LABEL
   s" : CONST ( n -- ) create , does> ( -- n ) @ dup 0= if ;" s" clause-open.f" LINE-FIXTURE
   s" E-REJECTED" s" ;" 1 54 53 54 REFUSED-AT
   s" a match open at ;" T-LABEL
   SB-RESET
   s" SUMTYPE mres 0 VARIANT ok n ;VARIANT ;SUMTYPE" SB-APPEND LF+
   s" : X ( mres -- n ) MATCH mres ok OF 1 ;" SB-APPEND LF+
   s" match-open.f" FIXTURE!
   s" E-MATCH-UNTERMINATED" s" ;" 2 38 83 84 REFUSED-AT ;

\ A match beyond the control-frame limit leaves its body to the load. The
\ checker's abandoned-match walk must still spend literal payloads before
\ counting a real ;MATCH, so their text cannot replace the depth refusal.
: MATCH-DEPTH-START ( -- )
   SB-RESET
   s" ENUM shade red ;ENUM" SB-APPEND LF+
   s" : X ( -- )" SB-APPEND
   31 0 DO s"  true if" SB-APPEND LOOP LF+
   s" MATCH shade red OF " SB-APPEND ;

: MATCH-DEPTH-END ( -- )
   s"  ENDOF ;MATCH" SB-APPEND LF+
   31 0 DO s"  then" SB-APPEND LOOP
   s"  ;" SB-APPEND LF+ ;

: TEST-MATCH-DEPTH-PAYLOAD ( -- )
   s" a string cannot close an abandoned match" T-LABEL
   MATCH-DEPTH-START
   s" s" SB-APPEND 34 SB-APPEND-C
   s"  ;match NOPE" SB-APPEND 34 SB-APPEND-C
   s"  2drop" SB-APPEND
   MATCH-DEPTH-END
   s" depth-string.f" FIXTURE!
   s" E-MATCH-DEPTH" s" shade" 3 7 286 291 REFUSED-AT
   s" a [char] operand cannot close an abandoned match" T-LABEL
   MATCH-DEPTH-START
   s" [char] ;match drop" SB-APPEND
   MATCH-DEPTH-END
   s" depth-char.f" FIXTURE!
   s" E-MATCH-DEPTH" s" shade" 3 7 286 291 REFUSED-AT ;

\ AMB, which packages PA and PB both publish, and with GLOBAL the global
\ wordlist too (line 9 defines AMC otherwise), and on line 13 the body BODY of
\ a definition under `using PA` and `using PB`.
: AMBIG-FIXTURE ( bool ptr u8 n -- )
   {: global:bool body:ptr bodyu:n :}
   SB-RESET
   s" package PA" SB-APPEND LF+
   s" public" SB-APPEND LF+
   s" : AMB ( -- ) ;" SB-APPEND LF+
   s" ;package" SB-APPEND LF+
   s" package PB" SB-APPEND LF+
   s" public" SB-APPEND LF+
   s" : AMB ( -- ) ;" SB-APPEND LF+
   s" ;package" SB-APPEND LF+
   global IF s" : AMB ( -- ) ;" ELSE s" : AMC ( -- ) ;" THEN SB-APPEND LF+
   s" using PA" SB-APPEND LF+
   s" using PB" SB-APPEND LF+
   s" : USE-IT ( -- )" SB-APPEND LF+
   body bodyu SB-APPEND LF+
   s" ;using" SB-APPEND LF+
   s" ;using" SB-APPEND LF+ ;

\ A called name a used public shadows a global for, or two used publics export
\ beside one, is refused by the load's check at `;`: after every word the load
\ cannot compile, wherever it stands, and before any other check. Ticked, or
\ exported by two used publics and no global, the load's tick or lookup
\ refuses it while compiling, in source order.
: TEST-USING-ORDER ( -- )
   s" a called shadowed name before an undefined word" T-LABEL
   s" : SHADE ( -- ) ;" s"    SHADE NOPE ;" SHADOW-FIXTURE
   s" shade-undefined.f" FIXTURE!
   s" E-UNDEFINED" s" NOPE" 8 10 93 97 REFUSED-AT
   s" an undefined word before a called shadowed name" T-LABEL
   s" : SHADE ( -- ) ;" s"    NOPE SHADE ;" SHADOW-FIXTURE
   s" undefined-shade.f" FIXTURE!
   s" E-UNDEFINED" s" NOPE" 8 4 87 91 REFUSED-AT
   s" an undefined word before a ticked shadowed name" T-LABEL
   s" : SHADE ( -- ) ;" s"    NOPE ['] SHADE drop ;" SHADOW-FIXTURE
   s" undefined-tick-shade.f" FIXTURE!
   s" E-UNDEFINED" s" NOPE" 8 4 87 91 REFUSED-AT
   s" a trust-boundary word before a called shadowed name" T-LABEL
   s" : SHADE ( -- ) ;" s"    evaluate SHADE ;" SHADOW-FIXTURE
   s" unsafe-shade.f" FIXTURE!
   s" E-USING-SHADOW-GLOBAL" s" SHADE" 8 13 96 101 REFUSED-AT
   s" a called name two used publics and a global export before an undefined word" T-LABEL
   true s"    AMB NOPE ;" AMBIG-FIXTURE
   s" ambig-global-undefined.f" FIXTURE!
   s" E-UNDEFINED" s" NOPE" 13 8 140 144 REFUSED-AT
   s" an undefined word before a name only two used publics export" T-LABEL
   false s"    NOPE AMB ;" AMBIG-FIXTURE
   s" undefined-ambig.f" FIXTURE!
   s" E-UNDEFINED" s" NOPE" 13 4 136 140 REFUSED-AT ;

: MAIN ( -- )
   T-RESET
   ROOT!
   TEST-LINES
   TEST-TAB
   TEST-CRLF
   TEST-SPACES
   TEST-COMMENTS
   TEST-STRING
   TEST-EMOJI-STRING
   TEST-UTF8
   TEST-ONE-LINE
   TEST-PAREN-EMOJI
   TEST-SIGNATURE-LINE
   TEST-LATER
   TEST-SHADOW
   TEST-TICK-SHADOW
   TEST-IS-SHADOW
   TEST-SHADOWED-ARITY
   TEST-NOT-RECORDED
   TEST-GENERATES
   TEST-GENERATES-SHADOW
   TEST-ALL-ERRORS
   TEST-COMPOSE-DEP
   TEST-COMPOSE-AFTER
   TEST-COMPOSE-ALL
   TEST-COMPOSE-VERIFY
   TEST-DECLARATION
   TEST-NEWTYPE
   TEST-ENUM
   TEST-ENUM-CLOSE
   TEST-STRUCTURE
   TEST-BAD-SIGNATURE
   TEST-REFUSAL-ORDER
   TEST-REPAIR-FOLLOWS
   TEST-LIVE-BRANCHES
   TEST-TICK-TARGET
   TEST-UNROWED
   TEST-CONTROL-ORPHAN
   TEST-SECOND-ELSE
   TEST-CONTROL-OPEN
   TEST-MATCH-DEPTH-PAYLOAD
   TEST-USING-ORDER
   CLEANUP-RUN
   T-REPORT
   s" diag-position-test: ok" type cr ;

MAIN

;package
