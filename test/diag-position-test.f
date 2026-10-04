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
   CLEANUP-RUN
   T-REPORT
   s" diag-position-test: ok" type cr ;

MAIN

;package
