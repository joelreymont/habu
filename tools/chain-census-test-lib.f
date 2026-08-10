\ chain-census-test-lib.f - checked fixtures for tools/chain-census-core.f.
\
\ Every case here drives the REAL entry points - CHAIN-CENSUS:PATH+, :RUN,
\ :FINISH, :REPORT - over real files on disk, because that is what the tool does
\ in production: it loads a Habu source, lexes it, reopens its packages and hands
\ each body to the native chain. A fixture that stopped at a string in memory
\ would exercise none of the load, the reopen or the chain.
\
\ THE FIXTURES ARE WRITTEN TO A TEMPORARY DIRECTORY rather than kept in the tree,
\ for two reasons. They have to LOAD - a fixture that does not compile teaches the
\ census nothing - and a loadable Habu file sitting in tools/ would be swept by
\ every tree-wide lint. Writing them per run also lets one file carry the hostile
\ rows without their text being read by anything else.
\
\ WHAT THE HOSTILE FIXTURE IS FOR. A census that searched TEXT rather than reading
\ TOKENS would agree with this suite on every ordinary file. Each hostile row
\ names, above it, the exact wrong answer a text-matching census would give, and
\ the assertions pin the right one - not "it did not crash".

require lib/errors.f
require lib/string.f
require lib/fmt.f
require lib/test.f
require lib/memory.f
require lib/vector.f
require lib/fs.f
require lib/fs-mutate.f
require lib/sort.f
require tools/lint/text.f
require tools/lint/token.f
require tools/lint/lib.f
require tools/lint/source-lex.f
require tools/lint/def.f
require lib/content-key.f
require src/compiler/native/clobber.f
require src/compiler/native/publish.f
require tools/chain-census-core.f

package CHAIN-CENSUS-TEST
private

\ ---- fixture text ---------------------------------------------------------------
\ Its own appender rather than the shared string builder: SB holds 1024 bytes and
\ the hostile fixture is larger, and the typed STR:BUF surface would want a CAD-NUM
\ role bridge for a length this file only ever uses as a byte count.
$4000 constant FIX-CAP
8192 constant OUT-CAP                        \ holds one whole census report

create FIX-BUF FIX-CAP allot
variable FIX-U
create OUT-BUF OUT-CAP allot

: FIX-RESET ( -- )
   0 FIX-U ! ;

: FIX+ ( ptr u8 n -- ) {: a:ptr u:n :}
   FIX-U @ u + FIX-CAP > if E-TBL-BOUNDS throw then
   a FIX-BUF FIX-U @ + u BYTE-COPY
   FIX-U @ u + FIX-U ! ;

: FIX-C ( n -- ) {: c:n :}
   FIX-U @ 1+ FIX-CAP > if E-TBL-BOUNDS throw then
   c FIX-BUF FIX-U @ + c!
   FIX-U @ 1+ FIX-U ! ;

: FIX$ ( -- ptr u8 n )
   FIX-BUF FIX-U @ ;

: LF ( -- )
   10 FIX-C ;

: DQ ( -- )
   34 FIX-C ;

: LINE ( ptr u8 n -- )
   FIX+ LF ;

\ ---- where the fixtures live -------------------------------------------------------
create ROOT-BUF FS-PATH-CAP allot      variable ROOT-U
create GOOD-BUF FS-PATH-CAP allot      variable GOOD-U
create MOD-BUF FS-PATH-CAP allot       variable MOD-U
create STR-BUF FS-PATH-CAP allot       variable STR-U
create PRIV-BUF FS-PATH-CAP allot      variable PRIV-U
create REGS-BUF FS-PATH-CAP allot      variable REGS-U
create HOST-BUF FS-PATH-CAP allot      variable HOST-U
create UNL-BUF FS-PATH-CAP allot       variable UNL-U
create ALREADY-BUF FS-PATH-CAP allot   variable ALREADY-U
create STALE-BUF FS-PATH-CAP allot     variable STALE-U
create BADLOAD-BUF FS-PATH-CAP allot   variable BADLOAD-U
create LONG-BUF FS-PATH-CAP allot      variable LONG-U
create SEALED-BUF FS-PATH-CAP allot    variable SEALED-U
create HIDDEN-BUF FS-PATH-CAP allot    variable HIDDEN-U
create SUITE-BUF FS-PATH-CAP allot     variable SUITE-U
create MISS-BUF FS-PATH-CAP allot      variable MISS-U

: ROOT$ ( -- ptr u8 n )     ROOT-BUF ROOT-U @ ;
: GOOD$ ( -- ptr u8 n )     GOOD-BUF GOOD-U @ ;
: MOD$ ( -- ptr u8 n )      MOD-BUF MOD-U @ ;
: STR$ ( -- ptr u8 n )      STR-BUF STR-U @ ;
: PRIV$ ( -- ptr u8 n )     PRIV-BUF PRIV-U @ ;
: REGS$ ( -- ptr u8 n )     REGS-BUF REGS-U @ ;
: HOST$ ( -- ptr u8 n )     HOST-BUF HOST-U @ ;
: UNL$ ( -- ptr u8 n )      UNL-BUF UNL-U @ ;
: ALREADY$ ( -- ptr u8 n )  ALREADY-BUF ALREADY-U @ ;
: STALE$ ( -- ptr u8 n )    STALE-BUF STALE-U @ ;
: BADLOAD$ ( -- ptr u8 n )  BADLOAD-BUF BADLOAD-U @ ;
: LONG$ ( -- ptr u8 n )     LONG-BUF LONG-U @ ;
: SEALED$ ( -- ptr u8 n )   SEALED-BUF SEALED-U @ ;
: HIDDEN$ ( -- ptr u8 n )   HIDDEN-BUF HIDDEN-U @ ;
: SUITEF$ ( -- ptr u8 n )   SUITE-BUF SUITE-U @ ;
: MISS$ ( -- ptr u8 n )     MISS-BUF MISS-U @ ;

: ROOT! ( ptr u8 n -- ) {: a:ptr u:n :}
   a ROOT-BUF u BYTE-COPY
   u ROOT-U ! ;

\ ---- the fixture sources -----------------------------------------------------------
\ Every definition inside the chain's dialect.
: GOOD-SRC ( -- ptr u8 n )
   FIX-RESET
   s" package CENSUS-FIX-GOOD" LINE
   s" public" LINE
   s" : CFG-ADD ( n -- n ) dup + 1+ ;" LINE
   s" ;package" LINE
   FIX$ ;

\ One word the dialect does not model, written where the chain will meet it.
\
\ WHY THE RETURN STACK IS THE FIXTURE. A body word the dialect has no operation
\ for is not refused for that alone: src/compiler/native/elaborate.f RESOLVE-SCAN
\ puts every unmodelled name to the running engine, and a word the engine can
\ name and the checker can size becomes a CALL. `mod`, which this fixture used to
\ be written with, compiles that way now, and so does a named CONSTANT, which is
\ what it was written with after that: dot habu-export-the-checker-2bbc831c gave
\ the checker's stored effects a per-cell width, so `-- a` sizes at one cell.
\ `>r` is refused for a reason no width can settle - the dialect stages no
\ operation for the return stack at all, and there is nothing to ask the engine
\ about. The elaborator refuses the body by the dialect's own name and says which
\ token it was.
\
\ THE CONSTANT STAYS, AS A CONTROL. It is still a definition of the file that is
\ not a colon definition, so it still holds the population line below; and it is
\ now a word the chain CAN size, named in the same body, which is what makes the
\ refusal below unambiguously about the return-stack token. A fixture that only
\ said "this body is refused" would not distinguish the two.
\
\ AND THE COUPLING TO A MISSING CAPABILITY IS STILL DELIBERATE. When the dialect
\ models the return stack this body compiles and this fixture must change with
\ it. That is what a fixture is for: it fails when the thing it describes stops
\ being true.
: MOD-SRC ( -- ptr u8 n )
   FIX-RESET
   s" package CENSUS-FIX-MOD" LINE
   s" public" LINE
   s" 3 constant CFM-K" LINE
   s" : CFM-MOD ( n -- n ) >r CFM-K r> + ;" LINE
   s" ;package" LINE
   FIX$ ;

\ A string literal, which the chain compiles: the reader fills the tape's string
\ kind with the literal's body and the elaborator stages an address and a length.
\ It was E-HIR-UNMODELED at the token spelled `s"` until dot
\ habu-compile-str-literals-30a7121b landed the tranche.
: STR-SRC ( -- ptr u8 n )
   FIX-RESET
   s" package CENSUS-FIX-STR" LINE
   s" public" LINE
   s" : CFS-STR ( -- ) s" FIX+ DQ s"  x" FIX+ DQ s"  2drop ;" LINE
   s" ;package" LINE
   FIX$ ;

\ Two bodies that name PRIVATE words of their own package. Driven at top level
\ either would die at the engine with an unresolved name; driven inside the
\ reopened package both reach the chain. That difference is the whole proof that
\ the reopen works, and each half of it says something the other cannot.
\
\ CFP-USES names the private HELPER and compiles, because the reopen let the
\ engine answer where the helper's code starts and the checker answer how many
\ cells a call to it moves. CFP-HELPER names the private CONSTANT and now
\ compiles too: dot habu-export-the-checker-2bbc831c gave the checker's stored
\ effects a per-cell width, so a constant sizes at one cell where it used to be
\ refused. THAT IS A STRONGER PROOF THAN THE REFUSAL IT REPLACES, and worth
\ saying plainly. A refusal only showed the private name had been REACHED; a
\ body that compiles must have RESOLVED it, because a body naming a private
\ word its package was not reopened for is refused by the engine before the
\ chain sees it - which is the count the last assertion below pins at zero.
: PRIV-SRC ( -- ptr u8 n )
   FIX-RESET
   s" package CENSUS-FIX-PRIV" LINE
   s" private" LINE
   s" 3 constant CFP-K" LINE
   s" : CFP-HELPER ( n -- n ) CFP-K + ;" LINE
   s" public" LINE
   s" : CFP-USES ( n -- n ) CFP-HELPER 1+ ;" LINE
   s" ;package" LINE
   FIX$ ;

\ Ten values carried across a nested counted loop. At the census's budget - every
\ general register the ABI can describe - the allocator still cannot keep them
\ all. That refusal is a fact about the program and must be classed as pressure,
\ never as a missing language feature.
: REGS-SRC ( -- ptr u8 n )
   FIX-RESET
   s" package CENSUS-FIX-REGS" LINE
   s" public" LINE
   s" : CFR-NEST ( n n n n n n n n n n -- n )" LINE
   s"    {: a:n b:n c:n d:n e:n f:n g:n h:n i2:n j:n :}" LINE
   s"    0 j 0 ?do" LINE
   s"       a b + c + d + e +" LINE
   s"       j 0 ?do f g + h + i2 + i + + loop" LINE
   s"       + i +" LINE
   s"    loop ;" LINE
   s" ;package" LINE
   FIX$ ;

\ A refusal code the census's own table does not name. A hexadecimal literal is one
\ the tape's producer cannot read back, and it throws a code no row here predicts -
\ exactly the case that must widen the report instead of ending the run.
: UNL-SRC ( -- ptr u8 n )
   FIX-RESET
   s" package CENSUS-FIX-UNLISTED" LINE
   s" public" LINE
   s" : CFU-HEX ( -- n ) $FF ;" LINE
   s" ;package" LINE
   FIX$ ;

\ A definition at GLOBAL scope, so the suite can ask the dictionary about it by the
\ name the file gave it.
: ALREADY-SRC ( -- ptr u8 n )
   FIX-RESET
   s" : CENSUS-FIX-ALREADY ( n -- n ) dup + ;" LINE
   FIX$ ;

\ The canonical hazard shape. CFT-FIRST uses the return stack, which the chain
\ cannot compile for the reason MOD-SRC gives, so it is refused BY THE ELABORATOR
\ and leaves that word in the elaborator's refused-token record; CFT-SECOND is
\ refused by the ENGINE, before any elaboration begins, so nothing in the
\ elaborator runs and the record would still name CFT-FIRST's word.
\
\ WHY CFT-SECOND IS REFUSED AT CENSUS TIME BUT LOADS FINE. `using` lasts for the
\ file that wrote it: while this file loads, CFT-HELPER resolves bare, and the
\ moment it has loaded that stops being true. The census reopens a definition's
\ PACKAGE, and these two are at global scope with no package to reopen, so the
\ bare name no longer resolves and the engine refuses the source. That is a real
\ shape from the real tree, not a contrivance for this suite.
: STALE-SRC ( -- ptr u8 n )
   FIX-RESET
   s" package CENSUS-FIX-STALE" LINE
   s" public" LINE
   s" : CFT-HELPER ( n -- n ) dup + ;" LINE
   s" ;package" LINE
   s" using CENSUS-FIX-STALE" LINE
   s" 3 constant CFT-K" LINE
   s" : CFT-FIRST ( n -- n ) >r CFT-K r> + ;" LINE
   s" : CFT-SECOND ( n -- n ) CFT-HELPER 1+ ;" LINE
   FIX$ ;

\ A file that exists and will not compile. Its failure is catchable, unlike a path
\ that is not there at all, and the two are separate lines of the report.
: BADLOAD-SRC ( -- ptr u8 n )
   FIX-RESET
   s" : CENSUS-FIX-BADLOAD ( n -- n ) CENSUS-FIX-NO-SUCH-WORD ;" LINE
   FIX$ ;

\ A definition past the RECORDER's own text ceiling. It is a real refusal with a
\ real code, and it is the instrument's rather than the dialect's: nothing about
\ the body is outside the subset, the unit that records the tape simply will not
\ take a source that long. There is deliberately no fixture for a body too long
\ for the census's own staging buffer, because there cannot be one - the engine
\ refuses to compile a definition of that size at all, so no file holding one
\ would load.
170 constant MID-STEPS                       \ about a kilobyte of body

\ Wrapped every ten steps: a source line has a ceiling, and a body written as one
\ enormous line is truncated at it - which ends the definition early and hands the
\ rest to the interpreter. The wrapping is about the FILE being loadable and
\ changes nothing about the definition's length in bytes, which is what is being
\ measured.
10 constant STEPS-PER-LINE

: STEP1 ( n -- ) {: k:n :}
   s" 1+ 1- " FIX+
   k 1+ STEPS-PER-LINE mod 0= if LF then ;

: STEPS ( n -- ) {: n:n :}
   n 0 ?do i STEP1 loop ;

: LONG-SRC ( -- ptr u8 n )
   FIX-RESET
   s" package CENSUS-FIX-LONG" LINE
   s" public" LINE
   s" : CFL-MID ( n -- n ) " FIX+ MID-STEPS STEPS s" ;" LINE
   s" ;package" LINE
   FIX$ ;

\ A package that seals its own wordlists, exactly as every substrate file does.
\ Reopening one ENDS THE PROCESS - status 84, the package name as the whole
\ message - so this fixture is also the strongest failure the suite can produce:
\ take the capability probe out of the census and this test does not fail, it
\ kills the run.
: SEALED-SRC ( -- ptr u8 n )
   FIX-RESET
   s" package CENSUS-FIX-SEALED" LINE
   s" public" LINE
   s" : CFE-SEALED ( n -- n ) dup + ;" LINE
   s" private" LINE
   s" get-current prot-wid-add" LINE
   s" public" LINE
   s" get-current prot-wid-add" LINE
   s" ;package" LINE
   FIX$ ;

\ A package with nothing public in it. Its wordlists are not sealed, but no
\ qualified spelling of any of its definitions resolves, so the census cannot
\ learn which wordlist the section is and has no safe way to find out. It is
\ skipped for the same reason and counted on the same line.
: HIDDEN-SRC ( -- ptr u8 n )
   FIX-RESET
   s" package CENSUS-FIX-HIDDEN" LINE
   s" private" LINE
   s" : CFN-HIDDEN ( n -- n ) dup + ;" LINE
   s" ;package" LINE
   FIX$ ;

\ A file the census must never load, and it proves it the only way that is not
\ circular: if the census loads it, the process ends on the spot with a status
\ nothing else produces, so the assertion below cannot pass by accident.
: SUITE-SRC ( -- ptr u8 n )
   FIX-RESET
   s" : CFQ-NEVER ( n -- n ) dup + ;" LINE
   s" s" FIX+ DQ s"  a suite the census loaded" FIX+ DQ s"  99 die" LINE
   FIX$ ;

\ ---- the hostile file ---------------------------------------------------------------
\ Seven rows, each naming the wrong answer it is built to produce.
: HOST-SRC ( -- ptr u8 n )
   FIX-RESET
   s" package CENSUS-FIX-HOSTILE" LINE
   s" public" LINE
   s" \ row 1: the word below is inside a line comment, so a text-matching" LINE
   s" \ census reports a dialect gap this body does not have.  mod" LINE
   s" : CFH-CLEAN ( n -- n ) dup + ;" LINE
   s" \ row 2: the same trap in a paren comment rather than a line comment." LINE
   s" : CFH-PAREN ( n -- n ) ( negate ) 1+ ;" LINE
   s" \ row 3: a string literal holding definition-shaped text, a bare closer and" LINE
   s" \ an unmodeled word. A text-matching census counts a definition named FAKE," LINE
   s" \ ends THIS definition at the quoted closer, and blames the quoted word." LINE
   s" : CFH-QUOTE ( -- ) s" FIX+ DQ s"  : FAKE ( -- ) ; mod" FIX+ DQ s"  2drop ;" LINE
   s" \ row 4: a quotation. Its closer is not the definition closer, and a" LINE
   s" \ closer-finder matching a leading semicolon would end this row early." LINE
   s" : CFH-QUOT ( n -- n ) [: 1+ ;] execute ;" LINE
   s" \ row 5: the definition after the quotation. If row 4 ended early this one" LINE
   s" \ is never seen, so its presence is what proves row 4 was read whole." LINE
   s" : CFH-AFTER ( n -- n ) 1+ ;" LINE
   s" \ row 6: the signature comment names four terms before the arrow and the" LINE
   s" \ checker certified three, because `ptr u8` is one value. A census counting" LINE
   s" \ the comment would declare four inputs and hand the chain a lie." LINE
   s" : CFH-ARITY ( ptr u8 n n -- n ) {: a:ptr u:n k:n :} u k + ;" LINE
   s" \ row 7: an unchecked definer. It fills no tape, so it is a population line" LINE
   s" \ and not a verdict - and a census keying on the colon alone would census it." LINE
   s" TRUSTED: CFH-TRUST ( n -- n ) 1+ ;" LINE
   s" ;package" LINE
   FIX$ ;

\ ---- laying the fixtures down ---------------------------------------------------------
: PATH! ( ptr u8 n ptr u8 ptr a -- ) {: name:ptr nameu:n buf:ptr lenv:ptr :}
   ROOT$ name nameu buf JOIN-PATH lenv !
   buf lenv @ CLEANUP+ ;

: PATHS ( -- )
   s" good.f" GOOD-BUF GOOD-U PATH!
   s" mod.f" MOD-BUF MOD-U PATH!
   s" str.f" STR-BUF STR-U PATH!
   s" priv.f" PRIV-BUF PRIV-U PATH!
   s" regs.f" REGS-BUF REGS-U PATH!
   s" hostile.f" HOST-BUF HOST-U PATH!
   s" unlisted.f" UNL-BUF UNL-U PATH!
   s" already.f" ALREADY-BUF ALREADY-U PATH!
   s" stale.f" STALE-BUF STALE-U PATH!
   s" badload.f" BADLOAD-BUF BADLOAD-U PATH!
   s" long.f" LONG-BUF LONG-U PATH!
   s" sealed.f" SEALED-BUF SEALED-U PATH!
   s" hidden.f" HIDDEN-BUF HIDDEN-U PATH!
   s" suite-test.f" SUITE-BUF SUITE-U PATH! ;

\ Written in an order that is not alphabetical, so a report that comes out sorted
\ did not get there by copying whatever order the host's directory walk answers.
: WRITE-ALL-FIXTURES ( -- )
   HOST$ HOST-SRC WRITE-ALL
   ALREADY$ ALREADY-SRC WRITE-ALL
   UNL$ UNL-SRC WRITE-ALL
   STR$ STR-SRC WRITE-ALL
   PRIV$ PRIV-SRC WRITE-ALL
   REGS$ REGS-SRC WRITE-ALL
   STALE$ STALE-SRC WRITE-ALL
   BADLOAD$ BADLOAD-SRC WRITE-ALL
   LONG$ LONG-SRC WRITE-ALL
   SEALED$ SEALED-SRC WRITE-ALL
   HIDDEN$ HIDDEN-SRC WRITE-ALL
   SUITEF$ SUITE-SRC WRITE-ALL
   MOD$ MOD-SRC WRITE-ALL
   GOOD$ GOOD-SRC WRITE-ALL ;

: PREPARE ( -- )
   CLEANUP-RESET
   s" habu-chain-census" TMPDIR-MKDIR ROOT!
   ROOT$ CLEANUP-DIR+
   PATHS
   WRITE-ALL-FIXTURES ;

\ ---- driving one census -----------------------------------------------------------------
: CENSUS1 ( ptr u8 n -- ) {: a:ptr u:n :}
   CHAIN-CENSUS:RESET
   a u CHAIN-CENSUS:PATH+
   CHAIN-CENSUS:RUN
   CHAIN-CENSUS:FINISH ;

\ The same run with its report captured, so the page itself can be read back.
: CENSUS-REPORT ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   CHAIN-CENSUS:RESET
   a u CHAIN-CENSUS:PATH+
   CHAIN-CENSUS:RUN
   OUT-BUF OUT-CAP LINT-OUT-BUFFER!
   CHAIN-CENSUS:REPORT
   LINT-OUT$ {: oa:ptr ou:n :}
   LINT-OUT-BUFFER-OFF
   oa ou ;

\ ---- small readers the cases share --------------------------------------------------------
: DEF-BY ( ptr u8 n -- n ) {: a:ptr u:n :}   \ record index of a censused name, or -1
   0 begin dup CHAIN-CENSUS:DEFS < while
      dup CHAIN-CENSUS:DEF-NAME$ a u LINT-STR= if exit then
      1+
   repeat drop -1 ;

: SPELLED? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   0 begin dup CHAIN-CENSUS:SPELLS < while
      dup CHAIN-CENSUS:SPELL$ a u LINT-STR= if drop true exit then
      1+
   repeat drop false ;

variable ACC

: SPELL-TOTAL ( -- n )
   0 ACC !
   0 begin dup CHAIN-CENSUS:SPELLS < while
      dup CHAIN-CENSUS:SPELL-N ACC @ + ACC !
      1+
   repeat drop
   ACC @ ;

: CODE-AT-NAME ( ptr u8 n -- n )
   DEF-BY CHAIN-CENSUS:DEF-CODE ;

: SPELL-AT-NAME$ ( ptr u8 n -- ptr u8 n )
   DEF-BY CHAIN-CENSUS:DEF-SPELL$ ;

: SPELL-LEN-AT-NAME ( ptr u8 n -- n )
   SPELL-AT-NAME$ {: a:ptr u:n :}
   u ;

\ ---- (a) a body wholly inside the dialect ---------------------------------------------------
: CASE-GOOD ( -- )
   GOOD$ CENSUS1
   s" good: one definition examined" T-LABEL
      0 CHAIN-CENSUS:FILE-EXAMINED 1 T=
   s" good: it compiled" T-LABEL
      0 CHAIN-CENSUS:FILE-COMPILED 1 T=
   s" good: nothing refused" T-LABEL
      0 CHAIN-CENSUS:FILE-REFUSED 0 T=
   s" good: the record names the source word" T-LABEL
      0 CHAIN-CENSUS:DEF-NAME$ s" CFG-ADD" T$=
   s" good: the chain returned no error" T-LABEL
      0 CHAIN-CENSUS:DEF-CODE 0 T= ;

\ ---- (b) a word the dialect does not model ---------------------------------------------------
: CASE-MOD ( -- )
   MOD$ CENSUS1
   s" unmodeled: one colon definition was examined" T-LABEL
      0 CHAIN-CENSUS:FILE-EXAMINED 1 T=
   \ The constant the body names. It is a definition of the file and the census
   \ saw it, but it is not a colon definition, so it is counted on the population
   \ line rather than offered to the chain - which is why adding it to the fixture
   \ moved no verdict and shifted no record index.
   s" unmodeled: the constant is a population line, not a censused body" T-LABEL
      0 CHAIN-CENSUS:FILE-NOTCOLON 1 T=
   s" unmodeled: refused" T-LABEL
      0 CHAIN-CENSUS:FILE-REFUSED 1 T=
   s" unmodeled: refused as unmodeled" T-LABEL
      0 CHAIN-CENSUS:DEF-CODE E-HIR-UNMODELED T=
   s" unmodeled: the refusal names the word" T-LABEL
      0 CHAIN-CENSUS:DEF-SPELL$ s" >r" T$=
   s" unmodeled: the sub-histogram has one entry" T-LABEL
      CHAIN-CENSUS:SPELLS 1 T=
   s" unmodeled: and it is that word" T-LABEL
      0 CHAIN-CENSUS:SPELL$ s" >r" T$= ;

\ ---- (c) a string literal ----------------------------------------------------------------------
\ The chain compiles one now, so the verdict is no verdict: the body reaches the
\ chain, the chain has a rule for the kind, and the definition publishes. Both
\ refusal codes a string literal could earn are pinned at zero beside it - the
\ dialect refusal it used to earn until dot habu-compile-str-literals-30a7121b,
\ and the kind refusal the brief once expected - so the day either starts firing
\ on a string this suite says so instead of quietly agreeing.
: CASE-STRING ( -- )
   STR$ CENSUS1
   s" string: the body compiled" T-LABEL
      0 CHAIN-CENSUS:DEF-CODE 0 T=
   s" string: it was offered rather than skipped" T-LABEL
      0 CHAIN-CENSUS:FILE-EXAMINED 1 T=
   s" string: E-HIR-UNMODELED is unreached" T-LABEL
      E-HIR-UNMODELED CHAIN-CENSUS:COUNT-OF 0 T=
   s" string: E-HIR-KIND is still unreached" T-LABEL
      E-HIR-KIND CHAIN-CENSUS:COUNT-OF 0 T= ;

\ ---- (d) a private callee, which only a reopened package can resolve -----------------------------
: CASE-PRIVATE ( -- )
   PRIV$ CENSUS1
   s" private: both definitions were offered" T-LABEL
      0 CHAIN-CENSUS:FILE-EXAMINED 2 T=
   s" private: none was skipped for want of an effect" T-LABEL
      0 CHAIN-CENSUS:FILE-NOEFFECT 0 T=
   s" private: the private word's arity came from the checker" T-LABEL
      s" CFP-HELPER" DEF-BY CHAIN-CENSUS:DEF-IN 1 T=
   s" private: and its output count too" T-LABEL
      s" CFP-HELPER" DEF-BY CHAIN-CENSUS:DEF-OUT 1 T=
   s" private: the caller reached the CHAIN and compiled" T-LABEL
      s" CFP-USES" CODE-AT-NAME 0 T=
   s" private: the body naming the private CONSTANT compiled too" T-LABEL
      s" CFP-HELPER" CODE-AT-NAME 0 T=
   s" private: no name went unresolved" T-LABEL
      CHAIN-CENSUS:RC-UNDEFINED CHAIN-CENSUS:COUNT-OF 0 T= ;

\ ---- (e) register pressure is not a dialect gap -----------------------------------------------------
: CASE-REGISTERS ( -- )
   REGS$ CENSUS1
   s" registers: refused" T-LABEL
      0 CHAIN-CENSUS:FILE-REFUSED 1 T=
   s" registers: by the allocator" T-LABEL
      0 CHAIN-CENSUS:DEF-CODE E-A64RA-SPILL T=
   s" registers: classed as pressure" T-LABEL
      E-A64RA-SPILL CHAIN-CENSUS:CLASS-OF CHAIN-CENSUS:CL-PRESSURE T=
   s" registers: nothing landed in a dialect bucket" T-LABEL
      E-HIR-UNMODELED CHAIN-CENSUS:COUNT-OF 0 T=
   s" registers: and no spelling was recorded" T-LABEL
      CHAIN-CENSUS:SPELLS 0 T= ;

\ ---- (f) the hostile file ------------------------------------------------------------------------------
: CASE-HOSTILE-COUNTS ( -- )
   s" hostile: six checked colon definitions were offered" T-LABEL
      0 CHAIN-CENSUS:FILE-EXAMINED 6 T=
   s" hostile: six records, no phantom seventh" T-LABEL
      CHAIN-CENSUS:DEFS 6 T=
   s" hostile: the unchecked definer is a population line" T-LABEL
      0 CHAIN-CENSUS:FILE-NOTCOLON 1 T= ;

: CASE-HOSTILE-NAMES ( -- )
   s" hostile: row 1" T-LABEL  0 CHAIN-CENSUS:DEF-NAME$ s" CFH-CLEAN" T$=
   s" hostile: row 2" T-LABEL  1 CHAIN-CENSUS:DEF-NAME$ s" CFH-PAREN" T$=
   s" hostile: row 3" T-LABEL  2 CHAIN-CENSUS:DEF-NAME$ s" CFH-QUOTE" T$=
   s" hostile: row 4" T-LABEL  3 CHAIN-CENSUS:DEF-NAME$ s" CFH-QUOT" T$=
   s" hostile: row 5 was reached at all" T-LABEL
      4 CHAIN-CENSUS:DEF-NAME$ s" CFH-AFTER" T$=
   s" hostile: row 6" T-LABEL  5 CHAIN-CENSUS:DEF-NAME$ s" CFH-ARITY" T$=
   s" hostile: the quoted definition was never a definition" T-LABEL
      s" FAKE" DEF-BY -1 T=
   s" hostile: neither was the unchecked one" T-LABEL
      s" CFH-TRUST" DEF-BY -1 T= ;

: CASE-HOSTILE-VERDICTS ( -- )
   s" hostile: the commented word did not refuse row 1" T-LABEL
      s" CFH-CLEAN" CODE-AT-NAME 0 T=
   s" hostile: the paren-commented word did not refuse row 2" T-LABEL
      s" CFH-PAREN" CODE-AT-NAME 0 T=
   s" hostile: row 5 compiled" T-LABEL
      s" CFH-AFTER" CODE-AT-NAME 0 T=
   s" hostile: row 6 compiled" T-LABEL
      s" CFH-ARITY" CODE-AT-NAME 0 T=
   s" hostile: row 6 carried the checker's three inputs" T-LABEL
      s" CFH-ARITY" DEF-BY CHAIN-CENSUS:DEF-IN 3 T=
   s" hostile: and its one output" T-LABEL
      s" CFH-ARITY" DEF-BY CHAIN-CENSUS:DEF-OUT 1 T=
   s" hostile: row 3 compiled, literal and all" T-LABEL
      s" CFH-QUOTE" CODE-AT-NAME 0 T=
   s" hostile: row 4 was refused for the quotation opener" T-LABEL
      s" CFH-QUOT" SPELL-AT-NAME$ s" [:" T$= ;

: CASE-HOSTILE-SPELLINGS ( -- )
   s" hostile: one refused spelling" T-LABEL
      CHAIN-CENSUS:SPELLS 1 T=
   s" hostile: the line-commented word is not one of them" T-LABEL
      s" mod" SPELLED? TFALSE
   s" hostile: nor is the paren-commented one" T-LABEL
      s" negate" SPELLED? TFALSE
   s" hostile: the quotation opener is" T-LABEL
      s" [:" SPELLED? TTRUE
   s" hostile: spellings come out ordered by count then name" T-LABEL
      0 CHAIN-CENSUS:SPELL$ s" [:" T$=
   s" hostile: the sub-histogram adds up to the buckets that carry spellings" T-LABEL
      SPELL-TOTAL
      E-HIR-UNMODELED CHAIN-CENSUS:COUNT-OF
      E-NELAB-QUOT CHAIN-CENSUS:COUNT-OF + T= ;

: CASE-HOSTILE ( -- )
   HOST$ CENSUS1
   CASE-HOSTILE-COUNTS
   CASE-HOSTILE-NAMES
   CASE-HOSTILE-VERDICTS
   CASE-HOSTILE-SPELLINGS ;

\ ---- (g) a name the dictionary already holds -------------------------------------------------------------
\ The census loads every file it measures, so by the time a body is offered its own
\ name is already taken - which is why the body is driven under a name of its own.
\ Censusing the same file twice makes the point twice: the second run meets a
\ dictionary that already holds the name, still offers the body, and leaves the
\ record the file published exactly where it was.
: CASE-RENAME ( -- )
   ALREADY$ CENSUS1
   s" rename: the global definition was censused" T-LABEL
      0 CHAIN-CENSUS:DEF-NAME$ s" CENSUS-FIX-ALREADY" T$=
   s" rename: and compiled" T-LABEL
      0 CHAIN-CENSUS:DEF-CODE 0 T=
   s" CENSUS-FIX-ALREADY" XREF-FIND-INDEX {: first:n :}
   s" rename: the file's own record is in the dictionary" T-LABEL
      first 0 >= TTRUE
   ALREADY$ CENSUS1
   s" rename: a second census still offers the body" T-LABEL
      0 CHAIN-CENSUS:DEF-CODE 0 T=
   s" rename: the original name still denotes the same record" T-LABEL
      s" CENSUS-FIX-ALREADY" XREF-FIND-INDEX first T=
   s" rename: no duplicate was refused" T-LABEL
      CHAIN-CENSUS:RC-DUPLICATE CHAIN-CENSUS:COUNT-OF 0 T= ;

\ ---- a code the table does not name -------------------------------------------------------------------------
: CASE-UNLISTED ( -- )
   UNL$ CENSUS-REPORT {: ra:ptr ru:n :}
   s" unlisted: the run finished" T-LABEL
      CHAIN-CENSUS:BUCKETS 1 T=
   s" unlisted: with the code the chain really threw" T-LABEL
      0 CHAIN-CENSUS:BUCKET-CODE E-NFEED-LITERAL T=
   s" unlisted: no row names it" T-LABEL
      E-NFEED-LITERAL CHAIN-CENSUS:CLASS-OF CHAIN-CENSUS:CL-DIALECT T=
   s" unlisted: the report prints it raw" T-LABEL
      ra ru s" unlisted code -8405" LINT-CONTAINS? TTRUE ;

\ ---- a refusal that never reached the elaborator carries no spelling ------------------------------------------
\ The elaborator's refused-token record is cleared when its walk is ENTERED, so a
\ refusal raised before that - the tape's own producer declining a literal it
\ cannot read back - leaves the PREVIOUS definition's word sitting in the record.
\ This drives exactly that pair and pins that the second row carries no spelling.
\ Attributing the first row's word to the second would be the worst thing a census
\ can do: a real-looking number on the wrong definition.
: CASE-STALE ( -- )
   STALE$ CENSUS1
   s" stale: all three bodies were offered" T-LABEL
      0 CHAIN-CENSUS:FILE-EXAMINED 3 T=
   s" stale: the helper compiled" T-LABEL
      s" CFT-HELPER" CODE-AT-NAME 0 T=
   s" stale: the first global was refused by the elaborator, for its word" T-LABEL
      s" CFT-FIRST" SPELL-AT-NAME$ s" >r" T$=
   s" stale: the second was refused by the engine instead" T-LABEL
      s" CFT-SECOND" CODE-AT-NAME CHAIN-CENSUS:RC-UNDEFINED T=
   s" stale: and carries no spelling of its own" T-LABEL
      s" CFT-SECOND" SPELL-LEN-AT-NAME 0 T=
   \ The assertion the per-attempt clear exists for. Delete REFUSED-CLEAR from
   \ RUN-ONE and this goes to one: the engine-refused row arrives still holding
   \ the word the row before it was refused for.
   s" stale: no outcome carried a record it cannot have made" T-LABEL
      CHAIN-CENSUS:STALE-RECORDS 0 T=
   s" stale: the sub-histogram still adds up to its bucket" T-LABEL
      SPELL-TOTAL E-HIR-UNMODELED CHAIN-CENSUS:COUNT-OF T= ;

\ ---- paths that are not censusable at all --------------------------------------------------------------------
\ Three separate facts, three separate lines, and none of them a refusal by the
\ chain: a path that is not Habu source, a path that is not there, and a file that
\ is there and will not compile.
: CASE-POPULATION ( -- )
   ROOT$ s" nowhere.md" MISS-BUF JOIN-PATH MISS-U !
   MISS$ CENSUS1
   s" population: a path that is not Habu source is its own line" T-LABEL
      0 CHAIN-CENSUS:FILE-STATUS CHAIN-CENSUS:ST-NOT-SOURCE T=
   s" population: and is never loaded" T-LABEL
      CHAIN-CENSUS:DEFS 0 T=
   ROOT$ s" nowhere.f" MISS-BUF JOIN-PATH MISS-U !
   MISS$ CENSUS1
   s" population: a path that is not there is its own line" T-LABEL
      0 CHAIN-CENSUS:FILE-STATUS CHAIN-CENSUS:ST-MISSING T=
   SUITEF$ CENSUS1
   s" population: a suite is its own line" T-LABEL
      0 CHAIN-CENSUS:FILE-STATUS CHAIN-CENSUS:ST-SUITE T=
   s" population: and was never loaded" T-LABEL
      CHAIN-CENSUS:DEFS 0 T=
   \ src/core/include.f raises the include depth before it evaluates a file and
   \ lowers it after, with no lowering on the failure path - so a refused load
   \ leaks one level, sixteen of them exhaust INCLUDE-MAX-DEPTH, and the
   \ seventeenth load of a perfectly good file ends the process. A sweep meets
   \ sixteen easily; src/core plus src/habu holds twenty-four. So the census puts
   \ the level back, and this is what says it did.
   INCLUDE-DEPTH @ {: depth0:n :}
   BADLOAD$ CENSUS1
   s" population: a file that will not compile is its own line" T-LABEL
      0 CHAIN-CENSUS:FILE-STATUS CHAIN-CENSUS:ST-LOAD-REFUSED T=
   s" population: and refuses nothing" T-LABEL
      CHAIN-CENSUS:BUCKETS 0 T=
   s" population: a refused load leaves the include depth where it found it" T-LABEL
      INCLUDE-DEPTH @ depth0 T= ;

\ ---- the two ways a definition can be too long ----------------------------------------------------------
: CASE-LENGTH ( -- )
   LONG$ CENSUS1
   s" length: the body was offered" T-LABEL
      0 CHAIN-CENSUS:FILE-EXAMINED 1 T=
   s" length: and refused by the recorder that holds the tape" T-LABEL
      s" CFL-MID" CODE-AT-NAME E-NMIGRATE-TEXT T=
   s" length: and is classed as the instrument's refusal" T-LABEL
      E-NMIGRATE-TEXT CHAIN-CENSUS:CLASS-OF CHAIN-CENSUS:CL-INSTRUMENT T=
   s" length: nothing was blamed on the dialect" T-LABEL
      E-HIR-UNMODELED CHAIN-CENSUS:COUNT-OF 0 T= ;

\ ---- packages the census may not reopen -----------------------------------------------------------------
: CASE-CLOSED ( -- )
   SEALED$ CENSUS1
   s" sealed: nothing was offered to the chain" T-LABEL
      0 CHAIN-CENSUS:FILE-EXAMINED 0 T=
   s" sealed: the definition is counted on its own line" T-LABEL
      0 CHAIN-CENSUS:FILE-CLOSED 1 T=
   s" sealed: and is not a refusal" T-LABEL
      CHAIN-CENSUS:BUCKETS 0 T=
   HIDDEN$ CENSUS1
   s" hidden: a package with nothing public is skipped too" T-LABEL
      0 CHAIN-CENSUS:FILE-CLOSED 1 T=
   s" hidden: and nothing was offered" T-LABEL
      0 CHAIN-CENSUS:FILE-EXAMINED 0 T=
   \ The second seal, over the tree's own file. src/core/check-hook.f opens
   \ `package LOWER-CERT-HOOK`, a name the engine reserves outright - it is NOT a
   \ protected wordlist, so the bitmap alone lets it through and the reopen ends
   \ the process. There is no way to write this as a fixture: a file that opened
   \ a reserved package could not itself be loaded. So the subject is the real
   \ file, and taking the name gate out does not fail this case, it kills the run.
   s" src/core/check-hook.f" CENSUS1
   s" reserved: a reserved-name package is skipped, not opened" T-LABEL
      0 CHAIN-CENSUS:FILE-CLOSED 0 > TTRUE
   s" reserved: and nothing of it was offered" T-LABEL
      0 CHAIN-CENSUS:FILE-EXAMINED 0 T= ;

\ ---- the engine's reserved-package table, re-derived from the engine ----------------------------------------
\ The census holds a MIRROR of src/habu/habu2.f's baked RESTAB table, because that
\ table is private to `package KWDATA` inside the image and no running program can
\ read it. A mirror nobody checks is the thing that drifts and then kills a run
\ with an uncatchable exit 84, so this case reads the engine's own source and
\ rebuilds the table from it: the `create RESTAB-BUF` block, record by record,
\ `[len][len bytes]` to the zero terminator, exactly as habu2.f's comment
\ describes and as the native C-SEAL-MATCH walk reads it. tools/bootstrap-mirror-lint.f
\ is the tree's shape for "a claim about src/, checked by lexing src/", and this
\ follows it: the shared source lexer, token positions, no substring search.
\
\ IT IS THE BYTES AND NOT THE COMMENTS. Each record's letters are written as `$74
\ c,` hex cells with the spelling only in a trailing `\` comment, which the lexer
\ never emits - so a comment edited to lie about a record cannot make this pass,
\ and a record whose bytes changed cannot make it pass either.
create ENG-BUF 64 allot
variable ENG-U
variable ENG-I
variable ENG-N

: ENG$ ( -- ptr u8 n )
   ENG-BUF ENG-U @ ;

: HEX? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   u 2 < if false exit then
   a c@ 36 = ;

variable HEXACC

: HEX>N ( ptr u8 n -- n ) {: a:ptr u:n :}
   0 HEXACC !
   1 begin dup u < while
      HEXACC @ 4 lshift  over a + c@ CONTENT-KEY:HEX-NIB +  HEXACC !
      1+
   repeat drop
   HEXACC @ ;

\ One `N c,` or `$XX c,` cell. The `c,` is required, so a number that is not a
\ comma-store cell ends the block rather than being read as one.
: CELL-AT ( n -- n ) {: k:n :}
   k 1+ LINT-LEX:COUNT >= if -1 exit then
   k 1+ LINT-LEX:TOKEN s" c," LINT-STR= 0= if -1 exit then
   k LINT-LEX:TOKEN {: a:ptr u:n :}
   a u HEX? if a u HEX>N exit then
   a u STR>NUMBER? MATCH option
      none OF -1 ENDOF
      some OF ENDOF
   ;MATCH ;

: RESTAB-START ( -- n )   \ token index of the first cell after `create RESTAB-BUF`
   1 begin dup LINT-LEX:COUNT < while
      dup LINT-LEX:TOKEN s" RESTAB-BUF" LINT-STR= if
         dup 1- LINT-LEX:TOKEN s" create" LINT-STR= if 1+ exit then
      then
      1+
   repeat drop -1 ;

\ Read one record's bytes into ENG-BUF; answer the index just past it, or -1 at
\ the terminator or on anything that is not a record.
variable ENG-B

: RECORD-AT ( n -- n ) {: k:n :}
   0 ENG-U !
   k CELL-AT {: len:n :}
   len 0 <= if -1 exit then
   len 64 > if -1 exit then
   0 begin dup len < while
      dup 1+ 2 * k + CELL-AT ENG-B !
      ENG-B @ 0 < if drop -1 exit then
      ENG-B @ ENG-BUF ENG-U @ + c!
      ENG-U @ 1+ ENG-U !
      1+
   repeat drop
   ENG-U @ len <> if -1 exit then
   k len 1+ 2 * + ;

\ One record, read and held against the mirror's entry of the same position.
\ False at the table's zero terminator, which is what ends the walk.
: MIRROR-STEP ( -- bool )
   ENG-I @ RECORD-AT {: nxt:n :}
   nxt 0 < if false exit then
   nxt ENG-I !
   s" reserved: the mirror has a name for this record" T-LABEL
      ENG-N @ CHAIN-CENSUS:RESERVED < TTRUE
   ENG-N @ CHAIN-CENSUS:RESERVED < if
      s" reserved: and it is the record's own bytes" T-LABEL
         ENG-N @ CHAIN-CENSUS:RESERVED$ ENG$ T$=
   then
   ENG-N @ 1+ ENG-N !
   true ;

: CASE-RESERVED-MIRROR ( -- )
   s" src/habu/habu2.f" LINT-SOURCE:LOAD
   LINT-SOURCE:TEXT LINT-LEX:SOURCE
   s" reserved: the engine source lexed cleanly" T-LABEL
      LINT-LEX:ERROR? TFALSE
   RESTAB-START {: start:n :}
   s" reserved: the engine's RESTAB-BUF block was found" T-LABEL
      start 0 >= TTRUE
   start 0 < if exit then
   start ENG-I !
   0 ENG-N !
   begin MIRROR-STEP while repeat
   \ The loop above stops at the zero terminator, so the count it reached is the
   \ engine's own record count and nothing else.
   s" reserved: the mirror holds exactly the engine's records" T-LABEL
      ENG-N @ CHAIN-CENSUS:RESERVED T=
   s" reserved: and the engine really does hold some" T-LABEL
      ENG-N @ 0 > TTRUE ;

\ ---- the whole directory, in one sorted run --------------------------------------------------------------------
: PAIR-ORDERED? ( n -- bool ) {: k:n :}
   k CHAIN-CENSUS:FILE-PATH$ {: a:ptr u:n :}
   k 1+ CHAIN-CENSUS:FILE-PATH$ {: b:ptr v:n :}
   a u b v LINT-ORDER:CMP-CI 0 < ;

: ORDERED? ( -- bool )
   0 begin dup CHAIN-CENSUS:FILES 1- < while
      dup PAIR-ORDERED? 0= if drop false exit then
      1+
   repeat drop true ;

: BUCKETS-DESCENDING? ( -- bool )
   0 begin dup CHAIN-CENSUS:BUCKETS 1- < while
      dup CHAIN-CENSUS:BUCKET-N
      over 1+ CHAIN-CENSUS:BUCKET-N < if drop false exit then
      1+
   repeat drop true ;

: SPELLS-DESCENDING? ( -- bool )
   0 begin dup CHAIN-CENSUS:SPELLS 1- < while
      dup CHAIN-CENSUS:SPELL-N
      over 1+ CHAIN-CENSUS:SPELL-N < if drop false exit then
      1+
   repeat drop true ;

: ATTRIBUTED? ( -- bool )
   0 begin dup CHAIN-CENSUS:DEFS < while
      dup CHAIN-CENSUS:DEF-FILE {: f:n :}
      f 0 < f CHAIN-CENSUS:FILES >= or if drop false exit then
      dup 0 > if
         dup 1- CHAIN-CENSUS:DEF-FILE f > if drop false exit then
      then
      1+
   repeat drop true ;

\ Density order, checked by the same cross-multiplication the tool uses, plus the
\ rule the arithmetic alone cannot express: a file with nothing examined has no
\ density and comes after every file that has one. Before that rule existed such a
\ file compared equal to everything, which is not an ordering, and the sort built
\ on it printed a file of density one below a file of density 0.94.
: DENSE-PAIR? ( n -- bool ) {: k:n :}
   k CHAIN-CENSUS:DENSE-FILE {: l:n :}
   k 1+ CHAIN-CENSUS:DENSE-FILE {: r:n :}
   l CHAIN-CENSUS:FILE-EXAMINED {: le:n :}
   r CHAIN-CENSUS:FILE-EXAMINED {: re:n :}
   re 0= if true exit then
   le 0= if false exit then
   l CHAIN-CENSUS:FILE-REFUSED re *
   r CHAIN-CENSUS:FILE-REFUSED le * >= ;

: DENSE-ORDERED? ( -- bool )
   0 begin dup CHAIN-CENSUS:FILES 1- < while
      dup DENSE-PAIR? 0= if drop false exit then
      1+
   repeat drop true ;

: CASE-DIRECTORY ( -- )
   ROOT$ CENSUS1
   s" directory: every fixture was visited" T-LABEL
      CHAIN-CENSUS:FILES 14 T=
   s" directory: the alphabetically first file is reported first" T-LABEL
      0 CHAIN-CENSUS:FILE-PATH$ s" already.f" LINT-ENDS-WITH? TTRUE
   s" directory: the alphabetically last is reported last" T-LABEL
      13 CHAIN-CENSUS:FILE-PATH$ s" unlisted.f" LINT-ENDS-WITH? TTRUE
   s" directory: and the whole list is sorted" T-LABEL
      ORDERED? TTRUE
   s" directory: refusal buckets come out biggest first" T-LABEL
      BUCKETS-DESCENDING? TTRUE
   s" directory: so do the refused spellings" T-LABEL
      SPELLS-DESCENDING? TTRUE
   \ Exactly one, and it is the `using` row of stale.f - the one fixture written
   \ to produce it. Pinning the number rather than merely "some" is what makes a
   \ second unresolved name, from anywhere, a failure.
   s" directory: exactly one name went unresolved" T-LABEL
      CHAIN-CENSUS:RC-UNDEFINED CHAIN-CENSUS:COUNT-OF 1 T=
   s" directory: no rename collided" T-LABEL
      CHAIN-CENSUS:RC-DUPLICATE CHAIN-CENSUS:COUNT-OF 0 T=
   s" directory: the census never mis-stated an arity" T-LABEL
      E-NELAB-ARITY CHAIN-CENSUS:COUNT-OF 0 T=
   s" directory: no outcome carried a stale elaborator record" T-LABEL
      CHAIN-CENSUS:STALE-RECORDS 0 T=
   \ Each record has to point back at the file it came from, and the files are
   \ censused in the sorted order asserted above, so the file indices across the
   \ record table can only ever go forwards.
   s" directory: every record names the file it came from" T-LABEL
      ATTRIBUTED? TTRUE
   s" directory: files come out densest first, empty ones last" T-LABEL
      DENSE-ORDERED? TTRUE ;

\ ---- a capacity reached is not a capability missing ----------------------------
\ WHAT THIS IS ABOUT. A publication writes a row into two records that may not
\ drop one to make space, and both are full at some point. Their refusals -
\ E-NCLOB-CAP for the clobber record, E-NPUB-CAP for the replacement log - are
\ raised AFTER the definition has been selected, allocated, verified and emitted,
\ so a definition they refuse is one the chain compiled. While the code table did
\ not name them the report printed them raw, in the DIALECT bucket, which is where
\ a reader looks for what the chain still cannot compile: the 2026-08-07 whole-tree
\ run put 1275 of them there, three times the biggest real dialect gap, and the
\ compiled count it printed beside them was the size of the table rather than the
\ size of the tree.
\
\ AND THE SECOND HALF IS THAT A CENSUS CANNOT REACH THEM ANY MORE. It measures
\ through NMIGRATE:MEASURE-HELD, which makes every refusal a publication can make
\ and then writes none of the rows, so a whole census run costs neither record a
\ single row. That is asserted against the records themselves, before and after a
\ real run over a real file, rather than against the census's own report of
\ itself.
variable PUB-ROWS0
variable PUB-LOG0

: CASE-CEILINGS ( -- )
   s" ceilings: the clobber record's full table is a named instrument code" T-LABEL
      E-NCLOB-CAP CHAIN-CENSUS:CLASS-OF CHAIN-CENSUS:CL-INSTRUMENT T=
   s" ceilings: and so is the replacement log's" T-LABEL
      E-NPUB-CAP CHAIN-CENSUS:CLASS-OF CHAIN-CENSUS:CL-INSTRUMENT T=

   NCLOB:ROWS PUB-ROWS0 !
   NPUB:REPUBLISHED PUB-LOG0 !
   GOOD$ CENSUS1

   s" ceilings: a census that compiled definitions really did compile them" T-LABEL
      0 CHAIN-CENSUS:FILE-COMPILED 0 T<>

   s" ceilings: and it spent no row of the clobber record" T-LABEL
      NCLOB:ROWS PUB-ROWS0 @ T=
   s" ceilings: nor a row of the replacement log" T-LABEL
      NPUB:REPUBLISHED PUB-LOG0 @ T=
   s" ceilings: so neither ceiling was reached" T-LABEL
      E-NCLOB-CAP CHAIN-CENSUS:COUNT-OF 0 T=
      E-NPUB-CAP CHAIN-CENSUS:COUNT-OF 0 T= ;

public

: MAIN ( -- )
   T-RESET
   PREPARE
   CASE-GOOD
   CASE-MOD
   CASE-STRING
   CASE-PRIVATE
   CASE-REGISTERS
   CASE-HOSTILE
   CASE-RENAME
   CASE-UNLISTED
   CASE-STALE
   CASE-POPULATION
   CASE-LENGTH
   CASE-CLOSED
   CASE-RESERVED-MIRROR
   CASE-DIRECTORY
   CASE-CEILINGS
   CLEANUP-RUN
   T-REPORT
   s" chain-census-test: ok" type cr ;

;package
