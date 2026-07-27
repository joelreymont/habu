\ internal-word-gate.f - engine-internal word execution gate regressions (dot
\ habu-hb-crash-bare-c5be6634).
\
\ A word defined by the engine prefix with no checker-known effect (no
\ certified/trusted signature and no primitive axiom) carries DNAME-INT after
\ the seal-time marking pass (src/core/internal-mark.f). Interpret-mode
\ execution AND tick of such a word must fail closed with
\ `hb: internal engine word: <token>` + rc 70. Previously a bare `U-TYPE` in a
\ load file
\ consumed below-base garbage as type-term handles and corrupted the process
\ (wild loads/stores, SIGSEGV at pc=0), so the user-facing top-level name
\ universe now equals the checker's. Positives prove the public surface is
\ untouched: undefined words still report E-UNDEFINED, underflow still reports
\ E-UNDERFLOW, user unchecked words stay executable, top-level TRUST rows /
\ TRUSTED: / structures + type-family DSLs still work, and XREF introspection
\ of internal words survives.
\
\ Semantic cases run in disposable SUBJECT forks. Exact outcome/stdout/stderr
\ parity retains direct `--load` and stdin representatives below.
\
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/memory.f lib/fs.f
\   lib/fs-mutate.f lib/process.f lib/process-argv.f lib/process-env.f
\   test/internal-word-gate.f
\
\ Package ownership (dot habu-pkg-internal-word-da4149d9). The whole file is one
\ package, INTERNAL-WORD-GATE, and every word in it is private. This suite is a
\ leaf: a whole-tree sweep found no file that calls any word it defines, only
\ files that name its PATH (the gate slice in test/gate-stdlib-inline-lib.f, the
\ suite row in test/gate-stdlib-cases.f and FILEMAP.md). The owner replaces 85
\ raw global IWG- names and also absorbs the three small inner packages IWG-EXEC,
\ IWG-TOMB and IWG-PARITY, which could not remain nested inside a file-wide
\ owner.
\
\ Nothing is public: the package exports no words at all. The suite still has to
\ RUN at global scope, and that part is forced. Most cases here run their child
\ program in a disposable SUBJECT fork, and a fork inherits the parent's open
\ package scope; a child that opens its own package (the EXPORT cases forge
\ `package IWGXP ... ;package`) would then be opening a nested package and exit
\ 75 with the `package` diagnostic. Measured directly: forking with a package
\ open gives the child rc 75, forking after `;package` gives rc 0. What the fork
\ seam forces is the position of the runner, not an export. The standing pattern
\ carries a private word across the close as a value: `ACTION` returns a checked
\ quotation over MAIN, evaluating ACTION before `;package` leaves that quotation
\ on the stack, and `execute` on the line after the close runs it at global
\ scope. Same shape as test/run-worker-stdlib.f and test/gate-stdlib-cases.f.
\
\ Names. The 85 words that carried the IWG- stem simply dropped it. Each bare
\ tail was first checked against this file's real dependency image by a collision
\ oracle - one global definition attempt per tail, where a `duplicate definition`
\ reject means the name is already live - and all 85 came back free.
\
\ The other 11 renamed words are the ones the three dissolved packages held. They
\ never carried the stem, so there was nothing to strip; each needed a name that
\ still reads correctly outside its package. Three groups:
\ - RUN-SUBJECT, the dissolved-package sibling, was IWG-EXEC:SUBJECT. A bare
\   SUBJECT would read as the unrelated SUBJECT package whose SUBJECT:RUN this
\   very word calls. RUN-SUBJECT is simply the third sibling of RUN-LOAD and
\   RUN-STDIN.
\ - The two TOMB forge builders, TOMB-RETIRED-FORGE$ and TOMB-TEXT-FORGE$, were
\   IWG-TOMB:RETIRED$ and IWG-TOMB:TEXT$. Bare RETIRED$ and TEXT$ would sit among
\   twenty other *-FORGE$ child-source builders without saying they are builders
\   too, and TEXT$ reads like "the text" rather than "the tombstone spelled as
\   inert text".
\ - The eight PARITY- group words, anchored on PARITY-CHECK: the oracle found
\   CHECK already live in this image, so IWG-PARITY:CHECK had to change no matter
\   what. The rest of its group (DIRECT-N, SUBJECT-N, RESULT, NEG-LOAD, NEG-STDIN,
\   POS-LOAD, TEST) took the same PARITY- marker, because their dissolved package
\   supplied it and bare RESULT, TEST, NEG-LOAD and POS-LOAD would misdescribe
\   themselves standing next to the general RUN-LOAD and the file-wide assertions.
\
\ The child-process fixture strings are deliberately byte-identical to before.
\ Every IWG- name still inside an s" body - IWG-RAW, IWG-V, IWG-LV, IWG-PFV,
\ IWG-NO-SUCH-WORD and the rest - is a word in a program handed to a separate
\ bin/hb process, not a name in this file's namespace, so renaming them would
\ change what the children declare while proving nothing.

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/test/subject.f
require test/tail-ratchet.f
require lib/type/deftype.f         \ DEFTYPE - the declared-nominal exemplar used at top level

package INTERNAL-WORD-GATE

2048 constant CAP
70 constant REJECT-RC           \ interpret-level reject exit (RC-REJECT)
67 constant THROW-RC            \ engine uncaught-throw boundary exit

variable ROOT-U
variable CHILD-U
variable IN-U
variable OUT-U
variable ERR-U
variable EXITED                 \ bool: child completed by exit
variable RC

create ROOT-BUF FS-PATH-CAP allot
create CHILD-BUF FS-PATH-CAP allot
create IN CAP allot         \ stdin-piped program
create OUT CAP allot
create ERR CAP allot
create EMPTY 1 allot            \ zero-length stdin

: COPY! ( ptr u8 n ptr u8 ptr n -- ) {: src:ptr u:n dst:ptr lenp:ptr :}
   u FS-PATH-CAP > if E-FS-CAPACITY throw then
   src dst u BYTE-COPY
   u lenp ! ;

: ROOT ( -- ptr u8 n )
   ROOT-BUF ROOT-U @ ;

: CHILD ( -- ptr u8 n )
   CHILD-BUF CHILD-U @ ;

: IN$ ( -- ptr u8 n )
   IN IN-U @ ;

: ERR$ ( -- ptr u8 n )
   ERR ERR-U @ ;

\ Resolve the child engine: gate default env HABU_UNDER_TEST -> the candidate;
\ standalone runs fall back to bin/hb.
: HB$ ( -- ptr u8 n )
   s" HABU_UNDER_TEST" >LEN PROC-ENV-DEFAULT$? if LEN>N exit then
   2drop
   s" HABU_UNDER_TEST" GETENV dup 0= if
      2drop s" bin/hb" exit
   then ;

: STORE! ( len len outcome -- )
   MATCH outcome
     exited OF RC ! 0 0= EXITED ! ENDOF
     signaled OF RC ! 0 0= 0= EXITED ! ENDOF
     timeout OF 0 RC ! 0 0= 0= EXITED ! ENDOF
   ;MATCH
   LEN>N ERR-U !  LEN>N OUT-U ! ;

: IN! ( ptr u8 n -- ) {: a:ptr u:n :}
   u CAP > if E-FS-CAPACITY throw then
   a IN u BYTE-COPY
   u IN-U ! ;

\ Run the program as a --load file with empty stdin.
: RUN-LOAD ( ptr u8 n -- )
   TAIL-RATCHET:DIRECT
   CHILD 2swap WRITE-ALL
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   CHILD >LEN PROC-ARGV+
   HB$ >LEN  EMPTY 0 >LEN  OUT CAP >LEN
   ERR CAP >LEN  TAIL-BUDGET:TIMEOUT-MS >MS  RUN-ARGV-STDIN-CAPTURE-OUTCOME
   STORE! ;

\ Run the program as a piped stdin program (no --load), the other cold-prefix path.
: RUN-STDIN ( ptr u8 n -- )
   TAIL-RATCHET:DIRECT
   IN!
   PROC-ARGV-RESET
   HB$ >LEN  IN$ >LEN  OUT CAP >LEN
   ERR CAP >LEN  TAIL-BUDGET:TIMEOUT-MS >MS  RUN-ARGV-STDIN-CAPTURE-OUTCOME
   STORE! ;

\ The third child-run path, beside RUN-LOAD and RUN-STDIN above: a disposable
\ SUBJECT fork rather than a fresh engine process.
: RUN-SUBJECT ( ptr u8 n -- )
   TAIL-RATCHET:SUBJECT
   OUT CAP >LEN ERR CAP >LEN
   TAIL-BUDGET:TIMEOUT-MS >MS SUBJECT:RUN STORE! ;

: LF ( -- )
   10 SB-APPEND-C ;

: TOKEN$ ( ptr u8 n -- ptr u8 n )    \ program = the bare token on one line
   SB-RESET
   SB-APPEND LF
   SB$ ;

: ASSERT-INTERNAL ( ptr u8 n -- ) {: a:ptr u:n :}   \ fail-closed reject naming the word
   EXITED @ TTRUE
   RC @ REJECT-RC T=
   ERR$ s" hb: internal engine word: " CONTAINS? TTRUE
   ERR$ a u CONTAINS? TTRUE ;

: ASSERT-OK ( -- )
   EXITED @ TTRUE
   RC @ 0 T= ;

: NEG ( ptr u8 n -- ) {: a:ptr u:n :}
   a u TOKEN$ RUN-SUBJECT
   a u ASSERT-INTERNAL ;

\ --- negatives: internal checker words fail closed before their body runs ---

: NEG-BARE ( -- )
   s" bare U-TYPE fails closed (was SIGSEGV)" T-LABEL
   s" U-TYPE" NEG
   s" bare T-RES fails closed" T-LABEL
   s" T-RES" NEG
   s" bare PAIR fails closed" T-LABEL
   s" PAIR" NEG
   s" bare CHECKER-FIND-ACTIVE-SIG fails closed" T-LABEL
   s" CHECKER-FIND-ACTIVE-SIG" NEG
   s" bare E-INST fails closed" T-LABEL
   s" E-INST" NEG
   s" bare CT-LIVE? field-liveness query fails closed" T-LABEL
   s" CT-LIVE?" NEG ;

: ARGS-FORGE$ ( -- ptr u8 n )        \ args present: the gate is not depth-keyed
   SB-RESET
   s" 1 2 U-TYPE" SB-APPEND LF
   SB$ ;

: TICK-FORGE$ ( -- ptr u8 n )        \ tick would launder the xt to execute
   SB-RESET
   s" ' U-TYPE" SB-APPEND LF
   SB$ ;

: PRIM-FORGE$ ( -- ptr u8 n )        \ the marking prim itself is sealed
   SB-RESET
   s" 0 int-mark" SB-APPEND LF
   SB$ ;

: NEG-SHAPES ( -- )
   s" 1 2 U-TYPE (satisfied depth) still fails closed" T-LABEL
   ARGS-FORGE$ RUN-SUBJECT
   s" U-TYPE" ASSERT-INTERNAL
   s" ' U-TYPE (tick laundering) fails closed" T-LABEL
   TICK-FORGE$ RUN-SUBJECT
   s" U-TYPE" ASSERT-INTERNAL
   s" 0 int-mark: the marking prim is itself internal" T-LABEL
   PRIM-FORGE$ RUN-SUBJECT
   s" int-mark" ASSERT-INTERNAL ;

\ --- positives: the public top-level surface is untouched -------------------

: ASSERT-DIAG ( ptr u8 n -- ) {: a:ptr u:n :}   \ child rejected rc 70 with the given diagnostic
   EXITED @ TTRUE
   RC @ REJECT-RC T=
   ERR$ a u CONTAINS? TTRUE ;

: UNDEF-FORGE$ ( -- ptr u8 n )
   SB-RESET
   s" IWG-NO-SUCH-WORD" SB-APPEND LF
   SB$ ;

: RAW-FORGE$ ( -- ptr u8 n )         \ user unchecked word stays executable
   SB-RESET
   s" 0 set-check" SB-APPEND LF
   s" : IWG-RAW 42 . cr ;" SB-APPEND LF
   s" IWG-RAW" SB-APPEND LF
   SB$ ;

: TRUST-FORGE$ ( -- ptr u8 n )       \ top-level TRUST row still works
   SB-RESET
   s" variable IWG-V" SB-APPEND LF
   S\" s\" IWG-V\" s\" -- ptr a\" TRUST" SB-APPEND LF
   SB$ ;

: TRUSTED-FORGE$ ( -- ptr u8 n )     \ TRUSTED: definition + bare call still work
   SB-RESET
   s" TRUSTED: IWG-T ( -- n ) 3 ;" SB-APPEND LF
   s" IWG-T . cr" SB-APPEND LF
   SB$ ;

: STRUCT-FORGE$ ( -- ptr u8 n )      \ structures DSL still works at top level
   SB-RESET
   s" BEGIN-STRUCTURE IWG-PT" SB-APPEND LF
   s"    CELL +FIELD IWG-PT.X" SB-APPEND LF
   s" END-STRUCTURE" SB-APPEND LF
   SB$ ;

: SUMTYPE-FORGE$ ( -- ptr u8 n )     \ type-family DSL still works at top level
   SB-RESET
   s" SUMTYPE iwgfoo 1 VARIANT iwgbar a ;VARIANT ;SUMTYPE" SB-APPEND LF
   SB$ ;

: XREF-FORGE$ ( -- ptr u8 n )        \ introspection of internal words survives
   SB-RESET
   s" XREF U-TYPE" SB-APPEND LF
   SB$ ;

: LBUF-FORGE$ ( -- ptr u8 n )        \ LAYOUT-BUFFER DSL still works at top level
   SB-RESET
   s" ENUM iwgclr iwgred iwgblue ;ENUM" SB-APPEND LF
   s" 4 LAYOUT-BUFFER IWG-LB iwgclr" SB-APPEND LF
   s" 3 IWG-LB drop" SB-APPEND LF
   SB$ ;

: LBUF-BODY-FORGE$ ( -- ptr u8 n )   \ layout-buffer inside a checked body is unsafe
   SB-RESET
   s" : IWG-LBAD ( n -- ) LAYOUT-BUFFER ;" SB-APPEND LF
   SB$ ;

\ --- type-DSL openers are top-level-interpret-only: their ( -- ) axiom rows
\ hide runtime input parsing + registry mutation, so UNSAFE-TOK? rejects them
\ inside checked bodies (dot habu-checker-in-body-af7cf855, LBUF parity). ----

: TDSL-TOP-FORGE$ ( -- ptr u8 n )    \ NEWTYPE/PRODUCT still work at top level
   SB-RESET
   s" NEWTYPE iwgtf 0" SB-APPEND LF
   s" PRODUCT iwgpr 0 FIELD x n ;PRODUCT" SB-APPEND LF
   SB$ ;

\ --- the retired NEWTYPE spelling. Until 2026-07-26 this definer was called
\ TYPEFAMILY. Joel ruled (dot habu-rename-typefamily-definer-538979cc) that the
\ arity-0 nominal wrapper definer is permanent and renamed it to NEWTYPE with no
\ alias and no compatibility definer, because in type theory a type family is a
\ type-level function while this declaration is exactly a Haskell newtype. The
\ old spelling is therefore not a word at all, and the child below proves it on
\ the real load path: rc 70 with the ordinary undefined-word diagnostic naming
\ the token, having declared nothing. The companion child proves the tombstone is
\ structural rather than textual - the same characters in a line comment and in a
\ string literal are inert text, and a real NEWTYPE declaration beside them still
\ loads, so the child exits 0. ----

: TOMB-RETIRED-FORGE$ ( -- ptr u8 n )   \ the retired spelling is an undefined word
   SB-RESET
   s" TYPEFAMILY iwgtomb 0" SB-APPEND LF
   SB$ ;

: TOMB-TEXT-FORGE$ ( -- ptr u8 n )      \ same characters as comment and string text
   SB-RESET
   s" \ TYPEFAMILY iwgtombc 0" SB-APPEND LF
   S\" : IWG-TOMB-TEXT ( -- ptr u8 n ) s\" TYPEFAMILY iwgtombs 0\" ;" SB-APPEND LF
   s" NEWTYPE iwgtombok 0" SB-APPEND LF
   SB$ ;

: PF-REFLECT-FORGE$ ( -- ptr u8 n )  \ packaged committed reflection is checked/public
   SB-RESET
   s" PRODUCT iwgpf 0 FIELD x n ;PRODUCT" SB-APPEND LF
   s" : IWG-PF-READ ( -- n )" SB-APPEND LF
   s"    TYPE-FIELD:COUNT drop" SB-APPEND LF
   s"    TFAM-N@ 1 - TYPE-FIELD:NO-VARIANT 0 TYPE-FIELD:EACH if drop else drop then" SB-APPEND LF
   S\"    TFAM-N@ 1 - TYPE-FIELD:NO-VARIANT s\" x\" TYPE-FIELD:FIND" SB-APPEND LF
   s"    if dup TYPE-FIELD:FAMILY@ drop dup TYPE-FIELD:VARIANT@ drop" SB-APPEND LF
   s"       dup TYPE-FIELD:NAME$ 2drop dup TYPE-FIELD:SCHEMA@ drop" SB-APPEND LF
   s"       dup TYPE-FIELD:CELLS@ drop dup TYPE-FIELD:BYTE-OFF@ drop" SB-APPEND LF
   s"       dup TYPE-FIELD:BYTES@ drop dup TYPE-FIELD:ALIGN@ drop" SB-APPEND LF
   s"       dup TYPE-FIELD:FLAGS@ drop TYPE-FIELD:SLOT@ else drop -1 then ;" SB-APPEND LF
   s" IWG-PF-READ . cr" SB-APPEND LF
   SB$ ;

: PF-RAW-FORGE$ ( -- ptr u8 n )      \ raw implementation names are not checked/public
   s" : IWG-PF-RAW ( n n ptr u8 n -- n bool ) PF-FIND ;" ;

\ The DEFTYPE nominal surface and the roles.f DEFLINEAR/VALUE-RECORD definers
\ are the same hazard class as the openers but admitted via certified usigs, not
\ PRIM: axioms (dot habu-checker-deftype-deflinear-8e9d1dc5): declare + use each
\ at top level.

: ROLES-TOP-FORGE$ ( -- ptr u8 n )   \ DEFTYPE/DEFLINEAR/VALUE-RECORD at top level
   SB-RESET
   s" DEFTYPE iwgid" SB-APPEND LF
   s" : IWG-ID-RT ( n -- n ) >iwgid iwgid>N ;" SB-APPEND LF
   s" 7 IWG-ID-RT . cr" SB-APPEND LF
   s" DEFLINEAR iwgown" SB-APPEND LF
   s" : IWG-OWN-PASS ( iwgown -- iwgown ) ;" SB-APPEND LF
   s" VALUE-RECORD iwgpt x n y n END-VALUE-RECORD" SB-APPEND LF
   s" : IWG-PT-KEEP ( iwgpt -- iwgpt ) ;" SB-APPEND LF
   SB$ ;

\ EXPORT must not mint a qualified alias for an UNSAFE-TOK? name: the alias
\ spelling (PKG:NAME) would escape the name-keyed body reject (dot
\ habu-checker-unsafety-must-d12bc784 part a). Pinned like export-package.f's
\ E-EXPORT-PRIM case: uncaught named code E-EXPORT-UNSAFE 7120 -> rc 67.

: EXPORT-FORGE$ ( ptr u8 n -- ptr u8 n )   \ "package .. EXPORT <name> ;package"
   SB-RESET
   s" package IWGXP" SB-APPEND LF
   s" public" SB-APPEND LF
   s" EXPORT " SB-APPEND SB-APPEND LF
   s" ;package" SB-APPEND LF
   SB$ ;

: ASSERT-EXPORT-UNSAFE ( -- )        \ child threw E-EXPORT-UNSAFE (code named)
   EXITED @ TTRUE
   RC @ THROW-RC T=
   ERR$ s" 7120" CONTAINS? TTRUE ;

: NEG-EXPORT ( ptr u8 n -- )         \ EXPORT of an unsafe name rejects
   EXPORT-FORGE$ RUN-SUBJECT
   ASSERT-EXPORT-UNSAFE ;

: EXPORT-BODY-FORGE$ ( -- ptr u8 n ) \ alias-minting line is the reject; the body never checks
   SB-RESET
   s" package IWGXP" SB-APPEND LF
   s" public" SB-APPEND LF
   s" EXPORT DEFLINEAR" SB-APPEND LF
   s" ;package" SB-APPEND LF
   s" : IWG-XBAD ( -- ) IWGXP:DEFLINEAR ;" SB-APPEND LF
   SB$ ;

: EXPORT-OK-FORGE$ ( -- ptr u8 n )   \ EXPORT of a normal checked word still works
   SB-RESET
   s" package IWGXA" SB-APPEND LF
   s" public" SB-APPEND LF
   s" : IWGW ( -- n ) 3 ;" SB-APPEND LF
   s" ;package" SB-APPEND LF
   s" package IWGXB" SB-APPEND LF
   s" public" SB-APPEND LF
   s" EXPORT IWGXA:IWGW" SB-APPEND LF
   s" ;package" SB-APPEND LF
   s" : IWG-XOK ( -- n ) IWGXB:IWGW ;" SB-APPEND LF
   s" IWG-XOK . cr" SB-APPEND LF
   SB$ ;

: OPENER-BODY-FORGE$ ( ptr u8 n -- ptr u8 n )   \ ": IWG-OBAD ( -- ) <opener> ;"
   SB-RESET
   s" : IWG-OBAD ( -- ) " SB-APPEND
   SB-APPEND
   s"  ;" SB-APPEND LF
   SB$ ;

: OPENER-DIAG$ ( ptr u8 n -- ptr u8 n )   \ pinned reject site "at '<opener>'"
   SB-RESET
   s" at '" SB-APPEND
   SB-APPEND
   s" '" SB-APPEND
   SB$ ;

: NEG-OPENER ( ptr u8 n -- )         \ in-body opener at empty declared stack rejects
   2dup OPENER-BODY-FORGE$ RUN-SUBJECT
   OPENER-DIAG$ ASSERT-DIAG ;

: OPENER-CASES ( -- )
   s" NEWTYPE/PRODUCT DSL still works at top level" T-LABEL
   TDSL-TOP-FORGE$ RUN-SUBJECT ASSERT-OK
   s" the retired TYPEFAMILY spelling is an undefined word" T-LABEL
   TOMB-RETIRED-FORGE$ RUN-SUBJECT
   s" E-UNDEFINED: TYPEFAMILY" ASSERT-DIAG
   s" the retired spelling as comment or string text declares nothing" T-LABEL
   TOMB-TEXT-FORGE$ RUN-SUBJECT ASSERT-OK
   s" packaged field reflection is checked/public" T-LABEL
   PF-REFLECT-FORGE$ RUN-LOAD ASSERT-OK
   s" raw field reflection is unavailable to checked code" T-LABEL
   PF-RAW-FORGE$ RUN-LOAD
   s" at 'PF-FIND'" ASSERT-DIAG
   s" NEWTYPE in a checked body is rejected unsafe" T-LABEL
   s" NEWTYPE" NEG-OPENER
   s" SUMTYPE in a checked body is rejected unsafe" T-LABEL
   s" SUMTYPE" NEG-OPENER
   s" ENUM in a checked body is rejected unsafe" T-LABEL
   s" ENUM" NEG-OPENER
   s" PRODUCT in a checked body is rejected unsafe" T-LABEL
   s" PRODUCT" NEG-OPENER
   s" DEFTYPE/DEFLINEAR/VALUE-RECORD still work at top level" T-LABEL
   ROLES-TOP-FORGE$ RUN-SUBJECT ASSERT-OK
   s" DEFLINEAR in a checked body is rejected unsafe" T-LABEL
   s" DEFLINEAR" NEG-OPENER
   s" VALUE-RECORD in a checked body is rejected unsafe" T-LABEL
   s" VALUE-RECORD" NEG-OPENER
   s" cast: in a checked body is rejected unsafe" T-LABEL
   s" cast:" NEG-OPENER
   s" DEFER-LAYOUT-BUFFER in a checked body is rejected unsafe" T-LABEL
   s" DEFER-LAYOUT-BUFFER" NEG-OPENER
   s" EXPORT of a checked word still works" T-LABEL
   EXPORT-OK-FORGE$ RUN-SUBJECT ASSERT-OK
   s" EXPORT DEFLINEAR rejects E-EXPORT-UNSAFE" T-LABEL
   s" DEFLINEAR" NEG-EXPORT
   s" EXPORT VALUE-RECORD rejects E-EXPORT-UNSAFE" T-LABEL
   s" VALUE-RECORD" NEG-EXPORT
   s" EXPORT cast: rejects E-EXPORT-UNSAFE" T-LABEL
   s" cast:" NEG-EXPORT
   s" EXPORT SUMTYPE rejects E-EXPORT-UNSAFE" T-LABEL
   s" SUMTYPE" NEG-EXPORT
   s" EXPORT ENUM rejects E-EXPORT-UNSAFE" T-LABEL
   s" ENUM" NEG-EXPORT
   s" EXPORT PRODUCT rejects E-EXPORT-UNSAFE" T-LABEL
   s" PRODUCT" NEG-EXPORT
   s" EXPORT NEWTYPE rejects E-EXPORT-UNSAFE" T-LABEL
   s" NEWTYPE" NEG-EXPORT
   s" EXPORT DEFER-LAYOUT-BUFFER rejects E-EXPORT-UNSAFE" T-LABEL
   s" DEFER-LAYOUT-BUFFER" NEG-EXPORT
   s" qualified unsafe alias for a body cannot be minted" T-LABEL
   EXPORT-BODY-FORGE$ RUN-SUBJECT
   ASSERT-EXPORT-UNSAFE ;

\ --- defer/is laundering (dot habu-checker-unsafety-as-1c537c1f, acceptance b).
\ A checked `is` installs a QUOTATION whose body is checked, and a raw xt from
\ tick has a different checker type than a quotation — so neither `['] <unsafe>
\ is X` nor `[: <unsafe> ;] is X` can bind an unsafe target from a checked body.
\ The first rejects at `is` (xt is not a quotation), the second rejects at the
\ unsafe token inside the quotation body (identity/name reject). Both are
\ in-body checker rejects: rc 70 with an `at '<token>'` diagnostic. ----
: DEFER-TICK-FORGE$ ( -- ptr u8 n )  \ ['] <unsafe> is X : rejects at 'is'
   SB-RESET
   s" defer IWG-DACT ( -- )" SB-APPEND LF
   s" : IWG-DSET ( -- ) ['] deflinear is IWG-DACT ;" SB-APPEND LF
   SB$ ;

: DEFER-QUOT-FORGE$ ( -- ptr u8 n )  \ [: <unsafe> ;] is X : rejects at 'deflinear'
   SB-RESET
   s" defer IWG-DACT2 ( -- )" SB-APPEND LF
   s" : IWG-DSET2 ( -- ) [: deflinear ;] is IWG-DACT2 ;" SB-APPEND LF
   SB$ ;

: DEFER-CASES ( -- )
   s" ['] <unsafe> is X (tick laundering) rejects at 'is'" T-LABEL
   DEFER-TICK-FORGE$ RUN-SUBJECT
   s" at 'is'" ASSERT-DIAG
   s" [: <unsafe> ;] is X (quotation laundering) rejects at 'deflinear'" T-LABEL
   DEFER-QUOT-FORGE$ RUN-SUBJECT
   s" at 'deflinear'" ASSERT-DIAG ;

\ --- laundered-execute closure (dot habu-checker-exec-of-5923c543). Executing an
\ xt fetched from an untyped variable is now rejected at CHECK time as
\ E-EXEC-OPAQUE-XT (RSEXEC T-VAR flip), so the definer-mint and protected-registry
\ writes the RCA laundered through `variable V  ' W V !  : F V @ execute ;` can no
\ longer certify. Each child rejects rc 70 with the named diagnostic before the
\ body runs, so the mint / store never happens. ----

: LAUNDER-DEFINER$ ( ptr u8 n -- ptr u8 n )   \ tick <definer> into a var, launder its execute
   {: a:ptr u:n :}
   SB-RESET
   s" variable IWG-LV" SB-APPEND LF
   s" ' " SB-APPEND  a u SB-APPEND  s"  IWG-LV !" SB-APPEND LF
   s" : IWG-LBAD ( -- ) IWG-LV @ execute ;" SB-APPEND LF
   SB$ ;

: PF-LAUNDER-FORGE$ ( -- ptr u8 n )   \ tick the protected PF cell writer, launder its execute
   SB-RESET
   s" variable IWG-PFV" SB-APPEND LF
   s" : IWG-PFSET ( -- ) ['] PF-COMMIT-N IWG-PFV ! ;" SB-APPEND LF
   s" IWG-PFSET" SB-APPEND LF
   s" : IWG-PFBAD ( -- ) IWG-PFV @ execute 99 swap ! ;" SB-APPEND LF
   SB$ ;

: CATCH-DEFINER$ ( ptr u8 n -- ptr u8 n )   \ tick <definer> into a var, launder its catch
   {: a:ptr u:n :}
   SB-RESET
   s" variable IWG-LV" SB-APPEND LF
   s" ' " SB-APPEND  a u SB-APPEND  s"  IWG-LV !" SB-APPEND LF
   s" : IWG-LBAD ( -- n ) IWG-LV @ catch ;" SB-APPEND LF
   SB$ ;

: ASSERT-OPAQUE ( -- )   \ child rejected at CHECK (rc 70) naming the opaque-execute reject
   EXITED @ TTRUE
   RC @ REJECT-RC T=
   ERR$ s" at 'execute'" CONTAINS? TTRUE
   ERR$ s" opaque xt of unknown provenance" CONTAINS? TTRUE ;

: ASSERT-OPAQUE-CATCH ( -- )   \ child rejected at CHECK (rc 70) naming the opaque-catch reject
   EXITED @ TTRUE
   RC @ REJECT-RC T=
   ERR$ s" at 'catch'" CONTAINS? TTRUE
   ERR$ s" opaque xt of unknown provenance" CONTAINS? TTRUE ;

: LAUNDER-CASES ( -- )
   s" deflinear laundered through a variable rejects at CHECK (E-EXEC-OPAQUE-XT)" T-LABEL
   s" deflinear" LAUNDER-DEFINER$ RUN-SUBJECT
   ASSERT-OPAQUE
   s" value-record laundered through a variable rejects at CHECK" T-LABEL
   s" VALUE-RECORD" LAUNDER-DEFINER$ RUN-SUBJECT
   ASSERT-OPAQUE
   s" layout-buffer laundered through a variable rejects at CHECK" T-LABEL
   s" LAYOUT-BUFFER" LAUNDER-DEFINER$ RUN-SUBJECT
   ASSERT-OPAQUE
   s" PF registry-cell write laundered through a variable rejects at CHECK, not runtime" T-LABEL
   PF-LAUNDER-FORGE$ RUN-SUBJECT
   ASSERT-OPAQUE
   s" deflinear laundered through a variable + catch also rejects at CHECK" T-LABEL
   s" deflinear" CATCH-DEFINER$ RUN-SUBJECT
   ASSERT-OPAQUE-CATCH ;

: POSITIVES ( -- )
   s" undefined word still reports E-UNDEFINED" T-LABEL
   UNDEF-FORGE$ RUN-SUBJECT
   s" E-UNDEFINED" ASSERT-DIAG
   s" bare drop still reports E-UNDERFLOW" T-LABEL
   s" drop" TOKEN$ RUN-SUBJECT
   s" E-UNDERFLOW" ASSERT-DIAG
   s" user unchecked word stays executable at top level" T-LABEL
   RAW-FORGE$ RUN-SUBJECT ASSERT-OK
   s" top-level TRUST row still works" T-LABEL
   TRUST-FORGE$ RUN-SUBJECT ASSERT-OK
   s" TRUSTED: + bare call still work" T-LABEL
   TRUSTED-FORGE$ RUN-SUBJECT ASSERT-OK
   s" structures DSL still works" T-LABEL
   STRUCT-FORGE$ RUN-SUBJECT ASSERT-OK
   s" SUMTYPE DSL still works" T-LABEL
   SUMTYPE-FORGE$ RUN-SUBJECT ASSERT-OK
   s" LAYOUT-BUFFER DSL still works" T-LABEL
   LBUF-FORGE$ RUN-SUBJECT ASSERT-OK
   s" layout-buffer in a checked body is rejected unsafe" T-LABEL
   LBUF-BODY-FORGE$ RUN-SUBJECT
   s" at 'LAYOUT-BUFFER'" ASSERT-DIAG
   s" XREF of an internal word still works" T-LABEL
   XREF-FORGE$ RUN-SUBJECT ASSERT-OK ;

\ --- registry write-protection (dot habu-protect-type-field-04d91409, Layer 1).
\ A din=0 registry control cell (variable/create) used to stay executable at top
\ level because the internal-word pass exempts data records; REG-PROTECT +
\ IMK-SEAL-REGISTRY now mark the PF cells DNAME-INT, so a bare cell name and the
\ confirmed `99 PF-COMMIT-N !` write both fail closed (rc 70, internal engine
\ word) on --load and stdin, while checked user code still rejects them earlier
\ as non-certified (E-UNDEFINED, covered by OPENER-CASES' PF-FIND row). ----
: PF-EXPLOIT-FORGE$ ( -- ptr u8 n )   \ the confirmed exploit: a bare registry-cell write
   SB-RESET
   s" 99 PF-COMMIT-N !" SB-APPEND LF
   SB$ ;

: REGISTRY-CASES ( -- )
   s" bare PF-CAP-V registry cell fails closed" T-LABEL
   s" PF-CAP-V" NEG
   s" bare PF-A-BOOT arena fails closed" T-LABEL
   s" PF-A-BOOT" NEG
   s" bare PF-A-P arena base fails closed" T-LABEL
   s" PF-A-P" NEG
   s" bare PF-N registry cell fails closed" T-LABEL
   s" PF-N" NEG
   s" bare PF-COMMIT-N registry cell fails closed" T-LABEL
   s" PF-COMMIT-N" NEG
   s" bare PF-TX-CAP-V registry cell fails closed" T-LABEL
   s" PF-TX-CAP-V" NEG
   s" bare PF-TX-BOOT arena fails closed" T-LABEL
   s" PF-TX-BOOT" NEG
   s" bare PF-TX-P arena base fails closed" T-LABEL
   s" PF-TX-P" NEG
   s" bare PF-TX-DEPTH registry cell fails closed" T-LABEL
   s" PF-TX-DEPTH" NEG
   s" bare PF-TX-SERIAL registry cell fails closed" T-LABEL
   s" PF-TX-SERIAL" NEG
   s" registry-cell write exploit fails closed on --load" T-LABEL
   PF-EXPLOIT-FORGE$ RUN-LOAD
   s" PF-COMMIT-N" ASSERT-INTERNAL
   s" registry-cell write exploit fails closed on stdin" T-LABEL
   PF-EXPLOIT-FORGE$ RUN-STDIN
   s" PF-COMMIT-N" ASSERT-INTERNAL ;

\ --- sibling type-registry write-protection (dot habu-protect-sibling-type-44eec932).
\ The family (TFAM), sum-variant (SUMV), interned-string, param-kind, logical-layout
\ (src/core/type-family.f) and schema/schema-root (src/core/type-schema.f) registries
\ are as exposed as PF-COMMIT-N was: each control cell is a din=0 data record the
\ internal-word pass would leave executable. REG-PROTECT + IMK-SEAL-REGISTRY now seal
\ every cell DNAME-INT, so a bare cell name, a bare `99 <cell> !` write, and a bare
\ `' <cell>` tick all fail closed (rc 70, internal engine word) exactly like the PF
\ cells. Compiled cold-prefix writers and the certified accessors (TFAM-N@, SUMV-N@,
\ TF-STR-U@, TF-PK-N@, SCHEMA-N@, SCHEMA-ROOT-N@) are unaffected. ----
: WRITE-FORGE$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}   \ program "99 <cell> !"
   SB-RESET
   s" 99 " SB-APPEND  a u SB-APPEND  s"  !" SB-APPEND LF
   SB$ ;

: TICK-CELL-FORGE$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}   \ program "' <cell>"
   SB-RESET
   s" ' " SB-APPEND  a u SB-APPEND LF
   SB$ ;

: CELL-WT ( ptr u8 n -- ) {: a:ptr u:n :}   \ bare write AND bare tick both fail closed naming the cell
   a u WRITE-FORGE$ RUN-SUBJECT       a u ASSERT-INTERNAL
   a u TICK-CELL-FORGE$ RUN-SUBJECT   a u ASSERT-INTERNAL ;

: TFAM-CASES ( -- )
   s" bare TF-CAP-V family-registry capacity cell fails closed" T-LABEL   s" TF-CAP-V" NEG
   s" bare TF-A-BOOT family arena fails closed" T-LABEL                    s" TF-A-BOOT" NEG
   s" bare TF-A-P family arena base fails closed" T-LABEL                  s" TF-A-P" NEG
   s" bare TFAM-N family high-water fails closed" T-LABEL                  s" TFAM-N" NEG
   s" TFAM-N high-water write + tick fail closed" T-LABEL                  s" TFAM-N" CELL-WT ;

: SUMV-CASES ( -- )
   s" bare SUMV-CAP-V variant-registry capacity cell fails closed" T-LABEL  s" SUMV-CAP-V" NEG
   s" bare SUMV-A-BOOT variant arena fails closed" T-LABEL                  s" SUMV-A-BOOT" NEG
   s" bare SUMV-A-P variant arena base fails closed" T-LABEL                s" SUMV-A-P" NEG
   s" bare SUMV-N variant high-water fails closed" T-LABEL                  s" SUMV-N" NEG
   s" SUMV-N high-water write + tick fail closed" T-LABEL                   s" SUMV-N" CELL-WT ;

: STR-CASES ( -- )
   s" bare TF-STR-CAP-V string-pool capacity cell fails closed" T-LABEL   s" TF-STR-CAP-V" NEG
   s" bare TF-STR-BOOT string pool fails closed" T-LABEL                  s" TF-STR-BOOT" NEG
   s" bare TF-STR-P string-pool base fails closed" T-LABEL               s" TF-STR-P" NEG
   s" bare TF-STR-U string-pool high-water fails closed" T-LABEL         s" TF-STR-U" NEG
   s" TF-STR-U high-water write + tick fail closed" T-LABEL              s" TF-STR-U" CELL-WT ;

: PK-CASES ( -- )
   s" bare TF-PK-CAP-V param-pool capacity cell fails closed" T-LABEL   s" TF-PK-CAP-V" NEG
   s" bare TF-PK-BOOT param pool fails closed" T-LABEL                  s" TF-PK-BOOT" NEG
   s" bare TF-PK-P param-pool base fails closed" T-LABEL               s" TF-PK-P" NEG
   s" bare TF-PK-N param-pool high-water fails closed" T-LABEL         s" TF-PK-N" NEG
   s" TF-PK-N high-water write + tick fail closed" T-LABEL             s" TF-PK-N" CELL-WT ;

: LAY-CASES ( -- )
   s" bare LAY-CAP-V layout-registry capacity cell fails closed" T-LABEL   s" LAY-CAP-V" NEG
   s" bare LAY-A-BOOT layout arena fails closed" T-LABEL                   s" LAY-A-BOOT" NEG
   s" bare LAY-A-P layout arena base fails closed" T-LABEL                 s" LAY-A-P" NEG
   s" bare LAY-N layout high-water fails closed" T-LABEL                   s" LAY-N" NEG
   s" LAY-N high-water write + tick fail closed" T-LABEL                   s" LAY-N" CELL-WT ;

: SCH-CASES ( -- )
   s" bare SCH-CAP-V schema-registry capacity cell fails closed" T-LABEL     s" SCH-CAP-V" NEG
   s" bare SCH-A-BOOT schema arena fails closed" T-LABEL                     s" SCH-A-BOOT" NEG
   s" bare SCH-A-P schema arena base fails closed" T-LABEL                   s" SCH-A-P" NEG
   s" bare SCH-N schema node high-water fails closed" T-LABEL                s" SCH-N" NEG
   s" SCH-N node high-water write + tick fail closed" T-LABEL                s" SCH-N" CELL-WT
   s" bare SCH-ROOT-CAP-V schema-root capacity cell fails closed" T-LABEL    s" SCH-ROOT-CAP-V" NEG
   s" bare SCH-ROOT-BOOT schema-root pool fails closed" T-LABEL              s" SCH-ROOT-BOOT" NEG
   s" bare SCH-ROOT-P schema-root base fails closed" T-LABEL                 s" SCH-ROOT-P" NEG
   s" bare SCH-ROOT-N schema-root high-water fails closed" T-LABEL           s" SCH-ROOT-N" NEG
   s" SCH-ROOT-N high-water write + tick fail closed" T-LABEL                s" SCH-ROOT-N" CELL-WT ;

: SIBLING-CASES ( -- )
   TFAM-CASES
   SUMV-CASES
   STR-CASES
   PK-CASES
   LAY-CASES
   SCH-CASES ;

\ --- field-liveness internalization (dot habu-internalize-field-liveness).
\ src/core/checker.f used to publish a global CT-LIVE? primitive-effect axiom
\ solely so the field-schema validator (type-family.f PF-NODE-KIND?) could ask
\ whether a SCHEMA-CON's concrete-type code is live. That axiom left the
\ checker-internal concrete-type registry query top-level executable and callable
\ from checked user code. The axiom is gone, so CT-LIVE? now carries DNAME-INT
\ like its sibling CT-LINEAR? — a bare call fails closed on BOTH cold-prefix
\ paths (--load file and piped stdin) while the compiled field-validation caller
\ is unaffected. ----
: CTLIVE-CASES ( -- )
   s" CT-LIVE? field-liveness query fails closed on --load" T-LABEL
   s" CT-LIVE?" TOKEN$ RUN-LOAD
   s" CT-LIVE?" ASSERT-INTERNAL
   s" CT-LIVE? field-liveness query fails closed on stdin" T-LABEL
   s" CT-LIVE?" TOKEN$ RUN-STDIN
   s" CT-LIVE?" ASSERT-INTERNAL ;

\ --- direct/subject parity. The PARITY- group used to be its own package; the
\ names keep that marker because they are about the direct-versus-fork
\ comparison, not about running a child in general. ----

9 constant PARITY-DIRECT-N
103 constant PARITY-SUBJECT-N   \ +2: the retired-TYPEFAMILY reject and its inert-text control

: PARITY-RESULT ( -- ptr u8 n ptr u8 n n )
   OUT OUT-U @ ERR ERR-U @ RC @ ;

: PARITY-NEG-LOAD ( ptr u8 n -- )
   2dup RUN-LOAD
   s" U-TYPE" ASSERT-INTERNAL
   PARITY-RESULT TAIL-RATCHET:SNAPSHOT
   RUN-SUBJECT
   s" U-TYPE" ASSERT-INTERNAL
   PARITY-RESULT TAIL-RATCHET:SAME ;

: PARITY-NEG-STDIN ( ptr u8 n -- )
   2dup RUN-STDIN
   s" U-TYPE" ASSERT-INTERNAL
   PARITY-RESULT TAIL-RATCHET:SNAPSHOT
   RUN-SUBJECT
   s" U-TYPE" ASSERT-INTERNAL
   PARITY-RESULT TAIL-RATCHET:SAME ;

: PARITY-POS-LOAD ( ptr u8 n -- )
   2dup RUN-LOAD ASSERT-OK
   PARITY-RESULT TAIL-RATCHET:SNAPSHOT
   RUN-SUBJECT ASSERT-OK
   PARITY-RESULT TAIL-RATCHET:SAME ;

: PARITY-TEST ( -- )
   s" direct --load and subject preserve raw internal-word results" T-LABEL
   s" U-TYPE" TOKEN$ PARITY-NEG-LOAD
   s" direct stdin and subject preserve raw internal-word results" T-LABEL
   s" U-TYPE" TOKEN$ PARITY-NEG-STDIN
   s" direct --load and subject preserve raw successful results" T-LABEL
   RAW-FORGE$ PARITY-POS-LOAD ;

: PARITY-CHECK ( -- )
   PARITY-DIRECT-N PARITY-SUBJECT-N TAIL-RATCHET:CHECK ;

: PREPARE ( -- )
   CLEANUP-RESET
   s" habu-iwg" TMPDIR-MKDIR {: a:ptr u:n :}
   a u ROOT-BUF ROOT-U COPY!
   ROOT CLEANUP-TREE+
   ROOT s" forge.f" CHILD-BUF JOIN-PATH CHILD-U ! ;

: CLEANUP ( -- )
   CLEANUP-RUN
   ROOT EXISTS? TFALSE ;

: MAIN ( -- )
   T-RESET
   TAIL-RATCHET:START
   PREPARE
   PARITY-TEST
   NEG-BARE
   REGISTRY-CASES
   SIBLING-CASES
   CTLIVE-CASES
   NEG-SHAPES
   POSITIVES
   OPENER-CASES
   DEFER-CASES
   LAUNDER-CASES
   PARITY-CHECK
   CLEANUP
   T-REPORT
   s" internal-word-gate: ok" type cr ;

\ The suite has to run after the package closes - see the header note on SUBJECT
\ forks - and MAIN is private, so it cannot be named from out there. ACTION hands
\ the runner across the boundary as a checked quotation: evaluating ACTION here
\ leaves the quotation on the stack, and `execute` below runs it at global scope.
: ACTION ( -- [ -- ] )
   [: MAIN ;] ;

ACTION

;package

execute
