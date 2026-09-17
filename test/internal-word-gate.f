\ internal-word-gate.f - engine-internal word execution gate regressions (dot
\ habu-hb-crash-bare-c5be6634).
\
\ A word defined by the engine prefix with no checker-known effect (no
\ certified/trusted signature and no primitive axiom) carries DNAME-INT after
\ the seal-time marking pass (src/core/internal-mark.f) — whether its top-level
\ spelling is a bare global name or a package public's qualified PKG:TAIL one
\ (SEAL-CASES below). Interpret-mode
\ execution AND tick of such a word must fail closed with
\ `hb: internal engine word: <token>` + rc 70. Previously a bare `U-TYPE` in a
\ load file
\ consumed below-base garbage as type-term handles and corrupted the process
\ (wild loads/stores, SIGSEGV at pc=0), so the user-facing top-level name
\ universe now equals the checker's. Positives prove the public surface is
\ untouched: undefined words still report E-UNDEFINED, underflow still reports
\ E-UNDERFLOW, inferred user words stay executable, top-level TRUST rows /
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
\ INTERNAL-WORD-GATE privately owns every definition in this file, exports
\ nothing, and has no external callers.

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
require test/tail-parity.f
require lib/type/deftype.f         \ DEFTYPE - the declared-nominal exemplar used at top level

package INTERNAL-WORD-GATE

2048 constant CAP
30000 constant TIMEOUT-MS
70 constant REJECT-RC           \ interpret-level reject exit (RC-REJECT)
67 constant THROW-RC            \ engine uncaught-throw boundary exit
84 constant SEAL-RC             \ ENGINE-ERROR:SEAL-PACKAGE, the reserved-name guard

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

: OUT$ ( -- ptr u8 n )
   OUT OUT-U @ ;

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
   CHILD 2swap WRITE-ALL
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   CHILD >LEN PROC-ARGV+
   HB$ >LEN  EMPTY 0 >LEN  OUT CAP >LEN
   ERR CAP >LEN  TIMEOUT-MS >MS  RUN-ARGV-STDIN-CAPTURE-OUTCOME
   STORE! ;

\ Run the program as a piped stdin program (no --load), the other cold-prefix path.
: RUN-STDIN ( ptr u8 n -- )
   IN!
   PROC-ARGV-RESET
   HB$ >LEN  IN$ >LEN  OUT CAP >LEN
   ERR CAP >LEN  TIMEOUT-MS >MS  RUN-ARGV-STDIN-CAPTURE-OUTCOME
   STORE! ;

\ The third child-run path, beside RUN-LOAD and RUN-STDIN above: a disposable
\ SUBJECT fork rather than a fresh engine process.
: RUN-SUBJECT ( ptr u8 n -- )
   OUT CAP >LEN ERR CAP >LEN
   TIMEOUT-MS >MS SUBJECT:RUN STORE! ;

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

: ASSERT-DIAG ( ptr u8 n -- ) {: a:ptr u:n :}   \ child rejected rc 70 with the given diagnostic
   EXITED @ TTRUE
   RC @ REJECT-RC T=
   ERR$ a u CONTAINS? TTRUE ;

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
   s" CT-LIVE?" NEG
   s" the prefix cursor reader stays internal: only REWIND carries a row" T-LABEL
   s" CHECKER-BOUND:CURSORS" NEG ;

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
   s" int-mark" ASSERT-INTERNAL
   s" 0 0 min-in-mark: arity marking is internal too" T-LABEL
   s" 0 0 min-in-mark" RUN-SUBJECT
   s" min-in-mark" ASSERT-INTERNAL ;

: SEED-RESET-TICK$ ( -- ptr u8 n )
   SB-RESET
   s" ' seed-ndict!" SB-APPEND LF
   SB$ ;

: SEED-RESET-CHECKED$ ( -- ptr u8 n )
   SB-RESET
   s" : IWG-SEED-RESET ( n -- ) seed-ndict! ;" SB-APPEND LF
   SB$ ;

: SEED-RESET-SEARCH$ ( -- ptr u8 n )
   S\" s\" seed-ndict!\" 0 search-wl . cr" ;

: SEED-RESET-EQUAL$ ( -- ptr u8 n )
   SB-RESET
   s" TRUSTED: IWG-SEED-EQUAL ( -- ) ndict@ seed-ndict! ;" SB-APPEND LF
   s" IWG-SEED-EQUAL" SB-APPEND LF
   SB$ ;

: NULL-PTR-BARE$ ( -- ptr u8 n )
   SB-RESET
   s" 1 NULL-PTR-CELL !" SB-APPEND LF
   SB$ ;

: NULL-PTR-TICK$ ( -- ptr u8 n )
   SB-RESET
   s" 1 ' NULL-PTR-CELL !" SB-APPEND LF
   SB$ ;

: NULL-PTR-SEARCH$ ( -- ptr u8 n )
   S\" s\" NULL-PTR-CELL\" 0 search-wl . cr" ;

: NULL-PTR-FIXED-MUTATE$ ( -- ptr u8 n )
   SB-RESET
   s" : IWG-NULL-MUTATE ( -- ) 1 NULL-PTR-CELL ! ;" SB-APPEND LF
   s" IWG-NULL-MUTATE" SB-APPEND LF
   SB$ ;

: NULL-PTR-FIXED@$ ( -- ptr u8 n )
   SB-RESET
   s" TRUSTED: IWG-NULL-READ ( -- n ) NULL-PTR-CELL @ ;" SB-APPEND LF
   s" IWG-NULL-READ . cr" SB-APPEND LF
   SB$ ;

\ The cell is a reserved DATA header offset now, and pointer-storage.f has to
\ spell that offset for itself because src/habu/layout.f loads about forty files
\ later in the target prefix. Read all three out of this booted engine, where
\ they are all live -- the cell's actual address, pointer-storage.f's copy, and
\ layout.f's reservation -- and refuse any drift between them.
: NULL-PTR-OFFSET$ ( -- ptr u8 n )
   SB-RESET
   s" TRUSTED: IWG-NULL-AT ( -- n ) NULL-PTR-CELL data-base - NULL-PTR-CELL-OFF - ;" SB-APPEND LF
   s" TRUSTED: IWG-NULL-MIRROR ( -- n ) NULL-PTR-OFF NULL-PTR-CELL-OFF - ;" SB-APPEND LF
   s" IWG-NULL-AT . cr IWG-NULL-MIRROR . cr" SB-APPEND LF
   SB$ ;

: NULL-PTR-CELL-CASES ( -- )
   s" a bare NULL-PTR-CELL store is engine-internal" T-LABEL
   NULL-PTR-BARE$ RUN-SUBJECT
   s" NULL-PTR-CELL" ASSERT-INTERNAL
   s" a tick-derived NULL-PTR-CELL store is engine-internal" T-LABEL
   NULL-PTR-TICK$ RUN-SUBJECT
   s" NULL-PTR-CELL" ASSERT-INTERNAL
   s" search-wl cannot launder a NULL-PTR-CELL store" T-LABEL
   NULL-PTR-SEARCH$ RUN-SUBJECT
   ASSERT-OK
   OUT$ S\" 0\n\n" T$=
   s" checked source cannot compile a NULL-PTR-CELL store" T-LABEL
   NULL-PTR-FIXED-MUTATE$ RUN-SUBJECT
   s" E-UNDEFINED" ASSERT-DIAG
   ERR$ s" NULL-PTR-CELL" CONTAINS? TTRUE
   s" a compiled cell store respects the engine's protected span" T-LABEL
   s" TRUSTED: IWG-BAND-STORE ( -- ) 1 data-base FRIEND-ARENA + ! ; IWG-BAND-STORE" RUN-SUBJECT
   EXITED @ TTRUE
   RC @ ENGINE-ERROR:SEAL-VIOLATION T=
   s" a compiled byte store respects the same protected span" T-LABEL
   s" TRUSTED: IWG-BAND-BYTE ( -- ) 1 data-base FRIEND-ARENA + c! ; IWG-BAND-BYTE" RUN-SUBJECT
   EXITED @ TTRUE
   RC @ ENGINE-ERROR:SEAL-VIOLATION T=
   s" NULL-PTR-CELL remains numeric zero" T-LABEL
   NULL-PTR-FIXED@$ RUN-SUBJECT
   ASSERT-OK
   OUT$ S\" 0\n\n" T$=
   s" NULL-PTR-CELL sits at the layout-reserved offset, and both spellings agree" T-LABEL
   NULL-PTR-OFFSET$ RUN-SUBJECT
   ASSERT-OK
   OUT$ S\" 0\n\n0\n\n" T$= ;

\ seed-ndict! has a global record so the native compiler can resolve the direct
\ call in a TRUSTED reset. Its DNAME-INT record and BSWL refusal close ordinary
\ interpretation, tick, checked-source, and dynamic-lookup routes. TRUSTED:
\ remains authority by the documented design.
: SEED-RESET-CASES ( -- )
   s" bare seed-ndict! is engine-internal" T-LABEL
   s" seed-ndict!" NEG
   s" tick of seed-ndict! is engine-internal" T-LABEL
   SEED-RESET-TICK$ RUN-SUBJECT
   s" seed-ndict!" ASSERT-INTERNAL
   s" checked source cannot compile seed-ndict!" T-LABEL
   SEED-RESET-CHECKED$ RUN-SUBJECT
   \ The engine now refuses this internal call before the checker sees its
   \ trusted-only primitive row; the checker verdict is covered separately.
   s" E-UNDEFINED: seed-ndict!" ASSERT-DIAG
   s" search-wl cannot launder seed-ndict! to execute" T-LABEL
   SEED-RESET-SEARCH$ RUN-SUBJECT
   ASSERT-OK
   OUT$ S\" 0\n\n" T$=
   s" trusted seed-ndict! refuses an equal dictionary count" T-LABEL
   SEED-RESET-EQUAL$ RUN-SUBJECT
   EXITED @ TTRUE
   RC @ 74 T= ;

\ --- positives: the public top-level surface is untouched -------------------

: UNDEF-FORGE$ ( -- ptr u8 n )
   SB-RESET
   s" IWG-NO-SUCH-WORD" SB-APPEND LF
   SB$ ;

: RAW-FORGE$ ( -- ptr u8 n )         \ an ordinary inferred word stays executable
   SB-RESET
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
   s"    TFAM:TFAM-N@ 1 - TYPE-FIELD:NO-VARIANT 0 TYPE-FIELD:EACH if drop else drop then" SB-APPEND LF
   S\"    TFAM:TFAM-N@ 1 - TYPE-FIELD:NO-VARIANT s\" x\" TYPE-FIELD:FIND" SB-APPEND LF
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

\ A name that is not a word at all cannot be aliased, and the engine says so
\ before the checker is asked: EXPORT's own lookup misses and the load dies
\ naming the token. This is the STRONGER half of the two laundering routes —
\ nonexistence rather than a blocklist entry.
: ASSERT-EXPORT-ABSENT ( ptr u8 n -- ) {: a:ptr u:n :}
   EXITED @ TTRUE
   RC @ REJECT-RC T=
   ERR$ a u CONTAINS? TTRUE ;

: NEG-EXPORT-ABSENT ( ptr u8 n -- )
   2dup EXPORT-FORGE$ RUN-SUBJECT
   ASSERT-EXPORT-ABSENT ;

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
   \ The raw implementation name is still refused, and the refusal moved EARLIER
   \ than the checker. Before the TFAM seal PF-FIND was a global the checker had
   \ no signature for, so the reject arrived from the publish hook as
   \ `non-certified definition: iwg-pf-raw at 'PF-FIND'`. It is a package private
   \ now, so the ENGINE's own find misses the token first and the body never
   \ compiles: E-UNDEFINED, which is the answer an absent name gets.
   s" raw field reflection is unavailable to checked code" T-LABEL
   PF-RAW-FORGE$ RUN-LOAD
   s" E-UNDEFINED" ASSERT-DIAG
   ERR$ s" PF-FIND" CONTAINS? TTRUE
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
   \ `cast:` is an interpret-mode reader keyword (habu2.f EM-INTERPRET-DEFINE-KEYWORDS)
   \ and not a dictionary word, so a colon body holding it resolves no name and the
   \ ENGINE answers before the checker: E-UNDEFINED naming the token, the same shift
   \ PF-FIND made above. The declarer's own unsafe-token reject stays pinned on the
   \ candidate path by test/cast-negative-suite.f.
   s" cast: in a checked body is an undefined word" T-LABEL
   s" cast:" OPENER-BODY-FORGE$ RUN-SUBJECT
   s" E-UNDEFINED: cast:" ASSERT-DIAG
   s" DEFER-LAYOUT-BUFFER in a checked body is rejected unsafe" T-LABEL
   s" DEFER-LAYOUT-BUFFER" NEG-OPENER
   s" EXPORT of a checked word still works" T-LABEL
   EXPORT-OK-FORGE$ RUN-SUBJECT ASSERT-OK
   s" EXPORT DEFLINEAR rejects E-EXPORT-UNSAFE" T-LABEL
   s" DEFLINEAR" NEG-EXPORT
   s" EXPORT VALUE-RECORD rejects E-EXPORT-UNSAFE" T-LABEL
   s" VALUE-RECORD" NEG-EXPORT
   s" EXPORT cast: has no word to alias" T-LABEL
   s" cast:" NEG-EXPORT-ABSENT
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

\ Both a tick and an inline quotation reject unsafe callback bodies.
: DEFER-TICK-FORGE$ ( -- ptr u8 n )  \ the unsafe tick rejects before binding
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
   s" ['] <unsafe> is X rejects at the unsafe tick" T-LABEL
   DEFER-TICK-FORGE$ RUN-SUBJECT
   s" at '[']'" ASSERT-DIAG
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

\ The tick itself refuses the protected target, so it never reaches the untyped
\ cell: habu2.f C-BTICK carries the DNAME-INT dispatch gate C-TICK applies to a
\ bare tick. Until it did, this source stored the xt and the interpret-level
\ `IWG-PFV @ execute` returned the protected cell's address (measured).
: PF-LAUNDER-FORGE$ ( -- ptr u8 n )   \ tick a protected registry cell, launder its execute
   SB-RESET
   s" variable IWG-PFV" SB-APPEND LF
   s" : IWG-PFSET ( -- ) ['] SCHEMA-REG:SCH-N IWG-PFV ! ;" SB-APPEND LF
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
   s" protected registry target cannot be ticked into a variable" T-LABEL
   PF-LAUNDER-FORGE$ RUN-SUBJECT
   s" SCHEMA-REG:SCH-N" ASSERT-INTERNAL
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
   s" inferred user word stays executable at top level" T-LABEL
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

\ --- sibling type-registry write-protection (dot habu-protect-sibling-type-44eec932).
\ The family (TFAM), sum-variant (SUMV), interned-string, param-kind, logical-layout
\ (src/core/type-family.f) and schema/schema-root (src/core/type-schema.f) registries
\ are as exposed as PF-COMMIT-N was: each control cell is a din=0 data record the
\ internal-word pass would leave executable. REG-PROTECT + IMK-SEAL-REGISTRY now seal
\ every cell DNAME-INT, so a bare cell name, a bare `99 <cell> !` write, and a bare
\ `' <cell>` tick all fail closed (rc 70, internal engine word) exactly like the PF
\ cells. Compiled cold-prefix writers and the certified accessors (TFAM-N@, SUMV-N@,
\ TF-STR-U@, TF-PK-N@, SCHEMA-N@, SCHEMA-ROOT-N@) are unaffected. ----
\ EVERY REGISTRY CELL BELOW NOW LIVES IN A PACKAGE — SCHEMA-REG for the schema
\ half (dot habu-seal-type-schema-c65f76cc) and TFAM for the family, variant,
\ string-pool, param-pool, layout and product-field halves (this dot) — so each
\ one has TWO spellings to refuse and they are refused by different mechanisms.
\ The bare tail is E-UNDEFINED because no global record carries it any more; that
\ is the seal, and it is a strictly stronger answer than the `internal engine
\ word` these cases used to assert. The qualified tail is either the REG-PROTECTed
\ public record (the control cells the rest of the checker reads) or E-UNDEFINED
\ again (the arenas and bases, which are private and have no qualified spelling at
\ all). Both legs are asserted for every cell: dropping the bare leg would miss a
\ cell that silently escaped back to global scope, dropping the qualified one
\ would miss a private that was published by mistake.
\
\ Bare `'` is deliberately NOT asserted on a sealed tail. Tick of a name that does
\ not exist exits 0 on this engine (it does so on master too, for any spelling),
\ so a bare tick case would pass for the wrong reason once the name is gone. The
\ tick exploit is kept where the record still resolves: the qualified public.
64 constant QNAME-CAP
create QNAME QNAME-CAP allot

: QUAL-NAME$ ( ptr u8 n ptr u8 n -- ptr u8 n )   \ "PKG:" prefix, tail -> PKG:tail
   {: pa:ptr pu:n a:ptr u:n :}
   pu u + QNAME-CAP > if E-FS-CAPACITY throw then
   pa QNAME pu BYTE-COPY
   a QNAME pu + u BYTE-COPY
   QNAME pu u + ;

: QUAL-PROG$ ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- ptr u8 n )  \ "<pre>PKG:<tail><post>"
   {: xa:ptr xu:n pa:ptr pu:n ta:ptr tu:n sa:ptr su:n :}
   SB-RESET
   xa xu SB-APPEND  pa pu ta tu QUAL-NAME$ SB-APPEND  sa su SB-APPEND LF
   SB$ ;

: ASSERT-UNDEF ( ptr u8 n -- ) {: a:ptr u:n :}   \ child rejected: no such top-level name
   SB-RESET
   s" E-UNDEFINED: " SB-APPEND  a u SB-APPEND
   SB$ ASSERT-DIAG ;

: ASSERT-SEALED ( ptr u8 n -- ) {: a:ptr u:n :}   \ engine reserved-name guard: rc 84 naming the token
   EXITED @ TTRUE
   RC @ SEAL-RC T=
   ERR$ a u CONTAINS? TTRUE ;

: CELL-PUB ( ptr u8 n ptr u8 n -- ) {: pa:ptr pu:n a:ptr u:n :}   \ REG-PROTECTed public control cell
   a u TOKEN$ RUN-SUBJECT                                a u ASSERT-UNDEF
   s" " pa pu a u s" " QUAL-PROG$ RUN-SUBJECT            pa pu a u QUAL-NAME$ ASSERT-INTERNAL
   s" 99 " pa pu a u s"  !" QUAL-PROG$ RUN-SUBJECT       pa pu a u QUAL-NAME$ ASSERT-INTERNAL ;

: CELL-PRIV ( ptr u8 n ptr u8 n -- ) {: pa:ptr pu:n a:ptr u:n :}  \ arena or base: private, unspellable
   a u TOKEN$ RUN-SUBJECT                                a u ASSERT-UNDEF
   s" " pa pu a u s" " QUAL-PROG$ RUN-SUBJECT            pa pu a u QUAL-NAME$ ASSERT-UNDEF ;

\ The tick leg splits by OWNER, and the split is the engine's, not a convention.
\ `tfam` is one of the seven reserved system-package names baked into habu2.f
\ KWDATA:RESTAB-BUF, so C-QUALIFY-SEAL-GUARD refuses `' TFAM:<anything>` with
\ ENGINE-ERROR:SEAL-PACKAGE (rc 84) BEFORE any lookup — a strictly stronger
\ answer than the marked record's rc 70, and one that covers privates too.
\ SCHEMA-REG is not a reserved name, so its tick reaches the marked record and
\ answers `internal engine word`, which is a PER-CELL fact (it is the record's own
\ DNAME-INT flag) and is asserted for every schema cell. The TFAM answer is not a
\ per-cell fact at all — it is one guard reading one name table — so it is
\ asserted once on a public and once on a private in TFAM-SEAL-CASES below, not
\ twenty times here. Twenty children for one rule is cost without evidence.
: TF-CELL-PUB ( ptr u8 n -- )    s" TFAM:" 2swap CELL-PUB ;
: TF-CELL-PRIV ( ptr u8 n -- )   s" TFAM:" 2swap CELL-PRIV ;

: SCH-CELL-PUB ( ptr u8 n -- ) {: a:ptr u:n :}
   s" SCHEMA-REG:" a u CELL-PUB
   s" ' " s" SCHEMA-REG:" a u s" " QUAL-PROG$ RUN-SUBJECT
   s" SCHEMA-REG:" a u QUAL-NAME$ ASSERT-INTERNAL ;

: SCH-CELL-PRIV ( ptr u8 n -- )  s" SCHEMA-REG:" 2swap CELL-PRIV ;

: TFAM-CASES ( -- )
   s" TFAM-N family high-water: bare gone, qualified write + tick marked" T-LABEL s" TFAM-N" TF-CELL-PUB
   s" TF-CAP-V family capacity cell is private on both spellings" T-LABEL    s" TF-CAP-V" TF-CELL-PRIV
   s" TF-A-BOOT family arena is private on both spellings" T-LABEL           s" TF-A-BOOT" TF-CELL-PRIV
   s" TF-A-P family arena base is private on both spellings" T-LABEL         s" TF-A-P" TF-CELL-PRIV ;

: SUMV-CASES ( -- )
   s" SUMV-N variant high-water: bare gone, qualified write + tick marked" T-LABEL s" SUMV-N" TF-CELL-PUB
   s" SUMV-CAP-V variant capacity cell is private on both spellings" T-LABEL s" SUMV-CAP-V" TF-CELL-PRIV
   s" SUMV-A-BOOT variant arena is private on both spellings" T-LABEL        s" SUMV-A-BOOT" TF-CELL-PRIV
   s" SUMV-A-P variant arena base is private on both spellings" T-LABEL      s" SUMV-A-P" TF-CELL-PRIV ;

: STR-CASES ( -- )
   s" TF-STR-U pool high-water: bare gone, qualified write + tick marked" T-LABEL s" TF-STR-U" TF-CELL-PUB
   s" TF-STR-CAP-V pool capacity cell is private on both spellings" T-LABEL  s" TF-STR-CAP-V" TF-CELL-PRIV
   s" TF-STR-BOOT string pool is private on both spellings" T-LABEL          s" TF-STR-BOOT" TF-CELL-PRIV
   s" TF-STR-P string-pool base is private on both spellings" T-LABEL        s" TF-STR-P" TF-CELL-PRIV ;

: PK-CASES ( -- )
   s" TF-PK-N pool high-water: bare gone, qualified write + tick marked" T-LABEL s" TF-PK-N" TF-CELL-PUB
   s" TF-PK-CAP-V pool capacity cell is private on both spellings" T-LABEL   s" TF-PK-CAP-V" TF-CELL-PRIV
   s" TF-PK-BOOT param pool is private on both spellings" T-LABEL            s" TF-PK-BOOT" TF-CELL-PRIV
   s" TF-PK-P param-pool base is private on both spellings" T-LABEL          s" TF-PK-P" TF-CELL-PRIV ;

: LAY-CASES ( -- )
   s" LAY-N layout high-water: bare gone, qualified write + tick marked" T-LABEL s" LAY-N" TF-CELL-PUB
   s" LAY-CAP-V layout capacity cell is private on both spellings" T-LABEL   s" LAY-CAP-V" TF-CELL-PRIV
   s" LAY-A-BOOT layout arena is private on both spellings" T-LABEL          s" LAY-A-BOOT" TF-CELL-PRIV
   s" LAY-A-P layout arena base is private on both spellings" T-LABEL        s" LAY-A-P" TF-CELL-PRIV ;

: SCH-CASES ( -- )
   s" SCH-CAP-V capacity cell: bare gone, qualified marked" T-LABEL         s" SCH-CAP-V" SCH-CELL-PUB
   s" SCH-N node high-water: bare gone, qualified write + tick marked" T-LABEL s" SCH-N" SCH-CELL-PUB
   s" SCH-ROOT-CAP-V capacity cell: bare gone, qualified marked" T-LABEL    s" SCH-ROOT-CAP-V" SCH-CELL-PUB
   s" SCH-ROOT-N high-water: bare gone, qualified write + tick marked" T-LABEL s" SCH-ROOT-N" SCH-CELL-PUB
   s" SCH-A-BOOT schema arena is private on both spellings" T-LABEL         s" SCH-A-BOOT" SCH-CELL-PRIV
   s" SCH-A-P schema arena base is private on both spellings" T-LABEL       s" SCH-A-P" SCH-CELL-PRIV
   s" SCH-ROOT-BOOT schema-root pool is private on both spellings" T-LABEL  s" SCH-ROOT-BOOT" SCH-CELL-PRIV
   s" SCH-ROOT-P schema-root base is private on both spellings" T-LABEL     s" SCH-ROOT-P" SCH-CELL-PRIV ;

\ --- product-field registry write-protection (dot habu-protect-type-field-04d91409,
\ Layer 1). A din=0 registry control cell (variable/create) used to stay executable
\ at top level because the internal-word pass exempts data records; REG-PROTECT +
\ IMK-SEAL-REGISTRY marked the PF cells DNAME-INT, and the TFAM seal took the bare
\ spelling away on top of that. The confirmed exploit `99 PF-COMMIT-N !` therefore
\ has two answers to pin: E-UNDEFINED bare, and the older `internal engine word`
\ under the qualified spelling that still resolves. Both are asserted on --load and
\ on stdin, because the cold-prefix paths are separate. ----
: PF-EXPLOIT-FORGE$ ( -- ptr u8 n )   \ the confirmed exploit: a bare registry-cell write
   SB-RESET
   s" 99 PF-COMMIT-N !" SB-APPEND LF
   SB$ ;

: PF-QEXPLOIT-FORGE$ ( -- ptr u8 n )  \ the same write under the qualified spelling
   SB-RESET
   s" 99 TFAM:PF-COMMIT-N !" SB-APPEND LF
   SB$ ;

: REGISTRY-CASES ( -- )
   s" PF-N field high-water: bare gone, qualified write + tick marked" T-LABEL  s" PF-N" TF-CELL-PUB
   s" PF-COMMIT-N commit cursor: bare gone, qualified marked" T-LABEL           s" PF-COMMIT-N" TF-CELL-PUB
   s" PF-TX-CAP-V transaction capacity: bare gone, qualified marked" T-LABEL    s" PF-TX-CAP-V" TF-CELL-PUB
   s" PF-TX-P transaction base: bare gone, qualified marked" T-LABEL            s" PF-TX-P" TF-CELL-PUB
   s" PF-TX-DEPTH transaction depth: bare gone, qualified marked" T-LABEL       s" PF-TX-DEPTH" TF-CELL-PUB
   s" PF-TX-SERIAL transaction serial: bare gone, qualified marked" T-LABEL     s" PF-TX-SERIAL" TF-CELL-PUB
   s" PF-CAP-V field capacity cell is private on both spellings" T-LABEL        s" PF-CAP-V" TF-CELL-PRIV
   s" PF-A-BOOT field arena is private on both spellings" T-LABEL               s" PF-A-BOOT" TF-CELL-PRIV
   s" PF-A-P field arena base is private on both spellings" T-LABEL             s" PF-A-P" TF-CELL-PRIV
   s" PF-TX-BOOT transaction arena is private on both spellings" T-LABEL        s" PF-TX-BOOT" TF-CELL-PRIV
   s" registry-cell write exploit has no bare name left on --load" T-LABEL
   PF-EXPLOIT-FORGE$ RUN-LOAD
   s" PF-COMMIT-N" ASSERT-UNDEF
   s" registry-cell write exploit has no bare name left on stdin" T-LABEL
   PF-EXPLOIT-FORGE$ RUN-STDIN
   s" PF-COMMIT-N" ASSERT-UNDEF
   s" the qualified write still fails closed on --load" T-LABEL
   PF-QEXPLOIT-FORGE$ RUN-LOAD
   s" TFAM:PF-COMMIT-N" ASSERT-INTERNAL
   s" the qualified write still fails closed on stdin" T-LABEL
   PF-QEXPLOIT-FORGE$ RUN-STDIN
   s" TFAM:PF-COMMIT-N" ASSERT-INTERNAL ;

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

\ --- the sealed schema registry (dot habu-seal-type-schema-c65f76cc). This
\ group proves the package-public/private answer for a file that BECAME a package:
\ src/core/type-schema.f keeps its schema implementation in SCHEMA-REG;
\ private storage and rewind operations have no external spelling.
\
\ THE MEASURABLE DIFFERENCE IS THE BARE NAME. Before the seal, `SCHEMA-A@` was a
\ global the marking pass reached, so it answered `internal engine word` — the
\ same answer a marked package public gives. Only E-UNDEFINED distinguishes "this
\ name is not in the top-level universe at all" from "it is, and it is refused",
\ so the bare leg below is what shows the seal did something the marking pass
\ could not: it removed the spelling rather than flagging the record.
\
\ THE PRIVATE LEG IS THE PRODUCT CLAIM. SCH-RBF-P is the schema rollback-frame
\ arena base. It is a `variable` — a data record, which the marking pass exempts
\ by design — with no REG-PROTECT row and no reference outside its own file, and
\ before the seal an ordinary `--load` program could write it: `0 SCH-RBF-P !`
\ followed by any SUMTYPE declaration took the engine down with SIGSEGV (rc 134),
\ the c5be6634 crash class reached through a name rather than a stack. Sealing is
\ what closes it, because a package private has no top-level spelling on any of
\ the three routes — bare, qualified, or through a `using`.
\
\ THE ONE ROUTE THAT STILL REACHES IT is `package SCHEMA-REG` in user source,
\ which puts the private wordlist back on the bare chain. That is not this seal's
\ defect. It is dot
\ habu-pkg-reopen-reaches-113ecd89, whose acceptance owns the fix, so no case here
\ asserts a crash as expected behaviour. ----

: SEAL-BARE-FORGE$ ( -- ptr u8 n )   \ the pre-seal crash program, verbatim
   SB-RESET
   s" 0 SCH-RBF-P !" SB-APPEND LF
   s" SUMTYPE iwgseal 0" SB-APPEND LF
   s" VARIANT iwgsa n ;VARIANT" SB-APPEND LF
   s" VARIANT iwgsb n ;VARIANT" SB-APPEND LF
   s" ;SUMTYPE" SB-APPEND LF
   SB$ ;

: SEAL-USING-FORGE$ ( -- ptr u8 n )  \ `using` imports publics only, never privates
   SB-RESET
   s" using SCHEMA-REG" SB-APPEND LF
   s" 0 SCH-RBF-P !" SB-APPEND LF
   SB$ ;

: QUAL-USING-FORGE$ ( -- ptr u8 n )   \ a using cannot reach the private rewind
   SB-RESET
   s" using SCHEMA-REG" SB-APPEND LF
   s" 0 0 REWIND" SB-APPEND LF
   SB$ ;

: QUAL-REWIND-FORGE$ ( -- ptr u8 n )  \ the private rewind has no qualified spelling
   SB-RESET
   s" 0 0 SCHEMA-REG:REWIND" SB-APPEND LF
   SB$ ;

: SEAL-AXIOM-FORGE$ ( -- ptr u8 n )  \ the two PPRIM: SCHEMA-REG rows relocated from checker.f
   SB-RESET
   s" SCHEMA-REG:SCHEMA-N@ drop SCHEMA-REG:SCHEMA-ROOT-N@ drop" SB-APPEND LF
   SB$ ;

: SEAL-PROTECT-FORGE$ ( -- ptr u8 n ) \ a public control cell still fails closed on its raw store
   SB-RESET
   s" 5 SCHEMA-REG:SCH-RBF-DEPTH !" SB-APPEND LF
   SB$ ;

\ The IR verifier's package-scope mirror, moved here from
\ test/compiler/ir-id.f when that suite became a WHITEBOX-SUITE: the claim is
\ about the PRODUCT engine's refusal, so it belongs with the rest of the seal
\ and not on a host built without one. src/compiler/ir/id.f still owns the
\ positive side - that the mirror is the only way the verifier reaches a
\ package scope - and asserts the rest of that surface is not addressable.
: VERIFY-MIRROR-CASES ( -- )
   s" the IR verifier's package mirror start is engine-internal" T-LABEL
   s" CHECKER-VERIFY-PKG-START" NEG
   s" and its close is too" T-LABEL
   s" CHECKER-VERIFY-PKG-DONE" NEG ;

: SEAL-CASES ( -- )
   s" the sealed tail left the global universe: bare SCHEMA-A@ is E-UNDEFINED" T-LABEL
   s" SCHEMA-A@" TOKEN$ RUN-SUBJECT
   s" E-UNDEFINED: SCHEMA-A@" ASSERT-DIAG
   s" and its qualified spelling is a marked public instead" T-LABEL
   s" SCHEMA-REG:SCHEMA-A@" NEG
   s" using cannot expose the private schema rewind" T-LABEL
   QUAL-USING-FORGE$ RUN-SUBJECT
   s" REWIND" ASSERT-UNDEF
   s" 0 0 SCHEMA-REG:REWIND no longer wipes the schema registry" T-LABEL
   QUAL-REWIND-FORGE$ RUN-LOAD
   s" SCHEMA-REG:REWIND" ASSERT-UNDEF
   s" a sealed private has no qualified spelling either" T-LABEL
   s" SCHEMA-REG:SCH-RBF-P" TOKEN$ RUN-SUBJECT
   s" E-UNDEFINED: SCHEMA-REG:SCH-RBF-P" ASSERT-DIAG
   s" nor a bare one through a using, which imports publics only" T-LABEL
   SEAL-USING-FORGE$ RUN-SUBJECT
   s" E-UNDEFINED: SCH-RBF-P" ASSERT-DIAG
   s" a REG-PROTECTed public control cell still fails closed on its raw store" T-LABEL
   SEAL-PROTECT-FORGE$ RUN-SUBJECT
   s" SCHEMA-REG:SCH-RBF-DEPTH" ASSERT-INTERNAL
   s" the two relocated PPRIM: SCHEMA-REG axioms keep their publics callable" T-LABEL
   SEAL-AXIOM-FORGE$ RUN-SUBJECT ASSERT-OK
   s" `0 SCH-RBF-P !` before a declaration no longer SIGSEGVs the engine" T-LABEL
   SEAL-BARE-FORGE$ RUN-LOAD
   s" E-UNDEFINED: SCH-RBF-P" ASSERT-DIAG ;

\ --- the sealed family registry (dot habu-tfam-2b-sealed-1b77662c). Same three-way
\ answer as the schema seal above, for a file that defined 572 globals and now
\ defines 221 publics and 345 privates under `package TFAM` with nothing renamed.
\
\ TWO REGRESSIONS ARE THE PRODUCT CLAIM, and both were measured on master before
\ the seal, through `bin/hb --load`:
\   `0 TF-RBF-P !` then a SUMTYPE declaration exited 134 with a SIGSEGV register
\   dump — the rollback-frame arena base is a `variable`, which the marking pass
\   exempts by design, and it carried no REG-PROTECT row. It is the exact sibling
\   of SCH-RBF-P and it is private now, so the program has no name to write.
\   `99999 SVX-HI !` then a REJECTED declaration exited 76 `tfam: ctor index
\   retired after its rows went`, turning a clean catchable duplicate-family reject
\   (rc 67) into an engine die. SVX-HI has two readers outside the file, so it is
\   public and carries the REG-PROTECT its siblings already had; TF-RBF-DEPTH (dot
\   habu-tf-rbf-depth-614c88e0) is the other cell that gained one.
\
\ SIX NAMES STAY GLOBAL AND ARE PINNED HERE, because the ENGINE resolves them by
\ bare spelling and no package spelling can reach that lookup: TFAM-NAME$,
\ TFL-CON-FAM?, TFL-CVAR? and TFL-MATCH-FAM? are read by habu2.f C-FIND-GLOBAL for
\ `construct`/`match`/`;match` (and mirrored byte for byte in bootstrap/cg/forth.fs),
\ while TF-SHA16-XT and TFAM-CTOR-WORD? are named by AOT-captured engine call sites
\ that the boot seed re-resolves. The negative leg below is what keeps that surface
\ from growing: an ordinary public must NOT answer bare. ----

: TSEAL-RBF-FORGE$ ( -- ptr u8 n )    \ the pre-seal SIGSEGV program, verbatim
   SB-RESET
   s" 0 TF-RBF-P !" SB-APPEND LF
   s" SUMTYPE iwgtseal 0" SB-APPEND LF
   s" VARIANT iwgta n ;VARIANT" SB-APPEND LF
   s" VARIANT iwgtb n ;VARIANT" SB-APPEND LF
   s" ;SUMTYPE" SB-APPEND LF
   SB$ ;

: TSEAL-SVX-FORGE$ ( -- ptr u8 n )    \ the pre-seal `die` program, verbatim
   SB-RESET
   s" SUMTYPE iwgsvx 0" SB-APPEND LF
   s" VARIANT iwgsv1 n ;VARIANT" SB-APPEND LF
   s" ;SUMTYPE" SB-APPEND LF
   s" 99999 SVX-HI !" SB-APPEND LF
   s" SUMTYPE iwgsvx 0" SB-APPEND LF
   s" VARIANT iwgsv2 n ;VARIANT" SB-APPEND LF
   s" ;SUMTYPE" SB-APPEND LF
   SB$ ;

: TSEAL-USING-FORGE$ ( -- ptr u8 n )  \ `using` imports publics only, never privates
   SB-RESET
   s" using TFAM" SB-APPEND LF
   s" 0 TF-RBF-P !" SB-APPEND LF
   SB$ ;

: TSEAL-AXIOM-FORGE$ ( -- ptr u8 n )  \ two of the 24 PPRIM: TFAM rows relocated from checker.f
   SB-RESET
   s" TFAM:TFAM-N@ drop TFAM:SUMV-N@ drop" SB-APPEND LF
   SB$ ;

: TSEAL-PROTECT-FORGE$ ( -- ptr u8 n ) \ the cell dot 614c88e0 named, now REG-PROTECTed
   SB-RESET
   s" 5 TFAM:TF-RBF-DEPTH !" SB-APPEND LF
   SB$ ;

\ A bridge name is proved global by the fact that its BARE token still resolves,
\ and the two ways it can be refused after that are different mechanisms, so they
\ get different assertions rather than one loose "not E-UNDEFINED". A name with a
\ PRIM: axiom is checker-known, so the marking pass poked DNAME-MIN-IN and a bare
\ call short of its declared inputs answers `interpret stack underdepth`. TFL-CVAR?
\ has no axiom, so the same pass set DNAME-INT and it answers `internal engine
\ word`. Either way the record is there — which is the claim.
: TSEAL-BRIDGE-AXIOM ( ptr u8 n -- ) {: a:ptr u:n :}
   a u TOKEN$ RUN-SUBJECT
   SB-RESET  s" interpret stack underdepth: " SB-APPEND  a u SB-APPEND
   SB$ ASSERT-DIAG ;

: TSEAL-BRIDGE-MARKED ( ptr u8 n -- ) {: a:ptr u:n :}
   a u TOKEN$ RUN-SUBJECT  a u ASSERT-INTERNAL ;

: TSEAL-TICK-FORGE$ ( -- ptr u8 n )  \ the launder route, refused at the tick by the seal guard (habu2.f C-BTICK C-QUALIFY-SEAL-GUARD, before any lookup or elaboration)
   SB-RESET
   s" variable IWG-TFV" SB-APPEND LF
   s" : IWG-TFSET ( -- ) ['] TFAM:PF-COMMIT-N IWG-TFV ! ;" SB-APPEND LF
   SB$ ;

: TFAM-SEAL-CASES ( -- )
   s" the sealed tail left the global universe: bare PF-FIND is E-UNDEFINED" T-LABEL
   s" PF-FIND" TOKEN$ RUN-SUBJECT
   s" E-UNDEFINED: PF-FIND" ASSERT-DIAG
   s" and its qualified spelling is a marked public instead" T-LABEL
   s" TFAM:PF-FIND" NEG
   s" a sealed private has no qualified spelling either" T-LABEL
   s" TFAM:TF-RBF-P" TOKEN$ RUN-SUBJECT
   s" E-UNDEFINED: TFAM:TF-RBF-P" ASSERT-DIAG
   s" nor a bare one through a using, which imports publics only" T-LABEL
   TSEAL-USING-FORGE$ RUN-SUBJECT
   s" E-UNDEFINED: TF-RBF-P" ASSERT-DIAG
   s" the REG-PROTECT dot 614c88e0 asked for holds on the raw store" T-LABEL
   TSEAL-PROTECT-FORGE$ RUN-SUBJECT
   s" TFAM:TF-RBF-DEPTH" ASSERT-INTERNAL
   s" the relocated PPRIM: TFAM axioms keep their publics callable" T-LABEL
   TSEAL-AXIOM-FORGE$ RUN-SUBJECT ASSERT-OK
   s" `0 TF-RBF-P !` before a declaration no longer SIGSEGVs the engine" T-LABEL
   TSEAL-RBF-FORGE$ RUN-LOAD
   s" E-UNDEFINED: TF-RBF-P" ASSERT-DIAG
   s" `99999 SVX-HI !` no longer turns a reject into an engine die" T-LABEL
   TSEAL-SVX-FORGE$ RUN-LOAD
   s" E-UNDEFINED: SVX-HI" ASSERT-DIAG
   s" a compiled tick of a public tail is refused by the same guard" T-LABEL
   TSEAL-TICK-FORGE$ RUN-SUBJECT
   s" TFAM:PF-COMMIT-N" ASSERT-SEALED
   s" a bare tick of a public tail is refused by the same guard" T-LABEL
   s" ' " s" TFAM:" s" PF-COMMIT-N" s" " QUAL-PROG$ RUN-SUBJECT
   s" TFAM:PF-COMMIT-N" ASSERT-SEALED
   s" and so is a tick of a private tail, before any lookup can miss" T-LABEL
   s" ' " s" TFAM:" s" TF-RBF-P" s" " QUAL-PROG$ RUN-SUBJECT
   s" TFAM:TF-RBF-P" ASSERT-SEALED
   s" TFAM-NAME$ stays global: habu2.f C-FIND-GLOBAL reads that spelling" T-LABEL
   s" TFAM-NAME$" TSEAL-BRIDGE-AXIOM
   s" TFL-MATCH-FAM? stays global for the same match-keyword bridge" T-LABEL
   s" TFL-MATCH-FAM?" TSEAL-BRIDGE-AXIOM
   s" TFL-CON-FAM? stays global for the construct-keyword bridge" T-LABEL
   s" TFL-CON-FAM?" TSEAL-BRIDGE-AXIOM
   s" TFL-CVAR? stays global for the construct bridge, marked not axiom'd" T-LABEL
   s" TFL-CVAR?" TSEAL-BRIDGE-MARKED
   s" TFAM-CTOR-WORD? stays global: an AOT call site names it" T-LABEL
   s" TFAM-CTOR-WORD?" TSEAL-BRIDGE-AXIOM
   s" TF-SHA16-XT stays global: an AOT call site names it" T-LABEL
   s" TF-SHA16-XT" TSEAL-BRIDGE-AXIOM
   s" and the global surface stopped there: TFL-VAR? is package-only" T-LABEL
   s" TFL-VAR?" TOKEN$ RUN-SUBJECT
   s" E-UNDEFINED: TFL-VAR?" ASSERT-DIAG
   s" TFAM-SLOTS@ is package-only too, though a PPRIM row names it" T-LABEL
   s" TFAM-SLOTS@" TOKEN$ RUN-SUBJECT
   s" E-UNDEFINED: TFAM-SLOTS@" ASSERT-DIAG ;

\ --- the engine's own build chain (dot habu-give-the-build-4b825045). Three
\ sites in it used to name a word this pass marks DNAME-INT - two through a
\ TRUSTED: body, one through an XREF-FIND on the spelling - which keeps the name
\ alive in a shipped image for one caller and states nothing about the word. A
\ product image that keeps only the names the checker knows therefore could not
\ build the next generation (`ncomp: cannot compile REG-INCOMING?`, rc 67).
\
\ THE THREE ANSWERS ARE NOT THE SAME ANSWER, and the point of these cases is
\ which one each site got. Two callees the build chain really must reach carry a
\ declared effect now, so their callers are ordinary checked Habu and the pass
\ leaves the name callable. The third had no consumer at all - hide.f's
\ signature-store reset was never called by anything - so `USIGS` keeps the seal
\ and the uncalled words are gone instead.
\
\ AND THEIR NEIGHBOURS STAYED SEALED, which is the half a row could quietly take
\ away: CHECKER-BOUND:MARK is what TAKES the core-prefix boundary and only
\ src/core/lower-cert-seal.f may do that, CURSORS (in NEG-BARE above) is the
\ number tools/build-fixpoint.f deliberately cannot hold beside the mark's own
\ copy, and REWIND is guarded by its own body. Running REWIND in a booted engine
\ discards the checker's symbols above the prefix, so the case below pins the
\ outcome that makes the row safe to have: the next reference is refused, not
\ mis-certified.

: BCC-REWIND-FORGE$ ( -- ptr u8 n )   \ a rewound boundary refuses, it does not lie
   SB-RESET
   s" : IWGBCA ( -- n ) 7 ;" SB-APPEND LF
   s" CHECKER-BOUND:REWIND" SB-APPEND LF
   s" : IWGBCB ( -- n ) IWGBCA ;" SB-APPEND LF
   SB$ ;

: BUILD-CHAIN-CALLEE-CASES ( -- )
   s" the merge comparator carries a row: aot-file.f MERGE-REG calls it checked" T-LABEL
   s" TFAM:REG-AOT-MERGE-INCOMING?" TSEAL-BRIDGE-AXIOM
   s" the boundary rewind carries one too: prefix-rewind.f TO-CORE calls it checked" T-LABEL
   s" CHECKER-BOUND:REWIND" TOKEN$ RUN-SUBJECT ASSERT-OK
   s" and a rewound boundary refuses the next reference instead of certifying it" T-LABEL
   BCC-REWIND-FORGE$ RUN-SUBJECT
   s" undefined word 'IWGBCA'" ASSERT-DIAG
   s" MARK keeps the seal: only lower-cert-seal.f may take the boundary" T-LABEL
   s" CHECKER-BOUND:MARK" NEG
   s" USIGS keeps it as well: no reset names it any more (RECOVERY-RESET below)" T-LABEL
   s" USIGS" NEG ;

\ --- the recovery launcher's prologue (dot habu-route-the-recovery-4ef43bd9).
\ tools/bootstrap.sh feeds ONE boot-hide prologue to the gforth stage0 and to
\ every sealed native stage after it, and its checker half used to be a hand
\ rewind: `0 UEND !` and the store's terminator, written from a TRUSTED: body
\ spelling `USIGS`. That is a fourth instance of the shape above - the seal
\ marks `USIGS` DNAME-INT, so its record survived in a stage image only because
\ that one body spelled it, and a stage stripped to the names the checker knows
\ could not compile the prelude the whole recovery starts from.
\
\ IT GOT A ROW OF ITS OWN, unlike the third site, because the reset really is
\ distinct: CHECKER-BOUND:REWIND returns to the prefix's END and this caller
\ reloads the prefix from its FIRST record, so it needs the store at zero, and
\ rewinding the boundary there instead builds a stage-1 engine that cannot boot
\ (measured: silent rc 78). The row is CHECKER-BOUND:EMPTY-STORE, a second
\ public on the same package.
\
\ WHAT THE ROW IS NOT is the seam underneath it. USIGS-RESTORE-END takes the new
\ end, and an axiom'd name is tickable whatever PRIM-TRUSTED-ONLY! says about
\ the call site, so declaring THAT would hand every program a store write at an
\ offset of its choosing - measured on a tree that tried it: `'
\ USIGS-RESTORE-END` executed with $7FFFFFFF crashed with SIGSEGV. So the last
\ case below keeps that seam sealed, beside the store base and the boundary's
\ MARK one section above. The declared word takes no argument and its
\ consequence is the one REWIND's case pins there - the next reference is
\ refused, not mis-certified - which is what makes a tickable row safe to have.

: RRR-EMPTY$ ( -- ptr u8 n )      \ the prologue's call, and what it costs
   SB-RESET
   s" : IWGRRA ( -- n ) 7 ;" SB-APPEND LF
   s" CHECKER-BOUND:EMPTY-STORE" SB-APPEND LF
   s" : IWGRRB ( -- n ) IWGRRA ;" SB-APPEND LF
   SB$ ;

: RRR-USIGS$ ( -- ptr u8 n )      \ what the prologue stopped spelling
   SB-RESET
   s" : IWGRR-USIGS ( -- ptr u8 ) USIGS ;" SB-APPEND LF
   SB$ ;

: RECOVERY-RESET-CASES ( -- )
   s" the prologue's store reset carries a row and discards the source effects" T-LABEL
   RRR-EMPTY$ RUN-SUBJECT
   s" undefined word 'IWGRRA'" ASSERT-DIAG
   s" USIGS is outside the checker's universe, so checked source cannot name it" T-LABEL
   RRR-USIGS$ RUN-SUBJECT
   s" E-UNDEFINED: USIGS" ASSERT-DIAG
   s" the seam under the row keeps the seal: no offset of the caller's choosing" T-LABEL
   s" USIGS-RESTORE-END" NEG ;

\ --- the sealed declaration grammar (dot habu-tfam-2b-sealed-1b77662c, third of
\ three). src/core/sumtype.f defined 316 globals and now defines 45 publics and
\ 271 privates under `package TYPE-DECL`, with SEVEN names left global.
\
\ THIS OWNER IS NOT A RESERVED SYSTEM-PACKAGE NAME, and that is the difference
\ from TFAM-SEAL-CASES above rather than an omission here. `tfam` is in habu2.f
\ KWDATA:RESTAB-BUF, so C-QUALIFY-SEAL-GUARD refuses `' TFAM:<anything>` with
\ rc 84 before any lookup; `type-decl` is not in that table, so this package
\ answers the SCHEMA-REG way and every case below asserts that answer: a marked
\ public is `hb: internal engine word` at rc 70, and a private is E-UNDEFINED
\ under its own qualified spelling. Reading rc 84 here would mean somebody added
\ the name to RESTAB, which is a different design and has to be argued for.
\
\ ONE REGRESSION IS THE PRODUCT CLAIM, measured on master before the seal through
\ `bin/hb --load`: `0 TDPLAN-P !` followed by any SUMTYPE declaration exited 134
\ with a SIGSEGV register dump. TDPLAN-P is the generated-declaration plan arena
\ base, a PTR-VARIABLE, which the marking pass exempts by design, and it carried
\ no REG-PROTECT row -- the exact SCH-RBF-P / TF-RBF-P sibling, and one of six
\ such cells in this file (TDPLAN-P, TDPLAN-ROW-P, TDPV-CNT-P, TDPV-CELLS-P,
\ TDPV-OFF-P, TDPV-NODE-P). All six are private now, so the program has no name
\ to write on any route.
\
\ THE SEVEN GLOBALS ARE THE DECLARATION GRAMMAR and are pinned here so the
\ surface cannot grow: NEWTYPE, SUMTYPE and PRODUCT are the language's own block
\ openers -- a user declaration must register from genuine top level, which is
\ test/type-decl-suite.f's whole premise -- and CHECKER-DEFFAMILY, CHECKER-DEFSUM,
\ CHECKER-DEFSUM-NOEND and CHECKER-DEFPRODUCT are the same grammar reached with a
\ body text instead of the input stream. The negative leg is what bounds it: the
\ package public each one calls must NOT answer bare.
\
\ THE ONE ROUTE THAT STILL REACHES A PRIVATE is the one the schema block above
\ names: `package TYPE-DECL / private / 0 TDPLAN-P !` puts the private wordlist
\ back on the bare chain and the SIGSEGV comes back, exactly as `package
\ SCHEMA-REG` does on master. That is dot habu-pkg-reopen-reaches-113ecd89,
\ whose acceptance owns the
\ fix, so no case here asserts a crash as expected behaviour. TFAM is closed
\ against it only because `tfam` is in habu2.f KWDATA:RESTAB-BUF, which is a
\ decision about which packages are system packages rather than a property of
\ sealing; whether `type-decl` joins that table is that dot's call. ----

: DSEAL-PLAN-FORGE$ ( -- ptr u8 n )   \ the pre-seal SIGSEGV program, verbatim
   SB-RESET
   s" 0 TDPLAN-P !" SB-APPEND LF
   s" SUMTYPE iwgdseal 0" SB-APPEND LF
   s" VARIANT iwgda n ;VARIANT" SB-APPEND LF
   s" VARIANT iwgdb n ;VARIANT" SB-APPEND LF
   s" ;SUMTYPE" SB-APPEND LF
   SB$ ;

: DSEAL-USING-FORGE$ ( -- ptr u8 n )  \ `using` imports publics only, never privates
   SB-RESET
   s" using TYPE-DECL" SB-APPEND LF
   s" 0 TDPLAN-P !" SB-APPEND LF
   SB$ ;

: DSEAL-PROTECT-FORGE$ ( -- ptr u8 n ) \ a public control cell fails closed on its raw store
   SB-RESET
   s" 5 TYPE-DECL:TDPLAN-N !" SB-APPEND LF
   SB$ ;

\ The one public record in this file that deliberately carries no REG-PROTECT,
\ because its writers arm at genuine top level and REG-PROTECT would refuse them
\ (see the note beside it in src/core/sumtype.f). What the seal bought is still
\ asserted: the BARE name is gone, and the surviving qualified spelling fails
\ closed on a named rc 76 die instead of corrupting anything. If this ever
\ answers 0 and prints nothing, the fail-closed guard went with it.
: DSEAL-ARMED-FORGE$ ( -- ptr u8 n )
   SB-RESET
   s" 0 TDECL-EVAL-ARMED !" SB-APPEND LF
   SB$ ;

: DSEAL-ARMED-QUAL-FORGE$ ( -- ptr u8 n )
   SB-RESET
   s" 0 TYPE-DECL:TDECL-EVAL-ARMED !" SB-APPEND LF
   s" SUMTYPE iwgdarm 0" SB-APPEND LF
   s" VARIANT iwgdc n ;VARIANT" SB-APPEND LF
   s" ;SUMTYPE" SB-APPEND LF
   SB$ ;

\ The three block openers cannot be probed as bare tokens: their axiom is ( -- ),
\ so a bare token would run and parse whatever followed it. The declaration IS
\ the probe, written the way a user writes one -- at genuine top level, with no
\ package open -- which is exactly the standing the seven names exist to keep.
: DSEAL-GRAMMAR-FORGE$ ( -- ptr u8 n )
   SB-RESET
   s" NEWTYPE iwgdnt 0" SB-APPEND LF
   s" PRODUCT iwgdpr 0 FIELD iwgdf n ;PRODUCT" SB-APPEND LF
   s" SUMTYPE iwgdsm 0" SB-APPEND LF
   s" VARIANT iwgdv n ;VARIANT" SB-APPEND LF
   s" ;SUMTYPE" SB-APPEND LF
   SB$ ;

: TYPE-DECL-SEAL-CASES ( -- )
   s" the sealed tail left the global universe: bare TDPLAN-N is E-UNDEFINED" T-LABEL
   s" TDPLAN-N" TOKEN$ RUN-SUBJECT
   s" E-UNDEFINED: TDPLAN-N" ASSERT-DIAG
   s" and its qualified spelling is a marked public instead" T-LABEL
   s" TYPE-DECL:TDPLAN-N" NEG
   s" a colon public went the same way: bare TDECL-CTOR-PUBLISH is undefined" T-LABEL
   s" TDECL-CTOR-PUBLISH" TOKEN$ RUN-SUBJECT
   s" E-UNDEFINED: TDECL-CTOR-PUBLISH" ASSERT-DIAG
   s" a sealed private has no qualified spelling either" T-LABEL
   s" TYPE-DECL:TDPLAN-P" TOKEN$ RUN-SUBJECT
   s" E-UNDEFINED: TYPE-DECL:TDPLAN-P" ASSERT-DIAG
   s" nor a bare one through a using, which imports publics only" T-LABEL
   DSEAL-USING-FORGE$ RUN-SUBJECT
   s" E-UNDEFINED: TDPLAN-P" ASSERT-DIAG
   s" a REG-PROTECTed public control cell fails closed on its raw store" T-LABEL
   DSEAL-PROTECT-FORGE$ RUN-SUBJECT
   s" TYPE-DECL:TDPLAN-N" ASSERT-INTERNAL
   s" the tick answers the schema way, not the reserved-name way: public" T-LABEL
   s" ' " s" TYPE-DECL:" s" TDPLAN-N" s" " QUAL-PROG$ RUN-SUBJECT
   s" TYPE-DECL:TDPLAN-N" ASSERT-INTERNAL
   s" `0 TDPLAN-P !` before a declaration no longer SIGSEGVs the engine" T-LABEL
   DSEAL-PLAN-FORGE$ RUN-LOAD
   s" E-UNDEFINED: TDPLAN-P" ASSERT-DIAG
   s" the armed flag lost its bare name like every other sealed record" T-LABEL
   DSEAL-ARMED-FORGE$ RUN-SUBJECT
   s" E-UNDEFINED: TDECL-EVAL-ARMED" ASSERT-DIAG
   s" and its one surviving spelling still fails closed, on a named die" T-LABEL
   DSEAL-ARMED-QUAL-FORGE$ RUN-LOAD
   EXITED @ TTRUE
   RC @ 76 T=
   ERR$ s" sumtype: constructor eval hook not installed" CONTAINS? TTRUE
   s" the three block openers still declare from genuine top level" T-LABEL
   DSEAL-GRAMMAR-FORGE$ RUN-LOAD ASSERT-OK
   s" CHECKER-DEFFAMILY stays global: it is the grammar without the stream" T-LABEL
   s" CHECKER-DEFFAMILY" TSEAL-BRIDGE-AXIOM
   s" CHECKER-DEFSUM stays global for the same reason" T-LABEL
   s" CHECKER-DEFSUM" TSEAL-BRIDGE-AXIOM
   s" CHECKER-DEFSUM-NOEND stays global for the same reason" T-LABEL
   s" CHECKER-DEFSUM-NOEND" TSEAL-BRIDGE-AXIOM
   s" CHECKER-DEFPRODUCT stays global for the same reason" T-LABEL
   s" CHECKER-DEFPRODUCT" TSEAL-BRIDGE-AXIOM
   s" and the global surface stopped there: TDECL-DEFSUM is package-only" T-LABEL
   s" TDECL-DEFSUM" TOKEN$ RUN-SUBJECT
   s" E-UNDEFINED: TDECL-DEFSUM" ASSERT-DIAG
   s" TDECL-RUN, the transaction under all seven, is package-only too" T-LABEL
   s" TDECL-RUN" TOKEN$ RUN-SUBJECT
   s" E-UNDEFINED: TDECL-RUN" ASSERT-DIAG ;

\ --- direct/subject parity. The PARITY- group used to be its own package; the
\ names keep that marker because they are about the direct-versus-fork
\ comparison, not about running a child in general. ----

: PARITY-RESULT ( -- ptr u8 n ptr u8 n n )
   OUT OUT-U @ ERR ERR-U @ RC @ ;

: PARITY-NEG-LOAD ( ptr u8 n -- )
   2dup RUN-LOAD
   s" U-TYPE" ASSERT-INTERNAL
   PARITY-RESULT TAIL-PARITY:SNAPSHOT
   RUN-SUBJECT
   s" U-TYPE" ASSERT-INTERNAL
   PARITY-RESULT TAIL-PARITY:SAME ;

: PARITY-NEG-STDIN ( ptr u8 n -- )
   2dup RUN-STDIN
   s" U-TYPE" ASSERT-INTERNAL
   PARITY-RESULT TAIL-PARITY:SNAPSHOT
   RUN-SUBJECT
   s" U-TYPE" ASSERT-INTERNAL
   PARITY-RESULT TAIL-PARITY:SAME ;

: PARITY-POS-LOAD ( ptr u8 n -- )
   2dup RUN-LOAD ASSERT-OK
   PARITY-RESULT TAIL-PARITY:SNAPSHOT
   RUN-SUBJECT ASSERT-OK
   PARITY-RESULT TAIL-PARITY:SAME ;

: PARITY-TEST ( -- )
   s" direct --load and subject preserve raw internal-word results" T-LABEL
   s" U-TYPE" TOKEN$ PARITY-NEG-LOAD
   s" direct stdin and subject preserve raw internal-word results" T-LABEL
   s" U-TYPE" TOKEN$ PARITY-NEG-STDIN
   s" direct --load and subject preserve raw successful results" T-LABEL
   RAW-FORGE$ PARITY-POS-LOAD ;

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
   PREPARE
   PARITY-TEST
   NEG-BARE
   REGISTRY-CASES
   SIBLING-CASES
   CTLIVE-CASES
   SEAL-CASES
   VERIFY-MIRROR-CASES
   TFAM-SEAL-CASES
   BUILD-CHAIN-CALLEE-CASES
   RECOVERY-RESET-CASES
   TYPE-DECL-SEAL-CASES
   NEG-SHAPES
   SEED-RESET-CASES
   NULL-PTR-CELL-CASES
   POSITIVES
   OPENER-CASES
   DEFER-CASES
   LAUNDER-CASES
   CLEANUP
   T-REPORT
   s" internal-word-gate: ok" type cr ;

\ ACTION returns a checked quotation for private MAIN before the package closes;
\ execute it globally after the close so SUBJECT forks inherit no open package.
: ACTION ( -- [ -- ] )
   [: MAIN ;] ;

ACTION

;package

execute
