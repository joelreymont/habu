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

2048 constant IWG-CAP
70 constant IWG-REJECT-RC           \ interpret-level reject exit (RC-REJECT)
67 constant IWG-THROW-RC            \ engine uncaught-throw boundary exit

variable IWG-ROOT-U
variable IWG-CHILD-U
variable IWG-IN-U
variable IWG-OUT-U
variable IWG-ERR-U
variable IWG-EXITED                 \ bool: child completed by exit
variable IWG-RC

create IWG-ROOT-BUF FS-PATH-CAP allot
create IWG-CHILD-BUF FS-PATH-CAP allot
create IWG-IN IWG-CAP allot         \ stdin-piped program
create IWG-OUT IWG-CAP allot
create IWG-ERR IWG-CAP allot
create IWG-EMPTY 1 allot            \ zero-length stdin

: IWG-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: src:ptr u:n dst:ptr lenp:ptr :}
   u FS-PATH-CAP > if E-FS-CAPACITY throw then
   src dst u BYTE-COPY
   u lenp ! ;

: IWG-ROOT ( -- ptr u8 n )
   IWG-ROOT-BUF IWG-ROOT-U @ ;

: IWG-CHILD ( -- ptr u8 n )
   IWG-CHILD-BUF IWG-CHILD-U @ ;

: IWG-IN$ ( -- ptr u8 n )
   IWG-IN IWG-IN-U @ ;

: IWG-ERR$ ( -- ptr u8 n )
   IWG-ERR IWG-ERR-U @ ;

\ Resolve the child engine: gate default env HABU_UNDER_TEST -> the candidate;
\ standalone runs fall back to bin/hb.
: IWG-HB$ ( -- ptr u8 n )
   s" HABU_UNDER_TEST" >LEN PROC-ENV-DEFAULT$? if LEN>N exit then
   2drop
   s" HABU_UNDER_TEST" GETENV dup 0= if
      2drop s" bin/hb" exit
   then ;

: IWG-STORE! ( len len outcome -- )
   MATCH outcome
     exited OF IWG-RC ! 0 0= IWG-EXITED ! ENDOF
     signaled OF IWG-RC ! 0 0= 0= IWG-EXITED ! ENDOF
     timeout OF 0 IWG-RC ! 0 0= 0= IWG-EXITED ! ENDOF
   ;MATCH
   LEN>N IWG-ERR-U !  LEN>N IWG-OUT-U ! ;

: IWG-IN! ( ptr u8 n -- ) {: a:ptr u:n :}
   u IWG-CAP > if E-FS-CAPACITY throw then
   a IWG-IN u BYTE-COPY
   u IWG-IN-U ! ;

\ Run the program as a --load file with empty stdin.
: IWG-RUN-LOAD ( ptr u8 n -- )
   TAIL-RATCHET:DIRECT
   IWG-CHILD 2swap WRITE-ALL
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   IWG-CHILD >LEN PROC-ARGV+
   IWG-HB$ >LEN  IWG-EMPTY 0 >LEN  IWG-OUT IWG-CAP >LEN
   IWG-ERR IWG-CAP >LEN  TAIL-BUDGET:TIMEOUT-MS >MS  RUN-ARGV-STDIN-CAPTURE-OUTCOME
   IWG-STORE! ;

\ Run the program as a piped stdin program (no --load), the other cold-prefix path.
: IWG-RUN-STDIN ( ptr u8 n -- )
   TAIL-RATCHET:DIRECT
   IWG-IN!
   PROC-ARGV-RESET
   IWG-HB$ >LEN  IWG-IN$ >LEN  IWG-OUT IWG-CAP >LEN
   IWG-ERR IWG-CAP >LEN  TAIL-BUDGET:TIMEOUT-MS >MS  RUN-ARGV-STDIN-CAPTURE-OUTCOME
   IWG-STORE! ;

package IWG-EXEC

public

: SUBJECT ( ptr u8 n -- )
   TAIL-RATCHET:SUBJECT
   IWG-OUT IWG-CAP >LEN IWG-ERR IWG-CAP >LEN
   TAIL-BUDGET:TIMEOUT-MS >MS SUBJECT:RUN IWG-STORE! ;

;package

: IWG-LF ( -- )
   10 SB-APPEND-C ;

: IWG-TOKEN$ ( ptr u8 n -- ptr u8 n )    \ program = the bare token on one line
   SB-RESET
   SB-APPEND IWG-LF
   SB$ ;

: IWG-ASSERT-INTERNAL ( ptr u8 n -- ) {: a:ptr u:n :}   \ fail-closed reject naming the word
   IWG-EXITED @ TTRUE
   IWG-RC @ IWG-REJECT-RC T=
   IWG-ERR$ s" hb: internal engine word: " CONTAINS? TTRUE
   IWG-ERR$ a u CONTAINS? TTRUE ;

: IWG-ASSERT-OK ( -- )
   IWG-EXITED @ TTRUE
   IWG-RC @ 0 T= ;

: IWG-NEG ( ptr u8 n -- ) {: a:ptr u:n :}
   a u IWG-TOKEN$ IWG-EXEC:SUBJECT
   a u IWG-ASSERT-INTERNAL ;

\ --- negatives: internal checker words fail closed before their body runs ---

: IWG-NEG-BARE ( -- )
   s" bare U-TYPE fails closed (was SIGSEGV)" T-LABEL
   s" U-TYPE" IWG-NEG
   s" bare T-RES fails closed" T-LABEL
   s" T-RES" IWG-NEG
   s" bare PAIR fails closed" T-LABEL
   s" PAIR" IWG-NEG
   s" bare CHECKER-FIND-ACTIVE-SIG fails closed" T-LABEL
   s" CHECKER-FIND-ACTIVE-SIG" IWG-NEG
   s" bare E-INST fails closed" T-LABEL
   s" E-INST" IWG-NEG
   s" bare PF-BEGIN mutation fails closed" T-LABEL
   s" PF-BEGIN" IWG-NEG
   s" bare CT-LIVE? field-liveness query fails closed" T-LABEL
   s" CT-LIVE?" IWG-NEG ;

: IWG-ARGS-FORGE$ ( -- ptr u8 n )        \ args present: the gate is not depth-keyed
   SB-RESET
   s" 1 2 U-TYPE" SB-APPEND IWG-LF
   SB$ ;

: IWG-TICK-FORGE$ ( -- ptr u8 n )        \ tick would launder the xt to execute
   SB-RESET
   s" ' U-TYPE" SB-APPEND IWG-LF
   SB$ ;

: IWG-PRIM-FORGE$ ( -- ptr u8 n )        \ the marking prim itself is sealed
   SB-RESET
   s" 0 int-mark" SB-APPEND IWG-LF
   SB$ ;

: IWG-NEG-SHAPES ( -- )
   s" 1 2 U-TYPE (satisfied depth) still fails closed" T-LABEL
   IWG-ARGS-FORGE$ IWG-EXEC:SUBJECT
   s" U-TYPE" IWG-ASSERT-INTERNAL
   s" ' U-TYPE (tick laundering) fails closed" T-LABEL
   IWG-TICK-FORGE$ IWG-EXEC:SUBJECT
   s" U-TYPE" IWG-ASSERT-INTERNAL
   s" 0 int-mark: the marking prim is itself internal" T-LABEL
   IWG-PRIM-FORGE$ IWG-EXEC:SUBJECT
   s" int-mark" IWG-ASSERT-INTERNAL ;

\ --- positives: the public top-level surface is untouched -------------------

: IWG-ASSERT-DIAG ( ptr u8 n -- ) {: a:ptr u:n :}   \ child rejected rc 70 with the given diagnostic
   IWG-EXITED @ TTRUE
   IWG-RC @ IWG-REJECT-RC T=
   IWG-ERR$ a u CONTAINS? TTRUE ;

: IWG-UNDEF-FORGE$ ( -- ptr u8 n )
   SB-RESET
   s" IWG-NO-SUCH-WORD" SB-APPEND IWG-LF
   SB$ ;

: IWG-RAW-FORGE$ ( -- ptr u8 n )         \ user unchecked word stays executable
   SB-RESET
   s" 0 set-check" SB-APPEND IWG-LF
   s" : IWG-RAW 42 . cr ;" SB-APPEND IWG-LF
   s" IWG-RAW" SB-APPEND IWG-LF
   SB$ ;

: IWG-TRUST-FORGE$ ( -- ptr u8 n )       \ top-level TRUST row still works
   SB-RESET
   s" variable IWG-V" SB-APPEND IWG-LF
   S\" s\" IWG-V\" s\" -- ptr a\" TRUST" SB-APPEND IWG-LF
   SB$ ;

: IWG-TRUSTED-FORGE$ ( -- ptr u8 n )     \ TRUSTED: definition + bare call still work
   SB-RESET
   s" TRUSTED: IWG-T ( -- n ) 3 ;" SB-APPEND IWG-LF
   s" IWG-T . cr" SB-APPEND IWG-LF
   SB$ ;

: IWG-STRUCT-FORGE$ ( -- ptr u8 n )      \ structures DSL still works at top level
   SB-RESET
   s" BEGIN-STRUCTURE IWG-PT" SB-APPEND IWG-LF
   s"    CELL +FIELD IWG-PT.X" SB-APPEND IWG-LF
   s" END-STRUCTURE" SB-APPEND IWG-LF
   SB$ ;

: IWG-SUMTYPE-FORGE$ ( -- ptr u8 n )     \ type-family DSL still works at top level
   SB-RESET
   s" SUMTYPE iwgfoo 1 VARIANT iwgbar a ;VARIANT ;SUMTYPE" SB-APPEND IWG-LF
   SB$ ;

: IWG-XREF-FORGE$ ( -- ptr u8 n )        \ introspection of internal words survives
   SB-RESET
   s" XREF U-TYPE" SB-APPEND IWG-LF
   SB$ ;

: IWG-LBUF-FORGE$ ( -- ptr u8 n )        \ LAYOUT-BUFFER DSL still works at top level
   SB-RESET
   s" ENUM iwgclr iwgred iwgblue ;ENUM" SB-APPEND IWG-LF
   s" 4 LAYOUT-BUFFER IWG-LB iwgclr" SB-APPEND IWG-LF
   s" 3 IWG-LB drop" SB-APPEND IWG-LF
   SB$ ;

: IWG-LBUF-BODY-FORGE$ ( -- ptr u8 n )   \ layout-buffer inside a checked body is unsafe
   SB-RESET
   s" : IWG-LBAD ( n -- ) LAYOUT-BUFFER ;" SB-APPEND IWG-LF
   SB$ ;

\ --- type-DSL openers are top-level-interpret-only: their ( -- ) axiom rows
\ hide runtime input parsing + registry mutation, so UNSAFE-TOK? rejects them
\ inside checked bodies (dot habu-checker-in-body-af7cf855, LBUF parity). ----

: IWG-TDSL-TOP-FORGE$ ( -- ptr u8 n )    \ TYPEFAMILY/PRODUCT still work at top level
   SB-RESET
   s" TYPEFAMILY iwgtf 0" SB-APPEND IWG-LF
   s" PRODUCT iwgpr 0 FIELD x n ;PRODUCT" SB-APPEND IWG-LF
   SB$ ;

: IWG-PF-REFLECT-FORGE$ ( -- ptr u8 n )  \ packaged committed reflection is checked/public
   SB-RESET
   s" PRODUCT iwgpf 0 FIELD x n ;PRODUCT" SB-APPEND IWG-LF
   s" : IWG-PF-READ ( -- n )" SB-APPEND IWG-LF
   s"    TYPE-FIELD:COUNT drop" SB-APPEND IWG-LF
   s"    TFAM-N@ 1 - TYPE-FIELD:NO-VARIANT 0 TYPE-FIELD:EACH if drop else drop then" SB-APPEND IWG-LF
   S\"    TFAM-N@ 1 - TYPE-FIELD:NO-VARIANT s\" x\" TYPE-FIELD:FIND" SB-APPEND IWG-LF
   s"    if dup TYPE-FIELD:FAMILY@ drop dup TYPE-FIELD:VARIANT@ drop" SB-APPEND IWG-LF
   s"       dup TYPE-FIELD:NAME$ 2drop dup TYPE-FIELD:SCHEMA@ drop" SB-APPEND IWG-LF
   s"       dup TYPE-FIELD:CELLS@ drop dup TYPE-FIELD:BYTE-OFF@ drop" SB-APPEND IWG-LF
   s"       dup TYPE-FIELD:BYTES@ drop dup TYPE-FIELD:ALIGN@ drop" SB-APPEND IWG-LF
   s"       dup TYPE-FIELD:FLAGS@ drop TYPE-FIELD:SLOT@ else drop -1 then ;" SB-APPEND IWG-LF
   s" IWG-PF-READ . cr" SB-APPEND IWG-LF
   SB$ ;

: IWG-PF-RAW-FORGE$ ( -- ptr u8 n )      \ raw implementation names are not checked/public
   s" : IWG-PF-RAW ( n n ptr u8 n -- n bool ) PF-FIND ;" ;

\ The DEFTYPE nominal surface and the roles.f DEFLINEAR/VALUE-RECORD definers
\ are the same hazard class as the openers but admitted via certified usigs, not
\ PRIM: axioms (dot habu-checker-deftype-deflinear-8e9d1dc5): declare + use each
\ at top level.

: IWG-ROLES-TOP-FORGE$ ( -- ptr u8 n )   \ DEFTYPE/DEFLINEAR/VALUE-RECORD at top level
   SB-RESET
   s" DEFTYPE iwgid" SB-APPEND IWG-LF
   s" : IWG-ID-RT ( n -- n ) >iwgid iwgid>N ;" SB-APPEND IWG-LF
   s" 7 IWG-ID-RT . cr" SB-APPEND IWG-LF
   s" DEFLINEAR iwgown" SB-APPEND IWG-LF
   s" : IWG-OWN-PASS ( iwgown -- iwgown ) ;" SB-APPEND IWG-LF
   s" VALUE-RECORD iwgpt x n y n END-VALUE-RECORD" SB-APPEND IWG-LF
   s" : IWG-PT-KEEP ( iwgpt -- iwgpt ) ;" SB-APPEND IWG-LF
   SB$ ;

\ EXPORT must not mint a qualified alias for an UNSAFE-TOK? name: the alias
\ spelling (PKG:NAME) would escape the name-keyed body reject (dot
\ habu-checker-unsafety-must-d12bc784 part a). Pinned like export-package.f's
\ E-EXPORT-PRIM case: uncaught named code E-EXPORT-UNSAFE 7120 -> rc 67.

: IWG-EXPORT-FORGE$ ( ptr u8 n -- ptr u8 n )   \ "package .. EXPORT <name> ;package"
   SB-RESET
   s" package IWGXP" SB-APPEND IWG-LF
   s" public" SB-APPEND IWG-LF
   s" EXPORT " SB-APPEND SB-APPEND IWG-LF
   s" ;package" SB-APPEND IWG-LF
   SB$ ;

: IWG-ASSERT-EXPORT-UNSAFE ( -- )        \ child threw E-EXPORT-UNSAFE (code named)
   IWG-EXITED @ TTRUE
   IWG-RC @ IWG-THROW-RC T=
   IWG-ERR$ s" 7120" CONTAINS? TTRUE ;

: IWG-NEG-EXPORT ( ptr u8 n -- )         \ EXPORT of an unsafe name rejects
   IWG-EXPORT-FORGE$ IWG-EXEC:SUBJECT
   IWG-ASSERT-EXPORT-UNSAFE ;

: IWG-EXPORT-BODY-FORGE$ ( -- ptr u8 n ) \ alias-minting line is the reject; the body never checks
   SB-RESET
   s" package IWGXP" SB-APPEND IWG-LF
   s" public" SB-APPEND IWG-LF
   s" EXPORT DEFLINEAR" SB-APPEND IWG-LF
   s" ;package" SB-APPEND IWG-LF
   s" : IWG-XBAD ( -- ) IWGXP:DEFLINEAR ;" SB-APPEND IWG-LF
   SB$ ;

: IWG-EXPORT-OK-FORGE$ ( -- ptr u8 n )   \ EXPORT of a normal checked word still works
   SB-RESET
   s" package IWGXA" SB-APPEND IWG-LF
   s" public" SB-APPEND IWG-LF
   s" : IWGW ( -- n ) 3 ;" SB-APPEND IWG-LF
   s" ;package" SB-APPEND IWG-LF
   s" package IWGXB" SB-APPEND IWG-LF
   s" public" SB-APPEND IWG-LF
   s" EXPORT IWGXA:IWGW" SB-APPEND IWG-LF
   s" ;package" SB-APPEND IWG-LF
   s" : IWG-XOK ( -- n ) IWGXB:IWGW ;" SB-APPEND IWG-LF
   s" IWG-XOK . cr" SB-APPEND IWG-LF
   SB$ ;

: IWG-OPENER-BODY-FORGE$ ( ptr u8 n -- ptr u8 n )   \ ": IWG-OBAD ( -- ) <opener> ;"
   SB-RESET
   s" : IWG-OBAD ( -- ) " SB-APPEND
   SB-APPEND
   s"  ;" SB-APPEND IWG-LF
   SB$ ;

: IWG-OPENER-DIAG$ ( ptr u8 n -- ptr u8 n )   \ pinned reject site "at '<opener>'"
   SB-RESET
   s" at '" SB-APPEND
   SB-APPEND
   s" '" SB-APPEND
   SB$ ;

: IWG-NEG-OPENER ( ptr u8 n -- )         \ in-body opener at empty declared stack rejects
   2dup IWG-OPENER-BODY-FORGE$ IWG-EXEC:SUBJECT
   IWG-OPENER-DIAG$ IWG-ASSERT-DIAG ;

: IWG-OPENER-CASES ( -- )
   s" TYPEFAMILY/PRODUCT DSL still works at top level" T-LABEL
   IWG-TDSL-TOP-FORGE$ IWG-EXEC:SUBJECT IWG-ASSERT-OK
   s" packaged field reflection is checked/public" T-LABEL
   IWG-PF-REFLECT-FORGE$ IWG-RUN-LOAD IWG-ASSERT-OK
   s" raw field reflection is unavailable to checked code" T-LABEL
   IWG-PF-RAW-FORGE$ IWG-RUN-LOAD
   s" at 'PF-FIND'" IWG-ASSERT-DIAG
   s" TYPEFAMILY in a checked body is rejected unsafe" T-LABEL
   s" TYPEFAMILY" IWG-NEG-OPENER
   s" SUMTYPE in a checked body is rejected unsafe" T-LABEL
   s" SUMTYPE" IWG-NEG-OPENER
   s" ENUM in a checked body is rejected unsafe" T-LABEL
   s" ENUM" IWG-NEG-OPENER
   s" PRODUCT in a checked body is rejected unsafe" T-LABEL
   s" PRODUCT" IWG-NEG-OPENER
   s" DEFTYPE/DEFLINEAR/VALUE-RECORD still work at top level" T-LABEL
   IWG-ROLES-TOP-FORGE$ IWG-EXEC:SUBJECT IWG-ASSERT-OK
   s" DEFLINEAR in a checked body is rejected unsafe" T-LABEL
   s" DEFLINEAR" IWG-NEG-OPENER
   s" VALUE-RECORD in a checked body is rejected unsafe" T-LABEL
   s" VALUE-RECORD" IWG-NEG-OPENER
   s" cast: in a checked body is rejected unsafe" T-LABEL
   s" cast:" IWG-NEG-OPENER
   s" DEFER-LAYOUT-BUFFER in a checked body is rejected unsafe" T-LABEL
   s" DEFER-LAYOUT-BUFFER" IWG-NEG-OPENER
   s" EXPORT of a checked word still works" T-LABEL
   IWG-EXPORT-OK-FORGE$ IWG-EXEC:SUBJECT IWG-ASSERT-OK
   s" EXPORT DEFLINEAR rejects E-EXPORT-UNSAFE" T-LABEL
   s" DEFLINEAR" IWG-NEG-EXPORT
   s" EXPORT VALUE-RECORD rejects E-EXPORT-UNSAFE" T-LABEL
   s" VALUE-RECORD" IWG-NEG-EXPORT
   s" EXPORT cast: rejects E-EXPORT-UNSAFE" T-LABEL
   s" cast:" IWG-NEG-EXPORT
   s" EXPORT SUMTYPE rejects E-EXPORT-UNSAFE" T-LABEL
   s" SUMTYPE" IWG-NEG-EXPORT
   s" EXPORT ENUM rejects E-EXPORT-UNSAFE" T-LABEL
   s" ENUM" IWG-NEG-EXPORT
   s" EXPORT PRODUCT rejects E-EXPORT-UNSAFE" T-LABEL
   s" PRODUCT" IWG-NEG-EXPORT
   s" EXPORT TYPEFAMILY rejects E-EXPORT-UNSAFE" T-LABEL
   s" TYPEFAMILY" IWG-NEG-EXPORT
   s" EXPORT DEFER-LAYOUT-BUFFER rejects E-EXPORT-UNSAFE" T-LABEL
   s" DEFER-LAYOUT-BUFFER" IWG-NEG-EXPORT
   s" qualified unsafe alias for a body cannot be minted" T-LABEL
   IWG-EXPORT-BODY-FORGE$ IWG-EXEC:SUBJECT
   IWG-ASSERT-EXPORT-UNSAFE ;

\ --- defer/is laundering (dot habu-checker-unsafety-as-1c537c1f, acceptance b).
\ A checked `is` installs a QUOTATION whose body is checked, and a raw xt from
\ tick has a different checker type than a quotation — so neither `['] <unsafe>
\ is X` nor `[: <unsafe> ;] is X` can bind an unsafe target from a checked body.
\ The first rejects at `is` (xt is not a quotation), the second rejects at the
\ unsafe token inside the quotation body (identity/name reject). Both are
\ in-body checker rejects: rc 70 with an `at '<token>'` diagnostic. ----
: IWG-DEFER-TICK-FORGE$ ( -- ptr u8 n )  \ ['] <unsafe> is X : rejects at 'is'
   SB-RESET
   s" defer IWG-DACT ( -- )" SB-APPEND IWG-LF
   s" : IWG-DSET ( -- ) ['] deflinear is IWG-DACT ;" SB-APPEND IWG-LF
   SB$ ;

: IWG-DEFER-QUOT-FORGE$ ( -- ptr u8 n )  \ [: <unsafe> ;] is X : rejects at 'deflinear'
   SB-RESET
   s" defer IWG-DACT2 ( -- )" SB-APPEND IWG-LF
   s" : IWG-DSET2 ( -- ) [: deflinear ;] is IWG-DACT2 ;" SB-APPEND IWG-LF
   SB$ ;

: IWG-DEFER-CASES ( -- )
   s" ['] <unsafe> is X (tick laundering) rejects at 'is'" T-LABEL
   IWG-DEFER-TICK-FORGE$ IWG-EXEC:SUBJECT
   s" at 'is'" IWG-ASSERT-DIAG
   s" [: <unsafe> ;] is X (quotation laundering) rejects at 'deflinear'" T-LABEL
   IWG-DEFER-QUOT-FORGE$ IWG-EXEC:SUBJECT
   s" at 'deflinear'" IWG-ASSERT-DIAG ;

\ --- laundered-execute closure (dot habu-checker-exec-of-5923c543). Executing an
\ xt fetched from an untyped variable is now rejected at CHECK time as
\ E-EXEC-OPAQUE-XT (RSEXEC T-VAR flip), so the definer-mint and protected-registry
\ writes the RCA laundered through `variable V  ' W V !  : F V @ execute ;` can no
\ longer certify. Each child rejects rc 70 with the named diagnostic before the
\ body runs, so the mint / store never happens. ----

: IWG-LAUNDER-DEFINER$ ( ptr u8 n -- ptr u8 n )   \ tick <definer> into a var, launder its execute
   {: a:ptr u:n :}
   SB-RESET
   s" variable IWG-LV" SB-APPEND IWG-LF
   s" ' " SB-APPEND  a u SB-APPEND  s"  IWG-LV !" SB-APPEND IWG-LF
   s" : IWG-LBAD ( -- ) IWG-LV @ execute ;" SB-APPEND IWG-LF
   SB$ ;

: IWG-PF-LAUNDER-FORGE$ ( -- ptr u8 n )   \ tick the protected PF cell writer, launder its execute
   SB-RESET
   s" variable IWG-PFV" SB-APPEND IWG-LF
   s" : IWG-PFSET ( -- ) ['] PF-COMMIT-N IWG-PFV ! ;" SB-APPEND IWG-LF
   s" IWG-PFSET" SB-APPEND IWG-LF
   s" : IWG-PFBAD ( -- ) IWG-PFV @ execute 99 swap ! ;" SB-APPEND IWG-LF
   SB$ ;

: IWG-CATCH-DEFINER$ ( ptr u8 n -- ptr u8 n )   \ tick <definer> into a var, launder its catch
   {: a:ptr u:n :}
   SB-RESET
   s" variable IWG-LV" SB-APPEND IWG-LF
   s" ' " SB-APPEND  a u SB-APPEND  s"  IWG-LV !" SB-APPEND IWG-LF
   s" : IWG-LBAD ( -- n ) IWG-LV @ catch ;" SB-APPEND IWG-LF
   SB$ ;

: IWG-ASSERT-OPAQUE ( -- )   \ child rejected at CHECK (rc 70) naming the opaque-execute reject
   IWG-EXITED @ TTRUE
   IWG-RC @ IWG-REJECT-RC T=
   IWG-ERR$ s" at 'execute'" CONTAINS? TTRUE
   IWG-ERR$ s" opaque xt of unknown provenance" CONTAINS? TTRUE ;

: IWG-ASSERT-OPAQUE-CATCH ( -- )   \ child rejected at CHECK (rc 70) naming the opaque-catch reject
   IWG-EXITED @ TTRUE
   IWG-RC @ IWG-REJECT-RC T=
   IWG-ERR$ s" at 'catch'" CONTAINS? TTRUE
   IWG-ERR$ s" opaque xt of unknown provenance" CONTAINS? TTRUE ;

: IWG-LAUNDER-CASES ( -- )
   s" deflinear laundered through a variable rejects at CHECK (E-EXEC-OPAQUE-XT)" T-LABEL
   s" deflinear" IWG-LAUNDER-DEFINER$ IWG-EXEC:SUBJECT
   IWG-ASSERT-OPAQUE
   s" value-record laundered through a variable rejects at CHECK" T-LABEL
   s" VALUE-RECORD" IWG-LAUNDER-DEFINER$ IWG-EXEC:SUBJECT
   IWG-ASSERT-OPAQUE
   s" layout-buffer laundered through a variable rejects at CHECK" T-LABEL
   s" LAYOUT-BUFFER" IWG-LAUNDER-DEFINER$ IWG-EXEC:SUBJECT
   IWG-ASSERT-OPAQUE
   s" PF registry-cell write laundered through a variable rejects at CHECK, not runtime" T-LABEL
   IWG-PF-LAUNDER-FORGE$ IWG-EXEC:SUBJECT
   IWG-ASSERT-OPAQUE
   s" deflinear laundered through a variable + catch also rejects at CHECK" T-LABEL
   s" deflinear" IWG-CATCH-DEFINER$ IWG-EXEC:SUBJECT
   IWG-ASSERT-OPAQUE-CATCH ;

: IWG-POSITIVES ( -- )
   s" undefined word still reports E-UNDEFINED" T-LABEL
   IWG-UNDEF-FORGE$ IWG-EXEC:SUBJECT
   s" E-UNDEFINED" IWG-ASSERT-DIAG
   s" bare drop still reports E-UNDERFLOW" T-LABEL
   s" drop" IWG-TOKEN$ IWG-EXEC:SUBJECT
   s" E-UNDERFLOW" IWG-ASSERT-DIAG
   s" user unchecked word stays executable at top level" T-LABEL
   IWG-RAW-FORGE$ IWG-EXEC:SUBJECT IWG-ASSERT-OK
   s" top-level TRUST row still works" T-LABEL
   IWG-TRUST-FORGE$ IWG-EXEC:SUBJECT IWG-ASSERT-OK
   s" TRUSTED: + bare call still work" T-LABEL
   IWG-TRUSTED-FORGE$ IWG-EXEC:SUBJECT IWG-ASSERT-OK
   s" structures DSL still works" T-LABEL
   IWG-STRUCT-FORGE$ IWG-EXEC:SUBJECT IWG-ASSERT-OK
   s" SUMTYPE DSL still works" T-LABEL
   IWG-SUMTYPE-FORGE$ IWG-EXEC:SUBJECT IWG-ASSERT-OK
   s" LAYOUT-BUFFER DSL still works" T-LABEL
   IWG-LBUF-FORGE$ IWG-EXEC:SUBJECT IWG-ASSERT-OK
   s" layout-buffer in a checked body is rejected unsafe" T-LABEL
   IWG-LBUF-BODY-FORGE$ IWG-EXEC:SUBJECT
   s" at 'LAYOUT-BUFFER'" IWG-ASSERT-DIAG
   s" XREF of an internal word still works" T-LABEL
   IWG-XREF-FORGE$ IWG-EXEC:SUBJECT IWG-ASSERT-OK ;

\ --- registry write-protection (dot habu-protect-type-field-04d91409, Layer 1).
\ A din=0 registry control cell (variable/create) used to stay executable at top
\ level because the internal-word pass exempts data records; REG-PROTECT +
\ IMK-SEAL-REGISTRY now mark the PF cells DNAME-INT, so a bare cell name and the
\ confirmed `99 PF-COMMIT-N !` write both fail closed (rc 70, internal engine
\ word) on --load and stdin, while checked user code still rejects them earlier
\ as non-certified (E-UNDEFINED, covered by IWG-OPENER-CASES' PF-FIND row). ----
: IWG-PF-EXPLOIT-FORGE$ ( -- ptr u8 n )   \ the confirmed exploit: a bare registry-cell write
   SB-RESET
   s" 99 PF-COMMIT-N !" SB-APPEND IWG-LF
   SB$ ;

: IWG-REGISTRY-CASES ( -- )
   s" bare PF-CAP-V registry cell fails closed" T-LABEL
   s" PF-CAP-V" IWG-NEG
   s" bare PF-A-BOOT arena fails closed" T-LABEL
   s" PF-A-BOOT" IWG-NEG
   s" bare PF-A-P arena base fails closed" T-LABEL
   s" PF-A-P" IWG-NEG
   s" bare PF-N registry cell fails closed" T-LABEL
   s" PF-N" IWG-NEG
   s" bare PF-COMMIT-N registry cell fails closed" T-LABEL
   s" PF-COMMIT-N" IWG-NEG
   s" bare PF-TX-CAP-V registry cell fails closed" T-LABEL
   s" PF-TX-CAP-V" IWG-NEG
   s" bare PF-TX-BOOT arena fails closed" T-LABEL
   s" PF-TX-BOOT" IWG-NEG
   s" bare PF-TX-P arena base fails closed" T-LABEL
   s" PF-TX-P" IWG-NEG
   s" bare PF-TX-DEPTH registry cell fails closed" T-LABEL
   s" PF-TX-DEPTH" IWG-NEG
   s" bare PF-TX-SERIAL registry cell fails closed" T-LABEL
   s" PF-TX-SERIAL" IWG-NEG
   s" registry-cell write exploit fails closed on --load" T-LABEL
   IWG-PF-EXPLOIT-FORGE$ IWG-RUN-LOAD
   s" PF-COMMIT-N" IWG-ASSERT-INTERNAL
   s" registry-cell write exploit fails closed on stdin" T-LABEL
   IWG-PF-EXPLOIT-FORGE$ IWG-RUN-STDIN
   s" PF-COMMIT-N" IWG-ASSERT-INTERNAL ;

\ --- sibling type-registry write-protection (dot habu-protect-sibling-type-44eec932).
\ The family (TFAM), sum-variant (SUMV), interned-string, param-kind, logical-layout
\ (src/core/type-family.f) and schema/schema-root (src/core/type-schema.f) registries
\ are as exposed as PF-COMMIT-N was: each control cell is a din=0 data record the
\ internal-word pass would leave executable. REG-PROTECT + IMK-SEAL-REGISTRY now seal
\ every cell DNAME-INT, so a bare cell name, a bare `99 <cell> !` write, and a bare
\ `' <cell>` tick all fail closed (rc 70, internal engine word) exactly like the PF
\ cells. Compiled cold-prefix writers and the certified accessors (TFAM-N@, SUMV-N@,
\ TF-STR-U@, TF-PK-N@, SCHEMA-N@, SCHEMA-ROOT-N@) are unaffected. ----
: IWG-WRITE-FORGE$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}   \ program "99 <cell> !"
   SB-RESET
   s" 99 " SB-APPEND  a u SB-APPEND  s"  !" SB-APPEND IWG-LF
   SB$ ;

: IWG-TICK-CELL-FORGE$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}   \ program "' <cell>"
   SB-RESET
   s" ' " SB-APPEND  a u SB-APPEND IWG-LF
   SB$ ;

: IWG-CELL-WT ( ptr u8 n -- ) {: a:ptr u:n :}   \ bare write AND bare tick both fail closed naming the cell
   a u IWG-WRITE-FORGE$ IWG-EXEC:SUBJECT      a u IWG-ASSERT-INTERNAL
   a u IWG-TICK-CELL-FORGE$ IWG-EXEC:SUBJECT   a u IWG-ASSERT-INTERNAL ;

: IWG-TFAM-CASES ( -- )
   s" bare TF-CAP-V family-registry capacity cell fails closed" T-LABEL   s" TF-CAP-V" IWG-NEG
   s" bare TF-A-BOOT family arena fails closed" T-LABEL                    s" TF-A-BOOT" IWG-NEG
   s" bare TF-A-P family arena base fails closed" T-LABEL                  s" TF-A-P" IWG-NEG
   s" bare TFAM-N family high-water fails closed" T-LABEL                  s" TFAM-N" IWG-NEG
   s" TFAM-N high-water write + tick fail closed" T-LABEL                  s" TFAM-N" IWG-CELL-WT ;

: IWG-SUMV-CASES ( -- )
   s" bare SUMV-CAP-V variant-registry capacity cell fails closed" T-LABEL  s" SUMV-CAP-V" IWG-NEG
   s" bare SUMV-A-BOOT variant arena fails closed" T-LABEL                  s" SUMV-A-BOOT" IWG-NEG
   s" bare SUMV-A-P variant arena base fails closed" T-LABEL                s" SUMV-A-P" IWG-NEG
   s" bare SUMV-N variant high-water fails closed" T-LABEL                  s" SUMV-N" IWG-NEG
   s" SUMV-N high-water write + tick fail closed" T-LABEL                   s" SUMV-N" IWG-CELL-WT ;

: IWG-STR-CASES ( -- )
   s" bare TF-STR-CAP-V string-pool capacity cell fails closed" T-LABEL   s" TF-STR-CAP-V" IWG-NEG
   s" bare TF-STR-BOOT string pool fails closed" T-LABEL                  s" TF-STR-BOOT" IWG-NEG
   s" bare TF-STR-P string-pool base fails closed" T-LABEL               s" TF-STR-P" IWG-NEG
   s" bare TF-STR-U string-pool high-water fails closed" T-LABEL         s" TF-STR-U" IWG-NEG
   s" TF-STR-U high-water write + tick fail closed" T-LABEL              s" TF-STR-U" IWG-CELL-WT ;

: IWG-PK-CASES ( -- )
   s" bare TF-PK-CAP-V param-pool capacity cell fails closed" T-LABEL   s" TF-PK-CAP-V" IWG-NEG
   s" bare TF-PK-BOOT param pool fails closed" T-LABEL                  s" TF-PK-BOOT" IWG-NEG
   s" bare TF-PK-P param-pool base fails closed" T-LABEL               s" TF-PK-P" IWG-NEG
   s" bare TF-PK-N param-pool high-water fails closed" T-LABEL         s" TF-PK-N" IWG-NEG
   s" TF-PK-N high-water write + tick fail closed" T-LABEL             s" TF-PK-N" IWG-CELL-WT ;

: IWG-LAY-CASES ( -- )
   s" bare LAY-CAP-V layout-registry capacity cell fails closed" T-LABEL   s" LAY-CAP-V" IWG-NEG
   s" bare LAY-A-BOOT layout arena fails closed" T-LABEL                   s" LAY-A-BOOT" IWG-NEG
   s" bare LAY-A-P layout arena base fails closed" T-LABEL                 s" LAY-A-P" IWG-NEG
   s" bare LAY-N layout high-water fails closed" T-LABEL                   s" LAY-N" IWG-NEG
   s" LAY-N high-water write + tick fail closed" T-LABEL                   s" LAY-N" IWG-CELL-WT ;

: IWG-SCH-CASES ( -- )
   s" bare SCH-CAP-V schema-registry capacity cell fails closed" T-LABEL     s" SCH-CAP-V" IWG-NEG
   s" bare SCH-A-BOOT schema arena fails closed" T-LABEL                     s" SCH-A-BOOT" IWG-NEG
   s" bare SCH-A-P schema arena base fails closed" T-LABEL                   s" SCH-A-P" IWG-NEG
   s" bare SCH-N schema node high-water fails closed" T-LABEL                s" SCH-N" IWG-NEG
   s" SCH-N node high-water write + tick fail closed" T-LABEL                s" SCH-N" IWG-CELL-WT
   s" bare SCH-ROOT-CAP-V schema-root capacity cell fails closed" T-LABEL    s" SCH-ROOT-CAP-V" IWG-NEG
   s" bare SCH-ROOT-BOOT schema-root pool fails closed" T-LABEL              s" SCH-ROOT-BOOT" IWG-NEG
   s" bare SCH-ROOT-P schema-root base fails closed" T-LABEL                 s" SCH-ROOT-P" IWG-NEG
   s" bare SCH-ROOT-N schema-root high-water fails closed" T-LABEL           s" SCH-ROOT-N" IWG-NEG
   s" SCH-ROOT-N high-water write + tick fail closed" T-LABEL                s" SCH-ROOT-N" IWG-CELL-WT ;

: IWG-SIBLING-CASES ( -- )
   IWG-TFAM-CASES
   IWG-SUMV-CASES
   IWG-STR-CASES
   IWG-PK-CASES
   IWG-LAY-CASES
   IWG-SCH-CASES ;

\ --- field-liveness internalization (dot habu-internalize-field-liveness).
\ src/core/checker.f used to publish a global CT-LIVE? primitive-effect axiom
\ solely so the field-schema validator (type-family.f PF-NODE-KIND?) could ask
\ whether a SCHEMA-CON's concrete-type code is live. That axiom left the
\ checker-internal concrete-type registry query top-level executable and callable
\ from checked user code. The axiom is gone, so CT-LIVE? now carries DNAME-INT
\ like its sibling CT-LINEAR? — a bare call fails closed on BOTH cold-prefix
\ paths (--load file and piped stdin) while the compiled field-validation caller
\ is unaffected. ----
: IWG-CTLIVE-CASES ( -- )
   s" CT-LIVE? field-liveness query fails closed on --load" T-LABEL
   s" CT-LIVE?" IWG-TOKEN$ IWG-RUN-LOAD
   s" CT-LIVE?" IWG-ASSERT-INTERNAL
   s" CT-LIVE? field-liveness query fails closed on stdin" T-LABEL
   s" CT-LIVE?" IWG-TOKEN$ IWG-RUN-STDIN
   s" CT-LIVE?" IWG-ASSERT-INTERNAL ;

package IWG-PARITY

9 constant DIRECT-N
102 constant SUBJECT-N
: RESULT ( -- ptr u8 n ptr u8 n n )
   IWG-OUT IWG-OUT-U @ IWG-ERR IWG-ERR-U @ IWG-RC @ ;

: NEG-LOAD ( ptr u8 n -- )
   2dup IWG-RUN-LOAD
   s" U-TYPE" IWG-ASSERT-INTERNAL
   RESULT TAIL-RATCHET:SNAPSHOT
   IWG-EXEC:SUBJECT
   s" U-TYPE" IWG-ASSERT-INTERNAL
   RESULT TAIL-RATCHET:SAME ;

: NEG-STDIN ( ptr u8 n -- )
   2dup IWG-RUN-STDIN
   s" U-TYPE" IWG-ASSERT-INTERNAL
   RESULT TAIL-RATCHET:SNAPSHOT
   IWG-EXEC:SUBJECT
   s" U-TYPE" IWG-ASSERT-INTERNAL
   RESULT TAIL-RATCHET:SAME ;

: POS-LOAD ( ptr u8 n -- )
   2dup IWG-RUN-LOAD IWG-ASSERT-OK
   RESULT TAIL-RATCHET:SNAPSHOT
   IWG-EXEC:SUBJECT IWG-ASSERT-OK
   RESULT TAIL-RATCHET:SAME ;

public

: TEST ( -- )
   s" direct --load and subject preserve raw internal-word results" T-LABEL
   s" U-TYPE" IWG-TOKEN$ NEG-LOAD
   s" direct stdin and subject preserve raw internal-word results" T-LABEL
   s" U-TYPE" IWG-TOKEN$ NEG-STDIN
   s" direct --load and subject preserve raw successful results" T-LABEL
   IWG-RAW-FORGE$ POS-LOAD ;

: CHECK ( -- )
   DIRECT-N SUBJECT-N TAIL-RATCHET:CHECK ;

;package

: IWG-PREPARE ( -- )
   CLEANUP-RESET
   s" habu-iwg" TMPDIR-MKDIR {: a:ptr u:n :}
   a u IWG-ROOT-BUF IWG-ROOT-U IWG-COPY!
   IWG-ROOT CLEANUP-TREE+
   IWG-ROOT s" forge.f" IWG-CHILD-BUF JOIN-PATH IWG-CHILD-U ! ;

: IWG-CLEANUP ( -- )
   CLEANUP-RUN
   IWG-ROOT EXISTS? TFALSE ;

: IWG-MAIN ( -- )
   T-RESET
   TAIL-RATCHET:START
   IWG-PREPARE
   IWG-PARITY:TEST
   IWG-NEG-BARE
   IWG-REGISTRY-CASES
   IWG-SIBLING-CASES
   IWG-CTLIVE-CASES
   IWG-NEG-SHAPES
   IWG-POSITIVES
   IWG-OPENER-CASES
   IWG-DEFER-CASES
   IWG-LAUNDER-CASES
   IWG-PARITY:CHECK
   IWG-CLEANUP
   T-REPORT
   s" internal-word-gate: ok" type cr ;

IWG-MAIN
