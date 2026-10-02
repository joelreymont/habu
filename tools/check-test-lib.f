\ check-test-lib.f - checked engine CLI/core smoke coverage library.
\ Run: bin/hb --load lib/date.f lib/errors.f lib/string.f lib/test.f lib/memory.f
\ lib/vector.f lib/fs.f lib/fs-mutate.f lib/process.f lib/process-argv.f
\ lib/process-env.f lib/process-cwd.f lib/source.f
\ tools/lint/text.f tools/lint/token.f tools/lint/lib.f
\ tools/lint/json-writer.f tools/lint/source-lex.f
\ tools/diag-origin-core.f tools/json.f tools/json-only-core.f
\ tools/checked-boundary-lint-core.f
\ tools/reserved-name-lint-core.f
\ tools/check-all-errors-core.f lib/argv.f
\ tools/check-core.f tools/check-test.f

require lib/date.f
require lib/errors.f
require lib/string.f
require lib/test.f
require lib/test/runner.f
require lib/memory.f
require lib/span.f
require lib/vector.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/process-cwd.f
require lib/source.f
require tools/lint/text.f
require tools/lint/token.f
require tools/lint/lib.f
require tools/lint/json-writer.f
require tools/lint/source-lex.f
require tools/diag-origin-core.f
require tools/json.f
require tools/json-only-core.f
require tools/checked-boundary-lint-core.f
require tools/reserved-name-lint-core.f
require tools/check-all-errors-core.f
require lib/argv.f
require tools/check-core.f
require lib/fmt.f                        \ FMT:.INT - one-line number text

package CHECK-TEST

using CHECK
private

$4000 constant BUF-CAP
$100001 constant OVERCAP-SOURCE-LEN
128 constant LIST-ENTRY-CAP

\ What the child cases here prove is the standalone command line: the exit status
\ of `bin/hb tools/check.f ...`, the diagnostics it writes, and the temporary
\ files it removes. None of that depends on how long a child takes, so the
\ millisecond budget handed to every capture is a deadlock guard: it exists so a
\ child that never exits cannot hang the gate forever.
\
\ Measured 2026-07-30 on a 12-core machine. The heaviest child is the cleanup
\ child, 4.7 to 5.0 s at an ambient load average of 13 and 11.2 to 13.4 s while
\ eight gate pool slots are busy; the `--load tools/check.f` children are 2.0 to
\ 3.2 s and 7.5 to 8.7 s under the same two conditions. WORST-CHILD-MS records
\ the busiest measurement. HANG-MARGIN is 4 rather than the order of magnitude
\ the cheaper fixtures can afford, because the product is bounded from above by
\ the registry's outer timeout as well.
\
\ Load can still reach the 54 s guard: nothing bounds how far a busy host
\ stretches a child. At a load average near 73 an engine build took 2.9 times
\ its time under ordinary gate load (259 s against about 90 s), and the busiest
\ child above stretched that far takes 39 s. The current cases are faster: the
\ slowest took 2.8 s alone at a load average of 7 to 10, and the whole row took
\ 11.2 s in a full gate at 6 to 40, so a child now reaches the guard only about
\ 19 times slower than that. An expiry is therefore a timeout, not proof of a
\ deadlock: CASE-HUNG names the case and rethrows E-PROC-TIMEOUT, and the gate
\ pool labels the row TIMEOUT-UNDER-LOAD.
13500 constant WORST-CHILD-MS
4 constant HANG-MARGIN
WORST-CHILD-MS HANG-MARGIN * constant CHILD-HANG-MS

create TMP-ROOT FS-PATH-CAP allot
create BAD-PATH FS-PATH-CAP allot
create DIRECT-PATH FS-PATH-CAP allot
create LIST-PATH FS-PATH-CAP allot
create INC-DEP-PATH FS-PATH-CAP allot
create INC-ENTRY-PATH FS-PATH-CAP allot
create SUP-PATH FS-PATH-CAP allot
create USE-PATH FS-PATH-CAP allot
create CYC-ROOT-PATH FS-PATH-CAP allot
create CYC-HOST-PATH FS-PATH-CAP allot
create CYC-ENTRY-PATH FS-PATH-CAP allot
create MUT-SRC $200 allot
create MUT-LABEL FS-PATH-CAP allot
create MUT-PATH FS-PATH-CAP allot
create BOUNDARY-OUT BUF-CAP allot
create CLEANUP-TMP FS-PATH-CAP allot
create CLI-ROOT FS-PATH-CAP allot
create CLI-TARGET FS-PATH-CAP allot
create CLI-LINK-PATH FS-PATH-CAP allot
create CLI-HB FS-PATH-CAP allot
create ENV-PROBE-PATH FS-PATH-CAP allot

TYPED-VARIABLE OUT-A ptr u8
TYPED-VARIABLE ERR-A ptr u8
variable TMP-ROOT-U
variable BAD-U
variable DIRECT-U
variable LIST-U
variable INC-DEP-U
variable INC-ENTRY-U
variable SUP-U
variable USE-U
variable CYC-ROOT-U
variable CYC-HOST-U
variable CYC-ENTRY-U
variable MUT-SRC-U
variable MUT-LABEL-U
variable MUT-PATH-U
variable CLEANUP-TMP-U
variable CLI-ROOT-U
variable CLI-TARGET-U
variable CLI-LINK-U
variable CLI-HB-U
variable ENV-PROBE-U
variable START-NS

: PATH-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u:n dst:ptr lenp:ptr :}
   a dst u BYTE-COPY
   u lenp ! ;

: ROOT$ ( -- ptr u8 n )
   TMP-ROOT TMP-ROOT-U @ ;

: CLI-ROOT$ ( -- ptr u8 n )
   CLI-ROOT CLI-ROOT-U @ ;

: BAD$ ( -- ptr u8 n )
   BAD-PATH BAD-U @ ;

: DIRECT$ ( -- ptr u8 n )
   DIRECT-PATH DIRECT-U @ ;

: CLEANUP-TMP$ ( -- ptr u8 n )
   CLEANUP-TMP CLEANUP-TMP-U @ ;

: ENV-PROBE$ ( -- ptr u8 n )
   ENV-PROBE-PATH ENV-PROBE-U @ ;

: LIST$ ( -- ptr u8 n )
   LIST-PATH LIST-U @ ;

: INC-DEP$ ( -- ptr u8 n )
   INC-DEP-PATH INC-DEP-U @ ;

: INC-ENTRY$ ( -- ptr u8 n )
   INC-ENTRY-PATH INC-ENTRY-U @ ;

: SUP$ ( -- ptr u8 n )
   SUP-PATH SUP-U @ ;

: USE$ ( -- ptr u8 n )
   USE-PATH USE-U @ ;

: CYC-ROOT$ ( -- ptr u8 n )
   CYC-ROOT-PATH CYC-ROOT-U @ ;

: CYC-HOST$ ( -- ptr u8 n )
   CYC-HOST-PATH CYC-HOST-U @ ;

: CYC-ENTRY$ ( -- ptr u8 n )
   CYC-ENTRY-PATH CYC-ENTRY-U @ ;

: ABS-PATH? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   u 0 > if a c@ $2F = else 0 0= 0= then ;

\ A relative path made absolute against the process's real working directory.
\ The environment's PWD is not that: a gate runs its children under `env -i`,
\ where PWD is unset, and a link target joined to an empty PWD dangles, so the
\ missing-engine child died 74 on its own load path before it could say
\ `bin/hb missing`.
: CLI-ABS! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u:n buf:ptr lenp:ptr :}
   a u FS-PATHZ buf FS-PATH-CAP realpath {: n:n :}
   n 0 <= if E-FS-PATH throw then
   n lenp ! ;

: CLI-LINK+ ( ptr u8 n -- ) {: name:ptr nameu:n :}
   name nameu CLI-TARGET CLI-TARGET-U CLI-ABS!
   CLI-ROOT$ name nameu CLI-LINK-PATH JOIN-PATH CLI-LINK-U !
   CLI-TARGET CLI-TARGET-U @ CLI-LINK-PATH CLI-LINK-U @ MAKE-SYMLINK ;

: OUT-A@ ( -- ptr u8 )
   OUT-A @ ;

: ERR-A@ ( -- ptr u8 )
   ERR-A @ ;

: OUT-A! ( ptr u8 -- )
   OUT-A ! ;

: ERR-A! ( ptr u8 -- )
   ERR-A ! ;

: CAP-ALLOC ( -- ptr u8 )
   BUF-CAP MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop ;

: CAP-OUT ( -- ptr u8 )
   OUT-A @ 0= if CAP-ALLOC OUT-A! then
   OUT-A@ ;

: CAP-ERR ( -- ptr u8 )
   ERR-A @ 0= if CAP-ALLOC ERR-A! then
   ERR-A@ ;

\ Stream capture for the in-process CHECK drivers below. The check tool writes
\ its diagnostics with plain `write` calls on the process stdout and stderr, so
\ the only faithful way to read them back without a test-only hook inside the
\ tool is to move those two descriptors for the duration of one run
\ (lib/test/runner.f GT-CAPTURE-ACTION). The production code stays untouched
\ and the assertions see exactly the bytes a command-line user would see.
: IN-PROC ( [ -- ] -- n n n )   \ outn errn code, the same triple the CLI drivers return
   {: q :}
   q ROOT$ CAP-OUT BUF-CAP SPAN:MAKE CAP-ERR BUF-CAP SPAN:MAKE GT-CAPTURE-ACTION
   {: outu:len erru:len rc:n :}
   outu LEN>N erru LEN>N rc ;

\ CHECK:MAIN turns a non-zero run code into a throw, which the engine reports as
\ the process exit status. The in-process drivers reproduce that mapping so a
\ converted test still compares against the code the command line would exit with.
: RUN-ACT ( -- )
   RUN dup 0 <> if throw then drop ;

: CORE-ACT ( -- )
   BAD$ BAD$ CHECK-ALL-ERRORS:FILE ;

: CORE-JSON ( ptr u8 n -- n n n ) {: src:ptr srcu:n :}
   BAD$ src srcu WRITE-ALL
   CAP-ERR BUF-CAP CAP-OUT BUF-CAP CHECK-ALL-ERRORS:BUFFERS!
   0 0= CHECK-ALL-ERRORS:JSON!
   [: CORE-ACT ;] catch {: rc:n :}
   0 CHECK-ALL-ERRORS:OUT$ nip rc ;

: MUT-SRC$ ( -- ptr u8 n )
   MUT-SRC MUT-SRC-U @ ;

: MUT-LABEL$ ( -- ptr u8 n )
   MUT-LABEL MUT-LABEL-U @ ;

: MUT-PATH$ ( -- ptr u8 n )
   MUT-PATH MUT-PATH-U @ ;

: MUT-SRC! ( ptr u8 n -- )
   MUT-SRC MUT-SRC-U PATH-COPY! ;

: MUT-LABEL! ( ptr u8 n -- )
   MUT-LABEL MUT-LABEL-U PATH-COPY! ;

: MUT-PATH! ( ptr u8 n -- )
   MUT-PATH MUT-PATH-U PATH-COPY! ;

: EMPTY-SOURCE ( -- )
   s" " s" empty-source.f" SOURCE ;

: BAD-FILE ( -- )
   BAD$ FILE ;

: LIST-OPT ( -- )
   s" source-list" OPT ;

: CAPTURE>N ( result<pcap:captured,pcap:failed> -- n n n )   \ outn errn code (0 on clean exit)
   MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE {: o:len e:len :} o LEN>N e LEN>N 0 ENDOF
     err OF PCAP-FAILED:UNMAKE  {: o:len e:len c:rc :} o LEN>N e LEN>N c RC>N ENDOF
   ;MATCH ;

: GOOD$ ( -- ptr u8 n )
   s" : CKT-OK ( i64 -- i64 ) dup * ;" ;

: BAD$SRC ( -- ptr u8 n )
   s" : CKT-BAD-WORD ( i64 -- i64 ) dup ;" ;

: PRELUDE-EVAL$ ( ptr u8 n -- ptr u8 n ) {: body:ptr bodyu:n :}
   SB-RESET
   s" s" SB-APPEND
   $22 SB-APPEND-C
   $20 SB-APPEND-C
   body bodyu SB-APPEND
   $22 SB-APPEND-C
   s"  evaluate" SB-APPEND
   SB$ ;

: FWDREF$ ( -- ptr u8 n )
   s" : CKT-FWDREF ( -- ) CKT-MISSING ;" ;

: HB$ ( -- ptr u8 n )
   s" HABU_UNDER_TEST" >LEN PROC-ENV-DEFAULT$? if LEN>N exit then
   2drop
   s" HABU_UNDER_TEST" GETENV dup 0= if
      2drop s" bin/hb" exit
   then ;

: CHECK-ARGV-START ( -- )
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   s" tools/check.f" >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+ ;

: CHECK-ARG+ ( ptr u8 n -- )
   >LEN PROC-ARGV+ ;

: CHECK-CAPTURE ( -- n n n )
   HB$ >LEN CAP-OUT BUF-CAP >LEN
   CAP-ERR BUF-CAP >LEN CHILD-HANG-MS >MS RUN-ARGV-CAPTURE
   CAPTURE>N ;

: CHECK-STDIN-CAPTURE ( ptr u8 n -- n n n )
   {: src:ptr srcu:n :}
   HB$ >LEN src srcu >LEN
   CAP-OUT BUF-CAP >LEN CAP-ERR BUF-CAP >LEN
   CHILD-HANG-MS >MS RUN-ARGV-STDIN-CAPTURE
   CAPTURE>N ;

\ ---- command-line drivers ---------------------------------------------------
\ These stay on a real child process because they are the standalone entry
\ coverage: argument parsing, reading the program from standard input, and the
\ exit status of `bin/hb tools/check.f`. Everything else drives the same tool
\ in this process through the CHECK package's public words, and CLI-PATH is the
\ verdict such an in-process check must repeat.

: CLI-STDIN ( ptr u8 n -- n n n )
   CHECK-ARGV-START
   CHECK-STDIN-CAPTURE ;

: CLI-PATH ( ptr u8 n -- n n n )
   CHECK-ARGV-START
   CHECK-ARG+
   CHECK-CAPTURE ;

: CLI-ALL-LIST ( ptr u8 n ptr u8 n -- n n n )
   {: aa:ptr au:n ba:ptr bu:n :}
   CHECK-ARGV-START
   s" --json-errors" CHECK-ARG+
   s" --all-errors" CHECK-ARG+
   s" --source-list" CHECK-ARG+
   aa au CHECK-ARG+
   ba bu CHECK-ARG+
   CHECK-CAPTURE ;

: CLI-BAD-FLAG ( -- n n n )
   CHECK-ARGV-START
   s" --bad-flag" CHECK-ARG+
   CHECK-CAPTURE ;

\ ---- in-process drivers -----------------------------------------------------
\ Same tool, same phases, same diagnostics: only the process boundary is gone.
\ Sources that the command line reads from standard input are selected with
\ CHECK:SOURCE under the `<stdin>` label the tool itself uses, so every
\ diagnostic still names the same file.

: STDIN-LABEL$ ( -- ptr u8 n )
   s" <stdin>" ;

: SELECT-STDIN ( ptr u8 n -- ) {: src:ptr srcu:n :}
   src srcu STDIN-LABEL$ SOURCE ;

: DIRECT-STDIN ( ptr u8 n -- n n n )
   RESET
   SELECT-STDIN
   [: RUN-ACT ;] IN-PROC ;

: DIRECT-ALL-STDIN ( ptr u8 n -- n n n )
   RESET
   s" all-errors" OPT
   SELECT-STDIN
   [: RUN-ACT ;] IN-PROC ;

: ALL-JSON-STDIN ( ptr u8 n -- n n n )
   RESET
   s" all-errors" OPT
   s" json-errors" OPT
   SELECT-STDIN
   [: RUN-ACT ;] IN-PROC ;

: DIRECT-JSON-STDIN ( ptr u8 n -- n n n )
   RESET
   s" json-errors" OPT
   SELECT-STDIN
   [: RUN-ACT ;] IN-PROC ;

: LIST-RUN ( ptr u8 n -- n n n )
   RESET
   LIST-OPT
   FILE
   [: RUN-ACT ;] IN-PROC ;

: EMPTY-LIST-RUN ( -- n n n )
   RESET
   LIST-OPT
   [: RUN-ACT ;] IN-PROC ;

: PATH-RUN ( ptr u8 n -- n n n )
   RESET
   FILE
   [: RUN-ACT ;] IN-PROC ;

: CLI-HB$ ( -- ptr u8 n )
   HB$ {: hb:ptr hbu:n :}
   hb hbu ABS-PATH? if hb hbu exit then
   hb hbu CLI-HB CLI-HB-U CLI-ABS!
   CLI-HB CLI-HB-U @ ;

: CLI-SETUP ( -- )
   s" habu-check-missing-engine" HB-TMP-MKDIR
   CLI-ROOT CLI-ROOT-U PATH-COPY!
   CLI-ROOT$ CLEANUP-TREE+
   s" lib" CLI-LINK+
   s" tools" CLI-LINK+
   s" src" CLI-LINK+ ;

: CLI-MISSING-HB ( ptr u8 n -- n n n ) {: entry:ptr entryu:n :}
   PROC-ARGV-RESET
   PROC-ENV-RESET
   PROC-ENV-INHERIT-MISSING
   s" --load" >LEN PROC-ARGV+
   entry entryu >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   CLI-HB$ >LEN CLI-ROOT$ >LEN
   CAP-OUT BUF-CAP >LEN CAP-ERR BUF-CAP >LEN CHILD-HANG-MS >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-CAPTURE
   CAPTURE>N ;

: CLEANUP-TMP! ( ptr u8 n -- )
   ROOT$ 2swap CLEANUP-TMP JOIN-PATH CLEANUP-TMP-U !
   CLEANUP-TMP$ MAKE-DIR ;

: CLEANUP-TMP-RESTORE ( -- )
   CLEANUP-TMP$ 2dup EXISTS? if
      FS-MUT-MODE-PRIVATE-DIR CHMOD-MODE
   else
      2drop
   then ;

: CLEANUP-CHILD-RUN ( ptr u8 n -- n n n )
   {: mode:ptr modeu:n :}
   mode modeu CLEANUP-TMP!
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   s" tools/check-cleanup-child.f" >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   mode modeu >LEN PROC-ARGV+
   PROC-ENV-RESET
   s" TMPDIR" >LEN CLEANUP-TMP$ >LEN PROC-ENV+
   \ The checker's scratch resolves through HB-TMP-MKDIR, so the test-owned
   \ root must be the child's HB_TMP too, or an inherited pool slot takes it.
   s" HB_TMP" >LEN CLEANUP-TMP$ >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   HB$ >LEN CAP-OUT BUF-CAP >LEN CAP-ERR BUF-CAP >LEN
   CHILD-HANG-MS >MS RUN-ARGV-ENV-CAPTURE
   CAPTURE>N ;

: CLEANUP-CHILD-ERR ( ptr u8 n n -- )
   {: mode:ptr modeu:n erru:n :}
   mode modeu s" cleanup" STR= if erru 0 T= exit then
   CAP-ERR erru s" cleanup primary" CONTAINS? TTRUE ;

: CLEANUP-CHILD-CASE ( ptr u8 n -- )
   {: mode:ptr modeu:n :}
   mode modeu CLEANUP-CHILD-RUN
   {: outu:n erru:n rc:n :}
   CLEANUP-TMP-RESTORE
   rc 0 T=
   CAP-OUT outu s" test: ok" CONTAINS? TTRUE
   CLEANUP-TMP$ EXISTS? TFALSE
   mode modeu erru CLEANUP-CHILD-ERR ;

: TEST-CLEANUP-FAILURE ( -- )
   s" cleanup" CLEANUP-CHILD-CASE
   s" primary" CLEANUP-CHILD-CASE ;

\ ---- the run stage's environment --------------------------------------------
\ The tool's run stage spawns the checked program, and a program that resolves
\ an executable through $PATH or writes under $HB_TMP needs the environment the
\ tool itself was given. CHECK-CAPTURE spawns the tool without one, so a row
\ that only read PATH would prove nothing: the tool would have no PATH to pass
\ on. This row therefore gives the child tool an environment of its own - one
\ marker row plus this process's - and reads the marker back out of the checked
\ program's output. An env-less run stage prints an empty marker line instead.

: ENV-MARK$ ( -- ptr u8 n )
   s" hb-check-env-probe-9f3c" ;

\ Computed the same way in this process and in the checked program, so the two
\ agree on a host that exports no PATH at all.
: ENV-PATH-LINE$ ( -- ptr u8 n )
   s" PATH" GETENV nip 0 > if
      s" env-path yes" else s" env-path no" then ;

: ENV-PROBE$SRC ( -- ptr u8 n )
   SB-RESET
   s\" : CKT-ENV-SHOW ( -- )\n" SB-APPEND
   s\"    s\q HB_CHECK_ENV_PROBE\q GETENV type cr\n" SB-APPEND
   s\"    s\q PATH\q GETENV nip 0 > if\n" SB-APPEND
   s\"       s\q env-path yes\q else s\q env-path no\q then type cr ;\n" SB-APPEND
   s\" CKT-ENV-SHOW\n" SB-APPEND
   SB$ ;

: ENV-PROBE-RUN ( -- n n n )
   ENV-PROBE$ ENV-PROBE$SRC WRITE-ALL
   CHECK-ARGV-START
   ENV-PROBE$ CHECK-ARG+
   PROC-ENV-RESET
   s" HB_CHECK_ENV_PROBE" >LEN ENV-MARK$ >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   HB$ >LEN CAP-OUT BUF-CAP >LEN CAP-ERR BUF-CAP >LEN
   CHILD-HANG-MS >MS RUN-ARGV-ENV-CAPTURE
   CAPTURE>N ;

: TEST-RUN-ENVIRONMENT ( -- )
   ENV-PROBE-RUN 0 T=
   {: outu:n erru:n :}
   erru 0 T=
   CAP-OUT outu ENV-MARK$ CONTAINS? TTRUE
   CAP-OUT outu ENV-PATH-LINE$ CONTAINS? TTRUE ;

: HB-LOAD-SRC ( ptr u8 n -- n n n ) {: src:ptr srcu:n :}
   BAD$ src srcu WRITE-ALL
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   BAD$ >LEN PROC-ARGV+
   HB$ >LEN CAP-OUT BUF-CAP >LEN
   CAP-ERR BUF-CAP >LEN CHILD-HANG-MS >MS RUN-ARGV-CAPTURE
   CAPTURE>N ;

: DUP$SRC ( -- ptr u8 n )
   SB-RESET
   s" : CKT-DUP ( i64 -- i64 ) 1 + ;" SB-APPEND
   $0a SB-APPEND-C
   s" : CKT-DUP ( i64 -- i64 ) 2 + ;" SB-APPEND
   SB$ ;

: RESERVED$ ( -- ptr u8 n )
   s" variable I" ;

: UNDEFINED$SRC ( -- ptr u8 n )
   s" : CKT-MISS ( i64 -- i64 ) dup NOPE ;" ;

: VREC-GOOD$ ( -- ptr u8 n )
   SB-RESET
   s" DEFLINEAR own" SB-APPEND
   $0a SB-APPEND-C
   s" VALUE-RECORD point x n y n END-VALUE-RECORD" SB-APPEND
   $0a SB-APPEND-C
   s" VALUE-RECORD box value a END-VALUE-RECORD" SB-APPEND
   $0a SB-APPEND-C
   s" VALUE-RECORD hdl owner own raw ptr u8 END-VALUE-RECORD" SB-APPEND
   $0a SB-APPEND-C
   s" : CKT-OWN-PASS ( own -- own ) ;" SB-APPEND
   $0a SB-APPEND-C
   s" : CKT-MAKE-POINT ( n n -- point ) ;" SB-APPEND
   $0a SB-APPEND-C
   s" : CKT-TAKE-POINT ( point -- n n ) ;" SB-APPEND
   $0a SB-APPEND-C
   s" : CKT-COPY-POINT ( point -- point point ) over over ;" SB-APPEND
   $0a SB-APPEND-C
   s" : CKT-POINT-X ( point -- n ) drop ;" SB-APPEND
   $0a SB-APPEND-C
   s" : CKT-POINT-Y ( point -- n ) nip ;" SB-APPEND
   $0a SB-APPEND-C
   s" : CKT-POINT-X! ( n point -- point ) swap drop ;" SB-APPEND
   $0a SB-APPEND-C
   s" : CKT-POINT-Y! ( point n -- point ) >r drop r> ;" SB-APPEND
   $0a SB-APPEND-C
   s" : CKT-MAKE-BOX ( a -- box ) ;" SB-APPEND
   $0a SB-APPEND-C
   s" : CKT-TAKE-BOX ( box -- a ) ;" SB-APPEND
   $0a SB-APPEND-C
   s" : CKT-HDL-PASS ( hdl -- hdl ) ;" SB-APPEND
   SB$ ;

: LINEAR-BAD$ ( -- ptr u8 n )
   SB-RESET
   s" DEFLINEAR own" SB-APPEND
   $0a SB-APPEND-C
   s" : CKT-BAD-OWN-DUP ( own -- own own ) dup ;" SB-APPEND
   SB$ ;

: TFAM-GOOD$ ( -- ptr u8 n )   \ declarations + signature use, multi-line
   SB-RESET
   s" NEWTYPE tfck 1" SB-APPEND
   $0a SB-APPEND-C
   s" SUMTYPE rsck 2" SB-APPEND
   $0a SB-APPEND-C
   s"   VARIANT ok  a ;VARIANT" SB-APPEND
   $0a SB-APPEND-C
   s"   VARIANT err b ;VARIANT" SB-APPEND
   $0a SB-APPEND-C
   s" ;SUMTYPE" SB-APPEND
   $0a SB-APPEND-C
   s" : CKT-TF-PASS ( tfck<n> -- tfck<n> ) ;" SB-APPEND
   $0a SB-APPEND-C
   s" : CKT-RS-PASS ( rsck<n,f> -- rsck<n,f> ) ;" SB-APPEND
   SB$ ;

: TFAM-BAD$ ( -- ptr u8 n )    \ duplicate variant rejects fail-closed
   SB-RESET
   s" SUMTYPE rsbad 1 VARIANT ok a ;VARIANT VARIANT ok a ;VARIANT ;SUMTYPE" SB-APPEND
   $0a SB-APPEND-C
   s" : CKT-AFTER-BAD ( n -- n ) ;" SB-APPEND
   SB$ ;

\ S2: a declaration PRE-CHECK reject (missing arity / unterminated) must report
\ the same E-BAD-DECLARATION packet the native path emits, not a raw die.
: TFAM-NOARITY$ ( -- ptr u8 n )   \ NEWTYPE missing its arity token
   SB-RESET
   s" NEWTYPE ckfnoar" SB-APPEND
   SB$ ;

: SUM-NOEND$ ( -- ptr u8 n )      \ SUMTYPE body never reaches ;SUMTYPE
   SB-RESET
   s" SUMTYPE cksnoend 1 VARIANT ok a ;VARIANT" SB-APPEND
   SB$ ;

: ENUM-GOOD$ ( -- ptr u8 n )   \ item 14 gap: enum declaration + signature use
   SB-RESET
   s" ENUM eck red green ;ENUM" SB-APPEND
   $0a SB-APPEND-C
   s" : CKT-EN-PASS ( eck -- eck ) ;" SB-APPEND
   SB$ ;

: ENUM-BAD$ ( -- ptr u8 n )    \ duplicate enum variant rejects fail-closed
   SB-RESET
   s" ENUM ebad red red ;ENUM" SB-APPEND
   $0a SB-APPEND-C
   s" : CKT-AFTER-EBAD ( n -- n ) ;" SB-APPEND
   SB$ ;

: PROD-GOOD$ ( -- ptr u8 n )   \ item 15: product declaration + signature use
   SB-RESET
   s" PRODUCT pck 0" SB-APPEND
   $0a SB-APPEND-C
   s"   FIELD x n" SB-APPEND
   $0a SB-APPEND-C
   s"   FIELD y n" SB-APPEND
   $0a SB-APPEND-C
   s" ;PRODUCT" SB-APPEND
   $0a SB-APPEND-C
   s" : CKT-PD-PASS ( pck -- pck ) ;" SB-APPEND
   SB$ ;

: PROD-BAD$ ( -- ptr u8 n )    \ duplicate product field rejects fail-closed
   SB-RESET
   s" PRODUCT pbad 1 FIELD x a FIELD x a ;PRODUCT" SB-APPEND
   $0a SB-APPEND-C
   s" : CKT-AFTER-PBAD ( n -- n ) ;" SB-APPEND
   SB$ ;

: PROD-ALL$SRC ( -- ptr u8 n ) \ all-errors support replay registers products
   SB-RESET
   s" PRODUCT pae 0 FIELD v n ;PRODUCT" SB-APPEND
   $0a SB-APPEND-C
   s" : CKT-PAE-USE ( pae -- pae ) ;" SB-APPEND
   SB$ ;

: VREC-BAD$ ( -- ptr u8 n )
   SB-RESET
   s" VALUE-RECORD point x n y n END-VALUE-RECORD" SB-APPEND
   $0a SB-APPEND-C
   s" VALUE-RECORD rect w n h n END-VALUE-RECORD" SB-APPEND
   $0a SB-APPEND-C
   s" : CKT-BAD-REC ( point -- rect ) ;" SB-APPEND
   SB$ ;

: VREC-PARTIAL$ ( -- ptr u8 n )
   SB-RESET
   s" VALUE-RECORD point x n y n END-VALUE-RECORD" SB-APPEND
   $0a SB-APPEND-C
   s" : CKT-BAD-PARTIAL ( n -- point ) ;" SB-APPEND
   SB$ ;

\ Unterminated block declarations must print their check.f diagnostic before
\ the rc-70 throw; the CHK-THROW message shape dropped the text silently.
: ENUM-NOEND$ ( -- ptr u8 n )
   s" ENUM enoend red green" ;

\ --- unified STRUCTURE declarations. The nominal pass had no arm for these at
\ all: the keyword was skipped, so the family was never registered and any later
\ use of it as a payload type rejected with "unknown payload type". That is the
\ live bug these cover — it made every STRUCTURE-declaring file uncheckable.
: STRUCT-GOOD$ ( -- ptr u8 n )   \ declaration + signature use of the family
   SB-RESET
   s" STRUCTURE sck 0 FIELD lo n FIELD hi n ;STRUCTURE" SB-APPEND
   $0a SB-APPEND-C
   s" : CKT-SD-PASS ( sck -- sck ) ;" SB-APPEND
   SB$ ;

\ The shape that actually broke: a STRUCTURE family named as a later
\ declaration's payload type inside a SUMTYPE.
: STRUCT-PAYLOAD$ ( -- ptr u8 n )
   SB-RESET
   s" STRUCTURE spay 0 FIELD v n ;STRUCTURE" SB-APPEND
   $0a SB-APPEND-C
   s" SUMTYPE sbox 0 VARIANT full spay ;VARIANT VARIANT empty ;VARIANT ;SUMTYPE" SB-APPEND
   $0a SB-APPEND-C
   s" : CKT-SD-BOX ( sbox -- sbox ) ;" SB-APPEND
   SB$ ;

: STRUCT-BAD$ ( -- ptr u8 n )    \ unresolvable field type rejects fail-closed
   SB-RESET
   s" STRUCTURE sbad 0 FIELD one nosuchtype ;STRUCTURE" SB-APPEND
   $0a SB-APPEND-C
   s" : CKT-AFTER-SBAD ( n -- n ) ;" SB-APPEND
   SB$ ;

: STRUCT-NOEND$ ( -- ptr u8 n )
   s" STRUCTURE snoend 0 FIELD x n" ;

\ --- comments inside a declaration body are SYNTAX ERRORS, and the nominal pass
\ must say so rather than launder them.
\
\ The engine reads a declaration body with `parse-name`, which has no comment
\ rule at all, so `\` and `(` inside one are ordinary tokens that hit the name
\ gate. The nominal pass's lexer disagrees: it drops a `\` line comment without
\ emitting any token. Rebuilding the body from those tokens therefore used to
\ hand the replay a body the source never had — measured, `ENUM c9 red \ note`
\ registered cleanly in the nominal pass and only died later in the child run,
\ reporting the raw engine throw (exit 67) instead of the tool's own rendered
\ declaration packet (exit 70). These four pin the repaired behavior: the same
\ named reject the live keyword gives, through check's own error path.
: ENUM-LINE-COMMENT$ ( -- ptr u8 n )
   SB-RESET
   s" ENUM ecmt red \ trailing note" SB-APPEND
   $0a SB-APPEND-C
   s" green ;ENUM" SB-APPEND
   SB$ ;

: ENUM-PAREN-COMMENT$ ( -- ptr u8 n )
   s" ENUM epcmt red ( note ) green ;ENUM" ;

: STRUCT-LINE-COMMENT$ ( -- ptr u8 n )
   SB-RESET
   s" STRUCTURE scmt 0 FIELD a n \ trailing note" SB-APPEND
   $0a SB-APPEND-C
   s" FIELD b n ;STRUCTURE" SB-APPEND
   SB$ ;

: STRUCT-PAREN-COMMENT$ ( -- ptr u8 n )
   s" STRUCTURE spcmt 0 FIELD a n ( note ) FIELD b n ;STRUCTURE" ;

\ --- a declaration longer than a capture buffer must REJECT, never truncate.
\
\ The shared string builder holds 1024 bytes, so this one needs its own. The
\ danger it guards is specific to the replay entries: they parse whatever tokens
\ arrive and have no length gate of their own, so a capture arm that silently
\ dropped an over-cap token would hand them a well-formed and WRONG declaration.
\ Both capture arms therefore raise instead — this tool's CHK-VREC-ROOM answers
\ E-FS-CAPACITY, and verify-source's BODY-APPEND answers the declaration layer's
\ own "declaration too long" (7118), which is also what the engine's TDECL-CAP
\ answers for the same source. test/decl-replay-verify-source.f pins the
\ verify-source half against its exact 8000-byte bound; this pins that the same
\ source is refused loudly, with that named code, all the way out through the
\ real command line.
$4000 constant LONG-CAP
create LONG-BUF LONG-CAP allot
variable LONG-U
variable LONG-I

: LONG-C ( n -- ) {: c:n :}
   LONG-U @ LONG-CAP >= if E-FS-CAPACITY throw then
   c LONG-BUF LONG-U @ + c!
   LONG-U @ 1 + LONG-U ! ;

: LONG-PUT ( ptr u8 n -- ) {: a:ptr u:n :}
   0 LONG-I !
   begin LONG-I @ u < while
      a LONG-I @ + c@ LONG-C
      LONG-I @ 1 + LONG-I !
   repeat ;

\ The bound is on bytes, so each variant is 31 of them: the engine registers an
\ ENUM in time that grows faster than its variant count (one load of 325, 650
\ and 1302 six-byte variants: 0.17, 0.46 and 1.48 s), and 1302 short ones made
\ this the row's slowest case at 2.9 s for a length a fifth as many reach.
: LONG-DIGIT ( n -- ) 48 + LONG-C ;
: LONG-VARIANT ( n -- ) {: i:n :}      \ `vNNNN`, a 25-letter tail, a space
   118 LONG-C
   i 1000 / 10 mod LONG-DIGIT
   i 100 / 10 mod LONG-DIGIT
   i 10 / 10 mod LONG-DIGIT
   i 10 mod LONG-DIGIT
   s" abcdefghijklmnopqrstuvwxy" LONG-PUT
   32 LONG-C ;

variable LONG-J
: LONG-ENUM$ ( n -- ptr u8 n ) {: n:n :}
   0 LONG-U !
   s" ENUM toolong " LONG-PUT
   0 LONG-J !
   begin LONG-J @ n < while
      LONG-J @ LONG-VARIANT
      LONG-J @ 1 + LONG-J !
   repeat
   s" ;ENUM" LONG-PUT
   LONG-BUF LONG-U @ ;

: PROD-NOEND$ ( -- ptr u8 n )
   s" PRODUCT pnoend 0 FIELD x n" ;

: VREC-NOEND$ ( -- ptr u8 n )
   s" VALUE-RECORD vnoend x n" ;

: NOM-SCAN-BODY$ ( -- ptr u8 n )
   SB-RESET
   s" : DEFTYPE ( -- ) ;" SB-APPEND
   $0a SB-APPEND-C
   s" : CKT-NOM-BODY ( -- ) DEFTYPE ( -- ) ;" SB-APPEND
   SB$ ;

\ A source that DECLARES a value nominal with DEFTYPE and USES both its type
\ (in signatures) and its derived converters >NAME / NAME>N (in bodies). The
\ source owns its runtime dependency while preverification recognizes the
\ declaration and converter effects before the child executes it.
: NOM-PREVERIFY$ ( -- ptr u8 n )
   SB-RESET
   s" require lib/type/deftype.f" SB-APPEND $0a SB-APPEND-C
   s" DEFTYPE CKT-WIDGET" SB-APPEND $0a SB-APPEND-C
   s" : CKT-WIDGET-RT ( ckt-widget -- ckt-widget ) ;" SB-APPEND $0a SB-APPEND-C
   s" : CKT-WIDGET-MK ( n -- ckt-widget ) >CKT-WIDGET ;" SB-APPEND $0a SB-APPEND-C
   s" : CKT-WIDGET-UN ( ckt-widget -- n ) CKT-WIDGET>N ;" SB-APPEND
   SB$ ;

\ DEFTYPE in a package may reuse a tail a global or another package's family
\ already has, exactly as NEWTYPE may: the loader admits both declarations
\ below. Only a second declaration of a tail the same package owns is refused.
: NOM-SHADOW$ ( -- ptr u8 n )
   SB-RESET
   s" require lib/type/deftype.f" SB-APPEND $0a SB-APPEND-C
   s" DEFTYPE CKD-GLOBAL" SB-APPEND $0a SB-APPEND-C
   s" package CKDA" SB-APPEND $0a SB-APPEND-C
   s" public" SB-APPEND $0a SB-APPEND-C
   s" DEFTYPE CKD-SHARED" SB-APPEND $0a SB-APPEND-C
   s" ;package" SB-APPEND $0a SB-APPEND-C
   s" package CKDB" SB-APPEND $0a SB-APPEND-C
   s" DEFTYPE CKD-SHARED" SB-APPEND $0a SB-APPEND-C
   s" DEFTYPE CKD-GLOBAL" SB-APPEND $0a SB-APPEND-C
   s" : CKDB-MK ( n n -- ckd-shared ckd-global ) >CKD-GLOBAL swap >CKD-SHARED swap ;" SB-APPEND $0a SB-APPEND-C
   s" ;package" SB-APPEND
   SB$ ;

: NOM-DUP$ ( -- ptr u8 n )
   SB-RESET
   s" require lib/type/deftype.f" SB-APPEND $0a SB-APPEND-C
   s" package CKDC" SB-APPEND $0a SB-APPEND-C
   s" DEFTYPE CKD-TWICE" SB-APPEND $0a SB-APPEND-C
   s" DEFTYPE CKD-TWICE" SB-APPEND $0a SB-APPEND-C
   s" ;package" SB-APPEND
   SB$ ;

\ A DEFTYPE tail an effect reads as something other than the family is refused
\ at the declaration in every scope: a bare `ptr` is the pointer constructor, so
\ a package's own `ptr` would leave its converter `( n -- ptr )` without a
\ pointee. Shadowing another package's family stays legal: `side` is
\ IR-SCHEMA's public enum, baked into the engine, and `( n -- side )` below can
\ only certify if it names the package's own type.
: NOM-PKG-DEF$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   SB-RESET
   s" require lib/type/deftype.f" SB-APPEND $0a SB-APPEND-C
   s" package CKDP" SB-APPEND $0a SB-APPEND-C
   s" DEFTYPE " SB-APPEND a u SB-APPEND $0a SB-APPEND-C
   s" ;package" SB-APPEND
   SB$ ;

: NOM-TOP-DEF$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   SB-RESET
   s" require lib/type/deftype.f" SB-APPEND $0a SB-APPEND-C
   s" DEFTYPE " SB-APPEND a u SB-APPEND
   SB$ ;

: NOM-SIDE$ ( -- ptr u8 n )
   SB-RESET
   s" require lib/type/deftype.f" SB-APPEND $0a SB-APPEND-C
   s" package CKDS" SB-APPEND $0a SB-APPEND-C
   s" DEFTYPE SIDE" SB-APPEND $0a SB-APPEND-C
   s" : CKDS-MK ( n -- side ) >SIDE ;" SB-APPEND $0a SB-APPEND-C
   s" ;package" SB-APPEND
   SB$ ;

\ ENUM and STRUCTURE refuse the same `ptr` tail through their own name gates:
\ inside a package, where shadowing a global family is otherwise legal, and at
\ top level, where the reserved name answers ahead of the duplicate family.
: DECL-PKG$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   SB-RESET
   s" package CKPT" SB-APPEND $0a SB-APPEND-C
   a u SB-APPEND $0a SB-APPEND-C
   s" ;package" SB-APPEND
   SB$ ;

\ An effect reads a value record's name as that record ahead of any family
\ (checker.f PSTACK), so a family of that tail could never be named: ENUM and
\ STRUCTURE refuse it in both scopes, as SUMTYPE does. The record is global, so
\ the package source declares it outside the package.
: VREC-LINE ( -- )
   s" VALUE-RECORD ckvr x n END-VALUE-RECORD" SB-APPEND $0a SB-APPEND-C ;

: VREC-TOP$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   SB-RESET
   VREC-LINE
   a u SB-APPEND
   SB$ ;

: VREC-PKG$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   SB-RESET
   VREC-LINE
   s" package CKVP" SB-APPEND $0a SB-APPEND-C
   a u SB-APPEND $0a SB-APPEND-C
   s" ;package" SB-APPEND
   SB$ ;

\ An effect spells a DEFLINEAR or VALUE-RECORD type exactly as it was declared,
\ so `CELL` and `PTR` are types of their own beside `cell` and `ptr`. A single
\ upper-case letter at the head of a stack is a row variable, and `--`, `[` and
\ `)` are effect syntax, so no effect could name such a type. A `(`, `"` or `\`
\ can open a comment, a string or a line comment where source names the type
\ again (checker.f TYPE-BAD-BYTE?). The loader and the check tool must refuse
\ exactly those names; each source below declares one and uses it.
: NOM-LIN$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   SB-RESET
   s" DEFLINEAR " SB-APPEND a u SB-APPEND $0a SB-APPEND-C
   s" : CKN-USE ( n " SB-APPEND a u SB-APPEND
   s"  -- " SB-APPEND a u SB-APPEND s"  n ) swap ;" SB-APPEND
   SB$ ;

: NOM-REC$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   SB-RESET
   s" VALUE-RECORD " SB-APPEND a u SB-APPEND
   s"  x n END-VALUE-RECORD" SB-APPEND $0a SB-APPEND-C
   s" : CKN-USE ( " SB-APPEND a u SB-APPEND
   s"  -- " SB-APPEND a u SB-APPEND s"  ) ;" SB-APPEND
   SB$ ;

\ The name `s"`, with a comment line that closes the string the check tool's
\ lexer opens at it, so that tool reaches the name instead of an unterminated
\ string.
: NOM-LIN-QUOTE$ ( -- ptr u8 n )
   SB-RESET
   s" DEFLINEAR s" SB-APPEND $22 SB-APPEND-C $0a SB-APPEND-C
   s" \ " SB-APPEND $22 SB-APPEND-C $0a SB-APPEND-C
   s" : CKN-USE ( n s" SB-APPEND $22 SB-APPEND-C
   s"  -- s" SB-APPEND $22 SB-APPEND-C s"  n ) swap ;" SB-APPEND
   SB$ ;

\ A family claims its tail in every scope that reads it: the owning package
\ reads its private row ahead of any other type, and each of two packages that
\ share a public tail reads its own row, though the unqualified fallback finds
\ the pair ambiguous. A linear or value record of that name, declared anywhere,
\ would lose to the family there, so its declaration is refused. Each source
\ declares the family, then the nominal at top level, then drops a value of
\ that name inside package CKFP: the family reading admits the drop, which
\ loses a linear value that must move exactly once.
: FAM-PRIV-HEAD ( -- )
   SB-RESET
   s" require lib/type/deftype.f" SB-APPEND $0a SB-APPEND-C
   s" package CKFP" SB-APPEND $0a SB-APPEND-C
   s" DEFTYPE CKF-TAIL" SB-APPEND $0a SB-APPEND-C
   s" ;package" SB-APPEND $0a SB-APPEND-C ;

: FAM-SHARED-HEAD ( -- )
   SB-RESET
   s" require lib/type/deftype.f" SB-APPEND $0a SB-APPEND-C
   s" package CKFP" SB-APPEND $0a SB-APPEND-C
   s" public" SB-APPEND $0a SB-APPEND-C
   s" DEFTYPE CKF-TAIL" SB-APPEND $0a SB-APPEND-C
   s" ;package" SB-APPEND $0a SB-APPEND-C
   s" package CKFQ" SB-APPEND $0a SB-APPEND-C
   s" public" SB-APPEND $0a SB-APPEND-C
   s" DEFTYPE CKF-TAIL" SB-APPEND $0a SB-APPEND-C
   s" ;package" SB-APPEND $0a SB-APPEND-C ;

: FAM-LIN ( ptr u8 n -- ) {: a:ptr u:n :}
   s" DEFLINEAR " SB-APPEND a u SB-APPEND $0a SB-APPEND-C ;

: FAM-REC ( ptr u8 n -- ) {: a:ptr u:n :}
   s" VALUE-RECORD " SB-APPEND a u SB-APPEND
   s"  x n END-VALUE-RECORD" SB-APPEND $0a SB-APPEND-C ;

: FAM-DROP$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   s" package CKFP" SB-APPEND $0a SB-APPEND-C
   s" : CKF-DROP ( " SB-APPEND a u SB-APPEND s"  -- ) drop ;" SB-APPEND $0a SB-APPEND-C
   s" ;package" SB-APPEND
   SB$ ;

\ Nominal-declarer sources for the check CLI's package-scoping contract. These
\ feed the real child-engine path and are checked end to end, so they use
\ DEFLINEAR: its interpret word is baked into the engine, so it runs in the
\ child with no require, while a DEFTYPE source would need
\ `require lib/type/deftype.f` for the child to define the declarer. The
\ check tool's static scanner and preverify path understand both DEFLINEAR and
\ (since stage B) the DEFTYPE family surface. A DEFLINEAR type is one linear
\ cell whose value moves exactly once, so bodies pass it through by identity
\ rather than dropping or binding it.
: LINEAR-GOOD$ ( -- ptr u8 n )
   SB-RESET
   s" package CKL-PRIV" SB-APPEND $0a SB-APPEND-C
   s" DEFLINEAR ckl-id" SB-APPEND $0a SB-APPEND-C
   s" private" SB-APPEND $0a SB-APPEND-C
   s" : CKL-PRIV-PASS ( ckl-id -- ckl-id ) ;" SB-APPEND $0a SB-APPEND-C
   s" public" SB-APPEND $0a SB-APPEND-C
   s" : CKL-ROUNDTRIP ( ckl-id -- ckl-id ) CKL-PRIV-PASS ;" SB-APPEND $0a SB-APPEND-C
   s" ;package" SB-APPEND $0a SB-APPEND-C
   s" package CKL-PRIV" SB-APPEND $0a SB-APPEND-C
   s" : CKL-REOPEN ( ckl-id -- ckl-id ) CKL-PRIV-PASS ;" SB-APPEND $0a SB-APPEND-C
   s" ;package" SB-APPEND $0a SB-APPEND-C
   s" : CKL-USE ( ckl-id -- ckl-id ) CKL-PRIV:CKL-ROUNDTRIP ;" SB-APPEND
   SB$ ;

\ A package-private word is not visible unqualified in another package: the
\ child engine hard-dies E-UNDEFINED and the check CLI reports it faithfully.
: LINEAR-CROSS$ ( -- ptr u8 n )
   SB-RESET
   s" package CKL-A" SB-APPEND $0a SB-APPEND-C
   s" DEFLINEAR ckl-a-id" SB-APPEND $0a SB-APPEND-C
   s" private" SB-APPEND $0a SB-APPEND-C
   s" : CKL-A-PRIV ( ckl-a-id -- ckl-a-id ) ;" SB-APPEND $0a SB-APPEND-C
   s" ;package" SB-APPEND $0a SB-APPEND-C
   s" package CKL-B" SB-APPEND $0a SB-APPEND-C
   s" : CROSS ( ckl-a-id -- ckl-a-id ) CKL-A-PRIV ;" SB-APPEND $0a SB-APPEND-C
   s" ;package" SB-APPEND
   SB$ ;

\ Same encapsulation seen from global scope: the package-private word does not
\ leak out, so the leaking reference is E-UNDEFINED.
: LINEAR-GLOBAL$ ( -- ptr u8 n )
   SB-RESET
   s" package CKL-HIDDEN" SB-APPEND $0a SB-APPEND-C
   s" DEFLINEAR ckl-hidden-id" SB-APPEND $0a SB-APPEND-C
   s" private" SB-APPEND $0a SB-APPEND-C
   s" : CKL-HIDDEN-PRIV ( ckl-hidden-id -- ckl-hidden-id ) ;" SB-APPEND $0a SB-APPEND-C
   s" ;package" SB-APPEND $0a SB-APPEND-C
   s" : GLOBAL-LEAK ( ckl-hidden-id -- ckl-hidden-id ) CKL-HIDDEN-PRIV ;" SB-APPEND
   SB$ ;

\ Two nominals declared in one package are distinct types: producing the left
\ one where the signature demands the right one is an E-MISMATCH.
: LINEAR-DISTINCT$ ( -- ptr u8 n )
   SB-RESET
   s" package CKL-DIST" SB-APPEND $0a SB-APPEND-C
   s" DEFLINEAR ckl-left-id" SB-APPEND $0a SB-APPEND-C
   s" DEFLINEAR ckl-right-id" SB-APPEND $0a SB-APPEND-C
   s" : WRONG ( ckl-left-id -- ckl-right-id ) ;" SB-APPEND $0a SB-APPEND-C
   s" ;package" SB-APPEND
   SB$ ;

\ Scanner package-block support (dot habu-tools-check-scanner-685b735e):
\ CHK-NOM-STEP replays package/public/private/;package at the checker level,
\ so family registrations land in the declaring package and the check CLI can
\ reach the foreign qualified pkg:tail contract end to end.
: FAM-GOOD$ ( -- ptr u8 n )        \ foreign public family, qualified good use
   SB-RESET
   s" package CKFA" SB-APPEND $0a SB-APPEND-C
   s" public" SB-APPEND $0a SB-APPEND-C
   s" SUMTYPE ckffam 0 VARIANT keep n ;VARIANT ;SUMTYPE" SB-APPEND $0a SB-APPEND-C
   s" ;package" SB-APPEND $0a SB-APPEND-C
   s" : CKF-PASS ( ckfa:ckffam -- ckfa:ckffam ) ;" SB-APPEND
   SB$ ;

: FAM-TWO$ ( -- ptr u8 n )         \ same tail in two packages: no spurious dup reject
   SB-RESET
   s" package CKFT1" SB-APPEND $0a SB-APPEND-C
   s" public" SB-APPEND $0a SB-APPEND-C
   s" SUMTYPE ckfsame 0 VARIANT keep n ;VARIANT ;SUMTYPE" SB-APPEND $0a SB-APPEND-C
   s" ;package" SB-APPEND $0a SB-APPEND-C
   s" package CKFT2" SB-APPEND $0a SB-APPEND-C
   s" public" SB-APPEND $0a SB-APPEND-C
   s" SUMTYPE ckfsame 0 VARIANT keep n ;VARIANT ;SUMTYPE" SB-APPEND $0a SB-APPEND-C
   s" ;package" SB-APPEND $0a SB-APPEND-C
   s" : CKF-TWO ( ckft1:ckfsame -- ckft1:ckfsame ) ;" SB-APPEND
   SB$ ;

: FAM-BOGUS$ ( -- ptr u8 n )       \ family qualified into a never-declared package
   s" : CKF-BOGUS ( ckfno:ckffam -- ckfno:ckffam ) ;" ;

: FAM-JSON$ ( -- ptr u8 n )        \ foreign family mismatch: qualified JSON family pin
   SB-RESET
   s" package CKFJ" SB-APPEND $0a SB-APPEND-C
   s" public" SB-APPEND $0a SB-APPEND-C
   s" SUMTYPE ckfjfam 0 VARIANT keep n ;VARIANT ;SUMTYPE" SB-APPEND $0a SB-APPEND-C
   s" ;package" SB-APPEND $0a SB-APPEND-C
   s" : CKF-JBAD ( n -- ckfj:ckfjfam ) ;" SB-APPEND
   SB$ ;

: FAM-ESC$ ( -- ptr u8 n )         \ a family of package `\`: its spelling needs JSON escapes
   SB-RESET
   s" package \" SB-APPEND $0a SB-APPEND-C
   s" public" SB-APPEND $0a SB-APPEND-C
   s" SUMTYPE ckfefam 0 VARIANT keep n ;VARIANT ;SUMTYPE" SB-APPEND $0a SB-APPEND-C
   s" ;package" SB-APPEND $0a SB-APPEND-C
   s" : CKF-EBAD ( n -- \:ckfefam ) ;" SB-APPEND
   SB$ ;

: FAM-PRIV$ ( -- ptr u8 n )        \ private family: qualified lookup is public-only
   SB-RESET
   s" package CKFP" SB-APPEND $0a SB-APPEND-C
   s" SUMTYPE ckfpfam 0 VARIANT keep n ;VARIANT ;SUMTYPE" SB-APPEND $0a SB-APPEND-C
   s" ;package" SB-APPEND $0a SB-APPEND-C
   s" : CKF-PRIV ( ckfp:ckfpfam -- ckfp:ckfpfam ) ;" SB-APPEND
   SB$ ;

: RESERVED-LIST-RUN ( -- n n n )
   LIST$ RESERVED$ WRITE-ALL
   RESERVED-NAME-LINT:RESET
   CAP-ERR BUF-CAP LINT-OUT-BUFFER!
   LIST$ s" <source-list>" RESERVED-NAME-LINT:FILE-AS
   [: RESERVED-NAME-LINT:FINISH ;] catch {: rc:n :}
   LINT-OUT$ nip
   LINT-OUT-BUFFER-OFF
   0 swap rc ;

: DIE$ ( -- ptr u8 n )
   SB-RESET
   s" : CKT-BYE ( -- ) s" SB-APPEND
   $22 SB-APPEND-C
   s"  bye" SB-APPEND
   $22 SB-APPEND-C
   s"  5 die ;" SB-APPEND
   $0a SB-APPEND-C
   s" CKT-BYE" SB-APPEND
   SB$ ;

: UNTERM-SDQ$ ( -- ptr u8 n )
   SB-RESET
   s" : CKT-UNTERM-SDQ ( -- ptr u8 n ) s" SB-APPEND
   $22 SB-APPEND-C
   s"  nope ;" SB-APPEND
   SB$ ;

: UNTERM-ESC$ ( -- ptr u8 n )
   SB-RESET
   s" : CKT-UNTERM-ESC ( -- ptr u8 n ) s\" SB-APPEND
   $22 SB-APPEND-C
   s"  nope ;" SB-APPEND
   SB$ ;

: PREPARE ( -- )
   CLEANUP-RESET
   s" habu-check-test" HB-TMP-MKDIR TMP-ROOT TMP-ROOT-U PATH-COPY!
   ROOT$ CLEANUP-TREE+
   ROOT$ s" bad.f" BAD-PATH JOIN-PATH BAD-U !
   ROOT$ s" direct-source.f" DIRECT-PATH JOIN-PATH DIRECT-U !
   ROOT$ s" local-test.f" LIST-PATH JOIN-PATH LIST-U !
   ROOT$ s" inc-dep.f" INC-DEP-PATH JOIN-PATH INC-DEP-U !
   ROOT$ s" inc-entry.f" INC-ENTRY-PATH JOIN-PATH INC-ENTRY-U !
   ROOT$ s" list-sup.f" SUP-PATH JOIN-PATH SUP-U !
   ROOT$ s" list-use.f" USE-PATH JOIN-PATH USE-U !
   ROOT$ s" cyc-root.f" CYC-ROOT-PATH JOIN-PATH CYC-ROOT-U !
   ROOT$ s" cyc-host.f" CYC-HOST-PATH JOIN-PATH CYC-HOST-U !
   ROOT$ s" cyc-entry.f" CYC-ENTRY-PATH JOIN-PATH CYC-ENTRY-U !
   ROOT$ s" env-probe.f" ENV-PROBE-PATH JOIN-PATH ENV-PROBE-U !
   BAD$ BAD$SRC WRITE-ALL ;

: TEST-GOOD ( -- )
   [: GOOD$ VERIFY:SOURCE-BUF ;] catch 0 T= ;

: PRINT-PARITY$ ( -- ptr u8 n )
   s" .( loud ) : CKP ( -- ) ;" ;

: TEST-PRINT-PARITY ( -- )
   [: PRINT-PARITY$ VERIFY:SOURCE-BUF ;] catch 0 T=
   PRINT-PARITY$ LINT-LEX:SOURCE
   LINT-LEX:ERROR? 0= TTRUE
   LINT-LEX:COUNT 5 T= ;

: TEST-PRELUDE-HOOK ( -- )
   s" : CKT-PRELUDE-GOOD ( i64 -- i64 ) dup * ;"
   PRELUDE-EVAL$ DIRECT-STDIN 0 T=
   {: outu:n erru:n :}
   outu 0 T=
   erru 0 T=
   s" : CKT-PRELUDE-BAD ( i64 -- i64 ) dup ;"
   PRELUDE-EVAL$ DIRECT-STDIN 70 T=
   {: bad-outu:n bad-erru:n :}
   bad-outu 0 T=
   CAP-ERR bad-erru s" hook: non-certified definition" CONTAINS? TTRUE
   CAP-ERR bad-erru s" ckt-prelude-bad" CONTAINS? TTRUE
   CAP-ERR bad-erru s" at 'dup'" CONTAINS? TTRUE ;

: LBUF-GOOD$ ( -- ptr u8 n )
   s" SUMTYPE cklb 1 VARIANT value a ;VARIANT ;SUMTYPE 2 LAYOUT-BUFFER CKLB-BUF cklb<n>" ;

: LBUF-OLD$ ( -- ptr u8 n )
   s" LAYOUT-BUFFER CKLB-OLD cklb<n>" ;

: TEST-LAYOUT-BUFFER ( -- )
   [: LBUF-GOOD$ VERIFY:SOURCE-BUF ;] catch 0 T=
   [: LBUF-OLD$ VERIFY:SOURCE-BUF ;] catch 70 T= ;

\ A buffer count is whatever the line leaves on the interpret stack, so a
\ constant's name or an expression sizes a buffer as a literal does, and the
\ accessor it declares is held to the same effect. `4 constant N  N
\ TYPED-BUFFER B n` is the line lib/content-key.f sizes its fold table with.
\ A count that cannot size a buffer is refused either way: a literal by the
\ source pre-pass, which can read its value, and a computed one by the definer
\ when the run reaches it.
: COUNT-NAMED$ ( -- ptr u8 n )
   s" 4 constant CKT-CAP CKT-CAP TYPED-BUFFER CKT-ROWS n : CKT-ROW ( n -- ptr n ) CKT-ROWS ;" ;

: COUNT-EXPR$ ( -- ptr u8 n )
   s" 2 constant CKT-CAP CKT-CAP 2 * TYPED-BUFFER CKT-ROWS n : CKT-ROW ( n -- ptr n ) CKT-ROWS ;" ;

: COUNT-LAYOUT$ ( -- ptr u8 n )
   s" SUMTYPE cklb 1 VARIANT value a ;VARIANT ;SUMTYPE 2 constant CKT-CAP CKT-CAP LAYOUT-BUFFER CKLB-BUF cklb<n> : CKT-ROW ( n -- ptr cklb<n> ) CKLB-BUF ;" ;

: COUNT-NAMED-MISUSE$ ( -- ptr u8 n )
   s" 4 constant CKT-CAP CKT-CAP TYPED-BUFFER CKT-ROWS n : CKT-BAD ( -- ptr n ) CKT-ROWS ;" ;

: COUNT-ZERO$ ( -- ptr u8 n )
   s" 0 TYPED-BUFFER CKT-ROWS n" ;

: COUNT-NAMED-ZERO$ ( -- ptr u8 n )
   s" 0 constant CKT-CAP CKT-CAP TYPED-BUFFER CKT-ROWS n" ;

: EXPECT-ACCEPTED ( n n n -- )
   0 T= {: outu:n erru:n :}
   outu 0 T=
   erru 0 T= ;

: TEST-BUFFER-COUNT ( -- )
   COUNT-NAMED$ DIRECT-STDIN EXPECT-ACCEPTED
   COUNT-EXPR$ DIRECT-STDIN EXPECT-ACCEPTED
   COUNT-LAYOUT$ DIRECT-STDIN EXPECT-ACCEPTED
   COUNT-NAMED-MISUSE$ DIRECT-JSON-STDIN 70 T=
   {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru s" E-INPUT-UNDERFLOW" CONTAINS? TTRUE
   COUNT-ZERO$ DIRECT-STDIN 70 T=
   {: outu2:n erru2:n :}
   outu2 0 T=
   CAP-ERR erru2 s" preverify failed" CONTAINS? TTRUE
   CAP-ERR erru2 s\" \"reason\":\"count outside the buffer's extent\"" CONTAINS? TTRUE
   COUNT-NAMED-ZERO$ DIRECT-STDIN 67 T=
   {: outu3:n erru3:n :}
   outu3 0 T=
   CAP-ERR erru3 s" preverify failed" CONTAINS? TFALSE
   CAP-ERR erru3 s" 7121" CONTAINS? TTRUE ;

\ A parsing keyword's operand is data to every stage of check.f, whatever it
\ spells: `'` and `char` at top level, `[char]` in a body, matched case-folded
\ as the engine and the checker match them. After `CHAR 0` the count is the
\ keyword's value, left to the run, and the line after such a keyword is still
\ read: a literal bad count and an undefined name are refused as before.
: OPERAND-COLON$ ( -- ptr u8 n )
   s" char : constant CKT-COLON : CKT-C ( -- n ) CKT-COLON ;" ;

: OPERAND-TICK$ ( -- ptr u8 n )
   s" ' TYPED-BUFFER constant CKT-TB" ;

: OPERAND-COUNT$ ( -- ptr u8 n )
   s" CHAR 0 TYPED-BUFFER CKT-ROWS n : CKT-ROW ( n -- ptr n ) CKT-ROWS ;" ;

: OPERAND-BODY$ ( -- ptr u8 n )
   s" : CKT-P ( -- n ) [char] ( ; : CKT-B ( -- n ) [CHAR] \ ; : CKT-Q ( -- n ) [char] : ;" ;

: OPERAND-BAD-COUNT$ ( -- ptr u8 n )
   s" char : constant CKT-COLON 0 TYPED-BUFFER CKT-ROWS n" ;

: OPERAND-UNDEFINED$ ( -- ptr u8 n )
   s" char : constant CKT-COLON : CKT-F ( -- n ) CKT-NOPE ;" ;

: TEST-PARSED-OPERAND ( -- )
   OPERAND-COLON$ DIRECT-STDIN EXPECT-ACCEPTED
   OPERAND-TICK$ DIRECT-STDIN EXPECT-ACCEPTED
   OPERAND-COUNT$ DIRECT-STDIN EXPECT-ACCEPTED
   OPERAND-BODY$ DIRECT-STDIN EXPECT-ACCEPTED
   OPERAND-BAD-COUNT$ DIRECT-STDIN 70 T=
   {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru s" preverify failed" CONTAINS? TTRUE
   CAP-ERR erru s\" \"reason\":\"count outside the buffer's extent\"" CONTAINS? TTRUE
   OPERAND-UNDEFINED$ DIRECT-JSON-STDIN 70 T=
   {: outu2:n erru2:n :}
   outu2 0 T=
   CAP-ERR erru2 s" E-UNDEFINED" CONTAINS? TTRUE ;

\ A word whose body reaches the loader's INCLUDE-EVALUATE defines words when it
\ runs, and the source pre-pass never sees their text: CKR-MAKE renders
\ CKR-SEVEN. A definition naming such a product is left to the run, which
\ certifies it, but only after the rendering statement and only in the wordlist
\ that statement ran in; anything else unknown is still refused by the
\ pre-pass. `FUNCTION:` is resident here (lib/fs-mutate.f requires
\ lib/ffi-abi.f), and TASK:+USER is the renderer lib/crypto/evp.f uses.
: CKR-DEF+ ( -- )
   s" : CKR-MAKE ( -- ) s" SB-APPEND $22 SB-APPEND-C
   s"  : CKR-SEVEN ( -- n ) 7 ;" SB-APPEND $22 SB-APPEND-C
   s"  INCLUDE-EVALUATE ; " SB-APPEND ;

: CKR-FIXTURE+ ( -- )
   CKR-DEF+ s" CKR-MAKE " SB-APPEND ;

: CKR-AFTER$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   SB-RESET CKR-FIXTURE+ a u SB-APPEND SB$ ;

: RENDER-USE$ ( -- ptr u8 n )
   s" : CKR-USE ( -- n ) CKR-SEVEN ;" CKR-AFTER$ ;

: RENDER-FFI$ ( -- ptr u8 n )
   s" require lib/ffi-abi.f PROCESS-SYMBOLS FUNCTION: CKR-PID getpid ( -- i32 ) ;FUNCTION : CKR-PID-N ( -- n ) CKR-PID ;" ;

: RENDER-TASK$ ( -- ptr u8 n )
   s" require lib/task.f TASK:#USER 8 TASK:+USER CKR-SLOT drop : CKR-SLOT-P ( -- ptr n ) CKR-SLOT ;" ;

\ A slot made inside a package lands in the section the statement ran in, like
\ lib/crypto/evp.f EVP-STORAGE (private) and lib/net/http-arena.f MY-SLOT.
: CKR-TASK-PKG$ ( ptr u8 n ptr u8 n -- ptr u8 n ) {: sec:ptr secu:n use:ptr useu:n :}
   SB-RESET
   s" require lib/task.f package CKR-T " SB-APPEND sec secu SB-APPEND
   s"  TASK:#USER 8 TASK:+USER CKR-SLOT drop " SB-APPEND
   use useu SB-APPEND SB$ ;

: RENDER-TASK-PRIVATE$ ( -- ptr u8 n )
   s" private" s" : CKR-SLOT-P ( -- ptr n ) CKR-SLOT ; ;package" CKR-TASK-PKG$ ;

: RENDER-TASK-PUBLIC$ ( -- ptr u8 n )
   s" public" s" ;package : CKR-SLOT-P ( -- ptr n ) CKR-T:CKR-SLOT ;" CKR-TASK-PKG$ ;

: RENDER-TASK-OUTSIDE$ ( -- ptr u8 n )
   s" private" s" ;package : CKR-SLOT-P ( -- ptr n ) CKR-T:CKR-SLOT ;" CKR-TASK-PKG$ ;

: RENDER-TASK-BARE$ ( -- ptr u8 n )
   s" private" s" ;package : CKR-SLOT-P ( -- ptr n ) CKR-SLOT ;" CKR-TASK-PKG$ ;

: RENDER-MISUSE$ ( -- ptr u8 n )
   s" : CKR-BAD ( -- ) CKR-SEVEN ;" CKR-AFTER$ ;

: RENDER-TYPO$ ( -- ptr u8 n )
   s" : CKR-TYPO ( -- n ) CKR-SEVN ;" CKR-AFTER$ ;

: RENDER-EARLY$ ( -- ptr u8 n )
   SB-RESET CKR-DEF+
   s" : CKR-EARLY ( -- n ) CKR-SEVEN ; CKR-MAKE" SB-APPEND SB$ ;

: RENDER-OTHER-PKG$ ( -- ptr u8 n )
   SB-RESET s" package CKR-A " SB-APPEND CKR-FIXTURE+
   s" ;package package CKR-B : CKR-TYPO ( -- n ) CKR-SEVEN ; ;package" SB-APPEND SB$ ;

: RENDER-PRIVATE$ ( -- ptr u8 n )
   SB-RESET s" package CKR-A " SB-APPEND CKR-FIXTURE+
   s" ;package : CKR-Q ( -- n ) CKR-A:CKR-SEVEN ;" SB-APPEND SB$ ;

: RENDER-PUBLIC$ ( -- ptr u8 n )
   SB-RESET s" package CKR-A public " SB-APPEND CKR-FIXTURE+
   s" ;package : CKR-Q ( -- n ) CKR-A:CKR-SEVEN ;" SB-APPEND SB$ ;

: RENDER-CALLER$ ( -- ptr u8 n )
   s" : CKR-USE ( -- n ) CKR-SEVEN ; : CKR-CALLER ( -- ) CKR-USE ;" CKR-AFTER$ ;

: EXPECT-PREVERIFY-REFUSED ( n n n -- )
   70 T= {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru s" preverify failed" CONTAINS? TTRUE ;

\ Refused by the pre-pass as undefined, naming the token.
: EXPECT-PREVERIFY-UNDEFINED ( n n n ptr u8 n -- ) {: outu:n erru:n rc:n tok:ptr toku:n :}
   outu erru rc EXPECT-PREVERIFY-REFUSED
   CAP-ERR erru s" E-UNDEFINED" CONTAINS? TTRUE
   CAP-ERR erru tok toku CONTAINS? TTRUE ;

\ Refused by the pre-pass in the named definition: a diagnostic's "word" is the
\ definition's name, folded to lower case.
: EXPECT-PREVERIFY-IN ( n n n ptr u8 n -- ) {: outu:n erru:n rc:n word:ptr wordu:n :}
   outu erru rc EXPECT-PREVERIFY-REFUSED
   CAP-ERR erru word wordu CONTAINS? TTRUE ;

\ Refused by the run rather than the pre-pass: the run's diagnostic names it.
: EXPECT-RUN-REFUSED ( n n n ptr u8 n -- ) {: outu:n erru:n rc:n want:ptr wantu:n :}
   rc 70 T=
   outu 0 T=
   CAP-ERR erru want wantu CONTAINS? TTRUE
   CAP-ERR erru s" preverify failed" CONTAINS? TFALSE ;

: TEST-RENDERED-PRODUCT ( -- )
   RENDER-USE$ DIRECT-STDIN EXPECT-ACCEPTED
   RENDER-FFI$ DIRECT-STDIN EXPECT-ACCEPTED
   RENDER-TASK$ DIRECT-STDIN EXPECT-ACCEPTED
   RENDER-TASK-PRIVATE$ DIRECT-STDIN EXPECT-ACCEPTED
   RENDER-TASK-PUBLIC$ DIRECT-STDIN EXPECT-ACCEPTED
   RENDER-TASK-OUTSIDE$ DIRECT-STDIN s" CKR-T:CKR-SLOT" EXPECT-PREVERIFY-UNDEFINED
   RENDER-TASK-BARE$ DIRECT-STDIN s" CKR-SLOT" EXPECT-PREVERIFY-UNDEFINED
   RENDER-PUBLIC$ DIRECT-STDIN EXPECT-ACCEPTED
   RENDER-USE$ DIRECT-ALL-STDIN EXPECT-ACCEPTED
   RENDER-MISUSE$ DIRECT-STDIN s" hook: non-certified definition" EXPECT-RUN-REFUSED
   RENDER-MISUSE$ DIRECT-JSON-STDIN s" E-MISMATCH" EXPECT-RUN-REFUSED
   RENDER-TYPO$ DIRECT-STDIN {: outt:n errt:n rct:n :}
   outt errt rct s" CKR-SEVN" EXPECT-RUN-REFUSED
   CAP-ERR errt s" E-UNDEFINED" CONTAINS? TTRUE
   RENDER-EARLY$ DIRECT-STDIN EXPECT-PREVERIFY-REFUSED
   RENDER-OTHER-PKG$ DIRECT-STDIN EXPECT-PREVERIFY-REFUSED
   RENDER-PRIVATE$ DIRECT-STDIN EXPECT-PREVERIFY-REFUSED
   RENDER-CALLER$ DIRECT-STDIN s" ckr-caller" EXPECT-PREVERIFY-IN
   RENDER-CALLER$ DIRECT-JSON-STDIN 70 T= {: outc:n errc:n :}
   outc 0 T=
   CAP-ERR errc s" E-MISMATCH" CONTAINS? TTRUE
   CAP-ERR errc s" ckr-caller" CONTAINS? TTRUE ;

\ A definer's created effect is its clause's declaration, so CKR-X is learned
\ when the clause or the definer's own body names a product only the run can
\ see, and a clause that misuses one is still refused, by the run.
: CKR-DEFR+ ( ptr u8 n ptr u8 n -- ) {: pre:ptr preu:n clause:ptr clauseu:n :}
   s" package CKR-A public " SB-APPEND CKR-FIXTURE+
   s" ;package : CKR-DEFR ( n -- ) " SB-APPEND pre preu SB-APPEND
   s"  create , does> ( -- n ) @ " SB-APPEND clause clauseu SB-APPEND
   s"  ; 5 CKR-DEFR CKR-X" SB-APPEND ;

: CKR-DOES$ ( ptr u8 n ptr u8 n -- ptr u8 n )
   SB-RESET CKR-DEFR+ s"  : CKR-U ( -- n ) CKR-X ;" SB-APPEND SB$ ;

: RENDER-DOES-CLAUSE$ ( -- ptr u8 n )
   s" " s" CKR-A:CKR-SEVEN +" CKR-DOES$ ;

: RENDER-DOES-DEFINER$ ( -- ptr u8 n )
   s" CKR-A:CKR-SEVEN +" s" " CKR-DOES$ ;

: RENDER-DOES-BAD$ ( -- ptr u8 n )
   s" " s" CKR-A:CKR-SEVEN + +" CKR-DOES$ ;

: TEST-RENDERED-DOES ( -- )
   RENDER-DOES-CLAUSE$ DIRECT-STDIN EXPECT-ACCEPTED
   RENDER-DOES-DEFINER$ DIRECT-STDIN EXPECT-ACCEPTED
   RENDER-DOES-BAD$ DIRECT-STDIN s" does>" EXPECT-RUN-REFUSED ;

\ A clause the pre-pass refuses is reported as a refused body is: by the name of
\ the record it would publish, the definer's with `;does`, the code, the token
\ and the place, in every output mode, whether the definer's own body certified
\ or was left to the run, and whether the created word is used or not. Each
\ check is a child process, as the command line runs it: a second in-process
\ check of the same refused source that renders a package's words reports
\ them undefined instead of leaving them to the run.
: CLAUSE-PLAIN$ ( -- ptr u8 n )
   s" : CKR-DEFR ( n -- ) create , does> ( -- n ) @ dup ; 5 CKR-DEFR CKR-X : CKR-U ( -- n ) CKR-X ;" ;

: CLAUSE-RENDERED-USED$ ( -- ptr u8 n )
   s" CKR-A:CKR-SEVEN +" s" dup" CKR-DOES$ ;

: CLAUSE-RENDERED-UNUSED$ ( -- ptr u8 n )
   SB-RESET s" CKR-A:CKR-SEVEN +" s" dup" CKR-DEFR+ SB$ ;

: CLAUSE-PLAIN-AT$ ( -- ptr u8 n )
   s\" \"line\":1,\"column\":47,\"byte_start\":46,\"byte_end\":49" ;

: CLAUSE-RENDERED-AT$ ( -- ptr u8 n )
   s\" \"line\":1,\"column\":170,\"byte_start\":169,\"byte_end\":172" ;

: CLI-FLAG-STDIN ( ptr u8 n ptr u8 n -- n n n ) {: src:ptr srcu:n flag:ptr flagu:n :}
   CHECK-ARGV-START
   flag flagu CHECK-ARG+
   src srcu CHECK-STDIN-CAPTURE ;

: EXPECT-CLAUSE-JSON ( n n ptr u8 n -- ) {: outu:n erru:n at:ptr atu:n :}
   outu 0 T=
   CAP-ERR erru s\" \"code\":\"E-MISMATCH\"" CONTAINS? TTRUE
   CAP-ERR erru s\" \"word\":\"ckr-defr;does\",\"token\":\"dup\"" CONTAINS? TTRUE
   CAP-ERR erru at atu CONTAINS? TTRUE ;

: EXPECT-CLAUSE-PROSE ( n n n -- )
   70 T= {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru s" habu: in ckr-defr;does: at 'dup'" CONTAINS? TTRUE
   CAP-ERR erru s" preverify failed" CONTAINS? TFALSE ;

: EXPECT-CLAUSE-REFUSED ( ptr u8 n ptr u8 n -- ) {: src:ptr srcu:n at:ptr atu:n :}
   src srcu CLI-STDIN 70 T= {: outd:n errd:n :}
   outd errd at atu EXPECT-CLAUSE-JSON
   CAP-ERR errd s" preverify failed" CONTAINS? TTRUE
   src srcu s" --json-errors" CLI-FLAG-STDIN 70 T= {: outj:n errj:n :}
   outj errj at atu EXPECT-CLAUSE-JSON
   src srcu s" --all-errors" CLI-FLAG-STDIN EXPECT-CLAUSE-PROSE ;

: TEST-REFUSED-CLAUSE ( -- )
   CLAUSE-PLAIN$ CLAUSE-PLAIN-AT$ EXPECT-CLAUSE-REFUSED
   CLAUSE-RENDERED-USED$ CLAUSE-RENDERED-AT$ EXPECT-CLAUSE-REFUSED
   CLAUSE-RENDERED-UNUSED$ CLAUSE-RENDERED-AT$ EXPECT-CLAUSE-REFUSED ;

\ A named buffer count certifies by the effect of the word the load would run:
\ it takes nothing and leaves one `n`. Its value is the definer's to bound. A
\ word that takes input ends an expression whose value only the load has, so
\ its count is the definer's too.
: LBUF-ACCEPT ( ptr u8 n -- )
   DIRECT-STDIN 0 T=
   {: outu:n erru:n :}
   outu 0 T=
   erru 0 T= ;

\ A count the pre-pass cannot certify is its storage refusal, at the token.
: LBUF-REFUSE ( ptr u8 n -- )
   DIRECT-STDIN 70 T=
   {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru s" preverify failed" CONTAINS? TTRUE
   CAP-ERR erru s\" \"reason\":\"count resolves to no ( -- n ) word\"" CONTAINS? TTRUE ;

: TEST-LAYOUT-BUFFER-COUNT ( -- )
   s" package CKLB-B 8 constant CAP CAP TYPED-BUFFER ROWS n ;package" LBUF-ACCEPT
   s" package CKLB-Q public 8 constant CAP ;package package CKLB-QU CKLB-Q:CAP TYPED-BUFFER ROWS n ;package" LBUF-ACCEPT
   s" package CKLB-C 6 constant OPS 2 constant KEYS OPS KEYS + constant VOCAB VOCAB TYPED-BUFFER ROWS n ;package" LBUF-ACCEPT
   s" package CKLB-H HIR:OPCODES TYPED-BUFFER ROWS n ;package" LBUF-ACCEPT
   s" package CKLB-W : CAP ( -- n ) 4 ; CAP TYPED-BUFFER ROWS n ;package" LBUF-ACCEPT
   s" package CKLB-L SUMTYPE cklb 1 VARIANT value a ;VARIANT ;SUMTYPE 2 constant CAP CAP LAYOUT-BUFFER BUF cklb<n> ;package" LBUF-ACCEPT
   s" package CKLB-U CKLB-NOPE TYPED-BUFFER ROWS n ;package" LBUF-REFUSE
   s" package CKLB-V variable CAP CAP TYPED-BUFFER ROWS n ;package" LBUF-REFUSE
   s" package CKLB-T : CAP ( -- bool ) 0 0= ; CAP TYPED-BUFFER ROWS n ;package" LBUF-REFUSE
   s" package CKLB-I : CAP ( n -- n ) 2 * ; 4 CAP TYPED-BUFFER ROWS n ;package" LBUF-ACCEPT
   s" package CKLB-E 2 3 * TYPED-BUFFER ROWS n ;package" LBUF-ACCEPT
   s" 8 constant CKLB-CAP package CKLB-S public 4 constant CKLB-CAP ;package using CKLB-S CKLB-CAP TYPED-BUFFER CKLB-ROWS n ;using" LBUF-REFUSE
   \ A zero count passes pre-verification; the run's load refuses it.
   s" package CKLB-Z 0 constant CAP CAP TYPED-BUFFER ROWS n ;package" DIRECT-STDIN 67 T=
   {: outu:n erru:n :}
   CAP-ERR erru s" preverify failed" CONTAINS? TFALSE
   CAP-ERR erru s" uncaught throw code 7121" CONTAINS? TTRUE ;

: TEST-FILE-LABEL ( -- )
   BAD$SRC CORE-JSON 70 T=
   {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru BAD$ CONTAINS? TTRUE ;

: TEST-USAGE ( -- )
   CLI-BAD-FLAG 64 T=
   {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru s" usage: tools/check.f" CONTAINS? TTRUE ;

: TEST-SOURCE-BYTES-COPY ( -- )
   GOOD$ MUT-SRC!
   s" owned-source.f" MUT-LABEL!
   RESET
   MUT-SRC$ MUT-LABEL$ SOURCE
   $58 MUT-SRC c!
   RUN 0 T=
   RESET ;

: TEST-FILE-PATH-COPY ( -- )
   BAD$ BAD$SRC WRITE-ALL
   BAD$ MUT-PATH!
   RESET
   MUT-PATH$ FILE
   $58 MUT-PATH c!
   RUN 70 T=
   RESET ;

\ Repeating source-list is idempotent and an empty list still takes the
\ production usage path.
: TEST-LIST-IDEMPOTENT ( -- )
   RESET
   LIST-OPT
   LIST-OPT
   RUN 64 T=
   RESET ;

\ FILE followed by source-list promotes exactly one copied path. Losing the
\ entry returns usage, retaining the caller buffer breaks after mutation, and
\ duplicating the entry makes the definition fail preverification.
: TEST-LIST-PROMOTION ( -- )
   DIRECT$ GOOD$ WRITE-ALL
   DIRECT$ MUT-PATH!
   RESET
   MUT-PATH$ FILE
   LIST-OPT
   $58 MUT-PATH c!
   RUN 0 T=
   RESET ;

: TEST-EMPTY-SOURCE-MODE ( -- )
   RESET
   EMPTY-SOURCE
   [: LIST-OPT ;] 64 TTHROWSQ
   RUN 0 T=
   RESET
   s" " s" " SOURCE
   RESET ;

: TEST-BOUNDARY-PHASE ( -- )
   RESET
   BOUNDARY-OUT BUF-CAP LINT-OUT-BUFFER!
   s" 0 set-check" s" boundary-phase.f" SOURCE
   RUN 1 T=
   LINT-OUT$ s" CHECKER-MUTATION" CONTAINS? TTRUE
   LINT-OUT-BUFFER-OFF
   RESET ;

: TEST-OPTIONS ( -- )
   RESET
   s" json-errors" OPT
   s" all-errors" OPT
   s" json-errors" OPT
   s" all-errors" OPT
   GOOD$ s" options.f" SOURCE
   RUN 0 T=
   RESET ;

: TEST-MODE-COLLISIONS ( -- )
   RESET
   EMPTY-SOURCE
   [: EMPTY-SOURCE ;] 64 TTHROWSQ
   [: BAD-FILE ;] 64 TTHROWSQ
   [: LIST-OPT ;] 64 TTHROWSQ
   RESET
   BAD-FILE
   [: BAD-FILE ;] 64 TTHROWSQ
   [: EMPTY-SOURCE ;] 64 TTHROWSQ
   RESET
   LIST-OPT
   [: EMPTY-SOURCE ;] 64 TTHROWSQ
   RESET ;

: TEST-DIE ( -- )
   DIE$ CLI-STDIN 5 T=
   {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru s" bye" CONTAINS? TTRUE ;

: TEST-FWDREF-DIRECT ( -- )
   FWDREF$ DIRECT-STDIN 70 T=
   {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru s" E-UNDEFINED" CONTAINS? TTRUE
   CAP-ERR erru s" CKT-MISSING" CONTAINS? TTRUE ;

: TEST-FWDREF-JSON ( -- )
   FWDREF$ DIRECT-JSON-STDIN 70 T=
   {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru s" E-UNDEFINED" CONTAINS? TTRUE
   CAP-ERR erru s" CKT-MISSING" CONTAINS? TTRUE ;

: ORIGIN-SCAN$ ( -- ptr u8 n )
   SB-RESET
   $20 SB-APPEND-C  $09 SB-APPEND-C  $5c SB-APPEND-C
   s"  : POISON-LINE ( n -- n ) dup ;" SB-APPEND
   $0d SB-APPEND-C  $0a SB-APPEND-C
   s" ( : POISON-PAREN ( n -- n ) dup ;" SB-APPEND
   $0d SB-APPEND-C  $0a SB-APPEND-C
   s" body )" SB-APPEND  $0d SB-APPEND-C  $0a SB-APPEND-C
   s" s" SB-APPEND  $22 SB-APPEND-C
   s"  : POISON-NORMAL ( n -- n ) dup ;" SB-APPEND
   $0d SB-APPEND-C  $0a SB-APPEND-C
   s" normal tail" SB-APPEND  $22 SB-APPEND-C
   $0d SB-APPEND-C  $0a SB-APPEND-C
   s" s" SB-APPEND  $5c SB-APPEND-C  $22 SB-APPEND-C
   s"  prefix " SB-APPEND  $5c SB-APPEND-C  $22 SB-APPEND-C
   s"  : POISON-ESC ( n -- n ) dup ;" SB-APPEND
   $0d SB-APPEND-C  $0a SB-APPEND-C
   s" escaped tail" SB-APPEND  $22 SB-APPEND-C
   $0d SB-APPEND-C  $0a SB-APPEND-C
   s" : CKT-ORIGIN-SCAN ( n -- n ) dup ;" SB-APPEND
   $0d SB-APPEND-C  $0a SB-APPEND-C
   SB$ ;

: ORIGIN-BASE$ ( -- ptr u8 n )
   s" : CKT-ORIGIN-BASE ( n -- n ) dup ;" ;

: ORIGIN-NEXT$ ( -- ptr u8 n )
   SB-RESET
   $5c SB-APPEND-C  s"  base" SB-APPEND  $0a SB-APPEND-C
   s" : CKT-ORIGIN-NEXT ( n -- n ) dup ;" SB-APPEND
   SB$ ;

: ORIGIN-VERIFY-BASE ( -- n )
   CHECKER-CANDIDATE-SCOPE-START
   [: ORIGIN-BASE$ 7 9 100 VERIFY:SOURCE-BUF-AT-IN-SCOPE ;] catch {: rc:n :}
   CHECKER-CANDIDATE-SCOPE-DONE
   rc ;

: ORIGIN-VERIFY-NEXT ( -- n )
   CHECKER-CANDIDATE-SCOPE-START
   [: ORIGIN-NEXT$ 7 9 100 VERIFY:SOURCE-BUF-AT-IN-SCOPE ;] catch {: rc:n :}
   CHECKER-CANDIDATE-SCOPE-DONE
   rc ;

: TEST-ORIGIN-SCAN ( -- )
   ORIGIN-SCAN$ DIRECT-JSON-STDIN 70 T=
   {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru s\" \"word\":\"ckt-origin-scan\"" CONTAINS? TTRUE
   CAP-ERR erru s\" \"line\":8,\"column\":30,\"byte_start\":219,\"byte_end\":222" CONTAINS? TTRUE ;

: TEST-ORIGIN-BASE ( -- )
   CAP-ERR BUF-CAP DIAG-BUFFER!
   0 0= DIAG-JSON!
   ORIGIN-VERIFY-BASE 70 T=
   DIAG-BUFFER$ s\" \"line\":7,\"column\":38,\"byte_start\":129,\"byte_end\":132" CONTAINS? TTRUE
   CAP-ERR BUF-CAP DIAG-BUFFER!
   ORIGIN-VERIFY-NEXT 70 T=
   DIAG-BUFFER$ s\" \"line\":8,\"column\":30,\"byte_start\":136,\"byte_end\":139" CONTAINS? TTRUE
   DIAG-BUFFER-OFF
   0 0= 0= DIAG-JSON! ;

: RAW-FWDREF-TEST ( -- )
   FWDREF$ HB-LOAD-SRC 70 T=
   {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru s" E-UNDEFINED: CKT-MISSING" CONTAINS? TTRUE ;

: TEST-DUP-ALL ( -- )
   DUP$SRC CORE-JSON $4E T=
   {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru s" E-DUPLICATE-DEFINITION" CONTAINS? TTRUE
   CAP-ERR erru s" duplicate-definition" CONTAINS? TTRUE ;

: RESERVED-LIST-TEST ( -- )
   RESERVED-LIST-RUN 1 T=
   {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru s" E-RESERVED-DEFINITION" CONTAINS? TTRUE
   CAP-ERR erru s" <source-list>" CONTAINS? TTRUE ;

: AUDITED-LIB-TEST ( -- )
   \ The resident harness has already provided lib/test.f. A fresh tool
   \ process also exercises its resident verification instead of skipping it.
   CHECK-ARGV-START
   s" --source-list" CHECK-ARG+
   s" lib/test.f" CHECK-ARG+
   CHECK-CAPTURE 0 T=
   {: outu:n erru:n :}
   outu 0 T=
   erru 0 T= ;

: CHECK-TOOL-SOURCE ( ptr u8 n -- )
   CHECK-ARGV-START
   CHECK-ARG+
   CHECK-CAPTURE 0 T=
   {: outu:n erru:n :}
   outu 0 T=
   erru 0 T= ;

\ test/gate-images.f requires lib/aio.f, which ends by loading its macOS host
\ module (a require cycle) and calls SUMTYPE constructors it declares itself.
: TEST-IMAGE-TOOL-SOURCES ( -- )
   s" tools/engine-size.f" CHECK-TOOL-SOURCE
   s" tools/imgdump.f" CHECK-TOOL-SOURCE
   s" test/gate-images.f" CHECK-TOOL-SOURCE ;

\ ---- a subject the engine provides -------------------------------------------
\ A run loads nothing from a source the engine carries, so it checks nothing
\ there; rebuilding the engine checks it. A single file and a source list whose
\ every input is such a source are refused by one check, at each input, with
\ the same record under --json-errors and the same line otherwise. A source this
\ process loaded but the engine does not carry is still checked by the run.
: ENGINE-SRC$ ( -- ptr u8 n )
   s" src/core/type-schema.f" ;

: ENGINE-PROSE$ ( -- ptr u8 n )
   s\" E-ENGINE-PROVIDED src/core/type-schema.f:1:1: The engine provides this source; rebuild bin/hb to check a change to it.\n" ;

: ENGINE-JSON$ ( -- ptr u8 n )
   SB-RESET
   s\" {\"schema_version\":1,\"code\":\"E-ENGINE-PROVIDED\"," SB-APPEND
   s\" \"repair_class\":\"rebuild_engine\",\"verdict\":\"uncheckable\"," SB-APPEND
   s\" \"file\":\"src/core/type-schema.f\",\"line\":1,\"column\":1," SB-APPEND
   s\" \"suggestion\":\"The engine provides this source; rebuild bin/hb to check a change to it.\"}\n" SB-APPEND
   SB$ ;

: ENGINE-OPTS ( bool -- ) {: json:bool :}
   RESET
   s" all-errors" OPT
   json if s" json-errors" OPT then ;

: ENGINE-FILE-RUN ( bool -- n n n )
   ENGINE-OPTS
   ENGINE-SRC$ FILE
   [: RUN-ACT ;] IN-PROC ;

: ENGINE-LIST-RUN ( bool -- n n n )
   ENGINE-OPTS
   LIST-OPT
   ENGINE-SRC$ FILE
   [: RUN-ACT ;] IN-PROC ;

: EXPECT-ENGINE-REFUSAL ( n n n ptr u8 n -- ) {: outu:n erru:n rc:n want:ptr wantu:n :}
   rc 64 T=
   outu 0 T=
   CAP-ERR erru want wantu LINT-STR= TTRUE ;

: TEST-ENGINE-PROVIDED-JSON ( -- )
   true ENGINE-FILE-RUN ENGINE-JSON$ EXPECT-ENGINE-REFUSAL
   true ENGINE-LIST-RUN ENGINE-JSON$ EXPECT-ENGINE-REFUSAL ;

: TEST-ENGINE-PROVIDED-PROSE ( -- )
   false ENGINE-FILE-RUN ENGINE-PROSE$ EXPECT-ENGINE-REFUSAL
   false ENGINE-LIST-RUN ENGINE-PROSE$ EXPECT-ENGINE-REFUSAL ;

: EXPECT-CHECKED ( n n n -- )
   0 T= {: outu:n erru:n :}
   outu 0 T=
   erru 0 T= ;

\ This harness loaded lib/test.f; the engine does not carry it.
: TEST-HARNESS-LIB ( -- )
   s" lib/test.f" PATH-RUN EXPECT-CHECKED
   s" lib/test.f" LIST-RUN EXPECT-CHECKED ;

\ Each input is refused at its own spelling.
: EXPECT-PROVIDED-PAIR ( n n n -- )
   64 T= {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru s\" \"file\":\"src/core/type-schema.f\"," CONTAINS? TTRUE
   CAP-ERR erru s\" \"file\":\"./src/core/type-schema.f\"," CONTAINS? TTRUE ;

: PROVIDED-LIST-TEST ( -- )
   CHECK-ARGV-START
   s" --source-list" CHECK-ARG+
   ENGINE-SRC$ CHECK-ARG+
   CHECK-CAPTURE ENGINE-PROSE$ EXPECT-ENGINE-REFUSAL
   ENGINE-SRC$ s" ./src/core/type-schema.f"
   CLI-ALL-LIST EXPECT-PROVIDED-PAIR
   LIST$ GOOD$ WRITE-ALL
   s" src/core/type-schema.f" LIST$ CLI-ALL-LIST 0 T=
   {: outu:n erru:n :}
   outu 0 T= erru 0 T=
   LIST$ UNDEFINED$SRC WRITE-ALL
   s" src/core/type-schema.f" LIST$ CLI-ALL-LIST 70 T=
   {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru s" E-UNDEFINED" CONTAINS? TTRUE ;

: PREVERIFY-DIAG-TEST ( -- )
   LIST$ UNDEFINED$SRC WRITE-ALL
   LIST$ LIST-RUN 70 T=
   {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru s" check.f: source preverify failed before run" CONTAINS? TTRUE
   CAP-ERR erru LIST$ CONTAINS? TTRUE
   CAP-ERR erru s" E-UNDEFINED" CONTAINS? TTRUE
   CAP-ERR erru s" NOPE" CONTAINS? TTRUE ;

: VREC-GOOD-TEST ( -- )
   VREC-GOOD$ DIRECT-STDIN 0 T=
   {: outu:n erru:n :}
   outu 0 T=
   erru 0 T= ;

: TEST-LINEAR-BAD ( -- )
   LINEAR-BAD$ DIRECT-JSON-STDIN 70 T=
   {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru s" E-REJECTED" CONTAINS? TTRUE
   CAP-ERR erru s" dup" CONTAINS? TTRUE ;

: VREC-BAD-TEST ( -- )
   VREC-BAD$ DIRECT-JSON-STDIN 70 T=
   {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru s" E-MISMATCH" CONTAINS? TTRUE
   CAP-ERR erru s" field<rect,w,n>" CONTAINS? TTRUE ;

: TEST-NEWTYPE-GOOD ( -- )
   TFAM-GOOD$ DIRECT-STDIN 0 T=
   {: outu:n erru:n :}
   outu 0 T=
   erru 0 T= ;

: TEST-NEWTYPE-ALL ( -- )
   TFAM-GOOD$ DIRECT-ALL-STDIN 0 T=
   {: outu:n erru:n :}
   outu 0 T=
   erru 0 T= ;

: TEST-SUMTYPE-BAD ( -- )
   TFAM-BAD$ DIRECT-JSON-STDIN 70 T=
   {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru s" E-BAD-DECLARATION" CONTAINS? TTRUE
   CAP-ERR erru s" duplicate variant" CONTAINS? TTRUE ;

: TFAM-REDRIVE$ ( -- ptr u8 n )   \ good SUMTYPE + one real mismatch after it
   SB-RESET
   s" SUMTYPE zrc 0 VARIANT keep n ;VARIANT ;SUMTYPE" SB-APPEND
   $0a SB-APPEND-C
   s" : CKT-ZBAD ( n -- zrc ) ;" SB-APPEND
   SB$ ;

\ Regression habu-multi-err-re-60eb58a1 (fixed by the all-errors registry-replay
\ isolation, CHK-RUN-NOMINAL-LINTS closing its checker scope BEFORE CHK-RUN-ALL):
\ the redrive used to re-evaluate the SUMTYPE inside the nominal pass's
\ still-registered scope and emit a SPURIOUS E-BAD-DECLARATION duplicate-family
\ line beside the real mismatch. Pin: exactly ONE diagnostic, the mismatch.
: SUM-REDRIVE-TEST ( -- )
   TFAM-REDRIVE$ ALL-JSON-STDIN 70 T=
   {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru s" E-MISMATCH" CONTAINS? TTRUE
   CAP-ERR erru s" E-BAD-DECLARATION" CONTAINS? TFALSE
   CAP-ERR erru s" duplicate family" CONTAINS? TFALSE
   CAP-ERR erru 10 COUNT-CHAR 1 T= ;

\ Regression habu-fix-all-errors-67f4bdf9: the full check CLI --all-errors path
\ let a DEFTYPE family registration leak out of preverify into the redrive.
\ CHK-RUN-PREVERIFY drives VERIFY:SOURCE-BUF-IN-SCOPE, which registers the
\ source's family + derived casts but leaves rollback to the caller; the caller
\ never opened a scope, so those registrations survived into CHK-HANDLE-HB. When
\ the child load fails the JSON branch re-runs the all-errors redrive, whose own
\ re-registration then hit the leaked family and rejected with a spurious
\ E-DUPLICATE-DEFINITION. A DEFTYPE declarer needs `require deftype.f`
\ for the child to define it, so a bare source is statically clean yet its child
\ load fails E-UNDEFINED -- the honest verdict, and the only family surface that
\ reaches the JSON redrive re-run. SUMTYPE/NEWTYPE/DEFLINEAR declarers are
\ baked into the engine, so their child loads succeed, the redrive re-run never
\ fires, and they were never affected (probed: all report 0 through --all-errors).
: NOM-ALL-BARE$ ( -- ptr u8 n )   \ statically clean; bare child load is E-UNDEFINED
   SB-RESET
   s" DEFTYPE CKT-AEW" SB-APPEND $0a SB-APPEND-C
   s" : CKT-AEW-RT ( ckt-aew -- ckt-aew ) ;" SB-APPEND
   SB$ ;

\ Pin: the JSON redrive re-run reports the honest child E-UNDEFINED (70), never
\ a spurious E-DUPLICATE-DEFINITION from re-registering the preverified family.
: NOM-REDRIVE-TEST ( -- )
   NOM-ALL-BARE$ ALL-JSON-STDIN 70 T=
   {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru s" E-DUPLICATE-DEFINITION" CONTAINS? TFALSE ;

: NOM-ALL-CLEAN$ ( -- ptr u8 n )   \ require lets the child define DEFTYPE; zero errors
   SB-RESET
   s" require lib/type/deftype.f" SB-APPEND $0a SB-APPEND-C
   s" DEFTYPE CKT-AEOK" SB-APPEND $0a SB-APPEND-C
   s" : CKT-AEOK-RT ( ckt-aeok -- ckt-aeok ) ;" SB-APPEND
   SB$ ;

\ Green coverage: a DEFTYPE declaration through --all-errors reports zero errors
\ end to end when the child can load the declarer.
: NOM-CLEAN-TEST ( -- )
   NOM-ALL-CLEAN$ ALL-JSON-STDIN 0 T=
   {: outu:n erru:n :}
   outu 0 T=
   erru 0 T= ;

: TEST-TFAM-NOARITY ( -- )   \ S2 parity: missing arity -> declaration packet
   TFAM-NOARITY$ DIRECT-JSON-STDIN 70 T=
   {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru s" E-BAD-DECLARATION" CONTAINS? TTRUE
   CAP-ERR erru s" missing arity" CONTAINS? TTRUE ;

: TEST-SUM-NOEND ( -- )      \ S2 parity: unterminated sum -> declaration packet
   SUM-NOEND$ DIRECT-JSON-STDIN 70 T=
   {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru s" E-BAD-DECLARATION" CONTAINS? TTRUE
   CAP-ERR erru s" missing ;SUMTYPE" CONTAINS? TTRUE ;

\ S3 (dot habu-tfam-13-s3-truncate): the ALL-ERRORS collector path must report a
\ truncated declaration with the same packet, fail-closed — never a silent skip.
: SUM-NOEND-ALL ( -- )
   SUM-NOEND$ ALL-JSON-STDIN 70 T=
   {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru s" E-BAD-DECLARATION" CONTAINS? TTRUE
   CAP-ERR erru s" missing ;SUMTYPE" CONTAINS? TTRUE ;

: TFAM-NOARITY-ALL ( -- )
   TFAM-NOARITY$ ALL-JSON-STDIN 70 T=
   {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru s" E-BAD-DECLARATION" CONTAINS? TTRUE
   CAP-ERR erru s" missing arity" CONTAINS? TTRUE ;

: OVERCAP-SOURCE-BODY ( n ptr u8 NUM:alloc-byte-len -- )
   {: cap:n src:ptr extent:NUM:alloc-byte-len :}
   src cap s" <stdin>" SOURCE ;

: OVERCAP-SOURCE-THROW ( -- )
   OVERCAP-SOURCE-LEN dup MEM:BYTES-ALLOC-LEN
   [: OVERCAP-SOURCE-BODY ;] MEM:WITH-BYTES ;

: OVERCAP-FILE-BODY ( n ptr u8 NUM:alloc-byte-len -- )
   {: cap:n src:ptr extent:NUM:alloc-byte-len :}
   DIRECT$ src cap WRITE-ALL ;

: TEST-OVERCAP-SOURCE ( -- )
   RESET
   [: OVERCAP-SOURCE-THROW ;] catch 66 T=
   RESET
   OVERCAP-SOURCE-LEN dup MEM:BYTES-ALLOC-LEN
   [: OVERCAP-FILE-BODY ;] MEM:WITH-BYTES
   DIRECT$ PATH-RUN 66 T=
   {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru s" source exceeds capacity" CONTAINS? TTRUE
   RESET ;

: OVERCAP-PATH-BODY ( n ptr u8 NUM:alloc-byte-len -- )
   {: cap:n path:ptr extent:NUM:alloc-byte-len :}
   path cap FILE ;

: OVERCAP-PATH ( -- )
   FS-PATH-CAP 1+ dup MEM:BYTES-ALLOC-LEN
   [: OVERCAP-PATH-BODY ;] MEM:WITH-BYTES ;

: OVERCAP-LABEL-BODY ( n ptr u8 NUM:alloc-byte-len -- )
   {: cap:n label:ptr extent:NUM:alloc-byte-len :}
   s" " label cap SOURCE ;

: OVERCAP-LABEL ( -- )
   FS-PATH-CAP 1+ dup MEM:BYTES-ALLOC-LEN
   [: OVERCAP-LABEL-BODY ;] MEM:WITH-BYTES ;

: TEST-SELECTION-CAPACITY ( -- )
   RESET
   [: OVERCAP-PATH ;] E-FS-CAPACITY TTHROWSQ
   RESET
   [: OVERCAP-LABEL ;] E-FS-CAPACITY TTHROWSQ
   RESET ;

\ ---- every source the read accepts reaches a verdict --------------------------
\ Each pass after the read takes the source whole, so a source half the read's
\ cap passes, through a file and through standard input. The largest source
\ the read accepts cannot fit the run file the engine loads once the prefix and
\ the origin marks join it, so it is refused before the run, by name. Each
\ child gets a scratch root of its own as HB_TMP, and however it ends - a
\ verdict, a refusal or a die - it must leave that root empty.

OVERCAP-SOURCE-LEN 1 - constant CAP-SOURCE-LEN
CAP-SOURCE-LEN 2 / constant MID-SOURCE-LEN

create SCRATCH-PATH FS-PATH-CAP allot
create SIZED-PATH FS-PATH-CAP allot
variable SCRATCH-U
variable SIZED-U

: SCRATCH$ ( -- ptr u8 n )
   SCRATCH-PATH SCRATCH-U @ ;

: SIZED$ ( -- ptr u8 n )
   SIZED-PATH SIZED-U @ ;

: SCRATCH-MAKE ( -- )
   ROOT$ s" scratch" MAKE-TEMP-DIR SCRATCH-PATH SCRATCH-U PATH-COPY! ;

\ rmdir removes only an empty directory.
: SCRATCH-EMPTY ( -- )
   [: SCRATCH$ REMOVE-DIR ;] catch 0 T= ;

: SCRATCH-ENV ( -- )
   PROC-ENV-RESET
   s" HB_TMP" >LEN SCRATCH$ >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING ;

1 constant MODE-JSON                     \ check.f --json-errors
2 constant MODE-ALL                      \ check.f --all-errors

: MODE-ARGS ( n -- ) {: mode:n :}
   mode MODE-JSON and 0<> if s" --json-errors" CHECK-ARG+ then
   mode MODE-ALL and 0<> if s" --all-errors" CHECK-ARG+ then ;

\ check.f on the source as a file, in the given modes, with its standard error
\ captured into the given buffer.
: SCRATCH-MODE-RUN ( ptr u8 n n ptr u8 n -- n n n )
   {: src:ptr srcu:n mode:n err:ptr errcap:n :}
   ROOT$ s" sized-source.f" SIZED-PATH JOIN-PATH SIZED-U !
   SIZED$ src srcu WRITE-ALL
   SCRATCH-MAKE
   CHECK-ARGV-START
   mode MODE-ARGS
   SIZED$ CHECK-ARG+
   SCRATCH-ENV
   HB$ >LEN CAP-OUT BUF-CAP >LEN err errcap >LEN
   CHILD-HANG-MS >MS RUN-ARGV-ENV-CAPTURE
   CAPTURE>N ;

: SCRATCH-FILE-RUN ( ptr u8 n -- n n n )
   0 CAP-ERR BUF-CAP SCRATCH-MODE-RUN ;

: SCRATCH-STDIN-RUN ( ptr u8 n -- n n n ) {: src:ptr srcu:n :}
   SCRATCH-MAKE
   CHECK-ARGV-START
   SCRATCH-ENV
   HB$ >LEN src srcu >LEN CAP-OUT BUF-CAP >LEN CAP-ERR BUF-CAP >LEN
   CHILD-HANG-MS >MS RUN-ARGV-ENV-STDIN-CAPTURE
   CAPTURE>N ;

\ Blank lines, then a definition and its call on the last line, so the origin
\ pass marks a definition past the end of any buffer smaller than the read's.
: SIZED-DEF$ ( -- ptr u8 n )
   s" : CKT-SIZED ( -- ) ; CKT-SIZED" ;

: SIZED-FILL ( ptr u8 n -- ) {: a:ptr u:n :}
   SIZED-DEF$ {: d:ptr du:n :}
   u du - 0 ?do $0a a i + c! loop
   d a u + du - du BYTE-COPY ;

: EXPECT-PASS ( n n n -- ) {: outu:n erru:n rc:n :}
   rc 0 T=
   erru 0 T=
   SCRATCH-EMPTY ;

: EXPECT-OVERCAP ( n n n -- ) {: outu:n erru:n rc:n :}
   rc 66 T=
   CAP-ERR erru s" source exceeds capacity" CONTAINS? TTRUE
   SCRATCH-EMPTY ;

: MID-SOURCE-BODY ( n ptr u8 NUM:alloc-byte-len -- )
   {: u:n src:ptr extent:NUM:alloc-byte-len :}
   src u SIZED-FILL
   src u SCRATCH-FILE-RUN EXPECT-PASS
   src u SCRATCH-STDIN-RUN EXPECT-PASS ;

: CAP-SOURCE-BODY ( n ptr u8 NUM:alloc-byte-len -- )
   {: u:n src:ptr extent:NUM:alloc-byte-len :}
   src u SIZED-FILL
   src u SCRATCH-FILE-RUN EXPECT-OVERCAP
   src u SCRATCH-STDIN-RUN EXPECT-OVERCAP ;

: TEST-MID-SOURCE ( -- )
   MID-SOURCE-LEN dup MEM:BYTES-ALLOC-LEN
   [: MID-SOURCE-BODY ;] MEM:WITH-BYTES ;

: TEST-CAP-SOURCE ( -- )
   CAP-SOURCE-LEN dup MEM:BYTES-ALLOC-LEN
   [: CAP-SOURCE-BODY ;] MEM:WITH-BYTES ;

\ ---- a run's output past its capture is refused by name ----------------------
\ check.f captures what the run writes in order to replay it, 32768 bytes of
\ standard output and 131072 of standard error. A run that writes past either
\ is ended there and refused with check.f's refusal status by one line that
\ names the subject and both bounds, and its scratch root is left empty.

: OUT-OVER$ ( -- ptr u8 n )
   s\" : CKT-OUT-OVER ( -- ) 40000 0 ?do s\q x\q type loop ; CKT-OUT-OVER" ;

: ERR-OVER$ ( -- ptr u8 n )
   s\" : CKT-ERR-OVER ( -- ) 140000 0 ?do 2 s\q x\q write drop loop ; CKT-ERR-OVER" ;

: RUN-OVER-LINE$ ( ptr u8 n -- ptr u8 n ) {: label:ptr labelu:n :}
   SB-RESET
   s" check.f: " SB-APPEND
   label labelu SB-APPEND
   s" : the run wrote past its capture of 32768 bytes of standard output" SB-APPEND
   s\"  or 131072 of standard error\n" SB-APPEND
   SB$ ;

: EXPECT-RUN-OVER ( n n n ptr u8 n -- ) {: outu:n erru:n rc:n label:ptr labelu:n :}
   rc 70 T=
   outu 0 T=
   CAP-ERR erru label labelu RUN-OVER-LINE$ LINT-STR= TTRUE
   SCRATCH-EMPTY ;

: TEST-RUN-OUTPUT-CAP ( -- )
   OUT-OVER$ SCRATCH-FILE-RUN SIZED$ EXPECT-RUN-OVER
   ERR-OVER$ SCRATCH-STDIN-RUN s" <stdin>" EXPECT-RUN-OVER ;

\ verify-source ends the process with `die` on an unterminated signature.
: DIE-SIGNATURE$ ( -- ptr u8 n )
   s" : CKT-OPEN-SIG ( n -- n" ;

: TEST-DIE-SCRATCH ( -- )
   DIE-SIGNATURE$ SCRATCH-STDIN-RUN {: outu:n erru:n rc:n :}
   rc 0 T<>
   SCRATCH-EMPTY ;

\ ---- the report carries whatever the source makes it carry -------------------
\ A record carries its token whole, and the report carries every record: their
\ length and number are the source's, not a buffer's. A 40 KB name, which the
\ engine refuses at its definition, makes a record of over 40 KB in every mode;
\ a thousand refused definitions make an --all-errors report of over 50 KB in
\ either rendering. Each run ends in the refusal with the whole name or every
\ record on standard error, and leaves its scratch root empty. Each test's
\ allocation holds its source, then the capture of standard error.

4 constant MODE-N                        \ every combination of MODE-JSON and MODE-ALL
$100000 constant REPORT-ERR-CAP
40000 constant LONG-NAME-LEN
1000 constant REFUSAL-N

: LONG-HEAD$ ( -- ptr u8 n )
   s" : CKT-" ;

: LONG-TAIL$ ( -- ptr u8 n )
   s"  ( -- ) ;" ;

LONG-HEAD$ nip LONG-NAME-LEN + LONG-TAIL$ nip + constant LONG-SOURCE-LEN

: LONG-FILL ( ptr u8 -- ) {: a:ptr :}
   LONG-HEAD$ {: h:ptr hu:n :}
   LONG-TAIL$ {: t:ptr tu:n :}
   h a hu BYTE-COPY
   LONG-SOURCE-LEN tu - hu ?do $4c a i + c! loop
   t a LONG-SOURCE-LEN + tu - tu BYTE-COPY ;

\ The defined name: the source without the colon, its space and the tail.
: LONG-NAME$ ( ptr u8 -- ptr u8 n ) {: a:ptr :}
   a 2 +  LONG-SOURCE-LEN 2 - LONG-TAIL$ nip - ;

: LONG-NAME-MODE ( ptr u8 ptr u8 n -- ) {: src:ptr err:ptr mode:n :}
   src LONG-SOURCE-LEN mode err REPORT-ERR-CAP SCRATCH-MODE-RUN
   {: outu:n erru:n rc:n :}
   rc 70 T=
   err erru src LONG-NAME$ CONTAINS? TTRUE
   SCRATCH-EMPTY ;

: LONG-NAME-BODY ( ptr u8 NUM:alloc-byte-len -- ) {: a:ptr extent:NUM:alloc-byte-len :}
   a LONG-SOURCE-LEN + {: err:ptr :}
   a LONG-FILL
   MODE-N 0 ?do a err i LONG-NAME-MODE loop ;

: TEST-LONG-NAME ( -- )
   LONG-SOURCE-LEN REPORT-ERR-CAP + MEM:BYTES-ALLOC-LEN
   [: LONG-NAME-BODY ;] MEM:WITH-BYTES ;

: REFUSAL$ ( -- ptr u8 n )
   s" : CKT-R0000 ( n -- n ) dup ;" ;

REFUSAL$ nip 1+ constant REFUSAL-LINE-LEN
REFUSAL-N REFUSAL-LINE-LEN * constant REFUSAL-SOURCE-LEN

\ Line k defines CKT-Rk, k in four digits, and refuses it.
: REFUSAL-LINE ( ptr u8 n -- ) {: line:ptr k:n :}
   REFUSAL$ {: t:ptr tu:n :}
   t line tu BYTE-COPY
   k 4 0 ?do
      dup 10 mod $30 + line 10 i - + c!
      10 /
   loop drop
   $0a line tu + c! ;

: REFUSAL-FILL ( ptr u8 n -- )
   {: a:ptr count:n :}
   count 0 ?do a i REFUSAL-LINE-LEN * + i 1+ REFUSAL-LINE loop ;

\ One line per refused definition, in either rendering.
: REFUSALS-MODE ( ptr u8 ptr u8 n -- ) {: src:ptr err:ptr mode:n :}
   src REFUSAL-SOURCE-LEN mode err REPORT-ERR-CAP SCRATCH-MODE-RUN
   {: outu:n erru:n rc:n :}
   rc 70 T=
   err erru 10 COUNT-CHAR REFUSAL-N T=
   SCRATCH-EMPTY ;

: REFUSALS-BODY ( ptr u8 NUM:alloc-byte-len -- ) {: a:ptr extent:NUM:alloc-byte-len :}
   a REFUSAL-SOURCE-LEN + {: err:ptr :}
   a REFUSAL-N REFUSAL-FILL
   a err MODE-ALL REFUSALS-MODE
   a err MODE-ALL MODE-JSON or REFUSALS-MODE ;

: TEST-REFUSALS ( -- )
   REFUSAL-SOURCE-LEN REPORT-ERR-CAP + MEM:BYTES-ALLOC-LEN
   [: REFUSALS-BODY ;] MEM:WITH-BYTES ;

\ ---- a report past the scratch -----------------------------------------------
\ --all-errors renders every refusal into a scratch of INCLUDE-BUF-CAP bytes and
\ streams each record to standard error. Each JSON record here is over 400
\ bytes, so three thousand refusals pass the scratch: the run reports the
\ records that fit, each whole, then the next one's refusal as an
\ E-STATEMENT-THROW record whose throw_code is E-DIAG-CAPACITY (-2901), and
\ exits 70. A full scratch must neither end the process nor lose the records
\ before it. The capture holds twice the scratch.

3000 constant FULL-REFUSAL-N
FULL-REFUSAL-N REFUSAL-LINE-LEN * constant FULL-SOURCE-LEN
INCLUDE-BUF-CAP 2 * constant FULL-ERR-CAP

\ Where the last line of a report ending in a line feed starts.
: LAST-LINE-AT ( ptr u8 n -- n )
   {: a:ptr u:n :}
   0
   u 1 - 0 ?do a i + c@ $0a = if drop i 1+ then loop ;

: FULL-BODY ( ptr u8 NUM:alloc-byte-len -- )
   {: a:ptr extent:NUM:alloc-byte-len :}
   a FULL-SOURCE-LEN + {: err:ptr :}
   a FULL-REFUSAL-N REFUSAL-FILL
   a FULL-SOURCE-LEN MODE-ALL MODE-JSON or err FULL-ERR-CAP SCRATCH-MODE-RUN
   {: outu:n erru:n rc:n :}
   rc 70 T=
   err erru + 1 - c@ $0a T=
   err erru s\" \"word\":\"ckt-r0001\"" CONTAINS? TTRUE
   err erru LAST-LINE-AT {: cut:n :}
   err cut + erru cut - {: last:ptr lastu:n :}
   last lastu s\" \"code\":\"E-STATEMENT-THROW\"" CONTAINS? TTRUE
   last lastu s\" \"throw_code\":-2901" CONTAINS? TTRUE
   SCRATCH-EMPTY ;

: TEST-SCRATCH-FULL ( -- )
   FULL-SOURCE-LEN FULL-ERR-CAP + MEM:BYTES-ALLOC-LEN
   [: FULL-BODY ;] MEM:WITH-BYTES ;

: LIST-CAP-FILL ( -- )
   LIST-ENTRY-CAP 0 ?do BAD$ FILE loop ;

: TEST-LIST-CAPACITY ( -- )
   RESET
   LIST-OPT
   [: LIST-CAP-FILL ;] catch 0 T=
   [: BAD-FILE ;] catch 64 T=
   RESET ;

: MISSING-PATH! ( ptr u8 n -- )
   ROOT$ 2swap MUT-PATH JOIN-PATH MUT-PATH-U ! ;

: MISSING-FILE ( -- )
   s" missing-source.f" MISSING-PATH!
   MUT-PATH$ FILE ;

: TEST-EMPTY-LIST ( -- )
   EMPTY-LIST-RUN 64 T=
   {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru s" usage: tools/check.f" CONTAINS? TTRUE ;

: TEST-MISSING-FILE ( -- )
   RESET
   MISSING-FILE
   RUN 66 T=
   RESET ;

: MISSING-ENGINE-PATH ( ptr u8 n -- )
   CLI-MISSING-HB 69 T=
   {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru s\" check.f: bin/hb missing\n" LINT-STR= TTRUE ;

: TEST-MISSING-ENGINE ( -- )
   CLI-SETUP
   s" tools/check.f" MISSING-ENGINE-PATH
   s" ./tools/../tools/check.f" MISSING-ENGINE-PATH
   CLI-ROOT$ s" tools/./check.f" SOURCE-ROOT:JOIN MISSING-ENGINE-PATH ;

: TEST-REPEAT-SOURCE-OK ( -- )
   RESET
   GOOD$ s" repeat-source.f" SOURCE
   RUN 0 T=
   RUN 0 T=
   RESET ;

: TEST-REPEAT-SOURCE-FAIL ( -- )
   RESET
   s" json-errors" OPT
   FWDREF$ s" repeat-fail.f" SOURCE
   RUN 70 T=
   RUN 70 T=
   RESET ;

: TEST-REPEAT-FILE ( -- )
   BAD$ BAD$SRC WRITE-ALL
   RESET
   BAD-FILE
   RUN 70 T=
   BAD$ GOOD$ WRITE-ALL
   RUN 0 T=
   RESET
   BAD$ BAD$SRC WRITE-ALL ;

: TEST-REPEAT-LIST ( -- )
   BAD$ BAD$SRC WRITE-ALL
   RESET
   LIST-OPT
   BAD-FILE
   RUN 70 T=
   BAD$ GOOD$ WRITE-ALL
   RUN 0 T=
   RESET
   BAD$ BAD$SRC WRITE-ALL ;


\ C2: a declaration body over TDECL-CAP ($1000) must report the same
\ E-BAD-DECLARATION packet (the length check fires ahead of variant parsing, so
\ the repeated variant names never matter).
create BIG $2000 allot   variable BIG-U
: BIG-C, ( n -- ) BIG BIG-U @ + c!  BIG-U @ 1+ BIG-U ! ;
: BIG-APP ( ptr u8 n -- ) {: a:ptr u:n :}  u 0 ?do a i + c@ BIG-C, loop ;
: OVERSIZE$ ( -- ptr u8 n )
   0 BIG-U !
   s" SUMTYPE ckbig 1 " BIG-APP
   200 0 ?do s" VARIANT vvvvvvvvvvvvvvvvvvvv n ;VARIANT " BIG-APP loop
   s" ;SUMTYPE" BIG-APP
   BIG BIG-U @ ;
: TEST-OVERSIZE ( -- )
   OVERSIZE$ DIRECT-JSON-STDIN 70 T=
   {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru s" E-BAD-DECLARATION" CONTAINS? TTRUE
   CAP-ERR erru s" declaration too long" CONTAINS? TTRUE ;

: TEST-ENUM-GOOD ( -- )
   ENUM-GOOD$ DIRECT-STDIN 0 T=
   {: outu:n erru:n :}
   outu 0 T=
   erru 0 T= ;

: TEST-ENUM-BAD ( -- )
   ENUM-BAD$ DIRECT-JSON-STDIN 70 T=
   {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru s" E-BAD-DECLARATION" CONTAINS? TTRUE
   CAP-ERR erru s" duplicate variant" CONTAINS? TTRUE ;

: TEST-STRUCT-GOOD ( -- )
   STRUCT-GOOD$ DIRECT-STDIN 0 T=
   {: outu:n erru:n :}
   outu 0 T=
   erru 0 T= ;

: TEST-STRUCT-PAYLOAD ( -- )
   STRUCT-PAYLOAD$ DIRECT-STDIN 0 T=
   {: outu:n erru:n :}
   outu 0 T=
   erru 0 T= ;

\ Both diagnostic legs of a malformed STRUCTURE, through the check tool's own
\ declaration-packet capture (CHK-DECL-CAPTURE / CHK-DECL-FLUSH). The unified
\ front end raises through the shared DECL-REJECT packet, which renders with the
\ same writer the legacy definers use, so the check tool collects it unchanged.
: TEST-STRUCT-BAD-JSON ( -- )
   STRUCT-BAD$ DIRECT-JSON-STDIN 70 T=
   {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru s" E-BAD-DECLARATION" CONTAINS? TTRUE
   CAP-ERR erru s" unknown field type" CONTAINS? TTRUE
   CAP-ERR erru s" nosuchtype" CONTAINS? TTRUE ;

: TEST-STRUCT-BAD-PROSE ( -- )
   STRUCT-BAD$ DIRECT-STDIN 70 T=
   {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru s" bad structure declaration 'sbad'" CONTAINS? TTRUE
   CAP-ERR erru s" unknown field type at 'nosuchtype'" CONTAINS? TTRUE ;

\ The migrated ENUM arm reports through the same packet on the prose leg too;
\ the JSON leg is TEST-ENUM-BAD above.
: TEST-ENUM-BAD-PROSE ( -- )
   ENUM-BAD$ DIRECT-STDIN 70 T=
   {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru s" bad enum declaration 'ebad'" CONTAINS? TTRUE
   CAP-ERR erru s" duplicate variant at 'red'" CONTAINS? TTRUE ;

: TEST-PRODUCT-GOOD ( -- )
   PROD-GOOD$ DIRECT-STDIN 0 T=
   {: outu:n erru:n :}
   outu 0 T=
   erru 0 T= ;

: TEST-PRODUCT-BAD ( -- )
   PROD-BAD$ DIRECT-JSON-STDIN 70 T=
   {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru s" E-BAD-DECLARATION" CONTAINS? TTRUE
   CAP-ERR erru s" duplicate field" CONTAINS? TTRUE ;

: PROD-ALL-TEST ( -- )   \ verify-source support replay (RECORD-PRODUCT)
   PROD-ALL$SRC CORE-JSON 0 T=
   {: outu:n erru:n :}
   outu 0 T=
   erru 0 T= ;

: TEST-ENUM-NOEND ( -- )
   ENUM-NOEND$ DIRECT-JSON-STDIN 70 T=
   {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru s" E-BAD-DECLARATION" CONTAINS? TTRUE
   CAP-ERR erru s" enoend" CONTAINS? TTRUE
   CAP-ERR erru s" missing ;ENUM" CONTAINS? TTRUE ;

\ Each case asserts check's OWN exit (70) and its rendered packet, which is what
\ distinguishes "the nominal pass rejected it" from "the child run happened to
\ die later" (exit 67, no packet) — the state before the capture was repaired.
: TEST-ENUM-LINE-COMMENT ( -- )
   ENUM-LINE-COMMENT$ DIRECT-STDIN 70 T=
   {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru s" bad enum declaration 'ecmt'" CONTAINS? TTRUE
   CAP-ERR erru s" name must be a lowercase tail at '\" CONTAINS? TTRUE ;

: TEST-ENUM-PAREN-COMMENT ( -- )
   ENUM-PAREN-COMMENT$ DIRECT-STDIN 70 T=
   {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru s" bad enum declaration 'epcmt'" CONTAINS? TTRUE
   CAP-ERR erru s" at '('" CONTAINS? TTRUE ;

: TEST-STRUCT-LINE-COMMENT ( -- )
   STRUCT-LINE-COMMENT$ DIRECT-STDIN 70 T=
   {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru s" bad structure declaration 'scmt'" CONTAINS? TTRUE
   CAP-ERR erru s" unexpected token in structure declaration at '\" CONTAINS? TTRUE ;

: TEST-STRUCT-PAREN-COMMENT ( -- )
   STRUCT-PAREN-COMMENT$ DIRECT-STDIN 70 T=
   {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru s" bad structure declaration 'spcmt'" CONTAINS? TTRUE
   CAP-ERR erru s" at '('" CONTAINS? TTRUE ;

\ How long a declaration may be, and which bound answers when it is too long.
\
\ 252 variants is 7812 bytes of body. That used to be refused, by the ENGINE's
\ own 4096-byte TDECL-CAP: the legacy compact definer copied the whole body into
\ one fixed buffer before parsing it, and `TDECL-REQUIRE-FIT` refused anything
\ that did not fit. The global ENUM keyword is the unified front end now, which reads
\ tokens straight from the input source with a one-token pushback and never
\ buffers a body, so that bound is simply gone — it was a property of the old
\ collection strategy, not of the language, and the declaration is well formed.
\ The command exits 0; test/decl-replay-verify-source.f counts every variant of
\ a 1000-variant body through the same front end.
\
\ One length bound is left, and it is the one that matters here: this tool does
\ not interpret the source, it buffers each declaration through
\ src/habu/verify-source.f's 8000-byte body buffer and replays it. That buffer
\ RAISES rather than shortening — a buffer that cannot represent its input must
\ say so — so a longer declaration (271 variants, 8401 bytes) still fails
\ loudly, with the declaration layer's own "declaration too long" code 7118. It
\ surfaces from THIS process's preverify pass, not the child run, and the
\ preverify reports it as a statement throw naming that code, exiting 70 as for
\ a refusal. Its exact edge is pinned in test/decl-replay-verify-source.f. What
\ this case pins is the property that matters at the command line: a long
\ declaration either goes through whole or is refused loudly, and is never
\ quietly shortened into one that parses.
: TEST-DECL-OVER-CAP ( -- )
   252 LONG-ENUM$ DIRECT-STDIN 0 T=
   {: outu:n erru:n :}
   outu 0 T=
   erru 0 T=
   271 LONG-ENUM$ DIRECT-STDIN 70 T=
   {: outu2:n erru2:n :}
   outu2 0 T=
   CAP-ERR erru2 s" preverify failed" CONTAINS? TTRUE
   CAP-ERR erru2 s" : throw 7118 at '" CONTAINS? TTRUE ;

: TEST-STRUCT-NOEND ( -- )
   STRUCT-NOEND$ DIRECT-JSON-STDIN 70 T=
   {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru s" E-BAD-DECLARATION" CONTAINS? TTRUE
   CAP-ERR erru s" snoend" CONTAINS? TTRUE
   CAP-ERR erru s" missing ;STRUCTURE" CONTAINS? TTRUE ;

: TEST-PROD-NOEND ( -- )
   PROD-NOEND$ DIRECT-STDIN 70 T=
   {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru s" check.f: missing ;PRODUCT" CONTAINS? TTRUE ;

: TEST-VREC-NOEND ( -- )
   VREC-NOEND$ DIRECT-STDIN 70 T=
   {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru s" check.f: missing END-VALUE-RECORD" CONTAINS? TTRUE ;

\ Same arm through the real CLI entry: `bin/hb tools/check.f fixture` must
\ carry the diagnostic on stderr with the rc unchanged.
\ The engine's script form reads standard input to end of file before it runs
\ the script, so the child must be handed its own empty input. Letting it
\ inherit whatever input this test process happens to have makes the case block
\ until the capture timeout whenever that stream stays open.
: ENUM-CLI-RUN ( -- n n n )
   BAD$ ENUM-NOEND$ WRITE-ALL
   PROC-ARGV-RESET
   s" tools/check.f" >LEN PROC-ARGV+
   BAD$ >LEN PROC-ARGV+
   HB$ >LEN s" " >LEN
   CAP-OUT BUF-CAP >LEN CAP-ERR BUF-CAP >LEN
   CHILD-HANG-MS >MS RUN-ARGV-STDIN-CAPTURE
   CAPTURE>N ;

: ENUM-CLI-TEST ( -- )
   ENUM-CLI-RUN 70 T=
   {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru s" bad enum declaration 'enoend'" CONTAINS? TTRUE
   CAP-ERR erru s" missing ;ENUM" CONTAINS? TTRUE ;

: VREC-PARTIAL-TEST ( -- )
   VREC-PARTIAL$ DIRECT-JSON-STDIN 70 T=
   {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru s" E-MISMATCH" CONTAINS? TTRUE
   CAP-ERR erru s" field<point,y,n>" CONTAINS? TTRUE ;

\ This fixture deliberately defines a word called DEFTYPE at top level, so it
\ only type-checks in an engine whose dictionary does not already hold the real
\ DEFTYPE declarer. This test library runs inside the gate runner, which does
\ load lib/type/deftype.f, so checking the fixture here would be a true
\ duplicate definition. The standalone tools/check.f process has the smaller
\ dictionary the fixture needs, which is why this case keeps the child process.
: NOM-SCAN-TEST ( -- )
   NOM-SCAN-BODY$ CLI-STDIN 0 T=
   {: outu:n erru:n :}
   outu 0 T=
   erru 0 T= ;

: TEST-NOMINAL-PREVERIFY ( -- )
   NOM-PREVERIFY$ DIRECT-STDIN 0 T=
   {: outu:n erru:n :}
   outu 0 T=
   erru 0 T= ;

: TEST-NOMINAL-SHADOW ( -- )
   NOM-SHADOW$ DIRECT-STDIN 0 T=
   {: outu:n erru:n :}
   outu 0 T=
   erru 0 T= ;

: TEST-NOMINAL-DUP ( -- )
   NOM-DUP$ DIRECT-JSON-STDIN 70 T=
   {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru s" E-BAD-NOMINAL-TYPE" CONTAINS? TTRUE
   CAP-ERR erru s" CKD-TWICE" CONTAINS? TTRUE ;

: NOM-LOAD-REFUSED ( n n n -- )
   70 T=
   {: outu:n erru:n :}
   CAP-ERR erru s" bad or duplicate" CONTAINS? TTRUE ;

: NOM-CHECK-REFUSED ( ptr u8 n -- )
   DIRECT-JSON-STDIN 70 T=
   {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru s" E-BAD-NOMINAL-TYPE" CONTAINS? TTRUE ;

: NOM-LOAD-ADMITTED ( n n n -- )
   0 T=
   {: outu:n erru:n :}
   erru 0 T= ;

: NOM-CHECK-ADMITTED ( ptr u8 n -- )
   DIRECT-STDIN 0 T=
   {: outu:n erru:n :}
   outu 0 T=
   erru 0 T= ;

: DEF-LOAD-REFUSED ( n n n -- )
   67 T=
   {: outu:n erru:n :}
   CAP-ERR erru s" bad newtype declaration" CONTAINS? TTRUE
   CAP-ERR erru s" reserved name" CONTAINS? TTRUE ;

: DEF-PROSE-REFUSED ( ptr u8 n -- )
   DIRECT-STDIN 70 T=
   {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru s" bad nominal type" CONTAINS? TTRUE ;

: PKG-DEF-REFUSED ( ptr u8 n -- ) {: a:ptr u:n :}
   a u NOM-PKG-DEF$ HB-LOAD-SRC DEF-LOAD-REFUSED
   a u NOM-PKG-DEF$ NOM-CHECK-REFUSED
   a u NOM-PKG-DEF$ DEF-PROSE-REFUSED ;

: TOP-DEF-REFUSED ( ptr u8 n -- ) {: a:ptr u:n :}
   a u NOM-TOP-DEF$ HB-LOAD-SRC DEF-LOAD-REFUSED
   a u NOM-TOP-DEF$ NOM-CHECK-REFUSED
   a u NOM-TOP-DEF$ DEF-PROSE-REFUSED ;

: TEST-NOMINAL-CTOR-TAIL ( -- )
   s" PTR" PKG-DEF-REFUSED
   s" PTR" TOP-DEF-REFUSED ;

: TEST-NOMINAL-SHADOW-SIDE ( -- )
   NOM-SIDE$ HB-LOAD-SRC NOM-LOAD-ADMITTED
   NOM-SIDE$ NOM-CHECK-ADMITTED ;

\ Each refusal names the declared tail: `declaration 'ptr': reserved name`.
: RESERVED-LOAD-REFUSED ( n n n ptr u8 n -- ) {: want:ptr wantu:n :}
   67 T=
   {: outu:n erru:n :}
   CAP-ERR erru want wantu CONTAINS? TTRUE ;

: RESERVED-JSON-REFUSED ( ptr u8 n -- )
   DIRECT-JSON-STDIN 70 T=
   {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru s" E-BAD-DECLARATION" CONTAINS? TTRUE
   CAP-ERR erru s" reserved name" CONTAINS? TTRUE ;

: RESERVED-PROSE-REFUSED ( ptr u8 n ptr u8 n -- ) {: want:ptr wantu:n :}
   DIRECT-STDIN 70 T=
   {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru want wantu CONTAINS? TTRUE ;

: RESERVED-DECL-REFUSED ( [ -- ptr u8 n ] ptr u8 n -- ) {: q want:ptr wantu:n :}
   q execute HB-LOAD-SRC want wantu RESERVED-LOAD-REFUSED
   q execute RESERVED-JSON-REFUSED
   q execute want wantu RESERVED-PROSE-REFUSED ;

: PTR-REFUSAL$ ( -- ptr u8 n )
   s" declaration 'ptr': reserved name" ;

: TEST-DECL-CTOR-TAIL ( -- )
   [: s" ENUM ptr red green ;ENUM" DECL-PKG$ ;] PTR-REFUSAL$ RESERVED-DECL-REFUSED
   [: s" ENUM ptr red green ;ENUM" ;] PTR-REFUSAL$ RESERVED-DECL-REFUSED
   [: s" STRUCTURE ptr 0 FIELD x n ;STRUCTURE" DECL-PKG$ ;] PTR-REFUSAL$ RESERVED-DECL-REFUSED
   [: s" STRUCTURE ptr 0 FIELD x n ;STRUCTURE" ;] PTR-REFUSAL$ RESERVED-DECL-REFUSED ;

: VREC-REFUSAL$ ( -- ptr u8 n )
   s" declaration 'ckvr': reserved name" ;

: TEST-DECL-VREC-TAIL ( -- )
   [: s" ENUM ckvr red green ;ENUM" VREC-PKG$ ;] VREC-REFUSAL$ RESERVED-DECL-REFUSED
   [: s" ENUM ckvr red green ;ENUM" VREC-TOP$ ;] VREC-REFUSAL$ RESERVED-DECL-REFUSED
   [: s" STRUCTURE ckvr 0 FIELD x n ;STRUCTURE" VREC-PKG$ ;] VREC-REFUSAL$ RESERVED-DECL-REFUSED
   [: s" STRUCTURE ckvr 0 FIELD x n ;STRUCTURE" VREC-TOP$ ;] VREC-REFUSAL$ RESERVED-DECL-REFUSED ;

\ An atom-shaped tail (checker.f ATOM-TOK?) reads as an atom wherever the family
\ does not resolve, outside the package that owns it for one, so ENUM and
\ STRUCTURE refuse it as SUMTYPE does.
: ATOM-REFUSAL$ ( -- ptr u8 n )
   s" declaration 'space-ck': reserved name" ;

: TEST-DECL-ATOM-TAIL ( -- )
   [: s" ENUM space-ck red green ;ENUM" DECL-PKG$ ;] ATOM-REFUSAL$ RESERVED-DECL-REFUSED
   [: s" STRUCTURE space-ck 0 FIELD x n ;STRUCTURE" DECL-PKG$ ;] ATOM-REFUSAL$ RESERVED-DECL-REFUSED ;

: LIN-REFUSED ( ptr u8 n -- ) {: a:ptr u:n :}
   a u NOM-LIN$ HB-LOAD-SRC NOM-LOAD-REFUSED
   a u NOM-LIN$ NOM-CHECK-REFUSED ;

: LIN-ADMITTED ( ptr u8 n -- ) {: a:ptr u:n :}
   a u NOM-LIN$ HB-LOAD-SRC NOM-LOAD-ADMITTED
   a u NOM-LIN$ NOM-CHECK-ADMITTED ;

: REC-REFUSED ( ptr u8 n -- ) {: a:ptr u:n :}
   a u NOM-REC$ HB-LOAD-SRC NOM-LOAD-REFUSED
   a u NOM-REC$ NOM-CHECK-REFUSED ;

: REC-ADMITTED ( ptr u8 n -- ) {: a:ptr u:n :}
   a u NOM-REC$ HB-LOAD-SRC NOM-LOAD-ADMITTED
   a u NOM-REC$ NOM-CHECK-ADMITTED ;

: TEST-NOMINAL-NAME-REFUSED ( -- )
   s" N" LIN-REFUSED
   s" --" LIN-REFUSED
   s" [" LIN-REFUSED
   s" a)b" LIN-REFUSED
   s" (" LIN-REFUSED
   s" \" LIN-REFUSED
   s" R" REC-REFUSED
   s" (" REC-REFUSED
   s" \" REC-REFUSED
   NOM-LIN-QUOTE$ HB-LOAD-SRC NOM-LOAD-REFUSED
   NOM-LIN-QUOTE$ NOM-CHECK-REFUSED ;

: TEST-NOMINAL-NAME-ADMITTED ( -- )
   s" CELL" LIN-ADMITTED
   s" ckn-lin" LIN-ADMITTED
   s" CKN:lin" LIN-ADMITTED
   s" PTR" REC-ADMITTED
   s" ckn-rec" REC-ADMITTED
   s" CKN:rec" REC-ADMITTED ;

: FAM-CLAIM-REFUSED ( [ -- ptr u8 n ] -- )
   dup execute HB-LOAD-SRC NOM-LOAD-REFUSED
   execute NOM-CHECK-REFUSED ;

: TEST-NOMINAL-FAMILY-CLAIM ( -- )
   [: FAM-PRIV-HEAD s" ckf-tail" FAM-LIN s" ckf-tail" FAM-DROP$ ;] FAM-CLAIM-REFUSED
   [: FAM-PRIV-HEAD s" ckf-tail" FAM-REC s" ckf-tail" FAM-DROP$ ;] FAM-CLAIM-REFUSED
   [: FAM-PRIV-HEAD s" CKFP:ckf-tail" FAM-LIN s" CKFP:ckf-tail" FAM-DROP$ ;] FAM-CLAIM-REFUSED
   [: FAM-SHARED-HEAD s" ckf-tail" FAM-LIN s" ckf-tail" FAM-DROP$ ;] FAM-CLAIM-REFUSED
   [: FAM-SHARED-HEAD s" ckf-tail" FAM-REC s" ckf-tail" FAM-DROP$ ;] FAM-CLAIM-REFUSED ;

\ A definer takes its name with parse-name: the next whitespace-delimited token,
\ whatever it spells, so `package (` names the package `(` and `: \` the word
\ `\`, while `DEFLINEAR (` and `DEFLINEAR \` read the names `(` and `\` and
\ refuse them as types. Each source declares such a name on its first line. The
\ second line opens with a comment or with a reserved word, so a stage that
\ lexed the name as a comment opener would take that token for the name, or
\ none. Every stage of the check must reach the loader's verdict, and a refusal
\ names the same token.
: OPERAND-SRC$ ( ptr u8 n ptr u8 n -- ptr u8 n ) {: a:ptr u:n b:ptr v:n :}
   SB-RESET
   a u SB-APPEND $0a SB-APPEND-C
   b v SB-APPEND
   SB$ ;

: AFTER-COMMENT$ ( -- ptr u8 n )
   s" ( note ) : CKN-F ( -- n ) 1 ;" ;

: AFTER-RESERVED$ ( -- ptr u8 n )
   s" create CKN-B : CKN-F ( -- n ) 1 ;" ;

: OPERAND-ADMITTED ( ptr u8 n ptr u8 n -- ) {: a:ptr u:n b:ptr v:n :}
   a u b v OPERAND-SRC$ HB-LOAD-SRC NOM-LOAD-ADMITTED
   a u b v OPERAND-SRC$ NOM-CHECK-ADMITTED ;

: OPERAND-REFUSED ( ptr u8 n ptr u8 n -- ) {: a:ptr u:n w:ptr wu:n :}
   a u AFTER-COMMENT$ OPERAND-SRC$ HB-LOAD-SRC
   {: outu:n erru:n rc:n :}
   rc 0 T<>
   CAP-ERR erru w wu CONTAINS? TTRUE
   a u AFTER-COMMENT$ OPERAND-SRC$ DIRECT-STDIN 70 T=
   {: outu2:n erru2:n :}
   outu2 0 T=
   CAP-ERR erru2 w wu CONTAINS? TTRUE ;

\ A DEFLINEAR or VALUE-RECORD name holding `(` or `\` can open a comment or a
\ line comment where source names the type again, so the loader refuses it
\ (checker.f TYPE-BAD-BYTE?). The check refuses it at the name the loader read,
\ not at the comment opening the next line: `at` is the packet's token, its
\ index and its place.
: NOM-OPERAND-REFUSED ( ptr u8 n ptr u8 n -- ) {: a:ptr u:n at:ptr atu:n :}
   a u AFTER-COMMENT$ OPERAND-SRC$ HB-LOAD-SRC NOM-LOAD-REFUSED
   a u AFTER-COMMENT$ OPERAND-SRC$ DIRECT-JSON-STDIN 70 T=
   {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru s\" \"code\":\"E-BAD-NOMINAL-TYPE\"" CONTAINS? TTRUE
   CAP-ERR erru at atu CONTAINS? TTRUE ;

: TEST-OPERAND-NAME-ADMITTED ( -- )
   s" package ( ;package" AFTER-COMMENT$ OPERAND-ADMITTED
   s" package \ ;package" AFTER-COMMENT$ OPERAND-ADMITTED
   s" : \ ( -- n ) 1 ;" AFTER-RESERVED$ OPERAND-ADMITTED
   s" create \" AFTER-RESERVED$ OPERAND-ADMITTED
   s" variable \" AFTER-RESERVED$ OPERAND-ADMITTED
   s" 1 constant \" AFTER-RESERVED$ OPERAND-ADMITTED ;

: TEST-OPERAND-NAME-REFUSED ( -- )
   s" DEFLINEAR ("
   s\" \"token\":\"(\",\"token_index\":1,\"file\":\"<stdin>\",\"line\":1,\"column\":11,"
   NOM-OPERAND-REFUSED
   s" VALUE-RECORD ( x n END-VALUE-RECORD"
   s\" \"token\":\"(\",\"token_index\":1,\"file\":\"<stdin>\",\"line\":1,\"column\":14,"
   NOM-OPERAND-REFUSED
   s" DEFLINEAR \"
   s\" \"token\":\"\\\\\",\"token_index\":1,\"file\":\"<stdin>\",\"line\":1,\"column\":11,"
   NOM-OPERAND-REFUSED
   s" VALUE-RECORD \ x n END-VALUE-RECORD"
   s\" \"token\":\"\\\\\",\"token_index\":1,\"file\":\"<stdin>\",\"line\":1,\"column\":14,"
   NOM-OPERAND-REFUSED
   s" require lib/type/deftype.f DEFTYPE (" s" '('" OPERAND-REFUSED
   s" require lib/type/deftype.f DEFTYPE \" s" '\'" OPERAND-REFUSED
   s" NEWTYPE ( 0" s" '('" OPERAND-REFUSED
   s" NEWTYPE \ 0" s" '\'" OPERAND-REFUSED
   s" SUMTYPE ( 0 VARIANT ckn-a ;VARIANT ;SUMTYPE" s" '('" OPERAND-REFUSED
   s" SUMTYPE \ 0 VARIANT ckn-a ;VARIANT ;SUMTYPE" s" '\'" OPERAND-REFUSED
   s" ENUM ( ckn-a ;ENUM" s" '('" OPERAND-REFUSED
   s" ENUM \ ckn-a ;ENUM" s" '\'" OPERAND-REFUSED
   s" STRUCTURE ( 0 FIELD x n ;STRUCTURE" s" '('" OPERAND-REFUSED
   s" STRUCTURE \ 0 FIELD x n ;STRUCTURE" s" '\'" OPERAND-REFUSED
   s" PRODUCT ( 0 FIELD x n ;PRODUCT" s" '('" OPERAND-REFUSED
   s" PRODUCT \ 0 FIELD x n ;PRODUCT" s" '\'" OPERAND-REFUSED ;

\ A definer, or `undefine`, with nothing after it has no name to read. Each
\ source puts one alone on its second line, two columns in: the loader refuses
\ the source, and the check refuses it in prose and in JSON at that place,
\ naming that word, without reading past the last token. Checked as a file,
\ whose statements are also walked to place the files it loads, it is refused
\ once.
: NONAME-SRC$ ( ptr u8 n ptr u8 n -- ptr u8 n ) {: a:ptr u:n d:ptr du:n :}
   SB-RESET
   a u SB-APPEND $0a SB-APPEND-C
   s"   " SB-APPEND
   d du SB-APPEND $0a SB-APPEND-C
   SB$ ;

: NONAME-REFUSED ( ptr u8 n ptr u8 n -- ) {: a:ptr u:n d:ptr du:n :}
   a u d du NONAME-SRC$ HB-LOAD-SRC
   {: outu:n erru:n rc:n :}
   rc 0 T<>
   a u d du NONAME-SRC$ DIRECT-STDIN 70 T=
   {: outu2:n erru2:n :}
   outu2 0 T=
   SB-RESET
   s" check.f: <stdin>:2:3: missing name after '" SB-APPEND
   d du SB-APPEND 39 SB-APPEND-C
   CAP-ERR erru2 SB$ CONTAINS? TTRUE
   a u d du NONAME-SRC$ DIRECT-JSON-STDIN 70 T=
   {: outu3:n erru3:n :}
   outu3 0 T=
   CAP-ERR erru3 s\" \"code\":\"E-MISSING-NAME\"" CONTAINS? TTRUE
   CAP-ERR erru3 s\" \"line\":2,\"column\":3," CONTAINS? TTRUE
   SB-RESET
   s\" \"word\":\"" SB-APPEND
   d du SB-APPEND $22 SB-APPEND-C
   CAP-ERR erru3 SB$ CONTAINS? TTRUE
   BAD$ PATH-RUN 70 T=
   {: outu4:n erru4:n :}
   outu4 0 T=
   CAP-ERR erru4 10 COUNT-CHAR 1 T= ;

: TEST-OPERAND-MISSING ( -- )
   s" require lib/type/deftype.f" s" DEFTYPE" NONAME-REFUSED
   s" \ lead" s" DEFLINEAR" NONAME-REFUSED
   s" \ lead" s" VALUE-RECORD" NONAME-REFUSED
   s" \ lead" s" NEWTYPE" NONAME-REFUSED
   s" \ lead" s" SUMTYPE" NONAME-REFUSED
   s" \ lead" s" ENUM" NONAME-REFUSED
   s" \ lead" s" STRUCTURE" NONAME-REFUSED
   s" \ lead" s" PRODUCT" NONAME-REFUSED
   s" \ lead" s" package" NONAME-REFUSED
   s" \ lead" s" :" NONAME-REFUSED
   s" \ lead" s" TRUSTED:" NONAME-REFUSED
   s" \ lead" s" undefine" NONAME-REFUSED ;

\ A parsing keyword takes the next whitespace-delimited token raw, whatever it
\ spells, in every scanner of the check as in the loader, and a definer takes
\ its name the same way; either operand is data. After `char \`, `[char] \`
\ and `char (` the second line is ordinary source, so the number-shaped
\ definition there is refused as it is anywhere, and so it is after `create
\ char`, which names a word `char` that takes nothing. `char s"` and `[char] s"`
\ open no string, so each source loads and checks alike and prints 115. A check
\ of standard input skips source discovery, so these sources are checked as the
\ file the load read. `' :` names no word, so the load refuses it as an
\ undefined tick, and the check refuses it the same way, never as a definition
\ with no name; it starts no definition, so the nominal pass reads the line after
\ it as top-level source. `[char] ;` ends none, so the pass does not read the
\ local `newtype` after it as a declaration.
: RAW-NUMERIC ( ptr u8 n ptr u8 n -- ) {: a:ptr u:n b:ptr v:n :}
   a u b v OPERAND-SRC$ HB-LOAD-SRC NOM-LOAD-ADMITTED
   a u b v OPERAND-SRC$ DIRECT-JSON-STDIN 1 T=
   {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru s" E-NUMERIC-DEFINITION" CONTAINS? TTRUE
   CAP-ERR erru s\" \"word\":\"42\"" CONTAINS? TTRUE ;

: RAW-PRINTS ( ptr u8 n -- ) {: a:ptr u:n :}
   a u HB-LOAD-SRC 0 T=
   {: outu:n erru:n :}
   erru 0 T=
   CAP-OUT outu s" 115" CONTAINS? TTRUE
   BAD$ PATH-RUN 0 T=
   {: outu2:n erru2:n :}
   erru2 0 T=
   CAP-OUT outu2 s" 115" CONTAINS? TTRUE ;

: RAW-ADMITTED ( ptr u8 n -- )
   HB-LOAD-SRC NOM-LOAD-ADMITTED
   BAD$ PATH-RUN EXPECT-ACCEPTED ;

: RAW-TICK-KEYWORD ( -- )
   s" ' :" HB-LOAD-SRC 70 T=
   {: outu:n erru:n :}
   CAP-ERR erru s" E-UNDEFINED: :" CONTAINS? TTRUE
   BAD$ PATH-RUN 70 T=
   {: outu2:n erru2:n :}
   CAP-ERR erru2 s" E-UNDEFINED: :" CONTAINS? TTRUE
   CAP-ERR erru2 s" missing name" CONTAINS? TFALSE ;

: RAW-TICK-LOAD-REFUSED ( n n n -- )
   70 T=
   {: outu:n erru:n :}
   CAP-ERR erru s" E-UNDEFINED: :" CONTAINS? TTRUE ;

: RAW-LOCAL$ ( -- ptr u8 n )
   s" : CKT-L ( n -- n ) {: newtype :} [char] ; drop newtype ;" ;

\ The last source reaches the pre-verifier's registration of `DEFLINEAR N` when
\ the nominal pass misses it, and that registration dies, ending this process.
: TEST-RAW-OPERAND ( -- )
   s" char \" s" : 42 ( -- ) ;" RAW-NUMERIC
   s" : CKT-P ( -- n ) [char] \" s" ; : 42 ( -- ) ;" RAW-NUMERIC
   s" char (" s" : 42 ( -- ) ;" RAW-NUMERIC
   s" create char" s" : 42 ( -- ) ;" RAW-NUMERIC
   s\" char s\" constant CKT-SQ CKT-SQ ." RAW-PRINTS
   s\" : CKT-Q ( -- n ) [char] s\" ; CKT-Q ." RAW-PRINTS
   RAW-TICK-KEYWORD
   RAW-LOCAL$ RAW-ADMITTED
   s" ' :" s" DEFLINEAR N" OPERAND-SRC$ HB-LOAD-SRC RAW-TICK-LOAD-REFUSED
   s" ' :" s" DEFLINEAR N" OPERAND-SRC$ NOM-CHECK-REFUSED ;

\ Value-records whose fields the registration refuses. The loader dies at the
\ first with the registration's message and rc 70. The check names every
\ refused field at its line and column and exits 70: a field of unknown type, a
\ duplicate field, a field named by effect syntax, a record with no field (named
\ at its END-VALUE-RECORD), then later findings of other kinds. The refused
\ record leaves nothing behind, so declaring its name again is no finding.
: VREC-FIELDS$ ( -- ptr u8 n )
   SB-RESET
   s" VALUE-RECORD ckn-r x n y bogus END-VALUE-RECORD" SB-APPEND $0a SB-APPEND-C
   s" VALUE-RECORD ckn-s z n z n END-VALUE-RECORD" SB-APPEND $0a SB-APPEND-C
   s" VALUE-RECORD ckn-t -- n END-VALUE-RECORD" SB-APPEND $0a SB-APPEND-C
   s" VALUE-RECORD ckn-u END-VALUE-RECORD" SB-APPEND $0a SB-APPEND-C
   s" VALUE-RECORD ckn-r x n END-VALUE-RECORD" SB-APPEND $0a SB-APPEND-C
   s" DEFLINEAR N" SB-APPEND $0a SB-APPEND-C
   s" PRODUCT" SB-APPEND
   SB$ ;

: TEST-VREC-FIELD-REFUSED ( -- )
   VREC-FIELDS$ HB-LOAD-SRC 70 T=
   {: outu:n erru:n :}
   CAP-ERR erru s" checker: bad value-record field type" CONTAINS? TTRUE
   VREC-FIELDS$ DIRECT-STDIN 70 T=
   {: outu2:n erru2:n :}
   outu2 0 T=
   CAP-ERR erru2 s" <stdin>:1:24: checker: bad value-record field type 'y'" CONTAINS? TTRUE
   CAP-ERR erru2 s" <stdin>:2:24: checker: duplicate value-record field 'z'" CONTAINS? TTRUE
   CAP-ERR erru2 s" <stdin>:3:20: checker: bad value-record field '--'" CONTAINS? TTRUE
   CAP-ERR erru2 s" <stdin>:4:20: checker: empty value-record 'END-VALUE-RECORD'" CONTAINS? TTRUE
   CAP-ERR erru2 s" 'ckn-r'" CONTAINS? TFALSE
   CAP-ERR erru2 s" check.f: bad nominal type 'N'" CONTAINS? TTRUE
   CAP-ERR erru2 s" <stdin>:7:1: missing name after 'PRODUCT'" CONTAINS? TTRUE
   VREC-FIELDS$ DIRECT-JSON-STDIN 70 T=
   {: outu3:n erru3:n :}
   outu3 0 T=
   CAP-ERR erru3 s\" \"code\":\"E-BAD-RECORD-FIELD\"" CONTAINS? TTRUE
   CAP-ERR erru3 s\" \"line\":2,\"column\":24," CONTAINS? TTRUE
   CAP-ERR erru3 s\" \"reason\":\"checker: duplicate value-record field\"" CONTAINS? TTRUE
   CAP-ERR erru3 s\" \"code\":\"E-MISSING-NAME\"" CONTAINS? TTRUE ;

: LINEAR-GOOD-TEST ( -- )
   LINEAR-GOOD$ DIRECT-STDIN 0 T=
   {: outu:n erru:n :}
   outu 0 T=
   erru 0 T= ;

: LINEAR-CROSS-TEST ( -- )
   LINEAR-CROSS$ DIRECT-JSON-STDIN 70 T=
   {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru s" E-UNDEFINED" CONTAINS? TTRUE
   CAP-ERR erru s" CKL-A-PRIV" CONTAINS? TTRUE ;

: LINEAR-GLOBAL-TEST ( -- )
   LINEAR-GLOBAL$ DIRECT-JSON-STDIN 70 T=
   {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru s" E-UNDEFINED" CONTAINS? TTRUE
   CAP-ERR erru s" CKL-HIDDEN-PRIV" CONTAINS? TTRUE ;

: LINEAR-DISTINCT-TEST ( -- )
   LINEAR-DISTINCT$ DIRECT-JSON-STDIN 70 T=
   {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru s" E-MISMATCH" CONTAINS? TTRUE
   CAP-ERR erru s" ckl-right-id" CONTAINS? TTRUE
   CAP-ERR erru s" ckl-left-id" CONTAINS? TTRUE ;

: FAM-GOOD-TEST ( -- )
   FAM-GOOD$ DIRECT-STDIN 0 T=
   {: outu:n erru:n :}
   outu 0 T=
   erru 0 T= ;

: FAM-TWO-TEST ( -- )
   FAM-TWO$ DIRECT-STDIN 0 T=
   {: outu:n erru:n :}
   outu 0 T=
   erru 0 T= ;

: FAM-BOGUS-TEST ( -- )
   FAM-BOGUS$ DIRECT-JSON-STDIN 70 T=
   {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru s" E-UNKNOWN-SIGNATURE-TYPE" CONTAINS? TTRUE
   CAP-ERR erru s" ckfno:ckffam" CONTAINS? TTRUE ;

: FAM-JSON-PIN ( -- )
   FAM-JSON$ DIRECT-JSON-STDIN 70 T=
   {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru s" E-MISMATCH" CONTAINS? TTRUE
   CAP-ERR erru s\" \"family\":\"ckfj:ckfjfam\"" CONTAINS? TTRUE ;

\ Every string a JSON packet carries is escaped, whatever spelled it. A package
\ may be named `\`, and its family then renders as `\:tail` in the effects, the
\ expected row and the family pin; a source label may hold a control byte. The
\ packet must parse and give back exactly those bytes.
: ESC-ROOT ( n -- n )
   CAP-ERR swap JSON-PARSE-TRY MATCH result
     ok  OF ENDOF
     err OF s" the JSON packet parses" T-LABEL 0 T= -1 ENDOF
   ;MATCH ;

: ESC-STR= ( n ptr u8 n ptr u8 n -- ) {: root:n k:ptr ku:n w:ptr wu:n :}
   root -1 = IF EXIT THEN
   root k ku JSON-GET {: node:n :}
   node -1 <> TTRUE
   node -1 = IF EXIT THEN
   node JSON-STRING$ w wu STR= TTRUE ;

: FAM-ESC-TEST ( -- )
   FAM-ESC$ DIRECT-JSON-STDIN 70 T=
   {: outu:n erru:n :}
   outu 0 T=
   erru ESC-ROOT {: root:n :}
   root s" declared_effect" s" n -- \:ckfefam " ESC-STR=
   root s" expected" s" \:ckfefam " ESC-STR=
   root s" family" s" \:ckfefam" ESC-STR= ;

: ESC-LABEL$ ( -- ptr u8 n )
   SB-RESET s" ck" SB-APPEND 1 SB-APPEND-C s" label.f" SB-APPEND SB$ ;

: LABEL-ESC-TEST ( -- )
   RESET
   s" json-errors" OPT
   s" : CKF-LBAD ( n -- ) dup ;" ESC-LABEL$ SOURCE
   [: RUN-ACT ;] IN-PROC 70 T=
   {: outu:n erru:n :}
   outu 0 T=
   erru ESC-ROOT s" file" ESC-LABEL$ ESC-STR= ;

: FAM-PRIV-TEST ( -- )
   FAM-PRIV$ DIRECT-JSON-STDIN 70 T=
   {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru s" E-UNKNOWN-SIGNATURE-TYPE" CONTAINS? TTRUE
   CAP-ERR erru s" ckfp:ckfpfam" CONTAINS? TTRUE ;

: TEST-REQUIRE-FACADE ( -- )
   s" require lib/test/suite.f" DIRECT-STDIN 0 T=
   {: outu:n erru:n :}
   erru 0 T=
   outu 0 T= ;

\ A dep loaded through a literal `s" path" included` is part of the closure:
\ the entry's use of the dep's word preverifies (the pre-producer scan captured
\ require/required only and rejected this good program).
: INC-ENTRY-SRC$ ( -- ptr u8 n )
   SB-RESET
   $73 SB-APPEND-C $22 SB-APPEND-C $20 SB-APPEND-C
   INC-DEP$ SB-APPEND
   $22 SB-APPEND-C
   s"  included" SB-APPEND $0a SB-APPEND-C
   s" : CKT-USE-EIGHT ( -- n ) CKT-EIGHT ;" SB-APPEND $0a SB-APPEND-C
   SB$ ;

: TEST-INCLUDED-DEP ( -- )
   INC-DEP$ s\" : CKT-EIGHT ( -- n ) 8 ;\n" WRITE-ALL
   INC-ENTRY$ INC-ENTRY-SRC$ WRITE-ALL
   INC-ENTRY$ PATH-RUN 0 T=
   {: outu:n erru:n :}
   outu 0 T=
   erru 0 T= ;

\ A require cycle: the root declares a deferred word and loads its host module
\ last; the host requires the root back and installs into that word, as
\ lib/aio.f and lib/aio-macos.f do. The host loads inside the root, after the
\ root's declarations, so preverify verifies the root before the host. The
\ entry stands outside the cycle, as test/gate-images.f stands outside aio's.
\ A host word of the wrong effect for the deferred word is still refused.
: SB-REQUIRED ( ptr u8 n -- )
   $73 SB-APPEND-C $22 SB-APPEND-C $20 SB-APPEND-C
   SB-APPEND
   $22 SB-APPEND-C
   s"  required" SB-APPEND $0a SB-APPEND-C ;

: CYC-ROOT-SRC$ ( -- ptr u8 n )
   SB-RESET
   s\" package CKT-CYC\nprivate\ndefer CKT-HOOK ( -- n )\n;package\n" SB-APPEND
   CYC-HOST$ SB-REQUIRED
   SB$ ;

: CYC-HOST-SRC$ ( ptr u8 n -- ptr u8 n ) {: def:ptr defu:n :}
   SB-RESET
   CYC-ROOT$ SB-REQUIRED
   s\" package CKT-CYC\nprivate\n" SB-APPEND
   def defu SB-APPEND
   s\" \n: CKT-INSTALL ( -- ) ['] CKT-SEVEN is CKT-HOOK ;\n;package\n" SB-APPEND
   SB$ ;

: CYC-RUN ( ptr u8 n -- n n n ) {: def:ptr defu:n :}
   CYC-ROOT$ CYC-ROOT-SRC$ WRITE-ALL
   CYC-HOST$ def defu CYC-HOST-SRC$ WRITE-ALL
   SB-RESET CYC-ROOT$ SB-REQUIRED
   CYC-ENTRY$ SB$ WRITE-ALL
   CYC-ENTRY$ PATH-RUN ;

: TEST-REQUIRE-CYCLE ( -- )
   s" : CKT-SEVEN ( -- n ) 7 ;" CYC-RUN 0 T=
   {: outu:n erru:n :}
   outu 0 T=
   erru 0 T=
   s" : CKT-SEVEN ( -- ) ;" CYC-RUN 70 T=
   {: outu2:n erru2:n :}
   outu2 0 T=
   CAP-ERR erru2 CYC-HOST$ CONTAINS? TTRUE
   CAP-ERR erru2 s" ckt-install" CONTAINS? TTRUE ;

\ SUMTYPE and PRODUCT define their constructors when they load. The pre-pass
\ replays the constructors' checked effects, so a definition in the same source
\ that constructs one preverifies, as lib/aio.f's OUTCOME-OF builds
\ AIO-OUTCOME:ready. A constructor left without its payload is still refused,
\ and by its effect, not as an undefined word.
: CTOR-SRC$ ( ptr u8 n -- ptr u8 n ) {: use:ptr useu:n :}
   SB-RESET
   s\" package CKTCT\npublic\nSUMTYPE ctout 0\n" SB-APPEND
   s\"    VARIANT ready n ;VARIANT\n   VARIANT gone ;VARIANT\n;SUMTYPE\n" SB-APPEND
   s\" PRODUCT ctpt 0\n   FIELD x n\n   FIELD y n\n;PRODUCT\nprivate\n" SB-APPEND
   use useu SB-APPEND
   s\" \n;package\n" SB-APPEND
   SB$ ;

: CTOR-GOOD$ ( -- ptr u8 n )
   s" : CKT-OUT ( n -- ctout ) CKTCT-CTOUT:ready ; : CKT-PT ( n n -- ctpt ) CKTCT-CTPT:MAKE ;"
   CTOR-SRC$ ;

: CTOR-BAD$ ( -- ptr u8 n )
   s" : CKT-OUT-BAD ( -- ctout ) CKTCT-CTOUT:ready ;" CTOR-SRC$ ;

: TEST-DECLARED-CONSTRUCTORS ( -- )
   CTOR-GOOD$ DIRECT-STDIN EXPECT-ACCEPTED
   CTOR-GOOD$ DIRECT-ALL-STDIN EXPECT-ACCEPTED
   CTOR-BAD$ DIRECT-STDIN {: outu:n erru:n rc:n :}
   outu erru rc s" ckt-out-bad" EXPECT-PREVERIFY-IN
   CAP-ERR erru s" E-UNDEFINED" CONTAINS? TFALSE ;

\ A PRODUCT or STRUCTURE that derives init also defines its initialized-field
\ accessors when it loads (src/core/structure-make.f), so the pre-pass replays
\ their effects with the constructors': a definition in the same source that
\ reads or writes a field through one preverifies in both modes, and one that
\ misdeclares the accessor's effect is refused by that effect, not as an
\ undefined word.
: INIT-ACC-SRC$ ( ptr u8 n -- ptr u8 n ) {: use:ptr useu:n :}
   SB-RESET
   s\" package CKTIA\npublic\nPRODUCT ckcell 0 DERIVE init FIELD v n ;PRODUCT\nprivate\n" SB-APPEND
   use useu SB-APPEND
   s\" \n;package\n" SB-APPEND
   SB$ ;

: INIT-ACC-GOOD$ ( -- ptr u8 n )
   s\" : CKT-GET ( mut-view<a,b,c,init<d,ckcell>> -- mut-view<a,b,c,init<d,ckcell>> n ) CKTIA-CKCELL:V@ ;\n: CKT-SET ( mut-view<a,b,c,init<d,ckcell>> n -- mut-view<a,b,c,init<d,ckcell>> ) CKTIA-CKCELL:V! ;"
   INIT-ACC-SRC$ ;

: INIT-ACC-BAD$ ( -- ptr u8 n )
   s" : CKT-GET-BAD ( mut-view<a,b,c,init<d,ckcell>> -- n ) CKTIA-CKCELL:V@ ;"
   INIT-ACC-SRC$ ;

: INIT-ACC-STRUCT$ ( -- ptr u8 n )
   s\" package CKTIS\npublic\nSTRUCTURE ckpair 0 DERIVE init FIELD first n FIELD second n ;STRUCTURE\nprivate\n: CKS-GET ( mut-view<a,b,c,init<d,ckpair>> -- mut-view<a,b,c,init<d,ckpair>> n ) CKTIS-CKPAIR:SECOND@ ;\n;package\n" ;

: TEST-DERIVED-INIT-ACCESSORS ( -- )
   INIT-ACC-GOOD$ DIRECT-STDIN EXPECT-ACCEPTED
   INIT-ACC-GOOD$ DIRECT-ALL-STDIN EXPECT-ACCEPTED
   INIT-ACC-STRUCT$ DIRECT-STDIN EXPECT-ACCEPTED
   INIT-ACC-STRUCT$ DIRECT-ALL-STDIN EXPECT-ACCEPTED
   INIT-ACC-BAD$ DIRECT-STDIN {: outu:n erru:n rc:n :}
   outu erru rc s" ckt-get-bad" EXPECT-PREVERIFY-IN
   CAP-ERR erru s" E-UNDEFINED" CONTAINS? TFALSE ;

\ ---- a loaded file is expanded where its loader sits ------------------------
\ The pre-pass verifies a source and the files it loads in the order the loader
\ runs them: the text before a top-level `required` first, then the file it
\ loads (once), then the rest. How that can fail, and what holds each way:
\   - the loaded file checked ahead of the whole source, so a word the source
\     defines before its require is undefined in it (REQ-ORDER, the reduced
\     case behind every library that requires lib/aio.f);
\   - the loaded file's words visible to the source before the require
\     (REQ-EARLY), or the source's later words visible to the loaded file
\     (REQ-LATE): the load path refuses both E-UNDEFINED, and so must this;
\   - a file expanded twice when two requires or two files name it (REQ-ONCE):
\     a second verification is E-DUPLICATE-DEFINITION;
\   - a require cycle looping or reordering (REQ-CYCLE-SPLIT): a file still
\     being expanded is a no-op, as `required` of a registered path is;
\   - a split inside a definition, which loses it. A loader in a colon body
\     runs when the word does, so its file expands after that definition and
\     any package around it (REQ-BODY, the shape of lib/aio.f's AIO-LOAD:HOST),
\     and after a top-level loader in that package, which runs first;
\   - a top-level loader inside a package or under a `using`, expanded once the
\     scope closes (at the end of the file, for a `using` never closed): the
\     loader runs the file in that scope, so its file and the rest of the
\     source are checked in the scope the loader gives them, and the file's
\     own usings end with it (REQ-PKG, REQ-USING);
\   - a diagnostic after a split naming the wrong file, line or column
\     (REQ-ORIGIN);
\   - the check run loading in another order: it loads through the real loader,
\     and every accepted case here runs it;
\   - `--all-errors --source-list` checking whole files in dependency order, or
\     replaying whole files as support: it checks the same segments in one
\     session, with the default check's verdict at the same file, line and
\     column (the -ALL cases);
\   - a refused definition taking the clean ones beside it down, so a later
\     segment reports them undefined (REQ-CASC): the session keeps every clean
\     definition and a refused one's declared signature, as checking the whole
\     file at once does;
\   - a duplicate definition in one segment leaving a record of the word that a
\     later segment's clean call is checked against (REQ-DUP): the duplicate
\     ends the check, as it ends the load and a whole-file check;
\   - plain `--all-errors` checking the subject alone, so a word it takes from a
\     file it loads is undefined (REQ-USE): it checks the same segments the
\     source list does, with the same verdict at the same place;
\   - a throw out of a statement in a later segment, such as a `;using` with
\     no `using` open, escaping all-errors uncaught or, in prose, unreported
\     (REQ-THROW): it is a diagnostic at the statement and the run ends with
\     the checker's status;
\   - a storage declaration naming an unknown type in a segment after a
\     refused one, reported as a throw out of the statement or at the wrong
\     file, line or column (REQ-SIZE): it is the storage refusal at the type
\     and the run ends with the checker's status;
\   - the subject reached again through a require: it is still being expanded,
\     so the pre-pass expands it once; the run loading its text a second time is
\     CHK-BUILD-RUN's (dot c79b86b7);
\   - a path resolved against the wrong directory: expansion keeps discovery's
\     resolution against the entry root;
\   - the closure capacity: one split per expanded file and one tail per file
\     keep the segments under twice CHK-DEP-MAX;
\   - check time: each verified file is lexed once more to place its splits.
\ The fixtures name each other by absolute path because the run loads the
\ subject's text from a temporary file, where a path relative to the subject's
\ directory does not resolve.

create REQ-PATH FS-PATH-CAP allot
variable REQ-U

: REQ$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}   \ a fixture in the test root
   ROOT$ a u REQ-PATH JOIN-PATH REQ-U !
   REQ-PATH REQ-U @ ;

: REQ-LINE+ ( ptr u8 n -- )
   SB-APPEND $0a SB-APPEND-C ;

: REQ-LIT+ ( ptr u8 n -- )   \ `s" <fixture path>"`
   REQ$ {: p:ptr pu:n :}
   $73 SB-APPEND-C $22 SB-APPEND-C $20 SB-APPEND-C
   p pu SB-APPEND
   $22 SB-APPEND-C ;

: REQ-LOAD+ ( ptr u8 n -- )
   REQ-LIT+ s"  required" REQ-LINE+ ;

: REQ-WRITE ( ptr u8 n -- )   \ the built text becomes the named fixture
   REQ$ SB$ WRITE-ALL ;

: REQ-RUN ( ptr u8 n -- n n n )
   REQ$ PATH-RUN ;

: REQ-ALL-OPTS ( -- )
   RESET
   s" all-errors" OPT
   s" json-errors" OPT ;

\ `--all-errors --source-list` checks the same segments, each against the ones
\ before it, and must give the default check's verdict at the same place.
: REQ-ALL-RUN ( ptr u8 n -- n n n )
   REQ$ {: p:ptr pu:n :}
   REQ-ALL-OPTS
   LIST-OPT
   p pu FILE
   [: RUN-ACT ;] IN-PROC ;

\ Plain `--all-errors` on the one file must give the same verdict.
: REQ-PLAIN-RUN ( ptr u8 n -- n n n )
   REQ$ {: p:ptr pu:n :}
   REQ-ALL-OPTS
   p pu FILE
   [: RUN-ACT ;] IN-PROC ;

: REQ-ORDER-FILES ( -- )
   SB-RESET s" : CKT-RQ-USE ( -- n ) CKT-RQ-SECRET ;" REQ-LINE+
   s" req-order-dep.f" REQ-WRITE
   SB-RESET s" : CKT-RQ-SECRET ( -- n ) 5 ;" REQ-LINE+
   s" req-order-dep.f" REQ-LOAD+
   s" req-order.f" REQ-WRITE ;

: TEST-REQUIRE-ORDER ( -- )
   REQ-ORDER-FILES
   s" req-order.f" REQ-RUN EXPECT-ACCEPTED ;

: TEST-REQUIRE-ORDER-ALL ( -- )
   REQ-ORDER-FILES
   s" req-order.f" REQ-ALL-RUN EXPECT-ACCEPTED ;

: TEST-REQUIRE-EARLY ( -- )
   SB-RESET s" : CKT-RQ-LATER ( -- n ) 6 ;" REQ-LINE+
   s" req-early-dep.f" REQ-WRITE
   SB-RESET s" : CKT-RQ-EARLY ( -- n ) CKT-RQ-LATER ;" REQ-LINE+
   s" req-early-dep.f" REQ-LOAD+
   s" req-early.f" REQ-WRITE
   s" req-early.f" REQ-RUN s" CKT-RQ-LATER" EXPECT-PREVERIFY-UNDEFINED ;

: REQ-LATE-FILES ( -- )
   SB-RESET s" : CKT-RQ-NEEDS ( -- n ) CKT-RQ-LATE ;" REQ-LINE+
   s" req-late-dep.f" REQ-WRITE
   SB-RESET s" req-late-dep.f" REQ-LOAD+
   s" : CKT-RQ-LATE ( -- n ) 7 ;" REQ-LINE+
   s" req-late.f" REQ-WRITE ;

: REQ-LATE-AT$ ( -- ptr u8 n )
   s\" req-late-dep.f\",\"line\":1,\"column\":25," ;

: TEST-REQUIRE-LATE ( -- )
   REQ-LATE-FILES
   s" req-late.f" REQ-RUN {: outu:n erru:n rc:n :}
   outu erru rc s" CKT-RQ-LATE" EXPECT-PREVERIFY-UNDEFINED
   CAP-ERR erru REQ-LATE-AT$ CONTAINS? TTRUE ;

: TEST-REQUIRE-LATE-ALL ( -- )
   REQ-LATE-FILES
   s" req-late.f" REQ-ALL-RUN 70 T= {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru s" E-UNDEFINED" CONTAINS? TTRUE
   CAP-ERR erru REQ-LATE-AT$ CONTAINS? TTRUE ;

: TEST-REQUIRE-ONCE ( -- )
   SB-RESET s" : CKT-RQ-ONE ( -- n ) 1 ;" REQ-LINE+
   s" req-once-dep.f" REQ-WRITE
   SB-RESET s" req-once-dep.f" REQ-LOAD+
   s" : CKT-RQ-MID ( -- n ) CKT-RQ-ONE 1 + ;" REQ-LINE+
   s" req-once-mid.f" REQ-WRITE
   SB-RESET s" req-once-dep.f" REQ-LOAD+
   s" req-once-mid.f" REQ-LOAD+
   s" req-once-dep.f" REQ-LOAD+
   s" : CKT-RQ-TWO ( -- n ) CKT-RQ-ONE CKT-RQ-MID + ;" REQ-LINE+
   s" req-once.f" REQ-WRITE
   s" req-once.f" REQ-RUN EXPECT-ACCEPTED ;

\ The subject stays out of the cycle: the run would load it a second time.
\ req-cycle-a.f is split at its loader; its tail uses req-cycle-b.f's word.
: TEST-REQUIRE-CYCLE-SPLIT ( -- )
   SB-RESET s" req-cycle-a.f" REQ-LOAD+
   s" : CKT-RQ-CY-B ( -- n ) CKT-RQ-CY-A 1 + ;" REQ-LINE+
   s" req-cycle-b.f" REQ-WRITE
   SB-RESET s" : CKT-RQ-CY-A ( -- n ) 1 ;" REQ-LINE+
   s" req-cycle-b.f" REQ-LOAD+
   s" : CKT-RQ-CY-C ( -- n ) CKT-RQ-CY-B 1 + ;" REQ-LINE+
   s" req-cycle-a.f" REQ-WRITE
   SB-RESET s" req-cycle-a.f" REQ-LOAD+
   s" : CKT-RQ-CY-USE ( -- n ) CKT-RQ-CY-C ;" REQ-LINE+
   s" req-cycle.f" REQ-WRITE
   s" req-cycle.f" REQ-RUN EXPECT-ACCEPTED ;

\ Expanded right after LOAD-HOST, the loaded file's `package` would nest in
\ CKT-RQ-LOAD; expanded at the end of the file, CKT-RQ-AFTER would miss its word.
\ The top-level loader after LOAD-HOST runs first, so its file expands first,
\ inside CKT-RQ-LOAD, where CKT-RQ-TOP-USE needs it.
: TEST-REQUIRE-BODY ( -- )
   SB-RESET s" : CKT-RQ-TOP ( -- n ) 2 ;" REQ-LINE+
   s" req-body-top.f" REQ-WRITE
   SB-RESET s" package CKT-RQ-HOST" REQ-LINE+
   s" public" REQ-LINE+
   s" : CKT-RQ-HOST-USE ( -- n ) CKT-RQ-HOOK 1 + ;" REQ-LINE+
   s" ;package" REQ-LINE+
   s" req-body-dep.f" REQ-WRITE
   SB-RESET s" package CKT-RQ-HOST" REQ-LINE+
   s" public" REQ-LINE+
   s" : CKT-RQ-HOOK ( -- n ) 7 ;" REQ-LINE+
   s" ;package" REQ-LINE+
   s" package CKT-RQ-LOAD" REQ-LINE+
   s" public" REQ-LINE+
   s" : LOAD-HOST ( -- ) " SB-APPEND s" req-body-dep.f" REQ-LIT+ s"  required ;" REQ-LINE+
   s" req-body-top.f" REQ-LOAD+
   s" : CKT-RQ-TOP-USE ( -- n ) CKT-RQ-TOP ;" REQ-LINE+
   s" ;package" REQ-LINE+
   s" CKT-RQ-LOAD:LOAD-HOST" REQ-LINE+
   s" : CKT-RQ-AFTER ( -- n ) CKT-RQ-HOST:CKT-RQ-HOST-USE ;" REQ-LINE+
   s" req-body.f" REQ-WRITE
   s" req-body.f" REQ-RUN EXPECT-ACCEPTED ;

\ The split falls after `required` on line 4, mid-line, so the refused `dup`
\ there is placed by the segment's line and column base.
: REQ-ORIGIN-FILES ( -- )
   SB-RESET s" : CKT-RQ-ORIGIN-DEP ( -- n ) 1 ;" REQ-LINE+
   s" req-origin-dep.f" REQ-WRITE
   SB-RESET $5c SB-APPEND-C s"  the loader below splits this file" REQ-LINE+
   s" : CKT-RQ-ORIGIN-OK ( -- n ) 1 ;" REQ-LINE+
   s" req-origin-dep.f" REQ-LIT+ $0a SB-APPEND-C
   s" required : CKT-RQ-ORIGIN-BAD ( n -- n ) dup ;" REQ-LINE+
   s" req-origin.f" REQ-WRITE ;

: REQ-ORIGIN-AT$ ( -- ptr u8 n )
   s\" req-origin.f\",\"line\":4,\"column\":41," ;

: TEST-REQUIRE-ORIGIN ( -- )
   REQ-ORIGIN-FILES
   s" req-origin.f" REQ-RUN {: outu:n erru:n rc:n :}
   outu erru rc EXPECT-PREVERIFY-REFUSED
   CAP-ERR erru REQ-ORIGIN-AT$ CONTAINS? TTRUE ;

: TEST-REQUIRE-ORIGIN-ALL ( -- )
   REQ-ORIGIN-FILES
   s" req-origin.f" REQ-ALL-RUN 70 T= {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru REQ-ORIGIN-AT$ CONTAINS? TTRUE ;

\ The subject calls a word only the file it loads defines; the bad one also
\ calls a word nothing defines, refused at the same place in every mode.
: REQ-USE-FILES ( -- )
   SB-RESET s" : CKT-RQ-GIVEN ( -- n ) 3 ;" REQ-LINE+
   s" req-use-dep.f" REQ-WRITE
   SB-RESET s" req-use-dep.f" REQ-LOAD+
   s" : CKT-RQ-TAKE ( -- n ) CKT-RQ-GIVEN 1 + ;" REQ-LINE+
   s" req-use.f" REQ-WRITE
   SB-RESET s" req-use-dep.f" REQ-LOAD+
   s" : CKT-RQ-MISS ( -- n ) CKT-RQ-GIVEN CKT-RQ-ABSENT + ;" REQ-LINE+
   s" req-use-bad.f" REQ-WRITE ;

: REQ-USE-AT$ ( -- ptr u8 n )
   s\" req-use-bad.f\",\"line\":2,\"column\":37," ;

: EXPECT-USE-UNDEFINED ( n n n -- )
   70 T= {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru s" E-UNDEFINED" CONTAINS? TTRUE
   CAP-ERR erru REQ-USE-AT$ CONTAINS? TTRUE ;

: TEST-REQUIRE-USE ( -- )
   REQ-USE-FILES
   s" req-use.f" REQ-RUN EXPECT-ACCEPTED
   s" req-use-bad.f" REQ-RUN EXPECT-USE-UNDEFINED ;

: TEST-REQUIRE-USE-ALL ( -- )
   REQ-USE-FILES
   s" req-use.f" REQ-PLAIN-RUN EXPECT-ACCEPTED
   s" req-use-bad.f" REQ-PLAIN-RUN EXPECT-USE-UNDEFINED ;

: TEST-REQUIRE-USE-ALL-LIST ( -- )
   REQ-USE-FILES
   s" req-use.f" REQ-ALL-RUN EXPECT-ACCEPTED
   s" req-use-bad.f" REQ-ALL-RUN EXPECT-USE-UNDEFINED ;

\ After the loaded file's refusal, the subject closes a `using` it never
\ opened, and the checker throws out of that statement (E-USING-UNBALANCED).
: REQ-THROW-FILES ( -- )
   SB-RESET s" : CKT-RQ-BROKEN ( n -- n n ) ;" REQ-LINE+
   s" req-throw-dep.f" REQ-WRITE
   SB-RESET s" req-throw-dep.f" REQ-LOAD+
   s" ;using" REQ-LINE+
   s" req-throw.f" REQ-WRITE ;

: REQ-THROW-AT$ ( -- ptr u8 n )
   s\" req-throw.f\",\"line\":2,\"column\":1," ;

\ The refusal is reported, then the throw at the statement it left.
: EXPECT-THROW-AT ( n n n ptr u8 n -- ) {: outu:n erru:n rc:n at:ptr atu:n :}
   rc 70 T=
   outu 0 T=
   CAP-ERR erru s" ckt-rq-broken" CONTAINS? TTRUE
   CAP-ERR erru at atu CONTAINS? TTRUE ;

: TEST-REQUIRE-THROW-ALL ( -- )
   REQ-THROW-FILES
   s" req-throw.f" REQ-PLAIN-RUN REQ-THROW-AT$ EXPECT-THROW-AT ;

: TEST-REQUIRE-THROW-ALL-LIST ( -- )
   REQ-THROW-FILES
   s" req-throw.f" REQ-ALL-RUN REQ-THROW-AT$ EXPECT-THROW-AT ;

: ALL-PROSE-RUN ( ptr u8 n -- n n n )
   REQ$ {: p:ptr pu:n :}
   RESET
   s" all-errors" OPT
   p pu FILE
   [: RUN-ACT ;] IN-PROC ;

: TEST-REQUIRE-THROW-PROSE ( -- )
   REQ-THROW-FILES
   s" req-throw.f" ALL-PROSE-RUN s" req-throw.f:2:1:" EXPECT-THROW-AT ;

\ After the loaded file's refusal, the subject sizes a buffer of a type nothing
\ declares, and the definer refuses the type there.
: REQ-SIZE-FILES ( -- )
   SB-RESET s" : CKT-RQ-BROKEN ( n -- n n ) ;" REQ-LINE+
   s" req-size-dep.f" REQ-WRITE
   SB-RESET s" req-size-dep.f" REQ-LOAD+
   s" 4 TYPED-BUFFER CKT-RQ-CELLS ckt-rq-cell" REQ-LINE+
   s" req-size.f" REQ-WRITE ;

: REQ-SIZE-AT$ ( -- ptr u8 n )
   s\" req-size.f\",\"line\":2,\"column\":29," ;

\ The first refusal is reported, then the storage refusal at the type it could
\ not size, which the given text names.
: EXPECT-SIZE-AT ( n n n ptr u8 n -- ) {: outu:n erru:n rc:n at:ptr atu:n :}
   rc 70 T=
   outu 0 T=
   CAP-ERR erru s" ckt-rq-broken" CONTAINS? TTRUE
   CAP-ERR erru at atu CONTAINS? TTRUE ;

: EXPECT-SIZE-JSON ( n n n -- ) {: outu:n erru:n rc:n :}
   outu erru rc REQ-SIZE-AT$ EXPECT-SIZE-AT
   CAP-ERR erru s\" \"code\":\"E-BAD-STORAGE\"" CONTAINS? TTRUE ;

: TEST-REQUIRE-SIZE-ALL ( -- )
   REQ-SIZE-FILES
   s" req-size.f" REQ-PLAIN-RUN EXPECT-SIZE-JSON ;

: TEST-REQUIRE-SIZE-ALL-LIST ( -- )
   REQ-SIZE-FILES
   s" req-size.f" REQ-ALL-RUN EXPECT-SIZE-JSON ;

: TEST-REQUIRE-SIZE-PROSE ( -- )
   REQ-SIZE-FILES
   s" req-size.f" ALL-PROSE-RUN
   s" req-size.f:2:29: habu: in CKT-RQ-CELLS: unknown type 'ckt-rq-cell'" EXPECT-SIZE-AT ;

\ The default mode reports a statement the checker throws out of as the record
\ --all-errors writes, at the statement, and fails with the checker's refusal
\ status. A source that requires the throwing file reports it in that file.
: ST-THROW-FILES ( -- )
   SB-RESET s" : CKT-ST-OK ( -- n ) 1 ;" REQ-LINE+
   s" ;using" REQ-LINE+
   s" st-throw.f" REQ-WRITE
   SB-RESET s" st-throw.f" REQ-LOAD+
   s" st-throw-req.f" REQ-WRITE ;

: ST-THROW-AT$ ( -- ptr u8 n )
   s\" st-throw.f\",\"line\":2,\"column\":1," ;

: EXPECT-ST-THROW ( n n n ptr u8 n -- ) {: outu:n erru:n rc:n at:ptr atu:n :}
   rc 70 T=
   outu 0 T=
   CAP-ERR erru s\" \"code\":\"E-STATEMENT-THROW\"" CONTAINS? TTRUE
   CAP-ERR erru s\" \"throw_code\":7142" CONTAINS? TTRUE
   CAP-ERR erru at atu CONTAINS? TTRUE ;

: ST-JSON-RUN ( ptr u8 n -- n n n )
   REQ$ {: p:ptr pu:n :}
   RESET
   s" json-errors" OPT
   p pu FILE
   [: RUN-ACT ;] IN-PROC ;

: TEST-STATEMENT-THROW-JSON ( -- )
   ST-THROW-FILES
   s" st-throw.f" ST-JSON-RUN ST-THROW-AT$ EXPECT-ST-THROW
   s" st-throw-req.f" ST-JSON-RUN ST-THROW-AT$ EXPECT-ST-THROW ;

: TEST-STATEMENT-THROW-PROSE ( -- )
   ST-THROW-FILES
   s" st-throw.f" REQ-RUN {: outu:n erru:n rc:n :}
   rc 70 T=
   outu 0 T=
   CAP-ERR erru s" E-STATEMENT-THROW " CONTAINS? TTRUE
   CAP-ERR erru s" st-throw.f:2:1: throw 7142 at ';using'" CONTAINS? TTRUE ;

\ A string the file never closes stops source discovery before any segment
\ exists. Every --json-errors mode reports it by the one record --all-errors
\ writes for it on standard input, in the file that holds it and at the string,
\ and fails as a refusal; a source that requires the file reports it there. The
\ record's token is the opener as written, so an escaped opener spans 3 bytes.
: UT-FILES ( -- )
   s" ckt-ut.f" REQ$ UNTERM-SDQ$ WRITE-ALL
   s" ckt-ut-esc.f" REQ$ UNTERM-ESC$ WRITE-ALL
   SB-RESET s" ckt-ut.f" REQ-LOAD+
   s" ckt-ut-req.f" REQ-WRITE ;

: EXPECT-UT ( n n n ptr u8 n ptr u8 n -- )
   {: outu:n erru:n rc:n tok:ptr toku:n at:ptr atu:n :}
   rc 70 T=
   outu 0 T=
   CAP-ERR erru 10 COUNT-CHAR 1 T=
   CAP-ERR erru s\" \"code\":\"E-UNTERMINATED-STRING\",\"repair_class\":\"close_string\"," CONTAINS? TTRUE
   CAP-ERR erru tok toku CONTAINS? TTRUE
   CAP-ERR erru at atu CONTAINS? TTRUE ;

: UT-MODES ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: a:ptr u:n tok:ptr toku:n at:ptr atu:n :}
   a u ST-JSON-RUN tok toku at atu EXPECT-UT
   a u REQ-PLAIN-RUN tok toku at atu EXPECT-UT
   a u REQ-ALL-RUN tok toku at atu EXPECT-UT ;

: UT-SDQ-TOKEN$ ( -- ptr u8 n )
   s\" \"token\":\"s\\\"\",\"file\":" ;

: UT-SDQ-AT$ ( -- ptr u8 n )
   s\" /ckt-ut.f\",\"line\":1,\"column\":34,\"byte_start\":33,\"byte_end\":35," ;

: UT-ESC-TOKEN$ ( -- ptr u8 n )
   s\" \"token\":\"s\\\\\\\"\",\"file\":" ;

: UT-ESC-AT$ ( -- ptr u8 n )
   s\" /ckt-ut-esc.f\",\"line\":1,\"column\":34,\"byte_start\":33,\"byte_end\":36," ;

: TEST-UNTERM-STRING ( -- )
   UT-FILES
   s" ckt-ut.f" UT-SDQ-TOKEN$ UT-SDQ-AT$ UT-MODES
   s" ckt-ut-req.f" UT-SDQ-TOKEN$ UT-SDQ-AT$ UT-MODES
   s" ckt-ut-esc.f" UT-ESC-TOKEN$ UT-ESC-AT$ UT-MODES ;

\ The run refuses what the checker leaves to it: a `using` of a package nothing
\ defines dies in the engine, which names the file and line it is reading. The
\ run reads a copy of the subject with an origin marker before each definition,
\ and the report still names the subject at the line the statement sits on, in
\ prose and under --json-errors, for a file and for a source given by label.
: USING-AT-LINES ( -- )
   s" : CKT-UA-ONE ( -- n ) 1 ;" REQ-LINE+
   $0a SB-APPEND-C
   s" : CKT-UA-TWO ( -- n ) 2 ;" REQ-LINE+
   s" using CKT-UA-NOPE" REQ-LINE+ ;

\ The engine's refusal of the `using` on line 4 of the named source.
: USING-AT$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   SB-RESET
   s" hb: using: unknown package: CKT-UA-NOPE at " SB-APPEND
   a u SB-APPEND
   s" :4" SB-APPEND
   SB$ ;

: EXPECT-USING-AT ( n n n ptr u8 n -- ) {: outu:n erru:n rc:n at:ptr atu:n :}
   rc ENGINE-ERROR:USING-UNKNOWN T=
   outu 0 T=
   CAP-ERR erru at atu USING-AT$ CONTAINS? TTRUE ;

: TEST-USING-AT-SOURCE ( -- )
   SB-RESET USING-AT-LINES s" using-at.f" REQ-WRITE
   s" using-at.f" REQ-RUN s" using-at.f" REQ$ EXPECT-USING-AT
   s" using-at.f" ST-JSON-RUN s" using-at.f" REQ$ EXPECT-USING-AT
   RESET
   SB-RESET USING-AT-LINES SB$ s" ckt-using-at.f" SOURCE
   [: RUN-ACT ;] IN-PROC s" ckt-using-at.f" EXPECT-USING-AT ;

\ A source whose first line starts with `#!` is a script: the engine reads that
\ line as a comment in a file it runs or loads. check.f checks such a subject as
\ the engine loads it - clean in prose, under --json-errors, from standard input
\ and in a source list, where each file's own first line counts - and reports a
\ refusal at the subject's own line. A `#!` on any later line is an ordinary
\ token, which the engine refuses.
: SHEBANG+ ( -- )
   s" #!/usr/bin/env hb" REQ-LINE+ ;

: SHEBANG-ONE-LINES ( -- )
   SHEBANG+
   s" : CKT-SB-ONE ( -- n ) 1 ;" REQ-LINE+
   s" CKT-SB-ONE drop" REQ-LINE+ ;

\ The `using` stays on line 4, where EXPECT-USING-AT looks for it.
: SHEBANG-USING-LINES ( -- )
   SHEBANG+
   s" : CKT-UA-ONE ( -- n ) 1 ;" REQ-LINE+
   s" : CKT-UA-TWO ( -- n ) 2 ;" REQ-LINE+
   s" using CKT-UA-NOPE" REQ-LINE+ ;

: SHEBANG-FILES ( -- )
   SB-RESET SHEBANG-ONE-LINES s" sb-one.f" REQ-WRITE
   SB-RESET SHEBANG+ s" : CKT-SB-TWO ( -- n ) 2 ;" REQ-LINE+ s" sb-two.f" REQ-WRITE
   SB-RESET SHEBANG-USING-LINES s" sb-using.f" REQ-WRITE
   SB-RESET s" : CKT-SB-LATE ( -- n ) 3 ;" REQ-LINE+ SHEBANG+ s" sb-later.f" REQ-WRITE ;

: SHEBANG-LIST-RUN ( ptr u8 n ptr u8 n -- n n n ) {: a:ptr au:n b:ptr bu:n :}
   RESET
   LIST-OPT
   a au REQ$ FILE
   b bu REQ$ FILE
   [: RUN-ACT ;] IN-PROC ;

: EXPECT-SHEBANG-LATER ( n n n -- ) {: outu:n erru:n rc:n :}
   rc 70 T=
   outu 0 T=
   CAP-ERR erru s" E-UNDEFINED: #!/usr/bin/env" CONTAINS? TTRUE ;

: TEST-SHEBANG-CLEAN ( -- )
   SHEBANG-FILES
   s" sb-one.f" REQ-RUN EXPECT-ACCEPTED
   s" sb-one.f" ST-JSON-RUN EXPECT-ACCEPTED
   SB-RESET SHEBANG-ONE-LINES SB$ CLI-STDIN EXPECT-ACCEPTED
   SB-RESET SHEBANG-ONE-LINES SB$ DIRECT-STDIN EXPECT-ACCEPTED
   s" sb-one.f" s" sb-two.f" SHEBANG-LIST-RUN EXPECT-ACCEPTED ;

: TEST-SHEBANG-AT-LINE ( -- )
   SHEBANG-FILES
   s" sb-using.f" REQ-RUN s" sb-using.f" REQ$ EXPECT-USING-AT
   s" sb-using.f" ST-JSON-RUN s" sb-using.f" REQ$ EXPECT-USING-AT
   SB-RESET SHEBANG-USING-LINES SB$ CLI-STDIN STDIN-LABEL$ EXPECT-USING-AT
   s" sb-one.f" s" sb-using.f" SHEBANG-LIST-RUN s" sb-using.f" REQ$ EXPECT-USING-AT ;

: TEST-SHEBANG-LATER ( -- )
   SHEBANG-FILES
   s" sb-later.f" REQ-RUN EXPECT-SHEBANG-LATER
   s" sb-one.f" s" sb-later.f" SHEBANG-LIST-RUN EXPECT-SHEBANG-LATER ;

\ A checker capacity fault is no storage refusal: a DYNAMIC-BUFFER whose derived
\ names overrun the checker's name buffer (src/core/checker.f LBUF-NM-CAP)
\ throws E-CHECKER-LAYOUT-BUFFER (7121) out of its statement, at the token the
\ checker read last. The 250-byte name has a line of its own, under the engine's
\ 255-byte line.
: CAP-THROW-FILES ( -- )
   SB-RESET s" DYNAMIC-BUFFER" REQ-LINE+
   s" CKT-CT-" SB-APPEND
   243 0 ?do $4e SB-APPEND-C loop
   $0a SB-APPEND-C
   s" n" REQ-LINE+
   s" cap-throw.f" REQ-WRITE ;

: TEST-CAPACITY-THROW ( -- )
   CAP-THROW-FILES
   s" cap-throw.f" REQ-PLAIN-RUN 70 T= {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru s\" \"code\":\"E-STATEMENT-THROW\"" CONTAINS? TTRUE
   CAP-ERR erru s\" \"token\":\"n\"" CONTAINS? TTRUE
   CAP-ERR erru s\" cap-throw.f\",\"line\":3,\"column\":1," CONTAINS? TTRUE
   CAP-ERR erru s\" \"throw_code\":7121" CONTAINS? TTRUE ;

\ A top-level loader inside a package, or under a file-level `using`, runs its
\ file in that scope: the file defines into the package and sees the package's
\ words and the used publics, and the rest of the source sees the file's words.
\ Each source is checked in the default mode and both all-errors modes; each
\ bad one calls a word the load path leaves undefined, refused at its own place.
: EXPECT-UNDEFINED-AT ( n n n ptr u8 n -- ) {: outu:n erru:n rc:n at:ptr atu:n :}
   rc 70 T=
   outu 0 T=
   CAP-ERR erru s" E-UNDEFINED" CONTAINS? TTRUE
   CAP-ERR erru at atu CONTAINS? TTRUE ;

: REQ-ACCEPTED-EVERY ( ptr u8 n -- ) {: f:ptr fu:n :}
   f fu REQ-RUN EXPECT-ACCEPTED
   f fu REQ-PLAIN-RUN EXPECT-ACCEPTED
   f fu REQ-ALL-RUN EXPECT-ACCEPTED ;

: REQ-UNDEFINED-EVERY ( ptr u8 n ptr u8 n -- ) {: f:ptr fu:n at:ptr atu:n :}
   f fu REQ-RUN at atu EXPECT-UNDEFINED-AT
   f fu REQ-PLAIN-RUN at atu EXPECT-UNDEFINED-AT
   f fu REQ-ALL-RUN at atu EXPECT-UNDEFINED-AT ;

: REQ-PKG+ ( ptr u8 n -- ) {: line:ptr lineu:n :}   \ the given line follows the loader
   SB-RESET s" package CKT-RQ-PK" REQ-LINE+
   s" public" REQ-LINE+
   s" : CKT-RQ-PK-W ( -- n ) 1 ;" REQ-LINE+
   s" req-pkg-dep.f" REQ-LOAD+
   line lineu REQ-LINE+
   s" ;package" REQ-LINE+ ;

\ The bad sources: a word nothing defines, and outside the package the loaded
\ file's word, which the package holds.
: REQ-PKG-FILES ( -- )
   SB-RESET s" : CKT-RQ-PK-DEP ( -- n ) CKT-RQ-PK-W 1 + ;" REQ-LINE+
   s" req-pkg-dep.f" REQ-WRITE
   s" : CKT-RQ-PK-USE ( -- n ) CKT-RQ-PK-DEP CKT-RQ-PK-W + ;" REQ-PKG+
   s" req-pkg.f" REQ-WRITE
   s" : CKT-RQ-PK-MISS ( -- n ) CKT-RQ-PK-DEP CKT-RQ-PK-ABSENT + ;" REQ-PKG+
   s" req-pkg-bad.f" REQ-WRITE
   s" : CKT-RQ-PK-USE ( -- n ) CKT-RQ-PK-DEP CKT-RQ-PK-W + ;" REQ-PKG+
   s" : CKT-RQ-PK-OUT ( -- n ) CKT-RQ-PK-DEP ;" REQ-LINE+
   s" req-pkg-out.f" REQ-WRITE ;

: TEST-REQUIRE-PACKAGE ( -- )
   REQ-PKG-FILES
   s" req-pkg.f" REQ-ACCEPTED-EVERY
   s" req-pkg-bad.f" s\" req-pkg-bad.f\",\"line\":5,\"column\":41," REQ-UNDEFINED-EVERY
   s" req-pkg-out.f" s\" req-pkg-out.f\",\"line\":7,\"column\":26," REQ-UNDEFINED-EVERY ;

: REQ-USING+ ( ptr u8 n -- ) {: line:ptr lineu:n :}   \ the given line follows the loader
   SB-RESET s" package CKT-RQ-US" REQ-LINE+
   s" public" REQ-LINE+
   s" : CKT-RQ-US-W ( -- n ) 1 ;" REQ-LINE+
   s" ;package" REQ-LINE+
   s" using CKT-RQ-US" REQ-LINE+
   s" req-using-dep.f" REQ-LOAD+
   line lineu REQ-LINE+ ;

\ The `using` is never closed. A file's own `using` ends with the file, so the
\ source that loads one does not see what it imported (req-ulocal.f).
: REQ-USING-FILES ( -- )
   SB-RESET s" : CKT-RQ-US-DEP ( -- n ) CKT-RQ-US-W 1 + ;" REQ-LINE+
   s" req-using-dep.f" REQ-WRITE
   s" : CKT-RQ-US-USE ( -- n ) CKT-RQ-US-DEP CKT-RQ-US-W + ;" REQ-USING+
   s" req-using.f" REQ-WRITE
   s" : CKT-RQ-US-MISS ( -- n ) CKT-RQ-US-DEP CKT-RQ-US-ABSENT + ;" REQ-USING+
   s" req-using-bad.f" REQ-WRITE
   SB-RESET s" using CKT-RQ-UL" REQ-LINE+
   s" : CKT-RQ-UL-DEP ( -- n ) CKT-RQ-UL-W ;" REQ-LINE+
   s" req-ulocal-dep.f" REQ-WRITE
   SB-RESET s" package CKT-RQ-UL" REQ-LINE+
   s" public" REQ-LINE+
   s" : CKT-RQ-UL-W ( -- n ) 1 ;" REQ-LINE+
   s" ;package" REQ-LINE+
   s" req-ulocal-dep.f" REQ-LOAD+
   s" : CKT-RQ-UL-LEAK ( -- n ) CKT-RQ-UL-W ;" REQ-LINE+
   s" req-ulocal.f" REQ-WRITE ;

: TEST-REQUIRE-USING ( -- )
   REQ-USING-FILES
   s" req-using.f" REQ-ACCEPTED-EVERY
   s" req-using-bad.f" s\" req-using-bad.f\",\"line\":7,\"column\":41," REQ-UNDEFINED-EVERY
   s" req-ulocal.f" s\" req-ulocal.f\",\"line\":6,\"column\":27," REQ-UNDEFINED-EVERY ;

\ A refused definition takes nothing else down with it: the text after the
\ loader still sees the clean definition before the refused one, so the
\ refusal is the only diagnostic in both all-errors modes, as it is when the
\ whole file is checked at once.
: REQ-CASC-FILES ( -- )
   SB-RESET s" : CKT-RQ-CA-DEP ( -- n ) 2 ;" REQ-LINE+
   s" req-casc-dep.f" REQ-WRITE
   SB-RESET s" : CKT-RQ-CA-ONE ( -- n ) 1 ;" REQ-LINE+
   s" : CKT-RQ-CA-BAD ( -- n ) dup ;" REQ-LINE+
   s" req-casc-dep.f" REQ-LOAD+
   s" : CKT-RQ-CA-TWO ( -- n ) CKT-RQ-CA-ONE ;" REQ-LINE+
   s" : CKT-RQ-CA-THREE ( -- n ) CKT-RQ-CA-DEP ;" REQ-LINE+
   s" req-casc.f" REQ-WRITE ;

: EXPECT-ONE-UNDERFLOW ( n n n -- )
   70 T= {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru s" E-INPUT-UNDERFLOW" CONTAINS? TTRUE
   CAP-ERR erru s\" req-casc.f\",\"line\":2," CONTAINS? TTRUE
   CAP-ERR erru $0a COUNT-CHAR 1 T= ;

: TEST-REQUIRE-CASCADE ( -- )
   REQ-CASC-FILES
   s" req-casc.f" REQ-PLAIN-RUN EXPECT-ONE-UNDERFLOW
   s" req-casc.f" REQ-ALL-RUN EXPECT-ONE-UNDERFLOW ;

\ The loaded file defines again a word the subject defined before its loader.
\ The duplicate ends the check as the loader ends the load, so the clean
\ definition after the loader that calls the word is not checked against what
\ the duplicate left behind.
: REQ-DUP-FILES ( -- )
   SB-RESET s" : CKT-RQ-DU-X ( -- n ) 2 ;" REQ-LINE+
   s" req-dup-dep.f" REQ-WRITE
   SB-RESET s" : CKT-RQ-DU-X ( -- n ) 1 ;" REQ-LINE+
   s" req-dup-dep.f" REQ-LOAD+
   s" : CKT-RQ-DU-Y ( -- n ) CKT-RQ-DU-X ;" REQ-LINE+
   s" req-dup.f" REQ-WRITE ;

: EXPECT-ONE-DUPLICATE ( n n n -- )
   CHECK-ALL-ERRORS:DUP-RC T= {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru s" E-DUPLICATE-DEFINITION" CONTAINS? TTRUE
   CAP-ERR erru s\" req-dup-dep.f\",\"line\":1,\"column\":1," CONTAINS? TTRUE
   CAP-ERR erru $0a COUNT-CHAR 1 T= ;

: TEST-REQUIRE-DUPLICATE ( -- )
   REQ-DUP-FILES
   s" req-dup.f" ST-JSON-RUN EXPECT-ONE-DUPLICATE
   s" req-dup.f" REQ-PLAIN-RUN EXPECT-ONE-DUPLICATE
   s" req-dup.f" REQ-ALL-RUN EXPECT-ONE-DUPLICATE ;

\ In prose the duplicate's line names the file that defined the word again,
\ as the JSON record does, in the default mode and under --all-errors.
: REQ-DUP-PROSE$ ( -- ptr u8 n )
   s" req-dup-dep.f" REQ$ {: p:ptr pu:n :}
   SB-RESET
   s" checker: duplicate definition in " SB-APPEND
   p pu SB-APPEND
   SB$ ;

: EXPECT-DUP-PROSE ( n n n -- )
   CHECK-ALL-ERRORS:DUP-RC T= {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru REQ-DUP-PROSE$ CONTAINS? TTRUE ;

: TEST-REQUIRE-DUPLICATE-PROSE ( -- )
   REQ-DUP-FILES
   s" req-dup.f" REQ-RUN EXPECT-DUP-PROSE
   s" req-dup.f" ALL-PROSE-RUN EXPECT-DUP-PROSE ;

\ A storage declaration whose definer cannot size its type is the checker's
\ refusal at the type, in every mode, for every definer that sizes one (dot
\ 2eb1290e), not a throw out of the pre-pass, which reads a DEFER-LAYOUT-BUFFER
\ line too. Each declaration below puts its type at column 41.
: STG-TB$ ( -- ptr u8 n )
   s" 4 TYPED-BUFFER CKT-STG-TB               ckt-stg-none" ;

: STG-TV$ ( -- ptr u8 n )
   s" TYPED-VARIABLE CKT-STG-TV               ckt-stg-none" ;

: STG-LB$ ( -- ptr u8 n )
   s" 4 LAYOUT-BUFFER CKT-STG-LB              ckt-stg-none" ;

: STG-DB$ ( -- ptr u8 n )
   s" DYNAMIC-BUFFER CKT-STG-DB               ckt-stg-none" ;

: STG-DL$ ( -- ptr u8 n )
   s" DEFER-LAYOUT-BUFFER CKT-STG-DL          ckt-stg-none" ;

: STG-AT$ ( -- ptr u8 n )
   s\" stg.f\",\"line\":1,\"column\":41," ;

: EXPECT-STG-JSON ( n n n -- )
   70 T= {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru s\" \"code\":\"E-BAD-STORAGE\"" CONTAINS? TTRUE
   CAP-ERR erru s\" \"token\":\"ckt-stg-none\"" CONTAINS? TTRUE
   CAP-ERR erru STG-AT$ CONTAINS? TTRUE ;

: EXPECT-STG-PROSE ( n n n -- )
   70 T= {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru s" stg.f:1:41: habu: in CKT-STG-" CONTAINS? TTRUE
   CAP-ERR erru s" : unknown type 'ckt-stg-none'" CONTAINS? TTRUE ;

: STG-CASE ( ptr u8 n -- )
   SB-RESET REQ-LINE+ s" stg.f" REQ-WRITE
   s" stg.f" REQ-RUN EXPECT-STG-JSON
   s" stg.f" REQ-PLAIN-RUN EXPECT-STG-JSON
   s" stg.f" REQ-ALL-RUN EXPECT-STG-JSON
   s" stg.f" REQ$ LIST-RUN EXPECT-STG-JSON
   s" stg.f" ALL-PROSE-RUN EXPECT-STG-PROSE ;

: TEST-STORAGE-TYPE ( -- )
   STG-TB$ STG-CASE
   STG-TV$ STG-CASE
   STG-LB$ STG-CASE
   STG-DB$ STG-CASE
   STG-DL$ STG-CASE ;

\ The pre-pass registers what DEFER-LAYOUT-BUFFER publishes, so a definition
\ calling the accessor or either binder certifies, where each was E-UNDEFINED.
: STG-DEFER-GOOD$ ( -- ptr u8 n )
   s" NEWTYPE ckt-dk 0 DEFER-LAYOUT-BUFFER CKT-DK ckt-dk : CKT-DK-AT ( n -- ptr ckt-dk ) CKT-DK ; : CKT-DK-SIZE ( n -- ) CKT-DK-BIND ; : CKT-DK-MORE ( n -- ) CKT-DK-GROW ;" ;

: TEST-STORAGE-DEFER ( -- )
   STG-DEFER-GOOD$ DIRECT-STDIN EXPECT-ACCEPTED ;

\ The definer's name is refused through the same diagnostic, at the name.
: TEST-STORAGE-NAME ( -- )
   SB-RESET s" 4 TYPED-BUFFER CKT:STG:TB n" REQ-LINE+ s" stg.f" REQ-WRITE
   s" stg.f" REQ-RUN 70 T= {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru s\" \"token\":\"CKT:STG:TB\"" CONTAINS? TTRUE
   CAP-ERR erru s\" stg.f\",\"line\":1,\"column\":16," CONTAINS? TTRUE ;

\ `--all-errors --source-list a b` runs all-errors on the ORIGINAL files'
\ segments, in order, in one session: both bad defs in b report against b's
\ path (the pre-redrive materialized temp had zero defs, so all-errors was a
\ no-op and only preverify's first error surfaced).
: LIST-ALL-TEST ( -- )
   SUP$ s\" : CKT-SEVEN ( -- n ) 7 ;\n" WRITE-ALL
   USE$ s\" : CKT-GOOD-USE ( -- n ) CKT-SEVEN ;\n: CKT-BAD-ONE ( -- n n ) CKT-SEVEN ;\n: CKT-BAD-TWO ( n -- ) CKT-SEVEN ;\n" WRITE-ALL
   SUP$ USE$ CLI-ALL-LIST 70 T=
   {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru USE$ CONTAINS? TTRUE
   CAP-ERR erru s" ckt-bad-one" CONTAINS? TTRUE
   CAP-ERR erru s" ckt-bad-two" CONTAINS? TTRUE
   CAP-ERR erru s" E-UNDEFINED" CONTAINS? TFALSE
   CAP-ERR erru s" ckt-good-use" CONTAINS? TFALSE
   CAP-ERR erru s" preverify" CONTAINS? TFALSE ;

\ A check run in this process must give the verdict `bin/hb tools/check.f`
\ gives the same file. The pre-pass learns a `does>` definer by its checker
\ symbol, and a check's symbols go when its scope ends, so the next check hands
\ the same id to its own first word. After agree.f made CKT-AG-D a definer, the
\ edit that makes it a plain word is refused where the command line refuses it:
\ in the pre-pass, at CKT-AG-USE. A definer row that outlived its check read the
\ top-level call of CKT-AG-D as a definer creating the next token, the `:`, so
\ the pre-pass never read CKT-AG-USE and the run refused it instead.
create AGREE-ERR BUF-CAP allot
variable AGREE-ERR-U

: AGREE-DEFINER ( -- )
   SB-RESET s" : CKT-AG-D ( n -- ) create , does> ( -- n ) @ ;" REQ-LINE+
   s" agree.f" REQ-WRITE ;

: AGREE-PLAIN ( -- )
   SB-RESET s" : CKT-AG-D ( -- ) ;" REQ-LINE+
   s" CKT-AG-D" REQ-LINE+
   s" : CKT-AG-USE ( -- n ) CKT-AG-NOPE ;" REQ-LINE+
   s" agree.f" REQ-WRITE ;

: TEST-REPEAT-DEFINER ( -- )
   AGREE-DEFINER
   s" agree.f" REQ-RUN EXPECT-ACCEPTED
   AGREE-PLAIN
   s" agree.f" REQ-RUN {: outu:n erru:n rc:n :}
   CAP-ERR AGREE-ERR erru BYTE-COPY  erru AGREE-ERR-U !
   outu erru rc s" CKT-AG-NOPE" EXPECT-PREVERIFY-UNDEFINED
   s" agree.f" REQ$ CLI-PATH {: cliu:n clie:n clirc:n :}
   rc clirc T=
   outu cliu T=
   AGREE-ERR AGREE-ERR-U @ CAP-ERR clie T$= ;

\ A capture whose deadlock guard expires throws E-PROC-TIMEOUT from inside the
\ process library, which used to leave the run with nothing but `hb: uncaught
\ throw code -2502`: it named no case, no child and no budget, so a hung child
\ and a slow one looked identical from the gate log. CASE-RUN is the one place
\ that knows which case is running, so it is where that verdict gets its name.
\ The code then goes on uncaught, so the engine's report stays the row's last
\ stderr line and the gate pool labels the row TIMEOUT-UNDER-LOAD
\ (test/gate-pool.f GT-POOL-INNER-TIMEOUT?). Every other throw keeps
\ propagating untouched.
: CASE-HUNG ( ptr u8 n -- ) {: label:ptr labelu:n :}
   s" FAIL: " type label labelu type
   s"  - child never exited; deadlock guard ms: " type CHILD-HANG-MS FMT:.INT cr
   E-PROC-TIMEOUT throw ;

: CASE-THREW ( ptr u8 n n -- ) {: label:ptr labelu:n rc:n :}
   rc 0= if exit then
   rc E-PROC-TIMEOUT <> if rc throw then
   label labelu CASE-HUNG ;

: CASE-RUN ( ptr u8 n [ -- ] -- ) {: label:ptr labelu:n q :}
   mono-ns START-NS !
   q catch {: rc:n :}
   label labelu rc CASE-THREW
   s" PASS: " type label labelu type
   s"  (" type mono-ns START-NS @ - PROC-NS-PER-MS / FMT:.INT s"  ms)" type cr ;

\ --- package-owned caller: the checker's replay scopes must start neutral ---
\
\ Every scope tools/check-core.f opens around a replay of the SUBJECT source
\ has to start at neutral top level. If it inherited the caller's package
\ instead, the subject file would be checked as if it were part of that
\ package. The three runs below drive CHECK:RUN, the real entry point, and each
\ one is shaped to fail at a different scope if that scope inherits:
\
\   NEU-FAMILY-SRC$  a top-level family declaration whose tail this package
\                    already owns (the `NEWTYPE ckneub 0` line below). The
\                    nominal pass is the first pass that registers
\                    declarations, so an inherited CHK-RUN-NOMINAL-LINTS scope
\                    files the subject's family under CHECK-TEST and collides
\                    with the one already there; declared at top level, where
\                    the subject really is, there is no collision at all.
\   NEU-EXPORT-SRC$  a top-level EXPORT directive. The nominal pass ignores
\                    EXPORT, so this one reaches CHK-RUN-PREVERIFY; an
\                    inherited scope there reads the directive as an in-package
\                    re-export of a word that package already has.
\   BAD$SRC          a rejecting source, so the run leaves CHK-RUN-SCOPED by
\                    the throwing path.
\
\ After the clean runs and after the throwing one, VERIFY:SOURCE-BUF proves the
\ caller's package was put back exactly: it opens an INHERITING scope, which
\ fails closed unless the checker's package mirror still matches the engine's
\ live package record in mode, in length, and in name bytes.
\
\ These runs are made HERE, in the package body, because this is the only place
\ where the checker's package really is CHECK-TEST's; a case word runs with no
\ package open and cannot reproduce the fault. The case word below only asserts
\ what these runs recorded.

\ The tail the subject source below also declares, owned here by CHECK-TEST.
NEWTYPE ckneub 0

using CHECK

: NEU-FAMILY-SRC$ ( -- ptr u8 n )
   SB-RESET
   s" NEWTYPE ckneub 0" SB-APPEND $0a SB-APPEND-C
   s" : CKT-NEU-B ( ckneub -- ckneub ) ;" SB-APPEND
   SB$ ;

: NEU-EXPORT-SRC$ ( -- ptr u8 n )
   SB-RESET
   s" : CKT-NEU-A ( i64 -- i64 ) 1 + ;" SB-APPEND $0a SB-APPEND-C
   s" EXPORT CKT-NEU-A" SB-APPEND $0a SB-APPEND-C
   s" : CKT-NEU-A-USE ( i64 -- i64 ) CKT-NEU-A ;" SB-APPEND
   SB$ ;

variable NEU-FAMILY-RC
variable NEU-EXPORT-RC
variable NEU-THROW-RC
variable NEU-CLEAN-PKG-RC
variable NEU-THROW-PKG-RC

: NEU-RUN ( ptr u8 n -- n ) {: a:ptr u:n :}
   RESET
   a u s" ckt-neutral.f" SOURCE
   [: RUN-ACT ;] IN-PROC {: outu:n erru:n rc:n :}
   rc ;

: NEU-PKG-RESTORED ( -- n )   \ 0 only when the caller's package came back exact
   [: GOOD$ VERIFY:SOURCE-BUF ;] catch ;

: NEU-RECORD ( -- )
   NEU-FAMILY-SRC$ NEU-RUN NEU-FAMILY-RC !
   NEU-EXPORT-SRC$ NEU-RUN NEU-EXPORT-RC !
   NEU-PKG-RESTORED NEU-CLEAN-PKG-RC !
   BAD$SRC NEU-RUN NEU-THROW-RC !
   NEU-PKG-RESTORED NEU-THROW-PKG-RC !
   RESET ;

\ --- a rejected declaration must not poison the tail it half-registered ---
\
\ The registry reads families through an index that chains rows by id, so a row
\ left chained after its id goes out of range is found by the next lookup of
\ that tail, which then reads a record past the end of the store. The
\ declaration layer used to rewind the family and variant counters itself, and
\ the retirement the outer restore ran afterwards was a no-op that told the
\ index it was current, so the row stayed chained for the rest of the process.
\
\ The shape below is the one that showed it. The first source declares a sum,
\ registers the family, and then rejects on a duplicate variant name: a clean
\ reject, exit 70. The SECOND source declares the same tail correctly. Before
\ the fix that second run died 76 `tfam: bad family id` — not a checker verdict
\ at all, a hard exit out of the registry — because the duplicate-detection
\ lookup at the head of TFAM-DECL walked the bucket the first run left behind.
\
\ Both runs go through CHECK:RUN, which is what the command line runs. A test
\ that called TFAM-DECL directly would prove the registry and miss the seam the
\ two runs share, which is where the state survived from one to the other.
: POISON-BAD-SRC$ ( -- ptr u8 n )
   SB-RESET
   s" SUMTYPE cktpois 1 VARIANT ok a ;VARIANT VARIANT ok a ;VARIANT ;SUMTYPE" SB-APPEND
   SB$ ;

: POISON-GOOD-SRC$ ( -- ptr u8 n )
   SB-RESET
   s" SUMTYPE cktpois 1 VARIANT one a ;VARIANT VARIANT two a ;VARIANT ;SUMTYPE" SB-APPEND $0a SB-APPEND-C
   s" : CKT-POIS-USE ( cktpois<i64> -- cktpois<i64> ) ;" SB-APPEND
   SB$ ;

variable POISON-BAD-RC
variable POISON-GOOD-RC

: POISON-RECORD ( -- )
   POISON-BAD-SRC$ NEU-RUN POISON-BAD-RC !
   POISON-GOOD-SRC$ NEU-RUN POISON-GOOD-RC !
   RESET ;

PREPARE
NEU-RECORD
POISON-RECORD

;using

\ The rejected declaration is a verdict (70) and the valid one that reuses its
\ tail is a clean run (0). A 76 in either slot is the registry dying, not the
\ checker answering.
: TEST-DECL-REJECT-FREES-TAIL ( -- )
   POISON-BAD-RC @ 70 T=
   POISON-GOOD-RC @ 0 T= ;

: TEST-NEUTRAL-SCOPE ( -- )
   NEU-FAMILY-RC @ 0 T=
   NEU-EXPORT-RC @ 0 T=
   NEU-THROW-RC @ 70 T=
   NEU-CLEAN-PKG-RC @ 0 T=
   NEU-THROW-PKG-RC @ 0 T= ;

\ --- one file is checked in its load context -------------------------------
\
\ `tools/check.f FILE` checks FILE the way `bin/hb --load FILE` loads it. The
\ static stages verify FILE as the loader composes it, each file a top-level
\ loader statement loads where the statement stands, so a word FILE's own
\ require supplies resolves, and a rejection inside a dependency is reported
\ in the dependency. The run stage loads FILE from its own path, so a require
\ relative to FILE's directory resolves there too. A file the engine provides
\ is not checked and says so with an E-ENGINE-PROVIDED record and the usage
\ status; a file of check.f's own closure, which the checking image holds, is
\ left to the run stage, as `require` would skip it. Every case runs the real
\ command line.
\
\ A sibling require resolves against the directory of the file the command line
\ named; a file loaded with `required` gets the working directory instead when
\ it lies below it. LC-ROOT is such a working directory: the tree and the engine
\ under test are linked in beside `sub/`, which holds the files checked there.

create LC-PATH FS-PATH-CAP allot
create LC-ROOT FS-PATH-CAP allot
variable LC-PATH-U
variable LC-ROOT-U

: LC-PATH$ ( -- ptr u8 n )
   LC-PATH LC-PATH-U @ ;

: LC-ROOT$ ( -- ptr u8 n )
   LC-ROOT LC-ROOT-U @ ;

: LC-AT ( ptr u8 n -- ptr u8 n )
   ROOT$ 2swap LC-PATH JOIN-PATH LC-PATH-U !
   LC-PATH$ ;

: LC-IN-ROOT ( ptr u8 n -- ptr u8 n )
   LC-ROOT$ 2swap LC-PATH JOIN-PATH LC-PATH-U !
   LC-PATH$ ;

: LC-WRITE ( ptr u8 n ptr u8 n -- ) {: name:ptr nameu:n src:ptr srcu:n :}
   name nameu LC-AT src srcu WRITE-ALL ;

: LC-DEP$SRC ( -- ptr u8 n )
   s\" : CKT-LC-SEVEN ( -- n ) 7 ;\n" ;

: LC-USE$SRC ( -- ptr u8 n )
   s\" require lc-dep.f\n: CKT-LC-USE ( -- n ) CKT-LC-SEVEN ;\n" ;

: LC-USE-ABS$SRC ( -- ptr u8 n )
   SB-RESET
   s" require " SB-APPEND
   s" lc-dep.f" LC-AT SB-APPEND
   $0a SB-APPEND-C
   s\" : CKT-LC-USE ( -- n ) CKT-LC-SEVEN ;\n" SB-APPEND
   SB$ ;

: LC-UNDEF$SRC ( -- ptr u8 n )
   s\" require lc-dep.f\n: CKT-LC-OK ( -- n ) CKT-LC-SEVEN ;\n: CKT-LC-BAD ( -- n ) CKT-LC-NOPE ;\n" ;

: LC-DUP$SRC ( -- ptr u8 n )
   s\" require lc-dep.f\n: CKT-LC-SEVEN ( -- n ) 8 ;\n" ;

: LC-BAD-DEP$SRC ( -- ptr u8 n )
   s\" : CKT-LC-EIGHT ( -- n ) 8 ;\n: CKT-LC-BROKEN ( -- n n ) 8 ;\n" ;

: LC-USE-BAD$SRC ( -- ptr u8 n )
   s\" require lc-bad-dep.f\n: CKT-LC-USE8 ( -- n ) CKT-LC-EIGHT ;\n" ;

\ A family declaration missing its arity, which the nominal pass refuses.
: LC-DECL$SRC ( -- ptr u8 n )
   s\" NEWTYPE cklcnoar\n" ;

\ An unterminated string, which discovery refuses before any later stage reads
\ the file.
: LC-UNTERM$SRC ( -- ptr u8 n )
   s\" : CKT-LC-UNTERM ( -- ptr u8 n ) s\" nope ;\n" ;

: LC-QUOTE$ ( -- ptr u8 n )
   s\" lc-q\"uote.f" ;

: LC-BACK$ ( -- ptr u8 n )
   s\" lc-b\\ack.f" ;

\ A link to LC-QUOTE$, and one whose own name holds a backslash to
\ lc-bad-dep.f, which the verifier refuses.
: LC-QLINK$ ( -- ptr u8 n )
   s" lc-qlink.f" ;

: LC-BLINK$ ( -- ptr u8 n )
   s\" lc-b\\link.f" ;

: LC-FIXTURES ( -- )
   s" lc-dep.f" LC-DEP$SRC LC-WRITE
   s" lc-use.f" LC-USE$SRC LC-WRITE
   s" lc-use-abs.f" LC-USE-ABS$SRC LC-WRITE
   s" lc-undef.f" LC-UNDEF$SRC LC-WRITE
   s" lc-dup.f" LC-DUP$SRC LC-WRITE
   s" lc-bad-dep.f" LC-BAD-DEP$SRC LC-WRITE
   s" lc-use-bad.f" LC-USE-BAD$SRC LC-WRITE
   s" lc-decl.f" LC-DECL$SRC LC-WRITE
   LC-QUOTE$ LC-UNTERM$SRC LC-WRITE
   LC-BACK$ LC-UNTERM$SRC LC-WRITE
   LC-QUOTE$ LC-QLINK$ LC-AT MAKE-SYMLINK
   s" lc-bad-dep.f" LC-BLINK$ LC-AT MAKE-SYMLINK ;

: LC-LINK+ ( ptr u8 n -- ) {: name:ptr nameu:n :}
   name nameu CLI-TARGET CLI-TARGET-U CLI-ABS!
   CLI-TARGET CLI-TARGET-U @ name nameu LC-IN-ROOT MAKE-SYMLINK ;

\ No file in this root requires an engine-provided file, and none may: through
\ the links, a subject that requires one (`lib/string.f`) is refused with an
\ E-BAD-DECLARATION "duplicate family" whose file is `<input>`, a fault of
\ checking in a linked root that predates the load-context replay.
: LC-ROOT-SETUP ( -- )
   ROOT$ s" lc-root" LC-ROOT JOIN-PATH LC-ROOT-U !
   LC-ROOT$ MAKE-DIR
   s" lib" LC-LINK+
   s" tools" LC-LINK+
   s" src" LC-LINK+
   s" bin" LC-IN-ROOT MAKE-DIR
   CLI-HB$ s" bin/hb" LC-IN-ROOT MAKE-SYMLINK
   s" sub" LC-IN-ROOT MAKE-DIR
   s" sub/lc-dep.f" LC-IN-ROOT LC-DEP$SRC WRITE-ALL
   s" sub/lc-use.f" LC-IN-ROOT LC-USE$SRC WRITE-ALL ;

\ The child runs with this process's environment in the given directory.
: LC-CAPTURE ( ptr u8 n -- n n n ) {: cwd:ptr cwdu:n :}
   PROC-ENV-RESET
   PROC-ENV-INHERIT-MISSING
   CLI-HB$ >LEN cwd cwdu >LEN
   CAP-OUT BUF-CAP >LEN CAP-ERR BUF-CAP >LEN CHILD-HANG-MS >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-CAPTURE
   CAPTURE>N ;

: LC-ARGV-ALL ( -- )
   CHECK-ARGV-START
   s" --json-errors" CHECK-ARG+
   s" --all-errors" CHECK-ARG+ ;

: LC-ALL ( ptr u8 n -- n n n )
   LC-ARGV-ALL
   CHECK-ARG+
   CHECK-CAPTURE ;

: LC-STDIN ( ptr u8 n -- n n n )
   LC-ARGV-ALL
   CHECK-STDIN-CAPTURE ;

\ A case reads the packets it asserts on from the parsed stderr stream, so its
\ expectation states the contract: which file a packet names, at which line.

\ The value under KEY in packet NODE when it has KIND, else -1.
: LC-VALUE ( n ptr u8 n n -- n ) {: node:n key:ptr keyu:n kind:n :}
   node 0 < if -1 exit then
   node key keyu JSON-GET {: v:n :}
   v 0 < if -1 exit then
   v JSON-KIND kind = if v exit then
   -1 ;

: LC-STRING$ ( n ptr u8 n -- ptr u8 n )
   J-STR LC-VALUE dup 0 < if drop s" " exit then
   JSON-STRING$ ;

: LC-NUMBER$ ( n ptr u8 n -- ptr u8 n )
   J-NUM LC-VALUE dup 0 < if drop s" " exit then
   JSON-NUMBER$ ;

\ The first packet on the captured stderr whose string KEY is VALUE, or -1.
\ Its fields stay readable until the next parse.
: LC-PACKET ( n ptr u8 n ptr u8 n -- n ) {: erru:n key:ptr keyu:n val:ptr valu:n :}
   CAP-ERR erru JSONL-START
   begin
      JSONL-NEXT-OBJECT
      dup 0 < if exit then
      dup key keyu LC-STRING$ val valu LINT-STR= 0=
   while
      drop
   repeat ;

create LC-CANON FS-PATH-CAP allot
variable LC-CANON-U

\ A JSON packet names the file at PATH by its canonical absolute path.
: LC-EXPECT-FILE ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: f:ptr fu:n path:ptr pathu:n label:ptr labelu:n :}
   path pathu LC-CANON LC-CANON-U CLI-ABS!
   label labelu T-LABEL f fu LC-CANON LC-CANON-U @ T$= ;

\ One diagnostic line as stderr carries it.
: LC-LINE$ ( ptr u8 n -- ptr u8 n )
   SB-RESET SB-APPEND $0a SB-APPEND-C SB$ ;

: LC-EXPECT-CLEAN ( n n n ptr u8 n -- ) {: outu:n erru:n rc:n label:ptr labelu:n :}
   label labelu T-LABEL rc 0 T=
   label labelu T-LABEL outu 0 T=
   label labelu T-LABEL erru 0 T= ;

: LC-SIBLING-CASE ( -- )
   s" lc-use.f" LC-AT LC-ALL s" load-context: relative require" LC-EXPECT-CLEAN
   s" lc-use-abs.f" LC-AT LC-ALL s" load-context: absolute require" LC-EXPECT-CLEAN ;

: LC-ENTRY-ROOT-CASE ( -- )
   LC-ARGV-ALL
   s" sub/lc-use.f" CHECK-ARG+
   LC-ROOT$ LC-CAPTURE s" load-context: entry below the working directory" LC-EXPECT-CLEAN ;

: LC-TREE-CASE ( -- )
   s" test/addrmap-set.f" LC-ALL
   s" load-context: test/addrmap-set.f" T-LABEL 0 T=
   {: outu:n erru:n :}
   erru 0 T=
   CAP-OUT outu s" addrmap-set: ok" CONTAINS? TTRUE ;

\ Nothing checks a file the engine provides, so it is refused at its own
\ spelling, by an E-ENGINE-PROVIDED record, with the usage status.
: LC-PROVIDED-CASE ( -- )
   s" lib/string.f" LC-ALL
   s" load-context: engine-provided lib/string.f" T-LABEL 64 T=
   {: outu:n erru:n :}
   outu 0 T=
   erru s" code" s" E-ENGINE-PROVIDED" LC-PACKET s" file" LC-STRING$
   s" load-context: engine-provided, file as given" T-LABEL s" lib/string.f" T$= ;

\ A file of check.f's own closure is held by the checking image though the
\ engine does not provide it: resident verification skips it, as `require`
\ skips a held file, and the run stage checks it in an engine of its own, as
\ `bin/hb --load` loads it.
: LC-TOOL-CLOSURE-CASE ( -- )
   s" lib/process.f" LC-ALL
   s" load-context: a file of check.f's own closure" LC-EXPECT-CLEAN ;

\ A source list is refused only when the engine provides every input; a list
\ of one engine file and one the checking image holds is checked.
: LC-TOOL-LIST-CASE ( -- )
   CHECK-ARGV-START
   s" --source-list" CHECK-ARG+
   s" lib/string.f" CHECK-ARG+
   s" lib/process.f" CHECK-ARG+
   CHECK-CAPTURE
   s" load-context: a source list with a file check.f's own image holds" LC-EXPECT-CLEAN ;

: LC-UNDEFINED-CASE ( -- )
   s" lc-undef.f" LC-AT LC-ALL
   s" load-context: undefined word" T-LABEL 70 T=
   {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru s\" \"token\":\"CKT-LC-SEVEN\"" CONTAINS? TFALSE
   erru s" code" s" E-UNDEFINED" LC-PACKET {: p:n :}
   p s" token" LC-STRING$ s" CKT-LC-NOPE" T$=
   p s" file" LC-STRING$ s" lc-undef.f" LC-AT s" load-context: undefined word, file" LC-EXPECT-FILE
   p s" line" LC-NUMBER$ s" 3" T$= ;

: LC-DUPLICATE-CASE ( -- )
   s" lc-dup.f" LC-AT LC-ALL
   s" load-context: duplicate of a dependency's word" T-LABEL CHECK-ALL-ERRORS:DUP-RC T=
   {: outu:n erru:n :}
   outu 0 T=
   erru s" code" s" E-DUPLICATE-DEFINITION" LC-PACKET s" file" LC-STRING$
   s" lc-dup.f" LC-AT s" load-context: duplicate, file" LC-EXPECT-FILE ;

: LC-DEPENDENCY-CASE ( -- )
   s" lc-use-bad.f" LC-AT LC-ALL
   s" load-context: rejection inside a dependency" T-LABEL 70 T=
   {: outu:n erru:n :}
   outu 0 T=
   erru s" word" s" ckt-lc-broken" LC-PACKET {: p:n :}
   p s" file" LC-STRING$ s" lc-bad-dep.f" LC-AT s" load-context: dependency rejection, file" LC-EXPECT-FILE
   p s" line" LC-NUMBER$ s" 2" T$= ;

: LC-DECLARATION-CASE ( -- )
   s" lc-decl.f" LC-AT LC-ALL
   s" load-context: refused declaration" T-LABEL 70 T=
   {: outu:n erru:n :}
   outu 0 T=
   erru s" code" s" E-BAD-DECLARATION" LC-PACKET s" file" LC-STRING$
   s" lc-decl.f" LC-AT s" load-context: refused declaration, file" LC-EXPECT-FILE ;

\ The check quotes a path or label into a line it writes and hands a path to
\ the file system. A path or label holding a byte the quoting refuses (double
\ quote, backslash, CR, LF, NUL) is one usage line in every mode, and a listed
\ path holding a NUL is refused before the engine's resolver sees it. The
\ quoted spelling is the canonical path for a named file, the path as given
\ for a listed one, and the label in the run stage: a plain named file's path
\ as given, or a CHECK:SOURCE label. It is judged before anything reads the
\ source, so each case's source is one a later stage refuses, and the answer
\ is the path's whatever the file holds.
: LC-EXPECT-UNSAFE ( n n n ptr u8 n -- ) {: outu:n erru:n rc:n label:ptr labelu:n :}
   label labelu T-LABEL rc 64 T=
   label labelu T-LABEL outu 0 T=
   label labelu T-LABEL CAP-ERR erru
   s" check.f: source path or label contains a double quote, backslash, CR, LF or NUL"
   LC-LINE$ T$= ;

: LC-NUL$ ( -- ptr u8 n )
   SB-RESET s" lc-n" SB-APPEND 0 SB-APPEND-C s" ul.f" SB-APPEND SB$ ;

: LC-UNSAFE-TARGET-CASE ( -- )
   LC-QLINK$ LC-AT LC-ALL
   s" load-context: a link to a name with a double quote, refused by discovery"
   LC-EXPECT-UNSAFE ;

: LC-UNSAFE-LIST-CASE ( -- )
   CHECK-ARGV-START
   s" --source-list" CHECK-ARG+
   LC-BACK$ LC-AT CHECK-ARG+
   CHECK-CAPTURE
   s" load-context: a listed path with a backslash, refused by discovery"
   LC-EXPECT-UNSAFE ;

: LC-UNSAFE-LIST-NUL-CASE ( -- )
   LC-NUL$ LIST-RUN
   s" load-context: a listed path with a NUL" LC-EXPECT-UNSAFE ;

: LC-UNSAFE-PLAIN-CASE ( -- )
   CHECK-ARGV-START
   LC-BLINK$ LC-AT CHECK-ARG+
   CHECK-CAPTURE
   s" load-context: a plain named path with a backslash, refused by the verifier"
   LC-EXPECT-UNSAFE ;

: LC-UNSAFE-SOURCE-CASE ( -- )
   RESET
   s" json-errors" OPT
   LC-DECL$SRC LC-BACK$ SOURCE
   [: RUN-ACT ;] IN-PROC
   s" load-context: a JSON CHECK:SOURCE label with a backslash, refused declaration"
   LC-EXPECT-UNSAFE ;

: LC-UNSAFE-SOURCE-NUL-CASE ( -- )
   RESET
   LC-DECL$SRC LC-NUL$ SOURCE
   [: RUN-ACT ;] IN-PROC
   s" load-context: a CHECK:SOURCE label with a NUL, refused declaration"
   LC-EXPECT-UNSAFE ;

: LC-UNSAFE-NUL-CASE ( -- )
   LC-NUL$ PATH-RUN
   s" load-context: a named path with a NUL" LC-EXPECT-UNSAFE ;

\ Existence is checked before the path is quoted.
: LC-MISSING-QUOTE-CASE ( -- )
   s\" lc-missing-q\"uote.f" LC-AT LC-ALL
   s" load-context: a missing path with a double quote" T-LABEL 66 T=
   {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru s" check.f: no such source" LC-LINE$ T$= ;

\ A relative path as long as a path may be names no file, and its absolute
\ spelling is longer than the engine's resolver takes. Listed, it is missing,
\ as it is named, and the resolver never sees it.
: LC-LONG$ ( -- ptr u8 n )
   SB-RESET
   FS-PATH-CAP 2 - 0 ?do $61 SB-APPEND-C loop
   s" .f" SB-APPEND SB$ ;

: LC-LONG-LIST-CASE ( -- )
   CHECK-ARGV-START
   s" --source-list" CHECK-ARG+
   LC-LONG$ CHECK-ARG+
   CHECK-CAPTURE
   s" load-context: a listed path longer than the engine resolves" T-LABEL 66 T=
   {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru s" check.f: no such source" LC-LINE$ T$= ;

\ One run names a file one way: every JSON packet about the file the command
\ line gives names it by its canonical absolute path. A lint finding ends the
\ static stage, so no run holds both a lint packet and a verifier packet: the
\ path holds three sources in turn, whose packets come from the lint, the
\ verifier and the run stage's child.
: LC-NAMED$ ( -- ptr u8 n )
   s" sub/lc-named.f" ;

: LC-LINT$SRC ( -- ptr u8 n )
   s\" : I ( -- ) ;\n" ;

: LC-VERIFY$SRC ( -- ptr u8 n )
   s\" : CKT-LC-B ( -- n ) 1 ;\n: CKT-LC-C ( -- n ) CKT-LC-B 1 ;\n" ;

\ Only the run stage executes top-level code, so only its child sees CKT-LC-E.
: LC-RUN$SRC ( -- ptr u8 n )
   s\" s\" : CKT-LC-E ( -- n ) 1 2 ;\" evaluate\n" ;

\ Once the named path holds SRC, its check exits RC and the first CODE packet
\ names the file.
: LC-NAMED-CASE ( ptr u8 n n ptr u8 n ptr u8 n -- )
   {: src:ptr srcu:n rc:n code:ptr codeu:n label:ptr labelu:n :}
   LC-NAMED$ LC-IN-ROOT src srcu WRITE-ALL
   LC-ARGV-ALL
   LC-NAMED$ CHECK-ARG+
   LC-ROOT$ LC-CAPTURE
   label labelu T-LABEL rc T=
   {: outu:n erru:n :}
   label labelu T-LABEL outu 0 T=
   erru s" code" code codeu LC-PACKET s" file" LC-STRING$
   LC-NAMED$ LC-IN-ROOT label labelu LC-EXPECT-FILE ;

: LC-SPELLING-CASE ( -- )
   LC-LINT$SRC 1 s" E-RESERVED-DEFINITION" s" load-context: lint packet" LC-NAMED-CASE
   LC-VERIFY$SRC 70 s" E-MISMATCH" s" load-context: verifier packet" LC-NAMED-CASE
   LC-RUN$SRC 70 s" E-MISMATCH" s" load-context: run-stage packet" LC-NAMED-CASE ;

: LC-STDIN-CASE ( -- )
   BAD$SRC LC-STDIN
   s" load-context: stdin rejection" T-LABEL 70 T=
   {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru s\" \"file\":\"<stdin>\",\"line\":1," CONTAINS? TTRUE ;

: TEST-LOAD-CONTEXT ( -- )
   LC-FIXTURES
   LC-ROOT-SETUP
   LC-SIBLING-CASE
   LC-ENTRY-ROOT-CASE
   LC-TREE-CASE
   LC-PROVIDED-CASE
   LC-TOOL-CLOSURE-CASE
   LC-TOOL-LIST-CASE
   LC-UNDEFINED-CASE
   LC-DUPLICATE-CASE
   LC-DEPENDENCY-CASE
   LC-DECLARATION-CASE
   LC-UNSAFE-TARGET-CASE
   LC-UNSAFE-LIST-CASE
   LC-UNSAFE-LIST-NUL-CASE
   LC-UNSAFE-PLAIN-CASE
   LC-UNSAFE-SOURCE-CASE
   LC-UNSAFE-SOURCE-NUL-CASE
   LC-UNSAFE-NUL-CASE
   LC-MISSING-QUOTE-CASE
   LC-LONG-LIST-CASE
   LC-SPELLING-CASE
   LC-STDIN-CASE ;

\ A source checked with the files it loads; a case list of its own keeps
\ TEST-MAIN under the 8000-byte body limit.
: REQUIRE-CASES ( -- )
   s" check/require-facade" [: TEST-REQUIRE-FACADE ;] CASE-RUN
   s" check/included-dep" [: TEST-INCLUDED-DEP ;] CASE-RUN
   s" check/require-cycle" [: TEST-REQUIRE-CYCLE ;] CASE-RUN
   s" check/require-order" [: TEST-REQUIRE-ORDER ;] CASE-RUN
   s" check/require-early" [: TEST-REQUIRE-EARLY ;] CASE-RUN
   s" check/require-late" [: TEST-REQUIRE-LATE ;] CASE-RUN
   s" check/require-once" [: TEST-REQUIRE-ONCE ;] CASE-RUN
   s" check/require-cycle-split" [: TEST-REQUIRE-CYCLE-SPLIT ;] CASE-RUN
   s" check/require-body" [: TEST-REQUIRE-BODY ;] CASE-RUN
   s" check/require-origin" [: TEST-REQUIRE-ORIGIN ;] CASE-RUN
   s" check/require-order-all-list" [: TEST-REQUIRE-ORDER-ALL ;] CASE-RUN
   s" check/require-late-all-list" [: TEST-REQUIRE-LATE-ALL ;] CASE-RUN
   s" check/require-origin-all-list" [: TEST-REQUIRE-ORIGIN-ALL ;] CASE-RUN
   s" check/require-use" [: TEST-REQUIRE-USE ;] CASE-RUN
   s" check/require-use-all" [: TEST-REQUIRE-USE-ALL ;] CASE-RUN
   s" check/require-use-all-list" [: TEST-REQUIRE-USE-ALL-LIST ;] CASE-RUN
   s" check/require-throw-all" [: TEST-REQUIRE-THROW-ALL ;] CASE-RUN
   s" check/require-throw-all-list" [: TEST-REQUIRE-THROW-ALL-LIST ;] CASE-RUN
   s" check/require-throw-prose" [: TEST-REQUIRE-THROW-PROSE ;] CASE-RUN
   s" check/require-package" [: TEST-REQUIRE-PACKAGE ;] CASE-RUN
   s" check/require-using" [: TEST-REQUIRE-USING ;] CASE-RUN
   s" check/require-cascade" [: TEST-REQUIRE-CASCADE ;] CASE-RUN
   s" check/require-duplicate" [: TEST-REQUIRE-DUPLICATE ;] CASE-RUN
   s" check/require-duplicate-prose" [: TEST-REQUIRE-DUPLICATE-PROSE ;] CASE-RUN
   s" check/require-size-all" [: TEST-REQUIRE-SIZE-ALL ;] CASE-RUN
   s" check/require-size-all-list" [: TEST-REQUIRE-SIZE-ALL-LIST ;] CASE-RUN
   s" check/require-size-prose" [: TEST-REQUIRE-SIZE-PROSE ;] CASE-RUN ;

\ Declaration forms (ENUM, PRODUCT, STRUCTURE, VALUE-RECORD, nominal names,
\ package families); a case list of its own keeps TEST-MAIN under the
\ 8000-byte body limit.
: DECL-CASES ( -- )
   s" check/enum-good" [: TEST-ENUM-GOOD ;] CASE-RUN
   s" check/enum-bad" [: TEST-ENUM-BAD ;] CASE-RUN
   s" check/product-good" [: TEST-PRODUCT-GOOD ;] CASE-RUN
   s" check/product-bad" [: TEST-PRODUCT-BAD ;] CASE-RUN
   s" check/product-all-errors" [: PROD-ALL-TEST ;] CASE-RUN
   s" check/struct-good" [: TEST-STRUCT-GOOD ;] CASE-RUN
   s" check/struct-payload" [: TEST-STRUCT-PAYLOAD ;] CASE-RUN
   s" check/struct-bad-json" [: TEST-STRUCT-BAD-JSON ;] CASE-RUN
   s" check/struct-bad-prose" [: TEST-STRUCT-BAD-PROSE ;] CASE-RUN
   s" check/enum-bad-prose" [: TEST-ENUM-BAD-PROSE ;] CASE-RUN
   s" check/enum-line-comment" [: TEST-ENUM-LINE-COMMENT ;] CASE-RUN
   s" check/enum-paren-comment" [: TEST-ENUM-PAREN-COMMENT ;] CASE-RUN
   s" check/struct-line-comment" [: TEST-STRUCT-LINE-COMMENT ;] CASE-RUN
   s" check/struct-paren-comment" [: TEST-STRUCT-PAREN-COMMENT ;] CASE-RUN
   s" check/decl-over-cap" [: TEST-DECL-OVER-CAP ;] CASE-RUN
   s" check/struct-noend" [: TEST-STRUCT-NOEND ;] CASE-RUN
   s" check/enum-noend" [: TEST-ENUM-NOEND ;] CASE-RUN
   s" check/product-noend" [: TEST-PROD-NOEND ;] CASE-RUN
   s" check/value-record-noend" [: TEST-VREC-NOEND ;] CASE-RUN
   s" check/enum-noend-cli" [: ENUM-CLI-TEST ;] CASE-RUN
   s" check/nominal-scan-top-level" [: NOM-SCAN-TEST ;] CASE-RUN
   s" check/nominal-preverify" [: TEST-NOMINAL-PREVERIFY ;] CASE-RUN
   s" check/nominal-shadow" [: TEST-NOMINAL-SHADOW ;] CASE-RUN
   s" check/nominal-dup" [: TEST-NOMINAL-DUP ;] CASE-RUN
   s" check/nominal-ctor-tail" [: TEST-NOMINAL-CTOR-TAIL ;] CASE-RUN
   s" check/nominal-shadow-side" [: TEST-NOMINAL-SHADOW-SIDE ;] CASE-RUN
   s" check/decl-ctor-tail" [: TEST-DECL-CTOR-TAIL ;] CASE-RUN
   s" check/decl-vrec-tail" [: TEST-DECL-VREC-TAIL ;] CASE-RUN
   s" check/decl-atom-tail" [: TEST-DECL-ATOM-TAIL ;] CASE-RUN
   s" check/nominal-name-refused" [: TEST-NOMINAL-NAME-REFUSED ;] CASE-RUN
   s" check/nominal-name-admitted" [: TEST-NOMINAL-NAME-ADMITTED ;] CASE-RUN
   s" check/nominal-family-claim" [: TEST-NOMINAL-FAMILY-CLAIM ;] CASE-RUN
   s" check/operand-name-admitted" [: TEST-OPERAND-NAME-ADMITTED ;] CASE-RUN
   s" check/operand-name-refused" [: TEST-OPERAND-NAME-REFUSED ;] CASE-RUN
   s" check/operand-missing" [: TEST-OPERAND-MISSING ;] CASE-RUN
   s" check/raw-operand" [: TEST-RAW-OPERAND ;] CASE-RUN
   s" check/value-record-field-refused" [: TEST-VREC-FIELD-REFUSED ;] CASE-RUN
   s" check/package-linear-good" [: LINEAR-GOOD-TEST ;] CASE-RUN
   s" check/package-linear-cross" [: LINEAR-CROSS-TEST ;] CASE-RUN
   s" check/package-linear-global" [: LINEAR-GLOBAL-TEST ;] CASE-RUN
   s" check/package-linear-distinct" [: LINEAR-DISTINCT-TEST ;] CASE-RUN
   s" check/package-family-good" [: FAM-GOOD-TEST ;] CASE-RUN
   s" check/package-family-two" [: FAM-TWO-TEST ;] CASE-RUN
   s" check/package-family-bogus" [: FAM-BOGUS-TEST ;] CASE-RUN
   s" check/package-family-json-pin" [: FAM-JSON-PIN ;] CASE-RUN
   s" check/package-family-json-escape" [: FAM-ESC-TEST ;] CASE-RUN
   s" check/label-json-escape" [: LABEL-ESC-TEST ;] CASE-RUN
   s" check/package-family-private" [: FAM-PRIV-TEST ;] CASE-RUN
   s" check/declared-constructors" [: TEST-DECLARED-CONSTRUCTORS ;] CASE-RUN
   s" check/derived-init-accessors" [: TEST-DERIVED-INIT-ACCESSORS ;] CASE-RUN ;

: TEST-MAIN ( -- )
   T-RESET
   s" check/package-caller-neutral" [: TEST-NEUTRAL-SCOPE ;] CASE-RUN
   s" check/decl-reject-frees-tail" [: TEST-DECL-REJECT-FREES-TAIL ;] CASE-RUN
   s" check/good" [: TEST-GOOD ;] CASE-RUN
   s" check/print-parity" [: TEST-PRINT-PARITY ;] CASE-RUN
   s" check/prelude-hook-public" [: TEST-PRELUDE-HOOK ;] CASE-RUN
   s" check/layout-buffer" [: TEST-LAYOUT-BUFFER ;] CASE-RUN
   s" check/buffer-count" [: TEST-BUFFER-COUNT ;] CASE-RUN
   s" check/parsed-operand" [: TEST-PARSED-OPERAND ;] CASE-RUN
   s" check/rendered-product" [: TEST-RENDERED-PRODUCT ;] CASE-RUN
   s" check/rendered-does" [: TEST-RENDERED-DOES ;] CASE-RUN
   s" check/refused-clause" [: TEST-REFUSED-CLAUSE ;] CASE-RUN
   s" check/layout-buffer-count" [: TEST-LAYOUT-BUFFER-COUNT ;] CASE-RUN
   s" check/file-label" [: TEST-FILE-LABEL ;] CASE-RUN
   s" check/usage-direct" [: TEST-USAGE ;] CASE-RUN
   s" check/source-bytes-copy" [: TEST-SOURCE-BYTES-COPY ;] CASE-RUN
   s" check/file-path-copy" [: TEST-FILE-PATH-COPY ;] CASE-RUN
   s" check/source-list-idempotent" [: TEST-LIST-IDEMPOTENT ;] CASE-RUN
   s" check/source-list-promotion" [: TEST-LIST-PROMOTION ;] CASE-RUN
   s" check/empty-source-mode" [: TEST-EMPTY-SOURCE-MODE ;] CASE-RUN
   s" check/boundary-phase" [: TEST-BOUNDARY-PHASE ;] CASE-RUN
   s" check/options" [: TEST-OPTIONS ;] CASE-RUN
   s" check/mode-collisions" [: TEST-MODE-COLLISIONS ;] CASE-RUN
   s" check/die" [: TEST-DIE ;] CASE-RUN
   s" check/forward-ref-direct" [: TEST-FWDREF-DIRECT ;] CASE-RUN
   s" check/forward-ref-json" [: TEST-FWDREF-JSON ;] CASE-RUN
   s" check/origin-scan" [: TEST-ORIGIN-SCAN ;] CASE-RUN
   s" check/origin-base" [: TEST-ORIGIN-BASE ;] CASE-RUN
   s" check/forward-ref-raw-load" [: RAW-FWDREF-TEST ;] CASE-RUN
   s" check/unterminated-string" [: TEST-UNTERM-STRING ;] CASE-RUN
   s" check/duplicate-all-errors" [: TEST-DUP-ALL ;] CASE-RUN
   s" check/source-list-reserved" [: RESERVED-LIST-TEST ;] CASE-RUN
   s" check/source-list-audited-lib" [: AUDITED-LIB-TEST ;] CASE-RUN
   s" check/source-list-provided" [: PROVIDED-LIST-TEST ;] CASE-RUN
   s" check/engine-provided-json" [: TEST-ENGINE-PROVIDED-JSON ;] CASE-RUN
   s" check/engine-provided-prose" [: TEST-ENGINE-PROVIDED-PROSE ;] CASE-RUN
   s" check/source-list-harness-lib" [: TEST-HARNESS-LIB ;] CASE-RUN
   s" check/source-list-preverify-diag" [: PREVERIFY-DIAG-TEST ;] CASE-RUN
   s" check/value-record-good" [: VREC-GOOD-TEST ;] CASE-RUN
   s" check/linear-bad" [: TEST-LINEAR-BAD ;] CASE-RUN
   s" check/value-record-bad" [: VREC-BAD-TEST ;] CASE-RUN
   s" check/value-record-partial" [: VREC-PARTIAL-TEST ;] CASE-RUN
   s" check/newtype-good" [: TEST-NEWTYPE-GOOD ;] CASE-RUN
   s" check/newtype-all-errors" [: TEST-NEWTYPE-ALL ;] CASE-RUN
   s" check/sumtype-bad" [: TEST-SUMTYPE-BAD ;] CASE-RUN
   s" check/sumtype-all-redrive" [: SUM-REDRIVE-TEST ;] CASE-RUN
   s" check/nominal-all-redrive" [: NOM-REDRIVE-TEST ;] CASE-RUN
   s" check/nominal-all-clean" [: NOM-CLEAN-TEST ;] CASE-RUN
   s" check/tfam-noarity" [: TEST-TFAM-NOARITY ;] CASE-RUN
   s" check/sum-noend" [: TEST-SUM-NOEND ;] CASE-RUN
   s" check/sum-noend-all" [: SUM-NOEND-ALL ;] CASE-RUN
   s" check/tfam-noarity-all" [: TFAM-NOARITY-ALL ;] CASE-RUN
   s" check/overcap-source" [: TEST-OVERCAP-SOURCE ;] CASE-RUN
   s" check/selection-capacity" [: TEST-SELECTION-CAPACITY ;] CASE-RUN
   s" check/mid-source" [: TEST-MID-SOURCE ;] CASE-RUN
   s" check/cap-source" [: TEST-CAP-SOURCE ;] CASE-RUN
   s" check/run-output-cap" [: TEST-RUN-OUTPUT-CAP ;] CASE-RUN
   s" check/die-scratch" [: TEST-DIE-SCRATCH ;] CASE-RUN
   s" check/long-name" [: TEST-LONG-NAME ;] CASE-RUN
   s" check/refusals" [: TEST-REFUSALS ;] CASE-RUN
   s" check/scratch-full" [: TEST-SCRATCH-FULL ;] CASE-RUN
   s" check/list-capacity" [: TEST-LIST-CAPACITY ;] CASE-RUN
   s" check/empty-list" [: TEST-EMPTY-LIST ;] CASE-RUN
   s" check/missing-file" [: TEST-MISSING-FILE ;] CASE-RUN
   s" check/missing-engine" [: TEST-MISSING-ENGINE ;] CASE-RUN
   s" check/cleanup-failure" [: TEST-CLEANUP-FAILURE ;] CASE-RUN
   s" check/run-environment" [: TEST-RUN-ENVIRONMENT ;] CASE-RUN
   s" check/repeat-source-ok" [: TEST-REPEAT-SOURCE-OK ;] CASE-RUN
   s" check/repeat-source-fail" [: TEST-REPEAT-SOURCE-FAIL ;] CASE-RUN
   s" check/repeat-file" [: TEST-REPEAT-FILE ;] CASE-RUN
   s" check/repeat-list" [: TEST-REPEAT-LIST ;] CASE-RUN
   s" check/repeat-definer" [: TEST-REPEAT-DEFINER ;] CASE-RUN
   s" check/oversize" [: TEST-OVERSIZE ;] CASE-RUN
   DECL-CASES
   REQUIRE-CASES
   s" check/statement-throw-json" [: TEST-STATEMENT-THROW-JSON ;] CASE-RUN
   s" check/statement-throw-prose" [: TEST-STATEMENT-THROW-PROSE ;] CASE-RUN
   s" check/using-at-source" [: TEST-USING-AT-SOURCE ;] CASE-RUN
   s" check/shebang-clean" [: TEST-SHEBANG-CLEAN ;] CASE-RUN
   s" check/shebang-at-line" [: TEST-SHEBANG-AT-LINE ;] CASE-RUN
   s" check/shebang-later" [: TEST-SHEBANG-LATER ;] CASE-RUN
   s" check/capacity-throw" [: TEST-CAPACITY-THROW ;] CASE-RUN
   s" check/storage-type" [: TEST-STORAGE-TYPE ;] CASE-RUN
   s" check/storage-name" [: TEST-STORAGE-NAME ;] CASE-RUN
   s" check/storage-defer" [: TEST-STORAGE-DEFER ;] CASE-RUN
   s" check/image-tool-sources" [: TEST-IMAGE-TOOL-SOURCES ;] CASE-RUN
   s" check/source-list-all-errors" [: LIST-ALL-TEST ;] CASE-RUN
   s" check/file-load-context" [: TEST-LOAD-CONTEXT ;] CASE-RUN
   CLEANUP-RUN
   T-REPORT
   s" check-test: ok" type cr ;

public

: TEST ( -- )
   TEST-MAIN ;

;package
