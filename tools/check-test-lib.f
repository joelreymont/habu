\ check-test-lib.f - checked engine CLI/core smoke coverage library.
\ Run: bin/hb --load lib/date.f lib/errors.f lib/string.f lib/test.f lib/memory.f
\ lib/vector.f lib/fs.f lib/fs-mutate.f lib/process.f lib/process-argv.f
\ lib/process-env.f lib/process-cwd.f lib/fmt.f lib/source.f
\ tools/lint/text.f tools/lint/token.f tools/lint/lib.f
\ tools/lint/json-writer.f tools/lint/source-lex.f
\ tools/diag-origin-core.f tools/json.f tools/json-only-core.f
\ tools/checked-boundary-lint-core.f
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
require lib/fmt.f
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
require tools/check-all-errors-core.f
require lib/argv.f
require tools/check-core.f
require lib/fmt.f                        \ FMT:.INT - one-line number text

package CHECK-TEST

using CHECK
private

$4000 constant BUF-CAP
128 constant LIST-ENTRY-CAP

\ What the child cases here prove is the standalone command line: the exit status
\ of `bin/hb tools/check.f ...`, the diagnostics it writes, and the temporary
\ files it removes. None of that depends on how long a child takes, so the
\ millisecond budget handed to every capture is a deadlock guard: it exists so a
\ child that never exits cannot hang the gate forever.
\
\ WORST-CHILD-MS records the busiest child measured when the guard was set, on
\ a 12-core machine: the cleanup child, which took 4.7 to 5.0 s at an ambient
\ load average of 13 and 11.2 to 13.4 s while eight gate pool slots were busy.
\ HANG-MARGIN is 4 rather than the order of magnitude the cheaper fixtures can
\ afford, because the product is bounded from above by the registry's outer
\ timeout as well.
\
\ Load can still reach the 54 s guard: nothing bounds how far a busy host
\ stretches a child. At a load average near 73 an engine build took 2.9 times
\ its time under ordinary gate load (259 s against about 90 s). The current
\ cases are lighter: the heaviest child takes 0.62 s of CPU, so the guard is
\ about 87 times that, and the whole row takes 27.7 s of user time and 117 s
\ of wall time alone at a load average of 82. An expiry is therefore a timeout,
\ not proof of a deadlock: CASE-HUNG names the case and rethrows
\ E-PROC-TIMEOUT, and the gate pool labels the row TIMEOUT-UNDER-LOAD.
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
create CLI-TARGET FS-PATH-CAP allot
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
variable CLI-TARGET-U
variable CLI-HB-U
variable ENV-PROBE-U
variable START-NS

: PATH-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u:n dst:ptr lenp:ptr :}
   a dst u BYTE-COPY
   u lenp ! ;

: ROOT$ ( -- ptr u8 n )
   TMP-ROOT TMP-ROOT-U @ ;

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
\ where PWD is unset, and a link target joined to an empty PWD dangles, so a
\ child run among LC-ROOT's links dies 74 on its own load path.
: CLI-ABS! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u:n buf:ptr lenp:ptr :}
   a u FS-PATHZ buf FS-PATH-CAP realpath {: n:n :}
   n 0 <= if E-FS-PATH throw then
   n lenp ! ;

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

: DIE$ ( -- ptr u8 n )
   s\" : CKT-BYE ( -- ) s\q bye\q 5 die ;\nCKT-BYE" ;

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
\ pre-pass. The text may define a package, so a qualified name there is left to
\ the run as a bare one is. The fixtures require lib/ffi-abi.f for `FUNCTION:` and lib/task.f
\ for TASK:+USER, the renderer lib/crypto/evp.f uses. The pre-pass checks
\ FUNCTION:'s word against its declaration group and a TASK:+USER slot
\ against the `generates:` row lib/task.f states; CMD:COMMAND's row declares
\ only NAME, so NAME#VEC and NAME#BUF are left to the run.
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

: RENDER-QUALIFIED$ ( -- ptr u8 n )
   s\" s\" package CKR-QP public : CKR-QW ( -- n ) 7 ; ;package\" evaluate : CKR-QU ( -- n ) CKR-QP:CKR-QW ;" ;

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

: RENDER-CMD$ ( ptr u8 n -- ptr u8 n )
   {: use:ptr useu:n :}
   SB-RESET s" require lib/process-command.f CMD:COMMAND CKR-CMD " SB-APPEND
   use useu SB-APPEND SB$ ;

: RENDER-CMD-VEC$ ( -- ptr u8 n )
   s" : CKR-VEC ( -- ptr ptr u8 ) CKR-CMD#VEC ;" RENDER-CMD$ ;

: RENDER-CMD-BUF$ ( -- ptr u8 n )
   s" : CKR-BUF ( -- ) CKR-CMD#BUF drop ;" RENDER-CMD$ ;

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
   RENDER-QUALIFIED$ DIRECT-STDIN EXPECT-ACCEPTED
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

: TEST-RENDERED-COMMAND ( -- )
   RENDER-CMD-VEC$ DIRECT-STDIN EXPECT-ACCEPTED
   RENDER-CMD-BUF$ DIRECT-STDIN EXPECT-ACCEPTED
   RENDER-CMD-VEC$ DIRECT-ALL-STDIN EXPECT-ACCEPTED
   RENDER-CMD-BUF$ DIRECT-ALL-STDIN EXPECT-ACCEPTED ;

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

\ A count the load refuses to resolve is refused as the load refuses it, at the
\ token, once, before the definer reads it; a shadowed one ends the check as a
\ body's shadowed word does.
: LBUF-UNDEFINED ( ptr u8 n -- )
   {: src:ptr srcu:n :}
   src srcu DIRECT-STDIN 70 T=
   {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru s\" \"code\":\"E-UNDEFINED-TOP-LEVEL\",\"repair_class\":\"unknown_rejection\",\"verdict\":\"rejected\",\"token\":\"CKLB-NOPE\"" CONTAINS? TTRUE
   src srcu s" --all-errors" CLI-FLAG-STDIN 70 T=
   {: outa:n erra:n :}
   outa 0 T=
   CAP-ERR erra s" undefined word 'CKLB-NOPE'" CONTAINS? TTRUE
   CAP-ERR erra s" count resolves" CONTAINS? TFALSE ;

: LBUF-SHADOWED ( ptr u8 n -- )
   DIRECT-STDIN 70 T=
   {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru s\" \"code\":\"E-USING-SHADOW-GLOBAL\",\"repair_class\":\"disambiguate_using_shadow\",\"verdict\":\"rejected\",\"token\":\"CKLB-CAP\"" CONTAINS? TTRUE ;

: TEST-LAYOUT-BUFFER-COUNT ( -- )
   s" package CKLB-B 8 constant CAP CAP TYPED-BUFFER ROWS n ;package" LBUF-ACCEPT
   s" package CKLB-Q public 8 constant CAP ;package package CKLB-QU CKLB-Q:CAP TYPED-BUFFER ROWS n ;package" LBUF-ACCEPT
   s" package CKLB-C 6 constant OPS 2 constant KEYS OPS KEYS + constant VOCAB VOCAB TYPED-BUFFER ROWS n ;package" LBUF-ACCEPT
   s" package CKLB-H HIR:OPCODES TYPED-BUFFER ROWS n ;package" LBUF-ACCEPT
   s" package CKLB-W : CAP ( -- n ) 4 ; CAP TYPED-BUFFER ROWS n ;package" LBUF-ACCEPT
   s" package CKLB-L SUMTYPE cklb 1 VARIANT value a ;VARIANT ;SUMTYPE 2 constant CAP CAP LAYOUT-BUFFER BUF cklb<n> ;package" LBUF-ACCEPT
   s" package CKLB-U CKLB-NOPE TYPED-BUFFER ROWS n ;package" LBUF-UNDEFINED
   s" package CKLB-V variable CAP CAP TYPED-BUFFER ROWS n ;package" LBUF-REFUSE
   s" package CKLB-T : CAP ( -- bool ) 0 0= ; CAP TYPED-BUFFER ROWS n ;package" LBUF-REFUSE
   s" package CKLB-I : CAP ( n -- n ) 2 * ; 4 CAP TYPED-BUFFER ROWS n ;package" LBUF-ACCEPT
   s" package CKLB-E 2 3 * TYPED-BUFFER ROWS n ;package" LBUF-ACCEPT
   s" 8 constant CKLB-CAP package CKLB-S public 4 constant CKLB-CAP ;package using CKLB-S CKLB-CAP TYPED-BUFFER CKLB-ROWS n ;using" LBUF-SHADOWED
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

\ ---- the child's deadline ----------------------------------------------------
\ --deadline-ms gives the child a deadline other than check.f's 120 s: a whole
\ number of milliseconds from 1 to 2147483647, the longest wait poll(2) takes,
\ given once. Anything else is a usage error. A run past its deadline is
\ test/check-signal-test.f's: it needs the run's processes watched.

: DEADLINE-RUN ( ptr u8 n -- n n n ) {: value:ptr valueu:n :}
   CHECK-ARGV-START
   s" --deadline-ms" CHECK-ARG+
   value valueu CHECK-ARG+
   DIRECT$ CHECK-ARG+
   CHECK-CAPTURE ;

: EXPECT-DEADLINE-USAGE ( n n n ptr u8 n -- ) {: outu:n erru:n rc:n label:ptr labelu:n :}
   label labelu T-LABEL rc 64 T=
   label labelu T-LABEL outu 0 T=
   label labelu T-LABEL CAP-ERR erru s" usage: tools/check.f" CONTAINS? TTRUE ;

: DEADLINE-TWICE ( -- n n n )
   CHECK-ARGV-START
   s" --deadline-ms" CHECK-ARG+
   s" 60000" CHECK-ARG+
   s" --deadline-ms" CHECK-ARG+
   s" 60000" CHECK-ARG+
   DIRECT$ CHECK-ARG+
   CHECK-CAPTURE ;

: DEADLINE-MISSING ( -- n n n )
   CHECK-ARGV-START
   s" --deadline-ms" CHECK-ARG+
   CHECK-CAPTURE ;

: TEST-DEADLINE-OPTION ( -- )
   DIRECT$ GOOD$ WRITE-ALL
   s" 2147483647" DEADLINE-RUN
   s" deadline: the longest deadline is taken" T-LABEL 0 T=
   2drop
   s" 0" DEADLINE-RUN s" deadline: zero" EXPECT-DEADLINE-USAGE
   s" 2147483648" DEADLINE-RUN s" deadline: past the longest" EXPECT-DEADLINE-USAGE
   s" 5s" DEADLINE-RUN s" deadline: not a number" EXPECT-DEADLINE-USAGE
   DEADLINE-TWICE s" deadline: given twice" EXPECT-DEADLINE-USAGE
   DEADLINE-MISSING s" deadline: no value" EXPECT-DEADLINE-USAGE ;

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

\ ---- a source list lints each listed file for the checked boundary ----------
\ The list runs one `script-required` line per listed file, so the boundary
\ lint reads the listed files: one that switches the checker off and then
\ defines is refused in every mode at its own line and column, as the single
\ file is. The switch carries across the list: a definition in a later file is
\ refused at its own line and column. An engine source beside a clean subject
\ is not linted, as the run loads nothing from it: the strict lint refuses
\ src/habu/prims.f's `set-check`.
: BOUNDARY-OFF$ ( -- ptr u8 n )
   SB-RESET
   s" 0 set-check" SB-APPEND $0a SB-APPEND-C
   s" : CKT-BX ( -- n ) 1 ;" SB-APPEND $0a SB-APPEND-C
   SB$ ;

: BOUNDARY-PROSE$ ( ptr u8 n -- ptr u8 n ) {: at:ptr atu:n :}
   SB-RESET
   s" UNCHECKED-DEFINITION " SB-APPEND
   LIST$ SB-APPEND
   at atu SB-APPEND
   s"  `CKT-BX`" SB-APPEND
   SB$ ;

: BOUNDARY-MUTATION$ ( -- ptr u8 n )
   SB-RESET
   s" CHECKER-MUTATION " SB-APPEND
   SUP$ SB-APPEND
   s" :1:3:" SB-APPEND
   SB$ ;

: BOUNDARY-JSON$ ( -- ptr u8 n )
   SB-RESET
   s\" \"file\":\"" SB-APPEND
   LIST$ SB-APPEND
   s\" \",\"line\":2,\"column\":3," SB-APPEND
   SB$ ;

: BOUNDARY-LIST-RUN ( [ -- ] -- n n n ) {: setup :}
   RESET
   setup execute
   LIST-OPT
   LIST$ FILE
   [: RUN-ACT ;] IN-PROC ;

: EXPECT-BOUNDARY ( n n n ptr u8 n -- ) {: outu:n erru:n rc:n want:ptr wantu:n :}
   rc 1 T=
   outu 0 T=
   CAP-ERR erru want wantu CONTAINS? TTRUE ;

: TEST-BOUNDARY-LIST ( -- )
   LIST$ BOUNDARY-OFF$ WRITE-ALL
   [: ;] BOUNDARY-LIST-RUN s" :2:3:" BOUNDARY-PROSE$ EXPECT-BOUNDARY
   [: s" all-errors" OPT ;] BOUNDARY-LIST-RUN s" :2:3:" BOUNDARY-PROSE$ EXPECT-BOUNDARY
   [: s" json-errors" OPT ;] BOUNDARY-LIST-RUN
   {: outu:n erru:n rc:n :}
   outu erru rc BOUNDARY-JSON$ EXPECT-BOUNDARY
   CAP-ERR erru s\" \"code\":\"E-UNCHECKED-DEFINITION\"" CONTAINS? TTRUE
   SUP$ s\" 0 set-check\n" WRITE-ALL
   LIST$ s\" : CKT-BX ( -- n ) 1 ;\n" WRITE-ALL
   [: SUP$ FILE ;] BOUNDARY-LIST-RUN
   {: outu:n erru:n rc:n :}
   outu erru rc s" :1:3:" BOUNDARY-PROSE$ EXPECT-BOUNDARY
   CAP-ERR erru BOUNDARY-MUTATION$ CONTAINS? TTRUE
   LIST$ GOOD$ WRITE-ALL
   [: s" src/habu/prims.f" FILE ;] BOUNDARY-LIST-RUN 0 T=
   {: outu:n erru:n :}
   outu 0 T=
   erru 0 T= ;

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
   s" ( : POISON-PAREN dup ;" SB-APPEND
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

: ORIGIN-MULTI$ ( -- ptr u8 n )
   SB-RESET
   s" : CKT-ORIGIN-MULTI ( n -- n )" SB-APPEND  $0a SB-APPEND-C
   s"    dup ;" SB-APPEND
   SB$ ;

: ORIGIN-SUM$ ( -- ptr u8 n )      \ the arity slot holds VARIANT
   SB-RESET
   s" SUMTYPE cktosum" SB-APPEND  $0a SB-APPEND-C
   s"    VARIANT Up" SB-APPEND  $0a SB-APPEND-C
   s" ;SUMTYPE" SB-APPEND
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

: ORIGIN-VERIFY-MULTI ( -- n )
   CHECKER-CANDIDATE-SCOPE-START
   [: ORIGIN-MULTI$ 7 9 100 VERIFY:SOURCE-BUF-AT-IN-SCOPE ;] catch {: rc:n :}
   CHECKER-CANDIDATE-SCOPE-DONE
   rc ;

: ORIGIN-VERIFY-SUM ( -- n )
   CHECKER-CANDIDATE-SCOPE-START
   [: ORIGIN-SUM$ 7 9 100 VERIFY:SOURCE-BUF-AT-IN-SCOPE ;] catch {: rc:n :}
   CHECKER-CANDIDATE-SCOPE-DONE
   rc ;

: TEST-ORIGIN-SCAN ( -- )
   ORIGIN-SCAN$ DIRECT-JSON-STDIN 70 T=
   {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru s\" \"word\":\"ckt-origin-scan\"" CONTAINS? TTRUE
   CAP-ERR erru s\" \"line\":8,\"column\":30,\"byte_start\":208,\"byte_end\":211" CONTAINS? TTRUE ;

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

\ A body on the line after its name: the packet puts `dup` where the scanner
\ read it, not at its offset in the one-line body the checker sees.
: TEST-ORIGIN-MULTI ( -- )
   CAP-ERR BUF-CAP DIAG-BUFFER!
   0 0= DIAG-JSON!
   ORIGIN-VERIFY-MULTI 70 T=
   DIAG-BUFFER$ s\" \"line\":8,\"column\":4,\"byte_start\":133,\"byte_end\":136" CONTAINS? TTRUE
   DIAG-BUFFER-OFF
   0 0= 0= DIAG-JSON! ;

\ The declaration packet of verify-source's SUMTYPE, the CHECK operation's
\ path, names its token where the scanner read it.
: TEST-ORIGIN-DECL ( -- )
   CAP-ERR BUF-CAP DIAG-BUFFER!
   0 0= DIAG-JSON!
   ORIGIN-VERIFY-SUM 7108 T=                       \ E-TDECL-ARITY
   DIAG-BUFFER$ s\" \"token\":\"VARIANT\"" CONTAINS? TTRUE
   DIAG-BUFFER$ s\" \"line\":8,\"column\":4,\"byte_start\":119,\"byte_end\":126" CONTAINS? TTRUE
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
   CAP-ERR erru S\" \qword\q:\qCKT-DUP\q," CONTAINS? TTRUE
   CAP-ERR erru S\" \qline\q:2,\qcolumn\q:3,\qbyte_start\q:33,\qbyte_end\q:40," CONTAINS? TTRUE ;

\ A source list lints each file it names, so the finding names that file.
: RESERVED-LIST-AT$ ( -- ptr u8 n )
   SB-RESET
   LIST$ SB-APPEND
   s" :1:10: `I`" SB-APPEND
   SB$ ;

: RESERVED-LIST-TEST ( -- )
   LIST$ RESERVED$ WRITE-ALL
   LIST$ LIST-RUN 1 T=
   {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru s" E-RESERVED-DEFINITION" CONTAINS? TTRUE
   CAP-ERR erru RESERVED-LIST-AT$ CONTAINS? TTRUE ;

: AUDITED-LIB-TEST ( -- )
   \ check.f in a process of its own, which never loaded lib/test.f, checks
   \ the source list as this harness does in process.
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
\ tools/object-image.f loads its target's image writers in target arms, and
\ src/habu/driver-io.f, which it loads after them, calls their ASM-CODE.
\ tools/native-emit.f's writers read src/os/image-bytes.f's MSIZE as they load.
: TEST-IMAGE-TOOL-SOURCES ( -- )
   s" tools/engine-size.f" CHECK-TOOL-SOURCE
   s" tools/imgdump.f" CHECK-TOOL-SOURCE
   s" test/gate-images.f" CHECK-TOOL-SOURCE
   s" tools/object-image.f" CHECK-TOOL-SOURCE
   s" tools/native-emit.f" CHECK-TOOL-SOURCE ;

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
   \ Nor is it linted: src/core/include.f defines the loaders the lint reserves.
   s" src/core/include.f" LIST$ CLI-ALL-LIST EXPECT-CHECKED
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

\ A refusal only the run reaches - text `evaluate` runs, which the pre-pass
\ never reads - exits 70 as the pre-pass's refusal of the same text does. Its
\ throw keeps its own code past every handler, and the load exits the refusal
\ status for it because the checker rendered that refusal.
: RUN-REFUSED ( ptr u8 n ptr u8 n -- ) {: src:ptr srcu:n code:ptr codeu:n :}
   src srcu DIRECT-JSON-STDIN 70 T=
   {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru code codeu CONTAINS? TTRUE ;

: TEST-RUN-REFUSALS ( -- )
   s\" s\" SUMTYPE ckrr 0 VARIANT same ;VARIANT VARIANT same ;VARIANT ;SUMTYPE\" evaluate"
   s" E-BAD-DECLARATION" RUN-REFUSED
   s\" package CKRR-USG public : CKRR-GW ( -- n ) 1 ; ;package : CKRR-GW ( n n -- n ) + ; s\" using CKRR-USG : CKRR-R1 ( -- n ) CKRR-GW ; ;using\" evaluate"
   s" E-USING-SHADOW-GLOBAL" RUN-REFUSED
   \ The row names a word, so the load parses its signature.
   s\" : CKRR-SIG ( -- n ) 1 ; s\\\" s\\q CKRR-SIG\\q s\\q -- ckrr-no-such-type\\q trust\" evaluate"
   s" E-BAD-STORED-SIGNATURE" RUN-REFUSED ;

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
\ The engine writes that refusal as prose, which --json-errors puts on stdout.
: NOM-REDRIVE-TEST ( -- )
   NOM-ALL-BARE$ ALL-JSON-STDIN 70 T=
   {: outu:n erru:n :}
   CAP-OUT outu s" E-UNDEFINED: DEFTYPE" CONTAINS? TTRUE
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

\ A cast whose signature does not parse, here a bare `ptr` that also hides a
\ wrong-arity layout, is refused at the cast gate before any walk reads the row:
\ the all-errors run renders the bad stored signature and exits 70.
: CAST-BADSIG$ ( -- ptr u8 n )
   SB-RESET
   s" STRUCTURE ckt-cbox 1 FIELD value a ;STRUCTURE" SB-APPEND
   $0a SB-APPEND-C
   s" cast: CKT-CBAD ( ptr -- ckt-cbox )" SB-APPEND
   SB$ ;

: CAST-BADSIG-ALL ( -- )
   CAST-BADSIG$ DIRECT-ALL-STDIN 70 T=
   {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru s" habu: in CKT-CBAD: bad stored signature 'ptr -- ckt-cbox'"
   CONTAINS? TTRUE ;

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

\ ---- a source of any size reaches a verdict ----------------------------------
\ Every buffer that holds the source grows to it, so a source past a megabyte
\ passes through a file, through standard input and through SOURCE, and the
\ engine loads the run file that holds it behind the prefix, with the origin
\ marks. Each child gets a scratch root of its own as HB_TMP, and however it
\ ends - a verdict, a refusal or a die - it must leave that root empty.

$180000 constant BIG-SOURCE-LEN

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
4 constant MODE-VERIFY                   \ check.f --verify-only

: MODE-ARGS ( n -- ) {: mode:n :}
   mode MODE-JSON and 0<> if s" --json-errors" CHECK-ARG+ then
   mode MODE-ALL and 0<> if s" --all-errors" CHECK-ARG+ then
   mode MODE-VERIFY and 0<> if s" --verify-only" CHECK-ARG+ then ;

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
\ pass marks a definition at the end of the source.
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

: BIG-SOURCE-BODY ( n ptr u8 NUM:alloc-byte-len -- )
   {: u:n src:ptr extent:NUM:alloc-byte-len :}
   src u SIZED-FILL
   src u SCRATCH-FILE-RUN EXPECT-PASS
   src u SCRATCH-STDIN-RUN EXPECT-PASS
   RESET
   src u s" big-source.f" SOURCE
   RUN 0 T=
   RESET ;

: TEST-BIG-SOURCE ( -- )
   BIG-SOURCE-LEN dup MEM:BYTES-ALLOC-LEN
   [: BIG-SOURCE-BODY ;] MEM:WITH-BYTES ;

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

\ A source that dies when it runs ends the check with its exit code.
: TEST-DIE-SCRATCH ( -- )
   DIE$ SCRATCH-STDIN-RUN {: outu:n erru:n rc:n :}
   rc 5 T=
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

\ ---- every refusal after an undefined word ------------------------------------
\ An undefined word refuses CKT-UONE. A later definition sees it by its declared
\ effect: CKT-UCALL uses that effect and certifies, CKT-UMISUSE calls it with
\ nothing on the stack and is refused, and CKT-UBAD is refused on its own.
\ --all-errors and --verify-only report the three refusals, one packet each, and
\ --verify-only writes nothing to standard output; without --all-errors check.f
\ stops at the first.

: EVERY-SOURCE$ ( -- ptr u8 n )
   s\" : CKT-UONE ( n -- n ) NOPE ;\n: CKT-UCALL ( n -- n ) CKT-UONE 1 + ;\n: CKT-UMISUSE ( -- n ) CKT-UONE ;\n: CKT-UBAD ( n -- n ) dup ;\n" ;

: EVERY-HAS? ( n ptr u8 n -- bool ) {: erru:n s:ptr su:n :}
   CAP-ERR erru s su CONTAINS? ;

\ check.f in the given modes refuses the source: what it wrote to standard
\ output and to standard error.
: EVERY-RUN ( n ptr u8 n -- n n ) {: mode:n label:ptr labelu:n :}
   EVERY-SOURCE$ mode CAP-ERR BUF-CAP SCRATCH-MODE-RUN
   {: outu:n erru:n rc:n :}
   label labelu T-LABEL rc 70 T=
   SCRATCH-EMPTY
   outu erru ;

: EVERY-REPORTED ( n ptr u8 n -- ) {: erru:n label:ptr labelu:n :}
   label labelu T-LABEL CAP-ERR erru 10 COUNT-CHAR 3 T=
   label labelu T-LABEL erru s\" \"word\":\"ckt-uone\",\"token\":\"NOPE\"" EVERY-HAS? TTRUE
   label labelu T-LABEL erru s\" \"word\":\"ckt-umisuse\",\"token\":\"CKT-UONE\"" EVERY-HAS? TTRUE
   label labelu T-LABEL erru s\" \"word\":\"ckt-ubad\",\"token\":\"dup\"" EVERY-HAS? TTRUE
   label labelu T-LABEL erru s\" \"word\":\"ckt-ucall\"" EVERY-HAS? TFALSE ;

: TEST-EVERY-REFUSAL ( -- )
   MODE-ALL MODE-JSON or s" every-refusal: --all-errors" EVERY-RUN nip
   s" every-refusal: --all-errors" EVERY-REPORTED
   MODE-VERIFY s" every-refusal: --verify-only" EVERY-RUN {: outu:n erru:n :}
   s" every-refusal: --verify-only's stdout" T-LABEL outu 0 T=
   erru s" every-refusal: --verify-only" EVERY-REPORTED
   MODE-JSON s" every-refusal: --json-errors" EVERY-RUN nip {: first:n :}
   s" every-refusal: --json-errors reports the first" T-LABEL
   first s\" \"word\":\"ckt-uone\",\"token\":\"NOPE\"" EVERY-HAS? TTRUE
   s" every-refusal: --json-errors stops there" T-LABEL
   first s\" \"word\":\"ckt-umisuse\"" EVERY-HAS? TFALSE ;

\ An uncheckable definition is reported before its record, which a name that
\ keys no record refuses by a throw (checker.f CHECKER-RECORD-NAME), so prose
\ --all-errors reports both: the definition's undefined word, then the record.
: EVERY-MALNAME$ ( -- ptr u8 n )
   s" : CKT:UQ:B ( n -- n ) NOPE ;" ;

: TEST-EVERY-RECORD ( -- )
   EVERY-MALNAME$ DIRECT-ALL-STDIN 70 T= {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru s" E-UNDEFINED habu: in ckt:uq:b: undefined word 'NOPE'" CONTAINS? TTRUE
   CAP-ERR erru s" E-BAD-QUALIFIED-RECORD habu: record for 'ckt:uq:b' refused" CONTAINS? TTRUE ;

\ ---- a report past the scratch -----------------------------------------------
\ --all-errors renders every refusal into a scratch of
\ CHECK-ALL-ERRORS:SCRATCH-CAP bytes and streams each record to standard
\ error. Each JSON record here is over 400 bytes, so three thousand refusals
\ pass the scratch: the run reports the records that fit, each whole, then the
\ next one's refusal as an E-STATEMENT-THROW record whose throw_code is
\ E-DIAG-CAPACITY (-2901), and exits 70. A full scratch must neither end the
\ process nor lose the records before it. The capture holds twice the scratch.

3000 constant FULL-REFUSAL-N
FULL-REFUSAL-N REFUSAL-LINE-LEN * constant FULL-SOURCE-LEN
CHECK-ALL-ERRORS:SCRATCH-CAP 2 * constant FULL-ERR-CAP

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

\ ---- a record past the render buffer -----------------------------------------
\ The renderer holds each record in its own 16 KiB (src/core/render.f RSBUF)
\ before the scratch sees it. A word of WIDE-N backslashes is within the 7999
\ bytes `:` defines, and each byte is two in a JSON string, so the record of its
\ refusal, which names the word and echoes its source, passes that buffer. The
\ run reports the refusal before it, then that one's as an E-STATEMENT-THROW
\ record whose throw_code is E-DIAG-CAPACITY (-2901), and exits 70.

7900 constant WIDE-N

: WIDE-HEAD$ ( -- ptr u8 n )
   s\" : CKT-R ( n -- n ) dup ;\n: " ;

: WIDE-TAIL$ ( -- ptr u8 n )
   s\"  ( n -- n ) dup ;\n" ;

WIDE-HEAD$ nip WIDE-N + WIDE-TAIL$ nip + constant WIDE-SOURCE-LEN

: WIDE-FILL ( ptr u8 -- ) {: a:ptr :}
   WIDE-HEAD$ {: h:ptr hu:n :}
   WIDE-TAIL$ {: t:ptr tu:n :}
   h a hu BYTE-COPY
   WIDE-SOURCE-LEN tu - hu ?do $5c a i + c! loop
   t a WIDE-SOURCE-LEN + tu - tu BYTE-COPY ;

: RENDER-FULL-BODY ( ptr u8 NUM:alloc-byte-len -- )
   {: a:ptr extent:NUM:alloc-byte-len :}
   a WIDE-SOURCE-LEN + {: err:ptr :}
   a WIDE-FILL
   a WIDE-SOURCE-LEN MODE-ALL MODE-JSON or err REPORT-ERR-CAP SCRATCH-MODE-RUN
   {: outu:n erru:n rc:n :}
   rc 70 T=
   err erru s\" \"word\":\"ckt-r\"" CONTAINS? TTRUE
   err erru LAST-LINE-AT {: cut:n :}
   err cut + erru cut - {: last:ptr lastu:n :}
   last lastu s\" \"code\":\"E-STATEMENT-THROW\"" CONTAINS? TTRUE
   last lastu s\" \"throw_code\":-2901" CONTAINS? TTRUE
   SCRATCH-EMPTY ;

: TEST-RENDER-FULL ( -- )
   WIDE-SOURCE-LEN REPORT-ERR-CAP + MEM:BYTES-ALLOC-LEN
   [: RENDER-FULL-BODY ;] MEM:WITH-BYTES ;

\ The sig recorder renders each certified word's effect through the same buffer.
\ USE declares none, so it records the one the checker infers: six values of a
\ 12-parameter family whose name and arguments are each EFFECT-N bytes, the
\ longest a declared name may be. A value renders in about 3.3 KB, the six past
\ that buffer. The run reports it as an E-STATEMENT-THROW record whose
\ throw_code is E-DIAG-CAPACITY (-2901) and exits 70; the refusal after it
\ would be the last record had the throw been lost.

TFAM:TF-NAME-MAX constant EFFECT-N

\ EFFECT-N bytes of the given letter.
: EFFECT-NAME ( n -- ) {: c:n :}
   EFFECT-N 0 ?do c LONG-C loop ;

: EFFECT$ ( -- ptr u8 n )
   0 LONG-U !
   s" NEWTYPE " LONG-PUT
   $62 EFFECT-NAME
   s\"  0\nNEWTYPE " LONG-PUT
   $61 EFFECT-NAME
   s\"  12\nTRUSTED: MK ( -- " LONG-PUT
   $61 EFFECT-NAME
   s" <" LONG-PUT
   $62 EFFECT-NAME
   11 0 ?do
      s" ," LONG-PUT
      $62 EFFECT-NAME
   loop
   s\" > ) 0 ;\n: USE MK MK MK MK MK MK ;\n: CKT-R ( n -- n ) dup ;\n" LONG-PUT
   LONG-BUF LONG-U @ ;

: TEST-RENDER-FULL-EFFECT ( -- )
   EFFECT$ MODE-JSON CAP-ERR BUF-CAP SCRATCH-MODE-RUN
   {: outu:n erru:n rc:n :}
   rc 70 T=
   CAP-ERR erru LAST-LINE-AT {: cut:n :}
   CAP-ERR cut + erru cut - {: last:ptr lastu:n :}
   last lastu s\" \"code\":\"E-STATEMENT-THROW\"" CONTAINS? TTRUE
   last lastu s\" \"throw_code\":-2901" CONTAINS? TTRUE
   SCRATCH-EMPTY ;

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

\ A refused declaration the checker rendered exits 70 from the load.
: DEF-LOAD-REFUSED ( n n n -- )
   70 T=
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
\ The load exits 70 for it, as check.f does, because the checker rendered it.
: RESERVED-LOAD-REFUSED ( n n n ptr u8 n -- ) {: want:ptr wantu:n :}
   70 T=
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

\ A definer, a parsing word, or `undefine`, with nothing after it has no name
\ to read. Each source puts one alone on its second line, two columns in: the
\ loader refuses the source, and the check refuses it at that place, naming
\ that word, without reading past the last token: in prose and under
\ --all-errors by the one line the nominal pass writes, and in JSON. The
\ pre-verifier reads the names the nominal pass does not list (`defer`,
\ `create`, a learned definer, `char`, a field word) and refuses them by that
\ same line. Checked as a file, whose statements are also walked to place the
\ files it loads, it is refused once.
: NONAME-SRC$ ( ptr u8 n ptr u8 n -- ptr u8 n ) {: a:ptr u:n d:ptr du:n :}
   SB-RESET
   a u SB-APPEND $0a SB-APPEND-C
   s"   " SB-APPEND
   d du SB-APPEND $0a SB-APPEND-C
   SB$ ;

: NONAME-LINE$ ( ptr u8 n -- ptr u8 n )
   {: d:ptr du:n :}
   SB-RESET
   s" check.f: <stdin>:2:3: missing name after '" SB-APPEND
   d du SB-APPEND 39 SB-APPEND-C
   SB$ ;

: NONAME-PROSE ( n n n ptr u8 n -- )
   {: outu:n erru:n rc:n d:ptr du:n :}
   rc 70 T=
   outu 0 T=
   CAP-ERR erru d du NONAME-LINE$ CONTAINS? TTRUE
   CAP-ERR erru 10 COUNT-CHAR 1 T= ;

: NONAME-REFUSED ( ptr u8 n ptr u8 n -- )
   {: a:ptr u:n d:ptr du:n :}
   a u d du NONAME-SRC$ HB-LOAD-SRC
   {: outu:n erru:n rc:n :}
   rc 0 T<>
   a u d du NONAME-SRC$ DIRECT-STDIN d du NONAME-PROSE
   a u d du NONAME-SRC$ DIRECT-ALL-STDIN d du NONAME-PROSE
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
   s" \ lead" s" undefine" NONAME-REFUSED
   s" \ lead" s" defer" NONAME-REFUSED
   s" \ lead" s" cast:" NONAME-REFUSED
   s" \ lead" s" using" NONAME-REFUSED
   s" \ lead" s" EXPORT" NONAME-REFUSED
   s" \ lead" s" create" NONAME-REFUSED
   s" \ lead" s" variable" NONAME-REFUSED
   s" \ lead" s" constant" NONAME-REFUSED
   s" \ lead" s" PTR-VARIABLE" NONAME-REFUSED
   s" \ lead" s" PERSISTED-PTR-VARIABLE" NONAME-REFUSED
   s" \ lead" s" PTR-U8-TABLE" NONAME-REFUSED
   s" \ lead" s" PERSISTED-PTR-U8-TABLE-VARIABLE" NONAME-REFUSED
   s" \ lead" s" RESERVED-PTR-U8-CELL" NONAME-REFUSED
   s" \ lead" s" BEGIN-STRUCTURE" NONAME-REFUSED
   s" \ lead" s" '" NONAME-REFUSED
   s" \ lead" s" char" NONAME-REFUSED
   s" : CKC ( -- n )" s" char" NONAME-REFUSED
   s" : CKC ( -- n )" s" [char]" NONAME-REFUSED
   s" : CKC ( -- n )" s" [']" NONAME-REFUSED
   s" BEGIN-STRUCTURE CKB" s" +FIELD" NONAME-REFUSED
   s" BEGIN-STRUCTURE CKB" s" CFIELD:" NONAME-REFUSED
   s" BEGIN-STRUCTURE CKB" s" PTR-FIELD:" NONAME-REFUSED
   s" : CKM ( n -- ) create , does> ( -- ptr n ) ;" s" CKM" NONAME-REFUSED
   s" \ lead" s" generates:" NONAME-REFUSED
   s" require lib/ffi-abi.f" s" FUNCTION:" NONAME-REFUSED ;

\ A parsing keyword takes the next whitespace-delimited token raw, whatever it
\ spells, in every scanner of the check as in the loader, and a definer takes
\ its name the same way; either operand is data. After `char \`, `[char] \`
\ and `char (` the second line is ordinary source, so the number-shaped
\ definition there is refused as it is anywhere, and so it is after `create
\ char`, which names a word `char` that takes nothing. The top-level `char`
\ values are dropped, so each source has a closed load. `char s"` and `[char] s"`
\ open no string, so each source loads and checks alike and prints 115. A check
\ of standard input skips source discovery, so these sources are checked as the
\ file the load read. `' :` names no word, so the load refuses it as an
\ undefined tick, and the check refuses it the same way, never as a definition
\ with no name; it starts no definition, so the nominal pass reads the line after
\ it as top-level source. `[char] ;` ends none, so the pass does not read the
\ local `newtype` after it as a declaration.
: NUMERIC-REFUSED ( n n n -- )
   1 T=
   {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru s" E-NUMERIC-DEFINITION" CONTAINS? TTRUE
   CAP-ERR erru s\" \"word\":\"42\"" CONTAINS? TTRUE ;

: RAW-NUMERIC ( ptr u8 n ptr u8 n -- ) {: a:ptr u:n b:ptr v:n :}
   a u b v OPERAND-SRC$ HB-LOAD-SRC NOM-LOAD-ADMITTED
   a u b v OPERAND-SRC$ DIRECT-JSON-STDIN NUMERIC-REFUSED ;

: RAW-PRINTS ( ptr u8 n ptr u8 n -- ) {: a:ptr u:n want:ptr wantu:n :}
   a u HB-LOAD-SRC 0 T=
   {: outu:n erru:n :}
   erru 0 T=
   CAP-OUT outu want wantu CONTAINS? TTRUE
   BAD$ PATH-RUN 0 T=
   {: outu2:n erru2:n :}
   erru2 0 T=
   CAP-OUT outu2 want wantu CONTAINS? TTRUE ;

: RAW-ADMITTED ( ptr u8 n -- )
   HB-LOAD-SRC NOM-LOAD-ADMITTED
   BAD$ PATH-RUN EXPECT-ACCEPTED ;

\ The load refuses a tick of `:` as an undefined word, never as a missing name;
\ the check refuses it the same way, at the token, before anything runs.
: RAW-TICK-KEYWORD ( -- )
   s" ' :" HB-LOAD-SRC 70 T=
   {: outu:n erru:n :}
   CAP-ERR erru s" E-UNDEFINED: :" CONTAINS? TTRUE
   BAD$ PATH-RUN 70 T=
   {: outu2:n erru2:n :}
   CAP-ERR erru2 s\" \"code\":\"E-UNDEFINED-TOP-LEVEL\",\"repair_class\":\"unknown_rejection\",\"verdict\":\"rejected\",\"token\":\":\"" CONTAINS? TTRUE
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
   s" char \ drop" s" : 42 ( -- ) ;" RAW-NUMERIC
   s" : CKT-P ( -- n ) [char] \" s" ; : 42 ( -- ) ;" RAW-NUMERIC
   s" char ( drop" s" : 42 ( -- ) ;" RAW-NUMERIC
   s" create char" s" : 42 ( -- ) ;" RAW-NUMERIC
   s\" char s\" constant CKT-SQ CKT-SQ ." s" 115" RAW-PRINTS
   s\" : CKT-Q ( -- n ) [char] s\" ; CKT-Q ." s" 115" RAW-PRINTS
   RAW-TICK-KEYWORD
   RAW-LOCAL$ RAW-ADMITTED
   s" ' :" s" DEFLINEAR N" OPERAND-SRC$ HB-LOAD-SRC RAW-TICK-LOAD-REFUSED
   s" ' :" s" DEFLINEAR N" OPERAND-SRC$ NOM-CHECK-REFUSED ;

\ A body looks a token up among its live locals before the parsing keywords, as
\ the loader and the checker do, byte for byte: after `{: KW :}` the `KW` pushes
\ the local, takes no operand, and the `;` after it ends the definition. `.(`
\ is the same: the loader looks it up after the locals, unlike the `\` and `(`
\ comments. Each form loads and checks alike and prints 5, the `42` after one
\ is refused and `DEFLINEAR N` after one is refused at its name, on line 2.
\ Measured before every scanner of the check looked locals up: the
\ pre-verifier refused the `char`, `[char]` and `.(` forms (rc 74,
\ unterminated definition) though they load, and the reserved-name lint and
\ the nominal pass read past the `;` of every form, so the `42` after the
\ `[']` and `'` forms was admitted and `DEFLINEAR N` after any form was
\ refused with no location.
: LOCAL-KW$ ( ptr u8 n ptr u8 n -- ptr u8 n ) {: kw:ptr kwu:n tail:ptr tailu:n :}
   SB-RESET
   s" : CKT-LK ( n -- n ) {: " SB-APPEND  kw kwu SB-APPEND
   s"  :} " SB-APPEND  kw kwu SB-APPEND  s"  ;" SB-APPEND
   $0a SB-APPEND-C  tail tailu SB-APPEND
   SB$ ;

: LOCAL-NOMINAL ( ptr u8 n -- ) {: kw:ptr kwu:n :}
   kw kwu s" DEFLINEAR N" LOCAL-KW$ HB-LOAD-SRC NOM-LOAD-REFUSED
   kw kwu s" DEFLINEAR N" LOCAL-KW$ DIRECT-JSON-STDIN 70 T=
   {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru s\" \"code\":\"E-BAD-NOMINAL-TYPE\"" CONTAINS? TTRUE
   CAP-ERR erru s\" \"token\":\"N\"" CONTAINS? TTRUE
   CAP-ERR erru s\" \"line\":2,\"column\":11," CONTAINS? TTRUE ;

: LOCAL-KW ( ptr u8 n -- ) {: kw:ptr kwu:n :}
   kw kwu s" 5 CKT-LK ." LOCAL-KW$ s" 5" RAW-PRINTS
   kw kwu s" : 42 ( -- ) ;" LOCAL-KW$ HB-LOAD-SRC NOM-LOAD-ADMITTED
   kw kwu s" : 42 ( -- ) ;" LOCAL-KW$ DIRECT-JSON-STDIN NUMERIC-REFUSED ;

\ `['] \` in a body ticks a word named `\`, as the loader reads it, and the
\ check runs the source and prints 7. Measured before `[']` joined the
\ pre-verifier's body keywords: its operand opened a comment there and hid the
\ `;` (rc 74, unterminated definition).
: TICK-NAME$ ( -- ptr u8 n )
   SB-RESET
   s" : \ ( -- ) ;" SB-APPEND $0a SB-APPEND-C
   s" : CKT-TB ( -- [ -- ] ) ['] \ ;" SB-APPEND $0a SB-APPEND-C
   s" CKT-TB drop 7 ." SB-APPEND
   SB$ ;

\ An enclosing local is still that local inside a quotation: the loader refuses
\ it at that token (rc 75) and the check refuses it as a local in a quotation,
\ after the `;` that ends the definition. Measured before: the pre-verifier read
\ the `;` as the keyword's operand (rc 74, unterminated definition).
: LOCAL-QUOTATION$ ( -- ptr u8 n )
   s" : CKT-LQ ( n -- n ) {: char :} [: char ;" ;

: LOCAL-QUOTATION ( -- )
   LOCAL-QUOTATION$ HB-LOAD-SRC 75 T=
   {: outu:n erru:n :}
   CAP-ERR erru s" char at" CONTAINS? TTRUE
   LOCAL-QUOTATION$ DIRECT-JSON-STDIN 70 T=
   {: outu2:n erru2:n :}
   outu2 0 T=
   CAP-ERR erru2 s\" \"code\":\"E-BAD-LOCAL-SHAPE\"" CONTAINS? TTRUE ;

\ A parsing keyword outside its own state is refused by the loader and the check
\ alike: `[char]` at top level, and `char` and `'` in a body. The checker
\ refuses `[char]` and `'`; `char` in a body passes it, and the run stage's
\ engine refuses it in prose, which --json-errors puts on stdout. Each leaves
\ the lengths of the check's capture.
: OUT-OF-STATE-RUNS ( ptr u8 n -- n n )
   {: a:ptr u:n :}
   a u HB-LOAD-SRC 70 T=
   {: outu:n erru:n :}
   CAP-ERR erru s" E-UNDEFINED" CONTAINS? TTRUE
   a u DIRECT-JSON-STDIN 70 T= ;

: OUT-OF-STATE ( ptr u8 n -- )
   OUT-OF-STATE-RUNS
   {: outu:n erru:n :}
   CAP-ERR erru s" E-UNDEFINED" CONTAINS? TTRUE ;

: OUT-OF-STATE-RUN ( ptr u8 n -- )
   OUT-OF-STATE-RUNS
   {: outu:n erru:n :}
   erru 0 T=
   CAP-OUT outu s" E-UNDEFINED: char" CONTAINS? TTRUE ;

\ A body binding a 65th local is refused by the loader at that local (rc 70),
\ and the check refuses it the same way, with the checker's located
\ E-TOO-MANY-LOCALS. Measured before the pre-verifier stopped recording locals
\ at the engine's cap: it died first, rc 74 with no location.
: MANY-LOCALS$ ( -- ptr u8 n )
   SB-RESET
   s" : CKT-ML ( " SB-APPEND
   65 0 ?do s" n " SB-APPEND loop
   s" -- ) {:" SB-APPEND
   65 0 ?do s"  l" SB-APPEND i 1 + FMT:SB-U loop
   s"  :} ;" SB-APPEND
   SB$ ;

: MANY-LOCALS ( -- )
   MANY-LOCALS$ HB-LOAD-SRC 70 T=
   {: outu:n erru:n :}
   CAP-ERR erru s" more than 64 locals in one definition: l65" CONTAINS? TTRUE
   MANY-LOCALS$ DIRECT-JSON-STDIN 70 T=
   {: outu2:n erru2:n :}
   outu2 0 T=
   CAP-ERR erru2 s\" \"code\":\"E-TOO-MANY-LOCALS\"" CONTAINS? TTRUE
   CAP-ERR erru2 s\" \"token\":\"l65\"" CONTAINS? TTRUE
   CAP-ERR erru2 s\" \"line\":1,\"column\":397," CONTAINS? TTRUE ;

\ The 65-local body and the DEFLINEAR sources come last: a pre-verifier that
\ dies on the first, or a nominal pass that misses one of the others and hands
\ it to the pre-verifier's registration, ends this process.
: TEST-LOCAL-OPERAND ( -- )
   s" char" LOCAL-KW
   s" [char]" LOCAL-KW
   s" [']" LOCAL-KW
   s" '" LOCAL-KW
   s" .(" LOCAL-KW
   TICK-NAME$ s" 7" RAW-PRINTS
   LOCAL-QUOTATION
   s" [char] A ." OUT-OF-STATE
   s" : CKT-OC ( -- n ) char A ;" OUT-OF-STATE-RUN
   s" : CKT-OT ( -- n ) ' dup drop 1 ;" OUT-OF-STATE
   MANY-LOCALS
   s" char" LOCAL-NOMINAL
   s" [char]" LOCAL-NOMINAL
   s" [']" LOCAL-NOMINAL
   s" '" LOCAL-NOMINAL
   s" .(" LOCAL-NOMINAL ;

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

\ A value-record field with a scoped dependency, which the registration
\ refuses before it stores the field. The loader dies with its message and
\ rc 70; the check names the field at its line and column, in prose with and
\ without --all-errors and in JSON with the registration's reason.
: VREC-SCOPED$ ( -- ptr u8 n )
   s" VALUE-RECORD ckn-w value read-view<p,q,u8> END-VALUE-RECORD" ;

: TEST-VREC-SCOPED-REFUSED ( -- )
   VREC-SCOPED$ HB-LOAD-SRC 70 T=
   {: outu:n erru:n :}
   CAP-ERR erru s" checker: value-record field contains a scoped dependency" CONTAINS? TTRUE
   VREC-SCOPED$ DIRECT-STDIN 70 T=
   {: outu2:n erru2:n :}
   outu2 0 T=
   CAP-ERR erru2 s" <stdin>:1:20: checker: value-record field contains a scoped dependency 'value'" CONTAINS? TTRUE
   VREC-SCOPED$ DIRECT-ALL-STDIN 70 T=
   {: outu3:n erru3:n :}
   outu3 0 T=
   CAP-ERR erru3 s" <stdin>:1:20: checker: value-record field contains a scoped dependency 'value'" CONTAINS? TTRUE
   VREC-SCOPED$ DIRECT-JSON-STDIN 70 T=
   {: outu4:n erru4:n :}
   outu4 0 T=
   CAP-ERR erru4 s\" \"code\":\"E-BAD-RECORD-FIELD\"" CONTAINS? TTRUE
   CAP-ERR erru4 s\" \"line\":1,\"column\":20," CONTAINS? TTRUE
   CAP-ERR erru4 s\" \"reason\":\"checker: value-record field contains a scoped dependency\"" CONTAINS? TTRUE ;

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

\ A program that runs the scanner's PRODUCT replay itself (src/habu/
\ verify-source.f RECORD-PRODUCT: CHECKER-DEFPRODUCT, then the constructors'
\ effects) holds a checked `CKDREP:MAKE` with no word behind it. The scan
\ cannot see that run, so the refusal of a later definition of the name comes
\ from the run, and it must name the definition and where it is, as every other
\ duplicate does: it used to exit 78 with nothing on stderr (src/core/
\ check-hook.f DUP-RC).
: PREPLAY-DUP$ ( -- ptr u8 n )
   SB-RESET
   S\" : CKT-PREPLAY ( -- ) s\q ckdrep\q s\q 0 FIELD x n\q CHECKER-DEFPRODUCT GENERATED-DECL-CTOR:REPLAY-LEGACY ;"
   SB-APPEND $0a SB-APPEND-C
   s" CKT-PREPLAY" SB-APPEND $0a SB-APPEND-C
   s" : CKDREP:MAKE ( n -- n ) 1 + ;" SB-APPEND $0a SB-APPEND-C
   SB$ ;

: TEST-PREPLAY-DUP ( -- )
   PREPLAY-DUP$ DIRECT-STDIN
   $4E T= {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru s" duplicate definition: CKDREP:MAKE at " CONTAINS? TTRUE ;

\ An ordinary PRODUCT or SUMTYPE, then a definition of a name it generated. The
\ scan replays the generated words, so the scan refuses the definition, and
\ check.f must name it and where it is, as --load does, in every form: the
\ record used to be a placeholder word at line 1.
: GDUP-PROD-AS$ ( ptr u8 n -- ptr u8 n ) {: def:ptr defu:n :}
   SB-RESET
   s" PRODUCT ckdprod 0 FIELD x n ;PRODUCT" SB-APPEND $0a SB-APPEND-C
   def defu SB-APPEND $0a SB-APPEND-C
   SB$ ;

: GDUP-PROD$ ( -- ptr u8 n )
   s" : CKDPROD:MAKE ( n -- n ) 1 + ;" GDUP-PROD-AS$ ;

: GDUP-SUM$ ( -- ptr u8 n )
   SB-RESET
   s" SUMTYPE ckdsum 0 VARIANT ckdone n ;VARIANT ;SUMTYPE" SB-APPEND $0a SB-APPEND-C
   s" : CKDSUM:CKDONE ( n -- n ) 1 + ;" SB-APPEND $0a SB-APPEND-C
   SB$ ;

: EXPECT-GDUP-PROSE ( n n n ptr u8 n -- ) {: outu:n erru:n rc:n la:ptr lu:n :}
   rc $4E T=
   outu 0 T=
   CAP-ERR erru la lu CONTAINS? TTRUE ;

\ The packet names the definition and where its name starts, and it is the
\ only line on stderr.
: EXPECT-GDUP-JSON ( n n n ptr u8 n ptr u8 n -- )
   {: outu:n erru:n rc:n wa:ptr wu:n pa:ptr pu:n :}
   rc $4E T=
   outu 0 T=
   CAP-ERR erru s" E-DUPLICATE-DEFINITION" CONTAINS? TTRUE
   CAP-ERR erru wa wu CONTAINS? TTRUE
   CAP-ERR erru pa pu CONTAINS? TTRUE
   CAP-ERR erru 10 COUNT-CHAR 1 T= ;

\ The source, then the prose line, the packet's word and its place, in the
\ default mode, --json-errors, --all-errors and both.
: EXPECT-GDUP-FORMS ( [ -- ptr u8 n ] ptr u8 n ptr u8 n ptr u8 n -- )
   {: src line:ptr lineu:n wa:ptr wu:n pa:ptr pu:n :}
   src execute DIRECT-STDIN line lineu EXPECT-GDUP-PROSE
   src execute DIRECT-JSON-STDIN wa wu pa pu EXPECT-GDUP-JSON
   src execute DIRECT-ALL-STDIN line lineu EXPECT-GDUP-PROSE
   src execute ALL-JSON-STDIN wa wu pa pu EXPECT-GDUP-JSON ;

: GDUP-PROD-LINE$ ( -- ptr u8 n )
   S\" duplicate definition: CKDPROD:MAKE at <stdin>:2\n" ;

: GDUP-PROD-WORD$ ( -- ptr u8 n )
   S\" \qword\q:\qCKDPROD:MAKE\q,\qtoken\q:\qCKDPROD:MAKE\q,\qtoken_index\q:0," ;

: TEST-GDUP-PRODUCT ( -- )
   [: GDUP-PROD$ ;] GDUP-PROD-LINE$ GDUP-PROD-WORD$
   S\" \qfile\q:\q<stdin>\q,\qline\q:2,\qcolumn\q:3,\qbyte_start\q:39,\qbyte_end\q:51,"
   EXPECT-GDUP-FORMS ;

: TEST-GDUP-SUMTYPE ( -- )
   [: GDUP-SUM$ ;]
   S\" duplicate definition: CKDSUM:CKDONE at <stdin>:2\n"
   S\" \qword\q:\qCKDSUM:CKDONE\q,\qtoken\q:\qCKDSUM:CKDONE\q,\qtoken_index\q:0,"
   S\" \qfile\q:\q<stdin>\q,\qline\q:2,\qcolumn\q:3,\qbyte_start\q:54,\qbyte_end\q:67,"
   EXPECT-GDUP-FORMS ;

\ The same name taken by the other rows the scan refuses it at: a typed storage
\ definer, a cast and a re-export. A private PRODUCT puts its constructor's tail
\ in the current section, where a re-export of the same tail lands.
: GDUP-EXPORT$ ( -- ptr u8 n )
   SB-RESET
   s" package CKDXP" SB-APPEND $0a SB-APPEND-C
   s" public" SB-APPEND $0a SB-APPEND-C
   s" : CKDPRIV-MAKE ( -- ) ;" SB-APPEND $0a SB-APPEND-C
   s" ;package" SB-APPEND $0a SB-APPEND-C
   s" package CKDXQ" SB-APPEND $0a SB-APPEND-C
   s" private" SB-APPEND $0a SB-APPEND-C
   s" PRODUCT ckdpriv 0 FIELD x n ;PRODUCT" SB-APPEND $0a SB-APPEND-C
   s" EXPORT CKDXP:CKDPRIV-MAKE" SB-APPEND $0a SB-APPEND-C
   s" ;package" SB-APPEND $0a SB-APPEND-C
   SB$ ;

: TEST-GDUP-DEFINERS ( -- )
   [: s" TYPED-VARIABLE CKDPROD:MAKE n" GDUP-PROD-AS$ ;] GDUP-PROD-LINE$ GDUP-PROD-WORD$
   S\" \qfile\q:\q<stdin>\q,\qline\q:2,\qcolumn\q:16,\qbyte_start\q:52,\qbyte_end\q:64,"
   EXPECT-GDUP-FORMS
   [: s" CAST: CKDPROD:MAKE ( n -- u8 )" GDUP-PROD-AS$ ;] GDUP-PROD-LINE$ GDUP-PROD-WORD$
   S\" \qfile\q:\q<stdin>\q,\qline\q:2,\qcolumn\q:7,\qbyte_start\q:43,\qbyte_end\q:55,"
   EXPECT-GDUP-FORMS
   [: GDUP-EXPORT$ ;]
   S\" duplicate definition: CKDXP:CKDPRIV-MAKE at <stdin>:8\n"
   S\" \qword\q:\qCKDXP:CKDPRIV-MAKE\q,\qtoken\q:\qCKDXP:CKDPRIV-MAKE\q,\qtoken_index\q:0,"
   S\" \qfile\q:\q<stdin>\q,\qline\q:8,\qcolumn\q:8,\qbyte_start\q:120,\qbyte_end\q:138,"
   EXPECT-GDUP-FORMS ;

\ ---- a loaded file is composed where its loader sits ------------------------
\ The pre-pass verifies a source and the files it loads in the order the loader
\ runs them (VERIFY:SOURCE-COMPOSE-LABELED-IN-SCOPE): the text before a
\ top-level `required` first, then the file it loads (once), then the rest. How
\ that can fail, and what holds each way:
\   - the loaded file checked ahead of the whole source, so a word the source
\     defines before its require is undefined in it (REQ-ORDER, the reduced
\     case behind every library that requires lib/aio.f);
\   - the loaded file's words visible to the source before the require
\     (REQ-EARLY), or the source's later words visible to the loaded file
\     (REQ-LATE): the load path refuses both E-UNDEFINED, and so must this;
\   - a file loaded twice when two requires or two files name it (REQ-ONCE):
\     a second verification is E-DUPLICATE-DEFINITION;
\   - a require cycle looping or reordering (REQ-CYCLE-SPLIT): a file already
\     being composed is a no-op, as `required` of a registered path is;
\   - a loader in a colon body, which runs when the word does: its file waits
\     until that definition and any package around it close, at the first
\     neutral top-level point (PEND-RELEASE), and after a top-level loader in
\     that package, which runs first (REQ-BODY, the shape of lib/aio.f's
\     AIO-LOAD:HOST); a loader inside a control word is left to the run;
\   - a top-level loader inside a package or under a `using`, composed where it
\     sits, inside that scope: the loader runs the file in that scope, so its
\     file and the rest of the source are checked in the scope the loader gives
\     them, and the file's own usings end with it (REQ-PKG, REQ-USING);
\   - a diagnostic after a loaded file naming the wrong file, line or column
\     (REQ-ORIGIN): the composition names the including file again;
\   - the check run loading in another order: it loads through the real loader,
\     and every accepted case here runs it;
\   - `--all-errors --source-list` checking whole files in dependency order, or
\     replaying whole files as support: it composes the same way in one
\     session, with the default check's verdict at the same file, line and
\     column (the -ALL cases);
\   - a refused definition taking the clean ones beside it down, so a later
\     file reports them undefined (REQ-CASC): the session keeps every clean
\     definition and a refused one's declared signature, as checking the whole
\     file at once does;
\   - a duplicate definition leaving a record of the word that a later clean
\     call is checked against (REQ-DUP): the duplicate stops the composition
\     in the file it names (SOURCE-COMPOSE-STOPPED$), as it ends the load;
\   - plain `--all-errors` checking the subject alone, so a word it takes from a
\     file it loads is undefined (REQ-USE): it composes the same files the
\     source list does, with the same verdict at the same place;
\   - a throw out of a statement in a later file, such as a `;using` with no
\     `using` open, escaping all-errors uncaught or, in prose, unreported
\     (REQ-THROW): it stops the composition with a diagnostic at the statement,
\     and the run ends with the checker's status;
\   - a storage declaration naming an unknown type after a refused one,
\     reported as a throw out of the statement or at the wrong file, line or
\     column (REQ-SIZE): it is the storage refusal at the type and the run ends
\     with the checker's status;
\   - the subject reached again through a require: the composition loads it
\     once, as `required` of a registered path does;
\   - a path resolved against the wrong directory: composition keeps
\     discovery's resolution against the entry root.
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

\ `--all-errors --source-list` composes the same files, each against the ones
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

\ The loaded file is composed after `required` on line 4, mid-line, so the
\ refused `dup` there is placed at the including file's own line and column.
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

\ A name with a second ':' after a non-edge first one keys no record, and the
\ checker refuses it where the record would be written (checker.f
\ CHECKER-RECORD-NAME) by a throw, so the check reports it as a statement that
\ threw, at the statement, in prose and JSON. Under --all-errors a definition
\ refused before it is still reported: ending the process there reported
\ nothing but the refusal's own line.
: MALNAME$ ( -- ptr u8 n )
   s" : CKT:MAL:NAME ( -- ) ;" ;

: MALNAME-AFTER$ ( -- ptr u8 n )
   s\" : CKT-MAL-BAD ( -- ) 1 ;\ndefer CKT:MAL:DEF ( -- )" ;

: TEST-MALFORMED-NAME ( -- )
   MALNAME$ DIRECT-STDIN 70 T= {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru s" E-STATEMENT-THROW <stdin>:1:23: throw 7152 at ';'" CONTAINS? TTRUE
   MALNAME$ DIRECT-JSON-STDIN 70 T= {: outu2:n erru2:n :}
   outu2 0 T=
   CAP-ERR erru2 s\" \"code\":\"E-BAD-QUALIFIED-RECORD\"" CONTAINS? TTRUE
   CAP-ERR erru2 s\" \"token\":\"ckt:mal:name\"" CONTAINS? TTRUE
   CAP-ERR erru2 s\" \"line\":1,\"column\":23," CONTAINS? TTRUE
   CAP-ERR erru2 s\" \"throw_code\":7152" CONTAINS? TTRUE ;

: TEST-MALFORMED-NAME-ALL ( -- )
   MALNAME-AFTER$ DIRECT-ALL-STDIN 70 T= {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru s" habu: in ckt-mal-bad: " CONTAINS? TTRUE
   CAP-ERR erru s" E-BAD-QUALIFIED-RECORD habu: record for 'ckt:mal:def' refused" CONTAINS? TTRUE
   CAP-ERR erru s" E-STATEMENT-THROW <stdin>:2:7: throw 7152 at 'CKT:MAL:DEF'" CONTAINS? TTRUE ;

\ A call to a malformed qualified name can never resolve, so its definition is
\ refused like any other and --all-errors goes on to report each later one, in
\ prose and JSON, one record per definition.
: MALCALL$ ( -- ptr u8 n )
   s\" : CKT-MAL-CALL ( -- ) CKT:MAL:CALL ;\n: CKT-MAL-LATER ( -- ) 1 ;" ;

: TEST-MALFORMED-CALL-ALL ( -- )
   MALCALL$ DIRECT-ALL-STDIN 70 T= {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru 10 COUNT-CHAR 2 T=
   CAP-ERR erru s" E-BAD-QUALIFIED habu: in ckt-mal-call: malformed qualified name 'CKT:MAL:CALL'" CONTAINS? TTRUE
   CAP-ERR erru s" habu: in ckt-mal-later: at '1'" CONTAINS? TTRUE
   MALCALL$ ALL-JSON-STDIN 70 T= {: outu2:n erru2:n :}
   outu2 0 T=
   CAP-ERR erru2 10 COUNT-CHAR 2 T=
   CAP-ERR erru2 s\" \"code\":\"E-BAD-QUALIFIED\",\"repair_class\":\"fix_qualified_name\",\"verdict\":\"rejected\",\"word\":\"ckt-mal-call\"" CONTAINS? TTRUE
   CAP-ERR erru2 s\" \"code\":\"E-MISMATCH\",\"repair_class\":\"remove_producer\",\"verdict\":\"rejected\",\"word\":\"ckt-mal-later\"" CONTAINS? TTRUE ;

\ The throw leaves a raw storage definer's signature (verify-source.f
\ RAW-TRUST-NEXT) mid-parse; a later check in the same process must still read
\ an ordinary signature's type variables as ordinary, not as raw cells.
: TEST-MALFORMED-RAW ( -- )
   s" variable CKT:MAL:VAR" DIRECT-STDIN 70 T= {: outu:n erru:n :}
   CAP-ERR erru s" throw 7152" CONTAINS? TTRUE
   s\" : CKT-MAL-KEEP ( a -- a ) ;\n: CKT-MAL-USE ( ptr u8 -- ptr u8 ) CKT-MAL-KEEP ;"
   DIRECT-STDIN 0 T= {: outu2:n erru2:n :}
   erru2 0 T= ;

\ A string the file never closes stops source discovery before any file is
\ composed. Every --json-errors mode reports it by the one record --all-errors
\ writes for it on standard input, in the file that holds it and at the string,
\ and fails as a refusal; a source that requires the file reports it there. The
\ record's token is the opener as written, so an escaped opener spans 3 bytes.
: UT-FILES ( -- )
   s" ckt-ut.f" REQ$ UNTERM-SDQ$ WRITE-ALL
   s" ckt-ut-esc.f" REQ$ UNTERM-ESC$ WRITE-ALL
   SB-RESET s" ckt-ut.f" REQ-LOAD+
   s" ckt-ut-req.f" REQ-WRITE ;

\ A refusal reported by one record: its code and class, token and place.
: EXPECT-ONE ( n n n ptr u8 n ptr u8 n ptr u8 n -- )
   {: outu:n erru:n rc:n code:ptr codeu:n tok:ptr toku:n at:ptr atu:n :}
   rc 70 T=
   outu 0 T=
   CAP-ERR erru 10 COUNT-CHAR 1 T=
   CAP-ERR erru code codeu CONTAINS? TTRUE
   CAP-ERR erru tok toku CONTAINS? TTRUE
   CAP-ERR erru at atu CONTAINS? TTRUE ;

: ONE-MODES ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- )
   {: a:ptr u:n code:ptr codeu:n tok:ptr toku:n at:ptr atu:n :}
   a u ST-JSON-RUN code codeu tok toku at atu EXPECT-ONE
   a u REQ-PLAIN-RUN code codeu tok toku at atu EXPECT-ONE
   a u REQ-ALL-RUN code codeu tok toku at atu EXPECT-ONE ;

: UT-CODE$ ( -- ptr u8 n )
   s\" \"code\":\"E-UNTERMINATED-STRING\",\"repair_class\":\"close_string\"," ;

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
   s" ckt-ut.f" UT-CODE$ UT-SDQ-TOKEN$ UT-SDQ-AT$ ONE-MODES
   s" ckt-ut-req.f" UT-CODE$ UT-SDQ-TOKEN$ UT-SDQ-AT$ ONE-MODES
   s" ckt-ut-esc.f" UT-CODE$ UT-ESC-TOKEN$ UT-ESC-AT$ ONE-MODES ;

\ A require of a file that is not there, of one the file system will not read,
\ or of a literal path within the 1024 bytes a loader word takes that resolves
\ past them, and a loader form discovery cannot follow, stop the require
\ closure. Every --json-errors mode reports each by one record at the loader
\ word, in the file that holds it, and fails as a refusal; a source that
\ requires the file reports it there.
: CL-LONG+ ( -- )
   s" require " SB-APPEND
   496 0 ?do s" a/" SB-APPEND loop
   s" absent.f" REQ-LINE+ ;

: CL-FILES ( -- )
   SB-RESET s" \ lost" REQ-LINE+ s" require ckt-cl-none.f" REQ-LINE+
   s" ckt-cl-lost.f" REQ-WRITE
   SB-RESET s" \ dyn" REQ-LINE+ s" PATH$ included" REQ-LINE+
   s" ckt-cl-dyn.f" REQ-WRITE
   SB-RESET s" ckt-cl-dyn.f" REQ-LOAD+
   s" ckt-cl-dyn-req.f" REQ-WRITE
   SB-RESET s" \ unread" REQ-LINE+ s" require ckt-cl-locked.f" REQ-LINE+
   s" ckt-cl-unread.f" REQ-WRITE
   SB-RESET s" \ locked" REQ-LINE+ s" ckt-cl-locked.f" REQ-WRITE
   s" ckt-cl-locked.f" REQ$ 0 CHMOD-MODE
   SB-RESET CL-LONG+ s" ckt-cl-long.f" REQ-WRITE ;

: CL-LOST-CODE$ ( -- ptr u8 n )
   s\" \"code\":\"E-MISSING-SOURCE\",\"repair_class\":\"fix_load_path\"," ;

: CL-REQUIRE-TOKEN$ ( -- ptr u8 n )
   s\" \"token\":\"require\",\"file\":" ;

: CL-LOST-AT$ ( -- ptr u8 n )
   s\" /ckt-cl-lost.f\",\"line\":2,\"column\":1,\"byte_start\":7,\"byte_end\":14," ;

: CL-UNREAD-CODE$ ( -- ptr u8 n )
   s\" \"code\":\"E-UNREADABLE-SOURCE\",\"repair_class\":\"make_source_readable\"," ;

: CL-UNREAD-AT$ ( -- ptr u8 n )
   s\" /ckt-cl-unread.f\",\"line\":2,\"column\":1,\"byte_start\":9,\"byte_end\":16," ;

: CL-DYN-CODE$ ( -- ptr u8 n )
   s\" \"code\":\"E-LOADER-FORM\",\"repair_class\":\"literal_loader_form\"," ;

: CL-DYN-TOKEN$ ( -- ptr u8 n )
   s\" \"token\":\"included\",\"file\":" ;

: CL-DYN-AT$ ( -- ptr u8 n )
   s\" /ckt-cl-dyn.f\",\"line\":2,\"column\":7,\"byte_start\":12,\"byte_end\":20," ;

: CL-LONG-AT$ ( -- ptr u8 n )
   s\" /ckt-cl-long.f\",\"line\":1,\"column\":1,\"byte_start\":0,\"byte_end\":7," ;

: TEST-CLOSURE-STOP ( -- )
   CL-FILES
   s" ckt-cl-lost.f" CL-LOST-CODE$ CL-REQUIRE-TOKEN$ CL-LOST-AT$ ONE-MODES
   s" ckt-cl-unread.f" CL-UNREAD-CODE$ CL-REQUIRE-TOKEN$ CL-UNREAD-AT$ ONE-MODES
   s" ckt-cl-dyn.f" CL-DYN-CODE$ CL-DYN-TOKEN$ CL-DYN-AT$ ONE-MODES
   s" ckt-cl-dyn-req.f" CL-DYN-CODE$ CL-DYN-TOKEN$ CL-DYN-AT$ ONE-MODES
   s" ckt-cl-long.f" CL-DYN-CODE$ CL-REQUIRE-TOKEN$ CL-LONG-AT$ ONE-MODES ;

\ A source given on standard input, or through SOURCE under a label, is walked
\ as a named file is: a require of a file that is not there is the same record,
\ at the loader word, in the file the label names, and so is a string the
\ source never closes, at its opener.
: CL-STDIN$ ( -- ptr u8 n )
   s\" \\ lost\nrequire ckt-cl-none.f\n" ;

: CL-STDIN-AT$ ( -- ptr u8 n )
   s\" \"<stdin>\",\"line\":2,\"column\":1,\"byte_start\":7,\"byte_end\":14," ;

: EXPECT-STDIN-LOST ( n n n -- )
   CL-LOST-CODE$ CL-REQUIRE-TOKEN$ CL-STDIN-AT$ EXPECT-ONE ;

: UT-STDIN$ ( -- ptr u8 n )
   s\" : CKS ( -- ) s\" abc ;\n" ;

: UT-STDIN-AT$ ( -- ptr u8 n )
   s\" \"<stdin>\",\"line\":1,\"column\":14,\"byte_start\":13,\"byte_end\":15," ;

: EXPECT-STDIN-UNTERM ( n n n -- )
   UT-CODE$ UT-SDQ-TOKEN$ UT-STDIN-AT$ EXPECT-ONE ;

: TEST-CLOSURE-STDIN ( -- )
   CL-STDIN$ s" --json-errors" CLI-FLAG-STDIN EXPECT-STDIN-LOST
   CL-STDIN$ DIRECT-JSON-STDIN EXPECT-STDIN-LOST
   CL-STDIN$ ALL-JSON-STDIN EXPECT-STDIN-LOST
   UT-STDIN$ s" --json-errors" CLI-FLAG-STDIN EXPECT-STDIN-UNTERM
   UT-STDIN$ DIRECT-JSON-STDIN EXPECT-STDIN-UNTERM
   UT-STDIN$ ALL-JSON-STDIN EXPECT-STDIN-UNTERM ;

\ A run that passes keeps what the subject wrote: under --json-errors its
\ stderr goes to stdout beside its stdout, and stderr stays empty.
: NOISE$ ( -- ptr u8 n )
   s\" : CKT-NOISE ( -- ) 2 s\" ordinary stderr\" write drop ;\nCKT-NOISE\n" ;

: EXPECT-NOISE-OUT ( n n n -- )
   {: outu:n erru:n rc:n :}
   rc 0 T=
   erru 0 T=
   CAP-OUT outu s" ordinary stderr" CONTAINS? TTRUE ;

: TEST-SUCCESS-STDERR ( -- )
   s" ckt-noise.f" REQ$ NOISE$ WRITE-ALL
   s" ckt-noise.f" ST-JSON-RUN EXPECT-NOISE-OUT
   s" ckt-noise.f" REQ-PLAIN-RUN EXPECT-NOISE-OUT ;

\ A closure wider than a fixed table: the subject requires CL-HUBS files and
\ each of those CL-LEAVES files of its own, so every text fits the string
\ builder; every --json-errors mode checks it.
10 constant CL-HUBS
13 constant CL-LEAVES

: CL-HUB+ ( n -- )
   s" ckt-cl-" SB-APPEND FMT:SB-U ;

: CL-LEAF+ ( n n -- )
   swap CL-HUB+ s" -" SB-APPEND FMT:SB-U ;

: CL-HUB-FILES ( n -- )
   {: h:n :}
   CL-LEAVES 0 ?do
      SB-RESET h i CL-LEAF+ s" .f" SB-APPEND
      SB$ REQ$ s\" \\ one of many\n" WRITE-ALL
   loop
   SB-RESET h CL-HUB+ s" .f" SB-APPEND SB$ REQ$ {: path:ptr pathu:n :}
   SB-RESET
   CL-LEAVES 0 ?do s" require " SB-APPEND h i CL-LEAF+ s" .f" REQ-LINE+ loop
   path pathu SB$ WRITE-ALL ;

: CL-WIDE-FILES ( -- )
   CL-HUBS 0 ?do i CL-HUB-FILES loop
   SB-RESET
   CL-HUBS 0 ?do s" require " SB-APPEND i CL-HUB+ s" .f" REQ-LINE+ loop
   s" ckt-cl-wide.f" REQ-WRITE ;

: TEST-CLOSURE-WIDE ( -- )
   CL-WIDE-FILES
   s" ckt-cl-wide.f" ST-JSON-RUN EXPECT-ACCEPTED
   s" ckt-cl-wide.f" REQ-PLAIN-RUN EXPECT-ACCEPTED
   s" ckt-cl-wide.f" REQ-ALL-RUN EXPECT-ACCEPTED ;

\ A refusal: status 70, nothing on stdout, one line on stderr, whose length
\ this answers.
: REFUSED-LINE ( n n n -- n )
   {: outu:n erru:n rc:n :}
   rc 70 T=
   outu 0 T=
   CAP-ERR erru 10 COUNT-CHAR 1 T=
   erru ;

\ A string or a primitive-axiom row the subject never closes stops the check
\ where the lexer sees it. Read from standard input the subject is refused by
\ the record --all-errors writes for the defect, in prose and in JSON, and
\ checked as a file it is refused by that prose, each at the opener as
\ written. The prose names the code, the file and the place.
: LEXD-FILE$ ( -- ptr u8 n )
   s" ckt-lxd.f" REQ$ ;

: LEXD-REFUSED ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- )
   {: src:ptr srcu:n code:ptr codeu:n at:ptr atu:n place:ptr placeu:n :}
   LEXD-FILE$ src srcu WRITE-ALL
   src srcu DIRECT-STDIN REFUSED-LINE {: e1:n :}
   SB-RESET code codeu SB-APPEND s"  <stdin>" SB-APPEND at atu SB-APPEND
   CAP-ERR e1 SB$ STARTS-WITH? TTRUE
   src srcu DIRECT-JSON-STDIN REFUSED-LINE {: e2:n :}
   SB-RESET s\" \"code\":\"" SB-APPEND code codeu SB-APPEND $22 SB-APPEND-C
   CAP-ERR e2 SB$ CONTAINS? TTRUE
   CAP-ERR e2 place placeu CONTAINS? TTRUE
   LEXD-FILE$ PATH-RUN REFUSED-LINE {: e3:n :}
   CAP-ERR e3 code codeu STARTS-WITH? TTRUE
   SB-RESET s" /ckt-lxd.f" SB-APPEND at atu SB-APPEND
   CAP-ERR e3 SB$ CONTAINS? TTRUE ;

: TEST-LEX-LOCATED ( -- )
   s\" : CKS ( -- ) s\" abc ;" s" E-UNTERMINATED-STRING"
   s\" :1:14: string literal opened at 's\"' does not close"
   s\" \"file\":\"<stdin>\",\"line\":1,\"column\":14," LEXD-REFUSED
   s" PRIM: CKR PE-N PE-IN" s" E-MALFORMED-REGISTRY-ROW"
   s" :1:1: primitive-axiom row opened at 'PRIM:' does not close"
   s\" \"file\":\"<stdin>\",\"line\":1,\"column\":1," LEXD-REFUSED
   s\" \\ lead\n  PRIM:\n" s" E-MALFORMED-REGISTRY-ROW"
   s" :2:3: primitive-axiom row opened at 'PRIM:' does not close"
   s\" \"file\":\"<stdin>\",\"line\":2,\"column\":3," LEXD-REFUSED
   s\" \\ lead\n  PPRIM:\n" s" E-MALFORMED-REGISTRY-ROW"
   s" :2:3: primitive-axiom row opened at 'PPRIM:' does not close"
   s\" \"file\":\"<stdin>\",\"line\":2,\"column\":3," LEXD-REFUSED
   s" PRIM: CKR char" s" E-MALFORMED-REGISTRY-ROW"
   s" :1:1: primitive-axiom row opened at 'PRIM:' does not close"
   s\" \"file\":\"<stdin>\",\"line\":1,\"column\":1," LEXD-REFUSED ;

\ Standard input skips source discovery, so a file it requires is first read
\ by the pre-verifier, and what that file leaves open stops the check there.
\ The stop is reported where it stands in that file, by the record the subject
\ would get: a definer with no name by the nominal pass's line, an open string
\ or primitive-axiom row by the lexer's, in prose, under --all-errors and in
\ JSON.
: NEST-FILE ( ptr u8 n ptr u8 n -- )
   {: f:ptr fu:n txt:ptr txtu:n :}
   f fu REQ$ txt txtu WRITE-ALL ;

: NEST-SRC$ ( ptr u8 n -- ptr u8 n )
   {: f:ptr fu:n :}
   SB-RESET s" require " SB-APPEND f fu REQ$ SB-APPEND SB$ ;

\ The fixture's name after a slash, then what follows the file in the record.
: NEST-AT$ ( ptr u8 n ptr u8 n -- ptr u8 n )
   {: f:ptr fu:n at:ptr atu:n :}
   SB-RESET $2f SB-APPEND-C f fu SB-APPEND at atu SB-APPEND SB$ ;

: NEST-PROSE ( n n n ptr u8 n ptr u8 n ptr u8 n -- )
   {: outu:n erru:n rc:n head:ptr headu:n f:ptr fu:n at:ptr atu:n :}
   outu erru rc REFUSED-LINE drop
   CAP-ERR erru head headu STARTS-WITH? TTRUE
   CAP-ERR erru f fu at atu NEST-AT$ CONTAINS? TTRUE ;

: NEST-REFUSED ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- )
   {: f:ptr fu:n head:ptr headu:n code:ptr codeu:n at:ptr atu:n jat:ptr jatu:n :}
   f fu NEST-SRC$ DIRECT-STDIN head headu f fu at atu NEST-PROSE
   f fu NEST-SRC$ DIRECT-ALL-STDIN head headu f fu at atu NEST-PROSE
   f fu NEST-SRC$ DIRECT-JSON-STDIN REFUSED-LINE {: e:n :}
   CAP-ERR e code codeu CONTAINS? TTRUE
   CAP-ERR e f fu jat jatu NEST-AT$ CONTAINS? TTRUE ;

: TEST-NESTED-LOCATED ( -- )
   s" ckt-nst-create.f" s\" \\ lead\n  create\n" NEST-FILE
   s" ckt-nst-create.f" s" check.f: /" s\" \"code\":\"E-MISSING-NAME\""
   s" :2:3: missing name after 'create'"
   s\" \",\"line\":2,\"column\":3," NEST-REFUSED
   s" ckt-nst-package.f" s\" \\ lead\n  package\n" NEST-FILE
   s" ckt-nst-package.f" s" check.f: /" s\" \"code\":\"E-MISSING-NAME\""
   s" :2:3: missing name after 'package'"
   s\" \",\"line\":2,\"column\":3," NEST-REFUSED
   s" ckt-nst-str.f" s\" : CKS ( -- ) s\" abc ;" NEST-FILE
   s" ckt-nst-str.f" s" E-UNTERMINATED-STRING /"
   s\" \"code\":\"E-UNTERMINATED-STRING\""
   s\" :1:14: string literal opened at 's\"' does not close"
   s\" \",\"line\":1,\"column\":14," NEST-REFUSED
   s" ckt-nst-row.f" s" PRIM: CKR PE-N PE-IN" NEST-FILE
   s" ckt-nst-row.f" s" E-MALFORMED-REGISTRY-ROW /"
   s\" \"code\":\"E-MALFORMED-REGISTRY-ROW\""
   s" :1:1: primitive-axiom row opened at 'PRIM:' does not close"
   s\" \",\"line\":1,\"column\":1," NEST-REFUSED ;

\ --verify-only locates a refused definition by its packet, and a stop by the
\ packet the other modes write for it: the one line on standard error, naming
\ the file the stop is in and the place of the reader or the row opener. The
\ subject is a file, or standard input under --stdin-path, a path that need
\ not exist; a file the subject requires stops it the same way.
: VO-FILE-RUN ( ptr u8 n -- n n n )
   RESET
   s" verify-only" OPT
   FILE
   [: RUN-ACT ;] IN-PROC ;

: VO-STDIN-RUN ( ptr u8 n ptr u8 n -- n n n )
   {: src:ptr srcu:n path:ptr pathu:n :}
   SCRATCH-MAKE
   CHECK-ARGV-START
   s" --verify-only" CHECK-ARG+
   s" --stdin-path" CHECK-ARG+
   path pathu CHECK-ARG+
   SCRATCH-ENV
   HB$ >LEN src srcu >LEN CAP-OUT BUF-CAP >LEN CAP-ERR BUF-CAP >LEN
   CHILD-HANG-MS >MS RUN-ARGV-ENV-STDIN-CAPTURE
   CAPTURE>N
   SCRATCH-EMPTY ;

: VO-LOCATED ( n n n ptr u8 n ptr u8 n ptr u8 n -- )
   {: outu:n erru:n rc:n code:ptr codeu:n f:ptr fu:n at:ptr atu:n :}
   rc 70 T=
   CAP-ERR erru 10 COUNT-CHAR 1 T=
   SB-RESET s\" \"code\":\"" SB-APPEND code codeu SB-APPEND $22 SB-APPEND-C
   CAP-ERR erru SB$ CONTAINS? TTRUE
   CAP-ERR erru f fu at atu NEST-AT$ CONTAINS? TTRUE ;

: TEST-VERIFY-LOCATED ( -- )
   s" ckt-vo.f" s" \ lead" s" create" NONAME-SRC$ NEST-FILE
   s" ckt-vo.f" REQ$ VO-FILE-RUN
   s" E-MISSING-NAME" s" ckt-vo.f" s\" \",\"line\":2,\"column\":3," VO-LOCATED
   s\" : CKC ( -- n )\n  char\n" s" ckt-vo-new.f" REQ$ VO-STDIN-RUN
   s" E-MISSING-NAME" s" ckt-vo-new.f" s\" \",\"line\":2,\"column\":3," VO-LOCATED
   s\" \\ lead\n  PRIM:\n" s" ckt-vo-new.f" REQ$ VO-STDIN-RUN
   s" E-MALFORMED-REGISTRY-ROW" s" ckt-vo-new.f" s\" \",\"line\":2,\"column\":3," VO-LOCATED
   s" PRIM: CKR char" s" ckt-vo-new.f" REQ$ VO-STDIN-RUN
   s" E-MALFORMED-REGISTRY-ROW" s" ckt-vo-new.f" s\" \",\"line\":1,\"column\":1," VO-LOCATED
   s" ckt-vo-dep.f" s\" \\ lead\n  package\n" NEST-FILE
   s" ckt-vo.f" s" ckt-vo-dep.f" NEST-SRC$ NEST-FILE
   s" ckt-vo.f" REQ$ VO-FILE-RUN
   s" E-MISSING-NAME" s" ckt-vo-dep.f" s\" \",\"line\":2,\"column\":3," VO-LOCATED
   s" ckt-vo-row.f" s" PRIM: CKR PE-N PE-IN" NEST-FILE
   s" ckt-vo.f" s" ckt-vo-row.f" NEST-SRC$ NEST-FILE
   s" ckt-vo.f" REQ$ VO-FILE-RUN
   s" E-MALFORMED-REGISTRY-ROW" s" ckt-vo-row.f" s\" \",\"line\":1,\"column\":1," VO-LOCATED ;

\ A statement the source ends inside, or one that lacks a part it must have,
\ stops the check at its opener: a definition or its signature never closed
\ (7155: a FUNCTION: with no symbol among them), a locals group never closed
\ (discovery's E-DISC-UNTERM, on standard input as in a file), a definer's
\ signature missing or never closed (7157: FUNCTION:'s declaration group too), a
\ TRUST with no name and signature strings before it (7158), a declaration
\ never ended (TYPE-DECL:E-TDECL-SYNTAX, 7107), a generates: effect past the
\ engine's bound (7199). A DEFLINEAR or VALUE-RECORD name no type may take
\ (7200) and a VALUE-RECORD field the checker refuses (7198) stop it there;
\ the nominal pass refuses those first in every mode but --verify-only. Each
\ is reported where it stands, by the record of a statement that throws: first
\ in prose, alone under --all-errors, in JSON and under --verify-only, for
\ standard input, a file and a file standard input requires. The nominal pass,
\ which runs before the pre-verifier, refuses an unended ENUM, STRUCTURE,
\ PRODUCT or VALUE-RECORD in every file of the closure, and standard input
\ walks its closure as a named file does: only --verify-only, which has no
\ nominal pass, reaches the pre-verifier's stop at one (SS-DECL-NESTED).
TYPED-VARIABLE SS-TXT-A ptr u8                \ the statement, a source's second line
variable SS-TXT-U
variable SS-CODE                              \ the code it stops with
variable SS-LINE                              \ where
variable SS-COL
TYPED-VARIABLE SS-TOK-A ptr u8                \ and the token there
variable SS-TOK-U

: SS-STOP! ( ptr u8 n n n n ptr u8 n -- )
   {: txt:ptr txtu:n code:n line:n col:n tok:ptr toku:n :}
   txt SS-TXT-A !  txtu SS-TXT-U !
   code SS-CODE !  line SS-LINE !  col SS-COL !
   tok SS-TOK-A !  toku SS-TOK-U ! ;

: SS-TOK$ ( -- ptr u8 n )
   SS-TOK-A @ SS-TOK-U @ ;

\ The source, built again for each run, since the check uses the builder too.
: SS-SRC$ ( -- ptr u8 n )
   s" \ lead" SS-TXT-A @ SS-TXT-U @ NONAME-SRC$ ;

: SS-FILE$ ( -- ptr u8 n )
   s" ckt-ss.f" ;

: SS-AT$ ( -- ptr u8 n )
   s" /ckt-ss.f" ;

\ The prose record starts the report and names the place in the file whose
\ name ends with the given text, the code and the token there.
: SS-PROSE ( n ptr u8 n -- )
   {: erru:n f:ptr fu:n :}
   CAP-ERR erru s" E-STATEMENT-THROW " STARTS-WITH? TTRUE
   SB-RESET f fu SB-APPEND
   $3a SB-APPEND-C SS-LINE @ FMT:SB-U $3a SB-APPEND-C SS-COL @ FMT:SB-U
   s" : throw " SB-APPEND SS-CODE @ FMT:SB-INT
   s"  at '" SB-APPEND SS-TOK$ SB-APPEND 39 SB-APPEND-C
   CAP-ERR erru SB$ CONTAINS? TTRUE ;

: SS-JSON ( n ptr u8 n -- )
   {: erru:n f:ptr fu:n :}
   CAP-ERR erru s\" \"code\":\"E-STATEMENT-THROW\"" CONTAINS? TTRUE
   SB-RESET s\" \"token\":\"" SB-APPEND SS-TOK$ SB-APPEND s\" \",\"file\":\"" SB-APPEND
   CAP-ERR erru SB$ CONTAINS? TTRUE
   SB-RESET f fu SB-APPEND s\" \",\"line\":" SB-APPEND SS-LINE @ FMT:SB-U
   s\" ,\"column\":" SB-APPEND SS-COL @ FMT:SB-U $2c SB-APPEND-C
   CAP-ERR erru SB$ CONTAINS? TTRUE
   SB-RESET s\" \"throw_code\":" SB-APPEND SS-CODE @ FMT:SB-INT $2c SB-APPEND-C
   CAP-ERR erru SB$ CONTAINS? TTRUE ;

: SS-PROSE-RUN ( n n n ptr u8 n -- )
   {: outu:n erru:n rc:n f:ptr fu:n :}
   rc 70 T=
   outu 0 T=
   erru f fu SS-PROSE ;

: SS-STDIN-JSON ( -- )
   SS-SRC$ DIRECT-JSON-STDIN REFUSED-LINE STDIN-LABEL$ SS-JSON ;

: SS-STDIN ( -- )
   SS-SRC$ DIRECT-STDIN STDIN-LABEL$ SS-PROSE-RUN
   SS-SRC$ DIRECT-ALL-STDIN REFUSED-LINE STDIN-LABEL$ SS-PROSE
   SS-STDIN-JSON ;

: SS-FILE ( -- )
   SS-FILE$ SS-SRC$ NEST-FILE
   SS-FILE$ REQ$ PATH-RUN SS-AT$ SS-PROSE-RUN ;

: SS-NESTED ( -- )
   SS-FILE$ SS-SRC$ NEST-FILE
   SS-FILE$ NEST-SRC$ DIRECT-STDIN SS-AT$ SS-PROSE-RUN
   SS-FILE$ NEST-SRC$ DIRECT-ALL-STDIN REFUSED-LINE SS-AT$ SS-PROSE
   SS-FILE$ NEST-SRC$ DIRECT-JSON-STDIN REFUSED-LINE SS-AT$ SS-JSON ;

\ --verify-only writes the record on standard error and its prose on standard
\ output.
: SS-VERIFIED ( n n n ptr u8 n -- )
   {: outu:n erru:n rc:n f:ptr fu:n :}
   rc 70 T=
   CAP-ERR erru 10 COUNT-CHAR 1 T=
   erru f fu SS-JSON ;

: SS-VERIFY ( -- )
   SS-FILE$ SS-SRC$ NEST-FILE
   SS-FILE$ REQ$ VO-FILE-RUN SS-AT$ SS-VERIFIED ;

: SS-EVERY ( -- )
   SS-STDIN SS-FILE SS-NESTED SS-VERIFY ;

\ An unended declaration in a file standard input requires. Under
\ --verify-only, the language server's check, the pre-verifier stops at it,
\ located in that file. In the other modes the nominal pass refuses it first,
\ in standard input's closure as in a named file's, so standard input's report
\ in each mode is the one for the same bytes in a named file.
create SS-OUT BUF-CAP allot                   \ the named file's report
variable SS-OUT-U
create SS-ERR BUF-CAP allot
variable SS-ERR-U
variable SS-RC
create SS-NEST FS-PATH-CAP 8 + allot          \ `require ` and the file's path
variable SS-NEST-U

: SS-KEEP ( n n n -- )
   {: outu:n erru:n rc:n :}
   rc 70 T=
   CAP-OUT SS-OUT outu BYTE-COPY  outu SS-OUT-U !
   CAP-ERR SS-ERR erru BYTE-COPY  erru SS-ERR-U !
   rc SS-RC ! ;

: SS-SAME ( n n n -- )
   {: outu:n erru:n rc:n :}
   rc SS-RC @ T=
   SS-OUT SS-OUT-U @ CAP-OUT outu T$=
   SS-ERR SS-ERR-U @ CAP-ERR erru T$= ;

: SS-REQ$ ( -- ptr u8 n )
   s" ckt-ss-req.f" ;

\ Standard input's source for --verify-only, kept apart from the string
\ builder, which the scratch directory the run makes reuses.
: SS-NEST$ ( -- ptr u8 n )
   SS-FILE$ NEST-SRC$ {: a:ptr u:n :}
   a SS-NEST u BYTE-COPY  u SS-NEST-U !
   SS-NEST SS-NEST-U @ ;

: SS-DECL-NESTED ( -- )
   SS-FILE$ SS-SRC$ NEST-FILE
   SS-REQ$ SS-FILE$ NEST-SRC$ NEST-FILE
   SS-REQ$ REQ$ PATH-RUN SS-KEEP
   SS-FILE$ NEST-SRC$ DIRECT-STDIN SS-SAME
   SS-REQ$ ALL-PROSE-RUN SS-KEEP
   SS-FILE$ NEST-SRC$ DIRECT-ALL-STDIN SS-SAME
   SS-REQ$ ST-JSON-RUN SS-KEEP
   SS-FILE$ NEST-SRC$ DIRECT-JSON-STDIN SS-SAME
   SS-REQ$ REQ-PLAIN-RUN SS-KEEP
   SS-FILE$ NEST-SRC$ ALL-JSON-STDIN SS-SAME
   SS-NEST$ s" ckt-ss-new.f" REQ$ VO-STDIN-RUN SS-AT$ SS-VERIFIED ;

\ The line and the column of the given byte of BIG.
: BIG-PLACE ( n -- n n )
   {: at:n :}
   1 1 at 0 ?do
      BIG i + c@ $0a = if drop 1+ 1 else 1+ then
   loop ;

\ BIG stops with the given code at the token that starts at the given byte.
: BIG-STOP! ( n n -- )
   {: code:n at:n :}
   at begin dup BIG-U @ < if BIG over + c@ 32 > else false then while 1+ repeat
   {: end:n :}
   BIG 0 code at BIG-PLACE BIG at + end at - SS-STOP! ;

: BIG-N ( n -- )
   SB-RESET FMT:SB-U SB$ BIG-APP ;

: SS-BIG-JSON ( -- )
   BIG BIG-U @ DIRECT-JSON-STDIN REFUSED-LINE STDIN-LABEL$ SS-JSON ;

\ The pre-verifier's tables grow with what a source holds, so each source below,
\ one past a bound a table once had, verifies under --verify-only, read as a
\ file whose loads find their file beside it. Only the verdict is asserted: the
\ run is the program's own, and lib/type/deftype.f refuses CAP-NAME's name
\ there (E-VNOM-CAP: it mangles at most 32 bytes).
: SS-BIG-VERIFIES ( -- )
   s" ckt-x.f" s" \ ckt-x.f - the file CAP-LOADS' definitions load" NEST-FILE
   s" ckt-big.f" BIG BIG-U @ NEST-FILE
   s" ckt-big.f" REQ$ VO-FILE-RUN EXPECT-ACCEPTED ;

\ A DEFTYPE name longer than the pre-verifier once folded.
: CAP-NAME ( -- )
   0 BIG-U !
   s" require lib/type/deftype.f" BIG-APP $0a BIG-C,
   s" DEFTYPE " BIG-APP
   65 0 ?do $4e BIG-C, loop $0a BIG-C, ;

\ A clause signature longer than the slot the definer table once gave an
\ effect: the longest effect a `generates:` row states (checker.f GENR-SIG-CAP).
: CAP-CLAUSE ( -- )
   0 BIG-U !
   s" : CKO ( n -- ) create , does> (" BIG-APP
   GENR-SIG-CAP 0 ?do 32 BIG-C, loop
   s" -- n ) @ ;" BIG-APP $0a BIG-C, ;

\ One does> definer more than the table once held.
: CAP-DEFINERS ( -- )
   0 BIG-U !
   130 1 ?do
      s" : CKD" BIG-APP i BIG-N
      s"  ( n -- ) create , does> ( -- n ) @ ;" BIG-APP $0a BIG-C,
   loop ;

\ One load more than the pre-verifier once held while an open package keeps the
\ loads its definitions make waiting.
: CAP-LOADS ( -- )
   0 BIG-U !
   s" package CKP" BIG-APP $0a BIG-C,
   18 1 ?do
      s" : CKL" BIG-APP i BIG-N
      s\"  ( -- ) s\" ckt-x.f\" required ;" BIG-APP $0a BIG-C,
   loop
   s" ;package" BIG-APP $0a BIG-C, ;

\ A generates: effect longer than the engine's row holds (checker.f
\ GENR-SIG-CAP) stops the pre-verifier at the row's opener (7199).
: GEN-LONG ( -- )
   0 BIG-U !
   s" : CKG ( n -- ) create , ;" BIG-APP $0a BIG-C,
   BIG-U @ {: at:n :}
   s" generates: CKG (" BIG-APP
   GENR-SIG-CAP 0 ?do 32 BIG-C, loop
   s" -- n )" BIG-APP $0a BIG-C,
   7199 at BIG-STOP! ;

: TEST-STATEMENT-STOP-LOCATED ( -- )
   s" : CKD ( -- n ) 1" 7155 2 3 s" :" SS-STOP! SS-EVERY
   s" defer CKF" 7157 2 3 s" defer" SS-STOP! SS-EVERY
   s" TRUST" 7158 2 3 s" TRUST" SS-STOP! SS-EVERY
   s" BEGIN-STRUCTURE CKB 8 +FIELD CKB-X" 7107 2 3 s" BEGIN-STRUCTURE" SS-STOP! SS-EVERY
   s" : CKG ( n -- n" 7155 2 3 s" :" SS-STOP! SS-STDIN-JSON
   \ A locals group never closed stops at its `{:`, on standard input as in a
   \ file (TEST-DISCOVERY-LOCATED).
   s" : CKL ( n -- n ) {: a" E-DISC-UNTERM 2 20 s" {:" SS-STOP! SS-STDIN-JSON
   s" : CKO ( n -- ) create , does> ( -- n ) @" 7155 2 3 s" :" SS-STOP! SS-STDIN-JSON
   s" TRUSTED: CKT ( -- ) 1 drop" 7155 2 3 s" TRUSTED:" SS-STOP! SS-STDIN-JSON
   s" defer CKF foo" 7157 2 3 s" defer" SS-STOP! SS-STDIN-JSON
   s" defer CKF ( n -- n" 7157 2 3 s" defer" SS-STOP! SS-STDIN-JSON
   s" : CKO ( n -- ) create , does> @ ;" 7157 2 3 s" :" SS-STOP! SS-STDIN-JSON
   s\" s\" CKX\" TRUST" 7158 2 11 s" TRUST" SS-STOP! SS-STDIN-JSON
   \ The nominal pass refuses an unended declaration first, in standard
   \ input's closure as in a file's: the pre-verifier's stop at it is located
   \ under --verify-only, and standard input reports in the other modes what
   \ the same bytes as a named file do (SS-DECL-NESTED).
   s" ENUM ckcolor red" 7107 2 3 s" ENUM" SS-STOP! SS-DECL-NESTED SS-VERIFY
   s" STRUCTURE ckpoint 0 FIELD x n" 7107 2 3 s" STRUCTURE" SS-STOP! SS-DECL-NESTED SS-VERIFY
   s" PRODUCT ckpair 0 FIELD x n" 7107 2 3 s" PRODUCT" SS-STOP! SS-DECL-NESTED SS-VERIFY
   s" VALUE-RECORD ckvr x n" 7107 2 3 s" VALUE-RECORD" SS-STOP! SS-DECL-NESTED SS-VERIFY
   s\" require lib/ffi-abi.f\nFUNCTION: CKF" 7155 3 1 s" FUNCTION:" SS-STOP! SS-EVERY
   s\" require lib/ffi-abi.f\nFUNCTION: CKF getpid" 7157 3 1 s" FUNCTION:" SS-STOP! SS-EVERY
   s\" require lib/ffi-abi.f\nFUNCTION: CKF getpid foo" 7157 3 1 s" FUNCTION:" SS-STOP! SS-STDIN-JSON
   s\" require lib/ffi-abi.f\nFUNCTION: CKF getpid ( n -- n" 7157 3 1 s" FUNCTION:" SS-STOP! SS-STDIN-JSON
   s" DEFLINEAR n" 7200 2 13 s" n" SS-STOP! SS-VERIFY
   s" VALUE-RECORD n x n END-VALUE-RECORD" 7200 2 16 s" n" SS-STOP! SS-VERIFY
   s" VALUE-RECORD ckvr x bogus END-VALUE-RECORD" 7198 2 21 s" x" SS-STOP! SS-VERIFY
   s" VALUE-RECORD ckvr value read-view<p,q,u8> END-VALUE-RECORD" 7198 2 21 s" value" SS-STOP! SS-VERIFY
   s" VALUE-RECORD ckvr END-VALUE-RECORD" 7198 2 21 s" END-VALUE-RECORD" SS-STOP! SS-VERIFY
   CAP-NAME SS-BIG-VERIFIES
   CAP-CLAUSE SS-BIG-VERIFIES
   CAP-DEFINERS SS-BIG-VERIFIES
   CAP-LOADS SS-BIG-VERIFIES
   GEN-LONG SS-BIG-JSON ;

\ Discovery walks every file of a closure before the check, and a string or a
\ locals group a file never closes ends the walk at its opener. A string is
\ reported there by the lexer's record, and a group, which the lexer does not
\ read, by the statement-throw record of discovery's code: under --verify-only
\ for a file, for standard input under --stdin-path and for a file the subject
\ requires, and in every mode of a file check.
: TEST-DISCOVERY-LOCATED ( -- )
   s" ckt-vod.f" s\" : CKS ( -- ) s\" abc ;" NEST-FILE
   s" ckt-vod.f" REQ$ VO-FILE-RUN
   s" E-UNTERMINATED-STRING" s" ckt-vod.f" s\" \",\"line\":1,\"column\":14," VO-LOCATED
   s\" : CKS ( -- ) s\" abc ;" s" ckt-vod-new.f" REQ$ VO-STDIN-RUN
   s" E-UNTERMINATED-STRING" s" ckt-vod-new.f" s\" \",\"line\":1,\"column\":14," VO-LOCATED
   s" ckt-vod-dep.f" s\" : CKS ( -- ) s\" abc ;" NEST-FILE
   s" ckt-vod.f" s" ckt-vod-dep.f" NEST-SRC$ NEST-FILE
   s" ckt-vod.f" REQ$ VO-FILE-RUN
   s" E-UNTERMINATED-STRING" s" ckt-vod-dep.f" s\" \",\"line\":1,\"column\":14," VO-LOCATED
   s" : CKL ( n -- n ) {: a" E-DISC-UNTERM 2 20 s" {:" SS-STOP!
   SS-FILE$ SS-SRC$ NEST-FILE
   SS-FILE$ REQ$ PATH-RUN REFUSED-LINE SS-AT$ SS-PROSE
   SS-FILE$ ST-JSON-RUN REFUSED-LINE SS-AT$ SS-JSON
   SS-FILE$ ALL-PROSE-RUN REFUSED-LINE SS-AT$ SS-PROSE
   SS-FILE$ REQ-PLAIN-RUN REFUSED-LINE SS-AT$ SS-JSON
   SS-VERIFY
   s\" \\ lead\n  : CKL ( n -- n ) {: a\n" s" ckt-vod-new.f" REQ$ VO-STDIN-RUN
   s" /ckt-vod-new.f" SS-VERIFIED ;

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

\ Under --json-errors the engine's refusal, prose, is on stdout.
: EXPECT-USING-AT-JSON ( n n n ptr u8 n -- )
   {: outu:n erru:n rc:n at:ptr atu:n :}
   rc ENGINE-ERROR:USING-UNKNOWN T=
   erru 0 T=
   CAP-OUT outu at atu USING-AT$ CONTAINS? TTRUE ;

: TEST-USING-AT-SOURCE ( -- )
   SB-RESET USING-AT-LINES s" using-at.f" REQ-WRITE
   s" using-at.f" REQ-RUN s" using-at.f" REQ$ EXPECT-USING-AT
   s" using-at.f" ST-JSON-RUN s" using-at.f" REQ$ EXPECT-USING-AT-JSON
   RESET
   SB-RESET USING-AT-LINES SB$ s" ckt-using-at.f" SOURCE
   [: RUN-ACT ;] IN-PROC s" ckt-using-at.f" EXPECT-USING-AT ;

\ A colon in a `using` or `package` name is refused by the check, at the
\ statement the load refuses: the replay throws there as the live statement
\ does inside evaluate. `using` throws the engine's own code
\ (ENGINE-ERROR:USING-BAD-NAME, 90); `package` throws the checker's
\ E-PKG-CONTEXT (7136), since the engine's code for it is private to
\ packages.f. A qualified name through such a `using` is never reached: the
\ `using` before it is refused, not the name (E-BAD-QUALIFIED).
: COLON-REFUSED ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: f:ptr fu:n at:ptr atu:n code:ptr codeu:n :}
   f fu REQ-WRITE
   f fu ST-JSON-RUN {: outu:n erru:n rc:n :}
   f fu T-LABEL rc 70 T=
   outu 0 T=
   CAP-ERR erru s\" \"code\":\"E-STATEMENT-THROW\"" CONTAINS? TTRUE
   CAP-ERR erru code codeu CONTAINS? TTRUE
   CAP-ERR erru at atu CONTAINS? TTRUE
   CAP-ERR erru s" E-BAD-QUALIFIED" CONTAINS? TFALSE ;

: TEST-NAME-COLON ( -- )
   SB-RESET
   s" : CKT-NC-ONE ( -- n ) 1 ;" REQ-LINE+
   s" using CKT-NC-A:B" REQ-LINE+
   s" ;using" REQ-LINE+
   s" nc-using.f" s\" nc-using.f\",\"line\":2,\"column\":7,"
   s\" \"throw_code\":90," COLON-REFUSED
   SB-RESET
   s" : CKT-NC-ONE ( -- n ) 1 ;" REQ-LINE+
   s" package CKT-NC-A:B" REQ-LINE+
   s" ;package" REQ-LINE+
   s" nc-package.f" s\" nc-package.f\",\"line\":2,\"column\":9,"
   s\" \"throw_code\":7136," COLON-REFUSED
   SB-RESET
   s" using CKT-NC-A:B" REQ-LINE+
   s" : CKT-NC-TWO ( -- n ) CKT-NC-A:B:CKT-NC-X ;" REQ-LINE+
   s" ;using" REQ-LINE+
   s" nc-qualified.f" s\" nc-qualified.f\",\"line\":1,\"column\":7,"
   s\" \"throw_code\":90," COLON-REFUSED ;

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
   CAP-ERR erru s\" \"code\":\"E-UNDEFINED-TOP-LEVEL\",\"repair_class\":\"unknown_rejection\",\"verdict\":\"rejected\",\"token\":\"#!/usr/bin/env\"" CONTAINS? TTRUE ;

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
   s" sb-using.f" ST-JSON-RUN s" sb-using.f" REQ$ EXPECT-USING-AT-JSON
   SB-RESET SHEBANG-USING-LINES SB$ CLI-STDIN STDIN-LABEL$ EXPECT-USING-AT
   s" sb-one.f" s" sb-using.f" SHEBANG-LIST-RUN s" sb-using.f" REQ$ EXPECT-USING-AT ;

: TEST-SHEBANG-LATER ( -- )
   SHEBANG-FILES
   s" sb-later.f" REQ-RUN EXPECT-SHEBANG-LATER
   s" sb-one.f" s" sb-later.f" SHEBANG-LIST-RUN EXPECT-SHEBANG-LATER ;

\ A checker capacity fault is no storage refusal: a DYNAMIC-BUFFER whose derived
\ names overrun the checker's name buffer (src/core/checker.f LBUF-NM-CAP)
\ throws E-CHECKER-LAYOUT-BUFFER (7121) out of its statement, at the token the
\ checker read last: the type, after the 250-byte name on the definer's line.
: CAP-THROW-FILES ( -- )
   SB-RESET s" DYNAMIC-BUFFER CKT-CT-" SB-APPEND
   243 0 ?do $4e SB-APPEND-C loop
   s"  n" REQ-LINE+
   s" cap-throw.f" REQ-WRITE ;

: TEST-CAPACITY-THROW ( -- )
   CAP-THROW-FILES
   s" cap-throw.f" REQ-PLAIN-RUN 70 T= {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru s\" \"code\":\"E-STATEMENT-THROW\"" CONTAINS? TTRUE
   CAP-ERR erru s\" \"token\":\"n\"" CONTAINS? TTRUE
   CAP-ERR erru s\" cap-throw.f\",\"line\":1,\"column\":267," CONTAINS? TTRUE
   CAP-ERR erru s\" \"throw_code\":7121" CONTAINS? TTRUE ;

\ ---- what the checker cannot store is refused by name ------------------------
\ Each of these ended the process with die 76 (review 389): a declared name
\ whose derived constructor spelling overran src/core/type-family.f TF-CTOR-BUF,
\ a package name past the checker's package row (src/core/checker.f
\ CHECKER-PACKAGE-CAP) and a definition whose effect was deeper than the effect
\ store's walks go (src/core/checker.f EFFECT-DEPTH-MAX) or took more cells
\ than its record holds (EFFECT-MIN-IN-MAX). Each is now the
\ located refusal of its declaration, statement or definition, rc 70, and the
\ longest that fits is still taken. The sources are built in LONG-BUF.

\ count bytes of c into the long source.
: CAPN-REP ( n n -- )
   {: c:n count:n :}
   count 0 ?do c LONG-C loop ;

\ head, count bytes of c, then tail: a declaration around one long name.
: CAPN-DECL+ ( ptr u8 n n n ptr u8 n -- )
   {: h:ptr hu:n c:n count:n t:ptr tu:n :}
   h hu LONG-PUT
   c count CAPN-REP
   t tu LONG-PUT ;

\ The long source as the fixture named f, checked with --json-errors.
: CAPN-RUN ( ptr u8 n -- n n n )
   {: f:ptr fu:n :}
   f fu REQ$ LONG-BUF LONG-U @ WRITE-ALL
   f fu ST-JSON-RUN ;

\ The declaration in fixture f is refused at its long name: the packet names
\ the declaration's kind, carries the name as its token and the limit in its
\ reason, and the run fails as for a refusal.
: CAPN-DECL-REFUSED ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: f:ptr fu:n kind:ptr kindu:n at:ptr atu:n :}
   f fu CAPN-RUN {: outu:n erru:n rc:n :}
   f fu T-LABEL rc 70 T=
   outu 0 T=
   CAP-ERR erru s\" \"code\":\"E-BAD-DECLARATION\"" CONTAINS? TTRUE
   CAP-ERR erru kind kindu CONTAINS? TTRUE
   CAP-ERR erru at atu CONTAINS? TTRUE
   CAP-ERR erru s\" \"reason\":\"name longer than 255 bytes\"" CONTAINS? TTRUE ;

\ A family, variant or field name is at most 255 bytes in every declaration
\ front end. The SUMTYPE tail and the STRUCTURE field are review 389's 1100-byte
\ fixtures; a 255-byte tail is still declared.
: TEST-DECLARED-NAME-CAP ( -- )
   0 LONG-U !
   s" SUMTYPE " $61 1100 s"  1 VARIANT value a ;VARIANT ;SUMTYPE" CAPN-DECL+
   s" capn-sum-tail.f" s\" \"decl\":\"sumtype\""
   s\" \"token\":\"aaaaaaaaaaaaaaaa" CAPN-DECL-REFUSED
   0 LONG-U !
   s\" SUMTYPE capn-sv 0\nVARIANT " $76 256 s\"  ;VARIANT ;SUMTYPE" CAPN-DECL+
   s" capn-sum-variant.f" s\" \"decl\":\"sumtype\""
   s\" \"token\":\"vvvvvvvvvvvvvvvv" CAPN-DECL-REFUSED
   0 LONG-U !
   s" PRODUCT capn-pr 0 FIELD " $66 256 s"  n ;PRODUCT" CAPN-DECL+
   s" capn-product-field.f" s\" \"decl\":\"product\""
   s\" \"token\":\"ffffffffffffffff" CAPN-DECL-REFUSED
   0 LONG-U !
   s" STRUCTURE " $73 256 s"  0 FIELD x n ;STRUCTURE" CAPN-DECL+
   s" capn-struct-tail.f" s\" \"decl\":\"structure\""
   s\" \"token\":\"ssssssssssssssss" CAPN-DECL-REFUSED
   0 LONG-U !
   s\" require lib/c2-memory.f\npackage CAPN-FLD\npublic\nSTRUCTURE capn-rec 0 DERIVE init FIELD "
   $65 1100 s\"  n FIELD right n ;STRUCTURE\n;package" CAPN-DECL+
   s" capn-struct-field.f" s\" \"decl\":\"structure\""
   s\" \"token\":\"eeeeeeeeeeeeeeee" CAPN-DECL-REFUSED
   0 LONG-U !
   s" ENUM " $6d 256 s"  0 VARIANT red ;VARIANT ;ENUM" CAPN-DECL+
   s" capn-enum-tail.f" s\" \"decl\":\"enum\""
   s\" \"token\":\"mmmmmmmmmmmmmmmm" CAPN-DECL-REFUSED
   0 LONG-U !
   s" ENUM capn-col 0 VARIANT " $76 256 s"  ;VARIANT ;ENUM" CAPN-DECL+
   s" capn-enum-variant.f" s\" \"decl\":\"enum\""
   s\" \"token\":\"vvvvvvvvvvvvvvvv" CAPN-DECL-REFUSED
   0 LONG-U !
   s" ENUM capn-tone " $74 256 s"  ;ENUM" CAPN-DECL+
   s" capn-enum-compact.f" s\" \"decl\":\"enum\""
   s\" \"token\":\"tttttttttttttttt" CAPN-DECL-REFUSED
   0 LONG-U !
   s" ENUM capn-shape 0 VARIANT dot FIELD " $64 256 s"  n ;VARIANT ;ENUM" CAPN-DECL+
   s" capn-enum-field.f" s\" \"decl\":\"enum\""
   s\" \"token\":\"dddddddddddddddd" CAPN-DECL-REFUSED
   0 LONG-U !
   s" SUMTYPE " $62 255 s\"  0 VARIANT capn-one ;VARIANT ;SUMTYPE\n" CAPN-DECL+
   s" SUMTYPE " $63 256 s"  0 VARIANT capn-two ;VARIANT ;SUMTYPE" CAPN-DECL+
   s" capn-sum-edge.f" CAPN-RUN {: outu:n erru:n rc:n :}
   s" capn-sum-edge.f" T-LABEL rc 70 T=
   outu 0 T=
   CAP-ERR erru s\" \"family\":\"ccc" CONTAINS? TTRUE
   CAP-ERR erru s\" \"family\":\"bbb" CONTAINS? TFALSE ;

\ A package name is at most 255 bytes: `package` and `using` refuse a longer
\ one, a statement throw at the name, as `package` does review 389's 1100-byte
\ tf3.f and a name longer than any the engine defines (CK-NAME-MAX), which
\ ended the process as no name at all. The 255-byte package on the lines before
\ it is opened and closed.
: CAPN-PKG+ ( n n -- )
   {: c:n count:n :}
   s" package " LONG-PUT
   c count CAPN-REP
   s\" \n;package\n" LONG-PUT ;

: CAPN-PKG-REFUSED ( ptr u8 n ptr u8 n -- )
   {: f:ptr fu:n at:ptr atu:n :}
   f fu CAPN-RUN {: outu:n erru:n rc:n :}
   f fu T-LABEL rc 70 T=
   outu 0 T=
   CAP-ERR erru s\" \"code\":\"E-STATEMENT-THROW\"" CONTAINS? TTRUE
   CAP-ERR erru s\" \"throw_code\":7154" CONTAINS? TTRUE
   CAP-ERR erru at atu CONTAINS? TTRUE ;

: TEST-PACKAGE-NAME-CAP ( -- )
   0 LONG-U !
   s" package " $61 1100 s\" \nNEWTYPE tt 0\n;package\n" CAPN-DECL+
   s" capn-pkg.f" s\" capn-pkg.f\",\"line\":1,\"column\":9," CAPN-PKG-REFUSED
   0 LONG-U !
   $61 8001 CAPN-PKG+
   s" capn-pkg-long.f" s\" capn-pkg-long.f\",\"line\":1,\"column\":9," CAPN-PKG-REFUSED
   0 LONG-U !
   $50 255 CAPN-PKG+
   $51 256 CAPN-PKG+
   s" capn-pkg-edge.f" s\" capn-pkg-edge.f\",\"line\":3,\"column\":9," CAPN-PKG-REFUSED
   0 LONG-U !
   s" using " $55 256 s\" \n;using\n" CAPN-DECL+
   s" capn-using.f" s\" capn-using.f\",\"line\":1,\"column\":7," CAPN-PKG-REFUSED
   0 LONG-U !
   s" using " $55 8001 s\" \n;using\n" CAPN-DECL+
   s" capn-using-long.f" s\" capn-using-long.f\",\"line\":1,\"column\":7," CAPN-PKG-REFUSED ;

\ A qualifier longer than any package name names no package, so the type it
\ qualifies is unknown.
: TEST-QUALIFIER-CAP ( -- )
   0 LONG-U !
   s" : CKT-QL ( " $71 300 s\" :tail -- ) drop ;\n" CAPN-DECL+
   s" capn-qual.f" CAPN-RUN {: outu:n erru:n rc:n :}
   rc 70 T=
   outu 0 T=
   CAP-ERR erru s\" \"code\":\"E-UNKNOWN-SIGNATURE-TYPE\"" CONTAINS? TTRUE
   CAP-ERR erru s\" capn-qual.f\",\"line\":1,\"column\":12," CONTAINS? TTRUE ;

\ `: name 1 1 ... ;`, count literals, on a line of its own.
: CAPN-ONES+ ( ptr u8 n n -- )
   {: a:ptr u:n count:n :}
   s" : " LONG-PUT
   a u LONG-PUT
   count 0 ?do s"  1" LONG-PUT loop
   s\"  ;\n" LONG-PUT ;

\ CKT-EA leaves 2048 cells and CKT-EB 4096, the deepest effect recorded.
: CAPN-ROWS+ ( -- )
   s" CKT-EA" 2048 CAPN-ONES+
   s\" : CKT-EB CKT-EA CKT-EA ;\n" LONG-PUT ;

\ Fixture f's definition is uncheckable: the packet names the word and the
\ depth against the ceiling, at the definition's last token.
: CAPN-DEEP-REFUSED ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- )
   {: f:ptr fu:n w:ptr wu:n why:ptr whyu:n at:ptr atu:n :}
   f fu CAPN-RUN {: outu:n erru:n rc:n :}
   f fu T-LABEL rc 70 T=
   outu 0 T=
   CAP-ERR erru s\" \"code\":\"E-UNCHECKABLE\"" CONTAINS? TTRUE
   CAP-ERR erru w wu CONTAINS? TTRUE
   CAP-ERR erru why whyu CONTAINS? TTRUE
   CAP-ERR erru at atu CONTAINS? TTRUE ;

\ A recorded effect is at most 4096 levels deep: a row's entries, and below
\ an entry the levels its type nests. A definition leaving 4096 cells is
\ recorded and a caller instantiates it, while one leaving 4097, or returning a
\ quotation that leaves 8192, is uncheckable. Review 389's bigb5.f left 10000;
\ a record of 8191 was taken and then faulted the data stack of every checked
\ caller. The quotation's refusal renders the top 64 cells of its row:
\ rendering the whole row faulted the data stack (exit 102).
: TEST-EFFECT-DEPTH-CAP ( -- )
   0 LONG-U !
   CAPN-ROWS+
   s\" : CKT-EE CKT-EB ;\n: CKT-EC CKT-EB 1 ;\n" LONG-PUT
   s" capn-row.f" s\" \"word\":\"ckt-ec\""
   s\" \"reason\":\"effect too deep to record (depth 4097, at most 4096)\""
   s\" capn-row.f\",\"line\":4,\"column\":17," CAPN-DEEP-REFUSED
   0 LONG-U !
   CAPN-ROWS+
   s\" : CKT-EQ [: CKT-EB CKT-EB ;] ;\n" LONG-PUT
   s" capn-quot.f" s\" \"word\":\"ckt-eq\""
   s\" \"reason\":\"effect too deep to record (depth 8194, at most 4096)\""
   s\" capn-quot.f\",\"line\":3,\"column\":27," CAPN-DEEP-REFUSED ;

\ `: name ( n ... n -- )`, count cells in.
: CAPN-WIDE+ ( ptr u8 n n -- )
   {: a:ptr u:n count:n :}
   s" : " LONG-PUT  a u LONG-PUT  s"  (" LONG-PUT
   count 0 ?do s"  n" LONG-PUT loop
   s"  -- )" LONG-PUT ;

\ CKT-W254 drops 254 cells, and CKT-W255 and its caller CKT-WE take 255, the
\ widest input row recorded.
: CAPN-WIDTHS+ ( -- )
   s" CKT-W254" 254 CAPN-WIDE+
   127 0 ?do s"  2drop" LONG-PUT loop
   s\"  ;\n" LONG-PUT
   s" CKT-W255" 255 CAPN-WIDE+  s\"  drop CKT-W254 ;\n" LONG-PUT
   s" CKT-WE" 255 CAPN-WIDE+  s\"  CKT-W255 ;\n" LONG-PUT ;

: CAPN-W256$ ( -- ptr u8 n )
   s\" \"reason\":\"input row too wide to record (256 cells, at most 255)\"" ;

\ CKT-WR names CKT-SEVEN, which only the run defines: it cannot be deferred,
\ so it is uncheckable by its width, at that token, and CKT-SEVEN is no
\ undefined word.
: CAPN-UNDEFERRED ( -- )
   s" capn-width-defer.f" CAPN-RUN {: outu:n erru:n rc:n :}
   rc 70 T=
   outu 0 T=
   CAP-ERR erru s\" \"code\":\"E-UNCHECKABLE\"" CONTAINS? TTRUE
   CAP-ERR erru s\" \"verdict\":\"uncheckable\"" CONTAINS? TTRUE
   CAP-ERR erru s\" \"word\":\"ckt-wr\"" CONTAINS? TTRUE
   CAP-ERR erru CAPN-W256$ CONTAINS? TTRUE
   CAP-ERR erru s\" capn-width-defer.f\",\"line\":6,\"column\":529," CONTAINS? TTRUE
   CAP-ERR erru s" E-UNDEFINED" CONTAINS? TFALSE ;

\ Under --all-errors CKT-WJ is refused for leaving its 256 inputs, and the
\ check goes on past it to certify CKT-WK.
: CAPN-WIDE-REJECTED ( -- )
   s" capn-width-reject.f" REQ$ LONG-BUF LONG-U @ WRITE-ALL
   s" capn-width-reject.f" REQ-PLAIN-RUN {: outu:n erru:n rc:n :}
   rc 70 T=
   CAP-ERR erru s\" \"word\":\"ckt-wj\"" CONTAINS? TTRUE
   CAP-ERR erru s\" \"word\":\"ckt-wk\"" CONTAINS? TFALSE ;

\ A recorded input row is at most 255 cells: the minimum a call must provide,
\ in eight bits of the record. A definition taking more, inferred or declared,
\ returning or not, is uncheckable by that width, and so is a RECURSE in a body
\ declared that wide. A body the run would judge cannot be deferred, since
\ deferring records the declaration. A body refused for its own reason keeps
\ it, and under --all-errors keeps no record. Each ended the process with die
\ 76.
: TEST-INPUT-WIDTH-CAP ( -- )
   0 LONG-U !
   CAPN-WIDTHS+
   s\" : CKT-W508 CKT-W254 CKT-W254 ;\n" LONG-PUT
   s" capn-width.f" s\" \"word\":\"ckt-w508\""
   s\" \"reason\":\"input row too wide to record (508 cells, at most 255)\""
   s\" capn-width.f\",\"line\":4,\"column\":21," CAPN-DEEP-REFUSED
   0 LONG-U !
   CAPN-WIDTHS+
   s" CKT-WD" 256 CAPN-WIDE+  s\"  2drop CKT-W254 ;\n" LONG-PUT
   s" capn-width-decl.f" s\" \"word\":\"ckt-wd\"" CAPN-W256$
   s\" capn-width-decl.f\",\"line\":4,\"column\":535," CAPN-DEEP-REFUSED
   0 LONG-U !
   s\" : CKT-BOOM ( -- ) s\" boom\" 1 die ;\n" LONG-PUT
   s" CKT-WN" 256 CAPN-WIDE+  s\"  CKT-BOOM ;\n" LONG-PUT
   s" capn-width-dead.f" s\" \"word\":\"ckt-wn\"" CAPN-W256$
   s\" capn-width-dead.f\",\"line\":2,\"column\":529," CAPN-DEEP-REFUSED
   0 LONG-U !
   \ No declaration too deep to record reaches a RECURSE: each level costs at
   \ least a two-byte token and a definition's text holds 8000 bytes, so 4097
   \ outputs are refused at the text and 3985, the most that fit, certify.
   s" CKT-WQ" 256 CAPN-WIDE+  s\"  RECURSE ;\n" LONG-PUT
   s" capn-width-recurse.f" s\" \"word\":\"ckt-wq\"" CAPN-W256$
   s\" capn-width-recurse.f\",\"line\":1,\"column\":529," CAPN-DEEP-REFUSED
   0 LONG-U !
   s\" : CKT-MAKE ( -- ) s\" : CKT-SEVEN ( -- n ) 7 ;\" INCLUDE-EVALUATE ;\nCKT-MAKE\n"
   LONG-PUT
   CAPN-WIDTHS+
   s" CKT-WR" 256 CAPN-WIDE+  s\"  CKT-SEVEN drop 2drop CKT-W254 ;\n" LONG-PUT
   CAPN-UNDEFERRED
   0 LONG-U !
   s" CKT-WJ" 256 CAPN-WIDE+  s\"  ;\n: CKT-WK ( -- n ) 1 ;\n" LONG-PUT
   CAPN-WIDE-REJECTED ;

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
   CAP-ERR erru s\" \"word\":\"CKT-RQ-DU-X\"," CONTAINS? TTRUE
   CAP-ERR erru s\" req-dup-dep.f\",\"line\":1,\"column\":3," CONTAINS? TTRUE
   CAP-ERR erru $0a COUNT-CHAR 1 T= ;

: TEST-REQUIRE-DUPLICATE ( -- )
   REQ-DUP-FILES
   s" req-dup.f" ST-JSON-RUN EXPECT-ONE-DUPLICATE
   s" req-dup.f" REQ-PLAIN-RUN EXPECT-ONE-DUPLICATE
   s" req-dup.f" REQ-ALL-RUN EXPECT-ONE-DUPLICATE ;

\ In prose the duplicate's line names the word and the file and line that
\ defined it again, as the JSON record does and as --load does, in the default
\ mode and under --all-errors.
: REQ-DUP-PROSE$ ( -- ptr u8 n )
   s" req-dup-dep.f" REQ$ {: p:ptr pu:n :}
   SB-RESET
   s" duplicate definition: CKT-RQ-DU-X at " SB-APPEND
   p pu SB-APPEND
   s" :1" SB-APPEND
   SB$ ;

: EXPECT-DUP-PROSE ( n n n -- )
   CHECK-ALL-ERRORS:DUP-RC T= {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru REQ-DUP-PROSE$ CONTAINS? TTRUE ;

: TEST-REQUIRE-DUPLICATE-PROSE ( -- )
   REQ-DUP-FILES
   s" req-dup.f" REQ-RUN EXPECT-DUP-PROSE
   s" req-dup.f" ALL-PROSE-RUN EXPECT-DUP-PROSE ;

\ The files of a set, one per target, define the same word, and a body loads
\ the one its target predicate answers for, as tools/object-image.f loads
\ sys.f. The engine answers every HB-TARGET-*? one way, so the arm it runs
\ loads whenever the body does and the other arm never: its file left to the
\ run leaves the word undefined, and the other arm's file checked too reads
\ the word as a duplicate. req-alt.f picks as tools/object-image.f does, an
\ arm ending in `exit`; req-alt2.f reads each target `if` to its `then`;
\ req-alt3.f nests a target `if` in the `else` arm of another; and req-alt5.f
\ ends every arm in `EXIT`, which the engine reads as `exit`, so the loader
\ after the arms never runs. A loader under any other condition is the run's:
\ the one in CKT-RQ-COND never runs, and its file defines the word again. So
\ are those of req-alt4.f, which defines the word itself: each of its `if`s
\ tests a local pushed after a target predicate, not the predicate.
: REQ-PICK+ ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: guard:ptr guardu:n file:ptr fileu:n tail:ptr tailu:n :}
   s"    " SB-APPEND guard guardu SB-APPEND s"  if " SB-APPEND
   file fileu REQ-LIT+ tail tailu REQ-LINE+ ;

: REQ-ALT-USE ( ptr u8 n -- )   \ the use, and the built text becomes the fixture
   s" : CKT-RQ-ALT-USE ( -- n ) CKT-RQ-ALT ;" REQ-LINE+
   REQ-WRITE ;

: REQ-TARGET-FILES ( -- )
   SB-RESET s" : CKT-RQ-ALT ( -- n ) 1 ;" REQ-LINE+
   s" req-alt-linux.f" REQ-WRITE
   s" req-alt-macos.f" REQ-WRITE
   s" req-alt-x64.f" REQ-WRITE
   s" req-alt-cond.f" REQ-WRITE
   SB-RESET s" : CKT-RQ-PICK ( -- )" REQ-LINE+
   s" HB-TARGET-LINUX?" s" req-alt-linux.f" s"  required exit then" REQ-PICK+
   s" HB-TARGET-MACOS?" s" req-alt-macos.f" s"  required exit then" REQ-PICK+
   s" HB-TARGET-LINUX-X86-64?" s" req-alt-x64.f" s"  required then ;" REQ-PICK+
   s" CKT-RQ-PICK" REQ-LINE+
   s" : CKT-RQ-COND ( -- )" REQ-LINE+
   s" 0 0= 0=" s" req-alt-cond.f" s"  required then ;" REQ-PICK+
   s" CKT-RQ-COND" REQ-LINE+
   s" req-alt.f" REQ-ALT-USE
   SB-RESET s" : CKT-RQ-PICK2 ( -- )" REQ-LINE+
   s" HB-TARGET-LINUX?" s" req-alt-linux.f" s"  required then" REQ-PICK+
   s" HB-TARGET-MACOS?" s" req-alt-macos.f" s"  required then" REQ-PICK+
   s" HB-TARGET-LINUX-X86-64?" s" req-alt-x64.f" s"  required then ;" REQ-PICK+
   s" CKT-RQ-PICK2" REQ-LINE+
   s" req-alt2.f" REQ-ALT-USE
   SB-RESET s" : CKT-RQ-PICK3 ( -- )" REQ-LINE+
   s" HB-TARGET-LINUX-X86-64?" s" req-alt-x64.f" s"  required" REQ-PICK+
   s"    else HB-TARGET-MACOS? if " SB-APPEND
   s" req-alt-macos.f" REQ-LIT+ s"  required" REQ-LINE+
   s"    else " SB-APPEND s" req-alt-linux.f" REQ-LIT+ s"  required then then ;" REQ-LINE+
   s" CKT-RQ-PICK3" REQ-LINE+
   s" req-alt3.f" REQ-ALT-USE
   SB-RESET s" : CKT-RQ-ALT ( -- n ) 1 ;" REQ-LINE+
   s" : CKT-RQ-LOCAL ( bool -- )" REQ-LINE+
   s"    {: off:bool :}" REQ-LINE+
   s" HB-TARGET-LINUX? off" s" req-alt-cond.f" s"  required then drop" REQ-PICK+
   s" HB-TARGET-MACOS? off" s" req-alt-cond.f" s"  required then drop ;" REQ-PICK+
   s" 0 0= 0= CKT-RQ-LOCAL" REQ-LINE+
   s" req-alt4.f" REQ-ALT-USE
   SB-RESET s" : CKT-RQ-PICK4 ( -- )" REQ-LINE+
   s" HB-TARGET-LINUX?" s" req-alt-linux.f" s"  required EXIT then" REQ-PICK+
   s" HB-TARGET-MACOS?" s" req-alt-macos.f" s"  required EXIT then" REQ-PICK+
   s" HB-TARGET-LINUX-X86-64?" s" req-alt-x64.f" s"  required EXIT then" REQ-PICK+
   s"    " SB-APPEND s" req-alt-cond.f" REQ-LIT+ s"  required ;" REQ-LINE+
   s" CKT-RQ-PICK4" REQ-LINE+
   s" req-alt5.f" REQ-ALT-USE ;

: TEST-REQUIRE-TARGET ( -- )
   REQ-TARGET-FILES
   s" req-alt.f" REQ-ACCEPTED-EVERY
   s" req-alt2.f" REQ-ACCEPTED-EVERY
   s" req-alt3.f" REQ-ACCEPTED-EVERY
   s" req-alt4.f" REQ-ACCEPTED-EVERY
   s" req-alt5.f" REQ-ACCEPTED-EVERY ;

\ The pre-pass reads a target predicate's spelling as the engine's answer, so a
\ source that defines that spelling, by `:` or by `defer`, is refused at the
\ name before an arm is skipped on the engine's word, in every mode: a source
\ list lints each file it names. Each subject defines both host predicates, so
\ the refusal does not depend on the host.
: REQ-SHADOW-FILES ( -- )
   SB-RESET s" : CKT-RQ-SH ( -- n ) 3 ;" REQ-LINE+
   s" req-shadow-dep.f" REQ-WRITE
   SB-RESET s" package CKT-RQ-SHADOW" REQ-LINE+
   s" : HB-TARGET-LINUX? ( -- bool ) 0 0= ;" REQ-LINE+
   s" : HB-TARGET-MACOS? ( -- bool ) 0 0= ;" REQ-LINE+
   s" public" REQ-LINE+
   s" : CKT-RQ-SH-LOAD ( -- )" REQ-LINE+
   s" HB-TARGET-LINUX?" s" req-shadow-dep.f" s"  required exit then" REQ-PICK+
   s" HB-TARGET-MACOS?" s" req-shadow-dep.f" s"  required then ;" REQ-PICK+
   s" ;package" REQ-LINE+
   s" CKT-RQ-SHADOW:CKT-RQ-SH-LOAD" REQ-LINE+
   s" : CKT-RQ-SH-USE ( -- n ) CKT-RQ-SH ;" REQ-LINE+
   s" req-shadow.f" REQ-WRITE
   SB-RESET s" package CKT-RQ-SHADOW-D" REQ-LINE+
   s" defer HB-TARGET-LINUX? ( -- bool )" REQ-LINE+
   s" defer HB-TARGET-MACOS? ( -- bool )" REQ-LINE+
   s" ;package" REQ-LINE+
   s" req-shadow-defer.f" REQ-WRITE ;

: EXPECT-RESERVED-AT ( n n n ptr u8 n -- ) {: outu:n erru:n rc:n at:ptr atu:n :}
   rc 1 T=
   outu 0 T=
   CAP-ERR erru s" E-RESERVED-DEFINITION" CONTAINS? TTRUE
   CAP-ERR erru at atu CONTAINS? TTRUE ;

\ The default run reports the place in prose, the all-errors runs as JSON.
: SHADOW-CASE ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: f:ptr fu:n at:ptr atu:n jat:ptr jatu:n :}
   f fu REQ-RUN at atu EXPECT-RESERVED-AT
   f fu REQ-PLAIN-RUN jat jatu EXPECT-RESERVED-AT
   f fu REQ-ALL-RUN jat jatu EXPECT-RESERVED-AT ;

: TEST-TARGET-SHADOW ( -- )
   REQ-SHADOW-FILES
   s" req-shadow.f" s" req-shadow.f:2:3"
   s\" req-shadow.f\",\"line\":2,\"column\":3," SHADOW-CASE
   s" req-shadow-defer.f" s" req-shadow-defer.f:2:7"
   s\" req-shadow-defer.f\",\"line\":2,\"column\":7," SHADOW-CASE ;

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

\ `--all-errors --source-list a b` runs all-errors on the ORIGINAL files, in
\ order, in one session: both bad defs in b report against b's
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

\ The fixture is refused by the pre-pass as undefined at the token, here and on
\ the command line, with the same report.
: AGREE-UNDEFINED ( ptr u8 n ptr u8 n -- ) {: file:ptr fileu:n tok:ptr toku:n :}
   file fileu REQ-RUN {: outu:n erru:n rc:n :}
   CAP-ERR AGREE-ERR erru BYTE-COPY  erru AGREE-ERR-U !
   outu erru rc tok toku EXPECT-PREVERIFY-UNDEFINED
   file fileu REQ$ CLI-PATH {: cliu:n clie:n clirc:n :}
   rc clirc T=
   outu cliu T=
   AGREE-ERR AGREE-ERR-U @ CAP-ERR clie T$= ;

: TEST-REPEAT-DEFINER ( -- )
   AGREE-DEFINER
   s" agree.f" REQ-RUN EXPECT-ACCEPTED
   AGREE-PLAIN
   s" agree.f" s" CKT-AG-NOPE" AGREE-UNDEFINED ;

\ The pre-pass sees the program the run loads: the engine and what the subject
\ loads. A word only the checking process loaded - lib/test.f's T= in this
\ harness, lib/fs.f's FILE-SIZE here and in check.f alike - is undefined to it
\ in both, a subject that requires lib/fs.f itself has FILE-SIZE, and one that
\ includes lib/process.f declares its `outcome` family afresh, as the load
\ does, though this process declared it when it loaded the file.
: TEST-PREPASS-VIEW ( -- )
   SB-RESET s" : CKT-PV-ASSERT ( n n -- ) T= ;" REQ-LINE+
   s" view-harness.f" REQ-WRITE
   s" view-harness.f" s" T=" AGREE-UNDEFINED
   SB-RESET s" : CKT-PV-SIZE ( ptr u8 n -- n ) FILE-SIZE ;" REQ-LINE+
   s" view-tool.f" REQ-WRITE
   s" view-tool.f" s" FILE-SIZE" AGREE-UNDEFINED
   SB-RESET s" require lib/fs.f" REQ-LINE+
   s" : CKT-PV-SIZE ( ptr u8 n -- n ) FILE-SIZE ;" REQ-LINE+
   s" view-own.f" REQ-WRITE
   s" view-own.f" REQ-RUN EXPECT-ACCEPTED
   s" view-own.f" REQ$ CLI-PATH EXPECT-ACCEPTED
   SB-RESET s" include lib/process.f" REQ-LINE+
   s" view-family.f" REQ-WRITE
   s" view-family.f" REQ-RUN EXPECT-ACCEPTED ;

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
\ it lies below it. LC-ROOT is such a working directory: the tree is linked in
\ beside `sub/`, which holds the files checked there, and its bin/hb is no
\ engine but a script that exits 97.
\
\ LC-OTHER is another tree: the same lib and src, an engine, and a
\ tools/check-verify-child.f of its own that dies with 98. A require resolves
\ against the requiring file's root before the working directory, so
\ LC-ROOT's lc-check.f loads check.f from LC-ROOT's tree with LC-OTHER current,
\ as a server loads its checker from the engine's tree with the editor's
\ directory current.

create LC-PATH FS-PATH-CAP allot
create LC-ROOT FS-PATH-CAP allot
create LC-OTHER FS-PATH-CAP allot
variable LC-PATH-U
variable LC-ROOT-U
variable LC-OTHER-U

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

: LC-OTHER$ ( -- ptr u8 n )
   LC-OTHER LC-OTHER-U @ ;

: LC-IN-OTHER ( ptr u8 n -- ptr u8 n )
   LC-OTHER$ 2swap LC-PATH JOIN-PATH LC-PATH-U !
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

\ A structure whose field has the type a required file declares, the require
\ absolute so that it resolves wherever the bytes are checked.
: LC-TYPE-DEP$SRC ( -- ptr u8 n )
   s\" NEWTYPE cklctyval 0\n" ;

: LC-TYPE-USE$SRC ( -- ptr u8 n )
   SB-RESET
   s" require " SB-APPEND
   s" lc-type-dep.f" LC-AT SB-APPEND
   $0a SB-APPEND-C
   s\" STRUCTURE cklctypair 0 FIELD value cklctyval ;STRUCTURE\n" SB-APPEND
   SB$ ;

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
   s" lc-type-dep.f" LC-TYPE-DEP$SRC LC-WRITE
   s" lc-type-use.f" LC-TYPE-USE$SRC LC-WRITE
   LC-QUOTE$ LC-UNTERM$SRC LC-WRITE
   LC-BACK$ LC-UNTERM$SRC LC-WRITE
   LC-QUOTE$ LC-QLINK$ LC-AT MAKE-SYMLINK
   s" lc-bad-dep.f" LC-BLINK$ LC-AT MAKE-SYMLINK ;

\ Links ROOT/NAME to this tree's NAME.
: LC-LINK ( ptr u8 n ptr u8 n -- )
   {: name:ptr nameu:n root:ptr rootu:n :}
   name nameu CLI-TARGET CLI-TARGET-U CLI-ABS!
   root rootu name nameu LC-PATH JOIN-PATH LC-PATH-U !
   CLI-TARGET CLI-TARGET-U @ LC-PATH$ MAKE-SYMLINK ;

\ No file in this root requires an engine-provided file, and none may: through
\ the links, a subject that requires one (`lib/string.f`) is refused with an
\ E-BAD-DECLARATION "duplicate family" whose file is `<input>`, a fault of
\ checking in a linked root that predates the load-context replay.
: LC-ROOT-SETUP ( -- )
   ROOT$ s" lc-root" LC-ROOT JOIN-PATH LC-ROOT-U !
   LC-ROOT$ MAKE-DIR
   s" lib" LC-ROOT$ LC-LINK
   s" tools" LC-ROOT$ LC-LINK
   s" src" LC-ROOT$ LC-LINK
   s" bin" LC-IN-ROOT MAKE-DIR
   s" bin/hb" LC-IN-ROOT s\" #!/bin/sh\nexit 97\n" WRITE-ALL
   s" bin/hb" LC-IN-ROOT CHMOD-X
   s" sub" LC-IN-ROOT MAKE-DIR
   s" sub/lc-dep.f" LC-IN-ROOT LC-DEP$SRC WRITE-ALL
   s" sub/lc-use.f" LC-IN-ROOT LC-USE$SRC WRITE-ALL
   s" lc-check.f" LC-IN-ROOT s\" require tools/check.f\n" WRITE-ALL ;

: LC-OTHER-SETUP ( -- )
   ROOT$ s" lc-other" LC-OTHER JOIN-PATH LC-OTHER-U !
   LC-OTHER$ MAKE-DIR
   s" lib" LC-OTHER$ LC-LINK
   s" src" LC-OTHER$ LC-LINK
   s" bin" LC-IN-OTHER MAKE-DIR
   CLI-HB$ s" bin/hb" LC-IN-OTHER MAKE-SYMLINK
   s" tools" LC-IN-OTHER MAKE-DIR
   s" tools/check-verify-child.f" LC-IN-OTHER
   s\" s\" check-verify-child: stub\" 98 die\n" WRITE-ALL ;

\ PROGRAM runs in the given directory with the environment the caller set,
\ completed from this process's.
: LC-RUN-CAPTURE ( ptr u8 n ptr u8 n -- n n n )
   {: prog:ptr progu:n cwd:ptr cwdu:n :}
   PROC-ENV-INHERIT-MISSING
   prog progu >LEN cwd cwdu >LEN
   CAP-OUT BUF-CAP >LEN CAP-ERR BUF-CAP >LEN CHILD-HANG-MS >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-CAPTURE
   CAPTURE>N ;

\ The engine runs in the given directory with the environment the caller set.
: LC-ENV-CAPTURE ( ptr u8 n -- n n n )
   CLI-HB$ 2swap LC-RUN-CAPTURE ;

\ The child runs with this process's environment in the given directory.
: LC-CAPTURE ( ptr u8 n -- n n n )
   PROC-ENV-RESET
   LC-ENV-CAPTURE ;

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

\ A file below the working directory, named or listed, is an entry: its bare
\ require resolves against its own directory, as under `bin/hb --load`. A plain
\ list loads it in the verifier child and --all-errors in check.f's own
\ process, so both list modes are run.
: LC-ENTRY-ROOT-CASE ( -- )
   LC-ARGV-ALL
   s" sub/lc-use.f" CHECK-ARG+
   LC-ROOT$ LC-CAPTURE s" load-context: entry below the working directory" LC-EXPECT-CLEAN
   CHECK-ARGV-START
   s" --source-list" CHECK-ARG+
   s" sub/lc-use.f" CHECK-ARG+
   LC-ROOT$ LC-CAPTURE s" load-context: listed entry below the working directory" LC-EXPECT-CLEAN
   LC-ARGV-ALL
   s" --source-list" CHECK-ARG+
   s" sub/lc-use.f" CHECK-ARG+
   LC-ROOT$ LC-CAPTURE s" load-context: --all-errors's listed entry below the working directory" LC-EXPECT-CLEAN ;

\ The checker spawns the engine lib/engine-candidate.f names, here the one
\ running this test, never the working directory's bin/hb: in LC-ROOT that
\ would end the run stage, and --verify-only's verifier child, with 97.
: LC-ENGINE-CASE ( -- )
   CHECK-ARGV-START
   s" sub/lc-use.f" CHECK-ARG+
   LC-ROOT$ LC-CAPTURE s" load-context: the run stage spawns the running engine" LC-EXPECT-CLEAN
   CHECK-ARGV-START
   s" --verify-only" CHECK-ARG+
   s" sub/lc-use.f" CHECK-ARG+
   LC-ROOT$ LC-CAPTURE s" load-context: --verify-only spawns the running engine" LC-EXPECT-CLEAN ;

\ What the engine writes on stderr for a throw nothing caught.
: LC-UNCAUGHT$ ( n -- ptr u8 n ) {: code:n :}
   SB-RESET
   s" hb: uncaught throw code " SB-APPEND
   code FMT:SB-INT
   SB$ ;

\ An engine the resolver refuses, a HABU_UNDER_TEST that names no executable,
\ is the check's error: its E-FS-OPEN reaches the command line uncaught.
: LC-REFUSED-ENGINE ( ptr u8 n -- ) {: label:ptr labelu:n :}
   PROC-ENV-RESET
   s" HABU_UNDER_TEST" >LEN s" sub/lc-dep.f" LC-IN-ROOT >LEN PROC-ENV-SET
   LC-ROOT$ LC-ENV-CAPTURE
   label labelu T-LABEL 67 T=
   {: outu:n erru:n :}
   label labelu T-LABEL CAP-ERR erru E-FS-OPEN LC-UNCAUGHT$ CONTAINS? TTRUE ;

\ Under --json-errors stderr holds packets only: the command line says on
\ stdout that the selection is no usable executable, whatever the resolver
\ refused it for, with the status its refusal has uncaught.
: LC-REFUSED-ENGINE-JSON ( ptr u8 n ptr u8 n -- )
   {: engine:ptr engineu:n label:ptr labelu:n :}
   PROC-ENV-RESET
   s" HABU_UNDER_TEST" >LEN engine engineu >LEN PROC-ENV-SET
   LC-ROOT$ LC-ENV-CAPTURE {: outu:n erru:n rc:n :}
   label labelu T-LABEL rc 67 T=
   label labelu T-LABEL erru 0 T=
   label labelu T-LABEL CAP-OUT outu s" is not a usable executable" CONTAINS? TTRUE ;

: LC-NOT-ENGINE$ ( -- ptr u8 n )
   s" sub/lc-dep.f" LC-IN-ROOT ;

: LC-REFUSED-ENGINE-CASE ( -- )
   CHECK-ARGV-START
   s" sub/lc-use.f" CHECK-ARG+
   s" load-context: the run stage's engine refused" LC-REFUSED-ENGINE
   CHECK-ARGV-START
   s" --verify-only" CHECK-ARG+
   s" sub/lc-use.f" CHECK-ARG+
   s" load-context: --verify-only's engine refused" LC-REFUSED-ENGINE
   CHECK-ARGV-START
   s" --json-errors" CHECK-ARG+
   s" sub/lc-use.f" CHECK-ARG+
   LC-NOT-ENGINE$ s" load-context: --json-errors's engine refused" LC-REFUSED-ENGINE-JSON
   LC-ARGV-ALL
   s" sub/lc-use.f" CHECK-ARG+
   LC-NOT-ENGINE$ s" load-context: --all-errors's engine refused" LC-REFUSED-ENGINE-JSON
   LC-ARGV-ALL
   s" --verify-only" CHECK-ARG+
   s" sub/lc-use.f" CHECK-ARG+
   LC-NOT-ENGINE$ s" load-context: --json-errors --verify-only's engine refused" LC-REFUSED-ENGINE-JSON ;

FS-PATH-CAP 1+ constant LC-LONG-U
create LC-LONG-ENGINE LC-LONG-U allot

\ A selection one byte longer than a path the file system takes, which it
\ refuses (E-FS-PATH) before anything looks for an executable there.
: LC-LONG-ENGINE$ ( -- ptr u8 n )
   $2f LC-LONG-ENGINE c!
   LC-LONG-U 1 ?do $61 LC-LONG-ENGINE i + c! loop
   LC-LONG-ENGINE LC-LONG-U ;

: LC-LONG-ENGINE-CASE ( -- )
   CHECK-ARGV-START
   s" --json-errors" CHECK-ARG+
   s" sub/lc-use.f" CHECK-ARG+
   LC-LONG-ENGINE$ s" load-context: --json-errors's overlong engine" LC-REFUSED-ENGINE-JSON
   LC-ARGV-ALL
   s" sub/lc-use.f" CHECK-ARG+
   LC-LONG-ENGINE$ s" load-context: --all-errors's overlong engine" LC-REFUSED-ENGINE-JSON
   LC-ARGV-ALL
   s" --verify-only" CHECK-ARG+
   s" sub/lc-use.f" CHECK-ARG+
   LC-LONG-ENGINE$ s" load-context: --json-errors --verify-only's overlong engine" LC-REFUSED-ENGINE-JSON ;

\ A subject the file system will not read ends the check before any child, so
\ no engine is selected: every --json-errors mode says so on stdout, by the
\ subject's canonical path, with the status the closure walk gives such a file,
\ and a selection that would fail with the same code is not what it reports.
\ An empty ENGINE leaves this process's selection in force.
: LC-UNREAD-EXPECT ( ptr u8 n ptr u8 n -- )
   {: engine:ptr engineu:n label:ptr labelu:n :}
   PROC-ENV-RESET
   engineu 0<> if s" HABU_UNDER_TEST" >LEN engine engineu >LEN PROC-ENV-SET then
   LC-ROOT$ LC-ENV-CAPTURE {: outu:n erru:n rc:n :}
   label labelu T-LABEL rc 74 T=
   label labelu T-LABEL erru 0 T=
   label labelu T-LABEL CAP-OUT outu s" check.f: cannot read /" CONTAINS? TTRUE
   label labelu T-LABEL CAP-OUT outu s" /sub/lc-locked.f" CONTAINS? TTRUE
   label labelu T-LABEL CAP-OUT outu s" usable executable" CONTAINS? TFALSE ;

: LC-LOCKED-SETUP ( -- )
   s" sub/lc-locked.f" LC-IN-ROOT s\" : LC-LOCKED ( -- n ) 8 ;\n" WRITE-ALL
   s" sub/lc-locked.f" LC-IN-ROOT 0 CHMOD-MODE ;

: LC-UNREAD-CASE ( -- )
   LC-LOCKED-SETUP
   CHECK-ARGV-START
   s" --json-errors" CHECK-ARG+
   s" sub/lc-locked.f" CHECK-ARG+
   NULL$ s" load-context: --json-errors's unreadable subject" LC-UNREAD-EXPECT
   LC-ARGV-ALL
   s" sub/lc-locked.f" CHECK-ARG+
   NULL$ s" load-context: --all-errors's unreadable subject" LC-UNREAD-EXPECT
   LC-ARGV-ALL
   s" --verify-only" CHECK-ARG+
   s" sub/lc-locked.f" CHECK-ARG+
   NULL$ s" load-context: --json-errors --verify-only's unreadable subject" LC-UNREAD-EXPECT ;

\ HABU_UNDER_TEST naming no file fails the resolver with E-FS-OPEN, the code
\ reading the unreadable subject fails with.
: LC-NO-ENGINE$ ( -- ptr u8 n )
   s" no-engine" LC-IN-ROOT ;

: LC-UNREAD-ENGINE-CASE ( -- )
   CHECK-ARGV-START
   s" --json-errors" CHECK-ARG+
   s" sub/lc-locked.f" CHECK-ARG+
   LC-NO-ENGINE$ s" load-context: --json-errors's unreadable subject, absent engine" LC-UNREAD-EXPECT
   LC-ARGV-ALL
   s" sub/lc-locked.f" CHECK-ARG+
   LC-NO-ENGINE$ s" load-context: --all-errors's unreadable subject, absent engine" LC-UNREAD-EXPECT
   LC-ARGV-ALL
   s" --verify-only" CHECK-ARG+
   s" sub/lc-locked.f" CHECK-ARG+
   LC-NO-ENGINE$ s" load-context: --json-errors --verify-only's unreadable subject, absent engine" LC-UNREAD-EXPECT ;

\ A source list walks each listed file in turn. The first here reads the file
\ it requires; the second is the file the file system will not read, so the
\ check names it as it names an unreadable subject, and stderr holds no packet
\ placing it at the first file's require.
: LC-UNREAD-LIST-CASE ( -- )
   CHECK-ARGV-START
   s" --json-errors" CHECK-ARG+
   s" --source-list" CHECK-ARG+
   s" sub/lc-use.f" CHECK-ARG+
   s" sub/lc-locked.f" CHECK-ARG+
   NULL$ s" load-context: --json-errors's unreadable second listed file" LC-UNREAD-EXPECT
   LC-ARGV-ALL
   s" --source-list" CHECK-ARG+
   s" sub/lc-use.f" CHECK-ARG+
   s" sub/lc-locked.f" CHECK-ARG+
   NULL$ s" load-context: --all-errors's unreadable second listed file" LC-UNREAD-EXPECT ;

\ What check.f says on stdout under --json-errors for a throw nothing caught.
: LC-CHECK-UNCAUGHT$ ( n -- ptr u8 n )
   {: code:n :}
   SB-RESET
   s" check.f: uncaught throw code " SB-APPEND
   code FMT:SB-INT
   SB$ ;

\ A check whose own scratch fails ends with that throw uncaught, no fault of
\ the source's or of the engine's: under --json-errors the command line says
\ it on stdout by its code, with the status hb gives such a throw, and stderr
\ stays empty.
: LC-SCRATCH-EXPECT ( n n n n ptr u8 n -- )
   {: outu:n erru:n rc:n code:n label:ptr labelu:n :}
   label labelu T-LABEL rc 67 T=
   label labelu T-LABEL erru 0 T=
   label labelu T-LABEL CAP-OUT outu code LC-CHECK-UNCAUGHT$ CONTAINS? TTRUE
   label labelu T-LABEL CAP-OUT outu s" usable executable" CONTAINS? TFALSE ;

$140 constant LC-MODE-0500   \ a directory its owner may list and enter, not write

: LC-RO-TMP$ ( -- ptr u8 n )
   s" ro-tmp" LC-IN-ROOT ;

\ An HB_TMP check.f may not write in: its scratch directory cannot be made
\ there (E-FS-IO).
: LC-RO-TMP-RUN ( ptr u8 n -- )
   {: label:ptr labelu:n :}
   PROC-ENV-RESET
   s" HB_TMP" >LEN LC-RO-TMP$ >LEN PROC-ENV-SET
   LC-ROOT$ LC-ENV-CAPTURE E-FS-IO label labelu LC-SCRATCH-EXPECT ;

: LC-RO-TMP-CASE ( -- )
   LC-RO-TMP$ MAKE-DIR
   LC-RO-TMP$ LC-MODE-0500 CHMOD-MODE
   CHECK-ARGV-START
   s" --json-errors" CHECK-ARG+
   s" sub/lc-use.f" CHECK-ARG+
   s" load-context: --json-errors's read-only HB_TMP" LC-RO-TMP-RUN
   LC-ARGV-ALL
   s" sub/lc-use.f" CHECK-ARG+
   s" load-context: --all-errors's read-only HB_TMP" LC-RO-TMP-RUN ;

\ /bin/sh sets the file mode mask to 0222, then becomes the engine ($0) with
\ the arguments CHECK-ARGV-START begins.
: LC-UMASK-ARGV-START ( -- )
   PROC-ARGV-RESET
   s" -c" CHECK-ARG+
   s\" umask 0222 && exec \"$0\" \"$@\"" CHECK-ARG+
   CLI-HB$ CHECK-ARG+
   s" --load" CHECK-ARG+
   s" tools/check.f" CHECK-ARG+
   s" --" CHECK-ARG+ ;

\ Under that mask check.f makes its scratch directory without owner write, so
\ writing the source into it fails with E-FS-OPEN, the code the resolver
\ refuses HABU_UNDER_TEST naming no file with. The check ends there, before
\ any child, so no selection is judged and the scratch failure is what it
\ reports.
: LC-UMASK-RUN ( ptr u8 n -- )
   {: label:ptr labelu:n :}
   PROC-ENV-RESET
   s" HABU_UNDER_TEST" >LEN LC-NO-ENGINE$ >LEN PROC-ENV-SET
   s" /bin/sh" LC-ROOT$ LC-RUN-CAPTURE E-FS-OPEN label labelu LC-SCRATCH-EXPECT ;

: LC-UMASK-CASE ( -- )
   LC-UMASK-ARGV-START
   s" --json-errors" CHECK-ARG+
   s" sub/lc-use.f" CHECK-ARG+
   s" load-context: --json-errors's unwritable scratch, absent engine" LC-UMASK-RUN
   LC-UMASK-ARGV-START
   s" --json-errors" CHECK-ARG+
   s" --all-errors" CHECK-ARG+
   s" sub/lc-use.f" CHECK-ARG+
   s" load-context: --all-errors's unwritable scratch, absent engine" LC-UMASK-RUN ;

\ The verifier child is the one in the tree check.f was loaded from, never the
\ working directory's: LC-OTHER's would end the check with 98.
: LC-OTHER-TREE-CASE ( -- )
   PROC-ARGV-RESET
   s" --load" CHECK-ARG+
   s" lc-check.f" LC-IN-ROOT CHECK-ARG+
   s" --" CHECK-ARG+
   s" --verify-only" CHECK-ARG+
   s" sub/lc-use.f" LC-IN-ROOT CHECK-ARG+
   LC-OTHER$ LC-CAPTURE s" load-context: the verifier child of check.f's own tree" LC-EXPECT-CLEAN ;

\ A tree suite that runs on the product alone: a parent that provides the
\ whitebox engine for a window child exits 67 here (E-BUILD-PATH uncaught).
: LC-TREE-CASE ( -- )
   s" test/aot-seeded-address-sites.f" LC-ALL
   s" load-context: test/aot-seeded-address-sites.f" T-LABEL 0 T=
   {: outu:n erru:n :}
   erru 0 T=
   CAP-OUT outu s" aot-seeded-address-sites: ok" CONTAINS? TTRUE ;

\ Nothing checks a file the engine provides, so it is refused at its own
\ spelling, by an E-ENGINE-PROVIDED record, with the usage status.
: LC-PROVIDED-CASE ( -- )
   s" lib/string.f" LC-ALL
   s" load-context: engine-provided lib/string.f" T-LABEL 64 T=
   {: outu:n erru:n :}
   outu 0 T=
   erru s" code" s" E-ENGINE-PROVIDED" LC-PACKET s" file" LC-STRING$
   s" load-context: engine-provided, file as given" T-LABEL s" lib/string.f" T$= ;

\ A file of check.f's own closure, which the engine does not provide, is
\ checked as any other: the pre-pass verifies it in the verifier child, whose
\ image does not hold it, and the run stage loads it in an engine of its own,
\ as `bin/hb --load` does.
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

FS-PATH-CAP 2 * constant LC-TYPE-CAP     \ a loader line of the longest path, and the rest
create LC-TYPE-BUF LC-TYPE-CAP allot

\ lc-type-use.f's bytes, for the checks that take a source as bytes.
: LC-TYPE-BYTES ( -- ptr u8 n )
   s" lc-type-use.f" LC-AT LC-TYPE-BUF LC-TYPE-CAP READ-ALL {: u:n :}
   LC-TYPE-BUF u ;

\ Every pass after the closure walk visits the files it found, so a type a
\ required file declares is in scope wherever the source comes from: a named
\ file, standard input or a SOURCE buffer, under --json-errors with and
\ without --all-errors.
: LC-TYPE-CASE ( -- )
   s" lc-type-use.f" ST-JSON-RUN s" load-context: required type, file" LC-EXPECT-CLEAN
   s" lc-type-use.f" REQ-PLAIN-RUN s" load-context: required type, file, all" LC-EXPECT-CLEAN
   LC-TYPE-BYTES s" --json-errors" CLI-FLAG-STDIN s" load-context: required type, stdin" LC-EXPECT-CLEAN
   LC-TYPE-BYTES LC-STDIN s" load-context: required type, stdin, all" LC-EXPECT-CLEAN
   LC-TYPE-BYTES DIRECT-JSON-STDIN s" load-context: required type, SOURCE" LC-EXPECT-CLEAN
   LC-TYPE-BYTES ALL-JSON-STDIN s" load-context: required type, SOURCE, all" LC-EXPECT-CLEAN ;

\ The check quotes a path or label into a line it writes and hands a path to
\ the file system. A path or label holding a byte the quoting refuses (double
\ quote, backslash, CR, LF, NUL) is one usage line in every mode, on stderr or,
\ under --json-errors, on stdout, and a listed
\ path holding a NUL is refused before the engine's resolver sees it. The
\ quoted spelling is the canonical path for a named file, the path as given
\ for a listed one, and the label in the run stage: a plain named file's path
\ as given, or a CHECK:SOURCE label. It is judged before anything reads the
\ source, so each case's source is one a later stage refuses, and the answer
\ is the path's whatever the file holds.
: LC-UNSAFE-LINE$ ( -- ptr u8 n )
   s" check.f: source path or label contains a double quote, backslash, CR, LF or NUL"
   LC-LINE$ ;

: LC-EXPECT-UNSAFE ( n n n ptr u8 n -- ) {: outu:n erru:n rc:n label:ptr labelu:n :}
   label labelu T-LABEL rc 64 T=
   label labelu T-LABEL outu 0 T=
   label labelu T-LABEL CAP-ERR erru LC-UNSAFE-LINE$ T$= ;

: LC-EXPECT-UNSAFE-JSON ( n n n ptr u8 n -- )
   {: outu:n erru:n rc:n label:ptr labelu:n :}
   label labelu T-LABEL rc 64 T=
   label labelu T-LABEL erru 0 T=
   label labelu T-LABEL CAP-OUT outu LC-UNSAFE-LINE$ T$= ;

: LC-NUL$ ( -- ptr u8 n )
   SB-RESET s" lc-n" SB-APPEND 0 SB-APPEND-C s" ul.f" SB-APPEND SB$ ;

: LC-UNSAFE-TARGET-CASE ( -- )
   LC-QLINK$ LC-AT LC-ALL
   s" load-context: a link to a name with a double quote, refused by discovery"
   LC-EXPECT-UNSAFE-JSON ;

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
   LC-EXPECT-UNSAFE-JSON ;

: LC-UNSAFE-SOURCE-NUL-CASE ( -- )
   RESET
   LC-DECL$SRC LC-NUL$ SOURCE
   [: RUN-ACT ;] IN-PROC
   s" load-context: a CHECK:SOURCE label with a NUL, refused declaration"
   LC-EXPECT-UNSAFE ;

: LC-UNSAFE-NUL-CASE ( -- )
   LC-NUL$ PATH-RUN
   s" load-context: a named path with a NUL" LC-EXPECT-UNSAFE ;

\ Existence is checked before the path is quoted; under --json-errors the line
\ is on stdout.
: LC-MISSING-QUOTE-CASE ( -- )
   s\" lc-missing-q\"uote.f" LC-AT LC-ALL
   s" load-context: a missing path with a double quote" T-LABEL 66 T=
   {: outu:n erru:n :}
   erru 0 T=
   CAP-OUT outu s" check.f: no such source" LC-LINE$ T$= ;

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
   LC-OTHER-SETUP
   LC-SIBLING-CASE
   LC-ENTRY-ROOT-CASE
   LC-ENGINE-CASE
   LC-REFUSED-ENGINE-CASE
   LC-LONG-ENGINE-CASE
   LC-UNREAD-CASE
   LC-UNREAD-ENGINE-CASE
   LC-UNREAD-LIST-CASE
   LC-RO-TMP-CASE
   LC-UMASK-CASE
   LC-OTHER-TREE-CASE
   LC-TREE-CASE
   LC-PROVIDED-CASE
   LC-TOOL-CLOSURE-CASE
   LC-TOOL-LIST-CASE
   LC-UNDEFINED-CASE
   LC-DUPLICATE-CASE
   LC-DEPENDENCY-CASE
   LC-DECLARATION-CASE
   LC-TYPE-CASE
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

\ A bare name two used packages both export is refused in a definition
\ (E-USING-AMBIGUOUS, checker.f LIVE-BIND) with a diagnostic of its own,
\ as a used public a global shadows is (E-USING-SHADOW-GLOBAL): every check.f
\ path writes a packet naming the token where the file holds it and each
\ package it resolves in, or under --all-errors the same as prose, and none
\ reports a statement throw. The token has a line of its own, so its place is
\ the token's, not the definition's. The refusal refuses its definition as an
\ E-UNDEFINED does, and every path exits 70: plain check.f stops there, and
\ where the check reports every refused definition, --verify-only and
\ --all-errors, it goes on, the next definition calls the refused one by its
\ declared effect and the last one's extra cell is reported too.
: UAMB-LINES ( -- )
   s" package CKT-UAMB-A" REQ-LINE+
   s" public" REQ-LINE+
   s" : CKT-UAMB-W ( n -- ) drop ;" REQ-LINE+
   s" ;package" REQ-LINE+
   s" package CKT-UAMB-B" REQ-LINE+
   s" public" REQ-LINE+
   s" : CKT-UAMB-W ( n -- ) drop ;" REQ-LINE+
   s" ;package" REQ-LINE+
   s" using CKT-UAMB-A" REQ-LINE+
   s" using CKT-UAMB-B" REQ-LINE+
   s" : CKT-UAMB-USE ( -- )" REQ-LINE+
   s"    1" REQ-LINE+
   s"    CKT-UAMB-W" REQ-LINE+
   s" ;" REQ-LINE+
   s" : CKT-UAMB-CALL ( -- ) CKT-UAMB-USE ;" REQ-LINE+
   s" : CKT-UAMB-EXTRA ( -- ) 1 ;" REQ-LINE+
   s" ;using" REQ-LINE+
   s" ;using" REQ-LINE+ ;

: UAMB$ ( -- ptr u8 n )  s" ckt-uamb.f" REQ$ ;

: USH-LINES ( -- )
   s" : CKT-USH-W ( n -- ) drop ;" REQ-LINE+
   s" package CKT-USH-P" REQ-LINE+
   s" public" REQ-LINE+
   s" : CKT-USH-W ( n -- ) drop ;" REQ-LINE+
   s" ;package" REQ-LINE+
   s" using CKT-USH-P" REQ-LINE+
   s" : CKT-USH-USE ( -- )" REQ-LINE+
   s"    1" REQ-LINE+
   s"    CKT-USH-W" REQ-LINE+
   s" ;" REQ-LINE+
   s" : CKT-USH-CALL ( -- ) CKT-USH-USE ;" REQ-LINE+
   s" : CKT-USH-EXTRA ( -- ) 1 ;" REQ-LINE+
   s" ;using" REQ-LINE+ ;

: USH$ ( -- ptr u8 n )  s" ckt-ush.f" REQ$ ;

\ tools/check.f run on the fixture as the command line runs it, after the
\ options CHECK-ARG+ added: its stderr length and exit status.
: REFUSAL-RUN ( ptr u8 n -- n n )
   CHECK-ARG+
   CHECK-CAPTURE rot drop ;

: UAMB-NUM ( n ptr u8 n ptr u8 n ptr u8 n -- )
   {: p:n key:ptr keyu:n want:ptr wantu:n label:ptr labelu:n :}
   label labelu T-LABEL p key keyu LC-NUMBER$ want wantu T$= ;

\ What the run reports after the refused definition, named by EXTRA and CALL as
\ the report spells them: the extra cell exactly when the check goes on (MORE),
\ and never the caller, which checks against the refused definition's declared
\ effect.
: REFUSAL-REST ( n bool ptr u8 n ptr u8 n ptr u8 n -- )
   {: erru:n more:bool extra:ptr extrau:n call:ptr callu:n label:ptr labelu:n :}
   label labelu T-LABEL CAP-ERR erru extra extrau CONTAINS?
   more IF TTRUE ELSE TFALSE THEN
   label labelu T-LABEL CAP-ERR erru call callu CONTAINS? TFALSE
   label labelu T-LABEL CAP-ERR erru s" E-UNDEFINED" CONTAINS? TFALSE
   label labelu T-LABEL CAP-ERR erru s" E-STATEMENT-THROW" CONTAINS? TFALSE ;

\ The checker's packet.
: UAMB-PACKET ( n ptr u8 n -- )
   {: erru:n label:ptr labelu:n :}
   erru s" code" s" E-USING-AMBIGUOUS" LC-PACKET {: p:n :}
   label labelu T-LABEL p 0 >= TTRUE
   label labelu T-LABEL
   p s" repair_class" LC-STRING$ s" disambiguate_using_ambiguous" T$=
   label labelu T-LABEL p s" token" LC-STRING$ s" CKT-UAMB-W" T$=
   p s" file" LC-STRING$ UAMB$ label labelu LC-EXPECT-FILE
   p s" line" s" 13" label labelu UAMB-NUM
   p s" column" s" 4" label labelu UAMB-NUM
   p s" byte_start" s" 192" label labelu UAMB-NUM
   p s" byte_end" s" 202" label labelu UAMB-NUM
   label labelu T-LABEL
   CAP-ERR erru s\" \"used_packages\":[\"ckt-uamb-a\",\"ckt-uamb-b\"]," CONTAINS? TTRUE ;

: UAMB-JSON-CASE ( n n n bool ptr u8 n -- )
   {: erru:n rc:n want:n more:bool label:ptr labelu:n :}
   label labelu T-LABEL rc want T=
   erru label labelu UAMB-PACKET
   erru more s" ckt-uamb-extra" s" ckt-uamb-call" label labelu REFUSAL-REST ;

\ bin/hb --load compiles the source, and the checker binds a body's token
\ through the engine's lookup (checker.f LIVE-BIND), not through the replay
\ check.f's packet comes from. With no global of the tail the engine's own
\ lookup refuses the token first (rc 94); a global defined under both usings
\ binds, and the checker refuses the reference as ambiguous, naming both
\ packages there too, at tier 0 and at tier 1.
: UAMB-LOAD$ ( n -- ptr u8 n )
   {: tier:n :}
   SB-RESET
   s" package CKT-UAMB-A public : CKT-UAMB-W ( n -- ) drop ; ;package" SB-APPEND
   $0a SB-APPEND-C
   s" package CKT-UAMB-B public : CKT-UAMB-W ( n -- ) drop ; ;package" SB-APPEND
   $0a SB-APPEND-C
   tier 0 <> IF s" 1 set-tier" SB-APPEND $0a SB-APPEND-C THEN
   s" using CKT-UAMB-A using CKT-UAMB-B" SB-APPEND
   $0a SB-APPEND-C
   s" : CKT-UAMB-W ( n -- ) drop ;" SB-APPEND
   $0a SB-APPEND-C
   s" : CKT-UAMB-USE ( -- ) 1 CKT-UAMB-W ;" SB-APPEND
   $0a SB-APPEND-C
   s" ;using ;using" SB-APPEND
   SB$ ;

: UAMB-LOAD-CASE ( n ptr u8 n -- )
   {: tier:n label:ptr labelu:n :}
   tier UAMB-LOAD$ HB-LOAD-SRC rot drop {: erru:n rc:n :}
   label labelu T-LABEL rc 67 T=
   label labelu T-LABEL
   CAP-ERR erru s" 'ckt-uamb-a:CKT-UAMB-W', 'ckt-uamb-b:CKT-UAMB-W'" CONTAINS? TTRUE ;

: TEST-USING-AMBIGUOUS ( -- )
   SB-RESET UAMB-LINES s" ckt-uamb.f" REQ-WRITE
   CHECK-ARGV-START UAMB$ REFUSAL-RUN 70 false s" using-ambiguous: plain" UAMB-JSON-CASE
   CHECK-ARGV-START s" --json-errors" CHECK-ARG+
   UAMB$ REFUSAL-RUN 70 false s" using-ambiguous: --json-errors" UAMB-JSON-CASE
   CHECK-ARGV-START s" --verify-only" CHECK-ARG+
   UAMB$ REFUSAL-RUN 70 true s" using-ambiguous: --verify-only" UAMB-JSON-CASE
   CHECK-ARGV-START s" --all-errors" CHECK-ARG+
   UAMB$ REFUSAL-RUN {: erru:n rc:n :}
   s" using-ambiguous: --all-errors" T-LABEL rc 70 T=
   s" using-ambiguous: --all-errors" T-LABEL
   CAP-ERR erru s" E-USING-AMBIGUOUS habu: bare 'CKT-UAMB-W' " CONTAINS? TTRUE
   s" using-ambiguous: --all-errors" T-LABEL
   CAP-ERR erru s" 'ckt-uamb-a:CKT-UAMB-W', 'ckt-uamb-b:CKT-UAMB-W'" CONTAINS? TTRUE
   erru true s" in ckt-uamb-extra:" s" ckt-uamb-call" s" using-ambiguous: --all-errors" REFUSAL-REST
   0 s" using-ambiguous: --load" UAMB-LOAD-CASE
   1 s" using-ambiguous: --load at tier 1" UAMB-LOAD-CASE ;

\ The shadow's packet: the token where the file holds it and the package it is
\ used from.
: USH-PACKET ( n ptr u8 n -- )
   {: erru:n label:ptr labelu:n :}
   erru s" code" s" E-USING-SHADOW-GLOBAL" LC-PACKET {: p:n :}
   label labelu T-LABEL p 0 >= TTRUE
   label labelu T-LABEL p s" token" LC-STRING$ s" CKT-USH-W" T$=
   p s" file" LC-STRING$ USH$ label labelu LC-EXPECT-FILE
   p s" line" s" 9" label labelu UAMB-NUM
   p s" column" s" 4" label labelu UAMB-NUM
   label labelu T-LABEL
   CAP-ERR erru s\" \"used_packages\":[\"ckt-ush-p\"]," CONTAINS? TTRUE ;

: USH-CASE ( n n n bool ptr u8 n -- )
   {: erru:n rc:n want:n more:bool label:ptr labelu:n :}
   label labelu T-LABEL rc want T=
   erru label labelu USH-PACKET
   erru more s" ckt-ush-extra" s" ckt-ush-call" label labelu REFUSAL-REST ;

: TEST-USING-SHADOW ( -- )
   SB-RESET USH-LINES s" ckt-ush.f" REQ-WRITE
   CHECK-ARGV-START s" --json-errors" CHECK-ARG+
   USH$ REFUSAL-RUN 70 false s" using-shadow: --json-errors" USH-CASE
   CHECK-ARGV-START s" --json-errors" CHECK-ARG+ s" --verify-only" CHECK-ARG+
   USH$ REFUSAL-RUN 70 true s" using-shadow: --verify-only" USH-CASE
   CHECK-ARGV-START s" --json-errors" CHECK-ARG+ s" --all-errors" CHECK-ARG+
   USH$ REFUSAL-RUN 70 true s" using-shadow: --all-errors" USH-CASE ;

\ A public its package's private word shadows at another width
\ (E-SHADOWED-ARITY, checker.f SHADOW-ARITY-CK) refuses its definition as the
\ using refusals above do; the caller names the public qualified.
: SBA-LINES ( -- )
   s" package CKT-SBA" REQ-LINE+
   s" : CKT-SBA-W ( n n -- n ) + ;" REQ-LINE+
   s" public" REQ-LINE+
   s" : CKT-SBA-W ( n -- n ) 1 + ;" REQ-LINE+
   s" ;package" REQ-LINE+
   s" : CKT-SBA-CALL ( n -- n ) CKT-SBA:CKT-SBA-W ;" REQ-LINE+
   s" : CKT-SBA-EXTRA ( -- ) 1 ;" REQ-LINE+ ;

: SBA$ ( -- ptr u8 n )  s" ckt-sba.f" REQ$ ;

: SBA-CASE ( n n n bool ptr u8 n -- )
   {: erru:n rc:n want:n more:bool label:ptr labelu:n :}
   label labelu T-LABEL rc want T=
   label labelu T-LABEL erru s" code" s" E-SHADOWED-ARITY" LC-PACKET 0 >= TTRUE
   erru more s" ckt-sba-extra" s" ckt-sba-call" label labelu REFUSAL-REST ;

: TEST-SHADOWED-ARITY ( -- )
   SB-RESET SBA-LINES s" ckt-sba.f" REQ-WRITE
   CHECK-ARGV-START s" --json-errors" CHECK-ARG+
   SBA$ REFUSAL-RUN 70 false s" shadowed-arity: --json-errors" SBA-CASE
   CHECK-ARGV-START s" --json-errors" CHECK-ARG+ s" --verify-only" CHECK-ARG+
   SBA$ REFUSAL-RUN 70 true s" shadowed-arity: --verify-only" SBA-CASE
   CHECK-ARGV-START s" --json-errors" CHECK-ARG+ s" --all-errors" CHECK-ARG+
   SBA$ REFUSAL-RUN 70 true s" shadowed-arity: --all-errors" SBA-CASE ;

\ The using refusals in does> clauses: a definer's clause is inside its
\ definition, so each refuses its definer as one in a body does. Plain check.f
\ stops at the first, the shadow; --verify-only and --all-errors go on to the
\ ambiguity and the last definition's extra cell (USING-PATH-CASE).
: DUSE-LINES ( -- )
   s" : CKT-DUSE-W ( n -- ) drop ;" REQ-LINE+
   s" package CKT-DUSE-A" REQ-LINE+
   s" public" REQ-LINE+
   s" : CKT-DUSE-W ( n -- ) drop ;" REQ-LINE+
   s" : CKT-DUSE-V ( n -- ) drop ;" REQ-LINE+
   s" ;package" REQ-LINE+
   s" package CKT-DUSE-B" REQ-LINE+
   s" public" REQ-LINE+
   s" : CKT-DUSE-V ( n -- ) drop ;" REQ-LINE+
   s" ;package" REQ-LINE+
   s" using CKT-DUSE-A" REQ-LINE+
   s" using CKT-DUSE-B" REQ-LINE+
   s" : CKT-DUSE-SHADOW ( n -- ) create , does> ( -- ) @ CKT-DUSE-W ;" REQ-LINE+
   s" : CKT-DUSE-AMB ( n -- ) create , does> ( -- ) @ CKT-DUSE-V ;" REQ-LINE+
   s" : CKT-DUSE-EXTRA ( -- ) 1 ;" REQ-LINE+
   s" ;using" REQ-LINE+
   s" ;using" REQ-LINE+ ;

: DUSE$ ( -- ptr u8 n )  s" ckt-duse.f" REQ$ ;

\ A run that refuses a using shadow first and a using ambiguity after it, then
\ a definition named EXTRA by its extra cell: it exits 70 with the shadow's
\ packet, and with the ambiguity's and the extra cell exactly when the check
\ goes on (MORE), never with a statement throw.
: USING-PATH-CASE ( n n bool ptr u8 n ptr u8 n -- )
   {: erru:n rc:n more:bool extra:ptr extrau:n label:ptr labelu:n :}
   label labelu T-LABEL rc 70 T=
   label labelu T-LABEL
   erru s" code" s" E-USING-SHADOW-GLOBAL" LC-PACKET 0 >= TTRUE
   label labelu T-LABEL
   erru s" code" s" E-USING-AMBIGUOUS" LC-PACKET 0 >=
   more IF TTRUE ELSE TFALSE THEN
   label labelu T-LABEL CAP-ERR erru extra extrau CONTAINS?
   more IF TTRUE ELSE TFALSE THEN
   label labelu T-LABEL CAP-ERR erru s" E-STATEMENT-THROW" CONTAINS? TFALSE ;

: TEST-USING-CLAUSE ( -- )
   SB-RESET DUSE-LINES s" ckt-duse.f" REQ-WRITE
   CHECK-ARGV-START s" --json-errors" CHECK-ARG+
   DUSE$ REFUSAL-RUN false s" ckt-duse-extra" s" using-clause: --json-errors" USING-PATH-CASE
   CHECK-ARGV-START s" --json-errors" CHECK-ARG+ s" --verify-only" CHECK-ARG+
   DUSE$ REFUSAL-RUN true s" ckt-duse-extra" s" using-clause: --verify-only" USING-PATH-CASE
   CHECK-ARGV-START s" --json-errors" CHECK-ARG+ s" --all-errors" CHECK-ARG+
   DUSE$ REFUSAL-RUN true s" ckt-duse-extra" s" using-clause: --all-errors" USING-PATH-CASE ;

\ The using refusals of top-level tokens: each refuses at its token as one in a
\ body refuses its definition, by the same rule (USING-PATH-CASE). Loaded, the
\ shadow exits 105 and the ambiguity 94.
: UTOP-LINES ( -- )
   s" : CKT-UTOP-W ( n -- ) drop ;" REQ-LINE+
   s" package CKT-UTOP-A" REQ-LINE+
   s" public" REQ-LINE+
   s" : CKT-UTOP-W ( n -- ) drop ;" REQ-LINE+
   s" : CKT-UTOP-V ( n -- ) drop ;" REQ-LINE+
   s" ;package" REQ-LINE+
   s" package CKT-UTOP-B" REQ-LINE+
   s" public" REQ-LINE+
   s" : CKT-UTOP-V ( n -- ) drop ;" REQ-LINE+
   s" ;package" REQ-LINE+
   s" using CKT-UTOP-A" REQ-LINE+
   s" using CKT-UTOP-B" REQ-LINE+
   s" 1 CKT-UTOP-W" REQ-LINE+
   s" : CKT-UTOP-MID ( -- ) ;" REQ-LINE+
   s" 1 CKT-UTOP-V" REQ-LINE+
   s" : CKT-UTOP-EXTRA ( -- ) 1 ;" REQ-LINE+
   s" ;using" REQ-LINE+
   s" ;using" REQ-LINE+ ;

: UTOP$ ( -- ptr u8 n )  s" ckt-utop.f" REQ$ ;

: TEST-USING-TOP-LEVEL ( -- )
   SB-RESET UTOP-LINES s" ckt-utop.f" REQ-WRITE
   CHECK-ARGV-START s" --json-errors" CHECK-ARG+
   UTOP$ REFUSAL-RUN false s" ckt-utop-extra" s" using-top-level: --json-errors" USING-PATH-CASE
   CHECK-ARGV-START s" --json-errors" CHECK-ARG+ s" --verify-only" CHECK-ARG+
   UTOP$ REFUSAL-RUN true s" ckt-utop-extra" s" using-top-level: --verify-only" USING-PATH-CASE
   CHECK-ARGV-START s" --json-errors" CHECK-ARG+ s" --all-errors" CHECK-ARG+
   UTOP$ REFUSAL-RUN true s" ckt-utop-extra" s" using-top-level: --all-errors" USING-PATH-CASE ;

\ A renderer `using` refuses at top level runs no word, so it defines none:
\ past a structural boundary the check refuses an undefined top-level name,
\ and a later body's (RENDERER-PATH-CASE). Loaded, `using` refuses the call
\ and neither `evaluate` runs: the shadow exits 105, the ambiguity (TWO) 94.
: UREN-WRITE ( bool ptr u8 n -- )
   {: two:bool name:ptr nameu:n :}
   SB-RESET
   s" package CKT-UREN-A public : evaluate ( ptr u8 n -- ) 2drop ; ;package" REQ-LINE+
   s" package CKT-UREN-B public : evaluate ( ptr u8 n -- ) 2drop ; ;package" REQ-LINE+
   s" using CKT-UREN-A" REQ-LINE+
   two IF s" using CKT-UREN-B" REQ-LINE+ THEN
   s\" s\" \" evaluate" REQ-LINE+
   s" : CKT-UREN-MID ( -- ) ;" REQ-LINE+
   s" CKT-UREN-NOSUCH" REQ-LINE+
   s" : CKT-UREN-AFTER ( -- ) CKT-UREN-NOBODY ;" REQ-LINE+
   two IF s" ;using" REQ-LINE+ THEN
   s" ;using" REQ-LINE+
   name nameu REQ-WRITE ;

\ A run that refuses `evaluate` by the using refusal WHY: it exits 70 and goes
\ on to refuse the undefined top-level name and the later body's undefined
\ name, deferring neither.
: RENDERER-PATH-CASE ( n n ptr u8 n ptr u8 n -- )
   {: erru:n rc:n why:ptr whyu:n label:ptr labelu:n :}
   label labelu T-LABEL rc 70 T=
   label labelu T-LABEL
   erru s" code" why whyu LC-PACKET s" token" LC-STRING$ s" evaluate" T$=
   label labelu T-LABEL
   erru s" code" s" E-UNDEFINED-TOP-LEVEL" LC-PACKET s" token" LC-STRING$ s" CKT-UREN-NOSUCH" T$=
   label labelu T-LABEL
   erru s" code" s" E-UNDEFINED" LC-PACKET s" token" LC-STRING$ s" CKT-UREN-NOBODY" T$=
   label labelu T-LABEL CAP-ERR erru s" W-CHECK-DEFERRED" CONTAINS? TFALSE ;

: TEST-USING-RENDERER ( -- )
   false s" ckt-uren-shadow.f" UREN-WRITE
   CHECK-ARGV-START s" --json-errors" CHECK-ARG+ s" --verify-only" CHECK-ARG+
   s" ckt-uren-shadow.f" REQ$ REFUSAL-RUN s" E-USING-SHADOW-GLOBAL"
   s" using-renderer: shadow, --verify-only" RENDERER-PATH-CASE
   CHECK-ARGV-START s" --json-errors" CHECK-ARG+ s" --all-errors" CHECK-ARG+
   s" ckt-uren-shadow.f" REQ$ REFUSAL-RUN s" E-USING-SHADOW-GLOBAL"
   s" using-renderer: shadow, --all-errors" RENDERER-PATH-CASE
   true s" ckt-uren-amb.f" UREN-WRITE
   CHECK-ARGV-START s" --json-errors" CHECK-ARG+ s" --verify-only" CHECK-ARG+
   s" ckt-uren-amb.f" REQ$ REFUSAL-RUN s" E-USING-AMBIGUOUS"
   s" using-renderer: ambiguity, --verify-only" RENDERER-PATH-CASE
   CHECK-ARGV-START s" --json-errors" CHECK-ARG+ s" --all-errors" CHECK-ARG+
   s" ckt-uren-amb.f" REQ$ REFUSAL-RUN s" E-USING-AMBIGUOUS"
   s" using-renderer: ambiguity, --all-errors" RENDERER-PATH-CASE ;

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
   s" check/require-target" [: TEST-REQUIRE-TARGET ;] CASE-RUN
   s" check/target-shadow" [: TEST-TARGET-SHADOW ;] CASE-RUN
   s" check/require-size-all" [: TEST-REQUIRE-SIZE-ALL ;] CASE-RUN
   s" check/require-size-all-list" [: TEST-REQUIRE-SIZE-ALL-LIST ;] CASE-RUN
   s" check/require-size-prose" [: TEST-REQUIRE-SIZE-PROSE ;] CASE-RUN
   s" check/closure-stop" [: TEST-CLOSURE-STOP ;] CASE-RUN
   s" check/closure-wide" [: TEST-CLOSURE-WIDE ;] CASE-RUN
   s" check/closure-stdin" [: TEST-CLOSURE-STDIN ;] CASE-RUN
   s" check/success-stderr" [: TEST-SUCCESS-STDERR ;] CASE-RUN ;

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
   s" check/lexer-defect-located" [: TEST-LEX-LOCATED ;] CASE-RUN
   s" check/nested-stop-located" [: TEST-NESTED-LOCATED ;] CASE-RUN
   s" check/verify-only-located" [: TEST-VERIFY-LOCATED ;] CASE-RUN
   s" check/statement-stop-located" [: TEST-STATEMENT-STOP-LOCATED ;] CASE-RUN
   s" check/discovery-stop-located" [: TEST-DISCOVERY-LOCATED ;] CASE-RUN
   s" check/raw-operand" [: TEST-RAW-OPERAND ;] CASE-RUN
   s" check/local-operand" [: TEST-LOCAL-OPERAND ;] CASE-RUN
   s" check/value-record-field-refused" [: TEST-VREC-FIELD-REFUSED ;] CASE-RUN
   s" check/value-record-scoped-refused" [: TEST-VREC-SCOPED-REFUSED ;] CASE-RUN
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
   s" check/derived-init-accessors" [: TEST-DERIVED-INIT-ACCESSORS ;] CASE-RUN
   s" check/product-replay-duplicate" [: TEST-PREPLAY-DUP ;] CASE-RUN
   s" check/product-generated-duplicate" [: TEST-GDUP-PRODUCT ;] CASE-RUN
   s" check/sumtype-generated-duplicate" [: TEST-GDUP-SUMTYPE ;] CASE-RUN
   s" check/generated-duplicate-definers" [: TEST-GDUP-DEFINERS ;] CASE-RUN ;

\ --- a definer that writes its word as text: the generates: row --------------
\ CKT-GMAKE builds `: NAME ( -- n ) OFF ;` and evaluates it, the shape of
\ lib/process-command.f COMMAND, so only its `generates:` row tells the
\ preverify what it makes. The row is a claim: the preverify believes it, and
\ the run stage, which compiles the generated text, holds it to the real word.
: GENR-PRELUDE ( -- )
   SB-RESET
   s\" require lib/codegen.f\n$60 CODEGEN:BUFFER CKT-GTEXT\n" SB-APPEND
   s\" : CKT-GMAKE ( n -- )\n   {: off:n :}\n   parse-name\n   {: a:ptr u:n :}\n" SB-APPEND
   s\"    CKT-GTEXT CODEGEN:RESET  s\q : \q CKT-GTEXT CODEGEN:APPEND-STRING\n" SB-APPEND
   s\"    a u CKT-GTEXT CODEGEN:APPEND-STRING  s\q  ( -- n ) \q CKT-GTEXT CODEGEN:APPEND-STRING\n" SB-APPEND
   s\"    off CKT-GTEXT CODEGEN:APPEND-DECIMAL  s\q  ;\q CKT-GTEXT CODEGEN:APPEND-STRING\n" SB-APPEND
   s\"    CKT-GTEXT CODEGEN:CONTENTS INCLUDE-EVALUATE ;\n" SB-APPEND ;

\ The created word is known, so the undefined word named is the real one. The
\ use sits outside the package section the statement marks: inside it, the mark
\ would leave the whole definition to the run, row or no row.
: TEST-GENR-UNDEFINED ( -- )
   GENR-PRELUDE
   s\" generates: CKT-GMAKE ( -- n )\npackage CKT-GP\npublic\n7 CKT-GMAKE CKT-GSEVEN\n;package\n" SB-APPEND
   s\" : CKT-GUSE ( -- n ) CKT-GP:CKT-GSEVEN CKT-GNOPE + ;\n" SB-APPEND
   SB$ DIRECT-STDIN 70 T=
   {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru s" E-UNDEFINED" CONTAINS? TTRUE
   CAP-ERR erru s\" \"token\":\"CKT-GNOPE\"" CONTAINS? TTRUE
   CAP-ERR erru s\" \"token\":\"CKT-GP:CKT-GSEVEN\"" CONTAINS? TFALSE ;

\ A lying row is believed: callers that use the word as its text defines it are
\ refused before the run ...
: TEST-GENR-LIE-CONTRADICTED ( -- )
   GENR-PRELUDE
   s\" generates: CKT-GMAKE ( -- ptr n )\n7 CKT-GMAKE CKT-GSEVEN\n: CKT-GUSE ( -- n ) CKT-GSEVEN 1 + ;\n" SB-APPEND
   SB$ DIRECT-STDIN 70 T=
   {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru s" E-MISMATCH" CONTAINS? TTRUE
   CAP-ERR erru s" preverify" CONTAINS? TTRUE ;

\ ... and callers that agree with the lie pass the preverify and are refused
\ by the run stage against the word the text really defines.
: TEST-GENR-LIE-AGREED ( -- )
   GENR-PRELUDE
   s\" generates: CKT-GMAKE ( -- ptr n )\n7 CKT-GMAKE CKT-GSEVEN\n: CKT-GUSE ( -- ptr n ) CKT-GSEVEN ;\n" SB-APPEND
   SB$ DIRECT-STDIN 70 T=
   {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru s" at 'CKT-GSEVEN' expected: ptr n actual: n" CONTAINS? TTRUE
   CAP-ERR erru s" preverify" CONTAINS? TFALSE ;

\ A row the preverify cannot keep is refused under its own code, saying why. The
\ checker reports that refusal itself, and a reported refusal is a refusal: the
\ check fails with 70, the packet keeps the row's code, and under --json-errors
\ standard error holds that packet and nothing else (TEST-GENR-BADSIG-JSON).
: GENR-REFUSED ( ptr u8 n ptr u8 n -- )
   {: src:ptr srcu:n why:ptr whyu:n :}
   GENR-PRELUDE
   src srcu SB-APPEND
   SB$ DIRECT-STDIN 70 T=
   {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru s" preverify failed" CONTAINS? TTRUE
   CAP-ERR erru s\" \"code\":\"E-GENERATES-ROW\"" CONTAINS? TTRUE
   CAP-ERR erru why whyu CONTAINS? TTRUE ;

: TEST-GENR-ON-DOES ( -- )
   s\" : CKT-GDD ( n -- ) create , does> ( -- ptr n ) ;\ngenerates: CKT-GDD ( -- ptr n )\n"
   s" already states what it makes" GENR-REFUSED ;

: TEST-GENR-BEFORE ( -- )
   s\" generates: CKT-GLATE ( -- n )\n: CKT-GLATE ( n -- ) CKT-GMAKE ;\n"
   s" names no word here" GENR-REFUSED ;

: TEST-GENR-BADSIG ( -- )
   s\" generates: CKT-GMAKE ( -- i32 )\n"
   s" Use a known stack-signature type" GENR-REFUSED ;

\ A row whose effect does not parse carries the signature refusal's own class.
\ The command line fails as for any refusal, its standard error one JSON packet.
: TEST-GENR-BADSIG-JSON ( -- )
   GENR-PRELUDE
   s\" generates: CKT-GMAKE ( -- i32 )\n" SB-APPEND
   SB$ s" --json-errors" CLI-FLAG-STDIN REFUSED-LINE {: erru:n :}
   CAP-ERR c@ $7B T=
   CAP-ERR erru s\" \"code\":\"E-GENERATES-ROW\"" CONTAINS? TTRUE
   CAP-ERR erru s\" \"repair_class\":\"fix_signature_type\"" CONTAINS? TTRUE ;

\ Under --all-errors a refused row is one finding among the file's others.
: TEST-GENR-ALL-ERRORS ( -- )
   GENR-PRELUDE
   s\" generates: CKT-GMAKE ( -- i32 )\n: CKT-GBAD ( -- n n ) 1 ;\n" SB-APPEND
   SB$ ALL-JSON-STDIN 70 T=
   {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru s" E-GENERATES-ROW" CONTAINS? TTRUE
   CAP-ERR erru s" ckt-gbad" CONTAINS? TTRUE ;

\ A support file's row is replayed ahead of the file that uses it once per
\ scope, so no replay meets the row an earlier one recorded.
: TEST-GENR-CLOSURE ( -- )
   GENR-PRELUDE
   s\" generates: CKT-GMAKE ( -- n )\n" SB-APPEND
   SUP$ SB$ WRITE-ALL
   USE$ s\" 7 CKT-GMAKE CKT-GSEVEN\n: CKT-GUSE ( -- n ) CKT-GSEVEN ;\n" WRITE-ALL
   SUP$ USE$ CLI-ALL-LIST 0 T=
   {: outu:n erru:n :}
   outu 0 T=
   erru 0 T= ;

\ A name the definer never makes passes the preverify on the row's word and is
\ undefined when the source really runs.
: TEST-GENR-NEVER-MADE ( -- )
   SB-RESET
   s\" : CKT-GNOP ( n -- ) drop parse-name 2drop ;\ngenerates: CKT-GNOP ( -- n )\n" SB-APPEND
   s\" 7 CKT-GNOP CKT-GHOLLOW\n: CKT-GUSE ( -- n ) CKT-GHOLLOW ;\n" SB-APPEND
   SB$ DIRECT-STDIN 70 T=
   {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru s" E-UNDEFINED: CKT-GHOLLOW" CONTAINS? TTRUE
   CAP-ERR erru s" preverify" CONTAINS? TFALSE ;

\ A definer name the used scopes refuse is refused by the preverify as a use of
\ it is, with that refusal's own code and repair class, and fails as a refusal:
\ here a used public shadows a global, which the load refuses as
\ E-USING-SHADOW-GLOBAL.
: TEST-GENR-SHADOW ( -- )
   SB-RESET
   s\" : CKT-GW ( n -- ) drop ;\npackage CKT-GP\npublic\n: CKT-GW ( n -- ) drop parse-name 2drop ;\n;package\n" SB-APPEND
   s\" using CKT-GP\ngenerates: CKT-GW ( -- n )\n;using\n" SB-APPEND
   SB$ DIRECT-STDIN 70 T=
   {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru s" preverify failed" CONTAINS? TTRUE
   CAP-ERR erru s\" \"code\":\"E-USING-SHADOW-GLOBAL\"" CONTAINS? TTRUE
   CAP-ERR erru s" disambiguate_using_shadow" CONTAINS? TTRUE ;

\ Every mode reads a row as the load does, so each takes the row the load takes
\ and refuses the row it refuses: the load, the check, and the check that stops
\ before the run. Each runs in a process of its own, as a pre-pass that dies
\ ends its process. The definer makes nothing, so the row is all a source says.
: GENR-MODE ( n n n bool -- )
   {: outu:n erru:n rc:n taken:bool :}
   taken IF rc 0 T= ELSE rc 0 T<> THEN ;

: GENR-MODES ( ptr u8 n bool -- )
   {: row:ptr rowu:n taken:bool :}
   SB-RESET
   s\" : CKT-GNOP ( n -- ) drop parse-name 2drop ;\n" SB-APPEND
   row rowu SB-APPEND
   s" generates: row under --load" T-LABEL
   SB$ HB-LOAD-SRC taken GENR-MODE
   s" generates: row under check.f" T-LABEL
   SB$ CLI-STDIN taken GENR-MODE
   s" generates: row under check.f --verify-only" T-LABEL
   CHECK-ARGV-START
   s" --verify-only" CHECK-ARG+
   s" --stdin-path" CHECK-ARG+
   s" generates-row.f" CHECK-ARG+
   SB$ CHECK-STDIN-CAPTURE taken GENR-MODE ;

\ `undefine` retires the definer's row, so the definer defined again states
\ what it makes afresh.
: TEST-GENR-UNDEFINE ( -- )
   s\" generates: CKT-GNOP ( -- n )\nundefine CKT-GNOP\n: CKT-GNOP ( n -- ) drop parse-name 2drop ;\ngenerates: CKT-GNOP ( -- ptr n )\n"
   0 0= GENR-MODES ;

\ The effect is read as a definition head's signature is: the first `)` byte
\ closes it, whatever it is glued to or followed by ...
: TEST-GENR-GLUED-CLOSE ( -- )
   s\" generates: CKT-GNOP ( -- n)\n" 0 0= GENR-MODES ;

: TEST-GENR-AFTER-CLOSE ( -- )
   s\" generates: CKT-GNOP ( -- n )7 drop\n" 0 0= GENR-MODES ;

\ ... a `(` opens it glued to a token ...
: TEST-GENR-GLUED-OPEN ( -- )
   s\" generates: CKT-GNOP (-- n )\n" 0 0= GENR-MODES ;

\ ... a line feed inside it stays in the token it ends, which no type names ...
: TEST-GENR-EFFECT-LINES ( -- )
   s\" generates: CKT-GNOP ( -- n\n)\n" 0 0= 0= GENR-MODES ;

\ ... and the name is the next token, a comment opener too, so no effect follows.
: TEST-GENR-COMMENT-NAME ( -- )
   s\" generates: \\ the definer\nCKT-GNOP ( -- n )\n" 0 0= 0= GENR-MODES ;

\ The pre-pass keeps room for every effect the engine takes: here 76 bytes.
: TEST-GENR-LONG-EFFECT ( -- )
   s\" generates: CKT-GNOP ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- ptr u8 n ptr u8 n ptr u8 n ptr u8 n )\n"
   0 0= GENR-MODES ;

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
   s" check/rendered-command" [: TEST-RENDERED-COMMAND ;] CASE-RUN
   s" check/rendered-does" [: TEST-RENDERED-DOES ;] CASE-RUN
   s" check/refused-clause" [: TEST-REFUSED-CLAUSE ;] CASE-RUN
   s" check/layout-buffer-count" [: TEST-LAYOUT-BUFFER-COUNT ;] CASE-RUN
   s" check/file-label" [: TEST-FILE-LABEL ;] CASE-RUN
   s" check/usage-direct" [: TEST-USAGE ;] CASE-RUN
   s" check/deadline-option" [: TEST-DEADLINE-OPTION ;] CASE-RUN
   s" check/source-bytes-copy" [: TEST-SOURCE-BYTES-COPY ;] CASE-RUN
   s" check/file-path-copy" [: TEST-FILE-PATH-COPY ;] CASE-RUN
   s" check/source-list-idempotent" [: TEST-LIST-IDEMPOTENT ;] CASE-RUN
   s" check/source-list-promotion" [: TEST-LIST-PROMOTION ;] CASE-RUN
   s" check/empty-source-mode" [: TEST-EMPTY-SOURCE-MODE ;] CASE-RUN
   s" check/boundary-phase" [: TEST-BOUNDARY-PHASE ;] CASE-RUN
   s" check/source-list-boundary" [: TEST-BOUNDARY-LIST ;] CASE-RUN
   s" check/options" [: TEST-OPTIONS ;] CASE-RUN
   s" check/mode-collisions" [: TEST-MODE-COLLISIONS ;] CASE-RUN
   s" check/die" [: TEST-DIE ;] CASE-RUN
   s" check/forward-ref-direct" [: TEST-FWDREF-DIRECT ;] CASE-RUN
   s" check/forward-ref-json" [: TEST-FWDREF-JSON ;] CASE-RUN
   s" check/origin-scan" [: TEST-ORIGIN-SCAN ;] CASE-RUN
   s" check/origin-base" [: TEST-ORIGIN-BASE ;] CASE-RUN
   s" check/origin-multi" [: TEST-ORIGIN-MULTI ;] CASE-RUN
   s" check/origin-declaration" [: TEST-ORIGIN-DECL ;] CASE-RUN
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
   s" check/run-refusals" [: TEST-RUN-REFUSALS ;] CASE-RUN
   s" check/sumtype-all-redrive" [: SUM-REDRIVE-TEST ;] CASE-RUN
   s" check/nominal-all-redrive" [: NOM-REDRIVE-TEST ;] CASE-RUN
   s" check/nominal-all-clean" [: NOM-CLEAN-TEST ;] CASE-RUN
   s" check/tfam-noarity" [: TEST-TFAM-NOARITY ;] CASE-RUN
   s" check/sum-noend" [: TEST-SUM-NOEND ;] CASE-RUN
   s" check/sum-noend-all" [: SUM-NOEND-ALL ;] CASE-RUN
   s" check/tfam-noarity-all" [: TFAM-NOARITY-ALL ;] CASE-RUN
   s" check/cast-badsig-all-errors" [: CAST-BADSIG-ALL ;] CASE-RUN
   s" check/selection-capacity" [: TEST-SELECTION-CAPACITY ;] CASE-RUN
   s" check/big-source" [: TEST-BIG-SOURCE ;] CASE-RUN
   s" check/run-output-cap" [: TEST-RUN-OUTPUT-CAP ;] CASE-RUN
   s" check/die-scratch" [: TEST-DIE-SCRATCH ;] CASE-RUN
   s" check/long-name" [: TEST-LONG-NAME ;] CASE-RUN
   s" check/refusals" [: TEST-REFUSALS ;] CASE-RUN
   s" check/every-refusal" [: TEST-EVERY-REFUSAL ;] CASE-RUN
   s" check/every-refusal-record" [: TEST-EVERY-RECORD ;] CASE-RUN
   s" check/scratch-full" [: TEST-SCRATCH-FULL ;] CASE-RUN
   s" check/render-full" [: TEST-RENDER-FULL ;] CASE-RUN
   s" check/render-full-effect" [: TEST-RENDER-FULL-EFFECT ;] CASE-RUN
   s" check/list-capacity" [: TEST-LIST-CAPACITY ;] CASE-RUN
   s" check/empty-list" [: TEST-EMPTY-LIST ;] CASE-RUN
   s" check/missing-file" [: TEST-MISSING-FILE ;] CASE-RUN
   s" check/cleanup-failure" [: TEST-CLEANUP-FAILURE ;] CASE-RUN
   s" check/run-environment" [: TEST-RUN-ENVIRONMENT ;] CASE-RUN
   s" check/repeat-source-ok" [: TEST-REPEAT-SOURCE-OK ;] CASE-RUN
   s" check/repeat-source-fail" [: TEST-REPEAT-SOURCE-FAIL ;] CASE-RUN
   s" check/repeat-file" [: TEST-REPEAT-FILE ;] CASE-RUN
   s" check/repeat-list" [: TEST-REPEAT-LIST ;] CASE-RUN
   s" check/repeat-definer" [: TEST-REPEAT-DEFINER ;] CASE-RUN
   s" check/prepass-view" [: TEST-PREPASS-VIEW ;] CASE-RUN
   s" check/oversize" [: TEST-OVERSIZE ;] CASE-RUN
   DECL-CASES
   REQUIRE-CASES
   s" check/statement-throw-json" [: TEST-STATEMENT-THROW-JSON ;] CASE-RUN
   s" check/statement-throw-prose" [: TEST-STATEMENT-THROW-PROSE ;] CASE-RUN
   s" check/malformed-name" [: TEST-MALFORMED-NAME ;] CASE-RUN
   s" check/malformed-name-all-errors" [: TEST-MALFORMED-NAME-ALL ;] CASE-RUN
   s" check/malformed-call-all-errors" [: TEST-MALFORMED-CALL-ALL ;] CASE-RUN
   s" check/malformed-raw" [: TEST-MALFORMED-RAW ;] CASE-RUN
   s" check/using-at-source" [: TEST-USING-AT-SOURCE ;] CASE-RUN
   s" check/name-colon" [: TEST-NAME-COLON ;] CASE-RUN
   s" check/using-ambiguous" [: TEST-USING-AMBIGUOUS ;] CASE-RUN
   s" check/using-shadow" [: TEST-USING-SHADOW ;] CASE-RUN
   s" check/shadowed-arity" [: TEST-SHADOWED-ARITY ;] CASE-RUN
   s" check/using-clause" [: TEST-USING-CLAUSE ;] CASE-RUN
   s" check/using-top-level" [: TEST-USING-TOP-LEVEL ;] CASE-RUN
   s" check/using-renderer" [: TEST-USING-RENDERER ;] CASE-RUN
   s" check/shebang-clean" [: TEST-SHEBANG-CLEAN ;] CASE-RUN
   s" check/shebang-at-line" [: TEST-SHEBANG-AT-LINE ;] CASE-RUN
   s" check/shebang-later" [: TEST-SHEBANG-LATER ;] CASE-RUN
   s" check/capacity-throw" [: TEST-CAPACITY-THROW ;] CASE-RUN
   s" check/declared-name-cap" [: TEST-DECLARED-NAME-CAP ;] CASE-RUN
   s" check/package-name-cap" [: TEST-PACKAGE-NAME-CAP ;] CASE-RUN
   s" check/qualifier-cap" [: TEST-QUALIFIER-CAP ;] CASE-RUN
   s" check/effect-depth-cap" [: TEST-EFFECT-DEPTH-CAP ;] CASE-RUN
   s" check/input-width-cap" [: TEST-INPUT-WIDTH-CAP ;] CASE-RUN
   s" check/storage-type" [: TEST-STORAGE-TYPE ;] CASE-RUN
   s" check/storage-name" [: TEST-STORAGE-NAME ;] CASE-RUN
   s" check/storage-defer" [: TEST-STORAGE-DEFER ;] CASE-RUN
   s" check/image-tool-sources" [: TEST-IMAGE-TOOL-SOURCES ;] CASE-RUN
   s" check/source-list-all-errors" [: LIST-ALL-TEST ;] CASE-RUN
   s" check/file-load-context" [: TEST-LOAD-CONTEXT ;] CASE-RUN
   s" check/generates-undefined" [: TEST-GENR-UNDEFINED ;] CASE-RUN
   s" check/generates-lie-contradicted" [: TEST-GENR-LIE-CONTRADICTED ;] CASE-RUN
   s" check/generates-lie-agreed" [: TEST-GENR-LIE-AGREED ;] CASE-RUN
   s" check/generates-on-does" [: TEST-GENR-ON-DOES ;] CASE-RUN
   s" check/generates-before" [: TEST-GENR-BEFORE ;] CASE-RUN
   s" check/generates-badsig" [: TEST-GENR-BADSIG ;] CASE-RUN
   s" check/generates-badsig-json" [: TEST-GENR-BADSIG-JSON ;] CASE-RUN
   s" check/generates-all-errors" [: TEST-GENR-ALL-ERRORS ;] CASE-RUN
   s" check/generates-closure" [: TEST-GENR-CLOSURE ;] CASE-RUN
   s" check/generates-never-made" [: TEST-GENR-NEVER-MADE ;] CASE-RUN
   s" check/generates-shadow" [: TEST-GENR-SHADOW ;] CASE-RUN
   s" check/generates-undefine" [: TEST-GENR-UNDEFINE ;] CASE-RUN
   s" check/generates-glued-close" [: TEST-GENR-GLUED-CLOSE ;] CASE-RUN
   s" check/generates-after-close" [: TEST-GENR-AFTER-CLOSE ;] CASE-RUN
   s" check/generates-glued-open" [: TEST-GENR-GLUED-OPEN ;] CASE-RUN
   s" check/generates-effect-lines" [: TEST-GENR-EFFECT-LINES ;] CASE-RUN
   s" check/generates-comment-name" [: TEST-GENR-COMMENT-NAME ;] CASE-RUN
   s" check/generates-long-effect" [: TEST-GENR-LONG-EFFECT ;] CASE-RUN
   CLEANUP-RUN
   T-REPORT
   s" check-test: ok" type cr ;

public

: TEST ( -- )
   TEST-MAIN ;

;package
