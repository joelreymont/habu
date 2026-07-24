\ check-test-lib.f - checked engine CLI/core smoke coverage library.
\ Run: bin/hb --load lib/date.f lib/errors.f lib/string.f lib/test.f lib/memory.f
\ lib/vector.f lib/fs.f lib/fs-mutate.f lib/process.f lib/process-argv.f
\ lib/process-env.f lib/process-cwd.f lib/source.f
\ tools/lint/text.f tools/lint/token.f tools/lint/lib.f
\ tools/lint/json-writer.f tools/lint/source-lex.f
\ tools/diag-origin-core.f tools/json.f tools/json-only-core.f
\ tools/signature-lint-core.f tools/checked-boundary-lint-core.f
\ tools/reserved-name-lint-core.f
\ tools/trust-lint-core.f tools/check-all-errors-core.f lib/argv.f
\ tools/check-core.f tools/check-test.f

require lib/date.f
require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
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
require tools/signature-lint-core.f
require tools/checked-boundary-lint-core.f
require tools/reserved-name-lint-core.f
require tools/trust-lint-core.f
require tools/check-all-errors-core.f
require lib/argv.f
require tools/check-core.f

package CHECK-TEST

using CHECK
private

7121 constant LBUF-RC

$4000 constant BUF-CAP
$100001 constant OVERCAP-SOURCE-LEN
128 constant LIST-ENTRY-CAP

create TMP-ROOT FS-PATH-CAP allot
create BAD-PATH FS-PATH-CAP allot
create DIRECT-PATH FS-PATH-CAP allot
create LIST-PATH FS-PATH-CAP allot
create INC-DEP-PATH FS-PATH-CAP allot
create INC-ENTRY-PATH FS-PATH-CAP allot
create SUP-PATH FS-PATH-CAP allot
create USE-PATH FS-PATH-CAP allot
create MUT-SRC $200 allot
create MUT-LABEL FS-PATH-CAP allot
create MUT-PATH FS-PATH-CAP allot
create BOUNDARY-OUT BUF-CAP allot
create CLEANUP-TMP FS-PATH-CAP allot
create CLI-ROOT FS-PATH-CAP allot
create CLI-TARGET FS-PATH-CAP allot
create CLI-LINK-PATH FS-PATH-CAP allot
create CLI-HB FS-PATH-CAP allot

variable OUT-A
variable ERR-A
variable TMP-ROOT-U
variable BAD-U
variable DIRECT-U
variable LIST-U
variable INC-DEP-U
variable INC-ENTRY-U
variable SUP-U
variable USE-U
variable MUT-SRC-U
variable MUT-LABEL-U
variable MUT-PATH-U
variable CLEANUP-TMP-U
variable CLI-ROOT-U
variable CLI-TARGET-U
variable CLI-LINK-U
variable CLI-HB-U
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

: ABS-PATH? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   u 0 > if a c@ $2F = else 0 0= 0= then ;

: CLI-LINK+ ( ptr u8 n -- ) {: name:ptr nameu:n :}
   s" PWD" GETENV name nameu CLI-TARGET JOIN-PATH CLI-TARGET-U !
   CLI-ROOT$ name nameu CLI-LINK-PATH JOIN-PATH CLI-LINK-U !
   CLI-TARGET CLI-TARGET-U @ CLI-LINK-PATH CLI-LINK-U @ MAKE-SYMLINK ;

: OUT-A-FIELD ( -- ptr ptr u8 )
   OUT-A 0 ptr-field ;

: ERR-A-FIELD ( -- ptr ptr u8 )
   ERR-A 0 ptr-field ;

: OUT-A@ ( -- ptr u8 )
   OUT-A-FIELD @ ;

: ERR-A@ ( -- ptr u8 )
   ERR-A-FIELD @ ;

: OUT-A! ( ptr u8 -- )
   OUT-A-FIELD ! ;

: ERR-A! ( ptr u8 -- )
   ERR-A-FIELD ! ;

: CAP-ALLOC ( -- ptr u8 )
   BUF-CAP MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop ;

: CAP-OUT ( -- ptr u8 )
   OUT-A @ 0= if CAP-ALLOC OUT-A! then
   OUT-A@ ;

: CAP-ERR ( -- ptr u8 )
   ERR-A @ 0= if CAP-ALLOC ERR-A! then
   ERR-A@ ;

: CORE-ACT ( -- )
   BAD$ BAD$ CHECK-ALL-ERRORS-FILE ;

: CORE-JSON ( ptr u8 n -- n n n ) {: src:ptr srcu:n :}
   BAD$ src srcu WRITE-ALL
   CAP-ERR BUF-CAP CAP-OUT BUF-CAP CHECK-ALL-ERRORS-BUFFERS!
   0 0= CHECK-ALL-ERRORS-JSON!
   [: CORE-ACT ;] catch {: rc:n :}
   0 CHECK-ALL-ERRORS-OUT$ nip rc ;

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
   CAP-ERR BUF-CAP >LEN $2710 >MS RUN-ARGV-CAPTURE
   CAPTURE>N ;

: CHECK-STDIN-CAPTURE ( ptr u8 n -- n n n )
   {: src:ptr srcu:n :}
   HB$ >LEN src srcu >LEN
   CAP-OUT BUF-CAP >LEN CAP-ERR BUF-CAP >LEN
   $2710 >MS RUN-ARGV-STDIN-CAPTURE
   CAPTURE>N ;

: DIRECT-STDIN ( ptr u8 n -- n n n )
   CHECK-ARGV-START
   CHECK-STDIN-CAPTURE ;

: DIRECT-ALL-STDIN ( ptr u8 n -- n n n )
   CHECK-ARGV-START
   s" --all-errors" CHECK-ARG+
   CHECK-STDIN-CAPTURE ;

: ALL-JSON-STDIN ( ptr u8 n -- n n n )
   CHECK-ARGV-START
   s" --all-errors" CHECK-ARG+
   s" --json-errors" CHECK-ARG+
   CHECK-STDIN-CAPTURE ;

: DIRECT-JSON-STDIN ( ptr u8 n -- n n n )
   CHECK-ARGV-START
   s" --json-errors" CHECK-ARG+
   CHECK-STDIN-CAPTURE ;

: LIST-RUN ( ptr u8 n -- n n n )
   CHECK-ARGV-START
   s" --source-list" CHECK-ARG+
   CHECK-ARG+
   CHECK-CAPTURE ;

: DIRECT-ALL-LIST ( ptr u8 n ptr u8 n -- n n n )
   {: aa:ptr au:n ba:ptr bu:n :}
   CHECK-ARGV-START
   s" --json-errors" CHECK-ARG+
   s" --all-errors" CHECK-ARG+
   s" --source-list" CHECK-ARG+
   aa au CHECK-ARG+
   ba bu CHECK-ARG+
   CHECK-CAPTURE ;

: LIST-PREVERIFY ( ptr u8 n -- n n n )
   LIST-RUN ;

: DIRECT-PREVERIFY-PATH ( ptr u8 n -- n n n )
   CHECK-ARGV-START
   CHECK-ARG+
   CHECK-CAPTURE ;

: DIRECT-BAD-FLAG ( -- n n n )
   CHECK-ARGV-START
   s" --bad-flag" CHECK-ARG+
   CHECK-CAPTURE ;

: CLI-HB$ ( -- ptr u8 n )
   HB$ {: hb:ptr hbu:n :}
   hb hbu ABS-PATH? if hb hbu exit then
   s" PWD" GETENV hb hbu CLI-HB JOIN-PATH CLI-HB-U !
   CLI-HB CLI-HB-U @ ;

: CLI-SETUP ( -- )
   s" habu-check-missing-engine" TMPDIR-MKDIR
   CLI-ROOT CLI-ROOT-U PATH-COPY!
   CLI-ROOT$ CLEANUP-TREE+
   s" lib" CLI-LINK+
   s" tools" CLI-LINK+
   s" src" CLI-LINK+ ;

: CLI-MISSING-HB ( -- n n n )
   CLI-SETUP
   PROC-ARGV-RESET
   PROC-ENV-RESET
   PROC-ENV-INHERIT-MISSING
   s" --load" >LEN PROC-ARGV+
   s" tools/check.f" >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   CLI-HB$ >LEN CLI-ROOT$ >LEN
   CAP-OUT BUF-CAP >LEN CAP-ERR BUF-CAP >LEN $2710 >MS
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
   PROC-ENV-INHERIT-MISSING
   HB$ >LEN CAP-OUT BUF-CAP >LEN CAP-ERR BUF-CAP >LEN
   $2710 >MS RUN-ARGV-ENV-CAPTURE
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

: HB-LOAD-FWDREF ( -- n n n )
   BAD$ FWDREF$ WRITE-ALL
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   BAD$ >LEN PROC-ARGV+
   HB$ >LEN CAP-OUT BUF-CAP >LEN
   CAP-ERR BUF-CAP >LEN $2710 >MS RUN-ARGV-CAPTURE
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
   s" TYPEFAMILY tfck 1" SB-APPEND
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
: TFAM-NOARITY$ ( -- ptr u8 n )   \ TYPEFAMILY missing its arity token
   SB-RESET
   s" TYPEFAMILY ckfnoar" SB-APPEND
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

: FAM-PRIV$ ( -- ptr u8 n )        \ private family: qualified lookup is public-only
   SB-RESET
   s" package CKFP" SB-APPEND $0a SB-APPEND-C
   s" SUMTYPE ckfpfam 0 VARIANT keep n ;VARIANT ;SUMTYPE" SB-APPEND $0a SB-APPEND-C
   s" ;package" SB-APPEND $0a SB-APPEND-C
   s" : CKF-PRIV ( ckfp:ckfpfam -- ckfp:ckfpfam ) ;" SB-APPEND
   SB$ ;

: FAM-NONAME$ ( -- ptr u8 n )      \ scanner boundary: package without a name token
   s" package" ;

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

: PREPARE ( -- )
   CLEANUP-RESET
   s" habu-check-test" TMPDIR-MKDIR TMP-ROOT TMP-ROOT-U PATH-COPY!
   ROOT$ CLEANUP-TREE+
   ROOT$ s" bad.f" BAD-PATH JOIN-PATH BAD-U !
   ROOT$ s" direct-source.f" DIRECT-PATH JOIN-PATH DIRECT-U !
   ROOT$ s" local-test.f" LIST-PATH JOIN-PATH LIST-U !
   ROOT$ s" inc-dep.f" INC-DEP-PATH JOIN-PATH INC-DEP-U !
   ROOT$ s" inc-entry.f" INC-ENTRY-PATH JOIN-PATH INC-ENTRY-U !
   ROOT$ s" list-sup.f" SUP-PATH JOIN-PATH SUP-U !
   ROOT$ s" list-use.f" USE-PATH JOIN-PATH USE-U !
   BAD$ BAD$SRC WRITE-ALL ;

: TEST-GOOD ( -- )
   [: GOOD$ VERIFY:SOURCE-BUF ;] catch 0 T= ;

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
   [: LBUF-OLD$ VERIFY:SOURCE-BUF ;] catch LBUF-RC T= ;

: TEST-FILE-LABEL ( -- )
   BAD$SRC CORE-JSON 70 T=
   {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru BAD$ CONTAINS? TTRUE ;

: TEST-USAGE ( -- )
   DIRECT-BAD-FLAG 64 T=
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
   s" strict-signatures" OPT
   s" json-errors" OPT
   s" all-errors" OPT
   s" json-errors" OPT
   s" strict-signatures" OPT
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
   DIE$ DIRECT-STDIN 5 T=
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
   HB-LOAD-FWDREF 70 T=
   {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru s" E-UNDEFINED: CKT-MISSING" CONTAINS? TTRUE ;

: EXPECT-UNTERM-STRING ( ptr u8 n -- ) {: src:ptr srcu:n :}
   src srcu CORE-JSON 70 T=
   {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru s" E-" CONTAINS? TTRUE ;

: TEST-UNTERM-STRING ( -- )
   UNTERM-SDQ$ EXPECT-UNTERM-STRING ;

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
   s" lib/test.f" LIST-PREVERIFY 0 T=
   {: outu:n erru:n :}
   outu 0 T=
   erru 0 T= ;

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

: TEST-TYPEFAMILY-GOOD ( -- )
   TFAM-GOOD$ DIRECT-STDIN 0 T=
   {: outu:n erru:n :}
   outu 0 T=
   erru 0 T= ;

: TEST-TYPEFAMILY-ALL ( -- )
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
\ reaches the JSON redrive re-run. SUMTYPE/TYPEFAMILY/DEFLINEAR declarers are
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

: OVERCAP-SOURCE-BODY ( n ptr u8 CAD-NUM:alloc-byte-len -- )
   {: cap:n src:ptr extent:CAD-NUM:alloc-byte-len :}
   src cap s" <stdin>" SOURCE ;

: OVERCAP-SOURCE-THROW ( -- )
   OVERCAP-SOURCE-LEN dup MEM:BYTES-ALLOC-LEN
   [: OVERCAP-SOURCE-BODY ;] MEM:WITH-BYTES ;

: OVERCAP-FILE-BODY ( n ptr u8 CAD-NUM:alloc-byte-len -- )
   {: cap:n src:ptr extent:CAD-NUM:alloc-byte-len :}
   DIRECT$ src cap WRITE-ALL ;

: TEST-OVERCAP-SOURCE ( -- )
   RESET
   [: OVERCAP-SOURCE-THROW ;] catch 66 T=
   RESET
   OVERCAP-SOURCE-LEN dup MEM:BYTES-ALLOC-LEN
   [: OVERCAP-FILE-BODY ;] MEM:WITH-BYTES
   CHECK-ARGV-START
   DIRECT$ CHECK-ARG+
   CHECK-CAPTURE 66 T=
   {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru s" source exceeds capacity" CONTAINS? TTRUE
   RESET ;

: OVERCAP-PATH-BODY ( n ptr u8 CAD-NUM:alloc-byte-len -- )
   {: cap:n path:ptr extent:CAD-NUM:alloc-byte-len :}
   path cap FILE ;

: OVERCAP-PATH ( -- )
   FS-PATH-CAP 1+ dup MEM:BYTES-ALLOC-LEN
   [: OVERCAP-PATH-BODY ;] MEM:WITH-BYTES ;

: OVERCAP-LABEL-BODY ( n ptr u8 CAD-NUM:alloc-byte-len -- )
   {: cap:n label:ptr extent:CAD-NUM:alloc-byte-len :}
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
   CHECK-ARGV-START
   s" --source-list" CHECK-ARG+
   CHECK-CAPTURE 64 T=
   {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru s" usage: tools/check.f" CONTAINS? TTRUE ;

: TEST-MISSING-FILE ( -- )
   RESET
   MISSING-FILE
   RUN 66 T=
   RESET ;

: TEST-MISSING-ENGINE ( -- )
   CLI-MISSING-HB 69 T=
   {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru s\" check.f: bin/hb missing\n" LINT-STR= TTRUE ;

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
   ENUM-NOEND$ DIRECT-STDIN 70 T=
   {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru s" check.f: missing ;ENUM" CONTAINS? TTRUE ;

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
: ENUM-CLI-RUN ( -- n n n )
   BAD$ ENUM-NOEND$ WRITE-ALL
   PROC-ARGV-RESET
   s" tools/check.f" >LEN PROC-ARGV+
   BAD$ >LEN PROC-ARGV+
   HB$ >LEN CAP-OUT BUF-CAP >LEN
   CAP-ERR BUF-CAP >LEN $2710 >MS RUN-ARGV-CAPTURE
   CAPTURE>N ;

: ENUM-CLI-TEST ( -- )
   ENUM-CLI-RUN 70 T=
   {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru s" check.f: missing ;ENUM" CONTAINS? TTRUE ;

: VREC-PARTIAL-TEST ( -- )
   VREC-PARTIAL$ DIRECT-JSON-STDIN 70 T=
   {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru s" E-MISMATCH" CONTAINS? TTRUE
   CAP-ERR erru s" field<point,y,n>" CONTAINS? TTRUE ;

: NOM-SCAN-TEST ( -- )
   NOM-SCAN-BODY$ DIRECT-STDIN 0 T=
   {: outu:n erru:n :}
   outu 0 T=
   erru 0 T= ;

: TEST-NOMINAL-PREVERIFY ( -- )
   NOM-PREVERIFY$ DIRECT-STDIN 0 T=
   {: outu:n erru:n :}
   outu 0 T=
   erru 0 T= ;

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

: FAM-PRIV-TEST ( -- )
   FAM-PRIV$ DIRECT-JSON-STDIN 70 T=
   {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru s" E-UNKNOWN-SIGNATURE-TYPE" CONTAINS? TTRUE
   CAP-ERR erru s" ckfp:ckfpfam" CONTAINS? TTRUE ;

: FAM-NONAME-TEST ( -- )
   FAM-NONAME$ DIRECT-STDIN 70 T=
   {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru s" check.f: missing package name" CONTAINS? TTRUE ;

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
   INC-ENTRY$ DIRECT-PREVERIFY-PATH 0 T=
   {: outu:n erru:n :}
   outu 0 T=
   erru 0 T= ;

\ `--all-errors --source-list a b` runs all-errors per ORIGINAL file with
\ prior entries replayed as support: both bad defs in b report against b's
\ path (the pre-redrive materialized temp had zero defs, so all-errors was a
\ no-op and only preverify's first error surfaced).
: LIST-ALL-TEST ( -- )
   SUP$ s\" : CKT-SEVEN ( -- n ) 7 ;\n" WRITE-ALL
   USE$ s\" : CKT-GOOD-USE ( -- n ) CKT-SEVEN ;\n: CKT-BAD-ONE ( -- n n ) CKT-SEVEN ;\n: CKT-BAD-TWO ( n -- ) CKT-SEVEN ;\n" WRITE-ALL
   SUP$ USE$ DIRECT-ALL-LIST 70 T=
   {: outu:n erru:n :}
   outu 0 T=
   CAP-ERR erru USE$ CONTAINS? TTRUE
   CAP-ERR erru s" ckt-bad-one" CONTAINS? TTRUE
   CAP-ERR erru s" ckt-bad-two" CONTAINS? TTRUE
   CAP-ERR erru s" E-UNDEFINED" CONTAINS? TFALSE
   CAP-ERR erru s" ckt-good-use" CONTAINS? TFALSE
   CAP-ERR erru s" preverify" CONTAINS? TFALSE ;

\ typed-local-lint: allow-bare-local - CASE-RUN q preserves its quotation effect.
: CASE-RUN ( ptr u8 n [ -- ] -- ) {: label:ptr labelu:n q :}
   mono-ns START-NS !
   q execute
   s" PASS: " type label labelu type
   s"  (" type mono-ns START-NS @ - PROC-NS-PER-MS / . s" ms)" type cr ;

variable TA-PUB-WID

: PUBLIC-WID ( ptr u8 n -- n )
   XREF-FIND
   dup XREF-FOUND? TTRUE
   XREF-WORDLIST ;

: TA-PUBLIC? ( ptr u8 n -- bool )
   TA-PUB-WID @ XREF-FIND-WL XREF-FOUND? ;

: TA-GLOBAL? ( ptr u8 n -- bool )
   0 XREF-FIND-WL XREF-FOUND? ;

: TA-RETIRED-GLOBALS? ( -- bool )
   LINT-TRUE
   s" CHECK-MAIN" TA-GLOBAL? 0= and
   s" CHECK-RUN" TA-GLOBAL? 0= and
   s" CAPTURE-OFF" TA-GLOBAL? 0= and
   s" CHK-CAPTURE-BUFFERS!" TA-GLOBAL? 0= and
   s" CHK-CAPTURE-OFF" TA-GLOBAL? 0= and
   s" CHK-CAPTURE-OUT$" TA-GLOBAL? 0= and
   s" CHK-CAPTURE-ERR$" TA-GLOBAL? 0= and
   s" CHK-RUN-AS" TA-GLOBAL? 0= and
   s" CHK-BUILD-PREFIX" TA-GLOBAL? 0= and
   s" CHK-RUN-ACT" TA-GLOBAL? 0= and ;

;package

s" : CHK-CAPTURE-BUFFERS! ( -- ) ;" evaluate

package CHECK-TEST
private

TA-RETIRED-GLOBALS? 0= constant TA-GLOBAL-MUTATION-REJECTED

;package

undefine CHK-CAPTURE-BUFFERS!

package CHECK-TEST
private

: WID-WORD-N ( n -- n ) {: wid:n :}
   0 0
   begin over ndict@ < while
      over XREF-REC XREF-WORDLIST wid = if 1+ then
      swap 1+ swap
   repeat
   nip ;

\ XREF reads live wordlists, so comments, strings, and duplicate source text
\ cannot satisfy the package or retired-global inventories.
: TEST-CHECK-PUBLIC ( -- )
   s" CHECK:RESET" PUBLIC-WID TA-PUB-WID !
   TA-PUB-WID @ WID-WORD-N 6 T=
   s" RESET" TA-PUBLIC? TTRUE
   s" OPT" TA-PUBLIC? TTRUE
   s" SOURCE" TA-PUBLIC? TTRUE
   s" FILE" TA-PUBLIC? TTRUE
   s" RUN" TA-PUBLIC? TTRUE
   s" MAIN" TA-PUBLIC? TTRUE
   s" CAPTURE-OFF" TA-PUBLIC? TFALSE
   s" CHECK-RUN" TA-PUBLIC? TFALSE
   s" CHK-CAPTURE-BUFFERS!" TA-PUBLIC? TFALSE
   s" CHK-RUN-AS" TA-PUBLIC? TFALSE
   s" CHK-BUILD-PREFIX" TA-PUBLIC? TFALSE
   s" CHK-RUN-ACT" TA-PUBLIC? TFALSE ;

: TEST-CHECK-TEST-PUBLIC ( -- )
   s" CHECK-TEST:TEST" PUBLIC-WID WID-WORD-N 1 T= ;

: TEST-RETIRED-GLOBALS ( -- )
   TA-RETIRED-GLOBALS? TTRUE ;

: TEST-GLOBAL-MUTATION ( -- )
   TA-GLOBAL-MUTATION-REJECTED TTRUE
   TA-RETIRED-GLOBALS? TTRUE ;

: TEST-MAIN ( -- )
   T-RESET
   PREPARE
   s" check/public-api" [: TEST-CHECK-PUBLIC ;] CASE-RUN
   s" check/test-public-api" [: TEST-CHECK-TEST-PUBLIC ;] CASE-RUN
   s" check/retired-globals" [: TEST-RETIRED-GLOBALS ;] CASE-RUN
   s" check/global-mutation" [: TEST-GLOBAL-MUTATION ;] CASE-RUN
   s" check/good" [: TEST-GOOD ;] CASE-RUN
   s" check/prelude-hook-public" [: TEST-PRELUDE-HOOK ;] CASE-RUN
   s" check/layout-buffer" [: TEST-LAYOUT-BUFFER ;] CASE-RUN
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
   s" check/source-list-preverify-diag" [: PREVERIFY-DIAG-TEST ;] CASE-RUN
   s" check/value-record-good" [: VREC-GOOD-TEST ;] CASE-RUN
   s" check/linear-bad" [: TEST-LINEAR-BAD ;] CASE-RUN
   s" check/value-record-bad" [: VREC-BAD-TEST ;] CASE-RUN
   s" check/value-record-partial" [: VREC-PARTIAL-TEST ;] CASE-RUN
   s" check/typefamily-good" [: TEST-TYPEFAMILY-GOOD ;] CASE-RUN
   s" check/typefamily-all-errors" [: TEST-TYPEFAMILY-ALL ;] CASE-RUN
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
   s" check/list-capacity" [: TEST-LIST-CAPACITY ;] CASE-RUN
   s" check/empty-list" [: TEST-EMPTY-LIST ;] CASE-RUN
   s" check/missing-file" [: TEST-MISSING-FILE ;] CASE-RUN
   s" check/missing-engine" [: TEST-MISSING-ENGINE ;] CASE-RUN
   s" check/cleanup-failure" [: TEST-CLEANUP-FAILURE ;] CASE-RUN
   s" check/repeat-source-ok" [: TEST-REPEAT-SOURCE-OK ;] CASE-RUN
   s" check/repeat-source-fail" [: TEST-REPEAT-SOURCE-FAIL ;] CASE-RUN
   s" check/repeat-file" [: TEST-REPEAT-FILE ;] CASE-RUN
   s" check/repeat-list" [: TEST-REPEAT-LIST ;] CASE-RUN
   s" check/oversize" [: TEST-OVERSIZE ;] CASE-RUN
   s" check/enum-good" [: TEST-ENUM-GOOD ;] CASE-RUN
   s" check/enum-bad" [: TEST-ENUM-BAD ;] CASE-RUN
   s" check/product-good" [: TEST-PRODUCT-GOOD ;] CASE-RUN
   s" check/product-bad" [: TEST-PRODUCT-BAD ;] CASE-RUN
   s" check/product-all-errors" [: PROD-ALL-TEST ;] CASE-RUN
   s" check/enum-noend" [: TEST-ENUM-NOEND ;] CASE-RUN
   s" check/product-noend" [: TEST-PROD-NOEND ;] CASE-RUN
   s" check/value-record-noend" [: TEST-VREC-NOEND ;] CASE-RUN
   s" check/enum-noend-cli" [: ENUM-CLI-TEST ;] CASE-RUN
   s" check/nominal-scan-top-level" [: NOM-SCAN-TEST ;] CASE-RUN
   s" check/nominal-preverify" [: TEST-NOMINAL-PREVERIFY ;] CASE-RUN
   s" check/package-linear-good" [: LINEAR-GOOD-TEST ;] CASE-RUN
   s" check/package-linear-cross" [: LINEAR-CROSS-TEST ;] CASE-RUN
   s" check/package-linear-global" [: LINEAR-GLOBAL-TEST ;] CASE-RUN
   s" check/package-linear-distinct" [: LINEAR-DISTINCT-TEST ;] CASE-RUN
   s" check/package-family-good" [: FAM-GOOD-TEST ;] CASE-RUN
   s" check/package-family-two" [: FAM-TWO-TEST ;] CASE-RUN
   s" check/package-family-bogus" [: FAM-BOGUS-TEST ;] CASE-RUN
   s" check/package-family-json-pin" [: FAM-JSON-PIN ;] CASE-RUN
   s" check/package-family-private" [: FAM-PRIV-TEST ;] CASE-RUN
   s" check/package-missing-name" [: FAM-NONAME-TEST ;] CASE-RUN
   s" check/require-facade" [: TEST-REQUIRE-FACADE ;] CASE-RUN
   s" check/included-dep" [: TEST-INCLUDED-DEP ;] CASE-RUN
   s" check/source-list-all-errors" [: LIST-ALL-TEST ;] CASE-RUN
   CLEANUP-RUN
   T-REPORT
   s" check-test: ok" type cr ;

public

: TEST ( -- )
   TEST-MAIN ;

;package
