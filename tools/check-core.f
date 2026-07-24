\ check-core.f - reusable Habu-native checked engine core.
\ Load after lib/date.f, lib/errors.f lib/string.f lib/memory.f lib/vector.f lib/fs.f
\ lib/fs-mutate.f lib/process.f lib/process-argv.f lib/source.f,
\ tools/lint/text.f, tools/lint/token.f, tools/lint/lib.f,
\ tools/lint/json-writer.f, tools/lint/source-lex.f,
\ tools/diag-origin-core.f, tools/json.f, tools/json-only-core.f,
\ tools/signature-lint-core.f, tools/checked-boundary-lint-core.f,
\ tools/reserved-name-lint-core.f, tools/trust-lint-core.f,
\ tools/check-all-errors-core.f (which loads verify-source.f), and tools/argv.f.
\ The dependency-closure producer (whole-file ordered loader events) and its
\ dynamic-tail manifest are required below.

require tools/dynamic-tail-manifest.f
require tools/source-discovery.f

\ This tool installs its checker hook before validating generated source
\ snippets with CHECK!. CHECK! (engine checker entrypoint) is modeled as a
\ primitive axiom so the hook definition itself compiles checked
\ (axiom owner: habu-primitive-effect-axiom-1119f176).
s" CHECK!" s" ptr u8 n -- n" TRUST
s" TYPE-RESERVED?" s" ptr u8 n -- bool" TRUST
s" CHECKER-DEFLINEAR" s" ptr u8 n --" TRUST
s" CHECKER-DEFRECORD" s" ptr u8 n ptr u8 n --" TRUST
s" CHECKER-SCOPE-START" s" --" TRUST
s" CHECKER-SCOPE-DONE" s" --" TRUST

package CHECK

: CHK-CHECK-HOOK ( ptr u8 n -- n )
   CHECK! dup -1 <> if 70 throw then ;
LOWER-CERT-HOOK:INSTALL
' CHK-CHECK-HOOK set-check

$100000 constant CHK-SRC-CAP
$120000 constant CHK-RUN-CAP
$100000 constant CHK-ORIGIN-CAP
$8000 constant CHK-OUT-CAP
$20000 constant CHK-ERR-CAP
32 constant CHK-NUM-CAP
128 constant CHK-MAX-POS
128 constant CHK-DEP-MAX
120000 constant CHK-TIMEOUT-MS

0 constant CHK-SEL-NONE
1 constant CHK-SEL-SOURCE
2 constant CHK-SEL-FILE
3 constant CHK-SEL-LIST

10 constant CHK-LF
13 constant CHK-CR
32 constant CHK-SP
34 constant CHK-DQ
45 constant CHK-DASH
64 constant CHK-E-USAGE
66 constant CHK-E-NOINPUT
69 constant CHK-E-UNAVAILABLE
70 constant CHK-E-CHECK

create CHK-NUM-BUF CHK-NUM-CAP allot
create CHK-ROOT-BUF FS-PATH-CAP allot
create CHK-SRC-PATH-BUF FS-PATH-CAP allot
create CHK-RUN-PATH-BUF FS-PATH-CAP allot
create CHK-SEL-LABEL-BUF FS-PATH-CAP allot
create CHK-POS-BUF CHK-MAX-POS FS-PATH-CAP * allot
create CHK-POS-U CHK-MAX-POS cells allot
create CHK-DEP-PATHS CHK-DEP-MAX FS-PATH-CAP * allot
create CHK-DEP-US CHK-DEP-MAX cells allot
create CHK-DEP-STATES CHK-DEP-MAX cells allot
create CHK-DIR-IDS CHK-DEP-MAX cells allot
create CHK-DEP-ORDER CHK-DEP-MAX cells allot
create CHK-ONE 1 allot

variable CHK-SRC-BUF-A
variable CHK-RUN-BUF-A
variable CHK-ORIGIN-BUF-A
variable CHK-OUT-BUF-A
variable CHK-ERR-BUF-A
variable CHK-EXP-BUF-A
variable CHK-SEL-SRC-BUF-A

variable CHK-ARG-I
variable CHK-POS-N
variable CHK-JSON
variable CHK-STRICT
variable CHK-ALL
variable CHK-SEL-MODE
variable CHK-SEL-SRC-U
variable CHK-SEL-LABEL-U
variable CHK-SRC-U
variable CHK-PRE-U
variable CHK-RUN-U
variable CHK-ORIGIN-U
variable CHK-OUT-U
variable CHK-ERR-U
variable CHK-RC
variable CHK-CHILD-RC
variable CHK-NUM-I
variable CHK-LABEL-A
variable CHK-LABEL-U
variable CHK-SRC-A
variable CHK-HB-A
variable CHK-HB-U
variable CHK-SRC-PATH-U
variable CHK-RUN-PATH-U
variable CHK-ROOT-U
variable CHK-NOM-I
variable CHK-NOM-U
variable CHK-EXP-U
variable CHK-EXP-OUT-U
variable CHK-DEP-N
variable CHK-DIR-N
variable CHK-DEP-ORDER-N
variable CHK-DISC-ID
variable CHK-ALL-ID
variable CHK-ALL-RC
variable CHK-VREC-DEF-I
variable CHK-VREC-NAME-I
variable CHK-TFAM-NAME-I

: CHK-PTR-U8-FIELD ( ptr a -- ptr ptr u8 )
   0 ptr-field ;

: CHK-PTR-U8@ ( ptr a -- ptr u8 )
   CHK-PTR-U8-FIELD @ ;

: CHK-PTR-U8! ( ptr u8 ptr a -- )
   CHK-PTR-U8-FIELD ! ;

: CHK-PTR-U8-SLOT ( n ptr a -- ptr ptr u8 )
   swap cells + CHK-PTR-U8-FIELD ;

: CHK-PTR-U8-SLOT@ ( n ptr a -- ptr u8 )
   CHK-PTR-U8-SLOT @ ;

: CHK-PTR-U8-SLOT! ( ptr u8 n ptr a -- )
   CHK-PTR-U8-SLOT ! ;

: CHK-HB-A-FIELD ( -- ptr ptr u8 )
   CHK-HB-A 0 ptr-field ;

: CHK-HB-A@ ( -- ptr u8 )
   CHK-HB-A-FIELD @ ;

: CHK-HB-A! ( ptr u8 -- )
   CHK-HB-A-FIELD ! ;

: CHK-HB! ( ptr u8 n -- ) {: a:ptr u:n :}
   a CHK-HB-A!
   u CHK-HB-U ! ;

: CHK-HB$ ( -- ptr u8 n )
   CHK-HB-A@ CHK-HB-U @ ;

: CHK-ALLOC-BUF ( n -- ptr u8 )
   MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop ;

: CHK-SRC-BUF ( -- ptr u8 )
   CHK-SRC-BUF-A @ 0= if CHK-SRC-CAP CHK-ALLOC-BUF CHK-SRC-BUF-A ! then
   CHK-SRC-BUF-A @ ;

: CHK-RUN-BUF ( -- ptr u8 )
   CHK-RUN-BUF-A @ 0= if CHK-RUN-CAP CHK-ALLOC-BUF CHK-RUN-BUF-A ! then
   CHK-RUN-BUF-A @ ;

: CHK-ORIGIN-BUF ( -- ptr u8 )
   CHK-ORIGIN-BUF-A @ 0= if CHK-ORIGIN-CAP CHK-ALLOC-BUF CHK-ORIGIN-BUF-A ! then
   CHK-ORIGIN-BUF-A @ ;

: CHK-OUT-BUF ( -- ptr u8 )
   CHK-OUT-BUF-A @ 0= if CHK-OUT-CAP CHK-ALLOC-BUF CHK-OUT-BUF-A ! then
   CHK-OUT-BUF-A @ ;

: CHK-ERR-BUF ( -- ptr u8 )
   CHK-ERR-BUF-A @ 0= if CHK-ERR-CAP CHK-ALLOC-BUF CHK-ERR-BUF-A ! then
   CHK-ERR-BUF-A @ ;

: CHK-EXP-BUF ( -- ptr u8 )
   CHK-EXP-BUF-A @ 0= if CHK-SRC-CAP CHK-ALLOC-BUF CHK-EXP-BUF-A ! then
   CHK-EXP-BUF-A @ ;

: CHK-SEL-SRC-BUF ( -- ptr u8 )
   CHK-SEL-SRC-BUF-A @ 0= if
      CHK-SRC-CAP CHK-ALLOC-BUF CHK-SEL-SRC-BUF-A !
   then
   CHK-SEL-SRC-BUF-A @ ;

: CHK-WRITE ( n ptr u8 n -- ) {: fd:n a:ptr u:n :}
   u 0= if exit then
   fd a u write u <> if E-FS-IO throw then ;

: CHK-OUT ( ptr u8 n -- )
   1 -rot CHK-WRITE ;

: CHK-ERR ( ptr u8 n -- )
   2 -rot CHK-WRITE ;

: CHK-C! ( n -- )
   CHK-ONE c! ;

: CHK-ERR-C ( n -- )
   CHK-C!
   2 CHK-ONE 1 CHK-WRITE ;

: CHK-ERR-LN ( ptr u8 n -- )
   CHK-ERR
   CHK-LF CHK-ERR-C ;

: CHK-USAGE ( -- )
   s" usage: tools/check.f [--json-errors] [--strict-signatures] [--all-errors] [--source-list file ... | prog.f]" CHK-ERR-LN
   CHK-E-USAGE throw ;

: CHK-THROW ( n -- )
   throw ;

: CHK-FAIL ( ptr u8 n n -- ) {: msg:ptr u:n code:n :}
   msg u CHK-ERR-LN
   code CHK-THROW ;

: CHK-ARG$ ( n -- ptr u8 n )
   SCRIPT-ARGV$ ;

: CHK-ARG= ( n ptr u8 n -- bool ) {: idx:n a:ptr u:n :}
   idx CHK-ARG$ a u LINT-STR= ;

: CHK-DASH? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   u 0 > if a c@ CHK-DASH = else 0 0= 0= then ;

: CHK-POS-SLOT ( n -- ptr u8 )
   FS-PATH-CAP * CHK-POS-BUF + ;

: CHK-POS-U-SLOT ( n -- ptr n )
   cells CHK-POS-U + ;

: CHK-POS$ ( n -- ptr u8 n ) {: idx:n :}
   idx 0 < if CHK-USAGE then
   idx CHK-POS-N @ >= if CHK-USAGE then
   idx CHK-POS-SLOT
   idx CHK-POS-U-SLOT @ ;

: CHK-ADD-POS ( ptr u8 n -- ) {: a:ptr u:n :}
   u FS-PATH-CAP > if E-FS-CAPACITY throw then
   CHK-POS-N @ CHK-MAX-POS >= if CHK-USAGE then
   a CHK-POS-N @ CHK-POS-SLOT u BYTE-COPY
   u CHK-POS-N @ CHK-POS-U-SLOT !
   CHK-POS-N @ 1+ CHK-POS-N ! ;

: CHK-LIST-OPT ( -- )
   CHK-SEL-MODE @
   case
      CHK-SEL-NONE of CHK-SEL-LIST CHK-SEL-MODE ! endof
      CHK-SEL-FILE of CHK-SEL-LIST CHK-SEL-MODE ! endof
      CHK-SEL-LIST of endof
      CHK-USAGE
   endcase ;

: CHK-OPT ( ptr u8 n -- ) {: a:ptr u:n :}
   a u s" json-errors" LINT-STR= if LINT-TRUE CHK-JSON ! exit then
   a u s" strict-signatures" LINT-STR= if LINT-TRUE CHK-STRICT ! exit then
   a u s" all-errors" LINT-STR= if LINT-TRUE CHK-ALL ! exit then
   a u s" source-list" LINT-STR= if CHK-LIST-OPT exit then
   CHK-USAGE ;

public

: OPT ( ptr u8 n -- )
   CHK-OPT ;

: FILE ( ptr u8 n -- ) {: path:ptr pathu:n :}
   CHK-SEL-MODE @
   case
      CHK-SEL-NONE of
         path pathu CHK-ADD-POS
         CHK-SEL-FILE CHK-SEL-MODE !
      endof
      CHK-SEL-LIST of path pathu CHK-ADD-POS endof
      CHK-USAGE
   endcase ;

private

: CHK-PARSE-ONE ( ptr u8 n -- ) {: a:ptr u:n :}
   a u s" --json-errors" LINT-STR= if s" json-errors" OPT exit then
   a u s" --strict-signatures" LINT-STR= if s" strict-signatures" OPT exit then
   a u s" --all-errors" LINT-STR= if s" all-errors" OPT exit then
   a u s" --source-list" LINT-STR= if s" source-list" OPT exit then
   a u CHK-DASH? if CHK-USAGE then
   a u FILE ;

: CHK-COLLECT-REST ( -- )
   begin CHK-ARG-I @ SCRIPT-ARGC < while
      CHK-ARG-I @ CHK-ARG$ FILE
      CHK-ARG-I @ 1+ CHK-ARG-I !
   repeat ;

: CHK-PARSE ( -- )
   0 CHK-ARG-I !
   begin CHK-ARG-I @ SCRIPT-ARGC < while
      CHK-ARG-I @ s" --" CHK-ARG= if
         CHK-ARG-I @ 1+ CHK-ARG-I !
         CHK-COLLECT-REST
         exit
      then
      CHK-ARG-I @ CHK-ARG$ CHK-PARSE-ONE
      CHK-ARG-I @ 1+ CHK-ARG-I !
   repeat ;

: CHK-POS-LENS-CLEAR ( -- )
   0 begin dup CHK-MAX-POS < while
      0 over CHK-POS-U-SLOT !
      1+
   repeat drop ;

: CHK-SELECT-CLEAR ( -- )
   CHK-SEL-NONE CHK-SEL-MODE !
   0 CHK-SEL-SRC-U !
   0 CHK-SEL-LABEL-U !
   0 CHK-POS-N !
   CHK-POS-LENS-CLEAR ;

: CHK-RUN-TEMP-CLEAR ( -- )
   0 CHK-SRC-U !
   0 CHK-PRE-U !
   0 CHK-RUN-U !
   0 CHK-ORIGIN-U !
   0 CHK-OUT-U !
   0 CHK-ERR-U !
   0 CHK-RC !
   0 CHK-CHILD-RC !
   0 CHK-NUM-I !
   0 CHK-LABEL-U !
   NULL$ drop CHK-LABEL-A CHK-PTR-U8!
   NULL$ drop CHK-SRC-A CHK-PTR-U8!
   0 CHK-HB-U !
   NULL$ drop CHK-HB-A!
   0 CHK-NOM-I !
   0 CHK-NOM-U !
   0 CHK-VREC-DEF-I !
   0 CHK-VREC-NAME-I !
   0 CHK-TFAM-NAME-I !
   0 CHK-EXP-U !
   0 CHK-EXP-OUT-U !
   0 CHK-DEP-N !
   0 CHK-DIR-N !
   0 CHK-DEP-ORDER-N !
   0 CHK-DISC-ID !
   0 CHK-ALL-ID !
   0 CHK-ALL-RC ! ;

: CHK-RESET-CFG ( -- )
   0 CHK-ARG-I !
   0 CHK-JSON !
   0 CHK-STRICT !
   0 CHK-ALL !
   CHK-SELECT-CLEAR
   CHK-RUN-TEMP-CLEAR ;

: CHK-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u:n dst:ptr lenp:ptr :}
   u FS-PATH-CAP > if E-FS-CAPACITY throw then
   a dst u BYTE-COPY
   u lenp ! ;

: CHK-ROOT ( -- ptr u8 n )
   CHK-ROOT-BUF CHK-ROOT-U @ ;

: CHK-SRC-PATH ( -- ptr u8 n )
   CHK-SRC-PATH-BUF CHK-SRC-PATH-U @ ;

: CHK-RUN-PATH ( -- ptr u8 n )
   CHK-RUN-PATH-BUF CHK-RUN-PATH-U @ ;

: CHK-TEMP-CLEAN ( -- )
   CHK-ROOT-U @ 0= if exit then
   CHK-ROOT 2dup EXISTS? if REMOVE-TREE else 2drop then
   0 CHK-ROOT-U !
   0 CHK-SRC-PATH-U !
   0 CHK-RUN-PATH-U ! ;

: CHK-SESSION-CLEAR ( -- )
   CHK-RESET-CFG ;

: CHK-FIRST-RC ( n n -- n ) {: rc:n next-rc:n :}
   rc 0 <> if
      \ One public operation returns one code, so simultaneous propagation is
      \ impossible; the earliest primary code has precedence.
      rc exit
   then
   next-rc ;

public

: RESET ( -- )
   [: CHK-TEMP-CLEAN ;] catch {: rc:n :}
   CHK-SESSION-CLEAR
   [: CHECKED-BOUNDARY-LINT:RESET ;] catch {: provider-rc:n :}
   rc provider-rc CHK-FIRST-RC
   dup 0 <> if throw then drop ;

private

: CHK-MAKE-TEMP ( -- )
   s" habu-check" TMPDIR-MKDIR CHK-ROOT-BUF CHK-ROOT-U CHK-COPY!
   CHK-ROOT s" source.f" CHK-SRC-PATH-BUF JOIN-PATH CHK-SRC-PATH-U !
   CHK-ROOT s" run.f" CHK-RUN-PATH-BUF JOIN-PATH CHK-RUN-PATH-U ! ;

: CHK-LABEL-STDIN ( -- )
   s" <stdin>" CHK-LABEL-U ! CHK-LABEL-A CHK-PTR-U8! ;

: CHK-LABEL-FILE ( -- )
   CHK-SRC-A CHK-PTR-U8@ CHK-LABEL-A CHK-PTR-U8!
   CHK-SRC-U @ CHK-LABEL-U ! ;

: CHK-LABEL ( -- ptr u8 n )
   CHK-LABEL-A CHK-PTR-U8@ CHK-LABEL-U @ ;

: CHK-SOURCE ( -- ptr u8 n )
   CHK-SRC-A CHK-PTR-U8@ CHK-SRC-U @ ;

: CHK-LABEL! ( ptr u8 n -- ) {: a:ptr u:n :}
   u CHK-LABEL-U !
   a CHK-LABEL-A CHK-PTR-U8! ;

: CHK-SOURCE! ( ptr u8 n -- ) {: a:ptr u:n :}
   u CHK-SRC-U !
   a CHK-SRC-A CHK-PTR-U8! ;

: CHK-SINGLE-FILE? ( -- bool )
   CHK-SEL-MODE @ CHK-SEL-FILE = ;

: CHK-LINT-SOURCE ( -- ptr u8 n )
   CHK-SINGLE-FILE? if 0 CHK-POS$ exit then
   CHK-SOURCE ;

: CHK-LINT-LABEL ( -- ptr u8 n )
   CHK-SINGLE-FILE? if 0 CHK-POS$ exit then
   CHK-LABEL ;

: CHK-DEP-CHECK ( n -- ) {: id:n :}
   id 0 < if E-TBL-BOUNDS throw then
   id CHK-DEP-MAX >= if E-TBL-BOUNDS throw then ;

: CHK-DEP-PATH ( n -- ptr u8 ) {: id:n :}
   id CHK-DEP-CHECK
   CHK-DEP-PATHS id FS-PATH-CAP * + ;

: CHK-DEP-U ( n -- ptr n ) {: id:n :}
   id CHK-DEP-CHECK
   CHK-DEP-US id cells + ;

: CHK-DEP-STATE ( n -- ptr n ) {: id:n :}
   id CHK-DEP-CHECK
   CHK-DEP-STATES id cells + ;

: CHK-DEP$ ( n -- ptr u8 n ) {: id:n :}
   id CHK-DEP-PATH
   id CHK-DEP-U @ ;

: CHK-DEP-MATCH? ( ptr u8 n n -- bool ) {: a:ptr u:n id:n :}
   a u id CHK-DEP$ LINT-STR= ;

: CHK-DEP-FIND ( ptr u8 n -- n ) {: a:ptr u:n :}
   0 begin dup CHK-DEP-N @ < while
      dup a u rot CHK-DEP-MATCH? if exit then
      1+
   repeat drop -1 ;

: CHK-DEP-NEW ( ptr u8 n -- n ) {: a:ptr u:n :}
   u FS-PATH-CAP > if E-FS-CAPACITY throw then
   CHK-DEP-N @ CHK-DEP-MAX >= if E-TBL-BOUNDS throw then
   CHK-DEP-N @ {: id:n :}
   a id CHK-DEP-PATH u BYTE-COPY
   u id CHK-DEP-U !
   0 id CHK-DEP-STATE !
   id 1+ CHK-DEP-N !
   id ;

: CHK-DEP-ID ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u CHK-DEP-FIND dup 0 >= if exit then
   drop a u CHK-DEP-NEW ;

: CHK-DIR-PUSH ( n -- ) {: id:n :}
   CHK-DIR-N @ CHK-DEP-MAX >= if E-TBL-BOUNDS throw then
   id CHK-DIR-IDS CHK-DIR-N @ cells + !
   CHK-DIR-N @ 1+ CHK-DIR-N ! ;

: CHK-DEP-ORDER-PUSH ( n -- ) {: id:n :}
   CHK-DEP-ORDER-N @ CHK-DEP-MAX >= if E-TBL-BOUNDS throw then
   id CHK-DEP-ORDER CHK-DEP-ORDER-N @ cells + !
   CHK-DEP-ORDER-N @ 1+ CHK-DEP-ORDER-N ! ;

: CHK-DEP-DIRECT+ ( ptr u8 n -- )
   CHK-DEP-ID CHK-DIR-PUSH ;

\ Dependency closure: the shared whole-file ordered-event producer
\ (tools/source-discovery.f) scans every token of a file - colon bodies
\ included - and records one event per literal loader form
\ (include/included/require/required/provided). Every event path is a direct
\ dep, so the closure is a superset of the runtime load set; dynamic or
\ retired loader forms reject fail-closed unless manifested.

: CHK-DISC-RC? ( n -- bool ) {: rc:n :}
   rc E-DISC-FIRST <= rc E-DISC-LAST >= and ;

: CHK-DISC-MSG$ ( n -- ptr u8 n ) {: rc:n :}
   rc E-DISC-SHADOW = if s" check.f: discovery rejected: loader word shadowed or undefined" exit then
   rc E-DISC-DYNAMIC = if s" check.f: discovery rejected: dynamic (non-literal) loader path" exit then
   rc E-DISC-OPENER = if s" check.f: discovery rejected: unsupported string opener before a loader word" exit then
   rc E-DISC-RETIRE = if s" check.f: discovery rejected: loader word retired (UNDEFINE-IF-DEFINED)" exit then
   rc E-DISC-UNTERM = if s" check.f: discovery rejected: unterminated string" exit then
   s" check.f: discovery rejected: capacity exceeded" ;

: CHK-DISC-FAIL ( n -- )
   CHK-DISC-MSG$ CHK-E-CHECK CHK-FAIL ;

: CHK-DISCOVER-ACT ( -- )
   CHK-DISC-ID @ CHK-DEP$ DISCOVER:RUN ;

: CHK-EXPAND-SCAN ( n -- ) {: id:n :}
   id CHK-DISC-ID !
   [: CHK-DISCOVER-ACT ;] catch {: rc:n :}
   rc 0= if exit then
   rc CHK-DISC-RC? if rc CHK-DISC-FAIL then
   rc throw ;

: CHK-EVENT-DEP+ ( n -- ) {: ix:n :}
   ix EVENT-PATH@ CHK-DEP-DIRECT+ ;

: CHK-EVENTS>DEPS ( -- )
   0 begin dup EVENT-COUNT < while
      dup CHK-EVENT-DEP+
      1+
   repeat drop ;

: CHK-EXPAND-ID ( n -- ) {: id:n :}
   id CHK-DEP-CHECK
   id CHK-DEP-STATE @ 2 = if exit then
   id CHK-DEP-STATE @ 1 = if exit then
   1 id CHK-DEP-STATE !
   CHK-DIR-N @
   id CHK-DEP$ FILE? 0= if s" check.f: no such source" CHK-E-NOINPUT CHK-FAIL then
   id CHK-EXPAND-SCAN
   CHK-EVENTS>DEPS
   dup CHK-DIR-N @
   begin 2dup < while
      over cells CHK-DIR-IDS + @ RECURSE
      swap 1+ swap
   repeat
   2drop CHK-DIR-N !
   id CHK-DEP-ORDER-PUSH
   2 id CHK-DEP-STATE ! ;

: CHK-EXPAND-PATH ( ptr u8 n -- )
   CHK-DEP-ID CHK-EXPAND-ID ;

: CHK-EXPAND-RESET ( -- )
   0 CHK-EXP-OUT-U !
   0 CHK-DEP-N !
   0 CHK-DIR-N !
   0 CHK-DEP-ORDER-N ! ;

: CHK-WRITE-EXPANDED-SOURCE ( -- )
   CHK-SRC-PATH CHK-SRC-BUF CHK-EXP-OUT-U @ LEN>N WRITE-ALL
   CHK-SRC-PATH CHK-SRC-U ! CHK-SRC-A CHK-PTR-U8! ;

: CHK-EXP-APP ( ptr u8 n -- )
   >LEN CHK-SRC-BUF CHK-SRC-CAP >LEN CHK-EXP-OUT-U SOURCE-APPEND-BYTES ;

: CHK-EXP-C ( n -- )
   CHK-SRC-BUF CHK-SRC-CAP >LEN CHK-EXP-OUT-U SOURCE-APPEND-C ;

: CHK-APPEND-REQUIRED ( ptr u8 n -- ) {: path:ptr pathu:n :}
   path pathu >LEN CHK-SRC-BUF CHK-SRC-CAP >LEN CHK-EXP-OUT-U SOURCE-APPEND-QPATH
   CHK-SP CHK-EXP-C
   s" required" CHK-EXP-APP
   CHK-LF CHK-EXP-C ;

: CHK-MATERIALIZE-STDIN ( -- )
   CHK-LABEL-STDIN
   CHK-SRC-BUF CHK-SRC-CAP >LEN READ-STDIN-ALL LEN>N CHK-SRC-U !
   CHK-SRC-PATH CHK-SRC-BUF CHK-SRC-U @ WRITE-ALL
   CHK-SRC-PATH CHK-SRC-U ! CHK-SRC-A CHK-PTR-U8! ;

: CHK-MATERIALIZE-FILE ( -- )
   0 CHK-POS$ CHK-LABEL!
   CHK-LABEL FILE? 0= if s" check.f: no such source" CHK-E-NOINPUT CHK-FAIL then
   CHK-EXPAND-RESET
   CHK-LABEL CHK-EXPAND-PATH
   CHK-LABEL CHK-SOURCE! ;

: CHK-MATERIALIZE-SOURCE ( -- )
   CHK-SEL-SRC-BUF CHK-SRC-BUF CHK-SEL-SRC-U @ BYTE-COPY
   CHK-SEL-SRC-U @ CHK-SRC-U !
   CHK-SEL-LABEL-BUF CHK-SEL-LABEL-U @ CHK-LABEL!
   CHK-SRC-PATH CHK-SRC-BUF CHK-SRC-U @ WRITE-ALL
   CHK-SRC-PATH CHK-SOURCE! ;

: CHK-MATERIALIZE-LIST ( -- )
   CHK-POS-N @ 0= if CHK-USAGE then
   s" <source-list>" CHK-LABEL!
   CHK-EXPAND-RESET
   0 begin dup CHK-POS-N @ < while
      dup CHK-POS$ CHK-EXPAND-PATH
      1+
   repeat drop
   0 CHK-EXP-OUT-U !
   0 begin dup CHK-POS-N @ < while
      dup CHK-POS$ CHK-APPEND-REQUIRED
      1+
   repeat drop
   CHK-WRITE-EXPANDED-SOURCE ;

: CHK-SOURCE-TOO-BIG ( -- )
   s" check.f: source exceeds capacity" CHK-E-NOINPUT CHK-FAIL ;

public

: SOURCE ( ptr u8 n ptr u8 n -- )
   {: src:ptr srcu:n label:ptr labelu:n :}
   CHK-SEL-MODE @ CHK-SEL-NONE <> if CHK-USAGE then
   srcu CHK-SRC-CAP > if CHK-SOURCE-TOO-BIG then
   labelu FS-PATH-CAP > if E-FS-CAPACITY throw then
   src CHK-SEL-SRC-BUF srcu BYTE-COPY
   label CHK-SEL-LABEL-BUF labelu BYTE-COPY
   srcu CHK-SEL-SRC-U !
   labelu CHK-SEL-LABEL-U !
   CHK-SEL-SOURCE CHK-SEL-MODE ! ;

private

: CHK-MATERIALIZE-DISPATCH ( -- )
   CHK-SEL-MODE @
   case
      CHK-SEL-NONE of CHK-MATERIALIZE-STDIN endof
      CHK-SEL-SOURCE of CHK-MATERIALIZE-SOURCE endof
      CHK-SEL-FILE of CHK-MATERIALIZE-FILE endof
      CHK-SEL-LIST of CHK-MATERIALIZE-LIST endof
      E-TBL-BOUNDS throw
   endcase ;

\ A source (or its facade expansion) over CHK-SRC-CAP fails closed with the
\ clean NOINPUT diagnostic instead of an uncaught E-FS-CAPACITY from the read
\ layer (dot habu-tfam-13-c2-checkcore-cap).
: CHK-MATERIALIZE ( -- )
   CHK-HB$ FILE? 0= if s" check.f: bin/hb missing" CHK-E-UNAVAILABLE CHK-FAIL then
   CHK-MAKE-TEMP
   [: CHK-MATERIALIZE-DISPATCH ;] catch {: rc:n :}
   rc 0= if exit then
   rc E-FS-CAPACITY = if CHK-SOURCE-TOO-BIG then
   rc throw ;

: CHK-WORD-TOK? ( n -- bool ) {: k:n :}
   k L# @ >= IF LINT-FALSE exit THEN
   k LK@ L-WORD = ;

: CHK-TOK=CI ( n ptr u8 n -- bool ) {: k:n a:ptr u:n :}
   k CHK-WORD-TOK? 0= IF LINT-FALSE exit THEN
   k LEX-TOK a u LINT-STR=CI ;

: CHK-TOK-END ( n -- n ) {: k:n :}
   k LB@ k LEX-TOK nip + ;

: CHK-NOM-SRC$ ( n n -- ptr u8 n ) {: def:n name:n :}
   CHK-SRC-BUF def LB@ +
   name CHK-TOK-END def LB@ - ;

: CHK-NOM-BAD-SUG$ ( -- ptr u8 n )
   s" Choose a unique non-reserved nominal type name." ;

: CHK-NOM-JSTR ( ptr u8 n ptr u8 n -- ) {: key:ptr keyu:n val:ptr valu:n :}
   key keyu LJW-KEY val valu LJW-STRING LJW-COMMA ;

: CHK-NOM-JU ( n ptr u8 n -- )
   LJW-KEY LJW-U LJW-COMMA ;

: CHK-TYPE-JSON ( n n ptr u8 n -- ) {: def:n name:n word:ptr wordu:n :}
   LJW-RESET
   LJW-OBJECT-START
   1 s" schema_version" CHK-NOM-JU
   s" code" s" E-BAD-NOMINAL-TYPE" CHK-NOM-JSTR
   s" repair_class" s" fix_nominal_type" CHK-NOM-JSTR
   s" verdict" s" rejected" CHK-NOM-JSTR
   s" word" word wordu CHK-NOM-JSTR
   s" token" LJW-KEY name LEX-TOK LJW-STRING LJW-COMMA
   name s" token_index" CHK-NOM-JU
   s" file" LJW-KEY CHK-LABEL LJW-STRING LJW-COMMA
   name LL@ s" line" CHK-NOM-JU
   name LC@ s" column" CHK-NOM-JU
   name LB@ s" byte_start" CHK-NOM-JU
   name CHK-TOK-END s" byte_end" CHK-NOM-JU
   s" definition_source" LJW-KEY def name CHK-NOM-SRC$ LJW-STRING LJW-COMMA
   s" declared_effect" s" unknown " CHK-NOM-JSTR
   s" declared_effect_source" s" unknown" CHK-NOM-JSTR
   s" inferred_effect" s" unknown " CHK-NOM-JSTR
   s" return_stack" LJW-KEY
   LJW-OBJECT-START
   s" expected" LJW-KEY s" " LJW-STRING LJW-COMMA
   s" actual" LJW-KEY s" " LJW-STRING
   LJW-OBJECT-END
   LJW-COMMA
   s" suggestion" LJW-KEY CHK-NOM-BAD-SUG$ LJW-STRING
   LJW-OBJECT-END
   LJW$ CHK-ERR
   CHK-LF CHK-ERR-C ;

: CHK-NOM-PROSE ( n -- ) {: name:n :}
   s" check.f: bad nominal type '" CHK-ERR
   name LEX-TOK CHK-ERR
   39 CHK-ERR-C
   CHK-LF CHK-ERR-C ;

: CHK-TYPE-FAIL ( n n ptr u8 n -- ) {: def:n name:n word:ptr wordu:n :}
   CHK-JSON @ IF def name word wordu CHK-TYPE-JSON ELSE name CHK-NOM-PROSE THEN
   CHK-E-CHECK CHK-THROW ;

: CHK-NOM-FAIL ( n n -- )
   s" deftype" CHK-TYPE-FAIL ;

: CHK-LIN-FAIL ( n n -- )
   s" deflinear" CHK-TYPE-FAIL ;

\ DEFTYPE NAME folds the UPPER-CASE surface name to the lowercase family tail
\ (SERIAL -> serial), so the reserved-name check and the CHECKER-DEFFAMILY mint
\ both run on the tail, matching lib/type/deftype.f. The bad-name
\ diagnostic still reports the surface token the user wrote.
128 constant CHK-NOM-TAIL-CAP
create CHK-NOM-TAIL-BUF CHK-NOM-TAIL-CAP allot

: CHK-NOM-TAIL$ ( n -- ptr u8 n ) {: name:n :}
   name LEX-TOK {: a:ptr u:n :}
   u CHK-NOM-TAIL-CAP > IF E-FS-CAPACITY throw THEN
   a u CHK-NOM-TAIL-BUF FOLD-TO
   CHK-NOM-TAIL-BUF u ;

: CHK-NOM-NAME-BAD? ( n -- bool ) {: name:n :}
   name CHK-WORD-TOK? 0= IF LINT-TRUE exit THEN
   name CHK-NOM-TAIL$ TYPE-RESERVED? ;

: CHK-NOM-REGISTER ( n n -- ) {: def:n name:n :}
   name CHK-NOM-NAME-BAD? IF def name CHK-NOM-FAIL THEN
   name CHK-NOM-TAIL$ s" 0" CHECKER-DEFFAMILY ;

: CHK-LIN-REGISTER ( n n -- ) {: def:n name:n :}
   name CHK-NOM-NAME-BAD? IF def name CHK-LIN-FAIL THEN
   name LEX-TOK CHECKER-DEFLINEAR ;

: CHK-VREC-FAIL ( n n -- )
   s" value-record" CHK-TYPE-FAIL ;

: CHK-VREC-RESET ( -- )
   0 CHK-EXP-U ! ;

: CHK-VREC-ROOM ( n -- )
   CHK-EXP-U @ + CHK-SRC-CAP > IF E-FS-CAPACITY throw THEN ;

: CHK-VREC-C ( n -- ) {: c:n :}
   1 CHK-VREC-ROOM
   c CHK-EXP-BUF CHK-EXP-U @ + c!
   CHK-EXP-U @ 1+ CHK-EXP-U ! ;

: CHK-VREC-APP ( ptr u8 n -- ) {: a:ptr u:n :}
   u CHK-VREC-ROOM
   a CHK-EXP-BUF CHK-EXP-U @ + u BYTE-COPY
   CHK-EXP-U @ u + CHK-EXP-U ! ;

: CHK-VREC-TOKEN+ ( ptr u8 n -- )
   CHK-EXP-U @ 0 > IF CHK-SP CHK-VREC-C THEN
   CHK-VREC-APP ;

: CHK-VREC-END? ( n -- bool )
   s" END-VALUE-RECORD" CHK-TOK=CI ;

: CHK-VREC-DO-DEF ( -- )
   CHK-VREC-NAME-I @ LEX-TOK
   CHK-EXP-BUF CHK-EXP-U @
   CHECKER-DEFRECORD ;

: CHK-VREC-DEFRECORD ( n n -- ) {: def:n name:n :}
   def CHK-VREC-DEF-I !
   name CHK-VREC-NAME-I !
   [: CHK-VREC-DO-DEF ;] catch dup 0= IF drop EXIT THEN
   dup CHK-E-CHECK <> IF throw THEN drop
   CHK-VREC-DEF-I @ CHK-VREC-NAME-I @ CHK-VREC-FAIL ;

: CHK-VREC-REGISTER ( n n -- n ) {: def:n name:n :}
   name CHK-NOM-NAME-BAD? IF def name CHK-VREC-FAIL THEN
   CHK-VREC-RESET
   name 1+
   begin dup L# @ < while
      dup CHK-VREC-END? if
         def name CHK-VREC-DEFRECORD
         1+ exit
      then
      dup LEX-TOK CHK-VREC-TOKEN+
      1+
   repeat
   s" check.f: missing END-VALUE-RECORD" CHK-E-CHECK CHK-FAIL ;

: CHK-TFAM-DO-DEF ( -- )         \ arity token, or empty when absent (missing-arity packet)
   CHK-TFAM-NAME-I @ LEX-TOK
   CHK-TFAM-NAME-I @ 1+ dup L# @ < IF LEX-TOK ELSE drop s" " THEN
   CHECKER-DEFFAMILY ;

: CHK-SUM-DO-DEF ( -- )
   CHK-TFAM-NAME-I @ LEX-TOK
   CHK-EXP-BUF CHK-EXP-U @
   CHECKER-DEFSUM ;

: CHK-SUM-DO-NOEND ( -- )        \ unterminated: declaration packet from name + partial body
   CHK-TFAM-NAME-I @ LEX-TOK
   CHK-EXP-BUF CHK-EXP-U @
   CHECKER-DEFSUM-NOEND ;

\ A failed TYPEFAMILY/SUMTYPE declaration already reported through the
\ checker's declaration diagnostics (TDECL-DIAG, declaration-shaped packet);
\ capture that packet into the check error stream (the preverify pattern) and
\ map the registration throw to the check rc without a second packet.
: CHK-DECL-CAPTURE ( -- )
   CHK-JSON @ DIAG-JSON!
   CHK-ERR-BUF CHK-ERR-CAP DIAG-BUFFER! ;

: CHK-DECL-FLUSH ( -- )
   DIAG-BUFFER$ CHK-ERR
   DIAG-BUFFER-OFF ;

: CHK-DECL-FAIL ( n -- )
   dup 0= IF drop EXIT THEN
   drop CHK-E-CHECK CHK-THROW ;

\ Missing arity is reported by CHECKER-DEFFAMILY through the declaration packet
\ (§24), matching native/verify-source -- not a raw pre-check throw.
: CHK-TFAM-REGISTER ( n -- n ) {: k:n :}   \ k at 'typefamily'; next scan index
   k 1+ CHK-TFAM-NAME-I !
   CHK-DECL-CAPTURE
   [: CHK-TFAM-DO-DEF ;] catch
   CHK-DECL-FLUSH
   CHK-DECL-FAIL
   k 3 + ;

\ shared block-declaration collector: name at k+1, body tokens buffered from
\ k+2 to the end token. Returns the next scan index and true, or k and false
\ when the input ends unterminated (caller reports its own terminator).
: CHK-BLOCK-COLLECT ( n ptr u8 n -- n bool ) {: k:n ea:ptr eu:n :}
   k 1+ CHK-TFAM-NAME-I !
   CHK-VREC-RESET
   k 2 +
   begin dup L# @ < while
      dup ea eu CHK-TOK=CI if 1+ LINT-TRUE exit then
      dup LEX-TOK CHK-VREC-TOKEN+
      1+
   repeat drop k LINT-FALSE ;

\ Unterminated SUMTYPE is reported by CHECKER-DEFSUM-NOEND through the declaration packet
\ (§24), matching native/verify-source -- not a raw pre-check throw.
: CHK-SUM-REGISTER ( n -- n ) {: k:n :}    \ k at 'sumtype'; next scan index
   k s" ;SUMTYPE" CHK-BLOCK-COLLECT 0= if
      drop
      CHK-DECL-CAPTURE
      [: CHK-SUM-DO-NOEND ;] catch
      CHK-DECL-FLUSH
      CHK-DECL-FAIL
      L# @ exit
   then {: nxt:n :}
   CHK-DECL-CAPTURE
   [: CHK-SUM-DO-DEF ;] catch
   CHK-DECL-FLUSH
   CHK-DECL-FAIL
   nxt ;

\ ENUM/PRODUCT block declarations (items 14/15) register through the same
\ collector so signature uses of the family later in the file resolve. The
\ enum arm also closes the item-14 gap where an enum-declaring file failed
\ the nominal pass with unknown-family rejects.
: CHK-ENUM-DO-DEF ( -- )
   CHK-TFAM-NAME-I @ LEX-TOK
   CHK-EXP-BUF CHK-EXP-U @
   CHECKER-DEFENUM ;

: CHK-PROD-DO-DEF ( -- )
   CHK-TFAM-NAME-I @ LEX-TOK
   CHK-EXP-BUF CHK-EXP-U @
   CHECKER-DEFPRODUCT ;

: CHK-ENUM-REGISTER ( n -- n ) {: k:n :}   \ k at 'enum'; next scan index
   k s" ;ENUM" CHK-BLOCK-COLLECT 0= if
      drop s" check.f: missing ;ENUM" CHK-E-CHECK CHK-FAIL
   then {: nxt:n :}
   CHK-DECL-CAPTURE
   [: CHK-ENUM-DO-DEF ;] catch
   CHK-DECL-FLUSH
   CHK-DECL-FAIL
   nxt ;

: CHK-PROD-REGISTER ( n -- n ) {: k:n :}   \ k at 'product'; next scan index
   k s" ;PRODUCT" CHK-BLOCK-COLLECT 0= if
      drop s" check.f: missing ;PRODUCT" CHK-E-CHECK CHK-FAIL
   then {: nxt:n :}
   CHK-DECL-CAPTURE
   [: CHK-PROD-DO-DEF ;] catch
   CHK-DECL-FLUSH
   CHK-DECL-FAIL
   nxt ;

\ Package blocks (dot habu-tools-check-scanner-685b735e) mirror verify-source's
\ RECORD-PACKAGE/-PUBLIC/-PRIVATE/-END-PACKAGE at the checker level, so the
\ TYPEFAMILY/SUMTYPE/ENUM/PRODUCT registrations above land in the declaring
\ package under the live visibility mode (TDECL reads CHECKER-PACKAGE-*), and
\ qualified pkg:tail signature uses resolve public-only exactly as native.
\ The CHECKER-SCOPE frame wrapping the nominal pass saves/restores package
\ state (RBF.PKGMODE/PKGU). Boundary: the name must be the plain next word
\ token — a missing or comment-shaped name rejects fail-closed here, while
\ native-loader misuse (nesting, public outside a package, ':' in a name)
\ stays fail-closed through preverify and the child run.
: CHK-PKG-REGISTER ( n -- n ) {: k:n :}   \ k at 'package'; next scan index
   k 1+ CHK-WORD-TOK? 0= if
      s" check.f: missing package name" CHK-E-CHECK CHK-FAIL
   then
   k 1+ LEX-TOK CHECKER-PACKAGE
   k 2 + ;

: CHK-PKG-STEP ( n -- n bool ) {: k:n :}   \ package-word dispatch: next index, handled
   k s" package" CHK-TOK=CI if k CHK-PKG-REGISTER LINT-TRUE exit then
   k s" public" CHK-TOK=CI if CHECKER-PUBLIC k 1+ LINT-TRUE exit then
   k s" private" CHK-TOK=CI if CHECKER-PRIVATE k 1+ LINT-TRUE exit then
   k s" ;package" CHK-TOK=CI if CHECKER-END-PACKAGE k 1+ LINT-TRUE exit then
   k LINT-FALSE ;

: CHK-TOK-SEMI? ( n -- bool )
   s" ;" CHK-TOK=CI ;

: CHK-DEF-OPENER? ( n -- bool ) {: k:n :}
   k s" :" CHK-TOK=CI IF LINT-TRUE exit THEN
   k s" TRUSTED:" CHK-TOK=CI ;

: CHK-SKIP-DEF ( n -- n )
   begin dup L# @ < while
      dup CHK-TOK-SEMI? if 1+ exit then
      1+
   repeat ;

: CHK-NOM-STEP ( n -- n ) {: k:n :}
   k CHK-DEF-OPENER? if k 1+ CHK-SKIP-DEF exit then
   k CHK-PKG-STEP if exit then drop
   k s" deftype" CHK-TOK=CI if
      k k 1+ CHK-NOM-REGISTER
      k 2 + exit
   then
   k s" deflinear" CHK-TOK=CI if
      k k 1+ CHK-LIN-REGISTER
      k 2 + exit
   then
   k s" VALUE-RECORD" CHK-TOK=CI if
      k k 1+ CHK-VREC-REGISTER exit
   then
   k s" TYPEFAMILY" CHK-TOK=CI if
      k CHK-TFAM-REGISTER exit
   then
   k s" SUMTYPE" CHK-TOK=CI if
      k CHK-SUM-REGISTER exit
   then
   k s" ENUM" CHK-TOK=CI if
      k CHK-ENUM-REGISTER exit
   then
   k s" PRODUCT" CHK-TOK=CI if
      k CHK-PROD-REGISTER exit
   then
   k 1 + ;

: CHK-RUN-NOMINAL-FILE ( ptr u8 n -- ) {: path:ptr pathu:n :}
   path pathu FILE-SIZE dup CHK-SRC-CAP > if E-FS-CAPACITY throw then drop
   path pathu CHK-SRC-BUF CHK-SRC-CAP READ-ALL CHK-NOM-U !
   CHK-SRC-BUF CHK-NOM-U @ LEX-SOURCE
   0 CHK-NOM-I !
   begin CHK-NOM-I @ L# @ < while
      CHK-NOM-I @ CHK-NOM-STEP CHK-NOM-I !
   repeat ;

: CHK-RUN-NOMINAL-AS ( ptr u8 n ptr u8 n -- ) {: label:ptr labelu:n path:ptr pathu:n :}
   label labelu CHK-LABEL!
   path pathu CHK-RUN-NOMINAL-FILE ;

: CHK-DEP-PRELOAD? ( n -- bool ) {: id:n :}
   id CHK-DEP$ REQUIRE-KNOWN? 0= ;

: CHK-RUN-NOMINAL-ID ( n -- ) {: id:n :}
   id CHK-DEP-PRELOAD? 0= if exit then
   id CHK-DEP$ 2dup CHK-RUN-NOMINAL-AS ;

: CHK-RUN-NOMINAL-ORDER ( -- )
   CHK-LABEL {: old:ptr oldu:n :}
   0 begin dup CHK-DEP-ORDER-N @ < while
      dup cells CHK-DEP-ORDER + @ CHK-RUN-NOMINAL-ID
      1+
   repeat drop
   old oldu CHK-LABEL! ;

: CHK-RUN-NOMINAL ( -- )
   CHK-DEP-ORDER-N @ 0 > if CHK-RUN-NOMINAL-ORDER exit then
   CHK-SOURCE CHK-RUN-NOMINAL-FILE ;

: CHK-LABEL-DQ? ( -- bool )
   CHK-LABEL CHK-DQ LINT-INDEX-OF MATCH option
     none OF 0 0= 0= ENDOF
     some OF drop 0 0= ENDOF
   ;MATCH ;

: CHK-CHECK-LABEL ( -- )
   CHK-LABEL-DQ? if s" check.f: source path contains a double quote, cannot set DIAG-FILE" CHK-E-USAGE CHK-FAIL then ;

: CHK-RUN-RESET ( -- )
   0 CHK-RUN-U ! ;

: CHK-RUN+ ( ptr u8 n -- ) {: a:ptr u:n :}
   CHK-RUN-U @ u + CHK-RUN-CAP > if E-FS-CAPACITY throw then
   a CHK-RUN-BUF CHK-RUN-U @ + u BYTE-COPY
   CHK-RUN-U @ u + CHK-RUN-U ! ;

: CHK-RUN-C ( n -- ) {: c:n :}
   CHK-RUN-U @ 1+ CHK-RUN-CAP > if E-FS-CAPACITY throw then
   c CHK-RUN-BUF CHK-RUN-U @ + c!
   CHK-RUN-U @ 1+ CHK-RUN-U ! ;

: CHK-RUN-LN ( ptr u8 n -- )
   CHK-RUN+
   CHK-LF CHK-RUN-C ;

: CHK-U$ ( n -- ptr u8 n ) {: u:n :}
   CHK-NUM-CAP CHK-NUM-I !
   u 0= if
      CHK-NUM-I @ 1- CHK-NUM-I !
      48 CHK-NUM-BUF CHK-NUM-I @ + c!
      CHK-NUM-BUF CHK-NUM-I @ + 1
      exit
   then
   u begin dup 0 > while
      dup 10 mod 48 +
      CHK-NUM-I @ 1- CHK-NUM-I !
      CHK-NUM-BUF CHK-NUM-I @ + c!
      10 /
   repeat drop
   CHK-NUM-BUF CHK-NUM-I @ + CHK-NUM-CAP CHK-NUM-I @ - ;

: CHK-RUN-N ( n -- )
   CHK-U$ CHK-RUN+ ;

: CHK-ERR-NONNEG ( n -- )
   dup 0 < if drop s" <negative>" CHK-ERR exit then
   CHK-U$ CHK-ERR ;

: CHK-BUILD-PREFIX ( -- )
   s" 0 set-check" CHK-RUN-LN
   CHK-LABEL >LEN CHK-RUN-BUF CHK-RUN-CAP >LEN CHK-RUN-U SOURCE-APPEND-QPATH
   s"  DIAG-FILE!" CHK-RUN-LN
   CHK-JSON @ if s" -1 JSON-DIAGS !" CHK-RUN-LN then
   s" : CHECK-F-HOOK ( ptr u8 n -- n )" CHK-RUN-LN
   s"    LOWER-CERT-HOOK:HOOK ;" CHK-RUN-LN
   s" LOWER-CERT-HOOK:INSTALL" CHK-RUN-LN
   s" ' CHECK-F-HOOK set-check" CHK-RUN-LN ;

: CHK-BUILD-RUN ( -- )
   CHK-RUN-RESET
   CHK-BUILD-PREFIX
   CHK-ORIGIN-BUF CHK-ORIGIN-U @ CHK-RUN+ ;

: CHK-ARG+ ( ptr u8 n -- )
   >LEN PROC-ARGV+ ;

: CHK-LOAD-RESET ( -- )
   PROC-ARGV-RESET
   s" --load" CHK-ARG+ ;

: CHK-RUN-CAPTURE ( -- )
   CHK-HB$ >LEN CHK-OUT-BUF CHK-OUT-CAP >LEN
   CHK-ERR-BUF CHK-ERR-CAP >LEN CHK-TIMEOUT-MS >MS
   RUN-ARGV-CAPTURE MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE {: outu:len erru:len :}
             0 CHK-RC !
             erru LEN>N CHK-ERR-U !
             outu LEN>N CHK-OUT-U ! ENDOF
     err OF PCAP-FAILED:UNMAKE  {: outu:len erru:len rc:rc :}
             rc RC>N CHK-RC !
             erru LEN>N CHK-ERR-U !
             outu LEN>N CHK-OUT-U ! ENDOF
   ;MATCH ;

: CHK-REPLAY ( -- )
   CHK-OUT-BUF CHK-OUT-U @ CHK-OUT
   CHK-ERR-BUF CHK-ERR-U @ CHK-ERR ;

: CHK-RUN-STRICT ( -- )
   CHK-STRICT @ 0= if exit then
   SIGNATURE-LINT:RESET
   2 >FD SIGNATURE-LINT:OUT-FD!
   CHK-JSON @ SIGNATURE-LINT:JSON!
   CHK-LINT-SOURCE CHK-LINT-LABEL SIGNATURE-LINT:FILE-AS
   SIGNATURE-LINT:FINISH ;

: CHK-RUN-BOUNDARY ( -- )
   2 >FD CHECKED-BOUNDARY-LINT:OUT-FD!
   CHK-JSON @ CHECKED-BOUNDARY-LINT:JSON!
   LINT-TRUE CHECKED-BOUNDARY-LINT:STRICT!
   CHK-LINT-SOURCE CHECKED-BOUNDARY-LINT:FILE
   CHECKED-BOUNDARY-LINT:FINISH ;

: CHK-RUN-RESERVED-NAMES ( -- )
   RESERVED-NAME-LINT:RESET
   2 >FD RESERVED-NAME-LINT:OUT-FD!
   CHK-JSON @ RESERVED-NAME-LINT:JSON!
   CHK-LINT-SOURCE CHK-LINT-LABEL RESERVED-NAME-LINT:FILE-AS
   RESERVED-NAME-LINT:FINISH ;

: CHK-TRUST-SETUP ( -- )
   CHK-RUN-BUF CHK-RUN-CAP CHK-ORIGIN-BUF CHK-ORIGIN-CAP TRUST-LINT-BUFFERS!
   2 >FD TL-OUT-FD!
   TL-FALSE TL-REPORT-SUCCESS!
   s" ." TRUST-LINT-ROOT!
   TRUST-LINT-TODAY-NOW ;

: CHK-RUN-TRUST-SOURCE-CURRENT ( -- )
   CHK-TRUST-SETUP
   CHK-LINT-SOURCE TRUST-LINT-SOURCE-FILE ;

: CHK-RUN-TRUST-LIST-CURRENT ( -- )
   CHK-TRUST-SETUP
   TRUST-LINT-RESET
   0 begin dup CHK-POS-N @ < while
      dup CHK-POS$ TRUST-LINT-SOURCE+
      1+
   repeat drop
   TRUST-LINT-SOURCES-FINISH ;

: CHK-RUN-TRUST-SOURCE ( -- )
   [: CHK-RUN-TRUST-SOURCE-CURRENT ;] catch dup 0= if drop exit then
   CHK-THROW ;

: CHK-RUN-TRUST-LIST ( -- )
   [: CHK-RUN-TRUST-LIST-CURRENT ;] catch dup 0= if drop exit then
   CHK-THROW ;

: CHK-RUN-TRUST ( -- )
   CHK-SEL-MODE @ CHK-SEL-LIST = if CHK-RUN-TRUST-LIST exit then
   CHK-RUN-TRUST-SOURCE ;

\ Source-list all-errors redrive: run all-errors per ORIGINAL file in
\ dependency order, registering each verified file as cross-file support so
\ later files check against real prefix state. Per-file check failures
\ (70/duplicate) are collected so every file reports; any other throw aborts.

: CHK-ALL-ID-ACT ( -- )
   CHK-ALL-ID @ CHK-DEP$ 2dup CHECK-ALL-ERRORS-FILE ;

: CHK-ALL-RC-NOTE ( n -- ) {: rc:n :}
   CHK-ALL-RC @ 0= if rc CHK-ALL-RC ! then ;

: CHK-ALL-SUPPORT+ ( n -- ) {: id:n :}
   id CHK-DEP$ CHECK-ALL-ERRORS-SUPPORT+ ;

: CHK-RUN-ALL-ID ( n -- ) {: id:n :}
   id CHK-DEP-PRELOAD? 0= if exit then
   id CHK-ALL-ID !
   [: CHK-ALL-ID-ACT ;] catch {: rc:n :}
   rc 0= if id CHK-ALL-SUPPORT+ exit then
   rc CHK-E-CHECK = rc CA-DUP-RC = or if rc CHK-ALL-RC-NOTE exit then
   rc throw ;

: CHK-RUN-ALL-LIST-CURRENT ( -- )
   CHECK-ALL-ERRORS-SUPPORT-RESET
   0 CHK-ALL-RC !
   0 begin dup CHK-DEP-ORDER-N @ < while
      dup cells CHK-DEP-ORDER + @ CHK-RUN-ALL-ID
      1+
   repeat drop
   CHK-ALL-RC @ 0 <> if CHK-ALL-RC @ throw then ;

: CHK-RUN-ALL-CURRENT ( -- )
   CHK-OUT-BUF CHK-OUT-CAP CHK-RUN-BUF CHK-RUN-CAP CHECK-ALL-ERRORS-BUFFERS!
   CHK-JSON @ CHECK-ALL-ERRORS-JSON!
   CHK-SEL-MODE @ CHK-SEL-LIST = if CHK-RUN-ALL-LIST-CURRENT exit then
   CHECK-ALL-ERRORS-SUPPORT-RESET
   CHK-LABEL CHK-SOURCE CHECK-ALL-ERRORS-FILE ;

: CHK-RUN-ALL-FLUSH ( -- )
   CHECK-ALL-ERRORS-OUT$ CHK-ERR ;

: CHK-RUN-ALL ( -- )
   [: CHK-RUN-ALL-CURRENT ;] catch
   CHK-RUN-ALL-FLUSH
   dup 0= if drop exit then
   CHK-THROW ;

: CHK-RUN-STATIC ( -- )
   CHK-RUN-ALL ;

: CHK-RUN-DIAG ( -- )
   CHK-SOURCE CHK-ORIGIN-BUF CHK-ORIGIN-CAP >LEN
   DIAG-ORIGIN>BUF LEN>N CHK-ORIGIN-U ! ;

: CHK-PREVERIFY-ACT ( -- )
   CHK-SRC-BUF CHK-PRE-U @ VERIFY:SOURCE-BUF-IN-SCOPE ;

: CHK-PREVERIFY-CAPTURE ( -- n )
   CHK-ERR-BUF CHK-ERR-CAP DIAG-BUFFER!
   [: CHK-PREVERIFY-ACT ;] catch {: rc:n :}
   DIAG-BUFFER$ CHK-ERR
   DIAG-BUFFER-OFF
   rc ;

: CHK-PREVERIFY-FILE-AS ( ptr u8 n ptr u8 n -- ) {: label:ptr labelu:n path:ptr pathu:n :}
   label labelu DIAG-FILE!
   CHK-JSON @ 0= if 0 0= 0= else 0 0= then DIAG-JSON!
   path pathu FILE-SIZE dup CHK-SRC-CAP > if E-FS-CAPACITY throw then drop
   path pathu CHK-SRC-BUF CHK-SRC-CAP READ-ALL CHK-PRE-U !
   CHK-PREVERIFY-CAPTURE dup 0 <> if throw then drop ;

: CHK-PREVERIFY-ID ( n -- ) {: id:n :}
   id CHK-DEP-PRELOAD? 0= if exit then
   id CHK-DEP$ 2dup CHK-PREVERIFY-FILE-AS ;

: CHK-PREVERIFY-ORDER ( -- )
   0 begin dup CHK-DEP-ORDER-N @ < while
      dup cells CHK-DEP-ORDER + @ CHK-PREVERIFY-ID
      1+
   repeat drop ;

: CHK-RUN-PREVERIFY-ACT ( -- )
   CHK-DEP-ORDER-N @ 0 > if CHK-PREVERIFY-ORDER exit then
   CHK-LABEL CHK-SOURCE CHK-PREVERIFY-FILE-AS ;

: CHK-SOURCE-LIST-REPORT ( -- )
   CHK-SEL-MODE @ CHK-SEL-LIST <> if exit then
   s" check.f: source-list entries:" CHK-ERR-LN
   0 begin dup CHK-POS-N @ < while
      s"   " CHK-ERR
      dup CHK-POS$ CHK-ERR
      CHK-LF CHK-ERR-C
      1+
   repeat drop ;

: CHK-PREVERIFY-DIAG-START ( -- )
   CHK-ERR-BUF CHK-ERR-CAP DIAG-BUFFER! ;

: CHK-PREVERIFY-DIAG-FLUSH ( -- )
   DIAG-BUFFER$ CHK-ERR
   DIAG-BUFFER-OFF ;

: CHK-PREVERIFY-FAIL ( n -- ) {: rc:n :}
   CHK-JSON @ if CHK-PREVERIFY-DIAG-FLUSH rc CHK-THROW then
   s" check.f: source preverify failed before run" CHK-ERR-LN
   s" check.f: label " CHK-ERR  CHK-LABEL CHK-ERR  CHK-LF CHK-ERR-C
   s" check.f: throw code " CHK-ERR  rc CHK-ERR-NONNEG  CHK-LF CHK-ERR-C
   CHK-SOURCE-LIST-REPORT
   CHK-PREVERIFY-DIAG-FLUSH
   rc CHK-THROW ;

: CHK-RUN-PREVERIFY ( -- )
   CHK-JSON @ {: old-json:bool :}
   CHK-PREVERIFY-DIAG-START
   LINT-TRUE CHK-JSON !
   CHECKER-SCOPE-START
   [: CHK-RUN-PREVERIFY-ACT ;] catch {: rc:n :}
   CHECKER-SCOPE-DONE
   old-json CHK-JSON !
   rc 0= if DIAG-BUFFER-OFF exit then
   rc CHK-PREVERIFY-FAIL ;

: CHK-RUN-HB ( -- )
   CHK-RUN-PATH CHK-RUN-BUF CHK-RUN-U @ WRITE-ALL
   CHK-LOAD-RESET
   CHK-RUN-PATH CHK-ARG+
   CHK-RUN-CAPTURE ;

: CHK-RUN-JSON-ONLY ( -- )
   2 >FD 2 >FD JSON-ONLY-FDS!
   CHK-ERR-BUF CHK-ERR-U @ JSON-ONLY-FILTER ;

: CHK-HANDLE-HB-NONJSON ( -- )
   CHK-ERR-U @ 0= if CHK-RUN-STATIC exit then
   CHK-ERR-BUF CHK-ERR-U @ CHK-ERR ;

: CHK-HANDLE-HB ( -- )
   CHK-RC @ 0= if
      CHK-REPLAY
      exit
   then
   CHK-RC @ CHK-CHILD-RC !
   CHK-OUT-BUF CHK-OUT-U @ CHK-OUT
   CHK-JSON @ if
      CHK-RUN-STATIC
      CHK-RUN-JSON-ONLY
   else
      CHK-HANDLE-HB-NONJSON
   then
   CHK-CHILD-RC @ CHK-THROW ;

: CHK-RUN-NOMINAL-LINTS ( -- )
   CHECKER-SCOPE-START
   [:
      CHK-RUN-NOMINAL
      CHK-RUN-RESERVED-NAMES
      CHK-RUN-BOUNDARY
      CHK-RUN-TRUST
      CHK-RUN-STRICT
   ;] catch {: rc:n :}
   CHECKER-SCOPE-DONE
   rc 0 <> if rc throw then ;

: CHK-RUN-STATIC-LINTS ( -- )
   \ Zero-byte input traverses every phase; a bypass is publicly unobservable.
   CHK-RUN-NOMINAL-LINTS
   CHK-ALL @ if CHK-RUN-ALL then ;

: CHK-RUN-CURRENT ( -- )
   CHK-RUN-STATIC-LINTS
   CHK-CHECK-LABEL
   CHK-RUN-DIAG
   CHK-RUN-PREVERIFY
   CHK-BUILD-RUN
   CHK-RUN-HB
   CHK-HANDLE-HB ;

: CHK-RUN-SCOPED ( -- )
   CHECKER-SCOPE-START
   [: CHK-RUN-CURRENT ;] catch {: rc:n :}
   CHECKER-SCOPE-DONE
   rc 0 <> if rc throw then ;

: CHK-RUN-INNER ( -- )
   CHECKED-BOUNDARY-LINT:RESET
   CHK-MATERIALIZE
   CHK-RUN-SCOPED ;

\ typed-local-lint: allow-bare-local - q is the next guarded run action.
: CHK-IF-CLEAN ( n [ -- ] -- n ) {: rc:n q :}
   rc 0 <> if rc exit then
   q catch ;

: CHK-FINAL-CLEAN ( n -- n n ) {: rc:n :}
   [: CHK-TEMP-CLEAN ;] catch {: temp-rc:n :}
   [: CHECKED-BOUNDARY-LINT:RESET ;] catch {: provider-rc:n :}
   temp-rc provider-rc CHK-FIRST-RC {: clean-rc:n :}
   rc clean-rc CHK-FIRST-RC
   clean-rc ;

: CHK-RUN-ACT ( -- n n )
   [: CHK-TEMP-CLEAN ;] catch
   [: CHK-RUN-INNER ;] CHK-IF-CLEAN
   CHK-FINAL-CLEAN ;

: CHK-RUN-FINISH ( n n -- n ) {: rc:n clean-rc:n :}
   clean-rc 0 <> if CHK-SESSION-CLEAR else CHK-RUN-TEMP-CLEAR then
   rc ;

: CHK-RUN-AS ( ptr u8 n -- n ) {: hb:ptr hbu:n :}
   CHK-RUN-TEMP-CLEAR
   hb hbu CHK-HB!
   CHK-RUN-ACT CHK-RUN-FINISH ;

public

: RUN ( -- n )
   s" bin/hb" CHK-RUN-AS ;

: MAIN ( -- )
   RESET
   CHK-PARSE
   RUN dup 0 <> if throw then drop ;

;package
