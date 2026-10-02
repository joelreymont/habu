\ check-core.f - reusable Habu-native checked engine core.

require lib/date.f
require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/vector.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f                \ the run stage spawns with an environment
require lib/process-tree.f               \ a signal's answer ends the run stage's child tree
require lib/signal.f                     \ check.f answers SIGTERM, SIGINT and SIGHUP
require lib/fmt.f                        \ the answer names a step that throws
require lib/source.f
require lib/argv.f
require tools/lint/text.f
require tools/lint/token.f
require tools/lint/lib.f
require tools/lint/json-writer.f
require tools/lint/source-lex.f
require tools/json.f
require tools/json-only-core.f
require tools/diag-origin-core.f
require tools/checked-boundary-lint-core.f
require tools/reserved-name-lint-core.f
require tools/check-all-errors-core.f    \ loads src/habu/verify-source.f
\ The dependency-closure producer (whole-file ordered loader events) and its
\ dynamic-tail manifest.
require tools/dynamic-tail-manifest.f
require tools/source-discovery.f
require src/core/checker-owner-guard.f

\ These checker axioms retire with habu-primitive-effect-axiom-1119f176.
\ CHECK! certifies snippets so the fail-closed source hook compiles checked.
\ TYPE-RESERVED? is the DEFLINEAR and VALUE-RECORD name rule.
\ CHECKER-DEFLINEAR publishes parsed linearity metadata in the child scope.
\ CHECKER-TRY-RECORD publishes a parsed record, or answers the field it refuses.
\ CHECKER-SCOPE-START/DONE isolate, then roll back, generated dependency effects.
s" CHECK!" s" ptr u8 n -- n" TRUST
s" TYPE-RESERVED?" s" ptr u8 n -- bool" TRUST
s" CHECKER-DEFLINEAR" s" ptr u8 n --" TRUST
s" CHECKER-TRY-RECORD" s" ptr u8 n ptr u8 n -- n ptr u8 n" TRUST
s" CHECKER-SCOPE-START" s" --" TRUST
s" CHECKER-SCOPE-START-NEUTRAL" s" --" TRUST
s" CHECKER-SCOPE-DONE" s" --" TRUST

package CHECK
using SOURCE                             \ the shared source-string emitters
using SOURCE-ROOT

: CHK-CHECK-HOOK ( ptr u8 n -- n )
   CHECK! dup -1 <> if 70 throw then ;
LOWER-CERT-HOOK:INSTALL
' CHK-CHECK-HOOK set-check

\ A source is at most what the engine loads from one file; tools/diag-origin.f
\ reads to the same bound.
INCLUDE-BUF-CAP constant CHK-SRC-CAP
\ The run file is the subject behind a prefix, with an origin mark on each
\ definition, and the run loads it with `--load`: it holds what the engine
\ loads from one file.
INCLUDE-BUF-CAP constant CHK-RUN-CAP
$8000 constant CHK-OUT-CAP
$20000 constant CHK-ERR-CAP
32 constant CHK-NUM-CAP
128 constant CHK-MAX-POS
128 constant CHK-DEP-MAX
\ One split per expanded file and one tail per file bound the segments.
CHK-DEP-MAX 2 * constant CHK-SEG-MAX
120000 constant CHK-TIMEOUT-MS

0 constant CHK-SEL-NONE
1 constant CHK-SEL-SOURCE
2 constant CHK-SEL-FILE
3 constant CHK-SEL-LIST

0 constant CHK-DEP-UNSEEN
1 constant CHK-DEP-OPEN
2 constant CHK-DEP-DONE

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
create CHK-DEP-ROOTS CHK-DEP-MAX FS-PATH-CAP * allot
create CHK-DEP-ROOT-US CHK-DEP-MAX cells allot
create CHK-DEP-STATES CHK-DEP-MAX cells allot
create CHK-DIR-IDS CHK-DEP-MAX cells allot
create CHK-DIR-ATS CHK-DEP-MAX cells allot
create CHK-SEG-IDS CHK-SEG-MAX cells allot
create CHK-SEG-STARTS CHK-SEG-MAX cells allot
create CHK-SEG-ENDS CHK-SEG-MAX cells allot
create CHK-ONE 1 allot

\ The lazily allocated byte buffers hold addresses, so each is a declared
\ pointer cell: a raw `variable` would launder the address through a cell of
\ unknown type.
TYPED-VARIABLE CHK-SRC-BUF-A ptr u8
TYPED-VARIABLE CHK-RUN-BUF-A ptr u8
TYPED-VARIABLE CHK-OUT-BUF-A ptr u8
TYPED-VARIABLE CHK-ERR-BUF-A ptr u8
TYPED-VARIABLE CHK-MAP-BUF-A ptr u8
TYPED-VARIABLE CHK-EXP-BUF-A ptr u8
TYPED-VARIABLE CHK-SEL-SRC-BUF-A ptr u8

variable CHK-ARG-I
variable CHK-POS-N
variable CHK-JSON
variable CHK-ALL
variable CHK-SEL-MODE
variable CHK-SEL-SRC-U
variable CHK-SEL-LABEL-U
variable CHK-SRC-U
variable CHK-PRE-AT
variable CHK-PRE-U
TYPED-VARIABLE CHK-PRE-SCOPE-A ptr u8    \ the scope statements the verified bytes start in
variable CHK-PRE-SCOPE-U
variable CHK-RUN-U
variable CHK-OUT-U
variable CHK-ERR-U
variable CHK-MAP-U
variable CHK-RC
variable CHK-CHILD-RC
variable CHK-NUM-I
variable CHK-LABEL-A
variable CHK-LABEL-U
variable CHK-SRC-A
TYPED-VARIABLE CHK-HB-A ptr u8
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
variable CHK-SEG-N
variable CHK-DISC-ID
variable CHK-ALL-SEG
variable CHK-ALL-RC
variable CHK-NOM-BAD                     \ the nominal pass reported a finding
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

: CHK-HB! ( ptr u8 n -- ) {: a:ptr u:n :}
   a CHK-HB-A !
   u CHK-HB-U ! ;

: CHK-HB$ ( -- ptr u8 n )
   CHK-HB-A @ CHK-HB-U @ ;

: CHK-ALLOC-BUF ( n -- ptr u8 )
   MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop ;

: CHK-SRC-BUF ( -- ptr u8 )
   CHK-SRC-BUF-A @ 0= if CHK-SRC-CAP CHK-ALLOC-BUF CHK-SRC-BUF-A ! then
   CHK-SRC-BUF-A @ ;

: CHK-RUN-BUF ( -- ptr u8 )
   CHK-RUN-BUF-A @ 0= if CHK-RUN-CAP CHK-ALLOC-BUF CHK-RUN-BUF-A ! then
   CHK-RUN-BUF-A @ ;

: CHK-OUT-BUF ( -- ptr u8 )
   CHK-OUT-BUF-A @ 0= if CHK-OUT-CAP CHK-ALLOC-BUF CHK-OUT-BUF-A ! then
   CHK-OUT-BUF-A @ ;

: CHK-ERR-BUF ( -- ptr u8 )
   CHK-ERR-BUF-A @ 0= if CHK-ERR-CAP CHK-ALLOC-BUF CHK-ERR-BUF-A ! then
   CHK-ERR-BUF-A @ ;

: CHK-MAP-BUF ( -- ptr u8 )
   CHK-MAP-BUF-A @ 0= if CHK-ERR-CAP CHK-ALLOC-BUF CHK-MAP-BUF-A ! then
   CHK-MAP-BUF-A @ ;

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
   s" usage: tools/check.f [--json-errors] [--all-errors] [--source-list file ... | prog.f]" CHK-ERR-LN
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
   0 CHK-PRE-AT !
   0 CHK-PRE-U !
   0 CHK-RUN-U !
   0 CHK-OUT-U !
   0 CHK-ERR-U !
   0 CHK-RC !
   0 CHK-CHILD-RC !
   0 CHK-NUM-I !
   0 CHK-LABEL-U !
   NULL$ drop CHK-LABEL-A CHK-PTR-U8!
   NULL$ drop CHK-SRC-A CHK-PTR-U8!
   0 CHK-HB-U !
   NULL$ drop CHK-HB-A !
   0 CHK-NOM-I !
   0 CHK-NOM-U !
   LINT-FALSE CHK-NOM-BAD !
   0 CHK-TFAM-NAME-I !
   0 CHK-EXP-U !
   0 CHK-EXP-OUT-U !
   0 CHK-DEP-N !
   0 CHK-DIR-N !
   0 CHK-SEG-N !
   0 CHK-DISC-ID !
   0 CHK-ALL-SEG !
   0 CHK-ALL-RC ! ;

: CHK-RESET-CFG ( -- )
   0 CHK-ARG-I !
   0 CHK-JSON !
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
   CHK-ROOT CLEANUP-FORGET
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

\ The root is spelled canonically: the engine names a file it loads by its
\ canonical path, so the run file's name in the run's diagnostics is the one
\ CHK-RUN-PATH holds (CHK-ERR-NAME-SUBJECT). It is registered for removal at
\ process exit as soon as it is held: a `die` below - a lint library's, the
\ verifier's, the engine's - ends the process without unwinding to
\ CHK-TEMP-CLEAN, which forgets the root once it has removed it.
: CHK-MAKE-TEMP ( -- )
   s" habu-check" HB-TMP-MKDIR SOURCE-ROOT:CANON-OS 0= if E-FS-IO throw then
   CHK-ROOT-BUF CHK-ROOT-U CHK-COPY!
   CHK-ROOT CLEANUP-TREE+
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

: CHK-DEP-ROOT$ ( n -- ptr u8 n ) {: id:n :}
   id CHK-DEP-CHECK
   CHK-DEP-ROOTS id FS-PATH-CAP * +
   CHK-DEP-ROOT-US id cells + @ ;

: CHK-DEP-MATCH? ( ptr u8 n n -- bool ) {: a:ptr u:n id:n :}
   a u id CHK-DEP$ LINT-STR= ;

: CHK-DEP-FIND ( ptr u8 n -- n ) {: a:ptr u:n :}
   0 begin dup CHK-DEP-N @ < while
      dup a u rot CHK-DEP-MATCH? if exit then
      1+
   repeat drop -1 ;

: CHK-DEP-NEW ( ptr u8 n ptr u8 n -- n ) {: a:ptr u:n root:ptr rootu:n :}
   u FS-PATH-CAP > rootu FS-PATH-CAP > or if E-FS-CAPACITY throw then
   CHK-DEP-N @ CHK-DEP-MAX >= if E-TBL-BOUNDS throw then
   CHK-DEP-N @ {: id:n :}
   a id CHK-DEP-PATH u BYTE-COPY
   u id CHK-DEP-U !
   root CHK-DEP-ROOTS id FS-PATH-CAP * + rootu BYTE-COPY
   rootu CHK-DEP-ROOT-US id cells + !
   CHK-DEP-UNSEEN id CHK-DEP-STATE !
   id 1+ CHK-DEP-N !
   id ;

: CHK-DEP-ID ( ptr u8 n ptr u8 n -- n ) {: a:ptr u:n root:ptr rootu:n :}
   a u CHK-DEP-FIND dup 0 >= if exit then
   drop a u root rootu CHK-DEP-NEW ;

\ A direct dep and the byte offset of the loader that names it, which
\ CHK-EXPAND-POINTS turns into the byte where the dep expands.
: CHK-DIR-PUSH ( n n -- ) {: id:n at:n :}
   CHK-DIR-N @ CHK-DEP-MAX >= if E-TBL-BOUNDS throw then
   id CHK-DIR-IDS CHK-DIR-N @ cells + !
   at CHK-DIR-ATS CHK-DIR-N @ cells + !
   CHK-DIR-N @ 1+ CHK-DIR-N ! ;

: CHK-DIR-ID@ ( n -- n )
   cells CHK-DIR-IDS + @ ;

: CHK-DIR-AT@ ( n -- n )
   cells CHK-DIR-ATS + @ ;

: CHK-DIR-AT! ( n n -- )
   cells CHK-DIR-ATS + ! ;

: CHK-DIR-ID! ( n n -- )
   cells CHK-DIR-IDS + ! ;

: CHK-DIR-SWAP ( n -- ) {: ix:n :}       \ entries ix and ix+1 trade places
   ix CHK-DIR-ID@ ix CHK-DIR-AT@ {: id:n at:n :}
   ix 1+ CHK-DIR-ID@ ix CHK-DIR-ID!
   ix 1+ CHK-DIR-AT@ ix CHK-DIR-AT!
   id ix 1+ CHK-DIR-ID!
   at ix 1+ CHK-DIR-AT! ;

: CHK-DIR-LATER? ( n -- bool ) {: ix:n :}   \ entry ix expands after entry ix+1
   ix CHK-DIR-AT@ ix 1+ CHK-DIR-AT@ > ;

\ The direct deps from base on, in the order of the bytes where they expand; a
\ stable sort, so deps that expand at one byte keep their loaders' order.
: CHK-DIR-SORT ( n -- ) {: base:n :}
   base 1+ begin dup CHK-DIR-N @ < while
      dup begin dup base > if dup 1- CHK-DIR-LATER? else LINT-FALSE then while
         1- dup CHK-DIR-SWAP
      repeat drop
      1+
   repeat drop ;

\ The scope the loader runs a file in: the open package, its visibility and the
\ `using` depth it opened at, and the `using` rows. A loaded file starts in its
\ loader's scope. When it ends, the package scope stays as the file left it and
\ the `using` depth returns to where the file started: the engine's `evaluate`
\ keeps usings file-local and package scope across a clean include
\ (src/habu/habu2.f). The bounds are the engine's (USE-MAX) and the checker's
\ (CHECKER-PACKAGE-CAP).
create CHK-SCOPE-ROWS USE-MAX CHECKER-PACKAGE-CAP * allot
create CHK-SCOPE-ROW-US USE-MAX cells allot
create CHK-SCOPE-PKG CHECKER-PACKAGE-CAP allot
variable CHK-SCOPE-PKG-U                 \ 0: no package is open
variable CHK-SCOPE-PUBLIC
variable CHK-SCOPE-SAVE                  \ the `using` depth the package opened at
variable CHK-SCOPE-DEPTH

: CHK-SCOPE-RESET ( -- )
   0 CHK-SCOPE-PKG-U !
   LINT-FALSE CHK-SCOPE-PUBLIC !
   0 CHK-SCOPE-SAVE !
   0 CHK-SCOPE-DEPTH ! ;

: CHK-SCOPE-ROW ( n -- ptr u8 )
   CHECKER-PACKAGE-CAP * CHK-SCOPE-ROWS + ;

: CHK-SCOPE-ROW$ ( n -- ptr u8 n ) {: row:n :}
   row CHK-SCOPE-ROW row cells CHK-SCOPE-ROW-US + @ ;

: CHK-SCOPE-PACKAGE ( ptr u8 n -- ) {: a:ptr u:n :}
   a CHK-SCOPE-PKG u BYTE-COPY
   u CHK-SCOPE-PKG-U !
   LINT-FALSE CHK-SCOPE-PUBLIC !
   CHK-SCOPE-DEPTH @ CHK-SCOPE-SAVE ! ;

: CHK-SCOPE-VISIBLE ( bool -- )          \ `public` is true, `private` false
   CHK-SCOPE-PKG-U @ 0 <> if CHK-SCOPE-PUBLIC ! else drop then ;

: CHK-SCOPE-END-PACKAGE ( -- )
   CHK-SCOPE-PKG-U @ 0= if exit then
   CHK-SCOPE-SAVE @ CHK-SCOPE-DEPTH !
   0 CHK-SCOPE-PKG-U ! ;

\ A `using` past the engine's bound is refused at its own token.
: CHK-SCOPE-USING ( ptr u8 n -- ) {: a:ptr u:n :}
   CHK-SCOPE-DEPTH @ {: d:n :}
   d USE-MAX >= if exit then
   a d CHK-SCOPE-ROW u BYTE-COPY
   u CHK-SCOPE-ROW-US d cells + !
   d 1+ CHK-SCOPE-DEPTH ! ;

: CHK-SCOPE-END-USING ( -- )
   CHK-SCOPE-DEPTH @ 0 > if -1 CHK-SCOPE-DEPTH +! then ;

\ A verified file's top-level scope statements while it expands: the byte each
\ starts at, its kind and the name it takes. A file's rows go when it has
\ expanded, so the table holds the files on the current load path.
0 constant CHK-EV-PACKAGE
1 constant CHK-EV-PUBLIC
2 constant CHK-EV-PRIVATE
3 constant CHK-EV-END-PACKAGE
4 constant CHK-EV-USING
5 constant CHK-EV-END-USING
1024 constant CHK-EV-MAX
$4000 constant CHK-EV-NAMES-CAP
create CHK-EV-ATS CHK-EV-MAX cells allot
create CHK-EV-KINDS CHK-EV-MAX cells allot
create CHK-EV-OFFS CHK-EV-MAX cells allot
create CHK-EV-US CHK-EV-MAX cells allot
create CHK-EV-NAMES CHK-EV-NAMES-CAP allot
variable CHK-EV-N
variable CHK-EV-NAMES-U

: CHK-EV-AT@ ( n -- n )
   cells CHK-EV-ATS + @ ;

: CHK-EV-KIND@ ( n -- n )
   cells CHK-EV-KINDS + @ ;

: CHK-EV-OFF@ ( n -- n )
   cells CHK-EV-OFFS + @ ;

: CHK-EV-NAME$ ( n -- ptr u8 n ) {: e:n :}
   CHK-EV-NAMES e CHK-EV-OFF@ + e cells CHK-EV-US + @ ;

: CHK-EV-PUSH ( ptr u8 n n n -- ) {: a:ptr u:n at:n kind:n :}
   CHK-EV-N @ CHK-EV-MAX >= if E-TBL-BOUNDS throw then
   u CHECKER-PACKAGE-CAP >= if E-TBL-BOUNDS throw then
   CHK-EV-NAMES-U @ u + CHK-EV-NAMES-CAP > if E-TBL-BOUNDS throw then
   CHK-EV-N @ {: e:n :}
   a CHK-EV-NAMES CHK-EV-NAMES-U @ + u BYTE-COPY
   at CHK-EV-ATS e cells + !
   kind CHK-EV-KINDS e cells + !
   CHK-EV-NAMES-U @ CHK-EV-OFFS e cells + !
   u CHK-EV-US e cells + !
   CHK-EV-NAMES-U @ u + CHK-EV-NAMES-U !
   e 1+ CHK-EV-N ! ;

: CHK-EV-DROP ( n -- ) {: mark:n :}      \ the rows from mark on go
   mark CHK-EV-N @ < if mark CHK-EV-OFF@ CHK-EV-NAMES-U ! then
   mark CHK-EV-N ! ;

: CHK-EV-APPLY ( n -- ) {: e:n :}
   e CHK-EV-KIND@ {: kind:n :}
   kind CHK-EV-PACKAGE = if e CHK-EV-NAME$ CHK-SCOPE-PACKAGE exit then
   kind CHK-EV-PUBLIC = if LINT-TRUE CHK-SCOPE-VISIBLE exit then
   kind CHK-EV-PRIVATE = if LINT-FALSE CHK-SCOPE-VISIBLE exit then
   kind CHK-EV-END-PACKAGE = if CHK-SCOPE-END-PACKAGE exit then
   kind CHK-EV-USING = if e CHK-EV-NAME$ CHK-SCOPE-USING exit then
   CHK-SCOPE-END-USING ;

\ The scope statements from row e on that start before byte at take effect;
\ the answer is the first row left.
: CHK-EV-APPLY-TO ( n n -- n ) {: e:n at:n :}
   e begin dup CHK-EV-N @ < if dup CHK-EV-AT@ at < else LINT-FALSE then while
      dup CHK-EV-APPLY
      1+
   repeat ;

\ Each segment starts in the scope its text runs in, written as the scope
\ statements that reopen it: the usings open when the package opened, the
\ package and its visibility, then the usings opened inside it (or the `;using`
\ that closed some of the earlier ones). verify-source reads them before the
\ segment, in its window.
$10000 constant CHK-SCOPE-TEXT-CAP
create CHK-SCOPE-TEXT CHK-SCOPE-TEXT-CAP allot
variable CHK-SCOPE-TEXT-U
create CHK-SEG-SCOPE-ATS CHK-SEG-MAX cells allot
create CHK-SEG-SCOPE-US CHK-SEG-MAX cells allot

: CHK-SCOPE-SAY ( ptr u8 n -- )          \ one word of the statements
   >LEN CHK-SCOPE-TEXT CHK-SCOPE-TEXT-CAP >LEN CHK-SCOPE-TEXT-U SOURCE-APPEND-BYTES
   CHK-SP CHK-SCOPE-TEXT CHK-SCOPE-TEXT-CAP >LEN CHK-SCOPE-TEXT-U SOURCE-APPEND-C ;

: CHK-SCOPE-SAY-USINGS ( n n -- ) {: from:n to:n :}
   from begin dup to < while
      s" using" CHK-SCOPE-SAY
      dup CHK-SCOPE-ROW$ CHK-SCOPE-SAY
      1+
   repeat drop ;

: CHK-SCOPE-SAY-CLOSED ( -- )            \ usings `;using` closed inside the package
   CHK-SCOPE-DEPTH @ begin dup CHK-SCOPE-SAVE @ < while
      s" ;using" CHK-SCOPE-SAY
      1+
   repeat drop ;

: CHK-SCOPE-SAY-ALL ( -- )
   CHK-SCOPE-PKG-U @ 0= if 0 CHK-SCOPE-DEPTH @ CHK-SCOPE-SAY-USINGS exit then
   0 CHK-SCOPE-SAVE @ CHK-SCOPE-SAY-USINGS
   s" package" CHK-SCOPE-SAY
   CHK-SCOPE-PKG CHK-SCOPE-PKG-U @ CHK-SCOPE-SAY
   CHK-SCOPE-PUBLIC @ if s" public" CHK-SCOPE-SAY then
   CHK-SCOPE-SAVE @ CHK-SCOPE-DEPTH @ CHK-SCOPE-SAY-USINGS
   CHK-SCOPE-SAY-CLOSED ;

: CHK-SEG-ID@ ( n -- n )
   cells CHK-SEG-IDS + @ ;

: CHK-SEG-START@ ( n -- n )
   cells CHK-SEG-STARTS + @ ;

: CHK-SEG-END@ ( n -- n )
   cells CHK-SEG-ENDS + @ ;

: CHK-SEG-SCOPE$ ( n -- ptr u8 n ) {: seg:n :}
   CHK-SCOPE-TEXT seg cells CHK-SEG-SCOPE-ATS + @ +
   seg cells CHK-SEG-SCOPE-US + @ ;

\ An empty span verifies nothing, so it is never recorded. A segment takes the
\ scope current when it is recorded, the scope at its first byte.
: CHK-SEG-PUSH ( n n n -- ) {: id:n start:n end:n :}
   end start <= if exit then
   CHK-SEG-N @ CHK-SEG-MAX >= if E-TBL-BOUNDS throw then
   CHK-SEG-N @ {: seg:n :}
   id CHK-SEG-IDS seg cells + !
   start CHK-SEG-STARTS seg cells + !
   end CHK-SEG-ENDS seg cells + !
   CHK-SCOPE-TEXT-U @ LEN>N {: at:n :}
   CHK-SCOPE-SAY-ALL
   at CHK-SEG-SCOPE-ATS seg cells + !
   CHK-SCOPE-TEXT-U @ LEN>N at - CHK-SEG-SCOPE-US seg cells + !
   seg 1+ CHK-SEG-N ! ;

: CHK-TARGET-LAYOUT-ACTIVE? ( ptr u8 n -- bool ) {: path:ptr pathu:n :}
   path pathu s" src/os/linux/layout.f" LINT-STR= if HB-TARGET-LINUX? exit then
   path pathu s" src/os/macos/layout.f" LINT-STR= if HB-TARGET-MACOS? exit then
   path pathu s" src/os/linux-x86-64/layout.f" LINT-STR= if
      HB-TARGET-LINUX-X86-64? exit
   then
   true ;

: CHK-DEP-PRELOAD? ( n -- bool ) {: id:n :}
   \ Discovery deliberately over-approximates guarded loaders.  The three
   \ executable layouts cannot share a checker scope: each publishes the same
   \ global names, while only the current target branch is loadable.
   id CHK-DEP$ SOURCE-ROOT:CWD$ SOURCE-ROOT:RELATIVE
   CHK-TARGET-LAYOUT-ACTIVE? 0= if false exit then
   id CHK-DEP$ RESOLVE nip nip 0= ;

: CHK-WORD-TOK? ( n -- bool ) {: k:n :}
   k LINT-LEX:COUNT >= IF LINT-FALSE exit THEN
   k LINT-LEX:KIND@ LINT-LEX:WORD = ;

: CHK-TOK=CI ( n ptr u8 n -- bool ) {: k:n a:ptr u:n :}
   k CHK-WORD-TOK? 0= IF LINT-FALSE exit THEN
   k LINT-LEX:TOKEN a u LINT-STR=CI ;

\ A raw operand is data: `[char] ;` ends no definition.
: CHK-TOK-SEMI? ( n -- bool ) {: k:n :}
   k LINT-LEX:OPERAND? if LINT-FALSE exit then
   k s" ;" CHK-TOK=CI ;

\ A definer reads its name with parse-name, the next whitespace-delimited token
\ whatever it spells: `DEFLINEAR (` names the type `(`, which TYPE-RESERVED?
\ refuses, and `: \` defines the word `\`, as the loader reads them. On a match
\ the lexer reads the token after the definer again by that rule, so no scan
\ takes a comment, or the token after one, for the name.
: CHK-DEFINER-TOK? ( n ptr u8 n -- bool ) {: k:n a:ptr u:n :}
   k a u CHK-TOK=CI 0= IF LINT-FALSE exit THEN
   k LINT-LEX:OPERAND
   LINT-TRUE ;

: CHK-TOK-BEFORE? ( n n -- bool ) {: k:n end:n :}   \ token k starts before byte end
   k LINT-LEX:COUNT >= if LINT-FALSE exit then
   k LINT-LEX:BYTE@ end < ;

: CHK-TOK-AT ( n -- n ) {: at:n :}   \ the first token starting at or past byte at
   0 begin dup at CHK-TOK-BEFORE? while 1+ repeat ;

\ Where the loader runs each file a source loads. A top-level loader statement
\ runs where it sits, so its file expands right after it, inside whatever
\ package and `using` scope the source has open there; the file's segments and
\ the rest of the source start in the scope the loader gives them (CHK-SEG-PUSH).
\ One inside a definition runs when the word does, after that definition at the
\ earliest; its file expands at the first boundary after the definition where
\ the source has closed every package and `using` it opened, as lib/aio.f's
\ AIO-LOAD:HOST runs once its package closes. A statement follows
\ verify-source's reading: `:` and `TRUSTED:` run to `;`, `char` and `'` take
\ the next token at top level and `char` and `[char]` in a body, `require` and
\ `include` take their path, and `package` and `using` their name.
-1 constant CHK-AT-PENDING               \ a loader in a body, waiting for a boundary
variable CHK-WALK-PKG                    \ a package is open
variable CHK-WALK-USE                    \ file-level `using` depth outside a package
variable CHK-WALK-IX                     \ the first direct dep still waiting for its byte
variable CHK-WALK-PENDING                \ body loaders still waiting

: CHK-TOP-PARSER? ( n -- bool ) {: k:n :}
   k s" char" CHK-TOK=CI if LINT-TRUE exit then
   k s" '" CHK-TOK=CI if LINT-TRUE exit then
   k s" require" CHK-TOK=CI if LINT-TRUE exit then
   k s" include" CHK-TOK=CI ;

: CHK-BODY-PARSER? ( n -- bool ) {: k:n :}
   k s" char" CHK-TOK=CI if LINT-TRUE exit then
   k s" [char]" CHK-TOK=CI ;

\ A definition opens at `:` or `TRUSTED:`, read as the nominal pass reads it; a
\ nameless one is that pass's to refuse (CHK-DEFINER?), so the walk only reads.
: CHK-WALK-OPENER? ( n -- bool ) {: k:n :}
   k s" :" CHK-DEFINER-TOK? if LINT-TRUE exit then
   k s" TRUSTED:" CHK-DEFINER-TOK? ;

: CHK-WALK-DEF ( n -- n )                \ from past the opener to past its `;`
   begin dup LINT-LEX:COUNT < while
      dup CHK-TOK-SEMI? if 1+ exit then
      dup CHK-BODY-PARSER? if 1+ then
      1+
   repeat ;

: CHK-WALK-NAMED ( n n -- ) {: k:n kind:n :}   \ a scope statement and the name it takes
   k 1+ CHK-WORD-TOK? if k 1+ LINT-LEX:TOKEN else s" " then
   k LINT-LEX:BYTE@ kind CHK-EV-PUSH ;

: CHK-WALK-MARK ( n n -- ) {: k:n kind:n :}    \ a scope statement that takes no name
   s" " k LINT-LEX:BYTE@ kind CHK-EV-PUSH ;

: CHK-WALK-PACKAGE ( n -- n bool ) {: k:n :}   \ package words: next index, handled
   k s" package" CHK-TOK=CI if
      LINT-TRUE CHK-WALK-PKG !
      k CHK-EV-PACKAGE CHK-WALK-NAMED k 2 + LINT-TRUE exit
   then
   k s" ;package" CHK-TOK=CI if
      LINT-FALSE CHK-WALK-PKG !
      k CHK-EV-END-PACKAGE CHK-WALK-MARK k 1+ LINT-TRUE exit
   then
   k s" public" CHK-TOK=CI if k CHK-EV-PUBLIC CHK-WALK-MARK k 1+ LINT-TRUE exit then
   k s" private" CHK-TOK=CI if k CHK-EV-PRIVATE CHK-WALK-MARK k 1+ LINT-TRUE exit then
   k LINT-FALSE ;

: CHK-WALK-USING ( n -- n bool ) {: k:n :}   \ using words: next index, handled
   k s" using" CHK-TOK=CI if
      CHK-WALK-PKG @ 0= if 1 CHK-WALK-USE +! then
      k CHK-EV-USING CHK-WALK-NAMED k 2 + LINT-TRUE exit
   then
   k s" ;using" CHK-TOK=CI if
      CHK-WALK-PKG @ 0= CHK-WALK-USE @ 0 > and if -1 CHK-WALK-USE +! then
      k CHK-EV-END-USING CHK-WALK-MARK k 1+ LINT-TRUE exit
   then
   k LINT-FALSE ;

: CHK-WALK-STEP ( n -- n ) {: k:n :}    \ the token index past one statement
   k CHK-WALK-OPENER? if k 1+ CHK-WALK-DEF exit then
   k CHK-WALK-PACKAGE if exit then drop
   k CHK-WALK-USING if exit then drop
   k CHK-TOP-PARSER? if k 2 + exit then
   k 1+ ;

: CHK-WALK-NEUTRAL? ( -- bool )
   CHK-WALK-PKG @ 0= CHK-WALK-USE @ 0= and ;

: CHK-WALK-WAITING? ( n -- bool ) {: at:n :}   \ the next waiting loader sits before at
   CHK-WALK-IX @ CHK-DIR-N @ >= if LINT-FALSE exit then
   CHK-WALK-IX @ CHK-DIR-AT@ at < ;

\ Every waiting loader before byte at, which ends a statement: one in the top
\ level expands at at, one in the definition just read waits.
: CHK-WALK-POINT ( n bool -- ) {: at:n body:bool :}
   begin at CHK-WALK-WAITING? while
      body if CHK-AT-PENDING 1 CHK-WALK-PENDING +! else at then
      CHK-WALK-IX @ CHK-DIR-AT!
      1 CHK-WALK-IX +!
   repeat ;

: CHK-WALK-RELEASE ( n n -- ) {: base:n at:n :}   \ every body loader expands at at
   CHK-WALK-PENDING @ 0= if exit then
   base begin dup CHK-WALK-IX @ < while
      dup CHK-DIR-AT@ CHK-AT-PENDING = if at over CHK-DIR-AT! then
      1+
   repeat drop
   0 CHK-WALK-PENDING ! ;

: CHK-WALK-BYTE ( n n -- n ) {: k:n len:n :}   \ where token k starts, or the end
   k LINT-LEX:COUNT >= if len exit then
   k LINT-LEX:BYTE@ ;

\ The loader offsets of the direct deps from base on become the bytes where
\ each dep expands, in that order, and the file's top-level scope statements
\ are recorded; the answer is the file's length.
: CHK-EXPAND-POINTS ( n n -- n ) {: id:n base:n :}
   id CHK-DEP$ FILE-SIZE CHK-SRC-CAP > if E-FS-CAPACITY throw then
   id CHK-DEP$ CHK-SRC-BUF CHK-SRC-CAP READ-ALL {: len:n :}
   CHK-SRC-BUF len LINT-LEX:SOURCE
   base CHK-WALK-IX !
   0 CHK-WALK-PENDING !
   LINT-FALSE CHK-WALK-PKG !
   0 CHK-WALK-USE !
   0 begin dup LINT-LEX:COUNT < while
      {: k:n :}
      k CHK-WALK-STEP {: next:n :}
      next len CHK-WALK-BYTE {: at:n :}
      at k CHK-WALK-OPENER? CHK-WALK-POINT
      CHK-WALK-NEUTRAL? if base at CHK-WALK-RELEASE then
      next
   repeat drop
   len LINT-FALSE CHK-WALK-POINT
   base len CHK-WALK-RELEASE
   base CHK-DIR-SORT
   len ;

\ Dependency closure: the shared whole-file ordered-event producer
\ (tools/source-discovery.f) scans every token of a file - colon bodies
\ included - and records one event per literal loader form
\ (include/included/require/required/provided) with the loader's byte offset.
\ Every event path is a direct dep, so the closure is a superset of the runtime
\ load set; dynamic or retired loader forms reject fail-closed unless manifested.

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
   CHK-DISC-ID @ CHK-DEP$ CHK-DISC-ID @ CHK-DEP-ROOT$ DISCOVER:RUN-IN ;

\ Discovery stops at a string the file never closes without saying where. Under
\ --json-errors the lexer reports the defect where it stands, by the record
\ --all-errors writes for it, and the check fails as a refusal. An end the lexer
\ does not see, such as a `{:` group left open, keeps discovery's line.
: CHK-DISC-LEX-ACT ( -- )
   CHK-DISC-ID @ CHK-DEP$ 2dup CHECK-ALL-ERRORS:LEX-FILE ;

: CHK-DISC-LEX ( -- )
   CHK-OUT-BUF CHK-OUT-CAP CHK-RUN-BUF CHK-RUN-CAP CHECK-ALL-ERRORS:BUFFERS!
   CHK-JSON @ CHECK-ALL-ERRORS:JSON!
   [: CHK-DISC-LEX-ACT ;] catch {: rc:n :}
   CHECK-ALL-ERRORS:OUT$ CHK-ERR
   rc 0 <> if rc CHK-THROW then ;

: CHK-EXPAND-SCAN ( n -- ) {: id:n :}
   id CHK-DISC-ID !
   [: CHK-DISCOVER-ACT ;] catch {: rc:n :}
   rc 0= if exit then
   rc E-DISC-UNTERM = if CHK-JSON @ if CHK-DISC-LEX then then
   rc CHK-DISC-RC? if rc CHK-DISC-FAIL then
   rc throw ;

: CHK-EVENT-DEP+ ( n -- ) {: ix:n :}
   ix EVENT-PATH@ ix SOURCE-EVENT:ROOT@ CHK-DEP-ID
   ix EVENT-TOK@ drop CHK-DIR-PUSH ;

: CHK-EVENTS>DEPS ( -- )
   0 begin dup EVENT-COUNT < while
      dup CHK-EVENT-DEP+
      1+
   repeat drop ;

\ A file has ended: its `using` depth returns to the one it was loaded at, and
\ its scope statements go.
: CHK-EXPAND-CLOSE ( n n -- ) {: ev:n depth:n :}
   depth CHK-SCOPE-DEPTH !
   ev CHK-EV-DROP ;

\ A source and the files it loads expand in the order the loader runs them: a
\ file's text up to the byte where it loads another is a segment, that file's
\ segments follow, then the rest of the text. A word the source defines before
\ its require is visible to the file it loads, and none it defines after. A
\ file expands once, at its first load, as `require` loads a path once, and a
\ file still expanding - a cycle back into it - loads nothing, as a registered
\ path does. Only a file the pre-pass verifies is cut into segments; one the
\ checking engine already holds still expands the files it loads, in order.
\ The scope follows the same order: a file's scope statements take effect up
\ to each byte where it loads another, so that file starts in the scope its
\ loader runs it in.
: CHK-EXPAND-ID ( n -- ) {: id:n :}
   id CHK-DEP-CHECK
   id CHK-DEP-STATE @ CHK-DEP-UNSEEN <> if exit then
   CHK-DEP-OPEN id CHK-DEP-STATE !
   id CHK-DEP$ FILE? 0= if s" check.f: no such source" CHK-E-NOINPUT CHK-FAIL then
   CHK-DIR-N @ {: base:n :}
   CHK-EV-N @ {: ev:n :}
   CHK-SCOPE-DEPTH @ {: depth:n :}
   id CHK-EXPAND-SCAN
   CHK-EVENTS>DEPS
   id CHK-DEP-PRELOAD? {: verified:bool :}
   verified if id base CHK-EXPAND-POINTS else 0 then {: len:n :}
   ev 0 base
   begin dup CHK-DIR-N @ < while
      {: e:n pos:n ix:n :}
      ix CHK-DIR-ID@ {: dep:n :}
      dep CHK-DEP-STATE @ CHK-DEP-UNSEEN = if
         ix CHK-DIR-AT@ {: at:n :}
         verified if id pos at CHK-SEG-PUSH then
         e at CHK-EV-APPLY-TO {: later:n :}
         dep RECURSE
         later at
      else
         e pos
      then
      ix 1+
   repeat
   drop {: e:n tail:n :}
   verified if id tail len CHK-SEG-PUSH then
   e len CHK-EV-APPLY-TO drop
   ev depth CHK-EXPAND-CLOSE
   base CHK-DIR-N !
   CHK-DEP-DONE id CHK-DEP-STATE ! ;

\ A source read from a path has its closure expanded into segments; standard
\ input is checked whole.
: CHK-EXPANDED? ( -- bool )
   CHK-DEP-N @ 0 > ;

: CHK-EXPAND-PATH ( ptr u8 n -- )
   ENTRY-RESOLVE drop RESOLVED-ROOT$ CHK-DEP-ID CHK-EXPAND-ID ;

: CHK-EXPAND-RESET ( -- )
   0 CHK-EXP-OUT-U !
   0 CHK-DEP-N !
   0 CHK-DIR-N !
   0 CHK-SEG-N !
   0 CHK-EV-N !
   0 CHK-EV-NAMES-U !
   0 CHK-SCOPE-TEXT-U !
   CHK-SCOPE-RESET ;

: CHK-WRITE-EXPANDED-SOURCE ( -- )
   CHK-SRC-PATH CHK-SRC-BUF CHK-EXP-OUT-U @ LEN>N WRITE-ALL
   CHK-SRC-PATH CHK-SRC-U ! CHK-SRC-A CHK-PTR-U8! ;

: CHK-EXP-APP ( ptr u8 n -- )
   >LEN CHK-SRC-BUF CHK-SRC-CAP >LEN CHK-EXP-OUT-U SOURCE-APPEND-BYTES ;

: CHK-EXP-C ( n -- )
   CHK-SRC-BUF CHK-SRC-CAP >LEN CHK-EXP-OUT-U SOURCE-APPEND-C ;

: CHK-APPEND-REQUIRED ( ptr u8 n -- ) {: path:ptr pathu:n :}
   \ Boot-owned dependencies are recorded portably relative to the current
   \ tree.  Materializing their canonical absolute spelling makes the child
   \ miss its boot row and reload a family such as option; paths outside the
   \ tree remain absolute and retain their distinct application identity.
   path pathu SOURCE-ROOT:CWD$ SOURCE-ROOT:RELATIVE
   >LEN CHK-SRC-BUF CHK-SRC-CAP >LEN CHK-EXP-OUT-U SOURCE-APPEND-QPATH
   CHK-SP CHK-EXP-C
   s" required" CHK-EXP-APP
   CHK-LF CHK-EXP-C ;

: CHK-MATERIALIZE-STDIN ( -- )
   CHK-LABEL-STDIN
   CHK-SRC-BUF CHK-SRC-CAP >LEN READ-STDIN-ALL LEN>N CHK-SRC-U !
   CHK-SRC-PATH CHK-SRC-BUF CHK-SRC-U @ WRITE-ALL
   CHK-SRC-PATH CHK-SRC-U ! CHK-SRC-A CHK-PTR-U8! ;

: CHK-SOURCE-TOO-BIG ( -- )
   s" check.f: source exceeds capacity" CHK-E-NOINPUT CHK-FAIL ;

\ A capacity fault while the tool takes in the subject or builds the file the
\ run loads is the clean NOINPUT diagnostic, not an uncaught E-FS-CAPACITY.
: CHK-CAPPED ( [ -- ] -- ) {: q :}
   q catch {: rc:n :}
   rc 0= if exit then
   rc E-FS-CAPACITY = if CHK-SOURCE-TOO-BIG then
   rc throw ;

\ The engine carries its own sources, so a run loads nothing from one and checks
\ nothing there; rebuilding the engine checks it. An input set that is all such
\ sources - a single file is a set of one - is refused at each input. Any other
\ input is checked by the run, in an engine of its own, even when this process
\ has loaded it.
: CHK-ENGINE-SUG$ ( -- ptr u8 n )
   s" The engine provides this source; rebuild bin/hb to check a change to it." ;

: CHK-ENGINE-JSON ( ptr u8 n -- ) {: path:ptr pathu:n :}
   LJW-RESET
   LJW-OBJECT-START
   s" schema_version" LJW-KEY 1 LJW-U LJW-COMMA
   s" code" LJW-KEY s" E-ENGINE-PROVIDED" LJW-STRING LJW-COMMA
   s" repair_class" LJW-KEY s" rebuild_engine" LJW-STRING LJW-COMMA
   s" verdict" LJW-KEY s" uncheckable" LJW-STRING LJW-COMMA
   s" file" LJW-KEY path pathu LJW-STRING LJW-COMMA
   s" line" LJW-KEY 1 LJW-U LJW-COMMA
   s" column" LJW-KEY 1 LJW-U LJW-COMMA
   s" suggestion" LJW-KEY CHK-ENGINE-SUG$ LJW-STRING
   LJW-OBJECT-END
   LJW$ CHK-ERR-LN ;

: CHK-ENGINE-PROSE ( ptr u8 n -- ) {: path:ptr pathu:n :}
   s" E-ENGINE-PROVIDED " CHK-ERR
   path pathu CHK-ERR
   s" :1:1: " CHK-ERR
   CHK-ENGINE-SUG$ CHK-ERR-LN ;

: CHK-ENGINE-INPUTS? ( -- bool )
   CHK-POS-N @ 0 ?do
      i CHK-POS$ ENGINE-PROVIDES? 0= if unloop false exit then
   loop
   true ;

: CHK-CHECK-INPUTS ( -- )
   CHK-ENGINE-INPUTS? 0= if exit then
   CHK-POS-N @ 0 ?do
      i CHK-POS$ CHK-JSON @ if CHK-ENGINE-JSON else CHK-ENGINE-PROSE then
   loop
   CHK-E-USAGE CHK-THROW ;

\ The bound is checked on the file itself: discovery sizes its own scratch to
\ the source, so a source over CHK-SRC-CAP no longer refuses there, and the
\ later read into the source buffer sits outside CHK-MATERIALIZE's catch.
: CHK-MATERIALIZE-FILE ( -- )
   CHK-CHECK-INPUTS
   0 CHK-POS$ CHK-LABEL!
   CHK-LABEL FILE? 0= if s" check.f: no such source" CHK-E-NOINPUT CHK-FAIL then
   CHK-LABEL FILE-SIZE CHK-SRC-CAP > if CHK-SOURCE-TOO-BIG then
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
   CHK-CHECK-INPUTS
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
\ clean NOINPUT diagnostic (dot habu-tfam-13-c2-checkcore-cap).
: CHK-MATERIALIZE ( -- )
   CHK-HB$ FILE? 0= if s" check.f: bin/hb missing" CHK-E-UNAVAILABLE CHK-FAIL then
   CHK-MAKE-TEMP
   [: CHK-MATERIALIZE-DISPATCH ;] CHK-CAPPED ;

: CHK-TOK-END ( n -- n ) {: k:n :}
   k LINT-LEX:BYTE@ k LINT-LEX:TOKEN nip + ;

: CHK-NOM-SRC$ ( n n -- ptr u8 n ) {: def:n name:n :}
   CHK-SRC-BUF def LINT-LEX:BYTE@ +
   name CHK-TOK-END def LINT-LEX:BYTE@ - ;

: CHK-NOM-BAD-SUG$ ( -- ptr u8 n )
   s" Choose a unique non-reserved nominal type name." ;

: CHK-NOM-JSTR ( ptr u8 n ptr u8 n -- ) {: key:ptr keyu:n val:ptr valu:n :}
   key keyu LJW-KEY val valu LJW-STRING LJW-COMMA ;

: CHK-NOM-JU ( n ptr u8 n -- )
   LJW-KEY LJW-U LJW-COMMA ;

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

\ A declaration packet up to its suggestion: the declaration from def to token
\ tok, which the packet names and locates.
: CHK-PACKET-START ( n n ptr u8 n ptr u8 n ptr u8 n -- )
   {: def:n tok:n code:ptr codeu:n class:ptr classu:n word:ptr wordu:n :}
   LJW-RESET
   LJW-OBJECT-START
   1 s" schema_version" CHK-NOM-JU
   s" code" code codeu CHK-NOM-JSTR
   s" repair_class" class classu CHK-NOM-JSTR
   s" verdict" s" rejected" CHK-NOM-JSTR
   s" word" word wordu CHK-NOM-JSTR
   s" token" LJW-KEY tok LINT-LEX:TOKEN LJW-STRING LJW-COMMA
   tok s" token_index" CHK-NOM-JU
   s" file" LJW-KEY CHK-LABEL LJW-STRING LJW-COMMA
   tok LINT-LEX:LINE@ s" line" CHK-NOM-JU
   tok LINT-LEX:COL@ s" column" CHK-NOM-JU
   tok LINT-LEX:BYTE@ s" byte_start" CHK-NOM-JU
   tok CHK-TOK-END s" byte_end" CHK-NOM-JU
   s" definition_source" LJW-KEY def tok CHK-NOM-SRC$ LJW-STRING LJW-COMMA
   s" declared_effect" s" unknown " CHK-NOM-JSTR
   s" declared_effect_source" s" unknown" CHK-NOM-JSTR
   s" inferred_effect" s" unknown " CHK-NOM-JSTR
   s" return_stack" LJW-KEY
   LJW-OBJECT-START
   s" expected" LJW-KEY s" " LJW-STRING LJW-COMMA
   s" actual" LJW-KEY s" " LJW-STRING
   LJW-OBJECT-END
   LJW-COMMA ;

: CHK-PACKET-END ( ptr u8 n -- ) {: sug:ptr sugu:n :}
   s" suggestion" LJW-KEY sug sugu LJW-STRING
   LJW-OBJECT-END
   LJW$ CHK-ERR
   CHK-LF CHK-ERR-C ;

: CHK-TYPE-JSON ( n n ptr u8 n -- ) {: def:n name:n word:ptr wordu:n :}
   def name s" E-BAD-NOMINAL-TYPE" s" fix_nominal_type" word wordu CHK-PACKET-START
   CHK-NOM-BAD-SUG$ CHK-PACKET-END ;

: CHK-NOM-PROSE ( n -- ) {: name:n :}
   s" check.f: bad nominal type '" CHK-ERR
   name LINT-LEX:TOKEN CHK-ERR
   39 CHK-ERR-C
   CHK-LF CHK-ERR-C ;

\ A nominal finding is reported and the scan goes on to the next declaration;
\ CHK-RUN-NOMINAL fails the run once every file is read, so one run reports
\ every finding the pass can make.
: CHK-NOM-FOUND ( -- )
   LINT-TRUE CHK-NOM-BAD ! ;

: CHK-TYPE-FAIL ( n n ptr u8 n -- ) {: def:n name:n word:ptr wordu:n :}
   CHK-JSON @ IF def name word wordu CHK-TYPE-JSON ELSE name CHK-NOM-PROSE THEN
   CHK-NOM-FOUND ;

: CHK-NOM-FAIL ( n n -- )
   s" deftype" CHK-TYPE-FAIL ;

: CHK-LIN-FAIL ( n n -- )
   s" deflinear" CHK-TYPE-FAIL ;

\ Prose located at token k: label, line and column.
: CHK-AT-PROSE ( n -- ) {: k:n :}
   s" check.f: " CHK-ERR
   CHK-LABEL CHK-ERR
   58 CHK-ERR-C
   k LINT-LEX:LINE@ CHK-U$ CHK-ERR
   58 CHK-ERR-C
   k LINT-LEX:COL@ CHK-U$ CHK-ERR
   s" : " CHK-ERR ;

: CHK-NONAME-SUG$ ( -- ptr u8 n )
   s" Give the definer a name: the next whitespace-delimited token." ;

\ Definer k has nothing after it, so it has no name to read: the loader refuses
\ it, and the check does at the definer.
: CHK-NONAME-FAIL ( n -- ) {: k:n :}
   CHK-JSON @ IF
      k k s" E-MISSING-NAME" s" fix_missing_name" k LINT-LEX:TOKEN CHK-PACKET-START
      CHK-NONAME-SUG$ CHK-PACKET-END
   ELSE
      k CHK-AT-PROSE
      s" missing name after '" CHK-ERR
      k LINT-LEX:TOKEN CHK-ERR
      39 CHK-ERR-C
      CHK-LF CHK-ERR-C
   THEN
   CHK-NOM-FOUND ;

\ The definer's name is read as CHK-DEFINER-TOK? reads it. A definer with no
\ token after it is refused here and answers false, so a caller answered true
\ finds the name at the next token.
: CHK-DEFINER? ( n ptr u8 n -- bool ) {: k:n a:ptr u:n :}
   k a u CHK-DEFINER-TOK? 0= IF LINT-FALSE exit THEN
   k 1+ LINT-LEX:COUNT < IF LINT-TRUE exit THEN
   k CHK-NONAME-FAIL
   LINT-FALSE ;

\ DEFTYPE NAME folds the UPPER-CASE surface name to the lowercase family tail
\ (SERIAL -> serial) and mints the tail with CHECKER-DEFFAMILY, as
\ lib/type/deftype.f does. The bad-name diagnostic still reports the surface
\ token the user wrote.
128 constant CHK-NOM-TAIL-CAP
create CHK-NOM-TAIL-BUF CHK-NOM-TAIL-CAP allot

: CHK-NOM-TAIL$ ( n -- ptr u8 n ) {: name:n :}
   name LINT-LEX:TOKEN {: a:ptr u:n :}
   u CHK-NOM-TAIL-CAP > IF E-FS-CAPACITY throw THEN
   a u CHK-NOM-TAIL-BUF FOLD-TO
   CHK-NOM-TAIL-BUF u ;

\ A DEFLINEAR or VALUE-RECORD name is spelled in effects as written, unfolded,
\ and its loader refuses it by TYPE-RESERVED? on those bytes; asking the same
\ word first is what lets a refusal become a diagnostic instead of the
\ registration's die.
: CHK-NOM-NAME-BAD? ( n -- bool )
   LINT-LEX:TOKEN TYPE-RESERVED? ;

: CHK-LIN-REGISTER ( n n -- ) {: def:n name:n :}
   name CHK-NOM-NAME-BAD? IF def name CHK-LIN-FAIL EXIT THEN
   name LINT-LEX:TOKEN CHECKER-DEFLINEAR ;

: CHK-VREC-FAIL ( n n -- )
   s" value-record" CHK-TYPE-FAIL ;

: CHK-FIELD-SUG$ ( -- ptr u8 n )
   s" Declare at least one field, each with a unique name and a known type." ;

\ A value-record field the registration refuses, at field token tok of the
\ declaration from def: the packet carries the refusal as its reason, and the
\ prose gives it with the field.
: CHK-FIELD-FAIL ( n n ptr u8 n -- ) {: def:n tok:n msg:ptr msgu:n :}
   CHK-JSON @ IF
      def tok s" E-BAD-RECORD-FIELD" s" fix_record_field" s" value-record" CHK-PACKET-START
      s" reason" msg msgu CHK-NOM-JSTR
      CHK-FIELD-SUG$ CHK-PACKET-END
   ELSE
      tok CHK-AT-PROSE
      msg msgu CHK-ERR
      s"  '" CHK-ERR
      tok LINT-LEX:TOKEN CHK-ERR
      39 CHK-ERR-C
      CHK-LF CHK-ERR-C
   THEN
   CHK-NOM-FOUND ;

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

\ The token holding byte at of the field text CHK-VREC-TOKEN+ joined, one space
\ apart, from the tokens after the record name. The end of an empty text is the
\ END-VALUE-RECORD token.
: CHK-VREC-FIELD-TOKEN ( n n -- n ) {: name:n at:n :}
   name 1+ 0
   begin over LINT-LEX:TOKEN nip over + at < while
      over LINT-LEX:TOKEN nip + 1+
      swap 1+ swap
   repeat drop ;

\ The registration answers the field it refuses instead of dying, and leaves
\ no part of the record behind, so the scan reports it and goes on.
: CHK-VREC-DEFRECORD ( n n -- ) {: def:n name:n :}
   name CHK-NOM-NAME-BAD? IF def name CHK-VREC-FAIL EXIT THEN
   name LINT-LEX:TOKEN CHK-EXP-BUF CHK-EXP-U @ CHECKER-TRY-RECORD
   {: at:n msg:ptr msgu:n :}
   msgu 0= IF EXIT THEN
   def name at CHK-VREC-FIELD-TOKEN msg msgu CHK-FIELD-FAIL ;

: CHK-VREC-REGISTER ( n n -- n ) {: def:n name:n :}
   CHK-VREC-RESET
   name 1+
   begin dup LINT-LEX:COUNT < while
      dup CHK-VREC-END? if
         def name CHK-VREC-DEFRECORD
         1+ exit
      then
      dup LINT-LEX:TOKEN CHK-VREC-TOKEN+
      1+
   repeat
   s" check.f: missing END-VALUE-RECORD" CHK-E-CHECK CHK-FAIL ;

: CHK-TFAM-DO-DEF ( -- )         \ arity token, or empty when absent (missing-arity packet)
   CHK-TFAM-NAME-I @ LINT-LEX:TOKEN
   CHK-TFAM-NAME-I @ 1+ dup LINT-LEX:COUNT < IF LINT-LEX:TOKEN ELSE drop s" " THEN
   CHECKER-DEFFAMILY ;

: CHK-SUM-DO-DEF ( -- )
   CHK-TFAM-NAME-I @ LINT-LEX:TOKEN
   CHK-EXP-BUF CHK-EXP-U @
   CHECKER-DEFSUM ;

: CHK-SUM-DO-NOEND ( -- )        \ unterminated: declaration packet from name + partial body
   CHK-TFAM-NAME-I @ LINT-LEX:TOKEN
   CHK-EXP-BUF CHK-EXP-U @
   CHECKER-DEFSUM-NOEND ;

\ A failed NEWTYPE/SUMTYPE declaration already reported through the
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
: CHK-TFAM-REGISTER ( n -- n ) {: k:n :}   \ k at 'newtype'; next scan index
   k 1+ CHK-TFAM-NAME-I !
   CHK-DECL-CAPTURE
   [: CHK-TFAM-DO-DEF ;] catch
   CHK-DECL-FLUSH
   CHK-DECL-FAIL
   k 3 + ;

: CHK-NOM-DO-DEF ( -- )
   CHK-TFAM-NAME-I @ CHK-NOM-TAIL$ s" 0" CHECKER-DEFFAMILY ;

\ A DEFTYPE name is refused exactly when CHECKER-DEFFAMILY refuses its tail,
\ which is the loader's rule. TYPE-RESERVED? is not that rule: it refuses any
\ tail a family in any package claims, including a global family's or another
\ package's that a package family may share (TDECL-REQUIRE-FAMILY-NAME), and
\ it passes control words and sum variants. With the arity fixed at 0 a refusal
\ can only be about the name, so the bad-nominal diagnostic replaces the
\ declaration packet.
: CHK-NOM-REGISTER ( n n -- ) {: def:n name:n :}
   name CHK-TFAM-NAME-I !
   CHK-DECL-CAPTURE
   [: CHK-NOM-DO-DEF ;] catch {: rc:n :}
   DIAG-BUFFER-OFF
   rc 0= IF EXIT THEN
   def name CHK-NOM-FAIL ;

\ shared block-declaration collector: name at k+1, body tokens buffered from
\ k+2 to the end token. Returns the next scan index and true, or k and false
\ when the input ends unterminated (caller reports its own terminator).
: CHK-BLOCK-COLLECT ( n ptr u8 n -- n bool ) {: k:n ea:ptr eu:n :}
   k 1+ CHK-TFAM-NAME-I !
   CHK-VREC-RESET
   k 2 +
   begin dup LINT-LEX:COUNT < while
      dup ea eu CHK-TOK=CI if 1+ LINT-TRUE exit then
      dup LINT-LEX:TOKEN CHK-VREC-TOKEN+
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
      LINT-LEX:COUNT exit
   then {: nxt:n :}
   CHK-DECL-CAPTURE
   [: CHK-SUM-DO-DEF ;] catch
   CHK-DECL-FLUSH
   CHK-DECL-FAIL
   nxt ;

\ ENUM/PRODUCT/STRUCTURE block declarations (items 14/15) register through the
\ same collector so signature uses of the family later in the file resolve. The
\ enum arm also closes the item-14 gap where an enum-declaring file failed
\ the nominal pass with unknown-family rejects.
\
\ ENUM and STRUCTURE register through the unified front ends' replay entries
\ (ENUM-DECL:ED-REPLAY, STRUCTURE-DECL:SD-REPLAY), which run the real
\ declaration grammar over the collected tokens and define no word. ENUM used to
\ drive sumtype.f's CHECKER-DEFENUM, which the type-DSL cutover deletes and
\ which only ever understood the compact form; the replay entry accepts both
\ modes. STRUCTURE had no arm at all, so any file declaring one left that family
\ unregistered and the next declaration naming it as a payload type rejected
\ with "unknown payload type".
\ PRODUCT stays on the legacy definer; the cutover retires that arm with the
\ definer itself.
\
\ CHK-BLOCK-COLLECT stops ON the terminator and does not buffer it, so each
\ replayed body re-appends the exact terminator the collector just matched. The
\ front ends parse their own terminator, and a body missing one is what their
\ "missing ;ENUM" / "missing ;STRUCTURE" gates exist to report.
\
\ COMMENTS MUST SURVIVE THE CAPTURE. The engine reads a declaration body with
\ `parse-name`, which has no comment rule, so a `\` or `(` inside one is an
\ ordinary token that hits the name gate: `ENUM c red \ note` rejects 7101
\ "name must be a lowercase tail at '\'". The lexer above disagrees — it drops a
\ `\` line comment without emitting any token at all (a `( .. )` comment does
\ become one, which is why the paren case already rejected). Rebuilding the body
\ from those tokens would therefore LAUNDER a `\` comment out of the source and
\ let the replay register a family the engine refuses.
\
\ So these two arms rebuild the body from the RAW SOURCE BYTES of the
\ declaration window instead of from the token list: from the first body token's
\ byte offset up to the terminator token's. That is byte-for-byte what the live
\ keyword would have read, comments included. The span is bounded by the two
\ token offsets the collector already found, so it is scoped strictly to one
\ declaration; every other scan in this file, and the legacy SUMTYPE/PRODUCT
\ arms, keep the token-joined body unchanged.
: CHK-DECL-RAW-BODY ( n n ptr u8 n -- ) {: k:n nxt:n ea:ptr eu:n :}
   CHK-VREC-RESET
   k 2 + {: b:n :}                     \ first body token
   nxt 1 - {: t:n :}                   \ the terminator the collector matched
   b t < IF
      b LINT-LEX:BYTE@ {: s:n :}
      t LINT-LEX:BYTE@ {: e:n :}
      CHK-SRC-BUF s +  e s -  CHK-VREC-APP
   THEN
   ea eu CHK-VREC-TOKEN+ ;
: CHK-ENUM-DO-DEF ( -- )
   CHK-TFAM-NAME-I @ LINT-LEX:TOKEN
   CHK-EXP-BUF CHK-EXP-U @
   ENUM-DECL:ED-REPLAY ;

: CHK-STRUCT-DO-DEF ( -- )
   CHK-TFAM-NAME-I @ LINT-LEX:TOKEN
   CHK-EXP-BUF CHK-EXP-U @
   STRUCTURE-DECL:SD-REPLAY ;

: CHK-PROD-DO-DEF ( -- )
   CHK-TFAM-NAME-I @ LINT-LEX:TOKEN
   CHK-EXP-BUF CHK-EXP-U @
   CHECKER-DEFPRODUCT ;

: CHK-ENUM-REGISTER ( n -- n ) {: k:n :}   \ k at 'enum'; next scan index
   k s" ;ENUM" CHK-BLOCK-COLLECT 0= if
      drop
      CHK-DECL-CAPTURE
      [: CHK-ENUM-DO-DEF ;] catch
      CHK-DECL-FLUSH
      CHK-DECL-FAIL
      LINT-LEX:COUNT exit
   then {: nxt:n :}
   k nxt s" ;ENUM" CHK-DECL-RAW-BODY
   CHK-DECL-CAPTURE
   [: CHK-ENUM-DO-DEF ;] catch
   CHK-DECL-FLUSH
   CHK-DECL-FAIL
   nxt ;

: CHK-STRUCT-REGISTER ( n -- n ) {: k:n :}   \ k at 'structure'; next scan index
   k s" ;STRUCTURE" CHK-BLOCK-COLLECT 0= if
      drop
      CHK-DECL-CAPTURE
      [: CHK-STRUCT-DO-DEF ;] catch
      CHK-DECL-FLUSH
      CHK-DECL-FAIL
      LINT-LEX:COUNT exit
   then {: nxt:n :}
   k nxt s" ;STRUCTURE" CHK-DECL-RAW-BODY
   CHK-DECL-CAPTURE
   [: CHK-STRUCT-DO-DEF ;] catch
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
\ NEWTYPE/SUMTYPE/ENUM/PRODUCT registrations above land in the declaring
\ package under the live visibility mode (TDECL reads CHECKER-PACKAGE-*), and
\ qualified pkg:tail signature uses resolve public-only exactly as native.
\ The CHECKER-SCOPE frame wrapping the nominal pass saves/restores package
\ state (RBF.PKGMODE/PKGU). Boundary: the name is the token parse-name takes
\ after `package`, and CHK-DEFINER? refuses a missing one, while
\ native-loader misuse (nesting, public outside a package, ':' in a name)
\ stays fail-closed through preverify and the child run.
: CHK-PKG-REGISTER ( n -- n ) {: k:n :}   \ k at 'package'; next scan index
   k 1+ LINT-LEX:TOKEN CHECKER-PACKAGE
   k 2 + ;

: CHK-PKG-STEP ( n -- n bool ) {: k:n :}   \ package-word dispatch: next index, handled
   k s" package" CHK-DEFINER? if k CHK-PKG-REGISTER LINT-TRUE exit then
   k s" public" CHK-TOK=CI if CHECKER-PUBLIC k 1+ LINT-TRUE exit then
   k s" private" CHK-TOK=CI if CHECKER-PRIVATE k 1+ LINT-TRUE exit then
   k s" ;package" CHK-TOK=CI if CHECKER-END-PACKAGE k 1+ LINT-TRUE exit then
   k LINT-FALSE ;

: CHK-DEF-OPENER? ( n -- bool ) {: k:n :}
   k s" :" CHK-DEFINER? IF LINT-TRUE exit THEN
   k s" TRUSTED:" CHK-DEFINER? ;

\ A raw operand is data, never a definer: `' :` starts no definition. A
\ definition ends where the expansion walker ends it, so `[char] ;` in a body
\ does not end it here either. `undefine` takes its name as a definer does, so
\ a missing one is refused here, at `undefine`, before the pre-verifier, which
\ dies on it with no location (src/habu/verify-source.f UNDEFINE-WORD).
: CHK-NOM-STEP ( n -- n ) {: k:n :}
   k LINT-LEX:OPERAND? if k 1+ exit then
   k CHK-DEF-OPENER? if k 1+ CHK-WALK-DEF exit then
   k s" undefine" CHK-DEFINER? if k 2 + exit then
   k CHK-PKG-STEP if exit then drop
   k s" deftype" CHK-DEFINER? if
      k k 1+ CHK-NOM-REGISTER
      k 2 + exit
   then
   k s" deflinear" CHK-DEFINER? if
      k k 1+ CHK-LIN-REGISTER
      k 2 + exit
   then
   k s" VALUE-RECORD" CHK-DEFINER? if
      k k 1+ CHK-VREC-REGISTER exit
   then
   k s" NEWTYPE" CHK-DEFINER? if
      k CHK-TFAM-REGISTER exit
   then
   k s" SUMTYPE" CHK-DEFINER? if
      k CHK-SUM-REGISTER exit
   then
   k s" ENUM" CHK-DEFINER? if
      k CHK-ENUM-REGISTER exit
   then
   k s" STRUCTURE" CHK-DEFINER? if
      k CHK-STRUCT-REGISTER exit
   then
   k s" PRODUCT" CHK-DEFINER? if
      k CHK-PROD-REGISTER exit
   then
   k 1 + ;

\ The whole file is lexed so a segment's tokens keep the file's own positions;
\ only the tokens that start inside it are registered.
: CHK-RUN-NOMINAL-SPAN ( ptr u8 n n n -- ) {: path:ptr pathu:n start:n end:n :}
   path pathu FILE-SIZE dup CHK-SRC-CAP > if E-FS-CAPACITY throw then drop
   path pathu CHK-SRC-BUF CHK-SRC-CAP READ-ALL CHK-NOM-U !
   CHK-SRC-BUF CHK-NOM-U @ LINT-LEX:SOURCE
   start CHK-TOK-AT CHK-NOM-I !
   begin CHK-NOM-I @ end CHK-TOK-BEFORE? while
      CHK-NOM-I @ CHK-NOM-STEP CHK-NOM-I !
   repeat ;

: CHK-RUN-NOMINAL-SEG ( n -- ) {: seg:n :}
   seg CHK-SEG-ID@ CHK-DEP$ {: path:ptr pathu:n :}
   path pathu CHK-LABEL!
   path pathu seg CHK-SEG-START@ seg CHK-SEG-END@ CHK-RUN-NOMINAL-SPAN ;

: CHK-RUN-NOMINAL-ORDER ( -- )
   CHK-LABEL {: old:ptr oldu:n :}
   0 begin dup CHK-SEG-N @ < while
      dup CHK-RUN-NOMINAL-SEG
      1+
   repeat drop
   old oldu CHK-LABEL! ;

: CHK-RUN-NOMINAL-FILES ( -- )
   CHK-EXPANDED? if CHK-RUN-NOMINAL-ORDER exit then
   CHK-SOURCE 0 CHK-SRC-CAP CHK-RUN-NOMINAL-SPAN ;

: CHK-RUN-NOMINAL ( -- )
   LINT-FALSE CHK-NOM-BAD !
   CHK-RUN-NOMINAL-FILES
   CHK-NOM-BAD @ if CHK-E-CHECK CHK-THROW then ;

: CHK-VERIFIER-XT ( n -- n ) {: off:n :}
   data-base NCOMP-DISPATCH:DECL-CELL + 0 ptr-field @
   off CELL + CHECKER-OWNER-GUARD:VALIDATE
   off + CELL-VIEW @ dup 0= IF E-NCOMP-OWNER throw THEN ;

TRUSTED: CHK-VERIFIER-ACTION ( n -- [ -- ] ) ;

TRUSTED: CHK-RUN-NOMINAL-AUTH ( -- )
   CHECKER-OWNER-ABI:VERIFY-START-OFF CHK-VERIFIER-XT CHK-VERIFIER-ACTION execute
   [: CHK-RUN-NOMINAL ;] catch
   CHECKER-OWNER-ABI:VERIFY-DONE-OFF CHK-VERIFIER-XT CHK-VERIFIER-ACTION execute
   dup 0= if drop exit then
   throw ;

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

: CHK-RUN-SP ( ptr u8 n -- )
   CHK-RUN+
   CHK-SP CHK-RUN-C ;

: CHK-ERR-NONNEG ( n -- )
   dup 0 < if drop s" <negative>" CHK-ERR exit then
   CHK-U$ CHK-ERR ;

\ The prefix shares the subject's first line, and the origin markers add no
\ line break, so line N of the run file is line N of the subject:
\ the engine counts the lines of the file it reads when it refuses a statement.
: CHK-BUILD-PREFIX ( -- )
   s" 0 set-check" CHK-RUN-SP
   CHK-LABEL >LEN CHK-RUN-BUF CHK-RUN-CAP >LEN CHK-RUN-U SOURCE-APPEND-QPATH
   s"  DIAG-FILE!" CHK-RUN-SP
   CHK-JSON @ if s" -1 JSON-DIAGS !" CHK-RUN-SP then
   s" : CHECK-F-HOOK ( ptr u8 n -- n ) LOWER-CERT-HOOK:HOOK ;" CHK-RUN-SP
   s" LOWER-CERT-HOOK:INSTALL" CHK-RUN-SP
   s" ' CHECK-F-HOOK set-check" CHK-RUN-SP ;

\ The origin pass reads the subject through check.f's own buffer and cap, as
\ the nominal pass does, and writes the marked copy straight after the prefix.
\ The engine comments a leading `#!` line only at the start of a file it reads,
\ and the prefix starts the run file, so the pass comments the subject's own
\ first, with the engine's rewrite: the scan then reads that line as a comment,
\ and the marked copy carries the rewrite the run needs.
: CHK-BUILD-ORIGIN ( -- )
   CHK-SOURCE CHK-SRC-BUF CHK-SRC-CAP READ-ALL {: len:n :}
   CHK-SRC-BUF len SOURCE-ROOT:SHEBANG-COMMENT
   CHK-SRC-BUF len
   CHK-RUN-BUF CHK-RUN-U @ +  CHK-RUN-CAP CHK-RUN-U @ - >LEN
   DIAG-ORIGIN-SOURCE>BUF LEN>N CHK-RUN-U @ + CHK-RUN-U ! ;

\ A source the read admits can still outgrow the run file once the prefix and
\ the origin marks join it; it is refused here, before the run.
: CHK-BUILD-RUN ( -- )
   CHK-RUN-RESET
   CHK-BUILD-PREFIX
   [: CHK-BUILD-ORIGIN ;] CHK-CAPPED ;

: CHK-ARG+ ( ptr u8 n -- )
   >LEN PROC-ARGV+ ;

: CHK-LOAD-RESET ( -- )
   PROC-ARGV-ENV-RESET
   s" --load" CHK-ARG+ ;

\ The checked program runs with check.f's own environment: PATH for a TOOL
\ lookup, HOME and HB_TMP for a scratch tree. An env-less spawn hands it a
\ one-NULL envp, and a program that resolves an executable through $PATH then
\ fails in the run stage while `--load` of the same file passes.
: CHK-RUN-CAPTURE ( -- )
   PROC-ENV-INHERIT-MISSING
   CHK-HB$ >LEN CHK-OUT-BUF CHK-OUT-CAP >LEN
   CHK-ERR-BUF CHK-ERR-CAP >LEN CHK-TIMEOUT-MS >MS
   RUN-ARGV-ENV-CAPTURE MATCH result
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

\ All-errors on a source read from a path or a source list: every segment in
\ the order the pre-pass verifies them (a loaded file where its loader sits),
\ in one checker scope and one multi-error session, as the default check
\ verifies them in one scope. A segment sees what a whole-file check sees: the
\ clean definitions before it, refused segments' included, and the declared
\ signature of each refused definition, so a later call of a refused word
\ checks against its declaration instead of reporting it undefined. Each
\ segment reports under its own file. A refusal (70, including SPAN's
\ E-STATEMENT-THROW for a statement's throw) is collected so every segment
\ reports. DUP-RC and any status SPAN lets out unreported abort. A duplicate
\ definition ends the check, as it ends the load and a whole-file check: the
\ load never reaches a later segment, and the checker's record of the
\ redefined word is not one a later call can be checked against.

: CHK-ALL-SEG-ACT ( -- )
   CHK-ALL-SEG @ {: seg:n :}
   seg CHK-SEG-SCOPE$
   seg CHK-SEG-ID@ CHK-DEP$ 2dup seg CHK-SEG-START@ seg CHK-SEG-END@
   CHECK-ALL-ERRORS:SPAN ;

: CHK-RUN-ALL-SEG ( n -- ) {: seg:n :}
   seg CHK-ALL-SEG !
   [: CHK-ALL-SEG-ACT ;] catch {: rc:n :}
   rc 0= if exit then
   rc CHK-E-CHECK = if rc CHK-ALL-RC ! exit then
   rc throw ;

: CHK-RUN-ALL-SEGS ( -- )
   0 begin dup CHK-SEG-N @ < while
      dup CHK-RUN-ALL-SEG
      1+
   repeat drop ;

: CHK-RUN-ALL-EXPANDED ( -- )
   0 CHK-ALL-RC !
   [: CHK-RUN-ALL-SEGS ;] CHECK-ALL-ERRORS:SESSION
   CHK-ALL-RC @ 0 <> if CHK-ALL-RC @ throw then ;

\ The report goes to standard error as the core makes it: every record of every
\ segment, however many and however long, with no buffer to outgrow.
: CHK-RUN-ALL ( -- )
   2 >FD CHK-RUN-BUF CHK-RUN-CAP CHECK-ALL-ERRORS:STREAM!
   CHK-JSON @ CHECK-ALL-ERRORS:JSON!
   CHK-EXPANDED? if CHK-RUN-ALL-EXPANDED exit then
   CHK-LABEL CHK-SOURCE CHECK-ALL-ERRORS:FILE ;

: CHK-RUN-STATIC ( -- )
   CHK-RUN-ALL ;

\ A segment is verified from its own first byte, in the scope it starts in, so
\ its diagnostics name the line, column and byte the file has there.
: CHK-PRE-SCOPE! ( ptr u8 n -- ) {: a:ptr u:n :}
   u CHK-PRE-SCOPE-U !
   a CHK-PRE-SCOPE-A ! ;

: CHK-PREVERIFY-ACT ( -- )
   CHK-PRE-SCOPE-A @ CHK-PRE-SCOPE-U @
   CHK-SRC-BUF CHK-PRE-AT @ + CHK-PRE-U @
   CHK-SRC-BUF CHK-PRE-AT @ CHECK-ALL-ERRORS:BYTE-ORIGIN CHK-PRE-AT @
   VERIFY:SOURCE-BUF-AT-IN-SCOPE ;

: CHK-PREVERIFY-CAPTURE ( -- n )
   CHK-ERR-BUF CHK-ERR-CAP DIAG-BUFFER!
   [: CHK-PREVERIFY-ACT ;] catch {: rc:n :}
   DIAG-BUFFER$ CHK-ERR
   DIAG-BUFFER-OFF
   rc ;

\ A statement that threw while it was checked is reported where it stood, by
\ the record --all-errors writes in the run's mode, and fails the run like a
\ refusal.
: CHK-PREVERIFY-THREW ( n ptr u8 n -- ) {: rc:n label:ptr labelu:n :}
   CHK-JSON @ CHECK-ALL-ERRORS:JSON!
   rc label labelu CHK-SRC-BUF CHK-PRE-AT @ CHK-PRE-U @ +
   CHECK-ALL-ERRORS:THROW-RECORD$ CHK-ERR-LN
   CHK-E-CHECK CHK-THROW ;

\ The checker reports nothing for a duplicate definition, so it is reported by
\ the record --all-errors writes in the run's mode, and fails the run with the
\ duplicate's status, as it fails the load.
: CHK-PREVERIFY-DUP ( ptr u8 n -- ) {: label:ptr labelu:n :}
   CHK-JSON @ CHECK-ALL-ERRORS:JSON!
   label labelu CHECK-ALL-ERRORS:DUP-RECORD$ CHK-ERR-LN ;

\ The checker's own diagnostics are JSON lines in either mode.
: CHK-PREVERIFY-SPAN ( ptr u8 n ptr u8 n n n -- )
   {: label:ptr labelu:n path:ptr pathu:n start:n end:n :}
   label labelu DIAG-FILE!
   0 0= DIAG-JSON!
   path pathu FILE-SIZE dup CHK-SRC-CAP > if E-FS-CAPACITY throw then drop
   path pathu CHK-SRC-BUF CHK-SRC-CAP READ-ALL end min start - CHK-PRE-U !
   start CHK-PRE-AT !
   CHK-PREVERIFY-CAPTURE {: rc:n :}
   rc CHECK-ALL-ERRORS:THREW? if rc label labelu CHK-PREVERIFY-THREW then
   rc CHECK-ALL-ERRORS:DUP-RC = if label labelu CHK-PREVERIFY-DUP then
   rc 0 <> if rc throw then ;

: CHK-PREVERIFY-SEG ( n -- ) {: seg:n :}
   seg CHK-SEG-SCOPE$ CHK-PRE-SCOPE!
   seg CHK-SEG-ID@ CHK-DEP$ 2dup seg CHK-SEG-START@ seg CHK-SEG-END@
   CHK-PREVERIFY-SPAN ;

: CHK-PREVERIFY-ORDER ( -- )
   0 begin dup CHK-SEG-N @ < while
      dup CHK-PREVERIFY-SEG
      1+
   repeat drop ;

: CHK-RUN-PREVERIFY-ACT ( -- )
   CHK-EXPANDED? if CHK-PREVERIFY-ORDER exit then
   s" " CHK-PRE-SCOPE!
   CHK-LABEL CHK-SOURCE 0 CHK-SRC-CAP CHK-PREVERIFY-SPAN ;

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

\ The preverified files are standalone sources, not a continuation of whatever
\ package this tool was called from, so the scope starts at neutral top level.
: CHK-RUN-PREVERIFY ( -- )
   CHK-PREVERIFY-DIAG-START
   CHECKER-SCOPE-START-NEUTRAL
   [: CHK-RUN-PREVERIFY-ACT ;] catch {: rc:n :}
   CHECKER-SCOPE-DONE
   rc 0= if DIAG-BUFFER-OFF exit then
   rc CHK-PREVERIFY-FAIL ;

: CHK-MAP+ ( ptr u8 n -- )
   >LEN CHK-MAP-BUF CHK-ERR-CAP >LEN CHK-MAP-U SOURCE-APPEND-BYTES ;

\ Where the run file's name next starts in the given bytes, or -1.
: CHK-RUN-NAMED-AT ( ptr u8 n -- n )
   CHK-RUN-PATH FIND-SUB MATCH option
     some OF IDX>N ENDOF
     none OF -1 ENDOF
   ;MATCH ;

\ The bytes before the run file's name at at, then the label in its place;
\ the bytes after the name remain.
: CHK-MAP-NAME ( ptr u8 n n -- ptr u8 n ) {: a:ptr u:n at:n :}
   a at CHK-MAP+
   CHK-LABEL CHK-MAP+
   at CHK-RUN-PATH-U @ + {: past:n :}
   a past +  u past - ;

\ The run file is the subject line for line (CHK-BUILD-PREFIX), so where the
\ run's diagnostics name it, as the engine's refusal of a statement does, they
\ name the subject at that line.
: CHK-ERR-NAME-SUBJECT ( -- )
   0 CHK-MAP-U !
   CHK-ERR-BUF CHK-ERR-U @
   begin 2dup CHK-RUN-NAMED-AT dup 0 >= while
      CHK-MAP-NAME
   repeat drop
   CHK-MAP+
   CHK-MAP-BUF CHK-ERR-BUF CHK-MAP-U @ LEN>N BYTE-COPY
   CHK-MAP-U @ LEN>N CHK-ERR-U ! ;

: CHK-RUN-HB ( -- )
   CHK-RUN-PATH CHK-RUN-BUF CHK-RUN-U @ WRITE-ALL
   CHK-LOAD-RESET
   CHK-RUN-PATH CHK-ARG+
   CHK-RUN-CAPTURE
   CHK-ERR-NAME-SUBJECT ;

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

\ The nominal pass registers the declarations it finds in the subject source.
\ Those declarations belong to the packages that source declares, so the scope
\ starts at neutral top level instead of adopting the caller's package.
: CHK-RUN-NOMINAL-LINTS ( -- )
   CHECKER-SCOPE-START-NEUTRAL
   [:
      CHK-RUN-NOMINAL-AUTH
      CHK-RUN-RESERVED-NAMES
      CHK-RUN-BOUNDARY
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
   CHK-RUN-PREVERIFY
   CHK-BUILD-RUN
   CHK-RUN-HB
   CHK-HANDLE-HB ;

\ Outermost boundary of one check run. Everything inside it is work on the
\ subject source, so the whole run starts at neutral top level and the caller's
\ package is restored when the run ends, however it ends.
: CHK-RUN-SCOPED ( -- )
   CHECKER-SCOPE-START-NEUTRAL
   [: CHK-RUN-CURRENT ;] catch {: rc:n :}
   CHECKER-SCOPE-DONE
   rc 0 <> if rc throw then ;

: CHK-RUN-INNER ( -- )
   CHECKED-BOUNDARY-LINT:RESET
   CHK-MATERIALIZE
   CHK-RUN-SCOPED ;

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

\ ---- a signal that stops check.f -----------------------------------------------
\
\ check.f ANSWERS SIGTERM, SIGINT AND SIGHUP AS THE GATE ROOT DOES (lib/signal.f
\ CATCH-STOPS). Under the default action it ends where it stands, and its run
\ stage's child, a process group of its own, runs on under init beside the
\ temporary directory: a `timeout` that kills check.f's group does not reach
\ it. MAIN catches the three before it makes that directory, the capture that
\ waits on the child hears them (lib/process.f PROC-STOP), and MAIN asks once
\ more when the run's own cleanup is done, for one that arrived while no
\ capture waited. RUN catches nothing: a program that runs a check in process
\ keeps its own answer.
\
\ The answer kills the child with every process under it (lib/process-tree.f)
\ and reaps it, removes the temporary directory, and dies of the signal. A step
\ that throws is named and the answer goes on. A run that ended before its child
\ started - a refused program, a usage error, resident inputs - leaves the row's
\ pid at 0 or PROC-NO-PID, and neither names a child: kill(0) is check.f's own
\ process group.
: CHK-SAY-THROW ( ptr u8 n n -- ) {: what:ptr whatu:n code:n :}
   code 0= if exit then
   s" check: " CHK-ERR what whatu CHK-ERR s"  threw " CHK-ERR
   SB-RESET code FMT:SB-INT SB$ CHK-ERR-LN ;

: CHK-KILL-TREE ( -- )
   PROC-PID @ >PID PROC-TREE:KILL-TREE ;

: CHK-KILL-CHILD ( -- )
   PROC-PID @ 0 <= if exit then
   s" child tree kill" [: CHK-KILL-TREE ;] catch CHK-SAY-THROW
   s" child reap" [: PROC-KILL-CAPTURE ;] catch CHK-SAY-THROW ;

: CHK-SIGNAL-ANSWER ( n -- ) {: sig:n :}
   CHK-KILL-CHILD
   s" cleanup" [: CHK-TEMP-CLEAN ;] catch CHK-SAY-THROW
   s" check: signal" sig SIGNAL:DIE-OF ;

: CHK-SIGNAL-CHECK ( -- )
   SIGNAL:TAKE MATCH SIGNAL:signal-result
      signal OF CHK-SIGNAL-ANSWER ENDOF
      timeout OF ENDOF
   ;MATCH ;

: CHK-CATCH-STOPS ( -- )
   SIGNAL:CATCH-STOPS
   SIGNAL:FD FD>N PROC-STOP-FD !
   [: CHK-SIGNAL-CHECK ;] is PROC-STOP ;

public

: RUN ( -- n )
   s" bin/hb" CHK-RUN-AS ;

private

: CHK-MAIN-RUN ( -- )
   RESET
   CHK-PARSE
   RUN dup 0 <> if throw then drop ;

public

: MAIN ( -- )
   CHK-CATCH-STOPS
   [: CHK-MAIN-RUN ;] catch {: rc:n :}
   CHK-SIGNAL-CHECK
   rc 0 <> if rc throw then ;

;using
;using
;package
