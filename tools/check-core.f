\ check-core.f - reusable Habu-native checked engine core.
\ Load after tools/date.f, lib/errors.f lib/string.f lib/memory.f lib/vector.f lib/fs.f
\ lib/fs-mutate.f lib/process.f lib/process-argv.f lib/source.f,
\ tools/lint/text.f, tools/lint/token.f, tools/lint/lib.f,
\ tools/lint/json-writer.f, tools/lint/source-lex.f,
\ tools/diag-origin-core.f, tools/json.f, tools/json-only-core.f,
\ tools/signature-lint-core.f, tools/checked-boundary-lint-core.f,
\ tools/reserved-name-lint-core.f, tools/trust-lint-core.f,
\ tools/check-all-errors-core.f (which loads verify-source.f), and tools/argv.f.

\ Audited hook-install boundary: this tool must install its checker hook before
\ validating generated source snippets with CHECK!.
0 set-check

: CHK-CHECK-HOOK ( ptr u8 n -- n )
   CHECK! dup -1 <> if 70 throw then ;
' CHK-CHECK-HOOK set-check
s" TYPE-RESERVED?" s" ptr u8 n -- bool" TRUST
s" CHECKER-DEFTYPE" s" ptr u8 n --" TRUST
s" CHECKER-SCOPE-START" s" --" TRUST
s" CHECKER-SCOPE-DONE" s" --" TRUST

$40000 constant CHK-SRC-CAP
$50000 constant CHK-RUN-CAP
$50000 constant CHK-ORIGIN-CAP
$8000 constant CHK-OUT-CAP
$20000 constant CHK-ERR-CAP
32 constant CHK-NUM-CAP
128 constant CHK-MAX-POS
128 constant CHK-DEP-MAX
120000 constant CHK-TIMEOUT-MS

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
create CHK-POS-A CHK-MAX-POS cells allot
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

variable CHK-ARG-I
variable CHK-POS-N
variable CHK-JSON
variable CHK-STRICT
variable CHK-ALL
variable CHK-SOURCE-LIST
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
variable CHK-SRC-PATH-U
variable CHK-RUN-PATH-U
variable CHK-ROOT-U
variable CHK-CAPTURE
variable CHK-CAP-OUT-A
variable CHK-CAP-ERR-A
variable CHK-CAP-OUT-CAP
variable CHK-CAP-ERR-CAP
variable CHK-CAP-OUT-U
variable CHK-CAP-ERR-U
variable CHK-NOM-I
variable CHK-NOM-U
variable CHK-EXP-U
variable CHK-EXP-OUT-U
variable CHK-DEP-N
variable CHK-DIR-N
variable CHK-DEP-ORDER-N

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

: CHK-CAP-OUT-A-FIELD ( -- ptr ptr u8 )
   CHK-CAP-OUT-A 0 ptr-field ;

: CHK-CAP-ERR-A-FIELD ( -- ptr ptr u8 )
   CHK-CAP-ERR-A 0 ptr-field ;

: CHK-CAP-OUT-A@ ( -- ptr u8 )
   CHK-CAP-OUT-A-FIELD @ ;

: CHK-CAP-ERR-A@ ( -- ptr u8 )
   CHK-CAP-ERR-A-FIELD @ ;

: CHK-CAP-OUT-A! ( ptr u8 -- )
   CHK-CAP-OUT-A-FIELD ! ;

: CHK-CAP-ERR-A! ( ptr u8 -- )
   CHK-CAP-ERR-A-FIELD ! ;

: CHK-ALLOC-BUF ( n -- ptr u8 )
   MEM-ALLOC-BYTES drop ;

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

: CHK-WRITE ( n ptr u8 n -- ) {: fd:n a:ptr u:n :}
   u 0= if exit then
   fd a u write u <> if E-FS-IO throw then ;

: CHK-CAP-OUT+ ( ptr u8 n -- ) {: a:ptr u:n :}
   CHK-CAP-OUT-U @ u + CHK-CAP-OUT-CAP @ > if E-STR-CAPACITY throw then
   a CHK-CAP-OUT-A@ CHK-CAP-OUT-U @ + u BYTE-COPY
   CHK-CAP-OUT-U @ u + CHK-CAP-OUT-U ! ;

: CHK-CAP-ERR+ ( ptr u8 n -- ) {: a:ptr u:n :}
   CHK-CAP-ERR-U @ u + CHK-CAP-ERR-CAP @ > if E-STR-CAPACITY throw then
   a CHK-CAP-ERR-A@ CHK-CAP-ERR-U @ + u BYTE-COPY
   CHK-CAP-ERR-U @ u + CHK-CAP-ERR-U ! ;

: CHK-OUT ( ptr u8 n -- )
   CHK-CAPTURE @ if CHK-CAP-OUT+ exit then
   1 -rot CHK-WRITE ;

: CHK-ERR ( ptr u8 n -- )
   CHK-CAPTURE @ if CHK-CAP-ERR+ exit then
   2 -rot CHK-WRITE ;

: CHK-CAPTURE-BUFFERS! ( ptr u8 n ptr u8 n -- ) {: out:ptr outcap:n err:ptr errcap:n :}
   out CHK-CAP-OUT-A!
   err CHK-CAP-ERR-A!
   outcap CHK-CAP-OUT-CAP !
   errcap CHK-CAP-ERR-CAP !
   0 CHK-CAP-OUT-U !
   0 CHK-CAP-ERR-U !
   -1 CHK-CAPTURE ! ;

: CHK-CAPTURE-OFF ( -- )
   0 CHK-CAPTURE ! ;

: CHK-CAPTURE-OUT$ ( -- ptr u8 n )
   CHK-CAP-OUT-A@ CHK-CAP-OUT-U @ ;

: CHK-CAPTURE-ERR$ ( -- ptr u8 n )
   CHK-CAP-ERR-A@ CHK-CAP-ERR-U @ ;

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
   CLEANUP-RUN
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

: CHK-POS-SLOT ( n -- ptr ptr u8 )
   CHK-POS-A CHK-PTR-U8-SLOT ;

: CHK-POS-U-SLOT ( n -- ptr n )
   cells CHK-POS-U + ;

: CHK-POS$ ( n -- ptr u8 n ) {: idx:n :}
   idx 0 < if CHK-USAGE then
   idx CHK-POS-N @ >= if CHK-USAGE then
   idx CHK-POS-SLOT @
   idx CHK-POS-U-SLOT @ ;

: CHK-ADD-POS ( ptr u8 n -- ) {: a:ptr u:n :}
   CHK-POS-N @ CHK-MAX-POS >= if CHK-USAGE then
   CHK-SOURCE-LIST @ 0= if CHK-POS-N @ 0 > if CHK-USAGE then then
   a CHK-POS-N @ CHK-POS-A CHK-PTR-U8-SLOT!
   u CHK-POS-N @ CHK-POS-U-SLOT !
   CHK-POS-N @ 1+ CHK-POS-N ! ;

: CHK-PARSE-ONE ( ptr u8 n -- ) {: a:ptr u:n :}
   a u s" --json-errors" LINT-STR= if -1 CHK-JSON ! exit then
   a u s" --strict-signatures" LINT-STR= if -1 CHK-STRICT ! exit then
   a u s" --all-errors" LINT-STR= if -1 CHK-ALL ! exit then
   a u s" --source-list" LINT-STR= if -1 CHK-SOURCE-LIST ! exit then
   a u CHK-DASH? if CHK-USAGE then
   a u CHK-ADD-POS ;

: CHK-COLLECT-REST ( -- )
   begin CHK-ARG-I @ SCRIPT-ARGC < while
      CHK-ARG-I @ CHK-ARG$ CHK-ADD-POS
      CHK-ARG-I @ 1+ CHK-ARG-I !
   repeat ;

: CHK-PARSE ( -- )
   0 CHK-ARG-I !
   0 CHK-POS-N !
   0 CHK-JSON !
   0 CHK-STRICT !
   0 CHK-ALL !
   0 CHK-SOURCE-LIST !
   begin CHK-ARG-I @ SCRIPT-ARGC < while
      CHK-ARG-I @ s" --" CHK-ARG= if
         CHK-ARG-I @ 1+ CHK-ARG-I !
         CHK-COLLECT-REST
         exit
      then
      CHK-ARG-I @ CHK-ARG$ CHK-PARSE-ONE
      CHK-ARG-I @ 1+ CHK-ARG-I !
   repeat ;

: CHK-RESET-CFG ( -- )
   0 CHK-ARG-I !
   0 CHK-POS-N !
   0 CHK-JSON !
   0 CHK-STRICT !
   0 CHK-ALL !
   0 CHK-SOURCE-LIST !
   0 CHK-SRC-U !
   0 CHK-PRE-U !
   0 CHK-RUN-U !
   0 CHK-ORIGIN-U !
   0 CHK-OUT-U !
   0 CHK-ERR-U !
   0 CHK-RC !
   0 CHK-CHILD-RC !
   0 CHK-EXP-U !
   0 CHK-EXP-OUT-U !
   0 CHK-DEP-N !
   0 CHK-DIR-N !
   0 CHK-DEP-ORDER-N ! ;

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

: CHK-MAKE-TEMP ( -- )
   CLEANUP-RESET
   s" habu-check" TMPDIR-MKDIR CHK-ROOT-BUF CHK-ROOT-U CHK-COPY!
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
   CHK-SOURCE-LIST @ if LINT-FALSE exit then
   CHK-POS-N @ 1 = ;

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

: CHK-LEX-WORD? ( n -- bool ) {: k:n :}
   k 0 < if LINT-FALSE exit then
   k L# @ >= if LINT-FALSE exit then
   k LK@ L-WORD = ;

: CHK-LEX=CI ( n ptr u8 n -- bool ) {: k:n a:ptr u:n :}
   k CHK-LEX-WORD? 0= if LINT-FALSE exit then
   k LEX-TOK a u LINT-STR=CI ;

: CHK-REQUIRE-TOK? ( n -- bool )
   s" require" CHK-LEX=CI ;

: CHK-REQUIRED-TOK? ( n -- bool )
   s" required" CHK-LEX=CI ;

: CHK-SQUOTE-TOK? ( n -- bool ) {: k:n :}
   k CHK-LEX-WORD? 0= if LINT-FALSE exit then
   k LEX-TOK {: a:ptr u:n :}
   u 2 <> if LINT-FALSE exit then
   a c@ LINT-FOLD 115 <> if LINT-FALSE exit then
   a 1 + c@ CHK-DQ = ;

: CHK-WS? ( n -- bool )
   dup CHK-SP = over 9 = or over CHK-LF = or swap CHK-CR = or ;

: CHK-SQ-SKIP ( n -- n )
   begin dup CHK-EXP-U @ < while
      CHK-EXP-BUF over + c@ CHK-WS? if 1+ else exit then
   repeat ;

: CHK-SQ-END ( n -- n )
   begin dup CHK-EXP-U @ < while
      CHK-EXP-BUF over + c@ CHK-DQ = if exit then
      1+
   repeat ;

: CHK-SQ-PATH$ ( n -- ptr u8 n bool ) {: k:n :}
   k CHK-SQUOTE-TOK? 0= if CHK-EXP-BUF 0 LINT-FALSE exit then
   k LB@ k LEX-TOK nip + CHK-SQ-SKIP {: start:n :}
   start CHK-SQ-END {: end:n :}
   end CHK-EXP-U @ >= if CHK-EXP-BUF 0 LINT-FALSE exit then
   CHK-EXP-BUF start + end start - LINT-TRUE ;

: CHK-SCAN-REQUIRE ( n -- ) {: k:n :}
   k 1+ CHK-LEX-WORD? 0= if exit then
   k 1+ LEX-TOK CHK-DEP-DIRECT+ ;

: CHK-SCAN-REQUIRED ( n -- ) {: k:n :}
   k 0 <= if exit then
   k 1- CHK-SQ-PATH$ if CHK-DEP-DIRECT+ else 2drop then ;

: CHK-SCAN-DEPS ( -- )
   0 begin dup L# @ < while
      dup CHK-REQUIRE-TOK? if dup CHK-SCAN-REQUIRE then
      dup CHK-REQUIRED-TOK? if dup CHK-SCAN-REQUIRED then
      1+
   repeat drop ;

: CHK-EXPAND-ID ( n -- ) {: id:n :}
   id CHK-DEP-CHECK
   id CHK-DEP-STATE @ 2 = if exit then
   id CHK-DEP-STATE @ 1 = if exit then
   1 id CHK-DEP-STATE !
   CHK-DIR-N @
   id CHK-DEP$ FILE? 0= if s" check.f: no such source" CHK-E-NOINPUT CHK-FAIL then
   id CHK-DEP$ CHK-EXP-BUF CHK-SRC-CAP READ-ALL CHK-EXP-U !
   CHK-EXP-BUF CHK-EXP-U @ LEX-SOURCE
   CHK-SCAN-DEPS
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

: CHK-APPEND-REQUIRED ( ptr u8 n -- )
   s" s" CHK-EXP-APP
   CHK-DQ CHK-EXP-C
   CHK-SP CHK-EXP-C
   CHK-EXP-APP
   CHK-DQ CHK-EXP-C
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

: CHK-MATERIALIZE ( -- )
   s" bin/hb" FILE? 0= if s" check.f: bin/hb missing" CHK-E-UNAVAILABLE CHK-FAIL then
   CHK-MAKE-TEMP
   CHK-SOURCE-LIST @ if CHK-MATERIALIZE-LIST exit then
   CHK-POS-N @ 0= if CHK-MATERIALIZE-STDIN else CHK-MATERIALIZE-FILE then ;

: CHK-MATERIALIZE-BUF-AS ( ptr u8 n ptr u8 n -- )
   {: src:ptr srcu:n label:ptr labelu:n :}
   s" bin/hb" FILE? 0= if s" check.f: bin/hb missing" CHK-E-UNAVAILABLE CHK-FAIL then
   srcu CHK-SRC-CAP > if E-FS-CAPACITY throw then
   src CHK-SRC-BUF srcu BYTE-COPY
   srcu CHK-SRC-U !
   CHK-MAKE-TEMP
   labelu CHK-LABEL-U !
   label CHK-LABEL-A CHK-PTR-U8!
   CHK-SRC-PATH CHK-SRC-BUF CHK-SRC-U @ WRITE-ALL
   CHK-SRC-PATH CHK-SRC-U ! CHK-SRC-A CHK-PTR-U8! ;

: CHK-MATERIALIZE-LIST-PATH ( ptr u8 n -- ) {: path:ptr pathu:n :}
   s" bin/hb" FILE? 0= if s" check.f: bin/hb missing" CHK-E-UNAVAILABLE CHK-FAIL then
   -1 CHK-SOURCE-LIST !
   path pathu CHK-ADD-POS
   CHK-MAKE-TEMP
   CHK-MATERIALIZE-LIST ;

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

: CHK-NOM-JSON ( n n -- ) {: def:n name:n :}
   LJW-RESET
   LJW-OBJECT-START
   1 s" schema_version" CHK-NOM-JU
   s" code" s" E-BAD-NOMINAL-TYPE" CHK-NOM-JSTR
   s" repair_class" s" fix_nominal_type" CHK-NOM-JSTR
   s" verdict" s" rejected" CHK-NOM-JSTR
   s" word" s" deftype" CHK-NOM-JSTR
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

: CHK-NOM-FAIL ( n n -- ) {: def:n name:n :}
   CHK-JSON @ IF def name CHK-NOM-JSON ELSE name CHK-NOM-PROSE THEN
   CHK-E-CHECK CHK-THROW ;

: CHK-NOM-NAME-BAD? ( n -- bool ) {: name:n :}
   name CHK-WORD-TOK? 0= IF LINT-TRUE exit THEN
   name LEX-TOK TYPE-RESERVED? ;

: CHK-NOM-REGISTER ( n n -- ) {: def:n name:n :}
   name CHK-NOM-NAME-BAD? IF def name CHK-NOM-FAIL THEN
   name LEX-TOK CHECKER-DEFTYPE ;

: CHK-RUN-NOMINAL-FILE ( ptr u8 n -- ) {: path:ptr pathu:n :}
   path pathu FILE-SIZE dup CHK-SRC-CAP > if E-FS-CAPACITY throw then drop
   path pathu CHK-SRC-BUF CHK-SRC-CAP READ-ALL CHK-NOM-U !
   CHK-SRC-BUF CHK-NOM-U @ LEX-SOURCE
   0 CHK-NOM-I !
   begin CHK-NOM-I @ L# @ < while
      CHK-NOM-I @ s" deftype" CHK-TOK=CI if
         CHK-NOM-I @ CHK-NOM-I @ 1+ CHK-NOM-REGISTER
         CHK-NOM-I @ 2 + CHK-NOM-I !
      else
         CHK-NOM-I @ 1 + CHK-NOM-I !
      then
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
   CHK-LABEL CHK-DQ LINT-INDEX-OF 0 >= ;

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

: CHK-BUILD-PREFIX ( -- )
   s" 0 set-check" CHK-RUN-LN
   s" s" CHK-RUN+
   CHK-DQ CHK-RUN-C
   CHK-SP CHK-RUN-C
   CHK-LABEL CHK-RUN+
   CHK-DQ CHK-RUN-C
   s"  DIAG-FILE!" CHK-RUN-LN
   CHK-JSON @ if s" -1 JSON-DIAGS !" CHK-RUN-LN then
   s" : CHECK-F-HOOK ( ptr u8 n -- n )" CHK-RUN-LN
   s"    CHECK! dup -1 <> IF 70 throw THEN ;" CHK-RUN-LN
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
   s" bin/hb" >LEN CHK-OUT-BUF CHK-OUT-CAP >LEN
   CHK-ERR-BUF CHK-ERR-CAP >LEN CHK-TIMEOUT-MS >MS
   RUN-ARGV-CAPTURE {: outu:len erru:len rc:rc :}
   rc RC>N CHK-RC !
   erru LEN>N CHK-ERR-U !
   outu LEN>N CHK-OUT-U ! ;

: CHK-REPLAY ( -- )
   CHK-OUT-BUF CHK-OUT-U @ CHK-OUT
   CHK-ERR-BUF CHK-ERR-U @ CHK-ERR ;

: CHK-RUN-STRICT ( -- )
   CHK-STRICT @ 0= if exit then
   SIGNATURE-LINT-RESET
   2 >FD SL-OUT-FD!
   CHK-JSON @ SL-JSON!
   CHK-LINT-SOURCE CHK-LINT-LABEL SIGNATURE-LINT-FILE-AS
   SIGNATURE-LINT-FINISH ;

: CHK-RUN-BOUNDARY ( -- )
   CHECKED-BOUNDARY-LINT-RESET
   2 >FD UB-OUT-FD!
   CHK-JSON @ UB-JSON!
   UB-TRUE UB-STRICT-BOUNDARY!
   CHK-LINT-SOURCE CHECKED-BOUNDARY-LINT-FILE
   CHECKED-BOUNDARY-LINT-FINISH ;

: CHK-RUN-RESERVED-NAMES ( -- )
   RESERVED-NAME-LINT-RESET
   2 >FD RNL-OUT-FD!
   CHK-JSON @ RNL-JSON!
   CHK-LINT-SOURCE CHK-LINT-LABEL RESERVED-NAME-LINT-FILE-AS
   RESERVED-NAME-LINT-FINISH ;

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
   CHK-SOURCE-LIST @ if CHK-RUN-TRUST-LIST exit then
   CHK-RUN-TRUST-SOURCE ;

: CHK-RUN-ALL-CURRENT ( -- )
   CHK-OUT-BUF CHK-OUT-CAP CHK-RUN-BUF CHK-RUN-CAP CHECK-ALL-ERRORS-BUFFERS!
   CHK-JSON @ CHECK-ALL-ERRORS-JSON!
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

: CHK-PREVERIFY-FILE-AS ( ptr u8 n ptr u8 n -- ) {: label:ptr labelu:n path:ptr pathu:n :}
   label labelu DIAG-FILE!
   CHK-JSON @ 0= if 0 0= 0= else 0 0= then DIAG-JSON!
   path pathu FILE-SIZE dup CHK-SRC-CAP > if E-FS-CAPACITY throw then drop
   path pathu CHK-SRC-BUF CHK-SRC-CAP READ-ALL CHK-PRE-U !
   CHK-SRC-BUF CHK-PRE-U @ VERIFY-SOURCE-BUF-IN-SCOPE ;

: CHK-PREVERIFY-ID ( n -- ) {: id:n :}
   id CHK-DEP-PRELOAD? 0= if exit then
   id CHK-DEP$ 2dup CHK-PREVERIFY-FILE-AS ;

: CHK-PREVERIFY-ORDER ( -- )
   0 begin dup CHK-DEP-ORDER-N @ < while
      dup cells CHK-DEP-ORDER + @ CHK-PREVERIFY-ID
      1+
   repeat drop ;

: CHK-RUN-PREVERIFY ( -- )
   CHK-DEP-ORDER-N @ 0 > if CHK-PREVERIFY-ORDER exit then
   CHK-LABEL CHK-SOURCE CHK-PREVERIFY-FILE-AS ;

: CHK-RUN-HB ( -- )
   CHK-RUN-PATH CHK-RUN-BUF CHK-RUN-U @ WRITE-ALL
   CHK-LOAD-RESET
   CHK-RUN-PATH CHK-ARG+
   CHK-RUN-CAPTURE ;

: CHK-RUN-JSON-ONLY ( -- )
   2 >FD 2 >FD JSON-ONLY-FDS!
   CHK-ERR-BUF CHK-ERR-U @ JSON-ONLY-FILTER ;

: CHK-HANDLE-HB ( -- )
   CHK-RC @ 0= if
      CHK-REPLAY
      CLEANUP-RUN
      exit
   then
   CHK-RC @ CHK-CHILD-RC !
   CHK-OUT-BUF CHK-OUT-U @ CHK-OUT
   CHK-JSON @ if
      CHK-RUN-STATIC
      CHK-RUN-JSON-ONLY
   else
      CHK-ERR-BUF CHK-ERR-U @ CHK-ERR
   then
   CHK-CHILD-RC @ CHK-THROW ;

: CHK-RUN-CURRENT ( -- )
   CHK-RUN-NOMINAL
   CHK-RUN-RESERVED-NAMES
   CHK-RUN-BOUNDARY
   CHK-RUN-TRUST
   CHK-RUN-STRICT
   CHK-ALL @ if CHK-RUN-ALL then
   CHK-CHECK-LABEL
   CHK-RUN-DIAG
   CHK-RUN-PREVERIFY
   CHK-BUILD-RUN
   CHK-RUN-HB
   CHK-HANDLE-HB ;

: CHK-DIRECT-FINISH ( n -- n ) {: rc:n :}
   rc 0 = 0= if CLEANUP-RUN then
   rc ;

: CHK-RUN-SCOPED ( -- )
   CHECKER-SCOPE-START
   [: CHK-RUN-CURRENT ;] catch {: rc:n :}
   CHECKER-SCOPE-DONE
   rc 0 <> if rc throw then ;

: CHK-DIRECT-RUN ( -- n )
   [: CHK-RUN-SCOPED ;] catch CHK-DIRECT-FINISH ;

: CHECK-MAIN ( -- )
   CHK-PARSE
   CHK-MATERIALIZE
   CHK-RUN-SCOPED ;
