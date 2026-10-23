\ The patched host whole-source-read-test.f loads once and forks each case from.
\ FILE-SIZE answers a path's size and then, for the file the case grows, appends
\ blank lines past every reader's first room: past src/lib/source.f's
\ SOURCE-ROOM-MIN and past the 64 KiB a region of MEM-ALLOC-64K-SPAN rounds to.
\ It does so on every stat, so each reader that samples the file meets bytes past
\ its sample and grows its room to hold them. The subject's first stat also
\ appends a require of grown.f. Each reader is loaded after the replacement and
\ so calls it.
\
\ CASE! names a case's MODE, SUBJECT and GROW, the file to grow; MAIN runs it.
\ MODE is one of check, json, all, dep, origin, origin-refused and lsp. The test
\ calls CASE! and runs MAIN in a fork child (src/lib/test/subject.f), so the
\ case's state, its readers' and the first-stat flag never reach the next case.

require src/lib/string.f
require src/lib/fs.f

package WHOLE-SOURCE-READ-CHILD
private

variable REQUIRED
$20 constant MODE-CAP
create MODE MODE-CAP allot variable MODE-U
\ The test sizes diag-origin's capture to this growth (its GROWTH).
$11000 constant BLANK-LEN
create BLANKS BLANK-LEN allot

create SUBJECT-PATH FS-PATH-CAP allot variable SUBJECT-PATH-U
create GROW-PATH FS-PATH-CAP allot variable GROW-PATH-U
create SUBJECT-CANON FS-PATH-CAP allot variable SUBJECT-U
create GROW-CANON FS-PATH-CAP allot variable GROW-U

: SUBJECT$ ( -- ptr u8 n )
   SUBJECT-PATH SUBJECT-PATH-U @ ;

: GROW$ ( -- ptr u8 n )
   GROW-PATH GROW-PATH-U @ ;

: MODE? ( ptr u8 n -- bool )
   MODE MODE-U @ STR= ;

\ dst receives a's bytes and u its length, refused past cap.
: KEEP! ( ptr u8 n ptr u8 ptr n n -- ) {: a:ptr u:n dst:ptr up:ptr cap:n :}
   u cap > if E-STR-CAPACITY throw then
   a dst u BYTE-COPY
   u up ! ;

\ In mode origin-refused no size is answered: the stat refuses as a failed
\ mapping does (E-MEM-MAP), with a code that is not about the file.
: REFUSED? ( -- bool )
   s" origin-refused" MODE? ;

public

: SIZE ( ptr u8 n -- n )
   {: path:ptr pathu:n :}
   REFUSED? if E-MEM-MAP throw then
   path pathu FILE-SIZE {: size:n :}
   path pathu SOURCE-ROOT:CANONICAL drop {: canon:ptr canonu:n :}
   canon canonu GROW-CANON GROW-U @ STR= if
      BLANK-LEN 0 ?do $0a BLANKS i + c! loop
      path pathu BLANKS BLANK-LEN APPEND-FILE
   then
   REQUIRED @ 0= canon canonu SUBJECT-CANON SUBJECT-U @ STR= and if
      1 REQUIRED !
      path pathu S\" s\" grown.f\" required\n" APPEND-FILE
   then
   size ;

: CASE! ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: mode:ptr modeu:n subj:ptr subju:n grow:ptr growu:n :}
   mode modeu MODE MODE-U MODE-CAP KEEP!
   subj subju SUBJECT-PATH SUBJECT-PATH-U FS-PATH-CAP KEEP!
   grow growu GROW-PATH GROW-PATH-U FS-PATH-CAP KEEP!
   SUBJECT$ SOURCE-ROOT:CANONICAL drop SUBJECT-CANON SUBJECT-U FS-PATH-CAP KEEP!
   GROW$ SOURCE-ROOT:CANONICAL drop GROW-CANON GROW-U FS-PATH-CAP KEEP!
   0 REQUIRED ! ;

;package

undefine FILE-SIZE
: FILE-SIZE ( ptr u8 n -- n ) WHOLE-SOURCE-READ-CHILD:SIZE ;

require tools/check-core.f
require tools/lsp-diag.f

package WHOLE-SOURCE-READ-CHILD
private

\ The packet the language server publishes, built here rather than in SB,
\ which the server writes while it publishes: a path and the fields around it.
FS-PATH-CAP 256 + constant PKT-CAP
create PKT PKT-CAP allot
variable PKT-U

\ check.f's own entry with --json-errors and the options of the mode: its
\ status is typed, its packets go to standard error.
: CHECK-RUN ( -- )
   CHECK:RESET
   s" json-errors" CHECK:OPT
   s" all" MODE? if s" all-errors" CHECK:OPT then
   s" check" MODE? if s" all-errors" CHECK:OPT s" verify-only" CHECK:OPT then
   s" dep" MODE? if s" verify-only" CHECK:OPT then
   SUBJECT$ CHECK:FILE
   s" check: " type CHECK:RUN . cr ;

: CHECK-MODE? ( -- bool )
   s" check" MODE? s" json" MODE? or s" all" MODE? or s" dep" MODE? or ;

: PKT+ ( ptr u8 n -- )
   {: a:ptr u:n :}
   PKT-U @ u + PKT-CAP > if E-STR-CAPACITY throw then
   a PKT PKT-U @ + u BYTE-COPY
   u PKT-U +! ;

\ The language server, with the subject open, publishes one packet about GROW,
\ a file besides it, whose range it counts in GROW's text read from disk:
\ bytes 10 to 11 of it.
: LSP-RUN ( -- )
   LSP-DIAG:PREPARE
   SB-RESET s" file://" SB-APPEND SUBJECT$ SB-APPEND
   SB$ 1 SUBJECT$ s" " LSP-DOCS:DOC-OPEN
   0 PKT-U !
   S\" {\"file\":\"" PKT+
   GROW$ PKT+
   S\" \",\"byte_start\":10,\"byte_end\":11,\"verdict\":\"rejected\",\"code\":\"E-MISMATCH\",\"message\":\"bad\"}\n" PKT+
   0 PKT PKT-U @ LSP-DIAG:PUBLISH ;

public

: MAIN ( -- )
   CHECK-MODE? if CHECK-RUN exit then
   s" origin" MODE? REFUSED? or if SUBJECT$ DIAG-ORIGIN exit then
   s" lsp" MODE? if LSP-RUN exit then
   s" whole-source-read-child: no such mode" 64 die ;

;package
