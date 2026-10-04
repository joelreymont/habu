\ Fresh-process side of whole-source-read-test.f. FILE-SIZE answers a path's
\ size and then, for the file the case grows, appends blank lines past every
\ reader's first room: past lib/source.f's SOURCE-ROOM-MIN and past the 64 KiB
\ a region of MEM-ALLOC-64K-SPAN rounds to. It does so on every stat, so each
\ reader that samples the file meets bytes past its sample and grows its room to
\ hold them. The subject's first stat also appends a require of grown.f. Each
\ reader is loaded after the replacement and so calls it.
\
\ Run by the test: bin/hb --load test/whole-source-read-child.f -- MODE SUBJECT
\ GROW, MODE one of check, json, all, dep, view, origin, origin-refused and
\ lsp, GROW the file to grow.

require lib/fs.f

package WHOLE-SOURCE-READ-CHILD
private

variable REQUIRED
\ The test sizes diag-origin's capture to this growth (its GROWTH).
$11000 constant BLANK-LEN
create BLANKS BLANK-LEN allot

: SUBJECT$ ( -- ptr u8 n )
   1 SCRIPT-ARGV$ ;

: GROW$ ( -- ptr u8 n )
   2 SCRIPT-ARGV$ ;

\ In mode origin-refused no size is answered: the stat refuses as a failed
\ mapping does (E-MEM-MAP), with a code that is not about the file.
: REFUSED? ( -- bool )
   0 SCRIPT-ARGV$ s" origin-refused" STR= ;

public

: SIZE ( ptr u8 n -- n )
   {: path:ptr pathu:n :}
   REFUSED? if E-MEM-MAP throw then
   path pathu FILE-SIZE {: size:n :}
   path pathu GROW$ STR= if
      BLANK-LEN 0 ?do $0a BLANKS i + c! loop
      path pathu BLANKS BLANK-LEN APPEND-FILE
   then
   REQUIRED @ 0= path pathu SUBJECT$ STR= and if
      1 REQUIRED !
      path pathu S\" s\" grown.f\" required\n" APPEND-FILE
   then
   size ;

;package

undefine FILE-SIZE
: FILE-SIZE ( ptr u8 n -- n ) WHOLE-SOURCE-READ-CHILD:SIZE ;

require tools/check-core.f
require tools/native-source-view.f
require tools/lsp-diag.f

package WHOLE-SOURCE-READ-CHILD
private

\ The packet the language server publishes, built here rather than in SB,
\ which the server writes while it publishes: a path and the fields around it.
FS-PATH-CAP 256 + constant PKT-CAP
create PKT PKT-CAP allot
variable PKT-U

: MODE? ( ptr u8 n -- bool )
   0 SCRIPT-ARGV$ STR= ;

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

: VIEW-COLLECT ( -- )
   SUBJECT$ SOURCE-VIEW:COLLECT ;

\ A file the view cannot read is named on standard error; the code is typed.
: VIEW-RUN ( -- )
   SOURCE-VIEW:OPEN
   [: VIEW-COLLECT ;] catch
   SOURCE-VIEW:CLOSE
   s" view: " type . cr ;

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
   s" view" MODE? if VIEW-RUN exit then
   s" origin" MODE? REFUSED? or if SUBJECT$ DIAG-ORIGIN exit then
   s" lsp" MODE? if LSP-RUN exit then
   s" whole-source-read-child: no such mode" 64 die ;

;package

WHOLE-SOURCE-READ-CHILD:MAIN
