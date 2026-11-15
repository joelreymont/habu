\ The language server whole-source-read-test.f runs in a fork child: with a
\ subject open, it publishes one packet about another file, which it reads from
\ disk to range the packet.

require src/lib/string.f
require src/lib/fs.f
require tools/lsp-docs.f
require tools/lsp-diag.f

package WHOLE-SOURCE-READ-CHILD
private

create SUBJECT-PATH PATH-CAP allot variable SUBJECT-PATH-U
create FILE-PATH PATH-CAP allot variable FILE-PATH-U

\ The packet, built here rather than in SB, which the server writes while it
\ publishes: a path and the fields around it.
PATH-CAP 256 + constant PKT-CAP
create PKT PKT-CAP allot
variable PKT-U

: SUBJECT$ ( -- ^u8 n )
   SUBJECT-PATH SUBJECT-PATH-U @ ;

: FILE$ ( -- ^u8 n )
   FILE-PATH FILE-PATH-U @ ;

\ dst receives a's bytes and u its length, refused past cap.
: KEEP! ( ^u8 n ^u8 ^n n -- ) {: a:^ u:n dst:^ up:^ cap:n :}
   u cap > if E-STR-CAPACITY throw then
   a dst u BYTE-COPY
   u up ! ;

: PKT+ ( ^u8 n -- )
   {: a:^ u:n :}
   PKT-U @ u + PKT-CAP > if E-STR-CAPACITY throw then
   a PKT PKT-U @ + u BYTE-COPY
   u PKT-U +! ;

public

\ SUBJECT is the open document, FILE the file the packet names.
: CASE! ( ^u8 n ^u8 n -- )
   {: subj:^ subju:n file:^ fileu:n :}
   subj subju SUBJECT-PATH SUBJECT-PATH-U PATH-CAP KEEP!
   file fileu FILE-PATH FILE-PATH-U PATH-CAP KEEP! ;

\ The packet ranges bytes 10 to 11 of FILE's text.
: MAIN ( -- )
   LSP-DIAG:PREPARE
   SB-RESET s" file://" SB-APPEND SUBJECT$ SB-APPEND
   SB$ 1 SUBJECT$ s" " LSP-DOCS:DOC-OPEN
   0 PKT-U !
   S\" {\"file\":\"" PKT+
   FILE$ PKT+
   S\" \",\"byte_start\":10,\"byte_end\":11,\"verdict\":\"rejected\",\"code\":\"E-MISMATCH\",\"message\":\"bad\"}\n" PKT+
   0 PKT PKT-U @ LSP-DIAG:PUBLISH ;

;package
