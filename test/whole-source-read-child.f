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

: SUBJECT$ ( -- str )
   SUBJECT-PATH SUBJECT-PATH-U @ >str ;

: FILE$ ( -- str )
   FILE-PATH FILE-PATH-U @ >str ;

\ dst receives s's bytes and up its length, rejected past cap.
: KEEP! ( str ^u8 ^n n -- ) {: s:str dst:^ up:^ cap:n :}
   s STR:LEN cap > if E-STR-CAPACITY throw then
   s STR:DATA dst s STR:LEN BYTE-COPY
   s STR:LEN up ! ;

: PKT+ ( str -- )
   {: s:str :}
   PKT-U @ s STR:LEN + PKT-CAP > if E-STR-CAPACITY throw then
   s STR:DATA PKT PKT-U @ + s STR:LEN BYTE-COPY
   s STR:LEN PKT-U +! ;

public

\ SUBJECT is the open document, FILE the file the packet names.
: CASE! ( str str -- )
   {: subj:str file:str :}
   subj SUBJECT-PATH SUBJECT-PATH-U PATH-CAP KEEP!
   file FILE-PATH FILE-PATH-U PATH-CAP KEEP! ;

\ The packet ranges bytes 10 to 11 of FILE's text.
: MAIN ( -- )
   LSP-DIAG:PREPARE
   SB-RESET s" file://" SB-APPEND SUBJECT$ SB-APPEND
   SB$ 1 SUBJECT$ s" " LSP-DOCS:DOC-OPEN
   0 PKT-U !
   S\" {\"file\":\"" PKT+
   FILE$ PKT+
   S\" \",\"byte_start\":10,\"byte_end\":11,\"verdict\":\"rejected\",\"code\":\"E-MISMATCH\",\"message\":\"bad\"}\n" PKT+
   0 PKT PKT-U @ >str LSP-DIAG:PUBLISH ;

;package
