\ record.f - machine-readable test failure records.
\ One TSV line per failing case: TFAIL <layer> <case-id> <label>.
\ Shared by lib/test/assert.f, lib/test/snap.f, and lib/test/runner.f so gate
\ logs and repair loops parse one record shape across all test layers.

require lib/errors.f
require lib/string.f

512 constant TREC-CAP

create TREC-BUF TREC-CAP allot

variable TREC-U

: TREC-RESET ( -- )
   0 TREC-U ! ;

: TREC-C+ ( n -- )
   TREC-U @ TREC-CAP >= if drop E-STR-CAPACITY throw then
   TREC-BUF TREC-U @ + c!
   TREC-U @ 1+ TREC-U ! ;

\ Payload bytes are sanitized: tab/CR/LF would break the one-line TSV record
\ contract, so they degrade to spaces (column tabs come from the builder
\ itself, never from payloads).
: TREC-PAYLOAD-C ( n -- n )
   dup STR-TAB = over STR-LF = or over STR-CR = or if drop STR-SPACE then ;

: TREC-$+ ( ptr u8 n -- ) {: a:ptr u:n :}
   0 begin dup u < while
      dup a + c@ TREC-PAYLOAD-C TREC-C+
      1+
   repeat drop ;

: TREC-DIGITS+ ( n -- )
   dup 10 >= if dup 10 / RECURSE then
   10 mod STR-ZERO + TREC-C+ ;

: TREC-N+ ( n -- )
   dup 0 < if STR-MINUS TREC-C+ negate then
   TREC-DIGITS+ ;

: TREC$ ( -- ptr u8 n )
   TREC-BUF TREC-U @ ;

: TREC-N. ( n -- )
   TREC-RESET TREC-N+
   TREC$ type ;

\ The returned span is TREC-BUF scratch: the next record build clobbers it.
: TREC-FAIL$ ( ptr u8 n n ptr u8 n -- ptr u8 n )
   {: la:ptr lu:n id:n ba:ptr bu:n :}
   TREC-RESET
   s" TFAIL" TREC-$+
   STR-TAB TREC-C+
   la lu TREC-$+
   STR-TAB TREC-C+
   id TREC-N+
   STR-TAB TREC-C+
   ba bu TREC-$+
   TREC$ ;

: TREC-FAIL ( ptr u8 n n ptr u8 n -- )
   TREC-FAIL$ type cr ;
