\ parallel-agent-lint.f - keep docs/parallel-agents.md enforceable.
\ Load after tools/lint/lib.f.

0 set-check

$20000 constant PAL-CAP

create PAL-BUF PAL-CAP allot
create PAL-NUM 32 allot

variable PAL-LEN
variable PAL-BAD
variable PAL-NUM-L

: PAL-NL ( -- )
   10 emit ;

: PAL-U. ( u -- )
   0 PAL-NUM-L !
   dup 0= IF drop 48 emit exit THEN
   begin dup 0 > while
      dup 10 mod 48 + PAL-NUM PAL-NUM-L @ + c!
      10 /
      PAL-NUM-L @ 1+ PAL-NUM-L !
   repeat drop
   begin PAL-NUM-L @ 0 > while
      PAL-NUM-L @ 1- PAL-NUM-L !
      PAL-NUM PAL-NUM-L @ + c@ emit
   repeat ;

: PAL-MISSING {: a u :} ( ptr u8 n -- )
   s" PARALLEL-AGENT-LINT docs/parallel-agents.md missing `" type
   a u type
   s" `" type PAL-NL
   PAL-BAD @ 1+ PAL-BAD ! ;

: PAL-REQ {: a u :} ( ptr u8 n -- )
   PAL-BUF PAL-LEN @ a u CONTAINS? 0= IF a u PAL-MISSING THEN ;

: PARALLEL-AGENT-LINT ( -- )
   0 PAL-BAD !
   s" docs/parallel-agents.md" PAL-BUF PAL-CAP READ-FILE nip PAL-LEN !
   s" Map Phase" PAL-REQ
   s" Reduce Phase" PAL-REQ
   s" dot ready" PAL-REQ
   s" dot on" PAL-REQ
   s" dot off" PAL-REQ
   s" dot add" PAL-REQ
   s" jj workspace add" PAL-REQ
   s" jj rebase" PAL-REQ
   s" read-only" PAL-REQ
   s" must not edit the current working tree" PAL-REQ
   s" disjoint" PAL-REQ
   s" ( cd test && ./run.sh )" PAL-REQ
   s" parallel-agent-lint: " type PAL-BAD @ PAL-U. s"  finding(s)" type PAL-NL
   PAL-BAD @ 0 > IF 1 throw THEN ;

PARALLEL-AGENT-LINT
