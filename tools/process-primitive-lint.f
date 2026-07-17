\ process-primitive-lint.f - reject raw process primitive calls.

require tools/process-primitive-lint-core.f

package PROCESS-LINT

: N. ( n -- ) {: n:n :}
   n 10 >= if n 10 / RECURSE then
   n 10 mod 48 + emit ;

: RUN ( -- )
   s" ." PROCESS-LINT:SCAN {: bad:n :}
   s" process-primitive-lint: " type bad N. s"  finding(s)" type cr
   bad 0 > if 1 throw then ;

RUN

;package
