\ codewalk.f - the machine code this process is running, read as evidence. One
\ concern: the walk over a record's instructions, and over every record that
\ holds any.

require lib/prelude.f
require lib/errors.f
require src/compiler/native/branch.f

package NWALK

private

variable AT

: AT-PTR ( -- ptr u8 )
   AT 0 ptr-field @ ;

-1 constant NAMESPACE-WL              \ a package name, not a word
0 constant LOWEST-WL

public

: INSN@ ( n -- n ) {: a:n :}
   a AT !
   AT-PTR c@
   AT-PTR 1 + c@ 8 lshift or
   AT-PTR 2 + c@ 16 lshift or
   AT-PTR 3 + c@ 24 lshift or ;

\ typed-local-lint: allow-bare-local - q receives an instruction's address and
\ the instruction at it, and a local annotation cannot carry a quotation effect.
: SPAN-EACH ( n n [ n n -- ] -- ) {: s:n len:n q :}
   len NBR:INSN-BYTES / 0 ?do
      s i NBR:INSN-BYTES * +  dup INSN@  q execute
   loop ;

: RECS ( -- n )
   ndict@ ;

: REC-WL ( n -- n ) {: k:n :}
   k XREF-REC XREF-WORDLIST ;

: REC-START ( n -- n ) {: k:n :}
   k XREF-REC XREF-START ;

: REC-LEN ( n -- n ) {: k:n :}
   k XREF-REC XREF-LEN ;

\ A record holds code when its wordlist is real and its length is not zero:
\ wordlist -1 is a package name and -2 is retired, and neither start is code.
: CODED? ( n -- bool ) {: k:n :}
   k REC-WL LOWEST-WL < if false exit then
   k REC-LEN 0 > ;

\ typed-local-lint: allow-bare-local - q is the walk's own callback.
: REC-EACH ( n [ n n -- ] -- ) {: k:n q :}
   k CODED? 0= if exit then
   k REC-START k REC-LEN q SPAN-EACH ;

\ Written out rather than delegating to REC-EACH: a quotation cannot read the
\ enclosing word's locals, so the callback has to stay in this body's scope.
\ typed-local-lint: allow-bare-local - q is the walk's own callback.
: LIVE-EACH ( [ n n -- ] -- ) {: q :}
   RECS 0 ?do
      i CODED? if i REC-START i REC-LEN q SPAN-EACH then
   loop ;

private

get-current prot-wid-add

public
get-current prot-wid-add

;package
