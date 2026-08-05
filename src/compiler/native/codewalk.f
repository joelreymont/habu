\ codewalk.f - the machine code this process is running, read as evidence. One
\ concern: the walk over a record's instructions, and over every record that
\ holds any.
\
\ WHY THERE IS ONE WALK AND WHY IT IS HERE. Two questions about a live system
\ are answered by reading its compiled code and by nothing else: who calls a
\ word (tools/codegen-workload-scan.f), and which of those call sites a
\ republication has to move (src/compiler/native/reach.f). Both are a loop over
\ the instructions of every dictionary record that holds code, and the loop is
\ the part that is silently wrong when it is wrong: a walk that started one
\ instruction late or stopped one instruction early undercounts calls in every
\ record with one at that end, and two walks written out side by side can drift
\ apart while each stays plausible. So there is one, and the readers that would
\ otherwise have written their own take it from here.
\
\ WHICH RECORDS HOLD CODE. Every record whose wordlist number is a real
\ wordlist and whose length is not zero. The two negative wordlists are not real
\ ones: -1 marks a package NAME, whose record carries the package's identity
\ where a word's record carries a code address, and -2 marks a retired record.
\ Reading a package record's `start` as a code address is how an earlier version
\ of the workload scan walked into address 1 and took the process down, which is
\ why the test is here rather than in each caller.
\
\ AND WHY AN ADDRESS ARRIVES AS A NUMBER. That is what a dictionary record holds
\ and what a branch displacement computes. A byte read needs a pointer, so the
\ number goes through a cell the checker will hand back as one - the same route
\ the workload scan and the comparison suite read emitted instructions by.
\
\ WHAT IT DOES NOT DECIDE. What any instruction MEANS. This file hands a caller
\ an address and the instruction word at it, in order, and every reading of that
\ word belongs to whoever asked: the branch form is
\ src/compiler/native/branch.f, and the engine's copy rule is the workload
\ scan's.

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

\ One instruction word out of the code region.
: INSN@ ( n -- n ) {: a:n :}
   a AT !
   AT-PTR c@
   AT-PTR 1 + c@ 8 lshift or
   AT-PTR 2 + c@ 16 lshift or
   AT-PTR 3 + c@ 24 lshift or ;

\ Every instruction of a code span, in address order, as an address and the
\ instruction word at it.
\
\ typed-local-lint: allow-bare-local - q receives an instruction's address and
\ the instruction at it, and a local annotation cannot carry a quotation effect.
: SPAN-EACH ( n n [ n n -- ] -- ) {: s:n len:n q :}
   len NBR:INSN-BYTES / 0 ?do
      s i NBR:INSN-BYTES * +  dup INSN@  q execute
   loop ;

\ How many records the live dictionary holds.
: RECS ( -- n )
   ndict@ ;

: REC-WL ( n -- n ) {: k:n :}
   k XREF-REC XREF-WORDLIST ;

: REC-START ( n -- n ) {: k:n :}
   k XREF-REC XREF-START ;

: REC-LEN ( n -- n ) {: k:n :}
   k XREF-REC XREF-LEN ;

\ Does this record hold a word's code: a real wordlist, and something in it?
: CODED? ( n -- bool ) {: k:n :}
   k REC-WL LOWEST-WL < if false exit then
   k REC-LEN 0 > ;

\ Every instruction of one record, or nothing at all if the record holds no
\ code.
\
\ typed-local-lint: allow-bare-local - q is the walk's own callback.
: REC-EACH ( n [ n n -- ] -- ) {: k:n q :}
   k CODED? 0= if exit then
   k REC-START k REC-LEN q SPAN-EACH ;

\ Every instruction of every record that holds code, record by record in
\ dictionary order. The record loop reads the callback out of a local rather
\ than out of a variable, which is why it is written out here instead of being
\ handed to REC-EACH inside a quotation: a quotation cannot read the enclosing
\ word's locals.
\
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
