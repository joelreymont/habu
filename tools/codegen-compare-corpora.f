\ codegen-compare-corpora.f - the register of pinned corpora.
\ One concern: what each corpus is called, where its committed table lives, and
\ which source file that table is the measurement of.
\
\ WHY THERE IS A REGISTER. Those three facts used to be stated in each corpus's
\ case file and every driver named every corpus by hand: the check ran four
\ hand-written pairs, the update wrote four hand-written triples, and a fifth
\ corpus meant editing both. They are stated once now, where the cases are, and
\ the drivers walk what was declared - so a driver cannot check three tables
\ while printing a verdict for four.
\
\ WHAT A CORPUS IS CALLED. The name is the word a person types after --update to
\ regenerate that one table, and it is also what the committed table prints as
\ the command that rewrote it. One spelling, so the instruction inside a table
\ and the argument the entry accepts cannot drift apart.
\
\ WHAT IS NOT HERE. The measurement itself: a corpus's cases, its pinned inputs
\ and its new column are the part that is genuinely different, and they stay in
\ that corpus's own files. This holds the part that was copied.

require lib/errors.f
require lib/prelude.f
require lib/string.f

package CODEGEN-CORPORA

private

8 constant CORPUS-MAX             \ four are declared today; the store leaves room
64 constant NAME-MAX              \ a selector name
128 constant PATH-MAX             \ a repository-relative path

CORPUS-MAX NAME-MAX * BUFFER: NAME-BYTES
CORPUS-MAX PATH-MAX * BUFFER: BASELINE-BYTES
CORPUS-MAX PATH-MAX * BUFFER: SOURCE-BYTES
create NAME-LENS CORPUS-MAX cells allot
create BASELINE-LENS CORPUS-MAX cells allot
create SOURCE-LENS CORPUS-MAX cells allot

variable CORPUS-N

: SLOT ( ptr a n -- ptr a )
   cells + ;

\ A corpus index the register really holds. Reading past the declarations would
\ hand a driver an empty path and a table nobody wrote.
: CORPUS-OK ( n -- n )
   dup 0 < over CORPUS-N @ >= or if E-CODEGEN-COMPARE-ROW throw then ;

: NAME-AT ( n -- ptr u8 )
   NAME-MAX * NAME-BYTES + ;

: BASELINE-AT ( n -- ptr u8 )
   PATH-MAX * BASELINE-BYTES + ;

: SOURCE-AT ( n -- ptr u8 )
   PATH-MAX * SOURCE-BYTES + ;

\ Each declaration is copied into this file's own store rather than kept as the
\ caller's span. A declaration is made from a top-level line as its case file
\ loads, and the text of such a line does not outlive the line.
: COPY-IN ( ptr u8 n ptr u8 n -- n ) {: a:ptr u:n dst:ptr cap:n :}
   u cap > if E-CODEGEN-COMPARE-CAP throw then
   a dst u STR-LEN BYTE-COPY-LEN
   u ;

public

\ Declare one corpus: the name --update takes for it, the committed table it is
\ compared against, and the source file the table is the measurement of. Called
\ once from the bottom of each corpus's case file, so that naming a corpus once
\ brings everything about it with it.
: DECLARE ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: na:ptr nu:n ba:ptr bu:n sa:ptr su:n :}
   CORPUS-N @ CORPUS-MAX >= if E-CODEGEN-COMPARE-CAP throw then
   na nu  CORPUS-N @ NAME-AT NAME-MAX COPY-IN
   NAME-LENS CORPUS-N @ SLOT !
   ba bu  CORPUS-N @ BASELINE-AT PATH-MAX COPY-IN
   BASELINE-LENS CORPUS-N @ SLOT !
   sa su  CORPUS-N @ SOURCE-AT PATH-MAX COPY-IN
   SOURCE-LENS CORPUS-N @ SLOT !
   CORPUS-N @ 1+ CORPUS-N ! ;

\ How many corpora have been declared. The drivers walk this, so a corpus that
\ was loaded is measured and a corpus that was not is not counted.
: COUNT ( -- n )
   CORPUS-N @ ;

: NAME$ ( n -- ptr u8 n ) {: k:n :}
   k CORPUS-OK NAME-AT
   NAME-LENS k SLOT @ ;

: BASELINE$ ( n -- ptr u8 n ) {: k:n :}
   k CORPUS-OK BASELINE-AT
   BASELINE-LENS k SLOT @ ;

: SOURCE$ ( n -- ptr u8 n ) {: k:n :}
   k CORPUS-OK SOURCE-AT
   SOURCE-LENS k SLOT @ ;

\ The corpus of this name, or -1. This is what turns `--update corpus3` into one
\ table; a caller that gets -1 has been handed a name nobody declared and must
\ say so rather than guess which table was meant.
: FIND ( ptr u8 n -- n ) {: a:ptr u:n :}
   0 begin dup CORPUS-N @ < while
      dup NAME$ a u STR= if exit then
      1+
   repeat drop -1 ;

\ Every declared name, one per line, for a caller that has to tell a person what
\ it would have accepted.
: NAMES. ( -- )
   CORPUS-N @ 0 ?do
      s"   " type i NAME$ type
      s"   (" type i SOURCE$ type s" )" type cr
   loop ;

;package
