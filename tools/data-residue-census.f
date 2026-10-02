\ data-residue-census.f - the 8-residue of every address-kind word's DATA cell.
\
\ WHY IT EXISTS. An AOT seed copies a captured DATA window to the booting
\ engine's DP and rebases every recorded address by one delta; a merge appends a
\ second window behind the first. Nothing in either step is obliged to keep a
\ cell's ALIGNMENT, and the atomics do not tolerate losing it - LDAXR and STLR
\ fault on a misaligned address, which is how a merged engine died SIGBUS in
\ FIND-B holding an odd pointer (dot habu-merged-data-window-b8fec035).
\
\ THE RESIDUE IS THE ONLY COMPARABLE FACT, and only PAIRWISE. Two engines lay
\ their DATA out at different absolute addresses, so a census that counts aligned
\ cells on each side separately says nothing at all: it reported 740 against 369
\ and concluded nothing. What has to match is each word's OWN residue, matched by
\ name and wordlist across the two processes.
\
\ WHAT IT READS, AND WHY RUNNING THE WORD IS THE HONEST READ. A record the
\ definer stamped DKIND:ADDR pushes its DATA address and returns - that is what
\ the stamp MEANS (src/habu/layout.f), and a `does>` clause clears it the instant
\ that stops being true. So entering such a word is a read and not a side effect,
\ and no move-wide chain has to be decoded to learn the address.
\
\ Run it in any engine and diff two outputs by name and wid:
\   printf 'require tools/data-residue-census.f\nrequire src/compiler/native/compiler.f\nDATA-RESIDUE:CENSUS\n' | bin/hb
\   printf 'require tools/data-residue-census.f\nDATA-RESIDUE:CENSUS\n' | <merged-engine>
\ Each row is three lines: the name, the wordlist, the address modulo eight.

require lib/errors.f
require lib/string.f
require src/habu/layout.f
require src/habu/xref.f
require src/compiler/native/dict.f

package DATA-RESIDUE

8 constant ALIGN

\ The stamp is the permission: NDICT:REC-VALUE enters only a live record whose
\ definer stamped it, and the census asks it only of a record stamped ADDR. A
\ namespace row has no body and a retired row is no longer a word.
: ADDR-WORD? ( ptr n -- bool ) {: rec:ptr :}
   rec XREF-WORDLIST XREF-NAMESPACE-WL = if false exit then
   rec XREF-RETIRED? if false exit then
   rec XREF-FLAGS DKIND:MASK and DKIND:ADDR = ;

public

: CENSUS ( -- )
   ndict@ 0 ?do
      i XREF-REC
      dup ADDR-WORD? if
         dup XREF-NAME$ type cr
         dup XREF-WORDLIST .
         NDICT:REC-VALUE ALIGN 1 - and .
      else drop then
   loop
   s" data-residue: end" type cr ;

;package
