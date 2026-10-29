\ generated-declaration-dictionary.f - reversible native dictionary owner.
\
\ A declaration retains the native dictionary record, code, and data high-waters
\ until it succeeds, and a rollback truncates every word, package record, code
\ byte, and data allocation created below its savepoint.  Declarations never
\ nest, so the savepoint is one set of three cells.
\ WIDN deliberately follows layout.f's process-wide monotonic allocation contract:
\ rollback leaves an unreferenced hole instead of reusing an identity.  Truncating
\ the dictionary records removes every lookup path to each consumed WID, while
\ monotonic non-reuse prevents stale WID-bearing state from aliasing a later list.
\
\ The code high-water is the one watermark here that is NOT private to this
\ owner: the bytes above it are handed back to the engine's code arena and
\ rewritten by whatever is compiled next, so anything holding a fact keyed to a
\ code address in that span is describing somebody else's routine afterwards.
\ Rolling it back therefore goes through src/habu/xref.f's CODE-RECLAIM, which
\ tells those holders the floor before the space is released. `ndict` and `dp`
\ need no such notice - a truncated record is simply unreachable.

package GENERATED-DECL-DICTIONARY

7174 constant E-DICTIONARY-TX
7175 constant E-DICTIONARY-CAP
\ What a declaration reject reports for each code (generated-declaration.f).
s" generated-name transaction is out of order" E-DICTIONARY-TX DECL-REJECT:EXPLAIN
s" generated-name table is full" E-DICTIONARY-CAP DECL-REJECT:EXPLAIN

variable NDICT-MARK
variable CP-MARK
PTR-VARIABLE DP-MARK

\ A literal segment opened inside the transaction outlives its rollback: DP stops
\ at the DATA floor (src/habu/layout.f DATA-FLOOR-CELL). The release high-water
\ rises to the DP this lowers from, so `allot` zeroes the reused space it hands
\ out again (src/habu/layout.f DPHW-CELL), and the `,` run ends
\ (src/habu/layout.f COMMA-RUN-CELL).
: DICTIONARY-DP! ( ptr a -- )
   data-base NULL-PTR - {: base:n :}
   data-base DP-CELL + @ base -  data-base DPHW-CELL + @ max  data-base DPHW-CELL + !
   0 data-base COMMA-RUN-CELL + !
   NULL-PTR -  data-base DATA-FLOOR-CELL + @ base + max
   data-base DP-CELL + ! ;

public

: SNAPSHOT ( -- )
   ndict@ NDICT-MARK !
   cp@ CP-MARK !
   here DP-MARK ! ;

: ROLLBACK ( -- )
   NDICT-MARK @ ndict!
   CP-MARK @ CODE-RECLAIM:TRUNCATE
   DP-MARK @ DICTIONARY-DP! ;

: PREFLIGHT ( n -- )
   dup 0 < IF E-DICTIONARY-CAP throw THEN
   DICT-CAP ndict@ - > IF E-DICTIONARY-CAP throw THEN ;

\ The code mark is a JIT address, which differs from run to run; clearing the
\ savepoint keeps it out of a captured image so successive generations match.
: SNAPSHOT-RESET ( -- )
   DECLARATION-TRANSACTION:ACTIVE? IF E-DICTIONARY-TX throw THEN
   0 NDICT-MARK !
   0 CP-MARK !
   NULL-PTR DP-MARK ! ;

private

get-current prot-wid-add

;package
