\ generated-declaration-dictionary.f - a declaration's dictionary savepoint
\ (record count, code and data high-waters), rolled back on reject. Declarations
\ never nest, so the savepoint is three cells. WIDN is never rolled back
\ (layout.f): a reused wordlist id would alias stale state. Code rollback goes
\ through src/habu/xref.f CODE-RECLAIM, which tells holders of code-address facts
\ the floor before the space is reused; a truncated record needs no notice.

package GENERATED-DECL-DICTIONARY

7175 constant E-DICTIONARY-CAP
\ What a declaration reject reports for the code (generated-declaration.f).
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
   {: mark:ptr :}
   data-base NULL-PTR - {: base:n :}
   mark NULL-PTR -  data-base DATA-FLOOR-CELL + @ base +  max {: cut:n :}
   data-base DP-CELL + @ base -  data-base DPHW-CELL + @ max  data-base DPHW-CELL + !
   0 data-base COMMA-RUN-CELL + !
   cut INTERN:KEEP-BELOW
   cut data-base DP-CELL + ! ;

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

private

seal-package

;package
