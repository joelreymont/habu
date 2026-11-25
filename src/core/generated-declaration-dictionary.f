split-str
\ generated-declaration-dictionary.f - a declaration's dictionary savepoint
\ (record count, code and data high-waters), rolled back on reject. Declarations
\ never nest, so the savepoint is three cells. WIDN is never rolled back
\ (layout.f): a reused wordlist id would alias stale state. Code rollback goes
\ through src/habu/xref.f CODE-RECLAIM, which tells holders of code-address facts
\ the floor before the space is reused; a truncated record needs no notice.

package GENERATED-DECL-DICTIONARY

variable NDICT-MARK
variable CP-MARK
PTR-VARIABLE DP-MARK

EXPORT BOOT:data-base
EXPORT CODE-RECLAIM:TRUNCATE
EXPORT XREF:RETIRE-IF-TASKS

\ Rewinds DATA to cut, in DP-CELL's units, raising the release high-water to a
\ DP it lowers, so the build rejects the data given back.
: DATA-REWIND ( n -- )
   {: cut:n :}
   data-base NULL-PTR - {: base:n :}
   cut base - ADDRESS-CELLS:KEEP-BELOW
   cut INTERN:KEEP-BELOW
   data-base DP-CELL + @ {: from:n :}
   from cut > if
      from base -  data-base DPHW-CELL + @ max  data-base DPHW-CELL + !
   then
   0 data-base COMMA-RUN-CELL + !
   cut data-base DP-CELL + ! ;

public

: SNAPSHOT ( -- )
   ndict@ NDICT-MARK !
   cp@ CP-MARK !
   here DP-MARK ! ;

: ROLLBACK ( -- )
   NDICT-MARK @ RETIRE-IF-TASKS IF EXIT THEN
   NDICT-MARK @ ndict!
   CP-MARK @ TRUNCATE
   \ A literal segment opened inside the transaction outlives its rollback: DP
   \ stops at the DATA floor.
   data-base NULL-PTR - {: base:n :}
   DP-MARK @ NULL-PTR -  data-base DATA-FLOOR-CELL + @ base +  max
   DATA-REWIND ;

\ Rejects a declaration whose first word is a u when its n records do not fit,
\ placed at the token read last, where the declaration ran out of room.
: PREFLIGHT ( str n -- ) {: a:^ u:n n:n :}
   n DICT-CAP ndict@ - > IF a u SRCLOC:TOKEN$ REJECTION:DICTIONARY-FULL THEN ;

private

seal-package

;package
