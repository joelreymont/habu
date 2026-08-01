\ builder.f - the linear transactional builder for the immutable nominal
\ collection (dot habu-add-immutable-nominal-9290a81f).
\
\ NOM:NEW opens the single live transaction and returns a LINEAR `nom-builder`
\ token. The checker enforces the linearity: the token cannot be duplicated,
\ dropped, or stored, and only NOM:ADD (thread), NOM:FREEZE (commit), and
\ NOM:ROLLBACK (abort) may consume it - so a builder cannot be reused after it is
\ frozen and a half-built transaction cannot silently escape. ADD appends binding
\ indices to a transient scratch arena (append-only, O(1)); FREEZE sorts once
\ (O(n log n)), collapses full-row duplicates, fails closed on a path/slot
\ conflict, and routes the sorted-unique chunk through PUBLISH-CHUNK. Any FREEZE
\ failure and every ROLLBACK restore the scratch and chunk high-water marks and
\ clear the open flag, so no partial state is ever published.
\
\ The resource budget (NOM:BUDGET!) caps bindings PER TRANSACTION and is a policy
\ knob wholly separate from semantic capacity: a row may hold far more than the
\ old 25-binding by-value limit; only the configured budget, arena overflow, or
\ allocator failure stops it.
\
\ Load after lib/nominal/row.f and lib/sort.f. Reopens package NOM.

require lib/nominal/row.f
require lib/sort.f

$100000 constant NOM-DEFAULT-BUDGET         \ per-transaction binding budget (resource, not capacity)

DEFLINEAR nom-builder

package NOM
private

create NOM-SCR 4 cells allot                \ transient unsorted binding-index scratch
variable NOM-OPEN                            \ 1 while a transaction is live
variable NOM-BUDGET
variable NOM-M-CHUNK                         \ chunk high-water captured at NEW
variable FRZ-PREV
variable FRZ-ROW
variable ADD-B                               \ stashed binding index across the ADD guard

\ NEW/ADD alone mint the transaction token; ADD/FREEZE/ROLLBACK consume it where
\ the checker cannot express linear discard. Retirement owner:
\ habu-epic-type-system-b88c9ecc.
TRUSTED: MK-BUILDER ( -- nom-builder )   0 ;   \ audited linear-token mint
TRUSTED: BUILDER-DROP ( nom-builder -- )   drop ;   \ audited linear-token consume

: SCR@ ( n -- n ) {: i:n :}   NOM-SCR i AR-CELL@ ;

: FRZ-ONE ( n n -- ) {: start:n cur:n :}    \ push cur unless a full duplicate; conflict throws
   FRZ-PREV @ 0 < if cur CHUNK+ drop cur FRZ-PREV ! exit then
   FRZ-PREV @ cur BIND-PATH-EQ? if
      FRZ-PREV @ cur BIND-SLOT-EQ? if exit then
      start CHUNK-TRUNC E-NOM-CONFLICT throw
   then
   cur CHUNK+ drop  cur FRZ-PREV ! ;

: COMMIT-SORTED ( -- n n )                  \ dedup the sorted scratch into a fresh chunk
   CHUNK-MARK {: start:n :}
   -1 FRZ-PREV !
   NOM-SCR AR-USED@ 0 ?do  start i SCR@ FRZ-ONE  loop
   start  CHUNK-USED start - ;

: FREEZE-CORE ( -- )
   NOM-SCR AR-BASE@ NOM-SCR AR-USED@ [: BIND-LESS? ;] SORT:SORT!
   COMMIT-SORTED PUBLISH-CHUNK ROW-IDX FRZ-ROW ! ;

: FREEZE-BUILD ( -- row )
   [: FREEZE-CORE ;] catch {: code:n :}
   NOM-SCR AR-RESET
   0 NOM-OPEN !
   code 0 <> if code throw then
   FRZ-ROW @ ROW-BY-IDX ;

: ROLLBACK-DO ( -- )
   NOM-SCR AR-RESET
   NOM-M-CHUNK @ CHUNK-TRUNC
   0 NOM-OPEN ! ;

: ADD-CORE ( -- )                           \ append the stashed binding; may throw
   NOM-OPEN @ 0= if E-NOM-OPEN throw then
   NOM-SCR AR-USED@ NOM-BUDGET @ >= if E-NOM-BUDGET throw then
   ADD-B @ BIND-CHECK-IDX NOM-SCR AR-PUSH drop ;

: NOM-BUILDER-INIT ( -- )
   NOM-SCR AR-INIT
   0 NOM-OPEN !
   NOM-DEFAULT-BUDGET NOM-BUDGET ! ;

public

: BUDGET! ( n -- ) {: v:n :}
   v 0 < if E-NOM-BUDGET throw then
   v NOM-BUDGET ! ;
: BUDGET@ ( -- n )   NOM-BUDGET @ ;

: NEW ( -- nom-builder )
   NOM-OPEN @ if E-NOM-OPEN throw then
   NOM-SCR AR-RESET
   CHUNK-MARK NOM-M-CHUNK !
   1 NOM-OPEN !
   MK-BUILDER ;

: ADD ( nom-builder binding -- nom-builder ) {: b:binding :}
   BUILDER-DROP
   b BIND-IDX ADD-B !
   [: ADD-CORE ;] catch {: code:n :}
   code 0 <> if ROLLBACK-DO code throw then   \ any ADD error auto-aborts the transaction
   MK-BUILDER ;

: FREEZE ( nom-builder -- row )
   BUILDER-DROP
   NOM-OPEN @ 0= if E-NOM-OPEN throw then
   FREEZE-BUILD ;

: ROLLBACK ( nom-builder -- )
   BUILDER-DROP
   NOM-OPEN @ 0= if E-NOM-OPEN throw then
   ROLLBACK-DO ;

: RESET ( -- )                              \ (re)initialise every arena; resets process-local pointers
   NOM-PATH-INIT
   NOM-BIND-INIT
   NOM-CODEC-INIT
   NOM-ROW-INIT
   NOM-BUILDER-INIT ;

private
NOM-BUILDER-INIT
;package
