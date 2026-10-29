\ raw-cell-pointer-refusals.f - rejected programs for "a raw storage cell never
\ holds an address". Every mention of a raw storage word (variable, create,
\ constant, here) re-freshens its published `-- ptr a`, so an `n` stored through
\ one mention would come back from the next as an address `@`, `c@`, `!` or
\ `0 ptr-field` accepts.
require src/lib/test.f
require src/lib/string.f
require src/lib/task.f

\ File-global: the programs below compile at run time, in the scope a real
\ program writes them in.
variable RCP-V
create RCP-C 8 allot
$1234 constant RCP-K
create RCP-BYTES 256 allot
TASK:#USER CELL TASK:+USER RCP-SLOT drop

\ DECLARED code cells for the executable-value controls: TYPED-VARIABLE /
\ TYPED-BUFFER over `[ in -- out ]` mints the quotation type at the storage
\ word, so nothing may write the cell at another type.
TYPED-VARIABLE RCP-HK [ -- n ]
2 TYPED-BUFFER RCP-HKB [ -- n ]

\ The nominal identity CASE-FIELD-FORGE-NOMINAL tries to forge.
\ test/snapshot-xt-cell-decl.f keeps its own subject through a declared handle.
NEWTYPE rcpthing

package RAW-CELL-PTR-TEST

private

70 constant CHECK-RC                 \ the engine refusing a definition it cannot certify

create DIAG-BUF 8192 allot
8192 constant DIAG-CAP

: ARM ( -- )   \ capture the next diagnostic as JSON instead of printing it
   DIAG-BUF DIAG-CAP DIAG-BUFFER!  true DIAG-JSON! ;

: DISARM ( -- )
   false DIAG-JSON!  DIAG-BUFFER-OFF ;

: CODE$ ( -- ptr u8 n )
   S\" \"code\":\"E-RAW-CELL-PTR\"" ;

: VALUE-REASON$ ( -- ptr u8 n )
   S\" \"reason\":\"raw storage cell: a pointer cannot be stored in or fetched from an undeclared cell\"" ;

: FIELD-REASON$ ( -- ptr u8 n )
   S\" \"reason\":\"ptr-field: base is an undeclared raw storage cell, not a declared pointer cell\"" ;

: REPAIR$ ( -- ptr u8 n )
   S\" \"repair_class\":\"declare_pointer_cell\"" ;

\ An EXECUTABLE value through the same cell answers the same code with its own
\ reason and repair class: a declared CODE cell, not a declared pointer cell.
: EXEC-REASON$ ( -- ptr u8 n )
   S\" \"reason\":\"raw storage cell: an undeclared cell cannot hold an execution token / a quotation\"" ;

: EXEC-REPAIR$ ( -- ptr u8 n )
   S\" \"repair_class\":\"declare_xt_cell\"" ;

: RETURN-REPAIR$ ( -- ptr u8 n )
   S\" \"repair_class\":\"fix_return_stack\"" ;

: HAS? ( ptr u8 n -- )
   DIAG-BUFFER$ 2swap CONTAINS? TTRUE ;

: LACKS? ( ptr u8 n -- )
   DIAG-BUFFER$ 2swap CONTAINS? TFALSE ;

\ Each shape is refused with its own named diagnostic (the code and the prose
\ naming what was laundered), not a bare mismatch.
\ Every value-position refusal answers one code, one repair class and one
\ reason. Asserting the reason keeps the prose from silently becoming a
\ different rule's prose.
: NAMED ( -- )
   CODE$ HAS?  REPAIR$ HAS?  VALUE-REASON$ HAS? ;

: NAMED-EXEC ( -- )
   CODE$ HAS?  EXEC-REPAIR$ HAS?  EXEC-REASON$ HAS?
   VALUE-REASON$ LACKS?  REPAIR$ LACKS? ;

\ The cases run the live engine's checker through `evaluate-closed`; a refusal
\ is a throw out of the compile, not a value.
: CASE-VARIABLE ( -- )
   s" a plain variable cannot answer an address it was never given" T-LABEL
   ARM
   [: s" : RCP-PEEK ( n -- n ) RCP-V ! RCP-V @ @ ;" evaluate-closed ;] CHECK-RC TTHROWSQ
   NAMED
   DISARM ;

: CASE-VARIABLE-STORE ( -- )
   s" and the write half of the same pun is refused at the store" T-LABEL
   ARM
   [: s" : RCP-POKE ( n n -- ) RCP-V ! RCP-V @ ! ;" evaluate-closed ;] CHECK-RC TTHROWSQ
   NAMED
   DISARM ;

: CASE-CREATE ( -- )
   s" a create'd cell is the same cell and gets the same answer" T-LABEL
   ARM
   [: s" : RCP-CPEEK ( n -- n ) RCP-C ! RCP-C @ @ ;" evaluate-closed ;] CHECK-RC TTHROWSQ
   NAMED
   DISARM ;

\ A constant needs no store at all: its own published value is the laundered
\ one, so this is the shortest form of the forgery.
: CASE-CONSTANT ( -- )
   s" a constant's value is not an address either, with no store in sight" T-LABEL
   ARM
   [: s" : RCP-KPEEK ( -- n ) RCP-K @ ;" evaluate-closed ;] CHECK-RC TTHROWSQ
   NAMED
   DISARM ;

: CASE-HERE ( -- )
   s" here's cell is raw storage and answers the same refusal" T-LABEL
   ARM
   [: s" : RCP-HPEEK ( n -- n ) here ! here @ @ ;" evaluate-closed ;] CHECK-RC TTHROWSQ
   NAMED
   DISARM ;

\ The byte door is the same door: `c@` wants a `ptr u8`, and a value out of a
\ raw cell is no more a byte address than it is a cell address.
: CASE-BYTE-FETCH ( -- )
   s" the byte view of the pun is refused where the cell view is" T-LABEL
   ARM
   [: s" : RCP-BPEEK ( n -- u8 ) RCP-V ! RCP-V @ c@ ;" evaluate-closed ;] CHECK-RC TTHROWSQ
   NAMED
   DISARM ;

\ The other direction. Refusing the fetch alone would leave the cell holding an
\ address for some other mention to read back.
: CASE-POINTER-IN ( -- )
   s" and a real pointer cannot be put into a plain cell in the first place" T-LABEL
   ARM
   [: s" : RCP-PSTORE ( ptr a -- ) RCP-V ! ;" evaluate-closed ;] CHECK-RC TTHROWSQ
   NAMED
   DISARM ;

\ The field door: `ptr-field`'s row is `ptr a n -- ptr ptr b` with `b` free, and
\ the value-position rule never sees that pointer, so the raw base is refused at
\ the `ptr-field` token whatever the result is then used as.

: CASE-FIELD-FORGE ( -- )
   s" the field of a raw cell is refused, so the seal cannot be walked around" T-LABEL
   ARM
   [: s" : RCP-FORGE ( n -- n ) RCP-V ! RCP-V 0 ptr-field @ @ ;" evaluate-closed ;]
      CHECK-RC TTHROWSQ
   CODE$ HAS?  REPAIR$ HAS?  FIELD-REASON$ HAS?
   DISARM ;

\ The same program with a NOMINAL result, the forgery the seal in
\ test/raw-storage-load-seal-test.f stops, written through the field: refused at
\ the same token by the same code, since the base is judged before the row is
\ applied.
: CASE-FIELD-FORGE-NOMINAL ( -- )
   s" and the nominal forgery through that field is refused at the same token" T-LABEL
   ARM
   [: s" : RCP-FORGE-N ( n -- rcpthing ) RCP-V ! RCP-V 0 ptr-field @ @ ;" evaluate-closed ;]
      CHECK-RC TTHROWSQ
   CODE$ HAS?  REPAIR$ HAS?  FIELD-REASON$ HAS?
   DISARM ;

\ A refusal with no token to sit on: every token accepts the value (`@` takes
\ `ptr a`, answers `a`) and only the DECLARED output row refuses it, after the
\ last token was checked. The reason is still the raw-cell one.
: CASE-SIGNATURE-BOUNDARY ( -- )
   s" the declared output row refuses it too, and names the same rule" T-LABEL
   ARM
   [: s" : RCP-VBASE ( -- ptr u8 ) RCP-V @ ;" evaluate-closed ;] CHECK-RC TTHROWSQ
   NAMED
   DISARM ;

\ The RETURN row is judged by the same unifier and named the same way: `>r`
\ moves any one cell, and a branch join judges the arms' return rows against
\ each other, one carrying the raw cell's value, the other the TASK slot's real
\ address.
: CASE-RETURN-JOIN ( -- )
   s" a branch join on the return row names it as well" T-LABEL
   ARM
   [: s" : RCP-VJOIN ( n -- ) 0= IF RCP-V @ >r ELSE RCP-SLOT >r THEN r> drop ;" evaluate-closed ;]
      CHECK-RC TTHROWSQ
   NAMED
   DISARM ;

\ The executable value: with `variable ZQW  : ZQWQ ( -- ptr [ -- n ] ) ZQW ;  :
\ I1 ( -- n ) ZQWQ @ execute ;`, `1 ZQW !  I1` would branch to address 1. The
\ fetched value carries a declared quotation type, so the opaque-execute rule
\ (which judges opacity, not provenance) never fires; the refusal lands on the
\ accessor, so the cell never gets a code type.

: CASE-VARIABLE-AS-XT-CELL ( -- )
   s" a plain variable cannot be declared to hold an execution token" T-LABEL
   ARM
   [: s" : RCP-QCELL ( -- ptr [ -- n ] ) RCP-V ;" evaluate-closed ;] CHECK-RC TTHROWSQ
   NAMED-EXEC
   DISARM ;

: CASE-CREATE-AS-XT-CELL ( -- )
   s" a create'd cell gets the same answer" T-LABEL
   ARM
   [: s" : RCP-QCELLC ( -- ptr [ -- n ] ) RCP-C ;" evaluate-closed ;] CHECK-RC TTHROWSQ
   NAMED-EXEC
   DISARM ;

\ The write half. Refusing the accessor alone would leave `!` free to put a
\ quotation into the cell for a later mention to read back.
: CASE-QUOTATION-STORE ( -- )
   s" a quotation cannot be stored into an undeclared cell either" T-LABEL
   ARM
   [: s" : RCP-QPUT ( -- ) [: 1 ;] RCP-V ! ;" evaluate-closed ;] CHECK-RC TTHROWSQ
   NAMED-EXEC
   DISARM ;

: CASE-OPAQUE-XT-STORE ( -- )
   s" a compiled opaque tick cannot enter raw storage" T-LABEL
   ARM
   [: s" : RCP-OPAQUEPUT ( -- ) ['] execute RCP-V ! ;" evaluate-closed ;]
      CHECK-RC TTHROWSQ
   NAMED-EXEC
   DISARM ;

\ Control for the return-row case: a return row that does not balance keeps the
\ return-stack repair class and stays unnamed.
: CASE-RETURN-BALANCE ( -- )
   s" an ordinary return-stack mismatch is still not named by this rule" T-LABEL
   ARM
   [: s" : RCP-RBAL ( -- ) 1 >r ;" evaluate-closed ;] CHECK-RC TTHROWSQ
   CODE$ LACKS?  VALUE-REASON$ LACKS?  RETURN-REPAIR$ HAS?
   DISARM ;

\ A `create`d byte buffer is the honest use of raw storage: the pointee binds to
\ the scalar `u8` con.
\ Controls: a `create`d byte buffer handed to `type` certifies; a TASK:+USER
\ slot keeps its ordinary refusal.
: CASE-BYTE-BUFFER ( -- )
   s" a create'd byte buffer still certifies through type" T-LABEL
   [: s" : RCP-SHOW ( -- ) RCP-BYTES 4 type ;" evaluate-closed ;] 0 TTHROWSQ ;

\ The latch is raised inside the unifier, so a row that is then thrown away can
\ raise it: TRY-PRIMS tries each candidate, and `V @ cell+` refuses the `ptr a
\ -- ptr a` row then succeeds on `n -- n`. A later rejection keeps its own
\ reason.
: CASE-ABANDONED-CANDIDATE ( -- )
   s" a candidate row that was abandoned does not name the real refusal" T-LABEL
   ARM
   [: s" : RCP-SPEC ( -- n ) RCP-V @ cell+ 0= ;" evaluate-closed ;] CHECK-RC TTHROWSQ
   CODE$ LACKS?  VALUE-REASON$ LACKS?
   DISARM ;

\ The DECLARED code cells. A rule that also refused these would refuse every
\ hook the engine ships: the storage word mints the quotation type, so the cell
\ is a code cell by declaration and its fetched value is executed with a known
\ row.
: CASE-TYPED-VARIABLE-XT ( -- )
   s" a TYPED-VARIABLE code cell still certifies, and is still executed" T-LABEL
   [: s" : RCP-HKACC ( -- ptr [ -- n ] ) RCP-HK ;" evaluate-closed ;] 0 TTHROWSQ
   [: s" : RCP-HKRUN ( -- n ) RCP-HK @ execute ;" evaluate-closed ;] 0 TTHROWSQ
   [: s" : RCP-HKPUT ( -- ) [: 1 ;] RCP-HK ! ;" evaluate-closed ;] 0 TTHROWSQ ;

: CASE-TYPED-BUFFER-XT ( -- )
   s" and a TYPED-BUFFER of hooks certifies the same way" T-LABEL
   [: s" : RCP-HKBRUN ( -- n ) 0 RCP-HKB @ execute ;" evaluate-closed ;] 0 TTHROWSQ ;

: CASE-DEFER-IS ( -- )
   s" defer/is is the other declared code cell and is untouched" T-LABEL
   [: s" defer RCP-DF ( -- n )" evaluate-closed ;] 0 TTHROWSQ
   [: s" : RCP-DFB ( -- ) [: 7 ;] is RCP-DF ;" evaluate-closed ;] 0 TTHROWSQ
   [: s" : RCP-DFC ( -- n ) RCP-DF ;" evaluate-closed ;] 0 TTHROWSQ ;

\ `xt!` is the sanctioned mint: the one prim whose purpose is to declare that
\ the cell it writes holds an execution token (the engine installs its persisted
\ callbacks with it, at an address worked out from a table base and a row
\ index). Its token opens the window, and it is the only token that does.
: CASE-XT-STORE ( -- )
   s" xt! declares the cell it writes, so it keeps certifying" T-LABEL
   [: s" : RCP-XTPUT ( -- ) [: 1 ;] RCP-V xt! ;" evaluate-closed ;] 0 TTHROWSQ
   [: s" : RCP-XTPUTD ( -- ) [: 1 ;] data-base 8 + xt! ;" evaluate-closed ;] 0 TTHROWSQ ;

\ The window closes with the token: the next mention of the same cell is judged
\ by the rule again.
: CASE-XT-WINDOW-CLOSES ( -- )
   s" and the window it opens does not outlive its own token" T-LABEL
   ARM
   [: s" : RCP-XTTHEN ( -- ptr [ -- n ] ) [: 1 ;] RCP-V xt!  RCP-V ;" evaluate-closed ;]
      CHECK-RC TTHROWSQ
   NAMED-EXEC
   DISARM ;

\ A TASK:+USER slot publishes a concrete `-- ptr n`: its refusal is the ordinary
\ one, without the raw-cell code.
: CASE-TASK-SLOT ( -- )
   s" a TASK:+USER slot is refused as before, and not by the new rule" T-LABEL
   ARM
   [: s" : RCP-SPEEK ( -- n ) RCP-SLOT @ @ ;" evaluate-closed ;] CHECK-RC TTHROWSQ
   CODE$ LACKS?  VALUE-REASON$ LACKS?
   DISARM ;

public

: RUN ( -- )
   T-RESET
   CASE-VARIABLE
   CASE-VARIABLE-STORE
   CASE-CREATE
   CASE-CONSTANT
   CASE-HERE
   CASE-BYTE-FETCH
   CASE-POINTER-IN
   CASE-FIELD-FORGE
   CASE-FIELD-FORGE-NOMINAL
   CASE-SIGNATURE-BOUNDARY
   CASE-RETURN-JOIN
   CASE-RETURN-BALANCE
   CASE-ABANDONED-CANDIDATE
   CASE-VARIABLE-AS-XT-CELL
   CASE-CREATE-AS-XT-CELL
   CASE-QUOTATION-STORE
   CASE-OPAQUE-XT-STORE
   CASE-BYTE-BUFFER
   CASE-TYPED-VARIABLE-XT
   CASE-TYPED-BUFFER-XT
   CASE-DEFER-IS
   CASE-XT-STORE
   CASE-XT-WINDOW-CLOSES
   CASE-TASK-SLOT
   T-REPORT ;

;package

RAW-CELL-PTR-TEST:RUN
