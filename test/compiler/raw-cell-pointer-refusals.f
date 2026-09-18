\ raw-cell-pointer-refusals.f - the rejected programs for "a raw storage cell
\ never holds an address" (dot habu-refuse-a-ptr-5ad2734e).
\
\ WHAT WAS WRONG. A checked word could read and write memory at any integer
\ address with no TRUST row by passing the integer through an undeclared cell:
\ `variable V  : PEEK ( n -- n ) V ! V @ @ ;` certified and ran, because every
\ mention of a raw storage word re-freshens its published `-- ptr a`, so the `n`
\ one mention stored came back from the next as a value `@` and `!` accepted as
\ an address. `create`, `constant` and `here` published the same scheme, `c@`
\ took the same value as a BYTE address, and `0 ptr-field` laundered the whole
\ cell into a fully typed `ptr ptr b` that even forged nominal identities the
\ direct fetch is refused (the seal in test/raw-storage-load-seal-test.f).
\
\ WHAT IS PINNED HERE. Every one of those shapes is refused, and refused with
\ its OWN named diagnostic rather than a bare type mismatch: the code, and the
\ prose that says what was laundered. Two controls stand beside them, because a
\ rule that also refuses honest code is not this rule: a `create`d byte buffer
\ handed to `type` still certifies (the raw pointee binds to the `u8` con), and
\ a TASK:+USER slot - concrete `-- ptr n`, never a raw cell - keeps being
\ refused for the reason it always was, WITHOUT the new code.
\
\ These run against the live engine's own checker through `evaluate`, which is
\ the path `bin/hb --load` puts every definition through.
require lib/test.f
require lib/string.f
require lib/task.f

\ The cells the evaluated programs name. They are file-global because the
\ programs below are compiled at run time, in the scope a real program writes
\ them in.
variable RCP-V
create RCP-C 8 allot
$1234 constant RCP-K
create RCP-BYTES 256 allot
TASK:#USER CELL TASK:+USER RCP-SLOT drop

\ The nominal identity the field door used to forge. test/snapshot-xt-cell-decl.f
\ keeps its own subject through a declared handle; the checked twin of its FORGE
\ is CASE-FIELD-FORGE-NOMINAL below.
NEWTYPE rcpthing 0

package RAW-CELL-PTR-TEST

private

\ `evaluate` is the metaprogramming boundary the checker does not model, and it
\ is how a suite compiles a program that must be REFUSED: the refusal is a throw
\ out of the compile, not a value.
\ Retirement owner: habu-type-isolated-dynamic-244c0e2c.
TRUSTED: EV ( ptr u8 n -- ) evaluate ;

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

: HAS? ( ptr u8 n -- )
   DIAG-BUFFER$ 2swap CONTAINS? TTRUE ;

: LACKS? ( ptr u8 n -- )
   DIAG-BUFFER$ 2swap CONTAINS? TFALSE ;

\ Every value-position refusal answers one code, one repair class and one
\ reason. Asserting the reason and not only the code is what keeps the prose
\ from silently becoming a different rule's prose.
: NAMED ( -- )
   CODE$ HAS?  REPAIR$ HAS?  VALUE-REASON$ HAS? ;

\ ---- the pun itself: store an integer, fetch an address ----------------------

: CASE-VARIABLE ( -- )
   s" a plain variable cannot answer an address it was never given" T-LABEL
   ARM
   [: s" : RCP-PEEK ( n -- n ) RCP-V ! RCP-V @ @ ;" EV ;] CHECK-RC TTHROWSQ
   NAMED
   DISARM ;

: CASE-VARIABLE-STORE ( -- )
   s" and the write half of the same pun is refused at the store" T-LABEL
   ARM
   [: s" : RCP-POKE ( n n -- ) RCP-V ! RCP-V @ ! ;" EV ;] CHECK-RC TTHROWSQ
   NAMED
   DISARM ;

: CASE-CREATE ( -- )
   s" a create'd cell is the same cell and gets the same answer" T-LABEL
   ARM
   [: s" : RCP-CPEEK ( n -- n ) RCP-C ! RCP-C @ @ ;" EV ;] CHECK-RC TTHROWSQ
   NAMED
   DISARM ;

\ A constant needs no store at all: its own published value is the laundered
\ one, so this is the shortest form of the forgery.
: CASE-CONSTANT ( -- )
   s" a constant's value is not an address either, with no store in sight" T-LABEL
   ARM
   [: s" : RCP-KPEEK ( -- n ) RCP-K @ ;" EV ;] CHECK-RC TTHROWSQ
   NAMED
   DISARM ;

: CASE-HERE ( -- )
   s" here's cell is raw storage and answers the same refusal" T-LABEL
   ARM
   [: s" : RCP-HPEEK ( n -- n ) here ! here @ @ ;" EV ;] CHECK-RC TTHROWSQ
   NAMED
   DISARM ;

\ The byte door is the same door: `c@` wants a `ptr u8`, and a value out of a
\ raw cell is no more a byte address than it is a cell address.
: CASE-BYTE-FETCH ( -- )
   s" the byte view of the pun is refused where the cell view is" T-LABEL
   ARM
   [: s" : RCP-BPEEK ( n -- u8 ) RCP-V ! RCP-V @ c@ ;" EV ;] CHECK-RC TTHROWSQ
   NAMED
   DISARM ;

\ The other direction. Refusing the fetch alone would leave the cell holding an
\ address for some other mention to read back.
: CASE-POINTER-IN ( -- )
   s" and a real pointer cannot be put into a plain cell in the first place" T-LABEL
   ARM
   [: s" : RCP-PSTORE ( ptr a -- ) RCP-V ! ;" EV ;] CHECK-RC TTHROWSQ
   NAMED
   DISARM ;

\ ---- the field door ----------------------------------------------------------
\ `ptr-field`'s row is `ptr a n -- ptr ptr b` with `b` free, so the field of a
\ raw cell used to answer a fully typed pointer-to-pointer with no relation to
\ the cell. The value-position rule above never sees that pointer, which is why
\ the base has to be refused instead. It is refused at the `ptr-field` token, so
\ the shape below is the whole bypass whatever the result is then used as -- the
\ nominal-forgery version of it is pinned in test/raw-storage-load-seal-test.f,
\ where a candidate check can name the family.

: CASE-FIELD-FORGE ( -- )
   s" the field of a raw cell is refused, so the seal cannot be walked around" T-LABEL
   ARM
   [: s" : RCP-FORGE ( n -- n ) RCP-V ! RCP-V 0 ptr-field @ @ ;" EV ;]
      CHECK-RC TTHROWSQ
   CODE$ HAS?  REPAIR$ HAS?  FIELD-REASON$ HAS?
   DISARM ;

\ The same program with a NOMINAL result: the forgery the seal in
\ test/raw-storage-load-seal-test.f exists to stop, written through the field
\ instead of through the direct fetch. It is refused at the SAME token and by
\ the same code as the scalar twin above -- the base is judged before the row is
\ applied, so what the forged pointer is then read AS never enters into it.
: CASE-FIELD-FORGE-NOMINAL ( -- )
   s" and the nominal forgery through that field is refused at the same token" T-LABEL
   ARM
   [: s" : RCP-FORGE-N ( n -- rcpthing ) RCP-V ! RCP-V 0 ptr-field @ @ ;" EV ;]
      CHECK-RC TTHROWSQ
   CODE$ HAS?  REPAIR$ HAS?  FIELD-REASON$ HAS?
   DISARM ;

\ ---- the refusal that lands on the signature ---------------------------------
\ Not every raw-cell refusal has a token to sit on. Every token here accepts the
\ value the cell hands out -- `@` takes `ptr a` and answers `a` -- and only the
\ DECLARED output row refuses it as an address. The reason still has to be the
\ raw-cell one rather than a bare `expected: ptr u8 actual: a`, so the naming
\ has to survive a rejection raised after the last token was checked.
: CASE-SIGNATURE-BOUNDARY ( -- )
   s" the declared output row refuses it too, and names the same rule" T-LABEL
   ARM
   [: s" : RCP-VBASE ( -- ptr u8 ) RCP-V @ ;" EV ;] CHECK-RC TTHROWSQ
   NAMED
   DISARM ;

\ ---- the controls ------------------------------------------------------------

\ A `create`d byte buffer is the honest use of raw storage: the pointee binds to
\ the `u8` con, which is a scalar and stays admissible. If this ever stops
\ certifying, the rule has grown past its own statement.
: CASE-BYTE-BUFFER ( -- )
   s" a create'd byte buffer still certifies through type" T-LABEL
   [: s" : RCP-SHOW ( -- ) RCP-BYTES 4 type ;" EV ;] 0 TTHROWSQ ;

\ The latch is raised inside the unifier, which means it can be raised by a row
\ that is then thrown away: TRY-PRIMS applies each candidate in turn, so
\ `V @ cell+` refuses the `ptr a -- ptr a` row and then succeeds on `n -- n`.
\ Whatever rejects the definition AFTERWARDS must keep its own reason -- a
\ refusal that borrowed this one would send the reader to a cell that was never
\ the problem.
: CASE-ABANDONED-CANDIDATE ( -- )
   s" a candidate row that was abandoned does not name the real refusal" T-LABEL
   ARM
   [: s" : RCP-SPEC ( -- n ) RCP-V @ cell+ 0= ;" EV ;] CHECK-RC TTHROWSQ
   CODE$ LACKS?  VALUE-REASON$ LACKS?
   DISARM ;

\ A TASK:+USER slot publishes a CONCRETE `-- ptr n`, so it was never part of
\ this pun and its refusal is the ordinary one it always gave. It is here to
\ prove the new code names the new rule and nothing else.
: CASE-TASK-SLOT ( -- )
   s" a TASK:+USER slot is refused as before, and not by the new rule" T-LABEL
   ARM
   [: s" : RCP-SPEEK ( -- n ) RCP-SLOT @ @ ;" EV ;] CHECK-RC TTHROWSQ
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
   CASE-ABANDONED-CANDIDATE
   CASE-BYTE-BUFFER
   CASE-TASK-SLOT
   T-REPORT ;

;package

RAW-CELL-PTR-TEST:RUN
