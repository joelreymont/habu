\ postpone-effect.f - an effect a definer builds renders as the signature its
\ built word records, naming a private family by its package, and its nodes
\ are transient: no persistent schema root takes one. A multi-error load that
\ refuses a built body keeps its effect, which names the definer's
\ structure, for a caller in another package. A word built over
\ EFFECT:READ-TYPE or EFFECT:NAMED of a type takes the type its text form
\ declares, and READ-TYPE refuses a family the running package does not know.
\ A type held on the stack or in a local across a load that builds types keeps
\ its own effect. No storage, field or payload holds one, at any depth, and a
\ file that stores one is rejected at its storage, before a requirer reads it.
\ The file calls the running checker's words, so it is loaded, never built.

require src/core/exit-status.f
require src/lib/test.f
require src/lib/adt/nullable.f
require src/lib/adt/option.f
require test/decl-diag-capture.f

\ First, so no earlier node outlives the load whose end would free the held
\ type's id: a node a later type takes would answer for it.
package POSTPONE-KEEP
private
: PK-ONE ( -- n ) 1 ;

\ : NAME ( -- n ) PK-ONE ; over effect E.
: MAKE-ONE ( EFFECT:type ^u8 n -- )
   rot define-word  postpone PK-ONE  postpone ; ;

EFFECT:EMPTY EFFECT:N EFFECT:OUT
require test/fixtures/postpone-types.f
EFFECT:EMPTY EFFECT:N EFFECT:IN drop
s" PK-STACK" MAKE-ONE

: KEEP-LOCAL ( -- )
   EFFECT:EMPTY EFFECT:N EFFECT:OUT {: t :}
   s" test/fixtures/postpone-types.f" included
   EFFECT:EMPTY EFFECT:N EFFECT:IN drop
   t s" PK-LOCAL" MAKE-ONE ;
KEEP-LOCAL

s" PK-STACK drop" TEST-EVAL:RC 0 T=
s" : PK-STACK-IN ( n -- ) PK-STACK ;" TEST-EVAL:RC 70 T=
s" PK-LOCAL drop" TEST-EVAL:RC 0 T=
s" : PK-LOCAL-IN ( n -- ) PK-LOCAL ;" TEST-EVAL:RC 70 T=
;package

package POSTPONE-CELL
public
STRUCTURE pdcell FIELD v n ;STRUCTURE
private
STRUCTURE pdhidden FIELD v n ;STRUCTURE
1 TYPED-BUFFER PD-CELLS pdcell

\ ( ^pdcell -- ^pdcell ).
: CELL-EFFECT ( -- EFFECT:type )
   s" POSTPONE-CELL:pdcell" EFFECT:NAMED EFFECT:PTR
   {: p :}  EFFECT:EMPTY p EFFECT:IN  p EFFECT:OUT ;

public
: PD-CELL ( -- ^pdcell ) 0 PD-CELLS ;

\ : NAME ( ^pdcell -- ^pdcell ) drop drop ;, an underflow.
: MAKE-CELL-BROKEN ( ^u8 n -- )
   CELL-EFFECT define-word
   postpone drop  postpone drop
   postpone ; ;
;package

package POSTPONE-EFFECT
private
STRUCTURE peslot FIELD a n ;STRUCTURE
CAST: >NODE ( EFFECT:type -- n )

: SLOT-PTR ( -- EFFECT:type )
   s" POSTPONE-EFFECT:peslot" EFFECT:NAMED EFFECT:PTR ;

\ ( ^peslot n -- ^peslot ), one pointer node in both rows.
: SLOT-EFFECT ( -- EFFECT:type )
   SLOT-PTR {: p :}
   EFFECT:EMPTY p EFFECT:IN  EFFECT:N EFFECT:IN  p EFFECT:OUT ;

\ : PE-SLOT-W ( ^peslot n -- ^peslot ) drop ; over SLOT-EFFECT.
: MAKE-SLOT ( -- )
   SLOT-EFFECT s" PE-SLOT-W" rot define-word  postpone drop  postpone ; ;

\ : PE-XT-W ( xt -- xt ) ; over an `xt` in and out.
: MAKE-XT ( -- )
   s" xt" EFFECT:READ-TYPE {: x :}
   EFFECT:EMPTY x EFFECT:IN  x EFFECT:OUT  s" PE-XT-W" rot define-word
   postpone ; ;

public
MAKE-SLOT
MAKE-XT
private

\ The built word records its effect as its signature, bare and package-qualified.
s" PE-SLOT-A ( ^peslot n -- ^peslot ) PE-SLOT-W" CHECK-QUIET-CANDIDATE! -1 T=
s" PE-SLOT-B ( ^postpone-effect:peslot n -- ^postpone-effect:peslot ) PE-SLOT-W"
CHECK-QUIET-CANDIDATE! -1 T=
s" PE-XT-A ( xt -- xt ) PE-XT-W" CHECK-QUIET-CANDIDATE! -1 T=
s" PE-XT-B ( n -- n ) PE-XT-W" CHECK-QUIET-CANDIDATE! 0 T=

\ The captured record opens with the line L and gives the transient-node help.
: SAID-BAD ( ^u8 n -- ) {: l:^ lu:n :}
   REJECTION:BUFFER$ l lu STARTS-WITH? TTRUE
   REJECTION:BUFFER$
   S\" = help: pass a node made outside transient mode\n"
   CONTAINS? TTRUE  REJECTION:BUFFER-OFF ;

: ROOT-PERSIST ( -- )
   SLOT-PTR >NODE {: node:n :}
   REJECTION:CAPTURE
   [: node SCHEMA-REG:SCHEMA-ROOT+ drop ;] EXIT-STATUS:REJECTED TTHROWSQ
   S\" error[E-SCHEMA-BAD]: `SCHEMA-REG:SCHEMA-ROOT+` was given a transient node outside transient mode\n"
   SAID-BAD ;
ROOT-PERSIST

\ Nor does a persistent pointer node.
: PTR-PERSIST ( -- )
   SLOT-PTR >NODE {: node:n :}
   REJECTION:CAPTURE
   [: node SCHEMA-REG:SCHEMA-PTR drop ;] EXIT-STATUS:REJECTED TTHROWSQ
   S\" error[E-SCHEMA-BAD]: `SCHEMA-REG:SCHEMA-PTR` was given a transient node outside transient mode\n"
   SAID-BAD ;
PTR-PERSIST

TYPED-VARIABLE EVAL-A ^u8
variable EVAL-U
: EVAL-RUN ( -- ) EVAL-A @ EVAL-U @ evaluate-closed ;
\ The refusals counted and the throw code when the text is evaluated under the
\ multi-error load state.
: MULTI ( ^u8 n -- n n )
   EVAL-U ! EVAL-A !  CHECK:MULTI-ERR-BEGIN [: EVAL-RUN ;] catch CHECK:MULTI-ERR-END ;

\ The refused built word keeps its effect: its caller certifies, one refusal.
S\" s\" CELL-BROKEN\" POSTPONE-CELL:MAKE-CELL-BROKEN\n: CELL-USE ( -- ) POSTPONE-CELL:PD-CELL CELL-BROKEN drop ;"
   MULTI 1 T= 0 T=

1 TYPED-BUFFER PE-SLOTS peslot
TYPED-VARIABLE PE-FLAG bool
CAST: N>R ( n -- r )
CAST: R>N ( r -- n )

\ : NAME ( t -- t ) ;
: MAKE-ID ( ^u8 n EFFECT:type -- ) {: t :}
   EFFECT:EMPTY t EFFECT:IN  t EFFECT:OUT  define-word
   postpone ; ;

s" PE-U32" s" u32" EFFECT:READ-TYPE MAKE-ID
s" PE-BYTES" s" ^u8" EFFECT:READ-TYPE MAKE-ID
s" PE-NUL" s" nullable<u8>" EFFECT:READ-TYPE MAKE-ID
s" PE-SLOT" s" ^peslot" EFFECT:READ-TYPE MAKE-ID
s" PE-R" s" r" EFFECT:NAMED MAKE-ID
s" PE-BOOL" s" bool" EFFECT:NAMED MAKE-ID
s" PE-U8" s" u8" EFFECT:NAMED MAKE-ID
s" PE-N" s" n" EFFECT:NAMED MAKE-ID

: PE-NUL-BYTE ( nullable<u8> -- n )
   MATCH nullable
      none OF 0 ENDOF
      some OF c@ ENDOF
   ;MATCH ;

7 PE-U32 7 T=
s" : PE-U32-NARROW ( u32 -- u8 ) PE-U32 ;" TEST-EVAL:RC 70 T=
s" ab" drop PE-BYTES c@ 97 T=
s" 0 PE-SLOTS PE-BYTES drop" TEST-EVAL:RC 70 T=
s" Q" drop NULLABLE:SOME PE-NUL PE-NUL-BYTE 81 T=
s" PE-FLAG NULLABLE:SOME PE-NUL drop" TEST-EVAL:RC 70 T=
0 PE-SLOTS PE-SLOT NULL-PTR -  0 PE-SLOTS NULL-PTR -  T=
s" POSTPONE-CELL:PD-CELL PE-SLOT drop" TEST-EVAL:RC 70 T=
5 N>R PE-R R>N 5 T=
s" 5 PE-R drop" TEST-EVAL:RC 70 T=
true PE-BOOL TTRUE
s" 5 PE-BOOL drop" TEST-EVAL:RC 70 T=
65 PE-U8 65 T=
s" : PE-U8-WIDE ( -- n ) 300 PE-U8 ;" TEST-EVAL:RC 70 T=
5 PE-N 5 T=
s" true PE-N drop" TEST-EVAL:RC 70 T=

\ POSTPONE-CELL's private family is unknown here.
: READ-HIDDEN ( -- n ) [: s" pdhidden" EFFECT:READ-TYPE drop ;] catch ;
READ-HIDDEN 70 T=
;package

\ From another package, a misuse of the built word is rejected, naming the
\ private family by its package.
DECL-DIAG:PROSE
s" PE-SLOT-C ( n n -- n ) POSTPONE-EFFECT:PE-SLOT-W" CHECK-CANDIDATE! 0 T=
s" postpone-effect:peslot" DECL-DIAG:HAS? TTRUE
DECL-DIAG:OFF

package POSTPONE-STORE
private
: REJECTED ( ^u8 n -- n ) {: a:^ u:n :}
   DECL-DIAG:JSON  [: a u evaluate-closed ;] catch ;
: REASON ( -- )
   s" a definer's type ends with the load that built it: build it where it is used"
   DECL-DIAG:HAS? TTRUE ;
: STORAGE-HELP ( -- )
   s" build the type where it is used, from its text or EFFECT's words, and pass it on the stack or in a local"
   DECL-DIAG:HAS? TTRUE ;
: STORAGE ( -- )
   S\" \"code\":\"E-BAD-STORAGE\"" DECL-DIAG:HAS? TTRUE
   REASON  STORAGE-HELP  DECL-DIAG:OFF ;
: MEMBER ( -- )
   S\" \"code\":\"E-BAD-DECLARATION\"" DECL-DIAG:HAS? TTRUE
   REASON  DECL-DIAG:OFF ;

s" TYPED-VARIABLE PS-VAR EFFECT:type" REJECTED 70 T=  STORAGE
s" 4 TYPED-BUFFER PS-BUF EFFECT:type" REJECTED 70 T=  STORAGE
s" TYPED-VARIABLE PS-OPT option<EFFECT:type>" REJECTED 70 T=  STORAGE
s" STRUCTURE psfield FIELD t EFFECT:type ;STRUCTURE" REJECTED 0 T<>
MEMBER
s" SUMTYPE pspay VARIANT one EFFECT:type ;VARIANT ;SUMTYPE" REJECTED 0 T<>
MEMBER
s" STRUCTURE psprodp FIELD t ^EFFECT:type ;STRUCTURE" REJECTED 0 T<>
MEMBER
s" ENUM psenum VARIANT one FIELD t EFFECT:type ;VARIANT ;ENUM" REJECTED 0 T<>
MEMBER
s" ENUM psenump VARIANT one FIELD t ^EFFECT:type ;VARIANT ;ENUM" REJECTED
0 T<>  MEMBER

public
\ Loaded with no package open: the fixtures open their own.
: REPRODUCE ( -- )
   DECL-DIAG:JSON
   [: s" test/fixtures/postpone-stored-use.f" included ;] catch 70 T=
   s" HELD" DECL-DIAG:HAS? TTRUE  STORAGE ;
;package

POSTPONE-STORE:REPRODUCE

T-REPORT
