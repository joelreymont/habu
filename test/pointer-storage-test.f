\ pointer-storage-test.f - focused pointer-slot ownership and effect regression.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/fs.f
\   src/habu/verify-source.f test/pointer-storage-test.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/fs.f
require src/habu/verify-source.f
require test/checker-assert.f

package POINTER-STORAGE-TEST

$1000 constant SOURCE-CAP        \ the whole of pointer-storage.f is read in below

create SOURCE SOURCE-CAP allot
variable SOURCE-U
PTR-VARIABLE ZERO-SLOT                   \ a declared slot nothing ever writes
create TARGET 0 ,
PTR-VARIABLE SLOT
PERSISTED-PTR-VARIABLE PERSISTED-SLOT
4 PTR-U8-TABLE TABLE-SLOT
PERSISTED-PTR-U8-TABLE-VARIABLE TABLE-HEAD

: ZERO-PTR ( -- ptr n )
   ZERO-SLOT @ ;

: ADDRESS ( -- ptr ptr n )
   SLOT ;

: LOAD-SOURCE ( -- )
   s" src/core/pointer-storage.f" SOURCE SOURCE-CAP READ-ALL SOURCE-U ! ;

: HAS? ( ptr u8 n -- bool )
   SOURCE SOURCE-U @ 2swap CONTAINS? ;

: MUST-HAVE ( ptr u8 n -- )
   HAS? TTRUE ;

: MUST-LACK ( ptr u8 n -- )
   HAS? TFALSE ;

: RUNTIME ( -- )
   ADDRESS @ ZERO-PTR = TTRUE
   TARGET ADDRESS !
   ADDRESS @ TARGET = TTRUE ;

: SAMPLE ( -- ptr u8 )
   s" pointer-storage" drop ;

: TABLE-FIELD ( n -- ptr ptr u8 )
   cells TABLE-SLOT + 0 ptr-field ;

\ A declared table is ordinary storage at runtime: the base goes into the head
\ and a byte pointer round-trips through a cell of it.
: TABLE-RUNTIME ( -- )
   TABLE-SLOT TABLE-HEAD !
   TABLE-HEAD @ TABLE-SLOT = TTRUE
   SAMPLE 0 TABLE-FIELD !
   0 TABLE-FIELD @ SAMPLE = TTRUE ;

: VERIFY-EFFECT ( -- )
   [: s" PTR-VARIABLE VS-PTR : VS-ADDR ( -- ptr ptr n ) VS-PTR ;" VERIFY:SOURCE-BUF ;]
   catch 0 T=
   [: s" PERSISTED-PTR-VARIABLE VPS-PTR : VPS-ADDR ( -- ptr ptr n ) VPS-PTR ;" VERIFY:SOURCE-BUF ;]
   catch 0 T= ;

\ Raw-definer VALUE-side mint (habu-nominal-storage-raw): create/variable/constant
\ publish a RAW cell whose fetch is a TVK-RAW var, so laundering an arity-0
\ nominal family through raw storage rejects, while a plain scalar round-trip
\ through the same raw cell still certifies. The definers are registered through
\ the verify-source RAW-TRUST-NEXT path (the enforcing gate); the laundering word
\ is then checked with the quiet candidate checker so the reject renders no stray
\ diagnostic. Verdict 0 = rejected, -1 = certified.
: REG-RAW-DEFINERS ( -- )
   s\" NEWTYPE rsvfam 0\nvariable RSVV\ncreate RSVC 8 allot\n7 constant RSVK" VERIFY:SOURCE-BUF-IN-SCOPE ;
: VERIFY-RAW-VALUE ( -- )
   REG-RAW-DEFINERS
   s" RSV-VAR-MINT ( n -- rsvfam ) RSVV ! RSVV @" CHECK-QUIET-CANDIDATE! 0 T=
   s" RSV-CREATE-MINT ( n -- rsvfam ) RSVC ! RSVC @" CHECK-QUIET-CANDIDATE! 0 T=
   s" RSV-CONST-MINT ( -- rsvfam ) RSVK" CHECK-QUIET-CANDIDATE! 0 T=
   s" RSV-VAR-N ( n -- n ) RSVV ! RSVV @" CHECK-QUIET-CANDIDATE! -1 T=
   s" RSV-CONST-N ( -- n ) RSVK" CHECK-QUIET-CANDIDATE! -1 T= ;

\ Declared-pointee storage (dot habu-refuse-a-ptr-5ad2734e). PTR-U8-TABLE and
\ PERSISTED-PTR-U8-TABLE-VARIABLE spell the pointee in the does> clause, so the
\ published effect carries no type variable, `trust-raw` has nothing to seal, and
\ the cell holds EXACTLY the type named: it admits that type and refuses every
\ other one, in both directions. The raw form is the control - it keeps absorbing
\ a scalar pointee, which is all DYNAMIC-BUFFER's control head ever needs. That a
\ raw cell REFUSES a pointer pointee is the rule's own fixture
\ (test/compiler/raw-cell-pointer-refusals.f), not this file's: the rule lands
\ after these conversions, and this file must read the same before and after it.
: REG-DECLARED-CELLS ( -- )
   s\" 4 PTR-U8-TABLE DPT\nPERSISTED-PTR-U8-TABLE-VARIABLE DPH\nPTR-VARIABLE DPR"
   VERIFY:SOURCE-BUF-IN-SCOPE ;

: VERIFY-DECLARED-POINTEE ( -- )
   REG-DECLARED-CELLS
   s" DP-BASE ( -- ptr ptr u8 ) DPT" CHECK-QUIET-CANDIDATE! -1 T=
   s" DP-READ ( -- ptr ptr u8 ) DPH @" CHECK-QUIET-CANDIDATE! -1 T=
   s" DP-STORE ( ptr u8 -- ) 0 cells DPT + 0 ptr-field !" CHECK-QUIET-CANDIDATE! -1 T=
   s" DP-RAW-SCALAR ( -- ptr u8 ) DPR @" CHECK-QUIET-CANDIDATE! -1 T=
   s" DP-BASE-WRONG ( -- ptr ptr n ) DPT" CHECK-QUIET-CANDIDATE! 0 T=
   s" DP-READ-WRONG ( -- ptr u8 ) DPH @" CHECK-QUIET-CANDIDATE! 0 T=
   s" DP-STORE-WRONG ( n -- ) 0 cells DPT + 0 ptr-field !" CHECK-QUIET-CANDIDATE! 0 T=
   s" DP-HEAD-WRONG ( ptr ptr n -- ) DPH !" CHECK-QUIET-CANDIDATE! 0 T= ;

: ISOLATION ( -- )
   LOAD-SOURCE
   s" PTR-VARIABLE" MUST-HAVE
   s" PERSISTED-PTR-VARIABLE" MUST-HAVE
   s" PTR-U8-TABLE" MUST-HAVE
   s" PERSISTED-PTR-U8-TABLE-VARIABLE" MUST-HAVE
   s" +FIELD" MUST-LACK
   s" CFIELD:" MUST-LACK
   s" STRUCT-BYTE+" MUST-LACK
   s" STRUCT-ACTIVE" MUST-LACK
   s" BEGIN-STRUCTURE" MUST-LACK
   s" END-STRUCTURE" MUST-LACK
   s" parse-name" MUST-LACK ;

: RUN ( -- )
   T-RESET
   RUNTIME
   TABLE-RUNTIME
   VERIFY-EFFECT
   VERIFY-RAW-VALUE
   VERIFY-DECLARED-POINTEE
   ISOLATION
   T-REPORT ;

RUN

;package
