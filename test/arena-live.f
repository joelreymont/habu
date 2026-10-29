\ Child of test/arena.f: a top-level name too long for its family is refused on
\ stderr, and counted with nothing defined under a multi-error load; checked
\ code at top level reaches no head accessor and no typed access copies an
\ owner (test/arena.f refuses the casts; address arithmetic still reaches a
\ head), and it can neither call nor tick the arena runtime words outside
\ ARENA's generated words; a fetch through a family missing its argument is
\ refused for its arity; a package's
\ own `field-project` is refused at its definition; capture refuses a live
\ arena declared at top level and one declared in a package, each named on
\ stderr, and admits both released.
require src/lib/string.f
require src/lib/image-lifecycle.f

STRUCTURE top-rec DERIVE addr FIELD x n ;STRUCTURE
ARENA TOP-RECS top-rec
TYPED-VARIABLE TOP-POOL top-recs

\ A top-level name whose constructor package, its hyphens doubled, passes 255
\ bytes is STRUCTURE's refusal, spoken before anything is defined; this name
\ is 181 bytes. A multi-error load counts that refusal and still defines
\ nothing.
: LONG-SRC$ ( -- ptr u8 n ) s" ARENA a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a top-rec" ;


: LONG-UNDEFINED ( -- )
   s" a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-a-NEW" get-current search-wl
   0= if s" undefined" else s" defined" then type cr ;


: LONG-ROW ( -- )
   s" arena-long: " type
   [: LONG-SRC$ INCLUDE-EVALUATE ;] catch .  LONG-UNDEFINED
   s" arena-long-multi: " type
   MULTI-ERR-BEGIN [: LONG-SRC$ INCLUDE-EVALUATE ;] catch MULTI-ERR-END . .
   LONG-UNDEFINED ;
LONG-ROW

\ A top-level name past the family-name cap: ARENA runs STRUCTURE's own name
\ gate on it before spelling anything from it, so it prints what a bare
\ STRUCTURE of that name prints and is refused with the same code. The name is
\ `a` and 350 `-a`, 701 bytes: its constructor package, hyphens doubled, would
\ not fit TFAM's spelling buffer.
1024 constant CAP-ROOM
CAP-ROOM BUFFER: CAP-SRC
variable CAP-U
: CAP, ( ptr u8 n -- )
   {: a:ptr u:n :}
   u 0 ?do a i + c@ CAP-SRC CAP-U @ + c! CAP-U @ 1 + CAP-U ! loop ;


: CAP-TEXT ( ptr u8 n ptr u8 n -- ptr u8 n )
   {: h:ptr hu:n t:ptr tu:n :}
   0 CAP-U !  h hu CAP,  s" a" CAP,  350 0 ?do s" -a" CAP, loop  t tu CAP,
   CAP-SRC CAP-U @ ;


: CAP-ROW ( -- )
   s" arena-cap: " type
   [: s" ARENA " s"  top-rec" CAP-TEXT INCLUDE-EVALUATE ;] catch .
   [: s" STRUCTURE " s"  ;STRUCTURE" CAP-TEXT INCLUDE-EVALUATE ;] catch .
   s" " s" -NEW" CAP-TEXT get-current search-wl
   0= if s" undefined" else s" defined" then type cr ;
CAP-ROW

\ An owner's head is reached by no accessor, so no bytes can be planted as a
\ chunk and no head copied through one; the owner itself is never fetched or
\ stored whole. P4C, if it certified, would run 32 records into FOO-A, copy it
\ into FOO-B, grow and release both: a double unmap. Each row prints its
\ definition's refusal code. A fetch through a `ptr box` missing its argument
\ asks whether the record lives in place: it is refused for its arity, never
\ walked.
ARENA FOOS top-rec
STRUCTURE box<a> DERIVE addr FIELD v a ;STRUCTURE
TYPED-VARIABLE FOO-A foos
TYPED-VARIABLE FOO-B foos
256 BUFFER: FAKE
TYPED-VARIABLE ROW-A ptr u8
variable ROW-U
: ROW-RUN ( -- ) ROW-A @ ROW-U @ INCLUDE-EVALUATE ;
: OWNER-ROW ( ptr u8 n ptr u8 n -- ) ROW-U ! ROW-A ! type [: ROW-RUN ;] catch . ;


: OWNER-ROWS ( -- )
   s" arena-forge: " s" : P1 ( -- ptr top-rec ) FAKE FOO-A FOOS:HEAD ! FOO-A FOOS-NEW ;" OWNER-ROW
   s" arena-head-copy: " s" : P4 ( -- ) FOO-A FOOS:HEAD @ FOO-B FOOS:HEAD ! ;" OWNER-ROW
   s" arena-owner-copy: " s" : P4B ( -- ) FOO-A @ FOO-B ! ;" OWNER-ROW
   s" arena-owner-copy-run: " s" : P4C ( -- ) 32 0 do FOO-A FOOS-NEW drop loop FOO-A @ FOO-B ! FOO-A FOOS-NEW drop FOO-B FOOS-NEW drop FOO-A FOOS-RELEASE FOO-B FOOS-RELEASE ; P4C" OWNER-ROW
   s" arena-arity: " s" : G1 ( ptr box -- ) @ drop ;" OWNER-ROW ;
OWNER-ROWS

\ A head the user declares is inert: it may be named, but ARENA-NEW and
\ ARENA-RELEASE answer only ARENA's generated NAME-NEW and NAME-RELEASE, so no
\ call or tick of either, directly or through a re-export, certifies elsewhere. Z2, if it certified,
\ would let an 8-byte record stand for any type; TOP-CALL would run on a NULL
\ head. Each row prints its refusal code; SIG prints 0.
TYPED-VARIABLE HH DYNAMIC-STORAGE:arena-head
: CALL-NAME ( -- ptr u8 n ) s" x" ;
package ARENA-PX
public
EXPORT DYNAMIC-STORAGE:ARENA-RELEASE
;package

: CALL-ROWS ( -- )
   s" arena-sig: " s" : Y3 ( ptr DYNAMIC-STORAGE:arena-head -- ) drop ;" OWNER-ROW
   s" arena-new: " s" : Z2 ( -- ptr u8 ) HH 8 CALL-NAME DYNAMIC-STORAGE:ARENA-NEW ;" OWNER-ROW
   s" arena-release: " s" : Z3 ( -- ) HH DYNAMIC-STORAGE:ARENA-RELEASE ;" OWNER-ROW
   s" arena-null-new: " s" : Y1 ( -- ptr u8 ) NULL-PTR 8 CALL-NAME DYNAMIC-STORAGE:ARENA-NEW ;" OWNER-ROW
   s" arena-null-release: " s" : Y2 ( -- ) NULL-PTR DYNAMIC-STORAGE:ARENA-RELEASE ;" OWNER-ROW
   s" arena-tick: " s" : T1 ( -- ptr u8 ) NULL-PTR 8 CALL-NAME ['] DYNAMIC-STORAGE:ARENA-NEW execute ;" OWNER-ROW
   s" arena-quot: " s" : T3 ( -- ptr u8 ) NULL-PTR 8 CALL-NAME [: DYNAMIC-STORAGE:ARENA-NEW ;] execute ;" OWNER-ROW
   s" arena-top-tick: " s" ' DYNAMIC-STORAGE:ARENA-RELEASE drop" OWNER-ROW
   s" arena-top-call: " s" NULL-PTR DYNAMIC-STORAGE:ARENA-RELEASE" OWNER-ROW
   s" arena-export: " s" : E1 ( -- ) NULL-PTR ARENA-PX:ARENA-RELEASE ;" OWNER-ROW ;
CALL-ROWS

\ ARENA's NAME-NEW and NAME-RELEASE and STRUCTURE's accessors spell
\ `field-project` unqualified, so a sealed engine refuses it as a definition
\ name. Were ARENA-CAPTURE's admitted, SMALLS-NEW and BIGS-NEW would carve
\ records from CAP-HEAD: a big record's field would show through a small one,
\ printing 4242. Were STRUCT-CAPTURE's admitted, S2-Y's store would land in V,
\ printing 77 twice. Each row prints its refusal code. The packages open in
\ this file, so a refused row leaves none open.
: LINE, ( ptr u8 n -- ) CAP, s"  " CAP, ;


: ARENA-CAPTURE-SRC$ ( -- ptr u8 n )
   0 CAP-U !
   s" STRUCTURE small DERIVE addr FIELD p ptr u8 ;STRUCTURE" LINE,
   s" STRUCTURE big DERIVE addr FIELD x n FIELD y n ;STRUCTURE" LINE,
   s" TYPED-VARIABLE CAP-HEAD DYNAMIC-STORAGE:arena-head" LINE,
   s" : field-project ( a n -- ptr DYNAMIC-STORAGE:arena-head ) drop drop CAP-HEAD ;" LINE,
   s" ARENA SMALLS small  ARENA BIGS big" LINE,
   s" TYPED-VARIABLE SP smalls  TYPED-VARIABLE BP bigs" LINE,
   s" : RUN ( -- ) SP SMALLS-NEW drop  BP BIGS-NEW {: b:ptr :}  SP SMALLS-NEW {: s:ptr :}" LINE,
   s" 4242 b BIG-X !  s SMALL-P @ NULL-PTR - .  SP SMALLS-RELEASE  BP BIGS-RELEASE ;  RUN" LINE,
   CAP-SRC CAP-U @ ;


: STRUCT-CAPTURE-SRC$ ( -- ptr u8 n )
   0 CAP-U !
   s" variable V" LINE,
   s" : field-project ( a n -- ptr n ) drop drop V ;" LINE,
   s" STRUCTURE s2 DERIVE addr FIELD x n FIELD y n ;STRUCTURE" LINE,
   s" TYPED-VARIABLE R s2" LINE,
   s" : RUN ( -- ) 77 R S2-Y !  V @ .  R S2-Y @ . ;  RUN" LINE,
   CAP-SRC CAP-U @ ;


: ARENA-CAPTURE-ROW ( -- ) s" arena-capture: " ARENA-CAPTURE-SRC$ OWNER-ROW ;
: STRUCT-CAPTURE-ROW ( -- ) s" struct-capture: " STRUCT-CAPTURE-SRC$ OWNER-ROW ;
package ARENA-CAPTURE
ARENA-CAPTURE-ROW
;package
package STRUCT-CAPTURE
STRUCT-CAPTURE-ROW
;package

package ARENA-LIVE
STRUCTURE rec DERIVE addr FIELD x n ;STRUCTURE
ARENA LIVE-RECS rec
TYPED-VARIABLE POOL live-recs

: PREPARED ( -- n ) [: IMAGE-LIFECYCLE:PREPARE ;] catch ;

public

: RUN ( -- )
   s" arena-live: " type
   TOP-POOL TOP-RECS-NEW drop PREPARED . TOP-POOL TOP-RECS-RELEASE
   POOL LIVE-RECS-NEW drop PREPARED . POOL LIVE-RECS-RELEASE
   PREPARED . ;
;package

ARENA-LIVE:RUN
