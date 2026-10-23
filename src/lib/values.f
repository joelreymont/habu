\ values.f - a typed, fixed-cell append/read collection owned by one trusted
\ load scope.
\
\ VALUES:MAKE reserves a known capacity (zero allowed), ADD appends one typed
\ value, GET reads a zero-based index in the initialized prefix and never
\ exposes unused capacity. The checker keeps the element's nominal type and
\ refuses wider by-value layouts; representation, pointer and owner are not
\ reachable through package VALUES.
\ VALUE-SCOPE:WITH is for the trusted source loader only: one scope at a time,
\ and it releases every collection on return or throw. A collection that
\ outlives its scope is stale, and ADD and GET refuse it before touching its
\ former allocation, even while a later scope is active. The loader admits
\ only VALUES to untrusted source, never VALUE-SCOPE, raw memory or
\ representation access. Errors are the E-VALUES-* constants below.
\
\ STORAGE CLASS. PROCESS-WIDE: the scope's block tables, ACTIVE and EPOCH are
\ static, for a single source-load process with no concurrent tasks.
require src/lib/memory.f

-9480 constant E-VALUES-FIRST
-9489 constant E-VALUES-LAST
-9480 constant E-VALUES-STATE      \ no active scope, nested scope, or an expired collection
-9481 constant E-VALUES-CAPACITY   \ invalid capacity or an append past it
-9482 constant E-VALUES-BOUNDS     \ GET outside the initialized prefix
-9483 constant E-VALUES-EPOCH      \ another scope would wrap its generation

package VALUE-SCOPE
private
DYNAMIC-BUFFER BLOCKS ptr u8
DYNAMIC-BUFFER SIZES NUM:alloc-byte-len
variable COUNT
variable ACTIVE
variable EPOCH

: OPEN ( -- )
   ACTIVE @ if E-VALUES-STATE throw then
   EPOCH @ MEM-MAX-N = if E-VALUES-EPOCH throw then
   true ACTIVE ! ;

: CLOSE ( -- )
   false ACTIVE !
   1 EPOCH +!
   COUNT @ 0 ?do
      i BLOCKS @ i SIZES @ MEM:RELEASE-BYTES
   loop
   0 COUNT !
   BLOCKS-RELEASE SIZES-RELEASE ;

public
: CURRENT ( -- n )
   ACTIVE @ 0= if E-VALUES-STATE throw then
   EPOCH @ ;

: ALLOCATE ( n -- ptr a )
   {: cap:n :}
   CURRENT drop
   cap 0 < cap MEM-MAX-CELLS 1- > or if E-VALUES-CAPACITY throw then
   COUNT @ 1+ BLOCKS-RESERVE
   COUNT @ 1+ SIZES-RESERVE
   cap 1+ MEM:CELLS-ALLOC-COUNT MEM:ALLOC-CELLS {: data:ptr :}
   data BYTE-VIEW COUNT @ BLOCKS !
   cap 1+ cells MEM:BYTES-ALLOC-LEN COUNT @ SIZES !
   1 COUNT +!
   data ;

: WITH ( R [ R -- S ] -- S )
   OPEN [: CLOSE ;] finally ;
;package

package VALUES
public
STRUCTURE values<a> OPAQUE
   FIELD data ptr a FIELD used ptr n FIELD cap n FIELD epoch n
;STRUCTURE
private
: OPENED ( values<a> -- ptr a ptr n n )
   VALUES-UNMAKE {: data:ptr used:ptr cap:n epoch:n :}
   epoch VALUE-SCOPE:CURRENT <> if E-VALUES-STATE throw then
   data used cap ;

: INDEX ( values<a> n -- ptr a )
   {: val idx:n :}
   val OPENED {: data:ptr used:ptr cap:n :}
   idx 0 < idx used @ >= or if E-VALUES-BOUNDS throw then
   data idx cells + ;

public
: MAKE ( n -- values<a> )
   {: cap:n :}
   cap VALUE-SCOPE:ALLOCATE {: data:ptr :}
   data BYTE-VIEW cap cells + CELL-VIEW {: used:ptr :}
   0 used !
   data used cap VALUE-SCOPE:CURRENT VALUES-MAKE ;

: ADD ( a values<a> -- )
   {: value val :}
   val OPENED {: data:ptr used:ptr cap:n :}
   used @ cap >= if E-VALUES-CAPACITY throw then
   value data used @ cells + !
   1 used +! ;

: GET ( values<a> n -- a ) INDEX @ ;
;package
