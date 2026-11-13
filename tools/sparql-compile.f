\ W3C adapter: one load compiles a JSON array of [query, base-IRI] pairs.
\ Emit one plan descriptor or numeric error per line. PostgreSQL execution
\ and fixture comparison stay in the external conformance harness.

require src/lib/sparql/compile.f
require src/lib/json/read.f
require src/lib/fs.f

package SPARQL
private

CAST: >DRIVER-CURSOR ( ptr u8 -- ptr JSON:cursor )

: DRIVER-CURSOR ( ptr ARENA:arena -- ptr JSON:cursor )
   JSON:CURSOR-BYTES [: >DRIVER-CURSOR JSON:READ-DISPOSE ;]
      ARENA:ALLOC-DISPOSE drop >DRIVER-CURSOR ;

: DRIVER-TEXT ( JSON:reader ptr ARENA:arena n
      -- JSON:reader ptr u8 n )
   {: arena capacity:n :}
   JSON:READ-NEXT JSON:T-STR <> if E-JSON-READ-STATE throw then
   arena ARENA:ALLOCATOR capacity 1+ ALLOC:BYTES {: out size:n :}
   out size JSON:READ-STR out swap ;

: DRIVER-ERROR ( n -- )
   s\" {\"error\":" type FMT:INT$ type s" }" type cr ;

: DRIVER-ONE ( ptr u8 n ptr ARENA:arena -- )
   {: bytes size:n arena :}
   arena DRIVER-CURSOR bytes size JSON:READ-INIT
   JSON:READ-NEXT JSON:T-ARR <> if E-JSON-READ-STATE throw then
   arena size DRIVER-TEXT {: reader query qu:n :}
   reader arena size DRIVER-TEXT {: next base bu:n :}
   next JSON:READ-NEXT JSON:T-ARR-END <> if E-JSON-READ-STATE throw then
   JSON:READ-FINISH
   [: query qu base bu TIME:MONO-NS 10000000000 + arena PARSE-UNTIL
      1 SCRIPT-ARGV$ COMPILE PLAN-REQUEST$ type cr ;]
   catch dup 0<> if DRIVER-ERROR else drop then ;

: DRIVER-FILE ( ptr ARENA:arena -- ptr u8 n )
   {: arena :}
   0 SCRIPT-ARGV$ FS:FILE-SIZE {: size:n :}
   arena ARENA:ALLOCATOR size 1+ ALLOC:BYTES drop {: text :}
   0 SCRIPT-ARGV$ text size FS:READ-ALL size <> if E-FS-IO throw then
   text size ;

: DRIVER-BATCH ( ptr ARENA:arena -- )
   {: arena :}
   arena DRIVER-CURSOR arena DRIVER-FILE JSON:READ-INIT
   JSON:READ-NEXT JSON:T-ARR <> if E-JSON-READ-STATE throw then
   begin JSON:READ-NEXT dup JSON:T-ARR-END <> while
      JSON:T-ARR <> if E-JSON-READ-STATE throw then
      JSON:READ-VALUE-SPAN$ [: DRIVER-ONE ;] ARENA:WITH
   repeat drop JSON:READ-FINISH ;

: DRIVER-MAIN ( -- )
   SCRIPT-ARGC 2 <> if
      s" usage: hb --load tools/sparql-compile.f -- batch.json schema" type cr
      E-SYNTAX throw
   then [: DRIVER-BATCH ;] ARENA:WITH ;

DRIVER-MAIN

;package
