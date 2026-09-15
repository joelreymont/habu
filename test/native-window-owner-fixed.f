\ Compile the actual dictionary boundary against a fresh owner at tier1.
s" src/habu/layout.f" provided
s" src/core/checker-owner-abi.f" provided
require src/habu/code-span.f
require src/habu/xref.f
require src/compiler/native/dict.f

package OWNER-FIXED-CHECK

43 constant VALUE
variable SLOT
create BYTES 8 allot
variable ENTERED

\ Compare the raw dictionary result with the known address's representation.
TRUSTED: SLOT-N ( -- n ) SLOT ;
TRUSTED: BYTES-N ( -- n ) BYTES ;

: EQ! ( n n -- ) <> if 79 throw then ;
: ORDINARY ( -- n ) 1 ENTERED ! 55 ;
: WRONG-EFFECT ( n -- n n ) 1 ENTERED ! dup ;
: WRONG-KIND ( -- ) s" ORDINARY" NDICT:FIXED-VALUE drop ;
: WRONG-ARITY ( -- ) s" WRONG-EFFECT" NDICT:FIXED-VALUE drop ;

: RUN ( -- )
   tier@ 1 EQ!
   s" VALUE" NDICT:FIXED-VALUE 43 EQ!
   s" SLOT" NDICT:FIXED-VALUE SLOT-N EQ!
   s" BYTES" NDICT:FIXED-VALUE BYTES-N EQ!
   [: WRONG-KIND ;] catch E-NDICT-KIND EQ!
   [: WRONG-ARITY ;] catch E-NDICT-KIND EQ!
   ENTERED @ 0 EQ! ;

RUN
;package
