\ native-create-does.f - ordinary colon compiles defining words natively.

require lib/test.f
require src/compiler/native/compiler.f
require src/habu/verify-source.f

1 set-tier

package NATIVE-CREATE-DOES-PUBLIC
public
: MAKE-CHECKED ( n -- ) create , does> ( -- ptr n ) ;
TRUSTED: MAKE-TRUSTED ( n -- ) create , does> ( -- ptr n ) ;
8 MAKE-CHECKED CHECKED-LIVE
8 MAKE-TRUSTED TRUSTED-LIVE
: CHECKED-VALUE ( -- n ) CHECKED-LIVE @ ;
: TRUSTED-VALUE ( -- n ) TRUSTED-LIVE @ ;
: PLAIN ( n -- n ) 1 + ;
\ Runs inside a pending definer's body and hands the compiler entry a source of
\ its own, which holds no `does> ` at the cut the engine recorded.
: FORGE-CUT ( -- ) s" ab" NCOMP:COMPILE ; immediate
s" FORGE-CUT" 0 parse-imm
;package

package NATIVE-CREATE-DOES-TEST
private

: MAKE-CELL ( n -- n )
   dup create ,
   1 +
   does> ( -- n ) @ ;

\ The first spelling must remain a string payload, not become the split row.
: MAKE-INERT-CELL ( n -- n )
   s" does>" 2drop
   dup create ,
   1 +
   DoEs> ( -- n ) @ ;

: MAKE-ADDER ( n -- n )
   dup create ,
   1 +
   does> ( n -- n ) @ + ;

: MAKE-ADDRESS ( n -- )
   create ,
   does> ( -- ptr a ) ;

TRUSTED: TRUSTED-MAKE ( n -- )
   drop dbase@ drop
   create 55 ,
   DOES> ( -- n ) @ ;

: MAYBE-PATCH ( bool -- )
   create 99 ,
   if exit then
   does> ( -- n ) drop 7 ;

41 MAKE-CELL CREATED-CELL 42 T=
50 MAKE-INERT-CELL INERT-CELL 51 T=
5 MAKE-ADDER ADD5 6 T=
73 MAKE-ADDRESS CREATED-ADDRESS
9 TRUSTED-MAKE TRUSTED-CELL
0 0= MAYBE-PATCH UNPATCHED
1 0= MAYBE-PATCH PATCHED
UNPATCHED @ 99 T=

public

\ The scan compiles nothing: a word it registers (CHECKED-MADE, PLAIN-MADE, the
\ ghosts) binds through the checker overlay only while the package-neutral
\ scope holding the scan and its probes is open. Every probe asks the certify
\ path (VERIFY:CANDIDATE-IN-SCOPE), where a scanned name binds and so does any
\ row the checker kept, a refused parent's leftover signature included. The
\ live definitions run between scopes: an open scope refuses a definition
\ (ENGINE-ERROR:OVERLAY-OPEN).
: RUN ( -- )
   CHECKER-SCOPE-START-NEUTRAL
   s\" 9 NATIVE-CREATE-DOES-PUBLIC:MAKE-CHECKED CHECKED-MADE\n"
      VERIFY:SOURCE-BUF-IN-SCOPE
   s\" 9 NATIVE-CREATE-DOES-PUBLIC:MAKE-TRUSTED TRUSTED-MADE\n"
      VERIFY:SOURCE-BUF-IN-SCOPE
   s" a native checked definer publishes its created effect" T-LABEL
   s" NC1 ( -- ptr n ) CHECKED-MADE" VERIFY:CANDIDATE-IN-SCOPE -1 T=
   s" a native checked definer refuses a scalar use" T-LABEL
   s" NC2 ( -- n ) CHECKED-MADE" VERIFY:CANDIDATE-IN-SCOPE 0 T=
   s" a native trusted definer publishes its created effect" T-LABEL
   s" NT1 ( -- ptr n ) TRUSTED-MADE" VERIFY:CANDIDATE-IN-SCOPE -1 T=
   s" a native trusted definer refuses a scalar use" T-LABEL
   s" NT2 ( -- n ) TRUSTED-MADE" VERIFY:CANDIDATE-IN-SCOPE 0 T=
   CHECKER-SCOPE-DONE
   s" both native definers create working storage" T-LABEL
   NATIVE-CREATE-DOES-PUBLIC:CHECKED-VALUE 8 T=
   NATIVE-CREATE-DOES-PUBLIC:TRUSTED-VALUE 8 T=
   CHECKER-SCOPE-START-NEUTRAL
   s\" 5 NATIVE-CREATE-DOES-PUBLIC:PLAIN PLAIN-MADE\n"
      VERIFY:SOURCE-BUF-IN-SCOPE
   s" a native plain definition inherits no clause" T-LABEL
   s" NP1 ( -- ptr n ) PLAIN-MADE" VERIFY:CANDIDATE-IN-SCOPE 1 T=
   CHECKER-SCOPE-DONE
   s" a refused native clause publishes no definer" T-LABEL
   [: s\" : NATIVE-DOES-BAD ( n -- ) NATIVE-CREATE-DOES-PUBLIC:MAKE-CHECKED does> ( -- n ) ;\n"
      evaluate-closed ;]
      E-NCOMP-VERDICT TTHROWSQ
   s" a refused clause leaves no parent signature" T-LABEL
   s" NB0 ( n -- ) NATIVE-DOES-BAD" VERIFY:CANDIDATE-IN-SCOPE 1 T=
   CHECKER-SCOPE-START-NEUTRAL
   s\" 9 NATIVE-DOES-BAD BAD-GHOST\n" VERIFY:SOURCE-BUF-IN-SCOPE
   s" a refused wrapper parent cannot create a phantom child" T-LABEL
   s" NB1 ( -- ptr n ) BAD-GHOST" VERIFY:CANDIDATE-IN-SCOPE 1 T=
   CHECKER-SCOPE-DONE
   s\" : NATIVE-DOES-AFTER ( n -- n ) 2 * ;\n" evaluate-closed
   CHECKER-SCOPE-START-NEUTRAL
   s\" 6 NATIVE-DOES-AFTER AFTER-MADE\n" VERIFY:SOURCE-BUF-IN-SCOPE
   s" the definition after a refused clause inherits nothing" T-LABEL
   s" NP2 ( -- ptr n ) AFTER-MADE" VERIFY:CANDIDATE-IN-SCOPE 1 T=
   CHECKER-SCOPE-DONE
   s" a later native refusal retracts an accepted clause" T-LABEL
   [: s\" : NATIVE-DOES-LATE ( n -- ) NATIVE-CREATE-DOES-PUBLIC:MAKE-CHECKED does> ( -- ptr n ) [: 1 ;] drop ;\n"
      evaluate-closed ;] E-NELAB-QUOT TTHROWSQ
   s" a later refusal retracts the parent signature" T-LABEL
   s" NL0 ( n -- ) NATIVE-DOES-LATE" VERIFY:CANDIDATE-IN-SCOPE 1 T=
   CHECKER-SCOPE-START-NEUTRAL
   s\" 9 NATIVE-DOES-LATE LATE-GHOST\n" VERIFY:SOURCE-BUF-IN-SCOPE
   s" a later failure cannot leave a wrapper child" T-LABEL
   s" NL1 ( -- ptr n ) LATE-GHOST" VERIFY:CANDIDATE-IN-SCOPE 1 T=
   CHECKER-SCOPE-DONE
   s\" : NATIVE-DOES-NEXT ( n -- n ) 1 + ;\n" evaluate-closed
   CHECKER-SCOPE-START-NEUTRAL
   s\" 6 NATIVE-DOES-NEXT NEXT-MADE\n" VERIFY:SOURCE-BUF-IN-SCOPE
   s" the next definition inherits no accepted clause from that refusal" T-LABEL
   s" NP3 ( -- ptr n ) NEXT-MADE" VERIFY:CANDIDATE-IN-SCOPE 1 T=
   CHECKER-SCOPE-DONE
   s" a does> cut outside the compiled source is refused as the cut" T-LABEL
   [: s\" : NATIVE-DOES-FORGED ( n -- ) create , does> ( -- n ) NATIVE-CREATE-DOES-PUBLIC:FORGE-CUT @ ;\n"
      evaluate-closed ;] E-NFEED-CUT TTHROWSQ
   s" a refused cut publishes no definer" T-LABEL
   s" NF0 ( n -- ) NATIVE-DOES-FORGED" VERIFY:CANDIDATE-IN-SCOPE 1 T=

   s" created storage and the published output signature survive" T-LABEL
   CREATED-CELL 41 T=

   s" the clause consumes the created word's declared input" T-LABEL
   7 ADD5 12 T=

   s" an empty does> clause remains an address identity" T-LABEL
   CREATED-ADDRESS @ 73 T=

   s" does> inside a string is not the defining-word split" T-LABEL
   INERT-CELL 50 T=

   s" an uncheckable trusted parent still publishes its checked clause" T-LABEL
   TRUSTED-CELL 55 T=

   s" an explicit exit leaves CREATE behavior; fallthrough installs does>" T-LABEL
   PATCHED 7 T=

   T-REPORT ;

;package

NATIVE-CREATE-DOES-TEST:RUN
