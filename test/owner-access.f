\ owner-access.f - an internal primitive its owner's rows type. A DNAME-INT
\ primitive has no top-level effect: the prompt, `'` and search-wl refuse it and
\ a compiled call needs a TRUSTED: boundary. One a package row types
\ (src/habu/prims.f EPPRIM: ... ECLOSE-PRIVATE) also carries DNAME-OWNED
\ (src/habu/layout.f): still hidden, while a compiled call is the checker's
\ decision. Owner-open calls are test/prim-owner-scope.f's.
require src/lib/string.f
require src/lib/test.f
require src/lib/test/subject.f

package OWNER-ACCESS-TEST

$1000 constant IO-CAP
create OUT IO-CAP allot
create ERR IO-CAP allot

70 constant REJECT-RC                  \ a compile reject and an internal engine word

: OA-MARK$ ( -- ptr u8 n ) s" @" ;

\ The child hands the source to `evaluate`, the engine's own interpret loop.
: CHILD ( ptr u8 n -- len len n )
   OUT IO-CAP >LEN ERR IO-CAP >LEN 10000 >MS SUBJECT:RUN PROC-OUTCOME>RC RC>N ;

\ A call the engine compiled and ran, ending the child with status n. OA-AT's
\ marker as the whole of fd 1 proves the definition compiled and the call
\ reached the primitive. The child's fd 2 is left for the caller.
: RAN ( ptr u8 n n -- ptr u8 n ) {: src:ptr size:n rc:n :}
   src size CHILD {: outu:len erru:len got:n :}
   got rc <> if ERR erru LEN>N type then
   src size T-LABEL  got rc T=
   src size T-LABEL  OUT outu LEN>N OA-MARK$ T$=
   ERR erru LEN>N ;

\ A call that ran though fd 2 holds the verdict WANT, which the unjudged regime
\ reports and does not enforce.
: REACHES-REPORTED ( ptr u8 n n ptr u8 n -- )
   {: src:ptr size:n rc:n want:ptr wantu:n :}
   src size rc RAN {: ea:ptr eu:n :}
   src size T-LABEL  ea eu want wantu CONTAINS? TTRUE ;

\ A source the engine refuses before anything runs: the reject status and the
\ diagnostic naming why.
: REJECTS ( ptr u8 n ptr u8 n -- ) {: src:ptr size:n want:ptr wantu:n :}
   src size CHILD {: outu:len erru:len got:n :}
   ERR erru LEN>N {: ea:ptr eu:n :}
   ea eu want wantu CONTAINS? 0= if ea eu type then
   src size T-LABEL  got REJECT-RC T=
   src size T-LABEL  ea eu want wantu CONTAINS? TTRUE ;

public

: OA-AT ( -- ) OA-MARK$ type ;

private

\ package-scope! and namespace-record are CHECKER-OVERLAY's, source-unit-run and
\ source-file-run SOURCE-ROOT's; int-mark has no package row. Unchecked code
\ compiles a call to an owned primitive at both tiers: the call runs, and each
\ writer refuses its arguments itself (record 0 is no namespace row; an empty
\ name) with exit 83. Both tiers' native compilers report the global
\ trusted-only row's E-CAP-TRUSTED, as they report every unjudged verdict, and
\ compile the call.
: UNCHECKED ( -- )
   s" 0 set-check : OA-SCOPE ( n n -- ) package-scope! ; 0 0 OA-AT OA-SCOPE"
   ENGINE-ERROR:SEAL-VIOLATION
   s" E-CAP-TRUSTED habu: in oa-scope: 'package-scope!' is a trust-boundary primitive"
   REACHES-REPORTED
   s" 0 set-check : OA-NS ( ptr u8 n bool -- n ) namespace-record ; parse-name OAN drop 0 true OA-AT OA-NS"
   ENGINE-ERROR:SEAL-VIOLATION
   s" E-CAP-TRUSTED habu: in oa-ns: 'namespace-record' is a trust-boundary primitive"
   REACHES-REPORTED
   s" 1 set-tier 0 set-check : OA-NS1 ( ptr u8 n bool -- n ) namespace-record ; parse-name OAN drop 0 true OA-AT OA-NS1"
   ENGINE-ERROR:SEAL-VIOLATION
   s" E-CAP-TRUSTED habu: in oa-ns1: 'namespace-record' is a trust-boundary primitive"
   REACHES-REPORTED ;

\ OWNED comes from the rows, not a name list: source-unit-run's SOURCE-ROOT
\ row makes it owned too, so unchecked tier-0 code compiles a call to it, and
\ the call runs its callback, which prints the marker. The native compiler
\ reads the signature even unchecked, so it names a real type.
: UNIT-RUN ( -- )
   s" 0 set-check : OA-UNIT ( [ -- ] -- ) source-unit-run ; ' OA-AT OA-UNIT"
   0
   s" E-CAP-TRUSTED habu: in oa-unit: 'source-unit-run' is a trust-boundary primitive"
   REACHES-REPORTED ;

\ An internal primitive no package row types keeps the compile refusal, so
\ unchecked code still needs a TRUSTED: boundary to call it.
: INTERNAL-ONLY ( -- )
   s" 0 set-check : OA-MARK ( n -- ) int-mark ;"
   s" E-UNDEFINED: int-mark" REJECTS ;

\ OWNED opens nothing that DNAME-INT closes: the prompt and `'` refuse an owned
\ primitive as they refuse every internal engine word.
: HIDDEN ( -- )
   s" parse-name OAH 0 0= namespace-record"
   s" hb: internal engine word: namespace-record" REJECTS
   s" ' namespace-record"
   s" hb: internal engine word: namespace-record" REJECTS
   s" 0 0 package-scope!"
   s" hb: internal engine word: package-scope!" REJECTS
   s" ' package-scope!"
   s" hb: internal engine word: package-scope!" REJECTS ;

\ Outside the owner the checker refuses a checked caller through the global
\ trusted-only row, at both tiers: the call guards admit the record (tier 0's
\ C-COMPILE-CALL-GUARD, tier 1's NDICT:INT-CALL?), so the refusal is the row's.
: OUTSIDE ( -- )
   s" 1 set-tier : OA-OUT1 ( n n -- ) package-scope! ;"
   s" E-CAP-TRUSTED habu: in oa-out1: 'package-scope!' is a trust-boundary primitive" REJECTS
   s" : OA-NS-OUT0 ( ptr u8 n bool -- n ) namespace-record ;"
   s" E-CAP-TRUSTED habu: in oa-ns-out0: 'namespace-record' is a trust-boundary primitive" REJECTS
   s" 1 set-tier : OA-NS-OUT1 ( ptr u8 n bool -- n ) namespace-record ;"
   s" E-CAP-TRUSTED habu: in oa-ns-out1: 'namespace-record' is a trust-boundary primitive" REJECTS
   s" : OA-SUR0 ( [ -- ] -- ) source-unit-run ;"
   s" E-CAP-TRUSTED habu: in oa-sur0: 'source-unit-run' is a trust-boundary primitive" REJECTS
   s" 1 set-tier : OA-SUR1 ( [ -- ] -- ) source-unit-run ;"
   s" E-CAP-TRUSTED habu: in oa-sur1: 'source-unit-run' is a trust-boundary primitive" REJECTS
   s" : OA-SFR0 ( [ -- ] -- ) source-file-run ;"
   s" E-CAP-TRUSTED habu: in oa-sfr0: 'source-file-run' is a trust-boundary primitive" REJECTS
   s" 1 set-tier : OA-SFR1 ( [ -- ] -- ) source-file-run ;"
   s" E-CAP-TRUSTED habu: in oa-sfr1: 'source-file-run' is a trust-boundary primitive" REJECTS ;

public

: OA-RUN ( -- )
   T-RESET
   s" unchecked code calls an owned primitive at both tiers" T-LABEL UNCHECKED T-NEXT
   s" a package row owns source-unit-run" T-LABEL UNIT-RUN T-NEXT
   s" an unowned internal primitive stays uncompilable" T-LABEL INTERNAL-ONLY T-NEXT
   s" the prompt and tick refuse an owned primitive" T-LABEL HIDDEN T-NEXT
   s" a checked caller outside the owner is refused by its row" T-LABEL OUTSIDE T-NEXT
   T-REPORT ;

;package

\ The children inherit this package's imports, which OA-AT needs.
using OWNER-ACCESS-TEST
OA-RUN
;using
