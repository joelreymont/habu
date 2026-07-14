\ maki/store-rehydrate.f - typed rehydration of persisted CAD store rows (R7 store
\ rehydrate, sub-dot 6 v2-typestate-store-rehydrate, dot
\ habu-v2-typestate-store-57afdc0a; MODEL-CAD-V2-PLAN.md § R7 Design Addendum,
\ sub-dot at :1953-1961, transition-words note at :1824-1826).
\
\ A persisted store row is UNTRUSTED input: a latest-wins line scan (maki/store.f)
\ hands back whatever bytes are on disk. This file is the typed READ boundary for the
\ evidence class - the store rows that carry ENUMS and KINDS (per-gate verdicts, the
\ golden leg, the licensed precision) and could otherwise launder untyped text back
\ into a promotion decision. Instead of the raw text EVID-GET returns, EVID-ROW-DECODE
\ parses each field through its typed family, so a malformed field count, an
\ out-of-domain verdict/precision token, or an unknown field label REJECTS with a named
\ throw (never silently skipped, never accepted raw). The golden leg (EVID:golden-leg)
\ and licensed precision (EVID:prec-class) come back as the exact typed values
\ maki/evidence/schema.f owns - the retired maki/golden.f ambient globals' typed home.
\
\ DEVIATION (no proof tokens, no full products): the R7 addendum requires that "a
\ rehydrated row never mints a proof token" (:1824-1826) - provenance proof tokens exist
\ ONLY downstream of a real gate transition (maki/evidence/schema.f). So decoding a
\ persisted row yields the typed FIELDS it can honestly recover - the four verdicts, the
\ golden leg, and the precision class - NOT the sealed EVID:certified / EVID:golden
\ products (whose construction is gated on the class-private MINT-*-PROOF, and whose
\ full EVID:bundle is unconstructable in readable source anyway, TF-CTOR-NAME-LIMIT;
\ dots habu-raise-or-alias-5d2a6b70 / habu-public-producers-for-7084d81c). Mapping the
\ recovered verdicts to the got/none presence slots is POLICY:CHECK's job (it owns the
\ proof-carrying evidence); this file only certifies the persisted TEXT is well-typed.
\
\ NO WRITER here: rehydration is read-only. Re-rendering a decoded value to the wire goes
\ back through maki/store.f's sealed EVID-PUT-G (the one on-disk-encoding owner), so this
\ file adds no new store-planting surface (census probe 3 stays closed) and never
\ duplicates the wire format.
\
\ maki -> habu only; store-rehydrate owns -5086..-5089 (the free block between sched-key
\ -5084..-5085 and store -5090..-5099).

require lib/string.f
require maki/store.f              \ EVID-GET + the sealed EVID-PUT-G wire owner; V-* + verdict enum
require maki/evidence/schema.f    \ EVID:golden-leg / EVID:prec-class (the typed provenance home)

-5086 constant E-EVID-ROW-FIELDS   \ persisted evidence row: not exactly four pipe-delimited fields
-5087 constant E-EVID-ROW-LABEL    \ a field's label is not a known evidence class (unknown kind)
-5088 constant E-EVID-ROW-VERDICT  \ a verdict token is outside the pass/fail/not-run domain
-5089 constant E-EVID-ROW-PREC     \ a precision token is outside f32/tf32 (or missing on a device leg)

package MAKI
private

7 constant EVID-DEV-PREFIX-LEN     \ length of the "device-" golden-leg wire prefix

\ ---- field splitting (the suffix EVID-GET returns is exactly four '|' fields) ----
: EVID-SPLIT1 ( ptr u8 n n -- ptr u8 n n ) {: a:ptr u:n start:n :}   \ one '|' field from start -> field + next start
   a u $7C start SPLIT-NEXT             \ fa fu next found?
   0= if E-EVID-ROW-FIELDS throw then ; \ fa fu next

: EVID-ROW-SPLIT4 ( ptr u8 n -- ptr u8 n ptr u8 n ptr u8 n ptr u8 n ) {: a:ptr u:n :}
   a u 0  EVID-SPLIT1  {: s1:n :}       \ certify field slice on stack
   a u s1 EVID-SPLIT1  {: s2:n :}       \ + golden field slice
   a u s2 EVID-SPLIT1  {: s3:n :}       \ + gradcheck field slice
   a u s3 EVID-SPLIT1  {: s4:n :}       \ + profile field slice
   a u $7C s4 SPLIT-NEXT                \ a fifth field would over-fill the row
   if E-EVID-ROW-FIELDS throw then
   drop drop drop ;                     \ no fifth field: drop the empty split result

\ ---- one field's value after its expected "label=" prefix -----------------------
: EVID-FIELD-VALUE ( ptr u8 n ptr u8 n -- ptr u8 n ) {: fa:ptr fu:n la:ptr lu:n :}
   fa fu la lu STARTS-WITH? 0= if E-EVID-ROW-LABEL throw then
   fa lu +  fu lu - ;

\ ---- token -> typed family (fail closed on an out-of-domain token) --------------
: EVID-DECODE-VERDICT ( ptr u8 n -- verdict ) {: a:ptr u:n :}
   a u s" pass"    STR= if MAKI-VERDICT:PASS    exit then
   a u s" fail"    STR= if MAKI-VERDICT:FAIL    exit then
   a u s" not-run" STR= if MAKI-VERDICT:NOT-RUN exit then
   E-EVID-ROW-VERDICT throw ;

: EVID-DECODE-PREC ( ptr u8 n -- EVID:prec-class ) {: a:ptr u:n :}
   a u s" f32"  STR= if EVID-PREC--CLASS:PREC-F32  exit then
   a u s" tf32" STR= if EVID-PREC--CLASS:PREC-TF32 exit then
   E-EVID-ROW-PREC throw ;

\ ---- golden field: optional "device-" leg + verdict + optional ":<prec>" --------
: EVID-SPLIT-COLON ( ptr u8 n -- ptr u8 n ptr u8 n ) {: a:ptr u:n :}   \ "v:prec" -> (v) (prec); a device leg MUST carry precision
   a u $3A INDEX-OF MATCH option
     none OF E-EVID-ROW-PREC throw ENDOF
     some OF IDX>N ENDOF
   ;MATCH {: i:n :}
   a i  a i 1+ +  u i 1+ - ;

: EVID-DECODE-DEV ( ptr u8 n -- verdict EVID:prec-class )   \ "fail:tf32" -> verdict prec
   EVID-SPLIT-COLON {: pa:ptr pu:n :}
   EVID-DECODE-VERDICT
   pa pu EVID-DECODE-PREC ;

: EVID-DECODE-GOLD ( ptr u8 n -- EVID:golden-leg verdict EVID:prec-class ) {: a:ptr u:n :}
   a u s" device-" STARTS-WITH? if
      EVID-GOLDEN--LEG:DEVICE
      a EVID-DEV-PREFIX-LEN +  u EVID-DEV-PREFIX-LEN -  EVID-DECODE-DEV
   else
      EVID-GOLDEN--LEG:HOST         \ host leg carries no precision axis (store.f wire)
      a u EVID-DECODE-VERDICT
      EVID-PREC--CLASS:PREC-F32
   then ;

public

\ ---- typed render projections (typed evidence value -> the store's raw wire ids) --
\ These are the enum->code boundary the wire owner (maki/store.f) consumes; they let a
\ decoded value be re-rendered through the sealed EVID-PUT-G without duplicating the
\ on-disk format, and let callers assert a decoded value's fields against the V-*/PREC-*
\ vocabulary. Pure projections: no store write, so no new planting surface.
: EVID-VERDICT>CODE ( verdict -- n )
   MATCH verdict
      pass    OF V-PASS   ENDOF
      fail    OF V-FAIL   ENDOF
      not-run OF V-NOTRUN ENDOF
   ;MATCH ;

: EVID-LEG>DEV? ( EVID:golden-leg -- bool )            \ device leg drives the "device-" wire prefix
   MATCH golden-leg
      host     OF false ENDOF
      external OF false ENDOF
      device   OF true  ENDOF
   ;MATCH ;

: EVID-PREC>CODE ( EVID:prec-class -- n )
   MATCH prec-class
      prec-f32  OF PREC-F32  ENDOF
      prec-tf32 OF PREC-TF32 ENDOF
   ;MATCH ;

\ ---- the typed rehydration boundary --------------------------------------------
\ Decode a persisted evidence-row SUFFIX (the bytes EVID-GET returns, i.e. the row past
\ "<key>|") into its typed fields. Every field is validated through its family: a wrong
\ field count (E-EVID-ROW-FIELDS), an unknown field label (E-EVID-ROW-LABEL), an
\ out-of-domain verdict (E-EVID-ROW-VERDICT), or a bad/absent device precision
\ (E-EVID-ROW-PREC) rejects with a named throw. Returns, deepest first: certify verdict,
\ golden leg, golden verdict, licensed precision, gradcheck verdict, profile verdict.
: EVID-ROW-DECODE ( ptr u8 n -- verdict EVID:golden-leg verdict EVID:prec-class verdict verdict )
   EVID-ROW-SPLIT4
   {: ca:ptr cu:n ga:ptr gu:n gra:ptr gru:n pa:ptr pu:n :}
   ca cu   s" certify="   EVID-FIELD-VALUE EVID-DECODE-VERDICT
   ga gu   s" golden="    EVID-FIELD-VALUE EVID-DECODE-GOLD
   gra gru s" gradcheck=" EVID-FIELD-VALUE EVID-DECODE-VERDICT
   pa pu   s" profile="   EVID-FIELD-VALUE EVID-DECODE-VERDICT ;

;package
