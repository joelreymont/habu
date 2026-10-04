\ Owned bytes and rows survive later emission and a failed child context.
require lib/test.f
require lib/fs.f
require test/compiler/x64-chain-fixture.f
require src/compiler/session/emission.f

package X64CHAIN-TEST
private

TYPED-VARIABLE DEAD-ART NART:emission
TYPED-VARIABLE DEAD-CTX IR-CTX:ctx
TYPED-VARIABLE W-LEASE NLEASE:lease

: OWN-CALL ( IR-CTX:ctx -- NART:emission )
   HIR-MOD
   CALLEE-ENTRY BUILD-PCALLER
   1 1 NBACK:L-CALLED CHAIN-LINKED {: m:IR-BUILD:module :}
   NS m EMIT-SLOT NBACK:EMIT
   NS NART:COPY ;

: OWN-QUOT ( IR-CTX:ctx -- NART:emission )
   HIR-MOD BUILD-QUOTER
   0 1 CHAIN {: m:IR-BUILD:module :}
   NS m EMIT-SLOT NBACK:EMIT
   NS NART:COPY ;

: ART-DIGEST ( NART:emission -- CDIGEST:digest )
   {: e:NART:emission :}
   e NART:BYTES e NART:SIZE CDIGEST:COMPUTE ;

: CALL-ART ( NART:emission -- )
   {: e:NART:emission :}
   e NART:FUNCTIONS 1 T=
   e 0 NART:FUNCTION-OFFSET@ 0 T=
   e NART:CALL-SITES 1 T=
   e 0 NART:CALL-KIND@ NEMIT:CALL T=
   e 0 NART:CALL-TARGET@ CALLEE-ENTRY T=
   e NART:BYTES e 0 NART:CALL-SITE@ + c@ $E8 T=
   e NART:ADDR-SITES 0 T=
   e NART:RET-BYTES 0 T=
   e NART:PLACED? TTRUE
   e NART:PLACEMENT EMIT-SLOT T=
   e NART:BINDING WBND CBIND:SAME? TTRUE ;

: QUOT-ART ( NART:emission -- )
   {: e:NART:emission :}
   e NART:FUNCTIONS 2 T=
   e 1 NART:FUNCTION-OFFSET@ 0 > TTRUE
   e NART:CALL-SITES 0 T=
   e NART:ADDR-SITES 1 T=
   e 0 NART:ADDR-SITE-KIND@ X64IR:ADDR-CODE T=
   e 0 NART:ADDR-SITE@ e NART:SIZE < TTRUE ;

: CHILD-FAR ( IR-CTX:ctx -- )
   HIR-MOD FAR-ENTRY BUILD-PCALLER
   1 1 NBACK:L-CALLED CHAIN-LINKED 0 W-MOD !
   FAR-EMIT ;

: CHILD-CONTEXT ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c DEAD-CTX !
   W-LEASE @ [: OWN-QUOT ;] c CASE-CONTEXT DEAD-ART !
   W-LEASE @ [: CHILD-FAR ;] c CASE-CONTEXT ;

: FAIL-CHILD ( -- )
   WBND [: CHILD-CONTEXT ;] IR-CTX:WITH-CONTEXT ;

: DEAD-SIZE ( -- )
   DEAD-ART @ NART:SIZE drop ;

: DEAD-BYTES ( -- )
   DEAD-ART @ NART:BYTES drop ;

: OWNED-CONTEXT ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   W-LEASE @ [: OWN-CALL ;] c CASE-CONTEXT {: first:NART:emission :}
   first ART-DIGEST {: digest:CDIGEST:digest :}
   W-LEASE @ [: OWN-QUOT ;] c CASE-CONTEXT {: quot:NART:emission :}
   quot QUOT-ART
   first CALL-ART
   first ART-DIGEST digest CDIGEST-DIGEST:EQ TTRUE
   [: FAIL-CHILD ;] E-X64EMIT-REACH TTHROWSQ
   DEAD-CTX @ IR-CTX:LIVE? TFALSE
   [: DEAD-SIZE ;] E-IR-ARENA-STALE TTHROWSQ
   [: DEAD-BYTES ;] E-IR-ARENA-STALE TTHROWSQ
   first CALL-ART
   quot QUOT-ART
   W-LEASE @ [: OWN-CALL ;] c CASE-CONTEXT ART-DIGEST
      digest CDIGEST-DIGEST:EQ TTRUE
   s" emission-owned.bin" TMP-PATH first NART:BYTES first NART:SIZE WRITE-ALL
   first DEAD-ART !
   first NART:RELEASE
   [: DEAD-SIZE ;] E-IR-ARENA-STALE TTHROWSQ
   [: DEAD-BYTES ;] E-IR-ARENA-STALE TTHROWSQ ;

: OWNED-LEASE ( NLEASE:lease -- )
   W-LEASE !
   WBND [: OWNED-CONTEXT ;] IR-CTX:WITH-CONTEXT ;

public

: RUN-OWNED ( -- )
   T-RESET
   s" retained emission survives overwrite, retirement and a failed child" T-LABEL
   [: OWNED-LEASE ;] NLEASE:WITH
   T-REPORT ;

;package

X64CHAIN-TEST:RUN-OWNED
