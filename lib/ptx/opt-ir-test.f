\ opt-ir-test.f - fixtures for the PTX instruction-table IR parser + renderer.
\
\ Proves line classification (pure vs opaque vs directive/label/brace/predicated),
\ operand parsing, fail-closed passthrough of unmodelled lines, and byte-exact
\ round-trip. Load after lib/ptx/opt-ir.f. Checked Habu.

require lib/ptx/test-prelude.f
require lib/ptx/opt-ir.f

T-RESET

\ parse one line and expose line-0 fields
: OIT-1 ( ptr u8 n -- ) PTX-PARSE ;
: OIT-KIND ( -- n )  0 OPTX.KIND OPTX@ ;
: OIT-CLASS ( -- n ) 0 OPTX.CLASS OPTX@ ;
: OIT-NSRC ( -- n )  0 OPTX.NSRC OPTX@ ;

: OITT-COUNTS ( -- )
   s\" .visible .entry SAXPY\n{\nmul.rn.f32 %f4, %f1, %f2;\nadd.rn.f32 %f4, %f4, %f3;\nst.global.f32 [%rd5], %f4;\n}\n" PTX-PARSE
   OPTX-N @ 6 T=              \ entry, {, mul, add, st, }
   PTX-INSN-COUNT 3 T=        \ mul, add, st (directive/braces excluded)
   OPTX-SYM-N @ 4 T= ;        \ %f4 %f1 %f2 %f3 (st operands are opaque, not interned)

: OITT-CLASS-BIN ( -- )
   s\" add.rn.f32 %f4, %f1, %f2;\n" OIT-1
   OIT-KIND 1 T=  OIT-CLASS OPTX-C-BIN T=  OIT-NSRC 2 T= ;
: OITT-CLASS-TERN ( -- )
   s\" fma.rn.f32 %f10, %f26, %f30, %f10;\n" OIT-1
   OIT-KIND 1 T=  OIT-CLASS OPTX-C-TERN T=  OIT-NSRC 3 T= ;
: OITT-CLASS-UNARY ( -- )
   s\" cvt.rna.tf32.f32 %r50, %f26;\n" OIT-1
   OIT-KIND 1 T=  OIT-CLASS OPTX-C-UNARY T= ;
: OITT-CLASS-MOVIMM ( -- )
   s\" mov.f32 %f2, 0f3F800000;\n" OIT-1
   OIT-KIND 1 T=  OIT-CLASS OPTX-C-MOVIMM T= ;
: OITT-CLASS-MOVREG ( -- )
   s\" mov.f32 %f2, %f1;\n" OIT-1
   OIT-KIND 1 T=  OIT-CLASS OPTX-C-MOVREG T= ;
: OITT-SPECIAL-OPAQUE ( -- )   \ mov of a special register is NOT a copy (opaque)
   s\" mov.u32 %r2, %ctaid.x;\n" OIT-1
   OIT-KIND 0 T= ;
: OITT-LOAD-OPAQUE ( -- )
   s\" ld.global.f32 %f2, [%rd4];\n" OIT-1  OIT-KIND 0 T= ;
: OITT-PRED-OPAQUE ( -- )
   s\" @%p1 bra DONE;\n" OIT-1  OIT-KIND 0 T=  0 OPTX.PRED OPTX@ 1 T= ;
: OITT-MEM-OPAQUE ( -- )       \ a memory operand makes even an arithmetic mnemonic opaque
   s\" mul.wide.u32 %rd3, %r5, [%rd4];\n" OIT-1  OIT-KIND 0 T= ;
: OITT-UNKNOWN-OPAQUE ( -- )   \ fail-closed: unmodelled mnemonic is passthrough
   s\" wibble.f32 %f2, %f1;\n" OIT-1  OIT-KIND 0 T= ;

\ byte-exact round-trip of a module that mixes directives, labels, braces, opaque
: OITT-ROUNDTRIP ( -- )
   s\" .version 8.3\n.visible .entry K\n{\nadd.rn.f32 %f4, %f1, %f2;\nDONE:\nret;\n}\n" PTX-PARSE
   PTX-RENDER PTX-RENDER$
   s\" .version 8.3\n.visible .entry K\n{\nadd.rn.f32 %f4, %f1, %f2;\nDONE:\nret;\n}\n" STR= TTRUE ;

OITT-COUNTS
OITT-CLASS-BIN
OITT-CLASS-TERN
OITT-CLASS-UNARY
OITT-CLASS-MOVIMM
OITT-CLASS-MOVREG
OITT-SPECIAL-OPAQUE
OITT-LOAD-OPAQUE
OITT-PRED-OPAQUE
OITT-MEM-OPAQUE
OITT-UNKNOWN-OPAQUE
OITT-ROUNDTRIP

T-REPORT
