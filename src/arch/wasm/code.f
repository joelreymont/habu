\ code.f - WCODE, the bytes and addresses Wasm code is written with.

require src/arch/wasm/profile.f
require src/habu/layout.f

package WCODE
public

$00 constant OP-UNREACHABLE
$01 constant OP-NOP
$02 constant OP-BLOCK
$03 constant OP-LOOP
$04 constant OP-IF
$05 constant OP-ELSE
$0B constant OP-END
$0C constant OP-BR
$0D constant OP-BR-IF
$0F constant OP-RETURN
$10 constant OP-CALL
$11 constant OP-CALL-INDIRECT
$1A constant OP-DROP
$1B constant OP-SELECT
$20 constant OP-LOCAL-GET
$21 constant OP-LOCAL-SET
$22 constant OP-LOCAL-TEE
$28 constant OP-I32-LOAD
$29 constant OP-I64-LOAD
$2B constant OP-F64-LOAD
$31 constant OP-I64-LOAD8-U
$36 constant OP-I32-STORE
$37 constant OP-I64-STORE
$39 constant OP-F64-STORE
$3C constant OP-I64-STORE8
$40 constant OP-MEMORY-GROW          \ followed by its memory index
$41 constant OP-I32-CONST
$42 constant OP-I64-CONST
$44 constant OP-F64-CONST
$45 constant OP-I32-EQZ
$46 constant OP-I32-EQ
$47 constant OP-I32-NE
$4B constant OP-I32-GT-U
$4F constant OP-I32-GE-U
$50 constant OP-I64-EQZ
$51 constant OP-I64-EQ
$52 constant OP-I64-NE
$53 constant OP-I64-LT-S
$54 constant OP-I64-LT-U
$55 constant OP-I64-GT-S
$56 constant OP-I64-GT-U
$57 constant OP-I64-LE-S
$59 constant OP-I64-GE-S
$61 constant OP-F64-EQ
$62 constant OP-F64-NE
$63 constant OP-F64-LT
$64 constant OP-F64-GT
$6A constant OP-I32-ADD
$6B constant OP-I32-SUB
$72 constant OP-I32-OR
$74 constant OP-I32-SHL
$7C constant OP-I64-ADD
$7D constant OP-I64-SUB
$7E constant OP-I64-MUL
$7F constant OP-I64-DIV-S
$81 constant OP-I64-REM-S
$83 constant OP-I64-AND
$84 constant OP-I64-OR
$85 constant OP-I64-XOR
$86 constant OP-I64-SHL
$88 constant OP-I64-SHR-U
$99 constant OP-F64-ABS
$9A constant OP-F64-NEG
$9F constant OP-F64-SQRT
$A0 constant OP-F64-ADD
$A1 constant OP-F64-SUB
$A2 constant OP-F64-MUL
$A3 constant OP-F64-DIV
$A7 constant OP-I32-WRAP-I64
$AD constant OP-I64-EXTEND-I32-U
$B9 constant OP-F64-CONVERT-I64-S

$FC constant OP-MISC                 \ the prefix each MISC- opcode follows
6 constant MISC-I64-TRUNC-SAT-F64-S
$0A constant MISC-MEMORY-COPY
11 constant MISC-MEMORY-FILL         \ after OP-MISC, followed by its memory index

$7F constant I32
$7E constant I64
$7C constant F64
$40 constant EMPTY-TYPE              \ a block, loop or if taking and leaving nothing

\ A load's or store's alignment exponent.
0 constant ALIGN-8
2 constant ALIGN-32
3 constant ALIGN-64

\ Tier 0's return and loop stack depths, i64 cells in a module's memory.
WPROF:DATA-BASE RSP-CELL + constant RDEPTH-AT
WPROF:DATA-BASE LOOPSP-CELL + constant LDEPTH-AT

;package
