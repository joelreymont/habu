\ regfile.f - the ARM64 register-file description src/compiler/native/a64ir.f
\ builds from src/compiler/native/regfile.f.

require src/lib/test.f
require src/compiler/native/regfile.f
require src/arch/arm64/machine.f
require src/compiler/native/a64ir.f

package REGFILE-TEST
private

: G-ALC ( NREGFILE:file -- n )     NREGFILE:GPR-ALLOCATABLE NREGFILE:REGS-BITS ;

public

: ARM64 ( -- )
   \ The engine's own registers are reserved and not allocatable, which is the
   \ one thing a description getting this wrong would break silently.
   A64IR:REGFILE G-ALC  A64M:ENGINE-GPRS and  0 T=
   A64IR:REGFILE G-ALC  1 A64M:LINK-GPR lshift and  0 T=
   A64IR:REGFILE G-ALC  1 A64M:ZERO-GPR lshift and  0 T= ;

: RUN ( -- )
   T-RESET
   ARM64
   T-REPORT ;

;package

REGFILE-TEST:RUN
