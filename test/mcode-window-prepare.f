\ mcode-window-prepare.f - the owners' window for tests that run minted or
\ emitted machine code, loaded last in test/native-window-owner-child.f's list.
\ Executing written bytes needs two owner-private rows: the bounded FFI calls,
\ which take a code address (package FFI), and code-publish, the code arena's
\ append window (package NPUB); outside its owner a checked caller is refused by
\ name.

\ src/lib/ffi-abi.f requires src/lib/adt/option.f, whose ENUM dies on the unset
\ TYPE-DECL:TDECL-TXN-XT until the last declaration participant installs the
\ declaration runner, so both participants load before any library.
\
\ This file reopens both owners before src/core/internal-mark.f seals the window
\ and publishes one public word per row. The sealed product refuses such a
\ reopen, so its children run on the unsealed engine test/whitebox-child.f
\ names.
require src/core/prefix-boundary.f
include src/core/generated-declaration-dictionary.f
include src/core/generated-declaration-protection.f

require src/lib/ffi-abi.f

package FFI
public

\ argc fn: the integer registers FFI:VALUE! / READABLE! / WRITABLE! staged.
: CALL-AT ( n n -- n )
   {: argc:n fn:n :}
   FFI-BUF FFI-REG-LEN-BUF argc fn ffi-call-bounded ;

\ spills fn: integer, float and stack-spill slots; the integer x0 returns.
: CALL-ABI-AT ( n n -- n )
   {: spills:n fn:n :}
   FFI-BUF FFI-FBUF FFI-STACK-BUF FFI-REG-LEN-BUF FFI-STACK-LEN-BUF
   spills fn ffi-call-abi-bounded ;

\ spills fn: as CALL-ABI-AT; the float d0 returns.
: CALL-ABI-R-AT ( n n -- r )
   {: spills:n fn:n :}
   FFI-BUF FFI-FBUF FFI-STACK-BUF FFI-REG-LEN-BUF FFI-STACK-LEN-BUF
   spills fn ffi-call-abi-r-bounded ;

;package

package NPUB
public

\ src dst len: append len bytes at the free code slot dst (cp@) and advance it.
: WRITE-AT ( ptr u8 n n -- ) code-publish ;

;package

include src/core/internal-mark.f
