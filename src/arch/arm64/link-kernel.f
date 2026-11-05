\ link-kernel.f - the arm64 kernel after a product's start-up, and the map from
\ the running hb's kernel primitive entries to the product kernel's. The two
\ kernels bake different inputs, so the map goes by name and wordlist.
require src/lib/string.f
require src/arch/arm64/asm.f
require src/arch/arm64/icode.f
require src/arch/arm64/mnem.f
require src/habu/layout.f
require src/habu/xref.f

package LINKER
private
: LOAD-SYS ( -- )
   HB-TARGET-LINUX? if s" src/os/linux/sys.f" required exit then
   HB-TARGET-MACOS? if s" src/os/macos/sys.f" required exit then
   s" link-kernel: unknown target" 76 die ;
' LOAD-SYS
;package
execute

require src/os/script-argv.f
require src/habu/rt.f
require src/habu/crash.f

package LINKER
private
: LOAD-IMAGE ( -- )
   HB-TARGET-LINUX? if
      s" src/os/linux/elf.f" required
      s" src/os/linux/sign.f" required
      s" src/os/linux/proc-watch.f" required
      s" src/os/linux/proc-control.f" required exit
   then
   HB-TARGET-MACOS? if
      s" src/os/macos/macho.f" required
      s" src/os/macos/sign2.f" required
      s" src/os/macos/proc-watch.f" required
      s" src/os/macos/proc-control.f" required exit
   then
   s" link-kernel: unknown target" 76 die ;
' LOAD-IMAGE
;package
execute

require src/habu/regalloc.f
require src/habu/habu1.f
require src/habu/jit.f
require src/habu/prof.f
require src/habu/fdio.f
require src/habu/aot-decl.f
require src/habu/aot-ident.f
require src/habu/aot-owned.f
require src/habu/habu2.f

package LINKER
private
package ARM64

\ The host-entry table, ascending by host entry: HOSTS[i] is where a running
\ hb's kernel primitive enters, ROWS[i] the product kernel row of that primitive.
DYNAMIC-BUFFER HOSTS n
DYNAMIC-BUFFER ROWS n
variable N-HOSTS


\ The running hb's own code is [rbase, CODE-END-CELL).
: HOST-KERNEL? ( n -- bool )
   {: a:n :}
   a rbase >= a data-base CODE-END-CELL + @ < and ;


: SHIFT-UP ( n -- )
   {: j:n :}
   j 1 - HOSTS @ j HOSTS !
   j 1 - ROWS @ j ROWS ! ;


: INSERT ( n n -- )
   {: host:n row:n :}
   N-HOSTS @ {: k:n :}
   k 1 + HOSTS-RESERVE
   k 1 + ROWS-RESERVE
   k begin dup 0 > if dup 1 - HOSTS @ host > else false then while
      dup SHIFT-UP 1 -
   repeat {: at:n :}
   host at HOSTS !
   row at ROWS !
   k 1 + N-HOSTS ! ;


\ A seeded kernel record enters at its text base plus its kernel offset
\ (habu2.f EM-SEED-DICT); a package row's start holds its wid, never code.
: KEEP ( ptr n -- )
   {: rec:ptr :}
   rec XREF-START {: start:n :}
   start HOST-KERNEL? 0= if exit then
   rec XREF-NAME$ rec XREF-WORDLIST AOT-PRIM-INDEX {: row:n :}
   row 0 < if exit then
   start row INSERT ;


: HOST-ENTRIES ( -- )
   0 N-HOSTS !
   ndict@ 0 ?do i XREF-REC KEEP loop ;


\ The table index whose host entry is a, or -1.
: FIND ( n -- n )
   {: a:n :}
   0 N-HOSTS @
   begin 2dup < while
      2dup + 2 /
      dup HOSTS @ a = if nip nip exit then
      dup HOSTS @ a < if rot drop 1 + swap else nip then
   repeat
   2drop -1 ;


public

\ A product kernel row's entry, in bytes from the start of the code buffer.
: PRIM-OFFSET ( n -- n )
   ENGINE-PRIMS:FIRST-LABEL LABEL>N A64ICODE:LBLP @ 4 * ;


\ Code emitted between OPEN and KERNEL, the start-up, precedes the kernel; the
\ product's own code follows KERNEL. No C2 entry is bound.
: OPEN ( -- )
   0 0= STDIN? !
   NULL$ ENGINE-EMIT:OPEN ;

: KERNEL ( -- ) ENGINE-EMIT:KERNEL HOST-ENTRIES ;


\ The product kernel offset of the primitive that enters the running hb at the
\ host address, or -1 when no kept primitive enters exactly there.
: KERNEL-ENTRY ( n -- n )
   FIND dup 0 < if exit then
   ROWS @ PRIM-OFFSET ;

;package
;package
