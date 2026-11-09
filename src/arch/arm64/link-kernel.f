\ link-kernel.f - the arm64 kernel after a product's start-up, the map from the
\ running hb's kernel primitive entries to the product kernel's, and the cut
\ that keeps only the kernel code a program reaches. The two kernels bake
\ different inputs, so the map goes by name and wordlist.
require src/lib/le.f
require src/lib/string.f
require src/arch/arm64/asm.f
require src/arch/arm64/icode.f
require src/arch/arm64/mnem.f
require src/habu/layout.f
require src/habu/xref.f

HB-TARGET-LINUX? HB-TARGET-MACOS? or 0= [if] s" link-kernel: unknown target" 76 die [then]
HB-TARGET-LINUX? [if] require src/os/linux/sys.f [then]
HB-TARGET-MACOS? [if] require src/os/macos/sys.f [then]

require src/os/script-argv.f
require src/habu/rt.f
require src/habu/crash.f

HB-TARGET-LINUX? [if]
   require src/os/linux/elf.f
   require src/os/linux/sign.f
   require src/os/linux/proc-watch.f
   require src/os/linux/proc-control.f
[then]
HB-TARGET-MACOS? [if]
   require src/os/macos/macho.f
   require src/os/macos/sign2.f
   require src/os/macos/proc-watch.f
   require src/os/macos/proc-control.f
[then]

require src/habu/regalloc.f
require src/habu/habu1.f
require src/habu/jit.f
require src/habu/prof.f
require src/habu/fdio.f
require src/habu/aot-decl.f
require src/habu/aot-ident.f
require src/habu/aot-owned.f
require src/habu/habu2.f
using ENGINE-EMIT

package LINKER
private
package ARM64

\ The host-entry table, ascending by host entry: HOSTS[i] is where a running
\ hb's kernel primitive enters, ROWS[i] the product kernel row of that primitive.
DYNAMIC-BUFFER HOSTS n
DYNAMIC-BUFFER ROWS n
variable N-HOSTS


public

\ The running hb's kernel is [rbase, CODE-END-CELL).
: HOST-KERNEL? ( n -- bool )
   {: a:n :}
   a rbase >= a data-base CODE-END-CELL + @ < and ;

private

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


\ The kernel's first word position. Nothing emits code between KERNEL and CUT,
\ so the region CUT edits is [K0, ASM-CP).
variable K0

\ Labels whose kernel code the program reaches besides the start-up's
\ references, as LABEL>N.
DYNAMIC-BUFFER ROOTS n
variable N-ROOTS

\ A node runs from K0 or a label bound in the region to the next one or ASM-CP;
\ NODE-OF holds each word's node, STARTS each node's first, KEPT [start, end).
DYNAMIC-BUFFER NODE-OF n
DYNAMIC-BUFFER STARTS n
DYNAMIC-BUFFER REACHED n
DYNAMIC-BUFFER WORK n
DYNAMIC-BUFFER KEPT n
variable N-NODES
variable N-WORK
variable N-KEPT


: REGION-END ( -- n ) A64ICODE:ASM-CP @ ;

: IN-REGION? ( n -- bool )
   {: w:n :}
   w K0 @ >=  w REGION-END < and ;

: LABEL-POS ( label -- n ) LABEL>N A64ICODE:LBLP @ ;


\ NODE-OF holds 1 at each node's start and 0 elsewhere until NUMBER.
: MARK-STARTS ( -- )
   REGION-END K0 @ - {: n:n :}
   n NODE-OF-RESERVE
   n 0 ?do 0 i NODE-OF ! loop
   1 0 NODE-OF !
   A64ICODE:NLBL @ 0 ?do
      i A64ICODE:LBLP @ dup IN-REGION? if K0 @ - 1 swap NODE-OF ! else drop then
   loop ;


: NUMBER ( -- )
   -1
   REGION-END K0 @ - 0 ?do
      i NODE-OF @ 0<> if
         1 +  dup 1 + STARTS-RESERVE  i K0 @ + over STARTS !
      then
      dup i NODE-OF !
   loop
   1 + N-NODES ! ;


: NODE-END ( n -- n )
   1 + dup N-NODES @ < if STARTS @ exit then
   drop REGION-END ;


\ Word position p reaches the node holding it, once; a position outside the
\ region, an unbound label's -1 among them, reaches nothing.
: REACH ( n -- )
   {: p:n :}
   p IN-REGION? 0= if exit then
   p K0 @ - NODE-OF @ {: node:n :}
   node REACHED @ 0<> if exit then
   1 node REACHED !
   N-WORK @ 1 + WORK-RESERVE
   node N-WORK @ WORK !
   1 N-WORK +! ;


\ The instruction word at word position w, little-endian.
: WORD@ ( n -- n ) A64ICODE:CW@ LE:U32@ ;


\ ret, brk, or b or br whatever its field holds: control never passes on. The
\ field masks are derived as A64ICODE's FX-CLEAR derives its own.
: TERMINAL? ( n -- bool )
   {: w:n :}
   w A64ASM:ENC-RET =
   w A64ASM:ENC-BRK = or
   w  -4 A64ASM:ENC-B 0 A64ASM:ENC-B xor invert and  0 A64ASM:ENC-B = or
   w  31 A64ASM:ENC-BR 0 A64ASM:ENC-BR xor invert and  0 A64ASM:ENC-BR = or ;


\ The first reference record whose site is at or after word position a; records
\ ascend by site.
: FIRST-REF ( n -- n )
   {: a:n :}
   0 A64ICODE:REFS
   begin 2dup < while
      2dup + 2 /
      dup A64ICODE:REF-SITE a < if rot drop 1 + swap else nip then
   repeat
   drop ;


\ A node reaches the labels its sites name and, unless its last word is
\ terminal, the node after it: a bl to a word that never returns counts as
\ falling through, so reach is an upper bound.
: VISIT ( n -- )
   {: node:n :}
   node STARTS @ {: a:n :}
   node NODE-END {: b:n :}
   a FIRST-REF
   begin dup A64ICODE:REFS < if dup A64ICODE:REF-SITE b < else false then while
      dup A64ICODE:REF-LABEL LABEL-POS REACH  1 +
   repeat drop
   b REGION-END < if b 1 - WORD@ TERMINAL? 0= if b REACH then then ;


\ The start-up's references, the records sited below K0, root the walk; boot.f
\ names no root. START, ends in an exit syscall, so nothing falls into node 0.
: ROOT-STARTUP ( -- )
   K0 @ FIRST-REF 0 ?do i A64ICODE:REF-LABEL LABEL-POS REACH loop ;


: ROOT-LABELS ( -- )
   N-ROOTS @ 0 ?do i ROOTS @ >LABEL LABEL-POS REACH loop ;


: WALK ( -- )
   MARK-STARTS NUMBER
   N-NODES @ REACHED-RESERVE
   N-NODES @ 0 ?do 0 i REACHED ! loop
   0 N-WORK !
   ROOT-STARTUP ROOT-LABELS
   begin N-WORK @ 0 > while
      -1 N-WORK +!  N-WORK @ WORK @ VISIT
   repeat ;


\ Appends node n's words to KEPT, extending the last pair when they adjoin.
: KEEP-NODE ( n -- )
   {: node:n :}
   node STARTS @ {: a:n :}
   node NODE-END {: b:n :}
   N-KEPT @ 0 > if
      N-KEPT @ 2 * 1 - KEPT @ a = if b N-KEPT @ 2 * 1 - KEPT ! exit then
   then
   N-KEPT @ 1 + 2 * KEPT-RESERVE
   a N-KEPT @ 2 * KEPT !
   b N-KEPT @ 2 * 1 + KEPT !
   1 N-KEPT +! ;


public

\ A bound label's byte offset in the code buffer.
: OFFSET ( label -- n )
   {: l:label :}
   l A64ICODE:LBL-BOUND? 0= if
      s" link: a label the link reads is unbound" 74 die
   then
   l LABEL-POS 4 * ;


\ A product kernel row's entry, in bytes from the start of the code buffer; a
\ row whose code CUT removed dies in OFFSET.
: PRIM-OFFSET ( n -- n ) ENGINE-PRIMS:FIRST-LABEL OFFSET ;


\ Code emitted between OPEN and KERNEL, the start-up, precedes the kernel; the
\ product's own code follows KERNEL. No C2 entry is bound.
: OPEN ( -- )
   0 0= STDIN? !
   NULL$ ENGINE-EMIT:OPEN ;

: KERNEL ( -- )
   A64ICODE:ASM-CP @ K0 !  0 N-ROOTS !
   ENGINE-EMIT:KERNEL HOST-ENTRIES ;


\ The kernel's first byte, where the start-up ends; CUT keeps the start-up whole.
: KERNEL-AT ( -- n ) K0 @ 4 * ;


\ The product kernel row of the primitive that enters the running hb at the
\ host address, or -1 when no host-entry row enters exactly there.
: KERNEL-ROW ( n -- n )
   FIND dup 0 < if exit then
   ROWS @ ;

\ That primitive's product kernel offset, or -1.
: KERNEL-ENTRY ( n -- n )
   KERNEL-ROW dup 0 < if exit then
   PRIM-OFFSET ;


\ Keeps the kernel code at a label; one bound outside the kernel keeps nothing.
: ROOT-LABEL ( label -- )
   N-ROOTS @ 1 + ROOTS-RESERVE
   LABEL>N N-ROOTS @ ROOTS !
   1 N-ROOTS +! ;


\ True when host address a is a kernel primitive's entry, whose code it then
\ keeps.
: ROOT-ENTRY ( n -- bool )
   FIND dup 0 < if drop false exit then
   ROWS @ ENGINE-PRIMS:FIRST-LABEL ROOT-LABEL true ;


\ Removes the kernel nodes WALK does not reach and moves the rest down.
: CUT ( -- )
   WALK
   0 N-KEPT !  2 KEPT-RESERVE
   N-NODES @ 0 ?do i REACHED @ 0<> if i KEEP-NODE then loop
   0 KEPT N-KEPT @ K0 @ A64ICODE:CUT
   NODE-OF-RELEASE STARTS-RELEASE REACHED-RELEASE WORK-RELEASE KEPT-RELEASE ;

;package
;package
;using
