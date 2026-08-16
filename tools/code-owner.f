\ code-owner.f - turn a live code address into the record that owns it.
\
\ WHAT IT IS FOR. A debugger stop, a crash dump and a breakpoint all hand back
\ numbers: a pc, a saved x30, a value out of a defer cell. In a seeded engine
\ those addresses are in the JIT region, where no external symbol table reaches
\ them - `nm` and lldb see the loaded __text and nothing else - so the question
\ "which word is that?" had no tool and was answered by arithmetic on a
\ disassembly. The dictionary already holds the answer: every record carries its
\ routine's start and length, so the owner of an address is the record whose span
\ contains it. This asks that, and it runs INSIDE the engine under study, which is
\ the only process where those records exist.
\
\ IT REPORTS EVERY OWNER, NOT THE FIRST. `EXPORT` publishes a second record over
\ one routine's code and a republication leaves an early record pointing at late
\ code, so an address can legitimately sit inside more than one span - and which
\ of them a reader wants is the reader's business, not this tool's. A single
\ answer would have to pick, and picking is how the sealed-WID gate's own
\ predecessor got the wrong wordlist (src/habu/habu2.f EM-AOTWIDGATE).
\
\ THE OFFSET IS WHAT MAKES IT USABLE ACROSS RUNS. ASLR moves the region every
\ boot, so an address caught in one process means nothing in the next; `off=` is
\ the distance into the owning routine, which is stable, and REGION-OWNER. takes
\ its argument that way - as a region offset - so a number read out of one run can
\ be resolved in another.
\
\ Run inside the engine being studied:
\   <engine> --load tools/code-owner.f -- <hex-or-decimal region offset>
\ or require it and call CODE-OWNER:AT. / CODE-OWNER:REGION-OWNER. directly.

package CODE-OWNER

private

variable HITS

: SPAN? ( n ptr a -- bool ) {: a:n r:ptr :}
   r XREF-LEN 0= if 0 0= 0= exit then
   r XREF-WORDLIST XREF-NAMESPACE-WL = if 0 0= 0= exit then   \ a package row's [0] is a wid, not code
   r XREF-RETIRED? if 0 0= 0= exit then
   a r XREF-START >=  a r XREF-START r XREF-LEN + <  and ;

: ROW. ( n ptr a -- ) {: a:n r:ptr :}
   s" owner=" type r XREF-NAME$ type
   s"  wid=" type r XREF-WORDLIST .
   s"  start=" type r XREF-START .
   s"  len=" type r XREF-LEN .
   s"  off=" type a r XREF-START - . cr ;

public

\ The live region base this run got, so a caller can turn its own offsets into
\ addresses the way REGION-OWNER. does.
: BASE ( -- n ) dbase@ ;

\ Every record whose routine contains `a`. Prints one line each and answers how
\ many there were, so a caller can tell "no owner" (a primitive in the loaded
\ __text, or an address in no routine at all) from a silent success.
: AT ( n -- n ) {: a:n :}
   0 HITS !
   ndict@ 0 ?do
      i XREF-REC {: r:ptr :}
      a r SPAN? if  a r ROW.  1 HITS +!  then
   loop
   HITS @ ;

: AT. ( n -- ) {: a:n :}
   s" addr=" type a .
   s"  region-off=" type a BASE - . cr
   a AT 0= if s" no record owns it - a primitive in __text, or not in a routine" type cr then ;

: REGION-OWNER. ( n -- ) {: off:n :}
   BASE off + AT. ;

;package
