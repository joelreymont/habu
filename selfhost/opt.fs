\ opt.fs — a peephole optimizer for the standalone's token compiler: store-to-load
\ FORWARDING. A `ldr Rd,[x19]` immediately after a `str Rd,[x19]` is redundant (Rd still
\ holds the value) — drop it. Sound for the branchless arithmetic bodies walk.fs emits
\ (no branch can target the dropped ldr). Compacts CODE in place. Needs icode.fs + walk.fs
\ (CODE, CP, RD32). First step of porting caf's optimizer (opt.fs) to the standalone.
4290772992 constant STRMASK    \ 0xFFC00000  (str/ldr opcode + size bits)
4177526784 constant STRVAL     \ 0xF9000000  (str x?,[x?,#?])
4194304    constant LDRBIT     \ 0x00400000  (ldr = str | this)
variable WP
\ NB: a local named `i` resolves to the loop-index word I — name it `ix`.
: WGET {: ix :}  CODE ix 4 * + RD32 ;
: WSET {: w ix :}  CODE ix 4 * + WP !
   w 255 and WP @ c!  w 8 rshift 255 and WP @ 1 + c!
   w 16 rshift 255 and WP @ 2 + c!  w 24 rshift 255 and WP @ 3 + c! ;
variable OII  variable OOI  variable OCUR  variable ONXT
: OPT
   0 OII !  0 OOI !
   BEGIN OII @ CP @ < WHILE
     OII @ WGET OCUR !
     OII @ 1 + CP @ < IF OII @ 1 + WGET ELSE 0 THEN ONXT !
     OCUR @ STRMASK and STRVAL =  ONXT @ OCUR @ LDRBIT or =  and IF
       OCUR @ OOI @ WSET  OOI @ 1 + OOI !  OII @ 2 + OII !       \ keep str, drop ldr
     ELSE
       OCUR @ OOI @ WSET  OOI @ 1 + OOI !  OII @ 1 + OII !
     THEN
   REPEAT
   OOI @ CP ! ;
