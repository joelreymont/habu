\ snap.f — image writer run by bin/hb (or by a temporary stdin engine during
\ the native build-fixpoint driver):
\ writes a new binary = engine text copy + the LIVE dict/code region + the LIVE
\ data region + a 40-byte trailer. The engine's startup loader (em-startup)
\ detects the trailer, restores both regions (fixed VAs make region addresses
\ valid as-is), relocates engine-text call chains, and boots WARM — zero
\ recompile. The output is unsigned; tools/snap-hb.sh codesigns it.
$48425350414E5321 constant SNAP-MAGIC

\ output path — the single knob; tools/snap-hb.sh owns/moves the artifact
: SNAP-OUT s" hb-snap0" TMP-PATH ;

create TRL 40 allot
create ZPG 4096 allot
variable WOF
variable ZREM
variable ZC
variable STB  variable STSZ  variable SDB  variable SCL  variable SDL
variable SNL  variable SFTS  variable SFD
: STB@ STB @ ;
s" STB@" s" -- ptr u8" TRUST
: SDB@ SDB @ ;
s" SDB@" s" -- ptr u8" TRUST

: WALL {: fd a:ptr u :}
   0 WOF !
   BEGIN WOF @ u < WHILE
     fd  a WOF @ +  u WOF @ -  write
     dup 0 > 0= IF s" snap: write failed" 74 die THEN
     WOF @ + WOF !
   REPEAT ;

: WPAD {: fd n :}
   n ZREM !
   BEGIN ZREM @ 0 > WHILE
     ZREM @ 4096 > IF 4096 ELSE ZREM @ THEN ZC !
     fd ZPG ZC @ WALL
     ZREM @ ZC @ - ZREM !
   REPEAT ;

: SNAP-HDR
   \ NB: the toolchain's DSL constant RBASE (=x20) shadows the `rbase` prim
   \ after case folding — read the saved text base straight from its cell.
   $340000000 RBASE-CELL + @ STB !        \ text CONTENT base
   STB @ $1000 - 216 + @ STSZ !           \ our own __text size ([loadbase+216])
   dbase@ SDB !
   cp@ SDB @ - SCL !                      \ region payload (dict + compiled code)
   here $340000000 - SDL !                \ data payload (through DP)
   STSZ @ SCL @ + SDL @ + 40 + SNL !      \ new __text content size
   CODE-OFF SNL @ + $3FFF + $3FFF invert and SFTS !   \ aligned file/vm text size
   \ header (fits MBUF) — BUILD-MACHO's layout with SFTS in place of MPAGE
   M-RESET  0 NCMDS !
   MH-HDR,
   s" __PAGEZERO" 0 VMBASE 0 0 0 0 0 SEG,  LC+
   s" __TEXT" VMBASE SFTS @ 0 SFTS @ 5 1 80 SEG,  LC+
      s" __text" s" __TEXT" VMBASE CODE-OFF + SNL @ CODE-OFF 2 $80000400 SECT,
   M-HERE LE-OFF !
   s" __LINKEDIT" VMBASE SFTS @ + MPAGE SFTS @ 0 1 0 0 SEG,  LC+
   DYLINKER,  LC+   CODE-OFF MAIN,  LC+   DYLIB,  LC+
   PATCH-HDR
   CODE-OFF M-PAD ;
: SNAPGO
   SNAP-HDR
   \ trailer: magic, old text base, dict count, region length, data length
   SNAP-MAGIC TRL !  STB @ TRL 8 + !  ndict@ TRL 16 + !
   SCL @ TRL 24 + !  SDL @ TRL 32 + !
   \ stream: header, engine text, region, data, trailer, zero pad
   SNAP-OUT PATH0 1537 493 open SFD !
   SFD @ MBUF CODE-OFF WALL
   SFD @ STB@ STSZ @ WALL
   SFD @ SDB@ SCL @ WALL
   SFD @ $340000000 SDL @ WALL
   SFD @ TRL 40 WALL
   SFD @  SFTS @ CODE-OFF - SNL @ -  WPAD
   SFD @ close ;

\ Freeze the verify-on-definition hook into the emitted image: hb is fully
\ loaded, so a typed def in its REPL is checked against its sig.
: HOOK! CHECK! ;
' HOOK! set-check
SNAPGO
