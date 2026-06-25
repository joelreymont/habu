\ snap.f — image writer run by bin/hb (or by a temporary stdin engine during
\ the native build-fixpoint driver):
\ writes a new binary = engine text copy + the LIVE dict/code region + the LIVE
\ data region + a 40-byte trailer. The engine's startup loader (em-startup)
\ detects the trailer, restores both regions (fixed VAs make region addresses
\ valid as-is), relocates engine-text call chains, and boots WARM — zero
\ recompile. The native build-fixpoint driver signs and installs the output.

\ output path — the single knob; build-fixpoint owns/moves the artifact
: SNAP-OUT s" hb-snap0" TMP-PATH ;

create TRL 40 allot
create ZPG 4096 allot
variable ZREM
variable ZC
variable STB  variable STSZ  variable SDB  variable SCL  variable SDL
variable SNL  variable SFTS  variable SFD
: STB@ STB @ ;
s" STB@" s" -- ptr u8" TRUST
: STB-CELL@ STB @ ;
s" STB-CELL@" s" -- ptr n" TRUST
: SDB@ SDB @ ;
s" SDB@" s" -- ptr u8" TRUST

: WPAD {: fd n :}
   n ZREM !
   BEGIN ZREM @ 0 > WHILE
     ZREM @ 4096 > IF 4096 ELSE ZREM @ THEN ZC !
     fd ZPG ZC @ DRV-WALL
     ZREM @ ZC @ - ZREM !
   REPEAT ;

: SNAP-HDR
   \ The builder's x20 register constant is XREG-RBASE so it does not shadow
   \ the `rbase` primitive; read the saved text base straight from its cell.
   data-base RBASE-CELL + @ STB !         \ text CONTENT base
   STB-CELL@ CODE-OFF - IMAGE-TEXT-SIZE-OFF + @ IMAGE-TEXT-CONTENT-ADJ - STSZ !  \ own text content size
   dbase@ SDB !
   cp@ SDB @ - SCL !                      \ region payload (dict + compiled code)
   here data-base - SDL !                 \ data payload (through DP)
   STSZ @ SCL @ + SDL @ + 40 + SNL !      \ new executable text content size
   SNL @ BUILD-SNAP-HDR SFTS ! ;
: SNAPGO
   SNAP-HDR
   \ trailer: magic, old text base, dict count, region length, data length
   SNAP-MAGIC TRL !  STB @ TRL 8 + !  ndict@ TRL 16 + !
   SCL @ TRL 24 + !  SDL @ TRL 32 + !
   \ stream: header, engine text, region, data, trailer, zero pad
   SNAP-OUT PATH0 1537 493 open SFD !
   SFD @ 0 < IF s" snap: cannot open output" 74 die THEN
   SFD @ MBUF CODE-OFF DRV-WALL
   SFD @ STB@ STSZ @ DRV-WALL
   SFD @ SDB@ SCL @ DRV-WALL
   SFD @ data-base SDL @ DRV-WALL
   SFD @ TRL 40 DRV-WALL
   SFD @  SFTS @ CODE-OFF - SNL @ -  WPAD
   SFD @ close
   DRV-EXIT-OK ;

\ Freeze the verify-on-definition hook into the emitted image: hb is fully
\ loaded, so a typed def in its REPL is checked against its sig.
TRUSTED: SNAP-CHECK-HOOK ( ptr u8 n -- n )
   CHECK! dup -1 <> IF 70 throw THEN ;
TRUSTED: SNAP-INSTALL-HOOK ( -- )
   ['] SNAP-CHECK-HOOK set-check ;
SNAP-INSTALL-HOOK
CHECKER-SNAPSHOT-PREPARE
SNAPGO
