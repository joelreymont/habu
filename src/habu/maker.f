\ maker.f - emit a maker image from hb-maker-src.
\
\ tools/hb-build.f writes hb-maker-src as common engine source plus the selected
\ runtime driver. This driver is the separate build-time compiler that turns that
\ source into stage2-got; the resulting maker runs later against user source.

\ Audited build-driver boundary: appended after the toolchain hook is enabled.
0 set-check

: MK-IN ( -- ptr u8 n )
   s" hb-maker-src" TMP-PATH ;

: MK-OUT ( -- ptr u8 n )
   s" stage2-got" TMP-PATH ;

variable MK-SBUF
variable MK-SLEN
variable MK-FD
variable MK-RD

$80000 constant MK-SOURCE-CAP

: MK-SBUF@ ( -- ptr u8 )
   MK-SBUF @ ;
s" MK-SBUF@" s" -- ptr u8" TRUST

: MK-READ-SRC ( -- )
   MK-IN PATH0 0 0 open MK-FD !
   MK-FD @ 0 < IF s" maker: cannot open source" 74 die THEN
   here MK-SBUF !  MK-SOURCE-CAP allot  0 MK-SLEN !
   begin
      MK-FD @ MK-SBUF@ MK-SLEN @ + MK-SOURCE-CAP MK-SLEN @ - read MK-RD !
      MK-RD @ 0 >
   while
      MK-SLEN @ MK-RD @ + MK-SLEN !
   repeat
   MK-RD @ 0 < IF MK-FD @ close s" maker: read failed" 74 die THEN
   MK-FD @ close
   MK-SLEN @ 0 > 0= if s" maker: empty source" 74 die then
   MK-SLEN @ MK-SOURCE-CAP = if s" maker: source exceeds buffer" 74 die then ;

: MK-GO ( -- )
   MK-READ-SRC
   MK-SBUF@ MK-SLEN @ EMIT-FORTH
   ASM-CODE BUILD-IMAGE
   s" hb" SET-SIGID CODESIG2
   MK-OUT DRV-WRITE-IMAGE ;

MK-GO
