\ maker.f - emit a maker image from hb-maker-src.
\
\ tools/hb-build.f writes hb-maker-src as common engine source plus the selected
\ runtime driver. This driver is the separate build-time compiler that turns that
\ source into stage2-got; the resulting maker runs later against user source.

\ Audited build-driver boundary: appended after the toolchain hook is enabled.
\ Dissolves with staged fixpoint source checking: habu-staged-fixpoint-src-0b5fc6e6.
0 set-check

: MK-IN ( -- ptr u8 n )
   s" hb-maker-src" TMP-PATH ;

: MK-OUT ( -- ptr u8 n )
   s" stage2-got" TMP-PATH ;

variable MK-SBUF
variable MK-SLEN
variable MK-FD
variable MK-RD

$200000 constant MK-SOURCE-CAP  \ mmap'd source cap shared with stage2.f. The owner-bearing
                                \ stage source measured 1,050,737 bytes, crossing the retired
                                \ $100000 edge by 2,161 bytes; grow to the next power of two.
$1002 constant MK-MAP-PRIVATE-ANON

: MK-SBUF@ ( -- ptr u8 )
   MK-SBUF @ ;
s" MK-SBUF@" s" -- ptr u8" TRUST

: MK-ALLOC-SOURCE ( -- )
   0 MK-SOURCE-CAP 3 MK-MAP-PRIVATE-ANON -1 0 mmap
   dup 0 < if s" maker: source mmap failed" 74 die then
   MK-SBUF ! ;

: MK-READ-SRC ( -- )
   MK-IN PATH0 0 0 open MK-FD !
   MK-FD @ 0 < IF s" maker: cannot open source" 74 die THEN
   MK-ALLOC-SOURCE  0 MK-SLEN !
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
   DRV-RETIRE-RELOADS
   MK-SBUF@ MK-SLEN @ ENGINE-BUILD:BUILD
   s" hb" MK-OUT DRV-EMIT-IMAGE ;

\ Process boundary: report uncaught throws instead of exiting silently
\ (driver-io.f DRV-FAIL; exit code stays the throw code when representable,
\ else die maps it to UNCAUGHT-RC).
: MK-RUN ( -- )
   [: MK-GO ;] catch
   dup 0 = IF drop EXIT THEN
   DRV-FAIL ;

MK-RUN
