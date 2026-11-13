\ host.f - implementation-owned facts for native construction calls.
\ A dictionary occurrence is the selection key; a published implementation row
\ supplies its exact code.
\ Rows retired by code reclamation never become live again when bytes are reused.

require src/lib/prelude.f
require src/compiler/target/model.f
require src/compiler/native/fetch-check.f
require src/habu/xref.f
require src/habu/native-observer-cells.f
require src/habu/native-host-cells.f
require src/core/source-producer.f

package NHOST
public

-9440 constant E-FIRST
-9449 constant E-LAST
-9441 constant E-STATE

private

0 CWORK-VARIABLE IMPL-N n
DYNAMIC-BUFFER IMPL-ENTRY n
DYNAMIC-BUFFER IMPL-LEN n
DYNAMIC-BUFFER IMPL-LIVE n

\ One process's install state (INSTALL): a save or a file's object writes it
\ zero, as CAPTURE-PREPARE leaves it. The engine cell INSTALL fills holds no
\ other callback, so a program whose file object kept it installed installs
\ again over its own entries.
variable OWNER-INSTALLED    OWNER-INSTALLED scratch-cell-mark
variable KERNEL-BEGIN       KERNEL-BEGIN scratch-cell-mark
variable KERNEL-END         KERNEL-END scratch-cell-mark
variable SEEDED-N           SEEDED-N scratch-cell-mark

: RESERVE-IMPL ( n -- )
   {: count:n :}
   count IMPL-ENTRY-RESERVE
   count IMPL-LEN-RESERVE
   count IMPL-LIVE-RESERVE ;

public

\ A zero id means no producer ever published facts for this exact entry.
: ID-OF ( n -- n )
   {: entry:n :}
   IMPL-N @ {: count:n :}
   count 0= if 0 exit then
   0 IMPL-LIVE {: live:ptr :}
   0 IMPL-ENTRY {: entries:ptr :}
   count
   begin dup 0 > while
      dup 1- {: row:n :}
      live row cells + @ 0<> entries row cells + @ entry = and if exit then
      1-
   repeat ;

\ A restored process has no retained implementation facts. Its callback cell
\ is cleared with the other process-owned cells; the next source compilation
\ starts a fresh owner without reading the saved image's old dynamic buffers.
: INVALIDATE ( n n -- )
   {: first:n end:n :}
   first end >= if exit then
   KERNEL-END @ KERNEL-BEGIN @ >
   first KERNEL-END @ < and
   end KERNEL-BEGIN @ > and if
      SEEDED-N @ 0 ?do 0 i IMPL-LIVE ! loop
      0 data-base NATIVE-HOST-CELLS:SCALAR-PRIM + !
   then
   IMPL-N @ 0 ?do
      i IMPL-ENTRY @ end <
      i IMPL-ENTRY @ i IMPL-LEN @ + first > and if 0 i IMPL-LIVE ! then
   loop ;

\ The digest slot follows the entire immutable kernel code interval.
: KERNEL-OWNER ( -- )
   0 KERNEL-BEGIN !  0 KERNEL-END !
   SOURCE-PRODUCER:KERNEL$
      {: digest:ptr size:n known:bool :}
   known 0= if exit then
   size 32 <> if E-STATE throw then
   data-base RBASE-CELL + @ {: base:n :}
   digest NULL-PTR BYTE-VIEW - {: end:n :}
   base 0 <= end base <= or if E-STATE throw then
   base KERNEL-BEGIN !  end KERNEL-END ! ;

\ A registry ordinal selects the exact booted occurrence.
: SEED-ROW ( n -- )
   {: marker:n :}
   marker 0< marker ndict@ > or if E-STATE throw then
   marker 1- XREF-REC {: rec:ptr :}
   rec XREF-START {: entry:n :}
   rec XREF-CODE-BYTES {: len:n :}
   entry 0 <= len 0 <= or if E-STATE throw then
   RTARGET:HOST-TARGET RTARGET:CORE
   MATCH RTARGET:core-result
      unsupported OF E-STATE throw ENDOF
      supported OF
         drop
         IMPL-N @ 1+ dup RESERVE-IMPL {: count:n :}
         count 1- {: row:n :}
         entry row IMPL-ENTRY !
         len row IMPL-LEN !
         1 row IMPL-LIVE !
         count IMPL-N !
      ENDOF
   ;MATCH ;

\ The scalar body is call-free.
: SEED-SCALAR ( -- )
   data-base NATIVE-HOST-CELLS:SCALAR-PRIM + @ {: marker:n :}
   marker 0<> if marker SEED-ROW then ;

: INSTALL ( -- )
   OWNER-INSTALLED @ 0<> if exit then
   0 IMPL-N !
   KERNEL-OWNER
   SEED-SCALAR
   IMPL-N @ SEEDED-N !
   ['] INVALIDATE data-base NATIVE-OBS-CELLS:HOST-INVALIDATE + xt!
   1 OWNER-INSTALLED ! ;

: CAPTURE-PREPARE ( -- )
   OWNER-INSTALLED @ 0<> if
      0 data-base NATIVE-OBS-CELLS:HOST-INVALIDATE + !
   then
   0 OWNER-INSTALLED !
   0 KERNEL-BEGIN !  0 KERNEL-END !  0 SEEDED-N ! ;

private

\ Prepare all storage while publication may still throw. The publisher only
\ exposes the row after its bytes and dictionary record are committed.
: PREPARE ( n n -- n )
   {: entry:n len:n :}
   entry 0 <= len 0 <= or if E-STATE throw then
   IMPL-N @ 1+ dup RESERVE-IMPL {: id:n :}
   id 1- {: row:n :}
   entry row IMPL-ENTRY !
   len row IMPL-LEN !
   0 row IMPL-LIVE !
   id IMPL-N !
   id ;

;package

NHOST:INSTALL
